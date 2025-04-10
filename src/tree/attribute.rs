// Copyright of the original code is the following.
// --------
// Summary: interfaces for tree manipulation
// Description: this module describes the structures found in an tree resulting
//              from an XML or HTML parsing, as well as the API provided for
//              various processing on that tree
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// tree.c : implementation of access function for an XML tree.
//
// References:
//   XHTML 1.0 W3C REC: http://www.w3.org/TR/2002/REC-xhtml1-20020801/
//
// See Copyright for the status of this software.
//
// daniel@veillard.com

use std::{
    any::type_name,
    borrow::Cow,
    ops::{Deref, DerefMut},
    os::raw::c_void,
    ptr::{NonNull, null_mut},
};

use crate::{
    globals::{get_deregister_node_func, get_register_node_func},
    libxml::valid::{xml_add_id, xml_is_id, xml_remove_id},
};

use super::{
    InvalidNodePointerCastError, NodeCommon, XML_XML_NAMESPACE, XmlAttributeType, XmlDocPtr,
    XmlElementType, XmlGenericNodePtr, XmlNodePtr, XmlNsPtr, xml_free_node_list, xml_new_doc_text,
    xml_new_ns, xml_new_reconciled_ns, xml_ns_in_scope, xml_static_copy_node_list,
    xml_tree_err_memory,
};

#[repr(C)]
pub struct XmlAttr {
    pub _private: *mut c_void,                  /* application data */
    pub(crate) typ: XmlElementType,             /* XML_ATTRIBUTE_NODE, must be second ! */
    pub(crate) name: Box<str>,                  /* the name of the property */
    pub(crate) children: Option<XmlNodePtr>,    /* the value of the property */
    pub(crate) last: Option<XmlNodePtr>,        /* NULL */
    pub(crate) parent: Option<XmlNodePtr>,      /* child->parent link */
    pub(crate) next: Option<XmlAttrPtr>,        /* next sibling link  */
    pub(crate) prev: Option<XmlAttrPtr>,        /* previous sibling link  */
    pub(crate) doc: Option<XmlDocPtr>,          /* the containing document */
    pub(crate) ns: Option<XmlNsPtr>,            /* pointer to the associated namespace */
    pub(crate) atype: Option<XmlAttributeType>, /* the attribute type if validating */
    pub(crate) psvi: *mut c_void,               /* for type/PSVI information */
}

impl XmlAttr {
    /// Search a Ns aliasing a given URI.
    /// Recurse on the parents until it finds the defined namespace or return NULL otherwise.
    ///
    /// Returns the namespace pointer or NULL.
    #[doc(alias = "xmlSearchNsByHref")]
    pub fn search_ns_by_href(&mut self, doc: Option<XmlDocPtr>, href: &str) -> Option<XmlNsPtr> {
        if href == XML_XML_NAMESPACE {
            let mut doc = doc.or(self.document())?;
            // Return the XML namespace declaration held by the doc.
            if doc.old_ns.is_none() {
                return doc.ensure_xmldecl();
            } else {
                return doc.old_ns;
            }
        }
        let mut node = self.parent.map(XmlGenericNodePtr::from);
        while let Some(now) = node {
            if matches!(
                now.element_type(),
                XmlElementType::XmlEntityRefNode
                    | XmlElementType::XmlEntityNode
                    | XmlElementType::XmlEntityDecl
            ) {
                return None;
            }
            if let Some(now) = XmlNodePtr::try_from(now)
                .ok()
                .filter(|now| now.element_type() == XmlElementType::XmlElementNode)
            {
                // let href = CString::new(href).unwrap();
                let mut cur = now.ns_def;
                while let Some(cur_ns) = cur {
                    if cur_ns.href.is_some()
                        && cur_ns.href().as_deref() == Some(href)
                        && cur_ns.prefix().is_some()
                        && xml_ns_in_scope(
                            doc,
                            XmlGenericNodePtr::from_raw(self as *mut Self),
                            Some(now.into()),
                            cur_ns.prefix.as_deref(),
                        ) == 1
                    {
                        return Some(cur_ns);
                    }
                    cur = cur_ns.next;
                }
                let cur = now.ns;
                if let Some(cur) = cur.filter(|cur| {
                    cur.href.as_deref().is_some_and(|h| h == href)
                        && cur.prefix().is_some()
                        && xml_ns_in_scope(
                            doc,
                            XmlGenericNodePtr::from_raw(self as *mut Self),
                            Some(now.into()),
                            cur.prefix.as_deref(),
                        ) == 1
                }) {
                    return Some(cur);
                }
            }
            node = now.parent();
        }
        None
    }

    pub(super) fn get_prop_node_value_internal(&self) -> Option<String> {
        // Note that we return at least the empty string.
        // TODO: Do we really always want that?
        if let Some(children) = self.children() {
            if children.next().is_none()
                && matches!(
                    children.element_type(),
                    XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
                )
            {
                let children = XmlNodePtr::try_from(children).unwrap();
                // Optimization for the common case: only 1 text node.
                return children.content.clone();
            } else if let Some(ret) = children.get_string(self.document(), 1) {
                return Some(ret);
            }
        }
        Some("".to_owned())
    }

    /// Read the value of a node, this can be either the text carried
    /// directly by this node if it's a TEXT node or the aggregate string
    /// of the values carried by this node child's (TEXT and ENTITY_REF).  
    ///
    /// Entity references are substituted.
    ///
    /// Returns a new #XmlChar * or null_mut() if no content is available.  
    /// It's up to the caller to free the memory with xml_free().
    #[doc(alias = "xmlNodeGetContent")]
    pub fn get_content(&self) -> Option<String> {
        self.get_prop_node_value_internal()
    }

    /// Read the value of a node `cur`, this can be either the text carried
    /// directly by this node if it's a TEXT node or the aggregate string
    /// of the values carried by this node child's (TEXT and ENTITY_REF).
    ///
    /// Entity references are substituted. Fills up the buffer `buf` with this value.
    ///
    /// Returns 0 in case of success and -1 in case of error.
    #[doc(alias = "xmlBufGetNodeContent")]
    pub fn get_content_to(&self, buf: &mut String) -> i32 {
        assert!(matches!(
            self.element_type(),
            XmlElementType::XmlAttributeNode
        ));
        let mut tmp = self.children();

        while let Some(now) = tmp {
            if matches!(now.element_type(), XmlElementType::XmlTextNode) {
                let now = XmlNodePtr::try_from(now).unwrap();
                buf.push_str(now.content.as_deref().unwrap());
            } else {
                now.get_content_to(buf);
            }
            tmp = now.next();
        }
        0
    }

    /// Set (or reset) the base URI of a node, i.e. the value of the xml:base attribute.
    #[doc(alias = "xmlNodeSetBase")]
    #[cfg(any(feature = "libxml_tree", feature = "xinclude"))]
    pub fn set_base(&mut self, _uri: Option<&str>) {
        use crate::tree::XML_XML_NAMESPACE;

        self.search_ns_by_href(self.document(), XML_XML_NAMESPACE);
    }

    /// update all nodes under the tree to point to the right document
    #[doc(alias = "xmlSetTreeDoc")]
    pub fn set_doc(&mut self, doc: Option<XmlDocPtr>) {
        if self.document() != doc {
            if let Some(children) = self.children() {
                children.set_doc_all_sibling(doc);
            }

            // FIXME: self.ns should be updated as in xmlStaticCopyNode().
            self.set_document(doc);
        }
    }
}

impl Default for XmlAttr {
    fn default() -> Self {
        Self {
            _private: null_mut(),
            typ: XmlElementType::XmlAttributeNode,
            name: "".into(),
            children: None,
            last: None,
            parent: None,
            next: None,
            prev: None,
            doc: None,
            ns: None,
            atype: None,
            psvi: null_mut(),
        }
    }
}

impl NodeCommon for XmlAttr {
    fn document(&self) -> Option<XmlDocPtr> {
        self.doc
    }
    fn set_document(&mut self, doc: Option<XmlDocPtr>) {
        self.doc = doc;
    }
    fn element_type(&self) -> XmlElementType {
        self.typ
    }
    fn name(&self) -> Option<Cow<'_, str>> {
        Some(Cow::Borrowed(&self.name))
    }
    fn children(&self) -> Option<XmlGenericNodePtr> {
        self.children.map(|children| children.into())
    }
    fn set_children(&mut self, children: Option<XmlGenericNodePtr>) {
        self.children = children.map(|children| XmlNodePtr::try_from(children).unwrap());
    }
    fn last(&self) -> Option<XmlGenericNodePtr> {
        self.last.map(|last| last.into())
    }
    fn set_last(&mut self, last: Option<XmlGenericNodePtr>) {
        self.last = last.map(|children| XmlNodePtr::try_from(children).unwrap());
    }
    fn next(&self) -> Option<XmlGenericNodePtr> {
        self.next.map(|next| next.into())
    }
    fn set_next(&mut self, next: Option<XmlGenericNodePtr>) {
        self.next = next.map(|next| XmlAttrPtr::try_from(next).unwrap())
    }
    fn prev(&self) -> Option<XmlGenericNodePtr> {
        self.prev.map(|prev| prev.into())
    }
    fn set_prev(&mut self, prev: Option<XmlGenericNodePtr>) {
        self.prev = prev.map(|prev| XmlAttrPtr::try_from(prev).unwrap())
    }
    fn parent(&self) -> Option<XmlGenericNodePtr> {
        self.parent.map(|parent| parent.into())
    }
    fn set_parent(&mut self, parent: Option<XmlGenericNodePtr>) {
        self.parent = parent.map(|children| XmlNodePtr::try_from(children).unwrap());
    }

    fn unlink(&mut self) {
        if let Some(mut parent) = self.parent {
            let attr = unsafe {
                // # Safety
                // Please see the document of `XmlAttrPtr::from_raw`.
                // In addition, this pointer is not leaked to the out of this function.
                XmlAttrPtr::from_raw(self).unwrap()
            };
            if parent.properties == attr {
                parent.properties = attr.and_then(|attr| attr.next);
            }
            self.set_parent(None);
        }
        if let Some(mut next) = self.next() {
            next.set_prev(self.prev());
        }
        if let Some(mut prev) = self.prev() {
            prev.set_next(self.next());
        }
        self.set_next(None);
        self.set_prev(None);
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub struct XmlAttrPtr(NonNull<XmlAttr>);

impl XmlAttrPtr {
    /// Allocate new memory and create new `XmlAttrPtr` from an owned xml node.
    ///
    /// This method leaks allocated memory.  
    /// Users can use `free` method for deallocating memory.
    pub(crate) fn new(node: XmlAttr) -> Option<Self> {
        let boxed = Box::new(node);
        NonNull::new(Box::leak(boxed)).map(Self)
    }

    /// Create `XmlAttrPtr` from a raw pointer.  
    ///
    /// If `ptr` is a NULL pointer, return `Ok(None)`.  
    /// If `ptr` is a valid pointer of `XmlAttr`, return `Ok(Some(Self))`.  
    /// Otherwise, return `Err`.
    ///
    /// # Safety
    /// - `ptr` must be a pointer of types that is implemented `NodeCommon` at least.
    pub(crate) unsafe fn from_raw(
        ptr: *mut XmlAttr,
    ) -> Result<Option<Self>, InvalidNodePointerCastError> {
        unsafe {
            if ptr.is_null() {
                return Ok(None);
            }
            match (*ptr).element_type() {
                XmlElementType::XmlAttributeNode => Ok(Some(Self(NonNull::new_unchecked(ptr)))),
                _ => Err(InvalidNodePointerCastError {
                    from: (*ptr).element_type(),
                    to: type_name::<Self>(),
                }),
            }
        }
    }

    // pub(crate) fn as_ptr(self) -> *mut XmlAttr {
    //     self.0.as_ptr()
    // }

    /// Deallocate memory.
    ///
    /// # Safety
    /// This method should be called only once.  
    /// If called more than twice, the behavior is undefined.
    pub(crate) unsafe fn free(self) {
        unsafe {
            let _ = *Box::from_raw(self.0.as_ptr());
        }
    }

    // /// Acquire the ownership of the inner value.
    // /// As a result, `self` will be invalid. `self` must not be used after performs this method.
    // ///
    // /// # Safety
    // /// This method should be called only once.
    // /// If called more than twice, the behavior is undefined.
    // pub(crate) unsafe fn into_inner(self) -> Box<XmlAttr> {
    //     unsafe { Box::from_raw(self.0.as_ptr()) }
    // }

    /// Unlink and free one attribute, all the content is freed too.
    ///
    /// Note this doesn't work for namespace definition attributes.
    ///
    /// Returns 0 if success and -1 in case of error.
    #[doc(alias = "xmlRemoveProp")]
    pub unsafe fn remove_prop(self) -> i32 {
        unsafe {
            let Some(mut parent) = self
                .parent()
                .map(|parent| XmlNodePtr::try_from(parent).unwrap())
            else {
                return -1;
            };
            let mut tmp = parent.properties;
            if tmp == Some(self) {
                parent.properties = self.next;
                if let Some(mut next) = self.next {
                    next.prev = None;
                }
                xml_free_prop(self);
                return 0;
            }
            while let Some(mut now) = tmp {
                if now.next == Some(self) {
                    now.next = self.next;
                    if let Some(mut next) = now.next {
                        next.prev = Some(now);
                    }
                    xml_free_prop(self);
                    return 0;
                }
                tmp = now.next;
            }
            -1
        }
    }
}

impl Clone for XmlAttrPtr {
    fn clone(&self) -> Self {
        *self
    }
}

impl Copy for XmlAttrPtr {}

impl Deref for XmlAttrPtr {
    type Target = XmlAttr;
    fn deref(&self) -> &Self::Target {
        // # Safety
        // I don't implement the pointer casting and addition/subtraction methods
        // and don't expose the inner `NonNull` for `*mut XmlAttr`.
        // Therefore, as long as the constructor is correctly implemented,
        // the pointer dereference is valid.
        unsafe { self.0.as_ref() }
    }
}

impl DerefMut for XmlAttrPtr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // # Safety
        // I don't implement the pointer casting and addition/subtraction methods
        // and don't expose the inner `NonNull` for `*mut XmlAttr`.
        // Therefore, as long as the constructor is correctly implemented,
        // the pointer dereference is valid.
        unsafe { self.0.as_mut() }
    }
}

impl TryFrom<XmlGenericNodePtr> for XmlAttrPtr {
    type Error = InvalidNodePointerCastError;

    fn try_from(value: XmlGenericNodePtr) -> Result<Self, Self::Error> {
        match value.element_type() {
            XmlElementType::XmlAttributeNode => Ok(Self(value.0.cast())),
            _ => Err(InvalidNodePointerCastError {
                from: value.element_type(),
                to: type_name::<Self>(),
            }),
        }
    }
}

impl From<XmlAttrPtr> for XmlGenericNodePtr {
    fn from(value: XmlAttrPtr) -> Self {
        Self(value.0 as NonNull<dyn NodeCommon>)
    }
}

impl From<XmlAttrPtr> for *mut XmlAttr {
    fn from(value: XmlAttrPtr) -> Self {
        value.0.as_ptr()
    }
}

/// Create a new property carried by a document.  
/// Returns a pointer to the attribute
///
/// # NOTE
/// `value` is supposed to be a piece of XML CDATA, so it allows entity references,
/// but XML special chars need to be escaped first by using.  
/// xmlEncodeEntitiesReentrant(). Use xmlNewProp() if you don't need entities support.
#[doc(alias = "xmlNewDocProp")]
pub fn xml_new_doc_prop(
    doc: Option<XmlDocPtr>,
    name: &str,
    value: Option<&str>,
) -> Option<XmlAttrPtr> {
    // Allocate a new property and fill the fields.
    let Some(mut cur) = XmlAttrPtr::new(XmlAttr {
        typ: XmlElementType::XmlAttributeNode,
        name: name.into(),
        doc,
        ..Default::default()
    }) else {
        xml_tree_err_memory("building attribute");
        return None;
    };
    if let Some(value) = value {
        cur.children = doc.and_then(|doc| doc.get_node_list(value));
        cur.last = None;

        let mut tmp = cur.children();
        while let Some(mut now) = tmp {
            now.set_parent(Some(cur.into()));
            if now.next().is_none() {
                cur.set_last(Some(now));
            }
            tmp = now.next();
        }
    }

    if let Some(register) = get_register_node_func() {
        register(cur.into());
    }

    Some(cur)
}

pub(super) fn xml_new_prop_internal(
    node: Option<XmlNodePtr>,
    ns: Option<XmlNsPtr>,
    name: &str,
    value: Option<&str>,
) -> Option<XmlAttrPtr> {
    if node.is_some_and(|node| !matches!(node.element_type(), XmlElementType::XmlElementNode)) {
        return None;
    }

    // Allocate a new property and fill the fields.
    let Some(mut cur) = XmlAttrPtr::new(XmlAttr {
        typ: XmlElementType::XmlAttributeNode,
        parent: node,
        ns,
        name: name.into(),
        ..Default::default()
    }) else {
        xml_tree_err_memory("building attribute");
        return None;
    };

    let mut doc = None;
    if let Some(node) = node {
        doc = node.doc;
        cur.doc = doc;
    }

    if let Some(value) = value {
        cur.set_children(xml_new_doc_text(doc, Some(value)).map(|node| node.into()));
        cur.set_last(None);
        let mut tmp = cur.children();
        while let Some(mut now) = tmp {
            now.set_parent(Some(cur.into()));
            if now.next().is_none() {
                cur.set_last(Some(now));
            }
            tmp = now.next();
        }
    }

    // Add it at the end to preserve parsing order ...
    if let Some(mut node) = node {
        if let Some(mut prev) = node.properties {
            while let Some(next) = prev.next {
                prev = next;
            }
            prev.next = Some(cur);
            cur.prev = Some(prev);
        } else {
            node.properties = Some(cur);
        }
    }

    if let Some(value) = value {
        if let Some(node) = node {
            if xml_is_id(node.doc, Some(node), Some(cur)) == 1 {
                xml_add_id(None, node.doc.unwrap(), value, cur);
            }
        }
    }

    if let Some(register) = get_register_node_func() {
        register(cur.into());
    }

    Some(cur)
}

/// Create a new property carried by a node.  
/// Returns a pointer to the attribute
#[doc(alias = "xmlNewProp")]
#[cfg(any(feature = "libxml_tree", feature = "html", feature = "schema"))]
pub fn xml_new_prop(
    node: Option<XmlNodePtr>,
    name: &str,
    value: Option<&str>,
) -> Option<XmlAttrPtr> {
    xml_new_prop_internal(node, None, name, value)
}

/// Create a new property tagged with a namespace and carried by a node.  
/// Returns a pointer to the attribute
#[doc(alias = "xmlNewNsProp")]
pub fn xml_new_ns_prop(
    node: Option<XmlNodePtr>,
    ns: Option<XmlNsPtr>,
    name: &str,
    value: Option<&str>,
) -> Option<XmlAttrPtr> {
    xml_new_prop_internal(node, ns, name, value)
}

pub(super) unsafe fn xml_copy_prop_internal(
    doc: Option<XmlDocPtr>,
    target: Option<XmlNodePtr>,
    cur: XmlAttrPtr,
) -> Option<XmlAttrPtr> {
    unsafe {
        if target
            .is_some_and(|target| !matches!(target.element_type(), XmlElementType::XmlElementNode))
        {
            return None;
        }
        let mut ret = if let Some(target) = target {
            xml_new_doc_prop(target.doc, &cur.name, None)
        } else if let Some(doc) = doc {
            xml_new_doc_prop(Some(doc), &cur.name, None)
        } else if let Some(parent) = cur.parent() {
            xml_new_doc_prop(parent.document(), &cur.name, None)
        } else if let Some(children) = cur.children() {
            xml_new_doc_prop(children.document(), &cur.name, None)
        } else {
            xml_new_doc_prop(None, &cur.name, None)
        }?;
        ret.parent = target;

        if let Some((cur_ns, mut target)) = cur.ns.zip(target) {
            let prefix = cur_ns.prefix();
            let target_doc = target.doc;
            if let Some(ns) = target.search_ns(target_doc, prefix.as_deref()) {
                // we have to find something appropriate here since
                // we can't be sure, that the namespace we found is identified
                // by the prefix
                if ns.href == cur_ns.href {
                    // this is the nice case
                    ret.ns = Some(ns);
                } else {
                    // we are in trouble: we need a new reconciled namespace.
                    // This is expensive
                    ret.ns = xml_new_reconciled_ns(target.doc, target, cur_ns);
                }
            } else {
                // Humm, we are copying an element whose namespace is defined
                // out of the new tree scope. Search it in the original tree
                // and add it at the top of the new tree
                if let Some(ns) = cur.parent().unwrap().search_ns(cur.doc, prefix.as_deref()) {
                    let mut root = XmlGenericNodePtr::from(target);
                    let mut pred = None;

                    while let Some(parent) = root.parent() {
                        pred = Some(root);
                        root = parent;
                    }
                    ret.ns = if Some(root) == target.doc.map(|doc| doc.into()) {
                        // correct possibly cycling above the document elt
                        xml_new_ns(
                            pred.map(|p| XmlNodePtr::try_from(p).unwrap()),
                            ns.href.as_deref(),
                            ns.prefix().as_deref(),
                        )
                    } else {
                        xml_new_ns(
                            Some(XmlNodePtr::try_from(root).unwrap()),
                            ns.href.as_deref(),
                            ns.prefix().as_deref(),
                        )
                    };
                }
            }
        } else {
            ret.ns = None;
        }

        if let Some(children) = cur.children() {
            let doc = ret.doc;
            let parent = Some(ret.into());
            ret.set_children(xml_static_copy_node_list(Some(children), doc, parent));
            ret.set_last(None);
            let mut tmp = ret.children();
            while let Some(now) = tmp {
                // (*tmp).parent = ret;
                if now.next().is_none() {
                    ret.set_last(Some(now));
                }
                tmp = now.next();
            }
        }
        // Try to handle IDs
        if let Some(target_doc) = target.and_then(|target| target.doc).filter(|_| {
            cur.doc.is_some_and(|doc| doc.ids.is_some())
                && cur
                    .parent
                    .filter(|&p| xml_is_id(cur.doc, Some(p), Some(cur)) != 0)
                    .is_some()
        }) {
            let children = cur.children();
            if let Some(id) = children.and_then(|c| c.get_string(cur.doc, 1)) {
                xml_add_id(None, target_doc, &id, ret);
            }
        }
        Some(ret)
    }
}

/// Do a copy of the attribute.
///
/// Returns: a new #xmlAttrPtr, or null_mut() in case of error.
#[doc(alias = "xmlCopyProp")]
pub unsafe fn xml_copy_prop(target: Option<XmlNodePtr>, cur: XmlAttrPtr) -> Option<XmlAttrPtr> {
    unsafe { xml_copy_prop_internal(None, target, cur) }
}

/// Do a copy of an attribute list.
///
/// Returns: a new #xmlAttrPtr, or null_mut() in case of error.
#[doc(alias = "xmlCopyPropList")]
pub unsafe fn xml_copy_prop_list(
    target: Option<XmlNodePtr>,
    mut cur: Option<XmlAttrPtr>,
) -> Option<XmlAttrPtr> {
    unsafe {
        if target
            .is_some_and(|target| !matches!(target.element_type(), XmlElementType::XmlElementNode))
        {
            return None;
        }
        let mut ret = None;
        let mut p = None::<XmlAttrPtr>;
        while let Some(now) = cur {
            let Some(mut q) = xml_copy_prop(target, now) else {
                xml_free_prop_list(ret);
                return None;
            };
            if let Some(mut np) = p {
                np.next = Some(q);
                q.prev = Some(np);
                p = Some(q);
            } else {
                ret = Some(q);
                p = Some(q);
            }
            cur = now.next;
        }
        ret
    }
}

/// Free one attribute, all the content is freed too
#[doc(alias = "xmlFreeProp")]
pub unsafe fn xml_free_prop(cur: XmlAttrPtr) {
    unsafe {
        if let Some(deregister) = get_deregister_node_func() {
            deregister(cur.into());
        }

        // Check for ID removal -> leading to invalid references !
        if let Some(doc) = cur
            .doc
            .filter(|_| matches!(cur.atype, Some(XmlAttributeType::XmlAttributeID)))
        {
            xml_remove_id(doc, cur);
        }
        if let Some(children) = cur.children() {
            xml_free_node_list(Some(children));
        }
        cur.free();
    }
}

/// Free a property and all its siblings, all the children are freed too.
#[doc(alias = "xmlFreePropList")]
pub unsafe fn xml_free_prop_list(cur: Option<XmlAttrPtr>) {
    unsafe {
        if let Some(cur) = cur {
            let mut next = cur.next;
            xml_free_prop(cur);
            while let Some(now) = next {
                next = now.next;
                xml_free_prop(now);
            }
        }
    }
}
