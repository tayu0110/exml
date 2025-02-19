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
    ffi::CStr,
    ops::{Deref, DerefMut},
    os::raw::c_void,
    ptr::{null_mut, NonNull},
    sync::atomic::Ordering,
};

use crate::libxml::{
    globals::{xml_deregister_node_default_value, xml_free, xml_register_node_default_value},
    valid::{xml_add_id, xml_is_id, xml_remove_id},
    xmlstring::{xml_str_equal, xml_strdup, xml_strndup, XmlChar},
};

use super::{
    xml_free_node_list, xml_new_doc_text, xml_new_ns, xml_new_reconciled_ns, xml_ns_in_scope,
    xml_static_copy_node_list, xml_tree_err_memory, InvalidNodePointerCastError, NodeCommon,
    NodePtr, XmlAttributeType, XmlDocPtr, XmlElementType, XmlGenericNodePtr, XmlNode, XmlNodePtr,
    XmlNsPtr, XML_XML_NAMESPACE, __XML_REGISTER_CALLBACKS,
};

#[repr(C)]
pub struct XmlAttr {
    pub(crate) _private: *mut c_void,           /* application data */
    pub(crate) typ: XmlElementType,             /* XML_ATTRIBUTE_NODE, must be second ! */
    pub(crate) name: *const XmlChar,            /* the name of the property */
    pub(crate) children: Option<NodePtr>,       /* the value of the property */
    pub(crate) last: Option<NodePtr>,           /* NULL */
    pub(crate) parent: Option<NodePtr>,         /* child->parent link */
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
    pub unsafe fn search_ns_by_href(
        &mut self,
        doc: Option<XmlDocPtr>,
        href: &str,
    ) -> Option<XmlNsPtr> {
        if href == XML_XML_NAMESPACE.to_str().unwrap() {
            let mut doc = doc.or(self.document())?;
            // Return the XML namespace declaration held by the doc.
            if doc.old_ns.is_none() {
                return doc.ensure_xmldecl();
            } else {
                return doc.old_ns;
            }
        }
        let mut node = self
            .parent
            .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()));
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
                    if !cur_ns.href.is_null()
                        && cur_ns.href().as_deref() == Some(href)
                        && cur_ns.prefix().is_some()
                        && xml_ns_in_scope(doc, self as *mut Self as _, now.as_ptr(), cur_ns.prefix)
                            == 1
                    {
                        return Some(cur_ns);
                    }
                    cur = XmlNsPtr::from_raw(cur_ns.next).unwrap();
                }
                let cur = now.ns;
                if let Some(cur) = cur.filter(|cur| {
                    !cur.href.is_null()
                        && cur.href().as_deref() == Some(href)
                        && cur.prefix().is_some()
                        && xml_ns_in_scope(doc, self as *mut Self as _, now.as_ptr(), cur.prefix)
                            == 1
                }) {
                    return Some(cur);
                }
            }
            node = now
                .parent()
                .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()));
        }
        None
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
    pub unsafe fn get_content(&self) -> Option<String> {
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
    pub unsafe fn get_content_to(&self, buf: &mut String) -> i32 {
        assert!(matches!(
            self.element_type(),
            XmlElementType::XmlAttributeNode
        ));
        let mut tmp = self.children;

        while let Some(now) = tmp {
            if matches!(now.element_type(), XmlElementType::XmlTextNode) {
                buf.push_str(
                    CStr::from_ptr(now.content as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                );
            } else {
                now.get_content_to(buf);
            }
            tmp = now.next;
        }
        0
    }

    /// Set (or reset) the base URI of a node, i.e. the value of the xml:base attribute.
    #[doc(alias = "xmlNodeSetBase")]
    #[cfg(any(feature = "libxml_tree", feature = "xinclude"))]
    pub unsafe fn set_base(&mut self, _uri: Option<&str>) {
        use crate::tree::XML_XML_NAMESPACE;

        self.search_ns_by_href(self.document(), XML_XML_NAMESPACE.to_str().unwrap());
    }
}

impl Default for XmlAttr {
    fn default() -> Self {
        Self {
            _private: null_mut(),
            typ: XmlElementType::XmlAttributeNode,
            name: null_mut(),
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
        (!self.name.is_null())
            .then(|| unsafe { CStr::from_ptr(self.name as *const i8).to_string_lossy() })
    }
    fn children(&self) -> Option<NodePtr> {
        self.children
    }
    fn set_children(&mut self, children: Option<NodePtr>) {
        self.children = children;
    }
    fn last(&self) -> Option<NodePtr> {
        self.last
    }
    fn set_last(&mut self, last: Option<NodePtr>) {
        self.last = last;
    }
    fn next(&self) -> Option<NodePtr> {
        self.next
            .and_then(|next| NodePtr::from_ptr(next.as_ptr() as *mut XmlNode))
    }
    fn set_next(&mut self, next: Option<NodePtr>) {
        self.next = next.and_then(|next| unsafe {
            XmlAttrPtr::from_raw(next.as_ptr() as *mut XmlAttr).unwrap()
        })
    }
    fn prev(&self) -> Option<NodePtr> {
        self.prev
            .and_then(|prev| NodePtr::from_ptr(prev.as_ptr() as *mut XmlNode))
    }
    fn set_prev(&mut self, prev: Option<NodePtr>) {
        self.prev = prev.and_then(|prev| unsafe {
            XmlAttrPtr::from_raw(prev.as_ptr() as *mut XmlAttr).unwrap()
        })
    }
    fn parent(&self) -> Option<NodePtr> {
        self.parent
    }
    fn set_parent(&mut self, parent: Option<NodePtr>) {
        self.parent = parent;
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

    pub(crate) fn as_ptr(self) -> *mut XmlAttr {
        self.0.as_ptr()
    }

    /// Deallocate memory.
    ///
    /// # Safety
    /// This method should be called only once.  
    /// If called more than twice, the behavior is undefined.
    pub(crate) unsafe fn free(self) {
        let _ = *Box::from_raw(self.0.as_ptr());
    }

    /// Acquire the ownership of the inner value.  
    /// As a result, `self` will be invalid. `self` must not be used after performs this method.
    ///
    /// # Safety
    /// This method should be called only once.  
    /// If called more than twice, the behavior is undefined.
    pub(crate) unsafe fn into_inner(self) -> Box<XmlAttr> {
        Box::from_raw(self.0.as_ptr())
    }

    /// Unlink and free one attribute, all the content is freed too.
    ///
    /// Note this doesn't work for namespace definition attributes.
    ///
    /// Returns 0 if success and -1 in case of error.
    #[doc(alias = "xmlRemoveProp")]
    pub unsafe fn remove_prop(&mut self) -> i32 {
        let Some(mut parent) = self.parent else {
            return -1;
        };
        let mut tmp = parent.properties;
        if tmp == Some(*self) {
            parent.properties = self.next;
            if let Some(mut next) = self.next {
                next.prev = None;
            }
            xml_free_prop(*self);
            return 0;
        }
        while let Some(mut now) = tmp {
            if now.next == Some(*self) {
                now.next = self.next;
                if let Some(mut next) = now.next {
                    next.prev = Some(now);
                }
                xml_free_prop(*self);
                return 0;
            }
            tmp = now.next;
        }
        -1
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
pub unsafe fn xml_new_doc_prop(
    doc: Option<XmlDocPtr>,
    name: *const XmlChar,
    value: *const XmlChar,
) -> Option<XmlAttrPtr> {
    if name.is_null() {
        return None;
    }

    // Allocate a new property and fill the fields.
    let Some(mut cur) = XmlAttrPtr::new(XmlAttr {
        typ: XmlElementType::XmlAttributeNode,
        name: xml_strdup(name),
        doc,
        ..Default::default()
    }) else {
        xml_tree_err_memory("building attribute");
        return None;
    };
    if !value.is_null() {
        cur.children = doc.and_then(|doc| NodePtr::from_ptr(doc.get_node_list(value)));
        cur.last = None;

        let mut tmp = cur.children();
        while let Some(mut now) = tmp {
            now.set_parent(NodePtr::from_ptr(cur.as_ptr() as *mut XmlNode));
            if now.next.is_none() {
                cur.set_last(Some(now));
            }
            tmp = now.next();
        }
    }

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    //  && xmlRegisterNodeDefaultValue.is_some()
    {
        xml_register_node_default_value(cur.as_ptr() as _);
    }
    Some(cur)
}

pub(super) unsafe fn xml_new_prop_internal(
    node: Option<XmlNodePtr>,
    ns: Option<XmlNsPtr>,
    name: &str,
    value: *const XmlChar,
) -> Option<XmlAttrPtr> {
    if node.map_or(false, |node| {
        !matches!(node.element_type(), XmlElementType::XmlElementNode)
    }) {
        return None;
    }

    // Allocate a new property and fill the fields.
    let Some(mut cur) = XmlAttrPtr::new(XmlAttr {
        typ: XmlElementType::XmlAttributeNode,
        parent: NodePtr::from_ptr(node.map_or(null_mut(), |node| node.as_ptr())),
        ns,
        name: xml_strndup(name.as_ptr(), name.len() as i32),
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

    if !value.is_null() {
        cur.children = NodePtr::from_ptr(
            xml_new_doc_text(doc, value).map_or(null_mut(), |node| node.as_ptr()),
        );
        cur.set_last(None);
        let mut tmp = cur.children;
        while let Some(mut now) = tmp {
            now.set_parent(NodePtr::from_ptr(cur.as_ptr() as *mut XmlNode));
            if now.next.is_none() {
                cur.last = Some(now);
            }
            tmp = now.next;
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

    if !value.is_null() {
        if let Some(node) = node {
            if xml_is_id(node.doc, Some(node), Some(cur)) == 1 {
                xml_add_id(
                    null_mut(),
                    node.doc.unwrap(),
                    CStr::from_ptr(value as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    cur,
                );
            }
        }
    }

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    //  && xmlRegisterNodeDefaultValue.is_some()
    {
        xml_register_node_default_value(cur.as_ptr() as _);
    }
    Some(cur)
}

/// Create a new property carried by a node.  
/// Returns a pointer to the attribute
#[doc(alias = "xmlNewProp")]
#[cfg(any(feature = "libxml_tree", feature = "html", feature = "schema"))]
pub unsafe fn xml_new_prop(
    node: Option<XmlNodePtr>,
    name: *const XmlChar,
    value: *const XmlChar,
) -> Option<XmlAttrPtr> {
    if name.is_null() {
        return None;
    }

    let n = CStr::from_ptr(name as *const i8).to_string_lossy();
    xml_new_prop_internal(node, None, &n, value)
}

/// Create a new property tagged with a namespace and carried by a node.  
/// Returns a pointer to the attribute
#[doc(alias = "xmlNewNsProp")]
pub unsafe fn xml_new_ns_prop(
    node: Option<XmlNodePtr>,
    ns: Option<XmlNsPtr>,
    name: &str,
    value: *const XmlChar,
) -> Option<XmlAttrPtr> {
    xml_new_prop_internal(node, ns, name, value)
}

/// Create a new property tagged with a namespace and carried by a node.  
/// Returns a pointer to the attribute
#[doc(alias = "xmlNewNsPropEatName")]
pub unsafe fn xml_new_ns_prop_eat_name(
    node: Option<XmlNodePtr>,
    ns: Option<XmlNsPtr>,
    name: *mut XmlChar,
    value: *const XmlChar,
) -> Option<XmlAttrPtr> {
    if name.is_null() {
        return None;
    }

    let n = CStr::from_ptr(name as *const i8).to_string_lossy();
    let res = xml_new_prop_internal(node, ns, &n, value);
    xml_free(name as _);
    res
}

pub(super) unsafe fn xml_copy_prop_internal(
    doc: Option<XmlDocPtr>,
    target: Option<XmlNodePtr>,
    cur: XmlAttrPtr,
) -> Option<XmlAttrPtr> {
    if target.map_or(false, |target| {
        !matches!(target.element_type(), XmlElementType::XmlElementNode)
    }) {
        return None;
    }
    let mut ret = if let Some(target) = target {
        xml_new_doc_prop(target.doc, cur.name, null_mut())
    } else if let Some(doc) = doc {
        xml_new_doc_prop(Some(doc), cur.name, null_mut())
    } else if let Some(parent) = cur.parent() {
        xml_new_doc_prop(parent.doc, cur.name, null_mut())
    } else if let Some(children) = cur.children() {
        xml_new_doc_prop(children.doc, cur.name, null_mut())
    } else {
        xml_new_doc_prop(None, cur.name, null_mut())
    }?;
    ret.parent = NodePtr::from_ptr(target.map_or(null_mut(), |target| target.as_ptr()));

    if let Some((cur_ns, mut target)) = cur.ns.zip(target) {
        let prefix = cur_ns.prefix();
        let target_doc = target.doc;
        if let Some(ns) = target.search_ns(target_doc, prefix.as_deref()) {
            // we have to find something appropriate here since
            // we can't be sure, that the namespace we found is identified
            // by the prefix
            if xml_str_equal(ns.href, cur_ns.href) {
                // this is the nice case
                ret.ns = Some(ns);
            } else {
                // we are in trouble: we need a new reconciled namespace.
                // This is expensive
                ret.ns = xml_new_reconciled_ns(target.doc, target.as_ptr(), cur_ns);
            }
        } else {
            // Humm, we are copying an element whose namespace is defined
            // out of the new tree scope. Search it in the original tree
            // and add it at the top of the new tree
            if let Some(ns) = cur.parent.unwrap().search_ns(cur.doc, prefix.as_deref()) {
                let mut root: *mut XmlNode = target.as_ptr();
                let mut pred: *mut XmlNode = null_mut();

                while let Some(parent) = (*root).parent() {
                    pred = root;
                    root = parent.as_ptr();
                }
                if root == target.doc.map_or(null_mut(), |doc| doc.as_ptr()) as _ {
                    // correct possibly cycling above the document elt
                    root = pred;
                }
                ret.ns = xml_new_ns(root, ns.href, ns.prefix().as_deref());
            }
        }
    } else {
        ret.ns = None;
    }

    if let Some(children) = cur.children() {
        ret.children = NodePtr::from_ptr(xml_static_copy_node_list(
            children.as_ptr(),
            ret.doc,
            ret.as_ptr() as _,
        ));
        ret.last = None;
        let mut tmp = ret.children;
        while let Some(now) = tmp {
            // (*tmp).parent = ret;
            if now.next.is_none() {
                ret.last = Some(now);
            }
            tmp = now.next;
        }
    }
    // Try to handle IDs
    if let Some(target_doc) = target.and_then(|target| target.doc).filter(|_| {
        cur.doc.map_or(false, |doc| doc.ids.is_some())
            && cur
                .parent
                .filter(|p| {
                    xml_is_id(
                        cur.doc,
                        XmlNodePtr::from_raw(p.as_ptr()).unwrap(),
                        Some(cur),
                    ) != 0
                })
                .is_some()
    }) {
        let children = cur.children;
        if let Some(id) = children.and_then(|c| c.get_string(cur.doc, 1)) {
            xml_add_id(null_mut(), target_doc, &id, ret);
        }
    }
    Some(ret)
}

/// Do a copy of the attribute.
///
/// Returns: a new #xmlAttrPtr, or null_mut() in case of error.
#[doc(alias = "xmlCopyProp")]
pub unsafe fn xml_copy_prop(target: Option<XmlNodePtr>, cur: XmlAttrPtr) -> Option<XmlAttrPtr> {
    xml_copy_prop_internal(None, target, cur)
}

/// Do a copy of an attribute list.
///
/// Returns: a new #xmlAttrPtr, or null_mut() in case of error.
#[doc(alias = "xmlCopyPropList")]
pub unsafe fn xml_copy_prop_list(
    target: Option<XmlNodePtr>,
    mut cur: Option<XmlAttrPtr>,
) -> Option<XmlAttrPtr> {
    if target.map_or(false, |target| {
        !matches!(target.element_type(), XmlElementType::XmlElementNode)
    }) {
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

/// Free one attribute, all the content is freed too
#[doc(alias = "xmlFreeProp")]
pub unsafe fn xml_free_prop(cur: XmlAttrPtr) {
    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    // && xmlDeregisterNodeDefaultValue.is_some()
    {
        xml_deregister_node_default_value(cur.as_ptr() as _);
    }

    // Check for ID removal -> leading to invalid references !
    if let Some(doc) = cur
        .doc
        .filter(|_| matches!(cur.atype, Some(XmlAttributeType::XmlAttributeID)))
    {
        xml_remove_id(doc, cur);
    }
    if let Some(children) = cur.children() {
        xml_free_node_list(children.as_ptr());
    }
    if !cur.name.is_null() {
        xml_free(cur.name as _);
    }
    cur.free();
}

/// Free a property and all its siblings, all the children are freed too.
#[doc(alias = "xmlFreePropList")]
pub unsafe fn xml_free_prop_list(cur: Option<XmlAttrPtr>) {
    if let Some(cur) = cur {
        let mut next = cur.next;
        xml_free_prop(cur);
        while let Some(now) = next {
            next = now.next;
            xml_free_prop(now);
        }
    }
}
