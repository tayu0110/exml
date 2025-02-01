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
    xml_free_node_list, xml_new_doc_text, xml_new_ns, xml_new_reconciled_ns,
    xml_static_copy_node_list, xml_tree_err_memory, InvalidNodePointerCastError, NodeCommon,
    NodePtr, XmlAttributeType, XmlDoc, XmlElementType, XmlGenericNodePtr, XmlNode, XmlNsPtr,
    __XML_REGISTER_CALLBACKS,
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
    pub(crate) prev: *mut XmlAttr,              /* previous sibling link  */
    pub(crate) doc: *mut XmlDoc,                /* the containing document */
    pub(crate) ns: Option<XmlNsPtr>,            /* pointer to the associated namespace */
    pub(crate) atype: Option<XmlAttributeType>, /* the attribute type if validating */
    pub(crate) psvi: *mut c_void,               /* for type/PSVI information */
}

impl Default for XmlAttr {
    fn default() -> Self {
        Self {
            _private: null_mut(),
            typ: XmlElementType::default(),
            name: null_mut(),
            children: None,
            last: None,
            parent: None,
            next: None,
            prev: null_mut(),
            doc: null_mut(),
            ns: None,
            atype: None,
            psvi: null_mut(),
        }
    }
}

impl NodeCommon for XmlAttr {
    fn document(&self) -> *mut XmlDoc {
        self.doc
    }
    fn set_document(&mut self, doc: *mut XmlDoc) {
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
        NodePtr::from_ptr(self.prev as *mut XmlNode)
    }
    fn set_prev(&mut self, prev: Option<NodePtr>) {
        self.prev = prev.map_or(null_mut(), |p| p.as_ptr()) as *mut XmlAttr;
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
        let mut tmp = XmlAttrPtr::from_raw(parent.properties).unwrap();
        if tmp == Some(*self) {
            parent.properties = self.next.map_or(null_mut(), |next| next.as_ptr());
            if let Some(mut next) = self.next {
                next.prev = null_mut();
            }
            xml_free_prop(*self);
            return 0;
        }
        while let Some(mut now) = tmp {
            if now.next == Some(*self) {
                now.next = self.next;
                if let Some(mut next) = now.next {
                    next.prev = now.as_ptr();
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
    doc: *mut XmlDoc,
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
        cur.children = (!doc.is_null())
            .then(|| NodePtr::from_ptr((*doc).get_node_list(value)))
            .flatten();
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
    node: *mut XmlNode,
    ns: Option<XmlNsPtr>,
    name: &str,
    value: *const XmlChar,
) -> Option<XmlAttrPtr> {
    let mut doc: *mut XmlDoc = null_mut();

    if !node.is_null() && !matches!((*node).element_type(), XmlElementType::XmlElementNode) {
        return None;
    }

    // Allocate a new property and fill the fields.
    let Some(mut cur) = XmlAttrPtr::new(XmlAttr {
        typ: XmlElementType::XmlAttributeNode,
        parent: NodePtr::from_ptr(node),
        ns,
        name: xml_strndup(name.as_ptr(), name.len() as i32),
        ..Default::default()
    }) else {
        xml_tree_err_memory("building attribute");
        return None;
    };

    if !node.is_null() {
        doc = (*node).doc;
        cur.doc = doc;
    }

    if !value.is_null() {
        cur.children = NodePtr::from_ptr(xml_new_doc_text(doc, value));
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
    if !node.is_null() {
        if let Some(mut prev) = XmlAttrPtr::from_raw((*node).properties).unwrap() {
            while let Some(next) = prev.next {
                prev = next;
            }
            prev.next = Some(cur);
            cur.prev = prev.as_ptr();
        } else {
            (*node).properties = cur.as_ptr();
        }
    }

    if !value.is_null() && !node.is_null() && xml_is_id((*node).doc, node, Some(cur)) == 1 {
        xml_add_id(
            null_mut(),
            (*node).doc,
            CStr::from_ptr(value as *const i8)
                .to_string_lossy()
                .as_ref(),
            cur,
        );
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
    node: *mut XmlNode,
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
    node: *mut XmlNode,
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
    node: *mut XmlNode,
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
    doc: *mut XmlDoc,
    target: *mut XmlNode,
    cur: XmlAttrPtr,
) -> Option<XmlAttrPtr> {
    if !target.is_null() && !matches!((*target).element_type(), XmlElementType::XmlElementNode) {
        return None;
    }
    let mut ret = if !target.is_null() {
        xml_new_doc_prop((*target).doc, cur.name, null_mut())
    } else if !doc.is_null() {
        xml_new_doc_prop(doc, cur.name, null_mut())
    } else if let Some(parent) = cur.parent() {
        xml_new_doc_prop(parent.doc, cur.name, null_mut())
    } else if let Some(children) = cur.children() {
        xml_new_doc_prop(children.doc, cur.name, null_mut())
    } else {
        xml_new_doc_prop(null_mut(), cur.name, null_mut())
    }?;
    ret.parent = NodePtr::from_ptr(target);

    if let Some(cur_ns) = cur.ns.filter(|_| !target.is_null()) {
        let prefix = cur_ns.prefix();
        if let Some(ns) = (*target).search_ns((*target).doc, prefix.as_deref()) {
            // we have to find something appropriate here since
            // we can't be sure, that the namespace we found is identified
            // by the prefix
            if xml_str_equal(ns.href, cur_ns.href) {
                // this is the nice case
                ret.ns = Some(ns);
            } else {
                // we are in trouble: we need a new reconciled namespace.
                // This is expensive
                ret.ns = xml_new_reconciled_ns((*target).doc, target, cur_ns);
            }
        } else {
            // Humm, we are copying an element whose namespace is defined
            // out of the new tree scope. Search it in the original tree
            // and add it at the top of the new tree
            if let Some(ns) = cur.parent.unwrap().search_ns(cur.doc, prefix.as_deref()) {
                let mut root: *mut XmlNode = target;
                let mut pred: *mut XmlNode = null_mut();

                while let Some(parent) = (*root).parent() {
                    pred = root;
                    root = parent.as_ptr();
                }
                if root == (*target).doc as _ {
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
    if !target.is_null()
        && !(*target).doc.is_null()
        && !cur.doc.is_null()
        && (*cur.doc).ids.is_some()
        && cur
            .parent
            .filter(|p| xml_is_id(cur.doc, p.as_ptr(), Some(cur)) != 0)
            .is_some()
    {
        let children = cur.children;
        if let Some(id) = children.and_then(|c| c.get_string(cur.doc, 1)) {
            xml_add_id(null_mut(), (*target).doc, &id, ret);
        }
    }
    Some(ret)
}

/// Do a copy of the attribute.
///
/// Returns: a new #xmlAttrPtr, or null_mut() in case of error.
#[doc(alias = "xmlCopyProp")]
pub unsafe fn xml_copy_prop(target: *mut XmlNode, cur: XmlAttrPtr) -> Option<XmlAttrPtr> {
    xml_copy_prop_internal(null_mut(), target, cur)
}

/// Do a copy of an attribute list.
///
/// Returns: a new #xmlAttrPtr, or null_mut() in case of error.
#[doc(alias = "xmlCopyPropList")]
pub unsafe fn xml_copy_prop_list(
    target: *mut XmlNode,
    mut cur: Option<XmlAttrPtr>,
) -> Option<XmlAttrPtr> {
    if !target.is_null() && !matches!((*target).element_type(), XmlElementType::XmlElementNode) {
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
            q.prev = np.as_ptr();
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
    if !cur.doc.is_null() && matches!(cur.atype, Some(XmlAttributeType::XmlAttributeID)) {
        xml_remove_id(cur.doc, cur);
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
