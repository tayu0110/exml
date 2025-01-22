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
};

use libc::memset;

use crate::libxml::{
    globals::{xml_free, xml_malloc},
    xmlstring::{xml_str_equal, xml_strdup, xml_strndup, XmlChar},
};

use super::{
    xml_tree_err_memory, InvalidNodePointerCastError, NodeCommon, NodePtr, XmlDoc, XmlElementType,
    XmlGenericNodePtr, XmlNode, XmlNsType, XML_LOCAL_NAMESPACE, XML_XML_NAMESPACE,
};

#[repr(C)]
pub struct XmlNs {
    pub next: *mut XmlNs,             /* next Ns link for this node  */
    pub(crate) typ: XmlNsType,        /* global or local */
    pub href: *const XmlChar,         /* URL for the namespace */
    pub prefix: *const XmlChar,       /* prefix for the namespace */
    pub(crate) _private: *mut c_void, /* application data */
    pub(crate) context: *mut XmlDoc,  /* normally an xmlDoc */
}

impl XmlNs {
    pub unsafe fn prefix(&self) -> Option<Cow<'_, str>> {
        let prefix = self.prefix;
        (!prefix.is_null()).then(|| CStr::from_ptr(prefix as *const i8).to_string_lossy())
    }

    pub unsafe fn href(&self) -> Option<Cow<'_, str>> {
        let href = self.href;
        (!href.is_null()).then(|| CStr::from_ptr(href as *const i8).to_string_lossy())
    }
}

impl Default for XmlNs {
    fn default() -> Self {
        Self {
            next: null_mut(),
            typ: XmlNsType::XmlInvalidNode,
            href: null_mut(),
            prefix: null_mut(),
            _private: null_mut(),
            context: null_mut(),
        }
    }
}

impl NodeCommon for XmlNs {
    fn document(&self) -> *mut XmlDoc {
        self.context
    }
    fn set_document(&mut self, doc: *mut XmlDoc) {
        self.context = doc;
    }
    fn element_type(&self) -> XmlElementType {
        self.typ
    }
    fn name(&self) -> Option<Cow<'_, str>> {
        (!self.href.is_null())
            .then(|| unsafe { CStr::from_ptr(self.href as *const i8).to_string_lossy() })
    }
    fn children(&self) -> Option<NodePtr> {
        None
    }
    fn set_children(&mut self, _children: Option<NodePtr>) {}
    fn last(&self) -> Option<NodePtr> {
        None
    }
    fn set_last(&mut self, _last: Option<NodePtr>) {}
    fn next(&self) -> Option<NodePtr> {
        NodePtr::from_ptr(self.next as *mut XmlNode)
    }
    fn set_next(&mut self, next: Option<NodePtr>) {
        self.next = next.map_or(null_mut(), |p| p.as_ptr() as *mut XmlNs);
    }
    fn prev(&self) -> Option<NodePtr> {
        None
    }
    fn set_prev(&mut self, _prev: Option<NodePtr>) {}
    fn parent(&self) -> Option<NodePtr> {
        None
    }
    fn set_parent(&mut self, _parent: Option<NodePtr>) {}
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub struct XmlNsPtr(NonNull<XmlNs>);

impl XmlNsPtr {
    /// Allocate new memory and create new `XmlNsPtr` from an owned xml node.
    ///
    /// This method leaks allocated memory.  
    /// Users can use `free` method for deallocating memory.
    pub(crate) fn new(node: XmlNs) -> Option<Self> {
        let boxed = Box::new(node);
        NonNull::new(Box::leak(boxed)).map(Self)
    }

    /// Create `XmlNsPtr` from a raw pointer.  
    ///
    /// If `ptr` is a NULL pointer, return `Ok(None)`.  
    /// If `ptr` is a valid pointer of `XmlNs`, return `Ok(Some(Self))`.  
    /// Otherwise, return `Err`.
    ///
    /// # Safety
    /// - `ptr` must be a pointer of types that is implemented `NodeCommon` at least.
    pub(crate) unsafe fn from_raw(
        ptr: *mut XmlNs,
    ) -> Result<Option<Self>, InvalidNodePointerCastError> {
        if ptr.is_null() {
            return Ok(None);
        }
        match (*ptr).element_type() {
            XmlElementType::XmlNamespaceDecl => Ok(Some(Self(NonNull::new_unchecked(ptr)))),
            _ => Err(InvalidNodePointerCastError {
                from: (*ptr).element_type(),
                to: type_name::<Self>(),
            }),
        }
    }

    pub(crate) fn as_ptr(self) -> *mut XmlNs {
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
    pub(crate) unsafe fn into_inner(self) -> Box<XmlNs> {
        Box::from_raw(self.0.as_ptr())
    }
}

impl Clone for XmlNsPtr {
    fn clone(&self) -> Self {
        *self
    }
}

impl Copy for XmlNsPtr {}

impl Deref for XmlNsPtr {
    type Target = XmlNs;
    fn deref(&self) -> &Self::Target {
        // # Safety
        // I don't implement the pointer casting and addition/subtraction methods
        // and don't expose the inner `NonNull` for `*mut XmlNs`.
        // Therefore, as long as the constructor is correctly implemented,
        // the pointer dereference is valid.
        unsafe { self.0.as_ref() }
    }
}

impl DerefMut for XmlNsPtr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // # Safety
        // I don't implement the pointer casting and addition/subtraction methods
        // and don't expose the inner `NonNull` for `*mut XmlNs`.
        // Therefore, as long as the constructor is correctly implemented,
        // the pointer dereference is valid.
        unsafe { self.0.as_mut() }
    }
}

impl TryFrom<XmlGenericNodePtr> for XmlNsPtr {
    type Error = InvalidNodePointerCastError;

    fn try_from(value: XmlGenericNodePtr) -> Result<Self, Self::Error> {
        match value.element_type() {
            XmlElementType::XmlNamespaceDecl => Ok(Self(value.0.cast())),
            _ => Err(InvalidNodePointerCastError {
                from: value.element_type(),
                to: type_name::<Self>(),
            }),
        }
    }
}

impl From<XmlNsPtr> for XmlGenericNodePtr {
    fn from(value: XmlNsPtr) -> Self {
        Self(value.0 as NonNull<dyn NodeCommon>)
    }
}

impl From<XmlNsPtr> for *mut XmlNs {
    fn from(value: XmlNsPtr) -> Self {
        value.0.as_ptr()
    }
}

/// Creation of a new Namespace. This function will refuse to create
/// a namespace with a similar prefix than an existing one present on this node.
/// Note that for a default namespace, `prefix` should be NULL.
///
/// We use href == NULL in the case of an element creation where the namespace was not defined.
///
/// Returns a new namespace pointer or NULL
#[doc(alias = "xmlNewNs")]
pub unsafe fn xml_new_ns(
    node: *mut XmlNode,
    href: *const XmlChar,
    prefix: Option<&str>,
) -> *mut XmlNs {
    if !node.is_null() && !matches!((*node).element_type(), XmlElementType::XmlElementNode) {
        return null_mut();
    }

    if prefix == Some("xml") {
        /* xml namespace is predefined, no need to add it */
        if xml_str_equal(href, XML_XML_NAMESPACE.as_ptr() as _) {
            return null_mut();
        }

        // Problem, this is an attempt to bind xml prefix to a wrong
        // namespace, which breaks
        // Namespace constraint: Reserved Prefixes and Namespace Names
        // from XML namespace. But documents authors may not care in
        // their context so let's proceed.
    }

    // Allocate a new Namespace and fill the fields.
    let cur: *mut XmlNs = xml_malloc(size_of::<XmlNs>()) as _;
    if cur.is_null() {
        xml_tree_err_memory("building namespace");
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlNs>());
    (*cur).typ = XML_LOCAL_NAMESPACE;

    if !href.is_null() {
        (*cur).href = xml_strdup(href);
    }
    if let Some(prefix) = prefix {
        (*cur).prefix = xml_strndup(prefix.as_ptr(), prefix.len() as i32);
    }

    // Add it at the end to preserve parsing order ...
    // and checks for existing use of the prefix
    if !node.is_null() {
        if (*node).ns_def.is_null() {
            (*node).ns_def = cur;
        } else {
            let mut prev: *mut XmlNs = (*node).ns_def;

            if ((*prev).prefix.is_null() && (*cur).prefix.is_null())
                || xml_str_equal((*prev).prefix, (*cur).prefix)
            {
                xml_free_ns(cur);
                return null_mut();
            }
            while !(*prev).next.is_null() {
                prev = (*prev).next;
                if ((*prev).prefix.is_null() && (*cur).prefix.is_null())
                    || xml_str_equal((*prev).prefix, (*cur).prefix)
                {
                    xml_free_ns(cur);
                    return null_mut();
                }
            }
            (*prev).next = cur;
        }
    }
    cur
}

/// Free up the structures associated to a namespace
#[doc(alias = "xmlFreeNs")]
pub unsafe extern "C" fn xml_free_ns(cur: *mut XmlNs) {
    if cur.is_null() {
        return;
    }
    if !(*cur).href.is_null() {
        xml_free((*cur).href as _);
    }
    if !(*cur).prefix.is_null() {
        xml_free((*cur).prefix as _);
    }
    xml_free(cur as _);
}

/// Free up all the structures associated to the chained namespaces.
#[doc(alias = "xmlFreeNsList")]
pub unsafe fn xml_free_ns_list(mut cur: *mut XmlNs) {
    let mut next: *mut XmlNs;
    if cur.is_null() {
        return;
    }
    while !cur.is_null() {
        next = (*cur).next;
        xml_free_ns(cur);
        cur = next;
    }
}
