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
    ptr::{drop_in_place, null_mut, NonNull},
};

use crate::{
    libxml::{globals::xml_free, xmlstring::XmlChar},
    tree::{
        InvalidNodePointerCastError, NodeCommon, NodePtr, XmlAttributeDefault, XmlAttributeType,
        XmlDoc, XmlDtd, XmlElementType, XmlGenericNodePtr, XmlNode,
    },
};

use super::XmlEnumeration;

#[repr(C)]
#[derive(Clone)]
pub struct XmlAttribute {
    pub(crate) _private: *mut c_void,     /* application data */
    pub(crate) typ: XmlElementType,       /* XML_ATTRIBUTE_DECL, must be second ! */
    pub(crate) name: *const XmlChar,      /* Attribute name */
    pub(crate) children: Option<NodePtr>, /* NULL */
    pub(crate) last: Option<NodePtr>,     /* NULL */
    pub(crate) parent: *mut XmlDtd,       /* -> DTD */
    pub(crate) next: Option<NodePtr>,     /* next sibling link  */
    pub(crate) prev: Option<NodePtr>,     /* previous sibling link  */
    pub(crate) doc: *mut XmlDoc,          /* the containing document */

    pub(crate) nexth: Option<XmlAttributePtr>, /* next in hash table */
    pub(crate) atype: XmlAttributeType,        /* The attribute type */
    pub(crate) def: XmlAttributeDefault,       /* the default */
    pub(crate) default_value: *const XmlChar,  /* or the default value */
    pub(crate) tree: Option<Box<XmlEnumeration>>, /* or the enumeration tree if any */
    pub(crate) prefix: Option<String>,         /* the namespace prefix if any */
    pub(crate) elem: Option<String>,           /* Element holding the attribute */
}

impl Default for XmlAttribute {
    fn default() -> Self {
        Self {
            _private: null_mut(),
            typ: XmlElementType::default(),
            name: null_mut(),
            children: None,
            last: None,
            parent: null_mut(),
            next: None,
            prev: None,
            doc: null_mut(),
            nexth: None,
            atype: XmlAttributeType::XmlAttributeCDATA,
            def: XmlAttributeDefault::XmlAttributeNone,
            default_value: null_mut(),
            tree: None,
            prefix: None,
            elem: None,
        }
    }
}

impl NodeCommon for XmlAttribute {
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
    }
    fn set_next(&mut self, next: Option<NodePtr>) {
        self.next = next;
    }
    fn prev(&self) -> Option<NodePtr> {
        self.prev
    }
    fn set_prev(&mut self, prev: Option<NodePtr>) {
        self.prev = prev;
    }
    fn parent(&self) -> Option<NodePtr> {
        NodePtr::from_ptr(self.parent as *mut XmlNode)
    }
    fn set_parent(&mut self, parent: Option<NodePtr>) {
        self.parent = parent.map_or(null_mut(), |p| p.as_ptr()) as *mut XmlDtd;
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub struct XmlAttributePtr(NonNull<XmlAttribute>);

impl XmlAttributePtr {
    /// Allocate new memory and create new `XmlAttributePtr` from an owned xml node.
    ///
    /// This method leaks allocated memory.  
    /// Users can use `free` method for deallocating memory.
    pub(crate) fn new(node: XmlAttribute) -> Option<Self> {
        let boxed = Box::new(node);
        NonNull::new(Box::leak(boxed)).map(Self)
    }

    /// Create `XmlAttributePtr` from a raw pointer.  
    ///
    /// If `ptr` is a NULL pointer, return `Ok(None)`.  
    /// If `ptr` is a valid pointer of `XmlAttribute`, return `Ok(Some(Self))`.  
    /// Otherwise, return `Err`.
    ///
    /// # Safety
    /// - `ptr` must be a pointer of types that is implemented `NodeCommon` at least.
    pub(crate) unsafe fn from_raw(
        ptr: *mut XmlAttribute,
    ) -> Result<Option<Self>, InvalidNodePointerCastError> {
        if ptr.is_null() {
            return Ok(None);
        }
        match (*ptr).element_type() {
            XmlElementType::XmlAttributeDecl => Ok(Some(Self(NonNull::new_unchecked(ptr)))),
            _ => Err(InvalidNodePointerCastError {
                from: (*ptr).element_type(),
                to: type_name::<Self>(),
            }),
        }
    }

    pub(crate) fn as_ptr(self) -> *mut XmlAttribute {
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
    pub(crate) unsafe fn into_inner(self) -> Box<XmlAttribute> {
        Box::from_raw(self.0.as_ptr())
    }
}

impl Clone for XmlAttributePtr {
    fn clone(&self) -> Self {
        *self
    }
}

impl Copy for XmlAttributePtr {}

impl Deref for XmlAttributePtr {
    type Target = XmlAttribute;
    fn deref(&self) -> &Self::Target {
        // # Safety
        // I don't implement the pointer casting and addition/subtraction methods
        // and don't expose the inner `NonNull` for `*mut XmlAttribute`.
        // Therefore, as long as the constructor is correctly implemented,
        // the pointer dereference is valid.
        unsafe { self.0.as_ref() }
    }
}

impl DerefMut for XmlAttributePtr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // # Safety
        // I don't implement the pointer casting and addition/subtraction methods
        // and don't expose the inner `NonNull` for `*mut XmlAttribute`.
        // Therefore, as long as the constructor is correctly implemented,
        // the pointer dereference is valid.
        unsafe { self.0.as_mut() }
    }
}

impl TryFrom<XmlGenericNodePtr> for XmlAttributePtr {
    type Error = InvalidNodePointerCastError;

    fn try_from(value: XmlGenericNodePtr) -> Result<Self, Self::Error> {
        match value.element_type() {
            XmlElementType::XmlAttributeDecl => Ok(Self(value.0.cast())),
            _ => Err(InvalidNodePointerCastError {
                from: value.element_type(),
                to: type_name::<Self>(),
            }),
        }
    }
}

impl From<XmlAttributePtr> for XmlGenericNodePtr {
    fn from(value: XmlAttributePtr) -> Self {
        Self(value.0 as NonNull<dyn NodeCommon>)
    }
}

impl From<XmlAttributePtr> for *mut XmlAttribute {
    fn from(value: XmlAttributePtr) -> Self {
        value.0.as_ptr()
    }
}

/// Deallocate the memory used by an attribute definition
#[doc(alias = "xmlFreeAttribute")]
pub(crate) unsafe fn xml_free_attribute(attr: *mut XmlAttribute) {
    if attr.is_null() {
        return;
    }
    (*attr).unlink();
    (*attr).elem = None;
    if !(*attr).name.is_null() {
        xml_free((*attr).name as _);
    }
    if !(*attr).default_value.is_null() {
        xml_free((*attr).default_value as _);
    }
    (*attr).prefix = None;
    drop_in_place(attr);
    xml_free(attr as _);
}
