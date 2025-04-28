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
    cell::RefCell,
    ops::{Deref, DerefMut},
    os::raw::c_void,
    ptr::{NonNull, null_mut},
    rc::Rc,
};

#[cfg(feature = "libxml_regexp")]
use crate::libxml::xmlregexp::XmlRegexp;
use crate::tree::{
    InvalidNodePointerCastError, NodeCommon, XmlDocPtr, XmlElementContent, XmlElementType,
    XmlElementTypeVal, XmlGenericNodePtr,
};

use super::{XmlAttributePtr, XmlDtdPtr};

#[repr(C)]
pub struct XmlElement {
    pub _private: *mut c_void,                      /* application data */
    pub(crate) typ: XmlElementType,                 /* XML_ELEMENT_DECL, must be second ! */
    pub(crate) name: Option<String>,                /* Element name */
    pub(crate) children: Option<XmlGenericNodePtr>, /* NULL */
    pub(crate) last: Option<XmlGenericNodePtr>,     /* NULL */
    pub(crate) parent: Option<XmlDtdPtr>,           /* -> DTD */
    pub(crate) next: Option<XmlGenericNodePtr>,     /* next sibling link  */
    pub(crate) prev: Option<XmlGenericNodePtr>,     /* previous sibling link  */
    pub(crate) doc: Option<XmlDocPtr>,              /* the containing document */

    pub(crate) etype: XmlElementTypeVal, /* The type */
    pub(crate) content: Option<Rc<RefCell<XmlElementContent>>>, /* the allowed element content */
    pub(crate) attributes: Option<XmlAttributePtr>, /* List of the declared attributes */
    pub(crate) prefix: Option<String>,   /* the namespace prefix if any */
    #[cfg(feature = "libxml_regexp")]
    pub(crate) cont_model: Option<Rc<XmlRegexp>>, /* the validating regexp */
    #[cfg(not(feature = "libxml_regexp"))]
    pub(crate) cont_model: *mut c_void,
}

impl Default for XmlElement {
    fn default() -> Self {
        Self {
            _private: null_mut(),
            typ: XmlElementType::XmlElementDecl,
            name: None,
            children: None,
            last: None,
            parent: None,
            next: None,
            prev: None,
            doc: None,
            etype: XmlElementTypeVal::XmlElementTypeUndefined,
            content: None,
            attributes: None,
            prefix: None,
            cont_model: None,
        }
    }
}

impl NodeCommon for XmlElement {
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
        self.name.as_deref().map(Cow::Borrowed)
    }
    fn children(&self) -> Option<XmlGenericNodePtr> {
        self.children
    }
    fn set_children(&mut self, children: Option<XmlGenericNodePtr>) {
        self.children = children;
    }
    fn last(&self) -> Option<XmlGenericNodePtr> {
        self.last
    }
    fn set_last(&mut self, last: Option<XmlGenericNodePtr>) {
        self.last = last;
    }
    fn next(&self) -> Option<XmlGenericNodePtr> {
        self.next
    }
    fn set_next(&mut self, next: Option<XmlGenericNodePtr>) {
        self.next = next;
    }
    fn prev(&self) -> Option<XmlGenericNodePtr> {
        self.prev
    }
    fn set_prev(&mut self, prev: Option<XmlGenericNodePtr>) {
        self.prev = prev;
    }
    fn parent(&self) -> Option<XmlGenericNodePtr> {
        self.parent.map(|node| node.into())
    }
    fn set_parent(&mut self, parent: Option<XmlGenericNodePtr>) {
        self.parent = parent.map(|p| XmlDtdPtr::try_from(p).unwrap());
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub struct XmlElementPtr(NonNull<XmlElement>);

impl XmlElementPtr {
    /// Allocate new memory and create new `XmlElementPtr` from an owned xml node.
    ///
    /// This method leaks allocated memory.  
    /// Users can use `free` method for deallocating memory.
    pub(crate) fn new(node: XmlElement) -> Option<Self> {
        let boxed = Box::new(node);
        NonNull::new(Box::leak(boxed)).map(Self)
    }

    // /// Create `XmlElementPtr` from a raw pointer.
    // ///
    // /// If `ptr` is a NULL pointer, return `Ok(None)`.
    // /// If `ptr` is a valid pointer of `XmlElement`, return `Ok(Some(Self))`.
    // /// Otherwise, return `Err`.
    // ///
    // /// # Safety
    // /// - `ptr` must be a pointer of types that is implemented `NodeCommon` at least.
    // pub(crate) unsafe fn from_raw(
    //     ptr: *mut XmlElement,
    // ) -> Result<Option<Self>, InvalidNodePointerCastError> {
    //     unsafe {
    //         if ptr.is_null() {
    //             return Ok(None);
    //         }
    //         match (*ptr).element_type() {
    //             XmlElementType::XmlElementDecl => Ok(Some(Self(NonNull::new_unchecked(ptr)))),
    //             _ => Err(InvalidNodePointerCastError {
    //                 from: (*ptr).element_type(),
    //                 to: type_name::<Self>(),
    //             }),
    //         }
    //     }
    // }

    // pub(crate) fn as_ptr(self) -> *mut XmlElement {
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
    // pub(crate) unsafe fn into_inner(self) -> Box<XmlElement> {
    //     unsafe { Box::from_raw(self.0.as_ptr()) }
    // }
}

impl Clone for XmlElementPtr {
    fn clone(&self) -> Self {
        *self
    }
}

impl Copy for XmlElementPtr {}

impl Deref for XmlElementPtr {
    type Target = XmlElement;
    fn deref(&self) -> &Self::Target {
        // # Safety
        // I don't implement the pointer casting and addition/subtraction methods
        // and don't expose the inner `NonNull` for `*mut XmlElement`.
        // Therefore, as long as the constructor is correctly implemented,
        // the pointer dereference is valid.
        unsafe { self.0.as_ref() }
    }
}

impl DerefMut for XmlElementPtr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // # Safety
        // I don't implement the pointer casting and addition/subtraction methods
        // and don't expose the inner `NonNull` for `*mut XmlElement`.
        // Therefore, as long as the constructor is correctly implemented,
        // the pointer dereference is valid.
        unsafe { self.0.as_mut() }
    }
}

impl TryFrom<XmlGenericNodePtr> for XmlElementPtr {
    type Error = InvalidNodePointerCastError;

    fn try_from(value: XmlGenericNodePtr) -> Result<Self, Self::Error> {
        match value.element_type() {
            XmlElementType::XmlElementDecl => Ok(Self(value.0.cast())),
            _ => Err(InvalidNodePointerCastError {
                from: value.element_type(),
                to: type_name::<Self>(),
            }),
        }
    }
}

impl From<XmlElementPtr> for XmlGenericNodePtr {
    fn from(value: XmlElementPtr) -> Self {
        Self(value.0 as NonNull<dyn NodeCommon>)
    }
}

impl From<XmlElementPtr> for *mut XmlElement {
    fn from(value: XmlElementPtr) -> Self {
        value.0.as_ptr()
    }
}

/// Build a copy of an element.
///
/// Returns the new xmlElementPtr or null_mut() in case of error.
#[doc(alias = "xmlCopyElement")]
#[cfg(feature = "libxml_tree")]
pub(crate) fn xml_copy_element(elem: XmlElementPtr) -> Option<XmlElementPtr> {
    use crate::libxml::valid::{xml_copy_element_content, xml_verr_memory};

    let res = XmlElementPtr::new(XmlElement {
        typ: XmlElementType::XmlElementDecl,
        etype: elem.etype,
        name: elem.name.clone(),
        prefix: elem.prefix.clone(),
        content: xml_copy_element_content(elem.content.clone()),
        // TODO : rebuild the attribute list on the copy
        attributes: None,
        ..Default::default()
    });
    if res.is_none() {
        xml_verr_memory(None, Some("malloc failed"));
    }
    res
}

/// Deallocate the memory used by an element definition
#[doc(alias = "xmlFreeElement")]
pub(crate) unsafe fn xml_free_element(elem: Option<XmlElementPtr>) {
    unsafe {
        let Some(mut elem) = elem else {
            return;
        };
        elem.unlink();
        elem.free();
    }
}
