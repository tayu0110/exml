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

use std::ptr::null_mut;

use super::{XmlAttrPtr, XmlDoc};

/// An XML ID instance.
#[repr(C)]
pub struct XmlID {
    pub(crate) next: Option<Box<XmlID>>, /* next ID */
    pub(crate) value: String,            /* The ID name */
    pub(crate) attr: XmlAttrPtr,         /* The attribute holding it */
    pub(crate) name: Option<String>,     /* The attribute if attr is not available */
    pub(crate) lineno: i32,              /* The line number if attr is not available */
    pub(crate) doc: *mut XmlDoc,         /* The document holding the ID */
}

impl Default for XmlID {
    fn default() -> Self {
        Self {
            next: None,
            value: "".to_owned(),
            attr: null_mut(),
            name: None,
            lineno: 0,
            doc: null_mut(),
        }
    }
}

/// An XML IDREF instance.
pub type XmlRefPtr = *mut XmlRef;
#[repr(C)]
pub struct XmlRef {
    pub(crate) next: Option<Box<XmlRef>>, /* next Ref */
    pub(crate) value: String,             /* The Ref name */
    pub(crate) attr: XmlAttrPtr,          /* The attribute holding it */
    pub(crate) name: Option<String>,      /* The attribute if attr is not available */
    pub(crate) lineno: i32,               /* The line number if attr is not available */
}

impl Default for XmlRef {
    fn default() -> Self {
        Self {
            next: None,
            value: "".to_owned(),
            attr: null_mut(),
            name: None,
            lineno: 0,
        }
    }
}
