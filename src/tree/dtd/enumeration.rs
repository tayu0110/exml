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

/// List structure used when there is an enumeration in DTDs.
pub type XmlEnumerationPtr = *mut XmlEnumeration;
#[repr(C)]
pub struct XmlEnumeration {
    pub(crate) next: *mut XmlEnumeration, /* next one */
    pub(crate) name: Option<String>,      /* Enumeration name */
}

impl Default for XmlEnumeration {
    fn default() -> Self {
        Self {
            next: null_mut(),
            name: None,
        }
    }
}
