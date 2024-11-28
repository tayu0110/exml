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

/// A DTD Notation definition.
pub type XmlNotationPtr = *mut XmlNotation;
#[repr(C)]
#[derive(Clone, Default)]
pub struct XmlNotation {
    pub(crate) name: Option<String>,      /* Notation name */
    pub(crate) public_id: Option<String>, /* Public identifier, if any */
    pub(crate) system_id: Option<String>, /* System identifier, if any */
}
