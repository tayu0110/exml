//! Provide methods and data structures for parsing XML documents.
//!
//! This module is based on `libxml/parser.h`, `parser.c`, and so on in `libxml2-v2.11.8`.  
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: the core parser module
// Description: Interfaces, constants and types related to the XML parser
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// parser.c : an XML 1.0 parser, namespaces and validity support are mostly
//            implemented on top of the SAX interfaces
//
// References:
//   The XML specification:
//     http://www.w3.org/TR/REC-xml
//   Original 1.0 version:
//     http://www.w3.org/TR/1998/REC-xml-19980210
//   XML second edition working draft
//     http://www.w3.org/TR/2000/WD-xml-2e-20000814
//
// Okay this is a big file, the parser core is around 7000 lines, then it
// is followed by the progressive parser top routines, then the various
// high level APIs to call the parser and a few miscellaneous functions.
// A number of helper functions and deprecated ones have been moved to
// parserInternals.c to reduce this file size.
// As much as possible the functions are associated with their relative
// production in the XML specification. A few productions defining the
// different ranges of character are actually implanted either in
// parserInternals.h or parserInternals.c
// The DOM tree build is realized from the default SAX callbacks in
// the module SAX.c.
// The routines doing the validation checks are in valid.c and called either
// from the SAX callbacks or as standalone functions using a preparsed
// document.
//
// See Copyright for the status of this software.
//
// daniel@veillard.com
mod attribute;
mod cdata;
mod comment;
mod dtd;
mod element;
mod entity;
mod literal;
mod names;
mod pi;
mod reference;
mod xmldecl;

pub(crate) use attribute::*;
pub(crate) use cdata::*;
pub(crate) use entity::*;

use crate::libxml::parser::XmlParserInputState;

use super::XmlParserCtxt;

/// Parse an XML Misc* optional field.
///
/// ```text
/// [27] Misc ::= Comment | PI |  S
/// ```
#[doc(alias = "xmlParseMisc")]
pub(crate) unsafe fn parse_misc(ctxt: &mut XmlParserCtxt) {
    unsafe {
        while !matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            ctxt.skip_blanks();
            ctxt.grow();
            if ctxt.content_bytes().starts_with(b"<?") {
                ctxt.parse_pi();
            } else if ctxt.content_bytes().starts_with(b"<!--") {
                ctxt.parse_comment();
            } else {
                break;
            }
        }
    }
}
