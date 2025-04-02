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

use crate::{
    encoding::{XmlCharEncoding, detect_encoding},
    error::XmlParserErrors,
    libxml::{
        chvalid::xml_is_blank_char, globals::xml_default_sax_locator, parser::xml_init_parser,
    },
    parser::XmlParserOption,
    tree::{XmlDocProperties, xml_free_doc},
};

use super::{XmlParserCtxt, XmlParserInputState, xml_fatal_err, xml_fatal_err_msg};

/// The default version of XML used: 1.0
pub(crate) const XML_DEFAULT_VERSION: &str = "1.0";

pub(crate) const SAX_COMPAT_MODE: &str = "SAX compatibility mode document";

impl XmlParserCtxt {
    /// Parse an XML document (and build a tree if using the standard SAX
    /// interface).
    ///
    /// ```text
    /// [1] document ::= prolog element Misc*
    /// [22] prolog ::= XMLDecl? Misc* (doctypedecl Misc*)?
    /// ```
    ///
    /// Returns 0, -1 in case of error. the parser context is augmented
    ///                as a result of the parsing.
    #[doc(alias = "xmlParseDocument")]
    pub unsafe fn parse_document(&mut self) -> i32 {
        unsafe {
            xml_init_parser();

            if self.input().is_none() {
                return -1;
            }

            self.grow();

            // SAX: detecting the level.
            self.detect_sax2();

            // SAX: beginning of the document processing.
            if let Some(sax) = self.sax.as_deref_mut() {
                if let Some(set_document_locator) = sax.set_document_locator {
                    set_document_locator(self.user_data.clone(), xml_default_sax_locator());
                }
            }
            if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                return -1;
            }

            if self.encoding().is_none() && self.input().unwrap().remainder_len() >= 4 {
                // Get the 4 first bytes and decode the charset
                // if enc != XML_CHAR_ENCODING_NONE
                // plug some encoding conversion routines.
                let enc = detect_encoding(&self.content_bytes()[..4]);
                if !matches!(enc, XmlCharEncoding::None) {
                    self.switch_encoding(enc);
                }
            }

            self.grow();
            if self.content_bytes().starts_with(b"<?xml")
                && xml_is_blank_char(self.nth_byte(5) as u32)
            {
                // Note that we will switch encoding on the fly.
                self.parse_xmldecl();
                if self.err_no == XmlParserErrors::XmlErrUnsupportedEncoding as i32
                    || matches!(self.instate, XmlParserInputState::XmlParserEOF)
                {
                    // The XML REC instructs us to stop parsing right here
                    return -1;
                }
                self.standalone = self.input().unwrap().standalone;
                self.skip_blanks();
            } else {
                self.version = Some(XML_DEFAULT_VERSION.to_owned());
            }
            if self.disable_sax == 0 {
                if let Some(start_document) =
                    self.sax.as_deref_mut().and_then(|sax| sax.start_document)
                {
                    start_document(self.user_data.clone());
                }
            }
            if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                return -1;
            }
            if self.input().is_some()
                && self.input().unwrap().buf.is_some()
                && self
                    .input()
                    .unwrap()
                    .buf
                    .as_ref()
                    .unwrap()
                    .borrow()
                    .compressed
                    >= 0
            {
                if let Some(mut my_doc) = self.my_doc {
                    my_doc.compression = self
                        .input()
                        .unwrap()
                        .buf
                        .as_ref()
                        .unwrap()
                        .borrow()
                        .compressed;
                }
            }

            // The Misc part of the Prolog
            self.parse_misc();

            // Then possibly doc type declaration(s) and more Misc
            // (doctypedecl Misc*)?
            self.grow();
            if self.content_bytes().starts_with(b"<!DOCTYPE") {
                self.in_subset = 1;
                self.parse_doctypedecl();
                if self.current_byte() == b'[' {
                    self.instate = XmlParserInputState::XmlParserDTD;
                    self.parse_internal_subset();
                    if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                        return -1;
                    }
                }

                // Create and update the external subset.
                self.in_subset = 2;
                if self.disable_sax == 0 {
                    if let Some(external_subset) =
                        self.sax.as_deref_mut().and_then(|sax| sax.external_subset)
                    {
                        external_subset(
                            self.user_data.clone(),
                            self.int_sub_name.as_deref(),
                            self.ext_sub_system.as_deref(),
                            self.ext_sub_uri.as_deref(),
                        );
                    }
                }
                if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                    return -1;
                }
                self.in_subset = 0;
                self.clean_special_attr();
                self.instate = XmlParserInputState::XmlParserProlog;
                self.parse_misc();
            }

            // Time to start parsing the tree itself
            self.grow();
            if self.current_byte() != b'<' {
                xml_fatal_err_msg(
                    self,
                    XmlParserErrors::XmlErrDocumentEmpty,
                    "Start tag expected, '<' not found\n",
                );
            } else {
                self.instate = XmlParserInputState::XmlParserContent;
                self.parse_element();
                self.instate = XmlParserInputState::XmlParserEpilog;

                // The Misc part at the end
                self.parse_misc();

                if self.current_byte() != 0 {
                    xml_fatal_err(self, XmlParserErrors::XmlErrDocumentEnd, None);
                }
                self.instate = XmlParserInputState::XmlParserEOF;
            }

            // SAX: end of the document processing.
            if let Some(end_document) = self.sax.as_deref_mut().and_then(|sax| sax.end_document) {
                end_document(self.user_data.clone());
            }

            // Remove locally kept entity definitions if the tree was not built
            if let Some(my_doc) = self
                .my_doc
                .take_if(|doc| doc.version.as_deref() == Some(SAX_COMPAT_MODE))
            {
                xml_free_doc(my_doc);
            }

            if self.well_formed != 0 {
                if let Some(mut my_doc) = self.my_doc {
                    my_doc.properties |= XmlDocProperties::XmlDocWellformed as i32;
                    if self.valid != 0 {
                        my_doc.properties |= XmlDocProperties::XmlDocDTDValid as i32;
                    }
                    if self.ns_well_formed != 0 {
                        my_doc.properties |= XmlDocProperties::XmlDocNsvalid as i32;
                    }
                    if self.options & XmlParserOption::XmlParseOld10 as i32 != 0 {
                        my_doc.properties |= XmlDocProperties::XmlDocOld10 as i32;
                    }
                }
            }
            if self.well_formed == 0 {
                self.valid = 0;
                return -1;
            }
            0
        }
    }

    /// Parse an XML Misc* optional field.
    ///
    /// ```text
    /// [27] Misc ::= Comment | PI |  S
    /// ```
    #[doc(alias = "xmlParseMisc")]
    pub(crate) unsafe fn parse_misc(&mut self) {
        unsafe {
            while !matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                self.skip_blanks();
                self.grow();
                if self.content_bytes().starts_with(b"<?") {
                    self.parse_pi();
                } else if self.content_bytes().starts_with(b"<!--") {
                    self.parse_comment();
                } else {
                    break;
                }
            }
        }
    }
}
