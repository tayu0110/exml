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

use std::{borrow::Cow, mem::take};

pub(crate) use attribute::*;
pub use dtd::*;
pub(crate) use entity::*;

#[cfg(feature = "html")]
use crate::html::parser::{HtmlParserOption, html_create_memory_parser_ctxt, html_parse_content};
use crate::{
    encoding::{XmlCharEncoding, detect_encoding, find_encoding_handler},
    error::XmlParserErrors,
    globals::GenericErrorContext,
    libxml::chvalid::xml_is_blank_char,
    parser::XmlParserOption,
    tree::{
        NodeCommon, XML_XML_NAMESPACE, XmlDocProperties, XmlDocPtr, XmlElementType,
        XmlGenericNodePtr, XmlNodePtr, xml_free_doc, xml_free_node, xml_free_node_list,
        xml_new_doc, xml_new_doc_comment, xml_new_doc_node,
    },
};

use super::{
    XML_SKIP_IDS, XmlParserCtxt, XmlParserInputState, XmlSAXLocator, xml_fatal_err,
    xml_fatal_err_msg, xml_init_parser,
};

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
    pub fn parse_document(&mut self) -> i32 {
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
                    set_document_locator(self, XmlSAXLocator::default());
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
                    start_document(self);
                }
            }
            if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                return -1;
            }
            if self.input().is_some()
                && self
                    .input()
                    .unwrap()
                    .buf
                    .as_ref()
                    .is_some_and(|buf| buf.compressed >= 0)
            {
                if let Some(mut my_doc) = self.my_doc {
                    my_doc.compression = self.input().unwrap().buf.as_ref().unwrap().compressed;
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
                            self,
                            self.int_sub_name.clone().as_deref(),
                            self.ext_sub_system.clone().as_deref(),
                            self.ext_sub_uri.clone().as_deref(),
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
                end_document(self);
            }

            // Remove locally kept entity definitions if the tree was not built
            if let Some(my_doc) = self
                .my_doc
                .take_if(|doc| doc.version.as_deref() == Some(SAX_COMPAT_MODE))
            {
                xml_free_doc(my_doc);
            }

            if self.well_formed {
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
            if !self.well_formed {
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
    pub(crate) fn parse_misc(&mut self) {
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

/// Parse a well-balanced chunk of an XML document
/// within the context (DTD, namespaces, etc ...) of the given node.
///
/// The allowed sequence for the data is a Well Balanced Chunk defined by
/// the content production in the XML grammar:
///
/// `[43] content ::= (element | CharData | Reference | CDSect | PI | Comment)*`
///
/// Returns xmlParserErrors::XML_ERR_OK if the chunk is well balanced, and the parser
/// error code otherwise
#[doc(alias = "xmlParseInNodeContext")]
pub unsafe fn xml_parse_in_node_context(
    node: XmlGenericNodePtr,
    data: Vec<u8>,
    mut options: i32,
    lst: &mut Option<XmlGenericNodePtr>,
) -> XmlParserErrors {
    unsafe {
        let mut nsnr = 0;
        let ret: XmlParserErrors;

        match node.element_type() {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlAttributeNode
            | XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlPINode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlDocumentNode
            | XmlElementType::XmlHTMLDocumentNode => {}
            _ => {
                return XmlParserErrors::XmlErrInternalError;
            }
        }
        let mut node = Some(node);
        while let Some(now) = node.filter(|node| {
            !matches!(
                node.element_type(),
                XmlElementType::XmlElementNode
                    | XmlElementType::XmlDocumentNode
                    | XmlElementType::XmlHTMLDocumentNode
            )
        }) {
            node = now.parent();
        }
        let Some(mut node) = node else {
            return XmlParserErrors::XmlErrInternalError;
        };
        let doc = if let Ok(doc) = XmlDocPtr::try_from(node) {
            Some(doc)
        } else {
            node.document()
        };
        let Some(doc) = doc else {
            return XmlParserErrors::XmlErrInternalError;
        };

        // allocate a context and set-up everything not related to the
        // node position in the tree
        let ctxt = match doc.typ {
            XmlElementType::XmlDocumentNode => XmlParserCtxt::from_memory(data),
            #[cfg(feature = "html")]
            XmlElementType::XmlHTMLDocumentNode => {
                // When parsing in context, it makes no sense to add implied
                // elements like html/body/etc...
                options |= HtmlParserOption::HtmlParseNoimplied as i32;
                html_create_memory_parser_ctxt(data)
            }
            _ => return XmlParserErrors::XmlErrInternalError,
        };

        let Some(mut ctxt) = ctxt else {
            return XmlParserErrors::XmlErrNoMemory;
        };

        // Use input doc's dict if present, else assure XML_PARSE_NODICT is set.
        // We need a dictionary for xmlDetectSAX2, so if there's no doc dict
        // we must wait until the last moment to free the original one.
        options |= XmlParserOption::XmlParseNoDict as i32;

        if let Some(encoding) = doc.encoding.as_deref() {
            ctxt.encoding = Some(encoding.to_owned());

            if let Some(handler) = find_encoding_handler(encoding) {
                ctxt.switch_to_encoding(handler);
            } else {
                return XmlParserErrors::XmlErrUnsupportedEncoding;
            }
        }

        ctxt.use_options_internal(options, None);
        ctxt.detect_sax2();
        ctxt.my_doc = Some(doc);
        // parsing in context, i.e. as within existing content
        ctxt.input_id = 2;
        ctxt.instate = XmlParserInputState::XmlParserContent;

        let Some(mut fake) = xml_new_doc_comment(node.document(), "") else {
            return XmlParserErrors::XmlErrNoMemory;
        };
        node.add_child(fake.into());

        // At this point, `node.element_type()` is ElementNode, DocumentNode or HTMLDocumentNode.
        if let Ok(node) = XmlNodePtr::try_from(node) {
            ctxt.node_push(node);
            // initialize the SAX2 namespaces stack
            let mut cur = Some(node);
            while let Some(now) =
                cur.filter(|cur| cur.element_type() == XmlElementType::XmlElementNode)
            {
                let mut ns = now.ns_def;
                while let Some(cur_ns) = ns {
                    if ctxt.get_namespace(cur_ns.prefix().as_deref()).is_none() {
                        ctxt.ns_push(cur_ns.prefix().as_deref(), &cur_ns.href().unwrap());
                        nsnr += 1;
                    }
                    ns = cur_ns.next;
                }
                cur = now.parent.and_then(|p| XmlNodePtr::try_from(p).ok());
            }
        }

        if ctxt.validate != 0 || ctxt.replace_entities {
            // ID/IDREF registration will be done in xmlValidateElement below
            ctxt.loadsubset |= XML_SKIP_IDS as i32;
        }

        #[cfg(feature = "html")]
        {
            if doc.typ == XmlElementType::XmlHTMLDocumentNode {
                html_parse_content(&mut ctxt);
            } else {
                ctxt.parse_content();
            }
        }
        #[cfg(not(feature = "html"))]
        {
            ctxt.parse_content();
        }

        ctxt.ns_pop(nsnr);
        if ctxt.current_byte() == b'<' && ctxt.nth_byte(1) == b'/' {
            xml_fatal_err(&mut ctxt, XmlParserErrors::XmlErrNotWellBalanced, None);
        } else if ctxt.current_byte() != 0 {
            xml_fatal_err(&mut ctxt, XmlParserErrors::XmlErrExtraContent, None);
        }
        if ctxt
            .node
            .is_some_and(|ctxt_node| XmlGenericNodePtr::from(ctxt_node) != node)
        {
            xml_fatal_err(&mut ctxt, XmlParserErrors::XmlErrNotWellBalanced, None);
            ctxt.well_formed = false;
        }

        if !ctxt.well_formed {
            if ctxt.err_no == 0 {
                ret = XmlParserErrors::XmlErrInternalError;
            } else {
                ret = XmlParserErrors::try_from(ctxt.err_no).unwrap();
            }
        } else {
            ret = XmlParserErrors::XmlErrOK;
        }

        // Return the newly created nodeset after unlinking it from
        // the pseudo sibling.

        let mut cur = fake.next.take();
        node.set_last(Some(fake.into()));

        if let Some(mut cur) = cur {
            cur.set_prev(None);
        }

        *lst = cur;

        while let Some(mut now) = cur {
            now.set_parent(None);
            cur = now.next();
        }

        fake.unlink();
        xml_free_node(fake);

        if !matches!(ret, XmlParserErrors::XmlErrOK) {
            xml_free_node_list(lst.take());
        }

        ret
    }
}

/// Parse a well-balanced chunk of an XML document called by the parser
/// The allowed sequence for the Well Balanced Chunk is the one defined by
/// the content production in the XML grammar:
///
/// ```text
/// [43] content ::= (element | CharData | Reference | CDSect | PI | Comment)*
/// ```
///
/// Returns xmlParserErrors::XML_ERR_OK if the chunk is well balanced, and the parser
/// error code otherwise
///
/// In case recover is set to 1, the nodelist will not be empty even if
/// the parsed chunk is not well balanced.
#[doc(alias = "xmlParseBalancedChunkMemoryInternal")]
pub(crate) fn xml_parse_balanced_chunk_memory_internal(
    oldctxt: &mut XmlParserCtxt,
    string: &str,
    user_data: Option<GenericErrorContext>,
    mut lst: Option<&mut Option<XmlGenericNodePtr>>,
) -> XmlParserErrors {
    unsafe {
        let mut content = None;
        let mut last = None;
        let ret: XmlParserErrors;

        if (oldctxt.depth > 40 && oldctxt.options & XmlParserOption::XmlParseHuge as i32 == 0)
            || oldctxt.depth > 100
        {
            xml_fatal_err_msg(
                oldctxt,
                XmlParserErrors::XmlErrEntityLoop,
                "Maximum entity nesting depth exceeded",
            );
            return XmlParserErrors::XmlErrEntityLoop;
        }

        if let Some(lst) = lst.as_mut() {
            **lst = None;
        }

        let Some(mut ctxt) = XmlParserCtxt::from_memory(string.as_bytes().to_vec()) else {
            return XmlParserErrors::XmlWarUndeclaredEntity;
        };
        ctxt.nb_errors = oldctxt.nb_errors;
        ctxt.nb_warnings = oldctxt.nb_warnings;
        ctxt.user_data = user_data;
        ctxt.input_id = oldctxt.input_id;
        ctxt.str_xml = Some(Cow::Borrowed("xml"));
        ctxt.str_xmlns = Some(Cow::Borrowed("xmlns"));
        ctxt.str_xml_ns = Some(Cow::Borrowed(XML_XML_NAMESPACE));

        // propagate namespaces down the entity
        for (pre, loc) in &oldctxt.ns_tab {
            ctxt.ns_push(pre.as_deref(), loc);
        }

        let oldsax = ctxt.sax.take();
        ctxt.sax = oldctxt.sax.take();
        ctxt.detect_sax2();
        ctxt.replace_entities = oldctxt.replace_entities;
        ctxt.options = oldctxt.options;
        ctxt._private = oldctxt._private;

        let mut new_doc = None;
        let mut my_doc = if let Some(my_doc) = oldctxt.my_doc {
            ctxt.my_doc = Some(my_doc);
            content = my_doc.children;
            last = my_doc.last;
            my_doc
        } else {
            let Some(mut new) = xml_new_doc(Some("1.0")) else {
                oldctxt.sax = ctxt.sax.take();
                ctxt.sax = oldsax;
                return XmlParserErrors::XmlErrInternalError;
            };
            new_doc = Some(new);
            new.properties = XmlDocProperties::XmlDocInternal as i32;
            ctxt.my_doc = Some(new);
            new
        };
        let Some(new_root) = xml_new_doc_node(ctxt.my_doc, None, "pseudoroot", None) else {
            oldctxt.sax = ctxt.sax.take();
            ctxt.sax = oldsax;
            if let Some(new_doc) = new_doc {
                xml_free_doc(new_doc);
            }
            return XmlParserErrors::XmlErrInternalError;
        };
        my_doc.children = None;
        my_doc.last = None;
        my_doc.add_child(new_root.into());
        ctxt.node_push(
            my_doc
                .children
                .map(|c| XmlNodePtr::try_from(c).unwrap())
                .unwrap(),
        );
        ctxt.instate = XmlParserInputState::XmlParserContent;
        ctxt.depth = oldctxt.depth;

        ctxt.validate = 0;
        ctxt.loadsubset = oldctxt.loadsubset;
        if oldctxt.validate != 0 || oldctxt.replace_entities {
            // ID/IDREF registration will be done in xmlValidateElement below
            ctxt.loadsubset |= XML_SKIP_IDS as i32;
        }
        ctxt.dict_names = oldctxt.dict_names;
        ctxt.atts_default = take(&mut oldctxt.atts_default);
        ctxt.atts_special = oldctxt.atts_special;

        ctxt.parse_content();
        if ctxt.content_bytes().starts_with(b"</") {
            xml_fatal_err(&mut ctxt, XmlParserErrors::XmlErrNotWellBalanced, None);
        } else if ctxt.current_byte() != 0 {
            xml_fatal_err(&mut ctxt, XmlParserErrors::XmlErrExtraContent, None);
        }
        if my_doc.children != ctxt.node.map(|node| node.into()) {
            xml_fatal_err(&mut ctxt, XmlParserErrors::XmlErrNotWellBalanced, None);
        }

        if !ctxt.well_formed {
            ret = XmlParserErrors::try_from(ctxt.err_no).unwrap();
            oldctxt.err_no = ctxt.err_no;
            oldctxt.well_formed = false;
            oldctxt.last_error = ctxt.last_error.clone();
        } else {
            ret = XmlParserErrors::XmlErrOK;
        }

        if let Some(lst) = lst {
            if matches!(ret, XmlParserErrors::XmlErrOK) {
                // Return the newly created nodeset after unlinking it from
                // they pseudo parent.
                let mut cur = my_doc.children().unwrap().children();
                *lst = cur;
                while let Some(mut now) = cur {
                    #[cfg(feature = "libxml_valid")]
                    if oldctxt.validate != 0
                        && oldctxt.well_formed
                        && now.element_type() == XmlElementType::XmlElementNode
                    {
                        if let Some(my_doc) = oldctxt.my_doc.filter(|doc| doc.int_subset.is_some())
                        {
                            oldctxt.valid &= oldctxt.validate_element(my_doc, cur);
                        }
                    }
                    now.set_parent(None);
                    cur = now.next();
                }
                my_doc.children().unwrap().set_children(None);
            }
        }
        if let Some(mut my_doc) = ctxt.my_doc {
            xml_free_node(my_doc.children().unwrap());
            my_doc.children = content;
            my_doc.last = last;
        }

        // Also record the size of the entity parsed
        if ctxt.input().is_some() {
            let mut consumed: u64 = ctxt.input().unwrap().consumed;
            consumed = consumed.saturating_add(ctxt.input().unwrap().offset_from_base() as u64);

            oldctxt.sizeentcopy = oldctxt.sizeentcopy.saturating_add(consumed);
            oldctxt.sizeentcopy = oldctxt.sizeentcopy.saturating_add(ctxt.sizeentcopy);
        }

        oldctxt.nb_errors = ctxt.nb_errors;
        oldctxt.nb_warnings = ctxt.nb_warnings;
        oldctxt.sax = ctxt.sax.take();
        ctxt.sax = oldsax;
        ctxt.atts_default.clear();
        ctxt.atts_special = None;
        if let Some(new_doc) = new_doc {
            xml_free_doc(new_doc);
        }

        ret
    }
}
