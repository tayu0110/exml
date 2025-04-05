//! Provide methods and data structures for text writing APIs.
//!
//! This module is based on `libxml/xmlwriter.h`, `xmlwriter.c` and so on in `libxml2-v2.11.8`.  
//! Please refer to original libxml2 documents also.
//!
//! The original API has functions that support the format, but not here.
//!
//! Although Rust has no variable-length arguments,
//! it is simpler and more natural to limit the API to writing a single string,
//! since it is easy to write the format to a single String using `format!`.

// Copyright of the original code is the following.
// --------
// Summary: text writing API for XML
// Description: text writing API for XML
//
// Copy: See Copyright for the status of this software.
//
// Author: Alfred Mickautsch <alfred@mickautsch.de>
// --------
// xmlwriter.c: XML text writer implementation
//
// For license and disclaimer see the license and disclaimer of libxml2.
//
// alfred@mickautsch.de

use std::{
    borrow::Cow,
    cell::{Cell, RefCell},
    collections::VecDeque,
    io::{self, Write},
    os::raw::c_void,
    ptr::null_mut,
    rc::Rc,
};

use crate::{
    buf::XmlBufRef,
    encoding::find_encoding_handler,
    error::{__xml_raise_error, XmlErrorDomain, XmlErrorLevel, XmlParserErrors},
    globals::GenericErrorContext,
    html::tree::html_new_doc_no_dtd,
    io::XmlOutputBuffer,
    libxml::sax2::{
        xml_sax2_end_element, xml_sax2_init_default_sax_handler, xml_sax2_start_element,
    },
    list::XmlList,
    parser::{
        XML_DEFAULT_VERSION, XmlParserCtxtPtr, XmlParserInputState, XmlSAXHandler,
        xml_create_push_parser_ctxt, xml_free_parser_ctxt,
    },
    save::attr_serialize_text_content,
    tree::{XmlDocPtr, XmlNodePtr, xml_encode_special_chars, xml_free_doc, xml_new_doc},
    uri::canonic_path,
};

// Types are kept private
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum XmlTextWriterState {
    XmlTextwriterNone = 0,
    XmlTextwriterName,
    XmlTextwriterAttribute,
    XmlTextwriterText,
    XmlTextwriterPI,
    XmlTextwriterPIText,
    XmlTextwriterCDATA,
    XmlTextwriterDTD,
    XmlTextwriterDTDText,
    XmlTextwriterDTDElem,
    XmlTextwriterDTDElemText,
    XmlTextwriterDTDAttl,
    XmlTextwriterDTDAttlText,
    XmlTextwriterDTDEnty, /* entity */
    XmlTextwriterDTDEntyText,
    XmlTextwriterDTDPEnt, /* parameter entity */
    XmlTextwriterComment,
}

#[repr(C)]
struct XmlTextWriterStackEntry {
    name: Option<String>,
    state: Cell<XmlTextWriterState>,
}

#[repr(C)]
struct XmlTextWriterNsStackEntry {
    prefix: String,
    uri: String,
    elem: Option<Rc<XmlTextWriterStackEntry>>,
}

#[repr(C)]
pub struct XmlTextWriter<'a> {
    // output buffer
    out: XmlOutputBuffer<'a>,
    // element name stack
    nodes: VecDeque<Rc<XmlTextWriterStackEntry>>,
    // name spaces stack
    nsstack: XmlList<XmlTextWriterNsStackEntry>,
    level: i32,
    // enable indent
    indent: i32,
    // internal indent flag
    doindent: i32,
    // indent character
    ichar: Cow<'static, str>,
    // character used for quoting attribute values
    qchar: u8,
    ctxt: XmlParserCtxtPtr,
    no_doc_free: i32,
    doc: Option<XmlDocPtr>,
}

impl<'a> XmlTextWriter<'a> {
    /// Create a new xmlNewTextWriter structure using an xmlOutputBufferPtr
    ///
    /// # Note
    /// The @out parameter will be deallocated when the writer is closed
    /// (if the call succeed.)
    ///
    /// Returns the new xmlTextWriterPtr or NULL in case of error
    #[doc(alias = "xmlNewTextWriter")]
    pub unsafe fn new(out: XmlOutputBuffer<'a>) -> Self {
        unsafe {
            Self {
                nsstack: XmlList::new(
                    None,
                    Rc::new(|d1, d2| {
                        if std::ptr::eq(d1, d2) {
                            return std::cmp::Ordering::Equal;
                        }

                        let mut rc = d1.prefix.cmp(&d2.prefix);

                        if !rc.is_eq() {
                            rc = std::cmp::Ordering::Less;
                        }
                        match (d1.elem.as_ref(), d2.elem.as_ref()) {
                            (Some(d1), Some(d2)) => {
                                if !Rc::ptr_eq(d1, d2) {
                                    rc = std::cmp::Ordering::Less;
                                }
                            }
                            (None, None) => {}
                            _ => rc = std::cmp::Ordering::Less,
                        }
                        rc
                    }),
                ),
                out,
                ichar: Cow::Borrowed(" "),
                qchar: b'"',
                doc: xml_new_doc(None),
                no_doc_free: 0,
                nodes: VecDeque::new(),
                ..Default::default()
            }
        }
    }

    /// Create a new xmlNewTextWriter structure with @uri as output
    ///
    /// Returns the new xmlTextWriterPtr or NULL in case of error
    #[doc(alias = "xmlNewTextWriterFilename")]
    pub unsafe fn from_filename<'b: 'a>(uri: &'b str, compression: i32) -> Option<Self> {
        unsafe {
            let Some(out) = XmlOutputBuffer::from_uri(uri, None, compression) else {
                xml_writer_err_msg(
                    None,
                    XmlParserErrors::XmlIOEIO,
                    "xmlNewTextWriterFilename : cannot open uri\n",
                );
                return None;
            };

            let mut ret = XmlTextWriter::new(out);
            ret.indent = 0;
            ret.doindent = 0;
            Some(ret)
        }
    }

    /// Create a new xmlNewTextWriter structure with @ctxt as output
    /// NOTE: the @ctxt context will be freed with the resulting writer
    ///       (if the call succeeds).
    /// TODO: handle compression
    ///
    /// Returns the new xmlTextWriterPtr or NULL in case of error
    #[doc(alias = "xmlNewTextWriterPushParser")]
    pub unsafe fn from_push_parser(ctxt: XmlParserCtxtPtr, _compression: i32) -> Option<Self> {
        unsafe {
            if ctxt.is_null() {
                xml_writer_err_msg(
                    None,
                    XmlParserErrors::XmlErrInternalError,
                    "xmlNewTextWriterPushParser : invalid context!\n",
                );
                return None;
            }

            let Some(out) =
                XmlOutputBuffer::from_writer(TextWriterPushContext { context: ctxt }, None)
            else {
                xml_writer_err_msg(
                    None,
                    XmlParserErrors::XmlErrInternalError,
                    "xmlNewTextWriterPushParser : error at xmlOutputBufferCreateIO!\n",
                );
                return None;
            };

            let mut ret = XmlTextWriter::new(out);
            ret.ctxt = ctxt;
            Some(ret)
        }
    }

    /// Create a new xmlNewTextWriter structure with @*doc as output
    ///
    /// Returns the new xmlTextWriterPtr or NULL in case of error
    #[doc(alias = "xmlNewTextWriterDoc")]
    pub unsafe fn with_doc(doc: Option<&mut Option<XmlDocPtr>>, compression: i32) -> Option<Self> {
        unsafe {
            let mut sax_handler = XmlSAXHandler::default();
            xml_sax2_init_default_sax_handler(&mut sax_handler, 1);
            sax_handler.start_document = Some(xml_text_writer_start_document_callback);
            sax_handler.start_element = Some(xml_sax2_start_element);
            sax_handler.end_element = Some(xml_sax2_end_element);

            let ctxt: XmlParserCtxtPtr =
                xml_create_push_parser_ctxt(Some(Box::new(sax_handler)), None, b"", None);
            if ctxt.is_null() {
                xml_writer_err_msg(
                    None,
                    XmlParserErrors::XmlErrInternalError,
                    "xmlNewTextWriterDoc : error at xmlCreatePushParserCtxt!\n",
                );
                return None;
            }
            // For some reason this seems to completely break if node names are interned.
            (*ctxt).dict_names = 0;

            (*ctxt).my_doc = xml_new_doc(Some(XML_DEFAULT_VERSION));
            let Some(mut my_doc) = (*ctxt).my_doc else {
                xml_free_parser_ctxt(ctxt);
                xml_writer_err_msg(
                    None,
                    XmlParserErrors::XmlErrInternalError,
                    "xmlNewTextWriterDoc : error at xmlNewDoc!\n",
                );
                return None;
            };

            let Some(mut ret) = XmlTextWriter::from_push_parser(ctxt, compression) else {
                xml_free_doc(my_doc);
                xml_free_parser_ctxt(ctxt);
                xml_writer_err_msg(
                    None,
                    XmlParserErrors::XmlErrInternalError,
                    "xmlNewTextWriterDoc : error at xmlNewTextWriterPushParser!\n",
                );
                return None;
            };

            my_doc.set_compress_mode(compression);

            if let Some(doc) = doc {
                *doc = Some(my_doc);
                ret.no_doc_free = 1;
            }

            Some(ret)
        }
    }

    /// Create a new xmlNewTextWriter structure with @doc as output starting at @node
    ///
    /// Returns the new xmlTextWriterPtr or NULL in case of error
    #[doc(alias = "xmlNewTextWriterTree")]
    pub unsafe fn with_tree(
        mut doc: XmlDocPtr,
        node: XmlNodePtr,
        compression: i32,
    ) -> Option<Self> {
        unsafe {
            let mut sax_handler = XmlSAXHandler::default();
            xml_sax2_init_default_sax_handler(&mut sax_handler, 1);
            sax_handler.start_document = Some(xml_text_writer_start_document_callback);
            sax_handler.start_element = Some(xml_sax2_start_element);
            sax_handler.end_element = Some(xml_sax2_end_element);

            let ctxt: XmlParserCtxtPtr =
                xml_create_push_parser_ctxt(Some(Box::new(sax_handler)), None, b"", None);
            if ctxt.is_null() {
                xml_writer_err_msg(
                    None,
                    XmlParserErrors::XmlErrInternalError,
                    "xmlNewTextWriterDoc : error at xmlCreatePushParserCtxt!\n",
                );
                return None;
            }
            // For some reason this seems to completely break if node names are interned.
            (*ctxt).dict_names = 0;

            let Some(mut ret) = XmlTextWriter::from_push_parser(ctxt, compression) else {
                xml_free_parser_ctxt(ctxt);
                xml_writer_err_msg(
                    None,
                    XmlParserErrors::XmlErrInternalError,
                    "xmlNewTextWriterDoc : error at xmlNewTextWriterPushParser!\n",
                );
                return None;
            };

            (*ctxt).my_doc = Some(doc);
            (*ctxt).node = Some(node);
            ret.no_doc_free = 1;

            doc.set_compress_mode(compression);

            Some(ret)
        }
    }

    /// Write state dependent strings.
    ///
    /// Returns -1 on error or the number of characters written.
    #[doc(alias = "xmlTextWriterHandleStateDependencies")]
    unsafe fn handle_state_dependencies(
        &mut self,
        p: Rc<XmlTextWriterStackEntry>,
    ) -> io::Result<usize> {
        unsafe {
            let mut sum = 0;
            match p.state.get() {
                XmlTextWriterState::XmlTextwriterName => {
                    // Output namespace declarations
                    sum += self.output_nsdecl()?;
                    sum += self.out.write_str(">")?;
                    p.state.set(XmlTextWriterState::XmlTextwriterText);
                }
                XmlTextWriterState::XmlTextwriterPI => {
                    sum += self.out.write_str(" ")?;
                    p.state.set(XmlTextWriterState::XmlTextwriterPIText);
                }
                XmlTextWriterState::XmlTextwriterDTD => {
                    sum += self.out.write_str(" [")?;
                    p.state.set(XmlTextWriterState::XmlTextwriterDTDText);
                }
                XmlTextWriterState::XmlTextwriterDTDElem => {
                    sum += self.out.write_str(" ")?;
                    p.state.set(XmlTextWriterState::XmlTextwriterDTDElemText);
                }
                XmlTextWriterState::XmlTextwriterDTDAttl => {
                    sum += self.out.write_str(" ")?;
                    p.state.set(XmlTextWriterState::XmlTextwriterDTDAttlText);
                }
                XmlTextWriterState::XmlTextwriterDTDEnty
                | XmlTextWriterState::XmlTextwriterDTDPEnt => {
                    sum += self.out.write_bytes(&[b' ', self.qchar])?;
                    p.state.set(XmlTextWriterState::XmlTextwriterDTDEntyText);
                }
                _ => {}
            }

            Ok(sum)
        }
    }

    /// Set indentation output. indent = 0 do not indentation. indent > 0 do indentation.
    ///
    /// Returns -1 on error or 0 otherwise.
    #[doc(alias = "xmlTextWriterSetIndent")]
    pub fn set_indent(&mut self, indent: i32) -> i32 {
        if indent < 0 {
            return -1;
        }

        self.indent = indent;
        self.doindent = 1;

        0
    }

    /// Set string indentation.
    ///
    /// Returns -1 on error or 0 otherwise.
    #[doc(alias = "xmlTextWriterSetIndentString")]
    pub fn set_indent_string(&mut self, indent: &str) -> i32 {
        self.ichar = Cow::Owned(indent.to_owned());
        0
    }

    /// Set the character used for quoting attributes.
    ///
    /// Returns -1 on error or 0 otherwise.
    #[doc(alias = "xmlTextWriterSetQuoteChar")]
    pub fn set_quote_char(&mut self, quotechar: u8) -> i32 {
        if quotechar != b'\'' && quotechar != b'"' {
            return -1;
        }

        self.qchar = quotechar;

        0
    }

    /// Flush the output buffer.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterFlush")]
    pub fn flush(&mut self) -> i32 {
        self.out.flush()
    }

    /// Write an xml text.
    /// TODO: what about entities and special chars??
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterWriteRawLen")]
    #[doc(alias = "xmlTextWriterWriteRaw")]
    pub unsafe fn write_bytes(&mut self, content: &[u8]) -> io::Result<usize> {
        unsafe {
            let mut sum = 0;
            if let Some(lk) = self.nodes.front().cloned() {
                sum += self.handle_state_dependencies(lk)?;
            }

            if self.indent != 0 {
                self.doindent = 0;
            }

            sum += self.out.write_bytes(content)?;
            Ok(sum)
        }
    }

    /// Write an xml text.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterWriteString")]
    pub unsafe fn write_string(&mut self, content: &str) -> io::Result<usize> {
        unsafe {
            let mut sum = 0;
            if let Some(lk) = self.nodes.front() {
                match lk.state.get() {
                    XmlTextWriterState::XmlTextwriterName
                    | XmlTextWriterState::XmlTextwriterText => {
                        let buf = xml_encode_special_chars(None, content);
                        sum += self.write_bytes(buf.as_bytes())?;
                    }
                    XmlTextWriterState::XmlTextwriterAttribute => {
                        attr_serialize_text_content(&mut self.out, self.doc, None, content);
                    }
                    _ => {
                        sum += self.write_bytes(content.as_bytes())?;
                    }
                }
            }

            Ok(sum)
        }
    }

    /// Write an base64 encoded xml text.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterWriteBase64")]
    pub unsafe fn write_base64(&mut self, data: &[u8]) -> io::Result<usize> {
        unsafe {
            let mut sum = 0;
            if let Some(lk) = self.nodes.front().cloned() {
                sum += self.handle_state_dependencies(lk)?;
            }

            if self.indent != 0 {
                self.doindent = 0;
            }

            sum += xml_output_buffer_write_base64(&mut self.out, data)?;
            Ok(sum)
        }
    }

    /// Write a BinHex encoded xml text.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterWriteBinHex")]
    pub unsafe fn write_bin_hex(&mut self, data: &[u8]) -> io::Result<usize> {
        unsafe {
            let mut sum = 0;
            if let Some(lk) = self.nodes.front().cloned() {
                sum += self.handle_state_dependencies(lk)?;
            }

            if self.indent != 0 {
                self.doindent = 0;
            }

            sum += xml_output_buffer_write_bin_hex(&mut self.out, data)?;
            Ok(sum)
        }
    }

    /// Write indent string.
    ///
    /// Returns -1 on error or the number of strings written.
    #[doc(alias = "xmlTextWriterWriteIndent")]
    unsafe fn write_indent(&mut self) -> io::Result<usize> {
        let lksize = self.nodes.len();
        if lksize < 1 {
            return Err(io::Error::other("List is empty"));
        }
        for _ in 0..lksize - 1 {
            self.out.write_str(&self.ichar)?;
        }

        Ok(lksize - 1)
    }

    /// Start a new xml document
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterStartDocument")]
    pub fn start_document(
        &mut self,
        version: Option<&str>,
        encoding: Option<&str>,
        standalone: Option<&str>,
    ) -> io::Result<usize> {
        if self.nodes.front().is_some() {
            xml_writer_err_msg(
                Some(self),
                XmlParserErrors::XmlErrInternalError,
                "xmlTextWriterStartDocument : not allowed in this context!\n",
            );
            return Err(io::Error::other(
                "xmlTextWriterStartDocument : not allowed in this context!",
            ));
        }

        let encoder = if let Some(encoding) = encoding {
            let Some(encoder) = find_encoding_handler(encoding) else {
                xml_writer_err_msg(
                    Some(self),
                    XmlParserErrors::XmlErrUnsupportedEncoding,
                    "xmlTextWriterStartDocument : unsupported encoding\n",
                );
                return Err(io::Error::other(
                    "xmlTextWriterStartDocument : unsupported encoding",
                ));
            };
            Some(encoder)
        } else {
            None
        };

        self.out.encoder = encoder.map(|e| Rc::new(RefCell::new(e)));
        if self.out.encoder.is_some() {
            if self.out.conv.is_none() {
                self.out.conv = XmlBufRef::with_capacity(4000);
            }
            self.out.encode(true).ok();
            if let Some(mut doc) = self.doc.filter(|doc| doc.encoding.is_none()) {
                let encoder = self.out.encoder.as_ref().unwrap().borrow();
                doc.encoding = Some(encoder.name().to_owned());
            }
        } else {
            self.out.conv = None;
        }

        let mut sum = self.out.write_str("<?xml version=")?;
        sum += self.out.write_bytes(&[self.qchar])?;
        sum += self.out.write_str(version.unwrap_or("1.0"))?;
        sum += self.out.write_bytes(&[self.qchar])?;
        if self.out.encoder.is_some() {
            sum += self.out.write_str(" encoding=")?;
            sum += self.out.write_bytes(&[self.qchar])?;
            let name = self
                .out
                .encoder
                .as_ref()
                .unwrap()
                .borrow()
                .name()
                .to_owned();
            sum += self.out.write_str(&name)?;
            sum += self.out.write_bytes(&[self.qchar])?;
        }

        if let Some(standalone) = standalone {
            sum += self.out.write_str(" standalone=")?;
            sum += self.out.write_bytes(&[self.qchar])?;
            sum += self.out.write_str(standalone)?;
            sum += self.out.write_bytes(&[self.qchar])?;
        }

        sum += self.out.write_str("?>\n")?;
        Ok(sum)
    }

    /// End an xml document. All open elements are closed, and
    /// the content is flushed to the output.
    ///
    /// Returns the bytes written or -1 in case of error
    #[doc(alias = "xmlTextWriterEndDocument")]
    pub unsafe fn end_document(&mut self) -> io::Result<usize> {
        unsafe {
            let mut sum = 0;
            while let Some(lk) = self.nodes.front() {
                match lk.state.get() {
                    XmlTextWriterState::XmlTextwriterName
                    | XmlTextWriterState::XmlTextwriterAttribute
                    | XmlTextWriterState::XmlTextwriterText => {
                        sum += self.end_element()?;
                    }
                    XmlTextWriterState::XmlTextwriterPI
                    | XmlTextWriterState::XmlTextwriterPIText => {
                        sum += self.end_pi()?;
                    }
                    XmlTextWriterState::XmlTextwriterCDATA => {
                        sum += self.end_cdata()?;
                    }
                    XmlTextWriterState::XmlTextwriterDTD
                    | XmlTextWriterState::XmlTextwriterDTDText
                    | XmlTextWriterState::XmlTextwriterDTDElem
                    | XmlTextWriterState::XmlTextwriterDTDElemText
                    | XmlTextWriterState::XmlTextwriterDTDAttl
                    | XmlTextWriterState::XmlTextwriterDTDAttlText
                    | XmlTextWriterState::XmlTextwriterDTDEnty
                    | XmlTextWriterState::XmlTextwriterDTDEntyText
                    | XmlTextWriterState::XmlTextwriterDTDPEnt => {
                        sum += self.end_dtd()?;
                    }
                    XmlTextWriterState::XmlTextwriterComment => {
                        sum += self.end_comment()?;
                    }
                    _ => {}
                }
            }

            if self.indent == 0 {
                sum += self.out.write_str("\n")?;
            }

            sum += self.flush() as usize;
            Ok(sum)
        }
    }

    /// Start an xml element.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterStartElement")]
    pub unsafe fn start_element(&mut self, name: &str) -> io::Result<usize> {
        unsafe {
            if name.is_empty() {
                return Err(io::Error::other("Writer or name is NULL"));
            }

            let mut sum = 0;
            if let Some(lk) = self.nodes.front().cloned() {
                match lk.state.get() {
                    XmlTextWriterState::XmlTextwriterPI
                    | XmlTextWriterState::XmlTextwriterPIText => {
                        return Err(io::Error::other("Not start element"));
                    }
                    XmlTextWriterState::XmlTextwriterNone => {}
                    ty @ XmlTextWriterState::XmlTextwriterAttribute
                    | ty @ XmlTextWriterState::XmlTextwriterName => {
                        if matches!(ty, XmlTextWriterState::XmlTextwriterAttribute) {
                            sum += self.end_attribute()?;
                        }

                        // Output namespace declarations
                        sum += self.output_nsdecl()?;
                        sum += self.out.write_str(">")?;
                        if self.indent != 0 {
                            // count =
                            self.out.write_str("\n")?;
                        }
                        lk.state.set(XmlTextWriterState::XmlTextwriterText);
                    }
                    _ => {}
                }
            }

            let p = XmlTextWriterStackEntry {
                name: Some(name.to_owned()),
                state: Cell::new(XmlTextWriterState::XmlTextwriterName),
            };
            self.nodes.push_front(p.into());

            if self.indent != 0 {
                sum += self.write_indent()?;
            }

            sum += self.out.write_str("<")?;
            sum += self.out.write_str(name)?;
            Ok(sum)
        }
    }

    /// Write an xml element.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterWriteElement")]
    pub unsafe fn write_element(&mut self, name: &str, content: Option<&str>) -> io::Result<usize> {
        unsafe {
            let mut sum = self.start_element(name)?;
            if let Some(content) = content {
                sum += self.write_string(content)?;
            }
            sum += self.end_element()?;
            Ok(sum)
        }
    }

    /// End the current xml element.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterEndElement")]
    pub unsafe fn end_element(&mut self) -> io::Result<usize> {
        unsafe {
            let Some(lk) = self.nodes.front().cloned() else {
                self.nsstack.clear();
                return Err(io::Error::other("Nodes link is NULL"));
            };

            let mut sum = 0;
            match lk.state.get() {
                ty @ XmlTextWriterState::XmlTextwriterAttribute
                | ty @ XmlTextWriterState::XmlTextwriterName => {
                    if matches!(ty, XmlTextWriterState::XmlTextwriterAttribute) {
                        let count = self.end_attribute();
                        if count.is_err() {
                            self.nsstack.clear();
                            return count;
                        }
                        sum += count.unwrap();
                    }

                    // Output namespace declarations
                    sum += self.output_nsdecl()?;

                    // next element needs indent
                    if self.indent != 0 {
                        self.doindent = 1;
                    }
                    sum += self.out.write_str("/>")?;
                }
                XmlTextWriterState::XmlTextwriterText => {
                    if self.indent != 0 && self.doindent != 0 {
                        sum += self.write_indent()?;
                        self.doindent = 1;
                    } else {
                        self.doindent = 1;
                    }
                    sum += self.out.write_str("</")?;
                    sum += self.out.write_str(lk.name.as_deref().unwrap())?;
                    sum += self.out.write_str(">")?;
                }
                _ => {
                    return Err(io::Error::other("Invalid state"));
                }
            }

            if self.indent != 0 {
                sum += self.out.write_str("\n")?;
            }

            self.nodes.pop_front();
            Ok(sum)
        }
    }

    /// End the current xml element. Writes an end tag even if the element is empty
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterFullEndElement")]
    pub unsafe fn full_end_element(&mut self) -> io::Result<usize> {
        unsafe {
            let Some(lk) = self.nodes.front().cloned() else {
                return Err(io::Error::other("Nodes link is NULL"));
            };

            let mut sum = 0;
            match lk.state.get() {
                ty @ XmlTextWriterState::XmlTextwriterAttribute
                | ty @ XmlTextWriterState::XmlTextwriterName
                | ty @ XmlTextWriterState::XmlTextwriterText => {
                    if matches!(ty, XmlTextWriterState::XmlTextwriterAttribute) {
                        sum += self.end_attribute()?;
                    }

                    if matches!(
                        ty,
                        XmlTextWriterState::XmlTextwriterAttribute
                            | XmlTextWriterState::XmlTextwriterName
                    ) {
                        // Output namespace declarations
                        sum += self.output_nsdecl()?;
                        sum += self.out.write_str(">")?;
                        if self.indent != 0 {
                            self.doindent = 0;
                        }
                    }

                    if self.indent != 0 && self.doindent != 0 {
                        sum += self.write_indent()?;
                        self.doindent = 1;
                    } else {
                        self.doindent = 1;
                    }
                    sum += self.out.write_str("</")?;
                    sum += self.out.write_str(lk.name.as_deref().unwrap())?;
                    sum += self.out.write_str(">")?;
                }
                _ => {
                    return Err(io::Error::other("Invalid state"));
                }
            }

            if self.indent != 0 {
                sum += self.out.write_str("\n")?;
            }

            self.nodes.pop_front();
            Ok(sum)
        }
    }

    /// Start an xml element with namespace support.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterStartElementNS")]
    pub unsafe fn start_element_ns(
        &mut self,
        prefix: Option<&str>,
        name: &str,
        namespace_uri: Option<&str>,
    ) -> io::Result<usize> {
        unsafe {
            if name.is_empty() {
                return Err(io::Error::other("Writer or name is NULL"));
            }

            let mut buf = String::new();
            if let Some(prefix) = prefix {
                buf.push_str(prefix);
                buf.push(':');
            }
            buf.push_str(name);

            let sum = self.start_element(&buf)?;

            if let Some(namespace_uri) = namespace_uri {
                let mut buf = "xmlns".to_owned();
                if let Some(prefix) = prefix {
                    buf.push(':');
                    buf.push_str(prefix);
                }

                let p = XmlTextWriterNsStackEntry {
                    prefix: buf,
                    uri: namespace_uri.to_owned(),
                    elem: self.nodes.front().cloned(),
                };
                self.nsstack.push_first(p);
            }

            Ok(sum)
        }
    }

    /// Write an xml element with namespace support.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterWriteElementNS")]
    pub unsafe fn write_element_ns(
        &mut self,
        prefix: Option<&str>,
        name: &str,
        namespace_uri: Option<&str>,
        content: &str,
    ) -> io::Result<usize> {
        unsafe {
            if name.is_empty() {
                return Err(io::Error::other("Writer or name is NULL"));
            }

            let mut sum = self.start_element_ns(prefix, name, namespace_uri)?;
            sum += self.write_string(content)?;
            sum += self.end_element()?;
            Ok(sum)
        }
    }

    /// Start an xml attribute.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterStartAttribute")]
    pub unsafe fn start_attribute(&mut self, name: &str) -> io::Result<usize> {
        unsafe {
            if name.is_empty() {
                return Err(io::Error::other("Writer or name is invalid"));
            }

            let Some(lk) = self.nodes.front().cloned() else {
                return Err(io::Error::other("Node link is NULL"));
            };

            let mut sum = 0;
            match lk.state.get() {
                ty @ XmlTextWriterState::XmlTextwriterAttribute
                | ty @ XmlTextWriterState::XmlTextwriterName => {
                    if matches!(ty, XmlTextWriterState::XmlTextwriterAttribute) {
                        sum += self.end_attribute()?;
                    }

                    sum += self.out.write_str(" ")?;
                    sum += self.out.write_str(name)?;
                    sum += self.out.write_str("=")?;
                    sum += self.out.write_bytes(&[self.qchar])?;
                    lk.state.set(XmlTextWriterState::XmlTextwriterAttribute);
                }
                _ => {
                    return Err(io::Error::other("State is invalid"));
                }
            }

            Ok(sum)
        }
    }

    /// Write an xml attribute.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterWriteAttribute")]
    pub unsafe fn write_attribute(&mut self, name: &str, content: &str) -> io::Result<usize> {
        unsafe {
            let mut sum = 0;
            sum += self.start_attribute(name)?;
            sum += self.write_string(content)?;
            sum += self.end_attribute()?;
            Ok(sum)
        }
    }

    /// End the current xml element.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterEndAttribute")]
    pub unsafe fn end_attribute(&mut self) -> io::Result<usize> {
        let Some(lk) = self.nodes.front_mut() else {
            return Err(io::Error::other("List is NULL"));
        };

        let mut sum = 0;
        match lk.state.get() {
            XmlTextWriterState::XmlTextwriterAttribute => {
                lk.state.set(XmlTextWriterState::XmlTextwriterName);

                sum += self.out.write_bytes(&[self.qchar])?;
            }
            _ => {
                return Err(io::Error::other("Not attribute"));
            }
        }

        Ok(sum)
    }

    /// Start an xml attribute with namespace support.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterStartAttributeNS")]
    pub unsafe fn start_attribute_ns(
        &mut self,
        prefix: Option<&str>,
        name: &str,
        namespace_uri: Option<&str>,
    ) -> io::Result<usize> {
        unsafe {
            if name.is_empty() {
                return Err(io::Error::other("Writer or name is invalid"));
            }

            // Handle namespace first in case of error
            if let Some(namespace_uri) = namespace_uri {
                let mut buf = "xmlns".to_owned();
                if let Some(prefix) = prefix {
                    buf.push(':');
                    buf.push_str(prefix);
                }

                let nsentry = XmlTextWriterNsStackEntry {
                    prefix: buf.clone(),
                    uri: namespace_uri.to_owned(),
                    elem: self.nodes.front().cloned(),
                };

                if let Some(curns) = self.nsstack.search(&nsentry) {
                    if curns.uri == namespace_uri {
                        // Namespace already defined on element skip
                        buf.clear();
                    } else {
                        // Prefix mismatch so error out
                        return Err(io::Error::other("Prefix mismatch"));
                    }
                }

                // Do not add namespace decl to list - it is already there
                if !buf.is_empty() {
                    let p = XmlTextWriterNsStackEntry {
                        prefix: buf,
                        uri: namespace_uri.to_owned(),
                        elem: self.nodes.front().cloned(),
                    };
                    self.nsstack.push_first(p);
                }
            }

            let mut buf = String::new();
            if let Some(prefix) = prefix {
                buf.push_str(prefix);
                buf.push(':');
            }
            buf.push_str(name);

            self.start_attribute(&buf)
        }
    }

    /// Write an xml attribute.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterWriteAttributeNS")]
    pub unsafe fn write_attribute_ns(
        &mut self,
        prefix: Option<&str>,
        name: &str,
        namespace_uri: Option<&str>,
        content: &str,
    ) -> io::Result<usize> {
        unsafe {
            if name.is_empty() {
                return Err(io::Error::other("Writer or name is NULL"));
            }

            let mut sum = self.start_attribute_ns(prefix, name, namespace_uri)?;
            sum += self.write_string(content)?;
            sum += self.end_attribute()?;
            Ok(sum)
        }
    }

    /// Output the current namespace declarations.
    #[doc(alias = "xmlTextWriterOutputNSDecl")]
    unsafe fn output_nsdecl(&mut self) -> io::Result<usize> {
        unsafe {
            let mut sum = 0;
            while let Some(lk) = self.nsstack.pop_first() {
                let count = self.write_attribute(&lk.prefix, &lk.uri);
                if count.is_err() {
                    self.nsstack.clear();
                    return count;
                }
                sum += count.unwrap();
            }
            Ok(sum)
        }
    }

    /// Start an xml PI.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterStartPI")]
    pub unsafe fn start_pi(&mut self, target: &str) -> io::Result<usize> {
        unsafe {
            if target.is_empty() {
                return Err(io::Error::other("Writer or target is NULL"));
            }

            if target.eq_ignore_ascii_case("xml") {
                xml_writer_err_msg(
                    Some(self),
                    XmlParserErrors::XmlErrInternalError,
                    "xmlTextWriterStartPI : target name [Xx][Mm][Ll] is reserved for xml standardization!\n",
                );
                return Err(io::Error::other(
                    "xmlTextWriterStartPI : target name [Xx][Mm][Ll] is reserved for xml standardization!",
                ));
            }

            let mut sum = 0;
            if let Some(lk) = self.nodes.front().cloned() {
                match lk.state.get() {
                    ty @ XmlTextWriterState::XmlTextwriterAttribute
                    | ty @ XmlTextWriterState::XmlTextwriterName => {
                        if matches!(ty, XmlTextWriterState::XmlTextwriterAttribute) {
                            sum += self.end_attribute()?;
                        }
                        // Output namespace declarations
                        sum += self.output_nsdecl()?;
                        sum += self.out.write_str(">")?;
                        lk.state.set(XmlTextWriterState::XmlTextwriterText);
                    }
                    XmlTextWriterState::XmlTextwriterNone
                    | XmlTextWriterState::XmlTextwriterText
                    | XmlTextWriterState::XmlTextwriterDTD => {}
                    XmlTextWriterState::XmlTextwriterPI
                    | XmlTextWriterState::XmlTextwriterPIText => {
                        xml_writer_err_msg(
                            Some(self),
                            XmlParserErrors::XmlErrInternalError,
                            "xmlTextWriterStartPI : nested PI!\n",
                        );
                        return Err(io::Error::other("xmlTextWriterStartPI : nested PI!"));
                    }
                    _ => {
                        return Err(io::Error::other("Invalid state"));
                    }
                }
            }

            let p = XmlTextWriterStackEntry {
                name: Some(target.to_owned()),
                state: Cell::new(XmlTextWriterState::XmlTextwriterPI),
            };
            self.nodes.push_front(p.into());

            sum += self.out.write_str("<?")?;
            sum += self.out.write_str(target)?;
            Ok(sum)
        }
    }

    /// Write an xml PI.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterWritePI")]
    pub unsafe fn write_pi(&mut self, target: &str, content: Option<&str>) -> io::Result<usize> {
        unsafe {
            let mut sum = self.start_pi(target)?;
            if let Some(content) = content {
                sum += self.write_string(content)?;
            }
            sum += self.end_pi()?;
            Ok(sum)
        }
    }

    /// End the current xml PI.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterEndPI")]
    pub unsafe fn end_pi(&mut self) -> io::Result<usize> {
        let Some(lk) = self.nodes.front() else {
            return Ok(0);
        };

        let mut sum = 0;
        match lk.state.get() {
            XmlTextWriterState::XmlTextwriterPI | XmlTextWriterState::XmlTextwriterPIText => {
                sum += self.out.write_str("?>")?;
            }
            _ => {
                return Err(io::Error::other("Not PI"));
            }
        }

        if self.indent != 0 {
            sum += self.out.write_str("\n")?;
        }

        self.nodes.pop_front();
        Ok(sum)
    }

    /// Start an xml CDATA section.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterStartCDATA")]
    pub unsafe fn start_cdata(&mut self) -> io::Result<usize> {
        unsafe {
            let mut sum = 0;
            if let Some(lk) = self.nodes.front().cloned() {
                match lk.state.get() {
                    XmlTextWriterState::XmlTextwriterNone
                    | XmlTextWriterState::XmlTextwriterText
                    | XmlTextWriterState::XmlTextwriterPI
                    | XmlTextWriterState::XmlTextwriterPIText => {}
                    ty @ XmlTextWriterState::XmlTextwriterAttribute
                    | ty @ XmlTextWriterState::XmlTextwriterName => {
                        if matches!(ty, XmlTextWriterState::XmlTextwriterAttribute) {
                            sum += self.end_attribute()?;
                        }

                        // Output namespace declarations
                        sum += self.output_nsdecl()?;
                        sum += self.out.write_str(">")?;
                        lk.state.set(XmlTextWriterState::XmlTextwriterText);
                    }
                    XmlTextWriterState::XmlTextwriterCDATA => {
                        xml_writer_err_msg(
                            Some(self),
                            XmlParserErrors::XmlErrInternalError,
                            "xmlTextWriterStartCDATA : CDATA not allowed in this context!\n",
                        );
                        return Err(io::Error::other(
                            "xmlTextWriterStartCDATA : CDATA not allowed in this context!",
                        ));
                    }
                    _ => {
                        return Err(io::Error::other("Invalid State"));
                    }
                }
            }

            self.nodes.push_front(
                XmlTextWriterStackEntry {
                    name: None,
                    state: Cell::new(XmlTextWriterState::XmlTextwriterCDATA),
                }
                .into(),
            );

            sum += self.out.write_str("<![CDATA[")?;
            Ok(sum)
        }
    }

    /// Write an xml CDATA.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterWriteCDATA")]
    pub unsafe fn write_cdata(&mut self, content: Option<&str>) -> io::Result<usize> {
        unsafe {
            let mut sum = self.start_cdata()?;
            if let Some(content) = content {
                sum += self.write_string(content)?;
            }
            sum += self.end_cdata()?;
            Ok(sum)
        }
    }

    /// End an xml CDATA section.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterEndCDATA")]
    pub unsafe fn end_cdata(&mut self) -> io::Result<usize> {
        let Some(lk) = self.nodes.front() else {
            return Err(io::Error::other("Node list is NULL"));
        };

        let mut sum = 0;
        match lk.state.get() {
            XmlTextWriterState::XmlTextwriterCDATA => {
                sum += self.out.write_str("]]>")?;
            }
            _ => {
                return Err(io::Error::other("Invalid state"));
            }
        }

        self.nodes.pop_front();
        Ok(sum)
    }

    /// Start an xml comment.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterStartComment")]
    pub unsafe fn start_comment(&mut self) -> io::Result<usize> {
        unsafe {
            let mut sum = 0;
            if let Some(lk) = self.nodes.front().cloned() {
                match lk.state.get() {
                    XmlTextWriterState::XmlTextwriterText
                    | XmlTextWriterState::XmlTextwriterNone => {}
                    XmlTextWriterState::XmlTextwriterName => {
                        // Output namespace declarations
                        sum += self.output_nsdecl()?;
                        sum += self.out.write_str(">")?;
                        if self.indent != 0 {
                            sum += self.out.write_str("\n")?;
                        }
                        lk.state.set(XmlTextWriterState::XmlTextwriterText);
                    }
                    _ => {
                        return Err(io::Error::other("Invalid state"));
                    }
                }
            }

            self.nodes.push_front(
                XmlTextWriterStackEntry {
                    name: None,
                    state: Cell::new(XmlTextWriterState::XmlTextwriterComment),
                }
                .into(),
            );

            if self.indent != 0 {
                sum += self.write_indent()?;
            }

            sum += self.out.write_str("<!--")?;
            Ok(sum)
        }
    }

    /// Write an xml comment.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterWriteComment")]
    pub unsafe fn write_comment(&mut self, content: &str) -> io::Result<usize> {
        unsafe {
            let mut sum = self.start_comment()?;
            sum += self.write_string(content)?;
            sum += self.end_comment()?;
            Ok(sum)
        }
    }

    /// End the current xml comment.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterEndComment")]
    pub fn end_comment(&mut self) -> io::Result<usize> {
        let Some(lk) = self.nodes.front() else {
            xml_writer_err_msg(
                Some(self),
                XmlParserErrors::XmlErrInternalError,
                "xmlTextWriterEndComment : not allowed in this context!\n",
            );
            return Err(io::Error::other(
                "xmlTextWriterEndComment : not allowed in this context!",
            ));
        };

        let mut sum = 0;
        match lk.state.get() {
            XmlTextWriterState::XmlTextwriterComment => {
                sum += self.out.write_str("-->")?;
            }
            _ => {
                return Err(io::Error::other("Invalid state"));
            }
        }

        if self.indent != 0 {
            sum += self.out.write_str("\n")?;
        }

        self.nodes.pop_front();
        Ok(sum)
    }

    /// Start an xml DTD.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterStartDTD")]
    pub fn start_dtd(
        &mut self,
        name: &str,
        pubid: Option<&str>,
        sysid: Option<&str>,
    ) -> io::Result<usize> {
        if name.is_empty() {
            return Err(io::Error::other("Writer or name is NULL"));
        }

        if self.nodes.front().is_some() {
            xml_writer_err_msg(
                Some(self),
                XmlParserErrors::XmlErrInternalError,
                "xmlTextWriterStartDTD : DTD allowed only in prolog!\n",
            );
            return Err(io::Error::other(
                "xmlTextWriterStartDTD : DTD allowed only in prolog!",
            ));
        }

        let p = XmlTextWriterStackEntry {
            name: Some(name.to_owned()),
            state: Cell::new(XmlTextWriterState::XmlTextwriterDTD),
        };
        self.nodes.push_front(p.into());

        let mut sum = self.out.write_str("<!DOCTYPE ")?;
        sum += self.out.write_str(name)?;

        if let Some(pubid) = pubid {
            if sysid.is_none() {
                xml_writer_err_msg(
                    Some(self),
                    XmlParserErrors::XmlErrInternalError,
                    "xmlTextWriterStartDTD : system identifier needed!\n",
                );
                return Err(io::Error::other(
                    "xmlTextWriterStartDTD : system identifier needed!",
                ));
            };

            if self.indent != 0 {
                sum += self.out.write_bytes(b"\n")?;
            } else {
                sum += self.out.write_bytes(b" ")?;
            }

            sum += self.out.write_str("PUBLIC ")?;
            sum += self.out.write_bytes(&[self.qchar])?;
            sum += self.out.write_str(pubid)?;
            sum += self.out.write_bytes(&[self.qchar])?;
        }

        if let Some(sysid) = sysid {
            if pubid.is_some() {
                if self.indent != 0 {
                    sum += self.out.write_str("\n       ")?;
                } else {
                    sum += self.out.write_bytes(b" ")?;
                }
            } else {
                if self.indent != 0 {
                    sum += self.out.write_bytes(b"\n")?;
                } else {
                    sum += self.out.write_bytes(b" ")?;
                }
                sum += self.out.write_str("SYSTEM ")?;
            }

            sum += self.out.write_bytes(&[self.qchar])?;
            sum += self.out.write_str(sysid)?;
            sum += self.out.write_bytes(&[self.qchar])?;
        }

        Ok(sum)
    }

    /// Write a DTD.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterWriteDTD")]
    pub unsafe fn write_dtd(
        &mut self,
        name: &str,
        pubid: Option<&str>,
        sysid: Option<&str>,
        subset: Option<&str>,
    ) -> io::Result<usize> {
        unsafe {
            let mut sum = self.start_dtd(name, pubid, sysid)?;
            if let Some(subset) = subset {
                sum += self.write_string(subset)?;
            }
            sum += self.end_dtd()?;
            Ok(sum)
        }
    }

    /// End an xml DTD.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterEndDTD")]
    pub unsafe fn end_dtd(&mut self) -> io::Result<usize> {
        unsafe {
            let mut sum = 0;
            while let Some(lk) = self.nodes.front() {
                match lk.state.get() {
                    ty @ XmlTextWriterState::XmlTextwriterDTDText
                    | ty @ XmlTextWriterState::XmlTextwriterDTD => {
                        if matches!(ty, XmlTextWriterState::XmlTextwriterDTDText) {
                            sum += self.out.write_str("]")?;
                        }
                        sum += self.out.write_str(">")?;

                        if self.indent != 0 {
                            sum += self.out.write_str("\n")?;
                        }

                        self.nodes.pop_front();
                    }
                    XmlTextWriterState::XmlTextwriterDTDElem
                    | XmlTextWriterState::XmlTextwriterDTDElemText => {
                        sum += self.end_dtd_element()?;
                    }
                    XmlTextWriterState::XmlTextwriterDTDAttl
                    | XmlTextWriterState::XmlTextwriterDTDAttlText => {
                        sum += self.end_dtd_attlist()?;
                    }
                    XmlTextWriterState::XmlTextwriterDTDEnty
                    | XmlTextWriterState::XmlTextwriterDTDPEnt
                    | XmlTextWriterState::XmlTextwriterDTDEntyText => {
                        sum += self.end_dtd_entity()?;
                    }
                    XmlTextWriterState::XmlTextwriterComment => {
                        sum += self.end_comment()?;
                    }
                    _ => {
                        break;
                    }
                }
            }

            Ok(sum)
        }
    }

    /// Start an xml DTD element.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterStartDTDElement")]
    pub unsafe fn start_dtd_element(&mut self, name: &str) -> io::Result<usize> {
        unsafe {
            if name.is_empty() {
                return Err(io::Error::other("Writer or name is NULL"));
            }

            let Some(lk) = self.nodes.front_mut() else {
                return Err(io::Error::other("Node list is NULL"));
            };

            let mut sum = 0;
            match lk.state.get() {
                XmlTextWriterState::XmlTextwriterDTD => {
                    sum += self.out.write_str(" [")?;
                    if self.indent != 0 {
                        sum += self.out.write_str("\n")?;
                    }
                    lk.state.set(XmlTextWriterState::XmlTextwriterDTDText);
                }
                XmlTextWriterState::XmlTextwriterDTDText
                | XmlTextWriterState::XmlTextwriterNone => {}
                _ => {
                    return Err(io::Error::other("Invalid state"));
                }
            }

            let p = XmlTextWriterStackEntry {
                name: Some(name.to_owned()),
                state: Cell::new(XmlTextWriterState::XmlTextwriterDTDElem),
            };
            self.nodes.push_front(p.into());

            if self.indent != 0 {
                sum += self.write_indent()?;
            }

            sum += self.out.write_str("<!ELEMENT ")?;
            sum += self.out.write_str(name)?;
            Ok(sum)
        }
    }

    /// Write a DTD element.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterWriteDTDElement")]
    pub unsafe fn write_dtd_element(&mut self, name: &str, content: &str) -> io::Result<usize> {
        unsafe {
            let mut sum = self.start_dtd_element(name)?;
            sum += self.write_string(content)?;
            sum += self.end_dtd_element()?;
            Ok(sum)
        }
    }

    /// End an xml DTD element.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterEndDTDElement")]
    pub unsafe fn end_dtd_element(&mut self) -> io::Result<usize> {
        let Some(lk) = self.nodes.front() else {
            return Err(io::Error::other("Node list is NULL"));
        };

        let mut sum = 0;
        match lk.state.get() {
            XmlTextWriterState::XmlTextwriterDTDElem
            | XmlTextWriterState::XmlTextwriterDTDElemText => {
                sum += self.out.write_str(">")?;
            }
            _ => {
                return Err(io::Error::other("Invalid state"));
            }
        }

        if self.indent != 0 {
            sum += self.out.write_str("\n")?;
        }

        self.nodes.pop_front();
        Ok(sum)
    }

    /// Start an xml DTD ATTLIST.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterStartDTDAttlist")]
    pub unsafe fn start_dtd_attlist(&mut self, name: &str) -> io::Result<usize> {
        unsafe {
            if name.is_empty() {
                return Err(io::Error::other("Writer or name is NULL"));
            }

            let Some(lk) = self.nodes.front_mut() else {
                return Err(io::Error::other("Node list is NULL"));
            };

            let mut sum = 0;
            match lk.state.get() {
                XmlTextWriterState::XmlTextwriterDTD => {
                    sum += self.out.write_str(" [")?;
                    if self.indent != 0 {
                        sum += self.out.write_str("\n")?;
                    }
                    lk.state.set(XmlTextWriterState::XmlTextwriterDTDText);
                }
                XmlTextWriterState::XmlTextwriterDTDText
                | XmlTextWriterState::XmlTextwriterNone => {}
                _ => {
                    return Err(io::Error::other("Invalid state"));
                }
            }

            let p = XmlTextWriterStackEntry {
                name: Some(name.to_owned()),
                state: Cell::new(XmlTextWriterState::XmlTextwriterDTDAttl),
            };
            self.nodes.push_front(p.into());

            if self.indent != 0 {
                sum += self.write_indent()?;
            }

            sum += self.out.write_str("<!ATTLIST ")?;
            sum += self.out.write_str(name)?;
            Ok(sum)
        }
    }

    /// Write a DTD ATTLIST.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterWriteDTDAttlist")]
    pub unsafe fn write_dtd_attlist(&mut self, name: &str, content: &str) -> io::Result<usize> {
        unsafe {
            let mut sum = self.start_dtd_attlist(name)?;
            sum += self.write_string(content)?;
            sum += self.end_dtd_attlist()?;
            Ok(sum)
        }
    }

    /// End an xml DTD attribute list.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterEndDTDAttlist")]
    pub unsafe fn end_dtd_attlist(&mut self) -> io::Result<usize> {
        let Some(lk) = self.nodes.front() else {
            return Err(io::Error::other("Node list is NULL"));
        };

        let mut sum = 0;
        match lk.state.get() {
            XmlTextWriterState::XmlTextwriterDTDAttl
            | XmlTextWriterState::XmlTextwriterDTDAttlText => {
                sum += self.out.write_str(">")?;
            }
            _ => {
                return Err(io::Error::other("Invalid state"));
            }
        }

        if self.indent != 0 {
            sum += self.out.write_str("\n")?;
        }
        self.nodes.pop_front();
        Ok(sum)
    }

    /// Start an xml DTD ATTLIST.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterStartDTDEntity")]
    pub unsafe fn start_dtd_entity(&mut self, pe: i32, name: &str) -> io::Result<usize> {
        unsafe {
            if name.is_empty() {
                return Err(io::Error::other("Writer or name is NULL"));
            }

            let mut sum = 0;
            if let Some(lk) = self.nodes.front_mut() {
                match lk.state.get() {
                    XmlTextWriterState::XmlTextwriterDTD => {
                        sum += self.out.write_str(" [")?;
                        if self.indent != 0 {
                            sum += self.out.write_str("\n")?;
                        }
                        lk.state.set(XmlTextWriterState::XmlTextwriterDTDText);
                    }
                    _ty @ XmlTextWriterState::XmlTextwriterDTDText
                    | _ty @ XmlTextWriterState::XmlTextwriterNone => {}
                    _ => {
                        return Err(io::Error::other("Invalid state"));
                    }
                }
            }

            let p = XmlTextWriterStackEntry {
                name: Some(name.to_owned()),
                state: if pe != 0 {
                    Cell::new(XmlTextWriterState::XmlTextwriterDTDPEnt)
                } else {
                    Cell::new(XmlTextWriterState::XmlTextwriterDTDEnty)
                },
            };
            self.nodes.push_front(p.into());

            if self.indent != 0 {
                sum += self.write_indent()?;
            }
            sum += self.out.write_str("<!ENTITY ")?;
            if pe != 0 {
                sum += self.out.write_str("% ")?;
            }
            sum += self.out.write_str(name)?;
            Ok(sum)
        }
    }

    /// Write a DTD entity.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterWriteDTDEntity")]
    pub unsafe fn write_dtd_entity(
        &mut self,
        pe: i32,
        name: &str,
        pubid: Option<&str>,
        sysid: Option<&str>,
        ndataid: Option<&str>,
        content: Option<&str>,
    ) -> io::Result<usize> {
        unsafe {
            if content.is_none() && pubid.is_none() && sysid.is_none() {
                return Err(io::Error::other("Content, PublicID and SystemID are NULL"));
            }
            if pe != 0 && ndataid.is_some() {
                return Err(io::Error::other("This is PE, but NDATAID is not NULL"));
            }

            if pubid.is_none() && sysid.is_none() {
                return self.write_dtd_internal_entity(pe, name, content.unwrap());
            }

            self.write_dtd_external_entity(pe, name, pubid, sysid, ndataid)
        }
    }

    /// Write a DTD internal entity.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterWriteDTDInternalEntity")]
    pub unsafe fn write_dtd_internal_entity(
        &mut self,
        pe: i32,
        name: &str,
        content: &str,
    ) -> io::Result<usize> {
        unsafe {
            if name.is_empty() {
                return Err(io::Error::other("Writer or name is NULL"));
            }

            let mut sum = self.start_dtd_entity(pe, name)?;
            sum += self.write_string(content)?;
            sum += self.end_dtd_entity()?;
            Ok(sum)
        }
    }

    /// Write a DTD external entity. The entity must have been started with xmlTextWriterStartDTDEntity
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterWriteDTDExternalEntity")]
    pub unsafe fn write_dtd_external_entity(
        &mut self,
        pe: i32,
        name: &str,
        pubid: Option<&str>,
        sysid: Option<&str>,
        ndataid: Option<&str>,
    ) -> io::Result<usize> {
        unsafe {
            if pubid.is_none() && sysid.is_none() {
                return Err(io::Error::other("Both ExternalID and PublicID is NULL"));
            }
            if pe != 0 && ndataid.is_some() {
                return Err(io::Error::other("This is PE, but NDATAID is not NULL"));
            }

            let mut sum = self.start_dtd_entity(pe, name)?;
            sum += self.write_dtd_external_entity_contents(pubid, sysid, ndataid)?;
            sum += self.end_dtd_entity()?;
            Ok(sum)
        }
    }

    /// Write the contents of a DTD external entity.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterWriteDTDExternalEntityContents")]
    pub fn write_dtd_external_entity_contents(
        &mut self,
        pubid: Option<&str>,
        sysid: Option<&str>,
        ndataid: Option<&str>,
    ) -> io::Result<usize> {
        let Some(lk) = self.nodes.front() else {
            xml_writer_err_msg(
                Some(self),
                XmlParserErrors::XmlErrInternalError,
                "xmlTextWriterWriteDTDExternalEntityContents: you must call xmlTextWriterStartDTDEntity before the call to this function!\n",
            );
            return Err(io::Error::other(
                "xmlTextWriterWriteDTDExternalEntityContents: you must call xmlTextWriterStartDTDEntity before the call to this function!",
            ));
        };

        match lk.state.get() {
            XmlTextWriterState::XmlTextwriterDTDEnty => {}
            XmlTextWriterState::XmlTextwriterDTDPEnt => {
                if ndataid.is_some() {
                    xml_writer_err_msg(
                        Some(self),
                        XmlParserErrors::XmlErrInternalError,
                        "xmlTextWriterWriteDTDExternalEntityContents: notation not allowed with parameter entities!\n",
                    );
                    return Err(io::Error::other(
                        "xmlTextWriterWriteDTDExternalEntityContents: notation not allowed with parameter entities!",
                    ));
                }
            }
            _ => {
                xml_writer_err_msg(
                    Some(self),
                    XmlParserErrors::XmlErrInternalError,
                    "xmlTextWriterWriteDTDExternalEntityContents: you must call xmlTextWriterStartDTDEntity before the call to this function!\n",
                );
                return Err(io::Error::other(
                    "xmlTextWriterWriteDTDExternalEntityContents: you must call xmlTextWriterStartDTDEntity before the call to this function!",
                ));
            }
        }

        let mut sum = 0;
        if let Some(pubid) = pubid {
            if sysid.is_none() {
                xml_writer_err_msg(
                    Some(self),
                    XmlParserErrors::XmlErrInternalError,
                    "xmlTextWriterWriteDTDExternalEntityContents: system identifier needed!\n",
                );
                return Err(io::Error::other(
                    "xmlTextWriterWriteDTDExternalEntityContents: system identifier needed!",
                ));
            }

            sum += self.out.write_str(" PUBLIC ")?;
            sum += self.out.write_bytes(&[self.qchar])?;
            sum += self.out.write_str(pubid)?;
            sum += self.out.write_bytes(&[self.qchar])?;
        }

        if let Some(sysid) = sysid {
            if pubid.is_none() {
                sum += self.out.write_str(" SYSTEM")?;
            }
            sum += self.out.write_str(" ")?;
            sum += self.out.write_bytes(&[self.qchar])?;
            sum += self.out.write_str(sysid)?;
            sum += self.out.write_bytes(&[self.qchar])?;
        }

        if let Some(ndataid) = ndataid {
            sum += self.out.write_str(" NDATA ")?;
            sum += self.out.write_str(ndataid)?;
        }
        Ok(sum)
    }

    /// End an xml DTD entity.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterEndDTDEntity")]
    pub unsafe fn end_dtd_entity(&mut self) -> io::Result<usize> {
        let Some(lk) = self.nodes.front() else {
            return Err(io::Error::other("Node list is NULL"));
        };

        let mut sum = 0;
        match lk.state.get() {
            ty @ XmlTextWriterState::XmlTextwriterDTDEntyText
            | ty @ XmlTextWriterState::XmlTextwriterDTDEnty
            | ty @ XmlTextWriterState::XmlTextwriterDTDPEnt => {
                if matches!(ty, XmlTextWriterState::XmlTextwriterDTDEntyText) {
                    sum += self.out.write_bytes(&[self.qchar])?;
                }
                sum += self.out.write_str(">")?;
            }
            _ => {
                return Err(io::Error::other("Invalid state"));
            }
        }

        if self.indent != 0 {
            sum += self.out.write_str("\n")?;
        }
        self.nodes.pop_front();
        Ok(sum)
    }

    /// Write a DTD entity.
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterWriteDTDNotation")]
    pub unsafe fn write_dtd_notation(
        &mut self,
        name: &str,
        pubid: Option<&str>,
        sysid: Option<&str>,
    ) -> io::Result<usize> {
        unsafe {
            if name.is_empty() {
                return Err(io::Error::other("Writer or name is NULL"));
            }

            let Some(lk) = self.nodes.front_mut() else {
                return Err(io::Error::other("Node list is NULL"));
            };

            let mut sum = 0;
            match lk.state.get() {
                XmlTextWriterState::XmlTextwriterDTD => {
                    sum += self.out.write_str(" [")?;
                    if self.indent != 0 {
                        sum += self.out.write_str("\n")?;
                    }
                    lk.state.set(XmlTextWriterState::XmlTextwriterDTDText);
                }
                XmlTextWriterState::XmlTextwriterDTDText => {}
                _ => {
                    return Err(io::Error::other("Invalid state"));
                }
            }

            if self.indent != 0 {
                sum += self.write_indent()?;
            }

            sum += self.out.write_str("<!NOTATION ")?;
            sum += self.out.write_str(name)?;

            if let Some(pubid) = pubid {
                sum += self.out.write_str(" PUBLIC ")?;
                sum += self.out.write_bytes(&[self.qchar])?;
                sum += self.out.write_str(pubid)?;
                sum += self.out.write_bytes(&[self.qchar])?;
            }

            if let Some(sysid) = sysid {
                if pubid.is_none() {
                    sum += self.out.write_str(" SYSTEM")?;
                }
                sum += self.out.write_str(" ")?;
                sum += self.out.write_bytes(&[self.qchar])?;
                sum += self.out.write_str(sysid)?;
                sum += self.out.write_bytes(&[self.qchar])?;
            }

            sum += self.out.write_str(">")?;
            Ok(sum)
        }
    }
}

impl Default for XmlTextWriter<'_> {
    fn default() -> Self {
        Self {
            out: XmlOutputBuffer::default(),
            nodes: VecDeque::new(),
            nsstack: XmlList::new(None, Rc::new(|_, _| std::cmp::Ordering::Equal)),
            level: 0,
            indent: 0,
            doindent: 0,
            ichar: Cow::Borrowed(" "),
            qchar: b'"',
            ctxt: null_mut(),
            no_doc_free: 0,
            doc: None,
        }
    }
}

impl Drop for XmlTextWriter<'_> {
    /// Deallocate all the resources associated to the writer
    #[doc(alias = "xmlFreeTextWriter")]
    fn drop(&mut self) {
        self.out.flush();

        unsafe {
            if !self.ctxt.is_null() {
                if let Some(my_doc) = (*self.ctxt).my_doc.take_if(|_| self.no_doc_free == 0) {
                    xml_free_doc(my_doc);
                }
                xml_free_parser_ctxt(self.ctxt);
            }

            if let Some(doc) = self.doc.take() {
                xml_free_doc(doc);
            }
        }
    }
}

/// Handle a writer error
#[doc(alias = "xmlWriterErrMsg")]
fn xml_writer_err_msg(ctxt: Option<&XmlTextWriter>, error: XmlParserErrors, msg: &str) {
    if let Some(ctxt) = ctxt {
        __xml_raise_error!(
            None,
            None,
            None,
            ctxt.ctxt as _,
            None,
            XmlErrorDomain::XmlFromWriter,
            error,
            XmlErrorLevel::XmlErrFatal,
            None,
            0,
            None,
            None,
            None,
            0,
            0,
            Some(msg),
        );
    } else {
        __xml_raise_error!(
            None,
            None,
            None,
            null_mut(),
            None,
            XmlErrorDomain::XmlFromWriter,
            error,
            XmlErrorLevel::XmlErrFatal,
            None,
            0,
            None,
            None,
            None,
            0,
            0,
            Some(msg),
        );
    }
}

/// Handle a writer error
#[doc(alias = "xmlWriterErrMsgInt")]
macro_rules! xml_writer_err_msg_int {
    ($ctxt:expr, $error:expr, $msg:literal, $val:expr) => {
        let ctxt = $ctxt as *mut XmlTextWriter;
        __xml_raise_error!(
            None,
            None,
            None,
            if !ctxt.is_null() {
                (*ctxt).ctxt as _
            } else {
                null_mut()
            },
            None,
            XmlErrorDomain::XmlFromWriter,
            $error,
            XmlErrorLevel::XmlErrFatal,
            None,
            0,
            None,
            None,
            None,
            $val,
            0,
            Some(format!($msg, $val).as_str()),
        );
    };
}

/// Write callback for the xmlOutputBuffer with target xmlBuffer
///
/// Returns -1, 0, 1
#[doc(alias = "xmlTextWriterWriteDocCallback")]
unsafe fn xml_text_writer_write_doc_callback(context: *mut c_void, s: &[u8]) -> i32 {
    unsafe {
        let ctxt: XmlParserCtxtPtr = context as XmlParserCtxtPtr;

        let rc = (*ctxt).parse_chunk(s, 0);
        if rc != 0 {
            xml_writer_err_msg_int!(
                null_mut(),
                XmlParserErrors::XmlErrInternalError,
                "xmlTextWriterWriteDocCallback : XML error {} !\n",
                rc
            );
            return -1;
        }

        s.len() as i32
    }
}

/// Close callback for the xmlOutputBuffer with target xmlBuffer
///
/// Returns -1, 0, 1
#[doc(alias = "xmlTextWriterCloseDocCallback")]
unsafe fn xml_text_writer_close_doc_callback(context: *mut c_void) -> i32 {
    unsafe {
        let ctxt: XmlParserCtxtPtr = context as XmlParserCtxtPtr;
        let rc: i32;

        let res = {
            rc = (*ctxt).parse_chunk(&[], 1);
            rc != 0
        };
        if res {
            xml_writer_err_msg_int!(
                null_mut(),
                XmlParserErrors::XmlErrInternalError,
                "xmlTextWriterCloseDocCallback : XML error {} !\n",
                rc
            );
            return -1;
        }

        0
    }
}

struct TextWriterPushContext {
    context: XmlParserCtxtPtr,
}

impl Write for TextWriterPushContext {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let res = unsafe { xml_text_writer_write_doc_callback(self.context as _, buf) };
        if res >= 0 {
            Ok(res as usize)
        } else {
            Err(io::Error::last_os_error())
        }
    }
    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl Drop for TextWriterPushContext {
    fn drop(&mut self) {
        unsafe {
            xml_text_writer_close_doc_callback(self.context as _);
        }
    }
}

/// called at the start of document processing.
#[doc(alias = "xmlTextWriterStartDocumentCallback")]
unsafe fn xml_text_writer_start_document_callback(ctx: Option<GenericErrorContext>) {
    unsafe {
        let ctxt = {
            let ctx = ctx.as_ref().unwrap();
            let lock = ctx.lock();
            *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
        };

        if (*ctxt).html != 0 {
            #[cfg(feature = "html")]
            {
                if (*ctxt).my_doc.is_none() {
                    (*ctxt).my_doc = html_new_doc_no_dtd(None, None);
                }
                if (*ctxt).my_doc.is_none() {
                    if let Some(error) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.error) {
                        error(
                            (*ctxt).user_data.clone(),
                            "SAX.startDocument(): out of memory\n",
                        );
                    }
                    (*ctxt).err_no = XmlParserErrors::XmlErrNoMemory as i32;
                    (*ctxt).instate = XmlParserInputState::XmlParserEOF;
                    (*ctxt).disable_sax = 1;
                    return;
                }
            }
            #[cfg(not(feature = "html"))]
            {
                xml_writer_err_msg(
                    None,
                    XmlParserErrors::XmlErrInternalError,
                    "libxml2 built without HTML support\n",
                );
                (*ctxt).err_no = XmlParserErrors::XmlErrInternalError as i32;
                (*ctxt).instate = XmlParserInputState::XmlParserEOF;
                (*ctxt).disable_sax = 1;
                return;
            }
        } else {
            let doc = (*ctxt).my_doc.or_else(|| {
                (*ctxt).my_doc = xml_new_doc((*ctxt).version.as_deref());
                (*ctxt).my_doc
            });
            if let Some(mut doc) = doc {
                if doc.children.is_none() {
                    doc.encoding = (*ctxt).encoding().map(|e| e.to_owned());
                    doc.standalone = (*ctxt).standalone;
                }
            } else {
                if let Some(error) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.error) {
                    error(
                        (*ctxt).user_data.clone(),
                        "SAX.startDocument(): out of memory\n",
                    );
                }
                (*ctxt).err_no = XmlParserErrors::XmlErrNoMemory as i32;
                (*ctxt).instate = XmlParserInputState::XmlParserEOF;
                (*ctxt).disable_sax = 1;
                return;
            }
        }
        if let Some(filename) = (*ctxt).input().and_then(|input| input.filename.as_deref()) {
            if let Some(mut my_doc) = (*ctxt).my_doc.filter(|doc| doc.url.is_none()) {
                let url = canonic_path(filename);
                my_doc.url = Some(url.into_owned());
                if my_doc.url.is_none() {
                    my_doc.url = Some(filename.to_owned());
                }
            }
        }
    }
}

const B64LINELEN: usize = 72;
const B64CRLF: &str = "\r\n";

/// Write base64 encoded data to an xmlOutputBuffer.
/// Adapted from John Walker's base64.c (http://www.fourmilab.ch/).
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlOutputBufferWriteBase64")]
unsafe fn xml_output_buffer_write_base64(
    out: &mut XmlOutputBuffer,
    data: &[u8],
) -> io::Result<usize> {
    const DTABLE: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

    let mut linelen = 0;
    let mut sum = 0;
    for igroup in data.chunks(3) {
        let mut ogroup: [u8; 4] = [0; 4];
        match *igroup {
            [i0, i1, i2, ..] => {
                ogroup[0] = DTABLE[i0 as usize >> 2];
                ogroup[1] = DTABLE[((i0 as usize & 3) << 4) | (i1 as usize >> 4)];
                ogroup[2] = DTABLE[((i1 as usize & 0xF) << 2) | (i2 as usize >> 6)];
                ogroup[3] = DTABLE[i2 as usize & 0x3F];
            }
            [i0, i1] => {
                ogroup[0] = DTABLE[i0 as usize >> 2];
                ogroup[1] = DTABLE[((i0 as usize & 3) << 4) | (i1 as usize >> 4)];
                ogroup[2] = DTABLE[(i1 as usize & 0xF) << 2];
                ogroup[3] = b'=';
            }
            [i0] => {
                ogroup[0] = DTABLE[i0 as usize >> 2];
                ogroup[1] = DTABLE[(i0 as usize & 3) << 4];
                ogroup[2] = b'=';
                ogroup[3] = b'=';
            }
            [] => {}
        }
        if linelen >= B64LINELEN as i32 {
            sum += out.write_bytes(B64CRLF.as_bytes())?;
            linelen = 0;
        }
        sum += out.write_bytes(&ogroup)?;
        linelen += 4;
    }

    Ok(sum)
}

/// Write hqx encoded data to an xmlOutputBuffer.
/// ::todo
///
/// Returns the bytes written (may be 0 because of buffering)
/// or -1 in case of error
#[doc(alias = "xmlOutputBufferWriteBinHex")]
fn xml_output_buffer_write_bin_hex(out: &mut XmlOutputBuffer, data: &[u8]) -> io::Result<usize> {
    const LUT: [u8; 512] = {
        let mut lut = [0; 512];
        let mut data = 0;
        while data < u8::MAX as usize {
            let (hi, lo) = (data >> 4, data & 0x0F);
            lut[data * 2] = hi as u8;
            lut[data * 2 + 1] = lo as u8;
            data += 1;
        }
        lut
    };

    let mut sum = 0;
    for &data in data {
        let start = data as usize * 2;
        sum += out.write_bytes(&LUT[start..start + 2])?;
    }

    Ok(sum)
}
