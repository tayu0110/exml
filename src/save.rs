//! Provide methods and data structures for serializing XML documents.
//!
//! This module is based on `libxml/xmlsave.h`, `xmlsave.c`, and so on in `libxml2-v2.11.8`.  
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: the XML document serializer
// Description: API to save document or subtree of document
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// xmlsave.c: Implementation of the document serializer
//
// See Copyright for the status of this software.
//
// daniel@veillard.com

use std::{
    borrow::Cow,
    cell::RefCell,
    ffi::CString,
    io::Write,
    os::raw::c_void,
    ptr::{fn_addr_eq, null_mut},
    rc::Rc,
    str::from_utf8_unchecked,
};

use crate::{
    buf::XmlBufRef,
    encoding::{XmlCharEncoding, XmlCharEncodingHandler, find_encoding_handler},
    error::{__xml_simple_error, __xml_simple_oom_error, XmlErrorDomain, XmlParserErrors},
    globals::{GLOBAL_STATE, get_indent_tree_output},
    io::XmlOutputBuffer,
    libxml::{
        chvalid::xml_is_char,
        valid::{xml_dump_attribute_decl, xml_dump_element_decl, xml_dump_notation_table},
    },
    parser::{XML_STRING_TEXT_NOENC, xml_init_parser},
    tree::{
        NodeCommon, XML_LOCAL_NAMESPACE, XmlAttrPtr, XmlAttributePtr, XmlDocPtr, XmlDtdPtr,
        XmlElementPtr, XmlElementType, XmlEntityPtr, XmlGenericNodePtr, XmlNodePtr, XmlNotation,
        XmlNsPtr, xml_dump_entity_decl,
    },
};
#[cfg(feature = "html")]
use crate::{
    html::tree::{
        html_doc_content_dump_format_output, html_get_meta_encoding, html_set_meta_encoding,
    },
    tree::{XmlNode, is_xhtml},
};

use super::hash::XmlHashTable;

const MAX_INDENT: usize = 60;

/// This is the set of XML save options that can be passed down
/// to the xmlSaveToFd() and similar calls.
#[doc(alias = "xmlSaveOption")]
#[repr(C)]
pub enum XmlSaveOption {
    XmlSaveFormat = 1 << 0,   // format save output
    XmlSaveNoDecl = 1 << 1,   // drop the xml declaration
    XmlSaveNoEmpty = 1 << 2,  // no empty tags
    XmlSaveNoXHTML = 1 << 3,  // disable XHTML1 specific rules
    XmlSaveXHTML = 1 << 4,    // force XHTML1 specific rules
    XmlSaveAsXML = 1 << 5,    // force XML serialization on HTML doc
    XmlSaveAsHTML = 1 << 6,   // force HTML serialization on XML doc
    XmlSaveWsNonSig = 1 << 7, // format with non-significant whitespace
}

#[repr(C)]
pub struct XmlSaveCtxt<'a> {
    pub(crate) _private: *mut c_void,
    pub(crate) typ: i32,
    pub(crate) fd: i32,
    pub(crate) filename: Option<String>,
    pub(crate) encoding: Option<String>,
    pub(crate) handler: Option<Rc<RefCell<XmlCharEncodingHandler>>>,
    pub(crate) buf: XmlOutputBuffer<'a>,
    pub(crate) options: i32,
    pub(crate) level: i32,
    pub(crate) format: i32,
    pub(crate) indent: [u8; MAX_INDENT + 1], // array for indenting output
    pub(crate) indent_nr: usize,
    pub(crate) indent_size: usize,
    pub(crate) escape: Option<fn(&str, &mut String) -> i32>, // used for element content
    pub(crate) escape_attr: Option<fn(&str, &mut String) -> i32>, // used for attribute content
}

impl<'a> XmlSaveCtxt<'a> {
    /// Create a new saving context
    ///
    /// Returns the new structure or NULL in case of error
    #[doc(alias = "xmlNewSaveCtxt")]
    fn new(encoding: Option<&str>, mut options: i32) -> Option<Self> {
        let mut ret = XmlSaveCtxt::default();
        if let Some(enc) = encoding {
            ret.handler = find_encoding_handler(enc).map(|e| Rc::new(RefCell::new(e)));
            if ret.handler.is_none() {
                xml_save_err(XmlParserErrors::XmlSaveUnknownEncoding, None, encoding);
                return None;
            }
            ret.encoding = Some(enc.to_owned());
            ret.escape = None;
        }
        ret.init();

        // Use the options
        // Re-check this option as it may already have been set
        if ret.options & XmlSaveOption::XmlSaveNoEmpty as i32 != 0
            && options & XmlSaveOption::XmlSaveNoEmpty as i32 == 0
        {
            options |= XmlSaveOption::XmlSaveNoEmpty as i32;
        }

        ret.options = options;
        if options & XmlSaveOption::XmlSaveFormat as i32 != 0 {
            ret.format = 1;
        } else if options & XmlSaveOption::XmlSaveWsNonSig as i32 != 0 {
            ret.format = 2;
        }

        Some(ret)
    }

    /// Create a document saving context serializing to a filename or possibly
    /// to an URL (but this is less reliable) with the encoding and the options given.
    ///
    /// Returns a new serialization context or NULL in case of error.
    #[doc(alias = "xmlSaveToFilename")]
    pub fn save_to_filename<'b: 'a>(
        filename: &'b str,
        encoding: Option<&'b str>,
        options: i32,
    ) -> Option<Self> {
        let compression: i32 = 0; /* TODO handle compression option */

        let mut ret = Self::new(encoding, options)?;
        let buf = XmlOutputBuffer::from_uri(filename, ret.handler.clone(), compression)?;
        ret.buf = buf;
        Some(ret)
    }

    /// Create a document saving context serializing to a file descriptor
    /// with the encoding and the options given
    ///
    /// Returns a new serialization context or NULL in case of error.
    #[doc(alias = "xmlSaveToIO")]
    pub fn save_to_io(
        ioctx: impl Write + 'a,
        encoding: Option<&'a str>,
        options: i32,
    ) -> Option<Self> {
        let mut ret = Self::new(encoding, options)?;
        let buf = XmlOutputBuffer::from_writer_with_wrapped_encoder(ioctx, ret.handler.clone())?;
        ret.buf = buf;
        Some(ret)
    }

    /// Initialize a saving context
    #[doc(alias = "xmlSaveCtxtInit")]
    pub(crate) fn init(&mut self) {
        if self.encoding.is_none() && self.escape.is_none() {
            self.escape = Some(escape_entities);
        }
        GLOBAL_STATE.with_borrow(|state| {
            let len = state.tree_indent_string.len();
            if len == 0 {
                self.indent.fill(0);
            } else {
                self.indent_size = len;
                self.indent_nr = MAX_INDENT / self.indent_size;
                for chunk in self.indent.chunks_exact_mut(self.indent_size) {
                    chunk.copy_from_slice(state.tree_indent_string.as_bytes());
                }
                self.indent[self.indent_nr * self.indent_size] = 0;
            }

            if state.save_no_empty_tags != 0 {
                self.options |= XmlSaveOption::XmlSaveNoEmpty as i32;
            }
        })
    }

    fn switch_encoding(&mut self, encoding: &str) -> i32 {
        if self.buf.encoder.is_none() && self.buf.conv.is_none() {
            self.buf.encoder = find_encoding_handler(encoding).map(|e| Rc::new(RefCell::new(e)));
            if self.buf.encoder.is_none() {
                xml_save_err(
                    XmlParserErrors::XmlSaveUnknownEncoding,
                    None,
                    Some(encoding),
                );
                return -1;
            }
            self.buf.conv = XmlBufRef::new();
            if self.buf.conv.is_none() {
                xml_save_err_memory("creating encoding buffer");
                return -1;
            }
            // initialize the state, e.g. if outputting a BOM
            self.buf.encode(true).ok();
        }
        0
    }

    fn clear_encoding(&mut self) -> i32 {
        self.buf.flush();
        let _ = self.buf.encoder.take();
        if let Some(conv) = self.buf.conv.take() {
            conv.free();
        }
        0
    }

    /// Set a custom escaping function to be used for text in element content
    ///
    /// Returns 0 if successful or -1 in case of error.
    #[doc(alias = "xmlSaveSetEscape")]
    pub fn set_escape(&mut self, escape: Option<fn(&str, &mut String) -> i32>) -> i32 {
        self.escape = escape;
        0
    }

    /// Set a custom escaping function to be used for text in attribute content
    ///
    /// Returns 0 if successful or -1 in case of error.
    #[doc(alias = "xmlSaveSetAttrEscape")]
    pub fn set_attr_escape(&mut self, escape: Option<fn(&str, &mut String) -> i32>) -> i32 {
        self.escape_attr = escape;
        0
    }

    /// Write out formatting for non-significant whitespace output.
    #[doc(alias = "xmlOutputBufferWriteWSNonSig")]
    fn write_ws_non_sig(&mut self, extra: i32) {
        self.buf.write_bytes(b"\n").ok();
        for i in (0..self.level + extra).step_by(self.indent_nr) {
            let len = self.indent_size
                * if self.level + extra - i > self.indent_nr as i32 {
                    self.indent_nr
                } else {
                    (self.level + extra - i) as usize
                };
            self.buf.write_bytes(&self.indent[..len]).ok();
        }
    }

    /// Save a full document to a saving context
    /// TODO: The function is not fully implemented yet as it does not return the
    /// byte count but 0 instead
    ///
    /// Returns the number of byte written or -1 in case of error
    #[doc(alias = "xmlSaveDoc")]
    pub unsafe fn save_doc(&mut self, doc: XmlDocPtr) -> i64 {
        unsafe {
            let ret: i64 = 0;

            if self.doc_content_dump_output(doc) < 0 {
                return -1;
            }
            ret
        }
    }

    /// Save a subtree starting at the node parameter to a saving context
    /// TODO: The function is not fully implemented yet as it does not return the
    /// byte count but 0 instead
    ///
    /// Returns the number of byte written or -1 in case of error
    #[doc(alias = "xmlSaveTree")]
    pub unsafe fn save_tree(&mut self, node: XmlGenericNodePtr) -> i64 {
        unsafe {
            let ret: i64 = 0;

            #[cfg(feature = "html")]
            {
                if self.options & XmlSaveOption::XmlSaveXHTML as i32 != 0 {
                    xhtml_node_dump_output(self, node);
                    return ret;
                }
                if (!matches!(node.element_type(), XmlElementType::XmlNamespaceDecl)
                    && node
                        .document()
                        .is_some_and(|doc| matches!(doc.typ, XmlElementType::XmlHTMLDocumentNode))
                    && self.options & XmlSaveOption::XmlSaveAsXML as i32 == 0)
                    || self.options & XmlSaveOption::XmlSaveAsHTML as i32 != 0
                {
                    html_node_dump_output_internal(self, node);
                    return ret;
                }
            }
            xml_node_dump_output_internal(self, node);
            ret
        }
    }

    /// Flush a document saving context, i.e. make sure that all bytes have been output.
    ///
    /// Returns the number of byte written or -1 in case of error.
    #[doc(alias = "xmlSaveFlush")]
    pub fn flush(&mut self) -> i32 {
        self.buf.flush()
    }

    /// Close a document saving context, i.e. make sure that all bytes have
    /// been output and free the associated data.
    ///
    /// Returns the number of byte written or -1 in case of error.
    #[doc(alias = "xmlSaveClose")]
    pub fn close(&mut self) -> i32 {
        self.flush()
    }

    /// Dump an XML document.
    #[doc(alias = "xmlDocContentDumpOutput")]
    pub(crate) unsafe fn doc_content_dump_output(&mut self, mut cur: XmlDocPtr) -> i32 {
        unsafe {
            #[cfg(feature = "html")]
            let mut is_html = false;
            let oldenc = cur.encoding.clone();
            let oldctxtenc = self.encoding.clone();
            let oldescape = self.escape;
            let oldescape_attr = self.escape_attr;
            let mut switched_encoding: i32 = 0;

            xml_init_parser();

            if !matches!(
                cur.element_type(),
                XmlElementType::XmlHTMLDocumentNode | XmlElementType::XmlDocumentNode
            ) {
                return -1;
            }

            let mut encoding = self.encoding.clone();
            if let Some(enc) = self.encoding.as_deref() {
                cur.encoding = Some(enc.to_owned());
            } else if let Some(enc) = cur.encoding.as_deref() {
                encoding = Some(enc.to_owned());
            }

            if (matches!(cur.element_type(), XmlElementType::XmlHTMLDocumentNode)
                && (self.options & XmlSaveOption::XmlSaveAsXML as i32) == 0
                && (self.options & XmlSaveOption::XmlSaveXHTML as i32) == 0)
                || self.options & XmlSaveOption::XmlSaveAsHTML as i32 != 0
            {
                #[cfg(feature = "html")]
                {
                    if let Some(enc) = encoding.as_deref() {
                        html_set_meta_encoding(cur, Some(enc));
                    }
                    if encoding.is_none() {
                        encoding = html_get_meta_encoding(cur);
                    }
                    if encoding.is_none() {
                        encoding = Some("HTML".to_owned());
                    }
                    if (encoding.is_some()
                        && oldctxtenc.is_none()
                        && self.buf.encoder.is_none()
                        && self.buf.conv.is_none())
                        && self.switch_encoding(encoding.as_deref().unwrap()) < 0
                    {
                        cur.encoding = oldenc;
                        return -1;
                    }
                    if self.options & XmlSaveOption::XmlSaveFormat as i32 != 0 {
                        html_doc_content_dump_format_output(
                            &mut self.buf,
                            Some(cur),
                            encoding.as_deref(),
                            1,
                        );
                    } else {
                        html_doc_content_dump_format_output(
                            &mut self.buf,
                            Some(cur),
                            encoding.as_deref(),
                            0,
                        );
                    }
                    if self.encoding.is_some() {
                        cur.encoding = oldenc;
                    }
                    return 0;
                }
                #[cfg(not(feature = "html"))]
                {
                    return -1;
                }
            } else if matches!(cur.element_type(), XmlElementType::XmlDocumentNode)
                || self.options & XmlSaveOption::XmlSaveAsXML as i32 != 0
                || self.options & XmlSaveOption::XmlSaveXHTML as i32 != 0
            {
                let enc = if let Some(encoding) = encoding.as_deref() {
                    encoding
                        .parse::<XmlCharEncoding>()
                        .unwrap_or(XmlCharEncoding::Error)
                } else {
                    XmlCharEncoding::None
                };
                if encoding.is_some()
                    && oldctxtenc.is_none()
                    && self.buf.encoder.is_none()
                    && self.buf.conv.is_none()
                    && (self.options & XmlSaveOption::XmlSaveNoDecl as i32) == 0
                {
                    if !matches!(
                        enc,
                        XmlCharEncoding::UTF8 | XmlCharEncoding::None | XmlCharEncoding::ASCII
                    ) {
                        // we need to match to this encoding but just for this
                        // document since we output the XMLDecl the conversion
                        // must be done to not generate not well formed documents.
                        if self.switch_encoding(encoding.as_deref().unwrap()) < 0 {
                            cur.encoding = oldenc;
                            return -1;
                        }
                        switched_encoding = 1;
                    }
                    if self.escape.is_some_and(|escape| {
                        fn_addr_eq(escape, escape_entities as fn(&str, &mut String) -> i32)
                    }) {
                        self.escape = None;
                    }
                    if self.escape_attr.is_some_and(|escape| {
                        fn_addr_eq(escape, escape_entities as fn(&str, &mut String) -> i32)
                    }) {
                        self.escape_attr = None;
                    }
                }

                // Save the XML declaration
                if self.options & XmlSaveOption::XmlSaveNoDecl as i32 == 0 {
                    self.buf.write_bytes(b"<?xml version=").ok();
                    if let Some(version) = cur.version.as_deref() {
                        if let Some(mut buf) = self.buf.buffer {
                            let version = CString::new(version).unwrap();
                            buf.push_quoted_cstr(&version).ok();
                        }
                    } else {
                        self.buf.write_bytes(b"\"1.0\"").ok();
                    }
                    if let Some(encoding) = encoding.as_deref() {
                        self.buf.write_bytes(b" encoding=").ok();
                        if let Some(mut buf) = self.buf.buffer {
                            let enc = CString::new(encoding).unwrap();
                            buf.push_quoted_cstr(enc.as_c_str()).ok();
                        }
                    }
                    match cur.standalone {
                        0 => {
                            self.buf.write_bytes(b" standalone=\"no\"").ok();
                        }
                        1 => {
                            self.buf.write_bytes(b" standalone=\"yes\"").ok();
                        }
                        _ => {}
                    }
                    self.buf.write_bytes(b"?>\n").ok();
                }

                #[cfg(feature = "html")]
                {
                    if self.options & XmlSaveOption::XmlSaveXHTML as i32 != 0 {
                        is_html = true;
                    }
                    if self.options & XmlSaveOption::XmlSaveNoXHTML as i32 == 0 {
                        let dtd = cur.get_int_subset();
                        if let Some(dtd) = dtd {
                            is_html =
                                is_xhtml(dtd.system_id.as_deref(), dtd.external_id.as_deref());
                        }
                    }
                }
                if let Some(children) = cur.children {
                    let mut child = Some(children);

                    while let Some(now) = child {
                        self.level = 0;
                        #[cfg(feature = "html")]
                        if is_html {
                            xhtml_node_dump_output(self, now);
                        } else {
                            xml_node_dump_output_internal(self, now);
                        }
                        #[cfg(not(feature = "html"))]
                        {
                            xml_node_dump_output_internal(self, now);
                        }
                        if !matches!(
                            now.element_type(),
                            XmlElementType::XmlXIncludeStart | XmlElementType::XmlXIncludeEnd
                        ) {
                            self.buf.write_bytes(b"\n").ok();
                        }
                        child = now.next();
                    }
                }
            }

            // Restore the state of the saving context at the end of the document
            if switched_encoding != 0 && oldctxtenc.is_none() {
                self.clear_encoding();
                self.escape = oldescape;
                self.escape_attr = oldescape_attr;
            }
            cur.encoding = oldenc;
            0
        }
    }
}

impl Default for XmlSaveCtxt<'_> {
    fn default() -> Self {
        Self {
            _private: null_mut(),
            typ: 0,
            fd: 0,
            filename: None,
            encoding: None,
            handler: None,
            buf: XmlOutputBuffer::default(),
            options: 0,
            level: 0,
            format: 0,
            indent: [0; MAX_INDENT + 1],
            indent_nr: 0,
            indent_size: 0,
            escape: None,
            escape_attr: None,
        }
    }
}

impl Drop for XmlSaveCtxt<'_> {
    fn drop(&mut self) {
        self.close();
    }
}

/// Handle an out of memory condition
#[doc(alias = "xmlSaveErr")]
pub(crate) fn xml_save_err(
    code: XmlParserErrors,
    node: Option<XmlGenericNodePtr>,
    extra: Option<&str>,
) {
    let msg: Cow<'static, str> = match code {
        XmlParserErrors::XmlSaveNotUTF8 => "string is not in UTF-8\n".into(),
        XmlParserErrors::XmlSaveCharInvalid => "invalid character value\n".into(),
        XmlParserErrors::XmlSaveUnknownEncoding => {
            format!("unknown encoding {}\n", extra.expect("Internal Error")).into()
        }
        XmlParserErrors::XmlSaveNoDoctype => "document has no DOCTYPE\n".into(),
        _ => "unexpected error number\n".into(),
    };
    __xml_simple_error!(XmlErrorDomain::XmlFromOutput, code, node, msg.as_ref());
}

/// Handle an out of memory condition
#[doc(alias = "xmlSaveErrMemory")]
pub(crate) fn xml_save_err_memory(extra: &str) {
    __xml_simple_oom_error(XmlErrorDomain::XmlFromOutput, None, Some(extra));
}

/// # Panics
/// - If c is NULL character, `out.len() >= 5` must be satisfied.
/// - Otherwise, `out.len() >= 5 + (c as u32).ilog2() / 4` must be satisfied.
pub(crate) fn serialize_hex_charref(out: &mut [u8], mut val: u32) -> &[u8] {
    out[..3].copy_from_slice(b"&#x");
    // corner case... if val == 0, ilog2 will panic.
    if val == 0 {
        out[3] = b'0';
        out[4] = b';';
        return &out[..5];
    }

    let len = val.ilog2() as usize / 4 + 1;
    for i in (3..3 + len).rev() {
        match val & 0xF {
            c @ 0..=9 => out[i] = b'0' + c as u8,
            c => out[i] = b'A' + (c as u8 - 10),
        }
        val >>= 4;
    }
    out[3 + len] = b';';
    &out[..=3 + len]
}

/// Take a block of UTF-8 chars in and escape them. Used when there is no
/// encoding specified.
///
/// Returns 0 if success, or -1 otherwise
/// The value of @inlen after return is the number of octets consumed
/// if the return value is positive, else unpredictable.
/// The value of @outlen after return is the number of octets consumed.
#[doc(alias = "xmlEscapeEntities")]
fn escape_entities(src: &str, dst: &mut String) -> i32 {
    let mut out = [0; 13];
    for c in src.chars() {
        match c {
            '<' => dst.push_str("&lt;"),
            '>' => dst.push_str("&gt;"),
            '&' => dst.push_str("&amp;"),
            c @ ('\n' | '\t' | '\u{20}'..'\u{80}') => dst.push(c),
            c => {
                let out = serialize_hex_charref(&mut out, c as u32);
                // # Safety
                // character reference includes only '&', '#', 'x', ';' and ascii hex digits,
                // so `from_utf8_unchecked` won't fail.
                unsafe {
                    dst.push_str(from_utf8_unchecked(out));
                }
            }
        }
    }
    0
}

/// Dump a local Namespace definition.
/// Should be called in the context of attributes dumps.
/// If @ctxt is supplied, @buf should be its buffer.
#[doc(alias = "xmlNsDumpOutput")]
fn xml_ns_dump_output(
    mut buf: Option<&mut XmlOutputBuffer>,
    cur: XmlNsPtr,
    mut ctxt: Option<&mut XmlSaveCtxt>,
) {
    if buf.is_none() && ctxt.is_none() {
        return;
    };
    if matches!(cur.element_type(), XML_LOCAL_NAMESPACE) && cur.href.is_some() {
        if (*cur).prefix().as_deref() == Some("xml") {
            return;
        }

        if let Some(ctxt) = ctxt.as_mut().filter(|ctxt| ctxt.format == 2) {
            ctxt.write_ws_non_sig(2);
        } else if let Some(buf) = buf.as_mut() {
            buf.write_bytes(b" ").ok();
        } else {
            ctxt.as_mut().unwrap().buf.write_bytes(b" ").ok();
        }

        let write = |buf: &mut XmlOutputBuffer| {
            // Within the context of an element attributes
            if let Some(prefix) = cur.prefix() {
                buf.write_bytes(b"xmlns:").ok();
                buf.write_str(&prefix).ok();
            } else {
                buf.write_bytes(b"xmlns").ok();
            }
            buf.write_bytes(b"=").ok();
            if let Some(mut buf) = buf.buffer {
                buf.push_quoted_cstr(&CString::new(cur.href.as_deref().unwrap()).unwrap())
                    .ok();
            }
        };
        if let Some(buf) = buf {
            write(buf);
        } else {
            write(&mut ctxt.as_mut().unwrap().buf);
        }
    }
}

/// This will dump the content of the notation table as an XML DTD definition
#[doc(alias = "xmlBufDumpNotationTable")]
unsafe fn xml_buf_dump_notation_table<'a>(
    buf: &mut (impl Write + 'a),
    table: &XmlHashTable<'_, XmlNotation>,
) {
    xml_dump_notation_table(buf, table);
}

/// This will dump the content of the element declaration as an XML DTD definition
#[doc(alias = "xmlBufDumpElementDecl")]
unsafe fn xml_buf_dump_element_decl<'a>(buf: &mut (impl Write + 'a), elem: XmlElementPtr) {
    unsafe {
        xml_dump_element_decl(buf, elem);
    }
}

/// This will dump the content of the attribute declaration as an XML DTD definition
#[doc(alias = "xmlBufDumpAttributeDecl")]
fn xml_buf_dump_attribute_decl<'a>(buf: &mut (impl Write + 'a), attr: XmlAttributePtr) {
    xml_dump_attribute_decl(buf, attr);
}

/// This will dump the content of the entity table as an XML DTD definition
#[doc(alias = "xmlBufDumpEntityDecl")]
fn xml_buf_dump_entity_decl<'a>(buf: &mut (impl Write + 'a), ent: XmlEntityPtr) {
    xml_dump_entity_decl(buf, ent);
}

/// Dump a list of local namespace definitions to a save context.
/// Should be called in the context of attribute dumps.
#[doc(alias = "xmlNsListDumpOutputCtxt")]
fn xml_ns_list_dump_output_ctxt(ctxt: &mut XmlSaveCtxt, mut cur: Option<XmlNsPtr>) {
    while let Some(now) = cur {
        xml_ns_dump_output(None, now, Some(ctxt));
        cur = now.next;
    }
}

/// Serialize the attribute in the buffer
#[doc(alias = "xmlAttrSerializeContent")]
fn xml_attr_serialize_content(buf: &mut XmlOutputBuffer, attr: XmlAttrPtr) {
    let mut children = attr.children();
    while let Some(now) = children {
        match now.element_type() {
            XmlElementType::XmlTextNode => {
                let now = XmlNodePtr::try_from(now).unwrap();
                let mut out = vec![];
                attr_serialize_text_content(
                    &mut out,
                    attr.doc,
                    Some(attr),
                    now.content.as_deref().unwrap(),
                );
                buf.write_bytes(&out).ok();
            }
            XmlElementType::XmlEntityRefNode => {
                let now = XmlNodePtr::try_from(now).unwrap();
                if let Some(mut buf) = buf.buffer {
                    buf.push_bytes(b"&").ok();
                    buf.push_bytes(now.name.as_bytes()).ok();
                    buf.push_bytes(b";").ok();
                }
            }
            _ => { /* should not happen unless we have a badly built tree */ }
        }
        children = now.next();
    }
}

/// Dump an XML attribute
#[doc(alias = "xmlAttrDumpOutput")]
fn xml_attr_dump_output(ctxt: &mut XmlSaveCtxt, cur: XmlAttrPtr) {
    if ctxt.format == 2 {
        ctxt.write_ws_non_sig(2);
    } else {
        ctxt.buf.write_bytes(b" ").ok();
    }
    if let Some(prefix) = cur.ns.as_deref().and_then(|ns| ns.prefix()) {
        ctxt.buf.write_str(&prefix).ok();
        ctxt.buf.write_bytes(b":").ok();
    }

    ctxt.buf.write_str(&cur.name().unwrap()).ok();
    ctxt.buf.write_bytes(b"=\"").ok();
    xml_attr_serialize_content(&mut ctxt.buf, cur);
    ctxt.buf.write_bytes(b"\"").ok();
}

/// Dump an XML node, recursive behaviour, children are printed too.
#[doc(alias = "xmlNodeDumpOutputInternal")]
pub(crate) unsafe fn xml_node_dump_output_internal(
    ctxt: &mut XmlSaveCtxt,
    mut cur: XmlGenericNodePtr,
) {
    unsafe {
        let format: i32 = ctxt.format;
        let mut unformatted_node = None;

        let root = cur;
        let mut parent = cur.parent();
        loop {
            match cur.element_type() {
                XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                    let doc = XmlDocPtr::try_from(cur).unwrap();
                    ctxt.doc_content_dump_output(doc);
                }
                XmlElementType::XmlDTDNode => {
                    let dtd = XmlDtdPtr::try_from(cur).unwrap();
                    xml_dtd_dump_output(ctxt, dtd);
                }
                XmlElementType::XmlDocumentFragNode => {
                    // Always validate cur.parent when descending.
                    if let Some(children) = cur.children().filter(|_| cur.parent() == parent) {
                        parent = Some(cur);
                        cur = children;
                        continue;
                    }
                }
                XmlElementType::XmlElementDecl => {
                    let elem_decl = XmlElementPtr::try_from(cur).unwrap();
                    xml_buf_dump_element_decl(&mut ctxt.buf, elem_decl);
                }
                XmlElementType::XmlAttributeDecl => {
                    let attr_decl = XmlAttributePtr::try_from(cur).unwrap();
                    xml_buf_dump_attribute_decl(&mut ctxt.buf, attr_decl);
                }
                XmlElementType::XmlEntityDecl => {
                    let ent = XmlEntityPtr::try_from(cur).unwrap();
                    xml_buf_dump_entity_decl(&mut ctxt.buf, ent);
                }
                XmlElementType::XmlElementNode => {
                    let node = XmlNodePtr::try_from(cur).unwrap();
                    if cur != root && ctxt.format == 1 && get_indent_tree_output() != 0 {
                        let len = ctxt.indent_size
                            * if ctxt.level > ctxt.indent_nr as i32 {
                                ctxt.indent_nr
                            } else {
                                ctxt.level as usize
                            };
                        ctxt.buf.write_bytes(&ctxt.indent[..len]).ok();
                    }

                    // Some users like lxml are known to pass nodes with a corrupted
                    // tree structure. Fall back to a recursive call to handle this case.
                    if cur.parent() != parent && cur.children().is_some() {
                        xml_node_dump_output_internal(ctxt, cur);
                    } else {
                        ctxt.buf.write_bytes(b"<").ok();
                        if let Some(prefix) = node.ns.as_deref().and_then(|ns| ns.prefix()) {
                            ctxt.buf.write_str(&prefix).ok();
                            ctxt.buf.write_bytes(b":").ok();
                        }
                        ctxt.buf.write_str(&node.name).ok();
                        if let Some(ns_def) = node.ns_def {
                            xml_ns_list_dump_output_ctxt(ctxt, Some(ns_def));
                        }
                        let mut attr = node.properties;
                        while let Some(now) = attr {
                            xml_attr_dump_output(ctxt, now);
                            attr = now.next;
                        }

                        if let Some(children) = cur.children() {
                            if ctxt.format == 1 {
                                let mut tmp = Some(children);
                                while let Some(now) = tmp {
                                    if matches!(
                                        now.element_type(),
                                        XmlElementType::XmlTextNode
                                            | XmlElementType::XmlCDATASectionNode
                                            | XmlElementType::XmlEntityRefNode
                                    ) {
                                        ctxt.format = 0;
                                        unformatted_node = Some(cur);
                                        break;
                                    }
                                    tmp = now.next();
                                }
                            }
                            if ctxt.format == 2 {
                                ctxt.write_ws_non_sig(1);
                            }
                            ctxt.buf.write_bytes(b">").ok();
                            if ctxt.format == 1 {
                                ctxt.buf.write_bytes(b"\n").ok();
                            }
                            if ctxt.level >= 0 {
                                ctxt.level += 1;
                            }
                            parent = Some(cur);
                            cur = children;
                            continue;
                        } else if ctxt.options & XmlSaveOption::XmlSaveNoEmpty as i32 == 0 {
                            if ctxt.format == 2 {
                                ctxt.write_ws_non_sig(0);
                            }
                            ctxt.buf.write_bytes(b"/>").ok();
                        } else {
                            if ctxt.format == 2 {
                                ctxt.write_ws_non_sig(1);
                            }
                            ctxt.buf.write_bytes(b"></").ok();
                            if let Some(prefix) = node.ns.as_deref().and_then(|ns| ns.prefix()) {
                                ctxt.buf.write_str(&prefix).ok();
                                ctxt.buf.write_bytes(b":").ok();
                            }

                            ctxt.buf.write_str(&node.name).ok();
                            if ctxt.format == 2 {
                                ctxt.write_ws_non_sig(0);
                            }
                            ctxt.buf.write_bytes(b">").ok();
                        }
                    }
                }
                XmlElementType::XmlTextNode => {
                    let node = XmlNodePtr::try_from(cur).unwrap();
                    if let Some(content) = node.content.as_deref() {
                        if node.name != XML_STRING_TEXT_NOENC {
                            ctxt.buf.write_str_with_escape(content, ctxt.escape).ok();
                        } else {
                            // Disable escaping, needed for XSLT
                            ctxt.buf.write_str(content).ok();
                        }
                    }
                }
                XmlElementType::XmlPINode => {
                    let node = XmlNodePtr::try_from(cur).unwrap();
                    if cur != root && ctxt.format == 1 && get_indent_tree_output() != 0 {
                        let len = ctxt.indent_size
                            * if ctxt.level > ctxt.indent_nr as i32 {
                                ctxt.indent_nr
                            } else {
                                ctxt.level as usize
                            };
                        ctxt.buf.write_bytes(&ctxt.indent[..len]).ok();
                    }

                    if let Some(content) = node.content.as_deref() {
                        ctxt.buf.write_bytes(b"<?").ok();
                        ctxt.buf.write_str(&node.name).ok();
                        if ctxt.format == 2 {
                            ctxt.write_ws_non_sig(0);
                        } else {
                            ctxt.buf.write_bytes(b" ").ok();
                        }

                        ctxt.buf.write_str(content).ok();
                        ctxt.buf.write_bytes(b"?>").ok();
                    } else {
                        ctxt.buf.write_bytes(b"<?").ok();
                        ctxt.buf.write_str(&node.name).ok();
                        if ctxt.format == 2 {
                            ctxt.write_ws_non_sig(0);
                        }
                        ctxt.buf.write_bytes(b"?>").ok();
                    }
                }
                XmlElementType::XmlCommentNode => {
                    let node = XmlNodePtr::try_from(cur).unwrap();
                    if cur != root && ctxt.format == 1 && get_indent_tree_output() != 0 {
                        let len = ctxt.indent_size
                            * if ctxt.level > ctxt.indent_nr as i32 {
                                ctxt.indent_nr
                            } else {
                                ctxt.level as usize
                            };
                        ctxt.buf.write_bytes(&ctxt.indent[..len]).ok();
                    }

                    if let Some(content) = node.content.as_deref() {
                        ctxt.buf.write_bytes(b"<!--").ok();
                        ctxt.buf.write_str(content).ok();
                        ctxt.buf.write_bytes(b"-->").ok();
                    }
                }
                XmlElementType::XmlEntityRefNode => {
                    let node = XmlNodePtr::try_from(cur).unwrap();
                    ctxt.buf.write_bytes(b"&").ok();
                    ctxt.buf.write_str(&node.name).ok();
                    ctxt.buf.write_bytes(b";").ok();
                }
                XmlElementType::XmlCDATASectionNode => {
                    let node = XmlNodePtr::try_from(cur).unwrap();
                    if let Some(content) = node.content.as_deref().filter(|cont| !cont.is_empty()) {
                        let mut start = content;
                        let mut end = content;
                        while !end.is_empty() {
                            if let Some(rem) = end.strip_prefix("]]>") {
                                let len = start.len() - rem.len();
                                ctxt.buf.write_bytes(b"<![CDATA[").ok();
                                ctxt.buf.write_str(&start[..len]).ok();
                                ctxt.buf.write_bytes(b"]]>").ok();
                                end = rem;
                                start = rem;
                            }
                            let mut next = end.chars();
                            next.next();
                            end = next.as_str();
                        }
                        if !start.is_empty() {
                            ctxt.buf.write_bytes(b"<![CDATA[").ok();
                            ctxt.buf.write_str(start).ok();
                            ctxt.buf.write_bytes(b"]]>").ok();
                        }
                    } else {
                        ctxt.buf.write_bytes(b"<![CDATA[]]>").ok();
                    }
                }
                XmlElementType::XmlAttributeNode => {
                    let attr = XmlAttrPtr::try_from(cur).unwrap();
                    xml_attr_dump_output(ctxt, attr);
                }
                XmlElementType::XmlNamespaceDecl => {
                    let ns = XmlNsPtr::try_from(cur).unwrap();
                    xml_ns_dump_output_ctxt(ctxt, ns);
                }
                _ => {}
            }

            loop {
                if cur == root {
                    return;
                }
                if ctxt.format == 1
                    && !matches!(
                        cur.element_type(),
                        XmlElementType::XmlXIncludeStart | XmlElementType::XmlXIncludeEnd
                    )
                {
                    ctxt.buf.write_bytes(b"\n").ok();
                }
                if let Some(next) = cur.next() {
                    cur = next;
                    break;
                }

                cur = parent.unwrap();
                // cur.parent was validated when descending.
                parent = cur.parent();

                if matches!(cur.element_type(), XmlElementType::XmlElementNode) {
                    let cur = XmlNodePtr::try_from(cur).unwrap();
                    if ctxt.level > 0 {
                        ctxt.level -= 1;
                    }
                    if get_indent_tree_output() != 0 && ctxt.format == 1 {
                        let len = ctxt.indent_size
                            * if ctxt.level > ctxt.indent_nr as i32 {
                                ctxt.indent_nr
                            } else {
                                ctxt.level as usize
                            };
                        ctxt.buf.write_bytes(&ctxt.indent[..len]).ok();
                    }

                    ctxt.buf.write_bytes(b"</").ok();
                    if let Some(prefix) = cur.ns.as_deref().and_then(|ns| ns.prefix()) {
                        ctxt.buf.write_str(&prefix).ok();
                        ctxt.buf.write_bytes(b":").ok();
                    }
                    ctxt.buf.write_str(&cur.name).ok();
                    if ctxt.format == 2 {
                        ctxt.write_ws_non_sig(0);
                    }
                    ctxt.buf.write_bytes(b">").ok();

                    if Some(cur.into()) == unformatted_node {
                        ctxt.format = format;
                        unformatted_node = None;
                    }
                }
            }
        }
    }
}

/// Dump the XML document DTD, if any.
#[doc(alias = "xmlDtdDumpOutput")]
unsafe fn xml_dtd_dump_output(ctxt: &mut XmlSaveCtxt, dtd: XmlDtdPtr) {
    unsafe {
        ctxt.buf.write_bytes(b"<!DOCTYPE ").ok();
        ctxt.buf.write_str(dtd.name.as_deref().unwrap()).ok();
        if let Some(external_id) = dtd.external_id.as_deref() {
            ctxt.buf.write_bytes(b" PUBLIC ").ok();
            if let Some(mut buf) = ctxt.buf.buffer {
                let external_id = CString::new(external_id).unwrap();
                buf.push_quoted_cstr(external_id.as_c_str()).ok();
            }
            ctxt.buf.write_bytes(b" ").ok();
            if let Some(mut buf) = ctxt.buf.buffer {
                let system_id = CString::new(dtd.system_id.as_deref().unwrap()).unwrap();
                buf.push_quoted_cstr(&system_id).ok();
            }
        } else if let Some(system_id) = dtd.system_id.as_deref() {
            ctxt.buf.write_bytes(b" SYSTEM ").ok();
            if let Some(mut buf) = ctxt.buf.buffer {
                let system_id = CString::new(system_id).unwrap();
                buf.push_quoted_cstr(&system_id).ok();
            }
        }
        if dtd.entities.is_none()
            && dtd.elements.is_none()
            && dtd.attributes.is_none()
            && dtd.notations.is_none()
            && dtd.pentities.is_none()
        {
            ctxt.buf.write_bytes(b">").ok();
            return;
        }
        ctxt.buf.write_bytes(b" [\n").ok();
        // Dump the notations first they are not in the DTD children list
        // Do this only on a standalone DTD or on the internal subset though.
        if dtd.doc.is_none_or(|doc| doc.int_subset == Some(dtd)) {
            if let Some(table) = dtd.notations.as_deref() {
                xml_buf_dump_notation_table(&mut ctxt.buf, table);
            }
        }
        let format: i32 = ctxt.format;
        let level: i32 = ctxt.level;
        ctxt.format = 0;
        ctxt.level = -1;
        let mut cur = dtd.children;
        while let Some(now) = cur {
            xml_node_dump_output_internal(ctxt, now);
            cur = now.next();
        }
        ctxt.format = format;
        ctxt.level = level;
        ctxt.buf.write_bytes(b"]>").ok();
    }
}

/// Dump a local Namespace definition to a save context.
/// Should be called in the context of attribute dumps.
#[doc(alias = "xmlNsDumpOutputCtxt")]
fn xml_ns_dump_output_ctxt(ctxt: &mut XmlSaveCtxt, cur: XmlNsPtr) {
    xml_ns_dump_output(None, cur, Some(ctxt));
}

/// Dump a list of XML attributes
#[doc(alias = "xhtmlAttrListDumpOutput")]
#[cfg(feature = "html")]
unsafe fn xhtml_attr_list_dump_output(ctxt: &mut XmlSaveCtxt, mut cur: Option<XmlAttrPtr>) {
    unsafe {
        use crate::{
            html::tree::html_is_boolean_attr,
            tree::{xml_free_node, xml_new_doc_text},
        };

        if cur.is_none() {
            return;
        }

        let mut xml_lang = None;
        let mut lang = None;
        let mut name = None;
        let mut id = None;
        let parent = cur.unwrap().parent();
        while let Some(mut now) = cur {
            if now.ns.is_none() && now.name().as_deref() == Some("id") {
                id = cur;
            } else if now.ns.is_none() && now.name().as_deref() == Some("name") {
                name = cur;
            } else if now.ns.is_none() && now.name().as_deref() == Some("lang") {
                lang = cur;
            } else if now.name().as_deref() == Some("lang")
                && now
                    .ns
                    .is_some_and(|ns| ns.prefix().as_deref() == Some("xml"))
            {
                xml_lang = cur;
            } else if now.ns.is_none()
                && now
                    .children()
                    .map(|children| XmlNodePtr::try_from(children).unwrap())
                    .is_none_or(|c| c.content.as_deref().is_none_or(|cont| cont.is_empty()))
                && html_is_boolean_attr(now.name().as_deref().unwrap())
            {
                if let Some(children) = now.children() {
                    xml_free_node(children);
                }
                let doc = now.doc;
                let name = now.name();
                let new_text = xml_new_doc_text(doc, name.as_deref()).map(|node| node.into());
                now.set_children(new_text);
                if let Some(mut children) = now.children() {
                    children.set_parent(Some(now.into()));
                }
            }
            xml_attr_dump_output(ctxt, now);
            cur = now.next;
        }
        // C.8
        if let Some(name) = name.filter(|_| {
            id.is_none()
                && parent
                    .as_deref()
                    .and_then(|parent| parent.name())
                    .filter(|name| {
                        name == "a"
                            || name == "p"
                            || name == "div"
                            || name == "img"
                            || name == "map"
                            || name == "applet"
                            || name == "form"
                            || name == "frame"
                            || name == "iframe"
                    })
                    .is_some()
        }) {
            ctxt.buf.write_bytes(b" id=\"").ok();
            xml_attr_serialize_content(&mut ctxt.buf, name);
            ctxt.buf.write_bytes(b"\"").ok();
        }
        // C.7.
        match (lang, xml_lang) {
            (Some(lang), None) => {
                ctxt.buf.write_bytes(b" xml:lang=\"").ok();
                xml_attr_serialize_content(&mut ctxt.buf, lang);
                ctxt.buf.write_bytes(b"\"").ok();
            }
            (None, Some(xml_lang)) => {
                ctxt.buf.write_bytes(b" lang=\"").ok();
                xml_attr_serialize_content(&mut ctxt.buf, xml_lang);
                ctxt.buf.write_bytes(b"\"").ok();
            }
            _ => {}
        }
    }
}

const XHTML_NS_NAME: &str = "http://www.w3.org/1999/xhtml";

/// Check if a node is an empty xhtml node
///
/// Returns `true` if the node is an empty node, `false`.
#[doc(alias = "xhtmlIsEmpty")]
#[cfg(feature = "html")]
fn xhtml_is_empty(node: &XmlNode) -> bool {
    if !matches!(node.element_type(), XmlElementType::XmlElementNode) {
        return false;
    }
    if node
        .ns
        .is_some_and(|ns| ns.href().as_deref() != Some(XHTML_NS_NAME))
    {
        return false;
    }
    if node.children().is_some() {
        return false;
    }
    match node.name.as_bytes().first() {
        Some(&b'a') => node.name().as_deref() == Some("area"),
        Some(&b'b') => {
            node.name().as_deref() == Some("br")
                || node.name().as_deref() == Some("base")
                || node.name().as_deref() == Some("basefont")
        }
        Some(&b'c') => node.name().as_deref() == Some("col"),
        Some(&b'f') => node.name().as_deref() == Some("frame"),
        Some(&b'h') => node.name().as_deref() == Some("hr"),
        Some(&b'i') => {
            node.name().as_deref() == Some("img")
                || node.name().as_deref() == Some("input")
                || node.name().as_deref() == Some("isindex")
        }
        Some(&b'l') => node.name().as_deref() == Some("link"),
        Some(&b'm') => node.name().as_deref() == Some("meta"),
        Some(&b'p') => node.name().as_deref() == Some("param"),
        _ => false,
    }
}

/// Dump an XHTML node, recursive behaviour, children are printed too.
#[doc(alias = "xhtmlNodeDumpOutput")]
#[cfg(feature = "html")]
pub(crate) unsafe fn xhtml_node_dump_output(ctxt: &mut XmlSaveCtxt, mut cur: XmlGenericNodePtr) {
    use crate::parser::XML_STRING_TEXT;

    unsafe {
        use crate::tree::{XmlEntityPtr, XmlNodePtr};

        let format: i32 = ctxt.format;
        let mut unformatted_node = None;

        let root = cur;
        let mut parent = cur.parent();
        loop {
            match cur.element_type() {
                XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                    let doc = XmlDocPtr::try_from(cur).unwrap();
                    ctxt.doc_content_dump_output(doc);
                }
                XmlElementType::XmlNamespaceDecl => {
                    let ns = XmlNsPtr::try_from(cur).unwrap();
                    xml_ns_dump_output_ctxt(ctxt, ns);
                }
                XmlElementType::XmlDTDNode => {
                    let dtd = XmlDtdPtr::try_from(cur).unwrap();
                    xml_dtd_dump_output(ctxt, dtd);
                }
                XmlElementType::XmlDocumentFragNode => {
                    // Always validate cur.parent when descending.
                    if let Some(children) = cur.children().filter(|_| cur.parent() == parent) {
                        parent = Some(cur);
                        cur = children;
                        continue;
                    }
                }
                XmlElementType::XmlElementDecl => {
                    let elem_decl = XmlElementPtr::try_from(cur).unwrap();
                    xml_buf_dump_element_decl(&mut ctxt.buf, elem_decl);
                }
                XmlElementType::XmlAttributeDecl => {
                    let attr_decl = XmlAttributePtr::try_from(cur).unwrap();
                    xml_buf_dump_attribute_decl(&mut ctxt.buf, attr_decl);
                }
                XmlElementType::XmlEntityDecl => {
                    let ent = XmlEntityPtr::try_from(cur).unwrap();
                    xml_buf_dump_entity_decl(&mut ctxt.buf, ent);
                }
                XmlElementType::XmlElementNode => {
                    let node = XmlNodePtr::try_from(cur).unwrap();
                    let mut addmeta = 0;

                    if cur != root && ctxt.format == 1 && get_indent_tree_output() != 0 {
                        let len = ctxt.indent_size
                            * if ctxt.level > ctxt.indent_nr as i32 {
                                ctxt.indent_nr
                            } else {
                                ctxt.level as usize
                            };
                        ctxt.buf.write_bytes(&ctxt.indent[..len]).ok();
                    }

                    // Some users like lxml are known to pass nodes with a corrupted
                    // tree structure. Fall back to a recursive call to handle this case.
                    if cur.parent() != parent && cur.children().is_some() {
                        xhtml_node_dump_output(ctxt, cur);
                        break;
                    }

                    ctxt.buf.write_bytes(b"<").ok();
                    if let Some(prefix) = node.ns.as_deref().and_then(|ns| ns.prefix()) {
                        ctxt.buf.write_str(&prefix).ok();
                        ctxt.buf.write_bytes(b":").ok();
                    }
                    ctxt.buf.write_str(&node.name).ok();
                    if let Some(ns_def) = node.ns_def {
                        xml_ns_list_dump_output_ctxt(ctxt, Some(ns_def));
                    }
                    if cur.name().as_deref() == Some("html")
                        && node.ns.is_none()
                        && node.ns_def.is_none()
                    {
                        // 3.1.1. Strictly Conforming Documents A.3.1.1 3/

                        ctxt.buf
                            .write_str(" xmlns=\"http://www.w3.org/1999/xhtml\"")
                            .ok();
                    }
                    if node.properties.is_some() {
                        xhtml_attr_list_dump_output(ctxt, node.properties);
                    }

                    if parent.is_some_and(|parent| {
                        parent.parent() == node.doc.map(|doc| doc.into())
                            && (*parent).name().as_deref() == Some("html")
                    }) && cur.name().as_deref() == Some("head")
                    {
                        let mut tmp = cur.children();
                        while let Some(now) = tmp {
                            if now.name().as_deref() == Some("meta") {
                                if let Some(httpequiv) = now.get_prop("http-equiv") {
                                    if httpequiv.eq_ignore_ascii_case("Content-Type") {
                                        break;
                                    }
                                }
                            }
                            tmp = now.next();
                        }
                        if tmp.is_none() {
                            addmeta = 1;
                        }
                    }

                    if let Some(children) = cur.children() {
                        ctxt.buf.write_bytes(b">").ok();
                        if addmeta == 1 {
                            if ctxt.format == 1 {
                                ctxt.buf.write_bytes(b"\n").ok();
                                if get_indent_tree_output() != 0 {
                                    let len = ctxt.indent_size
                                        * if ctxt.level + 1 > ctxt.indent_nr as i32 {
                                            ctxt.indent_nr
                                        } else {
                                            (ctxt.level + 1) as usize
                                        };
                                    ctxt.buf.write_bytes(&ctxt.indent[..len]).ok();
                                }
                            }

                            ctxt.buf.write_str(
                                "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=",
                            ).ok();
                            if let Some(encoding) = ctxt.encoding.as_deref() {
                                ctxt.buf.write_str(encoding).ok();
                            } else {
                                ctxt.buf.write_bytes(b"UTF-8").ok();
                            }
                            ctxt.buf.write_bytes(b"\" />").ok();
                        }

                        if ctxt.format == 1 {
                            let mut tmp = Some(children);
                            while let Some(now) = tmp {
                                if matches!(now.element_type(), XmlElementType::XmlTextNode)
                                    || matches!(
                                        now.element_type(),
                                        XmlElementType::XmlEntityRefNode
                                    )
                                {
                                    unformatted_node = Some(cur);
                                    ctxt.format = 0;
                                    break;
                                }
                                tmp = now.next();
                            }
                        }

                        if ctxt.format == 1 {
                            ctxt.buf.write_bytes(b"\n").ok();
                        }
                        if ctxt.level >= 0 {
                            ctxt.level += 1;
                        }
                        parent = Some(cur);
                        cur = children;
                        continue;
                    } else if node.ns.is_none_or(|ns| ns.prefix().is_none())
                        && xhtml_is_empty(&node)
                        && addmeta == 0
                    {
                        // C.2. Empty Elements
                        ctxt.buf.write_bytes(b" />").ok();
                    } else {
                        if addmeta == 1 {
                            ctxt.buf.write_bytes(b">").ok();
                            if ctxt.format == 1 {
                                ctxt.buf.write_bytes(b"\n").ok();
                                if get_indent_tree_output() != 0 {
                                    let len = ctxt.indent_size
                                        * if ctxt.level + 1 > ctxt.indent_nr as i32 {
                                            ctxt.indent_nr
                                        } else {
                                            (ctxt.level + 1) as usize
                                        };
                                    ctxt.buf.write_bytes(&ctxt.indent[..len]).ok();
                                }
                            }

                            ctxt.buf.write_str(
                                "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=",
                            ).ok();
                            if let Some(encoding) = ctxt.encoding.as_deref() {
                                ctxt.buf.write_str(encoding).ok();
                            } else {
                                ctxt.buf.write_bytes(b"UTF-8").ok();
                            }
                            ctxt.buf.write_bytes(b"\" />").ok();
                            if ctxt.format == 1 {
                                ctxt.buf.write_bytes(b"\n").ok();
                            }
                        } else {
                            ctxt.buf.write_bytes(b">").ok();
                        }
                        // C.3. Element Minimization and Empty Element Content
                        ctxt.buf.write_bytes(b"</").ok();
                        if let Some(prefix) = node.ns.as_deref().and_then(|ns| ns.prefix()) {
                            ctxt.buf.write_str(&prefix).ok();
                            ctxt.buf.write_bytes(b":").ok();
                        }
                        ctxt.buf.write_str(&node.name).ok();
                        ctxt.buf.write_bytes(b">").ok();
                    }
                }
                XmlElementType::XmlTextNode => {
                    let node = XmlNodePtr::try_from(cur).unwrap();
                    let Some(content) = node.content.as_deref() else {
                        break;
                    };
                    if node.name == XML_STRING_TEXT || node.name != XML_STRING_TEXT_NOENC {
                        ctxt.buf.write_str_with_escape(content, ctxt.escape).ok();
                    } else {
                        // Disable escaping, needed for XSLT

                        ctxt.buf.write_str(content).ok();
                    }
                }
                XmlElementType::XmlPINode => {
                    let node = XmlNodePtr::try_from(cur).unwrap();
                    if let Some(content) = node.content.as_deref() {
                        ctxt.buf.write_bytes(b"<?").ok();
                        ctxt.buf.write_str(&node.name).ok();
                        ctxt.buf.write_bytes(b" ").ok();
                        ctxt.buf.write_str(content).ok();
                        ctxt.buf.write_bytes(b"?>").ok();
                    } else {
                        ctxt.buf.write_bytes(b"<?").ok();
                        ctxt.buf.write_str(&node.name).ok();
                        ctxt.buf.write_bytes(b"?>").ok();
                    }
                }
                XmlElementType::XmlCommentNode => {
                    let node = XmlNodePtr::try_from(cur).unwrap();
                    if let Some(content) = node.content.as_deref() {
                        ctxt.buf.write_bytes(b"<!--").ok();
                        ctxt.buf.write_str(content).ok();
                        ctxt.buf.write_bytes(b"-->").ok();
                    }
                }
                XmlElementType::XmlEntityRefNode => {
                    let node = XmlNodePtr::try_from(cur).unwrap();
                    ctxt.buf.write_bytes(b"&").ok();
                    ctxt.buf.write_str(&node.name).ok();
                    ctxt.buf.write_bytes(b";").ok();
                }
                XmlElementType::XmlCDATASectionNode => {
                    let node = XmlNodePtr::try_from(cur).unwrap();
                    if let Some(content) = node.content.as_deref().filter(|cont| !cont.is_empty()) {
                        let mut start = content;
                        let mut end = content;
                        while !end.is_empty() {
                            if let Some(rem) = end.strip_prefix("]]>") {
                                let len = start.len() - rem.len();
                                ctxt.buf.write_bytes(b"<![CDATA[").ok();
                                ctxt.buf.write_str(&start[..len]).ok();
                                ctxt.buf.write_bytes(b"]]>").ok();
                                start = rem;
                                end = rem;
                            }
                            end = &end[1..];
                        }
                        if !start.is_empty() {
                            ctxt.buf.write_bytes(b"<![CDATA[").ok();
                            ctxt.buf.write_str(start).ok();
                            ctxt.buf.write_bytes(b"]]>").ok();
                        }
                    } else {
                        ctxt.buf.write_bytes(b"<![CDATA[]]>").ok();
                    }
                }
                XmlElementType::XmlAttributeNode => {
                    let attr = XmlAttrPtr::try_from(cur).unwrap();
                    xml_attr_dump_output(ctxt, attr);
                }
                _ => {}
            }

            loop {
                if cur == root {
                    return;
                }
                if ctxt.format == 1 {
                    ctxt.buf.write_bytes(b"\n").ok();
                }
                if let Some(next) = cur.next() {
                    cur = next;
                    break;
                }

                cur = parent.unwrap();
                // cur.parent was validated when descending.
                parent = cur.parent();

                if matches!(cur.element_type(), XmlElementType::XmlElementNode) {
                    let cur = XmlNodePtr::try_from(cur).unwrap();
                    if ctxt.level > 0 {
                        ctxt.level -= 1;
                    }
                    if get_indent_tree_output() != 0 && ctxt.format == 1 {
                        let len = ctxt.indent_size
                            * if ctxt.level > ctxt.indent_nr as i32 {
                                ctxt.indent_nr
                            } else {
                                ctxt.level as usize
                            };
                        ctxt.buf.write_bytes(&ctxt.indent[..len]).ok();
                    }

                    ctxt.buf.write_bytes(b"</").ok();
                    if let Some(prefix) = cur.ns.as_deref().and_then(|ns| ns.prefix()) {
                        ctxt.buf.write_str(&prefix).ok();
                        ctxt.buf.write_bytes(b":").ok();
                    }
                    ctxt.buf.write_str(&cur.name).ok();
                    ctxt.buf.write_bytes(b">").ok();

                    if Some(cur.into()) == unformatted_node {
                        ctxt.format = format;
                        unformatted_node = None;
                    }
                }
            }
        }
    }
}

/// Dump an HTML node, recursive behaviour, children are printed too.
#[doc(alias = "htmlNodeDumpOutputInternal")]
#[cfg(feature = "html")]
unsafe fn html_node_dump_output_internal(ctxt: &mut XmlSaveCtxt, cur: XmlGenericNodePtr) -> i32 {
    unsafe {
        use crate::html::tree::html_node_dump_format_output;

        let mut oldenc = None;
        let oldctxtenc = ctxt.encoding.clone();
        let mut encoding = ctxt.encoding.clone();
        let mut switched_encoding: i32 = 0;

        xml_init_parser();

        let doc = cur.document();
        if let Some(mut doc) = doc {
            oldenc = doc.encoding.clone();
            if let Some(encoding) = ctxt.encoding.as_deref() {
                doc.encoding = Some(encoding.to_owned());
            } else if let Some(enc) = doc.encoding.as_deref() {
                encoding = Some(enc.to_owned());
            }
        }

        if let (Some(encoding), Some(doc)) = (encoding.as_deref(), doc) {
            html_set_meta_encoding(doc, Some(encoding));
        } else if let Some(doc) = doc {
            encoding = html_get_meta_encoding(doc);
        }
        if encoding.is_none() {
            encoding = Some("HTML".to_owned());
        }
        if encoding.is_some()
            && oldctxtenc.is_none()
            && ctxt.buf.encoder.is_none()
            && ctxt.buf.conv.is_none()
        {
            if ctxt.switch_encoding(encoding.as_deref().unwrap()) < 0 {
                if let Some(mut doc) = doc {
                    doc.encoding = oldenc;
                }
                return -1;
            }
            switched_encoding = 1;
        }
        if ctxt.options & XmlSaveOption::XmlSaveFormat as i32 != 0 {
            html_node_dump_format_output(&mut ctxt.buf, doc, Some(cur), encoding.as_deref(), 1);
        } else {
            html_node_dump_format_output(&mut ctxt.buf, doc, Some(cur), encoding.as_deref(), 0);
        }
        // Restore the state of the saving context at the end of the document
        if switched_encoding != 0 && oldctxtenc.is_none() {
            ctxt.clear_encoding();
        }
        if let Some(mut doc) = doc {
            doc.encoding = oldenc;
        }
        0
    }
}

/// Serialize text attribute values to an xmlBufPtr
#[doc(alias = "xmlBufAttrSerializeTxtContent")]
pub(crate) fn attr_serialize_text_content<'a>(
    buf: &mut (impl Write + 'a),
    doc: Option<XmlDocPtr>,
    attr: Option<XmlAttrPtr>,
    string: &str,
) {
    let mut base = string;
    let mut cur = base;
    while !cur.is_empty() {
        if let Some(rem) = cur.strip_prefix(['\n', '\r', '\t']) {
            if base.len() != cur.len() {
                write!(buf, "{}", &base[..base.len() - cur.len()]).ok();
            }
            write!(buf, "&#{};", cur.as_bytes()[0]).ok();
            cur = rem;
            base = rem;
        } else if let Some(rem) = cur.strip_prefix(['"', '<', '>', '&']) {
            if base.len() != cur.len() {
                write!(buf, "{}", &base[..base.len() - cur.len()]).ok();
            }
            match cur.as_bytes()[0] {
                b'"' => write!(buf, "&quot;").ok(),
                b'<' => write!(buf, "&lt;").ok(),
                b'>' => write!(buf, "&gt;").ok(),
                b'&' => write!(buf, "&amp;").ok(),
                _ => None,
            };
            cur = rem;
            base = rem;
        } else if cur.len() > 1
            && cur.as_bytes()[0] >= 0x80
            && doc.is_none_or(|doc| doc.encoding.is_none())
        {
            // We assume we have UTF-8 content.
            let mut tmp = [0; 12];

            if base.len() != cur.len() {
                write!(buf, "{}", &base[..base.len() - cur.len()]).ok();
            }
            if cur.as_bytes()[0] < 0xC0 {
                xml_save_err(
                    XmlParserErrors::XmlSaveNotUTF8 as _,
                    attr.map(|attr| attr.into()),
                    None,
                );
                buf.write_all(serialize_hex_charref(&mut tmp, cur.as_bytes()[0] as u32))
                    .ok();
                cur = &cur[1..];
                base = cur;
                continue;
            }
            let val = cur.chars().next().unwrap();
            if val.len_utf8() == 1 || !xml_is_char(val as u32) {
                xml_save_err(
                    XmlParserErrors::XmlSaveCharInvalid as _,
                    attr.map(|attr| attr.into()),
                    None,
                );
                buf.write_all(serialize_hex_charref(&mut tmp, cur.as_bytes()[0] as u32))
                    .ok();
                cur = &cur[1..];
                base = cur;
                continue;
            }
            // We could do multiple things here. Just save as a c_char ref
            buf.write_all(serialize_hex_charref(&mut tmp, val as u32))
                .ok();
            cur = &cur[val.len_utf8()..];
            base = cur;
        } else {
            let c = cur.chars().next().unwrap();
            cur = &cur[c.len_utf8()..];
        }
    }
    if base.len() != cur.len() {
        write!(buf, "{}", base).ok();
    }
}

/// Dump a list of local Namespace definitions.
/// Should be called in the context of attributes dumps.
#[doc(alias = "xmlNsListDumpOutput")]
pub(crate) fn xml_ns_list_dump_output(buf: &mut XmlOutputBuffer, mut cur: Option<XmlNsPtr>) {
    while let Some(now) = cur {
        xml_ns_dump_output(Some(buf), now, None);
        cur = now.next;
    }
}
