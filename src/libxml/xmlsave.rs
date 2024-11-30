//! Provide methods and data structures for serializing XML documents.  
//! This module is based on `libxml/xmlsave.h`, `xmlsave.c`, and so on in `libxml2-v2.11.8`.
//!
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
    cell::RefCell,
    ffi::{c_char, CStr, CString},
    io::Write,
    mem::size_of,
    os::raw::c_void,
    ptr::{null, null_mut},
    rc::Rc,
    slice::from_raw_parts,
    str::from_utf8_unchecked,
};

use libc::memset;

use crate::{
    buf::{libxml_api::xml_buf_set_allocation_scheme, XmlBufRef},
    encoding::{find_encoding_handler, XmlCharEncoding, XmlCharEncodingHandler},
    error::{XmlErrorDomain, XmlParserErrors, __xml_simple_error},
    globals::{get_indent_tree_output, GLOBAL_STATE},
    io::XmlOutputBuffer,
    libxml::{
        entities::{xml_dump_entity_decl, XmlEntityPtr},
        globals::{xml_free, xml_malloc},
        htmltree::{
            html_doc_content_dump_format_output, html_get_meta_encoding, html_set_meta_encoding,
        },
        parser::xml_init_parser,
        parser_internals::XML_STRING_TEXT_NOENC,
        valid::{
            xml_dump_attribute_decl, xml_dump_element_decl, xml_dump_notation_table,
            XmlNotationTablePtr,
        },
        xmlstring::{xml_str_equal, XmlChar},
    },
    tree::{
        is_xhtml, xml_buf_add, NodeCommon, NodePtr, XmlAttrPtr, XmlAttributePtr, XmlBufPtr,
        XmlBufferAllocationScheme, XmlDocPtr, XmlDtdPtr, XmlElementPtr, XmlElementType, XmlNodePtr,
        XmlNsPtr, XML_LOCAL_NAMESPACE,
    },
};

use super::chvalid::xml_is_char;

const MAX_INDENT: usize = 60;

/// This is the set of XML save options that can be passed down
/// to the xmlSaveToFd() and similar calls.
#[doc(alias = "xmlSaveOption")]
#[repr(C)]
pub enum XmlSaveOption {
    XmlSaveFormat = 1 << 0,   /* format save output */
    XmlSaveNoDecl = 1 << 1,   /* drop the xml declaration */
    XmlSaveNoEmpty = 1 << 2,  /* no empty tags */
    XmlSaveNoXHTML = 1 << 3,  /* disable XHTML1 specific rules */
    XmlSaveXHTML = 1 << 4,    /* force XHTML1 specific rules */
    XmlSaveAsXML = 1 << 5,    /* force XML serialization on HTML doc */
    XmlSaveAsHTML = 1 << 6,   /* force HTML serialization on XML doc */
    XmlSaveWsnonsig = 1 << 7, /* format with non-significant whitespace */
}

pub type XmlSaveCtxtPtr<'a> = *mut XmlSaveCtxt<'a>;
#[repr(C)]
pub struct XmlSaveCtxt<'a> {
    pub(crate) _private: *mut c_void,
    pub(crate) typ: i32,
    pub(crate) fd: i32,
    pub(crate) filename: *const XmlChar,
    pub(crate) encoding: Option<String>,
    pub(crate) handler: Option<Rc<RefCell<XmlCharEncodingHandler>>>,
    pub(crate) buf: Rc<RefCell<XmlOutputBuffer<'a>>>,
    pub(crate) options: i32,
    pub(crate) level: i32,
    pub(crate) format: i32,
    pub(crate) indent: [u8; MAX_INDENT + 1], /* array for indenting output */
    pub(crate) indent_nr: usize,
    pub(crate) indent_size: usize,
    pub(crate) escape: Option<fn(&str, &mut String) -> i32>, /* used for element content */
    pub(crate) escape_attr: Option<fn(&str, &mut String) -> i32>, /* used for attribute content */
}

impl Default for XmlSaveCtxt<'_> {
    fn default() -> Self {
        Self {
            _private: null_mut(),
            typ: 0,
            fd: 0,
            filename: null(),
            encoding: None,
            handler: None,
            buf: Rc::new(RefCell::new(XmlOutputBuffer::default())),
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

/// Handle an out of memory condition
#[doc(alias = "xmlSaveErr")]
pub(crate) unsafe extern "C" fn xml_save_err(
    code: XmlParserErrors,
    node: XmlNodePtr,
    extra: *const c_char,
) {
    let msg = match code {
        XmlParserErrors::XmlSaveNotUTF8 => c"string is not in UTF-8\n".as_ptr() as _,
        XmlParserErrors::XmlSaveCharInvalid => c"invalid character value\n".as_ptr() as _,
        XmlParserErrors::XmlSaveUnknownEncoding => c"unknown encoding %s\n".as_ptr() as _,
        XmlParserErrors::XmlSaveNoDoctype => c"document has no DOCTYPE\n".as_ptr() as _,
        _ => c"unexpected error number\n".as_ptr() as _,
    };
    __xml_simple_error(XmlErrorDomain::XmlFromOutput, code, node, msg, extra);
}

/// Handle an out of memory condition
#[doc(alias = "xmlSaveErrMemory")]
pub(crate) unsafe extern "C" fn xml_save_err_memory(extra: *const c_char) {
    __xml_simple_error(
        XmlErrorDomain::XmlFromOutput,
        XmlParserErrors::XmlErrNoMemory,
        null_mut(),
        null(),
        extra,
    );
}

/// # Panics
/// - If c is NULL character, `out.len() >= 5` must be satisfied.
/// - Otherwise, `out.len() >= 5 + (c as u32).ilog2() / 4` must be satisfied.
pub(crate) fn xml_serialize_hex_char_ref(out: &mut [u8], mut val: u32) -> &[u8] {
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
fn xml_escape_entities(src: &str, dst: &mut String) -> i32 {
    let mut out = [0; 13];
    for c in src.chars() {
        match c {
            '<' => dst.push_str("&lt;"),
            '>' => dst.push_str("&gt;"),
            '&' => dst.push_str("&amp;"),
            c @ ('\n' | '\t' | '\u{20}'..'\u{80}') => dst.push(c),
            c => {
                let out = xml_serialize_hex_char_ref(&mut out, c as u32);
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

/// Initialize a saving context
#[doc(alias = "xmlSaveCtxtInit")]
pub(crate) fn xml_save_ctxt_init(ctxt: &mut XmlSaveCtxt) {
    if ctxt.encoding.is_none() && ctxt.escape.is_none() {
        ctxt.escape = Some(xml_escape_entities);
    }
    GLOBAL_STATE.with_borrow(|state| {
        let len = state.tree_indent_string.len();
        if len == 0 {
            ctxt.indent.fill(0);
        } else {
            ctxt.indent_size = len;
            ctxt.indent_nr = MAX_INDENT / ctxt.indent_size;
            for chunk in ctxt.indent.chunks_exact_mut(ctxt.indent_size) {
                chunk.copy_from_slice(state.tree_indent_string.as_bytes());
            }
            ctxt.indent[ctxt.indent_nr * ctxt.indent_size] = 0;
        }

        if state.save_no_empty_tags != 0 {
            ctxt.options |= XmlSaveOption::XmlSaveNoEmpty as i32;
        }
    })
}

unsafe fn xml_save_switch_encoding(ctxt: &mut XmlSaveCtxt, encoding: &str) -> i32 {
    let mut buf = ctxt.buf.borrow_mut();

    if buf.encoder.is_none() && buf.conv.is_none() {
        buf.encoder = find_encoding_handler(encoding).map(|e| Rc::new(RefCell::new(e)));
        if buf.encoder.is_none() {
            let encoding = CString::new(encoding).unwrap();
            xml_save_err(
                XmlParserErrors::XmlSaveUnknownEncoding,
                null_mut(),
                encoding.as_ptr(),
            );
            return -1;
        }
        buf.conv = XmlBufRef::new();
        if buf.conv.is_none() {
            xml_save_err_memory(c"creating encoding buffer".as_ptr() as _);
            return -1;
        }
        /*
         * initialize the state, e.g. if outputting a BOM
         */
        buf.encode(true);
    }
    0
}

/// Write out formatting for non-significant whitespace output.
#[doc(alias = "xmlOutputBufferWriteWSNonSig")]
unsafe extern "C" fn xml_output_buffer_write_ws_non_sig(ctxt: &mut XmlSaveCtxt, extra: i32) {
    ctxt.buf.borrow_mut().write_bytes(b"\n");
    for i in (0..ctxt.level + extra).step_by(ctxt.indent_nr) {
        let len = ctxt.indent_size
            * if ctxt.level + extra - i > ctxt.indent_nr as i32 {
                ctxt.indent_nr
            } else {
                (ctxt.level + extra - i) as usize
            };
        ctxt.buf.borrow_mut().write_bytes(&ctxt.indent[..len]);
    }
}

/// Dump a local Namespace definition.
/// Should be called in the context of attributes dumps.
/// If @ctxt is supplied, @buf should be its buffer.
#[doc(alias = "xmlNsDumpOutput")]
pub(crate) unsafe extern "C" fn xml_ns_dump_output(
    buf: &mut XmlOutputBuffer,
    cur: XmlNsPtr,
    ctxt: XmlSaveCtxtPtr,
) {
    if cur.is_null() {
        return;
    }
    if matches!((*cur).element_type(), XML_LOCAL_NAMESPACE) && !(*cur).href.is_null() {
        if xml_str_equal((*cur).prefix, c"xml".as_ptr() as _) {
            return;
        }

        if !ctxt.is_null() && (*ctxt).format == 2 {
            xml_output_buffer_write_ws_non_sig(&mut *ctxt, 2);
        } else {
            buf.write_bytes(b" ");
        }

        /* Within the context of an element attributes */
        if !(*cur).prefix.is_null() {
            buf.write_bytes(b"xmlns:");
            buf.write_str(
                CStr::from_ptr((*cur).prefix as _)
                    .to_string_lossy()
                    .as_ref(),
            );
        } else {
            buf.write_bytes(b"xmlns");
        }
        buf.write_bytes(b"=");
        if let Some(mut buf) = buf.buffer {
            buf.push_quoted_cstr(CStr::from_ptr((*cur).href as *const i8));
        }
    }
}

/// This will dump the content of the notation table as an XML DTD definition
#[doc(alias = "xmlBufDumpNotationTable")]
unsafe extern "C" fn xml_buf_dump_notation_table(buf: XmlBufPtr, table: XmlNotationTablePtr) {
    xml_buf_set_allocation_scheme(buf, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);
    xml_dump_notation_table(buf, table);
}

/// This will dump the content of the element declaration as an XML DTD definition
#[doc(alias = "xmlBufDumpElementDecl")]
unsafe extern "C" fn xml_buf_dump_element_decl(buf: XmlBufPtr, elem: XmlElementPtr) {
    xml_buf_set_allocation_scheme(buf, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);
    xml_dump_element_decl(buf, elem);
}

/// This will dump the content of the attribute declaration as an XML DTD definition
#[doc(alias = "xmlBufDumpAttributeDecl")]
unsafe extern "C" fn xml_buf_dump_attribute_decl(buf: XmlBufPtr, attr: XmlAttributePtr) {
    xml_buf_set_allocation_scheme(buf, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);
    xml_dump_attribute_decl(buf, attr);
}

/// This will dump the content of the entity table as an XML DTD definition
#[doc(alias = "xmlBufDumpEntityDecl")]
unsafe extern "C" fn xml_buf_dump_entity_decl(buf: XmlBufPtr, ent: XmlEntityPtr) {
    xml_buf_set_allocation_scheme(buf, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);
    xml_dump_entity_decl(buf, ent);
}

/// Dump a list of local namespace definitions to a save context.
/// Should be called in the context of attribute dumps.
#[doc(alias = "xmlNsListDumpOutputCtxt")]
unsafe extern "C" fn xml_ns_list_dump_output_ctxt(ctxt: XmlSaveCtxtPtr, mut cur: XmlNsPtr) {
    let mut buf = (*ctxt).buf.borrow_mut();
    while !cur.is_null() {
        xml_ns_dump_output(&mut buf, cur, ctxt);
        cur = (*cur).next;
    }
}

/// Serialize the attribute in the buffer
#[doc(alias = "xmlAttrSerializeContent")]
unsafe extern "C" fn xml_attr_serialize_content(buf: &mut XmlOutputBuffer, attr: XmlAttrPtr) {
    let mut children = (*attr).children;
    while let Some(now) = children {
        match now.element_type() {
            XmlElementType::XmlTextNode => {
                xml_buf_attr_serialize_txt_content(
                    buf.buffer.map_or(null_mut(), |buf| buf.as_ptr()),
                    (*attr).doc,
                    attr,
                    now.content,
                );
            }
            XmlElementType::XmlEntityRefNode => {
                if let Some(mut buf) = buf.buffer {
                    buf.push_bytes(b"&");
                    buf.push_cstr(CStr::from_ptr(now.name as *const i8));
                    buf.push_bytes(b";");
                }
            }
            _ => { /* should not happen unless we have a badly built tree */ }
        }
        children = now.next;
    }
}

/// Dump an XML attribute
#[doc(alias = "xmlAttrDumpOutput")]
unsafe extern "C" fn xml_attr_dump_output(ctxt: XmlSaveCtxtPtr, cur: XmlAttrPtr) {
    if cur.is_null() {
        return;
    }
    let mut buf = (*ctxt).buf.borrow_mut();
    if (*ctxt).format == 2 {
        xml_output_buffer_write_ws_non_sig(&mut *ctxt, 2);
    } else {
        buf.write_bytes(b" ");
    }
    if !(*cur).ns.is_null() && !(*(*cur).ns).prefix.is_null() {
        buf.write_str(
            CStr::from_ptr((*(*cur).ns).prefix as _)
                .to_string_lossy()
                .as_ref(),
        );
        buf.write_bytes(b":");
    }

    buf.write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
    buf.write_bytes(b"=\"");
    xml_attr_serialize_content(&mut buf, cur);
    buf.write_bytes(b"\"");
}

/// Dump an XML node, recursive behaviour, children are printed too.
#[doc(alias = "xmlNodeDumpOutputInternal")]
pub(crate) unsafe extern "C" fn xml_node_dump_output_internal(
    ctxt: XmlSaveCtxtPtr,
    mut cur: XmlNodePtr,
) {
    let format: i32 = (*ctxt).format;
    let mut tmp: XmlNodePtr;

    let mut unformatted_node: XmlNodePtr = null_mut();
    let mut parent: XmlNodePtr;
    let mut attr: XmlAttrPtr;
    let mut start: *mut XmlChar;
    let mut end: *mut XmlChar;

    if cur.is_null() {
        return;
    }

    let root: XmlNodePtr = cur;
    parent = (*cur).parent.map_or(null_mut(), |p| p.as_ptr());
    loop {
        match (*cur).element_type() {
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                xml_doc_content_dump_output(ctxt, cur as _);
            }
            XmlElementType::XmlDTDNode => {
                xml_dtd_dump_output(ctxt, cur as _);
            }
            XmlElementType::XmlDocumentFragNode => {
                // Always validate (*cur).parent when descending.
                if let Some(children) = (*cur)
                    .children()
                    .filter(|_| (*cur).parent == NodePtr::from_ptr(parent))
                {
                    parent = cur;
                    cur = children.as_ptr();
                    continue;
                }
            }
            XmlElementType::XmlElementDecl => {
                xml_buf_dump_element_decl(
                    (*ctxt)
                        .buf
                        .borrow()
                        .buffer
                        .map_or(null_mut(), |buf| buf.as_ptr()),
                    cur as _,
                );
            }
            XmlElementType::XmlAttributeDecl => {
                xml_buf_dump_attribute_decl(
                    (*ctxt)
                        .buf
                        .borrow()
                        .buffer
                        .map_or(null_mut(), |buf| buf.as_ptr()),
                    cur as _,
                );
            }
            XmlElementType::XmlEntityDecl => {
                xml_buf_dump_entity_decl(
                    (*ctxt)
                        .buf
                        .borrow()
                        .buffer
                        .map_or(null_mut(), |buf| buf.as_ptr()),
                    cur as _,
                );
            }
            XmlElementType::XmlElementNode => {
                if cur != root && (*ctxt).format == 1 && get_indent_tree_output() != 0 {
                    let len = (*ctxt).indent_size
                        * if (*ctxt).level > (*ctxt).indent_nr as i32 {
                            (*ctxt).indent_nr
                        } else {
                            (*ctxt).level as usize
                        };
                    (*ctxt).buf.borrow_mut().write_bytes(&(*ctxt).indent[..len]);
                }

                // Some users like lxml are known to pass nodes with a corrupted
                // tree structure. Fall back to a recursive call to handle this case.
                if (*cur).parent != NodePtr::from_ptr(parent) && (*cur).children().is_some() {
                    xml_node_dump_output_internal(ctxt, cur);
                } else {
                    (*ctxt).buf.borrow_mut().write_bytes(b"<");
                    if !(*cur).ns.is_null() && !(*(*cur).ns).prefix.is_null() {
                        (*ctxt).buf.borrow_mut().write_str(
                            CStr::from_ptr((*(*cur).ns).prefix as _)
                                .to_string_lossy()
                                .as_ref(),
                        );
                        (*ctxt).buf.borrow_mut().write_bytes(b":");
                    }

                    (*ctxt)
                        .buf
                        .borrow_mut()
                        .write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
                    if !(*cur).ns_def.is_null() {
                        xml_ns_list_dump_output_ctxt(ctxt, (*cur).ns_def);
                    }
                    attr = (*cur).properties;
                    while !attr.is_null() {
                        xml_attr_dump_output(ctxt, attr);
                        attr = (*attr).next;
                    }

                    if let Some(children) = (*cur).children() {
                        if (*ctxt).format == 1 {
                            tmp = children.as_ptr();
                            while !tmp.is_null() {
                                if matches!(
                                    (*tmp).element_type(),
                                    XmlElementType::XmlTextNode
                                        | XmlElementType::XmlCDATASectionNode
                                        | XmlElementType::XmlEntityRefNode
                                ) {
                                    (*ctxt).format = 0;
                                    unformatted_node = cur;
                                    break;
                                }
                                tmp = (*tmp).next.map_or(null_mut(), |n| n.as_ptr());
                            }
                        }
                        if (*ctxt).format == 2 {
                            xml_output_buffer_write_ws_non_sig(&mut *ctxt, 1);
                        }
                        (*ctxt).buf.borrow_mut().write_bytes(b">");
                        if (*ctxt).format == 1 {
                            (*ctxt).buf.borrow_mut().write_bytes(b"\n");
                        }
                        if (*ctxt).level >= 0 {
                            (*ctxt).level += 1;
                        }
                        parent = cur;
                        cur = children.as_ptr();
                        continue;
                    } else if (*ctxt).options & XmlSaveOption::XmlSaveNoEmpty as i32 == 0 {
                        if (*ctxt).format == 2 {
                            xml_output_buffer_write_ws_non_sig(&mut *ctxt, 0);
                        }
                        (*ctxt).buf.borrow_mut().write_bytes(b"/>");
                    } else {
                        if (*ctxt).format == 2 {
                            xml_output_buffer_write_ws_non_sig(&mut *ctxt, 1);
                        }
                        (*ctxt).buf.borrow_mut().write_bytes(b"></");
                        if !(*cur).ns.is_null() && !(*(*cur).ns).prefix.is_null() {
                            (*ctxt).buf.borrow_mut().write_str(
                                CStr::from_ptr((*(*cur).ns).prefix as _)
                                    .to_string_lossy()
                                    .as_ref(),
                            );
                            (*ctxt).buf.borrow_mut().write_bytes(b":");
                        }

                        (*ctxt)
                            .buf
                            .borrow_mut()
                            .write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
                        if (*ctxt).format == 2 {
                            xml_output_buffer_write_ws_non_sig(&mut *ctxt, 0);
                        }
                        (*ctxt).buf.borrow_mut().write_bytes(b">");
                    }
                }
            }
            XmlElementType::XmlTextNode => {
                if !(*cur).content.is_null() {
                    if (*cur).name != XML_STRING_TEXT_NOENC.as_ptr() as _ {
                        (*ctxt).buf.borrow_mut().write_str_with_escape(
                            CStr::from_ptr((*cur).content as *const i8)
                                .to_string_lossy()
                                .as_ref(),
                            (*ctxt).escape,
                        );
                    } else {
                        /*
                         * Disable escaping, needed for XSLT
                         */

                        (*ctxt).buf.borrow_mut().write_str(
                            CStr::from_ptr((*cur).content as _)
                                .to_string_lossy()
                                .as_ref(),
                        );
                    }
                }
            }
            XmlElementType::XmlPINode => {
                if cur != root && (*ctxt).format == 1 && get_indent_tree_output() != 0 {
                    let len = (*ctxt).indent_size
                        * if (*ctxt).level > (*ctxt).indent_nr as i32 {
                            (*ctxt).indent_nr
                        } else {
                            (*ctxt).level as usize
                        };
                    (*ctxt).buf.borrow_mut().write_bytes(&(*ctxt).indent[..len]);
                }

                if !(*cur).content.is_null() {
                    (*ctxt).buf.borrow_mut().write_bytes(b"<?");

                    (*ctxt)
                        .buf
                        .borrow_mut()
                        .write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
                    if !(*cur).content.is_null() {
                        if (*ctxt).format == 2 {
                            xml_output_buffer_write_ws_non_sig(&mut *ctxt, 0);
                        } else {
                            (*ctxt).buf.borrow_mut().write_bytes(b" ");
                        }

                        (*ctxt).buf.borrow_mut().write_str(
                            CStr::from_ptr((*cur).content as _)
                                .to_string_lossy()
                                .as_ref(),
                        );
                    }
                    (*ctxt).buf.borrow_mut().write_bytes(b"?>");
                } else {
                    (*ctxt).buf.borrow_mut().write_bytes(b"<?");

                    (*ctxt)
                        .buf
                        .borrow_mut()
                        .write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
                    if (*ctxt).format == 2 {
                        xml_output_buffer_write_ws_non_sig(&mut *ctxt, 0);
                    }
                    (*ctxt).buf.borrow_mut().write_bytes(b"?>");
                }
            }
            XmlElementType::XmlCommentNode => {
                if cur != root && (*ctxt).format == 1 && get_indent_tree_output() != 0 {
                    let len = (*ctxt).indent_size
                        * if (*ctxt).level > (*ctxt).indent_nr as i32 {
                            (*ctxt).indent_nr
                        } else {
                            (*ctxt).level as usize
                        };
                    (*ctxt).buf.borrow_mut().write_bytes(&(*ctxt).indent[..len]);
                }

                if !(*cur).content.is_null() {
                    (*ctxt).buf.borrow_mut().write_bytes(b"<!--");
                    (*ctxt).buf.borrow_mut().write_str(
                        CStr::from_ptr((*cur).content as _)
                            .to_string_lossy()
                            .as_ref(),
                    );
                    (*ctxt).buf.borrow_mut().write_bytes(b"-->");
                }
            }
            XmlElementType::XmlEntityRefNode => {
                (*ctxt).buf.borrow_mut().write_bytes(b"&");
                (*ctxt)
                    .buf
                    .borrow_mut()
                    .write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
                (*ctxt).buf.borrow_mut().write_bytes(b";");
            }
            XmlElementType::XmlCDATASectionNode => {
                if (*cur).content.is_null() || *(*cur).content == b'\0' {
                    (*ctxt).buf.borrow_mut().write_bytes(b"<![CDATA[]]>");
                } else {
                    start = (*cur).content;
                    end = (*cur).content;
                    while *end != b'\0' {
                        if *end == b']' && *end.add(1) == b']' && *end.add(2) == b'>' {
                            end = end.add(2);
                            (*ctxt).buf.borrow_mut().write_bytes(b"<![CDATA[");
                            (*ctxt).buf.borrow_mut().write_bytes(from_raw_parts(
                                start as _,
                                end.offset_from(start) as _,
                            ));
                            (*ctxt).buf.borrow_mut().write_bytes(b"]]>");
                            start = end;
                        }
                        end = end.add(1);
                    }
                    if start != end {
                        (*ctxt).buf.borrow_mut().write_bytes(b"<![CDATA[");
                        (*ctxt)
                            .buf
                            .borrow_mut()
                            .write_str(CStr::from_ptr(start as _).to_string_lossy().as_ref());
                        (*ctxt).buf.borrow_mut().write_bytes(b"]]>");
                    }
                }
            }
            XmlElementType::XmlAttributeNode => {
                xml_attr_dump_output(ctxt, cur as _);
            }
            XmlElementType::XmlNamespaceDecl => {
                xml_ns_dump_output_ctxt(ctxt, cur as _);
            }
            _ => {}
        }

        loop {
            if cur == root {
                return;
            }
            if (*ctxt).format == 1
                && !matches!(
                    (*cur).element_type(),
                    XmlElementType::XmlXIncludeStart | XmlElementType::XmlXIncludeEnd
                )
            {
                (*ctxt).buf.borrow_mut().write_bytes(b"\n");
            }
            if let Some(next) = (*cur).next {
                cur = next.as_ptr();
                break;
            }

            cur = parent;
            /* (*cur).parent was validated when descending. */
            parent = (*cur).parent.map_or(null_mut(), |p| p.as_ptr());

            if matches!((*cur).element_type(), XmlElementType::XmlElementNode) {
                if (*ctxt).level > 0 {
                    (*ctxt).level -= 1;
                }
                if get_indent_tree_output() != 0 && (*ctxt).format == 1 {
                    let len = (*ctxt).indent_size
                        * if (*ctxt).level > (*ctxt).indent_nr as i32 {
                            (*ctxt).indent_nr
                        } else {
                            (*ctxt).level as usize
                        };
                    (*ctxt).buf.borrow_mut().write_bytes(&(*ctxt).indent[..len]);
                }

                (*ctxt).buf.borrow_mut().write_bytes(b"</");
                if !(*cur).ns.is_null() && !(*(*cur).ns).prefix.is_null() {
                    (*ctxt).buf.borrow_mut().write_str(
                        CStr::from_ptr((*(*cur).ns).prefix as _)
                            .to_string_lossy()
                            .as_ref(),
                    );
                    (*ctxt).buf.borrow_mut().write_bytes(b":");
                }

                (*ctxt)
                    .buf
                    .borrow_mut()
                    .write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
                if (*ctxt).format == 2 {
                    xml_output_buffer_write_ws_non_sig(&mut *ctxt, 0);
                }
                (*ctxt).buf.borrow_mut().write_bytes(b">");

                if cur == unformatted_node {
                    (*ctxt).format = format;
                    unformatted_node = null_mut();
                }
            }
        }
    }
}

/// Dump the XML document DTD, if any.
#[doc(alias = "xmlDtdDumpOutput")]
unsafe extern "C" fn xml_dtd_dump_output(ctxt: XmlSaveCtxtPtr, dtd: XmlDtdPtr) {
    let mut cur: XmlNodePtr;

    if dtd.is_null() {
        return;
    }
    if ctxt.is_null() {
        return;
    }
    (*ctxt).buf.borrow_mut().write_bytes(b"<!DOCTYPE ");

    (*ctxt)
        .buf
        .borrow_mut()
        .write_str(CStr::from_ptr((*dtd).name as _).to_string_lossy().as_ref());
    if let Some(external_id) = (*dtd).external_id.as_deref() {
        (*ctxt).buf.borrow_mut().write_bytes(b" PUBLIC ");
        if let Some(mut buf) = (*ctxt).buf.borrow().buffer {
            let external_id = CString::new(external_id).unwrap();
            buf.push_quoted_cstr(external_id.as_c_str());
        }
        (*ctxt).buf.borrow_mut().write_bytes(b" ");
        if let Some(mut buf) = (*ctxt).buf.borrow().buffer {
            let system_id = CString::new((*dtd).system_id.as_deref().unwrap()).unwrap();
            buf.push_quoted_cstr(&system_id);
        }
    } else if let Some(system_id) = (*dtd).system_id.as_deref() {
        (*ctxt).buf.borrow_mut().write_bytes(b" SYSTEM ");
        if let Some(mut buf) = (*ctxt).buf.borrow().buffer {
            let system_id = CString::new(system_id).unwrap();
            buf.push_quoted_cstr(&system_id);
        }
    }
    if (*dtd).entities.is_none()
        && (*dtd).elements.is_null()
        && (*dtd).attributes.is_null()
        && (*dtd).notations.is_null()
        && (*dtd).pentities.is_none()
    {
        (*ctxt).buf.borrow_mut().write_bytes(b">");
        return;
    }
    (*ctxt).buf.borrow_mut().write_bytes(b" [\n");
    /*
     * Dump the notations first they are not in the DTD children list
     * Do this only on a standalone DTD or on the internal subset though.
     */
    if !(*dtd).notations.is_null() && ((*dtd).doc.is_null() || (*(*dtd).doc).int_subset == dtd) {
        let buf = (*ctxt)
            .buf
            .borrow()
            .buffer
            .map_or(null_mut(), |buf| buf.as_ptr());
        xml_buf_dump_notation_table(buf, (*dtd).notations as _);
    }
    let format: i32 = (*ctxt).format;
    let level: i32 = (*ctxt).level;
    (*ctxt).format = 0;
    (*ctxt).level = -1;
    cur = (*dtd).children.map_or(null_mut(), |c| c.as_ptr());
    while !cur.is_null() {
        xml_node_dump_output_internal(ctxt, cur);
        cur = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
    }
    (*ctxt).format = format;
    (*ctxt).level = level;
    (*ctxt).buf.borrow_mut().write_bytes(b"]>");
}

/// Dump a local Namespace definition to a save context.
/// Should be called in the context of attribute dumps.
#[doc(alias = "xmlNsDumpOutputCtxt")]
unsafe extern "C" fn xml_ns_dump_output_ctxt(ctxt: XmlSaveCtxtPtr, cur: XmlNsPtr) {
    let mut buf = (*ctxt).buf.borrow_mut();
    xml_ns_dump_output(&mut buf, cur, ctxt);
}

/// Dump a list of XML attributes
#[doc(alias = "xhtmlAttrListDumpOutput")]
#[cfg(feature = "html")]
unsafe extern "C" fn xhtml_attr_list_dump_output(ctxt: XmlSaveCtxtPtr, mut cur: XmlAttrPtr) {
    use crate::tree::{xml_free_node, xml_new_doc_text, XmlNode};

    use super::htmltree::html_is_boolean_attr;

    let mut xml_lang: XmlAttrPtr = null_mut();
    let mut lang: XmlAttrPtr = null_mut();
    let mut name: XmlAttrPtr = null_mut();
    let mut id: XmlAttrPtr = null_mut();

    if cur.is_null() {
        return;
    }

    let parent = (*cur).parent;
    while !cur.is_null() {
        if (*cur).ns.is_null() && xml_str_equal((*cur).name, c"id".as_ptr() as _) {
            id = cur;
        } else if (*cur).ns.is_null() && xml_str_equal((*cur).name, c"name".as_ptr() as _) {
            name = cur;
        } else if (*cur).ns.is_null() && xml_str_equal((*cur).name, c"lang".as_ptr() as _) {
            lang = cur;
        } else if !(*cur).ns.is_null()
            && xml_str_equal((*cur).name, c"lang".as_ptr() as _)
            && xml_str_equal((*(*cur).ns).prefix, c"xml".as_ptr() as _)
        {
            xml_lang = cur;
        } else if (*cur).ns.is_null()
            && (*cur)
                .children
                .map_or(true, |c| c.content.is_null() || *c.content.add(0) == 0)
            && html_is_boolean_attr((*cur).name) != 0
        {
            if let Some(children) = (*cur).children {
                xml_free_node(children.as_ptr());
            }
            (*cur).children = NodePtr::from_ptr(xml_new_doc_text((*cur).doc, (*cur).name));
            if let Some(mut children) = (*cur).children {
                children.parent = NodePtr::from_ptr(cur as *mut XmlNode);
            }
        }
        xml_attr_dump_output(ctxt, cur);
        cur = (*cur).next;
    }
    let mut buf = (*ctxt).buf.borrow_mut();
    // C.8
    if (!name.is_null() && id.is_null())
        && parent
            .filter(|p| {
                !p.name.is_null()
                    && (xml_str_equal(p.name, c"a".as_ptr() as _)
                        || xml_str_equal(p.name, c"p".as_ptr() as _)
                        || xml_str_equal(p.name, c"div".as_ptr() as _)
                        || xml_str_equal(p.name, c"img".as_ptr() as _)
                        || xml_str_equal(p.name, c"map".as_ptr() as _)
                        || xml_str_equal(p.name, c"applet".as_ptr() as _)
                        || xml_str_equal(p.name, c"form".as_ptr() as _)
                        || xml_str_equal(p.name, c"frame".as_ptr() as _)
                        || xml_str_equal(p.name, c"iframe".as_ptr() as _))
            })
            .is_some()
    {
        buf.write_bytes(b" id=\"");
        xml_attr_serialize_content(&mut buf, name);
        buf.write_bytes(b"\"");
    }
    /*
     * C.7.
     */
    if !lang.is_null() && xml_lang.is_null() {
        buf.write_bytes(b" xml:lang=\"");
        xml_attr_serialize_content(&mut buf, lang);
        buf.write_bytes(b"\"");
    } else if !xml_lang.is_null() && lang.is_null() {
        buf.write_bytes(b" lang=\"");
        xml_attr_serialize_content(&mut buf, xml_lang);
        buf.write_bytes(b"\"");
    }
}

const XHTML_NS_NAME: &str = "http://www.w3.org/1999/xhtml";

/// Check if a node is an empty xhtml node
///
/// Returns 1 if the node is an empty node, 0 if not and -1 in case of error
#[doc(alias = "xhtmlIsEmpty")]
#[cfg(feature = "html")]
unsafe extern "C" fn xhtml_is_empty(node: XmlNodePtr) -> i32 {
    if node.is_null() {
        return -1;
    }
    if !matches!((*node).element_type(), XmlElementType::XmlElementNode) {
        return 0;
    }
    {
        let str2 = CString::new(XHTML_NS_NAME).unwrap();
        if !(*node).ns.is_null() && !xml_str_equal((*(*node).ns).href, str2.as_ptr() as _) {
            return 0;
        }
    }
    if (*node).children().is_some() {
        return 0;
    }
    match *(*node).name.add(0) {
        b'a' => {
            if xml_str_equal((*node).name, c"area".as_ptr() as _) {
                return 1;
            }
            return 0;
        }
        b'b' => {
            if xml_str_equal((*node).name, c"br".as_ptr() as _) {
                return 1;
            }
            if xml_str_equal((*node).name, c"base".as_ptr() as _) {
                return 1;
            }
            if xml_str_equal((*node).name, c"basefont".as_ptr() as _) {
                return 1;
            }
            return 0;
        }
        b'c' => {
            if xml_str_equal((*node).name, c"col".as_ptr() as _) {
                return 1;
            }
            return 0;
        }
        b'f' => {
            if xml_str_equal((*node).name, c"frame".as_ptr() as _) {
                return 1;
            }
            return 0;
        }
        b'h' => {
            if xml_str_equal((*node).name, c"hr".as_ptr() as _) {
                return 1;
            }
            return 0;
        }
        b'i' => {
            if xml_str_equal((*node).name, c"img".as_ptr() as _) {
                return 1;
            }
            if xml_str_equal((*node).name, c"input".as_ptr() as _) {
                return 1;
            }
            if xml_str_equal((*node).name, c"isindex".as_ptr() as _) {
                return 1;
            }
            return 0;
        }
        b'l' => {
            if xml_str_equal((*node).name, c"link".as_ptr() as _) {
                return 1;
            }
            return 0;
        }
        b'm' => {
            if xml_str_equal((*node).name, c"meta".as_ptr() as _) {
                return 1;
            }
            return 0;
        }
        b'p' => {
            if xml_str_equal((*node).name, c"param".as_ptr() as _) {
                return 1;
            }
            return 0;
        }
        _ => {}
    }
    0
}

/// Dump an XHTML node, recursive behaviour, children are printed too.
#[doc(alias = "xhtmlNodeDumpOutput")]
#[cfg(feature = "html")]
pub(crate) unsafe extern "C" fn xhtml_node_dump_output(ctxt: XmlSaveCtxtPtr, mut cur: XmlNodePtr) {
    use crate::{
        libxml::{parser_internals::XML_STRING_TEXT, xmlstring::xml_strcasecmp},
        tree::XmlNode,
    };

    let format: i32 = (*ctxt).format;
    let mut addmeta: i32;
    let mut tmp: XmlNodePtr;
    let mut unformatted_node: XmlNodePtr = null_mut();
    let mut parent: XmlNodePtr;
    let mut start: *mut XmlChar;
    let mut end: *mut XmlChar;

    if cur.is_null() {
        return;
    }

    let root: XmlNodePtr = cur;
    parent = (*cur).parent.map_or(null_mut(), |p| p.as_ptr());
    loop {
        match (*cur).element_type() {
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                xml_doc_content_dump_output(ctxt, cur as _);
            }
            XmlElementType::XmlNamespaceDecl => {
                xml_ns_dump_output_ctxt(ctxt, cur as _);
            }
            XmlElementType::XmlDTDNode => {
                xml_dtd_dump_output(ctxt, cur as _);
            }
            XmlElementType::XmlDocumentFragNode => {
                /* Always validate (*cur).parent when descending. */
                if let Some(children) = (*cur)
                    .children()
                    .filter(|_| (*cur).parent == NodePtr::from_ptr(parent))
                {
                    parent = cur;
                    cur = children.as_ptr();
                    continue;
                }
            }
            XmlElementType::XmlElementDecl => {
                xml_buf_dump_element_decl(
                    (*ctxt)
                        .buf
                        .borrow()
                        .buffer
                        .map_or(null_mut(), |buf| buf.as_ptr()),
                    cur as _,
                );
            }
            XmlElementType::XmlAttributeDecl => {
                xml_buf_dump_attribute_decl(
                    (*ctxt)
                        .buf
                        .borrow()
                        .buffer
                        .map_or(null_mut(), |buf| buf.as_ptr()),
                    cur as _,
                );
            }
            XmlElementType::XmlEntityDecl => {
                xml_buf_dump_entity_decl(
                    (*ctxt)
                        .buf
                        .borrow()
                        .buffer
                        .map_or(null_mut(), |buf| buf.as_ptr()),
                    cur as _,
                );
            }
            XmlElementType::XmlElementNode => {
                addmeta = 0;

                if cur != root && (*ctxt).format == 1 && get_indent_tree_output() != 0 {
                    let len = (*ctxt).indent_size
                        * if (*ctxt).level > (*ctxt).indent_nr as i32 {
                            (*ctxt).indent_nr
                        } else {
                            (*ctxt).level as usize
                        };
                    (*ctxt).buf.borrow_mut().write_bytes(&(*ctxt).indent[..len]);
                }

                /*
                 * Some users like lxml are known to pass nodes with a corrupted
                 * tree structure. Fall back to a recursive call to handle this
                 * case.
                 */
                if (*cur).parent != NodePtr::from_ptr(parent) && (*cur).children().is_some() {
                    xhtml_node_dump_output(ctxt, cur);
                    break;
                }

                (*ctxt).buf.borrow_mut().write_bytes(b"<");
                if !(*cur).ns.is_null() && !(*(*cur).ns).prefix.is_null() {
                    (*ctxt).buf.borrow_mut().write_str(
                        CStr::from_ptr((*(*cur).ns).prefix as _)
                            .to_string_lossy()
                            .as_ref(),
                    );
                    (*ctxt).buf.borrow_mut().write_bytes(b":");
                }

                (*ctxt)
                    .buf
                    .borrow_mut()
                    .write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
                if !(*cur).ns_def.is_null() {
                    xml_ns_list_dump_output_ctxt(ctxt, (*cur).ns_def);
                }
                if xml_str_equal((*cur).name, c"html".as_ptr() as _)
                    && (*cur).ns.is_null()
                    && (*cur).ns_def.is_null()
                {
                    /*
                     * 3.1.1. Strictly Conforming Documents A.3.1.1 3/
                     */

                    (*ctxt)
                        .buf
                        .borrow_mut()
                        .write_str(" xmlns=\"http://www.w3.org/1999/xhtml\"");
                }
                if !(*cur).properties.is_null() {
                    xhtml_attr_list_dump_output(ctxt, (*cur).properties);
                }

                if !parent.is_null()
                    && ((*parent).parent == NodePtr::from_ptr((*cur).doc as *mut XmlNode))
                    && xml_str_equal((*cur).name, c"head".as_ptr() as _)
                    && xml_str_equal((*parent).name, c"html".as_ptr() as _)
                {
                    tmp = (*cur).children().map_or(null_mut(), |c| c.as_ptr());
                    while !tmp.is_null() {
                        if xml_str_equal((*tmp).name, c"meta".as_ptr() as _) {
                            let httpequiv: *mut XmlChar = (*tmp).get_prop("http-equiv");
                            if !httpequiv.is_null() {
                                if xml_strcasecmp(httpequiv, c"Content-Type".as_ptr() as _) == 0 {
                                    xml_free(httpequiv as _);
                                    break;
                                }
                                xml_free(httpequiv as _);
                            }
                        }
                        tmp = (*tmp).next.map_or(null_mut(), |n| n.as_ptr());
                    }
                    if tmp.is_null() {
                        addmeta = 1;
                    }
                }

                if let Some(children) = (*cur).children() {
                    (*ctxt).buf.borrow_mut().write_bytes(b">");
                    if addmeta == 1 {
                        if (*ctxt).format == 1 {
                            (*ctxt).buf.borrow_mut().write_bytes(b"\n");
                            if get_indent_tree_output() != 0 {
                                let len = (*ctxt).indent_size
                                    * if (*ctxt).level + 1 > (*ctxt).indent_nr as i32 {
                                        (*ctxt).indent_nr
                                    } else {
                                        ((*ctxt).level + 1) as usize
                                    };
                                (*ctxt).buf.borrow_mut().write_bytes(&(*ctxt).indent[..len]);
                            }
                        }

                        (*ctxt).buf.borrow_mut().write_str(
                            "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=",
                        );
                        if let Some(encoding) = (*ctxt).encoding.as_deref() {
                            (*ctxt).buf.borrow_mut().write_str(encoding);
                        } else {
                            (*ctxt).buf.borrow_mut().write_bytes(b"UTF-8");
                        }
                        (*ctxt).buf.borrow_mut().write_bytes(b"\" />");
                    }

                    if (*ctxt).format == 1 {
                        tmp = children.as_ptr();
                        while !tmp.is_null() {
                            if (matches!((*tmp).element_type(), XmlElementType::XmlTextNode)
                                || matches!(
                                    (*tmp).element_type(),
                                    XmlElementType::XmlEntityRefNode
                                ))
                            {
                                unformatted_node = cur;
                                (*ctxt).format = 0;
                                break;
                            }
                            tmp = (*tmp).next.map_or(null_mut(), |n| n.as_ptr());
                        }
                    }

                    if (*ctxt).format == 1 {
                        (*ctxt).buf.borrow_mut().write_bytes(b"\n");
                    }
                    if (*ctxt).level >= 0 {
                        (*ctxt).level += 1;
                    }
                    parent = cur;
                    cur = children.as_ptr();
                    continue;
                } else if ((*cur).ns.is_null() || (*(*cur).ns).prefix.is_null())
                    && (xhtml_is_empty(cur) == 1 && addmeta == 0)
                {
                    /*
                     * C.2. Empty Elements
                     */
                    (*ctxt).buf.borrow_mut().write_bytes(b" />");
                } else {
                    if addmeta == 1 {
                        (*ctxt).buf.borrow_mut().write_bytes(b">");
                        if (*ctxt).format == 1 {
                            (*ctxt).buf.borrow_mut().write_bytes(b"\n");
                            if get_indent_tree_output() != 0 {
                                let len = (*ctxt).indent_size
                                    * if (*ctxt).level + 1 > (*ctxt).indent_nr as i32 {
                                        (*ctxt).indent_nr
                                    } else {
                                        ((*ctxt).level + 1) as usize
                                    };
                                (*ctxt).buf.borrow_mut().write_bytes(&(*ctxt).indent[..len]);
                            }
                        }

                        (*ctxt).buf.borrow_mut().write_str(
                            "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=",
                        );
                        if let Some(encoding) = (*ctxt).encoding.as_deref() {
                            (*ctxt).buf.borrow_mut().write_str(encoding);
                        } else {
                            (*ctxt).buf.borrow_mut().write_bytes(b"UTF-8");
                        }
                        (*ctxt).buf.borrow_mut().write_bytes(b"\" />");
                        if (*ctxt).format == 1 {
                            (*ctxt).buf.borrow_mut().write_bytes(b"\n");
                        }
                    } else {
                        (*ctxt).buf.borrow_mut().write_bytes(b">");
                    }
                    /*
                     * C.3. Element Minimization and Empty Element Content
                     */
                    (*ctxt).buf.borrow_mut().write_bytes(b"</");
                    if !(*cur).ns.is_null() && !(*(*cur).ns).prefix.is_null() {
                        (*ctxt).buf.borrow_mut().write_str(
                            CStr::from_ptr((*(*cur).ns).prefix as _)
                                .to_string_lossy()
                                .as_ref(),
                        );
                        (*ctxt).buf.borrow_mut().write_bytes(b":");
                    }

                    (*ctxt)
                        .buf
                        .borrow_mut()
                        .write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
                    (*ctxt).buf.borrow_mut().write_bytes(b">");
                }
            }
            XmlElementType::XmlTextNode => {
                if (*cur).content.is_null() {
                    break;
                }
                if (*cur).name == XML_STRING_TEXT.as_ptr() as _
                    || (*cur).name != XML_STRING_TEXT_NOENC.as_ptr() as _
                {
                    (*ctxt).buf.borrow_mut().write_str_with_escape(
                        CStr::from_ptr((*cur).content as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                        (*ctxt).escape,
                    );
                } else {
                    /*
                     * Disable escaping, needed for XSLT
                     */

                    (*ctxt).buf.borrow_mut().write_str(
                        CStr::from_ptr((*cur).content as _)
                            .to_string_lossy()
                            .as_ref(),
                    );
                }
            }
            XmlElementType::XmlPINode => {
                if !(*cur).content.is_null() {
                    (*ctxt).buf.borrow_mut().write_bytes(b"<?");

                    (*ctxt)
                        .buf
                        .borrow_mut()
                        .write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
                    if !(*cur).content.is_null() {
                        (*ctxt).buf.borrow_mut().write_bytes(b" ");

                        (*ctxt).buf.borrow_mut().write_str(
                            CStr::from_ptr((*cur).content as _)
                                .to_string_lossy()
                                .as_ref(),
                        );
                    }
                    (*ctxt).buf.borrow_mut().write_bytes(b"?>");
                } else {
                    (*ctxt).buf.borrow_mut().write_bytes(b"<?");

                    (*ctxt)
                        .buf
                        .borrow_mut()
                        .write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
                    (*ctxt).buf.borrow_mut().write_bytes(b"?>");
                }
            }
            XmlElementType::XmlCommentNode => {
                if !(*cur).content.is_null() {
                    (*ctxt).buf.borrow_mut().write_bytes(b"<!--");

                    (*ctxt).buf.borrow_mut().write_str(
                        CStr::from_ptr((*cur).content as _)
                            .to_string_lossy()
                            .as_ref(),
                    );
                    (*ctxt).buf.borrow_mut().write_bytes(b"-->");
                }
            }
            XmlElementType::XmlEntityRefNode => {
                (*ctxt).buf.borrow_mut().write_bytes(b"&");

                (*ctxt)
                    .buf
                    .borrow_mut()
                    .write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
                (*ctxt).buf.borrow_mut().write_bytes(b";");
            }
            XmlElementType::XmlCDATASectionNode => {
                if (*cur).content.is_null() || *(*cur).content == b'\0' {
                    (*ctxt).buf.borrow_mut().write_bytes(b"<![CDATA[]]>");
                } else {
                    start = (*cur).content;
                    end = (*cur).content;
                    while *end != b'\0' {
                        if *end == b']' && *end.add(1) == b']' && *end.add(2) == b'>' {
                            end = end.add(2);
                            (*ctxt).buf.borrow_mut().write_bytes(b"<![CDATA[");

                            (*ctxt).buf.borrow_mut().write_bytes(from_raw_parts(
                                start as _,
                                end.offset_from(start) as _,
                            ));
                            (*ctxt).buf.borrow_mut().write_bytes(b"]]>");
                            start = end;
                        }
                        end = end.add(1);
                    }
                    if start != end {
                        (*ctxt).buf.borrow_mut().write_bytes(b"<![CDATA[");

                        (*ctxt)
                            .buf
                            .borrow_mut()
                            .write_str(CStr::from_ptr(start as _).to_string_lossy().as_ref());
                        (*ctxt).buf.borrow_mut().write_bytes(b"]]>");
                    }
                }
            }
            XmlElementType::XmlAttributeNode => {
                xml_attr_dump_output(ctxt, cur as _);
            }
            _ => {}
        }

        loop {
            if cur == root {
                return;
            }
            if (*ctxt).format == 1 {
                (*ctxt).buf.borrow_mut().write_bytes(b"\n");
            }
            if let Some(next) = (*cur).next {
                cur = next.as_ptr();
                break;
            }

            cur = parent;
            /* (*cur).parent was validated when descending. */
            parent = (*cur).parent.map_or(null_mut(), |p| p.as_ptr());

            if matches!((*cur).element_type(), XmlElementType::XmlElementNode) {
                if (*ctxt).level > 0 {
                    (*ctxt).level -= 1;
                }
                if get_indent_tree_output() != 0 && (*ctxt).format == 1 {
                    let len = (*ctxt).indent_size
                        * if (*ctxt).level > (*ctxt).indent_nr as i32 {
                            (*ctxt).indent_nr
                        } else {
                            (*ctxt).level as usize
                        };
                    (*ctxt).buf.borrow_mut().write_bytes(&(*ctxt).indent[..len]);
                }

                (*ctxt).buf.borrow_mut().write_bytes(b"</");
                if !(*cur).ns.is_null() && !(*(*cur).ns).prefix.is_null() {
                    (*ctxt).buf.borrow_mut().write_str(
                        CStr::from_ptr((*(*cur).ns).prefix as _)
                            .to_string_lossy()
                            .as_ref(),
                    );
                    (*ctxt).buf.borrow_mut().write_bytes(b":");
                }

                (*ctxt)
                    .buf
                    .borrow_mut()
                    .write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
                (*ctxt).buf.borrow_mut().write_bytes(b">");

                if cur == unformatted_node {
                    (*ctxt).format = format;
                    unformatted_node = null_mut();
                }
            }
        }
    }
}

unsafe extern "C" fn xml_save_clear_encoding(ctxt: XmlSaveCtxtPtr) -> i32 {
    let mut buf = (*ctxt).buf.borrow_mut();
    buf.flush();
    let _ = buf.encoder.take();
    if let Some(conv) = buf.conv.take() {
        conv.free();
    }
    0
}

/// Dump an XML document.
#[doc(alias = "xmlDocContentDumpOutput")]
pub(crate) unsafe extern "C" fn xml_doc_content_dump_output(
    ctxt: XmlSaveCtxtPtr,
    cur: XmlDocPtr,
) -> i32 {
    #[cfg(feature = "html")]
    let dtd: XmlDtdPtr;
    #[cfg(feature = "html")]
    let mut is_html = false;
    let oldenc = (*cur).encoding.clone();
    let oldctxtenc = (*ctxt).encoding.clone();
    let oldescape = (*ctxt).escape;
    let oldescape_attr = (*ctxt).escape_attr;
    let mut switched_encoding: i32 = 0;

    xml_init_parser();

    if !matches!(
        (*cur).element_type(),
        XmlElementType::XmlHTMLDocumentNode | XmlElementType::XmlDocumentNode
    ) {
        return -1;
    }

    let mut encoding = (*ctxt).encoding.clone();
    if let Some(enc) = (*ctxt).encoding.as_deref() {
        (*cur).encoding = Some(enc.to_owned());
    } else if let Some(enc) = (*cur).encoding.as_deref() {
        encoding = Some(enc.to_owned());
    }

    if (matches!((*cur).element_type(), XmlElementType::XmlHTMLDocumentNode)
        && ((*ctxt).options & XmlSaveOption::XmlSaveAsXML as i32) == 0
        && ((*ctxt).options & XmlSaveOption::XmlSaveXHTML as i32) == 0)
        || (*ctxt).options & XmlSaveOption::XmlSaveAsHTML as i32 != 0
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
                && (*ctxt).buf.borrow().encoder.is_none()
                && (*ctxt).buf.borrow().conv.is_none())
                && xml_save_switch_encoding(&mut *ctxt, encoding.as_deref().unwrap()) < 0
            {
                (*cur).encoding = oldenc;
                return -1;
            }
            if (*ctxt).options & XmlSaveOption::XmlSaveFormat as i32 != 0 {
                html_doc_content_dump_format_output(
                    &mut (*ctxt).buf.borrow_mut(),
                    cur,
                    encoding.as_deref(),
                    1,
                );
            } else {
                html_doc_content_dump_format_output(
                    &mut (*ctxt).buf.borrow_mut(),
                    cur,
                    encoding.as_deref(),
                    0,
                );
            }
            if (*ctxt).encoding.is_some() {
                (*cur).encoding = oldenc;
            }
            return 0;
        }
        #[cfg(not(feature = "html"))]
        {
            return -1;
        }
    } else if matches!((*cur).element_type(), XmlElementType::XmlDocumentNode)
        || (*ctxt).options & XmlSaveOption::XmlSaveAsXML as i32 != 0
        || (*ctxt).options & XmlSaveOption::XmlSaveXHTML as i32 != 0
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
            && (*ctxt).buf.borrow().encoder.is_none()
            && (*ctxt).buf.borrow().conv.is_none()
            && ((*ctxt).options & XmlSaveOption::XmlSaveNoDecl as i32) == 0
        {
            if !matches!(
                enc,
                XmlCharEncoding::UTF8 | XmlCharEncoding::None | XmlCharEncoding::ASCII
            ) {
                /*
                 * we need to match to this encoding but just for this
                 * document since we output the XMLDecl the conversion
                 * must be done to not generate not well formed documents.
                 */
                if xml_save_switch_encoding(&mut *ctxt, encoding.as_deref().unwrap()) < 0 {
                    (*cur).encoding = oldenc;
                    return -1;
                }
                switched_encoding = 1;
            }
            if (*ctxt).escape == Some(xml_escape_entities) {
                (*ctxt).escape = None;
            }
            if (*ctxt).escape_attr == Some(xml_escape_entities) {
                (*ctxt).escape_attr = None;
            }
        }

        /*
         * Save the XML declaration
         */
        if (*ctxt).options & XmlSaveOption::XmlSaveNoDecl as i32 == 0 {
            (*ctxt).buf.borrow_mut().write_bytes(b"<?xml version=");
            if let Some(version) = (*cur).version.as_deref() {
                if let Some(mut buf) = (*ctxt).buf.borrow().buffer {
                    let version = CString::new(version).unwrap();
                    buf.push_quoted_cstr(&version);
                }
            } else {
                (*ctxt).buf.borrow_mut().write_bytes(b"\"1.0\"");
            }
            if let Some(encoding) = encoding.as_deref() {
                (*ctxt).buf.borrow_mut().write_bytes(b" encoding=");
                if let Some(mut buf) = (*ctxt).buf.borrow().buffer {
                    let enc = CString::new(encoding).unwrap();
                    buf.push_quoted_cstr(enc.as_c_str());
                }
            }
            match (*cur).standalone {
                0 => {
                    (*ctxt).buf.borrow_mut().write_bytes(b" standalone=\"no\"");
                }
                1 => {
                    (*ctxt).buf.borrow_mut().write_bytes(b" standalone=\"yes\"");
                }
                _ => {}
            }
            (*ctxt).buf.borrow_mut().write_bytes(b"?>\n");
        }

        #[cfg(feature = "html")]
        {
            if (*ctxt).options & XmlSaveOption::XmlSaveXHTML as i32 != 0 {
                is_html = true;
            }
            if (*ctxt).options & XmlSaveOption::XmlSaveNoXHTML as i32 == 0 {
                dtd = (*cur).get_int_subset();
                if !dtd.is_null() {
                    is_html = is_xhtml((*dtd).system_id.as_deref(), (*dtd).external_id.as_deref());
                }
            }
        }
        if let Some(children) = (*cur).children {
            let mut child = Some(children);

            while let Some(now) = child {
                (*ctxt).level = 0;
                #[cfg(feature = "html")]
                {
                    if is_html {
                        xhtml_node_dump_output(ctxt, now.as_ptr());
                    } else {
                        xml_node_dump_output_internal(ctxt, now.as_ptr());
                    }
                }
                #[cfg(not(feature = "html"))]
                {
                    xml_node_dump_output_internal(ctxt, now.as_ptr());
                }
                if !matches!(
                    now.element_type(),
                    XmlElementType::XmlXIncludeStart | XmlElementType::XmlXIncludeEnd
                ) {
                    (*ctxt).buf.borrow_mut().write_bytes(b"\n");
                }
                child = now.next;
            }
        }
    }

    /*
     * Restore the state of the saving context at the end of the document
     */
    if switched_encoding != 0 && oldctxtenc.is_none() {
        xml_save_clear_encoding(ctxt);
        (*ctxt).escape = oldescape;
        (*ctxt).escape_attr = oldescape_attr;
    }
    (*cur).encoding = oldenc;
    0
}

/// Free a saving context, destroying the output in any remaining buffer
#[doc(alias = "xmlFreeSaveCtxt")]
unsafe extern "C" fn xml_free_save_ctxt(ctxt: XmlSaveCtxtPtr) {
    if ctxt.is_null() {
        return;
    }
    (*ctxt).encoding = None;
    // does it work ???
    (*ctxt).buf = Rc::new_uninit().assume_init();
    xml_free(ctxt as _);
}

/// Create a new saving context
///
/// Returns the new structure or NULL in case of error
#[doc(alias = "xmlNewSaveCtxt")]
unsafe fn xml_new_save_ctxt(encoding: Option<&str>, mut options: i32) -> XmlSaveCtxtPtr {
    let ret: XmlSaveCtxtPtr = xml_malloc(size_of::<XmlSaveCtxt>()) as _;
    if ret.is_null() {
        xml_save_err_memory(c"creating saving context".as_ptr() as _);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlSaveCtxt>());
    std::ptr::write(&mut *ret, XmlSaveCtxt::default());

    if let Some(enc) = encoding {
        (*ret).handler = find_encoding_handler(enc).map(|e| Rc::new(RefCell::new(e)));
        if (*ret).handler.is_none() {
            let encoding = CString::new(enc).unwrap();
            xml_save_err(
                XmlParserErrors::XmlSaveUnknownEncoding,
                null_mut(),
                encoding.as_ptr(),
            );
            xml_free_save_ctxt(ret);
            return null_mut();
        }
        (*ret).encoding = Some(enc.to_owned());
        (*ret).escape = None;
    }
    xml_save_ctxt_init(&mut *ret);

    /*
     * Use the options
     */

    /* Re-check this option as it may already have been set */
    if (*ret).options & XmlSaveOption::XmlSaveNoEmpty as i32 != 0
        && options & XmlSaveOption::XmlSaveNoEmpty as i32 == 0
    {
        options |= XmlSaveOption::XmlSaveNoEmpty as i32;
    }

    (*ret).options = options;
    if options & XmlSaveOption::XmlSaveFormat as i32 != 0 {
        (*ret).format = 1;
    } else if options & XmlSaveOption::XmlSaveWsnonsig as i32 != 0 {
        (*ret).format = 2;
    }

    ret
}

/// Create a document saving context serializing to a filename or possibly
/// to an URL (but this is less reliable) with the encoding and the options given.
///
/// Returns a new serialization context or NULL in case of error.
#[doc(alias = "xmlSaveToFilename")]
pub unsafe fn xml_save_to_filename<'a, 'b: 'a>(
    filename: &'b str,
    encoding: Option<&'b str>,
    options: i32,
) -> XmlSaveCtxtPtr<'a> {
    let compression: i32 = 0; /* TODO handle compression option */

    let ret: XmlSaveCtxtPtr = xml_new_save_ctxt(encoding, options);
    if ret.is_null() {
        return null_mut();
    }
    let Some(buf) = XmlOutputBuffer::from_uri(filename, (*ret).handler.clone(), compression) else {
        xml_free_save_ctxt(ret);
        return null_mut();
    };
    (*ret).buf = Rc::new(RefCell::new(buf));
    ret
}

/// Create a document saving context serializing to a file descriptor
/// with the encoding and the options given
///
/// Returns a new serialization context or NULL in case of error.
#[doc(alias = "xmlSaveToIO")]
pub unsafe fn xml_save_to_io<'a>(
    ioctx: impl Write + 'a,
    encoding: Option<&'a str>,
    options: i32,
) -> XmlSaveCtxtPtr {
    let ret: XmlSaveCtxtPtr = xml_new_save_ctxt(encoding, options);
    if ret.is_null() {
        return null_mut();
    }
    let Some(buf) =
        XmlOutputBuffer::from_writer_with_wrapped_encoder(ioctx, (*ret).handler.clone())
    else {
        xml_free_save_ctxt(ret);
        return null_mut();
    };
    (*ret).buf = Rc::new(RefCell::new(buf));
    ret
}

/// Save a full document to a saving context
/// TODO: The function is not fully implemented yet as it does not return the
/// byte count but 0 instead
///
/// Returns the number of byte written or -1 in case of error
#[doc(alias = "xmlSaveDoc")]
pub unsafe extern "C" fn xml_save_doc(ctxt: XmlSaveCtxtPtr, doc: XmlDocPtr) -> i64 {
    let ret: i64 = 0;

    if ctxt.is_null() || doc.is_null() {
        return -1;
    }
    if xml_doc_content_dump_output(ctxt, doc) < 0 {
        return -1;
    }
    ret
}

/// Dump an HTML node, recursive behaviour, children are printed too.
#[doc(alias = "htmlNodeDumpOutputInternal")]
#[cfg(feature = "html")]
unsafe extern "C" fn html_node_dump_output_internal(ctxt: XmlSaveCtxtPtr, cur: XmlNodePtr) -> i32 {
    use super::htmltree::html_node_dump_format_output;

    let mut oldenc = None;
    let oldctxtenc = (*ctxt).encoding.clone();
    let mut encoding = (*ctxt).encoding.clone();

    let mut switched_encoding: i32 = 0;

    xml_init_parser();

    let doc: XmlDocPtr = (*cur).doc;
    if !doc.is_null() {
        oldenc = (*doc).encoding.clone();
        if let Some(encoding) = (*ctxt).encoding.as_deref() {
            (*doc).encoding = Some(encoding.to_owned());
        } else if let Some(enc) = (*doc).encoding.as_deref() {
            encoding = Some(enc.to_owned());
        }
    }

    if encoding.is_some() && !doc.is_null() {
        html_set_meta_encoding(doc, encoding.as_deref());
    }
    if encoding.is_none() && !doc.is_null() {
        encoding = html_get_meta_encoding(doc);
    }
    if encoding.is_none() {
        encoding = Some("HTML".to_owned());
    }
    if encoding.is_some()
        && oldctxtenc.is_none()
        && (*ctxt).buf.borrow().encoder.is_none()
        && (*ctxt).buf.borrow().conv.is_none()
    {
        if xml_save_switch_encoding(&mut *ctxt, encoding.as_deref().unwrap()) < 0 {
            (*doc).encoding = oldenc;
            return -1;
        }
        switched_encoding = 1;
    }
    let mut buf = (*ctxt).buf.borrow_mut();
    if (*ctxt).options & XmlSaveOption::XmlSaveFormat as i32 != 0 {
        html_node_dump_format_output(&mut buf, doc, cur, encoding.as_deref(), 1);
    } else {
        html_node_dump_format_output(&mut buf, doc, cur, encoding.as_deref(), 0);
    }
    /*
     * Restore the state of the saving context at the end of the document
     */
    if switched_encoding != 0 && oldctxtenc.is_none() {
        xml_save_clear_encoding(ctxt);
    }
    if !doc.is_null() {
        (*doc).encoding = oldenc;
    }
    0
}

/// Save a subtree starting at the node parameter to a saving context
/// TODO: The function is not fully implemented yet as it does not return the
/// byte count but 0 instead
///
/// Returns the number of byte written or -1 in case of error
#[doc(alias = "xmlSaveTree")]
pub unsafe extern "C" fn xml_save_tree(ctxt: XmlSaveCtxtPtr, node: XmlNodePtr) -> i64 {
    let ret: i64 = 0;

    if ctxt.is_null() || node.is_null() {
        return -1;
    }
    #[cfg(feature = "html")]
    {
        if (*ctxt).options & XmlSaveOption::XmlSaveXHTML as i32 != 0 {
            xhtml_node_dump_output(ctxt, node);
            return ret;
        }
        if (!matches!((*node).element_type(), XmlElementType::XmlNamespaceDecl)
            && !(*node).doc.is_null()
            && matches!((*(*node).doc).typ, XmlElementType::XmlHTMLDocumentNode)
            && (*ctxt).options & XmlSaveOption::XmlSaveAsXML as i32 == 0)
            || (*ctxt).options & XmlSaveOption::XmlSaveAsHTML as i32 != 0
        {
            html_node_dump_output_internal(ctxt, node);
            return ret;
        }
    }
    xml_node_dump_output_internal(ctxt, node);
    ret
}

/// Flush a document saving context, i.e. make sure that all bytes have been output.
///
/// Returns the number of byte written or -1 in case of error.
#[doc(alias = "xmlSaveFlush")]
pub unsafe extern "C" fn xml_save_flush(ctxt: XmlSaveCtxtPtr) -> i32 {
    if ctxt.is_null() {
        return -1;
    }
    (*ctxt).buf.borrow_mut().flush()
}

/// Close a document saving context, i.e. make sure that all bytes have
/// been output and free the associated data.
///
/// Returns the number of byte written or -1 in case of error.
#[doc(alias = "xmlSaveClose")]
pub unsafe extern "C" fn xml_save_close(ctxt: XmlSaveCtxtPtr) -> i32 {
    if ctxt.is_null() {
        return -1;
    }
    let ret: i32 = xml_save_flush(ctxt);
    xml_free_save_ctxt(ctxt);
    ret
}

/// Set a custom escaping function to be used for text in element content
///
/// Returns 0 if successful or -1 in case of error.
#[doc(alias = "xmlSaveSetEscape")]
pub unsafe fn xml_save_set_escape(
    ctxt: XmlSaveCtxtPtr,
    escape: Option<fn(&str, &mut String) -> i32>,
) -> i32 {
    if ctxt.is_null() {
        return -1;
    }
    (*ctxt).escape = escape;
    0
}

/// Set a custom escaping function to be used for text in attribute content
///
/// Returns 0 if successful or -1 in case of error.
#[doc(alias = "xmlSaveSetAttrEscape")]
pub unsafe fn xml_save_set_attr_escape(
    ctxt: XmlSaveCtxtPtr,
    escape: Option<fn(&str, &mut String) -> i32>,
) -> i32 {
    if ctxt.is_null() {
        return -1;
    }
    (*ctxt).escape_attr = escape;
    0
}

/// Serialize text attribute values to an xmlBufPtr
#[doc(alias = "xmlBufAttrSerializeTxtContent")]
pub(crate) unsafe extern "C" fn xml_buf_attr_serialize_txt_content(
    buf: XmlBufPtr,
    doc: XmlDocPtr,
    attr: XmlAttrPtr,
    string: *const XmlChar,
) {
    let mut base: *mut XmlChar;
    let mut cur: *mut XmlChar;

    if string.is_null() {
        return;
    }
    base = string as _;
    cur = base;
    while *cur != 0 {
        if *cur == b'\n' {
            if base != cur {
                xml_buf_add(buf, base, cur.offset_from(base) as _);
            }
            xml_buf_add(buf, c"&#10;".as_ptr() as _, 5);
            cur = cur.add(1);
            base = cur;
        } else if *cur == b'\r' {
            if base != cur {
                xml_buf_add(buf, base, cur.offset_from(base) as _);
            }
            xml_buf_add(buf, c"&#13;".as_ptr() as _, 5);
            cur = cur.add(1);
            base = cur;
        } else if *cur == b'\t' {
            if base != cur {
                xml_buf_add(buf, base, cur.offset_from(base) as _);
            }
            xml_buf_add(buf, c"&#9;".as_ptr() as _, 4);
            cur = cur.add(1);
            base = cur;
        } else if *cur == b'"' {
            if base != cur {
                xml_buf_add(buf, base, cur.offset_from(base) as _);
            }
            xml_buf_add(buf, c"&quot;".as_ptr() as _, 6);
            cur = cur.add(1);
            base = cur;
        } else if *cur == b'<' {
            if base != cur {
                xml_buf_add(buf, base, cur.offset_from(base) as _);
            }
            xml_buf_add(buf, c"&lt;".as_ptr() as _, 4);
            cur = cur.add(1);
            base = cur;
        } else if *cur == b'>' {
            if base != cur {
                xml_buf_add(buf, base, cur.offset_from(base) as _);
            }
            xml_buf_add(buf, c"&gt;".as_ptr() as _, 4);
            cur = cur.add(1);
            base = cur;
        } else if *cur == b'&' {
            if base != cur {
                xml_buf_add(buf, base, cur.offset_from(base) as _);
            }
            xml_buf_add(buf, c"&amp;".as_ptr() as _, 5);
            cur = cur.add(1);
            base = cur;
        } else if *cur >= 0x80 && *cur.add(1) != 0 && (doc.is_null() || (*doc).encoding.is_none()) {
            /*
             * We assume we have UTF-8 content.
             */
            let mut tmp: [u8; 12] = [0; 12];
            let mut val = 0;
            let mut l = 1;

            if base != cur {
                xml_buf_add(buf, base, cur.offset_from(base) as _);
            }
            if *cur < 0xC0 {
                xml_save_err(XmlParserErrors::XmlSaveNotUTF8 as _, attr as _, null_mut());
                xml_serialize_hex_char_ref(&mut tmp, *cur as _);
                xml_buf_add(buf, tmp.as_ptr() as _, -1);
                cur = cur.add(1);
                base = cur;
                continue;
            } else if *cur < 0xE0 {
                val = *cur.add(0) as i32 & 0x1F;
                val <<= 6;
                val |= *cur.add(1) as i32 & 0x3F;
                l = 2;
            } else if *cur < 0xF0 && *cur.add(2) != 0 {
                val = *cur.add(0) as i32 & 0x0F;
                val <<= 6;
                val |= *cur.add(1) as i32 & 0x3F;
                val <<= 6;
                val |= *cur.add(2) as i32 & 0x3F;
                l = 3;
            } else if *cur < 0xF8 && *cur.add(2) != 0 && *cur.add(3) != 0 {
                val = *cur.add(0) as i32 & 0x07;
                val <<= 6;
                val |= *cur.add(1) as i32 & 0x3F;
                val <<= 6;
                val |= *cur.add(2) as i32 & 0x3F;
                val <<= 6;
                val |= *cur.add(3) as i32 & 0x3F;
                l = 4;
            }
            if l == 1 || !xml_is_char(val as u32) {
                xml_save_err(
                    XmlParserErrors::XmlSaveCharInvalid as _,
                    attr as _,
                    null_mut(),
                );
                xml_serialize_hex_char_ref(&mut tmp, *cur as _);
                xml_buf_add(buf, tmp.as_ptr() as _, -1);
                cur = cur.add(1);
                base = cur;
                continue;
            }
            /*
             * We could do multiple things here. Just save
             * as a c_char ref
             */
            xml_serialize_hex_char_ref(&mut tmp, val as u32);
            xml_buf_add(buf, tmp.as_ptr() as _, -1);
            cur = cur.add(l as usize);
            base = cur;
        } else {
            cur = cur.add(1);
        }
    }
    if base != cur {
        xml_buf_add(buf, base, cur.offset_from(base) as _);
    }
}

/// Dump a list of local Namespace definitions.
/// Should be called in the context of attributes dumps.
#[doc(alias = "xmlNsListDumpOutput")]
pub(crate) unsafe extern "C" fn xml_ns_list_dump_output(
    buf: &mut XmlOutputBuffer,
    mut cur: XmlNsPtr,
) {
    while !cur.is_null() {
        xml_ns_dump_output(buf, cur, null_mut());
        cur = (*cur).next;
    }
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_save_close() {
        #[cfg(feature = "libxml_output")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SAVE_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_save_ctxt_ptr(n_ctxt, 0);

                let ret_val = xml_save_close(ctxt);
                desret_int(ret_val);
                des_xml_save_ctxt_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlSaveClose",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlSaveClose()");
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_save_doc() {
        #[cfg(feature = "libxml_output")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SAVE_CTXT_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_save_ctxt_ptr(n_ctxt, 0);
                    let doc = gen_xml_doc_ptr(n_doc, 1);

                    let ret_val = xml_save_doc(ctxt, doc);
                    desret_long(ret_val);
                    des_xml_save_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_xml_doc_ptr(n_doc, doc, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlSaveDoc",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlSaveDoc()");
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_doc);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_save_flush() {
        #[cfg(feature = "libxml_output")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SAVE_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_save_ctxt_ptr(n_ctxt, 0);

                let ret_val = xml_save_flush(ctxt);
                desret_int(ret_val);
                des_xml_save_ctxt_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlSaveFlush",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlSaveFlush()");
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_save_set_attr_escape() {

        /* missing type support */
    }

    #[test]
    fn test_xml_save_set_escape() {

        /* missing type support */
    }

    #[test]
    fn test_xml_save_to_buffer() {

        /* missing type support */
    }

    #[test]
    fn test_xml_save_to_fd() {

        /* missing type support */
    }

    #[test]
    fn test_xml_save_to_filename() {

        /* missing type support */
    }

    #[test]
    fn test_xml_save_tree() {
        #[cfg(feature = "libxml_output")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SAVE_CTXT_PTR {
                for n_cur in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_save_ctxt_ptr(n_ctxt, 0);
                    let cur = gen_xml_node_ptr(n_cur, 1);

                    let ret_val = xml_save_tree(ctxt, cur);
                    desret_long(ret_val);
                    des_xml_save_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_xml_node_ptr(n_cur, cur, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlSaveTree",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlSaveTree()");
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
        }
    }
}
