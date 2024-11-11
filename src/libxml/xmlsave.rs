//! Provide methods and data structures for serializing XML documents.  
//! This module is based on `libxml/xmlsave.h`, `xmlsave.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    cell::RefCell,
    ffi::{c_char, CStr, CString},
    mem::size_of,
    os::raw::c_void,
    ptr::{null, null_mut},
    rc::Rc,
    slice::from_raw_parts,
    str::from_utf8_unchecked,
    sync::atomic::Ordering,
};

use libc::memset;

use crate::{
    buf::XmlBufRef,
    encoding::{find_encoding_handler, XmlCharEncoding, XmlCharEncodingHandler},
    error::{XmlErrorDomain, XmlParserErrors, __xml_simple_error},
    globals::{get_indent_tree_output, GLOBAL_STATE},
    io::{
        xml_output_buffer_close, xml_output_buffer_create_filename, xml_output_buffer_create_io,
        XmlOutputBufferPtr, XmlOutputCloseCallback, XmlOutputWriteCallback,
    },
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
    private::{buf::xml_buf_set_allocation_scheme, save::xml_buf_attr_serialize_txt_content},
    tree::{
        is_xhtml, XmlAttrPtr, XmlAttributePtr, XmlBufPtr, XmlBufferAllocationScheme, XmlDocPtr,
        XmlDtdPtr, XmlElementPtr, XmlElementType, XmlNodePtr, XmlNsPtr, XML_LOCAL_NAMESPACE,
    },
};

const MAX_INDENT: usize = 60;

/**
 * xmlSaveOption:
 *
 * This is the set of XML save options that can be passed down
 * to the xmlSaveToFd() and similar calls.
 */
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

pub type XmlSaveCtxtPtr = *mut XmlSaveCtxt;
#[repr(C)]
pub struct XmlSaveCtxt {
    pub(crate) _private: *mut c_void,
    pub(crate) typ: i32,
    pub(crate) fd: i32,
    pub(crate) filename: *const XmlChar,
    pub(crate) encoding: Option<String>,
    pub(crate) handler: Option<Rc<RefCell<XmlCharEncodingHandler>>>,
    pub(crate) buf: XmlOutputBufferPtr,
    pub(crate) options: i32,
    pub(crate) level: i32,
    pub(crate) format: i32,
    pub(crate) indent: [u8; MAX_INDENT + 1], /* array for indenting output */
    pub(crate) indent_nr: usize,
    pub(crate) indent_size: usize,
    pub(crate) escape: Option<fn(&str, &mut String) -> i32>, /* used for element content */
    pub(crate) escape_attr: Option<fn(&str, &mut String) -> i32>, /* used for attribute content */
}

impl Default for XmlSaveCtxt {
    fn default() -> Self {
        Self {
            _private: null_mut(),
            typ: 0,
            fd: 0,
            filename: null(),
            encoding: None,
            handler: None,
            buf: null_mut(),
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

/**
 * xmlSaveErr:
 * @code:  the error number
 * @node:  the location of the error.
 * @extra:  extra information
 *
 * Handle an out of memory condition
 */
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

/**
 * xmlSaveErrMemory:
 * @extra:  extra information
 *
 * Handle an out of memory condition
 */
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

/**
 * xmlEscapeEntities:
 * @out:  a pointer to an array of bytes to store the result
 * @outlen:  the length of @out
 * @in:  a pointer to an array of unescaped UTF-8 bytes
 * @inlen:  the length of @in
 *
 * Take a block of UTF-8 chars in and escape them. Used when there is no
 * encoding specified.
 *
 * Returns 0 if success, or -1 otherwise
 * The value of @inlen after return is the number of octets consumed
 *     if the return value is positive, else unpredictable.
 * The value of @outlen after return is the number of octets consumed.
 */
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

/**
 * xmlSaveCtxtInit:
 * @ctxt: the saving context
 *
 * Initialize a saving context
 */
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
    let buf: XmlOutputBufferPtr = ctxt.buf;

    if (*buf).encoder.is_none() && (*buf).conv.is_none() {
        (*buf).encoder = find_encoding_handler(encoding).map(|e| Rc::new(RefCell::new(e)));
        if (*buf).encoder.is_none() {
            let encoding = CString::new(encoding).unwrap();
            xml_save_err(
                XmlParserErrors::XmlSaveUnknownEncoding,
                null_mut(),
                encoding.as_ptr(),
            );
            return -1;
        }
        (*buf).conv = XmlBufRef::new();
        if (*buf).conv.is_none() {
            xml_save_err_memory(c"creating encoding buffer".as_ptr() as _);
            return -1;
        }
        /*
         * initialize the state, e.g. if outputting a BOM
         */
        (*buf).encode(true);
    }
    0
}

/**
 * xmlOutputBufferWriteWSNonSig:
 * @ctxt:  The save context
 * @extra: Number of extra indents to apply to (*ctxt).level
 *
 * Write out formatting for non-significant whitespace output.
 */
unsafe extern "C" fn xml_output_buffer_write_ws_non_sig(ctxt: &mut XmlSaveCtxt, extra: i32) {
    if ctxt.buf.is_null() {
        return;
    }
    (*ctxt.buf).write_bytes(b"\n");
    for i in (0..ctxt.level + extra).step_by(ctxt.indent_nr) {
        let len = ctxt.indent_size
            * if ctxt.level + extra - i > ctxt.indent_nr as i32 {
                ctxt.indent_nr
            } else {
                (ctxt.level + extra - i) as usize
            };
        (*ctxt.buf).write_bytes(&ctxt.indent[..len]);
    }
}

/**
 * xmlNsDumpOutput:
 * @buf:  the XML buffer output
 * @cur:  a namespace
 * @ctxt: the output save context. Optional.
 *
 * Dump a local Namespace definition.
 * Should be called in the context of attributes dumps.
 * If @ctxt is supplied, @buf should be its buffer.
 */
pub(crate) unsafe extern "C" fn xml_ns_dump_output(
    buf: XmlOutputBufferPtr,
    cur: XmlNsPtr,
    ctxt: XmlSaveCtxtPtr,
) {
    if cur.is_null() || buf.is_null() {
        return;
    }
    if matches!((*cur).typ, Some(XML_LOCAL_NAMESPACE))
        && !(*cur).href.load(Ordering::Relaxed).is_null()
    {
        if xml_str_equal((*cur).prefix.load(Ordering::Relaxed), c"xml".as_ptr() as _) {
            return;
        }

        if !ctxt.is_null() && (*ctxt).format == 2 {
            xml_output_buffer_write_ws_non_sig(&mut *ctxt, 2);
        } else {
            (*buf).write_bytes(b" ");
        }

        /* Within the context of an element attributes */
        if !(*cur).prefix.load(Ordering::Relaxed).is_null() {
            (*buf).write_bytes(b"xmlns:");
            (*buf).write_str(
                CStr::from_ptr((*cur).prefix.load(Ordering::Relaxed) as _)
                    .to_string_lossy()
                    .as_ref(),
            );
        } else {
            (*buf).write_bytes(b"xmlns");
        }
        (*buf).write_bytes(b"=");
        if let Some(mut buf) = (*buf).buffer {
            buf.push_quoted_cstr(CStr::from_ptr(
                (*cur).href.load(Ordering::Relaxed) as *const i8
            ));
        }
    }
}

/**
 * xmlBufDumpNotationTable:
 * @buf:  an xmlBufPtr output
 * @table:  A notation table
 *
 * This will dump the content of the notation table as an XML DTD definition
 */
unsafe extern "C" fn xml_buf_dump_notation_table(buf: XmlBufPtr, table: XmlNotationTablePtr) {
    xml_buf_set_allocation_scheme(buf, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);
    xml_dump_notation_table(buf, table);
}

/**
 * xmlBufDumpElementDecl:
 * @buf:  an xmlBufPtr output
 * @elem:  An element table
 *
 * This will dump the content of the element declaration as an XML
 * DTD definition
 */
unsafe extern "C" fn xml_buf_dump_element_decl(buf: XmlBufPtr, elem: XmlElementPtr) {
    xml_buf_set_allocation_scheme(buf, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);
    xml_dump_element_decl(buf, elem);
}

/**
 * xmlBufDumpAttributeDecl:
 * @buf:  an xmlBufPtr output
 * @attr:  An attribute declaration
 *
 * This will dump the content of the attribute declaration as an XML
 * DTD definition
 */
unsafe extern "C" fn xml_buf_dump_attribute_decl(buf: XmlBufPtr, attr: XmlAttributePtr) {
    xml_buf_set_allocation_scheme(buf, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);
    xml_dump_attribute_decl(buf, attr);
}

/**
 * xmlBufDumpEntityDecl:
 * @buf:  an xmlBufPtr output
 * @ent:  An entity table
 *
 * This will dump the content of the entity table as an XML DTD definition
 */
unsafe extern "C" fn xml_buf_dump_entity_decl(buf: XmlBufPtr, ent: XmlEntityPtr) {
    xml_buf_set_allocation_scheme(buf, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);
    xml_dump_entity_decl(buf, ent);
}

/**
 * xmlNsListDumpOutputCtxt
 * @ctxt: the save context
 * @cur:  the first namespace
 *
 * Dump a list of local namespace definitions to a save context.
 * Should be called in the context of attribute dumps.
 */
unsafe extern "C" fn xml_ns_list_dump_output_ctxt(ctxt: XmlSaveCtxtPtr, mut cur: XmlNsPtr) {
    while !cur.is_null() {
        xml_ns_dump_output((*ctxt).buf, cur, ctxt);
        cur = (*cur).next.load(Ordering::Relaxed);
    }
}

/**
 * xmlAttrSerializeContent:
 * @buf:  the XML buffer output
 * @doc:  the document
 * @attr:  the attribute pointer
 *
 * Serialize the attribute in the buffer
 */
unsafe extern "C" fn xml_attr_serialize_content(buf: XmlOutputBufferPtr, attr: XmlAttrPtr) {
    let mut children: XmlNodePtr;

    children = (*attr).children;
    while !children.is_null() {
        match (*children).typ {
            XmlElementType::XmlTextNode => {
                xml_buf_attr_serialize_txt_content(
                    (*buf).buffer.map_or(null_mut(), |buf| buf.as_ptr()),
                    (*attr).doc,
                    attr,
                    (*children).content,
                );
            }
            XmlElementType::XmlEntityRefNode => {
                if let Some(mut buf) = (*buf).buffer {
                    buf.push_bytes(b"&");
                    buf.push_cstr(CStr::from_ptr((*children).name as *const i8));
                    buf.push_bytes(b";");
                }
            }
            _ => { /* should not happen unless we have a badly built tree */ }
        }
        children = (*children).next;
    }
}

/**
 * xmlAttrDumpOutput:
 * @buf:  the XML buffer output
 * @cur:  the attribute pointer
 *
 * Dump an XML attribute
 */
unsafe extern "C" fn xml_attr_dump_output(ctxt: XmlSaveCtxtPtr, cur: XmlAttrPtr) {
    if cur.is_null() {
        return;
    }
    let buf: XmlOutputBufferPtr = (*ctxt).buf;
    if buf.is_null() {
        return;
    }
    if (*ctxt).format == 2 {
        xml_output_buffer_write_ws_non_sig(&mut *ctxt, 2);
    } else {
        (*buf).write_bytes(b" ");
    }
    if !(*cur).ns.is_null() && !(*(*cur).ns).prefix.load(Ordering::Relaxed).is_null() {
        (*buf).write_str(
            CStr::from_ptr((*(*cur).ns).prefix.load(Ordering::Relaxed) as _)
                .to_string_lossy()
                .as_ref(),
        );
        (*buf).write_bytes(b":");
    }

    (*buf).write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
    (*buf).write_bytes(b"=\"");
    xml_attr_serialize_content(buf, cur);
    (*buf).write_bytes(b"\"");
}

/**
 * xmlNodeDumpOutputInternal:
 * @cur:  the current node
 *
 * Dump an XML node, recursive behaviour, children are printed too.
 */
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
    let buf: XmlOutputBufferPtr = (*ctxt).buf;

    let root: XmlNodePtr = cur;
    parent = (*cur).parent;
    loop {
        match (*cur).typ {
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                xml_doc_content_dump_output(ctxt, cur as _);
            }
            XmlElementType::XmlDTDNode => {
                xml_dtd_dump_output(ctxt, cur as _);
            }
            XmlElementType::XmlDocumentFragNode => {
                /* Always validate (*cur).parent when descending. */
                if (*cur).parent == parent && !(*cur).children.is_null() {
                    parent = cur;
                    cur = (*cur).children;
                    continue;
                }
            }
            XmlElementType::XmlElementDecl => {
                xml_buf_dump_element_decl(
                    (*buf).buffer.map_or(null_mut(), |buf| buf.as_ptr()),
                    cur as _,
                );
            }
            XmlElementType::XmlAttributeDecl => {
                xml_buf_dump_attribute_decl(
                    (*buf).buffer.map_or(null_mut(), |buf| buf.as_ptr()),
                    cur as _,
                );
            }
            XmlElementType::XmlEntityDecl => {
                xml_buf_dump_entity_decl(
                    (*buf).buffer.map_or(null_mut(), |buf| buf.as_ptr()),
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
                    (*buf).write_bytes(&(*ctxt).indent[..len]);
                }

                /*
                 * Some users like lxml are known to pass nodes with a corrupted
                 * tree structure. Fall back to a recursive call to handle this
                 * case.
                 */
                if (*cur).parent != parent && !(*cur).children.is_null() {
                    xml_node_dump_output_internal(ctxt, cur);
                } else {
                    (*buf).write_bytes(b"<");
                    if !(*cur).ns.is_null()
                        && !(*(*cur).ns).prefix.load(Ordering::Relaxed).is_null()
                    {
                        (*buf).write_str(
                            CStr::from_ptr((*(*cur).ns).prefix.load(Ordering::Relaxed) as _)
                                .to_string_lossy()
                                .as_ref(),
                        );
                        (*buf).write_bytes(b":");
                    }

                    (*buf).write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
                    if !(*cur).ns_def.is_null() {
                        xml_ns_list_dump_output_ctxt(ctxt, (*cur).ns_def);
                    }
                    attr = (*cur).properties;
                    while !attr.is_null() {
                        xml_attr_dump_output(ctxt, attr);
                        attr = (*attr).next;
                    }

                    if (*cur).children.is_null() {
                        if (*ctxt).options & XmlSaveOption::XmlSaveNoEmpty as i32 == 0 {
                            if (*ctxt).format == 2 {
                                xml_output_buffer_write_ws_non_sig(&mut *ctxt, 0);
                            }
                            (*buf).write_bytes(b"/>");
                        } else {
                            if (*ctxt).format == 2 {
                                xml_output_buffer_write_ws_non_sig(&mut *ctxt, 1);
                            }
                            (*buf).write_bytes(b"></");
                            if !(*cur).ns.is_null()
                                && !(*(*cur).ns).prefix.load(Ordering::Relaxed).is_null()
                            {
                                (*buf).write_str(
                                    CStr::from_ptr((*(*cur).ns).prefix.load(Ordering::Relaxed) as _).to_string_lossy().as_ref(),
                                );
                                (*buf).write_bytes(b":");
                            }

                            (*buf).write_str(
                                CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref(),
                            );
                            if (*ctxt).format == 2 {
                                xml_output_buffer_write_ws_non_sig(&mut *ctxt, 0);
                            }
                            (*buf).write_bytes(b">");
                        }
                    } else {
                        if (*ctxt).format == 1 {
                            tmp = (*cur).children;
                            while !tmp.is_null() {
                                if matches!(
                                    (*tmp).typ,
                                    XmlElementType::XmlTextNode
                                        | XmlElementType::XmlCDATASectionNode
                                        | XmlElementType::XmlEntityRefNode
                                ) {
                                    (*ctxt).format = 0;
                                    unformatted_node = cur;
                                    break;
                                }
                                tmp = (*tmp).next;
                            }
                        }
                        if (*ctxt).format == 2 {
                            xml_output_buffer_write_ws_non_sig(&mut *ctxt, 1);
                        }
                        (*buf).write_bytes(b">");
                        if (*ctxt).format == 1 {
                            (*buf).write_bytes(b"\n");
                        }
                        if (*ctxt).level >= 0 {
                            (*ctxt).level += 1;
                        }
                        parent = cur;
                        cur = (*cur).children;
                        continue;
                    }
                }
            }
            XmlElementType::XmlTextNode => {
                if !(*cur).content.is_null() {
                    if (*cur).name != XML_STRING_TEXT_NOENC.as_ptr() as _ {
                        (*buf).write_str_with_escape(
                            CStr::from_ptr((*cur).content as *const i8)
                                .to_string_lossy()
                                .as_ref(),
                            (*ctxt).escape,
                        );
                    } else {
                        /*
                         * Disable escaping, needed for XSLT
                         */

                        (*buf).write_str(
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
                    (*buf).write_bytes(&(*ctxt).indent[..len]);
                }

                if !(*cur).content.is_null() {
                    (*buf).write_bytes(b"<?");

                    (*buf).write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
                    if !(*cur).content.is_null() {
                        if (*ctxt).format == 2 {
                            xml_output_buffer_write_ws_non_sig(&mut *ctxt, 0);
                        } else {
                            (*buf).write_bytes(b" ");
                        }

                        (*buf).write_str(
                            CStr::from_ptr((*cur).content as _)
                                .to_string_lossy()
                                .as_ref(),
                        );
                    }
                    (*buf).write_bytes(b"?>");
                } else {
                    (*buf).write_bytes(b"<?");

                    (*buf).write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
                    if (*ctxt).format == 2 {
                        xml_output_buffer_write_ws_non_sig(&mut *ctxt, 0);
                    }
                    (*buf).write_bytes(b"?>");
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
                    (*buf).write_bytes(&(*ctxt).indent[..len]);
                }

                if !(*cur).content.is_null() {
                    (*buf).write_bytes(b"<!--");
                    (*buf).write_str(
                        CStr::from_ptr((*cur).content as _)
                            .to_string_lossy()
                            .as_ref(),
                    );
                    (*buf).write_bytes(b"-->");
                }
            }
            XmlElementType::XmlEntityRefNode => {
                (*buf).write_bytes(b"&");
                (*buf).write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
                (*buf).write_bytes(b";");
            }
            XmlElementType::XmlCDATASectionNode => {
                if (*cur).content.is_null() || *(*cur).content == b'\0' {
                    (*buf).write_bytes(b"<![CDATA[]]>");
                } else {
                    start = (*cur).content;
                    end = (*cur).content;
                    while *end != b'\0' {
                        if *end == b']' && *end.add(1) == b']' && *end.add(2) == b'>' {
                            end = end.add(2);
                            (*buf).write_bytes(b"<![CDATA[");
                            (*buf).write_bytes(from_raw_parts(
                                start as _,
                                end.offset_from(start) as _,
                            ));
                            (*buf).write_bytes(b"]]>");
                            start = end;
                        }
                        end = end.add(1);
                    }
                    if start != end {
                        (*buf).write_bytes(b"<![CDATA[");
                        (*buf).write_str(CStr::from_ptr(start as _).to_string_lossy().as_ref());
                        (*buf).write_bytes(b"]]>");
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
                    (*cur).typ,
                    XmlElementType::XmlXIncludeStart | XmlElementType::XmlXIncludeEnd
                )
            {
                (*buf).write_bytes(b"\n");
            }
            if !(*cur).next.is_null() {
                cur = (*cur).next;
                break;
            }

            cur = parent;
            /* (*cur).parent was validated when descending. */
            parent = (*cur).parent;

            if matches!((*cur).typ, XmlElementType::XmlElementNode) {
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
                    (*buf).write_bytes(&(*ctxt).indent[..len]);
                }

                (*buf).write_bytes(b"</");
                if !(*cur).ns.is_null() && !(*(*cur).ns).prefix.load(Ordering::Relaxed).is_null() {
                    (*buf).write_str(
                        CStr::from_ptr((*(*cur).ns).prefix.load(Ordering::Relaxed) as _)
                            .to_string_lossy()
                            .as_ref(),
                    );
                    (*buf).write_bytes(b":");
                }

                (*buf).write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
                if (*ctxt).format == 2 {
                    xml_output_buffer_write_ws_non_sig(&mut *ctxt, 0);
                }
                (*buf).write_bytes(b">");

                if cur == unformatted_node {
                    (*ctxt).format = format;
                    unformatted_node = null_mut();
                }
            }
        }
    }
}

/**
 * xmlDtdDumpOutput:
 * @buf:  the XML buffer output
 * @dtd:  the pointer to the DTD
 *
 * Dump the XML document DTD, if any.
 */
unsafe extern "C" fn xml_dtd_dump_output(ctxt: XmlSaveCtxtPtr, dtd: XmlDtdPtr) {
    let mut cur: XmlNodePtr;

    if dtd.is_null() {
        return;
    }
    if ctxt.is_null() || (*ctxt).buf.is_null() {
        return;
    }
    let buf: XmlOutputBufferPtr = (*ctxt).buf;
    (*buf).write_bytes(b"<!DOCTYPE ");

    (*buf).write_str(CStr::from_ptr((*dtd).name as _).to_string_lossy().as_ref());
    if !(*dtd).external_id.is_null() {
        (*buf).write_bytes(b" PUBLIC ");
        if let Some(mut buf) = (*buf).buffer {
            buf.push_quoted_cstr(CStr::from_ptr((*dtd).external_id as *const i8));
        }
        (*buf).write_bytes(b" ");
        if let Some(mut buf) = (*buf).buffer {
            buf.push_quoted_cstr(CStr::from_ptr((*dtd).system_id as *const i8));
        }
    } else if !(*dtd).system_id.is_null() {
        (*buf).write_bytes(b" SYSTEM ");
        if let Some(mut buf) = (*buf).buffer {
            buf.push_quoted_cstr(CStr::from_ptr((*dtd).system_id as *const i8));
        }
    }
    if (*dtd).entities.is_null()
        && (*dtd).elements.is_null()
        && (*dtd).attributes.is_null()
        && (*dtd).notations.is_null()
        && (*dtd).pentities.is_null()
    {
        (*buf).write_bytes(b">");
        return;
    }
    (*buf).write_bytes(b" [\n");
    /*
     * Dump the notations first they are not in the DTD children list
     * Do this only on a standalone DTD or on the internal subset though.
     */
    if !(*dtd).notations.is_null() && ((*dtd).doc.is_null() || (*(*dtd).doc).int_subset == dtd) {
        xml_buf_dump_notation_table(
            (*buf).buffer.map_or(null_mut(), |buf| buf.as_ptr()),
            (*dtd).notations as _,
        );
    }
    let format: i32 = (*ctxt).format;
    let level: i32 = (*ctxt).level;
    (*ctxt).format = 0;
    (*ctxt).level = -1;
    cur = (*dtd).children;
    while !cur.is_null() {
        xml_node_dump_output_internal(ctxt, cur);
        cur = (*cur).next;
    }
    (*ctxt).format = format;
    (*ctxt).level = level;
    (*buf).write_bytes(b"]>");
}

/**
 * xmlNsDumpOutputCtxt
 * @ctxt: the save context
 * @cur:  a namespace
 *
 * Dump a local Namespace definition to a save context.
 * Should be called in the context of attribute dumps.
 */
unsafe extern "C" fn xml_ns_dump_output_ctxt(ctxt: XmlSaveCtxtPtr, cur: XmlNsPtr) {
    xml_ns_dump_output((*ctxt).buf, cur, ctxt);
}

/**
 * xhtmlAttrListDumpOutput:
 * @cur:  the first attribute pointer
 *
 * Dump a list of XML attributes
 */
#[cfg(feature = "html")]
unsafe extern "C" fn xhtml_attr_list_dump_output(ctxt: XmlSaveCtxtPtr, mut cur: XmlAttrPtr) {
    use crate::tree::{xml_free_node, xml_new_doc_text};

    use super::htmltree::html_is_boolean_attr;

    let mut xml_lang: XmlAttrPtr = null_mut();
    let mut lang: XmlAttrPtr = null_mut();
    let mut name: XmlAttrPtr = null_mut();
    let mut id: XmlAttrPtr = null_mut();

    if cur.is_null() {
        return;
    }
    let buf: XmlOutputBufferPtr = (*ctxt).buf;
    let parent: XmlNodePtr = (*cur).parent;
    while !cur.is_null() {
        if (*cur).ns.is_null() && xml_str_equal((*cur).name, c"id".as_ptr() as _) {
            id = cur;
        } else if (*cur).ns.is_null() && xml_str_equal((*cur).name, c"name".as_ptr() as _) {
            name = cur;
        } else if (*cur).ns.is_null() && xml_str_equal((*cur).name, c"lang".as_ptr() as _) {
            lang = cur;
        } else if !(*cur).ns.is_null()
            && xml_str_equal((*cur).name, c"lang".as_ptr() as _)
            && xml_str_equal(
                (*(*cur).ns).prefix.load(Ordering::Relaxed),
                c"xml".as_ptr() as _,
            )
        {
            xml_lang = cur;
        } else if (*cur).ns.is_null()
            && ((*cur).children.is_null()
                || (*(*cur).children).content.is_null()
                || *(*(*cur).children).content.add(0) == 0)
            && html_is_boolean_attr((*cur).name) != 0
        {
            if !(*cur).children.is_null() {
                xml_free_node((*cur).children);
            }
            (*cur).children = xml_new_doc_text((*cur).doc, (*cur).name);
            if !(*cur).children.is_null() {
                (*(*cur).children).parent = cur as _;
            }
        }
        xml_attr_dump_output(ctxt, cur);
        cur = (*cur).next;
    }
    /*
     * C.8
     */
    if (!name.is_null() && id.is_null())
        && (!parent.is_null()
            && !(*parent).name.is_null()
            && (xml_str_equal((*parent).name, c"a".as_ptr() as _)
                || xml_str_equal((*parent).name, c"p".as_ptr() as _)
                || xml_str_equal((*parent).name, c"div".as_ptr() as _)
                || xml_str_equal((*parent).name, c"img".as_ptr() as _)
                || xml_str_equal((*parent).name, c"map".as_ptr() as _)
                || xml_str_equal((*parent).name, c"applet".as_ptr() as _)
                || xml_str_equal((*parent).name, c"form".as_ptr() as _)
                || xml_str_equal((*parent).name, c"frame".as_ptr() as _)
                || xml_str_equal((*parent).name, c"iframe".as_ptr() as _)))
    {
        (*buf).write_bytes(b" id=\"");
        xml_attr_serialize_content(buf, name);
        (*buf).write_bytes(b"\"");
    }
    /*
     * C.7.
     */
    if !lang.is_null() && xml_lang.is_null() {
        (*buf).write_bytes(b" xml:lang=\"");
        xml_attr_serialize_content(buf, lang);
        (*buf).write_bytes(b"\"");
    } else if !xml_lang.is_null() && lang.is_null() {
        (*buf).write_bytes(b" lang=\"");
        xml_attr_serialize_content(buf, xml_lang);
        (*buf).write_bytes(b"\"");
    }
}

const XHTML_NS_NAME: &str = "http://www.w3.org/1999/xhtml";

/**
 * xhtmlIsEmpty:
 * @node:  the node
 *
 * Check if a node is an empty xhtml node
 *
 * Returns 1 if the node is an empty node, 0 if not and -1 in case of error
 */
#[cfg(feature = "html")]
unsafe extern "C" fn xhtml_is_empty(node: XmlNodePtr) -> i32 {
    if node.is_null() {
        return -1;
    }
    if !matches!((*node).typ, XmlElementType::XmlElementNode) {
        return 0;
    }
    {
        let str2 = CString::new(XHTML_NS_NAME).unwrap();
        if !(*node).ns.is_null()
            && !xml_str_equal(
                (*(*node).ns).href.load(Ordering::Relaxed),
                str2.as_ptr() as _,
            )
        {
            return 0;
        }
    }
    if !(*node).children.is_null() {
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

/**
 * xhtmlNodeDumpOutput:
 * @buf:  the XML buffer output
 * @doc:  the XHTML document
 * @cur:  the current node
 * @level: the imbrication level for indenting
 * @format: is formatting allowed
 * @encoding:  an optional encoding string
 *
 * Dump an XHTML node, recursive behaviour, children are printed too.
 */
#[cfg(feature = "html")]
pub(crate) unsafe extern "C" fn xhtml_node_dump_output(ctxt: XmlSaveCtxtPtr, mut cur: XmlNodePtr) {
    use crate::libxml::{parser_internals::XML_STRING_TEXT, xmlstring::xml_strcasecmp};

    let format: i32 = (*ctxt).format;
    let mut addmeta: i32;
    let mut tmp: XmlNodePtr;

    let mut unformatted_node: XmlNodePtr = null_mut();
    let mut parent: XmlNodePtr;
    let mut start: *mut XmlChar;
    let mut end: *mut XmlChar;
    let buf: XmlOutputBufferPtr = (*ctxt).buf;

    if cur.is_null() {
        return;
    }

    let root: XmlNodePtr = cur;
    parent = (*cur).parent;
    loop {
        match (*cur).typ {
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
                if (*cur).parent == parent && !(*cur).children.is_null() {
                    parent = cur;
                    cur = (*cur).children;
                    continue;
                }
            }
            XmlElementType::XmlElementDecl => {
                xml_buf_dump_element_decl(
                    (*buf).buffer.map_or(null_mut(), |buf| buf.as_ptr()),
                    cur as _,
                );
            }
            XmlElementType::XmlAttributeDecl => {
                xml_buf_dump_attribute_decl(
                    (*buf).buffer.map_or(null_mut(), |buf| buf.as_ptr()),
                    cur as _,
                );
            }
            XmlElementType::XmlEntityDecl => {
                xml_buf_dump_entity_decl(
                    (*buf).buffer.map_or(null_mut(), |buf| buf.as_ptr()),
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
                    (*buf).write_bytes(&(*ctxt).indent[..len]);
                }

                /*
                 * Some users like lxml are known to pass nodes with a corrupted
                 * tree structure. Fall back to a recursive call to handle this
                 * case.
                 */
                if (*cur).parent != parent && !(*cur).children.is_null() {
                    xhtml_node_dump_output(ctxt, cur);
                    break;
                }

                (*buf).write_bytes(b"<");
                if !(*cur).ns.is_null() && !(*(*cur).ns).prefix.load(Ordering::Relaxed).is_null() {
                    (*buf).write_str(
                        CStr::from_ptr((*(*cur).ns).prefix.load(Ordering::Relaxed) as _)
                            .to_string_lossy()
                            .as_ref(),
                    );
                    (*buf).write_bytes(b":");
                }

                (*buf).write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
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

                    (*buf).write_str(" xmlns=\"http://www.w3.org/1999/xhtml\"");
                }
                if !(*cur).properties.is_null() {
                    xhtml_attr_list_dump_output(ctxt, (*cur).properties);
                }

                if !parent.is_null()
                    && ((*parent).parent == (*cur).doc as _)
                    && xml_str_equal((*cur).name, c"head".as_ptr() as _)
                    && xml_str_equal((*parent).name, c"html".as_ptr() as _)
                {
                    tmp = (*cur).children;
                    while !tmp.is_null() {
                        if xml_str_equal((*tmp).name, c"meta".as_ptr() as _) {
                            let httpequiv: *mut XmlChar =
                                (*tmp).get_prop(c"http-equiv".as_ptr() as _);
                            if !httpequiv.is_null() {
                                if xml_strcasecmp(httpequiv, c"Content-Type".as_ptr() as _) == 0 {
                                    xml_free(httpequiv as _);
                                    break;
                                }
                                xml_free(httpequiv as _);
                            }
                        }
                        tmp = (*tmp).next;
                    }
                    if tmp.is_null() {
                        addmeta = 1;
                    }
                }

                if (*cur).children.is_null() {
                    if ((*cur).ns.is_null()
                        || (*(*cur).ns).prefix.load(Ordering::Relaxed).is_null())
                        && (xhtml_is_empty(cur) == 1 && addmeta == 0)
                    {
                        /*
                         * C.2. Empty Elements
                         */
                        (*buf).write_bytes(b" />");
                    } else {
                        if addmeta == 1 {
                            (*buf).write_bytes(b">");
                            if (*ctxt).format == 1 {
                                (*buf).write_bytes(b"\n");
                                if get_indent_tree_output() != 0 {
                                    let len = (*ctxt).indent_size
                                        * if (*ctxt).level + 1 > (*ctxt).indent_nr as i32 {
                                            (*ctxt).indent_nr
                                        } else {
                                            ((*ctxt).level + 1) as usize
                                        };
                                    (*buf).write_bytes(&(*ctxt).indent[..len]);
                                }
                            }

                            (*buf).write_str(
                                "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=",
                            );
                            if let Some(encoding) = (*ctxt).encoding.as_deref() {
                                (*buf).write_str(encoding);
                            } else {
                                (*buf).write_bytes(b"UTF-8");
                            }
                            (*buf).write_bytes(b"\" />");
                            if (*ctxt).format == 1 {
                                (*buf).write_bytes(b"\n");
                            }
                        } else {
                            (*buf).write_bytes(b">");
                        }
                        /*
                         * C.3. Element Minimization and Empty Element Content
                         */
                        (*buf).write_bytes(b"</");
                        if !(*cur).ns.is_null()
                            && !(*(*cur).ns).prefix.load(Ordering::Relaxed).is_null()
                        {
                            (*buf).write_str(
                                CStr::from_ptr((*(*cur).ns).prefix.load(Ordering::Relaxed) as _)
                                    .to_string_lossy()
                                    .as_ref(),
                            );
                            (*buf).write_bytes(b":");
                        }

                        (*buf)
                            .write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
                        (*buf).write_bytes(b">");
                    }
                } else {
                    (*buf).write_bytes(b">");
                    if addmeta == 1 {
                        if (*ctxt).format == 1 {
                            (*buf).write_bytes(b"\n");
                            if get_indent_tree_output() != 0 {
                                let len = (*ctxt).indent_size
                                    * if (*ctxt).level + 1 > (*ctxt).indent_nr as i32 {
                                        (*ctxt).indent_nr
                                    } else {
                                        ((*ctxt).level + 1) as usize
                                    };
                                (*buf).write_bytes(&(*ctxt).indent[..len]);
                            }
                        }

                        (*buf).write_str(
                            "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=",
                        );
                        if let Some(encoding) = (*ctxt).encoding.as_deref() {
                            (*buf).write_str(encoding);
                        } else {
                            (*buf).write_bytes(b"UTF-8");
                        }
                        (*buf).write_bytes(b"\" />");
                    }

                    if (*ctxt).format == 1 {
                        tmp = (*cur).children;
                        while !tmp.is_null() {
                            if (matches!((*tmp).typ, XmlElementType::XmlTextNode)
                                || matches!((*tmp).typ, XmlElementType::XmlEntityRefNode))
                            {
                                unformatted_node = cur;
                                (*ctxt).format = 0;
                                break;
                            }
                            tmp = (*tmp).next;
                        }
                    }

                    if (*ctxt).format == 1 {
                        (*buf).write_bytes(b"\n");
                    }
                    if (*ctxt).level >= 0 {
                        (*ctxt).level += 1;
                    }
                    parent = cur;
                    cur = (*cur).children;
                    continue;
                }
            }
            XmlElementType::XmlTextNode => {
                if (*cur).content.is_null() {
                    break;
                }
                if (*cur).name == XML_STRING_TEXT.as_ptr() as _
                    || (*cur).name != XML_STRING_TEXT_NOENC.as_ptr() as _
                {
                    (*buf).write_str_with_escape(
                        CStr::from_ptr((*cur).content as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                        (*ctxt).escape,
                    );
                } else {
                    /*
                     * Disable escaping, needed for XSLT
                     */

                    (*buf).write_str(
                        CStr::from_ptr((*cur).content as _)
                            .to_string_lossy()
                            .as_ref(),
                    );
                }
            }
            XmlElementType::XmlPINode => {
                if !(*cur).content.is_null() {
                    (*buf).write_bytes(b"<?");

                    (*buf).write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
                    if !(*cur).content.is_null() {
                        (*buf).write_bytes(b" ");

                        (*buf).write_str(
                            CStr::from_ptr((*cur).content as _)
                                .to_string_lossy()
                                .as_ref(),
                        );
                    }
                    (*buf).write_bytes(b"?>");
                } else {
                    (*buf).write_bytes(b"<?");

                    (*buf).write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
                    (*buf).write_bytes(b"?>");
                }
            }
            XmlElementType::XmlCommentNode => {
                if !(*cur).content.is_null() {
                    (*buf).write_bytes(b"<!--");

                    (*buf).write_str(
                        CStr::from_ptr((*cur).content as _)
                            .to_string_lossy()
                            .as_ref(),
                    );
                    (*buf).write_bytes(b"-->");
                }
            }
            XmlElementType::XmlEntityRefNode => {
                (*buf).write_bytes(b"&");

                (*buf).write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
                (*buf).write_bytes(b";");
            }
            XmlElementType::XmlCDATASectionNode => {
                if (*cur).content.is_null() || *(*cur).content == b'\0' {
                    (*buf).write_bytes(b"<![CDATA[]]>");
                } else {
                    start = (*cur).content;
                    end = (*cur).content;
                    while *end != b'\0' {
                        if *end == b']' && *end.add(1) == b']' && *end.add(2) == b'>' {
                            end = end.add(2);
                            (*buf).write_bytes(b"<![CDATA[");

                            (*buf).write_bytes(from_raw_parts(
                                start as _,
                                end.offset_from(start) as _,
                            ));
                            (*buf).write_bytes(b"]]>");
                            start = end;
                        }
                        end = end.add(1);
                    }
                    if start != end {
                        (*buf).write_bytes(b"<![CDATA[");

                        (*buf).write_str(CStr::from_ptr(start as _).to_string_lossy().as_ref());
                        (*buf).write_bytes(b"]]>");
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
                (*buf).write_bytes(b"\n");
            }
            if !(*cur).next.is_null() {
                cur = (*cur).next;
                break;
            }

            cur = parent;
            /* (*cur).parent was validated when descending. */
            parent = (*cur).parent;

            if matches!((*cur).typ, XmlElementType::XmlElementNode) {
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
                    (*buf).write_bytes(&(*ctxt).indent[..len]);
                }

                (*buf).write_bytes(b"</");
                if !(*cur).ns.is_null() && !(*(*cur).ns).prefix.load(Ordering::Relaxed).is_null() {
                    (*buf).write_str(
                        CStr::from_ptr((*(*cur).ns).prefix.load(Ordering::Relaxed) as _)
                            .to_string_lossy()
                            .as_ref(),
                    );
                    (*buf).write_bytes(b":");
                }

                (*buf).write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
                (*buf).write_bytes(b">");

                if cur == unformatted_node {
                    (*ctxt).format = format;
                    unformatted_node = null_mut();
                }
            }
        }
    }
}

unsafe extern "C" fn xml_save_clear_encoding(ctxt: XmlSaveCtxtPtr) -> i32 {
    let buf: XmlOutputBufferPtr = (*ctxt).buf;
    (*buf).flush();
    let _ = (*buf).encoder.take();
    if let Some(conv) = (*buf).conv.take() {
        conv.free();
    }
    0
}

/**
 * xmlDocContentDumpOutput:
 * @cur:  the document
 *
 * Dump an XML document.
 */
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
    let buf: XmlOutputBufferPtr = (*ctxt).buf;
    let mut switched_encoding: i32 = 0;

    xml_init_parser();

    if !matches!(
        (*cur).typ,
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

    if (matches!((*cur).typ, XmlElementType::XmlHTMLDocumentNode)
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
                && (*buf).encoder.is_none()
                && (*buf).conv.is_none())
                && xml_save_switch_encoding(&mut *ctxt, encoding.as_deref().unwrap()) < 0
            {
                (*cur).encoding = oldenc;
                return -1;
            }
            if (*ctxt).options & XmlSaveOption::XmlSaveFormat as i32 != 0 {
                html_doc_content_dump_format_output(buf, cur, encoding.as_deref(), 1);
            } else {
                html_doc_content_dump_format_output(buf, cur, encoding.as_deref(), 0);
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
    } else if matches!((*cur).typ, XmlElementType::XmlDocumentNode)
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
            && (*buf).encoder.is_none()
            && (*buf).conv.is_none()
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
            (*buf).write_bytes(b"<?xml version=");
            if let Some(version) = (*cur).version.as_deref() {
                if let Some(mut buf) = (*buf).buffer {
                    let version = CString::new(version).unwrap();
                    buf.push_quoted_cstr(&version);
                }
            } else {
                (*buf).write_bytes(b"\"1.0\"");
            }
            if let Some(encoding) = encoding.as_deref() {
                (*buf).write_bytes(b" encoding=");
                if let Some(mut buf) = (*buf).buffer {
                    let enc = CString::new(encoding).unwrap();
                    buf.push_quoted_cstr(enc.as_c_str());
                }
            }
            match (*cur).standalone {
                0 => {
                    (*buf).write_bytes(b" standalone=\"no\"");
                }
                1 => {
                    (*buf).write_bytes(b" standalone=\"yes\"");
                }
                _ => {}
            }
            (*buf).write_bytes(b"?>\n");
        }

        #[cfg(feature = "html")]
        {
            if (*ctxt).options & XmlSaveOption::XmlSaveXHTML as i32 != 0 {
                is_html = true;
            }
            if (*ctxt).options & XmlSaveOption::XmlSaveNoXHTML as i32 == 0 {
                dtd = (*cur).get_int_subset();
                if !dtd.is_null() {
                    let system_id = (!(*dtd).system_id.is_null()).then(|| {
                        CStr::from_ptr((*dtd).system_id as *const i8)
                            .to_string_lossy()
                            .into_owned()
                    });
                    let external_id = (!(*dtd).external_id.is_null()).then(|| {
                        CStr::from_ptr((*dtd).external_id as *const i8)
                            .to_string_lossy()
                            .into_owned()
                    });
                    is_html = is_xhtml(system_id.as_deref(), external_id.as_deref());
                }
            }
        }
        if !(*cur).children.is_null() {
            let mut child: XmlNodePtr = (*cur).children;

            while !child.is_null() {
                (*ctxt).level = 0;
                #[cfg(feature = "html")]
                {
                    if is_html {
                        xhtml_node_dump_output(ctxt, child);
                    } else {
                        xml_node_dump_output_internal(ctxt, child);
                    }
                }
                #[cfg(not(feature = "html"))]
                {
                    xml_node_dump_output_internal(ctxt, child);
                }
                if !matches!(
                    (*child).typ,
                    XmlElementType::XmlXIncludeStart | XmlElementType::XmlXIncludeEnd
                ) {
                    (*buf).write_bytes(b"\n");
                }
                child = (*child).next;
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

/**
 * xmlFreeSaveCtxt:
 *
 * Free a saving context, destroying the output in any remaining buffer
 */
unsafe extern "C" fn xml_free_save_ctxt(ctxt: XmlSaveCtxtPtr) {
    if ctxt.is_null() {
        return;
    }
    (*ctxt).encoding = None;
    if !(*ctxt).buf.is_null() {
        xml_output_buffer_close((*ctxt).buf);
    }
    xml_free(ctxt as _);
}

/**
 * xmlNewSaveCtxt:
 *
 * Create a new saving context
 *
 * Returns the new structure or NULL in case of error
 */
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

/**
 * xmlSaveToFilename:
 * @filename:  a file name or an URL
 * @encoding:  the encoding name to use or NULL
 * @options:  a set of xmlSaveOptions
 *
 * Create a document saving context serializing to a filename or possibly
 * to an URL (but this is less reliable) with the encoding and the options
 * given.
 *
 * Returns a new serialization context or NULL in case of error.
 */
pub unsafe fn xml_save_to_filename(
    filename: *const c_char,
    encoding: Option<&str>,
    options: i32,
) -> XmlSaveCtxtPtr {
    let compression: i32 = 0; /* TODO handle compression option */

    let ret: XmlSaveCtxtPtr = xml_new_save_ctxt(encoding, options);
    if ret.is_null() {
        return null_mut();
    }
    (*ret).buf = xml_output_buffer_create_filename(filename, (*ret).handler.clone(), compression);
    if (*ret).buf.is_null() {
        xml_free_save_ctxt(ret);
        return null_mut();
    }
    ret
}

/**
 * xmlSaveToIO:
 * @iowrite:  an I/O write function
 * @ioclose:  an I/O close function
 * @ioctx:  an I/O handler
 * @encoding:  the encoding name to use or NULL
 * @options:  a set of xmlSaveOptions
 *
 * Create a document saving context serializing to a file descriptor
 * with the encoding and the options given
 *
 * Returns a new serialization context or NULL in case of error.
 */
pub unsafe fn xml_save_to_io(
    iowrite: Option<XmlOutputWriteCallback>,
    ioclose: Option<XmlOutputCloseCallback>,
    ioctx: *mut c_void,
    encoding: Option<&str>,
    options: i32,
) -> XmlSaveCtxtPtr {
    let ret: XmlSaveCtxtPtr = xml_new_save_ctxt(encoding, options);
    if ret.is_null() {
        return null_mut();
    }
    (*ret).buf = xml_output_buffer_create_io(iowrite, ioclose, ioctx, (*ret).handler.clone());
    if (*ret).buf.is_null() {
        xml_free_save_ctxt(ret);
        return null_mut();
    }
    ret
}

/**
 * xmlSaveDoc:
 * @ctxt:  a document saving context
 * @doc:  a document
 *
 * Save a full document to a saving context
 * TODO: The function is not fully implemented yet as it does not return the
 * byte count but 0 instead
 *
 * Returns the number of byte written or -1 in case of error
 */
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

/**
 * htmlNodeDumpOutputInternal:
 * @cur:  the current node
 *
 * Dump an HTML node, recursive behaviour, children are printed too.
 */
#[cfg(feature = "html")]
unsafe extern "C" fn html_node_dump_output_internal(ctxt: XmlSaveCtxtPtr, cur: XmlNodePtr) -> i32 {
    use super::htmltree::html_node_dump_format_output;

    let mut oldenc = None;
    let oldctxtenc = (*ctxt).encoding.clone();
    let mut encoding = (*ctxt).encoding.clone();
    let buf: XmlOutputBufferPtr = (*ctxt).buf;
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
        && (*buf).encoder.is_none()
        && (*buf).conv.is_none()
    {
        if xml_save_switch_encoding(&mut *ctxt, encoding.as_deref().unwrap()) < 0 {
            (*doc).encoding = oldenc;
            return -1;
        }
        switched_encoding = 1;
    }
    if (*ctxt).options & XmlSaveOption::XmlSaveFormat as i32 != 0 {
        html_node_dump_format_output(buf, doc, cur, encoding.as_deref(), 1);
    } else {
        html_node_dump_format_output(buf, doc, cur, encoding.as_deref(), 0);
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

/**
 * xmlSaveTree:
 * @ctxt:  a document saving context
 * @node:  the top node of the subtree to save
 *
 * Save a subtree starting at the node parameter to a saving context
 * TODO: The function is not fully implemented yet as it does not return the
 * byte count but 0 instead
 *
 * Returns the number of byte written or -1 in case of error
 */
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
        if (!matches!((*node).typ, XmlElementType::XmlNamespaceDecl)
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

/**
 * xmlSaveFlush:
 * @ctxt:  a document saving context
 *
 * Flush a document saving context, i.e. make sure that all bytes have
 * been output.
 *
 * Returns the number of byte written or -1 in case of error.
 */
pub unsafe extern "C" fn xml_save_flush(ctxt: XmlSaveCtxtPtr) -> i32 {
    if ctxt.is_null() {
        return -1;
    }
    if (*ctxt).buf.is_null() {
        return -1;
    }
    (*(*ctxt).buf).flush()
}

/**
 * xmlSaveClose:
 * @ctxt:  a document saving context
 *
 * Close a document saving context, i.e. make sure that all bytes have
 * been output and free the associated data.
 *
 * Returns the number of byte written or -1 in case of error.
 */
pub unsafe extern "C" fn xml_save_close(ctxt: XmlSaveCtxtPtr) -> i32 {
    if ctxt.is_null() {
        return -1;
    }
    let ret: i32 = xml_save_flush(ctxt);
    xml_free_save_ctxt(ctxt);
    ret
}

/**
 * xmlSaveSetEscape:
 * @ctxt:  a document saving context
 * @escape:  the escaping function
 *
 * Set a custom escaping function to be used for text in element content
 *
 * Returns 0 if successful or -1 in case of error.
 */
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

/**
 * xmlSaveSetAttrEscape:
 * @ctxt:  a document saving context
 * @escape:  the escaping function
 *
 * Set a custom escaping function to be used for text in attribute content
 *
 * Returns 0 if successful or -1 in case of error.
 */
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

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_save_close() {
        #[cfg(feature = "output")]
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
        #[cfg(feature = "output")]
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
        #[cfg(feature = "output")]
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
        #[cfg(feature = "output")]
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
