//! Provide methods and data structures for handling HTML tree.  
//! This module is based on `libxml/HTMLtree.h`, `HTMLtree.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: specific APIs to process HTML tree, especially serialization
// Description: this module implements a few function needed to process
//              tree in an HTML specific way.
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// HTMLtree.c : implementation of access function for an HTML tree.
//
// See Copyright for the status of this software.
//
// daniel@veillard.com

#[cfg(feature = "libxml_output")]
use std::io::Write;
use std::{
    ffi::{c_char, CStr, CString},
    mem::size_of,
    ptr::{null, null_mut},
    sync::atomic::Ordering,
};

use libc::memset;

use crate::{
    encoding::XmlCharEncoding,
    tree::{
        xml_create_int_subset, xml_free_node, xml_new_doc_node, xml_new_prop, NodeCommon, XmlAttr,
        XmlAttrPtr, XmlDoc, XmlDocProperties, XmlElementType, XmlNode, __XML_REGISTER_CALLBACKS,
    },
};
#[cfg(feature = "libxml_output")]
use crate::{error::XmlParserErrors, io::XmlOutputBuffer};

use super::{
    globals::{xml_malloc, xml_register_node_default_value},
    htmlparser::{html_err_memory, HtmlDocPtr, HtmlNodePtr},
    xmlstring::{xml_str_equal, xml_strcasecmp, xml_strstr, XmlChar},
};

/// Macro. A text node in a HTML document is really implemented
/// the same way as a text node in an XML document.
const HTML_TEXT_NODE: XmlElementType = XmlElementType::XmlTextNode;
/// Macro. An entity reference in a HTML document is really implemented
/// the same way as an entity reference in an XML document.
const HTML_ENTITY_REF_NODE: XmlElementType = XmlElementType::XmlEntityRefNode;
/// Macro. A comment in a HTML document is really implemented
/// the same way as a comment in an XML document.
const HTML_COMMENT_NODE: XmlElementType = XmlElementType::XmlCommentNode;
/// Macro. A preserved node in a HTML document is really implemented
/// the same way as a CDATA section in an XML document.
const HTML_PRESERVE_NODE: XmlElementType = XmlElementType::XmlCDATASectionNode;
/// Macro. A processing instruction in a HTML document is really implemented
/// the same way as a processing instruction in an XML document.
const HTML_PI_NODE: XmlElementType = XmlElementType::XmlPINode;

/// Creates a new HTML document
///
/// Returns a new document
#[doc(alias = "htmlNewDoc")]
pub unsafe fn html_new_doc(uri: *const XmlChar, external_id: *const XmlChar) -> HtmlDocPtr {
    if uri.is_null() && external_id.is_null() {
        return html_new_doc_no_dtd(
            c"http://www.w3.org/TR/REC-html40/loose.dtd".as_ptr() as _,
            c"-//W3C//DTD HTML 4.0 Transitional//EN".as_ptr() as _,
        );
    }

    html_new_doc_no_dtd(uri, external_id)
}

/// Creates a new HTML document without a DTD node if `uri` and `external_id` are NULL.
///
/// Returns a new document, do not initialize the DTD if not provided
#[doc(alias = "htmlNewDocNoDtD")]
pub unsafe fn html_new_doc_no_dtd(uri: *const XmlChar, external_id: *const XmlChar) -> HtmlDocPtr {
    // Allocate a new document and fill the fields.
    let cur: *mut XmlDoc = xml_malloc(size_of::<XmlDoc>()) as *mut XmlDoc;
    if cur.is_null() {
        html_err_memory(null_mut(), Some("HTML document creation failed\n"));
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlDoc>());
    std::ptr::write(&mut *cur, XmlDoc::default());

    (*cur).typ = XmlElementType::XmlHTMLDocumentNode;
    (*cur).version = None;
    (*cur).int_subset = None;
    (*cur).doc = cur;
    (*cur).name = null_mut();
    (*cur).children = None;
    (*cur).ext_subset = None;
    (*cur).old_ns = None;
    (*cur).encoding = None;
    (*cur).standalone = 1;
    (*cur).compression = 0;
    (*cur).ids = None;
    (*cur).refs = None;
    (*cur)._private = null_mut();
    (*cur).charset = XmlCharEncoding::UTF8;
    (*cur).properties =
        XmlDocProperties::XmlDocHTML as i32 | XmlDocProperties::XmlDocUserbuilt as i32;
    if !external_id.is_null() || !uri.is_null() {
        xml_create_int_subset(
            cur,
            Some("html"),
            (!external_id.is_null())
                .then(|| CStr::from_ptr(external_id as *const i8).to_string_lossy())
                .as_deref(),
            (!uri.is_null())
                .then(|| CStr::from_ptr(uri as *const i8).to_string_lossy())
                .as_deref(),
        );
    }
    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    /* && xmlRegisterNodeDefaultValue() */
    {
        xml_register_node_default_value(cur as *mut XmlNode);
    }
    cur
}

/// Encoding definition lookup in the Meta tags
///
/// Returns the current encoding as flagged in the HTML source
#[doc(alias = "htmlGetMetaEncoding")]
pub unsafe fn html_get_meta_encoding(doc: HtmlDocPtr) -> Option<String> {
    let mut cur: HtmlNodePtr;
    let mut content: *const XmlChar;
    let mut encoding: *const XmlChar;

    if doc.is_null() {
        return None;
    }
    cur = (*doc).children.map_or(null_mut(), |c| c.as_ptr());

    // Search the html
    'goto_found_meta: {
        'goto_found_head: {
            while !cur.is_null() {
                if matches!((*cur).element_type(), XmlElementType::XmlElementNode)
                    && !(*cur).name.is_null()
                {
                    if xml_str_equal((*cur).name, c"html".as_ptr() as _) {
                        break;
                    }
                    if xml_str_equal((*cur).name, c"head".as_ptr() as _) {
                        break 'goto_found_head;
                    }
                    if xml_str_equal((*cur).name, c"meta".as_ptr() as _) {
                        break 'goto_found_meta;
                    }
                }
                cur = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
            }
            if cur.is_null() {
                return None;
            }
            cur = (*cur).children().map_or(null_mut(), |c| c.as_ptr());
            // Search the head
            while !cur.is_null() {
                if matches!((*cur).element_type(), XmlElementType::XmlElementNode)
                    && !(*cur).name.is_null()
                {
                    if xml_str_equal((*cur).name, c"head".as_ptr() as _) {
                        break;
                    }
                    if xml_str_equal((*cur).name, c"meta".as_ptr() as _) {
                        break 'goto_found_meta;
                    }
                }
                cur = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
            }
            if cur.is_null() {
                return None;
            }
        }
        // found_head:
        cur = (*cur).children().map_or(null_mut(), |c| c.as_ptr());
    }

    // Search the meta elements

    // found_meta:
    while !cur.is_null() {
        if (matches!((*cur).element_type(), XmlElementType::XmlElementNode)
            && !(*cur).name.is_null())
            && xml_str_equal((*cur).name, c"meta".as_ptr() as _)
        {
            let mut attr = XmlAttrPtr::from_raw((*cur).properties).unwrap();
            let mut http = 0;
            let mut value: *const XmlChar;

            content = null_mut();
            while let Some(now) = attr {
                if let Some(children) = now.children.filter(|c| {
                    matches!(c.element_type(), XmlElementType::XmlTextNode) && c.next.is_none()
                }) {
                    value = children.content;
                    if xml_strcasecmp(now.name, c"http-equiv".as_ptr() as _) == 0
                        && xml_strcasecmp(value, c"Content-Type".as_ptr() as _) == 0
                    {
                        http = 1;
                    } else if !value.is_null()
                        && xml_strcasecmp(now.name, c"content".as_ptr() as _) == 0
                    {
                        content = value;
                    }
                    if http != 0 && !content.is_null() {
                        // goto found_content;
                        encoding = xml_strstr(content, c"charset=".as_ptr() as _);
                        if encoding.is_null() {
                            encoding = xml_strstr(content, c"Charset=".as_ptr() as _);
                        }
                        if encoding.is_null() {
                            encoding = xml_strstr(content, c"CHARSET=".as_ptr() as _);
                        }
                        if !encoding.is_null() {
                            encoding = encoding.add(8);
                        } else {
                            encoding = xml_strstr(content, c"charset =".as_ptr() as _);
                            if encoding.is_null() {
                                encoding = xml_strstr(content, c"Charset =".as_ptr() as _);
                            }
                            if encoding.is_null() {
                                encoding = xml_strstr(content, c"CHARSET =".as_ptr() as _);
                            }
                            if !encoding.is_null() {
                                encoding = encoding.add(9);
                            }
                        }
                        if !encoding.is_null() {
                            while *encoding == b' ' || *encoding == b'\t' {
                                encoding = encoding.add(1);
                            }
                        }
                        return Some(
                            CStr::from_ptr(encoding as *const i8)
                                .to_string_lossy()
                                .into_owned(),
                        );
                    }
                }
                attr = XmlAttrPtr::from_raw(now.next).unwrap();
            }
        }
        cur = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
    }
    None
}

/// Sets the current encoding in the Meta tags
///
/// # Note
/// This will not change the document content encoding, just the META flag associated.
///
/// Returns 0 in case of success and -1 in case of error
#[doc(alias = "htmlSetMetaEncoding")]
pub unsafe fn html_set_meta_encoding(doc: HtmlDocPtr, encoding: Option<&str>) -> i32 {
    let mut cur: HtmlNodePtr;
    let mut meta: HtmlNodePtr = null_mut();
    let mut head: HtmlNodePtr = null_mut();

    if doc.is_null() {
        return -1;
    }

    // html isn't a real encoding it's just libxml2 way to get entities
    if encoding.as_ref().map(|e| e.to_ascii_lowercase()).as_deref() == Some("html") {
        return -1;
    }

    let newcontent = if let Some(encoding) = encoding {
        format!("text/html; charset={encoding}")
    } else {
        String::new()
    };

    cur = (*doc).children.map_or(null_mut(), |c| c.as_ptr());

    let mut found_head = false;
    let mut found_meta = false;
    // Search the html
    while !cur.is_null() {
        if matches!((*cur).element_type(), XmlElementType::XmlElementNode) && !(*cur).name.is_null()
        {
            if xml_strcasecmp((*cur).name, c"html".as_ptr() as _) == 0 {
                break;
            }
            if xml_strcasecmp((*cur).name, c"head".as_ptr() as _) == 0 {
                // goto found_head;
                found_head = true;
                break;
            }
            if xml_strcasecmp((*cur).name, c"meta".as_ptr() as _) == 0 {
                // goto found_meta;
                found_meta = true;
                break;
            }
        }

        cur = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
    }

    if !found_head && !found_meta {
        if cur.is_null() {
            return -1;
        }
        cur = (*cur).children().map_or(null_mut(), |c| c.as_ptr());

        // Search the head
        while !cur.is_null() {
            if matches!((*cur).element_type(), XmlElementType::XmlElementNode)
                && !(*cur).name.is_null()
            {
                if xml_strcasecmp((*cur).name, c"head".as_ptr() as _) == 0 {
                    break;
                }
                if xml_strcasecmp((*cur).name, c"meta".as_ptr() as _) == 0 {
                    head = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
                    // goto found_meta;
                    found_meta = true;
                }
            }
            cur = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
        }
        if cur.is_null() {
            return -1;
        }
    }

    let create = |mut meta: *mut crate::tree::XmlNode,
                  encoding: Option<&str>,
                  head: *mut crate::tree::XmlNode,
                  newcontent: &str,
                  content: Option<&str>| {
        if meta.is_null() {
            if encoding.is_some() && !head.is_null() {
                // Create a new Meta element with the right attributes
                meta = xml_new_doc_node(doc, None, "meta", null_mut());
                if let Some(mut children) = (*head).children() {
                    children.add_prev_sibling(meta);
                } else {
                    (*head).add_child(meta);
                }
                xml_new_prop(
                    meta,
                    c"http-equiv".as_ptr() as _,
                    c"Content-Type".as_ptr() as _,
                );
                let newcontent = CString::new(newcontent).unwrap();
                xml_new_prop(meta, c"content".as_ptr() as _, newcontent.as_ptr() as _);
            }
        } else {
            // remove the meta tag if NULL is passed
            if encoding.is_none() {
                (*meta).unlink();
                xml_free_node(meta);
            }
            // change the document only if there is a real encoding change
            else if content.map_or(true, |c| {
                !c.to_ascii_lowercase()
                    .contains(&encoding.unwrap().to_ascii_lowercase())
            }) {
                (*meta).set_prop("content", Some(newcontent));
            }
        }

        0
    };

    // found_head:

    if !found_meta {
        head = cur;
        assert!(!cur.is_null());
        let Some(children) = (*cur).children() else {
            // goto create;
            return create(meta, encoding, head, &newcontent, None);
        };
        cur = children.as_ptr();
    }

    // found_meta:
    // Search and update all the remaining the meta elements carrying
    // encoding information
    let mut content = None;
    while !cur.is_null() {
        if (matches!((*cur).element_type(), XmlElementType::XmlElementNode)
            && !(*cur).name.is_null())
            && (xml_strcasecmp((*cur).name, c"meta".as_ptr() as _) == 0)
        {
            let mut attr = XmlAttrPtr::from_raw((*cur).properties).unwrap();
            let mut http = 0;
            let mut value: *const XmlChar;

            content = None;
            while let Some(now) = attr {
                if let Some(children) = now.children.filter(|c| {
                    matches!(c.element_type(), XmlElementType::XmlTextNode) && c.next.is_none()
                }) {
                    value = children.content;
                    if xml_strcasecmp(now.name, c"http-equiv".as_ptr() as _) == 0
                        && xml_strcasecmp(value, c"Content-Type".as_ptr() as _) == 0
                    {
                        http = 1;
                    } else if !value.is_null()
                        && xml_strcasecmp(now.name, c"content".as_ptr() as _) == 0
                    {
                        content = Some(
                            CStr::from_ptr(value as *const i8)
                                .to_string_lossy()
                                .into_owned(),
                        );
                    }
                    if http != 0 && content.is_some() {
                        break;
                    }
                }
                attr = XmlAttrPtr::from_raw(now.next).unwrap();
            }
            if http != 0 && content.is_some() {
                meta = cur;
                break;
            }
        }
        cur = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
    }
    // create:
    create(meta, encoding, head, &newcontent, content.as_deref())
}

/// Dump an HTML document in memory and return the xmlChar * and it's size.  
/// It's up to the caller to free the memory.
#[doc(alias = "htmlDocDumpMemory")]
#[cfg(feature = "libxml_output")]
pub unsafe fn html_doc_dump_memory(cur: *mut XmlDoc, mem: *mut *mut XmlChar, size: *mut i32) {
    html_doc_dump_memory_format(cur, mem, size, 1);
}

/// Handle an out of memory condition
#[doc(alias = "htmlSaveErr")]
#[cfg(feature = "libxml_output")]
unsafe fn html_save_err(code: XmlParserErrors, node: *mut XmlNode, extra: Option<&str>) {
    use std::borrow::Cow;

    use crate::error::__xml_simple_error;

    let msg: Cow<'static, str> = match code {
        XmlParserErrors::XmlSaveNotUTF8 => "string is not in UTF-8\n".into(),
        XmlParserErrors::XmlSaveCharInvalid => "invalid character value\n".into(),
        XmlParserErrors::XmlSaveUnknownEncoding => {
            format!("unknown encoding {}\n", extra.expect("Internal Error")).into()
        }
        XmlParserErrors::XmlSaveNoDoctype => "HTML has no DOCTYPE\n".into(),
        _ => "unexpected error number\n".into(),
    };
    __xml_simple_error!(XmlErrorDomain::XmlFromOutput, code, node, msg.as_ref());
}

/// Dump an HTML document in memory and return the xmlChar * and it's size.  
/// It's up to the caller to free the memory.
#[doc(alias = "htmlDocDumpMemoryFormat")]
#[cfg(feature = "libxml_output")]
pub unsafe fn html_doc_dump_memory_format(
    cur: *mut XmlDoc,
    mem: *mut *mut XmlChar,
    size: *mut i32,
    format: i32,
) {
    use std::{cell::RefCell, rc::Rc};

    use crate::{
        encoding::{find_encoding_handler, XmlCharEncoding},
        libxml::{parser::xml_init_parser, xmlstring::xml_strndup},
    };

    xml_init_parser();

    if mem.is_null() || size.is_null() {
        return;
    }
    if cur.is_null() {
        *mem = null_mut();
        *size = 0;
        return;
    }

    let handler = if let Some(enc) = html_get_meta_encoding(cur) {
        let e = enc.parse::<XmlCharEncoding>();
        if !matches!(e, Ok(XmlCharEncoding::UTF8)) {
            let handler = find_encoding_handler(&enc);
            if handler.is_none() {
                html_save_err(
                    XmlParserErrors::XmlSaveUnknownEncoding,
                    null_mut(),
                    Some(&enc),
                );
            }
            handler
        } else {
            None
        }
    } else if let Some(handler) = find_encoding_handler("HTML") {
        Some(handler)
    } else {
        find_encoding_handler("ascii")
    };

    let Some(mut buf) =
        XmlOutputBuffer::from_wrapped_encoder(handler.map(|e| Rc::new(RefCell::new(e))))
    else {
        *mem = null_mut();
        *size = 0;
        return;
    };

    html_doc_content_dump_format_output(&mut buf, cur, None, format);

    buf.flush();
    if let Some(conv) = buf.conv {
        *size = conv.len() as i32;
        *mem = xml_strndup(
            if conv.is_ok() {
                conv.as_ref().as_ptr()
            } else {
                null()
            },
            *size,
        );
    } else {
        *size = buf.buffer.map_or(0, |buf| buf.len() as i32);
        *mem = xml_strndup(
            buf.buffer.map_or(null(), |buf| {
                if buf.is_ok() {
                    buf.as_ref().as_ptr()
                } else {
                    null()
                }
            }),
            *size,
        );
    }
    buf.flush();
}

/// Dump an HTML document to an open FILE.
///
/// returns: the number of byte written or -1 in case of failure.
#[doc(alias = "htmlDocDump")]
#[cfg(feature = "libxml_output")]
pub unsafe fn html_doc_dump<'a>(f: &mut (impl Write + 'a), cur: *mut XmlDoc) -> i32 {
    use crate::{
        encoding::{find_encoding_handler, XmlCharEncoding},
        libxml::parser::xml_init_parser,
    };

    xml_init_parser();

    if cur.is_null() {
        return -1;
    }

    let handler = if let Some(enc) = html_get_meta_encoding(cur) {
        let e = enc.parse::<XmlCharEncoding>();
        if !matches!(e, Ok(XmlCharEncoding::UTF8)) {
            let handler = find_encoding_handler(&enc);
            if handler.is_none() {
                html_save_err(
                    XmlParserErrors::XmlSaveUnknownEncoding,
                    null_mut(),
                    Some(&enc),
                );
            }
            handler
        } else {
            None
        }
    } else if let Some(handler) = find_encoding_handler("HTML") {
        Some(handler)
    } else {
        find_encoding_handler("ascii")
    };

    let Some(mut buf) = XmlOutputBuffer::from_writer(f, handler) else {
        return -1;
    };
    html_doc_content_dump_output(&mut buf, cur, null_mut());

    if buf.error.is_ok() {
        buf.flush();
        buf.written
    } else {
        -1
    }
}

/// Dump an HTML document to a file. If `filename` is `"-"` the stdout file is used.
///
/// returns: the number of byte written or -1 in case of failure.
#[doc(alias = "htmlSaveFile")]
#[cfg(feature = "libxml_output")]
pub unsafe fn html_save_file(filename: *const c_char, cur: *mut XmlDoc) -> i32 {
    use std::{cell::RefCell, rc::Rc};

    use crate::{
        encoding::{find_encoding_handler, XmlCharEncoding},
        libxml::parser::xml_init_parser,
    };

    if cur.is_null() || filename.is_null() {
        return -1;
    }

    xml_init_parser();

    let handler = if let Some(enc) = html_get_meta_encoding(cur) {
        let e = enc.parse::<XmlCharEncoding>();
        if !matches!(e, Ok(XmlCharEncoding::UTF8)) {
            let handler = find_encoding_handler(&enc);
            if handler.is_none() {
                html_save_err(
                    XmlParserErrors::XmlSaveUnknownEncoding,
                    null_mut(),
                    Some(&enc),
                );
            }
            handler
        } else {
            None
        }
    } else if let Some(handler) = find_encoding_handler("HTML") {
        Some(handler)
    } else {
        find_encoding_handler("ascii")
    };

    // save the content to a temp buffer.
    let filename = CStr::from_ptr(filename).to_string_lossy();
    let Some(mut buf) = XmlOutputBuffer::from_uri(
        filename.as_ref(),
        handler.map(|e| Rc::new(RefCell::new(e))),
        (*cur).compression,
    ) else {
        return 0;
    };

    html_doc_content_dump_output(&mut buf, cur, null_mut());

    if buf.error.is_ok() {
        buf.flush();
        buf.written
    } else {
        -1
    }
}

/// Handle an out of memory condition
#[doc(alias = "htmlSaveErrMemory")]
#[cfg(feature = "libxml_output")]
unsafe fn html_save_err_memory(extra: &str) {
    use crate::error::{XmlErrorDomain, __xml_simple_oom_error};

    __xml_simple_oom_error(XmlErrorDomain::XmlFromOutput, null_mut(), Some(extra));
}

/// Dump an HTML node, recursive behaviour,children are printed too.
///
/// Returns the number of byte written or -1 in case of error
#[doc(alias = "htmlBufNodeDumpFormat")]
#[cfg(feature = "libxml_output")]
unsafe fn html_buf_node_dump_format<'a>(
    buf: &mut (impl Write + 'a),
    doc: *mut XmlDoc,
    cur: *mut XmlNode,
    format: i32,
) -> usize {
    use crate::io::XmlOutputBuffer;

    if cur.is_null() {
        return usize::MAX;
    }
    let Some(mut outbuf) = XmlOutputBuffer::from_writer(buf, None) else {
        return usize::MAX;
    };

    html_node_dump_format_output(&mut outbuf, doc, cur, None, format);
    // Is this correct ????
    outbuf.written as usize
}

/// Dump an HTML node, recursive behaviour,children are printed too, and formatting returns are added.
///
/// Returns the number of byte written or -1 in case of error
#[doc(alias = "htmlNodeDump")]
#[cfg(feature = "libxml_output")]
pub unsafe fn html_node_dump<'a>(
    buf: &mut (impl Write + 'a),
    doc: *mut XmlDoc,
    cur: *mut XmlNode,
) -> i32 {
    use crate::libxml::parser::xml_init_parser;

    if cur.is_null() {
        return -1;
    }

    xml_init_parser();

    html_buf_node_dump_format(buf, doc, cur, 1) as _
}

/// Dump an HTML node, recursive behaviour,children are printed too, and formatting returns are added.
#[doc(alias = "htmlNodeDumpFile")]
#[cfg(feature = "libxml_output")]
pub unsafe fn html_node_dump_file<'a>(
    out: &mut (impl Write + 'a),
    doc: *mut XmlDoc,
    cur: *mut XmlNode,
) {
    html_node_dump_file_format(out, doc, cur, null_mut(), 1);
}

/// Dump an HTML node, recursive behaviour,children are printed too.
///
/// TODO: if encoding.is_null() try to save in the doc encoding
///
/// returns: the number of byte written or -1 in case of failure.
#[doc(alias = "htmlNodeDumpFileFormat")]
#[cfg(feature = "libxml_output")]
pub unsafe fn html_node_dump_file_format<'a>(
    out: &mut (impl Write + 'a),
    doc: *mut XmlDoc,
    cur: *mut XmlNode,
    encoding: *const c_char,
    format: i32,
) -> i32 {
    use std::ffi::CStr;

    use crate::{
        encoding::{find_encoding_handler, XmlCharEncoding},
        libxml::parser::xml_init_parser,
    };

    xml_init_parser();

    let handler =
        if let Some(Ok(enc)) = (!encoding.is_null()).then(|| CStr::from_ptr(encoding).to_str()) {
            let e = enc.parse::<XmlCharEncoding>();
            if !matches!(e, Ok(XmlCharEncoding::UTF8)) {
                let handler = find_encoding_handler(enc);
                if handler.is_none() {
                    html_save_err(
                        XmlParserErrors::XmlSaveUnknownEncoding,
                        null_mut(),
                        Some(enc),
                    );
                }
                handler
            } else {
                None
            }
        } else if let Some(handler) = find_encoding_handler("HTML") {
            Some(handler)
        } else {
            find_encoding_handler("ascii")
        };

    // save the content to a temp buffer.
    let Some(mut buf) = XmlOutputBuffer::from_writer(out, handler) else {
        return 0;
    };

    html_node_dump_format_output(&mut buf, doc, cur, None, format);

    if buf.error.is_ok() {
        buf.flush();
        buf.written
    } else {
        -1
    }
}

/// Dump an HTML document to a file using a given encoding and formatting returns/spaces are added.
///
/// returns: the number of byte written or -1 in case of failure.
#[doc(alias = "htmlSaveFileEnc")]
#[cfg(feature = "libxml_output")]
pub unsafe fn html_save_file_enc(filename: &str, cur: *mut XmlDoc, encoding: Option<&str>) -> i32 {
    html_save_file_format(filename, cur, encoding, 1)
}

/// Dump an HTML document to a file using a given encoding.
///
/// returns: the number of byte written or -1 in case of failure.
#[doc(alias = "htmlSaveFileFormat")]
#[cfg(feature = "libxml_output")]
pub unsafe fn html_save_file_format(
    filename: &str,
    cur: *mut XmlDoc,
    encoding: Option<&str>,
    format: i32,
) -> i32 {
    use std::{cell::RefCell, rc::Rc};

    use crate::{
        encoding::{find_encoding_handler, XmlCharEncoding},
        libxml::parser::xml_init_parser,
    };

    if cur.is_null() {
        return -1;
    }

    xml_init_parser();

    let handler = if let Some(enc) = encoding.as_ref() {
        let e = enc.parse::<XmlCharEncoding>();
        let handler = if !matches!(e, Ok(XmlCharEncoding::UTF8)) {
            let handler = find_encoding_handler(enc);
            if handler.is_none() {
                html_save_err(
                    XmlParserErrors::XmlSaveUnknownEncoding,
                    null_mut(),
                    Some(enc),
                );
            }
            handler
        } else {
            None
        };
        html_set_meta_encoding(cur, Some(enc));
        handler
    } else {
        html_set_meta_encoding(cur, Some("UTF-8"));
        let handler = find_encoding_handler("HTML");
        if handler.is_some() {
            handler
        } else {
            find_encoding_handler("ascii")
        }
    };

    // save the content to a temp buffer.
    let Some(mut buf) =
        XmlOutputBuffer::from_uri(filename, handler.map(|e| Rc::new(RefCell::new(e))), 0)
    else {
        return 0;
    };
    html_doc_content_dump_format_output(&mut buf, cur, encoding, format);

    if buf.error.is_ok() {
        buf.flush();
        buf.written
    } else {
        -1
    }
}

/// TODO: check whether encoding is needed
///
/// Dump the HTML document DTD, if any.
#[doc(alias = "htmlDtdDumpOutput")]
#[cfg(feature = "libxml_output")]
unsafe fn html_dtd_dump_output(
    buf: &mut XmlOutputBuffer,
    doc: *mut XmlDoc,
    _encoding: *const c_char,
) {
    use std::ffi::CStr;

    let Some(cur) = (*doc).int_subset else {
        html_save_err(XmlParserErrors::XmlSaveNoDoctype, doc as _, None);
        return;
    };

    buf.write_str("<!DOCTYPE ");
    buf.write_str(CStr::from_ptr(cur.name as _).to_string_lossy().as_ref());
    if let Some(external_id) = cur.external_id.as_deref() {
        buf.write_str(" PUBLIC ");
        if let Some(mut buf) = buf.buffer {
            let external_id = CString::new(external_id).unwrap();
            buf.push_quoted_cstr(&external_id);
        }
        if let Some(system_id) = cur.system_id.as_deref() {
            buf.write_str(" ");
            if let Some(mut buf) = buf.buffer {
                let system_id = CString::new(system_id).unwrap();
                buf.push_quoted_cstr(&system_id);
            }
        }
    } else if let Some(system_id) = cur
        .system_id
        .as_deref()
        .filter(|&s| s != "about:legacy-compat")
    {
        buf.write_str(" SYSTEM ");
        if let Some(mut buf) = buf.buffer {
            let system_id = CString::new(system_id).unwrap();
            buf.push_quoted_cstr(&system_id);
        }
    }
    buf.write_str(">\n");
}

/// Dump an HTML attribute
#[doc(alias = "htmlAttrDumpOutput")]
#[cfg(feature = "libxml_output")]
unsafe fn html_attr_dump_output(buf: &mut XmlOutputBuffer, doc: *mut XmlDoc, cur: &XmlAttr) {
    use std::ffi::CStr;

    use crate::{libxml::chvalid::xml_is_blank_char, uri::escape_url_except};

    // The html output method should not escape a & character
    // occurring in an attribute value immediately followed by
    // a { character (see Section B.7.1 of the HTML 4.0 Recommendation).
    // This is implemented in xmlEncodeEntitiesReentrant

    buf.write_str(" ");
    if let Some(prefix) = cur.ns.as_deref().and_then(|ns| ns.prefix()) {
        buf.write_str(&prefix);
        buf.write_str(":");
    }

    buf.write_str(CStr::from_ptr(cur.name as _).to_string_lossy().as_ref());
    if let Some(children) = cur
        .children
        .filter(|_| html_is_boolean_attr(cur.name as _) == 0)
    {
        if let Some(value) = children.get_string(doc, 0) {
            buf.write_str("=");
            if cur.ns.is_none()
                && cur
                    .parent
                    .filter(|p| {
                        p.ns.is_none()
                            && (xml_strcasecmp(cur.name, c"href".as_ptr() as _) == 0
                                || xml_strcasecmp(cur.name, c"action".as_ptr() as _) == 0
                                || xml_strcasecmp(cur.name, c"src".as_ptr() as _) == 0
                                || (xml_strcasecmp(cur.name, c"name".as_ptr() as _) == 0
                                    && xml_strcasecmp(p.name, c"a".as_ptr() as _) == 0))
                    })
                    .is_some()
            {
                let tmp = value.trim_start_matches(|c| xml_is_blank_char(c as u32));

                // Angle brackets are technically illegal in URIs, but they're
                // used in server side includes, for example. Curly brackets
                // are illegal as well and often used in templates.
                // Don't escape non-whitespace, printable ASCII chars for
                // improved interoperability. Only escape space, control
                // and non-ASCII chars.
                let escaped = escape_url_except(tmp, b"\"#$%&+,/:;<=>?@[\\]^`{|}");
                if let Some(mut buf) = buf.buffer {
                    let escaped = CString::new(escaped.as_ref()).unwrap();
                    buf.push_quoted_cstr(&escaped);
                }
            } else if let Some(mut buf) = buf.buffer {
                let value = CString::new(value.as_str()).unwrap();
                buf.push_quoted_cstr(&value);
            }
        } else {
            buf.write_str("=\"\"");
        }
    }
}

/// Dump an HTML node, recursive behaviour,children are printed too.
#[doc(alias = "htmlNodeDumpFormatOutput")]
#[cfg(feature = "libxml_output")]
pub unsafe fn html_node_dump_format_output(
    buf: &mut XmlOutputBuffer,
    doc: *mut XmlDoc,
    mut cur: *mut XmlNode,
    _encoding: Option<&str>,
    format: i32,
) {
    use std::ffi::CStr;

    use crate::{
        libxml::{
            globals::xml_free,
            htmlparser::html_tag_lookup,
            parser::xml_init_parser,
            parser_internals::{XML_STRING_TEXT, XML_STRING_TEXT_NOENC},
        },
        save::xml_ns_list_dump_output,
        tree::{xml_encode_entities_reentrant, NodePtr},
    };

    let mut parent: *mut XmlNode;

    xml_init_parser();

    if cur.is_null() {
        return;
    }

    let root: *mut XmlNode = cur;
    parent = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
    'main: loop {
        match (*cur).element_type() {
            XmlElementType::XmlHTMLDocumentNode | XmlElementType::XmlDocumentNode => {
                if (*cur)
                    .as_document_node()
                    .unwrap()
                    .as_ref()
                    .int_subset
                    .is_some()
                {
                    html_dtd_dump_output(buf, cur as _, null_mut());
                }
                if let Some(children) = (*cur).children() {
                    // Always validate (*cur).parent when descending.
                    if (*cur).parent() == NodePtr::from_ptr(parent) {
                        parent = cur;
                        cur = children.as_ptr();
                        continue;
                    }
                } else {
                    buf.write_str("\n");
                }
            }

            XmlElementType::XmlElementNode => 'to_break: {
                // Some users like lxml are known to pass nodes with a corrupted
                // tree structure. Fall back to a recursive call to handle this case.
                if (*cur).parent() != NodePtr::from_ptr(parent) && (*cur).children().is_some() {
                    html_node_dump_format_output(buf, doc, cur, _encoding, format);
                    break 'to_break;
                }

                // Get specific HTML info for that node.
                let info = if (*cur).ns.is_none() {
                    html_tag_lookup((*cur).name().as_deref().unwrap())
                } else {
                    None
                };

                buf.write_str("<");
                if let Some(prefix) = (*cur).ns.as_deref().and_then(|ns| ns.prefix()) {
                    buf.write_str(&prefix);
                    buf.write_str(":");
                }

                buf.write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
                if let Some(ns_def) = (*cur).ns_def {
                    xml_ns_list_dump_output(buf, Some(ns_def));
                }
                let mut attr = XmlAttrPtr::from_raw((*cur).properties).unwrap();
                while let Some(now) = attr {
                    html_attr_dump_output(buf, doc, &now);
                    attr = XmlAttrPtr::from_raw(now.next).unwrap();
                }

                if info.map_or(false, |info| info.empty != 0) {
                    buf.write_str(">");
                } else if let Some(children) = (*cur).children() {
                    buf.write_str(">");
                    if format != 0
                        && info.map_or(false, |info| info.isinline == 0)
                        && !matches!(
                            children.element_type(),
                            HTML_TEXT_NODE | HTML_ENTITY_REF_NODE
                        )
                        && (*cur).children() != (*cur).last()
                        && !(*cur).name.is_null()
                        && *(*cur).name.add(0) != b'p'
                    {
                        // p, pre, param
                        buf.write_str("\n");
                    }
                    parent = cur;
                    cur = children.as_ptr();
                    continue 'main;
                } else if info.map_or(false, |info| {
                    info.save_end_tag != 0 && info.name != "html" && info.name != "body"
                }) {
                    buf.write_str(">");
                } else {
                    buf.write_str("></");
                    if let Some(prefix) = (*cur).ns.as_deref().and_then(|ns| ns.prefix()) {
                        buf.write_str(&prefix);
                        buf.write_str(":");
                    }

                    buf.write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
                    buf.write_str(">");
                }

                if (format != 0
                    && (*cur).next.is_some()
                    && info.map_or(false, |info| info.isinline == 0))
                    && (!matches!(
                        (*cur).next.unwrap().element_type(),
                        HTML_TEXT_NODE | HTML_ENTITY_REF_NODE
                    ) && !parent.is_null()
                        && !(*parent).name.is_null()
                        && *(*parent).name.add(0) != b'p')
                {
                    buf.write_str("\n");
                }
            }
            XmlElementType::XmlAttributeNode => {
                html_attr_dump_output(
                    buf,
                    doc,
                    &XmlAttrPtr::from_raw(cur as *mut XmlAttr).unwrap().unwrap(),
                );
            }

            HTML_TEXT_NODE => 'to_break: {
                if (*cur).content.is_null() {
                    break 'to_break;
                }
                if ((*cur).name == XML_STRING_TEXT.as_ptr() as _
                    || (*cur).name != XML_STRING_TEXT_NOENC.as_ptr() as _)
                    && (parent.is_null()
                        || (xml_strcasecmp((*parent).name, c"script".as_ptr() as _) != 0
                            && xml_strcasecmp((*parent).name, c"style".as_ptr() as _) != 0))
                {
                    let buffer: *mut XmlChar = xml_encode_entities_reentrant(doc, (*cur).content);
                    if !buffer.is_null() {
                        buf.write_str(CStr::from_ptr(buffer as _).to_string_lossy().as_ref());
                        xml_free(buffer as _);
                    }
                } else {
                    buf.write_str(
                        CStr::from_ptr((*cur).content as _)
                            .to_string_lossy()
                            .as_ref(),
                    );
                }
            }

            HTML_COMMENT_NODE => {
                if !(*cur).content.is_null() {
                    buf.write_str("<!--");

                    buf.write_str(
                        CStr::from_ptr((*cur).content as _)
                            .to_string_lossy()
                            .as_ref(),
                    );
                    buf.write_str("-->");
                }
            }

            HTML_PI_NODE => {
                if !(*cur).name.is_null() {
                    buf.write_str("<?");

                    buf.write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
                    if !(*cur).content.is_null() {
                        buf.write_str(" ");

                        buf.write_str(
                            CStr::from_ptr((*cur).content as _)
                                .to_string_lossy()
                                .as_ref(),
                        );
                    }
                    buf.write_str(">");
                }
            }
            HTML_ENTITY_REF_NODE => {
                buf.write_str("&");

                buf.write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
                buf.write_str(";");
            }
            HTML_PRESERVE_NODE => {
                if !(*cur).content.is_null() {
                    buf.write_str(
                        CStr::from_ptr((*cur).content as _)
                            .to_string_lossy()
                            .as_ref(),
                    );
                }
            }
            _ => {}
        }

        loop {
            if cur == root {
                return;
            }
            if let Some(next) = (*cur).next {
                cur = next.as_ptr();
                break;
            }

            cur = parent;
            // (*cur).parent was validated when descending.
            parent = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());

            if matches!(
                (*cur).element_type(),
                XmlElementType::XmlHTMLDocumentNode | XmlElementType::XmlDocumentNode
            ) {
                buf.write_str("\n");
            } else {
                let info = if format != 0 && (*cur).ns.is_none() {
                    html_tag_lookup((*cur).name().as_deref().unwrap())
                } else {
                    None
                };

                if format != 0
                    && info.map_or(false, |info| info.isinline == 0)
                    && !matches!(
                        (*cur).last().unwrap().element_type(),
                        HTML_TEXT_NODE | HTML_ENTITY_REF_NODE
                    )
                    && (*cur).children() != (*cur).last()
                    && !(*cur).name.is_null()
                    && *(*cur).name.add(0) != b'p'
                {
                    /* p, pre, param */
                    buf.write_str("\n");
                }

                buf.write_str("</");
                if let Some(prefix) = (*cur).ns.as_deref().and_then(|ns| ns.prefix()) {
                    buf.write_str(&prefix);
                    buf.write_str(":");
                }

                buf.write_str(CStr::from_ptr((*cur).name as _).to_string_lossy().as_ref());
                buf.write_str(">");

                if (format != 0
                    && info.map_or(false, |info| info.isinline == 0)
                    && (*cur).next.is_some())
                    && (!matches!(
                        (*cur).next.unwrap().element_type(),
                        HTML_TEXT_NODE | HTML_ENTITY_REF_NODE
                    ) && !parent.is_null()
                        && !(*parent).name.is_null()
                        && *(*parent).name.add(0) != b'p')
                {
                    buf.write_str("\n");
                }
            }
        }
    }
}

/// Dump an HTML document. Formatting return/spaces are added.
#[doc(alias = "htmlDocContentDumpOutput")]
#[cfg(feature = "libxml_output")]
pub unsafe fn html_doc_content_dump_output(
    buf: &mut XmlOutputBuffer,
    cur: *mut XmlDoc,
    _encoding: *const c_char,
) {
    html_node_dump_format_output(buf, cur, cur as _, None, 1);
}

/// Dump an HTML document.
#[doc(alias = "htmlDocContentDumpFormatOutput")]
#[cfg(feature = "libxml_output")]
pub unsafe fn html_doc_content_dump_format_output(
    buf: &mut XmlOutputBuffer,
    cur: *mut XmlDoc,
    _encoding: Option<&str>,
    format: i32,
) {
    let mut typ: i32 = 0;
    if !cur.is_null() {
        typ = (*cur).typ as i32;
        (*cur).typ = XmlElementType::XmlHTMLDocumentNode;
    }
    html_node_dump_format_output(buf, cur, cur as _, None, format);
    if !cur.is_null() {
        (*cur).typ = typ.try_into().unwrap();
    }
}

/// Dump an HTML node, recursive behaviour,children are printed too,
/// and formatting returns/spaces are added.
#[doc(alias = "htmlNodeDumpOutput")]
#[cfg(feature = "libxml_output")]
pub unsafe fn html_node_dump_output(
    buf: &mut XmlOutputBuffer,
    doc: *mut XmlDoc,
    cur: *mut XmlNode,
    _encoding: *const c_char,
) {
    html_node_dump_format_output(buf, doc, cur, None, 1);
}

/// These are the HTML attributes which will be output
/// in minimized form, i.e. <option selected="selected"> will be
/// output as <option selected>, as per XSLT 1.0 16.2 "HTML Output Method"
const HTML_BOOLEAN_ATTRS: &[*const c_char] = &[
    c"checked".as_ptr() as _,
    c"compact".as_ptr() as _,
    c"declare".as_ptr() as _,
    c"defer".as_ptr() as _,
    c"disabled".as_ptr() as _,
    c"ismap".as_ptr() as _,
    c"multiple".as_ptr() as _,
    c"nohref".as_ptr() as _,
    c"noresize".as_ptr() as _,
    c"noshade".as_ptr() as _,
    c"nowrap".as_ptr() as _,
    c"readonly".as_ptr() as _,
    c"selected".as_ptr() as _,
    null(),
];

/// Determine if a given attribute is a boolean attribute.
///
/// returns: false if the attribute is not boolean, true otherwise.
#[doc(alias = "htmlIsBooleanAttr")]
pub unsafe fn html_is_boolean_attr(name: *const XmlChar) -> i32 {
    let mut i: usize = 0;

    while !HTML_BOOLEAN_ATTRS[i].is_null() {
        if xml_strcasecmp(HTML_BOOLEAN_ATTRS[i] as _, name) == 0 {
            return 1;
        }
        i += 1;
    }
    0
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_html_doc_dump() {
        #[cfg(all(feature = "html", feature = "libxml_output"))]
        unsafe {
            let mut leaks = 0;

            for n_f in 0..GEN_NB_FILE_PTR {
                for n_cur in 0..GEN_NB_XML_DOC_PTR {
                    let mem_base = xml_mem_blocks();
                    let mut f = gen_file_ptr(n_f, 0).unwrap();
                    let cur = gen_xml_doc_ptr(n_cur, 1);

                    let ret_val = html_doc_dump(&mut f, cur);
                    desret_int(ret_val);
                    des_xml_doc_ptr(n_cur, cur, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in htmlDocDump",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_f);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in htmlDocDump()");
        }
    }

    #[test]
    fn test_html_doc_dump_memory() {
        #[cfg(all(feature = "html", feature = "libxml_output"))]
        unsafe {
            let mut leaks = 0;

            for n_cur in 0..GEN_NB_XML_DOC_PTR {
                for n_mem in 0..GEN_NB_XML_CHAR_PTR_PTR {
                    for n_size in 0..GEN_NB_INT_PTR {
                        let mem_base = xml_mem_blocks();
                        let cur = gen_xml_doc_ptr(n_cur, 0);
                        let mem = gen_xml_char_ptr_ptr(n_mem, 1);
                        let size = gen_int_ptr(n_size, 2);

                        html_doc_dump_memory(cur, mem, size);
                        des_xml_doc_ptr(n_cur, cur, 0);
                        des_xml_char_ptr_ptr(n_mem, mem, 1);
                        des_int_ptr(n_size, size, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in htmlDocDumpMemory",
                                xml_mem_blocks() - mem_base
                            );
                            eprint!(" {}", n_cur);
                            eprint!(" {}", n_mem);
                            eprintln!(" {}", n_size);
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in htmlDocDumpMemory()");
        }
    }

    #[test]
    fn test_html_doc_dump_memory_format() {
        #[cfg(all(feature = "html", feature = "libxml_output"))]
        unsafe {
            let mut leaks = 0;

            for n_cur in 0..GEN_NB_XML_DOC_PTR {
                for n_mem in 0..GEN_NB_XML_CHAR_PTR_PTR {
                    for n_size in 0..GEN_NB_INT_PTR {
                        for n_format in 0..GEN_NB_INT {
                            let mem_base = xml_mem_blocks();
                            let cur = gen_xml_doc_ptr(n_cur, 0);
                            let mem = gen_xml_char_ptr_ptr(n_mem, 1);
                            let size = gen_int_ptr(n_size, 2);
                            let format = gen_int(n_format, 3);

                            html_doc_dump_memory_format(cur, mem, size, format);
                            des_xml_doc_ptr(n_cur, cur, 0);
                            des_xml_char_ptr_ptr(n_mem, mem, 1);
                            des_int_ptr(n_size, size, 2);
                            des_int(n_format, format, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in htmlDocDumpMemoryFormat",
                                    xml_mem_blocks() - mem_base
                                );
                                eprint!(" {}", n_cur);
                                eprint!(" {}", n_mem);
                                eprint!(" {}", n_size);
                                eprintln!(" {}", n_format);
                            }
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in htmlDocDumpMemoryFormat()"
            );
        }
    }

    #[test]
    fn test_html_is_boolean_attr() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let name = gen_const_xml_char_ptr(n_name, 0);

                let ret_val = html_is_boolean_attr(name as *const XmlChar);
                desret_int(ret_val);
                des_const_xml_char_ptr(n_name, name, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in htmlIsBooleanAttr",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in htmlIsBooleanAttr()");
                    eprintln!(" {}", n_name);
                }
            }
        }
    }

    #[test]
    fn test_html_new_doc() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            for n_uri in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_external_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let uri = gen_const_xml_char_ptr(n_uri, 0);
                    let external_id = gen_const_xml_char_ptr(n_external_id, 1);

                    let ret_val = html_new_doc(uri as *const XmlChar, external_id);
                    desret_html_doc_ptr(ret_val);
                    des_const_xml_char_ptr(n_uri, uri, 0);
                    des_const_xml_char_ptr(n_external_id, external_id, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in htmlNewDoc",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_uri);
                        eprintln!(" {}", n_external_id);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in htmlNewDoc()");
        }
    }

    #[test]
    fn test_html_new_doc_no_dt_d() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            for n_uri in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_external_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let uri = gen_const_xml_char_ptr(n_uri, 0);
                    let external_id = gen_const_xml_char_ptr(n_external_id, 1);

                    let ret_val = html_new_doc_no_dtd(uri as *const XmlChar, external_id);
                    desret_html_doc_ptr(ret_val);
                    des_const_xml_char_ptr(n_uri, uri, 0);
                    des_const_xml_char_ptr(n_external_id, external_id, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in htmlNewDocNoDtD",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_uri);
                        eprintln!(" {}", n_external_id);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in htmlNewDocNoDtD()");
        }
    }

    #[test]
    fn test_html_node_dump_file() {
        #[cfg(all(feature = "html", feature = "libxml_output"))]
        unsafe {
            let mut leaks = 0;

            for n_out in 0..GEN_NB_FILE_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_cur in 0..GEN_NB_XML_NODE_PTR {
                        let mem_base = xml_mem_blocks();
                        let mut out = gen_file_ptr(n_out, 0).unwrap();
                        let doc = gen_xml_doc_ptr(n_doc, 1);
                        let cur = gen_xml_node_ptr(n_cur, 2);

                        html_node_dump_file(&mut out, doc, cur);
                        des_xml_doc_ptr(n_doc, doc, 1);
                        des_xml_node_ptr(n_cur, cur, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in htmlNodeDumpFile",
                                xml_mem_blocks() - mem_base
                            );
                            eprint!(" {}", n_out);
                            eprint!(" {}", n_doc);
                            eprintln!(" {}", n_cur);
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in htmlNodeDumpFile()");
        }
    }

    #[test]
    fn test_html_node_dump_file_format() {
        #[cfg(all(feature = "html", feature = "libxml_output"))]
        unsafe {
            let mut leaks = 0;

            for n_out in 0..GEN_NB_FILE_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_cur in 0..GEN_NB_XML_NODE_PTR {
                        for n_encoding in 0..GEN_NB_CONST_CHAR_PTR {
                            for n_format in 0..GEN_NB_INT {
                                let mem_base = xml_mem_blocks();
                                let mut out = gen_file_ptr(n_out, 0).unwrap();
                                let doc = gen_xml_doc_ptr(n_doc, 1);
                                let cur = gen_xml_node_ptr(n_cur, 2);
                                let encoding = gen_const_char_ptr(n_encoding, 3);
                                let format = gen_int(n_format, 4);

                                let ret_val = html_node_dump_file_format(
                                    &mut out, doc, cur, encoding, format,
                                );
                                desret_int(ret_val);
                                des_xml_doc_ptr(n_doc, doc, 1);
                                des_xml_node_ptr(n_cur, cur, 2);
                                des_const_char_ptr(n_encoding, encoding, 3);
                                des_int(n_format, format, 4);
                                reset_last_error();
                                if mem_base != xml_mem_blocks() {
                                    leaks += 1;
                                    eprint!(
                                        "Leak of {} blocks found in htmlNodeDumpFileFormat",
                                        xml_mem_blocks() - mem_base
                                    );
                                    eprint!(" {}", n_out);
                                    eprint!(" {}", n_doc);
                                    eprint!(" {}", n_cur);
                                    eprint!(" {}", n_encoding);
                                    eprintln!(" {}", n_format);
                                }
                            }
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in htmlNodeDumpFileFormat()"
            );
        }
    }

    #[test]
    fn test_html_save_file() {
        #[cfg(all(feature = "html", feature = "libxml_output"))]
        unsafe {
            let mut leaks = 0;

            for n_filename in 0..GEN_NB_FILEOUTPUT {
                for n_cur in 0..GEN_NB_XML_DOC_PTR {
                    let mem_base = xml_mem_blocks();
                    let filename = gen_fileoutput(n_filename, 0);
                    let cur = gen_xml_doc_ptr(n_cur, 1);

                    let ret_val = html_save_file(filename, cur);
                    desret_int(ret_val);
                    des_fileoutput(n_filename, filename, 0);
                    des_xml_doc_ptr(n_cur, cur, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in htmlSaveFile",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_filename);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in htmlSaveFile()");
        }
    }
}
