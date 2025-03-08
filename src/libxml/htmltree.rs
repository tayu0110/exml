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
    ffi::{CStr, CString, c_char},
    ptr::{null, null_mut},
    sync::atomic::Ordering,
};

use crate::{
    encoding::XmlCharEncoding,
    tree::{
        __XML_REGISTER_CALLBACKS, NodeCommon, XmlAttr, XmlAttrPtr, XmlDoc, XmlDocProperties,
        XmlDocPtr, XmlElementType, XmlNodePtr, xml_create_int_subset, xml_free_node,
        xml_new_doc_node, xml_new_prop,
    },
};
#[cfg(feature = "libxml_output")]
use crate::{error::XmlParserErrors, io::XmlOutputBuffer, tree::XmlGenericNodePtr};

use super::{
    globals::xml_register_node_default_value,
    htmlparser::{HtmlDocPtr, HtmlNodePtr, html_err_memory},
    xmlstring::{XmlChar, xml_str_equal, xml_strcasecmp, xml_strstr},
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
pub unsafe fn html_new_doc(uri: *const XmlChar, external_id: *const XmlChar) -> Option<HtmlDocPtr> {
    unsafe {
        if uri.is_null() && external_id.is_null() {
            return html_new_doc_no_dtd(
                c"http://www.w3.org/TR/REC-html40/loose.dtd".as_ptr() as _,
                c"-//W3C//DTD HTML 4.0 Transitional//EN".as_ptr() as _,
            );
        }

        html_new_doc_no_dtd(uri, external_id)
    }
}

/// Creates a new HTML document without a DTD node if `uri` and `external_id` are NULL.
///
/// Returns a new document, do not initialize the DTD if not provided
#[doc(alias = "htmlNewDocNoDtD")]
pub unsafe fn html_new_doc_no_dtd(
    uri: *const XmlChar,
    external_id: *const XmlChar,
) -> Option<HtmlDocPtr> {
    unsafe {
        // Allocate a new document and fill the fields.
        let Some(mut cur) = XmlDocPtr::new(XmlDoc {
            typ: XmlElementType::XmlHTMLDocumentNode,
            version: None,
            int_subset: None,
            name: null_mut(),
            children: None,
            ext_subset: None,
            old_ns: None,
            encoding: None,
            standalone: 1,
            compression: 0,
            ids: None,
            refs: None,
            _private: null_mut(),
            charset: XmlCharEncoding::UTF8,
            properties: XmlDocProperties::XmlDocHTML as i32
                | XmlDocProperties::XmlDocUserbuilt as i32,
            ..Default::default()
        }) else {
            html_err_memory(null_mut(), Some("HTML document creation failed\n"));
            return None;
        };
        cur.doc = Some(cur);
        if !external_id.is_null() || !uri.is_null() {
            xml_create_int_subset(
                Some(cur),
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
            xml_register_node_default_value(cur.into());
        }
        Some(cur)
    }
}

/// Encoding definition lookup in the Meta tags
///
/// Returns the current encoding as flagged in the HTML source
#[doc(alias = "htmlGetMetaEncoding")]
pub unsafe fn html_get_meta_encoding(doc: XmlDocPtr) -> Option<String> {
    unsafe {
        let mut content: *const XmlChar;
        let mut encoding: *const XmlChar;

        let mut cur = doc.children;

        // Search the html
        'goto_found_meta: {
            'goto_found_head: {
                while let Some(now) = cur {
                    if matches!(now.element_type(), XmlElementType::XmlElementNode) {
                        let cur = HtmlNodePtr::try_from(now).unwrap();
                        if !cur.name.is_null() {
                            if xml_str_equal(cur.name, c"html".as_ptr() as _) {
                                break;
                            }
                            if xml_str_equal(cur.name, c"head".as_ptr() as _) {
                                break 'goto_found_head;
                            }
                            if xml_str_equal(cur.name, c"meta".as_ptr() as _) {
                                break 'goto_found_meta;
                            }
                        }
                    }
                    cur = now.next();
                }
                cur = cur?.children();
                // Search the head
                while let Some(now) = cur {
                    if matches!(now.element_type(), XmlElementType::XmlElementNode) {
                        let now = HtmlNodePtr::try_from(now).unwrap();
                        if !now.name.is_null() {
                            if xml_str_equal(now.name, c"head".as_ptr() as _) {
                                break;
                            }
                            if xml_str_equal(now.name, c"meta".as_ptr() as _) {
                                break 'goto_found_meta;
                            }
                        }
                    }
                    cur = now.next();
                }
            }
            // found_head:
            cur = cur?.children();
        }

        // Search the meta elements

        // found_meta:
        while let Some(cur_node) = cur {
            if matches!(cur_node.element_type(), XmlElementType::XmlElementNode) {
                let cur_node = HtmlNodePtr::try_from(cur_node).unwrap();
                if !cur_node.name.is_null() && cur_node.name().as_deref() == Some("meta") {
                    let mut attr = cur_node.properties;
                    let mut http = 0;
                    let mut value: *const XmlChar;

                    content = null_mut();
                    while let Some(now) = attr {
                        if let Some(children) = now
                            .children()
                            .filter(|c| {
                                matches!(c.element_type(), XmlElementType::XmlTextNode)
                                    && c.next().is_none()
                            })
                            .map(|children| XmlNodePtr::try_from(children).unwrap())
                        {
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
                        attr = now.next;
                    }
                }
            }
            cur = cur_node.next();
        }
        None
    }
}

/// Sets the current encoding in the Meta tags
///
/// # Note
/// This will not change the document content encoding, just the META flag associated.
///
/// Returns 0 in case of success and -1 in case of error
#[doc(alias = "htmlSetMetaEncoding")]
pub unsafe fn html_set_meta_encoding(doc: XmlDocPtr, encoding: Option<&str>) -> i32 {
    unsafe {
        // html isn't a real encoding it's just libxml2 way to get entities
        if encoding.as_ref().map(|e| e.to_ascii_lowercase()).as_deref() == Some("html") {
            return -1;
        }

        let newcontent = if let Some(encoding) = encoding {
            format!("text/html; charset={encoding}")
        } else {
            String::new()
        };

        let mut cur = doc.children;

        let mut found_head = false;
        let mut found_meta = false;
        // Search the html
        while let Some(now) = cur {
            if matches!(now.element_type(), XmlElementType::XmlElementNode) {
                let now = HtmlNodePtr::try_from(now).unwrap();
                if !now.name.is_null() {
                    if xml_strcasecmp(now.name, c"html".as_ptr() as _) == 0 {
                        break;
                    }
                    if xml_strcasecmp(now.name, c"head".as_ptr() as _) == 0 {
                        // goto found_head;
                        found_head = true;
                        break;
                    }
                    if xml_strcasecmp(now.name, c"meta".as_ptr() as _) == 0 {
                        // goto found_meta;
                        found_meta = true;
                        break;
                    }
                }
            }

            cur = now.next();
        }

        let mut head = None;
        if !found_head && !found_meta {
            let Some(tmp) = cur else {
                return -1;
            };
            cur = tmp.children();

            // Search the head
            while let Some(now) = cur {
                if matches!(now.element_type(), XmlElementType::XmlElementNode) {
                    let now = HtmlNodePtr::try_from(now).unwrap();
                    if !now.name.is_null() {
                        if xml_strcasecmp(now.name, c"head".as_ptr() as _) == 0 {
                            break;
                        }
                        if xml_strcasecmp(now.name, c"meta".as_ptr() as _) == 0 {
                            head = now.parent();
                            // goto found_meta;
                            found_meta = true;
                        }
                    }
                }
                cur = now.next();
            }
            if cur.is_none() {
                return -1;
            }
        }

        // found_head:

        if !found_meta {
            head = cur;
            assert!(cur.is_some());
            let Some(children) = cur.unwrap().children() else {
                // goto create;
                if encoding.is_some() {
                    if let Some(mut head) = head {
                        // Create a new Meta element with the right attributes
                        let meta = xml_new_doc_node(Some(doc), None, "meta", null_mut());
                        if let Some(children) = head.children() {
                            children.add_prev_sibling(meta.unwrap().into());
                        } else {
                            head.add_child(meta.unwrap().into());
                        }
                        xml_new_prop(
                            meta,
                            c"http-equiv".as_ptr() as _,
                            c"Content-Type".as_ptr() as _,
                        );
                        let newcontent = CString::new(newcontent).unwrap();
                        xml_new_prop(meta, c"content".as_ptr() as _, newcontent.as_ptr() as _);
                    }
                }
                return 0;
            };
            cur = Some(children);
        }

        // found_meta:
        // Search and update all the remaining the meta elements carrying
        // encoding information
        let mut meta = None;
        let mut content = None;
        while let Some(cur_node) = cur {
            if matches!(cur_node.element_type(), XmlElementType::XmlElementNode) {
                let cur_node = HtmlNodePtr::try_from(cur_node).unwrap();
                if cur_node
                    .name()
                    .as_deref()
                    .is_some_and(|name| name.eq_ignore_ascii_case("meta"))
                {
                    let mut attr = cur_node.properties;
                    let mut http = 0;
                    let mut value: *const XmlChar;

                    content = None;
                    while let Some(now) = attr {
                        if let Some(children) = now
                            .children()
                            .filter(|c| {
                                matches!(c.element_type(), XmlElementType::XmlTextNode)
                                    && c.next().is_none()
                            })
                            .map(|children| XmlNodePtr::try_from(children).unwrap())
                        {
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
                        attr = now.next;
                    }
                    if http != 0 && content.is_some() {
                        meta = cur;
                        break;
                    }
                }
            }
            cur = cur_node.next();
        }
        // create:
        if let Some(mut meta) = meta {
            // remove the meta tag if NULL is passed
            if encoding.is_none() {
                meta.unlink();
                xml_free_node(meta);
            }
            // change the document only if there is a real encoding change
            else if content.is_none_or(|c| {
                !c.to_ascii_lowercase()
                    .contains(&encoding.unwrap().to_ascii_lowercase())
            }) {
                meta.set_prop("content", Some(&newcontent));
            }
        } else if let Some(mut head) = head.filter(|_| encoding.is_some()) {
            // Create a new Meta element with the right attributes
            let meta = xml_new_doc_node(Some(doc), None, "meta", null_mut());
            if let Some(children) = head.children() {
                children.add_prev_sibling(meta.unwrap().into());
            } else {
                head.add_child(meta.unwrap().into());
            }
            xml_new_prop(
                meta,
                c"http-equiv".as_ptr() as _,
                c"Content-Type".as_ptr() as _,
            );
            let newcontent = CString::new(newcontent).unwrap();
            xml_new_prop(meta, c"content".as_ptr() as _, newcontent.as_ptr() as _);
        }

        0
    }
}

/// Dump an HTML document in memory and return the xmlChar * and it's size.  
/// It's up to the caller to free the memory.
#[doc(alias = "htmlDocDumpMemory")]
#[cfg(feature = "libxml_output")]
pub unsafe fn html_doc_dump_memory(cur: XmlDocPtr, mem: *mut *mut XmlChar, size: *mut i32) {
    unsafe {
        html_doc_dump_memory_format(cur, mem, size, 1);
    }
}

/// Handle an out of memory condition
#[doc(alias = "htmlSaveErr")]
#[cfg(feature = "libxml_output")]
unsafe fn html_save_err(
    code: XmlParserErrors,
    node: Option<XmlGenericNodePtr>,
    extra: Option<&str>,
) {
    unsafe {
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
}

/// Dump an HTML document in memory and return the xmlChar * and it's size.  
/// It's up to the caller to free the memory.
#[doc(alias = "htmlDocDumpMemoryFormat")]
#[cfg(feature = "libxml_output")]
pub unsafe fn html_doc_dump_memory_format(
    cur: XmlDocPtr,
    mem: *mut *mut XmlChar,
    size: *mut i32,
    format: i32,
) {
    unsafe {
        use std::{cell::RefCell, rc::Rc};

        use crate::{
            encoding::{XmlCharEncoding, find_encoding_handler},
            libxml::{parser::xml_init_parser, xmlstring::xml_strndup},
        };

        xml_init_parser();

        if mem.is_null() || size.is_null() {
            return;
        }
        // if cur.is_null() {
        //     *mem = null_mut();
        //     *size = 0;
        //     return;
        // }

        let handler = if let Some(enc) = html_get_meta_encoding(cur) {
            let e = enc.parse::<XmlCharEncoding>();
            if !matches!(e, Ok(XmlCharEncoding::UTF8)) {
                let handler = find_encoding_handler(&enc);
                if handler.is_none() {
                    html_save_err(XmlParserErrors::XmlSaveUnknownEncoding, None, Some(&enc));
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

        html_doc_content_dump_format_output(&mut buf, Some(cur), None, format);

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
}

/// Dump an HTML document to an open FILE.
///
/// returns: the number of byte written or -1 in case of failure.
#[doc(alias = "htmlDocDump")]
#[cfg(feature = "libxml_output")]
pub unsafe fn html_doc_dump<'a>(f: &mut (impl Write + 'a), cur: XmlDocPtr) -> i32 {
    unsafe {
        use crate::{
            encoding::{XmlCharEncoding, find_encoding_handler},
            libxml::parser::xml_init_parser,
        };

        xml_init_parser();

        // if cur.is_null() {
        //     return -1;
        // }

        let handler = if let Some(enc) = html_get_meta_encoding(cur) {
            let e = enc.parse::<XmlCharEncoding>();
            if !matches!(e, Ok(XmlCharEncoding::UTF8)) {
                let handler = find_encoding_handler(&enc);
                if handler.is_none() {
                    html_save_err(XmlParserErrors::XmlSaveUnknownEncoding, None, Some(&enc));
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
        html_doc_content_dump_output(&mut buf, Some(cur), null_mut());

        if buf.error.is_ok() {
            buf.flush();
            buf.written
        } else {
            -1
        }
    }
}

/// Dump an HTML document to a file. If `filename` is `"-"` the stdout file is used.
///
/// returns: the number of byte written or -1 in case of failure.
#[doc(alias = "htmlSaveFile")]
#[cfg(feature = "libxml_output")]
pub unsafe fn html_save_file(filename: *const c_char, cur: XmlDocPtr) -> i32 {
    unsafe {
        use std::{cell::RefCell, rc::Rc};

        use crate::{
            encoding::{XmlCharEncoding, find_encoding_handler},
            libxml::parser::xml_init_parser,
        };

        // if cur.is_null() {
        //     return -1;
        // }
        if filename.is_null() {
            return -1;
        }

        xml_init_parser();

        let handler = if let Some(enc) = html_get_meta_encoding(cur) {
            let e = enc.parse::<XmlCharEncoding>();
            if !matches!(e, Ok(XmlCharEncoding::UTF8)) {
                let handler = find_encoding_handler(&enc);
                if handler.is_none() {
                    html_save_err(XmlParserErrors::XmlSaveUnknownEncoding, None, Some(&enc));
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
            cur.compression,
        ) else {
            return 0;
        };

        html_doc_content_dump_output(&mut buf, Some(cur), null_mut());

        if buf.error.is_ok() {
            buf.flush();
            buf.written
        } else {
            -1
        }
    }
}

/// Dump an HTML node, recursive behaviour,children are printed too.
///
/// Returns the number of byte written or -1 in case of error
#[doc(alias = "htmlBufNodeDumpFormat")]
#[cfg(feature = "libxml_output")]
unsafe fn html_buf_node_dump_format<'a>(
    buf: &mut (impl Write + 'a),
    doc: Option<XmlDocPtr>,
    cur: HtmlNodePtr,
    format: i32,
) -> usize {
    unsafe {
        use crate::io::XmlOutputBuffer;

        let Some(mut outbuf) = XmlOutputBuffer::from_writer(buf, None) else {
            return usize::MAX;
        };

        html_node_dump_format_output(&mut outbuf, doc, Some(cur.into()), None, format);
        // Is this correct ????
        outbuf.written as usize
    }
}

/// Dump an HTML node, recursive behaviour,children are printed too, and formatting returns are added.
///
/// Returns the number of byte written or -1 in case of error
#[doc(alias = "htmlNodeDump")]
#[cfg(feature = "libxml_output")]
pub unsafe fn html_node_dump<'a>(
    buf: &mut (impl Write + 'a),
    doc: Option<XmlDocPtr>,
    cur: HtmlNodePtr,
) -> i32 {
    unsafe {
        use crate::libxml::parser::xml_init_parser;

        xml_init_parser();

        html_buf_node_dump_format(buf, doc, cur, 1) as _
    }
}

/// Dump an HTML node, recursive behaviour,children are printed too, and formatting returns are added.
#[doc(alias = "htmlNodeDumpFile")]
#[cfg(feature = "libxml_output")]
pub unsafe fn html_node_dump_file<'a>(
    out: &mut (impl Write + 'a),
    doc: Option<XmlDocPtr>,
    cur: Option<XmlGenericNodePtr>,
) {
    unsafe {
        html_node_dump_file_format(out, doc, cur, null_mut(), 1);
    }
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
    doc: Option<XmlDocPtr>,
    cur: Option<XmlGenericNodePtr>,
    encoding: *const c_char,
    format: i32,
) -> i32 {
    unsafe {
        use std::ffi::CStr;

        use crate::{
            encoding::{XmlCharEncoding, find_encoding_handler},
            libxml::parser::xml_init_parser,
        };

        xml_init_parser();

        let handler = if let Some(Ok(enc)) =
            (!encoding.is_null()).then(|| CStr::from_ptr(encoding).to_str())
        {
            let e = enc.parse::<XmlCharEncoding>();
            if !matches!(e, Ok(XmlCharEncoding::UTF8)) {
                let handler = find_encoding_handler(enc);
                if handler.is_none() {
                    html_save_err(XmlParserErrors::XmlSaveUnknownEncoding, None, Some(enc));
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
}

/// Dump an HTML document to a file using a given encoding and formatting returns/spaces are added.
///
/// returns: the number of byte written or -1 in case of failure.
#[doc(alias = "htmlSaveFileEnc")]
#[cfg(feature = "libxml_output")]
pub unsafe fn html_save_file_enc(filename: &str, cur: XmlDocPtr, encoding: Option<&str>) -> i32 {
    unsafe { html_save_file_format(filename, cur, encoding, 1) }
}

/// Dump an HTML document to a file using a given encoding.
///
/// returns: the number of byte written or -1 in case of failure.
#[doc(alias = "htmlSaveFileFormat")]
#[cfg(feature = "libxml_output")]
pub unsafe fn html_save_file_format(
    filename: &str,
    cur: XmlDocPtr,
    encoding: Option<&str>,
    format: i32,
) -> i32 {
    unsafe {
        use std::{cell::RefCell, rc::Rc};

        use crate::{
            encoding::{XmlCharEncoding, find_encoding_handler},
            libxml::parser::xml_init_parser,
        };

        // if cur.is_null() {
        //     return -1;
        // }

        xml_init_parser();

        let handler = if let Some(enc) = encoding.as_ref() {
            let e = enc.parse::<XmlCharEncoding>();
            let handler = if !matches!(e, Ok(XmlCharEncoding::UTF8)) {
                let handler = find_encoding_handler(enc);
                if handler.is_none() {
                    html_save_err(XmlParserErrors::XmlSaveUnknownEncoding, None, Some(enc));
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
        html_doc_content_dump_format_output(&mut buf, Some(cur), encoding, format);

        if buf.error.is_ok() {
            buf.flush();
            buf.written
        } else {
            -1
        }
    }
}

/// TODO: check whether encoding is needed
///
/// Dump the HTML document DTD, if any.
#[doc(alias = "htmlDtdDumpOutput")]
#[cfg(feature = "libxml_output")]
unsafe fn html_dtd_dump_output(
    buf: &mut XmlOutputBuffer,
    doc: XmlDocPtr,
    _encoding: *const c_char,
) {
    unsafe {
        let Some(cur) = doc.int_subset else {
            html_save_err(XmlParserErrors::XmlSaveNoDoctype, Some(doc.into()), None);
            return;
        };

        buf.write_str("<!DOCTYPE ").ok();
        buf.write_str(cur.name.as_deref().unwrap()).ok();
        if let Some(external_id) = cur.external_id.as_deref() {
            buf.write_str(" PUBLIC ").ok();
            if let Some(mut buf) = buf.buffer {
                let external_id = CString::new(external_id).unwrap();
                buf.push_quoted_cstr(&external_id).ok();
            }
            if let Some(system_id) = cur.system_id.as_deref() {
                buf.write_str(" ").ok();
                if let Some(mut buf) = buf.buffer {
                    let system_id = CString::new(system_id).unwrap();
                    buf.push_quoted_cstr(&system_id).ok();
                }
            }
        } else if let Some(system_id) = cur
            .system_id
            .as_deref()
            .filter(|&s| s != "about:legacy-compat")
        {
            buf.write_str(" SYSTEM ").ok();
            if let Some(mut buf) = buf.buffer {
                let system_id = CString::new(system_id).unwrap();
                buf.push_quoted_cstr(&system_id).ok();
            }
        }
        buf.write_str(">\n").ok();
    }
}

/// Dump an HTML attribute
#[doc(alias = "htmlAttrDumpOutput")]
#[cfg(feature = "libxml_output")]
unsafe fn html_attr_dump_output(buf: &mut XmlOutputBuffer, doc: Option<XmlDocPtr>, cur: &XmlAttr) {
    unsafe {
        use std::ffi::CStr;

        use crate::{libxml::chvalid::xml_is_blank_char, uri::escape_url_except};

        // The html output method should not escape a & character
        // occurring in an attribute value immediately followed by
        // a { character (see Section B.7.1 of the HTML 4.0 Recommendation).
        // This is implemented in xmlEncodeEntitiesReentrant

        buf.write_str(" ").ok();
        if let Some(prefix) = cur.ns.as_deref().and_then(|ns| ns.prefix()) {
            buf.write_str(&prefix).ok();
            buf.write_str(":").ok();
        }

        buf.write_str(CStr::from_ptr(cur.name as _).to_string_lossy().as_ref())
            .ok();
        if let Some(children) = cur
            .children()
            .filter(|_| html_is_boolean_attr(cur.name as _) == 0)
        {
            if let Some(value) = children.get_string(doc, 0) {
                buf.write_str("=").ok();
                if cur.ns.is_none()
                    && cur
                        .parent()
                        .map(|parent| XmlNodePtr::try_from(parent).unwrap())
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
                        buf.push_quoted_cstr(&escaped).ok();
                    }
                } else if let Some(mut buf) = buf.buffer {
                    let value = CString::new(value.as_str()).unwrap();
                    buf.push_quoted_cstr(&value).ok();
                }
            } else {
                buf.write_str("=\"\"").ok();
            }
        }
    }
}

/// Dump an HTML node, recursive behaviour,children are printed too.
#[doc(alias = "htmlNodeDumpFormatOutput")]
#[cfg(feature = "libxml_output")]
pub unsafe fn html_node_dump_format_output(
    buf: &mut XmlOutputBuffer,
    doc: Option<XmlDocPtr>,
    cur: Option<XmlGenericNodePtr>,
    _encoding: Option<&str>,
    format: i32,
) {
    unsafe {
        use std::ffi::CStr;

        use crate::{
            libxml::{
                globals::xml_free,
                htmlparser::html_tag_lookup,
                parser::xml_init_parser,
                parser_internals::{XML_STRING_TEXT, XML_STRING_TEXT_NOENC},
            },
            save::xml_ns_list_dump_output,
            tree::{XmlNodePtr, xml_encode_entities_reentrant},
        };

        xml_init_parser();

        let Some(mut cur) = cur else {
            return;
        };

        let root = cur;
        let mut parent = cur.parent();
        'main: loop {
            match cur.element_type() {
                XmlElementType::XmlHTMLDocumentNode | XmlElementType::XmlDocumentNode => {
                    let doc = XmlDocPtr::try_from(cur).unwrap();
                    if doc.int_subset.is_some() {
                        html_dtd_dump_output(buf, doc, null_mut());
                    }
                    if let Some(children) = cur.children() {
                        // Always validate cur.parent when descending.
                        if cur.parent() == parent {
                            parent = Some(doc.into());
                            cur = children;
                            continue;
                        }
                    } else {
                        buf.write_str("\n").ok();
                    }
                }

                XmlElementType::XmlElementNode => 'to_break: {
                    let node = XmlNodePtr::try_from(cur).unwrap();
                    // Some users like lxml are known to pass nodes with a corrupted
                    // tree structure. Fall back to a recursive call to handle this case.
                    if node.parent() != parent && node.children().is_some() {
                        html_node_dump_format_output(buf, doc, Some(cur), _encoding, format);
                        break 'to_break;
                    }

                    // Get specific HTML info for that node.
                    let info = if node.ns.is_none() {
                        html_tag_lookup(node.name().as_deref().unwrap())
                    } else {
                        None
                    };

                    buf.write_str("<").ok();
                    if let Some(prefix) = node.ns.as_deref().and_then(|ns| ns.prefix()) {
                        buf.write_str(&prefix).ok();
                        buf.write_str(":").ok();
                    }

                    buf.write_str(CStr::from_ptr(node.name as _).to_string_lossy().as_ref())
                        .ok();
                    if let Some(ns_def) = node.ns_def {
                        xml_ns_list_dump_output(buf, Some(ns_def));
                    }
                    let mut attr = node.properties;
                    while let Some(now) = attr {
                        html_attr_dump_output(buf, doc, &now);
                        attr = now.next;
                    }

                    if info.is_some_and(|info| info.empty != 0) {
                        buf.write_str(">").ok();
                    } else if let Some(children) = node.children() {
                        buf.write_str(">").ok();
                        if format != 0
                            && info.is_some_and(|info| info.isinline == 0)
                            && !matches!(
                                children.element_type(),
                                HTML_TEXT_NODE | HTML_ENTITY_REF_NODE
                            )
                            && node.children() != node.last()
                            && !node.name.is_null()
                            && *node.name.add(0) != b'p'
                        {
                            // p, pre, param
                            buf.write_str("\n").ok();
                        }
                        parent = Some(node.into());
                        cur = children;
                        continue 'main;
                    } else if info.is_some_and(|info| {
                        info.save_end_tag != 0 && info.name != "html" && info.name != "body"
                    }) {
                        buf.write_str(">").ok();
                    } else {
                        buf.write_str("></").ok();
                        if let Some(prefix) = node.ns.as_deref().and_then(|ns| ns.prefix()) {
                            buf.write_str(&prefix).ok();
                            buf.write_str(":").ok();
                        }

                        buf.write_str(CStr::from_ptr(node.name as _).to_string_lossy().as_ref())
                            .ok();
                        buf.write_str(">").ok();
                    }

                    if format != 0
                        && node.next().is_some()
                        && info.is_some_and(|info| info.isinline == 0)
                        && !matches!(
                            node.next().unwrap().element_type(),
                            HTML_TEXT_NODE | HTML_ENTITY_REF_NODE
                        )
                        && parent
                            .as_deref()
                            .and_then(|parent| parent.name())
                            .is_some_and(|name| !name.starts_with('p'))
                    {
                        buf.write_str("\n").ok();
                    }
                }
                XmlElementType::XmlAttributeNode => {
                    let attr = XmlAttrPtr::try_from(cur).unwrap();
                    html_attr_dump_output(buf, doc, &attr);
                }

                HTML_TEXT_NODE => 'to_break: {
                    let node = XmlNodePtr::try_from(cur).unwrap();
                    if node.content.is_null() {
                        break 'to_break;
                    }
                    if (node.name == XML_STRING_TEXT.as_ptr() as _
                        || node.name != XML_STRING_TEXT_NOENC.as_ptr() as _)
                        && parent.is_none_or(|parent| {
                            !parent.name().unwrap().eq_ignore_ascii_case("script")
                                && !parent.name().unwrap().eq_ignore_ascii_case("style")
                        })
                    {
                        let buffer: *mut XmlChar = xml_encode_entities_reentrant(doc, node.content);
                        if !buffer.is_null() {
                            buf.write_str(CStr::from_ptr(buffer as _).to_string_lossy().as_ref())
                                .ok();
                            xml_free(buffer as _);
                        }
                    } else {
                        buf.write_str(CStr::from_ptr(node.content as _).to_string_lossy().as_ref())
                            .ok();
                    }
                }

                HTML_COMMENT_NODE => {
                    let node = XmlNodePtr::try_from(cur).unwrap();
                    if !node.content.is_null() {
                        buf.write_str("<!--").ok();

                        buf.write_str(CStr::from_ptr(node.content as _).to_string_lossy().as_ref())
                            .ok();
                        buf.write_str("-->").ok();
                    }
                }

                HTML_PI_NODE => {
                    let node = XmlNodePtr::try_from(cur).unwrap();
                    if !node.name.is_null() {
                        buf.write_str("<?").ok();

                        buf.write_str(CStr::from_ptr(node.name as _).to_string_lossy().as_ref())
                            .ok();
                        if !node.content.is_null() {
                            buf.write_str(" ").ok();

                            buf.write_str(
                                CStr::from_ptr(node.content as _).to_string_lossy().as_ref(),
                            )
                            .ok();
                        }
                        buf.write_str(">").ok();
                    }
                }
                HTML_ENTITY_REF_NODE => {
                    let node = XmlNodePtr::try_from(cur).unwrap();
                    buf.write_str("&").ok();

                    buf.write_str(CStr::from_ptr(node.name as _).to_string_lossy().as_ref())
                        .ok();
                    buf.write_str(";").ok();
                }
                HTML_PRESERVE_NODE => {
                    let node = XmlNodePtr::try_from(cur).unwrap();
                    if !node.content.is_null() {
                        buf.write_str(CStr::from_ptr(node.content as _).to_string_lossy().as_ref())
                            .ok();
                    }
                }
                _ => {}
            }

            loop {
                if cur == root {
                    return;
                }
                if let Some(next) = cur.next() {
                    cur = next;
                    break;
                }

                cur = parent.unwrap();
                // cur.parent was validated when descending.
                parent = cur.parent();

                if matches!(
                    cur.element_type(),
                    XmlElementType::XmlHTMLDocumentNode | XmlElementType::XmlDocumentNode
                ) {
                    buf.write_str("\n").ok();
                } else {
                    // Is this convertion OK ?????
                    let node = XmlNodePtr::try_from(cur).unwrap();
                    let info = if format != 0 && node.ns.is_none() {
                        html_tag_lookup(node.name().as_deref().unwrap())
                    } else {
                        None
                    };

                    if format != 0
                        && info.is_some_and(|info| info.isinline == 0)
                        && !matches!(
                            node.last().unwrap().element_type(),
                            HTML_TEXT_NODE | HTML_ENTITY_REF_NODE
                        )
                        && node.children() != node.last()
                        && !node.name.is_null()
                        && *node.name.add(0) != b'p'
                    {
                        // p, pre, param
                        buf.write_str("\n").ok();
                    }

                    buf.write_str("</").ok();
                    if let Some(prefix) = node.ns.as_deref().and_then(|ns| ns.prefix()) {
                        buf.write_str(&prefix).ok();
                        buf.write_str(":").ok();
                    }

                    buf.write_str(CStr::from_ptr(node.name as _).to_string_lossy().as_ref())
                        .ok();
                    buf.write_str(">").ok();

                    if format != 0
                        && info.is_some_and(|info| info.isinline == 0)
                        && node.next().is_some()
                        && !matches!(
                            node.next().unwrap().element_type(),
                            HTML_TEXT_NODE | HTML_ENTITY_REF_NODE
                        )
                        && parent
                            .as_deref()
                            .and_then(|parent| parent.name())
                            .is_some_and(|name| !name.starts_with('p'))
                    {
                        buf.write_str("\n").ok();
                    }
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
    cur: Option<XmlDocPtr>,
    _encoding: *const c_char,
) {
    unsafe {
        html_node_dump_format_output(buf, cur, cur.map(|cur| cur.into()), None, 1);
    }
}

/// Dump an HTML document.
#[doc(alias = "htmlDocContentDumpFormatOutput")]
#[cfg(feature = "libxml_output")]
pub unsafe fn html_doc_content_dump_format_output(
    buf: &mut XmlOutputBuffer,
    cur: Option<XmlDocPtr>,
    _encoding: Option<&str>,
    format: i32,
) {
    unsafe {
        if let Some(mut cur) = cur {
            let typ = cur.typ;
            cur.typ = XmlElementType::XmlHTMLDocumentNode;
            html_node_dump_format_output(buf, Some(cur), Some(cur.into()), None, format);
            cur.typ = typ;
        } else {
            html_node_dump_format_output(buf, cur, cur.map(|cur| cur.into()), None, format);
        }
    }
}

/// Dump an HTML node, recursive behaviour,children are printed too,
/// and formatting returns/spaces are added.
#[doc(alias = "htmlNodeDumpOutput")]
#[cfg(feature = "libxml_output")]
pub unsafe fn html_node_dump_output(
    buf: &mut XmlOutputBuffer,
    doc: Option<XmlDocPtr>,
    cur: XmlGenericNodePtr,
    _encoding: *const c_char,
) {
    unsafe {
        html_node_dump_format_output(buf, doc, Some(cur), None, 1);
    }
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
    unsafe {
        let mut i: usize = 0;

        while !HTML_BOOLEAN_ATTRS[i].is_null() {
            if xml_strcasecmp(HTML_BOOLEAN_ATTRS[i] as _, name) == 0 {
                return 1;
            }
            i += 1;
        }
        0
    }
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

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
}
