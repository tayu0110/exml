//! Provide methods and data structures for handling HTML tree.  
//! This module is based on `libxml/HTMLtree.h`, `HTMLtree.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    ffi::{c_char, c_int},
    mem::size_of,
    ptr::{null, null_mut},
    sync::atomic::Ordering,
};

use libc::{memset, size_t, snprintf, FILE};

use super::{
    globals::{xml_malloc, xml_register_node_default_value},
    htmlparser::{html_err_memory, HtmlDocPtr, HtmlNodePtr},
    tree::{
        xml_add_child, xml_add_prev_sibling, xml_create_int_subset, xml_free_node,
        xml_new_doc_node, xml_new_prop, xml_set_prop, xml_unlink_node, XmlAttrPtr, XmlBufPtr,
        XmlDoc, XmlDocProperties, XmlDocPtr, XmlElementType, XmlNodePtr, __XML_REGISTER_CALLBACKS,
    },
    xml_io::XmlOutputBufferPtr,
    xmlerror::XmlParserErrors,
    xmlstring::{xml_str_equal, xml_strcasecmp, xml_strcasestr, xml_strstr, XmlChar},
};

/**
 * HTML_TEXT_NODE:
 *
 * Macro. A text node in a HTML document is really implemented
 * the same way as a text node in an XML document.
 */
const HTML_TEXT_NODE: XmlElementType = XmlElementType::XmlTextNode;
/**
 * HTML_ENTITY_REF_NODE:
 *
 * Macro. An entity reference in a HTML document is really implemented
 * the same way as an entity reference in an XML document.
 */
const HTML_ENTITY_REF_NODE: XmlElementType = XmlElementType::XmlEntityRefNode;
/**
 * HTML_COMMENT_NODE:
 *
 * Macro. A comment in a HTML document is really implemented
 * the same way as a comment in an XML document.
 */
const HTML_COMMENT_NODE: XmlElementType = XmlElementType::XmlCommentNode;
/**
 * HTML_PRESERVE_NODE:
 *
 * Macro. A preserved node in a HTML document is really implemented
 * the same way as a CDATA section in an XML document.
 */
const HTML_PRESERVE_NODE: XmlElementType = XmlElementType::XmlCdataSectionNode;
/**
 * HTML_PI_NODE:
 *
 * Macro. A processing instruction in a HTML document is really implemented
 * the same way as a processing instruction in an XML document.
 */
const HTML_PI_NODE: XmlElementType = XmlElementType::XmlPiNode;

/**
 * htmlNewDoc:
 * @URI:  URI for the dtd, or NULL
 * @ExternalID:  the external ID of the DTD, or NULL
 *
 * Creates a new HTML document
 *
 * Returns a new document
 */
pub unsafe extern "C" fn html_new_doc(
    uri: *const XmlChar,
    external_id: *const XmlChar,
) -> HtmlDocPtr {
    if uri.is_null() && external_id.is_null() {
        return html_new_doc_no_dtd(
            c"http://www.w3.org/TR/REC-html40/loose.dtd".as_ptr() as _,
            c"-//W3C//DTD HTML 4.0 Transitional//EN".as_ptr() as _,
        );
    }

    html_new_doc_no_dtd(uri, external_id)
}

/**
 * htmlNewDocNoDtD:
 * @URI:  URI for the dtd, or NULL
 * @ExternalID:  the external ID of the DTD, or NULL
 *
 * Creates a new HTML document without a DTD node if @URI and @ExternalID
 * are NULL
 *
 * Returns a new document, do not initialize the DTD if not provided
 */
pub unsafe extern "C" fn html_new_doc_no_dtd(
    uri: *const XmlChar,
    external_id: *const XmlChar,
) -> HtmlDocPtr {
    /*
     * Allocate a new document and fill the fields.
     */
    let cur: XmlDocPtr = xml_malloc(size_of::<XmlDoc>()) as XmlDocPtr;
    if cur.is_null() {
        html_err_memory(null_mut(), c"HTML document creation failed\n".as_ptr() as _);
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlDoc>());

    (*cur).typ = XmlElementType::XmlHtmlDocumentNode;
    (*cur).version = null_mut();
    (*cur).int_subset = null_mut();
    (*cur).doc = cur;
    (*cur).name = null_mut();
    (*cur).children = null_mut();
    (*cur).ext_subset = null_mut();
    (*cur).old_ns = null_mut();
    (*cur).encoding = null_mut();
    (*cur).standalone = 1;
    (*cur).compression = 0;
    (*cur).ids = null_mut();
    (*cur).refs = null_mut();
    (*cur)._private = null_mut();
    (*cur).charset = crate::encoding::XmlCharEncoding::UTF8;
    (*cur).properties =
        XmlDocProperties::XmlDocHtml as i32 | XmlDocProperties::XmlDocUserbuilt as i32;
    if !external_id.is_null() || !uri.is_null() {
        xml_create_int_subset(cur, c"html".as_ptr() as _, external_id, uri);
    }
    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    /* && xmlRegisterNodeDefaultValue() */
    {
        xml_register_node_default_value(cur as XmlNodePtr);
    }
    cur
}

/**
 * htmlGetMetaEncoding:
 * @doc:  the document
 *
 * Encoding definition lookup in the Meta tags
 *
 * Returns the current encoding as flagged in the HTML source
 */
pub unsafe extern "C" fn html_get_meta_encoding(doc: HtmlDocPtr) -> *const XmlChar {
    let mut cur: HtmlNodePtr;
    let mut content: *const XmlChar;
    let mut encoding: *const XmlChar;

    if doc.is_null() {
        return null_mut();
    }
    cur = (*doc).children;

    /*
     * Search the html
     */
    'goto_found_meta: {
        'goto_found_head: {
            while !cur.is_null() {
                if matches!((*cur).typ, XmlElementType::XmlElementNode) && !(*cur).name.is_null() {
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
                cur = (*cur).next;
            }
            if cur.is_null() {
                return null_mut();
            }
            cur = (*cur).children;
            /*
             * Search the head
             */
            while !cur.is_null() {
                if matches!((*cur).typ, XmlElementType::XmlElementNode) && !(*cur).name.is_null() {
                    if xml_str_equal((*cur).name, c"head".as_ptr() as _) {
                        break;
                    }
                    if xml_str_equal((*cur).name, c"meta".as_ptr() as _) {
                        break 'goto_found_meta;
                    }
                }
                cur = (*cur).next;
            }
            if cur.is_null() {
                return null_mut();
            }
        }
        // found_head:
        cur = (*cur).children;
    }

    /*
     * Search the meta elements
     */
    // found_meta:

    while !cur.is_null() {
        if (matches!((*cur).typ, XmlElementType::XmlElementNode) && !(*cur).name.is_null())
            && xml_str_equal((*cur).name, c"meta".as_ptr() as _)
        {
            let mut attr: XmlAttrPtr = (*cur).properties;
            let mut http: c_int;
            let mut value: *const XmlChar;

            content = null_mut();
            http = 0;
            while !attr.is_null() {
                if !(*attr).children.is_null()
                    && matches!((*(*attr).children).typ, XmlElementType::XmlTextNode)
                    && (*(*attr).children).next.is_null()
                {
                    value = (*(*attr).children).content;
                    if xml_strcasecmp((*attr).name, c"http-equiv".as_ptr() as _) == 0
                        && xml_strcasecmp(value, c"Content-Type".as_ptr() as _) == 0
                    {
                        http = 1;
                    } else if !value.is_null()
                        && xml_strcasecmp((*attr).name, c"content".as_ptr() as _) == 0
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
                        return encoding;
                    }
                }
                attr = (*attr).next;
            }
        }
        cur = (*cur).next;
    }
    null_mut()
}

/**
 * htmlSetMetaEncoding:
 * @doc:  the document
 * @encoding:  the encoding string
 *
 * Sets the current encoding in the Meta tags
 * NOTE: this will not change the document content encoding, just
 * the META flag associated.
 *
 * Returns 0 in case of success and -1 in case of error
 */
pub unsafe extern "C" fn html_set_meta_encoding(
    doc: HtmlDocPtr,
    encoding: *const XmlChar,
) -> c_int {
    let mut cur: HtmlNodePtr;
    let mut meta: HtmlNodePtr = null_mut();
    let mut head: HtmlNodePtr = null_mut();
    let mut content: *const XmlChar = null();
    let mut newcontent: [c_char; 100] = [0; 100];

    newcontent[0] = 0;

    if doc.is_null() {
        return -1;
    }

    /* html isn't a real encoding it's just libxml2 way to get entities */
    if xml_strcasecmp(encoding, c"html".as_ptr() as _) == 0 {
        return -1;
    }

    if !encoding.is_null() {
        snprintf(
            newcontent.as_mut_ptr() as _,
            newcontent.len(),
            c"text/html; charset=%s".as_ptr() as _,
            encoding,
        );
        newcontent[newcontent.len() - 1] = 0;
    }

    cur = (*doc).children;

    let mut found_head = false;
    let mut found_meta = false;
    /*
     * Search the html
     */
    while !cur.is_null() {
        if matches!((*cur).typ, XmlElementType::XmlElementNode) && !(*cur).name.is_null() {
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

        cur = (*cur).next;
    }

    if !found_head && !found_meta {
        if cur.is_null() {
            return -1;
        }
        cur = (*cur).children;

        /*
         * Search the head
         */
        while !cur.is_null() {
            if matches!((*cur).typ, XmlElementType::XmlElementNode) && !(*cur).name.is_null() {
                if xml_strcasecmp((*cur).name, c"head".as_ptr() as _) == 0 {
                    break;
                }
                if xml_strcasecmp((*cur).name, c"meta".as_ptr() as _) == 0 {
                    head = (*cur).parent;
                    // goto found_meta;
                    found_meta = true;
                }
            }
            cur = (*cur).next;
        }
        if cur.is_null() {
            return -1;
        }
    }

    let create = |mut meta: *mut super::tree::XmlNode,
                  encoding: *const u8,
                  head: *mut super::tree::XmlNode,
                  newcontent: [i8; 100],
                  content: *const u8| {
        if meta.is_null() {
            if !encoding.is_null() && !head.is_null() {
                /*
                 * Create a new Meta element with the right attributes
                 */

                meta = xml_new_doc_node(doc, null_mut(), c"meta".as_ptr() as _, null_mut());
                if (*head).children.is_null() {
                    xml_add_child(head, meta);
                } else {
                    xml_add_prev_sibling((*head).children, meta);
                }
                xml_new_prop(
                    meta,
                    c"http-equiv".as_ptr() as _,
                    c"Content-Type".as_ptr() as _,
                );
                xml_new_prop(meta, c"content".as_ptr() as _, newcontent.as_ptr() as _);
            }
        } else {
            /* remove the meta tag if NULL is passed */
            if encoding.is_null() {
                xml_unlink_node(meta);
                xml_free_node(meta);
            }
            /* change the document only if there is a real encoding change */
            else if xml_strcasestr(content, encoding).is_null() {
                xml_set_prop(meta, c"content".as_ptr() as _, newcontent.as_ptr() as _);
            }
        }

        0
    };

    // found_head:

    if !found_meta {
        head = cur;
        assert!(!cur.is_null());
        if (*cur).children.is_null() {
            // goto create;
            return create(meta, encoding, head, newcontent, content);
        }
        cur = (*cur).children;
    }

    // found_meta:
    /*
     * Search and update all the remaining the meta elements carrying
     * encoding information
     */
    while !cur.is_null() {
        if (matches!((*cur).typ, XmlElementType::XmlElementNode) && !(*cur).name.is_null())
            && (xml_strcasecmp((*cur).name, c"meta".as_ptr() as _) == 0)
        {
            let mut attr: XmlAttrPtr = (*cur).properties;
            let mut http: c_int;
            let mut value: *const XmlChar;

            content = null_mut();
            http = 0;
            while !attr.is_null() {
                if !(*attr).children.is_null()
                    && matches!((*(*attr).children).typ, XmlElementType::XmlTextNode)
                    && (*(*attr).children).next.is_null()
                {
                    value = (*(*attr).children).content;
                    if xml_strcasecmp((*attr).name, c"http-equiv".as_ptr() as _) == 0
                        && xml_strcasecmp(value, c"Content-Type".as_ptr() as _) == 0
                    {
                        http = 1;
                    } else if !value.is_null()
                        && xml_strcasecmp((*attr).name, c"content".as_ptr() as _) == 0
                    {
                        content = value;
                    }
                    if http != 0 && !content.is_null() {
                        break;
                    }
                }
                attr = (*attr).next;
            }
            if http != 0 && !content.is_null() {
                meta = cur;
                break;
            }
        }
        cur = (*cur).next;
    }
    // create:
    create(meta, encoding, head, newcontent, content)
}

/**
 * htmlDocDumpMemory:
 * @cur:  the document
 * @mem:  OUT: the memory pointer
 * @size:  OUT: the memory length
 *
 * Dump an HTML document in memory and return the xmlChar * and it's size.
 * It's up to the caller to free the memory.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn html_doc_dump_memory(
    cur: XmlDocPtr,
    mem: *mut *mut XmlChar,
    size: *mut c_int,
) {
    html_doc_dump_memory_format(cur, mem, size, 1);
}

/**
 * htmlSaveErr:
 * @code:  the error number
 * @node:  the location of the error.
 * @extra:  extra information
 *
 * Handle an out of memory condition
 */
#[cfg(feature = "output")]
unsafe extern "C" fn html_save_err(code: XmlParserErrors, node: XmlNodePtr, extra: *const c_char) {
    use crate::{error::XmlErrorDomain, private::error::__xml_simple_error};

    use super::xmlerror::XmlParserErrors;

    let msg = match code {
        XmlParserErrors::XmlSaveNotUtf8 => c"string is not in UTF-8\n".as_ptr() as _,
        XmlParserErrors::XmlSaveCharInvalid => c"invalid character value\n".as_ptr() as _,
        XmlParserErrors::XmlSaveUnknownEncoding => c"unknown encoding %s\n".as_ptr() as _,
        XmlParserErrors::XmlSaveNoDoctype => c"HTML has no DOCTYPE\n".as_ptr() as _,
        _ => c"unexpected error number\n".as_ptr() as _,
    };
    __xml_simple_error(XmlErrorDomain::XmlFromOutput, code, node, msg, extra);
}

/**
 * htmlDocDumpMemoryFormat:
 * @cur:  the document
 * @mem:  OUT: the memory pointer
 * @size:  OUT: the memory length
 * @format:  should formatting spaces been added
 *
 * Dump an HTML document in memory and return the xmlChar * and it's size.
 * It's up to the caller to free the memory.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn html_doc_dump_memory_format(
    cur: XmlDocPtr,
    mem: *mut *mut XmlChar,
    size: *mut c_int,
    format: c_int,
) {
    use std::{cell::RefCell, ffi::CStr, rc::Rc};

    use crate::{
        encoding::find_encoding_handler,
        libxml::{
            parser::xml_init_parser,
            xml_io::{
                xml_alloc_output_buffer_internal, xml_output_buffer_close, xml_output_buffer_flush,
            },
            xmlerror::XmlParserErrors,
            xmlstring::xml_strndup,
        },
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

    let encoding: *const c_char = html_get_meta_encoding(cur) as _;

    let handler =
        if let Some(Ok(enc)) = (!encoding.is_null()).then(|| CStr::from_ptr(encoding).to_str()) {
            let e = enc.parse::<crate::encoding::XmlCharEncoding>();
            if !matches!(e, Ok(crate::encoding::XmlCharEncoding::UTF8)) {
                let handler = find_encoding_handler(enc);
                if handler.is_none() {
                    html_save_err(
                        XmlParserErrors::XmlSaveUnknownEncoding,
                        null_mut(),
                        encoding,
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

    let buf: XmlOutputBufferPtr =
        xml_alloc_output_buffer_internal(handler.map(|e| Rc::new(RefCell::new(e))));
    if buf.is_null() {
        *mem = null_mut();
        *size = 0;
        return;
    }

    html_doc_content_dump_format_output(buf, cur, null_mut(), format);

    xml_output_buffer_flush(buf);
    if let Some(conv) = (*buf).conv {
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
        *size = (*buf).buffer.map_or(0, |buf| buf.len() as i32);
        *mem = xml_strndup(
            (*buf).buffer.map_or(null(), |buf| {
                if buf.is_ok() {
                    buf.as_ref().as_ptr()
                } else {
                    null()
                }
            }),
            *size,
        );
    }
    xml_output_buffer_close(buf);
}

/**
 * htmlDocDump:
 * @f:  the FILE*
 * @cur:  the document
 *
 * Dump an HTML document to an open FILE.
 *
 * returns: the number of byte written or -1 in case of failure.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn html_doc_dump(f: *mut FILE, cur: XmlDocPtr) -> c_int {
    use std::ffi::CStr;

    use crate::{
        encoding::find_encoding_handler,
        libxml::{
            parser::xml_init_parser,
            xml_io::{xml_output_buffer_close, xml_output_buffer_create_file},
            xmlerror::XmlParserErrors,
        },
    };

    xml_init_parser();

    if cur.is_null() || f.is_null() {
        return -1;
    }

    let encoding: *const c_char = html_get_meta_encoding(cur) as _;

    let handler =
        if let Some(Ok(enc)) = (!encoding.is_null()).then(|| CStr::from_ptr(encoding).to_str()) {
            let e = enc.parse::<crate::encoding::XmlCharEncoding>();
            if !matches!(e, Ok(crate::encoding::XmlCharEncoding::UTF8)) {
                let handler = find_encoding_handler(enc);
                if handler.is_none() {
                    html_save_err(
                        XmlParserErrors::XmlSaveUnknownEncoding,
                        null_mut(),
                        encoding,
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

    let buf: XmlOutputBufferPtr = xml_output_buffer_create_file(f, handler);
    if buf.is_null() {
        return -1;
    }
    html_doc_content_dump_output(buf, cur, null_mut());

    xml_output_buffer_close(buf)
}

/**
 * htmlSaveFile:
 * @filename:  the filename (or URL)
 * @cur:  the document
 *
 * Dump an HTML document to a file. If @filename is "-" the stdout file is
 * used.
 * returns: the number of byte written or -1 in case of failure.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn html_save_file(filename: *const c_char, cur: XmlDocPtr) -> c_int {
    use std::{cell::RefCell, ffi::CStr, rc::Rc};

    use crate::{
        encoding::find_encoding_handler,
        libxml::{
            parser::xml_init_parser,
            xml_io::{xml_output_buffer_close, xml_output_buffer_create_filename},
            xmlerror::XmlParserErrors,
        },
    };

    if cur.is_null() || filename.is_null() {
        return -1;
    }

    xml_init_parser();

    let encoding: *const c_char = html_get_meta_encoding(cur) as _;

    let handler =
        if let Some(Ok(enc)) = (!encoding.is_null()).then(|| CStr::from_ptr(encoding).to_str()) {
            let e = enc.parse::<crate::encoding::XmlCharEncoding>();
            if !matches!(e, Ok(crate::encoding::XmlCharEncoding::UTF8)) {
                let handler = find_encoding_handler(enc);
                if handler.is_none() {
                    html_save_err(
                        XmlParserErrors::XmlSaveUnknownEncoding,
                        null_mut(),
                        encoding,
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

    /*
     * save the content to a temp buffer.
     */
    let buf: XmlOutputBufferPtr = xml_output_buffer_create_filename(
        filename,
        handler.map(|e| Rc::new(RefCell::new(e))),
        (*cur).compression,
    );
    if buf.is_null() {
        return 0;
    }

    html_doc_content_dump_output(buf, cur, null_mut());

    xml_output_buffer_close(buf)
}

/**
 * htmlSaveErrMemory:
 * @extra:  extra information
 *
 * Handle an out of memory condition
 */
#[cfg(feature = "output")]
unsafe extern "C" fn html_save_err_memory(extra: *const c_char) {
    use crate::{
        error::XmlErrorDomain, libxml::xmlerror::XmlParserErrors,
        private::error::__xml_simple_error,
    };

    __xml_simple_error(
        XmlErrorDomain::XmlFromOutput,
        XmlParserErrors::XmlErrNoMemory,
        null_mut(),
        null_mut(),
        extra,
    );
}

/**
 * htmlBufNodeDumpFormat:
 * @buf:  the xmlBufPtr output
 * @doc:  the document
 * @cur:  the current node
 * @format:  should formatting spaces been added
 *
 * Dump an HTML node, recursive behaviour,children are printed too.
 *
 * Returns the number of byte written or -1 in case of error
 */
#[cfg(feature = "output")]
unsafe extern "C" fn html_buf_node_dump_format(
    buf: XmlBufPtr,
    doc: XmlDocPtr,
    cur: XmlNodePtr,
    format: c_int,
) -> size_t {
    use crate::{
        buf::XmlBufRef,
        libxml::{globals::xml_free, tree::xml_buf_use, xml_io::XmlOutputBuffer},
    };

    if cur.is_null() {
        return usize::MAX;
    }
    if buf.is_null() {
        return usize::MAX;
    }
    let outbuf: XmlOutputBufferPtr = xml_malloc(size_of::<XmlOutputBuffer>()) as _;
    if outbuf.is_null() {
        html_save_err_memory(c"allocating HTML output buffer".as_ptr() as _);
        return usize::MAX;
    }
    memset(outbuf as _, 0, size_of::<XmlOutputBuffer>());
    (*outbuf).buffer = XmlBufRef::from_raw(buf);
    (*outbuf).encoder = None;
    (*outbuf).writecallback = None;
    (*outbuf).closecallback = None;
    (*outbuf).context = null_mut();
    (*outbuf).written = 0;

    let using: size_t = xml_buf_use(buf);
    html_node_dump_format_output(outbuf, doc, cur, null_mut(), format);
    xml_free(outbuf as _);
    (xml_buf_use(buf) as i32 - using as i32) as _
}

/**
 * htmlNodeDump:
 * @buf:  the HTML buffer output
 * @doc:  the document
 * @cur:  the current node
 *
 * Dump an HTML node, recursive behaviour,children are printed too,
 * and formatting returns are added.
 *
 * Returns the number of byte written or -1 in case of error
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn html_node_dump(buf: XmlBufPtr, doc: XmlDocPtr, cur: XmlNodePtr) -> c_int {
    use crate::libxml::parser::xml_init_parser;

    if buf.is_null() || cur.is_null() {
        return -1;
    }

    xml_init_parser();
    // let buffer: XmlBufPtr = xml_buf_from_buffer(buf);
    // if buffer.is_null() {
    //     return -1;
    // }

    // let ret: size_t = html_buf_node_dump_format(buffer, doc, cur, 1);
    let ret: size_t = html_buf_node_dump_format(buf, doc, cur, 1);

    // xml_buf_back_to_buffer(buffer);

    // if ret > i32::MAX as usize {
    //     return -1;
    // }
    ret as _
}

/**
 * htmlNodeDumpFile:
 * @out:  the FILE pointer
 * @doc:  the document
 * @cur:  the current node
 *
 * Dump an HTML node, recursive behaviour,children are printed too,
 * and formatting returns are added.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn html_node_dump_file(out: *mut FILE, doc: XmlDocPtr, cur: XmlNodePtr) {
    html_node_dump_file_format(out, doc, cur, null_mut(), 1);
}

/**
 * htmlNodeDumpFileFormat:
 * @out:  the FILE pointer
 * @doc:  the document
 * @cur:  the current node
 * @encoding: the document encoding
 * @format:  should formatting spaces been added
 *
 * Dump an HTML node, recursive behaviour,children are printed too.
 *
 * TODO: if encoding.is_null() try to save in the doc encoding
 *
 * returns: the number of byte written or -1 in case of failure.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn html_node_dump_file_format(
    out: *mut FILE,
    doc: XmlDocPtr,
    cur: XmlNodePtr,
    encoding: *const c_char,
    format: c_int,
) -> c_int {
    use std::ffi::CStr;

    use crate::{
        encoding::find_encoding_handler,
        libxml::{
            parser::xml_init_parser,
            xml_io::{xml_output_buffer_close, xml_output_buffer_create_file},
            xmlerror::XmlParserErrors,
        },
    };

    xml_init_parser();

    let handler =
        if let Some(Ok(enc)) = (!encoding.is_null()).then(|| CStr::from_ptr(encoding).to_str()) {
            let e = enc.parse::<crate::encoding::XmlCharEncoding>();
            if !matches!(e, Ok(crate::encoding::XmlCharEncoding::UTF8)) {
                let handler = find_encoding_handler(enc);
                if handler.is_none() {
                    html_save_err(
                        XmlParserErrors::XmlSaveUnknownEncoding,
                        null_mut(),
                        encoding,
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

    /*
     * save the content to a temp buffer.
     */
    let buf: XmlOutputBufferPtr = xml_output_buffer_create_file(out, handler);
    if buf.is_null() {
        return 0;
    }

    html_node_dump_format_output(buf, doc, cur, null_mut(), format);

    xml_output_buffer_close(buf)
}

/**
 * htmlSaveFileEnc:
 * @filename:  the filename
 * @cur:  the document
 * @encoding: the document encoding
 *
 * Dump an HTML document to a file using a given encoding
 * and formatting returns/spaces are added.
 *
 * returns: the number of byte written or -1 in case of failure.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn html_save_file_enc(
    filename: *const c_char,
    cur: XmlDocPtr,
    encoding: *const c_char,
) -> c_int {
    html_save_file_format(filename, cur, encoding, 1)
}

/**
 * htmlSaveFileFormat:
 * @filename:  the filename
 * @cur:  the document
 * @format:  should formatting spaces been added
 * @encoding: the document encoding
 *
 * Dump an HTML document to a file using a given encoding.
 *
 * returns: the number of byte written or -1 in case of failure.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn html_save_file_format(
    filename: *const c_char,
    cur: XmlDocPtr,
    encoding: *const c_char,
    format: c_int,
) -> c_int {
    use std::{cell::RefCell, ffi::CStr, rc::Rc};

    use crate::{
        encoding::find_encoding_handler,
        libxml::{
            parser::xml_init_parser,
            xml_io::{xml_output_buffer_close, xml_output_buffer_create_filename},
            xmlerror::XmlParserErrors,
        },
    };

    if cur.is_null() || filename.is_null() {
        return -1;
    }

    xml_init_parser();

    let handler =
        if let Some(Ok(enc)) = (!encoding.is_null()).then(|| CStr::from_ptr(encoding).to_str()) {
            let e = enc.parse::<crate::encoding::XmlCharEncoding>();
            let handler = if !matches!(e, Ok(crate::encoding::XmlCharEncoding::UTF8)) {
                let handler = find_encoding_handler(enc);
                if handler.is_none() {
                    html_save_err(
                        XmlParserErrors::XmlSaveUnknownEncoding,
                        null_mut(),
                        encoding,
                    );
                }
                handler
            } else {
                None
            };
            html_set_meta_encoding(cur, encoding as _);
            handler
        } else {
            html_set_meta_encoding(cur, c"UTF-8".as_ptr() as _);
            let handler = find_encoding_handler("HTML");
            if handler.is_some() {
                handler
            } else {
                find_encoding_handler("ascii")
            }
        };

    /*
     * save the content to a temp buffer.
     */
    let buf: XmlOutputBufferPtr =
        xml_output_buffer_create_filename(filename, handler.map(|e| Rc::new(RefCell::new(e))), 0);
    if buf.is_null() {
        return 0;
    }

    html_doc_content_dump_format_output(buf, cur, encoding, format);

    xml_output_buffer_close(buf)
}

/**
 * htmlDtdDumpOutput:
 * @buf:  the HTML buffer output
 * @doc:  the document
 * @encoding:  the encoding string
 *
 * TODO: check whether encoding is needed
 *
 * Dump the HTML document DTD, if any.
 */
#[cfg(feature = "output")]
unsafe extern "C" fn html_dtd_dump_output(
    buf: XmlOutputBufferPtr,
    doc: XmlDocPtr,
    _encoding: *const c_char,
) {
    use std::ffi::CStr;

    use crate::libxml::{
        xml_io::xml_output_buffer_write_string, xmlerror::XmlParserErrors, xmlstring::xml_strcmp,
    };

    use super::tree::XmlDtdPtr;

    let cur: XmlDtdPtr = (*doc).int_subset;

    if cur.is_null() {
        html_save_err(XmlParserErrors::XmlSaveNoDoctype, doc as _, null_mut());
        return;
    }
    xml_output_buffer_write_string(&mut *buf, c"<!DOCTYPE ".as_ptr() as _);
    xml_output_buffer_write_string(&mut *buf, (*cur).name as _);
    if !(*cur).external_id.is_null() {
        xml_output_buffer_write_string(&mut *buf, c" PUBLIC ".as_ptr() as _);
        if let Some(mut buf) = (*buf).buffer {
            buf.push_quoted_cstr(CStr::from_ptr((*cur).external_id as *const i8));
        }
        if !(*cur).system_id.is_null() {
            xml_output_buffer_write_string(&mut *buf, c" ".as_ptr() as _);
            if let Some(mut buf) = (*buf).buffer {
                buf.push_quoted_cstr(CStr::from_ptr((*cur).system_id as *const i8));
            }
        }
    } else if !(*cur).system_id.is_null()
        && xml_strcmp((*cur).system_id, c"about:legacy-compat".as_ptr() as _) != 0
    {
        xml_output_buffer_write_string(&mut *buf, c" SYSTEM ".as_ptr() as _);
        if let Some(mut buf) = (*buf).buffer {
            buf.push_quoted_cstr(CStr::from_ptr((*cur).system_id as *const i8));
        }
    }
    xml_output_buffer_write_string(&mut *buf, c">\n".as_ptr() as _);
}

/**
 * htmlAttrDumpOutput:
 * @buf:  the HTML buffer output
 * @doc:  the document
 * @cur:  the attribute pointer
 *
 * Dump an HTML attribute
 */
#[cfg(feature = "output")]
unsafe extern "C" fn html_attr_dump_output(
    buf: XmlOutputBufferPtr,
    doc: XmlDocPtr,
    cur: XmlAttrPtr,
) {
    use std::ffi::CStr;

    use crate::{
        libxml::{
            globals::xml_free, tree::xml_node_list_get_string, uri::xml_uri_escape_str,
            xml_io::xml_output_buffer_write_string,
        },
        IS_BLANK_CH,
    };

    let value: *mut XmlChar;

    /*
     * The html output method should not escape a & character
     * occurring in an attribute value immediately followed by
     * a { character (see Section B.7.1 of the HTML 4.0 Recommendation).
     * This is implemented in xmlEncodeEntitiesReentrant
     */

    if cur.is_null() {
        return;
    }
    xml_output_buffer_write_string(&mut *buf, c" ".as_ptr() as _);
    if !(*cur).ns.is_null() && !(*(*cur).ns).prefix.load(Ordering::Relaxed).is_null() {
        xml_output_buffer_write_string(&mut *buf, (*(*cur).ns).prefix.load(Ordering::Relaxed) as _);
        xml_output_buffer_write_string(&mut *buf, c":".as_ptr() as _);
    }
    xml_output_buffer_write_string(&mut *buf, (*cur).name as _);
    if !(*cur).children.is_null() && html_is_boolean_attr((*cur).name as _) == 0 {
        value = xml_node_list_get_string(doc, (*cur).children, 0);
        if !value.is_null() {
            xml_output_buffer_write_string(&mut *buf, c"=".as_ptr() as _);
            if (*cur).ns.is_null()
                && !(*cur).parent.is_null()
                && (*(*cur).parent).ns.is_null()
                && (xml_strcasecmp((*cur).name, c"href".as_ptr() as _) == 0
                    || xml_strcasecmp((*cur).name, c"action".as_ptr() as _) == 0
                    || xml_strcasecmp((*cur).name, c"src".as_ptr() as _) == 0
                    || (xml_strcasecmp((*cur).name, c"name".as_ptr() as _) == 0
                        && xml_strcasecmp((*(*cur).parent).name, c"a".as_ptr() as _) == 0))
            {
                let mut tmp: *mut XmlChar = value;

                while IS_BLANK_CH!(*tmp) {
                    tmp = tmp.add(1);
                }

                /*
                 * Angle brackets are technically illegal in URIs, but they're
                 * used in server side includes, for example. Curly brackets
                 * are illegal as well and often used in templates.
                 * Don't escape non-whitespace, printable ASCII chars for
                 * improved interoperability. Only escape space, control
                 * and non-ASCII chars.
                 */
                let escaped: *mut XmlChar =
                    xml_uri_escape_str(tmp, c"\"#$%&+,/:;<=>?@[\\]^`{|}".as_ptr() as _);
                if !escaped.is_null() {
                    if let Some(mut buf) = (*buf).buffer {
                        buf.push_quoted_cstr(CStr::from_ptr(escaped as *const i8));
                    }
                    xml_free(escaped as _);
                } else if let Some(mut buf) = (*buf).buffer {
                    buf.push_quoted_cstr(CStr::from_ptr(value as *const i8));
                }
            } else if let Some(mut buf) = (*buf).buffer {
                buf.push_quoted_cstr(CStr::from_ptr(value as *const i8));
            }
            xml_free(value as _);
        } else {
            xml_output_buffer_write_string(&mut *buf, c"=\"\"".as_ptr() as _);
        }
    }
}

/**
 * htmlNodeDumpFormatOutput:
 * @buf:  the HTML buffer output
 * @doc:  the document
 * @cur:  the current node
 * @encoding:  the encoding string (unused)
 * @format:  should formatting spaces been added
 *
 * Dump an HTML node, recursive behaviour,children are printed too.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn html_node_dump_format_output(
    buf: XmlOutputBufferPtr,
    doc: XmlDocPtr,
    mut cur: XmlNodePtr,
    _encoding: *const c_char,
    format: c_int,
) {
    use crate::{
        libxml::{
            entities::xml_encode_entities_reentrant,
            globals::xml_free,
            htmlparser::html_tag_lookup,
            parser::xml_init_parser,
            parser_internals::{XML_STRING_TEXT, XML_STRING_TEXT_NOENC},
            xml_io::xml_output_buffer_write_string,
            xmlstring::xml_strcmp,
        },
        private::save::xml_ns_list_dump_output,
    };

    use super::htmlparser::HtmlElemDesc;

    let mut parent: XmlNodePtr;
    let mut attr: XmlAttrPtr;
    let mut info: *const HtmlElemDesc;

    xml_init_parser();

    if cur.is_null() || buf.is_null() {
        return;
    }

    let root: XmlNodePtr = cur;
    parent = (*cur).parent;
    'main: loop {
        match (*cur).typ {
            XmlElementType::XmlHtmlDocumentNode | XmlElementType::XmlDocumentNode => {
                if !(*(cur as XmlDocPtr)).int_subset.is_null() {
                    html_dtd_dump_output(buf, cur as _, null_mut());
                }
                if !(*cur).children.is_null() {
                    /* Always validate (*cur).parent when descending. */
                    if (*cur).parent == parent {
                        parent = cur;
                        cur = (*cur).children;
                        continue;
                    }
                } else {
                    xml_output_buffer_write_string(&mut *buf, c"\n".as_ptr() as _);
                }
            }

            XmlElementType::XmlElementNode => 'to_break: {
                /*
                 * Some users like lxml are known to pass nodes with a corrupted
                 * tree structure. Fall back to a recursive call to handle this
                 * case.
                 */
                if (*cur).parent != parent && !(*cur).children.is_null() {
                    html_node_dump_format_output(buf, doc, cur, _encoding, format);
                    break 'to_break;
                }

                /*
                 * Get specific HTML info for that node.
                 */
                if (*cur).ns.is_null() {
                    info = html_tag_lookup((*cur).name);
                } else {
                    info = null_mut();
                }

                xml_output_buffer_write_string(&mut *buf, c"<".as_ptr() as _);
                if !(*cur).ns.is_null() && !(*(*cur).ns).prefix.load(Ordering::Relaxed).is_null() {
                    xml_output_buffer_write_string(
                        &mut *buf,
                        (*(*cur).ns).prefix.load(Ordering::Relaxed) as _,
                    );
                    xml_output_buffer_write_string(&mut *buf, c":".as_ptr() as _);
                }
                xml_output_buffer_write_string(&mut *buf, (*cur).name as _);
                if !(*cur).ns_def.is_null() {
                    xml_ns_list_dump_output(buf, (*cur).ns_def);
                }
                attr = (*cur).properties;
                while !attr.is_null() {
                    html_attr_dump_output(buf, doc, attr);
                    attr = (*attr).next;
                }

                if !info.is_null() && (*info).empty != 0 {
                    xml_output_buffer_write_string(&mut *buf, c">".as_ptr() as _);
                } else if (*cur).children.is_null() {
                    if !info.is_null()
                        && (*info).save_end_tag != 0
                        && xml_strcmp((*info).name as _, c"html".as_ptr() as _) != 0
                        && xml_strcmp((*info).name as _, c"body".as_ptr() as _) != 0
                    {
                        xml_output_buffer_write_string(&mut *buf, c">".as_ptr() as _);
                    } else {
                        xml_output_buffer_write_string(&mut *buf, c"></".as_ptr() as _);
                        if !(*cur).ns.is_null()
                            && !(*(*cur).ns).prefix.load(Ordering::Relaxed).is_null()
                        {
                            xml_output_buffer_write_string(
                                &mut *buf,
                                (*(*cur).ns).prefix.load(Ordering::Relaxed) as _,
                            );
                            xml_output_buffer_write_string(&mut *buf, c":".as_ptr() as _);
                        }
                        xml_output_buffer_write_string(&mut *buf, (*cur).name as _);
                        xml_output_buffer_write_string(&mut *buf, c">".as_ptr() as _);
                    }
                } else {
                    xml_output_buffer_write_string(&mut *buf, c">".as_ptr() as _);
                    if format != 0
                        && !info.is_null()
                        && (*info).isinline == 0
                        && !matches!(
                            (*(*cur).children).typ,
                            HTML_TEXT_NODE | HTML_ENTITY_REF_NODE
                        )
                        && (*cur).children != (*cur).last
                        && !(*cur).name.is_null()
                        && *(*cur).name.add(0) != b'p'
                    /* p, pre, param */
                    {
                        xml_output_buffer_write_string(&mut *buf, c"\n".as_ptr() as _);
                    }
                    parent = cur;
                    cur = (*cur).children;
                    continue 'main;
                }

                if (format != 0
                    && !(*cur).next.is_null()
                    && !info.is_null()
                    && (*info).isinline == 0)
                    && (!matches!((*(*cur).next).typ, HTML_TEXT_NODE | HTML_ENTITY_REF_NODE)
                        && !parent.is_null()
                        && !(*parent).name.is_null()
                        && *(*parent).name.add(0) != b'p')
                {
                    xml_output_buffer_write_string(&mut *buf, c"\n".as_ptr() as _);
                }
            }
            XmlElementType::XmlAttributeNode => {
                html_attr_dump_output(buf, doc, cur as XmlAttrPtr);
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
                        xml_output_buffer_write_string(&mut *buf, buffer as _);
                        xml_free(buffer as _);
                    }
                } else {
                    xml_output_buffer_write_string(&mut *buf, (*cur).content as _);
                }
            }

            HTML_COMMENT_NODE => {
                if !(*cur).content.is_null() {
                    xml_output_buffer_write_string(&mut *buf, c"<!--".as_ptr() as _);
                    xml_output_buffer_write_string(&mut *buf, (*cur).content as _);
                    xml_output_buffer_write_string(&mut *buf, c"-->".as_ptr() as _);
                }
            }

            HTML_PI_NODE => {
                if !(*cur).name.is_null() {
                    xml_output_buffer_write_string(&mut *buf, c"<?".as_ptr() as _);
                    xml_output_buffer_write_string(&mut *buf, (*cur).name as _);
                    if !(*cur).content.is_null() {
                        xml_output_buffer_write_string(&mut *buf, c" ".as_ptr() as _);
                        xml_output_buffer_write_string(&mut *buf, (*cur).content as _);
                    }
                    xml_output_buffer_write_string(&mut *buf, c">".as_ptr() as _);
                }
            }
            HTML_ENTITY_REF_NODE => {
                xml_output_buffer_write_string(&mut *buf, c"&".as_ptr() as _);
                xml_output_buffer_write_string(&mut *buf, (*cur).name as _);
                xml_output_buffer_write_string(&mut *buf, c";".as_ptr() as _);
            }
            HTML_PRESERVE_NODE => {
                if !(*cur).content.is_null() {
                    xml_output_buffer_write_string(&mut *buf, (*cur).content as _);
                }
            }
            _ => {}
        }

        loop {
            if cur == root {
                return;
            }
            if !(*cur).next.is_null() {
                cur = (*cur).next;
                break;
            }

            cur = parent;
            /* (*cur).parent was validated when descending. */
            parent = (*cur).parent;

            if matches!(
                (*cur).typ,
                XmlElementType::XmlHtmlDocumentNode | XmlElementType::XmlDocumentNode
            ) {
                xml_output_buffer_write_string(&mut *buf, c"\n".as_ptr() as _);
            } else {
                if format != 0 && (*cur).ns.is_null() {
                    info = html_tag_lookup((*cur).name);
                } else {
                    info = null_mut();
                }

                if format != 0
                    && !info.is_null()
                    && (*info).isinline == 0
                    && !matches!((*(*cur).last).typ, HTML_TEXT_NODE | HTML_ENTITY_REF_NODE)
                    && (*cur).children != (*cur).last
                    && !(*cur).name.is_null()
                    && *(*cur).name.add(0) != b'p'
                /* p, pre, param */
                {
                    xml_output_buffer_write_string(&mut *buf, c"\n".as_ptr() as _);
                }

                xml_output_buffer_write_string(&mut *buf, c"</".as_ptr() as _);
                if !(*cur).ns.is_null() && !(*(*cur).ns).prefix.load(Ordering::Relaxed).is_null() {
                    xml_output_buffer_write_string(
                        &mut *buf,
                        (*(*cur).ns).prefix.load(Ordering::Relaxed) as _,
                    );
                    xml_output_buffer_write_string(&mut *buf, c":".as_ptr() as _);
                }
                xml_output_buffer_write_string(&mut *buf, (*cur).name as _);
                xml_output_buffer_write_string(&mut *buf, c">".as_ptr() as _);

                if (format != 0
                    && !info.is_null()
                    && (*info).isinline == 0
                    && !(*cur).next.is_null())
                    && (!matches!((*(*cur).next).typ, HTML_TEXT_NODE | HTML_ENTITY_REF_NODE)
                        && !parent.is_null()
                        && !(*parent).name.is_null()
                        && *(*parent).name.add(0) != b'p')
                {
                    xml_output_buffer_write_string(&mut *buf, c"\n".as_ptr() as _);
                }
            }
        }
    }
}

/**
 * htmlDocContentDumpOutput:
 * @buf:  the HTML buffer output
 * @cur:  the document
 * @encoding:  the encoding string (unused)
 *
 * Dump an HTML document. Formatting return/spaces are added.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn html_doc_content_dump_output(
    buf: XmlOutputBufferPtr,
    cur: XmlDocPtr,
    _encoding: *const c_char,
) {
    html_node_dump_format_output(buf, cur, cur as _, null_mut(), 1);
}

/**
 * htmlDocContentDumpFormatOutput:
 * @buf:  the HTML buffer output
 * @cur:  the document
 * @encoding:  the encoding string (unused)
 * @format:  should formatting spaces been added
 *
 * Dump an HTML document.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn html_doc_content_dump_format_output(
    buf: XmlOutputBufferPtr,
    cur: XmlDocPtr,
    _encoding: *const c_char,
    format: c_int,
) {
    let mut typ: c_int = 0;
    if !cur.is_null() {
        typ = (*cur).typ as i32;
        (*cur).typ = XmlElementType::XmlHtmlDocumentNode;
    }
    html_node_dump_format_output(buf, cur, cur as _, null_mut(), format);
    if !cur.is_null() {
        (*cur).typ = typ.try_into().unwrap();
    }
}

/**
 * htmlNodeDumpOutput:
 * @buf:  the HTML buffer output
 * @doc:  the document
 * @cur:  the current node
 * @encoding:  the encoding string (unused)
 *
 * Dump an HTML node, recursive behaviour,children are printed too,
 * and formatting returns/spaces are added.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn html_node_dump_output(
    buf: XmlOutputBufferPtr,
    doc: XmlDocPtr,
    cur: XmlNodePtr,
    _encoding: *const c_char,
) {
    html_node_dump_format_output(buf, doc, cur, null_mut(), 1);
}

/**
 * booleanHTMLAttrs:
 *
 * These are the HTML attributes which will be output
 * in minimized form, i.e. <option selected="selected"> will be
 * output as <option selected>, as per XSLT 1.0 16.2 "HTML Output Method"
 *
 */
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

/**
 * htmlIsBooleanAttr:
 * @name:  the name of the attribute to check
 *
 * Determine if a given attribute is a boolean attribute.
 *
 * returns: false if the attribute is not boolean, true otherwise.
 */
pub unsafe extern "C" fn html_is_boolean_attr(name: *const XmlChar) -> c_int {
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
    fn test_html_doc_content_dump_format_output() {
        #[cfg(all(feature = "html", feature = "output"))]
        unsafe {
            let mut leaks = 0;

            for n_buf in 0..GEN_NB_XML_OUTPUT_BUFFER_PTR {
                for n_cur in 0..GEN_NB_XML_DOC_PTR {
                    for n_encoding in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_format in 0..GEN_NB_INT {
                            let mem_base = xml_mem_blocks();
                            let buf = gen_xml_output_buffer_ptr(n_buf, 0);
                            let cur = gen_xml_doc_ptr(n_cur, 1);
                            let encoding = gen_const_char_ptr(n_encoding, 2);
                            let format = gen_int(n_format, 3);

                            html_doc_content_dump_format_output(buf, cur, encoding, format);
                            des_xml_output_buffer_ptr(n_buf, buf, 0);
                            des_xml_doc_ptr(n_cur, cur, 1);
                            des_const_char_ptr(n_encoding, encoding, 2);
                            des_int(n_format, format, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                eprintln!("Leak of {} blocks found in htmlDocContentDumpFormatOutput {n_buf} {n_cur} {n_encoding} {n_format}", xml_mem_blocks() - mem_base);
                                leaks += 1;
                            }
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in htmlDocContentDumpFormatOutput()"
            );
        }
    }

    #[test]
    fn test_html_get_meta_encoding() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_HTML_DOC_PTR {
                let mem_base = xml_mem_blocks();
                let doc = gen_html_doc_ptr(n_doc, 0);

                let ret_val = html_get_meta_encoding(doc);
                desret_const_xml_char_ptr(ret_val);
                des_html_doc_ptr(n_doc, doc, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in htmlGetMetaEncoding",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_doc);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in htmlGetMetaEncoding()"
            );
        }
    }

    #[test]
    fn test_html_doc_content_dump_output() {
        #[cfg(all(feature = "html", feature = "output"))]
        unsafe {
            let mut leaks = 0;

            for n_buf in 0..GEN_NB_XML_OUTPUT_BUFFER_PTR {
                for n_cur in 0..GEN_NB_XML_DOC_PTR {
                    for n_encoding in 0..GEN_NB_CONST_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let buf = gen_xml_output_buffer_ptr(n_buf, 0);
                        let cur = gen_xml_doc_ptr(n_cur, 1);
                        let encoding = gen_const_char_ptr(n_encoding, 2);

                        html_doc_content_dump_output(buf, cur, encoding);
                        des_xml_output_buffer_ptr(n_buf, buf, 0);
                        des_xml_doc_ptr(n_cur, cur, 1);
                        des_const_char_ptr(n_encoding, encoding, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in htmlDocContentDumpOutput",
                                xml_mem_blocks() - mem_base
                            );
                            eprint!(" {}", n_buf);
                            eprint!(" {}", n_cur);
                            eprintln!(" {}", n_encoding);
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in htmlDocContentDumpOutput()"
            );
        }
    }

    #[test]
    fn test_html_doc_dump() {
        #[cfg(all(feature = "html", feature = "output"))]
        unsafe {
            let mut leaks = 0;

            for n_f in 0..GEN_NB_FILE_PTR {
                for n_cur in 0..GEN_NB_XML_DOC_PTR {
                    let mem_base = xml_mem_blocks();
                    let f = gen_file_ptr(n_f, 0);
                    let cur = gen_xml_doc_ptr(n_cur, 1);

                    let ret_val = html_doc_dump(f, cur);
                    desret_int(ret_val);
                    des_file_ptr(n_f, f, 0);
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
        #[cfg(all(feature = "html", feature = "output"))]
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
        #[cfg(all(feature = "html", feature = "output"))]
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
    fn test_html_node_dump() {
        #[cfg(all(feature = "html", feature = "output"))]
        unsafe {
            let mut leaks = 0;

            for n_buf in 0..GEN_NB_XML_BUFFER_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_cur in 0..GEN_NB_XML_NODE_PTR {
                        let mem_base = xml_mem_blocks();
                        let buf = gen_const_xml_buf_ptr(n_buf, 0) as _;
                        let doc = gen_xml_doc_ptr(n_doc, 1);
                        let cur = gen_xml_node_ptr(n_cur, 2);

                        let ret_val = html_node_dump(buf, doc, cur);
                        desret_int(ret_val);
                        des_const_xml_buf_ptr(n_buf, buf, 0);
                        des_xml_doc_ptr(n_doc, doc, 1);
                        des_xml_node_ptr(n_cur, cur, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in htmlNodeDump",
                                xml_mem_blocks() - mem_base
                            );
                            eprint!(" {}", n_buf);
                            eprint!(" {}", n_doc);
                            eprintln!(" {}", n_cur);
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in htmlNodeDump()");
        }
    }

    #[test]
    fn test_html_node_dump_file() {
        #[cfg(all(feature = "html", feature = "output"))]
        unsafe {
            let mut leaks = 0;

            for n_out in 0..GEN_NB_FILE_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_cur in 0..GEN_NB_XML_NODE_PTR {
                        let mem_base = xml_mem_blocks();
                        let out = gen_file_ptr(n_out, 0);
                        let doc = gen_xml_doc_ptr(n_doc, 1);
                        let cur = gen_xml_node_ptr(n_cur, 2);

                        html_node_dump_file(out, doc, cur);
                        des_file_ptr(n_out, out, 0);
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
        #[cfg(all(feature = "html", feature = "output"))]
        unsafe {
            let mut leaks = 0;

            for n_out in 0..GEN_NB_FILE_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_cur in 0..GEN_NB_XML_NODE_PTR {
                        for n_encoding in 0..GEN_NB_CONST_CHAR_PTR {
                            for n_format in 0..GEN_NB_INT {
                                let mem_base = xml_mem_blocks();
                                let out = gen_file_ptr(n_out, 0);
                                let doc = gen_xml_doc_ptr(n_doc, 1);
                                let cur = gen_xml_node_ptr(n_cur, 2);
                                let encoding = gen_const_char_ptr(n_encoding, 3);
                                let format = gen_int(n_format, 4);

                                let ret_val =
                                    html_node_dump_file_format(out, doc, cur, encoding, format);
                                desret_int(ret_val);
                                des_file_ptr(n_out, out, 0);
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
    fn test_html_node_dump_format_output() {
        #[cfg(all(feature = "html", feature = "output"))]
        unsafe {
            let mut leaks = 0;

            for n_buf in 0..GEN_NB_XML_OUTPUT_BUFFER_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_cur in 0..GEN_NB_XML_NODE_PTR {
                        for n_encoding in 0..GEN_NB_CONST_CHAR_PTR {
                            for n_format in 0..GEN_NB_INT {
                                let mem_base = xml_mem_blocks();
                                let buf = gen_xml_output_buffer_ptr(n_buf, 0);
                                let doc = gen_xml_doc_ptr(n_doc, 1);
                                let cur = gen_xml_node_ptr(n_cur, 2);
                                let encoding = gen_const_char_ptr(n_encoding, 3);
                                let format = gen_int(n_format, 4);

                                html_node_dump_format_output(buf, doc, cur, encoding, format);
                                des_xml_output_buffer_ptr(n_buf, buf, 0);
                                des_xml_doc_ptr(n_doc, doc, 1);
                                des_xml_node_ptr(n_cur, cur, 2);
                                des_const_char_ptr(n_encoding, encoding, 3);
                                des_int(n_format, format, 4);
                                reset_last_error();
                                if mem_base != xml_mem_blocks() {
                                    leaks += 1;
                                    eprint!(
                                        "Leak of {} blocks found in htmlNodeDumpFormatOutput",
                                        xml_mem_blocks() - mem_base
                                    );
                                    eprint!(" {}", n_buf);
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
                "{leaks} Leaks are found in htmlNodeDumpFormatOutput()"
            );
        }
    }

    #[test]
    fn test_html_node_dump_output() {
        #[cfg(all(feature = "html", feature = "output"))]
        unsafe {
            let mut leaks = 0;

            for n_buf in 0..GEN_NB_XML_OUTPUT_BUFFER_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_cur in 0..GEN_NB_XML_NODE_PTR {
                        for n_encoding in 0..GEN_NB_CONST_CHAR_PTR {
                            let mem_base = xml_mem_blocks();
                            let buf = gen_xml_output_buffer_ptr(n_buf, 0);
                            let doc = gen_xml_doc_ptr(n_doc, 1);
                            let cur = gen_xml_node_ptr(n_cur, 2);
                            let encoding = gen_const_char_ptr(n_encoding, 3);

                            html_node_dump_output(buf, doc, cur, encoding);
                            des_xml_output_buffer_ptr(n_buf, buf, 0);
                            des_xml_doc_ptr(n_doc, doc, 1);
                            des_xml_node_ptr(n_cur, cur, 2);
                            des_const_char_ptr(n_encoding, encoding, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in htmlNodeDumpOutput",
                                    xml_mem_blocks() - mem_base
                                );
                                eprint!(" {}", n_buf);
                                eprint!(" {}", n_doc);
                                eprint!(" {}", n_cur);
                                eprintln!(" {}", n_encoding);
                            }
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in htmlNodeDumpOutput()"
            );
        }
    }

    #[test]
    fn test_html_save_file() {
        #[cfg(all(feature = "html", feature = "output"))]
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

    #[test]
    fn test_html_save_file_enc() {
        #[cfg(all(feature = "html", feature = "output"))]
        unsafe {
            let mut leaks = 0;

            for n_filename in 0..GEN_NB_FILEOUTPUT {
                for n_cur in 0..GEN_NB_XML_DOC_PTR {
                    for n_encoding in 0..GEN_NB_CONST_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let filename = gen_fileoutput(n_filename, 0);
                        let cur = gen_xml_doc_ptr(n_cur, 1);
                        let encoding = gen_const_char_ptr(n_encoding, 2);

                        let ret_val = html_save_file_enc(filename, cur, encoding);
                        desret_int(ret_val);
                        des_fileoutput(n_filename, filename, 0);
                        des_xml_doc_ptr(n_cur, cur, 1);
                        des_const_char_ptr(n_encoding, encoding, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in htmlSaveFileEnc",
                                xml_mem_blocks() - mem_base
                            );
                            eprint!(" {}", n_filename);
                            eprint!(" {}", n_cur);
                            eprintln!(" {}", n_encoding);
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in htmlSaveFileEnc()");
        }
    }

    #[test]
    fn test_html_save_file_format() {
        #[cfg(all(feature = "html", feature = "output"))]
        unsafe {
            let mut leaks = 0;

            for n_filename in 0..GEN_NB_FILEOUTPUT {
                for n_cur in 0..GEN_NB_XML_DOC_PTR {
                    for n_encoding in 0..GEN_NB_CONST_CHAR_PTR {
                        for n_format in 0..GEN_NB_INT {
                            let mem_base = xml_mem_blocks();
                            let filename = gen_fileoutput(n_filename, 0);
                            let cur = gen_xml_doc_ptr(n_cur, 1);
                            let encoding = gen_const_char_ptr(n_encoding, 2);
                            let format = gen_int(n_format, 3);

                            let ret_val = html_save_file_format(filename, cur, encoding, format);
                            desret_int(ret_val);
                            des_fileoutput(n_filename, filename, 0);
                            des_xml_doc_ptr(n_cur, cur, 1);
                            des_const_char_ptr(n_encoding, encoding, 2);
                            des_int(n_format, format, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in htmlSaveFileFormat",
                                    xml_mem_blocks() - mem_base
                                );
                                eprint!(" {}", n_filename);
                                eprint!(" {}", n_cur);
                                eprint!(" {}", n_encoding);
                                eprintln!(" {}", n_format);
                            }
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in htmlSaveFileFormat()"
            );
        }
    }
}
