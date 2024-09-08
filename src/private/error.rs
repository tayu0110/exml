//! Provide internal methods and data structures for error handling.  
//! This module is based on `private/error.h`, `error.c` and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    ffi::{c_char, c_int},
    ptr::null,
};

use crate::libxml::{tree::XmlNodePtr, xmlerror::XmlParserErrors};

/**
 * __xmlRaiseError:
 * @schannel: the structured callback channel
 * @channel: the old callback channel
 * @data: the callback data
 * @ctx: the parser context or NULL
 * @ctx: the parser context or NULL
 * @domain: the domain for the error
 * @code: the code for the error
 * @level: the XmlErrorLevel for the error
 * @file: the file source of the error (or NULL)
 * @line: the line of the error or 0 if N/A
 * @str1: extra string info
 * @str2: extra string info
 * @str3: extra string info
 * @int1: extra int info
 * @col: column number of the error or 0 if N/A
 * @msg:  the message to display/transmit
 * @...:  extra parameters for the message display
 *
 * Update the appropriate global or contextual error structure,
 * then forward the error message down the parser or generic
 * error callback handler
 */
#[doc(hidden)]
#[macro_export]
macro_rules! __xml_raise_error {
    ($schannel:expr, $channel:expr, $data:expr, $ctx:expr, $nod:expr, $domain:expr, $code:expr, $level:expr, $file:expr, $line:expr, $str1:expr, $str2:expr, $str3:expr, $int1:expr, $col:expr, $msg:expr, $( $args:expr ),*) => {{
        use std::ptr::{
            addr_of_mut, null_mut
        };

        use libc::{
            c_char, c_int, c_void,
        };

        use $crate::libxml::{
            globals::{
                __xml_structured_error, xml_generic_error, xml_generic_error_context, xml_get_warnings_default_value, xml_last_error,
                xml_structured_error_context,
            },
            parser:: {
                XmlParserCtxtPtr, XmlParserInputPtr, XML_SAX2_MAGIC,
            },
            tree::{
                XmlElementType, xml_get_line_no, xml_get_prop, XmlNodePtr,
            },
            xmlerror::{
                xmlCopyError, XmlErrorDomain, XmlErrorLevel, XmlErrorPtr, xmlGenericErrorDefaultFunc, XmlGenericErrorFunc,
                XmlParserErrors, xmlParserError, xmlParserValidityError, xmlParserValidityWarning, xmlParserWarning,
                xmlReportError, xmlResetError, XmlStructuredErrorFunc, XML_MAX_ERRORS,
            },
            xmlstring::{
                XmlChar, xml_strdup,
            },
        };
        (|mut schannel: Option<XmlStructuredErrorFunc>,
            mut channel: Option<XmlGenericErrorFunc>,
            mut data: *mut c_void,
            ctx: *mut c_void,
            nod: *mut c_void,
            domain: c_int,
            code: c_int,
            level: XmlErrorLevel,
            mut file: *const c_char,
            mut line: c_int,
            str1: *const c_char,
            str2: *const c_char,
            str3: *const c_char,
            int1: c_int,
            mut col: c_int,
            msg: *const c_char| {
                let mut ctxt: XmlParserCtxtPtr = null_mut();
                let mut node: XmlNodePtr = nod as XmlNodePtr;
                let mut str: *mut c_char;
                let mut input: XmlParserInputPtr;
                let mut to: XmlErrorPtr = xml_last_error();
                let mut baseptr: XmlNodePtr = null_mut();

                if code == XmlParserErrors::XmlErrOK as i32 {
                    return;
                }
                if *xml_get_warnings_default_value() == 0 && matches!(level, XmlErrorLevel::XmlErrWarning) {
                    return;
                }
                if domain == XmlErrorDomain::XmlFromParser as i32
                    || domain == XmlErrorDomain::XmlFromHtml as i32
                    || domain == XmlErrorDomain::XmlFromDtd as i32
                    || domain == XmlErrorDomain::XmlFromNamespace as i32
                    || domain == XmlErrorDomain::XmlFromIO as i32
                    || domain == XmlErrorDomain::XmlFromValid as i32 {
                    ctxt = ctx as XmlParserCtxtPtr;

                    if !ctxt.is_null() {
                        if matches!(level, XmlErrorLevel::XmlErrWarning) {
                            if (*ctxt).nb_warnings >= XML_MAX_ERRORS as u16 {
                                return;
                            }
                            (*ctxt).nb_warnings += 1;
                        } else {
                            if (*ctxt).nb_errors >= XML_MAX_ERRORS as u16 {
                                return;
                            }
                            (*ctxt).nb_errors += 1;
                        }

                        if schannel.is_none()
                            && !(*ctxt).sax.is_null()
                            && (*(*ctxt).sax).initialized == XML_SAX2_MAGIC as u32
                            && (*(*ctxt).sax).serror.is_some() {
                            schannel = (*(*ctxt).sax).serror;
                            data = (*ctxt).user_data;
                        }
                    }
                }
                /*
                 * Check if structured error handler set
                 */
                if schannel.is_none() {
                    schannel = __xml_structured_error();
                    /*
                     * if user has defined handler, change data ptr to user's choice
                     */
                    if schannel.is_some() {
                        data = *xml_structured_error_context() as _;
                    }
                }
                /*
                 * Formatting the message
                 */
                if msg.is_null() {
                    str = xml_strdup(c"No error message provided".as_ptr() as _) as *mut c_char;
                } else {
                    $crate::XML_GET_VAR_STR!(msg, str, $( $args ),*);
                }

                /*
                 * specific processing if a parser context is provided
                 */
                if !ctxt.is_null() {
                    if file.is_null() {
                        input = (*ctxt).input;
                        if !input.is_null() && (*input).filename.is_null() && (*ctxt).input_nr > 1 {
                            input = *(*ctxt).input_tab.add((*ctxt).input_nr as usize - 2);
                        }
                        if !input.is_null() {
                            file = (*input).filename;
                            line = (*input).line;
                            col = (*input).col;
                        }
                    }
                    to = addr_of_mut!((*ctxt).last_error);
                } else if !node.is_null() && file.is_null() {
                    if !(*node).doc.is_null() && !(*(*node).doc).url.is_null() {
                        baseptr = node;
                    /*	    file = (const c_char *) (*(*node).doc).URL; */
                    }
                    for _ in 0..10 {
                        if node.is_null() || matches!((*node).typ, XmlElementType::XmlElementNode) {
                            break;
                        }
                        node = (*node).parent;
                    }
                    if baseptr.is_null() && !node.is_null() && !(*node).doc.is_null() && !(*(*node).doc).url.is_null() {
                        baseptr = node;
                    }

                    if !node.is_null() && matches!((*node).typ, XmlElementType::XmlElementNode) {
                        line = (*node).line as _;
                    }
                    if line == 0 || line == 65535 {
                        line = xml_get_line_no(node) as _;
                    }
                }

                /*
                 * Save the information about the error
                 */
                xmlResetError(to);
                (*to).domain = domain;
                (*to).code = code;
                (*to).message = str;
                (*to).level = level;
                if !file.is_null() {
                    (*to).file = xml_strdup(file as *const XmlChar) as *mut c_char;
                } else if !baseptr.is_null() {
                    #[cfg(feature = "xinclude")]
                    {
                        /*
                         * We check if the error is within an XInclude section and,
                         * if so, attempt to print out the href of the XInclude instead
                         * of the usual "base" (doc->URL) for the node (bug 152623).
                         */
                        let mut prev: XmlNodePtr = baseptr;
                        let mut href: *mut c_char = null_mut();
                        let mut inclcount: c_int = 0;
                        while !prev.is_null() {
                            if (*prev).prev.is_null() {
                                prev = (*prev).parent;
                            } else {
                                prev = (*prev).prev;
                                if matches!((*prev).typ, XmlElementType::XmlXincludeStart) {
                                    if inclcount > 0 {
                                        inclcount -= 1;
                                    } else {
                                        href = xml_get_prop(prev, c"href".as_ptr() as _) as *mut c_char;
                                        if !href.is_null() {
                                            break;
                                        }
                                    }
                                } else if matches!((*prev).typ, XmlElementType::XmlXincludeEnd) {
                                    inclcount += 1;
                                }
                            }
                        }
                        if !href.is_null() {
                            (*to).file = href;
                        } else {
                            (*to).file = xml_strdup((*(*baseptr).doc).url) as *mut c_char;
                        }
                    }
                    #[cfg(not(feature = "xinclude"))] {
                        (*to).file = xml_strdup((*(*baseptr).doc).url) as *mut c_char;
                    }
                    if (*to).file.is_null() && !node.is_null() && !(*node).doc.is_null() {
                        (*to).file = xml_strdup((*(*node).doc).url) as *mut c_char;
                    }
                }
                (*to).line = line;
                if !str1.is_null() {
                    (*to).str1 = xml_strdup(str1 as *const XmlChar) as *mut c_char;
                }
                if !str2.is_null() {
                    (*to).str2 = xml_strdup(str2 as *const XmlChar) as *mut c_char;
                }
                if !str3.is_null() {
                    (*to).str3 = xml_strdup(str3 as *const XmlChar) as *mut c_char;
                }
                (*to).int1 = int1;
                (*to).int2 = col;
                (*to).node = node as _;
                (*to).ctxt = ctx;

                if to != xml_last_error() {
                    xmlCopyError(to, xml_last_error());
                }

                if let Some(schannel) = schannel {
                    schannel(data, to);
                    return;
                }

                /*
                 * Find the callback channel if channel param is NULL
                 */
                if !ctxt.is_null() && channel.is_none() && __xml_structured_error().is_none() && !(*ctxt).sax.is_null() {
                    if matches!(level, XmlErrorLevel::XmlErrWarning) {
                        channel = (*(*ctxt).sax).warning;
                    } else {
                        channel = (*(*ctxt).sax).error;
                    }
                    data = (*ctxt).user_data;
                } else if channel.is_none() {
                    channel = Some(xml_generic_error);
                    if !ctxt.is_null() {
                        data = ctxt as _;
                    } else {
                        data = xml_generic_error_context();
                    }
                }
                if channel.is_none() {
                    return;
                }

                let channel = channel.unwrap();
                if channel as usize == xmlParserError as usize
                    || channel as usize == xmlParserWarning as usize
                    || channel as usize == xmlParserValidityError as usize
                    || channel as usize == xmlParserValidityWarning as usize {
                    xmlReportError(to, ctxt, str, None, null_mut());
                } else if /* TODO: std::ptr::addr_of!(channel) == std::ptr::addr_of!(libc::fprintf) || */ channel as usize == xmlGenericErrorDefaultFunc as usize {
                    xmlReportError(to, ctxt, str, Some(channel), data);
                } else {
                    $crate::xml_error_with_format!(channel, data, c"%s".as_ptr(), str);
                }
        })($schannel, $channel, $data, $ctx, $nod, $domain, $code, $level, $file, $line, $str1, $str2, $str3, $int1, $col, $msg);
    }};
}

/**
 * __xmlSimpleError:
 * @domain: where the error comes from
 * @code: the error code
 * @node: the context node
 * @extra:  extra information
 *
 * Handle an out of memory condition
 */
#[doc(hidden)]
pub(crate) unsafe extern "C" fn __xml_simple_error(
    domain: c_int,
    code: c_int,
    node: XmlNodePtr,
    msg: *const c_char,
    extra: *const c_char,
) {
    if code == XmlParserErrors::XmlErrNoMemory as i32 {
        if !extra.is_null() {
            __xml_raise_error!(
                None,
                None,
                null_mut(),
                null_mut(),
                node as _,
                domain,
                XmlParserErrors::XmlErrNoMemory as i32,
                XmlErrorLevel::XmlErrFatal,
                null(),
                0,
                extra as _,
                null(),
                null(),
                0,
                0,
                c"Memory allocation failed : %s\n".as_ptr(),
                extra
            );
        } else {
            __xml_raise_error!(
                None,
                None,
                null_mut(),
                null_mut(),
                node as _,
                domain,
                XmlParserErrors::XmlErrNoMemory as i32,
                XmlErrorLevel::XmlErrFatal,
                null(),
                0,
                null(),
                null(),
                null(),
                0,
                0,
                c"Memory allocation failed\n".as_ptr(),
            );
        }
    } else {
        __xml_raise_error!(
            None,
            None,
            null_mut(),
            null_mut(),
            node as _,
            domain,
            code,
            XmlErrorLevel::XmlErrError,
            null(),
            0,
            extra as _,
            null(),
            null(),
            0,
            0,
            msg as _,
            extra
        );
    }
}
