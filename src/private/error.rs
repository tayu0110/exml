//! Provide internal methods and data structures for error handling.  
//! This module is based on `private/error.h`, `error.c` and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{ffi::c_char, ptr::null};

use crate::{
    error::XmlErrorDomain,
    libxml::{tree::XmlNodePtr, xmlerror::XmlParserErrors},
};

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
        use std::io::Write;
        use std::ffi::CStr;
        use std::ptr::{null_mut, NonNull};

        use libc::{c_char, c_int, c_void};

        use $crate::{
            globals::{GenericError, StructuredError, GLOBAL_STATE},
            error::{generic_error_default, report_error, ErrorContextWrap, XmlErrorDomain, XmlErrorLevel},
            libxml::{
                globals::{__xml_structured_error, xml_get_warnings_default_value},
                parser::{XmlParserCtxtPtr, XmlParserInputPtr, XML_SAX2_MAGIC},
                tree::{XmlElementType, xml_get_line_no, xml_get_prop, XmlNodePtr},
                xmlerror::{
                    XmlParserErrors, xml_parser_error, xml_parser_validity_error,
                    xml_parser_validity_warning, xml_parser_warning, XML_MAX_ERRORS,
                },
                xmlstring::{XmlChar, xml_strdup},
            }
        };
        (|mut schannel: Option<StructuredError>,
            mut channel: Option<GenericError>,
            mut data: *mut c_void,
            ctx: *mut c_void,
            nod: *mut c_void,
            domain: XmlErrorDomain,
            code: XmlParserErrors,
            level: XmlErrorLevel,
            mut file: *const c_char,
            mut line: c_int,
            str1: *const c_char,
            str2: *const c_char,
            str3: *const c_char,
            int1: c_int,
            mut col: c_int,
            msg: *const c_char| {
                GLOBAL_STATE.with_borrow_mut(|state| {
                    let mut ctxt: XmlParserCtxtPtr = null_mut();
                    let mut node: XmlNodePtr = nod as XmlNodePtr;
                    let mut str: *mut c_char;
                    let mut input: XmlParserInputPtr;
                    // let mut to: XmlErrorPtr = xml_last_error();
                    let mut to = &mut state.last_error;
                    let mut baseptr: XmlNodePtr = null_mut();

                    if code == XmlParserErrors::XmlErrOK {
                        return;
                    }
                    if *xml_get_warnings_default_value() == 0 && matches!(level, XmlErrorLevel::XmlErrWarning) {
                        return;
                    }
                    if domain == XmlErrorDomain::XmlFromParser
                        || domain == XmlErrorDomain::XmlFromHTML
                        || domain == XmlErrorDomain::XmlFromDTD
                        || domain == XmlErrorDomain::XmlFromNamespace
                        || domain == XmlErrorDomain::XmlFromIO
                        || domain == XmlErrorDomain::XmlFromValid {
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
                        schannel = state.structured_error;
                        /*
                         * if user has defined handler, change data ptr to user's choice
                         */
                        if schannel.is_some() {
                            data = state.structured_error_context;
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
                        // to = addr_of_mut!((*ctxt).last_error);
                        to = &mut (*ctxt).last_error;
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
                    to.reset();
                    // xml_reset_error(to);
                    (*to).domain = domain;
                    (*to).code = code;
                    if !str.is_null() {
                        (*to).message = Some(CStr::from_ptr(str).to_string_lossy().into());
                    }
                    // (*to).message = str;
                    (*to).level = level;
                    if !file.is_null() {
                        (*to).file = Some(
                            CStr::from_ptr(xml_strdup(file as *const XmlChar) as *const i8)
                                .to_string_lossy()
                                .into()
                            );
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
                                (*to).file = Some(CStr::from_ptr(href).to_string_lossy().into());
                            } else {
                                (*to).file = Some(
                                    CStr::from_ptr(xml_strdup((*(*baseptr).doc).url) as *const i8)
                                        .to_string_lossy()
                                        .into()
                                    );
                            }
                        }
                        #[cfg(not(feature = "xinclude"))] {
                            (*to).file = Some(
                                CStr::from_ptr(xml_strdup((*(*baseptr).doc).url) as *const i8)
                                    .to_string_lossy()
                                    .into()
                                );
                        }
                        if (*to).file.is_none() && !node.is_null() && !(*node).doc.is_null() {
                            (*to).file = Some(
                                CStr::from_ptr(xml_strdup((*(*node).doc).url) as *const i8)
                                    .to_string_lossy()
                                    .into()
                                );
                        }
                    }
                    (*to).line = line as usize;
                    if !str1.is_null() {
                        (*to).str1 = Some(
                            CStr::from_ptr(xml_strdup(str1 as *const XmlChar) as *const i8)
                                .to_string_lossy()
                                .into()
                            );
                    }
                    if !str2.is_null() {
                        (*to).str2 = Some(
                            CStr::from_ptr(xml_strdup(str2 as *const XmlChar) as *const i8)
                                .to_string_lossy()
                                .into()
                            );
                    }
                    if !str3.is_null() {
                        (*to).str3 = Some(
                            CStr::from_ptr(xml_strdup(str3 as *const XmlChar) as *const i8)
                                .to_string_lossy()
                                .into()
                            );
                    }
                    (*to).int1 = int1;
                    (*to).int2 = col;
                    (*to).node = NonNull::new(node as _);
                    (*to).ctxt = NonNull::new(ctx);

                    state.last_error = to.clone();
                    if let Some(schannel) = schannel {
                        schannel(data, &state.last_error);
                        return;
                    }

                    /*
                     * Find the callback channel if channel param is NULL
                     */
                    let data = if !ctxt.is_null() && channel.is_none()
                        && __xml_structured_error().is_none()
                        && !(*ctxt).sax.is_null() {
                        if matches!(level, XmlErrorLevel::XmlErrWarning) {
                            channel = (*(*ctxt).sax).warning;
                        } else {
                            channel = (*(*ctxt).sax).error;
                        }
                        let dum = (*ctxt).user_data as *mut ErrorContextWrap<Box<dyn Write>>;
                        Some((*dum).0.as_mut())
                        // data = (*ctxt).user_data;
                    } else if channel.is_none() {
                        channel = Some(state.generic_error);
                        if !ctxt.is_null() {
                            let dum = ctxt as *mut ErrorContextWrap<Box<dyn Write>>;
                            Some((*dum).0.as_mut())
                            // data = ctxt as _;
                        } else {
                            state.generic_error_context.as_deref_mut()
                            // data = xml_generic_error_context();
                        }
                    } else {
                        None
                    };
                    if channel.is_none() {
                        return;
                    }

                    let channel = channel.unwrap();
                    if channel as usize == xml_parser_error as usize
                        || channel as usize == xml_parser_warning as usize
                        || channel as usize == xml_parser_validity_error as usize
                        || channel as usize == xml_parser_validity_warning as usize {
                        let s = (!str.is_null()).then(|| CStr::from_ptr(str).to_string_lossy().into_owned());
                        if let Some(s) = s {
                            report_error(&state.last_error, ctxt, Some(s.as_str()), None, None);
                        } else {
                            report_error(&state.last_error, ctxt, None, None, None);
                        }
                    } else if /* TODO: std::ptr::addr_of!(channel) == std::ptr::addr_of!(libc::fprintf) || */
                        channel as usize == generic_error_default as usize {
                        let s = (!str.is_null()).then(|| CStr::from_ptr(str).to_string_lossy().into_owned());
                        if let Some(s) = s {
                            report_error(&state.last_error, ctxt, Some(s.as_str()), Some(channel), data);
                        } else {
                            report_error(&state.last_error, ctxt, None, Some(channel), data);
                        }
                    } else {
                        // Is not NULL check necessary ???
                        let s = CStr::from_ptr(str).to_string_lossy().into_owned();
                        channel(data, format!("{s}").as_str());
                    }
                });
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
pub(crate) unsafe fn __xml_simple_error(
    domain: XmlErrorDomain,
    code: XmlParserErrors,
    node: XmlNodePtr,
    msg: *const c_char,
    extra: *const c_char,
) {
    if code == XmlParserErrors::XmlErrNoMemory {
        if !extra.is_null() {
            __xml_raise_error!(
                None,
                None,
                null_mut(),
                null_mut(),
                node as _,
                domain,
                XmlParserErrors::XmlErrNoMemory,
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
                XmlParserErrors::XmlErrNoMemory,
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
