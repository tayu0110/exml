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
        use std::borrow::Cow;
        use std::ffi::{CStr, CString};
        use std::ptr::{null_mut, NonNull};

        use libc::{c_char, c_int, c_void};

        use $crate::{
            globals::{GenericError, GenericErrorContext, StructuredError, GLOBAL_STATE},
            error::{
                generic_error_default, parser_error, parser_warning, parser_validity_error,
                parser_validity_warning, report_error, XmlErrorDomain, XmlErrorLevel,
            },
            libxml::{
                parser::{XmlParserCtxtPtr, XmlParserInputPtr, XML_SAX2_MAGIC},
                tree::{XmlElementType, xml_get_line_no, xml_get_prop, XmlNodePtr},
                xmlerror::{XmlParserErrors, XML_MAX_ERRORS},
            }
        };
        (|mut schannel: Option<StructuredError>,
            mut channel: Option<GenericError>,
            mut data: Option<GenericErrorContext>,
            ctx: *mut c_void,
            nod: *mut c_void,
            domain: XmlErrorDomain,
            code: XmlParserErrors,
            level: XmlErrorLevel,
            mut file: *const c_char,
            mut line: c_int,
            str1: Option<Cow<'static, str>>,
            str2: Option<Cow<'static, str>>,
            str3: Option<Cow<'static, str>>,
            int1: c_int,
            mut col: c_int,
            msg: *const c_char| {
                let mut ctxt: XmlParserCtxtPtr = null_mut();
                let Some((channel, error, s, data)) = GLOBAL_STATE.with_borrow_mut(|state| {
                    let mut node: XmlNodePtr = nod as XmlNodePtr;
                    let mut str: *mut c_char;
                    let mut input: XmlParserInputPtr;
                    let mut to = &mut state.last_error;
                    let mut baseptr: XmlNodePtr = null_mut();
                    #[allow(unused_assignments)]
                    let mut dummy = None;

                    if code == XmlParserErrors::XmlErrOK {
                        return None;
                    }
                    if state.get_warnings_default_value == 0 && matches!(level, XmlErrorLevel::XmlErrWarning) {
                        return None;
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
                                    return None;
                                }
                                (*ctxt).nb_warnings += 1;
                            } else {
                                if (*ctxt).nb_errors >= XML_MAX_ERRORS as u16 {
                                    return None;
                                }
                                (*ctxt).nb_errors += 1;
                            }

                            if schannel.is_none()
                                && !(*ctxt).sax.is_null()
                                && (*(*ctxt).sax).initialized == XML_SAX2_MAGIC as u32
                                && (*(*ctxt).sax).serror.is_some() {
                                schannel = (*(*ctxt).sax).serror;
                                data = (*ctxt).user_data.clone();
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
                            data = state.structured_error_context.clone();
                        }
                    }
                    /*
                     * Formatting the message
                     */
                    let str = if msg.is_null() {
                        Cow::Borrowed("No error message provided")
                    } else {
                        $crate::XML_GET_VAR_STR!(msg, str, $( $args ),*);
                        assert!(!str.is_null());
                        let s = CStr::from_ptr(str).to_string_lossy();
                        let s = s.into_owned();
                        assert!(s.as_ptr() as *const i8 as usize != str as usize);
                        $crate::libxml::globals::xml_free(str as _);
                        s.into()
                    };

                    /*
                     * specific processing if a parser context is provided
                     */
                    if !ctxt.is_null() {
                        if file.is_null() {
                            input = (*ctxt).input;
                            if !input.is_null() && (*input).filename.is_none() && (*ctxt).input_tab.len() > 1 {
                                input = (*ctxt).input_tab[(*ctxt).input_tab.len() as usize - 2];
                            }
                            if !input.is_null() {
                                dummy = (*input).filename.as_deref().map(|f| CString::new(f).unwrap());
                                file = dummy.as_ref().map_or(null_mut(), |f| f.as_ptr());
                                line = (*input).line;
                                col = (*input).col;
                            }
                        }
                        to = &mut (*ctxt).last_error;
                    } else if !node.is_null() && file.is_null() {
                        if !(*node).doc.is_null() && !(*(*node).doc).url.is_none() {
                            baseptr = node;
                        /*	    file = (const c_char *) (*(*node).doc).URL; */
                        }
                        for _ in 0..10 {
                            if node.is_null() || matches!((*node).typ, XmlElementType::XmlElementNode) {
                                break;
                            }
                            node = (*node).parent;
                        }
                        if baseptr.is_null() && !node.is_null() && !(*node).doc.is_null() && !(*(*node).doc).url.is_none() {
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
                    to.domain = domain;
                    to.code = code;
                    to.message = Some(str.clone());
                    to.level = level;
                    if !file.is_null() {
                        to.file = Some(CStr::from_ptr(file as *const i8).to_string_lossy().into());
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
                                to.file = Some(CStr::from_ptr(href).to_string_lossy().into());
                            } else {
                                to.file = (*(*baseptr).doc).url.as_deref().map(|u| Cow::Owned(u.to_owned()));
                            }
                        }
                        #[cfg(not(feature = "xinclude"))] {
                            to.file = (!(*(*baseptr).doc).url.is_null()).then(|| {
                                CStr::from_ptr((*(*baseptr).doc).url as *const i8)
                                    .to_string_lossy()
                                    .into()
                            });
                        }
                        if to.file.is_none() && !node.is_null() && !(*node).doc.is_null() {
                            to.file = (*(*node).doc).url.as_deref().map(|u| Cow::Owned(u.to_owned()));
                        }
                    }
                    to.line = line as usize;
                    to.str1 = str1;
                    to.str2 = str2;
                    to.str3 = str3;
                    to.int1 = int1;
                    to.int2 = col;
                    to.node = NonNull::new(node as _);
                    to.ctxt = NonNull::new(ctx);

                    let error = to.clone();
                    state.last_error = to.clone();
                    if let Some(schannel) = schannel {
                        schannel(data, &state.last_error);
                        return None;
                    }

                    /*
                     * Find the callback channel if channel param is NULL
                     */
                    if !ctxt.is_null() && channel.is_none()
                        && state.structured_error.is_none()
                        && !(*ctxt).sax.is_null() {
                        if matches!(level, XmlErrorLevel::XmlErrWarning) {
                            channel = (*(*ctxt).sax).warning;
                        } else {
                            channel = (*(*ctxt).sax).error;
                        }
                        channel.map(|c| (c, error, str, (*ctxt).user_data.clone()))
                    } else if channel.is_none() {
                        channel = Some(state.generic_error);
                        if !ctxt.is_null() {
                            let context = GenericErrorContext::new(Box::new(ctxt));
                            channel.map(|c| (c, error, str, Some(context)))
                        } else {
                            channel.map(|c| (c, error, str, state.generic_error_context.clone()))
                        }
                    } else {
                        channel.map(|c| (c, error, str, None))
                    }
                }) else {
                    return;
                };
                if channel as usize == parser_error as usize
                    || channel as usize == parser_warning as usize
                    || channel as usize == parser_validity_error as usize
                    || channel as usize == parser_validity_warning as usize {
                    report_error(&error, ctxt, Some(s.as_ref()), None, None);
                } else if /* TODO: std::ptr::addr_of!(channel) == std::ptr::addr_of!(libc::fprintf) || */
                    channel as usize == generic_error_default as usize {
                    report_error(&error, ctxt, Some(s.as_ref()), Some(channel), data);
                } else {
                    channel(data, s.as_ref());
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
                None,
                null_mut(),
                node as _,
                domain,
                XmlParserErrors::XmlErrNoMemory,
                XmlErrorLevel::XmlErrFatal,
                null(),
                0,
                Some(CStr::from_ptr(extra).to_string_lossy().into_owned().into()),
                None,
                None,
                0,
                0,
                c"Memory allocation failed : %s\n".as_ptr(),
                extra
            );
        } else {
            __xml_raise_error!(
                None,
                None,
                None,
                null_mut(),
                node as _,
                domain,
                XmlParserErrors::XmlErrNoMemory,
                XmlErrorLevel::XmlErrFatal,
                null(),
                0,
                None,
                None,
                None,
                0,
                0,
                c"Memory allocation failed\n".as_ptr(),
            );
        }
    } else {
        __xml_raise_error!(
            None,
            None,
            None,
            null_mut(),
            node as _,
            domain,
            code,
            XmlErrorLevel::XmlErrError,
            null(),
            0,
            (!extra.is_null()).then(|| CStr::from_ptr(extra).to_string_lossy().into_owned().into()),
            None,
            None,
            0,
            0,
            msg as _,
            extra
        );
    }
}
