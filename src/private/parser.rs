//! Provide internal methods and data structures for parsing XML.  
//! This module is based on `private/parser.h`, `parser.c`, `parserInternals.c` and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    ffi::c_int,
    ptr::{null, null_mut},
};

use libc::{c_ulong, ptrdiff_t, size_t};

use crate::{
    __xml_raise_error,
    libxml::{
        parser::{XmlParserCtxtPtr, XmlParserInputPtr, XmlParserInputState, XmlParserOption},
        parser_internals::{
            input_pop, xml_err_internal, xml_free_input_stream, INPUT_CHUNK, LINE_LEN,
            XML_MAX_LOOKUP_LIMIT,
        },
        tree::xml_buf_shrink,
        xml_io::{
            xml_free_parser_input_buffer, xml_parser_input_buffer_grow, XmlParserInputBufferPtr,
        },
        xmlerror::XmlParserErrors,
        xmlstring::XmlChar,
    },
};

use super::buf::xml_buf_set_input_base_cur;

/**
 * XML_VCTXT_DTD_VALIDATED:
 *
 * Set after xmlValidateDtdFinal was called.
 */
pub(crate) const XML_VCTXT_DTD_VALIDATED: usize = 1usize << 0;
/**
 * XML_VCTXT_USE_PCTXT:
 *
 * Set if the validation context is part of a parser context.
 */
pub(crate) const XML_VCTXT_USE_PCTXT: usize = 1usize << 1;

/**
 * xmlErrMemory:
 * @ctxt:  an XML parser context
 * @extra:  extra information
 *
 * Handle a redefinition of attribute error
 */
#[doc(hidden)]
pub unsafe extern "C" fn xml_err_memory(ctxt: XmlParserCtxtPtr, extra: *const char) {
    if (!ctxt.is_null())
        && ((*ctxt).disable_sax != 0)
        && matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        return;
    }
    if !ctxt.is_null() {
        (*ctxt).err_no = XmlParserErrors::XmlErrNoMemory as i32;
        (*ctxt).instate = XmlParserInputState::XmlParserEOF;
        (*ctxt).disable_sax = 1;
    }
    if !extra.is_null() {
        __xml_raise_error!(
            None,
            None,
            null_mut(),
            ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromParser as i32,
            XmlParserErrors::XmlErrNoMemory as i32,
            XmlErrorLevel::XmlErrFatal,
            null_mut(),
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
            ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromParser as i32,
            XmlParserErrors::XmlErrNoMemory as i32,
            XmlErrorLevel::XmlErrFatal,
            null_mut(),
            0,
            null(),
            null(),
            null(),
            0,
            0,
            c"Memory allocation failed\n".as_ptr(),
        );
    }
}

/**
 * __xmlErrEncoding:
 * @ctxt:  an XML parser context
 * @xmlerr:  the error number
 * @msg:  the error message
 * @str1:  an string info
 * @str2:  an string info
 *
 * Handle an encoding error
 */
#[doc(hidden)]
pub unsafe extern "C" fn __xml_err_encoding(
    ctxt: XmlParserCtxtPtr,
    xmlerr: XmlParserErrors,
    msg: *const char,
    str1: *const XmlChar,
    str2: *const XmlChar,
) {
    if (!ctxt.is_null())
        && ((*ctxt).disable_sax != 0)
        && matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        return;
    }
    if !ctxt.is_null() {
        (*ctxt).err_no = xmlerr as _;
    }
    __xml_raise_error!(
        None,
        None,
        null_mut(),
        ctxt as _,
        null_mut(),
        XmlErrorDomain::XmlFromParser as i32,
        xmlerr as i32,
        XmlErrorLevel::XmlErrFatal,
        null(),
        0,
        str1 as *const c_char,
        str2 as *const c_char,
        null(),
        0,
        0,
        msg as _,
        str1,
        str2
    );
    if !ctxt.is_null() {
        (*ctxt).well_formed = 0;
        if (*ctxt).recovery == 0 {
            (*ctxt).disable_sax = 1;
        }
    }
}

/**
 * xmlHaltParser:
 * @ctxt:  an XML parser context
 *
 * Blocks further parser processing don't override error
 * for internal use
 */
#[doc(hidden)]
pub unsafe extern "C" fn xml_halt_parser(ctxt: XmlParserCtxtPtr) {
    if ctxt.is_null() {
        return;
    }
    (*ctxt).instate = XmlParserInputState::XmlParserEOF;
    (*ctxt).disable_sax = 1;
    #[allow(clippy::while_immutable_condition)]
    while (*ctxt).input_nr > 1 {
        xml_free_input_stream(input_pop(ctxt));
    }
    if !(*ctxt).input.is_null() {
        /*
         * in case there was a specific allocation deallocate before
         * overriding base
         */
        if let Some(free) = (*(*ctxt).input).free {
            free((*(*ctxt).input).base as *mut XmlChar);
            (*(*ctxt).input).free = None;
        }
        if !(*(*ctxt).input).buf.is_null() {
            xml_free_parser_input_buffer((*(*ctxt).input).buf);
            (*(*ctxt).input).buf = null_mut();
        }
        (*(*ctxt).input).cur = c"".as_ptr() as _;
        (*(*ctxt).input).length = 0;
        (*(*ctxt).input).base = (*(*ctxt).input).cur;
        (*(*ctxt).input).end = (*(*ctxt).input).cur;
    }
}

/**
 * xmlParserGrow:
 * @ctxt:  an XML parser context
 */
#[doc(hidden)]
pub unsafe extern "C" fn xml_parser_grow(ctxt: XmlParserCtxtPtr) -> c_int {
    let input: XmlParserInputPtr = (*ctxt).input;
    let buf: XmlParserInputBufferPtr = (*input).buf;
    let cur_end: ptrdiff_t = (*input).end.offset_from((*input).cur);
    let cur_base: ptrdiff_t = (*input).cur.offset_from((*input).base);

    if buf.is_null() {
        return 0;
    }
    /* Don't grow push parser buffer. */
    if (*ctxt).progressive != 0 {
        return 0;
    }
    /* Don't grow memory buffers. */
    if (*buf).encoder.is_null() && (*buf).readcallback.is_none() {
        return 0;
    }

    if (cur_end > XML_MAX_LOOKUP_LIMIT as isize || cur_base > XML_MAX_LOOKUP_LIMIT as isize)
        && (*ctxt).options & XmlParserOption::XmlParseHuge as i32 == 0
    {
        xml_err_internal(ctxt, c"Huge input lookup".as_ptr() as _, null());
        xml_halt_parser(ctxt);
        return -1;
    }

    if cur_end >= INPUT_CHUNK as isize {
        return 0;
    }

    let ret: c_int = xml_parser_input_buffer_grow(buf, INPUT_CHUNK as _);
    xml_buf_set_input_base_cur((*buf).buffer, input, 0, cur_base as _);

    /* TODO: Get error code from xmlParserInputBufferGrow */
    if ret < 0 {
        xml_err_internal(ctxt, c"Growing input buffer".as_ptr() as _, null());
        xml_halt_parser(ctxt);
    }

    ret
}

/**
 * xmlParserShrink:
 * @ctxt:  an XML parser context
 */
#[doc(hidden)]
pub unsafe extern "C" fn xml_parser_shrink(ctxt: XmlParserCtxtPtr) {
    let input: XmlParserInputPtr = (*ctxt).input;
    let buf: XmlParserInputBufferPtr = (*input).buf;
    let mut used: size_t;

    /* Don't shrink pull parser memory buffers. */
    if buf.is_null()
        || (*ctxt).progressive == 0 && (*buf).encoder.is_null() && (*buf).readcallback.is_none()
    {
        return;
    }

    used = (*input).cur.offset_from((*input).base) as usize;
    /*
     * Do not shrink on large buffers whose only a tiny fraction
     * was consumed
     */
    if used > INPUT_CHUNK {
        let res: size_t = xml_buf_shrink((*buf).buffer, used - LINE_LEN);

        if res > 0 {
            used -= res;
            if res > c_ulong::MAX as size_t || (*input).consumed > c_ulong::MAX - res as c_ulong {
                (*input).consumed = c_ulong::MAX;
            } else {
                (*input).consumed += res as u64;
            }
        }
    }

    xml_buf_set_input_base_cur((*buf).buffer, input, 0, used);
}
