//! Provide internal methods and data structures for parsing XML.  
//! This module is based on `private/parser.h`, `parser.c`, `parserInternals.c` and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::ptr::null;

use libc::{c_ulong, size_t};

use crate::{
    __xml_raise_error,
    libxml::{
        parser::{XmlParserCtxtPtr, XmlParserInputPtr, XmlParserInputState},
        parser_internals::{INPUT_CHUNK, LINE_LEN},
        xmlerror::XmlParserErrors,
        xmlstring::XmlChar,
    },
};

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
    if !ctxt.is_null()
        && (*ctxt).disable_sax != 0
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
            None,
            ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromParser,
            XmlParserErrors::XmlErrNoMemory,
            XmlErrorLevel::XmlErrFatal,
            null_mut(),
            0,
            (!extra.is_null()).then(|| CStr::from_ptr(extra as *const i8)
                .to_string_lossy()
                .into_owned()
                .into()),
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
            ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromParser,
            XmlParserErrors::XmlErrNoMemory,
            XmlErrorLevel::XmlErrFatal,
            null_mut(),
            0,
            None,
            None,
            None,
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
        None,
        ctxt as _,
        null_mut(),
        XmlErrorDomain::XmlFromParser,
        xmlerr,
        XmlErrorLevel::XmlErrFatal,
        null(),
        0,
        (!str1.is_null()).then(|| CStr::from_ptr(str1 as *const i8)
            .to_string_lossy()
            .into_owned()
            .into()),
        (!str2.is_null()).then(|| CStr::from_ptr(str2 as *const i8)
            .to_string_lossy()
            .into_owned()
            .into()),
        None,
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
 * xmlParserShrink:
 * @ctxt:  an XML parser context
 */
#[doc(hidden)]
pub unsafe extern "C" fn xml_parser_shrink(ctxt: XmlParserCtxtPtr) {
    let input: XmlParserInputPtr = (*ctxt).input;
    let mut used: size_t;

    /* Don't shrink pull parser memory buffers. */
    let Some(buf) = (*input).buf.as_mut() else {
        return;
    };
    if (*ctxt).progressive == 0
        && (*buf).borrow().encoder.is_none()
        && (*buf).borrow().context.is_none()
    {
        return;
    }

    used = (*input).cur.offset_from((*input).base) as usize;
    /*
     * Do not shrink on large buffers whose only a tiny fraction
     * was consumed
     */
    if used > INPUT_CHUNK {
        let res: size_t = buf
            .borrow()
            .buffer
            .map_or(0, |mut buf| buf.trim_head(used - LINE_LEN));

        if res > 0 {
            used -= res;
            if res > c_ulong::MAX as size_t || (*input).consumed > c_ulong::MAX - res as c_ulong {
                (*input).consumed = c_ulong::MAX;
            } else {
                (*input).consumed += res as u64;
            }
        }
    }

    (*input).set_base_and_cursor(0, used);
}
