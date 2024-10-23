//! Provide methods and data structures for handling character encodings.  
//! This module is based on `libxml/encoding.h`, `encoding.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::ffi::{c_char, c_int, c_uchar};

use crate::__xml_raise_error;

use super::xmlerror::XmlParserErrors;

/**
 * xmlCharEncodingOutputFunc:
 * @out:  a pointer to an array of bytes to store the result
 * @outlen:  the length of @out
 * @input:  a pointer to an array of UTF-8 chars
 * @inlen:  the length of @in
 *
 * Take a block of UTF-8 chars in and try to convert it to another
 * encoding.
 * Note: a first call designed to produce heading info is called with
 * in = NULL. If stateful this should also initialize the encoder state.
 *
 * Returns the number of bytes written, -1 if lack of space, or -2
 *     if the transcoding failed.
 * The value of @inlen after return is the number of octets consumed
 *     if the return value is positive, else unpredictiable.
 * The value of @outlen after return is the number of octets produced.
 */
pub type XmlCharEncodingOutputFunc = unsafe extern "C" fn(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int;

/**
 * xmlErrEncoding:
 * @error:  the error number
 * @msg:  the error message
 *
 * n encoding error
 */
pub(crate) unsafe extern "C" fn xml_encoding_err(
    error: XmlParserErrors,
    msg: *const c_char,
    val: *const c_char,
) {
    __xml_raise_error!(
        None,
        None,
        None,
        null_mut(),
        null_mut(),
        XmlErrorDomain::XmlFromI18N,
        error,
        XmlErrorLevel::XmlErrFatal,
        null_mut(),
        0,
        (!val.is_null()).then(|| CStr::from_ptr(val).to_string_lossy().into_owned().into()),
        None,
        None,
        0,
        0,
        msg,
        val
    );
}
