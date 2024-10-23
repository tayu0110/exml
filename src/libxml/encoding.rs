//! Provide methods and data structures for handling character encodings.  
//! This module is based on `libxml/encoding.h`, `encoding.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    ffi::{c_char, c_int, c_uchar},
    ptr::{null, null_mut},
    sync::atomic::{AtomicI32, AtomicPtr, AtomicUsize},
};

use crate::{__xml_raise_error, error::XmlErrorDomain, private::error::__xml_simple_error};

use super::xmlerror::XmlParserErrors;

/*
 * xmlCharEncoding:
 *
 * Predefined values for some standard encodings.
 * Libxml does not do beforehand translation on UTF8 and ISOLatinX.
 * It also supports ASCII, ISO-8859-1, and UTF16 (LE and BE) by default.
 *
 * Anything else would have to be translated to UTF8 before being
 * given to the parser itself. The BOM for UTF16 and the encoding
 * declaration are looked at and a converter is looked for at that
 * point. If not found the parser stops here as asked by the XML REC. A
 * converter can be registered by the user using xmlRegisterCharEncodingHandler
 * but the current form doesn't allow stateful transcoding (a serious
 * problem agreed !). If iconv has been found it will be used
 * automatically and allow stateful transcoding, the simplest is then
 * to be sure to enable iconv and to provide iconv libs for the encoding
 * support needed.
 *
 * Note that the generic "UTF-16" is not a predefined value.  Instead, only
 * the specific UTF-16LE and UTF-16BE are present.
 */
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlCharEncoding {
    Error = -1,     /* No char encoding detected */
    None = 0,       /* No char encoding detected */
    UTF8 = 1,       /* UTF-8 */
    UTF16LE = 2,    /* UTF-16 little endian */
    UTF16BE = 3,    /* UTF-16 big endian */
    UCS4LE = 4,     /* UCS-4 little endian */
    UCS4BE = 5,     /* UCS-4 big endian */
    EBCDIC = 6,     /* EBCDIC uh! */
    UCS4_2143 = 7,  /* UCS-4 unusual ordering */
    UCS4_3412 = 8,  /* UCS-4 unusual ordering */
    UCS2 = 9,       /* UCS-2 */
    ISO8859_1 = 10, /* ISO-8859-1 ISO Latin 1 */
    ISO8859_2 = 11, /* ISO-8859-2 ISO Latin 2 */
    ISO8859_3 = 12, /* ISO-8859-3 */
    ISO8859_4 = 13, /* ISO-8859-4 */
    ISO8859_5 = 14, /* ISO-8859-5 */
    ISO8859_6 = 15, /* ISO-8859-6 */
    ISO8859_7 = 16, /* ISO-8859-7 */
    ISO8859_8 = 17, /* ISO-8859-8 */
    ISO8859_9 = 18, /* ISO-8859-9 */
    ISO2022JP = 19, /* ISO-2022-JP */
    ShiftJIS = 20,  /* Shift_JIS */
    EUCJP = 21,     /* EUC-JP */
    ASCII = 22,     /* pure ASCII */
}

/**
 * xmlCharEncodingInputFunc:
 * @out:  a pointer to an array of bytes to store the UTF-8 result
 * @outlen:  the length of @out
 * @input:  a pointer to an array of chars in the original encoding
 * @inlen:  the length of @in
 *
 * Take a block of chars in the original encoding and try to convert
 * it to an UTF-8 block of chars out.
 *
 * Returns the number of bytes written, -1 if lack of space, or -2
 *     if the transcoding failed.
 * The value of @inlen after return is the number of octets consumed
 *     if the return value is positive, else unpredictiable.
 * The value of @outlen after return is the number of octets consumed.
 */
pub type XmlCharEncodingInputFunc = unsafe extern "C" fn(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int;

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

/*
 * Block defining the handlers for non UTF-8 encodings.
 * If iconv is supported, there are two extra fields.
 */
pub type XmlCharEncodingHandlerPtr = *mut XmlCharEncodingHandler;
#[repr(C)]
pub struct XmlCharEncodingHandler {
    pub(crate) name: AtomicPtr<c_char>,
    pub(crate) input: Option<XmlCharEncodingInputFunc>,
    pub(crate) output: Option<XmlCharEncodingOutputFunc>,
}

static HANDLERS: AtomicPtr<XmlCharEncodingHandlerPtr> = AtomicPtr::new(null_mut());
static NB_CHAR_ENCODING_HANDLER: AtomicUsize = AtomicUsize::new(0);

const MAX_ENCODING_HANDLERS: usize = 50;

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

/**
 * xmlEncodingErrMemory:
 * @extra:  extra information
 *
 * Handle an out of memory condition
 */
unsafe extern "C" fn xml_encoding_err_memory(extra: *const c_char) {
    __xml_simple_error(
        XmlErrorDomain::XmlFromI18N,
        XmlParserErrors::XmlErrNoMemory,
        null_mut(),
        null(),
        extra as _,
    );
}

macro_rules! MAKE_HANDLER {
    ($name:expr, $input:expr, $out:expr) => {
        XmlCharEncodingHandler {
            name: AtomicPtr::new($name as _),
            input: $input,
            output: $out,
        }
    };
}

pub(crate) static XML_LITTLE_ENDIAN: AtomicI32 = AtomicI32::new(1);

/**
 * xmlEncOutputChunk:
 * @handler:  encoding handler
 * @out:  a pointer to an array of bytes to store the result
 * @outlen:  the length of @out
 * @in:  a pointer to an array of input bytes
 * @inlen:  the length of @in
 *
 * Returns 0 if success, or
 *     -1 by lack of space, or
 *     -2 if the transcoding fails (for *in is not valid utf8 string or
 *        the result of transformation can't fit into the encoding we want), or
 *     -3 if there the last byte can't form a single output c_char.
 *     -4 if no output function was found.
 *
 * The value of @inlen after return is the number of octets consumed
 *     as the return value is 0, else unpredictable.
 * The value of @outlen after return is the number of octets produced.
 */
pub(crate) unsafe extern "C" fn xml_enc_output_chunk(
    handler: *mut XmlCharEncodingHandler,
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    let mut ret: c_int;

    if let Some(output) = (*handler).output {
        ret = output(out, outlen, input, inlen);
        if ret > 0 {
            ret = 0;
        }
    } else {
        *outlen = 0;
        *inlen = 0;
        ret = -4;
    }

    ret
}

#[cfg(test)]
mod tests {
    use crate::{
        globals::reset_last_error,
        libxml::{htmlparser::utf8_to_html, xmlmemory::xml_mem_blocks},
        test_util::*,
    };

    #[test]
    fn test_utf8_to_html() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            for n_out in 0..GEN_NB_UNSIGNED_CHAR_PTR {
                for n_outlen in 0..GEN_NB_INT_PTR {
                    for n_in in 0..GEN_NB_CONST_UNSIGNED_CHAR_PTR {
                        for n_inlen in 0..GEN_NB_INT_PTR {
                            let mem_base = xml_mem_blocks();
                            let out = gen_unsigned_char_ptr(n_out, 0);
                            let outlen = gen_int_ptr(n_outlen, 1);
                            let input = gen_const_unsigned_char_ptr(n_in, 2);
                            let inlen = gen_int_ptr(n_inlen, 3);

                            let ret_val = utf8_to_html(out, outlen, input, inlen);
                            desret_int(ret_val);
                            des_unsigned_char_ptr(n_out, out, 0);
                            des_int_ptr(n_outlen, outlen, 1);
                            des_const_unsigned_char_ptr(n_in, input, 2);
                            des_int_ptr(n_inlen, inlen, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                eprintln!("Leak of {} blocks found in UTF8ToHtml {n_out} {n_outlen} {n_in} {n_inlen}", xml_mem_blocks() - mem_base);
                                leaks += 1;
                            }
                        }
                    }
                }
            }

            assert!(leaks == 0, "{leaks} Leaks are found in utf8_to_html()");
        }
    }

    #[test]
    fn test_xml_new_char_encoding_handler() {

        /* missing type support */
    }
}
