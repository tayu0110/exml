//! Provide internal methods and data structures for handling character encoding.  
//! This module is based on `private/enc.h`, `encoding.c` and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    ffi::{c_char, c_int, c_uchar, c_ushort},
    ptr::{addr_of_mut, null, null_mut},
    sync::atomic::Ordering,
};

use libc::{size_t, snprintf};

use crate::libxml::{
    encoding::{xml_enc_output_chunk, xml_encoding_err, XmlCharEncodingHandler, XML_LITTLE_ENDIAN},
    tree::{xml_buf_content, xml_buf_end, xml_buf_shrink, xml_buf_use, XmlBufPtr},
    xml_io::{XmlOutputBufferPtr, XmlParserInputBufferPtr},
    xmlerror::XmlParserErrors,
    xmlstring::{xml_get_utf8_char, XmlChar},
};

use super::buf::{xml_buf_add_len, xml_buf_avail, xml_buf_grow};

/**
 * xmlInitEncodingInternal:
 *
 * Initialize the c_char encoding support.
 */
pub(crate) unsafe extern "C" fn xml_init_encoding_internal() {
    let mut tst: c_ushort = 0x1234;
    let ptr: *mut c_uchar = addr_of_mut!(tst) as _;

    if *ptr == 0x12 {
        XML_LITTLE_ENDIAN.store(0, Ordering::Relaxed);
    } else if *ptr == 0x34 {
        XML_LITTLE_ENDIAN.store(1, Ordering::Relaxed);
    } else {
        xml_encoding_err(
            XmlParserErrors::XmlErrInternalError,
            c"Odd problem at endianness detection\n".as_ptr() as _,
            null(),
        );
    }
}

/**
 * xmlEncInputChunk:
 * @handler:  encoding handler
 * @out:  a pointer to an array of bytes to store the result
 * @outlen:  the length of @out
 * @in:  a pointer to an array of input bytes
 * @inlen:  the length of @in
 * @flush:  flush (ICU-related)
 *
 * Returns 0 if success, or
 *     -1 by lack of space, or
 *     -2 if the transcoding fails (for *in is not valid utf8 string or
 *        the result of transformation can't fit into the encoding we want), or
 *     -3 if there the last byte can't form a single output c_char.
 *
 * The value of @inlen after return is the number of octets consumed
 *     as the return value is 0, else unpredictable.
 * The value of @outlen after return is the number of octets produced.
 */
pub(crate) unsafe extern "C" fn xml_enc_input_chunk(
    handler: *mut XmlCharEncodingHandler,
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
    _flush: c_int,
) -> c_int {
    let mut ret: c_int;

    // #ifdef LIBXML_ICONV_ENABLED
    //     else if ((*handler).iconv_in != NULL) {
    //         ret = xmlIconvWrapper((*handler).iconv_in, out, outlen, input, inlen);
    //     }
    // #endif /* LIBXML_ICONV_ENABLED */
    // #ifdef LIBXML_ICU_ENABLED
    //     else if ((*handler).uconv_in != NULL) {
    //         ret = xmlUconvWrapper((*handler).uconv_in, 1, out, outlen, input, inlen,
    //                               flush);
    //     }
    // #endif /* LIBXML_ICU_ENABLED */
    if let Some(finput) = (*handler).input {
        ret = finput(out, outlen, input, inlen);
        if ret > 0 {
            ret = 0;
        }
    } else {
        *outlen = 0;
        *inlen = 0;
        ret = -2;
    }

    ret
}

/**
 * xmlCharEncInput:
 * @input: a parser input buffer
 * @flush: try to flush all the raw buffer
 *
 * Generic front-end for the encoding handler on parser input
 *
 * Returns the number of byte written if success, or
 *     -1 general error
 *     -2 if the transcoding fails (for *in is not valid utf8 string or
 *        the result of transformation can't fit into the encoding we want), or
 */
pub(crate) unsafe extern "C" fn xml_char_enc_input(
    input: XmlParserInputBufferPtr,
    flush: c_int,
) -> c_int {
    let mut ret: c_int;
    let mut written: size_t;
    let mut toconv: size_t;
    let mut c_in: c_int;
    let mut c_out: c_int;

    if input.is_null()
        || (*input).encoder.is_null()
        || (*input).buffer.is_none()
        || (*input).raw.is_none()
    {
        return -1;
    }
    let out: XmlBufPtr = (*input).buffer.expect("Internal Error").as_ptr();
    let bufin: XmlBufPtr = (*input).raw.expect("Internal Error").as_ptr();

    toconv = xml_buf_use(bufin);
    if toconv == 0 {
        return 0;
    }
    if toconv > 64 * 1024 && flush == 0 {
        toconv = 64 * 1024;
    }
    written = xml_buf_avail(out);
    if toconv * 2 >= written {
        if xml_buf_grow(out, toconv as i32 * 2) < 0 {
            return -1;
        }
        written = xml_buf_avail(out);
    }
    if written > 128 * 1024 && flush == 0 {
        written = 128 * 1024;
    }

    c_in = toconv as _;
    c_out = written as _;
    ret = xml_enc_input_chunk(
        (*input).encoder,
        xml_buf_end(out),
        addr_of_mut!(c_out),
        xml_buf_content(bufin),
        addr_of_mut!(c_in),
        flush,
    );
    xml_buf_shrink(bufin, c_in as usize);
    xml_buf_add_len(out, c_out as usize);
    if ret == -1 {
        ret = -3;
    }

    match ret {
        0 => {
            // no-op
        }
        -1 => {
            // no-op
        }
        -3 => {
            // no-op
        }
        -2 => {
            let mut buf: [c_char; 50] = [0; 50];
            let content: *const XmlChar = xml_buf_content(bufin);

            snprintf(
                buf.as_mut_ptr().add(0) as _,
                49,
                c"0x%02X 0x%02X 0x%02X 0x%02X".as_ptr() as _,
                *content.add(0) as u32,
                *content.add(1) as u32,
                *content.add(2) as u32,
                *content.add(3) as u32,
            );
            buf[49] = 0;
            xml_encoding_err(
                XmlParserErrors::XmlI18nConvFailed,
                c"input conversion failed due to input error, bytes %s\n".as_ptr() as _,
                buf.as_ptr() as _,
            );
        }
        _ => {}
    }
    /*
     * Ignore when input buffer is not on a boundary
     */
    if ret == -3 {
        ret = 0;
    }
    if c_out != 0 {
        c_out
    } else {
        ret
    }
}

/**
 * xmlCharEncOutput:
 * @output: a parser output buffer
 * @init: is this an initialization call without data
 *
 * Generic front-end for the encoding handler on parser output
 * a first call with @init == 1 has to be made first to initiate the
 * output in case of non-stateless encoding needing to initiate their
 * state or the output (like the BOM in UTF16).
 * In case of UTF8 sequence conversion errors for the given encoder,
 * the content will be automatically remapped to a CharRef sequence.
 *
 * Returns the number of byte written if success, or
 *     -1 general error
 *     -2 if the transcoding fails (for *in is not valid utf8 string or
 *        the result of transformation can't fit into the encoding we want), or
 */
#[cfg(feature = "output")]
pub(crate) unsafe extern "C" fn xml_char_enc_output(
    output: XmlOutputBufferPtr,
    init: c_int,
) -> c_int {
    let mut ret: c_int;
    let mut written: size_t;
    let mut writtentot: c_int = 0;
    let mut toconv: size_t;
    let mut c_in: c_int;
    let mut c_out: c_int;

    if output.is_null()
        || (*output).encoder.is_null()
        || (*output).buffer.is_none()
        || (*output).conv.is_null()
    {
        return -1;
    }
    let out: XmlBufPtr = (*output).conv;
    let bufin: XmlBufPtr = (*output).buffer.expect("Internal Error").as_ptr();

    // retry:
    'retry: loop {
        written = xml_buf_avail(out);

        /*
         * First specific handling of the initialization call
         */
        if init != 0 {
            c_in = 0;
            c_out = written as _;
            /* TODO: Check return value. */
            xml_enc_output_chunk(
                (*output).encoder,
                xml_buf_end(out),
                addr_of_mut!(c_out),
                null_mut(),
                addr_of_mut!(c_in) as _,
            );
            xml_buf_add_len(out, c_out as _);
            return c_out;
        }

        /*
         * Conversion itself.
         */
        toconv = xml_buf_use(bufin);
        if toconv == 0 {
            return writtentot;
        }
        if toconv > 64 * 1024 {
            toconv = 64 * 1024;
        }
        if toconv * 4 >= written {
            xml_buf_grow(out, toconv as i32 * 4);
            written = xml_buf_avail(out);
        }
        if written > 256 * 1024 {
            written = 256 * 1024;
        }

        c_in = toconv as _;
        c_out = written as _;
        ret = xml_enc_output_chunk(
            (*output).encoder,
            xml_buf_end(out),
            addr_of_mut!(c_out) as _,
            xml_buf_content(bufin),
            addr_of_mut!(c_in) as _,
        );
        xml_buf_shrink(bufin, c_in as usize);
        xml_buf_add_len(out, c_out as usize);
        writtentot += c_out;
        if ret == -1 {
            if c_out > 0 {
                /* Can be a limitation of iconv or uconv */
                // goto retry;
                continue 'retry;
            }
            ret = -3;
        }

        /*
         * Attempt to handle error cases
         */
        match ret {
            0 => {
                // no-op
                break;
            }
            -1 => {
                // no-op
                break;
            }
            -3 => {
                // no-op
                break;
            }
            -4 => {
                xml_encoding_err(
                    XmlParserErrors::XmlI18nNoOutput,
                    c"xmlCharEncOutFunc: no output function !\n".as_ptr() as _,
                    null(),
                );
                ret = -1;
                break;
            }
            -2 => {
                let mut charref: [XmlChar; 20] = [0; 20];
                let mut len: c_int = xml_buf_use(bufin) as _;
                let content: *mut XmlChar = xml_buf_content(bufin);
                let charref_len: c_int;

                let cur: c_int = xml_get_utf8_char(content, addr_of_mut!(len) as _);
                if cur <= 0 {
                    break;
                } else {
                    charref_len = snprintf(
                        charref.as_mut_ptr().add(0) as _,
                        charref.len(),
                        c"&#%d;".as_ptr() as _,
                        cur,
                    );
                    xml_buf_shrink(bufin, len as usize);
                    xml_buf_grow(out, charref_len * 4);
                    c_out = xml_buf_avail(out) as _;
                    c_in = charref_len;
                    ret = xml_enc_output_chunk(
                        (*output).encoder,
                        xml_buf_end(out),
                        addr_of_mut!(c_out) as _,
                        charref.as_ptr() as _,
                        addr_of_mut!(c_in) as _,
                    );

                    if ret < 0 || c_in != charref_len {
                        let mut buf: [c_char; 50] = [0; 50];

                        snprintf(
                            buf.as_mut_ptr().add(0) as _,
                            49,
                            c"0x%02X 0x%02X 0x%02X 0x%02X".as_ptr() as _,
                            *content.add(0) as u32,
                            *content.add(1) as u32,
                            *content.add(2) as u32,
                            *content.add(3) as u32,
                        );
                        buf[49] = 0;
                        xml_encoding_err(
                            XmlParserErrors::XmlI18nConvFailed,
                            c"output conversion failed due to conv error, bytes %s\n".as_ptr() as _,
                            buf.as_ptr() as _,
                        );
                        *content.add(0) = b' ';
                        break;
                    }

                    xml_buf_add_len(out, c_out as _);
                    writtentot += c_out;
                    // goto retry;
                }
            }
            _ => {}
        }
    }
    if writtentot != 0 {
        writtentot
    } else {
        ret
    }
}
