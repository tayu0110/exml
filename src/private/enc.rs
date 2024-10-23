//! Provide internal methods and data structures for handling character encoding.  
//! This module is based on `private/enc.h`, `encoding.c` and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    ffi::{c_int, c_uchar},
    fmt::Write,
    str::from_utf8_mut,
};

use crate::{
    encoding::EncodingError,
    libxml::{
        encoding::{xml_encoding_err, XmlCharEncodingHandler},
        xml_io::{XmlOutputBufferPtr, XmlParserInputBufferPtr},
        xmlerror::XmlParserErrors,
    },
};

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
 *     -3 if there the last byte can't form a single output char.
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
    if input.is_null()
        || (*input).encoder.is_none()
        || (*input).buffer.is_none()
        || (*input).raw.is_none()
    {
        return -1;
    }
    let mut out = (*input).buffer.expect("Internal Error");
    let mut bufin = (*input).raw.expect("Internal Error");

    let mut toconv = bufin.len();
    if toconv == 0 {
        return 0;
    }
    if toconv > 64 * 1024 && flush == 0 {
        toconv = 64 * 1024;
    }
    let mut written = out.avail();
    if toconv * 2 >= written {
        if out.grow(toconv * 2).is_err() {
            return -1;
        }
        written = out.avail();
    }
    if written > 128 * 1024 && flush == 0 {
        written = 128 * 1024;
    }

    let c_in = toconv;
    let c_out = written;
    let src = &bufin.as_ref()[..c_in];
    let mut outstr = vec![0; c_out];
    let dst = from_utf8_mut(&mut outstr).unwrap();
    let ret = match (*input).encoder.as_mut().unwrap().decode(src, dst) {
        Ok((read, write)) => {
            bufin.trim_head(read);
            out.push_bytes(&outstr[..write]);
            // no-op
            0
        }
        Err(EncodingError::BufferTooShort) => {
            // no-op
            0
        }
        Err(EncodingError::Malformed {
            read,
            write,
            length,
            offset,
        }) => {
            bufin.trim_head(read - length - offset);
            out.push_bytes(&outstr[..write]);
            let content = bufin.as_ref();
            let mut buf = String::new();
            write!(
                buf,
                "0x{:02X} 0x{:02X} 0x{:02X} 0x{:02X}\0",
                content.first().unwrap_or(&0),
                content.get(1).unwrap_or(&0),
                content.get(2).unwrap_or(&0),
                content.get(3).unwrap_or(&0)
            );

            xml_encoding_err(
                XmlParserErrors::XmlI18nConvFailed,
                c"input conversion failed due to input error, bytes %s\n".as_ptr() as _,
                buf.as_ptr() as _,
            );
            -2
        }
        _ => 0,
    };
    if c_out != 0 {
        c_out as i32
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
    use std::str::from_utf8;

    use crate::encoding::floor_char_boundary;

    let ret: c_int;
    let mut writtentot: usize = 0;

    if output.is_null()
        || (*output).encoder.is_none()
        || (*output).buffer.is_none()
        || (*output).conv.is_none()
    {
        return -1;
    }
    let mut out = (*output).conv.unwrap();
    let mut bufin = (*output).buffer.unwrap();
    let mut encoder = (*output).encoder.as_mut().unwrap().borrow_mut();

    // retry:
    loop {
        let mut written = out.avail();

        /*
         * First specific handling of the initialization call
         */
        if init != 0 {
            let c_out = written;
            /* TODO: Check return value. */
            let mut dst = vec![0; c_out];
            return match encoder.encode("", &mut dst) {
                Ok((_, write)) => {
                    out.push_bytes(&dst[..write]);
                    write as i32
                }
                Err(EncodingError::Unmappable {
                    read: _,
                    write,
                    c: _,
                }) => {
                    out.push_bytes(&dst[..write]);
                    write as i32
                }
                _ => 0,
            };
        }

        /*
         * Conversion itself.
         */
        let mut toconv = bufin.len();
        if toconv == 0 {
            return writtentot as i32;
        }
        if toconv > 64 * 1024 {
            toconv = 64 * 1024;
        }
        if toconv * 4 >= written {
            out.grow(toconv * 4);
            written = out.avail();
        }
        if written > 256 * 1024 {
            written = 256 * 1024;
        }

        let c_in = floor_char_boundary(bufin.as_ref(), toconv);
        let c_out = written;
        let mut dst = vec![0; c_out];
        match encoder.encode(from_utf8(&bufin.as_ref()[..c_in]).unwrap(), &mut dst) {
            Ok((read, write)) => {
                ret = 0;
                bufin.trim_head(read);
                out.push_bytes(&dst[..write]);
                writtentot += write;
                break;
            }
            Err(EncodingError::BufferTooShort) => {
                ret = -3;
                break;
            }
            Err(EncodingError::Unmappable { read, write, c }) => {
                // `ret` should be set -2, but it is overwritten in next loop.
                // Therefore, ommit it.
                // ret = -2;
                bufin.trim_head(read);
                out.push_bytes(&dst[..write]);
                writtentot += write;

                let mut charref = String::new();
                write!(charref, "&#{};", c as u32);
                let charref_len = charref.len();

                out.grow(charref_len * 4);
                let c_out = out.avail();
                let mut dst = vec![0; c_out];
                let result = encoder.encode(&charref, &mut dst);

                match result {
                    Ok((read, write)) if read == charref_len => {
                        out.push_bytes(&dst[..write]);
                        writtentot += write;
                    }
                    e => {
                        ret = match e {
                            Ok(_) => 0,
                            Err(EncodingError::Unmappable {
                                read: _,
                                write: _,
                                c: _,
                            }) => -2,
                            Err(_) => -1,
                        };
                        let content = bufin.as_ref();
                        let mut msg = String::new();
                        write!(
                            msg,
                            "0x{:02X} 0x{:02X} 0x{:02X} 0x{:02X}\0",
                            content.first().unwrap_or(&0),
                            content.get(1).unwrap_or(&0),
                            content.get(2).unwrap_or(&0),
                            content.get(3).unwrap_or(&0)
                        )
                        .ok();

                        xml_encoding_err(
                            XmlParserErrors::XmlI18nConvFailed,
                            c"output conversion failed due to conv error, bytes %s\n".as_ptr() as _,
                            msg.as_ptr() as _,
                        );
                        out.push_bytes(b" ");
                        break;
                    }
                }
                // goto retry;
            }
            Err(EncodingError::Other { msg: _ }) => {
                ret = -1;
                // unreachable!(msg);
                break;
            }
            _ => {
                // ret = -1;
                unreachable!()
            }
        }
    }
    if writtentot != 0 {
        writtentot as i32
    } else {
        ret
    }
}
