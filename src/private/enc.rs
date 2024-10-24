//! Provide internal methods and data structures for handling character encoding.  
//! This module is based on `private/enc.h`, `encoding.c` and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::str::from_utf8_mut;

use crate::{
    encoding::{xml_encoding_err, EncodingError},
    libxml::{
        xml_io::{XmlOutputBuffer, XmlParserInputBuffer},
        xmlerror::XmlParserErrors,
    },
};

/// Generic front-end for the encoding handler on parser input.  
/// If you try to flush all the raw buffer, set `flush` to `true`.
///
/// If successfully encoded, return the number of written bytes.
/// If not, return the following `EncodingError`.
/// - general error (`EncodingError::Other`)
/// - encoding failure (`EncodingError::Malformed`)
pub(crate) fn xml_char_enc_input(
    input: &mut XmlParserInputBuffer,
    flush: bool,
) -> Result<usize, EncodingError> {
    if input.encoder.is_none() || input.buffer.is_none() || input.raw.is_none() {
        return Err(EncodingError::Other {
            msg: "Encoder or Buffer is not set.".into(),
        });
    }
    let mut out = input.buffer.expect("Internal Error");
    let mut bufin = input.raw.expect("Internal Error");

    let mut toconv = bufin.len();
    if toconv == 0 {
        return Ok(0);
    }
    if !flush {
        toconv = toconv.min(64 * 1024);
    }
    let mut written = out.avail();
    if toconv * 2 >= written {
        if out.grow(toconv * 2).is_err() {
            return Err(EncodingError::Other {
                msg: "Failed to grow output buffer.".into(),
            });
        }
        written = out.avail();
    }
    if !flush {
        written = written.min(128 * 1024);
    }

    let c_in = toconv;
    let c_out = written;
    let src = &bufin.as_ref()[..c_in];
    let mut outstr = vec![0; c_out];
    let dst = from_utf8_mut(&mut outstr).unwrap();
    let ret = match input.encoder.as_mut().unwrap().decode(src, dst) {
        Ok((read, write)) => {
            bufin.trim_head(read);
            out.push_bytes(&outstr[..write]);
            // no-op
            Ok(0)
        }
        Err(EncodingError::BufferTooShort) => {
            // no-op
            Ok(0)
        }
        Err(
            e @ EncodingError::Malformed {
                read,
                write,
                length,
                offset,
            },
        ) => {
            bufin.trim_head(read - length - offset);
            out.push_bytes(&outstr[..write]);
            let content = bufin.as_ref();
            let buf = format!(
                "0x{:02X} 0x{:02X} 0x{:02X} 0x{:02X}",
                content.first().unwrap_or(&0),
                content.get(1).unwrap_or(&0),
                content.get(2).unwrap_or(&0),
                content.get(3).unwrap_or(&0)
            );

            unsafe {
                xml_encoding_err(
                    XmlParserErrors::XmlI18nConvFailed,
                    format!("input conversion failed due to input error, bytes {buf}\n").as_str(),
                    &buf,
                );
            }
            Err(e)
        }
        _ => Ok(0),
    };
    if c_out != 0 {
        Ok(c_out)
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
/// Generic front-end for the encoding handler on parser input.  
///
/// On the first call, `init` should be set to `true`.  
/// This is utilized in stateless encoding schemes.
///
/// If successfully encoded, return the number of written bytes.
/// If not, return the following `EncodingError`.
/// - general error (`EncodingError::Other`)
/// - buffer too short (`EncodingError::BufferTooShort`)
/// - encoding failure (`EncodingError::Unmappable`)
#[cfg(feature = "output")]
pub(crate) fn xml_char_enc_output(
    output: &mut XmlOutputBuffer,
    init: bool,
) -> Result<usize, EncodingError> {
    use std::str::from_utf8;

    use crate::encoding::floor_char_boundary;

    let mut writtentot: usize = 0;

    if output.encoder.is_none() || output.buffer.is_none() || output.conv.is_none() {
        return Err(EncodingError::Other {
            msg: "Encoder or Buffer is not set.".into(),
        });
    }
    let mut out = output.conv.unwrap();
    let mut bufin = output.buffer.unwrap();
    let mut encoder = output.encoder.as_mut().unwrap().borrow_mut();

    // retry:
    let ret = loop {
        let mut written = out.avail();

        /*
         * First specific handling of the initialization call
         */
        if init {
            let c_out = written;
            /* TODO: Check return value. */
            let mut dst = vec![0; c_out];
            return match encoder.encode("", &mut dst) {
                Ok((_, write)) => {
                    out.push_bytes(&dst[..write]);
                    Ok(write)
                }
                Err(EncodingError::Unmappable {
                    read: _,
                    write,
                    c: _,
                }) => {
                    out.push_bytes(&dst[..write]);
                    Ok(write)
                }
                _ => Ok(0),
            };
        }

        /*
         * Conversion itself.
         */
        let mut toconv = bufin.len();
        if toconv == 0 {
            return Ok(writtentot);
        }
        toconv = toconv.min(64 * 1024);
        if toconv * 4 >= written {
            out.grow(toconv * 4);
            written = out.avail();
        }
        written = written.min(256 * 1024);

        let c_in = floor_char_boundary(bufin.as_ref(), toconv);
        let c_out = written;
        let mut dst = vec![0; c_out];
        match encoder.encode(from_utf8(&bufin.as_ref()[..c_in]).unwrap(), &mut dst) {
            Ok((read, write)) => {
                bufin.trim_head(read);
                out.push_bytes(&dst[..write]);
                writtentot += write;
                break Ok(0);
            }
            Err(e @ EncodingError::BufferTooShort) => {
                break Err(e);
            }
            Err(EncodingError::Unmappable { read, write, c }) => {
                // `ret` should be set -2, but it is overwritten in next loop.
                // Therefore, ommit it.
                // ret = -2;
                bufin.trim_head(read);
                out.push_bytes(&dst[..write]);
                writtentot += write;

                let charref = format!("&#{};", c as u32);
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
                        let content = bufin.as_ref();
                        let msg = format!(
                            "0x{:02X} 0x{:02X} 0x{:02X} 0x{:02X}",
                            content.first().unwrap_or(&0),
                            content.get(1).unwrap_or(&0),
                            content.get(2).unwrap_or(&0),
                            content.get(3).unwrap_or(&0)
                        );

                        unsafe {
                            xml_encoding_err(
                                XmlParserErrors::XmlI18nConvFailed,
                                format!(
                                    "output conversion failed due to conv error, bytes {msg}\n"
                                )
                                .as_str(),
                                &msg,
                            );
                        }
                        out.push_bytes(b" ");
                        break e.map(|_| 0);
                    }
                }
                // goto retry;
            }
            Err(e @ EncodingError::Other { msg: _ }) => {
                // unreachable!(msg);
                break Err(e);
            }
            _ => {
                // ret = -1;
                unreachable!()
            }
        }
    };
    if writtentot != 0 {
        Ok(writtentot)
    } else {
        ret
    }
}
