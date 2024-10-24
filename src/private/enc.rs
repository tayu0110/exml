//! Provide internal methods and data structures for handling character encoding.  
//! This module is based on `private/enc.h`, `encoding.c` and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use crate::{
    encoding::{xml_encoding_err, EncodingError},
    libxml::{xml_io::XmlOutputBuffer, xmlerror::XmlParserErrors},
};

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
