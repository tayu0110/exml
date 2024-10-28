use std::{cell::RefCell, ffi::c_void, ptr::null, rc::Rc, str::from_utf8};

use crate::{
    buf::XmlBufRef,
    encoding::{floor_char_boundary, xml_encoding_err, EncodingError, XmlCharEncodingHandler},
    libxml::xmlerror::XmlParserErrors,
};

use super::{xml_escape_content, xml_ioerr, MINLEN};

/**
 * xmlOutputMatchCallback:
 * @filename: the filename or URI
 *
 * Callback used in the I/O Output API to detect if the current handler
 * can provide output functionality for this resource.
 *
 * Returns 1 if yes and 0 if another Output module should be used
 */
pub type XmlOutputMatchCallback = unsafe extern "C" fn(filename: *const i8) -> i32;
/**
 * xmlOutputOpenCallback:
 * @filename: the filename or URI
 *
 * Callback used in the I/O Output API to open the resource
 *
 * Returns an Output context or NULL in case or error
 */
pub type XmlOutputOpenCallback = unsafe extern "C" fn(filename: *const i8) -> *mut c_void;
/**
 * xmlOutputWriteCallback:
 * @context:  an Output context
 * @buffer:  the buffer of data to write
 * @len:  the length of the buffer in bytes
 *
 * Callback used in the I/O Output API to write to the resource
 *
 * Returns the number of bytes written or -1 in case of error
 */
pub type XmlOutputWriteCallback =
    unsafe extern "C" fn(context: *mut c_void, buffer: *const i8, len: i32) -> i32;
/**
 * xmlOutputCloseCallback:
 * @context:  an Output context
 *
 * Callback used in the I/O Output API to close the resource
 *
 * Returns 0 or -1 in case of error
 */
pub type XmlOutputCloseCallback = unsafe extern "C" fn(context: *mut c_void) -> i32;

pub type XmlOutputBufferPtr = *mut XmlOutputBuffer;
#[repr(C)]
pub struct XmlOutputBuffer {
    pub(crate) context: *mut c_void,
    pub(crate) writecallback: Option<XmlOutputWriteCallback>,
    pub(crate) closecallback: Option<XmlOutputCloseCallback>,
    pub(crate) encoder: Option<Rc<RefCell<XmlCharEncodingHandler>>>, /* I18N conversions to UTF-8 */
    pub(crate) buffer: Option<XmlBufRef>, /* Local buffer encoded in UTF-8 or ISOLatin */
    pub(crate) conv: Option<XmlBufRef>,   /* if encoder != NULL buffer for output */
    pub(crate) written: i32,              /* total number of byte written */
    pub(crate) error: XmlParserErrors,
}

impl XmlOutputBuffer {
    /// Generic front-end for the encoding handler on parser output.  
    ///
    /// On the first call, `init` should be set to `true`.  
    /// This is utilized in stateless encoding schemes.
    ///
    /// If successfully encoded, return the number of written bytes.
    /// If not, return the following `EncodingError`.
    /// - general error (`EncodingError::Other`)
    /// - buffer too short (`EncodingError::BufferTooShort`)
    /// - encoding failure (`EncodingError::Unmappable`)
    pub(crate) fn encode(&mut self, init: bool) -> Result<usize, EncodingError> {
        let mut writtentot: usize = 0;

        if self.encoder.is_none() || self.buffer.is_none() || self.conv.is_none() {
            return Err(EncodingError::Other {
                msg: "Encoder or Buffer is not set.".into(),
            });
        }
        let mut out = self.conv.unwrap();
        let mut bufin = self.buffer.unwrap();
        let mut encoder = self.encoder.as_mut().unwrap().borrow_mut();

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

    /// Write the content of the array in the output I/O buffer.  
    /// This routine handle the I18N transcoding from internal UTF-8.  
    /// The buffer is lossless, i.e. will store in case of partial or delayed writes.
    ///
    /// Returns the number of chars immediately written, or -1 in case of error.
    #[doc(alias = "xmlOutputBufferWrite")]
    pub unsafe fn write_bytes(&mut self, buf: &[u8]) -> i32 {
        let mut ret; /* return from function call */
        let mut written = 0; /* number of c_char written to I/O so far */

        if !self.error.is_ok() {
            return -1;
        }

        let mut len = buf.len();
        for buf in buf.chunks(4 * MINLEN) {
            /*
             * first handle encoding stuff.
             */
            let nbchars = if self.encoder.is_some() {
                /*
                 * Store the data in the incoming raw buffer
                 */
                if self.conv.is_none() {
                    self.conv = XmlBufRef::new();
                }
                if self
                    .buffer
                    .map_or(true, |mut buffer| buffer.push_bytes(buf).is_err())
                {
                    return -1;
                }

                if self.buffer.map_or(0, |buf| buf.len()) < MINLEN && buf.len() == len {
                    break;
                }

                /*
                 * convert as much as possible to the parser reading buffer.
                 */
                let res = match self.encode(false) {
                    Ok(len) => Ok(len),
                    Err(EncodingError::BufferTooShort) => Err(EncodingError::BufferTooShort),
                    _ => {
                        xml_ioerr(XmlParserErrors::XmlIoEncoder, null());
                        self.error = XmlParserErrors::XmlIoEncoder;
                        return -1;
                    }
                };
                if self.writecallback.is_some() {
                    self.conv.map_or(0, |buf| buf.len())
                } else {
                    res.unwrap_or(0)
                }
            } else {
                if self
                    .buffer
                    .map_or(true, |mut buffer| buffer.push_bytes(buf).is_err())
                {
                    return -1;
                }
                if self.writecallback.is_some() {
                    self.buffer.map_or(0, |buf| buf.len())
                } else {
                    buf.len()
                }
            };
            len -= buf.len();

            if let Some(writecallback) = self.writecallback {
                if nbchars < MINLEN && len == 0 {
                    break;
                }

                /*
                 * second write the stuff to the I/O channel
                 */
                if self.encoder.is_some() {
                    ret = writecallback(
                        self.context,
                        self.conv
                            .map_or(null(), |buf| buf.as_ref().as_ptr() as *const i8),
                        nbchars as i32,
                    );
                    if ret >= 0 {
                        if let Some(mut conv) = self.conv {
                            conv.trim_head(ret as usize);
                        }
                    }
                } else {
                    ret = writecallback(
                        self.context,
                        self.buffer
                            .map_or(null(), |buf| buf.as_ref().as_ptr() as *const i8),
                        nbchars as i32,
                    );
                    if ret >= 0 {
                        if let Some(mut buf) = self.buffer {
                            buf.trim_head(ret as usize);
                        }
                    }
                }
                if ret < 0 {
                    xml_ioerr(XmlParserErrors::XmlIoWrite, null());
                    self.error = XmlParserErrors::XmlIoWrite;
                    return ret;
                }
                self.written = self.written.saturating_add(ret);
            }
            written += nbchars as i32;
        }

        written
    }

    /// Write the content of the string in the output I/O buffer.  
    /// This routine handle the I18N transcoding from internal UTF-8.  
    /// The buffer is lossless, i.e. will store in case of partial or delayed writes.
    ///
    /// Returns the number of chars immediately written, or -1 in case of error.
    #[doc(alias = "xmlOutputBufferWriteString")]
    pub unsafe fn write_str(&mut self, s: &str) -> i32 {
        if !self.error.is_ok() {
            return -1;
        }

        if !s.is_empty() {
            return self.write_bytes(s.as_bytes());
        }
        s.len() as i32
    }

    /// Write the content of the string in the output I/O buffer.  
    /// This routine escapes the characters and then handle the I18N transcoding from internal UTF-8.  
    /// The buffer is lossless, i.e. will store in case of partial or delayed writes.
    ///
    /// Returns the number of chars immediately written, or -1 in case of error.
    ///
    #[doc(alias = "xmlOutputBufferWriteEscape")]
    pub unsafe fn write_str_with_escape(
        &mut self,
        str: &str,
        escaping: Option<fn(&str, &mut String) -> i32>,
    ) -> i32 {
        let mut ret; /* return from function call */
        let mut written = 0; /* number of c_char written to I/O so far */
        let mut oldwritten; /* loop guard */

        if !self.error.is_ok() {
            return -1;
        }
        let Some(mut buffer) = self.buffer else {
            return -1;
        };

        let escaping = escaping.unwrap_or(xml_escape_content);

        loop {
            /*
             * make sure we have enough room to save first, if this is
             * not the case force a flush, but make sure we stay in the loop
             */
            if buffer.avail() < 40 {
                if buffer.grow(100).is_err() {
                    return -1;
                }
                oldwritten = -1;
                if str.is_empty() || oldwritten == written {
                    break;
                }
                continue;
            }

            /*
             * first handle encoding stuff.
             */
            let nbchars = if self.encoder.is_some() {
                /*
                 * Store the data in the incoming raw buffer
                 */
                let conv = *self.conv.get_or_insert_with(|| XmlBufRef::new().unwrap());
                let mut buf = String::new();
                ret = escaping(str, &mut buf);
                buffer.push_bytes(buf.as_bytes());
                if ret < 0 || buf.is_empty() {
                    /* chunk==0 => nothing done */
                    return -1;
                }
                if buffer.len() < MINLEN {
                    // goto done;
                    return written;
                }

                /*
                 * convert as much as possible to the output buffer.
                 */
                let ret = match self.encode(false) {
                    Ok(len) => Ok(len),
                    Err(EncodingError::BufferTooShort) => Err(EncodingError::BufferTooShort),
                    _ => {
                        xml_ioerr(XmlParserErrors::XmlIoEncoder, null());
                        self.error = XmlParserErrors::XmlIoEncoder;
                        return -1;
                    }
                };
                if self.writecallback.is_some() {
                    conv.len() as i32
                } else {
                    ret.unwrap_or(0) as i32
                }
            } else {
                let mut buf = String::new();
                ret = escaping(str, &mut buf);
                buffer.push_bytes(buf.as_bytes());
                if ret < 0 || buf.is_empty() {
                    /* chunk==0 => nothing done */
                    return -1;
                }
                if self.writecallback.is_some() {
                    buffer.len() as i32
                } else {
                    buf.len() as i32
                }
            };

            if let Some(writecallback) = self.writecallback {
                if nbchars < MINLEN as i32 {
                    // goto done;
                    return written;
                }

                /*
                 * second write the stuff to the I/O channel
                 */
                if self.encoder.is_some() {
                    ret = writecallback(
                        self.context,
                        self.conv.map_or(null(), |conv| {
                            if conv.is_ok() {
                                conv.as_ref().as_ptr()
                            } else {
                                null()
                            }
                        }) as *const i8,
                        nbchars,
                    );
                    if ret >= 0 {
                        if let Some(mut conv) = self.conv {
                            conv.trim_head(ret as usize);
                        }
                    }
                } else {
                    ret = writecallback(
                        self.context,
                        if buffer.is_ok() {
                            buffer.as_ref().as_ptr() as *const i8
                        } else {
                            null()
                        },
                        nbchars,
                    );
                    if ret >= 0 {
                        buffer.trim_head(ret as usize);
                    }
                }
                if ret < 0 {
                    xml_ioerr(XmlParserErrors::XmlIoWrite, null());
                    self.error = XmlParserErrors::XmlIoWrite;
                    return ret;
                }
                self.written = self.written.wrapping_add(ret);
            } else if buffer.avail() < MINLEN {
                buffer.grow(MINLEN);
            }
            written += nbchars;

            break;
        }

        // done:
        written
    }

    /// flushes the output I/O channel
    ///
    /// Returns the number of byte written or -1 in case of error.
    #[doc(alias = "xmlOutputBufferFlush")]
    pub unsafe extern "C" fn flush(&mut self) -> i32 {
        let mut ret = 0;

        if !self.error.is_ok() {
            return -1;
        }
        /*
         * first handle encoding stuff.
         */
        if self.conv.is_some() && self.encoder.is_some() {
            /*
             * convert as much as possible to the parser output buffer.
             */
            while {
                let Ok(nbchars) = self.encode(false) else {
                    xml_ioerr(XmlParserErrors::XmlIoEncoder, null());
                    self.error = XmlParserErrors::XmlIoEncoder;
                    return -1;
                };

                nbchars != 0
            } {}
        }

        /*
         * second flush the stuff to the I/O channel
         */
        if let Some(mut conv) = self
            .conv
            .filter(|_| self.encoder.is_some() && self.writecallback.is_some())
        {
            // if !(*out).conv.is_null() && !(*out).encoder.is_null() && (*out).writecallback.is_some() {
            ret = (self.writecallback.unwrap())(
                self.context,
                if conv.is_ok() {
                    conv.as_ref().as_ptr() as *const i8
                } else {
                    null()
                },
                conv.len() as i32,
            );
            if ret >= 0 {
                conv.trim_head(ret as usize);
            }
        } else if self.writecallback.is_some() {
            ret = (self.writecallback.unwrap())(
                self.context,
                self.buffer
                    .map_or(null(), |buf| buf.as_ref().as_ptr() as *const i8),
                self.buffer.map_or(0, |buf| buf.len() as i32),
            );
            if ret >= 0 {
                if let Some(mut buf) = self.buffer {
                    buf.trim_head(ret as usize);
                }
            }
        }
        if ret < 0 {
            xml_ioerr(XmlParserErrors::XmlIoFlush, null());
            self.error = XmlParserErrors::XmlIoFlush;
            return ret;
        }
        self.written = self.written.saturating_add(ret);

        ret
    }

    /// Gives a pointer to the data currently held in the output buffer
    ///
    /// Returns a pointer to the data or NULL in case of error
    #[doc(alias = "xmlOutputBufferGetContent")]
    pub fn get_buffer_content(&self) -> Option<&[u8]> {
        self.buffer
            .as_ref()
            .filter(|buf| buf.is_ok())
            .map(|b| b.as_ref())
    }

    /// Gives the length of the data currently held in the output buffer
    ///
    /// Returns 0 in case or error or no data is held, the size otherwise
    #[doc(alias = "xmlOutputBufferGetSize")]
    pub fn get_buffer_size(&self) -> usize {
        self.buffer.map_or(0, |buf| buf.len())
    }
}
