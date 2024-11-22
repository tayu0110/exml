use std::{
    borrow::Cow,
    cell::RefCell,
    ffi::{c_void, CString},
    ptr::{null, null_mut},
    rc::Rc,
    str::from_utf8,
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Mutex,
    },
};

use url::Url;

use crate::{
    buf::XmlBufRef,
    encoding::{floor_char_boundary, xml_encoding_err, EncodingError, XmlCharEncodingHandler},
    error::XmlParserErrors,
    globals::GLOBAL_STATE,
    io::{xml_io_http_close_put, xml_io_http_dflt_open_w, xml_io_http_match, xml_io_http_write},
    libxml::uri::unescape_url,
    tree::XmlBufferAllocationScheme,
};

use super::{
    xml_escape_content, xml_file_close, xml_file_match, xml_file_open_w, xml_file_write, xml_ioerr,
    MINLEN,
};

/// Callback used in the I/O Output API to detect if the current handler
/// can provide output functionality for this resource.
///
/// Returns 1 if yes and 0 if another Output module should be used
#[doc(alias = "xmlOutputMatchCallback")]
pub type XmlOutputMatchCallback = unsafe fn(filename: &str) -> i32;
/// Callback used in the I/O Output API to open the resource
///
/// Returns an Output context or NULL in case or error
#[doc(alias = "xmlOutputOpenCallback")]
pub type XmlOutputOpenCallback = unsafe extern "C" fn(filename: *const i8) -> *mut c_void;
/// Callback used in the I/O Output API to write to the resource
///
/// Returns the number of bytes written or -1 in case of error
#[doc(alias = "xmlOutputWriteCallback")]
pub type XmlOutputWriteCallback =
    unsafe extern "C" fn(context: *mut c_void, buffer: *const i8, len: i32) -> i32;
/// Callback used in the I/O Output API to close the resource
///
/// Returns 0 or -1 in case of error
#[doc(alias = "xmlOutputCloseCallback")]
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
                                    XmlParserErrors::XmlI18NConvFailed,
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

    /// Create a buffered parser output
    ///
    /// Returns the new parser output or NULL
    #[doc(alias = "xmlAllocOutputBufferInternal")]
    pub fn new(encoder: Option<Rc<RefCell<XmlCharEncodingHandler>>>) -> Option<Self> {
        let mut ret = Self::default();
        let mut buf = XmlBufRef::new()?;
        buf.set_allocation_scheme(XmlBufferAllocationScheme::XmlBufferAllocDoubleit);
        ret.buffer = Some(buf);
        ret.encoder = encoder;
        if ret.encoder.is_some() {
            ret.conv = Some(XmlBufRef::with_capacity(4000)?);

            // This call is designed to initiate the encoder state
            ret.encode(true);
        }
        ret.writecallback = None;
        ret.closecallback = None;
        ret.context = null_mut();
        ret.written = 0;
        Some(ret)
    }

    /// Create a buffered  output for the progressive saving of a file.
    ///
    /// If filename is `"-"` then we use stdout as the output.
    ///
    /// In original libxml2, automatic support for ZLIB/Compress compressed document is provided
    /// by default if found at compile-time.  
    /// However, this crate does not support now.
    ///
    /// If the resource indicated by `uri` is found, return the new output buffer.  
    /// Otherwise, return `None`.
    #[doc(alias = "xmlOutputBufferCreateFilename")]
    pub unsafe fn from_uri(
        uri: &str,
        encoder: Option<Rc<RefCell<XmlCharEncodingHandler>>>,
        compression: i32,
    ) -> Option<Self> {
        if let Some(f) = GLOBAL_STATE.with_borrow(|state| state.output_buffer_create_filename_value)
        {
            return f(uri, encoder, compression);
        }
        __xml_output_buffer_create_filename(uri, encoder, compression)
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
                        xml_ioerr(XmlParserErrors::XmlIOEncoder, null());
                        self.error = XmlParserErrors::XmlIOEncoder;
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
                    xml_ioerr(XmlParserErrors::XmlIOWrite, null());
                    self.error = XmlParserErrors::XmlIOWrite;
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
                        xml_ioerr(XmlParserErrors::XmlIOEncoder, null());
                        self.error = XmlParserErrors::XmlIOEncoder;
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
                    xml_ioerr(XmlParserErrors::XmlIOWrite, null());
                    self.error = XmlParserErrors::XmlIOWrite;
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
                    xml_ioerr(XmlParserErrors::XmlIOEncoder, null());
                    self.error = XmlParserErrors::XmlIOEncoder;
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
            xml_ioerr(XmlParserErrors::XmlIOFlush, null());
            self.error = XmlParserErrors::XmlIOFlush;
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

impl Default for XmlOutputBuffer {
    fn default() -> Self {
        Self {
            context: null_mut(),
            writecallback: None,
            closecallback: None,
            encoder: None,
            buffer: None,
            conv: None,
            written: 0,
            error: XmlParserErrors::default(),
        }
    }
}

impl Drop for XmlOutputBuffer {
    fn drop(&mut self) {
        unsafe {
            self.flush();
            if let Some(close) = self.closecallback {
                close(self.context);
            }
            if let Some(buf) = self.buffer.take() {
                buf.free();
            }
            if let Some(conv) = self.conv.take() {
                conv.free();
            }
        }
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
pub(crate) struct XmlOutputCallback {
    pub(in crate::io) matchcallback: Option<XmlOutputMatchCallback>,
    pub(in crate::io) opencallback: Option<XmlOutputOpenCallback>,
    pub(in crate::io) writecallback: Option<XmlOutputWriteCallback>,
    pub(in crate::io) closecallback: Option<XmlOutputCloseCallback>,
}

const MAX_OUTPUT_CALLBACK: usize = 15;
pub(in crate::io) static XML_OUTPUT_CALLBACK_TABLE: Mutex<
    [XmlOutputCallback; MAX_OUTPUT_CALLBACK],
> = Mutex::new(
    [XmlOutputCallback {
        matchcallback: None,
        opencallback: None,
        writecallback: None,
        closecallback: None,
    }; MAX_OUTPUT_CALLBACK],
);
pub(in crate::io) static XML_OUTPUT_CALLBACK_NR: AtomicUsize = AtomicUsize::new(0);
pub(in crate::io) static XML_OUTPUT_CALLBACK_INITIALIZED: AtomicBool = AtomicBool::new(false);

/// clears the entire output callback table. this includes the compiled-in I/O callbacks.
#[doc(alias = "xmlCleanupOutputCallbacks")]
pub fn xml_cleanup_output_callbacks() {
    let is_initialized = XML_OUTPUT_CALLBACK_INITIALIZED.load(Ordering::Acquire);
    if !is_initialized {
        return;
    }

    let num_callbacks = XML_OUTPUT_CALLBACK_NR.load(Ordering::Acquire);
    let mut callbacks = XML_OUTPUT_CALLBACK_TABLE.lock().unwrap();
    callbacks[..num_callbacks].fill(XmlOutputCallback {
        matchcallback: None,
        opencallback: None,
        writecallback: None,
        closecallback: None,
    });

    XML_OUTPUT_CALLBACK_NR.store(0, Ordering::Release);
    XML_OUTPUT_CALLBACK_INITIALIZED.store(false, Ordering::Release);
}

/// Remove the top output callbacks from the output stack.  
/// This includes the compiled-in I/O.
///
/// Returns the number of output callback registered or -1 in case of error.
#[doc(alias = "xmlPopOutputCallbacks")]
pub fn xml_pop_output_callbacks() -> i32 {
    if !XML_OUTPUT_CALLBACK_INITIALIZED.load(Ordering::Acquire) {
        return -1;
    }

    let mut num_callbacks = XML_OUTPUT_CALLBACK_NR.load(Ordering::Acquire);
    if num_callbacks == 0 {
        return -1;
    }

    num_callbacks -= 1;
    let mut callbacks = XML_OUTPUT_CALLBACK_TABLE.lock().unwrap();
    callbacks[num_callbacks].matchcallback = None;
    callbacks[num_callbacks].opencallback = None;
    callbacks[num_callbacks].writecallback = None;
    callbacks[num_callbacks].closecallback = None;

    XML_OUTPUT_CALLBACK_NR.store(num_callbacks, Ordering::Release);
    num_callbacks as _
}

/// Registers the default compiled-in I/O handlers.
#[doc(alias = "xmlRegisterDefaultOutputCallbacks")]
pub unsafe extern "C" fn xml_register_default_output_callbacks() {
    if XML_OUTPUT_CALLBACK_INITIALIZED.load(Ordering::Acquire) {
        return;
    }

    xml_register_output_callbacks(
        Some(xml_file_match),
        Some(xml_file_open_w),
        Some(xml_file_write),
        Some(xml_file_close),
    );

    #[cfg(feature = "http")]
    {
        xml_register_output_callbacks(
            Some(xml_io_http_match),
            Some(xml_io_http_dflt_open_w),
            Some(xml_io_http_write),
            Some(xml_io_http_close_put),
        );
    }

    XML_OUTPUT_CALLBACK_INITIALIZED.store(true, Ordering::Release);
}

/// Register a new set of I/O callback for handling output.
///
/// Returns the registered handler number or -1 in case of error
#[doc(alias = "xmlRegisterOutputCallbacks")]
pub fn xml_register_output_callbacks(
    match_func: Option<XmlOutputMatchCallback>,
    open_func: Option<XmlOutputOpenCallback>,
    write_func: Option<XmlOutputWriteCallback>,
    close_func: Option<XmlOutputCloseCallback>,
) -> i32 {
    let num_callbacks = XML_OUTPUT_CALLBACK_NR.load(Ordering::Acquire);
    if num_callbacks >= MAX_OUTPUT_CALLBACK {
        return -1;
    }
    let mut callbacks = XML_OUTPUT_CALLBACK_TABLE.lock().unwrap();
    callbacks[num_callbacks].matchcallback = match_func;
    callbacks[num_callbacks].opencallback = open_func;
    callbacks[num_callbacks].writecallback = write_func;
    callbacks[num_callbacks].closecallback = close_func;
    XML_OUTPUT_CALLBACK_INITIALIZED.store(true, Ordering::Release);
    XML_OUTPUT_CALLBACK_NR.store(num_callbacks + 1, Ordering::Release);
    num_callbacks as _
}

pub(crate) unsafe fn __xml_output_buffer_create_filename(
    uri: &str,
    encoder: Option<Rc<RefCell<XmlCharEncodingHandler>>>,
    _compression: i32,
) -> Option<XmlOutputBuffer> {
    let mut context: *mut c_void = null_mut();

    let is_initialized = XML_OUTPUT_CALLBACK_INITIALIZED.load(Ordering::Acquire);
    if !is_initialized {
        xml_register_default_output_callbacks();
    }

    let unescaped = Url::parse(uri)
        .ok()
        .filter(|url| url.scheme() == "file")
        .and_then(|_| unescape_url(uri).ok());

    let num_callbacks = XML_OUTPUT_CALLBACK_NR.load(Ordering::Acquire);
    let callbacks = XML_OUTPUT_CALLBACK_TABLE.lock().unwrap();

    // Try to find one of the output accept method accepting that scheme
    // Go in reverse to give precedence to user defined handlers.
    // try with an unescaped version of the URI
    if let Some(Cow::Owned(unescaped)) = unescaped {
        for i in (0..num_callbacks).rev() {
            if callbacks[i]
                .matchcallback
                .filter(|callback| callback(&unescaped) != 0)
                .is_some()
            {
                let unescaped = CString::new(unescaped.as_str()).unwrap();
                context = (callbacks[i].opencallback.unwrap())(unescaped.as_ptr());
                if !context.is_null() {
                    // Allocate the Output buffer front-end.
                    let mut ret = XmlOutputBuffer::new(encoder)?;
                    ret.context = context;
                    ret.writecallback = callbacks[i].writecallback;
                    ret.closecallback = callbacks[i].closecallback;
                    return Some(ret);
                }
            }
        }
    }

    // If this failed try with a non-escaped URI this may be a strange filename
    if context.is_null() {
        for i in (0..num_callbacks).rev() {
            if callbacks[i]
                .matchcallback
                .filter(|callback| callback(uri) != 0)
                .is_some()
            {
                let uri = CString::new(uri).unwrap();
                context = (callbacks[i].opencallback.unwrap())(uri.as_ptr());
                if !context.is_null() {
                    // Allocate the Output buffer front-end.
                    let mut ret = XmlOutputBuffer::new(encoder)?;
                    ret.context = context;
                    ret.writecallback = callbacks[i].writecallback;
                    ret.closecallback = callbacks[i].closecallback;
                    return Some(ret);
                }
            }
        }
    }

    None
}
