use std::{
    ffi::c_void,
    ptr::{null, null_mut},
    str::from_utf8_mut,
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Mutex,
    },
};

use crate::{
    buf::XmlBufRef,
    encoding::{
        get_encoding_handler, xml_encoding_err, EncodingError, XmlCharEncoding,
        XmlCharEncodingHandler,
    },
    globals::GLOBAL_STATE,
    io::{
        xml_io_ftp_close, xml_io_ftp_match, xml_io_ftp_open, xml_io_ftp_read, xml_io_http_close,
        xml_io_http_match, xml_io_http_open, xml_io_http_read,
    },
    libxml::{tree::XmlBufferAllocationScheme, xmlerror::XmlParserErrors},
};

use super::{
    xml_file_close, xml_file_match, xml_file_open, xml_file_read, xml_ioerr, xml_ioerr_memory,
    MINLEN,
};

/**
 * xmlInputMatchCallback:
 * @filename: the filename or URI
 *
 * Callback used in the I/O Input API to detect if the current handler
 * can provide input functionality for this resource.
 *
 * Returns 1 if yes and 0 if another Input module should be used
 */
pub type XmlInputMatchCallback = unsafe extern "C" fn(filename: *const i8) -> i32;
/**
 * xmlInputOpenCallback:
 * @filename: the filename or URI
 *
 * Callback used in the I/O Input API to open the resource
 *
 * Returns an Input context or NULL in case or error
 */
pub type XmlInputOpenCallback = unsafe extern "C" fn(filename: *const i8) -> *mut c_void;
/**
 * xmlInputReadCallback:
 * @context:  an Input context
 * @buffer:  the buffer to store data read
 * @len:  the length of the buffer in bytes
 *
 * Callback used in the I/O Input API to read the resource
 *
 * Returns the number of bytes read or -1 in case of error
 */
pub type XmlInputReadCallback =
    unsafe extern "C" fn(context: *mut c_void, buffer: *mut i8, len: i32) -> i32;
/**
 * xmlInputCloseCallback:
 * @context:  an Input context
 *
 * Callback used in the I/O Input API to close the resource
 *
 * Returns 0 or -1 in case of error
 */
pub type XmlInputCloseCallback = unsafe extern "C" fn(context: *mut c_void) -> i32;

#[repr(C)]
pub struct XmlParserInputBuffer {
    pub(crate) context: *mut c_void,
    pub(crate) readcallback: Option<XmlInputReadCallback>,
    pub(crate) closecallback: Option<XmlInputCloseCallback>,
    pub(crate) encoder: Option<XmlCharEncodingHandler>, /* I18N conversions to UTF-8 */
    pub buffer: Option<XmlBufRef>,                      /* Local buffer encoded in UTF-8 */
    pub(crate) raw: Option<XmlBufRef>, /* if encoder != NULL buffer for raw input */
    pub(crate) compressed: i32,        /* -1=unknown, 0=not compressed, 1=compressed */
    pub(crate) error: XmlParserErrors,
    pub(crate) rawconsumed: u64, /* amount consumed from raw */
}

impl XmlParserInputBuffer {
    #[doc(alias = "xmlAllocParserInputBuffer")]
    pub fn new(enc: XmlCharEncoding) -> Self {
        let mut ret = XmlParserInputBuffer {
            context: null_mut(),
            readcallback: None,
            closecallback: None,
            encoder: None,
            buffer: None,
            raw: None,
            compressed: 0,
            error: XmlParserErrors::default(),
            rawconsumed: 0,
        };
        let default_buffer_size = GLOBAL_STATE.with_borrow(|state| state.default_buffer_size);
        let mut new_buf = XmlBufRef::with_capacity(2 * default_buffer_size).unwrap();
        ret.buffer = Some(new_buf);
        new_buf.set_allocation_scheme(XmlBufferAllocationScheme::XmlBufferAllocDoubleit);
        ret.encoder = get_encoding_handler(enc);
        ret.raw = if ret.encoder.is_some() {
            XmlBufRef::with_capacity(2 * default_buffer_size)
        } else {
            None
        };
        ret.readcallback = None;
        ret.closecallback = None;
        ret.context = null_mut();
        ret.compressed = -1;
        ret.rawconsumed = 0;
        ret
    }

    /// Generic front-end for the encoding handler on parser input.  
    /// If you try to flush all the raw buffer, set `flush` to `true`.
    ///
    /// If successfully encoded, return the number of written bytes.
    /// If not, return the following `EncodingError`.
    /// - general error (`EncodingError::Other`)
    /// - encoding failure (`EncodingError::Malformed`)
    pub(crate) fn decode(&mut self, flush: bool) -> Result<usize, EncodingError> {
        if self.encoder.is_none() || self.buffer.is_none() || self.raw.is_none() {
            return Err(EncodingError::Other {
                msg: "Encoder or Buffer is not set.".into(),
            });
        }
        let mut out = self.buffer.expect("Internal Error");
        let mut bufin = self.raw.expect("Internal Error");

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
        let ret = match self.encoder.as_mut().unwrap().decode(src, dst) {
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
                        format!("input conversion failed due to input error, bytes {buf}\n")
                            .as_str(),
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

    /// Refresh the content of the input buffer, the old data are considered consumed.  
    /// This routine handle the I18N transcoding to internal UTF-8.
    ///
    /// Returns the number of chars read and stored in the buffer, or -1 in case of error.
    #[doc(alias = "xmlParserInputBufferRead")]
    pub unsafe fn read(&mut self, len: i32) -> i32 {
        self.grow(len)
    }

    /// Grow up the content of the input buffer, the old data are preserved
    /// This routine handle the I18N transcoding to internal UTF-8
    /// This routine is used when operating the parser in normal (pull) mode
    ///
    /// TODO: one should be able to remove one extra copy by copying directly
    /// onto (*in).buffer or (*in).raw
    ///
    /// Returns the number of chars read and stored in the buffer, or -1 in case of error.
    #[doc(alias = "xmlParserInputBufferGrow")]
    pub unsafe fn grow(&mut self, mut len: i32) -> i32 {
        let mut res: i32 = 0;

        if !self.error.is_ok() {
            return -1;
        }
        if len <= MINLEN as i32 && len != 4 {
            len = MINLEN as i32;
        }

        let buf = if self.encoder.is_none() {
            if self.readcallback.is_none() {
                return 0;
            }
            self.buffer
        } else {
            if self.raw.is_none() {
                self.raw = XmlBufRef::new();
            }
            self.raw
        };

        /*
         * Call the read method for this I/O type.
         */
        if let Some(callback) = self.readcallback {
            if buf.map_or(true, |mut buf| buf.grow((len + 1) as usize).is_err()) {
                xml_ioerr_memory(c"growing input buffer".as_ptr() as _);
                self.error = XmlParserErrors::XmlErrNoMemory;
                return -1;
            }

            res = callback(
                self.context,
                buf.map_or(null_mut(), |buf| {
                    buf.as_ref().as_ptr().add(buf.len()) as *mut i8
                }),
                len,
            );
            if res <= 0 {
                self.readcallback = Some(end_of_input);
            }
            if res < 0 {
                return -1;
            }

            if buf.map_or(true, |mut buf| buf.add_len(res as usize).is_err()) {
                return -1;
            }
        }

        /*
         * try to establish compressed status of input if not done already
         */
        if self.compressed == -1 {
            // #ifdef LIBXML_LZMA_ENABLED
            // 	if ((*input).readcallback == xmlXzfileRead)
            //             (*input).compressed = __libxml2_xzcompressed((*input).context);
            // #endif
        }

        if self.encoder.is_some() {
            /*
             * convert as much as possible to the parser reading buffer.
             */
            let using = buf.map_or(0, |buf| buf.len());
            let Ok(written) = self.decode(true) else {
                xml_ioerr(XmlParserErrors::XmlIoEncoder, null());
                self.error = XmlParserErrors::XmlIoEncoder;
                return -1;
            };
            res = written as i32;
            let consumed = using - buf.map_or(0, |buf| buf.len());
            self.rawconsumed = self.rawconsumed.saturating_add(consumed as u64);
        }
        res
    }

    /// Push the content of the arry in the input buffer.  
    /// This routine handle the I18N transcoding to internal UTF-8.  
    /// This is used when operating the parser in progressive (push) mode.
    ///
    /// Returns the number of chars read and stored in the buffer, or -1 in case of error.
    #[doc(alias = "xmlParserInputBufferPush")]
    pub fn push_bytes(&mut self, buf: &[u8]) -> i32 {
        if !self.error.is_ok() {
            return -1;
        }
        if self.encoder.is_some() {
            /*
             * Store the data in the incoming raw buffer
             */
            if self.raw.is_none() {
                self.raw = XmlBufRef::new();
            }
            if self
                .raw
                .map_or(true, |mut raw| raw.push_bytes(buf).is_err())
            {
                return -1;
            }

            /*
             * convert as much as possible to the parser reading buffer.
             */
            let using = self.raw.map_or(0, |raw| raw.len());
            let Ok(written) = self.decode(true) else {
                unsafe {
                    xml_ioerr(XmlParserErrors::XmlIoEncoder, null());
                }
                self.error = XmlParserErrors::XmlIoEncoder;
                return -1;
            };
            let consumed = using - self.raw.map_or(0, |raw| raw.len());
            self.rawconsumed = self.rawconsumed.saturating_add(consumed as u64);
            written as i32
        } else {
            if self
                .buffer
                .expect("Internal Error")
                .push_bytes(buf)
                .is_err()
            {
                return -1;
            }
            buf.len() as i32
        }
    }
}

impl Drop for XmlParserInputBuffer {
    fn drop(&mut self) {
        if let Some(callback) = self.closecallback {
            unsafe {
                callback(self.context);
            }
            self.context = null_mut();
        }
        if let Some(buffer) = self.buffer.take() {
            buffer.free();
        }
        if let Some(raw) = self.raw.take() {
            raw.free();
        }
    }
}

/**
 * endOfInput:
 *
 * When reading from an Input channel indicated end of file or error
 * don't reread from it again.
 */
unsafe extern "C" fn end_of_input(_context: *mut c_void, _bufferr: *mut i8, _len: i32) -> i32 {
    0
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct XmlInputCallback {
    pub(in crate::io) matchcallback: Option<XmlInputMatchCallback>,
    pub(in crate::io) opencallback: Option<XmlInputOpenCallback>,
    pub(in crate::io) readcallback: Option<XmlInputReadCallback>,
    pub(in crate::io) closecallback: Option<XmlInputCloseCallback>,
}

pub(crate) const MAX_INPUT_CALLBACK: usize = 15;
pub(in crate::io) static XML_INPUT_CALLBACK_TABLE: Mutex<[XmlInputCallback; MAX_INPUT_CALLBACK]> =
    Mutex::new(
        [XmlInputCallback {
            matchcallback: None,
            opencallback: None,
            readcallback: None,
            closecallback: None,
        }; MAX_INPUT_CALLBACK],
    );
pub(in crate::io) static XML_INPUT_CALLBACK_NR: AtomicUsize = AtomicUsize::new(0);
pub(in crate::io) static XML_INPUT_CALLBACK_INITIALIZED: AtomicBool = AtomicBool::new(false);

/*
 * Interfaces for input
 */
/**
 * xmlCleanupInputCallbacks:
 *
 * clears the entire input callback table. this includes the
 * compiled-in I/O.
 */
pub fn xml_cleanup_input_callbacks() {
    let is_initialized = XML_INPUT_CALLBACK_INITIALIZED.load(Ordering::Acquire);
    if !is_initialized {
        return;
    }

    let num_callbacks = XML_INPUT_CALLBACK_NR.load(Ordering::Acquire);
    let mut callbacks = XML_INPUT_CALLBACK_TABLE.lock().unwrap();
    callbacks[..num_callbacks].fill(XmlInputCallback {
        matchcallback: None,
        opencallback: None,
        readcallback: None,
        closecallback: None,
    });

    XML_INPUT_CALLBACK_NR.store(0, Ordering::Release);
    XML_INPUT_CALLBACK_INITIALIZED.store(false, Ordering::Release);
}

/**
 * xmlPopInputCallbacks:
 *
 * Clear the top input callback from the input stack. this includes the
 * compiled-in I/O.
 *
 * Returns the number of input callback registered or -1 in case of error.
 */
pub fn xml_pop_input_callbacks() -> i32 {
    let is_initialized = XML_INPUT_CALLBACK_INITIALIZED.load(Ordering::Acquire);
    if !is_initialized {
        return -1;
    }

    let mut num_callbacks = XML_INPUT_CALLBACK_NR.load(Ordering::Acquire);
    if num_callbacks == 0 {
        return -1;
    }

    num_callbacks -= 1;
    let mut callbacks = XML_INPUT_CALLBACK_TABLE.lock().unwrap();
    callbacks[num_callbacks].matchcallback = None;
    callbacks[num_callbacks].opencallback = None;
    callbacks[num_callbacks].readcallback = None;
    callbacks[num_callbacks].closecallback = None;

    XML_INPUT_CALLBACK_NR.store(num_callbacks, Ordering::Release);
    num_callbacks as _
}

/**
 * xmlRegisterDefaultInputCallbacks:
 *
 * Registers the default compiled-in I/O handlers.
 */
pub fn xml_register_default_input_callbacks() {
    if XML_INPUT_CALLBACK_INITIALIZED.load(Ordering::Acquire) {
        return;
    }

    xml_register_input_callbacks(
        Some(xml_file_match),
        Some(xml_file_open),
        Some(xml_file_read),
        Some(xml_file_close),
    );
    // #ifdef LIBXML_ZLIB_ENABLED
    //     xmlRegisterInputCallbacks(xmlGzfileMatch, xmlGzfileOpen,
    // 	                      xmlGzfileRead, xmlGzfileClose);
    // #endif /* LIBXML_ZLIB_ENABLED */
    // #ifdef LIBXML_LZMA_ENABLED
    //     xmlRegisterInputCallbacks(xmlXzfileMatch, xmlXzfileOpen,
    // 	                      xmlXzfileRead, xmlXzfileClose);
    // #endif /* LIBXML_LZMA_ENABLED */
    #[cfg(feature = "http")]
    {
        xml_register_input_callbacks(
            Some(xml_io_http_match),
            Some(xml_io_http_open),
            Some(xml_io_http_read),
            Some(xml_io_http_close),
        );
    }
    #[cfg(feature = "ftp")]
    {
        xml_register_input_callbacks(
            Some(xml_io_ftp_match),
            Some(xml_io_ftp_open),
            Some(xml_io_ftp_read),
            Some(xml_io_ftp_close),
        );
    }
    XML_INPUT_CALLBACK_INITIALIZED.store(true, Ordering::Release);
}

/**
 * xmlRegisterInputCallbacks:
 * @matchFunc:  the xmlInputMatchCallback
 * @openFunc:  the xmlInputOpenCallback
 * @readFunc:  the xmlInputReadCallback
 * @closeFunc:  the xmlInputCloseCallback
 *
 * Register a new set of I/O callback for handling parser input.
 *
 * Returns the registered handler number or -1 in case of error
 */
pub fn xml_register_input_callbacks(
    match_func: Option<XmlInputMatchCallback>,
    open_func: Option<XmlInputOpenCallback>,
    read_func: Option<XmlInputReadCallback>,
    close_func: Option<XmlInputCloseCallback>,
) -> i32 {
    let num_callbacks = XML_INPUT_CALLBACK_NR.load(Ordering::Acquire);
    if num_callbacks >= MAX_INPUT_CALLBACK {
        return -1;
    }
    let mut callbacks = XML_INPUT_CALLBACK_TABLE.lock().unwrap();
    callbacks[num_callbacks].matchcallback = match_func;
    callbacks[num_callbacks].opencallback = open_func;
    callbacks[num_callbacks].readcallback = read_func;
    callbacks[num_callbacks].closecallback = close_func;
    XML_INPUT_CALLBACK_INITIALIZED.store(true, Ordering::Relaxed);
    XML_INPUT_CALLBACK_NR.store(num_callbacks + 1, Ordering::Release);
    num_callbacks as _
}
