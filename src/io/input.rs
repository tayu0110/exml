use std::{
    any::{type_name, type_name_of_val},
    ffi::c_void,
    io::{self, Cursor, Read},
    ptr::null,
    str::from_utf8_mut,
    sync::{
        atomic::{AtomicBool, Ordering},
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
    io::DefaultHTTPIOCallbacks,
    libxml::{tree::XmlBufferAllocationScheme, xmlerror::XmlParserErrors},
    nanohttp::XmlNanoHTTPCtxt,
};

use super::{xml_ioerr, xml_ioerr_memory, DefaultFileIOCallbacks, MINLEN};

/**
 * xmlInputMatchCallback:
 * @filename: the filename or URI
 *
 * Callback used in the I/O Input API to detect if the current handler
 * can provide input functionality for this resource.
 *
 * Returns 1 if yes and 0 if another Input module should be used
 */
pub type XmlInputMatchCallback = unsafe fn(filename: &str) -> i32;
/**
 * xmlInputOpenCallback:
 * @filename: the filename or URI
 *
 * Callback used in the I/O Input API to open the resource
 *
 * Returns an Input context or NULL in case or error
 */
pub type XmlInputOpenCallback = unsafe fn(filename: &str) -> *mut c_void;
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
    pub(crate) context: Option<Box<dyn Read>>,
    pub(crate) encoder: Option<XmlCharEncodingHandler>, /* I18N conversions to UTF-8 */
    pub buffer: Option<XmlBufRef>,                      /* Local buffer encoded in UTF-8 */
    pub(crate) raw: Option<XmlBufRef>, /* if encoder != NULL buffer for raw input */
    pub(crate) compressed: i32,        /* -1=unknown, 0=not compressed, 1=compressed */
    pub(crate) error: XmlParserErrors,
    pub(crate) rawconsumed: u64, /* amount consumed from raw */
    pub(in crate::io) use_nanohttp: bool,
}

impl XmlParserInputBuffer {
    #[doc(alias = "xmlAllocParserInputBuffer")]
    pub fn new(enc: XmlCharEncoding) -> Self {
        let mut ret = XmlParserInputBuffer {
            context: None,
            encoder: None,
            buffer: None,
            raw: None,
            compressed: 0,
            error: XmlParserErrors::default(),
            rawconsumed: 0,
            use_nanohttp: false,
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
        ret.compressed = -1;
        ret.rawconsumed = 0;
        ret
    }

    /// Create a buffered parser input for the progressive parsing for the input from a memory area.  
    /// Returns the new parser input buffer.
    ///
    /// TODO: Allow the slice as memory.
    #[doc(alias = "xmlParserInputBufferCreateMem")]
    pub fn from_memory(mem: Vec<u8>, enc: XmlCharEncoding) -> Option<Self> {
        let mut ret = XmlParserInputBuffer::new(enc);
        // I believe `mem` should be set as `context`,
        // but for some reason the `testchar::test_user_encoding` test fails...
        //
        // Until the cause is found, push data directly into the buffer
        // and set the `context` to an empty source, as in the original code.
        ret.context = Some(Box::new(Cursor::new(vec![])));
        ret.buffer.as_mut().unwrap().push_bytes(&mem).ok()?;
        Some(ret)
    }

    /// Create a buffered parser input for the progressive parsing for the input from an I/O handler.  
    /// Returns the new parser input buffer.
    #[doc(alias = "xmlParserInputBufferCreateIO")]
    #[doc(alias = "xmlParserInputBufferCreateFile")]
    pub fn from_reader(reader: impl Read + 'static, enc: XmlCharEncoding) -> Self {
        if !XML_INPUT_CALLBACK_INITIALIZED.load(Ordering::Relaxed) {
            register_default_input_callbacks();
        }

        let mut ret = XmlParserInputBuffer::new(enc);
        ret.context = Some(Box::new(reader));
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
    pub fn read(&mut self, len: i32) -> i32 {
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
    pub fn grow(&mut self, mut len: i32) -> i32 {
        let mut res: i32 = 0;

        if !self.error.is_ok() {
            return -1;
        }
        if len <= MINLEN as i32 && len != 4 {
            len = MINLEN as i32;
        }

        let mut buf = if self.encoder.is_none() {
            if self.context.is_none() {
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
        if let Some(context) = self.context.as_mut() {
            if buf.map_or(true, |mut buf| buf.grow((len + 1) as usize).is_err()) {
                unsafe {
                    xml_ioerr_memory(c"growing input buffer".as_ptr() as _);
                }
                self.error = XmlParserErrors::XmlErrNoMemory;
                return -1;
            }

            let mut buffer = vec![0; len as usize];
            let Ok(len) = context.read(&mut buffer) else {
                return -1;
            };
            buf.as_mut().unwrap().push_bytes(&buffer[..len]);
            res = len as i32;
        }

        /*
         * try to establish compressed status of input if not done already
         */
        if self.compressed == -1 {
            // TODO: related with LIBXML_LZMA_ENABLED
        }

        if self.encoder.is_some() {
            /*
             * convert as much as possible to the parser reading buffer.
             */
            let using = buf.map_or(0, |buf| buf.len());
            let Ok(written) = self.decode(true) else {
                unsafe {
                    xml_ioerr(XmlParserErrors::XmlIoEncoder, null());
                }
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

    /// If the context of this buffer is `XmlNanoHTTPCtxt`, return the mutable context wrapped `Some`,
    /// otherwise return `None`.
    pub(crate) fn nanohttp_context(&mut self) -> Option<&mut XmlNanoHTTPCtxt> {
        self.use_nanohttp
            .then(|| {
                self.context.as_deref_mut().map(|ctxt| unsafe {
                    // Safety
                    // `use_nanohttp` is true only when the context is `XmlNanoHTTPCtxt`.
                    // The context is a Boxed Trait object,
                    // a fat-pointer consisting of the original element and a pointer to the vtable.
                    // As long as the constraints of `use_nanohttp` are observed, this conversion is safe.
                    let ptr = ctxt as *mut dyn Read;
                    let ptr = ptr as *mut XmlNanoHTTPCtxt;
                    &mut *ptr
                })
            })
            .flatten()
    }
}

impl Drop for XmlParserInputBuffer {
    fn drop(&mut self) {
        if let Some(buffer) = self.buffer.take() {
            buffer.free();
        }
        if let Some(raw) = self.raw.take() {
            raw.free();
        }
    }
}

pub trait XmlInputCallback: Send {
    fn is_match(&self, filename: &str) -> bool;
    fn open(&mut self, filename: &str) -> io::Result<Box<dyn Read>>;
}

pub(crate) const MAX_INPUT_CALLBACK: usize = 15;
pub(in crate::io) static XML_INPUT_CALLBACK_TABLE: Mutex<Vec<(Box<dyn XmlInputCallback>, bool)>> =
    Mutex::new(vec![]);
pub(in crate::io) static XML_INPUT_CALLBACK_INITIALIZED: AtomicBool = AtomicBool::new(false);

/// clears the entire input callback table. this includes the compiled-in I/O.
#[doc(alias = "xmlCleanupInputCallbacks")]
pub fn cleanup_input_callbacks() {
    let is_initialized = XML_INPUT_CALLBACK_INITIALIZED.load(Ordering::Acquire);
    if !is_initialized {
        return;
    }

    let mut callbacks = XML_INPUT_CALLBACK_TABLE.lock().unwrap();
    callbacks.clear();

    XML_INPUT_CALLBACK_INITIALIZED.store(false, Ordering::Release);
}

/// Clear the top input callback from the input stack. this includes the compiled-in I/O.  
/// Returns the number of input callback registered after execution.
///
/// Even if no callbacks are registered, this function does not fail and return `0`.
#[doc(alias = "xmlPopInputCallbacks")]
pub fn pop_input_callbacks() -> usize {
    let is_initialized = XML_INPUT_CALLBACK_INITIALIZED.load(Ordering::Acquire);
    if !is_initialized {
        return 0;
    }

    let mut callbacks = XML_INPUT_CALLBACK_TABLE.lock().unwrap();
    callbacks.pop();
    callbacks.len()
}

/// Registers the default compiled-in I/O handlers.
#[doc(alias = "xmlRegisterDefaultInputCallbacks")]
pub fn register_default_input_callbacks() {
    if XML_INPUT_CALLBACK_INITIALIZED.load(Ordering::Acquire) {
        return;
    }

    register_input_callbacks(DefaultFileIOCallbacks);
    #[cfg(feature = "http")]
    {
        register_input_callbacks(DefaultHTTPIOCallbacks);
    }
    #[cfg(feature = "ftp")]
    {
        // TODO:
        // xml_register_input_callbacks(
        //     Some(xml_io_ftp_match),
        //     Some(xml_io_ftp_open),
        //     Some(xml_io_ftp_read),
        //     Some(xml_io_ftp_close),
        // );
    }
    XML_INPUT_CALLBACK_INITIALIZED.store(true, Ordering::Release);
}

/// Register a new set of I/O callback for handling parser input.  
/// If success, returns the registered handler number wrapped `Ok`, otherwise return `Err`.
///
/// This function fails if the number of callbacks exceeds `MAX_INPUT_CALLBACKS`.
#[doc(alias = "xmlRegisterInputCallbacks")]
pub fn register_input_callbacks(callback: impl XmlInputCallback + 'static) -> io::Result<usize> {
    let mut callbacks = XML_INPUT_CALLBACK_TABLE.lock().unwrap();
    if callbacks.len() == MAX_INPUT_CALLBACK {
        return Err(io::Error::other("Too many input callbacks."));
    }
    let is_nanohttp = type_name_of_val(&callback) == type_name::<DefaultHTTPIOCallbacks>();
    callbacks.push((Box::new(callback), is_nanohttp));
    XML_INPUT_CALLBACK_INITIALIZED.store(true, Ordering::Relaxed);
    Ok(callbacks.len())
}
