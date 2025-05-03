// Copyright of the original code is the following.
// --------
// Summary: interface for the I/O interfaces used by the parser
// Description: interface for the I/O interfaces used by the parser
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// xmlIO.c : implementation of the I/O interfaces used by the parser
//
// See Copyright for the status of this software.
//
// daniel@veillard.com
//
// 14 Nov 2000 ht - for VMS, truncated name of long functions to under 32 char

use std::{
    any::{type_name, type_name_of_val},
    io::{self, Cursor, Read},
    mem::take,
    str::from_utf8_unchecked_mut,
    sync::{
        Mutex,
        atomic::{AtomicBool, Ordering},
    },
};

use crate::{
    encoding::{
        EncodingError, XmlCharEncoding, XmlCharEncodingHandler, get_encoding_handler,
        xml_encoding_err,
    },
    error::XmlParserErrors,
    globals::GLOBAL_STATE,
    io::DefaultHTTPIOCallbacks,
    nanohttp::XmlNanoHTTPCtxt,
};

use super::{DefaultFileIOCallbacks, MINLEN, xml_ioerr};

#[repr(C)]
pub struct XmlParserInputBuffer<'a> {
    pub(crate) context: Option<Box<dyn Read + 'a>>,
    pub(crate) encoder: Option<XmlCharEncodingHandler>, /* I18N conversions to UTF-8 */
    pub(crate) buffer: Vec<u8>,                         /* Local buffer encoded in UTF-8 */
    pub(crate) raw: Vec<u8>, /* if encoder != NULL buffer for raw input */
    pub(crate) error: XmlParserErrors,
    pub(crate) rawconsumed: u64, /* amount consumed from raw */
    pub(in crate::io) use_nanohttp: bool,
}

impl<'a> XmlParserInputBuffer<'a> {
    #[doc(alias = "xmlAllocParserInputBuffer")]
    pub fn new(enc: XmlCharEncoding) -> Self {
        let default_buffer_size = GLOBAL_STATE.with_borrow(|state| state.default_buffer_size);
        let mut ret = XmlParserInputBuffer {
            context: None,
            encoder: None,
            buffer: Vec::with_capacity(2 * default_buffer_size),
            raw: vec![],
            error: XmlParserErrors::default(),
            rawconsumed: 0,
            use_nanohttp: false,
        };
        ret.encoder = get_encoding_handler(enc);
        ret.rawconsumed = 0;
        ret
    }

    /// Create a buffered parser input for the progressive parsing for the input from a memory area.  
    /// Returns the new parser input buffer.
    ///
    /// TODO: Allow the slice as memory.
    #[doc(alias = "xmlParserInputBufferCreateMem")]
    pub fn from_memory(mem: &'a [u8], enc: XmlCharEncoding) -> Option<Self> {
        let mut ret = XmlParserInputBuffer::new(enc);
        ret.context = Some(Box::new(Cursor::new(mem)));
        Some(ret)
    }

    /// Create a buffered parser input for the progressive parsing for the input from an I/O handler.  
    /// Returns the new parser input buffer.
    #[doc(
        alias = "xmlParserInputBufferCreateIO",
        alias = "xmlParserInputBufferCreateFile"
    )]
    pub fn from_reader(reader: impl Read + 'a, enc: XmlCharEncoding) -> Self {
        if !XML_INPUT_CALLBACK_INITIALIZED.load(Ordering::Relaxed) {
            register_default_input_callbacks();
        }

        let mut ret = XmlParserInputBuffer::new(enc);
        ret.context = Some(Box::new(reader));
        ret
    }

    /// Create a buffered parser input for the progressive parsing of a file.  
    /// If filename is `"-"` then we use stdin as the input.  
    ///
    /// If successfully created, return the new parser input wrapped `Some`,
    /// otherwise return `None`.
    #[doc(alias = "xmlParserInputBufferCreateFilename")]
    pub fn from_uri(uri: &str, enc: XmlCharEncoding) -> Option<XmlParserInputBuffer<'a>> {
        if let Some(f) =
            GLOBAL_STATE.with_borrow(|state| state.parser_input_buffer_create_filename_value)
        {
            return f(uri, enc);
        }
        __xml_parser_input_buffer_create_filename(uri, enc)
    }

    /// Generic front-end for the encoding handler on parser input.  
    /// If you try to flush all the raw buffer, set `flush` to `true`.
    ///
    /// If successfully encoded, return the number of written bytes.
    /// If not, return the following `EncodingError`.
    /// - general error (`EncodingError::Other`)
    /// - encoding failure (`EncodingError::Malformed`)
    pub(crate) fn decode(&mut self, flush: bool) -> Result<usize, EncodingError> {
        if self.encoder.is_none() {
            return Err(EncodingError::Other {
                msg: "Encoder or Buffer is not set.".into(),
            });
        }
        let mut toconv = self.raw.len();
        if toconv == 0 {
            return Ok(0);
        }
        if !flush {
            toconv = toconv.min(64 * 1024);
        }
        let mut written = (toconv * 2).max(6);
        if !flush {
            written = written.min(128 * 1024);
        }

        let c_in = toconv;
        let c_out = written;
        let src = &self.raw[..c_in];
        let start = self.buffer.len();
        self.buffer.resize(start + c_out, 0);
        let dst = unsafe {
            // # Safety
            // `self.buffer[start..]` contains only NULL characters ('\0').
            // Therefore, UTF-8 validation won't fail.
            from_utf8_unchecked_mut(&mut self.buffer[start..])
        };
        let ret = match self.encoder.as_mut().unwrap().decode(src, dst) {
            Ok((read, write)) => {
                self.raw.drain(..read);
                self.buffer.truncate(start + write);
                Ok(write)
            }
            Err(EncodingError::BufferTooShort) => {
                self.buffer.truncate(start);
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
                self.raw.drain(..read - length - offset);
                self.buffer.truncate(start + write);
                let buf = format!(
                    "0x{:02X} 0x{:02X} 0x{:02X} 0x{:02X}",
                    self.raw.first().unwrap_or(&0),
                    self.raw.get(1).unwrap_or(&0),
                    self.raw.get(2).unwrap_or(&0),
                    self.raw.get(3).unwrap_or(&0)
                );

                xml_encoding_err!(
                    XmlParserErrors::XmlI18NConvFailed,
                    "input conversion failed due to input error, bytes {}\n",
                    buf.as_str()
                );
                if write > 0 { Ok(write) } else { Err(e) }
            }
            _ => {
                self.buffer.truncate(start);
                Ok(0)
            }
        };
        ret
    }

    /// Refresh the content of the input buffer, the old data are considered consumed.  
    /// This routine handle the I18N transcoding to internal UTF-8.
    ///
    /// Returns the number of chars read and stored in the buffer, or -1 in case of error.
    #[doc(alias = "xmlParserInputBufferRead")]
    pub fn read(&mut self, len: usize) -> i32 {
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
    pub fn grow(&mut self, mut len: usize) -> i32 {
        let mut res: i32 = 0;

        if !self.error.is_ok() {
            return -1;
        }
        if len <= MINLEN && len != 4 {
            len = MINLEN;
        }

        if self.encoder.is_none() && self.context.is_none() {
            return 0;
        }

        // Call the read method for this I/O type.
        if let Some(context) = self.context.as_mut() {
            let buffer = if self.encoder.is_none() {
                &mut self.buffer
            } else {
                &mut self.raw
            };
            let start = buffer.len();
            buffer.resize(start + len, 0);
            let Ok(len) = context.read(&mut buffer[start..start + len]) else {
                return -1;
            };
            buffer.truncate(start + len);
            res = len as i32;
        }

        let written = self.push_bytes(&[]);
        if self.encoder.is_some() { written } else { res }
    }

    /// Trim `len` bytes at the head of `self.buffer`.
    ///
    /// # Panics
    /// - `len <= self.buffer.len()` must be satisfied.
    pub(crate) fn trim_head(&mut self, len: usize) {
        assert!(len <= self.buffer.len());
        self.buffer.drain(..len);
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
            self.raw.extend(buf);
            // convert as much as possible to the parser reading buffer.
            let using = self.raw.len();
            let Ok(written) = self.decode(true) else {
                xml_ioerr(XmlParserErrors::XmlIOEncoder, None);
                self.error = XmlParserErrors::XmlIOEncoder;
                return -1;
            };
            let consumed = using - self.raw.len();
            self.rawconsumed = self.rawconsumed.saturating_add(consumed as u64);
            written as i32
        } else {
            self.buffer.extend(buf);
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

    /// Fallback UTF-8 buffers to ISO-8859-1.  
    /// This method moves the contents of the buffer to `self.raw`
    /// and decodes it again with a decoder for ISO-8859-1.
    ///
    /// # Panics
    /// - `self.encoder` must be `None`.
    /// - `self.raw` must be empty.
    pub(crate) fn fallback_to_iso_8859_1(&mut self) {
        assert!(self.encoder.is_none());
        assert!(self.raw.is_empty());

        let buf = take(&mut self.buffer);
        self.encoder = get_encoding_handler(XmlCharEncoding::ISO8859_1);
        self.push_bytes(&buf);
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

    register_input_callbacks(DefaultFileIOCallbacks).ok();
    #[cfg(feature = "http")]
    {
        register_input_callbacks(DefaultHTTPIOCallbacks { write_method: "" }).ok();
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

pub(crate) fn __xml_parser_input_buffer_create_filename(
    uri: &str,
    enc: XmlCharEncoding,
) -> Option<XmlParserInputBuffer<'static>> {
    if !XML_INPUT_CALLBACK_INITIALIZED.load(Ordering::Acquire) {
        register_default_input_callbacks();
    }

    let mut callbacks = XML_INPUT_CALLBACK_TABLE.lock().unwrap();
    // Try to find one of the input accept method accepting that scheme
    // Go in reverse to give precedence to user defined handlers.
    for (callback, is_nanohttp) in callbacks.iter_mut().rev() {
        if callback.is_match(uri) {
            if let Ok(context) = callback.open(uri) {
                // Allocate the Input buffer front-end.
                let mut ret = XmlParserInputBuffer::new(enc);
                ret.context = Some(context);
                ret.use_nanohttp = *is_nanohttp;
                return Some(ret);
            }
        }
    }
    None
}
