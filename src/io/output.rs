use std::{
    borrow::Cow,
    cell::RefCell,
    ffi::{c_void, CString},
    fs::File,
    io::{self, stdout, Write},
    path::Path,
    ptr::null,
    rc::Rc,
    str::from_utf8,
    sync::{
        atomic::{AtomicBool, Ordering},
        Mutex,
    },
};

use url::Url;

use crate::{
    buf::XmlBufRef,
    encoding::{floor_char_boundary, xml_encoding_err, EncodingError, XmlCharEncodingHandler},
    error::XmlParserErrors,
    globals::GLOBAL_STATE,
    libxml::uri::unescape_url,
    nanohttp::xml_nanohttp_method,
    tree::XmlBufferAllocationScheme,
};

use super::{
    xml_escape_content, xml_ioerr, DefaultFileIOCallbacks, DefaultHTTPIOCallbacks, MINLEN,
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

#[repr(C)]
#[derive(Default)]
pub struct XmlOutputBuffer<'a> {
    pub(crate) context: Option<Box<dyn Write + 'a>>,
    pub(crate) encoder: Option<Rc<RefCell<XmlCharEncodingHandler>>>, /* I18N conversions to UTF-8 */
    pub(crate) buffer: Option<XmlBufRef>, /* Local buffer encoded in UTF-8 or ISOLatin */
    pub(crate) conv: Option<XmlBufRef>,   /* if encoder != NULL buffer for output */
    pub(crate) written: i32,              /* total number of byte written */
    pub(crate) error: XmlParserErrors,
}

impl<'a> XmlOutputBuffer<'a> {
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
                        out.push_bytes(&dst[..write]).ok();
                        Ok(write)
                    }
                    Err(EncodingError::Unmappable {
                        read: _,
                        write,
                        c: _,
                    }) => {
                        out.push_bytes(&dst[..write]).ok();
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
                out.grow(toconv * 4).ok();
                written = out.avail();
            }
            written = written.min(256 * 1024);

            let c_in = floor_char_boundary(bufin.as_ref(), toconv);
            let c_out = written;
            let mut dst = vec![0; c_out];
            match encoder.encode(from_utf8(&bufin.as_ref()[..c_in]).unwrap(), &mut dst) {
                Ok((read, write)) => {
                    bufin.trim_head(read);
                    out.push_bytes(&dst[..write]).ok();
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
                    out.push_bytes(&dst[..write]).ok();
                    writtentot += write;

                    let charref = format!("&#{};", c as u32);
                    let charref_len = charref.len();

                    out.grow(charref_len * 4).ok();
                    let c_out = out.avail();
                    let mut dst = vec![0; c_out];
                    let result = encoder.encode(&charref, &mut dst);

                    match result {
                        Ok((read, write)) if read == charref_len => {
                            out.push_bytes(&dst[..write]).ok();
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
                            out.push_bytes(b" ").ok();
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
    #[doc(alias = "xmlAllocOutputBuffer")]
    #[cfg(feature = "output")]
    pub fn new(encoder: Option<XmlCharEncodingHandler>) -> Option<XmlOutputBuffer<'a>> {
        Self::from_wrapped_encoder(encoder.map(|e| Rc::new(RefCell::new(e))))
    }

    /// Create a buffered parser output
    ///
    /// If successfully created, return it. Otherwise return `None`.
    #[doc(alias = "xmlAllocOutputBufferInternal")]
    pub(crate) fn from_wrapped_encoder(
        encoder: Option<Rc<RefCell<XmlCharEncodingHandler>>>,
    ) -> Option<Self> {
        let mut ret = Self::default();
        let mut buf = XmlBufRef::new()?;
        buf.set_allocation_scheme(XmlBufferAllocationScheme::XmlBufferAllocDoubleit)
            .ok();
        ret.buffer = Some(buf);
        ret.encoder = encoder;
        if ret.encoder.is_some() {
            ret.conv = Some(XmlBufRef::with_capacity(4000)?);

            // This call is designed to initiate the encoder state
            ret.encode(true).ok();
        }
        ret.context = None;
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
    pub unsafe fn from_uri<'b: 'a>(
        uri: &'b str,
        encoder: Option<Rc<RefCell<XmlCharEncodingHandler>>>,
        compression: i32,
    ) -> Option<Self> {
        if let Some(f) = GLOBAL_STATE.with_borrow(|state| state.output_buffer_create_filename_value)
        {
            return f(uri, encoder, compression);
        }
        __xml_output_buffer_create_filename(uri, encoder, compression)
    }

    /// Create a buffered output for the progressive saving to a *mut FILE buffered C I/O.
    ///
    /// Returns the new parser output or NULL
    #[doc(alias = "xmlOutputBufferCreateFile")]
    pub fn from_writer(
        writer: impl Write + 'a,
        encoder: Option<XmlCharEncodingHandler>,
    ) -> Option<Self> {
        Self::from_writer_with_wrapped_encoder(writer, encoder.map(|e| Rc::new(RefCell::new(e))))
    }

    pub(crate) fn from_writer_with_wrapped_encoder(
        writer: impl Write + 'a,
        encoder: Option<Rc<RefCell<XmlCharEncodingHandler>>>,
    ) -> Option<Self> {
        if !XML_OUTPUT_CALLBACK_INITIALIZED.load(Ordering::Acquire) {
            register_default_output_callbacks();
        }

        XmlOutputBuffer::from_wrapped_encoder(encoder).map(|mut buf| {
            buf.context = Some(Box::new(writer));
            buf
        })
    }

    /// Write the content of the array in the output I/O buffer.  
    /// This routine handle the I18N transcoding from internal UTF-8.  
    /// The buffer is lossless, i.e. will store in case of partial or delayed writes.
    ///
    /// Returns the number of chars immediately written, or -1 in case of error.
    #[doc(alias = "xmlOutputBufferWrite")]
    pub unsafe fn write_bytes(&mut self, buf: &[u8]) -> i32 {
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
                if self.context.is_some() {
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
                if self.context.is_some() {
                    self.buffer.map_or(0, |buf| buf.len())
                } else {
                    buf.len()
                }
            };
            len -= buf.len();

            if let Some(context) = self.context.as_mut() {
                if nbchars < MINLEN && len == 0 {
                    break;
                }

                // second write the stuff to the I/O channel
                let buffer = if self.encoder.is_some() {
                    self.conv
                } else {
                    self.buffer
                };
                match buffer.map(|buf| context.write(&buf.as_ref()[..nbchars])) {
                    Some(Ok(ret)) => {
                        buffer.unwrap().trim_head(ret);
                        self.written = self.written.saturating_add(ret as i32);
                    }
                    _ => {
                        xml_ioerr(XmlParserErrors::XmlIOWrite, null());
                        self.error = XmlParserErrors::XmlIOWrite;
                        return -1;
                    }
                }
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
        let ret; /* return from function call */
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
                buffer.push_bytes(buf.as_bytes()).ok();
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
                if self.context.is_some() {
                    conv.len() as i32
                } else {
                    ret.unwrap_or(0) as i32
                }
            } else {
                let mut buf = String::new();
                ret = escaping(str, &mut buf);
                buffer.push_bytes(buf.as_bytes()).ok();
                if ret < 0 || buf.is_empty() {
                    /* chunk==0 => nothing done */
                    return -1;
                }
                if self.context.is_some() {
                    buffer.len() as i32
                } else {
                    buf.len() as i32
                }
            };

            if let Some(context) = self.context.as_mut() {
                if nbchars < MINLEN as i32 {
                    // goto done;
                    return written;
                }

                let buffer = if self.encoder.is_some() {
                    self.conv
                } else {
                    self.buffer
                };
                // second write the stuff to the I/O channel
                match buffer.map(|buf| context.write(&buf.as_ref()[..nbchars as usize])) {
                    Some(Ok(ret)) => {
                        buffer.unwrap().trim_head(ret);
                        self.written = self.written.saturating_add(ret as i32);
                    }
                    _ => {
                        xml_ioerr(XmlParserErrors::XmlIOWrite, null());
                        self.error = XmlParserErrors::XmlIOWrite;
                        return -1;
                    }
                }
            } else if buffer.avail() < MINLEN {
                buffer.grow(MINLEN).ok();
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

        let Some(context) = self.context.as_mut() else {
            return 0;
        };

        // second flush the stuff to the I/O channel
        let buf = if self.encoder.is_some() {
            self.conv
        } else {
            self.buffer
        };
        match buf.map(|buf| context.write(buf.as_ref())) {
            Some(Ok(ret)) => {
                buf.unwrap().trim_head(ret);
                self.written = self.written.saturating_add(ret as i32);
                ret as i32
            }
            _ => {
                xml_ioerr(XmlParserErrors::XmlIOFlush, null());
                self.error = XmlParserErrors::XmlIOFlush;
                -1
            }
        }
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

impl Drop for XmlOutputBuffer<'_> {
    fn drop(&mut self) {
        unsafe {
            self.flush();
            if let Some(buf) = self.buffer.take() {
                buf.free();
            }
            if let Some(conv) = self.conv.take() {
                conv.free();
            }
        }
    }
}

pub trait XmlOutputCallback: Send {
    fn is_match(&self, filename: &str) -> bool;
    fn open(&mut self, filename: &str) -> io::Result<Box<dyn Write>>;
}

const MAX_OUTPUT_CALLBACK: usize = 15;
pub(in crate::io) static XML_OUTPUT_CALLBACK_TABLE: Mutex<Vec<Box<dyn XmlOutputCallback>>> =
    Mutex::new(vec![]);
pub(in crate::io) static XML_OUTPUT_CALLBACK_INITIALIZED: AtomicBool = AtomicBool::new(false);

/// clears the entire output callback table. this includes the compiled-in I/O callbacks.
#[doc(alias = "xmlCleanupOutputCallbacks")]
pub fn cleanup_output_callbacks() {
    let is_initialized = XML_OUTPUT_CALLBACK_INITIALIZED.load(Ordering::Acquire);
    if !is_initialized {
        return;
    }

    let mut callbacks = XML_OUTPUT_CALLBACK_TABLE.lock().unwrap();
    callbacks.clear();

    XML_OUTPUT_CALLBACK_INITIALIZED.store(false, Ordering::Release);
}

/// Remove the top output callbacks from the output stack.  
/// This includes the compiled-in I/O.
///
/// Even if no callbacks are registered, this function does not fail and return `0`.
#[doc(alias = "xmlPopOutputCallbacks")]
pub fn pop_output_callbacks() -> usize {
    if !XML_OUTPUT_CALLBACK_INITIALIZED.load(Ordering::Acquire) {
        return 0;
    }

    let mut callbacks = XML_OUTPUT_CALLBACK_TABLE.lock().unwrap();
    callbacks.pop();
    callbacks.len()
}

/// Registers the default compiled-in I/O handlers.
#[doc(alias = "xmlRegisterDefaultOutputCallbacks")]
pub fn register_default_output_callbacks() {
    if XML_OUTPUT_CALLBACK_INITIALIZED.load(Ordering::Acquire) {
        return;
    }

    register_output_callbacks(DefaultFileIOCallbacks).ok();

    #[cfg(feature = "http")]
    {
        register_output_callbacks(DefaultHTTPIOCallbacks {
            write_method: "PUT",
        })
        .ok();
    }

    XML_OUTPUT_CALLBACK_INITIALIZED.store(true, Ordering::Release);
}

/// Register a new set of I/O callback for handling output.
///
/// Returns the registered handler number or -1 in case of error
#[doc(alias = "xmlRegisterOutputCallbacks")]
pub fn register_output_callbacks(callback: impl XmlOutputCallback + 'static) -> io::Result<usize> {
    let mut callbacks = XML_OUTPUT_CALLBACK_TABLE.lock().unwrap();
    if callbacks.len() == MAX_OUTPUT_CALLBACK {
        return Err(io::Error::other("Too many input callbacks."));
    }
    callbacks.push(Box::new(callback));
    XML_OUTPUT_CALLBACK_INITIALIZED.store(true, Ordering::Release);
    Ok(callbacks.len())
}

pub(crate) unsafe fn __xml_output_buffer_create_filename(
    uri: &str,
    encoder: Option<Rc<RefCell<XmlCharEncodingHandler>>>,
    _compression: i32,
) -> Option<XmlOutputBuffer> {
    let is_initialized = XML_OUTPUT_CALLBACK_INITIALIZED.load(Ordering::Acquire);
    if !is_initialized {
        register_default_output_callbacks();
    }

    let unescaped = Url::parse(uri)
        .ok()
        .filter(|url| url.scheme() == "file")
        .and_then(|_| unescape_url(uri).ok());

    let mut callbacks = XML_OUTPUT_CALLBACK_TABLE.lock().unwrap();

    // Try to find one of the output accept method accepting that scheme
    // Go in reverse to give precedence to user defined handlers.
    // try with an unescaped version of the URI
    if let Some(Cow::Owned(unescaped)) = unescaped {
        for callback in callbacks.iter_mut().rev() {
            if callback.is_match(&unescaped) {
                if let Ok(context) = callback.open(unescaped.as_str()) {
                    // Allocate the Output buffer front-end.
                    let mut ret = XmlOutputBuffer::from_wrapped_encoder(encoder)?;
                    ret.context = Some(context);
                    return Some(ret);
                }
            }
        }
    }

    // If this failed try with a non-escaped URI this may be a strange filename
    for callback in callbacks.iter_mut().rev() {
        if callback.is_match(uri) {
            if let Ok(context) = callback.open(uri) {
                // Allocate the Output buffer front-end.
                let mut ret = XmlOutputBuffer::from_wrapped_encoder(encoder)?;
                ret.context = Some(context);
                return Some(ret);
            }
        }
    }

    None
}

impl XmlOutputCallback for DefaultFileIOCallbacks {
    fn is_match(&self, _filename: &str) -> bool {
        true
    }

    fn open(&mut self, filename: &str) -> io::Result<Box<dyn io::Write>> {
        if filename == "-" {
            return Ok(Box::new(stdout()));
        }

        let filename = if let Ok(Ok(name)) = Url::parse(filename).map(|url| url.to_file_path()) {
            Cow::Owned(name)
        } else {
            Cow::Borrowed(Path::new(filename))
        };

        File::options()
            .write(true)
            .truncate(true)
            .create(true)
            .open(filename.as_ref())
            .inspect_err(|_| unsafe {
                let path = CString::new(filename.to_string_lossy().as_ref()).unwrap();
                xml_ioerr(XmlParserErrors::XmlErrOK, path.as_ptr())
            })
            .map(|file| Box::new(file) as Box<dyn Write>)
    }
}

#[cfg(feature = "http")]
#[repr(C)]
pub struct XmlIOHTTPWriteCtxt {
    compression: i32,
    uri: String,
    doc_buff: XmlOutputBuffer<'static>,
    method: &'static str,
}

#[cfg(feature = "http")]
impl XmlOutputCallback for DefaultHTTPIOCallbacks {
    fn is_match(&self, filename: &str) -> bool {
        filename.starts_with("http://")
    }

    #[doc(alias = "xmlIOHTTPOpenW")]
    fn open(&mut self, filename: &str) -> io::Result<Box<dyn Write>> {
        let ctxt = XmlIOHTTPWriteCtxt {
            compression: 0,
            uri: filename.to_owned(),
            doc_buff: XmlOutputBuffer::from_wrapped_encoder(None)
                .ok_or(io::Error::other("Failed to create XmlOutputBuffer"))?,
            method: self.write_method,
        };
        Ok(Box::new(ctxt))
    }
}

#[cfg(feature = "http")]
impl Write for XmlIOHTTPWriteCtxt {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        if !buf.is_empty() {
            let len = unsafe { self.doc_buff.write_bytes(buf) };
            if len < 0 {
                let error = io::Error::last_os_error();
                let msg = format!(
                    "xmlIOHTTPWrite:  {}\n{} '{}'.\n",
                    "Error appending to internal buffer.",
                    "Error sending document to URI",
                    self.uri
                );
                let msg = CString::new(msg).unwrap();
                unsafe {
                    xml_ioerr(XmlParserErrors::XmlIOWrite, msg.as_ptr());
                }
                return Err(error);
            }
            return Ok(len as usize);
        }
        Ok(0)
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

#[cfg(feature = "http")]
impl Drop for XmlIOHTTPWriteCtxt {
    fn drop(&mut self) {
        // Pull the data out of the memory output buffer

        let dctxt = &mut self.doc_buff;
        let http_content = dctxt.buffer.filter(|buf| buf.is_ok());
        let content_lgth = dctxt.buffer.map_or(0, |buf| buf.len()) as i32;

        if let Some(http_content) = http_content {
            let mut content_type = Some(Cow::Borrowed("text/xml"));
            let content_encoding = None;
            let content = String::from_utf8_lossy(http_content.as_ref());
            let http_ctxt = xml_nanohttp_method(
                &self.uri,
                Some(self.method),
                Some(content.as_ref()),
                &mut content_type,
                content_encoding,
            )
            .ok();

            if let Some(ctxt) = http_ctxt {
                let return_code = ctxt.return_code();
                if !(200..300).contains(&return_code) {
                    let msg = format!(
                        "xmlIOHTTPCloseWrite: HTTP '{}' of {} {}\n'{}' {} {}\n",
                        self.method,
                        content_lgth,
                        "bytes to URI",
                        self.uri,
                        "failed.  HTTP return code:",
                        return_code
                    );
                    let msg = CString::new(msg).unwrap();
                    unsafe {
                        xml_ioerr(XmlParserErrors::XmlIOWrite, msg.as_ptr() as _);
                    }
                }
            }
        } else {
            let msg = format!(
                "xmlIOHTTPCloseWrite:  {} '{}' {} '{}'.\n",
                "Error retrieving content.\nUnable to", self.method, "data to URI", self.uri
            );
            let msg = CString::new(msg).unwrap();
            unsafe {
                xml_ioerr(XmlParserErrors::XmlIOWrite, msg.as_ptr() as _);
            }
        }
    }
}

/// By default, libxml submits HTTP output requests using the "PUT" method.
/// Calling this method changes the HTTP output method to use the "POST"
/// method instead.
#[doc(alias = "xmlRegisterHTTPPostCallbacks")]
#[cfg(all(feature = "output", feature = "http"))]
pub fn xml_register_http_post_callbacks() {
    /*  Register defaults if not done previously  */

    if !XML_OUTPUT_CALLBACK_INITIALIZED.load(Ordering::Acquire) {
        register_default_output_callbacks();
    }

    register_output_callbacks(DefaultHTTPIOCallbacks {
        write_method: "POST",
    })
    .ok();
}
