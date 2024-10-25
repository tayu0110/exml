//! Provide methods and data structures for handling I/O actions.  
//! This module is based on `libxml/xmlIO.h`, `xmlIO.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

/*
 * Those are the functions and datatypes for the parser input
 * I/O structures.
 */

use std::{
    cell::RefCell,
    ffi::{c_char, c_int, c_uchar, c_uint, c_ulong, c_void, CStr},
    mem::{size_of, zeroed},
    ptr::{addr_of_mut, null, null_mut},
    rc::Rc,
    slice::from_raw_parts,
    str::{from_utf8, from_utf8_mut},
    sync::atomic::{AtomicBool, AtomicUsize, Ordering},
};

use libc::{
    __errno_location, close, fclose, ferror, fflush, fopen, fread, fwrite, getcwd, memset,
    ptrdiff_t, read, size_t, stat, strcmp, strlen, strncpy, write, EACCES, EADDRINUSE,
    EAFNOSUPPORT, EAGAIN, EALREADY, EBADF, EBADMSG, EBUSY, ECANCELED, ECHILD, ECONNREFUSED,
    EDEADLK, EDOM, EEXIST, EFAULT, EFBIG, EINPROGRESS, EINTR, EINVAL, EIO, EISCONN, EISDIR, EMFILE,
    EMLINK, EMSGSIZE, ENAMETOOLONG, ENFILE, ENODEV, ENOENT, ENOEXEC, ENOLCK, ENOMEM, ENOSPC,
    ENOSYS, ENOTDIR, ENOTEMPTY, ENOTSOCK, ENOTSUP, ENOTTY, ENXIO, EOF, EPERM, EPIPE, ERANGE, EROFS,
    ESPIPE, ESRCH, ETIMEDOUT, EXDEV, FILE, INT_MAX,
};

use crate::{
    __xml_raise_error,
    buf::XmlBufRef,
    encoding::{
        find_encoding_handler, floor_char_boundary, get_encoding_handler, xml_encoding_err,
        EncodingError, XmlCharEncoding, XmlCharEncodingHandler,
    },
    error::{XmlErrorDomain, XmlErrorLevel},
    globals::{GenericError, StructuredError, GLOBAL_STATE},
    libxml::{
        globals::xml_mem_strdup,
        parser::{XmlParserInputState, XML_SAX2_MAGIC},
        parser_internals::{xml_free_input_stream, xml_switch_input_encoding},
        xmlstring::{xml_strdup, xml_strstr},
    },
    private::{error::__xml_simple_error, parser::__xml_err_encoding},
    xml_str_printf,
};

use super::{
    catalog::{
        xml_catalog_get_defaults, xml_catalog_local_resolve, xml_catalog_local_resolve_uri,
        xml_catalog_resolve, xml_catalog_resolve_uri, XmlCatalogAllow,
    },
    encoding::XmlCharEncodingOutputFunc,
    globals::{xml_free, xml_malloc},
    nanoftp::{xml_nanoftp_close, xml_nanoftp_open, xml_nanoftp_read},
    nanohttp::{
        xml_nanohttp_close, xml_nanohttp_encoding, xml_nanohttp_method, xml_nanohttp_mime_type,
        xml_nanohttp_open, xml_nanohttp_read, xml_nanohttp_redir, xml_nanohttp_return_code,
    },
    parser::{XmlParserCtxtPtr, XmlParserInputPtr, XmlParserOption},
    parser_internals::xml_new_input_from_file,
    tree::XmlBufferAllocationScheme,
    uri::{xml_canonic_path, xml_free_uri, xml_parse_uri, xml_uri_unescape_string, XmlURIPtr},
    xmlerror::XmlParserErrors,
    xmlstring::{xml_str_equal, xml_strncasecmp, XmlChar},
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
pub type XmlInputMatchCallback = unsafe extern "C" fn(filename: *const c_char) -> c_int;
/**
 * xmlInputOpenCallback:
 * @filename: the filename or URI
 *
 * Callback used in the I/O Input API to open the resource
 *
 * Returns an Input context or NULL in case or error
 */
pub type XmlInputOpenCallback = unsafe extern "C" fn(filename: *const c_char) -> *mut c_void;
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
    unsafe extern "C" fn(context: *mut c_void, buffer: *mut c_char, len: c_int) -> c_int;
/**
 * xmlInputCloseCallback:
 * @context:  an Input context
 *
 * Callback used in the I/O Input API to close the resource
 *
 * Returns 0 or -1 in case of error
 */
pub type XmlInputCloseCallback = unsafe extern "C" fn(context: *mut c_void) -> c_int;

/*
 * Those are the functions and datatypes for the library output
 * I/O structures.
 */

/**
 * xmlOutputMatchCallback:
 * @filename: the filename or URI
 *
 * Callback used in the I/O Output API to detect if the current handler
 * can provide output functionality for this resource.
 *
 * Returns 1 if yes and 0 if another Output module should be used
 */
#[cfg(feature = "output")]
pub type XmlOutputMatchCallback = unsafe extern "C" fn(filename: *const c_char) -> c_int;
/**
 * xmlOutputOpenCallback:
 * @filename: the filename or URI
 *
 * Callback used in the I/O Output API to open the resource
 *
 * Returns an Output context or NULL in case or error
 */
#[cfg(feature = "output")]
pub type XmlOutputOpenCallback = unsafe extern "C" fn(filename: *const c_char) -> *mut c_void;
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
#[cfg(feature = "output")]
pub type XmlOutputWriteCallback =
    unsafe extern "C" fn(context: *mut c_void, buffer: *const c_char, len: c_int) -> c_int;
/**
 * xmlOutputCloseCallback:
 * @context:  an Output context
 *
 * Callback used in the I/O Output API to close the resource
 *
 * Returns 0 or -1 in case of error
 */
#[cfg(feature = "output")]
pub type XmlOutputCloseCallback = unsafe extern "C" fn(context: *mut c_void) -> c_int;

pub type XmlParserInputBufferPtr = *mut XmlParserInputBuffer;
#[repr(C)]
pub struct XmlParserInputBuffer {
    pub(crate) context: *mut c_void,
    pub(crate) readcallback: Option<XmlInputReadCallback>,
    pub(crate) closecallback: Option<XmlInputCloseCallback>,

    pub(crate) encoder: Option<XmlCharEncodingHandler>, /* I18N conversions to UTF-8 */

    pub buffer: Option<XmlBufRef>, /* Local buffer encoded in UTF-8 */
    pub(crate) raw: Option<XmlBufRef>, /* if encoder != NULL buffer for raw input */
    pub(crate) compressed: c_int,  /* -1=unknown, 0=not compressed, 1=compressed */
    pub(crate) error: c_int,
    pub(crate) rawconsumed: c_ulong, /* amount consumed from raw */
}

impl XmlParserInputBuffer {
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
    pub unsafe fn read(&mut self, len: c_int) -> c_int {
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
    pub unsafe fn grow(&mut self, mut len: c_int) -> c_int {
        let mut res: c_int = 0;

        if self.error != 0 {
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
                self.error = XmlParserErrors::XmlErrNoMemory as i32;
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
            let using: size_t = buf.map_or(0, |buf| buf.len());
            let Ok(written) = self.decode(true) else {
                xml_ioerr(XmlParserErrors::XmlIoEncoder, null());
                self.error = XmlParserErrors::XmlIoEncoder as i32;
                return -1;
            };
            res = written as i32;
            let consumed: size_t = using - buf.map_or(0, |buf| buf.len());
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
    pub fn push_bytes(&mut self, buf: &[u8]) -> c_int {
        if self.error != 0 {
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
            let using: size_t = self.raw.map_or(0, |raw| raw.len());
            let Ok(written) = self.decode(true) else {
                unsafe {
                    xml_ioerr(XmlParserErrors::XmlIoEncoder, null());
                }
                self.error = XmlParserErrors::XmlIoEncoder as i32;
                return -1;
            };
            let consumed: size_t = using - self.raw.map_or(0, |raw| raw.len());
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

pub type XmlOutputBufferPtr = *mut XmlOutputBuffer;
#[cfg(feature = "output")]
#[repr(C)]
pub struct XmlOutputBuffer {
    pub(crate) context: *mut c_void,
    pub(crate) writecallback: Option<XmlOutputWriteCallback>,
    pub(crate) closecallback: Option<XmlOutputCloseCallback>,

    pub(crate) encoder: Option<Rc<RefCell<XmlCharEncodingHandler>>>, /* I18N conversions to UTF-8 */

    pub(crate) buffer: Option<XmlBufRef>, /* Local buffer encoded in UTF-8 or ISOLatin */
    pub(crate) conv: Option<XmlBufRef>,   /* if encoder != NULL buffer for output */
    pub(crate) written: c_int,            /* total number of byte written */
    pub(crate) error: c_int,
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
    #[cfg(feature = "output")]
    pub unsafe fn write_bytes(&mut self, buf: &[u8]) -> i32 {
        use crate::encoding::EncodingError;

        let mut ret: c_int; /* return from function call */
        let mut written: c_int = 0; /* number of c_char written to I/O so far */

        if self.error != 0 {
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
                        self.error = XmlParserErrors::XmlIoEncoder as i32;
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
                    self.error = XmlParserErrors::XmlIoWrite as i32;
                    return ret;
                }
                self.written = self.written.saturating_add(ret);
            }
            written += nbchars as i32;
        }

        written
    }
}

pub(crate) const MAX_INPUT_CALLBACK: usize = 15;

/*
 * Input I/O callback sets
 */
#[repr(C)]
#[derive(Clone, Copy)]
pub struct XmlInputCallback {
    matchcallback: Option<XmlInputMatchCallback>,
    opencallback: Option<XmlInputOpenCallback>,
    readcallback: Option<XmlInputReadCallback>,
    closecallback: Option<XmlInputCloseCallback>,
}

static mut XML_INPUT_CALLBACK_TABLE: [XmlInputCallback; MAX_INPUT_CALLBACK] = [XmlInputCallback {
    matchcallback: None,
    opencallback: None,
    readcallback: None,
    closecallback: None,
};
    MAX_INPUT_CALLBACK];
static XML_INPUT_CALLBACK_NR: AtomicUsize = AtomicUsize::new(0);
static XML_INPUT_CALLBACK_INITIALIZED: AtomicBool = AtomicBool::new(false);

/*
 * Interfaces for input
 */
/**
 * xmlCleanupInputCallbacks:
 *
 * clears the entire input callback table. this includes the
 * compiled-in I/O.
 */
pub unsafe extern "C" fn xml_cleanup_input_callbacks() {
    let is_initialized = XML_INPUT_CALLBACK_INITIALIZED.load(Ordering::Acquire);
    if !is_initialized {
        return;
    }

    let num_callbacks = XML_INPUT_CALLBACK_NR.load(Ordering::Acquire);

    for i in (0..num_callbacks).rev() {
        XML_INPUT_CALLBACK_TABLE[i].matchcallback = None;
        XML_INPUT_CALLBACK_TABLE[i].opencallback = None;
        XML_INPUT_CALLBACK_TABLE[i].readcallback = None;
        XML_INPUT_CALLBACK_TABLE[i].closecallback = None;
    }

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
pub unsafe extern "C" fn xml_pop_input_callbacks() -> c_int {
    let is_initialized = XML_INPUT_CALLBACK_INITIALIZED.load(Ordering::Acquire);
    if !is_initialized {
        return -1;
    }

    let mut num_callbacks = XML_INPUT_CALLBACK_NR.load(Ordering::Acquire);
    if num_callbacks == 0 {
        return -1;
    }

    num_callbacks -= 1;
    XML_INPUT_CALLBACK_TABLE[num_callbacks].matchcallback = None;
    XML_INPUT_CALLBACK_TABLE[num_callbacks].opencallback = None;
    XML_INPUT_CALLBACK_TABLE[num_callbacks].readcallback = None;
    XML_INPUT_CALLBACK_TABLE[num_callbacks].closecallback = None;

    XML_INPUT_CALLBACK_NR.store(num_callbacks, Ordering::Release);
    num_callbacks as _
}

/**
 * xmlRegisterDefaultInputCallbacks:
 *
 * Registers the default compiled-in I/O handlers.
 */
pub unsafe extern "C" fn xml_register_default_input_callbacks() {
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
 * xmlIOErrMemory:
 * @extra:  extra information
 *
 * Handle an out of memory condition
 */
pub(crate) unsafe extern "C" fn xml_ioerr_memory(extra: *const c_char) {
    __xml_simple_error(
        XmlErrorDomain::XmlFromIO,
        XmlParserErrors::XmlErrNoMemory,
        null_mut(),
        null(),
        extra as _,
    );
}

/**
 * xmlAllocParserInputBuffer:
 * @enc:  the charset encoding if known
 *
 * Create a buffered parser input for progressive parsing
 *
 * Returns the new parser input or NULL
 */
pub unsafe fn xml_alloc_parser_input_buffer(enc: XmlCharEncoding) -> XmlParserInputBufferPtr {
    let ret: XmlParserInputBufferPtr =
        xml_malloc(size_of::<XmlParserInputBuffer>()) as XmlParserInputBufferPtr;
    if ret.is_null() {
        xml_ioerr_memory(c"creating input buffer".as_ptr() as _);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlParserInputBuffer>());
    let default_buffer_size = GLOBAL_STATE.with_borrow(|state| state.default_buffer_size);
    let Some(mut new_buf) = XmlBufRef::with_capacity(2 * default_buffer_size) else {
        xml_free(ret as _);
        return null_mut();
    };
    (*ret).buffer = Some(new_buf);
    new_buf.set_allocation_scheme(XmlBufferAllocationScheme::XmlBufferAllocDoubleit);
    (*ret).encoder = get_encoding_handler(enc);
    (*ret).raw = if (*ret).encoder.is_some() {
        XmlBufRef::with_capacity(2 * default_buffer_size)
    } else {
        None
    };
    (*ret).readcallback = None;
    (*ret).closecallback = None;
    (*ret).context = null_mut();
    (*ret).compressed = -1;
    (*ret).rawconsumed = 0;

    ret
}

/**
 * xmlParserInputBufferCreateFilename:
 * @URI:  a C string containing the URI or filename
 * @enc:  the charset encoding if known
 *
 * Create a buffered parser input for the progressive parsing of a file
 * If filename is "-' then we use stdin as the input.
 * Automatic support for ZLIB/Compress compressed document is provided
 * by default if found at compile-time.
 * Do an encoding check if enc == XML_CHAR_ENCODING_NONE
 *
 * Returns the new parser input or NULL
 */
pub unsafe fn xml_parser_input_buffer_create_filename(
    uri: *const c_char,
    enc: XmlCharEncoding,
) -> XmlParserInputBufferPtr {
    if let Some(f) =
        GLOBAL_STATE.with_borrow(|state| state.parser_input_buffer_create_filename_value)
    {
        let uri = CStr::from_ptr(uri).to_str().unwrap();
        return f(uri, enc);
    }
    // if let Some(f) = __xml_parser_input_buffer_create_filename_value() {
    //     return f(uri, enc);
    // }
    __xml_parser_input_buffer_create_filename(uri, enc)
}

const IOERR: &[*const c_char] = &[
    c"Unknown IO error".as_ptr() as _,                    /* UNKNOWN */
    c"Permission denied".as_ptr() as _,                   /* EACCES */
    c"Resource temporarily unavailable".as_ptr() as _,    /* EAGAIN */
    c"Bad file descriptor".as_ptr() as _,                 /* EBADF */
    c"Bad message".as_ptr() as _,                         /* EBADMSG */
    c"Resource busy".as_ptr() as _,                       /* EBUSY */
    c"Operation canceled".as_ptr() as _,                  /* ECANCELED */
    c"No child processes".as_ptr() as _,                  /* ECHILD */
    c"Resource deadlock avoided".as_ptr() as _,           /* EDEADLK */
    c"Domain error".as_ptr() as _,                        /* EDOM */
    c"File exists".as_ptr() as _,                         /* EEXIST */
    c"Bad address".as_ptr() as _,                         /* EFAULT */
    c"File too large".as_ptr() as _,                      /* EFBIG */
    c"Operation in progress".as_ptr() as _,               /* EINPROGRESS */
    c"Interrupted function call".as_ptr() as _,           /* EINTR */
    c"Invalid argument".as_ptr() as _,                    /* EINVAL */
    c"Input/output error".as_ptr() as _,                  /* EIO */
    c"Is a directory".as_ptr() as _,                      /* EISDIR */
    c"Too many open files".as_ptr() as _,                 /* EMFILE */
    c"Too many links".as_ptr() as _,                      /* EMLINK */
    c"Inappropriate message buffer length".as_ptr() as _, /* EMSGSIZE */
    c"Filename too long".as_ptr() as _,                   /* ENAMETOOLONG */
    c"Too many open files in system".as_ptr() as _,       /* ENFILE */
    c"No such device".as_ptr() as _,                      /* ENODEV */
    c"No such file or directory".as_ptr() as _,           /* ENOENT */
    c"Exec format error".as_ptr() as _,                   /* ENOEXEC */
    c"No locks available".as_ptr() as _,                  /* ENOLCK */
    c"Not enough space".as_ptr() as _,                    /* ENOMEM */
    c"No space left on device".as_ptr() as _,             /* ENOSPC */
    c"Function not implemented".as_ptr() as _,            /* ENOSYS */
    c"Not a directory".as_ptr() as _,                     /* ENOTDIR */
    c"Directory not empty".as_ptr() as _,                 /* ENOTEMPTY */
    c"Not supported".as_ptr() as _,                       /* ENOTSUP */
    c"Inappropriate I/O control operation".as_ptr() as _, /* ENOTTY */
    c"No such device or address".as_ptr() as _,           /* ENXIO */
    c"Operation not permitted".as_ptr() as _,             /* EPERM */
    c"Broken pipe".as_ptr() as _,                         /* EPIPE */
    c"Result too large".as_ptr() as _,                    /* ERANGE */
    c"Read-only file system".as_ptr() as _,               /* EROFS */
    c"Invalid seek".as_ptr() as _,                        /* ESPIPE */
    c"No such process".as_ptr() as _,                     /* ESRCH */
    c"Operation timed out".as_ptr() as _,                 /* ETIMEDOUT */
    c"Improper link".as_ptr() as _,                       /* EXDEV */
    c"Attempt to load network entity %s".as_ptr() as _,   /* XML_IO_NETWORK_ATTEMPT */
    c"encoder error".as_ptr() as _,                       /* XML_IO_ENCODER */
    c"flush error".as_ptr() as _,
    c"write error".as_ptr() as _,
    c"no input".as_ptr() as _,
    c"buffer full".as_ptr() as _,
    c"loading error".as_ptr() as _,
    c"not a socket".as_ptr() as _,           /* ENOTSOCK */
    c"already connected".as_ptr() as _,      /* EISCONN */
    c"connection refused".as_ptr() as _,     /* ECONNREFUSED */
    c"unreachable network".as_ptr() as _,    /* ENETUNREACH */
    c"address in use".as_ptr() as _,         /* EADDRINUSE */
    c"already in use".as_ptr() as _,         /* EALREADY */
    c"unknown address family".as_ptr() as _, /* EAFNOSUPPORT */
];

/**
 * __xmlIOErr:
 * @code:  the error number
 * @
 * @extra:  extra information
 *
 * Handle an I/O error
 */
pub(crate) unsafe fn __xml_ioerr(
    domain: XmlErrorDomain,
    mut code: XmlParserErrors,
    extra: *const c_char,
) {
    let mut idx: c_uint;
    let errno = *__errno_location();

    if code == XmlParserErrors::XmlErrOK {
        if errno == 0 {
            code = XmlParserErrors::XmlErrOK;
        } else if errno == EACCES {
            code = XmlParserErrors::XmlIoEacces;
        } else if errno == EAGAIN {
            code = XmlParserErrors::XmlIoEagain;
        } else if errno == EBADF {
            code = XmlParserErrors::XmlIoEbadf;
        } else if errno == EBADMSG {
            code = XmlParserErrors::XmlIoEbadmsg;
        } else if errno == EBUSY {
            code = XmlParserErrors::XmlIoEbusy;
        } else if errno == ECANCELED {
            code = XmlParserErrors::XmlIoEcanceled;
        } else if errno == ECHILD {
            code = XmlParserErrors::XmlIoEchild;
        } else if errno == EDEADLK {
            code = XmlParserErrors::XmlIoEdeadlk;
        } else if errno == EDOM {
            code = XmlParserErrors::XmlIoEdom;
        } else if errno == EEXIST {
            code = XmlParserErrors::XmlIoEexist;
        } else if errno == EFAULT {
            code = XmlParserErrors::XmlIoEfault;
        } else if errno == EFBIG {
            code = XmlParserErrors::XmlIoEfbig;
        } else if errno == EINPROGRESS {
            code = XmlParserErrors::XmlIoEinprogress;
        } else if errno == EINTR {
            code = XmlParserErrors::XmlIoEintr;
        } else if errno == EINVAL {
            code = XmlParserErrors::XmlIoEinval;
        } else if errno == EIO {
            code = XmlParserErrors::XmlIoEio;
        } else if errno == EISDIR {
            code = XmlParserErrors::XmlIoEisdir;
        } else if errno == EMFILE {
            code = XmlParserErrors::XmlIoEmfile;
        } else if errno == EMLINK {
            code = XmlParserErrors::XmlIoEmlink;
        } else if errno == EMSGSIZE {
            code = XmlParserErrors::XmlIoEmsgsize;
        } else if errno == ENAMETOOLONG {
            code = XmlParserErrors::XmlIoEnametoolong;
        } else if errno == ENFILE {
            code = XmlParserErrors::XmlIoEnfile;
        } else if errno == ENODEV {
            code = XmlParserErrors::XmlIoEnodev;
        } else if errno == ENOENT {
            code = XmlParserErrors::XmlIoEnoent;
        } else if errno == ENOEXEC {
            code = XmlParserErrors::XmlIoEnoexec;
        } else if errno == ENOLCK {
            code = XmlParserErrors::XmlIoEnolck;
        } else if errno == ENOMEM {
            code = XmlParserErrors::XmlIoEnomem;
        } else if errno == ENOSPC {
            code = XmlParserErrors::XmlIoEnospc;
        } else if errno == ENOSYS {
            code = XmlParserErrors::XmlIoEnosys;
        } else if errno == ENOTDIR {
            code = XmlParserErrors::XmlIoEnotdir;
        } else if errno == ENOTEMPTY {
            code = XmlParserErrors::XmlIoEnotempty;
        } else if errno == ENOTSUP {
            code = XmlParserErrors::XmlIoEnotsup;
        } else if errno == ENOTTY {
            code = XmlParserErrors::XmlIoEnotty;
        } else if errno == ENXIO {
            code = XmlParserErrors::XmlIoEnxio;
        } else if errno == EPERM {
            code = XmlParserErrors::XmlIoEperm;
        } else if errno == EPIPE {
            code = XmlParserErrors::XmlIoEpipe;
        } else if errno == ERANGE {
            code = XmlParserErrors::XmlIoErange;
        } else if errno == EROFS {
            code = XmlParserErrors::XmlIoErofs;
        } else if errno == ESPIPE {
            code = XmlParserErrors::XmlIoEspipe;
        } else if errno == ESRCH {
            code = XmlParserErrors::XmlIoEsrch;
        } else if errno == ETIMEDOUT {
            code = XmlParserErrors::XmlIoEtimedout;
        } else if errno == EXDEV {
            code = XmlParserErrors::XmlIoExdev;
        } else if errno == ENOTSOCK {
            code = XmlParserErrors::XmlIoEnotsock;
        } else if errno == EISCONN {
            code = XmlParserErrors::XmlIoEisconn;
        } else if errno == ECONNREFUSED {
            code = XmlParserErrors::XmlIoEconnrefused;
        } else if errno == EADDRINUSE {
            code = XmlParserErrors::XmlIoEaddrinuse;
        } else if errno == EALREADY {
            code = XmlParserErrors::XmlIoEalready;
        } else if errno == EAFNOSUPPORT {
            code = XmlParserErrors::XmlIoEafnosupport;
        } else {
            code = XmlParserErrors::XmlIoUnknown;
        }
    }
    idx = 0;
    if code as i32 >= XmlParserErrors::XmlIoUnknown as i32 {
        idx = code as u32 - XmlParserErrors::XmlIoUnknown as u32;
    }
    if idx >= IOERR.len() as u32 {
        idx = 0;
    }

    __xml_simple_error(domain, code, null_mut(), IOERR[idx as usize], extra);
}

/**
 * xmlIOErr:
 * @code:  the error number
 * @extra:  extra information
 *
 * Handle an I/O error
 */
unsafe extern "C" fn xml_ioerr(code: XmlParserErrors, extra: *const c_char) {
    __xml_ioerr(XmlErrorDomain::XmlFromIO, code, extra);
}

/**
 * xmlFileFlush:
 * @context:  the I/O context
 *
 * Flush an I/O channel
 */
unsafe extern "C" fn xml_file_flush(context: *mut c_void) -> c_int {
    if context.is_null() {
        return -1;
    }
    let ret: c_int = if fflush(context as *mut FILE) == EOF {
        -1
    } else {
        0
    };
    if ret < 0 {
        xml_ioerr(XmlParserErrors::XmlErrOK, c"fflush()".as_ptr() as _);
    }
    ret
}

/**
 * xmlParserInputBufferCreateFile:
 * @file:  a FILE*
 * @enc:  the charset encoding if known
 *
 * Create a buffered parser input for the progressive parsing of a FILE *
 * buffered C I/O
 *
 * Returns the new parser input or NULL
 */
pub unsafe fn xml_parser_input_buffer_create_file(
    file: *mut FILE,
    enc: XmlCharEncoding,
) -> XmlParserInputBufferPtr {
    if !XML_INPUT_CALLBACK_INITIALIZED.load(Ordering::Relaxed) {
        xml_register_default_input_callbacks();
    }

    if file.is_null() {
        return null_mut();
    }

    let ret: XmlParserInputBufferPtr = xml_alloc_parser_input_buffer(enc);
    if !ret.is_null() {
        (*ret).context = file as _;
        (*ret).readcallback = Some(xml_file_read);
        (*ret).closecallback = Some(xml_file_flush);
    }

    ret
}

/**
 * xmlFdRead:
 * @context:  the I/O context
 * @buffer:  where to drop data
 * @len:  number of bytes to read
 *
 * Read @len bytes to @buffer from the I/O channel.
 *
 * Returns the number of bytes written
 */
unsafe extern "C" fn xml_fd_read(context: *mut c_void, buffer: *mut c_char, len: c_int) -> c_int {
    let ret: c_int = read(context as ptrdiff_t as c_int, buffer.add(0) as _, len as _) as _;
    if ret < 0 {
        xml_ioerr(XmlParserErrors::XmlErrOK, c"read()".as_ptr() as _);
    }
    ret
}

/**
 * xmlFdClose:
 * @context:  the I/O context
 *
 * Close an I/O channel
 *
 * Returns 0 in case of success and error code otherwise
 */
unsafe extern "C" fn xml_fd_close(context: *mut c_void) -> c_int {
    let ret: c_int = close(context as ptrdiff_t as c_int);
    if ret < 0 {
        xml_ioerr(XmlParserErrors::XmlErrOK, c"close()".as_ptr() as _);
    }
    ret
}

/**
 * xmlParserInputBufferCreateFd:
 * @fd:  a file descriptor number
 * @enc:  the charset encoding if known
 *
 * Create a buffered parser input for the progressive parsing for the input
 * from a file descriptor
 *
 * Returns the new parser input or NULL
 */
pub unsafe fn xml_parser_input_buffer_create_fd(
    fd: c_int,
    enc: XmlCharEncoding,
) -> XmlParserInputBufferPtr {
    if fd < 0 {
        return null_mut();
    }

    let ret: XmlParserInputBufferPtr = xml_alloc_parser_input_buffer(enc);
    if !ret.is_null() {
        (*ret).context = fd as ptrdiff_t as *mut c_void;
        (*ret).readcallback = Some(xml_fd_read);
        (*ret).closecallback = Some(xml_fd_close);
    }

    ret
}

/**
 * xmlParserInputBufferCreateMem:
 * @mem:  the memory input
 * @size:  the length of the memory block
 * @enc:  the charset encoding if known
 *
 * Create a buffered parser input for the progressive parsing for the input
 * from a memory area.
 *
 * Returns the new parser input or NULL
 */
pub unsafe fn xml_parser_input_buffer_create_mem(
    mem: *const c_char,
    size: c_int,
    enc: XmlCharEncoding,
) -> XmlParserInputBufferPtr {
    if size < 0 {
        return null_mut();
    }
    if mem.is_null() {
        return null_mut();
    }

    let ret: XmlParserInputBufferPtr = xml_alloc_parser_input_buffer(enc);
    if !ret.is_null() {
        (*ret).context = mem as _;
        (*ret).readcallback = None;
        (*ret).closecallback = None;
        if (*ret)
            .buffer
            .expect("Internal Error")
            .push_bytes(from_raw_parts(mem as *const u8, size as usize))
            .is_err()
        {
            xml_free_parser_input_buffer(ret);
            return null_mut();
        }
    }

    ret
}

/**
 * xmlParserInputBufferCreateStatic:
 * @mem:  the memory input
 * @size:  the length of the memory block
 * @enc:  the charset encoding if known
 *
 * DEPRECATED: Use xmlParserInputBufferCreateMem.
 *
 * Returns the new parser input or NULL
 */
#[deprecated]
pub unsafe fn xml_parser_input_buffer_create_static(
    mem: *const c_char,
    size: c_int,
    enc: XmlCharEncoding,
) -> XmlParserInputBufferPtr {
    xml_parser_input_buffer_create_mem(mem, size, enc)
}

/**
 * xmlParserInputBufferCreateIO:
 * @ioread:  an I/O read function
 * @ioclose:  an I/O close function
 * @ioctx:  an I/O handler
 * @enc:  the charset encoding if known
 *
 * Create a buffered parser input for the progressive parsing for the input
 * from an I/O handler
 *
 * Returns the new parser input or NULL
 */
pub unsafe fn xml_parser_input_buffer_create_io(
    ioread: Option<XmlInputReadCallback>,
    ioclose: Option<XmlInputCloseCallback>,
    ioctx: *mut c_void,
    enc: XmlCharEncoding,
) -> XmlParserInputBufferPtr {
    if ioread.is_none() {
        return null_mut();
    }

    let ret: XmlParserInputBufferPtr = xml_alloc_parser_input_buffer(enc);
    if !ret.is_null() {
        (*ret).context = ioctx as _;
        (*ret).readcallback = ioread;
        (*ret).closecallback = ioclose;
    }

    ret
}

const MINLEN: usize = 4000;

/**
 * endOfInput:
 *
 * When reading from an Input channel indicated end of file or error
 * don't reread from it again.
 */
unsafe extern "C" fn end_of_input(
    _context: *mut c_void,
    _bufferr: *mut c_char,
    _len: c_int,
) -> c_int {
    0
}

/**
 * xmlFreeParserInputBuffer:
 * @in:  a buffered parser input
 *
 * Free up the memory used by a buffered parser input
 */
pub unsafe extern "C" fn xml_free_parser_input_buffer(input: XmlParserInputBufferPtr) {
    if input.is_null() {
        return;
    }

    if let Some(raw) = (*input).raw.take() {
        raw.free();
    }
    let _ = (*input).encoder.take();
    if let Some(callback) = (*input).closecallback {
        callback((*input).context);
    }
    if let Some(buffer) = (*input).buffer.take() {
        buffer.free();
    }

    xml_free(input as _);
}

/**
 * xmlParserGetDirectory:
 * @filename:  the path to a file
 *
 * lookup the directory for that file
 *
 * Returns a new allocated string containing the directory, or NULL.
 */
pub unsafe extern "C" fn xml_parser_get_directory(filename: *const c_char) -> *mut c_char {
    let mut ret: *mut c_char = null_mut();
    let mut dir: [c_char; 1024] = [0; 1024];
    let mut cur: *mut c_char;

    if !XML_INPUT_CALLBACK_INITIALIZED.load(Ordering::Relaxed) {
        xml_register_default_input_callbacks();
    }

    if filename.is_null() {
        return null_mut();
    }

    #[cfg(target_os = "windows")]
    fn is_xmlpgd_sep(ch: u8) -> bool {
        ch == b'/' || ch == b'\\'
    }
    #[cfg(not(target_os = "windows"))]
    fn is_xmlpgd_sep(ch: u8) -> bool {
        ch == b'/'
    }

    strncpy(dir.as_mut_ptr() as _, filename as _, 1023);
    dir[1023] = 0;
    cur = dir.as_mut_ptr().add(strlen(dir.as_ptr() as _));
    while cur > dir.as_ptr() as _ {
        if is_xmlpgd_sep(*cur as u8) {
            break;
        }
        cur = cur.sub(1);
    }
    if is_xmlpgd_sep(*cur as u8) {
        if cur == dir.as_ptr() as _ {
            dir[1] = 0;
        } else {
            *cur = 0;
        }
        ret = xml_mem_strdup(dir.as_ptr() as _) as _;
    } else if !getcwd(dir.as_ptr() as _, 1024).is_null() {
        dir[1023] = 0;
        ret = xml_mem_strdup(dir.as_ptr() as _) as _;
    }
    ret
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
pub unsafe extern "C" fn xml_register_input_callbacks(
    match_func: Option<XmlInputMatchCallback>,
    open_func: Option<XmlInputOpenCallback>,
    read_func: Option<XmlInputReadCallback>,
    close_func: Option<XmlInputCloseCallback>,
) -> c_int {
    let num_callbacks = XML_INPUT_CALLBACK_NR.load(Ordering::Acquire);
    if num_callbacks >= MAX_INPUT_CALLBACK {
        return -1;
    }
    XML_INPUT_CALLBACK_TABLE[num_callbacks].matchcallback = match_func;
    XML_INPUT_CALLBACK_TABLE[num_callbacks].opencallback = open_func;
    XML_INPUT_CALLBACK_TABLE[num_callbacks].readcallback = read_func;
    XML_INPUT_CALLBACK_TABLE[num_callbacks].closecallback = close_func;
    XML_INPUT_CALLBACK_INITIALIZED.store(true, Ordering::Relaxed);
    XML_INPUT_CALLBACK_NR.store(num_callbacks + 1, Ordering::Release);
    num_callbacks as _
}

pub(crate) unsafe fn __xml_parser_input_buffer_create_filename(
    uri: *const c_char,
    enc: XmlCharEncoding,
) -> XmlParserInputBufferPtr {
    let ret: XmlParserInputBufferPtr;
    let mut context: *mut c_void = null_mut();

    if !XML_INPUT_CALLBACK_INITIALIZED.load(Ordering::Acquire) {
        xml_register_default_input_callbacks();
    }

    if uri.is_null() {
        return null_mut();
    }

    let num_callbacks = XML_INPUT_CALLBACK_NR.load(Ordering::Acquire);
    /*
     * Try to find one of the input accept method accepting that scheme
     * Go in reverse to give precedence to user defined handlers.
     */
    if context.is_null() {
        for i in (0..num_callbacks).rev() {
            if XML_INPUT_CALLBACK_TABLE[i]
                .matchcallback
                .filter(|callback| callback(uri) != 0)
                .is_some()
            {
                context = (XML_INPUT_CALLBACK_TABLE[i].opencallback.unwrap())(uri);
                if !context.is_null() {
                    /*
                     * Allocate the Input buffer front-end.
                     */
                    ret = xml_alloc_parser_input_buffer(enc);
                    if !ret.is_null() {
                        (*ret).context = context;
                        (*ret).readcallback = XML_INPUT_CALLBACK_TABLE[i].readcallback;
                        (*ret).closecallback = XML_INPUT_CALLBACK_TABLE[i].closecallback;
                    // #ifdef LIBXML_ZLIB_ENABLED
                    // 	if ((xmlInputCallbackTable[i].opencallback == xmlGzfileOpen) &&
                    // 		strcmp(URI, "-") != 0) {
                    // // #if defined(ZLIB_VERNUM) && ZLIB_VERNUM >= 0x1230
                    // //             (*ret).compressed = !gzdirect(context);
                    // // #else
                    // // 	    if ((*(context as *mut z_stream)).avail_in > 4) {
                    // // 	        c_char *cptr, buff4[4];
                    // // 		cptr = (c_char *) (*(context as *mut z_stream)).next_in;
                    // // 		if (gzread(context, buff4, 4) == 4) {
                    // // 		    if (strncmp(buff4, cptr, 4) == 0)
                    // // 		        (*ret).compressed = 0;
                    // // 		    else
                    // // 		        (*ret).compressed = 1;
                    // // 		    gzrewind(context);
                    // // 		}
                    // // 	    }
                    // // #endif
                    // 	}
                    // #endif
                    // #ifdef LIBXML_LZMA_ENABLED
                    // 	if ((xmlInputCallbackTable[i].opencallback == xmlXzfileOpen) &&
                    // 		strcmp(URI, "-") != 0) {
                    //             (*ret).compressed = __libxml2_xzcompressed(context);
                    // 	}
                    // #endif
                    } else {
                        (XML_INPUT_CALLBACK_TABLE[i].closecallback.unwrap())(context);
                    }

                    return ret;
                }
            }
        }
    }
    null_mut()
}

/*
 * Interfaces for output
 */

/*
 * Output I/O callback sets
 */
#[cfg(feature = "output")]
#[repr(C)]
#[derive(Clone, Copy)]
pub(crate) struct XmlOutputCallback {
    matchcallback: Option<XmlOutputMatchCallback>,
    opencallback: Option<XmlOutputOpenCallback>,
    writecallback: Option<XmlOutputWriteCallback>,
    closecallback: Option<XmlOutputCloseCallback>,
}

#[cfg(feature = "output")]
const MAX_OUTPUT_CALLBACK: usize = 15;

#[cfg(feature = "output")]
static mut XML_OUTPUT_CALLBACK_TABLE: [XmlOutputCallback; MAX_OUTPUT_CALLBACK] =
    [XmlOutputCallback {
        matchcallback: None,
        opencallback: None,
        writecallback: None,
        closecallback: None,
    }; MAX_OUTPUT_CALLBACK];
#[cfg(feature = "output")]
static XML_OUTPUT_CALLBACK_NR: AtomicUsize = AtomicUsize::new(0);
#[cfg(feature = "output")]
static XML_OUTPUT_CALLBACK_INITIALIZED: AtomicBool = AtomicBool::new(false);

/**
 * xmlCleanupOutputCallbacks:
 *
 * clears the entire output callback table. this includes the
 * compiled-in I/O callbacks.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_cleanup_output_callbacks() {
    let is_initialized = XML_OUTPUT_CALLBACK_INITIALIZED.load(Ordering::Acquire);
    if !is_initialized {
        return;
    }

    let num_callbacks = XML_OUTPUT_CALLBACK_NR.load(Ordering::Acquire);
    for i in (0..num_callbacks).rev() {
        XML_OUTPUT_CALLBACK_TABLE[i].matchcallback = None;
        XML_OUTPUT_CALLBACK_TABLE[i].opencallback = None;
        XML_OUTPUT_CALLBACK_TABLE[i].writecallback = None;
        XML_OUTPUT_CALLBACK_TABLE[i].closecallback = None;
    }

    XML_OUTPUT_CALLBACK_NR.store(0, Ordering::Release);
    XML_OUTPUT_CALLBACK_INITIALIZED.store(false, Ordering::Release);
}

/**
 * xmlPopOutputCallbacks:
 *
 * Remove the top output callbacks from the output stack. This includes the
 * compiled-in I/O.
 *
 * Returns the number of output callback registered or -1 in case of error.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_pop_output_callbacks() -> c_int {
    if !XML_OUTPUT_CALLBACK_INITIALIZED.load(Ordering::Acquire) {
        return -1;
    }

    let mut num_callbacks = XML_OUTPUT_CALLBACK_NR.load(Ordering::Acquire);
    if num_callbacks == 0 {
        return -1;
    }

    num_callbacks -= 1;
    XML_OUTPUT_CALLBACK_TABLE[num_callbacks].matchcallback = None;
    XML_OUTPUT_CALLBACK_TABLE[num_callbacks].opencallback = None;
    XML_OUTPUT_CALLBACK_TABLE[num_callbacks].writecallback = None;
    XML_OUTPUT_CALLBACK_TABLE[num_callbacks].closecallback = None;

    XML_OUTPUT_CALLBACK_NR.store(num_callbacks, Ordering::Release);
    num_callbacks as _
}

/**
 * xmlFileOpenW:
 * @filename:  the URI for matching
 *
 * output to from FILE *,
 * if @filename is "-" then the standard output is used
 *
 * Returns an I/O context or NULL in case of error
 */
#[cfg(feature = "output")]
unsafe extern "C" fn xml_file_open_w(filename: *const c_char) -> *mut c_void {
    let path: *const c_char;
    let fd: *mut FILE;

    if strcmp(filename, c"-".as_ptr() as _) == 0 {
        extern "C" {
            // Does it work ?????
            static stdout: *mut FILE;
        }
        fd = stdout;
        return fd as _;
    }

    if xml_strncasecmp(filename as _, c"file://localhost/".as_ptr() as _, 17) == 0 {
        #[cfg(target_os = "windows")]
        {
            path = filename.add(17);
        }
        #[cfg(not(target_os = "windows"))]
        {
            path = filename.add(16);
        }
    } else if xml_strncasecmp(filename as _, c"file:///".as_ptr() as _, 8) == 0 {
        #[cfg(target_os = "windows")]
        {
            path = filename.add(8);
        }
        #[cfg(not(target_os = "windows"))]
        {
            path = filename.add(7);
        }
    } else {
        path = filename;
    }
    if path.is_null() {
        return null_mut();
    }

    #[cfg(target_os = "windows")]
    {
        fd = xmlWrapOpenUtf8(path, 1);
    }
    #[cfg(not(target_os = "windows"))]
    {
        fd = fopen(path, c"wb".as_ptr() as _);
    }

    if fd.is_null() {
        xml_ioerr(XmlParserErrors::XmlErrOK, path);
    }
    fd as _
}

/**
 * xmlFileWrite:
 * @context:  the I/O context
 * @buffer:  where to drop data
 * @len:  number of bytes to write
 *
 * Write @len bytes from @buffer to the I/O channel.
 *
 * Returns the number of bytes written
 */
#[cfg(feature = "output")]
unsafe extern "C" fn xml_file_write(
    context: *mut c_void,
    buffer: *const c_char,
    len: c_int,
) -> c_int {
    if context.is_null() || buffer.is_null() {
        return -1;
    }
    let items: c_int = fwrite(buffer.add(0) as _, len as usize, 1, context as _) as _;
    if items == 0 && ferror(context as _) != 0 {
        xml_ioerr(XmlParserErrors::XmlErrOK, c"fwrite()".as_ptr() as _);
        return -1;
    }
    items * len
}

/**
 * xmlIOHTTPDfltOpenW
 * @post_uri:  The destination URI for this document.
 *
 * Calls xmlIOHTTPOpenW with no compression to set up for a subsequent
 * HTTP post command.  This function should generally not be used as
 * the open callback is short circuited in xmlOutputBufferCreateFile.
 *
 * Returns a poc_inter to the new IO context.
 */
#[cfg(all(feature = "output", feature = "http"))]
unsafe extern "C" fn xml_io_http_dflt_open_w(post_uri: *const c_char) -> *mut c_void {
    xml_io_http_open_w(post_uri, 0)
}

#[cfg(all(feature = "output", feature = "http"))]
pub type XmlIOHTTPWriteCtxtPtr = *mut XmlIOHTTPWriteCtxt;
#[cfg(all(feature = "output", feature = "http"))]
#[repr(C)]
pub struct XmlIOHTTPWriteCtxt {
    compression: c_int,
    uri: *mut c_char,
    doc_buff: *mut c_void,
}

/**
 * xmlIOHTTPWrite
 * @context:  previously opened writing context
 * @buffer:   data to output to temporary buffer
 * @len:      bytes to output
 *
 * Collect data from memory buffer c_into a temporary file for later
 * processing.
 *
 * Returns number of bytes written.
 */
#[cfg(all(feature = "output", feature = "http"))]
unsafe extern "C" fn xml_io_http_write(
    context: *mut c_void,
    buffer: *const c_char,
    mut len: c_int,
) -> c_int {
    let ctxt: XmlIOHTTPWriteCtxtPtr = context as _;

    if ctxt.is_null() || (*ctxt).doc_buff.is_null() || buffer.is_null() {
        return -1;
    }

    if len > 0 {
        /*  Use gzwrite or fwrite as previously setup in the open call  */

        //  #ifdef LIBXML_ZLIB_ENABLED
        //      if ( (*ctxt).compression > 0 )
        //          len = xmlZMemBuffAppend( (*ctxt).doc_buff, buffer, len );

        //      else
        //  #endif
        len = (*((*ctxt).doc_buff as *mut XmlOutputBuffer))
            .write_bytes(from_raw_parts(buffer as *const u8, len as usize));

        if len < 0 {
            let mut msg: [XmlChar; 500] = [0; 500];
            xml_str_printf!(
                msg.as_mut_ptr(),
                500,
                c"xmlIOHTTPWrite:  %s\n%s '%s'.\n".as_ptr(),
                c"Error appending to c_internal buffer.".as_ptr(),
                c"Error sending document to URI".as_ptr(),
                (*ctxt).uri
            );
            xml_ioerr(XmlParserErrors::XmlIoWrite, msg.as_ptr() as _);
        }
    }

    len
}

/**
 * xmlFreeHTTPWriteCtxt
 * @ctxt:  Context to cleanup
 *
 * Free allocated memory and reclaim system resources.
 *
 * No return value.
 */
#[cfg(all(feature = "output", feature = "http"))]
unsafe extern "C" fn xml_free_http_write_ctxt(ctxt: XmlIOHTTPWriteCtxtPtr) {
    if !(*ctxt).uri.is_null() {
        xml_free((*ctxt).uri as _);
    }

    if !(*ctxt).doc_buff.is_null() {
        // #ifdef LIBXML_ZLIB_ENABLED
        // 	if ( (*ctxt).compression > 0 ) {
        // 	    xmlFreeZMemBuff( (*ctxt).doc_buff );
        // 	}
        // 	else
        // #endif
        {
            xml_output_buffer_close((*ctxt).doc_buff as _);
        }
    }

    xml_free(ctxt as _);
}

/**
 * xmlIOHTTCloseWrite
 * @context:  The I/O context
 * @http_mthd: The HTTP method to be used when sending the data
 *
 * Close the transmit HTTP I/O channel and actually send the data.
 */
#[cfg(all(feature = "output", feature = "http"))]
unsafe extern "C" fn xml_io_http_close_write(
    context: *mut c_void,
    http_mthd: *const c_char,
) -> c_int {
    let mut close_rc: c_int = -1;
    let http_rtn: c_int;
    let content_lgth: c_int;
    let ctxt: XmlIOHTTPWriteCtxtPtr = context as _;

    let http_content: *mut c_char;
    let content_encoding: *mut c_char = null_mut();
    let mut content_type: *mut c_char = c"text/xml".as_ptr() as _;
    let http_ctxt: *mut c_void;

    if ctxt.is_null() || http_mthd.is_null() {
        return -1;
    }

    /*  Retrieve the content from the appropriate buffer  */

    // #ifdef LIBXML_ZLIB_ENABLED

    //     if ( (*ctxt).compression > 0 ) {
    // 	content_lgth = xmlZMemBuffGetContent( (*ctxt).doc_buff, &http_content );
    // 	content_encoding = (c_char *) "Content-Encoding: gzip";
    //     }
    //     else
    // #endif
    {
        /*  Pull the data out of the memory output buffer  */

        let dctxt: XmlOutputBufferPtr = (*ctxt).doc_buff as _;
        http_content = (*dctxt).buffer.map_or(null_mut(), |buf| {
            if buf.is_ok() {
                buf.as_ref().as_ptr() as *mut i8
            } else {
                null_mut()
            }
        });
        content_lgth = (*dctxt).buffer.map_or(0, |buf| buf.len()) as i32;
    }

    if http_content.is_null() {
        let mut msg: [XmlChar; 500] = [0; 500];
        xml_str_printf!(
            msg.as_mut_ptr(),
            500,
            c"xmlIOHTTPCloseWrite:  %s '%s' %s '%s'.\n".as_ptr(),
            c"Error retrieving content.\nUnable to".as_ptr(),
            http_mthd,
            "data to URI",
            (*ctxt).uri
        );
        xml_ioerr(XmlParserErrors::XmlIoWrite, msg.as_ptr() as _);
    } else {
        http_ctxt = xml_nanohttp_method(
            (*ctxt).uri,
            http_mthd,
            http_content,
            addr_of_mut!(content_type) as _,
            content_encoding,
            content_lgth,
        );

        if !http_ctxt.is_null() {
            http_rtn = xml_nanohttp_return_code(http_ctxt);
            if (200..300).contains(&http_rtn) {
                close_rc = 0;
            } else {
                let mut msg: [XmlChar; 500] = [0; 500];
                xml_str_printf!(
                    msg.as_mut_ptr(),
                    500,
                    c"xmlIOHTTPCloseWrite: HTTP '%s' of %d %s\n'%s' %s %d\n".as_ptr(),
                    http_mthd,
                    content_lgth,
                    c"bytes to URI".as_ptr(),
                    (*ctxt).uri,
                    c"failed.  HTTP return code:".as_ptr(),
                    http_rtn
                );
                xml_ioerr(XmlParserErrors::XmlIoWrite, msg.as_ptr() as _);
            }

            xml_nanohttp_close(http_ctxt);
            xml_free(content_type as _);
        }
    }

    /*  Final cleanups  */

    xml_free_http_write_ctxt(ctxt);

    close_rc
}

/**
 * xmlIOHTTPClosePut
 *
 * @context:  The I/O context
 *
 * Close the transmit HTTP I/O channel and actually send data using a PUT
 * HTTP method.
 */
#[cfg(all(feature = "output", feature = "http"))]
unsafe extern "C" fn xml_io_http_close_put(ctxt: *mut c_void) -> c_int {
    xml_io_http_close_write(ctxt, c"PUT".as_ptr() as _)
}

/**
 * xmlRegisterDefaultOutputCallbacks:
 *
 * Registers the default compiled-in I/O handlers.
 */
#[cfg(feature = "output")]
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

    /*********************************
     No way a-priori to distinguish between gzipped files from
     uncompressed ones except opening if existing then closing
     and saving with same compression ratio ... a pain.

    #ifdef LIBXML_ZLIB_ENABLED
        xmlRegisterOutputCallbacks(xmlGzfileMatch, xmlGzfileOpen,
                               xmlGzfileWrite, xmlGzfileClose);
    #endif

     Nor FTP PUT ....
    #ifdef LIBXML_FTP_ENABLED
        xmlRegisterOutputCallbacks(xmlIOFTPMatch, xmlIOFTPOpen,
                               xmlIOFTPWrite, xmlIOFTPClose);
    #endif
     **********************************/
    XML_OUTPUT_CALLBACK_INITIALIZED.store(true, Ordering::Release);
}

/**
 * xmlAllocOutputBuffer:
 * @encoder:  the encoding converter or NULL
 *
 * Create a buffered parser output
 *
 * Returns the new parser output or NULL
 */
#[cfg(feature = "output")]
pub unsafe fn xml_alloc_output_buffer(
    encoder: Option<XmlCharEncodingHandler>,
) -> XmlOutputBufferPtr {
    let ret: XmlOutputBufferPtr = xml_malloc(size_of::<XmlOutputBuffer>()) as XmlOutputBufferPtr;
    if ret.is_null() {
        xml_ioerr_memory(c"creating output buffer".as_ptr() as _);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlOutputBuffer>());
    let Some(mut buf) = XmlBufRef::new() else {
        xml_free(ret as _);
        return null_mut();
    };
    buf.set_allocation_scheme(XmlBufferAllocationScheme::XmlBufferAllocDoubleit);
    (*ret).buffer = Some(buf);

    (*ret).encoder = encoder.map(|e| Rc::new(RefCell::new(e)));
    if (*ret).encoder.is_some() {
        (*ret).conv = XmlBufRef::with_capacity(4000);
        if (*ret).conv.is_none() {
            buf.free();
            xml_free(ret as _);
            return null_mut();
        }

        /*
         * This call is designed to initiate the encoder state
         */
        (*ret).encode(true);
    } else {
        (*ret).conv = None;
    }
    (*ret).writecallback = None;
    (*ret).closecallback = None;
    (*ret).context = null_mut();
    (*ret).written = 0;

    ret
}

/**
 * xmlOutputBufferCreateFilename:
 * @URI:  a C string containing the URI or filename
 * @encoder:  the encoding converter or NULL
 * @compression:  the compression ration (0 none, 9 max).
 *
 * Create a buffered  output for the progressive saving of a file
 * If filename is "-' then we use stdout as the output.
 * Automatic support for ZLIB/Compress compressed document is provided
 * by default if found at compile-time.
 * TODO: currently if compression is set, the library only support
 *       writing to a local file.
 *
 * Returns the new output or NULL
 */
#[cfg(feature = "output")]
pub unsafe fn xml_output_buffer_create_filename(
    uri: *const c_char,
    encoder: Option<Rc<RefCell<XmlCharEncodingHandler>>>,
    compression: c_int,
) -> XmlOutputBufferPtr {
    if let Some(f) = GLOBAL_STATE.with_borrow(|state| state.output_buffer_create_filename_value) {
        let uri = CStr::from_ptr(uri).to_str().unwrap();
        return f(uri, encoder, compression);
    }
    __xml_output_buffer_create_filename(uri, encoder, compression)
}

/**
 * xmlOutputBufferCreateFile:
 * @file:  a FILE*
 * @encoder:  the encoding converter or NULL
 *
 * Create a buffered output for the progressive saving to a FILE *
 * buffered C I/O
 *
 * Returns the new parser output or NULL
 */
#[cfg(feature = "output")]
pub unsafe fn xml_output_buffer_create_file(
    file: *mut FILE,
    encoder: Option<XmlCharEncodingHandler>,
) -> XmlOutputBufferPtr {
    if !XML_OUTPUT_CALLBACK_INITIALIZED.load(Ordering::Acquire) {
        xml_register_default_output_callbacks();
    }

    if file.is_null() {
        return null_mut();
    }

    let ret: XmlOutputBufferPtr =
        xml_alloc_output_buffer_internal(encoder.map(|e| Rc::new(RefCell::new(e))));
    if !ret.is_null() {
        (*ret).context = file as _;
        (*ret).writecallback = Some(xml_file_write);
        (*ret).closecallback = Some(xml_file_flush);
    }

    ret
}

/**
 * xmlFdWrite:
 * @context:  the I/O context
 * @buffer:  where to get data
 * @len:  number of bytes to write
 *
 * Write @len bytes from @buffer to the I/O channel.
 *
 * Returns the number of bytes written
 */
#[cfg(feature = "output")]
unsafe extern "C" fn xml_fd_write(
    context: *mut c_void,
    buffer: *const c_char,
    len: c_int,
) -> c_int {
    let mut ret: c_int = 0;

    if len > 0 {
        ret = write(context as ptrdiff_t as c_int, buffer.add(0) as _, len as _) as _;
        if ret < 0 {
            xml_ioerr(XmlParserErrors::XmlErrOK, c"write()".as_ptr() as _);
        }
    }
    ret
}

/**
 * xmlOutputBufferCreateFd:
 * @fd:  a file descriptor number
 * @encoder:  the encoding converter or NULL
 *
 * Create a buffered output for the progressive saving
 * to a file descriptor
 *
 * Returns the new parser output or NULL
 */
#[cfg(feature = "output")]
pub unsafe fn xml_output_buffer_create_fd(
    fd: c_int,
    encoder: Option<Rc<RefCell<XmlCharEncodingHandler>>>,
) -> XmlOutputBufferPtr {
    if fd < 0 {
        return null_mut();
    }

    let ret: XmlOutputBufferPtr = xml_alloc_output_buffer_internal(encoder);
    if !ret.is_null() {
        (*ret).context = fd as ptrdiff_t as *mut c_void;
        (*ret).writecallback = Some(xml_fd_write);
        (*ret).closecallback = None;
    }

    ret
}

/**
 * xmlOutputBufferCreateIO:
 * @iowrite:  an I/O write function
 * @ioclose:  an I/O close function
 * @ioctx:  an I/O handler
 * @encoder:  the charset encoding if known
 *
 * Create a buffered output for the progressive saving
 * to an I/O handler
 *
 * Returns the new parser output or NULL
 */
#[cfg(feature = "output")]
pub unsafe fn xml_output_buffer_create_io(
    iowrite: Option<XmlOutputWriteCallback>,
    ioclose: Option<XmlOutputCloseCallback>,
    ioctx: *mut c_void,
    encoder: Option<Rc<RefCell<XmlCharEncodingHandler>>>,
) -> XmlOutputBufferPtr {
    if iowrite.is_none() {
        return null_mut();
    }

    let ret: XmlOutputBufferPtr = xml_alloc_output_buffer_internal(encoder);
    if !ret.is_null() {
        (*ret).context = ioctx;
        (*ret).writecallback = iowrite;
        (*ret).closecallback = ioclose;
    }

    ret
}

/* Couple of APIs to get the output without digging into the buffers */
/**
 * xmlOutputBufferGetContent:
 * @out:  an xmlOutputBufferPtr
 *
 * Gives a poc_inter to the data currently held in the output buffer
 *
 * Returns a poc_inter to the data or NULL in case of error
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_output_buffer_get_content(out: XmlOutputBufferPtr) -> *const XmlChar {
    if out.is_null() {
        return null();
    }
    (*out).buffer.map_or(null(), |buf| {
        if buf.is_ok() {
            buf.as_ref().as_ptr()
        } else {
            null()
        }
    })
}

/**
 * xmlOutputBufferGetSize:
 * @out:  an xmlOutputBufferPtr
 *
 * Gives the length of the data currently held in the output buffer
 *
 * Returns 0 in case or error or no data is held, the size otherwise
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_output_buffer_get_size(out: XmlOutputBufferPtr) -> size_t {
    if out.is_null() {
        return 0;
    }
    (*out).buffer.map_or(0, |buf| buf.len())
}

/**
 * xmlOutputBufferWriteString:
 * @out:  a buffered parser output
 * @str:  a zero terminated C string
 *
 * Write the content of the string in the output I/O buffer
 * This routine handle the I18N transcoding from c_internal UTF-8
 * The buffer is lossless, i.e. will store in case of partial
 * or delayed writes.
 *
 * Returns the number of chars immediately written, or -1
 *         in case of error.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_output_buffer_write_string(
    out: XmlOutputBufferPtr,
    str: *const c_char,
) -> c_int {
    if out.is_null() || (*out).error != 0 {
        return -1;
    }
    if str.is_null() {
        return -1;
    }
    let len = strlen(str);

    if len > 0 {
        return (*out).write_bytes(from_raw_parts(str as *const u8, len));
    }
    len as i32
}

/**
 * xmlEscapeContent:
 * @out:  a poc_inter to an array of bytes to store the result
 * @outlen:  the length of @out
 * @in:  a poc_inter to an array of unescaped UTF-8 bytes
 * @inlen:  the length of @in
 *
 * Take a block of UTF-8 chars in and escape them.
 * Returns 0 if success, or -1 otherwise
 * The value of @inlen after return is the number of octets consumed
 *     if the return value is positive, else unpredictable.
 * The value of @outlen after return is the number of octets consumed.
 */
#[cfg(feature = "output")]
unsafe extern "C" fn xml_escape_content(
    mut out: *mut c_uchar,
    outlen: *mut c_int,
    mut input: *const XmlChar,
    inlen: *mut c_int,
) -> c_int {
    let outstart: *mut c_uchar = out;
    let base: *const c_uchar = input;
    let outend: *mut c_uchar = out.add(*outlen as usize);

    let inend: *const c_uchar = input.add(*inlen as usize);

    while input < inend && out < outend {
        if *input == b'<' {
            if outend.offset_from(out) < 4 {
                break;
            }
            *out = b'&';
            out = out.add(1);
            *out = b'l';
            out = out.add(1);
            *out = b't';
            out = out.add(1);
            *out = b';';
            out = out.add(1);
        } else if *input == b'>' {
            if outend.offset_from(out) < 4 {
                break;
            }
            *out = b'&';
            out = out.add(1);
            *out = b'g';
            out = out.add(1);
            *out = b't';
            out = out.add(1);
            *out = b';';
            out = out.add(1);
        } else if *input == b'&' {
            if outend.offset_from(out) < 5 {
                break;
            }
            *out = b'&';
            out = out.add(1);
            *out = b'a';
            out = out.add(1);
            *out = b'm';
            out = out.add(1);
            *out = b'p';
            out = out.add(1);
            *out = b';';
            out = out.add(1);
        } else if *input == b'\r' {
            if outend.offset_from(out) < 5 {
                break;
            }
            *out = b'&';
            out = out.add(1);
            *out = b'#';
            out = out.add(1);
            *out = b'1';
            out = out.add(1);
            *out = b'3';
            out = out.add(1);
            *out = b';';
            out = out.add(1);
        } else {
            *out = *input;
            out = out.add(1);
        }
        input = input.add(1);
    }
    *outlen = out.offset_from(outstart) as _;
    *inlen = input.offset_from(base) as _;
    0
}

/**
 * xmlOutputBufferWriteEscape:
 * @out:  a buffered parser output
 * @str:  a zero terminated UTF-8 string
 * @escaping:  an optional escaping function (or NULL)
 *
 * Write the content of the string in the output I/O buffer
 * This routine escapes the characters and then handle the I18N
 * transcoding from c_internal UTF-8
 * The buffer is lossless, i.e. will store in case of partial
 * or delayed writes.
 *
 * Returns the number of chars immediately written, or -1
 *         in case of error.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_output_buffer_write_escape(
    out: XmlOutputBufferPtr,
    mut str: *const XmlChar,
    escaping: Option<XmlCharEncodingOutputFunc>,
) -> c_int {
    use crate::encoding::EncodingError;

    let mut nbchars: c_int; /* number of chars to output to I/O */
    let mut ret: c_int; /* return from function call */
    let mut written: c_int = 0; /* number of c_char written to I/O so far */
    let mut oldwritten: c_int; /* loop guard */
    let mut chunk: c_int; /* number of byte currently processed from str */
    let mut len: c_int; /* number of bytes in str */
    let mut cons: c_int; /* byte from str consumed */

    if out.is_null() || (*out).error != 0 || str.is_null() || (*out).buffer.is_none() {
        return -1;
    }
    len = strlen(str as _) as _;
    if len < 0 {
        return 0;
    }
    if (*out).error != 0 {
        return -1;
    }

    let escaping = escaping.unwrap_or(xml_escape_content);

    loop {
        oldwritten = written;

        /*
         * how many bytes to consume and how many bytes to store.
         */
        cons = len;
        chunk = (*out).buffer.map_or(0, |buf| buf.avail() as i32);

        /*
         * make sure we have enough room to save first, if this is
         * not the case force a flush, but make sure we stay in the loop
         */
        if chunk < 40 {
            if (*out).buffer.map_or(true, |mut buf| buf.grow(100).is_err()) {
                return -1;
            }
            oldwritten = -1;
            if !(len > 0 && (oldwritten != written)) {
                break;
            }
            continue;
        }

        /*
         * first handle encoding stuff.
         */
        if (*out).encoder.is_some() {
            /*
             * Store the data in the incoming raw buffer
             */
            if (*out).conv.is_none() {
                (*out).conv = XmlBufRef::new();
            }
            ret = escaping(
                (*out).buffer.map_or(null_mut(), |buf| {
                    buf.as_ref().as_ptr().add(buf.len()) as *mut u8
                }),
                addr_of_mut!(chunk),
                str,
                addr_of_mut!(cons),
            );
            if ret < 0 || chunk == 0
            /* chunk==0 => nothing done */
            {
                return -1;
            }
            (*out)
                .buffer
                .expect("Internal Error")
                .add_len(chunk as usize);

            if (*out).buffer.map_or(0, |buf| buf.len()) < MINLEN && cons == len {
                // goto done;
                return written;
            }

            /*
             * convert as much as possible to the output buffer.
             */
            let ret = match (*out).encode(false) {
                Ok(len) => Ok(len),
                Err(EncodingError::BufferTooShort) => Err(EncodingError::BufferTooShort),
                _ => {
                    xml_ioerr(XmlParserErrors::XmlIoEncoder, null());
                    (*out).error = XmlParserErrors::XmlIoEncoder as i32;
                    return -1;
                }
            };
            if (*out).writecallback.is_some() {
                nbchars = (*out).conv.map_or(0, |conv| conv.len() as i32);
            } else {
                nbchars = if let Ok(len) = ret { len as i32 } else { 0 };
            }
        } else {
            ret = escaping(
                (*out).buffer.map_or(null_mut(), |buf| {
                    buf.as_ref().as_ptr().add(buf.len()) as *mut u8
                }),
                addr_of_mut!(chunk),
                str,
                addr_of_mut!(cons),
            );
            if ret < 0 || chunk == 0
            /* chunk==0 => nothing done */
            {
                return -1;
            }
            (*out)
                .buffer
                .expect("Internal Error")
                .add_len(chunk as usize);
            if (*out).writecallback.is_some() {
                nbchars = (*out).buffer.map_or(0, |buf| buf.len() as i32);
            } else {
                nbchars = chunk;
            }
        }
        str = str.add(cons as _);
        len -= cons;

        if let Some(writecallback) = (*out).writecallback {
            if nbchars < MINLEN as i32 && len <= 0 {
                // goto done;
                return written;
            }

            /*
             * second write the stuff to the I/O channel
             */
            if (*out).encoder.is_some() {
                ret = writecallback(
                    (*out).context,
                    (*out).conv.map_or(null(), |conv| {
                        if conv.is_ok() {
                            conv.as_ref().as_ptr()
                        } else {
                            null()
                        }
                    }) as *const c_char,
                    nbchars,
                );
                if ret >= 0 {
                    if let Some(mut conv) = (*out).conv {
                        conv.trim_head(ret as usize);
                    }
                }
            } else {
                ret = writecallback(
                    (*out).context,
                    (*out).buffer.map_or(null(), |buf| {
                        if buf.is_ok() {
                            buf.as_ref().as_ptr()
                        } else {
                            null()
                        }
                    }) as *const c_char,
                    nbchars,
                );
                if ret >= 0 {
                    if let Some(mut buf) = (*out).buffer {
                        buf.trim_head(ret as usize);
                    }
                }
            }
            if ret < 0 {
                xml_ioerr(XmlParserErrors::XmlIoWrite, null());
                (*out).error = XmlParserErrors::XmlIoWrite as i32;
                return ret;
            }
            if (*out).written > INT_MAX - ret {
                (*out).written = INT_MAX;
            } else {
                (*out).written += ret;
            }
        } else if (*out).buffer.map_or(0, |buf| buf.avail()) < MINLEN {
            if let Some(mut buf) = (*out).buffer {
                buf.grow(MINLEN);
            }
        }
        written += nbchars;

        if !(len > 0 && oldwritten != written) {
            break;
        }
    }

    // done:
    written
}

/**
 * xmlOutputBufferFlush:
 * @out:  a buffered output
 *
 * flushes the output I/O channel
 *
 * Returns the number of byte written or -1 in case of error.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_output_buffer_flush(out: XmlOutputBufferPtr) -> c_int {
    let mut ret: c_int = 0;

    if out.is_null() || (*out).error != 0 {
        return -1;
    }
    /*
     * first handle encoding stuff.
     */
    if (*out).conv.is_some() && (*out).encoder.is_some() {
        /*
         * convert as much as possible to the parser output buffer.
         */
        while {
            let Ok(nbchars) = (*out).encode(false) else {
                xml_ioerr(XmlParserErrors::XmlIoEncoder, null());
                (*out).error = XmlParserErrors::XmlIoEncoder as i32;
                return -1;
            };

            nbchars != 0
        } {}
    }

    /*
     * second flush the stuff to the I/O channel
     */
    if let Some(mut conv) = (*out)
        .conv
        .filter(|_| (*out).encoder.is_some() && (*out).writecallback.is_some())
    {
        // if !(*out).conv.is_null() && !(*out).encoder.is_null() && (*out).writecallback.is_some() {
        ret = ((*out).writecallback.unwrap())(
            (*out).context,
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
    } else if (*out).writecallback.is_some() {
        ret = ((*out).writecallback.unwrap())(
            (*out).context,
            (*out)
                .buffer
                .map_or(null(), |buf| buf.as_ref().as_ptr() as *const i8),
            (*out).buffer.map_or(0, |buf| buf.len() as i32),
        );
        if ret >= 0 {
            if let Some(mut buf) = (*out).buffer {
                buf.trim_head(ret as usize);
            }
        }
    }
    if ret < 0 {
        xml_ioerr(XmlParserErrors::XmlIoFlush, null());
        (*out).error = XmlParserErrors::XmlIoFlush as i32;
        return ret;
    }
    if (*out).written > INT_MAX - ret {
        (*out).written = INT_MAX;
    } else {
        (*out).written += ret;
    }

    ret
}

/**
 * xmlOutputBufferClose:
 * @out:  a buffered output
 *
 * flushes and close the output I/O channel
 * and free up all the associated resources
 *
 * Returns the number of byte written or -1 in case of error.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_output_buffer_close(out: XmlOutputBufferPtr) -> c_int {
    let mut err_rc: c_int = 0;

    if out.is_null() {
        return -1;
    }
    if (*out).writecallback.is_some() {
        xml_output_buffer_flush(out);
    }
    if let Some(closecallback) = (*out).closecallback {
        err_rc = closecallback((*out).context);
    }
    let written: c_int = (*out).written;
    if let Some(conv) = (*out).conv.take() {
        conv.free();
    }
    let _ = (*out).encoder.take();
    if let Some(buf) = (*out).buffer.take() {
        buf.free();
    }

    if (*out).error != 0 {
        err_rc = -1;
    }
    xml_free(out as _);
    if err_rc == 0 {
        written
    } else {
        err_rc
    }
}

/**
 * xmlRegisterOutputCallbacks:
 * @matchFunc:  the xmlOutputMatchCallback
 * @openFunc:  the xmlOutputOpenCallback
 * @writeFunc:  the xmlOutputWriteCallback
 * @closeFunc:  the xmlOutputCloseCallback
 *
 * Register a new set of I/O callback for handling output.
 *
 * Returns the registered handler number or -1 in case of error
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_register_output_callbacks(
    match_func: Option<XmlOutputMatchCallback>,
    open_func: Option<XmlOutputOpenCallback>,
    write_func: Option<XmlOutputWriteCallback>,
    close_func: Option<XmlOutputCloseCallback>,
) -> c_int {
    let num_callbacks = XML_OUTPUT_CALLBACK_NR.load(Ordering::Acquire);
    if num_callbacks >= MAX_OUTPUT_CALLBACK {
        return -1;
    }
    XML_OUTPUT_CALLBACK_TABLE[num_callbacks].matchcallback = match_func;
    XML_OUTPUT_CALLBACK_TABLE[num_callbacks].opencallback = open_func;
    XML_OUTPUT_CALLBACK_TABLE[num_callbacks].writecallback = write_func;
    XML_OUTPUT_CALLBACK_TABLE[num_callbacks].closecallback = close_func;
    XML_OUTPUT_CALLBACK_INITIALIZED.store(true, Ordering::Release);
    XML_OUTPUT_CALLBACK_NR.store(num_callbacks + 1, Ordering::Release);
    num_callbacks as _
}

/**
 * xmlAllocOutputBufferInternal:
 * @encoder:  the encoding converter or NULL
 *
 * Create a buffered parser output
 *
 * Returns the new parser output or NULL
 */
#[cfg(feature = "output")]
pub(crate) unsafe fn xml_alloc_output_buffer_internal(
    encoder: Option<Rc<RefCell<XmlCharEncodingHandler>>>,
) -> XmlOutputBufferPtr {
    let ret: XmlOutputBufferPtr = xml_malloc(size_of::<XmlOutputBuffer>()) as XmlOutputBufferPtr;
    if ret.is_null() {
        xml_ioerr_memory(c"creating output buffer".as_ptr() as _);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlOutputBuffer>());
    let Some(mut buf) = XmlBufRef::new() else {
        xml_free(ret as _);
        return null_mut();
    };
    (*ret).buffer = Some(buf);

    /*
     * For conversion buffers we use the special IO handling
     */
    buf.set_allocation_scheme(XmlBufferAllocationScheme::XmlBufferAllocIo);

    (*ret).encoder = encoder;
    if (*ret).encoder.is_some() {
        (*ret).conv = XmlBufRef::with_capacity(4000);
        if (*ret).conv.is_none() {
            buf.free();
            xml_free(ret as _);
            return null_mut();
        }

        /*
         * This call is designed to initiate the encoder state
         */
        (*ret).encode(true);
    } else {
        (*ret).conv = None;
    }
    (*ret).writecallback = None;
    (*ret).closecallback = None;
    (*ret).context = null_mut();
    (*ret).written = 0;

    ret
}

#[cfg(feature = "output")]
pub(crate) unsafe fn __xml_output_buffer_create_filename(
    uri: *const c_char,
    encoder: Option<Rc<RefCell<XmlCharEncodingHandler>>>,
    _compression: c_int,
) -> XmlOutputBufferPtr {
    let ret: XmlOutputBufferPtr;

    let mut context: *mut c_void = null_mut();
    let mut unescaped: *mut c_char = null_mut();
    // #ifdef LIBXML_ZLIB_ENABLED
    //     let is_file_uri: c_int = 1;
    // #endif

    let is_initialized = XML_OUTPUT_CALLBACK_INITIALIZED.load(Ordering::Acquire);
    if !is_initialized {
        xml_register_default_output_callbacks();
    }

    if uri.is_null() {
        return null_mut();
    }

    let puri: XmlURIPtr = xml_parse_uri(uri);
    if !puri.is_null() {
        // #ifdef LIBXML_ZLIB_ENABLED
        //         if (!(*puri).scheme.is_null() &&
        // 	    (!xmlStrEqual((*puri).scheme, "file"))) {
        //             is_file_uri = 0;
        //         }
        // #endif
        /*
         * try to limit the damages of the URI unescaping code.
         */
        if (*puri).scheme.is_null() || xml_str_equal((*puri).scheme as _, c"file".as_ptr() as _) {
            unescaped = xml_uri_unescape_string(uri, 0, null_mut());
        }
        xml_free_uri(puri);
    }

    let num_callbacks = XML_OUTPUT_CALLBACK_NR.load(Ordering::Acquire);

    /*
     * Try to find one of the output accept method accepting that scheme
     * Go in reverse to give precedence to user defined handlers.
     * try with an unescaped version of the URI
     */
    if !unescaped.is_null() {
        // #ifdef LIBXML_ZLIB_ENABLED
        // 	if ((compression > 0) && (compression <= 9) && (is_file_uri == 1)) {
        // 	    context = xmlGzfileOpenW(unescaped, compression);
        // 	    if !context.is_null() {
        // 		ret = xmlAllocOutputBufferInternal(encoder);
        // 		if !ret.is_null() {
        // 		    (*ret).context = context;
        // 		    (*ret).writecallback = xmlGzfileWrite;
        // 		    (*ret).closecallback = xmlGzfileClose;
        // 		}
        // 		xmlFree(unescaped as _);
        // 		return ret;
        // 	    }
        // 	}
        // #endif
        for i in (0..num_callbacks).rev() {
            if XML_OUTPUT_CALLBACK_TABLE[i]
                .matchcallback
                .filter(|callback| callback(unescaped) != 0)
                .is_some()
            {
                // #if defined(LIBXML_HTTP_ENABLED) && defined(LIBXML_ZLIB_ENABLED)
                // 		/*  Need to pass compression parameter c_into HTTP open calls  */
                // 		if (xmlOutputCallbackTable[i].matchcallback == xmlIOHTTPMatch) {
                // 		    context = xmlIOHTTPOpenW(unescaped, compression);
                //         }
                // 		else
                // #endif
                context = (XML_OUTPUT_CALLBACK_TABLE[i].opencallback.unwrap())(unescaped);
                if !context.is_null() {
                    xml_free(unescaped as _);
                    /*
                     * Allocate the Output buffer front-end.
                     */
                    ret = xml_alloc_output_buffer_internal(encoder);
                    if !ret.is_null() {
                        (*ret).context = context;
                        (*ret).writecallback = XML_OUTPUT_CALLBACK_TABLE[i].writecallback;
                        (*ret).closecallback = XML_OUTPUT_CALLBACK_TABLE[i].closecallback;
                    }
                    return ret;
                }
            }
        }
        xml_free(unescaped as _);
    }

    /*
     * If this failed try with a non-escaped URI this may be a strange
     * filename
     */
    if context.is_null() {
        // #ifdef LIBXML_ZLIB_ENABLED
        // 	if ((compression > 0) && (compression <= 9) && (is_file_uri == 1)) {
        // 	    context = xmlGzfileOpenW(URI, compression);
        // 	    if !context.is_null() {
        // 		ret = xmlAllocOutputBufferInternal(encoder);
        // 		if !ret.is_null() {
        // 		    (*ret).context = context;
        // 		    (*ret).writecallback = xmlGzfileWrite;
        // 		    (*ret).closecallback = xmlGzfileClose;
        // 		}
        // 		else {
        // 		    xmlGzfileClose(context);
        //         }
        // 		return ret;
        // 	    }
        // 	}
        // #endif
        for i in (0..num_callbacks).rev() {
            if XML_OUTPUT_CALLBACK_TABLE[i]
                .matchcallback
                .filter(|callback| callback(uri) != 0)
                .is_some()
            {
                // #if defined(LIBXML_HTTP_ENABLED) && defined(LIBXML_ZLIB_ENABLED)
                // 		/*  Need to pass compression parameter c_into HTTP open calls  */
                // 		if (xmlOutputCallbackTable[i].matchcallback == xmlIOHTTPMatch) {
                // 		    context = xmlIOHTTPOpenW(URI, compression);
                //         }
                // 		else
                // #endif
                context = (XML_OUTPUT_CALLBACK_TABLE[i].opencallback.unwrap())(uri);
                if !context.is_null() {
                    /*
                     * Allocate the Output buffer front-end.
                     */
                    ret = xml_alloc_output_buffer_internal(encoder);
                    if !ret.is_null() {
                        (*ret).context = context;
                        (*ret).writecallback = XML_OUTPUT_CALLBACK_TABLE[i].writecallback;
                        (*ret).closecallback = XML_OUTPUT_CALLBACK_TABLE[i].closecallback;
                    }
                    return ret;
                }
            }
        }
    }

    null_mut()
}

/**
 * xmlIOHTTPClosePost
 *
 * @context:  The I/O context
 *
 * Close the transmit HTTP I/O channel and actually send data using a POST
 * HTTP method.
 */
#[cfg(all(feature = "output", feature = "http"))]
unsafe extern "C" fn xml_io_http_close_post(ctxt: *mut c_void) -> c_int {
    xml_io_http_close_write(ctxt, c"POST".as_ptr() as _)
}

/*  This function only exists if HTTP support built into the library  */
/**
 * xmlRegisterHTTPPostCallbacks:
 *
 * By default, libxml submits HTTP output requests using the "PUT" method.
 * Calling this method changes the HTTP output method to use the "POST"
 * method instead.
 *
 */
#[cfg(all(feature = "output", feature = "http"))]
pub unsafe extern "C" fn xml_register_http_post_callbacks() {
    /*  Register defaults if not done previously  */

    if !XML_OUTPUT_CALLBACK_INITIALIZED.load(Ordering::Acquire) {
        xml_register_default_output_callbacks();
    }

    xml_register_output_callbacks(
        Some(xml_io_http_match),
        Some(xml_io_http_dflt_open_w),
        Some(xml_io_http_write),
        Some(xml_io_http_close_post),
    );
}

/**
 * __xmlLoaderErr:
 * @ctx: the parser context
 * @extra:  extra information
 *
 * Handle a resource access error
 */
pub(crate) unsafe extern "C" fn __xml_loader_err(
    ctx: *mut c_void,
    msg: *const c_char,
    filename: *const c_char,
) {
    let ctxt: XmlParserCtxtPtr = ctx as XmlParserCtxtPtr;
    let mut schannel: Option<StructuredError> = None;
    let mut channel: Option<GenericError> = None;
    let mut data = None;
    let mut level: XmlErrorLevel = XmlErrorLevel::XmlErrError;

    if !ctxt.is_null()
        && ((*ctxt).disable_sax != 0)
        && matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        return;
    }
    if !ctxt.is_null() && !(*ctxt).sax.is_null() {
        if (*ctxt).validate != 0 {
            channel = (*(*ctxt).sax).error;
            level = XmlErrorLevel::XmlErrError;
        } else {
            channel = (*(*ctxt).sax).warning;
            level = XmlErrorLevel::XmlErrWarning;
        }
        if (*(*ctxt).sax).initialized == XML_SAX2_MAGIC as u32 {
            schannel = (*(*ctxt).sax).serror;
        }
        data = (*ctxt).user_data.clone();
    }
    __xml_raise_error!(
        schannel,
        channel,
        data,
        ctxt as _,
        null_mut(),
        XmlErrorDomain::XmlFromIO,
        XmlParserErrors::XmlIoLoadError,
        level,
        null_mut(),
        0,
        (!filename.is_null()).then(|| CStr::from_ptr(filename)
            .to_string_lossy()
            .into_owned()
            .into()),
        None,
        None,
        0,
        0,
        msg,
        filename
    );
}

/**
 * xmlCheckHTTPInput:
 * @ctxt: an XML parser context
 * @ret: an XML parser input
 *
 * Check an input in case it was created from an HTTP stream, in that
 * case it will handle encoding and update of the base URL in case of
 * redirection. It also checks for HTTP errors in which case the input
 * is cleanly freed up and an appropriate error is raised in context
 *
 * Returns the input or NULL in case of HTTP error.
 */
pub unsafe extern "C" fn xml_check_http_input(
    ctxt: XmlParserCtxtPtr,
    mut ret: XmlParserInputPtr,
) -> XmlParserInputPtr {
    #[cfg(feature = "http")]
    {
        if !ret.is_null()
            && !(*ret).buf.is_null()
            && (*(*ret).buf).readcallback == Some(xml_io_http_read)
            && !(*(*ret).buf).context.is_null()
        {
            let encoding: *const c_char;
            let redir: *const c_char;
            let mime: *const c_char;

            let code: c_int = xml_nanohttp_return_code((*(*ret).buf).context);
            if code >= 400 {
                /* fatal error */
                if !(*ret).filename.is_null() {
                    __xml_loader_err(
                        ctxt as _,
                        c"failed to load HTTP resource \"%s\"\n".as_ptr() as _,
                        (*ret).filename as *const c_char,
                    );
                } else {
                    __xml_loader_err(
                        ctxt as _,
                        c"failed to load HTTP resource\n".as_ptr() as _,
                        null(),
                    );
                }
                xml_free_input_stream(ret);
                ret = null_mut();
            } else {
                mime = xml_nanohttp_mime_type((*(*ret).buf).context);
                if !xml_strstr(mime as _, c"/xml".as_ptr() as _).is_null()
                    || !xml_strstr(mime as _, c"+xml".as_ptr() as _).is_null()
                {
                    encoding = xml_nanohttp_encoding((*(*ret).buf).context);
                    if !encoding.is_null() {
                        if let Some(handler) =
                            find_encoding_handler(CStr::from_ptr(encoding).to_str().unwrap())
                        {
                            xml_switch_input_encoding(ctxt, ret, handler);
                        } else {
                            __xml_err_encoding(
                                ctxt,
                                XmlParserErrors::XmlErrUnknownEncoding,
                                c"Unknown encoding %s".as_ptr() as _,
                                encoding as _,
                                null(),
                            );
                        }
                        if (*ret).encoding.is_null() {
                            (*ret).encoding = xml_strdup(encoding as _);
                        }
                    }
                }
                redir = xml_nanohttp_redir((*(*ret).buf).context);
                if !redir.is_null() {
                    if !(*ret).filename.is_null() {
                        xml_free((*ret).filename as _);
                    }
                    if !(*ret).directory.is_null() {
                        xml_free((*ret).directory as _);
                        (*ret).directory = null_mut();
                    }
                    (*ret).filename = xml_strdup(redir as _) as _;
                }
            }
        }
    }
    ret
}

/**
 * xmlDefaultExternalEntityLoader:
 * @URL:  the URL for the entity to load
 * @ID:  the System ID for the entity to load
 * @ctxt:  the context in which the entity is called or NULL
 *
 * By default we don't load external entities, yet.
 *
 * Returns a new allocated xmlParserInputPtr, or NULL.
 */
pub(crate) unsafe extern "C" fn xml_default_external_entity_loader(
    url: *const c_char,
    mut id: *const c_char,
    ctxt: XmlParserCtxtPtr,
) -> XmlParserInputPtr {
    let ret: XmlParserInputPtr;
    let mut resource: *mut XmlChar;

    if !ctxt.is_null() && (*ctxt).options & XmlParserOption::XmlParseNonet as i32 != 0 {
        let options: c_int = (*ctxt).options;

        (*ctxt).options -= XmlParserOption::XmlParseNonet as i32;
        ret = xml_no_net_external_entity_loader(url, id, ctxt);
        (*ctxt).options = options;
        return ret;
    }
    #[cfg(feature = "catalog")]
    {
        resource = xml_resolve_resource_from_catalog(url, id, ctxt);
    }

    if resource.is_null() {
        resource = url as _;
    }

    if resource.is_null() {
        if id.is_null() {
            id = c"NULL".as_ptr() as _;
        }
        __xml_loader_err(
            ctxt as _,
            c"failed to load external entity \"%s\"\n".as_ptr() as _,
            id,
        );
        return null_mut();
    }
    ret = xml_new_input_from_file(ctxt, resource as _);
    if !resource.is_null() && resource != url as _ {
        xml_free(resource as _);
    }
    ret
}

pub(crate) unsafe extern "C" fn xml_no_net_exists(url: *const c_char) -> c_int {
    let path: *const c_char;

    if url.is_null() {
        return 0;
    }

    if xml_strncasecmp(url as _, c"file://localhost/".as_ptr() as _, 17) == 0 {
        #[cfg(target_os = "windows")]
        {
            path = url.add(17);
        }
        #[cfg(not(target_os = "windows"))]
        {
            path = url.add(16);
        }
    } else if xml_strncasecmp(url as _, c"file:///".as_ptr() as _, 8) == 0 {
        #[cfg(target_os = "windows")]
        {
            path = url.add(8);
        }
        #[cfg(not(target_os = "windows"))]
        {
            path = url.add(7);
        }
    } else {
        path = url;
    }

    xml_check_filename(path)
}

/**
 * xmlResolveResourceFromCatalog:
 * @URL:  the URL for the entity to load
 * @ID:  the System ID for the entity to load
 * @ctxt:  the context in which the entity is called or NULL
 *
 * Resolves the URL and ID against the appropriate catalog.
 * This function is used by xmlDefaultExternalEntityLoader and
 * xmlNoNetExternalEntityLoader.
 *
 * Returns a new allocated URL, or NULL.
 */
#[cfg(feature = "catalog")]
unsafe extern "C" fn xml_resolve_resource_from_catalog(
    url: *const c_char,
    id: *const c_char,
    ctxt: XmlParserCtxtPtr,
) -> *mut XmlChar {
    let mut resource: *mut XmlChar = null_mut();

    /*
     * If the resource doesn't exists as a file,
     * try to load it from the resource poc_inted in the catalogs
     */
    let pref: XmlCatalogAllow = xml_catalog_get_defaults();

    if !matches!(pref, XmlCatalogAllow::None) && xml_no_net_exists(url) == 0 {
        /*
         * Do a local lookup
         */
        if !ctxt.is_null()
            && !(*ctxt).catalogs.is_null()
            && matches!(pref, XmlCatalogAllow::All | XmlCatalogAllow::Document)
        {
            resource = xml_catalog_local_resolve((*ctxt).catalogs, id as _, url as _);
        }
        /*
         * Try a global lookup
         */
        if resource.is_null() && matches!(pref, XmlCatalogAllow::All | XmlCatalogAllow::Global) {
            resource = xml_catalog_resolve(id as _, url as _);
        }
        if resource.is_null() && !url.is_null() {
            resource = xml_strdup(url as _);
        }

        /*
         * TODO: do an URI lookup on the reference
         */
        if !resource.is_null() && xml_no_net_exists(resource as _) == 0 {
            let mut tmp: *mut XmlChar = null_mut();

            if !ctxt.is_null()
                && !(*ctxt).catalogs.is_null()
                && matches!(pref, XmlCatalogAllow::All | XmlCatalogAllow::Document)
            {
                tmp = xml_catalog_local_resolve_uri((*ctxt).catalogs, resource);
            }
            if tmp.is_null() && matches!(pref, XmlCatalogAllow::All | XmlCatalogAllow::Global) {
                tmp = xml_catalog_resolve_uri(resource);
            }

            if !tmp.is_null() {
                xml_free(resource as _);
                resource = tmp;
            }
        }
    }

    resource
}

/*
 * A predefined entity loader disabling network accesses
 */
/**
 * xmlNoNetExternalEntityLoader:
 * @URL:  the URL for the entity to load
 * @ID:  the System ID for the entity to load
 * @ctxt:  the context in which the entity is called or NULL
 *
 * A specific entity loader disabling network accesses, though still
 * allowing local catalog accesses for resolution.
 *
 * Returns a new allocated xmlParserInputPtr, or NULL.
 */
pub unsafe extern "C" fn xml_no_net_external_entity_loader(
    url: *const c_char,
    id: *const c_char,
    ctxt: XmlParserCtxtPtr,
) -> XmlParserInputPtr {
    let mut resource: *mut XmlChar;

    #[cfg(feature = "catalog")]
    {
        resource = xml_resolve_resource_from_catalog(url, id, ctxt);
    }
    #[cfg(not(feature = "catalog"))]
    {
        resource = null_mut();
    }

    if resource.is_null() {
        resource = url as _;
    }

    if !resource.is_null()
        && (xml_strncasecmp(resource as _, c"ftp://".as_ptr() as _, 6) == 0
            || xml_strncasecmp(resource as _, c"http://".as_ptr() as _, 7) == 0)
    {
        xml_ioerr(XmlParserErrors::XmlIoNetworkAttempt, resource as _);
        if resource != url as _ {
            xml_free(resource as _);
        }
        return null_mut();
    }
    let input: XmlParserInputPtr = xml_default_external_entity_loader(resource as _, id, ctxt);
    if resource != url as _ {
        xml_free(resource as _);
    }
    input
}

/*
 * xmlNormalizeWindowsPath is obsolete, don't use it.
 * Check xmlCanonicPath in uri.h for a better alternative.
 */
/**
 * xmlNormalizeWindowsPath:
 * @path: the input file path
 *
 * This function is obsolete. Please see xmlURIFromPath in uri.c for
 * a better solution.
 *
 * Returns a canonicalized version of the path
 */
pub unsafe extern "C" fn xml_normalize_windows_path(path: *const XmlChar) -> *mut XmlChar {
    xml_canonic_path(path)
}

/**
 * __xmlIOWin32UTF8ToWChar:
 * @u8String:  uft-8 string
 *
 * Convert a string from utf-8 to wchar (WINDOWS ONLY!)
 */
#[cfg(target_os = "windows")]
unsafe extern "C" fn __xmlIOWin32UTF8ToWChar(u8String: *const c_char) -> *mut wchar_t {
    let wString: *mut wchar_t = null_mut();

    if !u8String.is_null() {
        let wLen: c_int =
            MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, u8String, -1, null(), 0);
        if (wLen) {
            wString = xml_malloc(wLen * sizeof(wchar_t));
            if (wString) && (MultiByteToWideChar(CP_UTF8, 0, u8String, -1, wString, wLen) == 0) {
                xml_free(wString as _);
                wString = NULL;
            }
        }
    }

    return wString;
}

/**
 *  xmlWrapStatUtf8:
 * @path:  the path in utf-8 encoding
 * @info:  structure that stores results
 *
 * function obtains information about the file or directory
 *
 */
#[cfg(target_os = "windows")]
unsafe extern "C" fn xmlWrapStatUtf8(path: *const c_char, info: *mut stat) -> c_int {
    let retval: c_int = -1;
    let wPath: *mut wchar_t;

    wPath = __xmlIOWin32UTF8ToWChar(path);
    if !wPath.is_null() {
        retval = wstat(wPath, info);
        xml_free(wPath as _);
    }
    /* maybe path in native encoding */
    if retval < 0 {
        retval = stat(path, info);
    }
    return retval;
}

macro_rules! S_ISDIR {
    ($m:expr) => {
        $m & libc::S_IFMT == libc::S_IFDIR
    };
}

/**
 * xmlCheckFilename:
 * @path:  the path to check
 *
 * function checks to see if @path is a valid source
 * (file, socket...) for XML.
 *
 * if stat is not available on the target machine,
 * returns 1.  if stat fails, returns 0 (if calling
 * stat on the filename fails, it can't be right).
 * if stat succeeds and the file is a directory,
 * returns 2.  otherwise returns 1.
 */
pub unsafe extern "C" fn xml_check_filename(path: *const c_char) -> c_int {
    let mut stat_buffer: stat = unsafe { zeroed() };
    if path.is_null() {
        return 0;
    }

    #[cfg(target_os = "windows")]
    {
        /*
         * On Windows stat and wstat do not work with long pathname,
         * which start with '\\?\'
         */
        if *path.add(0) == b'\\' as i8
            && *path.add(1) == b'\\' as i8
            && *path.add(2) == b'?' as i8
            && *path.add(3) == b'\\' as i8
        {
            return 1;
        }

        if xmlWrapStatUtf8(path, addr_of_mut!(stat_buffer)) == -1 {
            return 0;
        }
    }
    #[cfg(not(target_os = "windows"))]
    {
        if stat(path, addr_of_mut!(stat_buffer)) == -1 {
            return 0;
        }
    }
    if S_ISDIR!(stat_buffer.st_mode) {
        return 2;
    }
    1
}

/**
 * Default 'file://' protocol callbacks
 */
/**
 * xmlFileMatch:
 * @filename:  the URI for matching
 *
 * input from FILE *
 *
 * Returns 1 if matches, 0 otherwise
 */
pub unsafe extern "C" fn xml_file_match(_filename: *const c_char) -> c_int {
    1
}

/**
 * xmlFileOpen_real:
 * @filename:  the URI for matching
 *
 * input from FILE *, supports compressed input
 * if @filename is " " then the standard input is used
 *
 * Returns an I/O context or NULL in case of error
 */
unsafe extern "C" fn xml_file_open_real(filename: *const c_char) -> *mut c_void {
    let mut path: *const c_char = filename;
    let fd: *mut FILE;

    if filename.is_null() {
        return null_mut();
    }

    if strcmp(filename as _, c"-".as_ptr() as _) == 0 {
        extern "C" {
            // Does it work ????
            static stdin: *mut FILE;
        }
        fd = stdin;
        return fd as _;
    }

    if xml_strncasecmp(filename as _, c"file://localhost/".as_ptr() as _, 17) == 0 {
        #[cfg(target_os = "windows")]
        {
            path = filename.add(17);
        }
        #[cfg(not(target_os = "windows"))]
        {
            path = filename.add(16);
        }
    } else if xml_strncasecmp(filename as _, c"file:///".as_ptr() as _, 8) == 0 {
        #[cfg(target_os = "windows")]
        {
            path = filename.add(8);
        }
        #[cfg(not(target_os = "windows"))]
        {
            path = filename.add(7);
        }
    } else if xml_strncasecmp(filename as _, c"file:/".as_ptr() as _, 6) == 0 {
        /* lots of generators seems to lazy to read RFC 1738 */
        #[cfg(target_os = "windows")]
        {
            path = filename.add(6);
        }
        #[cfg(not(target_os = "windows"))]
        {
            path = filename.add(5);
        }
    }

    /* Do not check DDNAME on zOS ! */
    // #if !defined(__MVS__)
    if xml_check_filename(path) == 0 {
        return null_mut();
    }
    // #endif

    #[cfg(target_os = "windows")]
    {
        fd = xmlWrapOpenUtf8(path, 0);
    }
    #[cfg(not(target_os = "windows"))]
    {
        fd = fopen(path, c"rb".as_ptr() as _);
    }
    if fd.is_null() {
        xml_ioerr(XmlParserErrors::XmlErrOK, path);
    }
    fd as _
}

/**
 * xmlFileOpen:
 * @filename:  the URI for matching
 *
 * Wrapper around xmlFileOpen_real that try it with an unescaped
 * version of @filename, if this fails fallback to @filename
 *
 * Returns a handler or NULL in case or failure
 */
pub unsafe extern "C" fn xml_file_open(filename: *const c_char) -> *mut c_void {
    let unescaped: *mut c_char;
    let mut retval: *mut c_void;

    retval = xml_file_open_real(filename);
    if retval.is_null() {
        unescaped = xml_uri_unescape_string(filename, 0, null_mut());
        if !unescaped.is_null() {
            retval = xml_file_open_real(unescaped);
            xml_free(unescaped as _);
        }
    }

    retval
}

/**
 * xmlFileRead:
 * @context:  the I/O context
 * @buffer:  where to drop data
 * @len:  number of bytes to write
 *
 * Read @len bytes to @buffer from the I/O channel.
 *
 * Returns the number of bytes written or < 0 in case of failure
 */
pub unsafe extern "C" fn xml_file_read(
    context: *mut c_void,
    buffer: *mut c_char,
    len: c_int,
) -> c_int {
    if context.is_null() || buffer.is_null() {
        return -1;
    }
    let ret: c_int = fread(buffer.add(0) as _, 1, len as _, context as _) as _;
    if ret < 0 {
        xml_ioerr(XmlParserErrors::XmlErrOK, c"fread()".as_ptr() as _);
    }
    ret
}

/**
 * xmlFileClose:
 * @context:  the I/O context
 *
 * Close an I/O channel
 *
 * Returns 0 or -1 in case of error
 */
pub unsafe extern "C" fn xml_file_close(context: *mut c_void) -> c_int {
    let ret: c_int;

    if context.is_null() {
        return -1;
    }
    let fil: *mut FILE = context as *mut FILE;
    extern "C" {
        // Does it work ???
        static stdin: *mut FILE;
        static stdout: *mut FILE;
        static stderr: *mut FILE;
    }
    if fil == stdout || fil == stderr {
        ret = fflush(fil);
        if ret < 0 {
            xml_ioerr(XmlParserErrors::XmlErrOK, c"fflush()".as_ptr() as _);
        }
        return 0;
    }
    if fil == stdin {
        return 0;
    }
    ret = if fclose(context as *mut FILE) == EOF {
        -1
    } else {
        0
    };
    if ret < 0 {
        xml_ioerr(XmlParserErrors::XmlErrOK, c"fclose()".as_ptr() as _);
    }
    ret
}

/**
 * Default 'http://' protocol callbacks
 */
/**
 * xmlIOHTTPMatch:
 * @filename:  the URI for matching
 *
 * check if the URI matches an HTTP one
 *
 * Returns 1 if matches, 0 otherwise
 */
#[cfg(feature = "http")]
pub unsafe extern "C" fn xml_io_http_match(filename: *const c_char) -> c_int {
    if xml_strncasecmp(filename as _, c"http://".as_ptr() as _, 7) == 0 {
        return 1;
    }
    0
}

/**
 * xmlIOHTTPOpen:
 * @filename:  the URI for matching
 *
 * open an HTTP I/O channel
 *
 * Returns an I/O context or NULL in case of error
 */
#[cfg(feature = "http")]
pub unsafe extern "C" fn xml_io_http_open(filename: *const c_char) -> *mut c_void {
    xml_nanohttp_open(filename as _, null_mut())
}

/**
 * xmlIOHTTPOpenW:
 * @post_uri:  The destination URI for the document
 * @compression:  The compression desired for the document.
 *
 * Open a temporary buffer to collect the document for a subsequent HTTP POST
 * request.  Non-static as is called from the output buffer creation routine.
 *
 * Returns an I/O context or NULL in case of error.
 */
#[cfg(all(feature = "http", feature = "output"))]
pub unsafe extern "C" fn xml_io_http_open_w(
    post_uri: *const c_char,
    _compression: c_int,
) -> *mut c_void {
    let mut ctxt: XmlIOHTTPWriteCtxtPtr;

    if post_uri.is_null() {
        return null_mut();
    }

    ctxt = xml_malloc(size_of::<XmlIOHTTPWriteCtxt>()) as _;
    if ctxt.is_null() {
        xml_ioerr_memory(c"creating HTTP output context".as_ptr() as _);
        return null_mut();
    }

    memset(ctxt as _, 0, size_of::<XmlIOHTTPWriteCtxt>());

    (*ctxt).uri = xml_strdup(post_uri as _) as _;
    if (*ctxt).uri.is_null() {
        xml_ioerr_memory(c"copying URI".as_ptr() as _);
        xml_free_http_write_ctxt(ctxt);
        return null_mut();
    }

    /*
     * **  Since the document length is required for an HTTP post,
     * **  need to put the document c_into a buffer.  A memory buffer
     * **  is being used to avoid pushing the data to disk and back.
     */
    // #ifdef LIBXML_ZLIB_ENABLED
    //     if ((compression > 0) && (compression <= 9)) {

    //         (*ctxt).compression = compression;
    //         (*ctxt).doc_buff = xmlCreateZMemBuff(compression);
    //     } else
    // #endif
    {
        /*  Any character conversions should have been done before this  */

        (*ctxt).doc_buff = xml_alloc_output_buffer_internal(None) as _;
    }

    if (*ctxt).doc_buff.is_null() {
        xml_free_http_write_ctxt(ctxt);
        ctxt = null_mut();
    }

    ctxt as _
}

/**
 * xmlIOHTTPRead:
 * @context:  the I/O context
 * @buffer:  where to drop data
 * @len:  number of bytes to write
 *
 * Read @len bytes to @buffer from the I/O channel.
 *
 * Returns the number of bytes written
 */
#[cfg(feature = "http")]
pub unsafe extern "C" fn xml_io_http_read(
    context: *mut c_void,
    buffer: *mut c_char,
    len: c_int,
) -> c_int {
    if buffer.is_null() || len < 0 {
        return -1;
    }
    xml_nanohttp_read(context, buffer.add(0) as _, len)
}

/**
 * xmlIOHTTPClose:
 * @context:  the I/O context
 *
 * Close an HTTP I/O channel
 *
 * Returns 0
 */
#[cfg(feature = "http")]
pub unsafe extern "C" fn xml_io_http_close(context: *mut c_void) -> c_int {
    xml_nanohttp_close(context);
    0
}

/**
 * Default 'ftp://' protocol callbacks
 */
/**
 * xmlIOFTPMatch:
 * @filename:  the URI for matching
 *
 * check if the URI matches an FTP one
 *
 * Returns 1 if matches, 0 otherwise
 */
#[cfg(feature = "ftp")]
pub unsafe extern "C" fn xml_io_ftp_match(filename: *const c_char) -> c_int {
    if xml_strncasecmp(filename as _, c"ftp://".as_ptr() as _, 6) == 0 {
        return 1;
    }
    0
}
/**
 * xmlIOFTPOpen:
 * @filename:  the URI for matching
 *
 * open an FTP I/O channel
 *
 * Returns an I/O context or NULL in case of error
 */
#[cfg(feature = "ftp")]
pub unsafe extern "C" fn xml_io_ftp_open(filename: *const c_char) -> *mut c_void {
    xml_nanoftp_open(filename)
}

/**
 * xmlIOFTPRead:
 * @context:  the I/O context
 * @buffer:  where to drop data
 * @len:  number of bytes to write
 *
 * Read @len bytes to @buffer from the I/O channel.
 *
 * Returns the number of bytes written
 */
#[cfg(feature = "ftp")]
pub unsafe extern "C" fn xml_io_ftp_read(
    context: *mut c_void,
    buffer: *mut c_char,
    len: c_int,
) -> c_int {
    if buffer.is_null() || len < 0 {
        return -1;
    }
    xml_nanoftp_read(context, buffer.add(0) as _, len)
}

/**
 * xmlIOFTPClose:
 * @context:  the I/O context
 *
 * Close an FTP I/O channel
 *
 * Returns 0
 */
#[cfg(feature = "ftp")]
pub unsafe extern "C" fn xml_io_ftp_close(context: *mut c_void) -> c_int {
    xml_nanoftp_close(context)
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_check_filename() {
        unsafe {
            let mut leaks = 0;

            for n_path in 0..GEN_NB_CONST_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let path = gen_const_char_ptr(n_path, 0);

                let ret_val = xml_check_filename(path);
                desret_int(ret_val);
                des_const_char_ptr(n_path, path, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlCheckFilename",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlCheckFilename()");
                    eprintln!(" {}", n_path);
                }
            }
        }
    }

    #[test]
    fn test_xml_check_httpinput() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                for n_ret in 0..GEN_NB_XML_PARSER_INPUT_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);
                    let ret = gen_xml_parser_input_ptr(n_ret, 1);

                    let ret_val = xml_check_http_input(ctxt, ret);
                    desret_xml_parser_input_ptr(ret_val);
                    des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_xml_parser_input_ptr(n_ret, ret, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlCheckHTTPInput",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlCheckHTTPInput()");
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_ret);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_cleanup_input_callbacks() {
        unsafe {
            let mut leaks = 0;
            let mem_base = xml_mem_blocks();

            xml_cleanup_input_callbacks();
            reset_last_error();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in xmlCleanupInputCallbacks",
                    xml_mem_blocks() - mem_base
                );
                assert!(
                    leaks == 0,
                    "{leaks} Leaks are found in xmlCleanupInputCallbacks()"
                );
            }
        }
    }

    #[test]
    fn test_xml_cleanup_output_callbacks() {
        #[cfg(feature = "output")]
        unsafe {
            let mut leaks = 0;

            let mem_base = xml_mem_blocks();

            xml_cleanup_output_callbacks();
            reset_last_error();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in xmlCleanupOutputCallbacks",
                    xml_mem_blocks() - mem_base
                );
                assert!(
                    leaks == 0,
                    "{leaks} Leaks are found in xmlCleanupOutputCallbacks()"
                );
            }
        }
    }

    #[test]
    fn test_xml_file_close() {
        unsafe {
            let mut leaks = 0;

            for n_context in 0..GEN_NB_VOID_PTR {
                let mem_base = xml_mem_blocks();
                let context = gen_void_ptr(n_context, 0);

                let ret_val = xml_file_close(context);
                desret_int(ret_val);
                des_void_ptr(n_context, context, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlFileClose",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlFileClose()");
                    eprintln!(" {}", n_context);
                }
            }
        }
    }

    #[test]
    fn test_xml_file_match() {
        unsafe {
            let mut leaks = 0;

            for n_filename in 0..GEN_NB_FILEPATH {
                let mem_base = xml_mem_blocks();
                let filename = gen_filepath(n_filename, 0);

                let ret_val = xml_file_match(filename);
                desret_int(ret_val);
                des_filepath(n_filename, filename, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlFileMatch",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlFileMatch()");
                    eprintln!(" {}", n_filename);
                }
            }
        }
    }

    #[test]
    fn test_xml_file_open() {
        unsafe {
            let mut leaks = 0;

            for n_filename in 0..GEN_NB_FILEPATH {
                let mem_base = xml_mem_blocks();
                let filename = gen_filepath(n_filename, 0);

                let ret_val = xml_file_open(filename);
                desret_void_ptr(ret_val);
                des_filepath(n_filename, filename, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlFileOpen",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlFileOpen()");
                    eprintln!(" {}", n_filename);
                }
            }
        }
    }

    #[test]
    fn test_xml_file_read() {
        unsafe {
            let mut leaks = 0;

            for n_context in 0..GEN_NB_VOID_PTR {
                for n_buffer in 0..GEN_NB_CHAR_PTR {
                    for n_len in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let context = gen_void_ptr(n_context, 0);
                        let buffer = gen_char_ptr(n_buffer, 1);
                        let len = gen_int(n_len, 2);

                        let ret_val = xml_file_read(context, buffer, len);
                        desret_int(ret_val);
                        des_void_ptr(n_context, context, 0);
                        des_char_ptr(n_buffer, buffer, 1);
                        des_int(n_len, len, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlFileRead",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlFileRead()");
                            eprint!(" {}", n_context);
                            eprint!(" {}", n_buffer);
                            eprintln!(" {}", n_len);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_ioftpclose() {
        #[cfg(feature = "ftp")]
        unsafe {
            let mut leaks = 0;

            for n_context in 0..GEN_NB_VOID_PTR {
                let mem_base = xml_mem_blocks();
                let context = gen_void_ptr(n_context, 0);

                let ret_val = xml_io_ftp_close(context);
                desret_int(ret_val);
                des_void_ptr(n_context, context, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlIOFTPClose",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlIOFTPClose()");
                    eprintln!(" {}", n_context);
                }
            }
        }
    }

    #[test]
    fn test_xml_ioftpmatch() {
        #[cfg(feature = "ftp")]
        unsafe {
            let mut leaks = 0;

            for n_filename in 0..GEN_NB_FILEPATH {
                let mem_base = xml_mem_blocks();
                let filename = gen_filepath(n_filename, 0);

                let ret_val = xml_io_ftp_match(filename);
                desret_int(ret_val);
                des_filepath(n_filename, filename, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlIOFTPMatch",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlIOFTPMatch()");
                    eprintln!(" {}", n_filename);
                }
            }
        }
    }

    #[test]
    fn test_xml_ioftpopen() {
        #[cfg(feature = "ftp")]
        unsafe {
            let mut leaks = 0;

            for n_filename in 0..GEN_NB_FILEPATH {
                let mem_base = xml_mem_blocks();
                let filename = gen_filepath(n_filename, 0);

                let ret_val = xml_io_ftp_open(filename);
                desret_void_ptr(ret_val);
                des_filepath(n_filename, filename, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlIOFTPOpen",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlIOFTPOpen()");
                    eprintln!(" {}", n_filename);
                }
            }
        }
    }

    #[test]
    fn test_xml_ioftpread() {
        #[cfg(feature = "ftp")]
        unsafe {
            let mut leaks = 0;

            for n_context in 0..GEN_NB_VOID_PTR {
                for n_buffer in 0..GEN_NB_CHAR_PTR {
                    for n_len in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let context = gen_void_ptr(n_context, 0);
                        let buffer = gen_char_ptr(n_buffer, 1);
                        let len = gen_int(n_len, 2);

                        let ret_val = xml_io_ftp_read(context, buffer, len);
                        desret_int(ret_val);
                        des_void_ptr(n_context, context, 0);
                        des_char_ptr(n_buffer, buffer, 1);
                        des_int(n_len, len, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlIOFTPRead",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlIOFTPRead()");
                            eprint!(" {}", n_context);
                            eprint!(" {}", n_buffer);
                            eprintln!(" {}", n_len);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_iohttpclose() {
        #[cfg(feature = "http")]
        unsafe {
            let mut leaks = 0;

            for n_context in 0..GEN_NB_VOID_PTR {
                let mem_base = xml_mem_blocks();
                let context = gen_void_ptr(n_context, 0);

                let ret_val = xml_io_http_close(context);
                desret_int(ret_val);
                des_void_ptr(n_context, context, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlIOHTTPClose",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlIOHTTPClose()");
                    eprintln!(" {}", n_context);
                }
            }
        }
    }

    #[test]
    fn test_xml_iohttpmatch() {
        #[cfg(feature = "http")]
        unsafe {
            let mut leaks = 0;

            for n_filename in 0..GEN_NB_FILEPATH {
                let mem_base = xml_mem_blocks();
                let filename = gen_filepath(n_filename, 0);

                let ret_val = xml_io_http_match(filename);
                desret_int(ret_val);
                des_filepath(n_filename, filename, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlIOHTTPMatch",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlIOHTTPMatch()");
                    eprintln!(" {}", n_filename);
                }
            }
        }
    }

    #[test]
    fn test_xml_iohttpopen() {
        #[cfg(feature = "http")]
        unsafe {
            let mut leaks = 0;

            for n_filename in 0..GEN_NB_FILEPATH {
                let mem_base = xml_mem_blocks();
                let filename = gen_filepath(n_filename, 0);

                let ret_val = xml_io_http_open(filename);
                desret_xml_nano_httpctxt_ptr(ret_val);
                des_filepath(n_filename, filename, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlIOHTTPOpen",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlIOHTTPOpen()");
                    eprintln!(" {}", n_filename);
                }
            }
        }
    }

    #[test]
    fn test_xml_iohttpread() {
        #[cfg(feature = "http")]
        unsafe {
            let mut leaks = 0;

            for n_context in 0..GEN_NB_VOID_PTR {
                for n_buffer in 0..GEN_NB_CHAR_PTR {
                    for n_len in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let context = gen_void_ptr(n_context, 0);
                        let buffer = gen_char_ptr(n_buffer, 1);
                        let len = gen_int(n_len, 2);

                        let ret_val = xml_io_http_read(context, buffer, len);
                        desret_int(ret_val);
                        des_void_ptr(n_context, context, 0);
                        des_char_ptr(n_buffer, buffer, 1);
                        des_int(n_len, len, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlIOHTTPRead",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlIOHTTPRead()");
                            eprint!(" {}", n_context);
                            eprint!(" {}", n_buffer);
                            eprintln!(" {}", n_len);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_no_net_external_entity_loader() {
        unsafe {
            let mut leaks = 0;

            for n_url in 0..GEN_NB_FILEPATH {
                for n_id in 0..GEN_NB_CONST_CHAR_PTR {
                    for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                        let mem_base = xml_mem_blocks();
                        let url = gen_filepath(n_url, 0);
                        let id = gen_const_char_ptr(n_id, 1);
                        let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 2);

                        let ret_val = xml_no_net_external_entity_loader(url, id, ctxt);
                        desret_xml_parser_input_ptr(ret_val);
                        des_filepath(n_url, url, 0);
                        des_const_char_ptr(n_id, id, 1);
                        des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlNoNetExternalEntityLoader",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlNoNetExternalEntityLoader()"
                            );
                            eprint!(" {}", n_url);
                            eprint!(" {}", n_id);
                            eprintln!(" {}", n_ctxt);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_normalize_windows_path() {
        unsafe {
            let mut leaks = 0;

            for n_path in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let path = gen_const_xml_char_ptr(n_path, 0);

                let ret_val = xml_normalize_windows_path(path as *const XmlChar);
                desret_xml_char_ptr(ret_val);
                des_const_xml_char_ptr(n_path, path, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlNormalizeWindowsPath",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlNormalizeWindowsPath()"
                    );
                    eprintln!(" {}", n_path);
                }
            }
        }
    }

    #[test]
    fn test_xml_output_buffer_flush() {
        #[cfg(feature = "output")]
        unsafe {
            let mut leaks = 0;

            for n_out in 0..GEN_NB_XML_OUTPUT_BUFFER_PTR {
                let mem_base = xml_mem_blocks();
                let out = gen_xml_output_buffer_ptr(n_out, 0);

                let ret_val = xml_output_buffer_flush(out);
                desret_int(ret_val);
                des_xml_output_buffer_ptr(n_out, out, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlOutputBufferFlush",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlOutputBufferFlush()"
                    );
                    eprintln!(" {}", n_out);
                }
            }
        }
    }

    #[test]
    fn test_xml_output_buffer_get_content() {
        #[cfg(feature = "output")]
        unsafe {
            let mut leaks = 0;

            for n_out in 0..GEN_NB_XML_OUTPUT_BUFFER_PTR {
                let mem_base = xml_mem_blocks();
                let out = gen_xml_output_buffer_ptr(n_out, 0);

                let ret_val = xml_output_buffer_get_content(out);
                desret_const_xml_char_ptr(ret_val);
                des_xml_output_buffer_ptr(n_out, out, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlOutputBufferGetContent",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlOutputBufferGetContent()"
                    );
                    eprintln!(" {}", n_out);
                }
            }
        }
    }

    #[test]
    fn test_xml_output_buffer_get_size() {

        /* missing type support */
    }

    // #[test]
    // fn test_xml_output_buffer_write() {
    //     #[cfg(feature = "output")]
    //     unsafe {
    //         let mut leaks = 0;

    //         for n_out in 0..GEN_NB_XML_OUTPUT_BUFFER_PTR {
    //             for n_len in 0..GEN_NB_INT {
    //                 for n_buf in 0..GEN_NB_CONST_CHAR_PTR {
    //                     let mem_base = xml_mem_blocks();
    //                     let out = gen_xml_output_buffer_ptr(n_out, 0);
    //                     let mut len = gen_int(n_len, 1);
    //                     let buf = gen_const_char_ptr(n_buf, 2);
    //                     if !buf.is_null() && len > xml_strlen(buf as _) {
    //                         len = 0;
    //                     }

    //                     let ret_val = xml_output_buffer_write(out, len, buf);
    //                     desret_int(ret_val);
    //                     des_xml_output_buffer_ptr(n_out, out, 0);
    //                     des_int(n_len, len, 1);
    //                     des_const_char_ptr(n_buf, buf, 2);
    //                     reset_last_error();
    //                     if mem_base != xml_mem_blocks() {
    //                         leaks += 1;
    //                         eprint!(
    //                             "Leak of {} blocks found in xmlOutputBufferWrite",
    //                             xml_mem_blocks() - mem_base
    //                         );
    //                         assert!(
    //                             leaks == 0,
    //                             "{leaks} Leaks are found in xmlOutputBufferWrite()"
    //                         );
    //                         eprint!(" {}", n_out);
    //                         eprint!(" {}", n_len);
    //                         eprintln!(" {}", n_buf);
    //                     }
    //                 }
    //             }
    //         }
    //     }
    // }

    #[test]
    fn test_xml_output_buffer_write_escape() {

        /* missing type support */
    }

    #[test]
    fn test_xml_output_buffer_write_string() {
        #[cfg(feature = "output")]
        unsafe {
            let mut leaks = 0;

            for n_out in 0..GEN_NB_XML_OUTPUT_BUFFER_PTR {
                for n_str in 0..GEN_NB_CONST_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let out = gen_xml_output_buffer_ptr(n_out, 0);
                    let str = gen_const_char_ptr(n_str, 1);

                    let ret_val = xml_output_buffer_write_string(out, str);
                    desret_int(ret_val);
                    des_xml_output_buffer_ptr(n_out, out, 0);
                    des_const_char_ptr(n_str, str, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlOutputBufferWriteString",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlOutputBufferWriteString()"
                        );
                        eprint!(" {}", n_out);
                        eprintln!(" {}", n_str);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_parser_get_directory() {

        /* missing type support */
    }

    // #[test]
    // fn test_xml_parser_input_buffer_grow() {
    //     unsafe {
    //         let mut leaks = 0;

    //         for n_in in 0..GEN_NB_XML_PARSER_INPUT_BUFFER_PTR {
    //             for n_len in 0..GEN_NB_INT {
    //                 let mem_base = xml_mem_blocks();
    //                 let input = gen_xml_parser_input_buffer_ptr(n_in, 0);
    //                 let len = gen_int(n_len, 1);

    //                 let ret_val = xml_parser_input_buffer_grow(input, len);
    //                 desret_int(ret_val);
    //                 des_xml_parser_input_buffer_ptr(n_in, input, 0);
    //                 des_int(n_len, len, 1);
    //                 reset_last_error();
    //                 if mem_base != xml_mem_blocks() {
    //                     leaks += 1;
    //                     eprint!(
    //                         "Leak of {} blocks found in xmlParserInputBufferGrow",
    //                         xml_mem_blocks() - mem_base
    //                     );
    //                     assert!(
    //                         leaks == 0,
    //                         "{leaks} Leaks are found in xmlParserInputBufferGrow()"
    //                     );
    //                     eprint!(" {}", n_in);
    //                     eprintln!(" {}", n_len);
    //                 }
    //             }
    //         }
    //     }
    // }

    // #[test]
    // fn test_xml_parser_input_buffer_push() {
    //     unsafe {
    //         let mut leaks = 0;

    //         for n_in in 0..GEN_NB_XML_PARSER_INPUT_BUFFER_PTR {
    //             for n_len in 0..GEN_NB_INT {
    //                 for n_buf in 0..GEN_NB_CONST_CHAR_PTR {
    //                     let mem_base = xml_mem_blocks();
    //                     let input = gen_xml_parser_input_buffer_ptr(n_in, 0);
    //                     let mut len = gen_int(n_len, 1);
    //                     let buf = gen_const_char_ptr(n_buf, 2);
    //                     if !buf.is_null() && len > xml_strlen(buf as _) {
    //                         len = 0;
    //                     }

    //                     let ret_val = xml_parser_input_buffer_push(input, len, buf);
    //                     desret_int(ret_val);
    //                     des_xml_parser_input_buffer_ptr(n_in, input, 0);
    //                     des_int(n_len, len, 1);
    //                     des_const_char_ptr(n_buf, buf, 2);
    //                     reset_last_error();
    //                     if mem_base != xml_mem_blocks() {
    //                         leaks += 1;
    //                         eprint!(
    //                             "Leak of {} blocks found in xmlParserInputBufferPush",
    //                             xml_mem_blocks() - mem_base
    //                         );
    //                         assert!(
    //                             leaks == 0,
    //                             "{leaks} Leaks are found in xmlParserInputBufferPush()"
    //                         );
    //                         eprint!(" {}", n_in);
    //                         eprint!(" {}", n_len);
    //                         eprintln!(" {}", n_buf);
    //                     }
    //                 }
    //             }
    //         }
    //     }
    // }

    // #[test]
    // fn test_xml_parser_input_buffer_read() {
    //     unsafe {
    //         let mut leaks = 0;

    //         for n_in in 0..GEN_NB_XML_PARSER_INPUT_BUFFER_PTR {
    //             for n_len in 0..GEN_NB_INT {
    //                 let mem_base = xml_mem_blocks();
    //                 let input = gen_xml_parser_input_buffer_ptr(n_in, 0);
    //                 let len = gen_int(n_len, 1);

    //                 let ret_val = xml_parser_input_buffer_read(input, len);
    //                 desret_int(ret_val);
    //                 des_xml_parser_input_buffer_ptr(n_in, input, 0);
    //                 des_int(n_len, len, 1);
    //                 reset_last_error();
    //                 if mem_base != xml_mem_blocks() {
    //                     leaks += 1;
    //                     eprint!(
    //                         "Leak of {} blocks found in xmlParserInputBufferRead",
    //                         xml_mem_blocks() - mem_base
    //                     );
    //                     assert!(
    //                         leaks == 0,
    //                         "{leaks} Leaks are found in xmlParserInputBufferRead()"
    //                     );
    //                     eprint!(" {}", n_in);
    //                     eprintln!(" {}", n_len);
    //                 }
    //             }
    //         }
    //     }
    // }

    #[test]
    fn test_xml_pop_input_callbacks() {
        unsafe {
            let mut leaks = 0;
            let mem_base = xml_mem_blocks();

            let ret_val = xml_pop_input_callbacks();
            desret_int(ret_val);
            reset_last_error();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in xmlPopInputCallbacks",
                    xml_mem_blocks() - mem_base
                );
                assert!(
                    leaks == 0,
                    "{leaks} Leaks are found in xmlPopInputCallbacks()"
                );
            }
        }
    }

    #[test]
    fn test_xml_pop_output_callbacks() {
        #[cfg(feature = "output")]
        unsafe {
            let mut leaks = 0;

            let mem_base = xml_mem_blocks();

            let ret_val = xml_pop_output_callbacks();
            desret_int(ret_val);
            reset_last_error();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in xmlPopOutputCallbacks",
                    xml_mem_blocks() - mem_base
                );
                assert!(
                    leaks == 0,
                    "{leaks} Leaks are found in xmlPopOutputCallbacks()"
                );
            }
        }
    }

    #[test]
    fn test_xml_register_default_input_callbacks() {
        unsafe {
            let mut leaks = 0;
            let mem_base = xml_mem_blocks();

            xml_register_default_input_callbacks();
            reset_last_error();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in xmlRegisterDefaultInputCallbacks",
                    xml_mem_blocks() - mem_base
                );
                assert!(
                    leaks == 0,
                    "{leaks} Leaks are found in xmlRegisterDefaultInputCallbacks()"
                );
            }
        }
    }

    #[test]
    fn test_xml_register_default_output_callbacks() {
        #[cfg(feature = "output")]
        unsafe {
            let mut leaks = 0;

            let mem_base = xml_mem_blocks();

            xml_register_default_output_callbacks();
            reset_last_error();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in xmlRegisterDefaultOutputCallbacks",
                    xml_mem_blocks() - mem_base
                );
                assert!(
                    leaks == 0,
                    "{leaks} Leaks are found in xmlRegisterDefaultOutputCallbacks()"
                );
            }
        }
    }

    #[test]
    fn test_xml_register_httppost_callbacks() {
        #[cfg(all(feature = "output", feature = "http"))]
        unsafe {
            let mut leaks = 0;

            let mem_base = xml_mem_blocks();

            xml_register_http_post_callbacks();
            reset_last_error();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in xmlRegisterHTTPPostCallbacks",
                    xml_mem_blocks() - mem_base
                );
                assert!(
                    leaks == 0,
                    "{leaks} Leaks are found in xmlRegisterHTTPPostCallbacks()"
                );
            }
        }
    }
}
