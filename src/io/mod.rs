//! Provide methods and data structures for handling I/O actions.
//!
//! This module is based on `libxml/xmlIO.h`, `xmlIO.c`, and so on in `libxml2-v2.11.8`.  
//! Please refer to original libxml2 documents also.

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

mod input;
#[cfg(feature = "libxml_output")]
mod output;

#[cfg(feature = "libxml_output")]
use std::io::Write;
use std::{
    borrow::Cow,
    env::current_dir,
    fs::{File, metadata},
    io::{self, ErrorKind, Read, stdin},
    path::{Path, PathBuf},
    ptr::null_mut,
};

use libc::{
    __errno_location, EACCES, EADDRINUSE, EAFNOSUPPORT, EAGAIN, EALREADY, EBADF, EBADMSG, EBUSY,
    ECANCELED, ECHILD, ECONNREFUSED, EDEADLK, EDOM, EEXIST, EFAULT, EFBIG, EINPROGRESS, EINTR,
    EINVAL, EIO, EISCONN, EISDIR, EMFILE, EMLINK, EMSGSIZE, ENAMETOOLONG, ENFILE, ENODEV, ENOENT,
    ENOEXEC, ENOLCK, ENOMEM, ENOSPC, ENOSYS, ENOTDIR, ENOTEMPTY, ENOTSOCK, ENOTSUP, ENOTTY, ENXIO,
    EPERM, EPIPE, ERANGE, EROFS, ESPIPE, ESRCH, ETIMEDOUT, EXDEV,
};
use url::Url;

use crate::{
    encoding::find_encoding_handler,
    error::{__xml_simple_error, __xml_simple_oom_error, XmlErrorDomain, XmlParserErrors},
    nanohttp::XmlNanoHTTPCtxt,
    parser::{__xml_err_encoding, XmlParserCtxt, XmlParserCtxtPtr, XmlParserInput},
    uri::canonic_path,
};

pub use input::*;
#[cfg(feature = "libxml_output")]
pub use output::*;

///  Handle an out of memory condition
#[doc(alias = "xmlIOErrMemory")]
pub(crate) fn xml_ioerr_memory(extra: &str) {
    __xml_simple_oom_error(XmlErrorDomain::XmlFromIO, None, Some(extra));
}

const IOERR: &[&str] = &[
    "Unknown IO error",                    /* UNKNOWN */
    "Permission denied",                   /* EACCES */
    "Resource temporarily unavailable",    /* EAGAIN */
    "Bad file descriptor",                 /* EBADF */
    "Bad message",                         /* EBADMSG */
    "Resource busy",                       /* EBUSY */
    "Operation canceled",                  /* ECANCELED */
    "No child processes",                  /* ECHILD */
    "Resource deadlock avoided",           /* EDEADLK */
    "Domain error",                        /* EDOM */
    "File exists",                         /* EEXIST */
    "Bad address",                         /* EFAULT */
    "File too large",                      /* EFBIG */
    "Operation in progress",               /* EINPROGRESS */
    "Interrupted function call",           /* EINTR */
    "Invalid argument",                    /* EINVAL */
    "Input/output error",                  /* EIO */
    "Is a directory",                      /* EISDIR */
    "Too many open files",                 /* EMFILE */
    "Too many links",                      /* EMLINK */
    "Inappropriate message buffer length", /* EMSGSIZE */
    "Filename too long",                   /* ENAMETOOLONG */
    "Too many open files in system",       /* ENFILE */
    "No such device",                      /* ENODEV */
    "No such file or directory",           /* ENOENT */
    "Exec format error",                   /* ENOEXEC */
    "No locks available",                  /* ENOLCK */
    "Not enough space",                    /* ENOMEM */
    "No space left on device",             /* ENOSPC */
    "Function not implemented",            /* ENOSYS */
    "Not a directory",                     /* ENOTDIR */
    "Directory not empty",                 /* ENOTEMPTY */
    "Not supported",                       /* ENOTSUP */
    "Inappropriate I/O control operation", /* ENOTTY */
    "No such device or address",           /* ENXIO */
    "Operation not permitted",             /* EPERM */
    "Broken pipe",                         /* EPIPE */
    "Result too large",                    /* ERANGE */
    "Read-only file system",               /* EROFS */
    "Invalid seek",                        /* ESPIPE */
    "No such process",                     /* ESRCH */
    "Operation timed out",                 /* ETIMEDOUT */
    "Improper link",                       /* EXDEV */
    "Attempt to load network entity %s",   /* XML_IO_NETWORK_ATTEMPT */
    "encoder error",                       /* XML_IO_ENCODER */
    "flush error",
    "write error",
    "no input",
    "buffer full",
    "loading error",
    "not a socket",           /* ENOTSOCK */
    "already connected",      /* EISCONN */
    "connection refused",     /* ECONNREFUSED */
    "unreachable network",    /* ENETUNREACH */
    "address in use",         /* EADDRINUSE */
    "already in use",         /* EALREADY */
    "unknown address family", /* EAFNOSUPPORT */
];

/// Handle an I/O error
#[doc(alias = "__xmlIOErr")]
pub(crate) fn __xml_ioerr(domain: XmlErrorDomain, mut code: XmlParserErrors, extra: Option<&str>) {
    let mut idx: u32;
    let errno = unsafe { *__errno_location() };

    if code == XmlParserErrors::XmlErrOK {
        if errno == 0 {
            code = XmlParserErrors::XmlErrOK;
        } else if errno == EACCES {
            code = XmlParserErrors::XmlIOEACCES;
        } else if errno == EAGAIN {
            code = XmlParserErrors::XmlIOEAGAIN;
        } else if errno == EBADF {
            code = XmlParserErrors::XmlIOEBADF;
        } else if errno == EBADMSG {
            code = XmlParserErrors::XmlIOEBADMSG;
        } else if errno == EBUSY {
            code = XmlParserErrors::XmlIOEBUSY;
        } else if errno == ECANCELED {
            code = XmlParserErrors::XmlIOECANCELED;
        } else if errno == ECHILD {
            code = XmlParserErrors::XmlIOECHILD;
        } else if errno == EDEADLK {
            code = XmlParserErrors::XmlIOEDEADLK;
        } else if errno == EDOM {
            code = XmlParserErrors::XmlIOEDOM;
        } else if errno == EEXIST {
            code = XmlParserErrors::XmlIOEEXIST;
        } else if errno == EFAULT {
            code = XmlParserErrors::XmlIOEFAULT;
        } else if errno == EFBIG {
            code = XmlParserErrors::XmlIOEFBIG;
        } else if errno == EINPROGRESS {
            code = XmlParserErrors::XmlIOEINPROGRESS;
        } else if errno == EINTR {
            code = XmlParserErrors::XmlIOEINTR;
        } else if errno == EINVAL {
            code = XmlParserErrors::XmlIOEINVAL;
        } else if errno == EIO {
            code = XmlParserErrors::XmlIOEIO;
        } else if errno == EISDIR {
            code = XmlParserErrors::XmlIOEISDIR;
        } else if errno == EMFILE {
            code = XmlParserErrors::XmlIOEMFILE;
        } else if errno == EMLINK {
            code = XmlParserErrors::XmlIOEMLINK;
        } else if errno == EMSGSIZE {
            code = XmlParserErrors::XmlIOEMSGSIZE;
        } else if errno == ENAMETOOLONG {
            code = XmlParserErrors::XmlIOENAMETOOLONG;
        } else if errno == ENFILE {
            code = XmlParserErrors::XmlIOENFILE;
        } else if errno == ENODEV {
            code = XmlParserErrors::XmlIOENODEV;
        } else if errno == ENOENT {
            code = XmlParserErrors::XmlIOENOENT;
        } else if errno == ENOEXEC {
            code = XmlParserErrors::XmlIOENOEXEC;
        } else if errno == ENOLCK {
            code = XmlParserErrors::XmlIOENOLCK;
        } else if errno == ENOMEM {
            code = XmlParserErrors::XmlIOENOMEM;
        } else if errno == ENOSPC {
            code = XmlParserErrors::XmlIOENOSPC;
        } else if errno == ENOSYS {
            code = XmlParserErrors::XmlIOENOSYS;
        } else if errno == ENOTDIR {
            code = XmlParserErrors::XmlIOENOTDIR;
        } else if errno == ENOTEMPTY {
            code = XmlParserErrors::XmlIOENOTEMPTY;
        } else if errno == ENOTSUP {
            code = XmlParserErrors::XmlIOENOTSUP;
        } else if errno == ENOTTY {
            code = XmlParserErrors::XmlIOENOTTY;
        } else if errno == ENXIO {
            code = XmlParserErrors::XmlIOENXIO;
        } else if errno == EPERM {
            code = XmlParserErrors::XmlIOEPERM;
        } else if errno == EPIPE {
            code = XmlParserErrors::XmlIOEPIPE;
        } else if errno == ERANGE {
            code = XmlParserErrors::XmlIOERANGE;
        } else if errno == EROFS {
            code = XmlParserErrors::XmlIOEROFS;
        } else if errno == ESPIPE {
            code = XmlParserErrors::XmlIOESPIPE;
        } else if errno == ESRCH {
            code = XmlParserErrors::XmlIOESRCH;
        } else if errno == ETIMEDOUT {
            code = XmlParserErrors::XmlIOETIMEOUT;
        } else if errno == EXDEV {
            code = XmlParserErrors::XmlIOEXDEV;
        } else if errno == ENOTSOCK {
            code = XmlParserErrors::XmlIOENOTSOCK;
        } else if errno == EISCONN {
            code = XmlParserErrors::XmlIOEISCONN;
        } else if errno == ECONNREFUSED {
            code = XmlParserErrors::XmlIOECONNREFUSED;
        } else if errno == EADDRINUSE {
            code = XmlParserErrors::XmlIOEADDRINUSE;
        } else if errno == EALREADY {
            code = XmlParserErrors::XmlIOEALREADY;
        } else if errno == EAFNOSUPPORT {
            code = XmlParserErrors::XmlIOEAFNOSUPPORT;
        } else {
            code = XmlParserErrors::XmlIOUnknown;
        }
    }
    idx = 0;
    if code as i32 >= XmlParserErrors::XmlIOUnknown as i32 {
        idx = code as u32 - XmlParserErrors::XmlIOUnknown as u32;
    }
    if idx >= IOERR.len() as u32 {
        idx = 0;
    }

    let msg: Cow<'static, str> = match idx {
        43 => format!(
            "Attempt to load network entity {}",
            extra.expect("Internal Error")
        )
        .into(), /* XML_IO_NETWORK_ATTEMPT */
        index => IOERR[index as usize].into(),
    };

    __xml_simple_error!(domain, code, None, &msg);
}

/// Handle an I/O error
#[doc(alias = "xmlIOErr")]
pub(crate) fn xml_ioerr(code: XmlParserErrors, extra: Option<&str>) {
    __xml_ioerr(XmlErrorDomain::XmlFromIO, code, extra);
}

#[cfg(feature = "libxml_output")]
pub(crate) fn write_quoted<'a>(out: &mut (impl Write + 'a), s: &str) -> std::io::Result<()> {
    if s.contains('"') {
        if s.contains('\'') {
            // If `s` contains both single and double-quote, quote with double-quote
            // and escape inner double-quotes
            write!(out, "\"")?;
            let mut split = s.split('"');
            write!(out, "{}", split.next().unwrap())?;
            for chunk in split {
                write!(out, "&quot;{chunk}")?;
            }
            write!(out, "\"")?;
        } else {
            // If `s` contains only double-quote, quote with single-quote
            write!(out, "'{s}'")?;
        }
    } else {
        // If `s` does not contain double-quotes, quote with double-quote
        write!(out, "\"{s}\"")?;
    }
    Ok(())
}

const MINLEN: usize = 4000;

/// lookup the directory for that file
///
/// Returns a new allocated string containing the directory, or NULL.
#[doc(alias = "xmlParserGetDirectory")]
pub fn xml_parser_get_directory(filename: impl AsRef<Path>) -> Option<PathBuf> {
    fn _xml_parser_get_directory(filename: &Path) -> Option<PathBuf> {
        filename
            .parent()
            .map(|p| p.to_path_buf())
            .or_else(|| current_dir().ok())
    }
    _xml_parser_get_directory(filename.as_ref())
}

/// Take a block of UTF-8 chars in and escape them.
/// Returns 0 if success, or -1 otherwise
///
/// The value of `inlen` after return is the number of octets consumed
/// if the return value is positive, else unpredictable.
/// The value of `outlen` after return is the number of octets consumed.
#[doc(alias = "xmlEscapeContent")]
#[cfg(feature = "libxml_output")]
fn xml_escape_content(input: &str, output: &mut String) -> i32 {
    for input in input.chars() {
        match input {
            '<' => output.push_str("&lt;"),
            '>' => output.push_str("&gt;"),
            '&' => output.push_str("&amp;"),
            '\r' => output.push_str("&#13;"),
            c => output.push(c),
        }
    }
    0
}

/// Handle a resource access error
#[doc(alias = "__xmlLoaderErr")]
macro_rules! __xml_loader_err {
    ($ctx:expr, $msg:literal) => {
        $crate::io::__xml_loader_err!(@inner $ctx, $msg, None);
    };
    ($ctx:expr, $msg:literal, $filename:expr) => {
        let msg = format!($msg, $filename);
        $crate::io::__xml_loader_err!(@inner $ctx, &msg, Some($filename.to_owned().into()));
    };
    (@inner $ctx:expr, $msg:expr, $filename:expr) => {
        use $crate::{
            error::XmlErrorLevel,
            globals::{GenericError, StructuredError},
            parser::XML_SAX2_MAGIC,
        };
        let mut schannel: Option<StructuredError> = None;
        let mut channel: Option<GenericError> = None;
        let mut data = None;
        let mut level = XmlErrorLevel::XmlErrError;

        if $ctx.disable_sax == 0
            || !matches!($ctx.instate, $crate::parser::XmlParserInputState::XmlParserEOF)
        {
            if let Some(sax) = $ctx.sax.as_deref_mut() {
                if $ctx.validate {
                    channel = sax.error;
                    level = XmlErrorLevel::XmlErrError;
                } else {
                    channel = sax.warning;
                    level = XmlErrorLevel::XmlErrWarning;
                }
                if sax.initialized == XML_SAX2_MAGIC as u32 {
                    schannel = sax.serror;
                }
                data = $ctx.user_data.clone();
            }
            $crate::error::__xml_raise_error!(
                schannel,
                channel,
                data,
                $ctx as XmlParserCtxtPtr as _,
                None,
                $crate::error::XmlErrorDomain::XmlFromIO,
                $crate::error::XmlParserErrors::XmlIOLoadError,
                level,
                None,
                0,
                $filename,
                None,
                None,
                0,
                0,
                Some($msg),
            );
        }
    };
}
pub(crate) use __xml_loader_err;

/// Check an input in case it was created from an HTTP stream,
/// in that case it will handle encoding and update of the base URL in case of redirection.  
/// It also checks for HTTP errors in which case the input is cleanly freed up
/// and an appropriate error is raised in context
///
/// Returns the input or NULL in case of HTTP error.
#[doc(alias = "xmlCheckHTTPInput")]
pub fn xml_check_http_input(
    ctxt: &mut XmlParserCtxt,
    mut ret: Option<XmlParserInput>,
) -> Option<XmlParserInput> {
    #[cfg(feature = "http")]
    {
        if ret.is_some()
            && ret.as_ref().unwrap().buf.is_some()
            && ret
                .as_mut()
                .unwrap()
                .buf
                .as_mut()
                .unwrap()
                .nanohttp_context()
                .is_some()
        {
            let buf = ret.as_mut().unwrap().buf.as_mut().unwrap();
            let context = buf.nanohttp_context().unwrap();
            let code = context.return_code();
            if code >= 400 {
                // fatal error
                if let Some(filename) = ret.as_ref().unwrap().filename.as_deref() {
                    __xml_loader_err!(ctxt, "failed to load HTTP resource \"{}\"\n", filename);
                } else {
                    __xml_loader_err!(ctxt, "failed to load HTTP resource\n");
                }

                return None;
            }
            let is_mime_xml = context
                .mime_type()
                .filter(|mime| mime.contains("/xml") || mime.contains("+xml"))
                .is_some();
            let encoding = context.encoding().map(|encoding| encoding.to_owned());
            let redirection = context
                .redirection()
                .map(|redirection| redirection.to_owned());
            if is_mime_xml {
                if let Some(encoding) = encoding {
                    if let Some(handler) = find_encoding_handler(&encoding) {
                        ctxt.switch_input_encoding(ret.as_mut().unwrap(), handler);
                    } else {
                        __xml_err_encoding!(
                            &mut *ctxt,
                            XmlParserErrors::XmlErrUnknownEncoding,
                            "Unknown encoding {}",
                            encoding
                        );
                    }
                    if ret.as_mut().unwrap().encoding.is_none() {
                        ret.as_mut().unwrap().encoding = Some(encoding);
                    }
                }
            }
            if let Some(redir) = redirection {
                ret.as_mut().unwrap().directory = None;
                ret.as_mut().unwrap().filename = Some(redir);
            }
        }
    }
    ret
}

/// This function is obsolete.
/// Please see `xmlURIFromPath` in uri.c for a better solution.
///
/// Returns a canonicalized version of the path
#[doc(alias = "xmlNormalizeWindowsPath")]
pub fn xml_normalize_windows_path(path: &str) -> Cow<'_, str> {
    canonic_path(path)
}

/// function checks to see if `path` is a valid source (file, socket...) for XML.
///
/// if stat is not available on the target machine, returns 1.  
/// if stat fails, returns 0 (if calling stat on the filename fails, it can't be right).  
/// if stat succeeds and the file is a directory, returns 2.  
/// otherwise returns 1.
#[doc(alias = "xmlCheckFilename")]
pub fn xml_check_filename(path: impl AsRef<Path>) -> i32 {
    fn check_filename(path: &Path) -> i32 {
        match metadata(path) {
            Ok(meta) => {
                if meta.is_dir() {
                    2
                } else {
                    1
                }
            }
            _ => 0,
        }
    }
    check_filename(path.as_ref())
}

#[derive(Debug, Clone, Copy)]
pub struct DefaultFileIOCallbacks;

impl XmlInputCallback for DefaultFileIOCallbacks {
    fn is_match(&self, _filename: &str) -> bool {
        true
    }

    fn open(&mut self, filename: &str) -> io::Result<Box<dyn Read>> {
        if filename == "-" {
            return Ok(Box::new(stdin()));
        }

        let filename = if let Ok(Ok(name)) = Url::parse(filename).map(|url| url.to_file_path()) {
            Cow::Owned(name)
        } else {
            Cow::Borrowed(Path::new(filename))
        };

        if xml_check_filename(filename.as_ref()) == 0 {
            return Err(io::Error::new(
                ErrorKind::NotFound,
                format!("{} is not found", filename.display()),
            ));
        }

        File::open(filename.as_ref())
            .inspect_err(|_| {
                xml_ioerr(
                    XmlParserErrors::XmlErrOK,
                    Some(filename.to_string_lossy().as_ref()),
                )
            })
            .map(|file| Box::new(file) as Box<dyn Read>)
    }
}

#[cfg(feature = "http")]
#[derive(Debug, Clone, Copy)]
pub struct DefaultHTTPIOCallbacks {
    write_method: &'static str,
}

#[cfg(feature = "http")]
impl XmlInputCallback for DefaultHTTPIOCallbacks {
    fn is_match(&self, filename: &str) -> bool {
        filename.starts_with("http://")
    }

    fn open(&mut self, filename: &str) -> io::Result<Box<dyn Read>> {
        XmlNanoHTTPCtxt::open(filename, &mut None).map(|ctxt| Box::new(ctxt) as Box<dyn Read>)
    }
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks};

    use super::*;

    #[test]
    fn test_xml_cleanup_input_callbacks() {
        unsafe {
            let mut leaks = 0;
            let mem_base = xml_mem_blocks();

            cleanup_input_callbacks();
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
        #[cfg(feature = "libxml_output")]
        unsafe {
            let mut leaks = 0;

            let mem_base = xml_mem_blocks();

            cleanup_output_callbacks();
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
    fn test_xml_register_default_input_callbacks() {
        unsafe {
            let mut leaks = 0;
            let mem_base = xml_mem_blocks();

            register_default_input_callbacks();
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
        #[cfg(feature = "libxml_output")]
        unsafe {
            let mut leaks = 0;

            let mem_base = xml_mem_blocks();

            register_default_output_callbacks();
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
        #[cfg(all(feature = "libxml_output", feature = "http"))]
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
