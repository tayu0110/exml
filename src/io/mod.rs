//! Provide methods and data structures for handling I/O actions.  
//! This module is based on `libxml/xmlIO.h`, `xmlIO.c`, and so on in `libxml2-v2.11.8`.
//!
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

use std::{
    borrow::Cow,
    ffi::{c_char, c_int, c_uint, c_void, CStr, CString},
    fs::{metadata, File},
    io::{self, stdin, ErrorKind, Read},
    path::Path,
    ptr::{null, null_mut},
    sync::atomic::Ordering,
};

use libc::{
    __errno_location, getcwd, strlen, strncpy, EACCES, EADDRINUSE, EAFNOSUPPORT, EAGAIN, EALREADY,
    EBADF, EBADMSG, EBUSY, ECANCELED, ECHILD, ECONNREFUSED, EDEADLK, EDOM, EEXIST, EFAULT, EFBIG,
    EINPROGRESS, EINTR, EINVAL, EIO, EISCONN, EISDIR, EMFILE, EMLINK, EMSGSIZE, ENAMETOOLONG,
    ENFILE, ENODEV, ENOENT, ENOEXEC, ENOLCK, ENOMEM, ENOSPC, ENOSYS, ENOTDIR, ENOTEMPTY, ENOTSOCK,
    ENOTSUP, ENOTTY, ENXIO, EPERM, EPIPE, ERANGE, EROFS, ESPIPE, ESRCH, ETIMEDOUT, EXDEV,
};
use url::Url;

use crate::{
    __xml_raise_error,
    encoding::find_encoding_handler,
    error::{XmlErrorDomain, XmlErrorLevel, XmlParserErrors, __xml_simple_error},
    globals::{GenericError, StructuredError},
    libxml::{
        catalog::{
            xml_catalog_get_defaults, xml_catalog_local_resolve, xml_catalog_local_resolve_uri,
            xml_catalog_resolve, xml_catalog_resolve_uri, XmlCatalogAllow,
        },
        globals::{xml_free, xml_mem_strdup},
        nanoftp::{xml_nanoftp_close, xml_nanoftp_open, xml_nanoftp_read},
        parser::{
            XmlParserCtxtPtr, XmlParserInputPtr, XmlParserInputState, XmlParserOption,
            XML_SAX2_MAGIC,
        },
        parser_internals::{__xml_err_encoding, xml_free_input_stream, xml_new_input_from_file},
        xmlstring::{xml_strdup, xml_strncasecmp, XmlChar},
    },
    nanohttp::XmlNanoHTTPCtxt,
    uri::canonic_path,
};

pub use input::*;
#[cfg(feature = "libxml_output")]
pub use output::*;

///  Handle an out of memory condition
#[doc(alias = "xmlIOErrMemory")]
pub(crate) unsafe extern "C" fn xml_ioerr_memory(extra: *const c_char) {
    __xml_simple_error(
        XmlErrorDomain::XmlFromIO,
        XmlParserErrors::XmlErrNoMemory,
        null_mut(),
        null(),
        extra as _,
    );
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

/// Handle an I/O error
#[doc(alias = "__xmlIOErr")]
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

    __xml_simple_error(domain, code, null_mut(), IOERR[idx as usize], extra);
}

/// Handle an I/O error
#[doc(alias = "xmlIOErr")]
unsafe extern "C" fn xml_ioerr(code: XmlParserErrors, extra: *const c_char) {
    __xml_ioerr(XmlErrorDomain::XmlFromIO, code, extra);
}

const MINLEN: usize = 4000;

/// lookup the directory for that file
///
/// Returns a new allocated string containing the directory, or NULL.
#[doc(alias = "xmlParserGetDirectory")]
pub unsafe extern "C" fn xml_parser_get_directory(filename: *const c_char) -> *mut c_char {
    let mut ret: *mut c_char = null_mut();
    let mut dir: [c_char; 1024] = [0; 1024];
    let mut cur: *mut c_char;

    if !XML_INPUT_CALLBACK_INITIALIZED.load(Ordering::Relaxed) {
        register_default_input_callbacks();
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
        XmlParserErrors::XmlIOLoadError,
        level,
        None,
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

/// Check an input in case it was created from an HTTP stream,
/// in that case it will handle encoding and update of the base URL in case of redirection.  
/// It also checks for HTTP errors in which case the input is cleanly freed up
/// and an appropriate error is raised in context
///
/// Returns the input or NULL in case of HTTP error.
#[doc(alias = "xmlCheckHTTPInput")]
pub unsafe extern "C" fn xml_check_http_input(
    ctxt: XmlParserCtxtPtr,
    mut ret: XmlParserInputPtr,
) -> XmlParserInputPtr {
    #[cfg(feature = "http")]
    {
        if !ret.is_null() {
            if let Some(buf) = (*ret).buf.as_mut() {
                if let Some(context) = buf.borrow_mut().nanohttp_context() {
                    let code = context.return_code();
                    if code >= 400 {
                        /* fatal error */
                        if let Some(filename) = (*ret).filename.as_deref() {
                            let filename = CString::new(filename).unwrap();
                            __xml_loader_err(
                                ctxt as _,
                                c"failed to load HTTP resource \"%s\"\n".as_ptr() as _,
                                filename.as_ptr() as *const c_char,
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
                        if context
                            .mime_type()
                            .filter(|mime| mime.contains("/xml") || mime.contains("+xml"))
                            .is_some()
                        {
                            if let Some(encoding) = context.encoding() {
                                let enc = CString::new(encoding).unwrap();
                                if let Some(handler) = find_encoding_handler(encoding) {
                                    (*ctxt).switch_input_encoding(ret, handler);
                                } else {
                                    __xml_err_encoding(
                                        ctxt,
                                        XmlParserErrors::XmlErrUnknownEncoding,
                                        c"Unknown encoding %s".as_ptr() as _,
                                        enc.as_ptr() as *const u8,
                                        null(),
                                    );
                                }
                                if (*ret).encoding.is_none() {
                                    (*ret).encoding = Some(encoding.to_owned());
                                }
                            }
                        }
                        if let Some(redir) = context.redirection() {
                            (*ret).directory = None;
                            (*ret).filename = Some(redir.to_owned());
                        }
                    }
                }
            }
        }
    }
    ret
}

/// By default we don't load external entities, yet.
///
/// Returns a new allocated xmlParserInputPtr, or NULL.
#[doc(alias = "xmlDefaultExternalEntityLoader")]
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

    xml_check_filename(CStr::from_ptr(path).to_string_lossy().as_ref())
}

/// Resolves the URL and ID against the appropriate catalog.
/// This function is used by xmlDefaultExternalEntityLoader and
/// xmlNoNetExternalEntityLoader.
///
/// Returns a new allocated URL, or NULL.
#[doc(alias = "xmlResolveResourceFromCatalog")]
#[cfg(feature = "catalog")]
unsafe extern "C" fn xml_resolve_resource_from_catalog(
    url: *const c_char,
    id: *const c_char,
    ctxt: XmlParserCtxtPtr,
) -> *mut XmlChar {
    let mut resource: *mut XmlChar = null_mut();

    // If the resource doesn't exists as a file,
    // try to load it from the resource poc_inted in the catalogs
    let pref: XmlCatalogAllow = xml_catalog_get_defaults();

    if !matches!(pref, XmlCatalogAllow::None) && xml_no_net_exists(url) == 0 {
        // Do a local lookup
        if !ctxt.is_null()
            && !(*ctxt).catalogs.is_null()
            && matches!(pref, XmlCatalogAllow::All | XmlCatalogAllow::Document)
        {
            resource = xml_catalog_local_resolve((*ctxt).catalogs, id as _, url as _);
        }
        // Try a global lookup
        if resource.is_null() && matches!(pref, XmlCatalogAllow::All | XmlCatalogAllow::Global) {
            resource = xml_catalog_resolve(id as _, url as _);
        }
        if resource.is_null() && !url.is_null() {
            resource = xml_strdup(url as _);
        }

        // TODO: do an URI lookup on the reference
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

/// A specific entity loader disabling network accesses,
/// though still allowing local catalog accesses for resolution.
///
/// Returns a new allocated xmlParserInputPtr, or NULL.
#[doc(alias = "xmlNoNetExternalEntityLoader")]
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
        xml_ioerr(XmlParserErrors::XmlIONetworkAttempt, resource as _);
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

/// check if the URI matches an FTP one
///
/// Returns 1 if matches, 0 otherwise
#[doc(alias = "xmlIOFTPMatch")]
#[cfg(feature = "ftp")]
pub fn xml_io_ftp_match(filename: &str) -> c_int {
    filename.starts_with("ftp://") as i32
}

/// open an FTP I/O channel
///
/// Returns an I/O context or NULL in case of error
#[doc(alias = "xmlIOFTPOpen")]
#[cfg(feature = "ftp")]
pub unsafe fn xml_io_ftp_open(filename: &str) -> *mut c_void {
    let filename = CString::new(filename).unwrap();
    xml_nanoftp_open(filename.as_ptr())
}

/// Read `len` bytes to `buffer` from the I/O channel.
///
/// Returns the number of bytes written
#[doc(alias = "xmlIOFTPRead")]
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

/// Close an FTP I/O channel
///
/// Returns 0
#[doc(alias = "xmlIOFTPClose")]
#[cfg(feature = "ftp")]
pub unsafe extern "C" fn xml_io_ftp_close(context: *mut c_void) -> c_int {
    xml_nanoftp_close(context)
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
            .inspect_err(|_| unsafe {
                let path = CString::new(filename.to_string_lossy().as_ref()).unwrap();
                xml_ioerr(XmlParserErrors::XmlErrOK, path.as_ptr())
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
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

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
    fn test_xml_no_net_external_entity_loader() {
        let lock = TEST_CATALOG_LOCK.lock().unwrap();
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
        drop(lock);
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
