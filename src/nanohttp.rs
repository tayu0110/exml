//! Provide methods and data structures for handling HTTP.  
//! This module is based on `libxml/nanohttp.h`, `nanohttp.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    borrow::Cow,
    ffi::{c_char, c_int, c_uint, CStr, CString},
    mem::{forget, size_of, size_of_val, zeroed},
    os::raw::c_void,
    ptr::{addr_of_mut, null, null_mut},
    sync::atomic::{AtomicBool, AtomicI32, AtomicPtr, Ordering},
};

use libc::{
    __errno_location, addrinfo, close, connect, fcntl, freeaddrinfo, getaddrinfo, getsockopt,
    memcpy, memmove, memset, open, poll, pollfd, recv, send, snprintf, sockaddr, sockaddr_in,
    sockaddr_in6, socket, strcmp, strlen, strncmp, strtol, write, AF_INET, AF_INET6, EAGAIN,
    ECONNRESET, EINPROGRESS, EINTR, ESHUTDOWN, EWOULDBLOCK, F_GETFL, F_SETFL, IPPROTO_TCP, O_CREAT,
    O_NONBLOCK, O_WRONLY, PF_INET, PF_INET6, POLLIN, POLLOUT, SOCK_STREAM, SOL_SOCKET, SO_ERROR,
};
use url::{Host, Url};

use crate::{
    error::XmlErrorDomain,
    libxml::{
        globals::{xml_free, xml_malloc, xml_malloc_atomic, xml_mem_strdup, xml_realloc},
        xml_io::__xml_ioerr,
        xmlerror::XmlParserErrors,
        xmlstring::{xml_strncasecmp, xml_strndup, xml_strstr, XmlChar},
    },
    private::error::__xml_simple_error,
};

const XML_NANO_HTTP_MAX_REDIR: usize = 10;
const XML_NANO_HTTP_CHUNK: usize = 4096;
const XML_NANO_HTTP_CLOSED: usize = 0;
const XML_NANO_HTTP_WRITE: usize = 1;
const XML_NANO_HTTP_READ: usize = 2;
const XML_NANO_HTTP_NONE: usize = 4;

/**
 * A couple portability macros
 */
unsafe fn closesocket(s: i32) -> i32 {
    close(s)
}
type Socket = i32;
const INVALID_SOCKET: i32 = -1;

pub type XmlNanoHTTPCtxtPtr = *mut XmlNanoHTTPCtxt;
#[repr(C)]
pub struct XmlNanoHTTPCtxt {
    protocol: Option<Cow<'static, str>>, /* the protocol name */
    hostname: Option<Cow<'static, str>>, /* the host name */
    port: i32,                           /* the port */
    path: Option<Cow<'static, str>>,     /* the path within the URL */
    query: Option<Cow<'static, str>>,    /* the query string */
    fd: Socket,                          /* the file descriptor for the socket */
    state: i32,                          /* WRITE / READ / CLOSED */
    out: *mut c_char,                    /* buffer sent (zero terminated) */
    outptr: *mut c_char,                 /* index within the buffer sent */
    input: *mut c_char,                  /* the receiving buffer */
    content: *mut c_char,                /* the start of the content */
    inptr: *mut c_char,                  /* the next byte to read from network */
    inrptr: *mut c_char,                 /* the next byte to give back to the client */
    inlen: i32,                          /* len of the input buffer */
    last: i32,                           /* return code for last operation */
    return_value: i32,                   /* the protocol return value */
    version: i32,                        /* the protocol version */
    content_length: i32,                 /* specified content length from HTTP header */
    content_type: *mut c_char,           /* the MIME type for the input */
    location: Option<Cow<'static, str>>, /* the new URL in case of redirect */
    auth_header: *mut c_char,            /* contents of {WWW,Proxy}-Authenticate header */
    encoding: *mut c_char,               /* encoding extracted from the contentType */
    mime_type: *mut c_char,              /* Mime-Type extracted from the contentType */
                                         // #ifdef LIBXML_ZLIB_ENABLED
                                         //     z_stream *strm;	/* Zlib stream object */
                                         //     int usesGzip;	/* "Content-Encoding: gzip" was detected */
                                         // #endif
}

static INITIALIZED: AtomicBool = AtomicBool::new(false);
static PROXY: AtomicPtr<c_char> = AtomicPtr::new(null_mut()); /* the proxy name if any */
static PROXY_PORT: AtomicI32 = AtomicI32::new(0); /* the proxy port if any */
static TIMEOUT: c_uint = 60; /* the select() timeout in seconds */

/**
 * xmlNanoHTTPInit:
 *
 * Initialize the HTTP protocol layer.
 * Currently it just checks for proxy information
 */
pub unsafe extern "C" fn xml_nanohttp_init() {
    if INITIALIZED.load(Ordering::Acquire) {
        return;
    }

    if PROXY.load(Ordering::Relaxed).is_null() {
        PROXY_PORT.store(80, Ordering::Relaxed);
        if std::env::var("no_proxy")
            .ok()
            .filter(|e| e == "*")
            .is_some()
        {
            INITIALIZED.store(true, Ordering::Release);
        }
        if let Ok(env) = std::env::var("http_proxy") {
            xml_nanohttp_scan_proxy(&env);
            INITIALIZED.store(true, Ordering::Release);
        }
        if let Ok(env) = std::env::var("HTTP_PROXY") {
            xml_nanohttp_scan_proxy(&env);
            INITIALIZED.store(true, Ordering::Release);
        }
    }

    INITIALIZED.store(true, Ordering::Release);
}

/**
 * xmlNanoHTTPCleanup:
 *
 * Cleanup the HTTP protocol layer.
 */
pub unsafe extern "C" fn xml_nanohttp_cleanup() {
    let p = PROXY.load(Ordering::Acquire);
    if !p.is_null() {
        xml_free(p as _);
        PROXY.store(null_mut(), Ordering::Release);
    }
    INITIALIZED.store(false, Ordering::Relaxed);
}

/**
 * xmlNanoHTTPScanProxy:
 * @URL:  The proxy URL used to initialize the proxy context
 *
 * (Re)Initialize the HTTP Proxy context by parsing the URL and finding
 * the protocol host port it indicates.
 * Should be like http://myproxy/ or http://myproxy:3128/
 * A NULL URL cleans up proxy information.
 */
pub unsafe fn xml_nanohttp_scan_proxy(url: &str) {
    let p = PROXY.load(Ordering::Acquire);
    if !p.is_null() {
        xml_free(p as _);
        PROXY.store(null_mut(), Ordering::Release);
    }
    PROXY_PORT.store(0, Ordering::Relaxed);

    let Some(uri) = Url::parse(url)
        .ok()
        .filter(|uri| uri.scheme() == "http" && uri.host_str().is_some())
    else {
        __xml_ioerr(
            XmlErrorDomain::XmlFromHTTP,
            XmlParserErrors::XmlHttpUrlSyntax,
            c"Syntax Error\n".as_ptr() as _,
        );
        return;
    };

    let host = uri.host_str().unwrap();
    let host = CString::new(host).unwrap();
    PROXY.store(xml_mem_strdup(host.as_ptr() as _) as _, Ordering::Release);
    if let Some(port) = uri.port() {
        PROXY_PORT.store(port as i32, Ordering::Release);
    }
}

/**
 * xmlHTTPErrMemory:
 * @extra:  extra information
 *
 * Handle an out of memory condition
 */
unsafe extern "C" fn xml_http_err_memory(extra: *const c_char) {
    __xml_simple_error(
        XmlErrorDomain::XmlFromHTTP,
        XmlParserErrors::XmlErrNoMemory,
        null_mut(),
        null(),
        extra,
    );
}

/**
 * xmlNanoHTTPRecv:
 * @ctxt:  an HTTP context
 *
 * Read information coming from the HTTP connection.
 * This is a blocking call (but it blocks in select(), not read()).
 *
 * Returns the number of byte read or -1 in case of error.
 */

unsafe extern "C" fn xml_nanohttp_recv(ctxt: XmlNanoHTTPCtxtPtr) -> c_int {
    let mut p: pollfd = unsafe { zeroed() };

    while (*ctxt).state & XML_NANO_HTTP_READ as i32 != 0 {
        if (*ctxt).input.is_null() {
            (*ctxt).input = xml_malloc_atomic(65000) as _;
            if (*ctxt).input.is_null() {
                xml_http_err_memory(c"allocating input".as_ptr() as _);
                (*ctxt).last = -1;
                return -1;
            }
            (*ctxt).inlen = 65000;
            (*ctxt).inptr = (*ctxt).input;
            (*ctxt).content = (*ctxt).input;
            (*ctxt).inrptr = (*ctxt).input;
        }
        if (*ctxt).inrptr > (*ctxt).input.add(XML_NANO_HTTP_CHUNK) {
            let delta: c_int = (*ctxt).inrptr.offset_from((*ctxt).input) as _;
            let len: c_int = (*ctxt).inptr.offset_from((*ctxt).inrptr) as _;

            memmove((*ctxt).input as _, (*ctxt).inrptr as _, len as _);
            (*ctxt).inrptr = (*ctxt).inrptr.sub(delta as usize);
            (*ctxt).content = (*ctxt).content.sub(delta as usize);
            (*ctxt).inptr = (*ctxt).inptr.sub(delta as usize);
        }
        if (*ctxt).input.add((*ctxt).inlen as usize) < (*ctxt).inptr.add(XML_NANO_HTTP_CHUNK) {
            let d_inptr: c_int = (*ctxt).inptr.offset_from((*ctxt).input) as _;
            let d_content: c_int = (*ctxt).content.offset_from((*ctxt).input) as _;
            let d_inrptr: c_int = (*ctxt).inrptr.offset_from((*ctxt).input) as _;
            let tmp_ptr: *mut c_char = (*ctxt).input;

            (*ctxt).inlen *= 2;
            (*ctxt).input = xml_realloc(tmp_ptr as _, (*ctxt).inlen as usize) as _;
            if (*ctxt).input.is_null() {
                xml_http_err_memory(c"allocating input buffer".as_ptr() as _);
                xml_free(tmp_ptr as _);
                (*ctxt).last = -1;
                return -1;
            }
            (*ctxt).inptr = (*ctxt).input.add(d_inptr as usize);
            (*ctxt).content = (*ctxt).input.add(d_content as usize);
            (*ctxt).inrptr = (*ctxt).input.add(d_inrptr as usize);
        }
        (*ctxt).last = recv((*ctxt).fd, (*ctxt).inptr as _, XML_NANO_HTTP_CHUNK, 0) as _;
        if (*ctxt).last > 0 {
            (*ctxt).inptr = (*ctxt).inptr.add((*ctxt).last as _);
            return (*ctxt).last;
        }
        if (*ctxt).last == 0 {
            return 0;
        }
        if (*ctxt).last == -1 {
            match *__errno_location() {
                EINPROGRESS | EWOULDBLOCK => {
                    // This pattern covers `EAGAIN`.
                }
                ECONNRESET | ESHUTDOWN => {
                    return 0;
                }
                _ => {
                    __xml_ioerr(
                        XmlErrorDomain::XmlFromHTTP,
                        XmlParserErrors::default(),
                        c"recv failed\n".as_ptr() as _,
                    );
                    return -1;
                }
            }
        }
        p.fd = (*ctxt).fd;
        p.events = POLLIN;
        if poll(addr_of_mut!(p), 1, TIMEOUT as i32 * 1000) < 1 && *__errno_location() != EINTR {
            return 0;
        }
    }
    0
}

/**
 * xmlNanoHTTPFetchContent:
 * @ctx:  the HTTP context
 * @ptr:  pointer to set to the content buffer.
 * @len:  integer pointer to hold the length of the content
 *
 * Check if all the content was read
 *
 * Returns 0 if all the content was read and available, returns
 * -1 if received content length was less than specified or an error
 * occurred.
 */
unsafe extern "C" fn xml_nanohttp_fetch_content(
    ctx: *mut c_void,
    mut ptr: *mut *mut c_char,
    mut len: *mut c_int,
) -> c_int {
    let ctxt: XmlNanoHTTPCtxtPtr = ctx as XmlNanoHTTPCtxtPtr;

    let mut rc: c_int = 0;
    let mut cur_lgth: c_int;
    let mut rcvd_lgth: c_int;
    let mut dummy_int: c_int = 0;
    let mut dummy_ptr: *mut c_char = null_mut();

    /*  Dummy up return input parameters if not provided  */

    if len.is_null() {
        len = addr_of_mut!(dummy_int);
    }

    if ptr.is_null() {
        ptr = addr_of_mut!(dummy_ptr);
    }

    /*  But can't work without the context pointer  */

    if (ctxt.is_null()) || ((*ctxt).content.is_null()) {
        *len = 0;
        *ptr = null_mut();
        return -1;
    }

    rcvd_lgth = (*ctxt).inptr.offset_from((*ctxt).content) as _;

    while {
        cur_lgth = xml_nanohttp_recv(ctxt);
        cur_lgth > 0
    } {
        rcvd_lgth += cur_lgth;
        if (*ctxt).content_length > 0 && rcvd_lgth >= (*ctxt).content_length {
            break;
        }
    }

    *ptr = (*ctxt).content;
    *len = rcvd_lgth;

    if ((*ctxt).content_length > 0 && rcvd_lgth < (*ctxt).content_length) || rcvd_lgth == 0 {
        rc = -1;
    }

    rc
}

/**
 * xmlNanoHTTPFetch:
 * @URL:  The URL to load
 * @filename:  the filename where the content should be saved
 * @contentType:  if available the Content-Type information will be
 *                returned at that location
 *
 * This function try to fetch the indicated resource via HTTP GET
 * and save it's content in the file.
 *
 * Returns -1 in case of failure, 0 in case of success. The contentType,
 *     if provided must be freed by the caller
 */
pub unsafe extern "C" fn xml_nanohttp_fetch(
    url: *const c_char,
    filename: *const c_char,
    content_type: *mut *mut c_char,
) -> c_int {
    let mut buf: *mut c_char = null_mut();
    let fd: c_int;
    let mut len: c_int = 0;
    let mut ret: c_int = 0;

    if filename.is_null() {
        return -1;
    }
    let ctxt: *mut c_void = xml_nanohttp_open(url, content_type);
    if ctxt.is_null() {
        return -1;
    }

    if strcmp(filename, c"-".as_ptr() as _) == 0 {
        fd = 0;
    } else {
        fd = open(filename, O_CREAT | O_WRONLY, 0o0644);
        if fd < 0 {
            xml_nanohttp_close(ctxt);
            if !content_type.is_null() && !(*content_type).is_null() {
                xml_free(*content_type as _);
                *content_type = null_mut();
            }
            return -1;
        }
    }

    xml_nanohttp_fetch_content(ctxt, addr_of_mut!(buf), addr_of_mut!(len));
    if len > 0 && write(fd, buf as _, len as _) == -1 {
        ret = -1;
    }

    xml_nanohttp_close(ctxt);
    close(fd);
    ret
}

/**
 * xmlNanoHTTPMethod:
 * @URL:  The URL to load
 * @method:  the HTTP method to use
 * @input:  the input string if any
 * @contentType:  the Content-Type information IN and OUT
 * @headers:  the extra headers
 * @ilen:  input length
 *
 * This function try to open a connection to the indicated resource
 * via HTTP using the given @method, adding the given extra headers
 * and the input buffer for the request content.
 *
 * Returns NULL in case of failure, otherwise a request handler.
 *     The contentType, if provided must be freed by the caller
 */
pub unsafe extern "C" fn xml_nanohttp_method(
    url: *const c_char,
    method: *const c_char,
    input: *const c_char,
    content_type: *mut *mut c_char,
    headers: *const c_char,
    ilen: c_int,
) -> *mut c_void {
    xml_nanohttp_method_redir(url, method, input, content_type, null_mut(), headers, ilen)
}

/**
 * xmlNanoHTTPScanURL:
 * @ctxt:  an HTTP context
 * @URL:  The URL used to initialize the context
 *
 * (Re)Initialize an HTTP context by parsing the URL and finding
 * the protocol host port and path it indicates.
 */
fn xml_nanohttp_scan_url(ctxt: &mut XmlNanoHTTPCtxt, url: &str) {
    /*
     * Clear any existing data from the context
     */
    ctxt.protocol = None;
    ctxt.hostname = None;
    ctxt.path = None;
    ctxt.query = None;

    let Ok(uri) = Url::parse(url) else {
        return;
    };

    let Some(host) = uri.host() else {
        return;
    };

    ctxt.protocol = Some(uri.scheme().to_owned().into());

    /* special case of IPv6 addresses, the [] need to be removed */
    if let Host::Ipv6(host) = host {
        ctxt.hostname = Some(host.to_string().into());
    } else {
        let host = uri.host_str().unwrap();
        ctxt.hostname = Some(host.to_owned().into());
    }
    ctxt.path = Some(uri.path().to_owned().into());
    ctxt.query = uri.query().map(|q| q.to_owned().into());
    if let Some(port) = uri.port() {
        ctxt.port = port as i32;
    }
}

/**
 * xmlNanoHTTPNewCtxt:
 * @URL:  The URL used to initialize the context
 *
 * Allocate and initialize a new HTTP context.
 *
 * Returns an HTTP context or NULL in case of error.
 */
unsafe extern "C" fn xml_nanohttp_new_ctxt(url: *const c_char) -> XmlNanoHTTPCtxtPtr {
    let ret: XmlNanoHTTPCtxtPtr = xml_malloc(size_of::<XmlNanoHTTPCtxt>()) as XmlNanoHTTPCtxtPtr;
    if ret.is_null() {
        xml_http_err_memory(c"allocating context".as_ptr() as _);
        return null_mut();
    }

    let mut tmp = XmlNanoHTTPCtxt {
        protocol: None,
        hostname: None,
        port: 0,
        path: None,
        query: None,
        fd: 0,
        state: 0,
        out: null_mut(),
        outptr: null_mut(),
        input: null_mut(),
        content: null_mut(),
        inptr: null_mut(),
        inrptr: null_mut(),
        inlen: 0,
        last: 0,
        return_value: 0,
        version: 0,
        content_length: 0,
        content_type: null_mut(),
        location: None,
        auth_header: null_mut(),
        encoding: null_mut(),
        mime_type: null_mut(),
    };
    tmp.port = 80;
    tmp.return_value = 0;
    tmp.fd = INVALID_SOCKET;
    tmp.content_length = -1;

    let url = CStr::from_ptr(url).to_string_lossy();
    xml_nanohttp_scan_url(&mut tmp, url.as_ref());

    memcpy(
        ret as _,
        &tmp as *const XmlNanoHTTPCtxt as _,
        size_of::<XmlNanoHTTPCtxt>(),
    );
    forget(tmp);

    ret
}

/**
 * xmlNanoHTTPFreeCtxt:
 * @ctxt:  an HTTP context
 *
 * Frees the context after closing the connection.
 */
unsafe extern "C" fn xml_nanohttp_free_ctxt(ctxt: XmlNanoHTTPCtxtPtr) {
    if ctxt.is_null() {
        return;
    }
    (*ctxt).hostname = None;
    (*ctxt).protocol = None;
    (*ctxt).path = None;
    (*ctxt).query = None;
    if !(*ctxt).out.is_null() {
        xml_free((*ctxt).out as _);
    }
    if !(*ctxt).input.is_null() {
        xml_free((*ctxt).input as _);
    }
    if !(*ctxt).content_type.is_null() {
        xml_free((*ctxt).content_type as _);
    }
    if !(*ctxt).encoding.is_null() {
        xml_free((*ctxt).encoding as _);
    }
    if !(*ctxt).mime_type.is_null() {
        xml_free((*ctxt).mime_type as _);
    }
    (*ctxt).location = None;
    if !(*ctxt).auth_header.is_null() {
        xml_free((*ctxt).auth_header as _);
    }
    //  #ifdef LIBXML_ZLIB_ENABLED
    //      if !(*ctxt).strm.is_null() {
    //          inflateEnd((*ctxt).strm);
    //          xmlFree((*ctxt).strm as _);
    //      }
    //  #endif

    (*ctxt).state = XML_NANO_HTTP_NONE as _;
    if (*ctxt).fd != INVALID_SOCKET {
        closesocket((*ctxt).fd);
    }
    (*ctxt).fd = INVALID_SOCKET;
    xml_free(ctxt as _);
}

/**
 * xmlNanoHTTPHostnameMatch:
 * @pattern: The pattern as it appears in no_proxy environment variable
 * @hostname: The hostname to test as it appears in the URL
 *
 * This function tests whether a given hostname matches a pattern. The pattern
 * usually is a token from the no_proxy environment variable. Wildcards in the
 * pattern are not supported.
 *
 * Returns true, iff hostname matches the pattern.
 */
fn xml_nanohttp_hostname_match(pattern: &str, hostname: &str) -> bool {
    if pattern.is_empty() {
        return false;
    }

    let pattern = if let Some(pattern) = pattern.strip_prefix('.') {
        pattern
    } else {
        pattern
    };

    let mut pc = pattern.chars().rev();
    let mut hc = hostname.chars().rev();

    for (p, h) in pc.by_ref().zip(hc.by_ref()) {
        if p.to_ascii_lowercase() != h.to_ascii_lowercase() {
            return false;
        }
    }

    pc.next().is_none() && matches!(hc.next(), None | Some('.'))
}

/**
 * xmlNanoHTTPBypassProxy:
 * @hostname: The hostname as it appears in the URL
 *
 * This function evaluates the no_proxy environment variable and returns
 * whether the proxy server should be bypassed for a given host.
 *
 * Returns true, iff a proxy server should be bypassed for the given hostname.
 */
fn xml_nanohttp_bypass_proxy(hostname: &str) -> bool {
    if let Ok(env) = std::env::var("no_proxy") {
        return env
            .split(',')
            .map(|e| e.trim())
            .any(|e| xml_nanohttp_hostname_match(e, hostname));
    }
    false
}

pub type XmlSocklenT = c_uint;

/**
 * xmlNanoHTTPConnectAttempt:
 * @addr:  a socket address structure
 *
 * Attempt a connection to the given IP:port endpoint. It forces
 * non-blocking semantic on the socket, and allow 60 seconds for
 * the host to answer.
 *
 * Returns -1 in case of failure, the file descriptor number otherwise
 */

unsafe extern "C" fn xml_nanohttp_connect_attempt(addr: *mut sockaddr) -> Socket {
    let mut p: pollfd = unsafe { zeroed() };
    let mut status: c_int;

    let addrlen: c_int;

    let s: Socket;

    if (*addr).sa_family == AF_INET6 as u16 {
        s = socket(PF_INET6, SOCK_STREAM, IPPROTO_TCP);
        addrlen = size_of::<sockaddr_in6>() as _;
    } else {
        s = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
        addrlen = size_of::<sockaddr_in>() as _;
    }
    if s == INVALID_SOCKET {
        __xml_ioerr(
            XmlErrorDomain::XmlFromHTTP,
            XmlParserErrors::default(),
            c"socket failed\n".as_ptr() as _,
        );
        return INVALID_SOCKET;
    }
    status = fcntl(s, F_GETFL, 0);
    if status != -1 {
        status |= O_NONBLOCK;
        status = fcntl(s, F_SETFL, status);
    }
    if status < 0 {
        __xml_ioerr(
            XmlErrorDomain::XmlFromHTTP,
            XmlParserErrors::default(),
            c"error setting non-blocking IO\n".as_ptr() as _,
        );
        closesocket(s);
        return INVALID_SOCKET;
    }
    if connect(s, addr, addrlen as _) == -1 {
        match *__errno_location() {
            EINPROGRESS | EWOULDBLOCK => {}
            _ => {
                __xml_ioerr(
                    XmlErrorDomain::XmlFromHTTP,
                    XmlParserErrors::default(),
                    c"error connecting to HTTP server".as_ptr() as _,
                );
                closesocket(s);
                return INVALID_SOCKET;
            }
        }
    }
    p.fd = s;
    p.events = POLLOUT;
    match poll(addr_of_mut!(p), 1, TIMEOUT as i32 * 1000) {
        0 => {
            /* Time out */
            __xml_ioerr(
                XmlErrorDomain::XmlFromHTTP,
                XmlParserErrors::default(),
                c"Connect attempt timed out".as_ptr() as _,
            );
            closesocket(s);
            return INVALID_SOCKET;
        }
        -1 => {
            /* Ermm.. ?? */
            __xml_ioerr(
                XmlErrorDomain::XmlFromHTTP,
                XmlParserErrors::default(),
                c"Connect failed".as_ptr() as _,
            );
            closesocket(s);
            return INVALID_SOCKET;
        }
        _ => {}
    }

    if p.revents == POLLOUT {
        let mut len: XmlSocklenT;

        len = size_of_val(&status) as _;
        if getsockopt(
            s,
            SOL_SOCKET,
            SO_ERROR,
            addr_of_mut!(status) as _,
            addr_of_mut!(len),
        ) < 0
        {
            /* Solaris error code */
            __xml_ioerr(
                XmlErrorDomain::XmlFromHTTP,
                XmlParserErrors::default(),
                c"getsockopt failed\n".as_ptr() as _,
            );
            closesocket(s);
            return INVALID_SOCKET;
        }
        if status != 0 {
            __xml_ioerr(
                XmlErrorDomain::XmlFromHTTP,
                XmlParserErrors::default(),
                c"Error connecting to remote host".as_ptr() as _,
            );
            closesocket(s);
            *__errno_location() = status;
            return INVALID_SOCKET;
        }
    } else {
        /* pbm */
        __xml_ioerr(
            XmlErrorDomain::XmlFromHTTP,
            XmlParserErrors::default(),
            c"select failed\n".as_ptr() as _,
        );
        closesocket(s);
        return INVALID_SOCKET;
    }

    s
}

/**
 * xmlNanoHTTPConnectHost:
 * @host:  the host name
 * @port:  the port number
 *
 * Attempt a connection to the given host:port endpoint. It tries
 * the multiple IP provided by the DNS if available.
 *
 * Returns -1 in case of failure, the file descriptor number otherwise
 */

unsafe fn xml_nanohttp_connect_host(host: &str, port: c_int) -> Socket {
    let mut addr: *mut sockaddr;
    let mut sockin: sockaddr_in = unsafe { zeroed() };
    let mut sockin6: sockaddr_in6 = unsafe { zeroed() };
    let mut s: Socket;
    let host = CString::new(host).unwrap();

    memset(addr_of_mut!(sockin) as _, 0, size_of_val(&sockin));

    {
        let mut hints: addrinfo = unsafe { zeroed() };
        let mut res: *mut addrinfo;
        let mut result: *mut addrinfo;

        memset(addr_of_mut!(sockin6) as _, 0, size_of_val(&sockin6));

        result = null_mut();
        memset(addr_of_mut!(hints) as _, 0, size_of_val(&hints));
        hints.ai_socktype = SOCK_STREAM;

        let status: c_int = getaddrinfo(
            host.as_ptr(),
            null_mut(),
            addr_of_mut!(hints) as _,
            addr_of_mut!(result),
        );
        if status != 0 {
            __xml_ioerr(
                XmlErrorDomain::XmlFromHTTP,
                XmlParserErrors::default(),
                c"getaddrinfo failed\n".as_ptr() as _,
            );
            return INVALID_SOCKET;
        }

        res = result;
        while !res.is_null() {
            if (*res).ai_family == AF_INET {
                if (*res).ai_addrlen as usize > size_of_val(&sockin) {
                    __xml_ioerr(
                        XmlErrorDomain::XmlFromHTTP,
                        XmlParserErrors::default(),
                        c"address size mismatch\n".as_ptr() as _,
                    );
                    freeaddrinfo(result);
                    return INVALID_SOCKET;
                }
                memcpy(
                    addr_of_mut!(sockin) as _,
                    (*res).ai_addr as _,
                    (*res).ai_addrlen as _,
                );
                // sockin.sin_port = htons(port);
                sockin.sin_port = port.to_be() as u16;
                addr = addr_of_mut!(sockin) as _;
            } else if (*res).ai_family == AF_INET6 {
                if (*res).ai_addrlen as usize > size_of_val(&sockin6) {
                    __xml_ioerr(
                        XmlErrorDomain::XmlFromHTTP,
                        XmlParserErrors::default(),
                        c"address size mismatch\n".as_ptr() as _,
                    );
                    freeaddrinfo(result);
                    return INVALID_SOCKET;
                }
                memcpy(
                    addr_of_mut!(sockin6) as _,
                    (*res).ai_addr as _,
                    (*res).ai_addrlen as _,
                );
                // sockin6.sin6_port = htons(port);
                sockin6.sin6_port = port.to_be() as u16;
                addr = addr_of_mut!(sockin6) as _;
            } else {
                res = (*res).ai_next;
                continue; /* for */
            }

            s = xml_nanohttp_connect_attempt(addr);
            if s != INVALID_SOCKET {
                freeaddrinfo(result);
                return s;
            }
            res = (*res).ai_next;
        }

        if !result.is_null() {
            freeaddrinfo(result);
        }
    }

    INVALID_SOCKET
}

/**
 * xmlNanoHTTPSend:
 * @ctxt:  an HTTP context
 *
 * Send the input needed to initiate the processing on the server side
 * Returns number of bytes sent or -1 on error.
 */
unsafe extern "C" fn xml_nanohttp_send(
    ctxt: XmlNanoHTTPCtxtPtr,
    xmt_ptr: *const c_char,
    outlen: c_int,
) -> c_int {
    let mut total_sent: c_int = 0;
    let mut p: pollfd = unsafe { zeroed() };

    if (*ctxt).state & XML_NANO_HTTP_WRITE as i32 != 0 && !xmt_ptr.is_null() {
        while total_sent < outlen {
            let nsent: c_int = send(
                (*ctxt).fd,
                xmt_ptr.add(total_sent as usize) as _,
                (outlen - total_sent) as usize,
                0,
            ) as _;

            if nsent > 0 {
                total_sent += nsent;
            } else if nsent == -1
                && *__errno_location() != EAGAIN
                && *__errno_location() != EWOULDBLOCK
            {
                __xml_ioerr(
                    XmlErrorDomain::XmlFromHTTP,
                    XmlParserErrors::default(),
                    c"send failed\n".as_ptr() as _,
                );
                if total_sent == 0 {
                    total_sent = -1;
                }
                break;
            } else {
                /*
                 * No data sent
                 * Since non-blocking sockets are used, wait for
                 * socket to be writable or default timeout prior
                 * to retrying.
                 */
                p.fd = (*ctxt).fd;
                p.events = POLLOUT;
                poll(addr_of_mut!(p), 1, TIMEOUT as i32 * 1000);
            }
        }
    }

    total_sent
}

/**
 * xmlNanoHTTPReadLine:
 * @ctxt:  an HTTP context
 *
 * Read one line in the HTTP server output, usually for extracting
 * the HTTP protocol information from the answer header.
 *
 * Returns a newly allocated string with a copy of the line, or NULL
 *         which indicate the end of the input.
 */
unsafe extern "C" fn xml_nanohttp_read_line(ctxt: XmlNanoHTTPCtxtPtr) -> *mut c_char {
    let mut buf: [c_char; 4096] = [0; 4096];
    let mut bp: *mut c_char = buf.as_mut_ptr();
    let mut rc: c_int;

    while bp.offset_from(buf.as_mut_ptr()) < 4095 {
        if (*ctxt).inrptr == (*ctxt).inptr {
            rc = xml_nanohttp_recv(ctxt);
            if rc == 0 {
                if bp == buf.as_mut_ptr() {
                    return null_mut();
                } else {
                    *bp = 0;
                }
                return xml_mem_strdup(buf.as_mut_ptr() as _) as _;
            } else if rc == -1 {
                return null_mut();
            }
        }
        *bp = *(*ctxt).inrptr;
        (*ctxt).inptr = (*ctxt).inptr.add(1);
        if *bp == b'\n' as i8 {
            *bp = 0;
            return xml_mem_strdup(buf.as_mut_ptr() as _) as _;
        }
        if *bp != b'\r' as _ {
            bp = bp.add(1);
        }
    }
    buf[4095] = 0;
    xml_mem_strdup(buf.as_mut_ptr() as _) as _
}

/**
 * xmlNanoHTTPScanAnswer:
 * @ctxt:  an HTTP context
 * @line:  an HTTP header line
 *
 * Try to extract useful information from the server answer.
 * We currently parse and process:
 *  - The HTTP revision/ return code
 *  - The Content-Type, Mime-Type and charset used
 *  - The Location for redirect processing.
 *
 * Returns -1 in case of failure, the file descriptor number otherwise
 */
unsafe extern "C" fn xml_nanohttp_scan_answer(ctxt: XmlNanoHTTPCtxtPtr, line: *const c_char) {
    let mut cur: *const c_char = line;

    if line.is_null() {
        return;
    }

    if strncmp(line, c"HTTP/".as_ptr() as _, 5) == 0 {
        let mut version: c_int = 0;
        let mut ret: c_int = 0;

        cur = cur.add(5);
        while *cur >= b'0' as i8 && *cur <= b'9' as i8 {
            version *= 10;
            version += (*cur - b'0' as i8) as i32;
            cur = cur.add(1);
        }
        if *cur == b'.' as i8 {
            cur = cur.add(1);
            if *cur >= b'0' as i8 && *cur <= b'9' as i8 {
                version *= 10;
                version += (*cur - b'0' as i8) as i32;
                cur = cur.add(1);
            }
            while *cur >= b'0' as i8 && *cur <= b'9' as i8 {
                cur = cur.add(1);
            }
        } else {
            version *= 10;
        }
        if *cur != b' ' as i8 && *cur != b'\t' as i8 {
            return;
        }
        while *cur == b' ' as i8 || *cur == b'\t' as i8 {
            cur = cur.add(1);
        }
        if *cur < b'0' as i8 || *cur > b'9' as i8 {
            return;
        }
        while *cur >= b'0' as i8 && *cur <= b'9' as i8 {
            ret *= 10;
            ret += (*cur - b'0' as i8) as i32;
            cur = cur.add(1);
        }
        if (*cur != 0) && *cur != b' ' as i8 && *cur != b'\t' as i8 {
            return;
        }
        (*ctxt).return_value = ret;
        (*ctxt).version = version;
    } else if xml_strncasecmp(line as _, c"Content-Type:".as_ptr() as _, 13) == 0 {
        let mut charset: *const XmlChar;
        let mut last: *const XmlChar;

        cur = cur.add(13);
        while *cur == b' ' as i8 || *cur == b'\t' as i8 {
            cur = cur.add(1);
        }
        if !(*ctxt).content_type.is_null() {
            xml_free((*ctxt).content_type as _);
        }
        (*ctxt).content_type = xml_mem_strdup(cur as _) as _;
        let mime: *const XmlChar = cur as _;
        last = mime;
        while *last != 0 && *last != b' ' && *last != b'\t' && *last != b';' && *last != b',' {
            last = last.add(1);
        }
        if !(*ctxt).mime_type.is_null() {
            xml_free((*ctxt).mime_type as _);
        }
        (*ctxt).mime_type = xml_strndup(mime, last.offset_from(mime) as _) as _;
        charset = xml_strstr((*ctxt).content_type as _, c"charset=".as_ptr() as _);
        if !charset.is_null() {
            charset = charset.add(8);
            last = charset;
            while *last != 0 && *last != b' ' && *last != b'\t' && *last != b';' && *last != b',' {
                last = last.add(1);
            }
            if !(*ctxt).encoding.is_null() {
                xml_free((*ctxt).encoding as _);
            }
            (*ctxt).encoding = xml_strndup(charset, last.offset_from(charset) as _) as _;
        }
    } else if xml_strncasecmp(line as _, c"ContentType:".as_ptr() as _, 12) == 0 {
        let mut charset: *const XmlChar;
        let mut last: *const XmlChar;

        cur = cur.add(12);
        if !(*ctxt).content_type.is_null() {
            return;
        }
        while *cur == b' ' as i8 || *cur == b'\t' as i8 {
            cur = cur.add(1);
        }
        (*ctxt).content_type = xml_mem_strdup(cur as _) as _;
        let mime: *const XmlChar = cur as _;
        last = mime;
        while *last != 0 && *last != b' ' && *last != b'\t' && *last != b';' && *last != b',' {
            last = last.add(1);
        }
        if !(*ctxt).mime_type.is_null() {
            xml_free((*ctxt).mime_type as _);
        }
        (*ctxt).mime_type = xml_strndup(mime, last.offset_from(mime) as _) as _;
        charset = xml_strstr((*ctxt).content_type as _, c"charset=".as_ptr() as _);
        if !charset.is_null() {
            charset = charset.add(8);
            last = charset;
            while *last != 0 && *last != b' ' && *last != b'\t' && *last != b';' && *last != b',' {
                last = last.add(1);
            }
            if !(*ctxt).encoding.is_null() {
                xml_free((*ctxt).encoding as _);
            }
            (*ctxt).encoding = xml_strndup(charset, last.offset_from(charset) as _) as _;
        }
    } else if xml_strncasecmp(line as _, c"Location:".as_ptr() as _, 9) == 0 {
        cur = cur.add(9);
        while *cur == b' ' as i8 || *cur == b'\t' as i8 {
            cur = cur.add(1);
        }
        (*ctxt).location = None;
        if *cur == b'/' as i8 {
            let loc = format!("http://{}", (*ctxt).hostname.as_deref().unwrap_or(""));
            (*ctxt).location = Some(loc.into());
        } else {
            let loc = CStr::from_ptr(cur).to_string_lossy().into_owned();
            (*ctxt).location = Some(loc.into());
        }
    } else if xml_strncasecmp(line as _, c"WWW-Authenticate:".as_ptr() as _, 17) == 0 {
        cur = cur.add(17);
        while *cur == b' ' as i8 || *cur == b'\t' as i8 {
            cur = cur.add(1);
        }
        if !(*ctxt).auth_header.is_null() {
            xml_free((*ctxt).auth_header as _);
        }
        (*ctxt).auth_header = xml_mem_strdup(cur as _) as _;
    } else if xml_strncasecmp(line as _, c"Proxy-Authenticate:".as_ptr() as _, 19) == 0 {
        cur = cur.add(19);
        while *cur == b' ' as i8 || *cur == b'\t' as i8 {
            cur = cur.add(1);
        }
        if !(*ctxt).auth_header.is_null() {
            xml_free((*ctxt).auth_header as _);
        }
        (*ctxt).auth_header = xml_mem_strdup(cur as _) as _;
    //  #ifdef LIBXML_ZLIB_ENABLED
    //      } else if ( !xmlStrncasecmp(  line, c"Content-Encoding:".as_ptr() as _, 17) ) {
    //      cur = cur.add(17);
    //      while *cur == b' ' || *cur == b'\t' {
    //          cur = cur.add(1);
    //      }
    //      if ( !xmlStrncasecmp(  cur, c"gzip".as_ptr() as _, 4) ) {
    //          (*ctxt).usesGzip = 1;
    //          (*ctxt).strm = xmlMalloc(sizeof(z_stream));
    //          if !(*ctxt).strm.is_null() {
    //          (*(*ctxt).strm).zalloc = Z_NULL;
    //          (*(*ctxt).strm).zfree = Z_NULL;
    //          (*(*ctxt).strm).opaque = Z_NULL;
    //          (*(*ctxt).strm).avail_in = 0;
    //          (*(*ctxt).strm).next_in = Z_NULL;
    //          inflateInit2( (*ctxt).strm, 31 );
    //          }
    //      }
    //  #endif
    } else if xml_strncasecmp(line as _, c"Content-Length:".as_ptr() as _, 15) == 0 {
        cur = cur.add(15);
        (*ctxt).content_length = strtol(cur, null_mut(), 10) as _;
    }
}

/**
 * xmlNanoHTTPMethodRedir:
 * @URL:  The URL to load
 * @method:  the HTTP method to use
 * @input:  the input string if any
 * @contentType:  the Content-Type information IN and OUT
 * @redir:  the redirected URL OUT
 * @headers:  the extra headers
 * @ilen:  input length
 *
 * This function try to open a connection to the indicated resource
 * via HTTP using the given @method, adding the given extra headers
 * and the input buffer for the request content.
 *
 * Returns NULL in case of failure, otherwise a request handler.
 *     The contentType, or redir, if provided must be freed by the caller
 */
pub unsafe extern "C" fn xml_nanohttp_method_redir(
    url: *const c_char,
    mut method: *const c_char,
    input: *const c_char,
    content_type: *mut *mut c_char,
    redir: *mut *mut c_char,
    headers: *const c_char,
    mut ilen: c_int,
) -> *mut c_void {
    let mut ctxt: XmlNanoHTTPCtxtPtr;
    let mut bp: *mut c_char;
    let mut p: *mut c_char;
    let mut blen: usize;
    let mut ret: Socket;
    let mut nb_redirects: c_int = 0;
    let mut use_proxy: c_int;
    let mut redir_url: *mut c_char = null_mut();

    if url.is_null() {
        return null_mut();
    }
    if method.is_null() {
        method = c"GET".as_ptr() as _;
    }
    xml_nanohttp_init();

    // retry:
    'retry: loop {
        if redir_url.is_null() {
            ctxt = xml_nanohttp_new_ctxt(url);
            if ctxt.is_null() {
                return null_mut();
            }
        } else {
            ctxt = xml_nanohttp_new_ctxt(redir_url);
            if ctxt.is_null() {
                return null_mut();
            }
            let redir_url = CStr::from_ptr(redir_url).to_string_lossy().into_owned();
            (*ctxt).location = Some(redir_url.into());
        }

        if (*ctxt).protocol != Some("http".into()) {
            __xml_ioerr(
                XmlErrorDomain::XmlFromHTTP,
                XmlParserErrors::XmlHttpUrlSyntax,
                c"Not a valid HTTP URI".as_ptr() as _,
            );
            xml_nanohttp_free_ctxt(ctxt);
            if !redir_url.is_null() {
                xml_free(redir_url as _);
            }
            return null_mut();
        }
        let Some(hostname) = (*ctxt).hostname.as_ref() else {
            __xml_ioerr(
                XmlErrorDomain::XmlFromHTTP,
                XmlParserErrors::XmlHttpUnknownHost,
                c"Failed to identify host in URI".as_ptr() as _,
            );
            xml_nanohttp_free_ctxt(ctxt);
            if !redir_url.is_null() {
                xml_free(redir_url as _);
            }
            return null_mut();
        };
        use_proxy = (!PROXY.load(Ordering::Relaxed).is_null()
            && !xml_nanohttp_bypass_proxy(hostname.as_ref())) as i32;
        if use_proxy != 0 {
            blen = (*ctxt).hostname.as_ref().map_or(0, |h| h.len()) * 2 + 16;
            ret = xml_nanohttp_connect_host(
                CStr::from_ptr(PROXY.load(Ordering::Relaxed) as _)
                    .to_string_lossy()
                    .as_ref(),
                PROXY_PORT.load(Ordering::Relaxed) as _,
            );
        } else {
            blen = (*ctxt).hostname.as_ref().map_or(0, |h| h.len());
            ret = xml_nanohttp_connect_host(hostname.as_ref(), (*ctxt).port);
        }
        if ret == INVALID_SOCKET {
            xml_nanohttp_free_ctxt(ctxt);
            if !redir_url.is_null() {
                xml_free(redir_url as _);
            }
            return null_mut();
        }
        (*ctxt).fd = ret;

        if input.is_null() {
            ilen = 0;
        } else {
            blen += 36;
        }

        if !headers.is_null() {
            blen += strlen(headers) + 2;
        }
        if !content_type.is_null() && !(*content_type).is_null() {
            /* reserve for string plus 'Content-Type: \r\n" */
            blen += strlen(*content_type) + 16;
        }
        if let Some(query) = (*ctxt).query.as_deref() {
            /* 1 for '?' */
            blen += query.len() + 1;
        }
        blen += strlen(method) + (*ctxt).path.as_ref().map_or(0, |s| s.len()) + 24;
        // #ifdef LIBXML_ZLIB_ENABLED
        //     /* reserve for possible 'Accept-Encoding: gzip' string */
        //     blen += 23;
        // #endif
        if (*ctxt).port != 80 {
            /* reserve space for ':xxxxx', incl. potential proxy */
            if use_proxy != 0 {
                blen += 17;
            } else {
                blen += 11;
            }
        }
        bp = xml_malloc_atomic(blen) as _;
        if bp.is_null() {
            xml_nanohttp_free_ctxt(ctxt);
            xml_http_err_memory(c"allocating header buffer".as_ptr() as _);
            return null_mut();
        }

        p = bp;

        if use_proxy != 0 {
            let hostname = CString::new(hostname.as_ref()).unwrap();
            let path = CString::new((*ctxt).path.as_ref().unwrap().as_ref()).unwrap();
            if (*ctxt).port != 80 {
                p = p.add(snprintf(
                    p as _,
                    blen - p.offset_from(bp) as usize,
                    c"%s http://%s:%d%s".as_ptr() as _,
                    method,
                    hostname.as_ptr(),
                    (*ctxt).port,
                    path.as_ptr(),
                ) as usize);
            } else {
                p = p.add(snprintf(
                    p as _,
                    blen - p.offset_from(bp) as usize,
                    c"%s http://%s%s".as_ptr() as _,
                    method,
                    hostname.as_ptr(),
                    path.as_ptr(),
                ) as usize);
            }
        } else {
            let path = CString::new((*ctxt).path.as_ref().unwrap().as_ref()).unwrap();
            p = p.add(snprintf(
                p,
                blen - p.offset_from(bp) as usize,
                c"%s %s".as_ptr() as _,
                method,
                path.as_ptr(),
            ) as usize);
        }

        if let Some(query) = (*ctxt).query.as_deref() {
            let query = CString::new(query).unwrap();
            p = p.add(snprintf(
                p,
                blen - p.offset_from(bp) as usize,
                c"?%s".as_ptr() as _,
                query.as_ptr(),
            ) as usize);
        }

        if (*ctxt).port == 80 {
            let hostname = CString::new(hostname.as_ref()).unwrap();
            p = p.add(snprintf(
                p,
                blen - p.offset_from(bp) as usize,
                c" HTTP/1.0\r\nHost: %s\r\n".as_ptr() as _,
                hostname.as_ptr(),
            ) as usize);
        } else {
            let hostname = CString::new(hostname.as_ref()).unwrap();
            p = p.add(snprintf(
                p,
                blen - p.offset_from(bp) as usize,
                c" HTTP/1.0\r\nHost: %s:%d\r\n".as_ptr() as _,
                hostname.as_ptr(),
                (*ctxt).port,
            ) as usize);
        }

        // #ifdef LIBXML_ZLIB_ENABLED
        //     p = p.add(snprintf(p, blen - p.offset_from(bp) as usize, c"Accept-Encoding: gzip\r\n");
        // #endif

        if !content_type.is_null() && !(*content_type).is_null() {
            p = p.add(snprintf(
                p,
                blen - p.offset_from(bp) as usize,
                c"Content-Type: %s\r\n".as_ptr() as _,
                *content_type,
            ) as usize);
        }

        if !headers.is_null() {
            p = p.add(snprintf(
                p,
                blen - p.offset_from(bp) as usize,
                c"%s".as_ptr() as _,
                headers,
            ) as usize);
        }

        if !input.is_null() {
            snprintf(
                p,
                blen - p.offset_from(bp) as usize,
                c"Content-Length: %d\r\n\r\n".as_ptr() as _,
                ilen,
            );
        } else {
            snprintf(p, blen - p.offset_from(bp) as usize, c"\r\n".as_ptr() as _);
        }

        (*ctxt).outptr = bp;
        (*ctxt).out = bp;
        (*ctxt).state = XML_NANO_HTTP_WRITE as _;
        blen = strlen((*ctxt).out);
        xml_nanohttp_send(ctxt, (*ctxt).out, blen as _);

        if !input.is_null() {
            xml_nanohttp_send(ctxt, input, ilen);
        }

        (*ctxt).state = XML_NANO_HTTP_READ as _;

        while {
            p = xml_nanohttp_read_line(ctxt);
            !p.is_null()
        } {
            if *p == 0 {
                (*ctxt).content = (*ctxt).inrptr;
                xml_free(p as _);
                break;
            }
            xml_nanohttp_scan_answer(ctxt, p);

            xml_free(p as _);
        }

        if let Some(location) = (*ctxt)
            .location
            .as_deref()
            .filter(|_| (*ctxt).return_value >= 300 && (*ctxt).return_value < 400)
        {
            while xml_nanohttp_recv(ctxt) > 0 {}
            if nb_redirects < XML_NANO_HTTP_MAX_REDIR as i32 {
                nb_redirects += 1;
                if !redir_url.is_null() {
                    xml_free(redir_url as _);
                }
                let url = CString::new(location).unwrap();
                redir_url = xml_mem_strdup(url.as_ptr() as _) as _;
                xml_nanohttp_free_ctxt(ctxt);
                continue 'retry;
            }

            break;
        }
        xml_nanohttp_free_ctxt(ctxt);
        if !redir_url.is_null() {
            xml_free(redir_url as _);
        }
        return null_mut();
    }

    if !content_type.is_null() {
        if !(*ctxt).content_type.is_null() {
            *content_type = xml_mem_strdup((*ctxt).content_type as _) as _;
        } else {
            *content_type = null_mut();
        }
    }

    if !redir.is_null() && !redir_url.is_null() {
        *redir = redir_url;
    } else {
        if !redir_url.is_null() {
            xml_free(redir_url as _);
        }
        if !redir.is_null() {
            *redir = null_mut();
        }
    }

    ctxt as _
}

/**
 * xmlNanoHTTPOpen:
 * @URL:  The URL to load
 * @contentType:  if available the Content-Type information will be
 *                returned at that location
 *
 * This function try to open a connection to the indicated resource
 * via HTTP GET.
 *
 * Returns NULL in case of failure, otherwise a request handler.
 *     The contentType, if provided must be freed by the caller
 */
pub unsafe extern "C" fn xml_nanohttp_open(
    url: *const c_char,
    content_type: *mut *mut c_char,
) -> *mut c_void {
    if !content_type.is_null() {
        *content_type = null_mut();
    }
    xml_nanohttp_method(url, null_mut(), null_mut(), content_type, null_mut(), 0)
}

/**
 * xmlNanoHTTPOpenRedir:
 * @URL:  The URL to load
 * @contentType:  if available the Content-Type information will be
 *                returned at that location
 * @redir: if available the redirected URL will be returned
 *
 * This function try to open a connection to the indicated resource
 * via HTTP GET.
 *
 * Returns NULL in case of failure, otherwise a request handler.
 *     The contentType, if provided must be freed by the caller
 */
pub unsafe extern "C" fn xml_nanohttp_open_redir(
    url: *const c_char,
    content_type: *mut *mut c_char,
    redir: *mut *mut c_char,
) -> *mut c_void {
    if !content_type.is_null() {
        *content_type = null_mut();
    }
    if !redir.is_null() {
        *redir = null_mut();
    }
    xml_nanohttp_method_redir(
        url,
        null_mut(),
        null_mut(),
        content_type,
        redir,
        null_mut(),
        0,
    )
}

/**
 * xmlNanoHTTPReturnCode:
 * @ctx:  the HTTP context
 *
 * Get the latest HTTP return code received
 *
 * Returns the HTTP return code for the request.
 */
pub unsafe extern "C" fn xml_nanohttp_return_code(ctx: *mut c_void) -> c_int {
    let ctxt: XmlNanoHTTPCtxtPtr = ctx as XmlNanoHTTPCtxtPtr;

    if ctxt.is_null() {
        return -1;
    }

    (*ctxt).return_value
}

/**
 * xmlNanoHTTPAuthHeader:
 * @ctx:  the HTTP context
 *
 * Get the authentication header of an HTTP context
 *
 * Returns the stashed value of the WWW-Authenticate or Proxy-Authenticate
 * header.
 */
pub unsafe extern "C" fn xml_nanohttp_auth_header(ctx: *mut c_void) -> *const c_char {
    let ctxt: XmlNanoHTTPCtxtPtr = ctx as XmlNanoHTTPCtxtPtr;

    if ctxt.is_null() {
        return null_mut();
    }

    (*ctxt).auth_header
}

/**
 * xmlNanoHTTPRedir:
 * @ctx:  the HTTP context
 *
 * Provides the specified redirection URL if available from the HTTP header.
 *
 * Return the specified redirection URL or NULL if not redirected.
 */
pub unsafe fn xml_nanohttp_redir(ctx: *mut c_void) -> Option<String> {
    let ctxt: XmlNanoHTTPCtxtPtr = ctx as XmlNanoHTTPCtxtPtr;

    if ctxt.is_null() {
        None
    } else {
        (*ctxt).location.as_deref().map(|s| s.to_owned())
    }
}

/**
 * xmlNanoHTTPContentLength:
 * @ctx:  the HTTP context
 *
 * Provides the specified content length from the HTTP header.
 *
 * Return the specified content length from the HTTP header.  Note that
 * a value of -1 indicates that the content length element was not included in
 * the response header.
 */
pub unsafe extern "C" fn xml_nanohttp_content_length(ctx: *mut c_void) -> c_int {
    let ctxt: XmlNanoHTTPCtxtPtr = ctx as XmlNanoHTTPCtxtPtr;

    if ctxt.is_null() {
        -1
    } else {
        (*ctxt).content_length
    }
}

/**
 * xmlNanoHTTPEncoding:
 * @ctx:  the HTTP context
 *
 * Provides the specified encoding if specified in the HTTP headers.
 *
 * Return the specified encoding or NULL if not available
 */
pub unsafe extern "C" fn xml_nanohttp_encoding(ctx: *mut c_void) -> *const c_char {
    let ctxt: XmlNanoHTTPCtxtPtr = ctx as XmlNanoHTTPCtxtPtr;

    if ctxt.is_null() {
        null()
    } else {
        (*ctxt).encoding
    }
}

/**
 * xmlNanoHTTPMimeType:
 * @ctx:  the HTTP context
 *
 * Provides the specified Mime-Type if specified in the HTTP headers.
 *
 * Return the specified Mime-Type or NULL if not available
 */
pub unsafe extern "C" fn xml_nanohttp_mime_type(ctx: *mut c_void) -> *const c_char {
    let ctxt: XmlNanoHTTPCtxtPtr = ctx as XmlNanoHTTPCtxtPtr;

    if ctxt.is_null() {
        null()
    } else {
        (*ctxt).mime_type
    }
}

/**
 * xmlNanoHTTPRead:
 * @ctx:  the HTTP context
 * @dest:  a buffer
 * @len:  the buffer length
 *
 * This function tries to read @len bytes from the existing HTTP connection
 * and saves them in @dest. This is a blocking call.
 *
 * Returns the number of byte read. 0 is an indication of an end of connection.
 *         -1 indicates a parameter error.
 */
pub unsafe extern "C" fn xml_nanohttp_read(
    ctx: *mut c_void,
    dest: *mut c_void,
    mut len: c_int,
) -> c_int {
    let ctxt: XmlNanoHTTPCtxtPtr = ctx as XmlNanoHTTPCtxtPtr;
    // #ifdef LIBXML_ZLIB_ENABLED
    //     c_int bytes_read = 0;
    //     c_int orig_avail_in;
    //     c_int z_ret;
    // #endif

    if ctx.is_null() {
        return -1;
    }
    if dest.is_null() {
        return -1;
    }
    if len <= 0 {
        return 0;
    }

    // #ifdef LIBXML_ZLIB_ENABLED
    //     if ((*ctxt).usesGzip == 1) {
    //         if (*ctxt).strm.is_null() {
    //             return 0;
    //         }
    //         (*(*ctxt).strm).next_out = dest;
    //         (*(*ctxt).strm).avail_out = len;
    // 	(*(*ctxt).strm).avail_in = (*ctxt).inptr.offset_from((*ctxt).inrptr);
    //         while ((*(*ctxt).strm).avail_out > 0 &&
    // 	       ((*(*ctxt).strm).avail_in > 0 || xmlNanoHTTPRecv(ctxt) > 0)) {
    //             orig_avail_in = (*(*ctxt).strm).avail_in =
    // 			    (*ctxt).inptr.offset_from((*ctxt).inrptr) - bytes_read;
    //             (*(*ctxt).strm).next_in =  ((*ctxt).inrptr + bytes_read);
    //             z_ret = inflate((*ctxt).strm, Z_NO_FLUSH);
    //             bytes_read += orig_avail_in - (*(*ctxt).strm).avail_in;
    //             if (z_ret != Z_OK) {
    //                 break;
    //             }
    // 	}
    //         (*ctxt).inrptr += bytes_read;
    //         return(len - (*(*ctxt).strm).avail_out);
    //     }
    // #endif

    while (*ctxt).inptr.offset_from((*ctxt).inrptr) < len as isize {
        if xml_nanohttp_recv(ctxt) <= 0 {
            break;
        }
    }
    if (*ctxt).inptr.offset_from((*ctxt).inrptr) < len as isize {
        len = (*ctxt).inptr.offset_from((*ctxt).inrptr) as i32;
    }
    memcpy(dest as _, (*ctxt).inrptr as _, len as usize);
    (*ctxt).inrptr = (*ctxt).inrptr.add(len as usize);
    len
}

/**
 * xmlNanoHTTPSave:
 * @ctxt:  the HTTP context
 * @filename:  the filename where the content should be saved
 *
 * This function saves the output of the HTTP transaction to a file
 * It closes and free the context at the end
 *
 * Returns -1 in case of failure, 0 in case of success.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_nanohttp_save(ctxt: *mut c_void, filename: *const c_char) -> c_int {
    let mut buf: *mut c_char = null_mut();
    let fd: c_int;
    let mut len: c_int = 0;
    let mut ret: c_int = 0;

    if ctxt.is_null() || filename.is_null() {
        return -1;
    }

    if strcmp(filename, c"-".as_ptr() as _) == 0 {
        fd = 0;
    } else {
        fd = open(filename, O_CREAT | O_WRONLY, 0o666);
        if fd < 0 {
            xml_nanohttp_close(ctxt);
            return -1;
        }
    }

    xml_nanohttp_fetch_content(ctxt, addr_of_mut!(buf) as _, addr_of_mut!(len));
    if len > 0 && write(fd, buf as _, len as _) == -1 {
        ret = -1;
    }

    xml_nanohttp_close(ctxt);
    close(fd);
    ret
}

/**
 * xmlNanoHTTPClose:
 * @ctx:  the HTTP context
 *
 * This function closes an HTTP context, it ends up the connection and
 * free all data related to it.
 */
pub unsafe extern "C" fn xml_nanohttp_close(ctx: *mut c_void) {
    let ctxt: XmlNanoHTTPCtxtPtr = ctx as XmlNanoHTTPCtxtPtr;

    if ctx.is_null() {
        return;
    }

    xml_nanohttp_free_ctxt(ctxt);
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_nano_httpauth_header() {
        #[cfg(feature = "http")]
        unsafe {
            let mut leaks = 0;

            for n_ctx in 0..GEN_NB_XML_NANO_HTTPCTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctx = gen_xml_nano_httpctxt_ptr(n_ctx, 0);

                let ret_val = xml_nanohttp_auth_header(ctx);
                desret_const_char_ptr(ret_val);
                des_xml_nano_httpctxt_ptr(n_ctx, ctx, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlNanoHTTPAuthHeader",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_ctx);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlNanoHTTPAuthHeader()"
            );
        }
    }

    #[test]
    fn test_xml_nano_httpcleanup() {
        #[cfg(feature = "http")]
        unsafe {
            let mut leaks = 0;

            let mem_base = xml_mem_blocks();

            xml_nanohttp_cleanup();
            reset_last_error();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in xmlNanoHTTPCleanup",
                    xml_mem_blocks() - mem_base
                );
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlNanoHTTPCleanup()"
            );
        }
    }

    #[test]
    fn test_xml_nano_httpcontent_length() {
        #[cfg(feature = "http")]
        unsafe {
            let mut leaks = 0;

            for n_ctx in 0..GEN_NB_XML_NANO_HTTPCTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctx = gen_xml_nano_httpctxt_ptr(n_ctx, 0);

                let ret_val = xml_nanohttp_content_length(ctx);
                desret_int(ret_val);
                des_xml_nano_httpctxt_ptr(n_ctx, ctx, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlNanoHTTPContentLength",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_ctx);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlNanoHTTPContentLength()"
            );
        }
    }

    #[test]
    fn test_xml_nano_httpencoding() {
        #[cfg(feature = "http")]
        unsafe {
            let mut leaks = 0;

            for n_ctx in 0..GEN_NB_XML_NANO_HTTPCTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctx = gen_xml_nano_httpctxt_ptr(n_ctx, 0);

                let ret_val = xml_nanohttp_encoding(ctx);
                desret_const_char_ptr(ret_val);
                des_xml_nano_httpctxt_ptr(n_ctx, ctx, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlNanoHTTPEncoding",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_ctx);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlNanoHTTPEncoding()"
            );
        }
    }

    #[test]
    fn test_xml_nano_httpfetch() {
        #[cfg(feature = "http")]
        unsafe {
            let mut leaks = 0;

            for n_url in 0..GEN_NB_FILEOUTPUT {
                for n_filename in 0..GEN_NB_FILEOUTPUT {
                    for n_content_type in 0..GEN_NB_CHAR_PTR_PTR {
                        let mem_base = xml_mem_blocks();
                        let url = gen_fileoutput(n_url, 0);
                        let filename = gen_fileoutput(n_filename, 1);
                        let content_type = gen_char_ptr_ptr(n_content_type, 2);

                        let ret_val = xml_nanohttp_fetch(url, filename, content_type);
                        desret_int(ret_val);
                        des_fileoutput(n_url, url, 0);
                        des_fileoutput(n_filename, filename, 1);
                        des_char_ptr_ptr(n_content_type, content_type, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlNanoHTTPFetch",
                                xml_mem_blocks() - mem_base
                            );
                            eprint!(" {}", n_url);
                            eprint!(" {}", n_filename);
                            eprintln!(" {}", n_content_type);
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlNanoHTTPFetch()");
        }
    }

    #[test]
    fn test_xml_nano_httpinit() {
        #[cfg(feature = "http")]
        unsafe {
            let mut leaks = 0;

            let mem_base = xml_mem_blocks();

            xml_nanohttp_init();
            reset_last_error();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in xmlNanoHTTPInit",
                    xml_mem_blocks() - mem_base
                );
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlNanoHTTPInit()");
        }
    }

    #[test]
    fn test_xml_nano_httpmime_type() {
        #[cfg(feature = "http")]
        unsafe {
            let mut leaks = 0;

            for n_ctx in 0..GEN_NB_XML_NANO_HTTPCTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctx = gen_xml_nano_httpctxt_ptr(n_ctx, 0);

                let ret_val = xml_nanohttp_mime_type(ctx);
                desret_const_char_ptr(ret_val);
                des_xml_nano_httpctxt_ptr(n_ctx, ctx, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlNanoHTTPMimeType",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_ctx);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlNanoHTTPMimeType()"
            );
        }
    }

    #[test]
    fn test_xml_nano_httpopen() {
        #[cfg(feature = "http")]
        unsafe {
            let mut leaks = 0;

            for n_url in 0..GEN_NB_FILEPATH {
                for n_content_type in 0..GEN_NB_CHAR_PTR_PTR {
                    let mem_base = xml_mem_blocks();
                    let url = gen_filepath(n_url, 0);
                    let content_type = gen_char_ptr_ptr(n_content_type, 1);

                    let ret_val = xml_nanohttp_open(url, content_type);
                    desret_xml_nano_httpctxt_ptr(ret_val);
                    des_filepath(n_url, url, 0);
                    des_char_ptr_ptr(n_content_type, content_type, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlNanoHTTPOpen",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_url);
                        eprintln!(" {}", n_content_type);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlNanoHTTPOpen()");
        }
    }

    #[test]
    fn test_xml_nano_httpopen_redir() {
        #[cfg(feature = "http")]
        unsafe {
            let mut leaks = 0;

            for n_url in 0..GEN_NB_FILEPATH {
                for n_content_type in 0..GEN_NB_CHAR_PTR_PTR {
                    for n_redir in 0..GEN_NB_CHAR_PTR_PTR {
                        let mem_base = xml_mem_blocks();
                        let url = gen_filepath(n_url, 0);
                        let content_type = gen_char_ptr_ptr(n_content_type, 1);
                        let redir = gen_char_ptr_ptr(n_redir, 2);

                        let ret_val = xml_nanohttp_open_redir(url, content_type, redir);
                        desret_xml_nano_httpctxt_ptr(ret_val);
                        des_filepath(n_url, url, 0);
                        des_char_ptr_ptr(n_content_type, content_type, 1);
                        des_char_ptr_ptr(n_redir, redir, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlNanoHTTPOpenRedir",
                                xml_mem_blocks() - mem_base
                            );
                            eprint!(" {}", n_url);
                            eprint!(" {}", n_content_type);
                            eprintln!(" {}", n_redir);
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlNanoHTTPOpenRedir()"
            );
        }
    }

    #[test]
    fn test_xml_nano_httpread() {
        #[cfg(feature = "http")]
        unsafe {
            let mut leaks = 0;

            for n_ctx in 0..GEN_NB_XML_NANO_HTTPCTXT_PTR {
                for n_dest in 0..GEN_NB_VOID_PTR {
                    for n_len in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let ctx = gen_xml_nano_httpctxt_ptr(n_ctx, 0);
                        let dest = gen_void_ptr(n_dest, 1);
                        let len = gen_int(n_len, 2);

                        let ret_val = xml_nanohttp_read(ctx, dest, len);
                        desret_int(ret_val);
                        des_xml_nano_httpctxt_ptr(n_ctx, ctx, 0);
                        des_void_ptr(n_dest, dest, 1);
                        des_int(n_len, len, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlNanoHTTPRead",
                                xml_mem_blocks() - mem_base
                            );
                            eprint!(" {}", n_ctx);
                            eprint!(" {}", n_dest);
                            eprintln!(" {}", n_len);
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlNanoHTTPRead()");
        }
    }

    #[test]
    fn test_xml_nano_httpredir() {

        /* missing type support */
    }

    #[test]
    fn test_xml_nano_httpreturn_code() {
        #[cfg(feature = "http")]
        unsafe {
            let mut leaks = 0;

            for n_ctx in 0..GEN_NB_XML_NANO_HTTPCTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctx = gen_xml_nano_httpctxt_ptr(n_ctx, 0);

                let ret_val = xml_nanohttp_return_code(ctx);
                desret_int(ret_val);
                des_xml_nano_httpctxt_ptr(n_ctx, ctx, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlNanoHTTPReturnCode",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_ctx);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlNanoHTTPReturnCode()"
            );
        }
    }

    #[test]
    fn test_xml_nano_httpsave() {
        #[cfg(all(feature = "http", feature = "output"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_VOID_PTR {
                for n_filename in 0..GEN_NB_FILEOUTPUT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_void_ptr(n_ctxt, 0);
                    let filename = gen_fileoutput(n_filename, 1);

                    let ret_val = xml_nanohttp_save(ctxt, filename);
                    desret_int(ret_val);
                    des_void_ptr(n_ctxt, ctxt, 0);
                    des_fileoutput(n_filename, filename, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlNanoHTTPSave",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_filename);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlNanoHTTPSave()");
        }
    }

    // #[test]
    // fn test_xml_nano_httpscan_proxy() {
    //     #[cfg(feature = "http")]
    //     unsafe {
    //         for n_url in 0..GEN_NB_FILEPATH {
    //             let url = gen_filepath(n_url, 0);

    //             xml_nanohttp_scan_proxy(url);
    //             des_filepath(n_url, url, 0);
    //             reset_last_error();
    //         }
    //     }
    // }
}
