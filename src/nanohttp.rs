//! Provide methods and data structures for handling HTTP.  
//! This module is based on `libxml/nanohttp.h`, `nanohttp.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    borrow::Cow,
    fs::File,
    io::{self, stdout, ErrorKind, Read, Write},
    net::{TcpStream, ToSocketAddrs},
    str::from_utf8,
    sync::{
        atomic::{AtomicBool, AtomicI32, Ordering},
        Mutex,
    },
};

use url::Url;

use crate::{
    error::XmlErrorDomain,
    libxml::{xml_io::__xml_ioerr, xmlerror::XmlParserErrors},
};

const XML_NANO_HTTP_MAX_REDIR: usize = 10;
const XML_NANO_HTTP_CHUNK: usize = 4096;
const XML_NANO_HTTP_WRITE: usize = 1;
const XML_NANO_HTTP_READ: usize = 2;

#[derive(Debug)]
pub struct XmlNanoHTTPCtxt {
    protocol: Option<Cow<'static, str>>, /* the protocol name */
    hostname: Option<Cow<'static, str>>, /* the host name */
    port: i32,                           /* the port */
    path: Option<Cow<'static, str>>,     /* the path within the URL */
    query: Option<Cow<'static, str>>,    /* the query string */
    socket: Option<TcpStream>,
    state: i32,                              /* WRITE / READ / CLOSED */
    out: Vec<u8>,                            /* buffer sent (zero terminated) */
    outptr: usize,                           /* index within the buffer sent */
    input: Vec<u8>,                          /* the receiving buffer */
    content: usize,                          /* the start of the content */
    inptr: usize,                            /* the next byte to read from network */
    inrptr: usize,                           /* the next byte to give back to the client */
    inlen: usize,                            /* len of the input buffer */
    last: i32,                               /* return code for last operation */
    return_value: i32,                       /* the protocol return value */
    version: i32,                            /* the protocol version */
    content_length: i32,                     /* specified content length from HTTP header */
    content_type: Option<Cow<'static, str>>, /* the MIME type for the input */
    location: Option<Cow<'static, str>>,     /* the new URL in case of redirect */
    auth_header: Option<Cow<'static, str>>,  /* contents of {WWW,Proxy}-Authenticate header */
    encoding: Option<Cow<'static, str>>,     /* encoding extracted from the contentType */
    mime_type: Option<Cow<'static, str>>,    /* Mime-Type extracted from the contentType */
}

static INITIALIZED: AtomicBool = AtomicBool::new(false);
static PROXY: Mutex<String> = Mutex::new(String::new()); /* the proxy name if any */
static PROXY_PORT: AtomicI32 = AtomicI32::new(0); /* the proxy port if any */

/**
 * xmlNanoHTTPInit:
 *
 * Initialize the HTTP protocol layer.
 * Currently it just checks for proxy information
 */
pub fn xml_nanohttp_init() {
    if INITIALIZED.load(Ordering::Acquire) {
        return;
    }

    let lock = PROXY.lock().unwrap();
    if lock.is_empty() {
        // PROXY is locked in xml_nanohttp_scan_proxy,
        // so lock should be dropped at here.
        drop(lock);
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
pub fn xml_nanohttp_cleanup() {
    let mut p = PROXY.lock().unwrap();
    p.clear();
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
pub fn xml_nanohttp_scan_proxy(url: &str) {
    let mut p = PROXY.lock().unwrap();
    p.clear();
    PROXY_PORT.store(0, Ordering::Relaxed);

    let Some(uri) = Url::parse(url)
        .ok()
        .filter(|uri| uri.scheme() == "http" && uri.host_str().is_some())
    else {
        unsafe {
            __xml_ioerr(
                XmlErrorDomain::XmlFromHTTP,
                XmlParserErrors::XmlHttpUrlSyntax,
                c"Syntax Error\n".as_ptr() as _,
            );
        }
        return;
    };

    let host = uri.host_str().unwrap();
    p.push_str(host);
    if let Some(port) = uri.port() {
        PROXY_PORT.store(port as i32, Ordering::Release);
    }
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
fn xml_nanohttp_recv(ctxt: &mut XmlNanoHTTPCtxt) -> io::Result<usize> {
    let Some(stream) = ctxt.socket.as_mut() else {
        return Err(io::Error::new(ErrorKind::Other, "Socket is invalid."));
    };

    while ctxt.state & XML_NANO_HTTP_READ as i32 != 0 {
        if ctxt.input.is_empty() {
            ctxt.input = vec![0; 65000];
            ctxt.inlen = 65000;
            ctxt.inptr = 0;
            ctxt.content = 0;
            ctxt.inrptr = 0;
        }
        if ctxt.inrptr > XML_NANO_HTTP_CHUNK {
            let delta = ctxt.inrptr;
            assert!(ctxt.inptr >= ctxt.inrptr);
            let len = ctxt.inptr - ctxt.inrptr;

            ctxt.input.copy_within(ctxt.inrptr..ctxt.inrptr + len, 0);
            ctxt.inrptr -= delta;
            ctxt.content -= delta;
            ctxt.inptr -= delta;
        }
        if ctxt.inlen < ctxt.inptr + XML_NANO_HTTP_CHUNK {
            ctxt.inlen *= 2;
            ctxt.input.resize(ctxt.inlen, 0);
        }
        match stream.read(&mut ctxt.input[ctxt.inptr..ctxt.inptr + XML_NANO_HTTP_CHUNK]) {
            Ok(len @ 1..) => {
                ctxt.inptr += len;
                ctxt.last = len as i32;
                return Ok(len);
            }
            Ok(0) => {
                return Ok(0);
            }
            Err(e) => {
                match e.kind() {
                    ErrorKind::WouldBlock | ErrorKind::TimedOut => {
                        // This pattern covers `EAGAIN`
                    }
                    ErrorKind::ConnectionReset | ErrorKind::ConnectionAborted => {
                        return Ok(0);
                    }
                    _ => unsafe {
                        __xml_ioerr(
                            XmlErrorDomain::XmlFromHTTP,
                            XmlParserErrors::default(),
                            c"recv failed\n".as_ptr() as _,
                        );
                        return Err(e);
                    },
                }
            }
        }
    }
    Ok(0)
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
fn xml_nanohttp_fetch_content(ctxt: &mut XmlNanoHTTPCtxt, ptr: &mut usize, len: &mut usize) -> i32 {
    let mut rcvd_lgth = ctxt.inptr - ctxt.content;

    while let Some(cur_lgth) = xml_nanohttp_recv(ctxt).ok().filter(|&len| len > 0) {
        rcvd_lgth += cur_lgth;
        if ctxt.content_length > 0 && rcvd_lgth >= ctxt.content_length as usize {
            break;
        }
    }

    *ptr = ctxt.content;
    *len = rcvd_lgth;

    if (ctxt.content_length > 0 && rcvd_lgth < ctxt.content_length as usize) || rcvd_lgth == 0 {
        -1
    } else {
        0
    }
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
pub fn xml_nanohttp_fetch(
    url: &str,
    filename: &str,
    content_type: &mut Option<Cow<'static, str>>,
) -> io::Result<()> {
    let mut len = 0;
    let mut ctxt = xml_nanohttp_open(url, content_type)?;

    let mut writer: Box<dyn Write> = if filename == "-" {
        Box::new(stdout())
    } else {
        let file = File::options()
            .create(true)
            .truncate(true)
            .read(true)
            .write(true)
            .open(filename)
            .inspect_err(|_| *content_type = None)?;
        Box::new(file)
    };

    let mut buf = 0;
    xml_nanohttp_fetch_content(&mut ctxt, &mut buf, &mut len);
    if len > 0 {
        writer.write_all(&ctxt.input[buf..buf + len])?;
    }

    Ok(())
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
pub fn xml_nanohttp_method(
    url: &str,
    method: Option<&str>,
    input: Option<&str>,
    content_type: &mut Option<Cow<'static, str>>,
    headers: Option<&str>,
) -> io::Result<XmlNanoHTTPCtxt> {
    xml_nanohttp_method_redir(url, method, input, content_type, &mut None, headers)
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

    if uri.host().is_none() {
        return;
    };

    ctxt.protocol = Some(uri.scheme().to_owned().into());

    /* special case of IPv6 addresses, the [] need to be removed */
    let host = uri.host_str().unwrap();
    ctxt.hostname = Some(host.to_owned().into());
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
fn xml_nanohttp_new_ctxt(url: &str) -> XmlNanoHTTPCtxt {
    let mut ret = XmlNanoHTTPCtxt {
        protocol: None,
        hostname: None,
        port: 0,
        path: None,
        query: None,
        socket: None,
        state: 0,
        out: vec![],
        outptr: 0,
        input: vec![],
        content: 0,
        inptr: 0,
        inrptr: 0,
        inlen: 0,
        last: 0,
        return_value: 0,
        version: 0,
        content_length: 0,
        content_type: None,
        location: None,
        auth_header: None,
        encoding: None,
        mime_type: None,
    };
    ret.port = 80;
    ret.return_value = 0;
    ret.content_length = -1;

    xml_nanohttp_scan_url(&mut ret, url);

    ret
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
fn xml_nanohttp_connect_host(host: &str, port: i32) -> io::Result<TcpStream> {
    let host = format!("{host}:{port}");
    for addr in host.to_socket_addrs()? {
        if let Ok(stream) = TcpStream::connect(addr) {
            stream.set_nonblocking(true)?;
            return Ok(stream);
        }
    }
    Err(io::Error::new(
        ErrorKind::InvalidInput,
        "Cannot convert `host` to socket addrs.",
    ))
}

/**
 * xmlNanoHTTPSend:
 * @ctxt:  an HTTP context
 *
 * Send the input needed to initiate the processing on the server side
 * Returns number of bytes sent or -1 on error.
 */
fn xml_nanohttp_send(ctxt: &mut XmlNanoHTTPCtxt, mut buf: &[u8]) -> i32 {
    let mut total_sent = 0;

    if ctxt.state & XML_NANO_HTTP_WRITE as i32 != 0 {
        let Some(socket) = ctxt.socket.as_mut() else {
            unsafe {
                __xml_ioerr(
                    XmlErrorDomain::XmlFromHTTP,
                    XmlParserErrors::default(),
                    c"send failed\n".as_ptr() as _,
                );
            }
            if total_sent == 0 {
                total_sent = -1;
            }
            return total_sent;
        };
        while !buf.is_empty() {
            match socket.write(buf) {
                Ok(len) => {
                    total_sent += len as i32;
                    buf = &buf[len..];
                }
                Err(e) => match e.kind() {
                    ErrorKind::WouldBlock | ErrorKind::TimedOut => {
                        continue;
                    }
                    _ => unsafe {
                        __xml_ioerr(
                            XmlErrorDomain::XmlFromHTTP,
                            XmlParserErrors::default(),
                            c"send failed\n".as_ptr() as _,
                        );
                        if total_sent == 0 {
                            total_sent = -1;
                        }
                        break;
                    },
                },
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
fn xml_nanohttp_read_line(ctxt: &mut XmlNanoHTTPCtxt) -> Option<Vec<u8>> {
    let mut buf: [u8; 4096] = [0; 4096];
    let mut bp = 0;

    while bp < buf.len() {
        if ctxt.inrptr == ctxt.inptr {
            match xml_nanohttp_recv(ctxt) {
                Ok(0) => {
                    if bp == 0 {
                        return None;
                    }
                    return Some(buf[..bp].to_vec());
                }
                Err(_) => return None,
                _ => {}
            }
        }
        buf[bp] = ctxt.input[ctxt.inrptr];
        ctxt.inrptr += 1;
        if buf[bp] == b'\n' {
            return Some(buf[..bp].to_vec());
        }
        if buf[bp] != b'\r' {
            bp += 1;
        }
    }
    Some(buf.to_vec())
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
fn xml_nanohttp_scan_answer(ctxt: &mut XmlNanoHTTPCtxt, line: &str) {
    if let Some(line) = line.strip_prefix("HTTP/") {
        let mut version = 0;
        let mut ret = 0;
        let mut cur = line.chars().peekable();

        while let Some(c) = cur.next_if(char::is_ascii_digit) {
            version *= 10;
            version += c as i32 - b'0' as i32;
        }
        if cur.next_if(|&c| c == '.').is_some() {
            if let Some(c) = cur.next_if(char::is_ascii_digit) {
                version *= 10;
                version += c as i32 - b'0' as i32;
            }
            while cur.next_if(char::is_ascii_digit).is_some() {}
        } else {
            version *= 10;
        }
        if !matches!(cur.peek(), Some(&' ') | Some(&'\t')) {
            return;
        }
        while cur.next_if(|&c| c == ' ' || c == '\t').is_some() {}
        if cur.peek().filter(|c| !c.is_ascii_digit()).is_none() {
            return;
        }
        while let Some(c) = cur.next_if(char::is_ascii_digit) {
            ret *= 10;
            ret += c as i32 - b'0' as i32;
        }
        if !matches!(cur.peek(), Some(&'\0' | &' ' | &'\t')) {
            return;
        }
        ctxt.return_value = ret;
        ctxt.version = version;
    } else if let Some(mut line) = line.strip_prefix("Content-Type:") {
        line = line.trim_start_matches([' ', '\t']);
        let base = line;
        ctxt.content_type = Some(base.to_owned().into());
        if let Some((mime, _)) = base.split_once(['\0', ' ', '\t', ';', ',']) {
            ctxt.mime_type = Some(mime.to_owned().into());
        } else {
            ctxt.mime_type = Some(base.to_owned().into());
        }
        if let Some(index) = base.find("charset=") {
            let charset = base[index..].strip_prefix("charset=").unwrap();
            let charset = charset
                .split_once(['\0', ' ', '\t', ';', ','])
                .unwrap_or((charset, ""))
                .0;
            ctxt.encoding = Some(charset.to_owned().into());
        }
    } else if let Some(mut line) = line.strip_prefix("ContentType:") {
        line = line.trim_start_matches([' ', '\t']);
        let base = line;
        ctxt.content_type = Some(base.to_owned().into());
        if let Some((mime, _)) = base.split_once(['\0', ' ', '\t', ';', ',']) {
            ctxt.mime_type = Some(mime.to_owned().into());
        } else {
            ctxt.mime_type = Some(base.to_owned().into());
        }
        if let Some(index) = base.find("charset=") {
            let charset = base[index..].strip_prefix("charset=").unwrap();
            let charset = charset
                .split_once(['\0', ' ', '\t', ';', ','])
                .unwrap_or((charset, ""))
                .0;
            ctxt.encoding = Some(charset.to_owned().into());
        }
    } else if let Some(mut line) = line.strip_prefix("Location:") {
        line = line.trim_start_matches([' ', '\t']);
        if line.starts_with('/') {
            let loc = format!("http://{}{line}", ctxt.hostname.as_deref().unwrap_or(""));
            ctxt.location = Some(loc.into());
        } else {
            ctxt.location = Some(line.to_owned().into());
        }
    } else if let Some(mut line) = line.strip_prefix("WWW-Authenticate:") {
        line = line.trim_start_matches([' ', '\t']);
        ctxt.auth_header = Some(line.to_owned().into());
    } else if let Some(mut line) = line.strip_prefix("Proxy-Authenticate:") {
        line = line.trim_start_matches([' ', '\t']);
        ctxt.auth_header = Some(line.to_owned().into());
    } else if let Some(mut line) = line.strip_prefix("Content-Length:") {
        line = line.trim();
        ctxt.content_length = line.parse().unwrap_or(0);
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
pub fn xml_nanohttp_method_redir(
    url: &str,
    method: Option<&str>,
    input: Option<&str>,
    content_type: &mut Option<Cow<'static, str>>,
    redir: &mut Option<String>,
    headers: Option<&str>,
) -> io::Result<XmlNanoHTTPCtxt> {
    let mut ctxt;
    let mut nb_redirects = 0;
    let mut redir_url = None::<String>;
    let method = method.unwrap_or("GET");

    xml_nanohttp_init();

    // retry:
    'retry: loop {
        if let Some(redir_url) = redir_url.as_deref() {
            ctxt = xml_nanohttp_new_ctxt(redir_url);
            ctxt.location = Some(redir_url.to_owned().into());
        } else {
            ctxt = xml_nanohttp_new_ctxt(url);
        }

        if ctxt.protocol != Some("http".into()) {
            unsafe {
                __xml_ioerr(
                    XmlErrorDomain::XmlFromHTTP,
                    XmlParserErrors::XmlHttpUrlSyntax,
                    c"Not a valid HTTP URI".as_ptr() as _,
                );
                return Err(io::Error::new(
                    ErrorKind::InvalidInput,
                    "Not a valid HTTP URI",
                ));
            }
        }
        let Some(hostname) = ctxt.hostname.as_ref() else {
            unsafe {
                __xml_ioerr(
                    XmlErrorDomain::XmlFromHTTP,
                    XmlParserErrors::XmlHttpUnknownHost,
                    c"Failed to identify host in URI".as_ptr() as _,
                );
            }
            return Err(io::Error::new(
                ErrorKind::AddrNotAvailable,
                "Failed to identify host in URI",
            ));
        };
        let proxy = PROXY.lock().unwrap();
        let use_proxy = !proxy.is_empty() && !xml_nanohttp_bypass_proxy(hostname.as_ref());
        let (mut blen, ret) = if use_proxy {
            let blen = hostname.len() * 2 + 16;
            let ret = xml_nanohttp_connect_host(&proxy, PROXY_PORT.load(Ordering::Relaxed) as _)?;
            (blen, ret)
        } else {
            let blen = hostname.len();
            let ret = xml_nanohttp_connect_host(hostname.as_ref(), ctxt.port)?;
            (blen, ret)
        };
        ctxt.socket = Some(ret);

        if input.is_some() {
            blen += 36;
        }

        if let Some(headers) = headers.as_ref() {
            blen += headers.len() + 2;
        }
        if let Some(content_type) = content_type.as_deref() {
            /* reserve for string plus 'Content-Type: \r\n" */
            blen += content_type.len() + 16;
        }
        if let Some(query) = ctxt.query.as_deref() {
            /* 1 for '?' */
            blen += query.len() + 1;
        }
        blen += method.len() + ctxt.path.as_ref().map_or(0, |s| s.len()) + 24;
        if ctxt.port != 80 {
            /* reserve space for ':xxxxx', incl. potential proxy */
            if use_proxy {
                blen += 17;
            } else {
                blen += 11;
            }
        }
        let mut bp = Vec::with_capacity(blen);
        if use_proxy {
            let path = ctxt.path.as_ref().unwrap();
            if ctxt.port != 80 {
                write!(bp, "{method} http://{hostname}:{}{path}", ctxt.port)?;
            } else {
                write!(bp, "{method} http://{hostname}{path}")?;
            }
        } else {
            let path = ctxt.path.as_ref().unwrap();
            write!(bp, "{method} {path}")?;
        }

        if let Some(query) = ctxt.query.as_deref() {
            write!(bp, "?{query}")?;
        }

        if ctxt.port == 80 {
            write!(bp, " HTTP/1.0\r\nHost: {hostname}\r\n")?;
        } else {
            write!(bp, " HTTP/1.0\r\nHost: {hostname}:{}\r\n", ctxt.port)?;
        }

        if let Some(content_type) = content_type.as_deref() {
            write!(bp, "Content-Type: {content_type}\r\n")?;
        }

        if let Some(headers) = headers.as_ref() {
            write!(bp, "{headers}")?;
        }

        if let Some(input) = input.as_ref() {
            write!(bp, "Content-Length: {}\r\n\r\n", input.len())?;
        } else {
            write!(bp, "\r\n")?;
        }

        ctxt.outptr = 0;
        ctxt.state = XML_NANO_HTTP_WRITE as _;
        xml_nanohttp_send(&mut ctxt, &bp);
        ctxt.out = bp;

        if let Some(input) = input {
            xml_nanohttp_send(&mut ctxt, input.as_bytes());
        }

        ctxt.state = XML_NANO_HTTP_READ as _;

        while let Some(p) = xml_nanohttp_read_line(&mut ctxt) {
            if p.is_empty() {
                ctxt.content = ctxt.inrptr;
                break;
            }
            xml_nanohttp_scan_answer(&mut ctxt, from_utf8(&p).unwrap());
        }

        if let Some(location) = ctxt
            .location
            .as_deref()
            .filter(|_| ctxt.return_value >= 300 && ctxt.return_value < 400)
            .map(|s| s.to_owned())
        {
            while xml_nanohttp_recv(&mut ctxt)
                .ok()
                .filter(|&len| len > 0)
                .is_some()
            {}
            if nb_redirects < XML_NANO_HTTP_MAX_REDIR as i32 {
                nb_redirects += 1;
                redir_url = Some(location);
                continue 'retry;
            }

            return Err(io::Error::new(ErrorKind::NotFound, "Too many redirects"));
        }
        break;
    }

    *content_type = ctxt.content_type.clone();
    *redir = redir_url;

    Ok(ctxt)
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
pub fn xml_nanohttp_open(
    url: &str,
    content_type: &mut Option<Cow<'static, str>>,
) -> io::Result<XmlNanoHTTPCtxt> {
    xml_nanohttp_method(url, None, None, content_type, None)
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
pub fn xml_nanohttp_open_redir(
    url: &str,
    content_type: &mut Option<Cow<'static, str>>,
    redir: &mut Option<String>,
) -> io::Result<XmlNanoHTTPCtxt> {
    xml_nanohttp_method_redir(url, None, None, content_type, redir, None)
}

/**
 * xmlNanoHTTPReturnCode:
 * @ctx:  the HTTP context
 *
 * Get the latest HTTP return code received
 *
 * Returns the HTTP return code for the request.
 */
pub fn xml_nanohttp_return_code(ctxt: &mut XmlNanoHTTPCtxt) -> i32 {
    ctxt.return_value
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
pub fn xml_nanohttp_auth_header(ctxt: &mut XmlNanoHTTPCtxt) -> Option<String> {
    ctxt.auth_header.as_deref().map(|s| s.to_owned())
}

/**
 * xmlNanoHTTPRedir:
 * @ctx:  the HTTP context
 *
 * Provides the specified redirection URL if available from the HTTP header.
 *
 * Return the specified redirection URL or NULL if not redirected.
 */
pub fn xml_nanohttp_redir(ctxt: &mut XmlNanoHTTPCtxt) -> Option<String> {
    ctxt.location.as_deref().map(|s| s.to_owned())
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
pub fn xml_nanohttp_content_length(ctxt: &mut XmlNanoHTTPCtxt) -> i32 {
    ctxt.content_length
}

/**
 * xmlNanoHTTPEncoding:
 * @ctx:  the HTTP context
 *
 * Provides the specified encoding if specified in the HTTP headers.
 *
 * Return the specified encoding or NULL if not available
 */
pub fn xml_nanohttp_encoding(ctxt: &mut XmlNanoHTTPCtxt) -> Option<String> {
    ctxt.encoding.as_deref().map(|s| s.to_owned())
}

/**
 * xmlNanoHTTPMimeType:
 * @ctx:  the HTTP context
 *
 * Provides the specified Mime-Type if specified in the HTTP headers.
 *
 * Return the specified Mime-Type or NULL if not available
 */
pub fn xml_nanohttp_mime_type(ctxt: &mut XmlNanoHTTPCtxt) -> Option<String> {
    ctxt.mime_type.as_deref().map(|s| s.to_owned())
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
pub fn xml_nanohttp_read(ctxt: &mut XmlNanoHTTPCtxt, dest: &mut [u8]) -> usize {
    if dest.is_empty() {
        return 0;
    }

    let mut len = dest.len();
    while ctxt.inptr - ctxt.inrptr < len {
        if xml_nanohttp_recv(ctxt)
            .ok()
            .filter(|&len| len > 0)
            .is_none()
        {
            break;
        }
    }
    if ctxt.inptr - ctxt.inrptr < len {
        len = ctxt.inptr - ctxt.inrptr;
    }
    dest[..len].copy_from_slice(&ctxt.input[ctxt.inrptr..ctxt.inrptr + len]);
    ctxt.inrptr += len;
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
pub fn xml_nanohttp_save(ctxt: &mut XmlNanoHTTPCtxt, filename: &str) -> i32 {
    use std::{fs::Permissions, os::unix::fs::PermissionsExt};

    let mut len = 0;
    let mut ret = 0;

    let mut writer: Box<dyn Write> = if filename == "-" {
        Box::new(stdout())
    } else {
        let Ok(file) = File::options()
            .write(true)
            .read(true)
            .create(true)
            .truncate(true)
            .open(filename)
        else {
            return -1;
        };
        file.set_permissions(Permissions::from_mode(0o666)).ok();
        Box::new(file)
    };

    let mut buf = 0;
    xml_nanohttp_fetch_content(ctxt, &mut buf, &mut len);
    if len > 0 && writer.write_all(&ctxt.input[buf..]).is_err() {
        ret = -1;
    }

    ret
}
