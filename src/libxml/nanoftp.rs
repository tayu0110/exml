//! Provide methods and data structures for handling FTP.  
//! This module is based on `libxml/nanoftp.h`, `nanoftp.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    ffi::{c_char, c_int, c_uchar, c_uint, c_ulong},
    mem::{size_of, size_of_val, zeroed},
    os::raw::c_void,
    ptr::{addr_of, addr_of_mut, null, null_mut},
    sync::atomic::{AtomicBool, AtomicI32, AtomicPtr, Ordering},
};

use libc::{
    addrinfo, bind, close, connect, fd_set, freeaddrinfo, getaddrinfo, getenv, getsockname,
    hostent, in6_addr, listen, memcpy, memmove, memset, recv, select, send, snprintf, sockaddr_in,
    sockaddr_in6, sockaddr_storage, socket, socklen_t, sscanf, strchr, strcmp, strlen, strncmp,
    timeval, AF_INET, AF_INET6, FD_SET, FD_ZERO, IPPROTO_TCP, SOCK_STREAM,
};

use crate::{
    libxml::{
        globals::{xml_free, xml_malloc, xml_mem_strdup},
        uri::{xml_free_uri, xml_parse_uri_raw, xml_uri_unescape_string, XmlURIPtr},
        xml_io::__xml_ioerr,
        xmlerror::{XmlErrorDomain, XmlParserErrors},
        xmlstring::xml_strndup,
    },
    private::error::__xml_simple_error,
};

/**
 * SOCKET:
 *
 * macro used to provide portability of code to windows sockets
 */
type Socket = c_int;
/**
 * INVALID_SOCKET:
 *
 * macro used to provide portability of code to windows sockets
 * the value to be used when the socket is not valid
 */
const INVALID_SOCKET: i32 = -1;

type XmlSocklenT = c_uint;

/**
 * ftpListCallback:
 * @userData:  user provided data for the callback
 * @filename:  the file name (including "->" when links are shown)
 * @attrib:  the attribute string
 * @owner:  the owner string
 * @group:  the group string
 * @size:  the file size
 * @links:  the link count
 * @year:  the year
 * @month:  the month
 * @day:  the day
 * @hour:  the hour
 * @minute:  the minute
 *
 * A callback for the xmlNanoFTPList command.
 * Note that only one of year and day:minute are specified.
 */
pub type FtpListCallback = unsafe extern "C" fn(
    userData: *mut c_void,
    filename: *const c_char,
    attrib: *const c_char,
    owner: *const c_char,
    group: *const c_char,
    size: c_ulong,
    links: c_int,
    year: c_int,
    month: *const c_char,
    day: c_int,
    hour: c_int,
    minute: c_int,
);
/**
 * ftpDataCallback:
 * @userData: the user provided context
 * @data: the data received
 * @len: its size in bytes
 *
 * A callback for the xmlNanoFTPGet command.
 */
pub type FtpDataCallback =
    unsafe extern "C" fn(userData: *mut c_void, data: *const c_char, len: c_int);

// const FTP_COMMAND_OK: i32 = 200;
// const FTP_SYNTAX_ERROR: i32 = 500;
// const FTP_GET_PASSWD: i32 = 331;
const FTP_BUF_SIZE: usize = 1024;

// const XML_NANO_MAX_URLBUF: usize = 4096;

pub type XmlNanoFtpctxtPtr = *mut XmlNanoFtpctxt;
#[repr(C)]
pub struct XmlNanoFtpctxt {
    protocol: *mut c_char,      /* the protocol name */
    hostname: *mut c_char,      /* the host name */
    port: c_int,                /* the port */
    path: *mut c_char,          /* the path within the URL */
    user: *mut c_char,          /* user string */
    passwd: *mut c_char,        /* passwd string */
    ftp_addr: sockaddr_storage, /* this is large enough to hold IPv6 address*/
    passive: c_int,             /* currently we support only passive !!! */
    control_fd: Socket,         /* the file descriptor for the control socket */
    data_fd: Socket,            /* the file descriptor for the data socket */
    state: c_int,               /* WRITE / READ / CLOSED */
    return_value: c_int,        /* the protocol return value */
    /* buffer for data received from the control connection */
    control_buf: [c_char; FTP_BUF_SIZE + 1],
    control_buf_index: c_int,
    control_buf_used: c_int,
    control_buf_answer: c_int,
}

static INITIALIZED: AtomicBool = AtomicBool::new(false);
static PROXY: AtomicPtr<c_char> = AtomicPtr::new(null_mut()); /* the proxy name if any */
static PROXY_PORT: AtomicI32 = AtomicI32::new(0); /* the proxy port if any */
static PROXY_USER: AtomicPtr<c_char> = AtomicPtr::new(null_mut()); /* user for proxy authentication */
static PROXY_PASSWD: AtomicPtr<c_char> = AtomicPtr::new(null_mut()); /* passwd for proxy authentication */
static PROXY_TYPE: AtomicI32 = AtomicI32::new(0); /* uses TYPE or a@b ? */

/*
 * Init
 */
/**
 * xmlNanoFTPInit:
 *
 * Initialize the FTP protocol layer.
 * Currently it just checks for proxy information,
 * and get the hostname
 */
#[deprecated]
pub unsafe extern "C" fn xmlNanoFTPInit() {
    let mut env: *const c_char;
    // #ifdef _WINSOCKAPI_
    //     WSADATA wsaData;
    // #endif

    if INITIALIZED.load(Ordering::Acquire) {
        return;
    }

    // #ifdef _WINSOCKAPI_
    //     if (WSAStartup(MAKEWORD(1, 1), &wsaData) != 0)
    // 	return;
    // #endif

    PROXY_PORT.store(21, Ordering::Release);
    env = getenv(c"no_proxy".as_ptr() as _);
    if !env.is_null() && (*env.add(0) == b'*' as i8 && *env.add(1) == 0) {
        return;
    }
    env = getenv(c"ftp_proxy".as_ptr() as _);
    if !env.is_null() {
        xmlNanoFTPScanProxy(env);
    } else {
        env = getenv(c"FTP_PROXY".as_ptr() as _);
        if !env.is_null() {
            xmlNanoFTPScanProxy(env);
        }
    }
    env = getenv(c"ftp_proxy_user".as_ptr() as _);
    if !env.is_null() {
        PROXY_USER.store(xml_mem_strdup(env as _) as _, Ordering::Release);
    }
    env = getenv(c"ftp_proxy_password".as_ptr() as _);
    if !env.is_null() {
        PROXY_PASSWD.store(xml_mem_strdup(env as _) as _, Ordering::Release);
    }
    INITIALIZED.store(true, Ordering::Release);
}

/**
 * xmlNanoFTPCleanup:
 *
 * Cleanup the FTP protocol layer. This cleanup proxy information.
 */
#[deprecated]
pub unsafe extern "C" fn xmlNanoFTPCleanup() {
    let p = PROXY.load(Ordering::Acquire);
    if !p.is_null() {
        xml_free(p as _);
        PROXY.store(null_mut(), Ordering::Release);
    }
    let p = PROXY_USER.load(Ordering::Acquire);
    if !p.is_null() {
        xml_free(p as _);
        PROXY_USER.store(null_mut(), Ordering::Release);
    }
    let p = PROXY_PASSWD.load(Ordering::Acquire);
    if !p.is_null() {
        xml_free(p as _);
        PROXY_PASSWD.store(null_mut(), Ordering::Release);
    }
    // #ifdef _WINSOCKAPI_
    //     if (initialized)
    // 	WSACleanup();
    // #endif
    INITIALIZED.store(false, Ordering::Release);
}

/**
 * xmlFTPErrMemory:
 * @extra:  extra information
 *
 * Handle an out of memory condition
 */
unsafe extern "C" fn xml_ftp_err_memory(extra: *const c_char) {
    __xml_simple_error(
        XmlErrorDomain::XmlFromFtp as i32,
        XmlParserErrors::XmlErrNoMemory as i32,
        null_mut(),
        null(),
        extra,
    );
}

/**
 * xmlNanoFTPScanURL:
 * @ctx:  an FTP context
 * @URL:  The URL used to initialize the context
 *
 * (Re)Initialize an FTP context by parsing the URL and finding
 * the protocol host port and path it indicates.
 */

unsafe extern "C" fn xmlNanoFTPScanURL(ctx: *mut c_void, url: *const c_char) {
    let ctxt: XmlNanoFtpctxtPtr = ctx as XmlNanoFtpctxtPtr;

    /*
     * Clear any existing data from the context
     */
    if !(*ctxt).protocol.is_null() {
        xml_free((*ctxt).protocol as _);
        (*ctxt).protocol = null_mut();
    }
    if !(*ctxt).hostname.is_null() {
        xml_free((*ctxt).hostname as _);
        (*ctxt).hostname = null_mut();
    }
    if !(*ctxt).path.is_null() {
        xml_free((*ctxt).path as _);
        (*ctxt).path = null_mut();
    }
    if url.is_null() {
        return;
    }

    let uri: XmlURIPtr = xml_parse_uri_raw(url, 1);
    if uri.is_null() {
        return;
    }

    if (*uri).scheme.is_null() || (*uri).server.is_null() {
        xml_free_uri(uri);
        return;
    }

    (*ctxt).protocol = xml_mem_strdup((*uri).scheme as _) as _;
    (*ctxt).hostname = xml_mem_strdup((*uri).server as _) as _;
    if !(*uri).path.is_null() {
        (*ctxt).path = xml_mem_strdup((*uri).path as _) as _;
    } else {
        (*ctxt).path = xml_mem_strdup(c"/".as_ptr() as _) as _;
    }
    if (*uri).port != 0 {
        (*ctxt).port = (*uri).port;
    }

    if !(*uri).user.is_null() {
        let cptr: *mut c_char;
        let res = {
            cptr = strchr((*uri).user, b':' as i32);
            cptr.is_null()
        };
        if res {
            (*ctxt).user = xml_mem_strdup((*uri).user as _) as _;
        } else {
            (*ctxt).user = xml_strndup((*uri).user as _, cptr.offset_from((*uri).user) as _) as _;
            (*ctxt).passwd = xml_mem_strdup(cptr.add(1) as _) as _;
        }
    }

    xml_free_uri(uri);
}

/*
 * Creating/freeing contexts.
 */
/**
 * xmlNanoFTPNewCtxt:
 * @URL:  The URL used to initialize the context
 *
 * Allocate and initialize a new FTP context.
 *
 * Returns an FTP context or NULL in case of error.
 */
#[deprecated]
pub unsafe extern "C" fn xmlNanoFTPNewCtxt(url: *const c_char) -> *mut c_void {
    let ret: XmlNanoFtpctxtPtr = xml_malloc(size_of::<XmlNanoFtpctxt>()) as _;
    if ret.is_null() {
        xml_ftp_err_memory(c"allocating FTP context".as_ptr() as _);
        return null_mut();
    }

    memset(ret as _, 0, size_of::<XmlNanoFtpctxt>());
    (*ret).port = 21;
    (*ret).passive = 1;
    (*ret).return_value = 0;
    (*ret).control_buf_index = 0;
    (*ret).control_buf_used = 0;
    (*ret).control_fd = INVALID_SOCKET;

    let unescaped: *mut c_char = xml_uri_unescape_string(url, 0, null_mut());
    if !unescaped.is_null() {
        xmlNanoFTPScanURL(ret as _, unescaped);
        xml_free(unescaped as _);
    } else if !url.is_null() {
        xmlNanoFTPScanURL(ret as _, url);
    }

    ret as _
}

unsafe extern "C" fn closesocket(s: i32) -> i32 {
    close(s)
}

/**
 * xmlNanoFTPFreeCtxt:
 * @ctx:  an FTP context
 *
 * Frees the context after closing the connection.
 */
#[deprecated]
pub unsafe extern "C" fn xmlNanoFTPFreeCtxt(ctx: *mut c_void) {
    let ctxt: XmlNanoFtpctxtPtr = ctx as XmlNanoFtpctxtPtr;
    if ctxt.is_null() {
        return;
    }
    if !(*ctxt).hostname.is_null() {
        xml_free((*ctxt).hostname as _);
    }
    if !(*ctxt).protocol.is_null() {
        xml_free((*ctxt).protocol as _);
    }
    if !(*ctxt).path.is_null() {
        xml_free((*ctxt).path as _);
    }
    if !(*ctxt).user.is_null() {
        xml_free((*ctxt).user as _);
    }
    if !(*ctxt).passwd.is_null() {
        xml_free((*ctxt).passwd as _);
    }
    (*ctxt).passive = 1;
    if (*ctxt).control_fd != INVALID_SOCKET {
        closesocket((*ctxt).control_fd);
    }
    (*ctxt).control_fd = INVALID_SOCKET;
    (*ctxt).control_buf_index = -1;
    (*ctxt).control_buf_used = -1;
    xml_free(ctxt as _);
}

/**
 * xmlNanoFTPConnectTo:
 * @server:  an FTP server name
 * @port:  the port (use 21 if 0)
 *
 * Tries to open a control connection to the given server/port
 *
 * Returns an fTP context or NULL if it failed
 */
#[deprecated]
pub unsafe extern "C" fn xmlNanoFTPConnectTo(server: *const c_char, port: c_int) -> *mut c_void {
    xmlNanoFTPInit();
    if server.is_null() {
        return null_mut();
    }
    if port <= 0 {
        return null_mut();
    }
    let ctxt: XmlNanoFtpctxtPtr = xmlNanoFTPNewCtxt(null_mut()) as _;
    if ctxt.is_null() {
        return null_mut();
    }
    (*ctxt).hostname = xml_mem_strdup(server as _) as _;
    if (*ctxt).hostname.is_null() {
        xmlNanoFTPFreeCtxt(ctxt as _);
        return null_mut();
    }
    (*ctxt).port = port;
    let res: c_int = xmlNanoFTPConnect(ctxt as _);
    if res < 0 {
        xmlNanoFTPFreeCtxt(ctxt as _);
        return null_mut();
    }
    ctxt as _
}

/*
 * Opening/closing session connections.
 */
/**
 * xmlNanoFTPOpen:
 * @URL: the URL to the resource
 *
 * Start to fetch the given ftp:// resource
 *
 * Returns an FTP context, or NULL
 */
#[deprecated]
pub unsafe extern "C" fn xmlNanoFTPOpen(url: *const c_char) -> *mut c_void {
    xmlNanoFTPInit();
    if url.is_null() {
        return null_mut();
    }
    if strncmp(c"ftp://".as_ptr() as _, url, 6) != 0 {
        return null_mut();
    }

    let ctxt: XmlNanoFtpctxtPtr = xmlNanoFTPNewCtxt(url) as _;
    if ctxt.is_null() {
        return null_mut();
    }
    if xmlNanoFTPConnect(ctxt as _) < 0 {
        xmlNanoFTPFreeCtxt(ctxt as _);
        return null_mut();
    }
    let sock: Socket = xmlNanoFTPGetSocket(ctxt as _, (*ctxt).path);
    if sock == INVALID_SOCKET {
        xmlNanoFTPFreeCtxt(ctxt as _);
        return null_mut();
    }
    ctxt as _
}

unsafe extern "C" fn have_ipv6() -> bool {
    let s: c_int = socket(AF_INET6, SOCK_STREAM, 0);
    if s != -1 {
        close(s);
        return true;
    }
    false
}

/**
 * Send the user authentication
 */

unsafe extern "C" fn xmlNanoFTPSendUser(ctx: *mut c_void) -> c_int {
    let ctxt: XmlNanoFtpctxtPtr = ctx as XmlNanoFtpctxtPtr;
    let mut buf: [c_char; 200] = [0; 200];

    if (*ctxt).user.is_null() {
        snprintf(
            buf.as_mut_ptr() as _,
            buf.len(),
            c"USER anonymous\r\n".as_ptr() as _,
        );
    } else {
        snprintf(
            buf.as_mut_ptr() as _,
            buf.len(),
            c"USER %s\r\n".as_ptr() as _,
            (*ctxt).user,
        );
    }
    *buf.last_mut().unwrap() = 0;
    let len: c_int = strlen(buf.as_ptr() as _) as _;
    // #ifdef DEBUG_FTP
    //     xmlGenericError(xmlGenericErrorContext, c"%s".as_ptr() as _, buf);
    // #endif
    let res: c_int = send((*ctxt).control_fd, buf.as_ptr() as _, len as _, 0) as _;
    if res < 0 {
        __xml_ioerr(
            XmlErrorDomain::XmlFromFtp as i32,
            0,
            c"send failed".as_ptr() as _,
        );
        return res;
    }
    0
}

/**
 * Send the password authentication
 */
unsafe extern "C" fn xmlNanoFTPSendPasswd(ctx: *mut c_void) -> c_int {
    let ctxt: XmlNanoFtpctxtPtr = ctx as XmlNanoFtpctxtPtr;
    let mut buf: [c_char; 200] = [0; 200];

    if (*ctxt).passwd.is_null() {
        snprintf(
            buf.as_mut_ptr() as _,
            buf.len(),
            c"PASS anonymous@\r\n".as_ptr() as _,
        );
    } else {
        snprintf(
            buf.as_mut_ptr() as _,
            buf.len(),
            c"PASS %s\r\n".as_ptr() as _,
            (*ctxt).passwd,
        );
    }
    *buf.last_mut().unwrap() = 0;
    let len: c_int = strlen(buf.as_ptr() as _) as _;
    // #ifdef DEBUG_FTP
    //     xmlGenericError(xmlGenericErrorContext, c"%s".as_ptr() as _, buf);
    // #endif
    let res: c_int = send((*ctxt).control_fd, buf.as_ptr() as _, len as _, 0) as _;
    if res < 0 {
        __xml_ioerr(
            XmlErrorDomain::XmlFromFtp as i32,
            0,
            c"send failed".as_ptr() as _,
        );
        return res;
    }
    0
}

/**
 * xmlNanoFTPConnect:
 * @ctx:  an FTP context
 *
 * Tries to open a control connection
 *
 * Returns -1 in case of error, 0 otherwise
 */
#[deprecated]
pub unsafe extern "C" fn xmlNanoFTPConnect(ctx: *mut c_void) -> c_int {
    let ctxt: XmlNanoFtpctxtPtr = ctx as XmlNanoFtpctxtPtr;
    let hp: *mut hostent;
    let mut port: c_int;
    let mut res: c_int;
    let addrlen: usize;

    if ctxt.is_null() {
        return -1;
    }
    if (*ctxt).hostname.is_null() {
        return -1;
    }

    /*
     * do the blocking DNS query.
     */
    if !PROXY.load(Ordering::Relaxed).is_null() {
        port = PROXY_PORT.load(Ordering::Relaxed);
    } else {
        port = (*ctxt).port;
    }
    if port == 0 {
        port = 21;
    }

    memset(
        addr_of_mut!((*ctxt).ftp_addr) as _,
        0,
        size_of_val(&(*ctxt).ftp_addr),
    );

    if have_ipv6() {
        let mut hints: addrinfo = unsafe { zeroed() };
        let mut tmp: *mut addrinfo;
        let mut result: *mut addrinfo;

        result = null_mut();
        memset(addr_of_mut!(hints) as _, 0, size_of_val(&hints));
        hints.ai_socktype = SOCK_STREAM;

        if !PROXY.load(Ordering::Relaxed).is_null() {
            if getaddrinfo(
                PROXY.load(Ordering::Relaxed) as _,
                null_mut(),
                addr_of_mut!(hints) as _,
                addr_of_mut!(result),
            ) != 0
            {
                __xml_ioerr(
                    XmlErrorDomain::XmlFromFtp as i32,
                    0,
                    c"getaddrinfo failed".as_ptr() as _,
                );
                return -1;
            }
        } else if getaddrinfo(
            (*ctxt).hostname,
            null_mut(),
            addr_of_mut!(hints) as _,
            addr_of_mut!(result),
        ) != 0
        {
            __xml_ioerr(
                XmlErrorDomain::XmlFromFtp as i32,
                0,
                c"getaddrinfo failed".as_ptr() as _,
            );
            return -1;
        }

        tmp = result;
        while !tmp.is_null() {
            if (*tmp).ai_family == AF_INET || (*tmp).ai_family == AF_INET6 {
                break;
            }
            tmp = (*tmp).ai_next;
        }

        if tmp.is_null() {
            if !result.is_null() {
                freeaddrinfo(result);
            }
            __xml_ioerr(
                XmlErrorDomain::XmlFromFtp as i32,
                0,
                c"getaddrinfo failed".as_ptr() as _,
            );
            return -1;
        }
        if (*tmp).ai_addrlen > size_of_val(&(*ctxt).ftp_addr) as u32 {
            if !result.is_null() {
                freeaddrinfo(result);
            }
            __xml_ioerr(
                XmlErrorDomain::XmlFromFtp as i32,
                0,
                c"gethostbyname address mismatch".as_ptr() as _,
            );
            return -1;
        }
        if (*tmp).ai_family == AF_INET6 {
            memcpy(
                addr_of_mut!((*ctxt).ftp_addr) as _,
                (*tmp).ai_addr as _,
                (*tmp).ai_addrlen as _,
            );
            // (*(addr_of_mut!((*ctxt).ftpAddr) as _ as *mut sockaddr_in6)).sin6_port = htons(port);
            (*(addr_of_mut!((*ctxt).ftp_addr) as *mut sockaddr_in6)).sin6_port = port.to_be() as _;
            (*ctxt).control_fd = socket(AF_INET6, SOCK_STREAM, 0);
        } else {
            memcpy(
                addr_of_mut!((*ctxt).ftp_addr) as _,
                (*tmp).ai_addr as _,
                (*tmp).ai_addrlen as _,
            );
            // (*(addr_of_mut!((*ctxt).ftpAddr) as _ as *mut sockaddr_in)).sin_port = htons(port);
            (*(addr_of_mut!((*ctxt).ftp_addr) as *mut sockaddr_in)).sin_port = port.to_be() as _;
            (*ctxt).control_fd = socket(AF_INET, SOCK_STREAM, 0);
        }
        addrlen = (*tmp).ai_addrlen as usize;
        freeaddrinfo(result);
    } else {
        extern "C" {
            fn gethostbyname(name: *const c_char) -> *mut hostent;
        }

        if !PROXY.load(Ordering::Relaxed).is_null() {
            hp = gethostbyname(PROXY.load(Ordering::Relaxed) as _);
        } else {
            hp = gethostbyname((*ctxt).hostname);
        }
        if hp.is_null() {
            __xml_ioerr(
                XmlErrorDomain::XmlFromFtp as i32,
                0,
                c"gethostbyname failed".as_ptr() as _,
            );
            return -1;
        }
        if (*hp).h_length as c_uint
            > size_of_val(&(*(addr_of_mut!((*ctxt).ftp_addr) as *mut sockaddr_in)).sin_addr) as u32
        {
            __xml_ioerr(
                XmlErrorDomain::XmlFromFtp as i32,
                0,
                c"gethostbyname address mismatch".as_ptr() as _,
            );
            return -1;
        }

        /*
         * Prepare the socket
         */
        (*(addr_of_mut!((*ctxt).ftp_addr) as *mut sockaddr_in)).sin_family = AF_INET as u16;
        memcpy(
            addr_of_mut!((*(addr_of_mut!((*ctxt).ftp_addr) as *mut sockaddr_in)).sin_addr) as _,
            *(*hp).h_addr_list.add(0) as _,
            (*hp).h_length as usize,
        );
        // (*(addr_of_mut!((*ctxt).ftpAddr) as _ as *mut sockaddr_in)).sin_port = htons(port as u16) as u16;
        (*(addr_of_mut!((*ctxt).ftp_addr) as *mut sockaddr_in)).sin_port = (port as u16).to_be();
        (*ctxt).control_fd = socket(AF_INET, SOCK_STREAM, 0);
        addrlen = size_of::<sockaddr_in>();
    }

    if (*ctxt).control_fd == INVALID_SOCKET {
        __xml_ioerr(
            XmlErrorDomain::XmlFromFtp as i32,
            0,
            c"socket failed".as_ptr() as _,
        );
        return -1;
    }

    /*
     * Do the connect.
     */
    if connect(
        (*ctxt).control_fd,
        addr_of_mut!((*ctxt).ftp_addr) as _,
        addrlen as _,
    ) < 0
    {
        __xml_ioerr(
            XmlErrorDomain::XmlFromFtp as i32,
            0,
            c"Failed to create a connection".as_ptr() as _,
        );
        closesocket((*ctxt).control_fd);
        (*ctxt).control_fd = INVALID_SOCKET;
        (*ctxt).control_fd = INVALID_SOCKET;
        return -1;
    }

    /*
     * Wait for the HELLO from the server.
     */
    res = xmlNanoFTPGetResponse(ctxt as _);
    if res != 2 {
        closesocket((*ctxt).control_fd);
        (*ctxt).control_fd = INVALID_SOCKET;
        (*ctxt).control_fd = INVALID_SOCKET;
        return -1;
    }

    /*
     * State diagram for the login operation on the FTP server
     *
     * Reference: RFC 959
     *
     *                       1
     * +---+   USER    +---+------------->+---+
     * | B |---------->| W | 2       ---->| E |
     * +---+           +---+------  |  -->+---+
     *                  | |       | | |
     *                3 | | 4,5   | | |
     *    --------------   -----  | | |
     *   |                      | | | |
     *   |                      | | | |
     *   |                 ---------  |
     *   |               1|     | |   |
     *   V                |     | |   |
     * +---+   PASS    +---+ 2  |  ------>+---+
     * |   |---------->| W |------------->| S |
     * +---+           +---+   ---------->+---+
     *                  | |   | |     |
     *                3 | |4,5| |     |
     *    --------------   --------   |
     *   |                    | |  |  |
     *   |                    | |  |  |
     *   |                 -----------
     *   |             1,3|   | |  |
     *   V                |  2| |  |
     * +---+   ACCT    +---+--  |   ----->+---+
     * |   |---------->| W | 4,5 -------->| F |
     * +---+           +---+------------->+---+
     *
     * Of course in case of using a proxy this get really nasty and is not
     * standardized at all :-(
     */
    if !PROXY.load(Ordering::Relaxed).is_null() {
        let mut len: c_int = 0;
        let mut buf: [c_char; 400] = [0; 400];

        if !PROXY_USER.load(Ordering::Relaxed).is_null() {
            /*
             * We need proxy auth
             */
            snprintf(
                buf.as_mut_ptr() as _,
                buf.len(),
                c"USER %s\r\n".as_ptr() as _,
                PROXY_USER.load(Ordering::Relaxed),
            );
            buf[buf.len() - 1] = 0;
            len = strlen(buf.as_ptr() as _) as _;
            // #ifdef DEBUG_FTP
            // 	    xmlGenericError(xmlGenericErrorContext, c"%s".as_ptr() as _, buf);
            // #endif
            res = send((*ctxt).control_fd, buf.as_ptr() as _, len as _, 0) as _;
            if res < 0 {
                __xml_ioerr(
                    XmlErrorDomain::XmlFromFtp as i32,
                    0,
                    c"send failed".as_ptr() as _,
                );
                closesocket((*ctxt).control_fd);
                (*ctxt).control_fd = INVALID_SOCKET;
                return res;
            }
            res = xmlNanoFTPGetResponse(ctxt as _);
            match res {
                /* Falls through. */
                r @ 2 | r @ 3 => {
                    if r == 2 && PROXY_PASSWD.load(Ordering::Relaxed).is_null() {
                        // break;
                    } else {
                        if !PROXY_PASSWD.load(Ordering::Relaxed).is_null() {
                            snprintf(
                                buf.as_mut_ptr() as _,
                                buf.len(),
                                c"PASS %s\r\n".as_ptr() as _,
                                PROXY_PASSWD.load(Ordering::Relaxed),
                            );
                        } else {
                            snprintf(
                                buf.as_mut_ptr() as _,
                                buf.len(),
                                c"PASS anonymous@\r\n".as_ptr() as _,
                            );
                            buf[buf.len() - 1] = 0;
                            len = strlen(buf.as_ptr() as _) as _;
                        }
                        // #ifdef DEBUG_FTP
                        // 		    xmlGenericError(xmlGenericErrorContext, c"%s".as_ptr() as _, buf);
                        // #endif
                        res = send((*ctxt).control_fd, buf.as_ptr() as _, len as _, 0) as _;
                        if res < 0 {
                            __xml_ioerr(
                                XmlErrorDomain::XmlFromFtp as i32,
                                0,
                                c"send failed".as_ptr() as _,
                            );
                            closesocket((*ctxt).control_fd);
                            (*ctxt).control_fd = INVALID_SOCKET;
                            return res;
                        }
                        res = xmlNanoFTPGetResponse(ctxt as _);
                        if res > 3 {
                            closesocket((*ctxt).control_fd);
                            (*ctxt).control_fd = INVALID_SOCKET;
                            return -1;
                        }
                    }
                }
                1 => {}
                4 | 5 | -1 | _ => {
                    closesocket((*ctxt).control_fd);
                    (*ctxt).control_fd = INVALID_SOCKET;
                    return -1;
                }
            }
        }

        /*
         * We assume we don't need more authentication to the proxy
         * and that it succeeded :-\
         */
        match PROXY_TYPE.load(Ordering::Relaxed) {
            p @ 0 | p @ 1 | p @ 2 | p @ 3 | p => {
                let mut need_break = false;
                if p == 0 || p == 1 {
                    /* Using SITE command */
                    snprintf(
                        buf.as_mut_ptr() as _,
                        buf.len(),
                        c"SITE %s\r\n".as_ptr() as _,
                        (*ctxt).hostname,
                    );
                    buf[buf.len() - 1] = 0;
                    len = strlen(buf.as_ptr() as _) as _;
                    // #ifdef DEBUG_FTP
                    // 		xmlGenericError(xmlGenericErrorContext, c"%s".as_ptr() as _, buf);
                    // #endif
                    res = send((*ctxt).control_fd, buf.as_ptr() as _, len as _, 0) as _;
                    if res < 0 {
                        __xml_ioerr(
                            XmlErrorDomain::XmlFromFtp as i32,
                            0,
                            c"send failed".as_ptr() as _,
                        );
                        closesocket((*ctxt).control_fd);
                        (*ctxt).control_fd = INVALID_SOCKET;
                        (*ctxt).control_fd = INVALID_SOCKET;
                        return res;
                    }
                    res = xmlNanoFTPGetResponse(ctxt as _);
                    if res == 2 {
                        /* we assume it worked :-\ 1 is error for SITE command */
                        PROXY_TYPE.store(1, Ordering::Relaxed);
                        need_break = true;
                    } else if PROXY_TYPE.load(Ordering::Relaxed) == 1 {
                        closesocket((*ctxt).control_fd);
                        (*ctxt).control_fd = INVALID_SOCKET;
                        (*ctxt).control_fd = INVALID_SOCKET;
                        return -1;
                    }
                }
                if !need_break {
                    if p == 0 || p == 1 || p == 2 {
                        /* USER user@host command */
                        if (*ctxt).user.is_null() {
                            snprintf(
                                buf.as_mut_ptr() as _,
                                buf.len(),
                                c"USER anonymous@%s\r\n".as_ptr() as _,
                                (*ctxt).hostname,
                            );
                        } else {
                            snprintf(
                                buf.as_mut_ptr() as _,
                                buf.len(),
                                c"USER %s@%s\r\n".as_ptr() as _,
                                (*ctxt).user,
                                (*ctxt).hostname,
                            );
                            buf[buf.len() - 1] = 0;
                            len = strlen(buf.as_ptr() as _) as _;
                        }
                        // #ifdef DEBUG_FTP
                        // 		xmlGenericError(xmlGenericErrorContext, c"%s".as_ptr() as _, buf);
                        // #endif
                        res = send((*ctxt).control_fd, buf.as_ptr() as _, len as _, 0) as _;
                        if res < 0 {
                            __xml_ioerr(
                                XmlErrorDomain::XmlFromFtp as i32,
                                0,
                                c"send failed".as_ptr() as _,
                            );
                            closesocket((*ctxt).control_fd);
                            (*ctxt).control_fd = INVALID_SOCKET;
                            (*ctxt).control_fd = INVALID_SOCKET;
                            return res;
                        }
                        res = xmlNanoFTPGetResponse(ctxt as _);
                        if res == 1 || res == 2 {
                            /* we assume it worked :-\ */
                            PROXY_TYPE.store(2, Ordering::Relaxed);
                            return 0;
                        }
                        if (*ctxt).passwd.is_null() {
                            snprintf(
                                buf.as_mut_ptr() as _,
                                buf.len(),
                                c"PASS anonymous@\r\n".as_ptr() as _,
                            );
                        } else {
                            snprintf(
                                buf.as_mut_ptr() as _,
                                buf.len(),
                                c"PASS %s\r\n".as_ptr() as _,
                                (*ctxt).passwd,
                            );
                            buf[buf.len() - 1] = 0;
                            len = strlen(buf.as_ptr() as _) as _;
                        }
                        // #ifdef DEBUG_FTP
                        // 		xmlGenericError(xmlGenericErrorContext, c"%s".as_ptr() as _, buf);
                        // #endif
                        res = send((*ctxt).control_fd, buf.as_ptr() as _, len as _, 0) as _;
                        if res < 0 {
                            __xml_ioerr(
                                XmlErrorDomain::XmlFromFtp as i32,
                                0,
                                c"send failed".as_ptr() as _,
                            );
                            closesocket((*ctxt).control_fd);
                            (*ctxt).control_fd = INVALID_SOCKET;
                            (*ctxt).control_fd = INVALID_SOCKET;
                            return res;
                        }
                        res = xmlNanoFTPGetResponse(ctxt as _);
                        if res == 1 || res == 2 {
                            /* we assume it worked :-\ */
                            PROXY_TYPE.store(2, Ordering::Relaxed);
                            return 0;
                        }
                        if PROXY_TYPE.load(Ordering::Relaxed) == 2 {
                            closesocket((*ctxt).control_fd);
                            (*ctxt).control_fd = INVALID_SOCKET;
                            (*ctxt).control_fd = INVALID_SOCKET;
                            return -1;
                        }
                    }
                    /*
                     * If you need support for other Proxy authentication scheme
                     * send the code or at least the sequence in use.
                     */
                    closesocket((*ctxt).control_fd);
                    (*ctxt).control_fd = INVALID_SOCKET;
                    (*ctxt).control_fd = INVALID_SOCKET;
                    return -1;
                }
            }
        }
    }
    /*
     * Non-proxy handling.
     */
    res = xmlNanoFTPSendUser(ctxt as _);
    if res < 0 {
        closesocket((*ctxt).control_fd);
        (*ctxt).control_fd = INVALID_SOCKET;
        (*ctxt).control_fd = INVALID_SOCKET;
        return -1;
    }
    res = xmlNanoFTPGetResponse(ctxt as _);
    match res {
        2 => {
            return 0;
        }
        3 => {}
        1 | 4 | 5 | -1 | _ => {
            closesocket((*ctxt).control_fd);
            (*ctxt).control_fd = INVALID_SOCKET;
            (*ctxt).control_fd = INVALID_SOCKET;
            return -1;
        }
    }
    res = xmlNanoFTPSendPasswd(ctxt as _);
    if res < 0 {
        closesocket((*ctxt).control_fd);
        (*ctxt).control_fd = INVALID_SOCKET;
        (*ctxt).control_fd = INVALID_SOCKET;
        return -1;
    }
    res = xmlNanoFTPGetResponse(ctxt as _);
    match res {
        2 => {}
        r @ 3 | r @ 1 | r @ 4 | r @ 5 | r @ -1 | r => {
            if r == 3 {
                __xml_ioerr(
                    XmlErrorDomain::XmlFromFtp as i32,
                    XmlParserErrors::XmlFtpAccnt as i32,
                    c"FTP server asking for ACCNT on anonymous\n".as_ptr() as _,
                );
            }

            closesocket((*ctxt).control_fd);
            (*ctxt).control_fd = INVALID_SOCKET;
            (*ctxt).control_fd = INVALID_SOCKET;
            return -1;
        }
    }

    0
}

/**
 * xmlNanoFTPClose:
 * @ctx: an FTP context
 *
 * Close the connection and both control and transport
 *
 * Returns -1 in case of error, 0 otherwise
 */
#[deprecated]
pub unsafe extern "C" fn xmlNanoFTPClose(ctx: *mut c_void) -> c_int {
    let ctxt: XmlNanoFtpctxtPtr = ctx as XmlNanoFtpctxtPtr;

    if ctxt.is_null() {
        return -1;
    }

    if (*ctxt).data_fd != INVALID_SOCKET {
        closesocket((*ctxt).data_fd);
        (*ctxt).data_fd = INVALID_SOCKET;
    }
    if (*ctxt).control_fd != INVALID_SOCKET {
        xmlNanoFTPQuit(ctxt as _);
        closesocket((*ctxt).control_fd);
        (*ctxt).control_fd = INVALID_SOCKET;
    }
    xmlNanoFTPFreeCtxt(ctxt as _);
    0
}

/**
 * xmlNanoFTPQuit:
 * @ctx:  an FTP context
 *
 * Send a QUIT command to the server
 *
 * Returns -1 in case of error, 0 otherwise
 */
#[deprecated]
pub unsafe extern "C" fn xmlNanoFTPQuit(ctx: *mut c_void) -> c_int {
    let ctxt: XmlNanoFtpctxtPtr = ctx as XmlNanoFtpctxtPtr;
    let mut buf: [c_char; 200] = [0; 200];

    if ctxt.is_null() || (*ctxt).control_fd == INVALID_SOCKET {
        return -1;
    }

    snprintf(buf.as_mut_ptr() as _, buf.len(), c"QUIT\r\n".as_ptr() as _);
    let len: c_int = strlen(buf.as_ptr() as _) as _;
    // #ifdef DEBUG_FTP
    //     xmlGenericError(xmlGenericErrorContext, c"%s".as_ptr() as _, buf); /* Just to be consistent, even though we know it can't have a % in it */
    // #endif
    let res: c_int = send((*ctxt).control_fd, buf.as_ptr() as _, len as _, 0) as _;
    if res < 0 {
        __xml_ioerr(
            XmlErrorDomain::XmlFromFtp as i32,
            0,
            c"send failed".as_ptr() as _,
        );
        return res;
    }
    0
}

/**
 * xmlNanoFTPScanProxy:
 * @URL:  The proxy URL used to initialize the proxy context
 *
 * (Re)Initialize the FTP Proxy context by parsing the URL and finding
 * the protocol host port it indicates.
 * Should be like ftp://myproxy/ or ftp://myproxy:3128/
 * A NULL URL cleans up proxy information.
 */
#[deprecated]
pub unsafe extern "C" fn xmlNanoFTPScanProxy(URL: *const c_char) {
    let p = PROXY.load(Ordering::Acquire);
    if !p.is_null() {
        xml_free(p as _);
        PROXY.store(null_mut(), Ordering::Release);
    }
    PROXY_PORT.store(0, Ordering::Relaxed);

    // #ifdef DEBUG_FTP
    //     if URL.is_null()
    // 	xmlGenericError(xmlGenericErrorContext,
    // 		c"Removing FTP proxy info\n".as_ptr() as _);
    //     else
    // 	xmlGenericError(xmlGenericErrorContext,
    // 		c"Using FTP proxy %s\n".as_ptr() as _, URL);
    // #endif
    if URL.is_null() {
        return;
    }

    let uri: XmlURIPtr = xml_parse_uri_raw(URL, 1);
    if uri.is_null()
        || (*uri).scheme.is_null()
        || strcmp((*uri).scheme, c"ftp".as_ptr() as _) != 0
        || (*uri).server.is_null()
    {
        __xml_ioerr(
            XmlErrorDomain::XmlFromFtp as i32,
            XmlParserErrors::XmlFtpUrlSyntax as i32,
            c"Syntax Error\n".as_ptr() as _,
        );
        if !uri.is_null() {
            xml_free_uri(uri);
        }
        return;
    }

    PROXY.store(xml_mem_strdup((*uri).server as _) as _, Ordering::Release);
    if (*uri).port != 0 {
        PROXY_PORT.store((*uri).port, Ordering::Relaxed);
    }

    xml_free_uri(uri);
}

/**
 * xmlNanoFTPProxy:
 * @host:  the proxy host name
 * @port:  the proxy port
 * @user:  the proxy user name
 * @passwd:  the proxy password
 * @type:  the type of proxy 1 for using SITE, 2 for USER a@b
 *
 * Setup the FTP proxy information.
 * This can also be done by using ftp_proxy ftp_proxy_user and
 * ftp_proxy_password environment variables.
 */
#[deprecated]
pub unsafe extern "C" fn xmlNanoFTPProxy(
    host: *const c_char,
    port: c_int,
    user: *const c_char,
    passwd: *const c_char,
    typ: c_int,
) {
    let p = PROXY.load(Ordering::Acquire);
    if !p.is_null() {
        xml_free(p as _);
        PROXY.store(null_mut(), Ordering::Release);
    }
    let p = PROXY_USER.load(Ordering::Acquire);
    if !p.is_null() {
        xml_free(p as _);
        PROXY_USER.store(null_mut(), Ordering::Release);
    }
    let p = PROXY_PASSWD.load(Ordering::Acquire);
    if !p.is_null() {
        xml_free(p as _);
        PROXY_PASSWD.store(null_mut(), Ordering::Release);
    }
    if !host.is_null() {
        PROXY.store(xml_mem_strdup(host as _) as _, Ordering::Release);
    }
    if !user.is_null() {
        PROXY_USER.store(xml_mem_strdup(user as _) as _, Ordering::Release);
    }
    if !passwd.is_null() {
        PROXY_PASSWD.store(xml_mem_strdup(passwd as _) as _, Ordering::Release);
    }
    PROXY_PORT.store(port, Ordering::Release);
    PROXY_TYPE.store(typ, Ordering::Release);
}

/**
 * xmlNanoFTPUpdateURL:
 * @ctx:  an FTP context
 * @URL:  The URL used to update the context
 *
 * Update an FTP context by parsing the URL and finding
 * new path it indicates. If there is an error in the
 * protocol, hostname, port or other information, the
 * error is raised. It indicates a new connection has to
 * be established.
 *
 * Returns 0 if Ok, -1 in case of error (other host).
 */
#[deprecated]
pub unsafe extern "C" fn xmlNanoFTPUpdateURL(ctx: *mut c_void, url: *const c_char) -> c_int {
    let ctxt: XmlNanoFtpctxtPtr = ctx as XmlNanoFtpctxtPtr;

    if url.is_null() {
        return -1;
    }
    if ctxt.is_null() {
        return -1;
    }
    if (*ctxt).protocol.is_null() {
        return -1;
    }
    if (*ctxt).hostname.is_null() {
        return -1;
    }

    let uri: XmlURIPtr = xml_parse_uri_raw(url, 1);
    if uri.is_null() {
        return -1;
    }

    if (*uri).scheme.is_null() || (*uri).server.is_null() {
        xml_free_uri(uri);
        return -1;
    }
    if strcmp((*ctxt).protocol, (*uri).scheme) != 0
        || strcmp((*ctxt).hostname, (*uri).server) != 0
        || ((*uri).port != 0 && (*ctxt).port != (*uri).port)
    {
        xml_free_uri(uri);
        return -1;
    }

    if (*uri).port != 0 {
        (*ctxt).port = (*uri).port;
    }

    if !(*ctxt).path.is_null() {
        xml_free((*ctxt).path as _);
        (*ctxt).path = null_mut();
    }

    if (*uri).path.is_null() {
        (*ctxt).path = xml_mem_strdup(c"/".as_ptr() as _) as _;
    } else {
        (*ctxt).path = xml_mem_strdup((*uri).path as _) as _;
    }

    xml_free_uri(uri);

    0
}

/**
 * xmlNanoFTPGetMore:
 * @ctx:  an FTP context
 *
 * Read more information from the FTP control connection
 * Returns the number of bytes read, < 0 indicates an error
 */
unsafe extern "C" fn xmlNanoFTPGetMore(ctx: *mut c_void) -> c_int {
    let ctxt: XmlNanoFtpctxtPtr = ctx as XmlNanoFtpctxtPtr;
    let len: c_int;

    if ctxt.is_null() || (*ctxt).control_fd == INVALID_SOCKET {
        return -1;
    }

    if (*ctxt).control_buf_index < 0 || (*ctxt).control_buf_index > FTP_BUF_SIZE as i32 {
        // #ifdef DEBUG_FTP
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlNanoFTPGetMore : controlBufIndex = %d\n".as_ptr() as _,
        // 		(*ctxt).controlBufIndex);
        // #endif
        return -1;
    }

    if (*ctxt).control_buf_used < 0 || (*ctxt).control_buf_used > FTP_BUF_SIZE as i32 {
        // #ifdef DEBUG_FTP
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlNanoFTPGetMore : controlBufUsed = %d\n".as_ptr() as _,
        // 		(*ctxt).controlBufUsed);
        // #endif
        return -1;
    }
    if (*ctxt).control_buf_index > (*ctxt).control_buf_used {
        // #ifdef DEBUG_FTP
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlNanoFTPGetMore : controlBufIndex > controlBufUsed %d > %d\n".as_ptr() as _,
        // 	       (*ctxt).controlBufIndex, (*ctxt).controlBufUsed);
        // #endif
        return -1;
    }

    /*
     * First pack the control buffer
     */
    if (*ctxt).control_buf_index > 0 {
        memmove(
            (*ctxt).control_buf.as_ptr().add(0) as _,
            (*ctxt)
                .control_buf
                .as_ptr()
                .add((*ctxt).control_buf_index as usize) as _,
            (*ctxt).control_buf_used as usize - (*ctxt).control_buf_index as usize,
        );
        (*ctxt).control_buf_used -= (*ctxt).control_buf_index;
        (*ctxt).control_buf_index = 0;
    }
    let size: c_int = FTP_BUF_SIZE as i32 - (*ctxt).control_buf_used;
    if size == 0 {
        // #ifdef DEBUG_FTP
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlNanoFTPGetMore : buffer full %d \n".as_ptr() as _, (*ctxt).controlBufUsed);
        // #endif
        return 0;
    }

    /*
     * Read the amount left on the control connection
     */
    let res = {
        len = recv(
            (*ctxt).control_fd,
            (*ctxt)
                .control_buf
                .as_ptr()
                .add((*ctxt).control_buf_index as usize) as _,
            size as _,
            0,
        ) as i32;
        len < 0
    };
    if res {
        __xml_ioerr(
            XmlErrorDomain::XmlFromFtp as i32,
            0,
            c"recv failed".as_ptr() as _,
        );
        closesocket((*ctxt).control_fd);
        (*ctxt).control_fd = INVALID_SOCKET;
        (*ctxt).control_fd = INVALID_SOCKET;
        return -1;
    }
    // #ifdef DEBUG_FTP
    //     xmlGenericError(xmlGenericErrorContext,
    // 	    c"xmlNanoFTPGetMore : read %d [%d - %d]\n".as_ptr() as _, len,
    // 	   (*ctxt).controlBufUsed, (*ctxt).controlBufUsed + len);
    // #endif
    (*ctxt).control_buf_used += len;
    (*ctxt).control_buf[(*ctxt).control_buf_used as usize] = 0;

    len
}

/**
 * xmlNanoFTPParseResponse:
 * @buf:  the buffer containing the response
 * @len:  the buffer length
 *
 * Parsing of the server answer, we just extract the code.
 *
 * returns 0 for errors
 *     +XXX for last line of response
 *     -XXX for response to be continued
 */
unsafe extern "C" fn xmlNanoFTPParseResponse(mut buf: *mut c_char, len: c_int) -> c_int {
    let mut val: c_int = 0;

    if len < 3 {
        return -1;
    }
    if *buf >= b'0' as i8 && *buf <= b'9' as i8 {
        val = val * 10 + (*buf - b'0' as i8) as i32;
    } else {
        return 0;
    }
    buf = buf.add(1);
    if *buf >= b'0' as i8 && *buf <= b'9' as i8 {
        val = val * 10 + (*buf - b'0' as i8) as i32;
    } else {
        return 0;
    }
    buf = buf.add(1);
    if *buf >= b'0' as i8 && *buf <= b'9' as i8 {
        val = val * 10 + (*buf - b'0' as i8) as i32;
    } else {
        return 0;
    }
    buf = buf.add(1);
    if *buf == b'-' as i8 {
        return -val;
    }
    val
}

/**
 * xmlNanoFTPReadResponse:
 * @ctx:  an FTP context
 *
 * Read the response from the FTP server after a command.
 * Returns the code number
 */
unsafe extern "C" fn xmlNanoFTPReadResponse(ctx: *mut c_void) -> c_int {
    let ctxt: XmlNanoFtpctxtPtr = ctx as XmlNanoFtpctxtPtr;
    let mut ptr: *mut c_char;
    let mut end: *mut c_char;
    let mut len: c_int;
    let mut res: c_int = -1;
    let mut cur: c_int;

    if ctxt.is_null() || (*ctxt).control_fd == INVALID_SOCKET {
        return -1;
    }

    // get_more:
    'get_more: loop {
        /*
         * Assumes everything up to controlBuf[controlBufIndex] has been read
         * and analyzed.
         */
        len = xmlNanoFTPGetMore(ctx);
        if len < 0 {
            return -1;
        }
        if (*ctxt).control_buf_used == 0 && len == 0 {
            return -1;
        }
        ptr = (*ctxt)
            .control_buf
            .as_mut_ptr()
            .add((*ctxt).control_buf_index as usize);
        end = (*ctxt)
            .control_buf
            .as_mut_ptr()
            .add((*ctxt).control_buf_used as usize);

        // #ifdef DEBUG_FTP
        //     xmlGenericError(xmlGenericErrorContext,
        // 	    c"\n<<<\n%s\n--\n".as_ptr() as _, ptr);
        // #endif
        while ptr < end {
            cur = xmlNanoFTPParseResponse(ptr, end.offset_from(ptr) as _);
            if cur > 0 {
                /*
                 * Successfully scanned the control code, scratch
                 * till the end of the line, but keep the index to be
                 * able to analyze the result if needed.
                 */
                res = cur;
                ptr = ptr.add(3);
                (*ctxt).control_buf_answer =
                    ptr.offset_from((*ctxt).control_buf.as_ptr() as _) as _;
                while ptr < end && *ptr != b'\n' as i8 {
                    ptr = ptr.add(1);
                }
                if *ptr == b'\n' as i8 {
                    ptr = ptr.add(1);
                }
                if *ptr == b'\r' as i8 {
                    ptr = ptr.add(1);
                }
                break;
            }
            while ptr < end && *ptr != b'\n' as i8 {
                ptr = ptr.add(1);
            }
            if ptr >= end {
                (*ctxt).control_buf_index = (*ctxt).control_buf_used;
                // goto get_more;
                continue 'get_more;
            }
            if *ptr != b'\r' as i8 {
                ptr = ptr.add(1);
            }
        }

        if res < 0 {
            // goto get_more;
            continue 'get_more;
        }

        break;
    }
    (*ctxt).control_buf_index = ptr.offset_from((*ctxt).control_buf.as_ptr() as _) as _;
    // #ifdef DEBUG_FTP
    //     ptr = &(*ctxt).controlBuf[(*ctxt).controlBufIndex];
    //     xmlGenericError(xmlGenericErrorContext, c"\n---\n%s\n--\n".as_ptr() as _, ptr);
    // #endif

    // #ifdef DEBUG_FTP
    //     xmlGenericError(xmlGenericErrorContext, c"Got %d\n".as_ptr() as _, res);
    // #endif
    res / 100
}

/*
 * Rather internal commands.
 */
/**
 * xmlNanoFTPGetResponse:
 * @ctx:  an FTP context
 *
 * Get the response from the FTP server after a command.
 * Returns the code number
 */
#[deprecated]
pub unsafe extern "C" fn xmlNanoFTPGetResponse(ctx: *mut c_void) -> c_int {
    let res: c_int = xmlNanoFTPReadResponse(ctx as _);

    res
}

/**
 * xmlNanoFTPCheckResponse:
 * @ctx:  an FTP context
 *
 * Check if there is a response from the FTP server after a command.
 * Returns the code number, or 0
 */
#[deprecated]
pub unsafe extern "C" fn xmlNanoFTPCheckResponse(ctx: *mut c_void) -> c_int {
    let ctxt: XmlNanoFtpctxtPtr = ctx as XmlNanoFtpctxtPtr;
    let mut rfd: fd_set = unsafe { zeroed() };
    let mut tv: timeval = unsafe { zeroed() };

    if ctxt.is_null() || (*ctxt).control_fd == INVALID_SOCKET {
        return -1;
    }
    tv.tv_sec = 0;
    tv.tv_usec = 0;
    FD_ZERO(addr_of_mut!(rfd));
    FD_SET((*ctxt).control_fd, addr_of_mut!(rfd));
    match select(
        (*ctxt).control_fd + 1,
        addr_of_mut!(rfd),
        null_mut(),
        null_mut(),
        addr_of_mut!(tv),
    ) {
        0 => {
            return 0;
        }
        -1 => {
            __xml_ioerr(
                XmlErrorDomain::XmlFromFtp as i32,
                0,
                c"select".as_ptr() as _,
            );
            return -1;
        }
        _ => {}
    }

    xmlNanoFTPReadResponse(ctx)
}

/*
 * CD/DIR/GET handlers.
 */
/**
 * xmlNanoFTPCwd:
 * @ctx:  an FTP context
 * @directory:  a directory on the server
 *
 * Tries to change the remote directory
 *
 * Returns -1 in case of error, 1 if CWD worked, 0 if it failed
 */
#[deprecated]
pub unsafe extern "C" fn xmlNanoFTPCwd(ctx: *mut c_void, directory: *const c_char) -> c_int {
    let ctxt: XmlNanoFtpctxtPtr = ctx as XmlNanoFtpctxtPtr;
    let mut buf: [c_char; 400] = [0; 400];

    let mut res: c_int;

    if ctxt.is_null() || (*ctxt).control_fd == INVALID_SOCKET {
        return -1;
    }
    if directory.is_null() {
        return 0;
    }

    /*
     * Expected response code for CWD:
     *
     * CWD
     *     250
     *     500, 501, 502, 421, 530, 550
     */
    snprintf(
        buf.as_mut_ptr() as _,
        buf.len(),
        c"CWD %s\r\n".as_ptr() as _,
        directory,
    );
    *buf.last_mut().unwrap() = 0;
    let len: c_int = strlen(buf.as_ptr() as _) as _;
    // #ifdef DEBUG_FTP
    //     xmlGenericError(xmlGenericErrorContext, c"%s".as_ptr() as _, buf);
    // #endif
    res = send((*ctxt).control_fd, buf.as_ptr() as _, len as _, 0) as _;
    if res < 0 {
        __xml_ioerr(
            XmlErrorDomain::XmlFromFtp as i32,
            0,
            c"send failed".as_ptr() as _,
        );
        return res;
    }
    res = xmlNanoFTPGetResponse(ctxt as _);
    if res == 4 {
        return -1;
    }
    if res == 2 {
        return 1;
    }
    if res == 5 {
        return 0;
    }
    0
}

/**
 * xmlNanoFTPDele:
 * @ctx:  an FTP context
 * @file:  a file or directory on the server
 *
 * Tries to delete an item (file or directory) from server
 *
 * Returns -1 in case of error, 1 if DELE worked, 0 if it failed
 */
#[deprecated]
pub unsafe extern "C" fn xmlNanoFTPDele(ctx: *mut c_void, file: *const c_char) -> c_int {
    let ctxt: XmlNanoFtpctxtPtr = ctx as XmlNanoFtpctxtPtr;
    let mut buf: [c_char; 400] = [0; 400];

    let mut res: c_int;

    if ctxt.is_null() || (*ctxt).control_fd == INVALID_SOCKET || file.is_null() {
        return -1;
    }

    /*
     * Expected response code for DELE:
     *
     * DELE
     *       250
     *       450, 550
     *       500, 501, 502, 421, 530
     */

    snprintf(
        buf.as_mut_ptr() as _,
        buf.len(),
        c"DELE %s\r\n".as_ptr() as _,
        file,
    );
    *buf.last_mut().unwrap() = 0;
    let len: c_int = strlen(buf.as_ptr() as _) as _;
    // #ifdef DEBUG_FTP
    //     xmlGenericError(xmlGenericErrorContext, c"%s".as_ptr() as _, buf);
    // #endif
    res = send((*ctxt).control_fd, buf.as_ptr() as _, len as _, 0) as _;
    if res < 0 {
        __xml_ioerr(
            XmlErrorDomain::XmlFromFtp as i32,
            0,
            c"send failed".as_ptr() as _,
        );
        return res;
    }
    res = xmlNanoFTPGetResponse(ctxt as _);
    if res == 4 {
        return -1;
    }
    if res == 2 {
        return 1;
    }
    if res == 5 {
        return 0;
    }
    0
}

/**
 * xmlNanoFTPGetConnection:
 * @ctx:  an FTP context
 *
 * Try to open a data connection to the server. Currently only
 * passive mode is supported.
 *
 * Returns -1 in case of error, 0 otherwise
 */
#[deprecated]
pub unsafe extern "C" fn xmlNanoFTPGetConnection(ctx: *mut c_void) -> Socket {
    let ctxt: XmlNanoFtpctxtPtr = ctx as XmlNanoFtpctxtPtr;
    let mut buf: [c_char; 200] = [0; 200];
    let mut cur: *mut c_char;
    let len: c_int;
    let mut res: c_int;
    let mut ad: [c_uchar; 6] = [0; 6];
    let adp: *mut c_uchar;
    let portp: *mut c_uchar;
    let mut temp: [c_uint; 6] = [0; 6];
    let mut data_addr: sockaddr_storage = unsafe { zeroed() };
    let mut data_addr_len: XmlSocklenT;

    if ctxt.is_null() {
        return INVALID_SOCKET;
    }

    memset(addr_of_mut!(data_addr) as _, 0, size_of_val(&data_addr));
    if ((*ctxt).ftp_addr).ss_family == AF_INET6 as u16 {
        (*ctxt).data_fd = socket(AF_INET6, SOCK_STREAM, IPPROTO_TCP);
        (*(addr_of_mut!(data_addr) as *mut sockaddr_in6)).sin6_family = AF_INET6 as _;
        data_addr_len = size_of::<sockaddr_in6>() as _;
    } else {
        (*ctxt).data_fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
        (*(addr_of_mut!(data_addr) as *mut sockaddr_in)).sin_family = AF_INET as _;
        data_addr_len = size_of::<sockaddr_in>() as _;
    }

    if (*ctxt).data_fd == INVALID_SOCKET {
        __xml_ioerr(
            XmlErrorDomain::XmlFromFtp as i32,
            0,
            c"socket failed".as_ptr() as _,
        );
        return INVALID_SOCKET;
    }

    if (*ctxt).passive != 0 {
        if ((*ctxt).ftp_addr).ss_family == AF_INET6 as u16 {
            snprintf(buf.as_mut_ptr() as _, buf.len(), c"EPSV\r\n".as_ptr() as _);
        } else {
            snprintf(buf.as_mut_ptr() as _, buf.len(), c"PASV\r\n".as_ptr() as _);
        }
        len = strlen(buf.as_ptr() as _) as _;
        // #ifdef DEBUG_FTP
        // 	xmlGenericError(xmlGenericErrorContext, c"%s".as_ptr() as _, buf);
        // #endif
        res = send((*ctxt).control_fd, buf.as_ptr() as _, len as _, 0) as _;
        if res < 0 {
            __xml_ioerr(
                XmlErrorDomain::XmlFromFtp as i32,
                0,
                c"send failed".as_ptr() as _,
            );
            closesocket((*ctxt).data_fd);
            (*ctxt).data_fd = INVALID_SOCKET;
            return INVALID_SOCKET;
        }
        res = xmlNanoFTPReadResponse(ctx);
        if res != 2 {
            if res == 5 {
                closesocket((*ctxt).data_fd);
                (*ctxt).data_fd = INVALID_SOCKET;
                return INVALID_SOCKET;
            } else {
                /*
                 * retry with an active connection
                 */
                closesocket((*ctxt).data_fd);
                (*ctxt).data_fd = INVALID_SOCKET;
                (*ctxt).passive = 0;
            }
        }
        cur = (*ctxt)
            .control_buf
            .as_mut_ptr()
            .add((*ctxt).control_buf_answer as usize);
        while ((*cur < '0' as i8) || (*cur > '9' as i8)) && *cur != b'\0' as i8 {
            cur = cur.add(1);
        }
        if ((*ctxt).ftp_addr).ss_family == AF_INET6 as u16 {
            if sscanf(cur, c"%u".as_ptr() as _, &temp[0]) != 1 {
                __xml_ioerr(
                    XmlErrorDomain::XmlFromFtp as i32,
                    XmlParserErrors::XmlFtpEpsvAnswer as i32,
                    c"Invalid answer to EPSV\n".as_ptr() as _,
                );
                if (*ctxt).data_fd != INVALID_SOCKET {
                    closesocket((*ctxt).data_fd);
                    (*ctxt).data_fd = INVALID_SOCKET;
                }
                return INVALID_SOCKET;
            }
            memcpy(
                addr_of_mut!((*(addr_of_mut!(data_addr) as *mut sockaddr_in6)).sin6_addr) as _,
                addr_of!((*(addr_of_mut!((*ctxt).ftp_addr) as *mut sockaddr_in6)).sin6_addr) as _,
                size_of::<in6_addr>(),
            );
            // (*(addr_of_mut!(dataAddr) as *mut sockaddr_in6)).sin6_port = htons(temp[0]);
            (*(addr_of_mut!(data_addr) as *mut sockaddr_in6)).sin6_port = temp[0].to_be() as u16;
        } else {
            if sscanf(
                cur,
                c"%u,%u,%u,%u,%u,%u".as_ptr() as _,
                addr_of_mut!(temp[0]),
                addr_of_mut!(temp[1]),
                addr_of_mut!(temp[2]),
                addr_of_mut!(temp[3]),
                addr_of_mut!(temp[4]),
                addr_of_mut!(temp[5]),
            ) != 6
            {
                __xml_ioerr(
                    XmlErrorDomain::XmlFromFtp as i32,
                    XmlParserErrors::XmlFtpPasvAnswer as i32,
                    c"Invalid answer to PASV\n".as_ptr() as _,
                );
                if (*ctxt).data_fd != INVALID_SOCKET {
                    closesocket((*ctxt).data_fd);
                    (*ctxt).data_fd = INVALID_SOCKET;
                }
                return INVALID_SOCKET;
            }
            for i in 0..6 {
                ad[i] = (temp[i] & 0xff) as c_uchar;
            }
            memcpy(
                addr_of_mut!((*(addr_of_mut!(data_addr) as *mut sockaddr_in)).sin_addr) as _,
                addr_of!(ad[0]) as _,
                4,
            );
            memcpy(
                addr_of_mut!((*(addr_of_mut!(data_addr) as *mut sockaddr_in)).sin_port) as _,
                addr_of!(ad[4]) as _,
                2,
            );
        }

        if connect((*ctxt).data_fd, addr_of!(data_addr) as _, data_addr_len) < 0 {
            __xml_ioerr(
                XmlErrorDomain::XmlFromFtp as i32,
                0,
                c"Failed to create a data connection".as_ptr() as _,
            );
            closesocket((*ctxt).data_fd);
            (*ctxt).data_fd = INVALID_SOCKET;
            return INVALID_SOCKET;
        }
    } else {
        getsockname(
            (*ctxt).data_fd,
            addr_of!(data_addr) as _,
            addr_of_mut!(data_addr_len) as _,
        );
        if ((*ctxt).ftp_addr).ss_family == AF_INET6 as u16 {
            (*(addr_of_mut!(data_addr) as *mut sockaddr_in6)).sin6_port = 0;
        } else {
            (*(addr_of_mut!(data_addr) as *mut sockaddr_in)).sin_port = 0;
        }

        if bind((*ctxt).data_fd, addr_of_mut!(data_addr) as _, data_addr_len) < 0 {
            __xml_ioerr(
                XmlErrorDomain::XmlFromFtp as i32,
                0,
                c"bind failed".as_ptr() as _,
            );
            closesocket((*ctxt).data_fd);
            (*ctxt).data_fd = INVALID_SOCKET;
            return INVALID_SOCKET;
        }
        getsockname(
            (*ctxt).data_fd,
            addr_of_mut!(data_addr) as _,
            addr_of_mut!(data_addr_len) as _,
        );

        if listen((*ctxt).data_fd, 1) < 0 {
            __xml_ioerr(
                XmlErrorDomain::XmlFromFtp as i32,
                0,
                c"listen failed".as_ptr() as _,
            );
            closesocket((*ctxt).data_fd);
            (*ctxt).data_fd = INVALID_SOCKET;
            return INVALID_SOCKET;
        }

        // reference : http://www5d.biglobe.ne.jp/~stssk/rfc/rfc2553j.html
        const INET6_ADDRSTRLEN: usize = 46;
        // reference : https://manpages.ubuntu.com/manpages/trusty/ja/man3/inet_ntop.3.html
        extern "C" {
            fn inet_ntop(
                af: c_int,
                src: *const c_void,
                dst: *mut c_char,
                size: socklen_t,
            ) -> *const c_char;
        }

        if ((*ctxt).ftp_addr).ss_family == AF_INET6 as u16 {
            let mut buf6: [c_char; INET6_ADDRSTRLEN] = [0; INET6_ADDRSTRLEN];
            inet_ntop(
                AF_INET6,
                addr_of_mut!((*(addr_of_mut!(data_addr) as *mut sockaddr_in6)).sin6_addr) as _,
                buf6.as_mut_ptr() as _,
                INET6_ADDRSTRLEN as _,
            );
            adp = buf6.as_mut_ptr() as _;
            portp = addr_of_mut!((*(addr_of_mut!(data_addr) as *mut sockaddr_in6)).sin6_port) as _;
            snprintf(
                buf.as_mut_ptr() as _,
                buf.len(),
                c"EPRT |2|%s|%s|\r\n".as_ptr() as _,
                adp,
                portp,
            );
        } else {
            adp = addr_of_mut!((*(addr_of_mut!(data_addr) as *mut sockaddr_in)).sin_addr) as _;
            portp = addr_of_mut!((*(addr_of_mut!(data_addr) as *mut sockaddr_in)).sin_port) as _;
            snprintf(
                buf.as_mut_ptr() as _,
                buf.len(),
                c"PORT %d,%d,%d,%d,%d,%d\r\n".as_ptr() as _,
                *adp.add(0) as i32 & 0xff,
                *adp.add(1) as i32 & 0xff,
                *adp.add(2) as i32 & 0xff,
                *adp.add(3) as i32 & 0xff,
                *portp.add(0) as i32 & 0xff,
                *portp.add(1) as i32 & 0xff,
            );
        }

        *buf.last_mut().unwrap() = 0;
        len = strlen(buf.as_ptr() as _) as _;
        // #ifdef DEBUG_FTP
        // 	xmlGenericError(xmlGenericErrorContext, c"%s".as_ptr() as _, buf);
        // #endif

        res = send((*ctxt).control_fd, buf.as_ptr() as _, len as _, 0) as _;
        if res < 0 {
            __xml_ioerr(
                XmlErrorDomain::XmlFromFtp as i32,
                0,
                c"send failed".as_ptr() as _,
            );
            closesocket((*ctxt).data_fd);
            (*ctxt).data_fd = INVALID_SOCKET;
            return INVALID_SOCKET;
        }
        res = xmlNanoFTPGetResponse(ctxt as _);
        if res != 2 {
            closesocket((*ctxt).data_fd);
            (*ctxt).data_fd = INVALID_SOCKET;
            return INVALID_SOCKET;
        }
    }
    (*ctxt).data_fd
}

/**
 * xmlNanoFTPCloseConnection:
 * @ctx:  an FTP context
 *
 * Close the data connection from the server
 *
 * Returns -1 in case of error, 0 otherwise
 */
#[deprecated]
pub unsafe extern "C" fn xmlNanoFTPCloseConnection(ctx: *mut c_void) -> c_int {
    let ctxt: XmlNanoFtpctxtPtr = ctx as XmlNanoFtpctxtPtr;
    let mut res: c_int;
    let mut rfd: fd_set = unsafe { zeroed() };
    let mut efd: fd_set = unsafe { zeroed() };
    let mut tv: timeval = unsafe { zeroed() };

    if ctxt.is_null() || (*ctxt).control_fd == INVALID_SOCKET {
        return -1;
    }

    closesocket((*ctxt).data_fd);
    (*ctxt).data_fd = INVALID_SOCKET;
    tv.tv_sec = 15;
    tv.tv_usec = 0;
    FD_ZERO(addr_of_mut!(rfd));
    FD_SET((*ctxt).control_fd, addr_of_mut!(rfd));
    FD_ZERO(addr_of_mut!(efd));
    FD_SET((*ctxt).control_fd, addr_of_mut!(efd));
    res = select(
        (*ctxt).control_fd + 1,
        addr_of_mut!(rfd),
        null_mut(),
        addr_of_mut!(efd),
        addr_of_mut!(tv),
    );
    if res < 0 {
        // #ifdef DEBUG_FTP
        // 	perror(c"select".as_ptr() as _);
        // #endif
        closesocket((*ctxt).control_fd);
        (*ctxt).control_fd = INVALID_SOCKET;
        return -1;
    }
    if res == 0 {
        // #ifdef DEBUG_FTP
        // 	xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlNanoFTPCloseConnection: timeout\n".as_ptr() as _);
        // #endif
        closesocket((*ctxt).control_fd);
        (*ctxt).control_fd = INVALID_SOCKET;
    } else {
        res = xmlNanoFTPGetResponse(ctxt as _);
        if res != 2 {
            closesocket((*ctxt).control_fd);
            (*ctxt).control_fd = INVALID_SOCKET;
            return -1;
        }
    }
    0
}

/**
 * xmlNanoFTPParseList:
 * @list:  some data listing received from the server
 * @callback:  the user callback
 * @userData:  the user callback data
 *
 * Parse at most one entry from the listing.
 *
 * Returns -1 in case of error, the length of data parsed otherwise
 */
unsafe extern "C" fn xmlNanoFTPParseList(
    list: *const c_char,
    callback: Option<FtpListCallback>,
    user_data: *mut c_void,
) -> c_int {
    let mut cur: *const c_char = list;
    let mut filename: [c_char; 151] = [0; 151];
    let mut attrib: [c_char; 11] = [0; 11];
    let mut owner: [c_char; 11] = [0; 11];
    let mut group: [c_char; 11] = [0; 11];
    let mut month: [c_char; 4] = [0; 4];
    let mut year: c_int = 0;
    let mut minute: c_int = 0;
    let mut hour: c_int = 0;
    let mut day: c_int = 0;
    let mut size: c_ulong = 0;
    let mut links: c_int = 0;
    let mut i: usize;

    if strncmp(cur, c"total".as_ptr() as _, 5) == 0 {
        cur = cur.add(5);
        while *cur == b' ' as i8 {
            cur = cur.add(1);
        }
        while *cur >= b'0' as i8 && *cur <= b'9' as i8 {
            links = (links * 10) + (*cur - b'0' as i8) as i32;
            cur = cur.add(1);
        }
        while *cur == b' ' as i8 || *cur == b'\n' as i8 || *cur == b'\r' as i8 {
            cur = cur.add(1);
        }
        return cur.offset_from(list) as _;
    } else if *list == b'+' as i8 {
        return 0;
    } else {
        while *cur == b' ' as i8 || *cur == b'\n' as i8 || *cur == b'\r' as i8 {
            cur = cur.add(1);
        }
        if *cur == 0 {
            return 0;
        }
        i = 0;
        while *cur != b' ' as i8 {
            if i < 10 {
                attrib[i] = *cur;
                i += 1;
            }
            cur = cur.add(1);
            if *cur == 0 {
                return 0;
            }
        }
        attrib[10] = 0;
        while *cur == b' ' as i8 {
            cur = cur.add(1);
        }
        if *cur == 0 {
            return 0;
        }
        while *cur >= b'0' as i8 && *cur <= b'9' as i8 {
            links = (links * 10) + (*cur - b'0' as i8) as i32;
            cur = cur.add(1);
        }
        while *cur == b' ' as i8 {
            cur = cur.add(1);
        }
        if *cur == 0 {
            return 0;
        }
        i = 0;
        while *cur != b' ' as i8 {
            if i < 10 {
                owner[i] = *cur;
                i += 1;
            }
            cur = cur.add(1);
            if *cur == 0 {
                return 0;
            }
        }
        owner[i] = 0;
        while *cur == b' ' as i8 {
            cur = cur.add(1);
        }
        if *cur == 0 {
            return 0;
        }
        i = 0;
        while *cur != b' ' as i8 {
            if i < 10 {
                group[i] = *cur;
                i += 1;
            }
            cur = cur.add(1);
            if *cur == 0 {
                return 0;
            }
        }
        group[i] = 0;
        while *cur == b' ' as i8 {
            cur = cur.add(1);
        }
        if *cur == 0 {
            return 0;
        }
        while *cur >= b'0' as i8 && *cur <= b'9' as i8 {
            size = (size * 10) + (*cur - b'0' as i8) as u64;
            cur = cur.add(1);
        }
        while *cur == b' ' as i8 {
            cur = cur.add(1);
        }
        if *cur == 0 {
            return 0;
        }
        i = 0;
        while *cur != b' ' as i8 {
            if i < 3 {
                month[i] = *cur;
                i += 1;
            }
            cur = cur.add(1);
            if *cur == 0 {
                return 0;
            }
        }
        month[i] = 0;
        while *cur == b' ' as i8 {
            cur = cur.add(1);
        }
        if *cur == 0 {
            return 0;
        }
        while *cur >= b'0' as i8 && *cur <= b'9' as i8 {
            day = (day * 10) + (*cur - b'0' as i8) as i32;
            cur = cur.add(1);
        }
        while *cur == b' ' as i8 {
            cur = cur.add(1);
        }
        if *cur == 0 {
            return 0;
        }
        if *cur.add(1) == 0 || *cur.add(2) == 0 {
            return 0;
        }
        if *cur.add(1) == b':' as i8 || *cur.add(2) == b':' as i8 {
            while *cur >= b'0' as i8 && *cur <= b'9' as i8 {
                hour = (hour * 10) + (*cur - b'0' as i8) as i32;
                cur = cur.add(1);
            }
            if *cur == b':' as i8 {
                cur = cur.add(1);
            }
            while *cur >= b'0' as i8 && *cur <= b'9' as i8 {
                minute = (minute * 10) + (*cur - b'0' as i8) as i32;
                cur = cur.add(1);
            }
        } else {
            while *cur >= b'0' as i8 && *cur <= b'9' as i8 {
                year = (year * 10) + (*cur - b'0' as i8) as i32;
                cur = cur.add(1);
            }
        }
        while *cur == b' ' as i8 {
            cur = cur.add(1);
        }
        if *cur == 0 {
            return 0;
        }
        i = 0;
        while *cur != b'\n' as i8 && *cur != b'\r' as i8 {
            if i < 150 {
                filename[i] = *cur;
                i += 1;
            }
            cur = cur.add(1);
            if *cur == 0 {
                return 0;
            }
        }
        filename[i] = 0;
        if *cur != b'\n' as i8 && *cur != b'\r' as i8 {
            return 0;
        }
        while *cur == b'\n' as i8 || *cur == b'\r' as i8 {
            cur = cur.add(1);
        }
    }
    if let Some(callback) = callback {
        callback(
            user_data,
            filename.as_ptr() as _,
            attrib.as_ptr() as _,
            owner.as_ptr() as _,
            group.as_ptr() as _,
            size,
            links,
            year,
            month.as_ptr() as _,
            day,
            hour,
            minute,
        );
    }
    cur.offset_from(list) as _
}

/**
 * xmlNanoFTPList:
 * @ctx:  an FTP context
 * @callback:  the user callback
 * @userData:  the user callback data
 * @filename:  optional files to list
 *
 * Do a listing on the server. All files info are passed back
 * in the callbacks.
 *
 * Returns -1 in case of error, 0 otherwise
 */
#[deprecated]
pub unsafe extern "C" fn xmlNanoFTPList(
    ctx: *mut c_void,
    callback: FtpListCallback,
    user_data: *mut c_void,
    filename: *const c_char,
) -> c_int {
    let ctxt: XmlNanoFtpctxtPtr = ctx as XmlNanoFtpctxtPtr;
    let mut buf: [c_char; 4096 + 1] = [0; 4096 + 1];
    let mut len: c_int;
    let mut res: c_int;
    let mut indx: c_int = 0;
    let mut base: c_int;
    let mut rfd: fd_set = unsafe { zeroed() };
    let mut efd: fd_set = unsafe { zeroed() };
    let mut tv: timeval = unsafe { zeroed() };

    if ctxt.is_null() {
        return -1;
    }
    if filename.is_null() {
        if xmlNanoFTPCwd(ctxt as _, (*ctxt).path) < 1 {
            return -1;
        }
        (*ctxt).data_fd = xmlNanoFTPGetConnection(ctxt as _);
        if (*ctxt).data_fd == INVALID_SOCKET {
            return -1;
        }
        snprintf(
            buf.as_mut_ptr() as _,
            buf.len(),
            c"LIST -L\r\n".as_ptr() as _,
        );
    } else {
        if *filename.add(0) != b'/' as i8 && xmlNanoFTPCwd(ctxt as _, (*ctxt).path) < 1 {
            return -1;
        }
        (*ctxt).data_fd = xmlNanoFTPGetConnection(ctxt as _);
        if (*ctxt).data_fd == INVALID_SOCKET {
            return -1;
        }
        snprintf(
            buf.as_mut_ptr() as _,
            buf.len(),
            c"LIST -L %s\r\n".as_ptr() as _,
            filename,
        );
    }
    *buf.last_mut().unwrap() = 0;
    len = strlen(buf.as_ptr() as _) as _;
    // #ifdef DEBUG_FTP
    //     xmlGenericError(xmlGenericErrorContext, c"%s".as_ptr() as _, buf);
    // #endif
    res = send((*ctxt).control_fd, buf.as_ptr() as _, len as _, 0) as _;
    if res < 0 {
        __xml_ioerr(
            XmlErrorDomain::XmlFromFtp as i32,
            0,
            c"send failed".as_ptr() as _,
        );
        closesocket((*ctxt).data_fd);
        (*ctxt).data_fd = INVALID_SOCKET;
        return res;
    }
    res = xmlNanoFTPReadResponse(ctxt as _);
    if res != 1 {
        closesocket((*ctxt).data_fd);
        (*ctxt).data_fd = INVALID_SOCKET;
        return -res;
    }

    loop {
        tv.tv_sec = 1;
        tv.tv_usec = 0;
        FD_ZERO(addr_of_mut!(rfd));
        FD_SET((*ctxt).data_fd, addr_of_mut!(rfd));
        FD_ZERO(addr_of_mut!(efd));
        FD_SET((*ctxt).data_fd, addr_of_mut!(efd));
        res = select(
            (*ctxt).data_fd + 1,
            addr_of_mut!(rfd),
            null_mut(),
            addr_of_mut!(efd),
            addr_of_mut!(tv),
        );
        if res < 0 {
            // #ifdef DEBUG_FTP
            // 	    perror(c"select".as_ptr() as _);
            // #endif
            closesocket((*ctxt).data_fd);
            (*ctxt).data_fd = INVALID_SOCKET;
            return -1;
        }
        if res == 0 {
            res = xmlNanoFTPCheckResponse(ctxt as _);
            if res < 0 {
                closesocket((*ctxt).data_fd);
                (*ctxt).data_fd = INVALID_SOCKET;
                (*ctxt).data_fd = INVALID_SOCKET;
                return -1;
            }
            if res == 2 {
                closesocket((*ctxt).data_fd);
                (*ctxt).data_fd = INVALID_SOCKET;
                return 0;
            }

            if len == 0 {
                break;
            }
            continue;
        }

        let f = {
            len = recv(
                (*ctxt).data_fd,
                buf.as_mut_ptr().add(indx as usize) as _,
                buf.len() - (indx + 1) as usize,
                0,
            ) as _;
            len < 0
        };
        if f {
            __xml_ioerr(XmlErrorDomain::XmlFromFtp as i32, 0, c"recv".as_ptr() as _);
            closesocket((*ctxt).data_fd);
            (*ctxt).data_fd = INVALID_SOCKET;
            (*ctxt).data_fd = INVALID_SOCKET;
            return -1;
        }
        // #ifdef DEBUG_FTP
        //         write(1, &buf[indx], len);
        // #endif
        indx += len;
        buf[indx as usize] = 0;
        base = 0;
        while {
            res = xmlNanoFTPParseList(addr_of_mut!(buf[base as usize]), Some(callback), user_data);
            base += res;
            res > 0
        } {}

        memmove(
            addr_of_mut!(buf[0]) as _,
            addr_of!(buf[base as usize]) as _,
            indx as usize - base as usize,
        );
        indx -= base;

        if len == 0 {
            break;
        }
    }
    xmlNanoFTPCloseConnection(ctxt as _);
    0
}

/**
 * xmlNanoFTPGetSocket:
 * @ctx:  an FTP context
 * @filename:  the file to retrieve (or NULL if path is in context).
 *
 * Initiate fetch of the given file from the server.
 *
 * Returns the socket for the data connection, or <0 in case of error
 */
#[deprecated]
pub unsafe extern "C" fn xmlNanoFTPGetSocket(ctx: *mut c_void, filename: *const c_char) -> Socket {
    let ctxt: XmlNanoFtpctxtPtr = ctx as XmlNanoFtpctxtPtr;
    let mut buf: [c_char; 300] = [0; 300];
    let mut res: c_int;
    let mut len: c_int;
    if ctx.is_null() {
        return INVALID_SOCKET;
    }
    if filename.is_null() && (*ctxt).path.is_null() {
        return INVALID_SOCKET;
    }
    (*ctxt).data_fd = xmlNanoFTPGetConnection(ctxt as _);
    if (*ctxt).data_fd == INVALID_SOCKET {
        return INVALID_SOCKET;
    }

    snprintf(
        buf.as_mut_ptr() as _,
        buf.len(),
        c"TYPE I\r\n".as_ptr() as _,
    );
    len = strlen(buf.as_ptr() as _) as _;
    // #ifdef DEBUG_FTP
    //     xmlGenericError(xmlGenericErrorContext, c"%s".as_ptr() as _, buf);
    // #endif
    res = send((*ctxt).control_fd, buf.as_ptr() as _, len as _, 0) as _;
    if res < 0 {
        __xml_ioerr(
            XmlErrorDomain::XmlFromFtp as i32,
            0,
            c"send failed".as_ptr() as _,
        );
        closesocket((*ctxt).data_fd);
        (*ctxt).data_fd = INVALID_SOCKET;
        return INVALID_SOCKET;
    }
    res = xmlNanoFTPReadResponse(ctxt as _);
    if res != 2 {
        closesocket((*ctxt).data_fd);
        (*ctxt).data_fd = INVALID_SOCKET;
        return INVALID_SOCKET;
    }
    if filename.is_null() {
        snprintf(
            buf.as_mut_ptr() as _,
            buf.len(),
            c"RETR %s\r\n".as_ptr() as _,
            (*ctxt).path,
        );
    } else {
        snprintf(
            buf.as_mut_ptr() as _,
            buf.len(),
            c"RETR %s\r\n".as_ptr() as _,
            filename,
        );
    }
    *buf.last_mut().unwrap() = 0;
    len = strlen(buf.as_ptr() as _) as _;
    // #ifdef DEBUG_FTP
    //     xmlGenericError(xmlGenericErrorContext, c"%s".as_ptr() as _, buf);
    // #endif
    res = send((*ctxt).control_fd, buf.as_ptr() as _, len as _, 0) as _;
    if res < 0 {
        __xml_ioerr(
            XmlErrorDomain::XmlFromFtp as i32,
            0,
            c"send failed".as_ptr() as _,
        );
        closesocket((*ctxt).data_fd);
        (*ctxt).data_fd = INVALID_SOCKET;
        return INVALID_SOCKET;
    }
    res = xmlNanoFTPReadResponse(ctxt as _);
    if res != 1 {
        closesocket((*ctxt).data_fd);
        (*ctxt).data_fd = INVALID_SOCKET;
        return INVALID_SOCKET;
    }
    (*ctxt).data_fd
}

/**
 * xmlNanoFTPGet:
 * @ctx:  an FTP context
 * @callback:  the user callback
 * @userData:  the user callback data
 * @filename:  the file to retrieve
 *
 * Fetch the given file from the server. All data are passed back
 * in the callbacks. The last callback has a size of 0 block.
 *
 * Returns -1 in case of error, 0 otherwise
 */
#[deprecated]
pub unsafe extern "C" fn xmlNanoFTPGet(
    ctx: *mut c_void,
    callback: Option<FtpDataCallback>,
    user_data: *mut c_void,
    filename: *const c_char,
) -> c_int {
    let ctxt: XmlNanoFtpctxtPtr = ctx as XmlNanoFtpctxtPtr;
    let mut buf: [c_char; 4096] = [0; 4096];
    let mut len: c_int = 0;
    let mut res: c_int;
    let mut rfd: fd_set = unsafe { zeroed() };
    let mut tv: timeval = unsafe { zeroed() };

    if ctxt.is_null() {
        return -1;
    }
    if filename.is_null() && (*ctxt).path.is_null() {
        return -1;
    }
    if callback.is_none() {
        return -1;
    }
    let callback = callback.unwrap();
    if xmlNanoFTPGetSocket(ctxt as _, filename) == INVALID_SOCKET {
        return -1;
    }

    loop {
        tv.tv_sec = 1;
        tv.tv_usec = 0;
        FD_ZERO(addr_of_mut!(rfd));
        FD_SET((*ctxt).data_fd, addr_of_mut!(rfd));
        res = select(
            (*ctxt).data_fd + 1,
            addr_of_mut!(rfd),
            null_mut(),
            null_mut(),
            addr_of_mut!(tv),
        );
        if res < 0 {
            // #ifdef DEBUG_FTP
            // 	    perror(c"select".as_ptr() as _);
            // #endif
            closesocket((*ctxt).data_fd);
            (*ctxt).data_fd = INVALID_SOCKET;
            return -1;
        }
        if res == 0 {
            res = xmlNanoFTPCheckResponse(ctxt as _);
            if res < 0 {
                closesocket((*ctxt).data_fd);
                (*ctxt).data_fd = INVALID_SOCKET;
                (*ctxt).data_fd = INVALID_SOCKET;
                return -1;
            }
            if res == 2 {
                closesocket((*ctxt).data_fd);
                (*ctxt).data_fd = INVALID_SOCKET;
                return 0;
            }

            if len == 0 {
                break;
            }
            continue;
        }
        let res = {
            len = recv((*ctxt).data_fd, buf.as_mut_ptr() as _, buf.len(), 0) as _;
            len < 0
        };
        if res {
            __xml_ioerr(
                XmlErrorDomain::XmlFromFtp as i32,
                0,
                c"recv failed".as_ptr() as _,
            );
            callback(user_data, buf.as_ptr() as _, len);
            closesocket((*ctxt).data_fd);
            (*ctxt).data_fd = INVALID_SOCKET;
            return -1;
        }
        callback(user_data, buf.as_ptr() as _, len);

        if len == 0 {
            break;
        }
    }

    xmlNanoFTPCloseConnection(ctxt as _)
}

/**
 * xmlNanoFTPRead:
 * @ctx:  the FTP context
 * @dest:  a buffer
 * @len:  the buffer length
 *
 * This function tries to read @len bytes from the existing FTP connection
 * and saves them in @dest. This is a blocking call.
 *
 * Returns the number of byte read. 0 is an indication of an end of connection.
 *         -1 indicates a parameter error.
 */
#[deprecated]
pub unsafe extern "C" fn xmlNanoFTPRead(
    ctx: *mut c_void,
    dest: *mut c_void,
    mut len: c_int,
) -> c_int {
    let ctxt: XmlNanoFtpctxtPtr = ctx as XmlNanoFtpctxtPtr;

    if ctx.is_null() {
        return -1;
    }
    if (*ctxt).data_fd == INVALID_SOCKET {
        return 0;
    }
    if dest.is_null() {
        return -1;
    }
    if len <= 0 {
        return 0;
    }

    len = recv((*ctxt).data_fd, dest, len as _, 0) as _;
    if len <= 0 {
        if len < 0 {
            __xml_ioerr(
                XmlErrorDomain::XmlFromFtp as i32,
                0,
                c"recv failed".as_ptr() as _,
            );
        }
        xmlNanoFTPCloseConnection(ctxt as _);
    }
    // #ifdef DEBUG_FTP
    //     xmlGenericError(xmlGenericErrorContext, c"Recvd %d bytes\n".as_ptr() as _, len);
    // #endif
    len
}
