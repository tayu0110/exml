/*
 * runtest.c: C program to run libxml2 regression tests without
 *            requiring make or Python, and reducing platform dependencies
 *            to a strict minimum.
 *
 * To compile on Unixes:
 * cc -o runtest `xml2-config --cflags` runtest.c `xml2-config --libs` -lpthread
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 */

use std::{
    env::args,
    ffi::{c_char, c_int, c_uint, c_ulong, CStr, CString},
    fs::{metadata, File},
    io::{BufRead, BufReader, Read, Write},
    mem::zeroed,
    os::{fd::AsRawFd, raw::c_void},
    process::exit,
    ptr::{addr_of_mut, null, null_mut},
    sync::{
        atomic::{AtomicPtr, Ordering},
        Mutex, OnceLock,
    },
};

use const_format::concatcp;
use exml::{
    libxml::{
        encoding::{
            xmlCharEncCloseFunc, xmlGetCharEncodingHandler, XmlCharEncoding,
            XmlCharEncodingHandlerPtr,
        },
        entities::XmlEntityPtr,
        globals::{set_xml_free, set_xml_malloc, set_xml_mem_strdup, set_xml_realloc, xml_free},
        htmlparser::{
            html_ctxt_read_file, html_free_parser_ctxt, html_new_sax_parser_ctxt, html_read_fd,
            html_read_file, HtmlParserCtxtPtr,
        },
        htmltree::htmlDocDumpMemory,
        parser::{
            xml_cleanup_parser, xml_ctxt_use_options, xml_free_parser_ctxt, xml_init_parser,
            xml_parse_document, xml_parse_file, xml_pedantic_parser_default, xml_read_fd,
            xml_read_file, xml_read_memory, xml_set_external_entity_loader, XmlParserCtxtPtr,
            XmlParserInputPtr, XmlParserOption, XmlSAXHandler, XmlSaxlocatorPtr, XML_SAX2_MAGIC,
        },
        parser_internals::xml_create_file_parser_ctxt,
        pattern::{XmlPatternPtr, XmlStreamCtxtPtr},
        relaxng::{xml_relaxng_init_types, XmlRelaxNGPtr},
        tree::{
            xml_doc_dump_memory, xml_free_doc, xml_save_file, XmlDoc, XmlDocPtr,
            XmlElementContentPtr, XmlElementType, XmlEnumerationPtr, XmlNodePtr,
        },
        uri::{
            xml_build_uri, xml_create_uri, xml_free_uri, xml_normalize_uri_path,
            xml_parse_uri_reference, xml_print_uri, XmlURIPtr,
        },
        valid::xml_free_enumeration,
        xinclude::xml_xinclude_process_flags,
        xml_io::{xmlNoNetExternalEntityLoader, xmlPopInputCallbacks, xmlRegisterInputCallbacks},
        xmlerror::{
            xmlResetLastError, xmlSetGenericErrorFunc, xmlSetStructuredErrorFunc, XmlErrorDomain,
            XmlErrorLevel, XmlErrorPtr, XmlGenericErrorFunc, XmlParserErrors,
        },
        xmlmemory::{
            xml_mem_free, xml_mem_malloc, xml_mem_realloc, xml_mem_setup, xml_mem_used,
            xml_memory_dump, xml_memory_strdup,
        },
        xmlreader::XmlTextReaderPtr,
        xmlregexp::XmlRegexpPtr,
        xmlschemas::{XmlSchemaPtr, XmlSchemaValidCtxtPtr},
        xmlschemastypes::xmlSchemaInitTypes,
        xmlstring::{xml_get_utf8_char, xml_strlen, XmlChar},
        xpath::XmlXPathObjectPtr,
    },
    xml_error_with_format, SYSCONFDIR,
};
use libc::{
    close, fdopen, fflush, free, glob, glob_t, globfree, malloc, memcmp, memcpy, open, pthread_t,
    size_t, snprintf, strcmp, strdup, strlen, strncpy, strstr, unlink, FILE, GLOB_DOOFFS, O_RDONLY,
};

/*
 * pseudo flag for the unification of HTML and XML tests
 */
const XML_PARSE_HTML: i32 = 1 << 24;

/*
 * O_BINARY is just for Windows compatibility - if it isn't defined
 * on this system, avoid any compilation error
 */
const RD_FLAGS: i32 = O_RDONLY;
// const WR_FLAGS: i32 = O_WRONLY | O_CREAT | O_TRUNC;

type Functest = unsafe extern "C" fn(
    filename: *const c_char,
    result: *const c_char,
    error: *const c_char,
    options: c_int,
) -> c_int;

struct TestDesc<'a> {
    desc: &'a str,            /* description of the test */
    func: Functest,           /* function implementing the test */
    input: Option<&'a CStr>,  /* glob to path for input files */
    out: Option<&'a CStr>,    /* output directory */
    suffix: Option<&'a CStr>, /* suffix for output files */
    err: Option<&'a CStr>,    /* suffix for error output files */
    options: c_int,           /* parser options for the test */
}

static mut UPDATE_RESULTS: c_int = 0;
static TEMP_DIRECTORY: OnceLock<CString> = OnceLock::new();

/************************************************************************
 *                                    *
 *        Libxml2 specific routines                *
 *                                    *
 ************************************************************************/

static mut NB_TESTS: c_int = 0;
static mut NB_ERRORS: c_int = 0;
static mut NB_LEAKS: c_int = 0;
static mut EXTRA_MEMORY_FROM_RESOLVER: c_int = 0;

unsafe extern "C" fn fatal_error() -> c_int {
    eprintln!("Exitting tests on fatal error");
    exit(1);
}

/*
 * We need to trap calls to the resolver to not account memory for the catalog
 * which is shared to the current running test. We also don't want to have
 * network downloads modifying tests.
 */
unsafe extern "C" fn test_external_entity_loader(
    url: *const c_char,
    id: *const c_char,
    ctxt: XmlParserCtxtPtr,
) -> XmlParserInputPtr {
    let ret: XmlParserInputPtr;

    if check_test_file(url) != 0 {
        ret = xmlNoNetExternalEntityLoader(url, id, ctxt);
    } else {
        let memused: c_int = xml_mem_used();
        ret = xmlNoNetExternalEntityLoader(url, id, ctxt);
        EXTRA_MEMORY_FROM_RESOLVER += xml_mem_used() - memused;
    }

    ret
}

/*
 * Trapping the error messages at the generic level to grab the equivalent of
 * stderr messages on CLI tools.
 */
static mut TEST_ERRORS: [XmlChar; 32769] = [0; 32769];
static mut TEST_ERRORS_SIZE: usize = 0;

unsafe extern "C" fn test_error_handler(_ctx: *mut c_void, msg: *const c_char) {
    if TEST_ERRORS_SIZE >= 32768 {
        return;
    }

    let m = CStr::from_ptr(msg);
    if TEST_ERRORS_SIZE + m.to_bytes().len() >= 32768 {
        TEST_ERRORS[TEST_ERRORS_SIZE..]
            .copy_from_slice(&m.to_bytes()[..TEST_ERRORS.len() - TEST_ERRORS_SIZE]);
        /* buffer is full */
        TEST_ERRORS_SIZE = 32768;
        TEST_ERRORS[TEST_ERRORS_SIZE] = 0;
    } else {
        TEST_ERRORS[TEST_ERRORS_SIZE..TEST_ERRORS_SIZE + m.to_bytes().len()]
            .copy_from_slice(m.to_bytes());
        TEST_ERRORS_SIZE += m.to_bytes().len();
    }
    TEST_ERRORS[TEST_ERRORS_SIZE] = 0;
}

unsafe extern "C" fn channel(_ctx: *mut c_void, msg: *const c_char) {
    if TEST_ERRORS_SIZE >= 32768 {
        return;
    }
    let m = CStr::from_ptr(msg);
    if TEST_ERRORS_SIZE + m.to_bytes().len() >= 32768 {
        TEST_ERRORS[TEST_ERRORS_SIZE..]
            .copy_from_slice(&m.to_bytes()[..TEST_ERRORS.len() - TEST_ERRORS_SIZE]);
        TEST_ERRORS_SIZE = 32768;
    } else {
        TEST_ERRORS[TEST_ERRORS_SIZE..TEST_ERRORS_SIZE + m.to_bytes().len()]
            .copy_from_slice(m.to_bytes());
        TEST_ERRORS_SIZE += m.to_bytes().len();
    }
    TEST_ERRORS[TEST_ERRORS_SIZE] = 0;
}

/**
 * xmlParserPrintFileContextInternal:
 * @input:  an xmlParserInputPtr input
 *
 * Displays current context within the input content for error tracking
 */

unsafe extern "C" fn xml_parser_print_file_context_internal(
    input: XmlParserInputPtr,
    chanl: XmlGenericErrorFunc,
    data: *mut c_void,
) {
    let mut cur: *const XmlChar;
    let mut n: usize;
    /* GCC warns if signed, because compared with sizeof() */
    let mut content: [XmlChar; 81] = [0; 81]; /* space for 80 chars + line terminator */
    let mut ctnt: *mut XmlChar;

    if input.is_null() || (*input).cur.is_null() {
        return;
    }

    cur = (*input).cur;
    let base: *const XmlChar = (*input).base;
    /* skip backwards over any end-of-lines */
    while cur > base && (*cur == b'\n' || *cur == b'\r') {
        cur = cur.sub(1);
    }
    n = 0;
    /* search backwards for beginning-of-line (to max buff size) */
    while {
        n += 1;
        n < content.len()
    } && cur > base
        && *cur != b'\n'
        && *cur != b'\r'
    {
        cur = cur.sub(1);
    }
    if *cur == b'\n' || *cur == b'\r' {
        cur = cur.add(1);
    } else {
        /* skip over continuation bytes */
        while cur < (*input).cur && *cur & 0xC0 == 0x80 {
            cur = cur.add(1);
        }
    }
    /* calculate the error position in terms of the current position */
    let col: c_uint = (*input).cur.offset_from(cur) as _;
    /* search forward for end-of-line (to max buff size) */
    n = 0;
    let start: *const XmlChar = cur;
    /* copy selected text to our buffer */
    while *cur != 0 && *cur != b'\n' && *cur != b'\r' {
        let mut len = (*input).end.offset_from(cur) as usize;
        let c: c_int = xml_get_utf8_char(cur, addr_of_mut!(len) as _);

        if c < 0 || n + len > content.len() - 1 {
            break;
        }
        cur = cur.add(len as _);
        n += len;
    }
    memcpy(content.as_mut_ptr() as _, start as _, n);
    content[n] = 0;
    /* print out the selected text */
    xml_error_with_format!(chanl, data, c"%s\n".as_ptr(), content.as_ptr());
    /* create blank line with problem pointer */
    n = 0;
    ctnt = content.as_mut_ptr();
    /* (leave buffer space for pointer + line terminator) */
    while n < col as usize
        && {
            n += 1;
            n < content.len() - 1
        }
        && *ctnt != 0
    {
        if *ctnt != b'\t' {
            *ctnt = b' ';
        }
        ctnt = ctnt.add(1);
    }
    *ctnt = b'^';
    ctnt = ctnt.add(1);
    *ctnt = 0;
    xml_error_with_format!(chanl, data, c"%s\n".as_ptr(), content.as_ptr());
}

unsafe extern "C" fn test_structured_error_handler(_ctx: *mut c_void, err: XmlErrorPtr) {
    let data: *mut c_void = null_mut();
    let mut name: *const XmlChar = null_mut();
    let mut input: XmlParserInputPtr = null_mut();
    let mut cur: XmlParserInputPtr = null_mut();
    let mut ctxt: XmlParserCtxtPtr = null_mut();

    if err.is_null() {
        return;
    }

    let file: *mut c_char = (*err).file;
    let line: c_int = (*err).line;
    let code: c_int = (*err).code;
    let domain: c_int = (*err).domain;
    let level: XmlErrorLevel = (*err).level;
    let node: XmlNodePtr = (*err).node as _;
    if domain == XmlErrorDomain::XmlFromParser as i32
        || domain == XmlErrorDomain::XmlFromHtml as i32
        || domain == XmlErrorDomain::XmlFromDtd as i32
        || domain == XmlErrorDomain::XmlFromNamespace as i32
        || domain == XmlErrorDomain::XmlFromIO as i32
        || domain == XmlErrorDomain::XmlFromValid as i32
    {
        ctxt = (*err).ctxt as _;
    }
    let str: *const c_char = (*err).message;

    if code == XmlParserErrors::XmlErrOK as i32 {
        return;
    }

    if !node.is_null() && (*node).typ == XmlElementType::XmlElementNode {
        name = (*node).name;
    }

    /*
     * Maintain the compatibility with the legacy error handling
     */
    if !ctxt.is_null() {
        input = (*ctxt).input;
        if !input.is_null() && (*input).filename.is_null() && (*ctxt).input_nr > 1 {
            cur = input;
            input = *(*ctxt).input_tab.add((*ctxt).input_nr as usize - 2);
        }
        if !input.is_null() {
            if !(*input).filename.is_null() {
                xml_error_with_format!(
                    channel,
                    data,
                    c"%s:%d: ".as_ptr(),
                    (*input).filename,
                    (*input).line
                );
            } else if line != 0 && domain == XmlErrorDomain::XmlFromParser as i32 {
                xml_error_with_format!(channel, data, c"Entity: line %d: ".as_ptr(), (*input).line);
            }
        }
    } else if !file.is_null() {
        xml_error_with_format!(channel, data, c"%s:%d: ".as_ptr(), file, line);
    } else if line != 0 && domain == XmlErrorDomain::XmlFromParser as i32 {
        xml_error_with_format!(channel, data, c"Entity: line %d: ".as_ptr(), line);
    }
    if !name.is_null() {
        xml_error_with_format!(channel, data, c"element %s: ".as_ptr(), name);
    }
    if code == XmlParserErrors::XmlErrOK as i32 {
        return;
    }
    match XmlErrorDomain::try_from(domain) {
        Ok(XmlErrorDomain::XmlFromParser) => {
            channel(data, c"parser ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromNamespace) => {
            channel(data, c"namespace ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromDtd) | Ok(XmlErrorDomain::XmlFromValid) => {
            channel(data, c"validity ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromHtml) => {
            channel(data, c"HTML parser ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromMemory) => {
            channel(data, c"memory ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromOutput) => {
            channel(data, c"output ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromIO) => {
            channel(data, c"I/O ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromXinclude) => {
            channel(data, c"XInclude ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromXpath) => {
            channel(data, c"XPath ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromXpointer) => {
            channel(data, c"parser ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromRegexp) => {
            channel(data, c"regexp ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromModule) => {
            channel(data, c"module ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromSchemasv) => {
            channel(data, c"Schemas validity ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromSchemasp) => {
            channel(data, c"Schemas parser ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromRelaxngp) => {
            channel(data, c"Relax-NG parser ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromRelaxngv) => {
            channel(data, c"Relax-NG validity ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromCatalog) => {
            channel(data, c"Catalog ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromC14N) => {
            channel(data, c"C14N ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromXSLT) => {
            channel(data, c"XSLT ".as_ptr());
        }
        _ => {}
    }
    if code == XmlParserErrors::XmlErrOK as i32 {
        return;
    }
    match level {
        XmlErrorLevel::XmlErrNone => {
            channel(data, c": ".as_ptr());
        }
        XmlErrorLevel::XmlErrWarning => {
            channel(data, c"warning : ".as_ptr());
        }
        XmlErrorLevel::XmlErrError => {
            channel(data, c"error : ".as_ptr());
        }
        XmlErrorLevel::XmlErrFatal => {
            channel(data, c"error : ".as_ptr());
        }
    }
    if code == XmlParserErrors::XmlErrOK as i32 {
        return;
    }
    if !str.is_null() {
        let len: c_int = xml_strlen(str as *const XmlChar);
        if len > 0 && *str.add(len as usize - 1) != b'\n' as _ {
            xml_error_with_format!(channel, data, c"%s\n".as_ptr(), str);
        } else {
            xml_error_with_format!(channel, data, c"%s".as_ptr(), str);
        }
    } else {
        xml_error_with_format!(
            channel,
            data,
            c"%s\n".as_ptr(),
            c"out of memory error".as_ptr()
        );
    }
    if code == XmlParserErrors::XmlErrOK as i32 {
        return;
    }

    if !ctxt.is_null() {
        xml_parser_print_file_context_internal(input, channel, data);
        if !cur.is_null() {
            if !(*cur).filename.is_null() {
                xml_error_with_format!(
                    channel,
                    data,
                    c"%s:%d: \n".as_ptr(),
                    (*cur).filename,
                    (*cur).line
                );
            } else if line != 0 && domain == XmlErrorDomain::XmlFromParser as i32 {
                xml_error_with_format!(channel, data, c"Entity: line %d: \n".as_ptr(), (*cur).line);
            }
            xml_parser_print_file_context_internal(cur, channel, data);
        }
    }
    if domain == XmlErrorDomain::XmlFromXpath as i32
        && !(*err).str1.is_null()
        && (*err).int1 < 100
        && (*err).int1 < xml_strlen((*err).str1 as *const XmlChar)
    {
        let mut buf: [XmlChar; 150] = [0; 150];

        xml_error_with_format!(channel, data, c"%s\n".as_ptr(), (*err).str1);
        for i in 0..(*err).int1 {
            buf[i as usize] = b' ';
        }
        buf[(*err).int1 as usize] = b'^';
        buf[(*err).int1 as usize + 1] = 0;
        xml_error_with_format!(channel, data, c"%s\n".as_ptr(), buf.as_ptr());
    }
}

unsafe extern "C" fn initialize_libxml2() {
    /*
     * This verifies that xmlInitParser doesn't allocate memory with
     * xmlMalloc
     */
    set_xml_free(None);
    set_xml_malloc(None);
    set_xml_realloc(None);
    set_xml_mem_strdup(None);
    xml_init_parser();
    xml_mem_setup(
        Some(xml_mem_free),
        Some(xml_mem_malloc),
        Some(xml_mem_realloc),
        Some(xml_memory_strdup),
    );
    xml_pedantic_parser_default(0);
    xml_set_external_entity_loader(test_external_entity_loader);
    xmlSetStructuredErrorFunc(null_mut(), Some(test_structured_error_handler));
    #[cfg(feature = "schema")]
    {
        xmlSchemaInitTypes();
        xml_relaxng_init_types();
    }
}

/************************************************************************
 *                                    *
 *        File name and path utilities                *
 *                                    *
 ************************************************************************/

unsafe extern "C" fn base_filename(filename: *const c_char) -> *const c_char {
    let mut cur: *const c_char;
    if filename.is_null() {
        return null_mut();
    }
    cur = filename.add(strlen(filename) as usize);
    while cur > filename && *cur != b'/' as _ {
        cur = cur.sub(1);
    }
    if *cur == b'/' as _ {
        return cur.add(1);
    }
    cur
}

unsafe extern "C" fn result_filename(
    filename: *const c_char,
    mut out: *const c_char,
    mut suffix: *const c_char,
) -> *mut c_char {
    let mut res: [c_char; 500] = [0; 500];
    let mut suffixbuff: [c_char; 500] = [0; 500];

    /*************
       if ((filename[0] == 't') && (filename[1] == 'e') &&
           (filename[2] == 's') && (filename[3] == 't') &&
       (filename[4] == '/'))
       filename = &filename[5];
    *************/

    let base: *const c_char = base_filename(filename);
    if suffix.is_null() {
        suffix = c".tmp".as_ptr();
    }
    if out.is_null() {
        out = c"".as_ptr();
    }

    strncpy(suffixbuff.as_mut_ptr(), suffix, 499);
    // #ifdef VMS
    //     if (strstr(base,".") && suffixbuff[0]=='.')
    //       suffixbuff[0]='_';
    // #endif

    if snprintf(
        res.as_mut_ptr() as _,
        499,
        c"%s%s%s".as_ptr(),
        out,
        base,
        suffixbuff.as_ptr(),
    ) >= 499
    {
        res[499] = 0;
    }
    strdup(res.as_ptr())
}

unsafe extern "C" fn check_test_file(filename: *const c_char) -> c_int {
    match metadata(CStr::from_ptr(filename).to_string_lossy().as_ref()) {
        Ok(meta) => meta.is_file() as i32,
        _ => 0,
    }
}

unsafe extern "C" fn compare_files(
    r1: *const c_char, /* temp */
    r2: *const c_char, /* result */
) -> c_int {
    let mut bytes1: [u8; 4096] = [0; 4096];
    let mut bytes2: [u8; 4096] = [0; 4096];

    if UPDATE_RESULTS != 0 {
        let Ok(mut fd1) = File::open(CStr::from_ptr(r1).to_string_lossy().as_ref()) else {
            return -1;
        };
        let Ok(mut fd2) = File::options()
            .write(true)
            .truncate(true)
            .create(true)
            .open(CStr::from_ptr(r2).to_string_lossy().as_ref())
        else {
            return -1;
        };
        let mut total = 0;
        while let Ok(res1) = fd1.read(&mut bytes1) {
            if res1 == 0 {
                if total == 0 {
                    unlink(r2);
                }
                return 1;
            }
            total += res1;
            if fd2
                .write(&bytes1)
                .ok()
                .filter(|&size| size == res1)
                .is_none()
            {
                if total == 0 {
                    unlink(r2);
                }
                return (res1 != 0) as i32;
            }
        }
        return 0;
    }

    let Ok(mut fd1) = File::open(CStr::from_ptr(r1).to_string_lossy().as_ref()) else {
        return -1;
    };
    let mut fd2 = File::open(CStr::from_ptr(r2).to_string_lossy().as_ref());
    while let Ok(res1) = fd1.read(&mut bytes1) {
        if fd2.as_mut().map_or(0, |f| f.read(&mut bytes2).unwrap_or(0)) != res1 {
            return 1;
        }
        if res1 == 0 {
            break;
        }
        if memcmp(bytes1.as_ptr() as _, bytes2.as_ptr() as _, res1) != 0 {
            return 1;
        }
    }
    0
}

unsafe extern "C" fn compare_file_mem(
    filename: *const c_char,
    mem: *const c_char,
    size: c_int,
) -> c_int {
    let mut bytes: [u8; 4096] = [0; 4096];

    if UPDATE_RESULTS != 0 {
        if size == 0 {
            unlink(filename);
            return 0;
        }
        let mut file = match File::options()
            .write(true)
            .truncate(true)
            .create(true)
            .open(CStr::from_ptr(filename).to_string_lossy().as_ref())
        {
            Ok(file) => file,
            _ => {
                eprint!(
                    "failed to open {} for writing",
                    CStr::from_ptr(filename).to_string_lossy()
                );
                return -1;
            }
        };
        let res = file
            .write(std::slice::from_raw_parts(mem as *const u8, size as usize))
            .unwrap();
        return (res != size as usize) as i32;
    }

    match metadata(CStr::from_ptr(filename).to_string_lossy().as_ref()) {
        Ok(meta) => {
            if meta.len() != size as u64 {
                eprintln!(
                    "file {} is {} bytes, result is {} bytes",
                    CStr::from_ptr(filename).to_string_lossy(),
                    meta.len(),
                    size,
                );
                return -1;
            }
        }
        Err(_) => {
            if size == 0 {
                return 0;
            }
            eprintln!(
                "failed to stat {}",
                CStr::from_ptr(filename).to_string_lossy()
            );
            return -1;
        }
    }
    let Ok(mut file) = File::open(CStr::from_ptr(filename).to_string_lossy().as_ref()) else {
        eprint!(
            "failed to open {} for reading",
            CStr::from_ptr(filename).to_string_lossy()
        );
        return -1;
    };
    let mut idx = 0;
    while idx < size as usize {
        let res = match file.read(&mut bytes) {
            Ok(size) => size,
            _ => break,
        };
        if res + idx > size as usize {
            break;
        }
        if memcmp(bytes.as_mut_ptr() as _, mem.add(idx) as _, res) != 0 {
            for (ix, &byte) in bytes.iter().enumerate().take(res + 1) {
                if idx == res || byte != *mem.add(idx + ix) as u8 {
                    eprintln!("Compare error at position {}", idx + ix);
                    return 1;
                }
            }
        }
        idx += res;
    }
    if idx != size as usize {
        eprintln!("Compare error index {}, size {}", idx, size);
    }
    (idx != size as usize) as i32
}

unsafe extern "C" fn load_mem(
    filename: *const c_char,
    mem: *mut *const c_char,
    size: *mut c_int,
) -> c_int {
    let filename = CStr::from_ptr(filename).to_string_lossy();
    match metadata(filename.as_ref()) {
        Ok(meta) => {
            let base: *mut c_char = malloc(meta.len() as usize + 1) as _;
            if base.is_null() {
                return -1;
            }
            match File::open(filename.as_ref()) {
                Ok(mut file) => {
                    let mut siz = 0;
                    while let Some(res) = file
                        .read(std::slice::from_raw_parts_mut(
                            base.add(siz) as _,
                            meta.len() as usize - siz,
                        ))
                        .ok()
                        .filter(|&res| res > 0)
                    {
                        siz += res;
                    }
                    if siz != meta.len() as usize {
                        free(base as _);
                        return -1;
                    }
                    *base.add(siz) = 0;
                    *mem = base;
                    *size = siz as _;
                    0
                }
                _ => {
                    free(base as _);
                    -1
                }
            }
        }
        _ => -1,
    }
}

unsafe extern "C" fn unload_mem(mem: *const c_char) -> c_int {
    free(mem as _);
    0
}

/************************************************************************
 *                                    *
 *        Tests implementations                    *
 *                                    *
 ************************************************************************/

/************************************************************************
 *                                    *
 *        Parse to SAX based tests                *
 *                                    *
 ************************************************************************/

static SAX_DEBUG: Mutex<Option<File>> = Mutex::new(None);

macro_rules! sax_debug {
    ( $fmt:literal, $( $args:expr ),* ) => {
        write!(SAX_DEBUG.lock().unwrap().as_mut().unwrap(), $fmt, $( $args ),*).ok();
    };
    ( $fmt:literal, $( $args:expr ),* ,) => {
        sax_debug!($fmt, $( $args ),*);
    };
    ( $fmt:literal ) => {
        sax_debug!($fmt, );
    }
}

macro_rules! sax_debugln {
    ( $fmt:literal, $( $args:expr ),* ) => {
        sax_debug!($fmt, $( $args ),*);
        sax_debug!("\n", );
    };
    ( $fmt:literal, $( $args:expr ),* ,) => {
        sax_debugln!($fmt, $( $args ),*);
    };
    ( $fmt:literal ) => {
        sax_debugln!($fmt, );
    }
}

/*
 * empty SAX block
 */
static mut EMPTY_SAXHANDLER_STRUCT: XmlSAXHandler = XmlSAXHandler {
    internal_subset: None,
    is_standalone: None,
    has_internal_subset: None,
    has_external_subset: None,
    resolve_entity: None,
    get_entity: None,
    entity_decl: None,
    notation_decl: None,
    attribute_decl: None,
    element_decl: None,
    unparsed_entity_decl: None,
    set_document_locator: None,
    start_document: None,
    end_document: None,
    start_element: None,
    end_element: None,
    reference: None,
    characters: None,
    ignorable_whitespace: None,
    processing_instruction: None,
    comment: None,
    warning: None,
    error: None,
    fatal_error: None,
    get_parameter_entity: None,
    cdata_block: None,
    external_subset: None,
    initialized: 1,
    _private: AtomicPtr::new(null_mut()),
    start_element_ns: None,
    end_element_ns: None,
    serror: None,
};

// static xmlSAXHandlerPtr emptySAXHandler = &emptySAXHandlerStruct;
static mut CALLBACKS: c_int = 0;
static mut QUIET: c_int = 0;

/**
 * isStandaloneDebug:
 * @ctxt:  An XML parser context
 *
 * Is this document tagged standalone ?
 *
 * Returns 1 if true
 */
unsafe extern "C" fn is_standalone_debug(_ctx: *mut c_void) -> c_int {
    CALLBACKS += 1;
    if QUIET != 0 {
        return 0;
    }
    sax_debugln!("SAX.isStandalone()");
    0
}

/**
 * hasInternalSubsetDebug:
 * @ctxt:  An XML parser context
 *
 * Does this document has an internal subset
 *
 * Returns 1 if true
 */
unsafe extern "C" fn has_internal_subset_debug(_ctx: *mut c_void) -> c_int {
    CALLBACKS += 1;
    if QUIET != 0 {
        return 0;
    }
    sax_debugln!("SAX.hasInternalSubset()");
    0
}

/**
 * hasExternalSubsetDebug:
 * @ctxt:  An XML parser context
 *
 * Does this document has an external subset
 *
 * Returns 1 if true
 */
unsafe extern "C" fn has_external_subset_debug(_ctx: *mut c_void) -> c_int {
    CALLBACKS += 1;
    if QUIET != 0 {
        return 0;
    }
    sax_debugln!("SAX.hasExternalSubset()");
    0
}

/**
 * internalSubsetDebug:
 * @ctxt:  An XML parser context
 *
 * Does this document has an internal subset
 */
unsafe extern "C" fn internal_subset_debug(
    _ctx: *mut c_void,
    mut name: *const XmlChar,
    external_id: *const XmlChar,
    system_id: *const XmlChar,
) {
    CALLBACKS += 1;
    if QUIET != 0 {
        return;
    }
    if name.is_null() {
        name = c"(null)".as_ptr() as _;
    }
    sax_debug!(
        "SAX.internalSubset({},",
        CStr::from_ptr(name as _).to_string_lossy()
    );
    if external_id.is_null() {
        sax_debug!(" ,");
    } else {
        sax_debug!(" {},", CStr::from_ptr(external_id as _).to_string_lossy());
    }
    if system_id.is_null() {
        sax_debugln!(" )");
    } else {
        sax_debugln!(" {})", CStr::from_ptr(system_id as _).to_string_lossy());
    }
}

/**
 * externalSubsetDebug:
 * @ctxt:  An XML parser context
 *
 * Does this document has an external subset
 */
unsafe extern "C" fn external_subset_debug(
    _ctx: *mut c_void,
    name: *const XmlChar,
    external_id: *const XmlChar,
    system_id: *const XmlChar,
) {
    CALLBACKS += 1;
    if QUIET != 0 {
        return;
    }
    sax_debug!(
        "SAX.externalSubset({},",
        CStr::from_ptr(name as _).to_string_lossy()
    );
    if external_id.is_null() {
        sax_debug!(" ,");
    } else {
        sax_debug!(" {},", CStr::from_ptr(external_id as _).to_string_lossy());
    }
    if system_id.is_null() {
        sax_debugln!(" )");
    } else {
        sax_debugln!(" {})", CStr::from_ptr(system_id as _).to_string_lossy());
    }
}

/**
 * resolveEntityDebug:
 * @ctxt:  An XML parser context
 * @publicId: The public ID of the entity
 * @systemId: The system ID of the entity
 *
 * Special entity resolver, better left to the parser, it has
 * more context than the application layer.
 * The default behaviour is to NOT resolve the entities, in that case
 * the ENTITY_REF nodes are built in the structure (and the parameter
 * values).
 *
 * Returns the xmlParserInputPtr if inlined or NULL for DOM behaviour.
 */
unsafe extern "C" fn resolve_entity_debug(
    _ctx: *mut c_void,
    public_id: *const XmlChar,
    system_id: *const XmlChar,
) -> XmlParserInputPtr {
    CALLBACKS += 1;
    if QUIET != 0 {
        return null_mut();
    }
    /* xmlParserCtxtPtr ctxt = (xmlParserCtxtPtr) ctx; */

    sax_debug!("SAX.resolveEntity(");
    if !public_id.is_null() {
        sax_debug!(
            "{}",
            CStr::from_ptr(public_id as *mut c_char).to_string_lossy()
        );
    } else {
        sax_debug!(" ");
    }
    if !system_id.is_null() {
        sax_debugln!(
            ", {})",
            CStr::from_ptr(system_id as *mut c_char).to_string_lossy()
        );
    } else {
        sax_debugln!(", )");
    }
    /*********
       if !systemId.is_null() {
           return(xmlNewInputFromFile(ctxt, systemId as *mut c_char));
       }
    *********/
    null_mut()
}

/**
 * getEntityDebug:
 * @ctxt:  An XML parser context
 * @name: The entity name
 *
 * Get an entity by name
 *
 * Returns the xmlParserInputPtr if inlined or NULL for DOM behaviour.
 */
unsafe extern "C" fn get_entity_debug(_ctx: *mut c_void, name: *const XmlChar) -> XmlEntityPtr {
    CALLBACKS += 1;
    if QUIET != 0 {
        return null_mut();
    }
    sax_debugln!(
        "SAX.getEntity({})",
        CStr::from_ptr(name as _).to_string_lossy()
    );
    null_mut()
}

/**
 * getParameterEntityDebug:
 * @ctxt:  An XML parser context
 * @name: The entity name
 *
 * Get a parameter entity by name
 *
 * Returns the xmlParserInputPtr
 */
unsafe extern "C" fn get_parameter_entity_debug(
    _ctx: *mut c_void,
    name: *const XmlChar,
) -> XmlEntityPtr {
    CALLBACKS += 1;
    if QUIET != 0 {
        return null_mut();
    }
    sax_debugln!(
        "SAX.getParameterEntity({})",
        CStr::from_ptr(name as _).to_string_lossy()
    );
    null_mut()
}

/**
 * entityDeclDebug:
 * @ctxt:  An XML parser context
 * @name:  the entity name
 * @type:  the entity type
 * @publicId: The public ID of the entity
 * @systemId: The system ID of the entity
 * @content: the entity value (without processing).
 *
 * An entity definition has been parsed
 */
unsafe extern "C" fn entity_decl_debug(
    _ctx: *mut c_void,
    name: *const XmlChar,
    typ: c_int,
    mut public_id: *const XmlChar,
    mut system_id: *const XmlChar,
    mut content: *mut XmlChar,
) {
    let nullstr: *const XmlChar = c"(null)".as_ptr() as _;
    /* not all libraries handle printing null pointers nicely */
    if public_id.is_null() {
        public_id = nullstr;
    }
    if system_id.is_null() {
        system_id = nullstr;
    }
    if content.is_null() {
        content = nullstr as *mut XmlChar;
    }
    CALLBACKS += 1;
    if QUIET != 0 {
        return;
    }
    sax_debugln!(
        "SAX.entityDecl({}, {}, {}, {}, {})",
        CStr::from_ptr(name as _).to_string_lossy(),
        typ,
        CStr::from_ptr(public_id as _).to_string_lossy(),
        CStr::from_ptr(system_id as _).to_string_lossy(),
        CStr::from_ptr(content as _).to_string_lossy(),
    );
}

/**
 * attributeDeclDebug:
 * @ctxt:  An XML parser context
 * @name:  the attribute name
 * @type:  the attribute type
 *
 * An attribute definition has been parsed
 */
unsafe extern "C" fn attribute_decl_debug(
    _ctx: *mut c_void,
    elem: *const XmlChar,
    name: *const XmlChar,
    typ: c_int,
    def: c_int,
    default_value: *const XmlChar,
    tree: XmlEnumerationPtr,
) {
    CALLBACKS += 1;
    if QUIET != 0 {
        return;
    }
    if default_value.is_null() {
        sax_debugln!(
            "SAX.attributeDecl({}, {}, {}, {}, NULL, ...)",
            CStr::from_ptr(elem as _).to_string_lossy(),
            CStr::from_ptr(name as _).to_string_lossy(),
            typ,
            def,
        );
    } else {
        sax_debugln!(
            "SAX.attributeDecl({}, {}, {}, {}, {}, ...)",
            CStr::from_ptr(elem as _).to_string_lossy(),
            CStr::from_ptr(name as _).to_string_lossy(),
            typ,
            def,
            CStr::from_ptr(default_value as _).to_string_lossy(),
        );
    }
    xml_free_enumeration(tree);
}

/**
 * elementDeclDebug:
 * @ctxt:  An XML parser context
 * @name:  the element name
 * @type:  the element type
 * @content: the element value (without processing).
 *
 * An element definition has been parsed
 */
unsafe extern "C" fn element_decl_debug(
    _ctx: *mut c_void,
    name: *const XmlChar,
    typ: c_int,
    _content: XmlElementContentPtr,
) {
    CALLBACKS += 1;
    if QUIET != 0 {
        return;
    }
    sax_debugln!(
        "SAX.elementDecl({}, {}, ...)",
        CStr::from_ptr(name as _).to_string_lossy(),
        typ
    );
}

/**
 * notationDeclDebug:
 * @ctxt:  An XML parser context
 * @name: The name of the notation
 * @publicId: The public ID of the entity
 * @systemId: The system ID of the entity
 *
 * What to do when a notation declaration has been parsed.
 */
unsafe extern "C" fn notation_decl_debug(
    _ctx: *mut c_void,
    name: *const XmlChar,
    public_id: *const XmlChar,
    system_id: *const XmlChar,
) {
    CALLBACKS += 1;
    if QUIET != 0 {
        return;
    }
    sax_debugln!(
        "SAX.notationDecl({}, {}, {})",
        CStr::from_ptr(name as *mut c_char).to_string_lossy(),
        CStr::from_ptr(public_id as *mut c_char).to_string_lossy(),
        CStr::from_ptr(system_id as *mut c_char).to_string_lossy()
    );
}

/**
 * unparsedEntityDeclDebug:
 * @ctxt:  An XML parser context
 * @name: The name of the entity
 * @publicId: The public ID of the entity
 * @systemId: The system ID of the entity
 * @notationName: the name of the notation
 *
 * What to do when an unparsed entity declaration is parsed
 */
unsafe extern "C" fn unparsed_entity_decl_debug(
    _ctx: *mut c_void,
    name: *const XmlChar,
    mut public_id: *const XmlChar,
    mut system_id: *const XmlChar,
    mut notation_name: *const XmlChar,
) {
    let nullstr: *const XmlChar = c"(null)".as_ptr() as _;

    if public_id.is_null() {
        public_id = nullstr;
    }
    if system_id.is_null() {
        system_id = nullstr;
    }
    if notation_name.is_null() {
        notation_name = nullstr;
    }
    CALLBACKS += 1;
    if QUIET != 0 {
        return;
    }
    sax_debugln!(
        "SAX.unparsedEntityDecl({}, {}, {}, {})",
        CStr::from_ptr(name as *mut c_char).to_string_lossy(),
        CStr::from_ptr(public_id as *mut c_char).to_string_lossy(),
        CStr::from_ptr(system_id as *mut c_char).to_string_lossy(),
        CStr::from_ptr(notation_name as *mut c_char).to_string_lossy()
    );
}

/**
 * setDocumentLocatorDebug:
 * @ctxt:  An XML parser context
 * @loc: A SAX Locator
 *
 * Receive the document locator at startup, actually xmlDefaultSAXLocator
 * Everything is available on the context, so this is useless in our case.
 */
unsafe extern "C" fn set_document_locator_debug(_ctx: *mut c_void, _loc: XmlSaxlocatorPtr) {
    CALLBACKS += 1;
    if QUIET != 0 {
        return;
    }
    sax_debugln!("SAX.setDocumentLocator()");
}

/**
 * startDocumentDebug:
 * @ctxt:  An XML parser context
 *
 * called when the document start being processed.
 */
unsafe extern "C" fn start_document_debug(_ctx: *mut c_void) {
    CALLBACKS += 1;
    if QUIET != 0 {
        return;
    }
    sax_debugln!("SAX.startDocument()");
}

/**
 * endDocumentDebug:
 * @ctxt:  An XML parser context
 *
 * called when the document end has been detected.
 */
unsafe extern "C" fn end_document_debug(_ctx: *mut c_void) {
    CALLBACKS += 1;
    if QUIET != 0 {
        return;
    }
    sax_debugln!("SAX.endDocument()");
}

/**
 * startElementDebug:
 * @ctxt:  An XML parser context
 * @name:  The element name
 *
 * called when an opening tag has been processed.
 */
unsafe extern "C" fn start_element_debug(
    _ctx: *mut c_void,
    name: *const XmlChar,
    atts: *mut *const XmlChar,
) {
    CALLBACKS += 1;
    if QUIET != 0 {
        return;
    }
    sax_debug!(
        "SAX.startElement({}",
        CStr::from_ptr(name as *mut c_char).to_string_lossy()
    );
    if !atts.is_null() {
        let mut i = 0;
        while !(*atts.add(i)).is_null() {
            sax_debug!(
                ", {}='",
                CStr::from_ptr(*atts.add(i) as _).to_string_lossy()
            );
            i += 1;
            if !(*atts.add(i)).is_null() {
                sax_debug!("{}'", CStr::from_ptr(*atts.add(i) as _).to_string_lossy());
            }
            i += 1;
        }
    }
    sax_debugln!(")");
}

/**
 * endElementDebug:
 * @ctxt:  An XML parser context
 * @name:  The element name
 *
 * called when the end of an element has been detected.
 */
unsafe extern "C" fn end_element_debug(_ctx: *mut c_void, name: *const XmlChar) {
    CALLBACKS += 1;
    if QUIET != 0 {
        return;
    }
    sax_debugln!(
        "SAX.endElement({})",
        CStr::from_ptr(name as *mut c_char).to_string_lossy()
    );
}

/**
 * charactersDebug:
 * @ctxt:  An XML parser context
 * @ch:  a XmlChar string
 * @len: the number of XmlChar
 *
 * receiving some chars from the parser.
 * Question: how much at a time ???
 */
unsafe extern "C" fn characters_debug(_ctx: *mut c_void, ch: *const XmlChar, len: c_int) {
    let mut output: [u8; 40] = [0; 40];

    CALLBACKS += 1;
    if QUIET != 0 {
        return;
    }
    for (i, o) in output.iter_mut().take(len.min(30) as usize).enumerate() {
        *o = *ch.add(i);
    }
    output[len.clamp(0, 30) as usize] = 0;

    sax_debug!("SAX.characters(");
    SAX_DEBUG
        .lock()
        .unwrap()
        .as_mut()
        .unwrap()
        .write_all(&output[..len.clamp(0, 30) as usize])
        .ok();
    sax_debugln!(", {len})");
}

/**
 * referenceDebug:
 * @ctxt:  An XML parser context
 * @name:  The entity name
 *
 * called when an entity reference is detected.
 */
unsafe extern "C" fn reference_debug(_ctx: *mut c_void, name: *const XmlChar) {
    CALLBACKS += 1;
    if QUIET != 0 {
        return;
    }
    sax_debugln!(
        "SAX.reference({})",
        CStr::from_ptr(name as _).to_string_lossy()
    );
}

/**
 * ignorableWhitespaceDebug:
 * @ctxt:  An XML parser context
 * @ch:  a XmlChar string
 * @start: the first c_char in the string
 * @len: the number of XmlChar
 *
 * receiving some ignorable whitespaces from the parser.
 * Question: how much at a time ???
 */
unsafe extern "C" fn ignorable_whitespace_debug(_ctx: *mut c_void, ch: *const XmlChar, len: c_int) {
    let mut output: [u8; 40] = [0; 40];

    CALLBACKS += 1;
    if QUIET != 0 {
        return;
    }
    for i in 0..len.min(30) {
        output[i as usize] = *ch.add(i as usize);
    }
    output[len.clamp(0, 30) as usize] = 0;
    sax_debug!("SAX.ignorableWhitespace(");
    SAX_DEBUG
        .lock()
        .unwrap()
        .as_mut()
        .unwrap()
        .write_all(&output[..len.clamp(0, 30) as usize])
        .ok();
    sax_debugln!(", {len})");
}

/**
 * processingInstructionDebug:
 * @ctxt:  An XML parser context
 * @target:  the target name
 * @data: the PI data's
 * @len: the number of XmlChar
 *
 * A processing instruction has been parsed.
 */
unsafe extern "C" fn processing_instruction_debug(
    _ctx: *mut c_void,
    target: *const XmlChar,
    data: *const XmlChar,
) {
    CALLBACKS += 1;
    if QUIET != 0 {
        return;
    }
    if !data.is_null() {
        sax_debugln!(
            "SAX.processingInstruction({}, {})",
            CStr::from_ptr(target as *mut c_char).to_string_lossy(),
            CStr::from_ptr(data as *mut c_char).to_string_lossy(),
        );
    } else {
        sax_debugln!(
            "SAX.processingInstruction({}, NULL)",
            CStr::from_ptr(target as *mut c_char).to_string_lossy(),
        );
    }
}

/**
 * cdataBlockDebug:
 * @ctx: the user data (XML parser context)
 * @value:  The pcdata content
 * @len:  the block length
 *
 * called when a pcdata block has been parsed
 */
unsafe extern "C" fn cdata_block_debug(_ctx: *mut c_void, value: *const XmlChar, len: c_int) {
    CALLBACKS += 1;
    if QUIET != 0 {
        return;
    }
    let value = CStr::from_ptr(value as *mut c_char).to_bytes();
    let l = value.len().min(20);
    sax_debug!("SAX.pcdata(");
    SAX_DEBUG
        .lock()
        .unwrap()
        .as_mut()
        .unwrap()
        .write_all(&value[..l])
        .ok();
    sax_debugln!(", {len})");
}

/**
 * commentDebug:
 * @ctxt:  An XML parser context
 * @value:  the comment content
 *
 * A comment has been parsed.
 */
unsafe extern "C" fn comment_debug(_ctx: *mut c_void, value: *const XmlChar) {
    CALLBACKS += 1;
    if QUIET != 0 {
        return;
    }
    sax_debugln!(
        "SAX.comment({})",
        CStr::from_ptr(value as _).to_string_lossy()
    );
}

/**
 * warningDebug:
 * @ctxt:  An XML parser context
 * @msg:  the message to display/transmit
 * @...:  extra parameters for the message display
 *
 * Display and format a warning messages, gives file, line, position and
 * extra parameters.
 */
unsafe extern "C" fn warning_debug(_ctx: *mut c_void, msg: *const c_char) {
    CALLBACKS += 1;
    if QUIET != 0 {
        return;
    }
    write!(
        SAX_DEBUG.lock().unwrap().as_mut().unwrap(),
        "SAX.warning: {}",
        CStr::from_ptr(msg).to_string_lossy()
    )
    .ok();
}

/**
 * errorDebug:
 * @ctxt:  An XML parser context
 * @msg:  the message to display/transmit
 * @...:  extra parameters for the message display
 *
 * Display and format a error messages, gives file, line, position and
 * extra parameters.
 */
unsafe extern "C" fn error_debug(_ctx: *mut c_void, msg: *const c_char) {
    CALLBACKS += 1;
    if QUIET != 0 {
        return;
    }
    sax_debug!("SAX.error: {}", CStr::from_ptr(msg).to_string_lossy());
}

/**
 * fatalErrorDebug:
 * @ctxt:  An XML parser context
 * @msg:  the message to display/transmit
 * @...:  extra parameters for the message display
 *
 * Display and format a fatalError messages, gives file, line, position and
 * extra parameters.
 */
unsafe extern "C" fn fatal_error_debug(_ctx: *mut c_void, msg: *const c_char) {
    CALLBACKS += 1;
    if QUIET != 0 {
        return;
    }
    sax_debug!("SAX.fatalError: {}", CStr::from_ptr(msg).to_string_lossy());
}

static mut DEBUG_SAXHANDLER_STRUCT: XmlSAXHandler = XmlSAXHandler {
    internal_subset: Some(internal_subset_debug),
    is_standalone: Some(is_standalone_debug),
    has_internal_subset: Some(has_internal_subset_debug),
    has_external_subset: Some(has_external_subset_debug),
    resolve_entity: Some(resolve_entity_debug),
    get_entity: Some(get_entity_debug),
    entity_decl: Some(entity_decl_debug),
    notation_decl: Some(notation_decl_debug),
    attribute_decl: Some(attribute_decl_debug),
    element_decl: Some(element_decl_debug),
    unparsed_entity_decl: Some(unparsed_entity_decl_debug),
    set_document_locator: Some(set_document_locator_debug),
    start_document: Some(start_document_debug),
    end_document: Some(end_document_debug),
    start_element: Some(start_element_debug),
    end_element: Some(end_element_debug),
    reference: Some(reference_debug),
    characters: Some(characters_debug),
    ignorable_whitespace: Some(ignorable_whitespace_debug),
    processing_instruction: Some(processing_instruction_debug),
    comment: Some(comment_debug),
    warning: Some(warning_debug),
    error: Some(error_debug),
    fatal_error: Some(fatal_error_debug),
    get_parameter_entity: Some(get_parameter_entity_debug),
    cdata_block: Some(cdata_block_debug),
    external_subset: Some(external_subset_debug),
    initialized: 1,
    _private: AtomicPtr::new(null_mut()),
    start_element_ns: None,
    end_element_ns: None,
    serror: None,
};

// static xmlSAXHandlerPtr debugSAXHandler = &debugSAXHandlerStruct;

/*
 * SAX2 specific callbacks
 */
/**
 * startElementNsDebug:
 * @ctxt:  An XML parser context
 * @name:  The element name
 *
 * called when an opening tag has been processed.
 */
unsafe extern "C" fn start_element_ns_debug(
    _ctx: *mut c_void,
    localname: *const XmlChar,
    prefix: *const XmlChar,
    uri: *const XmlChar,
    nb_namespaces: c_int,
    namespaces: *mut *const XmlChar,
    nb_attributes: c_int,
    nb_defaulted: c_int,
    attributes: *mut *const XmlChar,
) {
    CALLBACKS += 1;
    if QUIET != 0 {
        return;
    }
    sax_debug!(
        "SAX.startElementNs({}",
        CStr::from_ptr(localname as *mut c_char).to_string_lossy()
    );
    if prefix.is_null() {
        sax_debug!(", NULL");
    } else {
        sax_debug!(
            ", {}",
            CStr::from_ptr(prefix as *mut c_char).to_string_lossy()
        );
    }
    if uri.is_null() {
        sax_debug!(", NULL");
    } else {
        sax_debug!(
            ", '{}'",
            CStr::from_ptr(uri as *mut c_char).to_string_lossy()
        );
    }
    sax_debug!(", {}", nb_namespaces);

    if !namespaces.is_null() {
        let mut i = 0;
        while i < nb_namespaces as usize * 2 {
            sax_debug!(", xmlns");
            if !(*namespaces.add(i)).is_null() {
                sax_debug!(
                    ":{}",
                    CStr::from_ptr(*namespaces.add(i) as _).to_string_lossy()
                );
            }
            i += 1;
            sax_debug!(
                "='{}'",
                CStr::from_ptr(*namespaces.add(i) as _).to_string_lossy()
            );
            i += 1;
        }
    }
    sax_debug!(", {}, {}", nb_attributes, nb_defaulted);
    if !attributes.is_null() {
        for i in (0..nb_attributes as usize * 5).step_by(5) {
            if !(*attributes.add(i + 1)).is_null() {
                sax_debug!(
                    ", {}:{}='",
                    CStr::from_ptr(*attributes.add(i + 1) as _).to_string_lossy(),
                    CStr::from_ptr(*attributes.add(i) as _).to_string_lossy()
                );
            } else {
                sax_debug!(
                    ", {}='",
                    CStr::from_ptr(*attributes.add(i) as _).to_string_lossy()
                );
            }
            let value = CStr::from_ptr(*attributes.add(i + 3) as _).to_bytes();
            let l = value.len().min(4);
            SAX_DEBUG
                .lock()
                .unwrap()
                .as_mut()
                .unwrap()
                .write_all(&value[..l])
                .ok();
            sax_debug!(
                "...', {}",
                (*attributes.add(i + 4)).offset_from(*attributes.add(i + 3))
            );
        }
    }
    sax_debugln!(")");
}

/**
 * endElementDebug:
 * @ctxt:  An XML parser context
 * @name:  The element name
 *
 * called when the end of an element has been detected.
 */
unsafe extern "C" fn end_element_ns_debug(
    _ctx: *mut c_void,
    localname: *const XmlChar,
    prefix: *const XmlChar,
    uri: *const XmlChar,
) {
    CALLBACKS += 1;
    if QUIET != 0 {
        return;
    }
    sax_debug!(
        "SAX.endElementNs({}",
        CStr::from_ptr(localname as *mut c_char).to_string_lossy()
    );
    if prefix.is_null() {
        sax_debug!(", NULL");
    } else {
        sax_debug!(
            ", {}",
            CStr::from_ptr(prefix as *mut c_char).to_string_lossy()
        );
    }
    if uri.is_null() {
        sax_debugln!(", NULL)");
    } else {
        sax_debugln!(
            ", '{}')",
            CStr::from_ptr(uri as *mut c_char).to_string_lossy()
        );
    }
}

static mut DEBUG_SAX2_HANDLER_STRUCT: XmlSAXHandler = XmlSAXHandler {
    internal_subset: Some(internal_subset_debug),
    is_standalone: Some(is_standalone_debug),
    has_internal_subset: Some(has_internal_subset_debug),
    has_external_subset: Some(has_external_subset_debug),
    resolve_entity: Some(resolve_entity_debug),
    get_entity: Some(get_entity_debug),
    entity_decl: Some(entity_decl_debug),
    notation_decl: Some(notation_decl_debug),
    attribute_decl: Some(attribute_decl_debug),
    element_decl: Some(element_decl_debug),
    unparsed_entity_decl: Some(unparsed_entity_decl_debug),
    set_document_locator: Some(set_document_locator_debug),
    start_document: Some(start_document_debug),
    end_document: Some(end_document_debug),
    start_element: None,
    end_element: None,
    reference: Some(reference_debug),
    characters: Some(characters_debug),
    ignorable_whitespace: Some(ignorable_whitespace_debug),
    processing_instruction: Some(processing_instruction_debug),
    comment: Some(comment_debug),
    warning: Some(warning_debug),
    error: Some(error_debug),
    fatal_error: Some(fatal_error_debug),
    get_parameter_entity: Some(get_parameter_entity_debug),
    cdata_block: Some(cdata_block_debug),
    external_subset: Some(external_subset_debug),
    initialized: XML_SAX2_MAGIC as _,
    _private: AtomicPtr::new(null_mut()),
    start_element_ns: Some(start_element_ns_debug),
    end_element_ns: Some(end_element_ns_debug),
    serror: None,
};

// static xmlSAXHandlerPtr debugSAX2Handler = &debugSAX2HandlerStruct;

/**
 * htmlstartElementDebug:
 * @ctxt:  An XML parser context
 * @name:  The element name
 *
 * called when an opening tag has been processed.
 */
#[cfg(feature = "html")]
unsafe extern "C" fn htmlstart_element_debug(
    _ctx: *mut c_void,
    name: *const XmlChar,
    atts: *mut *const XmlChar,
) {
    use std::ffi::c_uchar;

    use exml::libxml::htmlparser::html_encode_entities;

    sax_debug!(
        "SAX.startElement({}",
        CStr::from_ptr(name as *mut c_char).to_string_lossy()
    );
    if !atts.is_null() {
        let mut i = 0;
        while !(*atts.add(i)).is_null() {
            sax_debug!(", {}", CStr::from_ptr(*atts.add(i) as _).to_string_lossy());
            i += 1;
            if !(*atts.add(i)).is_null() {
                let mut output: [c_uchar; 40] = [0; 40];
                let mut att: *const c_uchar = *atts.add(i);
                let mut outlen: usize;
                let mut attlen: usize;
                sax_debug!("='");
                while {
                    attlen = strlen(att as *mut c_char);
                    attlen > 0
                } {
                    outlen = output.len() - 1;
                    html_encode_entities(
                        output.as_mut_ptr(),
                        addr_of_mut!(outlen) as _,
                        att,
                        addr_of_mut!(attlen) as _,
                        b'\'' as _,
                    );
                    output[outlen] = 0;
                    sax_debug!("{}", CStr::from_ptr(output.as_ptr() as _).to_string_lossy());
                    att = att.add(attlen);
                }
                sax_debug!("'");
            }
            i += 1;
        }
    }
    sax_debugln!(")");
}

/**
 * htmlcharactersDebug:
 * @ctxt:  An XML parser context
 * @ch:  a XmlChar string
 * @len: the number of XmlChar
 *
 * receiving some chars from the parser.
 * Question: how much at a time ???
 */
#[cfg(feature = "html")]
unsafe extern "C" fn htmlcharacters_debug(_ctx: *mut c_void, ch: *const XmlChar, len: c_int) {
    use std::ffi::c_uchar;

    use exml::libxml::htmlparser::html_encode_entities;

    let mut output: [c_uchar; 40] = [0; 40];
    let mut inlen: c_int = len;
    let mut outlen: usize = 30;

    html_encode_entities(
        output.as_mut_ptr(),
        addr_of_mut!(outlen) as _,
        ch,
        addr_of_mut!(inlen),
        0,
    );
    output[outlen] = 0;

    sax_debugln!(
        "SAX.characters({}, {})",
        CStr::from_ptr(output.as_ptr() as _).to_string_lossy(),
        len
    );
}

/**
 * htmlcdataDebug:
 * @ctxt:  An XML parser context
 * @ch:  a XmlChar string
 * @len: the number of XmlChar
 *
 * receiving some cdata chars from the parser.
 * Question: how much at a time ???
 */
#[cfg(feature = "html")]
unsafe extern "C" fn htmlcdata_debug(_ctx: *mut c_void, ch: *const XmlChar, len: c_int) {
    use std::ffi::c_uchar;

    use exml::libxml::htmlparser::html_encode_entities;

    let mut output: [c_uchar; 40] = [0; 40];
    let mut inlen: c_int = len;
    let mut outlen: usize = 30;

    html_encode_entities(
        output.as_mut_ptr(),
        addr_of_mut!(outlen) as _,
        ch,
        addr_of_mut!(inlen),
        0,
    );
    output[outlen] = 0;

    sax_debugln!(
        "SAX.cdata({}, {})",
        CStr::from_ptr(output.as_ptr() as _).to_string_lossy(),
        len
    );
}

#[cfg(feature = "html")]
static mut DEBUG_HTMLSAXHANDLER_STRUCT: XmlSAXHandler = XmlSAXHandler {
    internal_subset: Some(internal_subset_debug),
    is_standalone: Some(is_standalone_debug),
    has_internal_subset: Some(has_internal_subset_debug),
    has_external_subset: Some(has_external_subset_debug),
    resolve_entity: Some(resolve_entity_debug),
    get_entity: Some(get_entity_debug),
    entity_decl: Some(entity_decl_debug),
    notation_decl: Some(notation_decl_debug),
    attribute_decl: Some(attribute_decl_debug),
    element_decl: Some(element_decl_debug),
    unparsed_entity_decl: Some(unparsed_entity_decl_debug),
    set_document_locator: Some(set_document_locator_debug),
    start_document: Some(start_document_debug),
    end_document: Some(end_document_debug),
    start_element: Some(htmlstart_element_debug),
    end_element: Some(end_element_debug),
    reference: Some(reference_debug),
    characters: Some(htmlcharacters_debug),
    ignorable_whitespace: Some(ignorable_whitespace_debug),
    processing_instruction: Some(processing_instruction_debug),
    comment: Some(comment_debug),
    warning: Some(warning_debug),
    error: Some(error_debug),
    fatal_error: Some(fatal_error_debug),
    get_parameter_entity: Some(get_parameter_entity_debug),
    cdata_block: Some(htmlcdata_debug),
    external_subset: Some(external_subset_debug),
    initialized: 1,
    _private: AtomicPtr::new(null_mut()),
    start_element_ns: None,
    end_element_ns: None,
    serror: None,
};

// #[cfg(feature = "html")]
// static xmlSAXHandlerPtr debugHTMLSAXHandler = &debugHTMLSAXHandlerStruct;

/**
 * saxParseTest:
 * @filename: the file to parse
 * @result: the file with expected result
 * @err: the file with error messages
 *
 * Parse a file using the SAX API and check for errors.
 *
 * Returns 0 in case of success, an error code otherwise
 */
unsafe extern "C" fn sax_parse_test(
    filename: *const c_char,
    result: *const c_char,
    _err: *const c_char,
    mut options: c_int,
) -> c_int {
    let mut ret: c_int;

    NB_TESTS += 1;
    let temp: *mut c_char = result_filename(
        filename,
        TEMP_DIRECTORY
            .get()
            .map(|t| t.as_ptr())
            .unwrap_or(null_mut()),
        c".res".as_ptr(),
    );
    if temp.is_null() {
        eprintln!("out of memory");
        fatal_error();
    }

    let Ok(out) = File::options()
        .write(true)
        .truncate(true)
        .create(true)
        .open(CStr::from_ptr(temp).to_string_lossy().as_ref())
    else {
        eprintln!(
            "Failed to write to {}",
            CStr::from_ptr(temp).to_string_lossy()
        );
        return -1;
    };
    *SAX_DEBUG.lock().unwrap() = Some(out);

    /* for SAX we really want the callbacks though the context handlers */
    xmlSetStructuredErrorFunc(null_mut(), None);
    xmlSetGenericErrorFunc(null_mut(), Some(test_error_handler));

    #[cfg(feature = "html")]
    if options & XML_PARSE_HTML != 0 {
        let ctxt: HtmlParserCtxtPtr =
            html_new_sax_parser_ctxt(addr_of_mut!(EMPTY_SAXHANDLER_STRUCT), null_mut());
        html_ctxt_read_file(ctxt, filename, null_mut(), options);
        html_free_parser_ctxt(ctxt);
        ret = 0;
    } else {
        let ctxt: XmlParserCtxtPtr = xml_create_file_parser_ctxt(filename);
        memcpy(
            (*ctxt).sax as _,
            addr_of_mut!(EMPTY_SAXHANDLER_STRUCT) as _,
            size_of::<XmlSAXHandler>(),
        );
        xml_ctxt_use_options(ctxt, options);
        xml_parse_document(ctxt);
        ret = if (*ctxt).well_formed != 0 {
            0
        } else {
            (*ctxt).err_no
        };
        xml_free_doc((*ctxt).my_doc);
        xml_free_parser_ctxt(ctxt);
    }

    #[cfg(not(feature = "html"))]
    {
        let ctxt: XmlParserCtxtPtr = xml_create_file_parser_ctxt(filename);
        memcpy(
            (*ctxt).sax,
            addr_of_mut!(EMPTY_SAXHANDLER_STRUCT),
            sizeof(XmlSAXHandler),
        );
        xml_ctxt_use_options(ctxt, options);
        xml_parse_document(ctxt);
        ret = if (*ctxt).wellFormed { 0 } else { (*ctxt).errNo };
        xml_free_doc((*ctxt).myDoc);
        xml_free_parser_ctxt(ctxt);
    }

    if ret == XmlParserErrors::XmlWarUndeclaredEntity as i32 {
        sax_debugln!("xmlSAXUserParseFile returned error {}", ret);
        ret = 0;
    }
    'done: {
        if ret != 0 {
            eprintln!(
                "Failed to parse {}",
                CStr::from_ptr(filename).to_string_lossy()
            );
            ret = 1;
            break 'done;
        }
        #[cfg(feature = "html")]
        if options & XML_PARSE_HTML != 0 {
            let ctxt: HtmlParserCtxtPtr =
                html_new_sax_parser_ctxt(addr_of_mut!(DEBUG_HTMLSAXHANDLER_STRUCT), null_mut());
            html_ctxt_read_file(ctxt, filename, null_mut(), options);
            html_free_parser_ctxt(ctxt);
            ret = 0;
        } else {
            let ctxt: XmlParserCtxtPtr = xml_create_file_parser_ctxt(filename);
            if options & XmlParserOption::XmlParseSax1 as i32 != 0 {
                memcpy(
                    (*ctxt).sax as _,
                    addr_of_mut!(DEBUG_SAXHANDLER_STRUCT) as _,
                    size_of::<XmlSAXHandler>(),
                );
                options -= XmlParserOption::XmlParseSax1 as i32;
            } else {
                memcpy(
                    (*ctxt).sax as _,
                    addr_of_mut!(DEBUG_SAX2_HANDLER_STRUCT) as _,
                    size_of::<XmlSAXHandler>(),
                );
            }
            xml_ctxt_use_options(ctxt, options);
            xml_parse_document(ctxt);
            ret = if (*ctxt).well_formed != 0 {
                0
            } else {
                (*ctxt).err_no
            };
            xml_free_doc((*ctxt).my_doc);
            xml_free_parser_ctxt(ctxt);
        }
        #[cfg(not(feature = "html"))]
        {
            let ctxt: XmlParserCtxtPtr = xml_create_file_parser_ctxt(filename);
            if options & XmlParserOption::XmlParseSax1 as i32 != 0 {
                memcpy(
                    (*ctxt).sax as _,
                    addr_of_mut!(DEBUG_SAXHANDLER_STRUCT) as _,
                    size_of::<XmlSAXHandler>(),
                );
                options -= XmlParserOption::XmlParseSax1 as i32;
            } else {
                memcpy(
                    (*ctxt).sax as _,
                    addr_of_mut!(DEBUG_SAX2_HANDLER_STRUCT) as _,
                    size_of::<XmlSAXHandler>(),
                );
            }
            xml_ctxt_use_options(ctxt, options);
            xml_parse_document(ctxt);
            ret = if (*ctxt).well_formed != 0 {
                0
            } else {
                (*ctxt).err_no
            };
            xml_free_doc((*ctxt).my_doc);
            xml_free_parser_ctxt(ctxt);
        }
        if ret == XmlParserErrors::XmlWarUndeclaredEntity as i32 {
            sax_debugln!("xmlSAXUserParseFile returned error {}", ret);
            ret = 0;
        }

        if compare_files(temp, result) != 0 {
            eprintln!(
                "Got a difference for {}",
                CStr::from_ptr(filename).to_string_lossy()
            );
            ret = 1;
        }
    }

    if !temp.is_null() {
        unlink(temp);
        free(temp as _);
    }

    /* switch back to structured error handling */
    xmlSetGenericErrorFunc(null_mut(), None);
    xmlSetStructuredErrorFunc(null_mut(), Some(test_structured_error_handler));

    ret
}

/************************************************************************
 *                                    *
 *        Parse to tree based tests                *
 *                                    *
 ************************************************************************/
/**
 * oldParseTest:
 * @filename: the file to parse
 * @result: the file with expected result
 * @err: the file with error messages: unused
 *
 * Parse a file using the old xmlParseFile API, then serialize back
 * reparse the result and serialize again, then check for deviation
 * in serialization.
 *
 * Returns 0 in case of success, an error code otherwise
 */
unsafe extern "C" fn old_parse_test(
    filename: *const c_char,
    result: *const c_char,
    _err: *const c_char,
    _options: c_int,
) -> c_int {
    let mut res: c_int = 0;

    NB_TESTS += 1;
    /*
     * base of the test, parse with the old API
     */
    #[cfg(feature = "sax1")]
    let mut doc = xml_parse_file(filename);
    #[cfg(not(feature = "sax1"))]
    let doc = xml_read_file(filename, NULL, 0);
    if doc.is_null() {
        return 1;
    }
    let temp: *mut c_char = result_filename(
        filename,
        TEMP_DIRECTORY
            .get()
            .map(|t| t.as_ptr())
            .unwrap_or(null_mut()),
        c".res".as_ptr(),
    );
    if temp.is_null() {
        eprintln!("out of memory");
        fatal_error();
    }
    xml_save_file(temp, doc);
    if compare_files(temp, result) != 0 {
        res = 1;
    }
    xml_free_doc(doc);

    /*
     * Parse the saved result to make sure the round trip is okay
     */
    #[cfg(feature = "sax1")]
    {
        doc = xml_parse_file(temp);
    }
    #[cfg(not(feature = "sax1"))]
    {
        doc = xml_read_file(temp, null_mut(), 0);
    }
    if doc.is_null() {
        return 1;
    }
    xml_save_file(temp, doc);
    if compare_files(temp, result) != 0 {
        res = 1;
    }
    xml_free_doc(doc);

    if !temp.is_null() {
        unlink(temp);
        free(temp as _);
    }
    res
}

/**
 * pushParseTest:
 * @filename: the file to parse
 * @result: the file with expected result
 * @err: the file with error messages: unused
 *
 * Parse a file using the Push API, then serialize back
 * to check for content.
 *
 * Returns 0 in case of success, an error code otherwise
 */
#[cfg(feature = "push")]
unsafe extern "C" fn push_parse_test(
    filename: *const c_char,
    result: *const c_char,
    err: *const c_char,
    options: c_int,
) -> c_int {
    use exml::libxml::{
        encoding::XmlCharEncoding,
        htmlparser::{html_create_push_parser_ctxt, html_parse_chunk},
        parser::{xml_create_push_parser_ctxt, xml_parse_chunk},
    };

    let mut base: *const c_char = null();
    let mut size: c_int = 0;
    let mut res: c_int;
    let mut cur: c_int = 0;
    let mut chunk_size: c_int = 4;

    NB_TESTS += 1;
    /*
     * load the document in memory and work from there.
     */
    if load_mem(filename, addr_of_mut!(base), addr_of_mut!(size)) != 0 {
        eprintln!(
            "Failed to load {}",
            CStr::from_ptr(filename).to_string_lossy()
        );
        return -1;
    }

    if chunk_size > size {
        chunk_size = size;
    }

    #[cfg(feature = "html")]
    let ctxt = if options & XML_PARSE_HTML != 0 {
        html_create_push_parser_ctxt(
            null_mut(),
            null_mut(),
            base.add(cur as _),
            chunk_size,
            filename,
            XmlCharEncoding::XmlCharEncodingNone,
        )
    } else {
        xml_create_push_parser_ctxt(
            null_mut(),
            null_mut(),
            base.add(cur as _),
            chunk_size,
            filename,
        )
    };
    #[cfg(not(feature = "html"))]
    let ctxt = xml_create_push_parser_ctxt(
        null_mut(),
        null_mut(),
        base.add(cur as _),
        chunk_size,
        filename,
    );
    xml_ctxt_use_options(ctxt, options);
    cur += chunk_size;
    chunk_size = 1024;
    'b: while {
        if cur + chunk_size >= size {
            #[cfg(feature = "html")]
            if options & XML_PARSE_HTML != 0 {
                html_parse_chunk(ctxt, base.add(cur as _), size - cur, 1);
            } else {
                xml_parse_chunk(ctxt, base.add(cur as _), size - cur, 1);
            }
            #[cfg(not(feature = "html"))]
            {
                xml_parse_chunk(ctxt, base.add(cur as _), size - cur, 1);
            }
            break 'b;
        } else {
            #[cfg(feature = "html")]
            if options & XML_PARSE_HTML != 0 {
                html_parse_chunk(ctxt, base.add(cur as _), chunk_size, 0);
            } else {
                xml_parse_chunk(ctxt, base.add(cur as _), chunk_size, 0);
            }
            #[cfg(not(feature = "html"))]
            {
                xml_parse_chunk(ctxt, base.add(cur as _), chunk_size, 0);
            }
            cur += chunk_size;
        }
        cur < size
    } {}
    let doc: XmlDocPtr = (*ctxt).my_doc;
    #[cfg(feature = "html")]
    if options & XML_PARSE_HTML != 0 {
        res = 1;
    } else {
        res = (*ctxt).well_formed;
    }
    #[cfg(not(feature = "html"))]
    {
        res = (*ctxt).wellFormed;
    }
    xml_free_parser_ctxt(ctxt);
    free(base as _);
    if res == 0 {
        xml_free_doc(doc);
        eprintln!(
            "Failed to parse {}",
            CStr::from_ptr(filename).to_string_lossy()
        );
        return -1;
    }
    #[cfg(feature = "html")]
    if options & XML_PARSE_HTML != 0 {
        htmlDocDumpMemory(
            doc,
            addr_of_mut!(base) as *mut *mut XmlChar,
            addr_of_mut!(size),
        );
    } else {
        xml_doc_dump_memory(
            doc,
            addr_of_mut!(base) as *mut *mut XmlChar,
            addr_of_mut!(size),
        );
    }
    #[cfg(not(feature = "html"))]
    {
        xml_doc_dump_memory(
            doc,
            addr_of_mut!(base) as *mut *mut XmlChar,
            addr_of_mut!(size),
        );
    }
    xml_free_doc(doc);
    res = compare_file_mem(result, base, size);
    if base.is_null() || res != 0 {
        if !base.is_null() {
            xml_free(base as _);
        }
        eprintln!(
            "Result for {} failed in {}",
            CStr::from_ptr(filename).to_string_lossy(),
            CStr::from_ptr(result).to_string_lossy()
        );
        return -1;
    }
    xml_free(base as _);
    if !err.is_null() {
        res = compare_file_mem(err, TEST_ERRORS.as_ptr() as _, TEST_ERRORS_SIZE as _);
        if res != 0 {
            eprintln!(
                "Error for {} failed",
                CStr::from_ptr(filename).to_string_lossy()
            );
            return -1;
        }
    }
    0
}

#[cfg(feature = "push")]
static mut PUSH_BOUNDARY_COUNT: c_int = 0;
#[cfg(feature = "push")]
static mut PUSH_BOUNDARY_REF_COUNT: c_int = 0;
#[cfg(feature = "push")]
static mut PUSH_BOUNDARY_CHARS_COUNT: c_int = 0;
#[cfg(feature = "push")]
static mut PUSH_BOUNDARY_CDATA_COUNT: c_int = 0;

#[cfg(feature = "push")]
unsafe extern "C" fn internal_subset_bnd(
    ctx: *mut c_void,
    name: *const XmlChar,
    external_id: *const XmlChar,
    system_id: *const XmlChar,
) {
    use exml::libxml::sax2::xmlSAX2InternalSubset;

    PUSH_BOUNDARY_COUNT += 1;
    xmlSAX2InternalSubset(ctx, name, external_id, system_id);
}

#[cfg(feature = "push")]
unsafe extern "C" fn reference_bnd(ctx: *mut c_void, name: *const XmlChar) {
    use exml::libxml::sax2::xmlSAX2Reference;

    PUSH_BOUNDARY_REF_COUNT += 1;
    xmlSAX2Reference(ctx, name);
}

#[cfg(feature = "push")]
unsafe extern "C" fn characters_bnd(ctx: *mut c_void, ch: *const XmlChar, len: c_int) {
    use exml::libxml::sax2::xmlSAX2Characters;

    PUSH_BOUNDARY_COUNT += 1;
    PUSH_BOUNDARY_CHARS_COUNT += 1;
    xmlSAX2Characters(ctx, ch, len);
}

#[cfg(feature = "push")]
unsafe extern "C" fn cdata_block_bnd(ctx: *mut c_void, ch: *const XmlChar, len: c_int) {
    use exml::libxml::sax2::xmlSAX2CDataBlock;

    PUSH_BOUNDARY_COUNT += 1;
    PUSH_BOUNDARY_CDATA_COUNT += 1;
    xmlSAX2CDataBlock(ctx, ch, len);
}

#[cfg(feature = "push")]
unsafe extern "C" fn processing_instruction_bnd(
    ctx: *mut c_void,
    target: *const XmlChar,
    data: *const XmlChar,
) {
    use exml::libxml::sax2::xmlSAX2ProcessingInstruction;

    PUSH_BOUNDARY_COUNT += 1;
    xmlSAX2ProcessingInstruction(ctx, target, data);
}

#[cfg(feature = "push")]
unsafe extern "C" fn comment_bnd(ctx: *mut c_void, value: *const XmlChar) {
    use exml::libxml::sax2::xmlSAX2Comment;

    let ctxt: XmlParserCtxtPtr = ctx as _;
    if (*ctxt).in_subset == 0 {
        PUSH_BOUNDARY_COUNT += 1;
    }
    xmlSAX2Comment(ctx, value);
}

#[cfg(feature = "push")]
unsafe extern "C" fn start_element_bnd(
    ctx: *mut c_void,
    xname: *const XmlChar,
    atts: *mut *const XmlChar,
) {
    use exml::libxml::sax2::xmlSAX2StartElement;

    let name: *const c_char = xname as *const c_char;

    /* Some elements might be created automatically. */
    if strcmp(name, c"html".as_ptr()) != 0
        && strcmp(name, c"body".as_ptr()) != 0
        && strcmp(name, c"head".as_ptr()) != 0
        && strcmp(name, c"p".as_ptr()) != 0
    {
        PUSH_BOUNDARY_COUNT += 1;
    }
    xmlSAX2StartElement(ctx, xname, atts);
}

#[cfg(feature = "push")]
unsafe extern "C" fn end_element_bnd(ctx: *mut c_void, name: *const XmlChar) {
    /*pushBoundaryCount++;*/

    use exml::libxml::sax2::xmlSAX2EndElement;
    xmlSAX2EndElement(ctx, name);
}

#[cfg(feature = "push")]
unsafe extern "C" fn start_element_ns_bnd(
    ctx: *mut c_void,
    localname: *const XmlChar,
    prefix: *const XmlChar,
    uri: *const XmlChar,
    nb_namespaces: c_int,
    namespaces: *mut *const XmlChar,
    nb_attributes: c_int,
    nb_defaulted: c_int,
    attributes: *mut *const XmlChar,
) {
    use exml::libxml::sax2::xmlSAX2StartElementNs;

    PUSH_BOUNDARY_COUNT += 1;
    xmlSAX2StartElementNs(
        ctx,
        localname,
        prefix,
        uri,
        nb_namespaces,
        namespaces,
        nb_attributes,
        nb_defaulted,
        attributes,
    );
}

#[cfg(feature = "push")]
unsafe extern "C" fn end_element_ns_bnd(
    ctx: *mut c_void,
    localname: *const XmlChar,
    prefix: *const XmlChar,
    uri: *const XmlChar,
) {
    /*pushBoundaryCount++;*/

    use exml::libxml::sax2::xmlSAX2EndElementNs;
    xmlSAX2EndElementNs(ctx, localname, prefix, uri);
}

/**
 * pushBoundaryTest:
 * @filename: the file to parse
 * @result: the file with expected result
 * @err: the file with error messages: unused
 *
 * Test whether the push parser detects boundaries between syntactical
 * elements correctly.
 *
 * Returns 0 in case of success, an error code otherwise
 */
#[cfg(feature = "push")]
unsafe extern "C" fn push_boundary_test(
    filename: *const c_char,
    result: *const c_char,
    err: *const c_char,
    options: c_int,
) -> c_int {
    use exml::libxml::{
        encoding::XmlCharEncoding,
        htmlparser::{html_create_push_parser_ctxt, html_parse_chunk},
        htmltree::htmlDocDumpMemory,
        parser::{
            xml_create_push_parser_ctxt, xml_parse_chunk, XmlParserInputState, XmlSAXHandler,
        },
        sax2::{xmlSAX2InitHtmlDefaultSAXHandler, xmlSAXVersion},
        tree::xml_doc_dump_memory,
    };
    use libc::memset;

    let mut bnd_sax: XmlSAXHandler = unsafe { zeroed() };
    let mut base: *const c_char = null();
    let mut size: c_int = 0;
    let mut res: c_int;
    let mut num_callbacks: c_int;
    let mut cur: c_int;
    let mut avail: c_ulong;
    let mut old_consumed: c_ulong = 0;
    let mut consumed: c_ulong;

    /*
     * If the parser made progress, check that exactly one construct was
     * processed and that the input buffer is (almost) empty.
     * Since we use a chunk size of 1, this tests whether content is
     * processed as early as possible.
     */

    NB_TESTS += 1;

    memset(addr_of_mut!(bnd_sax) as _, 0, size_of::<XmlSAXHandler>());
    #[cfg(feature = "html")]
    if options & XML_PARSE_HTML != 0 {
        xmlSAX2InitHtmlDefaultSAXHandler(addr_of_mut!(bnd_sax) as _);
        bnd_sax.start_element = Some(start_element_bnd);
        bnd_sax.end_element = Some(end_element_bnd);
    } else {
        xmlSAXVersion(addr_of_mut!(bnd_sax) as _, 2);
        bnd_sax.start_element_ns = Some(start_element_ns_bnd);
        bnd_sax.end_element_ns = Some(end_element_ns_bnd);
    }
    #[cfg(not(feature = "html"))]
    {
        xmlSAXVersion(addr_of_mut!(bndSAX) as _, 2);
        bndSAX.startElementNs = Some(start_element_ns_bnd);
        bndSAX.endElementNs = Some(end_element_ns_bnd);
    }

    bnd_sax.internal_subset = Some(internal_subset_bnd);
    bnd_sax.reference = Some(reference_bnd);
    bnd_sax.characters = Some(characters_bnd);
    bnd_sax.cdata_block = Some(cdata_block_bnd);
    bnd_sax.processing_instruction = Some(processing_instruction_bnd);
    bnd_sax.comment = Some(comment_bnd);

    /*
     * load the document in memory and work from there.
     */
    if load_mem(filename, addr_of_mut!(base), addr_of_mut!(size)) != 0 {
        eprintln!(
            "Failed to load {}",
            CStr::from_ptr(filename).to_string_lossy()
        );
        return -1;
    }

    #[cfg(feature = "html")]
    let ctxt = if options & XML_PARSE_HTML != 0 {
        html_create_push_parser_ctxt(
            addr_of_mut!(bnd_sax) as _,
            null_mut(),
            base,
            1,
            filename,
            XmlCharEncoding::XmlCharEncodingNone,
        )
    } else {
        xml_create_push_parser_ctxt(addr_of_mut!(bnd_sax) as _, null_mut(), base, 1, filename)
    };
    #[cfg(not(feature = "html"))]
    let ctxt =
        xml_create_push_parser_ctxt(addr_of_mut!(bndSAX) as _, null_mut(), base, 1, filename);
    xml_ctxt_use_options(ctxt, options);
    cur = 1;
    consumed = 0;
    num_callbacks = 0;
    avail = 0;
    while cur < size && num_callbacks <= 1 && avail == 0 {
        let terminate = (cur + 1 >= size) as c_int;
        let mut is_text: c_int = 0;

        if (*ctxt).instate == XmlParserInputState::XmlParserContent {
            let first_char: c_int = if (*(*ctxt).input).end > (*(*ctxt).input).cur {
                *(*(*ctxt).input).cur as i32
            } else {
                *base.add(cur as usize) as i32
            };

            if first_char != b'<' as i32
                && (options & XML_PARSE_HTML != 0 || first_char != b'&' as i32)
            {
                is_text = 1;
            }
        }

        old_consumed = (*(*ctxt).input).consumed
            + (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as c_ulong;

        PUSH_BOUNDARY_COUNT = 0;
        PUSH_BOUNDARY_REF_COUNT = 0;
        PUSH_BOUNDARY_CHARS_COUNT = 0;
        PUSH_BOUNDARY_CDATA_COUNT = 0;

        #[cfg(feature = "html")]
        if options & XML_PARSE_HTML != 0 {
            html_parse_chunk(ctxt, base.add(cur as _), 1, terminate);
        } else {
            xml_parse_chunk(ctxt, base.add(cur as _), 1, terminate);
        }
        #[cfg(not(feature = "html"))]
        {
            xml_parse_chunk(ctxt, base.add(cur as _), 1, terminate);
        }
        cur += 1;

        /*
         * Callback check: Check that only a single construct was parsed.
         */
        if PUSH_BOUNDARY_REF_COUNT > 0 {
            num_callbacks = 1;
        } else {
            num_callbacks = PUSH_BOUNDARY_COUNT;
            if PUSH_BOUNDARY_CHARS_COUNT > 1 {
                if options & XML_PARSE_HTML != 0 {
                    /*
                     * The HTML parser can generate a mix of chars and
                     * references.
                     */
                    num_callbacks -= PUSH_BOUNDARY_CHARS_COUNT - 1;
                } else {
                    /*
                     * Allow two chars callbacks. This can happen when
                     * multi-byte chars are split across buffer boundaries.
                     */
                    num_callbacks -= 1;
                }
            }
            if options & XML_PARSE_HTML != 0 {
                /*
                 * Allow multiple cdata callbacks in HTML mode.
                 */
                if PUSH_BOUNDARY_CDATA_COUNT > 1 {
                    num_callbacks -= PUSH_BOUNDARY_CDATA_COUNT - 1;
                }
            }
        }

        /*
         * Buffer check: If input was consumed, check that the input
         * buffer is (almost) empty.
         */
        consumed = (*(*ctxt).input).consumed
            + (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as c_ulong;
        if (*ctxt).instate != XmlParserInputState::XmlParserDTD
            && consumed >= 4
            && consumed != old_consumed
        {
            let mut max: size_t = 0;

            avail = (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) as _;

            if options & XML_PARSE_HTML != 0
                && ((*ctxt).instate == XmlParserInputState::XmlParserEndTag)
            {
                /* Something related to script parsing. */
                max = 3;
            } else if is_text != 0 {
                let c: c_int = *(*(*ctxt).input).cur as i32;

                /* 3 bytes for partial UTF-8 */
                max = if c == b'<' as i32 || c == b'&' as i32 {
                    1
                } else {
                    3
                };
            } else if (*ctxt).instate == XmlParserInputState::XmlParserCDATASection {
                /* 2 bytes for terminator, 3 bytes for UTF-8 */
                max = 5;
            }

            if avail <= max as _ {
                avail = 0;
            }
        }
    }
    let doc: XmlDocPtr = (*ctxt).my_doc;
    #[cfg(feature = "html")]
    if options & XML_PARSE_HTML != 0 {
        res = 1;
    } else {
        res = (*ctxt).well_formed;
    }
    #[cfg(not(feature = "html"))]
    {
        res = (*ctxt).wellFormed;
    }
    xml_free_parser_ctxt(ctxt);
    free(base as _);
    if num_callbacks > 1 {
        xml_free_doc(doc);
        eprintln!(
            "Failed push boundary callback test ({}@{}-{}): {}",
            num_callbacks,
            old_consumed,
            consumed,
            CStr::from_ptr(filename).to_string_lossy(),
        );
        return -1;
    }
    if avail > 0 {
        xml_free_doc(doc);
        eprintln!(
            "Failed push boundary buffer test ({}@{}): {}",
            avail,
            consumed,
            CStr::from_ptr(filename).to_string_lossy(),
        );
        return -1;
    }
    if res == 0 {
        xml_free_doc(doc);
        eprintln!(
            "Failed to parse {}",
            CStr::from_ptr(filename).to_string_lossy()
        );
        return -1;
    }
    #[cfg(feature = "html")]
    if options & XML_PARSE_HTML != 0 {
        htmlDocDumpMemory(
            doc,
            addr_of_mut!(base) as *mut *mut XmlChar,
            addr_of_mut!(size),
        );
    } else {
        xml_doc_dump_memory(
            doc,
            addr_of_mut!(base) as *mut *mut XmlChar,
            addr_of_mut!(size),
        );
    }
    #[cfg(not(feature = "html"))]
    {
        xml_doc_dump_memory(
            doc,
            addr_of_mut!(base) as *mut *mut XmlChar,
            addr_of_mut!(size),
        );
    }
    xml_free_doc(doc);
    res = compare_file_mem(result, base, size);
    if base.is_null() || res != 0 {
        if !base.is_null() {
            xml_free(base as _);
        }
        eprintln!(
            "Result for {} failed in {}",
            CStr::from_ptr(filename).to_string_lossy(),
            CStr::from_ptr(result).to_string_lossy()
        );
        return -1;
    }
    xml_free(base as _);
    if !err.is_null() {
        res = compare_file_mem(err, TEST_ERRORS.as_ptr() as _, TEST_ERRORS_SIZE as _);
        if res != 0 {
            eprintln!(
                "Error for {} failed",
                CStr::from_ptr(filename).to_string_lossy()
            );
            return -1;
        }
    }
    0
}

/**
 * memParseTest:
 * @filename: the file to parse
 * @result: the file with expected result
 * @err: the file with error messages: unused
 *
 * Parse a file using the old xmlReadMemory API, then serialize back
 * reparse the result and serialize again, then check for deviation
 * in serialization.
 *
 * Returns 0 in case of success, an error code otherwise
 */
unsafe extern "C" fn mem_parse_test(
    filename: *const c_char,
    result: *const c_char,
    _err: *const c_char,
    _options: c_int,
) -> c_int {
    let mut base: *const c_char = null();
    let mut size: c_int = 0;

    NB_TESTS += 1;
    /*
     * load and parse the memory
     */
    if load_mem(filename, addr_of_mut!(base), addr_of_mut!(size)) != 0 {
        eprintln!(
            "Failed to load {}",
            CStr::from_ptr(filename).to_string_lossy()
        );
        return -1;
    }

    let doc: XmlDocPtr = xml_read_memory(base, size, filename, null_mut(), 0);
    unload_mem(base);
    if doc.is_null() {
        return 1;
    }
    xml_doc_dump_memory(
        doc,
        addr_of_mut!(base) as *mut *mut XmlChar,
        addr_of_mut!(size),
    );
    xml_free_doc(doc);
    let res: c_int = compare_file_mem(result, base, size);
    if base.is_null() || res != 0 {
        if !base.is_null() {
            xml_free(base as _);
        }
        eprintln!(
            "Result for {} failed in {}",
            CStr::from_ptr(filename).to_string_lossy(),
            CStr::from_ptr(result).to_string_lossy()
        );
        return -1;
    }
    xml_free(base as _);
    0
}

/**
 * noentParseTest:
 * @filename: the file to parse
 * @result: the file with expected result
 * @err: the file with error messages: unused
 *
 * Parse a file with entity resolution, then serialize back
 * reparse the result and serialize again, then check for deviation
 * in serialization.
 *
 * Returns 0 in case of success, an error code otherwise
 */
unsafe extern "C" fn noent_parse_test(
    filename: *const c_char,
    result: *const c_char,
    _err: *const c_char,
    options: c_int,
) -> c_int {
    let mut doc: XmlDocPtr;
    let mut res: c_int = 0;

    NB_TESTS += 1;
    /*
     * base of the test, parse with the old API
     */
    doc = xml_read_file(filename, null_mut(), options);
    if doc.is_null() {
        return 1;
    }
    let temp: *mut c_char = result_filename(
        filename,
        TEMP_DIRECTORY
            .get()
            .map(|t| t.as_ptr())
            .unwrap_or(null_mut()),
        c".res".as_ptr(),
    );
    if temp.is_null() {
        eprintln!("Out of memory");
        fatal_error();
    }
    xml_save_file(temp, doc);
    if compare_files(temp, result) != 0 {
        res = 1;
    }
    xml_free_doc(doc);

    /*
     * Parse the saved result to make sure the round trip is okay
     */
    doc = xml_read_file(filename, null_mut(), options);
    if doc.is_null() {
        return 1;
    }
    xml_save_file(temp, doc);
    if compare_files(temp, result) != 0 {
        res = 1;
    }
    xml_free_doc(doc);

    if !temp.is_null() {
        unlink(temp);
        free(temp as _);
    }
    res
}

/**
 * errParseTest:
 * @filename: the file to parse
 * @result: the file with expected result
 * @err: the file with error messages
 *
 * Parse a file using the xmlReadFile API and check for errors.
 *
 * Returns 0 in case of success, an error code otherwise
 */
unsafe extern "C" fn err_parse_test(
    filename: *const c_char,
    result: *const c_char,
    err: *const c_char,
    options: c_int,
) -> c_int {
    let mut doc: XmlDocPtr;
    let mut base: *const c_char = null_mut();
    let mut size: c_int = 0;
    let mut res: c_int = 0;

    NB_TESTS += 1;
    if cfg!(feature = "html") && options & XML_PARSE_HTML != 0 {
        #[cfg(feature = "html")]
        {
            doc = html_read_file(filename, null_mut(), options);
        }
    } else if cfg!(feature = "xinclude") && options & XmlParserOption::XmlParseXinclude as i32 != 0
    {
        #[cfg(feature = "xinclude")]
        {
            doc = xml_read_file(filename, null_mut(), options);
            if xml_xinclude_process_flags(doc, options) < 0 {
                xml_free_doc(doc);
                doc = null_mut();
            }
        }
    } else {
        doc = xml_read_file(filename, null_mut(), options);
    }
    if !result.is_null() {
        if doc.is_null() {
            base = c"".as_ptr();
            size = 0;
        } else {
            #[cfg(feature = "html")]
            if options & XML_PARSE_HTML != 0 {
                htmlDocDumpMemory(
                    doc,
                    addr_of_mut!(base) as *mut *mut XmlChar,
                    addr_of_mut!(size),
                );
            } else {
                xml_doc_dump_memory(
                    doc,
                    addr_of_mut!(base) as *mut *mut XmlChar,
                    addr_of_mut!(size),
                );
            }
            #[cfg(not(feature = "html"))]
            {
                xml_doc_dump_memory(
                    doc,
                    addr_of_mut!(base) as *mut *mut XmlChar,
                    addr_of_mut!(size),
                );
            }
        }
        res = compare_file_mem(result, base, size);
    }
    if !doc.is_null() {
        if !base.is_null() {
            xml_free(base as _);
        }
        xml_free_doc(doc);
    }
    if res != 0 {
        eprintln!(
            "Result for {} failed in {}",
            CStr::from_ptr(filename).to_string_lossy(),
            CStr::from_ptr(result).to_string_lossy()
        );
        return -1;
    }
    if !err.is_null() {
        res = compare_file_mem(err, TEST_ERRORS.as_ptr() as _, TEST_ERRORS_SIZE as _);
        if res != 0 {
            eprintln!(
                "Error for {} failed",
                CStr::from_ptr(filename).to_string_lossy()
            );
            return -1;
        }
    } else if options & XmlParserOption::XmlParseDtdvalid as i32 != 0 && TEST_ERRORS_SIZE != 0 {
        eprintln!(
            "Validation for {} failed",
            CStr::from_ptr(filename).to_string_lossy()
        );
    }

    0
}

/**
 * fdParseTest:
 * @filename: the file to parse
 * @result: the file with expected result
 * @err: the file with error messages
 *
 * Parse a file using the xmlReadFd API and check for errors.
 *
 * Returns 0 in case of success, an error code otherwise
 */
unsafe extern "C" fn fd_parse_test(
    filename: *const c_char,
    result: *const c_char,
    err: *const c_char,
    options: c_int,
) -> c_int {
    let mut base: *const c_char = null_mut();
    let mut size: c_int = 0;
    let mut res: c_int = 0;

    NB_TESTS += 1;
    let fd: c_int = open(filename, RD_FLAGS);
    #[cfg(feature = "html")]
    let doc = if options & XML_PARSE_HTML != 0 {
        html_read_fd(fd, filename, null_mut(), options)
    } else {
        xml_read_fd(fd, filename, null_mut(), options)
    };
    #[cfg(not(feature = "html"))]
    let doc = xml_read_fd(fd, filename, null_mut(), options);

    close(fd);
    if !result.is_null() {
        if doc.is_null() {
            base = c"".as_ptr();
            size = 0;
        } else {
            #[cfg(feature = "html")]
            if options & XML_PARSE_HTML != 0 {
                htmlDocDumpMemory(
                    doc,
                    addr_of_mut!(base) as *mut *mut XmlChar,
                    addr_of_mut!(size),
                );
            } else {
                xml_doc_dump_memory(
                    doc,
                    addr_of_mut!(base) as *mut *mut XmlChar,
                    addr_of_mut!(size),
                );
            }
            #[cfg(not(feature = "html"))]
            {
                xml_doc_dump_memory(
                    doc,
                    addr_of_mut!(base) as *mut *mut XmlChar,
                    addr_of_mut!(size),
                );
            }
        }
        res = compare_file_mem(result, base, size);
    }
    if !doc.is_null() {
        if !base.is_null() {
            xml_free(base as _);
        }
        xml_free_doc(doc);
    }
    if res != 0 {
        eprintln!(
            "Result for {} failed in {}",
            CStr::from_ptr(filename).to_string_lossy(),
            CStr::from_ptr(result).to_string_lossy()
        );
        return -1;
    }
    if !err.is_null() {
        res = compare_file_mem(err, TEST_ERRORS.as_ptr() as _, TEST_ERRORS_SIZE as _);
        if res != 0 {
            eprintln!(
                "Error for {} failed",
                CStr::from_ptr(filename).to_string_lossy()
            );
            return -1;
        }
    } else if options & XmlParserOption::XmlParseDtdvalid as i32 != 0 && TEST_ERRORS_SIZE != 0 {
        eprintln!(
            "Validation for {} failed",
            CStr::from_ptr(filename).to_string_lossy()
        );
    }

    0
}

/************************************************************************
 *                                    *
 *        Reader based tests                    *
 *                                    *
 ************************************************************************/
#[cfg(feature = "libxml_reader")]
unsafe extern "C" fn process_node(out: &mut File, reader: XmlTextReaderPtr) {
    use exml::libxml::xmlreader::{
        xml_text_reader_const_name, xml_text_reader_const_value, xml_text_reader_depth,
        xml_text_reader_has_value, xml_text_reader_is_empty_element, xml_text_reader_node_type,
    };

    let mut name: *const XmlChar;

    let typ: c_int = xml_text_reader_node_type(reader);
    let empty: c_int = xml_text_reader_is_empty_element(reader);

    name = xml_text_reader_const_name(reader);
    if name.is_null() {
        name = c"--".as_ptr() as _;
    }

    let value: *const XmlChar = xml_text_reader_const_value(reader);

    write!(
        out,
        "{} {} {} {} {}",
        xml_text_reader_depth(reader),
        typ,
        CStr::from_ptr(name as _).to_string_lossy(),
        empty,
        xml_text_reader_has_value(reader),
    )
    .ok();
    if value.is_null() {
        writeln!(out).ok();
    } else {
        writeln!(out, " {}", CStr::from_ptr(value as _).to_string_lossy()).ok();
    }
}

#[cfg(feature = "libxml_reader")]
unsafe extern "C" fn stream_process_test(
    filename: *const c_char,
    result: *const c_char,
    err: *const c_char,
    reader: XmlTextReaderPtr,
    rng: *const c_char,
    _options: c_int,
) -> c_int {
    use exml::libxml::xmlreader::{
        xml_text_reader_is_valid, xml_text_reader_read, xml_text_reader_relaxng_validate,
    };

    let mut ret: c_int;
    let mut temp: *mut c_char = null_mut();

    if reader.is_null() {
        return -1;
    }

    NB_TESTS += 1;
    let mut t = None;
    if !result.is_null() {
        temp = result_filename(
            filename,
            TEMP_DIRECTORY
                .get()
                .map(|t| t.as_ptr())
                .unwrap_or(null_mut()),
            c".res".as_ptr(),
        );
        if temp.is_null() {
            eprintln!("Out of memory");
            fatal_error();
        }
        match File::options()
            .write(true)
            .truncate(true)
            .create(true)
            .open(CStr::from_ptr(temp).to_string_lossy().as_ref())
        {
            Ok(file) => t = Some(file),
            _ => {
                eprintln!(
                    "Can't open temp file {}",
                    CStr::from_ptr(temp).to_string_lossy()
                );
                free(temp as _);
                return -1;
            }
        };
    }
    #[cfg(feature = "schema")]
    if !rng.is_null() {
        ret = xml_text_reader_relaxng_validate(reader, rng);
        if ret < 0 {
            xml_error_with_format!(
                test_error_handler,
                null_mut(),
                c"Relax-NG schema %s failed to compile\n".as_ptr(),
                rng
            );

            if !temp.is_null() {
                unlink(temp);
                free(temp as _);
            }
            return 0;
        }
    }
    ret = xml_text_reader_read(reader);
    while ret == 1 {
        if let Some(t) = t.as_mut().filter(|_| rng.is_null()) {
            process_node(t, reader);
        }
        ret = xml_text_reader_read(reader);
    }
    if ret != 0 {
        xml_error_with_format!(
            test_error_handler,
            null_mut(),
            c"%s : failed to parse\n".as_ptr(),
            filename
        );
    }
    if !rng.is_null() {
        if xml_text_reader_is_valid(reader) != 1 {
            xml_error_with_format!(
                test_error_handler,
                null_mut(),
                c"%s fails to validate\n".as_ptr(),
                filename
            );
        } else {
            xml_error_with_format!(
                test_error_handler,
                null_mut(),
                c"%s validates\n".as_ptr(),
                filename
            );
        }
    }
    if t.is_some() {
        ret = compare_files(temp, result);
        if !temp.is_null() {
            unlink(temp);
            free(temp as _);
        }
        if ret != 0 {
            eprintln!(
                "Result for {} failed in {}",
                CStr::from_ptr(filename).to_string_lossy(),
                CStr::from_ptr(result).to_string_lossy()
            );
            return -1;
        }
    }
    if !err.is_null() {
        ret = compare_file_mem(err, TEST_ERRORS.as_ptr() as _, TEST_ERRORS_SIZE as _);
        if ret != 0 {
            eprintln!(
                "Error for {} failed",
                CStr::from_ptr(filename).to_string_lossy()
            );
            print!(
                "{}",
                CStr::from_ptr(TEST_ERRORS.as_ptr() as _).to_string_lossy()
            );
            return -1;
        }
    }

    0
}

/**
 * streamParseTest:
 * @filename: the file to parse
 * @result: the file with expected result
 * @err: the file with error messages
 *
 * Parse a file using the reader API and check for errors.
 *
 * Returns 0 in case of success, an error code otherwise
 */
#[cfg(feature = "libxml_reader")]
unsafe extern "C" fn stream_parse_test(
    filename: *const c_char,
    result: *const c_char,
    err: *const c_char,
    options: c_int,
) -> c_int {
    use exml::libxml::xmlreader::{xml_free_text_reader, xml_reader_for_file};

    let reader: XmlTextReaderPtr = xml_reader_for_file(filename, null_mut(), options);
    let ret: c_int = stream_process_test(filename, result, err, reader, null_mut(), options);
    xml_free_text_reader(reader);
    ret
}

/**
 * walkerParseTest:
 * @filename: the file to parse
 * @result: the file with expected result
 * @err: the file with error messages
 *
 * Parse a file using the walker, i.e. a reader built from a atree.
 *
 * Returns 0 in case of success, an error code otherwise
 */
#[cfg(feature = "libxml_reader")]
unsafe extern "C" fn walker_parse_test(
    filename: *const c_char,
    result: *const c_char,
    err: *const c_char,
    options: c_int,
) -> c_int {
    use exml::libxml::xmlreader::{xml_free_text_reader, xml_reader_walker};

    let doc: XmlDocPtr = xml_read_file(filename, null_mut(), options);
    if doc.is_null() {
        eprintln!(
            "Failed to parse {}",
            CStr::from_ptr(filename).to_string_lossy()
        );
        return -1;
    }
    let reader: XmlTextReaderPtr = xml_reader_walker(doc);
    let ret: c_int = stream_process_test(filename, result, err, reader, null_mut(), options);
    xml_free_text_reader(reader);
    xml_free_doc(doc);
    ret
}

/**
 * streamMemParseTest:
 * @filename: the file to parse
 * @result: the file with expected result
 * @err: the file with error messages
 *
 * Parse a file using the reader API from memory and check for errors.
 *
 * Returns 0 in case of success, an error code otherwise
 */
#[cfg(feature = "libxml_reader")]
unsafe extern "C" fn stream_mem_parse_test(
    filename: *const c_char,
    result: *const c_char,
    err: *const c_char,
    options: c_int,
) -> c_int {
    use exml::libxml::xmlreader::{xml_free_text_reader, xml_reader_for_memory};

    let mut base: *const c_char = null();
    let mut size: c_int = 0;

    /*
     * load and parse the memory
     */
    if load_mem(filename, addr_of_mut!(base), addr_of_mut!(size)) != 0 {
        eprintln!(
            "Failed to load {}",
            CStr::from_ptr(filename).to_string_lossy()
        );
        return -1;
    }
    let reader: XmlTextReaderPtr = xml_reader_for_memory(base, size, filename, null_mut(), options);
    let ret: c_int = stream_process_test(filename, result, err, reader, null_mut(), options);
    free(base as _);
    xml_free_text_reader(reader);
    ret
}

/************************************************************************
 *                                    *
 *        XPath and XPointer based tests                *
 *                                    *
 ************************************************************************/
#[cfg(all(feature = "xpath", feature = "libxml_debug"))]
static XPATH_OUTPUT: Mutex<Option<File>> = Mutex::new(None);
#[cfg(all(feature = "xpath", feature = "libxml_debug"))]
static XPATH_DOCUMENT: AtomicPtr<XmlDoc> = AtomicPtr::new(null_mut());

#[cfg(all(feature = "xpath", feature = "libxml_debug"))]
unsafe extern "C" fn ignore_generic_error(_ctx: *mut c_void, _msg: *const c_char) {}

#[cfg(all(feature = "xpath", feature = "libxml_debug"))]
unsafe extern "C" fn test_xpath(str: *const c_char, xptr: c_int, expr: c_int) {
    use std::sync::atomic::Ordering;

    use exml::libxml::{
        tree::xml_doc_get_root_element,
        xpath::{
            xml_xpath_compile, xml_xpath_compiled_eval, xml_xpath_eval_expression,
            xml_xpath_free_comp_expr, xml_xpath_free_context, xml_xpath_free_object,
            xml_xpath_new_context, XmlXPathCompExprPtr, XmlXPathContextPtr, XmlXPathObjectPtr,
        },
        xpath_internals::xml_xpath_debug_dump_object,
        xpointer::{xml_xptr_eval, xml_xptr_new_context},
    };

    let res: XmlXPathObjectPtr;
    let ctxt: XmlXPathContextPtr;

    /* Don't print generic errors to stderr. */
    xmlSetGenericErrorFunc(null_mut(), Some(ignore_generic_error));

    NB_TESTS += 1;
    if cfg!(feature = "libxml_xptr") && xptr != 0 {
        #[cfg(feature = "libxml_xptr")]
        {
            ctxt = xml_xptr_new_context(
                XPATH_DOCUMENT.load(Ordering::Relaxed),
                null_mut(),
                null_mut(),
            );
            res = xml_xptr_eval(str as _, ctxt);
        }
    } else {
        ctxt = xml_xpath_new_context(XPATH_DOCUMENT.load(Ordering::Relaxed));
        (*ctxt).node = xml_doc_get_root_element(XPATH_DOCUMENT.load(Ordering::Relaxed));
        if expr != 0 {
            res = xml_xpath_eval_expression(str as _, ctxt);
        } else {
            /* res = xmlXPathEval(str, ctxt); */

            let comp: XmlXPathCompExprPtr = xml_xpath_compile(str as _);
            if !comp.is_null() {
                res = xml_xpath_compiled_eval(comp, ctxt);
                xml_xpath_free_comp_expr(comp);
            } else {
                res = null_mut();
            }
        }
    }
    let fd = fdopen(
        XPATH_OUTPUT.lock().unwrap().as_ref().unwrap().as_raw_fd(),
        c"wb".as_ptr(),
    );
    assert!(!fd.is_null());
    xml_xpath_debug_dump_object(fd, res, 0);
    fflush(fd);
    xml_xpath_free_object(res);
    xml_xpath_free_context(ctxt);

    /* Reset generic error handler. */
    xmlSetGenericErrorFunc(null_mut(), None);
}

/**
 * xpathExprTest:
 * @filename: the file to parse
 * @result: the file with expected result
 * @err: the file with error messages
 *
 * Parse a file containing XPath standalone expressions and evaluate them
 *
 * Returns 0 in case of success, an error code otherwise
 */
#[cfg(all(feature = "xpath", feature = "libxml_debug"))]
unsafe extern "C" fn xpath_common_test(
    filename: *const c_char,
    result: *const c_char,
    xptr: c_int,
    expr: c_int,
) -> c_int {
    use std::io::{BufRead, BufReader};

    use exml::{libxml::globals::xml_generic_error_context, xml_generic_error};

    let mut ret: c_int = 0;

    let temp: *mut c_char = result_filename(
        filename,
        TEMP_DIRECTORY
            .get()
            .map(|t| t.as_ptr())
            .unwrap_or(null_mut()),
        c".res".as_ptr(),
    );
    if temp.is_null() {
        eprintln!("Out of memory");
        fatal_error();
    }

    let Ok(out) = File::options()
        .write(true)
        .truncate(true)
        .create(true)
        .open(CStr::from_ptr(temp).to_string_lossy().as_ref())
    else {
        eprintln!(
            "failed to open output file {}",
            CStr::from_ptr(temp).to_string_lossy()
        );
        free(temp as _);
        return -1;
    };
    *XPATH_OUTPUT.lock().unwrap() = Some(out);

    let mut input = match File::open(CStr::from_ptr(filename).to_string_lossy().as_ref()) {
        Ok(file) => BufReader::new(file),
        _ => {
            xml_generic_error!(
                xml_generic_error_context(),
                c"Cannot open %s for reading\n".as_ptr(),
                filename
            );
            free(temp as _);
            return -1;
        }
    };

    let mut expression = vec![];
    while input
        .read_until(b'\n', &mut expression)
        .ok()
        .filter(|&len| len > 0)
        .map(|len| len as i32)
        .is_some()
    {
        while expression
            .last()
            .filter(|&c| matches!(c, b'\n' | b'\t' | b'\r' | b' '))
            .is_some()
        {
            expression.pop();
        }
        if !expression.is_empty() {
            expression.push(0);
            writeln!(
                XPATH_OUTPUT.lock().unwrap().as_mut().unwrap(),
                "\n========================\nExpression: {}",
                CStr::from_ptr(expression.as_ptr() as _).to_string_lossy(),
            )
            .ok();
            test_xpath(expression.as_ptr() as _, xptr, expr);
        }
        expression.clear();
    }

    if !result.is_null() {
        ret = compare_files(temp, result);
        if ret != 0 {
            eprintln!(
                "Result for {} failed in {}",
                CStr::from_ptr(filename).to_string_lossy(),
                CStr::from_ptr(result).to_string_lossy()
            );
        }
    }

    if !temp.is_null() {
        unlink(temp);
        free(temp as _);
    }
    ret
}

/**
 * xpathExprTest:
 * @filename: the file to parse
 * @result: the file with expected result
 * @err: the file with error messages
 *
 * Parse a file containing XPath standalone expressions and evaluate them
 *
 * Returns 0 in case of success, an error code otherwise
 */
#[cfg(all(feature = "xpath", feature = "libxml_debug"))]
unsafe extern "C" fn xpath_expr_test(
    filename: *const c_char,
    result: *const c_char,
    _err: *const c_char,
    _options: c_int,
) -> c_int {
    xpath_common_test(filename, result, 0, 1)
}

/**
 * xpathDocTest:
 * @filename: the file to parse
 * @result: the file with expected result
 * @err: the file with error messages
 *
 * Parse a file containing XPath expressions and evaluate them against
 * a set of corresponding documents.
 *
 * Returns 0 in case of success, an error code otherwise
 */
#[cfg(all(feature = "xpath", feature = "libxml_debug"))]
unsafe extern "C" fn xpath_doc_test(
    filename: *const c_char,
    _resul: *const c_char,
    _err: *const c_char,
    options: c_int,
) -> c_int {
    use std::{mem::zeroed, sync::atomic::Ordering};

    use libc::{glob, glob_t, globfree, GLOB_DOOFFS};

    let mut pattern: [c_char; 500] = [0; 500];
    let mut result: [c_char; 500] = [0; 500];
    let mut globbuf: glob_t = unsafe { zeroed() };
    let mut ret: c_int = 0;
    let mut res: c_int;

    let xpath_document = xml_read_file(
        filename,
        null_mut(),
        options | XmlParserOption::XmlParseDtdattr as i32 | XmlParserOption::XmlParseNoent as i32,
    );
    if xpath_document.is_null() {
        eprintln!(
            "Failed to load {}",
            CStr::from_ptr(filename).to_string_lossy()
        );
        return -1;
    } else {
        XPATH_DOCUMENT.store(xpath_document, Ordering::Relaxed);
    }

    res = snprintf(
        pattern.as_mut_ptr() as _,
        499,
        c"./test/XPath/tests/%s*".as_ptr(),
        base_filename(filename),
    );
    if res >= 499 {
        pattern[499] = 0;
    }
    globbuf.gl_offs = 0;
    glob(pattern.as_ptr(), GLOB_DOOFFS, None, addr_of_mut!(globbuf));
    for i in 0..globbuf.gl_pathc {
        res = snprintf(
            result.as_mut_ptr() as _,
            499,
            c"./result/XPath/tests/%s".as_ptr(),
            base_filename(*globbuf.gl_pathv.add(i)),
        );
        if res >= 499 {
            result[499] = 0;
        }
        res = xpath_common_test(*globbuf.gl_pathv.add(i), addr_of_mut!(result[0]), 0, 0);
        if res != 0 {
            ret = res;
        }
    }
    globfree(addr_of_mut!(globbuf));

    xml_free_doc(XPATH_DOCUMENT.load(Ordering::Relaxed));
    ret
}

/**
 * xptrDocTest:
 * @filename: the file to parse
 * @result: the file with expected result
 * @err: the file with error messages
 *
 * Parse a file containing XPath expressions and evaluate them against
 * a set of corresponding documents.
 *
 * Returns 0 in case of success, an error code otherwise
 */
#[cfg(all(feature = "xpath", feature = "libxml_debug", feature = "libxml_xptr"))]
unsafe extern "C" fn xptr_doc_test(
    filename: *const c_char,
    _resul: *const c_char,
    _err: *const c_char,
    options: c_int,
) -> c_int {
    use std::{mem::zeroed, sync::atomic::Ordering};

    use libc::{glob, glob_t, globfree, GLOB_DOOFFS};

    let mut pattern: [c_char; 500] = [0; 500];
    let mut result: [c_char; 500] = [0; 500];
    let mut globbuf: glob_t = unsafe { zeroed() };
    let mut ret: c_int = 0;
    let mut res: c_int;
    let subdir: *const c_char = if options == -1 {
        c"xptr-xp1".as_ptr()
    } else {
        c"xptr".as_ptr()
    };

    let xpath_document = xml_read_file(
        filename,
        null_mut(),
        XmlParserOption::XmlParseDtdattr as i32 | XmlParserOption::XmlParseNoent as i32,
    );
    if xpath_document.is_null() {
        eprintln!(
            "Failed to load {}",
            CStr::from_ptr(filename).to_string_lossy()
        );
        return -1;
    } else {
        XPATH_DOCUMENT.store(xpath_document, Ordering::Relaxed)
    }

    res = snprintf(
        pattern.as_mut_ptr() as _,
        499,
        c"./test/XPath/%s/%s*".as_ptr(),
        subdir,
        base_filename(filename),
    );
    if res >= 499 {
        pattern[499] = 0;
    }
    globbuf.gl_offs = 0;
    glob(pattern.as_ptr(), GLOB_DOOFFS, None, addr_of_mut!(globbuf));
    for i in 0..globbuf.gl_pathc {
        res = snprintf(
            result.as_mut_ptr(),
            499,
            c"./result/XPath/%s/%s".as_ptr(),
            subdir,
            base_filename(*globbuf.gl_pathv.add(i)),
        );
        if res >= 499 {
            result[499] = 0;
        }
        res = xpath_common_test(*globbuf.gl_pathv.add(i), addr_of_mut!(result[0]), 1, 0);
        if res != 0 {
            ret = res;
        }
    }
    globfree(addr_of_mut!(globbuf));

    xml_free_doc(XPATH_DOCUMENT.load(Ordering::Relaxed));
    ret
}

/**
 * xmlidDocTest:
 * @filename: the file to parse
 * @result: the file with expected result
 * @err: the file with error messages
 *
 * Parse a file containing xml:id and check for errors and verify
 * that XPath queries will work on them as expected.
 *
 * Returns 0 in case of success, an error code otherwise
 */
#[cfg(all(feature = "xpath", feature = "libxml_debug", feature = "valid"))]
unsafe extern "C" fn xmlid_doc_test(
    filename: *const c_char,
    result: *const c_char,
    err: *const c_char,
    options: c_int,
) -> c_int {
    use std::sync::atomic::Ordering;

    let mut res: c_int = 0;
    let mut ret: c_int;

    let xpath_document = xml_read_file(
        filename,
        null_mut(),
        options | XmlParserOption::XmlParseDtdattr as i32 | XmlParserOption::XmlParseNoent as i32,
    );
    if xpath_document.is_null() {
        eprintln!(
            "Failed to load {}",
            CStr::from_ptr(filename).to_string_lossy()
        );
        return -1;
    } else {
        XPATH_DOCUMENT.store(xpath_document, Ordering::Relaxed)
    }

    let temp: *mut c_char = result_filename(
        filename,
        TEMP_DIRECTORY
            .get()
            .map(|t| t.as_ptr())
            .unwrap_or(null_mut()),
        c".res".as_ptr(),
    );
    if temp.is_null() {
        eprintln!("Out of memory");
        fatal_error();
    }

    let Ok(out) = File::options()
        .write(true)
        .truncate(true)
        .create(true)
        .open(CStr::from_ptr(temp).to_string_lossy().as_ref())
    else {
        eprintln!(
            "failed to open output file {}",
            CStr::from_ptr(temp).to_string_lossy()
        );
        xml_free_doc(XPATH_DOCUMENT.load(Ordering::Relaxed));
        free(temp as _);
        return -1;
    };
    *XPATH_OUTPUT.lock().unwrap() = Some(out);

    test_xpath(c"id('bar')".as_ptr() as _, 0, 0);

    if !result.is_null() {
        ret = compare_files(temp, result);
        if ret != 0 {
            eprintln!(
                "Result for {} failed in {}",
                CStr::from_ptr(filename).to_string_lossy(),
                CStr::from_ptr(result).to_string_lossy()
            );
            res = 1;
        }
    }

    if !temp.is_null() {
        unlink(temp);
        free(temp as _);
    }
    xml_free_doc(XPATH_DOCUMENT.load(Ordering::Relaxed));

    if !err.is_null() {
        ret = compare_file_mem(err, TEST_ERRORS.as_ptr() as _, TEST_ERRORS_SIZE as _);
        if ret != 0 {
            eprintln!(
                "Error for {} failed",
                CStr::from_ptr(filename).to_string_lossy()
            );
            res = 1;
        }
    }
    res
}

/************************************************************************
 *                                    *
 *            URI based tests                    *
 *                                    *
 ************************************************************************/

unsafe extern "C" fn handle_uri(str: *const c_char, base: *const c_char, o: &mut File) {
    let ret: c_int;
    let mut res: *mut XmlChar = null_mut();
    let uri: XmlURIPtr = xml_create_uri();

    if base.is_null() {
        ret = xml_parse_uri_reference(uri, str);
        if ret != 0 {
            writeln!(
                o,
                "{} : error {}",
                CStr::from_ptr(str).to_string_lossy(),
                ret
            )
            .ok();
        } else {
            xml_normalize_uri_path((*uri).path);
            let fd = fdopen(o.as_raw_fd(), c"wb".as_ptr());
            assert!(!fd.is_null());
            xml_print_uri(fd, uri);
            fflush(fd);
            writeln!(o).ok();
            o.flush().ok();
        }
    } else {
        res = xml_build_uri(str as *mut XmlChar, base as *mut XmlChar);
        if !res.is_null() {
            writeln!(
                o,
                "{}",
                CStr::from_ptr(res as *mut c_char).to_string_lossy()
            )
            .ok();
        } else {
            writeln!(o, "::ERROR::").ok();
        }
    }
    if !res.is_null() {
        xml_free(res as _);
    }
    xml_free_uri(uri);
}

/**
 * uriCommonTest:
 * @filename: the file to parse
 * @result: the file with expected result
 * @err: the file with error messages
 *
 * Parse a file containing URI and check for errors
 *
 * Returns 0 in case of success, an error code otherwise
 */
unsafe extern "C" fn uri_common_test(
    filename: *const c_char,
    result: *const c_char,
    err: *const c_char,
    base: *const c_char,
) -> c_int {
    let mut res: c_int = 0;
    let mut ret: c_int;

    let temp: *mut c_char = result_filename(
        filename,
        TEMP_DIRECTORY
            .get()
            .map(|t| t.as_ptr())
            .unwrap_or(null_mut()),
        c".res".as_ptr(),
    );
    if temp.is_null() {
        eprintln!("Out of memory");
        fatal_error();
    }
    let Ok(mut o) = File::options()
        .write(true)
        .truncate(true)
        .create(true)
        .open(CStr::from_ptr(temp).to_string_lossy().as_ref())
    else {
        eprintln!(
            "failed to open output file {}",
            CStr::from_ptr(temp).to_string_lossy()
        );
        free(temp as _);
        return -1;
    };
    let Ok(mut f) =
        File::open(CStr::from_ptr(filename).to_string_lossy().as_ref()).map(BufReader::new)
    else {
        eprintln!(
            "failed to open input file {}",
            CStr::from_ptr(filename).to_string_lossy()
        );
        if !temp.is_null() {
            unlink(temp);
            free(temp as _);
        }
        return -1;
    };

    let mut str = vec![];
    loop {
        /*
         * read one line in string buffer.
         */
        match f.read_until(b'\n', &mut str) {
            Ok(mut len) if len > 0 => {
                /*
                 * remove the ending spaces
                 */
                str.push(0);
                while len > 0
                    && (str[len - 1] == b'\n'
                        || str[len - 1] == b'\r'
                        || str[len - 1] == b' '
                        || str[len - 1] == b'\t')
                {
                    len -= 1;
                    if len == str.len() {
                        str.push(0);
                    } else {
                        str[len] = 0;
                    }
                }
                NB_TESTS += 1;
                handle_uri(str.as_ptr() as _, base, &mut o);
            }
            _ => {
                break;
            }
        }
        str.clear();
    }

    if !result.is_null() {
        ret = compare_files(temp, result);
        if ret != 0 {
            eprintln!(
                "Result for {} failed in {}",
                CStr::from_ptr(filename).to_string_lossy(),
                CStr::from_ptr(result).to_string_lossy()
            );
            res = 1;
        }
    }
    if !err.is_null() {
        ret = compare_file_mem(err, TEST_ERRORS.as_ptr() as _, TEST_ERRORS_SIZE as _);
        if ret != 0 {
            eprintln!(
                "Error for {} failed",
                CStr::from_ptr(filename).to_string_lossy()
            );
            res = 1;
        }
    }

    if !temp.is_null() {
        unlink(temp);
        free(temp as _);
    }
    res
}

/**
 * uriParseTest:
 * @filename: the file to parse
 * @result: the file with expected result
 * @err: the file with error messages
 *
 * Parse a file containing URI and check for errors
 *
 * Returns 0 in case of success, an error code otherwise
 */
unsafe extern "C" fn uri_parse_test(
    filename: *const c_char,
    result: *const c_char,
    err: *const c_char,
    _options: c_int,
) -> c_int {
    uri_common_test(filename, result, err, null_mut())
}

/**
 * uriBaseTest:
 * @filename: the file to parse
 * @result: the file with expected result
 * @err: the file with error messages
 *
 * Parse a file containing URI, compose them against a fixed base and
 * check for errors
 *
 * Returns 0 in case of success, an error code otherwise
 */
unsafe extern "C" fn uri_base_test(
    filename: *const c_char,
    result: *const c_char,
    err: *const c_char,
    _options: c_int,
) -> c_int {
    uri_common_test(
        filename,
        result,
        err,
        c"http://foo.com/path/to/index.html?orig#help".as_ptr(),
    )
}

static mut URIP_SUCCESS: c_int = 1;
static mut URIP_CURRENT: usize = 0;
const URIP_TEST_URLS: &[&CStr] = &[
    c"urip://example.com/a b.html",
    c"urip://example.com/a%20b.html",
    c"file:///path/to/a b.html",
    c"file:///path/to/a%20b.html",
    c"/path/to/a b.html",
    c"/path/to/a%20b.html",
    c"urip://example.com/r\xe9sum\xe9.html",
    c"urip://example.com/test?a=1&b=2%263&c=4#foo",
];
const URIP_RCVS_URLS: &[&CStr] = &[
    /* it is an URI the strings must be escaped */
    c"urip://example.com/a%20b.html",
    /* check that % escaping is not broken */
    c"urip://example.com/a%20b.html",
    /* it's an URI path the strings must be escaped */
    c"file:///path/to/a%20b.html",
    /* check that % escaping is not broken */
    c"file:///path/to/a%20b.html",
    /* this is not an URI, this is a path, so this should not be escaped */
    c"/path/to/a b.html",
    /* check that paths with % are not broken */
    c"/path/to/a%20b.html",
    /* out of context the encoding can't be guessed byte by byte conversion */
    c"urip://example.com/r%E9sum%E9.html",
    /* verify we don't destroy URIs especially the query part */
    c"urip://example.com/test?a=1&b=2%263&c=4#foo",
];
const URIP_RES: &CStr = c"<list/>";
static URIP_CUR: AtomicPtr<c_char> = AtomicPtr::new(null_mut());
static mut URIP_RLEN: usize = 0;

/**
 * uripMatch:
 * @URI: an URI to test
 *
 * Check for an urip: query
 *
 * Returns 1 if yes and 0 if another Input module should be used
 */
unsafe extern "C" fn urip_match(uri: *const c_char) -> c_int {
    const CATALOG_URL: &str = concatcp!("file://", SYSCONFDIR, "/xml/catalog\0");
    if uri.is_null() || strcmp(uri, CATALOG_URL.as_ptr() as _) == 0 {
        return 0;
    }
    /* Verify we received the escaped URL */
    if strcmp(URIP_RCVS_URLS[URIP_CURRENT].as_ptr(), uri) != 0 {
        URIP_SUCCESS = 0;
    }
    1
}

/**
 * uripOpen:
 * @URI: an URI to test
 *
 * Return a pointer to the urip: query handler, in this example simply
 * the urip_current pointer...
 *
 * Returns an Input context or NULL in case or error
 */
unsafe extern "C" fn urip_open(uri: *const c_char) -> *mut c_void {
    const CATALOG_URL: &str = concatcp!("file://", SYSCONFDIR, "/xml/catalog\0");
    if uri.is_null() || strcmp(uri, CATALOG_URL.as_ptr() as _) == 0 {
        return null_mut();
    }
    /* Verify we received the escaped URL */
    if strcmp(URIP_RCVS_URLS[URIP_CURRENT].as_ptr(), uri) != 0 {
        URIP_SUCCESS = 0;
    }
    URIP_CUR.store(URIP_RES.as_ptr() as _, Ordering::Relaxed);
    URIP_RLEN = URIP_RES.to_bytes().len();
    URIP_CUR.load(Ordering::Relaxed) as _
}

/**
 * uripClose:
 * @context: the read context
 *
 * Close the urip: query handler
 *
 * Returns 0 or -1 in case of error
 */
unsafe extern "C" fn urip_close(context: *mut c_void) -> c_int {
    if context.is_null() {
        return -1;
    }
    URIP_CUR.store(null_mut(), Ordering::Relaxed);
    URIP_RLEN = 0;
    0
}

/**
 * uripRead:
 * @context: the read context
 * @buffer: where to store data
 * @len: number of bytes to read
 *
 * Implement an urip: query read.
 *
 * Returns the number of bytes read or -1 in case of error
 */
unsafe extern "C" fn urip_read(context: *mut c_void, buffer: *mut c_char, mut len: c_int) -> c_int {
    let ptr: *const c_char = context as *const c_char;

    if context.is_null() || buffer.is_null() || len < 0 {
        return -1;
    }

    if len > URIP_RLEN as _ {
        len = URIP_RLEN as _;
    }
    memcpy(buffer as _, ptr as _, len as usize);
    URIP_RLEN -= len as usize;
    len
}

unsafe extern "C" fn urip_check_url(url: *const c_char) -> c_int {
    let doc: XmlDocPtr = xml_read_file(url, null_mut(), 0);
    if doc.is_null() {
        return -1;
    }
    xml_free_doc(doc);
    1
}

/**
 * uriPathTest:
 * @filename: ignored
 * @result: ignored
 * @err: ignored
 *
 * Run a set of tests to check how Path and URI are handled before
 * being passed to the I/O layer
 *
 * Returns 0 in case of success, an error code otherwise
 */
unsafe extern "C" fn uri_path_test(
    _filename: *const c_char,
    _result: *const c_char,
    _err: *const c_char,
    _options: c_int,
) -> c_int {
    let mut parsed: c_int;
    let mut failures: c_int = 0;

    /*
     * register the new I/O handlers
     */
    if xmlRegisterInputCallbacks(
        Some(urip_match),
        Some(urip_open),
        Some(urip_read),
        Some(urip_close),
    ) < 0
    {
        eprintln!("failed to register HTTP handler");
        return -1;
    }

    URIP_CURRENT = 0;
    while URIP_CURRENT < URIP_TEST_URLS.len() {
        URIP_SUCCESS = 1;
        parsed = urip_check_url(URIP_TEST_URLS[URIP_CURRENT].as_ptr());
        if URIP_SUCCESS != 1 {
            eprint!(
                "failed the URL passing test for {}",
                URIP_TEST_URLS[URIP_CURRENT].to_string_lossy(),
            );
            failures += 1;
        } else if parsed != 1 {
            eprint!(
                "failed the parsing test for {}",
                URIP_TEST_URLS[URIP_CURRENT].to_string_lossy(),
            );
            failures += 1;
        }
        NB_TESTS += 1;
        URIP_CURRENT += 1;
    }

    xmlPopInputCallbacks();
    failures
}

/************************************************************************
 *                                    *
 *            Schemas tests                    *
 *                                    *
 ************************************************************************/
#[cfg(feature = "schema")]
unsafe extern "C" fn schemas_one_test(
    sch: *const c_char,
    filename: *const c_char,
    result: *const c_char,
    options: c_int,
    schemas: XmlSchemaPtr,
) -> c_int {
    use exml::libxml::xmlschemas::{
        xml_schema_free_valid_ctxt, xml_schema_new_valid_ctxt, xml_schema_set_valid_errors,
        xml_schema_validate_doc,
    };

    let mut ret: c_int = 0;

    let doc: XmlDocPtr = xml_read_file(filename, null_mut(), options);
    if doc.is_null() {
        eprintln!(
            "failed to parse instance {} for {}",
            CStr::from_ptr(filename).to_string_lossy(),
            CStr::from_ptr(sch).to_string_lossy(),
        );
        return -1;
    }

    let temp: *mut c_char = result_filename(
        result,
        TEMP_DIRECTORY
            .get()
            .map(|t| t.as_ptr())
            .unwrap_or(null_mut()),
        c".res".as_ptr(),
    );
    if temp.is_null() {
        eprintln!("Out of memory");
        fatal_error();
    }
    let Ok(mut schemas_output) = File::options()
        .write(true)
        .truncate(true)
        .create(true)
        .open(CStr::from_ptr(temp).to_string_lossy().as_ref())
    else {
        eprintln!(
            "failed to open output file {}",
            CStr::from_ptr(temp).to_string_lossy()
        );
        xml_free_doc(doc);
        free(temp as _);
        return -1;
    };

    let ctxt: XmlSchemaValidCtxtPtr = xml_schema_new_valid_ctxt(schemas);
    xml_schema_set_valid_errors(
        ctxt,
        Some(test_error_handler),
        Some(test_error_handler),
        ctxt as _,
    );
    let valid_result: c_int = xml_schema_validate_doc(ctxt, doc);
    match valid_result.cmp(&0) {
        std::cmp::Ordering::Equal => {
            writeln!(
                schemas_output,
                "{} validates",
                CStr::from_ptr(filename).to_string_lossy()
            )
            .ok();
        }
        std::cmp::Ordering::Greater => {
            writeln!(
                schemas_output,
                "{} fails to validate",
                CStr::from_ptr(filename).to_string_lossy()
            )
            .ok();
        }
        std::cmp::Ordering::Less => {
            writeln!(
                schemas_output,
                "{} validation generated an internal error",
                CStr::from_ptr(filename).to_string_lossy(),
            )
            .ok();
        }
    }
    if !result.is_null() && compare_files(temp, result) != 0 {
        eprintln!(
            "Result for {} on {} failed",
            CStr::from_ptr(filename).to_string_lossy(),
            CStr::from_ptr(sch).to_string_lossy()
        );
        ret = 1;
    }
    if !temp.is_null() {
        unlink(temp);
        free(temp as _);
    }

    xml_schema_free_valid_ctxt(ctxt);
    xml_free_doc(doc);
    ret
}
/**
 * schemasTest:
 * @filename: the schemas file
 * @result: the file with expected result
 * @err: the file with error messages
 *
 * Parse a file containing URI, compose them against a fixed base and
 * check for errors
 *
 * Returns 0 in case of success, an error code otherwise
 */
#[cfg(feature = "schema")]
unsafe extern "C" fn schemas_test(
    filename: *const c_char,
    _resul: *const c_char,
    _errr: *const c_char,
    options: c_int,
) -> c_int {
    use std::mem::zeroed;

    use exml::libxml::xmlschemas::{
        xml_schema_free, xml_schema_free_parser_ctxt, xml_schema_new_parser_ctxt, xml_schema_parse,
        xml_schema_set_parser_errors, XmlSchemaParserCtxtPtr,
    };
    use libc::{glob, glob_t, globfree, GLOB_DOOFFS};

    let base: *const c_char = base_filename(filename);
    let mut base2: *const c_char;
    let mut instance: *const c_char;
    let mut res: c_int = 0;
    let mut len: usize;
    let mut ret: c_int;
    let mut pattern: [c_char; 500] = [0; 500];
    let mut prefix: [c_char; 500] = [0; 500];
    let mut result: [c_char; 500] = [0; 500];
    let mut err: [c_char; 500] = [0; 500];
    let mut globbuf: glob_t = unsafe { zeroed() };
    let mut count: c_char;

    /* first compile the schemas if possible */
    let ctxt: XmlSchemaParserCtxtPtr = xml_schema_new_parser_ctxt(filename);
    xml_schema_set_parser_errors(
        ctxt,
        Some(test_error_handler),
        Some(test_error_handler),
        ctxt as _,
    );
    let schemas: XmlSchemaPtr = xml_schema_parse(ctxt);
    xml_schema_free_parser_ctxt(ctxt);
    let parse_errors_size: usize = TEST_ERRORS_SIZE;

    /*
     * most of the mess is about the output filenames generated by the Makefile
     */
    len = strlen(base);
    if !(5..=499).contains(&len) {
        xml_schema_free(schemas);
        return -1;
    }
    len -= 4; /* remove trailing .xsd */
    if *base.add(len - 2) == b'_' as _ {
        len -= 2; /* remove subtest number */
    }
    if *base.add(len - 2) == b'_' as _ {
        len -= 2; /* remove subtest number */
    }
    memcpy(prefix.as_mut_ptr() as _, base as _, len);
    prefix[len] = 0;

    if snprintf(
        pattern.as_mut_ptr() as _,
        499,
        c"./test/schemas/%s_*.xml".as_ptr(),
        prefix.as_ptr(),
    ) >= 499
    {
        pattern[499] = 0;
    }

    if *base.add(len) == b'_' as _ {
        len += 2;
        memcpy(prefix.as_mut_ptr() as _, base as _, len);
        prefix[len] = 0;
    }

    globbuf.gl_offs = 0;
    glob(pattern.as_ptr(), GLOB_DOOFFS, None, addr_of_mut!(globbuf));
    for i in 0..globbuf.gl_pathc {
        TEST_ERRORS_SIZE = parse_errors_size;
        TEST_ERRORS[parse_errors_size] = 0;
        instance = *globbuf.gl_pathv.add(i);
        base2 = base_filename(instance);
        len = strlen(base2);
        if len > 6 && *base2.add(len - 6) == b'_' as _ {
            count = *base2.add(len - 5);
            ret = snprintf(
                result.as_mut_ptr(),
                499,
                c"./result/schemas/%s_%c".as_ptr(),
                prefix.as_ptr(),
                count as i32,
            );
            if ret >= 499 {
                result[499] = 0;
            }
            ret = snprintf(
                err.as_mut_ptr(),
                499,
                c"./result/schemas/%s_%c.err".as_ptr(),
                prefix.as_ptr(),
                count as i32,
            );
            if ret >= 499 {
                err[499] = 0;
            }
        } else {
            eprintln!(
                "don't know how to process {}",
                CStr::from_ptr(instance).to_string_lossy()
            );
            continue;
        }
        if !schemas.is_null() {
            NB_TESTS += 1;
            ret = schemas_one_test(filename, instance, result.as_ptr(), options, schemas);
            if ret != 0 {
                res = ret;
            }
        }
        if compare_file_mem(
            err.as_ptr(),
            TEST_ERRORS.as_ptr() as _,
            TEST_ERRORS_SIZE as _,
        ) != 0
        {
            eprintln!(
                "Error for {} on {} failed",
                CStr::from_ptr(instance).to_string_lossy(),
                CStr::from_ptr(filename).to_string_lossy()
            );
            res = 1;
        }
    }
    globfree(addr_of_mut!(globbuf));
    xml_schema_free(schemas);

    res
}

/************************************************************************
 *                                    *
 *            Schemas tests                    *
 *                                    *
 ************************************************************************/
#[cfg(feature = "schema")]
unsafe extern "C" fn rng_one_test(
    sch: *const c_char,
    filename: *const c_char,
    result: *const c_char,
    options: c_int,
    schemas: XmlRelaxNGPtr,
) -> c_int {
    use exml::libxml::relaxng::{
        xml_relaxng_free_valid_ctxt, xml_relaxng_new_valid_ctxt, xml_relaxng_set_valid_errors,
        xml_relaxng_validate_doc, XmlRelaxNGValidCtxtPtr,
    };

    let mut ret: c_int;

    let doc: XmlDocPtr = xml_read_file(filename, null_mut(), options);
    if doc.is_null() {
        eprintln!(
            "failed to parse instance {} for {}",
            CStr::from_ptr(filename).to_string_lossy(),
            CStr::from_ptr(sch).to_string_lossy(),
        );
        return -1;
    }

    let temp: *mut c_char = result_filename(
        result,
        TEMP_DIRECTORY
            .get()
            .map(|t| t.as_ptr())
            .unwrap_or(null_mut()),
        c".res".as_ptr(),
    );
    if temp.is_null() {
        eprintln!("Out of memory");
        fatal_error();
    }
    let Ok(_) = File::options()
        .write(true)
        .truncate(true)
        .create(true)
        .open(CStr::from_ptr(temp).to_string_lossy().as_ref())
    else {
        eprintln!(
            "failed to open output file {}",
            CStr::from_ptr(temp).to_string_lossy()
        );
        xml_free_doc(doc);
        free(temp as _);
        return -1;
    };

    let ctxt: XmlRelaxNGValidCtxtPtr = xml_relaxng_new_valid_ctxt(schemas);
    xml_relaxng_set_valid_errors(
        ctxt,
        Some(test_error_handler),
        Some(test_error_handler),
        ctxt as _,
    );
    ret = xml_relaxng_validate_doc(ctxt, doc);
    match ret.cmp(&0) {
        std::cmp::Ordering::Equal => xml_error_with_format!(
            test_error_handler,
            null_mut(),
            c"%s validates\n".as_ptr(),
            filename
        ),
        std::cmp::Ordering::Greater => xml_error_with_format!(
            test_error_handler,
            null_mut(),
            c"%s fails to validate\n".as_ptr(),
            filename
        ),
        std::cmp::Ordering::Less => xml_error_with_format!(
            test_error_handler,
            null_mut(),
            c"%s validation generated an internal error\n".as_ptr(),
            filename
        ),
    }

    ret = 0;
    if !result.is_null() && compare_files(temp, result) != 0 {
        eprintln!(
            "Result for {} on {} failed",
            CStr::from_ptr(filename).to_string_lossy(),
            CStr::from_ptr(sch).to_string_lossy()
        );
        ret = 1;
    }
    if !temp.is_null() {
        unlink(temp);
        free(temp as _);
    }

    xml_relaxng_free_valid_ctxt(ctxt);
    xml_free_doc(doc);
    ret
}
/**
 * rngTest:
 * @filename: the schemas file
 * @result: the file with expected result
 * @err: the file with error messages
 *
 * Parse an RNG schemas and then apply it to the related .xml
 *
 * Returns 0 in case of success, an error code otherwise
 */
#[cfg(feature = "schema")]
unsafe extern "C" fn rng_test(
    filename: *const c_char,
    _resul: *const c_char,
    _errr: *const c_char,
    options: c_int,
) -> c_int {
    use std::mem::zeroed;

    use exml::libxml::relaxng::{
        xml_relaxng_new_parser_ctxt, xml_relaxng_free, xml_relaxng_free_parser_ctxt,
        xml_relaxng_parse, xml_relaxng_set_parser_errors, XmlRelaxNGParserCtxtPtr,
    };
    use libc::{glob, glob_t, globfree, GLOB_DOOFFS};

    let base: *const c_char = base_filename(filename);
    let mut base2: *const c_char;
    let mut instance: *const c_char;
    let mut res: c_int;
    let mut len: usize;
    let mut ret: c_int = 0;
    let mut pattern: [c_char; 500] = [0; 500];
    let mut prefix: [c_char; 500] = [0; 500];
    let mut result: [c_char; 500] = [0; 500];
    let mut err: [c_char; 500] = [0; 500];
    let mut globbuf: glob_t = unsafe { zeroed() };
    let mut count: c_char;

    /* first compile the schemas if possible */
    let ctxt: XmlRelaxNGParserCtxtPtr = xml_relaxng_new_parser_ctxt(filename);
    xml_relaxng_set_parser_errors(
        ctxt,
        Some(test_error_handler),
        Some(test_error_handler),
        ctxt as _,
    );
    let schemas: XmlRelaxNGPtr = xml_relaxng_parse(ctxt);
    xml_relaxng_free_parser_ctxt(ctxt);
    if schemas.is_null() {
        xml_error_with_format!(
            test_error_handler,
            null_mut(),
            c"Relax-NG schema %s failed to compile\n".as_ptr(),
            filename
        );
    }
    let parse_errors_size: usize = TEST_ERRORS_SIZE;

    /*
     * most of the mess is about the output filenames generated by the Makefile
     */
    len = strlen(base);
    if !(5..=499).contains(&len) {
        xml_relaxng_free(schemas);
        return -1;
    }
    len -= 4; /* remove trailing .rng */
    memcpy(prefix.as_mut_ptr() as _, base as _, len);
    prefix[len] = 0;

    if snprintf(
        pattern.as_mut_ptr(),
        499,
        c"./test/relaxng/%s_?.xml".as_ptr(),
        prefix.as_ptr(),
    ) >= 499
    {
        pattern[499] = 0;
    }

    globbuf.gl_offs = 0;
    glob(pattern.as_ptr(), GLOB_DOOFFS, None, addr_of_mut!(globbuf));
    for i in 0..globbuf.gl_pathc {
        TEST_ERRORS_SIZE = parse_errors_size;
        TEST_ERRORS[parse_errors_size] = 0;
        instance = *globbuf.gl_pathv.add(i);
        base2 = base_filename(instance);
        len = strlen(base2);
        if len > 6 && *base2.add(len - 6) == b'_' as i8 {
            count = *base2.add(len - 5);
            res = snprintf(
                result.as_mut_ptr(),
                499,
                c"./result/relaxng/%s_%c".as_ptr(),
                prefix.as_ptr(),
                count as i32,
            );
            if res >= 499 {
                result[499] = 0;
            }
            res = snprintf(
                err.as_mut_ptr(),
                499,
                c"./result/relaxng/%s_%c.err".as_ptr(),
                prefix.as_ptr(),
                count as i32,
            );
            if res >= 499 {
                err[499] = 0;
            }
        } else {
            eprintln!(
                "don't know how to process {}",
                CStr::from_ptr(instance).to_string_lossy()
            );
            continue;
        }
        if !schemas.is_null() {
            NB_TESTS += 1;
            res = rng_one_test(filename, instance, result.as_ptr(), options, schemas);
            if res != 0 {
                ret = res;
            }
        }
        if compare_file_mem(
            err.as_ptr(),
            TEST_ERRORS.as_ptr() as _,
            TEST_ERRORS_SIZE as _,
        ) != 0
        {
            eprintln!(
                "Error for {} on {} failed",
                CStr::from_ptr(instance).to_string_lossy(),
                CStr::from_ptr(filename).to_string_lossy()
            );
            // res = 1;
        }
    }
    globfree(addr_of_mut!(globbuf));
    xml_relaxng_free(schemas);

    ret
}

/**
 * rngStreamTest:
 * @filename: the schemas file
 * @result: the file with expected result
 * @err: the file with error messages
 *
 * Parse a set of files with streaming, applying an RNG schemas
 *
 * Returns 0 in case of success, an error code otherwise
 */
#[cfg(all(feature = "schema", feature = "libxml_reader"))]
unsafe extern "C" fn rng_stream_test(
    filename: *const c_char,
    _resul: *const c_char,
    _errr: *const c_char,
    options: c_int,
) -> c_int {
    use std::mem::zeroed;

    use exml::libxml::xmlreader::{xml_free_text_reader, xml_reader_for_file};
    use libc::{glob, glob_t, globfree, GLOB_DOOFFS};

    let base: *const c_char = base_filename(filename);
    let mut base2: *const c_char;
    let mut instance: *const c_char;
    let mut res: c_int = 0;
    let mut len: usize;
    let mut ret: c_int;
    let mut pattern: [c_char; 500] = [0; 500];
    let mut prefix: [c_char; 500] = [0; 500];
    let mut result: [c_char; 500] = [0; 500];
    let mut err: [c_char; 500] = [0; 500];
    let mut globbuf: glob_t = unsafe { zeroed() };
    let mut count: c_char;
    let mut reader: XmlTextReaderPtr;
    let mut disable_err: c_int = 0;

    /*
     * most of the mess is about the output filenames generated by the Makefile
     */
    len = strlen(base);
    if !(5..=499).contains(&len) {
        eprintln!("len(base) == {} !", len);
        return -1;
    }
    len -= 4; /* remove trailing .rng */
    memcpy(prefix.as_mut_ptr() as _, base as _, len);
    prefix[len] = 0;

    /*
     * strictly unifying the error messages is nearly impossible this
     * hack is also done in the Makefile
     */
    if strcmp(prefix.as_ptr(), c"tutor10_1".as_ptr()) == 0
        || strcmp(prefix.as_ptr(), c"tutor10_2".as_ptr()) == 0
        || strcmp(prefix.as_ptr(), c"tutor3_2".as_ptr()) == 0
        || strcmp(prefix.as_ptr(), c"307377".as_ptr()) == 0
        || strcmp(prefix.as_ptr(), c"tutor8_2".as_ptr()) == 0
    {
        disable_err = 1;
    }

    if snprintf(
        pattern.as_mut_ptr(),
        499,
        c"./test/relaxng/%s_?.xml".as_ptr(),
        prefix.as_ptr(),
    ) >= 499
    {
        pattern[499] = 0;
    }

    globbuf.gl_offs = 0;
    glob(pattern.as_ptr(), GLOB_DOOFFS, None, addr_of_mut!(globbuf));
    for i in 0..globbuf.gl_pathc {
        TEST_ERRORS_SIZE = 0;
        TEST_ERRORS[0] = 0;
        instance = *globbuf.gl_pathv.add(i);
        base2 = base_filename(instance);
        len = strlen(base2);
        if len > 6 && *base2.add(len - 6) == b'_' as i8 {
            count = *base2.add(len - 5);
            ret = snprintf(
                result.as_mut_ptr(),
                499,
                c"./result/relaxng/%s_%c".as_ptr(),
                prefix.as_ptr(),
                count as i32,
            );
            if ret >= 499 {
                result[499] = 0;
            }
            ret = snprintf(
                err.as_mut_ptr(),
                499,
                c"./result/relaxng/%s_%c.err".as_ptr(),
                prefix.as_ptr(),
                count as i32,
            );
            if ret >= 499 {
                err[499] = 0;
            }
        } else {
            eprintln!(
                "don't know how to process {}",
                CStr::from_ptr(instance).to_string_lossy()
            );
            continue;
        }
        reader = xml_reader_for_file(instance, null_mut(), options);
        if reader.is_null() {
            eprintln!(
                "Failed to build reader for {}",
                CStr::from_ptr(instance).to_string_lossy()
            );
        }
        if disable_err == 1 {
            ret = stream_process_test(
                instance,
                result.as_ptr(),
                null_mut(),
                reader,
                filename,
                options,
            );
        } else {
            ret = stream_process_test(
                instance,
                result.as_ptr(),
                err.as_ptr(),
                reader,
                filename,
                options,
            );
        }
        xml_free_text_reader(reader);
        if ret != 0 {
            eprintln!(
                "instance {} failed",
                CStr::from_ptr(instance).to_string_lossy()
            );
            res = ret;
        }
    }
    globfree(addr_of_mut!(globbuf));

    res
}

/************************************************************************
 *                                    *
 *            Patterns tests                    *
 *                                    *
 ************************************************************************/
#[cfg(all(feature = "libxml_pattern", feature = "libxml_reader"))]
unsafe extern "C" fn pattern_node(
    out: &mut File,
    reader: XmlTextReaderPtr,
    pattern: *const c_char,
    patternc: XmlPatternPtr,
    mut patstream: XmlStreamCtxtPtr,
) {
    use exml::libxml::{
        pattern::{xml_free_stream_ctxt, xml_pattern_match, xml_stream_pop, xml_stream_push},
        tree::xml_get_node_path,
        xmlreader::{
            xml_text_reader_const_local_name, xml_text_reader_const_namespace_uri,
            xml_text_reader_current_node, xml_text_reader_is_empty_element,
            xml_text_reader_node_type, XmlReaderTypes,
        },
    };

    let mut path: *mut XmlChar = null_mut();
    let mut is_match: c_int = -1;

    let typ: c_int = xml_text_reader_node_type(reader);
    let empty: c_int = xml_text_reader_is_empty_element(reader);

    if typ == XmlReaderTypes::XmlReaderTypeElement as i32 {
        /* do the check only on element start */
        is_match = xml_pattern_match(patternc, xml_text_reader_current_node(reader));

        if is_match != 0 {
            path = xml_get_node_path(xml_text_reader_current_node(reader));
            writeln!(
                out,
                "Node {} matches pattern {}",
                CStr::from_ptr(path as _).to_string_lossy(),
                CStr::from_ptr(pattern).to_string_lossy()
            )
            .ok();
        }
    }
    if !patstream.is_null() {
        let mut ret: c_int;

        if typ == XmlReaderTypes::XmlReaderTypeElement as i32 {
            ret = xml_stream_push(
                patstream,
                xml_text_reader_const_local_name(reader),
                xml_text_reader_const_namespace_uri(reader),
            );
            if ret < 0 {
                writeln!(out, "xmlStreamPush() failure").ok();
                xml_free_stream_ctxt(patstream);
                patstream = null_mut();
            } else if ret != is_match {
                if path.is_null() {
                    path = xml_get_node_path(xml_text_reader_current_node(reader));
                }
                writeln!(out, "xmlPatternMatch and xmlStreamPush disagree").ok();
                writeln!(
                    out,
                    "  pattern {} node {}",
                    CStr::from_ptr(pattern).to_string_lossy(),
                    CStr::from_ptr(path as _).to_string_lossy()
                )
                .ok();
            }
        }
        if typ == XmlReaderTypes::XmlReaderTypeEndElement as i32
            || (typ == XmlReaderTypes::XmlReaderTypeElement as i32 && empty != 0)
        {
            ret = xml_stream_pop(patstream);
            if ret < 0 {
                writeln!(out, "xmlStreamPop() failure").ok();
                xml_free_stream_ctxt(patstream);
                // patstream = null_mut();
            }
        }
    }
    if !path.is_null() {
        xml_free(path as _);
    }
}

/**
 * patternTest:
 * @filename: the schemas file
 * @result: the file with expected result
 * @err: the file with error messages
 *
 * Parse a set of files with streaming, applying an RNG schemas
 *
 * Returns 0 in case of success, an error code otherwise
 */
#[cfg(all(feature = "libxml_pattern", feature = "libxml_reader"))]
unsafe extern "C" fn pattern_test(
    filename: *const c_char,
    _resul: *const c_char,
    _err: *const c_char,
    options: c_int,
) -> c_int {
    use std::sync::atomic::Ordering;

    use exml::libxml::{
        pattern::{
            xml_free_pattern, xml_free_stream_ctxt, xml_pattern_get_stream_ctxt,
            xml_patterncompile, xml_stream_push,
        },
        tree::{xml_doc_get_root_element, XmlNsPtr},
        xmlreader::{xml_free_text_reader, xml_reader_walker, xml_text_reader_read},
    };

    let mut patternc: XmlPatternPtr;
    let mut patstream: XmlStreamCtxtPtr;
    let mut xml: [c_char; 500] = [0; 500];
    let mut result: [c_char; 500] = [0; 500];
    let mut len: usize;
    let mut ret: c_int;
    let mut res: c_int;

    let mut reader: XmlTextReaderPtr;
    let mut doc: XmlDocPtr;

    len = strlen(filename);
    len -= 4;
    memcpy(xml.as_mut_ptr() as _, filename as _, len);
    xml[len] = 0;
    if snprintf(
        result.as_mut_ptr(),
        499,
        c"./result/pattern/%s".as_ptr(),
        base_filename(xml.as_ptr()),
    ) >= 499
    {
        result[499] = 0;
    }
    memcpy(xml.as_mut_ptr().add(len) as _, c".xml".as_ptr() as _, 5);

    if check_test_file(xml.as_ptr()) == 0 && UPDATE_RESULTS == 0 {
        eprintln!(
            "Missing xml file {}",
            CStr::from_ptr(xml.as_ptr()).to_string_lossy()
        );
        return -1;
    }
    let Ok(mut f) =
        File::open(CStr::from_ptr(filename).to_string_lossy().as_ref()).map(BufReader::new)
    else {
        eprintln!(
            "Failed to open {}\n",
            CStr::from_ptr(filename).to_string_lossy()
        );
        return -1;
    };
    let temp: *mut c_char = result_filename(
        filename,
        TEMP_DIRECTORY
            .get()
            .map(|t| t.as_ptr())
            .unwrap_or(null_mut()),
        c".res".as_ptr(),
    );
    if temp.is_null() {
        eprintln!("Out of memory");
        fatal_error();
    }
    let Ok(mut o) = File::options()
        .write(true)
        .truncate(true)
        .create(true)
        .open(CStr::from_ptr(temp).to_string_lossy().as_ref())
    else {
        eprintln!(
            "failed to open output file {}",
            CStr::from_ptr(temp).to_string_lossy()
        );
        free(temp as _);
        return -1;
    };
    let mut str = vec![];
    loop {
        /*
         * read one line in string buffer.
         */
        match f.read_until(b'\n', &mut str) {
            Ok(mut size) if size > 0 => {
                /*
                 * remove the ending spaces
                 */
                while size > 0
                    && (str[size - 1] == b'\n'
                        || str[size - 1] == b'\r'
                        || str[size - 1] == b' '
                        || str[size - 1] == b'\t')
                {
                    size -= 1;
                    str[size] = 0;
                }
                doc = xml_read_file(xml.as_ptr(), null_mut(), options);
                if doc.is_null() {
                    eprintln!(
                        "Failed to parse {}",
                        CStr::from_ptr(xml.as_ptr()).to_string_lossy()
                    );
                    // ret = 1;
                } else {
                    let mut namespaces: [*const XmlChar; 22] = [null(); 22];
                    let mut ns: XmlNsPtr;

                    let root: XmlNodePtr = xml_doc_get_root_element(doc);
                    ns = (*root).ns_def;
                    let mut j = 0;
                    while j < 20 && !ns.is_null() {
                        namespaces[j] = (*ns).href.load(Ordering::Relaxed);
                        j += 1;
                        namespaces[j] = (*ns).prefix.load(Ordering::Relaxed);
                        j += 1;
                        ns = (*ns).next.load(Ordering::Relaxed);
                    }
                    namespaces[j] = null();
                    j += 1;
                    namespaces[j] = null();

                    patternc =
                        xml_patterncompile(str.as_ptr(), (*doc).dict, 0, namespaces.as_mut_ptr());
                    if patternc.is_null() {
                        xml_error_with_format!(
                            test_error_handler,
                            null_mut(),
                            c"Pattern %s failed to compile\n".as_ptr(),
                            str.as_ptr()
                        );
                        xml_free_doc(doc);
                        // ret = 1;
                        continue;
                    }
                    patstream = xml_pattern_get_stream_ctxt(patternc);
                    if !patstream.is_null() {
                        ret = xml_stream_push(patstream, null_mut(), null_mut());
                        if ret < 0 {
                            eprintln!("xmlStreamPush() failure");
                            xml_free_stream_ctxt(patstream);
                            patstream = null_mut();
                        }
                    }
                    NB_TESTS += 1;

                    reader = xml_reader_walker(doc);
                    res = xml_text_reader_read(reader);
                    while res == 1 {
                        pattern_node(&mut o, reader, str.as_ptr() as _, patternc, patstream);
                        res = xml_text_reader_read(reader);
                    }
                    if res != 0 {
                        writeln!(
                            o,
                            "{} : failed to parse",
                            CStr::from_ptr(filename).to_string_lossy()
                        )
                        .ok();
                    }
                    xml_free_text_reader(reader);
                    xml_free_doc(doc);
                    xml_free_stream_ctxt(patstream);
                    // patstream = null_mut();
                    xml_free_pattern(patternc);
                }
            }
            _ => {
                break;
            }
        }
        str.clear();
    }

    ret = compare_files(temp, result.as_ptr());
    if ret != 0 {
        eprintln!(
            "Result for {} failed in {}",
            CStr::from_ptr(filename).to_string_lossy(),
            CStr::from_ptr(result.as_ptr()).to_string_lossy()
        );
        ret = 1;
    }
    if !temp.is_null() {
        unlink(temp);
        free(temp as _);
    }
    ret
}

/************************************************************************
 *                                    *
 *            Canonicalization tests                *
 *                                    *
 ************************************************************************/
#[cfg(feature = "libxml_c14n")]
unsafe extern "C" fn load_xpath_expr(
    parent_doc: XmlDocPtr,
    filename: *const c_char,
) -> XmlXPathObjectPtr {
    use std::sync::atomic::Ordering;

    use exml::libxml::{
        globals::xml_load_ext_dtd_default_value,
        parser::{
            xml_substitute_entities_default, XmlParserOption, XML_COMPLETE_ATTRS, XML_DETECT_IDS,
        },
        tree::{xml_doc_get_root_element, xml_node_get_content, XmlNsPtr},
        xmlstring::xml_str_equal,
        xpath::{
            xml_xpath_eval_expression, xml_xpath_free_context, xml_xpath_new_context,
            XmlXPathContextPtr,
        },
        xpath_internals::xml_xpath_register_ns,
    };

    let mut node: XmlNodePtr;
    let mut ns: XmlNsPtr;

    /*
     * load XPath expr as a file
     */
    *xml_load_ext_dtd_default_value() = XML_DETECT_IDS as i32 | XML_COMPLETE_ATTRS as i32;
    xml_substitute_entities_default(1);

    let doc: XmlDocPtr = xml_read_file(
        filename,
        null_mut(),
        XmlParserOption::XmlParseDtdattr as i32 | XmlParserOption::XmlParseNoent as i32,
    );
    if doc.is_null() {
        eprintln!(
            "Error: unable to parse file \"{}\"",
            CStr::from_ptr(filename).to_string_lossy()
        );
        return null_mut();
    }

    /*
     * Check the document is of the right kind
     */
    if xml_doc_get_root_element(doc).is_null() {
        eprintln!(
            "Error: empty document for file \"{}\"",
            CStr::from_ptr(filename).to_string_lossy()
        );
        xml_free_doc(doc);
        return null_mut();
    }

    node = (*doc).children;
    while !node.is_null() && xml_str_equal((*node).name, c"XPath".as_ptr() as *const XmlChar) == 0 {
        node = (*node).next;
    }

    if node.is_null() {
        eprintln!(
            "Error: XPath element expected in the file  \"{}\"",
            CStr::from_ptr(filename).to_string_lossy(),
        );
        xml_free_doc(doc);
        return null_mut();
    }

    let expr: *mut XmlChar = xml_node_get_content(node);
    if expr.is_null() {
        eprintln!(
            "Error: XPath content element is NULL \"{}\"",
            CStr::from_ptr(filename).to_string_lossy(),
        );
        xml_free_doc(doc);
        return null_mut();
    }

    let ctx: XmlXPathContextPtr = xml_xpath_new_context(parent_doc);
    if ctx.is_null() {
        eprintln!("Error: unable to create new context");
        xml_free(expr as _);
        xml_free_doc(doc);
        return null_mut();
    }

    /*
     * Register namespaces
     */
    ns = (*node).ns_def;
    while !ns.is_null() {
        if xml_xpath_register_ns(
            ctx,
            (*ns).prefix.load(Ordering::Relaxed),
            (*ns).href.load(Ordering::Relaxed),
        ) != 0
        {
            eprintln!(
                "Error: unable to register NS with prefix=\"{}\" and href=\"{}\"",
                CStr::from_ptr((*ns).prefix.load(Ordering::Relaxed) as _).to_string_lossy(),
                CStr::from_ptr((*ns).href.load(Ordering::Relaxed) as _).to_string_lossy(),
            );
            xml_free(expr as _);
            xml_xpath_free_context(ctx);
            xml_free_doc(doc);
            return null_mut();
        }
        ns = (*ns).next.load(Ordering::Relaxed);
    }

    /*
     * Evaluate xpath
     */
    let xpath: XmlXPathObjectPtr = xml_xpath_eval_expression(expr, ctx);
    if xpath.is_null() {
        eprintln!("Error: unable to evaluate xpath expression");
        xml_free(expr as _);
        xml_xpath_free_context(ctx);
        xml_free_doc(doc);
        return null_mut();
    }

    /* print_xpath_nodes((*xpath).nodesetval); */

    xml_free(expr as _);
    xml_xpath_free_context(ctx);
    xml_free_doc(doc);
    xpath
}

/*
 * Macro used to grow the current buffer.
 */
#[cfg(feature = "libxml_c14n")]
macro_rules! xxx_growBufferReentrant {
    ($buffer_size:expr, $buffer:expr) => {
        $buffer_size *= 2;
        $buffer = exml::libxml::globals::xml_realloc(
            $buffer as _,
            $buffer_size * size_of::<*mut XmlChar>(),
        ) as *mut *mut XmlChar;
        if $buffer.is_null() {
            perror(c"realloc failed".as_ptr());
            return null_mut();
        }
    };
}

#[cfg(feature = "libxml_c14n")]
unsafe extern "C" fn parse_list(mut str: *mut XmlChar) -> *mut *mut XmlChar {
    use exml::libxml::globals::xml_malloc;
    use libc::perror;

    let mut buffer: *mut *mut XmlChar;
    let mut out: *mut *mut XmlChar;
    let mut buffer_size: usize;

    if str.is_null() {
        return null_mut();
    }

    let len: usize = xml_strlen(str) as _;
    if *str.add(0) == b'\'' && *str.add(len - 1) == b'\'' {
        *str.add(len - 1) = b'\0';
        str = str.add(1);
    }
    /*
     * allocate an translation buffer.
     */
    buffer_size = 1000;
    buffer = xml_malloc(buffer_size * size_of::<*mut XmlChar>()) as *mut *mut XmlChar;
    if buffer.is_null() {
        perror(c"malloc failed".as_ptr());
        return null_mut();
    }
    out = buffer;

    while *str != b'\0' {
        if out.offset_from(buffer) as usize > buffer_size - 10 {
            let indx = out.offset_from(buffer) as usize;

            xxx_growBufferReentrant!(buffer_size, buffer);
            out = buffer.add(indx);
        }
        *out = str;
        out = out.add(1);
        while *str != b',' && *str != b'\0' {
            str = str.add(1);
        }
        if *str == b',' {
            *str = b'\0';
            str = str.add(1);
        }
    }
    (*out) = null_mut();
    buffer
}

#[cfg(feature = "libxml_c14n")]
unsafe extern "C" fn c14n_run_test(
    xml_filename: *const c_char,
    with_comments: c_int,
    mode: c_int,
    xpath_filename: *const c_char,
    ns_filename: *const c_char,
    result_file: *const c_char,
) -> c_int {
    use exml::libxml::{
        c14n::xml_c14n_doc_dump_memory,
        globals::xml_load_ext_dtd_default_value,
        parser::{
            xml_substitute_entities_default, XmlParserOption, XML_COMPLETE_ATTRS, XML_DETECT_IDS,
        },
        tree::xml_doc_get_root_element,
        xpath::xml_xpath_free_object,
    };

    let mut xpath: XmlXPathObjectPtr = null_mut();
    let mut result: *mut XmlChar = null_mut();
    let mut ret: c_int;
    let mut inclusive_namespaces: *mut *mut XmlChar = null_mut();
    let mut nslist: *const c_char = null_mut();
    let mut nssize: c_int = 0;

    /*
     * build an XML tree from a the file; we need to add default
     * attributes and resolve all character and entities references
     */
    *xml_load_ext_dtd_default_value() = XML_DETECT_IDS as i32 | XML_COMPLETE_ATTRS as i32;
    xml_substitute_entities_default(1);

    let doc: XmlDocPtr = xml_read_file(
        xml_filename,
        null_mut(),
        XmlParserOption::XmlParseDtdattr as i32 | XmlParserOption::XmlParseNoent as i32,
    );
    if doc.is_null() {
        eprintln!(
            "Error: unable to parse file \"{}\"",
            CStr::from_ptr(xml_filename).to_string_lossy()
        );
        return -1;
    }

    /*
     * Check the document is of the right kind
     */
    if xml_doc_get_root_element(doc).is_null() {
        eprintln!(
            "Error: empty document for file \"{}\"",
            CStr::from_ptr(xml_filename).to_string_lossy(),
        );
        xml_free_doc(doc);
        return -1;
    }

    /*
     * load xpath file if specified
     */
    if !xpath_filename.is_null() {
        xpath = load_xpath_expr(doc, xpath_filename);
        if xpath.is_null() {
            eprintln!("Error: unable to evaluate xpath expression");
            xml_free_doc(doc);
            return -1;
        }
    }

    if !ns_filename.is_null() {
        if load_mem(ns_filename, addr_of_mut!(nslist), addr_of_mut!(nssize)) != 0 {
            eprintln!("Error: unable to evaluate xpath expression");
            if !xpath.is_null() {
                xml_xpath_free_object(xpath);
            }
            xml_free_doc(doc);
            return -1;
        }
        inclusive_namespaces = parse_list(nslist as *mut XmlChar);
    }

    /*
     * Canonical form
     */
    /* fprintf(stderr,"File \"%s\" loaded: start canonization\n", xml_filename); */
    ret = xml_c14n_doc_dump_memory(
        doc,
        if !xpath.is_null() {
            (*xpath).nodesetval
        } else {
            null_mut()
        },
        mode,
        inclusive_namespaces,
        with_comments,
        addr_of_mut!(result),
    );
    if ret >= 0 {
        if !result.is_null() && compare_file_mem(result_file, result as *const c_char, ret) != 0 {
            eprintln!(
                "Result mismatch for {}",
                CStr::from_ptr(xml_filename).to_string_lossy()
            );
            eprintln!(
                "RESULT:\n{}",
                CStr::from_ptr(result as *const c_char).to_string_lossy()
            );
            ret = -1;
        }
    } else {
        eprintln!(
            "Error: failed to canonicalize XML file \"{}\" (ret={})",
            CStr::from_ptr(xml_filename).to_string_lossy(),
            ret,
        );
        ret = -1;
    }

    /*
     * Cleanup
     */
    if !result.is_null() {
        xml_free(result as _);
    }
    if !xpath.is_null() {
        xml_xpath_free_object(xpath);
    }
    if !inclusive_namespaces.is_null() {
        xml_free(inclusive_namespaces as _);
    }
    if !nslist.is_null() {
        free(nslist as _);
    }
    xml_free_doc(doc);

    ret
}

#[cfg(feature = "libxml_c14n")]
unsafe extern "C" fn c14n_common_test(
    filename: *const c_char,
    with_comments: c_int,
    mode: c_int,
    subdir: *const c_char,
) -> c_int {
    use libc::strdup;

    let mut buf: [c_char; 500] = [0; 500];
    let mut prefix: [c_char; 500] = [0; 500];
    let mut len: usize;
    let mut xpath: *mut c_char = null_mut();
    let mut ns: *mut c_char = null_mut();
    let mut ret: c_int = 0;

    let base: *const c_char = base_filename(filename);
    len = strlen(base);
    len -= 4;
    memcpy(prefix.as_mut_ptr() as _, base as _, len);
    prefix[len] = 0;

    if snprintf(
        buf.as_mut_ptr(),
        499,
        c"./result/c14n/%s/%s".as_ptr(),
        subdir,
        prefix.as_ptr(),
    ) >= 499
    {
        buf[499] = 0;
    }
    let result: *mut c_char = strdup(buf.as_ptr());
    if snprintf(
        buf.as_mut_ptr(),
        499,
        c"./test/c14n/%s/%s.xpath".as_ptr(),
        subdir,
        prefix.as_ptr(),
    ) >= 499
    {
        buf[499] = 0;
    }
    if check_test_file(buf.as_ptr()) != 0 {
        xpath = strdup(buf.as_ptr());
    }
    if snprintf(
        buf.as_mut_ptr(),
        499,
        c"./test/c14n/%s/%s.ns".as_ptr(),
        subdir,
        prefix.as_ptr(),
    ) >= 499
    {
        buf[499] = 0;
    }
    if check_test_file(buf.as_ptr()) != 0 {
        ns = strdup(buf.as_ptr());
    }

    NB_TESTS += 1;
    if c14n_run_test(filename, with_comments, mode, xpath, ns, result) < 0 {
        ret = 1;
    }

    if !result.is_null() {
        free(result as _);
    }
    if !xpath.is_null() {
        free(xpath as _);
    }
    if !ns.is_null() {
        free(ns as _);
    }
    ret
}

#[cfg(feature = "libxml_c14n")]
unsafe extern "C" fn c14n_with_comment_test(
    filename: *const c_char,
    _resul: *const c_char,
    _err: *const c_char,
    _options: c_int,
) -> c_int {
    use exml::libxml::c14n::XmlC14NMode;

    c14n_common_test(
        filename,
        1,
        XmlC14NMode::XmlC14N1_0 as i32,
        c"with-comments".as_ptr(),
    )
}

#[cfg(feature = "libxml_c14n")]
unsafe extern "C" fn c14n_without_comment_test(
    filename: *const c_char,
    _resul: *const c_char,
    _err: *const c_char,
    _options: c_int,
) -> c_int {
    use exml::libxml::c14n::XmlC14NMode;

    c14n_common_test(
        filename,
        0,
        XmlC14NMode::XmlC14N1_0 as i32,
        c"without-comments".as_ptr(),
    )
}

#[cfg(feature = "libxml_c14n")]
unsafe extern "C" fn c14n_exc_without_comment_test(
    filename: *const c_char,
    _resul: *const c_char,
    _err: *const c_char,
    _options: c_int,
) -> c_int {
    use exml::libxml::c14n::XmlC14NMode;

    c14n_common_test(
        filename,
        0,
        XmlC14NMode::XmlC14NExclusive1_0 as i32,
        c"exc-without-comments".as_ptr(),
    )
}

#[cfg(feature = "libxml_c14n")]
unsafe extern "C" fn c14n11_without_comment_test(
    filename: *const c_char,
    _resul: *const c_char,
    _err: *const c_char,
    _options: c_int,
) -> c_int {
    use exml::libxml::c14n::XmlC14NMode;

    c14n_common_test(
        filename,
        0,
        XmlC14NMode::XmlC14N1_1 as i32,
        c"1-1-without-comments".as_ptr(),
    )
}

/************************************************************************
 *                                    *
 *            Catalog and threads test            *
 *                                    *
 ************************************************************************/

/*
 * mostly a cut and paste from testThreads.c
 */
#[cfg(all(feature = "thread", feature = "catalog"))]
const MAX_ARGC: usize = 20;

#[cfg(all(feature = "thread", feature = "catalog"))]
#[repr(C)]
struct XmlThreadParams<'a> {
    filename: &'a CStr,
    okay: c_int,
}

#[cfg(all(feature = "thread", feature = "catalog"))]
const CATALOG: &CStr = c"./test/threads/complex.xml";
#[cfg(all(feature = "thread", feature = "catalog"))]
static mut THREAD_PARAMS: [XmlThreadParams; 7] = [
    XmlThreadParams {
        filename: c"./test/threads/abc.xml",
        okay: 0,
    },
    XmlThreadParams {
        filename: c"./test/threads/acb.xml",
        okay: 0,
    },
    XmlThreadParams {
        filename: c"./test/threads/bac.xml",
        okay: 0,
    },
    XmlThreadParams {
        filename: c"./test/threads/bca.xml",
        okay: 0,
    },
    XmlThreadParams {
        filename: c"./test/threads/cab.xml",
        okay: 0,
    },
    XmlThreadParams {
        filename: c"./test/threads/cba.xml",
        okay: 0,
    },
    XmlThreadParams {
        filename: c"./test/threads/invalid.xml",
        okay: 0,
    },
];
#[cfg(all(feature = "thread", feature = "catalog"))]
static NUM_THREADS: usize = unsafe { THREAD_PARAMS.len() };

#[cfg(all(feature = "thread", feature = "catalog"))]
extern "C" fn thread_specific_data(private_data: *mut c_void) -> *mut c_void {
    use exml::libxml::globals::__xml_generic_error_context;

    unsafe {
        use exml::libxml::globals::{
            xml_do_validity_checking_default_value, xml_generic_error_context,
        };

        let my_doc: XmlDocPtr;
        let params: *mut XmlThreadParams = private_data as *mut XmlThreadParams;
        let filename: *const c_char = (*params).filename.as_ptr();
        let mut okay: c_int = 1;

        extern "C" {
            static stdout: *mut FILE;
            static stderr: *mut FILE;
        }

        #[cfg(feature = "thread_alloc")]
        {
            xml_mem_setup(
                Some(xml_mem_free),
                Some(xml_mem_malloc),
                Some(xml_mem_realloc),
                Some(xml_memory_strdup),
            );
        }

        if strcmp(filename, c"./test/threads/invalid.xml".as_ptr()) == 0 {
            *xml_do_validity_checking_default_value() = 0;
            *__xml_generic_error_context() = stdout as _;
        } else {
            *xml_do_validity_checking_default_value() = 1;
            *__xml_generic_error_context() = stderr as _;
        }
        #[cfg(feature = "sax1")]
        {
            my_doc = xml_parse_file(filename);
        }
        #[cfg(not(feature = "sax1"))]
        {
            myDoc = xml_read_file(filename, NULL, XML_WITH_CATALOG);
        }
        if !my_doc.is_null() {
            xml_free_doc(my_doc);
        } else {
            println!("parse failed");
            okay = 0;
        }
        if strcmp(filename, c"./test/threads/invalid.xml".as_ptr()) == 0 {
            if *xml_do_validity_checking_default_value() != 0 {
                println!("ValidityCheckingDefaultValue override failed");
                okay = 0;
            }
            if xml_generic_error_context() != stdout as _ {
                println!("xmlGenericErrorContext override failed");
                okay = 0;
            }
        } else {
            if *xml_do_validity_checking_default_value() != 1 {
                println!("ValidityCheckingDefaultValue override failed");
                okay = 0;
            }
            if xml_generic_error_context() != stderr as _ {
                println!("xmlGenericErrorContext override failed");
                okay = 0;
            }
        }
        (*params).okay = okay;
    }
    null_mut()
}

#[cfg(all(feature = "thread", feature = "catalog"))]
static mut TID: [pthread_t; MAX_ARGC] = [0; MAX_ARGC];

#[cfg(all(feature = "thread", feature = "catalog"))]
unsafe extern "C" fn test_thread() -> c_int {
    use exml::libxml::catalog::{xml_catalog_cleanup, xml_load_catalog};
    use libc::{pthread_create, pthread_join};

    let mut ret: c_int;
    let mut res: c_int = 0;

    xml_init_parser();

    for _ in 0..500 {
        xml_load_catalog(CATALOG.as_ptr());
        NB_TESTS += 1;

        TID[..NUM_THREADS].fill(u64::MAX);

        for i in 0..NUM_THREADS {
            ret = pthread_create(
                addr_of_mut!(TID[i]),
                null(),
                thread_specific_data,
                addr_of_mut!(THREAD_PARAMS[i]) as _,
            );
            if ret != 0 {
                eprintln!("pthread_create failed");
                return 1;
            }
        }
        for &tid in TID.iter().take(NUM_THREADS) {
            let mut result: *mut c_void = null_mut();
            ret = pthread_join(tid, addr_of_mut!(result));
            if ret != 0 {
                eprintln!("pthread_join failed");
                return 1;
            }
        }

        xml_catalog_cleanup();
        for (i, params) in THREAD_PARAMS.iter().take(NUM_THREADS).enumerate() {
            if params.okay == 0 {
                eprintln!(
                    "Thread {} handling {} failed",
                    i,
                    params.filename.to_string_lossy()
                );
                res = 1;
            }
        }
    }
    res
}

#[cfg(all(feature = "thread", feature = "catalog"))]
unsafe extern "C" fn threads_test(
    _filename: *const c_char,
    _resul: *const c_char,
    _err: *const c_char,
    _options: c_int,
) -> c_int {
    test_thread()
}

/************************************************************************
 *                                    *
 *            Regexp tests                    *
 *                                    *
 ************************************************************************/
#[cfg(feature = "regexp")]
unsafe extern "C" fn test_regexp(output: &mut File, comp: XmlRegexpPtr, value: *const c_char) {
    use exml::libxml::xmlregexp::xml_regexp_exec;

    let ret: c_int = xml_regexp_exec(comp, value as _);
    if ret == 1 {
        writeln!(output, "{}: Ok", CStr::from_ptr(value).to_string_lossy()).ok();
    } else if ret == 0 {
        writeln!(output, "{}: Fail", CStr::from_ptr(value).to_string_lossy()).ok();
    } else {
        writeln!(
            output,
            "{}: Error: {}",
            CStr::from_ptr(value).to_string_lossy(),
            ret
        )
        .ok();
    }
}

#[cfg(feature = "regexp")]
unsafe extern "C" fn regexp_test(
    filename: *const c_char,
    result: *const c_char,
    err: *const c_char,
    _options: c_int,
) -> c_int {
    use std::io::{BufRead, BufReader};

    use exml::{
        libxml::{
            globals::xml_generic_error_context,
            xmlregexp::{xml_reg_free_regexp, xml_regexp_compile},
        },
        xml_generic_error,
    };

    let mut comp: XmlRegexpPtr = null_mut();

    let mut ret: c_int;
    let mut res: c_int = 0;

    NB_TESTS += 1;

    let mut input = match File::open(CStr::from_ptr(filename).to_string_lossy().as_ref()) {
        Ok(file) => BufReader::new(file),
        _ => {
            xml_generic_error!(
                xml_generic_error_context(),
                c"Cannot open %s for reading\n".as_ptr(),
                filename
            );
            return -1;
        }
    };
    let temp: *mut c_char = result_filename(filename, c"".as_ptr(), c".res".as_ptr());
    if temp.is_null() {
        eprintln!("Out of memory");
        fatal_error();
    }
    let mut output = match File::options()
        .write(true)
        .truncate(true)
        .create(true)
        .open(CStr::from_ptr(temp).to_string_lossy().as_ref())
    {
        Ok(file) => file,
        _ => {
            eprintln!(
                "failed to open output file {}",
                CStr::from_ptr(temp).to_string_lossy()
            );
            free(temp as _);
            return -1;
        }
    };
    let mut expression = vec![];
    while let Some(mut len) = input
        .read_until(b'\n', &mut expression)
        .ok()
        .filter(|&len| len > 0)
        .map(|len| len as i32)
    {
        len -= 1;
        while len >= 0
            && (expression[len as usize] == b'\n'
                || expression[len as usize] == b'\t'
                || expression[len as usize] == b'\r'
                || expression[len as usize] == b' ')
        {
            len -= 1;
        }
        if len + 1 == expression.len() as i32 {
            expression.push(0);
        } else {
            expression[(len + 1) as usize] = 0;
        }
        if len >= 0 {
            if expression[0] == b'#' {
                expression.clear();
                continue;
            }
            if expression[0] == b'=' && expression[1] == b'>' {
                let pattern: *mut c_char = expression.as_mut_ptr().add(2) as _;

                if !comp.is_null() {
                    xml_reg_free_regexp(comp);
                    // comp = null_mut();
                }
                writeln!(
                    output,
                    "Regexp: {}",
                    CStr::from_ptr(pattern).to_string_lossy()
                )
                .ok();
                comp = xml_regexp_compile(pattern as _);
                if comp.is_null() {
                    writeln!(output, "   failed to compile").ok();
                    break;
                }
            } else if comp.is_null() {
                writeln!(
                    output,
                    "Regexp: {}",
                    CStr::from_ptr(expression.as_ptr() as _).to_string_lossy()
                )
                .ok();
                comp = xml_regexp_compile(expression.as_ptr());
                if comp.is_null() {
                    writeln!(output, "   failed to compile").ok();
                    break;
                }
            } else if !comp.is_null() {
                test_regexp(&mut output, comp, expression.as_ptr() as _);
            }
        }
        expression.clear();
    }
    if !comp.is_null() {
        xml_reg_free_regexp(comp);
    }

    ret = compare_files(temp, result);
    if ret != 0 {
        eprintln!(
            "Result for {} failed in {}",
            CStr::from_ptr(filename).to_string_lossy(),
            CStr::from_ptr(result).to_string_lossy()
        );
        res = 1;
    }
    if !temp.is_null() {
        unlink(temp);
        free(temp as _);
    }

    ret = compare_file_mem(err, TEST_ERRORS.as_ptr() as _, TEST_ERRORS_SIZE as _);
    if ret != 0 {
        eprintln!(
            "Error for {} failed",
            CStr::from_ptr(filename).to_string_lossy()
        );
        res = 1;
    }

    res
}

/************************************************************************
 *                                    *
 *            Automata tests                    *
 *                                    *
 ************************************************************************/
#[cfg(feature = "libxml_automata")]
unsafe extern "C" fn scan_number(ptr: *mut *mut c_char) -> c_int {
    let mut ret: c_int = 0;
    let mut cur: *mut c_char;

    cur = *ptr;
    while *cur >= b'0' as _ && *cur <= b'9' as _ {
        ret = ret * 10 + (*cur - b'0' as i8) as i32;
        cur = cur.add(1);
    }
    *ptr = cur;
    ret
}

#[cfg(feature = "libxml_automata")]
unsafe extern "C" fn automata_test(
    filename: *const c_char,
    result: *const c_char,
    _err: *const c_char,
    _options: c_int,
) -> c_int {
    use std::io::{BufRead, BufReader};

    use exml::{
        libxml::{
            globals::xml_generic_error_context,
            xmlautomata::{
                xml_automata_compile, xml_automata_get_init_state, xml_automata_new_count_trans,
                xml_automata_new_epsilon, xml_automata_new_state, xml_automata_new_transition,
                xml_automata_set_final_state, xml_free_automata, xml_new_automata, XmlAutomataPtr,
                XmlAutomataStatePtr,
            },
            xmlregexp::{
                xml_reg_exec_push_string, xml_reg_free_exec_ctxt, xml_reg_free_regexp,
                xml_reg_new_exec_ctxt, XmlRegExecCtxtPtr,
            },
        },
        xml_generic_error,
    };

    let mut ret: c_int;
    let mut res: c_int = 0;
    let mut am: XmlAutomataPtr;
    let mut states: [XmlAutomataStatePtr; 1000] = [null_mut(); 1000];
    let mut regexp: XmlRegexpPtr = null_mut();
    let mut exec: XmlRegExecCtxtPtr = null_mut();

    NB_TESTS += 1;

    let mut input = match File::open(CStr::from_ptr(filename).to_string_lossy().as_ref()) {
        Ok(file) => BufReader::new(file),
        _ => {
            xml_generic_error!(
                xml_generic_error_context(),
                c"Cannot open %s for reading\n".as_ptr(),
                filename
            );
            return -1;
        }
    };
    let temp: *mut c_char = result_filename(filename, c"".as_ptr(), c".res".as_ptr());
    if temp.is_null() {
        eprintln!("Out of memory");
        fatal_error();
    }
    let Ok(mut output) = File::options()
        .write(true)
        .truncate(true)
        .create(true)
        .open(CStr::from_ptr(temp).to_string_lossy().as_ref())
    else {
        eprintln!(
            "failed to open output file {}",
            CStr::from_ptr(temp).to_string_lossy()
        );
        free(temp as _);
        return -1;
    };

    am = xml_new_automata();
    if am.is_null() {
        xml_generic_error!(
            xml_generic_error_context(),
            c"Cannot create automata\n".as_ptr()
        );
        return -1;
    }
    states[0] = xml_automata_get_init_state(am);
    if states[0].is_null() {
        xml_generic_error!(
            xml_generic_error_context(),
            c"Cannot get start state\n".as_ptr()
        );
        xml_free_automata(am);
        return -1;
    }
    ret = 0;

    let mut expr = vec![];
    while let Some(mut len) = input
        .read_until(b'\n', &mut expr)
        .ok()
        .filter(|&len| len > 0)
        .map(|len| len as i32)
    {
        if expr[0] == b'#' {
            expr.clear();
            continue;
        }
        len -= 1;
        while len >= 0
            && (expr[len as usize] == b'\n'
                || expr[len as usize] == b'\t'
                || expr[len as usize] == b'\r'
                || expr[len as usize] == b' ')
        {
            len -= 1;
        }
        if len + 1 == expr.len() as i32 {
            expr.push(0);
        } else {
            expr[(len + 1) as usize] = 0;
        }
        if len >= 0 {
            if !am.is_null() && expr[0] == b't' && expr[1] == b' ' {
                let mut ptr: *mut c_char = expr.as_mut_ptr().add(2) as _;

                let from: usize = scan_number(addr_of_mut!(ptr)) as _;
                if *ptr != b' ' as i8 {
                    xml_generic_error!(
                        xml_generic_error_context(),
                        c"Bad line %s\n".as_ptr(),
                        expr.as_ptr()
                    );
                    break;
                }
                if states[from].is_null() {
                    states[from] = xml_automata_new_state(am);
                }
                ptr = ptr.add(1);
                let to: usize = scan_number(addr_of_mut!(ptr)) as _;
                if *ptr != b' ' as i8 {
                    xml_generic_error!(
                        xml_generic_error_context(),
                        c"Bad line %s\n".as_ptr(),
                        expr.as_ptr()
                    );
                    break;
                }
                if states[to].is_null() {
                    states[to] = xml_automata_new_state(am);
                }
                ptr = ptr.add(1);
                xml_automata_new_transition(am, states[from], states[to], ptr as _, null_mut());
            } else if !am.is_null() && expr[0] == b'e' && expr[1] == b' ' {
                let mut ptr: *mut c_char = expr.as_mut_ptr().add(2) as _;

                let from: usize = scan_number(addr_of_mut!(ptr)) as _;
                if *ptr != b' ' as i8 {
                    xml_generic_error!(
                        xml_generic_error_context(),
                        c"Bad line %s\n".as_ptr(),
                        expr.as_ptr()
                    );
                    break;
                }
                if states[from].is_null() {
                    states[from] = xml_automata_new_state(am);
                }
                ptr = ptr.add(1);
                let to: usize = scan_number(addr_of_mut!(ptr)) as _;
                if states[to].is_null() {
                    states[to] = xml_automata_new_state(am);
                }
                xml_automata_new_epsilon(am, states[from], states[to]);
            } else if !am.is_null() && expr[0] == b'f' && expr[1] == b' ' {
                let mut ptr: *mut c_char = expr.as_mut_ptr().add(2) as _;

                let state: usize = scan_number(addr_of_mut!(ptr)) as _;
                if states[state].is_null() {
                    xml_generic_error!(
                        xml_generic_error_context(),
                        c"Bad state %d : %s\n".as_ptr(),
                        state,
                        expr.as_ptr()
                    );
                    break;
                }
                xml_automata_set_final_state(am, states[state]);
            } else if !am.is_null() && expr[0] == b'c' && expr[1] == b' ' {
                let mut ptr: *mut c_char = expr.as_mut_ptr().add(2) as _;

                let from: usize = scan_number(addr_of_mut!(ptr)) as _;
                if *ptr != b' ' as i8 {
                    xml_generic_error!(
                        xml_generic_error_context(),
                        c"Bad line %s\n".as_ptr(),
                        expr.as_ptr()
                    );
                    break;
                }
                if states[from].is_null() {
                    states[from] = xml_automata_new_state(am);
                }
                ptr = ptr.add(1);
                let to: usize = scan_number(addr_of_mut!(ptr)) as _;
                if *ptr != b' ' as i8 {
                    xml_generic_error!(
                        xml_generic_error_context(),
                        c"Bad line %s\n".as_ptr(),
                        expr.as_ptr()
                    );
                    break;
                }
                if states[to].is_null() {
                    states[to] = xml_automata_new_state(am);
                }
                ptr = ptr.add(1);
                let min: c_int = scan_number(addr_of_mut!(ptr));
                if *ptr != b' ' as i8 {
                    xml_generic_error!(
                        xml_generic_error_context(),
                        c"Bad line %s\n".as_ptr(),
                        expr.as_ptr()
                    );
                    break;
                }
                ptr = ptr.add(1);
                let max: c_int = scan_number(addr_of_mut!(ptr));
                if *ptr != b' ' as i8 {
                    xml_generic_error!(
                        xml_generic_error_context(),
                        c"Bad line %s\n".as_ptr(),
                        expr.as_ptr()
                    );
                    break;
                }
                ptr = ptr.add(1);
                xml_automata_new_count_trans(
                    am,
                    states[from],
                    states[to],
                    ptr as _,
                    min,
                    max,
                    null_mut(),
                );
            } else if !am.is_null() && expr[0] == b'-' && expr[1] == b'-' {
                /* end of the automata */
                regexp = xml_automata_compile(am);
                xml_free_automata(am);
                am = null_mut();
                if regexp.is_null() {
                    xml_generic_error!(
                        xml_generic_error_context(),
                        c"Failed to compile the automata".as_ptr()
                    );
                    break;
                }
            } else if expr[0] == b'=' && expr[1] == b'>' {
                if regexp.is_null() {
                    writeln!(output, "=> failed not compiled").ok();
                } else {
                    if exec.is_null() {
                        exec = xml_reg_new_exec_ctxt(regexp, None, null_mut());
                    }
                    if ret == 0 {
                        ret = xml_reg_exec_push_string(exec, null_mut(), null_mut());
                    }
                    if ret == 1 {
                        writeln!(output, "=> Passed").ok();
                    } else if ret == 0 || ret == -1 {
                        writeln!(output, "=> Failed").ok();
                    } else if ret < 0 {
                        writeln!(output, "=> Error").ok();
                    }
                    xml_reg_free_exec_ctxt(exec);
                    exec = null_mut();
                }
                ret = 0;
            } else if !regexp.is_null() {
                if exec.is_null() {
                    exec = xml_reg_new_exec_ctxt(regexp, None, null_mut());
                }
                ret = xml_reg_exec_push_string(exec, expr.as_ptr(), null_mut());
            } else {
                xml_generic_error!(
                    xml_generic_error_context(),
                    c"Unexpected line %s\n".as_ptr(),
                    expr.as_ptr()
                );
            }
        }
        expr.clear();
    }
    if !regexp.is_null() {
        xml_reg_free_regexp(regexp);
    }
    if !exec.is_null() {
        xml_reg_free_exec_ctxt(exec);
    }
    if !am.is_null() {
        xml_free_automata(am);
    }

    ret = compare_files(temp, result);
    if ret != 0 {
        eprintln!(
            "Result for {} failed in {}",
            CStr::from_ptr(filename).to_string_lossy(),
            CStr::from_ptr(result).to_string_lossy()
        );
        res = 1;
    }
    if !temp.is_null() {
        unlink(temp);
        free(temp as _);
    }

    res
}

/************************************************************************
 *                                    *
 *            Tests Descriptions                *
 *                                    *
 ************************************************************************/

const TEST_DESCRIPTIONS: &[TestDesc] = &[
    TestDesc {
        desc: "for debug",
        func: sax_parse_test,
        input: Some(c"./test/defattr2.xml"),
        out: Some(c"./result/"),
        suffix: Some(c".sax2"),
        err: None,
        options: 0,
    },
    TestDesc {
        desc: "XML regression tests",
        func: old_parse_test,
        input: Some(c"./test/*"),
        out: Some(c"./result/"),
        suffix: Some(c""),
        err: None,
        options: 0,
    },
    TestDesc {
        desc: "XML regression tests on memory",
        func: mem_parse_test,
        input: Some(c"./test/*"),
        out: Some(c"./result/"),
        suffix: Some(c""),
        err: None,
        options: 0,
    },
    TestDesc {
        desc: "XML entity subst regression tests",
        func: noent_parse_test,
        input: Some(c"./test/*"),
        out: Some(c"./result/noent/"),
        suffix: Some(c""),
        err: None,
        options: XmlParserOption::XmlParseNoent as i32,
    },
    TestDesc {
        desc: "XML Namespaces regression tests",
        func: err_parse_test,
        input: Some(c"./test/namespaces/*"),
        out: Some(c"./result/namespaces/"),
        suffix: Some(c""),
        err: Some(c".err"),
        options: 0,
    },
    #[cfg(feature = "valid")]
    TestDesc {
        desc: "Error cases regression tests",
        func: err_parse_test,
        input: Some(c"./test/errors/*.xml"),
        out: Some(c"./result/errors/"),
        suffix: Some(c""),
        err: Some(c".err"),
        options: 0,
    },
    #[cfg(feature = "valid")]
    TestDesc {
        desc: "Error cases regression tests from file descriptor",
        func: fd_parse_test,
        input: Some(c"./test/errors/*.xml"),
        out: Some(c"./result/errors/"),
        suffix: Some(c""),
        err: Some(c".err"),
        options: 0,
    },
    #[cfg(feature = "valid")]
    TestDesc {
        desc: "Error cases regression tests with entity substitution",
        func: err_parse_test,
        input: Some(c"./test/errors/*.xml"),
        out: Some(c"./result/errors/"),
        suffix: None,
        err: Some(c".ent"),
        options: XmlParserOption::XmlParseNoent as i32,
    },
    #[cfg(feature = "valid")]
    TestDesc {
        desc: "Error cases regression tests (old 1.0)",
        func: err_parse_test,
        input: Some(c"./test/errors10/*.xml"),
        out: Some(c"./result/errors10/"),
        suffix: Some(c""),
        err: Some(c".err"),
        options: XmlParserOption::XmlParseOld10 as i32,
    },
    #[cfg(all(feature = "libxml_reader", feature = "valid"))]
    TestDesc {
        desc: "Error cases stream regression tests",
        func: stream_parse_test,
        input: Some(c"./test/errors/*.xml"),
        out: Some(c"./result/errors/"),
        suffix: None,
        err: Some(c".str"),
        options: 0,
    },
    #[cfg(feature = "libxml_reader")]
    TestDesc {
        desc: "Reader regression tests",
        func: stream_parse_test,
        input: Some(c"./test/*"),
        out: Some(c"./result/"),
        suffix: Some(c".rdr"),
        err: None,
        options: 0,
    },
    #[cfg(feature = "libxml_reader")]
    TestDesc {
        desc: "Reader entities substitution regression tests",
        func: stream_parse_test,
        input: Some(c"./test/*"),
        out: Some(c"./result/"),
        suffix: Some(c".rde"),
        err: None,
        options: XmlParserOption::XmlParseNoent as i32,
    },
    #[cfg(feature = "libxml_reader")]
    TestDesc {
        desc: "Reader on memory regression tests",
        func: stream_mem_parse_test,
        input: Some(c"./test/*"),
        out: Some(c"./result/"),
        suffix: Some(c".rdr"),
        err: None,
        options: 0,
    },
    #[cfg(feature = "libxml_reader")]
    TestDesc {
        desc: "Walker regression tests",
        func: walker_parse_test,
        input: Some(c"./test/*"),
        out: Some(c"./result/"),
        suffix: Some(c".rdr"),
        err: None,
        options: 0,
    },
    #[cfg(feature = "sax1")]
    TestDesc {
        desc: "SAX1 callbacks regression tests",
        func: sax_parse_test,
        input: Some(c"./test/*"),
        out: Some(c"./result/"),
        suffix: Some(c".sax"),
        err: None,
        options: XmlParserOption::XmlParseSax1 as i32,
    },
    TestDesc {
        desc: "SAX2 callbacks regression tests",
        func: sax_parse_test,
        input: Some(c"./test/*"),
        out: Some(c"./result/"),
        suffix: Some(c".sax2"),
        err: None,
        options: 0,
    },
    TestDesc {
        desc: "SAX2 callbacks regression tests with entity substitution",
        func: sax_parse_test,
        input: Some(c"./test/*"),
        out: Some(c"./result/noent/"),
        suffix: Some(c".sax2"),
        err: None,
        options: XmlParserOption::XmlParseNoent as i32,
    },
    #[cfg(feature = "push")]
    TestDesc {
        desc: "XML push regression tests",
        func: push_parse_test,
        input: Some(c"./test/*"),
        out: Some(c"./result/"),
        suffix: Some(c""),
        err: None,
        options: 0,
    },
    #[cfg(feature = "push")]
    TestDesc {
        desc: "XML push boundary tests",
        func: push_boundary_test,
        input: Some(c"./test/*"),
        out: Some(c"./result/"),
        suffix: Some(c""),
        err: None,
        options: 0,
    },
    #[cfg(feature = "html")]
    TestDesc {
        desc: "HTML regression tests",
        func: err_parse_test,
        input: Some(c"./test/HTML/*"),
        out: Some(c"./result/HTML/"),
        suffix: Some(c""),
        err: Some(c".err"),
        options: XML_PARSE_HTML,
    },
    #[cfg(feature = "html")]
    TestDesc {
        desc: "HTML regression tests from file descriptor",
        func: fd_parse_test,
        input: Some(c"./test/HTML/*"),
        out: Some(c"./result/HTML/"),
        suffix: Some(c""),
        err: Some(c".err"),
        options: XML_PARSE_HTML,
    },
    #[cfg(all(feature = "html", feature = "push"))]
    TestDesc {
        desc: "Push HTML regression tests",
        func: push_parse_test,
        input: Some(c"./test/HTML/*"),
        out: Some(c"./result/HTML/"),
        suffix: Some(c""),
        err: Some(c".err"),
        options: XML_PARSE_HTML,
    },
    #[cfg(all(feature = "html", feature = "push"))]
    TestDesc {
        desc: "Push HTML boundary tests",
        func: push_boundary_test,
        input: Some(c"./test/HTML/*"),
        out: Some(c"./result/HTML/"),
        suffix: Some(c""),
        err: None,
        options: XML_PARSE_HTML,
    },
    #[cfg(feature = "html")]
    TestDesc {
        desc: "HTML SAX regression tests",
        func: sax_parse_test,
        input: Some(c"./test/HTML/*"),
        out: Some(c"./result/HTML/"),
        suffix: Some(c".sax"),
        err: None,
        options: XML_PARSE_HTML,
    },
    #[cfg(feature = "valid")]
    TestDesc {
        desc: "Valid documents regression tests",
        func: err_parse_test,
        input: Some(c"./test/VCM/*"),
        out: None,
        suffix: None,
        err: None,
        options: XmlParserOption::XmlParseDtdvalid as i32,
    },
    #[cfg(feature = "valid")]
    TestDesc {
        desc: "Validity checking regression tests",
        func: err_parse_test,
        input: Some(c"./test/VC/*"),
        out: Some(c"./result/VC/"),
        suffix: None,
        err: Some(c""),
        options: XmlParserOption::XmlParseDtdvalid as i32,
    },
    #[cfg(all(feature = "valid", feature = "libxml_reader"))]
    TestDesc {
        desc: "Streaming validity checking regression tests",
        func: stream_parse_test,
        input: Some(c"./test/valid/*.xml"),
        out: Some(c"./result/valid/"),
        suffix: None,
        err: Some(c".err.rdr"),
        options: XmlParserOption::XmlParseDtdvalid as i32,
    },
    #[cfg(all(feature = "valid", feature = "libxml_reader"))]
    TestDesc {
        desc: "Streaming validity error checking regression tests",
        func: stream_parse_test,
        input: Some(c"./test/VC/*"),
        out: Some(c"./result/VC/"),
        suffix: None,
        err: Some(c".rdr"),
        options: XmlParserOption::XmlParseDtdvalid as i32,
    },
    #[cfg(feature = "valid")]
    TestDesc {
        desc: "General documents valid regression tests",
        func: err_parse_test,
        input: Some(c"./test/valid/*"),
        out: Some(c"./result/valid/"),
        suffix: Some(c""),
        err: Some(c".err"),
        options: XmlParserOption::XmlParseDtdvalid as i32,
    },
    #[cfg(feature = "xinclude")]
    TestDesc {
        desc: "XInclude regression tests",
        func: err_parse_test,
        input: Some(c"./test/XInclude/docs/*"),
        out: Some(c"./result/XInclude/"),
        suffix: Some(c""),
        err: Some(c".err"),
        options: XmlParserOption::XmlParseXinclude as i32,
    },
    #[cfg(all(feature = "xinclude", feature = "libxml_reader"))]
    TestDesc {
        desc: "XInclude xmlReader regression tests",
        func: stream_parse_test,
        input: Some(c"./test/XInclude/docs/*"),
        out: Some(c"./result/XInclude/"),
        suffix: Some(c".rdr"),
        err: Some(c".err"),
        options: XmlParserOption::XmlParseXinclude as i32,
    },
    #[cfg(feature = "xinclude")]
    TestDesc {
        desc: "XInclude regression tests stripping include nodes",
        func: err_parse_test,
        input: Some(c"./test/XInclude/docs/*"),
        out: Some(c"./result/XInclude/"),
        suffix: Some(c""),
        err: Some(c".err"),
        options: XmlParserOption::XmlParseXinclude as i32
            | XmlParserOption::XmlParseNoxincnode as i32,
    },
    #[cfg(all(feature = "xinclude", feature = "libxml_reader"))]
    TestDesc {
        desc: "XInclude xmlReader regression tests stripping include nodes",
        func: stream_parse_test,
        input: Some(c"./test/XInclude/docs/*"),
        out: Some(c"./result/XInclude/"),
        suffix: Some(c".rdr"),
        err: Some(c".err"),
        options: XmlParserOption::XmlParseXinclude as i32
            | XmlParserOption::XmlParseNoxincnode as i32,
    },
    #[cfg(feature = "xinclude")]
    TestDesc {
        desc: "XInclude regression tests without reader",
        func: err_parse_test,
        input: Some(c"./test/XInclude/without-reader/*"),
        out: Some(c"./result/XInclude/"),
        suffix: Some(c""),
        err: Some(c".err"),
        options: XmlParserOption::XmlParseXinclude as i32,
    },
    #[cfg(all(feature = "xpath", feature = "libxml_debug"))]
    TestDesc {
        desc: "XPath expressions regression tests",
        func: xpath_expr_test,
        input: Some(c"./test/XPath/expr/*"),
        out: Some(c"./result/XPath/expr/"),
        suffix: Some(c""),
        err: None,
        options: 0,
    },
    #[cfg(all(feature = "xpath", feature = "libxml_debug"))]
    TestDesc {
        desc: "XPath document queries regression tests",
        func: xpath_doc_test,
        input: Some(c"./test/XPath/docs/*"),
        out: None,
        suffix: None,
        err: None,
        options: 0,
    },
    #[cfg(all(feature = "xpath", feature = "libxml_debug", feature = "libxml_xptr"))]
    TestDesc {
        desc: "XPointer document queries regression tests",
        func: xptr_doc_test,
        input: Some(c"./test/XPath/docs/*"),
        out: None,
        suffix: None,
        err: None,
        options: -1,
    },
    #[cfg(all(
        feature = "xpath",
        feature = "libxml_debug",
        feature = "libxml_xptr_locs"
    ))]
    TestDesc {
        desc: "XPointer xpointer() queries regression tests",
        func: xptr_doc_test,
        input: Some(c"./test/XPath/docs/*"),
        out: None,
        suffix: None,
        err: None,
        options: 0,
    },
    #[cfg(all(feature = "xpath", feature = "libxml_debug", feature = "valid"))]
    TestDesc {
        desc: "xml:id regression tests",
        func: xmlid_doc_test,
        input: Some(c"./test/xmlid/*"),
        out: Some(c"./result/xmlid/"),
        suffix: Some(c""),
        err: Some(c".err"),
        options: 0,
    },
    TestDesc {
        desc: "URI parsing tests",
        func: uri_parse_test,
        input: Some(c"./test/URI/*.uri"),
        out: Some(c"./result/URI/"),
        suffix: Some(c""),
        err: None,
        options: 0,
    },
    TestDesc {
        desc: "URI base composition tests",
        func: uri_base_test,
        input: Some(c"./test/URI/*.data"),
        out: Some(c"./result/URI/"),
        suffix: Some(c""),
        err: None,
        options: 0,
    },
    TestDesc {
        desc: "Path URI conversion tests",
        func: uri_path_test,
        input: None,
        out: None,
        suffix: None,
        err: None,
        options: 0,
    },
    #[cfg(feature = "schema")]
    TestDesc {
        desc: "Schemas regression tests",
        func: schemas_test,
        input: Some(c"./test/schemas/*_*.xsd"),
        out: None,
        suffix: None,
        err: None,
        options: 0,
    },
    #[cfg(feature = "schema")]
    TestDesc {
        desc: "Relax-NG regression tests",
        func: rng_test,
        input: Some(c"./test/relaxng/*.rng"),
        out: None,
        suffix: None,
        err: None,
        options: XmlParserOption::XmlParseDtdattr as i32 | XmlParserOption::XmlParseNoent as i32,
    },
    #[cfg(all(feature = "schema", feature = "libxml_reader"))]
    TestDesc {
        desc: "Relax-NG streaming regression tests",
        func: rng_stream_test,
        input: Some(c"./test/relaxng/*.rng"),
        out: None,
        suffix: None,
        err: None,
        options: XmlParserOption::XmlParseDtdattr as i32 | XmlParserOption::XmlParseNoent as i32,
    },
    #[cfg(all(feature = "libxml_pattern", feature = "libxml_reader"))]
    TestDesc {
        desc: "Pattern regression tests",
        func: pattern_test,
        input: Some(c"./test/pattern/*.pat"),
        out: Some(c"./result/pattern/"),
        suffix: None,
        err: None,
        options: 0,
    },
    #[cfg(feature = "libxml_c14n")]
    TestDesc {
        desc: "C14N with comments regression tests",
        func: c14n_with_comment_test,
        input: Some(c"./test/c14n/with-comments/*.xml"),
        out: None,
        suffix: None,
        err: None,
        options: 0,
    },
    #[cfg(feature = "libxml_c14n")]
    TestDesc {
        desc: "C14N without comments regression tests",
        func: c14n_without_comment_test,
        input: Some(c"./test/c14n/without-comments/*.xml"),
        out: None,
        suffix: None,
        err: None,
        options: 0,
    },
    #[cfg(feature = "libxml_c14n")]
    TestDesc {
        desc: "C14N exclusive without comments regression tests",
        func: c14n_exc_without_comment_test,
        input: Some(c"./test/c14n/exc-without-comments/*.xml"),
        out: None,
        suffix: None,
        err: None,
        options: 0,
    },
    #[cfg(feature = "libxml_c14n")]
    TestDesc {
        desc: "C14N 1.1 without comments regression tests",
        func: c14n11_without_comment_test,
        input: Some(c"./test/c14n/1-1-without-comments/*.xml"),
        out: None,
        suffix: None,
        err: None,
        options: 0,
    },
    #[cfg(all(feature = "thread", feature = "catalog"))]
    TestDesc {
        desc: "Catalog and Threads regression tests",
        func: threads_test,
        input: None,
        out: None,
        suffix: None,
        err: None,
        options: 0,
    },
    TestDesc {
        desc: "SVG parsing regression tests",
        func: old_parse_test,
        input: Some(c"./test/SVG/*.xml"),
        out: Some(c"./result/SVG/"),
        suffix: Some(c""),
        err: None,
        options: 0,
    },
    #[cfg(feature = "regexp")]
    TestDesc {
        desc: "Regexp regression tests",
        func: regexp_test,
        input: Some(c"./test/regexp/*"),
        out: Some(c"./result/regexp/"),
        suffix: Some(c""),
        err: Some(c".err"),
        options: 0,
    },
    #[cfg(feature = "libxml_automata")]
    TestDesc {
        desc: "Automata regression tests",
        func: automata_test,
        input: Some(c"./test/automata/*"),
        out: Some(c"./result/automata/"),
        suffix: Some(c""),
        err: None,
        options: 0,
    },
];

/************************************************************************
 *                                    *
 *        The main code driving the tests                *
 *                                    *
 ************************************************************************/

unsafe extern "C" fn launch_tests(tst: &TestDesc) -> c_int {
    let mut res: c_int;
    let mut err: c_int = 0;
    let mut result: *mut c_char;
    let mut error: *mut c_char;
    let mut mem: c_int;

    let ebcdic_handler: XmlCharEncodingHandlerPtr =
        xmlGetCharEncodingHandler(XmlCharEncoding::XmlCharEncodingEbcdic);
    let euc_jp_handler: XmlCharEncodingHandlerPtr =
        xmlGetCharEncodingHandler(XmlCharEncoding::XmlCharEncodingEucJp);

    if let Some(input) = tst.input {
        let mut globbuf: glob_t = unsafe { zeroed() };

        globbuf.gl_offs = 0;
        glob(input.as_ptr(), GLOB_DOOFFS, None, addr_of_mut!(globbuf));
        for i in 0..globbuf.gl_pathc {
            if check_test_file(*globbuf.gl_pathv.add(i)) == 0 {
                continue;
            }
            if (ebcdic_handler.is_null()
                && !strstr(*globbuf.gl_pathv.add(i), c"ebcdic".as_ptr()).is_null())
                || (euc_jp_handler.is_null()
                    && !strstr(*globbuf.gl_pathv.add(i), c"icu_parse_test".as_ptr()).is_null())
            {
                continue;
            }
            if let Some(suffix) = tst.suffix {
                result = result_filename(
                    *globbuf.gl_pathv.add(i),
                    tst.out.map(|o| o.as_ptr()).unwrap_or(null_mut()),
                    suffix.as_ptr(),
                );
                if result.is_null() {
                    eprintln!("Out of memory !");
                    fatal_error();
                }
            } else {
                result = null_mut();
            }
            if let Some(err) = tst.err {
                error = result_filename(
                    *globbuf.gl_pathv.add(i),
                    tst.out.map(|o| o.as_ptr()).unwrap_or(null_mut()),
                    err.as_ptr(),
                );
                if error.is_null() {
                    eprintln!("Out of memory !");
                    fatal_error();
                }
            } else {
                error = null_mut();
            }
            mem = xml_mem_used();
            EXTRA_MEMORY_FROM_RESOLVER = 0;
            TEST_ERRORS_SIZE = 0;
            TEST_ERRORS[0] = 0;
            res = (tst.func)(
                *globbuf.gl_pathv.add(i),
                result,
                error,
                tst.options | XmlParserOption::XmlParseCompact as i32,
            );
            xmlResetLastError();
            if res != 0 {
                eprintln!(
                    "File {} generated an error",
                    CStr::from_ptr(*globbuf.gl_pathv.add(i)).to_string_lossy(),
                );
                NB_ERRORS += 1;
                err += 1;
            } else if xml_mem_used() != mem
                && xml_mem_used() != mem
                && EXTRA_MEMORY_FROM_RESOLVER == 0
            {
                eprintln!(
                    "File {} leaked {} bytes",
                    CStr::from_ptr(*globbuf.gl_pathv.add(i)).to_string_lossy(),
                    xml_mem_used() - mem,
                );
                NB_LEAKS += 1;
                err += 1;
            }
            TEST_ERRORS_SIZE = 0;
            if !result.is_null() {
                free(result as _);
            }
            if !error.is_null() {
                free(error as _);
            }
        }
        globfree(addr_of_mut!(globbuf));
    } else {
        TEST_ERRORS_SIZE = 0;
        TEST_ERRORS[0] = 0;
        EXTRA_MEMORY_FROM_RESOLVER = 0;
        res = (tst.func)(null_mut(), null_mut(), null_mut(), tst.options);
        if res != 0 {
            NB_ERRORS += 1;
            err += 1;
        }
    }

    xmlCharEncCloseFunc(ebcdic_handler);
    xmlCharEncCloseFunc(euc_jp_handler);

    err
}

static mut VERBOSE: c_int = 0;
static mut TESTS_QUIET: c_int = 0;

unsafe extern "C" fn runtest(i: usize) -> c_int {
    let mut ret: c_int = 0;

    let old_errors: c_int = NB_ERRORS;
    let old_tests: c_int = NB_TESTS;
    let old_leaks: c_int = NB_LEAKS;
    if TESTS_QUIET == 0 {
        println!("## {}", TEST_DESCRIPTIONS[i].desc);
    }
    let res: c_int = launch_tests(&TEST_DESCRIPTIONS[i]);
    if res != 0 {
        ret += 1;
    }
    if VERBOSE != 0 {
        if NB_ERRORS == old_errors && NB_LEAKS == old_leaks {
            println!("Ran {} tests, no errors", NB_TESTS - old_tests);
        } else {
            println!(
                "Ran {} tests, {} errors, {} leaks",
                NB_TESTS - old_tests,
                NB_ERRORS - old_errors,
                NB_LEAKS - old_leaks,
            );
        }
    }
    ret
}

#[test]
fn main() {
    let mut ret: c_int = 0;
    let mut subset: c_int = 0;

    unsafe {
        initialize_libxml2();

        let mut args = args();
        while let Some(arg) = args.next() {
            if arg == "-v" {
                VERBOSE = 1;
            } else if arg == "-u" {
                UPDATE_RESULTS = 1;
            } else if arg == "-quiet" {
                TESTS_QUIET = 1;
            } else if arg == "--out" {
                if let Ok(s) = CString::new(args.next().unwrap()) {
                    TEMP_DIRECTORY.set(s).unwrap();
                }
            } else {
                for (i, descr) in TEST_DESCRIPTIONS.iter().enumerate() {
                    if descr.desc.contains(&arg) {
                        ret += runtest(i);
                        subset += 1;
                    }
                }
            }
        }
        if subset == 0 {
            for i in 0..TEST_DESCRIPTIONS.len() {
                ret += runtest(i);
            }
        }
        if NB_ERRORS == 0 && NB_LEAKS == 0 {
            ret = 0;
            println!("Total {} tests, no errors", NB_TESTS);
        } else {
            ret = 1;
            println!(
                "Total {} tests, {} errors, {} leaks",
                NB_TESTS, NB_ERRORS, NB_LEAKS,
            );
        }
        xml_cleanup_parser();
        xml_memory_dump();
    }

    assert_eq!(ret, 0);
}
