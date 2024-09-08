/*
 * testrecurse.c: C program to run libxml2 regression tests checking entities
 *            recursions
 *
 * To compile on Unixes:
 * cc -o testrecurse `xml2-config --cflags` testrecurse.c `xml2-config --libs` -lpthread
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 */

use std::{
    env::args,
    ffi::{c_char, c_int, c_ulong, c_void, CStr},
    fs::metadata,
    mem::zeroed,
    process::exit,
    ptr::{addr_of_mut, null, null_mut},
    sync::atomic::{AtomicPtr, Ordering},
};

use exml::{
    libxml::{
        entities::{xml_get_doc_entity, XmlEntityPtr},
        globals::xml_get_warnings_default_value,
        parser::{
            xml_cleanup_parser, xml_ctxt_read_file, xml_free_parser_ctxt, xml_init_parser,
            xml_new_parser_ctxt, xml_pedantic_parser_default, xml_set_external_entity_loader,
            XmlParserCtxtPtr, XmlParserInputPtr, XmlParserOption,
        },
        tree::{xml_free_doc, XmlDocPtr, XmlElementType, XmlNodePtr},
        xml_io::{xml_no_net_external_entity_loader, xml_register_input_callbacks},
        xmlerror::{
            xmlResetLastError, xmlSetStructuredErrorFunc, XmlErrorDomain, XmlErrorLevel,
            XmlErrorPtr, XmlGenericErrorFunc, XmlParserErrors,
        },
        xmlmemory::{
            xml_mem_free, xml_mem_malloc, xml_mem_realloc, xml_mem_setup, xml_mem_used,
            xml_memory_dump, xml_memory_strdup,
        },
        xmlstring::{xml_strlen, XmlChar},
    },
    xml_error_with_format,
};
use libc::{
    free, glob, glob_t, globfree, memcpy, snprintf, strcmp, strdup, strlen, strncpy, GLOB_DOOFFS,
};

const OPT_SAX: i32 = 1 << 0;
const OPT_NO_SUBST: i32 = 1 << 1;

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

/************************************************************************
 *									*
 *		Huge document generator					*
 *									*
 ************************************************************************/

struct XmlHugeDocParts<'a> {
    url: &'a CStr,
    start: &'a CStr,
    segment: &'a CStr,
    finish: &'a CStr,
}

const HUGE_DOC_TABLE: &[XmlHugeDocParts] = &[
    XmlHugeDocParts{
        url: c"test/recurse/huge.xml",
        start: c"<!DOCTYPE foo [<!ELEMENT foo (bar*)> <!ELEMENT bar (#PCDATA)> <!ATTLIST bar attr CDATA #IMPLIED> <!ENTITY a SYSTEM 'ga.ent'> <!ENTITY b SYSTEM 'gb.ent'> <!ENTITY c SYSTEM 'gc.ent'> <!ENTITY f 'some internal data'> <!ENTITY e '&f;&f;'> <!ENTITY d '&e;&e;'> ]> <foo>",
        segment: c"  <bar attr='&e; &f; &d;'>&a; &b; &c; &e; &f; &d;</bar>\n  <bar>_123456789_123456789_123456789_123456789</bar>\n  <bar>_123456789_123456789_123456789_123456789</bar>\n  <bar>_123456789_123456789_123456789_123456789</bar>\n  <bar>_123456789_123456789_123456789_123456789</bar>\n",
        finish: c"</foo>"
    },
    XmlHugeDocParts{
        url: c"test/recurse/huge_dtd.dtd",
        start: c"<!ELEMENT foo (#PCDATA)>\n<!ENTITY ent 'success'>\n<!ENTITY % a SYSTEM 'pa.ent'>\n<!ENTITY % b SYSTEM 'pb.ent'>\n<!ENTITY % c SYSTEM 'pc.ent'>\n<!ENTITY % d '<!-- comment -->'>\n<!ENTITY % e '%d;%d;'>\n<!ENTITY % f '%e;%e;'>\n",
        segment: c"<!ENTITY ent '%a; %b; %c; %d; %e; %f;'>\n%a; %b; %c; %d; %e; %f;\n<!-- _123456789_123456789_123456789_123456789 -->\n<!-- _123456789_123456789_123456789_123456789 -->\n<!-- _123456789_123456789_123456789_123456789 -->\n",
        finish: c""
    }
];

static mut HUGE_DOC_PARTS: &XmlHugeDocParts<'static> = &HUGE_DOC_TABLE[0];
static mut CURSEG: usize = 0;
static mut CURRENT: AtomicPtr<c_char> = AtomicPtr::new(null_mut());
static mut RLEN: usize = 0;

/**
 * hugeMatch:
 * @URI: an URI to test
 *
 * Check for a huge query
 *
 * Returns 1 if yes and 0 if another Input module should be used
 */
unsafe extern "C" fn huge_match(uri: *const c_char) -> c_int {
    if uri.is_null() {
        return 0;
    }

    for doc in HUGE_DOC_TABLE {
        if strcmp(uri, doc.url.as_ptr()) == 0 {
            return 1;
        }
    }

    0
}

/**
 * hugeOpen:
 * @URI: an URI to test
 *
 * Return a pointer to the huge query handler, in this example simply
 * the current pointer...
 *
 * Returns an Input context or NULL in case or error
 */
unsafe extern "C" fn huge_open(uri: *const c_char) -> *mut c_void {
    if uri.is_null() {
        return null_mut();
    }

    for doc in HUGE_DOC_TABLE {
        if strcmp(uri, doc.url.as_ptr()) == 0 {
            HUGE_DOC_PARTS = doc;
            CURRENT.store(doc.start.as_ptr() as _, Ordering::Relaxed);
            CURSEG = 0;
            RLEN = doc.start.to_bytes().len();
            return CURRENT.load(Ordering::Relaxed) as _;
        }
    }

    null_mut()
}

/**
 * hugeClose:
 * @context: the read context
 *
 * Close the huge query handler
 *
 * Returns 0 or -1 in case of error
 */
unsafe extern "C" fn huge_close(context: *mut c_void) -> c_int {
    if context.is_null() {
        return -1;
    }
    0
}

const MAX_NODES: usize = 1000;

/**
 * hugeRead:
 * @context: the read context
 * @buffer: where to store data
 * @len: number of bytes to read
 *
 * Implement an huge query read.
 *
 * Returns the number of bytes read or -1 in case of error
 */
unsafe extern "C" fn huge_read(context: *mut c_void, buffer: *mut c_char, mut len: c_int) -> c_int {
    if context.is_null() || buffer.is_null() || len < 0 {
        return -1;
    }

    let mut current = CURRENT.load(Ordering::Acquire);
    if len as usize >= RLEN {
        if CURSEG > MAX_NODES {
            RLEN = 0;
            return 0;
        }
        len = RLEN as i32;
        RLEN = 0;
        memcpy(buffer as _, current as _, len as _);
        CURSEG += 1;
        if CURSEG == MAX_NODES {
            current = HUGE_DOC_PARTS.finish.as_ptr() as _;
        } else {
            current = HUGE_DOC_PARTS.segment.as_ptr() as _;
        }
        RLEN = strlen(current as _);
    } else {
        memcpy(buffer as _, current as _, len as _);
        RLEN -= len as usize;
        current = current.add(len as _);
    }
    CURRENT.store(current, Ordering::Release);
    len
}

/************************************************************************
 *									*
 *		Libxml2 specific routines				*
 *									*
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
        ret = xml_no_net_external_entity_loader(url, id, ctxt);
    } else {
        let memused: c_int = xml_mem_used();
        ret = xml_no_net_external_entity_loader(url, id, ctxt);
        EXTRA_MEMORY_FROM_RESOLVER += xml_mem_used() - memused;
    }

    ret
}

/*
 * Trapping the error messages at the generic level to grab the equivalent of
 * stderr messages on CLI tools.
 */
static mut TEST_ERRORS: [u8; 32769] = [0; 32769];
static mut TEST_ERRORS_SIZE: usize = 0;

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
 * xmlParserPrintFileContext:
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

    if input.is_null() {
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
        n - 1 < content.len() - 1
    } && cur > base
        && *cur != b'\n'
        && *cur != b'\r'
    {
        cur = cur.sub(1);
    }
    if *cur == b'\n' || *cur == b'\r' {
        cur = cur.add(1);
    }
    /* calculate the error position in terms of the current position */
    let col: usize = (*input).cur.offset_from(cur) as _;
    /* search forward for end-of-line (to max buff size) */
    n = 0;
    ctnt = content.as_mut_ptr();
    /* copy selected text to our buffer */
    while *cur != 0 && *cur != b'\n' && *cur != b'\r' && n < content.len() - 1 {
        *ctnt = *cur;
        ctnt = ctnt.add(1);
        cur = cur.add(1);
        n += 1;
    }
    *ctnt = 0;
    /* print out the selected text */
    xml_error_with_format!(chanl, data, c"%s\n".as_ptr(), content);
    /* create blank line with problem pointer */
    n = 0;
    ctnt = content.as_mut_ptr();
    /* (leave buffer space for pointer + line terminator) */
    while n < col
        && {
            n += 1;
            n - 1 < content.len() - 2
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
    xml_error_with_format!(chanl, data, c"%s\n".as_ptr(), content);
}

unsafe extern "C" fn test_structured_error_handler(_ctx: *mut c_void, err: XmlErrorPtr) {
    let data: *mut c_void = null_mut();
    let mut name: *const XmlChar = null();
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
            xml_error_with_format!(channel, data, c"parser ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromNamespace) => {
            xml_error_with_format!(channel, data, c"namespace ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromDtd) | Ok(XmlErrorDomain::XmlFromValid) => {
            xml_error_with_format!(channel, data, c"validity ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromHtml) => {
            xml_error_with_format!(channel, data, c"HTML parser ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromMemory) => {
            xml_error_with_format!(channel, data, c"memory ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromOutput) => {
            xml_error_with_format!(channel, data, c"output ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromIO) => {
            xml_error_with_format!(channel, data, c"I/O ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromXinclude) => {
            xml_error_with_format!(channel, data, c"XInclude ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromXpath) => {
            xml_error_with_format!(channel, data, c"XPath ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromXpointer) => {
            xml_error_with_format!(channel, data, c"parser ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromRegexp) => {
            xml_error_with_format!(channel, data, c"regexp ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromModule) => {
            xml_error_with_format!(channel, data, c"module ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromSchemasv) => {
            xml_error_with_format!(channel, data, c"Schemas validity ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromSchemasp) => {
            xml_error_with_format!(channel, data, c"Schemas parser ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromRelaxngp) => {
            xml_error_with_format!(channel, data, c"Relax-NG parser ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromRelaxngv) => {
            xml_error_with_format!(channel, data, c"Relax-NG validity ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromCatalog) => {
            xml_error_with_format!(channel, data, c"Catalog ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromC14N) => {
            xml_error_with_format!(channel, data, c"C14N ".as_ptr());
        }
        Ok(XmlErrorDomain::XmlFromXSLT) => {
            xml_error_with_format!(channel, data, c"XSLT ".as_ptr());
        }
        _ => {}
    }
    if code == XmlParserErrors::XmlErrOK as i32 {
        return;
    }
    match level {
        XmlErrorLevel::XmlErrNone => {
            xml_error_with_format!(channel, data, c": ".as_ptr());
        }
        XmlErrorLevel::XmlErrWarning => {
            xml_error_with_format!(channel, data, c"warning : ".as_ptr());
        }
        XmlErrorLevel::XmlErrError => {
            xml_error_with_format!(channel, data, c"error : ".as_ptr());
        }
        XmlErrorLevel::XmlErrFatal => {
            xml_error_with_format!(channel, data, c"error : ".as_ptr());
        }
    }
    if code == XmlParserErrors::XmlErrOK as i32 {
        return;
    }
    if !str.is_null() {
        let len: c_int = xml_strlen(str as _);
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
        && (*err).int1 < xml_strlen((*err).str1 as _)
    {
        let mut buf: [XmlChar; 150] = [0; 150];

        xml_error_with_format!(channel, data, c"%s\n".as_ptr(), (*err).str1);
        for b in buf.iter_mut().take((*err).int1 as usize) {
            *b = b' ';
        }
        buf[(*err).int1 as usize] = b'^';
        buf[(*err).int1 as usize + 1] = 0;
        xml_error_with_format!(channel, data, c"%s\n".as_ptr(), buf);
    }
}

unsafe extern "C" fn initialize_libxml2() {
    *xml_get_warnings_default_value() = 0;
    xml_pedantic_parser_default(0);

    xml_mem_setup(
        Some(xml_mem_free),
        Some(xml_mem_malloc),
        Some(xml_mem_realloc),
        Some(xml_memory_strdup),
    );
    xml_init_parser();
    xml_set_external_entity_loader(test_external_entity_loader);
    xmlSetStructuredErrorFunc(null_mut(), Some(test_structured_error_handler));
    /*
     * register the new I/O handlers
     */
    if xml_register_input_callbacks(
        Some(huge_match),
        Some(huge_open),
        Some(huge_read),
        Some(huge_close),
    ) < 0
    {
        eprintln!("failed to register Huge handler");
        exit(1);
    }
}

unsafe extern "C" fn init_sax(ctxt: XmlParserCtxtPtr) {
    (*(*ctxt).sax).start_element_ns = None;
    (*(*ctxt).sax).end_element_ns = None;
    (*(*ctxt).sax).characters = None;
    (*(*ctxt).sax).cdata_block = None;
    (*(*ctxt).sax).ignorable_whitespace = None;
    (*(*ctxt).sax).processing_instruction = None;
    (*(*ctxt).sax).comment = None;
}

/************************************************************************
 *									*
 *		File name and path utilities				*
 *									*
 ************************************************************************/

unsafe extern "C" fn base_filename(filename: *const c_char) -> *const c_char {
    let mut cur: *const c_char;
    if filename.is_null() {
        return null_mut();
    }
    cur = filename.add(strlen(filename));
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
    let suffixbuff: [c_char; 500] = [0; 500];

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

    strncpy(suffixbuff.as_ptr() as _, suffix, 499);

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

/************************************************************************
 *									*
 *		Test to detect or not recursive entities		*
 *									*
 ************************************************************************/
/**
 * recursiveDetectTest:
 * @filename: the file to parse
 * @result: the file with expected result
 * @err: the file with error messages: unused
 *
 * Parse a file loading DTD and replacing entities check it fails for
 * lol cases
 *
 * Returns 0 in case of success, an error code otherwise
 */
unsafe extern "C" fn recursive_detect_test(
    filename: *const c_char,
    _result: *const c_char,
    _err: *const c_char,
    options: c_int,
) -> c_int {
    let res: c_int = 0;
    /*
     * xmlParserOption::XML_PARSE_DTDVALID is the only way to load external entities
     * without xmlParserOption::XML_PARSE_NOENT. The validation result doesn't matter
     * anyway.
     */
    let mut parser_options: c_int = XmlParserOption::XmlParseDtdvalid as i32;

    NB_TESTS += 1;

    let ctxt: XmlParserCtxtPtr = xml_new_parser_ctxt();
    if options & OPT_SAX != 0 {
        init_sax(ctxt);
    }
    if options & OPT_NO_SUBST == 0 {
        parser_options |= XmlParserOption::XmlParseNoent as i32;
    }
    /*
     * base of the test, parse with the old API
     */
    let doc: XmlDocPtr = xml_ctxt_read_file(ctxt, filename, null_mut(), parser_options);
    if !doc.is_null() || (*ctxt).last_error.code != XmlParserErrors::XmlErrEntityLoop as i32 {
        eprintln!(
            "Failed to detect recursion in {}",
            CStr::from_ptr(filename).to_string_lossy()
        );
        xml_free_parser_ctxt(ctxt);
        xml_free_doc(doc);
        return 1;
    }
    xml_free_parser_ctxt(ctxt);

    res
}

/**
 * notRecursiveDetectTest:
 * @filename: the file to parse
 * @result: the file with expected result
 * @err: the file with error messages: unused
 *
 * Parse a file loading DTD and replacing entities check it works for
 * good cases
 *
 * Returns 0 in case of success, an error code otherwise
 */
unsafe extern "C" fn not_recursive_detect_test(
    filename: *const c_char,
    _result: *const c_char,
    _err: *const c_char,
    options: c_int,
) -> c_int {
    let res: c_int = 0;
    let mut parser_options: c_int = XmlParserOption::XmlParseDtdload as i32;

    NB_TESTS += 1;

    let ctxt: XmlParserCtxtPtr = xml_new_parser_ctxt();
    if options & OPT_SAX != 0 {
        init_sax(ctxt);
    }
    if options & OPT_NO_SUBST == 0 {
        parser_options |= XmlParserOption::XmlParseNoent as i32;
    }
    /*
     * base of the test, parse with the old API
     */
    let doc: XmlDocPtr = xml_ctxt_read_file(ctxt, filename, null_mut(), parser_options);
    if doc.is_null() {
        eprintln!(
            "Failed to parse correct file {}",
            CStr::from_ptr(filename).to_string_lossy()
        );
        xml_free_parser_ctxt(ctxt);
        return 1;
    }
    xml_free_doc(doc);
    xml_free_parser_ctxt(ctxt);

    res
}

/**
 * notRecursiveHugeTest:
 * @filename: the file to parse
 * @result: the file with expected result
 * @err: the file with error messages: unused
 *
 * Parse a memory generated file
 * good cases
 *
 * Returns 0 in case of success, an error code otherwise
 */
unsafe extern "C" fn not_recursive_huge_test(
    _filename: *const c_char,
    _result: *const c_char,
    _err: *const c_char,
    options: c_int,
) -> c_int {
    let mut res: c_int = 0;
    let mut parser_options: c_int = XmlParserOption::XmlParseDtdvalid as i32;

    NB_TESTS += 1;

    let ctxt: XmlParserCtxtPtr = xml_new_parser_ctxt();
    if options & OPT_SAX != 0 {
        init_sax(ctxt);
    }
    if options & OPT_NO_SUBST == 0 {
        parser_options |= XmlParserOption::XmlParseNoent as i32;
    }
    let doc: XmlDocPtr = xml_ctxt_read_file(
        ctxt,
        c"test/recurse/huge.xml".as_ptr(),
        null_mut(),
        parser_options,
    );
    if doc.is_null() {
        eprintln!("Failed to parse huge.xml");
        res = 1;
    } else {
        let mut ent: XmlEntityPtr;
        let fixed_cost: c_ulong = 20;
        let allowed_expansion: c_ulong = 1000000;
        let f_size: c_ulong = xml_strlen(c"some internal data".as_ptr() as _) as u64;

        ent = xml_get_doc_entity(doc, c"e".as_ptr() as _);
        let e_size: c_ulong =
            f_size * 2 + xml_strlen(c"&f;".as_ptr() as _) as u64 * 2 + fixed_cost * 2;
        if (*ent).expanded_size != e_size {
            eprintln!(
                "Wrong size for entity e: {} (expected {})",
                (*ent).expanded_size,
                e_size
            );
            res = 1;
        }

        ent = xml_get_doc_entity(doc, c"b".as_ptr() as _);
        if (*ent).expanded_size != e_size {
            eprintln!(
                "Wrong size for entity b: {} (expected {})",
                (*ent).expanded_size,
                e_size
            );
            res = 1;
        }

        ent = xml_get_doc_entity(doc, c"d".as_ptr() as _);
        let d_size: c_ulong =
            e_size * 2 + xml_strlen(c"&e;".as_ptr() as _) as u64 * 2 + fixed_cost * 2;
        if (*ent).expanded_size != d_size {
            eprintln!(
                "Wrong size for entity d: {} (expected {})",
                (*ent).expanded_size,
                d_size
            );
            res = 1;
        }

        ent = xml_get_doc_entity(doc, c"c".as_ptr() as _);
        if (*ent).expanded_size != d_size {
            eprintln!(
                "Wrong size for entity c: {} (expected {})",
                (*ent).expanded_size,
                d_size
            );
            res = 1;
        }

        if (*ctxt).sizeentcopy < allowed_expansion {
            eprintln!("Total entity size too small: {}", (*ctxt).sizeentcopy);
            res = 1;
        }

        let total_size: c_ulong =
            (f_size + e_size + d_size + 3 * fixed_cost) * (MAX_NODES as u64 - 1) * 3;
        if (*ctxt).sizeentcopy != total_size {
            eprintln!(
                "Wrong total entity size: {} (expected {})",
                (*ctxt).sizeentcopy,
                total_size
            );
            res = 1;
        }

        if (*ctxt).sizeentities != 30 {
            eprintln!(
                "Wrong parsed entity size: {} (expected {})",
                (*ctxt).sizeentities,
                30
            );
            res = 1;
        }
    }

    xml_free_doc(doc);
    xml_free_parser_ctxt(ctxt);

    res
}

/**
 * notRecursiveHugeTest:
 * @filename: the file to parse
 * @result: the file with expected result
 * @err: the file with error messages: unused
 *
 * Parse a memory generated file
 * good cases
 *
 * Returns 0 in case of success, an error code otherwise
 */
unsafe extern "C" fn huge_dtd_test(
    _filename: *const c_char,
    _result: *const c_char,
    _err: *const c_char,
    options: c_int,
) -> c_int {
    let mut res: c_int = 0;
    let mut parser_options: c_int = XmlParserOption::XmlParseDtdvalid as i32;

    NB_TESTS += 1;

    let ctxt: XmlParserCtxtPtr = xml_new_parser_ctxt();
    if options & OPT_SAX != 0 {
        init_sax(ctxt);
    }
    if options & OPT_NO_SUBST == 0 {
        parser_options |= XmlParserOption::XmlParseNoent as i32;
    }
    let doc: XmlDocPtr = xml_ctxt_read_file(
        ctxt,
        c"test/recurse/huge_dtd.xml".as_ptr(),
        null_mut(),
        parser_options,
    );
    if doc.is_null() {
        eprintln!("Failed to parse huge_dtd.xml");
        res = 1;
    } else {
        let fixed_cost: usize = 20;
        let allowed_expansion: usize = 1000000;
        let a_size: usize = xml_strlen(c"<!-- comment -->".as_ptr() as _) as usize;
        let mut total_size: usize;

        if (*ctxt).sizeentcopy < allowed_expansion as u64 {
            eprintln!("Total entity size too small: {}", (*ctxt).sizeentcopy);
            res = 1;
        }

        let b_size: usize = (a_size + c"&a;".to_bytes().len() + fixed_cost) * 2;
        let c_size: usize = (b_size + c"&b;".to_bytes().len() + fixed_cost) * 2;
        /*
         * Internal parameter entites are substitued eagerly and
         * need different accounting.
         */
        let e_size: usize = a_size * 2;
        let f_size: usize = e_size * 2;
        total_size = e_size + f_size + fixed_cost * 4 + (a_size + e_size + f_size + fixed_cost * 3) * (MAX_NODES - 1) * 2 + // internal
                     (a_size + b_size + c_size + fixed_cost * 3) * (MAX_NODES - 1) * 2 // external
                     + c"success".to_bytes().len() + fixed_cost; // final reference in main doc
        if (*ctxt).sizeentcopy != total_size as u64 {
            eprintln!(
                "Wrong total entity size: {} (expected {})",
                (*ctxt).sizeentcopy,
                total_size
            );
            res = 1;
        }

        total_size = HUGE_DOC_PARTS.start.to_bytes().len()
            + HUGE_DOC_PARTS.segment.to_bytes().len() * (MAX_NODES - 1)
            + HUGE_DOC_PARTS.finish.to_bytes().len()
            + 28;
        if (*ctxt).sizeentities != total_size as u64 {
            eprintln!(
                "Wrong parsed entity size: {} (expected {})",
                (*ctxt).sizeentities,
                total_size
            );
            res = 1;
        }
    }

    xml_free_doc(doc);
    xml_free_parser_ctxt(ctxt);

    res
}

/************************************************************************
 *									*
 *			Tests Descriptions				*
 *									*
 ************************************************************************/

static mut TEST_DESCRIPTIONS: [TestDesc; 11] = [
    TestDesc {
        desc: "Parsing recursive test cases",
        func: recursive_detect_test,
        input: Some(c"./test/recurse/lol*.xml"),
        out: None,
        suffix: None,
        err: None,
        options: 0,
    },
    TestDesc {
        desc: "Parsing recursive test cases (no substitution)",
        func: recursive_detect_test,
        input: Some(c"./test/recurse/lol*.xml"),
        out: None,
        suffix: None,
        err: None,
        options: OPT_NO_SUBST,
    },
    TestDesc {
        desc: "Parsing recursive test cases (SAX)",
        func: recursive_detect_test,
        input: Some(c"./test/recurse/lol*.xml"),
        out: None,
        suffix: None,
        err: None,
        options: OPT_SAX,
    },
    TestDesc {
        desc: "Parsing recursive test cases (SAX, no substitution)",
        func: recursive_detect_test,
        input: Some(c"./test/recurse/lol*.xml"),
        out: None,
        suffix: None,
        err: None,
        options: OPT_SAX | OPT_NO_SUBST,
    },
    TestDesc {
        desc: "Parsing non-recursive test cases",
        func: not_recursive_detect_test,
        input: Some(c"./test/recurse/good*.xml"),
        out: None,
        suffix: None,
        err: None,
        options: 0,
    },
    TestDesc {
        desc: "Parsing non-recursive test cases (SAX)",
        func: not_recursive_detect_test,
        input: Some(c"./test/recurse/good*.xml"),
        out: None,
        suffix: None,
        err: None,
        options: OPT_SAX,
    },
    TestDesc {
        desc: "Parsing non-recursive huge case",
        func: not_recursive_huge_test,
        input: None,
        out: None,
        suffix: None,
        err: None,
        options: 0,
    },
    TestDesc {
        desc: "Parsing non-recursive huge case (no substitution)",
        func: not_recursive_huge_test,
        input: None,
        out: None,
        suffix: None,
        err: None,
        options: OPT_NO_SUBST,
    },
    TestDesc {
        desc: "Parsing non-recursive huge case (SAX)",
        func: not_recursive_huge_test,
        input: None,
        out: None,
        suffix: None,
        err: None,
        options: OPT_SAX,
    },
    TestDesc {
        desc: "Parsing non-recursive huge case (SAX, no substitution)",
        func: not_recursive_huge_test,
        input: None,
        out: None,
        suffix: None,
        err: None,
        options: OPT_SAX | OPT_NO_SUBST,
    },
    TestDesc {
        desc: "Parsing non-recursive huge DTD case",
        func: huge_dtd_test,
        input: None,
        out: None,
        suffix: None,
        err: None,
        options: 0,
    },
];

/************************************************************************
 *									*
 *		The main code driving the tests				*
 *									*
 ************************************************************************/

unsafe extern "C" fn launch_tests(tst: &TestDesc) -> c_int {
    let mut res: c_int;
    let mut err: c_int = 0;
    let mut result: *mut c_char;
    let mut error: *mut c_char;
    let mut mem: c_int;

    if let Some(input) = tst.input {
        let mut globbuf: glob_t = unsafe { zeroed() };

        globbuf.gl_offs = 0;
        glob(input.as_ptr(), GLOB_DOOFFS, None, addr_of_mut!(globbuf));
        for i in 0..globbuf.gl_pathc {
            if check_test_file(*globbuf.gl_pathv.add(i)) == 0 {
                continue;
            }
            if let Some(suffix) = tst.suffix {
                result = result_filename(
                    *globbuf.gl_pathv.add(i),
                    tst.out.map_or(null_mut(), |s| s.as_ptr()),
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
                    tst.out.map_or(null_mut(), |s| s.as_ptr()),
                    err.as_ptr(),
                );
                if error.is_null() {
                    eprintln!("Out of memory !");
                    fatal_error();
                }
            } else {
                error = null_mut();
            }
            if !result.is_null() && check_test_file(result) == 0 {
                eprintln!(
                    "Missing result file {}",
                    CStr::from_ptr(result).to_string_lossy()
                );
            } else if !error.is_null() && check_test_file(error) == 0 {
                eprintln!(
                    "Missing error file {}",
                    CStr::from_ptr(error).to_string_lossy()
                );
            } else {
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
                        CStr::from_ptr(*globbuf.gl_pathv.add(i)).to_string_lossy()
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
                        xml_mem_used() - mem
                    );
                    NB_LEAKS += 1;
                    err += 1;
                }
                TEST_ERRORS_SIZE = 0;
            }
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
                NB_LEAKS - old_leaks
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

        for arg in args().skip(1) {
            if arg == "-v" {
                VERBOSE = 1;
            } else if arg == "-quiet" {
                TESTS_QUIET = 1;
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
                NB_TESTS, NB_ERRORS, NB_LEAKS
            );
        }
        xml_cleanup_parser();
        xml_memory_dump();
    }

    assert_eq!(ret, 0);
}
