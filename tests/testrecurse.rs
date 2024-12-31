//! Rust implementation of original libxml2's `testrecurse.c`.  
//! If you want this to work, copy the `test/` and `result/` directories from the original libxml2.

use std::{
    env::args,
    ffi::{c_char, c_int, c_ulong, c_void, CStr},
    fs::metadata,
    io::{self, Read},
    mem::zeroed,
    process::exit,
    ptr::{addr_of_mut, null_mut},
    sync::atomic::{AtomicPtr, Ordering},
};

use exml::{
    error::{
        parser_print_file_context_internal, XmlError, XmlErrorDomain, XmlErrorLevel,
        XmlParserErrors,
    },
    globals::{
        reset_last_error, set_get_warnings_default_value, set_pedantic_parser_default_value,
        set_structured_error, GenericErrorContext,
    },
    io::{register_input_callbacks, xml_no_net_external_entity_loader, XmlInputCallback},
    libxml::{
        entities::{xml_get_doc_entity, XmlEntityPtr},
        parser::{
            xml_cleanup_parser, xml_ctxt_read_file, xml_free_parser_ctxt, xml_init_parser,
            xml_new_parser_ctxt, xml_set_external_entity_loader, XmlParserCtxtPtr,
            XmlParserInputPtr, XmlParserOption,
        },
        xmlmemory::{
            xml_mem_free, xml_mem_malloc, xml_mem_realloc, xml_mem_setup, xml_mem_used,
            xml_memory_dump, xml_memory_strdup,
        },
        xmlstring::{xml_strlen, XmlChar},
    },
    tree::{xml_free_doc, NodeCommon, XmlDocPtr, XmlElementType},
};
use libc::{free, glob, glob_t, globfree, memcpy, snprintf, strdup, strlen, strncpy, GLOB_DOOFFS};

const OPT_SAX: i32 = 1 << 0;
const OPT_NO_SUBST: i32 = 1 << 1;

type Functest =
    unsafe fn(filename: &str, result: *const c_char, error: *const c_char, options: c_int) -> c_int;

struct TestDesc<'a> {
    desc: &'a str,            /* description of the test */
    func: Functest,           /* function implementing the test */
    input: Option<&'a CStr>,  /* glob to path for input files */
    out: Option<&'a CStr>,    /* output directory */
    suffix: Option<&'a CStr>, /* suffix for output files */
    err: Option<&'a CStr>,    /* suffix for error output files */
    options: c_int,           /* parser options for the test */
}

struct XmlHugeDocParts<'a> {
    url: &'a str,
    start: &'a CStr,
    segment: &'a CStr,
    finish: &'a CStr,
}

const HUGE_DOC_TABLE: &[XmlHugeDocParts] = &[
    XmlHugeDocParts{
        url: "test/recurse/huge.xml",
        start: c"<!DOCTYPE foo [<!ELEMENT foo (bar*)> <!ELEMENT bar (#PCDATA)> <!ATTLIST bar attr CDATA #IMPLIED> <!ENTITY a SYSTEM 'ga.ent'> <!ENTITY b SYSTEM 'gb.ent'> <!ENTITY c SYSTEM 'gc.ent'> <!ENTITY f 'some internal data'> <!ENTITY e '&f;&f;'> <!ENTITY d '&e;&e;'> ]> <foo>",
        segment: c"  <bar attr='&e; &f; &d;'>&a; &b; &c; &e; &f; &d;</bar>\n  <bar>_123456789_123456789_123456789_123456789</bar>\n  <bar>_123456789_123456789_123456789_123456789</bar>\n  <bar>_123456789_123456789_123456789_123456789</bar>\n  <bar>_123456789_123456789_123456789_123456789</bar>\n",
        finish: c"</foo>"
    },
    XmlHugeDocParts{
        url: "test/recurse/huge_dtd.dtd",
        start: c"<!ELEMENT foo (#PCDATA)>\n<!ENTITY ent 'success'>\n<!ENTITY % a SYSTEM 'pa.ent'>\n<!ENTITY % b SYSTEM 'pb.ent'>\n<!ENTITY % c SYSTEM 'pc.ent'>\n<!ENTITY % d '<!-- comment -->'>\n<!ENTITY % e '%d;%d;'>\n<!ENTITY % f '%e;%e;'>\n",
        segment: c"<!ENTITY ent '%a; %b; %c; %d; %e; %f;'>\n%a; %b; %c; %d; %e; %f;\n<!-- _123456789_123456789_123456789_123456789 -->\n<!-- _123456789_123456789_123456789_123456789 -->\n<!-- _123456789_123456789_123456789_123456789 -->\n",
        finish: c""
    }
];

static mut HUGE_DOC_PARTS: &XmlHugeDocParts<'static> = &HUGE_DOC_TABLE[0];
static mut CURSEG: usize = 0;
static mut CURRENT: AtomicPtr<c_char> = AtomicPtr::new(null_mut());
static mut RLEN: usize = 0;

/// Check for a huge query
///
/// Returns 1 if yes and 0 if another Input module should be used
#[doc(alias = "hugeMatch")]
fn huge_match(uri: &str) -> c_int {
    for doc in HUGE_DOC_TABLE {
        if uri == doc.url {
            return 1;
        }
    }

    0
}

/// Return a pointer to the huge query handler, in this example simply
/// the current pointer...
///
/// Returns an Input context or NULL in case or error
#[doc(alias = "hugeOpen")]
unsafe fn huge_open(uri: &str) -> *mut c_void {
    for doc in HUGE_DOC_TABLE {
        if uri == doc.url {
            HUGE_DOC_PARTS = doc;
            CURRENT.store(doc.start.as_ptr() as _, Ordering::Relaxed);
            CURSEG = 0;
            RLEN = doc.start.to_bytes().len();
            return CURRENT.load(Ordering::Relaxed) as _;
        }
    }

    null_mut()
}

/// Close the huge query handler
///
/// Returns 0 or -1 in case of error
#[doc(alias = "hugeClose")]
unsafe extern "C" fn huge_close(context: *mut c_void) -> c_int {
    if context.is_null() {
        return -1;
    }
    0
}

const MAX_NODES: usize = 1000;

/// Implement an huge query read.
///
/// Returns the number of bytes read or -1 in case of error
#[doc(alias = "hugeRead")]
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

static mut NB_TESTS: c_int = 0;
static mut NB_ERRORS: c_int = 0;
static mut NB_LEAKS: c_int = 0;
static mut EXTRA_MEMORY_FROM_RESOLVER: c_int = 0;

unsafe extern "C" fn fatal_error() -> c_int {
    eprintln!("Exitting tests on fatal error");
    exit(1);
}

// We need to trap calls to the resolver to not account memory for the catalog
// which is shared to the current running test. We also don't want to have
// network downloads modifying tests.
unsafe fn test_external_entity_loader(
    url: Option<&str>,
    id: Option<&str>,
    ctxt: XmlParserCtxtPtr,
) -> XmlParserInputPtr {
    let ret: XmlParserInputPtr;

    if check_test_file(url.unwrap()) != 0 {
        ret = xml_no_net_external_entity_loader(url, id, ctxt);
    } else {
        let memused: c_int = xml_mem_used();
        ret = xml_no_net_external_entity_loader(url, id, ctxt);
        EXTRA_MEMORY_FROM_RESOLVER += xml_mem_used() - memused;
    }

    ret
}

// Trapping the error messages at the generic level to grab the equivalent of
// stderr messages on CLI tools.
static mut TEST_ERRORS: [u8; 32769] = [0; 32769];
static mut TEST_ERRORS_SIZE: usize = 0;

fn channel(_ctx: Option<GenericErrorContext>, msg: &str) {
    unsafe {
        if TEST_ERRORS_SIZE >= 32768 {
            return;
        }
        if TEST_ERRORS_SIZE + msg.len() >= TEST_ERRORS.len() {
            TEST_ERRORS[TEST_ERRORS_SIZE..]
                .copy_from_slice(&msg.as_bytes()[..TEST_ERRORS.len() - TEST_ERRORS_SIZE]);
            TEST_ERRORS_SIZE = TEST_ERRORS.len();
        } else {
            TEST_ERRORS[TEST_ERRORS_SIZE..TEST_ERRORS_SIZE + msg.len()]
                .copy_from_slice(msg.as_bytes());
            TEST_ERRORS_SIZE += msg.len();
        }
        TEST_ERRORS[TEST_ERRORS_SIZE] = 0;
    }
}

fn test_structured_error_handler(_ctx: Option<GenericErrorContext>, err: &XmlError) {
    let mut name: *const XmlChar = null_mut();
    let mut input: XmlParserInputPtr = null_mut();
    let mut cur: XmlParserInputPtr = null_mut();
    let mut ctxt: XmlParserCtxtPtr = null_mut();

    let file = err.file();
    let line = err.line();
    let code = err.code();
    let domain = err.domain();
    let level = err.level();
    let node = err.node();
    if domain == XmlErrorDomain::XmlFromParser
        || domain == XmlErrorDomain::XmlFromHTML
        || domain == XmlErrorDomain::XmlFromDTD
        || domain == XmlErrorDomain::XmlFromNamespace
        || domain == XmlErrorDomain::XmlFromIO
        || domain == XmlErrorDomain::XmlFromValid
    {
        ctxt = err.context().map_or(null_mut(), |c| c.as_ptr() as _);
    }
    let str = err.message();

    if code.is_ok() {
        return;
    }

    unsafe {
        if let Some(node) =
            node.filter(|n| n.as_ref().element_type() == XmlElementType::XmlElementNode)
        {
            name = node.as_ref().name;
        }

        // Maintain the compatibility with the legacy error handling
        if !ctxt.is_null() {
            input = (*ctxt).input;
            if !input.is_null() && (*input).filename.is_none() && (*ctxt).input_tab.len() > 1 {
                cur = input;
                input = (*ctxt).input_tab[(*ctxt).input_tab.len() - 2];
            }
            if !input.is_null() {
                if let Some(filename) = (*input).filename.as_deref() {
                    channel(None, format!("{filename}:{}: ", (*input).line).as_str());
                } else if line != 0 && domain == XmlErrorDomain::XmlFromParser {
                    channel(None, format!("Entity: line {}: ", (*input).line).as_str());
                }
            }
        } else if let Some(file) = file {
            channel(None, format!("{file}:{line}: ").as_str());
        } else if line != 0 && domain == XmlErrorDomain::XmlFromParser {
            channel(None, format!("Entity: line {line}: ").as_str());
        }
        if !name.is_null() {
            let name = CStr::from_ptr(name as *const i8).to_string_lossy();
            channel(None, format!("element {name}: ").as_str());
        }
        if code.is_ok() {
            return;
        }
        match domain {
            XmlErrorDomain::XmlFromParser => channel(None, "parser "),
            XmlErrorDomain::XmlFromNamespace => channel(None, "namespace "),
            XmlErrorDomain::XmlFromDTD | XmlErrorDomain::XmlFromValid => channel(None, "validity "),
            XmlErrorDomain::XmlFromHTML => channel(None, "HTML parser "),
            XmlErrorDomain::XmlFromMemory => channel(None, "memory "),
            XmlErrorDomain::XmlFromOutput => channel(None, "output "),
            XmlErrorDomain::XmlFromIO => channel(None, "I/O "),
            XmlErrorDomain::XmlFromXInclude => channel(None, "XInclude "),
            XmlErrorDomain::XmlFromXPath => channel(None, "XPath "),
            XmlErrorDomain::XmlFromXPointer => channel(None, "parser "),
            XmlErrorDomain::XmlFromRegexp => channel(None, "regexp "),
            XmlErrorDomain::XmlFromModule => channel(None, "module "),
            XmlErrorDomain::XmlFromSchemasv => channel(None, "Schemas validity "),
            XmlErrorDomain::XmlFromSchemasp => channel(None, "Schemas parser "),
            XmlErrorDomain::XmlFromRelaxngp => channel(None, "Relax-NG parser "),
            XmlErrorDomain::XmlFromRelaxngv => channel(None, "Relax-NG validity "),
            XmlErrorDomain::XmlFromCatalog => channel(None, "Catalog "),
            XmlErrorDomain::XmlFromC14N => channel(None, "C14N "),
            XmlErrorDomain::XmlFromXSLT => channel(None, "XSLT "),
            _ => {}
        }
        if code.is_ok() {
            return;
        }
        match level {
            XmlErrorLevel::XmlErrNone => channel(None, ": "),
            XmlErrorLevel::XmlErrWarning => channel(None, "warning : "),
            XmlErrorLevel::XmlErrError => channel(None, "error : "),
            XmlErrorLevel::XmlErrFatal => channel(None, "error : "),
        }
        if code.is_ok() {
            return;
        }
        if let Some(str) = str {
            if str.as_bytes().last() != Some(&b'\n') {
                channel(None, format!("{str}\n").as_str());
            } else {
                channel(None, str);
            }
        } else {
            channel(None, "out of memory error\n");
        }
        if code.is_ok() {
            return;
        }

        if !ctxt.is_null() {
            parser_print_file_context_internal(input, channel, None);
            if !cur.is_null() {
                if let Some(filename) = (*cur).filename.as_deref() {
                    channel(None, format!("{filename}:{}: \n", (*cur).line).as_str());
                } else if line != 0 && domain == XmlErrorDomain::XmlFromParser {
                    channel(None, format!("Entity: line {}: \n", (*cur).line).as_str());
                }
                parser_print_file_context_internal(cur, channel, None);
            }
        }
        if let Some(str1) = err.str1().filter(|s| {
            err.int1() < 100
                && err.int1() < s.len() as i32
                && domain == XmlErrorDomain::XmlFromXPath
        }) {
            channel(None, format!("{str1}\n").as_str());
            let mut buf = " ".repeat(err.int1() as usize);
            buf.push('^');
            channel(None, format!("{buf}\n").as_str());
        }
    }
}

unsafe extern "C" fn initialize_libxml2() {
    set_get_warnings_default_value(0);
    set_pedantic_parser_default_value(0);

    xml_mem_setup(
        Some(xml_mem_free),
        Some(xml_mem_malloc),
        Some(xml_mem_realloc),
        Some(xml_memory_strdup),
    );
    xml_init_parser();
    xml_set_external_entity_loader(test_external_entity_loader);
    set_structured_error(Some(test_structured_error_handler), None);
    // register the new I/O handlers
    struct HugeTestIO(*mut c_void);
    impl Read for HugeTestIO {
        fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
            let res = unsafe { huge_read(self.0, buf.as_mut_ptr() as *mut i8, buf.len() as i32) };
            if res < 0 {
                Err(io::Error::last_os_error())
            } else {
                Ok(res as usize)
            }
        }
    }
    unsafe impl Send for HugeTestIO {}
    impl Drop for HugeTestIO {
        fn drop(&mut self) {
            if !self.0.is_null() {
                unsafe {
                    huge_close(self.0);
                }
            }
        }
    }
    impl XmlInputCallback for HugeTestIO {
        fn is_match(&self, filename: &str) -> bool {
            huge_match(filename) != 0
        }
        fn open(&mut self, filename: &str) -> std::io::Result<Box<dyn Read>> {
            let ptr = unsafe { huge_open(filename) };
            if ptr.is_null() {
                Err(io::Error::other("Failed to execute huge_open"))
            } else {
                Ok(Box::new(Self(ptr)))
            }
        }
    }
    if register_input_callbacks(HugeTestIO(null_mut())).is_err() {
        eprintln!("failed to register Huge handler");
        exit(1);
    }
}

unsafe fn init_sax(ctxt: XmlParserCtxtPtr) {
    if let Some(sax) = (*ctxt).sax.as_deref_mut() {
        sax.start_element_ns = None;
        sax.end_element_ns = None;
        sax.characters = None;
        sax.cdata_block = None;
        sax.ignorable_whitespace = None;
        sax.processing_instruction = None;
        sax.comment = None;
    }
}

unsafe fn base_filename(filename: *const c_char) -> *const c_char {
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

unsafe fn result_filename(
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

fn check_test_file(filename: &str) -> c_int {
    match metadata(filename) {
        Ok(meta) => meta.is_file() as i32,
        _ => 0,
    }
}

/// Parse a file loading DTD and replacing entities check it fails for lol cases
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "recursiveDetectTest")]
unsafe fn recursive_detect_test(
    filename: &str,
    _result: *const c_char,
    _err: *const c_char,
    options: c_int,
) -> c_int {
    let res: c_int = 0;
    // xmlParserOption::XML_PARSE_DTDVALID is the only way to load external entities
    // without xmlParserOption::XML_PARSE_NOENT. The validation result doesn't matter anyway.
    let mut parser_options: c_int = XmlParserOption::XmlParseDtdvalid as i32;

    NB_TESTS += 1;

    let ctxt: XmlParserCtxtPtr = xml_new_parser_ctxt();
    if options & OPT_SAX != 0 {
        init_sax(ctxt);
    }
    if options & OPT_NO_SUBST == 0 {
        parser_options |= XmlParserOption::XmlParseNoent as i32;
    }
    // base of the test, parse with the old API
    let doc: XmlDocPtr = xml_ctxt_read_file(ctxt, filename, None, parser_options);
    if !doc.is_null() || (*ctxt).last_error.code() != XmlParserErrors::XmlErrEntityLoop {
        eprintln!("Failed to detect recursion in {filename}");
        xml_free_parser_ctxt(ctxt);
        xml_free_doc(doc);
        return 1;
    }
    xml_free_parser_ctxt(ctxt);

    res
}

/// Parse a file loading DTD and replacing entities check it works for
/// good cases
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "notRecursiveDetectTest")]
unsafe fn not_recursive_detect_test(
    filename: &str,
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
    // base of the test, parse with the old API
    let doc: XmlDocPtr = xml_ctxt_read_file(ctxt, filename, None, parser_options);
    if doc.is_null() {
        eprintln!("Failed to parse correct file {filename}");
        xml_free_parser_ctxt(ctxt);
        return 1;
    }
    xml_free_doc(doc);
    xml_free_parser_ctxt(ctxt);

    res
}

/// Parse a memory generated file good cases
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "notRecursiveHugeTest")]
unsafe fn not_recursive_huge_test(
    _filename: &str,
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
    let doc: XmlDocPtr = xml_ctxt_read_file(ctxt, "test/recurse/huge.xml", None, parser_options);
    if doc.is_null() {
        eprintln!("Failed to parse huge.xml");
        res = 1;
    } else {
        let mut ent: XmlEntityPtr;
        let fixed_cost: c_ulong = 20;
        let allowed_expansion: c_ulong = 1000000;
        let f_size: c_ulong = xml_strlen(c"some internal data".as_ptr() as _) as u64;

        ent = xml_get_doc_entity(doc, "e");
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

        ent = xml_get_doc_entity(doc, "b");
        if (*ent).expanded_size != e_size {
            eprintln!(
                "Wrong size for entity b: {} (expected {})",
                (*ent).expanded_size,
                e_size
            );
            res = 1;
        }

        ent = xml_get_doc_entity(doc, "d");
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

        ent = xml_get_doc_entity(doc, "c");
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

/// Parse a memory generated file good cases
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "notRecursiveHugeTest")]
unsafe fn huge_dtd_test(
    _filename: &str,
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
    let doc: XmlDocPtr =
        xml_ctxt_read_file(ctxt, "test/recurse/huge_dtd.xml", None, parser_options);
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
        // Internal parameter entites are substitued eagerly and
        // need different accounting.
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

unsafe fn launch_tests(tst: &TestDesc) -> c_int {
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
            let filename = CStr::from_ptr(*globbuf.gl_pathv.add(i)).to_string_lossy();
            if check_test_file(&filename) == 0 {
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
            if !result.is_null()
                && check_test_file(CStr::from_ptr(result).to_string_lossy().as_ref()) == 0
            {
                eprintln!(
                    "Missing result file {}",
                    CStr::from_ptr(result).to_string_lossy()
                );
            } else if !error.is_null()
                && check_test_file(CStr::from_ptr(error).to_string_lossy().as_ref()) == 0
            {
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
                    &filename,
                    result,
                    error,
                    tst.options | XmlParserOption::XmlParseCompact as i32,
                );
                reset_last_error();
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
        res = (tst.func)("", null_mut(), null_mut(), tst.options);
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
