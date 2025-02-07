//! Rust implementation of original libxml2's `runsuite.c`.  
//! If you want this to work, copy the `test/` and `result/` directories from the original libxml2.

use std::{
    env::args,
    ffi::{c_char, c_int, CStr, CString},
    fs::{metadata, File},
    ops::DerefMut,
    ptr::null_mut,
    sync::atomic::{AtomicPtr, Ordering},
};

use exml::{
    globals::{
        reset_last_error, set_generic_error, set_get_warnings_default_value,
        set_pedantic_parser_default_value, GenericErrorContext,
    },
    io::xml_no_net_external_entity_loader,
    libxml::{
        globals::xml_free,
        parser::{
            xml_cleanup_parser, xml_init_parser, xml_set_external_entity_loader, XmlParserOption,
        },
        relaxng::{
            xml_relaxng_free, xml_relaxng_parse, xml_relaxng_set_valid_errors,
            xml_relaxng_validate_doc, XmlRelaxNGPtr,
        },
        xmlmemory::{
            xml_mem_free, xml_mem_malloc, xml_mem_realloc, xml_mem_setup, xml_mem_used,
            xml_memory_dump, xml_memory_strdup,
        },
        xmlschemas::{
            xml_schema_free, xml_schema_free_parser_ctxt, xml_schema_free_valid_ctxt,
            xml_schema_new_parser_ctxt, xml_schema_new_valid_ctxt, xml_schema_parse,
            xml_schema_set_parser_errors, xml_schema_set_valid_errors, xml_schema_validate_doc,
            XmlSchemaParserCtxtPtr, XmlSchemaPtr, XmlSchemaValidCtxtPtr,
        },
        xmlschemastypes::xml_schema_init_types,
        xmlstring::{xml_str_equal, xml_strdup, xml_strndup, XmlChar},
    },
    parser::{
        xml_new_string_input_stream, xml_read_file, xml_read_memory, XmlParserCtxtPtr,
        XmlParserInputPtr,
    },
    relaxng::{
        xml_relaxng_free_parser_ctxt, xml_relaxng_free_valid_ctxt, xml_relaxng_init_types,
        xml_relaxng_new_mem_parser_ctxt, xml_relaxng_new_valid_ctxt, XmlRelaxNGValidCtxtPtr,
    },
    tree::{xml_free_doc, XmlDocPtr, XmlNode},
    uri::build_uri,
    xpath::{
        internals::xml_xpath_register_ns, xml_xpath_compile, xml_xpath_compiled_eval,
        xml_xpath_context_set_cache, xml_xpath_free_comp_expr, xml_xpath_free_context,
        xml_xpath_free_object, xml_xpath_new_context, XmlXPathCompExprPtr, XmlXPathContext,
        XmlXPathObjectPtr, XmlXPathObjectType,
    },
};
use libc::{snprintf, strcmp, strstr};

static mut VERBOSE: c_int = 0;

fn check_test_file(filename: &str) -> bool {
    match metadata(filename) {
        Ok(meta) => meta.is_file(),
        _ => false,
    }
}

unsafe fn compose_dir(dir: *const XmlChar, path: *const XmlChar) -> *mut XmlChar {
    let mut buf: [c_char; 500] = [0; 500];

    if dir.is_null() {
        return xml_strdup(path);
    }
    if path.is_null() {
        return null_mut();
    }

    snprintf(
        buf.as_mut_ptr(),
        500,
        c"%s/%s".as_ptr(),
        dir as *const c_char,
        path as *const c_char,
    );
    xml_strdup(buf.as_ptr() as *const XmlChar)
}

static mut NB_TESTS: c_int = 0;
static mut NB_ERRORS: c_int = 0;
static mut NB_INTERNALS: c_int = 0;
static mut NB_SCHEMATAS: c_int = 0;
static mut NB_UNIMPLEMENTED: c_int = 0;
static mut NB_LEAKS: c_int = 0;
static mut EXTRA_MEMORY_FROM_RESOLVER: c_int = 0;

const MAX_ENTITIES: usize = 20;
static mut TEST_ENTITIES_NAME: [*mut c_char; MAX_ENTITIES] = [null_mut(); MAX_ENTITIES];
static mut TEST_ENTITIES_VALUE: [*mut c_char; MAX_ENTITIES] = [null_mut(); MAX_ENTITIES];
static mut NB_ENTITIES: usize = 0;
unsafe fn reset_entities() {
    for i in 0..NB_ENTITIES {
        if !TEST_ENTITIES_NAME[i].is_null() {
            xml_free(TEST_ENTITIES_NAME[i] as _);
        }
        if !TEST_ENTITIES_VALUE[i].is_null() {
            xml_free(TEST_ENTITIES_VALUE[i] as _);
        }
    }
    NB_ENTITIES = 0;
}
unsafe fn add_entity(name: *mut c_char, content: *mut c_char) -> c_int {
    if NB_ENTITIES >= MAX_ENTITIES {
        eprintln!("Too many entities defined");
        return -1;
    }
    TEST_ENTITIES_NAME[NB_ENTITIES] = name;
    TEST_ENTITIES_VALUE[NB_ENTITIES] = content;
    NB_ENTITIES += 1;
    0
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

    for i in 0..NB_ENTITIES {
        let url = CString::new(url.unwrap()).unwrap();
        if strcmp(TEST_ENTITIES_NAME[i], url.as_ptr()) == 0 {
            ret = xml_new_string_input_stream(
                (!ctxt.is_null()).then(|| &mut *ctxt),
                &CStr::from_ptr(TEST_ENTITIES_VALUE[i]).to_string_lossy(),
            );
            if !ret.is_null() {
                (*ret).filename = Some(
                    CStr::from_ptr(TEST_ENTITIES_NAME[i])
                        .to_string_lossy()
                        .into_owned(),
                );
            }
            return ret;
        }
    }
    if check_test_file(url.unwrap()) {
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

macro_rules! test_log {
    ( $logfile:expr, $msg:literal, $( $args:expr ),* ) => {
        if let Some(logfile) = $logfile.as_mut() {
            use std::io::Write;
            writeln!(logfile, "\n------------").ok();
            write!(logfile, $msg, $( $args ),*).ok();
            write!(logfile, "{}", CStr::from_ptr(TEST_ERRORS.as_ptr() as _).to_string_lossy()).ok();
            TEST_ERRORS_SIZE = 0;
            TEST_ERRORS[0] = 0;
        }

        if VERBOSE != 0 {
            $(
                eprint!("{}", $args);
            )*
        }
    };
}

fn test_error_handler(_ctx: Option<GenericErrorContext>, msg: &str) {
    unsafe {
        if TEST_ERRORS_SIZE >= 32768 {
            return;
        }

        if TEST_ERRORS_SIZE + msg.len() >= 32768 {
            TEST_ERRORS[TEST_ERRORS_SIZE..]
                .copy_from_slice(&msg.as_bytes()[..TEST_ERRORS.len() - TEST_ERRORS_SIZE]);
            /* buffer is full */
            TEST_ERRORS_SIZE = 32768;
            TEST_ERRORS[TEST_ERRORS_SIZE] = 0;
        } else {
            TEST_ERRORS[TEST_ERRORS_SIZE..TEST_ERRORS_SIZE + msg.len()]
                .copy_from_slice(msg.as_bytes());
            TEST_ERRORS_SIZE += msg.len();
        }
        TEST_ERRORS[TEST_ERRORS_SIZE] = 0;
    }
}

static CTXT_XPATH: AtomicPtr<XmlXPathContext> = AtomicPtr::new(null_mut());

unsafe fn initialize_libxml2() {
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
    CTXT_XPATH.store(xml_xpath_new_context(null_mut()), Ordering::Relaxed);
    // Deactivate the cache if created; otherwise we have to create/free it
    // for every test, since it will confuse the memory leak detection.
    // Note that normally this need not be done, since the cache is not
    // created until set explicitly with xmlXPathContextSetCache();
    // but for test purposes it is sometimes useful to activate the
    // cache by default for the whole library.
    if !(*CTXT_XPATH.load(Ordering::Relaxed)).cache.is_null() {
        xml_xpath_context_set_cache(CTXT_XPATH.load(Ordering::Relaxed), 0, -1, 0);
    }
    // used as default namespace in xstc tests
    xml_xpath_register_ns(
        CTXT_XPATH.load(Ordering::Relaxed),
        c"ts".as_ptr() as _,
        c"TestSuite".as_ptr() as _,
    );
    xml_xpath_register_ns(
        CTXT_XPATH.load(Ordering::Relaxed),
        c"xlink".as_ptr() as _,
        c"http://www.w3.org/1999/xlink".as_ptr() as _,
    );
    set_generic_error(Some(test_error_handler), None);
    #[cfg(feature = "schema")]
    {
        xml_schema_init_types();
        xml_relaxng_init_types();
    }
}

unsafe fn get_next(cur: *mut XmlNode, xpath: *const c_char) -> *mut XmlNode {
    let mut ret: *mut XmlNode = null_mut();

    if cur.is_null() || (*cur).doc.is_null() || xpath.is_null() {
        return null_mut();
    }
    (*CTXT_XPATH.load(Ordering::Relaxed)).doc = (*cur).doc;
    (*CTXT_XPATH.load(Ordering::Relaxed)).node = cur;
    let comp: XmlXPathCompExprPtr = xml_xpath_compile(xpath as _);
    if comp.is_null() {
        eprintln!(
            "Failed to compile {}",
            CStr::from_ptr(xpath).to_string_lossy()
        );
        return null_mut();
    }
    let res: XmlXPathObjectPtr = xml_xpath_compiled_eval(comp, CTXT_XPATH.load(Ordering::Relaxed));
    xml_xpath_free_comp_expr(comp);
    if res.is_null() {
        return null_mut();
    }
    if (*res).typ == XmlXPathObjectType::XPathNodeset {
        if let Some(nodeset) = (*res).nodesetval.as_deref() {
            if !nodeset.node_tab.is_empty() {
                ret = nodeset.node_tab[0];
            }
        }
    }
    xml_xpath_free_object(res);
    ret
}

unsafe fn get_string(cur: *mut XmlNode, xpath: *const c_char) -> *mut XmlChar {
    let mut ret: *mut XmlChar = null_mut();

    if cur.is_null() || (*cur).doc.is_null() || xpath.is_null() {
        return null_mut();
    }
    (*CTXT_XPATH.load(Ordering::Relaxed)).doc = (*cur).doc;
    (*CTXT_XPATH.load(Ordering::Relaxed)).node = cur;
    let comp: XmlXPathCompExprPtr = xml_xpath_compile(xpath as _);
    if comp.is_null() {
        eprintln!(
            "Failed to compile {}",
            CStr::from_ptr(xpath).to_string_lossy()
        );
        return null_mut();
    }
    let res: XmlXPathObjectPtr = xml_xpath_compiled_eval(comp, CTXT_XPATH.load(Ordering::Relaxed));
    xml_xpath_free_comp_expr(comp);
    if res.is_null() {
        return null_mut();
    }
    if (*res).typ == XmlXPathObjectType::XPathString {
        let res = (*res).stringval.take().map(|s| CString::new(s).unwrap());
        ret = res
            .as_deref()
            .map_or(null_mut(), |s| xml_strdup(s.as_ptr() as *const u8));
    }
    xml_xpath_free_object(res);
    ret
}

unsafe fn xsd_incorrect_test_case(logfile: &mut Option<File>, mut cur: *mut XmlNode) -> c_int {
    let mut ret: c_int = 0;

    cur = get_next(cur, c"./incorrect[1]".as_ptr() as _);
    if cur.is_null() {
        return 0;
    }

    let test: *mut XmlNode = get_next(cur, c"./*".as_ptr() as _);
    if test.is_null() {
        test_log!(
            logfile,
            "Failed to find test in correct line {}\n",
            (*cur).get_line_no()
        );
        return 1;
    }

    let memt: c_int = xml_mem_used();
    EXTRA_MEMORY_FROM_RESOLVER = 0;
    // dump the schemas to a buffer, then reparse it and compile the schemas
    let mut buf = vec![];
    (*test).dump_memory(&mut buf, XmlDocPtr::from_raw((*test).doc).unwrap(), 0, 0);
    let pctxt = xml_relaxng_new_mem_parser_ctxt(buf.as_ptr() as *const c_char, buf.len() as _);
    (*pctxt).set_parser_errors(
        Some(test_error_handler),
        Some(test_error_handler),
        Some(GenericErrorContext::new(pctxt)) as _,
    );
    let rng: XmlRelaxNGPtr = xml_relaxng_parse(pctxt);
    xml_relaxng_free_parser_ctxt(pctxt);
    if !rng.is_null() {
        test_log!(
            logfile,
            "Failed to detect incorrect RNG line {}\n",
            (*test).get_line_no()
        );
        ret = 1;
    }

    // `buf` must live until at this point.
    drop(buf);
    if !rng.is_null() {
        xml_relaxng_free(rng);
    }
    reset_last_error();
    if memt < xml_mem_used() && EXTRA_MEMORY_FROM_RESOLVER == 0 {
        test_log!(
            logfile,
            "Validation of tests starting line {} leaked {}\n",
            (*cur).get_line_no(),
            xml_mem_used() - memt
        );
        NB_LEAKS += 1;
    }
    ret
}

unsafe fn install_resources(mut tst: *mut XmlNode, base: *const XmlChar) {
    let mut test: *mut XmlNode;
    let mut name: *mut XmlChar;
    let mut content: *mut XmlChar;
    let mut res: *mut XmlChar;

    let mut buf = vec![];
    (*tst).dump_memory(&mut buf, XmlDocPtr::from_raw((*tst).doc).unwrap(), 0, 0);

    while !tst.is_null() {
        test = get_next(tst, c"./*".as_ptr() as _);
        if !test.is_null() {
            buf.clear();
            (*test).dump_memory(&mut buf, XmlDocPtr::from_raw((*test).doc).unwrap(), 0, 0);
            name = get_string(tst, c"string(@name)".as_ptr() as _);
            content = xml_strndup(buf.as_ptr(), buf.len() as i32);
            if !name.is_null() && !content.is_null() {
                res = compose_dir(base, name);
                xml_free(name as _);
                add_entity(res as *mut c_char, content as *mut c_char);
            } else {
                if !name.is_null() {
                    xml_free(name as _);
                }
                if !content.is_null() {
                    xml_free(content as _);
                }
            }
        }
        tst = get_next(tst, c"following-sibling::resource[1]".as_ptr() as _);
    }
}

unsafe fn install_dirs(tst: *mut XmlNode, base: *const XmlChar) {
    let mut test: *mut XmlNode;

    let name: *mut XmlChar = get_string(tst, c"string(@name)".as_ptr() as _);
    if name.is_null() {
        return;
    }
    let res: *mut XmlChar = compose_dir(base, name);
    xml_free(name as _);
    if res.is_null() {
        return;
    }
    // Now process resources and subdir recursively
    test = get_next(tst, c"./resource[1]".as_ptr() as _);
    if !test.is_null() {
        install_resources(test, res);
    }
    test = get_next(tst, c"./dir[1]".as_ptr() as _);
    while !test.is_null() {
        install_dirs(test, res);
        test = get_next(test, c"following-sibling::dir[1]".as_ptr() as _);
    }
    xml_free(res as _);
}

unsafe fn xsd_test_case(logfile: &mut Option<File>, tst: *mut XmlNode) -> c_int {
    let mut test: *mut XmlNode;
    let mut tmp: *mut XmlNode;
    let mut ctxt: XmlRelaxNGValidCtxtPtr;
    let mut ret: c_int = 0;
    let mut mem: c_int;
    let mut memt: c_int;

    reset_entities();
    TEST_ERRORS_SIZE = 0;
    TEST_ERRORS[0] = 0;

    tmp = get_next(tst, c"./dir[1]".as_ptr() as _);
    if !tmp.is_null() {
        install_dirs(tmp, null_mut());
    }
    tmp = get_next(tst, c"./resource[1]".as_ptr() as _);
    if !tmp.is_null() {
        install_resources(tmp, null_mut());
    }

    let cur: *mut XmlNode = get_next(tst, c"./correct[1]".as_ptr() as _);
    if cur.is_null() {
        return xsd_incorrect_test_case(logfile, tst);
    }

    test = get_next(cur, c"./*".as_ptr() as _);
    if test.is_null() {
        eprintln!(
            "Failed to find test in correct line {}",
            (*cur).get_line_no()
        );
        return 1;
    }

    memt = xml_mem_used();
    EXTRA_MEMORY_FROM_RESOLVER = 0;
    // dump the schemas to a buffer, then reparse it and compile the schemas
    let mut buf = vec![];
    (*test).dump_memory(&mut buf, XmlDocPtr::from_raw((*test).doc).unwrap(), 0, 0);
    let pctxt = xml_relaxng_new_mem_parser_ctxt(buf.as_ptr() as *const c_char, buf.len() as _);
    (*pctxt).set_parser_errors(
        Some(test_error_handler),
        Some(test_error_handler),
        Some(GenericErrorContext::new(pctxt)) as _,
    );
    let rng: XmlRelaxNGPtr = xml_relaxng_parse(pctxt);
    xml_relaxng_free_parser_ctxt(pctxt);
    if EXTRA_MEMORY_FROM_RESOLVER != 0 {
        memt = 0;
    }

    if rng.is_null() {
        test_log!(
            logfile,
            "Failed to parse RNGtest line {}\n",
            (*test).get_line_no()
        );
        NB_ERRORS += 1;
        ret = 1;
        if !rng.is_null() {
            xml_relaxng_free(rng);
        }
        reset_last_error();
        if memt != xml_mem_used() && memt != 0 {
            test_log!(
                logfile,
                "Validation of tests starting line {} leaked {}\n",
                (*cur).get_line_no(),
                xml_mem_used() - memt
            );
            NB_LEAKS += 1;
        }
        return ret;
    }
    // now scan all the siblings of correct to process the <valid> tests
    tmp = get_next(cur, c"following-sibling::valid[1]".as_ptr() as _);
    while !tmp.is_null() {
        let dtd = (*tmp).get_prop("dtd");
        test = get_next(tmp, c"./*".as_ptr() as _);
        if test.is_null() {
            eprintln!(
                "Failed to find test in <valid> line {}\n",
                (*tmp).get_line_no(),
            );
        } else {
            buf.clear();
            if let Some(dtd) = dtd {
                buf.extend(dtd.as_bytes());
            }
            (*test).dump_memory(&mut buf, XmlDocPtr::from_raw((*test).doc).unwrap(), 0, 0);

            // We are ready to run the test
            mem = xml_mem_used();
            EXTRA_MEMORY_FROM_RESOLVER = 0;
            let buffer = buf.clone();
            if let Some(doc) = xml_read_memory(buffer, Some("test"), None, 0) {
                NB_TESTS += 1;
                ctxt = xml_relaxng_new_valid_ctxt(rng);
                xml_relaxng_set_valid_errors(
                    ctxt,
                    Some(test_error_handler),
                    Some(test_error_handler),
                    Some(GenericErrorContext::new(ctxt)),
                );
                ret = xml_relaxng_validate_doc(ctxt, doc);
                xml_relaxng_free_valid_ctxt(ctxt);
                match ret.cmp(&0) {
                    std::cmp::Ordering::Greater => {
                        test_log!(
                            logfile,
                            "Failed to validate valid instance line {}\n",
                            (*tmp).get_line_no()
                        );
                        NB_ERRORS += 1;
                    }
                    std::cmp::Ordering::Less => {
                        test_log!(
                            logfile,
                            "Internal error validating instance line {}\n",
                            (*tmp).get_line_no()
                        );
                        NB_ERRORS += 1;
                    }
                    _ => {}
                }
                xml_free_doc(doc);
            } else {
                test_log!(
                    logfile,
                    "Failed to parse valid instance line {}\n",
                    (*tmp).get_line_no()
                );
                NB_ERRORS += 1;
            }
            reset_last_error();
            if mem != xml_mem_used() && EXTRA_MEMORY_FROM_RESOLVER == 0 {
                test_log!(
                    logfile,
                    "Validation of instance line {} leaked {}\n",
                    (*tmp).get_line_no(),
                    xml_mem_used() - mem
                );
                xml_memory_dump();
                NB_LEAKS += 1;
            }
        }
        tmp = get_next(tmp, c"following-sibling::valid[1]".as_ptr() as _);
    }
    // now scan all the siblings of correct to process the <invalid> tests
    tmp = get_next(cur, c"following-sibling::invalid[1]".as_ptr() as _);
    while !tmp.is_null() {
        test = get_next(tmp, c"./*".as_ptr() as _);
        if test.is_null() {
            eprintln!(
                "Failed to find test in <invalid> line {}",
                (*tmp).get_line_no()
            );
        } else {
            buf.clear();
            (*test).dump_memory(&mut buf, XmlDocPtr::from_raw((*test).doc).unwrap(), 0, 0);

            // We are ready to run the test
            mem = xml_mem_used();
            EXTRA_MEMORY_FROM_RESOLVER = 0;
            let buffer = buf.clone();
            if let Some(doc) = xml_read_memory(buffer, Some("test"), None, 0) {
                NB_TESTS += 1;
                ctxt = xml_relaxng_new_valid_ctxt(rng);
                xml_relaxng_set_valid_errors(
                    ctxt,
                    Some(test_error_handler),
                    Some(test_error_handler),
                    Some(GenericErrorContext::new(ctxt)) as _,
                );
                ret = xml_relaxng_validate_doc(ctxt, doc);
                xml_relaxng_free_valid_ctxt(ctxt);
                match ret.cmp(&0) {
                    std::cmp::Ordering::Equal => {
                        test_log!(
                            logfile,
                            "Failed to detect invalid instance line {}\n",
                            (*tmp).get_line_no()
                        );
                        NB_ERRORS += 1;
                    }
                    std::cmp::Ordering::Less => {
                        test_log!(
                            logfile,
                            "Internal error validating instance line {}\n",
                            (*tmp).get_line_no()
                        );
                        NB_ERRORS += 1;
                    }
                    _ => {}
                }
                xml_free_doc(doc);
            } else {
                test_log!(
                    logfile,
                    "Failed to parse valid instance line {}\n",
                    (*tmp).get_line_no()
                );
                NB_ERRORS += 1;
            }
            reset_last_error();
            if mem != xml_mem_used() && EXTRA_MEMORY_FROM_RESOLVER == 0 {
                test_log!(
                    logfile,
                    "Validation of instance line {} leaked {}\n",
                    (*tmp).get_line_no(),
                    xml_mem_used() - mem
                );
                xml_memory_dump();
                NB_LEAKS += 1;
            }
        }
        tmp = get_next(tmp, c"following-sibling::invalid[1]".as_ptr() as _);
    }

    if !rng.is_null() {
        xml_relaxng_free(rng);
    }
    reset_last_error();
    if memt != xml_mem_used() && memt != 0 {
        test_log!(
            logfile,
            "Validation of tests starting line {} leaked {}\n",
            (*cur).get_line_no(),
            xml_mem_used() - memt
        );
        NB_LEAKS += 1;
    }
    ret
}

unsafe fn xsd_test_suite(logfile: &mut Option<File>, mut cur: *mut XmlNode) -> c_int {
    if VERBOSE != 0 {
        let doc: *mut XmlChar = get_string(cur, c"string(documentation)".as_ptr() as _);

        if !doc.is_null() {
            println!("Suite {}", CStr::from_ptr(doc as _).to_string_lossy());
            xml_free(doc as _);
        }
    }
    cur = get_next(cur, c"./testCase[1]".as_ptr() as _);
    while !cur.is_null() {
        xsd_test_case(logfile, cur);
        cur = get_next(cur, c"following-sibling::testCase[1]".as_ptr() as _);
    }

    0
}

unsafe fn xsd_test(logfile: &mut Option<File>) -> c_int {
    let mut cur: *mut XmlNode;
    let filename = "test/xsdtest/xsdtestsuite.xml";
    let mut ret: c_int = 0;

    let Some(doc) = xml_read_file(filename, None, XmlParserOption::XmlParseNoEnt as _) else {
        eprintln!("Failed to parse {}", filename);
        return -1;
    };
    println!("## XML Schemas datatypes test suite from James Clark");
    test_log!(logfile, "filename: {}\n", filename);

    cur = doc.get_root_element();
    if cur.is_null() || !xml_str_equal((*cur).name, c"testSuite".as_ptr() as _) {
        eprintln!("Unexpected format {}", filename);
        ret = -1;
        xml_free_doc(doc);
        return ret;
    }

    cur = get_next(cur, c"./testSuite[1]".as_ptr() as _);
    if cur.is_null() || !xml_str_equal((*cur).name, c"testSuite".as_ptr() as _) {
        eprintln!("Unexpected format {}", filename);
        ret = -1;
        xml_free_doc(doc);
        return ret;
    }
    while !cur.is_null() {
        xsd_test_suite(logfile, cur);
        cur = get_next(cur, c"following-sibling::testSuite[1]".as_ptr() as _);
    }

    xml_free_doc(doc);
    ret
}

unsafe fn rng_test_suite(logfile: &mut Option<File>, mut cur: *mut XmlNode) -> c_int {
    if VERBOSE != 0 {
        let mut doc: *mut XmlChar = get_string(cur, c"string(documentation)".as_ptr() as _);

        if !doc.is_null() {
            println!("Suite {}", CStr::from_ptr(doc as _).to_string_lossy());
            xml_free(doc as _);
        } else {
            doc = get_string(cur, c"string(section)".as_ptr() as _);
            if !doc.is_null() {
                println!("Section {}", CStr::from_ptr(doc as _).to_string_lossy());
                xml_free(doc as _);
            }
        }
    }
    cur = get_next(cur, c"./testSuite[1]".as_ptr() as _);
    while !cur.is_null() {
        xsd_test_suite(logfile, cur);
        cur = get_next(cur, c"following-sibling::testSuite[1]".as_ptr() as _);
    }

    0
}

unsafe fn rng_test1(logfile: &mut Option<File>) -> c_int {
    let mut cur: *mut XmlNode;
    let filename = "test/relaxng/OASIS/spectest.xml";
    let mut ret: c_int = 0;

    let Some(doc) = xml_read_file(filename, None, XmlParserOption::XmlParseNoEnt as _) else {
        eprintln!("Failed to parse {}", filename);
        return -1;
    };
    println!("## Relax NG test suite from James Clark");
    test_log!(logfile, "filename: {}\n", filename);

    cur = (*doc).get_root_element();
    if cur.is_null() || !xml_str_equal((*cur).name, c"testSuite".as_ptr() as _) {
        eprintln!("Unexpected format {}", filename);
        ret = -1;
        xml_free_doc(doc);
        return ret;
    }

    cur = get_next(cur, c"./testSuite[1]".as_ptr() as _);
    if cur.is_null() || !xml_str_equal((*cur).name, c"testSuite".as_ptr() as _) {
        eprintln!("Unexpected format {}", filename);
        ret = -1;
        xml_free_doc(doc);
        return ret;
    }
    while !cur.is_null() {
        rng_test_suite(logfile, cur);
        cur = get_next(cur, c"following-sibling::testSuite[1]".as_ptr() as _);
    }

    xml_free_doc(doc);
    ret
}

unsafe fn rng_test2(logfile: &mut Option<File>) -> c_int {
    let mut cur: *mut XmlNode;
    let filename = "test/relaxng/testsuite.xml";
    let mut ret: c_int = 0;

    let Some(doc) = xml_read_file(filename, None, XmlParserOption::XmlParseNoEnt as _) else {
        eprintln!("Failed to parse {}", filename);
        return -1;
    };
    println!("## Relax NG test suite for libxml2");
    test_log!(logfile, "filename: {}\n", filename);

    cur = (*doc).get_root_element();
    if cur.is_null() || !xml_str_equal((*cur).name, c"testSuite".as_ptr() as _) {
        eprintln!("Unexpected format {}", filename);
        ret = -1;
        xml_free_doc(doc);
        return ret;
    }

    cur = get_next(cur, c"./testSuite[1]".as_ptr() as _);
    if cur.is_null() || !xml_str_equal((*cur).name, c"testSuite".as_ptr() as _) {
        eprintln!("Unexpected format {}", filename);
        ret = -1;
        xml_free_doc(doc);
        return ret;
    }
    while !cur.is_null() {
        xsd_test_suite(logfile, cur);
        cur = get_next(cur, c"following-sibling::testSuite[1]".as_ptr() as _);
    }

    xml_free_doc(doc);
    ret
}

unsafe fn xstc_test_instance(
    logfile: &mut Option<File>,
    cur: *mut XmlNode,
    schemas: XmlSchemaPtr,
    spath: *const XmlChar,
    base: *const c_char,
) -> c_int {
    let mut validity: *mut XmlChar = null_mut();
    let mut ctxt: XmlSchemaValidCtxtPtr = null_mut();
    let mut ret: c_int;

    reset_last_error();
    TEST_ERRORS_SIZE = 0;
    TEST_ERRORS[0] = 0;
    let mem: c_int = xml_mem_used();
    let href: *mut XmlChar = get_string(
        cur,
        c"string(ts:instanceDocument/@xlink:href)".as_ptr() as _,
    );
    if href.is_null() || *href.add(0) == 0 {
        test_log!(
            logfile,
            "testGroup line {} misses href for schemaDocument\n",
            (*cur).get_line_no()
        );
        ret = -1;
    // goto done;
    } else if let Some(path) = build_uri(
        &CStr::from_ptr(href as *const i8).to_string_lossy(),
        &CStr::from_ptr(base).to_string_lossy(),
    ) {
        if !check_test_file(&path) {
            test_log!(
                logfile,
                "schemas for testGroup line {} is missing: {}\n",
                (*cur).get_line_no(),
                path
            );
            ret = -1;
            // goto done;
        } else {
            validity = get_string(cur, c"string(ts:expected/@validity)".as_ptr() as _);
            if validity.is_null() {
                eprintln!(
                    "instanceDocument line {} misses expected validity",
                    (*cur).get_line_no()
                );
                ret = -1;
                // goto done;
            } else {
                NB_TESTS += 1;
                if let Some(mut doc) =
                    xml_read_file(&path, None, XmlParserOption::XmlParseNoEnt as i32)
                {
                    ctxt = xml_schema_new_valid_ctxt(schemas);
                    xml_schema_set_valid_errors(
                        ctxt,
                        Some(test_error_handler),
                        Some(test_error_handler),
                        Some(GenericErrorContext::new(ctxt)) as _,
                    );
                    ret = xml_schema_validate_doc(ctxt, doc.deref_mut());

                    if xml_str_equal(validity, c"valid".as_ptr() as _) {
                        match ret.cmp(&0) {
                            std::cmp::Ordering::Greater => {
                                test_log!(
                                    logfile,
                                    "valid instance {} failed to validate against {}\n",
                                    path,
                                    CStr::from_ptr(spath as _).to_string_lossy()
                                );
                                NB_ERRORS += 1;
                            }
                            std::cmp::Ordering::Less => {
                                test_log!(
                                    logfile,
                                    "valid instance {} got internal error validating {}\n",
                                    path,
                                    CStr::from_ptr(spath as _).to_string_lossy()
                                );
                                NB_INTERNALS += 1;
                                NB_ERRORS += 1;
                            }
                            _ => {}
                        }
                    } else if xml_str_equal(validity, c"invalid".as_ptr() as _) {
                        if ret == 0 {
                            test_log!(
                                logfile,
                                "Failed to detect invalid instance {} against {}\n",
                                path,
                                CStr::from_ptr(spath as _).to_string_lossy()
                            );
                            NB_ERRORS += 1;
                        }
                    } else {
                        test_log!(
                            logfile,
                            "instanceDocument line {} has unexpected validity value{}\n",
                            (*cur).get_line_no(),
                            CStr::from_ptr(validity as _).to_string_lossy()
                        );
                        ret = -1;
                        // goto done;
                    }
                    xml_free_doc(doc);
                } else {
                    eprintln!("instance {} fails to parse", path);
                    ret = -1;
                    NB_ERRORS += 1;
                    // goto done;
                }
            }
        }
    } else {
        eprintln!(
            "Failed to build path to schemas testGroup line {} : {}",
            (*cur).get_line_no(),
            CStr::from_ptr(href as _).to_string_lossy()
        );
        ret = -1;
    }

    // done:
    if !href.is_null() {
        xml_free(href as _);
    }
    if !validity.is_null() {
        xml_free(validity as _);
    }
    if !ctxt.is_null() {
        xml_schema_free_valid_ctxt(ctxt);
    }
    reset_last_error();
    if mem != xml_mem_used() {
        test_log!(
            logfile,
            "Validation of tests starting line {} leaked {}\n",
            (*cur).get_line_no(),
            xml_mem_used() - mem
        );
        NB_LEAKS += 1;
    }
    ret
}

unsafe fn xstc_test_group(
    logfile: &mut Option<File>,
    cur: *mut XmlNode,
    base: *const c_char,
) -> c_int {
    let mut p = None::<String>;
    let mut validity: *mut XmlChar = null_mut();
    let mut schemas: XmlSchemaPtr = null_mut();
    let ctxt: XmlSchemaParserCtxtPtr;
    let mut instance: *mut XmlNode;
    let mut ret: c_int = 0;

    reset_last_error();
    TEST_ERRORS_SIZE = 0;
    TEST_ERRORS[0] = 0;
    let mem: c_int = xml_mem_used();
    let href: *mut XmlChar = get_string(
        cur,
        c"string(ts:schemaTest/ts:schemaDocument/@xlink:href)".as_ptr() as _,
    );
    if href.is_null() || *href.add(0) == 0 {
        test_log!(
            logfile,
            "testGroup line {} misses href for schemaDocument\n",
            (*cur).get_line_no()
        );
        ret = -1;
    // goto done;
    } else if let Some(path) = build_uri(
        &CStr::from_ptr(href as *const i8).to_string_lossy(),
        &CStr::from_ptr(base).to_string_lossy(),
    ) {
        if !check_test_file(&path) {
            test_log!(
                logfile,
                "schemas for testGroup line {} is missing: {}\n",
                (*cur).get_line_no(),
                path
            );
            ret = -1;
            // goto done;
        } else {
            validity = get_string(
                cur,
                c"string(ts:schemaTest/ts:expected/@validity)".as_ptr() as _,
            );
            if validity.is_null() {
                test_log!(
                    logfile,
                    "testGroup line {} misses expected validity\n",
                    (*cur).get_line_no()
                );
                ret = -1;
                // goto done;
            } else {
                NB_TESTS += 1;
                if xml_str_equal(validity, c"valid".as_ptr() as _) {
                    NB_SCHEMATAS += 1;
                    let cpath = CString::new(path.as_str()).unwrap();
                    ctxt = xml_schema_new_parser_ctxt(cpath.as_ptr());
                    xml_schema_set_parser_errors(
                        ctxt,
                        Some(test_error_handler),
                        Some(test_error_handler),
                        Some(GenericErrorContext::new(ctxt)),
                    );
                    schemas = xml_schema_parse(ctxt);
                    xml_schema_free_parser_ctxt(ctxt);
                    if schemas.is_null() {
                        test_log!(logfile, "valid schemas {} failed to parse\n", path);
                        ret = 1;
                        NB_ERRORS += 1;
                    }
                    if ret == 0
                        && !strstr(TEST_ERRORS.as_ptr() as _, c"nimplemented".as_ptr()).is_null()
                    {
                        test_log!(
                            logfile,
                            "valid schemas {} hit an unimplemented block\n",
                            path
                        );
                        ret = 1;
                        NB_UNIMPLEMENTED += 1;
                        NB_ERRORS += 1;
                    }
                    instance = get_next(cur, c"./ts:instanceTest[1]".as_ptr() as _);
                    while !instance.is_null() {
                        if !schemas.is_null() {
                            xstc_test_instance(
                                logfile,
                                instance,
                                schemas,
                                cpath.as_ptr() as *const u8,
                                base,
                            );
                        } else {
                            // We'll automatically mark the instances as failed
                            // if the schema was broken.
                            NB_ERRORS += 1;
                        }
                        instance = get_next(
                            instance,
                            c"following-sibling::ts:instanceTest[1]".as_ptr() as _,
                        );
                    }
                } else if xml_str_equal(validity, c"invalid".as_ptr() as _) {
                    NB_SCHEMATAS += 1;
                    let cpath = CString::new(path.as_str()).unwrap();
                    ctxt = xml_schema_new_parser_ctxt(cpath.as_ptr());
                    xml_schema_set_parser_errors(
                        ctxt,
                        Some(test_error_handler),
                        Some(test_error_handler),
                        Some(GenericErrorContext::new(ctxt)),
                    );
                    schemas = xml_schema_parse(ctxt);
                    xml_schema_free_parser_ctxt(ctxt);
                    if !schemas.is_null() {
                        test_log!(logfile, "Failed to detect error in schemas {}\n", path);
                        NB_ERRORS += 1;
                        ret = 1;
                    }
                    if ret == 0
                        && !strstr(TEST_ERRORS.as_ptr() as _, c"nimplemented".as_ptr()).is_null()
                    {
                        NB_UNIMPLEMENTED += 1;
                        test_log!(
                            logfile,
                            "invalid schemas {} hit an unimplemented block\n",
                            path
                        );
                        ret = 1;
                        NB_ERRORS += 1;
                    }
                } else {
                    test_log!(
                        logfile,
                        "testGroup line {} misses unexpected validity value{}\n",
                        (*cur).get_line_no(),
                        CStr::from_ptr(validity as _).to_string_lossy()
                    );
                    ret = -1;
                    // goto done;
                }
            }
        }
        p = Some(path);
    } else {
        test_log!(
            logfile,
            "Failed to build path to schemas testGroup line {} : {}\n",
            (*cur).get_line_no(),
            CStr::from_ptr(href as _).to_string_lossy()
        );
        ret = -1;
    }

    // done:
    if !href.is_null() {
        xml_free(href as _);
    }
    if !validity.is_null() {
        xml_free(validity as _);
    }
    if !schemas.is_null() {
        xml_schema_free(schemas);
    }
    reset_last_error();
    if mem != xml_mem_used() && EXTRA_MEMORY_FROM_RESOLVER == 0 {
        test_log!(
            logfile,
            "Processing test line {} {} leaked {}\n",
            (*cur).get_line_no(),
            p.as_deref().unwrap_or("(null)"),
            xml_mem_used() - mem
        );
        NB_LEAKS += 1;
    }
    ret
}

unsafe fn xstc_metadata(logfile: &mut Option<File>, metadata: &str, base: *const c_char) -> c_int {
    let mut cur: *mut XmlNode;
    let mut ret: c_int = 0;

    let Some(doc) = xml_read_file(metadata, None, XmlParserOption::XmlParseNoEnt as _) else {
        eprintln!("Failed to parse {metadata}");
        return -1;
    };

    cur = (*doc).get_root_element();
    if cur.is_null() || !xml_str_equal((*cur).name, c"testSet".as_ptr() as _) {
        eprintln!("Unexpected format {metadata}");
        return -1;
    }
    let contributor = (*cur)
        .get_prop("contributor")
        .unwrap_or("Unknown".to_owned());
    let name = (*cur).get_prop("name").unwrap_or("Unknown".to_owned());
    println!("## {contributor} test suite for Schemas version {name}");

    cur = get_next(cur, c"./ts:testGroup[1]".as_ptr() as _);
    if cur.is_null() || !xml_str_equal((*cur).name, c"testGroup".as_ptr() as _) {
        eprintln!("Unexpected format {metadata}");
        ret = -1;
        xml_free_doc(doc);
        return ret;
    }
    while !cur.is_null() {
        xstc_test_group(logfile, cur, base);
        cur = get_next(cur, c"following-sibling::ts:testGroup[1]".as_ptr() as _);
    }

    xml_free_doc(doc);
    ret
}

#[test]
fn main() {
    let ret: c_int;
    let mut old_errors: c_int;
    let mut old_tests: c_int;
    let mut old_leaks: c_int;

    const LOGFILE: &str = "runsuite.log";

    unsafe {
        let mut logfile = match File::options()
            .write(true)
            .create(true)
            .truncate(true)
            .open(LOGFILE)
        {
            Ok(file) => Some(file),
            Err(_) => {
                eprintln!("Could not open the log file, running in verbose mode");
                VERBOSE = 1;
                None
            }
        };
        initialize_libxml2();

        if args().any(|s| s == "-v") {
            VERBOSE = 1;
        }

        old_errors = NB_ERRORS;
        old_tests = NB_TESTS;
        old_leaks = NB_LEAKS;
        xsd_test(&mut logfile);
        println!(
            "Ran {} tests, {} errors, {} leaks",
            NB_TESTS - old_tests,
            NB_ERRORS - old_errors,
            NB_LEAKS - old_leaks,
        );
        if NB_ERRORS - old_errors == 10 {
            println!("10 errors were expected");
            NB_ERRORS = old_errors;
        } else {
            println!(
                "10 errors were expected, got {} errors",
                NB_ERRORS - old_errors,
            );
            NB_ERRORS = old_errors + 1;
        }

        old_errors = NB_ERRORS;
        old_tests = NB_TESTS;
        old_leaks = NB_LEAKS;
        rng_test1(&mut logfile);
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

        old_errors = NB_ERRORS;
        old_tests = NB_TESTS;
        old_leaks = NB_LEAKS;
        rng_test2(&mut logfile);
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

        old_errors = NB_ERRORS;
        old_tests = NB_TESTS;
        old_leaks = NB_LEAKS;
        NB_INTERNALS = 0;
        NB_SCHEMATAS = 0;
        xstc_metadata(
            &mut logfile,
            "xstc/Tests/Metadata/NISTXMLSchemaDatatypes.testSet",
            c"xstc/Tests/Metadata/".as_ptr(),
        );
        if NB_ERRORS == old_errors && NB_LEAKS == old_leaks {
            println!(
                "Ran {} tests ({} schemata), no errors",
                NB_TESTS - old_tests,
                NB_SCHEMATAS,
            );
        } else {
            println!(
                "Ran {} tests ({} schemata), {} errors ({} internals), {} leaks",
                NB_TESTS - old_tests,
                NB_SCHEMATAS,
                NB_ERRORS - old_errors,
                NB_INTERNALS,
                NB_LEAKS - old_leaks,
            );
        }

        old_errors = NB_ERRORS;
        old_tests = NB_TESTS;
        old_leaks = NB_LEAKS;
        NB_INTERNALS = 0;
        NB_SCHEMATAS = 0;
        xstc_metadata(
            &mut logfile,
            "xstc/Tests/Metadata/SunXMLSchema1-0-20020116.testSet",
            c"xstc/Tests/".as_ptr(),
        );
        if NB_ERRORS == old_errors && NB_LEAKS == old_leaks {
            println!(
                "Ran {} tests ({} schemata), no errors",
                NB_TESTS - old_tests,
                NB_SCHEMATAS,
            );
        } else {
            println!(
                "Ran {} tests ({} schemata), {} errors ({} internals), {} leaks",
                NB_TESTS - old_tests,
                NB_SCHEMATAS,
                NB_ERRORS - old_errors,
                NB_INTERNALS,
                NB_LEAKS - old_leaks,
            );
            println!("Some errors were expected.");
            NB_ERRORS = old_errors;
        }

        old_errors = NB_ERRORS;
        old_tests = NB_TESTS;
        old_leaks = NB_LEAKS;
        NB_INTERNALS = 0;
        NB_SCHEMATAS = 0;
        xstc_metadata(
            &mut logfile,
            "xstc/Tests/Metadata/MSXMLSchema1-0-20020116.testSet",
            c"xstc/Tests/".as_ptr(),
        );
        if NB_ERRORS == old_errors && NB_LEAKS == old_leaks {
            println!(
                "Ran {} tests ({} schemata), no errors",
                NB_TESTS - old_tests,
                NB_SCHEMATAS,
            );
        } else {
            println!(
                "Ran {} tests ({} schemata), {} errors ({} internals), {} leaks",
                NB_TESTS - old_tests,
                NB_SCHEMATAS,
                NB_ERRORS - old_errors,
                NB_INTERNALS,
                NB_LEAKS - old_leaks,
            );
            println!("Some errors were expected.");
            NB_ERRORS = old_errors;
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
        xml_xpath_free_context(CTXT_XPATH.load(Ordering::Relaxed));
        xml_cleanup_parser();
        xml_memory_dump();
    }

    assert_eq!(ret, 0);
}
