//! Rust implementation of original libxml2's `runsuite.c`.  
//! If you want this to work, copy the `test/` and `result/` directories from the original libxml2.

use std::{
    borrow::Cow,
    env::args,
    ffi::{CStr, CString, c_char, c_int},
    fs::{File, metadata},
    ptr::null_mut,
    sync::{
        Mutex,
        atomic::{AtomicI32, AtomicPtr, AtomicUsize, Ordering},
    },
};

use exml::{
    globals::{
        GenericErrorContext, reset_last_error, set_generic_error, set_get_warnings_default_value,
        set_pedantic_parser_default_value,
    },
    io::xml_no_net_external_entity_loader,
    libxml::{
        globals::xml_free,
        parser::{
            XmlParserOption, xml_cleanup_parser, xml_init_parser, xml_set_external_entity_loader,
        },
        relaxng::{
            XmlRelaxNGPtr, xml_relaxng_free, xml_relaxng_parse, xml_relaxng_set_valid_errors,
            xml_relaxng_validate_doc,
        },
        xmlmemory::{
            xml_mem_free, xml_mem_malloc, xml_mem_realloc, xml_mem_setup, xml_mem_used,
            xml_memory_dump, xml_memory_strdup,
        },
        xmlschemas::xml_schema_validate_doc,
        xmlschemastypes::xml_schema_init_types,
        xmlstring::{XmlChar, xml_strndup},
    },
    parser::{XmlParserCtxtPtr, XmlParserInput, xml_read_file, xml_read_memory},
    relaxng::{
        XmlRelaxNGValidCtxtPtr, xml_relaxng_free_parser_ctxt, xml_relaxng_free_valid_ctxt,
        xml_relaxng_init_types, xml_relaxng_new_mem_parser_ctxt, xml_relaxng_new_valid_ctxt,
    },
    tree::{XmlGenericNodePtr, XmlNodePtr, xml_free_doc},
    uri::build_uri,
    xmlschemas::{
        context::{
            XmlSchemaParserCtxtPtr, XmlSchemaValidCtxtPtr, xml_schema_free_parser_ctxt,
            xml_schema_free_valid_ctxt, xml_schema_new_parser_ctxt, xml_schema_new_valid_ctxt,
        },
        schema::{XmlSchemaPtr, xml_schema_free},
    },
    xpath::{
        XmlXPathCompExprPtr, XmlXPathContext, XmlXPathObjectPtr, XmlXPathObjectType,
        internals::xml_xpath_register_ns, xml_xpath_compile, xml_xpath_compiled_eval,
        xml_xpath_context_set_cache, xml_xpath_free_comp_expr, xml_xpath_free_context,
        xml_xpath_free_object, xml_xpath_new_context,
    },
};
use libc::strstr;

static mut VERBOSE: c_int = 0;

fn check_test_file(filename: &str) -> bool {
    match metadata(filename) {
        Ok(meta) => meta.is_file(),
        _ => false,
    }
}

fn compose_dir<'a>(dir: Option<&str>, path: &'a str) -> Cow<'a, str> {
    if let Some(dir) = dir {
        Cow::Owned(format!("{dir}/{path}"))
    } else {
        Cow::Borrowed(path)
    }
}

static NB_TESTS: AtomicI32 = AtomicI32::new(0);
static NB_ERRORS: AtomicI32 = AtomicI32::new(0);
static NB_INTERNALS: AtomicI32 = AtomicI32::new(0);
static NB_SCHEMATAS: AtomicI32 = AtomicI32::new(0);
static NB_UNIMPLEMENTED: AtomicI32 = AtomicI32::new(0);
static NB_LEAKS: AtomicI32 = AtomicI32::new(0);
static EXTRA_MEMORY_FROM_RESOLVER: AtomicI32 = AtomicI32::new(0);

const MAX_ENTITIES: usize = 20;
static mut TEST_ENTITIES_NAME: [Option<String>; MAX_ENTITIES] = [const { None }; MAX_ENTITIES];
static mut TEST_ENTITIES_VALUE: [*mut c_char; MAX_ENTITIES] = [null_mut(); MAX_ENTITIES];
static mut NB_ENTITIES: usize = 0;
unsafe fn reset_entities() {
    unsafe {
        for i in 0..NB_ENTITIES {
            TEST_ENTITIES_NAME[i] = None;
            if !TEST_ENTITIES_VALUE[i].is_null() {
                xml_free(TEST_ENTITIES_VALUE[i] as _);
            }
        }
        NB_ENTITIES = 0;
    }
}
unsafe fn add_entity(name: &str, content: *mut c_char) -> c_int {
    unsafe {
        if NB_ENTITIES >= MAX_ENTITIES {
            eprintln!("Too many entities defined");
            return -1;
        }
        TEST_ENTITIES_NAME[NB_ENTITIES] = Some(name.to_owned());
        TEST_ENTITIES_VALUE[NB_ENTITIES] = content;
        NB_ENTITIES += 1;
        0
    }
}

// We need to trap calls to the resolver to not account memory for the catalog
// which is shared to the current running test. We also don't want to have
// network downloads modifying tests.
unsafe fn test_external_entity_loader(
    url: Option<&str>,
    id: Option<&str>,
    ctxt: XmlParserCtxtPtr,
) -> Option<XmlParserInput> {
    unsafe {
        for i in 0..NB_ENTITIES {
            if TEST_ENTITIES_NAME[i].as_deref() == url {
                let mut ret = XmlParserInput::from_str(
                    (!ctxt.is_null()).then(|| &mut *ctxt),
                    &CStr::from_ptr(TEST_ENTITIES_VALUE[i]).to_string_lossy(),
                );
                if let Some(ret) = ret.as_mut() {
                    ret.filename = Some(TEST_ENTITIES_NAME[i].as_deref().unwrap().to_owned());
                }
                return ret;
            }
        }
        if check_test_file(url.unwrap()) {
            xml_no_net_external_entity_loader(url, id, ctxt)
        } else {
            let memused: c_int = xml_mem_used();
            let ret = xml_no_net_external_entity_loader(url, id, ctxt);
            EXTRA_MEMORY_FROM_RESOLVER.fetch_add(xml_mem_used() - memused, Ordering::Relaxed);
            ret
        }
    }
}

// Trapping the error messages at the generic level to grab the equivalent of
// stderr messages on CLI tools.
static TEST_ERRORS: Mutex<[u8; 32769]> = Mutex::new([0; 32769]);
static TEST_ERRORS_SIZE: AtomicUsize = AtomicUsize::new(0);

macro_rules! test_log {
    ( $logfile:expr, $msg:literal, $( $args:expr ),* ) => {
        if let Some(logfile) = $logfile.as_mut() {
            use std::io::Write;
            writeln!(logfile, "\n------------").ok();
            write!(logfile, $msg, $( $args ),*).ok();
            write!(logfile, "{}", CStr::from_ptr(TEST_ERRORS.lock().unwrap().as_ptr() as _).to_string_lossy()).ok();
            TEST_ERRORS_SIZE.store(0, Ordering::Relaxed);
            TEST_ERRORS.lock().unwrap()[0] = 0;
        }

        if VERBOSE != 0 {
            $(
                eprint!("{}", $args);
            )*
        }
    };
}

fn test_error_handler(_ctx: Option<GenericErrorContext>, msg: &str) {
    if TEST_ERRORS_SIZE.load(Ordering::Relaxed) >= 32768 {
        return;
    }

    let mut errors = TEST_ERRORS.lock().unwrap();
    if TEST_ERRORS_SIZE.load(Ordering::Relaxed) + msg.len() >= 32768 {
        let len = errors.len();
        errors[TEST_ERRORS_SIZE.load(Ordering::Relaxed)..]
            .copy_from_slice(&msg.as_bytes()[..len - TEST_ERRORS_SIZE.load(Ordering::Relaxed)]);
        // buffer is full
        TEST_ERRORS_SIZE.store(32768, Ordering::Relaxed);
        errors[TEST_ERRORS_SIZE.load(Ordering::Relaxed)] = 0;
    } else {
        errors[TEST_ERRORS_SIZE.load(Ordering::Relaxed)
            ..TEST_ERRORS_SIZE.load(Ordering::Relaxed) + msg.len()]
            .copy_from_slice(msg.as_bytes());
        TEST_ERRORS_SIZE.fetch_add(msg.len(), Ordering::Relaxed);
    }
    errors[TEST_ERRORS_SIZE.load(Ordering::Relaxed)] = 0;
}

static CTXT_XPATH: AtomicPtr<XmlXPathContext> = AtomicPtr::new(null_mut());

unsafe fn initialize_libxml2() {
    unsafe {
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
        CTXT_XPATH.store(xml_xpath_new_context(None), Ordering::Relaxed);
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
        xml_xpath_register_ns(CTXT_XPATH.load(Ordering::Relaxed), "ts", Some("TestSuite"));
        xml_xpath_register_ns(
            CTXT_XPATH.load(Ordering::Relaxed),
            "xlink",
            Some("http://www.w3.org/1999/xlink"),
        );
        set_generic_error(Some(test_error_handler), None);
        #[cfg(feature = "schema")]
        {
            xml_schema_init_types();
            xml_relaxng_init_types();
        }
    }
}

unsafe fn get_next(cur: Option<XmlNodePtr>, xpath: &str) -> Option<XmlNodePtr> {
    unsafe {
        let cur_doc = cur?.doc?;
        (*CTXT_XPATH.load(Ordering::Relaxed)).doc = Some(cur_doc);
        (*CTXT_XPATH.load(Ordering::Relaxed)).node = Some(XmlGenericNodePtr::from(cur?));
        let comp: XmlXPathCompExprPtr = xml_xpath_compile(xpath);
        if comp.is_null() {
            eprintln!("Failed to compile {}", xpath);
            return None;
        }
        let res: XmlXPathObjectPtr =
            xml_xpath_compiled_eval(comp, CTXT_XPATH.load(Ordering::Relaxed));
        xml_xpath_free_comp_expr(comp);
        if res.is_null() {
            return None;
        }
        let mut ret = None;
        if (*res).typ == XmlXPathObjectType::XPathNodeset {
            if let Some(nodeset) = (*res).nodesetval.as_deref() {
                if !nodeset.node_tab.is_empty() {
                    ret = Some(nodeset.node_tab[0]);
                }
            }
        }
        xml_xpath_free_object(res);
        ret.and_then(|ret| XmlNodePtr::try_from(ret).ok())
    }
}

unsafe fn get_string(cur: XmlNodePtr, xpath: &str) -> Option<String> {
    unsafe {
        let cur_doc = cur.doc?;
        (*CTXT_XPATH.load(Ordering::Relaxed)).doc = Some(cur_doc);
        (*CTXT_XPATH.load(Ordering::Relaxed)).node = Some(XmlGenericNodePtr::from(cur));
        let comp: XmlXPathCompExprPtr = xml_xpath_compile(xpath);
        if comp.is_null() {
            eprintln!("Failed to compile {}", xpath);
            return None;
        }
        let res: XmlXPathObjectPtr =
            xml_xpath_compiled_eval(comp, CTXT_XPATH.load(Ordering::Relaxed));
        xml_xpath_free_comp_expr(comp);
        if res.is_null() {
            return None;
        }
        let mut ret = None;
        if (*res).typ == XmlXPathObjectType::XPathString {
            ret = (*res).stringval.take();
        }
        xml_xpath_free_object(res);
        ret
    }
}

unsafe fn xsd_incorrect_test_case(logfile: &mut Option<File>, cur: Option<XmlNodePtr>) -> c_int {
    unsafe {
        let mut ret: c_int = 0;

        let Some(cur) = get_next(cur, "./incorrect[1]") else {
            return 0;
        };

        let Some(mut test) = get_next(Some(cur), "./*") else {
            test_log!(
                logfile,
                "Failed to find test in correct line {}\n",
                cur.get_line_no()
            );
            return 1;
        };

        let memt: c_int = xml_mem_used();
        EXTRA_MEMORY_FROM_RESOLVER.store(0, Ordering::Relaxed);
        // dump the schemas to a buffer, then reparse it and compile the schemas
        let mut buf = vec![];
        let test_doc = test.doc;
        test.dump_memory(&mut buf, test_doc, 0, 0);
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
        if memt < xml_mem_used() && EXTRA_MEMORY_FROM_RESOLVER.load(Ordering::Relaxed) == 0 {
            test_log!(
                logfile,
                "Validation of tests starting line {} leaked {}\n",
                cur.get_line_no(),
                xml_mem_used() - memt
            );
            NB_LEAKS.fetch_add(1, Ordering::Relaxed);
        }
        ret
    }
}

unsafe fn install_resources(mut tst: XmlNodePtr, base: Option<&str>) {
    unsafe {
        let mut content: *mut XmlChar;

        let mut buf = vec![];
        let test_doc = tst.doc;
        tst.dump_memory(&mut buf, test_doc, 0, 0);

        let mut tst = Some(tst);
        while let Some(now) = tst {
            if let Some(mut test) = get_next(Some(now), "./*") {
                buf.clear();
                let test_doc = test.doc;
                test.dump_memory(&mut buf, test_doc, 0, 0);
                let name = get_string(now, "string(@name)");
                content = xml_strndup(buf.as_ptr(), buf.len() as i32);
                if let Some(name) = name.filter(|_| !content.is_null()) {
                    let res = compose_dir(base, &name);
                    add_entity(&res, content as *mut c_char);
                } else if !content.is_null() {
                    xml_free(content as _);
                }
            }
            tst = get_next(Some(now), "following-sibling::resource[1]");
        }
    }
}

unsafe fn install_dirs(tst: XmlNodePtr, base: Option<&str>) {
    unsafe {
        let Some(name) = get_string(tst, "string(@name)") else {
            return;
        };
        let res = compose_dir(base, &name);
        // Now process resources and subdir recursively
        let test = get_next(Some(tst), "./resource[1]");
        if let Some(test) = test {
            install_resources(test, Some(&res));
        }
        let mut test = get_next(Some(tst), "./dir[1]");
        while let Some(now) = test {
            install_dirs(now, Some(&res));
            test = get_next(Some(now), "following-sibling::dir[1]");
        }
    }
}

unsafe fn xsd_test_case(logfile: &mut Option<File>, tst: Option<XmlNodePtr>) -> c_int {
    unsafe {
        let mut ctxt: XmlRelaxNGValidCtxtPtr;
        let mut ret: c_int = 0;
        let mut mem: c_int;
        let mut memt: c_int;

        reset_entities();
        TEST_ERRORS_SIZE.store(0, Ordering::Relaxed);
        TEST_ERRORS.lock().unwrap()[0] = 0;

        let tmp = get_next(tst, "./dir[1]");
        if let Some(tmp) = tmp {
            install_dirs(tmp, None);
        }
        let tmp = get_next(tst, "./resource[1]");
        if let Some(tmp) = tmp {
            install_resources(tmp, None);
        }

        let Some(cur) = get_next(tst, "./correct[1]") else {
            return xsd_incorrect_test_case(logfile, tst);
        };

        let Some(mut test) = get_next(Some(cur), "./*") else {
            eprintln!("Failed to find test in correct line {}", cur.get_line_no());
            return 1;
        };

        memt = xml_mem_used();
        EXTRA_MEMORY_FROM_RESOLVER.store(0, Ordering::Relaxed);
        // dump the schemas to a buffer, then reparse it and compile the schemas
        let mut buf = vec![];
        let test_doc = test.doc;
        test.dump_memory(&mut buf, test_doc, 0, 0);
        let pctxt = xml_relaxng_new_mem_parser_ctxt(buf.as_ptr() as *const c_char, buf.len() as _);
        (*pctxt).set_parser_errors(
            Some(test_error_handler),
            Some(test_error_handler),
            Some(GenericErrorContext::new(pctxt)) as _,
        );
        let rng: XmlRelaxNGPtr = xml_relaxng_parse(pctxt);
        xml_relaxng_free_parser_ctxt(pctxt);
        if EXTRA_MEMORY_FROM_RESOLVER.load(Ordering::Relaxed) != 0 {
            memt = 0;
        }

        if rng.is_null() {
            test_log!(
                logfile,
                "Failed to parse RNGtest line {}\n",
                (*test).get_line_no()
            );
            NB_ERRORS.fetch_add(1, Ordering::Relaxed);
            ret = 1;
            if !rng.is_null() {
                xml_relaxng_free(rng);
            }
            reset_last_error();
            if memt != xml_mem_used() && memt != 0 {
                test_log!(
                    logfile,
                    "Validation of tests starting line {} leaked {}\n",
                    cur.get_line_no(),
                    xml_mem_used() - memt
                );
                NB_LEAKS.fetch_add(1, Ordering::Relaxed);
            }
            return ret;
        }
        // now scan all the siblings of correct to process the <valid> tests
        let mut tmp = get_next(Some(cur), "following-sibling::valid[1]");
        while let Some(now) = tmp {
            let dtd = now.get_prop("dtd");
            if let Some(mut test) = get_next(Some(now), "./*") {
                buf.clear();
                if let Some(dtd) = dtd {
                    buf.extend(dtd.as_bytes());
                }
                let test_doc = test.doc;
                test.dump_memory(&mut buf, test_doc, 0, 0);

                // We are ready to run the test
                mem = xml_mem_used();
                EXTRA_MEMORY_FROM_RESOLVER.store(0, Ordering::Relaxed);
                let buffer = buf.clone();
                if let Some(doc) = xml_read_memory(buffer, Some("test"), None, 0) {
                    NB_TESTS.fetch_add(1, Ordering::Relaxed);
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
                                now.get_line_no()
                            );
                            NB_ERRORS.fetch_add(1, Ordering::Relaxed);
                        }
                        std::cmp::Ordering::Less => {
                            test_log!(
                                logfile,
                                "Internal error validating instance line {}\n",
                                now.get_line_no()
                            );
                            NB_ERRORS.fetch_add(1, Ordering::Relaxed);
                        }
                        _ => {}
                    }
                    xml_free_doc(doc);
                } else {
                    test_log!(
                        logfile,
                        "Failed to parse valid instance line {}\n",
                        now.get_line_no()
                    );
                    NB_ERRORS.fetch_add(1, Ordering::Relaxed);
                }
                reset_last_error();
                if mem != xml_mem_used() && EXTRA_MEMORY_FROM_RESOLVER.load(Ordering::Relaxed) == 0
                {
                    test_log!(
                        logfile,
                        "Validation of instance line {} leaked {}\n",
                        now.get_line_no(),
                        xml_mem_used() - mem
                    );
                    xml_memory_dump();
                    NB_LEAKS.fetch_add(1, Ordering::Relaxed);
                }
            } else {
                eprintln!(
                    "Failed to find test in <valid> line {}\n",
                    now.get_line_no(),
                );
            }
            tmp = get_next(Some(now), "following-sibling::valid[1]");
        }
        // now scan all the siblings of correct to process the <invalid> tests
        let mut tmp = get_next(Some(cur), "following-sibling::invalid[1]");
        while let Some(now) = tmp {
            if let Some(mut test) = get_next(Some(now), "./*") {
                buf.clear();
                let test_doc = test.doc;
                test.dump_memory(&mut buf, test_doc, 0, 0);

                // We are ready to run the test
                mem = xml_mem_used();
                EXTRA_MEMORY_FROM_RESOLVER.store(0, Ordering::Relaxed);
                let buffer = buf.clone();
                if let Some(doc) = xml_read_memory(buffer, Some("test"), None, 0) {
                    NB_TESTS.fetch_add(1, Ordering::Relaxed);
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
                                now.get_line_no()
                            );
                            NB_ERRORS.fetch_add(1, Ordering::Relaxed);
                        }
                        std::cmp::Ordering::Less => {
                            test_log!(
                                logfile,
                                "Internal error validating instance line {}\n",
                                now.get_line_no()
                            );
                            NB_ERRORS.fetch_add(1, Ordering::Relaxed);
                        }
                        _ => {}
                    }
                    xml_free_doc(doc);
                } else {
                    test_log!(
                        logfile,
                        "Failed to parse valid instance line {}\n",
                        now.get_line_no()
                    );
                    NB_ERRORS.fetch_add(1, Ordering::Relaxed);
                }
                reset_last_error();
                if mem != xml_mem_used() && EXTRA_MEMORY_FROM_RESOLVER.load(Ordering::Relaxed) == 0
                {
                    test_log!(
                        logfile,
                        "Validation of instance line {} leaked {}\n",
                        now.get_line_no(),
                        xml_mem_used() - mem
                    );
                    xml_memory_dump();
                    NB_LEAKS.fetch_add(1, Ordering::Relaxed);
                }
            } else {
                eprintln!(
                    "Failed to find test in <invalid> line {}",
                    now.get_line_no()
                );
            }
            tmp = get_next(Some(now), "following-sibling::invalid[1]");
        }

        if !rng.is_null() {
            xml_relaxng_free(rng);
        }
        reset_last_error();
        if memt != xml_mem_used() && memt != 0 {
            test_log!(
                logfile,
                "Validation of tests starting line {} leaked {}\n",
                cur.get_line_no(),
                xml_mem_used() - memt
            );
            NB_LEAKS.fetch_add(1, Ordering::Relaxed);
        }
        ret
    }
}

unsafe fn xsd_test_suite(logfile: &mut Option<File>, cur: XmlNodePtr) -> c_int {
    unsafe {
        if VERBOSE != 0 {
            if let Some(doc) = get_string(cur, "string(documentation)") {
                println!("Suite {doc}");
            }
        }
        let mut cur = get_next(Some(cur), "./testCase[1]");
        while let Some(now) = cur {
            xsd_test_case(logfile, Some(now));
            cur = get_next(Some(now), "following-sibling::testCase[1]");
        }

        0
    }
}

unsafe fn xsd_test(logfile: &mut Option<File>) -> c_int {
    unsafe {
        let filename = "test/xsdtest/xsdtestsuite.xml";
        let mut ret: c_int = 0;

        let Some(doc) = xml_read_file(filename, None, XmlParserOption::XmlParseNoEnt as _) else {
            eprintln!("Failed to parse {}", filename);
            return -1;
        };
        println!("## XML Schemas datatypes test suite from James Clark");
        test_log!(logfile, "filename: {}\n", filename);

        let Some(cur) = doc.get_root_element().filter(|cur| cur.name == "testSuite") else {
            eprintln!("Unexpected format {}", filename);
            ret = -1;
            xml_free_doc(doc);
            return ret;
        };

        let mut cur = get_next(cur.into(), "./testSuite[1]");
        if cur.is_none_or(|cur| cur.name != "testSuite") {
            eprintln!("Unexpected format {}", filename);
            ret = -1;
            xml_free_doc(doc);
            return ret;
        }
        while let Some(now) = cur {
            xsd_test_suite(logfile, now);
            cur = get_next(Some(now), "following-sibling::testSuite[1]");
        }

        xml_free_doc(doc);
        ret
    }
}

unsafe fn rng_test_suite(logfile: &mut Option<File>, cur: XmlNodePtr) -> i32 {
    unsafe {
        if VERBOSE != 0 {
            if let Some(doc) = get_string(cur, "string(documentation)") {
                println!("Suite {doc}");
            } else if let Some(doc) = get_string(cur, "string(section)") {
                println!("Section {doc}");
            }
        }
        let mut cur = get_next(Some(cur), "./testSuite[1]");
        while let Some(now) = cur {
            xsd_test_suite(logfile, now);
            cur = get_next(Some(now), "following-sibling::testSuite[1]");
        }

        0
    }
}

unsafe fn rng_test1(logfile: &mut Option<File>) -> c_int {
    unsafe {
        let filename = "test/relaxng/OASIS/spectest.xml";
        let mut ret: c_int = 0;

        let Some(doc) = xml_read_file(filename, None, XmlParserOption::XmlParseNoEnt as _) else {
            eprintln!("Failed to parse {}", filename);
            return -1;
        };
        println!("## Relax NG test suite from James Clark");
        test_log!(logfile, "filename: {}\n", filename);

        let Some(cur) = doc.get_root_element().filter(|cur| cur.name == "testSuite") else {
            eprintln!("Unexpected format {}", filename);
            ret = -1;
            xml_free_doc(doc);
            return ret;
        };

        let mut cur = get_next(cur.into(), "./testSuite[1]");
        if cur.is_none_or(|cur| cur.name != "testSuite") {
            eprintln!("Unexpected format {}", filename);
            ret = -1;
            xml_free_doc(doc);
            return ret;
        }
        while let Some(now) = cur {
            rng_test_suite(logfile, now);
            cur = get_next(Some(now), "following-sibling::testSuite[1]");
        }

        xml_free_doc(doc);
        ret
    }
}

unsafe fn rng_test2(logfile: &mut Option<File>) -> c_int {
    unsafe {
        let filename = "test/relaxng/testsuite.xml";
        let mut ret: c_int = 0;

        let Some(doc) = xml_read_file(filename, None, XmlParserOption::XmlParseNoEnt as _) else {
            eprintln!("Failed to parse {}", filename);
            return -1;
        };
        println!("## Relax NG test suite for libxml2");
        test_log!(logfile, "filename: {}\n", filename);

        let Some(cur) = doc.get_root_element().filter(|cur| cur.name == "testSuite") else {
            eprintln!("Unexpected format {}", filename);
            ret = -1;
            xml_free_doc(doc);
            return ret;
        };

        let mut cur = get_next(cur.into(), "./testSuite[1]");
        if cur.is_none_or(|cur| cur.name != "testSuite") {
            eprintln!("Unexpected format {}", filename);
            ret = -1;
            xml_free_doc(doc);
            return ret;
        }
        while let Some(now) = cur {
            xsd_test_suite(logfile, now);
            cur = get_next(Some(now), "following-sibling::testSuite[1]");
        }

        xml_free_doc(doc);
        ret
    }
}

unsafe fn xstc_test_instance(
    logfile: &mut Option<File>,
    cur: XmlNodePtr,
    schemas: XmlSchemaPtr,
    spath: *const XmlChar,
    base: &str,
) -> c_int {
    unsafe {
        let mut ctxt: XmlSchemaValidCtxtPtr = null_mut();
        let mut ret: c_int;

        reset_last_error();
        TEST_ERRORS_SIZE.store(0, Ordering::Relaxed);
        TEST_ERRORS.lock().unwrap()[0] = 0;
        let mem: c_int = xml_mem_used();
        if let Some(href) = get_string(cur, "string(ts:instanceDocument/@xlink:href)")
            .filter(|href| !href.is_empty())
        {
            if let Some(path) = build_uri(&href, base) {
                if !check_test_file(&path) {
                    test_log!(
                        logfile,
                        "schemas for testGroup line {} is missing: {}\n",
                        cur.get_line_no(),
                        path
                    );
                    ret = -1;
                    // goto done;
                } else if let Some(validity) = get_string(cur, "string(ts:expected/@validity)") {
                    NB_TESTS.fetch_add(1, Ordering::Relaxed);
                    if let Some(doc) =
                        xml_read_file(&path, None, XmlParserOption::XmlParseNoEnt as i32)
                    {
                        ctxt = xml_schema_new_valid_ctxt(schemas);
                        (*ctxt).set_errors(
                            Some(test_error_handler),
                            Some(test_error_handler),
                            Some(GenericErrorContext::new(ctxt)) as _,
                        );
                        ret = xml_schema_validate_doc(ctxt, doc);

                        if validity == "valid" {
                            match ret.cmp(&0) {
                                std::cmp::Ordering::Greater => {
                                    test_log!(
                                        logfile,
                                        "valid instance {} failed to validate against {}\n",
                                        path,
                                        CStr::from_ptr(spath as _).to_string_lossy()
                                    );
                                    NB_ERRORS.fetch_add(1, Ordering::Relaxed);
                                }
                                std::cmp::Ordering::Less => {
                                    test_log!(
                                        logfile,
                                        "valid instance {} got internal error validating {}\n",
                                        path,
                                        CStr::from_ptr(spath as _).to_string_lossy()
                                    );
                                    NB_INTERNALS.fetch_add(1, Ordering::Relaxed);
                                    NB_ERRORS.fetch_add(1, Ordering::Relaxed);
                                }
                                _ => {}
                            }
                        } else if validity == "invalid" {
                            if ret == 0 {
                                test_log!(
                                    logfile,
                                    "Failed to detect invalid instance {} against {}\n",
                                    path,
                                    CStr::from_ptr(spath as _).to_string_lossy()
                                );
                                NB_ERRORS.fetch_add(1, Ordering::Relaxed);
                            }
                        } else {
                            test_log!(
                                logfile,
                                "instanceDocument line {} has unexpected validity value{}\n",
                                cur.get_line_no(),
                                validity
                            );
                            ret = -1;
                            // goto done;
                        }
                        xml_free_doc(doc);
                    } else {
                        eprintln!("instance {} fails to parse", path);
                        ret = -1;
                        NB_ERRORS.fetch_add(1, Ordering::Relaxed);
                        // goto done;
                    }
                } else {
                    eprintln!(
                        "instanceDocument line {} misses expected validity",
                        cur.get_line_no()
                    );
                    ret = -1;
                }
            } else {
                eprintln!(
                    "Failed to build path to schemas testGroup line {} : {}",
                    cur.get_line_no(),
                    href
                );
                ret = -1;
            }
        } else {
            test_log!(
                logfile,
                "testGroup line {} misses href for schemaDocument\n",
                cur.get_line_no()
            );
            ret = -1;
        }

        // done:
        if !ctxt.is_null() {
            xml_schema_free_valid_ctxt(ctxt);
        }
        reset_last_error();
        if mem != xml_mem_used() {
            test_log!(
                logfile,
                "Validation of tests starting line {} leaked {}\n",
                cur.get_line_no(),
                xml_mem_used() - mem
            );
            NB_LEAKS.fetch_add(1, Ordering::Relaxed);
        }
        ret
    }
}

unsafe fn xstc_test_group(logfile: &mut Option<File>, cur: XmlNodePtr, base: &str) -> c_int {
    unsafe {
        let mut p = None::<String>;
        let mut schemas: XmlSchemaPtr = null_mut();
        let ctxt: XmlSchemaParserCtxtPtr;
        let mut ret: c_int = 0;

        reset_last_error();
        TEST_ERRORS_SIZE.store(0, Ordering::Relaxed);
        TEST_ERRORS.lock().unwrap()[0] = 0;
        let mem: c_int = xml_mem_used();
        if let Some(href) = get_string(cur, "string(ts:schemaTest/ts:schemaDocument/@xlink:href)")
            .filter(|href| !href.is_empty())
        {
            if let Some(path) = build_uri(&href, base) {
                if !check_test_file(&path) {
                    test_log!(
                        logfile,
                        "schemas for testGroup line {} is missing: {}\n",
                        cur.get_line_no(),
                        path
                    );
                    ret = -1;
                    // goto done;
                } else if let Some(validity) =
                    get_string(cur, "string(ts:schemaTest/ts:expected/@validity)")
                {
                    NB_TESTS.fetch_add(1, Ordering::Relaxed);
                    if validity == "valid" {
                        NB_SCHEMATAS.fetch_add(1, Ordering::Relaxed);
                        let cpath = CString::new(path.as_str()).unwrap();
                        ctxt = xml_schema_new_parser_ctxt(&path);
                        (*ctxt).set_errors(
                            Some(test_error_handler),
                            Some(test_error_handler),
                            Some(GenericErrorContext::new(ctxt)),
                        );
                        schemas = (*ctxt).parse();
                        xml_schema_free_parser_ctxt(ctxt);
                        if schemas.is_null() {
                            test_log!(logfile, "valid schemas {} failed to parse\n", path);
                            ret = 1;
                            NB_ERRORS.fetch_add(1, Ordering::Relaxed);
                        }
                        if ret == 0
                            && !strstr(
                                TEST_ERRORS.lock().unwrap().as_ptr() as _,
                                c"nimplemented".as_ptr(),
                            )
                            .is_null()
                        {
                            test_log!(
                                logfile,
                                "valid schemas {} hit an unimplemented block\n",
                                path
                            );
                            ret = 1;
                            NB_UNIMPLEMENTED.fetch_add(1, Ordering::Relaxed);
                            NB_ERRORS.fetch_add(1, Ordering::Relaxed);
                        }
                        let mut instance = get_next(Some(cur), "./ts:instanceTest[1]");
                        while let Some(now) = instance {
                            if !schemas.is_null() {
                                xstc_test_instance(
                                    logfile,
                                    now,
                                    schemas,
                                    cpath.as_ptr() as *const u8,
                                    base,
                                );
                            } else {
                                // We'll automatically mark the instances as failed
                                // if the schema was broken.
                                NB_ERRORS.fetch_add(1, Ordering::Relaxed);
                            }
                            instance = get_next(Some(now), "following-sibling::ts:instanceTest[1]");
                        }
                    } else if validity == "invalid" {
                        NB_SCHEMATAS.fetch_add(1, Ordering::Relaxed);
                        ctxt = xml_schema_new_parser_ctxt(&path);
                        (*ctxt).set_errors(
                            Some(test_error_handler),
                            Some(test_error_handler),
                            Some(GenericErrorContext::new(ctxt)),
                        );
                        schemas = (*ctxt).parse();
                        xml_schema_free_parser_ctxt(ctxt);
                        if !schemas.is_null() {
                            test_log!(logfile, "Failed to detect error in schemas {}\n", path);
                            NB_ERRORS.fetch_add(1, Ordering::Relaxed);
                            ret = 1;
                        }
                        if ret == 0
                            && !strstr(
                                TEST_ERRORS.lock().unwrap().as_ptr() as _,
                                c"nimplemented".as_ptr(),
                            )
                            .is_null()
                        {
                            NB_UNIMPLEMENTED.fetch_add(1, Ordering::Relaxed);
                            test_log!(
                                logfile,
                                "invalid schemas {} hit an unimplemented block\n",
                                path
                            );
                            ret = 1;
                            NB_ERRORS.fetch_add(1, Ordering::Relaxed);
                        }
                    } else {
                        test_log!(
                            logfile,
                            "testGroup line {} misses unexpected validity value{}\n",
                            cur.get_line_no(),
                            validity
                        );
                        ret = -1;
                        // goto done;
                    }
                } else {
                    test_log!(
                        logfile,
                        "testGroup line {} misses expected validity\n",
                        cur.get_line_no()
                    );
                    ret = -1;
                }
                p = Some(path);
            } else {
                test_log!(
                    logfile,
                    "Failed to build path to schemas testGroup line {} : {}\n",
                    cur.get_line_no(),
                    href
                );
                ret = -1;
            }
        } else {
            test_log!(
                logfile,
                "testGroup line {} misses href for schemaDocument\n",
                cur.get_line_no()
            );
            ret = -1;
        }

        // done:
        if !schemas.is_null() {
            xml_schema_free(schemas);
        }
        reset_last_error();
        if mem != xml_mem_used() && EXTRA_MEMORY_FROM_RESOLVER.load(Ordering::Relaxed) == 0 {
            test_log!(
                logfile,
                "Processing test line {} {} leaked {}\n",
                cur.get_line_no(),
                p.as_deref().unwrap_or("(null)"),
                xml_mem_used() - mem
            );
            NB_LEAKS.fetch_add(1, Ordering::Relaxed);
        }
        ret
    }
}

unsafe fn xstc_metadata(logfile: &mut Option<File>, metadata: &str, base: &str) -> c_int {
    unsafe {
        let mut ret: c_int = 0;

        let Some(doc) = xml_read_file(metadata, None, XmlParserOption::XmlParseNoEnt as _) else {
            eprintln!("Failed to parse {metadata}");
            return -1;
        };

        let Some(cur) = doc.get_root_element().filter(|cur| cur.name == "testSet") else {
            eprintln!("Unexpected format {metadata}");
            return -1;
        };
        let contributor = cur.get_prop("contributor").unwrap_or("Unknown".to_owned());
        let name = cur.get_prop("name").unwrap_or("Unknown".to_owned());
        println!("## {contributor} test suite for Schemas version {name}");

        let mut cur = get_next(cur.into(), "./ts:testGroup[1]");
        if cur.is_none_or(|cur| cur.name != "testGroup") {
            eprintln!("Unexpected format {metadata}");
            ret = -1;
            xml_free_doc(doc);
            return ret;
        }
        while let Some(now) = cur {
            xstc_test_group(logfile, now, base);
            cur = get_next(Some(now), "following-sibling::ts:testGroup[1]");
        }

        xml_free_doc(doc);
        ret
    }
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

        old_errors = NB_ERRORS.load(Ordering::Relaxed);
        old_tests = NB_TESTS.load(Ordering::Relaxed);
        old_leaks = NB_LEAKS.load(Ordering::Relaxed);
        xsd_test(&mut logfile);
        println!(
            "Ran {} tests, {} errors, {} leaks",
            NB_TESTS.load(Ordering::Relaxed) - old_tests,
            NB_ERRORS.load(Ordering::Relaxed) - old_errors,
            NB_LEAKS.load(Ordering::Relaxed) - old_leaks,
        );
        if NB_ERRORS.load(Ordering::Relaxed) - old_errors == 10 {
            println!("10 errors were expected");
            NB_ERRORS.store(old_errors, Ordering::Relaxed);
        } else {
            println!(
                "10 errors were expected, got {} errors",
                NB_ERRORS.load(Ordering::Relaxed) - old_errors,
            );
            NB_ERRORS.store(old_errors + 1, Ordering::Relaxed);
        }

        old_errors = NB_ERRORS.load(Ordering::Relaxed);
        old_tests = NB_TESTS.load(Ordering::Relaxed);
        old_leaks = NB_LEAKS.load(Ordering::Relaxed);
        rng_test1(&mut logfile);
        if NB_ERRORS.load(Ordering::Relaxed) == old_errors
            && NB_LEAKS.load(Ordering::Relaxed) == old_leaks
        {
            println!(
                "Ran {} tests, no errors",
                NB_TESTS.load(Ordering::Relaxed) - old_tests
            );
        } else {
            println!(
                "Ran {} tests, {} errors, {} leaks",
                NB_TESTS.load(Ordering::Relaxed) - old_tests,
                NB_ERRORS.load(Ordering::Relaxed) - old_errors,
                NB_LEAKS.load(Ordering::Relaxed) - old_leaks,
            );
        }

        old_errors = NB_ERRORS.load(Ordering::Relaxed);
        old_tests = NB_TESTS.load(Ordering::Relaxed);
        old_leaks = NB_LEAKS.load(Ordering::Relaxed);
        rng_test2(&mut logfile);
        if NB_ERRORS.load(Ordering::Relaxed) == old_errors
            && NB_LEAKS.load(Ordering::Relaxed) == old_leaks
        {
            println!(
                "Ran {} tests, no errors",
                NB_TESTS.load(Ordering::Relaxed) - old_tests
            );
        } else {
            println!(
                "Ran {} tests, {} errors, {} leaks",
                NB_TESTS.load(Ordering::Relaxed) - old_tests,
                NB_ERRORS.load(Ordering::Relaxed) - old_errors,
                NB_LEAKS.load(Ordering::Relaxed) - old_leaks,
            );
        }

        old_errors = NB_ERRORS.load(Ordering::Relaxed);
        old_tests = NB_TESTS.load(Ordering::Relaxed);
        old_leaks = NB_LEAKS.load(Ordering::Relaxed);
        NB_INTERNALS.store(0, Ordering::Relaxed);
        NB_SCHEMATAS.store(0, Ordering::Relaxed);
        xstc_metadata(
            &mut logfile,
            "xstc/Tests/Metadata/NISTXMLSchemaDatatypes.testSet",
            "xstc/Tests/Metadata/",
        );
        if NB_ERRORS.load(Ordering::Relaxed) == old_errors
            && NB_LEAKS.load(Ordering::Relaxed) == old_leaks
        {
            println!(
                "Ran {} tests ({} schemata), no errors",
                NB_TESTS.load(Ordering::Relaxed) - old_tests,
                NB_SCHEMATAS.load(Ordering::Relaxed),
            );
        } else {
            println!(
                "Ran {} tests ({} schemata), {} errors ({} internals), {} leaks",
                NB_TESTS.load(Ordering::Relaxed) - old_tests,
                NB_SCHEMATAS.load(Ordering::Relaxed),
                NB_ERRORS.load(Ordering::Relaxed) - old_errors,
                NB_INTERNALS.load(Ordering::Relaxed),
                NB_LEAKS.load(Ordering::Relaxed) - old_leaks,
            );
        }

        old_errors = NB_ERRORS.load(Ordering::Relaxed);
        old_tests = NB_TESTS.load(Ordering::Relaxed);
        old_leaks = NB_LEAKS.load(Ordering::Relaxed);
        NB_INTERNALS.store(0, Ordering::Relaxed);
        NB_SCHEMATAS.store(0, Ordering::Relaxed);
        xstc_metadata(
            &mut logfile,
            "xstc/Tests/Metadata/SunXMLSchema1-0-20020116.testSet",
            "xstc/Tests/",
        );
        if NB_ERRORS.load(Ordering::Relaxed) == old_errors
            && NB_LEAKS.load(Ordering::Relaxed) == old_leaks
        {
            println!(
                "Ran {} tests ({} schemata), no errors",
                NB_TESTS.load(Ordering::Relaxed) - old_tests,
                NB_SCHEMATAS.load(Ordering::Relaxed),
            );
        } else {
            println!(
                "Ran {} tests ({} schemata), {} errors ({} internals), {} leaks",
                NB_TESTS.load(Ordering::Relaxed) - old_tests,
                NB_SCHEMATAS.load(Ordering::Relaxed),
                NB_ERRORS.load(Ordering::Relaxed) - old_errors,
                NB_INTERNALS.load(Ordering::Relaxed),
                NB_LEAKS.load(Ordering::Relaxed) - old_leaks,
            );
            println!("Some errors were expected.");
            NB_ERRORS.store(old_errors, Ordering::Relaxed);
        }

        old_errors = NB_ERRORS.load(Ordering::Relaxed);
        old_tests = NB_TESTS.load(Ordering::Relaxed);
        old_leaks = NB_LEAKS.load(Ordering::Relaxed);
        NB_INTERNALS.store(0, Ordering::Relaxed);
        NB_SCHEMATAS.store(0, Ordering::Relaxed);
        xstc_metadata(
            &mut logfile,
            "xstc/Tests/Metadata/MSXMLSchema1-0-20020116.testSet",
            "xstc/Tests/",
        );
        if NB_ERRORS.load(Ordering::Relaxed) == old_errors
            && NB_LEAKS.load(Ordering::Relaxed) == old_leaks
        {
            println!(
                "Ran {} tests ({} schemata), no errors",
                NB_TESTS.load(Ordering::Relaxed) - old_tests,
                NB_SCHEMATAS.load(Ordering::Relaxed),
            );
        } else {
            println!(
                "Ran {} tests ({} schemata), {} errors ({} internals), {} leaks",
                NB_TESTS.load(Ordering::Relaxed) - old_tests,
                NB_SCHEMATAS.load(Ordering::Relaxed),
                NB_ERRORS.load(Ordering::Relaxed) - old_errors,
                NB_INTERNALS.load(Ordering::Relaxed),
                NB_LEAKS.load(Ordering::Relaxed) - old_leaks,
            );
            println!("Some errors were expected.");
            NB_ERRORS.store(old_errors, Ordering::Relaxed);
        }

        if NB_ERRORS.load(Ordering::Relaxed) == 0 && NB_LEAKS.load(Ordering::Relaxed) == 0 {
            ret = 0;
            println!(
                "Total {} tests, no errors",
                NB_TESTS.load(Ordering::Relaxed)
            );
        } else {
            ret = 1;
            println!(
                "Total {} tests, {} errors, {} leaks",
                NB_TESTS.load(Ordering::Relaxed),
                NB_ERRORS.load(Ordering::Relaxed),
                NB_LEAKS.load(Ordering::Relaxed),
            );
        }
        xml_xpath_free_context(CTXT_XPATH.load(Ordering::Relaxed));
        xml_cleanup_parser();
        xml_memory_dump();
    }

    assert_eq!(ret, 0);
}
