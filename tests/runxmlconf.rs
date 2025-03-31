//! Rust implementation of original libxml2's `runxmlconf.c`.  
//! If you want this to work, please download https://www.w3.org/XML/Test/xmlts20130923.tar.gz from [W3C](http://www.w3.org/XML/Test/).

use std::{
    env::args,
    ffi::{CStr, c_int},
    fs::{File, metadata},
    os::fd::AsRawFd,
    ptr::null_mut,
    sync::{
        Mutex,
        atomic::{AtomicI32, AtomicPtr, AtomicUsize, Ordering},
    },
};

use exml::{
    error::{XmlError, XmlErrorDomain, XmlErrorLevel, XmlParserErrors},
    globals::{
        GenericErrorContext, get_last_error, reset_last_error, set_get_warnings_default_value,
        set_pedantic_parser_default_value, set_structured_error,
    },
    libxml::{
        parser::{
            XmlParserOption, xml_cleanup_parser, xml_init_parser, xml_set_external_entity_loader,
        },
        xmlmemory::{
            xml_mem_display_last, xml_mem_free, xml_mem_malloc, xml_mem_realloc, xml_mem_setup,
            xml_mem_used, xml_memory_dump, xml_memory_strdup,
        },
    },
    parser::{
        XmlParserCtxtPtr, XmlParserInput, xml_ctxt_read_file, xml_free_parser_ctxt,
        xml_new_parser_ctxt, xml_read_file,
    },
    tree::{NodeCommon, XmlDocProperties, XmlDocPtr, XmlElementType, XmlNodePtr, xml_free_doc},
    xpath::{
        XmlXPathContext, xml_xpath_context_set_cache, xml_xpath_free_context, xml_xpath_new_context,
    },
};
use libc::fdopen;

static mut VERBOSE: c_int = 0;

const NB_EXPECTED_ERRORS: usize = 15;

const SKIPPED_TESTS: &[&str] = &[
    // http://lists.w3.org/Archives/Public/public-xml-testsuite/2008Jul/0000.html
    "rmt-ns10-035",
];

// File name and path utilities

fn check_test_file(filename: &str) -> bool {
    match metadata(filename) {
        Ok(meta) => meta.is_file(),
        _ => false,
    }
}

fn compose_dir(dir: Option<&str>, path: &str) -> String {
    let Some(dir) = dir else {
        return path.to_owned();
    };
    format!("{dir}/{path}")
}

// Libxml2 specific routines

static NB_SKIPPED: AtomicI32 = AtomicI32::new(0);
static NB_TESTS: AtomicI32 = AtomicI32::new(0);
static NB_ERRORS: AtomicI32 = AtomicI32::new(0);
static NB_LEAKS: AtomicI32 = AtomicI32::new(0);

// We need to trap calls to the resolver to not account memory for the catalog
// and not rely on any external resources.
unsafe fn test_external_entity_loader(
    url: Option<&str>,
    _id: Option<&str>,
    ctxt: XmlParserCtxtPtr,
) -> Option<XmlParserInput> {
    unsafe { XmlParserInput::from_filename(&mut *ctxt, url.unwrap()) }
}

// Trapping the error messages at the generic level to grab the equivalent of
// stderr messages on CLI tools.
static TEST_ERRORS: Mutex<[u8; 32769]> = Mutex::new([0; 32769]);
static TEST_ERRORS_SIZE: AtomicUsize = AtomicUsize::new(0);
static NB_ERROR: AtomicI32 = AtomicI32::new(0);
static NB_FATAL: AtomicI32 = AtomicI32::new(0);

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

fn test_error_handler(_user_data: Option<GenericErrorContext>, error: &XmlError) {
    if TEST_ERRORS_SIZE.load(Ordering::Relaxed) >= 32768 {
        return;
    }

    let msg = format!(
        "{}:{}: {}\n",
        error.file().unwrap_or("entity"),
        error.line(),
        error.message().unwrap_or("")
    );
    let mut errors = TEST_ERRORS.lock().unwrap();
    let written = TEST_ERRORS_SIZE.load(Ordering::Relaxed);
    let remain = errors.len() - written;
    let res = remain.min(msg.len());
    errors[written..written + res].copy_from_slice(&msg.as_bytes()[..res]);
    if error.level() == XmlErrorLevel::XmlErrFatal {
        NB_FATAL.fetch_add(1, Ordering::Relaxed);
    } else if error.level() == XmlErrorLevel::XmlErrError {
        NB_ERROR.fetch_add(1, Ordering::Relaxed);
    }
    if TEST_ERRORS_SIZE.load(Ordering::Relaxed) + res as usize >= 32768 {
        // buffer is full
        TEST_ERRORS_SIZE.store(32768, Ordering::Relaxed);
        errors[TEST_ERRORS_SIZE.load(Ordering::Relaxed)] = 0;
    } else {
        TEST_ERRORS_SIZE.fetch_add(res as usize, Ordering::Relaxed);
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
        set_structured_error(Some(test_error_handler), None);
    }
}

// Run the xmlconf test if found

unsafe fn xmlconf_test_invalid(
    logfile: &mut Option<File>,
    id: &str,
    filename: &str,
    options: i32,
) -> i32 {
    unsafe {
        let mut ret: i32 = 1;

        let ctxt: XmlParserCtxtPtr = xml_new_parser_ctxt();
        if ctxt.is_null() {
            test_log!(logfile, "test {id} : {filename} out of memory\n",);
            return 0;
        }
        if let Some(doc) = xml_ctxt_read_file(ctxt, filename, None, options) {
            // invalidity should be reported both in the context and in the document
            if (*ctxt).valid != 0 || doc.properties & XmlDocProperties::XmlDocDTDValid as i32 != 0 {
                test_log!(
                    logfile,
                    "test {id} : {filename} failed to detect invalid document\n",
                );
                NB_ERRORS.fetch_add(1, Ordering::Relaxed);
                ret = 0;
            }
            xml_free_doc(doc);
        } else {
            test_log!(
                logfile,
                "test {id} : {filename} invalid document turned not well-formed too\n",
            );
        }
        xml_free_parser_ctxt(ctxt);
        ret
    }
}

unsafe fn xmlconf_test_valid(
    logfile: &mut Option<File>,
    id: &str,
    filename: &str,
    options: c_int,
) -> c_int {
    unsafe {
        let mut ret: c_int = 1;

        let ctxt: XmlParserCtxtPtr = xml_new_parser_ctxt();
        if ctxt.is_null() {
            test_log!(logfile, "test {id} : {filename} out of memory\n",);
            return 0;
        }
        if let Some(doc) = xml_ctxt_read_file(ctxt, filename, None, options) {
            // validity should be reported both in the context and in the document
            if (*ctxt).valid == 0 || doc.properties & XmlDocProperties::XmlDocDTDValid as i32 == 0 {
                test_log!(
                    logfile,
                    "test {id} : {filename} failed to validate a valid document\n",
                );
                NB_ERRORS.fetch_add(1, Ordering::Relaxed);
                ret = 0;
            }
            xml_free_doc(doc);
        } else {
            test_log!(
                logfile,
                "test {id} : {filename} failed to parse a valid document\n",
            );
            NB_ERRORS.fetch_add(1, Ordering::Relaxed);
            ret = 0;
        }
        xml_free_parser_ctxt(ctxt);
        ret
    }
}

unsafe fn xmlconf_test_not_nswf(
    logfile: &mut Option<File>,
    id: &str,
    filename: &str,
    options: i32,
) -> i32 {
    unsafe {
        let mut ret: i32 = 1;

        // In case of Namespace errors, libxml2 will still parse the document
        // but log a Namespace error.
        if let Some(doc) = xml_read_file(filename, None, options) {
            let last_error = get_last_error();
            if last_error.code() == XmlParserErrors::XmlErrOK
                || last_error.domain() != XmlErrorDomain::XmlFromNamespace
            {
                test_log!(
                    logfile,
                    "test {id} : {filename} failed to detect namespace error\n",
                );
                NB_ERRORS.fetch_add(1, Ordering::Relaxed);
                ret = 0;
            }
            xml_free_doc(doc);
        } else {
            test_log!(logfile, "test {id} : {filename} failed to parse the XML\n",);
            NB_ERRORS.fetch_add(1, Ordering::Relaxed);
            ret = 0;
        }
        ret
    }
}

unsafe fn xmlconf_test_not_wf(
    logfile: &mut Option<File>,
    id: &str,
    filename: &str,
    options: i32,
) -> i32 {
    unsafe {
        let mut ret: i32 = 1;

        if let Some(doc) = xml_read_file(filename, None, options) {
            test_log!(
                logfile,
                "test {id} : {filename} failed to detect not well formedness\n",
            );
            NB_ERRORS.fetch_add(1, Ordering::Relaxed);
            xml_free_doc(doc);
            ret = 0;
        }
        ret
    }
}

unsafe fn xmlconf_test_item(logfile: &mut Option<File>, doc: XmlDocPtr, cur: XmlNodePtr) -> c_int {
    unsafe {
        let mut ret: c_int = -1;
        let mut options: c_int = 0;
        let mut nstest: c_int = 0;

        TEST_ERRORS_SIZE.store(0, Ordering::Relaxed);
        TEST_ERRORS.lock().unwrap()[0] = 0;
        NB_ERROR.store(0, Ordering::Relaxed);
        NB_FATAL.store(0, Ordering::Relaxed);
        let Some(id) = cur.get_prop("ID") else {
            test_log!(logfile, "test missing ID, line {}\n", cur.get_line_no());
            return ret;
        };
        for &skipped in SKIPPED_TESTS {
            if skipped == id {
                test_log!(logfile, "Skipping test {id} from skipped list\n",);
                ret = 0;
                NB_SKIPPED.fetch_add(1, Ordering::Relaxed);
                // goto error;
                return ret;
            }
        }
        let Some(typ) = cur.get_prop("TYPE") else {
            test_log!(logfile, "test {id} missing TYPE\n",);
            return ret;
        };
        let Some(uri) = cur.get_prop("URI") else {
            test_log!(logfile, "test {id} missing URI\n",);
            return ret;
        };
        let base = cur.get_base(Some(doc));
        let filename = compose_dir(base.as_deref(), &uri);
        if !check_test_file(filename.as_str()) {
            test_log!(logfile, "test {id} missing file {filename} \n",);
            return ret;
        }

        let version = cur.get_prop("VERSION");
        let entities = cur.get_prop("ENTITIES");
        if entities.as_deref() != Some("none") {
            options |= XmlParserOption::XmlParseDTDLoad as i32;
            options |= XmlParserOption::XmlParseNoEnt as i32;
        }
        let rec = cur.get_prop("RECOMMENDATION");
        if rec.as_deref().is_none_or(|rec| {
            rec == "XML1.0"
                || rec == "XML1.0-errata2e"
                || rec == "XML1.0-errata3e"
                || rec == "XML1.0-errata4e"
        }) {
            if let Some(version) = version.as_deref().filter(|&v| v != "1.0") {
                test_log!(logfile, "Skipping test {id} for {version}\n",);
                ret = 0;
                NB_SKIPPED.fetch_add(1, Ordering::Relaxed);
                // goto error;
                return ret;
            }
            ret = 1;
        } else if rec.as_deref() == Some("NS1.0") || rec.as_deref() == Some("NS1.0-errata1e") {
            ret = 1;
            nstest = 1;
        } else {
            let rec = rec.as_deref().unwrap();
            test_log!(logfile, "Skipping test {id} for REC {rec}\n",);
            ret = 0;
            NB_SKIPPED.fetch_add(1, Ordering::Relaxed);
            // goto error;
            return ret;
        }
        let edition = cur.get_prop("EDITION");
        if edition.as_deref().filter(|e| !e.contains('5')).is_some() {
            // test limited to all versions before 5th
            options |= XmlParserOption::XmlParseOld10 as i32;
        }

        // Reset errors and check memory usage before the test
        reset_last_error();
        TEST_ERRORS_SIZE.store(0, Ordering::Relaxed);
        TEST_ERRORS.lock().unwrap()[0] = 0;
        let mem: c_int = xml_mem_used();

        if typ == "not-wf" {
            if nstest == 0 {
                xmlconf_test_not_wf(logfile, &id, &filename, options);
            } else {
                xmlconf_test_not_nswf(logfile, &id, &filename, options);
            }
        } else if typ == "valid" {
            options |= XmlParserOption::XmlParseDTDValid as i32;
            xmlconf_test_valid(logfile, &id, &filename, options);
        } else if typ == "invalid" {
            options |= XmlParserOption::XmlParseDTDValid as i32;
            xmlconf_test_invalid(logfile, &id, &filename, options);
        } else if typ == "error" {
            test_log!(logfile, "Skipping error test {id} \n",);
            ret = 0;
            NB_SKIPPED.fetch_add(1, Ordering::Relaxed);
            // goto error;
            return ret;
        } else {
            test_log!(logfile, "test {id} unknown TYPE value {typ}\n",);
            ret = -1;
            // goto error;
            return ret;
        }

        // Reset errors and check memory usage after the test
        reset_last_error();
        let is_final: c_int = xml_mem_used();
        if is_final > mem {
            test_log!(
                logfile,
                "test {id} : {filename} leaked {} bytes\n",
                is_final - mem
            );
            NB_LEAKS.fetch_add(1, Ordering::Relaxed);
            let fp = logfile
                .as_ref()
                .map_or(null_mut(), |f| fdopen(f.as_raw_fd(), c"w".as_ptr()));
            xml_mem_display_last(fp, is_final as i64 - mem as i64);
        }
        NB_TESTS.fetch_add(1, Ordering::Relaxed);

        // error:
        ret
    }
}

unsafe fn xmlconf_test_cases(
    logfile: &mut Option<File>,
    doc: XmlDocPtr,
    cur: XmlNodePtr,
    mut level: c_int,
) -> c_int {
    unsafe {
        let mut ret: c_int = 0;
        let mut tests: c_int = 0;
        let mut output: c_int = 0;

        if level == 1 {
            if let Some(profile) = cur.get_prop("PROFILE") {
                output = 1;
                level += 1;
                println!("Test cases: {profile}",);
            }
        }
        let mut cur = cur.children();
        while let Some(cur_node) = cur {
            // look only at elements we ignore everything else
            if cur_node.element_type() == XmlElementType::XmlElementNode {
                let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                if cur_node.name == "TESTCASES" {
                    ret += xmlconf_test_cases(logfile, doc, cur_node, level);
                } else if cur_node.name == "TEST" {
                    if xmlconf_test_item(logfile, doc, cur_node) >= 0 {
                        ret += 1;
                    }
                    tests += 1;
                } else {
                    eprintln!("Unhandled element {}", cur_node.name);
                }
            }
            cur = cur_node.next();
        }
        if output == 1 && tests > 0 {
            println!("Test cases: {} tests", tests);
        }
        ret
    }
}

unsafe fn xmlconf_test_suite(logfile: &mut Option<File>, doc: XmlDocPtr, cur: XmlNodePtr) -> c_int {
    unsafe {
        let mut ret: c_int = 0;

        if let Some(profile) = cur.get_prop("PROFILE") {
            println!("Test suite: {profile}",);
        } else {
            println!("Test suite");
        }
        let mut cur = cur.children();
        while let Some(cur_node) = cur {
            // look only at elements we ignore everything else
            if cur_node.element_type() == XmlElementType::XmlElementNode {
                let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                if cur_node.name == "TESTCASES" {
                    ret += xmlconf_test_cases(logfile, doc, cur_node, 1);
                } else {
                    eprintln!("Unhandled element {}", cur_node.name);
                }
            }
            cur = cur_node.next();
        }
        ret
    }
}

fn xmlconf_info() {
    eprintln!("  you need to fetch and extract the");
    eprintln!("  latest XML Conformance Test Suites");
    eprintln!("  https://www.w3.org/XML/Test/xmlts20130923.tar.gz");
    eprintln!("  see http://www.w3.org/XML/Test/ for information");
}

unsafe fn xmlconf_test(logfile: &mut Option<File>) -> c_int {
    unsafe {
        let confxml: &str = "xmlconf/xmlconf.xml";

        if !check_test_file(confxml) {
            eprintln!("{} is missing ", confxml);
            xmlconf_info();
            return -1;
        }
        let Some(doc) = xml_read_file(confxml, None, XmlParserOption::XmlParseNoEnt as i32) else {
            eprintln!("{} is corrupted ", confxml);
            xmlconf_info();
            return -1;
        };

        let Some(cur) = doc.get_root_element().filter(|cur| cur.name == "TESTSUITE") else {
            eprintln!("Unexpected format {}", confxml);
            xmlconf_info();
            xml_free_doc(doc);
            return -1;
        };
        let ret = xmlconf_test_suite(logfile, doc, cur);
        xml_free_doc(doc);
        ret
    }
}

#[test]
fn main() {
    let mut ret: c_int;
    let old_errors: c_int;
    let old_tests: c_int;
    let old_leaks: c_int;

    const LOGFILE: &str = "runxmlconf.log";

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
        xmlconf_test(&mut logfile);
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
            println!("See {} for detailed output", LOGFILE);
            if NB_LEAKS.load(Ordering::Relaxed) == 0
                && NB_ERRORS.load(Ordering::Relaxed) == NB_EXPECTED_ERRORS as i32
            {
                println!("{} errors were expected", NB_ERRORS.load(Ordering::Relaxed));
                ret = 0;
            }
        }
        xml_xpath_free_context(CTXT_XPATH.load(Ordering::Relaxed));
        xml_cleanup_parser();
        xml_memory_dump();
    }

    assert_eq!(ret, 0);
}
