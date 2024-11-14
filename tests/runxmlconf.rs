//! Rust implementation of original libxml2's `runxmlconf.c`.  
//! If you want this to work, please download https://www.w3.org/XML/Test/xmlts20130923.tar.gz from [W3C](http://www.w3.org/XML/Test/).

use std::{
    env::args,
    ffi::{c_char, c_int, CStr, CString},
    fs::{metadata, File},
    os::fd::AsRawFd,
    ptr::{addr_of_mut, null, null_mut},
    sync::atomic::{AtomicPtr, Ordering},
};

use exml::{
    error::{XmlError, XmlErrorDomain, XmlErrorLevel, XmlParserErrors},
    globals::{
        get_last_error, reset_last_error, set_get_warnings_default_value, set_structured_error,
        GenericErrorContext,
    },
    libxml::{
        globals::xml_free,
        parser::{
            xml_cleanup_parser, xml_ctxt_read_file, xml_free_parser_ctxt, xml_init_parser,
            xml_new_parser_ctxt, xml_pedantic_parser_default, xml_read_file,
            xml_set_external_entity_loader, XmlParserCtxtPtr, XmlParserInputPtr, XmlParserOption,
        },
        parser_internals::xml_new_input_from_file,
        xmlmemory::{
            xml_mem_display_last, xml_mem_free, xml_mem_malloc, xml_mem_realloc, xml_mem_setup,
            xml_mem_used, xml_memory_dump, xml_memory_strdup,
        },
        xmlstring::{xml_str_equal, xml_strchr, xml_strdup, XmlChar},
        xpath::{
            xml_xpath_context_set_cache, xml_xpath_free_context, xml_xpath_new_context,
            XmlXPathContext,
        },
    },
    tree::{xml_free_doc, XmlDocProperties, XmlDocPtr, XmlElementType, XmlNodePtr},
};
use libc::{fdopen, snprintf, strcmp};

static mut VERBOSE: c_int = 0;

const NB_EXPECTED_ERRORS: usize = 15;

const SKIPPED_TESTS: &[*const c_char] = &[
    /* http://lists.w3.org/Archives/Public/public-xml-testsuite/2008Jul/0000.html */
    c"rmt-ns10-035".as_ptr(),
    null(),
];

/************************************************************************
 *									*
 *		File name and path utilities				*
 *									*
 ************************************************************************/

fn check_test_file(filename: &str) -> bool {
    match metadata(filename) {
        Ok(meta) => meta.is_file(),
        _ => false,
    }
}

unsafe extern "C" fn compose_dir(dir: *const XmlChar, path: *const XmlChar) -> *mut XmlChar {
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
    xml_strdup(buf.as_mut_ptr() as *const XmlChar)
}

/************************************************************************
 *									*
 *		Libxml2 specific routines				*
 *									*
 ************************************************************************/

static mut NB_SKIPPED: c_int = 0;
static mut NB_TESTS: c_int = 0;
static mut NB_ERRORS: c_int = 0;
static mut NB_LEAKS: c_int = 0;

/*
 * We need to trap calls to the resolver to not account memory for the catalog
 * and not rely on any external resources.
 */
unsafe extern "C" fn test_external_entity_loader(
    url: *const c_char,
    _id: *const c_char,
    ctxt: XmlParserCtxtPtr,
) -> XmlParserInputPtr {
    xml_new_input_from_file(ctxt, url as *const c_char)
}

/*
 * Trapping the error messages at the generic level to grab the equivalent of
 * stderr messages on CLI tools.
 */
static mut TEST_ERRORS: [u8; 32769] = [0; 32769];
static mut TEST_ERRORS_SIZE: usize = 0;
static mut NB_ERROR: c_int = 0;
static mut NB_FATAL: c_int = 0;

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

fn test_error_handler(_user_data: Option<GenericErrorContext>, error: &XmlError) {
    unsafe {
        if TEST_ERRORS_SIZE >= 32768 {
            return;
        }
        let file = CString::new(error.file().unwrap_or("entity")).unwrap();
        let message = CString::new(error.message().unwrap_or("")).unwrap();
        let res: c_int = snprintf(
            addr_of_mut!(TEST_ERRORS[TEST_ERRORS_SIZE]) as _,
            32768 - TEST_ERRORS_SIZE,
            c"%s:%d: %s\n".as_ptr(),
            file.as_ptr(),
            error.line(),
            message,
        );
        if error.level() == XmlErrorLevel::XmlErrFatal {
            NB_FATAL += 1;
        } else if error.level() == XmlErrorLevel::XmlErrError {
            NB_ERROR += 1;
        }
        if TEST_ERRORS_SIZE + res as usize >= 32768 {
            /* buffer is full */
            TEST_ERRORS_SIZE = 32768;
            TEST_ERRORS[TEST_ERRORS_SIZE] = 0;
        } else {
            TEST_ERRORS_SIZE += res as usize;
        }
        TEST_ERRORS[TEST_ERRORS_SIZE] = 0;
    }
}

static CTXT_XPATH: AtomicPtr<XmlXPathContext> = AtomicPtr::new(null_mut());

unsafe extern "C" fn initialize_libxml2() {
    set_get_warnings_default_value(0);
    xml_pedantic_parser_default(0);

    xml_mem_setup(
        Some(xml_mem_free),
        Some(xml_mem_malloc),
        Some(xml_mem_realloc),
        Some(xml_memory_strdup),
    );
    xml_init_parser();
    xml_set_external_entity_loader(test_external_entity_loader);
    CTXT_XPATH.store(xml_xpath_new_context(null_mut()), Ordering::Relaxed);
    /*
     * Deactivate the cache if created; otherwise we have to create/free it
     * for every test, since it will confuse the memory leak detection.
     * Note that normally this need not be done, since the cache is not
     * created until set explicitly with xmlXPathContextSetCache();
     * but for test purposes it is sometimes useful to activate the
     * cache by default for the whole library.
     */
    if !(*CTXT_XPATH.load(Ordering::Relaxed)).cache.is_null() {
        xml_xpath_context_set_cache(CTXT_XPATH.load(Ordering::Relaxed), 0, -1, 0);
    }
    set_structured_error(Some(test_error_handler), None);
}

/************************************************************************
 *									*
 *		Run the xmlconf test if found				*
 *									*
 ************************************************************************/

unsafe extern "C" fn xmlconf_test_invalid(
    logfile: &mut Option<File>,
    id: *const c_char,
    filename: *const c_char,
    options: c_int,
) -> c_int {
    let mut ret: c_int = 1;

    let ctxt: XmlParserCtxtPtr = xml_new_parser_ctxt();
    if ctxt.is_null() {
        test_log!(
            logfile,
            "test {} : {} out of memory\n",
            CStr::from_ptr(id).to_string_lossy(),
            CStr::from_ptr(filename).to_string_lossy()
        );
        return 0;
    }
    let doc: XmlDocPtr = xml_ctxt_read_file(ctxt, filename, None, options);
    if doc.is_null() {
        test_log!(
            logfile,
            "test {} : {} invalid document turned not well-formed too\n",
            CStr::from_ptr(id).to_string_lossy(),
            CStr::from_ptr(filename).to_string_lossy()
        );
    } else {
        /* invalidity should be reported both in the context and in the document */
        if (*ctxt).valid != 0 || (*doc).properties & XmlDocProperties::XmlDocDTDValid as i32 != 0 {
            test_log!(
                logfile,
                "test {} : {} failed to detect invalid document\n",
                CStr::from_ptr(id).to_string_lossy(),
                CStr::from_ptr(filename).to_string_lossy()
            );
            NB_ERRORS += 1;
            ret = 0;
        }
        xml_free_doc(doc);
    }
    xml_free_parser_ctxt(ctxt);
    ret
}

unsafe extern "C" fn xmlconf_test_valid(
    logfile: &mut Option<File>,
    id: *const c_char,
    filename: *const c_char,
    options: c_int,
) -> c_int {
    let mut ret: c_int = 1;

    let ctxt: XmlParserCtxtPtr = xml_new_parser_ctxt();
    if ctxt.is_null() {
        test_log!(
            logfile,
            "test {} : {} out of memory\n",
            CStr::from_ptr(id).to_string_lossy(),
            CStr::from_ptr(filename).to_string_lossy()
        );
        return 0;
    }
    let doc: XmlDocPtr = xml_ctxt_read_file(ctxt, filename, None, options);
    if doc.is_null() {
        test_log!(
            logfile,
            "test {} : {} failed to parse a valid document\n",
            CStr::from_ptr(id).to_string_lossy(),
            CStr::from_ptr(filename).to_string_lossy()
        );
        NB_ERRORS += 1;
        ret = 0;
    } else {
        /* validity should be reported both in the context and in the document */
        if (*ctxt).valid == 0 || (*doc).properties & XmlDocProperties::XmlDocDTDValid as i32 == 0 {
            test_log!(
                logfile,
                "test {} : {} failed to validate a valid document\n",
                CStr::from_ptr(id).to_string_lossy(),
                CStr::from_ptr(filename).to_string_lossy()
            );
            NB_ERRORS += 1;
            ret = 0;
        }
        xml_free_doc(doc);
    }
    xml_free_parser_ctxt(ctxt);
    ret
}

unsafe extern "C" fn xmlconf_test_not_nswf(
    logfile: &mut Option<File>,
    id: *const c_char,
    filename: *const c_char,
    options: c_int,
) -> c_int {
    let mut ret: c_int = 1;

    /*
     * In case of Namespace errors, libxml2 will still parse the document
     * but log a Namespace error.
     */
    let doc: XmlDocPtr = xml_read_file(filename, None, options);
    if doc.is_null() {
        test_log!(
            logfile,
            "test {} : {} failed to parse the XML\n",
            CStr::from_ptr(id).to_string_lossy(),
            CStr::from_ptr(filename).to_string_lossy()
        );
        NB_ERRORS += 1;
        ret = 0;
    } else {
        let last_error = get_last_error();
        if last_error.code() == XmlParserErrors::XmlErrOK
            || last_error.domain() != XmlErrorDomain::XmlFromNamespace
        {
            test_log!(
                logfile,
                "test {} : {} failed to detect namespace error\n",
                CStr::from_ptr(id).to_string_lossy(),
                CStr::from_ptr(filename).to_string_lossy()
            );
            NB_ERRORS += 1;
            ret = 0;
        }
        xml_free_doc(doc);
    }
    ret
}

unsafe extern "C" fn xmlconf_test_not_wf(
    logfile: &mut Option<File>,
    id: *const c_char,
    filename: *const c_char,
    options: c_int,
) -> c_int {
    let mut ret: c_int = 1;

    let doc: XmlDocPtr = xml_read_file(filename, None, options);
    if !doc.is_null() {
        test_log!(
            logfile,
            "test {} : {} failed to detect not well formedness\n",
            CStr::from_ptr(id).to_string_lossy(),
            CStr::from_ptr(filename).to_string_lossy()
        );
        NB_ERRORS += 1;
        xml_free_doc(doc);
        ret = 0;
    }
    ret
}

unsafe extern "C" fn xmlconf_test_item(
    logfile: &mut Option<File>,
    doc: XmlDocPtr,
    cur: XmlNodePtr,
) -> c_int {
    let mut ret: c_int = -1;
    let mut typ: *mut XmlChar = null_mut();
    let mut filename: *mut XmlChar = null_mut();
    let mut uri: *mut XmlChar = null_mut();
    let mut base: *mut XmlChar = null_mut();
    let mut rec: *mut XmlChar = null_mut();
    let mut version: *mut XmlChar = null_mut();
    let mut entities: *mut XmlChar = null_mut();
    let mut edition: *mut XmlChar = null_mut();
    let mut options: c_int = 0;
    let mut nstest: c_int = 0;
    let mem: c_int;
    let is_final: c_int;

    TEST_ERRORS_SIZE = 0;
    TEST_ERRORS[0] = 0;
    NB_ERROR = 0;
    NB_FATAL = 0;
    let id: *mut XmlChar = (*cur).get_prop("ID");
    if id.is_null() {
        test_log!(logfile, "test missing ID, line {}\n", (*cur).get_line_no());
    // goto error;
    } else {
        for i in (0..).take_while(|&i| !SKIPPED_TESTS[i].is_null()) {
            if strcmp(SKIPPED_TESTS[i], id as *mut c_char) == 0 {
                test_log!(
                    logfile,
                    "Skipping test {} from skipped list\n",
                    CStr::from_ptr(id as _).to_string_lossy()
                );
                ret = 0;
                NB_SKIPPED += 1;
                // goto error;
                if !typ.is_null() {
                    xml_free(typ as _);
                }
                if !entities.is_null() {
                    xml_free(entities as _);
                }
                if !edition.is_null() {
                    xml_free(edition as _);
                }
                if !version.is_null() {
                    xml_free(version as _);
                }
                if !filename.is_null() {
                    xml_free(filename as _);
                }
                if !uri.is_null() {
                    xml_free(uri as _);
                }
                if !base.is_null() {
                    xml_free(base as _);
                }
                if !id.is_null() {
                    xml_free(id as _);
                }
                if !rec.is_null() {
                    xml_free(rec as _);
                }
                return ret;
            }
        }
        typ = (*cur).get_prop("TYPE");
        if typ.is_null() {
            test_log!(
                logfile,
                "test {} missing TYPE\n",
                CStr::from_ptr(id as _).to_string_lossy()
            );
            // goto error;
        } else {
            uri = (*cur).get_prop("URI");
            if uri.is_null() {
                test_log!(
                    logfile,
                    "test {} missing URI\n",
                    CStr::from_ptr(id as _).to_string_lossy()
                );
                // goto error;
            } else {
                base = (*cur).get_base(doc);
                filename = compose_dir(base, uri);
                if !check_test_file(
                    CStr::from_ptr(filename as *const c_char)
                        .to_string_lossy()
                        .as_ref(),
                ) {
                    test_log!(
                        logfile,
                        "test {} missing file {} \n",
                        CStr::from_ptr(id as _).to_string_lossy(),
                        if !filename.is_null() {
                            CStr::from_ptr(filename as *mut c_char)
                                .to_string_lossy()
                                .to_string()
                        } else {
                            "NULL".to_owned()
                        }
                    );
                    // goto error;
                } else {
                    version = (*cur).get_prop("VERSION");

                    entities = (*cur).get_prop("ENTITIES");
                    if !xml_str_equal(entities, "none".as_ptr()) {
                        options |= XmlParserOption::XmlParseDtdload as i32;
                        options |= XmlParserOption::XmlParseNoent as i32;
                    }
                    rec = (*cur).get_prop("RECOMMENDATION");
                    if rec.is_null()
                        || xml_str_equal(rec, c"XML1.0".as_ptr() as _)
                        || xml_str_equal(rec, c"XML1.0-errata2e".as_ptr() as _)
                        || xml_str_equal(rec, c"XML1.0-errata3e".as_ptr() as _)
                        || xml_str_equal(rec, c"XML1.0-errata4e".as_ptr() as _)
                    {
                        if !version.is_null() && !xml_str_equal(version, c"1.0".as_ptr() as _) {
                            test_log!(
                                logfile,
                                "Skipping test {} for {}\n",
                                CStr::from_ptr(id as _).to_string_lossy(),
                                CStr::from_ptr(version as _).to_string_lossy()
                            );
                            ret = 0;
                            NB_SKIPPED += 1;
                            // goto error;
                            if !typ.is_null() {
                                xml_free(typ as _);
                            }
                            if !entities.is_null() {
                                xml_free(entities as _);
                            }
                            if !edition.is_null() {
                                xml_free(edition as _);
                            }
                            if !version.is_null() {
                                xml_free(version as _);
                            }
                            if !filename.is_null() {
                                xml_free(filename as _);
                            }
                            if !uri.is_null() {
                                xml_free(uri as _);
                            }
                            if !base.is_null() {
                                xml_free(base as _);
                            }
                            if !id.is_null() {
                                xml_free(id as _);
                            }
                            if !rec.is_null() {
                                xml_free(rec as _);
                            }
                            return ret;
                        }
                        ret = 1;
                    } else if xml_str_equal(rec, c"NS1.0".as_ptr() as _)
                        || xml_str_equal(rec, c"NS1.0-errata1e".as_ptr() as _)
                    {
                        ret = 1;
                        nstest = 1;
                    } else {
                        test_log!(
                            logfile,
                            "Skipping test {} for REC {}\n",
                            CStr::from_ptr(id as _).to_string_lossy(),
                            CStr::from_ptr(rec as _).to_string_lossy()
                        );
                        ret = 0;
                        NB_SKIPPED += 1;
                        // goto error;
                        if !typ.is_null() {
                            xml_free(typ as _);
                        }
                        if !entities.is_null() {
                            xml_free(entities as _);
                        }
                        if !edition.is_null() {
                            xml_free(edition as _);
                        }
                        if !version.is_null() {
                            xml_free(version as _);
                        }
                        if !filename.is_null() {
                            xml_free(filename as _);
                        }
                        if !uri.is_null() {
                            xml_free(uri as _);
                        }
                        if !base.is_null() {
                            xml_free(base as _);
                        }
                        if !id.is_null() {
                            xml_free(id as _);
                        }
                        if !rec.is_null() {
                            xml_free(rec as _);
                        }
                        return ret;
                    }
                    edition = (*cur).get_prop("EDITION");
                    if !edition.is_null() && xml_strchr(edition, b'5').is_null() {
                        /* test limited to all versions before 5th */
                        options |= XmlParserOption::XmlParseOld10 as i32;
                    }

                    /*
                     * Reset errors and check memory usage before the test
                     */
                    reset_last_error();
                    TEST_ERRORS_SIZE = 0;
                    TEST_ERRORS[0] = 0;
                    mem = xml_mem_used();

                    if xml_str_equal(typ, c"not-wf".as_ptr() as _) {
                        if nstest == 0 {
                            xmlconf_test_not_wf(
                                logfile,
                                id as *mut c_char,
                                filename as *mut c_char,
                                options,
                            );
                        } else {
                            xmlconf_test_not_nswf(
                                logfile,
                                id as *mut c_char,
                                filename as *mut c_char,
                                options,
                            );
                        }
                    } else if xml_str_equal(typ, c"valid".as_ptr() as _) {
                        options |= XmlParserOption::XmlParseDtdvalid as i32;
                        xmlconf_test_valid(
                            logfile,
                            id as *mut c_char,
                            filename as *mut c_char,
                            options,
                        );
                    } else if xml_str_equal(typ, c"invalid".as_ptr() as _) {
                        options |= XmlParserOption::XmlParseDtdvalid as i32;
                        xmlconf_test_invalid(
                            logfile,
                            id as *mut c_char,
                            filename as *mut c_char,
                            options,
                        );
                    } else if xml_str_equal(typ, c"error".as_ptr() as _) {
                        test_log!(
                            logfile,
                            "Skipping error test {} \n",
                            CStr::from_ptr(id as _).to_string_lossy()
                        );
                        ret = 0;
                        NB_SKIPPED += 1;
                        // goto error;
                        if !typ.is_null() {
                            xml_free(typ as _);
                        }
                        if !entities.is_null() {
                            xml_free(entities as _);
                        }
                        if !edition.is_null() {
                            xml_free(edition as _);
                        }
                        if !version.is_null() {
                            xml_free(version as _);
                        }
                        if !filename.is_null() {
                            xml_free(filename as _);
                        }
                        if !uri.is_null() {
                            xml_free(uri as _);
                        }
                        if !base.is_null() {
                            xml_free(base as _);
                        }
                        if !id.is_null() {
                            xml_free(id as _);
                        }
                        if !rec.is_null() {
                            xml_free(rec as _);
                        }
                        return ret;
                    } else {
                        test_log!(
                            logfile,
                            "test {} unknown TYPE value {}\n",
                            CStr::from_ptr(id as _).to_string_lossy(),
                            CStr::from_ptr(typ as _).to_string_lossy()
                        );
                        ret = -1;
                        // goto error;
                        if !typ.is_null() {
                            xml_free(typ as _);
                        }
                        if !entities.is_null() {
                            xml_free(entities as _);
                        }
                        if !edition.is_null() {
                            xml_free(edition as _);
                        }
                        if !version.is_null() {
                            xml_free(version as _);
                        }
                        if !filename.is_null() {
                            xml_free(filename as _);
                        }
                        if !uri.is_null() {
                            xml_free(uri as _);
                        }
                        if !base.is_null() {
                            xml_free(base as _);
                        }
                        if !id.is_null() {
                            xml_free(id as _);
                        }
                        if !rec.is_null() {
                            xml_free(rec as _);
                        }
                        return ret;
                    }

                    /*
                     * Reset errors and check memory usage after the test
                     */
                    reset_last_error();
                    is_final = xml_mem_used();
                    if is_final > mem {
                        test_log!(
                            logfile,
                            "test {} : {} leaked {} bytes\n",
                            CStr::from_ptr(id as _).to_string_lossy(),
                            CStr::from_ptr(filename as _).to_string_lossy(),
                            is_final - mem
                        );
                        NB_LEAKS += 1;
                        let fp = logfile
                            .as_ref()
                            .map_or(null_mut(), |f| fdopen(f.as_raw_fd(), c"w".as_ptr()));
                        xml_mem_display_last(fp, is_final as i64 - mem as i64);
                    }
                    NB_TESTS += 1;
                }
            }
        }
    }

    // error:
    if !typ.is_null() {
        xml_free(typ as _);
    }
    if !entities.is_null() {
        xml_free(entities as _);
    }
    if !edition.is_null() {
        xml_free(edition as _);
    }
    if !version.is_null() {
        xml_free(version as _);
    }
    if !filename.is_null() {
        xml_free(filename as _);
    }
    if !uri.is_null() {
        xml_free(uri as _);
    }
    if !base.is_null() {
        xml_free(base as _);
    }
    if !id.is_null() {
        xml_free(id as _);
    }
    if !rec.is_null() {
        xml_free(rec as _);
    }
    ret
}

unsafe extern "C" fn xmlconf_test_cases(
    logfile: &mut Option<File>,
    doc: XmlDocPtr,
    mut cur: XmlNodePtr,
    mut level: c_int,
) -> c_int {
    let profile: *mut XmlChar;
    let mut ret: c_int = 0;
    let mut tests: c_int = 0;
    let mut output: c_int = 0;

    if level == 1 {
        profile = (*cur).get_prop("PROFILE");
        if !profile.is_null() {
            output = 1;
            level += 1;
            println!(
                "Test cases: {}",
                CStr::from_ptr(profile as _).to_string_lossy()
            );
            xml_free(profile as _);
        }
    }
    cur = (*cur).children.map_or(null_mut(), |c| c.as_ptr());
    while !cur.is_null() {
        /* look only at elements we ignore everything else */
        if (*cur).typ == XmlElementType::XmlElementNode {
            if xml_str_equal((*cur).name, c"TESTCASES".as_ptr() as _) {
                ret += xmlconf_test_cases(logfile, doc, cur, level);
            } else if xml_str_equal((*cur).name, c"TEST".as_ptr() as _) {
                if xmlconf_test_item(logfile, doc, cur) >= 0 {
                    ret += 1;
                }
                tests += 1;
            } else {
                eprintln!(
                    "Unhandled element {}",
                    CStr::from_ptr((*cur).name as _).to_string_lossy(),
                );
            }
        }
        cur = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
    }
    if (output == 1) && (tests > 0) {
        println!("Test cases: {} tests", tests);
    }
    ret
}

unsafe extern "C" fn xmlconf_test_suite(
    logfile: &mut Option<File>,
    doc: XmlDocPtr,
    mut cur: XmlNodePtr,
) -> c_int {
    let mut ret: c_int = 0;

    let profile: *mut XmlChar = (*cur).get_prop("PROFILE");
    if !profile.is_null() {
        println!(
            "Test suite: {}",
            CStr::from_ptr(profile as _).to_string_lossy()
        );
        xml_free(profile as _);
    } else {
        println!("Test suite");
    }
    cur = (*cur).children.map_or(null_mut(), |c| c.as_ptr());
    while !cur.is_null() {
        /* look only at elements we ignore everything else */
        if (*cur).typ == XmlElementType::XmlElementNode {
            if xml_str_equal((*cur).name, c"TESTCASES".as_ptr() as _) {
                ret += xmlconf_test_cases(logfile, doc, cur, 1);
            } else {
                eprintln!(
                    "Unhandled element {}",
                    CStr::from_ptr((*cur).name as _).to_string_lossy(),
                );
            }
        }
        cur = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
    }
    ret
}

fn xmlconf_info() {
    eprintln!("  you need to fetch and extract the");
    eprintln!("  latest XML Conformance Test Suites");
    eprintln!("  https://www.w3.org/XML/Test/xmlts20130923.tar.gz");
    eprintln!("  see http://www.w3.org/XML/Test/ for information");
}

unsafe extern "C" fn xmlconf_test(logfile: &mut Option<File>) -> c_int {
    let confxml: &CStr = c"xmlconf/xmlconf.xml";

    if !check_test_file(confxml.to_string_lossy().as_ref()) {
        eprintln!("{} is missing ", confxml.to_string_lossy());
        xmlconf_info();
        return -1;
    }
    let doc: XmlDocPtr = xml_read_file(
        confxml.as_ptr(),
        None,
        XmlParserOption::XmlParseNoent as i32,
    );
    if doc.is_null() {
        eprintln!("{} is corrupted ", confxml.to_string_lossy());
        xmlconf_info();
        return -1;
    }

    let cur: XmlNodePtr = (*doc).get_root_element();
    let ret = if cur.is_null() || !xml_str_equal((*cur).name, c"TESTSUITE".as_ptr() as _) {
        eprintln!("Unexpected format {}", confxml.to_string_lossy());
        xmlconf_info();
        -1
    } else {
        xmlconf_test_suite(logfile, doc, cur)
    };
    xml_free_doc(doc);
    ret
}

/************************************************************************
 *									*
 *		The driver for the tests				*
 *									*
 ************************************************************************/
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

        old_errors = NB_ERRORS;
        old_tests = NB_TESTS;
        old_leaks = NB_LEAKS;
        xmlconf_test(&mut logfile);
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
        if NB_ERRORS == 0 && NB_LEAKS == 0 {
            ret = 0;
            println!("Total {} tests, no errors", NB_TESTS);
        } else {
            ret = 1;
            println!(
                "Total {} tests, {} errors, {} leaks",
                NB_TESTS, NB_ERRORS, NB_LEAKS,
            );
            println!("See {} for detailed output", LOGFILE);
            if NB_LEAKS == 0 && NB_ERRORS == NB_EXPECTED_ERRORS as i32 {
                println!("{} errors were expected", NB_ERRORS);
                ret = 0;
            }
        }
        xml_xpath_free_context(CTXT_XPATH.load(Ordering::Relaxed));
        xml_cleanup_parser();
        xml_memory_dump();
    }

    assert_eq!(ret, 0);
}
