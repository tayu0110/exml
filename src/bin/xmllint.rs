/*
 * xmllint.c : a small tester program for XML input.
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 */
#![allow(unused)]

use std::{
    env::args,
    ffi::{c_char, c_int, c_long, c_void, CStr, CString},
    fs::File,
    io::{stderr, stdin, Write},
    mem::zeroed,
    num::IntErrorKind,
    process::exit,
    ptr::{addr_of_mut, null, null_mut},
    slice::from_raw_parts,
    sync::{
        atomic::{AtomicPtr, Ordering},
        Mutex,
    },
};

use const_format::concatcp;
use exml::{
    encoding::{add_encoding_alias, XmlCharEncoding},
    error::generic_error_default,
    generic_error,
    globals::{
        get_load_ext_dtd_default_value, set_load_ext_dtd_default_value, set_parser_debug_entities,
        set_tree_indent_string, GenericError, GenericErrorContext,
    },
    io::{xml_file_flush, xml_file_write, xml_no_net_external_entity_loader, XmlParserInputBuffer},
    libxml::{
        c14n::{xml_c14n_doc_dump_memory, XmlC14NMode},
        catalog::xml_load_catalogs,
        debug_xml::{xml_debug_dump_document, xml_debug_dump_entities, xml_shell},
        entities::{xml_encode_entities_reentrant, XmlEntityPtr},
        globals::{xml_deregister_node_default, xml_free, xml_register_node_default},
        htmlparser::{
            html_create_push_parser_ctxt, html_ctxt_use_options, html_free_parser_ctxt,
            html_parse_chunk, html_read_file, html_read_memory, HtmlParserCtxtPtr,
            HtmlParserOption,
        },
        htmltree::{html_doc_dump, html_save_file, html_save_file_format},
        parser::{
            xml_cleanup_parser, xml_create_push_parser_ctxt, xml_ctxt_read_file, xml_ctxt_read_io,
            xml_ctxt_read_memory, xml_ctxt_use_options, xml_free_parser_ctxt,
            xml_get_external_entity_loader, xml_has_feature, xml_new_parser_ctxt,
            xml_new_sax_parser_ctxt, xml_parse_chunk, xml_parse_dtd, xml_read_file, xml_read_io,
            xml_read_memory, xml_set_external_entity_loader, ErrorSAXFunc, WarningSAXFunc,
            XmlExternalEntityLoader, XmlFeature, XmlParserCtxtPtr, XmlParserInputPtr,
            XmlParserOption, XmlSAXHandler, XmlSAXHandlerPtr, XmlSAXLocatorPtr, XML_COMPLETE_ATTRS,
            XML_DETECT_IDS, XML_SAX2_MAGIC,
        },
        pattern::{xml_free_pattern, xml_patterncompile, XmlPattern, XmlStreamCtxt},
        relaxng::{
            xml_relaxng_free, xml_relaxng_free_parser_ctxt, xml_relaxng_free_valid_ctxt,
            xml_relaxng_new_parser_ctxt, xml_relaxng_new_valid_ctxt, xml_relaxng_parse,
            xml_relaxng_set_parser_errors, xml_relaxng_set_valid_errors, xml_relaxng_validate_doc,
            XmlRelaxNG, XmlRelaxNGParserCtxtPtr, XmlRelaxNGValidCtxtPtr,
        },
        schematron::{
            xml_schematron_free, xml_schematron_free_parser_ctxt, xml_schematron_free_valid_ctxt,
            xml_schematron_new_parser_ctxt, xml_schematron_new_valid_ctxt, xml_schematron_parse,
            xml_schematron_validate_doc, XmlSchematron, XmlSchematronParserCtxtPtr,
            XmlSchematronValidCtxtPtr, XmlSchematronValidOptions,
        },
        valid::{
            xml_free_enumeration, xml_free_valid_ctxt, xml_new_valid_ctxt,
            xml_valid_get_valid_elements, xml_validate_document, xml_validate_dtd,
        },
        xinclude::xml_xinclude_process_flags,
        xmlmemory::{
            xml_mem_free, xml_mem_malloc, xml_mem_realloc, xml_mem_setup, xml_mem_size,
            xml_mem_used, xml_memory_dump, xml_memory_strdup,
        },
        xmlreader::XmlTextReaderPtr,
        xmlsave::{
            xml_save_close, xml_save_doc, xml_save_to_filename, xml_save_to_io, XmlSaveCtxtPtr,
            XmlSaveOption,
        },
        xmlschemas::{
            xml_schema_free, xml_schema_free_parser_ctxt, xml_schema_free_valid_ctxt,
            xml_schema_new_parser_ctxt, xml_schema_new_valid_ctxt, xml_schema_parse,
            xml_schema_set_parser_errors, xml_schema_set_valid_errors, xml_schema_validate_doc,
            xml_schema_validate_set_filename, xml_schema_validate_stream, XmlSchema,
            XmlSchemaParserCtxtPtr, XmlSchemaValidCtxtPtr,
        },
        xmlstring::XmlChar,
        xpath::{xml_xpath_order_doc_elems, XmlXPathObjectPtr},
    },
    tree::{
        set_compress_mode, xml_copy_doc, xml_free_doc, xml_free_dtd, xml_new_doc, xml_new_doc_node,
        NodeCommon, XmlDocPtr, XmlDtdPtr, XmlElementContentPtr, XmlEnumerationPtr, XmlNodePtr,
    },
    SYSCONFDIR,
};
use libc::{
    close, fclose, fopen, fread, free, gettimeofday, malloc, memset, mmap, munmap, open, size_t,
    snprintf, stat, strlen, timeval, write, FILE, MAP_FAILED, MAP_SHARED, O_RDONLY, PROT_READ,
};

const XML_XML_DEFAULT_CATALOG: &str = concatcp!("file://", SYSCONFDIR, "/xml/catalog");

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum XmllintReturnCode {
    ReturnOk = 0,      /* No error */
    ErrUnclass = 1,    /* Unclassified */
    ErrDtd = 2,        /* Error in DTD */
    ErrValid = 3,      /* Validation error */
    ErrRdfile = 4,     /* CtxtReadFile error */
    ErrSchemacomp = 5, /* Schema compilation */
    ErrOut = 6,        /* Error writing output */
    ErrSchemapat = 7,  /* Error in schema pattern */
    ErrRdregis = 8,    /* Error in Reader registration */
    ErrMem = 9,        /* Out of memory error */
    ErrXpath = 10,     /* XPath evaluation error */
}
#[cfg(feature = "libxml_debug")]
static mut SHELL: c_int = 0;
#[cfg(feature = "libxml_debug")]
static mut DEBUGENT: c_int = 0;
static mut DEBUG: c_int = 0;
static mut MAXMEM: c_int = 0;
#[cfg(feature = "tree")]
static mut COPY: c_int = 0;
static mut RECOVERY: c_int = 0;
static mut NOENT: c_int = 0;
static mut NOENC: c_int = 0;
static mut NOBLANKS: c_int = 0;
static mut NOOUT: c_int = 0;
static mut NOWRAP: c_int = 0;
static mut FORMAT: c_int = 0;
#[cfg(feature = "output")]
static OUTPUT: Mutex<Option<CString>> = Mutex::new(None);
#[cfg(feature = "output")]
static mut COMPRESS: c_int = 0;
#[cfg(feature = "output")]
static mut OLDOUT: c_int = 0;
#[cfg(feature = "valid")]
static mut VALID: c_int = 0;
#[cfg(feature = "valid")]
static mut POSTVALID: c_int = 0;
#[cfg(feature = "valid")]
static DTDVALID: Mutex<Option<CString>> = Mutex::new(None);
#[cfg(feature = "valid")]
static DTDVALIDFPI: Mutex<Option<CString>> = Mutex::new(None);
#[cfg(feature = "schema")]
static mut RELAXNG: Mutex<Option<CString>> = Mutex::new(None);
#[cfg(feature = "schema")]
static RELAXNGSCHEMAS: AtomicPtr<XmlRelaxNG> = AtomicPtr::new(null_mut());
#[cfg(feature = "schema")]
static mut SCHEMA: Mutex<Option<CString>> = Mutex::new(None);
#[cfg(feature = "schema")]
static WXSCHEMAS: AtomicPtr<XmlSchema> = AtomicPtr::new(null_mut());
#[cfg(feature = "libxml_schematron")]
static SCHEMATRON: Mutex<Option<CString>> = Mutex::new(None);
#[cfg(feature = "libxml_schematron")]
static WXSCHEMATRON: AtomicPtr<XmlSchematron> = AtomicPtr::new(null_mut());
static mut REPEAT: c_int = 0;
static mut INSERT: c_int = 0;
#[cfg(any(feature = "html", feature = "valid"))]
static mut HTML: c_int = 0;
#[cfg(any(feature = "html", feature = "valid"))]
static mut XMLOUT: c_int = 0;
static mut HTMLOUT: c_int = 0;
#[cfg(feature = "html")]
static mut NODEFDTD: c_int = 0;
#[cfg(feature = "push")]
static mut PUSH: c_int = 0;
#[cfg(feature = "push")]
static mut PUSHSIZE: c_int = 4096;
static mut MEMORY: c_int = 0;
static mut TEST_IO: c_int = 0;
static ENCODING: Mutex<Option<String>> = Mutex::new(None);
#[cfg(feature = "xinclude")]
static mut XINCLUDE: c_int = 0;
static mut DTDATTRS: c_int = 0;
static mut LOADDTD: c_int = 0;
static mut PROGRESULT: XmllintReturnCode = XmllintReturnCode::ReturnOk;
static mut QUIET: c_int = 0;
static mut TIMING: c_int = 0;
static mut GENERATE: c_int = 0;
static mut DROPDTD: c_int = 0;
#[cfg(feature = "catalog")]
static mut CATALOGS: c_int = 0;
#[cfg(feature = "catalog")]
static mut NOCATALOGS: c_int = 0;
#[cfg(feature = "libxml_c14n")]
static mut CANONICAL: c_int = 0;
#[cfg(feature = "libxml_c14n")]
static mut CANONICAL_11: c_int = 0;
#[cfg(feature = "libxml_c14n")]
static mut EXC_CANONICAL: c_int = 0;
#[cfg(feature = "libxml_reader")]
static mut STREAM: c_int = 0;
#[cfg(feature = "libxml_reader")]
static mut WALKER: c_int = 0;
#[cfg(all(feature = "libxml_reader", feature = "libxml_pattern"))]
static mut PATTERN: Mutex<Option<CString>> = Mutex::new(None);
#[cfg(all(feature = "libxml_reader", feature = "libxml_pattern"))]
static mut PATTERNC: AtomicPtr<XmlPattern> = AtomicPtr::new(null_mut());
#[cfg(all(feature = "libxml_reader", feature = "libxml_pattern"))]
static mut PATSTREAM: AtomicPtr<XmlStreamCtxt> = AtomicPtr::new(null_mut());
static mut CHKREGISTER: c_int = 0;
static mut NBREGISTER: c_int = 0;
#[cfg(feature = "sax1")]
static mut SAX1: c_int = 0;
#[cfg(feature = "xpath")]
static XPATHQUERY: Mutex<Option<CString>> = Mutex::new(None);
static mut OPTIONS: c_int =
    XmlParserOption::XmlParseCompact as i32 | XmlParserOption::XmlParseBigLines as i32;
static mut SAX: c_int = 0;
static mut OLDXML10: c_int = 0;

/************************************************************************
 *                                    *
 *         Entity loading control and customization.        *
 *                                    *
 ************************************************************************/
const MAX_PATHS: usize = 64;
#[cfg(target_os = "windows")]
const PATH_SEPARATOR: char = ';';
#[cfg(not(target_os = "windows"))]
const PATH_SEPARATOR: char = ':';
// static mut PATHS: [*mut xmlChar; MAX_PATHS + 1] = [null_mut(); MAX_PATHS + 1];
static PATHS: Mutex<Vec<String>> = Mutex::new(vec![]);
static mut NBPATHS: usize = 0;
static mut LOAD_TRACE: c_int = 0;

fn parse_path(mut path: &str) {
    let mut paths = PATHS.lock().unwrap();
    path = path.trim_start_matches([' ', PATH_SEPARATOR]);
    while let Some((p, rem)) = path.split_once([' ', PATH_SEPARATOR]) {
        if paths.len() >= MAX_PATHS {
            eprintln!("MAX_PATHS reached: too many paths");
            return;
        }

        if !p.is_empty() {
            paths.push(p.to_owned());
        }

        path = rem.trim_start_matches([' ', PATH_SEPARATOR]);
    }
}

static mut DEFAULT_ENTITY_LOADER: Option<XmlExternalEntityLoader> = None;

unsafe extern "C" fn xmllint_external_entity_loader(
    url: *const c_char,
    id: *const c_char,
    ctxt: XmlParserCtxtPtr,
) -> XmlParserInputPtr {
    let mut ret: XmlParserInputPtr;
    let mut warning: Option<GenericError> = None;
    let mut err: Option<GenericError> = None;

    let mut lastsegment: *const c_char = url;
    let mut iter: *const c_char = url;

    if NBPATHS > 0 && !iter.is_null() {
        while *iter != 0 {
            if *iter == b'/' as i8 {
                lastsegment = iter.add(1);
            }
            iter = iter.add(1);
        }
    }

    if !ctxt.is_null() && !(*ctxt).sax.is_null() {
        warning = (*(*ctxt).sax).warning;
        err = (*(*ctxt).sax).error;
        (*(*ctxt).sax).warning = None;
        (*(*ctxt).sax).error = None;
    }

    if let Some(loader) = DEFAULT_ENTITY_LOADER {
        ret = loader(url, id, ctxt);
        if !ret.is_null() {
            if warning.is_some() {
                (*(*ctxt).sax).warning = warning;
            }
            if err.is_some() {
                (*(*ctxt).sax).error = err;
            }
            if LOAD_TRACE != 0 {
                eprintln!(
                    "Loaded URL=\"{}\" ID=\"{}\"",
                    (if !url.is_null() {
                        CStr::from_ptr(url)
                    } else {
                        c"(null)"
                    })
                    .to_string_lossy(),
                    (if !id.is_null() {
                        CStr::from_ptr(id)
                    } else {
                        c"(null)"
                    })
                    .to_string_lossy()
                );
            }
            return ret;
        }

        let paths = PATHS.lock().unwrap();
        for path in paths.iter() {
            let mut new_url = path.clone();
            new_url.push('/');
            new_url.push_str(CStr::from_ptr(lastsegment).to_string_lossy().as_ref());
            // to fix format of new_url to C string.
            new_url.push('\0');
            ret = loader(new_url.as_ptr() as *const c_char, id, ctxt);
            if !ret.is_null() {
                if warning.is_some() {
                    (*(*ctxt).sax).warning = warning;
                }
                if err.is_some() {
                    (*(*ctxt).sax).error = err;
                }
                if LOAD_TRACE != 0 {
                    eprintln!(
                        "Loaded URL=\"{}\" ID=\"{}\"",
                        new_url,
                        (if !id.is_null() {
                            CStr::from_ptr(id)
                        } else {
                            c"(null)"
                        })
                        .to_string_lossy()
                    );
                }
                return ret;
            }
        }
    }
    if err.is_some() {
        (*(*ctxt).sax).error = err;
    }
    if let Some(warning) = warning {
        (*(*ctxt).sax).warning = Some(warning);
        if !url.is_null() {
            todo!()
            // xml_error_with_format!(
            //     warning,
            //     ctxt as _,
            //     c"failed to load external entity \"%s\"\n".as_ptr(),
            //     url
            // );
        } else if !id.is_null() {
            todo!()
            // xml_error_with_format!(
            //     warning,
            //     ctxt as _,
            //     c"failed to load external entity \"%s\"\n".as_ptr(),
            //     id
            // );
        }
    }
    null_mut()
}
/************************************************************************
 *                                    *
 * Memory allocation consumption debugging                *
 *                                    *
 ************************************************************************/

unsafe extern "C" fn oom() {
    eprintln!("Ran out of memory needs > {} bytes", MAXMEM);
    PROGRESULT = XmllintReturnCode::ErrMem;
}

unsafe extern "C" fn my_free_func(mem: *mut c_void) {
    xml_mem_free(mem);
}
unsafe extern "C" fn my_malloc_func(size: size_t) -> *mut c_void {
    let ret: *mut c_void = xml_mem_malloc(size);
    if !ret.is_null() && xml_mem_used() > MAXMEM {
        oom();
        xml_mem_free(ret);
        return null_mut();
    }
    ret
}
unsafe extern "C" fn my_realloc_func(mem: *mut c_void, size: size_t) -> *mut c_void {
    let oldsize: size_t = xml_mem_size(mem);

    if xml_mem_used() as usize + size - oldsize > MAXMEM as size_t {
        oom();
        return null_mut();
    }

    xml_mem_realloc(mem, size)
}
unsafe extern "C" fn my_strdup_func(str: *const u8) -> *mut u8 {
    let ret = xml_memory_strdup(str);
    if !ret.is_null() && xml_mem_used() > MAXMEM {
        oom();
        xml_free(ret as _);
        return null_mut();
    }
    ret
}
/************************************************************************
 *                                    *
 * Internal timing routines to remove the necessity to have        *
 * unix-specific function calls.                    *
 *                                    *
 ************************************************************************/
static mut BEGIN: timeval = unsafe { zeroed() };
static mut END: timeval = unsafe { zeroed() };

/*
 * startTimer: call where you want to start timing
 */
unsafe extern "C" fn start_timer() {
    gettimeofday(addr_of_mut!(BEGIN), null_mut());
}

/*
 * end_timer: call where you want to stop timing and to print out a
 *           message about the timing performed; format is a printf
 *           type argument
 */
macro_rules! end_timer {
    ( $fmt:literal, $( $args:expr ),* ) => {
        gettimeofday(addr_of_mut!(END), null_mut());
        let mut msec: c_long = END.tv_sec - BEGIN.tv_sec;
        msec *= 1000;
        msec += (END.tv_usec - BEGIN.tv_usec) / 1000;

        eprint!($fmt, $( $args ),*);
        eprintln!(" took {} ms", msec);
    };
    ( $fmt:literal ) => {
        end_timer!($fmt, );
    }
}
/************************************************************************
 *                                    *
 *            HTML output                    *
 *                                    *
 ************************************************************************/
static mut BUFFER: [c_char; 50000] = [0; 50000];

unsafe extern "C" fn xml_htmlencode_send() {
    /*
     * xmlEncodeEntitiesReentrant assumes valid UTF-8, but the buffer might
     * end with a truncated UTF-8 sequence. This is a hack to at least avoid
     * an out-of-bounds read.
     */
    memset(addr_of_mut!(BUFFER[BUFFER.len() - 4]) as _, 0, 4);
    let result: *mut c_char =
        xml_encode_entities_reentrant(null_mut(), BUFFER.as_ptr() as _) as *mut c_char;
    if !result.is_null() {
        let s = CStr::from_ptr(result).to_string_lossy().into_owned();
        generic_error!("{s}");
        xml_free(result as _);
    }
    BUFFER[0] = 0;
}

/**
 * xmlHTMLPrintFileInfo:
 * @input:  an xmlParserInputPtr input
 *
 * Displays the associated file and line information for the current input
 */

unsafe extern "C" fn xml_htmlprint_file_info(input: XmlParserInputPtr) {
    generic_error!("<p>");

    let len = strlen(BUFFER.as_ptr());
    if !input.is_null() {
        if (*input).filename.is_some() {
            let filename = CString::new((*input).filename.as_deref().unwrap()).unwrap();
            snprintf(
                addr_of_mut!(BUFFER[len]) as _,
                BUFFER.len() - len,
                c"%s:%d: ".as_ptr(),
                filename.as_ptr(),
                (*input).line,
            );
        } else {
            snprintf(
                addr_of_mut!(BUFFER[len]) as _,
                BUFFER.len() - len,
                c"Entity: line %d: ".as_ptr(),
                (*input).line,
            );
        }
    }
    xml_htmlencode_send();
}

/**
 * xmlHTMLPrintFileContext:
 * @input:  an xmlParserInputPtr input
 *
 * Displays current context within the input content for error tracking
 */

unsafe extern "C" fn xml_htmlprint_file_context(input: XmlParserInputPtr) {
    let mut cur: *const XmlChar;
    let mut base: *const XmlChar;
    let mut n: c_int;

    if input.is_null() {
        return;
    }
    generic_error!("<pre>\n");
    cur = (*input).cur;
    base = (*input).base;
    while cur > base && (*cur == b'\n' || *cur == b'\r') {
        cur = cur.sub(1);
    }
    n = 0;
    while {
        n += 1;
        n - 1 < 80
    } && cur > base
        && *cur != b'\n'
        && *cur != b'\r'
    {
        cur = cur.sub(1);
    }
    if *cur == b'\n' || *cur == b'\r' {
        cur = cur.add(1);
    }
    base = cur;
    n = 0;
    while *cur != 0 && *cur != b'\n' && *cur != b'\r' && n < 79 {
        let len = strlen(BUFFER.as_ptr());
        snprintf(
            addr_of_mut!(BUFFER[len]) as _,
            BUFFER.len() - len,
            c"%c".as_ptr(),
            *cur as i32,
        );
        cur = cur.add(1);
        n += 1;
    }
    let len = strlen(BUFFER.as_ptr());
    snprintf(
        addr_of_mut!(BUFFER[len]) as _,
        BUFFER.len() - len,
        c"\n".as_ptr(),
    );
    cur = (*input).cur;
    while cur > base && (*cur == b'\n' || *cur == b'\r') {
        cur = cur.sub(1);
    }
    n = 0;
    while cur != base && {
        n += 1;
        n - 1 < 80
    } {
        let len = strlen(BUFFER.as_ptr());
        snprintf(
            addr_of_mut!(BUFFER[len]) as _,
            BUFFER.len() - len,
            c" ".as_ptr(),
        );
        base = base.add(1);
    }
    let len = strlen(BUFFER.as_ptr());
    snprintf(
        addr_of_mut!(BUFFER[len]) as _,
        BUFFER.len() - len,
        c"^\n".as_ptr(),
    );
    xml_htmlencode_send();
    generic_error!("</pre>");
}

/**
 * xml_html_error:
 * @ctx:  an XML parser context
 * @msg:  the message to display/transmit
 * @...:  extra parameters for the message display
 *
 * Display and format an error messages, gives file, line, position and
 * extra parameters.
 */
fn xml_html_error(_ctx: Option<GenericErrorContext>, _msg: &str) {
    todo!()
    // let ctxt: XmlParserCtxtPtr = ctx as XmlParserCtxtPtr;
    // let mut input: XmlParserInputPtr;

    // BUFFER[0] = 0;
    // input = (*ctxt).input;
    // if !input.is_null() && (*input).filename.is_null() && (*ctxt).input_nr > 1 {
    //     input = *(*ctxt).input_tab.add((*ctxt).input_nr as usize - 2);
    // }

    // xml_htmlprint_file_info(input);

    // xml_generic_error!(xml_generic_error_context(), c"<b>error</b>: ".as_ptr());
    // let len = strlen(BUFFER.as_ptr());
    // snprintf(addr_of_mut!(BUFFER[len]) as _, BUFFER.len() - len, msg);
    // xml_htmlencode_send();
    // xml_generic_error!(xml_generic_error_context(), c"</p>\n".as_ptr());

    // xml_htmlprint_file_context(input);
    // xml_htmlencode_send();
}

/**
 * xmlHTMLWarning:
 * @ctx:  an XML parser context
 * @msg:  the message to display/transmit
 * @...:  extra parameters for the message display
 *
 * Display and format a warning messages, gives file, line, position and
 * extra parameters.
 */
fn xml_html_warning(_ctx: Option<GenericErrorContext>, _msg: &str) {
    todo!()
    // let ctxt: XmlParserCtxtPtr = ctx as XmlParserCtxtPtr;
    // let mut input: XmlParserInputPtr;

    // BUFFER[0] = 0;
    // input = (*ctxt).input;
    // if !input.is_null() && (*input).filename.is_null() && (*ctxt).input_nr > 1 {
    //     input = *(*ctxt).input_tab.add((*ctxt).input_nr as usize - 2);
    // }

    // xml_htmlprint_file_info(input);

    // xml_generic_error!(xml_generic_error_context(), c"<b>warning</b>: ".as_ptr());
    // let len = strlen(BUFFER.as_ptr());
    // snprintf(addr_of_mut!(BUFFER[len]) as _, BUFFER.len() - len, msg);
    // xml_htmlencode_send();
    // xml_generic_error!(xml_generic_error_context(), c"</p>\n".as_ptr());

    // xml_htmlprint_file_context(input);
    // xml_htmlencode_send();
}

/**
 * xmlHTMLValidityError:
 * @ctx:  an XML parser context
 * @msg:  the message to display/transmit
 * @...:  extra parameters for the message display
 *
 * Display and format an validity error messages, gives file,
 * line, position and extra parameters.
 */
fn xml_html_validity_error(_ctx: Option<GenericErrorContext>, _msg: &str) {
    todo!()
    // let ctxt: XmlParserCtxtPtr = ctx as XmlParserCtxtPtr;
    // let mut input: XmlParserInputPtr;

    // BUFFER[0] = 0;
    // input = (*ctxt).input;
    // if (*input).filename.is_null() && (*ctxt).input_nr > 1 {
    //     input = *(*ctxt).input_tab.add((*ctxt).input_nr as usize - 2);
    // }

    // xml_htmlprint_file_info(input);

    // xml_generic_error!(
    //     xml_generic_error_context(),
    //     c"<b>validity error</b>: ".as_ptr()
    // );
    // let len = strlen(BUFFER.as_ptr());
    // snprintf(addr_of_mut!(BUFFER[len]) as _, BUFFER.len() - len, msg);
    // xml_htmlencode_send();
    // xml_generic_error!(xml_generic_error_context(), c"</p>\n".as_ptr());

    // xml_htmlprint_file_context(input);
    // xml_htmlencode_send();
    // PROGRESULT = XmllintReturnCode::ErrValid;
}

/**
 * xmlHTMLValidityWarning:
 * @ctx:  an XML parser context
 * @msg:  the message to display/transmit
 * @...:  extra parameters for the message display
 *
 * Display and format a validity warning messages, gives file, line,
 * position and extra parameters.
 */
fn xml_html_validity_warning(_ctx: Option<GenericErrorContext>, _msg: &str) {
    todo!()
    // let ctxt: XmlParserCtxtPtr = ctx as XmlParserCtxtPtr;
    // let mut input: XmlParserInputPtr;

    // BUFFER[0] = 0;
    // input = (*ctxt).input;
    // if (*input).filename.is_null() && (*ctxt).input_nr > 1 {
    //     input = *(*ctxt).input_tab.add((*ctxt).input_nr as usize - 2);
    // }

    // xml_htmlprint_file_info(input);

    // xml_generic_error!(
    //     xml_generic_error_context(),
    //     c"<b>validity warning</b>: ".as_ptr()
    // );
    // let len = strlen(BUFFER.as_ptr());
    // snprintf(addr_of_mut!(BUFFER[len]) as _, BUFFER.len() - len, msg);
    // xml_htmlencode_send();
    // xml_generic_error!(xml_generic_error_context(), c"</p>\n".as_ptr());

    // xml_htmlprint_file_context(input);
    // xml_htmlencode_send();
}

/************************************************************************
 *                                    *
 *            Shell Interface                    *
 *                                    *
 ************************************************************************/
/**
 * xmlShellReadline:
 * @prompt:  the prompt value
 *
 * Read a string
 *
 * Returns a pointer to it or NULL on EOF the caller is expected to
 *     free the returned string.
 */
#[cfg(all(feature = "libxml_debug", feature = "xpath"))]
unsafe extern "C" fn xml_shell_readline(prompt: *mut c_char) -> *mut c_char {
    use std::io::{stdin, stdout, Write};

    use libc::{malloc, memcpy};

    if !prompt.is_null() {
        print!("{}", CStr::from_ptr(prompt).to_string_lossy());
    }
    stdout().flush().ok();
    let mut line_read = String::new();
    match stdin().read_line(&mut line_read) {
        Ok(len) if len > 0 => {
            let ret = malloc(len + 1) as *mut c_char;
            if !ret.is_null() {
                memcpy(ret as _, line_read.as_ptr() as _, len);
            }
            *ret.add(len) = 0;
            ret
        }
        _ => null_mut(),
    }
}

/************************************************************************
 *                                    *
 *            I/O Interfaces                    *
 *                                    *
 ************************************************************************/

unsafe extern "C" fn my_read(f: *mut c_void, buf: *mut c_char, len: c_int) -> c_int {
    fread(buf as _, 1, len as _, f as *mut FILE) as _
}
unsafe extern "C" fn my_close(context: *mut c_void) -> c_int {
    let f: *mut FILE = context as *mut FILE;
    extern "C" {
        static stdin: *mut FILE;
    }
    if f == stdin {
        return 0;
    }
    fclose(f)
}

/************************************************************************
 *                                    *
 *            SAX based tests                    *
 *                                    *
 ************************************************************************/

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
    initialized: XML_SAX2_MAGIC as _,
    _private: AtomicPtr::new(null_mut()),
    start_element_ns: None,
    end_element_ns: None,
    serror: None,
};

// static xmlSAXHandlerPtr emptySAXHandler = &emptySAXHandlerStruct;
// extern xmlSAXHandlerPtr debugSAXHandler;
static mut CALLBACKS: c_int = 0;

/**
 * isStandaloneDebug:
 * @ctxt:  An XML parser context
 *
 * Is this document tagged standalone ?
 *
 * Returns 1 if true
 */
unsafe fn is_standalone_debug(_ctx: Option<GenericErrorContext>) -> c_int {
    CALLBACKS += 1;
    if NOOUT != 0 {
        return 0;
    }
    println!("SAX.isStandalone()");
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
unsafe fn has_internal_subset_debug(_ctx: Option<GenericErrorContext>) -> c_int {
    CALLBACKS += 1;
    if NOOUT != 0 {
        return 0;
    }
    println!("SAX.hasInternalSubset()");
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
unsafe fn has_external_subset_debug(_ctx: Option<GenericErrorContext>) -> c_int {
    CALLBACKS += 1;
    if NOOUT != 0 {
        return 0;
    }
    println!("SAX.hasExternalSubset()");
    0
}

/**
 * internalSubsetDebug:
 * @ctxt:  An XML parser context
 *
 * Does this document has an internal subset
 */
unsafe fn internal_subset_debug(
    _ctx: Option<GenericErrorContext>,
    name: *const XmlChar,
    external_id: *const XmlChar,
    system_id: *const XmlChar,
) {
    CALLBACKS += 1;
    if NOOUT != 0 {
        return;
    }
    print!(
        "SAX.internalSubset({},",
        CStr::from_ptr(name as _).to_string_lossy()
    );
    if external_id.is_null() {
        print!(" ,");
    } else {
        print!(" {},", CStr::from_ptr(external_id as _).to_string_lossy());
    }
    if system_id.is_null() {
        println!(" )");
    } else {
        println!(" {})", CStr::from_ptr(system_id as _).to_string_lossy());
    }
}

/**
 * externalSubsetDebug:
 * @ctxt:  An XML parser context
 *
 * Does this document has an external subset
 */
unsafe fn external_subset_debug(
    _ctx: Option<GenericErrorContext>,
    name: *const XmlChar,
    external_id: *const XmlChar,
    system_id: *const XmlChar,
) {
    CALLBACKS += 1;
    if NOOUT != 0 {
        return;
    }
    print!(
        "SAX.externalSubset({},",
        CStr::from_ptr(name as _).to_string_lossy()
    );
    if external_id.is_null() {
        print!(" ,");
    } else {
        print!(" {},", CStr::from_ptr(external_id as _).to_string_lossy());
    }
    if system_id.is_null() {
        println!(" )");
    } else {
        println!(" {})", CStr::from_ptr(system_id as _).to_string_lossy());
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
unsafe fn resolve_entity_debug(
    _ctx: Option<GenericErrorContext>,
    public_id: *const XmlChar,
    system_id: *const XmlChar,
) -> XmlParserInputPtr {
    CALLBACKS += 1;
    if NOOUT != 0 {
        return null_mut();
    }
    /* let ctxt: xmlParserCtxtPtr = ctx as xmlParserCtxtPtr; */

    print!("SAX.resolveEntity(");
    if !public_id.is_null() {
        print!("{}", CStr::from_ptr(public_id as _).to_string_lossy());
    } else {
        print!(" ");
    }
    if !system_id.is_null() {
        println!(", {})", CStr::from_ptr(system_id as _).to_string_lossy());
    } else {
        println!(", )");
    }
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
unsafe fn get_entity_debug(
    _ctx: Option<GenericErrorContext>,
    name: *const XmlChar,
) -> XmlEntityPtr {
    CALLBACKS += 1;
    if NOOUT != 0 {
        return null_mut();
    }
    println!(
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
unsafe fn get_parameter_entity_debug(
    _ctx: Option<GenericErrorContext>,
    name: *const XmlChar,
) -> XmlEntityPtr {
    CALLBACKS += 1;
    if NOOUT != 0 {
        return null_mut();
    }
    println!(
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
unsafe fn entity_decl_debug(
    _ctx: Option<GenericErrorContext>,
    name: *const XmlChar,
    typ: c_int,
    mut public_id: *const XmlChar,
    mut system_id: *const XmlChar,
    mut content: *mut XmlChar,
) {
    let nullstr: &CStr = c"(null)";
    /* not all libraries handle printing null pointers nicely */
    if public_id.is_null() {
        public_id = nullstr.as_ptr() as _;
    }
    if system_id.is_null() {
        system_id = nullstr.as_ptr() as _;
    }
    if content.is_null() {
        content = nullstr.as_ptr() as _;
    }
    CALLBACKS += 1;
    if NOOUT != 0 {
        return;
    }
    println!(
        "SAX.entityDecl({}, {}, {}, {}, {})",
        CStr::from_ptr(name as _).to_string_lossy(),
        typ,
        CStr::from_ptr(public_id as _).to_string_lossy(),
        CStr::from_ptr(system_id as _).to_string_lossy(),
        CStr::from_ptr(content as _).to_string_lossy()
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
unsafe fn attribute_decl_debug(
    _ctx: Option<GenericErrorContext>,
    elem: *const XmlChar,
    name: *const XmlChar,
    typ: c_int,
    def: c_int,
    default_value: *const XmlChar,
    tree: XmlEnumerationPtr,
) {
    CALLBACKS += 1;
    if NOOUT != 0 {
        return;
    }
    if default_value.is_null() {
        println!(
            "SAX.attributeDecl({}, {}, {}, {}, NULL, ...)",
            CStr::from_ptr(elem as _).to_string_lossy(),
            CStr::from_ptr(name as _).to_string_lossy(),
            typ,
            def
        );
    } else {
        println!(
            "SAX.attributeDecl({}, {}, {}, {}, {}, ...)",
            CStr::from_ptr(elem as _).to_string_lossy(),
            CStr::from_ptr(name as _).to_string_lossy(),
            typ,
            def,
            CStr::from_ptr(default_value as _).to_string_lossy()
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
unsafe fn element_decl_debug(
    _ctx: Option<GenericErrorContext>,
    name: *const XmlChar,
    typ: c_int,
    _content: XmlElementContentPtr,
) {
    CALLBACKS += 1;
    if NOOUT != 0 {
        return;
    }
    println!(
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
unsafe fn notation_decl_debug(
    _ctx: Option<GenericErrorContext>,
    name: *const XmlChar,
    public_id: *const XmlChar,
    system_id: *const XmlChar,
) {
    CALLBACKS += 1;
    if NOOUT != 0 {
        return;
    }
    println!(
        "SAX.notationDecl({}, {}, {})",
        CStr::from_ptr(name as _).to_string_lossy(),
        CStr::from_ptr(public_id as _).to_string_lossy(),
        CStr::from_ptr(system_id as _).to_string_lossy()
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
unsafe fn unparsed_entity_decl_debug(
    _ctx: Option<GenericErrorContext>,
    name: *const XmlChar,
    mut public_id: *const XmlChar,
    mut system_id: *const XmlChar,
    mut notation_name: *const XmlChar,
) {
    let nullstr: &CStr = c"(null)";

    if public_id.is_null() {
        public_id = nullstr.as_ptr() as _;
    }
    if system_id.is_null() {
        system_id = nullstr.as_ptr() as _;
    }
    if notation_name.is_null() {
        notation_name = nullstr.as_ptr() as _;
    }
    CALLBACKS += 1;
    if NOOUT != 0 {
        return;
    }
    println!(
        "SAX.unparsedEntityDecl({}, {}, {}, {})",
        CStr::from_ptr(name as _).to_string_lossy(),
        CStr::from_ptr(public_id as _).to_string_lossy(),
        CStr::from_ptr(system_id as _).to_string_lossy(),
        CStr::from_ptr(notation_name as _).to_string_lossy()
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
unsafe fn set_document_locator_debug(_ctx: Option<GenericErrorContext>, _loc: XmlSAXLocatorPtr) {
    CALLBACKS += 1;
    if NOOUT != 0 {
        return;
    }
    println!("SAX.setDocumentLocator()");
}

/**
 * startDocumentDebug:
 * @ctxt:  An XML parser context
 *
 * called when the document start being processed.
 */
unsafe fn start_document_debug(_ctx: Option<GenericErrorContext>) {
    CALLBACKS += 1;
    if NOOUT != 0 {
        return;
    }
    println!("SAX.startDocument()");
}

/**
 * endDocumentDebug:
 * @ctxt:  An XML parser context
 *
 * called when the document end has been detected.
 */
unsafe fn end_document_debug(_ctx: Option<GenericErrorContext>) {
    CALLBACKS += 1;
    if NOOUT != 0 {
        return;
    }
    println!("SAX.endDocument()");
}

/**
 * startElementDebug:
 * @ctxt:  An XML parser context
 * @name:  The element name
 *
 * called when an opening tag has been processed.
 */
unsafe fn start_element_debug(
    _ctx: Option<GenericErrorContext>,
    name: *const XmlChar,
    atts: *mut *const XmlChar,
) {
    CALLBACKS += 1;
    if NOOUT != 0 {
        return;
    }
    print!(
        "SAX.startElement({}",
        CStr::from_ptr(name as _).to_string_lossy()
    );
    if !atts.is_null() {
        for i in (0..).step_by(2).take_while(|&i| !(*atts.add(i)).is_null()) {
            print!(
                ", {}='",
                CStr::from_ptr(*atts.add(i) as _).to_string_lossy()
            );
            if !(*atts.add(i + 1)).is_null() {
                print!(
                    "{}'",
                    CStr::from_ptr(*atts.add(i + 1) as _).to_string_lossy()
                );
            }
        }
    }
    println!(")");
}

/**
 * endElementDebug:
 * @ctxt:  An XML parser context
 * @name:  The element name
 *
 * called when the end of an element has been detected.
 */
unsafe fn end_element_debug(_ctx: Option<GenericErrorContext>, name: *const XmlChar) {
    CALLBACKS += 1;
    if NOOUT != 0 {
        return;
    }
    println!(
        "SAX.endElement({})",
        CStr::from_ptr(name as _).to_string_lossy()
    );
}

/**
 * charactersDebug:
 * @ctxt:  An XML parser context
 * @ch:  a xmlChar string
 * @len: the number of xmlChar
 *
 * receiving some chars from the parser.
 * Question: how much at a time ???
 */
unsafe fn characters_debug(_ctx: Option<GenericErrorContext>, ch: *const XmlChar, len: c_int) {
    let mut out: [c_char; 40] = [0; 40];

    CALLBACKS += 1;
    if NOOUT != 0 {
        return;
    }

    let mut i = 0;
    while i < len as usize && i < 30 {
        out[i] = *ch.add(i) as _;
        i += 1;
    }
    out[i] = 0;

    println!(
        "SAX.characters({}, {})",
        CStr::from_ptr(out.as_ptr()).to_string_lossy(),
        len
    );
}

/**
 * referenceDebug:
 * @ctxt:  An XML parser context
 * @name:  The entity name
 *
 * called when an entity reference is detected.
 */
unsafe fn reference_debug(_ctx: Option<GenericErrorContext>, name: *const XmlChar) {
    CALLBACKS += 1;
    if NOOUT != 0 {
        return;
    }
    println!(
        "SAX.reference({})",
        CStr::from_ptr(name as _).to_string_lossy()
    );
}

/**
 * ignorableWhitespaceDebug:
 * @ctxt:  An XML parser context
 * @ch:  a xmlChar string
 * @start: the first c_char in the string
 * @len: the number of xmlChar
 *
 * receiving some ignorable whitespaces from the parser.
 * Question: how much at a time ???
 */
unsafe fn ignorable_whitespace_debug(
    _ctx: Option<GenericErrorContext>,
    ch: *const XmlChar,
    len: c_int,
) {
    let mut out: [c_char; 40] = [0; 40];

    CALLBACKS += 1;
    if NOOUT != 0 {
        return;
    }

    let mut i = 0;
    while i < len as usize && i < 30 {
        out[i] = *ch.add(i) as _;
        i += 1;
    }
    out[i] = 0;
    println!(
        "SAX.ignorableWhitespace({}, {})",
        CStr::from_ptr(out.as_ptr()).to_string_lossy(),
        len
    );
}

/**
 * processingInstructionDebug:
 * @ctxt:  An XML parser context
 * @target:  the target name
 * @data: the PI data's
 * @len: the number of xmlChar
 *
 * A processing instruction has been parsed.
 */
unsafe fn processing_instruction_debug(
    _ctx: Option<GenericErrorContext>,
    target: *const XmlChar,
    data: *const XmlChar,
) {
    CALLBACKS += 1;
    if NOOUT != 0 {
        return;
    }
    if !data.is_null() {
        println!(
            "SAX.processingInstruction({}, {})",
            CStr::from_ptr(target as _).to_string_lossy(),
            CStr::from_ptr(data as _).to_string_lossy()
        );
    } else {
        println!(
            "SAX.processingInstruction({}, NULL)",
            CStr::from_ptr(target as _).to_string_lossy()
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
unsafe fn cdata_block_debug(_ctx: Option<GenericErrorContext>, value: *const XmlChar, len: c_int) {
    CALLBACKS += 1;
    if NOOUT != 0 {
        return;
    }
    println!(
        "SAX.pcdata({:20}, {})",
        CStr::from_ptr(value as _).to_string_lossy(),
        len
    );
}

/**
 * commentDebug:
 * @ctxt:  An XML parser context
 * @value:  the comment content
 *
 * A comment has been parsed.
 */
unsafe fn comment_debug(_ctx: Option<GenericErrorContext>, value: *const XmlChar) {
    CALLBACKS += 1;
    if NOOUT != 0 {
        return;
    }
    println!(
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
fn warning_debug(_ctx: Option<GenericErrorContext>, msg: &str) {
    unsafe {
        CALLBACKS += 1;
        if NOOUT != 0 {
            return;
        }
    }
    print!("SAX.warning: {}", msg);
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
fn error_debug(_ctx: Option<GenericErrorContext>, msg: &str) {
    unsafe {
        CALLBACKS += 1;
        if NOOUT != 0 {
            return;
        }
    }
    print!("SAX.error: {}", msg);
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
fn fatal_error_debug(_ctx: Option<GenericErrorContext>, msg: &str) {
    unsafe {
        CALLBACKS += 1;
        if NOOUT != 0 {
            return;
        }
    }
    print!("SAX.fatalError: {msg}");
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

// xmlSAXHandlerPtr debugSAXHandler = &debugSAXHandlerStruct;

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
#[allow(clippy::too_many_arguments)]
unsafe fn start_element_ns_debug(
    _ctx: Option<GenericErrorContext>,
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
    if NOOUT != 0 {
        return;
    }
    print!(
        "SAX.startElementNs({}",
        CStr::from_ptr(localname as _).to_string_lossy()
    );
    if prefix.is_null() {
        print!(", NULL");
    } else {
        print!(", {}", CStr::from_ptr(prefix as _).to_string_lossy());
    }
    if uri.is_null() {
        print!(", NULL");
    } else {
        print!(", '{}'", CStr::from_ptr(uri as _).to_string_lossy());
    }
    print!(", {}", nb_namespaces);

    if !namespaces.is_null() {
        for i in (0..nb_namespaces as usize * 2).step_by(2) {
            print!(", xmlns");
            if !(*namespaces.add(i)).is_null() {
                print!(
                    ":{}",
                    CStr::from_ptr(*namespaces.add(i) as _).to_string_lossy()
                );
            }
            print!(
                "='{}'",
                CStr::from_ptr(*namespaces.add(i + 1) as _).to_string_lossy()
            );
        }
    }
    print!(", {}, {}", nb_attributes, nb_defaulted);
    if !attributes.is_null() {
        for i in (0..nb_attributes as usize * 5).step_by(5) {
            if !(*attributes.add(i + 1)).is_null() {
                print!(
                    ", {}:{}='",
                    CStr::from_ptr(*attributes.add(i + 1) as _).to_string_lossy(),
                    CStr::from_ptr(*attributes.add(i) as _).to_string_lossy()
                );
            } else {
                print!(
                    ", {}='",
                    CStr::from_ptr(*attributes.add(i) as _).to_string_lossy()
                );
            }
            print!(
                "{:4}...', {}",
                CStr::from_ptr(*attributes.add(i + 3) as _).to_string_lossy(),
                (*attributes.add(i + 4)).offset_from(*attributes.add(i + 3))
            );
        }
    }
    println!(")");
}

/**
 * endElementDebug:
 * @ctxt:  An XML parser context
 * @name:  The element name
 *
 * called when the end of an element has been detected.
 */
unsafe fn end_element_ns_debug(
    _ctx: Option<GenericErrorContext>,
    localname: *const XmlChar,
    prefix: *const XmlChar,
    uri: *const XmlChar,
) {
    CALLBACKS += 1;
    if NOOUT != 0 {
        return;
    }
    print!(
        "SAX.endElementNs({}",
        CStr::from_ptr(localname as _).to_string_lossy()
    );
    if prefix.is_null() {
        print!(", NULL");
    } else {
        print!(", {}", CStr::from_ptr(prefix as _).to_string_lossy());
    }
    if uri.is_null() {
        println!(", NULL)");
    } else {
        println!(", '{}')", CStr::from_ptr(uri as _).to_string_lossy());
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

unsafe extern "C" fn test_sax(filename: *const c_char) {
    let handler: XmlSAXHandlerPtr;
    let user_data: &CStr = c"user_data"; /* mostly for debugging */

    CALLBACKS = 0;

    if NOOUT != 0 {
        handler = addr_of_mut!(EMPTY_SAXHANDLER_STRUCT);
    } else {
        #[cfg(feature = "sax1")]
        if SAX1 != 0 {
            handler = addr_of_mut!(DEBUG_SAXHANDLER_STRUCT);
        } else {
            handler = addr_of_mut!(DEBUG_SAX2_HANDLER_STRUCT);
        }
        #[cfg(not(feature = "sax1"))]
        {
            handler = addr_of_mut!(DEBUG_SAX2_HANDLER_STRUCT);
        }
    }

    #[cfg(not(feature = "schema"))]
    let f = false;
    #[cfg(feature = "schema")]
    let f = !WXSCHEMAS.load(Ordering::Relaxed).is_null();
    if f {
        #[cfg(feature = "schema")]
        {
            let Some(buf) = XmlParserInputBuffer::from_uri(
                CStr::from_ptr(filename).to_string_lossy().as_ref(),
                XmlCharEncoding::None,
            ) else {
                return;
            };

            let vctxt: XmlSchemaValidCtxtPtr =
                xml_schema_new_valid_ctxt(WXSCHEMAS.load(Ordering::Relaxed));
            if vctxt.is_null() {
                PROGRESULT = XmllintReturnCode::ErrMem;
                // xml_free_parser_input_buffer(buf);
                return;
            }
            xml_schema_set_valid_errors(
                vctxt,
                Some(generic_error_default),
                Some(generic_error_default),
                None,
            );
            xml_schema_validate_set_filename(vctxt, filename);

            let ret: c_int = xml_schema_validate_stream(
                vctxt,
                buf,
                XmlCharEncoding::None,
                handler,
                Some(GenericErrorContext::new(user_data.as_ptr())),
            );
            if REPEAT == 0 {
                match ret.cmp(&0) {
                    std::cmp::Ordering::Equal => {
                        if QUIET == 0 {
                            eprintln!("{} validates", CStr::from_ptr(filename).to_string_lossy());
                        }
                    }
                    std::cmp::Ordering::Greater => {
                        eprintln!(
                            "{} fails to validate",
                            CStr::from_ptr(filename).to_string_lossy()
                        );
                        PROGRESULT = XmllintReturnCode::ErrValid;
                    }
                    std::cmp::Ordering::Less => {
                        eprintln!(
                            "{} validation generated an internal error",
                            CStr::from_ptr(filename).to_string_lossy()
                        );
                        PROGRESULT = XmllintReturnCode::ErrValid;
                    }
                }
            }
            xml_schema_free_valid_ctxt(vctxt);
        }
    } else {
        /*
         * Create the parser context amd hook the input
         */
        let ctxt: XmlParserCtxtPtr =
            xml_new_sax_parser_ctxt(handler, Some(GenericErrorContext::new(user_data.as_ptr())));
        if ctxt.is_null() {
            PROGRESULT = XmllintReturnCode::ErrMem;
            return;
        }
        xml_ctxt_read_file(ctxt, filename, None, OPTIONS);

        if !(*ctxt).my_doc.is_null() {
            eprintln!("SAX generated a doc !");
            xml_free_doc((*ctxt).my_doc);
            (*ctxt).my_doc = null_mut();
        }
        xml_free_parser_ctxt(ctxt);
    }
}

/************************************************************************
 *                                    *
 *            Stream Test processing                *
 *                                    *
 ************************************************************************/
#[cfg(feature = "libxml_reader")]
unsafe extern "C" fn process_node(reader: XmlTextReaderPtr) {
    use exml::libxml::{
        pattern::{xml_free_stream_ctxt, xml_pattern_match, xml_stream_pop, xml_stream_push},
        xmlreader::{
            xml_text_reader_const_local_name, xml_text_reader_const_name,
            xml_text_reader_const_namespace_uri, xml_text_reader_const_value,
            xml_text_reader_current_node, xml_text_reader_depth, xml_text_reader_has_value,
            xml_text_reader_is_empty_element, xml_text_reader_node_type, XmlReaderTypes,
        },
    };

    let mut name: *const XmlChar;
    let value: *const XmlChar;

    let typ: c_int = xml_text_reader_node_type(&mut *reader);
    let empty: c_int = xml_text_reader_is_empty_element(&mut *reader);

    if DEBUG != 0 {
        name = xml_text_reader_const_name(&mut *reader);
        if name.is_null() {
            name = c"--".as_ptr() as _;
        }

        value = xml_text_reader_const_value(&mut *reader);

        print!(
            "{} {} {} {} {}",
            xml_text_reader_depth(&mut *reader),
            typ,
            CStr::from_ptr(name as _).to_string_lossy(),
            empty,
            xml_text_reader_has_value(&mut *reader)
        );
        if value.is_null() {
            println!();
        } else {
            println!(" {}", CStr::from_ptr(value as _).to_string_lossy());
        }
    }
    #[cfg(feature = "libxml_pattern")]
    if !PATTERNC.load(Ordering::Relaxed).is_null() {
        let mut path: *mut XmlChar = null_mut();
        let mut is_match: c_int = -1;

        if typ == XmlReaderTypes::XmlReaderTypeElement as i32 {
            /* do the check only on element start */
            is_match = xml_pattern_match(
                PATTERNC.load(Ordering::Relaxed),
                xml_text_reader_current_node(&mut *reader),
            );

            if is_match != 0 {
                #[cfg(any(feature = "tree", feature = "libxml_debug"))]
                {
                    path = (*xml_text_reader_current_node(&mut *reader)).get_node_path();
                    println!(
                        "Node {} matches pattern {}",
                        CStr::from_ptr(path as _).to_string_lossy(),
                        PATTERN.lock().unwrap().as_ref().unwrap().to_string_lossy()
                    );
                }
                #[cfg(not(any(feature = "tree", feature = "libxml_debug")))]
                {
                    println!(
                        "Node {} matches pattern {}",
                        CStr::from_ptr(xml_text_reader_const_name(reader)).to_string_lossy(),
                        PATTERN.lock().unwrap().as_ref().unwrap().to_string_lossy()
                    );
                }
            }
        }
        if !PATSTREAM.load(Ordering::Relaxed).is_null() {
            let mut ret: c_int;

            if typ == XmlReaderTypes::XmlReaderTypeElement as i32 {
                ret = xml_stream_push(
                    PATSTREAM.load(Ordering::Relaxed),
                    xml_text_reader_const_local_name(&mut *reader),
                    xml_text_reader_const_namespace_uri(&mut *reader),
                );
                if ret < 0 {
                    eprintln!("xmlStreamPush() failure");
                    xml_free_stream_ctxt(PATSTREAM.load(Ordering::Relaxed));
                    PATSTREAM.store(null_mut(), Ordering::Relaxed);
                } else if ret != is_match {
                    #[cfg(any(feature = "tree", feature = "libxml_debug"))]
                    if path.is_null() {
                        path = (*xml_text_reader_current_node(&mut *reader)).get_node_path();
                    }
                    eprintln!("xmlPatternMatch and xmlStreamPush disagree");
                    if !path.is_null() {
                        eprintln!(
                            "  pattern {} node {}",
                            PATTERN.lock().unwrap().as_ref().unwrap().to_string_lossy(),
                            CStr::from_ptr(path as _).to_string_lossy()
                        );
                    } else {
                        eprintln!(
                            "  pattern {} node {}",
                            PATTERN.lock().unwrap().as_ref().unwrap().to_string_lossy(),
                            CStr::from_ptr(xml_text_reader_const_name(&mut *reader) as _)
                                .to_string_lossy()
                        );
                    }
                }
            }
            if typ == XmlReaderTypes::XmlReaderTypeEndElement as i32
                || (typ == XmlReaderTypes::XmlReaderTypeElement as i32 && empty != 0)
            {
                ret = xml_stream_pop(PATSTREAM.load(Ordering::Relaxed));
                if ret < 0 {
                    eprintln!("xmlStreamPop() failure");
                    xml_free_stream_ctxt(PATSTREAM.load(Ordering::Relaxed));
                    PATSTREAM.store(null_mut(), Ordering::Relaxed);
                }
            }
        }
        if !path.is_null() {
            xml_free(path as _);
        }
    }
}

#[cfg(feature = "libxml_reader")]
unsafe extern "C" fn stream_file(filename: *mut c_char) {
    use std::{ptr::null, slice::from_raw_parts};

    use exml::libxml::{
        pattern::{xml_free_stream_ctxt, xml_pattern_get_stream_ctxt, xml_stream_push},
        xmlreader::{
            xml_free_text_reader, xml_reader_for_file, xml_reader_for_memory,
            xml_text_reader_is_valid, xml_text_reader_read, xml_text_reader_relaxng_validate,
            xml_text_reader_schema_validate, xml_text_reader_set_parser_prop, XmlParserProperties,
        },
    };
    use libc::{close, mmap, munmap, stat, MAP_FAILED, MAP_SHARED, PROT_READ};

    let reader: XmlTextReaderPtr;
    let mut ret: c_int;
    let mut fd: c_int = -1;
    let mut info: stat = unsafe { zeroed() };
    let mut base: *const c_char = null();

    if MEMORY != 0 {
        if stat(filename, addr_of_mut!(info)) < 0 {
            return;
        }
        fd = open(filename, O_RDONLY);
        if fd < 0 {
            return;
        }
        base = mmap(null_mut(), info.st_size as _, PROT_READ, MAP_SHARED, fd, 0) as _;
        if base == MAP_FAILED as _ {
            close(fd);
            eprintln!(
                "mmap failure for file {}",
                CStr::from_ptr(filename).to_string_lossy()
            );
            PROGRESULT = XmllintReturnCode::ErrRdfile;
            return;
        }

        let mem = from_raw_parts(base as *const u8, info.st_size as usize).to_vec();
        reader = xml_reader_for_memory(mem, filename, null_mut(), OPTIONS);
    } else {
        reader = xml_reader_for_file(filename, null_mut(), OPTIONS);
    }
    #[cfg(feature = "libxml_pattern")]
    if !PATTERNC.load(Ordering::Relaxed).is_null() {
        PATSTREAM.store(
            xml_pattern_get_stream_ctxt(PATTERNC.load(Ordering::Relaxed)),
            Ordering::Relaxed,
        );
        if !PATSTREAM.load(Ordering::Relaxed).is_null() {
            ret = xml_stream_push(PATSTREAM.load(Ordering::Relaxed), null_mut(), null_mut());
            if ret < 0 {
                eprintln!("xmlStreamPush() failure");
                xml_free_stream_ctxt(PATSTREAM.load(Ordering::Relaxed));
                PATSTREAM.store(null_mut(), Ordering::Relaxed);
            }
        }
    }

    if !reader.is_null() {
        #[cfg(feature = "valid")]
        if VALID != 0 {
            xml_text_reader_set_parser_prop(
                &mut *reader,
                XmlParserProperties::XmlParserValidate as i32,
                1,
            );
        } else if LOADDTD != 0 {
            xml_text_reader_set_parser_prop(
                &mut *reader,
                XmlParserProperties::XmlParserLoaddtd as i32,
                1,
            );
        }
        #[cfg(not(feature = "valid"))]
        if LOADDTD != 0 {
            xml_text_reader_set_parser_prop(reader, XmlParserProperties::XmlParserLoaddtd, 1);
        }
        #[cfg(feature = "schema")]
        if let Some(mut relaxng) = RELAXNG.lock().ok().filter(|r| r.is_some()) {
            if TIMING != 0 && REPEAT == 0 {
                start_timer();
            }
            ret = xml_text_reader_relaxng_validate(reader, relaxng.as_ref().unwrap().as_ptr());
            if ret < 0 {
                generic_error!(
                    "Relax-NG schema {} failed to compile\n",
                    relaxng.as_ref().unwrap().to_string_lossy()
                );
                PROGRESULT = XmllintReturnCode::ErrSchemacomp;
                *relaxng = None;
            }
            if TIMING != 0 && REPEAT == 0 {
                end_timer!("Compiling the schemas");
            }
        }
        #[cfg(feature = "schema")]
        if let Some(mut schema) = SCHEMA.lock().ok().filter(|s| s.is_some()) {
            if TIMING != 0 && REPEAT == 0 {
                start_timer();
            }
            ret = xml_text_reader_schema_validate(reader, schema.as_ref().unwrap().as_ptr());
            if ret < 0 {
                generic_error!(
                    "XSD schema {} failed to compile\n",
                    schema.as_ref().unwrap().to_string_lossy()
                );
                PROGRESULT = XmllintReturnCode::ErrSchemacomp;
                *schema = None;
            }
            if TIMING != 0 && REPEAT == 0 {
                end_timer!("Compiling the schemas");
            }
        }

        /*
         * Process all nodes in sequence
         */
        if TIMING != 0 && REPEAT == 0 {
            start_timer();
        }
        ret = xml_text_reader_read(&mut *reader);
        while ret == 1 {
            #[cfg(feature = "libxml_pattern")]
            let f = !PATTERNC.load(Ordering::Relaxed).is_null();
            #[cfg(not(feature = "libxml_pattern"))]
            let f = false;
            if DEBUG != 0 || f {
                process_node(reader);
            }
            ret = xml_text_reader_read(&mut *reader);
        }
        if TIMING != 0 && REPEAT == 0 {
            #[cfg(any(feature = "schema", feature = "valid"))]
            {
                let mut is_validating = false;
                #[cfg(feature = "schema")]
                {
                    is_validating |= RELAXNG.lock().unwrap().is_some();
                }
                #[cfg(feature = "valid")]
                {
                    is_validating |= VALID != 0;
                }
                if is_validating {
                    end_timer!("Parsing and validating");
                } else {
                    end_timer!("Parsing");
                }
            }
            #[cfg(not(any(feature = "schema", feature = "valid")))]
            {
                end_timer!("Parsing");
            }
        }

        #[cfg(feature = "valid")]
        if VALID != 0 && xml_text_reader_is_valid(&mut *reader) != 1 {
            let filename = CStr::from_ptr(filename).to_string_lossy().into_owned();
            generic_error!("Document {filename} does not validate\n");
            PROGRESULT = XmllintReturnCode::ErrValid;
        }
        #[cfg(feature = "schema")]
        if RELAXNG.lock().unwrap().is_some() || SCHEMA.lock().unwrap().is_some() {
            if xml_text_reader_is_valid(&mut *reader) != 1 {
                eprintln!(
                    "{} fails to validate",
                    CStr::from_ptr(filename).to_string_lossy()
                );
                PROGRESULT = XmllintReturnCode::ErrValid;
            } else if QUIET == 0 {
                eprintln!("{} validates", CStr::from_ptr(filename).to_string_lossy());
            }
        }
        /*
         * Done, cleanup and status
         */
        xml_free_text_reader(reader);
        if ret != 0 {
            eprintln!(
                "{} : failed to parse",
                CStr::from_ptr(filename).to_string_lossy()
            );
            PROGRESULT = XmllintReturnCode::ErrUnclass;
        }
    } else {
        eprintln!(
            "Unable to open {}",
            CStr::from_ptr(filename).to_string_lossy()
        );
        PROGRESULT = XmllintReturnCode::ErrUnclass;
    }
    #[cfg(feature = "libxml_pattern")]
    if !PATSTREAM.load(Ordering::Relaxed).is_null() {
        xml_free_stream_ctxt(PATSTREAM.load(Ordering::Relaxed));
        PATSTREAM.store(null_mut(), Ordering::Relaxed);
    }
    if MEMORY != 0 {
        // xml_free_parser_input_buffer(input);
        munmap(base as _, info.st_size as _);
        close(fd);
    }
}

#[cfg(feature = "libxml_reader")]
unsafe extern "C" fn walk_doc(doc: XmlDocPtr) {
    use std::{ptr::null, sync::atomic::Ordering};

    use exml::{
        libxml::{
            pattern::{
                xml_free_stream_ctxt, xml_pattern_get_stream_ctxt, xml_patterncompile,
                xml_stream_push,
            },
            xmlreader::{xml_free_text_reader, xml_reader_walker, xml_text_reader_read},
        },
        tree::XmlNodePtr,
    };

    let mut ret: c_int;

    #[cfg(feature = "libxml_pattern")]
    {
        let mut namespaces: [*const XmlChar; 22] = [null(); 22];

        let root: XmlNodePtr = if doc.is_null() {
            null_mut()
        } else {
            (*doc).get_root_element()
        };
        if root.is_null() {
            generic_error!("Document does not have a root element");
            PROGRESULT = XmllintReturnCode::ErrUnclass;
            return;
        }
        let mut i = 0;
        let mut ns = (*root).ns_def;
        while !ns.is_null() && i < 20 {
            namespaces[i] = (*ns).href;
            i += 1;
            namespaces[i] = (*ns).prefix;
            i += 1;
            ns = (*ns).next.load(Ordering::Relaxed);
        }
        namespaces[i] = null();
        i += 1;
        namespaces[i] = null();

        if PATTERN.lock().unwrap().is_some() {
            PATTERNC.store(
                xml_patterncompile(
                    PATTERN.lock().unwrap().as_ref().unwrap().as_ptr() as _,
                    (*doc).dict,
                    0,
                    addr_of_mut!(namespaces[0]),
                ),
                Ordering::Relaxed,
            );
            if PATTERNC.load(Ordering::Relaxed).is_null() {
                generic_error!(
                    "Pattern {} failed to compile\n",
                    PATTERN.lock().unwrap().as_ref().unwrap().to_string_lossy()
                );
                PROGRESULT = XmllintReturnCode::ErrSchemapat;
                *PATTERN.lock().unwrap() = None;
            }
        }
        if !PATTERNC.load(Ordering::Relaxed).is_null() {
            PATSTREAM.store(
                xml_pattern_get_stream_ctxt(PATTERNC.load(Ordering::Relaxed)),
                Ordering::Relaxed,
            );
            if !PATSTREAM.load(Ordering::Relaxed).is_null() {
                ret = xml_stream_push(PATSTREAM.load(Ordering::Relaxed), null_mut(), null_mut());
                if ret < 0 {
                    eprintln!("xmlStreamPush() failure");
                    xml_free_stream_ctxt(PATSTREAM.load(Ordering::Relaxed));
                    PATSTREAM.store(null_mut(), Ordering::Relaxed);
                }
            }
        }
    }
    let reader: XmlTextReaderPtr = xml_reader_walker(doc);
    if !reader.is_null() {
        if TIMING != 0 && REPEAT == 0 {
            start_timer();
        }
        ret = xml_text_reader_read(&mut *reader);
        while ret == 1 {
            #[cfg(feature = "libxml_pattern")]
            let f = !PATTERNC.load(Ordering::Relaxed).is_null();
            #[cfg(not(feature = "libxml_pattern"))]
            let f = false;
            if DEBUG != 0 || f {
                process_node(reader);
            }
            ret = xml_text_reader_read(&mut *reader);
        }
        if TIMING != 0 && REPEAT == 0 {
            end_timer!("walking through the doc");
        }
        xml_free_text_reader(reader);
        if ret != 0 {
            eprintln!("failed to walk through the doc");
            PROGRESULT = XmllintReturnCode::ErrUnclass;
        }
    } else {
        eprintln!("Failed to crate a reader from the document");
        PROGRESULT = XmllintReturnCode::ErrUnclass;
    }
    #[cfg(feature = "libxml_pattern")]
    if !PATSTREAM.load(Ordering::Relaxed).is_null() {
        xml_free_stream_ctxt(PATSTREAM.load(Ordering::Relaxed));
        PATSTREAM.store(null_mut(), Ordering::Relaxed);
    }
}

/************************************************************************
 *                                    *
 *            XPath Query                                     *
 *                                    *
 ************************************************************************/
#[cfg(feature = "xpath")]
unsafe extern "C" fn do_xpath_dump(cur: XmlXPathObjectPtr) {
    use exml::{
        io::{xml_output_buffer_close, xml_output_buffer_create_file, XmlOutputBufferPtr},
        libxml::xpath::{xml_xpath_is_inf, xml_xpath_is_nan, XmlXPathObjectType},
        tree::XmlNodePtr,
    };

    match (*cur).typ {
        XmlXPathObjectType::XpathNodeset => {
            let mut node: XmlNodePtr;
            #[cfg(feature = "output")]
            {
                let buf: XmlOutputBufferPtr;

                if (*cur).nodesetval.is_null() || (*(*cur).nodesetval).node_nr <= 0 {
                    if QUIET == 0 {
                        eprintln!("XPath set is empty");
                    }
                } else {
                    extern "C" {
                        static stdout: *mut FILE;
                    }
                    buf = xml_output_buffer_create_file(stdout, None);
                    if buf.is_null() {
                        eprintln!("Out of memory for XPath");
                        PROGRESULT = XmllintReturnCode::ErrMem;
                        return;
                    }
                    for i in 0..(*(*cur).nodesetval).node_nr {
                        node = *(*(*cur).nodesetval).node_tab.add(i as usize);
                        (*node).dump_output(buf, null_mut(), 0, 0, None);
                        (*buf).write_bytes(b"\n");
                    }
                    xml_output_buffer_close(buf);
                }
            }
            #[cfg(not(feature = "output"))]
            {
                println!("xpath returned {} nodes", (*(*cur).nodesetval).nodeNr);
            }
        }
        XmlXPathObjectType::XpathBoolean => {
            if (*cur).boolval != 0 {
                println!("true");
            } else {
                println!("false");
            }
        }
        XmlXPathObjectType::XpathNumber => match xml_xpath_is_inf((*cur).floatval) {
            1 => {
                println!("Infinity");
            }
            -1 => {
                println!("-Infinity");
            }
            _ => {
                if xml_xpath_is_nan((*cur).floatval) != 0 {
                    println!("NaN");
                } else {
                    println!("{}", (*cur).floatval);
                }
            }
        },
        XmlXPathObjectType::XpathString => {
            println!(
                "{}",
                CStr::from_ptr((*cur).stringval as _).to_string_lossy()
            );
        }
        XmlXPathObjectType::XpathUndefined => {
            eprintln!("XPath Object is uninitialized");
            PROGRESULT = XmllintReturnCode::ErrXpath;
        }
        _ => {
            eprintln!("XPath object of unexpected type");
            PROGRESULT = XmllintReturnCode::ErrXpath;
        }
    }
}

#[cfg(feature = "xpath")]
unsafe extern "C" fn do_xpath_query(doc: XmlDocPtr, query: *const c_char) {
    use exml::{
        libxml::xpath::{
            xml_xpath_eval, xml_xpath_free_context, xml_xpath_free_object, xml_xpath_new_context,
            XmlXPathContextPtr,
        },
        tree::XmlNodePtr,
    };

    let ctxt: XmlXPathContextPtr = xml_xpath_new_context(doc);
    if ctxt.is_null() {
        eprintln!("Out of memory for XPath");
        PROGRESULT = XmllintReturnCode::ErrMem;
        return;
    }
    (*ctxt).node = doc as XmlNodePtr;
    let res: XmlXPathObjectPtr = xml_xpath_eval(query as _, ctxt);
    xml_xpath_free_context(ctxt);

    if res.is_null() {
        eprintln!("XPath evaluation failure");
        PROGRESULT = XmllintReturnCode::ErrXpath;
        return;
    }
    do_xpath_dump(res);
    xml_xpath_free_object(res);
}

/************************************************************************
 *                                    *
 *            Tree Test processing                *
 *                                    *
 ************************************************************************/
unsafe fn parse_and_print_file(filename: Option<&str>, rectxt: XmlParserCtxtPtr) {
    let mut doc: XmlDocPtr = null_mut();
    #[cfg(feature = "tree")]
    let tmp: XmlDocPtr;

    if TIMING != 0 && REPEAT == 0 {
        start_timer();
    }

    if cfg!(feature = "tree") && filename.is_none() {
        #[cfg(feature = "tree")]
        {
            if GENERATE != 0 {
                doc = xml_new_doc(Some("1.0"));
                let n = xml_new_doc_node(doc, null_mut(), c"info".as_ptr() as _, null_mut());
                (*n).set_content(c"abc".as_ptr() as _);
                (*doc).set_root_element(n);
            }
        }
    } else if cfg!(feature = "html") && cfg!(feature = "push") && HTML != 0 && PUSH != 0 {
        #[cfg(all(feature = "html", feature = "push"))]
        {
            extern "C" {
                static stdin: *mut FILE;
            }

            let f = if filename == Some("-") {
                stdin
            } else {
                let f = CString::new(filename.unwrap()).unwrap();
                fopen(f.as_ptr(), c"rb".as_ptr())
            };
            if !f.is_null() {
                let mut res: c_int;
                let mut chars: [c_char; 4096] = [0; 4096];
                let ctxt: HtmlParserCtxtPtr;

                res = fread(chars.as_mut_ptr() as _, 1, 4, f) as _;
                if res > 0 {
                    let filename = filename.map(|f| CString::new(f).unwrap());
                    ctxt = html_create_push_parser_ctxt(
                        null_mut(),
                        None,
                        chars.as_ptr(),
                        res,
                        filename.map_or(null(), |f| f.as_ptr()),
                        XmlCharEncoding::None,
                    );
                    if ctxt.is_null() {
                        PROGRESULT = XmllintReturnCode::ErrMem;
                        if f != stdin {
                            fclose(f);
                        }
                        return;
                    }
                    html_ctxt_use_options(ctxt, OPTIONS);
                    while {
                        res = fread(chars.as_mut_ptr() as _, 1, PUSHSIZE as _, f) as _;
                        res > 0
                    } {
                        html_parse_chunk(ctxt, chars.as_ptr(), res, 0);
                    }
                    html_parse_chunk(ctxt, chars.as_ptr(), 0, 1);
                    doc = (*ctxt).my_doc;
                    html_free_parser_ctxt(ctxt);
                }
                if f != stdin {
                    fclose(f);
                }
            }
        }
    } else if cfg!(feature = "html") && HTML != 0 && MEMORY != 0 {
        #[cfg(feature = "html")]
        {
            let mut info: stat = unsafe { zeroed() };
            let fname = CString::new(filename.unwrap()).unwrap();
            if stat(fname.as_ptr(), addr_of_mut!(info)) < 0 {
                return;
            }
            let fd: c_int = open(fname.as_ptr(), O_RDONLY);
            if fd < 0 {
                return;
            }
            let base: *const c_char =
                mmap(null_mut(), info.st_size as _, PROT_READ, MAP_SHARED, fd, 0) as _;
            if base == MAP_FAILED as _ {
                close(fd);
                eprintln!("mmap failure for file {}", filename.unwrap());
                PROGRESULT = XmllintReturnCode::ErrRdfile;
                return;
            }

            let mem = from_raw_parts(base as *const u8, info.st_size as usize).to_vec();
            doc = html_read_memory(mem, filename, None, OPTIONS);

            munmap(base as _, info.st_size as _);
            close(fd);
        }
    } else if cfg!(feature = "html") && HTML != 0 {
        #[cfg(feature = "html")]
        {
            let filename = filename.map(|f| CString::new(f).unwrap());
            doc = html_read_file(filename.map_or(null(), |f| f.as_ptr()), None, OPTIONS);
        }
    } else if cfg!(feature = "push") && PUSH != 0 {
        /*
         * build an XML tree from a string;
         */
        #[cfg(feature = "push")]
        {
            extern "C" {
                static stdin: *mut FILE;
            }

            let fname = filename.map(|f| CString::new(f).unwrap());
            /* '-' Usually means stdin -<sven@zen.org> */
            let f = if filename == Some("-") {
                stdin
            } else {
                fopen(
                    fname.as_ref().map_or(null(), |f| f.as_ptr()),
                    c"rb".as_ptr(),
                )
            };
            if !f.is_null() {
                let ret: c_int;
                let mut res: c_int;
                let size: c_int = 1024;
                let mut chars: [c_char; 1024] = [0; 1024];
                let ctxt: XmlParserCtxtPtr;

                /* if (repeat) size = 1024; */
                res = fread(chars.as_mut_ptr() as _, 1, 4, f) as _;
                if res > 0 {
                    ctxt = xml_create_push_parser_ctxt(
                        null_mut(),
                        None,
                        chars.as_ptr(),
                        res,
                        fname.map_or(null(), |s| s.as_ptr()),
                    );
                    if ctxt.is_null() {
                        PROGRESULT = XmllintReturnCode::ErrMem;
                        if f != stdin {
                            fclose(f);
                        }
                        return;
                    }
                    xml_ctxt_use_options(ctxt, OPTIONS);
                    while {
                        res = fread(chars.as_mut_ptr() as _, 1, size as _, f) as i32;
                        res > 0
                    } {
                        xml_parse_chunk(ctxt, chars.as_ptr(), res, 0);
                    }
                    xml_parse_chunk(ctxt, chars.as_ptr(), 0, 1);
                    doc = (*ctxt).my_doc;
                    ret = (*ctxt).well_formed;
                    xml_free_parser_ctxt(ctxt);
                    if ret == 0 && RECOVERY == 0 {
                        xml_free_doc(doc);
                        doc = null_mut();
                    }
                }
                if f != stdin {
                    fclose(f);
                }
            }
        }
    } else if TEST_IO != 0 {
        if filename == Some("-") {
            doc = xml_read_io(stdin(), None, None, OPTIONS);
        } else {
            let fname = filename.map(|s| CString::new(s).unwrap());
            let f: *mut FILE = fopen(fname.map_or(null(), |f| f.as_ptr()), c"rb".as_ptr());
            if let Some(Ok(f)) = filename.map(File::open) {
                if rectxt.is_null() {
                    doc = xml_read_io(f, filename, None, OPTIONS);
                } else {
                    doc = xml_ctxt_read_io(rectxt, f, filename, None, OPTIONS);
                }
            } else {
                doc = null_mut();
            }
        }
    } else if HTMLOUT != 0 {
        let ctxt: XmlParserCtxtPtr;

        if rectxt.is_null() {
            ctxt = xml_new_parser_ctxt();
            if ctxt.is_null() {
                PROGRESULT = XmllintReturnCode::ErrMem;
                return;
            }
        } else {
            ctxt = rectxt;
        }

        (*(*ctxt).sax).error = Some(xml_html_error);
        (*(*ctxt).sax).warning = Some(xml_html_warning);
        (*ctxt).vctxt.error = Some(xml_html_validity_error);
        (*ctxt).vctxt.warning = Some(xml_html_validity_warning);

        let fname = filename.map(|f| CString::new(f).unwrap());
        doc = xml_ctxt_read_file(ctxt, fname.map_or(null(), |f| f.as_ptr()), None, OPTIONS);

        if rectxt.is_null() {
            xml_free_parser_ctxt(ctxt);
        }
    } else if MEMORY != 0 {
        let mut info: stat = unsafe { zeroed() };
        let fname = filename.map(|f| CString::new(f).unwrap());

        if stat(
            fname.as_ref().map_or(null(), |f| f.as_ptr()),
            addr_of_mut!(info),
        ) < 0
        {
            return;
        }
        let fd: c_int = open(fname.map_or(null(), |f| f.as_ptr()), O_RDONLY);
        if fd < 0 {
            return;
        }
        let base: *const c_char =
            mmap(null_mut(), info.st_size as _, PROT_READ, MAP_SHARED, fd, 0) as _;
        if base == MAP_FAILED as _ {
            close(fd);
            eprintln!("mmap failure for file {}", filename.unwrap());
            PROGRESULT = XmllintReturnCode::ErrRdfile;
            return;
        }

        let mem = from_raw_parts(base as *const u8, info.st_size as usize).to_vec();
        if rectxt.is_null() {
            doc = xml_read_memory(mem, filename, None, OPTIONS);
        } else {
            doc = xml_ctxt_read_memory(rectxt, mem, filename, None, OPTIONS);
        }

        munmap(base as _, info.st_size as _);
        close(fd);
    } else if cfg!(feature = "valid") && VALID != 0 {
        #[cfg(feature = "valid")]
        {
            let ctxt: XmlParserCtxtPtr;

            if rectxt.is_null() {
                ctxt = xml_new_parser_ctxt();
                if ctxt.is_null() {
                    PROGRESULT = XmllintReturnCode::ErrMem;
                    return;
                }
            } else {
                ctxt = rectxt;
            }

            let fname = filename.map(|f| CString::new(f).unwrap());
            doc = xml_ctxt_read_file(ctxt, fname.map_or(null(), |f| f.as_ptr()), None, OPTIONS);

            if (*ctxt).valid == 0 {
                PROGRESULT = XmllintReturnCode::ErrRdfile;
            }
            if rectxt.is_null() {
                xml_free_parser_ctxt(ctxt);
            }
        }
    } else if !rectxt.is_null() {
        let fname = filename.map(|f| CString::new(f).unwrap());
        doc = xml_ctxt_read_file(rectxt, fname.map_or(null(), |f| f.as_ptr()), None, OPTIONS);
    } else {
        let fname = filename.map(|f| CString::new(f).unwrap());
        doc = xml_read_file(fname.map_or(null(), |f| f.as_ptr()), None, OPTIONS);
    }

    /*
     * If we don't have a document we might as well give up.  Do we
     * want an error message here?  <sven@zen.org> */
    if doc.is_null() {
        PROGRESULT = XmllintReturnCode::ErrUnclass;
        return;
    }

    if TIMING != 0 && REPEAT == 0 {
        end_timer!("Parsing");
    }

    /*
     * Remove DOCTYPE nodes
     */
    if DROPDTD != 0 {
        let dtd: XmlDtdPtr = (*doc).get_int_subset();
        if !dtd.is_null() {
            (*dtd).unlink();
            (*doc).int_subset = null_mut();
            xml_free_dtd(dtd);
        }
    }

    #[cfg(feature = "xinclude")]
    if XINCLUDE != 0 {
        if TIMING != 0 && REPEAT == 0 {
            start_timer();
        }
        if xml_xinclude_process_flags(doc, OPTIONS) < 0 {
            PROGRESULT = XmllintReturnCode::ErrUnclass;
        }
        if TIMING != 0 && REPEAT == 0 {
            end_timer!("Xinclude processing");
        }
    }

    #[cfg(feature = "xpath")]
    if let Some(query) = XPATHQUERY.lock().unwrap().as_ref() {
        do_xpath_query(doc, query.as_ptr());
    }

    extern "C" {
        static stdout: *mut FILE;
    }

    /*
     * shell interaction
     */
    #[cfg(all(feature = "libxml_debug", feature = "xpath"))]
    if SHELL != 0 {
        xml_xpath_order_doc_elems(doc);
        let fname = filename.map(|f| CString::new(f).unwrap());
        xml_shell(
            doc,
            fname.map_or(null_mut(), |f| f.as_ptr() as _),
            Some(xml_shell_readline),
            stdout,
        );
    }

    /*
     * test intermediate copy if needed.
     */
    #[cfg(feature = "tree")]
    if COPY != 0 {
        tmp = doc;
        if TIMING != 0 {
            start_timer();
        }
        doc = xml_copy_doc(doc, 1);
        if TIMING != 0 {
            end_timer!("Copying");
        }
        if TIMING != 0 {
            start_timer();
        }
        xml_free_doc(tmp);
        if TIMING != 0 {
            end_timer!("Freeing original");
        }
    }

    if cfg!(feature = "valid") && INSERT != 0 && HTML == 0 {
        #[cfg(feature = "valid")]
        {
            let mut list: [*const XmlChar; 256] = [null(); 256];

            if !(*doc).children.is_null() {
                let mut node = (*doc).children;
                while !node.is_null() && (*node).last.is_null() {
                    node = (*node).next;
                }
                if !node.is_null() {
                    let nb = xml_valid_get_valid_elements(
                        (*node).last,
                        null_mut(),
                        list.as_mut_ptr(),
                        256,
                    );
                    match nb.cmp(&0) {
                        std::cmp::Ordering::Less => {
                            eprintln!("could not get valid list of elements")
                        }
                        std::cmp::Ordering::Equal => {
                            eprintln!("No element can be inserted under root")
                        }
                        std::cmp::Ordering::Greater => {
                            eprintln!("{} element types can be inserted under root:", nb);
                            for &l in list.iter().take(nb as usize) {
                                eprintln!("{}", CStr::from_ptr(l as _).to_string_lossy());
                            }
                        }
                    }
                }
            }
        }
    } else if cfg!(feature = "libxml_reader") && WALKER != 0 {
        #[cfg(feature = "libxml_reader")]
        {
            walk_doc(doc);
        }
    }
    #[cfg(feature = "output")]
    if NOOUT == 0 {
        let ret: c_int;

        /*
         * print it.
         */
        if !cfg!(feature = "libxml_debug") || DEBUG == 0 {
            if TIMING != 0 && REPEAT == 0 {
                start_timer();
            }
            if cfg!(feature = "html") && HTML != 0 && XMLOUT == 0 {
                #[cfg(feature = "html")]
                {
                    if COMPRESS != 0 {
                        let o = OUTPUT
                            .lock()
                            .unwrap()
                            .as_ref()
                            .map_or(c"-".as_ptr(), |o| o.as_ptr());
                        html_save_file(o, doc);
                    } else if let Some(encoding) = ENCODING.lock().unwrap().as_ref() {
                        let o = OUTPUT
                            .lock()
                            .unwrap()
                            .as_ref()
                            .map_or(c"-".as_ptr(), |o| o.as_ptr());
                        if FORMAT == 1 {
                            html_save_file_format(o, doc, Some(encoding.as_str()), 1);
                        } else {
                            html_save_file_format(o, doc, Some(encoding.as_str()), 0);
                        }
                    } else if FORMAT == 1 {
                        let o = OUTPUT
                            .lock()
                            .unwrap()
                            .as_ref()
                            .map_or(c"-".as_ptr(), |o| o.as_ptr());
                        html_save_file_format(o, doc, None, 1);
                    } else {
                        let out: *mut FILE = OUTPUT
                            .lock()
                            .unwrap()
                            .as_ref()
                            .map_or(stdout, |o| fopen(o.as_ptr(), c"wb".as_ptr()));
                        if !out.is_null() {
                            if html_doc_dump(out, doc) < 0 {
                                PROGRESULT = XmllintReturnCode::ErrOut;
                            }

                            if OUTPUT.lock().unwrap().is_some() {
                                fclose(out);
                            }
                        } else {
                            eprintln!(
                                "failed to open {}",
                                OUTPUT.lock().unwrap().as_ref().unwrap().to_string_lossy()
                            );
                            PROGRESULT = XmllintReturnCode::ErrOut;
                        }
                    }
                    if TIMING != 0 && REPEAT == 0 {
                        end_timer!("Saving");
                    }
                }
            } else if cfg!(feature = "libxml_c14n") && CANONICAL != 0 {
                #[cfg(feature = "libxml_c14n")]
                {
                    let mut result: *mut XmlChar = null_mut();

                    let size: c_int = xml_c14n_doc_dump_memory(
                        doc,
                        null_mut(),
                        XmlC14NMode::XmlC14N1_0 as i32,
                        null_mut(),
                        1,
                        addr_of_mut!(result),
                    );
                    if size >= 0 {
                        if write(1, result as _, size as _) == -1 {
                            eprintln!("Can't write data");
                        }
                        xml_free(result as _);
                    } else {
                        eprintln!("Failed to canonicalize");
                        PROGRESULT = XmllintReturnCode::ErrOut;
                    }
                }
            } else if cfg!(feature = "libxml_c14n") && CANONICAL_11 != 0 {
                #[cfg(feature = "libxml_c14n")]
                {
                    let mut result: *mut XmlChar = null_mut();

                    let size: c_int = xml_c14n_doc_dump_memory(
                        doc,
                        null_mut(),
                        XmlC14NMode::XmlC14N1_1 as i32,
                        null_mut(),
                        1,
                        addr_of_mut!(result),
                    );
                    if size >= 0 {
                        if write(1, result as _, size as _) == -1 {
                            eprintln!("Can't write data");
                        }
                        xml_free(result as _);
                    } else {
                        eprintln!("Failed to canonicalize");
                        PROGRESULT = XmllintReturnCode::ErrOut;
                    }
                }
            } else if cfg!(feature = "libxml_c14n") && EXC_CANONICAL != 0 {
                #[cfg(feature = "libxml_c14n")]
                {
                    let mut result: *mut XmlChar = null_mut();

                    let size: c_int = xml_c14n_doc_dump_memory(
                        doc,
                        null_mut(),
                        XmlC14NMode::XmlC14NExclusive1_0 as i32,
                        null_mut(),
                        1,
                        addr_of_mut!(result),
                    );
                    if size >= 0 {
                        if write(1, result as _, size as _) == -1 {
                            eprintln!("Can't write data");
                        }
                        xml_free(result as _);
                    } else {
                        eprintln!("Failed to canonicalize");
                        PROGRESULT = XmllintReturnCode::ErrOut;
                    }
                }
            } else if MEMORY != 0 {
                let mut result: *mut XmlChar = null_mut();
                let mut len: c_int = 0;

                if let Some(encoding) = ENCODING.lock().unwrap().as_ref() {
                    if FORMAT == 1 {
                        (*doc).dump_format_memory_enc(
                            addr_of_mut!(result),
                            addr_of_mut!(len),
                            Some(encoding.as_str()),
                            1,
                        );
                    } else {
                        (*doc).dump_memory_enc(
                            addr_of_mut!(result),
                            addr_of_mut!(len),
                            Some(encoding.as_str()),
                        );
                    }
                } else if FORMAT == 1 {
                    (*doc).dump_format_memory(addr_of_mut!(result), addr_of_mut!(len), 1);
                } else {
                    (*doc).dump_memory(addr_of_mut!(result), addr_of_mut!(len));
                }
                if result.is_null() {
                    eprintln!("Failed to save");
                    PROGRESULT = XmllintReturnCode::ErrOut;
                } else {
                    if write(1, result as _, len as _) == -1 {
                        eprintln!("Can't write data");
                    }
                    xml_free(result as _);
                }
            } else if COMPRESS != 0 {
                let o = OUTPUT
                    .lock()
                    .unwrap()
                    .as_ref()
                    .map_or(c"-".as_ptr(), |o| o.as_ptr());
                (*doc).save_file(o);
            } else if OLDOUT != 0 {
                if let Some(encoding) = ENCODING.lock().unwrap().as_ref() {
                    if FORMAT == 1 {
                        let o = OUTPUT
                            .lock()
                            .unwrap()
                            .as_ref()
                            .map_or(c"-".as_ptr(), |o| o.as_ptr());
                        ret = (*doc).save_format_file_enc(o, Some(encoding.as_str()), 1);
                    } else {
                        let o = OUTPUT
                            .lock()
                            .unwrap()
                            .as_ref()
                            .map_or(c"-".as_ptr(), |o| o.as_ptr());
                        ret = (*doc).save_file_enc(o, Some(encoding.as_str()));
                    }
                    if ret < 0 {
                        let lock = OUTPUT.lock().unwrap();
                        let o = lock.as_ref().map_or(c"-", |o| o.as_c_str());
                        eprintln!("failed save to {}", o.to_string_lossy());
                        PROGRESULT = XmllintReturnCode::ErrOut;
                    }
                } else if FORMAT == 1 {
                    let o = OUTPUT
                        .lock()
                        .unwrap()
                        .as_ref()
                        .map_or(c"-".as_ptr(), |o| o.as_ptr());
                    ret = (*doc).save_format_file(o, 1);
                    if ret < 0 {
                        let lock = OUTPUT.lock().unwrap();
                        let o = lock.as_ref().map_or(c"-", |o| o.as_c_str());
                        eprintln!("failed save to {}", o.to_string_lossy());
                        PROGRESULT = XmllintReturnCode::ErrOut;
                    }
                } else {
                    let out: *mut FILE = OUTPUT
                        .lock()
                        .unwrap()
                        .as_ref()
                        .map_or(stdout, |o| fopen(o.as_ptr(), c"wb".as_ptr()));
                    if !out.is_null() {
                        if doc.is_null() || (*doc).dump_file(out) < 0 {
                            PROGRESULT = XmllintReturnCode::ErrOut;
                        }

                        if OUTPUT.lock().unwrap().is_some() {
                            fclose(out);
                        }
                    } else {
                        eprintln!(
                            "failed to open {}",
                            OUTPUT.lock().unwrap().as_ref().unwrap().to_string_lossy()
                        );
                        PROGRESULT = XmllintReturnCode::ErrOut;
                    }
                }
            } else {
                let ctxt: XmlSaveCtxtPtr;
                let mut save_opts: c_int = 0;

                if FORMAT == 1 {
                    save_opts |= XmlSaveOption::XmlSaveFormat as i32;
                } else if FORMAT == 2 {
                    save_opts |= XmlSaveOption::XmlSaveWsnonsig as i32;
                }

                #[cfg(any(feature = "html", feature = "valid"))]
                if XMLOUT != 0 {
                    save_opts |= XmlSaveOption::XmlSaveAsXML as i32;
                }

                if let Some(o) = OUTPUT.lock().unwrap().as_ref() {
                    ctxt = xml_save_to_filename(
                        o.as_ptr(),
                        ENCODING.lock().unwrap().as_deref(),
                        save_opts,
                    );
                } else {
                    extern "C" {
                        static stdout: *mut FILE;
                    }
                    ctxt = xml_save_to_io(
                        Some(xml_file_write),
                        Some(xml_file_flush),
                        stdout as _,
                        ENCODING.lock().unwrap().as_deref(),
                        save_opts,
                    );
                }

                if !ctxt.is_null() {
                    if xml_save_doc(ctxt, doc) < 0 {
                        let lock = OUTPUT.lock().unwrap();
                        let o = lock.as_ref().map_or(c"-", |o| o.as_c_str());
                        eprintln!("failed save to {}", o.to_string_lossy());
                        PROGRESULT = XmllintReturnCode::ErrOut;
                    }
                    xml_save_close(ctxt);
                } else {
                    PROGRESULT = XmllintReturnCode::ErrOut;
                }
            }
            if TIMING != 0 && REPEAT == 0 {
                end_timer!("Saving");
            }
        } else {
            #[cfg(feature = "libxml_debug")]
            {
                let out: *mut FILE = OUTPUT
                    .lock()
                    .unwrap()
                    .as_ref()
                    .map_or(stdout, |o| fopen(o.as_ptr(), c"wb".as_ptr()));
                if !out.is_null() {
                    xml_debug_dump_document(out, doc);

                    if OUTPUT.lock().unwrap().is_some() {
                        fclose(out);
                    }
                } else {
                    eprintln!(
                        "failed to open {}",
                        OUTPUT.lock().unwrap().as_ref().unwrap().to_string_lossy()
                    );
                    PROGRESULT = XmllintReturnCode::ErrOut;
                }
            }
        }
    }

    /*
     * A posteriori validation test
     */
    #[cfg(feature = "valid")]
    if DTDVALID.lock().unwrap().is_some() || DTDVALIDFPI.lock().unwrap().is_some() {
        let dtd: XmlDtdPtr;

        if TIMING != 0 && REPEAT == 0 {
            start_timer();
        }
        if let Some(dtd_valid) = DTDVALID.lock().unwrap().as_ref() {
            dtd = xml_parse_dtd(null_mut(), dtd_valid.as_ptr() as _);
        } else {
            dtd = xml_parse_dtd(
                DTDVALIDFPI
                    .lock()
                    .unwrap()
                    .as_ref()
                    .map_or(null_mut(), |d| d.as_ptr() as *mut u8),
                null_mut(),
            );
        }
        if TIMING != 0 && REPEAT == 0 {
            end_timer!("Parsing DTD");
        }
        if dtd.is_null() {
            if let Some(dtd_valid) = DTDVALID.lock().unwrap().as_ref() {
                generic_error!("Could not parse DTD {}\n", dtd_valid.to_string_lossy());
            } else {
                generic_error!(
                    "Could not parse DTD {}\n",
                    DTDVALIDFPI
                        .lock()
                        .unwrap()
                        .as_ref()
                        .unwrap()
                        .to_string_lossy()
                );
            }
            PROGRESULT = XmllintReturnCode::ErrDtd;
        } else {
            let cvp = xml_new_valid_ctxt();
            if cvp.is_null() {
                generic_error!("Couldn't allocate validation context\n");
                PROGRESULT = XmllintReturnCode::ErrMem;
                xml_free_dtd(dtd);
                return;
            }
            (*cvp).error = Some(generic_error_default);
            (*cvp).warning = Some(generic_error_default);

            if TIMING != 0 && REPEAT == 0 {
                start_timer();
            }
            if xml_validate_dtd(cvp, doc, dtd) == 0 {
                let filename = filename.unwrap();
                if let Some(dtd_valid) = DTDVALID.lock().unwrap().as_ref() {
                    generic_error!(
                        "Document {filename} does not validate against {}\n",
                        dtd_valid.to_string_lossy()
                    );
                } else {
                    generic_error!(
                        "Document {filename} does not validate against {}\n",
                        DTDVALIDFPI
                            .lock()
                            .unwrap()
                            .as_ref()
                            .unwrap()
                            .to_string_lossy()
                    );
                }
                PROGRESULT = XmllintReturnCode::ErrValid;
            }
            if TIMING != 0 && REPEAT == 0 {
                end_timer!("Validating against DTD");
            }
            xml_free_valid_ctxt(cvp);
            xml_free_dtd(dtd);
        }
    } else if POSTVALID != 0 {
        let cvp = xml_new_valid_ctxt();
        if cvp.is_null() {
            generic_error!("Couldn't allocate validation context\n");
            PROGRESULT = XmllintReturnCode::ErrMem;
            xml_free_doc(doc);
            return;
        }

        if TIMING != 0 && REPEAT == 0 {
            start_timer();
        }
        (*cvp).error = Some(generic_error_default);
        (*cvp).warning = Some(generic_error_default);
        if xml_validate_document(cvp, doc) == 0 {
            let filename = filename.unwrap();
            generic_error!("Document {filename} does not validate\n");
            PROGRESULT = XmllintReturnCode::ErrValid;
        }
        if TIMING != 0 && REPEAT == 0 {
            end_timer!("Validating");
        }
        xml_free_valid_ctxt(cvp);
    }
    #[cfg(feature = "libxml_schematron")]
    if !WXSCHEMATRON.load(Ordering::Relaxed).is_null() {
        if TIMING != 0 && REPEAT == 0 {
            start_timer();
        }

        let mut flag = if DEBUG != 0 {
            XmlSchematronValidOptions::XmlSchematronOutXml as i32
        } else {
            XmlSchematronValidOptions::XmlSchematronOutText as i32
        };
        if NOOUT != 0 {
            flag |= XmlSchematronValidOptions::XmlSchematronOutQuiet as i32;
        }
        let ctxt: XmlSchematronValidCtxtPtr =
            xml_schematron_new_valid_ctxt(WXSCHEMATRON.load(Ordering::Relaxed), flag);
        if ctxt.is_null() {
            PROGRESULT = XmllintReturnCode::ErrMem;
            xml_free_doc(doc);
            return;
        }
        match xml_schematron_validate_doc(ctxt, doc).cmp(&0) {
            std::cmp::Ordering::Equal => {
                if QUIET == 0 {
                    eprintln!("{} validates", filename.unwrap());
                }
            }
            std::cmp::Ordering::Greater => {
                eprintln!("{} fails to validate", filename.unwrap());
                PROGRESULT = XmllintReturnCode::ErrValid;
            }
            std::cmp::Ordering::Less => {
                eprintln!(
                    "{} validation generated an internal error",
                    filename.unwrap()
                );
                PROGRESULT = XmllintReturnCode::ErrValid;
            }
        }
        xml_schematron_free_valid_ctxt(ctxt);
        if TIMING != 0 && REPEAT == 0 {
            end_timer!("Validating");
        }
    }
    #[cfg(feature = "schema")]
    if !RELAXNGSCHEMAS.load(Ordering::Relaxed).is_null() {
        if TIMING != 0 && REPEAT == 0 {
            start_timer();
        }

        let ctxt: XmlRelaxNGValidCtxtPtr =
            xml_relaxng_new_valid_ctxt(RELAXNGSCHEMAS.load(Ordering::Relaxed));
        if ctxt.is_null() {
            PROGRESULT = XmllintReturnCode::ErrMem;
            xml_free_doc(doc);
            return;
        }
        xml_relaxng_set_valid_errors(
            ctxt,
            Some(generic_error_default),
            Some(generic_error_default),
            None,
        );
        match xml_relaxng_validate_doc(ctxt, doc).cmp(&0) {
            std::cmp::Ordering::Equal => {
                if QUIET == 0 {
                    eprintln!("{} validates", filename.unwrap());
                }
            }
            std::cmp::Ordering::Greater => {
                eprintln!("{} fails to validate", filename.unwrap());
                PROGRESULT = XmllintReturnCode::ErrValid;
            }
            std::cmp::Ordering::Less => {
                eprintln!(
                    "{} validation generated an internal error",
                    filename.unwrap()
                );
                PROGRESULT = XmllintReturnCode::ErrValid;
            }
        }
        xml_relaxng_free_valid_ctxt(ctxt);
        if TIMING != 0 && REPEAT == 0 {
            end_timer!("Validating");
        }
    } else if !WXSCHEMAS.load(Ordering::Relaxed).is_null() {
        if TIMING != 0 && REPEAT == 0 {
            start_timer();
        }

        let ctxt: XmlSchemaValidCtxtPtr =
            xml_schema_new_valid_ctxt(WXSCHEMAS.load(Ordering::Relaxed));
        if ctxt.is_null() {
            PROGRESULT = XmllintReturnCode::ErrMem;
            xml_free_doc(doc);
            return;
        }
        xml_schema_set_valid_errors(
            ctxt,
            Some(generic_error_default),
            Some(generic_error_default),
            None,
        );
        match xml_schema_validate_doc(ctxt, doc).cmp(&0) {
            std::cmp::Ordering::Equal => {
                if QUIET == 0 {
                    eprintln!("{} validates", filename.unwrap());
                }
            }
            std::cmp::Ordering::Greater => {
                eprintln!("{} fails to validate", filename.unwrap());
                PROGRESULT = XmllintReturnCode::ErrValid;
            }
            std::cmp::Ordering::Less => {
                eprintln!(
                    "{} validation generated an internal error",
                    filename.unwrap()
                );
                PROGRESULT = XmllintReturnCode::ErrValid;
            }
        }
        xml_schema_free_valid_ctxt(ctxt);
        if TIMING != 0 && REPEAT == 0 {
            end_timer!("Validating");
        }
    }

    extern "C" {
        static stderr: *mut FILE;
    }

    #[cfg(all(feature = "libxml_debug", any(feature = "html", feature = "valid")))]
    if DEBUGENT != 0 && HTML == 0 {
        xml_debug_dump_entities(stderr, doc);
    }

    /*
     * free it.
     */
    if TIMING != 0 && REPEAT == 0 {
        start_timer();
    }
    xml_free_doc(doc);
    if TIMING != 0 && REPEAT == 0 {
        end_timer!("Freeing");
    }
}

/************************************************************************
 *                                    *
 *            Usage and Main                    *
 *                                    *
 ************************************************************************/
fn show_version(name: &str) {
    eprintln!(
        "{}: using libxml version {}",
        name,
        env!("CARGO_PKG_VERSION")
    );
    eprint!("   compiled with: ");
    if xml_has_feature(Some(XmlFeature::XmlWithThread)) {
        eprint!("Threads ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithTree)) {
        eprint!("Tree ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithOutput)) {
        eprint!("Output ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithPush)) {
        eprint!("Push ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithReader)) {
        eprint!("Reader ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithPattern)) {
        eprint!("Patterns ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithWriter)) {
        eprint!("Writer ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithSax1)) {
        eprint!("SAXv1 ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithFtp)) {
        eprint!("FTP ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithHttp)) {
        eprint!("HTTP ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithValid)) {
        eprint!("DTDValid ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithHtml)) {
        eprint!("HTML ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithLegacy)) {
        eprint!("Legacy ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithC14n)) {
        eprint!("C14N ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithCatalog)) {
        eprint!("Catalog ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithXpath)) {
        eprint!("XPath ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithXptr)) {
        eprint!("XPointer ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithXinclude)) {
        eprint!("XInclude ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithIconv)) {
        eprint!("Iconv ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithIcu)) {
        eprint!("ICU ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithIso8859x)) {
        eprint!("ISO8859X ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithUnicode)) {
        eprint!("Unicode ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithRegexp)) {
        eprint!("Regexps ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithAutomata)) {
        eprint!("Automata ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithExpr)) {
        eprint!("Expr ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithSchemas)) {
        eprint!("Schemas ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithSchematron)) {
        eprint!("Schematron ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithModules)) {
        eprint!("Modules ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithDebug)) {
        eprint!("Debug ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithDebugMem)) {
        eprint!("MemDebug ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithDebugRun)) {
        eprint!("RunDebug ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithZlib)) {
        eprint!("Zlib ");
    }
    if xml_has_feature(Some(XmlFeature::XmlWithLzma)) {
        eprint!("Lzma ");
    }
    eprintln!();
}

fn usage(f: &mut impl Write, name: &str) {
    writeln!(f, "Usage : {} [options] XMLfiles ...", name).ok();
    #[cfg(feature = "output")]
    {
        writeln!(
            f,
            "\tParse the XML files and output the result of the parsing",
        )
        .ok();
    }
    #[cfg(not(feature = "output"))]
    {
        writeln!(f, "\tParse the XML files").ok();
    }
    writeln!(
        f,
        "\t--version : display the version of the XML library used",
    )
    .ok();
    #[cfg(feature = "libxml_debug")]
    {
        writeln!(f, "\t--debug : dump a debug tree of the in-memory document",).ok();
        writeln!(f, "\t--shell : run a navigating shell").ok();
        writeln!(
            f,
            "\t--debugent : debug the entities defined in the document",
        )
        .ok();
    }
    #[cfg(not(feature = "libxml_debug"))]
    {
        #[cfg(feature = "libxml_reader")]
        {
            writeln!(f, "\t--debug : dump the nodes content when using --stream",).ok();
        }
    }
    #[cfg(feature = "tree")]
    {
        writeln!(
            f,
            "\t--copy : used to test the internal copy implementation",
        )
        .ok();
    }
    writeln!(
        f,
        "\t--recover : output what was parsable on broken XML documents",
    )
    .ok();
    writeln!(f, "\t--huge : remove any internal arbitrary parser limits",).ok();
    writeln!(f, "\t--noent : substitute entity references by their value",).ok();
    writeln!(
        f,
        "\t--noenc : ignore any encoding specified inside the document",
    )
    .ok();
    writeln!(f, "\t--noout : don't output the result tree").ok();
    writeln!(f, "\t--path 'paths': provide a set of paths for resources",).ok();
    writeln!(
        f,
        "\t--load-trace : print trace of all external entities loaded",
    )
    .ok();
    writeln!(
        f,
        "\t--nonet : refuse to fetch DTDs or entities over network",
    )
    .ok();
    writeln!(f, "\t--nocompact : do not generate compact text nodes").ok();
    writeln!(f, "\t--htmlout : output results as HTML").ok();
    writeln!(f, "\t--nowrap : do not put HTML doc wrapper").ok();
    #[cfg(feature = "valid")]
    {
        writeln!(
            f,
            "\t--valid : validate the document in addition to std well-formed check",
        )
        .ok();
        writeln!(
            f,
            "\t--postvalid : do a posteriori validation, i.e after parsing",
        )
        .ok();
        writeln!(
            f,
            "\t--dtdvalid URL : do a posteriori validation against a given DTD",
        )
        .ok();
        writeln!(
            f,
            "\t--dtdvalidfpi FPI : same but name the DTD with a Public Identifier",
        )
        .ok();
    }
    writeln!(f, "\t--quiet : be quiet when succeeded").ok();
    writeln!(f, "\t--timing : print some timings").ok();
    writeln!(f, "\t--output file or -o file: save to a given file").ok();
    writeln!(f, "\t--repeat : repeat 100 times, for timing or profiling",).ok();
    writeln!(f, "\t--insert : ad-hoc test for valid insertions").ok();
    // #ifdef LIBXML_ZLIB_ENABLED
    #[cfg(feature = "output")]
    {
        writeln!(f, "\t--compress : turn on gzip compression of output").ok();
    }
    // #endif
    #[cfg(feature = "html")]
    {
        writeln!(f, "\t--html : use the HTML parser").ok();
        writeln!(
            f,
            "\t--xmlout : force to use the XML serializer when using --html",
        )
        .ok();
        writeln!(f, "\t--nodefdtd : do not default HTML doctype").ok();
    }
    #[cfg(feature = "push")]
    {
        writeln!(f, "\t--push : use the push mode of the parser").ok();
        writeln!(
            f,
            "\t--pushsmall : use the push mode of the parser using tiny increments",
        )
        .ok();
    }
    writeln!(f, "\t--memory : parse from memory").ok();
    writeln!(
        f,
        "\t--maxmem nbbytes : limits memory allocation to nbbytes bytes",
    )
    .ok();
    writeln!(
        f,
        "\t--nowarning : do not emit warnings from parser/validator",
    )
    .ok();
    writeln!(f, "\t--noblanks : drop (ignorable?) blanks spaces").ok();
    writeln!(f, "\t--nocdata : replace cdata section with text nodes").ok();
    #[cfg(feature = "output")]
    {
        writeln!(f, "\t--format : reformat/reindent the output").ok();
        writeln!(f, "\t--encode encoding : output in the given encoding").ok();
        writeln!(f, "\t--dropdtd : remove the DOCTYPE of the input docs").ok();
        writeln!(f, "\t--pretty STYLE : pretty-print in a particular style").ok();
        writeln!(f, "\t                 0 Do not pretty print").ok();
        writeln!(
            f,
            "\t                 1 Format the XML content, as --format",
        )
        .ok();
        writeln!(
            f,
            "\t                 2 Add whitespace inside tags, preserving content",
        )
        .ok();
    }
    #[cfg(feature = "libxml_c14n")]
    {
        writeln!(
            f,
            "\t--c14n : save in W3C canonical format v1.0 (with comments)",
        )
        .ok();
        writeln!(
            f,
            "\t--c14n11 : save in W3C canonical format v1.1 (with comments)",
        )
        .ok();
        writeln!(
            f,
            "\t--exc-c14n : save in W3C exclusive canonical format (with comments)",
        )
        .ok();
    }
    writeln!(f, "\t--nsclean : remove redundant namespace declarations").ok();
    writeln!(f, "\t--testIO : test user I/O support").ok();
    #[cfg(feature = "catalog")]
    {
        writeln!(
            f,
            "\t--catalogs : use SGML catalogs from $SGML_CATALOG_FILES",
        )
        .ok();
        writeln!(f, "\t             otherwise XML Catalogs starting from ").ok();
        writeln!(
            f,
            "\t         {} are activated by default",
            XML_XML_DEFAULT_CATALOG,
        )
        .ok();
        writeln!(f, "\t--nocatalogs: deactivate all catalogs").ok();
    }
    writeln!(f, "\t--auto : generate a small doc on the fly").ok();
    #[cfg(feature = "xinclude")]
    {
        writeln!(f, "\t--xinclude : do XInclude processing").ok();
        writeln!(
            f,
            "\t--noxincludenode : same but do not generate XInclude nodes",
        )
        .ok();
        writeln!(f, "\t--nofixup-base-uris : do not fixup xml:base uris").ok();
    }
    writeln!(f, "\t--loaddtd : fetch external DTD").ok();
    writeln!(
        f,
        "\t--dtdattr : loaddtd + populate the tree with inherited attributes ",
    )
    .ok();
    #[cfg(feature = "libxml_reader")]
    {
        writeln!(
            f,
            "\t--stream : use the streaming interface to process very large files",
        )
        .ok();
        writeln!(
            f,
            "\t--walker : create a reader and walk though the resulting doc",
        )
        .ok();
    }
    #[cfg(all(feature = "libxml_reader", feature = "libxml_pattern"))]
    {
        writeln!(f, "\t--pattern pattern_value : test the pattern support").ok();
    }
    writeln!(f, "\t--chkregister : verify the node registration code").ok();
    #[cfg(feature = "schema")]
    {
        writeln!(
            f,
            "\t--relaxng schema : do RelaxNG validation against the schema",
        )
        .ok();
        writeln!(
            f,
            "\t--schema schema : do validation against the WXS schema",
        )
        .ok();
    }
    #[cfg(feature = "libxml_schematron")]
    {
        writeln!(
            f,
            "\t--schematron schema : do validation against a schematron",
        )
        .ok();
    }
    #[cfg(feature = "sax1")]
    {
        writeln!(f, "\t--sax1: use the old SAX1 interfaces for processing").ok();
    }
    writeln!(
        f,
        "\t--sax: do not build a tree but work just at the SAX level",
    )
    .ok();
    writeln!(
        f,
        "\t--oldxml10: use XML-1.0 parsing rules before the 5th edition",
    )
    .ok();
    #[cfg(feature = "xpath")]
    {
        writeln!(
            f,
            "\t--xpath expr: evaluate the XPath expression, imply --noout",
        )
        .ok();
    }

    writeln!(
        f,
        "\nLibxml project home page: https://gitlab.gnome.org/GNOME/libxml2",
    )
    .ok();
}

unsafe extern "C" fn register_node(node: XmlNodePtr) {
    (*node)._private = malloc(size_of::<c_long>());
    if (*node)._private.is_null() {
        eprintln!("Out of memory in xmllint:registerNode()");
        exit(XmllintReturnCode::ErrMem as i32);
    }
    *((*node)._private as *mut c_long) = 0x81726354;
    NBREGISTER += 1;
}

unsafe extern "C" fn deregister_node(node: XmlNodePtr) {
    assert!(!(*node)._private.is_null());
    assert!(*((*node)._private as *mut c_long) == 0x81726354);
    free((*node)._private);
    NBREGISTER -= 1;
}

fn main() {
    let mut files: c_int = 0;
    let mut version: c_int = 0;

    unsafe {
        let args = args().collect::<Vec<_>>();
        if args.len() == 1 {
            usage(&mut stderr(), &args[0]);
            exit(XmllintReturnCode::ErrUnclass as i32);
        }

        let mut args = args.into_iter();
        let program_name = args.next().unwrap();
        let mut remained = vec![];
        /* xmlMemSetup must be called before initializing the parser. */
        while let Some(arg) = args.next() {
            if !arg.starts_with('-') {
                remained.push(arg);
                continue;
            }

            if arg == "-maxmem" || arg == "--maxmem" {
                let Some(next) = args.next() else {
                    eprintln!("maxmem: missing integer value");
                    exit(XmllintReturnCode::ErrUnclass as i32);
                };

                match next.parse::<usize>() {
                    Ok(val) => MAXMEM = val as i32,
                    Err(e) => match e.kind() {
                        IntErrorKind::PosOverflow | IntErrorKind::NegOverflow => {
                            eprintln!("maxmem: integer out of range: {next}");
                            exit(XmllintReturnCode::ErrUnclass as i32);
                        }
                        _ => {
                            eprintln!("maxmem: invalid integer: {next}");
                            exit(XmllintReturnCode::ErrUnclass as i32);
                        }
                    },
                };
            } else if arg == "-debug" || arg == "--debug" {
                DEBUG += 1;
            } else if cfg!(feature = "libxml_debug") && (arg == "-shell" || arg == "--shell") {
                SHELL += 1;
                NOOUT = 1;
            } else if cfg!(feature = "tree") && (arg == "-copy" || arg == "--copy") {
                COPY += 1;
            } else if arg == "-recover" || arg == "--recover" {
                RECOVERY += 1;
                OPTIONS |= XmlParserOption::XmlParseRecover as i32;
            } else if arg == "-huge" || arg == "--huge" {
                OPTIONS |= XmlParserOption::XmlParseHuge as i32;
            } else if arg == "-noent" || arg == "--noent" {
                NOENT = 1;
            } else if arg == "-noenc" || arg == "--noenc" {
                NOENC += 1;
                OPTIONS |= XmlParserOption::XmlParseIgnoreEnc as i32;
            } else if arg == "-nsclean" || arg == "--nsclean" {
                OPTIONS |= XmlParserOption::XmlParseNsclean as i32;
            } else if arg == "-nocdata" || arg == "--nocdata" {
                OPTIONS |= XmlParserOption::XmlParseNocdata as i32;
            } else if arg == "-nodict" || arg == "--nodict" {
                OPTIONS |= XmlParserOption::XmlParseNodict as i32;
            } else if arg == "-version" || arg == "--version" {
                show_version(&program_name);
                version = 1;
            } else if arg == "-noout" || arg == "--noout" {
                NOOUT += 1;
            } else if cfg!(feature = "output")
                && (arg == "-o" || arg == "-output" || arg == "--output")
            {
                *OUTPUT.lock().unwrap() =
                    CString::new(args.next().expect("--output: file is not specified")).ok();
            } else if arg == "-htmlout" || arg == "--htmlout" {
                HTMLOUT += 1;
            } else if arg == "-nowrap" || arg == "--nowrap" {
                NOWRAP += 1;
            } else if cfg!(feature = "html") && (arg == "-html" || arg == "--html") {
                HTML += 1;
            } else if cfg!(feature = "html") && (arg == "-xmlout" || arg == "--xmlout") {
                XMLOUT += 1;
            } else if cfg!(feature = "html") && (arg == "-nodefdtd" || arg == "--nodefdtd") {
                NODEFDTD += 1;
                OPTIONS |= HtmlParserOption::HtmlParseNodefdtd as i32;
            } else if arg == "-loaddtd" || arg == "--loaddtd" {
                LOADDTD += 1;
                OPTIONS |= XmlParserOption::XmlParseDtdload as i32;
            } else if arg == "-dtdattr" || arg == "--dtdattr" {
                LOADDTD += 1;
                DTDATTRS += 1;
                OPTIONS |= XmlParserOption::XmlParseDtdattr as i32;
            } else if cfg!(feature = "valid") && (arg == "-valid" || arg == "--valid") {
                VALID += 1;
                OPTIONS |= XmlParserOption::XmlParseDtdvalid as i32;
            } else if cfg!(feature = "valid") && (arg == "-postvalid" || arg == "--postvalid") {
                POSTVALID += 1;
                LOADDTD += 1;
                OPTIONS |= XmlParserOption::XmlParseDtdload as i32;
            } else if cfg!(feature = "valid") && (arg == "-dtdvalid" || arg == "--dtdvalid") {
                *DTDVALID.lock().unwrap() =
                    CString::new(args.next().expect("--dtdvalid: DTD is not specified")).ok();
                LOADDTD += 1;
                OPTIONS |= XmlParserOption::XmlParseDtdload as i32;
            } else if cfg!(feature = "valid") && (arg == "-dtdvalidfpi" || arg == "--dtdvalidfpi") {
                *DTDVALIDFPI.lock().unwrap() =
                    CString::new(args.next().expect("--dtdvalidfpi: DTD is not specified")).ok();
                LOADDTD += 1;
                OPTIONS |= XmlParserOption::XmlParseDtdload as i32;
            } else if arg == "-dropdtd" || arg == "--dropdtd" {
                DROPDTD += 1;
            } else if arg == "-insert" || arg == "--insert" {
                INSERT += 1;
            } else if arg == "-quiet" || arg == "--quiet" {
                QUIET += 1;
            } else if arg == "-timing" || arg == "--timing" {
                TIMING += 1;
            } else if arg == "-auto" || arg == "--auto" {
                GENERATE += 1;
            } else if arg == "-repeat" || arg == "--repeat" {
                if REPEAT != 0 {
                    REPEAT *= 10;
                } else {
                    REPEAT = 100;
                }
            } else if cfg!(feature = "push") && (arg == "-push" || arg == "--push") {
                PUSH += 1;
            } else if cfg!(feature = "push") && (arg == "-pushsmall" || arg == "--pushsmall") {
                PUSH += 1;
                PUSHSIZE = 10;
            } else if arg == "-memory" || arg == "--memory" {
                MEMORY += 1;
            } else if arg == "-testIO" || arg == "--testIO" {
                TEST_IO += 1;
            } else if cfg!(feature = "xinclude") && (arg == "-xinclude" || arg == "--xinclude") {
                XINCLUDE += 1;
                OPTIONS |= XmlParserOption::XmlParseXinclude as i32;
            } else if cfg!(feature = "xinclude")
                && (arg == "-noxincludenode" || arg == "--noxincludenode")
            {
                XINCLUDE += 1;
                OPTIONS |= XmlParserOption::XmlParseXinclude as i32;
                OPTIONS |= XmlParserOption::XmlParseNoxincnode as i32;
            } else if cfg!(feature = "xinclude")
                && (arg == "-nofixup-base-uris" || arg == "--nofixup-base-uris")
            {
                XINCLUDE += 1;
                OPTIONS |= XmlParserOption::XmlParseXinclude as i32;
                OPTIONS |= XmlParserOption::XmlParseNobasefix as i32;
            } else if cfg!(feature = "output")
                // && cfg!(feature = "libxml_zlib")
                && (arg == "-compress" || arg == "--compress")
            {
                COMPRESS += 1;
                set_compress_mode(9);
            } else if arg == "-nowarning" || arg == "--nowarning" {
                OPTIONS |= XmlParserOption::XmlParseNowarning as i32;
                OPTIONS &= !(XmlParserOption::XmlParsePedantic as i32);
            } else if arg == "-pedantic" || arg == "--pedantic" {
                OPTIONS |= XmlParserOption::XmlParsePedantic as i32;
                OPTIONS &= !(XmlParserOption::XmlParseNowarning as i32);
            } else if cfg!(feature = "libxml_debug") && (arg == "-debugent" || arg == "--debugent")
            {
                DEBUGENT += 1;
                set_parser_debug_entities(1);
            } else if cfg!(feature = "libxml_c14n") && (arg == "-c14n" || arg == "--c14n") {
                CANONICAL += 1;
                OPTIONS |= XmlParserOption::XmlParseNoent as i32
                    | XmlParserOption::XmlParseDtdattr as i32
                    | XmlParserOption::XmlParseDtdload as i32;
            } else if cfg!(feature = "libxml_c14n") && (arg == "-c14n11" || arg == "--c14n11") {
                CANONICAL_11 += 1;
                OPTIONS |= XmlParserOption::XmlParseNoent as i32
                    | XmlParserOption::XmlParseDtdattr as i32
                    | XmlParserOption::XmlParseDtdload as i32;
            } else if cfg!(feature = "libxml_c14n") && (arg == "-exc-c14n" || arg == "--exc-c14n") {
                EXC_CANONICAL += 1;
                OPTIONS |= XmlParserOption::XmlParseNoent as i32
                    | XmlParserOption::XmlParseDtdattr as i32
                    | XmlParserOption::XmlParseDtdload as i32;
            } else if cfg!(feature = "catalog") && (arg == "-catalogs" || arg == "--catalogs") {
                CATALOGS += 1;
            } else if cfg!(feature = "catalog") && (arg == "-nocatalogs" || arg == "--nocatalogs") {
                NOCATALOGS += 1;
            } else if arg == "-encode" || arg == "--encode" {
                *ENCODING.lock().unwrap() =
                    Some(args.next().expect("--encode: Encoding is not specified"));
                /*
                 * OK it's for testing purposes
                 */
                add_encoding_alias("UTF-8", "DVEnc");
            } else if arg == "-noblanks" || arg == "--noblanks" {
                NOBLANKS = 1;
            } else if arg == "-format" || arg == "--format" {
                #[cfg(feature = "output")]
                {
                    FORMAT = 1;
                }
            } else if arg == "-pretty" || arg == "--pretty" {
                if let Some(next) = args.next() {
                    #[cfg(feature = "output")]
                    {
                        FORMAT = next.parse().expect("--pretty: Failed to parse integer");
                    }
                }
            } else if cfg!(feature = "libxml_reader") && (arg == "-stream" || arg == "--stream") {
                STREAM += 1;
            } else if cfg!(feature = "libxml_reader") && (arg == "-walker" || arg == "--walker") {
                WALKER += 1;
                NOOUT += 1;
            } else if cfg!(feature = "libxml_reader")
                && cfg!(feature = "libxml_pattern")
                && (arg == "-pattern" || arg == "--pattern")
            {
                *PATTERN.lock().unwrap() =
                    CString::new(args.next().expect("--pattern: Pattern is not specified")).ok();
            } else if cfg!(feature = "sax1") && (arg == "-sax1" || arg == "--sax1") {
                SAX1 += 1;
                OPTIONS |= XmlParserOption::XmlParseSax1 as i32;
            } else if arg == "-sax" || arg == "--sax" {
                SAX += 1;
            } else if arg == "-chkregister" || arg == "--chkregister" {
                CHKREGISTER += 1;
            } else if cfg!(feature = "schema") && (arg == "-relaxng" || arg == "--relaxng") {
                *RELAXNG.lock().unwrap() =
                    CString::new(args.next().expect("--relaxng: RelaxNG is not specified")).ok();
                NOENT = 1;
            } else if cfg!(feature = "schema") && (arg == "-schema" || arg == "--schema") {
                *SCHEMA.lock().unwrap() =
                    CString::new(args.next().expect("--schema: Schema is not specified")).ok();
                NOENT = 1;
            } else if cfg!(feature = "libxml_schematron")
                && (arg == "-schematron" || arg == "--schematron")
            {
                *SCHEMATRON.lock().unwrap() = CString::new(
                    args.next()
                        .expect("--schematron: Schematron is not specified"),
                )
                .ok();
                NOENT = 1;
            } else if arg == "-nonet" || arg == "--nonet" {
                OPTIONS |= XmlParserOption::XmlParseNonet as i32;
                xml_set_external_entity_loader(xml_no_net_external_entity_loader);
            } else if arg == "-nocompact" || arg == "--nocompact" {
                OPTIONS &= !(XmlParserOption::XmlParseCompact as i32);
            } else if arg == "-load-trace" || arg == "--load-trace" {
                LOAD_TRACE += 1;
            } else if arg == "-path" || arg == "--path" {
                parse_path(&args.next().expect("--path: Path is not specified"));
            } else if cfg!(feature = "xpath") && (arg == "-xpath" || arg == "--xpath") {
                NOOUT += 1;
                *XPATHQUERY.lock().unwrap() =
                    CString::new(args.next().expect("--xpath: XPath is not specified")).ok();
            } else if arg == "-oldxml10" || arg == "--oldxml10" {
                OLDXML10 += 1;
                OPTIONS |= XmlParserOption::XmlParseOld10 as i32;
            } else {
                eprintln!("Unknown option {}", arg);
                usage(&mut stderr(), &program_name);
                exit(XmllintReturnCode::ErrUnclass as i32);
            }
        }
        if MAXMEM != 0 {
            xml_mem_setup(
                Some(my_free_func),
                Some(my_malloc_func),
                Some(my_realloc_func),
                Some(my_strdup_func),
            );
        }

        #[cfg(feature = "catalog")]
        if NOCATALOGS == 0 && CATALOGS != 0 {
            if let Some(catal) = option_env!("SGML_CATALOG_FILES") {
                let catal = CString::new(catal).expect("Failed to construct Catalog path");
                xml_load_catalogs(catal.as_ptr());
            } else {
                eprintln!("Variable $SGML_CATALOG_FILES not set");
            }
        }

        if CHKREGISTER != 0 {
            xml_register_node_default(Some(register_node));
            xml_deregister_node_default(deregister_node);
        }

        if let Some(indent) = option_env!("XMLLINT_INDENT") {
            set_tree_indent_string(indent.into());
        }

        DEFAULT_ENTITY_LOADER = Some(xml_get_external_entity_loader());
        xml_set_external_entity_loader(xmllint_external_entity_loader);

        if LOADDTD != 0 {
            let mut old = get_load_ext_dtd_default_value();
            old |= XML_DETECT_IDS as i32;
            set_load_ext_dtd_default_value(old);
        }
        if DTDATTRS != 0 {
            let mut old = get_load_ext_dtd_default_value();
            old |= XML_COMPLETE_ATTRS as i32;
            set_load_ext_dtd_default_value(old);
        }
        if NOENT != 0 {
            OPTIONS |= XmlParserOption::XmlParseNoent as i32;
        }
        if NOBLANKS != 0 || FORMAT == 1 {
            OPTIONS |= XmlParserOption::XmlParseNoblanks as i32;
        }
        if HTMLOUT != 0 && NOWRAP == 0 {
            let program_name =
                CString::new(program_name.as_bytes()).expect("Failed to construct program name");
            generic_error!("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\"\n");
            generic_error!("\t\"http://www.w3.org/TR/REC-html40/loose.dtd\">\n");
            generic_error!(
                "<html><head><title>{} output</title></head>\n",
                program_name.to_string_lossy()
            );
            generic_error!(
                "<body bgcolor=\"#ffffff\"><h1 align=\"center\">{} output</h1>\n",
                program_name.to_string_lossy()
            );
        }

        #[cfg(feature = "libxml_reader")]
        let not_stream = STREAM == 0;
        #[cfg(not(feature = "libxml_reader"))]
        let not_stream = true;
        #[cfg(feature = "libxml_schematron")]
        if SAX == 0 && not_stream {
            let mut schematron = SCHEMATRON.lock().unwrap();
            if let Some(s) = schematron.as_ref() {
                /* forces loading the DTDs */
                let load_ext = get_load_ext_dtd_default_value() | 1;
                set_load_ext_dtd_default_value(load_ext);
                OPTIONS |= XmlParserOption::XmlParseDtdload as i32;
                if TIMING != 0 {
                    start_timer();
                }
                let ctxt: XmlSchematronParserCtxtPtr = xml_schematron_new_parser_ctxt(s.as_ptr());
                if ctxt.is_null() {
                    PROGRESULT = XmllintReturnCode::ErrMem;
                    // goto error;
                    xml_cleanup_parser();
                    xml_memory_dump();
                    exit(PROGRESULT as i32);
                }
                WXSCHEMATRON.store(xml_schematron_parse(ctxt), Ordering::Relaxed);
                if WXSCHEMATRON.load(Ordering::Relaxed).is_null() {
                    generic_error!(
                        "Schematron schema {} failed to compile\n",
                        s.to_string_lossy()
                    );
                    PROGRESULT = XmllintReturnCode::ErrSchemacomp;
                    *schematron = None;
                }
                xml_schematron_free_parser_ctxt(ctxt);
                if TIMING != 0 {
                    end_timer!("Compiling the schemas");
                }
            }
        }
        #[cfg(feature = "schema")]
        if RELAXNG.lock().unwrap().is_some() && SAX == 0 && not_stream {
            let mut relaxng = RELAXNG.lock().unwrap();
            if let Some(r) = relaxng.as_ref() {
                /* forces loading the DTDs */
                let load_ext = get_load_ext_dtd_default_value() | 1;
                set_load_ext_dtd_default_value(load_ext);
                OPTIONS |= XmlParserOption::XmlParseDtdload as i32;
                if TIMING != 0 {
                    start_timer();
                }
                let ctxt: XmlRelaxNGParserCtxtPtr = xml_relaxng_new_parser_ctxt(r.as_ptr());
                if ctxt.is_null() {
                    PROGRESULT = XmllintReturnCode::ErrMem;
                    // goto error;
                    xml_cleanup_parser();
                    xml_memory_dump();
                    exit(PROGRESULT as i32);
                }
                xml_relaxng_set_parser_errors(
                    ctxt,
                    Some(generic_error_default),
                    Some(generic_error_default),
                    None,
                );
                RELAXNGSCHEMAS.store(xml_relaxng_parse(ctxt), Ordering::Relaxed);
                if RELAXNGSCHEMAS.load(Ordering::Relaxed).is_null() {
                    generic_error!(
                        "Relax-NG schema {} failed to compile\n",
                        r.to_string_lossy()
                    );
                    PROGRESULT = XmllintReturnCode::ErrSchemacomp;
                    *relaxng = None;
                }
                xml_relaxng_free_parser_ctxt(ctxt);
                if TIMING != 0 {
                    end_timer!("Compiling the schemas");
                }
            }
        } else if SCHEMA.lock().unwrap().is_some() && not_stream {
            let mut schema = SCHEMA.lock().unwrap();
            if let Some(s) = schema.as_ref() {
                if TIMING != 0 {
                    start_timer();
                }
                let ctxt: XmlSchemaParserCtxtPtr = xml_schema_new_parser_ctxt(s.as_ptr());
                if ctxt.is_null() {
                    PROGRESULT = XmllintReturnCode::ErrMem;
                    // goto error;
                    xml_cleanup_parser();
                    xml_memory_dump();
                    exit(PROGRESULT as i32);
                }
                xml_schema_set_parser_errors(
                    ctxt,
                    Some(generic_error_default),
                    Some(generic_error_default),
                    None,
                );
                let wxschemas = xml_schema_parse(ctxt);
                if wxschemas.is_null() {
                    generic_error!("WXS schema {} failed to compile\n", s.to_string_lossy());
                    PROGRESULT = XmllintReturnCode::ErrSchemacomp;
                    *schema = None;
                }
                WXSCHEMAS.store(wxschemas, Ordering::Relaxed);
                xml_schema_free_parser_ctxt(ctxt);
                if TIMING != 0 {
                    end_timer!("Compiling the schemas");
                }
            }
        }

        #[cfg(all(feature = "libxml_reader", feature = "libxml_pattern"))]
        if PATTERN.lock().unwrap().is_some() && WALKER == 0 {
            let mut pattern = PATTERN.lock().unwrap();
            if let Some(p) = pattern.as_ref() {
                PATTERNC.store(
                    xml_patterncompile(p.as_ptr() as _, null_mut(), 0, null_mut()),
                    Ordering::Relaxed,
                );
                if PATTERNC.load(Ordering::Relaxed).is_null() {
                    generic_error!("Pattern {} failed to compile\n", p.to_string_lossy());
                    PROGRESULT = XmllintReturnCode::ErrSchemapat;
                    *pattern = None;
                }
            }
        }

        for arg in remained {
            if TIMING != 0 && REPEAT != 0 {
                start_timer();
            }
            /* Remember file names.  "-" means stdin.  <sven@zen.org> */
            if !arg.starts_with('-') || arg == "-" {
                if REPEAT != 0 {
                    let mut ctxt: XmlParserCtxtPtr = null_mut();

                    for _ in 0..REPEAT {
                        #[cfg(feature = "libxml_reader")]
                        {
                            let carg =
                                CString::new(arg.as_str()).expect("Failed to construct argument");
                            if STREAM != 0 {
                                stream_file(carg.as_ptr() as _);
                            } else if SAX != 0 {
                                test_sax(carg.as_ptr());
                            } else {
                                if ctxt.is_null() {
                                    ctxt = xml_new_parser_ctxt();
                                }
                                parse_and_print_file(Some(&arg), ctxt);
                            }
                        }
                        #[cfg(not(feature = "libxml_reader"))]
                        {
                            if SAX != 0 {
                                test_sax(arg.as_ptr() as _);
                            } else {
                                if ctxt.is_null() {
                                    ctxt = xml_new_parser_ctxt();
                                }
                                parse_and_print_file(arg.as_ptr() as _, ctxt);
                            }
                        }
                    }
                    if !ctxt.is_null() {
                        xml_free_parser_ctxt(ctxt);
                    }
                } else {
                    NBREGISTER = 0;
                    let carg = CString::new(arg.as_str()).expect("Failed to construct argument");

                    if cfg!(feature = "libxml_reader") && STREAM != 0 {
                        #[cfg(feature = "libxml_reader")]
                        {
                            stream_file(carg.as_ptr() as _);
                        }
                    } else if SAX != 0 {
                        test_sax(carg.as_ptr() as _);
                    } else {
                        parse_and_print_file(Some(&arg), null_mut());
                    }

                    if CHKREGISTER != 0 && NBREGISTER != 0 {
                        eprintln!("Registration count off: {}", NBREGISTER);
                        PROGRESULT = XmllintReturnCode::ErrRdregis;
                    }
                }
                files += 1;
                if TIMING != 0 && REPEAT != 0 {
                    end_timer!("{} iterations", REPEAT);
                }
            }
        }
        if GENERATE != 0 {
            parse_and_print_file(None, null_mut());
        }
        if HTMLOUT != 0 && NOWRAP == 0 {
            generic_error!("</body></html>\n");
        }
        if files == 0 && GENERATE == 0 && version == 0 {
            usage(&mut stderr(), &program_name);
            PROGRESULT = XmllintReturnCode::ErrUnclass;
        }
        #[cfg(feature = "libxml_schematron")]
        {
            let wxschematron = WXSCHEMATRON.load(Ordering::Relaxed);
            if !wxschematron.is_null() {
                xml_schematron_free(wxschematron);
            }
        }
        #[cfg(feature = "schema")]
        {
            let relaxngschemas = RELAXNGSCHEMAS.load(Ordering::Relaxed);
            if !relaxngschemas.is_null() {
                xml_relaxng_free(relaxngschemas);
            }
        }
        #[cfg(feature = "schema")]
        {
            let wxschemas = WXSCHEMAS.load(Ordering::Relaxed);
            if !wxschemas.is_null() {
                xml_schema_free(wxschemas);
            }
        }
        #[cfg(all(feature = "libxml_reader", feature = "libxml_pattern"))]
        {
            if !PATTERNC.load(Ordering::Relaxed).is_null() {
                xml_free_pattern(PATTERNC.load(Ordering::Relaxed));
            }
        }

        xml_cleanup_parser();
        xml_memory_dump();

        exit(PROGRESULT as i32)
    }
}
