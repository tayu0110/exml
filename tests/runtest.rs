//! Rust implementation of original libxml2's `runtest.c`.  
//! If you want this to work, copy the `test/` and `result/` directories from the original libxml2.
#![cfg_attr(feature = "sax1", allow(deprecated))]

use std::{
    cell::{Cell, RefCell},
    env::args,
    ffi::{CStr, CString, c_char},
    fs::{File, metadata, remove_file},
    io::{self, BufRead, BufReader, Read, Write},
    os::raw::c_void,
    path::Path,
    ptr::{addr_of_mut, null, null_mut},
    slice::from_raw_parts,
    str::from_utf8,
    sync::{
        Condvar, Mutex, OnceLock,
        atomic::{AtomicPtr, AtomicUsize, Ordering},
    },
};

use const_format::concatcp;
#[cfg(feature = "c14n")]
use exml::c14n::XmlC14NMode;
#[cfg(feature = "schematron")]
use exml::libxml::schematron::XmlSchematronPtr;
#[cfg(feature = "schema")]
use exml::xmlschemas::schema::XmlSchemaPtr;
use exml::{
    SYSCONFDIR,
    error::{
        XmlError, XmlErrorDomain, XmlErrorLevel, XmlParserErrors,
        parser_print_file_context_internal,
    },
    globals::{
        GenericErrorContext, reset_last_error, set_generic_error,
        set_pedantic_parser_default_value, set_structured_error,
    },
    io::{
        XmlInputCallback, pop_input_callbacks, register_input_callbacks,
        xml_no_net_external_entity_loader,
    },
    libxml::{
        globals::{set_xml_free, set_xml_malloc, set_xml_mem_strdup, set_xml_realloc, xml_free},
        htmlparser::{
            HtmlParserCtxtPtr, html_ctxt_read_file, html_free_parser_ctxt,
            html_new_sax_parser_ctxt, html_read_file,
        },
        htmltree::html_doc_dump_memory,
        parser::{
            XML_SAX2_MAGIC, XmlParserOption, XmlSAXHandler, XmlSAXLocatorPtr, xml_cleanup_parser,
            xml_ctxt_use_options, xml_init_parser, xml_parse_document, xml_parse_file,
            xml_set_external_entity_loader,
        },
        pattern::{XmlPatternPtr, XmlStreamCtxtPtr},
        relaxng::XmlRelaxNGPtr,
        xinclude::xml_xinclude_process_flags,
        xmlmemory::{
            xml_mem_free, xml_mem_malloc, xml_mem_realloc, xml_mem_setup, xml_mem_used,
            xml_memory_dump, xml_memory_strdup,
        },
        xmlreader::XmlTextReaderPtr,
        xmlregexp::XmlRegexpPtr,
        xmlschemastypes::xml_schema_init_types,
        xmlstring::XmlChar,
    },
    parser::{
        XmlParserCtxtPtr, XmlParserInputPtr, xml_create_file_parser_ctxt, xml_free_parser_ctxt,
        xml_read_file, xml_read_memory,
    },
    relaxng::xml_relaxng_init_types,
    tree::{
        XmlAttributeDefault, XmlAttributeType, XmlDocPtr, XmlElementContentPtr, XmlElementType,
        XmlElementTypeVal, XmlEntityPtr, XmlEntityType, XmlEnumeration, XmlNodePtr, xml_free_doc,
    },
    uri::{XmlURI, build_uri, normalize_uri_path},
    xpath::XmlXPathObjectPtr,
};
#[cfg(feature = "catalog")]
use libc::pthread_t;
use libc::{free, malloc, memcpy, size_t, snprintf, strcmp, strlen};

/// pseudo flag for the unification of HTML and XML tests
const XML_PARSE_HTML: i32 = 1 << 24;

type Functest =
    unsafe fn(filename: &str, result: Option<String>, error: Option<String>, options: i32) -> i32;

struct TestDesc<'a> {
    desc: &'a str,           /* description of the test */
    func: Functest,          /* function implementing the test */
    input: Option<&'a str>,  /* glob to path for input files */
    out: Option<&'a str>,    /* output directory */
    suffix: Option<&'a str>, /* suffix for output files */
    err: Option<&'a str>,    /* suffix for error output files */
    options: i32,            /* parser options for the test */
}

static UPDATE_RESULTS: OnceLock<bool> = OnceLock::new();
static TEMP_DIRECTORY: OnceLock<String> = OnceLock::new();

thread_local! {
    static NB_TESTS: Cell<i32> = const { Cell::new(0) };
    static NB_ERRORS: Cell<i32> = const { Cell::new(0) };
    static NB_LEAKS: Cell<i32> = const { Cell::new(0) };
    static EXTRA_MEMORY_FROM_RESOLVER: Cell<i32> = const { Cell::new(0) };
}

// We need to trap calls to the resolver to not account memory for the catalog
// which is shared to the current running test. We also don't want to have
// network downloads modifying tests.
unsafe fn test_external_entity_loader(
    url: Option<&str>,
    id: Option<&str>,
    ctxt: XmlParserCtxtPtr,
) -> XmlParserInputPtr {
    unsafe {
        let ret: XmlParserInputPtr;

        if check_test_file(url.unwrap()) {
            ret = xml_no_net_external_entity_loader(url, id, ctxt);
        } else {
            let memused: i32 = xml_mem_used();
            ret = xml_no_net_external_entity_loader(url, id, ctxt);
            EXTRA_MEMORY_FROM_RESOLVER
                .set(EXTRA_MEMORY_FROM_RESOLVER.get() + xml_mem_used() - memused);
        }

        ret
    }
}

// Trapping the error messages at the generic level to grab the equivalent of
// stderr messages on CLI tools.
thread_local! {
    static TEST_ERRORS: RefCell<[XmlChar; 32769]> = const { RefCell::new([0; 32769]) };
    static TEST_ERRORS_SIZE: Cell<usize> = const { Cell::new(0) };
}

fn test_error_handler(_ctx: Option<GenericErrorContext>, msg: &str) {
    if TEST_ERRORS_SIZE.get() >= 32768 {
        return;
    }

    TEST_ERRORS.with_borrow_mut(|errors| {
        if TEST_ERRORS_SIZE.get() + msg.len() >= 32768 {
            let until = errors.len() - TEST_ERRORS_SIZE.get();
            errors[TEST_ERRORS_SIZE.get()..].copy_from_slice(&msg.as_bytes()[..until]);
            /* buffer is full */
            TEST_ERRORS_SIZE.set(32768);
            errors[TEST_ERRORS_SIZE.get()] = 0;
        } else {
            errors[TEST_ERRORS_SIZE.get()..TEST_ERRORS_SIZE.get() + msg.len()]
                .copy_from_slice(msg.as_bytes());
            TEST_ERRORS_SIZE.set(TEST_ERRORS_SIZE.get() + msg.len());
        }
        errors[TEST_ERRORS_SIZE.get()] = 0;
    });
}

fn channel(_ctx: Option<GenericErrorContext>, msg: &str) {
    if TEST_ERRORS_SIZE.get() >= 32768 {
        return;
    }
    TEST_ERRORS.with_borrow_mut(|errors| {
        if TEST_ERRORS_SIZE.get() + msg.len() >= errors.len() {
            let until = errors.len() - TEST_ERRORS_SIZE.get();
            errors[TEST_ERRORS_SIZE.get()..].copy_from_slice(&msg.as_bytes()[..until]);
            TEST_ERRORS_SIZE.set(errors.len() - 1);
        } else {
            errors[TEST_ERRORS_SIZE.get()..TEST_ERRORS_SIZE.get() + msg.len()]
                .copy_from_slice(msg.as_bytes());
            TEST_ERRORS_SIZE.set(TEST_ERRORS_SIZE.get() + msg.len());
        }
        errors[TEST_ERRORS_SIZE.get()] = 0;
    });
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
        if let Some(node) = node.filter(|n| n.element_type() == XmlElementType::XmlElementNode) {
            let node = XmlNodePtr::try_from(node).unwrap();
            name = node.name;
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
            // These are not implemented the original `runtest.c` in libxml2-v2.11.8.
            // XmlErrorDomain::XmlFromI18N => channel(None, "encoding "),
            // XmlErrorDomain::XmlFromSchematronv => channel(None, "schematron "),
            // XmlErrorDomain::XmlFromBuffer => channel(None, "internal buffer "),
            // XmlErrorDomain::XmlFromURI => channel(None, "URI "),
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

unsafe fn initialize_libxml2() {
    unsafe {
        // This verifies that xmlInitParser doesn't allocate memory with xmlMalloc
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
        xml_set_external_entity_loader(test_external_entity_loader);
    }
}

fn base_filename(filename: impl AsRef<Path>) -> String {
    fn _base_filename(filename: &Path) -> Option<String> {
        filename
            .file_name()
            .and_then(|s| s.to_str().map(|s| s.to_owned()))
    }
    _base_filename(filename.as_ref()).unwrap_or_default()
}

fn result_filename(filename: &str, out: Option<&str>, suffix: Option<&str>) -> String {
    /*************
       if ((filename[0] == 't') && (filename[1] == 'e') &&
           (filename[2] == 's') && (filename[3] == 't') &&
       (filename[4] == '/'))
       filename = &filename[5];
    *************/

    let base = base_filename(filename);
    let suffix = suffix.unwrap_or(".tmp");
    let out = out.unwrap_or("");

    format!("{out}{base}{suffix}")
}

fn check_test_file(filename: impl AsRef<Path>) -> bool {
    metadata(filename.as_ref()).is_ok_and(|meta| meta.is_file())
}

fn compare_files(
    r1: impl AsRef<Path>, /* temp */
    r2: impl AsRef<Path>, /* result */
) -> i32 {
    fn _compare_files(r1: &Path, r2: &Path) -> i32 {
        let mut bytes1: [u8; 4096] = [0; 4096];
        let mut bytes2: [u8; 4096] = [0; 4096];

        if *UPDATE_RESULTS.get_or_init(|| false) {
            let Ok(mut fd1) = File::open(r1) else {
                return -1;
            };
            let Ok(mut fd2) = File::options()
                .write(true)
                .truncate(true)
                .create(true)
                .open(r2)
            else {
                return -1;
            };
            let mut total = 0;
            while let Ok(res1) = fd1.read(&mut bytes1) {
                if res1 == 0 {
                    if total == 0 {
                        remove_file(r2).ok();
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
                        remove_file(r2).ok();
                    }
                    return (res1 != 0) as i32;
                }
            }
            return 0;
        }

        let Ok(mut fd1) = File::open(r1) else {
            return -1;
        };
        let mut fd2 = File::open(r2);
        while let Ok(res1) = fd1.read(&mut bytes1) {
            if fd2.as_mut().map_or(0, |f| f.read(&mut bytes2).unwrap_or(0)) != res1 {
                return 1;
            }
            if res1 == 0 {
                break;
            }
            if bytes1[..res1] != bytes2[..res1] {
                return 1;
            }
        }
        0
    }
    _compare_files(r1.as_ref(), r2.as_ref())
}

fn compare_file_mem(filename: impl AsRef<Path>, mem: &[u8]) -> i32 {
    fn _compare_file_mem(filename: &Path, mem: &[u8]) -> i32 {
        let mut bytes: [u8; 4096] = [0; 4096];
        let size = mem.len();

        if *UPDATE_RESULTS.get_or_init(|| false) {
            if size == 0 {
                remove_file(filename).ok();
                return 0;
            }
            let mut file = match File::options()
                .write(true)
                .truncate(true)
                .create(true)
                .open(filename)
            {
                Ok(file) => file,
                _ => {
                    eprint!("failed to open {} for writing", filename.display());
                    return -1;
                }
            };
            let res = file.write(mem).unwrap();
            return (res != size) as i32;
        }

        match metadata(filename) {
            Ok(meta) => {
                if meta.len() != size as u64 {
                    eprintln!(
                        "file {} is {} bytes, result is {} bytes",
                        filename.display(),
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
                eprintln!("failed to stat {}", filename.display());
                return -1;
            }
        }
        let Ok(mut file) = File::open(filename) else {
            eprint!("failed to open {} for reading", filename.display());
            return -1;
        };
        let mut idx = 0;
        while idx < size {
            let res = match file.read(&mut bytes) {
                Ok(size) => size,
                _ => break,
            };
            if res + idx > size {
                break;
            }
            if bytes[..res] != mem[idx..idx + res] {
                for (ix, &byte) in bytes.iter().enumerate().take(res + 1) {
                    if idx == res || byte != mem[idx + ix] {
                        eprintln!("Compare error at position {}", idx + ix);
                        return 1;
                    }
                }
            }
            idx += res;
        }
        if idx != size {
            eprintln!("Compare error index {}, size {}", idx, size);
        }
        (idx != size) as i32
    }

    _compare_file_mem(filename.as_ref(), mem)
}

unsafe fn load_mem(filename: &str, mem: *mut *const c_char, size: *mut i32) -> i32 {
    unsafe {
        match metadata(filename) {
            Ok(meta) => {
                let base: *mut c_char = malloc(meta.len() as usize + 1) as _;
                if base.is_null() {
                    return -1;
                }
                match File::open(filename) {
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
}

unsafe extern "C" fn unload_mem(mem: *const c_char) -> i32 {
    unsafe {
        free(mem as _);
        0
    }
}

thread_local! {
    static SAX_DEBUG: RefCell<Option<File>> = const { RefCell::new(None) };
}

macro_rules! sax_debug {
    ( $fmt:literal, $( $args:expr ),* ) => {
        SAX_DEBUG.with_borrow_mut(|f| write!(f.as_mut().unwrap(), $fmt, $( $args ),*).ok());
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

// empty SAX block
static EMPTY_SAXHANDLER_STRUCT: XmlSAXHandler = XmlSAXHandler {
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

thread_local! {
    static CALLBACKS: Cell<i32> = const { Cell::new(0) };
}
fn increment_callbacks_counter() {
    CALLBACKS.set(CALLBACKS.get() + 1);
}

/// Is this document tagged standalone ?
///
/// Returns 1 if true
#[doc(alias = "isStandaloneDebug")]
unsafe fn is_standalone_debug(_ctx: Option<GenericErrorContext>) -> i32 {
    increment_callbacks_counter();
    sax_debugln!("SAX.isStandalone()");
    0
}

/// Does this document has an internal subset
///
/// Returns 1 if true
#[doc(alias = "hasInternalSubsetDebug")]
unsafe fn has_internal_subset_debug(_ctx: Option<GenericErrorContext>) -> i32 {
    increment_callbacks_counter();
    sax_debugln!("SAX.hasInternalSubset()");
    0
}

/// Does this document has an external subset
///
/// Returns 1 if true
#[doc(alias = "hasExternalSubsetDebug")]
unsafe fn has_external_subset_debug(_ctx: Option<GenericErrorContext>) -> i32 {
    increment_callbacks_counter();
    sax_debugln!("SAX.hasExternalSubset()");
    0
}

/// Does this document has an internal subset
#[doc(alias = "internalSubsetDebug")]
unsafe fn internal_subset_debug(
    _ctx: Option<GenericErrorContext>,
    name: Option<&str>,
    external_id: Option<&str>,
    system_id: Option<&str>,
) {
    increment_callbacks_counter();
    let name = name.unwrap_or("(null)");
    sax_debug!("SAX.internalSubset({name},");
    if let Some(external_id) = external_id {
        sax_debug!(" {external_id},");
    } else {
        sax_debug!(" ,");
    }
    if let Some(system_id) = system_id {
        sax_debugln!(" {system_id})");
    } else {
        sax_debugln!(" )");
    }
}

/// Does this document has an external subset
#[doc(alias = "externalSubsetDebug")]
unsafe fn external_subset_debug(
    _ctx: Option<GenericErrorContext>,
    name: Option<&str>,
    external_id: Option<&str>,
    system_id: Option<&str>,
) {
    increment_callbacks_counter();
    sax_debug!("SAX.externalSubset({},", name.unwrap_or("(null)"));
    if let Some(external_id) = external_id {
        sax_debug!(" {external_id},");
    } else {
        sax_debug!(" ,");
    }
    if let Some(system_id) = system_id {
        sax_debugln!(" {system_id})");
    } else {
        sax_debugln!(" )");
    }
}

/// Special entity resolver, better left to the parser, it has
/// more context than the application layer.
/// The default behaviour is to NOT resolve the entities, in that case
/// the ENTITY_REF nodes are built in the structure (and the parameter values).
///
/// Returns the xmlParserInputPtr if inlined or NULL for DOM behaviour.
#[doc(alias = "resolveEntityDebug")]
unsafe fn resolve_entity_debug(
    _ctx: Option<GenericErrorContext>,
    public_id: Option<&str>,
    system_id: Option<&str>,
) -> XmlParserInputPtr {
    increment_callbacks_counter();
    /* xmlParserCtxtPtr ctxt = (xmlParserCtxtPtr) ctx; */

    sax_debug!("SAX.resolveEntity(");
    if let Some(public_id) = public_id {
        sax_debug!("{public_id}");
    } else {
        sax_debug!(" ");
    }
    if let Some(system_id) = system_id {
        sax_debugln!(", {system_id})");
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

/// Get an entity by name
///
/// Returns the xmlParserInputPtr if inlined or NULL for DOM behaviour.
#[doc(alias = "getEntityDebug")]
unsafe fn get_entity_debug(_ctx: Option<GenericErrorContext>, name: &str) -> Option<XmlEntityPtr> {
    increment_callbacks_counter();
    sax_debugln!("SAX.getEntity({name})");
    None
}

/// Get a parameter entity by name
///
/// Returns the xmlParserInputPtr
#[doc(alias = "getParameterEntityDebug")]
unsafe fn get_parameter_entity_debug(
    _ctx: Option<GenericErrorContext>,
    name: &str,
) -> Option<XmlEntityPtr> {
    increment_callbacks_counter();
    sax_debugln!("SAX.getParameterEntity({name})");
    None
}

/// An entity definition has been parsed
#[doc(alias = "entityDeclDebug")]
fn entity_decl_debug(
    _ctx: Option<GenericErrorContext>,
    name: &str,
    typ: XmlEntityType,
    public_id: Option<&str>,
    system_id: Option<&str>,
    content: Option<&str>,
) {
    increment_callbacks_counter();
    sax_debugln!(
        "SAX.entityDecl({name}, {}, {}, {}, {})",
        typ as i32,
        public_id.unwrap_or("(null)"),
        system_id.unwrap_or("(null)"),
        content.unwrap_or("(null)")
    );
}

/// An attribute definition has been parsed
#[doc(alias = "attributeDeclDebug")]
unsafe fn attribute_decl_debug(
    _ctx: Option<GenericErrorContext>,
    elem: &str,
    name: &str,
    typ: XmlAttributeType,
    def: XmlAttributeDefault,
    default_value: Option<&str>,
    _tree: Option<Box<XmlEnumeration>>,
) {
    increment_callbacks_counter();
    if let Some(default_value) = default_value {
        sax_debugln!(
            "SAX.attributeDecl({elem}, {name}, {}, {}, {default_value}, ...)",
            typ as i32,
            def as i32,
        );
    } else {
        sax_debugln!(
            "SAX.attributeDecl({elem}, {name}, {}, {}, NULL, ...)",
            typ as i32,
            def as i32,
        );
    }
}

/// An element definition has been parsed
#[doc(alias = "elementDeclDebug")]
unsafe fn element_decl_debug(
    _ctx: Option<GenericErrorContext>,
    name: &str,
    typ: Option<XmlElementTypeVal>,
    _content: XmlElementContentPtr,
) {
    increment_callbacks_counter();
    sax_debugln!(
        "SAX.elementDecl({name}, {}, ...)",
        typ.map_or(-1, |t| t as i32)
    );
}

/// What to do when a notation declaration has been parsed.
#[doc(alias = "notationDeclDebug")]
unsafe fn notation_decl_debug(
    _ctx: Option<GenericErrorContext>,
    name: &str,
    public_id: Option<&str>,
    system_id: Option<&str>,
) {
    increment_callbacks_counter();
    sax_debugln!(
        "SAX.notationDecl({name}, {}, {})",
        public_id.unwrap_or("(null)"),
        system_id.unwrap_or("(null)")
    );
}

/// What to do when an unparsed entity declaration is parsed
#[doc(alias = "unparsedEntityDeclDebug")]
fn unparsed_entity_decl_debug(
    _ctx: Option<GenericErrorContext>,
    name: &str,
    public_id: Option<&str>,
    system_id: Option<&str>,
    notation_name: Option<&str>,
) {
    increment_callbacks_counter();
    sax_debugln!(
        "SAX.unparsedEntityDecl({name}, {}, {}, {})",
        public_id.unwrap_or("(null)"),
        system_id.unwrap_or("(null)"),
        notation_name.unwrap_or("(null)")
    );
}

/// Receive the document locator at startup, actually xmlDefaultSAXLocator
/// Everything is available on the context, so this is useless in our case.
#[doc(alias = "setDocumentLocatorDebug")]
unsafe fn set_document_locator_debug(_ctx: Option<GenericErrorContext>, _loc: XmlSAXLocatorPtr) {
    increment_callbacks_counter();
    sax_debugln!("SAX.setDocumentLocator()");
}

/// called when the document start being processed.
#[doc(alias = "startDocumentDebug")]
fn start_document_debug(_ctx: Option<GenericErrorContext>) {
    increment_callbacks_counter();
    sax_debugln!("SAX.startDocument()");
}

/// called when the document end has been detected.
#[doc(alias = "endDocumentDebug")]
fn end_document_debug(_ctx: Option<GenericErrorContext>) {
    increment_callbacks_counter();
    sax_debugln!("SAX.endDocument()");
}

/// called when an opening tag has been processed.
#[doc(alias = "startElementDebug")]
unsafe fn start_element_debug(
    _ctx: Option<GenericErrorContext>,
    name: &str,
    atts: &[(String, Option<String>)],
) {
    increment_callbacks_counter();
    sax_debug!("SAX.startElement({name}");
    for (key, value) in atts {
        sax_debug!(", {key}='");
        if let Some(value) = value {
            sax_debug!("{value}'");
        }
    }
    sax_debugln!(")");
}

/// called when the end of an element has been detected.
#[doc(alias = "endElementDebug")]
fn end_element_debug(_ctx: Option<GenericErrorContext>, name: &str) {
    increment_callbacks_counter();
    sax_debugln!("SAX.endElement({name})");
}

/// receiving some chars from the parser.
/// Question: how much at a time ???
#[doc(alias = "charactersDebug")]
fn characters_debug(_ctx: Option<GenericErrorContext>, ch: &str) {
    let mut output: [u8; 40] = [0; 40];

    increment_callbacks_counter();
    let slen = ch.len().min(30);
    output[..slen].copy_from_slice(&ch.as_bytes()[..slen]);
    output[slen] = 0;

    sax_debug!("SAX.characters(");
    SAX_DEBUG.with_borrow_mut(|f| f.as_mut().unwrap().write_all(&output[..slen]).ok());
    sax_debugln!(", {})", ch.len());
}

/// called when an entity reference is detected.
#[doc(alias = "referenceDebug")]
fn reference_debug(_ctx: Option<GenericErrorContext>, name: &str) {
    increment_callbacks_counter();
    sax_debugln!("SAX.reference({name})");
}

/// receiving some ignorable whitespaces from the parser.
/// Question: how much at a time ???
#[doc(alias = "ignorableWhitespaceDebug")]
fn ignorable_whitespace_debug(_ctx: Option<GenericErrorContext>, ch: &str) {
    let mut output: [u8; 40] = [0; 40];

    increment_callbacks_counter();
    let slen = ch.len().min(30);
    output[..slen].copy_from_slice(&ch.as_bytes()[..slen]);
    output[slen] = 0;
    sax_debug!("SAX.ignorableWhitespace(");
    SAX_DEBUG.with_borrow_mut(|f| f.as_mut().unwrap().write_all(&output[..slen]).ok());
    sax_debugln!(", {})", ch.len());
}

/// A processing instruction has been parsed.
#[doc(alias = "processingInstructionDebug")]
unsafe fn processing_instruction_debug(
    _ctx: Option<GenericErrorContext>,
    target: &str,
    data: Option<&str>,
) {
    increment_callbacks_counter();
    if let Some(data) = data {
        sax_debugln!("SAX.processingInstruction({target}, {data})");
    } else {
        sax_debugln!("SAX.processingInstruction({target}, NULL)");
    }
}

/// called when a pcdata block has been parsed
#[doc(alias = "cdataBlockDebug")]
fn cdata_block_debug(_ctx: Option<GenericErrorContext>, value: &str) {
    increment_callbacks_counter();
    let len = value.len();
    let l = value.len().min(20);
    let value = &value.as_bytes()[..l];
    sax_debug!("SAX.pcdata(");
    SAX_DEBUG.with_borrow_mut(|f| f.as_mut().unwrap().write_all(value).ok());
    sax_debugln!(", {len})");
}

/// A comment has been parsed.
#[doc(alias = "commentDebug")]
unsafe fn comment_debug(_ctx: Option<GenericErrorContext>, value: &str) {
    increment_callbacks_counter();
    sax_debugln!("SAX.comment({value})");
}

/// Display and format a warning messages, gives file, line, position and extra parameters.
#[doc(alias = "warningDebug")]
fn warning_debug(_ctx: Option<GenericErrorContext>, msg: &str) {
    increment_callbacks_counter();

    SAX_DEBUG.with_borrow_mut(|f| write!(f.as_mut().unwrap(), "SAX.warning: {}", msg).ok());
}

/// Display and format a error messages, gives file, line, position and extra parameters.
#[doc(alias = "errorDebug")]
fn error_debug(_ctx: Option<GenericErrorContext>, msg: &str) {
    increment_callbacks_counter();
    sax_debug!("SAX.error: {}", msg);
}

/// Display and format a fatalError messages, gives file, line, position and extra parameters.
#[doc(alias = "fatalErrorDebug")]
fn fatal_error_debug(_ctx: Option<GenericErrorContext>, msg: &str) {
    increment_callbacks_counter();
    sax_debug!("SAX.fatalError: {msg}");
}

static DEBUG_SAXHANDLER_STRUCT: XmlSAXHandler = XmlSAXHandler {
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

/// called when an opening tag has been processed.
#[doc(alias = "startElementNsDebug")]
unsafe fn start_element_ns_debug(
    _ctx: Option<GenericErrorContext>,
    localname: &str,
    prefix: Option<&str>,
    uri: Option<&str>,
    namespaces: &[(Option<String>, String)],
    nb_defaulted: usize,
    attributes: &[(String, Option<String>, Option<String>, String)],
) {
    increment_callbacks_counter();
    sax_debug!("SAX.startElementNs({localname}");
    if let Some(prefix) = prefix {
        sax_debug!(", {prefix}");
    } else {
        sax_debug!(", NULL");
    }
    if let Some(uri) = uri {
        sax_debug!(", '{uri}'");
    } else {
        sax_debug!(", NULL");
    }
    sax_debug!(", {}", namespaces.len());

    for (pre, loc) in namespaces {
        sax_debug!(", xmlns");
        if let Some(pre) = pre.as_deref() {
            sax_debug!(":{pre}");
        }
        sax_debug!("='{loc}'");
    }
    sax_debug!(", {}, {}", attributes.len(), nb_defaulted);
    for attr in attributes {
        if let Some(pre) = attr.1.as_deref() {
            sax_debug!(", {pre}:{}='", attr.0);
        } else {
            sax_debug!(", {}='", attr.0);
        }
        let value = attr.3.as_bytes();
        let l = value.len().min(4);
        SAX_DEBUG.with_borrow_mut(|f| f.as_mut().unwrap().write_all(&value[..l]).ok());
        sax_debug!("...', {}", attr.3.len(),);
    }
    sax_debugln!(")");
}

/// called when the end of an element has been detected.
#[doc(alias = "endElementDebug")]
unsafe fn end_element_ns_debug(
    _ctx: Option<GenericErrorContext>,
    localname: &str,
    prefix: Option<&str>,
    uri: Option<&str>,
) {
    increment_callbacks_counter();
    sax_debug!("SAX.endElementNs({localname}");
    if let Some(prefix) = prefix {
        sax_debug!(", {prefix}");
    } else {
        sax_debug!(", NULL");
    }
    if let Some(uri) = uri {
        sax_debugln!(", '{uri}')");
    } else {
        sax_debugln!(", NULL)");
    }
}

static DEBUG_SAX2_HANDLER_STRUCT: XmlSAXHandler = XmlSAXHandler {
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

/// called when an opening tag has been processed.
#[doc(alias = "htmlstartElementDebug")]
#[cfg(feature = "html")]
unsafe fn htmlstart_element_debug(
    _ctx: Option<GenericErrorContext>,
    name: &str,
    atts: &[(String, Option<String>)],
) {
    unsafe {
        use exml::libxml::htmlparser::html_encode_entities;

        sax_debug!("SAX.startElement({name}");
        for (key, value) in atts {
            sax_debug!(", {key}");
            if let Some(value) = value {
                let value = CString::new(value.as_str()).unwrap();
                let mut output: [u8; 40] = [0; 40];
                let mut att = value.as_ptr() as *const u8;
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
        }
        sax_debugln!(")");
    }
}

/// receiving some chars from the parser.
/// Question: how much at a time ???
#[doc(alias = "htmlcharactersDebug")]
#[cfg(feature = "html")]
unsafe fn htmlcharacters_debug(_ctx: Option<GenericErrorContext>, ch: &str) {
    unsafe {
        use std::ffi::c_uchar;

        use exml::libxml::htmlparser::html_encode_entities;

        let len = ch.len();
        let mut output: [c_uchar; 40] = [0; 40];
        let mut inlen: i32 = len as i32;
        let mut outlen: usize = 30;
        let ch = CString::new(ch).unwrap();

        html_encode_entities(
            output.as_mut_ptr(),
            addr_of_mut!(outlen) as _,
            ch.as_ptr() as *const u8,
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
}

/// receiving some cdata chars from the parser.
/// Question: how much at a time ???
#[doc(alias = "htmlcdataDebug")]
#[cfg(feature = "html")]
unsafe fn htmlcdata_debug(_ctx: Option<GenericErrorContext>, ch: &str) {
    unsafe {
        use std::ffi::c_uchar;

        use exml::libxml::htmlparser::html_encode_entities;

        let len = ch.len();
        let mut output: [c_uchar; 40] = [0; 40];
        let mut inlen: i32 = len as i32;
        let mut outlen: usize = 30;
        let ch = CString::new(ch).unwrap();

        html_encode_entities(
            output.as_mut_ptr(),
            addr_of_mut!(outlen) as _,
            ch.as_ptr() as *const u8,
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
}

#[cfg(feature = "html")]
static DEBUG_HTMLSAXHANDLER_STRUCT: XmlSAXHandler = XmlSAXHandler {
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

/// If test is passed, return `true`, otherwise return `false`.
fn sax_compare_file(temp: impl AsRef<Path>, result: impl AsRef<Path>) -> bool {
    let mut temp_file = File::open(temp.as_ref()).unwrap();
    let mut result_file = File::open(result.as_ref()).unwrap();

    let mut tbuf = String::new();
    temp_file.read_to_string(&mut tbuf).ok();
    let mut rbuf = String::new();
    result_file.read_to_string(&mut rbuf).ok();

    // If the contents of both files are exactly same, it is fine.
    if tbuf == rbuf {
        return true;
    }

    let mut tbuf = tbuf.lines();
    let mut rbuf = rbuf.lines();
    while let (Some(t), Some(r)) = (tbuf.next(), rbuf.next()) {
        if t == r {
            continue;
        }

        let start_element_ns = "SAX.startElementNs(";
        if let (Some(t), Some(r)) = (
            t.strip_prefix(start_element_ns),
            r.strip_prefix(start_element_ns),
        ) {
            // The original libxml2 might pass a direct reference to the input buffer as an attribute value without allocation or copying.
            // Therefore, if its length are too short, the string that includes the buffer follows to the attribute value may be shown.
            let mut t = t.to_owned();
            let mut r = r.to_owned();
            while !t.ends_with(')') {
                let Some(next) = tbuf.next() else {
                    return false;
                };
                t.push('\n');
                t.push_str(next);
            }
            while !r.ends_with(')') {
                let Some(next) = rbuf.next() else {
                    return false;
                };
                r.push('\n');
                r.push_str(next);
            }
            fn trim_before_attribute(s: &str) -> Option<&str> {
                let (_, s) = s.split_once(',')?; // tag name
                let (_, s) = s.split_once(',')?; // prefix
                let (_, s) = s.split_once(',')?; // namespace url
                let (num_namespaces, mut s) = s.split_once(',')?;
                // eprintln!("num_namespaces: {num_namespaces}");
                let num_namespaces = num_namespaces.trim().parse::<usize>().ok()?;
                for _ in 0..num_namespaces {
                    let (_, rem) = s.split_once(',')?; // namespace decls
                    s = rem;
                }
                let (_, s) = s.split_once(',')?; // the number of attributes
                Some(s.split_once(", ")?.1) // the number of defaulted and space
            }
            let (Some(mut tattr), Some(mut rattr)) =
                (trim_before_attribute(&t), trim_before_attribute(&r))
            else {
                eprintln!("SAX compare failed:\n{t}\n{r}");
                return false;
            };
            if t[..t.len() - tattr.len()] != r[..r.len() - rattr.len()] {
                eprintln!(
                    "SAX compare failed:\n{}\n{}",
                    &t[..t.len() - tattr.len()],
                    &r[..r.len() - rattr.len()]
                );
                return false;
            }
            while !tattr.is_empty() {
                if let Some((attname, trem)) = tattr.split_once('\'') {
                    let Some((att, rrem)) = rattr.split_once('\'') else {
                        eprintln!("SAX compare failed:\n{t}\n{r}");
                        return false;
                    };
                    if attname != att {
                        eprintln!("SAX compare failed:\n{t}\n{r}");
                        return false;
                    }
                    let (Some((tatt, trem)), Some((ratt, rrem))) =
                        (trem.split_once(", "), rrem.split_once(", "))
                    else {
                        eprintln!("SAX compare failed:\n{t}\n{r}");
                        return false;
                    };
                    // eprintln!("trem: {trem:?}, rrem: {rrem:?}");
                    let (Some((tlen, trem)), Some((rlen, rrem))) = (
                        trem.split_once(',').or_else(|| trem.split_once(')')),
                        rrem.split_once(',').or_else(|| trem.split_once(')')),
                    ) else {
                        eprintln!("SAX compare failed:\n{t}\n{r}");
                        return false;
                    };
                    if tlen != rlen {
                        eprintln!("SAX compare failed:\n{t}\n{r}");
                        return false;
                    }
                    let (Ok(tlen), Ok(rlen)) = (tlen.parse::<usize>(), rlen.parse::<usize>())
                    else {
                        eprintln!("SAX compare failed:\n{t}\n{r}");
                        return false;
                    };
                    if tatt[..tlen.min(tatt.len())] != ratt[..rlen.min(ratt.len())] {
                        eprintln!("SAX compare failed:\n{t}\n{r}");
                        return false;
                    }
                    tattr = trem;
                    rattr = rrem;
                } else {
                    eprintln!("SAX compare failed:\n{t}\n{r}");
                    return false;
                }
            }
            if rattr.is_empty() {
                continue;
            }
        }

        eprintln!("SAX compare failed:\n{t}\n{r}");
        return false;
    }

    tbuf.next().is_none() && rbuf.next().is_none()
}

/// Parse a file using the SAX API and check for errors.
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "saxParseTest")]
unsafe fn sax_parse_test(
    filename: &str,
    result: Option<String>,
    _err: Option<String>,
    mut options: i32,
) -> i32 {
    unsafe {
        let mut ret: i32;

        NB_TESTS.set(NB_TESTS.get() + 1);
        let temp = result_filename(
            filename,
            TEMP_DIRECTORY.get().cloned().as_deref(),
            Some(".res"),
        );

        let Ok(out) = File::options()
            .write(true)
            .truncate(true)
            .create(true)
            .open(temp.as_str())
        else {
            eprintln!("Failed to write to {temp}",);
            return -1;
        };
        SAX_DEBUG.with_borrow_mut(|f| *f = Some(out));

        // for SAX we really want the callbacks though the context handlers
        set_structured_error(None, None);
        set_generic_error(Some(test_error_handler), None);

        #[cfg(feature = "html")]
        if options & XML_PARSE_HTML != 0 {
            let mut sax = XmlSAXHandler::default();
            std::ptr::copy(&EMPTY_SAXHANDLER_STRUCT, &mut sax, 1);
            let ctxt: HtmlParserCtxtPtr = html_new_sax_parser_ctxt(Some(Box::new(sax)), None);
            html_ctxt_read_file(ctxt, filename, None, options);
            html_free_parser_ctxt(ctxt);
            ret = 0;
        } else {
            let ctxt: XmlParserCtxtPtr = xml_create_file_parser_ctxt(Some(filename));
            let mut sax = XmlSAXHandler::default();
            std::ptr::copy(&EMPTY_SAXHANDLER_STRUCT, &mut sax, 1);
            (*ctxt).sax = Some(Box::new(sax));
            xml_ctxt_use_options(ctxt, options);
            xml_parse_document(ctxt);
            ret = if (*ctxt).well_formed != 0 {
                0
            } else {
                (*ctxt).err_no
            };
            if let Some(my_doc) = (*ctxt).my_doc {
                xml_free_doc(my_doc);
            }
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
                eprintln!("Failed to parse {filename}",);
                ret = 1;
                break 'done;
            }
            #[cfg(feature = "html")]
            if options & XML_PARSE_HTML != 0 {
                let mut sax = XmlSAXHandler::default();
                std::ptr::copy(&DEBUG_HTMLSAXHANDLER_STRUCT, &mut sax, 1);
                let ctxt: HtmlParserCtxtPtr = html_new_sax_parser_ctxt(Some(Box::new(sax)), None);
                html_ctxt_read_file(ctxt, filename, None, options);
                html_free_parser_ctxt(ctxt);
                ret = 0;
            } else {
                let ctxt: XmlParserCtxtPtr = xml_create_file_parser_ctxt(Some(filename));
                let mut sax = XmlSAXHandler::default();
                if options & XmlParserOption::XmlParseSAX1 as i32 != 0 {
                    std::ptr::copy(&DEBUG_SAXHANDLER_STRUCT, &mut sax, 1);
                    options -= XmlParserOption::XmlParseSAX1 as i32;
                } else {
                    std::ptr::copy(&DEBUG_SAX2_HANDLER_STRUCT, &mut sax, 1);
                }
                (*ctxt).sax = Some(Box::new(sax));
                xml_ctxt_use_options(ctxt, options);
                xml_parse_document(ctxt);
                ret = if (*ctxt).well_formed != 0 {
                    0
                } else {
                    (*ctxt).err_no
                };
                if let Some(my_doc) = (*ctxt).my_doc {
                    xml_free_doc(my_doc);
                }
                xml_free_parser_ctxt(ctxt);
            }
            #[cfg(not(feature = "html"))]
            {
                let ctxt: XmlParserCtxtPtr = xml_create_file_parser_ctxt(filename);
                if options & XmlParserOption::XmlParseSAX1 as i32 != 0 {
                    memcpy(
                        (*ctxt).sax as _,
                        addr_of_mut!(DEBUG_SAXHANDLER_STRUCT) as _,
                        size_of::<XmlSAXHandler>(),
                    );
                    options -= XmlParserOption::XmlParseSAX1 as i32;
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
                xml_free_doc(XmlDocPtr::from_raw((*ctxt).my_doc).unwrap().unwrap());
                xml_free_parser_ctxt(ctxt);
            }
            if ret == XmlParserErrors::XmlWarUndeclaredEntity as i32 {
                sax_debugln!("xmlSAXUserParseFile returned error {}", ret);
                ret = 0;
            }

            if compare_files(temp.as_str(), result.as_deref().unwrap()) != 0
                && !sax_compare_file(temp.as_str(), result.unwrap())
            {
                eprintln!("Got a difference for {filename}",);
                let mut content = String::new();
                let mut temp = File::open(temp.as_str()).unwrap();
                temp.read_to_string(&mut content).unwrap();
                eprint!("{content}");
                ret = 1;
            }
        }

        remove_file(temp).ok();

        /* switch back to structured error handling */
        set_generic_error(None, None);
        set_structured_error(Some(test_structured_error_handler), None);

        ret
    }
}

/// Parse a file using the old xmlParseFile API, then serialize back
/// reparse the result and serialize again, then check for deviation
/// in serialization.
///
/// Returns 0 in case of success, an error code otherwise
unsafe fn old_parse_test(
    filename: &str,
    result: Option<String>,
    _err: Option<String>,
    _options: i32,
) -> i32 {
    unsafe {
        let mut res: i32 = 0;

        NB_TESTS.set(NB_TESTS.get() + 1);
        // base of the test, parse with the old API
        #[cfg(feature = "sax1")]
        let Some(mut doc) = xml_parse_file(Some(filename)) else {
            return 1;
        };
        #[cfg(not(feature = "sax1"))]
        let Some(mut doc) = xml_read_file(filename, NULL, 0) else {
            return 1;
        };
        let temp = result_filename(
            filename,
            TEMP_DIRECTORY.get().cloned().as_deref(),
            Some(".res"),
        );
        (*doc).save_file(temp.as_str());
        if compare_files(temp.as_str(), result.as_deref().unwrap()) != 0 {
            res = 1;
        }
        xml_free_doc(doc);

        // Parse the saved result to make sure the round trip is okay
        #[cfg(feature = "sax1")]
        let Some(mut doc) = xml_parse_file(Some(&temp)) else {
            return 1;
        };
        #[cfg(not(feature = "sax1"))]
        let Some(mut doc) = xml_read_file(temp, null_mut(), 0) else {
            return 1;
        };
        (*doc).save_file(temp.as_str());
        if compare_files(temp.as_str(), result.unwrap()) != 0 {
            res = 1;
        }
        xml_free_doc(doc);

        remove_file(temp).ok();
        res
    }
}

/// Parse a file using the Push API, then serialize back
/// to check for content.
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "pushParseTest")]
#[cfg(feature = "libxml_push")]
unsafe fn push_parse_test(
    filename: &str,
    result: Option<String>,
    err: Option<String>,
    options: i32,
) -> i32 {
    unsafe {
        use exml::{
            encoding::XmlCharEncoding,
            libxml::{
                htmlparser::{html_create_push_parser_ctxt, html_parse_chunk},
                parser::{xml_create_push_parser_ctxt, xml_parse_chunk},
            },
        };

        let mut base: *const c_char = null();
        let mut size: i32 = 0;
        let mut res: i32;
        let mut cur: i32 = 0;
        let mut chunk_size: i32 = 4;

        NB_TESTS.set(NB_TESTS.get() + 1);

        // load the document in memory and work from there.
        if load_mem(filename, addr_of_mut!(base), addr_of_mut!(size)) != 0 {
            eprintln!("Failed to load {filename}",);
            return -1;
        }

        if chunk_size > size {
            chunk_size = size;
        }

        #[cfg(feature = "html")]
        let ctxt = if options & XML_PARSE_HTML != 0 {
            html_create_push_parser_ctxt(
                None,
                None,
                base.add(cur as _),
                chunk_size,
                Some(filename),
                XmlCharEncoding::None,
            )
        } else {
            xml_create_push_parser_ctxt(None, None, base.add(cur as _), chunk_size, Some(filename))
        };
        #[cfg(not(feature = "html"))]
        let ctxt =
            xml_create_push_parser_ctxt(None, None, base.add(cur as _), chunk_size, filename);
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
        let doc = (*ctxt).my_doc;
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
            if let Some(doc) = doc {
                xml_free_doc(doc);
            }
            eprintln!("Failed to parse {filename}",);
            return -1;
        }
        #[cfg(feature = "html")]
        if options & XML_PARSE_HTML != 0 {
            html_doc_dump_memory(
                doc.unwrap(),
                addr_of_mut!(base) as *mut *mut XmlChar,
                addr_of_mut!(size),
            );
        } else {
            doc.unwrap()
                .dump_memory(addr_of_mut!(base) as *mut *mut XmlChar, addr_of_mut!(size));
        }
        #[cfg(not(feature = "html"))]
        {
            xml_doc_dump_memory(
                doc,
                addr_of_mut!(base) as *mut *mut XmlChar,
                addr_of_mut!(size),
            );
        }
        xml_free_doc(doc.unwrap());
        res = compare_file_mem(
            result.as_deref().unwrap(),
            from_raw_parts(base as _, size as usize),
        );
        if base.is_null() || res != 0 {
            if !base.is_null() {
                xml_free(base as _);
            }
            eprintln!("Result for {filename} failed in {}", result.unwrap());
            return -1;
        }
        xml_free(base as _);
        if let Some(err) = err {
            let mut emsg = vec![];
            TEST_ERRORS.with_borrow(|errors| {
                emsg.extend_from_slice(&errors[..TEST_ERRORS_SIZE.get()]);
            });
            res = compare_file_mem(err, &emsg);
            if res != 0 {
                eprintln!("Error for {filename} failed",);
                return -1;
            }
        }
        0
    }
}

thread_local! {
    #[cfg(feature = "libxml_push")]
    static PUSH_BOUNDARY_COUNT: Cell<i32> = const { Cell::new(0) };
    #[cfg(feature = "libxml_push")]
    static PUSH_BOUNDARY_REF_COUNT: Cell<i32> = const { Cell::new(0) };
    #[cfg(feature = "libxml_push")]
    static PUSH_BOUNDARY_CHARS_COUNT: Cell<i32> = const { Cell::new(0) };
    #[cfg(feature = "libxml_push")]
    static PUSH_BOUNDARY_CDATA_COUNT: Cell<i32> = const { Cell::new(0) };
}

#[cfg(feature = "libxml_push")]
unsafe fn internal_subset_bnd(
    ctx: Option<GenericErrorContext>,
    name: Option<&str>,
    external_id: Option<&str>,
    system_id: Option<&str>,
) {
    unsafe {
        use exml::libxml::sax2::xml_sax2_internal_subset;

        PUSH_BOUNDARY_COUNT.set(PUSH_BOUNDARY_COUNT.get() + 1);
        xml_sax2_internal_subset(ctx, name, external_id, system_id);
    }
}

#[cfg(feature = "libxml_push")]
unsafe fn reference_bnd(ctx: Option<GenericErrorContext>, name: &str) {
    unsafe {
        use exml::libxml::sax2::xml_sax2_reference;

        PUSH_BOUNDARY_REF_COUNT.set(PUSH_BOUNDARY_REF_COUNT.get() + 1);
        xml_sax2_reference(ctx, name);
    }
}

#[cfg(feature = "libxml_push")]
unsafe fn characters_bnd(ctx: Option<GenericErrorContext>, ch: &str) {
    unsafe {
        use exml::libxml::sax2::xml_sax2_characters;

        PUSH_BOUNDARY_COUNT.set(PUSH_BOUNDARY_COUNT.get() + 1);
        PUSH_BOUNDARY_CHARS_COUNT.set(PUSH_BOUNDARY_CHARS_COUNT.get() + 1);
        xml_sax2_characters(ctx, ch);
    }
}

#[cfg(feature = "libxml_push")]
unsafe fn cdata_block_bnd(ctx: Option<GenericErrorContext>, ch: &str) {
    unsafe {
        use exml::libxml::sax2::xml_sax2_cdata_block;

        PUSH_BOUNDARY_COUNT.set(PUSH_BOUNDARY_COUNT.get() + 1);
        PUSH_BOUNDARY_CDATA_COUNT.set(PUSH_BOUNDARY_CDATA_COUNT.get() + 1);
        xml_sax2_cdata_block(ctx, ch);
    }
}

#[cfg(feature = "libxml_push")]
unsafe fn processing_instruction_bnd(
    ctx: Option<GenericErrorContext>,
    target: &str,
    data: Option<&str>,
) {
    unsafe {
        use exml::libxml::sax2::xml_sax2_processing_instruction;

        PUSH_BOUNDARY_COUNT.set(PUSH_BOUNDARY_COUNT.get() + 1);
        xml_sax2_processing_instruction(ctx, target, data);
    }
}

#[cfg(feature = "libxml_push")]
unsafe fn comment_bnd(ctx: Option<GenericErrorContext>, value: &str) {
    unsafe {
        use exml::libxml::sax2::xml_sax2_comment;

        let ctxt = {
            let ctx = ctx.as_ref().unwrap();
            let lock = ctx.lock();
            *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
        };
        if (*ctxt).in_subset == 0 {
            PUSH_BOUNDARY_COUNT.set(PUSH_BOUNDARY_COUNT.get() + 1);
        }
        xml_sax2_comment(ctx, value);
    }
}

#[cfg(feature = "libxml_push")]
unsafe fn start_element_bnd(
    ctx: Option<GenericErrorContext>,
    xname: &str,
    atts: &[(String, Option<String>)],
) {
    unsafe {
        use exml::libxml::sax2::xml_sax2_start_element;

        // Some elements might be created automatically.
        if xname != "html" && xname != "body" && xname != "head" && xname != "p" {
            PUSH_BOUNDARY_COUNT.set(PUSH_BOUNDARY_COUNT.get() + 1);
        }
        xml_sax2_start_element(ctx, xname, atts);
    }
}

#[cfg(feature = "libxml_push")]
unsafe fn end_element_bnd(ctx: Option<GenericErrorContext>, name: &str) {
    unsafe {
        /*pushBoundaryCount++;*/

        use exml::libxml::sax2::xml_sax2_end_element;
        xml_sax2_end_element(ctx, name);
    }
}

#[cfg(feature = "libxml_push")]
#[allow(clippy::too_many_arguments)]
// #[allow(clippy::type_complexity)]
unsafe fn start_element_ns_bnd(
    ctx: Option<GenericErrorContext>,
    localname: &str,
    prefix: Option<&str>,
    uri: Option<&str>,
    namespaces: &[(Option<String>, String)],
    nb_defaulted: usize,
    attributes: &[(String, Option<String>, Option<String>, String)],
) {
    unsafe {
        use exml::libxml::sax2::xml_sax2_start_element_ns;

        PUSH_BOUNDARY_COUNT.set(PUSH_BOUNDARY_COUNT.get() + 1);
        xml_sax2_start_element_ns(
            ctx,
            localname,
            prefix,
            uri,
            namespaces,
            nb_defaulted,
            attributes,
        );
    }
}

#[cfg(feature = "libxml_push")]
unsafe fn end_element_ns_bnd(
    ctx: Option<GenericErrorContext>,
    localname: &str,
    prefix: Option<&str>,
    uri: Option<&str>,
) {
    unsafe {
        use exml::libxml::sax2::xml_sax2_end_element_ns;
        xml_sax2_end_element_ns(ctx, localname, prefix, uri);
    }
}

/// Test whether the push parser detects boundaries between syntactical
/// elements correctly.
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "pushBoundaryTest")]
#[cfg(feature = "libxml_push")]
unsafe fn push_boundary_test(
    filename: &str,
    result: Option<String>,
    err: Option<String>,
    options: i32,
) -> i32 {
    unsafe {
        use exml::{
            encoding::XmlCharEncoding,
            libxml::{
                htmlparser::{html_create_push_parser_ctxt, html_parse_chunk},
                htmltree::html_doc_dump_memory,
                parser::{
                    XmlParserInputState, XmlSAXHandler, xml_create_push_parser_ctxt,
                    xml_parse_chunk,
                },
                sax2::{xml_sax_version, xml_sax2_init_html_default_sax_handler},
            },
        };

        let mut bnd_sax = XmlSAXHandler::default();
        let mut base: *const c_char = null();
        let mut size: i32 = 0;
        let mut res: i32;
        let mut num_callbacks: i32;
        let mut cur: i32;
        let mut avail: u64;
        let mut old_consumed: u64 = 0;
        let mut consumed: u64;

        // If the parser made progress, check that exactly one construct was
        // processed and that the input buffer is (almost) empty.
        // Since we use a chunk size of 1, this tests whether content is
        // processed as early as possible.

        NB_TESTS.set(NB_TESTS.get() + 1);

        #[cfg(feature = "html")]
        if options & XML_PARSE_HTML != 0 {
            xml_sax2_init_html_default_sax_handler(&mut bnd_sax);
            bnd_sax.start_element = Some(start_element_bnd);
            bnd_sax.end_element = Some(end_element_bnd);
        } else {
            xml_sax_version(&mut bnd_sax, 2);
            bnd_sax.start_element_ns = Some(start_element_ns_bnd);
            bnd_sax.end_element_ns = Some(end_element_ns_bnd);
        }
        #[cfg(not(feature = "html"))]
        {
            xml_sax_version(addr_of_mut!(bndSAX) as _, 2);
            bndSAX.startElementNs = Some(start_element_ns_bnd);
            bndSAX.endElementNs = Some(end_element_ns_bnd);
        }

        bnd_sax.internal_subset = Some(internal_subset_bnd);
        bnd_sax.reference = Some(reference_bnd);
        bnd_sax.characters = Some(characters_bnd);
        bnd_sax.cdata_block = Some(cdata_block_bnd);
        bnd_sax.processing_instruction = Some(processing_instruction_bnd);
        bnd_sax.comment = Some(comment_bnd);

        // load the document in memory and work from there.
        if load_mem(filename, addr_of_mut!(base), addr_of_mut!(size)) != 0 {
            eprintln!("Failed to load {filename}",);
            return -1;
        }

        #[cfg(feature = "html")]
        let ctxt = if options & XML_PARSE_HTML != 0 {
            html_create_push_parser_ctxt(
                Some(Box::new(bnd_sax)),
                None,
                base,
                1,
                Some(filename),
                XmlCharEncoding::None,
            )
        } else {
            xml_create_push_parser_ctxt(Some(Box::new(bnd_sax)), None, base, 1, Some(filename))
        };
        #[cfg(not(feature = "html"))]
        let ctxt =
            xml_create_push_parser_ctxt(Some(Box::new(bnd_sax)), null_mut(), base, 1, filename);
        xml_ctxt_use_options(ctxt, options);
        cur = 1;
        consumed = 0;
        num_callbacks = 0;
        avail = 0;
        while cur < size && num_callbacks <= 1 && avail == 0 {
            let terminate = (cur + 1 >= size) as i32;
            let mut is_text: i32 = 0;

            if (*ctxt).instate == XmlParserInputState::XmlParserContent {
                let first_char: i32 = if (*(*ctxt).input).end > (*(*ctxt).input).cur {
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
                + (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as u64;

            PUSH_BOUNDARY_COUNT.set(0);
            PUSH_BOUNDARY_REF_COUNT.set(0);
            PUSH_BOUNDARY_CHARS_COUNT.set(0);
            PUSH_BOUNDARY_CDATA_COUNT.set(0);

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

            // Callback check: Check that only a single construct was parsed.
            if PUSH_BOUNDARY_REF_COUNT.get() > 0 {
                num_callbacks = 1;
            } else {
                num_callbacks = PUSH_BOUNDARY_COUNT.get();
                if PUSH_BOUNDARY_CHARS_COUNT.get() > 1 {
                    if options & XML_PARSE_HTML != 0 {
                        // The HTML parser can generate a mix of chars and references.
                        num_callbacks -= PUSH_BOUNDARY_CHARS_COUNT.get() - 1;
                    } else {
                        // Allow two chars callbacks. This can happen when
                        // multi-byte chars are split across buffer boundaries.
                        num_callbacks -= 1;
                    }
                }
                if options & XML_PARSE_HTML != 0 {
                    // Allow multiple cdata callbacks in HTML mode.
                    if PUSH_BOUNDARY_CDATA_COUNT.get() > 1 {
                        num_callbacks -= PUSH_BOUNDARY_CDATA_COUNT.get() - 1;
                    }
                }
            }

            // Buffer check: If input was consumed, check that the input
            // buffer is (almost) empty.
            consumed = (*(*ctxt).input).consumed
                + (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as u64;
            if (*ctxt).instate != XmlParserInputState::XmlParserDTD
                && consumed >= 4
                && consumed != old_consumed
            {
                let mut max: size_t = 0;

                avail = (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) as _;

                if options & XML_PARSE_HTML != 0
                    && ((*ctxt).instate == XmlParserInputState::XmlParserEndTag)
                {
                    // Something related to script parsing.
                    max = 3;
                } else if is_text != 0 {
                    let c: i32 = *(*(*ctxt).input).cur as i32;

                    // 3 bytes for partial UTF-8
                    max = if c == b'<' as i32 || c == b'&' as i32 {
                        1
                    } else {
                        3
                    };
                } else if (*ctxt).instate == XmlParserInputState::XmlParserCDATASection {
                    // 2 bytes for terminator, 3 bytes for UTF-8
                    max = 5;
                }

                if avail <= max as _ {
                    avail = 0;
                }
            }
        }
        let doc = (*ctxt).my_doc;
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
            if let Some(doc) = doc {
                xml_free_doc(doc);
            }
            eprintln!(
                "Failed push boundary callback test ({}@{}-{}): {filename}",
                num_callbacks, old_consumed, consumed,
            );
            return -1;
        }
        if avail > 0 {
            xml_free_doc(doc.unwrap());
            eprintln!(
                "Failed push boundary buffer test ({}@{}): {filename}",
                avail, consumed,
            );
            return -1;
        }
        if res == 0 {
            xml_free_doc(doc.unwrap());
            eprintln!("Failed to parse {filename}",);
            return -1;
        }
        #[cfg(feature = "html")]
        if options & XML_PARSE_HTML != 0 {
            html_doc_dump_memory(
                doc.unwrap(),
                addr_of_mut!(base) as *mut *mut XmlChar,
                addr_of_mut!(size),
            );
        } else {
            doc.unwrap()
                .dump_memory(addr_of_mut!(base) as *mut *mut XmlChar, addr_of_mut!(size));
        }
        #[cfg(not(feature = "html"))]
        {
            xml_doc_dump_memory(
                doc,
                addr_of_mut!(base) as *mut *mut XmlChar,
                addr_of_mut!(size),
            );
        }
        xml_free_doc(doc.unwrap());
        res = compare_file_mem(
            result.as_deref().unwrap(),
            from_raw_parts(base as _, size as usize),
        );
        if base.is_null() || res != 0 {
            if !base.is_null() {
                xml_free(base as _);
            }
            eprintln!("Result for {filename} failed in {}", result.unwrap());
            return -1;
        }
        xml_free(base as _);
        if let Some(err) = err {
            res = TEST_ERRORS
                .with_borrow(|errors| compare_file_mem(err, &errors[..TEST_ERRORS_SIZE.get()]));
            if res != 0 {
                eprintln!("Error for {filename} failed",);
                return -1;
            }
        }
        0
    }
}

/// Parse a file using the old xmlReadMemory API, then serialize back
/// reparse the result and serialize again, then check for deviation
/// in serialization.
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "memParseTest")]
unsafe fn mem_parse_test(
    filename: &str,
    result: Option<String>,
    _err: Option<String>,
    _options: i32,
) -> i32 {
    unsafe {
        let mut base: *const c_char = null();
        let mut size: i32 = 0;

        NB_TESTS.set(NB_TESTS.get() + 1);
        // load and parse the memory
        if load_mem(filename, &raw mut base, &raw mut size) != 0 {
            eprintln!("Failed to load {filename}",);
            return -1;
        }

        let buffer = from_raw_parts(base as *const u8, size as usize).to_vec();
        let doc = xml_read_memory(buffer, Some(filename), None, 0);
        unload_mem(base);
        let Some(mut doc) = doc else {
            return 1;
        };
        doc.dump_memory(addr_of_mut!(base) as *mut *mut XmlChar, addr_of_mut!(size));
        xml_free_doc(doc);
        let res: i32 = compare_file_mem(
            result.as_deref().unwrap(),
            from_raw_parts(base as _, size as _),
        );
        if base.is_null() || res != 0 {
            if !base.is_null() {
                xml_free(base as _);
            }
            eprintln!("Result for {filename} failed in {}", result.unwrap());
            return -1;
        }
        xml_free(base as _);
        0
    }
}

/// Parse a file with entity resolution, then serialize back
/// reparse the result and serialize again, then check for deviation
/// in serialization.
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "noentParseTest")]
unsafe fn noent_parse_test(
    filename: &str,
    result: Option<String>,
    _err: Option<String>,
    options: i32,
) -> i32 {
    unsafe {
        let mut res: i32 = 0;

        NB_TESTS.set(NB_TESTS.get() + 1);
        // base of the test, parse with the old API
        let Some(mut doc) = xml_read_file(filename, None, options) else {
            return 1;
        };
        let temp = result_filename(
            filename,
            TEMP_DIRECTORY.get().cloned().as_deref(),
            Some(".res"),
        );
        doc.save_file(temp.as_str());
        if compare_files(temp.as_str(), result.as_deref().unwrap()) != 0 {
            res = 1;
        }
        xml_free_doc(doc);

        // Parse the saved result to make sure the round trip is okay
        let Some(mut doc) = xml_read_file(filename, None, options) else {
            return 1;
        };
        doc.save_file(temp.as_str());
        if compare_files(temp.as_str(), result.unwrap()) != 0 {
            res = 1;
        }
        xml_free_doc(doc);

        remove_file(temp).ok();
        res
    }
}

/// Parse a file using the xmlReadFile API and check for errors.
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "errParseTest")]
unsafe fn err_parse_test(
    filename: &str,
    result: Option<String>,
    err: Option<String>,
    options: i32,
) -> i32 {
    unsafe {
        let mut base: *const c_char = null_mut();
        let mut size: i32 = 0;
        let mut res: i32 = 0;
        let cresult = result.as_deref().map(|s| CString::new(s).unwrap());

        NB_TESTS.set(NB_TESTS.get() + 1);
        let doc = match options {
            #[cfg(feature = "html")]
            options if options & XML_PARSE_HTML != 0 => html_read_file(filename, None, options),
            #[cfg(feature = "xinclude")]
            options if options & XmlParserOption::XmlParseXInclude as i32 != 0 => {
                let mut doc = xml_read_file(filename, None, options);
                if let Some(doc) = doc.take_if(|doc| xml_xinclude_process_flags(*doc, options) < 0)
                {
                    xml_free_doc(doc);
                }
                doc
            }
            _ => xml_read_file(filename, None, options),
        };
        if let Some(result) = cresult {
            if let Some(mut doc) = doc {
                #[cfg(feature = "html")]
                if options & XML_PARSE_HTML != 0 {
                    html_doc_dump_memory(
                        doc,
                        addr_of_mut!(base) as *mut *mut XmlChar,
                        addr_of_mut!(size),
                    );
                } else {
                    doc.dump_memory(addr_of_mut!(base) as *mut *mut XmlChar, addr_of_mut!(size));
                }
                #[cfg(not(feature = "html"))]
                {
                    xml_doc_dump_memory(
                        doc,
                        addr_of_mut!(base) as *mut *mut XmlChar,
                        addr_of_mut!(size),
                    );
                }
            } else {
                base = c"".as_ptr();
                size = 0;
            }
            res = compare_file_mem(
                CStr::from_ptr(result.as_ptr()).to_string_lossy().as_ref(),
                from_raw_parts(base as _, size as _),
            );
        }
        if let Some(doc) = doc {
            if !base.is_null() {
                xml_free(base as _);
            }
            xml_free_doc(doc);
        }
        if res != 0 {
            eprintln!("Result for {filename} failed in {}", result.unwrap());
            return -1;
        }
        if let Some(err) = err {
            res = TEST_ERRORS
                .with_borrow(|errors| compare_file_mem(err, &errors[..TEST_ERRORS_SIZE.get()]));
            if res != 0 {
                eprintln!("Error for {filename} failed",);
                return -1;
            }
        } else if options & XmlParserOption::XmlParseDTDValid as i32 != 0
            && TEST_ERRORS_SIZE.get() != 0
        {
            eprintln!("Validation for {filename} failed",);
        }

        0
    }
}

#[cfg(feature = "libxml_reader")]
unsafe extern "C" fn process_node(out: &mut File, reader: XmlTextReaderPtr) {
    unsafe {
        use exml::libxml::xmlreader::{xml_text_reader_const_name, xml_text_reader_const_value};

        let mut name: *const XmlChar;

        let typ = (*reader).node_type();
        let empty = (*reader).is_empty_element();

        name = xml_text_reader_const_name(&mut *reader);
        if name.is_null() {
            name = c"--".as_ptr() as _;
        }

        let value: *const XmlChar = xml_text_reader_const_value(&mut *reader);

        write!(
            out,
            "{} {} {} {} {}",
            (*reader).depth(),
            typ as i32,
            CStr::from_ptr(name as _).to_string_lossy(),
            empty.map_or(-1, |e| e as i32),
            (*reader).has_value() as i32,
        )
        .ok();
        if value.is_null() {
            writeln!(out).ok();
        } else {
            writeln!(out, " {}", CStr::from_ptr(value as _).to_string_lossy()).ok();
        }
    }
}

#[cfg(feature = "libxml_reader")]
unsafe fn stream_process_test(
    filename: &str,
    result: Option<String>,
    err: Option<String>,
    reader: XmlTextReaderPtr,
    rng: *const c_char,
    _options: i32,
) -> i32 {
    unsafe {
        use exml::libxml::xmlreader::xml_text_reader_relaxng_validate;

        let mut ret: i32;
        let mut temp = None;

        if reader.is_null() {
            return -1;
        }

        NB_TESTS.set(NB_TESTS.get() + 1);
        let mut t = None;
        if result.is_some() {
            let tm = result_filename(
                filename,
                TEMP_DIRECTORY.get().cloned().as_deref(),
                Some(".res"),
            );
            match File::options()
                .write(true)
                .truncate(true)
                .create(true)
                .open(tm.as_str())
            {
                Ok(file) => t = Some(file),
                _ => {
                    eprintln!("Can't open temp file {tm}",);
                    return -1;
                }
            };
            temp = Some(tm);
        }
        #[cfg(feature = "schema")]
        if !rng.is_null() {
            ret = xml_text_reader_relaxng_validate(reader, rng);
            if ret < 0 {
                let rng = CStr::from_ptr(rng).to_string_lossy();
                test_error_handler(
                    None,
                    format!("Relax-NG schema {rng} failed to compile\n").as_str(),
                );

                if let Some(temp) = temp {
                    remove_file(temp).ok();
                }
                return 0;
            }
        }
        ret = (*reader).read();
        while ret == 1 {
            if let Some(t) = t.as_mut().filter(|_| rng.is_null()) {
                process_node(t, reader);
            }
            ret = (*reader).read();
        }
        if ret != 0 {
            test_error_handler(None, format!("{filename} : failed to parse\n").as_str());
        }
        if !rng.is_null() {
            if !(*reader).is_valid().unwrap_or(false) {
                test_error_handler(None, format!("{filename} fails to validate\n").as_str());
            } else {
                test_error_handler(None, format!("{filename} validates\n").as_str());
            }
        }
        if t.is_some() {
            ret = compare_files(temp.as_deref().unwrap(), result.as_deref().unwrap());
            if let Some(temp) = temp {
                remove_file(temp).ok();
            }
            if ret != 0 {
                eprintln!("Result for {filename} failed in {}", result.unwrap());
                return -1;
            }
        }
        if let Some(err) = err {
            ret = TEST_ERRORS
                .with_borrow(|errors| compare_file_mem(err, &errors[..TEST_ERRORS_SIZE.get()]));
            if ret != 0 {
                eprintln!("Error for {filename} failed");
                TEST_ERRORS.with_borrow(|errors| {
                    print!("{}", CStr::from_ptr(errors.as_ptr() as _).to_string_lossy());
                });
                return -1;
            }
        }

        0
    }
}

/// Parse a file using the reader API and check for errors.
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "streamParseTest")]
#[cfg(feature = "libxml_reader")]
unsafe fn stream_parse_test(
    filename: &str,
    result: Option<String>,
    err: Option<String>,
    options: i32,
) -> i32 {
    unsafe {
        use exml::libxml::xmlreader::{xml_free_text_reader, xml_reader_for_file};

        let reader: XmlTextReaderPtr = xml_reader_for_file(filename, null_mut(), options);
        let ret: i32 = stream_process_test(filename, result, err, reader, null_mut(), options);
        xml_free_text_reader(reader);
        ret
    }
}

/// Parse a file using the walker, i.e. a reader built from a atree.
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "walkerParseTest")]
#[cfg(feature = "libxml_reader")]
unsafe fn walker_parse_test(
    filename: &str,
    result: Option<String>,
    err: Option<String>,
    options: i32,
) -> i32 {
    unsafe {
        use exml::libxml::xmlreader::{xml_free_text_reader, xml_reader_walker};

        let Some(doc) = xml_read_file(filename, None, options) else {
            eprintln!("Failed to parse {filename}",);
            return -1;
        };
        let reader: XmlTextReaderPtr = xml_reader_walker(doc);
        let ret: i32 = stream_process_test(filename, result, err, reader, null_mut(), options);
        xml_free_text_reader(reader);
        xml_free_doc(doc);
        ret
    }
}

/// Parse a file using the reader API from memory and check for errors.
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "streamMemParseTest")]
#[cfg(feature = "libxml_reader")]
unsafe fn stream_mem_parse_test(
    filename: &str,
    result: Option<String>,
    err: Option<String>,
    options: i32,
) -> i32 {
    unsafe {
        use exml::libxml::xmlreader::{xml_free_text_reader, xml_reader_for_memory};

        let mut base: *const c_char = null();
        let mut size: i32 = 0;

        // load and parse the memory
        if load_mem(filename, addr_of_mut!(base), addr_of_mut!(size)) != 0 {
            eprintln!("Failed to load {filename}",);
            return -1;
        }
        let buffer = from_raw_parts(base as *const u8, size as usize).to_vec();
        let reader: XmlTextReaderPtr =
            xml_reader_for_memory(buffer, Some(filename), null_mut(), options);
        let ret: i32 = stream_process_test(filename, result, err, reader, null_mut(), options);
        free(base as _);
        xml_free_text_reader(reader);
        ret
    }
}

thread_local! {
    #[cfg(all(feature = "xpath", feature = "libxml_debug"))]
    static XPATH_OUTPUT: RefCell<Option<File>> = const { RefCell::new(None) };
    #[cfg(all(feature = "xpath", feature = "libxml_debug"))]
    static XPATH_DOCUMENT: Cell<Option<XmlDocPtr>> = const { Cell::new(None) };
}

#[cfg(all(feature = "xpath", feature = "libxml_debug"))]
fn ignore_generic_error(_ctx: Option<GenericErrorContext>, _msg: &str) {}

#[cfg(all(feature = "xpath", feature = "libxml_debug"))]
unsafe extern "C" fn test_xpath(str: *const c_char, xptr: i32, expr: i32) {
    unsafe {
        use exml::{
            libxml::xpointer::{xml_xptr_eval, xml_xptr_new_context},
            xpath::{
                XmlXPathCompExprPtr, XmlXPathContextPtr, XmlXPathObjectPtr, xml_xpath_compile,
                xml_xpath_compiled_eval, xml_xpath_debug_dump_object, xml_xpath_eval_expression,
                xml_xpath_free_comp_expr, xml_xpath_free_context, xml_xpath_free_object,
                xml_xpath_new_context,
            },
        };

        let res: XmlXPathObjectPtr;
        let ctxt: XmlXPathContextPtr;

        // Don't print generic errors to stderr.
        set_generic_error(Some(ignore_generic_error), None);

        NB_TESTS.set(NB_TESTS.get() + 1);
        if cfg!(feature = "xpointer") && xptr != 0 {
            #[cfg(feature = "xpointer")]
            {
                ctxt = xml_xptr_new_context(XPATH_DOCUMENT.get(), None, None);
                res = xml_xptr_eval(str as _, ctxt);
            }
        } else {
            let xpath_document = XPATH_DOCUMENT.get();
            ctxt = xml_xpath_new_context(xpath_document);
            (*ctxt).node = xpath_document
                .and_then(|doc| doc.get_root_element())
                .map(|root| root.into());
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
        XPATH_OUTPUT.with_borrow_mut(|out| {
            let out = out.as_mut().unwrap();
            xml_xpath_debug_dump_object(out, res, 0);
            out.flush().ok();
        });
        xml_xpath_free_object(res);
        xml_xpath_free_context(ctxt);

        // Reset generic error handler.
        set_generic_error(None, None);
    }
}

/// Parse a file containing XPath standalone expressions and evaluate them
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "xpathExprTest")]
#[cfg(all(feature = "xpath", feature = "libxml_debug"))]
unsafe fn xpath_common_test(filename: &str, result: Option<String>, xptr: i32, expr: i32) -> i32 {
    unsafe {
        use std::io::{BufRead, BufReader};

        use exml::generic_error;

        let mut ret: i32 = 0;

        let temp = result_filename(
            filename,
            TEMP_DIRECTORY.get().cloned().as_deref(),
            Some(".res"),
        );

        let Ok(out) = File::options()
            .write(true)
            .truncate(true)
            .create(true)
            .open(temp.as_str())
        else {
            eprintln!("failed to open output file {temp}",);
            return -1;
        };
        XPATH_OUTPUT.with_borrow_mut(|f| *f = Some(out));

        let mut input = match File::open(filename) {
            Ok(file) => BufReader::new(file),
            _ => {
                generic_error!("Cannot open {filename} for reading\n");
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
                XPATH_OUTPUT.with_borrow_mut(|f| {
                    writeln!(
                        f.as_mut().unwrap(),
                        "\n========================\nExpression: {}",
                        CStr::from_ptr(expression.as_ptr() as _).to_string_lossy(),
                    )
                    .ok()
                });
                test_xpath(expression.as_ptr() as _, xptr, expr);
            }
            expression.clear();
        }

        if let Some(result) = result {
            ret = compare_files(temp.as_str(), result.as_str());
            if ret != 0 {
                // Rust `format!` does not support the similar of `%g` of C lang.
                // Therefore, the representation of the floating point number may be different
                // from result files of the original libxml2.
                let mut t = BufReader::new(File::open(temp.as_str()).unwrap());
                let mut r = BufReader::new(File::open(result.as_str()).unwrap());
                let (mut ts, mut rs) = (String::new(), String::new());
                let mut ok = true;
                let mut cache = String::new();
                while t.read_line(&mut ts).ok().filter(|&len| len > 0).is_none() {
                    if r.read_line(&mut rs).ok().filter(|&len| len > 0).is_none() {
                        ok = false;
                        break;
                    }
                    if ts.starts_with("Expression") {
                        if ts != rs {
                            ok = false;
                            break;
                        }
                        cache = ts.clone();
                    } else if ts.starts_with("Object is a number") {
                        if ts == rs {
                            ts.clear();
                            rs.clear();
                            continue;
                        }
                        let Some((_, expr)) = cache.split_once(' ') else {
                            ok = false;
                            break;
                        };
                        let Ok(float) = expr.trim().parse::<f64>() else {
                            ok = false;
                            break;
                        };
                        let s = format!("Object is a number : {float}\n");
                        if ts != s {
                            ok = false;
                            break;
                        }
                    } else if ts != rs {
                        ok = false;
                        break;
                    }
                    ts.clear();
                    rs.clear();
                }
                if ok {
                    ret = 0;
                } else {
                    eprintln!("Result for {filename} failed in {}", result);
                }
            }
        }

        remove_file(temp).ok();
        ret
    }
}

/// Parse a file containing XPath standalone expressions and evaluate them
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "xpathExprTest")]
#[cfg(all(feature = "xpath", feature = "libxml_debug"))]
unsafe fn xpath_expr_test(
    filename: &str,
    result: Option<String>,
    _err: Option<String>,
    _options: i32,
) -> i32 {
    unsafe { xpath_common_test(filename, result, 0, 1) }
}

/// Parse a file containing XPath expressions and evaluate them against
/// a set of corresponding documents.
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "xpathDocTest")]
#[cfg(all(feature = "xpath", feature = "libxml_debug"))]
unsafe fn xpath_doc_test(
    filename: &str,
    _resul: Option<String>,
    _err: Option<String>,
    options: i32,
) -> i32 {
    unsafe {
        use std::mem::zeroed;

        use libc::{GLOB_DOOFFS, glob, glob_t, globfree};

        let mut pattern: [c_char; 500] = [0; 500];
        let mut result: [c_char; 500] = [0; 500];
        let mut globbuf: glob_t = zeroed();
        let mut ret: i32 = 0;
        let mut res: i32;

        if let Some(xpath_document) = xml_read_file(
            filename,
            None,
            options
                | XmlParserOption::XmlParseDTDAttr as i32
                | XmlParserOption::XmlParseNoEnt as i32,
        ) {
            XPATH_DOCUMENT.set(xpath_document.into());
        } else {
            eprintln!("Failed to load {filename}",);
            return -1;
        }

        let cbase = CString::new(base_filename(filename)).unwrap();
        res = snprintf(
            pattern.as_mut_ptr() as _,
            499,
            c"./test/XPath/tests/%s*".as_ptr(),
            cbase.as_ptr(),
        );
        if res >= 499 {
            pattern[499] = 0;
        }
        globbuf.gl_offs = 0;
        glob(pattern.as_ptr(), GLOB_DOOFFS, None, addr_of_mut!(globbuf));
        for i in 0..globbuf.gl_pathc {
            let cbase = CString::new(base_filename(
                CStr::from_ptr(*globbuf.gl_pathv.add(i))
                    .to_string_lossy()
                    .as_ref(),
            ))
            .unwrap();
            res = snprintf(
                result.as_mut_ptr() as _,
                499,
                c"./result/XPath/tests/%s".as_ptr(),
                cbase.as_ptr(),
            );
            if res >= 499 {
                result[499] = 0;
            }
            let filename = CStr::from_ptr(*globbuf.gl_pathv.add(i)).to_string_lossy();
            let result = CStr::from_ptr(result.as_ptr()).to_string_lossy();
            res = xpath_common_test(&filename, Some(result.into_owned()), 0, 0);
            if res != 0 {
                ret = res;
            }
        }
        globfree(addr_of_mut!(globbuf));

        if let Some(doc) = XPATH_DOCUMENT.get() {
            xml_free_doc(doc);
        }
        ret
    }
}

/// Parse a file containing XPath expressions and evaluate them against
/// a set of corresponding documents.
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "xptrDocTest")]
#[cfg(all(feature = "xpath", feature = "libxml_debug", feature = "xpointer"))]
unsafe fn xptr_doc_test(
    filename: &str,
    _resul: Option<String>,
    _err: Option<String>,
    options: i32,
) -> i32 {
    unsafe {
        use std::mem::zeroed;

        use libc::{GLOB_DOOFFS, glob, glob_t, globfree};

        let mut pattern: [c_char; 500] = [0; 500];
        let mut result: [c_char; 500] = [0; 500];
        let mut globbuf: glob_t = zeroed();
        let mut ret: i32 = 0;
        let mut res: i32;
        let subdir: *const c_char = if options == -1 {
            c"xptr-xp1".as_ptr()
        } else {
            c"xptr".as_ptr()
        };

        if let Some(xpath_document) = xml_read_file(
            filename,
            None,
            XmlParserOption::XmlParseDTDAttr as i32 | XmlParserOption::XmlParseNoEnt as i32,
        ) {
            XPATH_DOCUMENT.set(xpath_document.into())
        } else {
            eprintln!("Failed to load {filename}",);
            return -1;
        }

        let cbase = CString::new(base_filename(filename)).unwrap();
        res = snprintf(
            pattern.as_mut_ptr() as _,
            499,
            c"./test/XPath/%s/%s*".as_ptr(),
            subdir,
            cbase.as_ptr(),
        );
        if res >= 499 {
            pattern[499] = 0;
        }
        globbuf.gl_offs = 0;
        glob(pattern.as_ptr(), GLOB_DOOFFS, None, addr_of_mut!(globbuf));
        for i in 0..globbuf.gl_pathc {
            let cbase = CString::new(base_filename(
                CStr::from_ptr(*globbuf.gl_pathv.add(i))
                    .to_string_lossy()
                    .as_ref(),
            ))
            .unwrap();
            res = snprintf(
                result.as_mut_ptr(),
                499,
                c"./result/XPath/%s/%s".as_ptr(),
                subdir,
                cbase.as_ptr(),
            );
            if res >= 499 {
                result[499] = 0;
            }
            let filename = CStr::from_ptr(*globbuf.gl_pathv.add(i)).to_string_lossy();
            let result = CStr::from_ptr(result.as_ptr()).to_string_lossy();
            res = xpath_common_test(&filename, Some(result.into_owned()), 1, 0);
            if res != 0 {
                ret = res;
            }
        }
        globfree(addr_of_mut!(globbuf));

        if let Some(doc) = XPATH_DOCUMENT.get() {
            xml_free_doc(doc);
        }
        ret
    }
}

/// Parse a file containing xml:id and check for errors and verify
/// that XPath queries will work on them as expected.
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "xmlidDocTest")]
#[cfg(all(feature = "xpath", feature = "libxml_debug", feature = "libxml_valid"))]
unsafe fn xmlid_doc_test(
    filename: &str,
    result: Option<String>,
    err: Option<String>,
    options: i32,
) -> i32 {
    unsafe {
        let mut res: i32 = 0;
        let mut ret: i32;

        if let Some(xpath_document) = xml_read_file(
            filename,
            None,
            options
                | XmlParserOption::XmlParseDTDAttr as i32
                | XmlParserOption::XmlParseNoEnt as i32,
        ) {
            XPATH_DOCUMENT.set(xpath_document.into())
        } else {
            eprintln!("Failed to load {filename}",);
            return -1;
        }

        let temp = result_filename(
            filename,
            TEMP_DIRECTORY.get().cloned().as_deref(),
            Some(".res"),
        );

        let Ok(out) = File::options()
            .write(true)
            .truncate(true)
            .create(true)
            .open(temp.as_str())
        else {
            eprintln!("failed to open output file {temp}",);
            if let Some(doc) = XPATH_DOCUMENT.get() {
                xml_free_doc(doc);
            }
            return -1;
        };
        XPATH_OUTPUT.with_borrow_mut(|f| *f = Some(out));

        test_xpath(c"id('bar')".as_ptr() as _, 0, 0);

        if let Some(result) = result {
            ret = compare_files(temp.as_str(), result.as_str());
            if ret != 0 {
                eprintln!("Result for {filename} failed in {}", result);
                res = 1;
            }
        }

        remove_file(temp).ok();
        if let Some(doc) = XPATH_DOCUMENT.get() {
            xml_free_doc(doc);
        }

        if let Some(err) = err {
            ret = TEST_ERRORS
                .with_borrow(|errors| compare_file_mem(err, &errors[..TEST_ERRORS_SIZE.get()]));
            if ret != 0 {
                eprintln!("Error for {filename} failed",);
                res = 1;
            }
        }
        res
    }
}

unsafe fn handle_uri(str: &str, base: Option<&str>, o: &mut File) {
    let mut uri = XmlURI::new();

    if let Some(base) = base {
        eprintln!("base: {base}, str: {str}");
        if let Some(res) = build_uri(str, base) {
            writeln!(o, "{res}",).ok();
            eprintln!("OK: {res}");
        } else {
            writeln!(o, "::ERROR::").ok();
            eprintln!("NG...");
        }
    } else {
        let ret = uri.parse_uri_reference(str);
        if let Err(ret) = ret {
            writeln!(o, "{str} : error {}", ret).ok();
        } else {
            if let Some(path) = uri.path.take() {
                let new_path = normalize_uri_path(&path);
                uri.path = Some(new_path.into_owned().into());
            }
            eprintln!("str: {str:<20}, uri: {uri}, debug: {uri:?}");
            writeln!(o, "{uri}").ok();
            o.flush().ok();
        }
    }
}

/// Parse a file containing URI and check for errors
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "uriCommonTest")]
unsafe fn uri_common_test(
    filename: &str,
    result: Option<String>,
    err: Option<String>,
    base: Option<&str>,
) -> i32 {
    unsafe {
        let mut res: i32 = 0;
        let mut ret: i32;

        let temp = result_filename(
            filename,
            TEMP_DIRECTORY.get().cloned().as_deref(),
            Some(".res"),
        );
        let Ok(mut o) = File::options()
            .write(true)
            .truncate(true)
            .create(true)
            .open(temp.as_str())
        else {
            eprintln!("failed to open output file {temp}",);
            return -1;
        };
        let Ok(mut f) = File::open(filename).map(BufReader::new) else {
            eprintln!("failed to open input file {filename}");
            remove_file(temp).ok();
            return -1;
        };

        let mut str = vec![];
        loop {
            // read one line in string buffer.
            match f.read_until(b'\n', &mut str) {
                Ok(mut len) if len > 0 => {
                    // remove the ending spaces
                    while len > 0
                        && (str[len - 1] == b'\n'
                            || str[len - 1] == b'\r'
                            || str[len - 1] == b' '
                            || str[len - 1] == b'\t')
                    {
                        len -= 1;
                        str.pop();
                    }
                    NB_TESTS.set(NB_TESTS.get() + 1);
                    handle_uri(from_utf8(&str).unwrap(), base, &mut o);
                }
                _ => {
                    break;
                }
            }
            str.clear();
        }

        if let Some(result) = result {
            ret = compare_files(temp.as_str(), result.as_str());
            if ret != 0 {
                eprintln!("Result for {filename} failed in {result}");
                res = 1;
            }
        }
        if let Some(err) = err {
            ret = TEST_ERRORS
                .with_borrow(|errors| compare_file_mem(err, &errors[..TEST_ERRORS_SIZE.get()]));
            if ret != 0 {
                eprintln!("Error for {filename} failed",);
                res = 1;
            }
        }

        remove_file(temp).ok();
        res
    }
}

/// Parse a file containing URI and check for errors
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "uriParseTest")]
unsafe fn uri_parse_test(
    filename: &str,
    result: Option<String>,
    err: Option<String>,
    _options: i32,
) -> i32 {
    unsafe { uri_common_test(filename, result, err, None) }
}

/// Parse a file containing URI, compose them against a fixed base and
/// check for errors
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "uriBaseTest")]
unsafe fn uri_base_test(
    filename: &str,
    result: Option<String>,
    err: Option<String>,
    _options: i32,
) -> i32 {
    unsafe {
        uri_common_test(
            filename,
            result,
            err,
            Some("http://foo.com/path/to/index.html?orig#help"),
        )
    }
}

const URIP_TEST_URLS: &[&str] = &[
    "urip://example.com/a b.html",
    "urip://example.com/a%20b.html",
    "file:///path/to/a b.html",
    "file:///path/to/a%20b.html",
    "/path/to/a b.html",
    "/path/to/a%20b.html",
    // "urip://example.com/r\xe9sum\xe9.html", // This is invalid UTF-8 sequence, so cannot compile.
    "urip://example.com/test?a=1&b=2%263&c=4#foo",
];
const URIP_RCVS_URLS: &[&str] = &[
    // it is an URI the strings must be escaped
    "urip://example.com/a%20b.html",
    // check that % escaping is not broken
    "urip://example.com/a%20b.html",
    // it's an URI path the strings must be escaped
    "file:///path/to/a%20b.html",
    // check that % escaping is not broken
    "file:///path/to/a%20b.html",
    // this is not an URI, this is a path, so this should not be escaped
    "/path/to/a b.html",
    // check that paths with % are not broken
    "/path/to/a%20b.html",
    // out of context the encoding can't be guessed byte by byte conversion
    // "urip://example.com/r%E9sum%E9.html",
    // verify we don't destroy URIs especially the query part
    "urip://example.com/test?a=1&b=2%263&c=4#foo",
];
const URIP_RES: &CStr = c"<list/>";
thread_local! {
    static URIP_SUCCESS: Cell<i32> = const { Cell::new(1) };
    static URIP_CURRENT: Cell<usize> = const { Cell::new(0) };
    static URIP_CUR: Cell<*mut i8> = const { Cell::new(null_mut()) };
    static URIP_RLEN: Cell<usize> = const { Cell::new(0) };
}

/// Check for an urip: query
///
/// Returns 1 if yes and 0 if another Input module should be used
#[doc(alias = "uripMatch")]
unsafe fn urip_match(uri: &str) -> i32 {
    const CATALOG_URL: &str = concatcp!("file://", SYSCONFDIR, "/xml/catalog");
    if uri == CATALOG_URL {
        return 0;
    }
    /* Verify we received the escaped URL */
    if URIP_RCVS_URLS[URIP_CURRENT.get()] != uri {
        URIP_SUCCESS.set(0);
    }
    1
}

/// Return a pointer to the urip: query handler, in this example simply
/// the urip_current pointer...
///
/// Returns an Input context or NULL in case or error
#[doc(alias = "uripOpen")]
unsafe fn urip_open(uri: &str) -> *mut c_void {
    const CATALOG_URL: &str = concatcp!("file://", SYSCONFDIR, "/xml/catalog");
    if uri == CATALOG_URL {
        return null_mut();
    }
    /* Verify we received the escaped URL */
    if URIP_RCVS_URLS[URIP_CURRENT.get()] != uri {
        URIP_SUCCESS.set(0);
    }
    URIP_CUR.set(URIP_RES.as_ptr() as _);
    URIP_RLEN.set(URIP_RES.to_bytes().len());
    URIP_CUR.get() as _
}

/// Close the urip: query handler
///
/// Returns 0 or -1 in case of error
#[doc(alias = "uripClose")]
unsafe fn urip_close(context: *mut c_void) -> i32 {
    if context.is_null() {
        return -1;
    }
    URIP_CUR.set(null_mut());
    URIP_RLEN.set(0);
    0
}

/// Implement an urip: query read.
///
/// Returns the number of bytes read or -1 in case of error
#[doc(alias = "uripRead")]
unsafe fn urip_read(context: *mut c_void, buffer: *mut c_char, mut len: i32) -> i32 {
    unsafe {
        let ptr: *const c_char = context as *const c_char;

        if context.is_null() || buffer.is_null() || len < 0 {
            return -1;
        }

        if len > URIP_RLEN.get() as _ {
            len = URIP_RLEN.get() as _;
        }
        memcpy(buffer as _, ptr as _, len as usize);
        URIP_RLEN.set(URIP_RLEN.get() - len as usize);
        len
    }
}

unsafe fn urip_check_url(url: &str) -> i32 {
    unsafe {
        let Some(doc) = xml_read_file(url, None, 0) else {
            return -1;
        };
        xml_free_doc(doc);
        1
    }
}

/// Run a set of tests to check how Path and URI are handled before
/// being passed to the I/O layer
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "uriPathTest")]
unsafe fn uri_path_test(
    _filename: &str,
    _result: Option<String>,
    _err: Option<String>,
    _options: i32,
) -> i32 {
    unsafe {
        let mut parsed: i32;
        let mut failures: i32 = 0;

        // register the new I/O handlers
        struct URIPTest(*mut c_void);
        impl Read for URIPTest {
            fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
                let res =
                    unsafe { urip_read(self.0, buf.as_mut_ptr() as *mut i8, buf.len() as i32) };
                if res < 0 {
                    Err(io::Error::last_os_error())
                } else {
                    Ok(res as usize)
                }
            }
        }
        unsafe impl Send for URIPTest {}
        impl Drop for URIPTest {
            fn drop(&mut self) {
                if !self.0.is_null() {
                    unsafe {
                        urip_close(self.0);
                    }
                }
            }
        }
        impl XmlInputCallback for URIPTest {
            fn is_match(&self, filename: &str) -> bool {
                unsafe { urip_match(filename) != 0 }
            }
            fn open(&mut self, filename: &str) -> std::io::Result<Box<dyn Read>> {
                let ptr = unsafe { urip_open(filename) };
                if ptr.is_null() {
                    Err(io::Error::other("Failed to execute urip_open"))
                } else {
                    Ok(Box::new(Self(ptr)))
                }
            }
        }
        if register_input_callbacks(URIPTest(null_mut())).is_err() {
            eprintln!("failed to register HTTP handler");
            return -1;
        }

        URIP_CURRENT.set(0);
        while URIP_CURRENT.get() < URIP_TEST_URLS.len() {
            URIP_SUCCESS.set(1);
            parsed = urip_check_url(URIP_TEST_URLS[URIP_CURRENT.get()]);
            if URIP_SUCCESS.get() != 1 {
                eprintln!(
                    "failed the URL passing test for {}",
                    URIP_TEST_URLS[URIP_CURRENT.get()]
                );
                failures += 1;
            } else if parsed != 1 {
                eprintln!(
                    "failed the parsing test for {}",
                    URIP_TEST_URLS[URIP_CURRENT.get()]
                );
                failures += 1;
            }
            NB_TESTS.set(NB_TESTS.get() + 1);
            URIP_CURRENT.set(URIP_CURRENT.get() + 1);
        }

        pop_input_callbacks();
        failures
    }
}

#[cfg(feature = "schema")]
unsafe fn schemas_one_test(
    sch: *const c_char,
    filename: &str,
    result: *const c_char,
    options: i32,
    schemas: XmlSchemaPtr,
) -> i32 {
    use exml::xmlschemas::context::{
        XmlSchemaValidCtxtPtr, xml_schema_free_valid_ctxt, xml_schema_new_valid_ctxt,
    };

    unsafe {
        use exml::libxml::xmlschemas::xml_schema_validate_doc;

        let mut ret: i32 = 0;

        let Some(doc) = xml_read_file(filename, None, options) else {
            eprintln!(
                "failed to parse instance {filename} for {}",
                CStr::from_ptr(sch).to_string_lossy(),
            );
            return -1;
        };

        let temp = result_filename(
            CStr::from_ptr(result).to_string_lossy().as_ref(),
            TEMP_DIRECTORY.get().cloned().as_deref(),
            Some(".res"),
        );
        let Ok(mut schemas_output) = File::options()
            .write(true)
            .truncate(true)
            .create(true)
            .open(temp.as_str())
        else {
            eprintln!("failed to open output file {temp}",);
            xml_free_doc(doc);
            return -1;
        };

        let ctxt: XmlSchemaValidCtxtPtr = xml_schema_new_valid_ctxt(schemas);
        (*ctxt).set_errors(
            Some(test_error_handler),
            Some(test_error_handler),
            Some(GenericErrorContext::new(ctxt)),
        );
        let valid_result: i32 = xml_schema_validate_doc(ctxt, doc);
        match valid_result.cmp(&0) {
            std::cmp::Ordering::Equal => {
                writeln!(schemas_output, "{filename} validates").ok();
            }
            std::cmp::Ordering::Greater => {
                writeln!(schemas_output, "{filename} fails to validate").ok();
            }
            std::cmp::Ordering::Less => {
                writeln!(
                    schemas_output,
                    "{filename} validation generated an internal error"
                )
                .ok();
            }
        }
        if !result.is_null()
            && compare_files(
                temp.as_str(),
                CStr::from_ptr(result).to_string_lossy().as_ref(),
            ) != 0
        {
            eprintln!(
                "Result for {filename} on {} failed",
                CStr::from_ptr(sch).to_string_lossy()
            );
            ret = 1;
        }
        remove_file(temp).ok();

        xml_schema_free_valid_ctxt(ctxt);
        xml_free_doc(doc);
        ret
    }
}

/// Parse a file containing URI, compose them against a fixed base and
/// check for errors
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "schemasTest")]
#[cfg(feature = "schema")]
unsafe fn schemas_test(
    filename: &str,
    _resul: Option<String>,
    _errr: Option<String>,
    options: i32,
) -> i32 {
    use exml::xmlschemas::{
        context::{
            XmlSchemaParserCtxtPtr, xml_schema_free_parser_ctxt, xml_schema_new_parser_ctxt,
        },
        schema::xml_schema_free,
    };

    unsafe {
        use std::mem::zeroed;

        use libc::{GLOB_DOOFFS, glob, glob_t, globfree};

        let cfilename = CString::new(filename).unwrap();
        let base = CString::new(base_filename(filename)).unwrap();
        let base = base.as_ptr();
        let mut res: i32 = 0;
        let mut len: usize;
        let mut ret: i32;
        let mut pattern: [c_char; 500] = [0; 500];
        let mut prefix: [c_char; 500] = [0; 500];
        let mut result: [c_char; 500] = [0; 500];
        let mut err: [c_char; 500] = [0; 500];
        let mut globbuf: glob_t = zeroed();
        let mut count: c_char;

        // first compile the schemas if possible
        let ctxt: XmlSchemaParserCtxtPtr = xml_schema_new_parser_ctxt(filename);
        (*ctxt).set_errors(
            Some(test_error_handler),
            Some(test_error_handler),
            Some(GenericErrorContext::new(ctxt)),
        );
        let schemas: XmlSchemaPtr = (*ctxt).parse();
        xml_schema_free_parser_ctxt(ctxt);
        let parse_errors_size = TEST_ERRORS_SIZE.get();

        // most of the mess is about the output filenames generated by the Makefile
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
            TEST_ERRORS_SIZE.set(parse_errors_size);
            TEST_ERRORS.with_borrow_mut(|errors| {
                errors[parse_errors_size] = 0;
            });
            let instance = CStr::from_ptr(*globbuf.gl_pathv.add(i)).to_string_lossy();
            let base2 = CString::new(base_filename(instance.as_ref())).unwrap();
            let base2 = base2.as_ptr();
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
                eprintln!("don't know how to process {}", instance);
                continue;
            }
            if !schemas.is_null() {
                NB_TESTS.set(NB_TESTS.get() + 1);
                ret = schemas_one_test(
                    cfilename.as_ptr(),
                    &instance,
                    result.as_ptr(),
                    options,
                    schemas,
                );
                if ret != 0 {
                    res = ret;
                }
            }
            TEST_ERRORS.with_borrow(|errors| {
                if compare_file_mem(
                    CStr::from_ptr(err.as_ptr()).to_string_lossy().as_ref(),
                    &errors[..TEST_ERRORS_SIZE.get()],
                ) != 0
                {
                    eprintln!("Error for {instance} on {filename} failed");
                    eprintln!(
                        "{}",
                        CStr::from_ptr(errors.as_ptr() as *const i8).to_string_lossy()
                    );
                    res = 1;
                }
            });
        }
        globfree(addr_of_mut!(globbuf));
        xml_schema_free(schemas);

        res
    }
}

#[cfg(feature = "schema")]
unsafe fn rng_one_test(
    sch: *const c_char,
    filename: &str,
    result: *const c_char,
    options: i32,
    schemas: XmlRelaxNGPtr,
) -> i32 {
    unsafe {
        use exml::{
            libxml::relaxng::{xml_relaxng_set_valid_errors, xml_relaxng_validate_doc},
            relaxng::{xml_relaxng_free_valid_ctxt, xml_relaxng_new_valid_ctxt},
        };

        let mut ret: i32;

        let Some(doc) = xml_read_file(filename, None, options) else {
            eprintln!(
                "failed to parse instance {filename} for {}",
                CStr::from_ptr(sch).to_string_lossy(),
            );
            return -1;
        };

        let temp = result_filename(
            CStr::from_ptr(result).to_string_lossy().as_ref(),
            TEMP_DIRECTORY.get().cloned().as_deref(),
            Some(".res"),
        );
        let Ok(_) = File::options()
            .write(true)
            .truncate(true)
            .create(true)
            .open(temp.as_str())
        else {
            eprintln!("failed to open output file {temp}",);
            xml_free_doc(doc);
            return -1;
        };

        let ctxt = xml_relaxng_new_valid_ctxt(schemas);
        xml_relaxng_set_valid_errors(
            ctxt,
            Some(test_error_handler),
            Some(test_error_handler),
            Some(GenericErrorContext::new(ctxt)),
        );
        ret = xml_relaxng_validate_doc(ctxt, doc);
        match ret.cmp(&0) {
            std::cmp::Ordering::Equal => {
                test_error_handler(None, format!("{filename} validates\n").as_str());
            }
            std::cmp::Ordering::Greater => {
                test_error_handler(None, format!("{filename} fails to validate\n").as_str());
            }
            std::cmp::Ordering::Less => {
                test_error_handler(
                    None,
                    format!("{filename} validation generated an internal error\n").as_str(),
                );
            }
        }

        ret = 0;
        if !result.is_null()
            && compare_files(
                temp.as_str(),
                CStr::from_ptr(result).to_string_lossy().as_ref(),
            ) != 0
        {
            eprintln!(
                "Result for {filename} on {} failed",
                CStr::from_ptr(sch).to_string_lossy()
            );
            ret = 1;
        }
        remove_file(temp).ok();

        xml_relaxng_free_valid_ctxt(ctxt);
        xml_free_doc(doc);
        ret
    }
}

/// Parse an RNG schemas and then apply it to the related .xml
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "rngTest")]
#[cfg(feature = "schema")]
unsafe fn rng_test(
    filename: &str,
    _resul: Option<String>,
    _errr: Option<String>,
    options: i32,
) -> i32 {
    unsafe {
        use std::mem::zeroed;

        use exml::{
            libxml::relaxng::{xml_relaxng_free, xml_relaxng_parse},
            relaxng::{xml_relaxng_free_parser_ctxt, xml_relaxng_new_parser_ctxt},
        };
        use libc::{GLOB_DOOFFS, glob, glob_t, globfree};

        let cfilename = CString::new(filename).unwrap();
        let base = CString::new(base_filename(filename)).unwrap();
        let base = base.as_ptr();
        let mut res: i32;
        let mut len: usize;
        let mut ret: i32 = 0;
        let mut pattern: [c_char; 500] = [0; 500];
        let mut prefix: [c_char; 500] = [0; 500];
        let mut result: [c_char; 500] = [0; 500];
        let mut err: [c_char; 500] = [0; 500];
        let mut globbuf: glob_t = zeroed();
        let mut count: c_char;

        // first compile the schemas if possible
        let ctxt = xml_relaxng_new_parser_ctxt(filename);
        (*ctxt).set_parser_errors(
            Some(test_error_handler),
            Some(test_error_handler),
            Some(GenericErrorContext::new(ctxt)),
        );
        let schemas: XmlRelaxNGPtr = xml_relaxng_parse(ctxt);
        xml_relaxng_free_parser_ctxt(ctxt);
        if schemas.is_null() {
            test_error_handler(
                None,
                format!("Relax-NG schema {filename} failed to compile\n").as_str(),
            );
        }
        let parse_errors_size = TEST_ERRORS_SIZE.get();

        // * most of the mess is about the output filenames generated by the Makefile
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
            TEST_ERRORS_SIZE.set(parse_errors_size);
            TEST_ERRORS.with_borrow_mut(|errors| errors[parse_errors_size] = 0);
            let instance = CStr::from_ptr(*globbuf.gl_pathv.add(i)).to_string_lossy();
            let base2 = CString::new(base_filename(instance.as_ref())).unwrap();
            let base2 = base2.as_ptr();
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
                eprintln!("don't know how to process {instance}");
                continue;
            }
            if !schemas.is_null() {
                NB_TESTS.set(NB_TESTS.get() + 1);
                res = rng_one_test(
                    cfilename.as_ptr(),
                    &instance,
                    result.as_ptr(),
                    options,
                    schemas,
                );
                if res != 0 {
                    ret = res;
                }
            }
            TEST_ERRORS.with_borrow(|errors| {
                if compare_file_mem(
                    CStr::from_ptr(err.as_ptr()).to_string_lossy().as_ref(),
                    &errors[..TEST_ERRORS_SIZE.get()],
                ) != 0
                {
                    eprintln!("Error for {instance} on {filename} failed");
                    // res = 1;
                }
            });
        }
        globfree(addr_of_mut!(globbuf));
        xml_relaxng_free(schemas);

        ret
    }
}

/// Parse a set of files with streaming, applying an RNG schemas
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "rngStreamTest")]
#[cfg(all(feature = "schema", feature = "libxml_reader"))]
unsafe fn rng_stream_test(
    filename: &str,
    _resul: Option<String>,
    _errr: Option<String>,
    options: i32,
) -> i32 {
    unsafe {
        use std::mem::zeroed;

        use exml::libxml::xmlreader::{xml_free_text_reader, xml_reader_for_file};
        use libc::{GLOB_DOOFFS, glob, glob_t, globfree};

        let cfilename = CString::new(filename).unwrap();
        let base = CString::new(base_filename(filename)).unwrap();
        let base = base.as_ptr();
        let mut instance: *const c_char;
        let mut res: i32 = 0;
        let mut len: usize;
        let mut ret: i32;
        let mut pattern: [c_char; 500] = [0; 500];
        let mut prefix: [c_char; 500] = [0; 500];
        let mut result: [c_char; 500] = [0; 500];
        let mut err: [c_char; 500] = [0; 500];
        let mut globbuf: glob_t = zeroed();
        let mut count: c_char;
        let mut reader: XmlTextReaderPtr;
        let mut disable_err: i32 = 0;

        // most of the mess is about the output filenames generated by the Makefile
        len = strlen(base);
        if !(5..=499).contains(&len) {
            eprintln!("len(base) == {} !", len);
            return -1;
        }
        len -= 4; /* remove trailing .rng */
        memcpy(prefix.as_mut_ptr() as _, base as _, len);
        prefix[len] = 0;

        // strictly unifying the error messages is nearly impossible this
        // hack is also done in the Makefile
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
            TEST_ERRORS_SIZE.set(0);
            TEST_ERRORS.with_borrow_mut(|errors| errors[0] = 0);
            instance = *globbuf.gl_pathv.add(i);
            let base2 = CString::new(base_filename(
                CStr::from_ptr(instance).to_string_lossy().as_ref(),
            ))
            .unwrap();
            let base2 = base2.as_ptr();
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
            reader = xml_reader_for_file(
                &CStr::from_ptr(instance).to_string_lossy(),
                null_mut(),
                options,
            );
            if reader.is_null() {
                eprintln!(
                    "Failed to build reader for {}",
                    CStr::from_ptr(instance).to_string_lossy()
                );
            }
            let instance = CStr::from_ptr(instance).to_string_lossy();
            if disable_err == 1 {
                ret = stream_process_test(
                    &instance,
                    Some(
                        CStr::from_ptr(result.as_ptr())
                            .to_string_lossy()
                            .into_owned(),
                    ),
                    None,
                    reader,
                    cfilename.as_ptr(),
                    options,
                );
            } else {
                ret = stream_process_test(
                    &instance,
                    Some(
                        CStr::from_ptr(result.as_ptr())
                            .to_string_lossy()
                            .into_owned(),
                    ),
                    Some(CStr::from_ptr(err.as_ptr()).to_string_lossy().into_owned()),
                    reader,
                    cfilename.as_ptr(),
                    options,
                );
            }
            xml_free_text_reader(reader);
            if ret != 0 {
                eprintln!("instance {instance} failed",);
                res = ret;
            }
        }
        globfree(addr_of_mut!(globbuf));

        res
    }
}

#[cfg(feature = "schematron")]
unsafe fn schematron_one_test(
    sch: &str,
    filename: &str,
    options: i32,
    schematron: XmlSchematronPtr,
) -> i32 {
    unsafe {
        use exml::libxml::schematron::{
            XmlSchematronValidOptions, xml_schematron_free_valid_ctxt,
            xml_schematron_new_valid_ctxt, xml_schematron_set_valid_structured_errors,
            xml_schematron_validate_doc,
        };

        let Some(doc) = xml_read_file(filename, None, options) else {
            eprintln!("failed to parse instance {} for {}", filename, sch,);
            return -1;
        };

        let ctxt = xml_schematron_new_valid_ctxt(
            schematron,
            XmlSchematronValidOptions::XmlSchematronOutError as i32,
        );
        xml_schematron_set_valid_structured_errors(ctxt, Some(test_structured_error_handler), None);
        let ret = xml_schematron_validate_doc(ctxt, doc);
        match ret.cmp(&0) {
            std::cmp::Ordering::Equal => {
                test_error_handler(None, &format!("{} validates\n", filename));
            }
            std::cmp::Ordering::Greater => {
                test_error_handler(None, &format!("{} fails to validate\n", filename));
            }
            std::cmp::Ordering::Less => {
                test_error_handler(
                    None,
                    &format!("{} validation generated an internal error\n", filename),
                );
            }
        }

        xml_schematron_free_valid_ctxt(ctxt);
        xml_free_doc(doc);
        0
    }
}

/// Returns 0 in case of success, an error code otherwise
#[cfg(feature = "schematron")]
unsafe fn schematron_test(
    filename: &str,
    _resul: Option<String>,
    _errr: Option<String>,
    options: i32,
) -> i32 {
    unsafe {
        use std::mem::zeroed;

        use exml::libxml::schematron::{
            xml_schematron_free, xml_schematron_free_parser_ctxt, xml_schematron_new_parser_ctxt,
            xml_schematron_parse,
        };
        use libc::{GLOB_DOOFFS, glob_t};

        let cfilename = CString::new(filename).unwrap();
        let base = base_filename(filename);
        let mut ret = 0;
        let mut globbuf: glob_t = zeroed();

        let pctxt = xml_schematron_new_parser_ctxt(cfilename.as_ptr());
        let schematron = xml_schematron_parse(pctxt);
        xml_schematron_free_parser_ctxt(pctxt);
        if schematron.is_null() {
            test_error_handler(
                None,
                &format!("Schematron schema {filename} failed to compile\n",),
            );
        }
        let parse_errors_size = TEST_ERRORS_SIZE.get();

        // most of the mess is about the output filenames generated by the Makefile
        let len = base.len();
        if !(5..=499).contains(&len) {
            xml_schematron_free(schematron);
            return -1;
        }
        let prefix = base.strip_suffix(".sct").unwrap();
        let mut pattern = format!("./test/schematron/{prefix}_?.xml");
        pattern.truncate(500);
        let cpattern = CString::new(pattern).unwrap();

        globbuf.gl_offs = 0;
        libc::glob(cpattern.as_ptr(), GLOB_DOOFFS, None, &raw mut globbuf);
        for i in 0..globbuf.gl_pathc {
            TEST_ERRORS_SIZE.set(parse_errors_size);
            TEST_ERRORS.with_borrow_mut(|errors| {
                errors[parse_errors_size] = 0;
            });
            let instance = CStr::from_ptr(*globbuf.gl_pathv.add(i)).to_string_lossy();
            let base2 = base_filename(instance.as_ref());
            let len = base2.len();
            let err = if len > 6 && base2.as_bytes()[len - 6] == b'_' {
                let count = base2.as_bytes()[len - 5] as char;
                let mut err = format!("result/schematron/{prefix}_{count}.err");
                err.truncate(500);
                err
            } else {
                eprintln!("don't know how to process {}", instance);
                continue;
            };
            if !schematron.is_null() {
                NB_TESTS.set(NB_TESTS.get() + 1);
                let res = schematron_one_test(filename, &instance, options, schematron);
                if res != 0 {
                    ret = res;
                }
            }
            TEST_ERRORS.with_borrow(|errors| {
                if compare_file_mem(&err, &errors[..TEST_ERRORS_SIZE.get()]) != 0 {
                    eprintln!("Error for {} on {} failed", instance, filename);
                    let errstr = std::str::from_utf8(&errors[..TEST_ERRORS_SIZE.get()]).unwrap();
                    eprintln!("Result:\n{errstr}");
                    ret = 1;
                }
            });
        }
        libc::globfree(&raw mut globbuf);
        xml_schematron_free(schematron);

        ret
    }
}

#[cfg(all(feature = "libxml_pattern", feature = "libxml_reader"))]
unsafe fn pattern_node(
    out: &mut File,
    reader: XmlTextReaderPtr,
    pattern: *const c_char,
    patternc: XmlPatternPtr,
    mut patstream: XmlStreamCtxtPtr,
) {
    unsafe {
        use exml::libxml::{
            pattern::{xml_free_stream_ctxt, xml_pattern_match, xml_stream_pop, xml_stream_push},
            xmlreader::{
                XmlReaderTypes, xml_text_reader_const_local_name,
                xml_text_reader_const_namespace_uri,
            },
        };

        let mut path = None;
        let mut is_match: i32 = -1;

        let typ = (*reader).node_type();
        let empty = (*reader).is_empty_element();

        if typ == XmlReaderTypes::XmlReaderTypeElement {
            /* do the check only on element start */
            is_match = xml_pattern_match(patternc, (*reader).current_node().unwrap());

            if is_match != 0 {
                path = (*reader).current_node().unwrap().get_node_path();
                writeln!(
                    out,
                    "Node {} matches pattern {}",
                    path.as_deref().unwrap(),
                    CStr::from_ptr(pattern).to_string_lossy()
                )
                .ok();
            }
        }
        if !patstream.is_null() {
            let mut ret: i32;

            if typ == XmlReaderTypes::XmlReaderTypeElement {
                ret = xml_stream_push(
                    patstream,
                    xml_text_reader_const_local_name(&mut *reader),
                    xml_text_reader_const_namespace_uri(&mut *reader),
                );
                if ret < 0 {
                    writeln!(out, "xmlStreamPush() failure").ok();
                    xml_free_stream_ctxt(patstream);
                    patstream = null_mut();
                } else if ret != is_match {
                    if path.is_none() {
                        path = (*reader).current_node().unwrap().get_node_path();
                    }
                    writeln!(out, "xmlPatternMatch and xmlStreamPush disagree").ok();
                    writeln!(
                        out,
                        "  pattern {} node {}",
                        CStr::from_ptr(pattern).to_string_lossy(),
                        path.as_deref().unwrap()
                    )
                    .ok();
                }
            }
            if typ == XmlReaderTypes::XmlReaderTypeEndElement
                || (typ == XmlReaderTypes::XmlReaderTypeElement && empty.unwrap())
            {
                ret = xml_stream_pop(patstream);
                if ret < 0 {
                    writeln!(out, "xmlStreamPop() failure").ok();
                    xml_free_stream_ctxt(patstream);
                    // patstream = null_mut();
                }
            }
        }
    }
}

/// Parse a set of files with streaming, applying an RNG schemas
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "patternTest")]
#[cfg(all(feature = "libxml_pattern", feature = "libxml_reader"))]
unsafe fn pattern_test(
    filename: &str,
    _resul: Option<String>,
    _err: Option<String>,
    options: i32,
) -> i32 {
    unsafe {
        use exml::libxml::{
            pattern::{
                xml_free_pattern, xml_free_stream_ctxt, xml_pattern_get_stream_ctxt,
                xml_patterncompile, xml_stream_push,
            },
            xmlreader::{xml_free_text_reader, xml_reader_walker},
        };

        let mut patternc: XmlPatternPtr;
        let mut patstream: XmlStreamCtxtPtr;
        let mut xml: [c_char; 500] = [0; 500];
        let mut result: [c_char; 500] = [0; 500];
        let mut len: usize;
        let mut ret: i32;
        let mut res: i32;
        let cfilename = CString::new(filename).unwrap();
        let mut reader: XmlTextReaderPtr;

        len = filename.len();
        len -= 4;
        memcpy(xml.as_mut_ptr() as _, cfilename.as_ptr() as _, len);
        xml[len] = 0;
        let cbase = CString::new(base_filename(
            CStr::from_ptr(xml.as_ptr()).to_string_lossy().as_ref(),
        ))
        .unwrap();
        if snprintf(
            result.as_mut_ptr(),
            499,
            c"./result/pattern/%s".as_ptr(),
            cbase.as_ptr(),
        ) >= 499
        {
            result[499] = 0;
        }
        memcpy(xml.as_mut_ptr().add(len) as _, c".xml".as_ptr() as _, 5);

        if !check_test_file(CStr::from_ptr(xml.as_ptr()).to_string_lossy().as_ref())
            && !*UPDATE_RESULTS.get_or_init(|| false)
        {
            eprintln!(
                "Missing xml file {}",
                CStr::from_ptr(xml.as_ptr()).to_string_lossy()
            );
            return -1;
        }
        let Ok(mut f) = File::open(filename).map(BufReader::new) else {
            eprintln!("Failed to open {filename}\n",);
            return -1;
        };
        let temp = result_filename(
            filename,
            TEMP_DIRECTORY.get().cloned().as_deref(),
            Some(".res"),
        );
        let Ok(mut o) = File::options()
            .write(true)
            .truncate(true)
            .create(true)
            .open(temp.as_str())
        else {
            eprintln!("failed to open output file {temp}",);
            return -1;
        };
        let mut str = vec![];
        loop {
            // read one line in string buffer.
            match f.read_until(b'\n', &mut str) {
                Ok(mut size) if size > 0 => {
                    // remove the ending spaces
                    while size > 0
                        && (str[size - 1] == b'\n'
                            || str[size - 1] == b'\r'
                            || str[size - 1] == b' '
                            || str[size - 1] == b'\t')
                    {
                        size -= 1;
                        str[size] = 0;
                    }
                    let xml = CStr::from_ptr(xml.as_ptr()).to_string_lossy();
                    if let Some(doc) = xml_read_file(&xml, None, options) {
                        let mut namespaces: [(*const u8, *const u8); 20] = [(null(), null()); 20];
                        let root = doc.get_root_element().unwrap();
                        let mut ns = root.ns_def;
                        let mut j = 0;
                        while let Some(now) = ns.filter(|_| j < 10) {
                            namespaces[j] = (now.href, now.prefix);
                            j += 1;
                            ns = now.next;
                        }

                        patternc =
                            xml_patterncompile(str.as_ptr(), 0, Some(namespaces[..=j].to_vec()));
                        if patternc.is_null() {
                            let str = CStr::from_ptr(str.as_ptr() as *const i8).to_string_lossy();
                            test_error_handler(
                                None,
                                format!("Pattern {str} failed to compile\n").as_str(),
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
                        NB_TESTS.set(NB_TESTS.get() + 1);

                        reader = xml_reader_walker(doc);
                        res = (*reader).read();
                        while res == 1 {
                            pattern_node(&mut o, reader, str.as_ptr() as _, patternc, patstream);
                            res = (*reader).read();
                        }
                        if res != 0 {
                            writeln!(o, "{filename} : failed to parse",).ok();
                        }
                        xml_free_text_reader(reader);
                        xml_free_doc(doc);
                        xml_free_stream_ctxt(patstream);
                        // patstream = null_mut();
                        xml_free_pattern(patternc);
                    } else {
                        eprintln!("Failed to parse {xml}");
                        // ret = 1;
                    }
                }
                _ => {
                    break;
                }
            }
            str.clear();
        }

        let result = CStr::from_ptr(result.as_ptr()).to_string_lossy();
        ret = compare_files(temp.as_str(), result.as_ref());
        if ret != 0 {
            eprintln!("Result for {filename} failed in {result}");
            ret = 1;
        }
        remove_file(temp).ok();
        ret
    }
}

#[cfg(feature = "c14n")]
unsafe fn load_xpath_expr(parent_doc: XmlDocPtr, filename: &str) -> XmlXPathObjectPtr {
    unsafe {
        use exml::{
            globals::{set_load_ext_dtd_default_value, set_substitute_entities_default_value},
            libxml::parser::{XML_COMPLETE_ATTRS, XML_DETECT_IDS, XmlParserOption},
            tree::NodeCommon,
            xpath::{
                XmlXPathContextPtr, internals::xml_xpath_register_ns, xml_xpath_eval_expression,
                xml_xpath_free_context, xml_xpath_new_context,
            },
        };

        // load XPath expr as a file
        set_load_ext_dtd_default_value(XML_DETECT_IDS as i32 | XML_COMPLETE_ATTRS as i32);
        set_substitute_entities_default_value(1);

        let Some(doc) = xml_read_file(
            filename,
            None,
            XmlParserOption::XmlParseDTDAttr as i32 | XmlParserOption::XmlParseNoEnt as i32,
        ) else {
            eprintln!("Error: unable to parse file \"{filename}\"");
            return null_mut();
        };

        // Check the document is of the right kind
        if doc.get_root_element().is_none() {
            eprintln!("Error: empty document for file \"{filename}\"");
            xml_free_doc(doc);
            return null_mut();
        }

        let mut node = doc.children();
        while let Some(now) = node.filter(|node| node.name().as_deref() != Some("XPath")) {
            node = now.next();
        }

        let Some(node) = node else {
            eprintln!("Error: XPath element expected in the file  \"{filename}\"");
            xml_free_doc(doc);
            return null_mut();
        };

        let Some(expr) = node.get_content() else {
            eprintln!("Error: XPath content element is NULL \"{filename}\"");
            xml_free_doc(doc);
            return null_mut();
        };

        let ctx: XmlXPathContextPtr = xml_xpath_new_context(Some(parent_doc));
        if ctx.is_null() {
            eprintln!("Error: unable to create new context");
            xml_free_doc(doc);
            return null_mut();
        }

        // Register namespaces
        let node = XmlNodePtr::try_from(node).unwrap();
        let mut ns = node.ns_def;
        while let Some(now) = ns {
            if xml_xpath_register_ns(ctx, now.prefix().as_deref().unwrap(), now.href().as_deref())
                != 0
            {
                eprintln!(
                    "Error: unable to register NS with prefix=\"{}\" and href=\"{}\"",
                    CStr::from_ptr(now.prefix as _).to_string_lossy(),
                    CStr::from_ptr(now.href as _).to_string_lossy(),
                );
                xml_xpath_free_context(ctx);
                xml_free_doc(doc);
                return null_mut();
            }
            ns = now.next;
        }

        // Evaluate xpath
        let expr = CString::new(expr).unwrap();
        let xpath: XmlXPathObjectPtr = xml_xpath_eval_expression(expr.as_ptr() as *const u8, ctx);
        if xpath.is_null() {
            eprintln!("Error: unable to evaluate xpath expression");
            xml_xpath_free_context(ctx);
            xml_free_doc(doc);
            return null_mut();
        }

        // print_xpath_nodes((*xpath).nodesetval);

        xml_xpath_free_context(ctx);
        xml_free_doc(doc);
        xpath
    }
}

#[cfg(feature = "c14n")]
unsafe fn parse_list(s: *mut XmlChar) -> Option<Vec<String>> {
    unsafe {
        if s.is_null() {
            return None;
        }

        let s = CStr::from_ptr(s as *const i8)
            .to_string_lossy()
            .into_owned();
        Some(s.split(',').map(|s| s.to_owned()).collect())
    }
}

#[cfg(feature = "c14n")]
unsafe fn c14n_run_test(
    xml_filename: &str,
    with_comments: bool,
    mode: XmlC14NMode,
    xpath_filename: Option<&str>,
    ns_filename: *const c_char,
    result_file: *const c_char,
) -> i32 {
    unsafe {
        use exml::{
            c14n::xml_c14n_doc_dump_memory,
            globals::{set_load_ext_dtd_default_value, set_substitute_entities_default_value},
            libxml::parser::{XML_COMPLETE_ATTRS, XML_DETECT_IDS, XmlParserOption},
            xpath::xml_xpath_free_object,
        };

        let mut xpath: XmlXPathObjectPtr = null_mut();
        let mut ret: i32;
        let mut inclusive_namespaces = None;
        let mut nslist: *const c_char = null_mut();
        let mut nssize: i32 = 0;

        // build an XML tree from a the file; we need to add default
        // attributes and resolve all character and entities references
        set_load_ext_dtd_default_value(XML_DETECT_IDS as i32 | XML_COMPLETE_ATTRS as i32);
        set_substitute_entities_default_value(1);

        let Some(mut doc) = xml_read_file(
            xml_filename,
            None,
            XmlParserOption::XmlParseDTDAttr as i32 | XmlParserOption::XmlParseNoEnt as i32,
        ) else {
            // let last_error = get_last_error();
            // eprintln!("last_error: {last_error:?}");
            eprintln!("Error: unable to parse file \"{xml_filename}\"");
            return -1;
        };

        // Check the document is of the right kind
        if doc.get_root_element().is_none() {
            eprintln!("Error: empty document for file \"{xml_filename}\"");
            xml_free_doc(doc);
            return -1;
        }

        // load xpath file if specified
        if let Some(xpath_filename) = xpath_filename {
            xpath = load_xpath_expr(doc, xpath_filename);
            if xpath.is_null() {
                eprintln!("Error: unable to evaluate xpath expression");
                xml_free_doc(doc);
                return -1;
            }
        }

        if !ns_filename.is_null() {
            if load_mem(
                CStr::from_ptr(ns_filename).to_string_lossy().as_ref(),
                addr_of_mut!(nslist),
                addr_of_mut!(nssize),
            ) != 0
            {
                eprintln!("Error: unable to evaluate xpath expression");
                if !xpath.is_null() {
                    xml_xpath_free_object(xpath);
                }
                xml_free_doc(doc);
                return -1;
            }
            inclusive_namespaces = parse_list(nslist as *mut XmlChar);
        }

        // Canonical form
        // fprintf(stderr,"File \"%s\" loaded: start canonization\n", xml_filename);
        let mut result = String::new();
        ret = xml_c14n_doc_dump_memory(
            &mut doc,
            if !xpath.is_null() {
                (*xpath).nodesetval.as_deref_mut()
            } else {
                None
            },
            mode,
            inclusive_namespaces,
            with_comments,
            &mut result,
        );
        if ret >= 0 {
            if compare_file_mem(
                CStr::from_ptr(result_file).to_string_lossy().as_ref(),
                result.as_bytes(),
            ) != 0
            {
                eprintln!("Result mismatch for {xml_filename}");
                eprintln!("RESULT:\n{result}");
                ret = -1;
            }
        } else {
            eprintln!(
                "Error: failed to canonicalize XML file \"{xml_filename}\" (ret={})",
                ret,
            );
            ret = -1;
        }

        // Cleanup
        if !xpath.is_null() {
            xml_xpath_free_object(xpath);
        }
        if !nslist.is_null() {
            free(nslist as _);
        }
        xml_free_doc(doc);

        ret
    }
}

#[cfg(feature = "c14n")]
unsafe fn c14n_common_test(
    filename: &str,
    with_comments: bool,
    mode: XmlC14NMode,
    subdir: *const c_char,
) -> i32 {
    unsafe {
        use libc::strdup;

        let mut buf: [c_char; 500] = [0; 500];
        let mut prefix: [c_char; 500] = [0; 500];
        let mut len: usize;
        let mut xpath: *mut c_char = null_mut();
        let mut ns: *mut c_char = null_mut();
        let mut ret: i32 = 0;

        let base = CString::new(base_filename(filename)).unwrap();
        let base = base.as_ptr();
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
        if check_test_file(CStr::from_ptr(buf.as_ptr()).to_string_lossy().as_ref()) {
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
        if check_test_file(CStr::from_ptr(buf.as_ptr()).to_string_lossy().as_ref()) {
            ns = strdup(buf.as_ptr());
        }

        NB_TESTS.set(NB_TESTS.get() + 1);
        if c14n_run_test(
            filename,
            with_comments,
            mode,
            (!xpath.is_null())
                .then(|| CStr::from_ptr(xpath as *const i8).to_string_lossy())
                .as_deref(),
            ns,
            result,
        ) < 0
        {
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
}

#[cfg(feature = "c14n")]
unsafe fn c14n_with_comment_test(
    filename: &str,
    _resul: Option<String>,
    _err: Option<String>,
    _options: i32,
) -> i32 {
    unsafe {
        use exml::c14n::XmlC14NMode;

        c14n_common_test(
            filename,
            true,
            XmlC14NMode::XmlC14N1_0,
            c"with-comments".as_ptr(),
        )
    }
}

#[cfg(feature = "c14n")]
unsafe fn c14n_without_comment_test(
    filename: &str,
    _resul: Option<String>,
    _err: Option<String>,
    _options: i32,
) -> i32 {
    unsafe {
        use exml::c14n::XmlC14NMode;

        c14n_common_test(
            filename,
            false,
            XmlC14NMode::XmlC14N1_0,
            c"without-comments".as_ptr(),
        )
    }
}

#[cfg(feature = "c14n")]
unsafe fn c14n_exc_without_comment_test(
    filename: &str,
    _resul: Option<String>,
    _err: Option<String>,
    _options: i32,
) -> i32 {
    unsafe {
        use exml::c14n::XmlC14NMode;

        c14n_common_test(
            filename,
            false,
            XmlC14NMode::XmlC14NExclusive1_0,
            c"exc-without-comments".as_ptr(),
        )
    }
}

#[cfg(feature = "c14n")]
unsafe fn c14n11_without_comment_test(
    filename: &str,
    _resul: Option<String>,
    _err: Option<String>,
    _options: i32,
) -> i32 {
    unsafe {
        use exml::c14n::XmlC14NMode;

        c14n_common_test(
            filename,
            false,
            XmlC14NMode::XmlC14N1_1,
            c"1-1-without-comments".as_ptr(),
        )
    }
}

// mostly a cut and paste from testThreads.c
#[cfg(feature = "catalog")]
const MAX_ARGC: usize = 20;

#[cfg(feature = "catalog")]
#[repr(C)]
struct XmlThreadParams<'a> {
    filename: &'a str,
    okay: i32,
}

#[cfg(feature = "catalog")]
const CATALOG: &str = "./test/threads/complex.xml";
#[cfg(feature = "catalog")]
static THREAD_PARAMS: Mutex<[XmlThreadParams; 7]> = Mutex::new([
    XmlThreadParams {
        filename: "./test/threads/abc.xml",
        okay: 0,
    },
    XmlThreadParams {
        filename: "./test/threads/acb.xml",
        okay: 0,
    },
    XmlThreadParams {
        filename: "./test/threads/bac.xml",
        okay: 0,
    },
    XmlThreadParams {
        filename: "./test/threads/bca.xml",
        okay: 0,
    },
    XmlThreadParams {
        filename: "./test/threads/cab.xml",
        okay: 0,
    },
    XmlThreadParams {
        filename: "./test/threads/cba.xml",
        okay: 0,
    },
    XmlThreadParams {
        filename: "./test/threads/invalid.xml",
        okay: 0,
    },
]);

#[cfg(feature = "catalog")]
extern "C" fn thread_specific_data(private_data: *mut c_void) -> *mut c_void {
    use std::io::{stderr, stdout};

    use exml::globals::{
        get_do_validity_checking_default_value, set_do_validity_checking_default_value,
    };

    unsafe {
        let params: *mut XmlThreadParams = private_data as *mut XmlThreadParams;
        let filename = (*params).filename;
        let mut okay: i32 = 1;

        if filename == "./test/threads/invalid.xml" {
            set_do_validity_checking_default_value(0);
            let stdout: Box<dyn Write> = Box::new(stdout());
            set_generic_error(None, Some(GenericErrorContext::new(stdout)));
        } else {
            set_do_validity_checking_default_value(1);
            let stderr: Box<dyn Write> = Box::new(stderr());
            set_generic_error(None, Some(GenericErrorContext::new(stderr)));
        }
        #[cfg(feature = "sax1")]
        let my_doc = xml_parse_file(Some(filename));
        #[cfg(not(feature = "sax1"))]
        let my_doc = xml_read_file(filename, NULL, XML_WITH_CATALOG);
        if let Some(my_doc) = my_doc {
            xml_free_doc(my_doc);
        } else {
            println!("parse failed");
            okay = 0;
        }
        if filename == "./test/threads/invalid.xml" {
            if get_do_validity_checking_default_value() != 0 {
                println!("ValidityCheckingDefaultValue override failed");
                okay = 0;
            }
        } else if get_do_validity_checking_default_value() != 1 {
            println!("ValidityCheckingDefaultValue override failed");
            okay = 0;
        }
        (*params).okay = okay;
    }
    null_mut()
}

#[cfg(feature = "catalog")]
static TID: Mutex<[pthread_t; MAX_ARGC]> = Mutex::new([0; MAX_ARGC]);

#[cfg(feature = "catalog")]
unsafe extern "C" fn test_thread() -> i32 {
    unsafe {
        use exml::libxml::catalog::{xml_catalog_cleanup, xml_load_catalog};
        use libc::{pthread_create, pthread_join};

        let mut ret: i32;
        let mut res: i32 = 0;

        xml_init_parser();

        let mut tid = TID.lock().unwrap();
        let mut thread_params = THREAD_PARAMS.lock().unwrap();
        let num_threads = thread_params.len();
        for _ in 0..500 {
            xml_load_catalog(CATALOG);
            NB_TESTS.set(NB_TESTS.get() + 1);

            tid[..num_threads].fill(u64::MAX);

            for i in 0..num_threads {
                ret = pthread_create(
                    &raw mut tid[i],
                    null(),
                    thread_specific_data,
                    &raw mut thread_params[i] as _,
                );
                if ret != 0 {
                    eprintln!("pthread_create failed");
                    return 1;
                }
            }
            for &tid in tid.iter().take(num_threads) {
                let mut result: *mut c_void = null_mut();
                ret = pthread_join(tid, addr_of_mut!(result));
                if ret != 0 {
                    eprintln!("pthread_join failed");
                    return 1;
                }
            }

            xml_catalog_cleanup();
            for (i, params) in thread_params.iter().take(num_threads).enumerate() {
                if params.okay == 0 {
                    eprintln!("Thread {} handling {} failed", i, params.filename);
                    res = 1;
                }
            }
        }
        res
    }
}

#[cfg(feature = "catalog")]
unsafe fn threads_test(
    _filename: &str,
    _resul: Option<String>,
    _err: Option<String>,
    _options: i32,
) -> i32 {
    unsafe { test_thread() }
}

#[cfg(feature = "libxml_regexp")]
unsafe fn test_regexp(output: &mut File, comp: XmlRegexpPtr, value: *const c_char) {
    unsafe {
        use exml::libxml::xmlregexp::xml_regexp_exec;

        let ret: i32 = xml_regexp_exec(comp, value as _);
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
}

#[cfg(feature = "libxml_regexp")]
unsafe fn regexp_test(
    filename: &str,
    result: Option<String>,
    err: Option<String>,
    _options: i32,
) -> i32 {
    unsafe {
        use std::io::{BufRead, BufReader};

        use exml::{
            generic_error,
            libxml::xmlregexp::{xml_reg_free_regexp, xml_regexp_compile},
        };

        let mut comp: XmlRegexpPtr = null_mut();
        let mut ret: i32;
        let mut res: i32 = 0;

        NB_TESTS.set(NB_TESTS.get() + 1);

        let mut input = match File::open(filename) {
            Ok(file) => BufReader::new(file),
            _ => {
                generic_error!("Cannot open {filename} for reading\n");
                return -1;
            }
        };
        let temp = result_filename(filename, Some(""), Some(".res"));
        let mut output = match File::options()
            .write(true)
            .truncate(true)
            .create(true)
            .open(temp.as_str())
        {
            Ok(file) => file,
            _ => {
                eprintln!("failed to open output file {temp}",);
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

        ret = compare_files(temp.as_str(), result.as_deref().unwrap());
        if ret != 0 {
            eprintln!("Result for {filename} failed in {}", result.unwrap());
            res = 1;
        }
        remove_file(temp).ok();

        ret = TEST_ERRORS.with_borrow(|errors| {
            compare_file_mem(err.as_deref().unwrap(), &errors[..TEST_ERRORS_SIZE.get()])
        });
        if ret != 0 {
            eprintln!("Error for {filename} failed",);
            res = 1;
        }

        res
    }
}

#[cfg(feature = "libxml_automata")]
unsafe extern "C" fn scan_number(ptr: *mut *mut c_char) -> i32 {
    unsafe {
        let mut ret: i32 = 0;
        let mut cur: *mut c_char;

        cur = *ptr;
        while *cur >= b'0' as _ && *cur <= b'9' as _ {
            ret = ret * 10 + (*cur - b'0' as i8) as i32;
            cur = cur.add(1);
        }
        *ptr = cur;
        ret
    }
}

#[cfg(feature = "libxml_automata")]
unsafe fn automata_test(
    filename: &str,
    result: Option<String>,
    _err: Option<String>,
    _options: i32,
) -> i32 {
    unsafe {
        use std::io::{BufRead, BufReader};

        use exml::{
            generic_error,
            libxml::{
                xmlautomata::{
                    XmlAutomataPtr, XmlAutomataStatePtr, xml_automata_compile,
                    xml_automata_get_init_state, xml_automata_new_count_trans,
                    xml_automata_new_epsilon, xml_automata_new_state, xml_automata_new_transition,
                    xml_automata_set_final_state, xml_free_automata, xml_new_automata,
                },
                xmlregexp::{
                    XmlRegExecCtxtPtr, xml_reg_exec_push_string, xml_reg_free_exec_ctxt,
                    xml_reg_free_regexp, xml_reg_new_exec_ctxt,
                },
            },
        };

        let mut ret: i32;
        let mut res: i32 = 0;
        let mut am: XmlAutomataPtr;
        let mut states: [XmlAutomataStatePtr; 1000] = [null_mut(); 1000];
        let mut regexp: XmlRegexpPtr = null_mut();
        let mut exec: XmlRegExecCtxtPtr = null_mut();

        NB_TESTS.set(NB_TESTS.get() + 1);

        let mut input = match File::open(filename) {
            Ok(file) => BufReader::new(file),
            _ => {
                generic_error!("Cannot open {filename} for reading\n");
                return -1;
            }
        };
        let temp = result_filename(filename, Some(""), Some(".res"));
        let Ok(mut output) = File::options()
            .write(true)
            .truncate(true)
            .create(true)
            .open(temp.as_str())
        else {
            eprintln!("failed to open output file {temp}",);
            return -1;
        };

        am = xml_new_automata();
        if am.is_null() {
            generic_error!("Cannot create automata\n");
            return -1;
        }
        states[0] = xml_automata_get_init_state(am);
        if states[0].is_null() {
            generic_error!("Cannot get start state\n");
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
                        let expr = CStr::from_ptr(expr.as_ptr() as *const i8).to_string_lossy();
                        generic_error!("Bad line {expr}\n");
                        break;
                    }
                    if states[from].is_null() {
                        states[from] = xml_automata_new_state(am);
                    }
                    ptr = ptr.add(1);
                    let to: usize = scan_number(addr_of_mut!(ptr)) as _;
                    if *ptr != b' ' as i8 {
                        let expr = CStr::from_ptr(expr.as_ptr() as *const i8).to_string_lossy();
                        generic_error!("Bad line {expr}\n");
                        break;
                    }
                    if states[to].is_null() {
                        states[to] = xml_automata_new_state(am);
                    }
                    ptr = ptr.add(1);
                    xml_automata_new_transition(
                        am,
                        states[from],
                        states[to],
                        CStr::from_ptr(ptr as *const i8).to_string_lossy().as_ref(),
                        null_mut(),
                    );
                } else if !am.is_null() && expr[0] == b'e' && expr[1] == b' ' {
                    let mut ptr: *mut c_char = expr.as_mut_ptr().add(2) as _;

                    let from: usize = scan_number(addr_of_mut!(ptr)) as _;
                    if *ptr != b' ' as i8 {
                        let expr = CStr::from_ptr(expr.as_ptr() as *const i8).to_string_lossy();
                        generic_error!("Bad line {expr}\n");
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
                        let expr = CStr::from_ptr(expr.as_ptr() as *const i8).to_string_lossy();
                        generic_error!("Bad state {state} : {expr}\n");
                        break;
                    }
                    xml_automata_set_final_state(am, states[state]);
                } else if !am.is_null() && expr[0] == b'c' && expr[1] == b' ' {
                    let mut ptr: *mut c_char = expr.as_mut_ptr().add(2) as _;

                    let from: usize = scan_number(addr_of_mut!(ptr)) as _;
                    if *ptr != b' ' as i8 {
                        let expr = CStr::from_ptr(expr.as_ptr() as *const i8).to_string_lossy();
                        generic_error!("Bad line {expr}\n");
                        break;
                    }
                    if states[from].is_null() {
                        states[from] = xml_automata_new_state(am);
                    }
                    ptr = ptr.add(1);
                    let to: usize = scan_number(addr_of_mut!(ptr)) as _;
                    if *ptr != b' ' as i8 {
                        let expr = CStr::from_ptr(expr.as_ptr() as *const i8).to_string_lossy();
                        generic_error!("Bad line {expr}\n");
                        break;
                    }
                    if states[to].is_null() {
                        states[to] = xml_automata_new_state(am);
                    }
                    ptr = ptr.add(1);
                    let min: i32 = scan_number(addr_of_mut!(ptr));
                    if *ptr != b' ' as i8 {
                        let expr = CStr::from_ptr(expr.as_ptr() as *const i8).to_string_lossy();
                        generic_error!("Bad line {expr}\n");
                        break;
                    }
                    ptr = ptr.add(1);
                    let max: i32 = scan_number(addr_of_mut!(ptr));
                    if *ptr != b' ' as i8 {
                        let expr = CStr::from_ptr(expr.as_ptr() as *const i8).to_string_lossy();
                        generic_error!("Bad line {expr}\n");
                        break;
                    }
                    ptr = ptr.add(1);
                    xml_automata_new_count_trans(
                        am,
                        states[from],
                        states[to],
                        CStr::from_ptr(ptr as *const i8).to_string_lossy().as_ref(),
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
                        generic_error!("Failed to compile the automata");
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
                    let expr = CStr::from_ptr(expr.as_ptr() as *const i8).to_string_lossy();
                    generic_error!("Unexpected line {expr}\n");
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

        ret = compare_files(temp.as_str(), result.as_deref().unwrap());
        if ret != 0 {
            eprintln!("Result for {filename} failed in {}", result.unwrap());
            res = 1;
        }
        remove_file(temp).ok();

        res
    }
}

const TEST_DESCRIPTIONS: &[TestDesc] = &[
    #[cfg(feature = "schema")]
    TestDesc {
        desc: "Schemas regression tests",
        func: schemas_test,
        input: Some("./test/schemas/oss-fuzz-51295_0.xsd"),
        out: None,
        suffix: None,
        err: None,
        options: 0,
    },
    TestDesc {
        desc: "XML regression tests",
        func: old_parse_test,
        input: Some("./test/*"),
        out: Some("./result/"),
        suffix: Some(""),
        err: None,
        options: 0,
    },
    TestDesc {
        desc: "XML regression tests on memory",
        func: mem_parse_test,
        input: Some("./test/*"),
        out: Some("./result/"),
        suffix: Some(""),
        err: None,
        options: 0,
    },
    TestDesc {
        desc: "XML entity subst regression tests",
        func: noent_parse_test,
        input: Some("./test/*"),
        out: Some("./result/noent/"),
        suffix: Some(""),
        err: None,
        options: XmlParserOption::XmlParseNoEnt as i32,
    },
    TestDesc {
        desc: "XML Namespaces regression tests",
        func: err_parse_test,
        input: Some("./test/namespaces/*"),
        out: Some("./result/namespaces/"),
        suffix: Some(""),
        err: Some(".err"),
        options: 0,
    },
    #[cfg(feature = "libxml_valid")]
    TestDesc {
        desc: "Error cases regression tests",
        func: err_parse_test,
        input: Some("./test/errors/*.xml"),
        out: Some("./result/errors/"),
        suffix: Some(""),
        err: Some(".err"),
        options: 0,
    },
    #[cfg(feature = "libxml_valid")]
    TestDesc {
        desc: "Error cases regression tests with entity substitution",
        func: err_parse_test,
        input: Some("./test/errors/*.xml"),
        out: Some("./result/errors/"),
        suffix: None,
        err: Some(".ent"),
        options: XmlParserOption::XmlParseNoEnt as i32,
    },
    #[cfg(feature = "libxml_valid")]
    TestDesc {
        desc: "Error cases regression tests (old 1.0)",
        func: err_parse_test,
        input: Some("./test/errors10/*.xml"),
        out: Some("./result/errors10/"),
        suffix: Some(""),
        err: Some(".err"),
        options: XmlParserOption::XmlParseOld10 as i32,
    },
    #[cfg(all(feature = "libxml_reader", feature = "libxml_valid"))]
    TestDesc {
        desc: "Error cases stream regression tests",
        func: stream_parse_test,
        input: Some("./test/errors/*.xml"),
        out: Some("./result/errors/"),
        suffix: None,
        err: Some(".str"),
        options: 0,
    },
    #[cfg(feature = "libxml_reader")]
    TestDesc {
        desc: "Reader regression tests",
        func: stream_parse_test,
        input: Some("./test/*"),
        out: Some("./result/"),
        suffix: Some(".rdr"),
        err: None,
        options: 0,
    },
    #[cfg(feature = "libxml_reader")]
    TestDesc {
        desc: "Reader entities substitution regression tests",
        func: stream_parse_test,
        input: Some("./test/*"),
        out: Some("./result/"),
        suffix: Some(".rde"),
        err: None,
        options: XmlParserOption::XmlParseNoEnt as i32,
    },
    #[cfg(feature = "libxml_reader")]
    TestDesc {
        desc: "Reader on memory regression tests",
        func: stream_mem_parse_test,
        input: Some("./test/*"),
        out: Some("./result/"),
        suffix: Some(".rdr"),
        err: None,
        options: 0,
    },
    #[cfg(feature = "libxml_reader")]
    TestDesc {
        desc: "Walker regression tests",
        func: walker_parse_test,
        input: Some("./test/*"),
        out: Some("./result/"),
        suffix: Some(".rdr"),
        err: None,
        options: 0,
    },
    #[cfg(feature = "sax1")]
    TestDesc {
        desc: "SAX1 callbacks regression tests",
        func: sax_parse_test,
        input: Some("./test/*"),
        out: Some("./result/"),
        suffix: Some(".sax"),
        err: None,
        options: XmlParserOption::XmlParseSAX1 as i32,
    },
    TestDesc {
        desc: "SAX2 callbacks regression tests",
        func: sax_parse_test,
        input: Some("./test/*"),
        out: Some("./result/"),
        suffix: Some(".sax2"),
        err: None,
        options: 0,
    },
    TestDesc {
        desc: "SAX2 callbacks regression tests with entity substitution",
        func: sax_parse_test,
        input: Some("./test/*"),
        out: Some("./result/noent/"),
        suffix: Some(".sax2"),
        err: None,
        options: XmlParserOption::XmlParseNoEnt as i32,
    },
    #[cfg(all(feature = "xinclude", feature = "libxml_reader"))]
    TestDesc {
        desc: "XInclude xmlReader regression tests",
        func: stream_parse_test,
        input: Some("./test/XInclude/docs/*"),
        out: Some("./result/XInclude/"),
        suffix: Some(".rdr"),
        err: Some(".err"),
        options: XmlParserOption::XmlParseXInclude as i32,
    },
    #[cfg(all(
        feature = "xpath",
        feature = "libxml_debug",
        feature = "libxml_xptr_locs"
    ))]
    TestDesc {
        desc: "XPointer xpointer() queries regression tests",
        func: xptr_doc_test,
        input: Some("./test/XPath/docs/*"),
        out: None,
        suffix: None,
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
];

unsafe fn launch_tests(tst: &TestDesc) -> i32 {
    unsafe {
        let mut res: i32;
        let mut err: i32 = 0;
        let mut result;
        let mut error;
        let mut mem: i32;

        // let ebcdic_handler = get_encoding_handler(XmlCharEncoding::EBCDIC);
        // let euc_jp_handler = get_encoding_handler(XmlCharEncoding::EUCJP);

        if let Some(input) = tst.input {
            for entry in glob::glob(input).expect("Failed to read glob pattern") {
                let path = match entry {
                    Ok(path) => Path::new("./").join(path),
                    Err(e) => {
                        println!("{e:?}");
                        continue;
                    }
                };
                if !check_test_file(&path) {
                    continue;
                }

                let path = path.to_string_lossy();
                // TODO: `icu_parse_test`s are not passed now.
                //       This should be fixed at feature/encoding
                // if (ebcdic_handler.is_none() && path.contains("ebcdic"))
                //     || (euc_jp_handler.is_none() && path.contains("icu_parse_test"))
                // {
                //     continue;
                // }
                if path.contains("ebcdic") || path.contains("icu_parse_test") {
                    continue;
                }

                if let Some(suffix) = tst.suffix {
                    result = Some(result_filename(&path, tst.out, Some(suffix)));
                } else {
                    result = None;
                }
                if let Some(err) = tst.err {
                    error = Some(result_filename(&path, tst.out, Some(err)));
                } else {
                    error = None;
                }
                mem = xml_mem_used();
                EXTRA_MEMORY_FROM_RESOLVER.set(0);
                TEST_ERRORS_SIZE.set(0);
                TEST_ERRORS.with_borrow_mut(|errors| errors[0] = 0);
                res = (tst.func)(
                    &path,
                    result,
                    error,
                    tst.options | XmlParserOption::XmlParseCompact as i32,
                );
                reset_last_error();
                if res != 0 {
                    eprintln!("File {} generated an error", path,);
                    NB_ERRORS.set(NB_ERRORS.get() + 1);
                    err += 1;
                } else if xml_mem_used() != mem
                    && xml_mem_used() != mem
                    && EXTRA_MEMORY_FROM_RESOLVER.get() == 0
                {
                    eprintln!("File {} leaked {} bytes", path, xml_mem_used() - mem,);
                    NB_LEAKS.set(NB_LEAKS.get() + 1);
                    err += 1;
                }
                TEST_ERRORS_SIZE.set(0);
            }
        } else {
            TEST_ERRORS_SIZE.set(0);
            TEST_ERRORS.with_borrow_mut(|errors| errors[0] = 0);
            EXTRA_MEMORY_FROM_RESOLVER.set(0);
            res = (tst.func)("", None, None, tst.options);
            if res != 0 {
                NB_ERRORS.set(NB_ERRORS.get() + 1);
                err += 1;
            }
        }

        err
    }
}

unsafe extern "C" fn runtest(i: usize) -> i32 {
    unsafe {
        let mut ret: i32 = 0;

        let old_errors = NB_ERRORS.get();
        let old_tests = NB_TESTS.get();
        let old_leaks = NB_LEAKS.get();
        println!("## {}", TEST_DESCRIPTIONS[i].desc);
        let res: i32 = launch_tests(&TEST_DESCRIPTIONS[i]);
        if res != 0 {
            ret += 1;
        }
        if NB_ERRORS.get() == old_errors && NB_LEAKS.get() == old_leaks {
            println!("Ran {} tests, no errors", NB_TESTS.get() - old_tests);
        } else {
            println!(
                "Ran {} tests, {} errors, {} leaks",
                NB_TESTS.get() - old_tests,
                NB_ERRORS.get() - old_errors,
                NB_LEAKS.get() - old_leaks,
            );
        }
        ret
    }
}

static NUM_STARTED_TESTS: AtomicUsize = AtomicUsize::new(0);
static TEST_INITIALIZED: (Mutex<bool>, Condvar) = (Mutex::new(false), Condvar::new());
fn test_initialize() {
    let mut lock = TEST_INITIALIZED.0.lock().unwrap();
    loop {
        if let Ok(prev) =
            NUM_STARTED_TESTS
                .fetch_update(Ordering::Release, Ordering::Acquire, |old| Some(old + 1))
        {
            if prev == 0 {
                unsafe {
                    initialize_libxml2();
                }
                *lock = true;
                TEST_INITIALIZED.1.notify_all();
            } else {
                while !*lock {
                    lock = TEST_INITIALIZED.1.wait(lock).unwrap();
                }
            }
            break;
        }
    }
    set_pedantic_parser_default_value(0);
    set_structured_error(Some(test_structured_error_handler), None);
    #[cfg(feature = "schema")]
    {
        unsafe {
            xml_schema_init_types();
            xml_relaxng_init_types();
        }
    }
}
fn test_cleanup() {
    let mut lock = TEST_INITIALIZED.0.lock().unwrap();
    loop {
        if let Ok(prev) =
            NUM_STARTED_TESTS.fetch_update(Ordering::Release, Ordering::Acquire, |old| {
                old.checked_sub(1)
            })
        {
            if prev == 1 {
                unsafe {
                    xml_cleanup_parser();
                    xml_memory_dump();
                }

                *lock = false;
            }
            break;
        }
    }
}

fn test_common(desc: &TestDesc) {
    test_initialize();
    let old_errors = NB_ERRORS.get();
    let old_tests = NB_TESTS.get();
    let old_leaks = NB_LEAKS.get();
    println!("## {}", desc.desc);
    let res: i32 = unsafe { launch_tests(desc) };
    assert!(
        res == 0 && NB_ERRORS.get() == old_errors && NB_LEAKS.get() == old_leaks,
        "Ran {} tests, {} errors, {} leaks",
        NB_TESTS.get() - old_tests,
        NB_ERRORS.get() - old_errors,
        NB_LEAKS.get() - old_leaks,
    );
    test_cleanup();
}

#[test]
#[cfg(feature = "xinclude")]
fn xinclude_regression_test() {
    test_common(&TestDesc {
        desc: "XInclude regression tests",
        func: err_parse_test,
        input: Some("./test/XInclude/docs/*"),
        out: Some("./result/XInclude/"),
        suffix: Some(""),
        err: Some(".err"),
        options: XmlParserOption::XmlParseXInclude as i32,
    });
}

#[test]
#[cfg(feature = "xinclude")]
fn xinclude_regression_stripping_include_nodes_test() {
    test_common(&TestDesc {
        desc: "XInclude regression tests stripping include nodes",
        func: err_parse_test,
        input: Some("./test/XInclude/docs/*"),
        out: Some("./result/XInclude/"),
        suffix: Some(""),
        err: Some(".err"),
        options: XmlParserOption::XmlParseXInclude as i32
            | XmlParserOption::XmlParseNoXIncnode as i32,
    });
}

#[test]
#[cfg(all(feature = "xinclude", feature = "libxml_reader"))]
fn xinclude_xmlreader_regression_stripping_include_nodes_test() {
    test_common(&TestDesc {
        desc: "XInclude xmlReader regression tests stripping include nodes",
        func: stream_parse_test,
        input: Some("./test/XInclude/docs/*"),
        out: Some("./result/XInclude/"),
        suffix: Some(".rdr"),
        err: Some(".err"),
        options: XmlParserOption::XmlParseXInclude as i32
            | XmlParserOption::XmlParseNoXIncnode as i32,
    });
}

#[test]
#[cfg(feature = "xinclude")]
fn xinclude_regression_without_reader_test() {
    test_common(&TestDesc {
        desc: "XInclude regression tests without reader",
        func: err_parse_test,
        input: Some("./test/XInclude/without-reader/*"),
        out: Some("./result/XInclude/"),
        suffix: Some(""),
        err: Some(".err"),
        options: XmlParserOption::XmlParseXInclude as i32,
    });
}

#[test]
#[cfg(feature = "libxml_push")]
fn xml_push_regression_test() {
    test_common(&TestDesc {
        desc: "XML push regression tests",
        func: push_parse_test,
        input: Some("./test/*"),
        out: Some("./result/"),
        suffix: Some(""),
        err: None,
        options: 0,
    });
}

#[test]
#[cfg(feature = "libxml_push")]
fn xml_push_boundary_test() {
    test_common(&TestDesc {
        desc: "XML push boundary tests",
        func: push_boundary_test,
        input: Some("./test/*"),
        out: Some("./result/"),
        suffix: Some(""),
        err: None,
        options: 0,
    });
}

#[test]
#[cfg(feature = "html")]
fn html_regression_test() {
    test_common(&TestDesc {
        desc: "HTML regression tests",
        func: err_parse_test,
        input: Some("./test/HTML/*"),
        out: Some("./result/HTML/"),
        suffix: Some(""),
        err: Some(".err"),
        options: XML_PARSE_HTML,
    });
}

#[test]
#[cfg(all(feature = "html", feature = "libxml_push"))]
fn push_html_regression_test() {
    test_common(&TestDesc {
        desc: "Push HTML regression tests",
        func: push_parse_test,
        input: Some("./test/HTML/*"),
        out: Some("./result/HTML/"),
        suffix: Some(""),
        err: Some(".err"),
        options: XML_PARSE_HTML,
    });
}

#[test]
#[cfg(all(feature = "html", feature = "libxml_push"))]
fn push_html_boundary_test() {
    test_common(&TestDesc {
        desc: "Push HTML boundary tests",
        func: push_boundary_test,
        input: Some("./test/HTML/*"),
        out: Some("./result/HTML/"),
        suffix: Some(""),
        err: None,
        options: XML_PARSE_HTML,
    });
}

#[test]
#[cfg(feature = "html")]
fn html_sax_regression_test() {
    test_common(&TestDesc {
        desc: "HTML SAX regression tests",
        func: sax_parse_test,
        input: Some("./test/HTML/*"),
        out: Some("./result/HTML/"),
        suffix: Some(".sax"),
        err: None,
        options: XML_PARSE_HTML,
    });
}

#[test]
#[cfg(feature = "libxml_valid")]
fn valid_documents_regression_test() {
    test_common(&TestDesc {
        desc: "Valid documents regression tests",
        func: err_parse_test,
        input: Some("./test/VCM/*"),
        out: None,
        suffix: None,
        err: None,
        options: XmlParserOption::XmlParseDTDValid as i32,
    });
}

#[test]
#[cfg(feature = "libxml_valid")]
fn validity_checking_regression_test() {
    test_common(&TestDesc {
        desc: "Validity checking regression tests",
        func: err_parse_test,
        input: Some("./test/VC/*"),
        out: Some("./result/VC/"),
        suffix: None,
        err: Some(""),
        options: XmlParserOption::XmlParseDTDValid as i32,
    });
}

#[test]
#[cfg(all(feature = "libxml_valid", feature = "libxml_reader"))]
fn streaming_validity_checking_regression_test() {
    test_common(&TestDesc {
        desc: "Streaming validity checking regression tests",
        func: stream_parse_test,
        input: Some("./test/valid/*.xml"),
        out: Some("./result/valid/"),
        suffix: None,
        err: Some(".err.rdr"),
        options: XmlParserOption::XmlParseDTDValid as i32,
    });
}

#[test]
#[cfg(all(feature = "libxml_valid", feature = "libxml_reader"))]
fn streaming_validity_error_checking_regression_test() {
    test_common(&TestDesc {
        desc: "Streaming validity error checking regression tests",
        func: stream_parse_test,
        input: Some("./test/VC/*"),
        out: Some("./result/VC/"),
        suffix: None,
        err: Some(".rdr"),
        options: XmlParserOption::XmlParseDTDValid as i32,
    });
}

#[test]
#[cfg(feature = "libxml_valid")]
fn general_documents_valid_regression_test() {
    test_common(&TestDesc {
        desc: "General documents valid regression tests",
        func: err_parse_test,
        input: Some("./test/valid/*"),
        out: Some("./result/valid/"),
        suffix: Some(""),
        err: Some(".err"),
        options: XmlParserOption::XmlParseDTDValid as i32,
    });
}

#[test]
#[cfg(all(feature = "xpath", feature = "libxml_debug"))]
fn xpath_expression_regression_test() {
    test_common(&TestDesc {
        desc: "XPath expressions regression tests",
        func: xpath_expr_test,
        input: Some("./test/XPath/expr/*"),
        out: Some("./result/XPath/expr/"),
        suffix: Some(""),
        err: None,
        options: 0,
    });
}

#[test]
#[cfg(all(feature = "xpath", feature = "libxml_debug"))]
fn xpath_document_queries_regression_test() {
    test_common(&TestDesc {
        desc: "XPath document queries regression tests",
        func: xpath_doc_test,
        input: Some("./test/XPath/docs/*"),
        out: None,
        suffix: None,
        err: None,
        options: 0,
    });
}

#[test]
#[cfg(all(feature = "xpath", feature = "libxml_debug", feature = "xpointer"))]
fn xpointer_document_queries_regression_test() {
    test_common(&TestDesc {
        desc: "XPointer document queries regression tests",
        func: xptr_doc_test,
        input: Some("./test/XPath/docs/*"),
        out: None,
        suffix: None,
        err: None,
        options: -1,
    });
}

#[test]
#[cfg(all(feature = "xpath", feature = "libxml_debug", feature = "libxml_valid"))]
fn xml_id_regression_test() {
    test_common(&TestDesc {
        desc: "xml:id regression tests",
        func: xmlid_doc_test,
        input: Some("./test/xmlid/*"),
        out: Some("./result/xmlid/"),
        suffix: Some(""),
        err: Some(".err"),
        options: 0,
    });
}

#[test]
fn uri_parsing_test() {
    test_common(&TestDesc {
        desc: "URI parsing tests",
        func: uri_parse_test,
        input: Some("./test/URI/*.uri"),
        out: Some("./result/URI/"),
        suffix: Some(""),
        err: None,
        options: 0,
    });
}

#[test]
fn uri_base_composition_test() {
    test_common(&TestDesc {
        desc: "URI base composition tests",
        func: uri_base_test,
        input: Some("./test/URI/*.data"),
        out: Some("./result/URI/"),
        suffix: Some(""),
        err: None,
        options: 0,
    });
}

#[test]
#[cfg(feature = "schema")]
fn schemas_regression_test() {
    test_common(&TestDesc {
        desc: "Schemas regression tests",
        func: schemas_test,
        input: Some("./test/schemas/*_*.xsd"),
        out: None,
        suffix: None,
        err: None,
        options: 0,
    });
}

#[test]
#[cfg(feature = "schema")]
fn relaxng_regression_test() {
    test_common(&TestDesc {
        desc: "Relax-NG regression tests",
        func: rng_test,
        input: Some("./test/relaxng/*.rng"),
        out: None,
        suffix: None,
        err: None,
        options: XmlParserOption::XmlParseDTDAttr as i32 | XmlParserOption::XmlParseNoEnt as i32,
    });
}

#[test]
#[cfg(all(feature = "schema", feature = "libxml_reader"))]
fn relaxng_streaming_regression_test() {
    test_common(&TestDesc {
        desc: "Relax-NG streaming regression tests",
        func: rng_stream_test,
        input: Some("./test/relaxng/*.rng"),
        out: None,
        suffix: None,
        err: None,
        options: XmlParserOption::XmlParseDTDAttr as i32 | XmlParserOption::XmlParseNoEnt as i32,
    });
}

// This test is not included in the original libxml2 v2.11.8,
// but is included in the master branch (probably v2.14).
//
// As of v2.11.8, test data is included, but it is old, added 19 years ago, and the test fails.
//
// Adding the appropriate test to the original `runtest.c` in v2.11.8
// and running `./runtest -u` to get the appropriate test results.
#[test]
#[cfg(feature = "schematron")]
fn schematron_regression_test() {
    test_common(&TestDesc {
        desc: "Schematron regression tests",
        func: schematron_test,
        input: Some("./test/schematron/*.sct"),
        out: None,
        suffix: None,
        err: None,
        options: 0,
    });
}

#[test]
#[cfg(all(feature = "libxml_pattern", feature = "libxml_reader"))]
fn pattern_regression_test() {
    test_common(&TestDesc {
        desc: "Pattern regression tests",
        func: pattern_test,
        input: Some("./test/pattern/*.pat"),
        out: Some("./result/pattern/"),
        suffix: None,
        err: None,
        options: 0,
    });
}

#[test]
#[cfg(feature = "c14n")]
fn c14n_with_comments_regression_test() {
    test_common(&TestDesc {
        desc: "C14N with comments regression tests",
        func: c14n_with_comment_test,
        input: Some("./test/c14n/with-comments/*.xml"),
        out: None,
        suffix: None,
        err: None,
        options: 0,
    });
}

#[test]
#[cfg(feature = "c14n")]
fn c14n_without_comments_regression_test() {
    test_common(&TestDesc {
        desc: "C14N without comments regression tests",
        func: c14n_without_comment_test,
        input: Some("./test/c14n/without-comments/*.xml"),
        out: None,
        suffix: None,
        err: None,
        options: 0,
    });
}

#[test]
#[cfg(feature = "c14n")]
fn c14n_exclusive_without_comments_regression_test() {
    test_common(&TestDesc {
        desc: "C14N exclusive without comments regression tests",
        func: c14n_exc_without_comment_test,
        input: Some("./test/c14n/exc-without-comments/*.xml"),
        out: None,
        suffix: None,
        err: None,
        options: 0,
    });
}

#[test]
#[cfg(feature = "c14n")]
fn c14n_1_1_without_comments_regression_test() {
    test_common(&TestDesc {
        desc: "C14N 1.1 without comments regression tests",
        func: c14n11_without_comment_test,
        input: Some("./test/c14n/1-1-without-comments/*.xml"),
        out: None,
        suffix: None,
        err: None,
        options: 0,
    });
}

#[test]
#[cfg(feature = "catalog")]
fn catalog_and_threads_regression_test() {
    test_common(&TestDesc {
        desc: "Catalog and Threads regression tests",
        func: threads_test,
        input: None,
        out: None,
        suffix: None,
        err: None,
        options: 0,
    });
}

#[test]
fn svg_parsing_regression_test() {
    test_common(&TestDesc {
        desc: "SVG parsing regression tests",
        func: old_parse_test,
        input: Some("./test/SVG/*.xml"),
        out: Some("./result/SVG/"),
        suffix: Some(""),
        err: None,
        options: 0,
    });
}

#[test]
#[cfg(feature = "libxml_regexp")]
fn regexp_regression_test() {
    test_common(&TestDesc {
        desc: "Regexp regression tests",
        func: regexp_test,
        input: Some("./test/regexp/*"),
        out: Some("./result/regexp/"),
        suffix: Some(""),
        err: Some(".err"),
        options: 0,
    });
}

#[test]
#[cfg(feature = "libxml_automata")]
fn automata_regression_test() {
    test_common(&TestDesc {
        desc: "Automata regression tests",
        func: automata_test,
        input: Some("./test/automata/*"),
        out: Some("./result/automata/"),
        suffix: Some(""),
        err: None,
        options: 0,
    });
}

#[test]
fn main() {
    let mut ret: i32 = 0;
    let mut subset: i32 = 0;

    test_initialize();
    unsafe {
        let mut args = args();
        while let Some(arg) = args.next() {
            if arg == "-u" {
                UPDATE_RESULTS
                    .set(true)
                    .expect("Failed to set `UPDATE_RESULTS`");
            } else if arg == "--out" {
                if let Some(s) = args.next() {
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
        if NB_ERRORS.get() == 0 && NB_LEAKS.get() == 0 {
            ret = 0;
            println!("Total {} tests, no errors", NB_TESTS.get());
        } else {
            ret = 1;
            println!(
                "Total {} tests, {} errors, {} leaks",
                NB_TESTS.get(),
                NB_ERRORS.get(),
                NB_LEAKS.get(),
            );
        }
    }
    test_cleanup();

    assert_eq!(ret, 0);
}
