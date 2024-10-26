//! Rust implementation of original libxml2's `testlimits.c`.  
//! If you want this to work, copy the `test/` and `result/` directories from the original libxml2.

use std::{
    env::args,
    ffi::{CStr, CString},
    os::raw::c_void,
    ptr::{addr_of, null_mut},
    sync::atomic::{AtomicBool, AtomicPtr, AtomicU64, Ordering},
    time::{SystemTime, UNIX_EPOCH},
};

use exml::{
    error::{parser_print_file_context_internal, XmlError, XmlErrorDomain, XmlErrorLevel},
    globals::{set_get_warnings_default_value, set_structured_error, GenericErrorContext},
    libxml::{
        entities::XmlEntityPtr,
        parser::{
            xml_cleanup_parser, xml_ctxt_read_file, xml_free_parser_ctxt, xml_init_parser,
            xml_new_sax_parser_ctxt, xml_pedantic_parser_default, xml_set_external_entity_loader,
            XmlParserCtxtPtr, XmlParserInputPtr, XmlParserOption, XmlSAXHandler, XmlSaxlocatorPtr,
            XML_SAX2_MAGIC,
        },
        parser_internals::{XML_MAX_LOOKUP_LIMIT, XML_MAX_TEXT_LENGTH},
        tree::{xml_free_doc, XmlDocPtr, XmlElementContentPtr, XmlElementType, XmlEnumerationPtr},
        xml_io::{xml_no_net_external_entity_loader, xml_register_input_callbacks},
        xmlmemory::{
            xml_mem_free, xml_mem_malloc, xml_mem_realloc, xml_mem_setup, xml_mem_used,
            xml_memory_dump, xml_memory_strdup,
        },
        xmlstring::XmlChar,
    },
};
use libc::{memcpy, strcmp, strlen, strncmp};

static mut VERBOSE: i32 = 0;
static mut TESTS_QUIET: i32 = 0;

/* maximum time for one parsing before declaring a timeout */
const MAX_TIME: u64 = 2; /* seconds */

static T0: AtomicU64 = AtomicU64::new(0);
static TIMEOUT: AtomicBool = AtomicBool::new(false);

fn reset_timout() {
    TIMEOUT.store(false, Ordering::Release);
    T0.store(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs(),
        Ordering::Release,
    );
}

fn check_time() -> i32 {
    let tnow = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs();
    if tnow - T0.load(Ordering::Acquire) > MAX_TIME {
        TIMEOUT.store(true, Ordering::Release);
        return 0;
    }
    1
}

/*
 * Huge documents are built using fixed start and end chunks
 * and filling between the two an unconventional amount of char data
 */

struct HugeTest<'a> {
    _description: &'a str,
    name: &'a CStr,
    start: &'a CStr,
    end: &'a CStr,
}

static HUGE_TESTS: &[HugeTest] = &[
    HugeTest {
        _description: "Huge text node",
        name: c"huge:textNode",
        start: c"<foo>",
        end: c"</foo>",
    },
    HugeTest {
        _description: "Huge attribute node",
        name: c"huge:attrNode",
        start: c"<foo bar='",
        end: c"'/>",
    },
    HugeTest {
        _description: "Huge comment node",
        name: c"huge:commentNode",
        start: c"<foo><!--",
        end: c"--></foo>",
    },
    HugeTest {
        _description: "Huge PI node",
        name: c"huge:piNode",
        start: c"<foo><?bar ",
        end: c"?></foo>",
    },
];

static mut CURRENT: AtomicPtr<i8> = AtomicPtr::new(null_mut());
static mut RLEN: usize = 0;
static mut CURRENT_TEST: usize = 0;
static mut INSTATE: i32 = 0;

/**
 * hugeMatch:
 * @URI: an URI to test
 *
 * Check for an huge: query
 *
 * Returns 1 if yes and 0 if another Input module should be used
 */
unsafe extern "C" fn huge_match(uri: *const i8) -> i32 {
    if !uri.is_null() && strncmp(uri, c"huge:".as_ptr(), 5) == 0 {
        return 1;
    }
    0
}

/**
 * hugeOpen:
 * @URI: an URI to test
 *
 * Return a pointer to the huge: query handler, in this example simply
 * the current pointer...
 *
 * Returns an Input context or NULL in case or error
 */
unsafe extern "C" fn huge_open(uri: *const i8) -> *mut c_void {
    if uri.is_null() || strncmp(uri, c"huge:".as_ptr(), 5) != 0 {
        return null_mut();
    }

    CURRENT_TEST = 0;
    while CURRENT_TEST < HUGE_TESTS.len() {
        if strcmp(HUGE_TESTS[CURRENT_TEST].name.as_ptr(), uri) == 0 {
            RLEN = HUGE_TESTS[CURRENT_TEST].start.to_bytes().len();
            CURRENT.store(
                HUGE_TESTS[CURRENT_TEST].start.as_ptr() as _,
                Ordering::Relaxed,
            );
            INSTATE = 0;
            return CURRENT.load(Ordering::Relaxed) as _;
        }
        CURRENT_TEST += 1;
    }

    null_mut()
}

/**
 * hugeClose:
 * @context: the read context
 *
 * Close the huge: query handler
 *
 * Returns 0 or -1 in case of error
 */
unsafe extern "C" fn huge_close(context: *mut c_void) -> i32 {
    if context.is_null() {
        return -1;
    }
    eprintln!();
    0
}

const CHUNK: usize = 4096;

static mut FILLING: [i8; CHUNK + 1] = [0; CHUNK + 1];

unsafe extern "C" fn fill_filling() {
    for f in FILLING.iter_mut().take(CHUNK) {
        *f = b'a' as _;
    }
    FILLING[CHUNK] = 0;
}

static mut MAXLEN: usize = 64 * 1024 * 1024;
static mut CURLEN: usize = 0;
static mut DOTLEN: usize = 0;

/**
 * hugeRead:
 * @context: the read context
 * @buffer: where to store data
 * @len: number of bytes to read
 *
 * Implement an huge: query read.
 *
 * Returns the number of bytes read or -1 in case of error
 */
unsafe extern "C" fn huge_read(context: *mut c_void, buffer: *mut i8, mut len: i32) -> i32 {
    if context.is_null() || buffer.is_null() || len < 0 {
        return -1;
    }

    if INSTATE == 0 {
        if len >= RLEN as i32 {
            len = RLEN as i32;
            RLEN = 0;
            memcpy(
                buffer as _,
                CURRENT.load(Ordering::Relaxed) as _,
                len as usize,
            );
            INSTATE = 1;
            CURLEN = 0;
            DOTLEN = MAXLEN / 10;
        } else {
            memcpy(
                buffer as _,
                CURRENT.load(Ordering::Relaxed) as _,
                len as usize,
            );
            RLEN -= len as usize;
            let _ = CURRENT.fetch_update(Ordering::Relaxed, Ordering::Relaxed, |c| {
                Some(c.add(len as usize))
            });
        }
    } else if INSTATE == 2 {
        if len >= RLEN as i32 {
            len = RLEN as i32;
            RLEN = 0;
            memcpy(
                buffer as _,
                CURRENT.load(Ordering::Relaxed) as _,
                len as usize,
            );
            INSTATE = 3;
            CURLEN = 0;
        } else {
            memcpy(
                buffer as _,
                CURRENT.load(Ordering::Relaxed) as _,
                len as usize,
            );
            RLEN -= len as usize;
            let _ = CURRENT.fetch_update(Ordering::Relaxed, Ordering::Relaxed, |c| {
                Some(c.add(len as usize))
            });
        }
    } else if INSTATE == 1 {
        if len as usize > CHUNK {
            len = CHUNK as i32;
        }
        memcpy(buffer as _, FILLING.as_ptr() as _, len as usize);
        CURLEN += len as usize;
        if CURLEN >= MAXLEN {
            RLEN = strlen(HUGE_TESTS[CURRENT_TEST].end.as_ptr() as _) as _;
            CURRENT.store(
                HUGE_TESTS[CURRENT_TEST].end.as_ptr() as _,
                Ordering::Relaxed,
            );
            INSTATE = 2;
        } else if CURLEN > DOTLEN {
            print!(".");
            DOTLEN += MAXLEN / 10;
        }
    } else {
        len = 0;
    }
    len
}

/************************************************************************
 *									*
 *		Crazy document generator				*
 *									*
 ************************************************************************/

static mut CRAZY_INDX: usize = 0;

const CRAZY: &CStr = c"<?xml version='1.0' encoding='UTF-8'?><?tst ?><!-- tst --><!DOCTYPE foo [<?tst ?><!-- tst --><!ELEMENT foo (#PCDATA)><!ELEMENT p (#PCDATA|emph)* >]><?tst ?><!-- tst --><foo bar='foo'><?tst ?><!-- tst -->foo<![CDATA[ ]]></foo><?tst ?><!-- tst -->";

/**
 * crazyMatch:
 * @URI: an URI to test
 *
 * Check for a crazy: query
 *
 * Returns 1 if yes and 0 if another Input module should be used
 */
unsafe extern "C" fn crazy_match(uri: *const i8) -> i32 {
    if !uri.is_null() && strncmp(uri, c"crazy:".as_ptr(), 6) == 0 {
        return 1;
    }
    0
}

/**
 * crazyOpen:
 * @URI: an URI to test
 *
 * Return a pointer to the crazy: query handler, in this example simply
 * the current pointer...
 *
 * Returns an Input context or NULL in case or error
 */
unsafe extern "C" fn crazy_open(uri: *const i8) -> *mut c_void {
    if uri.is_null() || strncmp(uri, c"crazy:".as_ptr(), 6) != 0 {
        return null_mut();
    }

    if CRAZY_INDX > CRAZY.to_bytes().len() {
        return null_mut();
    }
    reset_timout();
    RLEN = CRAZY_INDX;
    CURRENT.store(CRAZY.as_ptr() as _, Ordering::Relaxed);
    INSTATE = 0;
    CURRENT.load(Ordering::Relaxed) as _
}

/**
 * crazyClose:
 * @context: the read context
 *
 * Close the crazy: query handler
 *
 * Returns 0 or -1 in case of error
 */
unsafe extern "C" fn crazy_close(context: *mut c_void) -> i32 {
    if context.is_null() {
        return -1;
    }
    0
}

/**
 * crazyRead:
 * @context: the read context
 * @buffer: where to store data
 * @len: number of bytes to read
 *
 * Implement an crazy: query read.
 *
 * Returns the number of bytes read or -1 in case of error
 */
unsafe extern "C" fn crazy_read(context: *mut c_void, buffer: *mut i8, mut len: i32) -> i32 {
    if context.is_null() || buffer.is_null() || len < 0 {
        return -1;
    }

    if check_time() <= 0 && INSTATE == 1 {
        eprintln!("\ntimeout in crazy({})", CRAZY_INDX);
        RLEN = CRAZY.to_bytes().len() - CRAZY_INDX;
        CURRENT.store(CRAZY.as_ptr().add(CRAZY_INDX) as _, Ordering::Relaxed);
        INSTATE = 2;
    }
    if INSTATE == 0 {
        if len >= RLEN as i32 {
            len = RLEN as i32;
            RLEN = 0;
            memcpy(buffer as _, CURRENT.load(Ordering::Relaxed) as _, len as _);
            INSTATE = 1;
            CURLEN = 0;
        } else {
            memcpy(buffer as _, CURRENT.load(Ordering::Relaxed) as _, len as _);
            RLEN -= len as usize;
            let _ = CURRENT.fetch_update(Ordering::Relaxed, Ordering::Relaxed, |c| {
                Some(c.add(len as usize))
            });
        }
    } else if INSTATE == 2 {
        if len >= RLEN as i32 {
            len = RLEN as i32;
            RLEN = 0;
            memcpy(buffer as _, CURRENT.load(Ordering::Relaxed) as _, len as _);
            INSTATE = 3;
            CURLEN = 0;
        } else {
            memcpy(buffer as _, CURRENT.load(Ordering::Relaxed) as _, len as _);
            RLEN -= len as usize;
            let _ = CURRENT.fetch_update(Ordering::Relaxed, Ordering::Relaxed, |c| {
                Some(c.add(len as usize))
            });
        }
    } else if INSTATE == 1 {
        if len as usize > CHUNK {
            len = CHUNK as i32;
        }
        memcpy(buffer as _, FILLING.as_ptr() as _, len as _);
        CURLEN += len as usize;
        if CURLEN >= MAXLEN {
            RLEN = CRAZY.to_bytes().len() - CRAZY_INDX;
            CURRENT.store(CRAZY.as_ptr().add(CRAZY_INDX) as _, Ordering::Relaxed);
            INSTATE = 2;
        }
    } else {
        len = 0;
    }
    len
}
/************************************************************************
 *									*
 *		Libxml2 specific routines				*
 *									*
 ************************************************************************/

static mut NB_TESTS: i32 = 0;
static mut NB_ERRORS: i32 = 0;
static mut NB_LEAKS: i32 = 0;
static mut EXTRA_MEMORY_FROM_RESOLVER: i32 = 0;

/*
 * We need to trap calls to the resolver to not account memory for the catalog
 * which is shared to the current running test. We also don't want to have
 * network downloads modifying tests.
 */
unsafe extern "C" fn test_external_entity_loader(
    url: *const i8,
    id: *const i8,
    ctxt: XmlParserCtxtPtr,
) -> XmlParserInputPtr {
    let memused: i32 = xml_mem_used();

    let ret: XmlParserInputPtr = xml_no_net_external_entity_loader(url, id, ctxt);
    EXTRA_MEMORY_FROM_RESOLVER += xml_mem_used() - memused;

    ret
}

/*
 * Trapping the error messages at the generic level to grab the equivalent of
 * stderr messages on CLI tools.
 */
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
            TEST_ERRORS_SIZE = TEST_ERRORS.len() - 1;
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
        if let Some(node) = node.filter(|n| n.as_ref().typ == XmlElementType::XmlElementNode) {
            name = node.as_ref().name;
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
                    let file = CStr::from_ptr((*input).filename).to_string_lossy();
                    channel(None, format!("{file}:{}: ", (*input).line).as_str());
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
                if !(*cur).filename.is_null() {
                    let file = CStr::from_ptr((*cur).filename).to_string_lossy();
                    channel(None, format!("{file}:{}: \n", (*cur).line).as_str());
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
    xml_pedantic_parser_default(0);

    xml_mem_setup(
        Some(xml_mem_free),
        Some(xml_mem_malloc),
        Some(xml_mem_realloc),
        Some(xml_memory_strdup),
    );
    xml_init_parser();
    xml_set_external_entity_loader(test_external_entity_loader);
    set_structured_error(Some(test_structured_error_handler), None);
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
        panic!("failed to register Huge handlers");
    }
    if xml_register_input_callbacks(
        Some(crazy_match),
        Some(crazy_open),
        Some(crazy_read),
        Some(crazy_close),
    ) < 0
    {
        panic!("failed to register Crazy handlers");
    }
}

/************************************************************************
 *									*
 *		SAX empty callbacks                                     *
 *									*
 ************************************************************************/

static CALLBACKS: AtomicU64 = AtomicU64::new(0);

/**
 * isStandaloneCallback:
 * @ctxt:  An XML parser context
 *
 * Is this document tagged standalone ?
 *
 * Returns 1 if true
 */
fn is_standalone_callback(_ctx: Option<GenericErrorContext>) -> i32 {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    0
}

/**
 * hasInternalSubsetCallback:
 * @ctxt:  An XML parser context
 *
 * Does this document has an internal subset
 *
 * Returns 1 if true
 */
fn has_internal_subset_callback(_ctx: Option<GenericErrorContext>) -> i32 {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    0
}

/**
 * hasExternalSubsetCallback:
 * @ctxt:  An XML parser context
 *
 * Does this document has an external subset
 *
 * Returns 1 if true
 */
fn has_external_subset_callback(_ctx: Option<GenericErrorContext>) -> i32 {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    0
}

/**
 * internalSubsetCallback:
 * @ctxt:  An XML parser context
 *
 * Does this document has an internal subset
 */
fn internal_subset_callback(
    _ctx: Option<GenericErrorContext>,
    _name: *const XmlChar,
    _external_id: *const XmlChar,
    _system_id: *const XmlChar,
) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
}

/**
 * externalSubsetCallback:
 * @ctxt:  An XML parser context
 *
 * Does this document has an external subset
 */
fn external_subset_callback(
    _ctx: Option<GenericErrorContext>,
    _name: *const XmlChar,
    _external_id: *const XmlChar,
    _system_id: *const XmlChar,
) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
}

/**
 * resolveEntityCallback:
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
fn resolve_entity_callback(
    _ctx: Option<GenericErrorContext>,
    _public_id: *const XmlChar,
    _system_id: *const XmlChar,
) -> XmlParserInputPtr {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    null_mut()
}

/**
 * getEntityCallback:
 * @ctxt:  An XML parser context
 * @name: The entity name
 *
 * Get an entity by name
 *
 * Returns the xmlParserInputPtr if inlined or NULL for DOM behaviour.
 */
fn get_entity_callback(_ctx: Option<GenericErrorContext>, _name: *const XmlChar) -> XmlEntityPtr {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    null_mut()
}

/**
 * getParameterEntityCallback:
 * @ctxt:  An XML parser context
 * @name: The entity name
 *
 * Get a parameter entity by name
 *
 * Returns the xmlParserInputPtr
 */
fn get_parameter_entity_callback(
    _ctx: Option<GenericErrorContext>,
    _name: *const XmlChar,
) -> XmlEntityPtr {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    null_mut()
}

/**
 * entityDeclCallback:
 * @ctxt:  An XML parser context
 * @name:  the entity name
 * @type:  the entity type
 * @publicId: The public ID of the entity
 * @systemId: The system ID of the entity
 * @content: the entity value (without processing).
 *
 * An entity definition has been parsed
 */
fn entity_decl_callback(
    _ctx: Option<GenericErrorContext>,
    _name: *const XmlChar,
    _typ: i32,
    _public_id: *const XmlChar,
    _system_id: *const XmlChar,
    _content: *mut XmlChar,
) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
}

/**
 * attributeDeclCallback:
 * @ctxt:  An XML parser context
 * @name:  the attribute name
 * @type:  the attribute type
 *
 * An attribute definition has been parsed
 */
fn attribute_decl_callback(
    _ctx: Option<GenericErrorContext>,
    _elem: *const XmlChar,
    _name: *const XmlChar,
    _typ: i32,
    _def: i32,
    _default_value: *const XmlChar,
    _tree: XmlEnumerationPtr,
) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
}

/**
 * elementDeclCallback:
 * @ctxt:  An XML parser context
 * @name:  the element name
 * @type:  the element type
 * @content: the element value (without processing).
 *
 * An element definition has been parsed
 */
fn element_decl_callback(
    _ctx: Option<GenericErrorContext>,
    _name: *const XmlChar,
    _typ: i32,
    _content: XmlElementContentPtr,
) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
}

/**
 * notationDeclCallback:
 * @ctxt:  An XML parser context
 * @name: The name of the notation
 * @publicId: The public ID of the entity
 * @systemId: The system ID of the entity
 *
 * What to do when a notation declaration has been parsed.
 */
fn notation_decl_callback(
    _ctx: Option<GenericErrorContext>,
    _name: *const XmlChar,
    _public_id: *const XmlChar,
    _system_id: *const XmlChar,
) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
}

/**
 * unparsedEntityDeclCallback:
 * @ctxt:  An XML parser context
 * @name: The name of the entity
 * @publicId: The public ID of the entity
 * @systemId: The system ID of the entity
 * @notationName: the name of the notation
 *
 * What to do when an unparsed entity declaration is parsed
 */
fn unparsed_entity_decl_callback(
    _ctx: Option<GenericErrorContext>,
    _name: *const XmlChar,
    _public_id: *const XmlChar,
    _system_id: *const XmlChar,
    _notation_name: *const XmlChar,
) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
}

/**
 * setDocumentLocatorCallback:
 * @ctxt:  An XML parser context
 * @loc: A SAX Locator
 *
 * Receive the document locator at startup, actually xmlDefaultSAXLocator
 * Everything is available on the context, so this is useless in our case.
 */
fn set_document_locator_callback(_ctx: Option<GenericErrorContext>, _loc: XmlSaxlocatorPtr) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
}

/**
 * startDocumentCallback:
 * @ctxt:  An XML parser context
 *
 * called when the document start being processed.
 */
fn start_document_callback(_ctx: Option<GenericErrorContext>) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
}

/**
 * endDocumentCallback:
 * @ctxt:  An XML parser context
 *
 * called when the document end has been detected.
 */
fn end_document_callback(_ctx: Option<GenericErrorContext>) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
}

/**
 * charactersCallback:
 * @ctxt:  An XML parser context
 * @ch:  a xmlChar string
 * @len: the number of xmlChar
 *
 * receiving some chars from the parser.
 * Question: how much at a time ???
 */
fn characters_callback(_ctx: Option<GenericErrorContext>, _ch: *const XmlChar, _len: i32) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
}

/**
 * referenceCallback:
 * @ctxt:  An XML parser context
 * @name:  The entity name
 *
 * called when an entity reference is detected.
 */
fn reference_callback(_ctx: Option<GenericErrorContext>, _name: *const XmlChar) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
}

/**
 * ignorableWhitespaceCallback:
 * @ctxt:  An XML parser context
 * @ch:  a xmlChar string
 * @start: the first char in the string
 * @len: the number of xmlChar
 *
 * receiving some ignorable whitespaces from the parser.
 * Question: how much at a time ???
 */
fn ignorable_whitespace_callback(
    _ctx: Option<GenericErrorContext>,
    _ch: *const XmlChar,
    _len: i32,
) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
}

/**
 * processingInstructionCallback:
 * @ctxt:  An XML parser context
 * @target:  the target name
 * @data: the PI data's
 * @len: the number of xmlChar
 *
 * A processing instruction has been parsed.
 */
fn processing_instruction_callback(
    _ctx: Option<GenericErrorContext>,
    _target: *const XmlChar,
    _data: *const XmlChar,
) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
}

/**
 * cdataBlockCallback:
 * @ctx: the user data (XML parser context)
 * @value:  The pcdata content
 * @len:  the block length
 *
 * called when a pcdata block has been parsed
 */
fn cdata_block_callback(_ctx: Option<GenericErrorContext>, _value: *const XmlChar, _len: i32) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
}

/**
 * commentCallback:
 * @ctxt:  An XML parser context
 * @value:  the comment content
 *
 * A comment has been parsed.
 */
fn comment_callback(_ctx: Option<GenericErrorContext>, _value: *const XmlChar) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
}

/**
 * warningCallback:
 * @ctxt:  An XML parser context
 * @msg:  the message to display/transmit
 * @...:  extra parameters for the message display
 *
 * Display and format a warning messages, gives file, line, position and
 * extra parameters.
 */
fn warning_callback(_ctx: Option<GenericErrorContext>, _msg: &str) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
}

/**
 * errorCallback:
 * @ctxt:  An XML parser context
 * @msg:  the message to display/transmit
 * @...:  extra parameters for the message display
 *
 * Display and format a error messages, gives file, line, position and
 * extra parameters.
 */
fn error_callback(_ctx: Option<GenericErrorContext>, _msg: &str) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
}

/**
 * fatalErrorCallback:
 * @ctxt:  An XML parser context
 * @msg:  the message to display/transmit
 * @...:  extra parameters for the message display
 *
 * Display and format a fatalError messages, gives file, line, position and
 * extra parameters.
 */
fn fatal_error_callback(_ctx: Option<GenericErrorContext>, _msg: &str) {}

/*
 * SAX2 specific callbacks
 */

/**
 * startElementNsCallback:
 * @ctxt:  An XML parser context
 * @name:  The element name
 *
 * called when an opening tag has been processed.
 */
#[allow(clippy::too_many_arguments)]
fn start_element_ns_callback(
    _ctx: Option<GenericErrorContext>,
    _localname: *const XmlChar,
    _prefix: *const XmlChar,
    _uri: *const XmlChar,
    _nb_namespaces: i32,
    _namespaces: *mut *const XmlChar,
    _nb_attributes: i32,
    _nb_defaulted: i32,
    _attributes: *mut *const XmlChar,
) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
}

/**
 * endElementCallback:
 * @ctxt:  An XML parser context
 * @name:  The element name
 *
 * called when the end of an element has been detected.
 */
fn end_element_ns_callback(
    _ctx: Option<GenericErrorContext>,
    _localname: *const XmlChar,
    _prefix: *const XmlChar,
    _uri: *const XmlChar,
) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
}

static CALLBACK_SAX2_HANDLER_STRUCT: XmlSAXHandler = XmlSAXHandler {
    internal_subset: Some(internal_subset_callback),
    is_standalone: Some(is_standalone_callback),
    has_internal_subset: Some(has_internal_subset_callback),
    has_external_subset: Some(has_external_subset_callback),
    resolve_entity: Some(resolve_entity_callback),
    get_entity: Some(get_entity_callback),
    entity_decl: Some(entity_decl_callback),
    notation_decl: Some(notation_decl_callback),
    attribute_decl: Some(attribute_decl_callback),
    element_decl: Some(element_decl_callback),
    unparsed_entity_decl: Some(unparsed_entity_decl_callback),
    set_document_locator: Some(set_document_locator_callback),
    start_document: Some(start_document_callback),
    end_document: Some(end_document_callback),
    start_element: None,
    end_element: None,
    reference: Some(reference_callback),
    characters: Some(characters_callback),
    ignorable_whitespace: Some(ignorable_whitespace_callback),
    processing_instruction: Some(processing_instruction_callback),
    comment: Some(comment_callback),
    warning: Some(warning_callback),
    error: Some(error_callback),
    fatal_error: Some(fatal_error_callback),
    get_parameter_entity: Some(get_parameter_entity_callback),
    cdata_block: Some(cdata_block_callback),
    external_subset: Some(external_subset_callback),
    initialized: XML_SAX2_MAGIC as _,
    _private: AtomicPtr::new(null_mut()),
    start_element_ns: Some(start_element_ns_callback),
    end_element_ns: Some(end_element_ns_callback),
    serror: None,
};

/**
 * readerTest:
 * @filename: the file to parse
 * @max_size: size of the limit to test
 * @options: parsing options
 * @fail: should a failure be reported
 *
 * Parse a memory generated file using SAX
 *
 * Returns 0 in case of success, an error code otherwise
 */
unsafe extern "C" fn sax_test(filename: *const i8, limit: usize, options: i32, fail: i32) -> i32 {
    let res: i32;

    NB_TESTS += 1;

    MAXLEN = limit;
    let ctxt: XmlParserCtxtPtr =
        xml_new_sax_parser_ctxt(addr_of!(CALLBACK_SAX2_HANDLER_STRUCT), None);
    if ctxt.is_null() {
        eprintln!("Failed to create parser context");
        return 1;
    }
    let doc: XmlDocPtr = xml_ctxt_read_file(ctxt, filename, null_mut(), options);

    if !doc.is_null() {
        eprintln!("SAX parsing generated a document !");
        xml_free_doc(doc);
        res = 0;
    } else if (*ctxt).well_formed == 0 {
        if fail != 0 {
            res = 0;
        } else {
            eprintln!(
                "Failed to parse '{}' {limit}",
                CStr::from_ptr(filename).to_string_lossy()
            );
            res = 1;
        }
    } else if fail != 0 {
        eprintln!(
            "Failed to get failure for '{}' {limit}",
            CStr::from_ptr(filename).to_string_lossy()
        );
        res = 1;
    } else {
        res = 0;
    }
    xml_free_parser_ctxt(ctxt);

    res
}

/**
 * readerTest:
 * @filename: the file to parse
 * @max_size: size of the limit to test
 * @options: parsing options
 * @fail: should a failure be reported
 *
 * Parse a memory generated file using the xmlReader
 *
 * Returns 0 in case of success, an error code otherwise
 */
#[cfg(feature = "libxml_reader")]
unsafe extern "C" fn reader_test(
    filename: *const i8,
    limit: usize,
    options: i32,
    fail: i32,
) -> i32 {
    use exml::libxml::xmlreader::{
        xml_free_text_reader, xml_reader_for_file, xml_text_reader_read, XmlTextReaderPtr,
    };

    let mut res: i32;

    NB_TESTS += 1;

    MAXLEN = limit;
    let reader: XmlTextReaderPtr = xml_reader_for_file(filename, null_mut(), options);
    if reader.is_null() {
        eprintln!(
            "Failed to open '{}' test",
            CStr::from_ptr(filename).to_string_lossy()
        );
        return 1;
    }
    let mut ret = xml_text_reader_read(reader);
    while ret == 1 {
        ret = xml_text_reader_read(reader);
    }
    if ret != 0 {
        if fail != 0 {
            res = 0;
        } else {
            if strncmp(filename, c"crazy:".as_ptr(), 6) == 0 {
                eprintln!(
                    "Failed to parse '{}' {CRAZY_INDX}",
                    CStr::from_ptr(filename).to_string_lossy()
                );
            } else {
                eprintln!(
                    "Failed to parse '{}' {limit}",
                    CStr::from_ptr(filename).to_string_lossy()
                );
            }
            res = 1;
        }
    } else if fail != 0 {
        if strncmp(filename, c"crazy:".as_ptr(), 6) == 0 {
            eprintln!(
                "Failed to get failure for '{}' {CRAZY_INDX}",
                CStr::from_ptr(filename).to_string_lossy()
            );
        } else {
            eprintln!(
                "Failed to get failure for '{}' {limit}",
                CStr::from_ptr(filename).to_string_lossy()
            );
        }
        res = 1;
    } else {
        res = 0;
    }
    if TIMEOUT.load(Ordering::Acquire) {
        res = 1;
    }
    xml_free_text_reader(reader);

    res
}

/************************************************************************
 *									*
 *			Tests descriptions				*
 *									*
 ************************************************************************/

type Functest =
    unsafe extern "C" fn(filename: *const i8, limit: usize, options: i32, fail: i32) -> i32;

struct LimitDesc<'a> {
    name: &'a str, /* the huge generator name */
    limit: usize,  /* the limit to test */
    options: i32,  /* extra parser options */
    fail: i32,     /* whether the test should fail */
}

static LIMIT_DESCRIPTIONS: &[LimitDesc] = &[
    /* max length of a text node in content */
    LimitDesc {
        name: "huge:textNode",
        limit: XML_MAX_TEXT_LENGTH - CHUNK,
        options: 0,
        fail: 0,
    },
    LimitDesc {
        name: "huge:textNode",
        limit: XML_MAX_TEXT_LENGTH + CHUNK,
        options: 0,
        fail: 1,
    },
    LimitDesc {
        name: "huge:textNode",
        limit: XML_MAX_TEXT_LENGTH + CHUNK,
        options: XmlParserOption::XmlParseHuge as i32,
        fail: 0,
    },
    /* max length of a text node in content */
    LimitDesc {
        name: "huge:attrNode",
        limit: XML_MAX_TEXT_LENGTH - CHUNK,
        options: 0,
        fail: 0,
    },
    LimitDesc {
        name: "huge:attrNode",
        limit: XML_MAX_TEXT_LENGTH + CHUNK,
        options: 0,
        fail: 1,
    },
    LimitDesc {
        name: "huge:attrNode",
        limit: XML_MAX_TEXT_LENGTH + CHUNK,
        options: XmlParserOption::XmlParseHuge as i32,
        fail: 0,
    },
    /* max length of a comment node */
    LimitDesc {
        name: "huge:commentNode",
        limit: XML_MAX_TEXT_LENGTH - CHUNK,
        options: 0,
        fail: 0,
    },
    LimitDesc {
        name: "huge:commentNode",
        limit: XML_MAX_TEXT_LENGTH + CHUNK,
        options: 0,
        fail: 1,
    },
    LimitDesc {
        name: "huge:commentNode",
        limit: XML_MAX_TEXT_LENGTH + CHUNK,
        options: XmlParserOption::XmlParseHuge as i32,
        fail: 0,
    },
    /* max length of a PI node */
    LimitDesc {
        name: "huge:piNode",
        limit: XML_MAX_TEXT_LENGTH - CHUNK,
        options: 0,
        fail: 0,
    },
    LimitDesc {
        name: "huge:piNode",
        limit: XML_MAX_TEXT_LENGTH + CHUNK,
        options: 0,
        fail: 1,
    },
    LimitDesc {
        name: "huge:piNode",
        limit: XML_MAX_TEXT_LENGTH + CHUNK,
        options: XmlParserOption::XmlParseHuge as i32,
        fail: 0,
    },
];

#[derive(Debug, Clone, Copy)]
struct TestDesc<'a> {
    desc: Option<&'a str>,  /* description of the test */
    func: Option<Functest>, /* function implementing the test */
}

static TEST_DESCRIPTIONS: &[TestDesc] = &[
    TestDesc {
        desc: Some("Parsing of huge files with the sax parser"),
        func: Some(sax_test),
    },
    /*    { "Parsing of huge files with the tree parser", treeTest}, */
    #[cfg(feature = "libxml_reader")]
    TestDesc {
        desc: Some("Parsing of huge files with the reader"),
        func: Some(reader_test),
    },
    TestDesc {
        desc: None,
        func: None,
    },
];

struct TestException {
    test: u32,   /* the parser test number */
    limit: u32,  /* the limit test number */
    fail: i32,   /* new fail value or -1*/
    size: usize, /* new limit value or 0 */
}

static TEST_EXCEPTIONS: &[TestException] = &[
    /* the SAX parser doesn't hit a limit of XML_MAX_TEXT_LENGTH text nodes */
    TestException {
        test: 0,
        limit: 1,
        fail: 0,
        size: 0,
    },
];

unsafe extern "C" fn launch_tests(tst: &TestDesc, test: u32) -> i32 {
    let mut res: i32;
    let mut err: i32 = 0;
    let mut limit: usize;
    let mut fail: i32;

    for (i, descr) in LIMIT_DESCRIPTIONS.iter().enumerate() {
        limit = descr.limit;
        fail = descr.fail;
        /*
         * Handle exceptions if any
         */
        for exception in TEST_EXCEPTIONS {
            if exception.test == test && exception.limit == i as _ {
                if exception.fail != -1 {
                    fail = exception.fail;
                }
                if exception.size != 0 {
                    limit = exception.size;
                }
                break;
            }
        }
        let name = CString::new(descr.name).unwrap();
        res = tst.func.unwrap()(name.as_ptr() as _, limit, descr.options, fail);
        if res != 0 {
            NB_ERRORS += 1;
            err += 1;
        }
    }
    err
}

unsafe extern "C" fn runtest(i: u32) -> i32 {
    let mut ret: i32 = 0;

    let old_errors: i32 = NB_ERRORS;
    let old_tests: i32 = NB_TESTS;
    let old_leaks: i32 = NB_LEAKS;
    if TESTS_QUIET == 0 && TEST_DESCRIPTIONS[i as usize].desc.is_some() {
        println!("## {}", TEST_DESCRIPTIONS[i as usize].desc.unwrap());
    }
    let tmp = TEST_DESCRIPTIONS[i as usize];
    let res: i32 = launch_tests(&tmp, i);
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

unsafe extern "C" fn launch_crazy_sax(test: u32, fail: i32) -> i32 {
    let mut err: i32 = 0;

    CRAZY_INDX = test as _;

    let res = sax_test(
        c"crazy::test".as_ptr(),
        XML_MAX_LOOKUP_LIMIT - CHUNK,
        0,
        fail,
    );
    if res != 0 {
        NB_ERRORS += 1;
        err += 1;
    }
    if TESTS_QUIET == 0 {
        eprintln!("{}", CRAZY.to_bytes()[test as usize] as char);
    }

    err
}

#[cfg(feature = "libxml_reader")]
unsafe extern "C" fn launch_crazy(test: u32, fail: i32) -> i32 {
    let mut err: i32 = 0;

    CRAZY_INDX = test as _;

    let res = reader_test(
        c"crazy::test".as_ptr(),
        XML_MAX_LOOKUP_LIMIT - CHUNK,
        0,
        fail,
    );
    if res != 0 {
        NB_ERRORS += 1;
        err += 1;
    }
    if TESTS_QUIET == 0 {
        eprintln!("{}", CRAZY.to_bytes()[test as usize] as char);
    }

    err
}

unsafe extern "C" fn get_crazy_fail(test: i32) -> i32 {
    /*
     * adding 1000000 of character 'a' leads to parser failure mostly
     * everywhere except in those special spots. Need to be updated
     * each time crazy is updated
     */
    1 - (test == 44 || /* PI in Misc */
        (50..=55).contains(&test) || /* Comment in Misc */
        test == 79 || /* PI in DTD */
        (85..=90).contains(&test) || /* Comment in DTD */
        test == 154 || /* PI in Misc */
        (160..=165).contains(&test) || /* Comment in Misc */
        (178..=181).contains(&test) || /* attribute value */
        test == 183 || /* Text */
        test == 189 || /* PI in Content */
        test == 191 || /* Text */
        (195..=200).contains(&test) || /* Comment in Content */
        (203..=206).contains(&test) || /* Text */
        test == 215 || test == 216 || /* in CDATA */
        test == 219 || /* Text */
        test == 231 || /* PI in Misc */
        (237..=242).contains(&test)/* Comment in Misc */) as i32
}

unsafe extern "C" fn runcrazy() -> i32 {
    let mut ret: i32 = 0;
    let mut res: i32 = 0;

    let old_errors: i32 = NB_ERRORS;
    let old_tests: i32 = NB_TESTS;
    let old_leaks: i32 = NB_LEAKS;

    #[cfg(feature = "libxml_reader")]
    {
        if TESTS_QUIET == 0 {
            println!("## Crazy tests on reader");
        }
        for i in 0..CRAZY.to_bytes().len() {
            res += launch_crazy(i as _, get_crazy_fail(i as _));
            if res != 0 {
                ret += 1;
            }
        }
    }

    if TESTS_QUIET == 0 {
        println!("\n## Crazy tests on SAX");
    }
    for i in 0..CRAZY.to_bytes().len() {
        res += launch_crazy_sax(i as _, get_crazy_fail(i as _));
        if res != 0 {
            ret += 1;
        }
    }
    if TESTS_QUIET == 0 {
        eprintln!();
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
    let ret: i32;
    let mut subset: i32 = 0;

    unsafe {
        fill_filling();
        initialize_libxml2();

        for arg in args() {
            if arg == "-v" {
                VERBOSE = 1;
            } else if arg == "-quiet" {
                TESTS_QUIET = 1;
            } else if arg == "-crazy" {
                subset = 1;
            }
        }
        if subset == 0 {
            for i in (0..).take_while(|&i| TEST_DESCRIPTIONS[i].func.is_some()) {
                assert_eq!(runtest(i as _), 0, "Failed to pass run_test({i})");
            }
        }
        assert_eq!(runcrazy(), 0, "Failed to pass runcrazy()");
        if NB_ERRORS == 0 && NB_LEAKS == 0 {
            ret = 0;
            println!("Total {} tests, no errors", NB_TESTS);
        } else {
            ret = 1;
            println!(
                "Total {} tests, {} errors, {} leaks\n",
                NB_TESTS, NB_ERRORS, NB_LEAKS,
            );
        }
        xml_cleanup_parser();
        xml_memory_dump();
    }

    assert_eq!(ret, 0);
}
