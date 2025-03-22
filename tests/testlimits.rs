//! Rust implementation of original libxml2's `testlimits.c`.  
//! If you want this to work, copy the `test/` and `result/` directories from the original libxml2.

use std::{
    cell::{Cell, RefCell},
    ffi::CStr,
    io::{self, Read},
    os::raw::c_void,
    ptr::null_mut,
    sync::atomic::{AtomicPtr, AtomicU64, AtomicUsize, Ordering},
    time::{SystemTime, UNIX_EPOCH},
};

use exml::{
    error::{XmlError, XmlErrorDomain, XmlErrorLevel, parser_print_file_context_internal},
    globals::{
        GenericErrorContext, set_get_warnings_default_value, set_pedantic_parser_default_value,
        set_structured_error,
    },
    io::{XmlInputCallback, register_input_callbacks, xml_no_net_external_entity_loader},
    libxml::{
        parser::{
            XML_SAX2_MAGIC, XmlParserOption, XmlSAXHandler, XmlSAXLocatorPtr, xml_cleanup_parser,
            xml_init_parser, xml_set_external_entity_loader,
        },
        parser_internals::{XML_MAX_LOOKUP_LIMIT, XML_MAX_TEXT_LENGTH},
        xmlmemory::{
            xml_mem_free, xml_mem_malloc, xml_mem_realloc, xml_mem_setup, xml_mem_used,
            xml_memory_dump, xml_memory_strdup,
        },
        xmlstring::XmlChar,
    },
    parser::{
        XmlParserCtxtPtr, XmlParserInputPtr, xml_ctxt_read_file, xml_free_parser_ctxt,
        xml_new_sax_parser_ctxt,
    },
    tree::{
        XmlAttributeDefault, XmlAttributeType, XmlElementContentPtr, XmlElementType,
        XmlElementTypeVal, XmlEntityPtr, XmlEntityType, XmlEnumeration, XmlNodePtr, xml_free_doc,
    },
};
use libc::{memcpy, strlen};

// maximum time for one parsing before declaring a timeout
const MAX_TIME: u64 = 2; /* seconds */

thread_local! {
    static T0: Cell<u64> = const { Cell::new(0) };
    static TIMEOUT: Cell<bool> = const { Cell::new(false) };
}

fn reset_timout() {
    TIMEOUT.set(false);
    T0.set(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs(),
    );
}

fn check_time() -> i32 {
    let tnow = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs();
    if tnow - T0.get() > MAX_TIME {
        TIMEOUT.set(true);
        return 0;
    }
    1
}

// Huge documents are built using fixed start and end chunks
// and filling between the two an unconventional amount of char data

struct HugeTest<'a> {
    _description: &'a str,
    name: &'a str,
    start: &'a CStr,
    end: &'a CStr,
}

static HUGE_TESTS: &[HugeTest] = &[
    HugeTest {
        _description: "Huge text node",
        name: "huge:textNode",
        start: c"<foo>",
        end: c"</foo>",
    },
    HugeTest {
        _description: "Huge attribute node",
        name: "huge:attrNode",
        start: c"<foo bar='",
        end: c"'/>",
    },
    HugeTest {
        _description: "Huge comment node",
        name: "huge:commentNode",
        start: c"<foo><!--",
        end: c"--></foo>",
    },
    HugeTest {
        _description: "Huge PI node",
        name: "huge:piNode",
        start: c"<foo><?bar ",
        end: c"?></foo>",
    },
];

struct DocumentContext {
    current: *const i8,
    rlen: usize,
    current_test: usize,
    instate: i32,
    maxlen: usize,
    curlen: usize,
    dotlen: usize,
}

impl Default for DocumentContext {
    fn default() -> Self {
        Self {
            current: null_mut(),
            rlen: 0,
            current_test: 0,
            instate: 0,
            maxlen: 64 * 1024 * 1024,
            curlen: 0,
            dotlen: 0,
        }
    }
}

thread_local! {
    static DOCUMENT_CONTEXT: RefCell<DocumentContext> = RefCell::new(DocumentContext::default());
}

/// Check for an huge: query
///
/// Returns 1 if yes and 0 if another Input module should be used
#[doc(alias = "hugeMatch")]
fn huge_match(uri: &str) -> i32 {
    uri.starts_with("huge:") as i32
}

/// Return a pointer to the huge: query handler, in this example simply
/// the current pointer...
///
/// Returns an Input context or NULL in case or error
#[doc(alias = "hugeOpen")]
fn huge_open(uri: &str) -> *mut c_void {
    if !uri.starts_with("huge:") {
        return null_mut();
    }

    DOCUMENT_CONTEXT.with_borrow_mut(|context| {
        context.current_test = 0;
        while context.current_test < HUGE_TESTS.len() {
            if HUGE_TESTS[context.current_test].name == uri {
                context.rlen = HUGE_TESTS[context.current_test].start.to_bytes().len();
                context.current = HUGE_TESTS[context.current_test].start.as_ptr() as _;
                context.instate = 0;
                return context.current as *mut c_void;
            }
            context.current_test += 1;
        }
        null_mut()
    })
}

/// Close the huge: query handler
///
/// Returns 0 or -1 in case of error
#[doc(alias = "hugeClose")]
fn huge_close(context: *mut c_void) -> i32 {
    if context.is_null() {
        return -1;
    }
    eprintln!();
    0
}

const CHUNK: usize = 4096;

thread_local! {
    static FILLING: RefCell<[i8; CHUNK + 1]> = const { RefCell::new([0; CHUNK + 1]) };
}

fn fill_filling() {
    FILLING.with_borrow_mut(|filling| {
        for f in filling.iter_mut().take(CHUNK) {
            *f = b'a' as _;
        }
        filling[CHUNK] = 0;
    })
}

/// Implement an huge: query read.
///
/// Returns the number of bytes read or -1 in case of error
#[doc(alias = "hugeRead")]
unsafe fn huge_read(context: *mut c_void, buffer: *mut i8, mut len: i32) -> i32 {
    unsafe {
        if context.is_null() || buffer.is_null() || len < 0 {
            return -1;
        }

        DOCUMENT_CONTEXT.with_borrow_mut(|context| {
            if context.instate == 0 {
                if len >= context.rlen as i32 {
                    len = context.rlen as i32;
                    context.rlen = 0;
                    memcpy(buffer as _, context.current as _, len as usize);
                    context.instate = 1;
                    context.curlen = 0;
                    context.dotlen = context.maxlen / 10;
                } else {
                    memcpy(buffer as _, context.current as _, len as usize);
                    context.rlen -= len as usize;
                    context.current = context.current.add(len as usize);
                }
            } else if context.instate == 2 {
                if len >= context.rlen as i32 {
                    len = context.rlen as i32;
                    context.rlen = 0;
                    memcpy(buffer as _, context.current as _, len as usize);
                    context.instate = 3;
                    context.curlen = 0;
                } else {
                    memcpy(buffer as _, context.current as _, len as usize);
                    context.rlen -= len as usize;
                    context.current = context.current.add(len as usize);
                }
            } else if context.instate == 1 {
                if len as usize > CHUNK {
                    len = CHUNK as i32;
                }
                FILLING.with_borrow_mut(|filling| {
                    memcpy(buffer as _, filling.as_ptr() as _, len as usize);
                });
                context.curlen += len as usize;
                if context.curlen >= context.maxlen {
                    context.rlen = strlen(HUGE_TESTS[context.current_test].end.as_ptr() as _) as _;
                    context.current = HUGE_TESTS[context.current_test].end.as_ptr() as _;
                    context.instate = 2;
                } else if context.curlen > context.dotlen {
                    print!(".");
                    context.dotlen += context.maxlen / 10;
                }
            } else {
                len = 0;
            }
            len
        })
    }
}

thread_local! {
    static CRAZY_INDX: Cell<usize> = const { Cell::new(0) };
}

const CRAZY: &CStr = c"<?xml version='1.0' encoding='UTF-8'?><?tst ?><!-- tst --><!DOCTYPE foo [<?tst ?><!-- tst --><!ELEMENT foo (#PCDATA)><!ELEMENT p (#PCDATA|emph)* >]><?tst ?><!-- tst --><foo bar='foo'><?tst ?><!-- tst -->foo<![CDATA[ ]]></foo><?tst ?><!-- tst -->";

/// Check for a crazy: query
///
/// Returns 1 if yes and 0 if another Input module should be used
#[doc(alias = "crazyMatch")]
fn crazy_match(uri: &str) -> i32 {
    uri.starts_with("crazy:") as i32
}

/// Return a pointer to the crazy: query handler, in this example simply
/// the current pointer...
///
/// Returns an Input context or NULL in case or error
#[doc(alias = "crazyOpen")]
fn crazy_open(uri: &str) -> *mut c_void {
    if !uri.starts_with("crazy:") {
        return null_mut();
    }

    if CRAZY_INDX.get() > CRAZY.to_bytes().len() {
        return null_mut();
    }
    reset_timout();
    DOCUMENT_CONTEXT.with_borrow_mut(|context| {
        context.rlen = CRAZY_INDX.get();
        context.current = CRAZY.as_ptr();
        context.instate = 0;
        context.current as _
    })
}

/// Close the crazy: query handler
///
/// Returns 0 or -1 in case of error
#[doc(alias = "crazyClose")]
fn crazy_close(context: *mut c_void) -> i32 {
    if context.is_null() {
        return -1;
    }
    0
}

/// Implement an crazy: query read.
///
/// Returns the number of bytes read or -1 in case of error
#[doc(alias = "crazyRead")]
unsafe fn crazy_read(context: *mut c_void, buffer: *mut i8, mut len: i32) -> i32 {
    unsafe {
        if context.is_null() || buffer.is_null() || len < 0 {
            return -1;
        }

        DOCUMENT_CONTEXT.with_borrow_mut(|context| {
            if check_time() <= 0 && context.instate == 1 {
                eprintln!("\ntimeout in crazy({})", CRAZY_INDX.get());
                context.rlen = CRAZY.to_bytes().len() - CRAZY_INDX.get();
                context.current = CRAZY.as_ptr().add(CRAZY_INDX.get()) as _;
                context.instate = 2;
            }
            if context.instate == 0 {
                if len >= context.rlen as i32 {
                    len = context.rlen as i32;
                    context.rlen = 0;
                    memcpy(buffer as _, context.current as _, len as _);
                    context.instate = 1;
                    context.curlen = 0;
                } else {
                    memcpy(buffer as _, context.current as _, len as _);
                    context.rlen -= len as usize;
                    context.current = context.current.add(len as usize);
                }
            } else if context.instate == 2 {
                if len >= context.rlen as i32 {
                    len = context.rlen as i32;
                    context.rlen = 0;
                    memcpy(buffer as _, context.current as _, len as _);
                    context.instate = 3;
                    context.curlen = 0;
                } else {
                    memcpy(buffer as _, context.current as _, len as _);
                    context.rlen -= len as usize;
                    context.current = context.current.add(len as usize);
                }
            } else if context.instate == 1 {
                if len as usize > CHUNK {
                    len = CHUNK as i32;
                }
                FILLING.with_borrow_mut(|filling| {
                    memcpy(buffer as _, filling.as_ptr() as _, len as _);
                });
                context.curlen += len as usize;
                if context.curlen >= context.maxlen {
                    context.rlen = CRAZY.to_bytes().len() - CRAZY_INDX.get();
                    context.current = CRAZY.as_ptr().add(CRAZY_INDX.get()) as _;
                    context.instate = 2;
                }
            } else {
                len = 0;
            }
            len
        })
    }
}

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
        let memused: i32 = xml_mem_used();

        let ret: XmlParserInputPtr = xml_no_net_external_entity_loader(url, id, ctxt);
        EXTRA_MEMORY_FROM_RESOLVER.set(EXTRA_MEMORY_FROM_RESOLVER.get() + xml_mem_used() - memused);

        ret
    }
}

// Trapping the error messages at the generic level to grab the equivalent of
// stderr messages on CLI tools.
thread_local! {
    static TEST_ERRORS: RefCell<[u8; 32769]> = const { RefCell::new([0; 32769]) };
    static TEST_ERRORS_SIZE: Cell<usize> = const { Cell::new(0) };
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
            TEST_ERRORS_SIZE.set(TEST_ERRORS_SIZE.get() + msg.len())
        }
        errors[TEST_ERRORS_SIZE.get()] = 0;
    })
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
            if let Some(&inp) = (*ctxt).input() {
                input = inp;
            }
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
        set_structured_error(Some(test_structured_error_handler), None);
        // register the new I/O handlers
        struct HugeTestIO(*mut c_void);
        impl Read for HugeTestIO {
            fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
                let res =
                    unsafe { huge_read(self.0, buf.as_mut_ptr() as *mut i8, buf.len() as i32) };
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
                    huge_close(self.0);
                }
            }
        }
        impl XmlInputCallback for HugeTestIO {
            fn is_match(&self, filename: &str) -> bool {
                huge_match(filename) != 0
            }
            fn open(&mut self, filename: &str) -> std::io::Result<Box<dyn Read>> {
                let ptr = huge_open(filename);
                if ptr.is_null() {
                    Err(io::Error::other("Failed to execute huge_open"))
                } else {
                    Ok(Box::new(Self(ptr)))
                }
            }
        }
        if register_input_callbacks(HugeTestIO(null_mut())).is_err() {
            panic!("failed to register Huge handlers");
        }
        struct CrazyTestIO(*mut c_void);
        impl Read for CrazyTestIO {
            fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
                let res =
                    unsafe { crazy_read(self.0, buf.as_mut_ptr() as *mut i8, buf.len() as i32) };
                if res < 0 {
                    Err(io::Error::last_os_error())
                } else {
                    Ok(res as usize)
                }
            }
        }
        unsafe impl Send for CrazyTestIO {}
        impl Drop for CrazyTestIO {
            fn drop(&mut self) {
                if !self.0.is_null() {
                    crazy_close(self.0);
                }
            }
        }
        impl XmlInputCallback for CrazyTestIO {
            fn is_match(&self, filename: &str) -> bool {
                crazy_match(filename) != 0
            }
            fn open(&mut self, filename: &str) -> std::io::Result<Box<dyn Read>> {
                let ptr = crazy_open(filename);
                if ptr.is_null() {
                    Err(io::Error::other("Failed to execute crazy_open"))
                } else {
                    Ok(Box::new(Self(ptr)))
                }
            }
        }
        if register_input_callbacks(CrazyTestIO(null_mut())).is_err() {
            panic!("failed to register Crazy handlers");
        }
    }
}

thread_local! {
    static CALLBACKS: AtomicU64 = const { AtomicU64::new(0) };
}

/// Is this document tagged standalone ?
///
/// Returns 1 if true
#[doc(alias = "isStandaloneCallback")]
fn is_standalone_callback(_ctx: Option<GenericErrorContext>) -> i32 {
    CALLBACKS.with(|c| c.fetch_add(1, Ordering::Relaxed));
    0
}

/// Does this document has an internal subset
///
/// Returns 1 if true
#[doc(alias = "hasInternalSubsetCallback")]
fn has_internal_subset_callback(_ctx: Option<GenericErrorContext>) -> i32 {
    CALLBACKS.with(|c| c.fetch_add(1, Ordering::Relaxed));
    0
}

/// Does this document has an external subset
///
/// Returns 1 if true
#[doc(alias = "hasExternalSubsetCallback")]
fn has_external_subset_callback(_ctx: Option<GenericErrorContext>) -> i32 {
    CALLBACKS.with(|c| c.fetch_add(1, Ordering::Relaxed));
    0
}

/// Does this document has an internal subset
#[doc(alias = "internalSubsetCallback")]
fn internal_subset_callback(
    _ctx: Option<GenericErrorContext>,
    _name: Option<&str>,
    _external_id: Option<&str>,
    _system_id: Option<&str>,
) {
    CALLBACKS.with(|c| c.fetch_add(1, Ordering::Relaxed));
}

/// Does this document has an external subset
#[doc(alias = "externalSubsetCallback")]
fn external_subset_callback(
    _ctx: Option<GenericErrorContext>,
    _name: Option<&str>,
    _external_id: Option<&str>,
    _system_id: Option<&str>,
) {
    CALLBACKS.with(|c| c.fetch_add(1, Ordering::Relaxed));
}

/// Special entity resolver, better left to the parser, it has
/// more context than the application layer.
///
/// The default behaviour is to NOT resolve the entities, in that case
/// the ENTITY_REF nodes are built in the structure (and the parameter values).
///
/// Returns the xmlParserInputPtr if inlined or NULL for DOM behaviour.
#[doc(alias = "resolveEntityCallback")]
fn resolve_entity_callback(
    _ctx: Option<GenericErrorContext>,
    _public_id: Option<&str>,
    _system_id: Option<&str>,
) -> XmlParserInputPtr {
    CALLBACKS.with(|c| c.fetch_add(1, Ordering::Relaxed));
    null_mut()
}

/// Get an entity by name
///
/// Returns the xmlParserInputPtr if inlined or NULL for DOM behaviour.
#[doc(alias = "getEntityCallback")]
fn get_entity_callback(_ctx: Option<GenericErrorContext>, _name: &str) -> Option<XmlEntityPtr> {
    CALLBACKS.with(|c| c.fetch_add(1, Ordering::Relaxed));
    None
}

/// Get a parameter entity by name
///
/// Returns the xmlParserInputPtr
#[doc(alias = "getParameterEntityCallback")]
fn get_parameter_entity_callback(
    _ctx: Option<GenericErrorContext>,
    _name: &str,
) -> Option<XmlEntityPtr> {
    CALLBACKS.with(|c| c.fetch_add(1, Ordering::Relaxed));
    None
}

/// An entity definition has been parsed
#[doc(alias = "entityDeclCallback")]
fn entity_decl_callback(
    _ctx: Option<GenericErrorContext>,
    _name: &str,
    _typ: XmlEntityType,
    _public_id: Option<&str>,
    _system_id: Option<&str>,
    _content: Option<&str>,
) {
    CALLBACKS.with(|c| c.fetch_add(1, Ordering::Relaxed));
}

/// An attribute definition has been parsed
#[doc(alias = "attributeDeclCallback")]
fn attribute_decl_callback(
    _ctx: Option<GenericErrorContext>,
    _elem: &str,
    _name: &str,
    _typ: XmlAttributeType,
    _def: XmlAttributeDefault,
    _default_value: Option<&str>,
    _tree: Option<Box<XmlEnumeration>>,
) {
    CALLBACKS.with(|c| c.fetch_add(1, Ordering::Relaxed));
}

/// An element definition has been parsed
#[doc(alias = "elementDeclCallback")]
fn element_decl_callback(
    _ctx: Option<GenericErrorContext>,
    _name: &str,
    _typ: Option<XmlElementTypeVal>,
    _content: XmlElementContentPtr,
) {
    CALLBACKS.with(|c| c.fetch_add(1, Ordering::Relaxed));
}

/// What to do when a notation declaration has been parsed.
#[doc(alias = "notationDeclCallback")]
fn notation_decl_callback(
    _ctx: Option<GenericErrorContext>,
    _name: &str,
    _public_id: Option<&str>,
    _system_id: Option<&str>,
) {
    CALLBACKS.with(|c| c.fetch_add(1, Ordering::Relaxed));
}

/// What to do when an unparsed entity declaration is parsed
#[doc(alias = "unparsedEntityDeclCallback")]
fn unparsed_entity_decl_callback(
    _ctx: Option<GenericErrorContext>,
    _name: &str,
    _public_id: Option<&str>,
    _system_id: Option<&str>,
    _notation_name: Option<&str>,
) {
    CALLBACKS.with(|c| c.fetch_add(1, Ordering::Relaxed));
}

/// Receive the document locator at startup, actually xmlDefaultSAXLocator
/// Everything is available on the context, so this is useless in our case.
#[doc(alias = "setDocumentLocatorCallback")]
fn set_document_locator_callback(_ctx: Option<GenericErrorContext>, _loc: XmlSAXLocatorPtr) {
    CALLBACKS.with(|c| c.fetch_add(1, Ordering::Relaxed));
}

/// called when the document start being processed.
#[doc(alias = "startDocumentCallback")]
fn start_document_callback(_ctx: Option<GenericErrorContext>) {
    CALLBACKS.with(|c| c.fetch_add(1, Ordering::Relaxed));
}

/// called when the document end has been detected.
#[doc(alias = "endDocumentCallback")]
fn end_document_callback(_ctx: Option<GenericErrorContext>) {
    CALLBACKS.with(|c| c.fetch_add(1, Ordering::Relaxed));
}

/// receiving some chars from the parser.
/// Question: how much at a time ???
#[doc(alias = "charactersCallback")]
fn characters_callback(_ctx: Option<GenericErrorContext>, _ch: &str) {
    CALLBACKS.with(|c| c.fetch_add(1, Ordering::Relaxed));
}

/// called when an entity reference is detected.
#[doc(alias = "referenceCallback")]
fn reference_callback(_ctx: Option<GenericErrorContext>, _name: &str) {
    CALLBACKS.with(|c| c.fetch_add(1, Ordering::Relaxed));
}

/// receiving some ignorable whitespaces from the parser.
/// Question: how much at a time ???
#[doc(alias = "ignorableWhitespaceCallback")]
fn ignorable_whitespace_callback(_ctx: Option<GenericErrorContext>, _ch: &str) {
    CALLBACKS.with(|c| c.fetch_add(1, Ordering::Relaxed));
}

/// A processing instruction has been parsed.
#[doc(alias = "processingInstructionCallback")]
fn processing_instruction_callback(
    _ctx: Option<GenericErrorContext>,
    _target: &str,
    _data: Option<&str>,
) {
    CALLBACKS.with(|c| c.fetch_add(1, Ordering::Relaxed));
}

/// called when a pcdata block has been parsed
#[doc(alias = "cdataBlockCallback")]
fn cdata_block_callback(_ctx: Option<GenericErrorContext>, _value: &str) {
    CALLBACKS.with(|c| c.fetch_add(1, Ordering::Relaxed));
}

/// A comment has been parsed.
#[doc(alias = "commentCallback")]
fn comment_callback(_ctx: Option<GenericErrorContext>, _value: &str) {
    CALLBACKS.with(|c| c.fetch_add(1, Ordering::Relaxed));
}

/// Display and format a warning messages, gives file, line, position and
/// extra parameters.
#[doc(alias = "warningCallback")]
fn warning_callback(_ctx: Option<GenericErrorContext>, _msg: &str) {
    CALLBACKS.with(|c| c.fetch_add(1, Ordering::Relaxed));
}

/// Display and format a error messages, gives file, line, position and
/// extra parameters.
#[doc(alias = "errorCallback")]
fn error_callback(_ctx: Option<GenericErrorContext>, _msg: &str) {
    CALLBACKS.with(|c| c.fetch_add(1, Ordering::Relaxed));
}

/// Display and format a fatalError messages, gives file, line, position and
/// extra parameters.
#[doc(alias = "fatalErrorCallback")]
fn fatal_error_callback(_ctx: Option<GenericErrorContext>, _msg: &str) {}

/// called when an opening tag has been processed.
#[doc(alias = "startElementNsCallback")]
fn start_element_ns_callback(
    _ctx: Option<GenericErrorContext>,
    _localname: &str,
    _prefix: Option<&str>,
    _uri: Option<&str>,
    _namespaces: &[(Option<String>, String)],
    _nb_defaulted: usize,
    _attributes: &[(String, Option<String>, Option<String>, String)],
) {
    CALLBACKS.with(|c| c.fetch_add(1, Ordering::Relaxed));
}

/// called when the end of an element has been detected.
#[doc(alias = "endElementCallback")]
fn end_element_ns_callback(
    _ctx: Option<GenericErrorContext>,
    _localname: &str,
    _prefix: Option<&str>,
    _uri: Option<&str>,
) {
    CALLBACKS.with(|c| c.fetch_add(1, Ordering::Relaxed));
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

/// Parse a memory generated file using SAX
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "readerTest")]
unsafe fn sax_test(filename: &str, limit: usize, options: i32, fail: i32) -> i32 {
    unsafe {
        let res: i32;

        NB_TESTS.set(NB_TESTS.get() + 1);

        DOCUMENT_CONTEXT.with_borrow_mut(|context| context.maxlen = limit);
        let mut sax = XmlSAXHandler::default();
        std::ptr::copy(&CALLBACK_SAX2_HANDLER_STRUCT, &mut sax, 1);
        let Ok(ctxt) = xml_new_sax_parser_ctxt(Some(Box::new(sax)), None) else {
            eprintln!("Failed to create parser context");
            return 1;
        };
        if let Some(doc) = xml_ctxt_read_file(ctxt, filename, None, options) {
            eprintln!("SAX parsing generated a document !");
            xml_free_doc(doc);
            res = 0;
        } else if (*ctxt).well_formed == 0 {
            if fail != 0 {
                res = 0;
            } else {
                eprintln!("Failed to parse '{filename}' {limit}");
                res = 1;
            }
        } else if fail != 0 {
            eprintln!("Failed to get failure for '{filename}' {limit}");
            res = 1;
        } else {
            res = 0;
        }
        xml_free_parser_ctxt(ctxt);

        res
    }
}

/// Parse a memory generated file using the xmlReader
///
/// Returns 0 in case of success, an error code otherwise
#[doc(alias = "readerTest")]
#[cfg(feature = "libxml_reader")]
unsafe fn reader_test(filename: &str, limit: usize, options: i32, fail: i32) -> i32 {
    unsafe {
        use exml::libxml::xmlreader::{
            XmlTextReaderPtr, xml_free_text_reader, xml_reader_for_file,
        };

        let mut res: i32;

        NB_TESTS.set(NB_TESTS.get() + 1);

        DOCUMENT_CONTEXT.with_borrow_mut(|context| context.maxlen = limit);
        let reader: XmlTextReaderPtr = xml_reader_for_file(filename, None, options);
        if reader.is_null() {
            eprintln!("Failed to open '{}' test", filename);
            return 1;
        }
        let mut ret = (*reader).read();
        while ret == 1 {
            ret = (*reader).read();
        }
        if ret != 0 {
            if fail != 0 {
                res = 0;
            } else {
                if filename == "crazy:" {
                    eprintln!("Failed to parse '{}' {}", filename, CRAZY_INDX.get());
                } else {
                    eprintln!("Failed to parse '{}' {limit}", filename);
                }
                res = 1;
            }
        } else if fail != 0 {
            if filename == "crazy:" {
                eprintln!(
                    "Failed to get failure for '{}' {}",
                    filename,
                    CRAZY_INDX.get()
                );
            } else {
                eprintln!("Failed to get failure for '{}' {limit}", filename);
            }
            res = 1;
        } else {
            res = 0;
        }
        if TIMEOUT.get() {
            res = 1;
        }
        xml_free_text_reader(reader);

        res
    }
}

type Functest = unsafe fn(filename: &str, limit: usize, options: i32, fail: i32) -> i32;

struct LimitDesc<'a> {
    name: &'a str, /* the huge generator name */
    limit: usize,  /* the limit to test */
    options: i32,  /* extra parser options */
    fail: i32,     /* whether the test should fail */
}

static LIMIT_DESCRIPTIONS: &[LimitDesc] = &[
    // max length of a text node in content
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
    // max length of a text node in content
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
    // max length of a comment node
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
    // max length of a PI node
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
    // { "Parsing of huge files with the tree parser", treeTest},
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
    // the SAX parser doesn't hit a limit of XML_MAX_TEXT_LENGTH text nodes
    TestException {
        test: 0,
        limit: 1,
        fail: 0,
        size: 0,
    },
];

unsafe fn launch_tests(tst: &TestDesc, test: u32) -> i32 {
    unsafe {
        let mut res: i32;
        let mut err: i32 = 0;
        let mut limit: usize;
        let mut fail: i32;

        for (i, descr) in LIMIT_DESCRIPTIONS.iter().enumerate() {
            limit = descr.limit;
            fail = descr.fail;
            // Handle exceptions if any
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
            res = tst.func.unwrap()(descr.name, limit, descr.options, fail);
            if res != 0 {
                NB_ERRORS.set(NB_ERRORS.get() + 1);
                err += 1;
            }
        }
        err
    }
}

unsafe fn runtest(i: u32) -> i32 {
    unsafe {
        let mut ret: i32 = 0;

        let old_errors: i32 = NB_ERRORS.get();
        let old_tests: i32 = NB_TESTS.get();
        let old_leaks: i32 = NB_LEAKS.get();
        if TEST_DESCRIPTIONS[i as usize].desc.is_some() {
            println!("## {}", TEST_DESCRIPTIONS[i as usize].desc.unwrap());
        }
        let tmp = TEST_DESCRIPTIONS[i as usize];
        let res: i32 = launch_tests(&tmp, i);
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
                NB_LEAKS.get() - old_leaks
            );
        }
        ret
    }
}

unsafe fn launch_crazy_sax(test: u32, fail: i32) -> i32 {
    unsafe {
        let mut err: i32 = 0;

        CRAZY_INDX.set(test as usize);

        let res = sax_test("crazy::test", XML_MAX_LOOKUP_LIMIT - CHUNK, 0, fail);
        if res != 0 {
            NB_ERRORS.set(NB_ERRORS.get() + 1);
            err += 1;
        }
        eprintln!("{}", CRAZY.to_bytes()[test as usize] as char);

        err
    }
}

#[cfg(feature = "libxml_reader")]
unsafe fn launch_crazy(test: u32, fail: i32) -> i32 {
    unsafe {
        let mut err: i32 = 0;

        CRAZY_INDX.set(test as usize);

        let res = reader_test("crazy::test", XML_MAX_LOOKUP_LIMIT - CHUNK, 0, fail);
        if res != 0 {
            NB_ERRORS.set(NB_ERRORS.get() + 1);
            err += 1;
        }
        eprintln!("{}", CRAZY.to_bytes()[test as usize] as char);

        err
    }
}

unsafe extern "C" fn get_crazy_fail(test: i32) -> i32 {
    // adding 1000000 of character 'a' leads to parser failure mostly
    // everywhere except in those special spots. Need to be updated
    // each time crazy is updated
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

fn runcrazy(launch: unsafe fn(u32, i32) -> i32) {
    fill_filling();
    initialize_test();
    let mut ret: i32 = 0;
    let mut res: i32 = 0;

    let old_errors: i32 = NB_ERRORS.get();
    let old_tests: i32 = NB_TESTS.get();
    let old_leaks: i32 = NB_LEAKS.get();

    unsafe {
        for i in 0..CRAZY.to_bytes().len() {
            res += launch(i as _, get_crazy_fail(i as _));
            if res != 0 {
                ret += 1;
            }
        }
    }
    eprintln!();
    if NB_ERRORS.get() == old_errors && NB_LEAKS.get() == old_leaks {
        println!("Ran {} tests, no errors", NB_TESTS.get() - old_tests);
    } else {
        println!(
            "Ran {} tests, {} errors, {} leaks",
            NB_TESTS.get() - old_tests,
            NB_ERRORS.get() - old_errors,
            NB_LEAKS.get() - old_leaks
        );
    }
    cleanup_test();
    assert_eq!(
        ret,
        0,
        "Failed to pass runcrazy()\nTotal {} tests, {} errors, {} leaks\n",
        NB_TESTS.get(),
        NB_ERRORS.get(),
        NB_LEAKS.get(),
    );
}

static NUM_LAUNCH_TEST: AtomicUsize = AtomicUsize::new(0);

fn initialize_test() {
    loop {
        if NUM_LAUNCH_TEST
            .fetch_update(Ordering::Release, Ordering::Acquire, |old| {
                if old == 0 {
                    unsafe {
                        initialize_libxml2();
                    }
                }
                Some(old + 1)
            })
            .is_ok()
        {
            break;
        }
    }
}

fn cleanup_test() {
    loop {
        if NUM_LAUNCH_TEST
            .fetch_update(Ordering::Release, Ordering::Acquire, |old| {
                if old == 1 {
                    unsafe {
                        xml_cleanup_parser();
                        xml_memory_dump();
                    }
                }
                old.checked_sub(1)
            })
            .is_ok()
        {
            break;
        }
    }
}

#[test]
#[cfg(feature = "libxml_reader")]
fn runcrazy_reader() {
    println!("## Crazy tests on reader");
    runcrazy(launch_crazy);
}

#[test]
fn runcrazy_sax() {
    println!("## Crazy tests on SAX");
    runcrazy(launch_crazy_sax);
}

#[test]
fn main() {
    let ret: i32;
    fill_filling();
    initialize_test();

    unsafe {
        for i in (0..).take_while(|&i| TEST_DESCRIPTIONS[i].func.is_some()) {
            assert_eq!(runtest(i as _), 0, "Failed to pass run_test({i})");
        }
        if NB_ERRORS.get() == 0 && NB_LEAKS.get() == 0 {
            ret = 0;
            println!("Total {} tests, no errors", NB_TESTS.get());
        } else {
            ret = 1;
            println!(
                "Total {} tests, {} errors, {} leaks\n",
                NB_TESTS.get(),
                NB_ERRORS.get(),
                NB_LEAKS.get(),
            );
        }
    }
    cleanup_test();

    assert_eq!(ret, 0);
}
