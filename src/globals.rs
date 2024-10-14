use std::{borrow::Cow, cell::RefCell, ffi::c_void, io::Write, ptr::null_mut, rc::Rc};

use const_format::concatcp;
use libc::{free, malloc, realloc};

use crate::{
    encoding::{XmlCharEncoding, XmlCharEncodingHandler},
    error::{generic_error_default, XmlError},
    libxml::{
        globals::{XmlDeregisterNodeFunc, XmlRegisterNodeFunc},
        parser::{XmlSAXHandlerV1, XmlSaxlocator},
        sax2::{
            xml_sax2_get_column_number, xml_sax2_get_line_number, xml_sax2_get_public_id,
            xml_sax2_get_system_id,
        },
        tree::{XmlBufferAllocationScheme, BASE_BUFFER_SIZE},
        xml_io::{XmlOutputBuffer, XmlParserInputBuffer},
        xmlmemory::{XmlFreeFunc, XmlMallocFunc, XmlReallocFunc, XmlStrdupFunc},
        xmlstring::xml_strdup,
    },
};

pub type GenericError = for<'a> fn(Option<&mut (dyn Write + 'static)>, &str);
pub type StructuredError = fn(*mut c_void, &XmlError);
type ParserInputBufferCreateFilename =
    fn(uri: &str, enc: XmlCharEncoding) -> *mut XmlParserInputBuffer;
type OutputBufferCreateFilename = fn(
    uri: &str,
    encoder: Option<Rc<RefCell<XmlCharEncodingHandler>>>,
    compression: i32,
) -> *mut XmlOutputBuffer;

pub struct XmlGlobalState {
    parser_version: Cow<'static, str>,
    default_sax_locator: XmlSaxlocator,
    default_sax_handler: XmlSAXHandlerV1,
    html_default_sax_handler: XmlSAXHandlerV1,
    free: Option<XmlFreeFunc>,
    malloc: Option<XmlMallocFunc>,
    malloc_atomic: Option<XmlMallocFunc>,
    realloc: Option<XmlReallocFunc>,
    mem_strdup: Option<XmlStrdupFunc>,
    pub(crate) generic_error: GenericError,
    pub(crate) generic_error_context: Option<Box<dyn Write>>,
    pub(crate) structured_error: Option<StructuredError>,
    pub(crate) structured_error_context: *mut c_void,
    old_xml_wd_compatibility: bool,
    pub(crate) buffer_alloc_scheme: XmlBufferAllocationScheme,
    pub(crate) default_buffer_size: usize,
    substitute_entities_default_value: i32,
    do_validity_checking_default_value: i32,
    get_warnings_default_value: i32,
    keep_blanks_default_value: i32,
    line_numbers_default_value: i32,
    load_ext_dtd_default_value: i32,
    parser_debug_entities: i32,
    pedantic_parser_default_value: i32,
    save_no_empty_tags: i32,
    indent_tree_output: i32,
    tree_indent_string: Cow<'static, str>,
    register_node_default_value: Option<XmlRegisterNodeFunc>,
    deregister_node_default_value: Option<XmlDeregisterNodeFunc>,
    pub(crate) last_error: XmlError,
    pub(crate) parser_input_buffer_create_filename_value: Option<ParserInputBufferCreateFilename>,
    pub(crate) output_buffer_create_filename_value: Option<OutputBufferCreateFilename>,
}

impl XmlGlobalState {
    fn new() -> Self {
        const VERSION_STRING: &str = concatcp!(
            "{:0>2}{:0>2}{:0>2}",
            env!("CARGO_PKG_VERSION_MAJOR"),
            env!("CARGO_PKG_VERSION_MINOR"),
            env!("CARGO_PKG_VERSION_PATCH")
        );
        Self {
            parser_version: Cow::Borrowed(VERSION_STRING),
            default_sax_locator: XmlSaxlocator {
                get_public_id: xml_sax2_get_public_id,
                get_system_id: xml_sax2_get_system_id,
                get_line_number: xml_sax2_get_line_number,
                get_column_number: xml_sax2_get_column_number,
            },
            default_sax_handler: XmlSAXHandlerV1::default(),
            html_default_sax_handler: XmlSAXHandlerV1::default(),
            free: Some(free as XmlFreeFunc),
            malloc: Some(malloc as XmlMallocFunc),
            malloc_atomic: Some(malloc as XmlMallocFunc),
            realloc: Some(realloc as XmlReallocFunc),
            mem_strdup: Some(xml_strdup as XmlStrdupFunc),
            generic_error: generic_error_default,
            generic_error_context: None,
            structured_error: None,
            structured_error_context: null_mut(),
            old_xml_wd_compatibility: false,
            buffer_alloc_scheme: XmlBufferAllocationScheme::XmlBufferAllocExact,
            default_buffer_size: BASE_BUFFER_SIZE,
            substitute_entities_default_value: 0,
            do_validity_checking_default_value: 0,
            get_warnings_default_value: 1,
            keep_blanks_default_value: 1,
            line_numbers_default_value: 0,
            load_ext_dtd_default_value: 0,
            parser_debug_entities: 0,
            pedantic_parser_default_value: 0,
            save_no_empty_tags: 0,
            indent_tree_output: 1,
            tree_indent_string: Cow::Borrowed("  "),
            register_node_default_value: None,
            deregister_node_default_value: None,
            last_error: XmlError::default(),
            parser_input_buffer_create_filename_value: None,
            output_buffer_create_filename_value: None,
        }
    }
}

thread_local! {
    pub static GLOBAL_STATE: RefCell<XmlGlobalState> = RefCell::new(XmlGlobalState::new());
}

/// Set new generic error function and generic error context.
///
/// If `func` is `None`, set `generic_error_default`.  
/// If `context` is `None`, current context is clear and no context is set.  
pub fn set_generic_error(func: Option<GenericError>, context: Option<impl Write + 'static>) {
    GLOBAL_STATE.with_borrow_mut(|state| {
        state.generic_error = func.unwrap_or(generic_error_default);
        state.generic_error_context = context.map(|context| {
            let boxed: Box<dyn Write + 'static> = Box::new(context);
            boxed
        });
    });
}

/// Update default buffer allocation scheme.
///
/// The user can set the following 3 schemes
///
/// - `XmlBufferAllocationScheme::XmlBufferAllocExact`
/// - `XmlBufferAllocationScheme::XmlBufferAllocDoubleit`
/// - `XmlBufferAllocationScheme::XmlBufferAllocHybrid`
///
/// Other schemes cannot set as default scheme.
///
/// # Panics
/// - Do not specify unsupported schemes.
pub fn set_default_buffer_allocation_scheme(scheme: XmlBufferAllocationScheme) {
    match scheme {
        XmlBufferAllocationScheme::XmlBufferAllocExact
        | XmlBufferAllocationScheme::XmlBufferAllocDoubleit
        | XmlBufferAllocationScheme::XmlBufferAllocHybrid => {
            GLOBAL_STATE.with_borrow_mut(|state| state.buffer_alloc_scheme = scheme);
        }
        scheme => {
            panic!("Cannot set {scheme:?} as a default scheme.");
        }
    }
}

/// Get the current default buffer allocation scheme.
pub fn get_default_buffer_allocation_scheme() -> XmlBufferAllocationScheme {
    GLOBAL_STATE.with_borrow(|state| state.buffer_alloc_scheme)
}
