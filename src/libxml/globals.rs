//! Provide methods and data structures for global variables.  
//! This module is based on `libxml/globals.h`, `globals.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    cell::RefCell,
    ffi::{c_char, c_int, c_void},
    mem::{size_of, zeroed},
    ptr::{addr_of_mut, null_mut},
    rc::Rc,
    sync::atomic::{AtomicI32, AtomicPtr, Ordering},
};

use libc::{free, malloc, memset, realloc};

#[cfg(feature = "legacy")]
use crate::libxml::sax::{inithtmlDefaultSAXHandler, initxmlDefaultSAXHandler};
use crate::{
    encoding::{XmlCharEncoding, XmlCharEncodingHandler},
    error::{parser_error, parser_warning, XmlError},
    globals::reset_last_error,
    libxml::{
        parser::{XmlSAXHandlerV1, XmlSaxlocator},
        xml_io::{
            XmlOutputBufferPtr, XmlParserInputBufferPtr, __xml_output_buffer_create_filename,
        },
        xmlmemory::{XmlFreeFunc, XmlMallocFunc, XmlReallocFunc, XmlStrdupFunc},
    },
    private::threads::{__xml_global_init_mutex_destroy, xml_cleanup_mutex, xml_init_mutex},
};

use super::{
    parser::xml_init_parser,
    sax2::{
        xml_sax2_attribute_decl, xml_sax2_cdata_block, xml_sax2_characters, xml_sax2_comment,
        xml_sax2_element_decl, xml_sax2_end_document, xml_sax2_end_element, xml_sax2_entity_decl,
        xml_sax2_external_subset, xml_sax2_get_column_number, xml_sax2_get_entity,
        xml_sax2_get_line_number, xml_sax2_get_parameter_entity, xml_sax2_get_public_id,
        xml_sax2_get_system_id, xml_sax2_has_external_subset, xml_sax2_has_internal_subset,
        xml_sax2_ignorable_whitespace, xml_sax2_internal_subset, xml_sax2_is_standalone,
        xml_sax2_notation_decl, xml_sax2_processing_instruction, xml_sax2_reference,
        xml_sax2_resolve_entity, xml_sax2_set_document_locator, xml_sax2_start_document,
        xml_sax2_start_element, xml_sax2_unparsed_entity_decl,
    },
    threads::{xml_get_global_state, xml_mutex_lock, xml_mutex_unlock, XmlMutex},
    tree::{XmlBufferAllocationScheme, XmlNodePtr, BASE_BUFFER_SIZE, __XML_REGISTER_CALLBACKS},
    xml_io::__xml_parser_input_buffer_create_filename,
    xmlstring::{xml_char_strdup, xml_strdup, XmlChar},
    xmlversion::LIBXML_VERSION_STRING,
};

/**
* xmlInitGlobals:
*
* DEPRECATED: Alias for xmlInitParser.
*/
#[deprecated]
pub unsafe extern "C" fn xml_init_globals() {
    xml_init_parser();
}

/**
 * xmlCleanupGlobals:
 *
 * DEPRECATED: This function is a no-op. Call xmlCleanupParser
 * to free global state but see the warnings there. xmlCleanupParser
 * should be only called once at program exit. In most cases, you don't
 * have call cleanup functions at all.
 */
#[deprecated]
pub unsafe extern "C" fn xml_cleanup_globals() {}

/**
 * xmlParserInputBufferCreateFilenameFunc:
 * @URI: the URI to read from
 * @enc: the requested source encoding
 *
 * Signature for the function doing the lookup for a suitable input method
 * corresponding to an URI.
 *
 * Returns the new xmlParserInputBufferPtr in case of success or NULL if no
 *         method was found.
 */
pub type XmlParserInputBufferCreateFilenameFunc =
    unsafe fn(URI: *const c_char, enc: XmlCharEncoding) -> XmlParserInputBufferPtr;

/**
 * xmlOutputBufferCreateFilenameFunc:
 * @URI: the URI to write to
 * @enc: the requested target encoding
 *
 * Signature for the function doing the lookup for a suitable output method
 * corresponding to an URI.
 *
 * Returns the new xmlOutputBufferPtr in case of success or NULL if no
 *         method was found.
 */
pub type XmlOutputBufferCreateFilenameFunc = unsafe fn(
    URI: *const c_char,
    encoder: Option<Rc<RefCell<XmlCharEncodingHandler>>>,
    compression: c_int,
) -> XmlOutputBufferPtr;

/**
 * xmlParserInputBufferCreateFilenameDefault:
 * @func: function poc_inter to the new ParserInputBufferCreateFilenameFunc
 *
 * Registers a callback for URI input file handling
 *
 * Returns the old value of the registration function
 */
pub unsafe fn xml_parser_input_buffer_create_filename_default(
    func: Option<XmlParserInputBufferCreateFilenameFunc>,
) -> XmlParserInputBufferCreateFilenameFunc {
    let old: XmlParserInputBufferCreateFilenameFunc =
        _XML_PARSER_INPUT_BUFFER_CREATE_FILENAME_VALUE
            .unwrap_or(__xml_parser_input_buffer_create_filename);

    _XML_PARSER_INPUT_BUFFER_CREATE_FILENAME_VALUE = func;
    old
}

/**
* xmlOutputBufferCreateFilenameDefault:
* @func: function poc_inter to the new OutputBufferCreateFilenameFunc
*
* Registers a callback for URI output file handling
*
* Returns the old value of the registration function
*/
pub unsafe fn xml_output_buffer_create_filename_default(
    func: Option<XmlOutputBufferCreateFilenameFunc>,
) -> Option<XmlOutputBufferCreateFilenameFunc> {
    let old = _XML_OUTPUT_BUFFER_CREATE_FILENAME_VALUE;
    // #ifdef LIBXML_OUTPUT_ENABLED
    //    if old.is_null() {
    //        old = __xmlOutputBufferCreateFilename;
    //    }
    // #endif
    _XML_OUTPUT_BUFFER_CREATE_FILENAME_VALUE = func;
    old
}

/**
 * xmlRegisterNodeFunc:
 * @node: the current node
 *
 * Signature for the registration callback of a created node
 */
pub type XmlRegisterNodeFunc = unsafe extern "C" fn(node: XmlNodePtr);
/**
 * xmlDeregisterNodeFunc:
 * @node: the current node
 *
 * Signature for the deregistration callback of a discarded node
 */
pub type XmlDeregisterNodeFunc = unsafe extern "C" fn(node: XmlNodePtr);

pub type XmlGlobalStatePtr = *mut XmlGlobalState;
pub struct XmlGlobalState {
    pub(crate) xml_parser_version: *const c_char,

    pub(crate) xml_default_sax_locator: XmlSaxlocator,
    pub(crate) xml_default_sax_handler: XmlSAXHandlerV1,
    #[allow(unused)]
    pub(crate) docb_default_sax_handler: XmlSAXHandlerV1, /* unused */
    pub(crate) html_default_sax_handler: XmlSAXHandlerV1,

    pub(crate) xml_free: Option<XmlFreeFunc>,
    pub(crate) xml_malloc: Option<XmlMallocFunc>,
    pub(crate) xml_mem_strdup: Option<XmlStrdupFunc>,
    pub(crate) xml_realloc: Option<XmlReallocFunc>,

    // pub(crate) xml_generic_error: Option<XmlGenericErrorFunc>,
    // pub(crate) xml_structured_error: Option<XmlStructuredErrorFunc>,
    // pub(crate) xml_generic_error_context: AtomicPtr<c_void>,
    pub(crate) old_xml_wd_compatibility: c_int,

    pub(crate) xml_buffer_alloc_scheme: XmlBufferAllocationScheme,
    pub(crate) xml_default_buffer_size: c_int,

    pub(crate) xml_substitute_entities_default_value: c_int,
    pub(crate) xml_do_validity_checking_default_value: c_int,
    pub(crate) xml_get_warnings_default_value: c_int,
    pub(crate) xml_keep_blanks_default_value: c_int,
    pub(crate) xml_line_numbers_default_value: c_int,
    pub(crate) xml_load_ext_dtd_default_value: c_int,
    pub(crate) xml_parser_debug_entities: c_int,
    pub(crate) xml_pedantic_parser_default_value: c_int,

    pub(crate) xml_save_no_empty_tags: c_int,
    pub(crate) xml_indent_tree_output: c_int,
    // pub(crate) xml_tree_indent_string: *const c_char,
    pub(crate) xml_register_node_default_value: Option<XmlRegisterNodeFunc>,
    pub(crate) xml_deregister_node_default_value: Option<XmlDeregisterNodeFunc>,

    pub(crate) xml_malloc_atomic: Option<XmlMallocFunc>,
    pub(crate) xml_last_error: XmlError,

    pub(crate) xml_parser_input_buffer_create_filename_value:
        Option<XmlParserInputBufferCreateFilenameFunc>,
    pub(crate) xml_output_buffer_create_filename_value: Option<XmlOutputBufferCreateFilenameFunc>,

    pub(crate) xml_structured_error_context: AtomicPtr<c_void>,
}

/*
 * Mutex to protect "ForNewThreads" variables
 */
static mut XML_THR_DEF_MUTEX: XmlMutex = unsafe { zeroed() };

/**
 * xmlBufferAllocScheme:
 *
 * DEPRECATED: Don't use.
 *
 * Global setting, default allocation policy for buffers, default is
 * XML_BUFFER_ALLOC_EXACT
 */
pub(crate) static mut _XML_BUFFER_ALLOC_SCHEME: XmlBufferAllocationScheme =
    XmlBufferAllocationScheme::XmlBufferAllocExact;
static mut XML_BUFFER_ALLOC_SCHEME_THR_DEF: XmlBufferAllocationScheme =
    XmlBufferAllocationScheme::XmlBufferAllocExact;

/**
 * xmlDefaultBufferSize:
 *
 * DEPRECATED: Don't use.
 *
 * Global setting, default buffer size. Default value is BASE_BUFFER_SIZE
 */
pub(crate) static mut _XML_DEFAULT_BUFFER_SIZE: c_int = BASE_BUFFER_SIZE as i32;
pub(crate) static mut XML_DEFAULT_BUFFER_SIZE_THR_DEF: c_int = BASE_BUFFER_SIZE as i32;

/**
 * xmlDoValidityCheckingDefaultValue:
 *
 * DEPRECATED: Use the modern options API with XML_PARSE_DTDVALID.
 *
 * Global setting, indicate that the parser should work in validating mode.
 * Disabled by default.
 */
pub(crate) static _XML_DO_VALIDITY_CHECKING_DEFAULT_VALUE: AtomicI32 = AtomicI32::new(0);
static mut XML_DO_VALIDITY_CHECKING_DEFAULT_VALUE_THR_DEF: c_int = 0;

/**
 * xmlGetWarningsDefaultValue:
 *
 * DEPRECATED: Don't use
 *
 * Global setting, indicate that the DTD validation should provide warnings.
 * Activated by default.
 */
pub(crate) static _XML_GET_WARNINGS_DEFAULT_VALUE: AtomicI32 = AtomicI32::new(1);
static mut XML_GET_WARNINGS_DEFAULT_VALUE_THR_DEF: c_int = 1;

/*
 * output defaults
 */
/**
 * xmlIndentTreeOutput:
 *
 * Global setting, asking the serializer to indent the output tree by default
 * Enabled by default
 */
pub(crate) static _XML_INDENT_TREE_OUTPUT: AtomicI32 = AtomicI32::new(1);
static mut XML_INDENT_TREE_OUTPUT_THR_DEF: c_int = 1;

/**
 * xmlTreeIndentString:
 *
 * The string used to do one-level indent. By default is equal to "  " (two spaces)
 */
// pub(crate) static _XML_TREE_INDENT_STRING: AtomicPtr<c_char> = AtomicPtr::new(c"  ".as_ptr() as _);
// static XML_TREE_INDENT_STRING_THR_DEF: AtomicPtr<c_char> = AtomicPtr::new(c"  ".as_ptr() as _);

/**
 * xmlKeepBlanksDefaultValue:
 *
 * DEPRECATED: Use the modern options API with XML_PARSE_NOBLANKS.
 *
 * Global setting, indicate that the parser should keep all blanks
 * nodes found in the content
 * Activated by default, this is actually needed to have the parser
 * conformant to the XML Recommendation, however the option is kept
 * for some applications since this was libxml1 default behaviour.
 */
pub(crate) static _XML_KEEP_BLANKS_DEFAULT_VALUE: AtomicI32 = AtomicI32::new(1);
static mut XML_KEEP_BLANKS_DEFAULT_VALUE_THR_DEF: c_int = 1;

/**
 * xmlLineNumbersDefaultValue:
 *
 * DEPRECATED: The modern options API always enables line numbers.
 *
 * Global setting, indicate that the parser should store the line number
 * in the content field of elements in the DOM tree.
 * Disabled by default since this may not be safe for old classes of
 * application.
 */
pub(crate) static _XML_LINE_NUMBERS_DEFAULT_VALUE: AtomicI32 = AtomicI32::new(0);
static mut XML_LINE_NUMBERS_DEFAULT_VALUE_THR_DEF: c_int = 0;

/**
 * xmlLoadExtDtdDefaultValue:
 *
 * DEPRECATED: Use the modern options API with XML_PARSE_DTDLOAD.
 *
 * Global setting, indicate that the parser should load DTD while not
 * validating.
 * Disabled by default.
 */
pub(crate) static _XML_LOAD_EXT_DTD_DEFAULT_VALUE: AtomicI32 = AtomicI32::new(0);
static mut XML_LOAD_EXT_DTD_DEFAULT_VALUE_THR_DEF: c_int = 0;

/**
 * xmlParserDebugEntities:
 *
 * DEPRECATED: Don't use
 *
 * Global setting, asking the parser to print out debugging information.
 * while handling entities.
 * Disabled by default
 */
pub(crate) static _XML_PARSER_DEBUG_ENTITIES: AtomicI32 = AtomicI32::new(0);
static mut XML_PARSER_DEBUG_ENTITIES_THR_DEF: c_int = 0;

/**
 * xmlPedanticParserDefaultValue:
 *
 * DEPRECATED: Use the modern options API with XML_PARSE_PEDANTIC.
 *
 * Global setting, indicate that the parser be pedantic
 * Disabled by default.
 */
pub(crate) static _XML_PEDANTIC_PARSER_DEFAULT_VALUE: AtomicI32 = AtomicI32::new(0);
static mut XML_PEDANTIC_PARSER_DEFAULT_VALUE_THR_DEF: c_int = 0;

/**
 * xmlSaveNoEmptyTags:
 *
 * Global setting, asking the serializer to not output empty tags
 * as <empty/> but <empty></empty>. those two forms are indistinguishable
 * once parsed.
 * Disabled by default
 */
pub(crate) static _XML_SAVE_NO_EMPTY_TAGS: AtomicI32 = AtomicI32::new(0);
static mut XML_SAVE_NO_EMPTY_TAGS_THR_DEF: c_int = 0;

/**
 * xmlSubstituteEntitiesDefaultValue:
 *
 * DEPRECATED: Use the modern options API with XML_PARSE_NOENT.
 *
 * Global setting, indicate that the parser should not generate entity
 * references but replace them with the actual content of the entity
 * Disabled by default, this should be activated when using XPath since
 * the XPath data model requires entities replacement and the XPath
 * engine does not handle entities references transparently.
 */
pub(crate) static _XML_SUBSTITUTE_ENTITIES_DEFAULT_VALUE: AtomicI32 = AtomicI32::new(0);
static mut XML_SUBSTITUTE_ENTITIES_DEFAULT_VALUE_THR_DEF: c_int = 0;

// /**
//  * xmlGenericError:
//  *
//  * Global setting: function used for generic error callbacks
//  */
// pub(crate) static mut _XML_GENERIC_ERROR: Option<XmlGenericErrorFunc> =
//     Some(xml_generic_error_default_func);
// static mut XML_GENERIC_ERROR_THR_DEF: Option<XmlGenericErrorFunc> =
//     Some(xml_generic_error_default_func);

/**
 * xmlStructuredError:
 *
 * Global setting: function used for structured error callbacks
 */
// pub(crate) static mut _XML_STRUCTURED_ERROR: Option<XmlStructuredErrorFunc> = None;
// static mut XML_STRUCTURED_ERROR_THR_DEF: Option<XmlStructuredErrorFunc> = None;

/**
 * xmlGenericErrorContext:
 *
 * Global setting passed to generic error callbacks
 */
pub(crate) static _XML_GENERIC_ERROR_CONTEXT: AtomicPtr<c_void> = AtomicPtr::new(null_mut());
static mut XML_GENERIC_ERROR_CONTEXT_THR_DEF: AtomicPtr<c_void> = AtomicPtr::new(null_mut());

/**
 * xmlStructuredErrorContext:
 *
 * Global setting passed to structured error callbacks
 */
pub(crate) static _XML_STRUCTURED_ERROR_CONTEXT: AtomicPtr<c_void> = AtomicPtr::new(null_mut());
static mut XML_STRUCTURED_ERROR_CONTEXT_THR_DEF: AtomicPtr<c_void> = AtomicPtr::new(null_mut());

/**
 * xmlRegisterNodeDefaultValue:
 *
 * DEPRECATED: Don't use
 */
pub(crate) static mut _XML_REGISTER_NODE_DEFAULT_VALUE: Option<XmlRegisterNodeFunc> = None;
static mut XML_REGISTER_NODE_DEFAULT_VALUE_THR_DEF: Option<XmlRegisterNodeFunc> = None;

/**
 * xmlDeregisterNodeDefaultValue:
 *
 * DEPRECATED: Don't use
 */
pub(crate) static mut _XML_DEREGISTER_NODE_DEFAULT_VALUE: Option<XmlDeregisterNodeFunc> = None;
static mut XML_DEREGISTER_NODE_DEFAULT_VALUE_THR_DEF: Option<XmlDeregisterNodeFunc> = None;

/**
 * xmlParserInputBufferCreateFilenameValue:
 *
 * DEPRECATED: Don't use
 */
pub(crate) static mut _XML_PARSER_INPUT_BUFFER_CREATE_FILENAME_VALUE: Option<
    XmlParserInputBufferCreateFilenameFunc,
> = None;
pub(crate) static mut XML_PARSER_INPUT_BUFFER_CREATE_FILENAME_VALUE_THR_DEF: Option<
    XmlParserInputBufferCreateFilenameFunc,
> = None;

/**
 * xmlOutputBufferCreateFilenameValue:
 *
 * DEPRECATED: Don't use
 */
pub(crate) static mut _XML_OUTPUT_BUFFER_CREATE_FILENAME_VALUE: Option<
    XmlOutputBufferCreateFilenameFunc,
> = None;
pub(crate) static mut XML_OUTPUT_BUFFER_CREATE_FILENAME_VALUE_THR_DEF: Option<
    XmlOutputBufferCreateFilenameFunc,
> = None;

/**
 * xmlInitializeGlobalState:
 * @gs: a pointer to a newly allocated global state
 *
 * xmlInitializeGlobalState() initialize a global state with all the
 * default values of the library.
 */
pub unsafe extern "C" fn xml_initialize_global_state(gs: XmlGlobalStatePtr) {
    xml_mutex_lock(addr_of_mut!(XML_THR_DEF_MUTEX));

    #[cfg(all(feature = "html", feature = "legacy", feature = "sax1"))]
    {
        inithtmlDefaultSAXHandler(addr_of_mut!((*gs).html_default_sax_handler));
    }

    (*gs).old_xml_wd_compatibility = 0;
    (*gs).xml_buffer_alloc_scheme = XML_BUFFER_ALLOC_SCHEME_THR_DEF;
    (*gs).xml_default_buffer_size = XML_DEFAULT_BUFFER_SIZE_THR_DEF;
    #[cfg(all(feature = "sax1", feature = "legacy"))]
    {
        initxmlDefaultSAXHandler(addr_of_mut!((*gs).xml_default_sax_handler), 1);
    }
    (*gs).xml_default_sax_locator.get_public_id = xml_sax2_get_public_id;
    (*gs).xml_default_sax_locator.get_system_id = xml_sax2_get_system_id;
    (*gs).xml_default_sax_locator.get_line_number = xml_sax2_get_line_number;
    (*gs).xml_default_sax_locator.get_column_number = xml_sax2_get_column_number;
    (*gs).xml_do_validity_checking_default_value = XML_DO_VALIDITY_CHECKING_DEFAULT_VALUE_THR_DEF;
    // #if defined(DEBUG_MEMORY_LOCATION) | defined(DEBUG_MEMORY)
    //     {
    //         (*gs).xmlFree = (xmlFreeFunc) xmlMemFree;
    //         (*gs).xmlMalloc = (xmlMallocFunc) xmlMemMalloc;
    //         (*gs).xmlMallocAtomic = (xmlMallocFunc) xmlMemMalloc;
    //         (*gs).xmlRealloc = (xmlReallocFunc) xmlMemRealloc;
    //         (*gs).xmlMemStrdup = (xmlStrdupFunc) xmlMemoryStrdup;
    //     }
    // #else
    (*gs).xml_free = Some(free as XmlFreeFunc);
    (*gs).xml_malloc = Some(malloc as XmlMallocFunc);
    (*gs).xml_malloc_atomic = Some(malloc as XmlMallocFunc);
    (*gs).xml_realloc = Some(realloc as XmlReallocFunc);
    (*gs).xml_mem_strdup = Some(xml_strdup as XmlStrdupFunc);
    // #endif
    (*gs).xml_get_warnings_default_value = XML_GET_WARNINGS_DEFAULT_VALUE_THR_DEF;
    (*gs).xml_indent_tree_output = XML_INDENT_TREE_OUTPUT_THR_DEF;
    // (*gs).xml_tree_indent_string = XML_TREE_INDENT_STRING_THR_DEF.load(Ordering::Relaxed) as _;
    (*gs).xml_keep_blanks_default_value = XML_KEEP_BLANKS_DEFAULT_VALUE_THR_DEF;
    (*gs).xml_line_numbers_default_value = XML_LINE_NUMBERS_DEFAULT_VALUE_THR_DEF;
    (*gs).xml_load_ext_dtd_default_value = XML_LOAD_EXT_DTD_DEFAULT_VALUE_THR_DEF;
    (*gs).xml_parser_debug_entities = XML_PARSER_DEBUG_ENTITIES_THR_DEF;
    (*gs).xml_parser_version = LIBXML_VERSION_STRING.as_ptr();
    (*gs).xml_pedantic_parser_default_value = XML_PEDANTIC_PARSER_DEFAULT_VALUE_THR_DEF;
    (*gs).xml_save_no_empty_tags = XML_SAVE_NO_EMPTY_TAGS_THR_DEF;
    (*gs).xml_substitute_entities_default_value = XML_SUBSTITUTE_ENTITIES_DEFAULT_VALUE_THR_DEF;

    // (*gs).xml_generic_error = XML_GENERIC_ERROR_THR_DEF;
    // (*gs).xml_structured_error = XML_STRUCTURED_ERROR_THR_DEF;
    // (*gs).xml_generic_error_context.store(
    //     XML_GENERIC_ERROR_CONTEXT_THR_DEF.load(Ordering::Relaxed),
    //     Ordering::Relaxed,
    // );
    (*gs).xml_structured_error_context.store(
        XML_STRUCTURED_ERROR_CONTEXT_THR_DEF.load(Ordering::Relaxed),
        Ordering::Relaxed,
    );
    (*gs).xml_register_node_default_value = XML_REGISTER_NODE_DEFAULT_VALUE_THR_DEF;
    (*gs).xml_deregister_node_default_value = XML_DEREGISTER_NODE_DEFAULT_VALUE_THR_DEF;

    (*gs).xml_parser_input_buffer_create_filename_value =
        XML_PARSER_INPUT_BUFFER_CREATE_FILENAME_VALUE_THR_DEF;
    (*gs).xml_output_buffer_create_filename_value = XML_OUTPUT_BUFFER_CREATE_FILENAME_VALUE_THR_DEF;
    memset(
        addr_of_mut!((*gs).xml_last_error) as _,
        0,
        size_of::<XmlError>(),
    );

    xml_mutex_unlock(addr_of_mut!(XML_THR_DEF_MUTEX));
}

/**
 * xmlRegisterNodeDefault:
 * @func: function pointer to the new RegisterNodeFunc
 *
 * Registers a callback for node creation
 *
 * Returns the old value of the registration function
 */
pub unsafe extern "C" fn xml_register_node_default(
    func: Option<XmlRegisterNodeFunc>,
) -> Option<XmlRegisterNodeFunc> {
    let old = _XML_REGISTER_NODE_DEFAULT_VALUE;

    __XML_REGISTER_CALLBACKS.store(1, Ordering::Relaxed);
    _XML_REGISTER_NODE_DEFAULT_VALUE = func;
    old
}

pub unsafe extern "C" fn xml_thr_def_register_node_default(
    func: XmlRegisterNodeFunc,
) -> Option<XmlRegisterNodeFunc> {
    xml_mutex_lock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let old = XML_REGISTER_NODE_DEFAULT_VALUE_THR_DEF;

    __XML_REGISTER_CALLBACKS.store(1, Ordering::Relaxed);
    XML_REGISTER_NODE_DEFAULT_VALUE_THR_DEF = Some(func);
    xml_mutex_unlock(addr_of_mut!(XML_THR_DEF_MUTEX));

    old
}

/**
 * xmlDeregisterNodeDefault:
 * @func: function pointer to the new DeregisterNodeFunc
 *
 * Registers a callback for node destruction
 *
 * Returns the previous value of the deregistration function
 */
pub unsafe extern "C" fn xml_deregister_node_default(
    func: XmlDeregisterNodeFunc,
) -> Option<XmlDeregisterNodeFunc> {
    let old = _XML_DEREGISTER_NODE_DEFAULT_VALUE;

    __XML_REGISTER_CALLBACKS.store(1, Ordering::Relaxed);
    _XML_DEREGISTER_NODE_DEFAULT_VALUE = Some(func);
    old
}

pub unsafe extern "C" fn xml_thr_def_deregister_node_default(
    func: XmlDeregisterNodeFunc,
) -> Option<XmlDeregisterNodeFunc> {
    xml_mutex_lock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let old = XML_DEREGISTER_NODE_DEFAULT_VALUE_THR_DEF;

    __XML_REGISTER_CALLBACKS.store(1, Ordering::Relaxed);
    XML_DEREGISTER_NODE_DEFAULT_VALUE_THR_DEF = Some(func);
    xml_mutex_unlock(addr_of_mut!(XML_THR_DEF_MUTEX));

    old
}

pub unsafe fn xml_thr_def_output_buffer_create_filename_default(
    func: XmlOutputBufferCreateFilenameFunc,
) -> Option<XmlOutputBufferCreateFilenameFunc> {
    xml_mutex_lock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let mut old = XML_OUTPUT_BUFFER_CREATE_FILENAME_VALUE_THR_DEF;
    #[cfg(feature = "output")]
    {
        if old.is_none() {
            old = Some(__xml_output_buffer_create_filename);
        }
    }
    XML_OUTPUT_BUFFER_CREATE_FILENAME_VALUE_THR_DEF = Some(func);
    xml_mutex_unlock(addr_of_mut!(XML_THR_DEF_MUTEX));

    old
}

pub unsafe fn xml_thr_def_parser_input_buffer_create_filename_default(
    func: Option<XmlParserInputBufferCreateFilenameFunc>,
) -> XmlParserInputBufferCreateFilenameFunc {
    xml_mutex_lock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let old = XML_PARSER_INPUT_BUFFER_CREATE_FILENAME_VALUE_THR_DEF
        .unwrap_or(__xml_parser_input_buffer_create_filename);

    XML_PARSER_INPUT_BUFFER_CREATE_FILENAME_VALUE_THR_DEF = func;
    xml_mutex_unlock(addr_of_mut!(XML_THR_DEF_MUTEX));

    old
}

/*
 * Helpful Macro
 */
#[macro_export]
#[cfg(feature = "thread")]
macro_rules! IS_MAIN_THREAD {
    () => {
        $crate::libxml::threads::xml_is_main_thread()
    };
}
#[macro_export]
#[cfg(not(feature = "thread"))]
macro_rules! IS_MAIN_THREAD {
    () => {
        1
    };
}

/*
 * In general the memory allocation entry points are not kept
 * thread specific but this can be overridden by LIBXML_THREAD_ALLOC_ENABLED
 *    - xmlMalloc
 *    - xmlMallocAtomic
 *    - xmlRealloc
 *    - xmlMemStrdup
 *    - xmlFree
 */
/**
 * xmlMalloc:
 * @size:  the size requested in bytes
 *
 * The variable holding the libxml malloc() implementation
 *
 * Returns a pointer to the newly allocated block or NULL in case of error
 */
pub(in crate::libxml) static mut _XML_MALLOC: Option<XmlMallocFunc> = Some(malloc);
#[cfg(not(feature = "thread_alloc"))]
pub unsafe extern "C" fn xml_malloc(size: usize) -> *mut c_void {
    _XML_MALLOC.expect("Failed to allocate memory : _XML_MALLOC is None")(size)
}
#[cfg(feature = "thread_alloc")]
pub unsafe extern "C" fn xml_malloc(size: usize) -> *mut c_void {
    __xml_malloc().expect("Failed to allocate memory : _XML_MALLOC is None")(size)
}

pub fn set_xml_malloc(malloc: Option<XmlMallocFunc>) {
    unsafe {
        _XML_MALLOC = malloc;
        (*xml_get_global_state()).xml_malloc = malloc;
    }
}
/**
 * xmlMallocAtomic:
 * @size:  the size requested in bytes
 *
 * The variable holding the libxml malloc() implementation for atomic
 * data (i.e. blocks not containing pointers), useful when using a
 * garbage collecting allocator.
 *
 * Returns a pointer to the newly allocated block or NULL in case of error
 */
pub(in crate::libxml) static mut _XML_MALLOC_ATOMIC: XmlMallocFunc = malloc;
#[cfg(not(feature = "thread_alloc"))]
pub unsafe extern "C" fn xml_malloc_atomic(size: usize) -> *mut c_void {
    _XML_MALLOC_ATOMIC(size)
}
#[cfg(feature = "thread_alloc")]
pub unsafe extern "C" fn xml_malloc_atomic(size: usize) -> *mut c_void {
    __xml_malloc_atomic()(size)
}
/**
 * xmlRealloc:
 * @mem: an already allocated block of memory
 * @size:  the new size requested in bytes
 *
 * The variable holding the libxml realloc() implementation
 *
 * Returns a pointer to the newly reallocated block or NULL in case of error
 */
pub(in crate::libxml) static mut _XML_REALLOC: Option<XmlReallocFunc> = Some(realloc);
#[cfg(not(feature = "thread_alloc"))]
pub unsafe extern "C" fn xml_realloc(mem: *mut c_void, size: usize) -> *mut c_void {
    _XML_REALLOC.expect("Failed to reallocate memory : _XML_REALLOC is None")(mem, size)
}
#[cfg(feature = "thread_alloc")]
pub unsafe extern "C" fn xml_realloc(mem: *mut c_void, size: usize) -> *mut c_void {
    __xml_realloc().expect("Failed to reallocate memory : _XML_REALLOC is None")(mem, size)
}
pub fn set_xml_realloc(realloc: Option<XmlReallocFunc>) {
    unsafe {
        _XML_REALLOC = realloc;
        (*xml_get_global_state()).xml_realloc = realloc;
    }
}
/**
 * xmlFree:
 * @mem: an already allocated block of memory
 *
 * The variable holding the libxml free() implementation
 */
pub(in crate::libxml) static mut _XML_FREE: Option<XmlFreeFunc> = Some(free);
#[cfg(not(feature = "thread_alloc"))]
pub unsafe extern "C" fn xml_free(mem: *mut c_void) {
    _XML_FREE.expect("Failed to deallocate memory : _XML_FREE is None")(mem);
}
#[cfg(feature = "thread_alloc")]
pub unsafe extern "C" fn xml_free(mem: *mut c_void) {
    __xml_free().expect("Failed to deallocate memory : _XML_FREE is None")(mem);
}
pub fn set_xml_free(free: Option<XmlFreeFunc>) {
    unsafe {
        _XML_FREE = free;
        (*xml_get_global_state()).xml_free = free;
    }
}

/**
 * xmlPosixStrdup
 * @cur:  the input c_char *
 *
 * a strdup implementation with a type signature matching POSIX
 *
 * Returns a new xmlChar * or NULL
 */
unsafe extern "C" fn xml_posix_strdup(cur: *const XmlChar) -> *mut XmlChar {
    xml_char_strdup(cur as _) as _
}
/**
 * xmlMemStrdup:
 * @str: a zero terminated string
 *
 * The variable holding the libxml strdup() implementation
 *
 * Returns the copy of the string or NULL in case of error
 */
pub(in crate::libxml) static mut _XML_MEM_STRDUP: Option<XmlStrdupFunc> = Some(xml_posix_strdup);
#[cfg(not(feature = "thread_alloc"))]
pub unsafe extern "C" fn xml_mem_strdup(str: *const XmlChar) -> *mut XmlChar {
    _XML_MEM_STRDUP.expect("Failed to duplicate xml string : _XML_MEM_STRDUP is None")(str)
}
#[cfg(feature = "thread_alloc")]
pub unsafe extern "C" fn xml_mem_strdup(str: *const XmlChar) -> *mut XmlChar {
    __xml_mem_strdup().expect("Failed to duplicate xml string : _XML_MEM_STRDUP is None")(str)
}
pub fn set_xml_mem_strdup(mem_strdup: Option<XmlStrdupFunc>) {
    unsafe {
        _XML_MEM_STRDUP = mem_strdup;
        (*xml_get_global_state()).xml_mem_strdup = mem_strdup;
    }
}

#[cfg(feature = "thread_alloc")]
mod __globals_internal_for_thread_alloc {
    use super::*;
    use crate::libxml::threads::xml_get_global_state;

    #[cfg(feature = "thread")]
    pub unsafe extern "C" fn __xml_malloc() -> Option<XmlMallocFunc> {
        if IS_MAIN_THREAD!() != 0 {
            _XML_MALLOC
        } else {
            (*xml_get_global_state()).xml_malloc
        }
    }
    #[cfg(not(feature = "thread"))]
    pub unsafe extern "C" fn __xmlMalloc() -> XmlMallocFunc {
        _XML_MALLOC
    }

    #[cfg(feature = "thread")]
    pub unsafe extern "C" fn __xml_malloc_atomic() -> XmlMallocFunc {
        if IS_MAIN_THREAD!() != 0 {
            _XML_MALLOC_ATOMIC
        } else {
            (*xml_get_global_state()).xml_malloc_atomic.unwrap()
        }
    }
    #[cfg(not(feature = "thread"))]
    pub unsafe extern "C" fn __xmlMallocAtomic() -> XmlMallocFunc {
        _XML_MALLOC_ATOMIC
    }

    #[cfg(feature = "thread")]
    pub unsafe extern "C" fn __xml_realloc() -> Option<XmlReallocFunc> {
        if IS_MAIN_THREAD!() != 0 {
            _XML_REALLOC
        } else {
            (*xml_get_global_state()).xml_realloc
        }
    }
    #[cfg(not(feature = "thread"))]
    pub unsafe extern "C" fn __xmlRealloc() -> XmlReallocFunc {
        _XML_REALLOC
    }

    #[cfg(feature = "thread")]
    pub unsafe extern "C" fn __xml_free() -> Option<XmlFreeFunc> {
        if IS_MAIN_THREAD!() != 0 {
            _XML_FREE
        } else {
            (*xml_get_global_state()).xml_free
        }
    }
    #[cfg(not(feature = "thread"))]
    pub unsafe extern "C" fn __xmlFree() -> XmlFreeFunc {
        _XML_FREE
    }

    #[cfg(feature = "thread")]
    pub unsafe extern "C" fn __xml_mem_strdup() -> Option<XmlStrdupFunc> {
        if IS_MAIN_THREAD!() != 0 {
            _XML_MEM_STRDUP
        } else {
            (*xml_get_global_state()).xml_mem_strdup
        }
    }
    #[cfg(not(feature = "thread"))]
    pub unsafe extern "C" fn __xmlMemStrdup() -> XmlStrdupFunc {
        _XML_MEM_STRDUP
    }
}
#[cfg(feature = "thread_alloc")]
pub use __globals_internal_for_thread_alloc::*;

/**
 * htmlDefaultSAXHandler:
 *
 * DEPRECATED: This handler is unused and will be removed from future
 * versions.
 *
 * Default old SAX v1 handler for HTML, builds the DOM tree
 */
#[cfg(all(feature = "html", feature = "sax1"))]
static mut _HTML_DEFAULT_SAXHANDLER: XmlSAXHandlerV1 = XmlSAXHandlerV1 {
    internal_subset: Some(xml_sax2_internal_subset),
    is_standalone: None,
    has_internal_subset: None,
    has_external_subset: None,
    resolve_entity: None,
    get_entity: Some(xml_sax2_get_entity),
    entity_decl: None,
    notation_decl: None,
    attribute_decl: None,
    element_decl: None,
    unparsed_entity_decl: None,
    set_document_locator: Some(xml_sax2_set_document_locator),
    start_document: Some(xml_sax2_start_document),
    end_document: Some(xml_sax2_end_document),
    start_element: Some(xml_sax2_start_element),
    end_element: Some(xml_sax2_end_element),
    reference: None,
    characters: Some(xml_sax2_characters),
    ignorable_whitespace: Some(xml_sax2_ignorable_whitespace),
    processing_instruction: Some(xml_sax2_processing_instruction),
    comment: Some(xml_sax2_comment),
    warning: Some(parser_warning),
    error: Some(parser_error),
    fatal_error: Some(parser_error),
    get_parameter_entity: None,
    cdata_block: Some(xml_sax2_cdata_block),
    external_subset: None,
    initialized: 1,
};

#[cfg(feature = "html")]
mod __globals_internal_for_html {
    use super::*;

    #[deprecated]
    pub unsafe extern "C" fn __html_default_sax_handler() -> *mut XmlSAXHandlerV1 {
        if IS_MAIN_THREAD!() != 0 {
            addr_of_mut!(_HTML_DEFAULT_SAXHANDLER)
        } else {
            addr_of_mut!((*xml_get_global_state()).html_default_sax_handler)
        }
    }
    #[cfg(feature = "thread")]
    pub unsafe extern "C" fn html_default_sax_handler() -> *mut XmlSAXHandlerV1 {
        __html_default_sax_handler()
    }
    #[deprecated]
    #[cfg(not(feature = "thread"))]
    pub unsafe extern "C" fn html_default_sax_handler() -> *mut XmlSAXHandlerV1 {
        addr_of_mut!(_HTML_DEFAULT_SAXHANDLER)
    }
}
#[cfg(feature = "html")]
pub use __globals_internal_for_html::*;

/*
 * Everything starting from the line below is
 * Automatically generated by build_glob.py.
 * Do not modify the previous line.
 */

#[deprecated]
pub unsafe extern "C" fn __old_xml_wd_compatibility() -> *mut c_int {
    if IS_MAIN_THREAD!() != 0 {
        addr_of_mut!(_OLD_XMLWDCOMPATIBILITY)
    } else {
        addr_of_mut!((*xml_get_global_state()).old_xml_wd_compatibility)
    }
}

/**
 * oldXMLWDcompatibility:
 *
 * Global setting, DEPRECATED.
 */
static mut _OLD_XMLWDCOMPATIBILITY: c_int = 0; /* DEPRECATED */
#[cfg(feature = "thread")]
pub unsafe extern "C" fn old_xml_wd_compatibility() -> *mut c_int {
    __old_xml_wd_compatibility()
}
#[deprecated]
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn old_xml_wd_compatibility() -> *mut c_int {
    addr_of_mut!(_OLD_XMLWDCOMPATIBILITY)
}

#[deprecated]
pub unsafe extern "C" fn __xml_buffer_alloc_scheme() -> *mut XmlBufferAllocationScheme {
    if IS_MAIN_THREAD!() != 0 {
        addr_of_mut!(_XML_BUFFER_ALLOC_SCHEME)
    } else {
        addr_of_mut!((*xml_get_global_state()).xml_buffer_alloc_scheme)
    }
}
#[cfg(feature = "thread")]
pub unsafe extern "C" fn xml_buffer_alloc_scheme() -> *mut XmlBufferAllocationScheme {
    __xml_buffer_alloc_scheme()
}
#[deprecated]
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xml_buffer_alloc_scheme() -> *mut XmlBufferAllocationScheme {
    addr_of_mut!(_XML_BUFFER_ALLOC_SCHEME)
}
#[deprecated]
pub unsafe extern "C" fn xml_thr_def_buffer_alloc_scheme(
    v: XmlBufferAllocationScheme,
) -> XmlBufferAllocationScheme {
    xml_mutex_lock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let ret: XmlBufferAllocationScheme = XML_BUFFER_ALLOC_SCHEME_THR_DEF;
    XML_BUFFER_ALLOC_SCHEME_THR_DEF = v;
    xml_mutex_unlock(addr_of_mut!(XML_THR_DEF_MUTEX));
    ret
}

#[deprecated]
pub unsafe extern "C" fn __xml_default_buffer_size() -> *mut c_int {
    if IS_MAIN_THREAD!() != 0 {
        addr_of_mut!(_XML_DEFAULT_BUFFER_SIZE)
    } else {
        addr_of_mut!((*xml_get_global_state()).xml_default_buffer_size)
    }
}

#[cfg(feature = "thread")]
pub unsafe extern "C" fn xml_default_buffer_size() -> *mut c_int {
    __xml_default_buffer_size()
}
#[deprecated]
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xml_default_buffer_size() -> *mut c_int {
    addr_of_mut!(_XML_DEFAULT_BUFFER_SIZE)
}

#[deprecated]
pub unsafe extern "C" fn xml_thr_def_default_buffer_size(v: c_int) -> c_int {
    xml_mutex_lock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let ret: c_int = XML_DEFAULT_BUFFER_SIZE_THR_DEF;
    XML_DEFAULT_BUFFER_SIZE_THR_DEF = v;
    xml_mutex_unlock(addr_of_mut!(XML_THR_DEF_MUTEX));
    ret
}

#[deprecated]
pub unsafe extern "C" fn __xml_default_sax_handler() -> *mut XmlSAXHandlerV1 {
    if IS_MAIN_THREAD!() != 0 {
        addr_of_mut!(_XML_DEFAULT_SAXHANDLER)
    } else {
        addr_of_mut!((*xml_get_global_state()).xml_default_sax_handler)
    }
}

/**
 * xmlDefaultSAXHandler:
 *
 * DEPRECATED: This handler is unused and will be removed from future
 * versions.
 *
 * Default SAX version1 handler for XML, builds the DOM tree
 */
#[cfg(feature = "sax1")]
static mut _XML_DEFAULT_SAXHANDLER: XmlSAXHandlerV1 = XmlSAXHandlerV1 {
    internal_subset: Some(xml_sax2_internal_subset),
    is_standalone: Some(xml_sax2_is_standalone),
    has_internal_subset: Some(xml_sax2_has_internal_subset),
    has_external_subset: Some(xml_sax2_has_external_subset),
    resolve_entity: Some(xml_sax2_resolve_entity),
    get_entity: Some(xml_sax2_get_entity),
    entity_decl: Some(xml_sax2_entity_decl),
    notation_decl: Some(xml_sax2_notation_decl),
    attribute_decl: Some(xml_sax2_attribute_decl),
    element_decl: Some(xml_sax2_element_decl),
    unparsed_entity_decl: Some(xml_sax2_unparsed_entity_decl),
    set_document_locator: Some(xml_sax2_set_document_locator),
    start_document: Some(xml_sax2_start_document),
    end_document: Some(xml_sax2_end_document),
    start_element: Some(xml_sax2_start_element),
    end_element: Some(xml_sax2_end_element),
    reference: Some(xml_sax2_reference),
    characters: Some(xml_sax2_characters),
    ignorable_whitespace: Some(xml_sax2_characters),
    processing_instruction: Some(xml_sax2_processing_instruction),
    comment: Some(xml_sax2_comment),
    warning: Some(parser_warning),
    error: Some(parser_error),
    fatal_error: Some(parser_error),
    get_parameter_entity: Some(xml_sax2_get_parameter_entity),
    cdata_block: Some(xml_sax2_cdata_block),
    external_subset: Some(xml_sax2_external_subset),
    initialized: 1,
};

#[cfg(feature = "thread")]
pub unsafe extern "C" fn xml_default_sax_handler() -> *mut XmlSAXHandlerV1 {
    __xml_default_sax_handler()
}
#[deprecated]
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xml_default_sax_handler() -> *mut XmlSAXHandlerV1 {
    addr_of_mut!(_XML_DEFAULT_SAXHANDLER)
}

#[deprecated]
pub unsafe extern "C" fn __xml_default_sax_locator() -> *mut XmlSaxlocator {
    if IS_MAIN_THREAD!() != 0 {
        addr_of_mut!(_XML_DEFAULT_SAXLOCATOR)
    } else {
        addr_of_mut!((*xml_get_global_state()).xml_default_sax_locator)
    }
}

/**
 * xmlDefaultSAXLocator:
 *
 * DEPRECATED: Don't use
 *
 * The default SAX Locator
 * { getPublicId, getSystemId, getLineNumber, getColumnNumber}
 */
static mut _XML_DEFAULT_SAXLOCATOR: XmlSaxlocator = XmlSaxlocator {
    get_public_id: xml_sax2_get_public_id,
    get_system_id: xml_sax2_get_system_id,
    get_line_number: xml_sax2_get_line_number,
    get_column_number: xml_sax2_get_column_number,
};

#[cfg(feature = "thread")]
pub unsafe extern "C" fn xml_default_sax_locator() -> *mut XmlSaxlocator {
    __xml_default_sax_locator()
}
#[deprecated]
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xml_default_sax_locator() -> *mut XmlSaxlocator {
    addr_of_mut!(_XML_DEFAULT_SAXLOCATOR)
}

pub unsafe extern "C" fn __xml_do_validity_checking_default_value() -> *mut c_int {
    if IS_MAIN_THREAD!() != 0 {
        _XML_DO_VALIDITY_CHECKING_DEFAULT_VALUE.as_ptr()
    } else {
        addr_of_mut!((*xml_get_global_state()).xml_do_validity_checking_default_value)
    }
}
#[cfg(feature = "thread")]
pub unsafe extern "C" fn xml_do_validity_checking_default_value() -> *mut c_int {
    __xml_do_validity_checking_default_value()
}
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xml_do_validity_checking_default_value() -> *mut c_int {
    _XML_DO_VALIDITY_CHECKING_DEFAULT_VALUE.as_ptr()
}

pub unsafe extern "C" fn xml_thr_def_do_validity_checking_default_value(v: c_int) -> c_int {
    xml_mutex_lock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let ret: c_int = XML_DO_VALIDITY_CHECKING_DEFAULT_VALUE_THR_DEF;
    XML_DO_VALIDITY_CHECKING_DEFAULT_VALUE_THR_DEF = v;
    xml_mutex_unlock(addr_of_mut!(XML_THR_DEF_MUTEX));
    ret
}

pub unsafe extern "C" fn __xml_get_warnings_default_value() -> *mut c_int {
    if IS_MAIN_THREAD!() != 0 {
        _XML_GET_WARNINGS_DEFAULT_VALUE.as_ptr()
    } else {
        addr_of_mut!((*xml_get_global_state()).xml_get_warnings_default_value)
    }
}
#[cfg(feature = "thread")]
pub unsafe extern "C" fn xml_get_warnings_default_value() -> *mut c_int {
    __xml_get_warnings_default_value()
}
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xml_get_warnings_default_value() -> *mut c_int {
    _XML_GET_WARNINGS_DEFAULT_VALUE.as_ptr()
}

pub unsafe extern "C" fn xml_thr_def_get_warnings_default_value(v: c_int) -> c_int {
    xml_mutex_lock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let ret: c_int = XML_GET_WARNINGS_DEFAULT_VALUE_THR_DEF;
    XML_GET_WARNINGS_DEFAULT_VALUE_THR_DEF = v;
    xml_mutex_unlock(addr_of_mut!(XML_THR_DEF_MUTEX));
    ret
}

pub unsafe extern "C" fn __xml_indent_tree_output() -> *mut c_int {
    if IS_MAIN_THREAD!() != 0 {
        _XML_INDENT_TREE_OUTPUT.as_ptr()
    } else {
        addr_of_mut!((*xml_get_global_state()).xml_indent_tree_output)
    }
}
#[cfg(feature = "thread")]
pub unsafe extern "C" fn xml_indent_tree_output() -> *mut c_int {
    __xml_indent_tree_output()
}
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xml_indent_tree_output() -> *mut c_int {
    _XML_INDENT_TREE_OUTPUT.as_ptr()
}

pub unsafe extern "C" fn xml_thr_def_indent_tree_output(v: c_int) -> c_int {
    xml_mutex_lock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let ret: c_int = XML_INDENT_TREE_OUTPUT_THR_DEF;
    XML_INDENT_TREE_OUTPUT_THR_DEF = v;
    xml_mutex_unlock(addr_of_mut!(XML_THR_DEF_MUTEX));
    ret
}

pub unsafe extern "C" fn __xml_keep_blanks_default_value() -> *mut c_int {
    if IS_MAIN_THREAD!() != 0 {
        _XML_KEEP_BLANKS_DEFAULT_VALUE.as_ptr()
    } else {
        addr_of_mut!((*xml_get_global_state()).xml_keep_blanks_default_value)
    }
}
#[cfg(feature = "thread")]
pub unsafe extern "C" fn xml_keep_blanks_default_value() -> *mut c_int {
    __xml_keep_blanks_default_value()
}
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xml_keep_blanks_default_value() -> *mut c_int {
    _XML_KEEP_BLANKS_DEFAULT_VALUE.as_ptr()
}

pub unsafe extern "C" fn xml_thr_def_keep_blanks_default_value(v: c_int) -> c_int {
    xml_mutex_lock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let ret: c_int = XML_KEEP_BLANKS_DEFAULT_VALUE_THR_DEF;
    XML_KEEP_BLANKS_DEFAULT_VALUE_THR_DEF = v;
    xml_mutex_unlock(addr_of_mut!(XML_THR_DEF_MUTEX));
    ret
}

#[deprecated]
pub unsafe extern "C" fn __xml_line_numbers_default_value() -> *mut c_int {
    if IS_MAIN_THREAD!() != 0 {
        _XML_LINE_NUMBERS_DEFAULT_VALUE.as_ptr()
    } else {
        addr_of_mut!((*xml_get_global_state()).xml_line_numbers_default_value)
    }
}
#[cfg(feature = "thread")]
pub unsafe extern "C" fn xml_line_numbers_default_value() -> *mut c_int {
    __xml_line_numbers_default_value()
}
#[deprecated]
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xml_line_numbers_default_value() -> *mut c_int {
    _XML_LINE_NUMBERS_DEFAULT_VALUE.as_ptr()
}

#[deprecated]
pub unsafe extern "C" fn xml_thr_def_line_numbers_default_value(v: c_int) -> c_int {
    xml_mutex_lock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let ret: c_int = XML_LINE_NUMBERS_DEFAULT_VALUE_THR_DEF;
    XML_LINE_NUMBERS_DEFAULT_VALUE_THR_DEF = v;
    xml_mutex_unlock(addr_of_mut!(XML_THR_DEF_MUTEX));
    ret
}

pub unsafe extern "C" fn __xml_load_ext_dtd_default_value() -> *mut c_int {
    if IS_MAIN_THREAD!() != 0 {
        _XML_LOAD_EXT_DTD_DEFAULT_VALUE.as_ptr()
    } else {
        addr_of_mut!((*xml_get_global_state()).xml_load_ext_dtd_default_value)
    }
}
#[cfg(feature = "thread")]
pub unsafe extern "C" fn xml_load_ext_dtd_default_value() -> *mut c_int {
    __xml_load_ext_dtd_default_value()
}
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xml_load_ext_dtd_default_value() -> *mut c_int {
    _XML_LOAD_EXT_DTD_DEFAULT_VALUE.as_ptr()
}

pub unsafe extern "C" fn xml_thr_def_load_ext_dtd_default_value(v: c_int) -> c_int {
    xml_mutex_lock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let ret: c_int = XML_LOAD_EXT_DTD_DEFAULT_VALUE_THR_DEF;
    XML_LOAD_EXT_DTD_DEFAULT_VALUE_THR_DEF = v;
    xml_mutex_unlock(addr_of_mut!(XML_THR_DEF_MUTEX));
    ret
}

pub unsafe extern "C" fn __xml_parser_debug_entities() -> *mut c_int {
    if IS_MAIN_THREAD!() != 0 {
        _XML_PARSER_DEBUG_ENTITIES.as_ptr()
    } else {
        addr_of_mut!((*xml_get_global_state()).xml_parser_debug_entities)
    }
}
#[cfg(feature = "thread")]
pub unsafe extern "C" fn xml_parser_debug_entities() -> *mut c_int {
    __xml_parser_debug_entities()
}
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xml_parser_debug_entities() -> *mut c_int {
    _XML_PARSER_DEBUG_ENTITIES.as_ptr()
}

pub unsafe extern "C" fn xml_thr_def_parser_debug_entities(v: c_int) -> c_int {
    xml_mutex_lock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let ret: c_int = XML_PARSER_DEBUG_ENTITIES_THR_DEF;
    XML_PARSER_DEBUG_ENTITIES_THR_DEF = v;
    xml_mutex_unlock(addr_of_mut!(XML_THR_DEF_MUTEX));
    ret
}

// /**
//  * xmlParserVersion:
//  *
//  * Constant string describing the internal version of the library
//  */
// static _xmlParserVersion: AtomicPtr<c_char> =
//     AtomicPtr::new(concatcp!(LIBXML_VERSION_STRING, LIBXML_VERSION_EXTRA));

// pub unsafe extern "C" fn __xmlParserVersion() -> *mut *const c_char {
//     if IS_MAIN_THREAD!() != 0 {
//         _xmlParserVersion
//     } else {
//         addr_of_mut!((*xmlGetGlobalState()).xmlParserVersion)
//     }
// }

// #[cfg(feature = "thread")]
// pub static mut xmlParserVersion: *const c_char = unsafe { *(__xmlParserVersion()) };
// #[cfg(not(feature = "thread"))]
// pub static mut xmlParserVersion: *const c_char;

#[deprecated]
pub unsafe extern "C" fn __xml_pedantic_parser_default_value() -> *mut c_int {
    if IS_MAIN_THREAD!() != 0 {
        _XML_PEDANTIC_PARSER_DEFAULT_VALUE.as_ptr()
    } else {
        addr_of_mut!((*xml_get_global_state()).xml_pedantic_parser_default_value)
    }
}
#[cfg(feature = "thread")]
pub unsafe extern "C" fn xml_pedantic_parser_default_value() -> *mut c_int {
    __xml_pedantic_parser_default_value()
}
#[deprecated]
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xml_pedantic_parser_default_value() -> *mut c_int {
    _XML_PEDANTIC_PARSER_DEFAULT_VALUE.as_ptr()
}

#[deprecated]
pub unsafe extern "C" fn xml_thr_def_pedantic_parser_default_value(v: c_int) -> c_int {
    xml_mutex_lock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let ret: c_int = XML_PEDANTIC_PARSER_DEFAULT_VALUE_THR_DEF;
    XML_PEDANTIC_PARSER_DEFAULT_VALUE_THR_DEF = v;
    xml_mutex_unlock(addr_of_mut!(XML_THR_DEF_MUTEX));
    ret
}

pub unsafe extern "C" fn __xml_save_no_empty_tags() -> *mut c_int {
    if IS_MAIN_THREAD!() != 0 {
        _XML_SAVE_NO_EMPTY_TAGS.as_ptr()
    } else {
        addr_of_mut!((*xml_get_global_state()).xml_save_no_empty_tags)
    }
}
#[cfg(feature = "thread")]
pub unsafe extern "C" fn xml_save_no_empty_tags() -> *mut c_int {
    __xml_save_no_empty_tags()
}
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xml_save_no_empty_tags() -> *mut c_int {
    _XML_SAVE_NO_EMPTY_TAGS.as_ptr()
}

pub unsafe extern "C" fn xml_thr_def_save_no_empty_tags(v: c_int) -> c_int {
    xml_mutex_lock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let ret: c_int = XML_SAVE_NO_EMPTY_TAGS_THR_DEF;
    XML_SAVE_NO_EMPTY_TAGS_THR_DEF = v;
    xml_mutex_unlock(addr_of_mut!(XML_THR_DEF_MUTEX));
    ret
}

pub unsafe extern "C" fn __xml_substitute_entities_default_value() -> *mut c_int {
    if IS_MAIN_THREAD!() != 0 {
        _XML_SUBSTITUTE_ENTITIES_DEFAULT_VALUE.as_ptr()
    } else {
        addr_of_mut!((*xml_get_global_state()).xml_substitute_entities_default_value)
    }
}

#[cfg(feature = "thread")]
pub unsafe extern "C" fn xml_substitute_entities_default_value() -> *mut c_int {
    __xml_substitute_entities_default_value()
}
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xml_substitute_entities_default_value() -> *mut c_int {
    _XML_SUBSTITUTE_ENTITIES_DEFAULT_VALUE.as_ptr()
}

pub unsafe extern "C" fn xml_thr_def_substitute_entities_default_value(v: c_int) -> c_int {
    xml_mutex_lock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let ret: c_int = XML_SUBSTITUTE_ENTITIES_DEFAULT_VALUE_THR_DEF;
    XML_SUBSTITUTE_ENTITIES_DEFAULT_VALUE_THR_DEF = v;
    xml_mutex_unlock(addr_of_mut!(XML_THR_DEF_MUTEX));
    ret
}

#[deprecated]
pub unsafe extern "C" fn __xml_register_node_default_value() -> XmlRegisterNodeFunc {
    if IS_MAIN_THREAD!() != 0 {
        _XML_REGISTER_NODE_DEFAULT_VALUE.unwrap()
    } else {
        (*xml_get_global_state())
            .xml_register_node_default_value
            .unwrap()
    }
}

#[cfg(feature = "thread")]
pub unsafe extern "C" fn xml_register_node_default_value(node: XmlNodePtr) {
    __xml_register_node_default_value()(node)
}
#[deprecated]
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xml_register_node_default_value(node: XmlNodePtr) {
    _XML_REGISTER_NODE_DEFAULT_VALUE.unwrap()(node)
}

#[deprecated]
pub unsafe extern "C" fn __xml_deregister_node_default_value() -> XmlDeregisterNodeFunc {
    if IS_MAIN_THREAD!() != 0 {
        _XML_DEREGISTER_NODE_DEFAULT_VALUE.unwrap()
    } else {
        (*xml_get_global_state())
            .xml_deregister_node_default_value
            .unwrap()
    }
}

#[cfg(feature = "thread")]
pub unsafe extern "C" fn xml_deregister_node_default_value(node: XmlNodePtr) {
    __xml_deregister_node_default_value()(node)
}
#[deprecated]
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xml_deregister_node_default_value(node: XmlNodePtr) {
    _XML_DEREGISTER_NODE_DEFAULT_VALUE.unwrap()(node)
}

pub(crate) unsafe fn __xml_parser_input_buffer_create_filename_value(
) -> Option<XmlParserInputBufferCreateFilenameFunc> {
    if IS_MAIN_THREAD!() != 0 {
        _XML_PARSER_INPUT_BUFFER_CREATE_FILENAME_VALUE
    } else {
        (*xml_get_global_state()).xml_parser_input_buffer_create_filename_value
    }
}

#[cfg(feature = "thread")]
pub unsafe fn xml_parser_input_buffer_create_filename_value(
    uri: *const c_char,
    enc: XmlCharEncoding,
) -> XmlParserInputBufferPtr {
    if let Some(f) = __xml_parser_input_buffer_create_filename_value() {
        f(uri, enc)
    } else {
        null_mut()
    }
}
#[deprecated]
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xml_parser_input_buffer_create_filename_value(
    uri: *const c_char,
    enc: XmlCharEncoding,
) -> XmlParserInputBufferPtr {
    _XML_PARSER_INPUT_BUFFER_CREATE_FILENAME_VALUE.unwrap()(uri, enc)
}

#[deprecated]
pub unsafe fn __xml_output_buffer_create_filename_value(
) -> Option<XmlOutputBufferCreateFilenameFunc> {
    if IS_MAIN_THREAD!() != 0 {
        _XML_OUTPUT_BUFFER_CREATE_FILENAME_VALUE
    } else {
        (*xml_get_global_state()).xml_output_buffer_create_filename_value
    }
}

#[cfg(feature = "thread")]
pub unsafe fn xml_output_buffer_create_filename_value(
    uri: *const c_char,
    encoder: Option<Rc<RefCell<XmlCharEncodingHandler>>>,
    compression: c_int,
) -> XmlOutputBufferPtr {
    if let Some(fvalue) = __xml_output_buffer_create_filename_value() {
        fvalue(uri, encoder, compression)
    } else {
        null_mut()
    }
}
#[deprecated]
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xml_output_buffer_create_filename_value(
    uri: *const c_char,
    encoder: Option<XmlCharEncodingHandler>,
    compression: c_int,
) -> XmlOutputBufferPtr {
    _XML_OUTPUT_BUFFER_CREATE_FILENAME_VALUE.unwrap()(uri, encoder, compression)
}

/**
 * xmlInitGlobalsInternal:
 *
 * Additional initialisation for multi-threading
 */
pub(crate) unsafe extern "C" fn xml_init_globals_internal() {
    xml_init_mutex(addr_of_mut!(XML_THR_DEF_MUTEX));
}

/**
 * xmlCleanupGlobalsInternal:
 *
 * Additional cleanup for multi-threading
 */
pub(crate) unsafe extern "C" fn xml_cleanup_globals_internal() {
    reset_last_error();

    xml_cleanup_mutex(addr_of_mut!(XML_THR_DEF_MUTEX));
    __xml_global_init_mutex_destroy();
}
