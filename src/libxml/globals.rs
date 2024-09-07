//! Provide methods and data structures for global variables.  
//! This module is based on `libxml/globals.h`, `globals.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    ffi::{c_char, c_int, c_void},
    mem::{size_of, zeroed},
    ptr::{addr_of_mut, null_mut},
    sync::atomic::{AtomicI32, AtomicPtr, Ordering},
};

use libc::{free, malloc, memset, realloc};

#[cfg(feature = "legacy")]
use crate::libxml::sax::{inithtmlDefaultSAXHandler, initxmlDefaultSAXHandler};
use crate::{
    libxml::{
        parser::{XmlSAXHandlerV1, XmlSaxlocator},
        xml_io::{XmlOutputBufferPtr, XmlParserInputBufferPtr, __xmlOutputBufferCreateFilename},
        xmlerror::{XmlError, XmlGenericErrorFunc, XmlStructuredErrorFunc},
        xmlmemory::{XmlFreeFunc, XmlMallocFunc, XmlReallocFunc, XmlStrdupFunc},
    },
    private::threads::{__xml_global_init_mutex_destroy, xml_cleanup_mutex, xml_init_mutex},
};

use super::{
    encoding::{XmlCharEncoding, XmlCharEncodingHandlerPtr},
    parser::xml_init_parser,
    sax2::{
        xmlSAX2AttributeDecl, xmlSAX2CDataBlock, xmlSAX2Characters, xmlSAX2Comment,
        xmlSAX2ElementDecl, xmlSAX2EndDocument, xmlSAX2EndElement, xmlSAX2EntityDecl,
        xmlSAX2ExternalSubset, xmlSAX2GetColumnNumber, xmlSAX2GetEntity, xmlSAX2GetLineNumber,
        xmlSAX2GetParameterEntity, xmlSAX2GetPublicId, xmlSAX2GetSystemId,
        xmlSAX2HasExternalSubset, xmlSAX2HasInternalSubset, xmlSAX2IgnorableWhitespace,
        xmlSAX2InternalSubset, xmlSAX2IsStandalone, xmlSAX2NotationDecl,
        xmlSAX2ProcessingInstruction, xmlSAX2Reference, xmlSAX2ResolveEntity,
        xmlSAX2SetDocumentLocator, xmlSAX2StartDocument, xmlSAX2StartElement,
        xmlSAX2UnparsedEntityDecl,
    },
    threads::{xmlGetGlobalState, xmlMutexLock, xmlMutexUnlock, XmlMutex},
    tree::{XmlBufferAllocationScheme, XmlNodePtr, BASE_BUFFER_SIZE, __XML_REGISTER_CALLBACKS},
    xml_io::__xmlParserInputBufferCreateFilename,
    xmlerror::{
        xmlGenericErrorDefaultFunc, xmlParserError, xmlParserWarning, xmlResetError, XmlErrorPtr,
    },
    xmlstring::{xml_char_strdup, xml_strdup, XmlChar},
    xmlversion::LIBXML_VERSION_STRING,
};

/**
* xmlInitGlobals:
*
* DEPRECATED: Alias for xmlInitParser.
*/
#[deprecated]
pub unsafe extern "C" fn xmlInitGlobals() {
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
pub unsafe extern "C" fn xmlCleanupGlobals() {}

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
    unsafe extern "C" fn(URI: *const c_char, enc: XmlCharEncoding) -> XmlParserInputBufferPtr;

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
pub type XmlOutputBufferCreateFilenameFunc = unsafe extern "C" fn(
    URI: *const c_char,
    encoder: XmlCharEncodingHandlerPtr,
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
pub unsafe extern "C" fn xmlParserInputBufferCreateFilenameDefault(
    func: Option<XmlParserInputBufferCreateFilenameFunc>,
) -> XmlParserInputBufferCreateFilenameFunc {
    let old: XmlParserInputBufferCreateFilenameFunc =
        _XML_PARSER_INPUT_BUFFER_CREATE_FILENAME_VALUE
            .unwrap_or(__xmlParserInputBufferCreateFilename);

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
pub unsafe extern "C" fn xmlOutputBufferCreateFilenameDefault(
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

pub(crate) type XmlGlobalStatePtr = *mut XmlGlobalState;
pub(crate) struct XmlGlobalState {
    pub(crate) xmlParserVersion: *const c_char,

    pub(crate) xmlDefaultSAXLocator: XmlSaxlocator,
    pub(crate) xmlDefaultSAXHandler: XmlSAXHandlerV1,
    pub(crate) docbDefaultSAXHandler: XmlSAXHandlerV1, /* unused */
    pub(crate) htmlDefaultSAXHandler: XmlSAXHandlerV1,

    pub(crate) xmlFree: Option<XmlFreeFunc>,
    pub(crate) xmlMalloc: Option<XmlMallocFunc>,
    pub(crate) xmlMemStrdup: Option<XmlStrdupFunc>,
    pub(crate) xmlRealloc: Option<XmlReallocFunc>,

    pub(crate) xmlGenericError: Option<XmlGenericErrorFunc>,
    pub(crate) xmlStructuredError: Option<XmlStructuredErrorFunc>,
    pub(crate) xmlGenericErrorContext: AtomicPtr<c_void>,

    pub(crate) oldXMLWDcompatibility: c_int,

    pub(crate) xmlBufferAllocScheme: XmlBufferAllocationScheme,
    pub(crate) xmlDefaultBufferSize: c_int,

    pub(crate) xmlSubstituteEntitiesDefaultValue: c_int,
    pub(crate) xmlDoValidityCheckingDefaultValue: c_int,
    pub(crate) xmlGetWarningsDefaultValue: c_int,
    pub(crate) xmlKeepBlanksDefaultValue: c_int,
    pub(crate) xmlLineNumbersDefaultValue: c_int,
    pub(crate) xmlLoadExtDtdDefaultValue: c_int,
    pub(crate) xmlParserDebugEntities: c_int,
    pub(crate) xmlPedanticParserDefaultValue: c_int,

    pub(crate) xmlSaveNoEmptyTags: c_int,
    pub(crate) xmlIndentTreeOutput: c_int,
    pub(crate) xmlTreeIndentString: *const c_char,

    pub(crate) xmlRegisterNodeDefaultValue: Option<XmlRegisterNodeFunc>,
    pub(crate) xmlDeregisterNodeDefaultValue: Option<XmlDeregisterNodeFunc>,

    pub(crate) xmlMallocAtomic: Option<XmlMallocFunc>,
    pub(crate) xmlLastError: XmlError,

    pub(crate) xmlParserInputBufferCreateFilenameValue:
        Option<XmlParserInputBufferCreateFilenameFunc>,
    pub(crate) xmlOutputBufferCreateFilenameValue: Option<XmlOutputBufferCreateFilenameFunc>,

    pub(crate) xmlStructuredErrorContext: AtomicPtr<c_void>,
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
pub(crate) static _XML_TREE_INDENT_STRING: AtomicPtr<c_char> = AtomicPtr::new(c"  ".as_ptr() as _);
static XML_TREE_INDENT_STRING_THR_DEF: AtomicPtr<c_char> = AtomicPtr::new(c"  ".as_ptr() as _);

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

/**
 * xmlGenericError:
 *
 * Global setting: function used for generic error callbacks
 */
pub(crate) static mut _XML_GENERIC_ERROR: Option<XmlGenericErrorFunc> =
    Some(xmlGenericErrorDefaultFunc);
static mut XML_GENERIC_ERROR_THR_DEF: Option<XmlGenericErrorFunc> =
    Some(xmlGenericErrorDefaultFunc);

/**
 * xmlStructuredError:
 *
 * Global setting: function used for structured error callbacks
 */
pub(crate) static mut _XML_STRUCTURED_ERROR: Option<XmlStructuredErrorFunc> = None;
static mut XML_STRUCTURED_ERROR_THR_DEF: Option<XmlStructuredErrorFunc> = None;

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
pub unsafe extern "C" fn xmlInitializeGlobalState(gs: XmlGlobalStatePtr) {
    // #ifdef DEBUG_GLOBALS
    //     fprintf(stderr, "Initializing globals at %p for thread %d\n",
    // 	    (void *) gs, xmlGetThreadId());
    // #endif

    xmlMutexLock(addr_of_mut!(XML_THR_DEF_MUTEX));

    #[cfg(all(feature = "html", feature = "legacy", feature = "sax1"))]
    {
        inithtmlDefaultSAXHandler(addr_of_mut!((*gs).htmlDefaultSAXHandler));
    }

    (*gs).oldXMLWDcompatibility = 0;
    (*gs).xmlBufferAllocScheme = XML_BUFFER_ALLOC_SCHEME_THR_DEF;
    (*gs).xmlDefaultBufferSize = XML_DEFAULT_BUFFER_SIZE_THR_DEF;
    #[cfg(all(feature = "sax1", feature = "legacy"))]
    {
        initxmlDefaultSAXHandler(addr_of_mut!((*gs).xmlDefaultSAXHandler), 1);
    }
    (*gs).xmlDefaultSAXLocator.get_public_id = xmlSAX2GetPublicId;
    (*gs).xmlDefaultSAXLocator.get_system_id = xmlSAX2GetSystemId;
    (*gs).xmlDefaultSAXLocator.get_line_number = xmlSAX2GetLineNumber;
    (*gs).xmlDefaultSAXLocator.get_column_number = xmlSAX2GetColumnNumber;
    (*gs).xmlDoValidityCheckingDefaultValue = XML_DO_VALIDITY_CHECKING_DEFAULT_VALUE_THR_DEF;
    // #if defined(DEBUG_MEMORY_LOCATION) | defined(DEBUG_MEMORY)
    //     {
    //         (*gs).xmlFree = (xmlFreeFunc) xmlMemFree;
    //         (*gs).xmlMalloc = (xmlMallocFunc) xmlMemMalloc;
    //         (*gs).xmlMallocAtomic = (xmlMallocFunc) xmlMemMalloc;
    //         (*gs).xmlRealloc = (xmlReallocFunc) xmlMemRealloc;
    //         (*gs).xmlMemStrdup = (xmlStrdupFunc) xmlMemoryStrdup;
    //     }
    // #else
    (*gs).xmlFree = Some(free as XmlFreeFunc);
    (*gs).xmlMalloc = Some(malloc as XmlMallocFunc);
    (*gs).xmlMallocAtomic = Some(malloc as XmlMallocFunc);
    (*gs).xmlRealloc = Some(realloc as XmlReallocFunc);
    (*gs).xmlMemStrdup = Some(xml_strdup as XmlStrdupFunc);
    // #endif
    (*gs).xmlGetWarningsDefaultValue = XML_GET_WARNINGS_DEFAULT_VALUE_THR_DEF;
    (*gs).xmlIndentTreeOutput = XML_INDENT_TREE_OUTPUT_THR_DEF;
    (*gs).xmlTreeIndentString = XML_TREE_INDENT_STRING_THR_DEF.load(Ordering::Relaxed) as _;
    (*gs).xmlKeepBlanksDefaultValue = XML_KEEP_BLANKS_DEFAULT_VALUE_THR_DEF;
    (*gs).xmlLineNumbersDefaultValue = XML_LINE_NUMBERS_DEFAULT_VALUE_THR_DEF;
    (*gs).xmlLoadExtDtdDefaultValue = XML_LOAD_EXT_DTD_DEFAULT_VALUE_THR_DEF;
    (*gs).xmlParserDebugEntities = XML_PARSER_DEBUG_ENTITIES_THR_DEF;
    (*gs).xmlParserVersion = LIBXML_VERSION_STRING.as_ptr();
    (*gs).xmlPedanticParserDefaultValue = XML_PEDANTIC_PARSER_DEFAULT_VALUE_THR_DEF;
    (*gs).xmlSaveNoEmptyTags = XML_SAVE_NO_EMPTY_TAGS_THR_DEF;
    (*gs).xmlSubstituteEntitiesDefaultValue = XML_SUBSTITUTE_ENTITIES_DEFAULT_VALUE_THR_DEF;

    (*gs).xmlGenericError = XML_GENERIC_ERROR_THR_DEF;
    (*gs).xmlStructuredError = XML_STRUCTURED_ERROR_THR_DEF;
    (*gs).xmlGenericErrorContext.store(
        XML_GENERIC_ERROR_CONTEXT_THR_DEF.load(Ordering::Relaxed),
        Ordering::Relaxed,
    );
    (*gs).xmlStructuredErrorContext.store(
        XML_STRUCTURED_ERROR_CONTEXT_THR_DEF.load(Ordering::Relaxed),
        Ordering::Relaxed,
    );
    (*gs).xmlRegisterNodeDefaultValue = XML_REGISTER_NODE_DEFAULT_VALUE_THR_DEF;
    (*gs).xmlDeregisterNodeDefaultValue = XML_DEREGISTER_NODE_DEFAULT_VALUE_THR_DEF;

    (*gs).xmlParserInputBufferCreateFilenameValue =
        XML_PARSER_INPUT_BUFFER_CREATE_FILENAME_VALUE_THR_DEF;
    (*gs).xmlOutputBufferCreateFilenameValue = XML_OUTPUT_BUFFER_CREATE_FILENAME_VALUE_THR_DEF;
    memset(
        addr_of_mut!((*gs).xmlLastError) as _,
        0,
        size_of::<XmlError>(),
    );

    xmlMutexUnlock(addr_of_mut!(XML_THR_DEF_MUTEX));
}

pub unsafe extern "C" fn xmlThrDefSetGenericErrorFunc(
    ctx: *mut c_void,
    handler: Option<XmlGenericErrorFunc>,
) {
    xmlMutexLock(addr_of_mut!(XML_THR_DEF_MUTEX));
    XML_GENERIC_ERROR_CONTEXT_THR_DEF.store(ctx, Ordering::Relaxed);
    if let Some(handler) = handler {
        XML_GENERIC_ERROR_THR_DEF = Some(handler);
    } else {
        XML_GENERIC_ERROR_THR_DEF = Some(xmlGenericErrorDefaultFunc);
    }
    xmlMutexUnlock(addr_of_mut!(XML_THR_DEF_MUTEX));
}

pub unsafe extern "C" fn xmlThrDefSetStructuredErrorFunc(
    ctx: *mut c_void,
    handler: XmlStructuredErrorFunc,
) {
    xmlMutexLock(addr_of_mut!(XML_THR_DEF_MUTEX));
    XML_STRUCTURED_ERROR_CONTEXT_THR_DEF.store(ctx, Ordering::Relaxed);
    XML_STRUCTURED_ERROR_THR_DEF = Some(handler);
    xmlMutexUnlock(addr_of_mut!(XML_THR_DEF_MUTEX));
}

/**
 * xmlRegisterNodeDefault:
 * @func: function pointer to the new RegisterNodeFunc
 *
 * Registers a callback for node creation
 *
 * Returns the old value of the registration function
 */
pub unsafe extern "C" fn xmlRegisterNodeDefault(
    func: Option<XmlRegisterNodeFunc>,
) -> Option<XmlRegisterNodeFunc> {
    let old = _XML_REGISTER_NODE_DEFAULT_VALUE;

    __XML_REGISTER_CALLBACKS.store(1, Ordering::Relaxed);
    _XML_REGISTER_NODE_DEFAULT_VALUE = func;
    old
}

pub unsafe extern "C" fn xmlThrDefRegisterNodeDefault(
    func: XmlRegisterNodeFunc,
) -> Option<XmlRegisterNodeFunc> {
    xmlMutexLock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let old = XML_REGISTER_NODE_DEFAULT_VALUE_THR_DEF;

    __XML_REGISTER_CALLBACKS.store(1, Ordering::Relaxed);
    XML_REGISTER_NODE_DEFAULT_VALUE_THR_DEF = Some(func);
    xmlMutexUnlock(addr_of_mut!(XML_THR_DEF_MUTEX));

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
pub unsafe extern "C" fn xmlDeregisterNodeDefault(
    func: XmlDeregisterNodeFunc,
) -> Option<XmlDeregisterNodeFunc> {
    let old = _XML_DEREGISTER_NODE_DEFAULT_VALUE;

    __XML_REGISTER_CALLBACKS.store(1, Ordering::Relaxed);
    _XML_DEREGISTER_NODE_DEFAULT_VALUE = Some(func);
    old
}

pub unsafe extern "C" fn xmlThrDefDeregisterNodeDefault(
    func: XmlDeregisterNodeFunc,
) -> Option<XmlDeregisterNodeFunc> {
    xmlMutexLock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let old = XML_DEREGISTER_NODE_DEFAULT_VALUE_THR_DEF;

    __XML_REGISTER_CALLBACKS.store(1, Ordering::Relaxed);
    XML_DEREGISTER_NODE_DEFAULT_VALUE_THR_DEF = Some(func);
    xmlMutexUnlock(addr_of_mut!(XML_THR_DEF_MUTEX));

    old
}

pub unsafe extern "C" fn xmlThrDefOutputBufferCreateFilenameDefault(
    func: XmlOutputBufferCreateFilenameFunc,
) -> Option<XmlOutputBufferCreateFilenameFunc> {
    xmlMutexLock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let mut old = XML_OUTPUT_BUFFER_CREATE_FILENAME_VALUE_THR_DEF;
    #[cfg(feature = "output")]
    {
        if old.is_none() {
            old = Some(__xmlOutputBufferCreateFilename);
        }
    }
    XML_OUTPUT_BUFFER_CREATE_FILENAME_VALUE_THR_DEF = Some(func);
    xmlMutexUnlock(addr_of_mut!(XML_THR_DEF_MUTEX));

    old
}

pub unsafe extern "C" fn xmlThrDefParserInputBufferCreateFilenameDefault(
    func: Option<XmlParserInputBufferCreateFilenameFunc>,
) -> XmlParserInputBufferCreateFilenameFunc {
    xmlMutexLock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let old = XML_PARSER_INPUT_BUFFER_CREATE_FILENAME_VALUE_THR_DEF
        .unwrap_or(__xmlParserInputBufferCreateFilename);

    XML_PARSER_INPUT_BUFFER_CREATE_FILENAME_VALUE_THR_DEF = func;
    xmlMutexUnlock(addr_of_mut!(XML_THR_DEF_MUTEX));

    old
}

/*
 * Helpful Macro
 */
#[macro_export]
#[cfg(feature = "thread")]
macro_rules! IS_MAIN_THREAD {
    () => {
        $crate::libxml::threads::xmlIsMainThread()
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
        (*xmlGetGlobalState()).xmlMalloc = malloc;
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
        (*xmlGetGlobalState()).xmlRealloc = realloc;
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
        (*xmlGetGlobalState()).xmlFree = free;
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
        (*xmlGetGlobalState()).xmlMemStrdup = mem_strdup;
    }
}

#[cfg(feature = "thread_alloc")]
mod __globals_internal_for_thread_alloc {
    use super::*;
    use crate::libxml::threads::xmlGetGlobalState;

    #[cfg(feature = "thread")]
    pub unsafe extern "C" fn __xml_malloc() -> Option<XmlMallocFunc> {
        if IS_MAIN_THREAD!() != 0 {
            _XML_MALLOC
        } else {
            (*xmlGetGlobalState()).xmlMalloc
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
            (*xmlGetGlobalState()).xmlMallocAtomic.unwrap()
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
            (*xmlGetGlobalState()).xmlRealloc
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
            (*xmlGetGlobalState()).xmlFree
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
            (*xmlGetGlobalState()).xmlMemStrdup
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
    internal_subset: Some(xmlSAX2InternalSubset),
    is_standalone: None,
    has_internal_subset: None,
    has_external_subset: None,
    resolve_entity: None,
    get_entity: Some(xmlSAX2GetEntity),
    entity_decl: None,
    notation_decl: None,
    attribute_decl: None,
    element_decl: None,
    unparsed_entity_decl: None,
    set_document_locator: Some(xmlSAX2SetDocumentLocator),
    start_document: Some(xmlSAX2StartDocument),
    end_document: Some(xmlSAX2EndDocument),
    start_element: Some(xmlSAX2StartElement),
    end_element: Some(xmlSAX2EndElement),
    reference: None,
    characters: Some(xmlSAX2Characters),
    ignorable_whitespace: Some(xmlSAX2IgnorableWhitespace),
    processing_instruction: Some(xmlSAX2ProcessingInstruction),
    comment: Some(xmlSAX2Comment),
    warning: Some(xmlParserWarning),
    error: Some(xmlParserError),
    fatal_error: Some(xmlParserError),
    get_parameter_entity: None,
    cdata_block: Some(xmlSAX2CDataBlock),
    external_subset: None,
    initialized: 1,
};

#[cfg(feature = "html")]
mod __globals_internal_for_html {
    use super::*;

    #[deprecated]
    pub unsafe extern "C" fn __htmlDefaultSAXHandler() -> *mut XmlSAXHandlerV1 {
        if IS_MAIN_THREAD!() != 0 {
            addr_of_mut!(_HTML_DEFAULT_SAXHANDLER)
        } else {
            addr_of_mut!((*xmlGetGlobalState()).htmlDefaultSAXHandler)
        }
    }
    #[cfg(feature = "thread")]
    pub unsafe extern "C" fn htmlDefaultSAXHandler() -> *mut XmlSAXHandlerV1 {
        __htmlDefaultSAXHandler()
    }
    #[deprecated]
    #[cfg(not(feature = "thread"))]
    pub unsafe extern "C" fn htmlDefaultSAXHandler() -> *mut XmlSAXHandlerV1 {
        addr_of_mut!(_HTML_DEFAULT_SAXHANDLER)
    }
}
#[cfg(feature = "html")]
pub use __globals_internal_for_html::*;

pub unsafe extern "C" fn __xmlLastError() -> *mut XmlError {
    if IS_MAIN_THREAD!() != 0 {
        addr_of_mut!(_XML_LAST_ERROR)
    } else {
        addr_of_mut!((*xmlGetGlobalState()).xmlLastError)
    }
}

static mut _XML_LAST_ERROR: XmlError = unsafe { zeroed() };

#[cfg(feature = "thread")]
pub unsafe extern "C" fn xmlLastError() -> *mut XmlError {
    __xmlLastError()
}
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xmlLastError() -> *mut XmlError {
    addr_of_mut!(_XML_LAST_ERROR)
}

/*
 * Everything starting from the line below is
 * Automatically generated by build_glob.py.
 * Do not modify the previous line.
 */

#[deprecated]
pub unsafe extern "C" fn __oldXMLWDcompatibility() -> *mut c_int {
    if IS_MAIN_THREAD!() != 0 {
        addr_of_mut!(_OLD_XMLWDCOMPATIBILITY)
    } else {
        addr_of_mut!((*xmlGetGlobalState()).oldXMLWDcompatibility)
    }
}

/**
 * oldXMLWDcompatibility:
 *
 * Global setting, DEPRECATED.
 */
static mut _OLD_XMLWDCOMPATIBILITY: c_int = 0; /* DEPRECATED */
#[cfg(feature = "thread")]
pub unsafe extern "C" fn oldXMLWDcompatibility() -> *mut c_int {
    __oldXMLWDcompatibility()
}
#[deprecated]
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn oldXMLWDcompatibility() -> *mut c_int {
    addr_of_mut!(_OLD_XMLWDCOMPATIBILITY)
}

#[deprecated]
pub unsafe extern "C" fn __xmlBufferAllocScheme() -> *mut XmlBufferAllocationScheme {
    if IS_MAIN_THREAD!() != 0 {
        addr_of_mut!(_XML_BUFFER_ALLOC_SCHEME)
    } else {
        addr_of_mut!((*xmlGetGlobalState()).xmlBufferAllocScheme)
    }
}
#[cfg(feature = "thread")]
pub unsafe extern "C" fn xmlBufferAllocScheme() -> *mut XmlBufferAllocationScheme {
    __xmlBufferAllocScheme()
}
#[deprecated]
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xmlBufferAllocScheme() -> *mut XmlBufferAllocationScheme {
    addr_of_mut!(_XML_BUFFER_ALLOC_SCHEME)
}
#[deprecated]
pub unsafe extern "C" fn xmlThrDefBufferAllocScheme(
    v: XmlBufferAllocationScheme,
) -> XmlBufferAllocationScheme {
    xmlMutexLock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let ret: XmlBufferAllocationScheme = XML_BUFFER_ALLOC_SCHEME_THR_DEF;
    XML_BUFFER_ALLOC_SCHEME_THR_DEF = v;
    xmlMutexUnlock(addr_of_mut!(XML_THR_DEF_MUTEX));
    ret
}

#[deprecated]
pub unsafe extern "C" fn __xmlDefaultBufferSize() -> *mut c_int {
    if IS_MAIN_THREAD!() != 0 {
        addr_of_mut!(_XML_DEFAULT_BUFFER_SIZE)
    } else {
        addr_of_mut!((*xmlGetGlobalState()).xmlDefaultBufferSize)
    }
}

#[cfg(feature = "thread")]
pub unsafe extern "C" fn xmlDefaultBufferSize() -> *mut c_int {
    __xmlDefaultBufferSize()
}
#[deprecated]
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xmlDefaultBufferSize() -> *mut c_int {
    addr_of_mut!(_XML_DEFAULT_BUFFER_SIZE)
}

#[deprecated]
pub unsafe extern "C" fn xmlThrDefDefaultBufferSize(v: c_int) -> c_int {
    xmlMutexLock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let ret: c_int = XML_DEFAULT_BUFFER_SIZE_THR_DEF;
    XML_DEFAULT_BUFFER_SIZE_THR_DEF = v;
    xmlMutexUnlock(addr_of_mut!(XML_THR_DEF_MUTEX));
    ret
}

#[deprecated]
pub unsafe extern "C" fn __xmlDefaultSAXHandler() -> *mut XmlSAXHandlerV1 {
    if IS_MAIN_THREAD!() != 0 {
        addr_of_mut!(_XML_DEFAULT_SAXHANDLER)
    } else {
        addr_of_mut!((*xmlGetGlobalState()).xmlDefaultSAXHandler)
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
    internal_subset: Some(xmlSAX2InternalSubset),
    is_standalone: Some(xmlSAX2IsStandalone),
    has_internal_subset: Some(xmlSAX2HasInternalSubset),
    has_external_subset: Some(xmlSAX2HasExternalSubset),
    resolve_entity: Some(xmlSAX2ResolveEntity),
    get_entity: Some(xmlSAX2GetEntity),
    entity_decl: Some(xmlSAX2EntityDecl),
    notation_decl: Some(xmlSAX2NotationDecl),
    attribute_decl: Some(xmlSAX2AttributeDecl),
    element_decl: Some(xmlSAX2ElementDecl),
    unparsed_entity_decl: Some(xmlSAX2UnparsedEntityDecl),
    set_document_locator: Some(xmlSAX2SetDocumentLocator),
    start_document: Some(xmlSAX2StartDocument),
    end_document: Some(xmlSAX2EndDocument),
    start_element: Some(xmlSAX2StartElement),
    end_element: Some(xmlSAX2EndElement),
    reference: Some(xmlSAX2Reference),
    characters: Some(xmlSAX2Characters),
    ignorable_whitespace: Some(xmlSAX2Characters),
    processing_instruction: Some(xmlSAX2ProcessingInstruction),
    comment: Some(xmlSAX2Comment),
    warning: Some(xmlParserWarning),
    error: Some(xmlParserError),
    fatal_error: Some(xmlParserError),
    get_parameter_entity: Some(xmlSAX2GetParameterEntity),
    cdata_block: Some(xmlSAX2CDataBlock),
    external_subset: Some(xmlSAX2ExternalSubset),
    initialized: 1,
};

#[cfg(feature = "thread")]
pub unsafe extern "C" fn xmlDefaultSAXHandler() -> *mut XmlSAXHandlerV1 {
    __xmlDefaultSAXHandler()
}
#[deprecated]
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xmlDefaultSAXHandler() -> *mut XmlSAXHandlerV1 {
    addr_of_mut!(_XML_DEFAULT_SAXHANDLER)
}

#[deprecated]
pub unsafe extern "C" fn __xmlDefaultSAXLocator() -> *mut XmlSaxlocator {
    if IS_MAIN_THREAD!() != 0 {
        addr_of_mut!(_XML_DEFAULT_SAXLOCATOR)
    } else {
        addr_of_mut!((*xmlGetGlobalState()).xmlDefaultSAXLocator)
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
    get_public_id: xmlSAX2GetPublicId,
    get_system_id: xmlSAX2GetSystemId,
    get_line_number: xmlSAX2GetLineNumber,
    get_column_number: xmlSAX2GetColumnNumber,
};

#[cfg(feature = "thread")]
pub unsafe extern "C" fn xmlDefaultSAXLocator() -> *mut XmlSaxlocator {
    __xmlDefaultSAXLocator()
}
#[deprecated]
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xmlDefaultSAXLocator() -> *mut XmlSaxlocator {
    addr_of_mut!(_XML_DEFAULT_SAXLOCATOR)
}

pub unsafe extern "C" fn __xmlDoValidityCheckingDefaultValue() -> *mut c_int {
    if IS_MAIN_THREAD!() != 0 {
        _XML_DO_VALIDITY_CHECKING_DEFAULT_VALUE.as_ptr()
    } else {
        addr_of_mut!((*xmlGetGlobalState()).xmlDoValidityCheckingDefaultValue)
    }
}
#[cfg(feature = "thread")]
pub unsafe extern "C" fn xmlDoValidityCheckingDefaultValue() -> *mut c_int {
    __xmlDoValidityCheckingDefaultValue()
}
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xmlDoValidityCheckingDefaultValue() -> *mut c_int {
    _XML_DO_VALIDITY_CHECKING_DEFAULT_VALUE.as_ptr()
}

pub unsafe extern "C" fn xmlThrDefDoValidityCheckingDefaultValue(v: c_int) -> c_int {
    xmlMutexLock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let ret: c_int = XML_DO_VALIDITY_CHECKING_DEFAULT_VALUE_THR_DEF;
    XML_DO_VALIDITY_CHECKING_DEFAULT_VALUE_THR_DEF = v;
    xmlMutexUnlock(addr_of_mut!(XML_THR_DEF_MUTEX));
    ret
}

pub unsafe extern "C" fn __xmlGenericError() -> XmlGenericErrorFunc {
    if IS_MAIN_THREAD!() != 0 {
        _XML_GENERIC_ERROR.unwrap()
    } else {
        (*xmlGetGlobalState()).xmlGenericError.unwrap()
    }
}

#[cfg(feature = "thread")]
pub unsafe extern "C" fn xmlGenericError(ctx: *mut c_void, msg: *const c_char) {
    __xmlGenericError()(ctx, msg);
}
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xmlGenericError(ctx: *mut c_void, msg: *const c_char) {
    _XML_GENERIC_ERROR.unwrap()(ctx, msg)
}

pub unsafe extern "C" fn __xmlStructuredError() -> Option<XmlStructuredErrorFunc> {
    if IS_MAIN_THREAD!() != 0 {
        _XML_STRUCTURED_ERROR
    } else {
        (*xmlGetGlobalState()).xmlStructuredError
    }
}

#[cfg(feature = "thread")]
pub unsafe extern "C" fn xmlStructuredError(user_data: *mut c_void, error: XmlErrorPtr) {
    if let Some(serror) = __xmlStructuredError() {
        serror(user_data, error)
    }
}
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xmlStructuredError(user_data: *mut c_void, error: XmlErrorPtr) {
    _XML_STRUCTURED_ERROR.unwrap(user_data, error)
}

pub unsafe extern "C" fn __xmlGenericErrorContext() -> *mut *mut c_void {
    if IS_MAIN_THREAD!() != 0 {
        _XML_GENERIC_ERROR_CONTEXT.as_ptr()
    } else {
        (*xmlGetGlobalState()).xmlGenericErrorContext.as_ptr()
    }
}

#[cfg(feature = "thread")]
pub unsafe extern "C" fn xmlGenericErrorContext() -> *mut c_void {
    *__xmlGenericErrorContext()
}
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xmlGenericErrorContext() -> *mut *mut c_void {
    *_XML_GENERIC_ERROR_CONTEXT.as_ptr()
}

pub unsafe extern "C" fn __xmlStructuredErrorContext() -> *mut *mut c_void {
    if IS_MAIN_THREAD!() != 0 {
        _XML_STRUCTURED_ERROR_CONTEXT.as_ptr()
    } else {
        (*xmlGetGlobalState()).xmlStructuredErrorContext.as_ptr()
    }
}

#[cfg(feature = "thread")]
pub unsafe extern "C" fn xmlStructuredErrorContext() -> *mut *mut c_void {
    __xmlStructuredErrorContext()
}
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xmlStructuredErrorContext() -> *mut *mut c_void {
    _XML_STRUCTURED_ERROR_CONTEXT.as_ptr()
}

pub unsafe extern "C" fn __xmlGetWarningsDefaultValue() -> *mut c_int {
    if IS_MAIN_THREAD!() != 0 {
        _XML_GET_WARNINGS_DEFAULT_VALUE.as_ptr()
    } else {
        addr_of_mut!((*xmlGetGlobalState()).xmlGetWarningsDefaultValue)
    }
}
#[cfg(feature = "thread")]
pub unsafe extern "C" fn xmlGetWarningsDefaultValue() -> *mut c_int {
    __xmlGetWarningsDefaultValue()
}
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xmlGetWarningsDefaultValue() -> *mut c_int {
    _XML_GET_WARNINGS_DEFAULT_VALUE.as_ptr()
}

pub unsafe extern "C" fn xmlThrDefGetWarningsDefaultValue(v: c_int) -> c_int {
    xmlMutexLock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let ret: c_int = XML_GET_WARNINGS_DEFAULT_VALUE_THR_DEF;
    XML_GET_WARNINGS_DEFAULT_VALUE_THR_DEF = v;
    xmlMutexUnlock(addr_of_mut!(XML_THR_DEF_MUTEX));
    ret
}

pub unsafe extern "C" fn __xmlIndentTreeOutput() -> *mut c_int {
    if IS_MAIN_THREAD!() != 0 {
        _XML_INDENT_TREE_OUTPUT.as_ptr()
    } else {
        addr_of_mut!((*xmlGetGlobalState()).xmlIndentTreeOutput)
    }
}
#[cfg(feature = "thread")]
pub unsafe extern "C" fn xmlIndentTreeOutput() -> *mut c_int {
    __xmlIndentTreeOutput()
}
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xmlIndentTreeOutput() -> *mut c_int {
    _XML_INDENT_TREE_OUTPUT.as_ptr()
}

pub unsafe extern "C" fn xmlThrDefIndentTreeOutput(v: c_int) -> c_int {
    xmlMutexLock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let ret: c_int = XML_INDENT_TREE_OUTPUT_THR_DEF;
    XML_INDENT_TREE_OUTPUT_THR_DEF = v;
    xmlMutexUnlock(addr_of_mut!(XML_THR_DEF_MUTEX));
    ret
}

pub unsafe extern "C" fn __xmlTreeIndentString() -> *mut *const c_char {
    if IS_MAIN_THREAD!() != 0 {
        _XML_TREE_INDENT_STRING.as_ptr() as _
    } else {
        addr_of_mut!((*xmlGetGlobalState()).xmlTreeIndentString)
    }
}
#[cfg(feature = "thread")]
pub unsafe extern "C" fn xmlTreeIndentString() -> *mut *const c_char {
    __xmlTreeIndentString()
}
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xmlTreeIndentString() -> *mut *const c_char {
    _XML_TREE_INDENT_STRING.as_ptr() as _
}

pub unsafe extern "C" fn xmlThrDefTreeIndentString(v: *const c_char) -> *const c_char {
    xmlMutexLock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let ret: *const c_char = XML_TREE_INDENT_STRING_THR_DEF.load(Ordering::Relaxed);
    XML_TREE_INDENT_STRING_THR_DEF.store(v as _, Ordering::Relaxed);
    xmlMutexUnlock(addr_of_mut!(XML_THR_DEF_MUTEX));
    ret
}

pub unsafe extern "C" fn __xmlKeepBlanksDefaultValue() -> *mut c_int {
    if IS_MAIN_THREAD!() != 0 {
        _XML_KEEP_BLANKS_DEFAULT_VALUE.as_ptr()
    } else {
        addr_of_mut!((*xmlGetGlobalState()).xmlKeepBlanksDefaultValue)
    }
}
#[cfg(feature = "thread")]
pub unsafe extern "C" fn xmlKeepBlanksDefaultValue() -> *mut c_int {
    __xmlKeepBlanksDefaultValue()
}
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xmlKeepBlanksDefaultValue() -> *mut c_int {
    _XML_KEEP_BLANKS_DEFAULT_VALUE.as_ptr()
}

pub unsafe extern "C" fn xmlThrDefKeepBlanksDefaultValue(v: c_int) -> c_int {
    xmlMutexLock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let ret: c_int = XML_KEEP_BLANKS_DEFAULT_VALUE_THR_DEF;
    XML_KEEP_BLANKS_DEFAULT_VALUE_THR_DEF = v;
    xmlMutexUnlock(addr_of_mut!(XML_THR_DEF_MUTEX));
    ret
}

#[deprecated]
pub unsafe extern "C" fn __xmlLineNumbersDefaultValue() -> *mut c_int {
    if IS_MAIN_THREAD!() != 0 {
        _XML_LINE_NUMBERS_DEFAULT_VALUE.as_ptr()
    } else {
        addr_of_mut!((*xmlGetGlobalState()).xmlLineNumbersDefaultValue)
    }
}
#[cfg(feature = "thread")]
pub unsafe extern "C" fn xmlLineNumbersDefaultValue() -> *mut c_int {
    __xmlLineNumbersDefaultValue()
}
#[deprecated]
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xmlLineNumbersDefaultValue() -> *mut c_int {
    _XML_LINE_NUMBERS_DEFAULT_VALUE.as_ptr()
}

#[deprecated]
pub unsafe extern "C" fn xmlThrDefLineNumbersDefaultValue(v: c_int) -> c_int {
    xmlMutexLock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let ret: c_int = XML_LINE_NUMBERS_DEFAULT_VALUE_THR_DEF;
    XML_LINE_NUMBERS_DEFAULT_VALUE_THR_DEF = v;
    xmlMutexUnlock(addr_of_mut!(XML_THR_DEF_MUTEX));
    ret
}

pub unsafe extern "C" fn __xmlLoadExtDtdDefaultValue() -> *mut c_int {
    if IS_MAIN_THREAD!() != 0 {
        _XML_LOAD_EXT_DTD_DEFAULT_VALUE.as_ptr()
    } else {
        addr_of_mut!((*xmlGetGlobalState()).xmlLoadExtDtdDefaultValue)
    }
}
#[cfg(feature = "thread")]
pub unsafe extern "C" fn xmlLoadExtDtdDefaultValue() -> *mut c_int {
    __xmlLoadExtDtdDefaultValue()
}
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xmlLoadExtDtdDefaultValue() -> *mut c_int {
    _XML_LOAD_EXT_DTD_DEFAULT_VALUE.as_ptr()
}

pub unsafe extern "C" fn xmlThrDefLoadExtDtdDefaultValue(v: c_int) -> c_int {
    xmlMutexLock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let ret: c_int = XML_LOAD_EXT_DTD_DEFAULT_VALUE_THR_DEF;
    XML_LOAD_EXT_DTD_DEFAULT_VALUE_THR_DEF = v;
    xmlMutexUnlock(addr_of_mut!(XML_THR_DEF_MUTEX));
    ret
}

pub unsafe extern "C" fn __xmlParserDebugEntities() -> *mut c_int {
    if IS_MAIN_THREAD!() != 0 {
        _XML_PARSER_DEBUG_ENTITIES.as_ptr()
    } else {
        addr_of_mut!((*xmlGetGlobalState()).xmlParserDebugEntities)
    }
}
#[cfg(feature = "thread")]
pub unsafe extern "C" fn xmlParserDebugEntities() -> *mut c_int {
    __xmlParserDebugEntities()
}
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xmlParserDebugEntities() -> *mut c_int {
    _XML_PARSER_DEBUG_ENTITIES.as_ptr()
}

pub unsafe extern "C" fn xmlThrDefParserDebugEntities(v: c_int) -> c_int {
    xmlMutexLock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let ret: c_int = XML_PARSER_DEBUG_ENTITIES_THR_DEF;
    XML_PARSER_DEBUG_ENTITIES_THR_DEF = v;
    xmlMutexUnlock(addr_of_mut!(XML_THR_DEF_MUTEX));
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
pub unsafe extern "C" fn __xmlPedanticParserDefaultValue() -> *mut c_int {
    if IS_MAIN_THREAD!() != 0 {
        _XML_PEDANTIC_PARSER_DEFAULT_VALUE.as_ptr()
    } else {
        addr_of_mut!((*xmlGetGlobalState()).xmlPedanticParserDefaultValue)
    }
}
#[cfg(feature = "thread")]
pub unsafe extern "C" fn xmlPedanticParserDefaultValue() -> *mut c_int {
    __xmlPedanticParserDefaultValue()
}
#[deprecated]
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xmlPedanticParserDefaultValue() -> *mut c_int {
    _XML_PEDANTIC_PARSER_DEFAULT_VALUE.as_ptr()
}

#[deprecated]
pub unsafe extern "C" fn xmlThrDefPedanticParserDefaultValue(v: c_int) -> c_int {
    xmlMutexLock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let ret: c_int = XML_PEDANTIC_PARSER_DEFAULT_VALUE_THR_DEF;
    XML_PEDANTIC_PARSER_DEFAULT_VALUE_THR_DEF = v;
    xmlMutexUnlock(addr_of_mut!(XML_THR_DEF_MUTEX));
    ret
}

pub unsafe extern "C" fn __xmlSaveNoEmptyTags() -> *mut c_int {
    if IS_MAIN_THREAD!() != 0 {
        _XML_SAVE_NO_EMPTY_TAGS.as_ptr()
    } else {
        addr_of_mut!((*xmlGetGlobalState()).xmlSaveNoEmptyTags)
    }
}
#[cfg(feature = "thread")]
pub unsafe extern "C" fn xmlSaveNoEmptyTags() -> *mut c_int {
    __xmlSaveNoEmptyTags()
}
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xmlSaveNoEmptyTags() -> *mut c_int {
    _XML_SAVE_NO_EMPTY_TAGS.as_ptr()
}

pub unsafe extern "C" fn xmlThrDefSaveNoEmptyTags(v: c_int) -> c_int {
    xmlMutexLock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let ret: c_int = XML_SAVE_NO_EMPTY_TAGS_THR_DEF;
    XML_SAVE_NO_EMPTY_TAGS_THR_DEF = v;
    xmlMutexUnlock(addr_of_mut!(XML_THR_DEF_MUTEX));
    ret
}

pub unsafe extern "C" fn __xmlSubstituteEntitiesDefaultValue() -> *mut c_int {
    if IS_MAIN_THREAD!() != 0 {
        _XML_SUBSTITUTE_ENTITIES_DEFAULT_VALUE.as_ptr()
    } else {
        addr_of_mut!((*xmlGetGlobalState()).xmlSubstituteEntitiesDefaultValue)
    }
}

#[cfg(feature = "thread")]
pub unsafe extern "C" fn xmlSubstituteEntitiesDefaultValue() -> *mut c_int {
    __xmlSubstituteEntitiesDefaultValue()
}
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xmlSubstituteEntitiesDefaultValue() -> *mut c_int {
    _XML_SUBSTITUTE_ENTITIES_DEFAULT_VALUE.as_ptr()
}

pub unsafe extern "C" fn xmlThrDefSubstituteEntitiesDefaultValue(v: c_int) -> c_int {
    xmlMutexLock(addr_of_mut!(XML_THR_DEF_MUTEX));
    let ret: c_int = XML_SUBSTITUTE_ENTITIES_DEFAULT_VALUE_THR_DEF;
    XML_SUBSTITUTE_ENTITIES_DEFAULT_VALUE_THR_DEF = v;
    xmlMutexUnlock(addr_of_mut!(XML_THR_DEF_MUTEX));
    ret
}

#[deprecated]
pub unsafe extern "C" fn __xmlRegisterNodeDefaultValue() -> XmlRegisterNodeFunc {
    if IS_MAIN_THREAD!() != 0 {
        _XML_REGISTER_NODE_DEFAULT_VALUE.unwrap()
    } else {
        (*xmlGetGlobalState()).xmlRegisterNodeDefaultValue.unwrap()
    }
}

#[cfg(feature = "thread")]
pub unsafe extern "C" fn xmlRegisterNodeDefaultValue(node: XmlNodePtr) {
    __xmlRegisterNodeDefaultValue()(node)
}
#[deprecated]
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xmlRegisterNodeDefaultValue(node: XmlNodePtr) {
    _XML_REGISTER_NODE_DEFAULT_VALUE.unwrap()(node)
}

#[deprecated]
pub unsafe extern "C" fn __xmlDeregisterNodeDefaultValue() -> XmlDeregisterNodeFunc {
    if IS_MAIN_THREAD!() != 0 {
        _XML_DEREGISTER_NODE_DEFAULT_VALUE.unwrap()
    } else {
        (*xmlGetGlobalState())
            .xmlDeregisterNodeDefaultValue
            .unwrap()
    }
}

#[cfg(feature = "thread")]
pub unsafe extern "C" fn xmlDeregisterNodeDefaultValue(node: XmlNodePtr) {
    __xmlDeregisterNodeDefaultValue()(node)
}
#[deprecated]
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xmlDeregisterNodeDefaultValue(node: XmlNodePtr) {
    _XML_DEREGISTER_NODE_DEFAULT_VALUE.unwrap()(node)
}

pub(crate) unsafe extern "C" fn __xmlParserInputBufferCreateFilenameValue(
) -> Option<XmlParserInputBufferCreateFilenameFunc> {
    if IS_MAIN_THREAD!() != 0 {
        _XML_PARSER_INPUT_BUFFER_CREATE_FILENAME_VALUE
    } else {
        (*xmlGetGlobalState()).xmlParserInputBufferCreateFilenameValue
    }
}

#[cfg(feature = "thread")]
pub unsafe extern "C" fn xmlParserInputBufferCreateFilenameValue(
    uri: *const c_char,
    enc: XmlCharEncoding,
) -> XmlParserInputBufferPtr {
    if let Some(f) = __xmlParserInputBufferCreateFilenameValue() {
        f(uri, enc)
    } else {
        null_mut()
    }
}
#[deprecated]
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xmlParserInputBufferCreateFilenameValue(
    uri: *const c_char,
    enc: XmlCharEncoding,
) -> XmlParserInputBufferPtr {
    _XML_PARSER_INPUT_BUFFER_CREATE_FILENAME_VALUE.unwrap()(uri, enc)
}

#[deprecated]
pub unsafe extern "C" fn __xmlOutputBufferCreateFilenameValue(
) -> Option<XmlOutputBufferCreateFilenameFunc> {
    if IS_MAIN_THREAD!() != 0 {
        _XML_OUTPUT_BUFFER_CREATE_FILENAME_VALUE
    } else {
        (*xmlGetGlobalState()).xmlOutputBufferCreateFilenameValue
    }
}

#[cfg(feature = "thread")]
pub unsafe extern "C" fn xmlOutputBufferCreateFilenameValue(
    uri: *const c_char,
    encoder: XmlCharEncodingHandlerPtr,
    compression: c_int,
) -> XmlOutputBufferPtr {
    if let Some(fvalue) = __xmlOutputBufferCreateFilenameValue() {
        fvalue(uri, encoder, compression)
    } else {
        null_mut()
    }
}
#[deprecated]
#[cfg(not(feature = "thread"))]
pub unsafe extern "C" fn xmlOutputBufferCreateFilenameValue(
    uri: *const c_char,
    encoder: XmlCharEncodingHandlerPtr,
    compression: c_int,
) -> XmlOutputBufferPtr {
    _XML_OUTPUT_BUFFER_CREATE_FILENAME_VALUE.unwrap()(uri, encoder, compression)
}

/**
 * xmlInitGlobalsInternal:
 *
 * Additional initialisation for multi-threading
 */
pub(crate) unsafe extern "C" fn xmlInitGlobalsInternal() {
    xml_init_mutex(addr_of_mut!(XML_THR_DEF_MUTEX));
}

/**
 * xmlCleanupGlobalsInternal:
 *
 * Additional cleanup for multi-threading
 */
pub(crate) unsafe extern "C" fn xmlCleanupGlobalsInternal() {
    xmlResetError(xmlLastError());

    xml_cleanup_mutex(addr_of_mut!(XML_THR_DEF_MUTEX));
    __xml_global_init_mutex_destroy();
}
