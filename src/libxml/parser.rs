//! Provide methods and data structures for parsing XML documents.
//!
//! This module is based on `libxml/parser.h`, `parser.c`, and so on in `libxml2-v2.11.8`.  
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: the core parser module
// Description: Interfaces, constants and types related to the XML parser
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// parser.c : an XML 1.0 parser, namespaces and validity support are mostly
//            implemented on top of the SAX interfaces
//
// References:
//   The XML specification:
//     http://www.w3.org/TR/REC-xml
//   Original 1.0 version:
//     http://www.w3.org/TR/1998/REC-xml-19980210
//   XML second edition working draft
//     http://www.w3.org/TR/2000/WD-xml-2e-20000814
//
// Okay this is a big file, the parser core is around 7000 lines, then it
// is followed by the progressive parser top routines, then the various
// high level APIs to call the parser and a few miscellaneous functions.
// A number of helper functions and deprecated ones have been moved to
// parserInternals.c to reduce this file size.
// As much as possible the functions are associated with their relative
// production in the XML specification. A few productions defining the
// different ranges of character are actually implanted either in
// parserInternals.h or parserInternals.c
// The DOM tree build is realized from the default SAX callbacks in
// the module SAX.c.
// The routines doing the validation checks are in valid.c and called either
// from the SAX callbacks or as standalone functions using a preparsed
// document.
//
// See Copyright for the status of this software.
//
// daniel@veillard.com

use std::{
    ffi::{CStr, c_void},
    sync::atomic::{AtomicBool, AtomicPtr, Ordering},
};

#[cfg(feature = "catalog")]
use crate::libxml::catalog::xml_catalog_cleanup;
#[cfg(feature = "schema")]
use crate::relaxng::xml_relaxng_cleanup_types;
use crate::{
    error::{XmlError, XmlParserErrors},
    globals::{GenericErrorContext, StructuredError},
    io::{
        cleanup_input_callbacks, cleanup_output_callbacks, register_default_input_callbacks,
        register_default_output_callbacks, xml_default_external_entity_loader, xml_no_net_exists,
    },
    libxml::{
        dict::{__xml_initialize_dict, xml_cleanup_dict_internal},
        globals::{xml_cleanup_globals_internal, xml_init_globals_internal},
        xmlmemory::{xml_cleanup_memory_internal, xml_init_memory_internal},
        xmlschemastypes::xml_schema_cleanup_types,
        xmlstring::XmlChar,
    },
    parser::{
        XmlParserCtxtPtr, XmlParserInput, XmlParserInputState, XmlParserOption,
        xml_create_memory_parser_ctxt, xml_fatal_err, xml_free_parser_ctxt,
    },
    tree::{
        NodeCommon, XML_XML_NAMESPACE, XmlAttributeDefault, XmlAttributeType, XmlDocProperties,
        XmlDocPtr, XmlElementContentPtr, XmlElementTypeVal, XmlEntityPtr, XmlEntityType,
        XmlEnumeration, XmlGenericNodePtr, xml_free_doc, xml_new_doc, xml_new_doc_node,
    },
    uri::canonic_path,
    xpath::xml_init_xpath_internal,
};

use super::threads::{
    __xml_global_init_mutex_lock, __xml_global_init_mutex_unlock, xml_cleanup_threads_internal,
    xml_init_threads_internal,
};

#[repr(C)]
#[derive(Clone, Default)]
pub(crate) struct XmlStartTag {
    pub(crate) prefix: Option<String>,
    pub(crate) uri: Option<String>,
    pub(crate) line: i32,
    pub(crate) ns_nr: i32,
}

pub type XmlSAXLocatorPtr = *mut XmlSAXLocator;
/// A SAX Locator.
#[doc(alias = "xmlSAXLocator")]
#[repr(C)]
pub struct XmlSAXLocator {
    pub(crate) get_public_id: unsafe fn(ctx: *mut c_void) -> *const XmlChar,
    pub(crate) get_system_id: unsafe fn(ctx: *mut c_void) -> Option<String>,
    pub(crate) get_line_number: unsafe fn(ctx: *mut c_void) -> i32,
    pub(crate) get_column_number: unsafe fn(ctx: *mut c_void) -> i32,
}

/// Callback:
/// The entity loader, to control the loading of external entities,
/// the application can either:
///    - override this resolveEntity() callback in the SAX block
///    - or better use the xmlSetExternalEntityLoader() function to
///      set up it's own entity resolution routine
///
/// Returns the xmlParserInputPtr if inlined or NULL for DOM behaviour.
#[doc(alias = "resolveEntitySAXFunc")]
pub type ResolveEntitySAXFunc = unsafe fn(
    ctx: Option<GenericErrorContext>,
    public_id: Option<&str>,
    system_id: Option<&str>,
) -> Option<XmlParserInput>;
/// Callback on internal subset declaration.
#[doc(alias = "internalSubsetSAXFunc")]
pub type InternalSubsetSAXFunc = unsafe fn(
    ctx: Option<GenericErrorContext>,
    name: Option<&str>,
    external_id: Option<&str>,
    system_id: Option<&str>,
);

/// Callback on external subset declaration.
#[doc(alias = "externalSubsetSAXFunc")]
pub type ExternalSubsetSAXFunc = unsafe fn(
    ctx: Option<GenericErrorContext>,
    name: Option<&str>,
    external_id: Option<&str>,
    system_id: Option<&str>,
);

/// Get an entity by name.
///
/// Returns the xmlEntityPtr if found.
#[doc(alias = "getEntitySAXFunc")]
pub type GetEntitySAXFunc =
    unsafe fn(ctx: Option<GenericErrorContext>, name: &str) -> Option<XmlEntityPtr>;

/// Get a parameter entity by name.
///
/// Returns the xmlEntityPtr if found.
#[doc(alias = "getParameterEntitySAXFunc")]
pub type GetParameterEntitySAXFunc =
    unsafe fn(ctx: Option<GenericErrorContext>, name: &str) -> Option<XmlEntityPtr>;

/// An entity definition has been parsed.
#[doc(alias = "entityDeclSAXFunc")]
pub type EntityDeclSAXFunc = unsafe fn(
    ctx: Option<GenericErrorContext>,
    name: &str,
    typ: XmlEntityType,
    public_id: Option<&str>,
    system_id: Option<&str>,
    content: Option<&str>,
);

/// What to do when a notation declaration has been parsed.
#[doc(alias = "notationDeclSAXFunc")]
pub type NotationDeclSAXFunc = unsafe fn(
    ctx: Option<GenericErrorContext>,
    name: &str,
    public_id: Option<&str>,
    system_id: Option<&str>,
);

/// An attribute definition has been parsed.
#[doc(alias = "attributeDeclSAXFunc")]
pub type AttributeDeclSAXFunc = unsafe fn(
    ctx: Option<GenericErrorContext>,
    elem: &str,
    fullname: &str,
    typ: XmlAttributeType,
    def: XmlAttributeDefault,
    default_value: Option<&str>,
    tree: Option<Box<XmlEnumeration>>,
);

/// An element definition has been parsed.
#[doc(alias = "elementDeclSAXFunc")]
pub type ElementDeclSAXFunc = unsafe fn(
    ctx: Option<GenericErrorContext>,
    name: &str,
    typ: Option<XmlElementTypeVal>,
    content: XmlElementContentPtr,
);

/// What to do when an unparsed entity declaration is parsed.
#[doc(alias = "unparsedEntityDeclSAXFunc")]
pub type UnparsedEntityDeclSAXFunc = unsafe fn(
    ctx: Option<GenericErrorContext>,
    name: &str,
    public_id: Option<&str>,
    system_id: Option<&str>,
    notation_name: Option<&str>,
);

/// Receive the document locator at startup, actually xmlDefaultSAXLocator.
/// Everything is available on the context, so this is useless in our case.
#[doc(alias = "setDocumentLocatorSAXFunc")]
pub type SetDocumentLocatorSAXFunc =
    unsafe fn(ctx: Option<GenericErrorContext>, loc: XmlSAXLocatorPtr);

/// Called when the document start being processed.
#[doc(alias = "startDocumentSAXFunc")]
pub type StartDocumentSAXFunc = unsafe fn(ctx: Option<GenericErrorContext>);
/// Called when the document end has been detected.
#[doc(alias = "endDocumentSAXFunc")]
pub type EndDocumentSAXFunc = unsafe fn(ctx: Option<GenericErrorContext>);
/// Called when an opening tag has been processed.
///
/// The elements of `atts` are `(attribute_name, attribute_value)`.
#[doc(alias = "startElementSAXFunc")]
pub type StartElementSAXFunc =
    unsafe fn(ctx: Option<GenericErrorContext>, name: &str, atts: &[(String, Option<String>)]);

/// Called when the end of an element has been detected.
#[doc(alias = "endElementSAXFunc")]
pub type EndElementSAXFunc = unsafe fn(ctx: Option<GenericErrorContext>, name: &str);

/// Handle an attribute that has been read by the parser.
/// The default handling is to convert the attribute into an
/// DOM subtree and past it in a new xmlAttr element added to
/// the element.
#[doc(alias = "attributeSAXFunc")]
pub type AttributeSAXFunc = unsafe fn(ctx: Option<GenericErrorContext>, name: &str, value: &str);

/// Called when an entity reference is detected.
#[doc(alias = "referenceSAXFunc")]
pub type ReferenceSAXFunc = unsafe fn(ctx: Option<GenericErrorContext>, name: &str);

/// Receiving some chars from the parser.
#[doc(alias = "charactersSAXFunc")]
pub type CharactersSAXFunc = unsafe fn(ctx: Option<GenericErrorContext>, ch: &str);

/// Receiving some ignorable whitespaces from the parser.
/// UNUSED: by default the DOM building will use characters.
#[doc(alias = "ignorableWhitespaceSAXFunc")]
pub type IgnorableWhitespaceSAXFunc = unsafe fn(ctx: Option<GenericErrorContext>, ch: &str);

/// A processing instruction has been parsed.
#[doc(alias = "processingInstructionSAXFunc")]
pub type ProcessingInstructionSAXFunc =
    unsafe fn(ctx: Option<GenericErrorContext>, target: &str, data: Option<&str>);

/// A comment has been parsed.
#[doc(alias = "commentSAXFunc")]
pub type CommentSAXFunc = unsafe fn(ctx: Option<GenericErrorContext>, value: &str);

/// Called when a pcdata block has been parsed.
#[doc(alias = "cdataBlockSAXFunc")]
pub type CDATABlockSAXFunc = unsafe fn(ctx: Option<GenericErrorContext>, value: &str);

/// Display and format a warning messages, callback.
#[doc(alias = "warningSAXFunc")]
pub type WarningSAXFunc = fn(ctx: Option<GenericErrorContext>, msg: &str);

/// Display and format an error messages, callback.
#[doc(alias = "errorSAXFunc")]
pub type ErrorSAXFunc = fn(ctx: Option<GenericErrorContext>, msg: &str);

/// Display and format fatal error messages, callback.
/// # Note
/// so far fatalError() SAX callbacks are not used, error() get all the callbacks for errors.
#[doc(alias = "fatalErrorSAXFunc")]
pub type FatalErrorSAXFunc = fn(ctx: Option<GenericErrorContext>, msg: &str);

/// Is this document tagged standalone?
///
/// Returns 1 if true
#[doc(alias = "isStandaloneSAXFunc")]
pub type IsStandaloneSAXFunc = unsafe fn(ctx: Option<GenericErrorContext>) -> i32;
/// Does this document has an internal subset.
///
/// Returns 1 if true
#[doc(alias = "hasInternalSubsetSAXFunc")]
pub type HasInternalSubsetSAXFunc = unsafe fn(ctx: Option<GenericErrorContext>) -> i32;

/// Does this document has an external subset?
///
/// Returns 1 if true
#[doc(alias = "hasExternalSubsetSAXFunc")]
pub type HasExternalSubsetSAXFunc = unsafe fn(ctx: Option<GenericErrorContext>) -> i32;

/// SAX2 callback when an element start has been detected by the parser.
/// It provides the namespace information for the element, as well as
/// the new namespace declarations on the element.
#[doc(alias = "startElementNsSAX2Func")]
pub type StartElementNsSAX2Func = unsafe fn(
    ctx: Option<GenericErrorContext>,
    localname: &str,
    prefix: Option<&str>,
    uri: Option<&str>,
    namespaces: &[(Option<String>, String)],
    nb_defaulted: usize,
    attributes: &[(String, Option<String>, Option<String>, String)],
);

/// SAX2 callback when an element end has been detected by the parser.
/// It provides the namespace information for the element.
#[doc(alias = "endElementNsSAX2Func")]
pub type EndElementNsSAX2Func = unsafe fn(
    ctx: Option<GenericErrorContext>,
    localname: &str,
    prefix: Option<&str>,
    uri: Option<&str>,
);

pub type XmlSAXHandlerPtr = *mut XmlSAXHandler;
/// A SAX handler is bunch of callbacks called by the parser when processing
/// of the input generate data or structure information.
#[doc(alias = "xmlSAXHandler")]
#[repr(C)]
#[derive(Debug, Default)]
pub struct XmlSAXHandler {
    pub internal_subset: Option<InternalSubsetSAXFunc>,
    pub is_standalone: Option<IsStandaloneSAXFunc>,
    pub has_internal_subset: Option<HasInternalSubsetSAXFunc>,
    pub has_external_subset: Option<HasExternalSubsetSAXFunc>,
    pub resolve_entity: Option<ResolveEntitySAXFunc>,
    pub get_entity: Option<GetEntitySAXFunc>,
    pub entity_decl: Option<EntityDeclSAXFunc>,
    pub notation_decl: Option<NotationDeclSAXFunc>,
    pub attribute_decl: Option<AttributeDeclSAXFunc>,
    pub element_decl: Option<ElementDeclSAXFunc>,
    pub unparsed_entity_decl: Option<UnparsedEntityDeclSAXFunc>,
    pub set_document_locator: Option<SetDocumentLocatorSAXFunc>,
    pub start_document: Option<StartDocumentSAXFunc>,
    pub end_document: Option<EndDocumentSAXFunc>,
    pub start_element: Option<StartElementSAXFunc>,
    pub end_element: Option<EndElementSAXFunc>,
    pub reference: Option<ReferenceSAXFunc>,
    pub characters: Option<CharactersSAXFunc>,
    pub ignorable_whitespace: Option<IgnorableWhitespaceSAXFunc>,
    pub processing_instruction: Option<ProcessingInstructionSAXFunc>,
    pub comment: Option<CommentSAXFunc>,
    pub warning: Option<WarningSAXFunc>,
    pub error: Option<ErrorSAXFunc>,
    pub fatal_error: Option<FatalErrorSAXFunc>, // unused error() get all the errors
    pub get_parameter_entity: Option<GetParameterEntitySAXFunc>,
    pub cdata_block: Option<CDATABlockSAXFunc>,
    pub external_subset: Option<ExternalSubsetSAXFunc>,
    pub initialized: u32,
    // The following fields are extensions available only on version 2
    pub _private: AtomicPtr<c_void>,
    pub start_element_ns: Option<StartElementNsSAX2Func>,
    pub end_element_ns: Option<EndElementNsSAX2Func>,
    pub serror: Option<StructuredError>,
}

/// External entity loaders types.
///
/// Returns the entity input parser.
#[doc(alias = "xmlExternalEntityLoader")]
pub type XmlExternalEntityLoader = unsafe fn(
    url: Option<&str>,
    id: Option<&str>,
    context: XmlParserCtxtPtr,
) -> Option<XmlParserInput>;

static XML_PARSER_INITIALIZED: AtomicBool = AtomicBool::new(false);

/// Initialization function for the XML parser.
/// This is not reentrant. Call once before processing in case of
/// use in multithreaded programs.
#[doc(alias = "xmlInitParser")]
pub unsafe fn xml_init_parser() {
    unsafe {
        // Note that the initialization code must not make memory allocations.
        if XML_PARSER_INITIALIZED.load(Ordering::Acquire) {
            return;
        }

        __xml_global_init_mutex_lock();
        if !XML_PARSER_INITIALIZED.load(Ordering::Acquire) {
            xml_init_threads_internal();
            xml_init_globals_internal();
            xml_init_memory_internal();
            __xml_initialize_dict();
            register_default_input_callbacks();
            #[cfg(feature = "libxml_output")]
            {
                register_default_output_callbacks();
            }
            #[cfg(any(feature = "xpath", feature = "schema"))]
            {
                xml_init_xpath_internal();
            }
            XML_PARSER_INITIALIZED.store(true, Ordering::Release);
        }

        __xml_global_init_mutex_unlock();
    }
}

/// This function name is somewhat misleading. It does not clean up
/// parser state, it cleans up memory allocated by the library itself.
/// It is a cleanup function for the XML library. It tries to reclaim all
/// related global memory allocated for the library processing.
/// It doesn't deallocate any document related memory. One should
/// call xmlCleanupParser() only when the process has finished using
/// the library and all XML/HTML documents built with it.
/// See also xmlInitParser() which has the opposite function of preparing
/// the library for operations.
///
/// # Warning
/// if your application is multithreaded or has plugin support
/// calling this may crash the application if another thread or
/// a plugin is still using libxml2. It's sometimes very hard to
/// guess if libxml2 is in use in the application, some libraries
/// or plugins may use it without notice. In case of doubt abstain
/// from calling this function or do it just before calling exit()
/// to avoid leak reports from valgrind !
#[doc(alias = "xmlCleanupParser")]
pub unsafe fn xml_cleanup_parser() {
    unsafe {
        if !XML_PARSER_INITIALIZED.load(Ordering::Acquire) {
            return;
        }

        #[cfg(feature = "catalog")]
        {
            xml_catalog_cleanup();
        }
        xml_cleanup_dict_internal();
        cleanup_input_callbacks();
        #[cfg(feature = "libxml_output")]
        {
            cleanup_output_callbacks();
        }
        #[cfg(feature = "schema")]
        {
            xml_schema_cleanup_types();
            xml_relaxng_cleanup_types();
        }
        xml_cleanup_globals_internal();
        xml_cleanup_threads_internal();
        xml_cleanup_memory_internal();
        XML_PARSER_INITIALIZED.store(false, Ordering::Release);
    }
}

/// Parse a well-balanced chunk of an XML document
/// called by the parser
/// The allowed sequence for the Well Balanced Chunk is the one defined by
/// the content production in the XML grammar:
///
/// `[43] content ::= (element | CharData | Reference | CDSect | PI | Comment)*`
///
/// Returns 0 if the chunk is well balanced, -1 in case of args problem and
/// the parser error code otherwise
#[doc(alias = "xmlParseBalancedChunkMemory")]
#[cfg(feature = "sax1")]
pub unsafe fn xml_parse_balanced_chunk_memory(
    doc: Option<XmlDocPtr>,
    sax: Option<Box<XmlSAXHandler>>,
    user_data: Option<GenericErrorContext>,
    depth: i32,
    string: *const XmlChar,
    lst: Option<&mut Option<XmlGenericNodePtr>>,
) -> i32 {
    unsafe { xml_parse_balanced_chunk_memory_recover(doc, sax, user_data, depth, string, lst, 0) }
}

/// Parse a well-balanced chunk of an XML document
/// called by the parser
/// The allowed sequence for the Well Balanced Chunk is the one defined by
/// the content production in the XML grammar:
///
/// `[43] content ::= (element | CharData | Reference | CDSect | PI | Comment)*`
///
/// Returns 0 if the chunk is well balanced, -1 in case of args problem and
///    the parser error code otherwise
///
/// In case recover is set to 1, the nodelist will not be empty even if
/// the parsed chunk is not well balanced, assuming the parsing succeeded to
/// some extent.
#[doc(alias = "xmlParseBalancedChunkMemoryRecover")]
#[cfg(feature = "sax1")]
pub unsafe fn xml_parse_balanced_chunk_memory_recover(
    doc: Option<XmlDocPtr>,
    sax: Option<Box<XmlSAXHandler>>,
    user_data: Option<GenericErrorContext>,
    depth: i32,
    string: *const XmlChar,
    mut lst: Option<&mut Option<XmlGenericNodePtr>>,
    recover: i32,
) -> i32 {
    unsafe {
        let replaced = sax.is_some();
        let mut oldsax = None;
        let ret: i32;

        if depth > 40 {
            return XmlParserErrors::XmlErrEntityLoop as i32;
        }

        if let Some(lst) = lst.as_mut() {
            **lst = None;
        }
        if string.is_null() {
            return -1;
        }

        let ctxt: XmlParserCtxtPtr =
            xml_create_memory_parser_ctxt(CStr::from_ptr(string as *const i8).to_bytes().to_vec());
        std::ptr::write(&mut (*ctxt).last_error, XmlError::default());
        if ctxt.is_null() {
            return -1;
        }
        (*ctxt).user_data = Some(GenericErrorContext::new(ctxt));
        if let Some(sax) = sax {
            oldsax = (*ctxt).sax.replace(sax);
            if user_data.is_some() {
                (*ctxt).user_data = user_data;
            }
        }
        let Some(mut new_doc) = xml_new_doc(Some("1.0")) else {
            xml_free_parser_ctxt(ctxt);
            return -1;
        };
        new_doc.properties = XmlDocProperties::XmlDocInternal as i32;
        (*ctxt).use_options_internal(XmlParserOption::XmlParseNoDict as i32, None);
        // doc.is_null() is only supported for historic reasons
        if let Some(doc) = doc {
            new_doc.int_subset = doc.int_subset;
            new_doc.ext_subset = doc.ext_subset;
        }
        let Some(new_root) = xml_new_doc_node(Some(new_doc), None, "pseudoroot", None) else {
            if replaced {
                (*ctxt).sax = oldsax;
            }
            xml_free_parser_ctxt(ctxt);
            new_doc.int_subset = None;
            new_doc.ext_subset = None;
            xml_free_doc(new_doc);
            return -1;
        };
        new_doc.add_child(new_root.into());
        (*ctxt).node_push(new_root);
        // doc.is_null() is only supported for historic reasons
        if let Some(mut doc) = doc {
            (*ctxt).my_doc = Some(new_doc);
            new_doc.children().unwrap().set_document(Some(doc));
            // Ensure that doc has XML spec namespace
            let d = doc;
            doc.search_ns_by_href(Some(d), XML_XML_NAMESPACE.to_str().unwrap());
            new_doc.old_ns = doc.old_ns;
        } else {
            (*ctxt).my_doc = Some(new_doc);
        }
        (*ctxt).instate = XmlParserInputState::XmlParserContent;
        (*ctxt).input_id = 2;
        (*ctxt).depth = depth;

        // Doing validity checking on chunk doesn't make sense
        (*ctxt).validate = 0;
        (*ctxt).loadsubset = 0;
        (*ctxt).detect_sax2();

        if let Some(mut doc) = doc {
            let content = doc.children.take();
            (*ctxt).parse_content();
            doc.children = content;
        } else {
            (*ctxt).parse_content();
        }
        if (*ctxt).current_byte() == b'<' && (*ctxt).nth_byte(1) == b'/' {
            xml_fatal_err(&mut *ctxt, XmlParserErrors::XmlErrNotWellBalanced, None);
        } else if (*ctxt).current_byte() != 0 {
            xml_fatal_err(&mut *ctxt, XmlParserErrors::XmlErrExtraContent, None);
        }
        if new_doc.children != (*ctxt).node.map(|node| node.into()) {
            xml_fatal_err(&mut *ctxt, XmlParserErrors::XmlErrNotWellBalanced, None);
        }

        if (*ctxt).well_formed == 0 {
            if (*ctxt).err_no == 0 {
                ret = 1;
            } else {
                ret = (*ctxt).err_no;
            }
        } else {
            ret = 0;
        }

        if let Some(lst) = lst {
            if ret == 0 || recover == 1 {
                // Return the newly created nodeset after unlinking it from
                // they pseudo parent.
                let mut cur = new_doc.children().unwrap().children();
                *lst = cur;
                while let Some(mut now) = cur {
                    now.set_doc(doc);
                    now.set_parent(None);
                    cur = now.next();
                }
                new_doc.children().unwrap().set_children(None);
            }
        }

        if replaced {
            (*ctxt).sax = oldsax;
        }
        xml_free_parser_ctxt(ctxt);
        new_doc.int_subset = None;
        new_doc.ext_subset = None;
        // This leaks the namespace list if doc.is_null()
        new_doc.old_ns = None;
        xml_free_doc(new_doc);

        ret
    }
}

/// Creates a parser context for an XML in-memory document.
///
/// Returns the new parser context or NULL
#[doc(alias = "xmlCreateDocParserCtxt")]
pub unsafe fn xml_create_doc_parser_ctxt(cur: Vec<u8>) -> XmlParserCtxtPtr {
    unsafe { xml_create_memory_parser_ctxt(cur) }
}

pub(crate) const XML_PARSER_BIG_BUFFER_SIZE: usize = 300;

// const XML_PARSER_BIG_ENTITY: usize = 1000;
// const XML_PARSER_LOT_ENTITY: usize = 5000;

// XML_PARSER_NON_LINEAR is roughly the maximum allowed amplification factor
// of serialized output after entity expansion.
pub(crate) const XML_PARSER_NON_LINEAR: usize = 5;

// A certain amount is always allowed.
pub(crate) const XML_PARSER_ALLOWED_EXPANSION: usize = 1000000;

// Fixed cost for each entity reference. This crudely models processing time
// as well to protect, for example, against exponential expansion of empty
// or very short entities.
pub(crate) const XML_ENT_FIXED_COST: usize = 20;

static mut XML_CURRENT_EXTERNAL_ENTITY_LOADER: XmlExternalEntityLoader =
    xml_default_external_entity_loader;

/// Changes the defaultexternal entity resolver function for the application
#[doc(alias = "xmlSetExternalEntityLoader")]
pub unsafe fn xml_set_external_entity_loader(f: XmlExternalEntityLoader) {
    unsafe {
        XML_CURRENT_EXTERNAL_ENTITY_LOADER = f;
    }
}

/// Returns the xmlExternalEntityLoader function pointer
#[doc(alias = "xmlGetExternalEntityLoader")]
pub unsafe fn xml_get_external_entity_loader() -> XmlExternalEntityLoader {
    unsafe { XML_CURRENT_EXTERNAL_ENTITY_LOADER }
}

/// Load an external entity, note that the use of this function for
/// unparsed entities may generate problems
///
/// Returns the xmlParserInputPtr or NULL
#[doc(alias = "xmlLoadExternalEntity")]
pub unsafe fn xml_load_external_entity(
    url: Option<&str>,
    id: Option<&str>,
    ctxt: XmlParserCtxtPtr,
) -> Option<XmlParserInput> {
    unsafe {
        if let Some(url) = url.filter(|_| xml_no_net_exists(url) == 0) {
            let canonic_filename = canonic_path(url);
            return XML_CURRENT_EXTERNAL_ENTITY_LOADER(Some(&canonic_filename), id, ctxt);
        }
        XML_CURRENT_EXTERNAL_ENTITY_LOADER(url, id, ctxt)
    }
}
