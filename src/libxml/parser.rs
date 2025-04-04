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
    ffi::{CStr, c_char, c_void},
    io::Read,
    ptr::null_mut,
    slice::from_raw_parts,
    sync::atomic::{AtomicBool, AtomicPtr, Ordering},
};

#[cfg(feature = "catalog")]
use crate::libxml::catalog::xml_catalog_cleanup;
#[cfg(feature = "schema")]
use crate::relaxng::xml_relaxng_cleanup_types;
#[cfg(feature = "libxml_valid")]
use crate::tree::XmlDtdPtr;
use crate::{
    encoding::{XmlCharEncoding, detect_encoding, find_encoding_handler},
    error::{XmlError, XmlParserErrors},
    globals::{GenericErrorContext, StructuredError},
    html::parser::{__html_parse_content, HtmlParserOption, html_create_memory_parser_ctxt},
    io::{
        XmlParserInputBuffer, cleanup_input_callbacks, cleanup_output_callbacks,
        register_default_input_callbacks, register_default_output_callbacks,
        xml_default_external_entity_loader, xml_no_net_exists, xml_parser_get_directory,
    },
    libxml::{
        dict::{__xml_initialize_dict, xml_cleanup_dict_internal},
        globals::{
            xml_cleanup_globals_internal, xml_default_sax_locator, xml_init_globals_internal,
        },
        xmlmemory::{xml_cleanup_memory_internal, xml_init_memory_internal},
        xmlschemastypes::xml_schema_cleanup_types,
        xmlstring::XmlChar,
    },
    parser::{
        XML_DEFAULT_VERSION, XmlParserCtxtPtr, XmlParserInput, XmlParserInputState,
        XmlParserOption, xml_create_memory_parser_ctxt, xml_err_memory, xml_fatal_err,
        xml_fatal_err_msg_str, xml_free_parser_ctxt, xml_new_sax_parser_ctxt,
    },
    tree::{
        NodeCommon, XML_XML_NAMESPACE, XmlAttributeDefault, XmlAttributeType, XmlDocProperties,
        XmlDocPtr, XmlElementContentPtr, XmlElementType, XmlElementTypeVal, XmlEntityPtr,
        XmlEntityType, XmlEnumeration, XmlGenericNodePtr, XmlNodePtr, xml_free_doc, xml_free_node,
        xml_free_node_list, xml_new_doc, xml_new_doc_comment, xml_new_doc_node, xml_new_dtd,
    },
    uri::canonic_path,
    xpath::xml_init_xpath_internal,
};

use super::{
    chvalid::xml_is_blank_char,
    threads::{
        __xml_global_init_mutex_lock, __xml_global_init_mutex_unlock, xml_cleanup_threads_internal,
        xml_init_threads_internal,
    },
};

/// Callback for freeing some parser input allocations.
#[doc(alias = "xmlParserInputDeallocate")]
pub type XmlParserInputDeallocate = unsafe fn(*mut XmlChar) -> c_void;

/// Bit in the loadsubset context field to tell to do ID/REFs lookups.
/// Use it to initialize xmlLoadExtDtdDefaultValue.
pub const XML_DETECT_IDS: usize = 2;

/// Bit in the loadsubset context field to tell to do complete the
/// elements attributes lists with the ones defaulted from the DTDs.
/// Use it to initialize xmlLoadExtDtdDefaultValue.
pub const XML_COMPLETE_ATTRS: usize = 4;

/// Bit in the loadsubset context field to tell to not do ID/REFs registration.
/// Used to initialize xmlLoadExtDtdDefaultValue in some special cases.
pub const XML_SKIP_IDS: usize = 8;

/// A parser can operate in various modes
#[doc(alias = "xmlParserMode")]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum XmlParserMode {
    #[default]
    XmlParseUnknown = 0,
    XmlParseDOM = 1,
    XmlParseSAX = 2,
    XmlParsePushDOM = 3,
    XmlParsePushSAX = 4,
    XmlParseReader = 5,
}

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

/// Special constant found in SAX2 blocks initialized fields
pub const XML_SAX2_MAGIC: usize = 0xDEEDBEAF;

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

/// parse a general parsed entity
/// An external general parsed entity is well-formed if it matches the
/// production labeled extParsedEnt.
///
/// `[78] extParsedEnt ::= TextDecl? content`
///
/// Returns 0, -1 in case of error. the parser context is augmented as a result of the parsing.
#[doc(alias = "xmlParseExtParsedEnt")]
pub unsafe fn xml_parse_ext_parsed_ent(ctxt: XmlParserCtxtPtr) -> i32 {
    unsafe {
        if ctxt.is_null() || (*ctxt).input().is_none() {
            return -1;
        }

        (*ctxt).detect_sax2();

        (*ctxt).grow();

        // SAX: beginning of the document processing.
        if let Some(set_document_locator) = (*ctxt)
            .sax
            .as_deref_mut()
            .and_then(|sax| sax.set_document_locator)
        {
            set_document_locator((*ctxt).user_data.clone(), xml_default_sax_locator());
        }

        // Get the 4 first bytes and decode the charset
        // if enc != XML_CHAR_ENCODING_NONE
        // plug some encoding conversion routines.
        if (*ctxt).input().unwrap().remainder_len() >= 4 {
            let enc = detect_encoding(&(*ctxt).content_bytes()[..4]);
            if !matches!(enc, XmlCharEncoding::None) {
                (*ctxt).switch_encoding(enc);
            }
        }

        if (*ctxt).current_byte() == 0 {
            xml_fatal_err(&mut *ctxt, XmlParserErrors::XmlErrDocumentEmpty, None);
        }

        // Check for the XMLDecl in the Prolog.
        (*ctxt).grow();
        if (*ctxt).content_bytes().starts_with(b"<?xml")
            && xml_is_blank_char((*ctxt).nth_byte(5) as u32)
        {
            // Note that we will switch encoding on the fly.
            (*ctxt).parse_xmldecl();
            if (*ctxt).err_no == XmlParserErrors::XmlErrUnsupportedEncoding as i32 {
                // The XML REC instructs us to stop parsing right here
                return -1;
            }
            (*ctxt).skip_blanks();
        } else {
            (*ctxt).version = Some(XML_DEFAULT_VERSION.to_owned());
        }
        if (*ctxt).disable_sax == 0 {
            if let Some(start_document) = (*ctxt)
                .sax
                .as_deref_mut()
                .and_then(|sax| sax.start_document)
            {
                start_document((*ctxt).user_data.clone());
            }
        }
        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            return -1;
        }

        // Doing validity checking on chunk doesn't make sense
        (*ctxt).instate = XmlParserInputState::XmlParserContent;
        (*ctxt).validate = 0;
        (*ctxt).loadsubset = 0;
        (*ctxt).depth = 0;

        (*ctxt).parse_content();
        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            return -1;
        }

        if (*ctxt).current_byte() == b'<' && (*ctxt).nth_byte(1) == b'/' {
            xml_fatal_err(&mut *ctxt, XmlParserErrors::XmlErrNotWellBalanced, None);
        } else if (*ctxt).current_byte() != 0 {
            xml_fatal_err(&mut *ctxt, XmlParserErrors::XmlErrExtraContent, None);
        }

        // SAX: end of the document processing.
        if let Some(end_document) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_document) {
            end_document((*ctxt).user_data.clone());
        }

        if (*ctxt).well_formed == 0 {
            return -1;
        }
        0
    }
}

/// Load and parse an external subset.
///
/// Returns the resulting xmlDtdPtr or NULL in case of error.
#[doc(alias = "xmlParseDTD")]
#[cfg(feature = "libxml_valid")]
pub unsafe fn xml_parse_dtd(
    external_id: Option<&str>,
    system_id: Option<&str>,
) -> Option<XmlDtdPtr> {
    use crate::parser::xml_sax_parse_dtd;

    unsafe { xml_sax_parse_dtd(None, external_id, system_id) }
}

/// Load and parse a DTD
///
/// Returns the resulting xmlDtdPtr or NULL in case of error.
/// `input` will be freed by the function in any case.
#[doc(alias = "xmlIOParseDTD")]
#[cfg(feature = "libxml_valid")]
pub unsafe fn xml_io_parse_dtd(
    sax: Option<Box<XmlSAXHandler>>,
    input: XmlParserInputBuffer,
    mut enc: XmlCharEncoding,
) -> Option<XmlDtdPtr> {
    use crate::parser::XmlParserOption;

    unsafe {
        use crate::parser::xml_new_sax_parser_ctxt;

        let Ok(ctxt) = xml_new_sax_parser_ctxt(sax, None) else {
            return None;
        };

        // We are loading a DTD
        (*ctxt).options |= XmlParserOption::XmlParseDTDLoad as i32;

        (*ctxt).detect_sax2();

        // generate a parser input from the I/O handler
        let Some(pinput) = XmlParserInput::from_io(ctxt, input, XmlCharEncoding::None) else {
            xml_free_parser_ctxt(ctxt);
            return None;
        };
        let input_id = pinput.id;

        // plug some encoding conversion routines here.
        if (*ctxt).push_input(pinput) < 0 {
            xml_free_parser_ctxt(ctxt);
            return None;
        }
        if !matches!(enc, XmlCharEncoding::None) {
            (*ctxt).switch_encoding(enc);
        }

        if let Some(pinput) = (*ctxt)
            .input_tab
            .iter_mut()
            .find(|input| input.id == input_id)
        {
            pinput.filename = None;
            pinput.line = 1;
            pinput.col = 1;
            pinput.base.drain(..pinput.cur);
            pinput.cur = 0;
        }

        // let's parse that entity knowing it's an external subset.
        (*ctxt).in_subset = 2;
        (*ctxt).my_doc = xml_new_doc(Some("1.0"));
        let Some(mut my_doc) = (*ctxt).my_doc else {
            xml_err_memory(Some(&mut *ctxt), Some("New Doc failed"));
            return None;
        };
        my_doc.properties = XmlDocProperties::XmlDocInternal as i32;
        my_doc.ext_subset = xml_new_dtd((*ctxt).my_doc, Some("none"), Some("none"), Some("none"));

        if matches!(enc, XmlCharEncoding::None) && (*ctxt).input().unwrap().remainder_len() >= 4 {
            // Get the 4 first bytes and decode the charset
            // if enc != xmlCharEncoding::XML_CHAR_ENCODING_NONE
            // plug some encoding conversion routines.
            enc = detect_encoding(&(*ctxt).content_bytes()[..4]);
            if !matches!(enc, XmlCharEncoding::None) {
                (*ctxt).switch_encoding(enc);
            }
        }

        (*ctxt).parse_external_subset(Some("none"), Some("none"));

        let mut ret = None;
        if let Some(mut my_doc) = (*ctxt).my_doc.take() {
            if (*ctxt).well_formed != 0 {
                ret = my_doc.ext_subset.take();
                if let Some(mut ret) = ret {
                    ret.doc = None;
                    let mut tmp = ret.children();
                    while let Some(mut now) = tmp {
                        now.set_document(None);
                        tmp = now.next();
                    }
                }
            }
            xml_free_doc(my_doc);
        }
        xml_free_parser_ctxt(ctxt);

        ret
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
/// within the context (DTD, namespaces, etc ...) of the given node.
///
/// The allowed sequence for the data is a Well Balanced Chunk defined by
/// the content production in the XML grammar:
///
/// `[43] content ::= (element | CharData | Reference | CDSect | PI | Comment)*`
///
/// Returns xmlParserErrors::XML_ERR_OK if the chunk is well balanced, and the parser
/// error code otherwise
#[doc(alias = "xmlParseInNodeContext")]
pub unsafe fn xml_parse_in_node_context(
    node: XmlGenericNodePtr,
    data: Vec<u8>,
    mut options: i32,
    lst: &mut Option<XmlGenericNodePtr>,
) -> XmlParserErrors {
    unsafe {
        let ctxt: XmlParserCtxtPtr;
        let mut nsnr = 0;
        let ret: XmlParserErrors;

        match node.element_type() {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlAttributeNode
            | XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlPINode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlDocumentNode
            | XmlElementType::XmlHTMLDocumentNode => {}
            _ => {
                return XmlParserErrors::XmlErrInternalError;
            }
        }
        let mut node = Some(node);
        while let Some(now) = node.filter(|node| {
            !matches!(
                node.element_type(),
                XmlElementType::XmlElementNode
                    | XmlElementType::XmlDocumentNode
                    | XmlElementType::XmlHTMLDocumentNode
            )
        }) {
            node = now.parent();
        }
        let Some(mut node) = node else {
            return XmlParserErrors::XmlErrInternalError;
        };
        let doc = if let Ok(doc) = XmlDocPtr::try_from(node) {
            Some(doc)
        } else {
            node.document()
        };
        let Some(doc) = doc else {
            return XmlParserErrors::XmlErrInternalError;
        };

        // allocate a context and set-up everything not related to the
        // node position in the tree
        if doc.typ == XmlElementType::XmlDocumentNode {
            ctxt = xml_create_memory_parser_ctxt(data);
        } else if cfg!(feature = "html") && doc.typ == XmlElementType::XmlHTMLDocumentNode {
            #[cfg(feature = "html")]
            {
                ctxt = html_create_memory_parser_ctxt(data);
                // When parsing in context, it makes no sense to add implied
                // elements like html/body/etc...
                options |= HtmlParserOption::HtmlParseNoimplied as i32;
            }
        } else {
            return XmlParserErrors::XmlErrInternalError;
        }

        if ctxt.is_null() {
            return XmlParserErrors::XmlErrNoMemory;
        }

        // Use input doc's dict if present, else assure XML_PARSE_NODICT is set.
        // We need a dictionary for xmlDetectSAX2, so if there's no doc dict
        // we must wait until the last moment to free the original one.
        options |= XmlParserOption::XmlParseNoDict as i32;

        if let Some(encoding) = doc.encoding.as_deref() {
            (*ctxt).encoding = Some(encoding.to_owned());

            if let Some(handler) = find_encoding_handler(encoding) {
                (*ctxt).switch_to_encoding(handler);
            } else {
                return XmlParserErrors::XmlErrUnsupportedEncoding;
            }
        }

        (*ctxt).use_options_internal(options, None);
        (*ctxt).detect_sax2();
        (*ctxt).my_doc = Some(doc);
        // parsing in context, i.e. as within existing content
        (*ctxt).input_id = 2;
        (*ctxt).instate = XmlParserInputState::XmlParserContent;

        let Some(mut fake) = xml_new_doc_comment(node.document(), "") else {
            xml_free_parser_ctxt(ctxt);
            return XmlParserErrors::XmlErrNoMemory;
        };
        node.add_child(fake.into());

        // At this point, `node.element_type()` is ElementNode, DocumentNode or HTMLDocumentNode.
        if let Ok(node) = XmlNodePtr::try_from(node) {
            (*ctxt).node_push(node);
            // initialize the SAX2 namespaces stack
            let mut cur = Some(node);
            while let Some(now) =
                cur.filter(|cur| cur.element_type() == XmlElementType::XmlElementNode)
            {
                let mut ns = now.ns_def;
                while let Some(cur_ns) = ns {
                    if (*ctxt).get_namespace(cur_ns.prefix().as_deref()).is_none() {
                        (*ctxt).ns_push(cur_ns.prefix().as_deref(), &cur_ns.href().unwrap());
                        nsnr += 1;
                    }
                    ns = cur_ns.next;
                }
                cur = now.parent().and_then(|p| XmlNodePtr::try_from(p).ok());
            }
        }

        if (*ctxt).validate != 0 || (*ctxt).replace_entities != 0 {
            // ID/IDREF registration will be done in xmlValidateElement below
            (*ctxt).loadsubset |= XML_SKIP_IDS as i32;
        }

        #[cfg(feature = "html")]
        {
            if doc.typ == XmlElementType::XmlHTMLDocumentNode {
                __html_parse_content(ctxt as _);
            } else {
                (*ctxt).parse_content();
            }
        }
        #[cfg(not(feature = "html"))]
        {
            (*ctxt).parse_content();
        }

        (*ctxt).ns_pop(nsnr);
        if (*ctxt).current_byte() == b'<' && (*ctxt).nth_byte(1) == b'/' {
            xml_fatal_err(&mut *ctxt, XmlParserErrors::XmlErrNotWellBalanced, None);
        } else if (*ctxt).current_byte() != 0 {
            xml_fatal_err(&mut *ctxt, XmlParserErrors::XmlErrExtraContent, None);
        }
        if (*ctxt)
            .node
            .is_some_and(|ctxt_node| XmlGenericNodePtr::from(ctxt_node) != node)
        {
            xml_fatal_err(&mut *ctxt, XmlParserErrors::XmlErrNotWellBalanced, None);
            (*ctxt).well_formed = 0;
        }

        if (*ctxt).well_formed == 0 {
            if (*ctxt).err_no == 0 {
                ret = XmlParserErrors::XmlErrInternalError;
            } else {
                ret = XmlParserErrors::try_from((*ctxt).err_no).unwrap();
            }
        } else {
            ret = XmlParserErrors::XmlErrOK;
        }

        // Return the newly created nodeset after unlinking it from
        // the pseudo sibling.

        let mut cur = fake.next.take();
        node.set_last(Some(fake.into()));

        if let Some(mut cur) = cur {
            cur.set_prev(None);
        }

        *lst = cur;

        while let Some(mut now) = cur {
            now.set_parent(None);
            cur = now.next();
        }

        fake.unlink();
        xml_free_node(fake);

        if !matches!(ret, XmlParserErrors::XmlErrOK) {
            xml_free_node_list(lst.take());
        }

        xml_free_parser_ctxt(ctxt);

        ret
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

/// Create a parser context for using the XML parser with an existing I/O stream
///
/// Returns the new parser context or NULL
#[doc(alias = "xmlCreateIOParserCtxt")]
pub unsafe fn xml_create_io_parser_ctxt(
    sax: Option<Box<XmlSAXHandler>>,
    user_data: Option<GenericErrorContext>,
    ioctx: impl Read + 'static,
    enc: XmlCharEncoding,
) -> XmlParserCtxtPtr {
    unsafe {
        let buf = XmlParserInputBuffer::from_reader(ioctx, enc);
        let Ok(ctxt) = xml_new_sax_parser_ctxt(sax, user_data) else {
            return null_mut();
        };

        let Some(input_stream) = XmlParserInput::from_io(ctxt, buf, enc) else {
            xml_free_parser_ctxt(ctxt);
            return null_mut();
        };
        (*ctxt).input_push(input_stream);

        ctxt
    }
}

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

/// Reset a push parser context
///
/// Returns 0 in case of success and 1 in case of error
#[doc(alias = "xmlCtxtResetPush")]
pub unsafe fn xml_ctxt_reset_push(
    ctxt: XmlParserCtxtPtr,
    chunk: *const c_char,
    size: i32,
    filename: Option<&str>,
    encoding: Option<&str>,
) -> i32 {
    unsafe {
        if ctxt.is_null() {
            return 1;
        }

        let enc = if encoding.is_none() && !chunk.is_null() && size >= 4 {
            let input = from_raw_parts(chunk as *const u8, size as usize);
            detect_encoding(input)
        } else {
            XmlCharEncoding::None
        };

        let buf = XmlParserInputBuffer::new(enc);

        if ctxt.is_null() {
            return 1;
        }

        (*ctxt).reset();

        if filename.is_none() {
            (*ctxt).directory = None;
        } else if let Some(dir) = filename.and_then(xml_parser_get_directory) {
            (*ctxt).directory = Some(dir.to_string_lossy().into_owned());
        }

        let Some(mut input_stream) = XmlParserInput::new((!ctxt.is_null()).then(|| &mut *ctxt))
        else {
            return 1;
        };

        if let Some(filename) = filename {
            let canonic = canonic_path(filename);
            input_stream.filename = Some(canonic.into_owned());
        } else {
            input_stream.filename = None;
        }
        input_stream.buf = Some(buf);
        input_stream.reset_base();

        (*ctxt).input_push(input_stream);

        if size > 0
            && !chunk.is_null()
            && (*ctxt).input().is_some()
            && (*ctxt).input().unwrap().buf.is_some()
        {
            let base = (*ctxt).input().unwrap().get_base();
            let cur = (*ctxt).input().unwrap().offset_from_base();

            (*ctxt)
                .input_mut()
                .unwrap()
                .buf
                .as_mut()
                .unwrap()
                .push_bytes(from_raw_parts(chunk as *const u8, size as usize));
            (*ctxt).input_mut().unwrap().set_base_and_cursor(base, cur);
        }

        if let Some(encoding) = encoding {
            (*ctxt).encoding = Some(encoding.to_owned());

            if let Some(handler) = find_encoding_handler((*ctxt).encoding().unwrap()) {
                (*ctxt).switch_to_encoding(handler);
            } else {
                xml_fatal_err_msg_str!(
                    &mut *ctxt,
                    XmlParserErrors::XmlErrUnsupportedEncoding,
                    "Unsupported encoding {}\n",
                    encoding
                );
            };
        } else if !matches!(enc, XmlCharEncoding::None) {
            (*ctxt).switch_encoding(enc);
        }

        0
    }
}
