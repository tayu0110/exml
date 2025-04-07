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

use std::{ffi::c_void, rc::Rc, sync::atomic::AtomicPtr};

use crate::{
    globals::{GenericErrorContext, StructuredError},
    libxml::xmlstring::XmlChar,
    parser::XmlParserInput,
    tree::{
        XmlAttributeDefault, XmlAttributeType, XmlElementContentPtr, XmlElementTypeVal,
        XmlEntityPtr, XmlEntityType, XmlEnumeration,
    },
};

use super::XmlParserCtxt;

#[repr(C)]
#[derive(Clone, Default)]
pub(crate) struct XmlStartTag {
    pub(crate) prefix: Option<Rc<str>>,
    pub(crate) uri: Option<Rc<str>>,
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
pub type ResolveEntitySAXFunc =
    unsafe fn(&mut XmlParserCtxt, Option<&str>, Option<&str>) -> Option<XmlParserInput>;
/// Callback on internal subset declaration.
#[doc(alias = "internalSubsetSAXFunc")]
pub type InternalSubsetSAXFunc =
    unsafe fn(&mut XmlParserCtxt, Option<&str>, Option<&str>, Option<&str>);

/// Callback on external subset declaration.
#[doc(alias = "externalSubsetSAXFunc")]
pub type ExternalSubsetSAXFunc =
    unsafe fn(&mut XmlParserCtxt, Option<&str>, Option<&str>, Option<&str>);

/// Get an entity by name.
///
/// Returns the xmlEntityPtr if found.
#[doc(alias = "getEntitySAXFunc")]
pub type GetEntitySAXFunc = unsafe fn(&mut XmlParserCtxt, &str) -> Option<XmlEntityPtr>;

/// Get a parameter entity by name.
///
/// Returns the xmlEntityPtr if found.
#[doc(alias = "getParameterEntitySAXFunc")]
pub type GetParameterEntitySAXFunc = unsafe fn(&mut XmlParserCtxt, &str) -> Option<XmlEntityPtr>;

/// An entity definition has been parsed.
#[doc(alias = "entityDeclSAXFunc")]
pub type EntityDeclSAXFunc =
    unsafe fn(&mut XmlParserCtxt, &str, XmlEntityType, Option<&str>, Option<&str>, Option<&str>);

/// What to do when a notation declaration has been parsed.
#[doc(alias = "notationDeclSAXFunc")]
pub type NotationDeclSAXFunc = unsafe fn(&mut XmlParserCtxt, &str, Option<&str>, Option<&str>);

/// An attribute definition has been parsed.
#[doc(alias = "attributeDeclSAXFunc")]
pub type AttributeDeclSAXFunc = unsafe fn(
    &mut XmlParserCtxt,
    &str,
    &str,
    XmlAttributeType,
    XmlAttributeDefault,
    Option<&str>,
    Option<Box<XmlEnumeration>>,
);

/// An element definition has been parsed.
#[doc(alias = "elementDeclSAXFunc")]
pub type ElementDeclSAXFunc =
    unsafe fn(&mut XmlParserCtxt, &str, Option<XmlElementTypeVal>, XmlElementContentPtr);

/// What to do when an unparsed entity declaration is parsed.
#[doc(alias = "unparsedEntityDeclSAXFunc")]
pub type UnparsedEntityDeclSAXFunc =
    unsafe fn(&mut XmlParserCtxt, &str, Option<&str>, Option<&str>, Option<&str>);

/// Receive the document locator at startup, actually xmlDefaultSAXLocator.
/// Everything is available on the context, so this is useless in our case.
#[doc(alias = "setDocumentLocatorSAXFunc")]
pub type SetDocumentLocatorSAXFunc = unsafe fn(&mut XmlParserCtxt, XmlSAXLocatorPtr);

/// Called when the document start being processed.
#[doc(alias = "startDocumentSAXFunc")]
pub type StartDocumentSAXFunc = unsafe fn(&mut XmlParserCtxt);
/// Called when the document end has been detected.
#[doc(alias = "endDocumentSAXFunc")]
pub type EndDocumentSAXFunc = unsafe fn(&mut XmlParserCtxt);
/// Called when an opening tag has been processed.
///
/// The elements of `atts` are `(attribute_name, attribute_value)`.
#[doc(alias = "startElementSAXFunc")]
pub type StartElementSAXFunc = unsafe fn(&mut XmlParserCtxt, &str, &[(String, Option<String>)]);

/// Called when the end of an element has been detected.
#[doc(alias = "endElementSAXFunc")]
pub type EndElementSAXFunc = unsafe fn(&mut XmlParserCtxt, &str);

/// Handle an attribute that has been read by the parser.
/// The default handling is to convert the attribute into an
/// DOM subtree and past it in a new xmlAttr element added to
/// the element.
#[doc(alias = "attributeSAXFunc")]
pub type AttributeSAXFunc = unsafe fn(&mut XmlParserCtxt, &str, &str);

/// Called when an entity reference is detected.
#[doc(alias = "referenceSAXFunc")]
pub type ReferenceSAXFunc = unsafe fn(&mut XmlParserCtxt, &str);

/// Receiving some chars from the parser.
#[doc(alias = "charactersSAXFunc")]
pub type CharactersSAXFunc = unsafe fn(&mut XmlParserCtxt, &str);

/// Receiving some ignorable whitespaces from the parser.
/// UNUSED: by default the DOM building will use characters.
#[doc(alias = "ignorableWhitespaceSAXFunc")]
pub type IgnorableWhitespaceSAXFunc = unsafe fn(&mut XmlParserCtxt, &str);

/// A processing instruction has been parsed.
#[doc(alias = "processingInstructionSAXFunc")]
pub type ProcessingInstructionSAXFunc = unsafe fn(&mut XmlParserCtxt, &str, Option<&str>);

/// A comment has been parsed.
#[doc(alias = "commentSAXFunc")]
pub type CommentSAXFunc = unsafe fn(&mut XmlParserCtxt, &str);

/// Called when a pcdata block has been parsed.
#[doc(alias = "cdataBlockSAXFunc")]
pub type CDATABlockSAXFunc = unsafe fn(&mut XmlParserCtxt, &str);

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
pub type IsStandaloneSAXFunc = unsafe fn(&mut XmlParserCtxt) -> i32;
/// Does this document has an internal subset.
///
/// Returns 1 if true
#[doc(alias = "hasInternalSubsetSAXFunc")]
pub type HasInternalSubsetSAXFunc = unsafe fn(&mut XmlParserCtxt) -> i32;

/// Does this document has an external subset?
///
/// Returns 1 if true
#[doc(alias = "hasExternalSubsetSAXFunc")]
pub type HasExternalSubsetSAXFunc = unsafe fn(&mut XmlParserCtxt) -> i32;

/// SAX2 callback when an element start has been detected by the parser.
/// It provides the namespace information for the element, as well as
/// the new namespace declarations on the element.
#[doc(alias = "startElementNsSAX2Func")]
pub type StartElementNsSAX2Func = unsafe fn(
    &mut XmlParserCtxt,
    &str,
    Option<&str>,
    Option<&str>,
    &[(Option<String>, String)],
    usize,
    &[(String, Option<String>, Option<String>, String)],
);

/// SAX2 callback when an element end has been detected by the parser.
/// It provides the namespace information for the element.
#[doc(alias = "endElementNsSAX2Func")]
pub type EndElementNsSAX2Func = unsafe fn(&mut XmlParserCtxt, &str, Option<&str>, Option<&str>);

// pub type XmlSAXHandlerPtr = *mut XmlSAXHandler;
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
