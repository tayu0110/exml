//! Provide methods and data structures for old SAX1 handlers.  
//! This module is based on `libxml/SAX.h`, `SAX.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// Summary: Old SAX version 1 handler, deprecated
// Description: DEPRECATED set of SAX version 1 interfaces used to build the DOM tree.
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// SAX.c : Old SAX v1 handlers to build a tree. Deprecated except for compatibility
//
// See Copyright for the status of this software.
//
// Daniel Veillard <daniel@veillard.com>

use std::{ffi::c_void, ptr::null_mut};

use crate::libxml::{
    entities::XmlEntityPtr,
    globals::xml_generic_error_context,
    parser::{XmlParserInputPtr, XmlSAXHandlerV1, XmlSaxlocatorPtr},
    sax2::{
        xmlSAX2AttributeDecl, xmlSAX2CDataBlock, xmlSAX2Characters, xmlSAX2Comment,
        xmlSAX2ElementDecl, xmlSAX2EndDocument, xmlSAX2EndElement, xmlSAX2EntityDecl,
        xmlSAX2ExternalSubset, xmlSAX2GetColumnNumber, xmlSAX2GetEntity, xmlSAX2GetLineNumber,
        xmlSAX2GetParameterEntity, xmlSAX2GetPublicId, xmlSAX2GetSystemId,
        xmlSAX2HasExternalSubset, xmlSAX2HasInternalSubset, xmlSAX2InternalSubset,
        xmlSAX2IsStandalone, xmlSAX2NotationDecl, xmlSAX2ProcessingInstruction, xmlSAX2Reference,
        xmlSAX2ResolveEntity, xmlSAX2StartDocument, xmlSAX2StartElement, xmlSAX2UnparsedEntityDecl,
    },
    tree::{XmlElementContentPtr, XmlEnumerationPtr, XmlNsPtr},
    xmlstring::XmlChar,
};

static mut DEPRECATED_V1_MSG: i32 = 0;

macro_rules! DEPRECATED {
    ($n:literal) => {
        if DEPRECATED_V1_MSG == 0 {
            $crate::xml_generic_error!(
                xml_generic_error_context(),
                c"Use of deprecated SAXv1 function %s\n".as_ptr() as _,
                $n.as_ptr()
            );
            DEPRECATED_V1_MSG += 1;
        }
    };
}

/**
 * getPublicId:
 * @ctx: the user data (XML parser context)
 *
 * Provides the public ID e.g. "-//SGMLSOURCE//DTD DEMO//EN"
 * DEPRECATED: use xmlSAX2GetPublicId()
 *
 * Returns a xmlChar *
 */
#[deprecated]
pub unsafe extern "C" fn getPublicId(ctx: *mut c_void) -> *const XmlChar {
    DEPRECATED!(c"getPublicId");
    xmlSAX2GetPublicId(ctx)
}

/**
 * getSystemId:
 * @ctx: the user data (XML parser context)
 *
 * Provides the system ID, basically URL or filename e.g.
 * http://www.sgmlsource.com/dtds/memo.dtd
 * DEPRECATED: use xmlSAX2GetSystemId()
 *
 * Returns a xmlChar *
 */
#[deprecated]
pub unsafe extern "C" fn getSystemId(ctx: *mut c_void) -> *const XmlChar {
    DEPRECATED!(c"getSystemId");
    xmlSAX2GetSystemId(ctx)
}

/**
 * setDocumentLocator:
 * @ctx: the user data (XML parser context)
 * @loc: A SAX Locator
 *
 * Receive the document locator at startup, actually xmlDefaultSAXLocator
 * Everything is available on the context, so this is useless in our case.
 * DEPRECATED
 */
#[deprecated]
pub unsafe extern "C" fn setDocumentLocator(_ctx: *mut c_void, _loc: XmlSaxlocatorPtr) {
    DEPRECATED!(c"setDocumentLocator");
}

/**
 * getLineNumber:
 * @ctx: the user data (XML parser context)
 *
 * Provide the line number of the current parsing point.
 * DEPRECATED: use xmlSAX2GetLineNumber()
 *
 * Returns an int
 */
#[deprecated]
pub unsafe extern "C" fn getLineNumber(ctx: *mut c_void) -> i32 {
    DEPRECATED!(c"getLineNumber");
    xmlSAX2GetLineNumber(ctx)
}

/**
 * getColumnNumber:
 * @ctx: the user data (XML parser context)
 *
 * Provide the column number of the current parsing point.
 * DEPRECATED: use xmlSAX2GetColumnNumber()
 *
 * Returns an int
 */
#[deprecated]
pub unsafe extern "C" fn getColumnNumber(ctx: *mut c_void) -> i32 {
    DEPRECATED!(c"getColumnNumber");
    xmlSAX2GetColumnNumber(ctx)
}

/**
 * isStandalone:
 * @ctx: the user data (XML parser context)
 *
 * Is this document tagged standalone ?
 * DEPRECATED: use xmlSAX2IsStandalone()
 *
 * Returns 1 if true
 */
#[deprecated]
pub unsafe extern "C" fn isStandalone(ctx: *mut c_void) -> i32 {
    DEPRECATED!(c"isStandalone");
    xmlSAX2IsStandalone(ctx)
}

/**
 * hasInternalSubset:
 * @ctx: the user data (XML parser context)
 *
 * Does this document has an internal subset
 * DEPRECATED: use xmlSAX2HasInternalSubset()
 *
 * Returns 1 if true
 */
#[deprecated]
pub unsafe extern "C" fn hasInternalSubset(ctx: *mut c_void) -> i32 {
    DEPRECATED!(c"hasInternalSubset");
    xmlSAX2HasInternalSubset(ctx)
}

/**
 * hasExternalSubset:
 * @ctx: the user data (XML parser context)
 *
 * Does this document has an external subset
 * DEPRECATED: use xmlSAX2HasExternalSubset()
 *
 * Returns 1 if true
 */
#[deprecated]
pub unsafe extern "C" fn hasExternalSubset(ctx: *mut c_void) -> i32 {
    DEPRECATED!(c"hasExternalSubset");
    xmlSAX2HasExternalSubset(ctx)
}

/**
 * internalSubset:
 * @ctx:  the user data (XML parser context)
 * @name:  the root element name
 * @ExternalID:  the external ID
 * @SystemID:  the SYSTEM ID (e.g. filename or URL)
 *
 * Callback on internal subset declaration.
 * DEPRECATED: use xmlSAX2InternalSubset()
 */
#[deprecated]
pub unsafe extern "C" fn internalSubset(
    ctx: *mut c_void,
    name: *const XmlChar,
    external_id: *const XmlChar,
    system_id: *const XmlChar,
) {
    DEPRECATED!(c"internalSubset");
    xmlSAX2InternalSubset(ctx, name, external_id, system_id);
}

/**
 * externalSubset:
 * @ctx: the user data (XML parser context)
 * @name:  the root element name
 * @ExternalID:  the external ID
 * @SystemID:  the SYSTEM ID (e.g. filename or URL)
 *
 * Callback on external subset declaration.
 * DEPRECATED: use xmlSAX2ExternalSubset()
 */
#[deprecated]
pub unsafe extern "C" fn externalSubset(
    ctx: *mut c_void,
    name: *const XmlChar,
    external_id: *const XmlChar,
    system_id: *const XmlChar,
) {
    DEPRECATED!(c"externalSubset");
    xmlSAX2ExternalSubset(ctx, name, external_id, system_id);
}

/**
 * getEntity:
 * @ctx: the user data (XML parser context)
 * @name: The entity name
 *
 * Get an entity by name
 * DEPRECATED: use xmlSAX2GetEntity()
 *
 * Returns the xmlEntityPtr if found.
 */
#[deprecated]
pub unsafe extern "C" fn getEntity(ctx: *mut c_void, name: *const XmlChar) -> XmlEntityPtr {
    DEPRECATED!(c"getEntity");
    xmlSAX2GetEntity(ctx, name)
}

/**
 * getParameterEntity:
 * @ctx: the user data (XML parser context)
 * @name: The entity name
 *
 * Get a parameter entity by name
 * DEPRECATED: use xmlSAX2GetParameterEntity()
 *
 * Returns the xmlEntityPtr if found.
 */
#[deprecated]
pub unsafe extern "C" fn getParameterEntity(
    ctx: *mut c_void,
    name: *const XmlChar,
) -> XmlEntityPtr {
    DEPRECATED!(c"getParameterEntity");
    xmlSAX2GetParameterEntity(ctx, name)
}

/**
 * resolveEntity:
 * @ctx: the user data (XML parser context)
 * @publicId: The public ID of the entity
 * @systemId: The system ID of the entity
 *
 * The entity loader, to control the loading of external entities,
 * the application can either:
 *    - override this resolveEntity() callback in the SAX block
 *    - or better use the xmlSetExternalEntityLoader() function to
 *      set up it's own entity resolution routine
 * DEPRECATED: use xmlSAX2ResolveEntity()
 *
 * Returns the xmlParserInputPtr if inlined or NULL for DOM behaviour.
 */
#[deprecated]
pub unsafe extern "C" fn resolveEntity(
    ctx: *mut c_void,
    public_id: *const XmlChar,
    system_id: *const XmlChar,
) -> XmlParserInputPtr {
    DEPRECATED!(c"resolveEntity");
    xmlSAX2ResolveEntity(ctx, public_id, system_id)
}

/**
 * entityDecl:
 * @ctx: the user data (XML parser context)
 * @name:  the entity name
 * @type:  the entity type
 * @publicId: The public ID of the entity
 * @systemId: The system ID of the entity
 * @content: the entity value (without processing).
 *
 * An entity definition has been parsed
 * DEPRECATED: use xmlSAX2EntityDecl()
 */
#[deprecated]
pub unsafe extern "C" fn entityDecl(
    ctx: *mut c_void,
    name: *const XmlChar,
    typ: i32,
    public_id: *const XmlChar,
    system_id: *const XmlChar,
    content: *mut XmlChar,
) {
    DEPRECATED!(c"entityDecl");
    xmlSAX2EntityDecl(ctx, name, typ, public_id, system_id, content);
}

/**
 * attributeDecl:
 * @ctx: the user data (XML parser context)
 * @elem:  the name of the element
 * @fullname:  the attribute name
 * @type:  the attribute type
 * @def:  the type of default value
 * @defaultValue: the attribute default value
 * @tree:  the tree of enumerated value set
 *
 * An attribute definition has been parsed
 * DEPRECATED: use xmlSAX2AttributeDecl()
 */
#[deprecated]
pub unsafe extern "C" fn attributeDecl(
    ctx: *mut c_void,
    elem: *const XmlChar,
    fullname: *const XmlChar,
    typ: i32,
    def: i32,
    default_value: *const XmlChar,
    tree: XmlEnumerationPtr,
) {
    DEPRECATED!(c"attributeDecl");
    xmlSAX2AttributeDecl(ctx, elem, fullname, typ, def, default_value, tree);
}

/**
 * elementDecl:
 * @ctx: the user data (XML parser context)
 * @name:  the element name
 * @type:  the element type
 * @content: the element value tree
 *
 * An element definition has been parsed
 * DEPRECATED: use xmlSAX2ElementDecl()
 */
#[deprecated]
pub unsafe extern "C" fn elementDecl(
    ctx: *mut c_void,
    name: *const XmlChar,
    typ: i32,
    content: XmlElementContentPtr,
) {
    DEPRECATED!(c"elementDecl");
    xmlSAX2ElementDecl(ctx, name, typ, content);
}

/**
 * notationDecl:
 * @ctx: the user data (XML parser context)
 * @name: The name of the notation
 * @publicId: The public ID of the entity
 * @systemId: The system ID of the entity
 *
 * What to do when a notation declaration has been parsed.
 * DEPRECATED: use xmlSAX2NotationDecl()
 */
#[deprecated]
pub unsafe extern "C" fn notationDecl(
    ctx: *mut c_void,
    name: *const XmlChar,
    public_id: *const XmlChar,
    system_id: *const XmlChar,
) {
    DEPRECATED!(c"notationDecl");
    xmlSAX2NotationDecl(ctx, name, public_id, system_id);
}

/**
 * unparsedEntityDecl:
 * @ctx: the user data (XML parser context)
 * @name: The name of the entity
 * @publicId: The public ID of the entity
 * @systemId: The system ID of the entity
 * @notationName: the name of the notation
 *
 * What to do when an unparsed entity declaration is parsed
 * DEPRECATED: use xmlSAX2UnparsedEntityDecl()
 */
#[deprecated]
pub unsafe extern "C" fn unparsedEntityDecl(
    ctx: *mut c_void,
    name: *const XmlChar,
    public_id: *const XmlChar,
    system_id: *const XmlChar,
    notation_name: *const XmlChar,
) {
    DEPRECATED!(c"unparsedEntityDecl");
    xmlSAX2UnparsedEntityDecl(ctx, name, public_id, system_id, notation_name);
}

/**
 * startDocument:
 * @ctx: the user data (XML parser context)
 *
 * called when the document start being processed.
 * DEPRECATED: use xmlSAX2StartDocument()
 */
#[deprecated]
pub unsafe extern "C" fn startDocument(ctx: *mut c_void) {
    /* don't be too painful for glade users */
    /*  DEPRECATED("startDocument") */
    xmlSAX2StartDocument(ctx);
}

/**
 * endDocument:
 * @ctx: the user data (XML parser context)
 *
 * called when the document end has been detected.
 * DEPRECATED: use xmlSAX2EndDocument()
 */
#[deprecated]
pub unsafe extern "C" fn endDocument(ctx: *mut c_void) {
    DEPRECATED!(c"endDocument");
    xmlSAX2EndDocument(ctx);
}

/**
 * attribute:
 * @ctx: the user data (XML parser context)
 * @fullname:  The attribute name, including namespace prefix
 * @value:  The attribute value
 *
 * Handle an attribute that has been read by the parser.
 * The default handling is to convert the attribute into an
 * DOM subtree and past it in a new xmlAttr element added to
 * the element.
 * DEPRECATED: use xmlSAX2Attribute()
 */
#[deprecated]
pub unsafe extern "C" fn attribute(
    _ctx: *mut c_void,
    _fullname: *const XmlChar,
    _value: *const XmlChar,
) {
    DEPRECATED!(c"attribute");
}

/**
 * startElement:
 * @ctx: the user data (XML parser context)
 * @fullname:  The element name, including namespace prefix
 * @atts:  An array of name/value attributes pairs, NULL terminated
 *
 * called when an opening tag has been processed.
 * DEPRECATED: use xmlSAX2StartElement()
 */
#[deprecated]
pub unsafe extern "C" fn startElement(
    ctx: *mut c_void,
    fullname: *const XmlChar,
    atts: *mut *const XmlChar,
) {
    xmlSAX2StartElement(ctx, fullname, atts);
}

/**
 * endElement:
 * @ctx: the user data (XML parser context)
 * @name:  The element name
 *
 * called when the end of an element has been detected.
 * DEPRECATED: use xmlSAX2EndElement()
 */
#[deprecated]
pub unsafe extern "C" fn endElement(ctx: *mut c_void, name: *const XmlChar) {
    DEPRECATED!(c"endElement");
    xmlSAX2EndElement(ctx, name);
}

/**
 * reference:
 * @ctx: the user data (XML parser context)
 * @name:  The entity name
 *
 * called when an entity reference is detected.
 * DEPRECATED: use xmlSAX2Reference()
 */
#[deprecated]
pub unsafe extern "C" fn reference(ctx: *mut c_void, name: *const XmlChar) {
    DEPRECATED!(c"reference");
    xmlSAX2Reference(ctx, name);
}

/**
 * characters:
 * @ctx: the user data (XML parser context)
 * @ch:  a xmlChar string
 * @len: the number of xmlChar
 *
 * receiving some chars from the parser.
 * DEPRECATED: use xmlSAX2Characters()
 */
#[deprecated]
pub unsafe extern "C" fn characters(ctx: *mut c_void, ch: *const XmlChar, len: i32) {
    DEPRECATED!(c"characters");
    xmlSAX2Characters(ctx, ch, len);
}

/**
 * ignorableWhitespace:
 * @ctx: the user data (XML parser context)
 * @ch:  a xmlChar string
 * @len: the number of xmlChar
 *
 * receiving some ignorable whitespaces from the parser.
 * UNUSED: by default the DOM building will use characters
 * DEPRECATED: use xmlSAX2IgnorableWhitespace()
 */
#[deprecated]
pub unsafe extern "C" fn ignorableWhitespace(_ctx: *mut c_void, _ch: *const XmlChar, _len: i32) {
    DEPRECATED!(c"ignorableWhitespace");
}

/**
 * processingInstruction:
 * @ctx: the user data (XML parser context)
 * @target:  the target name
 * @data: the PI data's
 *
 * A processing instruction has been parsed.
 * DEPRECATED: use xmlSAX2ProcessingInstruction()
 */
#[deprecated]
pub unsafe extern "C" fn processingInstruction(
    ctx: *mut c_void,
    target: *const XmlChar,
    data: *const XmlChar,
) {
    DEPRECATED!(c"processingInstruction");
    xmlSAX2ProcessingInstruction(ctx, target, data);
}

/**
 * globalNamespace:
 * @ctx: the user data (XML parser context)
 * @href:  the namespace associated URN
 * @prefix: the namespace prefix
 *
 * An old global namespace has been parsed.
 * DEPRECATED
 */
#[deprecated]
pub unsafe extern "C" fn globalNamespace(
    _ctx: *mut c_void,
    _href: *const XmlChar,
    _prefix: *const XmlChar,
) {
    DEPRECATED!(c"globalNamespace");
}

/**
 * setNamespace:
 * @ctx: the user data (XML parser context)
 * @name:  the namespace prefix
 *
 * Set the current element namespace.
 * DEPRECATED
 */
#[deprecated]
pub unsafe extern "C" fn setNamespace(_ctx: *mut c_void, _name: *const XmlChar) {
    DEPRECATED!(c"setNamespace");
}

/**
 * getNamespace:
 * @ctx: the user data (XML parser context)
 *
 * Get the current element namespace.
 * DEPRECATED
 *
 * Returns the xmlNsPtr or NULL if none
 */
#[deprecated]
pub unsafe extern "C" fn getNamespace(_ctx: *mut c_void) -> XmlNsPtr {
    //  DEPRECATED("getNamespace")
    null_mut()
}

/**
 * checkNamespace:
 * @ctx: the user data (XML parser context)
 * @namespace: the namespace to check against
 *
 * Check that the current element namespace is the same as the
 * one read upon parsing.
 * DEPRECATED
 *
 * Returns 1 if true 0 otherwise
 */
#[deprecated]
pub unsafe extern "C" fn checkNamespace(_ctx: *mut c_void, _name_space: *mut XmlChar) -> i32 {
    //  DEPRECATED("checkNamespace")
    0
}

/**
 * namespaceDecl:
 * @ctx: the user data (XML parser context)
 * @href:  the namespace associated URN
 * @prefix: the namespace prefix
 *
 * A namespace has been parsed.
 * DEPRECATED
 */
#[deprecated]
pub unsafe extern "C" fn namespaceDecl(
    _ctx: *mut c_void,
    _href: *const XmlChar,
    _prefix: *const XmlChar,
) {
    DEPRECATED!(c"namespaceDecl");
}

/**
 * comment:
 * @ctx: the user data (XML parser context)
 * @value:  the comment content
 *
 * A comment has been parsed.
 * DEPRECATED: use xmlSAX2Comment()
 */
#[deprecated]
pub unsafe extern "C" fn comment(ctx: *mut c_void, value: *const XmlChar) {
    DEPRECATED!(c"comment");
    xmlSAX2Comment(ctx, value);
}

/**
 * cdataBlock:
 * @ctx: the user data (XML parser context)
 * @value:  The pcdata content
 * @len:  the block length
 *
 * called when a pcdata block has been parsed
 * DEPRECATED: use xmlSAX2CDataBlock()
 */
#[deprecated]
pub unsafe extern "C" fn cdataBlock(ctx: *mut c_void, value: *const XmlChar, len: i32) {
    DEPRECATED!(c"cdataBlock");
    xmlSAX2CDataBlock(ctx, value, len);
}

/**
 * initxmlDefaultSAXHandler:
 * @hdlr:  the SAX handler
 * @warning:  flag if non-zero sets the handler warning procedure
 *
 * Initialize the default XML SAX version 1 handler
 * DEPRECATED: use xmlSAX2InitDefaultSAXHandler() for the new SAX2 blocks
 */
#[deprecated]
#[cfg(feature = "sax1")]
pub unsafe extern "C" fn initxmlDefaultSAXHandler(hdlr: *mut XmlSAXHandlerV1, warning: i32) {
    use crate::libxml::{
        sax2::xmlSAX2SetDocumentLocator,
        xmlerror::{xmlParserError, xmlParserWarning},
    };

    if (*hdlr).initialized == 1 {
        return;
    }

    (*hdlr).internal_subset = Some(xmlSAX2InternalSubset);
    (*hdlr).external_subset = Some(xmlSAX2ExternalSubset);
    (*hdlr).is_standalone = Some(xmlSAX2IsStandalone);
    (*hdlr).has_internal_subset = Some(xmlSAX2HasInternalSubset);
    (*hdlr).has_external_subset = Some(xmlSAX2HasExternalSubset);
    (*hdlr).resolve_entity = Some(xmlSAX2ResolveEntity);
    (*hdlr).get_entity = Some(xmlSAX2GetEntity);
    (*hdlr).get_parameter_entity = Some(xmlSAX2GetParameterEntity);
    (*hdlr).entity_decl = Some(xmlSAX2EntityDecl);
    (*hdlr).attribute_decl = Some(xmlSAX2AttributeDecl);
    (*hdlr).element_decl = Some(xmlSAX2ElementDecl);
    (*hdlr).notation_decl = Some(xmlSAX2NotationDecl);
    (*hdlr).unparsed_entity_decl = Some(xmlSAX2UnparsedEntityDecl);
    (*hdlr).set_document_locator = Some(xmlSAX2SetDocumentLocator);
    (*hdlr).start_document = Some(xmlSAX2StartDocument);
    (*hdlr).end_document = Some(xmlSAX2EndDocument);
    (*hdlr).start_element = Some(xmlSAX2StartElement);
    (*hdlr).end_element = Some(xmlSAX2EndElement);
    (*hdlr).reference = Some(xmlSAX2Reference);
    (*hdlr).characters = Some(xmlSAX2Characters);
    (*hdlr).cdata_block = Some(xmlSAX2CDataBlock);
    (*hdlr).ignorable_whitespace = Some(xmlSAX2Characters);
    (*hdlr).processing_instruction = Some(xmlSAX2ProcessingInstruction);
    if warning == 0 {
        (*hdlr).warning = None;
    } else {
        (*hdlr).warning = Some(xmlParserWarning);
    }
    (*hdlr).error = Some(xmlParserError);
    (*hdlr).fatal_error = Some(xmlParserError);

    (*hdlr).initialized = 1;
}

/**
 * inithtmlDefaultSAXHandler:
 * @hdlr:  the SAX handler
 *
 * Initialize the default HTML SAX version 1 handler
 * DEPRECATED: use xmlSAX2InitHtmlDefaultSAXHandler() for the new SAX2 blocks
 */
#[deprecated]
#[cfg(all(feature = "sax1", feature = "html"))]
pub unsafe extern "C" fn inithtmlDefaultSAXHandler(hdlr: *mut XmlSAXHandlerV1) {
    use crate::libxml::{
        sax2::{xmlSAX2IgnorableWhitespace, xmlSAX2SetDocumentLocator},
        xmlerror::{xmlParserError, xmlParserWarning},
    };

    if (*hdlr).initialized == 1 {
        return;
    }

    (*hdlr).internal_subset = Some(xmlSAX2InternalSubset);
    (*hdlr).external_subset = None;
    (*hdlr).is_standalone = None;
    (*hdlr).has_internal_subset = None;
    (*hdlr).has_external_subset = None;
    (*hdlr).resolve_entity = None;
    (*hdlr).get_entity = Some(xmlSAX2GetEntity);
    (*hdlr).get_parameter_entity = None;
    (*hdlr).entity_decl = None;
    (*hdlr).attribute_decl = None;
    (*hdlr).element_decl = None;
    (*hdlr).notation_decl = None;
    (*hdlr).unparsed_entity_decl = None;
    (*hdlr).set_document_locator = Some(xmlSAX2SetDocumentLocator);
    (*hdlr).start_document = Some(xmlSAX2StartDocument);
    (*hdlr).end_document = Some(xmlSAX2EndDocument);
    (*hdlr).start_element = Some(xmlSAX2StartElement);
    (*hdlr).end_element = Some(xmlSAX2EndElement);
    (*hdlr).reference = None;
    (*hdlr).characters = Some(xmlSAX2Characters);
    (*hdlr).cdata_block = Some(xmlSAX2CDataBlock);
    (*hdlr).ignorable_whitespace = Some(xmlSAX2IgnorableWhitespace);
    (*hdlr).processing_instruction = Some(xmlSAX2ProcessingInstruction);
    (*hdlr).comment = Some(xmlSAX2Comment);
    (*hdlr).warning = Some(xmlParserWarning);
    (*hdlr).error = Some(xmlParserError);
    (*hdlr).fatal_error = Some(xmlParserError);

    (*hdlr).initialized = 1;
}
