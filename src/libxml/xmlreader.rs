//! Provide methods and data structures for XML streaming APIs.  
//! This module is based on `libxml/xmlreader.h`, `xmlreader.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: the XMLReader implementation
// Description: API of the XML streaming API based on C# interfaces.
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// xmlreader.c: implements the xmlTextReader streaming node API
//
// NOTE:
//   XmlTextReader.Normalization Property won't be supported, since
//     it makes the parser non compliant to the XML recommendation
//
// See Copyright for the status of this software.
//
// daniel@veillard.com

use std::{
    any::type_name,
    ffi::c_char,
    io::Read,
    mem::size_of,
    os::raw::c_void,
    ptr::{addr_of_mut, null, null_mut},
    sync::atomic::Ordering,
};

#[cfg(feature = "schema")]
use crate::relaxng::{xml_relaxng_new_parser_ctxt, XmlRelaxNGValidCtxtPtr};
#[cfg(feature = "libxml_reader")]
use crate::tree::{XmlAttr, XmlAttrPtr, XmlDocPtr, XmlNs};
use crate::{
    error::{
        parser_error, parser_validity_error, parser_validity_warning, parser_warning, XmlError,
    },
    globals::{GenericErrorContext, StructuredError},
    io::XmlParserInputBuffer,
    libxml::{
        dict::{xml_dict_create, xml_dict_free, xml_dict_lookup, XmlDictPtr},
        globals::{xml_deregister_node_default_value, xml_free, xml_malloc},
        parser::{
            xml_create_push_parser_ctxt, xml_ctxt_use_options, xml_parse_chunk, CDATABlockSAXFunc,
            CharactersSAXFunc, EndElementNsSAX2Func, EndElementSAXFunc, StartElementNsSAX2Func,
            StartElementSAXFunc, XmlParserInputState, XmlParserMode, XmlParserOption,
            XmlSAXHandler, XML_COMPLETE_ATTRS, XML_DETECT_IDS, XML_SAX2_MAGIC,
        },
        pattern::{xml_free_pattern, xml_pattern_match, xml_patterncompile, XmlPatternPtr},
        relaxng::{
            xml_relaxng_free, xml_relaxng_parse, xml_relaxng_set_valid_errors,
            xml_relaxng_set_valid_structured_errors, xml_relaxng_validate_full_element,
            xml_relaxng_validate_push_cdata, XmlRelaxNGPtr,
        },
        sax2::xml_sax_version,
        valid::{xml_validate_pop_element, xml_validate_push_cdata, xml_validate_push_element},
        xinclude::{
            xml_xinclude_new_context, xml_xinclude_process_node, xml_xinclude_set_flags,
            xml_xinclude_set_streaming_mode, XmlXIncludeCtxtPtr,
        },
        xmlschemas::{
            xml_schema_free, xml_schema_free_parser_ctxt, xml_schema_free_valid_ctxt,
            xml_schema_is_valid, xml_schema_new_parser_ctxt, xml_schema_new_valid_ctxt,
            xml_schema_parse, xml_schema_sax_plug, xml_schema_sax_unplug,
            xml_schema_set_parser_errors, xml_schema_set_valid_errors,
            xml_schema_set_valid_structured_errors, xml_schema_validate_set_locator,
            XmlSchemaParserCtxtPtr, XmlSchemaPtr, XmlSchemaSAXPlugPtr, XmlSchemaValidCtxtPtr,
        },
        xmlstring::{xml_str_equal, xml_strcat, xml_strdup, xml_strlen, XmlChar},
    },
    parser::XmlParserCtxtPtr,
    tree::{
        xml_copy_dtd, xml_doc_copy_node, xml_free_doc, xml_free_dtd, xml_free_node, xml_free_ns,
        xml_free_ns_list, xml_new_doc_text, XmlDoc, XmlElementType, XmlNode,
        __XML_REGISTER_CALLBACKS,
    },
};

/// How severe an error callback is when the per-reader error callback API is used.
#[doc(alias = "xmlParserSeverities")]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlParserSeverities {
    XmlParserSeverityValidityWarning = 1,
    XmlParserSeverityValidityError = 2,
    XmlParserSeverityWarning = 3,
    XmlParserSeverityError = 4,
}

/// Internal state values for the reader.
#[doc(alias = "xmlTextReaderMode")]
#[cfg(feature = "libxml_reader")]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum XmlTextReaderMode {
    #[default]
    XmlTextreaderModeInitial = 0,
    XmlTextreaderModeInteractive = 1,
    XmlTextreaderModeError = 2,
    XmlTextreaderModeEof = 3,
    XmlTextreaderModeClosed = 4,
    XmlTextreaderModeReading = 5,
}

/// Some common options to use with xmlTextReaderSetParserProp, but it
/// is better to use xmlParserOption and the xmlReaderNewxxx and
/// xmlReaderForxxx APIs now.
#[doc(alias = "xmlParserProperties")]
#[cfg(feature = "libxml_reader")]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlParserProperties {
    XmlParserLoadDTD = 1,
    XmlParserDefaultAttrs = 2,
    XmlParserValidate = 3,
    XmlParserSubstEntities = 4,
}

impl TryFrom<i32> for XmlParserProperties {
    type Error = anyhow::Error;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        if value == Self::XmlParserLoadDTD as i32 {
            Ok(Self::XmlParserLoadDTD)
        } else if value == Self::XmlParserDefaultAttrs as i32 {
            Ok(Self::XmlParserDefaultAttrs)
        } else if value == Self::XmlParserValidate as i32 {
            Ok(Self::XmlParserValidate)
        } else if value == Self::XmlParserSubstEntities as i32 {
            Ok(Self::XmlParserSubstEntities)
        } else {
            Err(anyhow::anyhow!(
                "Invalid convert from value '{value}' to {}",
                type_name::<Self>()
            ))
        }
    }
}

/// Predefined constants for the different types of nodes.
#[doc(alias = "xmlReaderTypes")]
#[cfg(feature = "libxml_reader")]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlReaderTypes {
    XmlReaderTypeNone = 0,
    XmlReaderTypeElement = 1,
    XmlReaderTypeAttribute = 2,
    XmlReaderTypeText = 3,
    XmlReaderTypeCDATA = 4,
    XmlReaderTypeEntityReference = 5,
    XmlReaderTypeEntity = 6,
    XmlReaderTypeProcessingInstruction = 7,
    XmlReaderTypeComment = 8,
    XmlReaderTypeDocument = 9,
    XmlReaderTypeDocumentType = 10,
    XmlReaderTypeDocumentFragment = 11,
    XmlReaderTypeNotation = 12,
    XmlReaderTypeWhitespace = 13,
    XmlReaderTypeSignificantWhitespace = 14,
    XmlReaderTypeEndElement = 15,
    XmlReaderTypeEndEntity = 16,
    XmlReaderTypeXmlDeclaration = 17,
}

#[cfg(feature = "libxml_reader")]
const XML_TEXTREADER_INPUT: i32 = 1;
#[cfg(feature = "libxml_reader")]
const XML_TEXTREADER_CTXT: i32 = 2;

#[cfg(feature = "libxml_reader")]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum XmlTextReaderState {
    None = -1,
    #[default]
    Start = 0,
    Element = 1,
    End = 2,
    Empty = 3,
    Backtrack = 4,
    Done = 5,
    Error = 6,
}

#[cfg(feature = "libxml_reader")]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum XmlTextReaderValidate {
    #[default]
    NotValidate = 0,
    ValidateDtd = 1,
    ValidateRng = 2,
    ValidateXsd = 4,
}

/// Pointer to an xmlReader context.
#[doc(alias = "xmlTextReaderPtr")]
#[cfg(feature = "libxml_reader")]
pub type XmlTextReaderPtr = *mut XmlTextReader;
/// Structure for an xmlReader context.
#[doc(alias = "xmlTextReader")]
#[cfg(feature = "libxml_reader")]
#[repr(C)]
pub struct XmlTextReader {
    // the parsing mode
    mode: XmlTextReaderMode,
    // when walking an existing doc
    doc: Option<XmlDocPtr>,
    // is there any validation
    validate: XmlTextReaderValidate,
    // what structure were deallocated
    allocs: i32,
    state: XmlTextReaderState,
    // the parser context
    ctxt: XmlParserCtxtPtr,
    // the parser SAX callbacks
    // sax: Option<Box<XmlSAXHandler>>,
    // the input
    input: Option<XmlParserInputBuffer>,
    // initial SAX callbacks
    start_element: Option<StartElementSAXFunc>,
    // idem
    end_element: Option<EndElementSAXFunc>,
    // idem
    start_element_ns: Option<StartElementNsSAX2Func>,
    // idem
    end_element_ns: Option<EndElementNsSAX2Func>,
    characters: Option<CharactersSAXFunc>,
    cdata_block: Option<CDATABlockSAXFunc>,
    // base of the segment in the input
    base: u32,
    // current position in the input
    cur: u32,
    // current node
    node: *mut XmlNode,
    // current attribute node
    curnode: *mut XmlNode,
    // depth of the current node
    depth: i32,
    // fake xmlNs chld
    faketext: *mut XmlNode,
    // preserve the resulting document
    preserve: i32,
    // used to return const xmlChar *
    buffer: String,
    // the context dictionary
    dict: XmlDictPtr,

    // entity stack when traversing entities content
    // Current Entity Ref Node
    ent: *mut XmlNode,
    // // Depth of the entities stack
    // ent_nr: i32,
    // // Max depth of the entities stack
    // ent_max: i32,
    // array of entities
    ent_tab: Vec<*mut XmlNode>,

    // error handling
    // callback function
    error_func: Option<XmlTextReaderErrorFunc>,
    // callback function user argument
    error_func_arg: Option<GenericErrorContext>,

    // Handling of RelaxNG validation
    // The Relax NG schemas
    #[cfg(feature = "schema")]
    rng_schemas: XmlRelaxNGPtr,
    // The Relax NG validation context
    #[cfg(feature = "schema")]
    rng_valid_ctxt: XmlRelaxNGValidCtxtPtr,
    // 1 if the context was provided by the user
    #[cfg(feature = "schema")]
    rng_preserve_ctxt: i32,
    // The number of errors detected
    #[cfg(feature = "schema")]
    rng_valid_errors: i32,
    // the node if RNG not progressive
    #[cfg(feature = "schema")]
    rng_full_node: *mut XmlNode,

    // Handling of Schemas validation
    // The Schemas schemas
    #[cfg(feature = "schema")]
    xsd_schemas: XmlSchemaPtr,
    // The Schemas validation context
    #[cfg(feature = "schema")]
    xsd_valid_ctxt: XmlSchemaValidCtxtPtr,
    // 1 if the context was provided by the user
    #[cfg(feature = "schema")]
    xsd_preserve_ctxt: i32,
    // The number of errors detected
    #[cfg(feature = "schema")]
    xsd_valid_errors: i32,
    // the schemas plug in SAX pipeline
    #[cfg(feature = "schema")]
    xsd_plug: XmlSchemaSAXPlugPtr,

    // Handling of XInclude processing
    // is xinclude asked for
    #[cfg(feature = "xinclude")]
    xinclude: i32,
    // the xinclude name from dict
    #[cfg(feature = "xinclude")]
    xinclude_name: *const XmlChar,
    // the xinclude context
    #[cfg(feature = "xinclude")]
    xincctxt: XmlXIncludeCtxtPtr,
    // counts for xinclude
    #[cfg(feature = "xinclude")]
    in_xinclude: i32,
    // // number of preserve patterns
    // #[cfg(feature = "libxml_pattern")]
    // pattern_nr: i32,
    // // max preserve patterns
    // #[cfg(feature = "libxml_pattern")]
    // pattern_max: i32,
    // array of preserve patterns
    #[cfg(feature = "libxml_pattern")]
    pattern_tab: Vec<XmlPatternPtr>,
    // level of preserves
    preserves: i32,
    // the set of options set
    parser_flags: i32,

    // Structured error handling
    serror_func: Option<StructuredError>, /* callback function */
}

impl XmlTextReader {
    /// Moves the position of the current instance to the next node in the stream,
    /// exposing its properties.
    ///
    /// Returns 1 if the node was read successfully, 0 if there is no more nodes to read,
    /// or -1 in case of error
    #[doc(alias = "xmlTextReaderRead")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn read(&mut self) -> i32 {
        use crate::{
            libxml::xinclude::{XINCLUDE_NS, XINCLUDE_OLD_NS},
            tree::{NodeCommon, NodePtr},
        };

        let mut val: i32;
        let mut olddepth = 0;
        let mut oldstate = XmlTextReaderState::Start;
        let mut oldnode: *mut XmlNode = null_mut();

        self.curnode = null_mut();
        if self.doc.is_some() {
            return self.read_tree();
        }
        if self.ctxt.is_null() {
            return -1;
        }

        let mut node_found = false;
        if self.mode == XmlTextReaderMode::XmlTextreaderModeInitial {
            self.mode = XmlTextReaderMode::XmlTextreaderModeInteractive;
            // Initial state
            while {
                val = self.push_data();
                if val < 0 {
                    self.mode = XmlTextReaderMode::XmlTextreaderModeError;
                    self.state = XmlTextReaderState::Error;
                    return -1;
                }
                (*self.ctxt).node.is_none()
                    && (self.mode != XmlTextReaderMode::XmlTextreaderModeEof
                        && self.state != XmlTextReaderState::Done)
            } {}
            if (*self.ctxt).node.is_none() {
                if let Some(my_doc) = (*self.ctxt).my_doc {
                    self.node = my_doc.children.map_or(null_mut(), |c| c.as_ptr());
                }
                if self.node.is_null() {
                    self.mode = XmlTextReaderMode::XmlTextreaderModeError;
                    self.state = XmlTextReaderState::Error;
                    return -1;
                }
                self.state = XmlTextReaderState::Element;
            } else {
                if let Some(my_doc) = (*self.ctxt).my_doc {
                    self.node = my_doc.children.map_or(null_mut(), |c| c.as_ptr());
                }
                if self.node.is_null() {
                    self.node = (*self.ctxt).node_tab[0].as_ptr();
                }
                self.state = XmlTextReaderState::Element;
            }
            self.depth = 0;
            (*self.ctxt).parse_mode = XmlParserMode::XmlParseReader;
            // goto node_found;
            node_found = true;
        } else {
            oldstate = self.state;
            olddepth = (*self.ctxt).node_tab.len();
            oldnode = self.node;
        }

        // get_next_node:
        'get_next_node: loop {
            if !node_found {
                if self.node.is_null() {
                    if self.mode == XmlTextReaderMode::XmlTextreaderModeEof {
                        return 0;
                    } else {
                        return -1;
                    }
                }

                'goto_node_found: {
                    // If we are not backtracking on ancestors or examined nodes,
                    // that the parser didn't finished or that we aren't at the end
                    // of stream, continue processing.
                    while !self.node.is_null()
                        && (*self.node).next.is_none()
                        && (*self.ctxt).node_tab.len() == olddepth
                        && (oldstate == XmlTextReaderState::Backtrack
                            || (*self.node).children().is_none()
                            || (*self.node).element_type() == XmlElementType::XmlEntityRefNode
                            || (*self.node)
                                .children()
                                .filter(|children| {
                                    children.element_type() == XmlElementType::XmlTextNode
                                        && children.next.is_none()
                                })
                                .is_some()
                            || matches!(
                                (*self.node).element_type(),
                                XmlElementType::XmlDTDNode
                                    | XmlElementType::XmlDocumentNode
                                    | XmlElementType::XmlHTMLDocumentNode
                            ))
                        && (*self.ctxt).node.map_or(true, |node| {
                            node.as_ptr() == self.node
                                || node.as_ptr()
                                    == (*self.node).parent().map_or(null_mut(), |p| p.as_ptr())
                        })
                        && !matches!((*self.ctxt).instate, XmlParserInputState::XmlParserEOF)
                    {
                        val = self.push_data();
                        if val < 0 {
                            self.mode = XmlTextReaderMode::XmlTextreaderModeError;
                            self.state = XmlTextReaderState::Error;
                            return -1;
                        }
                        if self.node.is_null() {
                            // goto node_end;
                            self.state = XmlTextReaderState::Done;
                            return 0;
                        }
                    }
                    if let Some(children) = (*self.node).children().filter(|_| {
                        oldstate != XmlTextReaderState::Backtrack
                            && !matches!(
                                (*self.node).element_type(),
                                XmlElementType::XmlEntityRefNode
                                    | XmlElementType::XmlXIncludeStart
                                    | XmlElementType::XmlDTDNode
                            )
                    }) {
                        self.node = children.as_ptr();
                        self.depth += 1;
                        self.state = XmlTextReaderState::Element;
                        break 'goto_node_found;
                    }
                    if let Some(next) = (*self.node).next {
                        #[cfg(not(feature = "xinclude"))]
                        let f = true;
                        #[cfg(feature = "xinclude")]
                        let f = self.in_xinclude <= 0;
                        if oldstate == XmlTextReaderState::Element
                            && (*self.node).element_type() == XmlElementType::XmlElementNode
                            && (*self.node).children().is_none()
                            && (*self.node).extra & NODE_IS_EMPTY as u16 == 0
                            && f
                        {
                            self.state = XmlTextReaderState::End;
                            break 'goto_node_found;
                        }
                        #[cfg(feature = "libxml_regexp")]
                        if self.validate as u32 != 0
                            && (*self.node).element_type() == XmlElementType::XmlElementNode
                        {
                            self.validate_pop();
                        }
                        if self.preserves > 0 && (*self.node).extra & NODE_IS_SPRESERVED as u16 != 0
                        {
                            self.preserves -= 1;
                        }
                        self.node = next.as_ptr();
                        self.state = XmlTextReaderState::Element;

                        // Cleanup of the old node
                        #[cfg(not(feature = "xinclude"))]
                        let f = true;
                        #[cfg(feature = "xinclude")]
                        let f = self.in_xinclude == 0;
                        if self.preserves == 0
                            && f
                            && self.ent_tab.is_empty()
                            && (*self.node).prev.is_some()
                            && (*self.node).prev.unwrap().element_type()
                                != XmlElementType::XmlDTDNode
                        {
                            let tmp: *mut XmlNode = (*self.node).prev.unwrap().as_ptr();
                            if (*tmp).extra & NODE_IS_PRESERVED as u16 == 0 {
                                if oldnode == tmp {
                                    oldnode = null_mut();
                                }
                                (*tmp).unlink();
                                xml_text_reader_free_node(self, tmp);
                            }
                        }

                        break 'goto_node_found;
                    }
                    if oldstate == XmlTextReaderState::Element
                        && (*self.node).element_type() == XmlElementType::XmlElementNode
                        && (*self.node).children().is_none()
                        && (*self.node).extra & NODE_IS_EMPTY as u16 == 0
                    {
                        self.state = XmlTextReaderState::End;
                        break 'goto_node_found;
                    }
                    #[cfg(feature = "libxml_regexp")]
                    if self.validate != XmlTextReaderValidate::NotValidate
                        && (*self.node).element_type() == XmlElementType::XmlElementNode
                    {
                        self.validate_pop();
                    }
                    if self.preserves > 0 && (*self.node).extra & NODE_IS_SPRESERVED as u16 != 0 {
                        self.preserves -= 1;
                    }
                    self.node = (*self.node).parent().map_or(null_mut(), |p| p.as_ptr());
                    if self.node.is_null()
                        || matches!(
                            (*self.node).element_type(),
                            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode
                        )
                    {
                        if self.mode != XmlTextReaderMode::XmlTextreaderModeEof {
                            val = xml_parse_chunk(self.ctxt, c"".as_ptr() as _, 0, 1);
                            self.state = XmlTextReaderState::Done;
                            if val != 0 {
                                return -1;
                            }
                        }
                        self.node = null_mut();
                        self.depth = -1;

                        // Cleanup of the old node
                        #[cfg(not(feature = "xinclude"))]
                        let f = true;
                        #[cfg(feature = "xinclude")]
                        let f = self.in_xinclude == 0;
                        if !oldnode.is_null()
                            && self.preserves == 0
                            && f
                            && self.ent_tab.is_empty()
                            && (*oldnode).element_type() != XmlElementType::XmlDTDNode
                            && (*oldnode).extra & NODE_IS_PRESERVED as u16 == 0
                        {
                            (*oldnode).unlink();
                            xml_text_reader_free_node(self, oldnode);
                        }

                        // goto node_end;
                        self.state = XmlTextReaderState::Done;
                        return 0;
                    }

                    #[cfg(not(feature = "xinclude"))]
                    let f = true;
                    #[cfg(feature = "xinclude")]
                    let f = self.in_xinclude == 0;
                    if self.preserves == 0
                        && f
                        && self.ent_tab.is_empty()
                        && (*self.node).last().is_some()
                        && (*self.node).last().unwrap().extra & NODE_IS_PRESERVED as u16 == 0
                    {
                        let tmp: *mut XmlNode = (*self.node).last().unwrap().as_ptr();
                        (*tmp).unlink();
                        xml_text_reader_free_node(self, tmp);
                    }
                    self.depth -= 1;
                    self.state = XmlTextReaderState::Backtrack;
                }
            }

            // node_found:
            node_found = false;
            // DUMP_READER

            // If we are in the middle of a piece of CDATA make sure it's finished
            if (!self.node.is_null()
                && (*self.node).next.is_none()
                && ((*self.node).element_type() == XmlElementType::XmlTextNode
                    || (*self.node).element_type() == XmlElementType::XmlCDATASectionNode))
                && self.expand().is_null()
            {
                return -1;
            }

            #[cfg(feature = "xinclude")]
            {
                // Handle XInclude if asked for
                if self.xinclude != 0
                    && self.in_xinclude == 0
                    && self.state != XmlTextReaderState::Backtrack
                    && !self.node.is_null()
                    && (*self.node).element_type() == XmlElementType::XmlElementNode
                    && (*self.node).ns.map_or(false, |ns| {
                        xml_str_equal(ns.href, XINCLUDE_NS.as_ptr() as _)
                            || xml_str_equal(ns.href, XINCLUDE_OLD_NS.as_ptr() as _)
                    })
                {
                    if self.xincctxt.is_null() {
                        self.xincctxt = xml_xinclude_new_context((*self.ctxt).my_doc.unwrap());
                        xml_xinclude_set_flags(
                            self.xincctxt,
                            self.parser_flags & !(XmlParserOption::XmlParseNoXIncnode as i32),
                        );
                        xml_xinclude_set_streaming_mode(self.xincctxt, 1);
                    }
                    // expand that node and process it
                    if self.expand().is_null() {
                        return -1;
                    }
                    xml_xinclude_process_node(self.xincctxt, self.node);
                }
                if !self.node.is_null()
                    && (*self.node).element_type() == XmlElementType::XmlXIncludeStart
                {
                    self.in_xinclude += 1;
                    // goto get_next_node;
                    continue 'get_next_node;
                }
                if !self.node.is_null()
                    && (*self.node).element_type() == XmlElementType::XmlXIncludeEnd
                {
                    self.in_xinclude -= 1;
                    // goto get_next_node;
                    continue 'get_next_node;
                }
            }
            // Handle entities enter and exit when in entity replacement mode
            if !self.node.is_null()
                && (*self.node).element_type() == XmlElementType::XmlEntityRefNode
                && !self.ctxt.is_null()
                && (*self.ctxt).replace_entities == 1
            {
                if let Some(children) = (*self.node).children().filter(|children| {
                    children.element_type() == XmlElementType::XmlEntityDecl
                        && children.children().is_some()
                }) {
                    if self.entity_push(self.node) < 0 {
                        // goto get_next_node;
                        continue 'get_next_node;
                    }
                    self.node = children.children().map_or(null_mut(), |c| c.as_ptr());
                }
            } else {
                #[cfg(feature = "libxml_regexp")]
                if !self.node.is_null()
                    && (*self.node).element_type() == XmlElementType::XmlEntityRefNode
                    && !self.ctxt.is_null()
                    && self.validate as i32 != 0
                {
                    self.validate_entity();
                }
            }
            if !self.node.is_null()
                && (*self.node).element_type() == XmlElementType::XmlEntityDecl
                && !self.ent.is_null()
                && (*self.ent).children() == NodePtr::from_ptr(self.node)
            {
                self.node = self.entity_pop();
                self.depth += 1;
                // goto get_next_node;
                continue 'get_next_node;
            }

            break;
        }
        #[cfg(feature = "libxml_regexp")]
        if self.validate != XmlTextReaderValidate::NotValidate && !self.node.is_null() {
            let node: *mut XmlNode = self.node;

            if (*node).element_type() == XmlElementType::XmlElementNode
                && !matches!(
                    self.state,
                    XmlTextReaderState::End | XmlTextReaderState::Backtrack
                )
            {
                self.validate_push();
            } else if matches!(
                (*node).element_type(),
                XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
            ) {
                self.validate_cdata((*node).content, xml_strlen((*node).content));
            }
        }
        #[cfg(feature = "libxml_pattern")]
        if !self.pattern_tab.is_empty()
            && !matches!(
                self.state,
                XmlTextReaderState::End | XmlTextReaderState::Backtrack
            )
        {
            for &pattern in &self.pattern_tab {
                if xml_pattern_match(pattern, self.node) == 1 {
                    self.preserve();
                    break;
                }
            }
        }
        #[cfg(feature = "schema")]
        if self.validate == XmlTextReaderValidate::ValidateXsd
            && self.xsd_valid_errors == 0
            && !self.xsd_valid_ctxt.is_null()
        {
            self.xsd_valid_errors = (xml_schema_is_valid(self.xsd_valid_ctxt) == 0) as i32;
        }
        1
        // node_end:
        //     (*reader).state = xmlTextReaderState::XML_TEXTREADER_DONE;
        //     return 0;
    }

    /// Reads the contents of the current node, including child nodes and markup.
    ///
    /// Returns a string containing the XML content, or NULL if the current node
    /// is neither an element nor attribute, or has no child nodes. The
    /// string must be deallocated by the caller.
    #[doc(alias = "xmlTextReaderReadInnerXml")]
    #[cfg(all(feature = "libxml_reader", feature = "libxml_writer"))]
    pub unsafe fn read_inner_xml(&mut self) -> *mut XmlChar {
        use crate::{
            libxml::xmlstring::xml_strndup,
            tree::{NodeCommon, XmlGenericNodePtr},
        };

        let mut node: *mut XmlNode;
        let mut cur_node: *mut XmlNode;

        if self.expand().is_null() {
            return null_mut();
        }
        let doc = (*self.node).doc;
        let mut buff = vec![];
        cur_node = (*self.node).children().map_or(null_mut(), |c| c.as_ptr());
        while !cur_node.is_null() {
            // XXX: Why is the node copied?
            node = xml_doc_copy_node(XmlGenericNodePtr::from_raw(cur_node).unwrap(), doc, 1);
            // XXX: Why do we need a second buffer?
            let mut buff2 = vec![];
            if (*node).dump_memory(&mut buff2, doc, 0, 0) == 0 {
                xml_free_node(node);
                return null_mut();
            }
            buff.extend(buff2);
            xml_free_node(node);
            cur_node = (*cur_node).next.map_or(null_mut(), |n| n.as_ptr());
        }
        xml_strndup(buff.as_ptr(), buff.len() as i32)
    }

    /// Reads the contents of the current node, including child nodes and markup.
    ///
    /// Returns a string containing the node and any XML content, or NULL if the
    /// current node cannot be serialized. The string must be deallocated by the caller.
    #[doc(alias = "xmlTextReaderReadOuterXml")]
    #[cfg(all(feature = "libxml_reader", feature = "libxml_writer"))]
    pub unsafe fn read_outer_xml(&mut self) -> *mut XmlChar {
        use crate::{
            libxml::xmlstring::xml_strndup,
            tree::{NodeCommon, XmlDtd, XmlDtdPtr, XmlGenericNodePtr},
        };

        let mut node: *mut XmlNode;

        if self.expand().is_null() {
            return null_mut();
        }
        node = self.node;
        let doc = (*node).doc;
        // XXX: Why is the node copied?
        if (*node).element_type() == XmlElementType::XmlDTDNode {
            node = xml_copy_dtd(XmlDtdPtr::from_raw(node as *mut XmlDtd).unwrap().unwrap())
                .map_or(null_mut(), |p| p.as_ptr()) as *mut XmlNode;
        } else {
            node = xml_doc_copy_node(XmlGenericNodePtr::from_raw(node).unwrap(), doc, 1);
        }
        let mut buff = vec![];
        if (*node).dump_memory(&mut buff, doc, 0, 0) == 0 {
            xml_free_node(node);
            return null_mut();
        }
        xml_free_node(node);
        xml_strndup(buff.as_ptr(), buff.len() as i32)
    }

    /// Moves the position of the current instance to the next node in
    /// the stream, exposing its properties.
    ///
    /// Returns 1 if the node was read successfully, 0 if there is no more nodes to read,
    /// or -1 in case of error
    #[doc(alias = "xmlTextReaderReadTree")]
    #[cfg(feature = "libxml_reader")]
    unsafe fn read_tree(&mut self) -> i32 {
        use crate::tree::NodeCommon;

        if self.state == XmlTextReaderState::End {
            return 0;
        }

        // next_node:
        'next_node: loop {
            if self.node.is_null() {
                let Some(children) = self.doc.unwrap().children else {
                    self.state = XmlTextReaderState::End;
                    return 0;
                };

                self.node = children.as_ptr();
                self.state = XmlTextReaderState::Start;
                // goto found_node;
            } else {
                if self.state != XmlTextReaderState::Backtrack
                    && !matches!(
                        (*self.node).element_type(),
                        XmlElementType::XmlDTDNode
                            | XmlElementType::XmlXIncludeStart
                            | XmlElementType::XmlEntityRefNode
                    )
                {
                    if let Some(children) = (*self.node).children() {
                        self.node = children.as_ptr();
                        self.depth += 1;
                        self.state = XmlTextReaderState::Start;
                        // goto found_node;
                        if matches!(
                            (*self.node).element_type(),
                            XmlElementType::XmlXIncludeStart | XmlElementType::XmlXIncludeEnd
                        ) {
                            // goto next_node;
                            continue 'next_node;
                        }
                        break;
                    }

                    if (*self.node).element_type() == XmlElementType::XmlAttributeNode {
                        self.state = XmlTextReaderState::Backtrack;
                        // goto found_node;
                        if matches!(
                            (*self.node).element_type(),
                            XmlElementType::XmlXIncludeStart | XmlElementType::XmlXIncludeEnd
                        ) {
                            // goto next_node;
                            continue 'next_node;
                        }
                        break;
                    }
                }

                if let Some(next) = (*self.node).next {
                    self.node = next.as_ptr();
                    self.state = XmlTextReaderState::Start;
                    // goto found_node;
                } else if let Some(parent) = (*self.node).parent() {
                    if matches!(
                        parent.element_type(),
                        XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode
                    ) {
                        self.state = XmlTextReaderState::End;
                        return 0;
                    }

                    self.node = parent.as_ptr();
                    self.depth -= 1;
                    self.state = XmlTextReaderState::Backtrack;
                    // goto found_node;
                } else {
                    self.state = XmlTextReaderState::End;
                }
            }

            // found_node:
            if matches!(
                (*self.node).element_type(),
                XmlElementType::XmlXIncludeStart | XmlElementType::XmlXIncludeEnd
            ) {
                // goto next_node;
                continue 'next_node;
            }

            break;
        }
        1
    }

    /// Reads the contents of an element or a text node as a string.
    ///
    /// Returns a string containing the contents of the Element or Text node,
    /// or NULL if the reader is positioned on any other type of node.
    /// The string must be deallocated by the caller.
    #[doc(alias = "xmlTextReaderReadString")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn read_string(&mut self) -> *mut XmlChar {
        use crate::tree::NodeCommon;

        if self.node.is_null() {
            return null_mut();
        }

        let node: *mut XmlNode = if !self.curnode.is_null() {
            self.curnode
        } else {
            self.node
        };
        match (*node).element_type() {
            XmlElementType::XmlTextNode => {
                if !(*node).content.is_null() {
                    return xml_strdup((*node).content);
                }
            }
            XmlElementType::XmlElementNode => {
                if self.do_expand() != -1 {
                    return xml_text_reader_collect_siblings(
                        (*node).children().map_or(null_mut(), |c| c.as_ptr()),
                    );
                }
            }
            XmlElementType::XmlAttributeNode => {
                // TODO
                todo!()
            }
            _ => {}
        }
        null_mut()
    }

    /// Parses an attribute value into one or more Text and EntityReference nodes.
    ///
    /// Returns 1 in case of success, 0 if the reader was not positioned on an
    /// attribute node or all the attribute values have been read, or -1 in case of error.
    #[doc(alias = "xmlTextReaderReadAttributeValue")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn read_attribute_value(&mut self) -> i32 {
        use crate::tree::{NodeCommon, XmlNs, XmlNsPtr};

        if self.node.is_null() {
            return -1;
        }
        if self.curnode.is_null() {
            return 0;
        }
        if (*self.curnode).element_type() == XmlElementType::XmlAttributeNode {
            let Some(children) = (*self.curnode).children() else {
                return 0;
            };
            self.curnode = children.as_ptr();
        } else if (*self.curnode).element_type() == XmlElementType::XmlNamespaceDecl {
            let ns = XmlNsPtr::from_raw(self.curnode as *mut XmlNs)
                .unwrap()
                .unwrap();

            if self.faketext.is_null() {
                self.faketext = xml_new_doc_text((*self.node).doc, ns.href)
                    .map_or(null_mut(), |node| node.as_ptr());
            } else {
                if !(*self.faketext).content.is_null() {
                    xml_free((*self.faketext).content as _);
                }
                (*self.faketext).content = xml_strdup(ns.href);
            }
            self.curnode = self.faketext;
        } else {
            let Some(next) = (*self.curnode).next else {
                return 0;
            };
            self.curnode = next.as_ptr();
        }
        1
    }

    /// Makes sure that the current node is fully read as well as all its descendant.  
    /// It means the full DOM subtree must be available at the end of the call.
    ///
    /// Returns 1 if the node was expanded successfully, 0 if there is no more nodes to read,
    /// or -1 in case of error
    #[doc(alias = "xmlTextReaderDoExpand")]
    #[cfg(feature = "libxml_reader")]
    unsafe fn do_expand(&mut self) -> i32 {
        let mut val: i32;

        if self.node.is_null() || self.ctxt.is_null() {
            return -1;
        }
        while {
            if matches!((*self.ctxt).instate, XmlParserInputState::XmlParserEOF) {
                return 1;
            }

            if !xml_text_reader_get_successor(self.node).is_null() {
                return 1;
            }
            if ((*self.ctxt).node_tab.len() as i32) < self.depth {
                return 1;
            }
            if self.mode == XmlTextReaderMode::XmlTextreaderModeEof {
                return 1;
            }
            val = self.push_data();
            if val < 0 {
                self.mode = XmlTextReaderMode::XmlTextreaderModeError;
                return -1;
            }

            self.mode != XmlTextReaderMode::XmlTextreaderModeEof
        } {}
        1
    }

    /// Reads the contents of the current node and the full subtree. It then makes
    /// the subtree available until the next xmlTextReaderRead() call
    ///
    /// Returns a node pointer valid until the next xmlTextReaderRead() call or NULL in case of error.
    #[doc(alias = "xmlTextReaderExpand")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn expand(&mut self) -> *mut XmlNode {
        if self.node.is_null() {
            return null_mut();
        }
        if self.doc.is_some() {
            return self.node;
        }
        if self.ctxt.is_null() {
            return null_mut();
        }
        if self.do_expand() < 0 {
            return null_mut();
        }
        self.node
    }

    /// Push data down the progressive parser until a significant callback got raised.
    ///
    /// Returns -1 in case of failure, 0 otherwise
    #[doc(alias = "xmlTextReaderPushData")]
    #[cfg(feature = "libxml_reader")]
    unsafe fn push_data(&mut self) -> i32 {
        let mut val: i32;
        let mut s: i32;

        if self.input.is_none() || self.input.as_ref().unwrap().buffer.is_none() {
            return -1;
        }

        let oldstate: XmlTextReaderState = self.state;
        self.state = XmlTextReaderState::None;
        let mut inbuf = self.input.as_ref().unwrap().buffer.unwrap();

        while self.state == XmlTextReaderState::None {
            if inbuf.len() < self.cur as usize + CHUNK_SIZE {
                // Refill the buffer unless we are at the end of the stream
                if self.mode != XmlTextReaderMode::XmlTextreaderModeEof {
                    val = self.input.as_mut().unwrap().read(4096);
                    if val == 0 && self.input.as_ref().unwrap().context.is_none() {
                        if inbuf.len() == self.cur as _ {
                            self.mode = XmlTextReaderMode::XmlTextreaderModeEof;
                            self.state = oldstate;
                        }
                    } else if val < 0 {
                        self.mode = XmlTextReaderMode::XmlTextreaderModeEof;
                        self.state = oldstate;
                        if oldstate != XmlTextReaderState::Start || (*self.ctxt).my_doc.is_some() {
                            return val;
                        }
                    } else if val == 0 {
                        // mark the end of the stream and process the remains
                        self.mode = XmlTextReaderMode::XmlTextreaderModeEof;
                        break;
                    }
                } else {
                    break;
                }
            }
            // parse by block of CHUNK_SIZE bytes, various tests show that
            // it's the best tradeoff at least on a 1.2GH Duron
            if inbuf.len() >= self.cur as usize + CHUNK_SIZE {
                val = xml_parse_chunk(
                    self.ctxt,
                    inbuf.as_ref().as_ptr().add(self.cur as usize) as _,
                    CHUNK_SIZE as _,
                    0,
                );
                self.cur += CHUNK_SIZE as u32;
                if val != 0 {
                    (*self.ctxt).well_formed = 0;
                }
                if (*self.ctxt).well_formed == 0 {
                    break;
                }
            } else {
                s = inbuf.len() as i32 - self.cur as i32;
                val = xml_parse_chunk(
                    self.ctxt,
                    inbuf.as_ref().as_ptr().add(self.cur as usize) as _,
                    s,
                    0,
                );
                self.cur += s as u32;
                if val != 0 {
                    (*self.ctxt).well_formed = 0;
                }
                break;
            }
        }

        // Discard the consumed input when needed and possible
        if self.mode == XmlTextReaderMode::XmlTextreaderModeInteractive {
            if self.input.as_ref().unwrap().context.is_some()
                && (self.cur >= 4096 && inbuf.len() - self.cur as usize <= CHUNK_SIZE)
            {
                val = inbuf.trim_head(self.cur as _) as _;
                if val >= 0 {
                    self.cur -= val as u32;
                }
            }
        }
        // At the end of the stream signal that the work is done to the Push parser.
        else if self.mode == XmlTextReaderMode::XmlTextreaderModeEof
            && self.state != XmlTextReaderState::Done
        {
            s = (inbuf.len() - self.cur as usize) as i32;
            val = xml_parse_chunk(
                self.ctxt,
                inbuf.as_ref().as_ptr().add(self.cur as usize) as _,
                s,
                1,
            );
            self.cur = inbuf.len() as _;
            self.state = XmlTextReaderState::Done;
            if val != 0 {
                if (*self.ctxt).well_formed != 0 {
                    (*self.ctxt).well_formed = 0;
                } else {
                    return -1;
                }
            }
        }
        self.state = oldstate;
        if (*self.ctxt).well_formed == 0 {
            self.mode = XmlTextReaderMode::XmlTextreaderModeEof;
            return -1;
        }

        0
    }

    /// Pushes a new entity reference node on top of the entities stack
    ///
    /// Returns -1 in case of error, the index in the stack otherwise
    #[doc(alias = "xmlTextReaderEntPush")]
    #[cfg(feature = "libxml_reader")]
    fn entity_push(&mut self, value: *mut XmlNode) -> i32 {
        self.ent_tab.push(value);
        self.ent = value;
        self.ent_tab.len() as i32 - 1
    }

    /// Pops the top element entity from the entities stack
    ///
    /// Returns the entity just removed
    #[doc(alias = "xmlTextReaderEntPop")]
    #[cfg(feature = "libxml_reader")]
    fn entity_pop(&mut self) -> *mut XmlNode {
        let Some(res) = self.ent_tab.pop() else {
            return null_mut();
        };
        self.ent = self.ent_tab.last().copied().unwrap_or(null_mut());
        res
    }

    /// Push the current node for validation
    #[doc(alias = "xmlTextReaderValidatePush")]
    #[cfg(all(feature = "libxml_reader", feature = "libxml_regexp"))]
    unsafe fn validate_push(&mut self) {
        use crate::tree::XmlNodePtr;

        let mut node: *mut XmlNode = self.node;

        #[cfg(feature = "libxml_valid")]
        if self.validate == XmlTextReaderValidate::ValidateDtd
            && !self.ctxt.is_null()
            && (*self.ctxt).validate == 1
        {
            if let Some(prefix) = (*node).ns.map(|ns| ns.prefix).filter(|p| !p.is_null()) {
                // TODO use the BuildQName interface
                let mut qname = xml_strdup(prefix);
                qname = xml_strcat(qname, c":".as_ptr() as _);
                qname = xml_strcat(qname, (*node).name);
                (*self.ctxt).valid &= xml_validate_push_element(
                    addr_of_mut!((*self.ctxt).vctxt),
                    (*self.ctxt).my_doc.unwrap(),
                    XmlNodePtr::from_raw(node).unwrap().unwrap(),
                    qname,
                );
                if !qname.is_null() {
                    xml_free(qname as _);
                }
            } else {
                (*self.ctxt).valid &= xml_validate_push_element(
                    addr_of_mut!((*self.ctxt).vctxt),
                    (*self.ctxt).my_doc.unwrap(),
                    XmlNodePtr::from_raw(node).unwrap().unwrap(),
                    (*node).name,
                );
            }
        }
        #[cfg(feature = "schema")]
        if self.validate == XmlTextReaderValidate::ValidateRng && !self.rng_valid_ctxt.is_null() {
            let mut ret: i32;

            if !self.rng_full_node.is_null() {
                return;
            }
            ret = (*self.rng_valid_ctxt).push_element((*self.ctxt).my_doc, node);
            if ret == 0 {
                // this element requires a full tree
                node = self.expand();
                if node.is_null() {
                    ret = -1;
                } else {
                    ret = xml_relaxng_validate_full_element(
                        self.rng_valid_ctxt,
                        (*self.ctxt).my_doc,
                        XmlNodePtr::from_raw(node).unwrap().unwrap(),
                    );
                    self.rng_full_node = node;
                }
            }
            if ret != 1 {
                self.rng_valid_errors += 1;
            }
        }
    }

    /// Pop the current node from validation
    #[doc(alias = "xmlTextReaderValidatePop")]
    #[cfg(feature = "libxml_reader")]
    unsafe fn validate_pop(&mut self) {
        use crate::tree::XmlNodePtr;

        let node: *mut XmlNode = self.node;

        #[cfg(feature = "libxml_valid")]
        if self.validate == XmlTextReaderValidate::ValidateDtd
            && !self.ctxt.is_null()
            && (*self.ctxt).validate == 1
        {
            if let Some(prefix) = (*node).ns.map(|ns| ns.prefix).filter(|p| !p.is_null()) {
                // TODO use the BuildQName interface

                let mut qname = xml_strdup(prefix);
                qname = xml_strcat(qname, c":".as_ptr() as _);
                qname = xml_strcat(qname, (*node).name);
                (*self.ctxt).valid &= xml_validate_pop_element(
                    addr_of_mut!((*self.ctxt).vctxt),
                    (*self.ctxt).my_doc,
                    XmlNodePtr::from_raw(node).unwrap(),
                    qname,
                );
                if !qname.is_null() {
                    xml_free(qname as _);
                }
            } else {
                (*self.ctxt).valid &= xml_validate_pop_element(
                    addr_of_mut!((*self.ctxt).vctxt),
                    (*self.ctxt).my_doc,
                    XmlNodePtr::from_raw(node).unwrap(),
                    (*node).name,
                );
            }
        }
        #[cfg(feature = "schema")]
        if self.validate == XmlTextReaderValidate::ValidateRng && !self.rng_valid_ctxt.is_null() {
            if !self.rng_full_node.is_null() {
                if node == self.rng_full_node {
                    self.rng_full_node = null_mut();
                }
                return;
            }
            let ret: i32 = (*self.rng_valid_ctxt).pop_element((*self.ctxt).my_doc, node);
            if ret != 1 {
                self.rng_valid_errors += 1;
            }
        }
    }

    /// Handle the validation when an entity reference is encountered and
    /// entity substitution is not activated. As a result the parser interface
    /// must walk through the entity and do the validation calls
    #[doc(alias = "xmlTextReaderValidateEntity")]
    #[cfg(all(feature = "libxml_reader", feature = "libxml_regexp"))]
    unsafe fn validate_entity(&mut self) {
        use crate::tree::{NodeCommon, NodePtr};

        let oldnode: *mut XmlNode = self.node;
        let mut node: *mut XmlNode = self.node;

        'main: while {
            'inner: {
                'skip_children: {
                    if (*node).element_type() == XmlElementType::XmlEntityRefNode {
                        if let Some(children) = (*node).children().filter(|children| {
                            children.element_type() == XmlElementType::XmlEntityDecl
                                && children.children().is_some()
                        }) {
                            if self.entity_push(node) < 0 {
                                if node == oldnode {
                                    // break;
                                    break 'main;
                                }
                                break 'skip_children;
                            }
                            node = children.children().map_or(null_mut(), |c| c.as_ptr());
                            // continue;
                            break 'inner;
                        } else {
                            // The error has probably been raised already.
                            if node == oldnode {
                                break 'main;
                            }
                            break 'skip_children;
                        }
                    } else {
                        #[cfg(feature = "libxml_regexp")]
                        if (*node).element_type() == XmlElementType::XmlElementNode {
                            self.node = node;
                            self.validate_push();
                        } else if matches!(
                            (*node).element_type(),
                            XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
                        ) {
                            self.validate_cdata((*node).content, xml_strlen((*node).content));
                        }
                    }
                    // go to next node
                    if let Some(children) = (*node).children() {
                        node = children.as_ptr();
                        // continue;
                        break 'inner;
                    } else if (*node).element_type() == XmlElementType::XmlElementNode {
                        self.validate_pop();
                    }
                }

                // skip_children:
                if let Some(next) = (*node).next() {
                    node = next.as_ptr();
                    // continue;
                    break 'inner;
                }

                loop {
                    node = (*node).parent().map_or(null_mut(), |p| p.as_ptr());
                    if (*node).element_type() == XmlElementType::XmlElementNode {
                        let mut tmp: *mut XmlNode;
                        if self.ent_tab.is_empty() {
                            while {
                                tmp = (*node).last().map_or(null_mut(), |l| l.as_ptr());
                                !tmp.is_null()
                            } {
                                if (*tmp).extra & NODE_IS_PRESERVED as u16 == 0 {
                                    (*tmp).unlink();
                                    xml_text_reader_free_node(self, tmp);
                                } else {
                                    break;
                                }
                            }
                        }
                        self.node = node;
                        self.validate_pop();
                    }
                    if (*node).element_type() == XmlElementType::XmlEntityDecl
                        && !self.ent.is_null()
                        && (*self.ent).children() == NodePtr::from_ptr(node)
                    {
                        node = self.entity_pop();
                    }
                    if node == oldnode {
                        break;
                    }
                    if let Some(next) = (*node).next {
                        node = next.as_ptr();
                        break;
                    }

                    if node.is_null() || node == oldnode {
                        break;
                    }
                }
            }
            !node.is_null() && node != oldnode
        } {}
        self.node = oldnode;
    }

    /// Push some CData for validation
    #[doc(alias = "xmlTextReaderValidateCData")]
    #[cfg(all(feature = "libxml_reader", feature = "libxml_regexp"))]
    unsafe fn validate_cdata(&mut self, data: *const XmlChar, len: i32) {
        #[cfg(feature = "libxml_valid")]
        if self.validate == XmlTextReaderValidate::ValidateDtd
            && !self.ctxt.is_null()
            && (*self.ctxt).validate == 1
        {
            (*self.ctxt).valid &=
                xml_validate_push_cdata(addr_of_mut!((*self.ctxt).vctxt), data, len);
        }
        #[cfg(feature = "schema")]
        if self.validate == XmlTextReaderValidate::ValidateRng && !self.rng_valid_ctxt.is_null() {
            if !self.rng_full_node.is_null() {
                return;
            }
            let ret: i32 = xml_relaxng_validate_push_cdata(self.rng_valid_ctxt, data, len);
            if ret != 1 {
                self.rng_valid_errors += 1;
            }
        }
    }

    /// Retrieve the validity status from the parser context
    ///
    /// Returns the flag value `Some(true)` if valid, `Some(false)` if no, and `None` in case of error
    #[doc(alias = "xmlTextReaderIsValid")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn is_valid(&self) -> Option<bool> {
        #[cfg(feature = "schema")]
        {
            if self.validate == XmlTextReaderValidate::ValidateRng {
                return Some(self.rng_valid_errors == 0);
            }
            if self.validate == XmlTextReaderValidate::ValidateXsd {
                return Some(self.xsd_valid_errors == 0);
            }
        }
        if !self.ctxt.is_null() && (*self.ctxt).validate == 1 {
            return if (*self.ctxt).valid < 0 {
                None
            } else {
                Some((*self.ctxt).valid != 0)
            };
        }
        Some(false)
    }

    /// Whether an Attribute  node was generated from the default value defined in the DTD or schema.
    ///
    /// Returns `false` if not defaulted, `true` if defaulted
    #[doc(alias = "xmlTextReaderIsDefault")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn is_default(&self) -> bool {
        false
    }

    /// Check if the current node is empty
    ///
    /// Returns `Some(true)` if empty, `Some(false)` if not and `None` in case of error
    #[doc(alias = "xmlTextReaderIsEmptyElement")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn is_empty_element(&self) -> Option<bool> {
        use crate::tree::NodeCommon;

        if self.node.is_null() {
            return None;
        }
        if (*self.node).element_type() != XmlElementType::XmlElementNode {
            return Some(false);
        }
        if !self.curnode.is_null() {
            return Some(false);
        }
        if (*self.node).children().is_some() {
            return Some(false);
        }
        if self.state == XmlTextReaderState::End {
            return Some(false);
        }
        if self.doc.is_some() {
            return Some(true);
        }
        #[cfg(feature = "xinclude")]
        if self.in_xinclude > 0 {
            return Some(true);
        }
        Some((*self.node).extra & NODE_IS_EMPTY as u16 != 0)
    }

    /// Determine whether the current node is a namespace declaration
    /// rather than a regular attribute.
    ///
    /// Returns `Some(true)` if the current node is a namespace declaration,
    /// `Some(false)` if it is a regular attribute or other type of node,
    /// or `None` in case of error.
    #[doc(alias = "xmlTextReaderIsNamespaceDecl")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn is_namespace_decl(&self) -> Option<bool> {
        use crate::tree::NodeCommon;

        if self.node.is_null() {
            return None;
        }
        let node = if !self.curnode.is_null() {
            self.curnode
        } else {
            self.node
        };

        Some(XmlElementType::XmlNamespaceDecl == (*node).element_type())
    }

    /// Whether the node has attributes.
    ///
    /// Returns `true` if true, `false` if false.
    #[doc(alias = "xmlTextReaderHasAttributes")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn has_attributes(&self) -> bool {
        use crate::tree::NodeCommon;

        if self.node.is_null() {
            return false;
        }
        let node = if !self.curnode.is_null() {
            self.curnode
        } else {
            self.node
        };

        if (*node).element_type() == XmlElementType::XmlElementNode
            && ((*node).properties.is_some() || (*node).ns_def.is_some())
        {
            return true;
        }
        // TODO: handle the xmlDecl
        false
    }

    /// Whether the node can have a text value.
    #[doc(alias = "xmlTextReaderHasValue")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn has_value(&self) -> bool {
        use crate::tree::NodeCommon;

        if self.node.is_null() {
            return false;
        }
        let node = if !self.curnode.is_null() {
            self.curnode
        } else {
            self.node
        };

        matches!(
            (*node).element_type(),
            XmlElementType::XmlAttributeNode
                | XmlElementType::XmlTextNode
                | XmlElementType::XmlCDATASectionNode
                | XmlElementType::XmlPINode
                | XmlElementType::XmlCommentNode
                | XmlElementType::XmlNamespaceDecl
        )
    }

    /// Provides the value of the attribute with the specified qualified name.
    ///
    /// Returns a string containing the value of the specified attribute, or `None` in case of error.  
    #[doc(alias = "xmlTextReaderGetAttribute")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn get_attribute(&mut self, name: &str) -> Option<String> {
        use std::ffi::CStr;

        use crate::{
            parser::split_qname2,
            tree::{NodeCommon, XmlNsPtr},
        };

        if self.node.is_null() {
            return None;
        }
        if !self.curnode.is_null() {
            return None;
        }

        // TODO: handle the xmlDecl
        if (*self.node).element_type() != XmlElementType::XmlElementNode {
            return None;
        }

        let Some((prefix, localname)) = split_qname2(name) else {
            // Namespace default decl
            if name == "xmlns" {
                let mut ns = (*self.node).ns_def;
                while let Some(now) = ns {
                    if now.prefix().is_none() {
                        return Some(
                            CStr::from_ptr(now.href as *const i8)
                                .to_string_lossy()
                                .into_owned(),
                        );
                    }
                    ns = XmlNsPtr::from_raw(now.next).unwrap();
                }
                return None;
            }
            return (*self.node).get_no_ns_prop(name);
        };

        // Namespace default decl
        let mut ret = None;
        if prefix == "xmlns" {
            let mut ns = (*self.node).ns_def;
            while let Some(now) = ns {
                if now.prefix().as_deref() == Some(localname) {
                    ret = Some(
                        CStr::from_ptr(now.href as *const i8)
                            .to_string_lossy()
                            .into_owned(),
                    );
                    break;
                }
                ns = XmlNsPtr::from_raw(now.next).unwrap();
            }
        } else if let Some(ns) = (*self.node).search_ns((*self.node).doc, Some(prefix)) {
            let href = ns.href;
            ret = (*self.node).get_ns_prop(
                localname,
                (!href.is_null())
                    .then(|| CStr::from_ptr(href as *const i8).to_string_lossy())
                    .as_deref(),
            );
        }
        ret
    }

    /// Provides the value of the specified attribute
    ///
    /// Returns a string containing the value of the specified attribute, or NULL in case of error.  
    /// The string must be deallocated by the caller.
    #[doc(alias = "xmlTextReaderGetAttributeNs")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn get_attribute_ns(
        &mut self,
        local_name: &str,
        namespace_uri: Option<&str>,
    ) -> Option<String> {
        use std::ffi::CStr;

        use crate::tree::{NodeCommon, XmlNsPtr};

        if self.node.is_null() {
            return None;
        }
        if !self.curnode.is_null() {
            return None;
        }

        // TODO: handle the xmlDecl
        if (*self.node).element_type() != XmlElementType::XmlElementNode {
            return None;
        }

        if namespace_uri == Some("http://www.w3.org/2000/xmlns/") {
            let prefix = (local_name != "xmlns").then_some(local_name);
            let mut ns = (*self.node).ns_def;
            while let Some(now) = ns {
                if (prefix.is_none() && now.prefix().is_none())
                    || now.prefix().as_deref() == Some(local_name)
                {
                    return Some(
                        CStr::from_ptr(now.href as *const i8)
                            .to_string_lossy()
                            .into_owned(),
                    );
                }
                ns = XmlNsPtr::from_raw(now.next).unwrap();
            }
            return None;
        }

        (*self.node).get_ns_prop(local_name, namespace_uri)
    }

    /// Provides the value of the attribute with the specified index relative
    /// to the containing element.
    ///
    /// Returns a string containing the value of the specified attribute, or `None` in case of error.  
    #[doc(alias = "xmlTextReaderGetAttributeNo")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn get_attribute_no(&mut self, no: i32) -> Option<String> {
        use std::ffi::CStr;

        use crate::tree::{NodeCommon, XmlNsPtr};

        if self.node.is_null() {
            return None;
        }
        if !self.curnode.is_null() {
            return None;
        }
        // TODO: handle the xmlDecl
        if (*self.node).element_type() != XmlElementType::XmlElementNode {
            return None;
        }

        let mut ns = (*self.node).ns_def;
        let mut i = 0;
        while let Some(now) = ns.filter(|_| i < no) {
            ns = XmlNsPtr::from_raw(now.next).unwrap();
            i += 1;
        }

        if let Some(ns) = ns {
            return Some(
                CStr::from_ptr(ns.href as *const i8)
                    .to_string_lossy()
                    .into_owned(),
            );
        }
        let mut cur = (*self.node).properties?;
        for _ in i..no {
            cur = cur.next?;
        }
        // TODO walk the DTD if present

        cur.children
            .and_then(|c| c.get_string((*self.node).doc, 1))
            .or_else(|| Some("".to_owned()))
    }

    /// Read the parser internal property.
    ///
    /// Returns the value, usually 0 or 1, or -1 in case of error.
    #[doc(alias = "xmlTextReaderGetParserProp")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn get_parser_prop(&mut self, prop: XmlParserProperties) -> i32 {
        if self.ctxt.is_null() {
            return -1;
        }
        let ctxt = self.ctxt;

        match prop {
            XmlParserProperties::XmlParserLoadDTD => {
                if (*ctxt).loadsubset != 0 || (*ctxt).validate != 0 {
                    return 1;
                }
                0
            }
            XmlParserProperties::XmlParserDefaultAttrs => {
                if (*ctxt).loadsubset & XML_COMPLETE_ATTRS as i32 != 0 {
                    return 1;
                }
                0
            }
            XmlParserProperties::XmlParserValidate => self.validate as i32,
            XmlParserProperties::XmlParserSubstEntities => (*ctxt).replace_entities,
        }
    }

    /// Provide the line number of the current parsing point.
    ///
    /// Returns an int or 0 if not available
    #[doc(alias = "xmlTextReaderGetParserLineNumber")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn get_parser_line_number(&mut self) -> i32 {
        if self.ctxt.is_null() || (*self.ctxt).input.is_null() {
            return 0;
        }
        (*(*self.ctxt).input).line
    }

    /// Provide the column number of the current parsing point.
    ///
    /// Returns an int or 0 if not available
    #[doc(alias = "xmlTextReaderGetParserColumnNumber")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn get_parser_column_number(&mut self) -> i32 {
        if self.ctxt.is_null() || (*self.ctxt).input.is_null() {
            return 0;
        }
        (*(*self.ctxt).input).col
    }

    /// Method to get the remainder of the buffered XML. this method stops the
    /// parser, set its state to End Of File and return the input stream with
    /// what is left that the parser did not use.
    ///
    /// The implementation is not good, the parser certainly progressed past
    /// what's left in (*reader).input, and there is an allocation problem. Best
    /// would be to rewrite it differently.
    ///
    /// Returns the xmlParserInputBufferPtr attached to the XML or NULL in case of error.
    #[doc(alias = "xmlTextReaderGetRemainder")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn get_remainder(&mut self) -> Option<XmlParserInputBuffer> {
        if self.node.is_null() {
            return None;
        }

        self.node = null_mut();
        self.curnode = null_mut();
        self.mode = XmlTextReaderMode::XmlTextreaderModeEof;
        if !self.ctxt.is_null() {
            (*self.ctxt).stop();
            if let Some(my_doc) = (*self.ctxt).my_doc.take() {
                if self.preserve == 0 {
                    xml_text_reader_free_doc(self, my_doc);
                }
            }
        }
        if self.allocs & XML_TEXTREADER_INPUT != 0 {
            self.allocs -= XML_TEXTREADER_INPUT;
            self.input.take()
        } else {
            // Hum, one may need to duplicate the data structure because
            // without reference counting the input may be freed twice:
            //   - by the layer which allocated it.
            //   - by the layer to which would have been returned to.
            // TODO
            None
        }
    }

    /// Retrieve the error callback function and user argument.
    #[doc(alias = "xmlTextReaderGetErrorHandler")]
    #[cfg(feature = "libxml_reader")]
    pub fn get_error_handler(
        &self,
        f: &mut Option<XmlTextReaderErrorFunc>,
        arg: &mut Option<GenericErrorContext>,
    ) {
        *f = self.error_func;
        *arg = self.error_func_arg.clone();
    }

    /// Moves the position of the current instance to the node that
    /// contains the current Attribute node.
    ///
    /// Returns 1 in case of success, -1 in case of error, 0 if not moved
    #[doc(alias = "xmlTextReaderMoveToElement")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn move_to_element(&mut self) -> i32 {
        use crate::tree::NodeCommon;

        if self.node.is_null() {
            return -1;
        }
        if (*self.node).element_type() != XmlElementType::XmlElementNode {
            return 0;
        }
        if !self.curnode.is_null() {
            self.curnode = null_mut();
            return 1;
        }
        0
    }

    /// Moves the position of the current instance to the attribute with the specified qualified name.
    ///
    /// Returns 1 in case of success, -1 in case of error, 0 if not found
    #[doc(alias = "xmlTextReaderMoveToAttribute")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn move_to_attribute(&mut self, name: &str) -> i32 {
        use crate::{
            parser::split_qname2,
            tree::{NodeCommon, XmlNsPtr},
        };

        if self.node.is_null() {
            return -1;
        }

        // TODO: handle the xmlDecl
        if (*self.node).element_type() != XmlElementType::XmlElementNode {
            return 0;
        }

        let Some((prefix, localname)) = split_qname2(name) else {
            // Namespace default decl
            if name == "xmlns" {
                let mut ns = (*self.node).ns_def;
                while let Some(now) = ns {
                    if now.prefix().is_none() {
                        self.curnode = now.as_ptr() as *mut XmlNode;
                        return 1;
                    }
                    ns = XmlNsPtr::from_raw(now.next).unwrap();
                }
                return 0;
            }

            let mut prop = (*self.node).properties;
            while let Some(now) = prop {
                // One need to have
                //   - same attribute names
                //   - and the attribute carrying that namespace
                if now.name().as_deref() == Some(name)
                    && now.ns.map_or(true, |ns| ns.prefix().is_none())
                {
                    self.curnode = now.as_ptr() as *mut XmlNode;
                    return 1;
                }
                prop = now.next;
            }
            return 0;
        };

        // Namespace default decl
        if prefix == "xmlns" {
            let mut ns = (*self.node).ns_def;
            while let Some(now) = ns {
                if now.prefix().as_deref() == Some(localname) {
                    self.curnode = now.as_ptr() as *mut XmlNode;
                    return 1;
                }
                ns = XmlNsPtr::from_raw(now.next).unwrap();
            }
        // goto not_found;
        } else {
            let mut prop = (*self.node).properties;
            while let Some(now) = prop {
                // One need to have
                //   - same attribute names
                //   - and the attribute carrying that namespace
                if now.name().as_deref() == Some(localname)
                    && now
                        .ns
                        .map_or(false, |ns| ns.prefix().as_deref() == Some(prefix))
                {
                    self.curnode = now.as_ptr() as *mut XmlNode;
                    return 1;
                }
                prop = now.next;
            }
        }
        0
    }

    /// Moves the position of the current instance to the attribute with the
    /// specified local name and namespace URI.
    ///
    /// Returns 1 in case of success, -1 in case of error, 0 if not found
    #[doc(alias = "xmlTextReaderMoveToAttributeNs")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn move_to_attribute_ns(&mut self, local_name: &str, namespace_uri: &str) -> i32 {
        use crate::tree::{NodeCommon, XmlNsPtr};

        if self.node.is_null() {
            return -1;
        }
        if (*self.node).element_type() != XmlElementType::XmlElementNode {
            return 0;
        }
        let node: *mut XmlNode = self.node;

        if namespace_uri == "http://www.w3.org/2000/xmlns/" {
            let prefix = (local_name != "xmlns").then_some(local_name);
            let mut ns = (*self.node).ns_def;
            while let Some(now) = ns {
                if (prefix.is_none() && now.prefix().is_none())
                    || now.prefix().as_deref() == Some(local_name)
                {
                    self.curnode = now.as_ptr() as *mut XmlNode;
                    return 1;
                }
                ns = XmlNsPtr::from_raw(now.next).unwrap();
            }
            return 0;
        }

        let mut prop = (*node).properties;
        while let Some(now) = prop {
            // One need to have
            //   - same attribute names
            //   - and the attribute carrying that namespace
            if now.name().as_deref() == Some(local_name)
                && now
                    .ns
                    .map_or(false, |ns| ns.href().as_deref() == Some(namespace_uri))
            {
                self.curnode = now.as_ptr() as *mut XmlNode;
                return 1;
            }
            prop = now.next;
        }
        0
    }

    /// Moves the position of the current instance to the attribute with
    /// the specified index relative to the containing element.
    ///
    /// Returns 1 in case of success, -1 in case of error, 0 if not found
    #[doc(alias = "xmlTextReaderMoveToAttributeNo")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn move_to_attribute_no(&mut self, no: i32) -> i32 {
        use crate::tree::{NodeCommon, XmlNsPtr};

        if self.node.is_null() {
            return -1;
        }
        // TODO: handle the xmlDecl
        if (*self.node).element_type() != XmlElementType::XmlElementNode {
            return -1;
        }

        self.curnode = null_mut();

        let mut ns = (*self.node).ns_def;
        let mut i = 0;
        while let Some(now) = ns.filter(|_| i < no) {
            ns = XmlNsPtr::from_raw(now.next).unwrap();
            i += 1;
        }

        if let Some(ns) = ns {
            self.curnode = ns.as_ptr() as *mut XmlNode;
            return 1;
        }

        let Some(mut cur) = (*self.node).properties else {
            return 0;
        };

        for _ in i..no {
            let Some(next) = cur.next else {
                return 0;
            };
            cur = next;
        }
        // TODO walk the DTD if present

        self.curnode = cur.as_ptr() as *mut XmlNode;
        1
    }

    /// Moves the position of the current instance to the first attribute
    /// associated with the current node.
    ///
    /// Returns 1 in case of success, -1 in case of error, 0 if not found
    #[doc(alias = "xmlTextReaderMoveToFirstAttribute")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn move_to_first_attribute(&mut self) -> i32 {
        use crate::tree::NodeCommon;

        if self.node.is_null() {
            return -1;
        }
        if (*self.node).element_type() != XmlElementType::XmlElementNode {
            return 0;
        }

        if let Some(ns_def) = (*self.node).ns_def {
            self.curnode = ns_def.as_ptr() as *mut XmlNode;
            return 1;
        }
        if let Some(prop) = (*self.node).properties {
            self.curnode = prop.as_ptr() as *mut XmlNode;
            return 1;
        }
        0
    }

    /// Moves the position of the current instance to the next attribute
    /// associated with the current node.
    ///
    /// Returns 1 in case of success, -1 in case of error, 0 if not found
    #[doc(alias = "xmlTextReaderMoveToNextAttribute")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn move_to_next_attribute(&mut self) -> i32 {
        use crate::tree::{NodeCommon, XmlNsPtr};

        if self.node.is_null() {
            return -1;
        }
        if (*self.node).element_type() != XmlElementType::XmlElementNode {
            return 0;
        }
        if self.curnode.is_null() {
            return self.move_to_first_attribute();
        }

        if (*self.curnode).element_type() == XmlElementType::XmlNamespaceDecl {
            let ns = XmlNsPtr::from_raw(self.curnode as *mut XmlNs)
                .unwrap()
                .unwrap();
            if !ns.next.is_null() {
                self.curnode = ns.next as *mut XmlNode;
                return 1;
            }
            if let Some(prop) = (*self.node).properties {
                self.curnode = prop.as_ptr() as *mut XmlNode;
                return 1;
            }
            return 0;
        } else if let Some(next) = (*self.curnode)
            .next
            .filter(|_| (*self.curnode).element_type() == XmlElementType::XmlAttributeNode)
        {
            self.curnode = next.as_ptr();
            return 1;
        }
        0
    }

    /// The quotation mark character used to enclose the value of an attribute.
    ///
    /// Returns " or ' and `None` in case of error
    #[doc(alias = "xmlTextReaderQuoteChar")]
    #[cfg(feature = "libxml_reader")]
    pub fn quote_char(&self) -> Option<char> {
        // TODO maybe lookup the attribute value for " first
        Some('"')
    }

    /// Gets the read state of the reader.
    ///
    /// Returns the state value, or -1 in case of error
    #[doc(alias = "xmlTextReaderReadState")]
    #[cfg(feature = "libxml_reader")]
    pub fn read_state(&self) -> XmlTextReaderMode {
        self.mode
    }

    /// This function provides the current index of the parser used
    /// by the reader, relative to the start of the current entity.
    /// This function actually just wraps a call to xmlBytesConsumed()
    /// for the parser context associated with the reader.
    /// See xmlBytesConsumed() for more information.
    ///
    /// Returns the index in bytes from the beginning of the entity or -1
    /// in case the index could not be computed.
    #[doc(alias = "xmlTextReaderByteConsumed")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn byte_consumed(&self) -> i64 {
        if self.ctxt.is_null() {
            return -1;
        }
        (*self.ctxt).byte_consumed()
    }

    /// Hacking interface allowing to get the xmlDocPtr corresponding to the
    /// current document being accessed by the xmlTextReader.
    ///
    /// # Note
    /// As a result of this call, the reader will not destroy the
    /// associated XML document and calling xmlFreeDoc() on the result
    /// is needed once the reader parsing has finished.
    ///
    /// Returns the xmlDocPtr or NULL in case of error.
    #[doc(alias = "xmlTextReaderCurrentDoc")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn current_doc(&mut self) -> Option<XmlDocPtr> {
        if self.doc.is_some() {
            return self.doc;
        }
        if self.ctxt.is_null() {
            return None;
        }
        let my_doc = (*self.ctxt).my_doc?;

        self.preserve = 1;
        Some(my_doc)
    }

    /// Hacking interface allowing to get the xmlNodePtr corresponding to the
    /// current node being accessed by the xmlTextReader. This is dangerous
    /// because the underlying node may be destroyed on the next Reads.
    ///
    /// Returns the xmlNodePtr or NULL in case of error.
    #[doc(alias = "xmlTextReaderCurrentNode")]
    #[cfg(feature = "libxml_reader")]
    pub fn current_node(&self) -> *mut XmlNode {
        if !self.curnode.is_null() {
            return self.curnode;
        }
        self.node
    }

    /// Skip to the node following the current one in document order while
    /// avoiding the subtree if any.
    ///
    /// Returns 1 if the node was read successfully, 0 if there is no more nodes to read,
    /// or -1 in case of error
    #[doc(alias = "xmlTextReaderNext")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn next(&mut self) -> i32 {
        use crate::tree::NodeCommon;

        let mut ret: i32;

        if self.doc.is_some() {
            return self.next_tree();
        }
        let cur: *mut XmlNode = self.node;
        if cur.is_null() || (*cur).element_type() != XmlElementType::XmlElementNode {
            return self.read();
        }
        if matches!(
            self.state,
            XmlTextReaderState::End | XmlTextReaderState::Backtrack
        ) {
            return self.read();
        }
        if (*cur).extra & NODE_IS_EMPTY as u16 != 0 {
            return self.read();
        }
        while {
            ret = self.read();
            if ret != 1 {
                return ret;
            }
            self.node != cur
        } {}
        self.read()
    }

    #[cfg(feature = "libxml_reader")]
    unsafe fn next_tree(&mut self) -> i32 {
        use crate::tree::NodeCommon;

        if self.state == XmlTextReaderState::End {
            return 0;
        }

        if self.node.is_null() {
            let Some(children) = self.doc.unwrap().children else {
                self.state = XmlTextReaderState::End;
                return 0;
            };

            self.node = children.as_ptr();
            self.state = XmlTextReaderState::Start;
            return 1;
        }

        if self.state != XmlTextReaderState::Backtrack {
            /* Here removed traversal to child, because we want to skip the subtree,
            replace with traversal to sibling to skip subtree */
            if let Some(next) = (*self.node).next {
                /* Move to sibling if present,skipping sub-tree */
                self.node = next.as_ptr();
                self.state = XmlTextReaderState::Start;
                return 1;
            }

            /* if (*(*reader).node).next is NULL mean no subtree for current node,
            so need to move to sibling of parent node if present */
            self.state = XmlTextReaderState::Backtrack;
            /* This will move to parent if present */
            self.read();
        }

        if let Some(next) = (*self.node).next {
            self.node = next.as_ptr();
            self.state = XmlTextReaderState::Start;
            return 1;
        }

        if let Some(parent) = (*self.node).parent() {
            if parent.element_type() == XmlElementType::XmlDocumentNode {
                self.state = XmlTextReaderState::End;
                return 0;
            }

            self.node = parent.as_ptr();
            self.depth -= 1;
            self.state = XmlTextReaderState::Backtrack;
            /* Repeat process to move to sibling of parent node if present */
            self.next_tree();
        }

        self.state = XmlTextReaderState::End;

        1
    }

    /// Skip to the node following the current one in document order while avoiding the subtree if any.
    /// Currently implemented only for Readers built on a document
    ///
    /// Returns 1 if the node was read successfully, 0 if there is no more nodes to read,
    /// or -1 in case of error
    #[doc(alias = "xmlTextReaderNextSibling")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn next_sibling(&mut self) -> i32 {
        if self.doc.is_none() {
            // TODO
            return -1;
        }

        if self.state == XmlTextReaderState::End {
            return 0;
        }

        if self.node.is_null() {
            return self.next_tree();
        }

        if let Some(next) = (*self.node).next {
            self.node = next.as_ptr();
            self.state = XmlTextReaderState::Start;
            return 1;
        }

        0
    }

    /// The depth of the node in the tree.
    ///
    /// Returns the depth or -1 in case of error
    #[doc(alias = "xmlTextReaderDepth")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn depth(&self) -> i32 {
        use crate::tree::NodeCommon;

        if self.node.is_null() {
            return 0;
        }

        if !self.curnode.is_null() {
            if matches!(
                (*self.curnode).element_type(),
                XmlElementType::XmlAttributeNode | XmlElementType::XmlNamespaceDecl
            ) {
                return self.depth + 1;
            }
            return self.depth + 2;
        }
        self.depth
    }

    /// The value indicating whether to normalize white space and attribute values.
    /// Since attribute value and end of line normalizations are a MUST in the XML
    /// specification only the value true is accepted. The broken behaviour of
    /// accepting out of range character entities like &#0; is of course not
    /// supported either.
    ///
    /// Returns 1 or -1 in case of error.
    #[doc(alias = "xmlTextReaderNormalization")]
    #[cfg(feature = "libxml_reader")]
    pub fn normalization(&self) -> i32 {
        1
    }

    /// Get the node type of the current node
    ///
    /// Reference:  
    /// <http://www.gnu.org/software/dotgnu/pnetlib-doc/System/Xml/XmlNodeType.html>
    ///
    /// Returns the xmlReaderTypes of the current node or -1 in case of error
    #[doc(alias = "xmlTextReaderNodeType")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn node_type(&self) -> XmlReaderTypes {
        use crate::tree::NodeCommon;

        if self.node.is_null() {
            return XmlReaderTypes::XmlReaderTypeNone;
        }
        let node = if !self.curnode.is_null() {
            self.curnode
        } else {
            self.node
        };
        match (*node).element_type() {
            XmlElementType::XmlElementNode => {
                if matches!(
                    self.state,
                    XmlTextReaderState::End | XmlTextReaderState::Backtrack
                ) {
                    return XmlReaderTypes::XmlReaderTypeEndElement;
                }
                XmlReaderTypes::XmlReaderTypeElement
            }
            XmlElementType::XmlNamespaceDecl | XmlElementType::XmlAttributeNode => {
                XmlReaderTypes::XmlReaderTypeAttribute
            }
            XmlElementType::XmlTextNode => {
                if (*self.node).is_blank_node() {
                    if (*self.node).get_space_preserve() != 0 {
                        XmlReaderTypes::XmlReaderTypeSignificantWhitespace
                    } else {
                        XmlReaderTypes::XmlReaderTypeWhitespace
                    }
                } else {
                    XmlReaderTypes::XmlReaderTypeText
                }
            }
            XmlElementType::XmlCDATASectionNode => XmlReaderTypes::XmlReaderTypeCDATA,
            XmlElementType::XmlEntityRefNode => XmlReaderTypes::XmlReaderTypeEntityReference,
            XmlElementType::XmlEntityNode => XmlReaderTypes::XmlReaderTypeEntity,
            XmlElementType::XmlPINode => XmlReaderTypes::XmlReaderTypeProcessingInstruction,
            XmlElementType::XmlCommentNode => XmlReaderTypes::XmlReaderTypeComment,
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                XmlReaderTypes::XmlReaderTypeDocument
            }
            XmlElementType::XmlDocumentFragNode => XmlReaderTypes::XmlReaderTypeDocumentFragment,
            XmlElementType::XmlNotationNode => XmlReaderTypes::XmlReaderTypeNotation,
            XmlElementType::XmlDocumentTypeNode | XmlElementType::XmlDTDNode => {
                XmlReaderTypes::XmlReaderTypeDocumentType
            }

            XmlElementType::XmlElementDecl
            | XmlElementType::XmlAttributeDecl
            | XmlElementType::XmlEntityDecl
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => XmlReaderTypes::XmlReaderTypeNone,
            _ => unreachable!(),
        }
        // return -1;
    }

    /// The base URI of the node.
    ///
    /// Returns the base URI or `None` if not available.
    #[doc(alias = "xmlTextReaderBaseUri")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn base_uri(&self) -> Option<String> {
        use crate::tree::NodeCommon;

        if self.node.is_null() {
            return None;
        }
        (*self.node).get_base(None)
    }

    /// The local name of the node.
    ///
    /// Returns the local name or `None` if not available.
    #[doc(alias = "xmlTextReaderLocalName")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn local_name(&self) -> Option<String> {
        use std::ffi::CStr;

        use crate::tree::{NodeCommon, XmlNsPtr};

        if self.node.is_null() {
            return None;
        }
        let node = if !self.curnode.is_null() {
            self.curnode
        } else {
            self.node
        };
        if (*node).element_type() == XmlElementType::XmlNamespaceDecl {
            let ns = XmlNsPtr::from_raw(node as *mut XmlNs).unwrap().unwrap();
            if let Some(prefix) = ns.prefix() {
                return Some(prefix.into_owned());
            } else {
                return Some("xmlns".to_owned());
            }
        }
        if !matches!(
            (*node).element_type(),
            XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode
        ) {
            return self.name();
        }
        (!(*node).name.is_null()).then(|| {
            CStr::from_ptr((*node).name as *const i8)
                .to_string_lossy()
                .into_owned()
        })
    }

    /// The qualified name of the node, equal to Prefix :LocalName.
    ///
    /// Returns the local name or `None` if not available.
    #[doc(alias = "xmlTextReaderName")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn name(&self) -> Option<String> {
        use std::ffi::CStr;

        use crate::tree::{NodeCommon, XmlNsPtr};

        if self.node.is_null() {
            return None;
        }
        let node = if !self.curnode.is_null() {
            self.curnode
        } else {
            self.node
        };
        match (*node).element_type() {
            XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode => {
                let Some(prefix) = (*node).ns.as_deref().and_then(|ns| ns.prefix()) else {
                    return (!(*node).name.is_null()).then(|| {
                        CStr::from_ptr((*node).name as *const i8)
                            .to_string_lossy()
                            .into_owned()
                    });
                };

                let mut ret = prefix.into_owned();
                ret.push(':');
                ret.push_str((*node).name().unwrap().as_ref());
                Some(ret)
            }
            XmlElementType::XmlTextNode => Some("#text".to_owned()),
            XmlElementType::XmlCDATASectionNode => Some("#cdata-section".to_owned()),
            XmlElementType::XmlEntityNode | XmlElementType::XmlEntityRefNode => {
                (!(*node).name.is_null()).then(|| {
                    CStr::from_ptr((*node).name as *const i8)
                        .to_string_lossy()
                        .into_owned()
                })
            }
            XmlElementType::XmlPINode => (!(*node).name.is_null()).then(|| {
                CStr::from_ptr((*node).name as *const i8)
                    .to_string_lossy()
                    .into_owned()
            }),
            XmlElementType::XmlCommentNode => Some("#comment".to_owned()),
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                Some("#document".to_owned())
            }
            XmlElementType::XmlDocumentFragNode => Some("#document-fragment".to_owned()),
            XmlElementType::XmlNotationNode => (!(*node).name.is_null()).then(|| {
                CStr::from_ptr((*node).name as *const i8)
                    .to_string_lossy()
                    .into_owned()
            }),
            XmlElementType::XmlDocumentTypeNode | XmlElementType::XmlDTDNode => {
                (!(*node).name.is_null()).then(|| {
                    CStr::from_ptr((*node).name as *const i8)
                        .to_string_lossy()
                        .into_owned()
                })
            }
            XmlElementType::XmlNamespaceDecl => {
                let ns = XmlNsPtr::from_raw(node as *mut XmlNs).unwrap().unwrap();

                let mut ret = "xmlns".to_owned();
                let Some(prefix) = ns.prefix() else {
                    return Some(ret);
                };
                ret.push(':');
                ret.push_str(&prefix);
                Some(ret)
            }
            XmlElementType::XmlElementDecl
            | XmlElementType::XmlAttributeDecl
            | XmlElementType::XmlEntityDecl
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => None,
            _ => unreachable!(),
        }
        // return null_mut();
    }

    /// The URI defining the namespace associated with the node.
    ///
    /// Returns the namespace URI or `None` if not available.
    #[doc(alias = "xmlTextReaderNamespaceUri")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn namespace_uri(&self) -> Option<String> {
        use std::ffi::CStr;

        use crate::tree::NodeCommon;

        if self.node.is_null() {
            return None;
        }
        let node = if !self.curnode.is_null() {
            self.curnode
        } else {
            self.node
        };
        if (*node).element_type() == XmlElementType::XmlNamespaceDecl {
            return Some("http://www.w3.org/2000/xmlns/".to_owned());
        }
        if !matches!(
            (*node).element_type(),
            XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode
        ) {
            return None;
        }
        if let Some(ns) = (*node).ns {
            return Some(
                CStr::from_ptr(ns.href as *const i8)
                    .to_string_lossy()
                    .into_owned(),
            );
        }
        None
    }

    /// A shorthand reference to the namespace associated with the node.
    ///
    /// Returns the prefix or `None` if not available.
    #[doc(alias = "xmlTextReaderPrefix")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn prefix(&self) -> Option<String> {
        use crate::tree::{NodeCommon, XmlNsPtr};

        if self.node.is_null() {
            return None;
        }
        let node = if !self.curnode.is_null() {
            self.curnode
        } else {
            self.node
        };
        if (*node).element_type() == XmlElementType::XmlNamespaceDecl {
            let ns = XmlNsPtr::from_raw(node as *mut XmlNs).unwrap().unwrap();
            ns.prefix()?;
            return Some("xmlns".to_owned());
        }
        if !matches!(
            (*node).element_type(),
            XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode
        ) {
            return None;
        }
        if let Some(prefix) = (*node).ns.as_deref().and_then(|ns| ns.prefix()) {
            return Some(prefix.into_owned());
        }
        None
    }

    /// Determine the standalone status of the document being read.
    ///
    /// Returns `Some(true)` if the document was declared to be standalone,
    /// `Some(false)`  if it was declared to be not standalone,
    /// or `None` if the document did not specify its standalone status or in case of error.
    #[doc(alias = "xmlTextReaderStandalone")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn standalone(&self) -> Option<bool> {
        let doc = self.doc.or_else(|| {
            (!self.ctxt.is_null())
                .then(|| (*self.ctxt).my_doc)
                .flatten()
        })?;

        match doc.standalone {
            1 => Some(true),
            0 => Some(false),
            _ => None,
        }
    }

    /// The xml:lang scope within which the node resides.
    ///
    /// Returns the xml:lang value or `None` if none exists.
    #[doc(alias = "xmlTextReaderXmlLang")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn xml_lang(&self) -> Option<String> {
        if self.node.is_null() {
            return None;
        }
        (*self.node).get_lang()
    }

    /// This tells the XML Reader to preserve the current node.
    /// The caller must also use xmlTextReaderCurrentDoc() to
    /// keep an handle on the resulting document once parsing has finished
    ///
    /// Returns the xmlNodePtr or NULL in case of error.
    #[doc(alias = "xmlTextReaderPreserve")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn preserve(&mut self) -> *mut XmlNode {
        use crate::tree::NodeCommon;

        let cur = if !self.curnode.is_null() {
            self.curnode
        } else {
            self.node
        };
        if cur.is_null() {
            return null_mut();
        }

        if !matches!(
            (*cur).element_type(),
            XmlElementType::XmlDocumentNode | XmlElementType::XmlDTDNode
        ) {
            (*cur).extra |= NODE_IS_PRESERVED as u16;
            (*cur).extra |= NODE_IS_SPRESERVED as u16;
        }
        self.preserves += 1;

        let mut parent = (*cur).parent();
        while let Some(mut now) = parent {
            if now.element_type() == XmlElementType::XmlElementNode {
                now.extra |= NODE_IS_PRESERVED as u16;
            }
            parent = now.parent();
        }
        cur
    }

    /// This tells the XML Reader to preserve all nodes matched by the pattern.
    /// The caller must also use xmlTextReaderCurrentDoc() to
    /// keep an handle on the resulting document once parsing has finished
    ///
    /// Returns a non-negative number in case of success and -1 in case of error
    #[doc(alias = "xmlTextReaderPreservePattern")]
    #[cfg(all(feature = "libxml_reader", feature = "libxml_pattern"))]
    pub unsafe fn preserve_pattern(
        &mut self,
        pattern: &str,
        namespaces: Option<Vec<(*const u8, *const u8)>>,
    ) -> i32 {
        use std::ffi::CString;

        let pattern = CString::new(pattern).unwrap();
        let comp: XmlPatternPtr = xml_patterncompile(pattern.as_ptr() as *const u8, 0, namespaces);
        if comp.is_null() {
            return -1;
        }

        self.pattern_tab.push(comp);
        self.pattern_tab.len() as i32 - 1
    }

    /// Provides the text value of the node if present
    ///
    /// Returns the string or `None` if not available.  
    #[doc(alias = "xmlTextReaderValue")]
    #[cfg(feature = "libxml_reader")]
    pub unsafe fn text_value(&self) -> Option<String> {
        use std::ffi::CStr;

        use crate::tree::{NodeCommon, XmlAttrPtr, XmlNsPtr};

        if self.node.is_null() {
            return None;
        }
        let node = if !self.curnode.is_null() {
            self.curnode
        } else {
            self.node
        };

        match (*node).element_type() {
            XmlElementType::XmlNamespaceDecl => {
                return Some(
                    CStr::from_ptr(
                        XmlNsPtr::from_raw(node as *mut XmlNs)
                            .unwrap()
                            .unwrap()
                            .href as *const i8,
                    )
                    .to_string_lossy()
                    .into_owned(),
                )
            }
            XmlElementType::XmlAttributeNode => {
                let attr = XmlAttrPtr::from_raw(node as *mut XmlAttr).unwrap().unwrap();

                return if let Some(parent) = attr.parent {
                    attr.children.and_then(|c| c.get_string(parent.doc, 1))
                } else {
                    attr.children.and_then(|c| c.get_string(None, 1))
                };
            }
            XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlPINode
            | XmlElementType::XmlCommentNode => {
                if !(*node).content.is_null() {
                    return Some(
                        CStr::from_ptr((*node).content as *const i8)
                            .to_string_lossy()
                            .into_owned(),
                    );
                }
            }
            _ => {}
        }
        None
    }
}

impl Default for XmlTextReader {
    fn default() -> Self {
        Self {
            mode: XmlTextReaderMode::default(),
            doc: None,
            validate: XmlTextReaderValidate::default(),
            allocs: 0,
            state: XmlTextReaderState::default(),
            ctxt: null_mut(),
            // sax: None,
            input: None,
            start_element: None,
            end_element: None,
            start_element_ns: None,
            end_element_ns: None,
            characters: None,
            cdata_block: None,
            base: 0,
            cur: 0,
            node: null_mut(),
            curnode: null_mut(),
            depth: 0,
            faketext: null_mut(),
            preserve: 0,
            buffer: "".to_owned(),
            dict: null_mut(),
            ent: null_mut(),
            // ent_nr: 0,
            // ent_max: 0,
            ent_tab: vec![],
            error_func: None,
            error_func_arg: None,
            #[cfg(feature = "schema")]
            rng_schemas: null_mut(),
            #[cfg(feature = "schema")]
            rng_valid_ctxt: null_mut(),
            #[cfg(feature = "schema")]
            rng_preserve_ctxt: 0,
            #[cfg(feature = "schema")]
            rng_valid_errors: 0,
            #[cfg(feature = "schema")]
            rng_full_node: null_mut(),
            #[cfg(feature = "schema")]
            xsd_schemas: null_mut(),
            #[cfg(feature = "schema")]
            xsd_valid_ctxt: null_mut(),
            #[cfg(feature = "schema")]
            xsd_preserve_ctxt: 0,
            #[cfg(feature = "schema")]
            xsd_valid_errors: 0,
            #[cfg(feature = "schema")]
            xsd_plug: null_mut(),
            #[cfg(feature = "xinclude")]
            xinclude: 0,
            #[cfg(feature = "xinclude")]
            xinclude_name: null(),
            #[cfg(feature = "xinclude")]
            xincctxt: null_mut(),
            #[cfg(feature = "xinclude")]
            in_xinclude: 0,
            // #[cfg(feature = "libxml_pattern")]
            // pattern_nr: 0,
            // #[cfg(feature = "libxml_pattern")]
            // pattern_max: 0,
            #[cfg(feature = "libxml_pattern")]
            pattern_tab: vec![],
            preserves: 0,
            parser_flags: 0,
            serror_func: None,
        }
    }
}

const NODE_IS_EMPTY: i32 = 0x1;
const NODE_IS_PRESERVED: i32 = 0x2;
const NODE_IS_SPRESERVED: i32 = 0x4;

/// Macro used to return an interned string
macro_rules! CONSTSTR {
    ($reader:expr, $str:expr) => {
        xml_dict_lookup((*$reader).dict, $str, -1)
    };
}
macro_rules! CONSTQSTR {
    ($reader:expr, $p:expr, $str:expr) => {
        $crate::libxml::dict::xml_dict_qlookup((*$reader).dict, $p, $str)
    };
}

/// called when an opening tag has been processed.
#[doc(alias = "xmlTextReaderStartElement")]
unsafe fn xml_text_reader_start_element(
    ctx: Option<GenericErrorContext>,
    fullname: &str,
    atts: &[(String, Option<String>)],
) {
    let ctxt = {
        let ctx = ctx.as_ref().unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    let reader: XmlTextReaderPtr = (*ctxt)._private as _;

    if !reader.is_null() {
        if let Some(start_element) = (*reader).start_element {
            start_element(ctx, fullname, atts);
            if let Some(mut node) = (*ctxt).node.filter(|_| {
                !(*ctxt).input.is_null()
                    && !(*(*ctxt).input).cur.is_null()
                    && *(*(*ctxt).input).cur.add(0) == b'/'
                    && *(*(*ctxt).input).cur.add(1) == b'>'
            }) {
                node.extra = NODE_IS_EMPTY as _;
            }
        }
    }
    if !reader.is_null() {
        (*reader).state = XmlTextReaderState::Element;
    }
}

/// called when an ending tag has been processed.
#[doc(alias = "xmlTextReaderEndElement")]
unsafe fn xml_text_reader_end_element(ctx: Option<GenericErrorContext>, fullname: &str) {
    let ctxt = {
        let ctx = ctx.as_ref().unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    let reader: XmlTextReaderPtr = (*ctxt)._private as _;

    if !reader.is_null() {
        if let Some(end_element) = (*reader).end_element {
            end_element(ctx, fullname);
        }
    }
}

/// Called when an opening tag has been processed.
#[doc(alias = "xmlTextReaderStartElementNs")]
#[allow(clippy::too_many_arguments)]
// #[allow(clippy::type_complexity)]
unsafe fn xml_text_reader_start_element_ns(
    ctx: Option<GenericErrorContext>,
    localname: &str,
    prefix: Option<&str>,
    uri: Option<&str>,
    namespaces: &[(Option<String>, String)],
    nb_defaulted: usize,
    attributes: &[(String, Option<String>, Option<String>, String)],
) {
    let ctxt = {
        let ctx = ctx.as_ref().unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    let reader: XmlTextReaderPtr = (*ctxt)._private as _;

    if !reader.is_null() {
        if let Some(start_element_ns) = (*reader).start_element_ns {
            start_element_ns(
                ctx,
                localname,
                prefix,
                uri,
                namespaces,
                nb_defaulted,
                attributes,
            );
            if let Some(mut node) = (*ctxt).node.filter(|_| {
                !(*ctxt).input.is_null()
                    && !(*(*ctxt).input).cur.is_null()
                    && *(*(*ctxt).input).cur.add(0) == b'/'
                    && *(*(*ctxt).input).cur.add(1) == b'>'
            }) {
                node.extra = NODE_IS_EMPTY as _;
            }
        }
    }
    if !reader.is_null() {
        (*reader).state = XmlTextReaderState::Element;
    }
}

/// Called when an ending tag has been processed.
#[doc(alias = "xmlTextReaderEndElementNs")]
unsafe fn xml_text_reader_end_element_ns(
    ctx: Option<GenericErrorContext>,
    localname: &str,
    prefix: Option<&str>,
    uri: Option<&str>,
) {
    let ctxt = {
        let ctx = ctx.as_ref().unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    let reader: XmlTextReaderPtr = (*ctxt)._private as _;

    if !reader.is_null() {
        if let Some(end_element_ns) = (*reader).end_element_ns {
            end_element_ns(ctx, localname, prefix, uri);
        }
    }
}

/// Receiving some chars from the parser.
#[doc(alias = "xmlTextReaderCharacters")]
unsafe fn xml_text_reader_characters(ctx: Option<GenericErrorContext>, ch: &str) {
    let ctxt = {
        let ctx = ctx.as_ref().unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    let reader: XmlTextReaderPtr = (*ctxt)._private as _;

    if !reader.is_null() {
        if let Some(characters) = (*reader).characters {
            characters(ctx, ch);
        }
    }
}

/// Called when a pcdata block has been parsed
#[doc(alias = "xmlTextReaderCDataBlock")]
unsafe fn xml_text_reader_cdata_block(ctx: Option<GenericErrorContext>, ch: &str) {
    let ctxt = {
        let ctx = ctx.as_ref().unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    let reader: XmlTextReaderPtr = (*ctxt)._private as _;

    if !reader.is_null() {
        if let Some(cdata_block) = (*reader).cdata_block {
            cdata_block(ctx, ch);
        }
    }
}

/// Create an xmlTextReader structure fed with @input
///
/// Returns the new xmlTextReaderPtr or NULL in case of error
#[doc(alias = "xmlNewTextReader")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_new_text_reader(
    input: XmlParserInputBuffer,
    uri: Option<&str>,
) -> XmlTextReaderPtr {
    use std::ptr::drop_in_place;

    use crate::generic_error;

    let ret: XmlTextReaderPtr = xml_malloc(size_of::<XmlTextReader>()) as _;
    if ret.is_null() {
        generic_error!("xmlNewTextReader : malloc failed\n");
        return null_mut();
    }
    std::ptr::write(ret, XmlTextReader::default());
    (*ret).doc = None;
    (*ret).input = Some(input);
    let mut sax = XmlSAXHandler::default();
    xml_sax_version(&mut sax, 2);
    (*ret).start_element = sax.start_element;
    sax.start_element = Some(xml_text_reader_start_element);
    (*ret).end_element = sax.end_element;
    sax.end_element = Some(xml_text_reader_end_element);
    #[cfg(feature = "sax1")]
    {
        if sax.initialized == XML_SAX2_MAGIC as u32 {
            (*ret).start_element_ns = sax.start_element_ns;
            sax.start_element_ns = Some(xml_text_reader_start_element_ns);
            (*ret).end_element_ns = sax.end_element_ns;
            sax.end_element_ns = Some(xml_text_reader_end_element_ns);
        } else {
            (*ret).start_element_ns = None;
            (*ret).end_element_ns = None;
        }
    }
    #[cfg(not(feature = "sax1"))]
    {
        (*ret).start_element_ns = sax.start_element_ns;
        sax.start_element_ns = Some(xml_text_reader_start_element_ns);
        (*ret).end_element_ns = sax.end_element_ns;
        sax.end_element_ns = Some(xml_text_reader_end_element_ns);
    }
    (*ret).characters = sax.characters;
    sax.characters = Some(xml_text_reader_characters);
    sax.ignorable_whitespace = Some(xml_text_reader_characters);
    (*ret).cdata_block = sax.cdata_block;
    sax.cdata_block = Some(xml_text_reader_cdata_block);

    (*ret).mode = XmlTextReaderMode::XmlTextreaderModeInitial as _;
    (*ret).node = null_mut();
    (*ret).curnode = null_mut();
    if (*ret)
        .input
        .as_ref()
        .unwrap()
        .buffer
        .map_or(0, |buf| buf.len())
        < 4
    {
        (*ret).input.as_mut().unwrap().read(4);
    }
    if (*ret)
        .input
        .as_ref()
        .unwrap()
        .buffer
        .map_or(0, |buf| buf.len())
        >= 4
    {
        (*ret).ctxt = xml_create_push_parser_ctxt(
            Some(Box::new(sax)),
            None,
            (*ret)
                .input
                .as_ref()
                .unwrap()
                .buffer
                .expect("Internal Error")
                .as_ref()
                .as_ptr() as _,
            4,
            uri,
        );
        (*ret).base = 0;
        (*ret).cur = 4;
    } else {
        (*ret).ctxt = xml_create_push_parser_ctxt(Some(Box::new(sax)), None, null_mut(), 0, uri);
        (*ret).base = 0;
        (*ret).cur = 0;
    }

    if (*ret).ctxt.is_null() {
        generic_error!("xmlNewTextReader : malloc failed\n");
        drop_in_place(ret);
        xml_free(ret as _);
        return null_mut();
    }
    (*(*ret).ctxt).parse_mode = XmlParserMode::XmlParseReader;
    (*(*ret).ctxt)._private = ret as _;
    (*(*ret).ctxt).linenumbers = 1;
    (*(*ret).ctxt).dict_names = 1;
    (*ret).allocs = XML_TEXTREADER_CTXT;
    // use the parser dictionary to allocate all elements and attributes names
    (*(*ret).ctxt).docdict = 1;
    (*ret).dict = (*(*ret).ctxt).dict;
    #[cfg(feature = "xinclude")]
    {
        (*ret).xinclude = 0;
    }
    #[cfg(feature = "libxml_pattern")]
    {
        (*ret).pattern_tab.clear();
    }
    ret
}

/// Create an xmlTextReader structure fed with the resource at @URI
///
/// Returns the new xmlTextReaderPtr or NULL in case of error
#[doc(alias = "xmlNewTextReaderFilename")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_new_text_reader_filename(uri: &str) -> XmlTextReaderPtr {
    use crate::{encoding::XmlCharEncoding, io::xml_parser_get_directory};

    let Some(input) = XmlParserInputBuffer::from_uri(uri, XmlCharEncoding::None) else {
        return null_mut();
    };
    let ret: XmlTextReaderPtr = xml_new_text_reader(input, Some(uri));
    if ret.is_null() {
        return null_mut();
    }
    (*ret).allocs |= XML_TEXTREADER_INPUT;
    if (*(*ret).ctxt).directory.is_none() {
        if let Some(directory) = xml_parser_get_directory(uri) {
            (*(*ret).ctxt).directory = Some(directory.to_string_lossy().into_owned());
        }
    }
    ret
}

/// Deallocate all the resources associated to the reader
#[doc(alias = "xmlFreeTextReader")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_free_text_reader(reader: XmlTextReaderPtr) {
    use std::ptr::drop_in_place;

    use crate::parser::xml_free_parser_ctxt;
    #[cfg(feature = "schema")]
    use crate::relaxng::xml_relaxng_free_valid_ctxt;

    use super::xinclude::xml_xinclude_free_context;

    if reader.is_null() {
        return;
    }
    #[cfg(feature = "schema")]
    {
        if !(*reader).rng_schemas.is_null() {
            xml_relaxng_free((*reader).rng_schemas);
            (*reader).rng_schemas = null_mut();
        }
        if !(*reader).rng_valid_ctxt.is_null() {
            if (*reader).rng_preserve_ctxt == 0 {
                xml_relaxng_free_valid_ctxt((*reader).rng_valid_ctxt);
            }
            (*reader).rng_valid_ctxt = null_mut();
        }
        if !(*reader).xsd_plug.is_null() {
            xml_schema_sax_unplug((*reader).xsd_plug);
            (*reader).xsd_plug = null_mut();
        }
        if !(*reader).xsd_valid_ctxt.is_null() {
            if (*reader).xsd_preserve_ctxt == 0 {
                xml_schema_free_valid_ctxt((*reader).xsd_valid_ctxt);
            }
            (*reader).xsd_valid_ctxt = null_mut();
        }
        if !(*reader).xsd_schemas.is_null() {
            xml_schema_free((*reader).xsd_schemas);
            (*reader).xsd_schemas = null_mut();
        }
    }
    #[cfg(feature = "xinclude")]
    if !(*reader).xincctxt.is_null() {
        xml_xinclude_free_context((*reader).xincctxt);
    }
    #[cfg(feature = "libxml_pattern")]
    {
        for pattern in (*reader).pattern_tab.drain(..) {
            if !pattern.is_null() {
                xml_free_pattern(pattern);
            }
        }
    }
    if (*reader).mode != XmlTextReaderMode::XmlTextreaderModeClosed {
        xml_text_reader_close(&mut *reader);
    }
    if !(*reader).ctxt.is_null() {
        if (*reader).dict == (*(*reader).ctxt).dict {
            (*reader).dict = null_mut();
        }
        if (*reader).allocs & XML_TEXTREADER_CTXT != 0 {
            xml_free_parser_ctxt((*reader).ctxt);
        }
    }
    if !(*reader).dict.is_null() {
        xml_dict_free((*reader).dict);
    }
    drop_in_place(reader);
    xml_free(reader as _);
}

/// Setup an XML reader with new options
///
/// Returns 0 in case of success and -1 in case of error.
#[doc(alias = "xmlTextReaderSetup")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_text_reader_setup(
    reader: XmlTextReaderPtr,
    input: Option<XmlParserInputBuffer>,
    url: Option<&str>,
    encoding: *const c_char,
    mut options: i32,
) -> i32 {
    use std::{cell::RefCell, ffi::CStr, rc::Rc};

    use crate::{
        encoding::{find_encoding_handler, XmlCharEncoding},
        generic_error,
        libxml::xinclude::{xml_xinclude_free_context, XINCLUDE_NODE},
        parser::{xml_new_input_stream, XmlParserInputPtr},
        uri::canonic_path,
    };

    if reader.is_null() {
        return -1;
    }

    // we force the generation of compact text nodes on the reader
    // since usr applications should never modify the tree
    options |= XmlParserOption::XmlParseCompact as i32;

    (*reader).doc = None;
    (*reader).ent_tab.clear();
    (*reader).parser_flags = options;
    (*reader).validate = XmlTextReaderValidate::NotValidate;
    if input.is_some() && (*reader).input.is_some() && (*reader).allocs & XML_TEXTREADER_INPUT != 0
    {
        let _ = (*reader).input.take();
        (*reader).allocs -= XML_TEXTREADER_INPUT;
    }
    let replaced = input.is_some();
    if input.is_some() {
        (*reader).input = input;
        (*reader).allocs |= XML_TEXTREADER_INPUT;
    }
    (*reader).buffer.clear();
    (*reader).buffer.reserve(100);
    let mut sax = (*(*reader).ctxt).sax.take().unwrap_or_default();
    xml_sax_version(&mut sax, 2);
    (*reader).start_element = sax.start_element;
    sax.start_element = Some(xml_text_reader_start_element);
    (*reader).end_element = sax.end_element;
    sax.end_element = Some(xml_text_reader_end_element);
    #[cfg(feature = "sax1")]
    {
        if sax.initialized == XML_SAX2_MAGIC as u32 {
            (*reader).start_element_ns = sax.start_element_ns;
            sax.start_element_ns = Some(xml_text_reader_start_element_ns);
            (*reader).end_element_ns = sax.end_element_ns;
            sax.end_element_ns = Some(xml_text_reader_end_element_ns);
        } else {
            (*reader).start_element_ns = None;
            (*reader).end_element_ns = None;
        }
    }
    #[cfg(not(feature = "sax1"))]
    {
        (*reader).start_element_ns = sax.start_element_ns;
        sax.start_element_ns = Some(xml_text_reader_start_element_ns);
        (*reader).end_element_ns = sax.end_element_ns;
        sax.end_element_ns = Some(xml_text_reader_end_element_ns);
    }
    (*reader).characters = sax.characters;
    sax.characters = Some(xml_text_reader_characters);
    sax.ignorable_whitespace = Some(xml_text_reader_characters);
    (*reader).cdata_block = sax.cdata_block;
    sax.cdata_block = Some(xml_text_reader_cdata_block);
    (*(*reader).ctxt).sax = Some(sax);

    (*reader).mode = XmlTextReaderMode::XmlTextreaderModeInitial as _;
    (*reader).node = null_mut();
    (*reader).curnode = null_mut();
    if replaced {
        if (*reader)
            .input
            .as_ref()
            .unwrap()
            .buffer
            .map_or(0, |buf| buf.len())
            < 4
        {
            (*reader).input.as_mut().unwrap().read(4);
        }
        if (*reader).ctxt.is_null() {
            if (*reader)
                .input
                .as_mut()
                .unwrap()
                .buffer
                .map_or(0, |buf| buf.len())
                >= 4
            {
                (*reader).ctxt = xml_create_push_parser_ctxt(
                    (*(*reader).ctxt).sax.take(),
                    None,
                    (*reader)
                        .input
                        .as_mut()
                        .unwrap()
                        .buffer
                        .expect("Internal Error")
                        .as_ref()
                        .as_ptr() as _,
                    4,
                    url,
                );
                (*reader).base = 0;
                (*reader).cur = 4;
            } else {
                (*reader).ctxt = xml_create_push_parser_ctxt(
                    (*(*reader).ctxt).sax.take(),
                    None,
                    null_mut(),
                    0,
                    url,
                );
                (*reader).base = 0;
                (*reader).cur = 0;
            }
        } else {
            let enc = XmlCharEncoding::None;

            (*(*reader).ctxt).reset();
            let buf = XmlParserInputBuffer::new(enc);
            let input_stream: XmlParserInputPtr = xml_new_input_stream(Some(&mut *(*reader).ctxt));
            if input_stream.is_null() {
                return -1;
            }

            if let Some(url) = url {
                let canonic = canonic_path(url);
                (*input_stream).filename = Some(canonic.into_owned());
            } else {
                (*input_stream).filename = None;
            }
            (*input_stream).buf = Some(Rc::new(RefCell::new(buf)));
            (*input_stream).reset_base();

            (*(*reader).ctxt).input_push(input_stream);
            (*reader).cur = 0;
        }
        if (*reader).ctxt.is_null() {
            generic_error!("xmlTextReaderSetup : malloc failed\n");
            return -1;
        }
    }
    if !(*reader).dict.is_null() {
        if !(*(*reader).ctxt).dict.is_null() {
            if (*reader).dict != (*(*reader).ctxt).dict {
                xml_dict_free((*reader).dict);
                (*reader).dict = (*(*reader).ctxt).dict;
            }
        } else {
            (*(*reader).ctxt).dict = (*reader).dict;
        }
    } else {
        if (*(*reader).ctxt).dict.is_null() {
            (*(*reader).ctxt).dict = xml_dict_create();
        }
        (*reader).dict = (*(*reader).ctxt).dict;
    }
    (*(*reader).ctxt)._private = reader as _;
    (*(*reader).ctxt).linenumbers = 1;
    (*(*reader).ctxt).dict_names = 1;
    // use the parser dictionary to allocate all elements and attributes names
    (*(*reader).ctxt).docdict = 1;
    (*(*reader).ctxt).parse_mode = XmlParserMode::XmlParseReader;

    #[cfg(feature = "xinclude")]
    {
        if !(*reader).xincctxt.is_null() {
            xml_xinclude_free_context((*reader).xincctxt);
            (*reader).xincctxt = null_mut();
        }
        if options & XmlParserOption::XmlParseXInclude as i32 != 0 {
            (*reader).xinclude = 1;
            (*reader).xinclude_name =
                xml_dict_lookup((*reader).dict, XINCLUDE_NODE.as_ptr() as _, -1);
            options -= XmlParserOption::XmlParseXInclude as i32;
        } else {
            (*reader).xinclude = 0;
        }
        (*reader).in_xinclude = 0;
    }
    #[cfg(feature = "libxml_pattern")]
    while let Some(pattern) = (*reader).pattern_tab.pop() {
        if !pattern.is_null() {
            xml_free_pattern(pattern);
        }
    }

    if options & XmlParserOption::XmlParseDTDValid as i32 != 0 {
        (*reader).validate = XmlTextReaderValidate::ValidateDtd;
    }

    xml_ctxt_use_options((*reader).ctxt, options);
    if !encoding.is_null() {
        if let Some(handler) = find_encoding_handler(CStr::from_ptr(encoding).to_str().unwrap()) {
            (*(*reader).ctxt).switch_to_encoding(handler);
        }
    }
    if !(*(*reader).ctxt).input.is_null() && (*(*(*reader).ctxt).input).filename.is_none() {
        if let Some(url) = url {
            (*(*(*reader).ctxt).input).filename = Some(url.to_owned());
        }
    }

    (*reader).doc = None;

    0
}

const CHUNK_SIZE: usize = 512;

#[cfg(feature = "libxml_reader")]
const MAX_FREE_NODES: i32 = 100;

/// Free a string if it is not owned by the "dict" dictionary in the current scope
#[cfg(feature = "libxml_reader")]
macro_rules! DICT_FREE {
    ($dict:expr, $str:expr) => {
        if !$str.is_null()
            && ($dict.is_null() || $crate::libxml::dict::xml_dict_owns($dict, $str as _) == 0)
        {
            xml_free($str as _);
        }
    };
}

/// Free a property and all its siblings, all the children are freed too.
#[doc(alias = "xmlTextReaderFreePropList")]
#[cfg(feature = "libxml_reader")]
unsafe fn xml_text_reader_free_prop_list(reader: &mut XmlTextReader, mut cur: Option<XmlAttrPtr>) {
    while let Some(now) = cur {
        let next = now.next;
        xml_text_reader_free_prop(reader, now);
        cur = next;
    }
}

/// Free a node and all its siblings, this is a recursive behaviour, all the children are freed too.
#[doc(alias = "xmlTextReaderFreeNodeList")]
#[cfg(feature = "libxml_reader")]
unsafe fn xml_text_reader_free_node_list(reader: XmlTextReaderPtr, mut cur: *mut XmlNode) {
    use crate::tree::{NodeCommon, NodePtr, XmlNodePtr, XmlNsPtr};

    let mut next: *mut XmlNode;
    let mut parent: *mut XmlNode;
    let mut depth: usize = 0;

    let dict = if !reader.is_null() && !(*reader).ctxt.is_null() {
        (*(*reader).ctxt).dict
    } else {
        null_mut()
    };
    if cur.is_null() {
        return;
    }
    if (*cur).element_type() == XmlElementType::XmlNamespaceDecl {
        xml_free_ns_list(XmlNsPtr::from_raw(cur as *mut XmlNs).unwrap().unwrap());
        return;
    }
    if matches!(
        (*cur).element_type(),
        XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode
    ) {
        xml_free_doc(XmlDocPtr::from_raw(cur as *mut XmlDoc).unwrap().unwrap());
        return;
    }
    loop {
        while let Some(children) = (*cur).children().filter(|children| {
            children.parent() == NodePtr::from_ptr(cur)
                && !matches!(
                    (*cur).element_type(),
                    XmlElementType::XmlDTDNode | XmlElementType::XmlEntityRefNode
                )
        }) {
            cur = children.as_ptr();
            depth += 1;
        }

        next = (*cur).next().map_or(null_mut(), |n| n.as_ptr());
        parent = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());

        /* unroll to speed up freeing the document */
        if (*cur).element_type() != XmlElementType::XmlDTDNode {
            if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0 {
                // if let Some(f) = xmlDeregisterNodeDefaultValue {
                //     f(cur as _);
                // }
                xml_deregister_node_default_value(cur as _);
            }

            if matches!(
                (*cur).element_type(),
                XmlElementType::XmlElementNode
                    | XmlElementType::XmlXIncludeStart
                    | XmlElementType::XmlXIncludeEnd
            ) && (*cur).properties.is_some()
            {
                xml_text_reader_free_prop_list(&mut *reader, (*cur).properties);
            }
            if !matches!(
                (*cur).element_type(),
                XmlElementType::XmlElementNode
                    | XmlElementType::XmlXIncludeStart
                    | XmlElementType::XmlXIncludeEnd
                    | XmlElementType::XmlEntityRefNode
            ) {
                DICT_FREE!(dict, (*cur).content);
            }
            if matches!(
                (*cur).element_type(),
                XmlElementType::XmlElementNode
                    | XmlElementType::XmlXIncludeStart
                    | XmlElementType::XmlXIncludeEnd
            ) {
                if let Some(ns_def) = (*cur).ns_def {
                    xml_free_ns_list(ns_def);
                }
            }

            // we don't free element names here they are interned now
            if !matches!(
                (*cur).element_type(),
                XmlElementType::XmlTextNode | XmlElementType::XmlCommentNode
            ) {
                DICT_FREE!(dict, (*cur).name);
            }
            if matches!(
                (*cur).element_type(),
                XmlElementType::XmlElementNode | XmlElementType::XmlTextNode
            ) && !reader.is_null()
                && !(*reader).ctxt.is_null()
                && (*(*reader).ctxt).free_elems_nr < MAX_FREE_NODES
            {
                (*cur).next = NodePtr::from_ptr((*(*reader).ctxt).free_elems);
                (*(*reader).ctxt).free_elems = cur;
                (*(*reader).ctxt).free_elems_nr += 1;
            } else {
                XmlNodePtr::from_raw(cur).unwrap().unwrap().free();
            }
        }

        if !next.is_null() {
            cur = next;
        } else {
            if depth == 0 || parent.is_null() {
                break;
            }
            depth -= 1;
            cur = parent;
            (*cur).set_children(None);
        }
    }
}

/// Free a node.
#[doc(alias = "xmlTextReaderFreeProp")]
#[cfg(feature = "libxml_reader")]
unsafe fn xml_text_reader_free_prop(reader: XmlTextReaderPtr, mut cur: XmlAttrPtr) {
    let dict = if !reader.is_null() && !(*reader).ctxt.is_null() {
        (*(*reader).ctxt).dict
    } else {
        null_mut()
    };

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0 {
        // if let Some(f) = xmlDeregisterNodeDefaultValue {
        //     f(cur as _);
        // }
        xml_deregister_node_default_value(cur.as_ptr() as _);
    }

    if let Some(children) = cur.children {
        xml_text_reader_free_node_list(reader, children.as_ptr());
    }

    DICT_FREE!(dict, cur.name);
    if !reader.is_null()
        && !(*reader).ctxt.is_null()
        && (*(*reader).ctxt).free_attrs_nr < MAX_FREE_NODES
    {
        cur.next = (*(*reader).ctxt).free_attrs;
        (*(*reader).ctxt).free_attrs = Some(cur);
        (*(*reader).ctxt).free_attrs_nr += 1;
    } else {
        cur.free();
    }
}

/// Free a node, this is a recursive behaviour, all the children are freed too.
/// This doesn't unlink the child from the list, use xmlUnlinkNode() first.
#[doc(alias = "xmlTextReaderFreeNode")]
#[cfg(feature = "libxml_reader")]
unsafe fn xml_text_reader_free_node(reader: XmlTextReaderPtr, cur: *mut XmlNode) {
    use crate::tree::{NodeCommon, NodePtr, XmlDtd, XmlDtdPtr, XmlNodePtr, XmlNsPtr};

    let dict = if !reader.is_null() && !(*reader).ctxt.is_null() {
        (*(*reader).ctxt).dict
    } else {
        null_mut()
    };
    if (*cur).element_type() == XmlElementType::XmlDTDNode {
        xml_free_dtd(XmlDtdPtr::from_raw(cur as *mut XmlDtd).unwrap().unwrap());
        return;
    }
    if (*cur).element_type() == XmlElementType::XmlNamespaceDecl {
        xml_free_ns(XmlNsPtr::from_raw(cur as *mut XmlNs).unwrap().unwrap());
        return;
    }
    if (*cur).element_type() == XmlElementType::XmlAttributeNode {
        xml_text_reader_free_prop(
            reader,
            XmlAttrPtr::from_raw(cur as *mut XmlAttr).unwrap().unwrap(),
        );
        return;
    }

    if let Some(children) = (*cur)
        .children()
        .filter(|_| (*cur).element_type() != XmlElementType::XmlEntityRefNode)
    {
        if children.parent() == NodePtr::from_ptr(cur) {
            xml_text_reader_free_node_list(reader, children.as_ptr());
        }
        (*cur).set_children(None);
    }

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0 {
        // if let Some(f) = xmlDeregisterNodeDefaultValue {
        //     f(cur);
        // }
        xml_deregister_node_default_value(cur);
    }

    if matches!(
        (*cur).element_type(),
        XmlElementType::XmlElementNode
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd
    ) && (*cur).properties.is_some()
    {
        xml_text_reader_free_prop_list(&mut *reader, (*cur).properties);
    }
    if !matches!(
        (*cur).element_type(),
        XmlElementType::XmlElementNode
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd
            | XmlElementType::XmlEntityRefNode
    ) {
        DICT_FREE!(dict, (*cur).content);
    }
    if matches!(
        (*cur).element_type(),
        XmlElementType::XmlElementNode
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd
    ) {
        if let Some(ns_def) = (*cur).ns_def {
            xml_free_ns_list(ns_def);
        }
    }

    // we don't free names here they are interned now
    if !matches!(
        (*cur).element_type(),
        XmlElementType::XmlTextNode | XmlElementType::XmlCommentNode
    ) {
        DICT_FREE!(dict, (*cur).name);
    }

    if matches!(
        (*cur).element_type(),
        XmlElementType::XmlElementNode | XmlElementType::XmlTextNode
    ) && !reader.is_null()
        && !(*reader).ctxt.is_null()
        && (*(*reader).ctxt).free_elems_nr < MAX_FREE_NODES
    {
        (*cur).next = NodePtr::from_ptr((*(*reader).ctxt).free_elems);
        (*(*reader).ctxt).free_elems = cur;
        (*(*reader).ctxt).free_elems_nr += 1;
    } else {
        XmlNodePtr::from_raw(cur).unwrap().unwrap().free();
    }
}

/// Get the successor of a node if available.
///
/// Returns the successor node or NULL
#[doc(alias = "xmlTextReaderGetSuccessor")]
#[cfg(feature = "libxml_reader")]
unsafe fn xml_text_reader_get_successor(mut cur: *mut XmlNode) -> *mut XmlNode {
    use crate::tree::NodeCommon;

    if cur.is_null() {
        return null_mut(); /* ERROR */
    }
    if let Some(next) = (*cur).next {
        return next.as_ptr();
    }
    'b: while {
        cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
        if cur.is_null() {
            break 'b;
        }
        if let Some(next) = (*cur).next {
            return next.as_ptr();
        }

        !cur.is_null()
    } {}
    cur
}

///  Traverse depth-first through all sibling nodes and their children
///  nodes and concatenate their content. This is an auxiliary function
///  to xmlTextReaderReadString.
///
///  Returns a string containing the content, or NULL in case of error.
#[doc(alias = "xmlTextReaderCollectSiblings")]
#[cfg(feature = "libxml_reader")]
unsafe fn xml_text_reader_collect_siblings(mut node: *mut XmlNode) -> *mut XmlChar {
    use std::ffi::CStr;

    use crate::{libxml::xmlstring::xml_strndup, tree::NodeCommon};

    if node.is_null() || (*node).element_type() == XmlElementType::XmlNamespaceDecl {
        return null_mut();
    }

    let mut buffer = vec![];
    while !node.is_null() {
        match (*node).element_type() {
            XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode => {
                buffer.extend(CStr::from_ptr((*node).content as *const i8).to_bytes());
            }
            XmlElementType::XmlElementNode => {
                let tmp: *mut XmlChar = xml_text_reader_collect_siblings(
                    (*node).children().map_or(null_mut(), |c| c.as_ptr()),
                );
                buffer.extend(CStr::from_ptr(tmp as *const i8).to_bytes());
                xml_free(tmp as _);
            }
            _ => {}
        }

        node = (*node).next.map_or(null_mut(), |n| n.as_ptr());
    }
    xml_strndup(buffer.as_ptr(), buffer.len() as i32)
}

/// Provides the number of attributes of the current node
///
/// Returns 0 i no attributes, -1 in case of error or the attribute count
#[doc(alias = "xmlTextReaderAttributeCount")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_text_reader_attribute_count(reader: &mut XmlTextReader) -> i32 {
    use crate::tree::{NodeCommon, XmlNsPtr};

    let mut ret: i32;

    if reader.node.is_null() {
        return 0;
    }

    let node = if !reader.curnode.is_null() {
        reader.curnode
    } else {
        reader.node
    };

    if (*node).element_type() != XmlElementType::XmlElementNode {
        return 0;
    }
    if matches!(
        reader.state,
        XmlTextReaderState::End | XmlTextReaderState::Backtrack
    ) {
        return 0;
    }
    ret = 0;
    let mut attr = (*node).properties;
    while let Some(now) = attr {
        ret += 1;
        attr = now.next;
    }
    let mut ns = (*node).ns_def;
    while let Some(now) = ns {
        ret += 1;
        ns = XmlNsPtr::from_raw(now.next).unwrap();
    }
    ret
}

/// The base URI of the node.
///
/// Returns the base URI or NULL if not available, the string will be deallocated with the reader
#[doc(alias = "xmlTextReaderConstBaseUri")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_text_reader_const_base_uri(reader: &mut XmlTextReader) -> *const XmlChar {
    use std::ffi::CString;

    use crate::tree::NodeCommon;

    if reader.node.is_null() {
        return null_mut();
    }
    let Some(tmp) = (*reader.node).get_base(None) else {
        return null_mut();
    };
    let tmp = CString::new(tmp).unwrap();
    let ret: *const XmlChar = CONSTSTR!(reader, tmp.as_ptr() as *const u8);
    ret
}

/// The local name of the node.
///
/// Returns the local name or NULL if not available, the string will be deallocated with the reader.
#[doc(alias = "xmlTextReaderConstLocalName")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_text_reader_const_local_name(reader: &mut XmlTextReader) -> *const XmlChar {
    use crate::tree::{NodeCommon, XmlNsPtr};

    if reader.node.is_null() {
        return null_mut();
    }
    let node = if !reader.curnode.is_null() {
        reader.curnode
    } else {
        reader.node
    };
    if (*node).element_type() == XmlElementType::XmlNamespaceDecl {
        let ns = XmlNsPtr::from_raw(node as *mut XmlNs).unwrap().unwrap();
        if ns.prefix.is_null() {
            return CONSTSTR!(reader, c"xmlns".as_ptr() as _);
        } else {
            return ns.prefix;
        }
    }
    if !matches!(
        (*node).element_type(),
        XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode
    ) {
        return xml_text_reader_const_name(reader);
    }
    (*node).name
}

/// The qualified name of the node, equal to Prefix :LocalName.
///
/// Returns the local name or NULL if not available, the string is deallocated with the reader.
#[doc(alias = "xmlTextReaderConstName")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_text_reader_const_name(reader: &mut XmlTextReader) -> *const XmlChar {
    use crate::tree::{NodeCommon, XmlNsPtr};

    if reader.node.is_null() {
        return null_mut();
    }
    let node = if !reader.curnode.is_null() {
        reader.curnode
    } else {
        reader.node
    };
    match (*node).element_type() {
        XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode => {
            let Some(prefix) = (*node).ns.map(|ns| ns.prefix).filter(|p| !p.is_null()) else {
                return (*node).name;
            };
            CONSTQSTR!(reader, prefix, (*node).name)
        }
        XmlElementType::XmlTextNode => CONSTSTR!(reader, c"#text".as_ptr() as _),
        XmlElementType::XmlCDATASectionNode => {
            CONSTSTR!(reader, c"#cdata-section".as_ptr() as _)
        }
        XmlElementType::XmlEntityNode | XmlElementType::XmlEntityRefNode => {
            CONSTSTR!(reader, (*node).name)
        }
        XmlElementType::XmlPINode => CONSTSTR!(reader, (*node).name),
        XmlElementType::XmlCommentNode => CONSTSTR!(reader, c"#comment".as_ptr() as _),
        XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
            CONSTSTR!(reader, c"#document".as_ptr() as _)
        }
        XmlElementType::XmlDocumentFragNode => {
            CONSTSTR!(reader, c"#document-fragment".as_ptr() as _)
        }
        XmlElementType::XmlNotationNode => CONSTSTR!(reader, (*node).name),
        XmlElementType::XmlDocumentTypeNode | XmlElementType::XmlDTDNode => {
            CONSTSTR!(reader, (*node).name)
        }
        XmlElementType::XmlNamespaceDecl => {
            let ns = XmlNsPtr::from_raw(node as *mut XmlNs).unwrap().unwrap();

            if ns.prefix.is_null() {
                return CONSTSTR!(reader, c"xmlns".as_ptr() as _);
            }
            CONSTQSTR!(reader, c"xmlns".as_ptr() as _, ns.prefix)
        }

        XmlElementType::XmlElementDecl
        | XmlElementType::XmlAttributeDecl
        | XmlElementType::XmlEntityDecl
        | XmlElementType::XmlXIncludeStart
        | XmlElementType::XmlXIncludeEnd => null_mut(),
        _ => unreachable!(),
    }
    // return null_mut();
}

/// The URI defining the namespace associated with the node.
///
/// Returns the namespace URI or NULL if not available, the string will be deallocated with the reader
#[doc(alias = "xmlTextReaderConstNamespaceUri")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_text_reader_const_namespace_uri(reader: &mut XmlTextReader) -> *const XmlChar {
    use crate::tree::NodeCommon;

    if reader.node.is_null() {
        return null_mut();
    }
    let node = if !reader.curnode.is_null() {
        reader.curnode
    } else {
        reader.node
    };
    if (*node).element_type() == XmlElementType::XmlNamespaceDecl {
        return CONSTSTR!(reader, c"http://www.w3.org/2000/xmlns/".as_ptr() as _);
    }
    if !matches!(
        (*node).element_type(),
        XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode
    ) {
        return null_mut();
    }
    if let Some(ns) = (*node).ns {
        return CONSTSTR!(reader, ns.href);
    }
    null_mut()
}

/// A shorthand reference to the namespace associated with the node.
///
/// Returns the prefix or NULL if not available, the string is deallocated with the reader.
#[doc(alias = "xmlTextReaderConstPrefix")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_text_reader_const_prefix(reader: &mut XmlTextReader) -> *const XmlChar {
    use crate::tree::{NodeCommon, XmlNsPtr};

    if reader.node.is_null() {
        return null_mut();
    }
    let node = if !reader.curnode.is_null() {
        reader.curnode
    } else {
        reader.node
    };
    if (*node).element_type() == XmlElementType::XmlNamespaceDecl {
        let ns = XmlNsPtr::from_raw(node as *mut XmlNs).unwrap().unwrap();
        if ns.prefix().is_none() {
            return null_mut();
        }
        return CONSTSTR!(reader, c"xmlns".as_ptr() as _);
    }
    if !matches!(
        (*node).element_type(),
        XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode
    ) {
        return null_mut();
    }
    if let Some(prefix) = (*node).ns.map(|ns| ns.prefix).filter(|p| !p.is_null()) {
        return CONSTSTR!(reader, prefix);
    }
    null_mut()
}

/// The xml:lang scope within which the node resides.
///
/// Returns the xml:lang value or NULL if none exists.
#[doc(alias = "xmlTextReaderConstXmlLang")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_text_reader_const_xml_lang(reader: &mut XmlTextReader) -> *const XmlChar {
    use std::ffi::CString;

    if reader.node.is_null() {
        return null_mut();
    }
    let Some(tmp) = (*reader.node).get_lang() else {
        return null_mut();
    };
    let tmp = CString::new(tmp).unwrap();
    let ret: *const XmlChar = CONSTSTR!(reader, tmp.as_ptr() as *const u8);
    ret
}

/// Get an interned string from the reader, allows for example to
/// speedup string name comparisons
///
/// Returns an interned copy of the string or NULL in case of error.
/// The string will be deallocated with the reader.
#[doc(alias = "xmlTextReaderConstString")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_text_reader_const_string(
    reader: &mut XmlTextReader,
    str: *const XmlChar,
) -> *const XmlChar {
    CONSTSTR!(reader, str)
}

/// Provides the text value of the node if present
///
/// Returns the string or NULL if not available.
/// The result will be deallocated on the next Read() operation.
#[doc(alias = "xmlTextReaderConstValue")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_text_reader_const_value(reader: &mut XmlTextReader) -> *const XmlChar {
    use crate::tree::{NodeCommon, XmlAttr, XmlNsPtr};

    if reader.node.is_null() {
        return null_mut();
    }
    let node = if !reader.curnode.is_null() {
        reader.curnode
    } else {
        reader.node
    };

    match (*node).element_type() {
        XmlElementType::XmlNamespaceDecl => {
            return XmlNsPtr::from_raw(node as *mut XmlNs)
                .unwrap()
                .unwrap()
                .href
        }
        XmlElementType::XmlAttributeNode => {
            let attr = XmlAttrPtr::from_raw(node as *mut XmlAttr).unwrap().unwrap();

            if let Some(children) = attr
                .children
                .filter(|c| c.element_type() == XmlElementType::XmlTextNode && c.next.is_none())
            {
                return children.content;
            } else {
                reader.buffer.clear();
                (*node).get_content_to(&mut reader.buffer);
                // temporary workaround for NULL terminated string
                reader.buffer.push('\0');
                return reader.buffer.as_ptr();
            }
        }
        XmlElementType::XmlTextNode
        | XmlElementType::XmlCDATASectionNode
        | XmlElementType::XmlPINode
        | XmlElementType::XmlCommentNode => return (*node).content,
        _ => {}
    }
    null_mut()
}

/// Free up all the structures used by a document, tree included.
#[doc(alias = "xmlTextReaderFreeDoc")]
#[cfg(feature = "libxml_reader")]
unsafe fn xml_text_reader_free_doc(reader: &mut XmlTextReader, mut cur: XmlDocPtr) {
    use crate::tree::NodeCommon;

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0 {
        // if let Some(f) = xmlDeregisterNodeDefaultValue {
        //     f(cur as _);
        // }
        xml_deregister_node_default_value(cur.as_ptr() as _);
    }

    // Do this before freeing the children list to avoid ID lookups
    cur.ids.take();
    cur.refs.take();
    let mut ext_subset = cur.ext_subset.take();
    let int_subset = cur.int_subset.take();
    if int_subset == ext_subset {
        ext_subset = None;
    }
    if let Some(mut ext_subset) = ext_subset {
        ext_subset.unlink();
        xml_free_dtd(ext_subset);
    }
    if let Some(mut int_subset) = int_subset {
        int_subset.unlink();
        xml_free_dtd(int_subset);
    }

    if let Some(children) = cur.children {
        xml_text_reader_free_node_list(reader, children.as_ptr());
    }

    cur.version = None;
    if !cur.name.is_null() {
        xml_free(cur.name as _);
    }
    cur.encoding = None;
    if let Some(old_ns) = cur.old_ns.take() {
        xml_free_ns_list(old_ns);
    }
    cur.url = None;
    cur.free();
}

/// This method releases any resources allocated by the current instance
/// changes the state to Closed and close any underlying input.
///
/// Returns 0 or -1 in case of error
#[doc(alias = "xmlTextReaderClose")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_text_reader_close(reader: &mut XmlTextReader) -> i32 {
    reader.node = null_mut();
    reader.curnode = null_mut();
    reader.mode = XmlTextReaderMode::XmlTextreaderModeClosed;
    if !reader.faketext.is_null() {
        xml_free_node(reader.faketext);
        reader.faketext = null_mut();
    }
    if !reader.ctxt.is_null() {
        #[cfg(all(feature = "libxml_regexp", feature = "libxml_valid"))]
        while !(*reader.ctxt).vctxt.vstate_tab.is_empty() {
            xml_validate_pop_element(addr_of_mut!((*reader.ctxt).vctxt), None, None, null_mut());
        }
        (*reader.ctxt).vctxt.vstate_tab.clear();
        (*reader.ctxt).stop();
        if let Some(my_doc) = (*reader.ctxt).my_doc.take() {
            if reader.preserve == 0 {
                xml_text_reader_free_doc(reader, my_doc);
            }
        }
    }
    if reader.input.is_some() && reader.allocs & XML_TEXTREADER_INPUT != 0 {
        let _ = reader.input.take();
        reader.allocs -= XML_TEXTREADER_INPUT;
    }
    0
}

/// Resolves a namespace prefix in the scope of the current element.
///
/// Returns a string containing the namespace URI to which the prefix maps or NULL in case of error.  
/// The string must be deallocated by the caller.
#[doc(alias = "xmlTextReaderLookupNamespace")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_text_reader_lookup_namespace(
    reader: &mut XmlTextReader,
    prefix: *const XmlChar,
) -> *mut XmlChar {
    use std::ffi::CStr;

    if reader.node.is_null() {
        return null_mut();
    }

    let Some(ns) = (*reader.node).search_ns(
        (*reader.node).doc,
        (!prefix.is_null())
            .then(|| CStr::from_ptr(prefix as *const i8).to_string_lossy())
            .as_deref(),
    ) else {
        return null_mut();
    };
    xml_strdup(ns.href)
}

/// Determine the encoding of the document being read.
///
/// Returns a string containing the encoding of the document or NULL in case of error.  
/// The string is deallocated with the reader.
#[doc(alias = "xmlTextReaderConstEncoding")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_text_reader_const_encoding(reader: &mut XmlTextReader) -> *const XmlChar {
    use std::ffi::CString;

    let Some(doc) = reader.doc.or_else(|| {
        (!reader.ctxt.is_null())
            .then(|| (*reader.ctxt).my_doc)
            .flatten()
    }) else {
        return null_mut();
    };

    if let Some(encoding) = doc.encoding.as_deref() {
        let encoding = CString::new(encoding).unwrap();
        CONSTSTR!(reader, encoding.as_ptr() as *const u8)
    } else {
        null_mut()
    }
}

/// Change the parser processing behaviour by changing some of its internal
/// properties. Note that some properties can only be changed before any read has been done.
///
/// Returns 0 if the call was successful, or -1 in case of error
#[doc(alias = "xmlTextReaderSetParserProp")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_text_reader_set_parser_prop(
    reader: &mut XmlTextReader,
    prop: i32,
    value: i32,
) -> i32 {
    if reader.ctxt.is_null() {
        return -1;
    }
    let ctxt: XmlParserCtxtPtr = reader.ctxt;

    match XmlParserProperties::try_from(prop) {
        Ok(XmlParserProperties::XmlParserLoadDTD) => {
            if value != 0 {
                if (*ctxt).loadsubset == 0 {
                    if reader.mode != XmlTextReaderMode::XmlTextreaderModeInitial {
                        return -1;
                    }
                    (*ctxt).loadsubset = XML_DETECT_IDS as i32;
                }
            } else {
                (*ctxt).loadsubset = 0;
            }
            0
        }
        Ok(XmlParserProperties::XmlParserDefaultAttrs) => {
            if value != 0 {
                (*ctxt).loadsubset |= XML_COMPLETE_ATTRS as i32;
            } else if (*ctxt).loadsubset & XML_COMPLETE_ATTRS as i32 != 0 {
                (*ctxt).loadsubset -= XML_COMPLETE_ATTRS as i32;
            }
            0
        }
        Ok(XmlParserProperties::XmlParserValidate) => {
            if value != 0 {
                (*ctxt).options |= XmlParserOption::XmlParseDTDValid as i32;
                (*ctxt).validate = 1;
                reader.validate = XmlTextReaderValidate::ValidateDtd;
            } else {
                (*ctxt).options &= !(XmlParserOption::XmlParseDTDValid as i32);
                (*ctxt).validate = 0;
            }
            0
        }
        Ok(XmlParserProperties::XmlParserSubstEntities) => {
            if value != 0 {
                (*ctxt).options |= XmlParserOption::XmlParseNoEnt as i32;
                (*ctxt).replace_entities = 1;
            } else {
                (*ctxt).options &= !(XmlParserOption::XmlParseNoEnt as i32);
                (*ctxt).replace_entities = 0;
            }
            0
        }
        _ => -1,
    }
}

#[cfg(feature = "libxml_reader")]
fn xml_text_reader_generic_error(
    ctxt: Option<GenericErrorContext>,
    severity: XmlParserSeverities,
    str: &str,
) {
    use std::ffi::CString;

    if let Some(ctxt) = ctxt {
        let lock = ctxt.context.lock().unwrap();
        let ctx = **lock
            .downcast_ref::<Box<XmlParserCtxtPtr>>()
            .expect("ctxt is not XmlParserCtxtPtr");

        unsafe {
            let reader: XmlTextReaderPtr = (*ctx)._private as XmlTextReaderPtr;

            if let Some(error) = (*reader).error_func {
                let str = CString::new(str).unwrap();
                error(
                    (*reader).error_func_arg.clone(),
                    str.as_ptr(),
                    severity,
                    ctx as XmlTextReaderLocatorPtr,
                );
            }
            // In the original code, `str` is dynamic memory allocated to construct strings from variable-length arguments,
            // but since variable-length arguments are not available in Rust, it is just a lateral pass of the passed message.
            //
            // Since we do not know where the memory of `str` should be handled, it is safe not to free it,
            // although we expect it to leak memory.
            // xml_free(str as _);
        }
    }
}

#[cfg(feature = "libxml_reader")]
fn xml_text_reader_validity_error(ctxt: Option<GenericErrorContext>, msg: &str) {
    let len = msg.len();

    if len > 1 && msg.as_bytes()[len - 2] != b':' {
        /*
         * some callbacks only report locator information:
         * skip them (mimicking behaviour in error.c)
         */
        xml_text_reader_generic_error(
            ctxt,
            XmlParserSeverities::XmlParserSeverityValidityError,
            msg,
        );
    }

    // original code is the following, but Rust cannot handle variable length arguments.

    // va_list ap;

    // len: i32 = xmlStrlen(msg);

    // if ((len > 1) && (msg[len - 2] != ':')) {
    //     /*
    //      * some callbacks only report locator information:
    //      * skip them (mimicking behaviour in error.c)
    //      */
    //     va_start(ap, msg);
    //     xmlTextReaderGenericError(ctxt,
    //                               XML_PARSER_SEVERITY_VALIDITY_ERROR,
    //                               xmlTextReaderBuildMessage(msg, ap));
    //     va_end(ap);
    // }
}

#[cfg(all(feature = "libxml_reader", feature = "schema"))]
fn xml_text_reader_validity_error_relay(ctx: Option<GenericErrorContext>, msg: &str) {
    use std::ffi::CString;

    if let Some(ctx) = ctx {
        let lock = ctx.context.lock().unwrap();
        let reader = **lock
            .downcast_ref::<Box<XmlTextReaderPtr>>()
            .expect("ctxt is not XmlTextReaderPtr");

        unsafe {
            if let Some(error) = (*reader).error_func {
                let msg = CString::new(msg).unwrap();
                error(
                    (*reader).error_func_arg.clone(),
                    msg.as_ptr(),
                    XmlParserSeverities::XmlParserSeverityValidityError,
                    null_mut(), /* locator */
                );
            } else {
                drop(lock);
                xml_text_reader_validity_error(Some(ctx), msg);
            }
        }
    }

    // original code is the following, but Rust cannot handle variable length arguments.

    // let reader: xmlTextReaderPtr = ctx as xmlTextReaderPtr;

    // let str: *mut c_char;

    // va_list ap;

    // va_start(ap, msg);
    // str = xmlTextReaderBuildMessage(msg, ap);
    // if (!(*reader).errorFunc) {
    //     xmlTextReaderValidityError(ctx, c"%s", str);
    // } else {
    //     (*reader).errorFunc((*reader).errorFuncArg, str,
    //                       XML_PARSER_SEVERITY_VALIDITY_ERROR,
    //                       NULL /* locator */ );
    // }
    // if !str.is_null() {
    //     xml_free(str as _);
    // }
    // va_end(ap);
}

#[cfg(feature = "libxml_reader")]
fn xml_text_reader_validity_warning(ctxt: Option<GenericErrorContext>, msg: &str) {
    let len = msg.len();

    if len != 0 && msg.as_bytes()[len - 1] != b':' {
        /*
         * some callbacks only report locator information:
         * skip them (mimicking behaviour in error.c)
         */
        xml_text_reader_generic_error(
            ctxt,
            XmlParserSeverities::XmlParserSeverityValidityWarning,
            msg as _,
        );
    }

    // original code is the following, but Rust cannot handle variable length arguments.

    // va_list ap;

    // len: i32 = xmlStrlen(msg);

    // if ((len != 0) && (msg[len - 1] != ':')) {
    //     /*
    //      * some callbacks only report locator information:
    //      * skip them (mimicking behaviour in error.c)
    //      */
    //     va_start(ap, msg);
    //     xmlTextReaderGenericError(ctxt,
    //                               XML_PARSER_SEVERITY_VALIDITY_WARNING,
    //                               xmlTextReaderBuildMessage(msg, ap));
    //     va_end(ap);
    // }
}

#[cfg(all(feature = "libxml_reader", feature = "schema"))]
fn xml_text_reader_validity_warning_relay(ctx: Option<GenericErrorContext>, msg: &str) {
    use std::ffi::CString;

    if let Some(ctx) = ctx {
        let lock = ctx.context.lock().unwrap();
        let reader = **lock
            .downcast_ref::<Box<XmlTextReaderPtr>>()
            .expect("ctxt is not XmlTextReaderPtr");

        unsafe {
            if let Some(error) = (*reader).error_func {
                let msg = CString::new(msg).unwrap();
                error(
                    (*reader).error_func_arg.clone(),
                    msg.as_ptr(),
                    XmlParserSeverities::XmlParserSeverityValidityWarning,
                    null_mut(), /* locator */
                );
            } else {
                drop(lock);
                xml_text_reader_validity_warning(Some(ctx), msg);
            }
        }
    }

    // original code is the following, but Rust cannot handle variable length arguments.

    // let reader: xmlTextReaderPtr = ctx as xmlTextReaderPtr;

    // let str: *mut c_char;

    // va_list ap;

    // va_start(ap, msg);
    // str = xmlTextReaderBuildMessage(msg, ap);
    // if (!(*reader).errorFunc) {
    //     xmlTextReaderValidityWarning(ctx, c"%s", str);
    // } else {
    //     (*reader).errorFunc((*reader).errorFuncArg, str,
    //                       XML_PARSER_SEVERITY_VALIDITY_WARNING,
    //                       NULL /* locator */ );
    // }
    // if !str.is_null() {
    //     xml_free(str as _);
    // }
    // va_end(ap);
}

#[cfg(feature = "libxml_reader")]
fn xml_text_reader_structured_error(ctxt: Option<GenericErrorContext>, error: &XmlError) {
    let ctx = ctxt
        .and_then(|c| {
            let lock = c.lock();
            lock.downcast_ref::<XmlParserCtxtPtr>().copied()
        })
        .unwrap_or(null_mut());

    unsafe {
        let reader: XmlTextReaderPtr = (*ctx)._private as XmlTextReaderPtr;

        // if !error.is_null() {
        if let Some(serror) = (*reader).serror_func {
            serror((*reader).error_func_arg.clone(), error);
        }
        // }
    }
}

#[cfg(all(feature = "libxml_reader", feature = "schema"))]
fn xml_text_reader_validity_structured_relay(
    user_data: Option<GenericErrorContext>,
    error: &XmlError,
) {
    let reader = user_data
        .as_ref()
        .and_then(|c| {
            let lock = c.lock();
            lock.downcast_ref::<XmlTextReaderPtr>().copied()
        })
        .unwrap_or(null_mut());

    unsafe {
        if let Some(serror) = (*reader).serror_func {
            serror((*reader).error_func_arg.clone(), error);
        } else {
            xml_text_reader_structured_error(user_data, error);
        }
    }
}

/// Use RelaxNG to validate the document as it is processed.
/// Activation is only possible before the first Read().
/// If both @rng and @ctxt are NULL, then RelaxNG validation is deactivated.
///
/// Returns 0 in case the RelaxNG validation could be (de)activated and -1 in case of error.
#[doc(alias = "xmlTextReaderRelaxNGValidateInternal")]
#[cfg(all(feature = "libxml_reader", feature = "schema"))]
unsafe fn xml_text_reader_relaxng_validate_internal(
    reader: XmlTextReaderPtr,
    rng: *const c_char,
    ctxt: XmlRelaxNGValidCtxtPtr,
    _options: i32,
) -> i32 {
    use std::ffi::CStr;

    use crate::relaxng::{
        xml_relaxng_free_parser_ctxt, xml_relaxng_free_valid_ctxt, xml_relaxng_new_valid_ctxt,
    };

    if reader.is_null() {
        return -1;
    }

    if !rng.is_null() && !ctxt.is_null() {
        return -1;
    }

    if (!rng.is_null() || !ctxt.is_null())
        && ((*reader).mode != XmlTextReaderMode::XmlTextreaderModeInitial
            || (*reader).ctxt.is_null())
    {
        return -1;
    }

    // Cleanup previous validation stuff.
    if !(*reader).rng_valid_ctxt.is_null() {
        if (*reader).rng_preserve_ctxt == 0 {
            xml_relaxng_free_valid_ctxt((*reader).rng_valid_ctxt);
        }
        (*reader).rng_valid_ctxt = null_mut();
    }
    (*reader).rng_preserve_ctxt = 0;
    if !(*reader).rng_schemas.is_null() {
        xml_relaxng_free((*reader).rng_schemas);
        (*reader).rng_schemas = null_mut();
    }

    if rng.is_null() && ctxt.is_null() {
        // We just want to deactivate the validation, so get out.
        return 0;
    }

    if !rng.is_null() {
        // Parse the schema and create validation environment.

        let pctxt = xml_relaxng_new_parser_ctxt(CStr::from_ptr(rng).to_string_lossy().as_ref());
        let ctx = GenericErrorContext::new(reader);
        if (*reader).error_func.is_some() {
            (*pctxt).set_parser_errors(
                Some(xml_text_reader_validity_error_relay),
                Some(xml_text_reader_validity_warning_relay),
                Some(ctx.clone()),
            );
        }
        if (*reader).serror_func.is_some() {
            xml_relaxng_set_valid_structured_errors(
                (*reader).rng_valid_ctxt,
                Some(xml_text_reader_validity_structured_relay),
                Some(ctx),
            );
        }
        (*reader).rng_schemas = xml_relaxng_parse(pctxt);
        xml_relaxng_free_parser_ctxt(pctxt);
        if (*reader).rng_schemas.is_null() {
            return -1;
        }
        (*reader).rng_valid_ctxt = xml_relaxng_new_valid_ctxt((*reader).rng_schemas);
        if (*reader).rng_valid_ctxt.is_null() {
            xml_relaxng_free((*reader).rng_schemas);
            (*reader).rng_schemas = null_mut();
            return -1;
        }
    } else {
        // Use the given validation context.
        (*reader).rng_valid_ctxt = ctxt;
        (*reader).rng_preserve_ctxt = 1;
    }
    // Redirect the validation context's error channels to use
    // the reader channels.
    // TODO: In case the user provides the validation context we
    //    could make this redirection optional.
    let ctx = GenericErrorContext::new(reader);
    if (*reader).error_func.is_some() {
        xml_relaxng_set_valid_errors(
            (*reader).rng_valid_ctxt,
            Some(xml_text_reader_validity_error_relay),
            Some(xml_text_reader_validity_warning_relay),
            Some(ctx.clone()),
        );
    }
    if (*reader).serror_func.is_some() {
        xml_relaxng_set_valid_structured_errors(
            (*reader).rng_valid_ctxt,
            Some(xml_text_reader_validity_structured_relay),
            Some(ctx),
        );
    }
    (*reader).rng_valid_errors = 0;
    (*reader).rng_full_node = null_mut();
    (*reader).validate = XmlTextReaderValidate::ValidateRng;
    0
}

/// Use RelaxNG schema to validate the document as it is processed.
/// Activation is only possible before the first Read().
/// If @rng is NULL, then RelaxNG schema validation is deactivated.
///
/// Returns 0 in case the schemas validation could be (de)activated and -1 in case of error.
#[doc(alias = "xmlTextReaderRelaxNGValidate")]
#[cfg(all(feature = "libxml_reader", feature = "schema"))]
pub unsafe fn xml_text_reader_relaxng_validate(
    reader: XmlTextReaderPtr,
    rng: *const c_char,
) -> i32 {
    xml_text_reader_relaxng_validate_internal(reader, rng, null_mut(), 0)
}

/// Use RelaxNG schema context to validate the document as it is processed.
/// Activation is only possible before the first Read().
/// If @ctxt is NULL, then RelaxNG schema validation is deactivated.
///
/// Returns 0 in case the schemas validation could be (de)activated and -1 in case of error.
#[doc(alias = "xmlTextReaderRelaxNGValidateCtxt")]
#[cfg(all(feature = "libxml_reader", feature = "schema"))]
pub unsafe fn xml_text_reader_relaxng_validate_ctxt(
    reader: XmlTextReaderPtr,
    ctxt: XmlRelaxNGValidCtxtPtr,
    options: i32,
) -> i32 {
    xml_text_reader_relaxng_validate_internal(reader, null_mut(), ctxt, options)
}

/// Use RelaxNG to validate the document as it is processed.
/// Activation is only possible before the first Read().
/// if @schema is NULL, then RelaxNG validation is deactivated.
/// The @schema should not be freed until the reader is deallocated
/// or its use has been deactivated.
///
/// Returns 0 in case the RelaxNG validation could be (de)activated and -1 in case of error.
#[doc(alias = "xmlTextReaderRelaxNGSetSchema")]
#[cfg(all(feature = "libxml_reader", feature = "schema"))]
pub unsafe fn xml_text_reader_relaxng_set_schema(
    reader: XmlTextReaderPtr,
    schema: XmlRelaxNGPtr,
) -> i32 {
    use crate::relaxng::{xml_relaxng_free_valid_ctxt, xml_relaxng_new_valid_ctxt};

    if reader.is_null() {
        return -1;
    }
    if schema.is_null() {
        if !(*reader).rng_schemas.is_null() {
            xml_relaxng_free((*reader).rng_schemas);
            (*reader).rng_schemas = null_mut();
        }
        if !(*reader).rng_valid_ctxt.is_null() {
            if (*reader).rng_preserve_ctxt == 0 {
                xml_relaxng_free_valid_ctxt((*reader).rng_valid_ctxt);
            }
            (*reader).rng_valid_ctxt = null_mut();
        }
        (*reader).rng_preserve_ctxt = 0;
        return 0;
    }
    if (*reader).mode != XmlTextReaderMode::XmlTextreaderModeInitial {
        return -1;
    }
    if !(*reader).rng_schemas.is_null() {
        xml_relaxng_free((*reader).rng_schemas);
        (*reader).rng_schemas = null_mut();
    }
    if !(*reader).rng_valid_ctxt.is_null() {
        if (*reader).rng_preserve_ctxt == 0 {
            xml_relaxng_free_valid_ctxt((*reader).rng_valid_ctxt);
        }
        (*reader).rng_valid_ctxt = null_mut();
    }
    (*reader).rng_preserve_ctxt = 0;
    (*reader).rng_valid_ctxt = xml_relaxng_new_valid_ctxt(schema);
    if (*reader).rng_valid_ctxt.is_null() {
        return -1;
    }
    let ctx = GenericErrorContext::new(reader);
    if (*reader).error_func.is_some() {
        xml_relaxng_set_valid_errors(
            (*reader).rng_valid_ctxt,
            Some(xml_text_reader_validity_error_relay),
            Some(xml_text_reader_validity_warning_relay),
            Some(ctx.clone()),
        );
    }
    if (*reader).serror_func.is_some() {
        xml_relaxng_set_valid_structured_errors(
            (*reader).rng_valid_ctxt,
            Some(xml_text_reader_validity_structured_relay),
            Some(ctx),
        );
    }
    (*reader).rng_valid_errors = 0;
    (*reader).rng_full_node = null_mut();
    (*reader).validate = XmlTextReaderValidate::ValidateRng;
    0
}

/// Internal locator function for the readers
///
/// Returns 0 in case the Schema validation could be (de)activated and -1 in case of error.
#[doc(alias = "xmlTextReaderLocator")]
#[cfg(all(feature = "libxml_reader", feature = "schema"))]
unsafe fn xml_text_reader_locator(
    ctx: *mut c_void,
    file: *mut Option<String>,
    line: *mut u64,
) -> i32 {
    if ctx.is_null() || (file.is_null() && line.is_null()) {
        return -1;
    }

    if !file.is_null() {
        *file = None;
    }
    if !line.is_null() {
        *line = 0;
    }

    let reader: XmlTextReaderPtr = ctx as XmlTextReaderPtr;
    if !(*reader).ctxt.is_null() && !(*(*reader).ctxt).input.is_null() {
        if !file.is_null() {
            *file = (*(*(*reader).ctxt).input).filename.clone();
        }
        if !line.is_null() {
            *line = (*(*(*reader).ctxt).input).line as _;
        }
        return 0;
    }
    if !(*reader).node.is_null() {
        let res: i64;
        let mut ret: i32 = 0;

        if !line.is_null() {
            res = (*(*reader).node).get_line_no();
            if res > 0 {
                *line = res as u64;
            } else {
                ret = -1;
            }
        }
        if !file.is_null() {
            if let Some(url) = (*(*reader).node)
                .doc
                .as_deref()
                .and_then(|doc| doc.url.as_deref())
            {
                *file = Some(url.to_owned());
            } else {
                ret = -1;
            }
        }
        return ret;
    }
    -1
}

/// Validate the document as it is processed using XML Schema.
/// Activation is only possible before the first Read().
/// If both @xsd and @ctxt are NULL then XML Schema validation is deactivated.
///
/// Returns 0 in case the schemas validation could be (de)activated and -1 in case of error.
#[doc(alias = "xmlTextReaderSchemaValidateInternal")]
#[cfg(all(feature = "libxml_reader", feature = "schema"))]
unsafe fn xml_text_reader_schema_validate_internal(
    reader: XmlTextReaderPtr,
    xsd: *const c_char,
    ctxt: XmlSchemaValidCtxtPtr,
    _options: i32,
) -> i32 {
    if reader.is_null() {
        return -1;
    }

    if !xsd.is_null() && !ctxt.is_null() {
        return -1;
    }

    if (!xsd.is_null() || !ctxt.is_null())
        && ((*reader).mode != XmlTextReaderMode::XmlTextreaderModeInitial
            || (*reader).ctxt.is_null())
    {
        return -1;
    }

    // Cleanup previous validation stuff.
    if !(*reader).xsd_plug.is_null() {
        xml_schema_sax_unplug((*reader).xsd_plug);
        (*reader).xsd_plug = null_mut();
    }
    if !(*reader).xsd_valid_ctxt.is_null() {
        if (*reader).xsd_preserve_ctxt == 0 {
            xml_schema_free_valid_ctxt((*reader).xsd_valid_ctxt);
        }
        (*reader).xsd_valid_ctxt = null_mut();
    }
    (*reader).xsd_preserve_ctxt = 0;
    if !(*reader).xsd_schemas.is_null() {
        xml_schema_free((*reader).xsd_schemas);
        (*reader).xsd_schemas = null_mut();
    }

    if xsd.is_null() && ctxt.is_null() {
        // We just want to deactivate the validation, so get out.
        return 0;
    }

    let ctx = GenericErrorContext::new(reader);
    if !xsd.is_null() {
        // Parse the schema and create validation environment.
        let pctxt: XmlSchemaParserCtxtPtr = xml_schema_new_parser_ctxt(xsd);
        if (*reader).error_func.is_some() {
            xml_schema_set_parser_errors(
                pctxt,
                Some(xml_text_reader_validity_error_relay),
                Some(xml_text_reader_validity_warning_relay),
                Some(ctx.clone()),
            );
        }
        (*reader).xsd_schemas = xml_schema_parse(pctxt);
        xml_schema_free_parser_ctxt(pctxt);
        if (*reader).xsd_schemas.is_null() {
            return -1;
        }
        (*reader).xsd_valid_ctxt = xml_schema_new_valid_ctxt((*reader).xsd_schemas);
        if (*reader).xsd_valid_ctxt.is_null() {
            xml_schema_free((*reader).xsd_schemas);
            (*reader).xsd_schemas = null_mut();
            return -1;
        }
        (*reader).xsd_plug = xml_schema_sax_plug(
            (*reader).xsd_valid_ctxt,
            &mut (*(*reader).ctxt).sax,
            addr_of_mut!((*(*reader).ctxt).user_data),
        );
        if (*reader).xsd_plug.is_null() {
            xml_schema_free((*reader).xsd_schemas);
            (*reader).xsd_schemas = null_mut();
            xml_schema_free_valid_ctxt((*reader).xsd_valid_ctxt);
            (*reader).xsd_valid_ctxt = null_mut();
            return -1;
        }
    } else {
        // Use the given validation context.
        (*reader).xsd_valid_ctxt = ctxt;
        (*reader).xsd_preserve_ctxt = 1;
        (*reader).xsd_plug = xml_schema_sax_plug(
            (*reader).xsd_valid_ctxt,
            &mut (*(*reader).ctxt).sax,
            addr_of_mut!((*(*reader).ctxt).user_data),
        );
        if (*reader).xsd_plug.is_null() {
            (*reader).xsd_valid_ctxt = null_mut();
            (*reader).xsd_preserve_ctxt = 0;
            return -1;
        }
    }
    xml_schema_validate_set_locator(
        (*reader).xsd_valid_ctxt,
        Some(xml_text_reader_locator),
        reader as *mut c_void,
    );
    // Redirect the validation context's error channels to use
    // the reader channels.
    // TODO: In case the user provides the validation context we
    //   could make this redirection optional.
    if (*reader).error_func.is_some() {
        xml_schema_set_valid_errors(
            (*reader).xsd_valid_ctxt,
            Some(xml_text_reader_validity_error_relay),
            Some(xml_text_reader_validity_warning_relay),
            Some(ctx.clone()),
        );
    }
    if (*reader).serror_func.is_some() {
        xml_schema_set_valid_structured_errors(
            (*reader).xsd_valid_ctxt,
            Some(xml_text_reader_validity_structured_relay),
            Some(ctx.clone()),
        );
    }
    (*reader).xsd_valid_errors = 0;
    (*reader).validate = XmlTextReaderValidate::ValidateXsd;
    0
}

/// Use W3C XSD schema to validate the document as it is processed.
/// Activation is only possible before the first Read().
/// If @xsd is NULL, then XML Schema validation is deactivated.
///
/// Returns 0 in case the schemas validation could be (de)activated and -1 in case of error.
#[doc(alias = "xmlTextReaderSchemaValidate")]
#[cfg(all(feature = "libxml_reader", feature = "schema"))]
pub unsafe fn xml_text_reader_schema_validate(reader: XmlTextReaderPtr, xsd: *const c_char) -> i32 {
    xml_text_reader_schema_validate_internal(reader, xsd, null_mut(), 0)
}

/// Use W3C XSD schema context to validate the document as it is processed.
/// Activation is only possible before the first Read().
/// If @ctxt is NULL, then XML Schema validation is deactivated.
///
/// Returns 0 in case the schemas validation could be (de)activated and -1 in case of error.
#[doc(alias = "xmlTextReaderSchemaValidateCtxt")]
#[cfg(all(feature = "libxml_reader", feature = "schema"))]
pub unsafe fn xml_text_reader_schema_validate_ctxt(
    reader: XmlTextReaderPtr,
    ctxt: XmlSchemaValidCtxtPtr,
    options: i32,
) -> i32 {
    xml_text_reader_schema_validate_internal(reader, null_mut(), ctxt, options)
}

/// Use XSD Schema to validate the document as it is processed.
/// Activation is only possible before the first Read().
/// if @schema is NULL, then Schema validation is deactivated.
/// The @schema should not be freed until the reader is deallocated
/// or its use has been deactivated.
///
/// Returns 0 in case the Schema validation could be (de)activated and -1 in case of error.
#[doc(alias = "xmlTextReaderSetSchema")]
#[cfg(all(feature = "libxml_reader", feature = "schema"))]
pub unsafe fn xml_text_reader_set_schema(reader: XmlTextReaderPtr, schema: XmlSchemaPtr) -> i32 {
    if reader.is_null() {
        return -1;
    }
    if schema.is_null() {
        if !(*reader).xsd_plug.is_null() {
            xml_schema_sax_unplug((*reader).xsd_plug);
            (*reader).xsd_plug = null_mut();
        }
        if !(*reader).xsd_valid_ctxt.is_null() {
            if (*reader).xsd_preserve_ctxt == 0 {
                xml_schema_free_valid_ctxt((*reader).xsd_valid_ctxt);
            }
            (*reader).xsd_valid_ctxt = null_mut();
        }
        (*reader).xsd_preserve_ctxt = 0;
        if !(*reader).xsd_schemas.is_null() {
            xml_schema_free((*reader).xsd_schemas);
            (*reader).xsd_schemas = null_mut();
        }
        return 0;
    }
    if (*reader).mode != XmlTextReaderMode::XmlTextreaderModeInitial {
        return -1;
    }
    if !(*reader).xsd_plug.is_null() {
        xml_schema_sax_unplug((*reader).xsd_plug);
        (*reader).xsd_plug = null_mut();
    }
    if !(*reader).xsd_valid_ctxt.is_null() {
        if (*reader).xsd_preserve_ctxt == 0 {
            xml_schema_free_valid_ctxt((*reader).xsd_valid_ctxt);
        }
        (*reader).xsd_valid_ctxt = null_mut();
    }
    (*reader).xsd_preserve_ctxt = 0;
    if !(*reader).xsd_schemas.is_null() {
        xml_schema_free((*reader).xsd_schemas);
        (*reader).xsd_schemas = null_mut();
    }
    (*reader).xsd_valid_ctxt = xml_schema_new_valid_ctxt(schema);
    if (*reader).xsd_valid_ctxt.is_null() {
        xml_schema_free((*reader).xsd_schemas);
        (*reader).xsd_schemas = null_mut();
        return -1;
    }
    (*reader).xsd_plug = xml_schema_sax_plug(
        (*reader).xsd_valid_ctxt,
        &mut (*(*reader).ctxt).sax,
        addr_of_mut!((*(*reader).ctxt).user_data),
    );
    if (*reader).xsd_plug.is_null() {
        xml_schema_free((*reader).xsd_schemas);
        (*reader).xsd_schemas = null_mut();
        xml_schema_free_valid_ctxt((*reader).xsd_valid_ctxt);
        (*reader).xsd_valid_ctxt = null_mut();
        return -1;
    }
    xml_schema_validate_set_locator(
        (*reader).xsd_valid_ctxt,
        Some(xml_text_reader_locator),
        reader as *mut c_void,
    );

    let ctx = GenericErrorContext::new(reader);
    if (*reader).error_func.is_some() {
        xml_schema_set_valid_errors(
            (*reader).xsd_valid_ctxt,
            Some(xml_text_reader_validity_error_relay),
            Some(xml_text_reader_validity_warning_relay),
            Some(ctx.clone()),
        );
    }
    if (*reader).serror_func.is_some() {
        xml_schema_set_valid_structured_errors(
            (*reader).xsd_valid_ctxt,
            Some(xml_text_reader_validity_structured_relay),
            Some(ctx.clone()),
        );
    }
    (*reader).xsd_valid_errors = 0;
    (*reader).validate = XmlTextReaderValidate::ValidateXsd;
    0
}

/// Determine the XML version of the document being read.
///
/// Returns a string containing the XML version of the document or NULL in case of error.  
/// The string is deallocated with the reader.
#[doc(alias = "xmlTextReaderConstXmlVersion")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_text_reader_const_xml_version(reader: &mut XmlTextReader) -> *const XmlChar {
    use std::ffi::CString;

    let Some(doc) = reader.doc.or_else(|| {
        (!reader.ctxt.is_null())
            .then(|| (*reader.ctxt).my_doc)
            .flatten()
    }) else {
        return null_mut();
    };

    if let Some(version) = doc.version.as_deref() {
        let version = CString::new(version).unwrap();
        CONSTSTR!(reader, version.as_ptr() as *const u8)
    } else {
        null_mut()
    }
}

/// Create an xmltextReader for a preparsed document.
///
/// Returns the new reader or NULL in case of error.
#[doc(alias = "xmlReaderWalker")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_reader_walker(doc: XmlDocPtr) -> XmlTextReaderPtr {
    use crate::generic_error;

    let ret: XmlTextReaderPtr = xml_malloc(size_of::<XmlTextReader>()) as _;
    if ret.is_null() {
        generic_error!("xmlNewTextReader : malloc failed\n");
        return null_mut();
    }
    std::ptr::write(&mut *ret, XmlTextReader::default());
    (*ret).mode = XmlTextReaderMode::XmlTextreaderModeInitial;
    (*ret).node = null_mut();
    (*ret).curnode = null_mut();
    (*ret).base = 0;
    (*ret).cur = 0;
    (*ret).allocs = XML_TEXTREADER_CTXT;
    (*ret).doc = Some(doc);
    (*ret).state = XmlTextReaderState::Start;
    (*ret).dict = xml_dict_create();
    ret
}

/// Create an xmltextReader for an XML in-memory document.
/// The parsing flags @options are a combination of xmlParserOption.
///
/// Returns the new reader or NULL in case of error.
#[doc(alias = "xmlReaderForDoc")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_reader_for_doc(
    cur: *const XmlChar,
    url: Option<&str>,
    encoding: *const c_char,
    options: i32,
) -> XmlTextReaderPtr {
    use std::ffi::CStr;

    if cur.is_null() {
        return null_mut();
    }

    xml_reader_for_memory(
        CStr::from_ptr(cur as *const i8).to_bytes().to_vec(),
        url,
        encoding,
        options,
    )
}

/// Parse an XML file from the filesystem or the network.
/// The parsing flags @options are a combination of xmlParserOption.
///
/// Returns the new reader or NULL in case of error.
#[doc(alias = "xmlReaderForFile")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_reader_for_file(
    filename: &str,
    encoding: *const c_char,
    options: i32,
) -> XmlTextReaderPtr {
    let reader: XmlTextReaderPtr = xml_new_text_reader_filename(filename);
    if reader.is_null() {
        return null_mut();
    }
    xml_text_reader_setup(reader, None, None, encoding, options);
    reader
}

/// Create an xmltextReader for an XML in-memory document.
/// The parsing flags @options are a combination of xmlParserOption.
///
/// Returns the new reader or NULL in case of error.
#[doc(alias = "xmlReaderForMemory")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_reader_for_memory(
    buffer: Vec<u8>,
    url: Option<&str>,
    encoding: *const c_char,
    options: i32,
) -> XmlTextReaderPtr {
    use crate::encoding::XmlCharEncoding;

    let Some(buf) = XmlParserInputBuffer::from_memory(buffer, XmlCharEncoding::None) else {
        return null_mut();
    };
    let reader: XmlTextReaderPtr = xml_new_text_reader(buf, url);
    if reader.is_null() {
        return null_mut();
    }
    (*reader).allocs |= XML_TEXTREADER_INPUT;
    xml_text_reader_setup(reader, None, url, encoding, options);
    reader
}

/// Create an xmltextReader for an XML document from I/O functions and source.
/// The parsing flags @options are a combination of xmlParserOption.
///
/// Returns the new reader or NULL in case of error.
#[doc(alias = "xmlReaderForIO")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_reader_for_io(
    ioctx: impl Read + 'static,
    url: Option<&str>,
    encoding: *const c_char,
    options: i32,
) -> XmlTextReaderPtr {
    use crate::encoding::XmlCharEncoding;

    let input = XmlParserInputBuffer::from_reader(ioctx, XmlCharEncoding::None);
    let reader: XmlTextReaderPtr = xml_new_text_reader(input, url);
    if reader.is_null() {
        return null_mut();
    }
    (*reader).allocs |= XML_TEXTREADER_INPUT;
    xml_text_reader_setup(reader, None, url, encoding, options);
    reader
}

/// Setup an xmltextReader to parse a preparsed XML document.
/// This reuses the existing @reader xmlTextReader.
///
/// Returns 0 in case of success and -1 in case of error
#[doc(alias = "xmlReaderNewWalker")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_reader_new_walker(reader: XmlTextReaderPtr, doc: XmlDocPtr) -> i32 {
    if reader.is_null() {
        return -1;
    }

    if (*reader).input.is_some() {
        let _ = (*reader).input.take();
    }
    if !(*reader).ctxt.is_null() {
        (*(*reader).ctxt).reset();
    }

    (*reader).ent_tab.clear();
    (*reader).input = None;
    (*reader).mode = XmlTextReaderMode::XmlTextreaderModeInitial;
    (*reader).node = null_mut();
    (*reader).curnode = null_mut();
    (*reader).base = 0;
    (*reader).cur = 0;
    (*reader).allocs = XML_TEXTREADER_CTXT;
    (*reader).doc = Some(doc);
    (*reader).state = XmlTextReaderState::Start;
    if (*reader).dict.is_null() {
        if !(*reader).ctxt.is_null() && !(*(*reader).ctxt).dict.is_null() {
            (*reader).dict = (*(*reader).ctxt).dict;
        } else {
            (*reader).dict = xml_dict_create();
        }
    }
    0
}

/// Setup an xmltextReader to parse an XML in-memory document.
/// The parsing flags @options are a combination of xmlParserOption.
/// This reuses the existing @reader xmlTextReader.
///
/// Returns 0 in case of success and -1 in case of error
#[doc(alias = "xmlReaderNewDoc")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_reader_new_doc(
    reader: XmlTextReaderPtr,
    cur: *const XmlChar,
    url: Option<&str>,
    encoding: *const c_char,
    options: i32,
) -> i32 {
    use std::ffi::CStr;

    if cur.is_null() {
        return -1;
    }
    if reader.is_null() {
        return -1;
    }

    xml_reader_new_memory(
        reader,
        CStr::from_ptr(cur as *const i8).to_bytes().to_vec(),
        url,
        encoding,
        options,
    )
}

/// Parse an XML file from the filesystem or the network.
/// The parsing flags @options are a combination of xmlParserOption.
/// This reuses the existing @reader xmlTextReader.
///
/// Returns 0 in case of success and -1 in case of error
#[doc(alias = "xmlReaderNewFile")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_reader_new_file(
    reader: XmlTextReaderPtr,
    filename: &str,
    encoding: *const c_char,
    options: i32,
) -> i32 {
    use crate::encoding::XmlCharEncoding;

    if reader.is_null() {
        return -1;
    }

    let Some(input) = XmlParserInputBuffer::from_uri(filename, XmlCharEncoding::None) else {
        return -1;
    };
    xml_text_reader_setup(reader, Some(input), Some(filename), encoding, options)
}

/// Setup an xmltextReader to parse an XML in-memory document.
/// The parsing flags @options are a combination of xmlParserOption.
/// This reuses the existing @reader xmlTextReader.
///
/// Returns 0 in case of success and -1 in case of error
#[doc(alias = "xmlReaderNewMemory")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_reader_new_memory(
    reader: XmlTextReaderPtr,
    buffer: Vec<u8>,
    url: Option<&str>,
    encoding: *const c_char,
    options: i32,
) -> i32 {
    use crate::encoding::XmlCharEncoding;

    if reader.is_null() {
        return -1;
    }

    let Some(input) = XmlParserInputBuffer::from_memory(buffer, XmlCharEncoding::None) else {
        return -1;
    };
    xml_text_reader_setup(reader, Some(input), url, encoding, options)
}

/// Setup an xmltextReader to parse an XML document from I/O functions and source.
/// The parsing flags @options are a combination of xmlParserOption.
/// This reuses the existing @reader xmlTextReader.
///
/// Returns 0 in case of success and -1 in case of error
#[doc(alias = "xmlReaderNewIO")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_reader_new_io(
    reader: XmlTextReaderPtr,
    ioctx: impl Read + 'static,
    url: Option<&str>,
    encoding: *const c_char,
    options: i32,
) -> i32 {
    use crate::encoding::XmlCharEncoding;

    if reader.is_null() {
        return -1;
    }

    let input = XmlParserInputBuffer::from_reader(ioctx, XmlCharEncoding::None);
    xml_text_reader_setup(reader, Some(input), url, encoding, options)
}

/// Error handling extensions
#[cfg(feature = "libxml_reader")]
pub type XmlTextReaderLocatorPtr = *mut c_void;

/// Signature of an error callback from a reader parser
#[doc(alias = "xmlTextReaderErrorFunc")]
#[cfg(feature = "libxml_reader")]
pub type XmlTextReaderErrorFunc = unsafe fn(
    arg: Option<GenericErrorContext>,
    msg: *const c_char,
    severity: XmlParserSeverities,
    locator: XmlTextReaderLocatorPtr,
);

/// Obtain the line number for the given locator.
///
/// Returns the line number or -1 in case of error.
#[doc(alias = "xmlTextReaderLocatorLineNumber")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_text_reader_locator_line_number(locator: XmlTextReaderLocatorPtr) -> i32 {
    // we know that locator is a xmlParserCtxtPtr

    use crate::parser::XmlParserInputPtr;
    let ctx: XmlParserCtxtPtr = locator as XmlParserCtxtPtr;
    let ret: i32;

    if locator.is_null() {
        return -1;
    }
    if let Some(node) = (*ctx).node {
        ret = node.get_line_no() as _;
    } else {
        // inspired from error.c
        let mut input: XmlParserInputPtr;
        input = (*ctx).input;
        if (*input).filename.is_none() && (*ctx).input_tab.len() > 1 {
            input = (*ctx).input_tab[(*ctx).input_tab.len() as usize - 2];
        }
        if !input.is_null() {
            ret = (*input).line;
        } else {
            ret = -1;
        }
    }

    ret
}

/// Obtain the base URI for the given locator.
///
/// Returns the base URI or NULL in case of error, if non NULL it need to be freed by the caller.
#[doc(alias = "xmlTextReaderLocatorBaseURI")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_text_reader_locator_base_uri(locator: XmlTextReaderLocatorPtr) -> *mut XmlChar {
    // we know that locator is a xmlParserCtxtPtr

    use std::ffi::CString;

    use crate::{parser::XmlParserInputPtr, tree::NodeCommon};
    let ctx: XmlParserCtxtPtr = locator as XmlParserCtxtPtr;
    let ret: *mut XmlChar;

    if locator.is_null() {
        return null_mut();
    }
    if let Some(node) = (*ctx).node {
        let tmp = node.get_base(None).map(|c| CString::new(c).unwrap());
        ret = xml_strdup(tmp.as_ref().map_or(null_mut(), |t| t.as_ptr() as *const u8));
    } else {
        // inspired from error.c
        let mut input: XmlParserInputPtr;
        input = (*ctx).input;
        if (*input).filename.is_none() && (*ctx).input_tab.len() > 1 {
            input = (*ctx).input_tab[(*ctx).input_tab.len() as usize - 2];
        }
        if !input.is_null() {
            if let Some(filename) = (*input).filename.as_deref() {
                let filename = CString::new(filename).unwrap();
                ret = xml_strdup(filename.as_ptr() as _);
            } else {
                ret = null_mut()
            }
        } else {
            ret = null_mut();
        }
    }

    ret
}

#[cfg(feature = "libxml_reader")]
fn xml_text_reader_error(ctxt: Option<GenericErrorContext>, msg: &str) {
    xml_text_reader_generic_error(ctxt, XmlParserSeverities::XmlParserSeverityError, msg as _);
}

#[cfg(feature = "libxml_reader")]
fn xml_text_reader_warning(ctxt: Option<GenericErrorContext>, msg: &str) {
    xml_text_reader_generic_error(ctxt, XmlParserSeverities::XmlParserSeverityWarning, msg);
}

/// Register a callback function that will be called on error and warnings.
///
/// If @f is NULL, the default error and warning handlers are restored.
#[doc(alias = "xmlTextReaderSetErrorHandler")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_text_reader_set_error_handler(
    reader: XmlTextReaderPtr,
    f: Option<XmlTextReaderErrorFunc>,
    arg: Option<GenericErrorContext>,
) {
    if f.is_some() {
        if let Some(sax) = (*(*reader).ctxt).sax.as_deref_mut() {
            sax.error = Some(xml_text_reader_error);
            sax.serror = None;
            sax.warning = Some(xml_text_reader_warning);
        }
        (*(*reader).ctxt).vctxt.error = Some(xml_text_reader_validity_error);
        (*(*reader).ctxt).vctxt.warning = Some(xml_text_reader_validity_warning);
        (*reader).error_func = f;
        (*reader).serror_func = None;
        (*reader).error_func_arg = arg;
        #[cfg(feature = "schema")]
        {
            let ctx = GenericErrorContext::new(reader);
            if !(*reader).rng_valid_ctxt.is_null() {
                xml_relaxng_set_valid_errors(
                    (*reader).rng_valid_ctxt,
                    Some(xml_text_reader_validity_error_relay),
                    Some(xml_text_reader_validity_warning_relay),
                    Some(ctx.clone()),
                );
                xml_relaxng_set_valid_structured_errors(
                    (*reader).rng_valid_ctxt,
                    None,
                    Some(ctx.clone()),
                );
            }
            if !(*reader).xsd_valid_ctxt.is_null() {
                xml_schema_set_valid_errors(
                    (*reader).xsd_valid_ctxt,
                    Some(xml_text_reader_validity_error_relay),
                    Some(xml_text_reader_validity_warning_relay),
                    Some(ctx.clone()),
                );
                xml_schema_set_valid_structured_errors((*reader).xsd_valid_ctxt, None, Some(ctx));
            }
        }
    } else {
        // restore defaults
        if let Some(sax) = (*(*reader).ctxt).sax.as_deref_mut() {
            sax.error = Some(parser_error);
            sax.warning = Some(parser_warning);
        }
        (*(*reader).ctxt).vctxt.error = Some(parser_validity_error);
        (*(*reader).ctxt).vctxt.warning = Some(parser_validity_warning);
        (*reader).error_func = None;
        (*reader).serror_func = None;
        (*reader).error_func_arg = None;
        #[cfg(feature = "schema")]
        {
            let ctx = GenericErrorContext::new(reader);
            if !(*reader).rng_valid_ctxt.is_null() {
                xml_relaxng_set_valid_errors(
                    (*reader).rng_valid_ctxt,
                    None,
                    None,
                    Some(ctx.clone()),
                );
                xml_relaxng_set_valid_structured_errors(
                    (*reader).rng_valid_ctxt,
                    None,
                    Some(ctx.clone()),
                );
            }
            if !(*reader).xsd_valid_ctxt.is_null() {
                xml_schema_set_valid_errors(
                    (*reader).xsd_valid_ctxt,
                    None,
                    None,
                    Some(ctx.clone()),
                );
                xml_schema_set_valid_structured_errors((*reader).xsd_valid_ctxt, None, Some(ctx));
            }
        }
    }
}

/// Register a callback function that will be called on error and warnings.
///
/// If @f is NULL, the default error and warning handlers are restored.
#[doc(alias = "xmlTextReaderSetStructuredErrorHandler")]
#[cfg(feature = "libxml_reader")]
pub unsafe fn xml_text_reader_set_structured_error_handler(
    reader: XmlTextReaderPtr,
    f: Option<StructuredError>,
    arg: Option<GenericErrorContext>,
) {
    use crate::error::parser_validity_warning;

    if f.is_some() {
        if let Some(sax) = (*(*reader).ctxt).sax.as_deref_mut() {
            sax.error = None;
            sax.serror = Some(xml_text_reader_structured_error);
            sax.warning = Some(xml_text_reader_warning);
        }
        (*(*reader).ctxt).vctxt.error = Some(xml_text_reader_validity_error);
        (*(*reader).ctxt).vctxt.warning = Some(xml_text_reader_validity_warning);
        (*reader).serror_func = f;
        (*reader).error_func = None;
        (*reader).error_func_arg = arg;
        #[cfg(feature = "schema")]
        {
            let ctx = GenericErrorContext::new(reader);
            if !(*reader).rng_valid_ctxt.is_null() {
                xml_relaxng_set_valid_errors(
                    (*reader).rng_valid_ctxt,
                    None,
                    None,
                    Some(ctx.clone()),
                );
                xml_relaxng_set_valid_structured_errors(
                    (*reader).rng_valid_ctxt,
                    Some(xml_text_reader_validity_structured_relay),
                    Some(ctx.clone()),
                );
            }
            if !(*reader).xsd_valid_ctxt.is_null() {
                xml_schema_set_valid_errors(
                    (*reader).xsd_valid_ctxt,
                    None,
                    None,
                    Some(ctx.clone()),
                );
                xml_schema_set_valid_structured_errors(
                    (*reader).xsd_valid_ctxt,
                    Some(xml_text_reader_validity_structured_relay),
                    Some(ctx),
                );
            }
        }
    } else {
        // restore defaults
        if let Some(sax) = (*(*reader).ctxt).sax.as_deref_mut() {
            sax.error = Some(parser_error);
            sax.serror = None;
            sax.warning = Some(parser_warning);
        }
        (*(*reader).ctxt).vctxt.error = Some(parser_validity_error);
        (*(*reader).ctxt).vctxt.warning = Some(parser_validity_warning);
        (*reader).error_func = None;
        (*reader).serror_func = None;
        (*reader).error_func_arg = None;
        #[cfg(feature = "schema")]
        {
            let ctx = GenericErrorContext::new(reader);
            if !(*reader).rng_valid_ctxt.is_null() {
                xml_relaxng_set_valid_errors(
                    (*reader).rng_valid_ctxt,
                    None,
                    None,
                    Some(ctx.clone()),
                );
                xml_relaxng_set_valid_structured_errors(
                    (*reader).rng_valid_ctxt,
                    None,
                    Some(ctx.clone()),
                );
            }
            if !(*reader).xsd_valid_ctxt.is_null() {
                xml_schema_set_valid_errors(
                    (*reader).xsd_valid_ctxt,
                    None,
                    None,
                    Some(ctx.clone()),
                );
                xml_schema_set_valid_structured_errors((*reader).xsd_valid_ctxt, None, Some(ctx));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_text_reader_locator_base_uri() {
        #[cfg(feature = "libxml_reader")]
        unsafe {
            let mut leaks = 0;

            for n_locator in 0..GEN_NB_XML_TEXT_READER_LOCATOR_PTR {
                let mem_base = xml_mem_blocks();
                let locator = gen_xml_text_reader_locator_ptr(n_locator, 0);

                let ret_val = xml_text_reader_locator_base_uri(locator);
                desret_xml_char_ptr(ret_val);
                des_xml_text_reader_locator_ptr(n_locator, locator, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlTextReaderLocatorBaseURI",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlTextReaderLocatorBaseURI()"
                    );
                    eprintln!(" {}", n_locator);
                }
            }
        }
    }

    #[test]
    fn test_xml_text_reader_locator_line_number() {
        #[cfg(feature = "libxml_reader")]
        unsafe {
            let mut leaks = 0;

            for n_locator in 0..GEN_NB_XML_TEXT_READER_LOCATOR_PTR {
                let mem_base = xml_mem_blocks();
                let locator = gen_xml_text_reader_locator_ptr(n_locator, 0);

                let ret_val = xml_text_reader_locator_line_number(locator);
                desret_int(ret_val);
                des_xml_text_reader_locator_ptr(n_locator, locator, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlTextReaderLocatorLineNumber",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlTextReaderLocatorLineNumber()"
                    );
                    eprintln!(" {}", n_locator);
                }
            }
        }
    }

    #[test]
    fn test_xml_text_reader_relax_ngset_schema() {
        #[cfg(all(feature = "libxml_reader", feature = "schema"))]
        unsafe {
            let mut leaks = 0;

            for n_reader in 0..GEN_NB_XML_TEXT_READER_PTR {
                for n_schema in 0..GEN_NB_XML_RELAXNG_PTR {
                    let mem_base = xml_mem_blocks();
                    let reader = gen_xml_text_reader_ptr(n_reader, 0);
                    let schema = gen_xml_relaxng_ptr(n_schema, 1);

                    let ret_val = xml_text_reader_relaxng_set_schema(reader, schema);
                    desret_int(ret_val);
                    des_xml_text_reader_ptr(n_reader, reader, 0);
                    des_xml_relaxng_ptr(n_schema, schema, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlTextReaderRelaxNGSetSchema",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlTextReaderRelaxNGSetSchema()"
                        );
                        eprint!(" {}", n_reader);
                        eprintln!(" {}", n_schema);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_reader_relax_ngvalidate() {
        #[cfg(all(feature = "libxml_reader", feature = "schema"))]
        unsafe {
            let mut leaks = 0;

            for n_reader in 0..GEN_NB_XML_TEXT_READER_PTR {
                for n_rng in 0..GEN_NB_CONST_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let reader = gen_xml_text_reader_ptr(n_reader, 0);
                    let rng = gen_const_char_ptr(n_rng, 1);

                    let ret_val = xml_text_reader_relaxng_validate(reader, rng);
                    desret_int(ret_val);
                    des_xml_text_reader_ptr(n_reader, reader, 0);
                    des_const_char_ptr(n_rng, rng, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlTextReaderRelaxNGValidate",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlTextReaderRelaxNGValidate()"
                        );
                        eprint!(" {}", n_reader);
                        eprintln!(" {}", n_rng);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_reader_relax_ngvalidate_ctxt() {
        #[cfg(all(feature = "libxml_reader", feature = "schema"))]
        unsafe {
            let mut leaks = 0;

            for n_reader in 0..GEN_NB_XML_TEXT_READER_PTR {
                for n_ctxt in 0..GEN_NB_XML_RELAXNG_VALID_CTXT_PTR {
                    for n_options in 0..GEN_NB_PARSEROPTIONS {
                        let mem_base = xml_mem_blocks();
                        let reader = gen_xml_text_reader_ptr(n_reader, 0);
                        let ctxt = gen_xml_relaxng_valid_ctxt_ptr(n_ctxt, 1);
                        let options = gen_parseroptions(n_options, 2);

                        let ret_val = xml_text_reader_relaxng_validate_ctxt(reader, ctxt, options);
                        desret_int(ret_val);
                        des_xml_text_reader_ptr(n_reader, reader, 0);
                        des_xml_relaxng_valid_ctxt_ptr(n_ctxt, ctxt, 1);
                        des_parseroptions(n_options, options, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlTextReaderRelaxNGValidateCtxt",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlTextReaderRelaxNGValidateCtxt()"
                            );
                            eprint!(" {}", n_reader);
                            eprint!(" {}", n_ctxt);
                            eprintln!(" {}", n_options);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_reader_schema_validate() {
        #[cfg(all(feature = "libxml_reader", feature = "schema"))]
        unsafe {
            for n_reader in 0..GEN_NB_XML_TEXT_READER_PTR {
                for n_xsd in 0..GEN_NB_CONST_CHAR_PTR {
                    let reader = gen_xml_text_reader_ptr(n_reader, 0);
                    let xsd = gen_const_char_ptr(n_xsd, 1);

                    let ret_val = xml_text_reader_schema_validate(reader, xsd);
                    desret_int(ret_val);
                    des_xml_text_reader_ptr(n_reader, reader, 0);
                    des_const_char_ptr(n_xsd, xsd, 1);
                    reset_last_error();
                }
            }
        }
    }

    #[test]
    fn test_xml_text_reader_schema_validate_ctxt() {
        #[cfg(all(feature = "libxml_reader", feature = "schema"))]
        unsafe {
            let mut leaks = 0;

            for n_reader in 0..GEN_NB_XML_TEXT_READER_PTR {
                for n_ctxt in 0..GEN_NB_XML_SCHEMA_VALID_CTXT_PTR {
                    for n_options in 0..GEN_NB_PARSEROPTIONS {
                        let mem_base = xml_mem_blocks();
                        let reader = gen_xml_text_reader_ptr(n_reader, 0);
                        let ctxt = gen_xml_schema_valid_ctxt_ptr(n_ctxt, 1);
                        let options = gen_parseroptions(n_options, 2);

                        let ret_val = xml_text_reader_schema_validate_ctxt(reader, ctxt, options);
                        desret_int(ret_val);
                        des_xml_text_reader_ptr(n_reader, reader, 0);
                        des_xml_schema_valid_ctxt_ptr(n_ctxt, ctxt, 1);
                        des_parseroptions(n_options, options, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlTextReaderSchemaValidateCtxt",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlTextReaderSchemaValidateCtxt()"
                            );
                            eprint!(" {}", n_reader);
                            eprint!(" {}", n_ctxt);
                            eprintln!(" {}", n_options);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_reader_set_error_handler() {

        /* missing type support */
    }

    #[test]
    fn test_xml_text_reader_set_schema() {
        #[cfg(all(feature = "libxml_reader", feature = "schema"))]
        unsafe {
            let mut leaks = 0;

            for n_reader in 0..GEN_NB_XML_TEXT_READER_PTR {
                for n_schema in 0..GEN_NB_XML_SCHEMA_PTR {
                    let mem_base = xml_mem_blocks();
                    let reader = gen_xml_text_reader_ptr(n_reader, 0);
                    let schema = gen_xml_schema_ptr(n_schema, 1);

                    let ret_val = xml_text_reader_set_schema(reader, schema);
                    desret_int(ret_val);
                    des_xml_text_reader_ptr(n_reader, reader, 0);
                    des_xml_schema_ptr(n_schema, schema, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlTextReaderSetSchema",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlTextReaderSetSchema()"
                        );
                        eprint!(" {}", n_reader);
                        eprintln!(" {}", n_schema);
                    }
                }
            }
        }
    }
}
