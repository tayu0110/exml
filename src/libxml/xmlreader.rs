//! Provide methods and data structures for XML streaming APIs.  
//! This module is based on `libxml/xmlreader.h`, `xmlreader.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
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
    mem::{size_of, size_of_val},
    os::raw::c_void,
    ptr::{addr_of_mut, null, null_mut},
    sync::atomic::Ordering,
};

use libc::memset;

use crate::{
    buf::libxml_api::{xml_buf_cat, xml_buf_create, xml_buf_free, XmlBufPtr},
    error::{
        parser_error, parser_validity_error, parser_validity_warning, parser_warning, XmlError,
    },
    globals::{GenericErrorContext, StructuredError},
    io::XmlParserInputBuffer,
    libxml::{
        dict::{xml_dict_create, xml_dict_free, xml_dict_lookup, XmlDictPtr},
        globals::{xml_deregister_node_default_value, xml_free, xml_malloc},
        parser::{
            xml_byte_consumed, xml_create_push_parser_ctxt, xml_ctxt_reset, xml_ctxt_use_options,
            xml_free_parser_ctxt, xml_parse_chunk, CdataBlockSAXFunc, CharactersSAXFunc,
            EndElementNsSAX2Func, EndElementSAXFunc, StartElementNsSAX2Func, StartElementSAXFunc,
            XmlParserCtxtPtr, XmlParserInputPtr, XmlParserInputState, XmlParserMode,
            XmlParserOption, XmlSAXHandler, XmlSAXHandlerPtr, XML_COMPLETE_ATTRS, XML_DETECT_IDS,
            XML_SAX2_MAGIC,
        },
        parser_internals::xml_new_input_stream,
        pattern::{xml_free_pattern, xml_pattern_match, xml_patterncompile, XmlPatternPtr},
        relaxng::{
            xml_relaxng_free, xml_relaxng_free_parser_ctxt, xml_relaxng_free_valid_ctxt,
            xml_relaxng_new_parser_ctxt, xml_relaxng_new_valid_ctxt, xml_relaxng_parse,
            xml_relaxng_set_parser_errors, xml_relaxng_set_valid_errors,
            xml_relaxng_set_valid_structured_errors, xml_relaxng_validate_full_element,
            xml_relaxng_validate_pop_element, xml_relaxng_validate_push_cdata,
            xml_relaxng_validate_push_element, XmlRelaxNGParserCtxtPtr, XmlRelaxNGPtr,
            XmlRelaxNGValidCtxtPtr,
        },
        sax2::xml_sax_version,
        uri::xml_canonic_path,
        valid::{
            xml_free_id_table, xml_free_ref_table, xml_validate_pop_element,
            xml_validate_push_cdata, xml_validate_push_element, XmlIDTablePtr, XmlRefTablePtr,
        },
        xinclude::{
            xml_xinclude_new_context, xml_xinclude_process_node, xml_xinclude_set_flags,
            xml_xinclude_set_streaming_mode, XmlXincludeCtxtPtr,
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
    private::buf::{xml_buf_create_size, xml_buf_empty, xml_buf_set_allocation_scheme},
    tree::{
        xml_buf_content, xml_buf_shrink, xml_buf_use, xml_copy_dtd, xml_doc_copy_node,
        xml_free_doc, xml_free_dtd, xml_free_node, xml_free_ns, xml_free_ns_list, xml_new_doc_text,
        xml_split_qname2, XmlAttrPtr, XmlBufferAllocationScheme, XmlDocPtr, XmlDtdPtr,
        XmlElementType, XmlNodePtr, XmlNsPtr, __XML_REGISTER_CALLBACKS,
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlTextReaderMode {
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
    XmlParserLoaddtd = 1,
    XmlParserDefaultattrs = 2,
    XmlParserValidate = 3,
    XmlParserSubstEntities = 4,
}

impl TryFrom<i32> for XmlParserProperties {
    type Error = anyhow::Error;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        if value == Self::XmlParserLoaddtd as i32 {
            Ok(Self::XmlParserLoaddtd)
        } else if value == Self::XmlParserDefaultattrs as i32 {
            Ok(Self::XmlParserDefaultattrs)
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
    XmlReaderTypeCdata = 4,
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
    mode: i32,                       /* the parsing mode */
    doc: XmlDocPtr,                  /* when walking an existing doc */
    validate: XmlTextReaderValidate, /* is there any validation */
    allocs: i32,                     /* what structure were deallocated */
    state: XmlTextReaderState,
    ctxt: XmlParserCtxtPtr,                           /* the parser context */
    sax: XmlSAXHandlerPtr,                            /* the parser SAX callbacks */
    input: Option<XmlParserInputBuffer>,              /* the input */
    start_element: Option<StartElementSAXFunc>,       /* initial SAX callbacks */
    end_element: Option<EndElementSAXFunc>,           /* idem */
    start_element_ns: Option<StartElementNsSAX2Func>, /* idem */
    end_element_ns: Option<EndElementNsSAX2Func>,     /* idem */
    characters: Option<CharactersSAXFunc>,
    cdata_block: Option<CdataBlockSAXFunc>,
    base: u32,            /* base of the segment in the input */
    cur: u32,             /* current position in the input */
    node: XmlNodePtr,     /* current node */
    curnode: XmlNodePtr,  /* current attribute node */
    depth: i32,           /* depth of the current node */
    faketext: XmlNodePtr, /* fake xmlNs chld */
    preserve: i32,        /* preserve the resulting document */
    buffer: XmlBufPtr,    /* used to return const xmlChar * */
    dict: XmlDictPtr,     /* the context dictionary */

    /* entity stack when traversing entities content */
    ent: XmlNodePtr,          /* Current Entity Ref Node */
    ent_nr: i32,              /* Depth of the entities stack */
    ent_max: i32,             /* Max depth of the entities stack */
    ent_tab: *mut XmlNodePtr, /* array of entities */

    /* error handling */
    error_func: Option<XmlTextReaderErrorFunc>, /* callback function */
    error_func_arg: Option<GenericErrorContext>, /* callback function user argument */

    /* Handling of RelaxNG validation */
    #[cfg(feature = "schema")]
    rng_schemas: XmlRelaxNGPtr, /* The Relax NG schemas */
    #[cfg(feature = "schema")]
    rng_valid_ctxt: XmlRelaxNGValidCtxtPtr, /* The Relax NG validation context */
    #[cfg(feature = "schema")]
    rng_preserve_ctxt: i32, /* 1 if the context was provided by the user */
    #[cfg(feature = "schema")]
    rng_valid_errors: i32, /* The number of errors detected */
    #[cfg(feature = "schema")]
    rng_full_node: XmlNodePtr, /* the node if RNG not progressive */
    /* Handling of Schemas validation */
    #[cfg(feature = "schema")]
    xsd_schemas: XmlSchemaPtr, /* The Schemas schemas */
    #[cfg(feature = "schema")]
    xsd_valid_ctxt: XmlSchemaValidCtxtPtr, /* The Schemas validation context */
    #[cfg(feature = "schema")]
    xsd_preserve_ctxt: i32, /* 1 if the context was provided by the user */
    #[cfg(feature = "schema")]
    xsd_valid_errors: i32, /* The number of errors detected */
    #[cfg(feature = "schema")]
    xsd_plug: XmlSchemaSAXPlugPtr, /* the schemas plug in SAX pipeline */
    /* Handling of XInclude processing */
    #[cfg(feature = "xinclude")]
    xinclude: i32, /* is xinclude asked for */
    #[cfg(feature = "xinclude")]
    xinclude_name: *const XmlChar, /* the xinclude name from dict */
    #[cfg(feature = "xinclude")]
    xincctxt: XmlXincludeCtxtPtr, /* the xinclude context */
    #[cfg(feature = "xinclude")]
    in_xinclude: i32, /* counts for xinclude */
    #[cfg(feature = "libxml_pattern")]
    pattern_nr: i32, /* number of preserve patterns */
    #[cfg(feature = "libxml_pattern")]
    pattern_max: i32, /* max preserve patterns */
    #[cfg(feature = "libxml_pattern")]
    pattern_tab: *mut XmlPatternPtr, /* array of preserve patterns */
    preserves: i32,    /* level of preserves */
    parser_flags: i32, /* the set of options set */
    /* Structured error handling */
    serror_func: Option<StructuredError>, /* callback function */
}

impl Default for XmlTextReader {
    fn default() -> Self {
        Self {
            mode: 0,
            doc: null_mut(),
            validate: XmlTextReaderValidate::default(),
            allocs: 0,
            state: XmlTextReaderState::default(),
            ctxt: null_mut(),
            sax: null_mut(),
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
            buffer: null_mut(),
            dict: null_mut(),
            ent: null_mut(),
            ent_nr: 0,
            ent_max: 0,
            ent_tab: null_mut(),
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
            #[cfg(feature = "libxml_pattern")]
            pattern_nr: 0,
            #[cfg(feature = "libxml_pattern")]
            pattern_max: 0,
            #[cfg(feature = "libxml_pattern")]
            pattern_tab: null_mut(),
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
    fullname: *const XmlChar,
    atts: *mut *const XmlChar,
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
            if !(*ctxt).node.is_null()
                && !(*ctxt).input.is_null()
                && !(*(*ctxt).input).cur.is_null()
                && *(*(*ctxt).input).cur.add(0) == b'/'
                && *(*(*ctxt).input).cur.add(1) == b'>'
            {
                (*(*ctxt).node).extra = NODE_IS_EMPTY as _;
            }
        }
    }
    if !reader.is_null() {
        (*reader).state = XmlTextReaderState::Element;
    }
}

/// called when an ending tag has been processed.
#[doc(alias = "xmlTextReaderEndElement")]
unsafe fn xml_text_reader_end_element(ctx: Option<GenericErrorContext>, fullname: *const XmlChar) {
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
unsafe fn xml_text_reader_start_element_ns(
    ctx: Option<GenericErrorContext>,
    localname: *const XmlChar,
    prefix: *const XmlChar,
    uri: *const XmlChar,
    nb_namespaces: i32,
    namespaces: *mut *const XmlChar,
    nb_attributes: i32,
    nb_defaulted: i32,
    attributes: *mut *const XmlChar,
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
                nb_namespaces,
                namespaces,
                nb_attributes,
                nb_defaulted,
                attributes,
            );
            if !(*ctxt).node.is_null()
                && !(*ctxt).input.is_null()
                && !(*(*ctxt).input).cur.is_null()
                && *(*(*ctxt).input).cur.add(0) == b'/'
                && *(*(*ctxt).input).cur.add(1) == b'>'
            {
                (*(*ctxt).node).extra = NODE_IS_EMPTY as _;
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
    localname: *const XmlChar,
    prefix: *const XmlChar,
    uri: *const XmlChar,
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
unsafe fn xml_text_reader_characters(
    ctx: Option<GenericErrorContext>,
    ch: *const XmlChar,
    len: i32,
) {
    let ctxt = {
        let ctx = ctx.as_ref().unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    let reader: XmlTextReaderPtr = (*ctxt)._private as _;

    if !reader.is_null() {
        if let Some(characters) = (*reader).characters {
            characters(ctx, ch, len);
        }
    }
}

/// Called when a pcdata block has been parsed
#[doc(alias = "xmlTextReaderCDataBlock")]
unsafe fn xml_text_reader_cdata_block(
    ctx: Option<GenericErrorContext>,
    ch: *const XmlChar,
    len: i32,
) {
    let ctxt = {
        let ctx = ctx.as_ref().unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    let reader: XmlTextReaderPtr = (*ctxt)._private as _;

    if !reader.is_null() {
        if let Some(cdata_block) = (*reader).cdata_block {
            cdata_block(ctx, ch, len);
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
    uri: *const c_char,
) -> XmlTextReaderPtr {
    use crate::generic_error;

    // if input.is_null() {
    //     return null_mut();
    // }
    let ret: XmlTextReaderPtr = xml_malloc(size_of::<XmlTextReader>()) as _;
    if ret.is_null() {
        generic_error!("xmlNewTextReader : malloc failed\n");
        return null_mut();
    }
    std::ptr::write(ret, XmlTextReader::default());
    (*ret).doc = null_mut();
    (*ret).ent_tab = null_mut();
    (*ret).ent_max = 0;
    (*ret).ent_nr = 0;
    (*ret).input = Some(input);
    (*ret).buffer = xml_buf_create_size(100);
    if (*ret).buffer.is_null() {
        xml_free(ret as _);
        generic_error!("xmlNewTextReader : malloc failed\n");
        return null_mut();
    }
    /* no operation on a reader should require a huge buffer */
    xml_buf_set_allocation_scheme(
        (*ret).buffer,
        XmlBufferAllocationScheme::XmlBufferAllocDoubleit,
    );
    (*ret).sax = xml_malloc(size_of::<XmlSAXHandler>()) as *mut XmlSAXHandler;
    if (*ret).sax.is_null() {
        xml_buf_free((*ret).buffer);
        xml_free(ret as _);
        generic_error!("xmlNewTextReader : malloc failed\n");
        return null_mut();
    }
    xml_sax_version((*ret).sax, 2);
    (*ret).start_element = (*(*ret).sax).start_element;
    (*(*ret).sax).start_element = Some(xml_text_reader_start_element);
    (*ret).end_element = (*(*ret).sax).end_element;
    (*(*ret).sax).end_element = Some(xml_text_reader_end_element);
    #[cfg(feature = "sax1")]
    {
        if (*(*ret).sax).initialized == XML_SAX2_MAGIC as u32 {
            (*ret).start_element_ns = (*(*ret).sax).start_element_ns;
            (*(*ret).sax).start_element_ns = Some(xml_text_reader_start_element_ns);
            (*ret).end_element_ns = (*(*ret).sax).end_element_ns;
            (*(*ret).sax).end_element_ns = Some(xml_text_reader_end_element_ns);
        } else {
            (*ret).start_element_ns = None;
            (*ret).end_element_ns = None;
        }
    }
    #[cfg(not(feature = "sax1"))]
    {
        (*ret).start_element_ns = (*(*ret).sax).start_element_ns;
        (*(*ret).sax).start_element_ns = Some(xml_text_reader_start_element_ns);
        (*ret).end_element_ns = (*(*ret).sax).end_element_ns;
        (*(*ret).sax).end_element_ns = Some(xml_text_reader_end_element_ns);
    }
    (*ret).characters = (*(*ret).sax).characters;
    (*(*ret).sax).characters = Some(xml_text_reader_characters);
    (*(*ret).sax).ignorable_whitespace = Some(xml_text_reader_characters);
    (*ret).cdata_block = (*(*ret).sax).cdata_block;
    (*(*ret).sax).cdata_block = Some(xml_text_reader_cdata_block);

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
            (*ret).sax,
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
        (*ret).ctxt = xml_create_push_parser_ctxt((*ret).sax, None, null_mut(), 0, uri);
        (*ret).base = 0;
        (*ret).cur = 0;
    }

    if (*ret).ctxt.is_null() {
        generic_error!("xmlNewTextReader : malloc failed\n");
        xml_buf_free((*ret).buffer);
        xml_free((*ret).sax as _);
        xml_free(ret as _);
        return null_mut();
    }
    (*(*ret).ctxt).parse_mode = XmlParserMode::XmlParseReader;
    (*(*ret).ctxt)._private = ret as _;
    (*(*ret).ctxt).linenumbers = 1;
    (*(*ret).ctxt).dict_names = 1;
    (*ret).allocs = XML_TEXTREADER_CTXT;
    /*
     * use the parser dictionary to allocate all elements and attributes names
     */
    (*(*ret).ctxt).docdict = 1;
    (*ret).dict = (*(*ret).ctxt).dict;
    #[cfg(feature = "xinclude")]
    {
        (*ret).xinclude = 0;
    }
    #[cfg(feature = "libxml_pattern")]
    {
        (*ret).pattern_max = 0;
        (*ret).pattern_tab = null_mut();
    }
    ret
}

/// Create an xmlTextReader structure fed with the resource at @URI
///
/// Returns the new xmlTextReaderPtr or NULL in case of error
#[doc(alias = "xmlNewTextReaderFilename")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_new_text_reader_filename(uri: *const c_char) -> XmlTextReaderPtr {
    use std::ffi::CStr;

    use crate::{encoding::XmlCharEncoding, io::xml_parser_get_directory};

    let mut directory: *mut c_char = null_mut();

    if uri.is_null() {
        return null_mut();
    }

    let Some(input) = XmlParserInputBuffer::from_uri(
        CStr::from_ptr(uri).to_string_lossy().as_ref(),
        XmlCharEncoding::None,
    ) else {
        return null_mut();
    };
    let ret: XmlTextReaderPtr = xml_new_text_reader(input, uri);
    if ret.is_null() {
        return null_mut();
    }
    (*ret).allocs |= XML_TEXTREADER_INPUT;
    if (*(*ret).ctxt).directory.is_none() {
        directory = xml_parser_get_directory(uri);
    }
    if (*(*ret).ctxt).directory.is_none() && !directory.is_null() {
        (*(*ret).ctxt).directory = Some(CStr::from_ptr(directory).to_string_lossy().into_owned());
    }
    if !directory.is_null() {
        xml_free(directory as _);
    }
    ret
}

/// Deallocate all the resources associated to the reader
#[doc(alias = "xmlFreeTextReader")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_free_text_reader(reader: XmlTextReaderPtr) {
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
    if !(*reader).pattern_tab.is_null() {
        for i in 0..(*reader).pattern_nr {
            if !(*(*reader).pattern_tab.add(i as usize)).is_null() {
                xml_free_pattern(*(*reader).pattern_tab.add(i as usize));
            }
        }
        xml_free((*reader).pattern_tab as _);
    }
    if (*reader).mode != XmlTextReaderMode::XmlTextreaderModeClosed as i32 {
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
    if !(*reader).sax.is_null() {
        xml_free((*reader).sax as _);
    }
    if !(*reader).buffer.is_null() {
        xml_buf_free((*reader).buffer);
    }
    if !(*reader).ent_tab.is_null() {
        xml_free((*reader).ent_tab as _);
    }
    if !(*reader).dict.is_null() {
        xml_dict_free((*reader).dict);
    }
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
    url: *const c_char,
    encoding: *const c_char,
    mut options: i32,
) -> i32 {
    use std::{cell::RefCell, ffi::CStr, rc::Rc};

    use crate::{
        encoding::{find_encoding_handler, XmlCharEncoding},
        generic_error,
        libxml::xinclude::{xml_xinclude_free_context, XINCLUDE_NODE},
    };

    if reader.is_null() {
        // if !input.is_none() {
        //     xml_free_parser_input_buffer(input);
        // }
        return -1;
    }

    /*
     * we force the generation of compact text nodes on the reader
     * since usr applications should never modify the tree
     */
    options |= XmlParserOption::XmlParseCompact as i32;

    (*reader).doc = null_mut();
    (*reader).ent_nr = 0;
    (*reader).parser_flags = options;
    (*reader).validate = XmlTextReaderValidate::NotValidate;
    if input.is_some() && (*reader).input.is_some() && (*reader).allocs & XML_TEXTREADER_INPUT != 0
    {
        // xml_free_parser_input_buffer((*reader).input);
        // (*reader).input = null_mut();
        let _ = (*reader).input.take();
        (*reader).allocs -= XML_TEXTREADER_INPUT;
    }
    let replaced = input.is_some();
    if input.is_some() {
        (*reader).input = input;
        (*reader).allocs |= XML_TEXTREADER_INPUT;
    }
    if (*reader).buffer.is_null() {
        (*reader).buffer = xml_buf_create_size(100);
    }
    if (*reader).buffer.is_null() {
        generic_error!("xmlTextReaderSetup : malloc failed\n");
        return -1;
    }
    /* no operation on a reader should require a huge buffer */
    xml_buf_set_allocation_scheme(
        (*reader).buffer,
        XmlBufferAllocationScheme::XmlBufferAllocDoubleit,
    );
    if (*reader).sax.is_null() {
        (*reader).sax = xml_malloc(size_of::<XmlSAXHandler>()) as *mut XmlSAXHandler;
    }
    if (*reader).sax.is_null() {
        generic_error!("xmlTextReaderSetup : malloc failed\n");
        return -1;
    }
    xml_sax_version((*reader).sax, 2);
    (*reader).start_element = (*(*reader).sax).start_element;
    (*(*reader).sax).start_element = Some(xml_text_reader_start_element);
    (*reader).end_element = (*(*reader).sax).end_element;
    (*(*reader).sax).end_element = Some(xml_text_reader_end_element);
    #[cfg(feature = "sax1")]
    {
        if (*(*reader).sax).initialized == XML_SAX2_MAGIC as u32 {
            (*reader).start_element_ns = (*(*reader).sax).start_element_ns;
            (*(*reader).sax).start_element_ns = Some(xml_text_reader_start_element_ns);
            (*reader).end_element_ns = (*(*reader).sax).end_element_ns;
            (*(*reader).sax).end_element_ns = Some(xml_text_reader_end_element_ns);
        } else {
            (*reader).start_element_ns = None;
            (*reader).end_element_ns = None;
        }
    }
    #[cfg(not(feature = "sax1"))]
    {
        (*reader).startElementNs = (*(*reader).sax).startElementNs;
        (*(*reader).sax).startElementNs = Some(xml_text_reader_start_element_ns);
        (*reader).endElementNs = (*(*reader).sax).endElementNs;
        (*(*reader).sax).endElementNs = Some(xml_text_reader_end_element_ns);
    }
    (*reader).characters = (*(*reader).sax).characters;
    (*(*reader).sax).characters = Some(xml_text_reader_characters);
    (*(*reader).sax).ignorable_whitespace = Some(xml_text_reader_characters);
    (*reader).cdata_block = (*(*reader).sax).cdata_block;
    (*(*reader).sax).cdata_block = Some(xml_text_reader_cdata_block);

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
                    (*reader).sax,
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
                (*reader).ctxt =
                    xml_create_push_parser_ctxt((*reader).sax, None, null_mut(), 0, url);
                (*reader).base = 0;
                (*reader).cur = 0;
            }
        } else {
            let enc = XmlCharEncoding::None;

            xml_ctxt_reset((*reader).ctxt);
            let buf = XmlParserInputBuffer::new(enc);
            // if buf.is_null() {
            //     return -1;
            // }
            let input_stream: XmlParserInputPtr = xml_new_input_stream((*reader).ctxt);
            if input_stream.is_null() {
                return -1;
            }

            if url.is_null() {
                (*input_stream).filename = None;
            } else {
                let canonic = xml_canonic_path(url as _);
                if !canonic.is_null() {
                    (*input_stream).filename = Some(
                        CStr::from_ptr(canonic as *const i8)
                            .to_string_lossy()
                            .into_owned(),
                    );
                    xml_free(canonic as _);
                }
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
    /*
     * use the parser dictionary to allocate all elements and attributes names
     */
    (*(*reader).ctxt).docdict = 1;
    (*(*reader).ctxt).parse_mode = XmlParserMode::XmlParseReader;

    #[cfg(feature = "xinclude")]
    {
        if !(*reader).xincctxt.is_null() {
            xml_xinclude_free_context((*reader).xincctxt);
            (*reader).xincctxt = null_mut();
        }
        if options & XmlParserOption::XmlParseXinclude as i32 != 0 {
            (*reader).xinclude = 1;
            (*reader).xinclude_name =
                xml_dict_lookup((*reader).dict, XINCLUDE_NODE.as_ptr() as _, -1);
            options -= XmlParserOption::XmlParseXinclude as i32;
        } else {
            (*reader).xinclude = 0;
        }
        (*reader).in_xinclude = 0;
    }
    #[cfg(feature = "libxml_pattern")]
    {
        if (*reader).pattern_tab.is_null() {
            (*reader).pattern_nr = 0;
            (*reader).pattern_max = 0;
        }
        while (*reader).pattern_nr > 0 {
            (*reader).pattern_nr -= 1;
            if !(*(*reader).pattern_tab.add((*reader).pattern_nr as usize)).is_null() {
                xml_free_pattern(*(*reader).pattern_tab.add((*reader).pattern_nr as usize));
                *(*reader).pattern_tab.add((*reader).pattern_nr as usize) = null_mut();
            }
        }
    }

    if options & XmlParserOption::XmlParseDtdvalid as i32 != 0 {
        (*reader).validate = XmlTextReaderValidate::ValidateDtd;
    }

    xml_ctxt_use_options((*reader).ctxt, options);
    if !encoding.is_null() {
        if let Some(handler) = find_encoding_handler(CStr::from_ptr(encoding).to_str().unwrap()) {
            (*(*reader).ctxt).switch_to_encoding(handler);
        }
    }
    if !url.is_null()
        && !(*(*reader).ctxt).input.is_null()
        && (*(*(*reader).ctxt).input).filename.is_none()
    {
        (*(*(*reader).ctxt).input).filename =
            Some(CStr::from_ptr(url).to_string_lossy().into_owned());
    }

    (*reader).doc = null_mut();

    0
}

/// Moves the position of the current instance to the next node in
/// the stream, exposing its properties.
///
/// Returns 1 if the node was read successfully, 0 if there is no more nodes to read,
/// or -1 in case of error
#[doc(alias = "xmlTextReaderReadTree")]
#[cfg(feature = "libxml_reader")]
unsafe extern "C" fn xml_text_reader_read_tree(reader: &mut XmlTextReader) -> i32 {
    if reader.state == XmlTextReaderState::End {
        return 0;
    }

    // next_node:
    'next_node: loop {
        if reader.node.is_null() {
            let Some(children) = (*reader.doc).children else {
                reader.state = XmlTextReaderState::End;
                return 0;
            };

            reader.node = children.as_ptr();
            reader.state = XmlTextReaderState::Start;
            // goto found_node;
        } else {
            if reader.state != XmlTextReaderState::Backtrack
                && !matches!(
                    (*reader.node).typ,
                    XmlElementType::XmlDTDNode
                        | XmlElementType::XmlXIncludeStart
                        | XmlElementType::XmlEntityRefNode
                )
            {
                if let Some(children) = (*reader.node).children {
                    reader.node = children.as_ptr();
                    reader.depth += 1;
                    reader.state = XmlTextReaderState::Start;
                    // goto found_node;
                    if matches!(
                        (*reader.node).typ,
                        XmlElementType::XmlXIncludeStart | XmlElementType::XmlXIncludeEnd
                    ) {
                        // goto next_node;
                        continue 'next_node;
                    }
                    break;
                }

                if (*reader.node).typ == XmlElementType::XmlAttributeNode {
                    reader.state = XmlTextReaderState::Backtrack;
                    // goto found_node;
                    if matches!(
                        (*reader.node).typ,
                        XmlElementType::XmlXIncludeStart | XmlElementType::XmlXIncludeEnd
                    ) {
                        // goto next_node;
                        continue 'next_node;
                    }
                    break;
                }
            }

            if let Some(next) = (*reader.node).next {
                reader.node = next.as_ptr();
                reader.state = XmlTextReaderState::Start;
                // goto found_node;
            } else if let Some(parent) = (*reader.node).parent {
                if matches!(
                    parent.typ,
                    XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode
                ) {
                    reader.state = XmlTextReaderState::End;
                    return 0;
                }

                reader.node = parent.as_ptr();
                reader.depth -= 1;
                reader.state = XmlTextReaderState::Backtrack;
                // goto found_node;
            } else {
                reader.state = XmlTextReaderState::End;
            }
        }

        // found_node:
        if matches!(
            (*reader.node).typ,
            XmlElementType::XmlXIncludeStart | XmlElementType::XmlXIncludeEnd
        ) {
            // goto next_node;
            continue 'next_node;
        }

        break;
    }
    1
}

const CHUNK_SIZE: usize = 512;

/// Push data down the progressive parser until a significant callback got raised.
///
/// Returns -1 in case of failure, 0 otherwise
#[doc(alias = "xmlTextReaderPushData")]
#[cfg(feature = "libxml_reader")]
unsafe extern "C" fn xml_text_reader_push_data(reader: &mut XmlTextReader) -> i32 {
    let mut val: i32;
    let mut s: i32;

    if reader.input.is_none() || reader.input.as_ref().unwrap().buffer.is_none() {
        return -1;
    }

    let oldstate: XmlTextReaderState = reader.state;
    reader.state = XmlTextReaderState::None;
    let inbuf: XmlBufPtr = reader.input.as_ref().unwrap().buffer.unwrap().as_ptr();

    while reader.state == XmlTextReaderState::None {
        if xml_buf_use(inbuf) < reader.cur as usize + CHUNK_SIZE {
            /*
             * Refill the buffer unless we are at the end of the stream
             */
            if reader.mode != XmlTextReaderMode::XmlTextreaderModeEof as i32 {
                val = reader.input.as_mut().unwrap().read(4096);
                if val == 0 && reader.input.as_ref().unwrap().context.is_none() {
                    if xml_buf_use(inbuf) == reader.cur as _ {
                        reader.mode = XmlTextReaderMode::XmlTextreaderModeEof as i32;
                        reader.state = oldstate;
                    }
                } else if val < 0 {
                    reader.mode = XmlTextReaderMode::XmlTextreaderModeEof as i32;
                    reader.state = oldstate;
                    if oldstate != XmlTextReaderState::Start || !(*reader.ctxt).my_doc.is_null() {
                        return val;
                    }
                } else if val == 0 {
                    /* mark the end of the stream and process the remains */
                    reader.mode = XmlTextReaderMode::XmlTextreaderModeEof as i32;
                    break;
                }
            } else {
                break;
            }
        }
        /*
         * parse by block of CHUNK_SIZE bytes, various tests show that
         * it's the best tradeoff at least on a 1.2GH Duron
         */
        if xml_buf_use(inbuf) >= reader.cur as usize + CHUNK_SIZE {
            val = xml_parse_chunk(
                reader.ctxt,
                xml_buf_content(inbuf).add(reader.cur as usize) as _,
                CHUNK_SIZE as _,
                0,
            );
            reader.cur += CHUNK_SIZE as u32;
            if val != 0 {
                (*reader.ctxt).well_formed = 0;
            }
            if (*reader.ctxt).well_formed == 0 {
                break;
            }
        } else {
            s = xml_buf_use(inbuf) as i32 - reader.cur as i32;
            val = xml_parse_chunk(
                reader.ctxt,
                xml_buf_content(inbuf).add(reader.cur as usize) as _,
                s,
                0,
            );
            reader.cur += s as u32;
            if val != 0 {
                (*reader.ctxt).well_formed = 0;
            }
            break;
        }
    }

    /*
     * Discard the consumed input when needed and possible
     */
    if reader.mode == XmlTextReaderMode::XmlTextreaderModeInteractive as i32 {
        if reader.input.as_ref().unwrap().context.is_some()
            && (reader.cur >= 4096 && xml_buf_use(inbuf) - reader.cur as usize <= CHUNK_SIZE)
        {
            val = xml_buf_shrink(inbuf, reader.cur as _) as _;
            if val >= 0 {
                reader.cur -= val as u32;
            }
        }
    }
    /*
     * At the end of the stream signal that the work is done to the Push
     * parser.
     */
    else if reader.mode == XmlTextReaderMode::XmlTextreaderModeEof as i32
        && reader.state != XmlTextReaderState::Done
    {
        s = (xml_buf_use(inbuf) - reader.cur as usize) as i32;
        val = xml_parse_chunk(
            reader.ctxt,
            xml_buf_content(inbuf).add(reader.cur as usize) as _,
            s,
            1,
        );
        reader.cur = xml_buf_use(inbuf) as _;
        reader.state = XmlTextReaderState::Done;
        if val != 0 {
            if (*reader.ctxt).well_formed != 0 {
                (*reader.ctxt).well_formed = 0;
            } else {
                return -1;
            }
        }
    }
    reader.state = oldstate;
    if (*reader.ctxt).well_formed == 0 {
        reader.mode = XmlTextReaderMode::XmlTextreaderModeEof as i32;
        return -1;
    }

    0
}

/// Pop the current node from validation
#[doc(alias = "xmlTextReaderValidatePop")]
#[cfg(feature = "libxml_reader")]
unsafe extern "C" fn xml_text_reader_validate_pop(reader: &mut XmlTextReader) {
    let node: XmlNodePtr = reader.node;

    #[cfg(feature = "valid")]
    if reader.validate == XmlTextReaderValidate::ValidateDtd
        && !reader.ctxt.is_null()
        && (*reader.ctxt).validate == 1
    {
        if (*node).ns.is_null() || (*(*node).ns).prefix.is_null() {
            (*reader.ctxt).valid &= xml_validate_pop_element(
                addr_of_mut!((*reader.ctxt).vctxt),
                (*reader.ctxt).my_doc,
                node,
                (*node).name,
            );
        } else {
            /* TODO use the BuildQName interface */
            let mut qname: *mut XmlChar;

            qname = xml_strdup((*(*node).ns).prefix);
            qname = xml_strcat(qname, c":".as_ptr() as _);
            qname = xml_strcat(qname, (*node).name);
            (*reader.ctxt).valid &= xml_validate_pop_element(
                addr_of_mut!((*reader.ctxt).vctxt),
                (*reader.ctxt).my_doc,
                node,
                qname,
            );
            if !qname.is_null() {
                xml_free(qname as _);
            }
        }
    }
    #[cfg(feature = "schema")]
    if reader.validate == XmlTextReaderValidate::ValidateRng && !reader.rng_valid_ctxt.is_null() {
        if !reader.rng_full_node.is_null() {
            if node == reader.rng_full_node {
                reader.rng_full_node = null_mut();
            }
            return;
        }
        let ret: i32 =
            xml_relaxng_validate_pop_element(reader.rng_valid_ctxt, (*reader.ctxt).my_doc, node);
        if ret != 1 {
            reader.rng_valid_errors += 1;
        }
    }
}

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
unsafe extern "C" fn xml_text_reader_free_prop_list(
    reader: &mut XmlTextReader,
    mut cur: XmlAttrPtr,
) {
    let mut next: XmlAttrPtr;

    while !cur.is_null() {
        next = (*cur).next;
        xml_text_reader_free_prop(reader, cur);
        cur = next;
    }
}

/// Free a node and all its siblings, this is a recursive behaviour, all the children are freed too.
#[doc(alias = "xmlTextReaderFreeNodeList")]
#[cfg(feature = "libxml_reader")]
unsafe extern "C" fn xml_text_reader_free_node_list(reader: XmlTextReaderPtr, mut cur: XmlNodePtr) {
    use crate::tree::NodePtr;

    let mut next: XmlNodePtr;
    let mut parent: XmlNodePtr;
    let mut depth: usize = 0;

    let dict = if !reader.is_null() && !(*reader).ctxt.is_null() {
        (*(*reader).ctxt).dict
    } else {
        null_mut()
    };
    if cur.is_null() {
        return;
    }
    if (*cur).typ == XmlElementType::XmlNamespaceDecl {
        xml_free_ns_list(cur as XmlNsPtr);
        return;
    }
    if matches!(
        (*cur).typ,
        XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode
    ) {
        xml_free_doc(cur as XmlDocPtr);
        return;
    }
    loop {
        while let Some(children) = (*cur).children.filter(|children| {
            children.parent == NodePtr::from_ptr(cur)
                && !matches!(
                    (*cur).typ,
                    XmlElementType::XmlDTDNode | XmlElementType::XmlEntityRefNode
                )
        }) {
            cur = children.as_ptr();
            depth += 1;
        }

        next = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
        parent = (*cur).parent.map_or(null_mut(), |p| p.as_ptr());

        /* unroll to speed up freeing the document */
        if (*cur).typ != XmlElementType::XmlDTDNode {
            if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0 {
                // if let Some(f) = xmlDeregisterNodeDefaultValue {
                //     f(cur as _);
                // }
                xml_deregister_node_default_value(cur as _);
            }

            if matches!(
                (*cur).typ,
                XmlElementType::XmlElementNode
                    | XmlElementType::XmlXIncludeStart
                    | XmlElementType::XmlXIncludeEnd
            ) && !(*cur).properties.is_null()
            {
                xml_text_reader_free_prop_list(&mut *reader, (*cur).properties);
            }
            if (*cur).content != addr_of_mut!((*cur).properties) as _
                && !matches!(
                    (*cur).typ,
                    XmlElementType::XmlElementNode
                        | XmlElementType::XmlXIncludeStart
                        | XmlElementType::XmlXIncludeEnd
                        | XmlElementType::XmlEntityRefNode
                )
            {
                DICT_FREE!(dict, (*cur).content);
            }
            if matches!(
                (*cur).typ,
                XmlElementType::XmlElementNode
                    | XmlElementType::XmlXIncludeStart
                    | XmlElementType::XmlXIncludeEnd
            ) && !(*cur).ns_def.is_null()
            {
                xml_free_ns_list((*cur).ns_def);
            }

            /*
             * we don't free element names here they are interned now
             */
            if !matches!(
                (*cur).typ,
                XmlElementType::XmlTextNode | XmlElementType::XmlCommentNode
            ) {
                DICT_FREE!(dict, (*cur).name);
            }
            if matches!(
                (*cur).typ,
                XmlElementType::XmlElementNode | XmlElementType::XmlTextNode
            ) && !reader.is_null()
                && !(*reader).ctxt.is_null()
                && (*(*reader).ctxt).free_elems_nr < MAX_FREE_NODES
            {
                (*cur).next = NodePtr::from_ptr((*(*reader).ctxt).free_elems);
                (*(*reader).ctxt).free_elems = cur;
                (*(*reader).ctxt).free_elems_nr += 1;
            } else {
                xml_free(cur as _);
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
            (*cur).children = None;
        }
    }
}

/// Free a node.
#[doc(alias = "xmlTextReaderFreeProp")]
#[cfg(feature = "libxml_reader")]
unsafe extern "C" fn xml_text_reader_free_prop(reader: XmlTextReaderPtr, cur: XmlAttrPtr) {
    let dict = if !reader.is_null() && !(*reader).ctxt.is_null() {
        (*(*reader).ctxt).dict
    } else {
        null_mut()
    };
    if cur.is_null() {
        return;
    }

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0 {
        // if let Some(f) = xmlDeregisterNodeDefaultValue {
        //     f(cur as _);
        // }
        xml_deregister_node_default_value(cur as _);
    }

    if let Some(children) = (*cur).children {
        xml_text_reader_free_node_list(reader, children.as_ptr());
    }

    DICT_FREE!(dict, (*cur).name);
    if !reader.is_null()
        && !(*reader).ctxt.is_null()
        && (*(*reader).ctxt).free_attrs_nr < MAX_FREE_NODES
    {
        (*cur).next = (*(*reader).ctxt).free_attrs;
        (*(*reader).ctxt).free_attrs = cur;
        (*(*reader).ctxt).free_attrs_nr += 1;
    } else {
        xml_free(cur as _);
    }
}

/// Free a node, this is a recursive behaviour, all the children are freed too.
/// This doesn't unlink the child from the list, use xmlUnlinkNode() first.
#[doc(alias = "xmlTextReaderFreeNode")]
#[cfg(feature = "libxml_reader")]
unsafe extern "C" fn xml_text_reader_free_node(reader: XmlTextReaderPtr, cur: XmlNodePtr) {
    use crate::tree::NodePtr;

    let dict = if !reader.is_null() && !(*reader).ctxt.is_null() {
        (*(*reader).ctxt).dict
    } else {
        null_mut()
    };
    if (*cur).typ == XmlElementType::XmlDTDNode {
        xml_free_dtd(cur as XmlDtdPtr);
        return;
    }
    if (*cur).typ == XmlElementType::XmlNamespaceDecl {
        xml_free_ns(cur as XmlNsPtr);
        return;
    }
    if (*cur).typ == XmlElementType::XmlAttributeNode {
        xml_text_reader_free_prop(reader, cur as XmlAttrPtr);
        return;
    }

    if let Some(children) = (*cur)
        .children
        .filter(|_| (*cur).typ != XmlElementType::XmlEntityRefNode)
    {
        if children.parent == NodePtr::from_ptr(cur) {
            xml_text_reader_free_node_list(reader, children.as_ptr());
        }
        (*cur).children = None;
    }

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0 {
        // if let Some(f) = xmlDeregisterNodeDefaultValue {
        //     f(cur);
        // }
        xml_deregister_node_default_value(cur);
    }

    if matches!(
        (*cur).typ,
        XmlElementType::XmlElementNode
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd
    ) && !(*cur).properties.is_null()
    {
        xml_text_reader_free_prop_list(&mut *reader, (*cur).properties);
    }
    if (*cur).content != addr_of_mut!((*cur).properties) as *mut XmlChar
        && !matches!(
            (*cur).typ,
            XmlElementType::XmlElementNode
                | XmlElementType::XmlXIncludeStart
                | XmlElementType::XmlXIncludeEnd
                | XmlElementType::XmlEntityRefNode
        )
    {
        DICT_FREE!(dict, (*cur).content);
    }
    if matches!(
        (*cur).typ,
        XmlElementType::XmlElementNode
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd
    ) && !(*cur).ns_def.is_null()
    {
        xml_free_ns_list((*cur).ns_def);
    }

    /*
     * we don't free names here they are interned now
     */
    if !matches!(
        (*cur).typ,
        XmlElementType::XmlTextNode | XmlElementType::XmlCommentNode
    ) {
        DICT_FREE!(dict, (*cur).name);
    }

    if matches!(
        (*cur).typ,
        XmlElementType::XmlElementNode | XmlElementType::XmlTextNode
    ) && !reader.is_null()
        && !(*reader).ctxt.is_null()
        && (*(*reader).ctxt).free_elems_nr < MAX_FREE_NODES
    {
        (*cur).next = NodePtr::from_ptr((*(*reader).ctxt).free_elems);
        (*(*reader).ctxt).free_elems = cur;
        (*(*reader).ctxt).free_elems_nr += 1;
    } else {
        xml_free(cur as _);
    }
}

/// Pushes a new entity reference node on top of the entities stack
///
/// Returns -1 in case of error, the index in the stack otherwise
#[doc(alias = "xmlTextReaderEntPush")]
#[cfg(feature = "libxml_reader")]
unsafe extern "C" fn xml_text_reader_ent_push(
    reader: &mut XmlTextReader,
    value: XmlNodePtr,
) -> i32 {
    use crate::generic_error;

    use super::globals::xml_realloc;

    if reader.ent_nr >= reader.ent_max {
        let new_size: usize = if reader.ent_max == 0 {
            10
        } else {
            reader.ent_max as usize * 2
        };

        let tmp: *mut XmlNodePtr =
            xml_realloc(reader.ent_tab as _, new_size * size_of::<XmlNodePtr>()) as *mut XmlNodePtr;
        if tmp.is_null() {
            generic_error!("xmlRealloc failed !\n");
            return -1;
        }
        reader.ent_tab = tmp;
        reader.ent_max = new_size as _;
    }
    *reader.ent_tab.add(reader.ent_nr as usize) = value;
    reader.ent = value;
    reader.ent_nr += 1;
    reader.ent_nr - 1
}

/// Push the current node for validation
#[doc(alias = "xmlTextReaderValidatePush")]
#[cfg(all(feature = "libxml_reader", feature = "regexp"))]
unsafe extern "C" fn xml_text_reader_validate_push(reader: &mut XmlTextReader) {
    let mut node: XmlNodePtr = reader.node;

    #[cfg(feature = "valid")]
    if reader.validate == XmlTextReaderValidate::ValidateDtd
        && !reader.ctxt.is_null()
        && (*reader.ctxt).validate == 1
    {
        if (*node).ns.is_null() || (*(*node).ns).prefix.is_null() {
            (*reader.ctxt).valid &= xml_validate_push_element(
                addr_of_mut!((*reader.ctxt).vctxt),
                (*reader.ctxt).my_doc,
                node,
                (*node).name,
            );
        } else {
            /* TODO use the BuildQName interface */
            let mut qname: *mut XmlChar;

            qname = xml_strdup((*(*node).ns).prefix);
            qname = xml_strcat(qname, c":".as_ptr() as _);
            qname = xml_strcat(qname, (*node).name);
            (*reader.ctxt).valid &= xml_validate_push_element(
                addr_of_mut!((*reader.ctxt).vctxt),
                (*reader.ctxt).my_doc,
                node,
                qname,
            );
            if !qname.is_null() {
                xml_free(qname as _);
            }
        }
    }
    #[cfg(feature = "schema")]
    if reader.validate == XmlTextReaderValidate::ValidateRng && !reader.rng_valid_ctxt.is_null() {
        let mut ret: i32;

        if !reader.rng_full_node.is_null() {
            return;
        }
        ret = xml_relaxng_validate_push_element(reader.rng_valid_ctxt, (*reader.ctxt).my_doc, node);
        if ret == 0 {
            /*
             * this element requires a full tree
             */
            node = xml_text_reader_expand(reader);
            if node.is_null() {
                ret = -1;
            } else {
                ret = xml_relaxng_validate_full_element(
                    reader.rng_valid_ctxt,
                    (*reader.ctxt).my_doc,
                    node,
                );
                reader.rng_full_node = node;
            }
        }
        if ret != 1 {
            reader.rng_valid_errors += 1;
        }
    }
}

/// Push some CData for validation
#[doc(alias = "xmlTextReaderValidateCData")]
#[cfg(all(feature = "libxml_reader", feature = "regexp"))]
unsafe extern "C" fn xml_text_reader_validate_cdata(
    reader: &mut XmlTextReader,
    data: *const XmlChar,
    len: i32,
) {
    #[cfg(feature = "valid")]
    if reader.validate == XmlTextReaderValidate::ValidateDtd
        && !reader.ctxt.is_null()
        && (*reader.ctxt).validate == 1
    {
        (*reader.ctxt).valid &=
            xml_validate_push_cdata(addr_of_mut!((*reader.ctxt).vctxt), data, len);
    }
    #[cfg(feature = "schema")]
    if reader.validate == XmlTextReaderValidate::ValidateRng && !reader.rng_valid_ctxt.is_null() {
        if !reader.rng_full_node.is_null() {
            return;
        }
        let ret: i32 = xml_relaxng_validate_push_cdata(reader.rng_valid_ctxt, data, len);
        if ret != 1 {
            reader.rng_valid_errors += 1;
        }
    }
}

/// Pops the top element entity from the entities stack
///
/// Returns the entity just removed
#[doc(alias = "xmlTextReaderEntPop")]
#[cfg(feature = "libxml_reader")]
unsafe extern "C" fn xml_text_reader_ent_pop(reader: &mut XmlTextReader) -> XmlNodePtr {
    if reader.ent_nr <= 0 {
        return null_mut();
    }
    reader.ent_nr -= 1;
    if reader.ent_nr > 0 {
        reader.ent = *reader.ent_tab.add(reader.ent_nr as usize - 1);
    } else {
        reader.ent = null_mut();
    }
    let ret: XmlNodePtr = *reader.ent_tab.add(reader.ent_nr as usize);
    *reader.ent_tab.add(reader.ent_nr as usize) = null_mut();
    ret
}

/// Handle the validation when an entity reference is encountered and
/// entity substitution is not activated. As a result the parser interface
/// must walk through the entity and do the validation calls
#[doc(alias = "xmlTextReaderValidateEntity")]
#[cfg(all(feature = "libxml_reader", feature = "regexp"))]
unsafe extern "C" fn xml_text_reader_validate_entity(reader: &mut XmlTextReader) {
    use crate::tree::{NodeCommon, NodePtr};

    let oldnode: XmlNodePtr = reader.node;
    let mut node: XmlNodePtr = reader.node;

    'main: while {
        'inner: {
            'skip_children: {
                if (*node).typ == XmlElementType::XmlEntityRefNode {
                    if let Some(children) = (*node).children.filter(|children| {
                        children.typ == XmlElementType::XmlEntityDecl && children.children.is_some()
                    }) {
                        if xml_text_reader_ent_push(reader, node) < 0 {
                            if node == oldnode {
                                // break;
                                break 'main;
                            }
                            break 'skip_children;
                        }
                        node = children.children.map_or(null_mut(), |c| c.as_ptr());
                        // continue;
                        break 'inner;
                    } else {
                        /*
                         * The error has probably been raised already.
                         */
                        if node == oldnode {
                            break 'main;
                        }
                        break 'skip_children;
                    }
                } else {
                    #[cfg(feature = "regexp")]
                    if (*node).typ == XmlElementType::XmlElementNode {
                        reader.node = node;
                        xml_text_reader_validate_push(reader);
                    } else if matches!(
                        (*node).typ,
                        XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
                    ) {
                        xml_text_reader_validate_cdata(
                            reader,
                            (*node).content,
                            xml_strlen((*node).content),
                        );
                    }
                }
                /*
                 * go to next node
                 */
                if let Some(children) = (*node).children {
                    node = children.as_ptr();
                    // continue;
                    break 'inner;
                } else if (*node).typ == XmlElementType::XmlElementNode {
                    xml_text_reader_validate_pop(reader);
                }
            }

            // skip_children:
            if let Some(next) = (*node).next {
                node = next.as_ptr();
                // continue;
                break 'inner;
            }

            loop {
                node = (*node).parent.map_or(null_mut(), |p| p.as_ptr());
                if (*node).typ == XmlElementType::XmlElementNode {
                    let mut tmp: XmlNodePtr;
                    if reader.ent_nr == 0 {
                        while {
                            tmp = (*node).last.map_or(null_mut(), |l| l.as_ptr());
                            !tmp.is_null()
                        } {
                            if (*tmp).extra & NODE_IS_PRESERVED as u16 == 0 {
                                (*tmp).unlink();
                                xml_text_reader_free_node(reader, tmp);
                            } else {
                                break;
                            }
                        }
                    }
                    reader.node = node;
                    xml_text_reader_validate_pop(reader);
                }
                if (*node).typ == XmlElementType::XmlEntityDecl
                    && !reader.ent.is_null()
                    && (*reader.ent).children == NodePtr::from_ptr(node)
                {
                    node = xml_text_reader_ent_pop(reader);
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
    reader.node = oldnode;
}

/// Moves the position of the current instance to the next node in the stream,
/// exposing its properties.
///
/// Returns 1 if the node was read successfully, 0 if there is no more nodes to read,
/// or -1 in case of error
#[doc(alias = "xmlTextReaderRead")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_read(reader: &mut XmlTextReader) -> i32 {
    use crate::{
        libxml::xinclude::{XINCLUDE_NS, XINCLUDE_OLD_NS},
        tree::{NodeCommon, NodePtr},
    };

    let mut val: i32;
    let mut olddepth = 0;
    let mut oldstate: XmlTextReaderState = XmlTextReaderState::Start;
    let mut oldnode: XmlNodePtr = null_mut();

    reader.curnode = null_mut();
    if !reader.doc.is_null() {
        return xml_text_reader_read_tree(reader);
    }
    if reader.ctxt.is_null() {
        return -1;
    }

    let mut node_found = false;
    if reader.mode == XmlTextReaderMode::XmlTextreaderModeInitial as i32 {
        reader.mode = XmlTextReaderMode::XmlTextreaderModeInteractive as i32;
        /*
         * Initial state
         */
        while {
            val = xml_text_reader_push_data(reader);
            if val < 0 {
                reader.mode = XmlTextReaderMode::XmlTextreaderModeError as i32;
                reader.state = XmlTextReaderState::Error;
                return -1;
            }
            (*reader.ctxt).node.is_null()
                && (reader.mode != XmlTextReaderMode::XmlTextreaderModeEof as i32
                    && reader.state != XmlTextReaderState::Done)
        } {}
        if (*reader.ctxt).node.is_null() {
            if !(*reader.ctxt).my_doc.is_null() {
                reader.node = (*(*reader.ctxt).my_doc)
                    .children
                    .map_or(null_mut(), |c| c.as_ptr());
            }
            if reader.node.is_null() {
                reader.mode = XmlTextReaderMode::XmlTextreaderModeError as i32;
                reader.state = XmlTextReaderState::Error;
                return -1;
            }
            reader.state = XmlTextReaderState::Element;
        } else {
            if !(*reader.ctxt).my_doc.is_null() {
                reader.node = (*(*reader.ctxt).my_doc)
                    .children
                    .map_or(null_mut(), |c| c.as_ptr());
            }
            if reader.node.is_null() {
                reader.node = (*reader.ctxt).node_tab[0];
            }
            reader.state = XmlTextReaderState::Element;
        }
        reader.depth = 0;
        (*reader.ctxt).parse_mode = XmlParserMode::XmlParseReader;
        // goto node_found;
        node_found = true;
    } else {
        oldstate = reader.state;
        olddepth = (*reader.ctxt).node_tab.len();
        oldnode = reader.node;
    }

    // get_next_node:
    'get_next_node: loop {
        if !node_found {
            if reader.node.is_null() {
                if reader.mode == XmlTextReaderMode::XmlTextreaderModeEof as i32 {
                    return 0;
                } else {
                    return -1;
                }
            }

            'goto_node_found: {
                /*
                 * If we are not backtracking on ancestors or examined nodes,
                 * that the parser didn't finished or that we aren't at the end
                 * of stream, continue processing.
                 */
                while !reader.node.is_null()
                    && (*reader.node).next.is_none()
                    && (*reader.ctxt).node_tab.len() == olddepth
                    && (oldstate == XmlTextReaderState::Backtrack
                        || (*reader.node).children.is_none()
                        || (*reader.node).typ == XmlElementType::XmlEntityRefNode
                        || (*reader.node)
                            .children
                            .filter(|children| {
                                children.typ == XmlElementType::XmlTextNode
                                    && children.next.is_none()
                            })
                            .is_some()
                        || matches!(
                            (*reader.node).typ,
                            XmlElementType::XmlDTDNode
                                | XmlElementType::XmlDocumentNode
                                | XmlElementType::XmlHTMLDocumentNode
                        ))
                    && ((*reader.ctxt).node.is_null()
                        || (*reader.ctxt).node == reader.node
                        || (*reader.ctxt).node
                            == (*reader.node).parent.map_or(null_mut(), |p| p.as_ptr()))
                    && !matches!((*reader.ctxt).instate, XmlParserInputState::XmlParserEOF)
                {
                    val = xml_text_reader_push_data(reader);
                    if val < 0 {
                        reader.mode = XmlTextReaderMode::XmlTextreaderModeError as i32;
                        reader.state = XmlTextReaderState::Error;
                        return -1;
                    }
                    if reader.node.is_null() {
                        // goto node_end;
                        reader.state = XmlTextReaderState::Done;
                        return 0;
                    }
                }
                if let Some(children) = (*reader.node).children.filter(|_| {
                    oldstate != XmlTextReaderState::Backtrack
                        && !matches!(
                            (*reader.node).typ,
                            XmlElementType::XmlEntityRefNode
                                | XmlElementType::XmlXIncludeStart
                                | XmlElementType::XmlDTDNode
                        )
                }) {
                    reader.node = children.as_ptr();
                    reader.depth += 1;
                    reader.state = XmlTextReaderState::Element;
                    break 'goto_node_found;
                }
                if let Some(next) = (*reader.node).next {
                    #[cfg(not(feature = "xinclude"))]
                    let f = true;
                    #[cfg(feature = "xinclude")]
                    let f = reader.in_xinclude <= 0;
                    if oldstate == XmlTextReaderState::Element
                        && (*reader.node).typ == XmlElementType::XmlElementNode
                        && (*reader.node).children.is_none()
                        && (*reader.node).extra & NODE_IS_EMPTY as u16 == 0
                        && f
                    {
                        reader.state = XmlTextReaderState::End;
                        break 'goto_node_found;
                    }
                    #[cfg(feature = "regexp")]
                    if reader.validate as u32 != 0
                        && (*reader.node).typ == XmlElementType::XmlElementNode
                    {
                        xml_text_reader_validate_pop(reader);
                    }
                    if reader.preserves > 0 && (*reader.node).extra & NODE_IS_SPRESERVED as u16 != 0
                    {
                        reader.preserves -= 1;
                    }
                    reader.node = next.as_ptr();
                    reader.state = XmlTextReaderState::Element;

                    /*
                     * Cleanup of the old node
                     */
                    #[cfg(not(feature = "xinclude"))]
                    let f = true;
                    #[cfg(feature = "xinclude")]
                    let f = reader.in_xinclude == 0;
                    if reader.preserves == 0
                        && f
                        && reader.ent_nr == 0
                        && (*reader.node).prev.is_some()
                        && (*reader.node).prev.unwrap().typ != XmlElementType::XmlDTDNode
                    {
                        let tmp: XmlNodePtr = (*reader.node).prev.unwrap().as_ptr();
                        if (*tmp).extra & NODE_IS_PRESERVED as u16 == 0 {
                            if oldnode == tmp {
                                oldnode = null_mut();
                            }
                            (*tmp).unlink();
                            xml_text_reader_free_node(reader, tmp);
                        }
                    }

                    break 'goto_node_found;
                }
                if oldstate == XmlTextReaderState::Element
                    && (*reader.node).typ == XmlElementType::XmlElementNode
                    && (*reader.node).children.is_none()
                    && (*reader.node).extra & NODE_IS_EMPTY as u16 == 0
                {
                    reader.state = XmlTextReaderState::End;
                    break 'goto_node_found;
                }
                #[cfg(feature = "regexp")]
                if reader.validate != XmlTextReaderValidate::NotValidate
                    && (*reader.node).typ == XmlElementType::XmlElementNode
                {
                    xml_text_reader_validate_pop(reader);
                }
                if reader.preserves > 0 && (*reader.node).extra & NODE_IS_SPRESERVED as u16 != 0 {
                    reader.preserves -= 1;
                }
                reader.node = (*reader.node).parent.map_or(null_mut(), |p| p.as_ptr());
                if reader.node.is_null()
                    || matches!(
                        (*reader.node).typ,
                        XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode
                    )
                {
                    if reader.mode != XmlTextReaderMode::XmlTextreaderModeEof as i32 {
                        val = xml_parse_chunk(reader.ctxt, c"".as_ptr() as _, 0, 1);
                        reader.state = XmlTextReaderState::Done;
                        if val != 0 {
                            return -1;
                        }
                    }
                    reader.node = null_mut();
                    reader.depth = -1;

                    /*
                     * Cleanup of the old node
                     */
                    #[cfg(not(feature = "xinclude"))]
                    let f = true;
                    #[cfg(feature = "xinclude")]
                    let f = reader.in_xinclude == 0;
                    if !oldnode.is_null()
                        && reader.preserves == 0
                        && f
                        && reader.ent_nr == 0
                        && (*oldnode).typ != XmlElementType::XmlDTDNode
                        && (*oldnode).extra & NODE_IS_PRESERVED as u16 == 0
                    {
                        (*oldnode).unlink();
                        xml_text_reader_free_node(reader, oldnode);
                    }

                    // goto node_end;
                    reader.state = XmlTextReaderState::Done;
                    return 0;
                }

                #[cfg(not(feature = "xinclude"))]
                let f = true;
                #[cfg(feature = "xinclude")]
                let f = reader.in_xinclude == 0;
                if reader.preserves == 0
                    && f
                    && reader.ent_nr == 0
                    && (*reader.node).last.is_some()
                    && (*reader.node).last.unwrap().extra & NODE_IS_PRESERVED as u16 == 0
                {
                    let tmp: XmlNodePtr = (*reader.node).last.unwrap().as_ptr();
                    (*tmp).unlink();
                    xml_text_reader_free_node(reader, tmp);
                }
                reader.depth -= 1;
                reader.state = XmlTextReaderState::Backtrack;
            }
        }

        // node_found:
        node_found = false;
        // DUMP_READER

        /*
         * If we are in the middle of a piece of CDATA make sure it's finished
         */
        if (!reader.node.is_null()
            && (*reader.node).next.is_none()
            && ((*reader.node).typ == XmlElementType::XmlTextNode
                || (*reader.node).typ == XmlElementType::XmlCDATASectionNode))
            && xml_text_reader_expand(reader).is_null()
        {
            return -1;
        }

        #[cfg(feature = "xinclude")]
        {
            /*
             * Handle XInclude if asked for
             */
            if reader.xinclude != 0
                && reader.in_xinclude == 0
                && reader.state != XmlTextReaderState::Backtrack
                && !reader.node.is_null()
                && (*reader.node).typ == XmlElementType::XmlElementNode
                && !(*reader.node).ns.is_null()
                && (xml_str_equal((*(*reader.node).ns).href, XINCLUDE_NS.as_ptr() as _)
                    || xml_str_equal((*(*reader.node).ns).href, XINCLUDE_OLD_NS.as_ptr() as _))
            {
                if reader.xincctxt.is_null() {
                    reader.xincctxt = xml_xinclude_new_context((*reader.ctxt).my_doc);
                    xml_xinclude_set_flags(
                        reader.xincctxt,
                        reader.parser_flags & !(XmlParserOption::XmlParseNoxincnode as i32),
                    );
                    xml_xinclude_set_streaming_mode(reader.xincctxt, 1);
                }
                /*
                 * expand that node and process it
                 */
                if xml_text_reader_expand(reader).is_null() {
                    return -1;
                }
                xml_xinclude_process_node(reader.xincctxt, reader.node);
            }
            if !reader.node.is_null() && (*reader.node).typ == XmlElementType::XmlXIncludeStart {
                reader.in_xinclude += 1;
                // goto get_next_node;
                continue 'get_next_node;
            }
            if !reader.node.is_null() && (*reader.node).typ == XmlElementType::XmlXIncludeEnd {
                reader.in_xinclude -= 1;
                // goto get_next_node;
                continue 'get_next_node;
            }
        }
        /*
         * Handle entities enter and exit when in entity replacement mode
         */
        if !reader.node.is_null()
            && (*reader.node).typ == XmlElementType::XmlEntityRefNode
            && !reader.ctxt.is_null()
            && (*reader.ctxt).replace_entities == 1
        {
            if let Some(children) = (*reader.node).children.filter(|children| {
                children.typ == XmlElementType::XmlEntityDecl && children.children.is_some()
            }) {
                if xml_text_reader_ent_push(reader, reader.node) < 0 {
                    // goto get_next_node;
                    continue 'get_next_node;
                }
                reader.node = children.children.map_or(null_mut(), |c| c.as_ptr());
            }
        } else {
            #[cfg(feature = "regexp")]
            if !reader.node.is_null()
                && (*reader.node).typ == XmlElementType::XmlEntityRefNode
                && !reader.ctxt.is_null()
                && reader.validate as i32 != 0
            {
                xml_text_reader_validate_entity(reader);
            }
        }
        if !reader.node.is_null()
            && (*reader.node).typ == XmlElementType::XmlEntityDecl
            && !reader.ent.is_null()
            && (*reader.ent).children == NodePtr::from_ptr(reader.node)
        {
            reader.node = xml_text_reader_ent_pop(reader);
            reader.depth += 1;
            // goto get_next_node;
            continue 'get_next_node;
        }

        break;
    }
    #[cfg(feature = "regexp")]
    if reader.validate != XmlTextReaderValidate::NotValidate && !reader.node.is_null() {
        let node: XmlNodePtr = reader.node;

        if (*node).typ == XmlElementType::XmlElementNode
            && !matches!(
                reader.state,
                XmlTextReaderState::End | XmlTextReaderState::Backtrack
            )
        {
            xml_text_reader_validate_push(reader);
        } else if matches!(
            (*node).typ,
            XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
        ) {
            xml_text_reader_validate_cdata(reader, (*node).content, xml_strlen((*node).content));
        }
    }
    #[cfg(feature = "libxml_pattern")]
    if reader.pattern_nr > 0
        && !matches!(
            reader.state,
            XmlTextReaderState::End | XmlTextReaderState::Backtrack
        )
    {
        for i in 0..reader.pattern_nr {
            if xml_pattern_match(*reader.pattern_tab.add(i as usize), reader.node) == 1 {
                xml_text_reader_preserve(reader);
                break;
            }
        }
    }
    #[cfg(feature = "schema")]
    if reader.validate == XmlTextReaderValidate::ValidateXsd
        && reader.xsd_valid_errors == 0
        && !reader.xsd_valid_ctxt.is_null()
    {
        reader.xsd_valid_errors = (xml_schema_is_valid(reader.xsd_valid_ctxt) == 0) as i32;
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
#[cfg(all(feature = "libxml_reader", feature = "writer"))]
pub unsafe extern "C" fn xml_text_reader_read_inner_xml(
    reader: &mut XmlTextReader,
) -> *mut XmlChar {
    use crate::{buf::XmlBufRef, private::buf::xml_buf_detach};

    let mut node: XmlNodePtr;
    let mut cur_node: XmlNodePtr;

    if xml_text_reader_expand(reader).is_null() {
        return null_mut();
    }
    let doc: XmlDocPtr = (*reader.node).doc;
    let buff = xml_buf_create();
    if buff.is_null() {
        return null_mut();
    }
    xml_buf_set_allocation_scheme(buff, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);
    cur_node = (*reader.node).children.map_or(null_mut(), |c| c.as_ptr());
    while !cur_node.is_null() {
        /* XXX: Why is the node copied? */
        node = xml_doc_copy_node(cur_node, doc, 1);
        /* XXX: Why do we need a second buffer? */
        let buff2 = xml_buf_create();
        xml_buf_set_allocation_scheme(buff, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);
        if (*node).dump_memory(buff2, doc, 0, 0) == 0
            || !XmlBufRef::from_raw(buff2).unwrap().is_ok()
        {
            xml_free_node(node);
            xml_buf_free(buff2);
            xml_buf_free(buff);
            return null_mut();
        }
        xml_buf_cat(buff, xml_buf_content(buff2));
        xml_free_node(node);
        xml_buf_free(buff2);
        cur_node = (*cur_node).next.map_or(null_mut(), |n| n.as_ptr());
    }
    let resbuf = xml_buf_detach(buff);

    xml_buf_free(buff);
    resbuf
}

/// Reads the contents of the current node, including child nodes and markup.
///
/// Returns a string containing the node and any XML content, or NULL if the
/// current node cannot be serialized. The string must be deallocated by the caller.
#[doc(alias = "xmlTextReaderReadOuterXml")]
#[cfg(all(feature = "libxml_reader", feature = "writer"))]
pub unsafe extern "C" fn xml_text_reader_read_outer_xml(
    reader: &mut XmlTextReader,
) -> *mut XmlChar {
    use crate::buf::XmlBufRef;

    let mut node: XmlNodePtr;

    if xml_text_reader_expand(reader).is_null() {
        return null_mut();
    }
    node = reader.node;
    let doc: XmlDocPtr = (*node).doc;
    /* XXX: Why is the node copied? */
    if (*node).typ == XmlElementType::XmlDTDNode {
        node = xml_copy_dtd(node as XmlDtdPtr) as XmlNodePtr;
    } else {
        node = xml_doc_copy_node(node, doc, 1);
    }
    let buff = xml_buf_create();
    xml_buf_set_allocation_scheme(buff, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);
    if (*node).dump_memory(buff, doc, 0, 0) == 0 || !XmlBufRef::from_raw(buff).unwrap().is_ok() {
        xml_free_node(node);
        xml_buf_free(buff);
        return null_mut();
    }

    let resbuf = xml_buf_content(buff);

    xml_free_node(node);
    xml_buf_free(buff);
    resbuf
}

/// Get the successor of a node if available.
///
/// Returns the successor node or NULL
#[doc(alias = "xmlTextReaderGetSuccessor")]
#[cfg(feature = "libxml_reader")]
unsafe extern "C" fn xml_text_reader_get_successor(mut cur: XmlNodePtr) -> XmlNodePtr {
    if cur.is_null() {
        return null_mut(); /* ERROR */
    }
    if let Some(next) = (*cur).next {
        return next.as_ptr();
    }
    'b: while {
        cur = (*cur).parent.map_or(null_mut(), |p| p.as_ptr());
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
unsafe extern "C" fn xml_text_reader_collect_siblings(mut node: XmlNodePtr) -> *mut XmlChar {
    if node.is_null() || (*node).typ == XmlElementType::XmlNamespaceDecl {
        return null_mut();
    }

    // let buffer: XmlBufferPtr = xml_buffer_create();
    let buffer = xml_buf_create();
    if buffer.is_null() {
        return null_mut();
    }
    // xml_buffer_set_allocation_scheme(buffer, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);
    xml_buf_set_allocation_scheme(buffer, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);

    while !node.is_null() {
        match (*node).typ {
            XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode => {
                // xml_buffer_cat(buffer, (*node).content);
                xml_buf_cat(buffer, (*node).content);
            }
            XmlElementType::XmlElementNode => {
                let tmp: *mut XmlChar = xml_text_reader_collect_siblings(
                    (*node).children.map_or(null_mut(), |c| c.as_ptr()),
                );
                // xml_buffer_cat(buffer, tmp);
                xml_buf_cat(buffer, tmp);
                xml_free(tmp as _);
            }
            _ => {}
        }

        node = (*node).next.map_or(null_mut(), |n| n.as_ptr());
    }
    // let ret: *mut XmlChar = (*buffer).content;
    // (*buffer).content = null_mut();
    // xml_buffer_free(buffer);
    let ret = xml_buf_content(buffer);
    xml_buf_free(buffer);
    ret
}

/// Makes sure that the current node is fully read as well as all its descendant.  
/// It means the full DOM subtree must be available at the end of the call.
///
/// Returns 1 if the node was expanded successfully, 0 if there is no more nodes to read,
/// or -1 in case of error
#[doc(alias = "xmlTextReaderDoExpand")]
#[cfg(feature = "libxml_reader")]
unsafe extern "C" fn xml_text_reader_do_expand(reader: &mut XmlTextReader) -> i32 {
    let mut val: i32;

    if reader.node.is_null() || reader.ctxt.is_null() {
        return -1;
    }
    while {
        if matches!((*reader.ctxt).instate, XmlParserInputState::XmlParserEOF) {
            return 1;
        }

        if !xml_text_reader_get_successor(reader.node).is_null() {
            return 1;
        }
        if ((*reader.ctxt).node_tab.len() as i32) < reader.depth {
            return 1;
        }
        if reader.mode == XmlTextReaderMode::XmlTextreaderModeEof as i32 {
            return 1;
        }
        val = xml_text_reader_push_data(reader);
        if val < 0 {
            reader.mode = XmlTextReaderMode::XmlTextreaderModeError as i32;
            return -1;
        }

        reader.mode != XmlTextReaderMode::XmlTextreaderModeEof as i32
    } {}
    1
}

/// Reads the contents of an element or a text node as a string.
///
/// Returns a string containing the contents of the Element or Text node,
/// or NULL if the reader is positioned on any other type of node.
/// The string must be deallocated by the caller.
#[doc(alias = "xmlTextReaderReadString")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_read_string(reader: &mut XmlTextReader) -> *mut XmlChar {
    if reader.node.is_null() {
        return null_mut();
    }

    let node: XmlNodePtr = if !reader.curnode.is_null() {
        reader.curnode
    } else {
        reader.node
    };
    match (*node).typ {
        XmlElementType::XmlTextNode => {
            if !(*node).content.is_null() {
                return xml_strdup((*node).content);
            }
        }
        XmlElementType::XmlElementNode => {
            if xml_text_reader_do_expand(reader) != -1 {
                return xml_text_reader_collect_siblings(
                    (*node).children.map_or(null_mut(), |c| c.as_ptr()),
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
pub unsafe extern "C" fn xml_text_reader_read_attribute_value(reader: &mut XmlTextReader) -> i32 {
    if reader.node.is_null() {
        return -1;
    }
    if reader.curnode.is_null() {
        return 0;
    }
    if (*reader.curnode).typ == XmlElementType::XmlAttributeNode {
        let Some(children) = (*reader.curnode).children else {
            return 0;
        };
        reader.curnode = children.as_ptr();
    } else if (*reader.curnode).typ == XmlElementType::XmlNamespaceDecl {
        let ns: XmlNsPtr = reader.curnode as XmlNsPtr;

        if reader.faketext.is_null() {
            reader.faketext = xml_new_doc_text((*reader.node).doc, (*ns).href);
        } else {
            if !(*reader.faketext).content.is_null()
                && (*reader.faketext).content != addr_of_mut!((*reader.faketext).properties) as _
            {
                xml_free((*reader.faketext).content as _);
            }
            (*reader.faketext).content = xml_strdup((*ns).href);
        }
        reader.curnode = reader.faketext;
    } else {
        let Some(next) = (*reader.curnode).next else {
            return 0;
        };
        reader.curnode = next.as_ptr();
    }
    1
}

/// Provides the number of attributes of the current node
///
/// Returns 0 i no attributes, -1 in case of error or the attribute count
#[doc(alias = "xmlTextReaderAttributeCount")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_attribute_count(reader: &mut XmlTextReader) -> i32 {
    let mut ret: i32;
    let mut attr: XmlAttrPtr;
    let mut ns: XmlNsPtr;

    if reader.node.is_null() {
        return 0;
    }

    let node = if !reader.curnode.is_null() {
        reader.curnode
    } else {
        reader.node
    };

    if (*node).typ != XmlElementType::XmlElementNode {
        return 0;
    }
    if matches!(
        reader.state,
        XmlTextReaderState::End | XmlTextReaderState::Backtrack
    ) {
        return 0;
    }
    ret = 0;
    attr = (*node).properties;
    while !attr.is_null() {
        ret += 1;
        attr = (*attr).next;
    }
    ns = (*node).ns_def;
    while !ns.is_null() {
        ret += 1;
        ns = (*ns).next;
    }
    ret
}

/// The depth of the node in the tree.
///
/// Returns the depth or -1 in case of error
#[doc(alias = "xmlTextReaderDepth")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_depth(reader: &mut XmlTextReader) -> i32 {
    if reader.node.is_null() {
        return 0;
    }

    if !reader.curnode.is_null() {
        if matches!(
            (*reader.curnode).typ,
            XmlElementType::XmlAttributeNode | XmlElementType::XmlNamespaceDecl
        ) {
            return reader.depth + 1;
        }
        return reader.depth + 2;
    }
    reader.depth
}

/// Whether the node has attributes.
///
/// Returns 1 if true, 0 if false, and -1 in case or error
#[doc(alias = "xmlTextReaderHasAttributes")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_has_attributes(reader: &mut XmlTextReader) -> i32 {
    if reader.node.is_null() {
        return 0;
    }
    let node = if !reader.curnode.is_null() {
        reader.curnode
    } else {
        reader.node
    };

    if (*node).typ == XmlElementType::XmlElementNode
        && (!(*node).properties.is_null() || !(*node).ns_def.is_null())
    {
        return 1;
    }
    /* TODO: handle the xmlDecl */
    0
}

/// Whether the node can have a text value.
///
/// Returns 1 if true, 0 if false, and -1 in case or error
#[doc(alias = "xmlTextReaderHasValue")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_has_value(reader: &mut XmlTextReader) -> i32 {
    if reader.node.is_null() {
        return 0;
    }
    let node = if !reader.curnode.is_null() {
        reader.curnode
    } else {
        reader.node
    };

    match (*node).typ {
        XmlElementType::XmlAttributeNode
        | XmlElementType::XmlTextNode
        | XmlElementType::XmlCDATASectionNode
        | XmlElementType::XmlPINode
        | XmlElementType::XmlCommentNode
        | XmlElementType::XmlNamespaceDecl => 1,
        _ => 0,
    }
}

/// Whether an Attribute  node was generated from the default value defined in the DTD or schema.
///
/// Returns 0 if not defaulted, 1 if defaulted, and -1 in case of error
#[doc(alias = "xmlTextReaderIsDefault")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_is_default(_reader: &mut XmlTextReader) -> i32 {
    0
}

/// Check if the current node is empty
///
/// Returns 1 if empty, 0 if not and -1 in case of error
#[doc(alias = "xmlTextReaderIsEmptyElement")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_is_empty_element(reader: &mut XmlTextReader) -> i32 {
    if reader.node.is_null() {
        return -1;
    }
    if (*reader.node).typ != XmlElementType::XmlElementNode {
        return 0;
    }
    if !reader.curnode.is_null() {
        return 0;
    }
    if (*reader.node).children.is_some() {
        return 0;
    }
    if reader.state == XmlTextReaderState::End {
        return 0;
    }
    if !reader.doc.is_null() {
        return 1;
    }
    #[cfg(feature = "xinclude")]
    if reader.in_xinclude > 0 {
        return 1;
    }
    ((*reader.node).extra & NODE_IS_EMPTY as u16 != 0) as i32
}

/// Get the node type of the current node
///
/// Reference:
/// http://www.gnu.org/software/dotgnu/pnetlib-doc/System/Xml/XmlNodeType.html
///
/// Returns the xmlReaderTypes of the current node or -1 in case of error
#[doc(alias = "xmlTextReaderNodeType")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_node_type(reader: &mut XmlTextReader) -> i32 {
    if reader.node.is_null() {
        return XmlReaderTypes::XmlReaderTypeNone as i32;
    }
    let node = if !reader.curnode.is_null() {
        reader.curnode
    } else {
        reader.node
    };
    match (*node).typ {
        XmlElementType::XmlElementNode => {
            if matches!(
                reader.state,
                XmlTextReaderState::End | XmlTextReaderState::Backtrack
            ) {
                return XmlReaderTypes::XmlReaderTypeEndElement as i32;
            }
            XmlReaderTypes::XmlReaderTypeElement as i32
        }
        XmlElementType::XmlNamespaceDecl | XmlElementType::XmlAttributeNode => {
            XmlReaderTypes::XmlReaderTypeAttribute as i32
        }
        XmlElementType::XmlTextNode => {
            if (*reader.node).is_blank_node() {
                if (*reader.node).get_space_preserve() != 0 {
                    XmlReaderTypes::XmlReaderTypeSignificantWhitespace as i32
                } else {
                    XmlReaderTypes::XmlReaderTypeWhitespace as i32
                }
            } else {
                XmlReaderTypes::XmlReaderTypeText as i32
            }
        }
        XmlElementType::XmlCDATASectionNode => XmlReaderTypes::XmlReaderTypeCdata as i32,
        XmlElementType::XmlEntityRefNode => XmlReaderTypes::XmlReaderTypeEntityReference as i32,
        XmlElementType::XmlEntityNode => XmlReaderTypes::XmlReaderTypeEntity as i32,
        XmlElementType::XmlPINode => XmlReaderTypes::XmlReaderTypeProcessingInstruction as i32,
        XmlElementType::XmlCommentNode => XmlReaderTypes::XmlReaderTypeComment as i32,
        XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
            XmlReaderTypes::XmlReaderTypeDocument as i32
        }
        XmlElementType::XmlDocumentFragNode => XmlReaderTypes::XmlReaderTypeDocumentFragment as i32,
        XmlElementType::XmlNotationNode => XmlReaderTypes::XmlReaderTypeNotation as i32,
        XmlElementType::XmlDocumentTypeNode | XmlElementType::XmlDTDNode => {
            XmlReaderTypes::XmlReaderTypeDocumentType as i32
        }

        XmlElementType::XmlElementDecl
        | XmlElementType::XmlAttributeDecl
        | XmlElementType::XmlEntityDecl
        | XmlElementType::XmlXIncludeStart
        | XmlElementType::XmlXIncludeEnd => XmlReaderTypes::XmlReaderTypeNone as i32,
        _ => unreachable!(),
    }
    // return -1;
}

/// The quotation mark character used to enclose the value of an attribute.
///
/// Returns " or ' and -1 in case of error
#[doc(alias = "xmlTextReaderQuoteChar")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_quote_char(_reader: &mut XmlTextReader) -> i32 {
    /* TODO maybe lookup the attribute value for " first */
    b'"' as _
}

/// Gets the read state of the reader.
///
/// Returns the state value, or -1 in case of error
#[doc(alias = "xmlTextReaderReadState")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_read_state(reader: &mut XmlTextReader) -> i32 {
    reader.mode
}

/// Determine whether the current node is a namespace declaration
/// rather than a regular attribute.
///
/// Returns 1 if the current node is a namespace declaration, 0 if it
/// is a regular attribute or other type of node, or -1 in case of error.
#[doc(alias = "xmlTextReaderIsNamespaceDecl")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_is_namespace_decl(reader: &mut XmlTextReader) -> i32 {
    if reader.node.is_null() {
        return -1;
    }
    let node = if !reader.curnode.is_null() {
        reader.curnode
    } else {
        reader.node
    };

    if XmlElementType::XmlNamespaceDecl == (*node).typ {
        1
    } else {
        0
    }
}

/// The base URI of the node.
///
/// Returns the base URI or NULL if not available, the string will be deallocated with the reader
#[doc(alias = "xmlTextReaderConstBaseUri")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_const_base_uri(
    reader: &mut XmlTextReader,
) -> *const XmlChar {
    if reader.node.is_null() {
        return null_mut();
    }
    let tmp: *mut XmlChar = (*reader.node).get_base(null_mut());
    if tmp.is_null() {
        return null_mut();
    }
    let ret: *const XmlChar = CONSTSTR!(reader, tmp);
    xml_free(tmp as _);
    ret
}

/// The local name of the node.
///
/// Returns the local name or NULL if not available, the string will be deallocated with the reader.
#[doc(alias = "xmlTextReaderConstLocalName")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_const_local_name(
    reader: &mut XmlTextReader,
) -> *const XmlChar {
    if reader.node.is_null() {
        return null_mut();
    }
    let node = if !reader.curnode.is_null() {
        reader.curnode
    } else {
        reader.node
    };
    if (*node).typ == XmlElementType::XmlNamespaceDecl {
        let ns: XmlNsPtr = node as XmlNsPtr;
        if (*ns).prefix.is_null() {
            return CONSTSTR!(reader, c"xmlns".as_ptr() as _);
        } else {
            return (*ns).prefix;
        }
    }
    if !matches!(
        (*node).typ,
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
pub unsafe extern "C" fn xml_text_reader_const_name(reader: &mut XmlTextReader) -> *const XmlChar {
    if reader.node.is_null() {
        return null_mut();
    }
    let node = if !reader.curnode.is_null() {
        reader.curnode
    } else {
        reader.node
    };
    match (*node).typ {
        XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode => {
            if (*node).ns.is_null() || (*(*node).ns).prefix.is_null() {
                return (*node).name;
            }
            CONSTQSTR!(reader, (*(*node).ns).prefix, (*node).name)
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
            let ns: XmlNsPtr = node as XmlNsPtr;

            if (*ns).prefix.is_null() {
                return CONSTSTR!(reader, c"xmlns".as_ptr() as _);
            }
            CONSTQSTR!(reader, c"xmlns".as_ptr() as _, (*ns).prefix)
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
pub unsafe extern "C" fn xml_text_reader_const_namespace_uri(
    reader: &mut XmlTextReader,
) -> *const XmlChar {
    if reader.node.is_null() {
        return null_mut();
    }
    let node = if !reader.curnode.is_null() {
        reader.curnode
    } else {
        reader.node
    };
    if (*node).typ == XmlElementType::XmlNamespaceDecl {
        return CONSTSTR!(reader, c"http://www.w3.org/2000/xmlns/".as_ptr() as _);
    }
    if !matches!(
        (*node).typ,
        XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode
    ) {
        return null_mut();
    }
    if !(*node).ns.is_null() {
        return CONSTSTR!(reader, (*(*node).ns).href);
    }
    null_mut()
}

/// A shorthand reference to the namespace associated with the node.
///
/// Returns the prefix or NULL if not available, the string is deallocated with the reader.
#[doc(alias = "xmlTextReaderConstPrefix")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_const_prefix(
    reader: &mut XmlTextReader,
) -> *const XmlChar {
    if reader.node.is_null() {
        return null_mut();
    }
    let node = if !reader.curnode.is_null() {
        reader.curnode
    } else {
        reader.node
    };
    if (*node).typ == XmlElementType::XmlNamespaceDecl {
        let ns: XmlNsPtr = node as XmlNsPtr;
        if (*ns).prefix.is_null() {
            return null_mut();
        }
        return CONSTSTR!(reader, c"xmlns".as_ptr() as _);
    }
    if !matches!(
        (*node).typ,
        XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode
    ) {
        return null_mut();
    }
    if !(*node).ns.is_null() && !(*(*node).ns).prefix.is_null() {
        return CONSTSTR!(reader, (*(*node).ns).prefix);
    }
    null_mut()
}

/// The xml:lang scope within which the node resides.
///
/// Returns the xml:lang value or NULL if none exists.
#[doc(alias = "xmlTextReaderConstXmlLang")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_const_xml_lang(
    reader: &mut XmlTextReader,
) -> *const XmlChar {
    if reader.node.is_null() {
        return null_mut();
    }
    let tmp: *mut XmlChar = (*reader.node).get_lang();
    if tmp.is_null() {
        return null_mut();
    }
    let ret: *const XmlChar = CONSTSTR!(reader, tmp);
    xml_free(tmp as _);
    ret
}

/// Get an interned string from the reader, allows for example to
/// speedup string name comparisons
///
/// Returns an interned copy of the string or NULL in case of error.
/// The string will be deallocated with the reader.
#[doc(alias = "xmlTextReaderConstString")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_const_string(
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
pub unsafe extern "C" fn xml_text_reader_const_value(reader: &mut XmlTextReader) -> *const XmlChar {
    use crate::generic_error;

    if reader.node.is_null() {
        return null_mut();
    }
    let node = if !reader.curnode.is_null() {
        reader.curnode
    } else {
        reader.node
    };

    match (*node).typ {
        XmlElementType::XmlNamespaceDecl => return (*(node as XmlNsPtr)).href,
        XmlElementType::XmlAttributeNode => {
            let attr: XmlAttrPtr = node as XmlAttrPtr;
            let mut ret: *const XmlChar;

            if let Some(children) = (*attr)
                .children
                .filter(|c| c.typ == XmlElementType::XmlTextNode && c.next.is_none())
            {
                return children.content;
            } else {
                if reader.buffer.is_null() {
                    reader.buffer = xml_buf_create_size(100);
                    if reader.buffer.is_null() {
                        generic_error!("xmlTextReaderSetup : malloc failed\n");
                        return null_mut();
                    }
                    xml_buf_set_allocation_scheme(
                        reader.buffer,
                        XmlBufferAllocationScheme::XmlBufferAllocDoubleit,
                    );
                } else {
                    xml_buf_empty(reader.buffer);
                }
                (*node).get_content_to(reader.buffer);
                ret = xml_buf_content(reader.buffer);
                if ret.is_null() {
                    /* error on the buffer best to reallocate */
                    xml_buf_free(reader.buffer);
                    reader.buffer = xml_buf_create_size(100);
                    xml_buf_set_allocation_scheme(
                        reader.buffer,
                        XmlBufferAllocationScheme::XmlBufferAllocDoubleit,
                    );
                    ret = c"".as_ptr() as _;
                }
                return ret;
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

/// The base URI of the node.
///
/// Returns the base URI or NULL if not available, if non NULL it need to be freed by the caller.
#[doc(alias = "xmlTextReaderBaseUri")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_base_uri(reader: &mut XmlTextReader) -> *mut XmlChar {
    if reader.node.is_null() {
        return null_mut();
    }
    (*reader.node).get_base(null_mut())
}

/// The local name of the node.
///
/// Returns the local name or NULL if not available, if non NULL it need to be freed by the caller.
#[doc(alias = "xmlTextReaderLocalName")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_local_name(reader: &mut XmlTextReader) -> *mut XmlChar {
    if reader.node.is_null() {
        return null_mut();
    }
    let node = if !reader.curnode.is_null() {
        reader.curnode
    } else {
        reader.node
    };
    if (*node).typ == XmlElementType::XmlNamespaceDecl {
        let ns: XmlNsPtr = node as XmlNsPtr;
        if (*ns).prefix.is_null() {
            return xml_strdup(c"xmlns".as_ptr() as _);
        } else {
            return xml_strdup((*ns).prefix);
        }
    }
    if !matches!(
        (*node).typ,
        XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode
    ) {
        return xml_text_reader_name(reader);
    }
    xml_strdup((*node).name)
}

/// The qualified name of the node, equal to Prefix :LocalName.
///
/// Returns the local name or NULL if not available,
/// if non NULL it need to be freed by the caller.
#[doc(alias = "xmlTextReaderName")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_name(reader: &mut XmlTextReader) -> *mut XmlChar {
    let mut ret: *mut XmlChar;

    if reader.node.is_null() {
        return null_mut();
    }
    let node = if !reader.curnode.is_null() {
        reader.curnode
    } else {
        reader.node
    };
    match (*node).typ {
        XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode => {
            if (*node).ns.is_null() || (*(*node).ns).prefix.is_null() {
                return xml_strdup((*node).name);
            }

            ret = xml_strdup((*(*node).ns).prefix);
            ret = xml_strcat(ret, c":".as_ptr() as _);
            ret = xml_strcat(ret, (*node).name);
            ret
        }
        XmlElementType::XmlTextNode => xml_strdup(c"#text".as_ptr() as _),
        XmlElementType::XmlCDATASectionNode => xml_strdup(c"#cdata-section".as_ptr() as _),
        XmlElementType::XmlEntityNode | XmlElementType::XmlEntityRefNode => {
            xml_strdup((*node).name)
        }
        XmlElementType::XmlPINode => xml_strdup((*node).name),
        XmlElementType::XmlCommentNode => xml_strdup(c"#comment".as_ptr() as _),
        XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
            xml_strdup(c"#document".as_ptr() as _)
        }
        XmlElementType::XmlDocumentFragNode => xml_strdup(c"#document-fragment".as_ptr() as _),
        XmlElementType::XmlNotationNode => xml_strdup((*node).name),
        XmlElementType::XmlDocumentTypeNode | XmlElementType::XmlDTDNode => {
            xml_strdup((*node).name)
        }
        XmlElementType::XmlNamespaceDecl => {
            let ns: XmlNsPtr = node as XmlNsPtr;

            ret = xml_strdup(c"xmlns".as_ptr() as _);
            if (*ns).prefix.is_null() {
                return ret;
            }
            ret = xml_strcat(ret, c":".as_ptr() as _);
            ret = xml_strcat(ret, (*ns).prefix);
            ret
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
/// Returns the namespace URI or NULL if not available,
/// if non NULL it need to be freed by the caller.
#[doc(alias = "xmlTextReaderNamespaceUri")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_namespace_uri(reader: &mut XmlTextReader) -> *mut XmlChar {
    if reader.node.is_null() {
        return null_mut();
    }
    let node = if !reader.curnode.is_null() {
        reader.curnode
    } else {
        reader.node
    };
    if (*node).typ == XmlElementType::XmlNamespaceDecl {
        return xml_strdup(c"http://www.w3.org/2000/xmlns/".as_ptr() as _);
    }
    if !matches!(
        (*node).typ,
        XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode
    ) {
        return null_mut();
    }
    if !(*node).ns.is_null() {
        return xml_strdup((*(*node).ns).href);
    }
    null_mut()
}

/// A shorthand reference to the namespace associated with the node.
///
/// Returns the prefix or NULL if not available,
/// if non NULL it need to be freed by the caller.
#[doc(alias = "xmlTextReaderPrefix")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_prefix(reader: &mut XmlTextReader) -> *mut XmlChar {
    if reader.node.is_null() {
        return null_mut();
    }
    let node = if !reader.curnode.is_null() {
        reader.curnode
    } else {
        reader.node
    };
    if (*node).typ == XmlElementType::XmlNamespaceDecl {
        let ns: XmlNsPtr = node as XmlNsPtr;
        if (*ns).prefix.is_null() {
            return null_mut();
        }
        return xml_strdup(c"xmlns".as_ptr() as _);
    }
    if !matches!(
        (*node).typ,
        XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode
    ) {
        return null_mut();
    }
    if !(*node).ns.is_null() && !(*(*node).ns).prefix.is_null() {
        return xml_strdup((*(*node).ns).prefix);
    }
    null_mut()
}

/// The xml:lang scope within which the node resides.
///
/// Returns the xml:lang value or NULL if none exists.,
/// if non NULL it need to be freed by the caller.
#[doc(alias = "xmlTextReaderXmlLang")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_xml_lang(reader: &mut XmlTextReader) -> *mut XmlChar {
    if reader.node.is_null() {
        return null_mut();
    }
    (*reader.node).get_lang()
}

/// Provides the text value of the node if present
///
/// Returns the string or NULL if not available. The result must be deallocated
///     with xml_free as _()
#[doc(alias = "xmlTextReaderValue")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_value(reader: &mut XmlTextReader) -> *mut XmlChar {
    if reader.node.is_null() {
        return null_mut();
    }
    let node = if !reader.curnode.is_null() {
        reader.curnode
    } else {
        reader.node
    };

    match (*node).typ {
        XmlElementType::XmlNamespaceDecl => return xml_strdup((*(node as XmlNsPtr)).href),
        XmlElementType::XmlAttributeNode => {
            let attr: XmlAttrPtr = node as XmlAttrPtr;

            if let Some(parent) = (*attr).parent {
                return (*attr)
                    .children
                    .map_or(null_mut(), |c| c.get_string(parent.doc, 1));
            } else {
                return (*attr)
                    .children
                    .map_or(null_mut(), |c| c.get_string(null_mut(), 1));
            }
        }
        XmlElementType::XmlTextNode
        | XmlElementType::XmlCDATASectionNode
        | XmlElementType::XmlPINode
        | XmlElementType::XmlCommentNode => {
            if !(*node).content.is_null() {
                return xml_strdup((*node).content);
            }
        }
        _ => {}
    }
    null_mut()
}

/// Free up all the structures used by a document, tree included.
#[doc(alias = "xmlTextReaderFreeDoc")]
#[cfg(feature = "libxml_reader")]
unsafe extern "C" fn xml_text_reader_free_doc(reader: &mut XmlTextReader, cur: XmlDocPtr) {
    use crate::tree::NodeCommon;

    let mut ext_subset: XmlDtdPtr;

    if cur.is_null() {
        return;
    }

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0 {
        // if let Some(f) = xmlDeregisterNodeDefaultValue {
        //     f(cur as _);
        // }
        xml_deregister_node_default_value(cur as _);
    }

    /*
     * Do this before freeing the children list to avoid ID lookups
     */
    if !(*cur).ids.is_null() {
        xml_free_id_table((*cur).ids as XmlIDTablePtr);
    }
    (*cur).ids = null_mut();
    if !(*cur).refs.is_null() {
        xml_free_ref_table((*cur).refs as XmlRefTablePtr);
    }
    (*cur).refs = null_mut();
    ext_subset = (*cur).ext_subset;
    let int_subset: XmlDtdPtr = (*cur).int_subset;
    if int_subset == ext_subset {
        ext_subset = null_mut();
    }
    if !ext_subset.is_null() {
        (*(*cur).ext_subset).unlink();
        (*cur).ext_subset = null_mut();
        xml_free_dtd(ext_subset);
    }
    if !int_subset.is_null() {
        (*(*cur).int_subset).unlink();
        (*cur).int_subset = null_mut();
        xml_free_dtd(int_subset);
    }

    if let Some(children) = (*cur).children {
        xml_text_reader_free_node_list(reader, children.as_ptr());
    }

    (*cur).version = None;
    if !(*cur).name.is_null() {
        xml_free((*cur).name as _);
    }
    (*cur).encoding = None;
    if !(*cur).old_ns.is_null() {
        xml_free_ns_list((*cur).old_ns);
    }
    (*cur).url = None;
    if !(*cur).dict.is_null() {
        xml_dict_free((*cur).dict);
    }

    xml_free(cur as _);
}

/// This method releases any resources allocated by the current instance
/// changes the state to Closed and close any underlying input.
///
/// Returns 0 or -1 in case of error
#[doc(alias = "xmlTextReaderClose")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_close(reader: &mut XmlTextReader) -> i32 {
    reader.node = null_mut();
    reader.curnode = null_mut();
    reader.mode = XmlTextReaderMode::XmlTextreaderModeClosed as i32;
    if !reader.faketext.is_null() {
        xml_free_node(reader.faketext);
        reader.faketext = null_mut();
    }
    if !reader.ctxt.is_null() {
        #[cfg(feature = "valid")]
        if !(*reader.ctxt).vctxt.vstate_tab.is_null() && (*reader.ctxt).vctxt.vstate_max > 0 {
            #[cfg(feature = "regexp")]
            while (*reader.ctxt).vctxt.vstate_nr > 0 {
                xml_validate_pop_element(
                    addr_of_mut!((*reader.ctxt).vctxt),
                    null_mut(),
                    null_mut(),
                    null_mut(),
                );
            }
            xml_free((*reader.ctxt).vctxt.vstate_tab as _);
            (*reader.ctxt).vctxt.vstate_tab = null_mut();
            (*reader.ctxt).vctxt.vstate_max = 0;
        }
        (*reader.ctxt).stop();
        if !(*reader.ctxt).my_doc.is_null() {
            if reader.preserve == 0 {
                xml_text_reader_free_doc(reader, (*reader.ctxt).my_doc);
            }
            (*reader.ctxt).my_doc = null_mut();
        }
    }
    if reader.input.is_some() && reader.allocs & XML_TEXTREADER_INPUT != 0 {
        // xml_free_parser_input_buffer((*reader).input);
        let _ = reader.input.take();
        reader.allocs -= XML_TEXTREADER_INPUT;
    }
    0
}

/// Provides the value of the attribute with the specified index relative
/// to the containing element.
///
/// Returns a string containing the value of the specified attribute, or NULL in case of error.  
/// The string must be deallocated by the caller.
#[doc(alias = "xmlTextReaderGetAttributeNo")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_get_attribute_no(
    reader: &mut XmlTextReader,
    no: i32,
) -> *mut XmlChar {
    let mut cur: XmlAttrPtr;
    let mut ns: XmlNsPtr;

    if reader.node.is_null() {
        return null_mut();
    }
    if !reader.curnode.is_null() {
        return null_mut();
    }
    /* TODO: handle the xmlDecl */
    if (*reader.node).typ != XmlElementType::XmlElementNode {
        return null_mut();
    }

    ns = (*reader.node).ns_def;
    let mut i = 0;
    while i < no && !ns.is_null() {
        ns = (*ns).next;
        i += 1;
    }

    if !ns.is_null() {
        return xml_strdup((*ns).href);
    }
    cur = (*reader.node).properties;
    if cur.is_null() {
        return null_mut();
    }

    for _ in i..no {
        cur = (*cur).next;
        if cur.is_null() {
            return null_mut();
        }
    }
    /* TODO walk the DTD if present */

    let ret: *mut XmlChar = (*cur)
        .children
        .map_or(null_mut(), |c| c.get_string((*reader.node).doc, 1));
    if ret.is_null() {
        return xml_strdup(c"".as_ptr() as _);
    }
    ret
}

/// Provides the value of the attribute with the specified qualified name.
///
/// Returns a string containing the value of the specified attribute, or NULL in case of error.  
/// The string must be deallocated by the caller.
#[doc(alias = "xmlTextReaderGetAttribute")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_get_attribute(
    reader: &mut XmlTextReader,
    name: *const XmlChar,
) -> *mut XmlChar {
    use std::ffi::CStr;

    let mut prefix: *mut XmlChar = null_mut();
    let mut ns: XmlNsPtr;
    let mut ret: *mut XmlChar = null_mut();

    if name.is_null() {
        return null_mut();
    }
    if reader.node.is_null() {
        return null_mut();
    }
    if !reader.curnode.is_null() {
        return null_mut();
    }

    /* TODO: handle the xmlDecl */
    if (*reader.node).typ != XmlElementType::XmlElementNode {
        return null_mut();
    }

    let localname: *mut XmlChar = xml_split_qname2(name, addr_of_mut!(prefix));
    if localname.is_null() {
        /*
         * Namespace default decl
         */
        if xml_str_equal(name, c"xmlns".as_ptr() as _) {
            ns = (*reader.node).ns_def;
            while !ns.is_null() {
                if (*ns).prefix.is_null() {
                    return xml_strdup((*ns).href);
                }
                ns = (*ns).next;
            }
            return null_mut();
        }
        return (*reader.node)
            .get_no_ns_prop(CStr::from_ptr(name as *const i8).to_string_lossy().as_ref());
    }

    /*
     * Namespace default decl
     */
    if xml_str_equal(prefix, c"xmlns".as_ptr() as _) {
        ns = (*reader.node).ns_def;
        while !ns.is_null() {
            if !(*ns).prefix.is_null() && xml_str_equal((*ns).prefix, localname) {
                ret = xml_strdup((*ns).href);
                break;
            }
            ns = (*ns).next;
        }
    } else {
        ns = (*reader.node).search_ns(
            (*reader.node).doc,
            (!prefix.is_null())
                .then(|| CStr::from_ptr(prefix as *const i8).to_string_lossy())
                .as_deref(),
        );
        if !ns.is_null() {
            let href = (*ns).href;
            ret = (*reader.node).get_ns_prop(
                CStr::from_ptr(localname as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                (!href.is_null())
                    .then(|| CStr::from_ptr(href as *const i8).to_string_lossy())
                    .as_deref(),
            );
        }
    }

    xml_free(localname as _);
    if !prefix.is_null() {
        xml_free(prefix as _);
    }
    ret
}

/// Provides the value of the specified attribute
///
/// Returns a string containing the value of the specified attribute, or NULL in case of error.  
/// The string must be deallocated by the caller.
#[doc(alias = "xmlTextReaderGetAttributeNs")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_get_attribute_ns(
    reader: &mut XmlTextReader,
    local_name: *const XmlChar,
    namespace_uri: *const XmlChar,
) -> *mut XmlChar {
    use std::ffi::CStr;

    let mut prefix: *mut XmlChar = null_mut();
    let mut ns: XmlNsPtr;

    if local_name.is_null() {
        return null_mut();
    }
    if reader.node.is_null() {
        return null_mut();
    }
    if !reader.curnode.is_null() {
        return null_mut();
    }

    /* TODO: handle the xmlDecl */
    if (*reader.node).typ != XmlElementType::XmlElementNode {
        return null_mut();
    }

    if xml_str_equal(
        namespace_uri,
        c"http://www.w3.org/2000/xmlns/".as_ptr() as _,
    ) {
        if !xml_str_equal(local_name, c"xmlns".as_ptr() as _) {
            prefix = local_name as _;
        }
        ns = (*reader.node).ns_def;
        while !ns.is_null() {
            if (prefix.is_null() && (*ns).prefix.is_null())
                || (!(*ns).prefix.is_null() && xml_str_equal((*ns).prefix, local_name))
            {
                return xml_strdup((*ns).href);
            }
            ns = (*ns).next;
        }
        return null_mut();
    }

    (*reader.node).get_ns_prop(
        CStr::from_ptr(local_name as *const i8)
            .to_string_lossy()
            .as_ref(),
        (!namespace_uri.is_null())
            .then(|| CStr::from_ptr(namespace_uri as *const i8).to_string_lossy())
            .as_deref(),
    )
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
pub unsafe fn xml_text_reader_get_remainder(
    reader: &mut XmlTextReader,
) -> Option<XmlParserInputBuffer> {
    if reader.node.is_null() {
        return None;
    }

    reader.node = null_mut();
    reader.curnode = null_mut();
    reader.mode = XmlTextReaderMode::XmlTextreaderModeEof as i32;
    if !reader.ctxt.is_null() {
        (*reader.ctxt).stop();
        if !(*reader.ctxt).my_doc.is_null() {
            if reader.preserve == 0 {
                xml_text_reader_free_doc(reader, (*reader.ctxt).my_doc);
            }
            (*reader.ctxt).my_doc = null_mut();
        }
    }
    if reader.allocs & XML_TEXTREADER_INPUT != 0 {
        reader.allocs -= XML_TEXTREADER_INPUT;
        reader.input.take()
    } else {
        /*
         * Hum, one may need to duplicate the data structure because
         * without reference counting the input may be freed twice:
         *   - by the layer which allocated it.
         *   - by the layer to which would have been returned to.
         */
        // TODO
        None
    }
}

/// Resolves a namespace prefix in the scope of the current element.
///
/// Returns a string containing the namespace URI to which the prefix maps or NULL in case of error.  
/// The string must be deallocated by the caller.
#[doc(alias = "xmlTextReaderLookupNamespace")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_lookup_namespace(
    reader: &mut XmlTextReader,
    prefix: *const XmlChar,
) -> *mut XmlChar {
    use std::ffi::CStr;

    if reader.node.is_null() {
        return null_mut();
    }

    let ns: XmlNsPtr = (*reader.node).search_ns(
        (*reader.node).doc,
        (!prefix.is_null())
            .then(|| CStr::from_ptr(prefix as *const i8).to_string_lossy())
            .as_deref(),
    );
    if ns.is_null() {
        return null_mut();
    }
    xml_strdup((*ns).href)
}

/// Moves the position of the current instance to the attribute with
/// the specified index relative to the containing element.
///
/// Returns 1 in case of success, -1 in case of error, 0 if not found
#[doc(alias = "xmlTextReaderMoveToAttributeNo")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_move_to_attribute_no(
    reader: &mut XmlTextReader,
    no: i32,
) -> i32 {
    let mut cur: XmlAttrPtr;
    let mut ns: XmlNsPtr;

    if reader.node.is_null() {
        return -1;
    }
    /* TODO: handle the xmlDecl */
    if (*reader.node).typ != XmlElementType::XmlElementNode {
        return -1;
    }

    reader.curnode = null_mut();

    ns = (*reader.node).ns_def;
    let mut i = 0;
    while i < no && !ns.is_null() {
        ns = (*ns).next;
        i += 1;
    }

    if !ns.is_null() {
        reader.curnode = ns as XmlNodePtr;
        return 1;
    }

    cur = (*reader.node).properties;
    if cur.is_null() {
        return 0;
    }

    for _ in i..no {
        cur = (*cur).next;
        if cur.is_null() {
            return 0;
        }
    }
    /* TODO walk the DTD if present */

    reader.curnode = cur as XmlNodePtr;
    1
}

/// Moves the position of the current instance to the attribute with the specified qualified name.
///
/// Returns 1 in case of success, -1 in case of error, 0 if not found
#[doc(alias = "xmlTextReaderMoveToAttribute")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_move_to_attribute(
    reader: &mut XmlTextReader,
    name: *const XmlChar,
) -> i32 {
    let mut prefix: *mut XmlChar = null_mut();
    let mut ns: XmlNsPtr;
    let mut prop: XmlAttrPtr;

    if name.is_null() {
        return -1;
    }
    if reader.node.is_null() {
        return -1;
    }

    /* TODO: handle the xmlDecl */
    if (*reader.node).typ != XmlElementType::XmlElementNode {
        return 0;
    }

    let localname: *mut XmlChar = xml_split_qname2(name, addr_of_mut!(prefix));
    if localname.is_null() {
        /*
         * Namespace default decl
         */
        if xml_str_equal(name, c"xmlns".as_ptr() as _) {
            ns = (*reader.node).ns_def;
            while !ns.is_null() {
                if (*ns).prefix.is_null() {
                    reader.curnode = ns as XmlNodePtr;
                    return 1;
                }
                ns = (*ns).next;
            }
            return 0;
        }

        prop = (*reader.node).properties;
        while !prop.is_null() {
            /*
             * One need to have
             *   - same attribute names
             *   - and the attribute carrying that namespace
             */
            if xml_str_equal((*prop).name, name)
                && ((*prop).ns.is_null() || (*(*prop).ns).prefix.is_null())
            {
                reader.curnode = prop as XmlNodePtr;
                return 1;
            }
            prop = (*prop).next;
        }
        return 0;
    }

    /*
     * Namespace default decl
     */
    if xml_str_equal(prefix, c"xmlns".as_ptr() as _) {
        ns = (*reader.node).ns_def;
        while !ns.is_null() {
            if !(*ns).prefix.is_null() && xml_str_equal((*ns).prefix, localname) {
                reader.curnode = ns as XmlNodePtr;
                // goto found;
                if !localname.is_null() {
                    xml_free(localname as _);
                }
                if !prefix.is_null() {
                    xml_free(prefix as _);
                }
                return 1;
            }
            ns = (*ns).next;
        }
    // goto not_found;
    } else {
        prop = (*reader.node).properties;
        while !prop.is_null() {
            /*
             * One need to have
             *   - same attribute names
             *   - and the attribute carrying that namespace
             */
            if xml_str_equal((*prop).name, localname)
                && !(*prop).ns.is_null()
                && xml_str_equal((*(*prop).ns).prefix, prefix)
            {
                reader.curnode = prop as XmlNodePtr;
                // goto found;
                if !localname.is_null() {
                    xml_free(localname as _);
                }
                if !prefix.is_null() {
                    xml_free(prefix as _);
                }
                return 1;
            }
            prop = (*prop).next;
        }
    }
    // not_found:
    if !localname.is_null() {
        xml_free(localname as _);
    }
    if !prefix.is_null() {
        xml_free(prefix as _);
    }
    0
}

/// Moves the position of the current instance to the attribute with the
/// specified local name and namespace URI.
///
/// Returns 1 in case of success, -1 in case of error, 0 if not found
#[doc(alias = "xmlTextReaderMoveToAttributeNs")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_move_to_attribute_ns(
    reader: &mut XmlTextReader,
    local_name: *const XmlChar,
    namespace_uri: *const XmlChar,
) -> i32 {
    let mut prop: XmlAttrPtr;
    let mut ns: XmlNsPtr;
    let mut prefix: *mut XmlChar = null_mut();

    if local_name.is_null() || namespace_uri.is_null() {
        return -1;
    }
    if reader.node.is_null() {
        return -1;
    }
    if (*reader.node).typ != XmlElementType::XmlElementNode {
        return 0;
    }
    let node: XmlNodePtr = reader.node;

    if xml_str_equal(
        namespace_uri,
        c"http://www.w3.org/2000/xmlns/".as_ptr() as _,
    ) {
        if !xml_str_equal(local_name, c"xmlns".as_ptr() as _) {
            prefix = local_name as _;
        }
        ns = (*reader.node).ns_def;
        while !ns.is_null() {
            if (prefix.is_null() && (*ns).prefix.is_null())
                || (!(*ns).prefix.is_null() && xml_str_equal((*ns).prefix, local_name))
            {
                reader.curnode = ns as XmlNodePtr;
                return 1;
            }
            ns = (*ns).next;
        }
        return 0;
    }

    prop = (*node).properties;
    while !prop.is_null() {
        /*
         * One need to have
         *   - same attribute names
         *   - and the attribute carrying that namespace
         */
        if xml_str_equal((*prop).name, local_name)
            && (!(*prop).ns.is_null() && xml_str_equal((*(*prop).ns).href, namespace_uri))
        {
            reader.curnode = prop as XmlNodePtr;
            return 1;
        }
        prop = (*prop).next;
    }
    0
}

/// Moves the position of the current instance to the first attribute
/// associated with the current node.
///
/// Returns 1 in case of success, -1 in case of error, 0 if not found
#[doc(alias = "xmlTextReaderMoveToFirstAttribute")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_move_to_first_attribute(
    reader: &mut XmlTextReader,
) -> i32 {
    if reader.node.is_null() {
        return -1;
    }
    if (*reader.node).typ != XmlElementType::XmlElementNode {
        return 0;
    }

    if !(*reader.node).ns_def.is_null() {
        reader.curnode = (*reader.node).ns_def as XmlNodePtr;
        return 1;
    }
    if !(*reader.node).properties.is_null() {
        reader.curnode = (*reader.node).properties as XmlNodePtr;
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
pub unsafe extern "C" fn xml_text_reader_move_to_next_attribute(reader: &mut XmlTextReader) -> i32 {
    if reader.node.is_null() {
        return -1;
    }
    if (*reader.node).typ != XmlElementType::XmlElementNode {
        return 0;
    }
    if reader.curnode.is_null() {
        return xml_text_reader_move_to_first_attribute(reader);
    }

    if (*reader.curnode).typ == XmlElementType::XmlNamespaceDecl {
        let ns: XmlNsPtr = reader.curnode as XmlNsPtr;
        if !(*ns).next.is_null() {
            reader.curnode = (*ns).next as XmlNodePtr;
            return 1;
        }
        if !(*reader.node).properties.is_null() {
            reader.curnode = (*reader.node).properties as XmlNodePtr;
            return 1;
        }
        return 0;
    } else if let Some(next) = (*reader.curnode)
        .next
        .filter(|_| (*reader.curnode).typ == XmlElementType::XmlAttributeNode)
    {
        reader.curnode = next.as_ptr();
        return 1;
    }
    0
}

/// Moves the position of the current instance to the node that
/// contains the current Attribute node.
///
/// Returns 1 in case of success, -1 in case of error, 0 if not moved
#[doc(alias = "xmlTextReaderMoveToElement")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_move_to_element(reader: &mut XmlTextReader) -> i32 {
    if reader.node.is_null() {
        return -1;
    }
    if (*reader.node).typ != XmlElementType::XmlElementNode {
        return 0;
    }
    if !reader.curnode.is_null() {
        reader.curnode = null_mut();
        return 1;
    }
    0
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
pub fn xml_text_reader_normalization(_reader: &mut XmlTextReader) -> i32 {
    1
}

/// Determine the encoding of the document being read.
///
/// Returns a string containing the encoding of the document or NULL in case of error.  
/// The string is deallocated with the reader.
#[doc(alias = "xmlTextReaderConstEncoding")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_const_encoding(
    reader: &mut XmlTextReader,
) -> *const XmlChar {
    use std::ffi::CString;

    let mut doc: XmlDocPtr = null_mut();
    if !reader.doc.is_null() {
        doc = reader.doc;
    } else if !reader.ctxt.is_null() {
        doc = (*reader.ctxt).my_doc;
    }
    if doc.is_null() {
        return null_mut();
    }

    if let Some(encoding) = (*doc).encoding.as_deref() {
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
pub unsafe extern "C" fn xml_text_reader_set_parser_prop(
    reader: &mut XmlTextReader,
    prop: i32,
    value: i32,
) -> i32 {
    if reader.ctxt.is_null() {
        return -1;
    }
    let ctxt: XmlParserCtxtPtr = reader.ctxt;

    match XmlParserProperties::try_from(prop) {
        Ok(XmlParserProperties::XmlParserLoaddtd) => {
            if value != 0 {
                if (*ctxt).loadsubset == 0 {
                    if reader.mode != XmlTextReaderMode::XmlTextreaderModeInitial as i32 {
                        return -1;
                    }
                    (*ctxt).loadsubset = XML_DETECT_IDS as i32;
                }
            } else {
                (*ctxt).loadsubset = 0;
            }
            0
        }
        Ok(XmlParserProperties::XmlParserDefaultattrs) => {
            if value != 0 {
                (*ctxt).loadsubset |= XML_COMPLETE_ATTRS as i32;
            } else if (*ctxt).loadsubset & XML_COMPLETE_ATTRS as i32 != 0 {
                (*ctxt).loadsubset -= XML_COMPLETE_ATTRS as i32;
            }
            0
        }
        Ok(XmlParserProperties::XmlParserValidate) => {
            if value != 0 {
                (*ctxt).options |= XmlParserOption::XmlParseDtdvalid as i32;
                (*ctxt).validate = 1;
                reader.validate = XmlTextReaderValidate::ValidateDtd;
            } else {
                (*ctxt).options &= !(XmlParserOption::XmlParseDtdvalid as i32);
                (*ctxt).validate = 0;
            }
            0
        }
        Ok(XmlParserProperties::XmlParserSubstEntities) => {
            if value != 0 {
                (*ctxt).options |= XmlParserOption::XmlParseNoent as i32;
                (*ctxt).replace_entities = 1;
            } else {
                (*ctxt).options &= !(XmlParserOption::XmlParseNoent as i32);
                (*ctxt).replace_entities = 0;
            }
            0
        }
        _ => -1,
    }
}

/// Read the parser internal property.
///
/// Returns the value, usually 0 or 1, or -1 in case of error.
#[doc(alias = "xmlTextReaderGetParserProp")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_get_parser_prop(
    reader: &mut XmlTextReader,
    prop: i32,
) -> i32 {
    if reader.ctxt.is_null() {
        return -1;
    }
    let ctxt: XmlParserCtxtPtr = reader.ctxt;

    match XmlParserProperties::try_from(prop) {
        Ok(XmlParserProperties::XmlParserLoaddtd) => {
            if (*ctxt).loadsubset != 0 || (*ctxt).validate != 0 {
                return 1;
            }
            0
        }
        Ok(XmlParserProperties::XmlParserDefaultattrs) => {
            if (*ctxt).loadsubset & XML_COMPLETE_ATTRS as i32 != 0 {
                return 1;
            }
            0
        }
        Ok(XmlParserProperties::XmlParserValidate) => reader.validate as i32,
        Ok(XmlParserProperties::XmlParserSubstEntities) => (*ctxt).replace_entities,
        _ => -1,
    }
}

/// Hacking interface allowing to get the xmlNodePtr corresponding to the
/// current node being accessed by the xmlTextReader. This is dangerous
/// because the underlying node may be destroyed on the next Reads.
///
/// Returns the xmlNodePtr or NULL in case of error.
#[doc(alias = "xmlTextReaderCurrentNode")]
#[cfg(feature = "libxml_reader")]
pub fn xml_text_reader_current_node(reader: &mut XmlTextReader) -> XmlNodePtr {
    if !reader.curnode.is_null() {
        return reader.curnode;
    }
    reader.node
}

/// Provide the line number of the current parsing point.
///
/// Returns an int or 0 if not available
#[doc(alias = "xmlTextReaderGetParserLineNumber")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_get_parser_line_number(reader: &mut XmlTextReader) -> i32 {
    if reader.ctxt.is_null() || (*reader.ctxt).input.is_null() {
        return 0;
    }
    (*(*reader.ctxt).input).line
}

/// Provide the column number of the current parsing point.
///
/// Returns an int or 0 if not available
#[doc(alias = "xmlTextReaderGetParserColumnNumber")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_get_parser_column_number(
    reader: &mut XmlTextReader,
) -> i32 {
    if reader.ctxt.is_null() || (*reader.ctxt).input.is_null() {
        return 0;
    }
    (*(*reader.ctxt).input).col
}

/// This tells the XML Reader to preserve the current node.
/// The caller must also use xmlTextReaderCurrentDoc() to
/// keep an handle on the resulting document once parsing has finished
///
/// Returns the xmlNodePtr or NULL in case of error.
#[doc(alias = "xmlTextReaderPreserve")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_preserve(reader: &mut XmlTextReader) -> XmlNodePtr {
    let cur = if !reader.curnode.is_null() {
        reader.curnode
    } else {
        reader.node
    };
    if cur.is_null() {
        return null_mut();
    }

    if !matches!(
        (*cur).typ,
        XmlElementType::XmlDocumentNode | XmlElementType::XmlDTDNode
    ) {
        (*cur).extra |= NODE_IS_PRESERVED as u16;
        (*cur).extra |= NODE_IS_SPRESERVED as u16;
    }
    reader.preserves += 1;

    let mut parent = (*cur).parent;
    while let Some(mut now) = parent {
        if now.typ == XmlElementType::XmlElementNode {
            now.extra |= NODE_IS_PRESERVED as u16;
        }
        parent = now.parent;
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
pub unsafe extern "C" fn xml_text_reader_preserve_pattern(
    reader: &mut XmlTextReader,
    pattern: *const XmlChar,
    namespaces: *mut *const XmlChar,
) -> i32 {
    use crate::generic_error;

    use super::globals::xml_realloc;

    if pattern.is_null() {
        return -1;
    }

    let comp: XmlPatternPtr = xml_patterncompile(pattern, reader.dict, 0, namespaces);
    if comp.is_null() {
        return -1;
    }

    if reader.pattern_max <= 0 {
        reader.pattern_max = 4;
        reader.pattern_tab =
            xml_malloc(reader.pattern_max as usize * size_of_val(&*reader.pattern_tab.add(0)))
                as *mut XmlPatternPtr;
        if reader.pattern_tab.is_null() {
            generic_error!("xmlMalloc failed !\n");
            return -1;
        }
    }
    if reader.pattern_nr >= reader.pattern_max {
        reader.pattern_max *= 2;
        let tmp: *mut XmlPatternPtr = xml_realloc(
            reader.pattern_tab as _,
            reader.pattern_max as usize * size_of_val(&*reader.pattern_tab.add(0)),
        ) as *mut XmlPatternPtr;
        if tmp.is_null() {
            generic_error!("xmlRealloc failed !\n");
            reader.pattern_max /= 2;
            return -1;
        }
        reader.pattern_tab = tmp;
    }
    *reader.pattern_tab.add(reader.pattern_nr as usize) = comp;
    let res = reader.pattern_nr;
    reader.pattern_nr += 1;
    res
}

/// Hacking interface allowing to get the xmlDocPtr corresponding to the
/// current document being accessed by the xmlTextReader.
///
/// # NOTE
/// As a result of this call, the reader will not destroy the
/// associated XML document and calling xmlFreeDoc() on the result
/// is needed once the reader parsing has finished.
///
/// Returns the xmlDocPtr or NULL in case of error.
#[doc(alias = "xmlTextReaderCurrentDoc")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_current_doc(reader: &mut XmlTextReader) -> XmlDocPtr {
    if !reader.doc.is_null() {
        return reader.doc;
    }
    if reader.ctxt.is_null() || (*reader.ctxt).my_doc.is_null() {
        return null_mut();
    }

    reader.preserve = 1;
    (*reader.ctxt).my_doc
}

/// Reads the contents of the current node and the full subtree. It then makes
/// the subtree available until the next xmlTextReaderRead() call
///
/// Returns a node pointer valid until the next xmlTextReaderRead() call or NULL in case of error.
#[doc(alias = "xmlTextReaderExpand")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_expand(reader: &mut XmlTextReader) -> XmlNodePtr {
    if reader.node.is_null() {
        return null_mut();
    }
    if !reader.doc.is_null() {
        return reader.node;
    }
    if reader.ctxt.is_null() {
        return null_mut();
    }
    if xml_text_reader_do_expand(reader) < 0 {
        return null_mut();
    }
    reader.node
}

#[cfg(feature = "libxml_reader")]
unsafe extern "C" fn xml_text_reader_next_tree(reader: &mut XmlTextReader) -> i32 {
    if reader.state == XmlTextReaderState::End {
        return 0;
    }

    if reader.node.is_null() {
        let Some(children) = (*reader.doc).children else {
            reader.state = XmlTextReaderState::End;
            return 0;
        };

        reader.node = children.as_ptr();
        reader.state = XmlTextReaderState::Start;
        return 1;
    }

    if reader.state != XmlTextReaderState::Backtrack {
        /* Here removed traversal to child, because we want to skip the subtree,
        replace with traversal to sibling to skip subtree */
        if let Some(next) = (*reader.node).next {
            /* Move to sibling if present,skipping sub-tree */
            reader.node = next.as_ptr();
            reader.state = XmlTextReaderState::Start;
            return 1;
        }

        /* if (*(*reader).node).next is NULL mean no subtree for current node,
        so need to move to sibling of parent node if present */
        reader.state = XmlTextReaderState::Backtrack;
        /* This will move to parent if present */
        xml_text_reader_read(reader);
    }

    if let Some(next) = (*reader.node).next {
        reader.node = next.as_ptr();
        reader.state = XmlTextReaderState::Start;
        return 1;
    }

    if let Some(parent) = (*reader.node).parent {
        if parent.typ == XmlElementType::XmlDocumentNode {
            reader.state = XmlTextReaderState::End;
            return 0;
        }

        reader.node = parent.as_ptr();
        reader.depth -= 1;
        reader.state = XmlTextReaderState::Backtrack;
        /* Repeat process to move to sibling of parent node if present */
        xml_text_reader_next_tree(reader);
    }

    reader.state = XmlTextReaderState::End;

    1
}

/// Skip to the node following the current one in document order while
/// avoiding the subtree if any.
///
/// Returns 1 if the node was read successfully, 0 if there is no more nodes to read,
/// or -1 in case of error
#[doc(alias = "xmlTextReaderNext")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_next(reader: &mut XmlTextReader) -> i32 {
    let mut ret: i32;

    if !reader.doc.is_null() {
        return xml_text_reader_next_tree(reader);
    }
    let cur: XmlNodePtr = reader.node;
    if cur.is_null() || (*cur).typ != XmlElementType::XmlElementNode {
        return xml_text_reader_read(reader);
    }
    if matches!(
        reader.state,
        XmlTextReaderState::End | XmlTextReaderState::Backtrack
    ) {
        return xml_text_reader_read(reader);
    }
    if (*cur).extra & NODE_IS_EMPTY as u16 != 0 {
        return xml_text_reader_read(reader);
    }
    while {
        ret = xml_text_reader_read(reader);
        if ret != 1 {
            return ret;
        }
        reader.node != cur
    } {}
    xml_text_reader_read(reader)
}

/// Skip to the node following the current one in document order while avoiding the subtree if any.
/// Currently implemented only for Readers built on a document
///
/// Returns 1 if the node was read successfully, 0 if there is no more nodes to read,
/// or -1 in case of error
#[doc(alias = "xmlTextReaderNextSibling")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_next_sibling(reader: &mut XmlTextReader) -> i32 {
    if reader.doc.is_null() {
        /* TODO */
        return -1;
    }

    if reader.state == XmlTextReaderState::End {
        return 0;
    }

    if reader.node.is_null() {
        return xml_text_reader_next_tree(reader);
    }

    if let Some(next) = (*reader.node).next {
        reader.node = next.as_ptr();
        reader.state = XmlTextReaderState::Start;
        return 1;
    }

    0
}

/// Retrieve the validity status from the parser context
///
/// Returns the flag value 1 if valid, 0 if no, and -1 in case of error
#[doc(alias = "xmlTextReaderIsValid")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_is_valid(reader: &mut XmlTextReader) -> i32 {
    #[cfg(feature = "schema")]
    {
        if reader.validate == XmlTextReaderValidate::ValidateRng {
            return (reader.rng_valid_errors == 0) as i32;
        }
        if reader.validate == XmlTextReaderValidate::ValidateXsd {
            return (reader.xsd_valid_errors == 0) as i32;
        }
    }
    if !reader.ctxt.is_null() && (*reader.ctxt).validate == 1 {
        return (*reader.ctxt).valid;
    }
    0
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
unsafe extern "C" fn xml_text_reader_relaxng_validate_internal(
    reader: XmlTextReaderPtr,
    rng: *const c_char,
    ctxt: XmlRelaxNGValidCtxtPtr,
    _options: i32,
) -> i32 {
    if reader.is_null() {
        return -1;
    }

    if !rng.is_null() && !ctxt.is_null() {
        return -1;
    }

    if (!rng.is_null() || !ctxt.is_null())
        && ((*reader).mode != XmlTextReaderMode::XmlTextreaderModeInitial as i32
            || (*reader).ctxt.is_null())
    {
        return -1;
    }

    /* Cleanup previous validation stuff. */
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
        /* We just want to deactivate the validation, so get out. */
        return 0;
    }

    if !rng.is_null() {
        /* Parse the schema and create validation environment. */

        let pctxt: XmlRelaxNGParserCtxtPtr = xml_relaxng_new_parser_ctxt(rng);
        let ctx = GenericErrorContext::new(reader);
        if (*reader).error_func.is_some() {
            xml_relaxng_set_parser_errors(
                pctxt,
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
        /* Use the given validation context. */
        (*reader).rng_valid_ctxt = ctxt;
        (*reader).rng_preserve_ctxt = 1;
    }
    /*
     * Redirect the validation context's error channels to use
     * the reader channels.
     * TODO: In case the user provides the validation context we
     *    could make this redirection optional.
     */
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
pub unsafe extern "C" fn xml_text_reader_relaxng_validate(
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
pub unsafe extern "C" fn xml_text_reader_relaxng_validate_ctxt(
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
pub unsafe extern "C" fn xml_text_reader_relaxng_set_schema(
    reader: XmlTextReaderPtr,
    schema: XmlRelaxNGPtr,
) -> i32 {
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
    if (*reader).mode != XmlTextReaderMode::XmlTextreaderModeInitial as i32 {
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
unsafe extern "C" fn xml_text_reader_locator(
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
            let doc: XmlDocPtr = (*(*reader).node).doc;
            if !doc.is_null() && (*doc).url.is_some() {
                *file = (*doc).url.clone();
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
unsafe extern "C" fn xml_text_reader_schema_validate_internal(
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
        && ((*reader).mode != XmlTextReaderMode::XmlTextreaderModeInitial as i32
            || (*reader).ctxt.is_null())
    {
        return -1;
    }

    /* Cleanup previous validation stuff. */
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
        /* We just want to deactivate the validation, so get out. */
        return 0;
    }

    let ctx = GenericErrorContext::new(reader);
    if !xsd.is_null() {
        /* Parse the schema and create validation environment. */
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
            addr_of_mut!((*(*reader).ctxt).sax),
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
        /* Use the given validation context. */
        (*reader).xsd_valid_ctxt = ctxt;
        (*reader).xsd_preserve_ctxt = 1;
        (*reader).xsd_plug = xml_schema_sax_plug(
            (*reader).xsd_valid_ctxt,
            addr_of_mut!((*(*reader).ctxt).sax),
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
    /*
     * Redirect the validation context's error channels to use
     * the reader channels.
     * TODO: In case the user provides the validation context we
     *   could make this redirection optional.
     */
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
pub unsafe extern "C" fn xml_text_reader_schema_validate(
    reader: XmlTextReaderPtr,
    xsd: *const c_char,
) -> i32 {
    xml_text_reader_schema_validate_internal(reader, xsd, null_mut(), 0)
}

/// Use W3C XSD schema context to validate the document as it is processed.
/// Activation is only possible before the first Read().
/// If @ctxt is NULL, then XML Schema validation is deactivated.
///
/// Returns 0 in case the schemas validation could be (de)activated and -1 in case of error.
#[doc(alias = "xmlTextReaderSchemaValidateCtxt")]
#[cfg(all(feature = "libxml_reader", feature = "schema"))]
pub unsafe extern "C" fn xml_text_reader_schema_validate_ctxt(
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
pub unsafe extern "C" fn xml_text_reader_set_schema(
    reader: XmlTextReaderPtr,
    schema: XmlSchemaPtr,
) -> i32 {
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
    if (*reader).mode != XmlTextReaderMode::XmlTextreaderModeInitial as i32 {
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
        addr_of_mut!((*(*reader).ctxt).sax),
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
pub unsafe extern "C" fn xml_text_reader_const_xml_version(
    reader: XmlTextReaderPtr,
) -> *const XmlChar {
    use std::ffi::CString;

    let mut doc: XmlDocPtr = null_mut();
    if reader.is_null() {
        return null_mut();
    }
    if !(*reader).doc.is_null() {
        doc = (*reader).doc;
    } else if !(*reader).ctxt.is_null() {
        doc = (*(*reader).ctxt).my_doc;
    }
    if doc.is_null() {
        return null_mut();
    }

    if let Some(version) = (*doc).version.as_deref() {
        let version = CString::new(version).unwrap();
        CONSTSTR!(reader, version.as_ptr() as *const u8)
    } else {
        null_mut()
    }
}

/// Determine the standalone status of the document being read.
///
/// Returns 1 if the document was declared to be standalone, 0 if it
/// was declared to be not standalone, or -1 if the document did not
/// specify its standalone status or in case of error.
#[doc(alias = "xmlTextReaderStandalone")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_standalone(reader: XmlTextReaderPtr) -> i32 {
    let mut doc: XmlDocPtr = null_mut();
    if reader.is_null() {
        return -1;
    }
    if !(*reader).doc.is_null() {
        doc = (*reader).doc;
    } else if !(*reader).ctxt.is_null() {
        doc = (*(*reader).ctxt).my_doc;
    }
    if doc.is_null() {
        return -1;
    }

    (*doc).standalone
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
pub unsafe extern "C" fn xml_text_reader_byte_consumed(reader: XmlTextReaderPtr) -> i64 {
    if reader.is_null() || (*reader).ctxt.is_null() {
        return -1;
    }
    xml_byte_consumed((*reader).ctxt)
}

/// Create an xmltextReader for a preparsed document.
///
/// Returns the new reader or NULL in case of error.
#[doc(alias = "xmlReaderWalker")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_reader_walker(doc: XmlDocPtr) -> XmlTextReaderPtr {
    use crate::generic_error;

    if doc.is_null() {
        return null_mut();
    }

    let ret: XmlTextReaderPtr = xml_malloc(size_of::<XmlTextReader>()) as _;
    if ret.is_null() {
        generic_error!("xmlNewTextReader : malloc failed\n");
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlTextReader>());
    (*ret).ent_nr = 0;
    // (*ret).input = None;
    std::ptr::write(&raw mut (*ret).input, None);
    (*ret).mode = XmlTextReaderMode::XmlTextreaderModeInitial as i32;
    (*ret).node = null_mut();
    (*ret).curnode = null_mut();
    (*ret).base = 0;
    (*ret).cur = 0;
    (*ret).allocs = XML_TEXTREADER_CTXT;
    (*ret).doc = doc;
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
pub unsafe extern "C" fn xml_reader_for_doc(
    cur: *const XmlChar,
    url: *const c_char,
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
pub unsafe extern "C" fn xml_reader_for_file(
    filename: *const c_char,
    encoding: *const c_char,
    options: i32,
) -> XmlTextReaderPtr {
    let reader: XmlTextReaderPtr = xml_new_text_reader_filename(filename);
    if reader.is_null() {
        return null_mut();
    }
    xml_text_reader_setup(reader, None, null_mut(), encoding, options);
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
    url: *const c_char,
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
pub unsafe extern "C" fn xml_reader_for_io(
    ioctx: impl Read + 'static,
    url: *const c_char,
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
pub unsafe extern "C" fn xml_reader_new_walker(reader: XmlTextReaderPtr, doc: XmlDocPtr) -> i32 {
    if doc.is_null() {
        return -1;
    }
    if reader.is_null() {
        return -1;
    }

    if (*reader).input.is_some() {
        // xml_free_parser_input_buffer((*reader).input);
        let _ = (*reader).input.take();
    }
    if !(*reader).ctxt.is_null() {
        xml_ctxt_reset((*reader).ctxt);
    }

    (*reader).ent_nr = 0;
    (*reader).input = None;
    (*reader).mode = XmlTextReaderMode::XmlTextreaderModeInitial as i32;
    (*reader).node = null_mut();
    (*reader).curnode = null_mut();
    (*reader).base = 0;
    (*reader).cur = 0;
    (*reader).allocs = XML_TEXTREADER_CTXT;
    (*reader).doc = doc;
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
pub unsafe extern "C" fn xml_reader_new_doc(
    reader: XmlTextReaderPtr,
    cur: *const XmlChar,
    url: *const c_char,
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
pub unsafe extern "C" fn xml_reader_new_file(
    reader: XmlTextReaderPtr,
    filename: *const c_char,
    encoding: *const c_char,
    options: i32,
) -> i32 {
    use std::ffi::CStr;

    use crate::encoding::XmlCharEncoding;

    if filename.is_null() {
        return -1;
    }
    if reader.is_null() {
        return -1;
    }

    let Some(input) = XmlParserInputBuffer::from_uri(
        CStr::from_ptr(filename).to_string_lossy().as_ref(),
        XmlCharEncoding::None,
    ) else {
        return -1;
    };
    xml_text_reader_setup(reader, Some(input), filename, encoding, options)
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
    url: *const c_char,
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
pub unsafe extern "C" fn xml_reader_new_io(
    reader: XmlTextReaderPtr,
    ioctx: impl Read + 'static,
    url: *const c_char,
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
pub unsafe extern "C" fn xml_text_reader_locator_line_number(
    locator: XmlTextReaderLocatorPtr,
) -> i32 {
    /* we know that locator is a xmlParserCtxtPtr */
    let ctx: XmlParserCtxtPtr = locator as XmlParserCtxtPtr;
    let ret: i32;

    if locator.is_null() {
        return -1;
    }
    if !(*ctx).node.is_null() {
        ret = (*(*ctx).node).get_line_no() as _;
    } else {
        /* inspired from error.c */
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
pub unsafe extern "C" fn xml_text_reader_locator_base_uri(
    locator: XmlTextReaderLocatorPtr,
) -> *mut XmlChar {
    /* we know that locator is a xmlParserCtxtPtr */

    use std::ffi::CString;
    let ctx: XmlParserCtxtPtr = locator as XmlParserCtxtPtr;
    let ret: *mut XmlChar;

    if locator.is_null() {
        return null_mut();
    }
    if !(*ctx).node.is_null() {
        ret = (*(*ctx).node).get_base(null_mut());
    } else {
        /* inspired from error.c */
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
    // original code is the following, but Rust cannot handle variable length arguments.

    // va_list ap;

    // va_start(ap, msg);
    // xmlTextReaderGenericError(ctxt,
    //                           XML_PARSER_SEVERITY_ERROR,
    //                           xmlTextReaderBuildMessage(msg, ap));
    // va_end(ap);
}

#[cfg(feature = "libxml_reader")]
fn xml_text_reader_warning(ctxt: Option<GenericErrorContext>, msg: &str) {
    xml_text_reader_generic_error(ctxt, XmlParserSeverities::XmlParserSeverityWarning, msg);

    // original code is the following, but Rust cannot handle variable length arguments.

    // va_list ap;

    // va_start(ap, msg);
    // xmlTextReaderGenericError(ctxt,
    //                           XML_PARSER_SEVERITY_WARNING,
    //                           xmlTextReaderBuildMessage(msg, ap));
    // va_end(ap);
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
        (*(*(*reader).ctxt).sax).error = Some(xml_text_reader_error);
        (*(*(*reader).ctxt).sax).serror = None;
        (*(*reader).ctxt).vctxt.error = Some(xml_text_reader_validity_error);
        (*(*(*reader).ctxt).sax).warning = Some(xml_text_reader_warning);
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
        /* restore defaults */
        (*(*(*reader).ctxt).sax).error = Some(parser_error);
        (*(*reader).ctxt).vctxt.error = Some(parser_validity_error);
        (*(*(*reader).ctxt).sax).warning = Some(parser_warning);
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
        (*(*(*reader).ctxt).sax).error = None;
        (*(*(*reader).ctxt).sax).serror = Some(xml_text_reader_structured_error);
        (*(*reader).ctxt).vctxt.error = Some(xml_text_reader_validity_error);
        (*(*(*reader).ctxt).sax).warning = Some(xml_text_reader_warning);
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
        /* restore defaults */
        (*(*(*reader).ctxt).sax).error = Some(parser_error);
        (*(*(*reader).ctxt).sax).serror = None;
        (*(*reader).ctxt).vctxt.error = Some(parser_validity_error);
        (*(*(*reader).ctxt).sax).warning = Some(parser_warning);
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

/// Retrieve the error callback function and user argument.
#[doc(alias = "xmlTextReaderGetErrorHandler")]
#[cfg(feature = "libxml_reader")]
pub unsafe extern "C" fn xml_text_reader_get_error_handler(
    reader: XmlTextReaderPtr,
    f: *mut Option<XmlTextReaderErrorFunc>,
    arg: *mut Option<GenericErrorContext>,
) {
    if !f.is_null() {
        *f = (*reader).error_func;
    }
    if !arg.is_null() {
        *arg = (*reader).error_func_arg.clone();
    }
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_new_text_reader_filename() {
        #[cfg(feature = "libxml_reader")]
        unsafe {
            let mut leaks = 0;

            for n_uri in 0..GEN_NB_FILEPATH {
                let mem_base = xml_mem_blocks();
                let uri = gen_filepath(n_uri, 0);

                let ret_val = xml_new_text_reader_filename(uri);
                desret_xml_text_reader_ptr(ret_val);
                des_filepath(n_uri, uri, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlNewTextReaderFilename",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlNewTextReaderFilename()"
                    );
                    eprintln!(" {}", n_uri);
                }
            }
        }
    }

    #[test]
    fn test_xml_reader_for_doc() {
        #[cfg(feature = "libxml_reader")]
        unsafe {
            let mut leaks = 0;

            for n_cur in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_url in 0..GEN_NB_FILEPATH {
                    for n_encoding in 0..GEN_NB_CONST_CHAR_PTR {
                        for n_options in 0..GEN_NB_PARSEROPTIONS {
                            let mem_base = xml_mem_blocks();
                            let cur = gen_const_xml_char_ptr(n_cur, 0);
                            let url = gen_filepath(n_url, 1);
                            let encoding = gen_const_char_ptr(n_encoding, 2);
                            let options = gen_parseroptions(n_options, 3);

                            let ret_val =
                                xml_reader_for_doc(cur as *const XmlChar, url, encoding, options);
                            desret_xml_text_reader_ptr(ret_val);
                            des_const_xml_char_ptr(n_cur, cur, 0);
                            des_filepath(n_url, url, 1);
                            des_const_char_ptr(n_encoding, encoding, 2);
                            des_parseroptions(n_options, options, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlReaderForDoc",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlReaderForDoc()");
                                eprint!(" {}", n_cur);
                                eprint!(" {}", n_url);
                                eprint!(" {}", n_encoding);
                                eprintln!(" {}", n_options);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_reader_for_file() {
        #[cfg(feature = "libxml_reader")]
        unsafe {
            let mut leaks = 0;

            for n_filename in 0..GEN_NB_FILEPATH {
                for n_encoding in 0..GEN_NB_CONST_CHAR_PTR {
                    for n_options in 0..GEN_NB_PARSEROPTIONS {
                        let mem_base = xml_mem_blocks();
                        let filename = gen_filepath(n_filename, 0);
                        let encoding = gen_const_char_ptr(n_encoding, 1);
                        let options = gen_parseroptions(n_options, 2);

                        let ret_val = xml_reader_for_file(filename, encoding, options);
                        desret_xml_text_reader_ptr(ret_val);
                        des_filepath(n_filename, filename, 0);
                        des_const_char_ptr(n_encoding, encoding, 1);
                        des_parseroptions(n_options, options, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlReaderForFile",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlReaderForFile()");
                            eprint!(" {}", n_filename);
                            eprint!(" {}", n_encoding);
                            eprintln!(" {}", n_options);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_reader_new_doc() {
        #[cfg(feature = "libxml_reader")]
        unsafe {
            let mut leaks = 0;

            for n_reader in 0..GEN_NB_XML_TEXT_READER_PTR {
                for n_cur in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_url in 0..GEN_NB_FILEPATH {
                        for n_encoding in 0..GEN_NB_CONST_CHAR_PTR {
                            for n_options in 0..GEN_NB_PARSEROPTIONS {
                                let mem_base = xml_mem_blocks();
                                let reader = gen_xml_text_reader_ptr(n_reader, 0);
                                let cur = gen_const_xml_char_ptr(n_cur, 1);
                                let url = gen_filepath(n_url, 2);
                                let encoding = gen_const_char_ptr(n_encoding, 3);
                                let options = gen_parseroptions(n_options, 4);

                                let ret_val =
                                    xml_reader_new_doc(reader, cur, url, encoding, options);
                                desret_int(ret_val);
                                des_xml_text_reader_ptr(n_reader, reader, 0);
                                des_const_xml_char_ptr(n_cur, cur, 1);
                                des_filepath(n_url, url, 2);
                                des_const_char_ptr(n_encoding, encoding, 3);
                                des_parseroptions(n_options, options, 4);
                                reset_last_error();
                                if mem_base != xml_mem_blocks() {
                                    leaks += 1;
                                    eprint!(
                                        "Leak of {} blocks found in xmlReaderNewDoc",
                                        xml_mem_blocks() - mem_base
                                    );
                                    assert!(
                                        leaks == 0,
                                        "{leaks} Leaks are found in xmlReaderNewDoc()"
                                    );
                                    eprint!(" {}", n_reader);
                                    eprint!(" {}", n_cur);
                                    eprint!(" {}", n_url);
                                    eprint!(" {}", n_encoding);
                                    eprintln!(" {}", n_options);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_reader_new_file() {
        #[cfg(feature = "libxml_reader")]
        unsafe {
            let mut leaks = 0;

            for n_reader in 0..GEN_NB_XML_TEXT_READER_PTR {
                for n_filename in 0..GEN_NB_FILEPATH {
                    for n_encoding in 0..GEN_NB_CONST_CHAR_PTR {
                        for n_options in 0..GEN_NB_PARSEROPTIONS {
                            let mem_base = xml_mem_blocks();
                            let reader = gen_xml_text_reader_ptr(n_reader, 0);
                            let filename = gen_filepath(n_filename, 1);
                            let encoding = gen_const_char_ptr(n_encoding, 2);
                            let options = gen_parseroptions(n_options, 3);

                            let ret_val = xml_reader_new_file(reader, filename, encoding, options);
                            desret_int(ret_val);
                            des_xml_text_reader_ptr(n_reader, reader, 0);
                            des_filepath(n_filename, filename, 1);
                            des_const_char_ptr(n_encoding, encoding, 2);
                            des_parseroptions(n_options, options, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlReaderNewFile",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlReaderNewFile()"
                                );
                                eprint!(" {}", n_reader);
                                eprint!(" {}", n_filename);
                                eprint!(" {}", n_encoding);
                                eprintln!(" {}", n_options);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_reader_new_walker() {
        #[cfg(feature = "libxml_reader")]
        unsafe {
            let mut leaks = 0;

            for n_reader in 0..GEN_NB_XML_TEXT_READER_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    let mem_base = xml_mem_blocks();
                    let reader = gen_xml_text_reader_ptr(n_reader, 0);
                    let doc = gen_xml_doc_ptr(n_doc, 1);

                    let ret_val = xml_reader_new_walker(reader, doc);
                    desret_int(ret_val);
                    des_xml_text_reader_ptr(n_reader, reader, 0);
                    des_xml_doc_ptr(n_doc, doc, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlReaderNewWalker",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlReaderNewWalker()"
                        );
                        eprint!(" {}", n_reader);
                        eprintln!(" {}", n_doc);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_reader_walker() {
        #[cfg(feature = "libxml_reader")]
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                let mem_base = xml_mem_blocks();
                let doc = gen_xml_doc_ptr(n_doc, 0);

                let ret_val = xml_reader_walker(doc);
                desret_xml_text_reader_ptr(ret_val);
                des_xml_doc_ptr(n_doc, doc, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlReaderWalker",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlReaderWalker()");
                    eprintln!(" {}", n_doc);
                }
            }
        }
    }

    #[test]
    fn test_xml_text_reader_byte_consumed() {
        #[cfg(feature = "libxml_reader")]
        unsafe {
            let mut leaks = 0;

            for n_reader in 0..GEN_NB_XML_TEXT_READER_PTR {
                let mem_base = xml_mem_blocks();
                let reader = gen_xml_text_reader_ptr(n_reader, 0);

                let ret_val = xml_text_reader_byte_consumed(reader);
                desret_long(ret_val);
                des_xml_text_reader_ptr(n_reader, reader, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlTextReaderByteConsumed",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlTextReaderByteConsumed()"
                    );
                    eprintln!(" {}", n_reader);
                }
            }
        }
    }

    #[test]
    fn test_xml_text_reader_const_xml_version() {
        #[cfg(feature = "libxml_reader")]
        unsafe {
            let mut leaks = 0;

            for n_reader in 0..GEN_NB_XML_TEXT_READER_PTR {
                let mem_base = xml_mem_blocks();
                let reader = gen_xml_text_reader_ptr(n_reader, 0);

                let ret_val = xml_text_reader_const_xml_version(reader);
                desret_const_xml_char_ptr(ret_val);
                des_xml_text_reader_ptr(n_reader, reader, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlTextReaderConstXmlVersion",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlTextReaderConstXmlVersion()"
                    );
                    eprintln!(" {}", n_reader);
                }
            }
        }
    }

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

    #[test]
    fn test_xml_text_reader_set_structured_error_handler() {

        /* missing type support */
    }

    #[test]
    fn test_xml_text_reader_standalone() {
        #[cfg(feature = "libxml_reader")]
        unsafe {
            let mut leaks = 0;

            for n_reader in 0..GEN_NB_XML_TEXT_READER_PTR {
                let mem_base = xml_mem_blocks();
                let reader = gen_xml_text_reader_ptr(n_reader, 0);

                let ret_val = xml_text_reader_standalone(reader);
                desret_int(ret_val);
                des_xml_text_reader_ptr(n_reader, reader, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlTextReaderStandalone",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlTextReaderStandalone()"
                    );
                    eprintln!(" {}", n_reader);
                }
            }
        }
    }
}
