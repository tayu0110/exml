use std::{
    ffi::{c_char, c_int, c_long, c_uchar, c_uint, c_ulong, CStr},
    os::raw::c_void,
    ptr::{addr_of_mut, null, null_mut},
    sync::atomic::{AtomicPtr, Ordering},
};

use libc::{fclose, fopen, snprintf, FILE};

use crate::libxml::{
    catalog::{XmlCatalogAllow, XmlCatalogPrefer, XmlCatalogPtr},
    chvalid::XmlChRangeGroup,
    debug_xml::XmlShellCtxtPtr,
    dict::{xml_dict_create, xml_dict_free, XmlDictPtr},
    encoding::{XmlCharEncoding, XmlCharEncodingHandlerPtr},
    entities::{XmlEntitiesTablePtr, XmlEntityPtr},
    globals::xml_free,
    hash::{xml_hash_create, xml_hash_free, XmlHashDeallocator, XmlHashTablePtr},
    htmlparser::{
        html_free_parser_ctxt, HtmlDocPtr, HtmlElemDesc, HtmlEntityDesc, HtmlNodePtr,
        HtmlParserCtxtPtr, HtmlSaxhandlerPtr, HtmlStatus,
    },
    list::{xml_list_create, xml_list_delete, XmlLinkPtr, XmlList, XmlListPtr},
    parser::{
        xml_free_parser_ctxt, xml_new_parser_ctxt, xml_read_memory, XmlFeature, XmlParserCtxt,
        XmlParserCtxtPtr, XmlParserInputPtr, XmlParserNodeInfo, XmlParserNodeInfoSeq,
        XmlParserNodeInfoSeqPtr, XmlParserOption, XmlSAXHandler, XmlSAXHandlerPtr,
        XmlSaxlocatorPtr,
    },
    parser_internals::{xml_create_memory_parser_ctxt, xml_free_input_stream},
    pattern::{XmlPatternPtr, XmlStreamCtxtPtr},
    relaxng::{
        XmlRelaxNGParserCtxtPtr, XmlRelaxNGPtr, XmlRelaxNGValidCtxtPtr,
        XmlRelaxNGValidityErrorFunc, XmlRelaxNGValidityWarningFunc,
    },
    schemas_internals::{XmlSchemaFacetPtr, XmlSchemaTypePtr, XmlSchemaValType},
    schematron::XmlSchematronValidCtxtPtr,
    tree::{
        xml_buffer_create, xml_buffer_create_static, xml_buffer_free, xml_free_doc, xml_free_node,
        xml_new_doc, xml_new_dtd, xml_new_pi, xml_new_text, xml_set_prop, xml_unlink_node, XmlAttr,
        XmlAttrPtr, XmlAttributeDefault, XmlAttributePtr, XmlAttributeType, XmlBuf, XmlBuffer,
        XmlBufferAllocationScheme, XmlBufferPtr, XmlDOMWrapCtxtPtr, XmlDoc, XmlDocPtr, XmlDtd,
        XmlDtdPtr, XmlElementContentPtr, XmlElementContentType, XmlElementPtr, XmlElementType,
        XmlElementTypeVal, XmlEnumerationPtr, XmlNode, XmlNodePtr, XmlNotationPtr, XmlNs, XmlNsPtr,
    },
    uri::XmlURIPtr,
    valid::{
        xml_free_element_content, xml_free_valid_ctxt, xml_new_valid_ctxt, XmlAttributeTablePtr,
        XmlElementTablePtr, XmlNotationTablePtr, XmlValidCtxtPtr,
    },
    xinclude::XmlXincludeCtxtPtr,
    xml_io::{
        xmlFreeParserInputBuffer, xmlParserInputBufferCreateFilename, XmlOutputBufferPtr,
        XmlParserInputBufferPtr,
    },
    xmlautomata::{XmlAutomataPtr, XmlAutomataStatePtr},
    xmlerror::{XmlErrorPtr, XmlGenericErrorFunc, XmlParserErrors},
    xmlmodule::XmlModulePtr,
    xmlreader::{XmlTextReaderErrorFunc, XmlTextReaderLocatorPtr, XmlTextReaderPtr},
    xmlregexp::{XmlExpCtxtPtr, XmlExpNodePtr, XmlRegExecCtxtPtr, XmlRegexpPtr},
    xmlsave::XmlSaveCtxtPtr,
    xmlschemas::{
        XmlSchemaParserCtxtPtr, XmlSchemaPtr, XmlSchemaSAXPlugPtr, XmlSchemaValidCtxtPtr,
        XmlSchemaValidityErrorFunc, XmlSchemaValidityWarningFunc,
    },
    xmlschemastypes::{XmlSchemaValPtr, XmlSchemaWhitespaceValueType},
    xmlstring::{xml_strdup, XmlChar},
    xmlwriter::{xmlFreeTextWriter, XmlTextWriterPtr},
    xpath::{
        XmlNodeSetPtr, XmlXPathCompExprPtr, XmlXPathContextPtr, XmlXPathObjectPtr,
        XmlXPathParserContextPtr,
    },
};

// static mut GENERIC_ERRORS: c_int = 0;
// static mut FUNCTION_TESTS: c_int = 0;
static mut CHARTAB: [XmlChar; 1024] = [0; 1024];
static mut LONGTAB: [c_ulong; 1024] = [0; 1024];
static mut INTTAB: [c_int; 1024] = [0; 1024];

static API_DOC: AtomicPtr<XmlDoc> = AtomicPtr::new(null_mut());
static API_DTD: AtomicPtr<XmlDtd> = AtomicPtr::new(null_mut());
static API_ROOT: AtomicPtr<XmlNode> = AtomicPtr::new(null_mut());
static API_ATTR: AtomicPtr<XmlAttr> = AtomicPtr::new(null_mut());
static API_NS: AtomicPtr<XmlNs> = AtomicPtr::new(null_mut());

pub(crate) const GEN_NB_HTML_DOC_PTR: i32 = 3;
pub(crate) const GEN_NB_UNSIGNED_CHAR_PTR: i32 = 1;
pub(crate) const GEN_NB_INT_PTR: i32 = 2;
pub(crate) const GEN_NB_CONST_UNSIGNED_CHAR_PTR: i32 = 1;
pub(crate) const GEN_NB_HTML_PARSER_CTXT_PTR: i32 = 3;
pub(crate) const GEN_NB_XML_CHAR_ENCODING: i32 = 4;
pub(crate) const GEN_NB_HTML_SAXHANDLER_PTR: i32 = 1;
pub(crate) const GEN_NB_FILEPATH: i32 = 8;
pub(crate) const GEN_NB_FILEOUTPUT: i32 = 6;
pub(crate) const GEN_NB_CONST_CHAR_PTR: i32 = 4;
pub(crate) const GEN_NB_CONST_HTML_ELEM_DESC_PTR: i32 = 1;
pub(crate) const GEN_NB_CONST_XML_CHAR_PTR: i32 = 5;
pub(crate) const GEN_NB_INT: i32 = 4;
pub(crate) const GEN_NB_CONST_HTML_SAXHANDLER_PTR: i32 = 1;
pub(crate) const GEN_NB_USERDATA: i32 = 3;
pub(crate) const GEN_NB_UNSIGNED_INT: i32 = 3;
#[cfg(feature = "html")]
pub(crate) const GEN_NB_HTML_NODE_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_OUTPUT_BUFFER_PTR: i32 = 2;
#[cfg(feature = "html")]
pub(crate) const GEN_NB_CONST_HTML_NODE_PTR: i32 = 1;
pub(crate) const GEN_NB_CONST_XML_CHAR_PTR_PTR: i32 = 1;
pub(crate) const GEN_NB_FILE_PTR: i32 = 2;
pub(crate) const GEN_NB_XML_CHAR_PTR_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_BUFFER_PTR: i32 = 3;
pub(crate) const STATIC_BUF_CONTENT: &CStr = c"a static buffer";
pub(crate) const GEN_NB_XML_NODE_PTR: i32 = 3;
pub(crate) const GEN_NB_VOID_PTR: i32 = 2;
pub(crate) const GEN_NB_XML_ENUMERATION_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_CHAR_PTR: i32 = 2;
pub(crate) const GEN_NB_XML_SAXLOCATOR_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_DOC_PTR: i32 = 4;
#[cfg(feature = "xpath")]
pub(crate) const GEN_NB_XML_NODE_SET_PTR: i32 = 1;
#[cfg(feature = "catalog")]
pub(crate) const GEN_NB_XML_CATALOG_PTR: i32 = 1;
#[cfg(feature = "catalog")]
pub(crate) const GEN_NB_XML_CATALOG_ALLOW: i32 = 4;
#[cfg(feature = "catalog")]
pub(crate) const GEN_NB_XML_CATALOG_PREFER: i32 = 3;
pub(crate) const GEN_NB_CONST_XML_CH_RANGE_GROUP_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_DICT_PTR: i32 = 2;
pub(crate) const GEN_NB_XML_CHAR_ENCODING_HANDLER_PTR: i32 = 1;
pub(crate) const GEN_NB_CONST_XML_DOC_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_ENTITY_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_ENTITIES_TABLE_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_HASH_TABLE_PTR: i32 = 2;
pub(crate) const GEN_NB_XML_HASH_DEALLOCATOR: i32 = 2;
pub(crate) const GEN_NB_CONST_XML_LIST_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_LINK_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_LIST_PTR: i32 = 2;
pub(crate) const GEN_NB_CHAR_PTR_PTR: i32 = 1;
#[cfg(feature = "http")]
pub(crate) const GEN_NB_XML_NANO_HTTPCTXT_PTR: i32 = 1;
pub(crate) const GEN_NB_CONST_XML_SAXHANDLER_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_NODE_PTR_PTR: i32 = 1;
pub(crate) const GEN_NB_CONST_XML_PARSER_NODE_INFO_PTR: i32 = 1;
pub(crate) const GEN_NB_CONST_XML_PARSER_CTXT_PTR: i32 = 1;
pub(crate) const GEN_NB_CONST_XML_NODE_PTR: i32 = 1;
pub(crate) const GEN_NB_CONST_XML_PARSER_NODE_INFO_SEQ_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_PARSER_INPUT_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_PARSER_CTXT_PTR: i32 = 3;
pub(crate) const GEN_NB_XML_PARSER_NODE_INFO_SEQ_PTR: i32 = 1;
#[cfg(any(feature = "sax1", feature = "valid", feature = "push"))]
pub(crate) const GEN_NB_XML_SAXHANDLER_PTR: i32 = 2;
pub(crate) const GEN_NB_PARSEROPTIONS: i32 = 5;
pub(crate) const GEN_NB_XML_FEATURE: i32 = 4;
pub(crate) const GEN_NB_XML_PARSER_INPUT_BUFFER_PTR: i32 = 8;
pub(crate) const GEN_NB_XML_CHAR: i32 = 4;
#[cfg(feature = "libxml_pattern")]
pub(crate) const GEN_NB_XML_STREAM_CTXT_PTR: i32 = 1;
#[cfg(feature = "libxml_pattern")]
pub(crate) const GEN_NB_XML_PATTERN_PTR: i32 = 1;
#[cfg(feature = "schema")]
pub(crate) const GEN_NB_XML_RELAXNG_PTR: i32 = 1;
#[cfg(feature = "schema")]
pub(crate) const GEN_NB_XML_RELAXNG_PARSER_CTXT_PTR: i32 = 1;
#[cfg(feature = "schema")]
pub(crate) const GEN_NB_XML_RELAXNG_VALIDITY_ERROR_FUNC_PTR: i32 = 1;
#[cfg(feature = "schema")]
pub(crate) const GEN_NB_XML_RELAXNG_VALIDITY_WARNING_FUNC_PTR: i32 = 1;
#[cfg(feature = "schema")]
pub(crate) const GEN_NB_XML_RELAXNG_VALID_CTXT_PTR: i32 = 1;
#[cfg(any(
    feature = "libxml_modules",
    feature = "libxml_reader",
    feature = "schema"
))]
pub(crate) const GEN_NB_VOID_PTR_PTR: i32 = 1;
#[cfg(feature = "libxml_schematron")]
pub(crate) const GEN_NB_XML_SCHEMATRON_VALID_CTXT_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_DOMWRAP_CTXT_PTR: i32 = 1;
pub(crate) const GEN_NB_CONST_XML_BUFFER_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_BUF_PTR: i32 = 1;
pub(crate) const GEN_NB_CONST_XML_BUF_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_NODE_PTR_IN: i32 = 3;
pub(crate) const GEN_NB_XML_ATTR_PTR: i32 = 2;
pub(crate) const GEN_NB_XML_DTD_PTR: i32 = 3;
pub(crate) const GEN_NB_XML_NS_PTR: i32 = 2;
pub(crate) const GEN_NB_EATEN_NAME: i32 = 2;
pub(crate) const GEN_NB_XML_URIPTR: i32 = 1;
pub(crate) const GEN_NB_CHAR_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_ELEMENT_CONTENT_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_NOTATION_TABLE_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_NOTATION_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_ELEMENT_TABLE_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_ELEMENT_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_ATTRIBUTE_TABLE_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_ATTRIBUTE_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_VALID_CTXT_PTR: i32 = 2;
pub(crate) const GEN_NB_XML_ATTRIBUTE_TYPE: i32 = 4;
pub(crate) const GEN_NB_XML_ATTRIBUTE_DEFAULT: i32 = 4;
pub(crate) const GEN_NB_XML_ELEMENT_TYPE_VAL: i32 = 4;
pub(crate) const GEN_NB_XML_ELEMENT_CONTENT_TYPE: i32 = 4;
#[cfg(feature = "xinclude")]
pub(crate) const GEN_NB_XML_XINCLUDE_CTXT_PTR: i32 = 1;
#[cfg(feature = "libxml_automata")]
pub(crate) const GEN_NB_XML_AUTOMATA_STATE_PTR: i32 = 1;
#[cfg(feature = "libxml_automata")]
pub(crate) const GEN_NB_XML_AUTOMATA_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_ERROR_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_GENERIC_ERROR_FUNC_PTR: i32 = 1;
#[cfg(feature = "libxml_modules")]
pub(crate) const GEN_NB_XML_MODULE_PTR: i32 = 1;
#[cfg(feature = "libxml_reader")]
pub(crate) const GEN_NB_XML_TEXT_READER_LOCATOR_PTR: i32 = 1;
#[cfg(feature = "libxml_reader")]
pub(crate) const GEN_NB_XML_TEXT_READER_ERROR_FUNC_PTR: i32 = 1;
#[cfg(feature = "libxml_reader")]
pub(crate) const GEN_NB_XML_TEXT_READER_PTR: i32 = 4;
#[cfg(feature = "schema")]
pub(crate) const GEN_NB_XML_SCHEMA_PTR: i32 = 1;
#[cfg(feature = "schema")]
pub(crate) const GEN_NB_XML_SCHEMA_VALID_CTXT_PTR: i32 = 1;
#[cfg(feature = "regexp")]
pub(crate) const GEN_NB_XML_REGEXP_PTR: i32 = 1;
#[cfg(feature = "regexp")]
pub(crate) const GEN_NB_XML_REG_EXEC_CTXT_PTR: i32 = 1;
#[cfg(all(feature = "regexp", feature = "libxml_expr"))]
pub(crate) const GEN_NB_XML_EXP_CTXT_PTR: i32 = 1;
#[cfg(all(feature = "regexp", feature = "libxml_expr"))]
pub(crate) const GEN_NB_XML_EXP_NODE_PTR: i32 = 1;
#[cfg(feature = "output")]
pub(crate) const GEN_NB_XML_SAVE_CTXT_PTR: i32 = 1;
#[cfg(feature = "schema")]
pub(crate) const GEN_NB_XML_SCHEMA_SAXPLUG_PTR: i32 = 1;
#[cfg(feature = "schema")]
pub(crate) const GEN_NB_XML_SCHEMA_PARSER_CTXT_PTR: i32 = 1;
#[cfg(feature = "schema")]
pub(crate) const GEN_NB_XML_SCHEMA_VALIDITY_ERROR_FUNC_PTR: i32 = 1;
#[cfg(feature = "schema")]
pub(crate) const GEN_NB_XML_SCHEMA_VALIDITY_WARNING_FUNC_PTR: i32 = 1;
#[cfg(feature = "schema")]
pub(crate) const GEN_NB_XML_SCHEMA_VAL_PTR_PTR: i32 = 1;
#[cfg(feature = "schema")]
pub(crate) const GEN_NB_XML_SCHEMA_VAL_PTR: i32 = 1;
#[cfg(feature = "schema")]
pub(crate) const GEN_NB_XML_SCHEMA_FACET_PTR: i32 = 1;
#[cfg(feature = "schema")]
pub(crate) const GEN_NB_XML_SCHEMA_TYPE_PTR: i32 = 1;
#[cfg(feature = "schema")]
pub(crate) const GEN_NB_XML_SCHEMA_WHITESPACE_VALUE_TYPE: i32 = 4;
#[cfg(feature = "schema")]
pub(crate) const GEN_NB_XML_SCHEMA_VAL_TYPE: i32 = 4;
#[cfg(feature = "schema")]
pub(crate) const GEN_NB_UNSIGNED_LONG: i32 = 4;
#[cfg(feature = "schema")]
pub(crate) const GEN_NB_UNSIGNED_LONG_PTR: i32 = 2;
#[cfg(feature = "writer")]
pub(crate) const GEN_NB_XML_TEXT_WRITER_PTR: i32 = 2;
#[cfg(feature = "xpath")]
pub(crate) const GEN_NB_XML_XPATH_COMP_EXPR_PTR: i32 = 1;
#[cfg(feature = "xpath")]
pub(crate) const GEN_NB_XML_XPATH_CONTEXT_PTR: i32 = 1;
#[cfg(feature = "xpath")]
pub(crate) const GEN_NB_XML_XPATH_OBJECT_PTR: i32 = 5;
#[cfg(any(feature = "xpath", feature = "schema"))]
pub(crate) const GEN_NB_DOUBLE: i32 = 4;
#[cfg(feature = "xpath")]
pub(crate) const GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR: i32 = 1;
#[cfg(feature = "libxml_debug")]
pub(crate) const GEN_NB_DEBUG_FILE_PTR: i32 = 2;
#[cfg(feature = "xpath")]
pub(crate) const GEN_NB_XML_SHELL_CTXT_PTR: i32 = 1;

#[cfg(feature = "xpath")]
pub(crate) fn gen_xml_shell_ctxt_ptr(_no: c_int, _nr: c_int) -> XmlShellCtxtPtr {
    null_mut()
}

#[cfg(feature = "xpath")]
pub(crate) fn des_xml_shell_ctxt_ptr(_no: c_int, _val: XmlShellCtxtPtr, _nr: c_int) {}

#[cfg(feature = "libxml_debug")]
pub(crate) unsafe extern "C" fn gen_debug_file_ptr(_no: c_int, _nr: c_int) -> *mut FILE {
    fopen(c"test.out".as_ptr() as _, c"a+".as_ptr() as _)
}

#[cfg(feature = "libxml_debug")]
pub(crate) unsafe extern "C" fn des_debug_file_ptr(_no: c_int, val: *mut FILE, _nr: c_int) {
    if !val.is_null() {
        fclose(val);
    }
}

#[cfg(feature = "xpath")]
pub(crate) fn gen_xml_xpath_parser_context_ptr(_no: i32, _nr: i32) -> XmlXPathParserContextPtr {
    null_mut()
}

#[cfg(feature = "xpath")]
pub(crate) fn des_xml_xpath_parser_context_ptr(_no: i32, _val: XmlXPathParserContextPtr, _nr: i32) {
}

#[cfg(feature = "xpath")]
pub(crate) unsafe extern "C" fn desret_xml_xpath_object_ptr(val: XmlXPathObjectPtr) {
    use crate::libxml::xpath::xml_xpath_free_object;

    xml_xpath_free_object(val);
}

#[cfg(feature = "xpath")]
pub(crate) unsafe extern "C" fn desret_xml_node_set_ptr(val: XmlNodeSetPtr) {
    use crate::libxml::xpath::xml_xpath_free_node_set;

    xml_xpath_free_node_set(val);
}

#[cfg(any(feature = "xpath", feature = "schema"))]
pub(crate) unsafe extern "C" fn gen_double(no: c_int, _nr: c_int) -> f64 {
    if no == 0 {
        return 0.0;
    }
    if no == 1 {
        return -1.1;
    }
    #[cfg(feature = "xpath")]
    if no == 2 {
        use crate::libxml::xpath::XML_XPATH_NAN;
        return XML_XPATH_NAN;
    }
    -1.0
}

#[cfg(any(feature = "xpath", feature = "schema"))]
pub(crate) fn des_double(_no: c_int, _val: f64, _nr: c_int) {}

#[cfg(feature = "xpath")]
pub(crate) unsafe extern "C" fn gen_xml_xpath_object_ptr(
    no: c_int,
    _nr: c_int,
) -> XmlXPathObjectPtr {
    use crate::libxml::xpath_internals::{
        xml_xpath_new_boolean, xml_xpath_new_float, xml_xpath_new_node_set, xml_xpath_new_string,
    };

    if no == 0 {
        return xml_xpath_new_string(c"string object".as_ptr() as _);
    }
    if no == 1 {
        return xml_xpath_new_float(1.1);
    }
    if no == 2 {
        return xml_xpath_new_boolean(1);
    }
    if no == 3 {
        return xml_xpath_new_node_set(null_mut());
    }
    null_mut()
}

#[cfg(feature = "xpath")]
pub(crate) unsafe extern "C" fn des_xml_xpath_object_ptr(
    _no: c_int,
    val: XmlXPathObjectPtr,
    _nr: c_int,
) {
    use crate::libxml::xpath::xml_xpath_free_object;

    if !val.is_null() {
        xml_xpath_free_object(val);
    }
}

#[cfg(feature = "xpath")]
pub(crate) fn desret_double(_val: f64) {}

#[cfg(feature = "xpath")]
pub(crate) fn gen_xml_xpath_comp_expr_ptr(_no: i32, _nr: i32) -> XmlXPathCompExprPtr {
    null_mut()
}

#[cfg(feature = "xpath")]
pub(crate) fn des_xml_xpath_comp_expr_ptr(_no: i32, _val: XmlXPathCompExprPtr, _nr: i32) {}

#[cfg(feature = "xpath")]
pub(crate) fn gen_xml_xpath_context_ptr(_no: i32, _nr: i32) -> XmlXPathContextPtr {
    null_mut()
}

#[cfg(feature = "xpath")]
pub(crate) fn des_xml_xpath_context_ptr(_no: i32, _val: XmlXPathContextPtr, _nr: i32) {}

#[cfg(feature = "writer")]
pub(crate) unsafe extern "C" fn gen_xml_text_writer_ptr(no: c_int, _nr: c_int) -> XmlTextWriterPtr {
    use crate::libxml::xmlwriter::xmlNewTextWriterFilename;

    if no == 0 {
        return xmlNewTextWriterFilename(c"test.out".as_ptr() as _, 0);
    }
    null_mut()
}

#[cfg(feature = "writer")]
pub(crate) unsafe extern "C" fn des_xml_text_writer_ptr(
    _no: c_int,
    val: XmlTextWriterPtr,
    _nr: c_int,
) {
    if !val.is_null() {
        xmlFreeTextWriter(val);
    }
}

#[cfg(feature = "writer")]
pub(crate) unsafe extern "C" fn desret_xml_text_writer_ptr(val: XmlTextWriterPtr) {
    xmlFreeTextWriter(val);
}

#[cfg(feature = "schema")]
pub(crate) fn gen_unsigned_long(no: c_int, _nr: c_int) -> c_ulong {
    if no == 0 {
        return 0;
    }
    if no == 1 {
        return 1;
    }
    if no == 2 {
        return 122;
    }
    u64::MAX
}

#[cfg(feature = "schema")]
pub(crate) fn des_unsigned_long(_no: c_int, _val: c_ulong, _nr: c_int) {}

#[cfg(feature = "schema")]
pub(crate) unsafe extern "C" fn gen_unsigned_long_ptr(no: c_int, nr: c_int) -> *mut c_ulong {
    if no == 0 {
        return addr_of_mut!(LONGTAB[nr as usize]);
    }
    null_mut()
}

#[cfg(feature = "schema")]
pub(crate) fn des_unsigned_long_ptr(_no: c_int, _val: *mut c_ulong, _nr: c_int) {}

#[cfg(feature = "schema")]
pub(crate) fn gen_xml_schema_val_type(no: c_int, _nr: c_int) -> XmlSchemaValType {
    if no == 1 {
        return XmlSchemaValType::XmlSchemasAnysimpletype;
    }
    if no == 2 {
        return XmlSchemaValType::XmlSchemasAnytype;
    }
    if no == 3 {
        return XmlSchemaValType::XmlSchemasAnyuri;
    }
    if no == 4 {
        return XmlSchemaValType::XmlSchemasBase64binary;
    }
    XmlSchemaValType::XmlSchemasUnknown
}

#[cfg(feature = "schema")]
pub(crate) fn des_xml_schema_val_type(_no: c_int, _val: XmlSchemaValType, _nr: c_int) {}

#[cfg(feature = "schema")]
pub(crate) fn desret_xml_schema_val_type(_val: XmlSchemaValType) {}

#[cfg(feature = "schema")]
pub(crate) fn gen_xml_schema_whitespace_value_type(
    no: c_int,
    _nr: c_int,
) -> XmlSchemaWhitespaceValueType {
    if no == 1 {
        return XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse;
    }
    if no == 2 {
        return XmlSchemaWhitespaceValueType::XmlSchemaWhitespacePreserve;
    }
    if no == 3 {
        return XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceReplace;
    }
    if no == 4 {
        return XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceUnknown;
    }
    XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceUnknown
}

#[cfg(feature = "schema")]
pub(crate) fn des_xml_schema_whitespace_value_type(
    _no: c_int,
    _val: XmlSchemaWhitespaceValueType,
    _nr: c_int,
) {
}

#[cfg(feature = "schema")]
pub(crate) fn gen_xml_schema_facet_ptr(_no: i32, _nr: i32) -> XmlSchemaFacetPtr {
    null_mut()
}

#[cfg(feature = "schema")]
pub(crate) fn des_xml_schema_facet_ptr(_no: i32, _val: XmlSchemaFacetPtr, _nr: i32) {}

#[cfg(feature = "schema")]
pub(crate) fn gen_xml_schema_type_ptr(_no: i32, _nr: i32) -> XmlSchemaTypePtr {
    null_mut()
}

#[cfg(feature = "schema")]
pub(crate) fn des_xml_schema_type_ptr(_no: i32, _val: XmlSchemaTypePtr, _nr: i32) {}

#[cfg(feature = "schema")]
pub(crate) fn gen_xml_schema_val_ptr(_no: i32, _nr: i32) -> XmlSchemaValPtr {
    null_mut()
}

#[cfg(feature = "schema")]
pub(crate) fn des_xml_schema_val_ptr(_no: i32, _val: XmlSchemaValPtr, _nr: i32) {}

#[cfg(feature = "schema")]
pub(crate) fn gen_xml_schema_val_ptr_ptr(_no: i32, _nr: i32) -> *mut XmlSchemaValPtr {
    null_mut()
}

#[cfg(feature = "schema")]
pub(crate) fn des_xml_schema_val_ptr_ptr(_no: i32, _val: *mut XmlSchemaValPtr, _nr: i32) {}

#[cfg(feature = "schema")]
pub(crate) fn gen_xml_schema_parser_ctxt_ptr(_no: i32, _nr: i32) -> XmlSchemaParserCtxtPtr {
    null_mut()
}

#[cfg(feature = "schema")]
pub(crate) fn des_xml_schema_parser_ctxt_ptr(_no: i32, _val: XmlSchemaParserCtxtPtr, _nr: i32) {}

#[cfg(feature = "schema")]
pub(crate) fn gen_xml_schema_validity_error_func_ptr(
    _no: i32,
    _nr: i32,
) -> *mut Option<XmlSchemaValidityErrorFunc> {
    null_mut()
}

#[cfg(feature = "schema")]
pub(crate) fn des_xml_schema_validity_error_func_ptr(
    _no: i32,
    _val: *mut Option<XmlSchemaValidityErrorFunc>,
    _nr: i32,
) {
}

#[cfg(feature = "schema")]
pub(crate) fn gen_xml_schema_validity_warning_func_ptr(
    _no: i32,
    _nr: i32,
) -> *mut Option<XmlSchemaValidityWarningFunc> {
    null_mut()
}

#[cfg(feature = "schema")]
pub(crate) fn des_xml_schema_validity_warning_func_ptr(
    _no: i32,
    _val: *mut Option<XmlSchemaValidityWarningFunc>,
    _nr: i32,
) {
}

#[cfg(feature = "schema")]
pub(crate) fn gen_xml_schema_saxplug_ptr(_no: i32, _nr: i32) -> XmlSchemaSAXPlugPtr {
    null_mut()
}

#[cfg(feature = "schema")]
pub(crate) fn des_xml_schema_saxplug_ptr(_no: i32, _val: XmlSchemaSAXPlugPtr, _nr: i32) {}

#[cfg(feature = "output")]
pub(crate) fn gen_xml_save_ctxt_ptr(_no: i32, _nr: i32) -> XmlSaveCtxtPtr {
    null_mut()
}

#[cfg(feature = "output")]
pub(crate) fn des_xml_save_ctxt_ptr(_no: i32, _val: XmlSaveCtxtPtr, _nr: i32) {}

#[cfg(all(feature = "regexp", feature = "libxml_expr"))]
pub(crate) fn gen_xml_exp_ctxt_ptr(_no: c_int, _nr: c_int) -> XmlExpCtxtPtr {
    null_mut()
}

#[cfg(all(feature = "regexp", feature = "libxml_expr"))]
pub(crate) fn des_xml_exp_ctxt_ptr(_no: c_int, _val: XmlExpCtxtPtr, _nr: c_int) {}

#[cfg(all(feature = "regexp", feature = "libxml_expr"))]
pub(crate) fn gen_xml_exp_node_ptr(_no: c_int, _nr: c_int) -> XmlExpNodePtr {
    null_mut()
}
#[cfg(all(feature = "regexp", feature = "libxml_expr"))]
pub(crate) fn des_xml_exp_node_ptr(_no: c_int, _val: XmlExpNodePtr, _nr: c_int) {}

#[cfg(feature = "regexp")]
pub(crate) fn gen_xml_reg_exec_ctxt_ptr(_no: i32, _nr: i32) -> XmlRegExecCtxtPtr {
    null_mut()
}

#[cfg(feature = "regexp")]
pub(crate) fn des_xml_reg_exec_ctxt_ptr(_no: i32, _val: XmlRegExecCtxtPtr, _nr: i32) {}

#[cfg(feature = "regexp")]
pub(crate) fn gen_xml_regexp_ptr(_no: i32, _nr: i32) -> XmlRegexpPtr {
    null_mut()
}

#[cfg(feature = "regexp")]
pub(crate) fn des_xml_regexp_ptr(_no: i32, _val: XmlRegexpPtr, _nr: i32) {}

#[cfg(feature = "schema")]
pub(crate) fn gen_xml_schema_ptr(_no: c_int, _nr: c_int) -> XmlSchemaPtr {
    null_mut()
}

#[cfg(feature = "schema")]
pub(crate) fn des_xml_schema_ptr(_no: c_int, _val: XmlSchemaPtr, _nr: c_int) {}

#[cfg(feature = "schema")]
pub(crate) fn gen_xml_schema_valid_ctxt_ptr(_no: c_int, _nr: c_int) -> XmlSchemaValidCtxtPtr {
    null_mut()
}

#[cfg(feature = "schema")]
pub(crate) fn des_xml_schema_valid_ctxt_ptr(_no: c_int, _val: XmlSchemaValidCtxtPtr, _nr: c_int) {}

#[cfg(feature = "libxml_reader")]
pub(crate) unsafe extern "C" fn gen_xml_text_reader_ptr(no: c_int, _nr: c_int) -> XmlTextReaderPtr {
    use crate::libxml::xmlreader::xml_new_text_reader_filename;

    if no == 0 {
        return xml_new_text_reader_filename(c"test/ent2".as_ptr() as _);
    }
    if no == 1 {
        return xml_new_text_reader_filename(c"test/valid/REC-xml-19980210.xml".as_ptr() as _);
    }
    if no == 2 {
        return xml_new_text_reader_filename(c"test/valid/dtds/xhtml1-strict.dtd".as_ptr() as _);
    }
    null_mut()
}

#[cfg(feature = "libxml_reader")]
pub(crate) unsafe extern "C" fn des_xml_text_reader_ptr(
    _no: c_int,
    val: XmlTextReaderPtr,
    _nr: c_int,
) {
    use crate::libxml::xmlreader::xml_free_text_reader;

    if !val.is_null() {
        xml_free_text_reader(val);
    }
}

#[cfg(feature = "libxml_reader")]
pub(crate) unsafe extern "C" fn desret_xml_text_reader_ptr(val: XmlTextReaderPtr) {
    use crate::libxml::xmlreader::xml_free_text_reader;

    xml_free_text_reader(val);
}

#[cfg(feature = "libxml_reader")]
pub(crate) fn gen_xml_text_reader_error_func_ptr(
    _no: i32,
    _nr: i32,
) -> *mut Option<XmlTextReaderErrorFunc> {
    null_mut()
}

#[cfg(feature = "libxml_reader")]
pub(crate) fn des_xml_text_reader_error_func_ptr(
    _no: i32,
    _val: *mut Option<XmlTextReaderErrorFunc>,
    _nr: i32,
) {
}

#[cfg(feature = "libxml_reader")]
pub(crate) fn gen_xml_text_reader_locator_ptr(_no: i32, _nr: i32) -> XmlTextReaderLocatorPtr {
    null_mut()
}

#[cfg(feature = "libxml_reader")]
pub(crate) fn des_xml_text_reader_locator_ptr(_no: i32, _val: XmlTextReaderLocatorPtr, _nr: i32) {}

#[cfg(feature = "libxml_modules")]
pub(crate) fn gen_xml_module_ptr(_no: i32, _nr: i32) -> XmlModulePtr {
    null_mut()
}

#[cfg(feature = "libxml_modules")]
pub(crate) fn des_xml_module_ptr(_no: i32, _val: XmlModulePtr, _nr: i32) {}

pub(crate) fn gen_xml_generic_error_func_ptr(_no: i32, _nr: i32) -> Option<XmlGenericErrorFunc> {
    None
}

pub(crate) fn des_xml_generic_error_func_ptr(
    _no: i32,
    _val: Option<XmlGenericErrorFunc>,
    _nr: i32,
) {
}

pub(crate) fn gen_xml_error_ptr(_no: i32, _nr: i32) -> XmlErrorPtr {
    null_mut()
}

pub(crate) fn des_xml_error_ptr(_no: i32, _val: XmlErrorPtr, _nr: i32) {}

#[cfg(feature = "libxml_automata")]
pub(crate) fn gen_xml_automata_ptr(_no: i32, _nr: i32) -> XmlAutomataPtr {
    null_mut()
}

#[cfg(feature = "libxml_automata")]
pub(crate) fn des_xml_automata_ptr(_no: i32, _val: XmlAutomataPtr, _nr: i32) {}

#[cfg(feature = "libxml_automata")]
pub(crate) fn gen_xml_automata_state_ptr(_no: i32, _nr: i32) -> XmlAutomataStatePtr {
    null_mut()
}

#[cfg(feature = "libxml_automata")]
pub(crate) fn des_xml_automata_state_ptr(_no: i32, _val: XmlAutomataStatePtr, _nr: i32) {}

#[cfg(feature = "output")]
pub(crate) unsafe extern "C" fn desret_xml_output_buffer_ptr(val: XmlOutputBufferPtr) {
    use crate::libxml::xml_io::xmlOutputBufferClose;

    xmlOutputBufferClose(val);
}

#[cfg(feature = "xinclude")]
pub(crate) fn gen_xml_xinclude_ctxt_ptr(_no: i32, _nr: i32) -> XmlXincludeCtxtPtr {
    null_mut()
}

#[cfg(feature = "xinclude")]
pub(crate) fn des_xml_xinclude_ctxt_ptr(_no: i32, _val: XmlXincludeCtxtPtr, _nr: i32) {}

pub(crate) fn gen_xml_element_content_type(no: c_int, _nr: c_int) -> XmlElementContentType {
    if no == 1 {
        return XmlElementContentType::XmlElementContentElement;
    }
    if no == 2 {
        return XmlElementContentType::XmlElementContentOr;
    }
    if no == 3 {
        return XmlElementContentType::XmlElementContentPcdata;
    }
    if no == 4 {
        return XmlElementContentType::XmlElementContentSeq;
    }
    XmlElementContentType::XmlElementContentPcdata
}

pub(crate) fn des_xml_element_content_type(_no: c_int, _val: XmlElementContentType, _nr: c_int) {}

pub(crate) fn gen_xml_element_type_val(no: c_int, _nr: c_int) -> XmlElementTypeVal {
    if no == 1 {
        return XmlElementTypeVal::XmlElementTypeAny;
    }
    if no == 2 {
        return XmlElementTypeVal::XmlElementTypeElement;
    }
    if no == 3 {
        return XmlElementTypeVal::XmlElementTypeEmpty;
    }
    if no == 4 {
        return XmlElementTypeVal::XmlElementTypeMixed;
    }
    XmlElementTypeVal::XmlElementTypeUndefined
}

pub(crate) fn des_xml_element_type_val(_no: c_int, _val: XmlElementTypeVal, _nr: c_int) {}

pub(crate) fn gen_xml_attribute_default(no: c_int, _nr: c_int) -> XmlAttributeDefault {
    if no == 1 {
        return XmlAttributeDefault::XmlAttributeFixed;
    }
    if no == 2 {
        return XmlAttributeDefault::XmlAttributeImplied;
    }
    if no == 3 {
        return XmlAttributeDefault::XmlAttributeNone;
    }
    if no == 4 {
        return XmlAttributeDefault::XmlAttributeRequired;
    }
    XmlAttributeDefault::XmlAttributeNone
}

pub(crate) fn des_xml_attribute_default(_no: c_int, _val: XmlAttributeDefault, _nr: c_int) {}

pub(crate) fn gen_xml_attribute_type(no: c_int, _nr: c_int) -> XmlAttributeType {
    if no == 1 {
        return XmlAttributeType::XmlAttributeCdata;
    }
    if no == 2 {
        return XmlAttributeType::XmlAttributeEntities;
    }
    if no == 3 {
        return XmlAttributeType::XmlAttributeEntity;
    }
    if no == 4 {
        return XmlAttributeType::XmlAttributeEnumeration;
    }
    XmlAttributeType::XmlAttributeCdata
}

pub(crate) fn des_xml_attribute_type(_no: c_int, _val: XmlAttributeType, _nr: c_int) {}

pub(crate) unsafe extern "C" fn gen_xml_valid_ctxt_ptr(no: c_int, _nr: c_int) -> XmlValidCtxtPtr {
    #[cfg(feature = "valid")]
    if no == 0 {
        return xml_new_valid_ctxt();
    }
    null_mut()
}
pub(crate) unsafe extern "C" fn des_xml_valid_ctxt_ptr(
    _no: c_int,
    val: XmlValidCtxtPtr,
    _nr: c_int,
) {
    #[cfg(feature = "valid")]
    if !val.is_null() {
        xml_free_valid_ctxt(val);
    }
}

pub(crate) fn gen_xml_attribute_ptr(_no: i32, _nr: i32) -> XmlAttributePtr {
    null_mut()
}

pub(crate) fn des_xml_attribute_ptr(_no: i32, _val: XmlAttributePtr, _nr: i32) {}

pub(crate) fn gen_xml_attribute_table_ptr(_no: i32, _nr: i32) -> XmlAttributeTablePtr {
    null_mut()
}

pub(crate) fn des_xml_attribute_table_ptr(_no: i32, _val: XmlAttributeTablePtr, _nr: i32) {}

pub(crate) fn gen_xml_element_ptr(_no: i32, _nr: i32) -> XmlElementPtr {
    null_mut()
}

pub(crate) fn des_xml_element_ptr(_no: i32, _val: XmlElementPtr, _nr: i32) {}

pub(crate) fn gen_xml_element_table_ptr(_no: i32, _nr: i32) -> XmlElementTablePtr {
    null_mut()
}

pub(crate) fn des_xml_element_table_ptr(_no: i32, _val: XmlElementTablePtr, _nr: i32) {}

pub(crate) fn gen_xml_notation_ptr(_no: i32, _nr: i32) -> XmlNotationPtr {
    null_mut()
}

pub(crate) fn des_xml_notation_ptr(_no: i32, _val: XmlNotationPtr, _nr: i32) {}

pub(crate) fn gen_xml_notation_table_ptr(_no: i32, _nr: i32) -> XmlNotationTablePtr {
    null_mut()
}

pub(crate) fn des_xml_notation_table_ptr(_no: i32, _val: XmlNotationTablePtr, _nr: i32) {}

pub(crate) fn gen_char_ptr(_no: i32, _nr: i32) -> *mut c_char {
    null_mut()
}

pub(crate) fn des_char_ptr(_no: i32, _val: *mut c_char, _nr: i32) {}

pub(crate) fn gen_xml_uriptr(_no: i32, _nr: i32) -> XmlURIPtr {
    null_mut()
}

pub(crate) fn des_xml_uriptr(_no: i32, _val: XmlURIPtr, _nr: i32) {}

pub(crate) unsafe extern "C" fn gen_eaten_name(no: c_int, _nr: c_int) -> *mut XmlChar {
    if no == 0 {
        return xml_strdup(c"eaten".as_ptr() as _);
    }
    null_mut()
}
pub(crate) fn des_eaten_name(_no: c_int, _val: *mut XmlChar, _nr: c_int) {}

unsafe extern "C" fn get_api_ns() -> XmlNsPtr {
    get_api_root();
    if !API_ROOT.load(Ordering::Relaxed).is_null() {
        API_NS.store(
            (*API_ROOT.load(Ordering::Relaxed)).ns_def,
            Ordering::Relaxed,
        );
    }
    API_NS.load(Ordering::Relaxed)
}

pub(crate) unsafe extern "C" fn gen_xml_ns_ptr(no: c_int, _nr: c_int) -> XmlNsPtr {
    if no == 0 {
        return get_api_ns();
    }
    null_mut()
}
pub(crate) unsafe extern "C" fn des_xml_ns_ptr(no: c_int, _val: XmlNsPtr, _nr: c_int) {
    if no == 0 {
        free_api_doc();
    }
}

unsafe extern "C" fn get_api_dtd() -> XmlDtdPtr {
    if API_DTD.load(Ordering::Relaxed).is_null()
        || (*API_DTD.load(Ordering::Relaxed)).typ != XmlElementType::XmlDtdNode
    {
        get_api_doc();
        if !API_DOC.load(Ordering::Relaxed).is_null()
            && !(*API_DOC.load(Ordering::Relaxed)).children.is_null()
            && (*(*API_DOC.load(Ordering::Relaxed)).children).typ == XmlElementType::XmlDtdNode
        {
            API_DTD.store(
                (*API_DOC.load(Ordering::Relaxed)).children as XmlDtdPtr,
                Ordering::Relaxed,
            );
        }
    }
    API_DTD.load(Ordering::Relaxed)
}

pub(crate) unsafe extern "C" fn gen_xml_dtd_ptr(no: c_int, _nr: c_int) -> XmlDtdPtr {
    if no == 0 {
        return xml_new_dtd(
            null_mut(),
            c"dtd".as_ptr() as _,
            c"foo".as_ptr() as _,
            c"bar".as_ptr() as _,
        );
    }
    if no == 1 {
        return get_api_dtd();
    }
    null_mut()
}
pub(crate) unsafe extern "C" fn des_xml_dtd_ptr(no: c_int, val: XmlDtdPtr, _nr: c_int) {
    if no == 1 {
        free_api_doc();
    } else if !val.is_null() {
        xml_unlink_node(val as XmlNodePtr);
        xml_free_node(val as XmlNodePtr);
    }
}

pub(crate) const GEN_NB_XML_BUFFER_ALLOCATION_SCHEME: i32 = 4;

pub(crate) fn gen_xml_buffer_allocation_scheme(no: c_int, _nr: c_int) -> XmlBufferAllocationScheme {
    if no == 1 {
        return XmlBufferAllocationScheme::XmlBufferAllocBounded;
    }
    if no == 2 {
        return XmlBufferAllocationScheme::XmlBufferAllocDoubleit;
    }
    if no == 3 {
        return XmlBufferAllocationScheme::XmlBufferAllocExact;
    }
    if no == 4 {
        return XmlBufferAllocationScheme::XmlBufferAllocHybrid;
    }
    XmlBufferAllocationScheme::XmlBufferAllocDoubleit
}

pub(crate) fn des_xml_buffer_allocation_scheme(
    _no: c_int,
    _val: XmlBufferAllocationScheme,
    _nr: c_int,
) {
}

pub(crate) fn desret_xml_buffer_allocation_scheme(_val: XmlBufferAllocationScheme) {}

pub(crate) unsafe extern "C" fn desret_xml_buffer_ptr(val: XmlBufferPtr) {
    xml_buffer_free(val);
}

unsafe extern "C" fn get_api_attr() -> XmlAttrPtr {
    #[cfg(any(
        feature = "tree",
        feature = "xinclude",
        feature = "schema",
        feature = "html"
    ))]
    static mut NR: c_int = 0;
    #[cfg(any(
        feature = "tree",
        feature = "xinclude",
        feature = "schema",
        feature = "html"
    ))]
    let mut name: [XmlChar; 20] = [0; 20];

    if API_ROOT.load(Ordering::Relaxed).is_null()
        || (*API_ROOT.load(Ordering::Relaxed)).typ != XmlElementType::XmlElementNode
    {
        get_api_root();
    }
    if API_ROOT.load(Ordering::Relaxed).is_null() {
        return null_mut();
    }
    if !(*API_ROOT.load(Ordering::Relaxed)).properties.is_null() {
        API_ATTR.store(
            (*API_ROOT.load(Ordering::Relaxed)).properties,
            Ordering::Relaxed,
        );
        return (*API_ROOT.load(Ordering::Relaxed)).properties;
    }
    API_ATTR.store(null_mut(), Ordering::Relaxed);
    #[cfg(any(
        feature = "tree",
        feature = "xinclude",
        feature = "schema",
        feature = "html"
    ))]
    {
        snprintf(name.as_mut_ptr() as _, 20, c"foo%d".as_ptr() as _, NR);
        NR += 1;
        API_ATTR.store(
            xml_set_prop(
                API_ROOT.load(Ordering::Relaxed),
                name.as_ptr() as _,
                c"bar".as_ptr() as _,
            ),
            Ordering::Relaxed,
        );
    }
    API_ATTR.load(Ordering::Relaxed)
}

pub(crate) unsafe extern "C" fn gen_xml_attr_ptr(no: c_int, _nr: c_int) -> XmlAttrPtr {
    if no == 0 {
        return get_api_attr();
    }
    null_mut()
}
pub(crate) unsafe extern "C" fn des_xml_attr_ptr(no: c_int, _val: XmlAttrPtr, _nr: c_int) {
    if no == 0 {
        free_api_doc();
    }
}

pub(crate) unsafe extern "C" fn gen_xml_node_ptr_in(no: c_int, _nr: c_int) -> XmlNodePtr {
    if no == 0 {
        return xml_new_pi(c"test".as_ptr() as _, null_mut());
    }
    if no == 0 {
        return xml_new_text(c"text".as_ptr() as _);
    }
    null_mut()
}

pub(crate) fn des_xml_node_ptr_in(_no: c_int, _val: XmlNodePtr, _nr: c_int) {}

pub(crate) fn gen_const_xml_buf_ptr(_no: i32, _nr: i32) -> *const XmlBuf {
    null()
}

pub(crate) fn des_const_xml_buf_ptr(_no: i32, _val: *const XmlBuf, _nr: i32) {}

pub(crate) fn gen_xml_buf_ptr(_no: i32, _nr: i32) -> XmlBufferPtr {
    null_mut()
}

pub(crate) fn des_xml_buf_ptr(_no: i32, _val: XmlBufferPtr, _nr: i32) {}

pub(crate) fn gen_const_xml_buffer_ptr(_no: i32, _nr: i32) -> *const XmlBuffer {
    null()
}

pub(crate) fn des_const_xml_buffer_ptr(_no: i32, _val: *const XmlBuffer, _nr: i32) {}

pub(crate) fn gen_xml_domwrap_ctxt_ptr(_no: i32, _nr: i32) -> XmlDOMWrapCtxtPtr {
    null_mut()
}

pub(crate) fn des_xml_domwrap_ctxt_ptr(_no: i32, _val: XmlDOMWrapCtxtPtr, _nr: i32) {}

#[cfg(feature = "libxml_schematron")]
pub(crate) fn gen_xml_schematron_valid_ctxt_ptr(_no: i32, _nr: i32) -> XmlSchematronValidCtxtPtr {
    null_mut()
}

#[cfg(feature = "libxml_schematron")]
pub(crate) fn des_xml_schematron_valid_ctxt_ptr(
    _no: i32,
    _val: XmlSchematronValidCtxtPtr,
    _nr: i32,
) {
}

#[cfg(feature = "schema")]
pub(crate) unsafe extern "C" fn desret_xml_schema_parser_ctxt_ptr(val: XmlSchemaParserCtxtPtr) {
    use crate::libxml::xmlschemas::xml_schema_free_parser_ctxt;

    xml_schema_free_parser_ctxt(val);
}

#[cfg(feature = "schema")]
pub(crate) fn desret_xml_schema_type_ptr(_val: XmlSchemaTypePtr) {}

#[cfg(feature = "schema")]
pub(crate) unsafe extern "C" fn desret_xml_relaxng_parser_ctxt_ptr(val: XmlRelaxNGParserCtxtPtr) {
    use crate::libxml::relaxng::xml_relaxng_free_parser_ctxt;

    xml_relaxng_free_parser_ctxt(val);
}

#[cfg(any(
    feature = "libxml_modules",
    feature = "libxml_reader",
    feature = "schema"
))]
pub(crate) fn gen_void_ptr_ptr(_no: c_int, _nr: c_int) -> *mut *mut c_void {
    null_mut()
}

#[cfg(any(
    feature = "libxml_modules",
    feature = "libxml_reader",
    feature = "schema"
))]
pub(crate) fn des_void_ptr_ptr(_no: c_int, _val: *mut *mut c_void, _nr: c_int) {}

#[cfg(feature = "schema")]
pub(crate) fn gen_xml_relaxng_valid_ctxt_ptr(_no: i32, _nr: i32) -> XmlRelaxNGValidCtxtPtr {
    null_mut()
}

#[cfg(feature = "schema")]
pub(crate) fn des_xml_relaxng_valid_ctxt_ptr(_no: i32, _val: XmlRelaxNGValidCtxtPtr, _nr: i32) {}

#[cfg(feature = "schema")]
pub(crate) fn gen_xml_relaxng_parser_ctxt_ptr(_no: i32, _nr: i32) -> XmlRelaxNGParserCtxtPtr {
    null_mut()
}

#[cfg(feature = "schema")]
pub(crate) fn des_xml_relaxng_parser_ctxt_ptr(_no: i32, _val: XmlRelaxNGParserCtxtPtr, _nr: i32) {}

#[cfg(feature = "schema")]
pub(crate) fn gen_xml_relaxng_validity_error_func_ptr(
    _no: i32,
    _nr: i32,
) -> *mut Option<XmlRelaxNGValidityErrorFunc> {
    null_mut()
}

#[cfg(feature = "schema")]
pub(crate) fn des_xml_relaxng_validity_error_func_ptr(
    _no: i32,
    _val: *mut Option<XmlRelaxNGValidityErrorFunc>,
    _nr: i32,
) {
}

#[cfg(feature = "schema")]
pub(crate) fn gen_xml_relaxng_validity_warning_func_ptr(
    _no: i32,
    _nr: i32,
) -> *mut Option<XmlRelaxNGValidityWarningFunc> {
    null_mut()
}

#[cfg(feature = "schema")]
pub(crate) fn des_xml_relaxng_validity_warning_func_ptr(
    _no: i32,
    _val: *mut Option<XmlRelaxNGValidityWarningFunc>,
    _nr: i32,
) {
}

#[cfg(feature = "schema")]
pub(crate) fn gen_xml_relaxng_ptr(_no: i32, _nr: i32) -> XmlRelaxNGPtr {
    null_mut()
}

#[cfg(feature = "schema")]
pub(crate) fn des_xml_relaxng_ptr(_no: i32, _val: XmlRelaxNGPtr, _nr: i32) {}

#[cfg(feature = "libxml_pattern")]
pub(crate) fn gen_xml_pattern_ptr(_no: c_int, _nr: c_int) -> XmlPatternPtr {
    null_mut()
}
#[cfg(feature = "libxml_pattern")]
pub(crate) fn des_xml_pattern_ptr(_no: c_int, _val: XmlPatternPtr, _nr: c_int) {}

#[cfg(feature = "libxml_pattern")]
pub(crate) fn gen_xml_stream_ctxt_ptr(_no: i32, _nr: i32) -> XmlStreamCtxtPtr {
    null_mut()
}

#[cfg(feature = "libxml_pattern")]
pub(crate) fn des_xml_stream_ctxt_ptr(_no: i32, _val: XmlStreamCtxtPtr, _nr: i32) {}

pub(crate) fn gen_xml_char(no: c_int, _nr: c_int) -> XmlChar {
    if no == 0 {
        return b'a';
    }
    if no == 1 {
        return b' ';
    }
    if no == 2 {
        return 0xf8;
    }
    0
}

pub(crate) fn des_xml_char(_no: c_int, _val: XmlChar, _nr: c_int) {}

pub(crate) fn desret_xml_parser_errors(_val: XmlParserErrors) {}

pub(crate) unsafe extern "C" fn desret_xml_node_ptr(val: XmlNodePtr) {
    if !val.is_null()
        && val != API_ROOT.load(Ordering::Relaxed)
        && val != API_DOC.load(Ordering::Relaxed) as XmlNodePtr
    {
        xml_unlink_node(val);
        xml_free_node(val);
    }
}
pub(crate) unsafe extern "C" fn desret_xml_attr_ptr(val: XmlAttrPtr) {
    if !val.is_null() {
        xml_unlink_node(val as XmlNodePtr);
        xml_free_node(val as XmlNodePtr);
    }
}

pub(crate) unsafe extern "C" fn desret_xml_element_ptr(val: XmlElementPtr) {
    if !val.is_null() {
        xml_unlink_node(val as XmlNodePtr);
    }
}

pub(crate) unsafe extern "C" fn desret_xml_attribute_ptr(val: XmlAttributePtr) {
    if !val.is_null() {
        xml_unlink_node(val as XmlNodePtr);
    }
}

pub(crate) fn desret_xml_ns_ptr(_val: XmlNsPtr) {}

pub(crate) unsafe extern "C" fn desret_xml_dtd_ptr(val: XmlDtdPtr) {
    desret_xml_node_ptr(val as XmlNodePtr);
}

pub(crate) unsafe extern "C" fn gen_xml_parser_input_buffer_ptr(
    no: c_int,
    _nr: c_int,
) -> XmlParserInputBufferPtr {
    if no == 0 {
        return xmlParserInputBufferCreateFilename(
            c"missing.xml".as_ptr(),
            XmlCharEncoding::XmlCharEncodingNone,
        );
    }
    if no == 1 {
        return xmlParserInputBufferCreateFilename(
            c"<foo/>".as_ptr(),
            XmlCharEncoding::XmlCharEncodingNone,
        );
    }
    if no == 2 {
        return xmlParserInputBufferCreateFilename(
            c"test/ent2".as_ptr(),
            XmlCharEncoding::XmlCharEncodingNone,
        );
    }
    if no == 3 {
        return xmlParserInputBufferCreateFilename(
            c"test/valid/REC-xml-19980210.xml".as_ptr(),
            XmlCharEncoding::XmlCharEncodingNone,
        );
    }
    if no == 4 {
        return xmlParserInputBufferCreateFilename(
            c"test/valid/dtds/xhtml1-strict.dtd".as_ptr(),
            XmlCharEncoding::XmlCharEncodingNone,
        );
    }
    if no == 5 {
        return xmlParserInputBufferCreateFilename(
            REMOTE1GOOD.as_ptr(),
            XmlCharEncoding::XmlCharEncodingNone,
        );
    }
    if no == 6 {
        return xmlParserInputBufferCreateFilename(
            REMOTE1BAD.as_ptr(),
            XmlCharEncoding::XmlCharEncodingNone,
        );
    }
    null_mut()
}
pub(crate) unsafe extern "C" fn des_xml_parser_input_buffer_ptr(
    _no: c_int,
    val: XmlParserInputBufferPtr,
    _nr: c_int,
) {
    xmlFreeParserInputBuffer(val);
}

pub(crate) fn gen_xml_feature(no: c_int, _nr: c_int) -> Option<XmlFeature> {
    if no == 1 {
        return Some(XmlFeature::XmlWithAutomata);
    }
    if no == 2 {
        return Some(XmlFeature::XmlWithC14n);
    }
    if no == 3 {
        return Some(XmlFeature::XmlWithCatalog);
    }
    if no == 4 {
        return Some(XmlFeature::XmlWithDebug);
    }
    None
}

pub(crate) fn des_xml_feature(_no: c_int, _val: Option<XmlFeature>, _nr: c_int) {}

#[cfg(any(feature = "sax1", feature = "valid", feature = "push"))]
pub(crate) unsafe extern "C" fn gen_xml_saxhandler_ptr(no: c_int, _nr: c_int) -> XmlSAXHandlerPtr {
    #[cfg(feature = "sax1")]
    if no == 0 {
        use crate::libxml::globals::xml_default_sax_handler;
        return xml_default_sax_handler() as _;
    }
    null_mut()
}

#[cfg(any(feature = "sax1", feature = "valid", feature = "push"))]
pub(crate) fn des_xml_saxhandler_ptr(_no: c_int, _val: XmlSAXHandlerPtr, _nr: c_int) {}

pub(crate) unsafe extern "C" fn desret_xml_parser_ctxt_ptr(val: XmlParserCtxtPtr) {
    xml_free_parser_ctxt(val);
}

pub(crate) unsafe extern "C" fn desret_xml_parser_input_buffer_ptr(val: XmlParserInputBufferPtr) {
    xmlFreeParserInputBuffer(val);
}

pub(crate) fn gen_xml_parser_node_info_seq_ptr(_no: c_int, _nr: c_int) -> XmlParserNodeInfoSeqPtr {
    null_mut()
}

pub(crate) fn des_xml_parser_node_info_seq_ptr(
    _no: c_int,
    _val: XmlParserNodeInfoSeqPtr,
    _nr: c_int,
) {
}

pub(crate) fn desret_const_xml_parser_node_info_ptr(_val: *const XmlParserNodeInfo) {}

pub(crate) unsafe extern "C" fn gen_xml_parser_ctxt_ptr(no: c_int, _nr: c_int) -> XmlParserCtxtPtr {
    if no == 0 {
        return xml_new_parser_ctxt();
    }
    if no == 1 {
        return xml_create_memory_parser_ctxt(c"<doc/>".as_ptr() as _, 6);
    }
    null_mut()
}

pub(crate) unsafe extern "C" fn des_xml_parser_ctxt_ptr(
    _no: c_int,
    val: XmlParserCtxtPtr,
    _nr: c_int,
) {
    if !val.is_null() {
        xml_free_parser_ctxt(val);
    }
}

pub(crate) fn gen_xml_parser_input_ptr(_no: i32, _nr: i32) -> XmlParserInputPtr {
    null_mut()
}

pub(crate) fn des_xml_parser_input_ptr(_no: i32, _val: XmlParserInputPtr, _nr: i32) {}

pub(crate) fn gen_const_xml_parser_node_info_seq_ptr(
    _no: i32,
    _nr: i32,
) -> *const XmlParserNodeInfoSeq {
    null()
}

pub(crate) fn des_const_xml_parser_node_info_seq_ptr(
    _no: i32,
    _val: *const XmlParserNodeInfoSeq,
    _nr: i32,
) {
}

pub(crate) fn gen_const_xml_parser_ctxt_ptr(_no: i32, _nr: i32) -> *const XmlParserCtxt {
    null()
}

pub(crate) fn des_const_xml_parser_ctxt_ptr(_no: i32, _val: *const XmlParserCtxt, _nr: i32) {}

pub(crate) fn gen_const_xml_node_ptr(_no: i32, _nr: i32) -> *const XmlNode {
    null()
}

pub(crate) fn des_const_xml_node_ptr(_no: i32, _val: *const XmlNode, _nr: i32) {}

pub(crate) fn gen_const_xml_parser_node_info_ptr(_no: i32, _nr: i32) -> *const XmlParserNodeInfo {
    null()
}

pub(crate) fn des_const_xml_parser_node_info_ptr(
    _no: i32,
    _val: *const XmlParserNodeInfo,
    _nr: i32,
) {
}

pub(crate) fn gen_xml_node_ptr_ptr(_no: i32, _nr: i32) -> *mut XmlNodePtr {
    null_mut()
}

pub(crate) fn des_xml_node_ptr_ptr(_no: i32, _val: *mut XmlNodePtr, _nr: i32) {}

pub(crate) fn gen_const_xml_saxhandler_ptr(_no: i32, _nr: i32) -> *const XmlSAXHandler {
    null()
}

pub(crate) fn des_const_xml_saxhandler_ptr(_no: i32, _val: *const XmlSAXHandler, _nr: i32) {}

#[cfg(feature = "http")]
pub(crate) unsafe extern "C" fn desret_xml_nano_httpctxt_ptr(val: *mut c_void) {
    use crate::libxml::nanohttp::xml_nanohttp_close;

    xml_nanohttp_close(val);
}

#[cfg(feature = "http")]
pub(crate) unsafe extern "C" fn gen_xml_nano_httpctxt_ptr(no: c_int, _nr: c_int) -> *mut c_void {
    use crate::libxml::nanohttp::xml_nanohttp_open;

    if no == 0 {
        return xml_nanohttp_open(REMOTE1GOOD.as_ptr() as _, null_mut());
    }
    if no == 1 {
        return xml_nanohttp_open(REMOTE2GOOD.as_ptr() as _, null_mut());
    }
    if no == 2 {
        return xml_nanohttp_open(REMOTE1BAD.as_ptr() as _, null_mut());
    }
    null_mut()
}

#[cfg(feature = "http")]
pub(crate) unsafe extern "C" fn des_xml_nano_httpctxt_ptr(
    _no: c_int,
    val: *mut c_void,
    _nr: c_int,
) {
    use crate::libxml::nanohttp::xml_nanohttp_close;

    if !val.is_null() {
        xml_nanohttp_close(val);
    }
}

pub(crate) fn gen_char_ptr_ptr(_no: i32, _nr: i32) -> *mut *mut c_char {
    null_mut()
}

pub(crate) fn des_char_ptr_ptr(_no: i32, _vall: *mut *mut c_char, _nr: i32) {}

pub(crate) unsafe extern "C" fn gen_xml_list_ptr(no: c_int, _nr: c_int) -> XmlListPtr {
    if no == 0 {
        return xml_list_create(None, None);
    }
    null_mut()
}
pub(crate) unsafe extern "C" fn des_xml_list_ptr(_no: c_int, val: XmlListPtr, _nr: c_int) {
    if !val.is_null() {
        xml_list_delete(val);
    }
}

pub(crate) fn gen_xml_link_ptr(_no: i32, _nr: i32) -> XmlLinkPtr {
    null_mut()
}

pub(crate) fn des_xml_link_ptr(_no: i32, _val: XmlLinkPtr, _nr: i32) {}

pub(crate) fn gen_const_xml_list_ptr(_no: i32, _nr: i32) -> *const XmlList {
    null_mut()
}

pub(crate) fn des_const_xml_list_ptr(_no: i32, _val: *const XmlList, _nr: i32) {}

unsafe extern "C" fn test_xml_hash_deallocator(_payload: *mut c_void, _namee: *const XmlChar) {}

pub(crate) fn gen_xml_hash_deallocator(no: c_int, _nr: c_int) -> Option<XmlHashDeallocator> {
    if no == 0 {
        return Some(test_xml_hash_deallocator);
    }
    None
}

pub(crate) fn des_xml_hash_deallocator(_no: c_int, _val: Option<XmlHashDeallocator>, _nr: c_int) {}

pub(crate) fn desret_xml_char(_val: XmlChar) {}

pub(crate) fn desret_long(_val: c_long) {}

pub(crate) fn desret_unsigned_long(_val: c_ulong) {}

pub(crate) unsafe extern "C" fn gen_xml_hash_table_ptr(no: c_int, _nr: c_int) -> XmlHashTablePtr {
    if no == 0 {
        return xml_hash_create(10);
    }
    null_mut()
}
pub(crate) unsafe extern "C" fn des_xml_hash_table_ptr(
    _no: c_int,
    val: XmlHashTablePtr,
    _nr: c_int,
) {
    if !val.is_null() {
        xml_hash_free(val, None);
    }
}

pub(crate) fn gen_xml_entities_table_ptr(_no: i32, _nr: i32) -> XmlEntitiesTablePtr {
    null_mut()
}

pub(crate) fn des_xml_entities_table_ptr(_no: i32, _val: XmlEntitiesTablePtr, _nr: i32) {}

pub(crate) fn gen_xml_entity_ptr(_no: i32, _nr: i32) -> XmlEntityPtr {
    null_mut()
}

pub(crate) fn des_xml_entity_ptr(_no: i32, _val: XmlEntityPtr, _nr: i32) {}

pub(crate) fn gen_const_xml_doc_ptr(_no: i32, _nr: i32) -> *const XmlDoc {
    null_mut()
}

pub(crate) fn des_const_xml_doc_ptr(_no: i32, _val: *const XmlDoc, _nr: i32) {}

pub(crate) fn desret_xml_char_encoding(_val: XmlCharEncoding) {}

pub(crate) fn gen_xml_char_encoding_handler_ptr(_no: i32, _nr: i32) -> XmlCharEncodingHandlerPtr {
    null_mut()
}

pub(crate) fn des_xml_char_encoding_handler_ptr(
    _no: i32,
    _val: XmlCharEncodingHandlerPtr,
    _nr: i32,
) {
}

pub(crate) unsafe extern "C" fn gen_xml_dict_ptr(no: c_int, _nr: c_int) -> XmlDictPtr {
    if no == 0 {
        return xml_dict_create();
    }
    null_mut()
}

pub(crate) unsafe extern "C" fn des_xml_dict_ptr(_no: c_int, val: XmlDictPtr, _nr: c_int) {
    if !val.is_null() {
        xml_dict_free(val);
    }
}

pub(crate) fn gen_const_xml_ch_range_group_ptr(_no: i32, _nr: i32) -> *const XmlChRangeGroup {
    null()
}

pub(crate) fn des_const_xml_ch_range_group_ptr(_no: i32, _val: *const XmlChRangeGroup, _nr: i32) {}

pub(crate) unsafe extern "C" fn desret_xml_doc_ptr(val: XmlDocPtr) {
    if val != API_DOC.load(Ordering::Relaxed) {
        xml_free_doc(val);
    }
}

pub(crate) unsafe extern "C" fn desret_xml_dict_ptr(val: XmlDictPtr) {
    xml_dict_free(val);
}

#[cfg(feature = "catalog")]
pub(crate) fn gen_xml_catalog_prefer(no: c_int, _nr: c_int) -> XmlCatalogPrefer {
    if no == 1 {
        return XmlCatalogPrefer::None;
    }
    if no == 2 {
        return XmlCatalogPrefer::Public;
    }
    if no == 3 {
        return XmlCatalogPrefer::System;
    }
    XmlCatalogPrefer::None
}

#[cfg(feature = "catalog")]
pub(crate) fn des_xml_catalog_prefer(_no: c_int, _val: XmlCatalogPrefer, _nr: c_int) {}

#[cfg(feature = "catalog")]
pub(crate) fn desret_xml_catalog_prefer(_val: XmlCatalogPrefer) {}

#[cfg(feature = "catalog")]
pub(crate) fn gen_xml_catalog_allow(no: c_int, _nr: c_int) -> XmlCatalogAllow {
    if no == 1 {
        return XmlCatalogAllow::All;
    }
    if no == 2 {
        return XmlCatalogAllow::Document;
    }
    if no == 3 {
        return XmlCatalogAllow::Global;
    }
    XmlCatalogAllow::None
}

#[cfg(feature = "catalog")]
pub(crate) fn des_xml_catalog_allow(_no: c_int, _val: XmlCatalogAllow, _nr: c_int) {}

#[cfg(feature = "catalog")]
pub(crate) fn desret_xml_catalog_allow(_val: XmlCatalogAllow) {}

pub(crate) fn desret_void_ptr(_val: *mut c_void) {}

pub(crate) fn desret_const_char_ptr(_val: *const c_char) {}

pub(crate) unsafe extern "C" fn desret_xml_char_ptr(val: *mut XmlChar) {
    if !val.is_null() {
        xml_free(val as _);
    }
}

#[cfg(feature = "catalog")]
pub(crate) fn gen_xml_catalog_ptr(_no: i32, _nr: i32) -> XmlCatalogPtr {
    null_mut()
}
#[cfg(feature = "catalog")]
pub(crate) fn des_xml_catalog_ptr(_no: i32, _val: XmlCatalogPtr, _nr: i32) {}

#[cfg(feature = "xpath")]
pub(crate) fn gen_xml_node_set_ptr(_no: c_int, _nr: c_int) -> XmlNodeSetPtr {
    null_mut()
}
#[cfg(feature = "xpath")]
pub(crate) fn des_xml_node_set_ptr(_no: c_int, _val: XmlNodeSetPtr, _nr: c_int) {}

pub(crate) fn gen_xml_saxlocator_ptr(_no: i32, _nr: i32) -> XmlSaxlocatorPtr {
    null_mut()
}
pub(crate) fn des_xml_saxlocator_ptr(_no: i32, _val: XmlSaxlocatorPtr, _nr: i32) {}

pub(crate) unsafe extern "C" fn desret_xml_parser_input_ptr(val: XmlParserInputPtr) {
    xml_free_input_stream(val);
}

pub(crate) unsafe extern "C" fn desret_xml_entity_ptr(val: XmlEntityPtr) {
    if !val.is_null() {
        xml_unlink_node(val as XmlNodePtr);
        xml_free_node(val as XmlNodePtr);
    }
}

pub(crate) unsafe fn gen_xml_char_ptr(no: c_int, _nr: c_int) -> *mut XmlChar {
    if no == 0 {
        return addr_of_mut!(CHARTAB[0]);
    }
    null_mut()
}
pub(crate) fn des_xml_char_ptr(_no: c_int, _val: *mut XmlChar, _nr: c_int) {}

pub(crate) fn gen_xml_element_content_ptr(_no: c_int, _nr: c_int) -> XmlElementContentPtr {
    null_mut()
}
pub(crate) unsafe extern "C" fn des_xml_element_content_ptr(
    _no: c_int,
    val: XmlElementContentPtr,
    _nr: c_int,
) {
    if !val.is_null() {
        xml_free_element_content(val);
    }
}
pub(crate) unsafe extern "C" fn desret_xml_element_content_ptr(val: XmlElementContentPtr) {
    if !val.is_null() {
        xml_free_element_content(val);
    }
}

pub(crate) fn gen_xml_enumeration_ptr(_no: i32, _nr: i32) -> XmlEnumerationPtr {
    null_mut()
}
pub(crate) fn des_xml_enumeration_ptr(_no: i32, _val: XmlEnumerationPtr, _nr: i32) {}

unsafe extern "C" fn get_api_root() -> XmlNodePtr {
    if (API_ROOT.load(Ordering::Relaxed).is_null())
        || (*API_ROOT.load(Ordering::Relaxed)).typ != XmlElementType::XmlElementNode
    {
        get_api_doc();
        if !API_DOC.load(Ordering::Relaxed).is_null()
            && !(*API_DOC.load(Ordering::Relaxed)).children.is_null()
            && !(*(*API_DOC.load(Ordering::Relaxed)).children)
                .next
                .is_null()
            && (*(*(*API_DOC.load(Ordering::Relaxed)).children).next).typ
                == XmlElementType::XmlElementNode
        {
            API_ROOT.store(
                (*(*API_DOC.load(Ordering::Relaxed)).children).next,
                Ordering::Relaxed,
            );
        }
    }
    API_ROOT.load(Ordering::Relaxed)
}

unsafe extern "C" fn get_api_doc() -> XmlDocPtr {
    if API_DOC.load(Ordering::Relaxed).is_null() {
        API_DOC.store(xml_read_memory(c"<!DOCTYPE root [<!ELEMENT root EMPTY>]><root xmlns:h='http://example.com/' h:foo='bar'/>".as_ptr() as _, 88, c"root_test".as_ptr() as _, null_mut(), 0), Ordering::Relaxed);
        API_ROOT.store(null_mut(), Ordering::Relaxed);
        API_ATTR.store(null_mut(), Ordering::Relaxed);
    }
    API_DOC.load(Ordering::Relaxed)
}

unsafe extern "C" fn free_api_doc() {
    xml_free_doc(API_DOC.load(Ordering::Relaxed));
    API_DOC.store(null_mut(), Ordering::Relaxed);
    API_DTD.store(null_mut(), Ordering::Relaxed);
    API_ROOT.store(null_mut(), Ordering::Relaxed);
    API_ATTR.store(null_mut(), Ordering::Relaxed);
    API_NS.store(null_mut(), Ordering::Relaxed);
}

pub(crate) unsafe extern "C" fn gen_xml_node_ptr(no: c_int, _nr: c_int) -> XmlNodePtr {
    if no == 0 {
        return xml_new_pi(c"test".as_ptr() as _, null_mut());
    }
    if no == 1 {
        return get_api_root();
    }
    null_mut()
    /*     if no == 2 {
    // return((xmlNodePtr) get_api_doc()); */
}
pub(crate) unsafe extern "C" fn des_xml_node_ptr(no: c_int, val: XmlNodePtr, _nr: c_int) {
    if no == 1 {
        free_api_doc();
    } else if !val.is_null() {
        xml_unlink_node(val);
        xml_free_node(val);
    }
}

pub(crate) unsafe extern "C" fn gen_xml_buffer_ptr(no: c_int, _nr: c_int) -> XmlBufferPtr {
    if no == 0 {
        return xml_buffer_create();
    }
    if no == 1 {
        return xml_buffer_create_static(STATIC_BUF_CONTENT.as_ptr() as _, 13);
    }
    null_mut()
}
pub(crate) unsafe extern "C" fn des_xml_buffer_ptr(_no: c_int, val: XmlBufferPtr, _nr: c_int) {
    if !val.is_null() {
        xml_buffer_free(val);
    }
}

pub(crate) fn gen_unsigned_char_ptr(_no: c_int, _nr: c_int) -> *mut c_uchar {
    null_mut()
}

pub(crate) unsafe extern "C" fn gen_int_ptr(no: c_int, nr: c_int) -> *mut c_int {
    if no == 0 {
        addr_of_mut!(INTTAB[nr as usize])
    } else {
        null_mut()
    }
}

pub(crate) fn gen_const_unsigned_char_ptr(_no: c_int, _nr: c_int) -> *mut c_uchar {
    null_mut()
}

pub(crate) fn desret_int(_val: c_int) {}

pub(crate) fn des_unsigned_char_ptr(_no: c_int, _val: *mut c_uchar, _nr: c_int) {}

pub(crate) fn des_int_ptr(_no: c_int, _val: *mut c_int, _nr: c_int) {}

pub(crate) fn des_const_unsigned_char_ptr(_no: c_int, _val: *const c_uchar, _nr: c_int) {}

#[cfg(feature = "html")]
pub(crate) fn gen_const_html_node_ptr(_no: c_int, _nr: c_int) -> HtmlNodePtr {
    null_mut()
}

#[cfg(feature = "html")]
pub(crate) fn des_const_html_node_ptr(_no: c_int, _val: HtmlNodePtr, _nr: c_int) {}

#[cfg(feature = "html")]
pub(crate) fn gen_const_html_saxhandler_ptr(_no: c_int, _nr: c_int) -> *mut XmlSAXHandlerPtr {
    null_mut()
}

#[cfg(feature = "html")]
pub(crate) fn des_const_html_saxhandler_ptr(_no: c_int, _val: *mut XmlSAXHandlerPtr, _nr: c_int) {}

#[cfg(feature = "html")]
pub(crate) unsafe extern "C" fn desret_html_parser_ctxt_ptr(val: HtmlParserCtxtPtr) {
    if !val.is_null() {
        html_free_parser_ctxt(val);
    }
}

#[cfg(feature = "html")]
pub(crate) unsafe extern "C" fn gen_html_doc_ptr(no: c_int, _nr: c_int) -> HtmlDocPtr {
    use crate::libxml::{htmlparser::html_read_memory, htmltree::html_new_doc};

    if no == 0 {
        return html_new_doc(null_mut(), null_mut());
    }
    if no == 1 {
        return html_read_memory(
            c"<html/>".as_ptr() as _,
            7,
            c"test".as_ptr() as _,
            null_mut(),
            0,
        );
    }
    null_mut()
}

#[cfg(feature = "html")]
pub(crate) unsafe extern "C" fn desret_html_doc_ptr(val: HtmlDocPtr) {
    if !val.is_null()
        && val != API_DOC.load(Ordering::Relaxed)
        && (*val).doc != API_DOC.load(Ordering::Relaxed)
    {
        xml_free_doc(val);
    }
}

#[cfg(feature = "html")]
pub(crate) unsafe extern "C" fn gen_html_node_ptr(_no: c_int, _nr: c_int) -> HtmlNodePtr {
    null_mut()
}
#[cfg(feature = "html")]
pub(crate) unsafe extern "C" fn des_html_node_ptr(_no: c_int, _val: HtmlNodePtr, _nr: c_int) {}

#[cfg(feature = "html")]
pub(crate) unsafe extern "C" fn des_html_doc_ptr(_no: c_int, val: HtmlDocPtr, _nr: c_int) {
    if !val.is_null()
        && val != API_DOC.load(Ordering::Relaxed)
        && (*val).doc != API_DOC.load(Ordering::Relaxed)
    {
        xml_free_doc(val);
    }
}

#[cfg(feature = "html")]
pub(crate) fn gen_const_html_elem_desc_ptr(_no: i32, _nr: i32) -> *const HtmlElemDesc {
    null()
}

#[cfg(feature = "html")]
pub(crate) fn des_const_html_elem_desc_ptr(_no: i32, _val: *const HtmlElemDesc, _nr: i32) {}

#[cfg(feature = "html")]
pub(crate) fn desret_html_status(_val: HtmlStatus) {}

#[cfg(feature = "html")]
pub(crate) fn gen_const_xml_char_ptr_ptr(_no: c_int, _nr: c_int) -> *mut *mut XmlChar {
    null_mut()
}

#[cfg(feature = "html")]
pub(crate) fn gen_html_saxhandler_ptr(_no: c_int, _nr: c_int) -> HtmlSaxhandlerPtr {
    null_mut()
}
#[cfg(feature = "html")]
pub(crate) fn des_html_saxhandler_ptr(_no: c_int, _val: HtmlSaxhandlerPtr, _nr: c_int) {}

#[cfg(feature = "html")]
pub(crate) unsafe extern "C" fn gen_html_parser_ctxt_ptr(
    no: c_int,
    _nr: c_int,
) -> HtmlParserCtxtPtr {
    use crate::libxml::htmlparser::html_create_memory_parser_ctxt;

    if no == 0 {
        return xml_new_parser_ctxt();
    }
    if no == 1 {
        return html_create_memory_parser_ctxt(c"<html/>".as_ptr(), 7);
    }
    null_mut()
}

#[cfg(feature = "html")]
pub(crate) unsafe extern "C" fn des_html_parser_ctxt_ptr(
    _no: c_int,
    val: HtmlParserCtxtPtr,
    _nr: c_int,
) {
    if !val.is_null() {
        html_free_parser_ctxt(val);
    }
}

pub(crate) fn desret_const_html_entity_desc_ptr(_val: *const HtmlEntityDesc) {}

pub(crate) fn gen_const_xml_char_ptr(no: c_int, _nr: c_int) -> *const XmlChar {
    if no == 0 {
        return c"foo".as_ptr() as _;
    }
    if no == 1 {
        return c"<foo/>".as_ptr() as _;
    }
    if no == 2 {
        static D: [u8; 5] = [b'n', 0xf8, b'n', b'e', 0];
        return D.as_ptr() as _;
    }
    if no == 3 {
        return c" 2ab ".as_ptr() as _;
    }
    null_mut()
}

pub(crate) fn des_const_xml_char_ptr(_no: c_int, _val: *const XmlChar, _nr: c_int) {}

pub(crate) fn des_userdata(_no: c_int, _val: *mut c_void, _nr: c_int) {}

static mut CALL_TESTS: c_int = 0;

pub(crate) unsafe extern "C" fn gen_userdata(no: c_int, _nr: c_int) -> *mut c_void {
    if no == 0 {
        return addr_of_mut!(CALL_TESTS) as _;
    }
    if no == 1 {
        return -1 as _;
    }
    null_mut()
}

pub(crate) fn gen_int(no: c_int, _nr: c_int) -> c_int {
    if no == 0 {
        return 0;
    }
    if no == 1 {
        return 1;
    }
    if no == 2 {
        return -1;
    }
    if no == 3 {
        return 122;
    }
    -1
}

pub(crate) fn des_int(_no: c_int, _val: c_int, _nr: c_int) {}

pub(crate) fn gen_parseroptions(no: c_int, _nr: c_int) -> c_int {
    if no == 0 {
        return XmlParserOption::XmlParseNoblanks as i32 | XmlParserOption::XmlParseRecover as i32;
    }
    if no == 1 {
        return XmlParserOption::XmlParseNoent as i32
            | XmlParserOption::XmlParseDtdload as i32
            | XmlParserOption::XmlParseDtdattr as i32
            | XmlParserOption::XmlParseDtdvalid as i32
            | XmlParserOption::XmlParseNocdata as i32;
    }
    if no == 2 {
        return XmlParserOption::XmlParseXinclude as i32
            | XmlParserOption::XmlParseNoxincnode as i32
            | XmlParserOption::XmlParseNsclean as i32;
    }
    if no == 3 {
        return XmlParserOption::XmlParseXinclude as i32 | XmlParserOption::XmlParseNodict as i32;
    }
    XmlParserOption::XmlParseSax1 as i32
}

pub(crate) fn des_parseroptions(_no: c_int, _val: c_int, _nr: c_int) {}

pub(crate) fn gen_void_ptr(_no: c_int, _nr: c_int) -> *mut c_void {
    null_mut()
}
pub(crate) fn des_void_ptr(_no: c_int, _val: *mut c_void, _nr: c_int) {}

#[cfg(feature = "output")]
pub(crate) unsafe extern "C" fn gen_xml_output_buffer_ptr(
    no: c_int,
    _nr: c_int,
) -> XmlOutputBufferPtr {
    use crate::libxml::xml_io::xmlOutputBufferCreateFilename;

    if no == 0 {
        return xmlOutputBufferCreateFilename(c"test.out".as_ptr() as _, null_mut(), 0);
    }
    null_mut()
}

#[cfg(feature = "output")]
pub(crate) unsafe extern "C" fn des_xml_output_buffer_ptr(
    _no: c_int,
    val: XmlOutputBufferPtr,
    _nr: c_int,
) {
    use crate::libxml::xml_io::xmlOutputBufferClose;

    if !val.is_null() {
        xmlOutputBufferClose(val);
    }
}

pub(crate) unsafe extern "C" fn gen_xml_doc_ptr(no: c_int, _nr: c_int) -> XmlDocPtr {
    if no == 0 {
        return xml_new_doc(c"1.0".as_ptr() as _);
    }
    if no == 1 {
        return xml_read_memory(
            c"<foo/>".as_ptr() as _,
            6,
            c"test".as_ptr() as _,
            null_mut(),
            0,
        );
    }
    if no == 2 {
        return xml_read_memory(
            c"<!DOCTYPE foo []> <foo/>".as_ptr() as _,
            24,
            c"test".as_ptr() as _,
            null_mut(),
            0,
        );
    }
    null_mut()
}

pub(crate) unsafe extern "C" fn des_xml_doc_ptr(_no: c_int, val: XmlDocPtr, _nr: c_int) {
    if !val.is_null()
        && val != API_DOC.load(Ordering::Relaxed)
        && (*val).doc != API_DOC.load(Ordering::Relaxed)
    {
        xml_free_doc(val);
    }
}

pub(crate) fn gen_const_char_ptr(no: c_int, _nr: c_int) -> *mut c_char {
    if no == 0 {
        return c"foo".as_ptr() as _;
    }
    if no == 1 {
        return c"<foo/>".as_ptr() as _;
    }
    if no == 2 {
        return c"test/ent2".as_ptr() as _;
    }
    null_mut()
}

pub(crate) fn des_const_char_ptr(_no: c_int, _val: *const c_char, _nr: c_int) {}

/*
 We need some "remote" addresses, but want to avoid getting into
 name resolution delays, so we use these
*/
const REMOTE1GOOD: &CStr = c"http://localhost/";
const REMOTE1BAD: &CStr = c"http:http://http";
const REMOTE2GOOD: &CStr = c"ftp://localhost/foo";

pub(crate) fn gen_fileoutput(no: c_int, _nr: c_int) -> *const c_char {
    if no == 0 {
        return c"missing/dir/missing.xml".as_ptr() as _;
    }
    if no == 1 {
        return c"<foo/>".as_ptr() as _;
    }
    if no == 2 {
        return REMOTE2GOOD.as_ptr();
    }
    if no == 3 {
        return REMOTE1GOOD.as_ptr();
    }
    if no == 4 {
        return REMOTE1BAD.as_ptr();
    }
    null()
}

pub(crate) fn des_fileoutput(_no: c_int, _val: *const c_char, _nr: c_int) {}

pub(crate) fn gen_filepath(no: c_int, _nr: c_int) -> *const c_char {
    if no == 0 {
        return c"missing.xml".as_ptr() as _;
    }
    if no == 1 {
        return c"<foo/>".as_ptr() as _;
    }
    if no == 2 {
        return c"test/ent2".as_ptr() as _;
    }
    if no == 3 {
        return c"test/valid/REC-xml-19980210.xml".as_ptr() as _;
    }
    if no == 4 {
        return c"test/valid/dtds/xhtml1-strict.dtd".as_ptr() as _;
    }
    if no == 5 {
        return REMOTE1GOOD.as_ptr();
    }
    if no == 6 {
        return REMOTE1BAD.as_ptr();
    }
    null_mut()
}
pub(crate) fn des_filepath(_no: c_int, _val: *const c_char, _nr: c_int) {}

pub(crate) fn gen_xml_char_encoding(no: c_int, _nr: c_int) -> XmlCharEncoding {
    if no == 0 {
        return XmlCharEncoding::XmlCharEncodingUtf8;
    }
    if no == 1 {
        return XmlCharEncoding::XmlCharEncodingNone;
    }
    if no == 2 {
        return XmlCharEncoding::XmlCharEncoding8859_1;
    }
    XmlCharEncoding::XmlCharEncodingError
}

pub(crate) fn des_xml_char_encoding(_no: c_int, _val: XmlCharEncoding, _nr: c_int) {}

pub(crate) fn desret_const_xml_char_ptr(_val: *const XmlChar) {}

pub(crate) fn gen_unsigned_int(no: c_int, _nr: c_int) -> c_uint {
    if no == 0 {
        return 0;
    }
    if no == 1 {
        return 1;
    }
    if no == 2 {
        return 122;
    }
    u32::MAX
}

pub(crate) fn des_unsigned_int(_no: c_int, _val: c_uint, _nr: c_int) {}

pub(crate) fn des_const_xml_char_ptr_ptr(_no: c_int, _val: *mut *const XmlChar, _nr: c_int) {}

pub(crate) unsafe extern "C" fn gen_file_ptr(no: c_int, _nr: c_int) -> *mut FILE {
    if no == 0 {
        return fopen(c"test.out".as_ptr() as _, c"a+".as_ptr() as _);
    }
    null_mut()
}
pub(crate) unsafe extern "C" fn des_file_ptr(_no: c_int, val: *mut FILE, _nr: c_int) {
    if !val.is_null() {
        fclose(val);
    }
}

pub(crate) fn gen_xml_char_ptr_ptr(_no: c_int, _nr: c_int) -> *mut *mut XmlChar {
    null_mut()
}
pub(crate) fn des_xml_char_ptr_ptr(_no: c_int, _val: *mut *mut XmlChar, _nr: c_int) {}
