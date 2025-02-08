use std::{
    cell::{Cell, RefCell},
    ffi::{c_char, CStr},
    fs::File,
    os::raw::c_void,
    ptr::{addr_of_mut, null, null_mut},
    sync::Mutex,
};

use libc::snprintf;

#[cfg(feature = "catalog")]
use crate::libxml::catalog::{XmlCatalogAllow, XmlCatalogPrefer, XmlCatalogPtr};
#[cfg(feature = "schema")]
use crate::relaxng::{XmlRelaxNGParserCtxtPtr, XmlRelaxNGValidCtxtPtr};
use crate::{
    buf::XmlBuf,
    io::XmlOutputBuffer,
    libxml::{
        debug_xml::XmlShellCtxtPtr,
        globals::xml_free,
        htmlparser::{
            html_free_parser_ctxt, HtmlDocPtr, HtmlElemDesc, HtmlEntityDesc, HtmlNodePtr,
            HtmlParserCtxtPtr, HtmlStatus,
        },
        parser::{XmlFeature, XmlParserOption, XmlSAXHandlerPtr},
        pattern::{XmlPatternPtr, XmlStreamCtxtPtr},
        relaxng::XmlRelaxNGPtr,
        schemas_internals::{XmlSchemaFacetPtr, XmlSchemaTypePtr, XmlSchemaValType},
        schematron::XmlSchematronValidCtxtPtr,
        uri::XmlURIPtr,
        valid::{
            xml_free_element_content, xml_free_valid_ctxt, xml_new_valid_ctxt, XmlValidCtxtPtr,
        },
        xinclude::XmlXincludeCtxtPtr,
        xmlautomata::{XmlAutomataPtr, XmlAutomataStatePtr},
        xmlmodule::XmlModulePtr,
        xmlreader::{XmlTextReaderLocatorPtr, XmlTextReaderPtr},
        xmlregexp::{XmlExpCtxtPtr, XmlExpNodePtr, XmlRegExecCtxtPtr, XmlRegexpPtr},
        xmlschemas::{
            XmlSchemaParserCtxtPtr, XmlSchemaPtr, XmlSchemaSAXPlugPtr, XmlSchemaValidCtxtPtr,
        },
        xmlschemastypes::{XmlSchemaValPtr, XmlSchemaWhitespaceValueType},
        xmlstring::{xml_strdup, XmlChar},
    },
    parser::{
        xml_create_memory_parser_ctxt, xml_free_input_stream, xml_free_parser_ctxt,
        xml_new_parser_ctxt, xml_read_memory, XmlParserCtxt, XmlParserCtxtPtr, XmlParserInputPtr,
        XmlParserNodeInfo, XmlParserNodeInfoSeq, XmlParserNodeInfoSeqPtr,
    },
    tree::{
        xml_free_doc, xml_free_node, xml_new_doc, xml_new_dtd, xml_new_pi, NodeCommon, XmlAttr,
        XmlAttribute, XmlAttributeDefault, XmlAttributeType, XmlDOMWrapCtxtPtr, XmlDoc, XmlDocPtr,
        XmlDtd, XmlElement, XmlElementContentPtr, XmlElementContentType, XmlElementType,
        XmlElementTypeVal, XmlEntitiesTablePtr, XmlEntity, XmlNode, XmlNs,
    },
    xpath::{
        XmlNodeSet, XmlXPathCompExprPtr, XmlXPathContextPtr, XmlXPathObjectPtr,
        XmlXPathParserContextPtr,
    },
};

pub(crate) static TEST_CATALOG_LOCK: Mutex<()> = Mutex::new(());

thread_local! {
    static CHARTAB: RefCell<[XmlChar; 1024]> = const { RefCell::new([0; 1024]) };
    static LONGTAB: RefCell<[u64; 1024]> = const { RefCell::new([0; 1024]) };
    static INTTAB: RefCell<[i32; 1024]> = const { RefCell::new([0; 1024]) };

    static API_DOC: Cell<*mut XmlDoc> = const { Cell::new(null_mut()) };
    static API_DTD: Cell<*mut XmlDtd> = const { Cell::new(null_mut()) };
    static API_ROOT: Cell<*mut XmlNode> = const { Cell::new(null_mut()) };
    static API_ATTR: Cell<*mut XmlAttr> = const { Cell::new(null_mut()) };
    static API_NS: Cell<*mut XmlNs> = const { Cell::new(null_mut()) };
}

pub(crate) const GEN_NB_HTML_DOC_PTR: i32 = 3;
pub(crate) const GEN_NB_UNSIGNED_CHAR_PTR: i32 = 1;
pub(crate) const GEN_NB_INT_PTR: i32 = 2;
pub(crate) const GEN_NB_CONST_UNSIGNED_CHAR_PTR: i32 = 1;
pub(crate) const GEN_NB_HTML_PARSER_CTXT_PTR: i32 = 3;
pub(crate) const GEN_NB_FILEPATH: i32 = 8;
pub(crate) const GEN_NB_FILEOUTPUT: i32 = 6;
pub(crate) const GEN_NB_CONST_CHAR_PTR: i32 = 4;
pub(crate) const GEN_NB_CONST_HTML_ELEM_DESC_PTR: i32 = 1;
pub(crate) const GEN_NB_CONST_XML_CHAR_PTR: i32 = 5;
pub(crate) const GEN_NB_INT: i32 = 4;
pub(crate) const GEN_NB_USERDATA: i32 = 3;
pub(crate) const GEN_NB_UNSIGNED_INT: i32 = 3;
#[cfg(feature = "html")]
pub(crate) const GEN_NB_HTML_NODE_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_OUTPUT_BUFFER_PTR: i32 = 2;
#[cfg(feature = "html")]
pub(crate) const GEN_NB_CONST_HTML_NODE_PTR: i32 = 1;
pub(crate) const GEN_NB_CONST_XML_CHAR_PTR_PTR: i32 = 1;
pub(crate) const GEN_NB_FILE_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_CHAR_PTR_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_BUFFER_PTR: i32 = 3;
pub(crate) const GEN_NB_XML_NODE_PTR: i32 = 3;
pub(crate) const GEN_NB_VOID_PTR: i32 = 2;
pub(crate) const GEN_NB_XML_ENUMERATION_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_CHAR_PTR: i32 = 2;
pub(crate) const GEN_NB_XML_DOC_PTR: i32 = 4;
#[cfg(feature = "xpath")]
pub(crate) const GEN_NB_XML_NODE_SET_PTR: i32 = 1;
#[cfg(feature = "catalog")]
pub(crate) const GEN_NB_XML_CATALOG_PTR: i32 = 1;
#[cfg(feature = "catalog")]
pub(crate) const GEN_NB_XML_CATALOG_ALLOW: i32 = 4;
#[cfg(feature = "catalog")]
pub(crate) const GEN_NB_XML_CATALOG_PREFER: i32 = 3;
pub(crate) const GEN_NB_CONST_XML_DOC_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_ENTITY_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_ENTITIES_TABLE_PTR: i32 = 1;
pub(crate) const GEN_NB_CHAR_PTR_PTR: i32 = 1;
#[cfg(feature = "http")]
pub(crate) const GEN_NB_XML_NANO_HTTPCTXT_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_NODE_PTR_PTR: i32 = 1;
pub(crate) const GEN_NB_CONST_XML_PARSER_NODE_INFO_PTR: i32 = 1;
pub(crate) const GEN_NB_CONST_XML_PARSER_CTXT_PTR: i32 = 1;
pub(crate) const GEN_NB_CONST_XML_NODE_PTR: i32 = 1;
pub(crate) const GEN_NB_CONST_XML_PARSER_NODE_INFO_SEQ_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_PARSER_INPUT_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_PARSER_CTXT_PTR: i32 = 3;
pub(crate) const GEN_NB_XML_PARSER_NODE_INFO_SEQ_PTR: i32 = 1;
#[cfg(any(feature = "sax1", feature = "libxml_valid", feature = "libxml_push"))]
pub(crate) const GEN_NB_XML_SAXHANDLER_PTR: i32 = 2;
pub(crate) const GEN_NB_PARSEROPTIONS: i32 = 5;
pub(crate) const GEN_NB_XML_FEATURE: i32 = 4;
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
pub(crate) const GEN_NB_XML_RELAXNG_VALID_CTXT_PTR: i32 = 1;
#[cfg(any(
    feature = "libxml_modules",
    feature = "libxml_reader",
    feature = "schema"
))]
pub(crate) const GEN_NB_VOID_PTR_PTR: i32 = 1;
#[cfg(feature = "schematron")]
pub(crate) const GEN_NB_XML_SCHEMATRON_VALID_CTXT_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_DOMWRAP_CTXT_PTR: i32 = 1;
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
#[cfg(feature = "libxml_modules")]
pub(crate) const GEN_NB_XML_MODULE_PTR: i32 = 1;
#[cfg(feature = "libxml_reader")]
pub(crate) const GEN_NB_XML_TEXT_READER_LOCATOR_PTR: i32 = 1;
#[cfg(feature = "libxml_reader")]
pub(crate) const GEN_NB_XML_TEXT_READER_PTR: i32 = 4;
#[cfg(feature = "schema")]
pub(crate) const GEN_NB_XML_SCHEMA_PTR: i32 = 1;
#[cfg(feature = "schema")]
pub(crate) const GEN_NB_XML_SCHEMA_VALID_CTXT_PTR: i32 = 1;
#[cfg(feature = "libxml_regexp")]
pub(crate) const GEN_NB_XML_REGEXP_PTR: i32 = 1;
#[cfg(feature = "libxml_regexp")]
pub(crate) const GEN_NB_XML_REG_EXEC_CTXT_PTR: i32 = 1;
#[cfg(all(feature = "libxml_regexp", feature = "libxml_expr"))]
pub(crate) const GEN_NB_XML_EXP_CTXT_PTR: i32 = 1;
#[cfg(all(feature = "libxml_regexp", feature = "libxml_expr"))]
pub(crate) const GEN_NB_XML_EXP_NODE_PTR: i32 = 1;
#[cfg(feature = "libxml_output")]
pub(crate) const GEN_NB_XML_SAVE_CTXT_PTR: i32 = 1;
#[cfg(feature = "schema")]
pub(crate) const GEN_NB_XML_SCHEMA_SAXPLUG_PTR: i32 = 1;
#[cfg(feature = "schema")]
pub(crate) const GEN_NB_XML_SCHEMA_PARSER_CTXT_PTR: i32 = 1;
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
#[cfg(feature = "libxml_writer")]
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
pub(crate) fn gen_xml_shell_ctxt_ptr(_no: i32, _nr: i32) -> XmlShellCtxtPtr<'static> {
    null_mut()
}

#[cfg(feature = "xpath")]
pub(crate) fn des_xml_shell_ctxt_ptr(_no: i32, _val: XmlShellCtxtPtr, _nr: i32) {}

#[cfg(feature = "libxml_debug")]
pub(crate) fn gen_debug_file_ptr(_no: i32, _nr: i32) -> Option<File> {
    return File::options()
        .append(true)
        .read(true)
        .create(true)
        .open("test.out")
        .ok();
}

#[cfg(feature = "libxml_debug")]
pub(crate) unsafe fn des_debug_file_ptr(_no: i32, _val: File, _nr: i32) {}

#[cfg(feature = "xpath")]
pub(crate) fn gen_xml_xpath_parser_context_ptr(_no: i32, _nr: i32) -> XmlXPathParserContextPtr {
    null_mut()
}

#[cfg(feature = "xpath")]
pub(crate) fn des_xml_xpath_parser_context_ptr(_no: i32, _val: XmlXPathParserContextPtr, _nr: i32) {
}

#[cfg(feature = "xpath")]
pub(crate) unsafe fn desret_xml_xpath_object_ptr(val: XmlXPathObjectPtr) {
    use crate::xpath::xml_xpath_free_object;

    xml_xpath_free_object(val);
}

#[cfg(feature = "xpath")]
pub(crate) unsafe fn desret_xml_node_set_ptr(val: Option<Box<crate::xpath::XmlNodeSet>>) {
    use crate::xpath::xml_xpath_free_node_set;

    xml_xpath_free_node_set(val);
}

#[cfg(any(feature = "xpath", feature = "schema"))]
pub(crate) unsafe fn gen_double(no: i32, _nr: i32) -> f64 {
    if no == 0 {
        return 0.0;
    }
    if no == 1 {
        return -1.1;
    }
    #[cfg(feature = "xpath")]
    if no == 2 {
        use crate::xpath::XML_XPATH_NAN;
        return XML_XPATH_NAN;
    }
    -1.0
}

#[cfg(any(feature = "xpath", feature = "schema"))]
pub(crate) fn des_double(_no: i32, _val: f64, _nr: i32) {}

#[cfg(feature = "xpath")]
pub(crate) unsafe fn gen_xml_xpath_object_ptr(no: i32, _nr: i32) -> XmlXPathObjectPtr {
    use crate::xpath::object::{
        xml_xpath_new_boolean, xml_xpath_new_float, xml_xpath_new_node_set, xml_xpath_new_string,
    };

    if no == 0 {
        return xml_xpath_new_string(Some("string object"));
    }
    if no == 1 {
        return xml_xpath_new_float(1.1);
    }
    if no == 2 {
        return xml_xpath_new_boolean(true);
    }
    if no == 3 {
        return xml_xpath_new_node_set(null_mut());
    }
    null_mut()
}

#[cfg(feature = "xpath")]
pub(crate) unsafe fn des_xml_xpath_object_ptr(_no: i32, val: XmlXPathObjectPtr, _nr: i32) {
    use crate::xpath::xml_xpath_free_object;

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

#[cfg(feature = "schema")]
pub(crate) fn gen_unsigned_long(no: i32, _nr: i32) -> u64 {
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
pub(crate) fn des_unsigned_long(_no: i32, _val: u64, _nr: i32) {}

#[cfg(feature = "schema")]
pub(crate) unsafe fn gen_unsigned_long_ptr(no: i32, nr: i32) -> *mut u64 {
    LONGTAB.with_borrow_mut(|table| {
        if no == 0 {
            return addr_of_mut!(table[nr as usize]);
        }
        null_mut()
    })
}

#[cfg(feature = "schema")]
pub(crate) fn des_unsigned_long_ptr(_no: i32, _val: *mut u64, _nr: i32) {}

#[cfg(feature = "schema")]
pub(crate) fn gen_xml_schema_val_type(no: i32, _nr: i32) -> XmlSchemaValType {
    if no == 1 {
        return XmlSchemaValType::XmlSchemasAnysimpletype;
    }
    if no == 2 {
        return XmlSchemaValType::XmlSchemasAnytype;
    }
    if no == 3 {
        return XmlSchemaValType::XmlSchemasAnyURI;
    }
    if no == 4 {
        return XmlSchemaValType::XmlSchemasBase64binary;
    }
    XmlSchemaValType::XmlSchemasUnknown
}

#[cfg(feature = "schema")]
pub(crate) fn des_xml_schema_val_type(_no: i32, _val: XmlSchemaValType, _nr: i32) {}

#[cfg(feature = "schema")]
pub(crate) fn desret_xml_schema_val_type(_val: XmlSchemaValType) {}

#[cfg(feature = "schema")]
pub(crate) fn gen_xml_schema_whitespace_value_type(
    no: i32,
    _nr: i32,
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
    _no: i32,
    _val: XmlSchemaWhitespaceValueType,
    _nr: i32,
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
pub(crate) fn gen_xml_schema_saxplug_ptr(_no: i32, _nr: i32) -> XmlSchemaSAXPlugPtr {
    null_mut()
}

#[cfg(feature = "schema")]
pub(crate) fn des_xml_schema_saxplug_ptr(_no: i32, _val: XmlSchemaSAXPlugPtr, _nr: i32) {}

#[cfg(all(feature = "libxml_regexp", feature = "libxml_expr"))]
pub(crate) fn gen_xml_exp_ctxt_ptr(_no: i32, _nr: i32) -> XmlExpCtxtPtr {
    null_mut()
}

#[cfg(all(feature = "libxml_regexp", feature = "libxml_expr"))]
pub(crate) fn des_xml_exp_ctxt_ptr(_no: i32, _val: XmlExpCtxtPtr, _nr: i32) {}

#[cfg(all(feature = "libxml_regexp", feature = "libxml_expr"))]
pub(crate) fn gen_xml_exp_node_ptr(_no: i32, _nr: i32) -> XmlExpNodePtr {
    null_mut()
}
#[cfg(all(feature = "libxml_regexp", feature = "libxml_expr"))]
pub(crate) fn des_xml_exp_node_ptr(_no: i32, _val: XmlExpNodePtr, _nr: i32) {}

#[cfg(feature = "libxml_regexp")]
pub(crate) fn gen_xml_reg_exec_ctxt_ptr(_no: i32, _nr: i32) -> XmlRegExecCtxtPtr {
    null_mut()
}

#[cfg(feature = "libxml_regexp")]
pub(crate) fn des_xml_reg_exec_ctxt_ptr(_no: i32, _val: XmlRegExecCtxtPtr, _nr: i32) {}

#[cfg(feature = "libxml_regexp")]
pub(crate) fn gen_xml_regexp_ptr(_no: i32, _nr: i32) -> XmlRegexpPtr {
    null_mut()
}

#[cfg(feature = "libxml_regexp")]
pub(crate) fn des_xml_regexp_ptr(_no: i32, _val: XmlRegexpPtr, _nr: i32) {}

#[cfg(feature = "schema")]
pub(crate) fn gen_xml_schema_ptr(_no: i32, _nr: i32) -> XmlSchemaPtr {
    null_mut()
}

#[cfg(feature = "schema")]
pub(crate) fn des_xml_schema_ptr(_no: i32, _val: XmlSchemaPtr, _nr: i32) {}

#[cfg(feature = "schema")]
pub(crate) fn gen_xml_schema_valid_ctxt_ptr(_no: i32, _nr: i32) -> XmlSchemaValidCtxtPtr {
    null_mut()
}

#[cfg(feature = "schema")]
pub(crate) fn des_xml_schema_valid_ctxt_ptr(_no: i32, _val: XmlSchemaValidCtxtPtr, _nr: i32) {}

#[cfg(feature = "libxml_reader")]
pub(crate) unsafe fn gen_xml_text_reader_ptr(no: i32, _nr: i32) -> XmlTextReaderPtr {
    use crate::libxml::xmlreader::xml_new_text_reader_filename;

    if no == 0 {
        return xml_new_text_reader_filename("test/ent2");
    }
    if no == 1 {
        return xml_new_text_reader_filename("test/valid/REC-xml-19980210.xml");
    }
    if no == 2 {
        return xml_new_text_reader_filename("test/valid/dtds/xhtml1-strict.dtd");
    }
    null_mut()
}

#[cfg(feature = "libxml_reader")]
pub(crate) unsafe fn des_xml_text_reader_ptr(_no: i32, val: XmlTextReaderPtr, _nr: i32) {
    use crate::libxml::xmlreader::xml_free_text_reader;

    if !val.is_null() {
        xml_free_text_reader(val);
    }
}

#[cfg(feature = "libxml_reader")]
pub(crate) unsafe fn desret_xml_text_reader_ptr(val: XmlTextReaderPtr) {
    use crate::libxml::xmlreader::xml_free_text_reader;

    xml_free_text_reader(val);
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

#[cfg(feature = "xinclude")]
pub(crate) fn gen_xml_xinclude_ctxt_ptr(_no: i32, _nr: i32) -> XmlXincludeCtxtPtr {
    null_mut()
}

#[cfg(feature = "xinclude")]
pub(crate) fn des_xml_xinclude_ctxt_ptr(_no: i32, _val: XmlXincludeCtxtPtr, _nr: i32) {}

pub(crate) fn gen_xml_element_content_type(no: i32, _nr: i32) -> XmlElementContentType {
    if no == 1 {
        return XmlElementContentType::XmlElementContentElement;
    }
    if no == 2 {
        return XmlElementContentType::XmlElementContentOr;
    }
    if no == 3 {
        return XmlElementContentType::XmlElementContentPCDATA;
    }
    if no == 4 {
        return XmlElementContentType::XmlElementContentSeq;
    }
    XmlElementContentType::XmlElementContentPCDATA
}

pub(crate) fn des_xml_element_content_type(_no: i32, _val: XmlElementContentType, _nr: i32) {}

pub(crate) fn gen_xml_element_type_val(no: i32, _nr: i32) -> XmlElementTypeVal {
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

pub(crate) fn des_xml_element_type_val(_no: i32, _val: XmlElementTypeVal, _nr: i32) {}

pub(crate) fn gen_xml_attribute_default(no: i32, _nr: i32) -> XmlAttributeDefault {
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

pub(crate) fn des_xml_attribute_default(_no: i32, _val: XmlAttributeDefault, _nr: i32) {}

pub(crate) fn gen_xml_attribute_type(no: i32, _nr: i32) -> XmlAttributeType {
    if no == 1 {
        return XmlAttributeType::XmlAttributeCDATA;
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
    XmlAttributeType::XmlAttributeCDATA
}

pub(crate) fn des_xml_attribute_type(_no: i32, _val: XmlAttributeType, _nr: i32) {}

pub(crate) unsafe fn gen_xml_valid_ctxt_ptr(no: i32, _nr: i32) -> XmlValidCtxtPtr {
    #[cfg(feature = "libxml_valid")]
    if no == 0 {
        return xml_new_valid_ctxt();
    }
    null_mut()
}
pub(crate) unsafe fn des_xml_valid_ctxt_ptr(_no: i32, val: XmlValidCtxtPtr, _nr: i32) {
    #[cfg(feature = "libxml_valid")]
    if !val.is_null() {
        xml_free_valid_ctxt(val);
    }
}

pub(crate) fn gen_xml_attribute_ptr(_no: i32, _nr: i32) -> *mut XmlAttribute {
    null_mut()
}

pub(crate) fn des_xml_attribute_ptr(_no: i32, _val: *mut XmlAttribute, _nr: i32) {}

pub(crate) fn gen_xml_element_ptr(_no: i32, _nr: i32) -> *mut XmlElement {
    null_mut()
}

pub(crate) fn des_xml_element_ptr(_no: i32, _val: *mut XmlElement, _nr: i32) {}

pub(crate) fn gen_char_ptr(_no: i32, _nr: i32) -> *mut c_char {
    null_mut()
}

pub(crate) fn des_char_ptr(_no: i32, _val: *mut c_char, _nr: i32) {}

pub(crate) fn gen_xml_uriptr(_no: i32, _nr: i32) -> XmlURIPtr {
    null_mut()
}

pub(crate) fn des_xml_uriptr(_no: i32, _val: XmlURIPtr, _nr: i32) {}

pub(crate) unsafe fn gen_eaten_name(no: i32, _nr: i32) -> *mut XmlChar {
    if no == 0 {
        return xml_strdup(c"eaten".as_ptr() as _);
    }
    null_mut()
}
pub(crate) fn des_eaten_name(_no: i32, _val: *mut XmlChar, _nr: i32) {}

unsafe fn get_api_ns() -> *mut XmlNs {
    get_api_root();
    if !API_ROOT.get().is_null() {
        API_NS.set(
            (*API_ROOT.get())
                .ns_def
                .map_or(null_mut(), |ns| ns.as_ptr()),
        );
    }
    API_NS.get()
}

pub(crate) unsafe fn gen_xml_ns_ptr(no: i32, _nr: i32) -> *mut XmlNs {
    if no == 0 {
        return get_api_ns();
    }
    null_mut()
}
pub(crate) unsafe fn des_xml_ns_ptr(no: i32, _val: *mut XmlNs, _nr: i32) {
    if no == 0 {
        free_api_doc();
    }
}

unsafe fn get_api_dtd() -> *mut XmlDtd {
    if API_DTD.get().is_null() || (*API_DTD.get()).typ != XmlElementType::XmlDTDNode {
        get_api_doc();
        if !API_DOC.get().is_null()
            && (*API_DOC.get()).children.is_some()
            && (*API_DOC.get()).children.unwrap().element_type() == XmlElementType::XmlDTDNode
        {
            API_DTD.set(
                (*(*API_DOC.get()).children().unwrap().as_ptr())
                    .as_dtd_node()
                    .unwrap()
                    .as_ptr(),
            );
        }
    }
    API_DTD.get()
}

pub(crate) unsafe fn gen_xml_dtd_ptr(no: i32, _nr: i32) -> *mut XmlDtd {
    if no == 0 {
        return xml_new_dtd(None, Some("dtd"), Some("foo"), Some("bar"))
            .map_or(null_mut(), |dtd| dtd.as_ptr());
    }
    if no == 1 {
        return get_api_dtd();
    }
    null_mut()
}
pub(crate) unsafe fn des_xml_dtd_ptr(no: i32, val: *mut XmlDtd, _nr: i32) {
    if no == 1 {
        free_api_doc();
    } else if !val.is_null() {
        (*val).unlink();
        xml_free_node(val as *mut XmlNode);
    }
}

unsafe fn get_api_attr() -> *mut XmlAttr {
    #[cfg(any(
        feature = "libxml_tree",
        feature = "xinclude",
        feature = "schema",
        feature = "html"
    ))]
    static mut NR: i32 = 0;
    #[cfg(any(
        feature = "libxml_tree",
        feature = "xinclude",
        feature = "schema",
        feature = "html"
    ))]
    let mut name: [XmlChar; 20] = [0; 20];

    if API_ROOT.get().is_null()
        || (*API_ROOT.get()).element_type() != XmlElementType::XmlElementNode
    {
        get_api_root();
    }
    if API_ROOT.get().is_null() {
        return null_mut();
    }
    if let Some(prop) = (*API_ROOT.get()).properties {
        API_ATTR.set(prop.as_ptr());
        return prop.as_ptr();
    }
    API_ATTR.set(null_mut());
    #[cfg(any(
        feature = "libxml_tree",
        feature = "xinclude",
        feature = "schema",
        feature = "html"
    ))]
    {
        snprintf(name.as_mut_ptr() as _, 20, c"foo%d".as_ptr() as _, NR);
        NR += 1;
        let name = CStr::from_ptr(name.as_ptr() as *const i8).to_string_lossy();
        API_ATTR.set(
            (*API_ROOT.get())
                .set_prop(name.as_ref(), Some("bar"))
                .map_or(null_mut(), |prop| prop.as_ptr()),
        );
    }
    API_ATTR.get()
}

pub(crate) unsafe fn gen_xml_attr_ptr(no: i32, _nr: i32) -> *mut XmlAttr {
    if no == 0 {
        return get_api_attr();
    }
    null_mut()
}
pub(crate) unsafe fn des_xml_attr_ptr(no: i32, _val: *mut XmlAttr, _nr: i32) {
    if no == 0 {
        free_api_doc();
    }
}

pub(crate) fn gen_const_xml_buf_ptr(_no: i32, _nr: i32) -> *const XmlBuf {
    null()
}

pub(crate) fn des_const_xml_buf_ptr(_no: i32, _val: *const XmlBuf, _nr: i32) {}

pub(crate) fn gen_xml_domwrap_ctxt_ptr(_no: i32, _nr: i32) -> XmlDOMWrapCtxtPtr {
    null_mut()
}

pub(crate) fn des_xml_domwrap_ctxt_ptr(_no: i32, _val: XmlDOMWrapCtxtPtr, _nr: i32) {}

#[cfg(feature = "schematron")]
pub(crate) fn gen_xml_schematron_valid_ctxt_ptr(_no: i32, _nr: i32) -> XmlSchematronValidCtxtPtr {
    null_mut()
}

#[cfg(feature = "schematron")]
pub(crate) fn des_xml_schematron_valid_ctxt_ptr(
    _no: i32,
    _val: XmlSchematronValidCtxtPtr,
    _nr: i32,
) {
}

#[cfg(feature = "schema")]
pub(crate) unsafe fn desret_xml_schema_parser_ctxt_ptr(val: XmlSchemaParserCtxtPtr) {
    use crate::libxml::xmlschemas::xml_schema_free_parser_ctxt;

    xml_schema_free_parser_ctxt(val);
}

#[cfg(feature = "schema")]
pub(crate) fn desret_xml_schema_type_ptr(_val: XmlSchemaTypePtr) {}

#[cfg(feature = "schema")]
pub(crate) unsafe fn desret_xml_relaxng_parser_ctxt_ptr(val: XmlRelaxNGParserCtxtPtr) {
    use crate::relaxng::xml_relaxng_free_parser_ctxt;

    xml_relaxng_free_parser_ctxt(val);
}

#[cfg(any(
    feature = "libxml_modules",
    feature = "libxml_reader",
    feature = "schema"
))]
pub(crate) fn gen_void_ptr_ptr(_no: i32, _nr: i32) -> *mut *mut c_void {
    null_mut()
}

#[cfg(any(
    feature = "libxml_modules",
    feature = "libxml_reader",
    feature = "schema"
))]
pub(crate) fn des_void_ptr_ptr(_no: i32, _val: *mut *mut c_void, _nr: i32) {}

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
pub(crate) fn gen_xml_relaxng_ptr(_no: i32, _nr: i32) -> XmlRelaxNGPtr {
    null_mut()
}

#[cfg(feature = "schema")]
pub(crate) fn des_xml_relaxng_ptr(_no: i32, _val: XmlRelaxNGPtr, _nr: i32) {}

#[cfg(feature = "libxml_pattern")]
pub(crate) fn gen_xml_pattern_ptr(_no: i32, _nr: i32) -> XmlPatternPtr {
    null_mut()
}
#[cfg(feature = "libxml_pattern")]
pub(crate) fn des_xml_pattern_ptr(_no: i32, _val: XmlPatternPtr, _nr: i32) {}

#[cfg(feature = "libxml_pattern")]
pub(crate) fn gen_xml_stream_ctxt_ptr(_no: i32, _nr: i32) -> XmlStreamCtxtPtr {
    null_mut()
}

#[cfg(feature = "libxml_pattern")]
pub(crate) fn des_xml_stream_ctxt_ptr(_no: i32, _val: XmlStreamCtxtPtr, _nr: i32) {}

pub(crate) fn gen_xml_char(no: i32, _nr: i32) -> XmlChar {
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

pub(crate) fn des_xml_char(_no: i32, _val: XmlChar, _nr: i32) {}

pub(crate) unsafe fn desret_xml_node_ptr(val: *mut XmlNode) {
    if !val.is_null() && val != API_ROOT.get() && val != API_DOC.get() as *mut XmlNode {
        (*val).unlink();
        xml_free_node(val);
    }
}
pub(crate) unsafe fn desret_xml_attr_ptr(val: *mut XmlAttr) {
    if !val.is_null() {
        (*val).unlink();
        xml_free_node(val as *mut XmlNode);
    }
}

pub(crate) unsafe fn desret_xml_element_ptr(val: *mut XmlElement) {
    if !val.is_null() {
        (*val).unlink();
    }
}

pub(crate) unsafe fn desret_xml_attribute_ptr(val: *mut XmlAttribute) {
    if !val.is_null() {
        (*val).unlink();
    }
}

pub(crate) fn desret_xml_ns_ptr(_val: *mut XmlNs) {}

pub(crate) unsafe fn desret_xml_dtd_ptr(val: *mut XmlDtd) {
    desret_xml_node_ptr(val as *mut XmlNode);
}

pub(crate) fn gen_xml_feature(no: i32, _nr: i32) -> Option<XmlFeature> {
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

pub(crate) fn des_xml_feature(_no: i32, _val: Option<XmlFeature>, _nr: i32) {}

#[cfg(any(feature = "sax1", feature = "libxml_valid", feature = "libxml_push"))]
pub(crate) unsafe fn gen_xml_saxhandler_ptr(_no: i32, _nr: i32) -> XmlSAXHandlerPtr {
    // #[cfg(feature = "sax1")]
    // if no == 0 {
    //     use crate::libxml::globals::xml_default_sax_handler;
    //     return xml_default_sax_handler() as _;
    // }
    null_mut()
}

#[cfg(any(feature = "sax1", feature = "libxml_valid", feature = "libxml_push"))]
pub(crate) fn des_xml_saxhandler_ptr(_no: i32, _val: XmlSAXHandlerPtr, _nr: i32) {}

pub(crate) unsafe fn desret_xml_parser_ctxt_ptr(val: XmlParserCtxtPtr) {
    xml_free_parser_ctxt(val);
}

pub(crate) fn gen_xml_parser_node_info_seq_ptr(_no: i32, _nr: i32) -> XmlParserNodeInfoSeqPtr {
    null_mut()
}

pub(crate) fn des_xml_parser_node_info_seq_ptr(_no: i32, _val: XmlParserNodeInfoSeqPtr, _nr: i32) {}

pub(crate) fn desret_const_xml_parser_node_info_ptr(_val: *const XmlParserNodeInfo) {}

pub(crate) unsafe fn gen_xml_parser_ctxt_ptr(no: i32, _nr: i32) -> XmlParserCtxtPtr {
    if no == 0 {
        return xml_new_parser_ctxt();
    }
    if no == 1 {
        return xml_create_memory_parser_ctxt("<doc/>".as_bytes().to_vec());
    }
    null_mut()
}

pub(crate) unsafe fn des_xml_parser_ctxt_ptr(_no: i32, val: XmlParserCtxtPtr, _nr: i32) {
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

pub(crate) fn gen_xml_node_ptr_ptr(_no: i32, _nr: i32) -> *mut *mut XmlNode {
    null_mut()
}

pub(crate) fn des_xml_node_ptr_ptr(_no: i32, _val: *mut *mut XmlNode, _nr: i32) {}

pub(crate) fn gen_char_ptr_ptr(_no: i32, _nr: i32) -> *mut *mut c_char {
    null_mut()
}

pub(crate) fn des_char_ptr_ptr(_no: i32, _vall: *mut *mut c_char, _nr: i32) {}

pub(crate) fn desret_xml_char(_val: XmlChar) {}

pub(crate) fn desret_long(_val: i64) {}

pub(crate) fn desret_unsigned_long(_val: u64) {}

pub(crate) fn gen_xml_entities_table_ptr(_no: i32, _nr: i32) -> XmlEntitiesTablePtr {
    null_mut()
}

pub(crate) fn des_xml_entities_table_ptr(_no: i32, _val: XmlEntitiesTablePtr, _nr: i32) {}

pub(crate) fn gen_xml_entity_ptr(_no: i32, _nr: i32) -> *mut XmlEntity {
    null_mut()
}

pub(crate) fn des_xml_entity_ptr(_no: i32, _val: *mut XmlEntity, _nr: i32) {}

pub(crate) fn gen_const_xml_doc_ptr(_no: i32, _nr: i32) -> *const XmlDoc {
    null_mut()
}

pub(crate) fn des_const_xml_doc_ptr(_no: i32, _val: *const XmlDoc, _nr: i32) {}

pub(crate) unsafe fn desret_xml_doc_ptr(val: XmlDocPtr) {
    if val.as_ptr() != API_DOC.get() {
        xml_free_doc(val);
    }
}

#[cfg(feature = "catalog")]
pub(crate) fn gen_xml_catalog_prefer(no: i32, _nr: i32) -> XmlCatalogPrefer {
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
pub(crate) fn des_xml_catalog_prefer(_no: i32, _val: XmlCatalogPrefer, _nr: i32) {}

#[cfg(feature = "catalog")]
pub(crate) fn desret_xml_catalog_prefer(_val: XmlCatalogPrefer) {}

#[cfg(feature = "catalog")]
pub(crate) fn gen_xml_catalog_allow(no: i32, _nr: i32) -> XmlCatalogAllow {
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
pub(crate) fn des_xml_catalog_allow(_no: i32, _val: XmlCatalogAllow, _nr: i32) {}

#[cfg(feature = "catalog")]
pub(crate) fn desret_xml_catalog_allow(_val: XmlCatalogAllow) {}

pub(crate) fn desret_void_ptr(_val: *mut c_void) {}

pub(crate) fn desret_const_char_ptr(_val: *const c_char) {}

pub(crate) unsafe fn desret_xml_char_ptr(val: *mut XmlChar) {
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
pub(crate) fn gen_xml_node_set_ptr(_no: i32, _nr: i32) -> Option<Box<XmlNodeSet>> {
    None
}
#[cfg(feature = "xpath")]
pub(crate) fn des_xml_node_set_ptr(_no: i32, _val: Option<Box<XmlNodeSet>>, _nr: i32) {}

pub(crate) unsafe fn desret_xml_parser_input_ptr(val: XmlParserInputPtr) {
    xml_free_input_stream(val);
}

pub(crate) unsafe fn desret_xml_entity_ptr(val: *mut XmlEntity) {
    if !val.is_null() {
        (*val).unlink();
        xml_free_node(val as *mut XmlNode);
    }
}

pub(crate) unsafe fn gen_xml_char_ptr(no: i32, _nr: i32) -> *mut XmlChar {
    CHARTAB.with_borrow_mut(|table| {
        if no == 0 {
            return addr_of_mut!(table[0]);
        }
        null_mut()
    })
}
pub(crate) fn des_xml_char_ptr(_no: i32, _val: *mut XmlChar, _nr: i32) {}

pub(crate) fn gen_xml_element_content_ptr(_no: i32, _nr: i32) -> XmlElementContentPtr {
    null_mut()
}
pub(crate) unsafe fn des_xml_element_content_ptr(_no: i32, val: XmlElementContentPtr, _nr: i32) {
    if !val.is_null() {
        xml_free_element_content(val);
    }
}
pub(crate) unsafe fn desret_xml_element_content_ptr(val: XmlElementContentPtr) {
    if !val.is_null() {
        xml_free_element_content(val);
    }
}

unsafe fn get_api_root() -> *mut XmlNode {
    if (API_ROOT.get().is_null())
        || (*API_ROOT.get()).element_type() != XmlElementType::XmlElementNode
    {
        get_api_doc();
        if !API_DOC.get().is_null()
            && (*API_DOC.get()).children.is_some()
            && (*API_DOC.get()).children.unwrap().next.is_some()
            && (*API_DOC.get())
                .children
                .unwrap()
                .next
                .unwrap()
                .element_type()
                == XmlElementType::XmlElementNode
        {
            API_ROOT.set((*API_DOC.get()).children.unwrap().next.unwrap().as_ptr());
        }
    }
    API_ROOT.get()
}

unsafe fn get_api_doc() -> *mut XmlDoc {
    if API_DOC.get().is_null() {
        API_DOC.set(xml_read_memory("<!DOCTYPE root [<!ELEMENT root EMPTY>]><root xmlns:h='http://example.com/' h:foo='bar'/>".as_bytes().to_vec(), Some("root_test"), None, 0).map_or(null_mut(), |doc| doc.as_ptr()));
        API_ROOT.set(null_mut());
        API_ATTR.set(null_mut());
    }
    API_DOC.get()
}

unsafe fn free_api_doc() {
    let doc = API_DOC.get();
    if !doc.is_null() {
        xml_free_doc(XmlDocPtr::from_raw(doc).unwrap().unwrap());
    }
    API_DOC.set(null_mut());
    API_DTD.set(null_mut());
    API_ROOT.set(null_mut());
    API_ATTR.set(null_mut());
    API_NS.set(null_mut());
}

pub(crate) unsafe fn gen_xml_node_ptr(no: i32, _nr: i32) -> *mut XmlNode {
    if no == 0 {
        return xml_new_pi("test", None).map_or(null_mut(), |node| node.as_ptr());
    }
    if no == 1 {
        return get_api_root();
    }
    null_mut()
    /*     if no == 2 {
    // return((xmlNodePtr) get_api_doc()); */
}
pub(crate) unsafe fn des_xml_node_ptr(no: i32, val: *mut XmlNode, _nr: i32) {
    if no == 1 {
        free_api_doc();
    } else if !val.is_null() {
        (*val).unlink();
        xml_free_node(val);
    }
}

pub(crate) fn gen_unsigned_char_ptr(_no: i32, _nr: i32) -> *mut u8 {
    null_mut()
}

pub(crate) unsafe fn gen_int_ptr(no: i32, nr: i32) -> *mut i32 {
    INTTAB.with_borrow_mut(|table| {
        if no == 0 {
            addr_of_mut!(table[nr as usize])
        } else {
            null_mut()
        }
    })
}

pub(crate) fn gen_const_unsigned_char_ptr(_no: i32, _nr: i32) -> *mut u8 {
    null_mut()
}

pub(crate) fn desret_int(_val: i32) {}

pub(crate) fn des_unsigned_char_ptr(_no: i32, _val: *mut u8, _nr: i32) {}

pub(crate) fn des_int_ptr(_no: i32, _val: *mut i32, _nr: i32) {}

pub(crate) fn des_const_unsigned_char_ptr(_no: i32, _val: *const u8, _nr: i32) {}

#[cfg(feature = "html")]
pub(crate) fn gen_const_html_node_ptr(_no: i32, _nr: i32) -> HtmlNodePtr {
    null_mut()
}

#[cfg(feature = "html")]
pub(crate) fn des_const_html_node_ptr(_no: i32, _val: HtmlNodePtr, _nr: i32) {}

#[cfg(feature = "html")]
pub(crate) unsafe fn desret_html_parser_ctxt_ptr(val: HtmlParserCtxtPtr) {
    if !val.is_null() {
        html_free_parser_ctxt(val);
    }
}

#[cfg(feature = "html")]
pub(crate) unsafe fn gen_html_doc_ptr(no: i32, _nr: i32) -> Option<HtmlDocPtr> {
    use crate::libxml::{htmlparser::html_read_memory, htmltree::html_new_doc};

    if no == 0 {
        return html_new_doc(null_mut(), null_mut());
    }
    if no == 1 {
        return html_read_memory("<html/>".as_bytes().to_vec(), Some("test"), None, 0);
    }
    None
}

#[cfg(feature = "html")]
pub(crate) unsafe fn desret_html_doc_ptr(val: Option<HtmlDocPtr>) {
    if let Some(val) = val.filter(|val| {
        val.as_ptr() != API_DOC.get()
            && val.doc.map_or(null_mut(), |doc| doc.as_ptr()) != API_DOC.get()
    }) {
        xml_free_doc(val);
    }
}

#[cfg(feature = "html")]
pub(crate) unsafe fn gen_html_node_ptr(_no: i32, _nr: i32) -> HtmlNodePtr {
    null_mut()
}
#[cfg(feature = "html")]
pub(crate) unsafe fn des_html_node_ptr(_no: i32, _val: HtmlNodePtr, _nr: i32) {}

#[cfg(feature = "html")]
pub(crate) unsafe fn des_html_doc_ptr(_no: i32, val: Option<HtmlDocPtr>, _nr: i32) {
    if let Some(val) = val.filter(|val| {
        val.as_ptr() != API_DOC.get()
            && val.doc.map_or(null_mut(), |doc| doc.as_ptr()) != API_DOC.get()
    }) {
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
pub(crate) fn gen_const_xml_char_ptr_ptr(_no: i32, _nr: i32) -> *mut *mut XmlChar {
    null_mut()
}

#[cfg(feature = "html")]
pub(crate) unsafe fn gen_html_parser_ctxt_ptr(no: i32, _nr: i32) -> HtmlParserCtxtPtr {
    use crate::libxml::htmlparser::html_create_memory_parser_ctxt;

    if no == 0 {
        return xml_new_parser_ctxt();
    }
    if no == 1 {
        return html_create_memory_parser_ctxt("<html/>".as_bytes().to_vec());
    }
    null_mut()
}

#[cfg(feature = "html")]
pub(crate) unsafe fn des_html_parser_ctxt_ptr(_no: i32, val: HtmlParserCtxtPtr, _nr: i32) {
    if !val.is_null() {
        html_free_parser_ctxt(val);
    }
}

pub(crate) fn desret_const_html_entity_desc_ptr(_val: *const HtmlEntityDesc) {}

pub(crate) fn gen_const_xml_char_ptr(no: i32, _nr: i32) -> *const XmlChar {
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

pub(crate) fn des_const_xml_char_ptr(_no: i32, _val: *const XmlChar, _nr: i32) {}

pub(crate) fn des_userdata(_no: i32, _val: *mut c_void, _nr: i32) {}

static mut CALL_TESTS: i32 = 0;

pub(crate) unsafe fn gen_userdata(no: i32, _nr: i32) -> *mut c_void {
    if no == 0 {
        return addr_of_mut!(CALL_TESTS) as _;
    }
    if no == 1 {
        return -1 as _;
    }
    null_mut()
}

pub(crate) fn gen_int(no: i32, _nr: i32) -> i32 {
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

pub(crate) fn des_int(_no: i32, _val: i32, _nr: i32) {}

pub(crate) fn gen_parseroptions(no: i32, _nr: i32) -> i32 {
    if no == 0 {
        return XmlParserOption::XmlParseNoBlanks as i32 | XmlParserOption::XmlParseRecover as i32;
    }
    if no == 1 {
        return XmlParserOption::XmlParseNoEnt as i32
            | XmlParserOption::XmlParseDTDLoad as i32
            | XmlParserOption::XmlParseDTDAttr as i32
            | XmlParserOption::XmlParseDTDValid as i32
            | XmlParserOption::XmlParseNoCDATA as i32;
    }
    if no == 2 {
        return XmlParserOption::XmlParseXInclude as i32
            | XmlParserOption::XmlParseNoXIncnode as i32
            | XmlParserOption::XmlParseNsClean as i32;
    }
    if no == 3 {
        return XmlParserOption::XmlParseXInclude as i32 | XmlParserOption::XmlParseNoDict as i32;
    }
    XmlParserOption::XmlParseSAX1 as i32
}

pub(crate) fn des_parseroptions(_no: i32, _val: i32, _nr: i32) {}

pub(crate) fn gen_void_ptr(_no: i32, _nr: i32) -> *mut c_void {
    null_mut()
}
pub(crate) fn des_void_ptr(_no: i32, _val: *mut c_void, _nr: i32) {}

#[cfg(feature = "libxml_output")]
pub(crate) unsafe fn gen_xml_output_buffer_ptr(
    no: i32,
    _nr: i32,
) -> Option<XmlOutputBuffer<'static>> {
    if no == 0 {
        return XmlOutputBuffer::from_uri("test.out", None, 0);
    }
    None
}

#[cfg(feature = "libxml_output")]
pub(crate) fn des_xml_output_buffer_ptr(_no: i32, _val: XmlOutputBuffer, _nr: i32) {}

pub(crate) unsafe fn gen_xml_doc_ptr(no: i32, _nr: i32) -> *mut XmlDoc {
    if no == 0 {
        return xml_new_doc(Some("1.0")).map_or(null_mut(), |doc| doc.as_ptr());
    }
    if no == 1 {
        return xml_read_memory("<foo/>".as_bytes().to_vec(), Some("test"), None, 0)
            .map_or(null_mut(), |doc| doc.as_ptr());
    }
    if no == 2 {
        return xml_read_memory(
            "<!DOCTYPE foo []> <foo/>".as_bytes().to_vec(),
            Some("test"),
            None,
            0,
        )
        .map_or(null_mut(), |doc| doc.as_ptr());
    }
    null_mut()
}

pub(crate) unsafe fn des_xml_doc_ptr(_no: i32, val: *mut XmlDoc, _nr: i32) {
    if !val.is_null()
        && val != API_DOC.get()
        && (*val).doc.map_or(null_mut(), |doc| doc.as_ptr()) != API_DOC.get()
    {
        xml_free_doc(XmlDocPtr::from_raw(val).unwrap().unwrap());
    }
}

pub(crate) fn gen_const_char_ptr(no: i32, _nr: i32) -> *mut c_char {
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

pub(crate) fn des_const_char_ptr(_no: i32, _val: *const c_char, _nr: i32) {}

/*
 We need some "remote" addresses, but want to avoid getting into
 name resolution delays, so we use these
*/
const REMOTE1GOOD: &CStr = c"http://localhost/";
const REMOTE1BAD: &CStr = c"http:http://http";
const REMOTE2GOOD: &CStr = c"ftp://localhost/foo";

pub(crate) fn gen_fileoutput(no: i32, _nr: i32) -> *const c_char {
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

pub(crate) fn des_fileoutput(_no: i32, _val: *const c_char, _nr: i32) {}

pub(crate) fn gen_filepath(no: i32, _nr: i32) -> *const c_char {
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
pub(crate) fn des_filepath(_no: i32, _val: *const c_char, _nr: i32) {}

pub(crate) fn desret_const_xml_char_ptr(_val: *const XmlChar) {}

pub(crate) fn gen_unsigned_int(no: i32, _nr: i32) -> u32 {
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

pub(crate) fn des_unsigned_int(_no: i32, _val: u32, _nr: i32) {}

pub(crate) fn des_const_xml_char_ptr_ptr(_no: i32, _val: *mut *const XmlChar, _nr: i32) {}

pub(crate) fn gen_file_ptr(_no: i32, _nr: i32) -> Option<File> {
    return File::options()
        .append(true)
        .read(true)
        .create(true)
        .open("test.out")
        .ok();
}
pub(crate) fn des_file_ptr(_no: i32, _val: File, _nr: i32) {}

pub(crate) fn gen_xml_char_ptr_ptr(_no: i32, _nr: i32) -> *mut *mut XmlChar {
    null_mut()
}
pub(crate) fn des_xml_char_ptr_ptr(_no: i32, _val: *mut *mut XmlChar, _nr: i32) {}
