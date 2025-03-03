use std::{
    cell::{Cell, RefCell},
    ffi::{CStr, c_char},
    fs::File,
    os::raw::c_void,
    ptr::{addr_of_mut, null, null_mut},
    sync::Mutex,
};

#[cfg(feature = "catalog")]
use crate::libxml::catalog::{XmlCatalogAllow, XmlCatalogPrefer};
use crate::{
    libxml::{
        globals::xml_free,
        htmlparser::{
            HtmlElemDesc, HtmlEntityDesc, HtmlParserCtxtPtr, HtmlStatus, html_free_parser_ctxt,
        },
        parser::{XmlFeature, XmlParserOption},
        pattern::{XmlPatternPtr, XmlStreamCtxtPtr},
        relaxng::XmlRelaxNGPtr,
        schemas_internals::{XmlSchemaFacetPtr, XmlSchemaValType},
        uri::XmlURIPtr,
        valid::{
            XmlValidCtxtPtr, xml_free_element_content, xml_free_valid_ctxt, xml_new_valid_ctxt,
        },
        xinclude::XmlXIncludeCtxtPtr,
        xmlautomata::{XmlAutomataPtr, XmlAutomataStatePtr},
        xmlmodule::XmlModulePtr,
        xmlreader::{XmlTextReaderLocatorPtr, XmlTextReaderPtr},
        xmlregexp::{XmlExpCtxtPtr, XmlExpNodePtr, XmlRegExecCtxtPtr, XmlRegexpPtr},
        xmlschemas::XmlSchemaPtr,
        xmlschemastypes::{XmlSchemaValPtr, XmlSchemaWhitespaceValueType},
        xmlstring::XmlChar,
    },
    parser::{
        XmlParserCtxtPtr, XmlParserInputPtr, xml_create_memory_parser_ctxt, xml_free_input_stream,
        xml_free_parser_ctxt, xml_new_parser_ctxt,
    },
    tree::{XmlAttr, XmlAttributeType, XmlDoc, XmlDtd, XmlElementContentPtr, XmlNode, XmlNs},
    xpath::{
        XmlNodeSet, XmlXPathCompExprPtr, XmlXPathContextPtr, XmlXPathObjectPtr,
        XmlXPathParserContextPtr,
    },
};
#[cfg(feature = "schema")]
use crate::{
    relaxng::XmlRelaxNGValidCtxtPtr,
    xmlschemas::{
        context::{XmlSchemaParserCtxtPtr, XmlSchemaValidCtxtPtr},
        items::XmlSchemaTypePtr,
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

pub(crate) const GEN_NB_UNSIGNED_CHAR_PTR: i32 = 1;
pub(crate) const GEN_NB_INT_PTR: i32 = 2;
pub(crate) const GEN_NB_CONST_UNSIGNED_CHAR_PTR: i32 = 1;
pub(crate) const GEN_NB_HTML_PARSER_CTXT_PTR: i32 = 3;
pub(crate) const GEN_NB_FILEPATH: i32 = 8;
pub(crate) const GEN_NB_CONST_CHAR_PTR: i32 = 4;
pub(crate) const GEN_NB_CONST_HTML_ELEM_DESC_PTR: i32 = 1;
pub(crate) const GEN_NB_CONST_XML_CHAR_PTR: i32 = 5;
pub(crate) const GEN_NB_INT: i32 = 4;
pub(crate) const GEN_NB_USERDATA: i32 = 3;
pub(crate) const GEN_NB_UNSIGNED_INT: i32 = 3;
pub(crate) const GEN_NB_CONST_XML_CHAR_PTR_PTR: i32 = 1;
pub(crate) const GEN_NB_FILE_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_CHAR_PTR_PTR: i32 = 1;
pub(crate) const GEN_NB_VOID_PTR: i32 = 2;
pub(crate) const GEN_NB_XML_CHAR_PTR: i32 = 2;
#[cfg(feature = "xpath")]
pub(crate) const GEN_NB_XML_NODE_SET_PTR: i32 = 1;
#[cfg(feature = "catalog")]
pub(crate) const GEN_NB_XML_CATALOG_ALLOW: i32 = 4;
#[cfg(feature = "catalog")]
pub(crate) const GEN_NB_XML_CATALOG_PREFER: i32 = 3;
pub(crate) const GEN_NB_XML_PARSER_INPUT_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_PARSER_CTXT_PTR: i32 = 3;
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
pub(crate) const GEN_NB_XML_RELAXNG_VALID_CTXT_PTR: i32 = 1;
#[cfg(any(
    feature = "libxml_modules",
    feature = "libxml_reader",
    feature = "schema"
))]
pub(crate) const GEN_NB_VOID_PTR_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_URIPTR: i32 = 1;
pub(crate) const GEN_NB_CHAR_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_ELEMENT_CONTENT_PTR: i32 = 1;
pub(crate) const GEN_NB_XML_VALID_CTXT_PTR: i32 = 2;
pub(crate) const GEN_NB_XML_ATTRIBUTE_TYPE: i32 = 4;
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

#[cfg(feature = "xpath")]
pub(crate) fn gen_xml_xpath_parser_context_ptr(_no: i32, _nr: i32) -> XmlXPathParserContextPtr {
    null_mut()
}

#[cfg(feature = "xpath")]
pub(crate) fn des_xml_xpath_parser_context_ptr(_no: i32, _val: XmlXPathParserContextPtr, _nr: i32) {
}

#[cfg(feature = "xpath")]
pub(crate) unsafe fn desret_xml_xpath_object_ptr(val: XmlXPathObjectPtr) {
    unsafe {
        use crate::xpath::xml_xpath_free_object;

        xml_xpath_free_object(val);
    }
}

#[cfg(feature = "xpath")]
pub(crate) unsafe fn desret_xml_node_set_ptr(val: Option<Box<crate::xpath::XmlNodeSet>>) {
    unsafe {
        use crate::xpath::xml_xpath_free_node_set;

        xml_xpath_free_node_set(val);
    }
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
    unsafe {
        use crate::xpath::object::{
            xml_xpath_new_boolean, xml_xpath_new_float, xml_xpath_new_node_set,
            xml_xpath_new_string,
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
            return xml_xpath_new_node_set(None);
        }
        null_mut()
    }
}

#[cfg(feature = "xpath")]
pub(crate) unsafe fn des_xml_xpath_object_ptr(_no: i32, val: XmlXPathObjectPtr, _nr: i32) {
    unsafe {
        use crate::xpath::xml_xpath_free_object;

        if !val.is_null() {
            xml_xpath_free_object(val);
        }
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
    unsafe {
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
}

#[cfg(feature = "libxml_reader")]
pub(crate) unsafe fn des_xml_text_reader_ptr(_no: i32, val: XmlTextReaderPtr, _nr: i32) {
    unsafe {
        use crate::libxml::xmlreader::xml_free_text_reader;

        if !val.is_null() {
            xml_free_text_reader(val);
        }
    }
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
pub(crate) fn gen_xml_xinclude_ctxt_ptr(_no: i32, _nr: i32) -> XmlXIncludeCtxtPtr {
    null_mut()
}

#[cfg(feature = "xinclude")]
pub(crate) fn des_xml_xinclude_ctxt_ptr(_no: i32, _val: XmlXIncludeCtxtPtr, _nr: i32) {}

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
    unsafe {
        #[cfg(feature = "libxml_valid")]
        if no == 0 {
            return xml_new_valid_ctxt();
        }
        null_mut()
    }
}
pub(crate) unsafe fn des_xml_valid_ctxt_ptr(_no: i32, val: XmlValidCtxtPtr, _nr: i32) {
    unsafe {
        #[cfg(feature = "libxml_valid")]
        if !val.is_null() {
            xml_free_valid_ctxt(val);
        }
    }
}

pub(crate) fn gen_char_ptr(_no: i32, _nr: i32) -> *mut c_char {
    null_mut()
}

pub(crate) fn des_char_ptr(_no: i32, _val: *mut c_char, _nr: i32) {}

pub(crate) fn gen_xml_uriptr(_no: i32, _nr: i32) -> XmlURIPtr {
    null_mut()
}

pub(crate) fn des_xml_uriptr(_no: i32, _val: XmlURIPtr, _nr: i32) {}

#[cfg(feature = "schema")]
pub(crate) unsafe fn desret_xml_schema_parser_ctxt_ptr(val: XmlSchemaParserCtxtPtr) {
    use crate::xmlschemas::context::xml_schema_free_parser_ctxt;

    unsafe {
        xml_schema_free_parser_ctxt(val);
    }
}

#[cfg(feature = "schema")]
pub(crate) fn desret_xml_schema_type_ptr(_val: XmlSchemaTypePtr) {}

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

pub(crate) unsafe fn desret_xml_parser_ctxt_ptr(val: XmlParserCtxtPtr) {
    unsafe {
        xml_free_parser_ctxt(val);
    }
}

pub(crate) unsafe fn gen_xml_parser_ctxt_ptr(no: i32, _nr: i32) -> XmlParserCtxtPtr {
    unsafe {
        if no == 0 {
            return xml_new_parser_ctxt();
        }
        if no == 1 {
            return xml_create_memory_parser_ctxt("<doc/>".as_bytes().to_vec());
        }
        null_mut()
    }
}

pub(crate) unsafe fn des_xml_parser_ctxt_ptr(_no: i32, val: XmlParserCtxtPtr, _nr: i32) {
    unsafe {
        if !val.is_null() {
            xml_free_parser_ctxt(val);
        }
    }
}

pub(crate) fn gen_xml_parser_input_ptr(_no: i32, _nr: i32) -> XmlParserInputPtr {
    null_mut()
}

pub(crate) fn des_xml_parser_input_ptr(_no: i32, _val: XmlParserInputPtr, _nr: i32) {}

pub(crate) fn desret_unsigned_long(_val: u64) {}

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

pub(crate) unsafe fn desret_xml_char_ptr(val: *mut XmlChar) {
    unsafe {
        if !val.is_null() {
            xml_free(val as _);
        }
    }
}

#[cfg(feature = "xpath")]
pub(crate) fn gen_xml_node_set_ptr(_no: i32, _nr: i32) -> Option<Box<XmlNodeSet>> {
    None
}
#[cfg(feature = "xpath")]
pub(crate) fn des_xml_node_set_ptr(_no: i32, _val: Option<Box<XmlNodeSet>>, _nr: i32) {}

pub(crate) unsafe fn desret_xml_parser_input_ptr(val: XmlParserInputPtr) {
    unsafe {
        xml_free_input_stream(val);
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
    unsafe {
        if !val.is_null() {
            xml_free_element_content(val);
        }
    }
}
pub(crate) unsafe fn desret_xml_element_content_ptr(val: XmlElementContentPtr) {
    unsafe {
        if !val.is_null() {
            xml_free_element_content(val);
        }
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
pub(crate) unsafe fn desret_html_parser_ctxt_ptr(val: HtmlParserCtxtPtr) {
    unsafe {
        if !val.is_null() {
            html_free_parser_ctxt(val);
        }
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
    unsafe {
        use crate::libxml::htmlparser::html_create_memory_parser_ctxt;

        if no == 0 {
            return xml_new_parser_ctxt();
        }
        if no == 1 {
            return html_create_memory_parser_ctxt("<html/>".as_bytes().to_vec());
        }
        null_mut()
    }
}

#[cfg(feature = "html")]
pub(crate) unsafe fn des_html_parser_ctxt_ptr(_no: i32, val: HtmlParserCtxtPtr, _nr: i32) {
    unsafe {
        if !val.is_null() {
            html_free_parser_ctxt(val);
        }
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
    File::options()
        .append(true)
        .read(true)
        .create(true)
        .open("test.out")
        .ok()
}

pub(crate) fn gen_xml_char_ptr_ptr(_no: i32, _nr: i32) -> *mut *mut XmlChar {
    null_mut()
}
pub(crate) fn des_xml_char_ptr_ptr(_no: i32, _val: *mut *mut XmlChar, _nr: i32) {}
