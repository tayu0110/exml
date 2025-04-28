use std::{
    cell::{Cell, RefCell},
    ffi::c_char,
    fs::File,
    ptr::{addr_of_mut, null_mut},
    sync::Mutex,
};

#[cfg(feature = "catalog")]
use crate::libxml::catalog::{XmlCatalogAllow, XmlCatalogPrefer};
#[cfg(feature = "libxml_reader")]
use crate::libxml::xmlreader::XmlTextReaderLocatorPtr;
#[cfg(feature = "schema")]
use crate::xmlschemas::{
    context::{XmlSchemaParserCtxtPtr, XmlSchemaValidCtxtPtr},
    items::XmlSchemaTypePtr,
    schema::XmlSchemaPtr,
};
use crate::{
    libxml::{
        globals::xml_free,
        relaxng::XmlRelaxNGPtr,
        schemas_internals::{XmlSchemaFacetPtr, XmlSchemaValType},
        xmlregexp::{XmlExpCtxtPtr, XmlExpNodePtr},
        xmlschemastypes::{XmlSchemaValPtr, XmlSchemaWhitespaceValueType},
        xmlstring::XmlChar,
    },
    tree::{XmlAttr, XmlDoc, XmlDtd, XmlNode, XmlNs},
    xpath::XmlNodeSet,
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

pub(crate) const GEN_NB_CONST_CHAR_PTR: i32 = 4;
pub(crate) const GEN_NB_CONST_XML_CHAR_PTR: i32 = 5;
pub(crate) const GEN_NB_INT: i32 = 4;
pub(crate) const GEN_NB_CONST_XML_CHAR_PTR_PTR: i32 = 1;
pub(crate) const GEN_NB_FILE_PTR: i32 = 1;
#[cfg(feature = "xpath")]
pub(crate) const GEN_NB_XML_NODE_SET_PTR: i32 = 1;
#[cfg(feature = "catalog")]
pub(crate) const GEN_NB_XML_CATALOG_ALLOW: i32 = 4;
#[cfg(feature = "catalog")]
pub(crate) const GEN_NB_XML_CATALOG_PREFER: i32 = 3;
#[cfg(feature = "schema")]
pub(crate) const GEN_NB_XML_RELAXNG_PTR: i32 = 1;
#[cfg(feature = "libxml_reader")]
pub(crate) const GEN_NB_XML_TEXT_READER_LOCATOR_PTR: i32 = 1;
#[cfg(feature = "schema")]
pub(crate) const GEN_NB_XML_SCHEMA_PTR: i32 = 1;
#[cfg(feature = "schema")]
pub(crate) const GEN_NB_XML_SCHEMA_VALID_CTXT_PTR: i32 = 1;
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
#[cfg(any(feature = "xpath", feature = "schema"))]
pub(crate) const GEN_NB_DOUBLE: i32 = 4;

#[cfg(feature = "xpath")]
pub(crate) fn desret_xml_node_set_ptr(val: Option<Box<crate::xpath::XmlNodeSet>>) {
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
pub(crate) fn desret_double(_val: f64) {}

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
        return XmlSchemaValType::XmlSchemasAnySimpletype;
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
pub(crate) fn gen_xml_text_reader_locator_ptr(_no: i32, _nr: i32) -> XmlTextReaderLocatorPtr {
    null_mut()
}

#[cfg(feature = "libxml_reader")]
pub(crate) fn des_xml_text_reader_locator_ptr(_no: i32, _val: XmlTextReaderLocatorPtr, _nr: i32) {}

#[cfg(feature = "schema")]
pub(crate) unsafe fn desret_xml_schema_parser_ctxt_ptr(val: XmlSchemaParserCtxtPtr) {
    use crate::xmlschemas::context::xml_schema_free_parser_ctxt;

    unsafe {
        xml_schema_free_parser_ctxt(val);
    }
}

#[cfg(feature = "schema")]
pub(crate) fn desret_xml_schema_type_ptr(_val: XmlSchemaTypePtr) {}

#[cfg(feature = "schema")]
pub(crate) fn gen_xml_relaxng_ptr(_no: i32, _nr: i32) -> XmlRelaxNGPtr {
    null_mut()
}

#[cfg(feature = "schema")]
pub(crate) fn des_xml_relaxng_ptr(_no: i32, _val: XmlRelaxNGPtr, _nr: i32) {}

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

pub(crate) fn desret_int(_val: i32) {}

pub(crate) fn gen_const_xml_char_ptr_ptr(_no: i32, _nr: i32) -> *mut *mut XmlChar {
    null_mut()
}

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

pub(crate) fn desret_const_xml_char_ptr(_val: *const XmlChar) {}

pub(crate) fn des_const_xml_char_ptr_ptr(_no: i32, _val: *mut *const XmlChar, _nr: i32) {}

pub(crate) fn gen_file_ptr(_no: i32, _nr: i32) -> Option<File> {
    File::options()
        .append(true)
        .read(true)
        .create(true)
        .open("test.out")
        .ok()
}
