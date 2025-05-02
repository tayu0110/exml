use std::{
    ffi::{CStr, CString},
    ptr::null_mut,
};

use crate::{
    dict::xml_dict_lookup,
    error::XmlParserErrors,
    libxml::{
        schemas_internals::{XmlSchemaTypeType, XmlSchemaValType},
        xmlschemas::{XmlSchemaAbstractCtxtPtr, xml_schema_get_prop_node},
        xmlschemastypes::xml_schema_val_predef_type_node,
    },
    tree::{XmlAttrPtr, XmlNodePtr},
};

use super::{
    context::XmlSchemaParserCtxt,
    error::{xml_schema_internal_err, xml_schema_perr, xml_schema_psimple_type_err},
    items::{XmlSchemaBasicItemPtr, XmlSchemaTypePtr},
};

impl XmlSchemaParserCtxt<'_> {
    /// Extracts and validates a value against the given built-in type.
    /// This one is intended to be used internally for validation
    /// of schema attribute values during parsing of the schema.
    ///
    /// Returns 0 if the value is valid, a positive error code
    /// number otherwise and -1 in case of an internal or API error.
    #[doc(alias = "xmlSchemaPValAttr")]
    pub(crate) unsafe fn validate_attribute(
        &mut self,
        owner_item: XmlSchemaBasicItemPtr,
        owner_elem: XmlNodePtr,
        name: &str,
        typ: XmlSchemaTypePtr,
        value: *mut *const u8,
    ) -> i32 {
        unsafe {
            if typ.is_null() {
                if !value.is_null() {
                    *value = null_mut();
                }
                return -1;
            }
            if (*typ).typ != XmlSchemaTypeType::XmlSchemaTypeBasic {
                if !value.is_null() {
                    *value = null_mut();
                }
                let name = CStr::from_ptr((*typ).name as *const i8).to_string_lossy();
                xml_schema_perr(
                    self,
                    Some(owner_elem.into()),
                    XmlParserErrors::XmlSchemapInternal,
                    format!(
                        "Internal error: xmlSchemaPValAttr, the given type '{}' is not a built-in type.\n",
                        name
                    ).as_str(),
                    Some(&name),
                    None,
                );
                return -1;
            }
            let Some(attr) = xml_schema_get_prop_node(owner_elem, name) else {
                if !value.is_null() {
                    *value = null_mut();
                }
                return 0;
            };
            self.validate_attr_node(owner_item, attr, typ, value)
        }
    }

    /// Extracts and validates a value against the given built-in type.
    /// This one is intended to be used internally for validation
    /// of schema attribute values during parsing of the schema.
    ///
    /// Returns 0 if the value is valid, a positive error code
    /// number otherwise and -1 in case of an internal or API error.
    #[doc(alias = "xmlSchemaPValAttrNode")]
    pub(crate) unsafe fn validate_attr_node(
        &mut self,
        owner_item: XmlSchemaBasicItemPtr,
        attr: XmlAttrPtr,
        typ: XmlSchemaTypePtr,
        value: *mut *const u8,
    ) -> i32 {
        unsafe {
            if typ.is_null() {
                return -1;
            }

            let val = self.get_node_content(Some(attr.into()));
            if !value.is_null() {
                *value = xml_dict_lookup(self.dict, val.as_ptr(), val.len() as i32);
            }

            let val = CString::new(val).unwrap();
            self.validate_attr_node_value(owner_item, attr, val.as_ptr() as *const u8, typ)
        }
    }

    /// Validates a value against the given built-in type.
    /// This one is intended to be used internally for validation
    /// of schema attribute values during parsing of the schema.
    ///
    /// Returns 0 if the value is valid, a positive error code
    /// number otherwise and -1 in case of an internal or API error.
    #[doc(alias = "xmlSchemaPValAttrNodeValue")]
    pub(crate) unsafe fn validate_attr_node_value(
        &mut self,
        owner_item: XmlSchemaBasicItemPtr,
        attr: XmlAttrPtr,
        value: *const u8,
        typ: XmlSchemaTypePtr,
    ) -> i32 {
        unsafe {
            let mut ret: i32;

            // NOTE: Should we move this to xmlschematypes.c? Hmm, but this
            // one is really meant to be used internally, so better not.
            if typ.is_null() {
                return -1;
            }
            if (*typ).typ != XmlSchemaTypeType::XmlSchemaTypeBasic {
                xml_schema_internal_err(
                    self as *mut Self as XmlSchemaAbstractCtxtPtr,
                    "xmlSchemaPValAttrNodeValue",
                    "the given type is not a built-in type",
                );
                return -1;
            }
            match (*typ).built_in_type {
                XmlSchemaValType::XmlSchemasNCName
                | XmlSchemaValType::XmlSchemasQName
                | XmlSchemaValType::XmlSchemasAnyURI
                | XmlSchemaValType::XmlSchemasToken
                | XmlSchemaValType::XmlSchemasLanguage => {
                    ret =
                        xml_schema_val_predef_type_node(typ, value, null_mut(), Some(attr.into()));
                }
                _ => {
                    xml_schema_internal_err(
                        self as *mut Self as XmlSchemaAbstractCtxtPtr,
                        "xmlSchemaPValAttrNodeValue",
                        "validation using the given type is not supported while parsing a schema",
                    );
                    return -1;
                }
            }
            // TODO: Should we use the S4S error codes instead?
            match ret.cmp(&0) {
                std::cmp::Ordering::Less => {
                    xml_schema_internal_err(
                        self as *mut Self as XmlSchemaAbstractCtxtPtr,
                        "xmlSchemaPValAttrNodeValue",
                        "failed to validate a schema attribute value",
                    );
                    return -1;
                }
                std::cmp::Ordering::Greater => {
                    if (*typ).wxs_is_list() {
                        ret = XmlParserErrors::XmlSchemavCvcDatatypeValid1_2_2 as i32;
                    } else {
                        ret = XmlParserErrors::XmlSchemavCvcDatatypeValid1_2_1 as i32;
                    }
                    let value = CStr::from_ptr(value as *const i8).to_string_lossy();
                    xml_schema_psimple_type_err(
                        self,
                        ret.try_into().unwrap(),
                        owner_item,
                        attr.into(),
                        Some(&*typ),
                        None,
                        Some(&value),
                        None,
                        None,
                        None,
                    );
                }
                std::cmp::Ordering::Equal => {}
            }
            ret
        }
    }
}
