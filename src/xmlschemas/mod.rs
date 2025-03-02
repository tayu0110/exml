//! Provide methods and data structures for parsing XML Schemas.
//!
//! This module is based on `libxml/xmlschemas.h`, `xmlschemas.c`, `xmlschemastypes.c` and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: incomplete XML Schemas structure implementation
// Description: interface to the XML Schemas handling and schema validity
//              checking, it is incomplete right now.
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// schemas.c : implementation of the XML Schema handling and schema validity checking
//
// See Copyright for the status of this software.
//
// Daniel Veillard <veillard@redhat.com>

pub(crate) mod error;
pub mod items;

use items::XmlSchemaType;

use crate::libxml::schemas_internals::{
    XML_SCHEMAS_TYPE_DERIVATION_METHOD_EXTENSION, XML_SCHEMAS_TYPE_DERIVATION_METHOD_RESTRICTION,
    XML_SCHEMAS_TYPE_VARIETY_ATOMIC, XML_SCHEMAS_TYPE_VARIETY_LIST, XML_SCHEMAS_TYPE_VARIETY_UNION,
    XmlSchemaTypeType, XmlSchemaValType,
};

pub(crate) unsafe fn wxs_is_atomic(r#type: *mut XmlSchemaType) -> bool {
    unsafe { (*r#type).flags & XML_SCHEMAS_TYPE_VARIETY_ATOMIC != 0 }
}

pub(crate) unsafe fn wxs_is_list(r#type: *mut XmlSchemaType) -> bool {
    unsafe { (*r#type).flags & XML_SCHEMAS_TYPE_VARIETY_LIST != 0 }
}

pub(crate) unsafe fn wxs_is_union(r#type: *mut XmlSchemaType) -> bool {
    unsafe { (*r#type).flags & XML_SCHEMAS_TYPE_VARIETY_UNION != 0 }
}

pub(crate) unsafe fn wxs_is_restriction(r#type: *mut XmlSchemaType) -> bool {
    unsafe { (*r#type).flags & XML_SCHEMAS_TYPE_DERIVATION_METHOD_RESTRICTION != 0 }
}

pub(crate) unsafe fn wxs_is_extension(r#type: *mut XmlSchemaType) -> bool {
    unsafe { (*r#type).flags & XML_SCHEMAS_TYPE_DERIVATION_METHOD_EXTENSION != 0 }
}

pub(crate) unsafe fn wxs_is_anytype(r#type: *mut XmlSchemaType) -> bool {
    unsafe {
        (*r#type).typ == XmlSchemaTypeType::XmlSchemaTypeBasic
            && (*r#type).built_in_type == XmlSchemaValType::XmlSchemasAnytype as i32
    }
}

pub(crate) unsafe fn wxs_is_simple(r#type: *mut XmlSchemaType) -> bool {
    unsafe {
        (*r#type).typ == XmlSchemaTypeType::XmlSchemaTypeSimple
            || ((*r#type).typ == XmlSchemaTypeType::XmlSchemaTypeBasic
                && (*r#type).built_in_type != XmlSchemaValType::XmlSchemasAnytype as i32)
    }
}

pub(crate) unsafe fn wxs_is_complex(r#type: *mut XmlSchemaType) -> bool {
    unsafe {
        (*r#type).typ == XmlSchemaTypeType::XmlSchemaTypeComplex
            || (*r#type).built_in_type == XmlSchemaValType::XmlSchemasAnytype as i32
    }
}

pub(crate) unsafe fn wxs_is_any_simple_type(r#type: *mut XmlSchemaType) -> bool {
    unsafe {
        (*r#type).typ == XmlSchemaTypeType::XmlSchemaTypeBasic
            && (*r#type).built_in_type == XmlSchemaValType::XmlSchemasAnysimpletype as i32
    }
}
