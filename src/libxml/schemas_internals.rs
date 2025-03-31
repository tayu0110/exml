//! Provide internal methods and data structures for parsing XML Schemas.
//!
//! This module is based on `libxml/schemasInternals.h`, `xmlschemas.c`, `xmlschemastypes.c` and so on in `libxml2-v2.11.8`.  
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: internal interfaces for XML Schemas
// Description: internal interfaces for the XML Schemas handling and schema validity checking
//              The Schemas development is a Work In Progress.
//              Some of those interfaces are not guaranteed to be API or ABI stable !
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

use std::{any::type_name, ffi::c_void, ptr::drop_in_place, rc::Rc};

use crate::{
    tree::XmlNodePtr,
    xmlschemas::{
        item_list::{XmlSchemaItemListPtr, xml_schema_item_list_free},
        items::{XmlSchemaAttribute, XmlSchemaTypePtr},
    },
};

use super::{
    globals::xml_free,
    xmlregexp::XmlRegexp,
    xmlschemastypes::{XmlSchemaValPtr, xml_schema_free_facet},
};

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlSchemaValType {
    XmlSchemasUnknown = 0,
    XmlSchemasString = 1,
    XmlSchemasNormString = 2,
    XmlSchemasDecimal = 3,
    XmlSchemasTime = 4,
    XmlSchemasGDay = 5,
    XmlSchemasGMonth = 6,
    XmlSchemasGMonthDay = 7,
    XmlSchemasGYear = 8,
    XmlSchemasGYearMonth = 9,
    XmlSchemasDate = 10,
    XmlSchemasDatetime = 11,
    XmlSchemasDuration = 12,
    XmlSchemasFloat = 13,
    XmlSchemasDouble = 14,
    XmlSchemasBoolean = 15,
    XmlSchemasToken = 16,
    XmlSchemasLanguage = 17,
    XmlSchemasNmtoken = 18,
    XmlSchemasNmtokens = 19,
    XmlSchemasName = 20,
    XmlSchemasQName = 21,
    XmlSchemasNCName = 22,
    XmlSchemasID = 23,
    XmlSchemasIDREF = 24,
    XmlSchemasIDREFS = 25,
    XmlSchemasEntity = 26,
    XmlSchemasEntities = 27,
    XmlSchemasNotation = 28,
    XmlSchemasAnyURI = 29,
    XmlSchemasInteger = 30,
    XmlSchemasNPInteger = 31,
    XmlSchemasNInteger = 32,
    XmlSchemasNNInteger = 33,
    XmlSchemasPInteger = 34,
    XmlSchemasInt = 35,
    XmlSchemasUInt = 36,
    XmlSchemasLong = 37,
    XmlSchemasULong = 38,
    XmlSchemasShort = 39,
    XmlSchemasUShort = 40,
    XmlSchemasByte = 41,
    XmlSchemasUByte = 42,
    XmlSchemasHexbinary = 43,
    XmlSchemasBase64binary = 44,
    XmlSchemasAnytype = 45,
    XmlSchemasAnySimpletype = 46,
}

impl PartialEq<i32> for XmlSchemaValType {
    fn eq(&self, other: &i32) -> bool {
        *self as i32 == *other
    }
}

impl TryFrom<i32> for XmlSchemaValType {
    type Error = anyhow::Error;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        if value == Self::XmlSchemasUnknown as i32 {
            Ok(Self::XmlSchemasUnknown)
        } else if value == Self::XmlSchemasString as i32 {
            Ok(Self::XmlSchemasString)
        } else if value == Self::XmlSchemasNormString as i32 {
            Ok(Self::XmlSchemasNormString)
        } else if value == Self::XmlSchemasDecimal as i32 {
            Ok(Self::XmlSchemasDecimal)
        } else if value == Self::XmlSchemasTime as i32 {
            Ok(Self::XmlSchemasTime)
        } else if value == Self::XmlSchemasGDay as i32 {
            Ok(Self::XmlSchemasGDay)
        } else if value == Self::XmlSchemasGMonth as i32 {
            Ok(Self::XmlSchemasGMonth)
        } else if value == Self::XmlSchemasGMonthDay as i32 {
            Ok(Self::XmlSchemasGMonthDay)
        } else if value == Self::XmlSchemasGYear as i32 {
            Ok(Self::XmlSchemasGYear)
        } else if value == Self::XmlSchemasGYearMonth as i32 {
            Ok(Self::XmlSchemasGYearMonth)
        } else if value == Self::XmlSchemasDate as i32 {
            Ok(Self::XmlSchemasDate)
        } else if value == Self::XmlSchemasDatetime as i32 {
            Ok(Self::XmlSchemasDatetime)
        } else if value == Self::XmlSchemasDuration as i32 {
            Ok(Self::XmlSchemasDuration)
        } else if value == Self::XmlSchemasFloat as i32 {
            Ok(Self::XmlSchemasFloat)
        } else if value == Self::XmlSchemasDouble as i32 {
            Ok(Self::XmlSchemasDouble)
        } else if value == Self::XmlSchemasBoolean as i32 {
            Ok(Self::XmlSchemasBoolean)
        } else if value == Self::XmlSchemasToken as i32 {
            Ok(Self::XmlSchemasToken)
        } else if value == Self::XmlSchemasLanguage as i32 {
            Ok(Self::XmlSchemasLanguage)
        } else if value == Self::XmlSchemasNmtoken as i32 {
            Ok(Self::XmlSchemasNmtoken)
        } else if value == Self::XmlSchemasNmtokens as i32 {
            Ok(Self::XmlSchemasNmtokens)
        } else if value == Self::XmlSchemasName as i32 {
            Ok(Self::XmlSchemasName)
        } else if value == Self::XmlSchemasQName as i32 {
            Ok(Self::XmlSchemasQName)
        } else if value == Self::XmlSchemasNCName as i32 {
            Ok(Self::XmlSchemasNCName)
        } else if value == Self::XmlSchemasID as i32 {
            Ok(Self::XmlSchemasID)
        } else if value == Self::XmlSchemasIDREF as i32 {
            Ok(Self::XmlSchemasIDREF)
        } else if value == Self::XmlSchemasIDREFS as i32 {
            Ok(Self::XmlSchemasIDREFS)
        } else if value == Self::XmlSchemasEntity as i32 {
            Ok(Self::XmlSchemasEntity)
        } else if value == Self::XmlSchemasEntities as i32 {
            Ok(Self::XmlSchemasEntities)
        } else if value == Self::XmlSchemasNotation as i32 {
            Ok(Self::XmlSchemasNotation)
        } else if value == Self::XmlSchemasAnyURI as i32 {
            Ok(Self::XmlSchemasAnyURI)
        } else if value == Self::XmlSchemasInteger as i32 {
            Ok(Self::XmlSchemasInteger)
        } else if value == Self::XmlSchemasNPInteger as i32 {
            Ok(Self::XmlSchemasNPInteger)
        } else if value == Self::XmlSchemasNInteger as i32 {
            Ok(Self::XmlSchemasNInteger)
        } else if value == Self::XmlSchemasNNInteger as i32 {
            Ok(Self::XmlSchemasNNInteger)
        } else if value == Self::XmlSchemasPInteger as i32 {
            Ok(Self::XmlSchemasPInteger)
        } else if value == Self::XmlSchemasInt as i32 {
            Ok(Self::XmlSchemasInt)
        } else if value == Self::XmlSchemasUInt as i32 {
            Ok(Self::XmlSchemasUInt)
        } else if value == Self::XmlSchemasLong as i32 {
            Ok(Self::XmlSchemasLong)
        } else if value == Self::XmlSchemasULong as i32 {
            Ok(Self::XmlSchemasULong)
        } else if value == Self::XmlSchemasShort as i32 {
            Ok(Self::XmlSchemasShort)
        } else if value == Self::XmlSchemasUShort as i32 {
            Ok(Self::XmlSchemasUShort)
        } else if value == Self::XmlSchemasByte as i32 {
            Ok(Self::XmlSchemasByte)
        } else if value == Self::XmlSchemasUByte as i32 {
            Ok(Self::XmlSchemasUByte)
        } else if value == Self::XmlSchemasHexbinary as i32 {
            Ok(Self::XmlSchemasHexbinary)
        } else if value == Self::XmlSchemasBase64binary as i32 {
            Ok(Self::XmlSchemasBase64binary)
        } else if value == Self::XmlSchemasAnytype as i32 {
            Ok(Self::XmlSchemasAnytype)
        } else if value == Self::XmlSchemasAnySimpletype as i32 {
            Ok(Self::XmlSchemasAnySimpletype)
        } else {
            Err(anyhow::anyhow!(
                "Invalid convert from value '{value}' to {}",
                type_name::<Self>()
            ))
        }
    }
}

/// XML Schemas defines multiple type of types.
#[doc(alias = "xmlSchemaTypeType")]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlSchemaTypeType {
    XmlSchemaTypeBasic = 1, /* A built-in datatype */
    XmlSchemaTypeAny,
    XmlSchemaTypeFacet,
    XmlSchemaTypeSimple,
    XmlSchemaTypeComplex,
    XmlSchemaTypeSequence = 6,
    XmlSchemaTypeChoice,
    XmlSchemaTypeAll,
    XmlSchemaTypeSimpleContent,
    XmlSchemaTypeComplexContent,
    XmlSchemaTypeUr,
    XmlSchemaTypeRestriction,
    XmlSchemaTypeExtension,
    XmlSchemaTypeElement,
    XmlSchemaTypeAttribute,
    XmlSchemaTypeAttributeGroup,
    XmlSchemaTypeGroup,
    XmlSchemaTypeNotation,
    XmlSchemaTypeList,
    XmlSchemaTypeUnion,
    XmlSchemaTypeAnyAttribute,
    XmlSchemaTypeIDCUnique,
    XmlSchemaTypeIDCKey,
    XmlSchemaTypeIDCKeyref,
    XmlSchemaTypeParticle = 25,
    XmlSchemaTypeAttributeUse,
    XmlSchemaFacetMinInclusive = 1000,
    XmlSchemaFacetMinExclusive,
    XmlSchemaFacetMaxInclusive,
    XmlSchemaFacetMaxExclusive,
    XmlSchemaFacetTotalDigits,
    XmlSchemaFacetFractionDigits,
    XmlSchemaFacetPattern,
    XmlSchemaFacetEnumeration,
    XmlSchemaFacetWhitespace,
    XmlSchemaFacetLength,
    XmlSchemaFacetMaxLength,
    XmlSchemaFacetMinLength,
    XmlSchemaExtraQNameRef = 2000,
    XmlSchemaExtraAttrUseProhib,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlSchemaContentType {
    XmlSchemaContentUnknown = 0,
    XmlSchemaContentEmpty = 1,
    XmlSchemaContentElements,
    XmlSchemaContentMixed,
    XmlSchemaContentSimple,
    XmlSchemaContentMixedOrElements, /* Obsolete */
    XmlSchemaContentBasic,
    XmlSchemaContentAny,
}

/// Annotation
pub type XmlSchemaAnnotPtr = *mut XmlSchemaAnnot;
#[repr(C)]
pub struct XmlSchemaAnnot {
    pub(crate) next: *mut XmlSchemaAnnot,
    pub(crate) content: XmlNodePtr, /* the annotation */
}

// /// Skip unknown attribute from validation Obsolete, not used anymore.
// const XML_SCHEMAS_ANYATTR_SKIP: i32 = 1;
// /// Ignore validation non definition on attributes Obsolete, not used anymore.
// const XML_SCHEMAS_ANYATTR_LAX: i32 = 2;
// /// Apply strict validation rules on attributes Obsolete, not used anymore.
// const XML_SCHEMAS_ANYATTR_STRICT: i32 = 3;
/// Skip unknown attribute from validation
pub(crate) const XML_SCHEMAS_ANY_SKIP: i32 = 1;
/// Used by wildcards.
/// Validate if type found, don't worry if not found
pub(crate) const XML_SCHEMAS_ANY_LAX: i32 = 2;
/// Used by wildcards.
/// Apply strict validation rules
pub(crate) const XML_SCHEMAS_ANY_STRICT: i32 = 3;
/// Used by wildcards.
/// The attribute is prohibited.
pub(crate) const XML_SCHEMAS_ATTR_USE_PROHIBITED: i32 = 0;
/// The attribute is required.
pub(crate) const XML_SCHEMAS_ATTR_USE_REQUIRED: i32 = 1;
/// The attribute is optional.
pub(crate) const XML_SCHEMAS_ATTR_USE_OPTIONAL: i32 = 2;
/// Allow elements in no namespace
pub(crate) const XML_SCHEMAS_ATTR_GLOBAL: i32 = 1 << 0;
// /// Allow elements in no namespace
// pub(crate) const XML_SCHEMAS_ATTR_NSDEFAULT: i32 = 1 << 7;
/// This is set when the "type" and "ref" references have been resolved.
pub(crate) const XML_SCHEMAS_ATTR_INTERNAL_RESOLVED: i32 = 1 << 8;
/// The attribute has a fixed value
pub(crate) const XML_SCHEMAS_ATTR_FIXED: i32 = 1 << 9;

pub type XmlSchemaAttributeLinkPtr = *mut XmlSchemaAttributeLink;
/// Used to build a list of attribute uses on complexType definitions.
/// WARNING: Deprecated; not used.
#[doc(alias = "xmlSchemaAttributeLink")]
#[repr(C)]
pub struct XmlSchemaAttributeLink {
    next: *mut XmlSchemaAttributeLink, /* the next attribute link ... */
    attr: *mut XmlSchemaAttribute,     /* the linked attribute */
}

// /// If the wildcard is complete.
// const XML_SCHEMAS_WILDCARD_COMPLETE: i32 = 1 << 0;

pub type XmlSchemaWildcardNsPtr = *mut XmlSchemaWildcardNs;
/// Used to build a list of namespaces on wildcards.
#[doc(alias = "xmlSchemaCharValueLink")]
#[repr(C)]
pub struct XmlSchemaWildcardNs {
    pub(crate) next: *mut XmlSchemaWildcardNs, /* the next constraint link ... */
    pub(crate) value: *const u8,               /* the value */
}

/// A wildcard.
#[doc(alias = "xmlSchemaWildcard")]
pub type XmlSchemaWildcardPtr = *mut XmlSchemaWildcard;
#[repr(C)]
pub struct XmlSchemaWildcard {
    pub(crate) typ: XmlSchemaTypeType, /* The kind of type */
    pub(crate) annot: XmlSchemaAnnotPtr,
    pub(crate) node: Option<XmlNodePtr>,
    pub(crate) process_contents: i32,
    pub(crate) any: i32, /* Indicates if the ns constraint is of ##any */
    pub(crate) ns_set: XmlSchemaWildcardNsPtr, /* The list of allowed namespaces */
    pub(crate) neg_ns_set: XmlSchemaWildcardNsPtr, /* The negated namespace */
    pub(crate) flags: i32,
}

/// The attribute wildcard has been built.
pub(crate) const XML_SCHEMAS_ATTRGROUP_WILDCARD_BUILDED: i32 = 1 << 0;
/// The attribute group has been defined.
pub(crate) const XML_SCHEMAS_ATTRGROUP_GLOBAL: i32 = 1 << 1;
/// Marks the attr group as marked; used for circular checks.
pub(crate) const XML_SCHEMAS_ATTRGROUP_MARKED: i32 = 1 << 2;

/// The attr group was redefined.
pub(crate) const XML_SCHEMAS_ATTRGROUP_REDEFINED: i32 = 1 << 3;
/// Whether this attr. group contains attr. group references.
pub(crate) const XML_SCHEMAS_ATTRGROUP_HAS_REFS: i32 = 1 << 4;

pub type XmlSchemaTypeLinkPtr = *mut XmlSchemaTypeLink;
/// Used to build a list of types (e.g. member types of
/// simpleType with variety "union").
#[doc(alias = "xmlSchemaTypeLink")]
#[repr(C)]
pub struct XmlSchemaTypeLink {
    pub(crate) next: *mut XmlSchemaTypeLink, /* the next type link ... */
    pub(crate) typ: XmlSchemaTypePtr,        /* the linked type */
}

pub type XmlSchemaFacetLinkPtr = *mut XmlSchemaFacetLink;
/// Used to build a list of facets.
#[doc(alias = "xmlSchemaFacetLink")]
#[repr(C)]
pub struct XmlSchemaFacetLink {
    pub(crate) next: *mut XmlSchemaFacetLink, /* the next facet link ... */
    pub(crate) facet: XmlSchemaFacetPtr,      /* the linked facet */
}

/// The element content type is mixed
pub(crate) const XML_SCHEMAS_TYPE_MIXED: i32 = 1 << 0;
/// The simple or complex type has a derivation method of "extension".
pub(crate) const XML_SCHEMAS_TYPE_DERIVATION_METHOD_EXTENSION: i32 = 1 << 1;
/// The simple or complex type has a derivation method of "restriction".
pub(crate) const XML_SCHEMAS_TYPE_DERIVATION_METHOD_RESTRICTION: i32 = 1 << 2;
/// The type is global
pub(crate) const XML_SCHEMAS_TYPE_GLOBAL: i32 = 1 << 3;
// /// The complexType owns an attribute wildcard, i.e.
// /// it can be freed by the complexType
// const XML_SCHEMAS_TYPE_OWNED_ATTR_WILDCARD: i32 =    1 << 4 /* Obsolete. */;
// /// The simpleType has a variety of "absent".
// /// TODO: Actually not necessary :-/, since if
// /// none of the variety flags occur then it's
// /// automatically absent.
// const XML_SCHEMAS_TYPE_VARIETY_ABSENT: i32 = 1 << 5;
/// The simpleType has a variety of "list".
pub(crate) const XML_SCHEMAS_TYPE_VARIETY_LIST: i32 = 1 << 6;
/// The simpleType has a variety of "union".
pub(crate) const XML_SCHEMAS_TYPE_VARIETY_UNION: i32 = 1 << 7;
/// The simpleType has a variety of "union".
pub(crate) const XML_SCHEMAS_TYPE_VARIETY_ATOMIC: i32 = 1 << 8;
/// The complexType has a final of "extension".
pub(crate) const XML_SCHEMAS_TYPE_FINAL_EXTENSION: i32 = 1 << 9;
/// The simpleType/complexType has a final of "restriction".
pub(crate) const XML_SCHEMAS_TYPE_FINAL_RESTRICTION: i32 = 1 << 10;
/// The simpleType has a final of "list".
pub(crate) const XML_SCHEMAS_TYPE_FINAL_LIST: i32 = 1 << 11;
/// The simpleType has a final of "union".
pub(crate) const XML_SCHEMAS_TYPE_FINAL_UNION: i32 = 1 << 12;
// /// The simpleType has a final of "default".
// const XML_SCHEMAS_TYPE_FINAL_DEFAULT: i32 = 1 << 13;
/// Marks the item as a builtin primitive.
pub(crate) const XML_SCHEMAS_TYPE_BUILTIN_PRIMITIVE: i32 = 1 << 14;
/// Marks the item as marked; used for circular checks.
pub(crate) const XML_SCHEMAS_TYPE_MARKED: i32 = 1 << 16;
// /// The complexType did not specify 'block' so use the default of the <schema> item.
// const XML_SCHEMAS_TYPE_BLOCK_DEFAULT: i32 = 1 << 17;
/// The complexType has a 'block' of "extension".
pub(crate) const XML_SCHEMAS_TYPE_BLOCK_EXTENSION: i32 = 1 << 18;
/// The complexType has a 'block' of "restriction".
pub(crate) const XML_SCHEMAS_TYPE_BLOCK_RESTRICTION: i32 = 1 << 19;
/// The simple/complexType is abstract.
pub(crate) const XML_SCHEMAS_TYPE_ABSTRACT: i32 = 1 << 20;
/// Indicates if the facets need a computed value
pub(crate) const XML_SCHEMAS_TYPE_FACETSNEEDVALUE: i32 = 1 << 21;
/// Indicates that the type was typefixed
pub(crate) const XML_SCHEMAS_TYPE_INTERNAL_RESOLVED: i32 = 1 << 22;
/// Indicates that the type is invalid
pub(crate) const XML_SCHEMAS_TYPE_INTERNAL_INVALID: i32 = 1 << 23;
/// A whitespace-facet value of "preserve"
pub(crate) const XML_SCHEMAS_TYPE_WHITESPACE_PRESERVE: i32 = 1 << 24;
/// A whitespace-facet value of "replace"
pub(crate) const XML_SCHEMAS_TYPE_WHITESPACE_REPLACE: i32 = 1 << 25;
/// A whitespace-facet value of "collapse"
pub(crate) const XML_SCHEMAS_TYPE_WHITESPACE_COLLAPSE: i32 = 1 << 26;
/// Has facets
pub(crate) const XML_SCHEMAS_TYPE_HAS_FACETS: i32 = 1 << 27;
/// Indicates if the facets (pattern) need a normalized value
pub(crate) const XML_SCHEMAS_TYPE_NORMVALUENEEDED: i32 = 1 << 28;

/// First stage of fixup was done.
pub(crate) const XML_SCHEMAS_TYPE_FIXUP_1: i32 = 1 << 29;

/// The type was redefined.
pub(crate) const XML_SCHEMAS_TYPE_REDEFINED: i32 = 1 << 30;
// The type redefines an other type.
// #define XML_SCHEMAS_TYPE_REDEFINING    1 << 31

/// The element is nillable
pub(crate) const XML_SCHEMAS_ELEM_NILLABLE: i32 = 1 << 0;
/// The element is global
pub(crate) const XML_SCHEMAS_ELEM_GLOBAL: i32 = 1 << 1;
/// The element has a default value
pub(crate) const XML_SCHEMAS_ELEM_DEFAULT: i32 = 1 << 2;
/// The element has a fixed value
pub(crate) const XML_SCHEMAS_ELEM_FIXED: i32 = 1 << 3;
/// The element is abstract
pub(crate) const XML_SCHEMAS_ELEM_ABSTRACT: i32 = 1 << 4;
/// The element is top level
/// obsolete: use XML_SCHEMAS_ELEM_GLOBAL instead
pub(crate) const XML_SCHEMAS_ELEM_TOPLEVEL: i32 = 1 << 5;
// /// The element is a reference to a type
// const XML_SCHEMAS_ELEM_REF: i32 = 1 << 6;
// /// Allow elements in no namespace
// /// Obsolete, not used anymore.
// const XML_SCHEMAS_ELEM_NSDEFAULT: i32 = 1 << 7;
/// This is set when "type", "ref", "substitutionGroup"
/// References have been resolved.
pub(crate) const XML_SCHEMAS_ELEM_INTERNAL_RESOLVED: i32 = 1 << 8;
/// A helper flag for the search of circular references.
pub(crate) const XML_SCHEMAS_ELEM_CIRCULAR: i32 = 1 << 9;
// /// The "block" attribute is absent
// const XML_SCHEMAS_ELEM_BLOCK_ABSENT: i32 = 1 << 10;
/// Disallowed substitutions are absent
pub(crate) const XML_SCHEMAS_ELEM_BLOCK_EXTENSION: i32 = 1 << 11;
/// Disallowed substitutions: "restriction"
pub(crate) const XML_SCHEMAS_ELEM_BLOCK_RESTRICTION: i32 = 1 << 12;
/// Disallowed substitutions: "substitution"
pub(crate) const XML_SCHEMAS_ELEM_BLOCK_SUBSTITUTION: i32 = 1 << 13;
// /// Substitution group exclusions are absent
// const XML_SCHEMAS_ELEM_FINAL_ABSENT: i32 = 1 << 14;
/// Substitution group exclusions: "extension"
pub(crate) const XML_SCHEMAS_ELEM_FINAL_EXTENSION: i32 = 1 << 15;
/// Substitution group exclusions: "restriction"
pub(crate) const XML_SCHEMAS_ELEM_FINAL_RESTRICTION: i32 = 1 << 16;
/// The declaration is a substitution group head
pub(crate) const XML_SCHEMAS_ELEM_SUBST_GROUP_HEAD: i32 = 1 << 17;
/// This is set when the elem decl has been checked against all constraints
pub(crate) const XML_SCHEMAS_ELEM_INTERNAL_CHECKED: i32 = 1 << 18;

// /// Unknown facet handling
// const XML_SCHEMAS_FACET_UNKNOWN: i32 = 0;
/// Preserve the type of the facet
pub(crate) const XML_SCHEMAS_FACET_PRESERVE: i32 = 1;
/// Replace the type of the facet
pub(crate) const XML_SCHEMAS_FACET_REPLACE: i32 = 2;
/// Collapse the types of the facet
pub(crate) const XML_SCHEMAS_FACET_COLLAPSE: i32 = 3;
/// A facet definition.
pub type XmlSchemaFacetPtr = *mut XmlSchemaFacet;
#[repr(C)]
pub struct XmlSchemaFacet {
    pub(crate) typ: XmlSchemaTypeType,    /* The kind of type */
    pub(crate) next: *mut XmlSchemaFacet, /* the next type if in a sequence ... */
    pub(crate) value: *const u8,          /* The original value */
    pub(crate) id: *const u8,             /* Obsolete */
    pub(crate) annot: XmlSchemaAnnotPtr,
    pub(crate) node: Option<XmlNodePtr>,
    pub(crate) fixed: i32, /* XML_SCHEMAS_FACET_PRESERVE, etc. */
    pub(crate) whitespace: i32,
    pub(crate) val: XmlSchemaValPtr,          /* The compiled value */
    pub(crate) regexp: Option<Rc<XmlRegexp>>, /* The regex for patterns */
}

// TODO: Actually all those flags used for the schema should sit
// on the schema parser context, since they are used only
// during parsing an XML schema document, and not available
// on the component level as per spec.
/// Reflects elementFormDefault == qualified in
/// an XML schema document.
pub(crate) const XML_SCHEMAS_QUALIF_ELEM: i32 = 1 << 0;
/// Reflects attributeFormDefault == qualified in
/// an XML schema document.
pub(crate) const XML_SCHEMAS_QUALIF_ATTR: i32 = 1 << 1;
/// The schema has "extension" in the set of finalDefault.
pub(crate) const XML_SCHEMAS_FINAL_DEFAULT_EXTENSION: i32 = 1 << 2;
/// The schema has "restriction" in the set of finalDefault.
pub(crate) const XML_SCHEMAS_FINAL_DEFAULT_RESTRICTION: i32 = 1 << 3;
/// The schema has "list" in the set of finalDefault.
pub(crate) const XML_SCHEMAS_FINAL_DEFAULT_LIST: i32 = 1 << 4;
/// The schema has "union" in the set of finalDefault.
pub(crate) const XML_SCHEMAS_FINAL_DEFAULT_UNION: i32 = 1 << 5;
/// The schema has "extension" in the set of blockDefault.
pub(crate) const XML_SCHEMAS_BLOCK_DEFAULT_EXTENSION: i32 = 1 << 6;
/// The schema has "restriction" in the set of blockDefault.
pub(crate) const XML_SCHEMAS_BLOCK_DEFAULT_RESTRICTION: i32 = 1 << 7;
/// The schema has "substitution" in the set of blockDefault.
pub(crate) const XML_SCHEMAS_BLOCK_DEFAULT_SUBSTITUTION: i32 = 1 << 8;
/// The schema is currently including an other schema with no target namespace.
pub(crate) const XML_SCHEMAS_INCLUDING_CONVERT_NS: i32 = 1 << 9;

/// Deallocate a annotation structure
#[doc(alias = "xmlSchemaFreeAnnot")]
pub(crate) unsafe fn xml_schema_free_annot(mut annot: XmlSchemaAnnotPtr) {
    unsafe {
        if annot.is_null() {
            return;
        }
        if (*annot).next.is_null() {
            xml_free(annot as _);
        } else {
            let mut prev: XmlSchemaAnnotPtr;

            while {
                prev = annot;
                annot = (*annot).next;
                xml_free(prev as _);
                !annot.is_null()
            } {}
        }
    }
}

/// Deallocate a list of types.
#[doc(alias = "xmlSchemaFreeTypeLinkList")]
unsafe fn xml_schema_free_type_link_list(mut link: XmlSchemaTypeLinkPtr) {
    unsafe {
        let mut next: XmlSchemaTypeLinkPtr;

        while !link.is_null() {
            next = (*link).next;
            xml_free(link as _);
            link = next;
        }
    }
}

/// Deallocate a Schema Type structure.
#[doc(alias = "xmlSchemaFreeType")]
pub unsafe fn xml_schema_free_type(typ: XmlSchemaTypePtr) {
    unsafe {
        if typ.is_null() {
            return;
        }
        if !(*typ).annot.is_null() {
            xml_schema_free_annot((*typ).annot);
        }
        if !(*typ).facets.is_null() {
            let mut facet: XmlSchemaFacetPtr;
            let mut next: XmlSchemaFacetPtr;

            facet = (*typ).facets;
            while !facet.is_null() {
                next = (*facet).next;
                xml_schema_free_facet(facet);
                facet = next;
            }
        }
        if !(*typ).attr_uses.is_null() {
            xml_schema_item_list_free((*typ).attr_uses as XmlSchemaItemListPtr<*mut c_void>);
        }
        if !(*typ).member_types.is_null() {
            xml_schema_free_type_link_list((*typ).member_types);
        }
        if !(*typ).facet_set.is_null() {
            let mut next: XmlSchemaFacetLinkPtr;
            let mut link: XmlSchemaFacetLinkPtr;

            link = (*typ).facet_set;
            while {
                next = (*link).next;
                xml_free(link as _);
                link = next;
                !link.is_null()
            } {}
        }
        (*typ).cont_model.take();
        drop_in_place(typ);
        xml_free(typ as _);
    }
}

/// Deallocates a list of wildcard constraint structures.
#[doc(alias = "xmlSchemaFreeWildcardNsSet")]
pub(crate) unsafe fn xml_schema_free_wildcard_ns_set(mut set: XmlSchemaWildcardNsPtr) {
    unsafe {
        let mut next: XmlSchemaWildcardNsPtr;

        while !set.is_null() {
            next = (*set).next;
            xml_free(set as _);
            set = next;
        }
    }
}

/// Deallocates a wildcard structure.
#[doc(alias = "xmlSchemaFreeWildcard")]
pub unsafe fn xml_schema_free_wildcard(wildcard: XmlSchemaWildcardPtr) {
    unsafe {
        if wildcard.is_null() {
            return;
        }
        if !(*wildcard).annot.is_null() {
            xml_schema_free_annot((*wildcard).annot);
        }
        if !(*wildcard).ns_set.is_null() {
            xml_schema_free_wildcard_ns_set((*wildcard).ns_set);
        }
        if !(*wildcard).neg_ns_set.is_null() {
            xml_free((*wildcard).neg_ns_set as _);
        }
        xml_free(wildcard as _);
    }
}
