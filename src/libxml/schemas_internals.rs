//! Provide internal methods and data structures for parsing XML Schemas.  
//! This module is based on `libxml/schemasInternals.h`, `xmlschemas.c`, `xmlschemastypes.c` and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{any::type_name, os::raw::c_void};

use super::{
    globals::xml_free,
    tree::XmlNodePtr,
    xmlregexp::{xml_reg_free_regexp, XmlRegexpPtr},
    xmlschemas::XmlSchemaItemListPtr,
    xmlschemastypes::{xml_schema_free_facet, XmlSchemaValPtr},
    xmlstring::XmlChar,
};

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlSchemaValType {
    XmlSchemasUnknown = 0,
    XmlSchemasString = 1,
    XmlSchemasNormstring = 2,
    XmlSchemasDecimal = 3,
    XmlSchemasTime = 4,
    XmlSchemasGday = 5,
    XmlSchemasGmonth = 6,
    XmlSchemasGmonthday = 7,
    XmlSchemasGyear = 8,
    XmlSchemasGyearmonth = 9,
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
    XmlSchemasQname = 21,
    XmlSchemasNcname = 22,
    XmlSchemasId = 23,
    XmlSchemasIdref = 24,
    XmlSchemasIdrefs = 25,
    XmlSchemasEntity = 26,
    XmlSchemasEntities = 27,
    XmlSchemasNotation = 28,
    XmlSchemasAnyuri = 29,
    XmlSchemasInteger = 30,
    XmlSchemasNpinteger = 31,
    XmlSchemasNinteger = 32,
    XmlSchemasNninteger = 33,
    XmlSchemasPinteger = 34,
    XmlSchemasInt = 35,
    XmlSchemasUint = 36,
    XmlSchemasLong = 37,
    XmlSchemasUlong = 38,
    XmlSchemasShort = 39,
    XmlSchemasUshort = 40,
    XmlSchemasByte = 41,
    XmlSchemasUbyte = 42,
    XmlSchemasHexbinary = 43,
    XmlSchemasBase64binary = 44,
    XmlSchemasAnytype = 45,
    XmlSchemasAnysimpletype = 46,
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
        } else if value == Self::XmlSchemasNormstring as i32 {
            Ok(Self::XmlSchemasNormstring)
        } else if value == Self::XmlSchemasDecimal as i32 {
            Ok(Self::XmlSchemasDecimal)
        } else if value == Self::XmlSchemasTime as i32 {
            Ok(Self::XmlSchemasTime)
        } else if value == Self::XmlSchemasGday as i32 {
            Ok(Self::XmlSchemasGday)
        } else if value == Self::XmlSchemasGmonth as i32 {
            Ok(Self::XmlSchemasGmonth)
        } else if value == Self::XmlSchemasGmonthday as i32 {
            Ok(Self::XmlSchemasGmonthday)
        } else if value == Self::XmlSchemasGyear as i32 {
            Ok(Self::XmlSchemasGyear)
        } else if value == Self::XmlSchemasGyearmonth as i32 {
            Ok(Self::XmlSchemasGyearmonth)
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
        } else if value == Self::XmlSchemasQname as i32 {
            Ok(Self::XmlSchemasQname)
        } else if value == Self::XmlSchemasNcname as i32 {
            Ok(Self::XmlSchemasNcname)
        } else if value == Self::XmlSchemasId as i32 {
            Ok(Self::XmlSchemasId)
        } else if value == Self::XmlSchemasIdref as i32 {
            Ok(Self::XmlSchemasIdref)
        } else if value == Self::XmlSchemasIdrefs as i32 {
            Ok(Self::XmlSchemasIdrefs)
        } else if value == Self::XmlSchemasEntity as i32 {
            Ok(Self::XmlSchemasEntity)
        } else if value == Self::XmlSchemasEntities as i32 {
            Ok(Self::XmlSchemasEntities)
        } else if value == Self::XmlSchemasNotation as i32 {
            Ok(Self::XmlSchemasNotation)
        } else if value == Self::XmlSchemasAnyuri as i32 {
            Ok(Self::XmlSchemasAnyuri)
        } else if value == Self::XmlSchemasInteger as i32 {
            Ok(Self::XmlSchemasInteger)
        } else if value == Self::XmlSchemasNpinteger as i32 {
            Ok(Self::XmlSchemasNpinteger)
        } else if value == Self::XmlSchemasNinteger as i32 {
            Ok(Self::XmlSchemasNinteger)
        } else if value == Self::XmlSchemasNninteger as i32 {
            Ok(Self::XmlSchemasNninteger)
        } else if value == Self::XmlSchemasPinteger as i32 {
            Ok(Self::XmlSchemasPinteger)
        } else if value == Self::XmlSchemasInt as i32 {
            Ok(Self::XmlSchemasInt)
        } else if value == Self::XmlSchemasUint as i32 {
            Ok(Self::XmlSchemasUint)
        } else if value == Self::XmlSchemasLong as i32 {
            Ok(Self::XmlSchemasLong)
        } else if value == Self::XmlSchemasUlong as i32 {
            Ok(Self::XmlSchemasUlong)
        } else if value == Self::XmlSchemasShort as i32 {
            Ok(Self::XmlSchemasShort)
        } else if value == Self::XmlSchemasUshort as i32 {
            Ok(Self::XmlSchemasUshort)
        } else if value == Self::XmlSchemasByte as i32 {
            Ok(Self::XmlSchemasByte)
        } else if value == Self::XmlSchemasUbyte as i32 {
            Ok(Self::XmlSchemasUbyte)
        } else if value == Self::XmlSchemasHexbinary as i32 {
            Ok(Self::XmlSchemasHexbinary)
        } else if value == Self::XmlSchemasBase64binary as i32 {
            Ok(Self::XmlSchemasBase64binary)
        } else if value == Self::XmlSchemasAnytype as i32 {
            Ok(Self::XmlSchemasAnytype)
        } else if value == Self::XmlSchemasAnysimpletype as i32 {
            Ok(Self::XmlSchemasAnysimpletype)
        } else {
            Err(anyhow::anyhow!(
                "Invalid convert from value '{value}' to {}",
                type_name::<Self>()
            ))
        }
    }
}

/*
 * XML Schemas defines multiple type of types.
 */
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
    XmlSchemaTypeAttributegroup,
    XmlSchemaTypeGroup,
    XmlSchemaTypeNotation,
    XmlSchemaTypeList,
    XmlSchemaTypeUnion,
    XmlSchemaTypeAnyAttribute,
    XmlSchemaTypeIdcUnique,
    XmlSchemaTypeIdcKey,
    XmlSchemaTypeIdcKeyref,
    XmlSchemaTypeParticle = 25,
    XmlSchemaTypeAttributeUse,
    XmlSchemaFacetMininclusive = 1000,
    XmlSchemaFacetMinexclusive,
    XmlSchemaFacetMaxinclusive,
    XmlSchemaFacetMaxexclusive,
    XmlSchemaFacetTotaldigits,
    XmlSchemaFacetFractiondigits,
    XmlSchemaFacetPattern,
    XmlSchemaFacetEnumeration,
    XmlSchemaFacetWhitespace,
    XmlSchemaFacetLength,
    XmlSchemaFacetMaxlength,
    XmlSchemaFacetMinlength,
    XmlSchemaExtraQnameref = 2000,
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

/**
 * Annotation
 */
pub type XmlSchemaAnnotPtr = *mut XmlSchemaAnnot;
#[repr(C)]
pub struct XmlSchemaAnnot {
    pub(crate) next: *mut XmlSchemaAnnot,
    pub(crate) content: XmlNodePtr, /* the annotation */
}

/**
 * XML_SCHEMAS_ANYATTR_SKIP:
 *
 * Skip unknown attribute from validation
 * Obsolete, not used anymore.
 */
const XML_SCHEMAS_ANYATTR_SKIP: i32 = 1;
/**
 * XML_SCHEMAS_ANYATTR_LAX:
 *
 * Ignore validation non definition on attributes
 * Obsolete, not used anymore.
 */
const XML_SCHEMAS_ANYATTR_LAX: i32 = 2;
/**
 * XML_SCHEMAS_ANYATTR_STRICT:
 *
 * Apply strict validation rules on attributes
 * Obsolete, not used anymore.
 */
const XML_SCHEMAS_ANYATTR_STRICT: i32 = 3;
/**
 * XML_SCHEMAS_ANY_SKIP:
 *
 * Skip unknown attribute from validation
 */
pub(crate) const XML_SCHEMAS_ANY_SKIP: i32 = 1;
/**
 * XML_SCHEMAS_ANY_LAX:
 *
 * Used by wildcards.
 * Validate if type found, don't worry if not found
 */
pub(crate) const XML_SCHEMAS_ANY_LAX: i32 = 2;
/**
 * XML_SCHEMAS_ANY_STRICT:
 *
 * Used by wildcards.
 * Apply strict validation rules
 */
pub(crate) const XML_SCHEMAS_ANY_STRICT: i32 = 3;
/**
 * XML_SCHEMAS_ATTR_USE_PROHIBITED:
 *
 * Used by wildcards.
 * The attribute is prohibited.
 */
pub(crate) const XML_SCHEMAS_ATTR_USE_PROHIBITED: i32 = 0;
/**
 * XML_SCHEMAS_ATTR_USE_REQUIRED:
 *
 * The attribute is required.
 */
pub(crate) const XML_SCHEMAS_ATTR_USE_REQUIRED: i32 = 1;
/**
 * XML_SCHEMAS_ATTR_USE_OPTIONAL:
 *
 * The attribute is optional.
 */
pub(crate) const XML_SCHEMAS_ATTR_USE_OPTIONAL: i32 = 2;
/**
 * XML_SCHEMAS_ATTR_GLOBAL:
 *
 * allow elements in no namespace
 */
pub(crate) const XML_SCHEMAS_ATTR_GLOBAL: i32 = 1 << 0;
/**
 * XML_SCHEMAS_ATTR_NSDEFAULT:
 *
 * allow elements in no namespace
 */
pub(crate) const XML_SCHEMAS_ATTR_NSDEFAULT: i32 = 1 << 7;
/**
 * XML_SCHEMAS_ATTR_INTERNAL_RESOLVED:
 *
 * this is set when the "type" and "ref" references
 * have been resolved.
 */
pub(crate) const XML_SCHEMAS_ATTR_INTERNAL_RESOLVED: i32 = 1 << 8;
/**
 * XML_SCHEMAS_ATTR_FIXED:
 *
 * the attribute has a fixed value
 */
pub(crate) const XML_SCHEMAS_ATTR_FIXED: i32 = 1 << 9;

/**
 * xmlSchemaAttribute:
 * An attribute definition.
 */

pub type XmlSchemaAttributePtr = *mut XmlSchemaAttribute;
#[repr(C)]
pub struct XmlSchemaAttribute {
    pub(crate) typ: XmlSchemaTypeType,
    pub(crate) next: *mut XmlSchemaAttribute, /* the next attribute (not used?) */
    pub(crate) name: *const XmlChar,          /* the name of the declaration */
    pub(crate) id: *const XmlChar,            /* Deprecated; not used */
    pub(crate) refe: *const XmlChar,          /* Deprecated; not used */
    pub(crate) ref_ns: *const XmlChar,        /* Deprecated; not used */
    pub(crate) type_name: *const XmlChar,     /* the local name of the type definition */
    pub(crate) type_ns: *const XmlChar,       /* the ns URI of the type definition */
    pub(crate) annot: XmlSchemaAnnotPtr,

    pub(crate) base: XmlSchemaTypePtr,    /* Deprecated; not used */
    pub(crate) occurs: i32,               /* Deprecated; not used */
    pub(crate) def_value: *const XmlChar, /* The initial value of the value constraint */
    pub(crate) subtypes: XmlSchemaTypePtr, /* the type definition */
    pub(crate) node: XmlNodePtr,
    pub(crate) target_namespace: *const XmlChar,
    pub(crate) flags: i32,
    pub(crate) ref_prefix: *const XmlChar, /* Deprecated; not used */
    pub(crate) def_val: XmlSchemaValPtr,   /* The compiled value constraint */
    pub(crate) ref_decl: XmlSchemaAttributePtr, /* Deprecated; not used */
}

/**
 * xmlSchemaAttributeLink:
 * Used to build a list of attribute uses on complexType definitions.
 * WARNING: Deprecated; not used.
 */
pub type XmlSchemaAttributeLinkPtr = *mut XmlSchemaAttributeLink;
#[repr(C)]
pub struct XmlSchemaAttributeLink {
    next: *mut XmlSchemaAttributeLink, /* the next attribute link ... */
    attr: *mut XmlSchemaAttribute,     /* the linked attribute */
}

/**
 * XML_SCHEMAS_WILDCARD_COMPLETE:
 *
 * If the wildcard is complete.
 */
const XML_SCHEMAS_WILDCARD_COMPLETE: i32 = 1 << 0;

/**
 * xmlSchemaCharValueLink:
 * Used to build a list of namespaces on wildcards.
 */
pub type XmlSchemaWildcardNsPtr = *mut XmlSchemaWildcardNs;
#[repr(C)]
pub struct XmlSchemaWildcardNs {
    pub(crate) next: *mut XmlSchemaWildcardNs, /* the next constraint link ... */
    pub(crate) value: *const XmlChar,          /* the value */
}

/**
 * xmlSchemaWildcard.
 * A wildcard.
 */
pub type XmlSchemaWildcardPtr = *mut XmlSchemaWildcard;
#[repr(C)]
pub struct XmlSchemaWildcard {
    pub(crate) typ: XmlSchemaTypeType, /* The kind of type */
    pub(crate) id: *const XmlChar,     /* Deprecated; not used */
    pub(crate) annot: XmlSchemaAnnotPtr,
    pub(crate) node: XmlNodePtr,
    pub(crate) min_occurs: i32, /* Deprecated; not used */
    pub(crate) max_occurs: i32, /* Deprecated; not used */
    pub(crate) process_contents: i32,
    pub(crate) any: i32, /* Indicates if the ns constraint is of ##any */
    pub(crate) ns_set: XmlSchemaWildcardNsPtr, /* The list of allowed namespaces */
    pub(crate) neg_ns_set: XmlSchemaWildcardNsPtr, /* The negated namespace */
    pub(crate) flags: i32,
}

/**
 * XML_SCHEMAS_ATTRGROUP_WILDCARD_BUILDED:
 *
 * The attribute wildcard has been built.
 */
pub(crate) const XML_SCHEMAS_ATTRGROUP_WILDCARD_BUILDED: i32 = 1 << 0;
/**
 * XML_SCHEMAS_ATTRGROUP_GLOBAL:
 *
 * The attribute group has been defined.
 */
pub(crate) const XML_SCHEMAS_ATTRGROUP_GLOBAL: i32 = 1 << 1;
/**
 * XML_SCHEMAS_ATTRGROUP_MARKED:
 *
 * Marks the attr group as marked; used for circular checks.
 */
pub(crate) const XML_SCHEMAS_ATTRGROUP_MARKED: i32 = 1 << 2;

/**
 * XML_SCHEMAS_ATTRGROUP_REDEFINED:
 *
 * The attr group was redefined.
 */
pub(crate) const XML_SCHEMAS_ATTRGROUP_REDEFINED: i32 = 1 << 3;
/**
 * XML_SCHEMAS_ATTRGROUP_HAS_REFS:
 *
 * Whether this attr. group contains attr. group references.
 */
pub(crate) const XML_SCHEMAS_ATTRGROUP_HAS_REFS: i32 = 1 << 4;

/**
 * An attribute group definition.
 *
 * xmlSchemaAttribute and xmlSchemaAttributeGroup start of structures
 * must be kept similar
 */
pub type XmlSchemaAttributeGroupPtr = *mut XmlSchemaAttributeGroup;
#[repr(C)]
pub struct XmlSchemaAttributeGroup {
    pub(crate) typ: XmlSchemaTypeType,        /* The kind of type */
    pub(crate) next: *mut XmlSchemaAttribute, /* the next attribute if in a group ... */
    pub(crate) name: *mut XmlChar,
    pub(crate) id: *const XmlChar,
    pub(crate) refe: *const XmlChar,   /* Deprecated; not used */
    pub(crate) ref_ns: *const XmlChar, /* Deprecated; not used */
    pub(crate) annot: XmlSchemaAnnotPtr,

    pub(crate) attributes: XmlSchemaAttributePtr, /* Deprecated; not used */
    pub(crate) node: XmlNodePtr,
    pub(crate) flags: i32,
    pub(crate) attribute_wildcard: XmlSchemaWildcardPtr,
    pub(crate) ref_prefix: *const XmlChar, /* Deprecated; not used */
    pub(crate) ref_item: XmlSchemaAttributeGroupPtr, /* Deprecated; not used */
    pub(crate) target_namespace: *const XmlChar,
    pub(crate) attr_uses: *mut c_void,
}

/**
 * xmlSchemaTypeLink:
 * Used to build a list of types (e.g. member types of
 * simpleType with variety "union").
 */
pub type XmlSchemaTypeLinkPtr = *mut XmlSchemaTypeLink;
#[repr(C)]
pub struct XmlSchemaTypeLink {
    pub(crate) next: *mut XmlSchemaTypeLink, /* the next type link ... */
    pub(crate) typ: XmlSchemaTypePtr,        /* the linked type */
}

/**
 * xmlSchemaFacetLink:
 * Used to build a list of facets.
 */
pub type XmlSchemaFacetLinkPtr = *mut XmlSchemaFacetLink;
#[repr(C)]
pub struct XmlSchemaFacetLink {
    pub(crate) next: *mut XmlSchemaFacetLink, /* the next facet link ... */
    pub(crate) facet: XmlSchemaFacetPtr,      /* the linked facet */
}

/**
 * XML_SCHEMAS_TYPE_MIXED:
 *
 * the element content type is mixed
 */
pub(crate) const XML_SCHEMAS_TYPE_MIXED: i32 = 1 << 0;
/**
 * XML_SCHEMAS_TYPE_DERIVATION_METHOD_EXTENSION:
 *
 * the simple or complex type has a derivation method of "extension".
 */
pub(crate) const XML_SCHEMAS_TYPE_DERIVATION_METHOD_EXTENSION: i32 = 1 << 1;
/**
 * XML_SCHEMAS_TYPE_DERIVATION_METHOD_RESTRICTION:
 *
 * the simple or complex type has a derivation method of "restriction".
 */
pub(crate) const XML_SCHEMAS_TYPE_DERIVATION_METHOD_RESTRICTION: i32 = 1 << 2;
/**
 * XML_SCHEMAS_TYPE_GLOBAL:
 *
 * the type is global
 */
pub(crate) const XML_SCHEMAS_TYPE_GLOBAL: i32 = 1 << 3;
/**
 * XML_SCHEMAS_TYPE_OWNED_ATTR_WILDCARD:
 *
 * the complexType owns an attribute wildcard, i.e.
 * it can be freed by the complexType
 */
const XML_SCHEMAS_TYPE_OWNED_ATTR_WILDCARD: i32 =    1 << 4 /* Obsolete. */;
/**
 * XML_SCHEMAS_TYPE_VARIETY_ABSENT:
 *
 * the simpleType has a variety of "absent".
 * TODO: Actually not necessary :-/, since if
 * none of the variety flags occur then it's
 * automatically absent.
 */
const XML_SCHEMAS_TYPE_VARIETY_ABSENT: i32 = 1 << 5;
/**
 * XML_SCHEMAS_TYPE_VARIETY_LIST:
 *
 * the simpleType has a variety of "list".
 */
pub(crate) const XML_SCHEMAS_TYPE_VARIETY_LIST: i32 = 1 << 6;
/**
 * XML_SCHEMAS_TYPE_VARIETY_UNION:
 *
 * the simpleType has a variety of "union".
 */
pub(crate) const XML_SCHEMAS_TYPE_VARIETY_UNION: i32 = 1 << 7;
/**
 * XML_SCHEMAS_TYPE_VARIETY_ATOMIC:
 *
 * the simpleType has a variety of "union".
 */
pub(crate) const XML_SCHEMAS_TYPE_VARIETY_ATOMIC: i32 = 1 << 8;
/**
 * XML_SCHEMAS_TYPE_FINAL_EXTENSION:
 *
 * the complexType has a final of "extension".
 */
pub(crate) const XML_SCHEMAS_TYPE_FINAL_EXTENSION: i32 = 1 << 9;
/**
 * XML_SCHEMAS_TYPE_FINAL_RESTRICTION:
 *
 * the simpleType/complexType has a final of "restriction".
 */
pub(crate) const XML_SCHEMAS_TYPE_FINAL_RESTRICTION: i32 = 1 << 10;
/**
 * XML_SCHEMAS_TYPE_FINAL_LIST:
 *
 * the simpleType has a final of "list".
 */
pub(crate) const XML_SCHEMAS_TYPE_FINAL_LIST: i32 = 1 << 11;
/**
 * XML_SCHEMAS_TYPE_FINAL_UNION:
 *
 * the simpleType has a final of "union".
 */
pub(crate) const XML_SCHEMAS_TYPE_FINAL_UNION: i32 = 1 << 12;
/**
 * XML_SCHEMAS_TYPE_FINAL_DEFAULT:
 *
 * the simpleType has a final of "default".
 */
const XML_SCHEMAS_TYPE_FINAL_DEFAULT: i32 = 1 << 13;
/**
 * XML_SCHEMAS_TYPE_BUILTIN_PRIMITIVE:
 *
 * Marks the item as a builtin primitive.
 */
pub(crate) const XML_SCHEMAS_TYPE_BUILTIN_PRIMITIVE: i32 = 1 << 14;
/**
 * XML_SCHEMAS_TYPE_MARKED:
 *
 * Marks the item as marked; used for circular checks.
 */
pub(crate) const XML_SCHEMAS_TYPE_MARKED: i32 = 1 << 16;
/**
 * XML_SCHEMAS_TYPE_BLOCK_DEFAULT:
 *
 * the complexType did not specify 'block' so use the default of the
 * <schema> item.
 */
const XML_SCHEMAS_TYPE_BLOCK_DEFAULT: i32 = 1 << 17;
/**
 * XML_SCHEMAS_TYPE_BLOCK_EXTENSION:
 *
 * the complexType has a 'block' of "extension".
 */
pub(crate) const XML_SCHEMAS_TYPE_BLOCK_EXTENSION: i32 = 1 << 18;
/**
 * XML_SCHEMAS_TYPE_BLOCK_RESTRICTION:
 *
 * the complexType has a 'block' of "restriction".
 */
pub(crate) const XML_SCHEMAS_TYPE_BLOCK_RESTRICTION: i32 = 1 << 19;
/**
 * XML_SCHEMAS_TYPE_ABSTRACT:
 *
 * the simple/complexType is abstract.
 */
pub(crate) const XML_SCHEMAS_TYPE_ABSTRACT: i32 = 1 << 20;
/**
 * XML_SCHEMAS_TYPE_FACETSNEEDVALUE:
 *
 * indicates if the facets need a computed value
 */
pub(crate) const XML_SCHEMAS_TYPE_FACETSNEEDVALUE: i32 = 1 << 21;
/**
 * XML_SCHEMAS_TYPE_INTERNAL_RESOLVED:
 *
 * indicates that the type was typefixed
 */
pub(crate) const XML_SCHEMAS_TYPE_INTERNAL_RESOLVED: i32 = 1 << 22;
/**
 * XML_SCHEMAS_TYPE_INTERNAL_INVALID:
 *
 * indicates that the type is invalid
 */
pub(crate) const XML_SCHEMAS_TYPE_INTERNAL_INVALID: i32 = 1 << 23;
/**
 * XML_SCHEMAS_TYPE_WHITESPACE_PRESERVE:
 *
 * a whitespace-facet value of "preserve"
 */
pub(crate) const XML_SCHEMAS_TYPE_WHITESPACE_PRESERVE: i32 = 1 << 24;
/**
 * XML_SCHEMAS_TYPE_WHITESPACE_REPLACE:
 *
 * a whitespace-facet value of "replace"
 */
pub(crate) const XML_SCHEMAS_TYPE_WHITESPACE_REPLACE: i32 = 1 << 25;
/**
 * XML_SCHEMAS_TYPE_WHITESPACE_COLLAPSE:
 *
 * a whitespace-facet value of "collapse"
 */
pub(crate) const XML_SCHEMAS_TYPE_WHITESPACE_COLLAPSE: i32 = 1 << 26;
/**
 * XML_SCHEMAS_TYPE_HAS_FACETS:
 *
 * has facets
 */
pub(crate) const XML_SCHEMAS_TYPE_HAS_FACETS: i32 = 1 << 27;
/**
 * XML_SCHEMAS_TYPE_NORMVALUENEEDED:
 *
 * indicates if the facets (pattern) need a normalized value
 */
pub(crate) const XML_SCHEMAS_TYPE_NORMVALUENEEDED: i32 = 1 << 28;

/**
 * XML_SCHEMAS_TYPE_FIXUP_1:
 *
 * First stage of fixup was done.
 */
pub(crate) const XML_SCHEMAS_TYPE_FIXUP_1: i32 = 1 << 29;

/**
 * XML_SCHEMAS_TYPE_REDEFINED:
 *
 * The type was redefined.
 */
pub(crate) const XML_SCHEMAS_TYPE_REDEFINED: i32 = 1 << 30;
/**
 * XML_SCHEMAS_TYPE_REDEFINING:
 *
 * The type redefines an other type.
 */
/* #define XML_SCHEMAS_TYPE_REDEFINING    1 << 31 */

/**
 * xmlSchemaType:
 *
 * Schemas type definition.
 */
pub type XmlSchemaTypePtr = *mut XmlSchemaType;
#[repr(C)]
pub struct XmlSchemaType {
    pub(crate) typ: XmlSchemaTypeType,   /* The kind of type */
    pub(crate) next: *mut XmlSchemaType, /* the next type if in a sequence ... */
    pub(crate) name: *const XmlChar,
    pub(crate) id: *const XmlChar,     /* Deprecated; not used */
    pub(crate) refe: *const XmlChar,   /* Deprecated; not used */
    pub(crate) ref_ns: *const XmlChar, /* Deprecated; not used */
    pub(crate) annot: XmlSchemaAnnotPtr,
    pub(crate) subtypes: XmlSchemaTypePtr,
    pub(crate) attributes: XmlSchemaAttributePtr, /* Deprecated; not used */
    pub(crate) node: XmlNodePtr,
    pub(crate) min_occurs: i32, /* Deprecated; not used */
    pub(crate) max_occurs: i32, /* Deprecated; not used */

    pub(crate) flags: i32,
    pub(crate) content_type: XmlSchemaContentType,
    pub(crate) base: *const XmlChar,    /* Base type's local name */
    pub(crate) base_ns: *const XmlChar, /* Base type's target namespace */
    pub(crate) base_type: XmlSchemaTypePtr, /* The base type component */
    pub(crate) facets: XmlSchemaFacetPtr, /* Local facets */
    pub(crate) redef: *mut XmlSchemaType, /* Deprecated; not used */
    pub(crate) recurse: i32,            /* Obsolete */
    pub(crate) attribute_uses: *mut XmlSchemaAttributeLinkPtr, /* Deprecated; not used */
    pub(crate) attribute_wildcard: XmlSchemaWildcardPtr,
    pub(crate) built_in_type: i32, /* Type of built-in types. */
    pub(crate) member_types: XmlSchemaTypeLinkPtr, /* member-types if a union type. */
    pub(crate) facet_set: XmlSchemaFacetLinkPtr, /* All facets (incl. inherited) */
    pub(crate) ref_prefix: *const XmlChar, /* Deprecated; not used */
    pub(crate) content_type_def: XmlSchemaTypePtr, /* Used for the simple content of complex types.
                                   Could we use @subtypes for this? */
    pub(crate) cont_model: XmlRegexpPtr, /* Holds the automaton of the content model */
    pub(crate) target_namespace: *const XmlChar,
    pub(crate) attr_uses: *mut c_void,
}

/*
 * xmlSchemaElement:
 * An element definition.
 *
 * xmlSchemaType, xmlSchemaFacet and xmlSchemaElement start of
 * structures must be kept similar
 */
/**
 * XML_SCHEMAS_ELEM_NILLABLE:
 *
 * the element is nillable
 */
pub(crate) const XML_SCHEMAS_ELEM_NILLABLE: i32 = 1 << 0;
/**
 * XML_SCHEMAS_ELEM_GLOBAL:
 *
 * the element is global
 */
pub(crate) const XML_SCHEMAS_ELEM_GLOBAL: i32 = 1 << 1;
/**
 * XML_SCHEMAS_ELEM_DEFAULT:
 *
 * the element has a default value
 */
pub(crate) const XML_SCHEMAS_ELEM_DEFAULT: i32 = 1 << 2;
/**
 * XML_SCHEMAS_ELEM_FIXED:
 *
 * the element has a fixed value
 */
pub(crate) const XML_SCHEMAS_ELEM_FIXED: i32 = 1 << 3;
/**
 * XML_SCHEMAS_ELEM_ABSTRACT:
 *
 * the element is abstract
 */
pub(crate) const XML_SCHEMAS_ELEM_ABSTRACT: i32 = 1 << 4;
/**
 * XML_SCHEMAS_ELEM_TOPLEVEL:
 *
 * the element is top level
 * obsolete: use XML_SCHEMAS_ELEM_GLOBAL instead
 */
pub(crate) const XML_SCHEMAS_ELEM_TOPLEVEL: i32 = 1 << 5;
/**
 * XML_SCHEMAS_ELEM_REF:
 *
 * the element is a reference to a type
 */
const XML_SCHEMAS_ELEM_REF: i32 = 1 << 6;
/**
 * XML_SCHEMAS_ELEM_NSDEFAULT:
 *
 * allow elements in no namespace
 * Obsolete, not used anymore.
 */
const XML_SCHEMAS_ELEM_NSDEFAULT: i32 = 1 << 7;
/**
 * XML_SCHEMAS_ELEM_INTERNAL_RESOLVED:
 *
 * this is set when "type", "ref", "substitutionGroup"
 * references have been resolved.
 */
pub(crate) const XML_SCHEMAS_ELEM_INTERNAL_RESOLVED: i32 = 1 << 8;
/**
 * XML_SCHEMAS_ELEM_CIRCULAR:
 *
 * a helper flag for the search of circular references.
 */
pub(crate) const XML_SCHEMAS_ELEM_CIRCULAR: i32 = 1 << 9;
/**
 * XML_SCHEMAS_ELEM_BLOCK_ABSENT:
 *
 * the "block" attribute is absent
 */
const XML_SCHEMAS_ELEM_BLOCK_ABSENT: i32 = 1 << 10;
/**
 * XML_SCHEMAS_ELEM_BLOCK_EXTENSION:
 *
 * disallowed substitutions are absent
 */
pub(crate) const XML_SCHEMAS_ELEM_BLOCK_EXTENSION: i32 = 1 << 11;
/**
 * XML_SCHEMAS_ELEM_BLOCK_RESTRICTION:
 *
 * disallowed substitutions: "restriction"
 */
pub(crate) const XML_SCHEMAS_ELEM_BLOCK_RESTRICTION: i32 = 1 << 12;
/**
 * XML_SCHEMAS_ELEM_BLOCK_SUBSTITUTION:
 *
 * disallowed substitutions: "substitution"
 */
pub(crate) const XML_SCHEMAS_ELEM_BLOCK_SUBSTITUTION: i32 = 1 << 13;
/**
 * XML_SCHEMAS_ELEM_FINAL_ABSENT:
 *
 * substitution group exclusions are absent
 */
const XML_SCHEMAS_ELEM_FINAL_ABSENT: i32 = 1 << 14;
/**
 * XML_SCHEMAS_ELEM_FINAL_EXTENSION:
 *
 * substitution group exclusions: "extension"
 */
pub(crate) const XML_SCHEMAS_ELEM_FINAL_EXTENSION: i32 = 1 << 15;
/**
 * XML_SCHEMAS_ELEM_FINAL_RESTRICTION:
 *
 * substitution group exclusions: "restriction"
 */
pub(crate) const XML_SCHEMAS_ELEM_FINAL_RESTRICTION: i32 = 1 << 16;
/**
 * XML_SCHEMAS_ELEM_SUBST_GROUP_HEAD:
 *
 * the declaration is a substitution group head
 */
pub(crate) const XML_SCHEMAS_ELEM_SUBST_GROUP_HEAD: i32 = 1 << 17;
/**
 * XML_SCHEMAS_ELEM_INTERNAL_CHECKED:
 *
 * this is set when the elem decl has been checked against
 * all constraints
 */
pub(crate) const XML_SCHEMAS_ELEM_INTERNAL_CHECKED: i32 = 1 << 18;

pub type XmlSchemaElementPtr = *mut XmlSchemaElement;
#[repr(C)]
pub struct XmlSchemaElement {
    pub(crate) typ: XmlSchemaTypeType,   /* The kind of type */
    pub(crate) next: *mut XmlSchemaType, /* Not used? */
    pub(crate) name: *const XmlChar,
    pub(crate) id: *const XmlChar,     /* Deprecated; not used */
    pub(crate) refe: *const XmlChar,   /* Deprecated; not used */
    pub(crate) ref_ns: *const XmlChar, /* Deprecated; not used */
    pub(crate) annot: XmlSchemaAnnotPtr,
    pub(crate) subtypes: XmlSchemaTypePtr, /* the type definition */
    pub(crate) attributes: XmlSchemaAttributePtr,
    pub(crate) node: XmlNodePtr,
    pub(crate) min_occurs: i32, /* Deprecated; not used */
    pub(crate) max_occurs: i32, /* Deprecated; not used */

    pub(crate) flags: i32,
    pub(crate) target_namespace: *const XmlChar,
    pub(crate) named_type: *const XmlChar,
    pub(crate) named_type_ns: *const XmlChar,
    pub(crate) subst_group: *const XmlChar,
    pub(crate) subst_group_ns: *const XmlChar,
    pub(crate) scope: *const XmlChar,
    pub(crate) value: *const XmlChar, /* The original value of the value constraint. */
    pub(crate) ref_decl: *mut XmlSchemaElement, /* This will now be used for the
                                      substitution group affiliation */
    pub(crate) cont_model: XmlRegexpPtr, /* Obsolete for WXS, maybe used for RelaxNG */
    pub(crate) content_type: XmlSchemaContentType,
    pub(crate) ref_prefix: *const XmlChar, /* Deprecated; not used */
    pub(crate) def_val: XmlSchemaValPtr,   /* The compiled value constraint. */
    pub(crate) idcs: *mut c_void,          /* The identity-constraint defs */
}

/*
 * XML_SCHEMAS_FACET_UNKNOWN:
 *
 * unknown facet handling
 */
const XML_SCHEMAS_FACET_UNKNOWN: i32 = 0;
/*
 * XML_SCHEMAS_FACET_PRESERVE:
 *
 * preserve the type of the facet
 */
pub(crate) const XML_SCHEMAS_FACET_PRESERVE: i32 = 1;
/*
 * XML_SCHEMAS_FACET_REPLACE:
 *
 * replace the type of the facet
 */
pub(crate) const XML_SCHEMAS_FACET_REPLACE: i32 = 2;
/*
 * XML_SCHEMAS_FACET_COLLAPSE:
 *
 * collapse the types of the facet
 */
pub(crate) const XML_SCHEMAS_FACET_COLLAPSE: i32 = 3;
/**
 * A facet definition.
 */
pub type XmlSchemaFacetPtr = *mut XmlSchemaFacet;
#[repr(C)]
pub struct XmlSchemaFacet {
    pub(crate) typ: XmlSchemaTypeType,    /* The kind of type */
    pub(crate) next: *mut XmlSchemaFacet, /* the next type if in a sequence ... */
    pub(crate) value: *const XmlChar,     /* The original value */
    pub(crate) id: *const XmlChar,        /* Obsolete */
    pub(crate) annot: XmlSchemaAnnotPtr,
    pub(crate) node: XmlNodePtr,
    pub(crate) fixed: i32, /* XML_SCHEMAS_FACET_PRESERVE, etc. */
    pub(crate) whitespace: i32,
    pub(crate) val: XmlSchemaValPtr, /* The compiled value */
    pub(crate) regexp: XmlRegexpPtr, /* The regex for patterns */
}

/**
 * A notation definition.
 */
pub type XmlSchemaNotationPtr = *mut XmlSchemaNotation;
#[repr(C)]
pub struct XmlSchemaNotation {
    pub(crate) typ: XmlSchemaTypeType, /* The kind of type */
    pub(crate) name: *const XmlChar,
    pub(crate) annot: XmlSchemaAnnotPtr,
    pub(crate) identifier: *const XmlChar,
    pub(crate) target_namespace: *const XmlChar,
}

/*
* TODO: Actually all those flags used for the schema should sit
* on the schema parser context, since they are used only
* during parsing an XML schema document, and not available
* on the component level as per spec.
*/
/**
 * XML_SCHEMAS_QUALIF_ELEM:
 *
 * Reflects elementFormDefault == qualified in
 * an XML schema document.
 */
pub(crate) const XML_SCHEMAS_QUALIF_ELEM: i32 = 1 << 0;
/**
 * XML_SCHEMAS_QUALIF_ATTR:
 *
 * Reflects attributeFormDefault == qualified in
 * an XML schema document.
 */
pub(crate) const XML_SCHEMAS_QUALIF_ATTR: i32 = 1 << 1;
/**
 * XML_SCHEMAS_FINAL_DEFAULT_EXTENSION:
 *
 * the schema has "extension" in the set of finalDefault.
 */
pub(crate) const XML_SCHEMAS_FINAL_DEFAULT_EXTENSION: i32 = 1 << 2;
/**
 * XML_SCHEMAS_FINAL_DEFAULT_RESTRICTION:
 *
 * the schema has "restriction" in the set of finalDefault.
 */
pub(crate) const XML_SCHEMAS_FINAL_DEFAULT_RESTRICTION: i32 = 1 << 3;
/**
 * XML_SCHEMAS_FINAL_DEFAULT_LIST:
 *
 * the schema has "list" in the set of finalDefault.
 */
pub(crate) const XML_SCHEMAS_FINAL_DEFAULT_LIST: i32 = 1 << 4;
/**
 * XML_SCHEMAS_FINAL_DEFAULT_UNION:
 *
 * the schema has "union" in the set of finalDefault.
 */
pub(crate) const XML_SCHEMAS_FINAL_DEFAULT_UNION: i32 = 1 << 5;
/**
 * XML_SCHEMAS_BLOCK_DEFAULT_EXTENSION:
 *
 * the schema has "extension" in the set of blockDefault.
 */
pub(crate) const XML_SCHEMAS_BLOCK_DEFAULT_EXTENSION: i32 = 1 << 6;
/**
 * XML_SCHEMAS_BLOCK_DEFAULT_RESTRICTION:
 *
 * the schema has "restriction" in the set of blockDefault.
 */
pub(crate) const XML_SCHEMAS_BLOCK_DEFAULT_RESTRICTION: i32 = 1 << 7;
/**
 * XML_SCHEMAS_BLOCK_DEFAULT_SUBSTITUTION:
 *
 * the schema has "substitution" in the set of blockDefault.
 */
pub(crate) const XML_SCHEMAS_BLOCK_DEFAULT_SUBSTITUTION: i32 = 1 << 8;
/**
 * XML_SCHEMAS_INCLUDING_CONVERT_NS:
 *
 * the schema is currently including an other schema with
 * no target namespace.
 */
pub(crate) const XML_SCHEMAS_INCLUDING_CONVERT_NS: i32 = 1 << 9;

/**
 * xmlSchemaFreeAnnot:
 * @annot:  a schema type structure
 *
 * Deallocate a annotation structure
 */
pub(crate) unsafe extern "C" fn xml_schema_free_annot(mut annot: XmlSchemaAnnotPtr) {
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

/**
 * xmlSchemaItemListFree:
 * @annot:  a schema type structure
 *
 * Deallocate a annotation structure
 */
pub(crate) unsafe extern "C" fn xml_schema_item_list_free(list: XmlSchemaItemListPtr) {
    if list.is_null() {
        return;
    }
    if !(*list).items.is_null() {
        xml_free((*list).items as _);
    }
    xml_free(list as _);
}

/**
 * xmlSchemaFreeTypeLinkList:
 * @alink: a type link
 *
 * Deallocate a list of types.
 */
unsafe extern "C" fn xml_schema_free_type_link_list(mut link: XmlSchemaTypeLinkPtr) {
    let mut next: XmlSchemaTypeLinkPtr;

    while !link.is_null() {
        next = (*link).next;
        xml_free(link as _);
        link = next;
    }
}

/**
 * xmlSchemaFreeType:
 * @type:  a schema type structure
 *
 * Deallocate a Schema Type structure.
 */
pub unsafe extern "C" fn xml_schema_free_type(typ: XmlSchemaTypePtr) {
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
        xml_schema_item_list_free((*typ).attr_uses as XmlSchemaItemListPtr);
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
    if !(*typ).cont_model.is_null() {
        xml_reg_free_regexp((*typ).cont_model);
    }
    xml_free(typ as _);
}

/**
 * xmlSchemaFreeWildcardNsSet:
 * set:  a schema wildcard namespace
 *
 * Deallocates a list of wildcard constraint structures.
 */
pub(crate) unsafe extern "C" fn xml_schema_free_wildcard_ns_set(mut set: XmlSchemaWildcardNsPtr) {
    let mut next: XmlSchemaWildcardNsPtr;

    while !set.is_null() {
        next = (*set).next;
        xml_free(set as _);
        set = next;
    }
}

/**
 * xmlSchemaFreeWildcard:
 * @wildcard:  a wildcard structure
 *
 * Deallocates a wildcard structure.
 */
pub unsafe extern "C" fn xml_schema_free_wildcard(wildcard: XmlSchemaWildcardPtr) {
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
