//! Provide methods and data structures for XML Schemas types.  
//! This module is based on `libxml/xmlschemastypes.h`, `xmlschemas.c`, `xmlschemastypes.c` and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: implementation of XML Schema Datatypes
// Description: module providing the XML Schema Datatypes implementation
//              both definition and validity checking
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// schemastypes.c : implementation of the XML Schema Datatypes definition and validity checking
//
// See Copyright for the status of this software.
//
// Daniel Veillard <veillard@redhat.com>

use std::{
    cell::Cell,
    ffi::{CStr, CString, c_char},
    mem::size_of,
    os::raw::c_void,
    ptr::{addr_of_mut, null, null_mut},
};

use libc::{memcpy, memmove, memset, snprintf, sscanf};

use crate::{
    error::{__xml_simple_oom_error, XmlErrorDomain, XmlParserErrors},
    libxml::{
        globals::{xml_free, xml_malloc, xml_malloc_atomic},
        hash::{
            XmlHashTable, xml_hash_add_entry2, xml_hash_create, xml_hash_free, xml_hash_lookup2,
        },
        schemas_internals::{
            XML_SCHEMAS_ANY_LAX, XML_SCHEMAS_FACET_COLLAPSE, XML_SCHEMAS_FACET_PRESERVE,
            XML_SCHEMAS_FACET_REPLACE, XML_SCHEMAS_TYPE_BUILTIN_PRIMITIVE,
            XML_SCHEMAS_TYPE_HAS_FACETS, XML_SCHEMAS_TYPE_VARIETY_ATOMIC,
            XML_SCHEMAS_TYPE_VARIETY_LIST, XmlSchemaContentType, XmlSchemaFacet, XmlSchemaFacetPtr,
            XmlSchemaTypeType, XmlSchemaValType, XmlSchemaWildcard, XmlSchemaWildcardPtr,
            xml_schema_free_annot, xml_schema_free_type, xml_schema_free_wildcard,
        },
        uri::{XmlURIPtr, xml_free_uri, xml_parse_uri},
        valid::{xml_add_id, xml_add_ref, xml_validate_notation_use},
        xmlregexp::{xml_reg_free_regexp, xml_regexp_compile, xml_regexp_exec},
        xmlschemas::{
            XmlSchemaAbstractCtxtPtr, xml_schema_facet_type_to_string, xml_schema_format_qname,
            xml_schema_vcheck_cvc_simple_type,
        },
        xmlstring::{
            XmlChar, xml_str_equal, xml_strcat, xml_strcmp, xml_strdup, xml_strndup,
            xml_utf8_strlen,
        },
    },
    tree::{
        XmlAttrPtr, XmlAttributeType, XmlEntityType, XmlGenericNodePtr, validate_ncname,
        xml_get_doc_entity, xml_split_qname2, xml_validate_name, xml_validate_nmtoken,
        xml_validate_qname,
    },
    xmlschemas::{
        context::{
            XmlSchemaParserCtxtPtr, xml_schema_free_parser_ctxt, xml_schema_new_parser_ctxt,
        },
        error::{xml_schema_custom_err, xml_schema_custom_err4},
        items::{
            XmlSchemaBasicItemPtr, XmlSchemaModelGroup, XmlSchemaModelGroupPtr, XmlSchemaParticle,
            XmlSchemaParticlePtr, XmlSchemaTreeItemPtr, XmlSchemaType, XmlSchemaTypePtr,
        },
    },
    xmlschemastypes::{xml_schema_collapse_string, xml_schema_white_space_replace},
    xpath::{XML_XPATH_NAN, XML_XPATH_NINF, XML_XPATH_PINF, xml_xpath_is_nan},
};

use super::{
    chvalid::{xml_is_blank_char, xml_is_digit},
    hash::CVoidWrapper,
};

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlSchemaWhitespaceValueType {
    XmlSchemaWhitespaceUnknown = 0,
    XmlSchemaWhitespacePreserve = 1,
    XmlSchemaWhitespaceReplace = 2,
    XmlSchemaWhitespaceCollapse = 3,
}

const XML_SCHEMAS_NAMESPACE_NAME: &CStr = c"http://www.w3.org/2001/XMLSchema";

macro_rules! IS_WSP_REPLACE_CH {
    ($c:expr) => {
        ($c == 0x9 || $c == 0xa) || $c == 0xd
    };
}

macro_rules! IS_WSP_SPACE_CH {
    ($c:expr) => {
        $c == 0x20
    };
}

macro_rules! IS_WSP_BLANK_CH {
    ($c:expr) => {
        $crate::libxml::chvalid::xml_is_blank_char($c as u32)
    };
}

// Date value
pub type XmlSchemaValDatePtr = *mut XmlSchemaValDate;
#[repr(C)]
#[derive(Clone, Copy)]
pub struct XmlSchemaValDate {
    year: i64,
    // u32	mon	:4;	/* 1 <=  mon    <= 12   */
    mon: u32,
    // u32	day	:5;	/* 1 <=  day    <= 31   */
    day: u32,
    // u32	hour	:5;	/* 0 <=  hour   <= 24   */
    hour: u32,
    // u32	min	:6;	/* 0 <=  min    <= 59	*/
    min: u32,
    sec: f64,
    // u32	tz_flag	:1;	/* is tzo explicitly set? */
    tz_flag: u32,
    // c_int		tzo	:12;	/* -1440 <= tzo <= 1440;
    // 				   currently only -840 to +840 are needed */
    tzo: i32,
}

// Duration value
pub type XmlSchemaValDurationPtr = *mut XmlSchemaValDuration;
#[repr(C)]
#[derive(Clone, Copy)]
pub struct XmlSchemaValDuration {
    mon: i64, /* mon stores years also */
    day: i64,
    sec: f64, /* sec stores min and hour also */
}

pub type XmlSchemaValDecimalPtr = *mut XmlSchemaValDecimal;
#[repr(C)]
#[derive(Clone, Copy)]
pub struct XmlSchemaValDecimal {
    /* would use long long but not portable */
    lo: u64,
    mi: u64,
    hi: u64,
    extra: u32,
    // u32 sign:1;
    sign: u32,
    // u32 frac:7;
    frac: u32,
    // u32 total:8;
    total: u32,
}

pub type XmlSchemaValQnamePtr = *mut XmlSchemaValQname;
#[repr(C)]
#[derive(Clone, Copy)]
pub struct XmlSchemaValQname {
    name: *mut XmlChar,
    uri: *mut XmlChar,
}

pub type XmlSchemaValHexPtr = *mut XmlSchemaValHex;
#[repr(C)]
#[derive(Clone, Copy)]
pub struct XmlSchemaValHex {
    str: *mut XmlChar,
    total: u32,
}

pub type XmlSchemaValBase64ptr = *mut XmlSchemaValBase64;
#[repr(C)]
#[derive(Clone, Copy)]
pub struct XmlSchemaValBase64 {
    str: *mut XmlChar,
    total: u32,
}

#[repr(C)]
union XmlSchemaValInternal {
    decimal: XmlSchemaValDecimal,
    date: XmlSchemaValDate,
    dur: XmlSchemaValDuration,
    qname: XmlSchemaValQname,
    hex: XmlSchemaValHex,
    base64: XmlSchemaValBase64,
    f: f32,
    d: f64,
    b: i32,
    str: *mut XmlChar,
}

pub type XmlSchemaValPtr = *mut XmlSchemaVal;
#[repr(C)]
pub struct XmlSchemaVal {
    typ: XmlSchemaValType,
    next: *mut XmlSchemaVal,
    value: XmlSchemaValInternal,
}

thread_local! {
    static XML_SCHEMA_TYPES_INITIALIZED: Cell<bool> = const { Cell::new(false) };
    static XML_SCHEMA_TYPES_BANK: Cell<*mut XmlHashTable<'static, CVoidWrapper>> =
        const { Cell::new(null_mut()) };

    /// Basic types
    static XML_SCHEMA_TYPE_STRING_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_ANY_TYPE_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_DECIMAL_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_DATETIME_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_DATE_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_TIME_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_GYEAR_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_GYEAR_MONTH_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_GDAY_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_GMONTH_DAY_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_GMONTH_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_DURATION_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_FLOAT_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_BOOLEAN_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_DOUBLE_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_HEX_BINARY_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_BASE64_BINARY_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_ANY_URIDEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };

    /// Derived types
    static XML_SCHEMA_TYPE_POSITIVE_INTEGER_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_NON_POSITIVE_INTEGER_DEF: Cell<*mut XmlSchemaType> =
        const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_NEGATIVE_INTEGER_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_NON_NEGATIVE_INTEGER_DEF: Cell<*mut XmlSchemaType> =
        const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_INTEGER_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_LONG_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_INT_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_SHORT_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_BYTE_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_UNSIGNED_LONG_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_UNSIGNED_INT_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_UNSIGNED_SHORT_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_UNSIGNED_BYTE_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_NORM_STRING_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_TOKEN_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_LANGUAGE_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_NAME_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_QNAME_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_NCNAME_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_ID_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_IDREF_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_IDREFS_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_ENTITY_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_ENTITIES_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_NOTATION_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_NMTOKEN_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
    static XML_SCHEMA_TYPE_NMTOKENS_DEF: Cell<*mut XmlSchemaType> = const { Cell::new(null_mut()) };
}

macro_rules! IS_TZO_CHAR {
    ($c:expr) => {
        $c == 0 || $c == b'Z' || $c == b'+' || $c == b'-'
    };
}

macro_rules! VALID_YEAR {
    ($yr:expr) => {
        $yr != 0
    };
}
macro_rules! VALID_MONTH {
    ($mon:expr) => {
        (1..=12).contains(&$mon)
    };
}
/* VALID_DAY should only be used when month is unknown */
macro_rules! VALID_DAY {
    ($day:expr) => {
        (1..=31).contains(&$day)
    };
}
macro_rules! VALID_HOUR {
    ($hr:expr) => {
        (0..=23).contains(&$hr)
    };
}
macro_rules! VALID_MIN {
    ($min:expr) => {
        (0..=59).contains(&$min)
    };
}
macro_rules! VALID_SEC {
    ($sec:expr) => {
        $sec >= 0.0 && $sec < 60.0
    };
}
macro_rules! VALID_TZO {
    ($tzo:expr) => {
        $tzo >= -840 && $tzo <= 840
    };
}
macro_rules! IS_LEAP {
    ($y:expr) => {
        ($y % 4 == 0 && $y % 100 != 0) || $y % 400 == 0
    };
}

const DAYS_IN_MONTH: [u32; 12] = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
const DAYS_IN_MONTH_LEAP: [u32; 12] = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

macro_rules! MAX_DAYINMONTH {
    ($yr:expr, $mon:expr) => {
        if IS_LEAP!($yr) {
            DAYS_IN_MONTH_LEAP[$mon as usize - 1]
        } else {
            DAYS_IN_MONTH[$mon as usize - 1]
        }
    };
}

macro_rules! VALID_MDAY {
    ($dt:expr) => {
        if IS_LEAP!((*$dt).year) {
            (*$dt).day <= DAYS_IN_MONTH_LEAP[(*$dt).mon as usize - 1]
        } else {
            (*$dt).day <= DAYS_IN_MONTH[(*$dt).mon as usize - 1]
        }
    };
}

macro_rules! VALID_DATE {
    ($dt:expr) => {
        VALID_YEAR!((*$dt).year) && VALID_MONTH!((*$dt).mon) && VALID_MDAY!($dt)
    };
}

macro_rules! VALID_END_OF_DAY {
    ($dt:expr) => {
        (*$dt).hour == 24 && (*$dt).min == 0 && (*$dt).sec == 0.0
    };
}

macro_rules! VALID_TIME {
    ($dt:expr) => {
        ((VALID_HOUR!((*$dt).hour) && VALID_MIN!((*$dt).min) && VALID_SEC!((*$dt).sec))
            || VALID_END_OF_DAY!($dt))
            && VALID_TZO!((*$dt).tzo)
    };
}

macro_rules! VALID_DATETIME {
    ($dt:expr) => {
        VALID_DATE!($dt) && VALID_TIME!($dt)
    };
}

const SECS_PER_MIN: i32 = 60;
const MINS_PER_HOUR: i32 = 60;
const HOURS_PER_DAY: i32 = 24;
const SECS_PER_HOUR: i32 = MINS_PER_HOUR * SECS_PER_MIN;
const SECS_PER_DAY: i32 = HOURS_PER_DAY * SECS_PER_HOUR;
const MINS_PER_DAY: i32 = HOURS_PER_DAY * MINS_PER_HOUR;

const DAY_IN_YEAR_BY_MONTH: [i64; 12] = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334];
const DAY_IN_LEAP_YEAR_BY_MONTH: [i64; 12] =
    [0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335];

macro_rules! DAY_IN_YEAR {
	($day:expr, $month:expr, $year:expr) => {
        if IS_LEAP!($year) {
			DAY_IN_LEAP_YEAR_BY_MONTH[$month as usize - 1]
		} else {
			DAY_IN_YEAR_BY_MONTH[$month as usize - 1]
		} + $day
	}
}

// WARNING: Those type reside normally in xmlschemas.c but are
// redefined here locally in oder of being able to use them for xs:anyType-
// TODO: Remove those definition if we move the types to a header file.
// TODO: Always keep those structs up-to-date with the originals.
const UNBOUNDED: i32 = 1 << 30;

extern "C" fn xml_schema_free_type_entry(typ: *mut c_void, _name: *const XmlChar) {
    unsafe {
        xml_schema_free_type(typ as XmlSchemaTypePtr);
    }
}

/// Cleanup the default XML Schemas type library
#[doc(alias = "xmlSchemaCleanupTypesInternal")]
unsafe fn xml_schema_cleanup_types_internal() {
    unsafe {
        let particle: XmlSchemaParticlePtr;

        // Free xs:anyType.
        let anytype_def = XML_SCHEMA_TYPE_ANY_TYPE_DEF.get();
        if !anytype_def.is_null() {
            // Attribute wildcard.
            xml_schema_free_wildcard((*anytype_def).attribute_wildcard);
            // Content type.
            particle = (*anytype_def).subtypes as XmlSchemaParticlePtr;
            // Wildcard.
            xml_schema_free_wildcard(
                (*(*(*particle).children).children).children as XmlSchemaWildcardPtr,
            );
            xml_free((*(*particle).children).children as _);
            // Sequence model group.
            xml_free((*particle).children as _);
            xml_free(particle as _);
            (*anytype_def).subtypes = null_mut();
            XML_SCHEMA_TYPE_ANY_TYPE_DEF.set(null_mut());
        }

        xml_hash_free(
            XML_SCHEMA_TYPES_BANK.get(),
            Some(xml_schema_free_type_entry),
        );
        XML_SCHEMA_TYPES_BANK.set(null_mut());
        // Note that the xmlSchemaType*Def pointers aren't set to NULL.
    }
}

/// Handle an out of memory condition
#[doc(alias = "xmlSchemaTypeErrMemory")]
unsafe fn xml_schema_type_err_memory(node: Option<XmlGenericNodePtr>, extra: Option<&str>) {
    unsafe {
        __xml_simple_oom_error(XmlErrorDomain::XmlFromDatatype, node, extra);
    }
}

/// Allocate a new simple type value
///
/// Returns a pointer to the new value or NULL in case of error
#[doc(alias = "xmlSchemaNewValue")]
unsafe fn xml_schema_new_value(typ: XmlSchemaValType) -> XmlSchemaValPtr {
    unsafe {
        let value: XmlSchemaValPtr = xml_malloc(size_of::<XmlSchemaVal>()) as XmlSchemaValPtr;
        if value.is_null() {
            return null_mut();
        }
        memset(value as _, 0, size_of::<XmlSchemaVal>());
        (*value).typ = typ;
        value
    }
}

unsafe fn xml_schema_new_min_length_facet(value: i32) -> XmlSchemaFacetPtr {
    unsafe {
        let ret: XmlSchemaFacetPtr = xml_schema_new_facet();
        if ret.is_null() {
            return null_mut();
        }
        (*ret).typ = XmlSchemaTypeType::XmlSchemaFacetMinLength;
        (*ret).val = xml_schema_new_value(XmlSchemaValType::XmlSchemasNNInteger);
        if (*ret).val.is_null() {
            xml_free(ret as _);
            return null_mut();
        }
        (*(*ret).val).value.decimal.lo = value as _;
        ret
    }
}

/// Initialize one primitive built-in type
#[doc(alias = "xmlSchemaInitBasicType")]
unsafe fn xml_schema_init_basic_type(
    name: *const c_char,
    typ: XmlSchemaValType,
    base_type: XmlSchemaTypePtr,
) -> XmlSchemaTypePtr {
    unsafe {
        let ret: XmlSchemaTypePtr = xml_malloc(size_of::<XmlSchemaType>()) as XmlSchemaTypePtr;
        if ret.is_null() {
            xml_schema_type_err_memory(None, Some("could not initialize basic types"));
            return null_mut();
        }
        memset(ret as _, 0, size_of::<XmlSchemaType>());
        (*ret).name = name as *const XmlChar;
        (*ret).target_namespace = XML_SCHEMAS_NAMESPACE_NAME.as_ptr() as _;
        (*ret).typ = XmlSchemaTypeType::XmlSchemaTypeBasic;
        (*ret).base_type = base_type;
        (*ret).content_type = XmlSchemaContentType::XmlSchemaContentBasic;
        // Primitive types.
        match typ {
            XmlSchemaValType::XmlSchemasString
            | XmlSchemaValType::XmlSchemasDecimal
            | XmlSchemaValType::XmlSchemasDate
            | XmlSchemaValType::XmlSchemasDatetime
            | XmlSchemaValType::XmlSchemasTime
            | XmlSchemaValType::XmlSchemasGYear
            | XmlSchemaValType::XmlSchemasGYearMonth
            | XmlSchemaValType::XmlSchemasGMonth
            | XmlSchemaValType::XmlSchemasGMonthDay
            | XmlSchemaValType::XmlSchemasGDay
            | XmlSchemaValType::XmlSchemasDuration
            | XmlSchemaValType::XmlSchemasFloat
            | XmlSchemaValType::XmlSchemasDouble
            | XmlSchemaValType::XmlSchemasBoolean
            | XmlSchemaValType::XmlSchemasAnyURI
            | XmlSchemaValType::XmlSchemasHexbinary
            | XmlSchemaValType::XmlSchemasBase64binary
            | XmlSchemaValType::XmlSchemasQName
            | XmlSchemaValType::XmlSchemasNotation => {
                (*ret).flags |= XML_SCHEMAS_TYPE_BUILTIN_PRIMITIVE;
            }
            _ => {}
        }
        // Set variety.
        match typ {
            XmlSchemaValType::XmlSchemasAnytype | XmlSchemaValType::XmlSchemasAnySimpletype => {}
            XmlSchemaValType::XmlSchemasIDREFS
            | XmlSchemaValType::XmlSchemasNmtokens
            | XmlSchemaValType::XmlSchemasEntities => {
                (*ret).flags |= XML_SCHEMAS_TYPE_VARIETY_LIST;
                (*ret).facets = xml_schema_new_min_length_facet(1);
                (*ret).flags |= XML_SCHEMAS_TYPE_HAS_FACETS;
            }
            _ => {
                (*ret).flags |= XML_SCHEMAS_TYPE_VARIETY_ATOMIC;
            }
        }
        xml_hash_add_entry2(
            XML_SCHEMA_TYPES_BANK.get(),
            (*ret).name,
            XML_SCHEMAS_NAMESPACE_NAME.as_ptr() as _,
            ret as _,
        );
        (*ret).built_in_type = typ as i32;
        ret
    }
}

unsafe fn xml_schema_add_particle() -> XmlSchemaParticlePtr {
    unsafe {
        let ret: XmlSchemaParticlePtr =
            xml_malloc(size_of::<XmlSchemaParticle>()) as XmlSchemaParticlePtr;
        if ret.is_null() {
            xml_schema_type_err_memory(None, Some("allocating particle component"));
            return null_mut();
        }
        memset(ret as _, 0, size_of::<XmlSchemaParticle>());
        (*ret).typ = XmlSchemaTypeType::XmlSchemaTypeParticle;
        (*ret).min_occurs = 1;
        (*ret).max_occurs = 1;
        ret
    }
}

/// Initialize the default XML Schemas type library
///
/// Returns 0 on success, -1 on error.
#[doc(alias = "xmlSchemaInitTypes")]
pub unsafe fn xml_schema_init_types() -> i32 {
    unsafe {
        if XML_SCHEMA_TYPES_INITIALIZED.get() {
            return 0;
        }

        'error: {
            XML_SCHEMA_TYPES_BANK.set(xml_hash_create(40));
            if XML_SCHEMA_TYPES_BANK.get().is_null() {
                xml_schema_type_err_memory(None, None);
                break 'error;
            }

            // 3.4.7 Built-in Complex Type Definition
            XML_SCHEMA_TYPE_ANY_TYPE_DEF.set(xml_schema_init_basic_type(
                c"anyType".as_ptr() as _,
                XmlSchemaValType::XmlSchemasAnytype,
                null_mut(),
            ));
            if XML_SCHEMA_TYPE_ANY_TYPE_DEF.get().is_null() {
                break 'error;
            }
            // Init the content type.
            let anytype_def = XML_SCHEMA_TYPE_ANY_TYPE_DEF.get();
            (*anytype_def).base_type = anytype_def;
            (*anytype_def).content_type = XmlSchemaContentType::XmlSchemaContentMixed;
            {
                let mut particle: XmlSchemaParticlePtr;
                let mut wild: XmlSchemaWildcardPtr;
                // First particle.
                particle = xml_schema_add_particle();
                if particle.is_null() {
                    break 'error;
                }
                (*anytype_def).subtypes = particle as XmlSchemaTypePtr;
                // Sequence model group.
                let sequence: XmlSchemaModelGroupPtr =
                    xml_malloc(size_of::<XmlSchemaModelGroup>()) as XmlSchemaModelGroupPtr;
                if sequence.is_null() {
                    xml_schema_type_err_memory(None, Some("allocating model group component"));
                    break 'error;
                }
                memset(sequence as _, 0, size_of::<XmlSchemaModelGroup>());
                (*sequence).typ = XmlSchemaTypeType::XmlSchemaTypeSequence;
                (*particle).children = sequence as _;
                // Second particle.
                particle = xml_schema_add_particle();
                if particle.is_null() {
                    break 'error;
                }
                (*particle).min_occurs = 0;
                (*particle).max_occurs = UNBOUNDED;
                (*sequence).children = particle as _;
                // The wildcard
                wild = xml_malloc(size_of::<XmlSchemaWildcard>()) as XmlSchemaWildcardPtr;
                if wild.is_null() {
                    xml_schema_type_err_memory(None, Some("allocating wildcard component"));
                    break 'error;
                }
                memset(wild as _, 0, size_of::<XmlSchemaWildcard>());
                (*wild).typ = XmlSchemaTypeType::XmlSchemaTypeAny;
                (*wild).any = 1;
                (*wild).process_contents = XML_SCHEMAS_ANY_LAX;
                (*particle).children = wild as XmlSchemaTreeItemPtr;
                // Create the attribute wildcard.
                wild = xml_malloc(size_of::<XmlSchemaWildcard>()) as XmlSchemaWildcardPtr;
                if wild.is_null() {
                    xml_schema_type_err_memory(
                        None,
                        Some("could not create an attribute wildcard on anyType"),
                    );
                    break 'error;
                }
                memset(wild as _, 0, size_of::<XmlSchemaWildcard>());
                (*wild).any = 1;
                (*wild).process_contents = XML_SCHEMAS_ANY_LAX;
                (*anytype_def).attribute_wildcard = wild;
            }
            XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.set(xml_schema_init_basic_type(
                c"anySimpleType".as_ptr() as _,
                XmlSchemaValType::XmlSchemasAnySimpletype,
                XML_SCHEMA_TYPE_ANY_TYPE_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.get().is_null() {
                break 'error;
            }
            // primitive datatypes
            XML_SCHEMA_TYPE_STRING_DEF.set(xml_schema_init_basic_type(
                c"string".as_ptr() as _,
                XmlSchemaValType::XmlSchemasString,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_STRING_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_DECIMAL_DEF.set(xml_schema_init_basic_type(
                c"decimal".as_ptr() as _,
                XmlSchemaValType::XmlSchemasDecimal,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_DECIMAL_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_DATE_DEF.set(xml_schema_init_basic_type(
                c"date".as_ptr() as _,
                XmlSchemaValType::XmlSchemasDate,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_DATE_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_DATETIME_DEF.set(xml_schema_init_basic_type(
                c"dateTime".as_ptr() as _,
                XmlSchemaValType::XmlSchemasDatetime,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_DATETIME_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_TIME_DEF.set(xml_schema_init_basic_type(
                c"time".as_ptr() as _,
                XmlSchemaValType::XmlSchemasTime,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_TIME_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_GYEAR_DEF.set(xml_schema_init_basic_type(
                c"gYear".as_ptr() as _,
                XmlSchemaValType::XmlSchemasGYear,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_GYEAR_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_GYEAR_MONTH_DEF.set(xml_schema_init_basic_type(
                c"gYearMonth".as_ptr() as _,
                XmlSchemaValType::XmlSchemasGYearMonth,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_GYEAR_MONTH_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_GMONTH_DEF.set(xml_schema_init_basic_type(
                c"gMonth".as_ptr() as _,
                XmlSchemaValType::XmlSchemasGMonth,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_GMONTH_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_GMONTH_DAY_DEF.set(xml_schema_init_basic_type(
                c"gMonthDay".as_ptr() as _,
                XmlSchemaValType::XmlSchemasGMonthDay,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_GMONTH_DAY_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_GDAY_DEF.set(xml_schema_init_basic_type(
                c"gDay".as_ptr() as _,
                XmlSchemaValType::XmlSchemasGDay,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_GDAY_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_DURATION_DEF.set(xml_schema_init_basic_type(
                c"duration".as_ptr() as _,
                XmlSchemaValType::XmlSchemasDuration,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_DURATION_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_FLOAT_DEF.set(xml_schema_init_basic_type(
                c"float".as_ptr() as _,
                XmlSchemaValType::XmlSchemasFloat,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_FLOAT_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_DOUBLE_DEF.set(xml_schema_init_basic_type(
                c"double".as_ptr() as _,
                XmlSchemaValType::XmlSchemasDouble,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_DOUBLE_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_BOOLEAN_DEF.set(xml_schema_init_basic_type(
                c"boolean".as_ptr() as _,
                XmlSchemaValType::XmlSchemasBoolean,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_BOOLEAN_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_ANY_URIDEF.set(xml_schema_init_basic_type(
                c"anyURI".as_ptr() as _,
                XmlSchemaValType::XmlSchemasAnyURI,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_ANY_URIDEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_HEX_BINARY_DEF.set(xml_schema_init_basic_type(
                c"hexBinary".as_ptr() as _,
                XmlSchemaValType::XmlSchemasHexbinary,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_HEX_BINARY_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_BASE64_BINARY_DEF.set(xml_schema_init_basic_type(
                c"base64Binary".as_ptr() as _,
                XmlSchemaValType::XmlSchemasBase64binary,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_BASE64_BINARY_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_NOTATION_DEF.set(xml_schema_init_basic_type(
                c"NOTATION".as_ptr() as _,
                XmlSchemaValType::XmlSchemasNotation,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_NOTATION_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_QNAME_DEF.set(xml_schema_init_basic_type(
                c"QName".as_ptr() as _,
                XmlSchemaValType::XmlSchemasQName,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_QNAME_DEF.get().is_null() {
                break 'error;
            }

            // derived datatypes
            XML_SCHEMA_TYPE_INTEGER_DEF.set(xml_schema_init_basic_type(
                c"integer".as_ptr() as _,
                XmlSchemaValType::XmlSchemasInteger,
                XML_SCHEMA_TYPE_DECIMAL_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_INTEGER_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_NON_POSITIVE_INTEGER_DEF.set(xml_schema_init_basic_type(
                c"nonPositiveInteger".as_ptr() as _,
                XmlSchemaValType::XmlSchemasNPInteger,
                XML_SCHEMA_TYPE_INTEGER_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_NON_POSITIVE_INTEGER_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_NEGATIVE_INTEGER_DEF.set(xml_schema_init_basic_type(
                c"negativeInteger".as_ptr() as _,
                XmlSchemaValType::XmlSchemasNInteger,
                XML_SCHEMA_TYPE_NON_POSITIVE_INTEGER_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_NEGATIVE_INTEGER_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_LONG_DEF.set(xml_schema_init_basic_type(
                c"long".as_ptr() as _,
                XmlSchemaValType::XmlSchemasLong,
                XML_SCHEMA_TYPE_INTEGER_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_LONG_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_INT_DEF.set(xml_schema_init_basic_type(
                c"int".as_ptr() as _,
                XmlSchemaValType::XmlSchemasInt,
                XML_SCHEMA_TYPE_LONG_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_INT_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_SHORT_DEF.set(xml_schema_init_basic_type(
                c"short".as_ptr() as _,
                XmlSchemaValType::XmlSchemasShort,
                XML_SCHEMA_TYPE_INT_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_SHORT_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_BYTE_DEF.set(xml_schema_init_basic_type(
                c"byte".as_ptr() as _,
                XmlSchemaValType::XmlSchemasByte,
                XML_SCHEMA_TYPE_SHORT_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_BYTE_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_NON_NEGATIVE_INTEGER_DEF.set(xml_schema_init_basic_type(
                c"nonNegativeInteger".as_ptr() as _,
                XmlSchemaValType::XmlSchemasNNInteger,
                XML_SCHEMA_TYPE_INTEGER_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_NON_NEGATIVE_INTEGER_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_UNSIGNED_LONG_DEF.set(xml_schema_init_basic_type(
                c"unsignedLong".as_ptr() as _,
                XmlSchemaValType::XmlSchemasULong,
                XML_SCHEMA_TYPE_NON_NEGATIVE_INTEGER_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_UNSIGNED_LONG_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_UNSIGNED_INT_DEF.set(xml_schema_init_basic_type(
                c"unsignedInt".as_ptr() as _,
                XmlSchemaValType::XmlSchemasUInt,
                XML_SCHEMA_TYPE_UNSIGNED_LONG_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_UNSIGNED_INT_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_UNSIGNED_SHORT_DEF.set(xml_schema_init_basic_type(
                c"unsignedShort".as_ptr() as _,
                XmlSchemaValType::XmlSchemasUShort,
                XML_SCHEMA_TYPE_UNSIGNED_INT_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_UNSIGNED_SHORT_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_UNSIGNED_BYTE_DEF.set(xml_schema_init_basic_type(
                c"unsignedByte".as_ptr() as _,
                XmlSchemaValType::XmlSchemasUByte,
                XML_SCHEMA_TYPE_UNSIGNED_SHORT_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_UNSIGNED_BYTE_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_POSITIVE_INTEGER_DEF.set(xml_schema_init_basic_type(
                c"positiveInteger".as_ptr() as _,
                XmlSchemaValType::XmlSchemasPInteger,
                XML_SCHEMA_TYPE_NON_NEGATIVE_INTEGER_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_POSITIVE_INTEGER_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_NORM_STRING_DEF.set(xml_schema_init_basic_type(
                c"normalizedString".as_ptr() as _,
                XmlSchemaValType::XmlSchemasNormString,
                XML_SCHEMA_TYPE_STRING_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_NORM_STRING_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_TOKEN_DEF.set(xml_schema_init_basic_type(
                c"token".as_ptr() as _,
                XmlSchemaValType::XmlSchemasToken,
                XML_SCHEMA_TYPE_NORM_STRING_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_TOKEN_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_LANGUAGE_DEF.set(xml_schema_init_basic_type(
                c"language".as_ptr() as _,
                XmlSchemaValType::XmlSchemasLanguage,
                XML_SCHEMA_TYPE_TOKEN_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_LANGUAGE_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_NAME_DEF.set(xml_schema_init_basic_type(
                c"Name".as_ptr() as _,
                XmlSchemaValType::XmlSchemasName,
                XML_SCHEMA_TYPE_TOKEN_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_NAME_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_NMTOKEN_DEF.set(xml_schema_init_basic_type(
                c"NMTOKEN".as_ptr() as _,
                XmlSchemaValType::XmlSchemasNmtoken,
                XML_SCHEMA_TYPE_TOKEN_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_NMTOKEN_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_NCNAME_DEF.set(xml_schema_init_basic_type(
                c"NCName".as_ptr() as _,
                XmlSchemaValType::XmlSchemasNCName,
                XML_SCHEMA_TYPE_NAME_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_NCNAME_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_ID_DEF.set(xml_schema_init_basic_type(
                c"ID".as_ptr() as _,
                XmlSchemaValType::XmlSchemasID,
                XML_SCHEMA_TYPE_NCNAME_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_ID_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_IDREF_DEF.set(xml_schema_init_basic_type(
                c"IDREF".as_ptr() as _,
                XmlSchemaValType::XmlSchemasIDREF,
                XML_SCHEMA_TYPE_NCNAME_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_IDREF_DEF.get().is_null() {
                break 'error;
            }
            XML_SCHEMA_TYPE_ENTITY_DEF.set(xml_schema_init_basic_type(
                c"ENTITY".as_ptr() as _,
                XmlSchemaValType::XmlSchemasEntity,
                XML_SCHEMA_TYPE_NCNAME_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_ENTITY_DEF.get().is_null() {
                break 'error;
            }
            // Derived list types.
            // ENTITIES
            XML_SCHEMA_TYPE_ENTITIES_DEF.set(xml_schema_init_basic_type(
                c"ENTITIES".as_ptr() as _,
                XmlSchemaValType::XmlSchemasEntities,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_ENTITIES_DEF.get().is_null() {
                break 'error;
            }
            (*XML_SCHEMA_TYPE_ENTITIES_DEF.get()).subtypes = XML_SCHEMA_TYPE_ENTITY_DEF.get();
            // IDREFS
            XML_SCHEMA_TYPE_IDREFS_DEF.set(xml_schema_init_basic_type(
                c"IDREFS".as_ptr() as _,
                XmlSchemaValType::XmlSchemasIDREFS,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_IDREFS_DEF.get().is_null() {
                break 'error;
            }
            (*XML_SCHEMA_TYPE_IDREFS_DEF.get()).subtypes = XML_SCHEMA_TYPE_IDREF_DEF.get();

            // NMTOKENS
            XML_SCHEMA_TYPE_NMTOKENS_DEF.set(xml_schema_init_basic_type(
                c"NMTOKENS".as_ptr() as _,
                XmlSchemaValType::XmlSchemasNmtokens,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.get(),
            ));
            if XML_SCHEMA_TYPE_NMTOKENS_DEF.get().is_null() {
                break 'error;
            }
            (*XML_SCHEMA_TYPE_NMTOKENS_DEF.get()).subtypes = XML_SCHEMA_TYPE_NMTOKEN_DEF.get();

            XML_SCHEMA_TYPES_INITIALIZED.set(true);
            return 0;
        }

        // error:
        xml_schema_cleanup_types_internal();
        -1
    }
}

/// DEPRECATED: This function will be made private. Call xmlCleanupParser
/// to free global state but see the warnings there. xmlCleanupParser
/// should be only called once at program exit. In most cases, you don't
/// have to call cleanup functions at all.
///
/// Cleanup the default XML Schemas type library
#[doc(alias = "xmlSchemaCleanupTypes")]
pub(crate) unsafe fn xml_schema_cleanup_types() {
    unsafe {
        if XML_SCHEMA_TYPES_INITIALIZED.get() {
            xml_schema_cleanup_types_internal();
            XML_SCHEMA_TYPES_INITIALIZED.set(false);
        }
    }
}

/// Lookup a type in the default XML Schemas type library
///
/// Returns the type if found, NULL otherwise
#[doc(alias = "xmlSchemaGetPredefinedType")]
pub unsafe fn xml_schema_get_predefined_type(name: &str, ns: &str) -> XmlSchemaTypePtr {
    unsafe {
        if !XML_SCHEMA_TYPES_INITIALIZED.get() && xml_schema_init_types() < 0 {
            return null_mut();
        }
        let name = CString::new(name).unwrap();
        let ns = CString::new(ns).unwrap();
        xml_hash_lookup2(
            XML_SCHEMA_TYPES_BANK.get(),
            name.as_ptr() as *const u8,
            ns.as_ptr() as *const u8,
        ) as XmlSchemaTypePtr
    }
}

/// Check that a value conforms to the lexical space of the predefined type.
/// if true a value is computed and returned in @val.
///
/// Returns 0 if this validates, a positive error code number otherwise
/// and -1 in case of internal or API error.
#[doc(alias = "xmlSchemaValidatePredefinedType")]
pub unsafe fn xml_schema_validate_predefined_type(
    typ: XmlSchemaTypePtr,
    value: *const XmlChar,
    val: *mut XmlSchemaValPtr,
) -> i32 {
    unsafe { xml_schema_val_predef_type_node(typ, value, val, None) }
}

/// Parse an u64 into 3 fields.
///
/// Returns the number of significant digits in the number or
/// -1 if overflow of the capacity and -2 if it's not a number.
#[doc(alias = "xmlSchemaParseUInt")]
unsafe fn xml_schema_parse_uint(
    str: *mut *const XmlChar,
    llo: *mut u64,
    lmi: *mut u64,
    lhi: *mut u64,
) -> i32 {
    unsafe {
        let mut lo: u64 = 0;
        let mut mi: u64 = 0;
        let mut hi: u64 = 0;
        let mut tmp: *const XmlChar;
        let mut cur: *const XmlChar = *str;
        let mut ret: i32 = 0;
        let mut i: i32 = 0;

        if !(*cur >= b'0' && *cur <= b'9') {
            return -2;
        }

        while *cur == b'0' {
            // ignore leading zeroes
            cur = cur.add(1);
        }
        tmp = cur;
        while *tmp != 0 && *tmp >= b'0' && *tmp <= b'9' {
            i += 1;
            tmp = tmp.add(1);
            ret += 1;
        }
        if i > 24 {
            *str = tmp;
            return -1;
        }
        while i > 16 {
            hi = hi * 10 + (*cur - b'0') as u64;
            cur = cur.add(1);
            i -= 1;
        }
        while i > 8 {
            mi = mi * 10 + (*cur - b'0') as u64;
            cur = cur.add(1);
            i -= 1;
        }
        while i > 0 {
            lo = lo * 10 + (*cur - b'0') as u64;
            cur = cur.add(1);
            i -= 1;
        }

        *str = cur;
        *llo = lo;
        *lmi = mi;
        *lhi = hi;
        ret
    }
}

/// Parses a 2-digits integer and updates @num with the value. @cur is
/// updated to point just after the integer.
/// In case of error, @invalid is set to %TRUE, values of @num and
/// @cur are undefined.
macro_rules! PARSE_2_DIGITS {
    ($num:expr, $cur:expr, $invalid:expr) => {
        if *$cur.add(0) < b'0' || *$cur.add(0) > b'9' || *$cur.add(1) < b'0' || *$cur.add(1) > b'9'
        {
            $invalid = 1;
        } else {
            $num = ((*$cur.add(0) - b'0') as u32 * 10 + (*$cur.add(1) - b'0') as u32) as _;
        }
        $cur = $cur.add(2);
    };
}

/// Parses a float and updates @num with the value. @cur is
/// updated to point just after the float. The float must have a
/// 2-digits integer part and may or may not have a decimal part.
/// In case of error, @invalid is set to %TRUE, values of @num and
/// @cur are undefined.
macro_rules! PARSE_FLOAT {
    ($num:expr, $cur:expr, $invalid:expr) => {
        PARSE_2_DIGITS!($num, $cur, $invalid);
        if $invalid == 0 && *$cur == b'.' {
            let mut mult: f64 = 1.;
            $cur = $cur.add(1);
            if *$cur < b'0' || *$cur > b'9' {
                $invalid = 1;
            }
            while *$cur >= b'0' && *$cur <= b'9' {
                mult /= 10.0;
                $num += (*$cur - b'0') as f64 * mult;
                $cur = $cur.add(1);
            }
        }
    };
}

/// Parses a time zone without time zone and fills in the appropriate
/// field of the @dt structure. @str is updated to point just after the time zone.
///
/// Returns 0 or the error code
#[doc(alias = "_xmlSchemaParseTimeZone")]
unsafe fn _xml_schema_parse_time_zone(dt: XmlSchemaValDatePtr, str: *mut *const XmlChar) -> i32 {
    unsafe {
        let mut cur: *const XmlChar;
        let mut ret: i32 = 0;

        if str.is_null() {
            return -1;
        }
        cur = *str;

        match *cur {
            0 => {
                (*dt).tz_flag = 0;
                (*dt).tzo = 0;
            }
            b'Z' => {
                (*dt).tz_flag = 1;
                (*dt).tzo = 0;
                cur = cur.add(1);
            }
            b'+' | b'-' => {
                let mut tmp: i32 = 0;
                let isneg: i32 = (*cur == b'-') as i32;

                cur = cur.add(1);

                PARSE_2_DIGITS!(tmp, cur, ret);
                if ret != 0 {
                    return ret;
                }
                if !VALID_HOUR!(tmp) {
                    return 2;
                }

                if *cur != b':' {
                    return 1;
                }
                cur = cur.add(1);

                (*dt).tzo = tmp * 60;

                PARSE_2_DIGITS!(tmp, cur, ret);
                if ret != 0 {
                    return ret;
                }
                if !VALID_MIN!(tmp) {
                    return 2;
                }

                (*dt).tzo += tmp;
                if isneg != 0 {
                    (*dt).tzo = -(*dt).tzo;
                }

                if !VALID_TZO!((*dt).tzo) {
                    return 2;
                }

                (*dt).tz_flag = 1;
            }
            _ => {
                return 1;
            }
        }

        *str = cur;
        0
    }
}

macro_rules! RETURN_TYPE_IF_VALID {
    ($cur:expr, $ret:expr, $dt:expr, $typ:expr, $val:expr, $t:expr) => {
        if IS_TZO_CHAR!(*$cur) {
            $ret = _xml_schema_parse_time_zone(addr_of_mut!((*$dt).value.date), addr_of_mut!($cur));
            if $ret == 0 {
                if *$cur != 0 {
                    if !$dt.is_null() {
                        xml_schema_free_value($dt);
                    }
                    return 1;
                }
                (*$dt).typ = $t;
                if $typ != XmlSchemaValType::XmlSchemasUnknown && $typ != (*$dt).typ {
                    if !$dt.is_null() {
                        xml_schema_free_value($dt);
                    }
                    return 1;
                }

                if !$val.is_null() {
                    *$val = $dt;
                } else {
                    xml_schema_free_value($dt);
                }

                return 0;
            }
        }
    };
}

/// Parses a xs:gDay without time zone and fills in the appropriate
/// field of the @dt structure. @str is updated to point just after the xs:gDay.
///
/// Returns 0 or the error code
#[doc(alias = "_xmlSchemaParseGDay")]
unsafe fn _xml_schema_parse_gday(dt: XmlSchemaValDatePtr, str: *mut *const XmlChar) -> i32 {
    unsafe {
        let mut cur: *const XmlChar = *str;
        let mut ret: i32 = 0;
        let mut value: u32 = 0;

        PARSE_2_DIGITS!(value, cur, ret);
        if ret != 0 {
            return ret;
        }

        if !VALID_DAY!(value) {
            return 2;
        }

        (*dt).day = value;
        *str = cur;
        0
    }
}

/// Parses a xs:gMonth without time zone and fills in the appropriate
/// field of the @dt structure. @str is updated to point just after the xs:gMonth.
///
/// Returns 0 or the error code
#[doc(alias = "_xmlSchemaParseGMonth")]
unsafe fn _xml_schema_parse_gmonth(dt: XmlSchemaValDatePtr, str: *mut *const XmlChar) -> i32 {
    unsafe {
        let mut cur: *const XmlChar = *str;
        let mut ret: i32 = 0;
        let mut value: u32 = 0;

        PARSE_2_DIGITS!(value, cur, ret);
        if ret != 0 {
            return ret;
        }

        if !VALID_MONTH!(value) {
            return 2;
        }

        (*dt).mon = value;

        *str = cur;
        0
    }
}

/// Parses a xs:time without time zone and fills in the appropriate
/// fields of the @dt structure. @str is updated to point just after the xs:time.
/// In case of error, values of @dt fields are undefined.
///
/// Returns 0 or the error code
#[doc(alias = "_xmlSchemaParseTime")]
unsafe fn _xml_schema_parse_time(dt: XmlSchemaValDatePtr, str: *mut *const XmlChar) -> i32 {
    unsafe {
        let mut cur: *const XmlChar = *str;
        let mut ret: i32 = 0;
        let mut value: i32 = 0;

        PARSE_2_DIGITS!(value, cur, ret);
        if ret != 0 {
            return ret;
        }
        if *cur != b':' {
            return 1;
        }

        // Allow end-of-day hour
        if !VALID_HOUR!(value) && value != 24 {
            return 2;
        }
        cur = cur.add(1);

        // the ':' insures this string is xs:time
        (*dt).hour = value as _;

        PARSE_2_DIGITS!(value, cur, ret);
        if ret != 0 {
            return ret;
        }
        if !VALID_MIN!(value) {
            return 2;
        }
        (*dt).min = value as _;

        if *cur != b':' {
            return 1;
        }
        cur = cur.add(1);

        PARSE_FLOAT!((*dt).sec, cur, ret);
        if ret != 0 {
            return ret;
        }

        if !VALID_TIME!(dt) {
            return 2;
        }

        *str = cur;
        0
    }
}

/// Parses a xs:gYear without time zone and fills in the appropriate
/// field of the @dt structure. @str is updated to point just after the
/// xs:gYear. It is supposed that @(*dt).year is big enough to contain the year.
///
/// Returns 0 or the error code
#[doc(alias = "_xmlSchemaParseGYear")]
unsafe fn _xml_schema_parse_gyear(dt: XmlSchemaValDatePtr, str: *mut *const XmlChar) -> i32 {
    unsafe {
        let mut cur: *const XmlChar = *str;
        let mut isneg: i32 = 0;
        let mut digcnt: i32 = 0;

        if (*cur < b'0' || *cur > b'9') && *cur != b'-' && *cur != b'+' {
            return -1;
        }

        if *cur == b'-' {
            isneg = 1;
            cur = cur.add(1);
        }

        let first_char: *const XmlChar = cur;

        while *cur >= b'0' && *cur <= b'9' {
            let digit: i32 = (*cur - b'0') as _;

            if (*dt).year > i64::MAX / 10 {
                return 2;
            }
            (*dt).year *= 10;
            if (*dt).year > i64::MAX - digit as i64 {
                return 2;
            }
            (*dt).year += digit as i64;
            cur = cur.add(1);
            digcnt += 1;
        }

        // year must be at least 4 digits (CCYY); over 4
        // digits cannot have a leading zero.
        if digcnt < 4 || (digcnt > 4 && *first_char == b'0') {
            return 1;
        }

        if isneg != 0 {
            (*dt).year = -(*dt).year;
        }

        if !VALID_YEAR!((*dt).year) {
            return 2;
        }

        *str = cur;
        0
    }
}

/// Check that @dateTime conforms to the lexical space of one of the date types.
/// if true a value is computed and returned in @val.
///
/// Returns 0 if this validates, a positive error code number otherwise
/// and -1 in case of internal or API error.
#[doc(alias = "xmlSchemaValidateDates")]
unsafe fn xml_schema_validate_dates(
    typ: XmlSchemaValType,
    date_time: *const XmlChar,
    val: *mut XmlSchemaValPtr,
    collapse: i32,
) -> i32 {
    unsafe {
        let mut ret: i32;
        let mut cur: *const XmlChar = date_time;

        if date_time.is_null() {
            return -1;
        }

        if collapse != 0 {
            while IS_WSP_BLANK_CH!(*cur) {
                cur = cur.add(1);
            }
        }

        if *cur != b'-' && *cur < b'0' && *cur > b'9' {
            return 1;
        }

        let dt: XmlSchemaValPtr = xml_schema_new_value(XmlSchemaValType::XmlSchemasUnknown);
        if dt.is_null() {
            return -1;
        }

        'error: {
            if *cur.add(0) == b'-' && *cur.add(1) == b'-' {
                // It's an incomplete date (xs:gMonthDay, xs:gMonth or xs:gDay)
                cur = cur.add(2);

                // is it an xs:gDay?
                if *cur == b'-' {
                    if typ == XmlSchemaValType::XmlSchemasGMonth {
                        break 'error;
                    }
                    cur = cur.add(1);
                    ret = _xml_schema_parse_gday(addr_of_mut!((*dt).value.date), addr_of_mut!(cur));
                    if ret != 0 {
                        break 'error;
                    }

                    RETURN_TYPE_IF_VALID!(cur, ret, dt, typ, val, XmlSchemaValType::XmlSchemasGDay);

                    break 'error;
                }

                // it should be an xs:gMonthDay or xs:gMonth
                ret = _xml_schema_parse_gmonth(addr_of_mut!((*dt).value.date), addr_of_mut!(cur));
                if ret != 0 {
                    break 'error;
                }

                // a '-' c_char could indicate this type is xs:gMonthDay or
                // a negative time zone offset. Check for xs:gMonthDay first.
                // Also the first three c_char's of a negative tzo (-MM:SS) can
                // appear to be a valid day; so even if the day portion
                // of the xs:gMonthDay verifies, we must insure it was not
                // a tzo.
                if *cur == b'-' {
                    let rewnd: *const XmlChar = cur;
                    cur = cur.add(1);

                    ret = _xml_schema_parse_gday(addr_of_mut!((*dt).value.date), addr_of_mut!(cur));
                    if ret == 0 && (*cur == 0 || *cur != b':') {
                        // we can use the VALID_MDAY macro to validate the month
                        // and day because the leap year test will flag year zero
                        // as a leap year (even though zero is an invalid year).
                        // FUTURE TODO: Zero will become valid in XML Schema 1.1
                        // probably.
                        if VALID_MDAY!((addr_of_mut!((*dt).value.date))) {
                            RETURN_TYPE_IF_VALID!(
                                cur,
                                ret,
                                dt,
                                typ,
                                val,
                                XmlSchemaValType::XmlSchemasGMonthDay
                            );

                            break 'error;
                        }
                    }

                    // not xs:gMonthDay so rewind and check if just xs:gMonth
                    // with an optional time zone.
                    cur = rewnd;
                }

                RETURN_TYPE_IF_VALID!(cur, ret, dt, typ, val, XmlSchemaValType::XmlSchemasGMonth);

                break 'error;
            }

            // It's a right-truncated date or an xs:time.
            // Try to parse an xs:time then fallback on right-truncated dates.
            if *cur >= b'0' && *cur <= b'9' {
                ret = _xml_schema_parse_time(addr_of_mut!((*dt).value.date), addr_of_mut!(cur));
                if ret == 0 {
                    // it's an xs:time
                    RETURN_TYPE_IF_VALID!(cur, ret, dt, typ, val, XmlSchemaValType::XmlSchemasTime);
                }
            }

            // fallback on date parsing
            cur = date_time;

            ret = _xml_schema_parse_gyear(addr_of_mut!((*dt).value.date), addr_of_mut!(cur));
            if ret != 0 {
                break 'error;
            }

            // is it an xs:gYear?
            RETURN_TYPE_IF_VALID!(cur, ret, dt, typ, val, XmlSchemaValType::XmlSchemasGYear);

            if *cur != b'-' {
                break 'error;
            }
            cur = cur.add(1);

            ret = _xml_schema_parse_gmonth(addr_of_mut!((*dt).value.date), addr_of_mut!(cur));
            if ret != 0 {
                break 'error;
            }

            // is it an xs:gYearMonth?
            RETURN_TYPE_IF_VALID!(
                cur,
                ret,
                dt,
                typ,
                val,
                XmlSchemaValType::XmlSchemasGYearMonth
            );

            if *cur != b'-' {
                break 'error;
            }
            cur = cur.add(1);

            ret = _xml_schema_parse_gday(addr_of_mut!((*dt).value.date), addr_of_mut!(cur));
            if ret != 0 || !VALID_DATE!((addr_of_mut!((*dt).value.date))) {
                break 'error;
            }

            // is it an xs:date?
            RETURN_TYPE_IF_VALID!(cur, ret, dt, typ, val, XmlSchemaValType::XmlSchemasDate);

            if *cur != b'T' {
                break 'error;
            }
            cur = cur.add(1);

            // it should be an xs:dateTime
            ret = _xml_schema_parse_time(addr_of_mut!((*dt).value.date), addr_of_mut!(cur));
            if ret != 0 {
                break 'error;
            }

            ret = _xml_schema_parse_time_zone(addr_of_mut!((*dt).value.date), addr_of_mut!(cur));
            if collapse != 0 {
                while IS_WSP_BLANK_CH!(*cur) {
                    cur = cur.add(1);
                }
            }
            if ret != 0 || *cur != 0 || !VALID_DATETIME!(addr_of_mut!((*dt).value.date)) {
                break 'error;
            }

            (*dt).typ = XmlSchemaValType::XmlSchemasDatetime;

            // done:
            // #if 1
            if typ != XmlSchemaValType::XmlSchemasUnknown && typ != (*dt).typ {
                break 'error;
            }
            // #else
            //     /*
            //      * insure the parsed type is equal to or less significant (right
            //      * truncated) than the desired type.
            //      */
            //     if ((type != XmlSchemaValType::XmlSchemasUnknown) && (type != (*dt).typ)) {

            //         /* time only matches time */
            //         if ((type == XmlSchemaValType::XML_SCHEMAS_TIME) && ((*dt).typ == XmlSchemaValType::XML_SCHEMAS_TIME))
            // goto error;
            //         if ((type == XmlSchemaValType::XML_SCHEMAS_DATETIME) &&
            //             (((*dt).typ != XmlSchemaValType::XML_SCHEMAS_DATE) ||
            //              ((*dt).typ != XmlSchemaValType::XML_SCHEMAS_GYEARMONTH) ||
            //              ((*dt).typ != XmlSchemaValType::XML_SCHEMAS_GYEAR)))
            // goto error;

            //         if ((type == XmlSchemaValType::XML_SCHEMAS_DATE) &&
            //             (((*dt).typ != XmlSchemaValType::XML_SCHEMAS_GYEAR) ||
            //              ((*dt).typ != XmlSchemaValType::XML_SCHEMAS_GYEARMONTH)))
            // goto error;

            //         if ((type == XmlSchemaValType::XML_SCHEMAS_GYEARMONTH) && ((*dt).typ != XmlSchemaValType::XML_SCHEMAS_GYEAR))
            // goto error;

            //         if ((type == XmlSchemaValType::XML_SCHEMAS_GMONTHDAY) && ((*dt).typ != XmlSchemaValType::XML_SCHEMAS_GMONTH))
            // goto error;
            //     }
            // #endif

            if !val.is_null() {
                *val = dt;
            } else {
                xml_schema_free_value(dt);
            }

            return 0;
        }

        // error:
        if !dt.is_null() {
            xml_schema_free_value(dt);
        }
        1
    }
}

/// Check that @duration conforms to the lexical space of the duration type.
/// if true a value is computed and returned in @val.
///
/// Returns 0 if this validates, a positive error code number otherwise
/// and -1 in case of internal or API error.
#[doc(alias = "xmlSchemaValidateDuration")]
unsafe fn xml_schema_validate_duration(
    _typ: XmlSchemaTypePtr,
    duration: *const XmlChar,
    val: *mut XmlSchemaValPtr,
    collapse: i32,
) -> i32 {
    unsafe {
        let mut cur: *const XmlChar = duration;
        let mut isneg: i32 = 0;
        let mut seq: usize = 0;
        let mut days: i64;
        let mut secs: i64 = 0;
        let mut sec_frac: f64 = 0.0;

        if duration.is_null() {
            return -1;
        }

        if collapse != 0 {
            while IS_WSP_BLANK_CH!(*cur) {
                cur = cur.add(1);
            }
        }

        if *cur == b'-' {
            isneg = 1;
            cur = cur.add(1);
        }

        // duration must start with 'P' (after sign)
        let f = *cur != b'P';
        cur = cur.add(1);
        if f {
            return 1;
        }

        if *cur == 0 {
            return 1;
        }

        let dur: XmlSchemaValPtr = xml_schema_new_value(XmlSchemaValType::XmlSchemasDuration);
        if dur.is_null() {
            return -1;
        }

        'error: {
            while *cur != 0 {
                let mut num: i64 = 0;
                let mut has_digits: usize = 0;
                let mut has_frac: i32 = 0;
                let desig: &[XmlChar] = b"YMDHMS";

                // input string should be empty or invalid date/time item
                if seq >= desig.len() {
                    break 'error;
                }

                // T designator must be present for time items
                if *cur == b'T' {
                    if seq > 3 {
                        break 'error;
                    }
                    cur = cur.add(1);
                    seq = 3;
                } else if seq == 3 {
                    break 'error;
                }

                // Parse integral part.
                while *cur >= b'0' && *cur <= b'9' {
                    let digit: i64 = (*cur - b'0') as _;

                    if num > i64::MAX / 10 {
                        break 'error;
                    }
                    num *= 10;
                    if num > i64::MAX - digit {
                        break 'error;
                    }
                    num += digit;

                    has_digits = 1;
                    cur = cur.add(1);
                }

                if *cur == b'.' {
                    // Parse fractional part.
                    let mut mult: f64 = 1.0;
                    cur = cur.add(1);
                    has_frac = 1;
                    while *cur >= b'0' && *cur <= b'9' {
                        mult /= 10.0;
                        sec_frac += (*cur - b'0') as f64 * mult;
                        has_digits = 1;
                        cur = cur.add(1);
                    }
                }

                while *cur != desig[seq] {
                    seq += 1;
                    // No T designator or invalid c_char.
                    if seq == 3 || seq == desig.len() {
                        break 'error;
                    }
                }
                cur = cur.add(1);

                if has_digits == 0 || (has_frac != 0 && seq != 5) {
                    break 'error;
                }

                match seq {
                    0 => {
                        // Year
                        if num > i64::MAX / 12 {
                            break 'error;
                        }
                        (*dur).value.dur.mon = num * 12;
                    }
                    1 => {
                        // Month
                        if (*dur).value.dur.mon > i64::MAX - num {
                            break 'error;
                        }
                        (*dur).value.dur.mon += num;
                    }
                    2 => {
                        // Day
                        (*dur).value.dur.day = num;
                    }
                    3 => {
                        // Hour
                        days = num / HOURS_PER_DAY as i64;
                        if (*dur).value.dur.day > i64::MAX - days {
                            break 'error;
                        }
                        (*dur).value.dur.day += days;
                        secs = (num % HOURS_PER_DAY as i64) * SECS_PER_HOUR as i64;
                    }
                    4 => {
                        // Minute
                        days = num / MINS_PER_DAY as i64;
                        if (*dur).value.dur.day > i64::MAX - days {
                            break 'error;
                        }
                        (*dur).value.dur.day += days;
                        secs += (num % MINS_PER_DAY as i64) * SECS_PER_MIN as i64;
                    }
                    5 => {
                        // Second
                        days = num / SECS_PER_DAY as i64;
                        if (*dur).value.dur.day > i64::MAX - days {
                            break 'error;
                        }
                        (*dur).value.dur.day += days;
                        secs += num % SECS_PER_DAY as i64;
                    }
                    _ => {}
                }

                seq += 1;
            }

            days = secs / SECS_PER_DAY as i64;
            if (*dur).value.dur.day > i64::MAX - days {
                break 'error;
            }
            (*dur).value.dur.day += days;
            (*dur).value.dur.sec = (secs % SECS_PER_DAY as i64) as f64 + sec_frac;

            if isneg != 0 {
                (*dur).value.dur.mon = -(*dur).value.dur.mon;
                (*dur).value.dur.day = -(*dur).value.dur.day;
                (*dur).value.dur.sec = -(*dur).value.dur.sec;
            }

            if !val.is_null() {
                *val = dur;
            } else {
                xml_schema_free_value(dur);
            }

            return 0;
        }

        // error:
        if !dur.is_null() {
            xml_schema_free_value(dur);
        }
        1
    }
}

/// Check that a value conforms to the lexical space of the language datatype.
/// Must conform to [a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*
///
/// Returns 1 if this validates, 0 otherwise.
#[doc(alias = "xmlSchemaCheckLanguageType")]
unsafe fn xml_schema_check_language_type(value: *const XmlChar) -> i32 {
    unsafe {
        let mut first: i32 = 1;
        let mut len: i32 = 0;
        let mut cur: *const XmlChar = value;

        if value.is_null() {
            return 0;
        }

        while *cur.add(0) != 0 {
            if !((*cur.add(0) >= b'a' && *cur.add(0) <= b'z')
                || (*cur.add(0) >= b'A' && *cur.add(0) <= b'Z')
                || *cur.add(0) == b'-'
                || (first == 0 && xml_is_digit(*cur.add(0) as u32)))
            {
                return 0;
            }
            if *cur.add(0) == b'-' {
                if !(1..=8).contains(&len) {
                    return 0;
                }
                len = 0;
                first = 0;
            } else {
                len += 1;
            }
            cur = cur.add(1);
        }
        if !(1..=8).contains(&len) {
            return 0;
        }

        1
    }
}

/// Check that a value conforms to the lexical space of the predefined
/// list type. if true a value is computed and returned in @ret.
///
/// Returns the number of items if this validates, a negative error code number otherwise
#[doc(alias = "xmlSchemaValAtomicListNode")]
unsafe fn xml_schema_val_atomic_list_node(
    typ: XmlSchemaTypePtr,
    value: *const XmlChar,
    ret: *mut XmlSchemaValPtr,
    node: Option<XmlGenericNodePtr>,
) -> i32 {
    unsafe {
        let mut cur: *mut XmlChar;
        let mut nb_values: i32 = 0;
        let mut tmp: i32 = 0;

        if value.is_null() {
            return -1;
        }
        let val: *mut XmlChar = xml_strdup(value);
        if val.is_null() {
            return -1;
        }
        if !ret.is_null() {
            *ret = null_mut();
        }
        cur = val;
        // Split the list
        while xml_is_blank_char(*cur as u32) {
            *cur = 0;
            cur = cur.add(1);
        }
        while *cur != 0 {
            if xml_is_blank_char(*cur as u32) {
                *cur = 0;
                cur = cur.add(1);
                while xml_is_blank_char(*cur as u32) {
                    *cur = 0;
                    cur = cur.add(1);
                }
            } else {
                nb_values += 1;
                cur = cur.add(1);
                while *cur != 0 && !xml_is_blank_char(*cur as u32) {
                    cur = cur.add(1);
                }
            }
        }
        if nb_values == 0 {
            xml_free(val as _);
            return nb_values;
        }
        let endval: *mut XmlChar = cur;
        cur = val;
        while *cur == 0 && cur != endval {
            cur = cur.add(1);
        }
        while cur != endval {
            tmp = xml_schema_val_predef_type_node(typ, cur, null_mut(), node);
            if tmp != 0 {
                break;
            }
            while *cur != 0 {
                cur = cur.add(1);
            }
            while *cur == 0 && cur != endval {
                cur = cur.add(1);
            }
        }
        /* TODO what return value ? c.f. bug #158628
        if !ret.is_null() {
        TODO
        } */
        xml_free(val as _);
        if tmp == 0 {
            return nb_values;
        }
        -1
    }
}

/// Removes the leading and ending spaces of a string
///
/// Returns the new string or NULL if no change was required.
#[doc(alias = "xmlSchemaStrip")]
unsafe fn xml_schema_strip(value: *const XmlChar) -> *mut XmlChar {
    unsafe {
        let mut start: *const XmlChar = value;
        let mut end: *const XmlChar;

        if value.is_null() {
            return null_mut();
        }
        while *start != 0 && xml_is_blank_char(*start as u32) {
            start = start.add(1);
        }
        end = start;
        while *end != 0 {
            end = end.add(1);
        }
        let f: *const XmlChar = end;
        end = end.sub(1);
        while end > start && xml_is_blank_char(*end as u32) {
            end = end.sub(1);
        }
        end = end.add(1);
        if start == value && f == end {
            return null_mut();
        }
        xml_strndup(start, end.offset_from(start) as _)
    }
}

/// Converts a base64 encoded character to its base 64 value.
///
/// Returns 0-63 (value), 64 (pad), or -1 (not recognized)
#[doc(alias = "_xmlSchemaBase64Decode")]
unsafe fn _xml_schema_base64_decode(ch: XmlChar) -> i32 {
    if ch.is_ascii_uppercase() {
        (ch - b'A') as i32
    } else if ch.is_ascii_lowercase() {
        (ch - b'a' + 26) as i32
    } else if ch.is_ascii_digit() {
        (ch - b'0' + 52) as i32
    } else if b'+' == ch {
        62
    } else if b'/' == ch {
        63
    } else if b'=' == ch {
        64
    } else {
        -1
    }
}

/// Check that a value conforms to the lexical space of the atomic type.
/// if true a value is computed and returned in @val.
/// This checks the value space for list types as well (IDREFS, NMTOKENS).
///
/// Returns 0 if this validates, a positive error code number otherwise
/// and -1 in case of internal or API error.
#[doc(alias = "xmlSchemaValAtomicType")]
#[allow(clippy::too_many_arguments)]
unsafe fn xml_schema_val_atomic_type(
    typ: XmlSchemaTypePtr,
    mut value: *const XmlChar,
    val: *mut XmlSchemaValPtr,
    node: Option<XmlGenericNodePtr>,
    flags: i32,
    ws: XmlSchemaWhitespaceValueType,
    norm_on_the_fly: i32,
    apply_norm: i32,
    create_string_value: i32,
) -> i32 {
    unsafe {
        let v: XmlSchemaValPtr;
        let mut norm: *mut XmlChar = null_mut();
        let mut ret: i32;

        if !XML_SCHEMA_TYPES_INITIALIZED.get() && xml_schema_init_types() < 0 {
            return -1;
        }
        if typ.is_null() {
            return -1;
        }

        // validating a non existent text node is similar to validating
        // an empty one.
        if value.is_null() {
            value = c"".as_ptr() as _;
        }

        if !val.is_null() {
            *val = null_mut();
        }
        if flags == 0
            && !value.is_null()
            && ((*typ).built_in_type != XmlSchemaValType::XmlSchemasString as i32
                && (*typ).built_in_type != XmlSchemaValType::XmlSchemasAnytype as i32
                && (*typ).built_in_type != XmlSchemaValType::XmlSchemasAnySimpletype as i32)
        {
            if (*typ).built_in_type == XmlSchemaValType::XmlSchemasNormString as i32 {
                norm = xml_schema_white_space_replace(
                    CStr::from_ptr(value as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                )
                .map_or(null_mut(), |res| {
                    xml_strndup(res.as_ptr(), res.len() as i32)
                });
            } else if let Some(res) = xml_schema_collapse_string(
                CStr::from_ptr(value as *const i8)
                    .to_string_lossy()
                    .as_ref(),
            ) {
                norm = xml_strndup(res.as_ptr(), res.len() as i32);
            }
            if !norm.is_null() {
                value = norm;
            }
        }

        'error: {
            'return0: {
                'return1: {
                    'return3: {
                        'done: {
                            match XmlSchemaValType::try_from((*typ).built_in_type).unwrap() {
                                XmlSchemaValType::XmlSchemasUnknown => {
                                    break 'error;
                                }
                                XmlSchemaValType::XmlSchemasAnytype
                                | XmlSchemaValType::XmlSchemasAnySimpletype => {
                                    if create_string_value != 0 && !val.is_null() {
                                        v = xml_schema_new_value(
                                            XmlSchemaValType::XmlSchemasAnySimpletype,
                                        );
                                        if !v.is_null() {
                                            (*v).value.str = xml_strdup(value as _) as _;
                                            *val = v;
                                        } else {
                                            break 'error;
                                        }
                                    }
                                    break 'return0;
                                }
                                XmlSchemaValType::XmlSchemasString => {
                                    if norm_on_the_fly == 0 {
                                        let mut cur: *const XmlChar = value;

                                        if ws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceReplace {
                                            while *cur != 0 {
                                                if *cur == 0xd || *cur == 0xa || *cur == 0x9 {
                                                    break 'return1;
                                                } else {
                                                    cur = cur.add(1);
                                                }
                                            }
                                        } else if ws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse {
                                            while *cur != 0 {
                                                if *cur == 0xd || *cur == 0xa || *cur == 0x9 {
                                                    break 'return1;
                                                } else if IS_WSP_SPACE_CH!(*cur) {
                                                    cur = cur.add(1);
                                                    if IS_WSP_SPACE_CH!(*cur) {
                                                        break 'return1;
                                                    }
                                                } else {
                                                    cur = cur.add(1);
                                                }
                                            }
                                        }
                                    }
                                    if create_string_value != 0 && !val.is_null() {
                                        if apply_norm != 0 {
                                            if ws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse {
                                                if let Some(res) = xml_schema_collapse_string(CStr::from_ptr(value as *const i8).to_string_lossy().as_ref()) {
                                                    norm = xml_strndup(res.as_ptr(), res.len() as i32);
                                                }
                                            } else if ws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceReplace {
                                                norm = xml_schema_white_space_replace(CStr::from_ptr(value as *const i8).to_string_lossy().as_ref()).map_or(null_mut(), |res| xml_strndup(res.as_ptr(), res.len() as i32));
                                            }
                                            if !norm.is_null() {
                                                value = norm;
                                            }
                                        }
                                        v = xml_schema_new_value(
                                            XmlSchemaValType::XmlSchemasString,
                                        );
                                        if !v.is_null() {
                                            (*v).value.str = xml_strdup(value);
                                            *val = v;
                                        } else {
                                            break 'error;
                                        }
                                    }
                                    break 'return0;
                                }
                                XmlSchemaValType::XmlSchemasNormString => {
                                    if norm_on_the_fly != 0 {
                                        if apply_norm != 0 {
                                            if ws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse {
                                                if let Some(res) = xml_schema_collapse_string(CStr::from_ptr(value as *const i8).to_string_lossy().as_ref()) {
                                                    norm = xml_strndup(res.as_ptr(), res.len() as i32);
                                                }
                                            } else {
                                                norm = xml_schema_white_space_replace(CStr::from_ptr(value as *const i8).to_string_lossy().as_ref()).map_or(null_mut(), |res| xml_strndup(res.as_ptr(), res.len() as i32));
                                            }
                                            if !norm.is_null() {
                                                value = norm;
                                            }
                                        }
                                    } else {
                                        let mut cur: *const XmlChar = value;
                                        while *cur != 0 {
                                            if *cur == 0xd || *cur == 0xa || *cur == 0x9 {
                                                break 'return1;
                                            } else {
                                                cur = cur.add(1);
                                            }
                                        }
                                    }
                                    if !val.is_null() {
                                        v = xml_schema_new_value(
                                            XmlSchemaValType::XmlSchemasNormString,
                                        );
                                        if !v.is_null() {
                                            (*v).value.str = xml_strdup(value);
                                            *val = v;
                                        } else {
                                            break 'error;
                                        }
                                    }
                                    break 'return0;
                                }
                                XmlSchemaValType::XmlSchemasDecimal => {
                                    let mut cur: *const XmlChar = value;
                                    let mut len: u32;
                                    let mut neg: u32;
                                    let mut integ: u32;
                                    let mut has_leading_zeroes: u32;
                                    let mut cval: [XmlChar; 25] = [0; 25];
                                    let mut cptr: *mut XmlChar = cval.as_mut_ptr();

                                    if cur.is_null() || *cur == 0 {
                                        break 'return1;
                                    }

                                    // xs:decimal has a whitespace-facet value of 'collapse'.
                                    if norm_on_the_fly != 0 {
                                        while IS_WSP_BLANK_CH!(*cur) {
                                            cur = cur.add(1);
                                        }
                                    }

                                    // First we handle an optional sign.
                                    neg = 0;
                                    if *cur == b'-' {
                                        neg = 1;
                                        cur = cur.add(1);
                                    } else if *cur == b'+' {
                                        cur = cur.add(1);
                                    }
                                    // Disallow: "", "-", "- "
                                    if *cur == 0 {
                                        break 'return1;
                                    }
                                    // Next we "pre-parse" the number, in preparation for calling
                                    // the common routine xmlSchemaParseUInt.  We get rid of any
                                    // leading zeroes (because we have reserved only 25 chars),
                                    // and note the position of a decimal point.
                                    len = 0;
                                    integ = !0;
                                    has_leading_zeroes = 0;
                                    // Skip leading zeroes.
                                    while *cur == b'0' {
                                        cur = cur.add(1);
                                        has_leading_zeroes = 1;
                                    }
                                    if *cur != 0 {
                                        loop {
                                            if *cur >= b'0' && *cur <= b'9' {
                                                *cptr = *cur;
                                                cptr = cptr.add(1);
                                                cur = cur.add(1);
                                                len += 1;
                                            } else if *cur == b'.' {
                                                cur = cur.add(1);
                                                integ = len;
                                                loop {
                                                    if *cur >= b'0' && *cur <= b'9' {
                                                        *cptr = *cur;
                                                        cptr = cptr.add(1);
                                                        cur = cur.add(1);
                                                        len += 1;
                                                    } else {
                                                        break;
                                                    }

                                                    if len >= 24 {
                                                        break;
                                                    }
                                                }
                                                // Disallow "." but allow "00."
                                                if len == 0 && has_leading_zeroes == 0 {
                                                    break 'return1;
                                                }
                                                break;
                                            } else {
                                                break;
                                            }

                                            if len >= 24 {
                                                break;
                                            }
                                        }
                                    }
                                    if norm_on_the_fly != 0 {
                                        while IS_WSP_BLANK_CH!(*cur) {
                                            cur = cur.add(1);
                                        }
                                    }
                                    if *cur != 0 {
                                        break 'return1;
                                        // error if any extraneous chars
                                    }
                                    if !val.is_null() {
                                        v = xml_schema_new_value(
                                            XmlSchemaValType::XmlSchemasDecimal,
                                        );
                                        if !v.is_null() {
                                            // Now evaluate the significant digits of the number
                                            if len != 0 {
                                                if integ != !0 {
                                                    // Get rid of trailing zeroes in the
                                                    // fractional part.
                                                    while len != integ && *cptr.sub(1) == b'0' {
                                                        cptr = cptr.sub(1);
                                                        len = len.wrapping_sub(1);
                                                    }
                                                }
                                                // Terminate the (preparsed) string.
                                                if len != 0 {
                                                    *cptr = 0;
                                                    cptr = cval.as_mut_ptr();

                                                    xml_schema_parse_uint(
                                                        addr_of_mut!(cptr) as _,
                                                        addr_of_mut!((*v).value.decimal.lo),
                                                        addr_of_mut!((*v).value.decimal.mi),
                                                        addr_of_mut!((*v).value.decimal.hi),
                                                    );
                                                }
                                            }
                                            // Set the total digits to 1 if a zero value.
                                            (*v).value.decimal.sign = neg;
                                            if len == 0 {
                                                // Speedup for zero values.
                                                (*v).value.decimal.total = 1;
                                            } else {
                                                (*v).value.decimal.total = len;
                                                if integ == !0 {
                                                    (*v).value.decimal.frac = 0;
                                                } else {
                                                    (*v).value.decimal.frac = len - integ;
                                                }
                                            }
                                            *val = v;
                                        }
                                    }
                                    break 'return0;
                                }
                                XmlSchemaValType::XmlSchemasTime
                                | XmlSchemaValType::XmlSchemasGDay
                                | XmlSchemaValType::XmlSchemasGMonth
                                | XmlSchemaValType::XmlSchemasGMonthDay
                                | XmlSchemaValType::XmlSchemasGYear
                                | XmlSchemaValType::XmlSchemasGYearMonth
                                | XmlSchemaValType::XmlSchemasDate
                                | XmlSchemaValType::XmlSchemasDatetime => {
                                    ret = xml_schema_validate_dates(
                                        (*typ).built_in_type.try_into().unwrap(),
                                        value,
                                        val,
                                        norm_on_the_fly,
                                    );
                                }
                                XmlSchemaValType::XmlSchemasDuration => {
                                    ret = xml_schema_validate_duration(
                                        typ,
                                        value,
                                        val,
                                        norm_on_the_fly,
                                    );
                                }
                                XmlSchemaValType::XmlSchemasFloat
                                | XmlSchemaValType::XmlSchemasDouble => {
                                    let mut cur: *const XmlChar = value;
                                    let mut neg: i32 = 0;
                                    let mut digits_before: i32 = 0;
                                    let mut digits_after: i32 = 0;

                                    if norm_on_the_fly != 0 {
                                        while IS_WSP_BLANK_CH!(*cur) {
                                            cur = cur.add(1);
                                        }
                                    }

                                    if *cur.add(0) == b'N'
                                        && *cur.add(1) == b'a'
                                        && *cur.add(2) == b'N'
                                    {
                                        cur = cur.add(3);
                                        if *cur != 0 {
                                            break 'return1;
                                        }
                                        if !val.is_null() {
                                            if typ == XML_SCHEMA_TYPE_FLOAT_DEF.get() {
                                                v = xml_schema_new_value(
                                                    XmlSchemaValType::XmlSchemasFloat,
                                                );
                                                if !v.is_null() {
                                                    (*v).value.f = XML_XPATH_NAN as f32;
                                                } else {
                                                    xml_schema_free_value(v);
                                                    break 'error;
                                                }
                                            } else {
                                                v = xml_schema_new_value(
                                                    XmlSchemaValType::XmlSchemasDouble,
                                                );
                                                if !v.is_null() {
                                                    (*v).value.d = XML_XPATH_NAN;
                                                } else {
                                                    xml_schema_free_value(v);
                                                    break 'error;
                                                }
                                            }
                                            *val = v;
                                        }
                                        break 'return0;
                                    }
                                    if *cur == b'-' {
                                        neg = 1;
                                        cur = cur.add(1);
                                    }
                                    if *cur.add(0) == b'I'
                                        && *cur.add(1) == b'N'
                                        && *cur.add(2) == b'F'
                                    {
                                        cur = cur.add(3);
                                        if *cur != 0 {
                                            break 'return1;
                                        }
                                        if !val.is_null() {
                                            if typ == XML_SCHEMA_TYPE_FLOAT_DEF.get() {
                                                v = xml_schema_new_value(
                                                    XmlSchemaValType::XmlSchemasFloat,
                                                );
                                                if !v.is_null() {
                                                    if neg != 0 {
                                                        (*v).value.f = XML_XPATH_NINF as f32;
                                                    } else {
                                                        (*v).value.f = XML_XPATH_PINF as f32;
                                                    }
                                                } else {
                                                    xml_schema_free_value(v);
                                                    break 'error;
                                                }
                                            } else {
                                                v = xml_schema_new_value(
                                                    XmlSchemaValType::XmlSchemasDouble,
                                                );
                                                if !v.is_null() {
                                                    if neg != 0 {
                                                        (*v).value.d = XML_XPATH_NINF;
                                                    } else {
                                                        (*v).value.d = XML_XPATH_PINF;
                                                    }
                                                } else {
                                                    xml_schema_free_value(v);
                                                    break 'error;
                                                }
                                            }
                                            *val = v;
                                        }
                                        break 'return0;
                                    }
                                    if neg == 0 && *cur == b'+' {
                                        cur = cur.add(1);
                                    }
                                    if *cur.add(0) == 0
                                        || *cur.add(0) == b'+'
                                        || *cur.add(0) == b'-'
                                    {
                                        break 'return1;
                                    }
                                    while *cur >= b'0' && *cur <= b'9' {
                                        cur = cur.add(1);
                                        digits_before += 1;
                                    }
                                    if *cur == b'.' {
                                        cur = cur.add(1);
                                        while *cur >= b'0' && *cur <= b'9' {
                                            cur = cur.add(1);
                                            digits_after += 1;
                                        }
                                    }
                                    if digits_before == 0 && digits_after == 0 {
                                        break 'return1;
                                    }
                                    if *cur == b'e' || *cur == b'E' {
                                        cur = cur.add(1);
                                        if *cur == b'-' || *cur == b'+' {
                                            cur = cur.add(1);
                                        }
                                        while *cur >= b'0' && *cur <= b'9' {
                                            cur = cur.add(1);
                                        }
                                    }
                                    if norm_on_the_fly != 0 {
                                        while IS_WSP_BLANK_CH!(*cur) {
                                            cur = cur.add(1);
                                        }
                                    }

                                    if *cur != 0 {
                                        break 'return1;
                                    }
                                    if !val.is_null() {
                                        if typ == XML_SCHEMA_TYPE_FLOAT_DEF.get() {
                                            v = xml_schema_new_value(
                                                XmlSchemaValType::XmlSchemasFloat,
                                            );
                                            if !v.is_null() {
                                                // TODO: sscanf seems not to give the correct
                                                // value for extremely high/low values.
                                                // E.g. "1E-149" results in zero.
                                                if sscanf(
                                                    value as _,
                                                    c"%f".as_ptr() as _,
                                                    addr_of_mut!((*v).value.f),
                                                ) == 1
                                                {
                                                    *val = v;
                                                } else {
                                                    xml_schema_free_value(v);
                                                    break 'return1;
                                                }
                                            } else {
                                                break 'error;
                                            }
                                        } else {
                                            v = xml_schema_new_value(
                                                XmlSchemaValType::XmlSchemasDouble,
                                            );
                                            if !v.is_null() {
                                                // TODO: sscanf seems not to give the correct
                                                // value for extremely high/low values.
                                                if sscanf(
                                                    value as _,
                                                    c"%lf".as_ptr() as _,
                                                    addr_of_mut!((*v).value.d),
                                                ) == 1
                                                {
                                                    *val = v;
                                                } else {
                                                    xml_schema_free_value(v);
                                                    break 'return1;
                                                }
                                            } else {
                                                break 'error;
                                            }
                                        }
                                    }
                                    break 'return0;
                                }
                                XmlSchemaValType::XmlSchemasBoolean => {
                                    let mut cur: *const XmlChar = value;

                                    if norm_on_the_fly != 0 {
                                        while IS_WSP_BLANK_CH!(*cur) {
                                            cur = cur.add(1);
                                        }
                                        if *cur == b'0' {
                                            ret = 0;
                                            cur = cur.add(1);
                                        } else if *cur == b'1' {
                                            ret = 1;
                                            cur = cur.add(1);
                                        } else if *cur == b't' {
                                            cur = cur.add(1);
                                            if {
                                                let f = *cur == b'r';
                                                cur = cur.add(1);
                                                f
                                            } && {
                                                let f = *cur == b'u';
                                                cur = cur.add(1);
                                                f
                                            } && {
                                                let f = *cur == b'e';
                                                cur = cur.add(1);
                                                f
                                            } {
                                                ret = 1;
                                            } else {
                                                break 'return1;
                                            }
                                        } else if *cur == b'f' {
                                            cur = cur.add(1);
                                            if {
                                                let f = *cur == b'a';
                                                cur = cur.add(1);
                                                f
                                            } && {
                                                let f = *cur == b'l';
                                                cur = cur.add(1);
                                                f
                                            } && {
                                                let f = *cur == b's';
                                                cur = cur.add(1);
                                                f
                                            } && {
                                                let f = *cur == b'e';
                                                cur = cur.add(1);
                                                f
                                            } {
                                                ret = 0;
                                            } else {
                                                break 'return1;
                                            }
                                        } else {
                                            break 'return1;
                                        }
                                        if *cur != 0 {
                                            while IS_WSP_BLANK_CH!(*cur) {
                                                cur = cur.add(1);
                                            }
                                            if *cur != 0 {
                                                break 'return1;
                                            }
                                        }
                                    } else if *cur.add(0) == b'0' && *cur.add(1) == 0 {
                                        ret = 0;
                                    } else if (*cur.add(0) == b'1' && *cur.add(1) == 0)
                                        || (*cur.add(0) == b't'
                                            && *cur.add(1) == b'r'
                                            && *cur.add(2) == b'u'
                                            && *cur.add(3) == b'e'
                                            && *cur.add(4) == 0)
                                    {
                                        ret = 1;
                                    } else if *cur.add(0) == b'f'
                                        && *cur.add(1) == b'a'
                                        && *cur.add(2) == b'l'
                                        && *cur.add(3) == b's'
                                        && *cur.add(4) == b'e'
                                        && *cur.add(5) == 0
                                    {
                                        ret = 0;
                                    } else {
                                        break 'return1;
                                    }
                                    if !val.is_null() {
                                        v = xml_schema_new_value(
                                            XmlSchemaValType::XmlSchemasBoolean,
                                        );
                                        if !v.is_null() {
                                            (*v).value.b = ret;
                                            *val = v;
                                        } else {
                                            break 'error;
                                        }
                                    }
                                    break 'return0;
                                }
                                XmlSchemaValType::XmlSchemasToken => {
                                    let mut cur: *const XmlChar = value;

                                    if norm_on_the_fly == 0 {
                                        while *cur != 0 {
                                            if *cur == 0xd || *cur == 0xa || *cur == 0x9 {
                                                break 'return1;
                                            } else if *cur == b' ' {
                                                cur = cur.add(1);
                                                if *cur == 0 {
                                                    break 'return1;
                                                }
                                                if *cur == b' ' {
                                                    break 'return1;
                                                }
                                            } else {
                                                cur = cur.add(1);
                                            }
                                        }
                                    }
                                    if !val.is_null() {
                                        v = xml_schema_new_value(XmlSchemaValType::XmlSchemasToken);
                                        if !v.is_null() {
                                            (*v).value.str = xml_strdup(value);
                                            *val = v;
                                        } else {
                                            break 'error;
                                        }
                                    }
                                    break 'return0;
                                }
                                XmlSchemaValType::XmlSchemasLanguage => {
                                    if norm.is_null() && norm_on_the_fly != 0 {
                                        if let Some(res) = xml_schema_collapse_string(
                                            CStr::from_ptr(value as *const i8)
                                                .to_string_lossy()
                                                .as_ref(),
                                        ) {
                                            norm = xml_strndup(res.as_ptr(), res.len() as i32);
                                        }
                                        if !norm.is_null() {
                                            value = norm;
                                        }
                                    }

                                    if xml_schema_check_language_type(value) == 1 {
                                        if !val.is_null() {
                                            v = xml_schema_new_value(
                                                XmlSchemaValType::XmlSchemasLanguage,
                                            );
                                            if !v.is_null() {
                                                (*v).value.str = xml_strdup(value);
                                                *val = v;
                                            } else {
                                                break 'error;
                                            }
                                        }
                                        break 'return0;
                                    }
                                    break 'return1;
                                }
                                XmlSchemaValType::XmlSchemasNmtoken => {
                                    if xml_validate_nmtoken(value, 1) == 0 {
                                        if !val.is_null() {
                                            v = xml_schema_new_value(
                                                XmlSchemaValType::XmlSchemasNmtoken,
                                            );
                                            if !v.is_null() {
                                                (*v).value.str = xml_strdup(value);
                                                *val = v;
                                            } else {
                                                break 'error;
                                            }
                                        }
                                        break 'return0;
                                    }
                                    break 'return1;
                                }
                                XmlSchemaValType::XmlSchemasNmtokens => {
                                    ret = xml_schema_val_atomic_list_node(
                                        XML_SCHEMA_TYPE_NMTOKEN_DEF.get(),
                                        value,
                                        val,
                                        node,
                                    );
                                    if ret > 0 {
                                        ret = 0;
                                    } else {
                                        ret = 1;
                                    }
                                    break 'done;
                                }
                                XmlSchemaValType::XmlSchemasName => {
                                    ret = xml_validate_name(value, 1);
                                    if ret == 0 && !val.is_null() && !value.is_null() {
                                        v = xml_schema_new_value(XmlSchemaValType::XmlSchemasName);
                                        if !v.is_null() {
                                            let mut start: *const XmlChar = value;
                                            let mut end: *const XmlChar;
                                            while xml_is_blank_char(*start as u32) {
                                                start = start.add(1);
                                            }
                                            end = start;
                                            while *end != 0 && !xml_is_blank_char(*end as u32) {
                                                end = end.add(1);
                                            }
                                            (*v).value.str =
                                                xml_strndup(start, end.offset_from(start) as _)
                                                    as _;
                                            *val = v;
                                        } else {
                                            break 'error;
                                        }
                                    }
                                    break 'done;
                                }
                                XmlSchemaValType::XmlSchemasQName => {
                                    let mut uri: *const XmlChar = null();
                                    let mut local: *mut XmlChar = null_mut();

                                    ret = xml_validate_qname(value, 1);
                                    if ret != 0 {
                                        break 'done;
                                    }
                                    if let Some(node) = node {
                                        let mut prefix: *mut XmlChar = null_mut();

                                        local = xml_split_qname2(value, addr_of_mut!(prefix));
                                        let ns = node.search_ns(
                                            node.document(),
                                            (!prefix.is_null())
                                                .then(|| {
                                                    CStr::from_ptr(prefix as *const i8)
                                                        .to_string_lossy()
                                                })
                                                .as_deref(),
                                        );
                                        if ns.is_none() && !prefix.is_null() {
                                            xml_free(prefix as _);
                                            if !local.is_null() {
                                                xml_free(local as _);
                                            }
                                            break 'return1;
                                        }
                                        if let Some(ns) = ns {
                                            uri = ns.href;
                                        }
                                        if !prefix.is_null() {
                                            xml_free(prefix as _);
                                        }
                                    }
                                    if !val.is_null() {
                                        v = xml_schema_new_value(XmlSchemaValType::XmlSchemasQName);
                                        if v.is_null() {
                                            if !local.is_null() {
                                                xml_free(local as _);
                                            }
                                            break 'error;
                                        }
                                        if !local.is_null() {
                                            (*v).value.qname.name = local;
                                        } else {
                                            (*v).value.qname.name = xml_strdup(value);
                                        }
                                        if !uri.is_null() {
                                            (*v).value.qname.uri = xml_strdup(uri);
                                        }
                                        *val = v;
                                    } else if !local.is_null() {
                                        xml_free(local as _);
                                    }
                                    break 'done;
                                }
                                XmlSchemaValType::XmlSchemasNCName => {
                                    ret = validate_ncname::<true>(
                                        CStr::from_ptr(value as *const i8)
                                            .to_string_lossy()
                                            .as_ref(),
                                    )
                                    .is_err() as i32;
                                    if ret == 0 && !val.is_null() {
                                        v = xml_schema_new_value(
                                            XmlSchemaValType::XmlSchemasNCName,
                                        );
                                        if !v.is_null() {
                                            (*v).value.str = xml_strdup(value);
                                            *val = v;
                                        } else {
                                            break 'error;
                                        }
                                    }
                                    break 'done;
                                }
                                XmlSchemaValType::XmlSchemasID => {
                                    ret = validate_ncname::<true>(
                                        CStr::from_ptr(value as *const i8)
                                            .to_string_lossy()
                                            .as_ref(),
                                    )
                                    .is_err() as i32;
                                    if ret == 0 && !val.is_null() {
                                        v = xml_schema_new_value(XmlSchemaValType::XmlSchemasID);
                                        if !v.is_null() {
                                            (*v).value.str = xml_strdup(value);
                                            *val = v;
                                        } else {
                                            break 'error;
                                        }
                                    }
                                    if ret == 0 {
                                        if let Some(mut attr) =
                                            node.and_then(|node| XmlAttrPtr::try_from(node).ok())
                                        {
                                            // NOTE: the IDness might have already be declared in the DTD
                                            if !matches!(
                                                attr.atype,
                                                Some(XmlAttributeType::XmlAttributeID)
                                            ) {
                                                let strip: *mut XmlChar = xml_schema_strip(value);
                                                let res = if !strip.is_null() {
                                                    let res = xml_add_id(
                                                        null_mut(),
                                                        attr.doc.unwrap(),
                                                        CStr::from_ptr(strip as *const i8)
                                                            .to_string_lossy()
                                                            .as_ref(),
                                                        attr,
                                                    );
                                                    xml_free(strip as _);
                                                    res
                                                } else {
                                                    xml_add_id(
                                                        null_mut(),
                                                        attr.doc.unwrap(),
                                                        CStr::from_ptr(value as *const i8)
                                                            .to_string_lossy()
                                                            .as_ref(),
                                                        attr,
                                                    )
                                                };
                                                if res.is_none() {
                                                    ret = 2;
                                                } else {
                                                    attr.atype =
                                                        Some(XmlAttributeType::XmlAttributeID);
                                                }
                                            }
                                        }
                                    }
                                    break 'done;
                                }
                                XmlSchemaValType::XmlSchemasIDREF => {
                                    ret = validate_ncname::<true>(
                                        CStr::from_ptr(value as *const i8)
                                            .to_string_lossy()
                                            .as_ref(),
                                    )
                                    .is_err() as i32;
                                    if ret == 0 && !val.is_null() {
                                        v = xml_schema_new_value(XmlSchemaValType::XmlSchemasIDREF);
                                        if v.is_null() {
                                            break 'error;
                                        }
                                        (*v).value.str = xml_strdup(value);
                                        *val = v;
                                    }
                                    if ret == 0 {
                                        if let Some(mut attr) =
                                            node.and_then(|node| XmlAttrPtr::try_from(node).ok())
                                        {
                                            let strip: *mut XmlChar = xml_schema_strip(value);
                                            if !strip.is_null() {
                                                xml_add_ref(
                                                    null_mut(),
                                                    attr.doc.unwrap(),
                                                    CStr::from_ptr(strip as *const i8)
                                                        .to_string_lossy()
                                                        .as_ref(),
                                                    attr,
                                                );
                                                xml_free(strip as _);
                                            } else {
                                                xml_add_ref(
                                                    null_mut(),
                                                    attr.doc.unwrap(),
                                                    CStr::from_ptr(value as *const i8)
                                                        .to_string_lossy()
                                                        .as_ref(),
                                                    attr,
                                                );
                                            }
                                            attr.atype = Some(XmlAttributeType::XmlAttributeIDREF);
                                        }
                                    }
                                    break 'done;
                                }
                                XmlSchemaValType::XmlSchemasIDREFS => {
                                    ret = xml_schema_val_atomic_list_node(
                                        XML_SCHEMA_TYPE_IDREF_DEF.get(),
                                        value,
                                        val,
                                        node,
                                    );
                                    if ret < 0 {
                                        ret = 2;
                                    } else {
                                        ret = 0;
                                    }
                                    if ret == 0 {
                                        if let Some(mut attr) =
                                            node.and_then(|node| XmlAttrPtr::try_from(node).ok())
                                        {
                                            attr.atype = Some(XmlAttributeType::XmlAttributeIDREFS);
                                        }
                                    }
                                    break 'done;
                                }
                                XmlSchemaValType::XmlSchemasEntity => {
                                    let strip: *mut XmlChar;

                                    ret = validate_ncname::<true>(
                                        CStr::from_ptr(value as *const i8)
                                            .to_string_lossy()
                                            .as_ref(),
                                    )
                                    .is_err() as i32;
                                    if let Some(node) =
                                        node.filter(|node| node.document().is_some())
                                    {
                                        if ret == 0 {
                                            strip = xml_schema_strip(value);
                                            let ent = if !strip.is_null() {
                                                let e = xml_get_doc_entity(
                                                    node.document(),
                                                    &CStr::from_ptr(strip as *const i8)
                                                        .to_string_lossy(),
                                                );
                                                xml_free(strip as _);
                                                e
                                            } else {
                                                xml_get_doc_entity(
                                                    node.document(),
                                                    &CStr::from_ptr(value as *const i8)
                                                        .to_string_lossy(),
                                                )
                                            };
                                            if ent.is_none_or(|ent| {
                                                !matches!(
                                                    ent.etype,
                                                    XmlEntityType::XmlExternalGeneralUnparsedEntity
                                                )
                                            }) {
                                                ret = 4;
                                            }
                                        }
                                        if ret == 0 && !val.is_null() {
                                            // TODO;
                                            todo!()
                                        }
                                        if ret == 0 {
                                            if let Ok(mut attr) = XmlAttrPtr::try_from(node) {
                                                attr.atype =
                                                    Some(XmlAttributeType::XmlAttributeEntity);
                                            }
                                        }
                                    } else {
                                        ret = 3;
                                    }
                                    break 'done;
                                }
                                XmlSchemaValType::XmlSchemasEntities => {
                                    let Some(node) = node.filter(|node| node.document().is_some())
                                    else {
                                        break 'return3;
                                    };
                                    ret = xml_schema_val_atomic_list_node(
                                        XML_SCHEMA_TYPE_ENTITY_DEF.get(),
                                        value,
                                        val,
                                        Some(node),
                                    );
                                    if ret <= 0 {
                                        ret = 1;
                                    } else {
                                        ret = 0;
                                    }
                                    if ret == 0 {
                                        if let Ok(mut attr) = XmlAttrPtr::try_from(node) {
                                            attr.atype =
                                                Some(XmlAttributeType::XmlAttributeEntities);
                                        }
                                    }
                                    break 'done;
                                }
                                XmlSchemaValType::XmlSchemasNotation => {
                                    let mut uri: *mut XmlChar = null_mut();
                                    let mut local: *mut XmlChar = null_mut();

                                    ret = xml_validate_qname(value, 1);
                                    if let Some(node) = node.filter(|_| ret == 0) {
                                        let mut prefix: *mut XmlChar = null_mut();

                                        local = xml_split_qname2(value, addr_of_mut!(prefix));
                                        if !prefix.is_null() {
                                            if let Some(ns) = node.search_ns(
                                                node.document(),
                                                Some(
                                                    CStr::from_ptr(prefix as *const i8)
                                                        .to_string_lossy()
                                                        .as_ref(),
                                                ),
                                            ) {
                                                if !val.is_null() {
                                                    uri = xml_strdup(ns.href);
                                                }
                                            } else {
                                                ret = 1;
                                            }
                                        }
                                        if !local.is_null() && (val.is_null() || ret != 0) {
                                            xml_free(local as _);
                                        }
                                        if !prefix.is_null() {
                                            xml_free(prefix as _);
                                        }
                                    }
                                    if let Some(doc) = node.and_then(|node| node.document()) {
                                        if ret == 0 {
                                            ret = xml_validate_notation_use(
                                                null_mut(),
                                                doc,
                                                CStr::from_ptr(value as *const i8)
                                                    .to_string_lossy()
                                                    .as_ref(),
                                            );
                                            if ret == 1 {
                                                ret = 0;
                                            } else {
                                                ret = 1;
                                            }
                                        }
                                        if ret == 0 && !val.is_null() {
                                            v = xml_schema_new_value(
                                                XmlSchemaValType::XmlSchemasNotation,
                                            );
                                            if !v.is_null() {
                                                if !local.is_null() {
                                                    (*v).value.qname.name = local;
                                                } else {
                                                    (*v).value.qname.name = xml_strdup(value);
                                                }
                                                if !uri.is_null() {
                                                    (*v).value.qname.uri = uri;
                                                }

                                                *val = v;
                                            } else {
                                                if !local.is_null() {
                                                    xml_free(local as _);
                                                }
                                                if !uri.is_null() {
                                                    xml_free(uri as _);
                                                }
                                                break 'error;
                                            }
                                        }
                                    } else {
                                        ret = 3;
                                    }
                                    break 'done;
                                }
                                XmlSchemaValType::XmlSchemasAnyURI => {
                                    if *value != 0 {
                                        let mut cur: *mut XmlChar;
                                        if norm.is_null() && norm_on_the_fly != 0 {
                                            if let Some(res) = xml_schema_collapse_string(
                                                CStr::from_ptr(value as *const i8)
                                                    .to_string_lossy()
                                                    .as_ref(),
                                            ) {
                                                norm = xml_strndup(res.as_ptr(), res.len() as i32);
                                            }
                                            if !norm.is_null() {
                                                value = norm;
                                            }
                                        }
                                        let tmpval: *mut XmlChar = xml_strdup(value);
                                        if tmpval.is_null() {
                                            break 'error;
                                        }
                                        cur = tmpval;
                                        while *cur != 0 {
                                            if *cur < 32
                                                || *cur >= 127
                                                || *cur == b' '
                                                || *cur == b'<'
                                                || *cur == b'>'
                                                || *cur == b'"'
                                                || *cur == b'{'
                                                || *cur == b'}'
                                                || *cur == b'|'
                                                || *cur == b'\\'
                                                || *cur == b'^'
                                                || *cur == b'`'
                                                || *cur == b'\''
                                            {
                                                *cur = b'_';
                                            }
                                            cur = cur.add(1);
                                        }
                                        let uri: XmlURIPtr = xml_parse_uri(tmpval as _);
                                        xml_free(tmpval as _);
                                        if uri.is_null() {
                                            break 'return1;
                                        }
                                        xml_free_uri(uri as _);
                                    }

                                    if !val.is_null() {
                                        v = xml_schema_new_value(
                                            XmlSchemaValType::XmlSchemasAnyURI,
                                        );
                                        if v.is_null() {
                                            break 'error;
                                        }
                                        (*v).value.str = xml_strdup(value);
                                        *val = v;
                                    }
                                    break 'return0;
                                }
                                XmlSchemaValType::XmlSchemasHexbinary => {
                                    let mut cur: *const XmlChar = value;
                                    let mut base: *mut XmlChar;
                                    let total: i32;
                                    let mut i: i32 = 0;

                                    if cur.is_null() {
                                        break 'return1;
                                    }

                                    if norm_on_the_fly != 0 {
                                        while IS_WSP_BLANK_CH!(*cur) {
                                            cur = cur.add(1);
                                        }
                                    }

                                    let start: *const XmlChar = cur;
                                    while (*cur >= b'0' && *cur <= b'9')
                                        || (*cur >= b'A' && *cur <= b'F')
                                        || (*cur >= b'a' && *cur <= b'f')
                                    {
                                        i += 1;
                                        cur = cur.add(1);
                                    }
                                    if norm_on_the_fly != 0 {
                                        while IS_WSP_BLANK_CH!(*cur) {
                                            cur = cur.add(1);
                                        }
                                    }

                                    if *cur != 0 {
                                        break 'return1;
                                    }
                                    if i % 2 != 0 {
                                        break 'return1;
                                    }

                                    if !val.is_null() {
                                        v = xml_schema_new_value(
                                            XmlSchemaValType::XmlSchemasHexbinary,
                                        );
                                        if v.is_null() {
                                            break 'error;
                                        }
                                        // Copy only the normalized piece.
                                        // CRITICAL TODO: Check this.
                                        cur = xml_strndup(start, i);
                                        if cur.is_null() {
                                            xml_schema_type_err_memory(
                                                node,
                                                Some("allocating hexbin data"),
                                            );
                                            xml_free(v as _);
                                            break 'return1;
                                        }

                                        total = i / 2; /* number of octets */

                                        base = cur as _;
                                        while i > 0 {
                                            i -= 1;
                                            if *base >= b'a' {
                                                *base -= b'a' - b'A';
                                            }
                                            base = base.add(1);
                                        }

                                        (*v).value.hex.str = cur as _;
                                        (*v).value.hex.total = total as _;
                                        *val = v;
                                    }
                                    break 'return0;
                                }
                                XmlSchemaValType::XmlSchemasBase64binary => {
                                    // ISSUE:
                                    //
                                    // Ignore all stray characters? (yes, currently)
                                    // Worry about long lines? (no, currently)
                                    //
                                    // rfc2045.txt:
                                    //
                                    // "The encoded output stream must be represented in lines of
                                    // no more than 76 characters each.  All line breaks or other
                                    // characters not found in Table 1 must be ignored by decoding
                                    // software.  In base64 data, characters other than those in
                                    // Table 1, line breaks, and other white space probably
                                    // indicate a transmission error, about which a warning
                                    // message or even a message rejection might be appropriate
                                    // under some circumstances."
                                    let mut cur: *const XmlChar = value;
                                    let mut base: *mut XmlChar;
                                    let mut total: i32;
                                    let mut i: i32 = 0;
                                    let mut pad: i32 = 0;

                                    if cur.is_null() {
                                        break 'return1;
                                    }

                                    while *cur != 0 {
                                        let decc: i32 = _xml_schema_base64_decode(*cur);
                                        if decc < 0 {
                                        } else if decc < 64 {
                                            i += 1;
                                        } else {
                                            break;
                                        }
                                        cur = cur.add(1);
                                    }
                                    while *cur != 0 {
                                        let decc: i32 = _xml_schema_base64_decode(*cur);
                                        if decc < 0 {
                                        } else if decc < 64 {
                                            break 'return1;
                                        }
                                        if decc == 64 {
                                            pad += 1;
                                        }
                                        cur = cur.add(1);
                                    }

                                    // rfc2045.txt: "Special processing is performed if fewer than
                                    // 24 bits are available at the end of the data being encoded.
                                    // A full encoding quantum is always completed at the end of a
                                    // body.  When fewer than 24 input bits are available in an
                                    // input group, zero bits are added (on the right) to form an
                                    // integral number of 6-bit groups.  Padding at the end of the
                                    // data is performed using the "=" character.  Since all
                                    // base64 input is an integral number of octets, only the
                                    // following cases can arise: (1) the final quantum of
                                    // encoding input is an integral multiple of 24 bits; here,
                                    // the final unit of encoded output will be an integral
                                    // multiple of indent: Standard input:701: Warning:old style
                                    // assignment ambiguity in "=*".  Assuming "= *" 4 characters
                                    // with no "=" padding, (2) the final
                                    // quantum of encoding input is exactly 8 bits; here, the
                                    // final unit of encoded output will be two characters
                                    // followed by two "=" padding characters, or (3) the final
                                    // quantum of encoding input is exactly 16 bits; here, the
                                    // final unit of encoded output will be three characters
                                    // followed by one "=" padding character."

                                    total = 3 * (i / 4);
                                    if pad == 0 {
                                        if i % 4 != 0 {
                                            break 'return1;
                                        }
                                    } else if pad == 1 {
                                        let mut decc: i32;

                                        if i % 4 != 3 {
                                            break 'return1;
                                        }
                                        decc = _xml_schema_base64_decode(*cur);
                                        while !(0..=63).contains(&decc) {
                                            cur = cur.sub(1);
                                            decc = _xml_schema_base64_decode(*cur)
                                        }
                                        // 16bits in 24bits means 2 pad bits: nnnnnn nnmmmm mmmm00
                                        // 00111100 -> 0x3c
                                        if decc & !0x3c != 0 {
                                            break 'return1;
                                        }
                                        total += 2;
                                    } else if pad == 2 {
                                        let mut decc: i32;

                                        if i % 4 != 2 {
                                            break 'return1;
                                        }
                                        decc = _xml_schema_base64_decode(*cur);
                                        while !(0..=63).contains(&decc) {
                                            cur = cur.sub(1);
                                            decc = _xml_schema_base64_decode(*cur)
                                        }
                                        // 8bits in 12bits means 4 pad bits: nnnnnn nn0000
                                        // 00110000 -> 0x30
                                        if decc & !0x30 != 0 {
                                            break 'return1;
                                        }
                                        total += 1;
                                    } else {
                                        break 'return1;
                                    }

                                    if !val.is_null() {
                                        v = xml_schema_new_value(
                                            XmlSchemaValType::XmlSchemasBase64binary,
                                        );
                                        if v.is_null() {
                                            break 'error;
                                        }
                                        base =
                                            xml_malloc_atomic(i as usize + pad as usize + 1) as _;
                                        if base.is_null() {
                                            xml_schema_type_err_memory(
                                                node,
                                                Some("allocating base64 data"),
                                            );
                                            xml_free(v as _);
                                            break 'return1;
                                        }
                                        (*v).value.base64.str = base;
                                        cur = value;
                                        while *cur != 0 {
                                            if _xml_schema_base64_decode(*cur) >= 0 {
                                                *base = *cur;
                                                base = base.add(1);
                                            }
                                            cur = cur.add(1);
                                        }
                                        *base = 0;
                                        (*v).value.base64.total = total as _;
                                        *val = v;
                                    }
                                    break 'return0;
                                }
                                XmlSchemaValType::XmlSchemasInteger
                                | XmlSchemaValType::XmlSchemasPInteger
                                | XmlSchemaValType::XmlSchemasNPInteger
                                | XmlSchemaValType::XmlSchemasNInteger
                                | XmlSchemaValType::XmlSchemasNNInteger => {
                                    let mut cur: *const XmlChar = value;
                                    let mut lo: u64 = 0;
                                    let mut mi: u64 = 0;
                                    let mut hi: u64 = 0;
                                    let mut sign: i32 = 0;

                                    if cur.is_null() {
                                        break 'return1;
                                    }
                                    if norm_on_the_fly != 0 {
                                        while IS_WSP_BLANK_CH!(*cur) {
                                            cur = cur.add(1);
                                        }
                                    }
                                    if *cur == b'-' {
                                        sign = 1;
                                        cur = cur.add(1);
                                    } else if *cur == b'+' {
                                        cur = cur.add(1);
                                    }
                                    ret = xml_schema_parse_uint(
                                        addr_of_mut!(cur),
                                        addr_of_mut!(lo),
                                        addr_of_mut!(mi),
                                        addr_of_mut!(hi),
                                    );
                                    if ret < 0 {
                                        break 'return1;
                                    }
                                    if norm_on_the_fly != 0 {
                                        while IS_WSP_BLANK_CH!(*cur) {
                                            cur = cur.add(1);
                                        }
                                    }
                                    if *cur != 0 {
                                        break 'return1;
                                    }
                                    if (*typ).built_in_type
                                        == XmlSchemaValType::XmlSchemasNPInteger as i32
                                    {
                                        if sign == 0 && (hi != 0 || mi != 0 || lo != 0) {
                                            break 'return1;
                                        }
                                    } else if (*typ).built_in_type
                                        == XmlSchemaValType::XmlSchemasPInteger as i32
                                    {
                                        if sign == 1 {
                                            break 'return1;
                                        }
                                        if hi == 0 && mi == 0 && lo == 0 {
                                            break 'return1;
                                        }
                                    } else if (*typ).built_in_type
                                        == XmlSchemaValType::XmlSchemasNInteger as i32
                                    {
                                        if sign == 0 {
                                            break 'return1;
                                        }
                                        if hi == 0 && mi == 0 && lo == 0 {
                                            break 'return1;
                                        }
                                    } else if (*typ).built_in_type
                                        == XmlSchemaValType::XmlSchemasNNInteger as i32
                                        && (sign == 1 && (hi != 0 || mi != 0 || lo != 0))
                                    {
                                        break 'return1;
                                    }
                                    if !val.is_null() {
                                        v = xml_schema_new_value(
                                            (*typ).built_in_type.try_into().unwrap(),
                                        );
                                        if !v.is_null() {
                                            if ret == 0 {
                                                ret += 1;
                                            }
                                            (*v).value.decimal.lo = lo;
                                            (*v).value.decimal.mi = mi;
                                            (*v).value.decimal.hi = hi;
                                            (*v).value.decimal.sign = sign as _;
                                            (*v).value.decimal.frac = 0;
                                            (*v).value.decimal.total = ret as _;
                                            *val = v;
                                        }
                                    }
                                    break 'return0;
                                }
                                XmlSchemaValType::XmlSchemasLong
                                | XmlSchemaValType::XmlSchemasByte
                                | XmlSchemaValType::XmlSchemasShort
                                | XmlSchemaValType::XmlSchemasInt => {
                                    let mut cur: *const XmlChar = value;
                                    let mut lo: u64 = 0;
                                    let mut mi: u64 = 0;
                                    let mut hi: u64 = 0;
                                    let mut sign: i32 = 0;

                                    if cur.is_null() {
                                        break 'return1;
                                    }
                                    if norm_on_the_fly != 0 {
                                        while IS_WSP_BLANK_CH!(*cur) {
                                            cur = cur.add(1);
                                        }
                                    }
                                    if *cur == b'-' {
                                        sign = 1;
                                        cur = cur.add(1);
                                    } else if *cur == b'+' {
                                        cur = cur.add(1);
                                    }
                                    ret = xml_schema_parse_uint(
                                        addr_of_mut!(cur),
                                        addr_of_mut!(lo),
                                        addr_of_mut!(mi),
                                        addr_of_mut!(hi),
                                    );
                                    if ret < 0 {
                                        break 'return1;
                                    }
                                    if norm_on_the_fly != 0 {
                                        while IS_WSP_BLANK_CH!(*cur) {
                                            cur = cur.add(1);
                                        }
                                    }
                                    if *cur != 0 {
                                        break 'return1;
                                    }
                                    if (*typ).built_in_type
                                        == XmlSchemaValType::XmlSchemasLong as i32
                                    {
                                        if hi >= 922 {
                                            if hi > 922 {
                                                break 'return1;
                                            }
                                            if mi >= 33720368 {
                                                if mi > 33720368 {
                                                    break 'return1;
                                                }
                                                if sign == 0 && lo > 54775807 {
                                                    break 'return1;
                                                }
                                                if sign == 1 && lo > 54775808 {
                                                    break 'return1;
                                                }
                                            }
                                        }
                                    } else if (*typ).built_in_type
                                        == XmlSchemaValType::XmlSchemasInt as i32
                                    {
                                        if hi != 0 {
                                            break 'return1;
                                        }
                                        if mi >= 21 {
                                            if mi > 21 {
                                                break 'return1;
                                            }
                                            if sign == 0 && lo > 47483647 {
                                                break 'return1;
                                            }
                                            if sign == 1 && lo > 47483648 {
                                                break 'return1;
                                            }
                                        }
                                    } else if (*typ).built_in_type
                                        == XmlSchemaValType::XmlSchemasShort as i32
                                    {
                                        if mi != 0 || hi != 0 {
                                            break 'return1;
                                        }
                                        if sign == 1 && lo > 32768 {
                                            break 'return1;
                                        }
                                        if sign == 0 && lo > 32767 {
                                            break 'return1;
                                        }
                                    } else if (*typ).built_in_type
                                        == XmlSchemaValType::XmlSchemasByte as i32
                                    {
                                        if mi != 0 || hi != 0 {
                                            break 'return1;
                                        }
                                        if sign == 1 && lo > 128 {
                                            break 'return1;
                                        }
                                        if sign == 0 && lo > 127 {
                                            break 'return1;
                                        }
                                    }
                                    if !val.is_null() {
                                        v = xml_schema_new_value(
                                            (*typ).built_in_type.try_into().unwrap(),
                                        );
                                        if !v.is_null() {
                                            (*v).value.decimal.lo = lo;
                                            (*v).value.decimal.mi = mi;
                                            (*v).value.decimal.hi = hi;
                                            (*v).value.decimal.sign = sign as _;
                                            (*v).value.decimal.frac = 0;
                                            (*v).value.decimal.total = ret as _;
                                            *val = v;
                                        }
                                    }
                                    break 'return0;
                                }
                                XmlSchemaValType::XmlSchemasUInt
                                | XmlSchemaValType::XmlSchemasULong
                                | XmlSchemaValType::XmlSchemasUShort
                                | XmlSchemaValType::XmlSchemasUByte => {
                                    let mut cur: *const XmlChar = value;
                                    let mut lo: u64 = 0;
                                    let mut mi: u64 = 0;
                                    let mut hi: u64 = 0;

                                    if cur.is_null() {
                                        break 'return1;
                                    }
                                    if norm_on_the_fly != 0 {
                                        while IS_WSP_BLANK_CH!(*cur) {
                                            cur = cur.add(1);
                                        }
                                    }
                                    ret = xml_schema_parse_uint(
                                        addr_of_mut!(cur),
                                        addr_of_mut!(lo),
                                        addr_of_mut!(mi),
                                        addr_of_mut!(hi),
                                    );
                                    if ret < 0 {
                                        break 'return1;
                                    }
                                    if norm_on_the_fly != 0 {
                                        while IS_WSP_BLANK_CH!(*cur) {
                                            cur = cur.add(1);
                                        }
                                    }
                                    if *cur != 0 {
                                        break 'return1;
                                    }
                                    if (*typ).built_in_type
                                        == XmlSchemaValType::XmlSchemasULong as i32
                                    {
                                        if hi >= 1844 {
                                            if hi > 1844 {
                                                break 'return1;
                                            }
                                            if mi >= 67440737 {
                                                if mi > 67440737 {
                                                    break 'return1;
                                                }
                                                if lo > 9551615 {
                                                    break 'return1;
                                                }
                                            }
                                        }
                                    } else if (*typ).built_in_type
                                        == XmlSchemaValType::XmlSchemasUInt as i32
                                    {
                                        if hi != 0 {
                                            break 'return1;
                                        }
                                        if mi >= 42 {
                                            if mi > 42 {
                                                break 'return1;
                                            }
                                            if lo > 94967295 {
                                                break 'return1;
                                            }
                                        }
                                    } else if (*typ).built_in_type
                                        == XmlSchemaValType::XmlSchemasUShort as i32
                                    {
                                        if mi != 0 || hi != 0 {
                                            break 'return1;
                                        }
                                        if lo > 65535 {
                                            break 'return1;
                                        }
                                    } else if (*typ).built_in_type
                                        == XmlSchemaValType::XmlSchemasUByte as i32
                                    {
                                        if mi != 0 || hi != 0 {
                                            break 'return1;
                                        }
                                        if lo > 255 {
                                            break 'return1;
                                        }
                                    }
                                    if !val.is_null() {
                                        v = xml_schema_new_value(
                                            (*typ).built_in_type.try_into().unwrap(),
                                        );
                                        if !v.is_null() {
                                            (*v).value.decimal.lo = lo;
                                            (*v).value.decimal.mi = mi;
                                            (*v).value.decimal.hi = hi;
                                            (*v).value.decimal.sign = 0;
                                            (*v).value.decimal.frac = 0;
                                            (*v).value.decimal.total = ret as _;
                                            *val = v;
                                        }
                                    }
                                    break 'return0;
                                }
                            }
                        }
                        //   done:
                        if !norm.is_null() {
                            xml_free(norm as _);
                        }
                        return ret;
                    }
                    //   return3:
                    if !norm.is_null() {
                        xml_free(norm as _);
                    }
                    return 3;
                }
                //   return1:
                if !norm.is_null() {
                    xml_free(norm as _);
                }
                return 1;
            }
            //   return0:
            if !norm.is_null() {
                xml_free(norm as _);
            }
            return 0;
        }
        //   error:
        if !norm.is_null() {
            xml_free(norm as _);
        }
        -1
    }
}

/// Check that a value conforms to the lexical space of the predefined type.
/// if true a value is computed and returned in @val.
///
/// Returns 0 if this validates, a positive error code number otherwise
/// and -1 in case of internal or API error.
#[doc(alias = "xmlSchemaValPredefTypeNode")]
pub unsafe fn xml_schema_val_predef_type_node(
    typ: XmlSchemaTypePtr,
    value: *const XmlChar,
    val: *mut XmlSchemaValPtr,
    node: Option<XmlGenericNodePtr>,
) -> i32 {
    unsafe {
        xml_schema_val_atomic_type(
            typ,
            value,
            val,
            node,
            0,
            XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceUnknown,
            1,
            1,
            0,
        )
    }
}

/// Compare 2 decimals
///
/// Returns -1 if x < y, 0 if x == y, 1 if x > y and -2 in case of error
#[doc(alias = "xmlSchemaCompareDecimals")]
unsafe fn xml_schema_compare_decimals(x: XmlSchemaValPtr, y: XmlSchemaValPtr) -> i32 {
    unsafe {
        let swp: XmlSchemaValPtr;
        let mut order: i32 = 1;
        let mut dlen: i32;
        let mut hi: u64;
        let mut mi: u64;
        let mut lo: u64;

        // First test: If x is -ve and not zero
        if (*x).value.decimal.sign != 0
            && ((*x).value.decimal.lo != 0
                || (*x).value.decimal.mi != 0
                || (*x).value.decimal.hi != 0)
        {
            // Then if y is -ve and not zero reverse the compare
            if (*y).value.decimal.sign != 0
                && ((*y).value.decimal.lo != 0
                    || (*y).value.decimal.mi != 0
                    || (*y).value.decimal.hi != 0)
            {
                order = -1;
            }
            // Otherwise (y >= 0) we have the answer
            else {
                return -1;
            }
        // If x is not -ve and y is -ve we have the answer
        } else if (*y).value.decimal.sign != 0
            && ((*y).value.decimal.lo != 0
                || (*y).value.decimal.mi != 0
                || (*y).value.decimal.hi != 0)
        {
            return 1;
        }
        // If it's not simply determined by a difference in sign,
        // then we need to compare the actual values of the two nums.
        // To do this, we start by looking at the integral parts.
        // If the number of integral digits differ, then we have our
        // answer.
        let integx: i32 = ((*x).value.decimal.total - (*x).value.decimal.frac) as _;
        let integy: i32 = ((*y).value.decimal.total - (*y).value.decimal.frac) as _;
        // NOTE: We changed the "total" for values like "0.1"
        //   (or "-0.1" or ".1") to be 1, which was 2 previously.
        //   Therefore the special case, when such values are
        //   compared with 0, needs to be handled separately;
        //   otherwise a zero would be recognized incorrectly as
        //   greater than those values. This has the nice side effect
        //   that we gain an overall optimized comparison with zeroes.
        // Note that a "0" has a "total" of 1 already.
        if integx == 1 && (*x).value.decimal.lo == 0 {
            if integy != 1 || (*y).value.decimal.lo != 0 {
                return -order;
            } else {
                return 0;
            }
        }
        if integy == 1 && (*y).value.decimal.lo == 0 {
            if integx != 1 || (*x).value.decimal.lo != 0 {
                return order;
            } else {
                return 0;
            }
        }

        match integx.cmp(&integy) {
            std::cmp::Ordering::Greater => return order,
            std::cmp::Ordering::Less => return -order,
            std::cmp::Ordering::Equal => {}
        }

        // If the number of integral digits is the same for both numbers,
        // then things get a little more complicated.  We need to "normalize"
        // the numbers in order to properly compare them.  To do this, we
        // look at the total length of each number (length => number of
        // significant digits), and divide the "shorter" by 10 (decreasing
        // the length) until they are of equal length.
        dlen = ((*x).value.decimal.total - (*y).value.decimal.total) as _;
        if dlen < 0 {
            // y has more digits than x
            swp = x;
            hi = (*y).value.decimal.hi;
            mi = (*y).value.decimal.mi;
            lo = (*y).value.decimal.lo;
            dlen = -dlen;
            order = -order;
        } else {
            // x has more digits than y
            swp = y;
            hi = (*x).value.decimal.hi;
            mi = (*x).value.decimal.mi;
            lo = (*x).value.decimal.lo;
        }
        while dlen > 8 {
            // in effect, right shift by 10**8
            lo = mi;
            mi = hi;
            hi = 0;
            dlen -= 8;
        }
        while dlen > 0 {
            let rem1: u64 = (hi % 10) * 100000000u64;
            hi /= 10;
            let rem2: u64 = (mi % 10) * 100000000u64;
            mi = (mi + rem1) / 10;
            lo = (lo + rem2) / 10;
            dlen -= 1;
        }

        match hi.cmp(&(*swp).value.decimal.hi) {
            std::cmp::Ordering::Greater => return order,
            std::cmp::Ordering::Equal => match mi.cmp(&(*swp).value.decimal.mi) {
                std::cmp::Ordering::Greater => return order,
                std::cmp::Ordering::Equal => match lo.cmp(&(*swp).value.decimal.lo) {
                    std::cmp::Ordering::Greater => return order,
                    std::cmp::Ordering::Equal => {
                        if (*x).value.decimal.total == (*y).value.decimal.total {
                            return 0;
                        } else {
                            return order;
                        }
                    }
                    std::cmp::Ordering::Less => {}
                },
                std::cmp::Ordering::Less => {}
            },
            std::cmp::Ordering::Less => {}
        }
        -order
    }
}

/// Compare 2 durations
///
/// Returns -1 if x < y, 0 if x == y, 1 if x > y, 2 if x <> y, and -2 in case of error
#[doc(alias = "xmlSchemaCompareDurations")]
unsafe fn xml_schema_compare_durations(x: XmlSchemaValPtr, y: XmlSchemaValPtr) -> i32 {
    unsafe {
        let mut sec: f64;
        let mut invert: i32 = 1;
        let mut xmon: i64;
        let xday: i64;

        let mut minday: i64;
        let mut maxday: i64;
        const DAY_RANGE: [[i64; 12]; 2] = [
            [0, 28, 59, 89, 120, 150, 181, 212, 242, 273, 303, 334],
            [0, 31, 62, 92, 123, 153, 184, 215, 245, 276, 306, 337],
        ];

        if x.is_null() || y.is_null() {
            return -2;
        }

        // months
        let mon: i64 = (*x).value.dur.mon - (*y).value.dur.mon;

        // seconds
        sec = (*x).value.dur.sec - (*y).value.dur.sec;
        let carry: i64 = (sec / SECS_PER_DAY as f64) as i64;
        sec -= carry as f64 * SECS_PER_DAY as f64;

        // days
        let day: i64 = (*x).value.dur.day - (*y).value.dur.day + carry;

        // easy test
        if mon == 0 {
            match day.cmp(&0) {
                std::cmp::Ordering::Equal => {
                    if sec == 0.0 {
                        return 0;
                    } else if sec < 0.0 {
                        return -1;
                    } else {
                        return 1;
                    }
                }
                std::cmp::Ordering::Less => {
                    return -1;
                }
                std::cmp::Ordering::Greater => {
                    return 1;
                }
            }
        }

        if mon > 0 {
            if day >= 0 && sec >= 0.0 {
                return 1;
            } else {
                xmon = mon;
                xday = -day;
            }
        } else if day <= 0 && sec <= 0.0 {
            return -1;
        } else {
            invert = -1;
            xmon = -mon;
            xday = day;
        }

        let myear: i64 = xmon / 12;
        if myear == 0 {
            minday = 0;
            maxday = 0;
        } else {
            if myear > i64::MAX / 366 {
                return -2;
            }
            // FIXME: This doesn't take leap year exceptions every 100/400 years
            // into account.
            maxday = 365 * myear + (myear + 3) / 4;
            // FIXME: Needs to be calculated separately
            minday = maxday - 1;
        }

        xmon %= 12;
        minday += DAY_RANGE[0][xmon as usize];
        maxday += DAY_RANGE[1][xmon as usize];

        if maxday == minday && maxday == xday {
            // can this really happen ?
            return 0;
        }
        if maxday < xday {
            return -invert;
        }
        if minday > xday {
            return invert;
        }

        // indeterminate
        2
    }
}

/// Makes a copy of @v. The calling program is responsible for freeing the returned value.
///
/// returns a pointer to a duplicated #xmlSchemaValPtr or NULL if error.
#[doc(alias = "xmlSchemaDupVal")]
unsafe fn xml_schema_dup_val(v: XmlSchemaValPtr) -> XmlSchemaValPtr {
    unsafe {
        let ret: XmlSchemaValPtr = xml_schema_new_value((*v).typ);
        if ret.is_null() {
            return null_mut();
        }

        memcpy(ret as _, v as _, size_of::<XmlSchemaVal>());
        (*ret).next = null_mut();
        ret
    }
}

// macros for adding date/times and durations
macro_rules! FQUOTIENT {
    ($a:expr, $b:expr) => {
        ($a as f64 / $b as f64).floor()
    };
}
macro_rules! MODULO {
    ($a:expr, $b:expr) => {
        $a as f64 - FQUOTIENT!($a, $b) * $b as f64
    };
}
macro_rules! FQUOTIENT_RANGE {
    ($a:expr, $low:expr, $high:expr) => {
        FQUOTIENT!($a - $low, $high - $low)
    };
}
macro_rules! MODULO_RANGE {
    ($a:expr, $low:expr, $high:expr) => {
        MODULO!($a - $low, $high - $low) + $low as f64
    };
}

/// Compute a new date/time from @dt and @dur. This function assumes @dt
/// is either #XmlSchemaValType::XML_SCHEMAS_DATETIME, #XmlSchemaValType::XML_SCHEMAS_DATE, #XmlSchemaValType::XML_SCHEMAS_GYEARMONTH,
/// or #XmlSchemaValType::XML_SCHEMAS_GYEAR. The returned #xmlSchemaVal is the same type as
/// @dt. The calling program is responsible for freeing the returned value.
///
/// Returns a pointer to a new #xmlSchemaVal or NULL if error.
#[doc(alias = "_xmlSchemaDateAdd")]
unsafe fn _xml_schema_date_add(dt: XmlSchemaValPtr, dur: XmlSchemaValPtr) -> XmlSchemaValPtr {
    unsafe {
        let mut carry: i64;
        let mut tempdays: i64;
        let mut temp: i64;

        if dt.is_null() || dur.is_null() {
            return null_mut();
        }

        let ret: XmlSchemaValPtr = xml_schema_new_value((*dt).typ);
        if ret.is_null() {
            return null_mut();
        }

        // make a copy so we don't alter the original value
        let tmp: XmlSchemaValPtr = xml_schema_dup_val(dt);
        if tmp.is_null() {
            xml_schema_free_value(ret);
            return null_mut();
        }

        let r: XmlSchemaValDatePtr = addr_of_mut!((*ret).value.date);
        let d: XmlSchemaValDatePtr = addr_of_mut!((*tmp).value.date);
        let u: XmlSchemaValDurationPtr = addr_of_mut!((*dur).value.dur);

        // normalization
        if (*d).mon == 0 {
            (*d).mon = 1;
        }

        // normalize for time zone offset
        (*u).sec -= (*d).tzo as f64 * 60.;
        (*d).tzo = 0;

        // normalization
        if (*d).day == 0 {
            (*d).day = 1;
        }

        // month
        carry = (*d).mon as i64 + (*u).mon;
        (*r).mon = MODULO_RANGE!(carry, 1, 13) as u32;
        carry = FQUOTIENT_RANGE!(carry, 1, 13) as i64;

        // year (may be modified later)
        (*r).year = (*d).year + carry;
        if (*r).year == 0 {
            if (*d).year > 0 {
                (*r).year -= 1;
            } else {
                (*r).year += 1;
            }
        }

        // time zone
        (*r).tzo = (*d).tzo;
        (*r).tz_flag = (*d).tz_flag;

        // seconds
        (*r).sec = (*d).sec + (*u).sec;
        carry = FQUOTIENT!((*r).sec as i64, 60) as i64;
        if (*r).sec != 0.0 {
            (*r).sec = MODULO!((*r).sec, 60.0);
        }

        // minute
        carry += (*d).min as i64;
        (*r).min = MODULO!(carry, 60) as u32;
        carry = FQUOTIENT!(carry, 60) as i64;

        // hours
        carry += (*d).hour as i64;
        (*r).hour = MODULO!(carry, 24) as u32;
        carry = FQUOTIENT!(carry, 24) as i64;

        // days
        // Note we use tempdays because the temporary values may need more than 5 bits
        if VALID_YEAR!((*r).year)
            && VALID_MONTH!((*r).mon)
            && (*d).day > MAX_DAYINMONTH!((*r).year, (*r).mon)
        {
            tempdays = MAX_DAYINMONTH!((*r).year, (*r).mon) as i64;
        } else if (*d).day < 1 {
            tempdays = 1;
        } else {
            tempdays = (*d).day as i64;
        }

        tempdays += (*u).day + carry;

        loop {
            if tempdays < 1 {
                let mut tmon: i64 = MODULO_RANGE!((*r).mon as i32 - 1, 1, 13) as i64;
                let mut tyr: i64 = (*r).year + FQUOTIENT_RANGE!((*r).mon as i32 - 1, 1, 13) as i64;
                if tyr == 0 {
                    tyr -= 1;
                }
                // Coverity detected an overrun in DAYS_IN_MONTH
                // of size 12 at position 12 with index variable "((r)->mon - 1)"
                tmon = tmon.clamp(1, 12);
                tempdays += MAX_DAYINMONTH!(tyr, tmon) as i64;
                carry = -1;
            } else if VALID_YEAR!((*r).year)
                && VALID_MONTH!((*r).mon)
                && tempdays > MAX_DAYINMONTH!((*r).year, (*r).mon) as i64
            {
                tempdays -= MAX_DAYINMONTH!((*r).year, (*r).mon) as i64;
                carry = 1;
            } else {
                break;
            }

            temp = (*r).mon as i64 + carry;
            (*r).mon = MODULO_RANGE!(temp, 1, 13) as u32;
            (*r).year += FQUOTIENT_RANGE!(temp, 1, 13) as i64;
            if (*r).year == 0 {
                if temp < 1 {
                    (*r).year -= 1;
                } else {
                    (*r).year += 1;
                }
            }
        }

        (*r).day = tempdays as u32;

        // adjust the date/time type to the date values
        if (*ret).typ != XmlSchemaValType::XmlSchemasDatetime {
            if (*r).hour != 0 || (*r).min != 0 || (*r).sec != 0.0 {
                (*ret).typ = XmlSchemaValType::XmlSchemasDatetime;
            } else if (*ret).typ != XmlSchemaValType::XmlSchemasDate {
                if (*r).mon != 1 && (*r).day != 1 {
                    (*ret).typ = XmlSchemaValType::XmlSchemasDate;
                } else if (*ret).typ != XmlSchemaValType::XmlSchemasGYearMonth && (*r).mon != 1 {
                    (*ret).typ = XmlSchemaValType::XmlSchemasGYearMonth;
                }
            }
        }

        xml_schema_free_value(tmp);

        ret
    }
}

/// Normalize @dt to GMT time. The @offset parameter is subtracted from
/// the return value is a time-zone offset is present on @dt.
///
/// Returns a normalized copy of @dt or NULL if error.
#[doc(alias = "xmlSchemaDateNormalize")]
unsafe fn xml_schema_date_normalize(dt: XmlSchemaValPtr, offset: f64) -> XmlSchemaValPtr {
    unsafe {
        if dt.is_null() {
            return null_mut();
        }

        if !matches!(
            (*dt).typ,
            XmlSchemaValType::XmlSchemasTime
                | XmlSchemaValType::XmlSchemasDatetime
                | XmlSchemaValType::XmlSchemasDate
        ) || (*dt).value.date.tzo == 0
        {
            return xml_schema_dup_val(dt);
        }

        let dur: XmlSchemaValPtr = xml_schema_new_value(XmlSchemaValType::XmlSchemasDuration);
        if dur.is_null() {
            return null_mut();
        }

        (*dur).value.date.sec -= offset;

        let ret: XmlSchemaValPtr = _xml_schema_date_add(dt, dur);
        if ret.is_null() {
            return null_mut();
        }

        xml_schema_free_value(dur);

        // (*ret).value.date.tzo = 0;
        ret
    }
}

/// Convert mon and year of @dt to total number of days. Take the
/// number of years since (or before) 1 AD and add the number of leap
/// years. This is a function  because negative
/// years must be handled a little differently and there is no zero year.
///
/// Returns number of days.
#[doc(alias = "_xmlSchemaDateCastYMToDays")]
unsafe fn _xml_schema_date_cast_ymto_days(dt: XmlSchemaValPtr) -> i64 {
    unsafe {
        let mut mon: i32;

        mon = (*dt).value.date.mon as _;
        if mon <= 0 {
            mon = 1; /* normalization */
        }

        if (*dt).value.date.year <= 0 {
            (*dt).value.date.year * 365
                + (((*dt).value.date.year + 1) / 4 - ((*dt).value.date.year + 1) / 100
                    + ((*dt).value.date.year + 1) / 400)
                + DAY_IN_YEAR!(0, mon, (*dt).value.date.year)
        } else {
            ((*dt).value.date.year - 1) * 365
                + (((*dt).value.date.year - 1) / 4 - ((*dt).value.date.year - 1) / 100
                    + ((*dt).value.date.year - 1) / 400)
                + DAY_IN_YEAR!(0, mon, (*dt).value.date.year)
        }
    }
}

/// Calculates the number of seconds in the time portion of @dt.
///
/// Returns seconds.
macro_rules! TIME_TO_NUMBER {
    ($dt:expr) => {
        (((*$dt).value.date.hour as i32 * SECS_PER_HOUR)
            + ((*$dt).value.date.min as i32 * SECS_PER_MIN)
            + ((*$dt).value.date.tzo * SECS_PER_MIN)) as f64
            + (*$dt).value.date.sec
    };
}

/// Compare 2 date/times
///
/// Returns -1 if x < y, 0 if x == y, 1 if x > y, 2 if x <> y, and -2 in case of error
#[doc(alias = "xmlSchemaCompareDates")]
unsafe fn xml_schema_compare_dates(x: XmlSchemaValPtr, y: XmlSchemaValPtr) -> i32 {
    unsafe {
        let mut p1: XmlSchemaValPtr;
        let p2: XmlSchemaValPtr;
        let mut q1: XmlSchemaValPtr;
        let q2: XmlSchemaValPtr;
        let mut p1d: i64;
        let p2d: i64;
        let mut q1d: i64;
        let q2d: i64;

        if x.is_null() || y.is_null() {
            return -2;
        }

        if (*x).value.date.year > i64::MAX / 366
            || (*x).value.date.year < i64::MIN / 366
            || (*y).value.date.year > i64::MAX / 366
            || (*y).value.date.year < i64::MIN / 366
        {
            // Possible overflow when converting to days.
            return -2;
        }

        if (*x).value.date.tz_flag != 0 {
            if (*y).value.date.tz_flag == 0 {
                p1 = xml_schema_date_normalize(x, 0.);
                if p1.is_null() {
                    return -2;
                }
                p1d = _xml_schema_date_cast_ymto_days(p1) + (*p1).value.date.day as i64;
                // normalize y + 14:00
                q1 = xml_schema_date_normalize(y, 14. * SECS_PER_HOUR as f64);
                if q1.is_null() {
                    xml_schema_free_value(p1);
                    return -2;
                }

                q1d = _xml_schema_date_cast_ymto_days(q1) + (*q1).value.date.day as i64;
                match p1d.cmp(&q1d) {
                    std::cmp::Ordering::Less => {
                        xml_schema_free_value(p1);
                        xml_schema_free_value(q1);
                        return -1;
                    }
                    std::cmp::Ordering::Equal => {
                        let mut sec: f64;

                        sec = TIME_TO_NUMBER!(p1) - TIME_TO_NUMBER!(q1);
                        if sec < 0.0 {
                            xml_schema_free_value(p1);
                            xml_schema_free_value(q1);
                            return -1;
                        } else {
                            let mut ret: i32 = 0;
                            // normalize y - 14:00
                            q2 = xml_schema_date_normalize(y, -(14. * SECS_PER_HOUR as f64));
                            if q2.is_null() {
                                xml_schema_free_value(p1);
                                xml_schema_free_value(q1);
                                return -2;
                            }
                            q2d = _xml_schema_date_cast_ymto_days(q2) + (*q2).value.date.day as i64;
                            match p1d.cmp(&q2d) {
                                std::cmp::Ordering::Greater => {
                                    ret = 1;
                                }
                                std::cmp::Ordering::Equal => {
                                    sec = TIME_TO_NUMBER!(p1) - TIME_TO_NUMBER!(q2);
                                    if sec > 0.0 {
                                        ret = 1;
                                    } else {
                                        ret = 2; /* indeterminate */
                                    }
                                }
                                std::cmp::Ordering::Less => {}
                            }
                            xml_schema_free_value(p1);
                            xml_schema_free_value(q1);
                            xml_schema_free_value(q2);
                            if ret != 0 {
                                return ret;
                            }
                        }
                    }
                    std::cmp::Ordering::Greater => {
                        xml_schema_free_value(p1);
                        xml_schema_free_value(q1);
                    }
                }
            }
        } else if (*y).value.date.tz_flag != 0 {
            q1 = xml_schema_date_normalize(y, 0.);
            if q1.is_null() {
                return -2;
            }
            q1d = _xml_schema_date_cast_ymto_days(q1) + (*q1).value.date.day as i64;

            // normalize x - 14:00
            p1 = xml_schema_date_normalize(x, -(14. * SECS_PER_HOUR as f64));
            if p1.is_null() {
                xml_schema_free_value(q1);
                return -2;
            }
            p1d = _xml_schema_date_cast_ymto_days(p1) + (*p1).value.date.day as i64;

            match p1d.cmp(&q1d) {
                std::cmp::Ordering::Less => {
                    xml_schema_free_value(p1);
                    xml_schema_free_value(q1);
                    return -1;
                }
                std::cmp::Ordering::Equal => {
                    let mut sec: f64;

                    sec = TIME_TO_NUMBER!(p1) - TIME_TO_NUMBER!(q1);
                    if sec < 0.0 {
                        xml_schema_free_value(p1);
                        xml_schema_free_value(q1);
                        return -1;
                    } else {
                        let mut ret: i32 = 0;
                        // normalize x + 14:00
                        p2 = xml_schema_date_normalize(x, 14.0 * SECS_PER_HOUR as f64);
                        if p2.is_null() {
                            xml_schema_free_value(p1);
                            xml_schema_free_value(q1);
                            return -2;
                        }
                        p2d = _xml_schema_date_cast_ymto_days(p2) + (*p2).value.date.day as i64;

                        match p2d.cmp(&q1d) {
                            std::cmp::Ordering::Greater => {
                                ret = 1;
                            }
                            std::cmp::Ordering::Equal => {
                                sec = TIME_TO_NUMBER!(p2) - TIME_TO_NUMBER!(q1);
                                if sec > 0.0 {
                                    ret = 1;
                                } else {
                                    ret = 2; /* indeterminate */
                                }
                            }
                            std::cmp::Ordering::Less => {}
                        }
                        xml_schema_free_value(p1);
                        xml_schema_free_value(q1);
                        xml_schema_free_value(p2);
                        if ret != 0 {
                            return ret;
                        }
                    }
                }
                std::cmp::Ordering::Greater => {
                    xml_schema_free_value(p1);
                    xml_schema_free_value(q1);
                }
            }
        }

        // if the same type then calculate the difference
        if (*x).typ == (*y).typ {
            let mut ret: i32 = 0;
            q1 = xml_schema_date_normalize(y, 0.);
            if q1.is_null() {
                return -2;
            }
            q1d = _xml_schema_date_cast_ymto_days(q1) + (*q1).value.date.day as i64;

            p1 = xml_schema_date_normalize(x, 0.);
            if p1.is_null() {
                xml_schema_free_value(q1);
                return -2;
            }
            p1d = _xml_schema_date_cast_ymto_days(p1) + (*p1).value.date.day as i64;

            match p1d.cmp(&q1d) {
                std::cmp::Ordering::Less => {
                    ret = -1;
                }
                std::cmp::Ordering::Greater => {
                    ret = 1;
                }
                std::cmp::Ordering::Equal => {
                    let sec: f64 = TIME_TO_NUMBER!(p1) - TIME_TO_NUMBER!(q1);
                    if sec < 0.0 {
                        ret = -1;
                    } else if sec > 0.0 {
                        ret = 1;
                    }
                }
            }
            xml_schema_free_value(p1);
            xml_schema_free_value(q1);
            return ret;
        }

        let xmask = match (*x).typ {
            XmlSchemaValType::XmlSchemasDatetime => 0xf,
            XmlSchemaValType::XmlSchemasDate => 0x7,
            XmlSchemaValType::XmlSchemasGYear => 0x1,
            XmlSchemaValType::XmlSchemasGMonth => 0x2,
            XmlSchemaValType::XmlSchemasGDay => 0x3,
            XmlSchemaValType::XmlSchemasGYearMonth => 0x3,
            XmlSchemaValType::XmlSchemasGMonthDay => 0x6,
            XmlSchemaValType::XmlSchemasTime => 0x8,
            _ => 0,
        };

        let ymask = match (*y).typ {
            XmlSchemaValType::XmlSchemasDatetime => 0xf,
            XmlSchemaValType::XmlSchemasDate => 0x7,
            XmlSchemaValType::XmlSchemasGYear => 0x1,
            XmlSchemaValType::XmlSchemasGMonth => 0x2,
            XmlSchemaValType::XmlSchemasGDay => 0x3,
            XmlSchemaValType::XmlSchemasGYearMonth => 0x3,
            XmlSchemaValType::XmlSchemasGMonthDay => 0x6,
            XmlSchemaValType::XmlSchemasTime => 0x8,
            _ => 0,
        };

        let xor_mask: u8 = xmask ^ ymask; /* mark type differences */
        let and_mask: u8 = xmask & ymask; /* mark field specification */

        // year
        if xor_mask & 1 != 0 {
            return 2; /* indeterminate */
        } else if and_mask & 1 != 0 {
            match (*x).value.date.year.cmp(&(*y).value.date.year) {
                std::cmp::Ordering::Less => {
                    return -1;
                }
                std::cmp::Ordering::Greater => {
                    return 1;
                }
                std::cmp::Ordering::Equal => {}
            }
        }

        // month
        if xor_mask & 2 != 0 {
            return 2; /* indeterminate */
        } else if and_mask & 2 != 0 {
            match (*x).value.date.mon.cmp(&(*y).value.date.mon) {
                std::cmp::Ordering::Less => {
                    return -1;
                }
                std::cmp::Ordering::Greater => {
                    return 1;
                }
                std::cmp::Ordering::Equal => {}
            }
        }

        // day
        if xor_mask & 4 != 0 {
            return 2; /* indeterminate */
        } else if and_mask & 4 != 0 {
            match (*x).value.date.day.cmp(&(*y).value.date.day) {
                std::cmp::Ordering::Less => {
                    return -1;
                }
                std::cmp::Ordering::Greater => {
                    return 1;
                }
                std::cmp::Ordering::Equal => {}
            }
        }

        // time
        if xor_mask & 8 != 0 {
            return 2; /* indeterminate */
        } else if and_mask & 8 != 0 {
            if (*x).value.date.hour < (*y).value.date.hour {
                return -1;
            } else if (*x).value.date.hour > (*y).value.date.hour {
                return 1;
            } else if (*x).value.date.min < (*y).value.date.min {
                return -1;
            } else if (*x).value.date.min > (*y).value.date.min {
                return 1;
            } else if (*x).value.date.sec < (*y).value.date.sec {
                return -1;
            } else if (*x).value.date.sec > (*y).value.date.sec {
                return 1;
            }
        }

        0
    }
}

/// Compare 2 string for their normalized values.
/// @x is a string with whitespace of "preserve", @y is
/// a string with a whitespace of "replace". I.e. @x could
/// be an "xsd:string" and @y an "xsd:normalizedString".
///
/// Returns -1 if x < y, 0 if x == y, 1 if x > y, and -2 in
/// case of error
#[doc(alias = "xmlSchemaComparePreserveReplaceStrings")]
unsafe fn xml_schema_compare_preserve_replace_strings(
    mut x: *const XmlChar,
    mut y: *const XmlChar,
    invert: i32,
) -> i32 {
    unsafe {
        let mut tmp: i32;

        while *x != 0 && *y != 0 {
            if IS_WSP_REPLACE_CH!(*y) {
                if !IS_WSP_SPACE_CH!(*x) {
                    if *x < 0x20 {
                        if invert != 0 {
                            return 1;
                        } else {
                            return -1;
                        }
                    } else if invert != 0 {
                        return -1;
                    } else {
                        return 1;
                    }
                }
            } else {
                tmp = *x as i32 - *y as i32;
                if tmp < 0 {
                    if invert != 0 {
                        return 1;
                    } else {
                        return -1;
                    }
                }
                if tmp > 0 {
                    if invert != 0 {
                        return -1;
                    } else {
                        return 1;
                    }
                }
            }
            x = x.add(1);
            y = x.add(1);
        }
        if *x != 0 {
            if invert != 0 {
                return -1;
            } else {
                return 1;
            }
        }
        if *y != 0 {
            if invert != 0 {
                return 1;
            } else {
                return -1;
            }
        }
        0
    }
}

/// Compare 2 string for their normalized values.
/// @x is a string with whitespace of "preserve", @y is
/// a string with a whitespace of "collapse". I.e. @x could
/// be an "xsd:string" and @y an "xsd:normalizedString".
///
/// Returns -1 if x < y, 0 if x == y, 1 if x > y, and -2 in
/// case of error
#[doc(alias = "xmlSchemaComparePreserveCollapseStrings")]
unsafe fn xml_schema_compare_replace_collapse_strings(
    mut x: *const XmlChar,
    mut y: *const XmlChar,
    invert: i32,
) -> i32 {
    unsafe {
        let mut tmp: i32;

        // Skip leading blank chars of the collapsed string.
        while IS_WSP_BLANK_CH!(*y) {
            y = y.add(1);
        }

        while *x != 0 && *y != 0 {
            if IS_WSP_BLANK_CH!(*y) {
                if !IS_WSP_BLANK_CH!(*x) {
                    // The yv character would have been replaced to 0x20.
                    if *x < 0x20 {
                        if invert != 0 {
                            return 1;
                        } else {
                            return -1;
                        }
                    } else if invert != 0 {
                        return -1;
                    } else {
                        return 1;
                    }
                }
                x = x.add(1);
                y = y.add(1);
                // Skip contiguous blank chars of the collapsed string.
                while IS_WSP_BLANK_CH!(*y) {
                    y = y.add(1);
                }
            } else {
                if IS_WSP_BLANK_CH!(*x) {
                    // The xv character would have been replaced to 0x20.
                    if 0x20 < *y {
                        if invert != 0 {
                            return 1;
                        } else {
                            return -1;
                        }
                    } else if invert != 0 {
                        return -1;
                    } else {
                        return 1;
                    }
                }
                tmp = *x as i32 - *y as i32;
                x = x.add(1);
                y = y.add(1);
                if tmp < 0 {
                    return -1;
                }
                if tmp > 0 {
                    return 1;
                }
            }
        }
        if *x != 0 {
            if invert != 0 {
                return -1;
            } else {
                return 1;
            }
        }
        if *y != 0 {
            // Skip trailing blank chars of the collapsed string.
            while IS_WSP_BLANK_CH!(*y) {
                y = y.add(1);
            }
            if *y != 0 {
                if invert != 0 {
                    return 1;
                } else {
                    return -1;
                }
            }
        }
        0
    }
}

/// Compare 2 string for their normalized values.
/// @x is a string with whitespace of "preserve", @y is
/// a string with a whitespace of "collapse". I.e. @x could
/// be an "xsd:string" and @y an "xsd:normalizedString".
///
/// Returns -1 if x < y, 0 if x == y, 1 if x > y, and -2 in case of error
#[doc(alias = "xmlSchemaComparePreserveCollapseStrings")]
unsafe fn xml_schema_compare_preserve_collapse_strings(
    mut x: *const XmlChar,
    mut y: *const XmlChar,
    invert: i32,
) -> i32 {
    unsafe {
        let mut tmp: i32;

        // Skip leading blank chars of the collapsed string.
        while IS_WSP_BLANK_CH!(*y) {
            y = y.add(1);
        }

        while *x != 0 && *y != 0 {
            if IS_WSP_BLANK_CH!(*y) {
                if !IS_WSP_SPACE_CH!(*x) {
                    // The yv character would have been replaced to 0x20.
                    if *x < 0x20 {
                        if invert != 0 {
                            return 1;
                        } else {
                            return -1;
                        }
                    } else if invert != 0 {
                        return -1;
                    } else {
                        return 1;
                    }
                }
                x = x.add(1);
                y = y.add(1);
                // Skip contiguous blank chars of the collapsed string.
                while IS_WSP_BLANK_CH!(*y) {
                    y = y.add(1);
                }
            } else {
                tmp = *x as i32 - *y as i32;
                x = x.add(1);
                y = y.add(1);
                if tmp < 0 {
                    if invert != 0 {
                        return 1;
                    } else {
                        return -1;
                    }
                }
                if tmp > 0 {
                    if invert != 0 {
                        return -1;
                    } else {
                        return 1;
                    }
                }
            }
        }
        if *x != 0 {
            if invert != 0 {
                return -1;
            } else {
                return 1;
            }
        }
        if *y != 0 {
            // Skip trailing blank chars of the collapsed string.
            while IS_WSP_BLANK_CH!(*y) {
                y = y.add(1);
            }
            if *y != 0 {
                if invert != 0 {
                    return 1;
                } else {
                    return -1;
                }
            }
        }
        0
    }
}

/// Compare 2 string for their normalized values.
///
/// Returns -1 if x < y, 0 if x == y, 1 if x > y, and -2 in case of error
#[doc(alias = "xmlSchemaCompareReplacedStrings")]
unsafe fn xml_schema_compare_replaced_strings(mut x: *const XmlChar, mut y: *const XmlChar) -> i32 {
    unsafe {
        let mut tmp: i32;

        while *x != 0 && *y != 0 {
            if IS_WSP_BLANK_CH!(*y) {
                if !IS_WSP_BLANK_CH!(*x) {
                    if *x < 0x20 {
                        return -1;
                    } else {
                        return 1;
                    }
                }
            } else {
                if IS_WSP_BLANK_CH!(*x) {
                    if 0x20 < *y {
                        return -1;
                    } else {
                        return 1;
                    }
                }
                tmp = *x as i32 - *y as i32;
                if tmp < 0 {
                    return -1;
                }
                if tmp > 0 {
                    return 1;
                }
            }
            x = x.add(1);
            y = y.add(1);
        }
        if *x != 0 {
            return 1;
        }
        if *y != 0 {
            return -1;
        }
        0
    }
}

/// Compare 2 string for their normalized values.
///
/// Returns -1 if x < y, 0 if x == y, 1 if x > y, and -2 in case of error
#[doc(alias = "xmlSchemaCompareNormStrings")]
unsafe fn xml_schema_compare_norm_strings(mut x: *const XmlChar, mut y: *const XmlChar) -> i32 {
    unsafe {
        let mut tmp: i32;

        while xml_is_blank_char(*x as u32) {
            x = x.add(1);
        }
        while xml_is_blank_char(*y as u32) {
            y = y.add(1);
        }
        while *x != 0 && *y != 0 {
            if xml_is_blank_char(*x as u32) {
                if !xml_is_blank_char(*y as u32) {
                    tmp = *x as i32 - *y as i32;
                    return tmp;
                }
                while xml_is_blank_char(*x as u32) {
                    x = x.add(1);
                }
                while xml_is_blank_char(*y as u32) {
                    y = y.add(1);
                }
            } else {
                tmp = *x as i32 - *y as i32;
                x = x.add(1);
                y = y.add(1);
                if tmp < 0 {
                    return -1;
                }
                if tmp > 0 {
                    return 1;
                }
            }
        }
        if *x != 0 {
            while xml_is_blank_char(*x as u32) {
                x = x.add(1);
            }
            if *x != 0 {
                return 1;
            }
        }
        if *y != 0 {
            while xml_is_blank_char(*y as u32) {
                y = y.add(1);
            }
            if *y != 0 {
                return -1;
            }
        }
        0
    }
}

/// Compare 2 values
///
/// Returns -1 if x < y, 0 if x == y, 1 if x > y, 2 if x <> y, and -2 in case of error
#[doc(alias = "xmlSchemaCompareFloats")]
unsafe fn xml_schema_compare_floats(x: XmlSchemaValPtr, y: XmlSchemaValPtr) -> i32 {
    unsafe {
        let d1: f64;
        let d2: f64;

        if x.is_null() || y.is_null() {
            return -2;
        }

        // Cast everything to doubles.
        if (*x).typ == XmlSchemaValType::XmlSchemasDouble {
            d1 = (*x).value.d;
        } else if (*x).typ == XmlSchemaValType::XmlSchemasFloat {
            d1 = (*x).value.f as _;
        } else {
            return -2;
        }

        if (*y).typ == XmlSchemaValType::XmlSchemasDouble {
            d2 = (*y).value.d;
        } else if (*y).typ == XmlSchemaValType::XmlSchemasFloat {
            d2 = (*y).value.f as _;
        } else {
            return -2;
        }

        // Check for special cases.
        if xml_xpath_is_nan(d1) {
            if xml_xpath_is_nan(d2) {
                return 0;
            }
            return 1;
        }
        if xml_xpath_is_nan(d2) {
            return -1;
        }
        if d1 == XML_XPATH_PINF {
            if d2 == XML_XPATH_PINF {
                return 0;
            }
            return 1;
        }
        if d2 == XML_XPATH_PINF {
            return -1;
        }
        if d1 == XML_XPATH_NINF {
            if d2 == XML_XPATH_NINF {
                return 0;
            }
            return -1;
        }
        if d2 == XML_XPATH_NINF {
            return 1;
        }

        // basic tests, the last one we should have equality, but
        // portability is more important than speed and handling
        // NaN or Inf in a portable way is always a challenge, so ...
        if d1 < d2 {
            return -1;
        }
        if d1 > d2 {
            return 1;
        }
        if d1 == d2 {
            return 0;
        }
        2
    }
}

/// Compare 2 values
///
/// Returns -1 if x < y, 0 if x == y, 1 if x > y, 2 if x <> y, 3 if not
/// comparable and -2 in case of error
#[doc(alias = "xmlSchemaCompareValues")]
#[allow(clippy::too_many_arguments)]
unsafe fn xml_schema_compare_values_internal(
    xtype: XmlSchemaValType,
    x: XmlSchemaValPtr,
    xvalue: *const XmlChar,
    xws: XmlSchemaWhitespaceValueType,
    ytype: XmlSchemaValType,
    y: XmlSchemaValPtr,
    yvalue: *const XmlChar,
    yws: XmlSchemaWhitespaceValueType,
) -> i32 {
    unsafe {
        match xtype {
            XmlSchemaValType::XmlSchemasUnknown | XmlSchemaValType::XmlSchemasAnytype => {
                return -2;
            }
            XmlSchemaValType::XmlSchemasInteger
            | XmlSchemaValType::XmlSchemasNPInteger
            | XmlSchemaValType::XmlSchemasNInteger
            | XmlSchemaValType::XmlSchemasNNInteger
            | XmlSchemaValType::XmlSchemasPInteger
            | XmlSchemaValType::XmlSchemasInt
            | XmlSchemaValType::XmlSchemasUInt
            | XmlSchemaValType::XmlSchemasLong
            | XmlSchemaValType::XmlSchemasULong
            | XmlSchemaValType::XmlSchemasShort
            | XmlSchemaValType::XmlSchemasUShort
            | XmlSchemaValType::XmlSchemasByte
            | XmlSchemaValType::XmlSchemasUByte
            | XmlSchemaValType::XmlSchemasDecimal => {
                if x.is_null() || y.is_null() {
                    return -2;
                }
                if ytype == xtype {
                    return xml_schema_compare_decimals(x, y);
                }
                if matches!(
                    ytype,
                    XmlSchemaValType::XmlSchemasDecimal
                        | XmlSchemaValType::XmlSchemasInteger
                        | XmlSchemaValType::XmlSchemasNPInteger
                        | XmlSchemaValType::XmlSchemasNInteger
                        | XmlSchemaValType::XmlSchemasNNInteger
                        | XmlSchemaValType::XmlSchemasPInteger
                        | XmlSchemaValType::XmlSchemasInt
                        | XmlSchemaValType::XmlSchemasUInt
                        | XmlSchemaValType::XmlSchemasLong
                        | XmlSchemaValType::XmlSchemasULong
                        | XmlSchemaValType::XmlSchemasShort
                        | XmlSchemaValType::XmlSchemasUShort
                        | XmlSchemaValType::XmlSchemasByte
                        | XmlSchemaValType::XmlSchemasUByte
                ) {
                    return xml_schema_compare_decimals(x, y);
                }
                return -2;
            }
            XmlSchemaValType::XmlSchemasDuration => {
                if x.is_null() || y.is_null() {
                    return -2;
                }
                if ytype == XmlSchemaValType::XmlSchemasDuration {
                    return xml_schema_compare_durations(x, y);
                }
                return -2;
            }
            XmlSchemaValType::XmlSchemasTime
            | XmlSchemaValType::XmlSchemasGDay
            | XmlSchemaValType::XmlSchemasGMonth
            | XmlSchemaValType::XmlSchemasGMonthDay
            | XmlSchemaValType::XmlSchemasGYear
            | XmlSchemaValType::XmlSchemasGYearMonth
            | XmlSchemaValType::XmlSchemasDate
            | XmlSchemaValType::XmlSchemasDatetime => {
                if x.is_null() || y.is_null() {
                    return -2;
                }
                if matches!(
                    ytype,
                    XmlSchemaValType::XmlSchemasDatetime
                        | XmlSchemaValType::XmlSchemasTime
                        | XmlSchemaValType::XmlSchemasGDay
                        | XmlSchemaValType::XmlSchemasGMonth
                        | XmlSchemaValType::XmlSchemasGMonthDay
                        | XmlSchemaValType::XmlSchemasGYear
                        | XmlSchemaValType::XmlSchemasDate
                        | XmlSchemaValType::XmlSchemasGYearMonth
                ) {
                    return xml_schema_compare_dates(x, y);
                }
                return -2;
            }
            // Note that we will support comparison of string types against
            // anySimpleType as well.
            XmlSchemaValType::XmlSchemasAnySimpletype
            | XmlSchemaValType::XmlSchemasString
            | XmlSchemaValType::XmlSchemasNormString
            | XmlSchemaValType::XmlSchemasToken
            | XmlSchemaValType::XmlSchemasLanguage
            | XmlSchemaValType::XmlSchemasNmtoken
            | XmlSchemaValType::XmlSchemasName
            | XmlSchemaValType::XmlSchemasNCName
            | XmlSchemaValType::XmlSchemasID
            | XmlSchemaValType::XmlSchemasIDREF
            | XmlSchemaValType::XmlSchemasEntity
            | XmlSchemaValType::XmlSchemasAnyURI => {
                let xv = if x.is_null() { xvalue } else { (*x).value.str };
                let yv = if y.is_null() { yvalue } else { (*y).value.str };
                // TODO: Compare those against QName.
                if ytype == XmlSchemaValType::XmlSchemasQName {
                    // todo!();
                    if y.is_null() {
                        return -2;
                    }
                    return -2;
                }
                if matches!(
                    ytype,
                    XmlSchemaValType::XmlSchemasAnySimpletype
                        | XmlSchemaValType::XmlSchemasString
                        | XmlSchemaValType::XmlSchemasNormString
                        | XmlSchemaValType::XmlSchemasToken
                        | XmlSchemaValType::XmlSchemasLanguage
                        | XmlSchemaValType::XmlSchemasNmtoken
                        | XmlSchemaValType::XmlSchemasName
                        | XmlSchemaValType::XmlSchemasNCName
                        | XmlSchemaValType::XmlSchemasID
                        | XmlSchemaValType::XmlSchemasIDREF
                        | XmlSchemaValType::XmlSchemasEntity
                        | XmlSchemaValType::XmlSchemasAnyURI
                ) {
                    if xws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespacePreserve {
                        if yws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespacePreserve {
                            // TODO: What about x < y or x > y.
                            if xml_str_equal(xv, yv) {
                                return 0;
                            } else {
                                return 2;
                            }
                        } else if yws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceReplace {
                            return xml_schema_compare_preserve_replace_strings(xv, yv, 0);
                        } else if yws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse {
                            return xml_schema_compare_preserve_collapse_strings(xv, yv, 0);
                        }
                    } else if xws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceReplace {
                        if yws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespacePreserve {
                            return xml_schema_compare_preserve_replace_strings(yv, xv, 1);
                        }
                        if yws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceReplace {
                            return xml_schema_compare_replaced_strings(xv, yv);
                        }
                        if yws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse {
                            return xml_schema_compare_replace_collapse_strings(xv, yv, 0);
                        }
                    } else if xws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse {
                        if yws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespacePreserve {
                            return xml_schema_compare_preserve_collapse_strings(yv, xv, 1);
                        }
                        if yws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceReplace {
                            return xml_schema_compare_replace_collapse_strings(yv, xv, 1);
                        }
                        if yws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse {
                            return xml_schema_compare_norm_strings(xv, yv);
                        }
                    } else {
                        return -2;
                    }
                }
                return -2;
            }
            XmlSchemaValType::XmlSchemasQName | XmlSchemaValType::XmlSchemasNotation => {
                if x.is_null() || y.is_null() {
                    return -2;
                }
                if matches!(
                    ytype,
                    XmlSchemaValType::XmlSchemasQName | XmlSchemaValType::XmlSchemasNotation
                ) {
                    if xml_str_equal((*x).value.qname.name, (*y).value.qname.name)
                        && xml_str_equal((*x).value.qname.uri, (*y).value.qname.uri)
                    {
                        return 0;
                    }
                    return 2;
                }
                return -2;
            }
            XmlSchemaValType::XmlSchemasFloat | XmlSchemaValType::XmlSchemasDouble => {
                if x.is_null() || y.is_null() {
                    return -2;
                }
                if matches!(
                    ytype,
                    XmlSchemaValType::XmlSchemasFloat | XmlSchemaValType::XmlSchemasDouble
                ) {
                    return xml_schema_compare_floats(x, y);
                }
                return -2;
            }
            XmlSchemaValType::XmlSchemasBoolean => {
                if x.is_null() || y.is_null() {
                    return -2;
                }
                if ytype == XmlSchemaValType::XmlSchemasBoolean {
                    if (*x).value.b == (*y).value.b {
                        return 0;
                    }
                    if (*x).value.b == 0 {
                        return -1;
                    }
                    return 1;
                }
                return -2;
            }
            XmlSchemaValType::XmlSchemasHexbinary => {
                if x.is_null() || y.is_null() {
                    return -2;
                }
                if ytype == XmlSchemaValType::XmlSchemasHexbinary {
                    match (*x).value.hex.total.cmp(&(*y).value.hex.total) {
                        std::cmp::Ordering::Equal => {
                            let ret: i32 = xml_strcmp((*x).value.hex.str, (*y).value.hex.str);
                            match ret.cmp(&0) {
                                std::cmp::Ordering::Greater => {
                                    return 1;
                                }
                                std::cmp::Ordering::Equal => {
                                    return 0;
                                }
                                std::cmp::Ordering::Less => {}
                            }
                        }
                        std::cmp::Ordering::Greater => {
                            return 1;
                        }
                        std::cmp::Ordering::Less => {}
                    }

                    return -1;
                }
                return -2;
            }
            XmlSchemaValType::XmlSchemasBase64binary => {
                if x.is_null() || y.is_null() {
                    return -2;
                }
                if ytype == XmlSchemaValType::XmlSchemasBase64binary {
                    match (*x).value.base64.total.cmp(&(*y).value.base64.total) {
                        std::cmp::Ordering::Equal => {
                            let ret: i32 = xml_strcmp((*x).value.base64.str, (*y).value.base64.str);
                            match ret.cmp(&0) {
                                std::cmp::Ordering::Greater => {
                                    return 1;
                                }
                                std::cmp::Ordering::Equal => {
                                    return 0;
                                }
                                std::cmp::Ordering::Less => {
                                    return -1;
                                }
                            }
                        }
                        std::cmp::Ordering::Greater => {
                            return 1;
                        }
                        std::cmp::Ordering::Less => {
                            return -1;
                        }
                    }
                }
                return -2;
            }
            XmlSchemaValType::XmlSchemasIDREFS
            | XmlSchemaValType::XmlSchemasEntities
            | XmlSchemaValType::XmlSchemasNmtokens => {
                // todo!()
            }
        }
        -2
    }
}

/// Compare 2 values
///
/// Returns -1 if x < y, 0 if x == y, 1 if x > y, 2 if x <> y, and -2 in
/// case of error
#[doc(alias = "xmlSchemaCompareValuesWhtspExt")]
#[allow(clippy::too_many_arguments)]
unsafe fn xml_schema_compare_values_whtsp_ext(
    xtype: XmlSchemaValType,
    x: XmlSchemaValPtr,
    xvalue: *const XmlChar,
    xws: XmlSchemaWhitespaceValueType,
    ytype: XmlSchemaValType,
    y: XmlSchemaValPtr,
    yvalue: *const XmlChar,
    yws: XmlSchemaWhitespaceValueType,
) -> i32 {
    unsafe { xml_schema_compare_values_internal(xtype, x, xvalue, xws, ytype, y, yvalue, yws) }
}

/// Computes the UTF8 length of the normalized value of the string
///
/// Returns the length or -1 in case of error.
#[doc(alias = "xmlSchemaNormLen")]
unsafe fn xml_schema_norm_len(value: *const XmlChar) -> i32 {
    unsafe {
        let mut utf: *const XmlChar;
        let mut ret: i32 = 0;

        if value.is_null() {
            return -1;
        }
        utf = value;
        while xml_is_blank_char(*utf as u32) {
            utf = utf.add(1);
        }
        while *utf != 0 {
            if *utf.add(0) & 0x80 != 0 {
                if *utf.add(1) & 0xc0 != 0x80 {
                    return -1;
                }
                if *utf.add(0) & 0xe0 == 0xe0 {
                    if *utf.add(2) & 0xc0 != 0x80 {
                        return -1;
                    }
                    if *utf.add(0) & 0xf0 == 0xf0 {
                        if *utf.add(0) & 0xf8 != 0xf0 || *utf.add(3) & 0xc0 != 0x80 {
                            return -1;
                        }
                        utf = utf.add(4);
                    } else {
                        utf = utf.add(3);
                    }
                } else {
                    utf = utf.add(2);
                }
            } else if xml_is_blank_char(*utf as u32) {
                while xml_is_blank_char(*utf as u32) {
                    utf = utf.add(1);
                }
                if *utf == 0 {
                    break;
                }
            } else {
                utf = utf.add(1);
            }
            ret += 1;
        }
        ret
    }
}

/// Check a value against a facet condition
///
/// Returns 0 if the element is schemas valid, a positive error code
/// number otherwise and -1 in case of internal or API error.
#[doc(alias = "xmlSchemaValidateFacetInternal")]
unsafe fn xml_schema_validate_facet_internal(
    facet: XmlSchemaFacetPtr,
    fws: XmlSchemaWhitespaceValueType,
    val_type: XmlSchemaValType,
    mut value: *const XmlChar,
    val: XmlSchemaValPtr,
    ws: XmlSchemaWhitespaceValueType,
) -> i32 {
    unsafe {
        let ret: i32;

        if facet.is_null() {
            return -1;
        }

        match (*facet).typ {
            XmlSchemaTypeType::XmlSchemaFacetPattern => {
                // NOTE that for patterns, the @value needs to be the normalized
                // value, *not* the lexical initial value or the canonical value.
                if value.is_null() {
                    return -1;
                }
                // If string-derived type, regexp must be tested on the value space of
                // the datatype.
                // See https://www.w3.org/TR/xmlschema-2/#rf-pattern
                if !val.is_null()
                    && !(*val).value.str.is_null()
                    && (((*val).typ as i32 >= XmlSchemaValType::XmlSchemasString as i32
                        && (*val).typ as i32 <= XmlSchemaValType::XmlSchemasNormString as i32)
                        || ((*val).typ as i32 >= XmlSchemaValType::XmlSchemasToken as i32
                            && (*val).typ as i32 <= XmlSchemaValType::XmlSchemasEntities as i32
                            && (*val).typ as i32 != XmlSchemaValType::XmlSchemasQName as i32))
                {
                    value = (*val).value.str;
                }
                ret = xml_regexp_exec((*facet).regexp, value);
                if ret == 1 {
                    return 0;
                }
                if ret == 0 {
                    return XmlParserErrors::XmlSchemavCvcPatternValid as i32;
                }
                return ret;
            }
            XmlSchemaTypeType::XmlSchemaFacetMaxExclusive => {
                ret = xml_schema_compare_values(val, (*facet).val);
                if ret == -2 {
                    return -1;
                }
                if ret == -1 {
                    return 0;
                }
                return XmlParserErrors::XmlSchemavCvcMaxExclusiveValid as i32;
            }
            XmlSchemaTypeType::XmlSchemaFacetMaxInclusive => {
                ret = xml_schema_compare_values(val, (*facet).val);
                if ret == -2 {
                    return -1;
                }
                if ret == -1 || ret == 0 {
                    return 0;
                }
                return XmlParserErrors::XmlSchemavCvcMaxInclusiveValid as i32;
            }
            XmlSchemaTypeType::XmlSchemaFacetMinExclusive => {
                ret = xml_schema_compare_values(val, (*facet).val);
                if ret == -2 {
                    return -1;
                }
                if ret == 1 {
                    return 0;
                }
                return XmlParserErrors::XmlSchemavCvcMinExclusiveValid as i32;
            }
            XmlSchemaTypeType::XmlSchemaFacetMinInclusive => {
                ret = xml_schema_compare_values(val, (*facet).val);
                if ret == -2 {
                    return -1;
                }
                if ret == 1 || ret == 0 {
                    return 0;
                }
                return XmlParserErrors::XmlSchemavCvcMinInclusiveValid as i32;
            }
            XmlSchemaTypeType::XmlSchemaFacetWhitespace => {
                // TODO whitespaces
                // NOTE: Whitespace should be handled to normalize
                // the value to be validated against a the facets;
                // not to normalize the value in-between.
                return 0;
            }
            XmlSchemaTypeType::XmlSchemaFacetEnumeration => {
                if ws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceUnknown {
                    // This is to ensure API compatibility with the old
                    // xmlSchemaValidateFacet().
                    // TODO: Get rid of this case.
                    if !(*facet).value.is_null() && xml_str_equal((*facet).value, value) {
                        return 0;
                    }
                } else {
                    ret = xml_schema_compare_values_whtsp_ext(
                        (*(*facet).val).typ,
                        (*facet).val,
                        (*facet).value,
                        fws,
                        val_type,
                        val,
                        value,
                        ws,
                    );
                    if ret == -2 {
                        return -1;
                    }
                    if ret == 0 {
                        return 0;
                    }
                }
                return XmlParserErrors::XmlSchemavCvcEnumerationValid as i32;
            }
            ty @ XmlSchemaTypeType::XmlSchemaFacetLength
            | ty @ XmlSchemaTypeType::XmlSchemaFacetMaxLength
            | ty @ XmlSchemaTypeType::XmlSchemaFacetMinLength => {
                if matches!(ty, XmlSchemaTypeType::XmlSchemaFacetLength) {
                    // SPEC (1.3) "if {primitive type definition} is QName or NOTATION,
                    // then any {value} is facet-valid."
                    if matches!(
                        val_type,
                        XmlSchemaValType::XmlSchemasQName | XmlSchemaValType::XmlSchemasNotation
                    ) {
                        return 0;
                    }
                }

                let mut len: u32 = 0;

                if matches!(
                    val_type,
                    XmlSchemaValType::XmlSchemasQName | XmlSchemaValType::XmlSchemasNotation
                ) {
                    return 0;
                }
                // TODO: length, maxLength and minLength must be of type
                // nonNegativeInteger only. Check if decimal is used somehow.
                if (*facet).val.is_null()
                    || !matches!(
                        (*(*facet).val).typ,
                        XmlSchemaValType::XmlSchemasDecimal | XmlSchemaValType::XmlSchemasNNInteger
                    )
                    || (*(*facet).val).value.decimal.frac != 0
                {
                    return -1;
                }
                if !val.is_null() && (*val).typ == XmlSchemaValType::XmlSchemasHexbinary {
                    len = (*val).value.hex.total;
                } else if !val.is_null() && (*val).typ == XmlSchemaValType::XmlSchemasBase64binary {
                    len = (*val).value.base64.total;
                } else {
                    match val_type {
                        XmlSchemaValType::XmlSchemasString
                        | XmlSchemaValType::XmlSchemasNormString => {
                            if ws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceUnknown {
                                // This is to ensure API compatibility with the old
                                // xmlSchemaValidateFacet(). Anyway, this was and
                                // is not the correct handling.
                                // TODO: Get rid of this case somehow.
                                if val_type == XmlSchemaValType::XmlSchemasString {
                                    len = xml_utf8_strlen(value) as _;
                                } else {
                                    len = xml_schema_norm_len(value) as _;
                                }
                            } else if !value.is_null() {
                                if ws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse {
                                    len = xml_schema_norm_len(value) as _;
                                } else {
                                    // Should be OK for "preserve" as well.
                                    len = xml_utf8_strlen(value) as _;
                                }
                            }
                        }
                        XmlSchemaValType::XmlSchemasIDREF
                        | XmlSchemaValType::XmlSchemasToken
                        | XmlSchemaValType::XmlSchemasLanguage
                        | XmlSchemaValType::XmlSchemasNmtoken
                        | XmlSchemaValType::XmlSchemasName
                        | XmlSchemaValType::XmlSchemasNCName
                        | XmlSchemaValType::XmlSchemasID
                        | XmlSchemaValType::XmlSchemasAnyURI => {
                            if !value.is_null() {
                                len = xml_schema_norm_len(value) as _;
                            }
                        }
                        _ => {
                            todo!()
                        }
                    }
                }
                if (*facet).typ == XmlSchemaTypeType::XmlSchemaFacetLength {
                    if len as u64 != (*(*facet).val).value.decimal.lo {
                        return XmlParserErrors::XmlSchemavCvcLengthValid as i32;
                    }
                } else if (*facet).typ == XmlSchemaTypeType::XmlSchemaFacetMinLength {
                    if (len as u64) < (*(*facet).val).value.decimal.lo {
                        return XmlParserErrors::XmlSchemavCvcMinLengthValid as i32;
                    }
                } else if len as u64 > (*(*facet).val).value.decimal.lo {
                    return XmlParserErrors::XmlSchemavCvcMaxLengthValid as i32;
                }
            }
            XmlSchemaTypeType::XmlSchemaFacetTotalDigits
            | XmlSchemaTypeType::XmlSchemaFacetFractionDigits => {
                if (*facet).val.is_null()
                    || !matches!(
                        (*(*facet).val).typ,
                        XmlSchemaValType::XmlSchemasPInteger
                            | XmlSchemaValType::XmlSchemasNNInteger
                    )
                    || (*(*facet).val).value.decimal.frac != 0
                {
                    return -1;
                }
                if val.is_null()
                    || !matches!(
                        (*val).typ,
                        XmlSchemaValType::XmlSchemasDecimal
                            | XmlSchemaValType::XmlSchemasInteger
                            | XmlSchemaValType::XmlSchemasNPInteger
                            | XmlSchemaValType::XmlSchemasNInteger
                            | XmlSchemaValType::XmlSchemasNNInteger
                            | XmlSchemaValType::XmlSchemasPInteger
                            | XmlSchemaValType::XmlSchemasInt
                            | XmlSchemaValType::XmlSchemasUInt
                            | XmlSchemaValType::XmlSchemasLong
                            | XmlSchemaValType::XmlSchemasULong
                            | XmlSchemaValType::XmlSchemasShort
                            | XmlSchemaValType::XmlSchemasUShort
                            | XmlSchemaValType::XmlSchemasByte
                            | XmlSchemaValType::XmlSchemasUByte
                    )
                {
                    return -1;
                }
                if (*facet).typ == XmlSchemaTypeType::XmlSchemaFacetTotalDigits {
                    if (*val).value.decimal.total as u64 > (*(*facet).val).value.decimal.lo {
                        return XmlParserErrors::XmlSchemavCvcTotalDigitsValid as i32;
                    }
                } else if (*facet).typ == XmlSchemaTypeType::XmlSchemaFacetFractionDigits
                    && (*val).value.decimal.frac as u64 > (*(*facet).val).value.decimal.lo
                {
                    return XmlParserErrors::XmlSchemavCvcFractionDigitsValid as i32;
                }
            }
            _ => {
                todo!()
            }
        }
        0
    }
}

/// Check a value against a facet condition
///
/// Returns 0 if the element is schemas valid, a positive error code
/// number otherwise and -1 in case of internal or API error.
#[doc(alias = "xmlSchemaValidateFacet")]
pub unsafe fn xml_schema_validate_facet(
    base: XmlSchemaTypePtr,
    facet: XmlSchemaFacetPtr,
    value: *const XmlChar,
    val: XmlSchemaValPtr,
) -> i32 {
    unsafe {
        // This tries to ensure API compatibility regarding the old
        // xmlSchemaValidateFacet() and the new xmlSchemaValidateFacetInternal() and
        // xmlSchemaValidateFacetWhtsp().
        if !val.is_null() {
            return xml_schema_validate_facet_internal(
                facet,
                XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceUnknown,
                (*val).typ,
                value,
                val,
                XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceUnknown,
            );
        } else if !base.is_null() {
            return xml_schema_validate_facet_internal(
                facet,
                XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceUnknown,
                (*base).built_in_type.try_into().unwrap(),
                value,
                val,
                XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceUnknown,
            );
        }
        -1
    }
}

/// Check a value against a facet condition. This takes value normalization
/// according to the specified whitespace types into account.
/// Note that @value needs to be the *normalized* value if the facet
/// is of type "pattern".
///
/// Returns 0 if the element is schemas valid, a positive error code
///     number otherwise and -1 in case of internal or API error.
#[doc(alias = "xmlSchemaValidateFacetWhtsp")]
pub unsafe fn xml_schema_validate_facet_whtsp(
    facet: XmlSchemaFacetPtr,
    fws: XmlSchemaWhitespaceValueType,
    val_type: XmlSchemaValType,
    value: *const XmlChar,
    val: XmlSchemaValPtr,
    ws: XmlSchemaWhitespaceValueType,
) -> i32 {
    unsafe { xml_schema_validate_facet_internal(facet, fws, val_type, value, val, ws) }
}

/// Cleanup the default XML Schemas type library
#[doc(alias = "xmlSchemaFreeValue")]
pub unsafe fn xml_schema_free_value(mut value: XmlSchemaValPtr) {
    unsafe {
        let mut prev: XmlSchemaValPtr;

        while !value.is_null() {
            match (*value).typ {
                XmlSchemaValType::XmlSchemasString
                | XmlSchemaValType::XmlSchemasNormString
                | XmlSchemaValType::XmlSchemasToken
                | XmlSchemaValType::XmlSchemasLanguage
                | XmlSchemaValType::XmlSchemasNmtoken
                | XmlSchemaValType::XmlSchemasNmtokens
                | XmlSchemaValType::XmlSchemasName
                | XmlSchemaValType::XmlSchemasNCName
                | XmlSchemaValType::XmlSchemasID
                | XmlSchemaValType::XmlSchemasIDREF
                | XmlSchemaValType::XmlSchemasIDREFS
                | XmlSchemaValType::XmlSchemasEntity
                | XmlSchemaValType::XmlSchemasEntities
                | XmlSchemaValType::XmlSchemasAnyURI
                | XmlSchemaValType::XmlSchemasAnySimpletype => {
                    if !(*value).value.str.is_null() {
                        xml_free((*value).value.str as _);
                    }
                }
                XmlSchemaValType::XmlSchemasNotation | XmlSchemaValType::XmlSchemasQName => {
                    if !(*value).value.qname.uri.is_null() {
                        xml_free((*value).value.qname.uri as _);
                    }
                    if !(*value).value.qname.name.is_null() {
                        xml_free((*value).value.qname.name as _);
                    }
                }
                XmlSchemaValType::XmlSchemasHexbinary => {
                    if !(*value).value.hex.str.is_null() {
                        xml_free((*value).value.hex.str as _);
                    }
                }
                XmlSchemaValType::XmlSchemasBase64binary => {
                    if !(*value).value.base64.str.is_null() {
                        xml_free((*value).value.base64.str as _);
                    }
                }
                _ => {}
            }
            prev = value;
            value = (*value).next;
            xml_free(prev as _);
        }
    }
}

/// Allocate a new Facet structure.
///
/// Returns the newly allocated structure or NULL in case or error
#[doc(alias = "xmlSchemaNewFacet")]
pub unsafe fn xml_schema_new_facet() -> XmlSchemaFacetPtr {
    unsafe {
        let ret: XmlSchemaFacetPtr = xml_malloc(size_of::<XmlSchemaFacet>()) as _;
        if ret.is_null() {
            return null_mut();
        }
        memset(ret as _, 0, size_of::<XmlSchemaFacet>());

        ret
    }
}

macro_rules! PERROR_INT {
    ($pctxt:expr, $func:expr, $msg:expr) => {
        $crate::xmlschemas::error::xml_schema_internal_err(
            $pctxt as XmlSchemaAbstractCtxtPtr,
            $func,
            $msg,
        );
    };
}

macro_rules! FREE_AND_NULL {
    ($str:expr) => {
        if !$str.is_null() {
            xml_free($str as _);
            #[allow(unused_assignments)]
            {
                $str = null_mut();
            }
        }
    };
}

/// Checks and computes the values of facets.
///
/// Returns 0 if valid, a positive error code if not valid and
/// -1 in case of an internal or API error.
#[doc(alias = "xmlSchemaCheckFacet")]
pub unsafe fn xml_schema_check_facet(
    facet: XmlSchemaFacetPtr,
    type_decl: XmlSchemaTypePtr,
    mut pctxt: XmlSchemaParserCtxtPtr,
    _name: *const XmlChar,
) -> i32 {
    unsafe {
        let mut ret: i32 = 0;

        if facet.is_null() || type_decl.is_null() {
            return -1;
        }
        // TODO: will the parser context be given if used from
        // the relaxNG module?
        let ctxt_given = if pctxt.is_null() { 0 } else { 1 };

        match (*facet).typ {
            XmlSchemaTypeType::XmlSchemaFacetMinInclusive
            | XmlSchemaTypeType::XmlSchemaFacetMinExclusive
            | XmlSchemaTypeType::XmlSchemaFacetMaxInclusive
            | XmlSchemaTypeType::XmlSchemaFacetMaxExclusive
            | XmlSchemaTypeType::XmlSchemaFacetEnumeration => {
                // Okay we need to validate the value at that point.
                let base: XmlSchemaTypePtr;

                // 4.3.5.5 Constraints on enumeration Schema Components
                // Schema Component Constraint: enumeration valid restriction
                // It is an `error` if any member of {value} is not in the
                // `value space` of {base type definition}.
                //
                // minInclusive, maxInclusive, minExclusive, maxExclusive:
                // The value `must` be in the
                // `value space` of the `base type`.

                // This function is intended to deliver a compiled value
                // on the facet. In this implementation of XML Schemata the
                // type holding a facet, won't be a built-in type.
                // Thus to ensure that other API
                // calls (relaxng) do work, if the given type is a built-in
                // type, we will assume that the given built-in type *is
                // already* the base type.
                if (*type_decl).typ != XmlSchemaTypeType::XmlSchemaTypeBasic {
                    base = (*type_decl).base_type;
                    if base.is_null() {
                        PERROR_INT!(
                            pctxt,
                            "xmlSchemaCheckFacet",
                            "a type user derived type has no base type"
                        );
                        return -1;
                    }
                } else {
                    base = type_decl;
                }

                if ctxt_given == 0 {
                    // A context is needed if called from RelaxNG.
                    pctxt = xml_schema_new_parser_ctxt("*");
                    if pctxt.is_null() {
                        return -1;
                    }
                }
                // NOTE: This call does not check the content nodes,
                // since they are not available:
                // (*facet).node is just the node holding the facet
                // definition, *not* the attribute holding the *value*
                // of the facet.
                ret = xml_schema_vcheck_cvc_simple_type(
                    pctxt as XmlSchemaAbstractCtxtPtr,
                    (*facet).node.map(|node| node.into()),
                    base,
                    (*facet).value,
                    addr_of_mut!((*facet).val),
                    1,
                    1,
                    0,
                );
                if ret != 0 {
                    if ret < 0 {
                        // No error message for RelaxNG.
                        if ctxt_given != 0 {
                            let value =
                                CStr::from_ptr((*facet).value as *const i8).to_string_lossy();
                            let facet_type = xml_schema_facet_type_to_string((*facet).typ);
                            xml_schema_custom_err(
                            pctxt as XmlSchemaAbstractCtxtPtr,
                            XmlParserErrors::XmlSchemapInternal,
                            (*facet).node.map(|node| node.into()),
                            null_mut(),
                            format!("Internal error: xmlSchemaCheckFacet, failed to validate the value '{value}' of the facet '{facet_type}' against the base type").as_str(),
                            Some(&value),
                            Some(facet_type),
                            );
                        }
                        // goto internal_error;
                        if ctxt_given == 0 && !pctxt.is_null() {
                            xml_schema_free_parser_ctxt(pctxt);
                        }
                        return -1;
                    }
                    ret = XmlParserErrors::XmlSchemapInvalidFacetValue as i32;
                    // No error message for RelaxNG.
                    if ctxt_given != 0 {
                        let value = CStr::from_ptr((*facet).value as *const i8).to_string_lossy();
                        let qname = xml_schema_format_qname(
                            Some(
                                CStr::from_ptr((*base).target_namespace as *const i8)
                                    .to_string_lossy()
                                    .as_ref(),
                            ),
                            Some(
                                CStr::from_ptr((*base).name as *const i8)
                                    .to_string_lossy()
                                    .as_ref(),
                            ),
                        );

                        xml_schema_custom_err(
                            pctxt as XmlSchemaAbstractCtxtPtr,
                            ret.try_into().unwrap(),
                            (*facet).node.map(|node| node.into()),
                            facet as XmlSchemaBasicItemPtr,
                            format!("The value '{value}' of the facet does not validate against the base type '{qname}'").as_str(),
                            Some(&value),
                            Some(&qname),
                        );
                    }
                    // goto exit;
                    if ctxt_given == 0 && !pctxt.is_null() {
                        xml_schema_free_parser_ctxt(pctxt);
                    }
                    return ret;
                } else if (*facet).val.is_null() && ctxt_given != 0 {
                    PERROR_INT!(pctxt, "xmlSchemaCheckFacet", "value was not computed");
                }
            }
            XmlSchemaTypeType::XmlSchemaFacetPattern => {
                (*facet).regexp = xml_regexp_compile((*facet).value);
                if (*facet).regexp.is_null() {
                    ret = XmlParserErrors::XmlSchemapRegexpInvalid as i32;
                    // No error message for RelaxNG.
                    if ctxt_given != 0 {
                        let value = CStr::from_ptr((*facet).value as *const i8).to_string_lossy();
                        xml_schema_custom_err(
                        pctxt as XmlSchemaAbstractCtxtPtr,
                        ret.try_into().unwrap(),
                        (*facet).node.map(|node| node.into()),
                        type_decl as XmlSchemaBasicItemPtr,
                        format!("The value '{value}' of the facet 'pattern' is not a valid regular expression").as_str(),
                        Some(&value),
                        None,
                    );
                    }
                }
            }
            XmlSchemaTypeType::XmlSchemaFacetTotalDigits
            | XmlSchemaTypeType::XmlSchemaFacetFractionDigits
            | XmlSchemaTypeType::XmlSchemaFacetLength
            | XmlSchemaTypeType::XmlSchemaFacetMaxLength
            | XmlSchemaTypeType::XmlSchemaFacetMinLength => {
                if (*facet).typ == XmlSchemaTypeType::XmlSchemaFacetTotalDigits {
                    ret = xml_schema_validate_predefined_type(
                        xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasPInteger),
                        (*facet).value,
                        addr_of_mut!((*facet).val),
                    );
                } else {
                    ret = xml_schema_validate_predefined_type(
                        xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasNNInteger),
                        (*facet).value,
                        addr_of_mut!((*facet).val),
                    );
                }
                if ret != 0 {
                    if ret < 0 {
                        // No error message for RelaxNG.
                        if ctxt_given != 0 {
                            PERROR_INT!(pctxt, "xmlSchemaCheckFacet", "validating facet value");
                        }
                        // goto internal_error;
                        if ctxt_given == 0 && !pctxt.is_null() {
                            xml_schema_free_parser_ctxt(pctxt);
                        }
                        return -1;
                    }
                    ret = XmlParserErrors::XmlSchemapInvalidFacetValue as i32;
                    // No error message for RelaxNG.
                    if ctxt_given != 0 {
                        let value = CStr::from_ptr((*facet).value as *const i8).to_string_lossy();
                        let facet_type = xml_schema_facet_type_to_string((*facet).typ);
                        let typename =
                            if (*facet).typ != XmlSchemaTypeType::XmlSchemaFacetTotalDigits {
                                "nonNegativeInteger"
                            } else {
                                "positiveInteger"
                            };
                        // error code
                        xml_schema_custom_err4(
                        pctxt as XmlSchemaAbstractCtxtPtr,
                        ret.try_into().unwrap(),
                        (*facet).node.map(|node| node.into()),
                        type_decl as XmlSchemaBasicItemPtr,
                        format!(
                            "The value '{value}' of the facet '{facet_type}' is not a valid '{typename}'"
                        )
                        .as_str(),
                        Some(&value),
                        Some(facet_type),
                        Some(typename),
                        None,
                    );
                    }
                }
            }
            XmlSchemaTypeType::XmlSchemaFacetWhitespace => {
                if xml_str_equal((*facet).value, c"preserve".as_ptr() as _) {
                    (*facet).whitespace = XML_SCHEMAS_FACET_PRESERVE;
                } else if xml_str_equal((*facet).value, c"replace".as_ptr() as _) {
                    (*facet).whitespace = XML_SCHEMAS_FACET_REPLACE;
                } else if xml_str_equal((*facet).value, c"collapse".as_ptr() as _) {
                    (*facet).whitespace = XML_SCHEMAS_FACET_COLLAPSE;
                } else {
                    ret = XmlParserErrors::XmlSchemapInvalidFacetValue as i32;
                    // No error message for RelaxNG.
                    if ctxt_given != 0 {
                        let value = CStr::from_ptr((*facet).value as *const i8).to_string_lossy();
                        // error was previously: XML_SCHEMAP_INVALID_WHITE_SPACE
                        xml_schema_custom_err(
                            pctxt as XmlSchemaAbstractCtxtPtr,
                            ret.try_into().unwrap(),
                            (*facet).node.map(|node| node.into()),
                            type_decl as XmlSchemaBasicItemPtr,
                            format!("The value '{value}' of the facet 'whitespace' is not valid")
                                .as_str(),
                            Some(&value),
                            None,
                        );
                    }
                }
            }
            _ => {}
        }
        // exit:
        if ctxt_given == 0 && !pctxt.is_null() {
            xml_schema_free_parser_ctxt(pctxt);
        }
        ret
        // internal_error:
        //     if ((! ctxtGiven) && !pctxt.is_null()) {
        // 		xmlSchemaFreeParserCtxt(pctxt);
        // 	}
        //     return -1;
    }
}

/// Deallocate a Schema Facet structure.
#[doc(alias = "xmlSchemaFreeFacet")]
pub unsafe fn xml_schema_free_facet(facet: XmlSchemaFacetPtr) {
    unsafe {
        if facet.is_null() {
            return;
        }
        if !(*facet).val.is_null() {
            xml_schema_free_value((*facet).val);
        }
        if !(*facet).regexp.is_null() {
            xml_reg_free_regexp((*facet).regexp);
        }
        if !(*facet).annot.is_null() {
            xml_schema_free_annot((*facet).annot);
        }
        xml_free(facet as _);
    }
}

/// Compare 2 values
///
/// Returns -1 if x < y, 0 if x == y, 1 if x > y, 2 if x <> y, and -2 in case of error
#[doc(alias = "xmlSchemaCompareValues")]
pub unsafe fn xml_schema_compare_values(x: XmlSchemaValPtr, y: XmlSchemaValPtr) -> i32 {
    unsafe {
        if x.is_null() || y.is_null() {
            return -2;
        }
        let xws = if (*x).typ == XmlSchemaValType::XmlSchemasString {
            XmlSchemaWhitespaceValueType::XmlSchemaWhitespacePreserve
        } else if (*x).typ == XmlSchemaValType::XmlSchemasNormString {
            XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceReplace
        } else {
            XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse
        };

        let yws = if (*y).typ == XmlSchemaValType::XmlSchemasString {
            XmlSchemaWhitespaceValueType::XmlSchemaWhitespacePreserve
        } else if (*y).typ == XmlSchemaValType::XmlSchemasNormString {
            XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceReplace
        } else {
            XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse
        };

        xml_schema_compare_values_internal(
            (*x).typ,
            x,
            null_mut(),
            xws,
            (*y).typ,
            y,
            null_mut(),
            yws,
        )
    }
}

/// Lookup function
///
/// Returns the item type of @type as defined by the built-in datatype
/// hierarchy of XML Schema Part 2: Datatypes, or NULL in case of an error.
#[doc(alias = "xmlSchemaGetBuiltInListSimpleTypeItemType")]
pub unsafe fn xml_schema_get_built_in_list_simple_type_item_type(
    typ: XmlSchemaTypePtr,
) -> XmlSchemaTypePtr {
    unsafe {
        if typ.is_null() || (*typ).typ != XmlSchemaTypeType::XmlSchemaTypeBasic {
            return null_mut();
        }
        match XmlSchemaValType::try_from((*typ).built_in_type) {
            Ok(XmlSchemaValType::XmlSchemasNmtokens) => XML_SCHEMA_TYPE_NMTOKEN_DEF.get(),
            Ok(XmlSchemaValType::XmlSchemasIDREFS) => XML_SCHEMA_TYPE_IDREF_DEF.get(),
            Ok(XmlSchemaValType::XmlSchemasEntities) => XML_SCHEMA_TYPE_ENTITY_DEF.get(),
            _ => null_mut(),
        }
    }
}

/// Checks the value of a list simple type against a facet.
///
/// Returns 0 if the value is valid, a positive error code
/// number otherwise and -1 in case of an internal error.
#[doc(alias = "xmlSchemaValidateListSimpleTypeFacet")]
pub unsafe fn xml_schema_validate_list_simple_type_facet(
    facet: XmlSchemaFacetPtr,
    value: *const XmlChar,
    actual_len: u64,
    expected_len: *mut u64,
) -> i32 {
    unsafe {
        if facet.is_null() {
            return -1;
        }
        // TODO: Check if this will work with large numbers.
        // (compare value.decimal.mi and value.decimal.hi as well?).
        if (*facet).typ == XmlSchemaTypeType::XmlSchemaFacetLength {
            if actual_len != (*(*facet).val).value.decimal.lo {
                if !expected_len.is_null() {
                    *expected_len = (*(*facet).val).value.decimal.lo;
                }
                return XmlParserErrors::XmlSchemavCvcLengthValid as i32;
            }
        } else if (*facet).typ == XmlSchemaTypeType::XmlSchemaFacetMinLength {
            if actual_len < (*(*facet).val).value.decimal.lo {
                if !expected_len.is_null() {
                    *expected_len = (*(*facet).val).value.decimal.lo;
                }
                return XmlParserErrors::XmlSchemavCvcMinLengthValid as i32;
            }
        } else if (*facet).typ == XmlSchemaTypeType::XmlSchemaFacetMaxLength {
            if actual_len > (*(*facet).val).value.decimal.lo {
                if !expected_len.is_null() {
                    *expected_len = (*(*facet).val).value.decimal.lo;
                }
                return XmlParserErrors::XmlSchemavCvcMaxLengthValid as i32;
            }
        } else {
            // NOTE: That we can pass NULL as xmlSchemaValPtr to
            // xmlSchemaValidateFacet, since the remaining facet types
            // are: xmlSchemaTypeType::XML_SCHEMA_FACET_PATTERN.as_ptr() as _, xmlSchemaTypeType::XML_SCHEMA_FACET_ENUMERATION.
            return xml_schema_validate_facet(null_mut(), facet, value, null_mut());
        }
        0
    }
}

/// Gives you the type struct for a built-in
/// type by its type id.
///
/// Returns the type if found, NULL otherwise.
#[doc(alias = "xmlSchemaGetBuiltInType")]
pub unsafe fn xml_schema_get_built_in_type(typ: XmlSchemaValType) -> XmlSchemaTypePtr {
    unsafe {
        if !XML_SCHEMA_TYPES_INITIALIZED.get() && xml_schema_init_types() < 0 {
            return null_mut();
        }
        match typ {
            XmlSchemaValType::XmlSchemasAnySimpletype => XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.get(),
            XmlSchemaValType::XmlSchemasString => XML_SCHEMA_TYPE_STRING_DEF.get(),
            XmlSchemaValType::XmlSchemasNormString => XML_SCHEMA_TYPE_NORM_STRING_DEF.get(),
            XmlSchemaValType::XmlSchemasDecimal => XML_SCHEMA_TYPE_DECIMAL_DEF.get(),
            XmlSchemaValType::XmlSchemasTime => XML_SCHEMA_TYPE_TIME_DEF.get(),
            XmlSchemaValType::XmlSchemasGDay => XML_SCHEMA_TYPE_GDAY_DEF.get(),
            XmlSchemaValType::XmlSchemasGMonth => XML_SCHEMA_TYPE_GMONTH_DEF.get(),
            XmlSchemaValType::XmlSchemasGMonthDay => XML_SCHEMA_TYPE_GMONTH_DAY_DEF.get(),
            XmlSchemaValType::XmlSchemasGYear => XML_SCHEMA_TYPE_GYEAR_DEF.get(),
            XmlSchemaValType::XmlSchemasGYearMonth => XML_SCHEMA_TYPE_GYEAR_MONTH_DEF.get(),
            XmlSchemaValType::XmlSchemasDate => XML_SCHEMA_TYPE_DATE_DEF.get(),
            XmlSchemaValType::XmlSchemasDatetime => XML_SCHEMA_TYPE_DATETIME_DEF.get(),
            XmlSchemaValType::XmlSchemasDuration => XML_SCHEMA_TYPE_DURATION_DEF.get(),
            XmlSchemaValType::XmlSchemasFloat => XML_SCHEMA_TYPE_FLOAT_DEF.get(),
            XmlSchemaValType::XmlSchemasDouble => XML_SCHEMA_TYPE_DOUBLE_DEF.get(),
            XmlSchemaValType::XmlSchemasBoolean => XML_SCHEMA_TYPE_BOOLEAN_DEF.get(),
            XmlSchemaValType::XmlSchemasToken => XML_SCHEMA_TYPE_TOKEN_DEF.get(),
            XmlSchemaValType::XmlSchemasLanguage => XML_SCHEMA_TYPE_LANGUAGE_DEF.get(),
            XmlSchemaValType::XmlSchemasNmtoken => XML_SCHEMA_TYPE_NMTOKEN_DEF.get(),
            XmlSchemaValType::XmlSchemasNmtokens => XML_SCHEMA_TYPE_NMTOKENS_DEF.get(),
            XmlSchemaValType::XmlSchemasName => XML_SCHEMA_TYPE_NAME_DEF.get(),
            XmlSchemaValType::XmlSchemasQName => XML_SCHEMA_TYPE_QNAME_DEF.get(),
            XmlSchemaValType::XmlSchemasNCName => XML_SCHEMA_TYPE_NCNAME_DEF.get(),
            XmlSchemaValType::XmlSchemasID => XML_SCHEMA_TYPE_ID_DEF.get(),
            XmlSchemaValType::XmlSchemasIDREF => XML_SCHEMA_TYPE_IDREF_DEF.get(),
            XmlSchemaValType::XmlSchemasIDREFS => XML_SCHEMA_TYPE_IDREFS_DEF.get(),
            XmlSchemaValType::XmlSchemasEntity => XML_SCHEMA_TYPE_ENTITY_DEF.get(),
            XmlSchemaValType::XmlSchemasEntities => XML_SCHEMA_TYPE_ENTITIES_DEF.get(),
            XmlSchemaValType::XmlSchemasNotation => XML_SCHEMA_TYPE_NOTATION_DEF.get(),
            XmlSchemaValType::XmlSchemasAnyURI => XML_SCHEMA_TYPE_ANY_URIDEF.get(),
            XmlSchemaValType::XmlSchemasInteger => XML_SCHEMA_TYPE_INTEGER_DEF.get(),
            XmlSchemaValType::XmlSchemasNPInteger => XML_SCHEMA_TYPE_NON_POSITIVE_INTEGER_DEF.get(),
            XmlSchemaValType::XmlSchemasNInteger => XML_SCHEMA_TYPE_NEGATIVE_INTEGER_DEF.get(),
            XmlSchemaValType::XmlSchemasNNInteger => XML_SCHEMA_TYPE_NON_NEGATIVE_INTEGER_DEF.get(),
            XmlSchemaValType::XmlSchemasPInteger => XML_SCHEMA_TYPE_POSITIVE_INTEGER_DEF.get(),
            XmlSchemaValType::XmlSchemasInt => XML_SCHEMA_TYPE_INT_DEF.get(),
            XmlSchemaValType::XmlSchemasUInt => XML_SCHEMA_TYPE_UNSIGNED_INT_DEF.get(),
            XmlSchemaValType::XmlSchemasLong => XML_SCHEMA_TYPE_LONG_DEF.get(),
            XmlSchemaValType::XmlSchemasULong => XML_SCHEMA_TYPE_UNSIGNED_LONG_DEF.get(),
            XmlSchemaValType::XmlSchemasShort => XML_SCHEMA_TYPE_SHORT_DEF.get(),
            XmlSchemaValType::XmlSchemasUShort => XML_SCHEMA_TYPE_UNSIGNED_SHORT_DEF.get(),
            XmlSchemaValType::XmlSchemasByte => XML_SCHEMA_TYPE_BYTE_DEF.get(),
            XmlSchemaValType::XmlSchemasUByte => XML_SCHEMA_TYPE_UNSIGNED_BYTE_DEF.get(),
            XmlSchemaValType::XmlSchemasHexbinary => XML_SCHEMA_TYPE_HEX_BINARY_DEF.get(),
            XmlSchemaValType::XmlSchemasBase64binary => XML_SCHEMA_TYPE_BASE64_BINARY_DEF.get(),
            XmlSchemaValType::XmlSchemasAnytype => XML_SCHEMA_TYPE_ANY_TYPE_DEF.get(),
            _ => null_mut(),
        }
    }
}

/// Evaluates if a specific facet can be
/// used in conjunction with a type.
///
/// Returns 1 if the facet can be used with the given built-in type,
/// 0 otherwise and -1 in case the type is not a built-in type.
#[doc(alias = "xmlSchemaIsBuiltInTypeFacet")]
pub unsafe fn xml_schema_is_built_in_type_facet(typ: XmlSchemaTypePtr, facet_type: i32) -> i32 {
    unsafe {
        if typ.is_null() {
            return -1;
        }
        if (*typ).typ != XmlSchemaTypeType::XmlSchemaTypeBasic {
            return -1;
        }
        match XmlSchemaValType::try_from((*typ).built_in_type) {
            Ok(XmlSchemaValType::XmlSchemasBoolean) => {
                if facet_type == XmlSchemaTypeType::XmlSchemaFacetPattern as i32
                    || facet_type == XmlSchemaTypeType::XmlSchemaFacetWhitespace as i32
                {
                    return 1;
                } else {
                    return 0;
                }
            }
            Ok(XmlSchemaValType::XmlSchemasString)
            | Ok(XmlSchemaValType::XmlSchemasNotation)
            | Ok(XmlSchemaValType::XmlSchemasQName)
            | Ok(XmlSchemaValType::XmlSchemasAnyURI)
            | Ok(XmlSchemaValType::XmlSchemasBase64binary)
            | Ok(XmlSchemaValType::XmlSchemasHexbinary) => {
                if facet_type == XmlSchemaTypeType::XmlSchemaFacetLength as i32
                    || facet_type == XmlSchemaTypeType::XmlSchemaFacetMinLength as i32
                    || facet_type == XmlSchemaTypeType::XmlSchemaFacetMaxLength as i32
                    || facet_type == XmlSchemaTypeType::XmlSchemaFacetPattern as i32
                    || facet_type == XmlSchemaTypeType::XmlSchemaFacetEnumeration as i32
                    || facet_type == XmlSchemaTypeType::XmlSchemaFacetWhitespace as i32
                {
                    return 1;
                } else {
                    return 0;
                }
            }
            Ok(XmlSchemaValType::XmlSchemasDecimal) => {
                if facet_type == XmlSchemaTypeType::XmlSchemaFacetTotalDigits as i32
                    || facet_type == XmlSchemaTypeType::XmlSchemaFacetFractionDigits as i32
                    || facet_type == XmlSchemaTypeType::XmlSchemaFacetPattern as i32
                    || facet_type == XmlSchemaTypeType::XmlSchemaFacetWhitespace as i32
                    || facet_type == XmlSchemaTypeType::XmlSchemaFacetEnumeration as i32
                    || facet_type == XmlSchemaTypeType::XmlSchemaFacetMaxInclusive as i32
                    || facet_type == XmlSchemaTypeType::XmlSchemaFacetMaxExclusive as i32
                    || facet_type == XmlSchemaTypeType::XmlSchemaFacetMinInclusive as i32
                    || facet_type == XmlSchemaTypeType::XmlSchemaFacetMinExclusive as i32
                {
                    return 1;
                } else {
                    return 0;
                }
            }
            Ok(XmlSchemaValType::XmlSchemasTime)
            | Ok(XmlSchemaValType::XmlSchemasGDay)
            | Ok(XmlSchemaValType::XmlSchemasGMonth)
            | Ok(XmlSchemaValType::XmlSchemasGMonthDay)
            | Ok(XmlSchemaValType::XmlSchemasGYear)
            | Ok(XmlSchemaValType::XmlSchemasGYearMonth)
            | Ok(XmlSchemaValType::XmlSchemasDate)
            | Ok(XmlSchemaValType::XmlSchemasDatetime)
            | Ok(XmlSchemaValType::XmlSchemasDuration)
            | Ok(XmlSchemaValType::XmlSchemasFloat)
            | Ok(XmlSchemaValType::XmlSchemasDouble) => {
                if facet_type == XmlSchemaTypeType::XmlSchemaFacetPattern as i32
                    || facet_type == XmlSchemaTypeType::XmlSchemaFacetEnumeration as i32
                    || facet_type == XmlSchemaTypeType::XmlSchemaFacetWhitespace as i32
                    || facet_type == XmlSchemaTypeType::XmlSchemaFacetMaxInclusive as i32
                    || facet_type == XmlSchemaTypeType::XmlSchemaFacetMaxExclusive as i32
                    || facet_type == XmlSchemaTypeType::XmlSchemaFacetMinInclusive as i32
                    || facet_type == XmlSchemaTypeType::XmlSchemaFacetMinExclusive as i32
                {
                    return 1;
                } else {
                    return 0;
                }
            }
            _ => {}
        }
        0
    }
}

/// Extract the value of a facet
///
/// Returns the value as a long
#[doc(alias = "xmlSchemaGetFacetValueAsULong")]
pub unsafe fn xml_schema_get_facet_value_as_ulong(facet: XmlSchemaFacetPtr) -> u64 {
    unsafe {
        // TODO: Check if this is a decimal.
        if facet.is_null() || (*facet).val.is_null() {
            return 0;
        }
        (*(*facet).val).value.decimal.lo
    }
}

/// Checka a value against a "length", "minLength" and "maxLength"
/// facet; sets @length to the computed length of @value.
///
/// Returns 0 if the value is valid, a positive error code
/// otherwise and -1 in case of an internal or API error.
#[doc(alias = "xmlSchemaValidateLengthFacet")]
unsafe fn xml_schema_validate_length_facet_internal(
    facet: XmlSchemaFacetPtr,
    val_type: XmlSchemaValType,
    value: *const XmlChar,
    val: XmlSchemaValPtr,
    length: *mut u64,
    ws: XmlSchemaWhitespaceValueType,
) -> i32 {
    unsafe {
        let mut len: u32 = 0;

        if length.is_null() || facet.is_null() {
            return -1;
        }
        *length = 0;
        if !matches!(
            (*facet).typ,
            XmlSchemaTypeType::XmlSchemaFacetLength
                | XmlSchemaTypeType::XmlSchemaFacetMaxLength
                | XmlSchemaTypeType::XmlSchemaFacetMinLength
        ) {
            return -1;
        }

        // TODO: length, maxLength and minLength must be of type
        // nonNegativeInteger only. Check if decimal is used somehow.
        if (*facet).val.is_null()
            || !matches!(
                (*(*facet).val).typ,
                XmlSchemaValType::XmlSchemasDecimal | XmlSchemaValType::XmlSchemasNNInteger
            )
            || (*(*facet).val).value.decimal.frac != 0
        {
            return -1;
        }
        if !val.is_null() && (*val).typ == XmlSchemaValType::XmlSchemasHexbinary {
            len = (*val).value.hex.total;
        } else if !val.is_null() && (*val).typ == XmlSchemaValType::XmlSchemasBase64binary {
            len = (*val).value.base64.total;
        } else {
            match val_type {
            XmlSchemaValType::XmlSchemasString | XmlSchemaValType::XmlSchemasNormString => {
                if ws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceUnknown {
                    // This is to ensure API compatibility with the old
                    // xmlSchemaValidateLengthFacet(). Anyway, this was and
                    // is not the correct handling.
                    // TODO: Get rid of this case somehow.
                    if val_type == XmlSchemaValType::XmlSchemasString {
                        len = xml_utf8_strlen(value) as _;
                    } else {
                        len = xml_schema_norm_len(value) as _;
                    }
                } else if !value.is_null() {
                    if ws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse {
                        len = xml_schema_norm_len(value) as _;
                    } else {
                        // Should be OK for "preserve" as well.
                        len = xml_utf8_strlen(value) as _;
                    }
                }
            }
            XmlSchemaValType::XmlSchemasIDREF
            | XmlSchemaValType::XmlSchemasToken
            | XmlSchemaValType::XmlSchemasLanguage
            | XmlSchemaValType::XmlSchemasNmtoken
            | XmlSchemaValType::XmlSchemasName
            | XmlSchemaValType::XmlSchemasNCName
            | XmlSchemaValType::XmlSchemasID
            // FIXME: What exactly to do with anyURI?
            | XmlSchemaValType::XmlSchemasAnyURI => {
                if !value.is_null() {
                    len = xml_schema_norm_len(value) as _;
                }
            }
            XmlSchemaValType::XmlSchemasQName | XmlSchemaValType::XmlSchemasNotation => {
                // For QName and NOTATION, those facets are
                // deprecated and should be ignored.
                return 0;
            }
            _ => {
                // TODO
                todo!()
            }
        }
        }
        *length = len as u64;
        // TODO: Return the whole expected value, i.e. "lo", "mi" and "hi".
        if (*facet).typ == XmlSchemaTypeType::XmlSchemaFacetLength {
            if len as u64 != (*(*facet).val).value.decimal.lo {
                return XmlParserErrors::XmlSchemavCvcLengthValid as i32;
            }
        } else if (*facet).typ == XmlSchemaTypeType::XmlSchemaFacetMinLength {
            if (len as u64) < (*(*facet).val).value.decimal.lo {
                return XmlParserErrors::XmlSchemavCvcMinLengthValid as i32;
            }
        } else if len as u64 > (*(*facet).val).value.decimal.lo {
            return XmlParserErrors::XmlSchemavCvcMaxLengthValid as i32;
        }

        0
    }
}

/// Checka a value against a "length", "minLength" and "maxLength"
/// facet; sets @length to the computed length of @value.
///
/// Returns 0 if the value is valid, a positive error code
/// otherwise and -1 in case of an internal or API error.
#[doc(alias = "xmlSchemaValidateLengthFacet")]
pub unsafe fn xml_schema_validate_length_facet(
    typ: XmlSchemaTypePtr,
    facet: XmlSchemaFacetPtr,
    value: *const XmlChar,
    val: XmlSchemaValPtr,
    length: *mut u64,
) -> i32 {
    unsafe {
        if typ.is_null() {
            return -1;
        }
        xml_schema_validate_length_facet_internal(
            facet,
            (*typ).built_in_type.try_into().unwrap(),
            value,
            val,
            length,
            XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceUnknown,
        )
    }
}

/// Checka a value against a "length", "minLength" and "maxLength"
/// facet; sets @length to the computed length of @value.
///
/// Returns 0 if the value is valid, a positive error code
/// otherwise and -1 in case of an internal or API error.
#[doc(alias = "xmlSchemaValidateLengthFacetWhtsp")]
pub unsafe fn xml_schema_validate_length_facet_whtsp(
    facet: XmlSchemaFacetPtr,
    val_type: XmlSchemaValType,
    value: *const XmlChar,
    val: XmlSchemaValPtr,
    length: *mut u64,
    ws: XmlSchemaWhitespaceValueType,
) -> i32 {
    unsafe { xml_schema_validate_length_facet_internal(facet, val_type, value, val, length, ws) }
}

/// Check that a value conforms to the lexical space of the predefined type.
/// if true a value is computed and returned in @val.
/// This one does apply any normalization to the value.
///
/// Returns 0 if this validates, a positive error code number otherwise
/// and -1 in case of internal or API error.
#[doc(alias = "xmlSchemaValPredefTypeNodeNoNorm")]
pub unsafe fn xml_schema_val_predef_type_node_no_norm(
    typ: XmlSchemaTypePtr,
    value: *const XmlChar,
    val: *mut XmlSchemaValPtr,
    node: Option<XmlGenericNodePtr>,
) -> i32 {
    unsafe {
        xml_schema_val_atomic_type(
            typ,
            value,
            val,
            node,
            1,
            XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceUnknown,
            1,
            0,
            1,
        )
    }
}

/// Get the canonical lexical representation of the value.
/// The caller has to FREE the returned retValue.
///
/// WARNING: Some value types are not supported yet, resulting
/// in a @retValue of "???".
///
/// TODO: XML Schema 1.0 does not define canonical representations
/// for: duration, gYearMonth, gYear, gMonthDay, gMonth, gDay,
/// anyURI, QName, NOTATION. This will be fixed in XML Schema 1.1.
///
///
/// Returns 0 if the value could be built, 1 if the value type is
/// not supported yet and -1 in case of API errors.
#[doc(alias = "xmlSchemaGetCanonValue")]
pub unsafe fn xml_schema_get_canon_value(
    val: XmlSchemaValPtr,
    ret_value: *mut *const XmlChar,
) -> i32 {
    unsafe {
        if ret_value.is_null() || val.is_null() {
            return -1;
        }
        *ret_value = null_mut();
        match (*val).typ {
            XmlSchemaValType::XmlSchemasString => {
                if (*val).value.str.is_null() {
                    *ret_value = xml_strdup(c"".as_ptr() as _);
                } else {
                    *ret_value = xml_strdup((*val).value.str);
                }
            }
            XmlSchemaValType::XmlSchemasNormString => {
                if (*val).value.str.is_null() {
                    *ret_value = xml_strdup(c"".as_ptr() as _);
                } else {
                    *ret_value = xml_schema_white_space_replace(
                        CStr::from_ptr((*val).value.str as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                    )
                    .map_or(null_mut(), |res| {
                        xml_strndup(res.as_ptr(), res.len() as i32)
                    });
                    if (*ret_value).is_null() {
                        *ret_value = xml_strdup((*val).value.str);
                    }
                }
            }
            XmlSchemaValType::XmlSchemasToken
            | XmlSchemaValType::XmlSchemasLanguage
            | XmlSchemaValType::XmlSchemasNmtoken
            | XmlSchemaValType::XmlSchemasName
            | XmlSchemaValType::XmlSchemasNCName
            | XmlSchemaValType::XmlSchemasID
            | XmlSchemaValType::XmlSchemasIDREF
            | XmlSchemaValType::XmlSchemasEntity
            | XmlSchemaValType::XmlSchemasNotation
            | XmlSchemaValType::XmlSchemasAnyURI => {
                if (*val).value.str.is_null() {
                    return -1;
                }
                if let Some(res) = xml_schema_collapse_string(
                    CStr::from_ptr((*val).value.str as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                ) {
                    *ret_value = xml_strndup(res.as_ptr(), res.len() as i32);
                }
                if (*ret_value).is_null() {
                    *ret_value = xml_strdup((*val).value.str);
                }
            }
            XmlSchemaValType::XmlSchemasQName => {
                // TODO: Unclear in XML Schema 1.0.
                if (*val).value.qname.uri.is_null() {
                    *ret_value = xml_strdup((*val).value.qname.name);
                    return 0;
                } else {
                    *ret_value = xml_strdup(c"{".as_ptr() as _);
                    *ret_value = xml_strcat(*ret_value as _, (*val).value.qname.uri);
                    *ret_value = xml_strcat(*ret_value as _, c"}".as_ptr() as _);
                    *ret_value = xml_strcat(*ret_value as _, (*val).value.qname.uri);
                }
            }
            XmlSchemaValType::XmlSchemasDecimal => {
                // TODO: Lookout for a more simple implementation.
                if (*val).value.decimal.total == 1 && (*val).value.decimal.lo == 0 {
                    *ret_value = xml_strdup(c"0.0".as_ptr() as _);
                } else {
                    let mut bufsize: i32;
                    let dec: XmlSchemaValDecimal = (*val).value.decimal;
                    let mut offs: *mut c_char;

                    // Add room for the decimal point as well.
                    bufsize = dec.total as i32 + 2;
                    if dec.sign != 0 {
                        bufsize += 1;
                    }
                    // Add room for leading/trailing zero.
                    if dec.frac == 0 || dec.frac == dec.total {
                        bufsize += 1;
                    }
                    let buf: *mut c_char = xml_malloc(bufsize as _) as _;
                    if buf.is_null() {
                        return -1;
                    }
                    offs = buf;
                    if dec.sign != 0 {
                        *offs = b'-' as _;
                        offs = offs.add(1);
                    }
                    if dec.frac == dec.total {
                        *offs = b'0' as _;
                        offs = offs.add(1);
                        *offs = b'.' as _;
                        offs = offs.add(1);
                    }
                    if dec.hi != 0 {
                        snprintf(
                            offs,
                            bufsize as usize - offs.offset_from(buf) as usize,
                            c"%lu%lu%lu".as_ptr() as _,
                            dec.hi,
                            dec.mi,
                            dec.lo,
                        );
                    } else if dec.mi != 0 {
                        snprintf(
                            offs,
                            bufsize as usize - offs.offset_from(buf) as usize,
                            c"%lu%lu".as_ptr() as _,
                            dec.mi,
                            dec.lo,
                        );
                    } else {
                        snprintf(
                            offs,
                            bufsize as usize - offs.offset_from(buf) as usize,
                            c"%lu".as_ptr() as _,
                            dec.lo,
                        );
                    }

                    if dec.frac != 0 {
                        if dec.frac != dec.total {
                            let diff: i32 = (dec.total - dec.frac) as i32;
                            // Insert the decimal point.
                            memmove(
                                offs.add(diff as usize + 1) as _,
                                offs.add(diff as usize) as _,
                                dec.frac as usize + 1,
                            );
                            *offs.add(diff as usize) = b'.' as _;
                        } else {
                            let mut i: u32 = 0;
                            // Insert missing zeroes behind the decimal point.
                            while *offs.add(i as usize) != 0 {
                                i += 1;
                            }
                            if i < dec.total {
                                memmove(
                                    offs.add(dec.total as usize - i as usize) as _,
                                    offs as _,
                                    i as usize + 1,
                                );
                                memset(offs as _, b'0' as _, dec.total as usize - i as usize);
                            }
                        }
                    } else {
                        // Append decimal point and zero.
                        offs = buf.add(bufsize as usize - 1);
                        *offs = 0;
                        offs = offs.sub(1);
                        *offs = b'0' as _;
                        offs = offs.sub(1);
                        *offs = b'.' as _;
                        // offs = offs.sub(1);
                    }
                    *ret_value = buf as _;
                }
            }
            XmlSchemaValType::XmlSchemasInteger
            | XmlSchemaValType::XmlSchemasPInteger
            | XmlSchemaValType::XmlSchemasNPInteger
            | XmlSchemaValType::XmlSchemasNInteger
            | XmlSchemaValType::XmlSchemasNNInteger
            | XmlSchemaValType::XmlSchemasLong
            | XmlSchemaValType::XmlSchemasByte
            | XmlSchemaValType::XmlSchemasShort
            | XmlSchemaValType::XmlSchemasInt
            | XmlSchemaValType::XmlSchemasUInt
            | XmlSchemaValType::XmlSchemasULong
            | XmlSchemaValType::XmlSchemasUShort
            | XmlSchemaValType::XmlSchemasUByte => {
                if (*val).value.decimal.total == 1 && (*val).value.decimal.lo == 0 {
                    *ret_value = xml_strdup(c"0".as_ptr() as _);
                } else {
                    let dec: XmlSchemaValDecimal = (*val).value.decimal;
                    let mut bufsize: i32 = dec.total as i32 + 1;

                    // Add room for the decimal point as well.
                    if dec.sign != 0 {
                        bufsize += 1;
                    }
                    *ret_value = xml_malloc(bufsize as usize) as _;
                    if (*ret_value).is_null() {
                        return -1;
                    }
                    if dec.hi != 0 {
                        if dec.sign != 0 {
                            snprintf(
                                *ret_value as _,
                                bufsize as _,
                                c"-%lu%lu%lu".as_ptr() as _,
                                dec.hi,
                                dec.mi,
                                dec.lo,
                            );
                        } else {
                            snprintf(
                                *ret_value as _,
                                bufsize as _,
                                c"%lu%lu%lu".as_ptr() as _,
                                dec.hi,
                                dec.mi,
                                dec.lo,
                            );
                        }
                    } else if dec.mi != 0 {
                        if dec.sign != 0 {
                            snprintf(
                                *ret_value as _,
                                bufsize as _,
                                c"-%lu%lu".as_ptr() as _,
                                dec.mi,
                                dec.lo,
                            );
                        } else {
                            snprintf(
                                *ret_value as _,
                                bufsize as _,
                                c"%lu%lu".as_ptr() as _,
                                dec.mi,
                                dec.lo,
                            );
                        }
                    } else if dec.sign != 0 {
                        snprintf(*ret_value as _, bufsize as _, c"-%lu".as_ptr() as _, dec.lo);
                    } else {
                        snprintf(*ret_value as _, bufsize as _, c"%lu".as_ptr() as _, dec.lo);
                    }
                }
            }
            XmlSchemaValType::XmlSchemasBoolean => {
                if (*val).value.b != 0 {
                    *ret_value = xml_strdup(c"true".as_ptr() as _);
                } else {
                    *ret_value = xml_strdup(c"false".as_ptr() as _);
                }
            }
            XmlSchemaValType::XmlSchemasDuration => {
                let mut buf: [c_char; 100] = [0; 100];
                let mut hour: u64 = 0;
                let mut min: u64 = 0;
                let mut sec: f64 = 0.;
                let mut left: f64;

                // TODO: Unclear in XML Schema 1.0
                // TODO: This results in a normalized output of the value
                // - which is NOT conformant to the spec -
                // since the exact values of each property are not
                // recoverable. Think about extending the structure to
                // provide a field for every property.
                let year: u64 = FQUOTIENT!((*val).value.dur.mon.abs(), 12) as u64;
                let mon: u64 = (*val).value.dur.mon.unsigned_abs() - 12 * year;

                let day: u64 = FQUOTIENT!((*val).value.dur.sec.abs(), 86400) as u64;
                left = (*val).value.dur.sec.abs() - (day * 86400) as f64;
                if left > 0. {
                    hour = FQUOTIENT!(left, 3600) as u64;
                    left -= (hour * 3600) as f64;
                    if left > 0. {
                        min = FQUOTIENT!(left, 60) as u64;
                        sec = left - (min * 60) as f64;
                    }
                }
                if (*val).value.dur.mon < 0 || (*val).value.dur.sec < 0. {
                    snprintf(
                        buf.as_mut_ptr() as _,
                        100,
                        c"P%luY%luM%luDT%luH%luM%.14gS".as_ptr() as _,
                        year,
                        mon,
                        day,
                        hour,
                        min,
                        sec,
                    );
                } else {
                    snprintf(
                        buf.as_mut_ptr() as _,
                        100,
                        c"-P%luY%luM%luDT%luH%luM%.14gS".as_ptr() as _,
                        year,
                        mon,
                        day,
                        hour,
                        min,
                        sec,
                    );
                }
                *ret_value = xml_strdup(buf.as_ptr() as _);
            }
            XmlSchemaValType::XmlSchemasGYear => {
                let mut buf: [c_char; 30] = [0; 30];
                // TODO: Unclear in XML Schema 1.0
                // TODO: What to do with the timezone?
                snprintf(
                    buf.as_mut_ptr() as _,
                    30,
                    c"%04ld".as_ptr() as _,
                    (*val).value.date.year,
                );
                *ret_value = xml_strdup(buf.as_ptr() as _);
            }
            XmlSchemaValType::XmlSchemasGMonth => {
                // TODO: Unclear in XML Schema 1.0
                // TODO: What to do with the timezone?
                *ret_value = xml_malloc(6) as _;
                if (*ret_value).is_null() {
                    return -1;
                }
                snprintf(
                    *ret_value as _,
                    6,
                    c"--%02u".as_ptr() as _,
                    (*val).value.date.mon,
                );
            }
            XmlSchemaValType::XmlSchemasGDay => {
                // TODO: Unclear in XML Schema 1.0
                // TODO: What to do with the timezone?
                *ret_value = xml_malloc(6) as _;
                if (*ret_value).is_null() {
                    return -1;
                }
                snprintf(
                    *ret_value as _,
                    6,
                    c"---%02u".as_ptr() as _,
                    (*val).value.date.day,
                );
            }
            XmlSchemaValType::XmlSchemasGMonthDay => {
                // TODO: Unclear in XML Schema 1.0
                // TODO: What to do with the timezone?
                *ret_value = xml_malloc(8) as _;
                if (*ret_value).is_null() {
                    return -1;
                }
                snprintf(
                    *ret_value as _,
                    8,
                    c"--%02u-%02u".as_ptr() as _,
                    (*val).value.date.mon,
                    (*val).value.date.day,
                );
            }
            XmlSchemaValType::XmlSchemasGYearMonth => {
                let mut buf: [c_char; 35] = [0; 35];
                // TODO: Unclear in XML Schema 1.0
                // TODO: What to do with the timezone?
                if (*val).value.date.year < 0 {
                    snprintf(
                        buf.as_mut_ptr() as _,
                        35,
                        c"-%04ld-%02u".as_ptr() as _,
                        (*val).value.date.year.abs(),
                        (*val).value.date.mon,
                    );
                } else {
                    snprintf(
                        buf.as_mut_ptr() as _,
                        35,
                        c"%04ld-%02u".as_ptr() as _,
                        (*val).value.date.year,
                        (*val).value.date.mon,
                    );
                }
                *ret_value = xml_strdup(buf.as_ptr() as _);
            }
            XmlSchemaValType::XmlSchemasTime => {
                let mut buf: [c_char; 30] = [0; 30];

                if (*val).value.date.tz_flag != 0 {
                    let norm: XmlSchemaValPtr = xml_schema_date_normalize(val, 0.);
                    if norm.is_null() {
                        return -1;
                    }
                    // TODO: Check if "%.14g" is portable.
                    snprintf(
                        buf.as_mut_ptr() as _,
                        30,
                        c"%02u:%02u:%02.14gZ".as_ptr() as _,
                        (*norm).value.date.hour,
                        (*norm).value.date.min,
                        (*norm).value.date.sec,
                    );
                    xml_schema_free_value(norm);
                } else {
                    snprintf(
                        buf.as_mut_ptr() as _,
                        30,
                        c"%02u:%02u:%02.14g".as_ptr() as _,
                        (*val).value.date.hour,
                        (*val).value.date.min,
                        (*val).value.date.sec,
                    );
                }
                *ret_value = xml_strdup(buf.as_ptr() as _);
            }
            XmlSchemaValType::XmlSchemasDate => {
                let mut buf: [c_char; 30] = [0; 30];

                if (*val).value.date.tz_flag != 0 {
                    let norm: XmlSchemaValPtr = xml_schema_date_normalize(val, 0.);
                    if norm.is_null() {
                        return -1;
                    }
                    // TODO: Append the canonical value of the
                    // recoverable timezone and not "Z".
                    snprintf(
                        buf.as_mut_ptr() as _,
                        30,
                        c"%04ld-%02u-%02uZ".as_ptr() as _,
                        (*norm).value.date.year,
                        (*norm).value.date.mon,
                        (*norm).value.date.day,
                    );
                    xml_schema_free_value(norm);
                } else {
                    snprintf(
                        buf.as_mut_ptr() as _,
                        30,
                        c"%04ld-%02u-%02u".as_ptr() as _,
                        (*val).value.date.year,
                        (*val).value.date.mon,
                        (*val).value.date.day,
                    );
                }
                *ret_value = xml_strdup(buf.as_ptr() as _) as _;
            }
            XmlSchemaValType::XmlSchemasDatetime => {
                let mut buf: [c_char; 50] = [0; 50];

                if (*val).value.date.tz_flag != 0 {
                    let norm: XmlSchemaValPtr = xml_schema_date_normalize(val, 0.);
                    if norm.is_null() {
                        return -1;
                    }
                    // TODO: Check if "%.14g" is portable.
                    snprintf(
                        buf.as_mut_ptr() as _,
                        50,
                        c"%04ld-%02u-%02uT%02u:%02u:%02.14gZ".as_ptr() as _,
                        (*norm).value.date.year,
                        (*norm).value.date.mon,
                        (*norm).value.date.day,
                        (*norm).value.date.hour,
                        (*norm).value.date.min,
                        (*norm).value.date.sec,
                    );
                    xml_schema_free_value(norm);
                } else {
                    snprintf(
                        buf.as_mut_ptr() as _,
                        50,
                        c"%04ld-%02u-%02uT%02u:%02u:%02.14g".as_ptr() as _,
                        (*val).value.date.year,
                        (*val).value.date.mon,
                        (*val).value.date.day,
                        (*val).value.date.hour,
                        (*val).value.date.min,
                        (*val).value.date.sec,
                    );
                }
                *ret_value = xml_strdup(buf.as_ptr() as _) as _;
            }
            XmlSchemaValType::XmlSchemasHexbinary => {
                *ret_value = xml_strdup((*val).value.hex.str);
            }
            XmlSchemaValType::XmlSchemasBase64binary => {
                // TODO: Is the following spec piece implemented?:
                // SPEC: "Note: For some values the canonical form defined
                // above does not conform to [RFC 2045], which requires breaking
                // with linefeeds at appropriate intervals."
                *ret_value = xml_strdup((*val).value.base64.str);
            }
            XmlSchemaValType::XmlSchemasFloat => {
                let mut buf: [c_char; 30] = [0; 30];
                // |m| < 16777216, -149 <= e <= 104.
                // TODO: Handle, NaN, INF, -INF. The format is not
                // yet conformant. The c type float does not cover
                // the whole range.
                snprintf(
                    buf.as_mut_ptr() as _,
                    30,
                    c"%01.14e".as_ptr() as _,
                    (*val).value.f as f64,
                );
                *ret_value = xml_strdup(buf.as_ptr() as _);
            }
            XmlSchemaValType::XmlSchemasDouble => {
                let mut buf: [c_char; 40] = [0; 40];
                // |m| < 9007199254740992, -1075 <= e <= 970
                // TODO: Handle, NaN, INF, -INF. The format is not
                // yet conformant. The c type float does not cover
                // the whole range.
                snprintf(
                    buf.as_mut_ptr() as _,
                    40,
                    c"%01.14e".as_ptr() as _,
                    (*val).value.d,
                );
                *ret_value = xml_strdup(buf.as_ptr() as _);
            }
            _ => {
                *ret_value = xml_strdup(c"???".as_ptr() as _);
                return 1;
            }
        }
        if (*ret_value).is_null() {
            return -1;
        }
        0
    }
}

/// Get the canonical representation of the value.
/// The caller has to free the returned @retValue.
///
/// Returns 0 if the value could be built, 1 if the value type is
/// not supported yet and -1 in case of API errors.
#[doc(alias = "xmlSchemaGetCanonValueWhtsp")]
pub unsafe fn xml_schema_get_canon_value_whtsp(
    val: XmlSchemaValPtr,
    ret_value: *mut *const XmlChar,
    ws: XmlSchemaWhitespaceValueType,
) -> i32 {
    unsafe {
        if ret_value.is_null() || val.is_null() {
            return -1;
        }
        if ws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceUnknown
            || ws as i32 > XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse as i32
        {
            return -1;
        }

        *ret_value = null_mut();
        match (*val).typ {
            XmlSchemaValType::XmlSchemasString => {
                if (*val).value.str.is_null() {
                    *ret_value = xml_strdup(c"".as_ptr() as _);
                } else if ws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse {
                    if let Some(res) = xml_schema_collapse_string(
                        CStr::from_ptr((*val).value.str as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                    ) {
                        *ret_value = xml_strndup(res.as_ptr(), res.len() as i32);
                    }
                } else if ws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceReplace {
                    *ret_value = xml_schema_white_space_replace(
                        CStr::from_ptr((*val).value.str as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                    )
                    .map_or(null_mut(), |res| {
                        xml_strndup(res.as_ptr(), res.len() as i32)
                    });
                }
                if (*ret_value).is_null() {
                    *ret_value = xml_strdup((*val).value.str);
                }
            }
            XmlSchemaValType::XmlSchemasNormString => {
                if (*val).value.str.is_null() {
                    *ret_value = xml_strdup(c"".as_ptr() as _);
                } else {
                    if ws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse {
                        if let Some(res) = xml_schema_collapse_string(
                            CStr::from_ptr((*val).value.str as *const i8)
                                .to_string_lossy()
                                .as_ref(),
                        ) {
                            *ret_value = xml_strndup(res.as_ptr(), res.len() as i32);
                        }
                    } else {
                        *ret_value = xml_schema_white_space_replace(
                            CStr::from_ptr((*val).value.str as *const i8)
                                .to_string_lossy()
                                .as_ref(),
                        )
                        .map_or(null_mut(), |res| {
                            xml_strndup(res.as_ptr(), res.len() as i32)
                        });
                    }
                    if (*ret_value).is_null() {
                        *ret_value = xml_strdup((*val).value.str);
                    }
                }
            }
            _ => {
                return xml_schema_get_canon_value(val, ret_value);
            }
        }
        0
    }
}

/// Appends a next sibling to a list of computed values.
///
/// Returns 0 if succeeded and -1 on API errors.
#[doc(alias = "xmlSchemaValueAppend")]
pub unsafe fn xml_schema_value_append(prev: XmlSchemaValPtr, cur: XmlSchemaValPtr) -> i32 {
    unsafe {
        if prev.is_null() || cur.is_null() {
            return -1;
        }
        (*prev).next = cur;
        0
    }
}

/// Accessor for the next sibling of a list of computed values.
///
/// Returns the next value or NULL if there was none, or on API errors.
#[doc(alias = "xmlSchemaValueGetNext")]
pub unsafe fn xml_schema_value_get_next(cur: XmlSchemaValPtr) -> XmlSchemaValPtr {
    unsafe {
        if cur.is_null() {
            return null_mut();
        }
        (*cur).next
    }
}

/// Accessor for the string value of a computed value.
///
/// Returns the string value or NULL if there was none, or on API errors.
#[doc(alias = "xmlSchemaValueGetAsString")]
pub unsafe fn xml_schema_value_get_as_string(val: XmlSchemaValPtr) -> *const XmlChar {
    unsafe {
        if val.is_null() {
            return null_mut();
        }
        match (*val).typ {
            XmlSchemaValType::XmlSchemasString
            | XmlSchemaValType::XmlSchemasNormString
            | XmlSchemaValType::XmlSchemasAnySimpletype
            | XmlSchemaValType::XmlSchemasToken
            | XmlSchemaValType::XmlSchemasLanguage
            | XmlSchemaValType::XmlSchemasNmtoken
            | XmlSchemaValType::XmlSchemasName
            | XmlSchemaValType::XmlSchemasNCName
            | XmlSchemaValType::XmlSchemasID
            | XmlSchemaValType::XmlSchemasIDREF
            | XmlSchemaValType::XmlSchemasEntity
            | XmlSchemaValType::XmlSchemasAnyURI => {
                return (*val).value.str;
            }
            _ => {}
        }
        null_mut()
    }
}

/// Accessor for the boolean value of a computed value.
///
/// Returns 1 if true and 0 if false, or in case of an error. Hmm.
#[doc(alias = "xmlSchemaValueGetAsBoolean")]
pub unsafe fn xml_schema_value_get_as_boolean(val: XmlSchemaValPtr) -> i32 {
    unsafe {
        if val.is_null() || (*val).typ != XmlSchemaValType::XmlSchemasBoolean {
            return 0;
        }
        (*val).value.b
    }
}

/// Allocate a new simple type value. The type can be
/// of XmlSchemaValType::XML_SCHEMAS_STRING.
/// WARNING: This one is intended to be expanded for other
/// string based types. We need this for anySimpleType as well.
/// The given value is consumed and freed with the struct.
///
/// Returns a pointer to the new value or NULL in case of error
#[doc(alias = "xmlSchemaNewStringValue")]
pub unsafe fn xml_schema_new_string_value(
    typ: XmlSchemaValType,
    value: *const XmlChar,
) -> XmlSchemaValPtr {
    unsafe {
        if typ != XmlSchemaValType::XmlSchemasString {
            return null_mut();
        }
        let val: XmlSchemaValPtr = xml_malloc(size_of::<XmlSchemaVal>()) as XmlSchemaValPtr;
        if val.is_null() {
            return null_mut();
        }
        memset(val as _, 0, size_of::<XmlSchemaVal>());
        (*val).typ = typ;
        (*val).value.str = value as *mut XmlChar;
        val
    }
}

/// Allocate a new NOTATION value.
/// The given values are consumed and freed with the struct.
///
/// Returns a pointer to the new value or NULL in case of error
#[doc(alias = "xmlSchemaNewNOTATIONValue")]
pub unsafe fn xml_schema_new_notation_value(
    name: *const XmlChar,
    ns: *const XmlChar,
) -> XmlSchemaValPtr {
    unsafe {
        let val: XmlSchemaValPtr = xml_schema_new_value(XmlSchemaValType::XmlSchemasNotation);
        if val.is_null() {
            return null_mut();
        }

        (*val).value.qname.name = name as _;
        if !ns.is_null() {
            (*val).value.qname.uri = ns as _;
        }
        val
    }
}

/// Allocate a new QName value.
/// The given values are consumed and freed with the struct.
///
/// Returns a pointer to the new value or NULL in case of an error.
#[doc(alias = "xmlSchemaNewQNameValue")]
pub unsafe fn xml_schema_new_qname_value(
    namespace_name: *const XmlChar,
    local_name: *const XmlChar,
) -> XmlSchemaValPtr {
    unsafe {
        let val: XmlSchemaValPtr = xml_schema_new_value(XmlSchemaValType::XmlSchemasQName);
        if val.is_null() {
            return null_mut();
        }

        (*val).value.qname.name = local_name as _;
        (*val).value.qname.uri = namespace_name as _;
        val
    }
}

/// Compare 2 values
///
/// Returns -1 if x < y, 0 if x == y, 1 if x > y, 2 if x <> y, and -2 in case of error
#[doc(alias = "xmlSchemaCompareValuesWhtsp")]
pub unsafe fn xml_schema_compare_values_whtsp(
    x: XmlSchemaValPtr,
    xws: XmlSchemaWhitespaceValueType,
    y: XmlSchemaValPtr,
    yws: XmlSchemaWhitespaceValueType,
) -> i32 {
    unsafe {
        if x.is_null() || y.is_null() {
            return -2;
        }
        xml_schema_compare_values_internal(
            (*x).typ,
            x,
            null_mut(),
            xws,
            (*y).typ,
            y,
            null_mut(),
            yws,
        )
    }
}

/// Copies the precomputed value. This duplicates any string within.
///
/// Returns the copy or NULL if a copy for a data-type is not implemented.
#[doc(alias = "xmlSchemaCopyValue")]
pub unsafe fn xml_schema_copy_value(mut val: XmlSchemaValPtr) -> XmlSchemaValPtr {
    unsafe {
        let mut ret: XmlSchemaValPtr = null_mut();
        let mut prev: XmlSchemaValPtr = null_mut();
        let mut cur: XmlSchemaValPtr;

        // Copy the string values.
        while !val.is_null() {
            match (*val).typ {
                XmlSchemaValType::XmlSchemasAnytype
                | XmlSchemaValType::XmlSchemasIDREFS
                | XmlSchemaValType::XmlSchemasEntities
                | XmlSchemaValType::XmlSchemasNmtokens => {
                    xml_schema_free_value(ret);
                    return null_mut();
                }
                XmlSchemaValType::XmlSchemasAnySimpletype
                | XmlSchemaValType::XmlSchemasString
                | XmlSchemaValType::XmlSchemasNormString
                | XmlSchemaValType::XmlSchemasToken
                | XmlSchemaValType::XmlSchemasLanguage
                | XmlSchemaValType::XmlSchemasName
                | XmlSchemaValType::XmlSchemasNCName
                | XmlSchemaValType::XmlSchemasID
                | XmlSchemaValType::XmlSchemasIDREF
                | XmlSchemaValType::XmlSchemasEntity
                | XmlSchemaValType::XmlSchemasNmtoken
                | XmlSchemaValType::XmlSchemasAnyURI => {
                    cur = xml_schema_dup_val(val);
                    if !(*val).value.str.is_null() {
                        (*cur).value.str = xml_strdup((*val).value.str);
                    }
                }
                XmlSchemaValType::XmlSchemasQName | XmlSchemaValType::XmlSchemasNotation => {
                    cur = xml_schema_dup_val(val);
                    if !(*val).value.qname.name.is_null() {
                        (*cur).value.qname.name = xml_strdup((*val).value.qname.name);
                    }
                    if !(*val).value.qname.uri.is_null() {
                        (*cur).value.qname.uri = xml_strdup((*val).value.qname.uri);
                    }
                }
                XmlSchemaValType::XmlSchemasHexbinary => {
                    cur = xml_schema_dup_val(val);
                    if !(*val).value.hex.str.is_null() {
                        (*cur).value.hex.str = xml_strdup((*val).value.hex.str);
                    }
                }
                XmlSchemaValType::XmlSchemasBase64binary => {
                    cur = xml_schema_dup_val(val);
                    if !(*val).value.base64.str.is_null() {
                        (*cur).value.base64.str = xml_strdup((*val).value.base64.str);
                    }
                }
                _ => {
                    cur = xml_schema_dup_val(val);
                }
            }
            if ret.is_null() {
                ret = cur;
            } else {
                (*prev).next = cur;
            }
            prev = cur;
            val = (*val).next;
        }
        ret
    }
}

/// Accessor for the type of a value
///
/// Returns the XmlSchemaValType of the value
#[doc(alias = "xmlSchemaGetValType")]
pub unsafe fn xml_schema_get_val_type(val: XmlSchemaValPtr) -> XmlSchemaValType {
    unsafe {
        if val.is_null() {
            return XmlSchemaValType::XmlSchemasUnknown;
        }
        (*val).typ
    }
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_schema_check_facet() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_facet in 0..GEN_NB_XML_SCHEMA_FACET_PTR {
                for n_type_decl in 0..GEN_NB_XML_SCHEMA_TYPE_PTR {
                    for n_pctxt in 0..GEN_NB_XML_SCHEMA_PARSER_CTXT_PTR {
                        for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            let mem_base = xml_mem_blocks();
                            let facet = gen_xml_schema_facet_ptr(n_facet, 0);
                            let type_decl = gen_xml_schema_type_ptr(n_type_decl, 1);
                            let pctxt = gen_xml_schema_parser_ctxt_ptr(n_pctxt, 2);
                            let name = gen_const_xml_char_ptr(n_name, 3);

                            let ret_val = xml_schema_check_facet(facet, type_decl, pctxt, name);
                            desret_int(ret_val);
                            des_xml_schema_facet_ptr(n_facet, facet, 0);
                            des_xml_schema_type_ptr(n_type_decl, type_decl, 1);
                            des_xml_schema_parser_ctxt_ptr(n_pctxt, pctxt, 2);
                            des_const_xml_char_ptr(n_name, name, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlSchemaCheckFacet",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlSchemaCheckFacet()"
                                );
                                eprint!(" {}", n_facet);
                                eprint!(" {}", n_type_decl);
                                eprint!(" {}", n_pctxt);
                                eprintln!(" {}", n_name);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_schema_cleanup_types() {
        #[cfg(feature = "schema")]
        unsafe {
            xml_schema_cleanup_types();
            reset_last_error();
        }
    }

    #[test]
    fn test_xml_schema_compare_values() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_x in 0..GEN_NB_XML_SCHEMA_VAL_PTR {
                for n_y in 0..GEN_NB_XML_SCHEMA_VAL_PTR {
                    let mem_base = xml_mem_blocks();
                    let x = gen_xml_schema_val_ptr(n_x, 0);
                    let y = gen_xml_schema_val_ptr(n_y, 1);

                    let ret_val = xml_schema_compare_values(x, y);
                    desret_int(ret_val);
                    des_xml_schema_val_ptr(n_x, x, 0);
                    des_xml_schema_val_ptr(n_y, y, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlSchemaCompareValues",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlSchemaCompareValues()"
                        );
                        eprint!(" {}", n_x);
                        eprintln!(" {}", n_y);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_schema_compare_values_whtsp() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_x in 0..GEN_NB_XML_SCHEMA_VAL_PTR {
                for n_xws in 0..GEN_NB_XML_SCHEMA_WHITESPACE_VALUE_TYPE {
                    for n_y in 0..GEN_NB_XML_SCHEMA_VAL_PTR {
                        for n_yws in 0..GEN_NB_XML_SCHEMA_WHITESPACE_VALUE_TYPE {
                            let mem_base = xml_mem_blocks();
                            let x = gen_xml_schema_val_ptr(n_x, 0);
                            let xws = gen_xml_schema_whitespace_value_type(n_xws, 1);
                            let y = gen_xml_schema_val_ptr(n_y, 2);
                            let yws = gen_xml_schema_whitespace_value_type(n_yws, 3);

                            let ret_val = xml_schema_compare_values_whtsp(x, xws, y, yws);
                            desret_int(ret_val);
                            des_xml_schema_val_ptr(n_x, x, 0);
                            des_xml_schema_whitespace_value_type(n_xws, xws, 1);
                            des_xml_schema_val_ptr(n_y, y, 2);
                            des_xml_schema_whitespace_value_type(n_yws, yws, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlSchemaCompareValuesWhtsp",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlSchemaCompareValuesWhtsp()"
                                );
                                eprint!(" {}", n_x);
                                eprint!(" {}", n_xws);
                                eprint!(" {}", n_y);
                                eprintln!(" {}", n_yws);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_schema_copy_value() {

        /* missing type support */
    }

    #[test]
    fn test_xml_schema_get_built_in_list_simple_type_item_type() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_type in 0..GEN_NB_XML_SCHEMA_TYPE_PTR {
                let mem_base = xml_mem_blocks();
                let typ = gen_xml_schema_type_ptr(n_type, 0);

                let ret_val = xml_schema_get_built_in_list_simple_type_item_type(typ);
                desret_xml_schema_type_ptr(ret_val);
                des_xml_schema_type_ptr(n_type, typ, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlSchemaGetBuiltInListSimpleTypeItemType",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlSchemaGetBuiltInListSimpleTypeItemType()"
                    );
                    eprintln!(" {}", n_type);
                }
            }
        }
    }

    #[test]
    fn test_xml_schema_get_built_in_type() {
        #[cfg(feature = "schema")]
        unsafe {
            for n_type in 0..GEN_NB_XML_SCHEMA_VAL_TYPE {
                let typ = gen_xml_schema_val_type(n_type, 0);

                let ret_val = xml_schema_get_built_in_type(typ);
                desret_xml_schema_type_ptr(ret_val);
                des_xml_schema_val_type(n_type, typ, 0);
                reset_last_error();
            }
        }
    }

    #[test]
    fn test_xml_schema_get_canon_value() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_XML_SCHEMA_VAL_PTR {
                for n_ret_value in 0..GEN_NB_CONST_XML_CHAR_PTR_PTR {
                    let mem_base = xml_mem_blocks();
                    let val = gen_xml_schema_val_ptr(n_val, 0);
                    let ret_value = gen_const_xml_char_ptr_ptr(n_ret_value, 1);

                    let ret_val = xml_schema_get_canon_value(val, ret_value as *mut *const XmlChar);
                    desret_int(ret_val);
                    des_xml_schema_val_ptr(n_val, val, 0);
                    des_const_xml_char_ptr_ptr(n_ret_value, ret_value as *mut *const XmlChar, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlSchemaGetCanonValue",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlSchemaGetCanonValue()"
                        );
                        eprint!(" {}", n_val);
                        eprintln!(" {}", n_ret_value);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_schema_get_canon_value_whtsp() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_XML_SCHEMA_VAL_PTR {
                for n_ret_value in 0..GEN_NB_CONST_XML_CHAR_PTR_PTR {
                    for n_ws in 0..GEN_NB_XML_SCHEMA_WHITESPACE_VALUE_TYPE {
                        let mem_base = xml_mem_blocks();
                        let val = gen_xml_schema_val_ptr(n_val, 0);
                        let ret_value = gen_const_xml_char_ptr_ptr(n_ret_value, 1);
                        let ws = gen_xml_schema_whitespace_value_type(n_ws, 2);

                        let ret_val = xml_schema_get_canon_value_whtsp(
                            val,
                            ret_value as *mut *const XmlChar,
                            ws,
                        );
                        desret_int(ret_val);
                        des_xml_schema_val_ptr(n_val, val, 0);
                        des_const_xml_char_ptr_ptr(
                            n_ret_value,
                            ret_value as *mut *const XmlChar,
                            1,
                        );
                        des_xml_schema_whitespace_value_type(n_ws, ws, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlSchemaGetCanonValueWhtsp",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlSchemaGetCanonValueWhtsp()"
                            );
                            eprint!(" {}", n_val);
                            eprint!(" {}", n_ret_value);
                            eprintln!(" {}", n_ws);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_schema_get_facet_value_as_ulong() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_facet in 0..GEN_NB_XML_SCHEMA_FACET_PTR {
                let mem_base = xml_mem_blocks();
                let facet = gen_xml_schema_facet_ptr(n_facet, 0);

                let ret_val = xml_schema_get_facet_value_as_ulong(facet);
                desret_unsigned_long(ret_val);
                des_xml_schema_facet_ptr(n_facet, facet, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlSchemaGetFacetValueAsULong",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlSchemaGetFacetValueAsULong()"
                    );
                    eprintln!(" {}", n_facet);
                }
            }
        }
    }

    #[test]
    fn test_xml_schema_get_val_type() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_XML_SCHEMA_VAL_PTR {
                let mem_base = xml_mem_blocks();
                let val = gen_xml_schema_val_ptr(n_val, 0);

                let ret_val = xml_schema_get_val_type(val);
                desret_xml_schema_val_type(ret_val);
                des_xml_schema_val_ptr(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlSchemaGetValType",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlSchemaGetValType()"
                    );
                    eprintln!(" {}", n_val);
                }
            }
        }
    }

    #[test]
    fn test_xml_schema_init_types() {
        #[cfg(feature = "schema")]
        unsafe {
            let ret_val = xml_schema_init_types();
            desret_int(ret_val);
            reset_last_error();
        }
    }

    #[test]
    fn test_xml_schema_is_built_in_type_facet() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_type in 0..GEN_NB_XML_SCHEMA_TYPE_PTR {
                for n_facet_type in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let typ = gen_xml_schema_type_ptr(n_type, 0);
                    let facet_type = gen_int(n_facet_type, 1);

                    let ret_val = xml_schema_is_built_in_type_facet(typ, facet_type);
                    desret_int(ret_val);
                    des_xml_schema_type_ptr(n_type, typ, 0);
                    des_int(n_facet_type, facet_type, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlSchemaIsBuiltInTypeFacet",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlSchemaIsBuiltInTypeFacet()"
                        );
                        eprint!(" {}", n_type);
                        eprintln!(" {}", n_facet_type);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_schema_validate_facet() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_base in 0..GEN_NB_XML_SCHEMA_TYPE_PTR {
                for n_facet in 0..GEN_NB_XML_SCHEMA_FACET_PTR {
                    for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_val in 0..GEN_NB_XML_SCHEMA_VAL_PTR {
                            let mem_base = xml_mem_blocks();
                            let base = gen_xml_schema_type_ptr(n_base, 0);
                            let facet = gen_xml_schema_facet_ptr(n_facet, 1);
                            let value = gen_const_xml_char_ptr(n_value, 2);
                            let val = gen_xml_schema_val_ptr(n_val, 3);

                            let ret_val = xml_schema_validate_facet(base, facet, value, val);
                            desret_int(ret_val);
                            des_xml_schema_type_ptr(n_base, base, 0);
                            des_xml_schema_facet_ptr(n_facet, facet, 1);
                            des_const_xml_char_ptr(n_value, value, 2);
                            des_xml_schema_val_ptr(n_val, val, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlSchemaValidateFacet",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlSchemaValidateFacet()"
                                );
                                eprint!(" {}", n_base);
                                eprint!(" {}", n_facet);
                                eprint!(" {}", n_value);
                                eprintln!(" {}", n_val);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_schema_validate_facet_whtsp() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_facet in 0..GEN_NB_XML_SCHEMA_FACET_PTR {
                for n_fws in 0..GEN_NB_XML_SCHEMA_WHITESPACE_VALUE_TYPE {
                    for n_val_type in 0..GEN_NB_XML_SCHEMA_VAL_TYPE {
                        for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            for n_val in 0..GEN_NB_XML_SCHEMA_VAL_PTR {
                                for n_ws in 0..GEN_NB_XML_SCHEMA_WHITESPACE_VALUE_TYPE {
                                    let mem_base = xml_mem_blocks();
                                    let facet = gen_xml_schema_facet_ptr(n_facet, 0);
                                    let fws = gen_xml_schema_whitespace_value_type(n_fws, 1);
                                    let val_type = gen_xml_schema_val_type(n_val_type, 2);
                                    let value = gen_const_xml_char_ptr(n_value, 3);
                                    let val = gen_xml_schema_val_ptr(n_val, 4);
                                    let ws = gen_xml_schema_whitespace_value_type(n_ws, 5);

                                    let ret_val = xml_schema_validate_facet_whtsp(
                                        facet, fws, val_type, value, val, ws,
                                    );
                                    desret_int(ret_val);
                                    des_xml_schema_facet_ptr(n_facet, facet, 0);
                                    des_xml_schema_whitespace_value_type(n_fws, fws, 1);
                                    des_xml_schema_val_type(n_val_type, val_type, 2);
                                    des_const_xml_char_ptr(n_value, value, 3);
                                    des_xml_schema_val_ptr(n_val, val, 4);
                                    des_xml_schema_whitespace_value_type(n_ws, ws, 5);
                                    reset_last_error();
                                    if mem_base != xml_mem_blocks() {
                                        leaks += 1;
                                        eprint!(
                                            "Leak of {} blocks found in xmlSchemaValidateFacetWhtsp",
                                            xml_mem_blocks() - mem_base
                                        );
                                        assert!(
                                            leaks == 0,
                                            "{leaks} Leaks are found in xmlSchemaValidateFacetWhtsp()"
                                        );
                                        eprint!(" {}", n_facet);
                                        eprint!(" {}", n_fws);
                                        eprint!(" {}", n_val_type);
                                        eprint!(" {}", n_value);
                                        eprint!(" {}", n_val);
                                        eprintln!(" {}", n_ws);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_schema_validate_length_facet() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_type in 0..GEN_NB_XML_SCHEMA_TYPE_PTR {
                for n_facet in 0..GEN_NB_XML_SCHEMA_FACET_PTR {
                    for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_val in 0..GEN_NB_XML_SCHEMA_VAL_PTR {
                            for n_length in 0..GEN_NB_UNSIGNED_LONG_PTR {
                                let mem_base = xml_mem_blocks();
                                let typ = gen_xml_schema_type_ptr(n_type, 0);
                                let facet = gen_xml_schema_facet_ptr(n_facet, 1);
                                let value = gen_const_xml_char_ptr(n_value, 2);
                                let val = gen_xml_schema_val_ptr(n_val, 3);
                                let length = gen_unsigned_long_ptr(n_length, 4);

                                let ret_val = xml_schema_validate_length_facet(
                                    typ, facet, value, val, length,
                                );
                                desret_int(ret_val);
                                des_xml_schema_type_ptr(n_type, typ, 0);
                                des_xml_schema_facet_ptr(n_facet, facet, 1);
                                des_const_xml_char_ptr(n_value, value, 2);
                                des_xml_schema_val_ptr(n_val, val, 3);
                                des_unsigned_long_ptr(n_length, length, 4);
                                reset_last_error();
                                if mem_base != xml_mem_blocks() {
                                    leaks += 1;
                                    eprint!(
                                        "Leak of {} blocks found in xmlSchemaValidateLengthFacet",
                                        xml_mem_blocks() - mem_base
                                    );
                                    assert!(
                                        leaks == 0,
                                        "{leaks} Leaks are found in xmlSchemaValidateLengthFacet()"
                                    );
                                    eprint!(" {}", n_type);
                                    eprint!(" {}", n_facet);
                                    eprint!(" {}", n_value);
                                    eprint!(" {}", n_val);
                                    eprintln!(" {}", n_length);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_schema_validate_length_facet_whtsp() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_facet in 0..GEN_NB_XML_SCHEMA_FACET_PTR {
                for n_val_type in 0..GEN_NB_XML_SCHEMA_VAL_TYPE {
                    for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_val in 0..GEN_NB_XML_SCHEMA_VAL_PTR {
                            for n_length in 0..GEN_NB_UNSIGNED_LONG_PTR {
                                for n_ws in 0..GEN_NB_XML_SCHEMA_WHITESPACE_VALUE_TYPE {
                                    let mem_base = xml_mem_blocks();
                                    let facet = gen_xml_schema_facet_ptr(n_facet, 0);
                                    let val_type = gen_xml_schema_val_type(n_val_type, 1);
                                    let value = gen_const_xml_char_ptr(n_value, 2);
                                    let val = gen_xml_schema_val_ptr(n_val, 3);
                                    let length = gen_unsigned_long_ptr(n_length, 4);
                                    let ws = gen_xml_schema_whitespace_value_type(n_ws, 5);

                                    let ret_val = xml_schema_validate_length_facet_whtsp(
                                        facet, val_type, value, val, length, ws,
                                    );
                                    desret_int(ret_val);
                                    des_xml_schema_facet_ptr(n_facet, facet, 0);
                                    des_xml_schema_val_type(n_val_type, val_type, 1);
                                    des_const_xml_char_ptr(n_value, value, 2);
                                    des_xml_schema_val_ptr(n_val, val, 3);
                                    des_unsigned_long_ptr(n_length, length, 4);
                                    des_xml_schema_whitespace_value_type(n_ws, ws, 5);
                                    reset_last_error();
                                    if mem_base != xml_mem_blocks() {
                                        leaks += 1;
                                        eprint!(
                                            "Leak of {} blocks found in xmlSchemaValidateLengthFacetWhtsp",
                                            xml_mem_blocks() - mem_base
                                        );
                                        assert!(
                                            leaks == 0,
                                            "{leaks} Leaks are found in xmlSchemaValidateLengthFacetWhtsp()"
                                        );
                                        eprint!(" {}", n_facet);
                                        eprint!(" {}", n_val_type);
                                        eprint!(" {}", n_value);
                                        eprint!(" {}", n_val);
                                        eprint!(" {}", n_length);
                                        eprintln!(" {}", n_ws);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_schema_validate_list_simple_type_facet() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_facet in 0..GEN_NB_XML_SCHEMA_FACET_PTR {
                for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_actual_len in 0..GEN_NB_UNSIGNED_LONG {
                        for n_expected_len in 0..GEN_NB_UNSIGNED_LONG_PTR {
                            let mem_base = xml_mem_blocks();
                            let facet = gen_xml_schema_facet_ptr(n_facet, 0);
                            let value = gen_const_xml_char_ptr(n_value, 1);
                            let actual_len = gen_unsigned_long(n_actual_len, 2);
                            let expected_len = gen_unsigned_long_ptr(n_expected_len, 3);

                            let ret_val = xml_schema_validate_list_simple_type_facet(
                                facet,
                                value,
                                actual_len,
                                expected_len,
                            );
                            desret_int(ret_val);
                            des_xml_schema_facet_ptr(n_facet, facet, 0);
                            des_const_xml_char_ptr(n_value, value, 1);
                            des_unsigned_long(n_actual_len, actual_len, 2);
                            des_unsigned_long_ptr(n_expected_len, expected_len, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlSchemaValidateListSimpleTypeFacet",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlSchemaValidateListSimpleTypeFacet()"
                                );
                                eprint!(" {}", n_facet);
                                eprint!(" {}", n_value);
                                eprint!(" {}", n_actual_len);
                                eprintln!(" {}", n_expected_len);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_schema_validate_predefined_type() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_type in 0..GEN_NB_XML_SCHEMA_TYPE_PTR {
                for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_val in 0..GEN_NB_XML_SCHEMA_VAL_PTR_PTR {
                        let mem_base = xml_mem_blocks();
                        let typ = gen_xml_schema_type_ptr(n_type, 0);
                        let value = gen_const_xml_char_ptr(n_value, 1);
                        let val = gen_xml_schema_val_ptr_ptr(n_val, 2);

                        let ret_val = xml_schema_validate_predefined_type(typ, value, val);
                        desret_int(ret_val);
                        des_xml_schema_type_ptr(n_type, typ, 0);
                        des_const_xml_char_ptr(n_value, value, 1);
                        des_xml_schema_val_ptr_ptr(n_val, val, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlSchemaValidatePredefinedType",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlSchemaValidatePredefinedType()"
                            );
                            eprint!(" {}", n_type);
                            eprint!(" {}", n_value);
                            eprintln!(" {}", n_val);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_schema_value_append() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_prev in 0..GEN_NB_XML_SCHEMA_VAL_PTR {
                for n_cur in 0..GEN_NB_XML_SCHEMA_VAL_PTR {
                    let mem_base = xml_mem_blocks();
                    let prev = gen_xml_schema_val_ptr(n_prev, 0);
                    let cur = gen_xml_schema_val_ptr(n_cur, 1);

                    let ret_val = xml_schema_value_append(prev, cur);
                    desret_int(ret_val);
                    des_xml_schema_val_ptr(n_prev, prev, 0);
                    des_xml_schema_val_ptr(n_cur, cur, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlSchemaValueAppend",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlSchemaValueAppend()"
                        );
                        eprint!(" {}", n_prev);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_schema_value_get_as_boolean() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_XML_SCHEMA_VAL_PTR {
                let mem_base = xml_mem_blocks();
                let val = gen_xml_schema_val_ptr(n_val, 0);

                let ret_val = xml_schema_value_get_as_boolean(val);
                desret_int(ret_val);
                des_xml_schema_val_ptr(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlSchemaValueGetAsBoolean",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlSchemaValueGetAsBoolean()"
                    );
                    eprintln!(" {}", n_val);
                }
            }
        }
    }

    #[test]
    fn test_xml_schema_value_get_as_string() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_XML_SCHEMA_VAL_PTR {
                let mem_base = xml_mem_blocks();
                let val = gen_xml_schema_val_ptr(n_val, 0);

                let ret_val = xml_schema_value_get_as_string(val);
                desret_const_xml_char_ptr(ret_val);
                des_xml_schema_val_ptr(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlSchemaValueGetAsString",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlSchemaValueGetAsString()"
                    );
                    eprintln!(" {}", n_val);
                }
            }
        }
    }
}
