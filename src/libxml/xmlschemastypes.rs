//! Provide methods and data structures for XML Schemas types.  
//! This module is based on `libxml/xmlschemastypes.h`, `xmlschemas.c`, `xmlschemastypes.c` and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    ffi::{c_char, c_int, c_long, c_uchar, c_ulong, CStr},
    mem::size_of,
    os::raw::c_void,
    ptr::{addr_of_mut, null, null_mut},
    sync::atomic::{AtomicBool, AtomicPtr, Ordering},
};

use libc::{c_uint, memcpy, memmove, memset, size_t, snprintf, sscanf};

use crate::{
    error::XmlErrorDomain,
    libxml::{
        entities::{xml_get_doc_entity, XmlEntityPtr, XmlEntityType},
        globals::{xml_free, xml_malloc, xml_malloc_atomic},
        hash::{
            xml_hash_add_entry2, xml_hash_create, xml_hash_free, xml_hash_lookup2, XmlHashTable,
        },
        schemas_internals::{
            xml_schema_free_annot, xml_schema_free_type, xml_schema_free_wildcard,
            XmlSchemaContentType, XmlSchemaFacet, XmlSchemaFacetPtr, XmlSchemaType,
            XmlSchemaTypePtr, XmlSchemaTypeType, XmlSchemaValType, XmlSchemaWildcard,
            XmlSchemaWildcardPtr, XML_SCHEMAS_ANY_LAX, XML_SCHEMAS_FACET_COLLAPSE,
            XML_SCHEMAS_FACET_PRESERVE, XML_SCHEMAS_FACET_REPLACE,
            XML_SCHEMAS_TYPE_BUILTIN_PRIMITIVE, XML_SCHEMAS_TYPE_HAS_FACETS,
            XML_SCHEMAS_TYPE_VARIETY_ATOMIC, XML_SCHEMAS_TYPE_VARIETY_LIST,
        },
        tree::{
            xml_search_ns, xml_split_qname2, xml_validate_name, xml_validate_ncname,
            xml_validate_nmtoken, xml_validate_qname, XmlAttrPtr, XmlAttributeType, XmlElementType,
            XmlIDPtr, XmlNodePtr, XmlNsPtr,
        },
        uri::{xml_free_uri, xml_parse_uri, XmlURIPtr},
        valid::{xml_add_id, xml_add_ref, xml_validate_notation_use},
        xmlerror::XmlParserErrors,
        xmlregexp::{xml_reg_free_regexp, xml_regexp_compile, xml_regexp_exec},
        xmlschemas::{
            xml_schema_custom_err, xml_schema_custom_err4, xml_schema_facet_type_to_string,
            xml_schema_format_qname, xml_schema_free_parser_ctxt, xml_schema_new_parser_ctxt,
            xml_schema_vcheck_cvc_simple_type, XmlSchemaAbstractCtxtPtr, XmlSchemaBasicItemPtr,
            XmlSchemaModelGroup, XmlSchemaModelGroupPtr, XmlSchemaParserCtxtPtr, XmlSchemaParticle,
            XmlSchemaParticlePtr, XmlSchemaTreeItemPtr,
        },
        xmlstring::{
            xml_str_equal, xml_strcat, xml_strcmp, xml_strdup, xml_strndup, xml_utf8_strlen,
            XmlChar,
        },
        xpath::{xml_xpath_is_nan, XML_XPATH_NAN, XML_XPATH_NINF, XML_XPATH_PINF},
    },
    private::error::__xml_simple_error,
    xml_is_digit_ch, IS_BLANK_CH,
};

use super::hash::CVoidWrapper;

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
        $crate::IS_BLANK_CH!($c)
    };
}

/* Date value */
pub type XmlSchemaValDatePtr = *mut XmlSchemaValDate;
#[repr(C)]
#[derive(Clone, Copy)]
pub struct XmlSchemaValDate {
    year: c_long,
    // c_uint	mon	:4;	/* 1 <=  mon    <= 12   */
    mon: u32,
    // c_uint	day	:5;	/* 1 <=  day    <= 31   */
    day: u32,
    // c_uint	hour	:5;	/* 0 <=  hour   <= 24   */
    hour: u32,
    // c_uint	min	:6;	/* 0 <=  min    <= 59	*/
    min: u32,
    sec: f64,
    // c_uint	tz_flag	:1;	/* is tzo explicitly set? */
    tz_flag: u32,
    // c_int		tzo	:12;	/* -1440 <= tzo <= 1440;
    // 				   currently only -840 to +840 are needed */
    tzo: i32,
}

/* Duration value */
pub type XmlSchemaValDurationPtr = *mut XmlSchemaValDuration;
#[repr(C)]
#[derive(Clone, Copy)]
pub struct XmlSchemaValDuration {
    mon: c_long, /* mon stores years also */
    day: c_long,
    sec: f64, /* sec stores min and hour also */
}

pub type XmlSchemaValDecimalPtr = *mut XmlSchemaValDecimal;
#[repr(C)]
#[derive(Clone, Copy)]
pub struct XmlSchemaValDecimal {
    /* would use long long but not portable */
    lo: c_ulong,
    mi: c_ulong,
    hi: c_ulong,
    extra: c_uint,
    // c_uint sign:1;
    sign: u32,
    // c_uint frac:7;
    frac: u32,
    // c_uint total:8;
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
    total: c_uint,
}

pub type XmlSchemaValBase64ptr = *mut XmlSchemaValBase64;
#[repr(C)]
#[derive(Clone, Copy)]
pub struct XmlSchemaValBase64 {
    str: *mut XmlChar,
    total: c_uint,
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
    b: c_int,
    str: *mut XmlChar,
}

pub type XmlSchemaValPtr = *mut XmlSchemaVal;
#[repr(C)]
pub struct XmlSchemaVal {
    typ: XmlSchemaValType,
    next: *mut XmlSchemaVal,
    value: XmlSchemaValInternal,
}

static XML_SCHEMA_TYPES_INITIALIZED: AtomicBool = AtomicBool::new(false);
static XML_SCHEMA_TYPES_BANK: AtomicPtr<XmlHashTable<'static, CVoidWrapper>> =
    AtomicPtr::new(null_mut());

/*
 * Basic types
 */
static XML_SCHEMA_TYPE_STRING_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_ANY_TYPE_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_DECIMAL_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_DATETIME_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_DATE_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_TIME_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_GYEAR_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_GYEAR_MONTH_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_GDAY_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_GMONTH_DAY_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_GMONTH_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_DURATION_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_FLOAT_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_BOOLEAN_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_DOUBLE_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_HEX_BINARY_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_BASE64_BINARY_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_ANY_URIDEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());

/*
 * Derived types
 */
static XML_SCHEMA_TYPE_POSITIVE_INTEGER_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_NON_POSITIVE_INTEGER_DEF: AtomicPtr<XmlSchemaType> =
    AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_NEGATIVE_INTEGER_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_NON_NEGATIVE_INTEGER_DEF: AtomicPtr<XmlSchemaType> =
    AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_INTEGER_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_LONG_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_INT_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_SHORT_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_BYTE_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_UNSIGNED_LONG_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_UNSIGNED_INT_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_UNSIGNED_SHORT_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_UNSIGNED_BYTE_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_NORM_STRING_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_TOKEN_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_LANGUAGE_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_NAME_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_QNAME_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_NCNAME_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_ID_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_IDREF_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_IDREFS_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_ENTITY_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_ENTITIES_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_NOTATION_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_NMTOKEN_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());
static XML_SCHEMA_TYPE_NMTOKENS_DEF: AtomicPtr<XmlSchemaType> = AtomicPtr::new(null_mut());

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

const DAYS_IN_MONTH: [c_uint; 12] = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
const DAYS_IN_MONTH_LEAP: [c_uint; 12] = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

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

const DAY_IN_YEAR_BY_MONTH: [c_long; 12] = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334];
const DAY_IN_LEAP_YEAR_BY_MONTH: [c_long; 12] =
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

/*
* WARNING: Those type reside normally in xmlschemas.c but are
* redefined here locally in oder of being able to use them for xs:anyType-
* TODO: Remove those definition if we move the types to a header file.
* TODO: Always keep those structs up-to-date with the originals.
*/
const UNBOUNDED: i32 = 1 << 30;

extern "C" fn xml_schema_free_type_entry(typ: *mut c_void, _name: *const XmlChar) {
    unsafe {
        xml_schema_free_type(typ as XmlSchemaTypePtr);
    }
}

/**
 * xmlSchemaCleanupTypesInternal:
 *
 * Cleanup the default XML Schemas type library
 */
unsafe extern "C" fn xml_schema_cleanup_types_internal() {
    let particle: XmlSchemaParticlePtr;

    /*
     * Free xs:anyType.
     */
    let anytype_def = XML_SCHEMA_TYPE_ANY_TYPE_DEF.load(Ordering::Acquire);
    if !anytype_def.is_null() {
        /* Attribute wildcard. */
        xml_schema_free_wildcard((*anytype_def).attribute_wildcard);
        /* Content type. */
        particle = (*anytype_def).subtypes as XmlSchemaParticlePtr;
        /* Wildcard. */
        xml_schema_free_wildcard(
            (*(*(*particle).children).children).children as XmlSchemaWildcardPtr,
        );
        xml_free((*(*particle).children).children as _);
        /* Sequence model group. */
        xml_free((*particle).children as _);
        xml_free(particle as _);
        (*anytype_def).subtypes = null_mut();
        XML_SCHEMA_TYPE_ANY_TYPE_DEF.store(null_mut(), Ordering::Release);
    }

    xml_hash_free(
        XML_SCHEMA_TYPES_BANK.load(Ordering::Acquire),
        Some(xml_schema_free_type_entry),
    );
    XML_SCHEMA_TYPES_BANK.store(null_mut(), Ordering::Release);
    /* Note that the xmlSchemaType*Def pointers aren't set to NULL. */
}

/**
 * xmlSchemaTypeErrMemory:
 * @extra:  extra information
 *
 * Handle an out of memory condition
 */
unsafe extern "C" fn xml_schema_type_err_memory(node: XmlNodePtr, extra: *const c_char) {
    __xml_simple_error(
        XmlErrorDomain::XmlFromDatatype,
        XmlParserErrors::XmlErrNoMemory,
        node,
        null(),
        extra,
    );
}

/**
 * xmlSchemaNewValue:
 * @type:  the value type
 *
 * Allocate a new simple type value
 *
 * Returns a pointer to the new value or NULL in case of error
 */
unsafe extern "C" fn xml_schema_new_value(typ: XmlSchemaValType) -> XmlSchemaValPtr {
    let value: XmlSchemaValPtr = xml_malloc(size_of::<XmlSchemaVal>()) as XmlSchemaValPtr;
    if value.is_null() {
        return null_mut();
    }
    memset(value as _, 0, size_of::<XmlSchemaVal>());
    (*value).typ = typ;
    value
}

unsafe extern "C" fn xml_schema_new_min_length_facet(value: c_int) -> XmlSchemaFacetPtr {
    let ret: XmlSchemaFacetPtr = xml_schema_new_facet();
    if ret.is_null() {
        return null_mut();
    }
    (*ret).typ = XmlSchemaTypeType::XmlSchemaFacetMinlength;
    (*ret).val = xml_schema_new_value(XmlSchemaValType::XmlSchemasNninteger);
    if (*ret).val.is_null() {
        xml_free(ret as _);
        return null_mut();
    }
    (*(*ret).val).value.decimal.lo = value as _;
    ret
}

/*
 * xmlSchemaInitBasicType:
 * @name:  the type name
 * @type:  the value type associated
 *
 * Initialize one primitive built-in type
 */
unsafe extern "C" fn xml_schema_init_basic_type(
    name: *const c_char,
    typ: XmlSchemaValType,
    base_type: XmlSchemaTypePtr,
) -> XmlSchemaTypePtr {
    let ret: XmlSchemaTypePtr = xml_malloc(size_of::<XmlSchemaType>()) as XmlSchemaTypePtr;
    if ret.is_null() {
        xml_schema_type_err_memory(
            null_mut(),
            c"could not initialize basic types".as_ptr() as _,
        );
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlSchemaType>());
    (*ret).name = name as *const XmlChar;
    (*ret).target_namespace = XML_SCHEMAS_NAMESPACE_NAME.as_ptr() as _;
    (*ret).typ = XmlSchemaTypeType::XmlSchemaTypeBasic;
    (*ret).base_type = base_type;
    (*ret).content_type = XmlSchemaContentType::XmlSchemaContentBasic;
    /*
     * Primitive types.
     */
    match typ {
        XmlSchemaValType::XmlSchemasString
        | XmlSchemaValType::XmlSchemasDecimal
        | XmlSchemaValType::XmlSchemasDate
        | XmlSchemaValType::XmlSchemasDatetime
        | XmlSchemaValType::XmlSchemasTime
        | XmlSchemaValType::XmlSchemasGyear
        | XmlSchemaValType::XmlSchemasGyearmonth
        | XmlSchemaValType::XmlSchemasGmonth
        | XmlSchemaValType::XmlSchemasGmonthday
        | XmlSchemaValType::XmlSchemasGday
        | XmlSchemaValType::XmlSchemasDuration
        | XmlSchemaValType::XmlSchemasFloat
        | XmlSchemaValType::XmlSchemasDouble
        | XmlSchemaValType::XmlSchemasBoolean
        | XmlSchemaValType::XmlSchemasAnyuri
        | XmlSchemaValType::XmlSchemasHexbinary
        | XmlSchemaValType::XmlSchemasBase64binary
        | XmlSchemaValType::XmlSchemasQname
        | XmlSchemaValType::XmlSchemasNotation => {
            (*ret).flags |= XML_SCHEMAS_TYPE_BUILTIN_PRIMITIVE;
        }
        _ => {}
    }
    /*
     * Set variety.
     */
    match typ {
        XmlSchemaValType::XmlSchemasAnytype | XmlSchemaValType::XmlSchemasAnysimpletype => {}
        XmlSchemaValType::XmlSchemasIdrefs
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
        XML_SCHEMA_TYPES_BANK.load(Ordering::Relaxed),
        (*ret).name,
        XML_SCHEMAS_NAMESPACE_NAME.as_ptr() as _,
        ret as _,
    );
    (*ret).built_in_type = typ as i32;
    ret
}

unsafe extern "C" fn xml_schema_add_particle() -> XmlSchemaParticlePtr {
    let ret: XmlSchemaParticlePtr =
        xml_malloc(size_of::<XmlSchemaParticle>()) as XmlSchemaParticlePtr;
    if ret.is_null() {
        xml_schema_type_err_memory(null_mut(), c"allocating particle component".as_ptr() as _);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlSchemaParticle>());
    (*ret).typ = XmlSchemaTypeType::XmlSchemaTypeParticle;
    (*ret).min_occurs = 1;
    (*ret).max_occurs = 1;
    ret
}

/*
 * xmlSchemaInitTypes:
 *
 * Initialize the default XML Schemas type library
 *
 * Returns 0 on success, -1 on error.
 */
pub unsafe extern "C" fn xml_schema_init_types() -> c_int {
    if XML_SCHEMA_TYPES_INITIALIZED.load(Ordering::Acquire) {
        return 0;
    }

    'error: {
        XML_SCHEMA_TYPES_BANK.store(xml_hash_create(40), Ordering::Release);
        if XML_SCHEMA_TYPES_BANK.load(Ordering::Acquire).is_null() {
            xml_schema_type_err_memory(null_mut(), null());
            break 'error;
        }

        /*
         * 3.4.7 Built-in Complex Type Definition
         */
        XML_SCHEMA_TYPE_ANY_TYPE_DEF.store(
            xml_schema_init_basic_type(
                c"anyType".as_ptr() as _,
                XmlSchemaValType::XmlSchemasAnytype,
                null_mut(),
            ),
            Ordering::Release,
        );
        if XML_SCHEMA_TYPE_ANY_TYPE_DEF
            .load(Ordering::Acquire)
            .is_null()
        {
            break 'error;
        }
        /*
         * Init the content type.
         */
        let anytype_def = XML_SCHEMA_TYPE_ANY_TYPE_DEF.load(Ordering::Acquire);
        (*anytype_def).base_type = anytype_def;
        (*anytype_def).content_type = XmlSchemaContentType::XmlSchemaContentMixed;
        {
            let mut particle: XmlSchemaParticlePtr;
            let mut wild: XmlSchemaWildcardPtr;
            /* First particle. */
            particle = xml_schema_add_particle();
            if particle.is_null() {
                break 'error;
            }
            (*anytype_def).subtypes = particle as XmlSchemaTypePtr;
            /* Sequence model group. */
            let sequence: XmlSchemaModelGroupPtr =
                xml_malloc(size_of::<XmlSchemaModelGroup>()) as XmlSchemaModelGroupPtr;
            if sequence.is_null() {
                xml_schema_type_err_memory(
                    null_mut(),
                    c"allocating model group component".as_ptr() as _,
                );
                break 'error;
            }
            memset(sequence as _, 0, size_of::<XmlSchemaModelGroup>());
            (*sequence).typ = XmlSchemaTypeType::XmlSchemaTypeSequence;
            (*particle).children = sequence as _;
            /* Second particle. */
            particle = xml_schema_add_particle();
            if particle.is_null() {
                break 'error;
            }
            (*particle).min_occurs = 0;
            (*particle).max_occurs = UNBOUNDED;
            (*sequence).children = particle as _;
            /* The wildcard */
            wild = xml_malloc(size_of::<XmlSchemaWildcard>()) as XmlSchemaWildcardPtr;
            if wild.is_null() {
                xml_schema_type_err_memory(
                    null_mut(),
                    c"allocating wildcard component".as_ptr() as _,
                );
                break 'error;
            }
            memset(wild as _, 0, size_of::<XmlSchemaWildcard>());
            (*wild).typ = XmlSchemaTypeType::XmlSchemaTypeAny;
            (*wild).any = 1;
            (*wild).process_contents = XML_SCHEMAS_ANY_LAX;
            (*particle).children = wild as XmlSchemaTreeItemPtr;
            /*
             * Create the attribute wildcard.
             */
            wild = xml_malloc(size_of::<XmlSchemaWildcard>()) as XmlSchemaWildcardPtr;
            if wild.is_null() {
                xml_schema_type_err_memory(
                    null_mut(),
                    c"could not create an attribute wildcard on anyType".as_ptr() as _,
                );
                break 'error;
            }
            memset(wild as _, 0, size_of::<XmlSchemaWildcard>());
            (*wild).any = 1;
            (*wild).process_contents = XML_SCHEMAS_ANY_LAX;
            (*anytype_def).attribute_wildcard = wild;
        }
        XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.store(
            xml_schema_init_basic_type(
                c"anySimpleType".as_ptr() as _,
                XmlSchemaValType::XmlSchemasAnysimpletype,
                XML_SCHEMA_TYPE_ANY_TYPE_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF
            .load(Ordering::Relaxed)
            .is_null()
        {
            break 'error;
        }
        /*
         * primitive datatypes
         */
        XML_SCHEMA_TYPE_STRING_DEF.store(
            xml_schema_init_basic_type(
                c"string".as_ptr() as _,
                XmlSchemaValType::XmlSchemasString,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_STRING_DEF.load(Ordering::Relaxed).is_null() {
            break 'error;
        }
        XML_SCHEMA_TYPE_DECIMAL_DEF.store(
            xml_schema_init_basic_type(
                c"decimal".as_ptr() as _,
                XmlSchemaValType::XmlSchemasDecimal,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_DECIMAL_DEF
            .load(Ordering::Relaxed)
            .is_null()
        {
            break 'error;
        }
        XML_SCHEMA_TYPE_DATE_DEF.store(
            xml_schema_init_basic_type(
                c"date".as_ptr() as _,
                XmlSchemaValType::XmlSchemasDate,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_DATE_DEF.load(Ordering::Relaxed).is_null() {
            break 'error;
        }
        XML_SCHEMA_TYPE_DATETIME_DEF.store(
            xml_schema_init_basic_type(
                c"dateTime".as_ptr() as _,
                XmlSchemaValType::XmlSchemasDatetime,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_DATETIME_DEF
            .load(Ordering::Relaxed)
            .is_null()
        {
            break 'error;
        }
        XML_SCHEMA_TYPE_TIME_DEF.store(
            xml_schema_init_basic_type(
                c"time".as_ptr() as _,
                XmlSchemaValType::XmlSchemasTime,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_TIME_DEF.load(Ordering::Relaxed).is_null() {
            break 'error;
        }
        XML_SCHEMA_TYPE_GYEAR_DEF.store(
            xml_schema_init_basic_type(
                c"gYear".as_ptr() as _,
                XmlSchemaValType::XmlSchemasGyear,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_GYEAR_DEF.load(Ordering::Relaxed).is_null() {
            break 'error;
        }
        XML_SCHEMA_TYPE_GYEAR_MONTH_DEF.store(
            xml_schema_init_basic_type(
                c"gYearMonth".as_ptr() as _,
                XmlSchemaValType::XmlSchemasGyearmonth,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_GYEAR_MONTH_DEF
            .load(Ordering::Relaxed)
            .is_null()
        {
            break 'error;
        }
        XML_SCHEMA_TYPE_GMONTH_DEF.store(
            xml_schema_init_basic_type(
                c"gMonth".as_ptr() as _,
                XmlSchemaValType::XmlSchemasGmonth,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_GMONTH_DEF.load(Ordering::Relaxed).is_null() {
            break 'error;
        }
        XML_SCHEMA_TYPE_GMONTH_DAY_DEF.store(
            xml_schema_init_basic_type(
                c"gMonthDay".as_ptr() as _,
                XmlSchemaValType::XmlSchemasGmonthday,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_GMONTH_DAY_DEF
            .load(Ordering::Relaxed)
            .is_null()
        {
            break 'error;
        }
        XML_SCHEMA_TYPE_GDAY_DEF.store(
            xml_schema_init_basic_type(
                c"gDay".as_ptr() as _,
                XmlSchemaValType::XmlSchemasGday,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_GDAY_DEF.load(Ordering::Relaxed).is_null() {
            break 'error;
        }
        XML_SCHEMA_TYPE_DURATION_DEF.store(
            xml_schema_init_basic_type(
                c"duration".as_ptr() as _,
                XmlSchemaValType::XmlSchemasDuration,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_DURATION_DEF
            .load(Ordering::Relaxed)
            .is_null()
        {
            break 'error;
        }
        XML_SCHEMA_TYPE_FLOAT_DEF.store(
            xml_schema_init_basic_type(
                c"float".as_ptr() as _,
                XmlSchemaValType::XmlSchemasFloat,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_FLOAT_DEF.load(Ordering::Relaxed).is_null() {
            break 'error;
        }
        XML_SCHEMA_TYPE_DOUBLE_DEF.store(
            xml_schema_init_basic_type(
                c"double".as_ptr() as _,
                XmlSchemaValType::XmlSchemasDouble,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_DOUBLE_DEF.load(Ordering::Relaxed).is_null() {
            break 'error;
        }
        XML_SCHEMA_TYPE_BOOLEAN_DEF.store(
            xml_schema_init_basic_type(
                c"boolean".as_ptr() as _,
                XmlSchemaValType::XmlSchemasBoolean,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_BOOLEAN_DEF
            .load(Ordering::Relaxed)
            .is_null()
        {
            break 'error;
        }
        XML_SCHEMA_TYPE_ANY_URIDEF.store(
            xml_schema_init_basic_type(
                c"anyURI".as_ptr() as _,
                XmlSchemaValType::XmlSchemasAnyuri,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_ANY_URIDEF.load(Ordering::Relaxed).is_null() {
            break 'error;
        }
        XML_SCHEMA_TYPE_HEX_BINARY_DEF.store(
            xml_schema_init_basic_type(
                c"hexBinary".as_ptr() as _,
                XmlSchemaValType::XmlSchemasHexbinary,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_HEX_BINARY_DEF
            .load(Ordering::Relaxed)
            .is_null()
        {
            break 'error;
        }
        XML_SCHEMA_TYPE_BASE64_BINARY_DEF.store(
            xml_schema_init_basic_type(
                c"base64Binary".as_ptr() as _,
                XmlSchemaValType::XmlSchemasBase64binary,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_BASE64_BINARY_DEF
            .load(Ordering::Relaxed)
            .is_null()
        {
            break 'error;
        }
        XML_SCHEMA_TYPE_NOTATION_DEF.store(
            xml_schema_init_basic_type(
                c"NOTATION".as_ptr() as _,
                XmlSchemaValType::XmlSchemasNotation,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_NOTATION_DEF
            .load(Ordering::Relaxed)
            .is_null()
        {
            break 'error;
        }
        XML_SCHEMA_TYPE_QNAME_DEF.store(
            xml_schema_init_basic_type(
                c"QName".as_ptr() as _,
                XmlSchemaValType::XmlSchemasQname,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_QNAME_DEF.load(Ordering::Relaxed).is_null() {
            break 'error;
        }

        /*
         * derived datatypes
         */
        XML_SCHEMA_TYPE_INTEGER_DEF.store(
            xml_schema_init_basic_type(
                c"integer".as_ptr() as _,
                XmlSchemaValType::XmlSchemasInteger,
                XML_SCHEMA_TYPE_DECIMAL_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_INTEGER_DEF
            .load(Ordering::Relaxed)
            .is_null()
        {
            break 'error;
        }
        XML_SCHEMA_TYPE_NON_POSITIVE_INTEGER_DEF.store(
            xml_schema_init_basic_type(
                c"nonPositiveInteger".as_ptr() as _,
                XmlSchemaValType::XmlSchemasNpinteger,
                XML_SCHEMA_TYPE_INTEGER_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_NON_POSITIVE_INTEGER_DEF
            .load(Ordering::Relaxed)
            .is_null()
        {
            break 'error;
        }
        XML_SCHEMA_TYPE_NEGATIVE_INTEGER_DEF.store(
            xml_schema_init_basic_type(
                c"negativeInteger".as_ptr() as _,
                XmlSchemaValType::XmlSchemasNinteger,
                XML_SCHEMA_TYPE_NON_POSITIVE_INTEGER_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_NEGATIVE_INTEGER_DEF
            .load(Ordering::Relaxed)
            .is_null()
        {
            break 'error;
        }
        XML_SCHEMA_TYPE_LONG_DEF.store(
            xml_schema_init_basic_type(
                c"long".as_ptr() as _,
                XmlSchemaValType::XmlSchemasLong,
                XML_SCHEMA_TYPE_INTEGER_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_LONG_DEF.load(Ordering::Relaxed).is_null() {
            break 'error;
        }
        XML_SCHEMA_TYPE_INT_DEF.store(
            xml_schema_init_basic_type(
                c"int".as_ptr() as _,
                XmlSchemaValType::XmlSchemasInt,
                XML_SCHEMA_TYPE_LONG_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_INT_DEF.load(Ordering::Relaxed).is_null() {
            break 'error;
        }
        XML_SCHEMA_TYPE_SHORT_DEF.store(
            xml_schema_init_basic_type(
                c"short".as_ptr() as _,
                XmlSchemaValType::XmlSchemasShort,
                XML_SCHEMA_TYPE_INT_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_SHORT_DEF.load(Ordering::Relaxed).is_null() {
            break 'error;
        }
        XML_SCHEMA_TYPE_BYTE_DEF.store(
            xml_schema_init_basic_type(
                c"byte".as_ptr() as _,
                XmlSchemaValType::XmlSchemasByte,
                XML_SCHEMA_TYPE_SHORT_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_BYTE_DEF.load(Ordering::Relaxed).is_null() {
            break 'error;
        }
        XML_SCHEMA_TYPE_NON_NEGATIVE_INTEGER_DEF.store(
            xml_schema_init_basic_type(
                c"nonNegativeInteger".as_ptr() as _,
                XmlSchemaValType::XmlSchemasNninteger,
                XML_SCHEMA_TYPE_INTEGER_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_NON_NEGATIVE_INTEGER_DEF
            .load(Ordering::Relaxed)
            .is_null()
        {
            break 'error;
        }
        XML_SCHEMA_TYPE_UNSIGNED_LONG_DEF.store(
            xml_schema_init_basic_type(
                c"unsignedLong".as_ptr() as _,
                XmlSchemaValType::XmlSchemasUlong,
                XML_SCHEMA_TYPE_NON_NEGATIVE_INTEGER_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_UNSIGNED_LONG_DEF
            .load(Ordering::Relaxed)
            .is_null()
        {
            break 'error;
        }
        XML_SCHEMA_TYPE_UNSIGNED_INT_DEF.store(
            xml_schema_init_basic_type(
                c"unsignedInt".as_ptr() as _,
                XmlSchemaValType::XmlSchemasUint,
                XML_SCHEMA_TYPE_UNSIGNED_LONG_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_UNSIGNED_INT_DEF
            .load(Ordering::Relaxed)
            .is_null()
        {
            break 'error;
        }
        XML_SCHEMA_TYPE_UNSIGNED_SHORT_DEF.store(
            xml_schema_init_basic_type(
                c"unsignedShort".as_ptr() as _,
                XmlSchemaValType::XmlSchemasUshort,
                XML_SCHEMA_TYPE_UNSIGNED_INT_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_UNSIGNED_SHORT_DEF
            .load(Ordering::Relaxed)
            .is_null()
        {
            break 'error;
        }
        XML_SCHEMA_TYPE_UNSIGNED_BYTE_DEF.store(
            xml_schema_init_basic_type(
                c"unsignedByte".as_ptr() as _,
                XmlSchemaValType::XmlSchemasUbyte,
                XML_SCHEMA_TYPE_UNSIGNED_SHORT_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_UNSIGNED_BYTE_DEF
            .load(Ordering::Relaxed)
            .is_null()
        {
            break 'error;
        }
        XML_SCHEMA_TYPE_POSITIVE_INTEGER_DEF.store(
            xml_schema_init_basic_type(
                c"positiveInteger".as_ptr() as _,
                XmlSchemaValType::XmlSchemasPinteger,
                XML_SCHEMA_TYPE_NON_NEGATIVE_INTEGER_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_POSITIVE_INTEGER_DEF
            .load(Ordering::Relaxed)
            .is_null()
        {
            break 'error;
        }
        XML_SCHEMA_TYPE_NORM_STRING_DEF.store(
            xml_schema_init_basic_type(
                c"normalizedString".as_ptr() as _,
                XmlSchemaValType::XmlSchemasNormstring,
                XML_SCHEMA_TYPE_STRING_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_NORM_STRING_DEF
            .load(Ordering::Relaxed)
            .is_null()
        {
            break 'error;
        }
        XML_SCHEMA_TYPE_TOKEN_DEF.store(
            xml_schema_init_basic_type(
                c"token".as_ptr() as _,
                XmlSchemaValType::XmlSchemasToken,
                XML_SCHEMA_TYPE_NORM_STRING_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_TOKEN_DEF.load(Ordering::Relaxed).is_null() {
            break 'error;
        }
        XML_SCHEMA_TYPE_LANGUAGE_DEF.store(
            xml_schema_init_basic_type(
                c"language".as_ptr() as _,
                XmlSchemaValType::XmlSchemasLanguage,
                XML_SCHEMA_TYPE_TOKEN_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_LANGUAGE_DEF
            .load(Ordering::Relaxed)
            .is_null()
        {
            break 'error;
        }
        XML_SCHEMA_TYPE_NAME_DEF.store(
            xml_schema_init_basic_type(
                c"Name".as_ptr() as _,
                XmlSchemaValType::XmlSchemasName,
                XML_SCHEMA_TYPE_TOKEN_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_NAME_DEF.load(Ordering::Relaxed).is_null() {
            break 'error;
        }
        XML_SCHEMA_TYPE_NMTOKEN_DEF.store(
            xml_schema_init_basic_type(
                c"NMTOKEN".as_ptr() as _,
                XmlSchemaValType::XmlSchemasNmtoken,
                XML_SCHEMA_TYPE_TOKEN_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_NMTOKEN_DEF
            .load(Ordering::Relaxed)
            .is_null()
        {
            break 'error;
        }
        XML_SCHEMA_TYPE_NCNAME_DEF.store(
            xml_schema_init_basic_type(
                c"NCName".as_ptr() as _,
                XmlSchemaValType::XmlSchemasNcname,
                XML_SCHEMA_TYPE_NAME_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_NCNAME_DEF.load(Ordering::Relaxed).is_null() {
            break 'error;
        }
        XML_SCHEMA_TYPE_ID_DEF.store(
            xml_schema_init_basic_type(
                c"ID".as_ptr() as _,
                XmlSchemaValType::XmlSchemasId,
                XML_SCHEMA_TYPE_NCNAME_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_ID_DEF.load(Ordering::Relaxed).is_null() {
            break 'error;
        }
        XML_SCHEMA_TYPE_IDREF_DEF.store(
            xml_schema_init_basic_type(
                c"IDREF".as_ptr() as _,
                XmlSchemaValType::XmlSchemasIdref,
                XML_SCHEMA_TYPE_NCNAME_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_IDREF_DEF.load(Ordering::Relaxed).is_null() {
            break 'error;
        }
        XML_SCHEMA_TYPE_ENTITY_DEF.store(
            xml_schema_init_basic_type(
                c"ENTITY".as_ptr() as _,
                XmlSchemaValType::XmlSchemasEntity,
                XML_SCHEMA_TYPE_NCNAME_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_ENTITY_DEF.load(Ordering::Relaxed).is_null() {
            break 'error;
        }
        /*
         * Derived list types.
         */
        /* ENTITIES */
        XML_SCHEMA_TYPE_ENTITIES_DEF.store(
            xml_schema_init_basic_type(
                c"ENTITIES".as_ptr() as _,
                XmlSchemaValType::XmlSchemasEntities,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_ENTITIES_DEF
            .load(Ordering::Relaxed)
            .is_null()
        {
            break 'error;
        }
        (*XML_SCHEMA_TYPE_ENTITIES_DEF.load(Ordering::Acquire)).subtypes =
            XML_SCHEMA_TYPE_ENTITY_DEF.load(Ordering::Relaxed);
        /* IDREFS */
        XML_SCHEMA_TYPE_IDREFS_DEF.store(
            xml_schema_init_basic_type(
                c"IDREFS".as_ptr() as _,
                XmlSchemaValType::XmlSchemasIdrefs,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_IDREFS_DEF.load(Ordering::Relaxed).is_null() {
            break 'error;
        }
        (*XML_SCHEMA_TYPE_IDREFS_DEF.load(Ordering::Relaxed)).subtypes =
            XML_SCHEMA_TYPE_IDREF_DEF.load(Ordering::Relaxed);

        /* NMTOKENS */
        XML_SCHEMA_TYPE_NMTOKENS_DEF.store(
            xml_schema_init_basic_type(
                c"NMTOKENS".as_ptr() as _,
                XmlSchemaValType::XmlSchemasNmtokens,
                XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.load(Ordering::Relaxed),
            ),
            Ordering::Relaxed,
        );
        if XML_SCHEMA_TYPE_NMTOKENS_DEF
            .load(Ordering::Relaxed)
            .is_null()
        {
            break 'error;
        }
        (*XML_SCHEMA_TYPE_NMTOKENS_DEF.load(Ordering::Relaxed)).subtypes =
            XML_SCHEMA_TYPE_NMTOKEN_DEF.load(Ordering::Relaxed);

        XML_SCHEMA_TYPES_INITIALIZED.store(true, Ordering::Release);
        return 0;
    }

    // error:
    xml_schema_cleanup_types_internal();
    -1
}

/**
 * xmlSchemaCleanupTypes:
 *
 * DEPRECATED: This function will be made private. Call xmlCleanupParser
 * to free global state but see the warnings there. xmlCleanupParser
 * should be only called once at program exit. In most cases, you don't
 * have to call cleanup functions at all.
 *
 * Cleanup the default XML Schemas type library
 */
pub(crate) unsafe extern "C" fn xml_schema_cleanup_types() {
    if XML_SCHEMA_TYPES_INITIALIZED.load(Ordering::Acquire) {
        xml_schema_cleanup_types_internal();
        XML_SCHEMA_TYPES_INITIALIZED.store(false, Ordering::Release);
    }
}

/**
 * xmlSchemaGetPredefinedType:
 * @name: the type name
 * @ns:  the URI of the namespace usually "http://www.w3.org/2001/XMLSchema"
 *
 * Lookup a type in the default XML Schemas type library
 *
 * Returns the type if found, NULL otherwise
 */
pub unsafe extern "C" fn xml_schema_get_predefined_type(
    name: *const XmlChar,
    ns: *const XmlChar,
) -> XmlSchemaTypePtr {
    if !XML_SCHEMA_TYPES_INITIALIZED.load(Ordering::Acquire) && xml_schema_init_types() < 0 {
        return null_mut();
    }
    if name.is_null() {
        return null_mut();
    }
    xml_hash_lookup2(XML_SCHEMA_TYPES_BANK.load(Ordering::Acquire), name, ns) as XmlSchemaTypePtr
}

/**
 * xmlSchemaValidatePredefinedType:
 * @type: the predefined type
 * @value: the value to check
 * @val:  the return computed value
 *
 * Check that a value conforms to the lexical space of the predefined type.
 * if true a value is computed and returned in @val.
 *
 * Returns 0 if this validates, a positive error code number otherwise
 *         and -1 in case of internal or API error.
 */
pub unsafe extern "C" fn xml_schema_validate_predefined_type(
    typ: XmlSchemaTypePtr,
    value: *const XmlChar,
    val: *mut XmlSchemaValPtr,
) -> c_int {
    xml_schema_val_predef_type_node(typ, value, val, null_mut())
}

/**
 * xmlSchemaParseUInt:
 * @str: pointer to the string R/W
 * @llo: pointer to the low result
 * @lmi: pointer to the mid result
 * @lhi: pointer to the high result
 *
 * Parse an c_ulong into 3 fields.
 *
 * Returns the number of significant digits in the number or
 * -1 if overflow of the capacity and -2 if it's not a number.
 */
unsafe extern "C" fn xml_schema_parse_uint(
    str: *mut *const XmlChar,
    llo: *mut c_ulong,
    lmi: *mut c_ulong,
    lhi: *mut c_ulong,
) -> c_int {
    let mut lo: c_ulong = 0;
    let mut mi: c_ulong = 0;
    let mut hi: c_ulong = 0;
    let mut tmp: *const XmlChar;
    let mut cur: *const XmlChar = *str;
    let mut ret: c_int = 0;
    let mut i: c_int = 0;

    if !(*cur >= b'0' && *cur <= b'9') {
        return -2;
    }

    while *cur == b'0' {
        /* ignore leading zeroes */
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

/**
 * PARSE_2_DIGITS:
 * @num:  the integer to fill in
 * @cur:  an #xmlChar *
 * @invalid: an integer
 *
 * Parses a 2-digits integer and updates @num with the value. @cur is
 * updated to point just after the integer.
 * In case of error, @invalid is set to %TRUE, values of @num and
 * @cur are undefined.
 */
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

/**
 * PARSE_FLOAT:
 * @num:  the double to fill in
 * @cur:  an #xmlChar *
 * @invalid: an integer
 *
 * Parses a float and updates @num with the value. @cur is
 * updated to point just after the float. The float must have a
 * 2-digits integer part and may or may not have a decimal part.
 * In case of error, @invalid is set to %TRUE, values of @num and
 * @cur are undefined.
 */
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

/**
 * _xmlSchemaParseTimeZone:
 * @dt:  pointer to a date structure
 * @str: pointer to the string to analyze
 *
 * Parses a time zone without time zone and fills in the appropriate
 * field of the @dt structure. @str is updated to point just after the
 * time zone.
 *
 * Returns 0 or the error code
 */
unsafe extern "C" fn _xml_schema_parse_time_zone(
    dt: XmlSchemaValDatePtr,
    str: *mut *const XmlChar,
) -> c_int {
    let mut cur: *const XmlChar;
    let mut ret: c_int = 0;

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
            let mut tmp: c_int = 0;
            let isneg: c_int = (*cur == b'-') as i32;

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

/**
 * _xmlSchemaParseGDay:
 * @dt:  pointer to a date structure
 * @str: pointer to the string to analyze
 *
 * Parses a xs:gDay without time zone and fills in the appropriate
 * field of the @dt structure. @str is updated to point just after the
 * xs:gDay.
 *
 * Returns 0 or the error code
 */
unsafe extern "C" fn _xml_schema_parse_gday(
    dt: XmlSchemaValDatePtr,
    str: *mut *const XmlChar,
) -> c_int {
    let mut cur: *const XmlChar = *str;
    let mut ret: c_int = 0;
    let mut value: c_uint = 0;

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

/**
 * _xmlSchemaParseGMonth:
 * @dt:  pointer to a date structure
 * @str: pointer to the string to analyze
 *
 * Parses a xs:gMonth without time zone and fills in the appropriate
 * field of the @dt structure. @str is updated to point just after the
 * xs:gMonth.
 *
 * Returns 0 or the error code
 */
unsafe extern "C" fn _xml_schema_parse_gmonth(
    dt: XmlSchemaValDatePtr,
    str: *mut *const XmlChar,
) -> c_int {
    let mut cur: *const XmlChar = *str;
    let mut ret: c_int = 0;
    let mut value: c_uint = 0;

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

/**
 * _xmlSchemaParseTime:
 * @dt:  pointer to a date structure
 * @str: pointer to the string to analyze
 *
 * Parses a xs:time without time zone and fills in the appropriate
 * fields of the @dt structure. @str is updated to point just after the
 * xs:time.
 * In case of error, values of @dt fields are undefined.
 *
 * Returns 0 or the error code
 */
unsafe extern "C" fn _xml_schema_parse_time(
    dt: XmlSchemaValDatePtr,
    str: *mut *const XmlChar,
) -> c_int {
    let mut cur: *const XmlChar = *str;
    let mut ret: c_int = 0;
    let mut value: c_int = 0;

    PARSE_2_DIGITS!(value, cur, ret);
    if ret != 0 {
        return ret;
    }
    if *cur != b':' {
        return 1;
    }

    /* Allow end-of-day hour */
    if !VALID_HOUR!(value) && value != 24 {
        return 2;
    }
    cur = cur.add(1);

    /* the ':' insures this string is xs:time */
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

/**
 * _xmlSchemaParseGYear:
 * @dt:  pointer to a date structure
 * @str: pointer to the string to analyze
 *
 * Parses a xs:gYear without time zone and fills in the appropriate
 * field of the @dt structure. @str is updated to point just after the
 * xs:gYear. It is supposed that @(*dt).year is big enough to contain
 * the year.
 *
 * Returns 0 or the error code
 */
unsafe extern "C" fn _xml_schema_parse_gyear(
    dt: XmlSchemaValDatePtr,
    str: *mut *const XmlChar,
) -> c_int {
    let mut cur: *const XmlChar = *str;
    let mut isneg: c_int = 0;
    let mut digcnt: c_int = 0;

    if (*cur < b'0' || *cur > b'9') && *cur != b'-' && *cur != b'+' {
        return -1;
    }

    if *cur == b'-' {
        isneg = 1;
        cur = cur.add(1);
    }

    let first_char: *const XmlChar = cur;

    while *cur >= b'0' && *cur <= b'9' {
        let digit: c_int = (*cur - b'0') as _;

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

    /* year must be at least 4 digits (CCYY); over 4
     * digits cannot have a leading zero. */
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

/**
 * xmlSchemaValidateDates:
 * @type: the expected type or XmlSchemaValType::XmlSchemasUnknown
 * @dateTime:  string to analyze
 * @val:  the return computed value
 *
 * Check that @dateTime conforms to the lexical space of one of the date types.
 * if true a value is computed and returned in @val.
 *
 * Returns 0 if this validates, a positive error code number otherwise
 *         and -1 in case of internal or API error.
 */
unsafe extern "C" fn xml_schema_validate_dates(
    typ: XmlSchemaValType,
    date_time: *const XmlChar,
    val: *mut XmlSchemaValPtr,
    collapse: c_int,
) -> c_int {
    let mut ret: c_int;
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
            /*
             * It's an incomplete date (xs:gMonthDay, xs:gMonth or
             * xs:gDay)
             */
            cur = cur.add(2);

            /* is it an xs:gDay? */
            if *cur == b'-' {
                if typ == XmlSchemaValType::XmlSchemasGmonth {
                    break 'error;
                }
                cur = cur.add(1);
                ret = _xml_schema_parse_gday(addr_of_mut!((*dt).value.date), addr_of_mut!(cur));
                if ret != 0 {
                    break 'error;
                }

                RETURN_TYPE_IF_VALID!(cur, ret, dt, typ, val, XmlSchemaValType::XmlSchemasGday);

                break 'error;
            }

            /*
             * it should be an xs:gMonthDay or xs:gMonth
             */
            ret = _xml_schema_parse_gmonth(addr_of_mut!((*dt).value.date), addr_of_mut!(cur));
            if ret != 0 {
                break 'error;
            }

            /*
             * a '-' c_char could indicate this type is xs:gMonthDay or
             * a negative time zone offset. Check for xs:gMonthDay first.
             * Also the first three c_char's of a negative tzo (-MM:SS) can
             * appear to be a valid day; so even if the day portion
             * of the xs:gMonthDay verifies, we must insure it was not
             * a tzo.
             */
            if *cur == b'-' {
                let rewnd: *const XmlChar = cur;
                cur = cur.add(1);

                ret = _xml_schema_parse_gday(addr_of_mut!((*dt).value.date), addr_of_mut!(cur));
                if ret == 0 && (*cur == 0 || *cur != b':') {
                    /*
                     * we can use the VALID_MDAY macro to validate the month
                     * and day because the leap year test will flag year zero
                     * as a leap year (even though zero is an invalid year).
                     * FUTURE TODO: Zero will become valid in XML Schema 1.1
                     * probably.
                     */
                    if VALID_MDAY!((addr_of_mut!((*dt).value.date))) {
                        RETURN_TYPE_IF_VALID!(
                            cur,
                            ret,
                            dt,
                            typ,
                            val,
                            XmlSchemaValType::XmlSchemasGmonthday
                        );

                        break 'error;
                    }
                }

                /*
                 * not xs:gMonthDay so rewind and check if just xs:gMonth
                 * with an optional time zone.
                 */
                cur = rewnd;
            }

            RETURN_TYPE_IF_VALID!(cur, ret, dt, typ, val, XmlSchemaValType::XmlSchemasGmonth);

            break 'error;
        }

        /*
         * It's a right-truncated date or an xs:time.
         * Try to parse an xs:time then fallback on right-truncated dates.
         */
        if *cur >= b'0' && *cur <= b'9' {
            ret = _xml_schema_parse_time(addr_of_mut!((*dt).value.date), addr_of_mut!(cur));
            if ret == 0 {
                /* it's an xs:time */
                RETURN_TYPE_IF_VALID!(cur, ret, dt, typ, val, XmlSchemaValType::XmlSchemasTime);
            }
        }

        /* fallback on date parsing */
        cur = date_time;

        ret = _xml_schema_parse_gyear(addr_of_mut!((*dt).value.date), addr_of_mut!(cur));
        if ret != 0 {
            break 'error;
        }

        /* is it an xs:gYear? */
        RETURN_TYPE_IF_VALID!(cur, ret, dt, typ, val, XmlSchemaValType::XmlSchemasGyear);

        if *cur != b'-' {
            break 'error;
        }
        cur = cur.add(1);

        ret = _xml_schema_parse_gmonth(addr_of_mut!((*dt).value.date), addr_of_mut!(cur));
        if ret != 0 {
            break 'error;
        }

        /* is it an xs:gYearMonth? */
        RETURN_TYPE_IF_VALID!(
            cur,
            ret,
            dt,
            typ,
            val,
            XmlSchemaValType::XmlSchemasGyearmonth
        );

        if *cur != b'-' {
            break 'error;
        }
        cur = cur.add(1);

        ret = _xml_schema_parse_gday(addr_of_mut!((*dt).value.date), addr_of_mut!(cur));
        if ret != 0 || !VALID_DATE!((addr_of_mut!((*dt).value.date))) {
            break 'error;
        }

        /* is it an xs:date? */
        RETURN_TYPE_IF_VALID!(cur, ret, dt, typ, val, XmlSchemaValType::XmlSchemasDate);

        if *cur != b'T' {
            break 'error;
        }
        cur = cur.add(1);

        /* it should be an xs:dateTime */
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

/**
 * xmlSchemaValidateDuration:
 * @type: the predefined type
 * @duration:  string to analyze
 * @val:  the return computed value
 *
 * Check that @duration conforms to the lexical space of the duration type.
 * if true a value is computed and returned in @val.
 *
 * Returns 0 if this validates, a positive error code number otherwise
 *         and -1 in case of internal or API error.
 */
unsafe extern "C" fn xml_schema_validate_duration(
    _typ: XmlSchemaTypePtr,
    duration: *const XmlChar,
    val: *mut XmlSchemaValPtr,
    collapse: c_int,
) -> c_int {
    let mut cur: *const XmlChar = duration;
    let mut isneg: c_int = 0;
    let mut seq: usize = 0;
    let mut days: c_long;
    let mut secs: c_long = 0;
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

    /* duration must start with 'P' (after sign) */
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
            let mut num: c_long = 0;
            let mut has_digits: size_t = 0;
            let mut has_frac: c_int = 0;
            let desig: &[XmlChar] = b"YMDHMS";

            /* input string should be empty or invalid date/time item */
            if seq >= desig.len() {
                break 'error;
            }

            /* T designator must be present for time items */
            if *cur == b'T' {
                if seq > 3 {
                    break 'error;
                }
                cur = cur.add(1);
                seq = 3;
            } else if seq == 3 {
                break 'error;
            }

            /* Parse integral part. */
            while *cur >= b'0' && *cur <= b'9' {
                let digit: c_long = (*cur - b'0') as _;

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
                /* Parse fractional part. */
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
                /* No T designator or invalid c_char. */
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
                    /* Year */
                    if num > i64::MAX / 12 {
                        break 'error;
                    }
                    (*dur).value.dur.mon = num * 12;
                }
                1 => {
                    /* Month */
                    if (*dur).value.dur.mon > i64::MAX - num {
                        break 'error;
                    }
                    (*dur).value.dur.mon += num;
                }
                2 => {
                    /* Day */
                    (*dur).value.dur.day = num;
                }
                3 => {
                    /* Hour */
                    days = num / HOURS_PER_DAY as i64;
                    if (*dur).value.dur.day > i64::MAX - days {
                        break 'error;
                    }
                    (*dur).value.dur.day += days;
                    secs = (num % HOURS_PER_DAY as i64) * SECS_PER_HOUR as i64;
                }
                4 => {
                    /* Minute */
                    days = num / MINS_PER_DAY as i64;
                    if (*dur).value.dur.day > i64::MAX - days {
                        break 'error;
                    }
                    (*dur).value.dur.day += days;
                    secs += (num % MINS_PER_DAY as i64) * SECS_PER_MIN as i64;
                }
                5 => {
                    /* Second */
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

/*
 * xmlSchemaCheckLanguageType
 * @value: the value to check
 *
 * Check that a value conforms to the lexical space of the language datatype.
 * Must conform to [a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*
 *
 * Returns 1 if this validates, 0 otherwise.
 */
unsafe extern "C" fn xml_schema_check_language_type(value: *const XmlChar) -> c_int {
    let mut first: c_int = 1;
    let mut len: c_int = 0;
    let mut cur: *const XmlChar = value;

    if value.is_null() {
        return 0;
    }

    while *cur.add(0) != 0 {
        if !((*cur.add(0) >= b'a' && *cur.add(0) <= b'z')
            || (*cur.add(0) >= b'A' && *cur.add(0) <= b'Z')
            || *cur.add(0) == b'-'
            || (first == 0 && xml_is_digit_ch!(*cur.add(0))))
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

/**
 * xmlSchemaValAtomicListNode:
 * @type: the predefined atomic type for a token in the list
 * @value: the list value to check
 * @ret:  the return computed value
 * @node:  the node containing the value
 *
 * Check that a value conforms to the lexical space of the predefined
 * list type. if true a value is computed and returned in @ret.
 *
 * Returns the number of items if this validates, a negative error code
 *         number otherwise
 */
unsafe extern "C" fn xml_schema_val_atomic_list_node(
    typ: XmlSchemaTypePtr,
    value: *const XmlChar,
    ret: *mut XmlSchemaValPtr,
    node: XmlNodePtr,
) -> c_int {
    let mut cur: *mut XmlChar;
    let mut nb_values: c_int = 0;
    let mut tmp: c_int = 0;

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
    /*
     * Split the list
     */
    while IS_BLANK_CH!(*cur) {
        *cur = 0;
        cur = cur.add(1);
    }
    while *cur != 0 {
        if IS_BLANK_CH!(*cur) {
            *cur = 0;
            cur = cur.add(1);
            while IS_BLANK_CH!(*cur) {
                *cur = 0;
                cur = cur.add(1);
            }
        } else {
            nb_values += 1;
            cur = cur.add(1);
            while *cur != 0 && !IS_BLANK_CH!(*cur) {
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

/**
 * xmlSchemaStrip:
 * @value: a value
 *
 * Removes the leading and ending spaces of a string
 *
 * Returns the new string or NULL if no change was required.
 */
unsafe extern "C" fn xml_schema_strip(value: *const XmlChar) -> *mut XmlChar {
    let mut start: *const XmlChar = value;
    let mut end: *const XmlChar;

    if value.is_null() {
        return null_mut();
    }
    while *start != 0 && IS_BLANK_CH!(*start) {
        start = start.add(1);
    }
    end = start;
    while *end != 0 {
        end = end.add(1);
    }
    let f: *const XmlChar = end;
    end = end.sub(1);
    while end > start && IS_BLANK_CH!(*end) {
        end = end.sub(1);
    }
    end = end.add(1);
    if start == value && f == end {
        return null_mut();
    }
    xml_strndup(start, end.offset_from(start) as _)
}

/**
 * _xmlSchemaBase64Decode:
 * @ch: a character
 *
 * Converts a base64 encoded character to its base 64 value.
 *
 * Returns 0-63 (value), 64 (pad), or -1 (not recognized)
 */
unsafe extern "C" fn _xml_schema_base64_decode(ch: XmlChar) -> c_int {
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

/**
 * xmlSchemaValAtomicType:
 * @type: the predefined type
 * @value: the value to check
 * @val:  the return computed value
 * @node:  the node containing the value
 * flags:  flags to control the validation
 *
 * Check that a value conforms to the lexical space of the atomic type.
 * if true a value is computed and returned in @val.
 * This checks the value space for list types as well (IDREFS, NMTOKENS).
 *
 * Returns 0 if this validates, a positive error code number otherwise
 *         and -1 in case of internal or API error.
 */
unsafe extern "C" fn xml_schema_val_atomic_type(
    typ: XmlSchemaTypePtr,
    mut value: *const XmlChar,
    val: *mut XmlSchemaValPtr,
    node: XmlNodePtr,
    flags: c_int,
    ws: XmlSchemaWhitespaceValueType,
    norm_on_the_fly: c_int,
    apply_norm: c_int,
    create_string_value: c_int,
) -> c_int {
    let v: XmlSchemaValPtr;
    let mut norm: *mut XmlChar = null_mut();
    let mut ret: c_int;

    if !XML_SCHEMA_TYPES_INITIALIZED.load(Ordering::Acquire) && xml_schema_init_types() < 0 {
        return -1;
    }
    if typ.is_null() {
        return -1;
    }

    /*
     * validating a non existent text node is similar to validating
     * an empty one.
     */
    if value.is_null() {
        value = c"".as_ptr() as _;
    }

    if !val.is_null() {
        *val = null_mut();
    }
    if (flags == 0 && !value.is_null())
        && ((*typ).built_in_type != XmlSchemaValType::XmlSchemasString as i32
            && (*typ).built_in_type != XmlSchemaValType::XmlSchemasAnytype as i32
            && (*typ).built_in_type != XmlSchemaValType::XmlSchemasAnysimpletype as i32)
    {
        if (*typ).built_in_type == XmlSchemaValType::XmlSchemasNormstring as i32 {
            norm = xml_schema_white_space_replace(value);
        } else {
            norm = xml_schema_collapse_string(value);
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
                            | XmlSchemaValType::XmlSchemasAnysimpletype => {
                                if create_string_value != 0 && !val.is_null() {
                                    v = xml_schema_new_value(
                                        XmlSchemaValType::XmlSchemasAnysimpletype,
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

                                    if ws
                                        == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceReplace
                                    {
                                        while *cur != 0 {
                                            if *cur == 0xd || *cur == 0xa || *cur == 0x9 {
                                                break 'return1;
                                            } else {
                                                cur = cur.add(1);
                                            }
                                        }
                                    } else if ws
                                        == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse
                                    {
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
                                        if ws
                                        == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse
                                    {
                                        norm = xml_schema_collapse_string(value);
                                    } else if ws
                                        == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceReplace
                                    {
                                        norm = xml_schema_white_space_replace(value);
                                    }
                                        if !norm.is_null() {
                                            value = norm;
                                        }
                                    }
                                    v = xml_schema_new_value(XmlSchemaValType::XmlSchemasString);
                                    if !v.is_null() {
                                        (*v).value.str = xml_strdup(value);
                                        *val = v;
                                    } else {
                                        break 'error;
                                    }
                                }
                                break 'return0;
                            }
                            XmlSchemaValType::XmlSchemasNormstring => {
                                if norm_on_the_fly != 0 {
                                    if apply_norm != 0 {
                                        if ws
                                        == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse
                                    {
                                        norm = xml_schema_collapse_string(value);
                                    } else {
                                        norm = xml_schema_white_space_replace(value);
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
                                        XmlSchemaValType::XmlSchemasNormstring,
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
                                let mut len: c_uint;
                                let mut neg: c_uint;
                                let mut integ: c_uint;
                                let mut has_leading_zeroes: c_uint;
                                let mut cval: [XmlChar; 25] = [0; 25];
                                let mut cptr: *mut XmlChar = cval.as_mut_ptr();

                                if cur.is_null() || *cur == 0 {
                                    break 'return1;
                                }

                                /*
                                 * xs:decimal has a whitespace-facet value of 'collapse'.
                                 */
                                if norm_on_the_fly != 0 {
                                    while IS_WSP_BLANK_CH!(*cur) {
                                        cur = cur.add(1);
                                    }
                                }

                                /*
                                 * First we handle an optional sign.
                                 */
                                neg = 0;
                                if *cur == b'-' {
                                    neg = 1;
                                    cur = cur.add(1);
                                } else if *cur == b'+' {
                                    cur = cur.add(1);
                                }
                                /*
                                 * Disallow: "", "-", "- "
                                 */
                                if *cur == 0 {
                                    break 'return1;
                                }
                                /*
                                 * Next we "pre-parse" the number, in preparation for calling
                                 * the common routine xmlSchemaParseUInt.  We get rid of any
                                 * leading zeroes (because we have reserved only 25 chars),
                                 * and note the position of a decimal point.
                                 */
                                len = 0;
                                integ = !0;
                                has_leading_zeroes = 0;
                                /*
                                 * Skip leading zeroes.
                                 */
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
                                            /*
                                             * Disallow "." but allow "00."
                                             */
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
                                    /* error if any extraneous chars */
                                }
                                if !val.is_null() {
                                    v = xml_schema_new_value(XmlSchemaValType::XmlSchemasDecimal);
                                    if !v.is_null() {
                                        /*
                                         * Now evaluate the significant digits of the number
                                         */
                                        if len != 0 {
                                            if integ != !0 {
                                                /*
                                                 * Get rid of trailing zeroes in the
                                                 * fractional part.
                                                 */
                                                while len != integ && *cptr.sub(1) == b'0' {
                                                    cptr = cptr.sub(1);
                                                    len = len.wrapping_sub(1);
                                                }
                                            }
                                            /*
                                             * Terminate the (preparsed) string.
                                             */
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
                                        /*
                                         * Set the total digits to 1 if a zero value.
                                         */
                                        (*v).value.decimal.sign = neg;
                                        if len == 0 {
                                            /* Speedup for zero values. */
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
                            | XmlSchemaValType::XmlSchemasGday
                            | XmlSchemaValType::XmlSchemasGmonth
                            | XmlSchemaValType::XmlSchemasGmonthday
                            | XmlSchemaValType::XmlSchemasGyear
                            | XmlSchemaValType::XmlSchemasGyearmonth
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
                                ret =
                                    xml_schema_validate_duration(typ, value, val, norm_on_the_fly);
                            }
                            XmlSchemaValType::XmlSchemasFloat
                            | XmlSchemaValType::XmlSchemasDouble => {
                                let mut cur: *const XmlChar = value;
                                let mut neg: c_int = 0;
                                let mut digits_before: c_int = 0;
                                let mut digits_after: c_int = 0;

                                if norm_on_the_fly != 0 {
                                    while IS_WSP_BLANK_CH!(*cur) {
                                        cur = cur.add(1);
                                    }
                                }

                                if *cur.add(0) == b'N' && *cur.add(1) == b'a' && *cur.add(2) == b'N'
                                {
                                    cur = cur.add(3);
                                    if *cur != 0 {
                                        break 'return1;
                                    }
                                    if !val.is_null() {
                                        if typ == XML_SCHEMA_TYPE_FLOAT_DEF.load(Ordering::Relaxed)
                                        {
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
                                if *cur.add(0) == b'I' && *cur.add(1) == b'N' && *cur.add(2) == b'F'
                                {
                                    cur = cur.add(3);
                                    if *cur != 0 {
                                        break 'return1;
                                    }
                                    if !val.is_null() {
                                        if typ == XML_SCHEMA_TYPE_FLOAT_DEF.load(Ordering::Relaxed)
                                        {
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
                                if *cur.add(0) == 0 || *cur.add(0) == b'+' || *cur.add(0) == b'-' {
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
                                    if typ == XML_SCHEMA_TYPE_FLOAT_DEF.load(Ordering::Relaxed) {
                                        v = xml_schema_new_value(XmlSchemaValType::XmlSchemasFloat);
                                        if !v.is_null() {
                                            /*
                                             * TODO: sscanf seems not to give the correct
                                             * value for extremely high/low values.
                                             * E.g. "1E-149" results in zero.
                                             */
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
                                            /*
                                             * TODO: sscanf seems not to give the correct
                                             * value for extremely high/low values.
                                             */
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
                                    v = xml_schema_new_value(XmlSchemaValType::XmlSchemasBoolean);
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
                                    norm = xml_schema_collapse_string(value);
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
                                    XML_SCHEMA_TYPE_NMTOKEN_DEF.load(Ordering::Relaxed),
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
                                        while IS_BLANK_CH!(*start) {
                                            start = start.add(1);
                                        }
                                        end = start;
                                        while *end != 0 && !IS_BLANK_CH!(*end) {
                                            end = end.add(1);
                                        }
                                        (*v).value.str =
                                            xml_strndup(start, end.offset_from(start) as _) as _;
                                        *val = v;
                                    } else {
                                        break 'error;
                                    }
                                }
                                break 'done;
                            }
                            XmlSchemaValType::XmlSchemasQname => {
                                let mut uri: *const XmlChar = null();
                                let mut local: *mut XmlChar = null_mut();

                                ret = xml_validate_qname(value, 1);
                                if ret != 0 {
                                    break 'done;
                                }
                                if !node.is_null() {
                                    let mut prefix: *mut XmlChar = null_mut();

                                    local = xml_split_qname2(value, addr_of_mut!(prefix));
                                    let ns: XmlNsPtr = xml_search_ns((*node).doc, node, prefix);
                                    if ns.is_null() && !prefix.is_null() {
                                        xml_free(prefix as _);
                                        if !local.is_null() {
                                            xml_free(local as _);
                                        }
                                        break 'return1;
                                    }
                                    if !ns.is_null() {
                                        uri = (*ns).href.load(Ordering::Relaxed);
                                    }
                                    if !prefix.is_null() {
                                        xml_free(prefix as _);
                                    }
                                }
                                if !val.is_null() {
                                    v = xml_schema_new_value(XmlSchemaValType::XmlSchemasQname);
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
                            XmlSchemaValType::XmlSchemasNcname => {
                                ret = xml_validate_ncname(value, 1);
                                if ret == 0 && !val.is_null() {
                                    v = xml_schema_new_value(XmlSchemaValType::XmlSchemasNcname);
                                    if !v.is_null() {
                                        (*v).value.str = xml_strdup(value);
                                        *val = v;
                                    } else {
                                        break 'error;
                                    }
                                }
                                break 'done;
                            }
                            XmlSchemaValType::XmlSchemasId => {
                                ret = xml_validate_ncname(value, 1);
                                if ret == 0 && !val.is_null() {
                                    v = xml_schema_new_value(XmlSchemaValType::XmlSchemasId);
                                    if !v.is_null() {
                                        (*v).value.str = xml_strdup(value);
                                        *val = v;
                                    } else {
                                        break 'error;
                                    }
                                }
                                if ret == 0
                                    && !node.is_null()
                                    && (*node).typ == XmlElementType::XmlAttributeNode
                                {
                                    let attr: XmlAttrPtr = node as XmlAttrPtr;

                                    /*
                                     * NOTE: the IDness might have already be declared in the DTD
                                     */
                                    if !matches!(
                                        (*attr).atype,
                                        Some(XmlAttributeType::XmlAttributeId)
                                    ) {
                                        let res: XmlIDPtr;

                                        let strip: *mut XmlChar = xml_schema_strip(value);
                                        if !strip.is_null() {
                                            res = xml_add_id(null_mut(), (*node).doc, strip, attr);
                                            xml_free(strip as _);
                                        } else {
                                            res = xml_add_id(null_mut(), (*node).doc, value, attr);
                                        }
                                        if res.is_null() {
                                            ret = 2;
                                        } else {
                                            (*attr).atype = Some(XmlAttributeType::XmlAttributeId);
                                        }
                                    }
                                }
                                break 'done;
                            }
                            XmlSchemaValType::XmlSchemasIdref => {
                                ret = xml_validate_ncname(value, 1);
                                if ret == 0 && !val.is_null() {
                                    v = xml_schema_new_value(XmlSchemaValType::XmlSchemasIdref);
                                    if v.is_null() {
                                        break 'error;
                                    }
                                    (*v).value.str = xml_strdup(value);
                                    *val = v;
                                }
                                if ret == 0
                                    && !node.is_null()
                                    && (*node).typ == XmlElementType::XmlAttributeNode
                                {
                                    let attr: XmlAttrPtr = node as XmlAttrPtr;

                                    let strip: *mut XmlChar = xml_schema_strip(value);
                                    if !strip.is_null() {
                                        xml_add_ref(null_mut(), (*node).doc, strip, attr);
                                        xml_free(strip as _);
                                    } else {
                                        xml_add_ref(null_mut(), (*node).doc, value, attr);
                                    }
                                    (*attr).atype = Some(XmlAttributeType::XmlAttributeIdref);
                                }
                                break 'done;
                            }
                            XmlSchemaValType::XmlSchemasIdrefs => {
                                ret = xml_schema_val_atomic_list_node(
                                    XML_SCHEMA_TYPE_IDREF_DEF.load(Ordering::Relaxed),
                                    value,
                                    val,
                                    node,
                                );
                                if ret < 0 {
                                    ret = 2;
                                } else {
                                    ret = 0;
                                }
                                if ret == 0
                                    && !node.is_null()
                                    && (*node).typ == XmlElementType::XmlAttributeNode
                                {
                                    let attr: XmlAttrPtr = node as XmlAttrPtr;

                                    (*attr).atype = Some(XmlAttributeType::XmlAttributeIdrefs);
                                }
                                break 'done;
                            }
                            XmlSchemaValType::XmlSchemasEntity => {
                                let strip: *mut XmlChar;

                                ret = xml_validate_ncname(value, 1);
                                if node.is_null() || (*node).doc.is_null() {
                                    ret = 3;
                                }
                                if ret == 0 {
                                    let ent: XmlEntityPtr;

                                    strip = xml_schema_strip(value);
                                    if !strip.is_null() {
                                        ent = xml_get_doc_entity((*node).doc, strip);
                                        xml_free(strip as _);
                                    } else {
                                        ent = xml_get_doc_entity((*node).doc, value);
                                    }
                                    if ent.is_null()
                                        || !matches!(
                                            (*ent).etype,
                                            Some(XmlEntityType::XmlExternalGeneralUnparsedEntity)
                                        )
                                    {
                                        ret = 4;
                                    }
                                }
                                if ret == 0 && !val.is_null() {
                                    // TODO;
                                    todo!()
                                }
                                if ret == 0
                                    && !node.is_null()
                                    && (*node).typ == XmlElementType::XmlAttributeNode
                                {
                                    let attr: XmlAttrPtr = node as XmlAttrPtr;

                                    (*attr).atype = Some(XmlAttributeType::XmlAttributeEntity);
                                }
                                break 'done;
                            }
                            XmlSchemaValType::XmlSchemasEntities => {
                                if node.is_null() || (*node).doc.is_null() {
                                    break 'return3;
                                }
                                ret = xml_schema_val_atomic_list_node(
                                    XML_SCHEMA_TYPE_ENTITY_DEF.load(Ordering::Relaxed),
                                    value,
                                    val,
                                    node,
                                );
                                if ret <= 0 {
                                    ret = 1;
                                } else {
                                    ret = 0;
                                }
                                if ret == 0
                                    && !node.is_null()
                                    && (*node).typ == XmlElementType::XmlAttributeNode
                                {
                                    let attr: XmlAttrPtr = node as XmlAttrPtr;

                                    (*attr).atype = Some(XmlAttributeType::XmlAttributeEntities);
                                }
                                break 'done;
                            }
                            XmlSchemaValType::XmlSchemasNotation => {
                                let mut uri: *mut XmlChar = null_mut();
                                let mut local: *mut XmlChar = null_mut();

                                ret = xml_validate_qname(value, 1);
                                if ret == 0 && !node.is_null() {
                                    let mut prefix: *mut XmlChar = null_mut();

                                    local = xml_split_qname2(value, addr_of_mut!(prefix));
                                    if !prefix.is_null() {
                                        let ns: XmlNsPtr = xml_search_ns((*node).doc, node, prefix);
                                        if ns.is_null() {
                                            ret = 1;
                                        } else if !val.is_null() {
                                            uri = xml_strdup((*ns).href.load(Ordering::Relaxed));
                                        }
                                    }
                                    if !local.is_null() && (val.is_null() || ret != 0) {
                                        xml_free(local as _);
                                    }
                                    if !prefix.is_null() {
                                        xml_free(prefix as _);
                                    }
                                }
                                if node.is_null() || (*node).doc.is_null() {
                                    ret = 3;
                                }
                                if ret == 0 {
                                    ret = xml_validate_notation_use(null_mut(), (*node).doc, value);
                                    if ret == 1 {
                                        ret = 0;
                                    } else {
                                        ret = 1;
                                    }
                                }
                                if ret == 0 && !val.is_null() {
                                    v = xml_schema_new_value(XmlSchemaValType::XmlSchemasNotation);
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
                                break 'done;
                            }
                            XmlSchemaValType::XmlSchemasAnyuri => {
                                if *value != 0 {
                                    let mut cur: *mut XmlChar;
                                    if norm.is_null() && norm_on_the_fly != 0 {
                                        norm = xml_schema_collapse_string(value);
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
                                    v = xml_schema_new_value(XmlSchemaValType::XmlSchemasAnyuri);
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
                                let total: c_int;
                                let mut i: c_int = 0;

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
                                    v = xml_schema_new_value(XmlSchemaValType::XmlSchemasHexbinary);
                                    if v.is_null() {
                                        break 'error;
                                    }
                                    /*
                                     * Copy only the normalized piece.
                                     * CRITICAL TODO: Check this.
                                     */
                                    cur = xml_strndup(start, i);
                                    if cur.is_null() {
                                        xml_schema_type_err_memory(
                                            node,
                                            c"allocating hexbin data".as_ptr() as _,
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
                                /* ISSUE:
                                 *
                                 * Ignore all stray characters? (yes, currently)
                                 * Worry about c_long lines? (no, currently)
                                 *
                                 * rfc2045.txt:
                                 *
                                 * "The encoded output stream must be represented in lines of
                                 * no more than 76 characters each.  All line breaks or other
                                 * characters not found in Table 1 must be ignored by decoding
                                 * software.  In base64 data, characters other than those in
                                 * Table 1, line breaks, and other white space probably
                                 * indicate a transmission error, about which a warning
                                 * message or even a message rejection might be appropriate
                                 * under some circumstances." */
                                let mut cur: *const XmlChar = value;
                                let mut base: *mut XmlChar;
                                let mut total: c_int;
                                let mut i: c_int = 0;
                                let mut pad: c_int = 0;

                                if cur.is_null() {
                                    break 'return1;
                                }

                                while *cur != 0 {
                                    let decc: c_int = _xml_schema_base64_decode(*cur);
                                    if decc < 0 {
                                    } else if decc < 64 {
                                        i += 1;
                                    } else {
                                        break;
                                    }
                                    cur = cur.add(1);
                                }
                                while *cur != 0 {
                                    let decc: c_int = _xml_schema_base64_decode(*cur);
                                    if decc < 0 {
                                    } else if decc < 64 {
                                        break 'return1;
                                    }
                                    if decc == 64 {
                                        pad += 1;
                                    }
                                    cur = cur.add(1);
                                }

                                /* rfc2045.txt: "Special processing is performed if fewer than
                                 * 24 bits are available at the end of the data being encoded.
                                 * A full encoding quantum is always completed at the end of a
                                 * body.  When fewer than 24 input bits are available in an
                                 * input group, zero bits are added (on the right) to form an
                                 * integral number of 6-bit groups.  Padding at the end of the
                                 * data is performed using the "=" character.  Since all
                                 * base64 input is an integral number of octets, only the
                                 * following cases can arise: (1) the final quantum of
                                 * encoding input is an integral multiple of 24 bits; here,
                                 * the final unit of encoded output will be an integral
                                 * multiple of indent: Standard input:701: Warning:old style
                                 * assignment ambiguity in "=*".  Assuming "= *" 4 characters
                                 * with no "=" padding, (2) the final
                                 * quantum of encoding input is exactly 8 bits; here, the
                                 * final unit of encoded output will be two characters
                                 * followed by two "=" padding characters, or (3) the final
                                 * quantum of encoding input is exactly 16 bits; here, the
                                 * final unit of encoded output will be three characters
                                 * followed by one "=" padding character." */

                                total = 3 * (i / 4);
                                if pad == 0 {
                                    if i % 4 != 0 {
                                        break 'return1;
                                    }
                                } else if pad == 1 {
                                    let mut decc: c_int;

                                    if i % 4 != 3 {
                                        break 'return1;
                                    }
                                    decc = _xml_schema_base64_decode(*cur);
                                    while !(0..=63).contains(&decc) {
                                        cur = cur.sub(1);
                                        decc = _xml_schema_base64_decode(*cur)
                                    }
                                    /* 16bits in 24bits means 2 pad bits: nnnnnn nnmmmm mmmm00*/
                                    /* 00111100 -> 0x3c */
                                    if decc & !0x3c != 0 {
                                        break 'return1;
                                    }
                                    total += 2;
                                } else if pad == 2 {
                                    let mut decc: c_int;

                                    if i % 4 != 2 {
                                        break 'return1;
                                    }
                                    decc = _xml_schema_base64_decode(*cur);
                                    while !(0..=63).contains(&decc) {
                                        cur = cur.sub(1);
                                        decc = _xml_schema_base64_decode(*cur)
                                    }
                                    /* 8bits in 12bits means 4 pad bits: nnnnnn nn0000 */
                                    /* 00110000 -> 0x30 */
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
                                    base = xml_malloc_atomic(i as usize + pad as usize + 1) as _;
                                    if base.is_null() {
                                        xml_schema_type_err_memory(
                                            node,
                                            c"allocating base64 data".as_ptr() as _,
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
                            | XmlSchemaValType::XmlSchemasPinteger
                            | XmlSchemaValType::XmlSchemasNpinteger
                            | XmlSchemaValType::XmlSchemasNinteger
                            | XmlSchemaValType::XmlSchemasNninteger => {
                                let mut cur: *const XmlChar = value;
                                let mut lo: c_ulong = 0;
                                let mut mi: c_ulong = 0;
                                let mut hi: c_ulong = 0;
                                let mut sign: c_int = 0;

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
                                    == XmlSchemaValType::XmlSchemasNpinteger as i32
                                {
                                    if sign == 0 && (hi != 0 || mi != 0 || lo != 0) {
                                        break 'return1;
                                    }
                                } else if (*typ).built_in_type
                                    == XmlSchemaValType::XmlSchemasPinteger as i32
                                {
                                    if sign == 1 {
                                        break 'return1;
                                    }
                                    if hi == 0 && mi == 0 && lo == 0 {
                                        break 'return1;
                                    }
                                } else if (*typ).built_in_type
                                    == XmlSchemaValType::XmlSchemasNinteger as i32
                                {
                                    if sign == 0 {
                                        break 'return1;
                                    }
                                    if hi == 0 && mi == 0 && lo == 0 {
                                        break 'return1;
                                    }
                                } else if (*typ).built_in_type
                                    == XmlSchemaValType::XmlSchemasNninteger as i32
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
                                let mut lo: c_ulong = 0;
                                let mut mi: c_ulong = 0;
                                let mut hi: c_ulong = 0;
                                let mut sign: c_int = 0;

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
                                if (*typ).built_in_type == XmlSchemaValType::XmlSchemasLong as i32 {
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
                            XmlSchemaValType::XmlSchemasUint
                            | XmlSchemaValType::XmlSchemasUlong
                            | XmlSchemaValType::XmlSchemasUshort
                            | XmlSchemaValType::XmlSchemasUbyte => {
                                let mut cur: *const XmlChar = value;
                                let mut lo: c_ulong = 0;
                                let mut mi: c_ulong = 0;
                                let mut hi: c_ulong = 0;

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
                                if (*typ).built_in_type == XmlSchemaValType::XmlSchemasUlong as i32
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
                                    == XmlSchemaValType::XmlSchemasUint as i32
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
                                    == XmlSchemaValType::XmlSchemasUshort as i32
                                {
                                    if mi != 0 || hi != 0 {
                                        break 'return1;
                                    }
                                    if lo > 65535 {
                                        break 'return1;
                                    }
                                } else if (*typ).built_in_type
                                    == XmlSchemaValType::XmlSchemasUbyte as i32
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

/**
 * xmlSchemaValPredefTypeNode:
 * @type: the predefined type
 * @value: the value to check
 * @val:  the return computed value
 * @node:  the node containing the value
 *
 * Check that a value conforms to the lexical space of the predefined type.
 * if true a value is computed and returned in @val.
 *
 * Returns 0 if this validates, a positive error code number otherwise
 *         and -1 in case of internal or API error.
 */
pub unsafe extern "C" fn xml_schema_val_predef_type_node(
    typ: XmlSchemaTypePtr,
    value: *const XmlChar,
    val: *mut XmlSchemaValPtr,
    node: XmlNodePtr,
) -> c_int {
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

/**
 * xmlSchemaCompareDecimals:
 * @x:  a first decimal value
 * @y:  a second decimal value
 *
 * Compare 2 decimals
 *
 * Returns -1 if x < y, 0 if x == y, 1 if x > y and -2 in case of error
 */
unsafe extern "C" fn xml_schema_compare_decimals(x: XmlSchemaValPtr, y: XmlSchemaValPtr) -> c_int {
    let swp: XmlSchemaValPtr;
    let mut order: c_int = 1;
    let mut dlen: c_int;
    let mut hi: c_ulong;
    let mut mi: c_ulong;
    let mut lo: c_ulong;

    /*
     * First test: If x is -ve and not zero
     */
    if (*x).value.decimal.sign != 0
        && ((*x).value.decimal.lo != 0 || (*x).value.decimal.mi != 0 || (*x).value.decimal.hi != 0)
    {
        /*
         * Then if y is -ve and not zero reverse the compare
         */
        if (*y).value.decimal.sign != 0
            && ((*y).value.decimal.lo != 0
                || (*y).value.decimal.mi != 0
                || (*y).value.decimal.hi != 0)
        {
            order = -1;
        }
        /*
         * Otherwise (y >= 0) we have the answer
         */
        else {
            return -1;
        }
    /*
     * If x is not -ve and y is -ve we have the answer
     */
    } else if (*y).value.decimal.sign != 0
        && ((*y).value.decimal.lo != 0 || (*y).value.decimal.mi != 0 || (*y).value.decimal.hi != 0)
    {
        return 1;
    }
    /*
     * If it's not simply determined by a difference in sign,
     * then we need to compare the actual values of the two nums.
     * To do this, we start by looking at the integral parts.
     * If the number of integral digits differ, then we have our
     * answer.
     */
    let integx: c_int = ((*x).value.decimal.total - (*x).value.decimal.frac) as _;
    let integy: c_int = ((*y).value.decimal.total - (*y).value.decimal.frac) as _;
    /*
     * NOTE: We changed the "total" for values like "0.1"
     *   (or "-0.1" or ".1") to be 1, which was 2 previously.
     *   Therefore the special case, when such values are
     *   compared with 0, needs to be handled separately;
     *   otherwise a zero would be recognized incorrectly as
     *   greater than those values. This has the nice side effect
     *   that we gain an overall optimized comparison with zeroes.
     * Note that a "0" has a "total" of 1 already.
     */
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

    /*
     * If the number of integral digits is the same for both numbers,
     * then things get a little more complicated.  We need to "normalize"
     * the numbers in order to properly compare them.  To do this, we
     * look at the total length of each number (length => number of
     * significant digits), and divide the "shorter" by 10 (decreasing
     * the length) until they are of equal length.
     */
    dlen = ((*x).value.decimal.total - (*y).value.decimal.total) as _;
    if dlen < 0 {
        /* y has more digits than x */
        swp = x;
        hi = (*y).value.decimal.hi;
        mi = (*y).value.decimal.mi;
        lo = (*y).value.decimal.lo;
        dlen = -dlen;
        order = -order;
    } else {
        /* x has more digits than y */
        swp = y;
        hi = (*x).value.decimal.hi;
        mi = (*x).value.decimal.mi;
        lo = (*x).value.decimal.lo;
    }
    while dlen > 8 {
        /* in effect, right shift by 10**8 */
        lo = mi;
        mi = hi;
        hi = 0;
        dlen -= 8;
    }
    while dlen > 0 {
        let rem1: c_ulong = (hi % 10) * 100000000u64;
        hi /= 10;
        let rem2: c_ulong = (mi % 10) * 100000000u64;
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

/**
 * xmlSchemaCompareDurations:
 * @x:  a first duration value
 * @y:  a second duration value
 *
 * Compare 2 durations
 *
 * Returns -1 if x < y, 0 if x == y, 1 if x > y, 2 if x <> y, and -2 in
 * case of error
 */
unsafe extern "C" fn xml_schema_compare_durations(x: XmlSchemaValPtr, y: XmlSchemaValPtr) -> c_int {
    let mut sec: f64;
    let mut invert: c_int = 1;
    let mut xmon: c_long;
    let xday: c_long;

    let mut minday: c_long;
    let mut maxday: c_long;
    const DAY_RANGE: [[c_long; 12]; 2] = [
        [0, 28, 59, 89, 120, 150, 181, 212, 242, 273, 303, 334],
        [0, 31, 62, 92, 123, 153, 184, 215, 245, 276, 306, 337],
    ];

    if x.is_null() || y.is_null() {
        return -2;
    }

    /* months */
    let mon: c_long = (*x).value.dur.mon - (*y).value.dur.mon;

    /* seconds */
    sec = (*x).value.dur.sec - (*y).value.dur.sec;
    let carry: c_long = (sec / SECS_PER_DAY as f64) as c_long;
    sec -= carry as f64 * SECS_PER_DAY as f64;

    /* days */
    let day: c_long = (*x).value.dur.day - (*y).value.dur.day + carry;

    /* easy test */
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

    let myear: c_long = xmon / 12;
    if myear == 0 {
        minday = 0;
        maxday = 0;
    } else {
        if myear > i64::MAX / 366 {
            return -2;
        }
        /* FIXME: This doesn't take leap year exceptions every 100/400 years
        into account. */
        maxday = 365 * myear + (myear + 3) / 4;
        /* FIXME: Needs to be calculated separately */
        minday = maxday - 1;
    }

    xmon %= 12;
    minday += DAY_RANGE[0][xmon as usize];
    maxday += DAY_RANGE[1][xmon as usize];

    if maxday == minday && maxday == xday {
        return 0; /* can this really happen ? */
    }
    if maxday < xday {
        return -invert;
    }
    if minday > xday {
        return invert;
    }

    /* indeterminate */
    2
}

/**
 * xmlSchemaDupVal:
 * @v: the #value: xmlSchemaValPtr to duplicate
 *
 * Makes a copy of @v. The calling program is responsible for freeing
 * the returned value.
 *
 * returns a pointer to a duplicated #xmlSchemaValPtr or NULL if error.
 */
unsafe extern "C" fn xml_schema_dup_val(v: XmlSchemaValPtr) -> XmlSchemaValPtr {
    let ret: XmlSchemaValPtr = xml_schema_new_value((*v).typ);
    if ret.is_null() {
        return null_mut();
    }

    memcpy(ret as _, v as _, size_of::<XmlSchemaVal>());
    (*ret).next = null_mut();
    ret
}

/*
 * macros for adding date/times and durations
 */
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

/**
 * _xmlSchemaDateAdd:
 * @dt: an #xmlSchemaValPtr
 * @dur: an #xmlSchemaValPtr of type #XS_DURATION
 *
 * Compute a new date/time from @dt and @dur. This function assumes @dt
 * is either #XmlSchemaValType::XML_SCHEMAS_DATETIME, #XmlSchemaValType::XML_SCHEMAS_DATE, #XmlSchemaValType::XML_SCHEMAS_GYEARMONTH,
 * or #XmlSchemaValType::XML_SCHEMAS_GYEAR. The returned #xmlSchemaVal is the same type as
 * @dt. The calling program is responsible for freeing the returned value.
 *
 * Returns a pointer to a new #xmlSchemaVal or NULL if error.
 */
unsafe extern "C" fn _xml_schema_date_add(
    dt: XmlSchemaValPtr,
    dur: XmlSchemaValPtr,
) -> XmlSchemaValPtr {
    let mut carry: c_long;
    let mut tempdays: c_long;
    let mut temp: c_long;

    if dt.is_null() || dur.is_null() {
        return null_mut();
    }

    let ret: XmlSchemaValPtr = xml_schema_new_value((*dt).typ);
    if ret.is_null() {
        return null_mut();
    }

    /* make a copy so we don't alter the original value */
    let tmp: XmlSchemaValPtr = xml_schema_dup_val(dt);
    if tmp.is_null() {
        xml_schema_free_value(ret);
        return null_mut();
    }

    let r: XmlSchemaValDatePtr = addr_of_mut!((*ret).value.date);
    let d: XmlSchemaValDatePtr = addr_of_mut!((*tmp).value.date);
    let u: XmlSchemaValDurationPtr = addr_of_mut!((*dur).value.dur);

    /* normalization */
    if (*d).mon == 0 {
        (*d).mon = 1;
    }

    /* normalize for time zone offset */
    (*u).sec -= (*d).tzo as f64 * 60.;
    (*d).tzo = 0;

    /* normalization */
    if (*d).day == 0 {
        (*d).day = 1;
    }

    /* month */
    carry = (*d).mon as i64 + (*u).mon;
    (*r).mon = MODULO_RANGE!(carry, 1, 13) as c_uint;
    carry = FQUOTIENT_RANGE!(carry, 1, 13) as c_long;

    /* year (may be modified later) */
    (*r).year = (*d).year + carry;
    if (*r).year == 0 {
        if (*d).year > 0 {
            (*r).year -= 1;
        } else {
            (*r).year += 1;
        }
    }

    /* time zone */
    (*r).tzo = (*d).tzo;
    (*r).tz_flag = (*d).tz_flag;

    /* seconds */
    (*r).sec = (*d).sec + (*u).sec;
    carry = FQUOTIENT!((*r).sec as c_long, 60) as c_long;
    if (*r).sec != 0.0 {
        (*r).sec = MODULO!((*r).sec, 60.0);
    }

    /* minute */
    carry += (*d).min as i64;
    (*r).min = MODULO!(carry, 60) as c_uint;
    carry = FQUOTIENT!(carry, 60) as c_long;

    /* hours */
    carry += (*d).hour as i64;
    (*r).hour = MODULO!(carry, 24) as c_uint;
    carry = FQUOTIENT!(carry, 24) as c_long;

    /*
     * days
     * Note we use tempdays because the temporary values may need more
     * than 5 bits
     */
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
            let mut tmon: c_long = MODULO_RANGE!((*r).mon as c_int - 1, 1, 13) as c_long;
            let mut tyr: c_long =
                (*r).year + FQUOTIENT_RANGE!((*r).mon as c_int - 1, 1, 13) as c_long;
            if tyr == 0 {
                tyr -= 1;
            }
            /*
             * Coverity detected an overrun in DAYS_IN_MONTH
             * of size 12 at position 12 with index variable "((r)->mon - 1)"
             */
            tmon = tmon.clamp(1, 12);
            tempdays += MAX_DAYINMONTH!(tyr, tmon) as i64;
            carry = -1;
        } else if VALID_YEAR!((*r).year)
            && VALID_MONTH!((*r).mon)
            && tempdays > MAX_DAYINMONTH!((*r).year, (*r).mon) as c_long
        {
            tempdays -= MAX_DAYINMONTH!((*r).year, (*r).mon) as i64;
            carry = 1;
        } else {
            break;
        }

        temp = (*r).mon as i64 + carry;
        (*r).mon = MODULO_RANGE!(temp, 1, 13) as c_uint;
        (*r).year += FQUOTIENT_RANGE!(temp, 1, 13) as c_long;
        if (*r).year == 0 {
            if temp < 1 {
                (*r).year -= 1;
            } else {
                (*r).year += 1;
            }
        }
    }

    (*r).day = tempdays as u32;

    /*
     * adjust the date/time type to the date values
     */
    if (*ret).typ != XmlSchemaValType::XmlSchemasDatetime {
        if (*r).hour != 0 || (*r).min != 0 || (*r).sec != 0.0 {
            (*ret).typ = XmlSchemaValType::XmlSchemasDatetime;
        } else if (*ret).typ != XmlSchemaValType::XmlSchemasDate {
            if (*r).mon != 1 && (*r).day != 1 {
                (*ret).typ = XmlSchemaValType::XmlSchemasDate;
            } else if (*ret).typ != XmlSchemaValType::XmlSchemasGyearmonth && (*r).mon != 1 {
                (*ret).typ = XmlSchemaValType::XmlSchemasGyearmonth;
            }
        }
    }

    xml_schema_free_value(tmp);

    ret
}

/**
 * xmlSchemaDateNormalize:
 * @dt: an #xmlSchemaValPtr of a date/time type value.
 * @offset: number of seconds to adjust @dt by.
 *
 * Normalize @dt to GMT time. The @offset parameter is subtracted from
 * the return value is a time-zone offset is present on @dt.
 *
 * Returns a normalized copy of @dt or NULL if error.
 */
unsafe extern "C" fn xml_schema_date_normalize(
    dt: XmlSchemaValPtr,
    offset: f64,
) -> XmlSchemaValPtr {
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

    /* (*ret).value.date.tzo = 0; */
    ret
}

/**
 * _xmlSchemaDateCastYMToDays:
 * @dt: an #xmlSchemaValPtr
 *
 * Convert mon and year of @dt to total number of days. Take the
 * number of years since (or before) 1 AD and add the number of leap
 * years. This is a function  because negative
 * years must be handled a little differently and there is no zero year.
 *
 * Returns number of days.
 */
unsafe extern "C" fn _xml_schema_date_cast_ymto_days(dt: XmlSchemaValPtr) -> c_long {
    let mut mon: c_int;

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

/**
 * TIME_TO_NUMBER:
 * @dt:  an #xmlSchemaValPtr
 *
 * Calculates the number of seconds in the time portion of @dt.
 *
 * Returns seconds.
 */
macro_rules! TIME_TO_NUMBER {
    ($dt:expr) => {
        (((*$dt).value.date.hour as i32 * SECS_PER_HOUR)
            + ((*$dt).value.date.min as i32 * SECS_PER_MIN)
            + ((*$dt).value.date.tzo * SECS_PER_MIN)) as f64
            + (*$dt).value.date.sec
    };
}

/**
 * xmlSchemaCompareDates:
 * @x:  a first date/time value
 * @y:  a second date/time value
 *
 * Compare 2 date/times
 *
 * Returns -1 if x < y, 0 if x == y, 1 if x > y, 2 if x <> y, and -2 in
 * case of error
 */
unsafe extern "C" fn xml_schema_compare_dates(x: XmlSchemaValPtr, y: XmlSchemaValPtr) -> c_int {
    let mut p1: XmlSchemaValPtr;
    let p2: XmlSchemaValPtr;
    let mut q1: XmlSchemaValPtr;
    let q2: XmlSchemaValPtr;
    let mut p1d: c_long;
    let p2d: c_long;
    let mut q1d: c_long;
    let q2d: c_long;

    if x.is_null() || y.is_null() {
        return -2;
    }

    if (*x).value.date.year > i64::MAX / 366
        || (*x).value.date.year < i64::MIN / 366
        || (*y).value.date.year > i64::MAX / 366
        || (*y).value.date.year < i64::MIN / 366
    {
        /* Possible overflow when converting to days. */
        return -2;
    }

    if (*x).value.date.tz_flag != 0 {
        if (*y).value.date.tz_flag == 0 {
            p1 = xml_schema_date_normalize(x, 0.);
            if p1.is_null() {
                return -2;
            }
            p1d = _xml_schema_date_cast_ymto_days(p1) + (*p1).value.date.day as i64;
            /* normalize y + 14:00 */
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
                        let mut ret: c_int = 0;
                        /* normalize y - 14:00 */
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

        /* normalize x - 14:00 */
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
                    let mut ret: c_int = 0;
                    /* normalize x + 14:00 */
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

    /*
     * if the same type then calculate the difference
     */
    if (*x).typ == (*y).typ {
        let mut ret: c_int = 0;
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
        XmlSchemaValType::XmlSchemasGyear => 0x1,
        XmlSchemaValType::XmlSchemasGmonth => 0x2,
        XmlSchemaValType::XmlSchemasGday => 0x3,
        XmlSchemaValType::XmlSchemasGyearmonth => 0x3,
        XmlSchemaValType::XmlSchemasGmonthday => 0x6,
        XmlSchemaValType::XmlSchemasTime => 0x8,
        _ => 0,
    };

    let ymask = match (*y).typ {
        XmlSchemaValType::XmlSchemasDatetime => 0xf,
        XmlSchemaValType::XmlSchemasDate => 0x7,
        XmlSchemaValType::XmlSchemasGyear => 0x1,
        XmlSchemaValType::XmlSchemasGmonth => 0x2,
        XmlSchemaValType::XmlSchemasGday => 0x3,
        XmlSchemaValType::XmlSchemasGyearmonth => 0x3,
        XmlSchemaValType::XmlSchemasGmonthday => 0x6,
        XmlSchemaValType::XmlSchemasTime => 0x8,
        _ => 0,
    };

    let xor_mask: c_uchar = xmask ^ ymask; /* mark type differences */
    let and_mask: c_uchar = xmask & ymask; /* mark field specification */

    /* year */
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

    /* month */
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

    /* day */
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

    /* time */
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

/**
 * xmlSchemaComparePreserveReplaceStrings:
 * @x:  a first string value
 * @y:  a second string value
 * @invert: inverts the result if x < y or x > y.
 *
 * Compare 2 string for their normalized values.
 * @x is a string with whitespace of "preserve", @y is
 * a string with a whitespace of "replace". I.e. @x could
 * be an "xsd:string" and @y an "xsd:normalizedString".
 *
 * Returns -1 if x < y, 0 if x == y, 1 if x > y, and -2 in
 * case of error
 */
unsafe extern "C" fn xml_schema_compare_preserve_replace_strings(
    mut x: *const XmlChar,
    mut y: *const XmlChar,
    invert: c_int,
) -> c_int {
    let mut tmp: c_int;

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

/**
 * xmlSchemaComparePreserveCollapseStrings:
 * @x:  a first string value
 * @y:  a second string value
 *
 * Compare 2 string for their normalized values.
 * @x is a string with whitespace of "preserve", @y is
 * a string with a whitespace of "collapse". I.e. @x could
 * be an "xsd:string" and @y an "xsd:normalizedString".
 *
 * Returns -1 if x < y, 0 if x == y, 1 if x > y, and -2 in
 * case of error
 */
unsafe extern "C" fn xml_schema_compare_replace_collapse_strings(
    mut x: *const XmlChar,
    mut y: *const XmlChar,
    invert: c_int,
) -> c_int {
    let mut tmp: c_int;

    /*
     * Skip leading blank chars of the collapsed string.
     */
    while IS_WSP_BLANK_CH!(*y) {
        y = y.add(1);
    }

    while *x != 0 && *y != 0 {
        if IS_WSP_BLANK_CH!(*y) {
            if !IS_WSP_BLANK_CH!(*x) {
                /*
                 * The yv character would have been replaced to 0x20.
                 */
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
            /*
             * Skip contiguous blank chars of the collapsed string.
             */
            while IS_WSP_BLANK_CH!(*y) {
                y = y.add(1);
            }
        } else {
            if IS_WSP_BLANK_CH!(*x) {
                /*
                 * The xv character would have been replaced to 0x20.
                 */
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
        /*
         * Skip trailing blank chars of the collapsed string.
         */
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

/**
 * xmlSchemaComparePreserveCollapseStrings:
 * @x:  a first string value
 * @y:  a second string value
 *
 * Compare 2 string for their normalized values.
 * @x is a string with whitespace of "preserve", @y is
 * a string with a whitespace of "collapse". I.e. @x could
 * be an "xsd:string" and @y an "xsd:normalizedString".
 *
 * Returns -1 if x < y, 0 if x == y, 1 if x > y, and -2 in
 * case of error
 */
unsafe extern "C" fn xml_schema_compare_preserve_collapse_strings(
    mut x: *const XmlChar,
    mut y: *const XmlChar,
    invert: c_int,
) -> c_int {
    let mut tmp: c_int;

    /*
     * Skip leading blank chars of the collapsed string.
     */
    while IS_WSP_BLANK_CH!(*y) {
        y = y.add(1);
    }

    while *x != 0 && *y != 0 {
        if IS_WSP_BLANK_CH!(*y) {
            if !IS_WSP_SPACE_CH!(*x) {
                /*
                 * The yv character would have been replaced to 0x20.
                 */
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
            /*
             * Skip contiguous blank chars of the collapsed string.
             */
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
        /*
         * Skip trailing blank chars of the collapsed string.
         */
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

/**
 * xmlSchemaCompareReplacedStrings:
 * @x:  a first string value
 * @y:  a second string value
 *
 * Compare 2 string for their normalized values.
 *
 * Returns -1 if x < y, 0 if x == y, 1 if x > y, and -2 in
 * case of error
 */
unsafe extern "C" fn xml_schema_compare_replaced_strings(
    mut x: *const XmlChar,
    mut y: *const XmlChar,
) -> c_int {
    let mut tmp: c_int;

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

/**
 * xmlSchemaCompareNormStrings:
 * @x:  a first string value
 * @y:  a second string value
 *
 * Compare 2 string for their normalized values.
 *
 * Returns -1 if x < y, 0 if x == y, 1 if x > y, and -2 in
 * case of error
 */
unsafe extern "C" fn xml_schema_compare_norm_strings(
    mut x: *const XmlChar,
    mut y: *const XmlChar,
) -> c_int {
    let mut tmp: c_int;

    while IS_BLANK_CH!(*x) {
        x = x.add(1);
    }
    while IS_BLANK_CH!(*y) {
        y = y.add(1);
    }
    while *x != 0 && *y != 0 {
        if IS_BLANK_CH!(*x) {
            if !IS_BLANK_CH!(*y) {
                tmp = *x as i32 - *y as i32;
                return tmp;
            }
            while IS_BLANK_CH!(*x) {
                x = x.add(1);
            }
            while IS_BLANK_CH!(*y) {
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
        while IS_BLANK_CH!(*x) {
            x = x.add(1);
        }
        if *x != 0 {
            return 1;
        }
    }
    if *y != 0 {
        while IS_BLANK_CH!(*y) {
            y = y.add(1);
        }
        if *y != 0 {
            return -1;
        }
    }
    0
}

/**
 * xmlSchemaCompareFloats:
 * @x:  a first float or double value
 * @y:  a second float or double value
 *
 * Compare 2 values
 *
 * Returns -1 if x < y, 0 if x == y, 1 if x > y, 2 if x <> y, and -2 in
 * case of error
 */
unsafe extern "C" fn xml_schema_compare_floats(x: XmlSchemaValPtr, y: XmlSchemaValPtr) -> c_int {
    let d1: f64;
    let d2: f64;

    if x.is_null() || y.is_null() {
        return -2;
    }

    /*
     * Cast everything to doubles.
     */
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

    /*
     * Check for special cases.
     */
    if xml_xpath_is_nan(d1) != 0 {
        if xml_xpath_is_nan(d2) != 0 {
            return 0;
        }
        return 1;
    }
    if xml_xpath_is_nan(d2) != 0 {
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

    /*
     * basic tests, the last one we should have equality, but
     * portability is more important than speed and handling
     * NaN or Inf in a portable way is always a challenge, so ...
     */
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

/**
 * xmlSchemaCompareValues:
 * @x:  a first value
 * @xvalue: the first value as a string (optional)
 * @xwtsp: the whitespace type
 * @y:  a second value
 * @xvalue: the second value as a string (optional)
 * @ywtsp: the whitespace type
 *
 * Compare 2 values
 *
 * Returns -1 if x < y, 0 if x == y, 1 if x > y, 2 if x <> y, 3 if not
 * comparable and -2 in case of error
 */
unsafe extern "C" fn xml_schema_compare_values_internal(
    xtype: XmlSchemaValType,
    x: XmlSchemaValPtr,
    xvalue: *const XmlChar,
    xws: XmlSchemaWhitespaceValueType,
    ytype: XmlSchemaValType,
    y: XmlSchemaValPtr,
    yvalue: *const XmlChar,
    yws: XmlSchemaWhitespaceValueType,
) -> c_int {
    match xtype {
        XmlSchemaValType::XmlSchemasUnknown | XmlSchemaValType::XmlSchemasAnytype => {
            return -2;
        }
        XmlSchemaValType::XmlSchemasInteger
        | XmlSchemaValType::XmlSchemasNpinteger
        | XmlSchemaValType::XmlSchemasNinteger
        | XmlSchemaValType::XmlSchemasNninteger
        | XmlSchemaValType::XmlSchemasPinteger
        | XmlSchemaValType::XmlSchemasInt
        | XmlSchemaValType::XmlSchemasUint
        | XmlSchemaValType::XmlSchemasLong
        | XmlSchemaValType::XmlSchemasUlong
        | XmlSchemaValType::XmlSchemasShort
        | XmlSchemaValType::XmlSchemasUshort
        | XmlSchemaValType::XmlSchemasByte
        | XmlSchemaValType::XmlSchemasUbyte
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
                    | XmlSchemaValType::XmlSchemasNpinteger
                    | XmlSchemaValType::XmlSchemasNinteger
                    | XmlSchemaValType::XmlSchemasNninteger
                    | XmlSchemaValType::XmlSchemasPinteger
                    | XmlSchemaValType::XmlSchemasInt
                    | XmlSchemaValType::XmlSchemasUint
                    | XmlSchemaValType::XmlSchemasLong
                    | XmlSchemaValType::XmlSchemasUlong
                    | XmlSchemaValType::XmlSchemasShort
                    | XmlSchemaValType::XmlSchemasUshort
                    | XmlSchemaValType::XmlSchemasByte
                    | XmlSchemaValType::XmlSchemasUbyte
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
        | XmlSchemaValType::XmlSchemasGday
        | XmlSchemaValType::XmlSchemasGmonth
        | XmlSchemaValType::XmlSchemasGmonthday
        | XmlSchemaValType::XmlSchemasGyear
        | XmlSchemaValType::XmlSchemasGyearmonth
        | XmlSchemaValType::XmlSchemasDate
        | XmlSchemaValType::XmlSchemasDatetime => {
            if x.is_null() || y.is_null() {
                return -2;
            }
            if matches!(
                ytype,
                XmlSchemaValType::XmlSchemasDatetime
                    | XmlSchemaValType::XmlSchemasTime
                    | XmlSchemaValType::XmlSchemasGday
                    | XmlSchemaValType::XmlSchemasGmonth
                    | XmlSchemaValType::XmlSchemasGmonthday
                    | XmlSchemaValType::XmlSchemasGyear
                    | XmlSchemaValType::XmlSchemasDate
                    | XmlSchemaValType::XmlSchemasGyearmonth
            ) {
                return xml_schema_compare_dates(x, y);
            }
            return -2;
        }
        /*
         * Note that we will support comparison of string types against
         * anySimpleType as well.
         */
        XmlSchemaValType::XmlSchemasAnysimpletype
        | XmlSchemaValType::XmlSchemasString
        | XmlSchemaValType::XmlSchemasNormstring
        | XmlSchemaValType::XmlSchemasToken
        | XmlSchemaValType::XmlSchemasLanguage
        | XmlSchemaValType::XmlSchemasNmtoken
        | XmlSchemaValType::XmlSchemasName
        | XmlSchemaValType::XmlSchemasNcname
        | XmlSchemaValType::XmlSchemasId
        | XmlSchemaValType::XmlSchemasIdref
        | XmlSchemaValType::XmlSchemasEntity
        | XmlSchemaValType::XmlSchemasAnyuri => {
            let xv = if x.is_null() { xvalue } else { (*x).value.str };
            let yv = if y.is_null() { yvalue } else { (*y).value.str };
            /*
             * TODO: Compare those against QName.
             */
            if ytype == XmlSchemaValType::XmlSchemasQname {
                // todo!();
                if y.is_null() {
                    return -2;
                }
                return -2;
            }
            if matches!(
                ytype,
                XmlSchemaValType::XmlSchemasAnysimpletype
                    | XmlSchemaValType::XmlSchemasString
                    | XmlSchemaValType::XmlSchemasNormstring
                    | XmlSchemaValType::XmlSchemasToken
                    | XmlSchemaValType::XmlSchemasLanguage
                    | XmlSchemaValType::XmlSchemasNmtoken
                    | XmlSchemaValType::XmlSchemasName
                    | XmlSchemaValType::XmlSchemasNcname
                    | XmlSchemaValType::XmlSchemasId
                    | XmlSchemaValType::XmlSchemasIdref
                    | XmlSchemaValType::XmlSchemasEntity
                    | XmlSchemaValType::XmlSchemasAnyuri
            ) {
                if xws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespacePreserve {
                    if yws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespacePreserve {
                        /* TODO: What about x < y or x > y. */
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
        XmlSchemaValType::XmlSchemasQname | XmlSchemaValType::XmlSchemasNotation => {
            if x.is_null() || y.is_null() {
                return -2;
            }
            if matches!(
                ytype,
                XmlSchemaValType::XmlSchemasQname | XmlSchemaValType::XmlSchemasNotation
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
                        let ret: c_int = xml_strcmp((*x).value.hex.str, (*y).value.hex.str);
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
                        let ret: c_int = xml_strcmp((*x).value.base64.str, (*y).value.base64.str);
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
        XmlSchemaValType::XmlSchemasIdrefs
        | XmlSchemaValType::XmlSchemasEntities
        | XmlSchemaValType::XmlSchemasNmtokens => {
            // todo!()
        }
    }
    -2
}

/**
 * xmlSchemaCompareValuesWhtspExt:
 * @x:  a first value
 * @xws: the whitespace value of x
 * @y:  a second value
 * @yws: the whitespace value of y
 *
 * Compare 2 values
 *
 * Returns -1 if x < y, 0 if x == y, 1 if x > y, 2 if x <> y, and -2 in
 * case of error
 */
unsafe extern "C" fn xml_schema_compare_values_whtsp_ext(
    xtype: XmlSchemaValType,
    x: XmlSchemaValPtr,
    xvalue: *const XmlChar,
    xws: XmlSchemaWhitespaceValueType,
    ytype: XmlSchemaValType,
    y: XmlSchemaValPtr,
    yvalue: *const XmlChar,
    yws: XmlSchemaWhitespaceValueType,
) -> c_int {
    xml_schema_compare_values_internal(xtype, x, xvalue, xws, ytype, y, yvalue, yws)
}

/**
 * xmlSchemaNormLen:
 * @value:  a string
 *
 * Computes the UTF8 length of the normalized value of the string
 *
 * Returns the length or -1 in case of error.
 */
unsafe extern "C" fn xml_schema_norm_len(value: *const XmlChar) -> c_int {
    let mut utf: *const XmlChar;
    let mut ret: c_int = 0;

    if value.is_null() {
        return -1;
    }
    utf = value;
    while IS_BLANK_CH!(*utf) {
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
        } else if IS_BLANK_CH!(*utf) {
            while IS_BLANK_CH!(*utf) {
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

/**
 * xmlSchemaValidateFacetInternal:
 * @facet:  the facet to check
 * @fws: the whitespace type of the facet's value
 * @valType: the built-in type of the value
 * @value:  the lexical repr of the value to validate
 * @val:  the precomputed value
 * @ws: the whitespace type of the value
 *
 * Check a value against a facet condition
 *
 * Returns 0 if the element is schemas valid, a positive error code
 *     number otherwise and -1 in case of internal or API error.
 */
unsafe extern "C" fn xml_schema_validate_facet_internal(
    facet: XmlSchemaFacetPtr,
    fws: XmlSchemaWhitespaceValueType,
    val_type: XmlSchemaValType,
    mut value: *const XmlChar,
    val: XmlSchemaValPtr,
    ws: XmlSchemaWhitespaceValueType,
) -> c_int {
    let ret: c_int;

    if facet.is_null() {
        return -1;
    }

    match (*facet).typ {
        XmlSchemaTypeType::XmlSchemaFacetPattern => {
            /*
             * NOTE that for patterns, the @value needs to be the normalized
             * value, *not* the lexical initial value or the canonical value.
             */
            if value.is_null() {
                return -1;
            }
            /*
             * If string-derived type, regexp must be tested on the value space of
             * the datatype.
             * See https://www.w3.org/TR/xmlschema-2/#rf-pattern
             */
            if !val.is_null()
                && !(*val).value.str.is_null()
                && (((*val).typ as i32 >= XmlSchemaValType::XmlSchemasString as i32
                    && (*val).typ as i32 <= XmlSchemaValType::XmlSchemasNormstring as i32)
                    || ((*val).typ as i32 >= XmlSchemaValType::XmlSchemasToken as i32
                        && (*val).typ as i32 <= XmlSchemaValType::XmlSchemasEntities as i32
                        && (*val).typ as i32 != XmlSchemaValType::XmlSchemasQname as i32))
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
        XmlSchemaTypeType::XmlSchemaFacetMaxexclusive => {
            ret = xml_schema_compare_values(val, (*facet).val);
            if ret == -2 {
                return -1;
            }
            if ret == -1 {
                return 0;
            }
            return XmlParserErrors::XmlSchemavCvcMaxExclusiveValid as i32;
        }
        XmlSchemaTypeType::XmlSchemaFacetMaxinclusive => {
            ret = xml_schema_compare_values(val, (*facet).val);
            if ret == -2 {
                return -1;
            }
            if ret == -1 || ret == 0 {
                return 0;
            }
            return XmlParserErrors::XmlSchemavCvcMaxInclusiveValid as i32;
        }
        XmlSchemaTypeType::XmlSchemaFacetMinexclusive => {
            ret = xml_schema_compare_values(val, (*facet).val);
            if ret == -2 {
                return -1;
            }
            if ret == 1 {
                return 0;
            }
            return XmlParserErrors::XmlSchemavCvcMinExclusiveValid as i32;
        }
        XmlSchemaTypeType::XmlSchemaFacetMininclusive => {
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
            /* TODO whitespaces */
            /*
             * NOTE: Whitespace should be handled to normalize
             * the value to be validated against a the facets;
             * not to normalize the value in-between.
             */
            return 0;
        }
        XmlSchemaTypeType::XmlSchemaFacetEnumeration => {
            if ws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceUnknown {
                /*
                 * This is to ensure API compatibility with the old
                 * xmlSchemaValidateFacet().
                 * TODO: Get rid of this case.
                 */
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
        | ty @ XmlSchemaTypeType::XmlSchemaFacetMaxlength
        | ty @ XmlSchemaTypeType::XmlSchemaFacetMinlength => {
            if matches!(ty, XmlSchemaTypeType::XmlSchemaFacetLength) {
                /*
                	* SPEC (1.3) "if {primitive type definition} is QName or NOTATION,
                	* then any {value} is facet-valid."
                	*/
                if matches!(
                    val_type,
                    XmlSchemaValType::XmlSchemasQname | XmlSchemaValType::XmlSchemasNotation
                ) {
                    return 0;
                }
            }

            let mut len: c_uint = 0;

            if matches!(
                val_type,
                XmlSchemaValType::XmlSchemasQname | XmlSchemaValType::XmlSchemasNotation
            ) {
                return 0;
            }
            /*
             * TODO: length, maxLength and minLength must be of type
             * nonNegativeInteger only. Check if decimal is used somehow.
             */
            if (*facet).val.is_null()
                || !matches!(
                    (*(*facet).val).typ,
                    XmlSchemaValType::XmlSchemasDecimal | XmlSchemaValType::XmlSchemasNninteger
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
                    XmlSchemaValType::XmlSchemasString | XmlSchemaValType::XmlSchemasNormstring => {
                        if ws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceUnknown {
                            /*
                            	* This is to ensure API compatibility with the old
                            	* xmlSchemaValidateFacet(). Anyway, this was and
                            	* is not the correct handling.
                            	* TODO: Get rid of this case somehow.
                            	*/
                            if val_type == XmlSchemaValType::XmlSchemasString {
                                len = xml_utf8_strlen(value) as _;
                            } else {
                                len = xml_schema_norm_len(value) as _;
                            }
                        } else if !value.is_null() {
                            if ws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse {
                                len = xml_schema_norm_len(value) as _;
                            } else {
                                /*
                                	* Should be OK for "preserve" as well.
                                	*/
                                len = xml_utf8_strlen(value) as _;
                            }
                        }
                    }
                    XmlSchemaValType::XmlSchemasIdref
                    | XmlSchemaValType::XmlSchemasToken
                    | XmlSchemaValType::XmlSchemasLanguage
                    | XmlSchemaValType::XmlSchemasNmtoken
                    | XmlSchemaValType::XmlSchemasName
                    | XmlSchemaValType::XmlSchemasNcname
                    | XmlSchemaValType::XmlSchemasId
                    | XmlSchemaValType::XmlSchemasAnyuri => {
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
            } else if (*facet).typ == XmlSchemaTypeType::XmlSchemaFacetMinlength {
                if (len as u64) < (*(*facet).val).value.decimal.lo {
                    return XmlParserErrors::XmlSchemavCvcMinLengthValid as i32;
                }
            } else if len as u64 > (*(*facet).val).value.decimal.lo {
                return XmlParserErrors::XmlSchemavCvcMaxLengthValid as i32;
            }
        }
        XmlSchemaTypeType::XmlSchemaFacetTotaldigits
        | XmlSchemaTypeType::XmlSchemaFacetFractiondigits => {
            if (*facet).val.is_null()
                || !matches!(
                    (*(*facet).val).typ,
                    XmlSchemaValType::XmlSchemasPinteger | XmlSchemaValType::XmlSchemasNninteger
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
                        | XmlSchemaValType::XmlSchemasNpinteger
                        | XmlSchemaValType::XmlSchemasNinteger
                        | XmlSchemaValType::XmlSchemasNninteger
                        | XmlSchemaValType::XmlSchemasPinteger
                        | XmlSchemaValType::XmlSchemasInt
                        | XmlSchemaValType::XmlSchemasUint
                        | XmlSchemaValType::XmlSchemasLong
                        | XmlSchemaValType::XmlSchemasUlong
                        | XmlSchemaValType::XmlSchemasShort
                        | XmlSchemaValType::XmlSchemasUshort
                        | XmlSchemaValType::XmlSchemasByte
                        | XmlSchemaValType::XmlSchemasUbyte
                )
            {
                return -1;
            }
            if (*facet).typ == XmlSchemaTypeType::XmlSchemaFacetTotaldigits {
                if (*val).value.decimal.total as u64 > (*(*facet).val).value.decimal.lo {
                    return XmlParserErrors::XmlSchemavCvcTotalDigitsValid as i32;
                }
            } else if (*facet).typ == XmlSchemaTypeType::XmlSchemaFacetFractiondigits
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

/**
 * xmlSchemaValidateFacet:
 * @base:  the base type
 * @facet:  the facet to check
 * @value:  the lexical repr of the value to validate
 * @val:  the precomputed value
 *
 * Check a value against a facet condition
 *
 * Returns 0 if the element is schemas valid, a positive error code
 *     number otherwise and -1 in case of internal or API error.
 */
pub unsafe extern "C" fn xml_schema_validate_facet(
    base: XmlSchemaTypePtr,
    facet: XmlSchemaFacetPtr,
    value: *const XmlChar,
    val: XmlSchemaValPtr,
) -> c_int {
    /*
     * This tries to ensure API compatibility regarding the old
     * xmlSchemaValidateFacet() and the new xmlSchemaValidateFacetInternal() and
     * xmlSchemaValidateFacetWhtsp().
     */
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

/**
 * xmlSchemaValidateFacetWhtsp:
 * @facet:  the facet to check
 * @fws: the whitespace type of the facet's value
 * @valType: the built-in type of the value
 * @value:  the lexical (or normalized for pattern) repr of the value to validate
 * @val:  the precomputed value
 * @ws: the whitespace type of the value
 *
 * Check a value against a facet condition. This takes value normalization
 * according to the specified whitespace types into account.
 * Note that @value needs to be the *normalized* value if the facet
 * is of type "pattern".
 *
 * Returns 0 if the element is schemas valid, a positive error code
 *     number otherwise and -1 in case of internal or API error.
 */
pub unsafe extern "C" fn xml_schema_validate_facet_whtsp(
    facet: XmlSchemaFacetPtr,
    fws: XmlSchemaWhitespaceValueType,
    val_type: XmlSchemaValType,
    value: *const XmlChar,
    val: XmlSchemaValPtr,
    ws: XmlSchemaWhitespaceValueType,
) -> c_int {
    xml_schema_validate_facet_internal(facet, fws, val_type, value, val, ws)
}

/**
 * xmlSchemaFreeValue:
 * @value:  the value to free
 *
 * Cleanup the default XML Schemas type library
 */
pub unsafe extern "C" fn xml_schema_free_value(mut value: XmlSchemaValPtr) {
    let mut prev: XmlSchemaValPtr;

    while !value.is_null() {
        match (*value).typ {
            XmlSchemaValType::XmlSchemasString
            | XmlSchemaValType::XmlSchemasNormstring
            | XmlSchemaValType::XmlSchemasToken
            | XmlSchemaValType::XmlSchemasLanguage
            | XmlSchemaValType::XmlSchemasNmtoken
            | XmlSchemaValType::XmlSchemasNmtokens
            | XmlSchemaValType::XmlSchemasName
            | XmlSchemaValType::XmlSchemasNcname
            | XmlSchemaValType::XmlSchemasId
            | XmlSchemaValType::XmlSchemasIdref
            | XmlSchemaValType::XmlSchemasIdrefs
            | XmlSchemaValType::XmlSchemasEntity
            | XmlSchemaValType::XmlSchemasEntities
            | XmlSchemaValType::XmlSchemasAnyuri
            | XmlSchemaValType::XmlSchemasAnysimpletype => {
                if !(*value).value.str.is_null() {
                    xml_free((*value).value.str as _);
                }
            }
            XmlSchemaValType::XmlSchemasNotation | XmlSchemaValType::XmlSchemasQname => {
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

/**
 * xmlSchemaNewFacet:
 *
 * Allocate a new Facet structure.
 *
 * Returns the newly allocated structure or NULL in case or error
 */
pub unsafe extern "C" fn xml_schema_new_facet() -> XmlSchemaFacetPtr {
    let ret: XmlSchemaFacetPtr = xml_malloc(size_of::<XmlSchemaFacet>()) as _;
    if ret.is_null() {
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlSchemaFacet>());

    ret
}

// macro_rules! VERROR {
//     ($vctxt:expr, $err:expr, $typ:expr, $msg:expr) => {
//         xmlSchemaCustomErr(
//             $vctxt as XmlSchemaAbstractCtxtPtr,
//             $err,
//             null_mut(),
//             $typ,
//             $msg,
//             null_mut(),
//             null_mut(),
//         );
//     };
// }

// macro_rules! VERROR_INT {
//     ($vctxt:expr, $func:expr, $msg:expr) => {
//         $crate::libxml::xmlschemas::xml_schema_internal_err(
//             $vctxt as XmlSchemaAbstractCtxtPtr,
//             $func,
//             $msg,
//         );
//     };
// }

macro_rules! PERROR_INT {
    ($pctxt:expr, $func:expr, $msg:expr) => {
        $crate::libxml::xmlschemas::xml_schema_internal_err(
            $pctxt as XmlSchemaAbstractCtxtPtr,
            $func,
            $msg,
        );
    };
}
// macro_rules! PERROR_INT2 {
//     ($ctxt:expr, $func:expr, $msg:expr) => {
//         $crate::libxml::xmlschemas::xml_schema_internal_err(
//             $ctxt as XmlSchemaAbstractCtxtPtr,
//             $func,
//             $msg,
//         );
//     };
// }

// macro_rules! AERROR_INT {
//     ($actxt:expr, $func:expr, $msg:expr) => {
//         $crate::libxml::xmlschemas::xml_schema_internal_err($actxt, $func, $msg);
//     };
// }

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

/**
 * xmlSchemaCheckFacet:
 * @facet:  the facet
 * @typeDecl:  the schema type definition
 * @pctxt:  the schema parser context or NULL
 * @name: the optional name of the type
 *
 * Checks and computes the values of facets.
 *
 * Returns 0 if valid, a positive error code if not valid and
 *         -1 in case of an internal or API error.
 */
pub unsafe extern "C" fn xml_schema_check_facet(
    facet: XmlSchemaFacetPtr,
    type_decl: XmlSchemaTypePtr,
    mut pctxt: XmlSchemaParserCtxtPtr,
    _name: *const XmlChar,
) -> c_int {
    let mut ret: c_int = 0;

    if facet.is_null() || type_decl.is_null() {
        return -1;
    }
    /*
     * TODO: will the parser context be given if used from
     * the relaxNG module?
     */
    let ctxt_given = if pctxt.is_null() { 0 } else { 1 };

    match (*facet).typ {
        XmlSchemaTypeType::XmlSchemaFacetMininclusive
        | XmlSchemaTypeType::XmlSchemaFacetMinexclusive
        | XmlSchemaTypeType::XmlSchemaFacetMaxinclusive
        | XmlSchemaTypeType::XmlSchemaFacetMaxexclusive
        | XmlSchemaTypeType::XmlSchemaFacetEnumeration => {
            /*
             * Okay we need to validate the value
             * at that point.
             */
            let base: XmlSchemaTypePtr;

            /* 4.3.5.5 Constraints on enumeration Schema Components
             * Schema Component Constraint: enumeration valid restriction
             * It is an `error` if any member of {value} is not in the
             * `value space` of {base type definition}.
             *
             * minInclusive, maxInclusive, minExclusive, maxExclusive:
             * The value `must` be in the
             * `value space` of the `base type`.
             */
            /*
             * This function is intended to deliver a compiled value
             * on the facet. In this implementation of XML Schemata the
             * type holding a facet, won't be a built-in type.
             * Thus to ensure that other API
             * calls (relaxng) do work, if the given type is a built-in
             * type, we will assume that the given built-in type *is
             * already* the base type.
             */
            if (*type_decl).typ != XmlSchemaTypeType::XmlSchemaTypeBasic {
                base = (*type_decl).base_type;
                if base.is_null() {
                    PERROR_INT!(
                        pctxt,
                        c"xmlSchemaCheckFacet".as_ptr() as _,
                        c"a type user derived type has no base type".as_ptr() as _
                    );
                    return -1;
                }
            } else {
                base = type_decl;
            }

            if ctxt_given == 0 {
                /*
                 * A context is needed if called from RelaxNG.
                 */
                pctxt = xml_schema_new_parser_ctxt(c"*".as_ptr() as _);
                if pctxt.is_null() {
                    return -1;
                }
            }
            /*
             * NOTE: This call does not check the content nodes,
             * since they are not available:
             * (*facet).node is just the node holding the facet
             * definition, *not* the attribute holding the *value*
             * of the facet.
             */
            ret = xml_schema_vcheck_cvc_simple_type(
                pctxt as XmlSchemaAbstractCtxtPtr,
                (*facet).node,
                base,
                (*facet).value,
                addr_of_mut!((*facet).val),
                1,
                1,
                0,
            );
            if ret != 0 {
                if ret < 0 {
                    /* No error message for RelaxNG. */
                    if ctxt_given != 0 {
                        xml_schema_custom_err(pctxt as XmlSchemaAbstractCtxtPtr, XmlParserErrors::XmlSchemapInternal, (*facet).node, null_mut(), c"Internal error: xmlSchemaCheckFacet, failed to validate the value '%s' of the facet '%s' against the base type".as_ptr() as _, (*facet).value, xml_schema_facet_type_to_string((*facet).typ));
                    }
                    // goto internal_error;
                    if ctxt_given == 0 && !pctxt.is_null() {
                        xml_schema_free_parser_ctxt(pctxt);
                    }
                    return -1;
                }
                ret = XmlParserErrors::XmlSchemapInvalidFacetValue as i32;
                /* No error message for RelaxNG. */
                if ctxt_given != 0 {
                    let mut str: *mut XmlChar = null_mut();

                    xml_schema_custom_err(
                        pctxt as XmlSchemaAbstractCtxtPtr,
                        ret.try_into().unwrap(),
                        (*facet).node,
                        facet as XmlSchemaBasicItemPtr,
                        c"The value '%s' of the facet does not validate against the base type '%s'"
                            .as_ptr() as _,
                        (*facet).value,
                        xml_schema_format_qname(
                            addr_of_mut!(str),
                            (*base).target_namespace,
                            (*base).name,
                        ),
                    );
                    FREE_AND_NULL!(str);
                }
                // goto exit;
                if ctxt_given == 0 && !pctxt.is_null() {
                    xml_schema_free_parser_ctxt(pctxt);
                }
                return ret;
            } else if (*facet).val.is_null() && ctxt_given != 0 {
                PERROR_INT!(
                    pctxt,
                    c"xmlSchemaCheckFacet".as_ptr() as _,
                    c"value was not computed".as_ptr() as _
                );
            }
        }
        XmlSchemaTypeType::XmlSchemaFacetPattern => {
            (*facet).regexp = xml_regexp_compile((*facet).value);
            if (*facet).regexp.is_null() {
                ret = XmlParserErrors::XmlSchemapRegexpInvalid as i32;
                /* No error message for RelaxNG. */
                if ctxt_given != 0 {
                    xml_schema_custom_err(
                        pctxt as XmlSchemaAbstractCtxtPtr,
                        ret.try_into().unwrap(),
                        (*facet).node,
                        type_decl as XmlSchemaBasicItemPtr,
                        c"The value '%s' of the facet 'pattern' is not a valid regular expression"
                            .as_ptr() as _,
                        (*facet).value,
                        null(),
                    );
                }
            }
        }
        XmlSchemaTypeType::XmlSchemaFacetTotaldigits
        | XmlSchemaTypeType::XmlSchemaFacetFractiondigits
        | XmlSchemaTypeType::XmlSchemaFacetLength
        | XmlSchemaTypeType::XmlSchemaFacetMaxlength
        | XmlSchemaTypeType::XmlSchemaFacetMinlength => {
            if (*facet).typ == XmlSchemaTypeType::XmlSchemaFacetTotaldigits {
                ret = xml_schema_validate_predefined_type(
                    xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasPinteger),
                    (*facet).value,
                    addr_of_mut!((*facet).val),
                );
            } else {
                ret = xml_schema_validate_predefined_type(
                    xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasNninteger),
                    (*facet).value,
                    addr_of_mut!((*facet).val),
                );
            }
            if ret != 0 {
                if ret < 0 {
                    /* No error message for RelaxNG. */
                    if ctxt_given != 0 {
                        PERROR_INT!(
                            pctxt,
                            c"xmlSchemaCheckFacet".as_ptr() as _,
                            c"validating facet value".as_ptr() as _
                        );
                    }
                    // goto internal_error;
                    if ctxt_given == 0 && !pctxt.is_null() {
                        xml_schema_free_parser_ctxt(pctxt);
                    }
                    return -1;
                }
                ret = XmlParserErrors::XmlSchemapInvalidFacetValue as i32;
                /* No error message for RelaxNG. */
                if ctxt_given != 0 {
                    /* error code */
                    xml_schema_custom_err4(
                        pctxt as XmlSchemaAbstractCtxtPtr,
                        ret.try_into().unwrap(),
                        (*facet).node,
                        type_decl as XmlSchemaBasicItemPtr,
                        c"The value '%s' of the facet '%s' is not a valid '%s'".as_ptr() as _,
                        (*facet).value,
                        xml_schema_facet_type_to_string((*facet).typ),
                        if (*facet).typ != XmlSchemaTypeType::XmlSchemaFacetTotaldigits {
                            c"nonNegativeInteger".as_ptr() as _
                        } else {
                            c"positiveInteger".as_ptr() as _
                        },
                        null_mut(),
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
                /* No error message for RelaxNG. */
                if ctxt_given != 0 {
                    /* error was previously: XML_SCHEMAP_INVALID_WHITE_SPACE */
                    xml_schema_custom_err(
                        pctxt as XmlSchemaAbstractCtxtPtr,
                        ret.try_into().unwrap(),
                        (*facet).node,
                        type_decl as XmlSchemaBasicItemPtr,
                        c"The value '%s' of the facet 'whitespace' is not valid".as_ptr() as _,
                        (*facet).value,
                        null_mut(),
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

/**
 * xmlSchemaFreeFacet:
 * @facet:  a schema facet structure
 *
 * Deallocate a Schema Facet structure.
 */
pub unsafe extern "C" fn xml_schema_free_facet(facet: XmlSchemaFacetPtr) {
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

/**
 * xmlSchemaCompareValues:
 * @x:  a first value
 * @y:  a second value
 *
 * Compare 2 values
 *
 * Returns -1 if x < y, 0 if x == y, 1 if x > y, 2 if x <> y, and -2 in
 * case of error
 */
pub unsafe extern "C" fn xml_schema_compare_values(
    x: XmlSchemaValPtr,
    y: XmlSchemaValPtr,
) -> c_int {
    if x.is_null() || y.is_null() {
        return -2;
    }
    let xws = if (*x).typ == XmlSchemaValType::XmlSchemasString {
        XmlSchemaWhitespaceValueType::XmlSchemaWhitespacePreserve
    } else if (*x).typ == XmlSchemaValType::XmlSchemasNormstring {
        XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceReplace
    } else {
        XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse
    };

    let yws = if (*y).typ == XmlSchemaValType::XmlSchemasString {
        XmlSchemaWhitespaceValueType::XmlSchemaWhitespacePreserve
    } else if (*y).typ == XmlSchemaValType::XmlSchemasNormstring {
        XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceReplace
    } else {
        XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse
    };

    xml_schema_compare_values_internal((*x).typ, x, null_mut(), xws, (*y).typ, y, null_mut(), yws)
}

/**
 * xmlSchemaGetBuiltInListSimpleTypeItemType:
 * @type: the built-in simple type.
 *
 * Lookup function
 *
 * Returns the item type of @type as defined by the built-in datatype
 * hierarchy of XML Schema Part 2: Datatypes, or NULL in case of an error.
 */
pub unsafe extern "C" fn xml_schema_get_built_in_list_simple_type_item_type(
    typ: XmlSchemaTypePtr,
) -> XmlSchemaTypePtr {
    if typ.is_null() || (*typ).typ != XmlSchemaTypeType::XmlSchemaTypeBasic {
        return null_mut();
    }
    match XmlSchemaValType::try_from((*typ).built_in_type) {
        Ok(XmlSchemaValType::XmlSchemasNmtokens) => {
            XML_SCHEMA_TYPE_NMTOKEN_DEF.load(Ordering::Relaxed)
        }
        Ok(XmlSchemaValType::XmlSchemasIdrefs) => XML_SCHEMA_TYPE_IDREF_DEF.load(Ordering::Relaxed),
        Ok(XmlSchemaValType::XmlSchemasEntities) => {
            XML_SCHEMA_TYPE_ENTITY_DEF.load(Ordering::Relaxed)
        }
        _ => null_mut(),
    }
}

/**
 * xmlSchemaValidateListSimpleTypeFacet:
 * @facet:  the facet to check
 * @value:  the lexical repr of the value to validate
 * @actualLen:  the number of list items
 * @expectedLen: the resulting expected number of list items
 *
 * Checks the value of a list simple type against a facet.
 *
 * Returns 0 if the value is valid, a positive error code
 * number otherwise and -1 in case of an internal error.
 */
pub unsafe extern "C" fn xml_schema_validate_list_simple_type_facet(
    facet: XmlSchemaFacetPtr,
    value: *const XmlChar,
    actual_len: c_ulong,
    expected_len: *mut c_ulong,
) -> c_int {
    if facet.is_null() {
        return -1;
    }
    /*
     * TODO: Check if this will work with large numbers.
     * (compare value.decimal.mi and value.decimal.hi as well?).
     */
    if (*facet).typ == XmlSchemaTypeType::XmlSchemaFacetLength {
        if actual_len != (*(*facet).val).value.decimal.lo {
            if !expected_len.is_null() {
                *expected_len = (*(*facet).val).value.decimal.lo;
            }
            return XmlParserErrors::XmlSchemavCvcLengthValid as i32;
        }
    } else if (*facet).typ == XmlSchemaTypeType::XmlSchemaFacetMinlength {
        if actual_len < (*(*facet).val).value.decimal.lo {
            if !expected_len.is_null() {
                *expected_len = (*(*facet).val).value.decimal.lo;
            }
            return XmlParserErrors::XmlSchemavCvcMinLengthValid as i32;
        }
    } else if (*facet).typ == XmlSchemaTypeType::XmlSchemaFacetMaxlength {
        if actual_len > (*(*facet).val).value.decimal.lo {
            if !expected_len.is_null() {
                *expected_len = (*(*facet).val).value.decimal.lo;
            }
            return XmlParserErrors::XmlSchemavCvcMaxLengthValid as i32;
        }
    } else {
        /*
         * NOTE: That we can pass NULL as xmlSchemaValPtr to
         * xmlSchemaValidateFacet, since the remaining facet types
         * are: xmlSchemaTypeType::XML_SCHEMA_FACET_PATTERN.as_ptr() as _, xmlSchemaTypeType::XML_SCHEMA_FACET_ENUMERATION.
         */
        return xml_schema_validate_facet(null_mut(), facet, value, null_mut());
    }
    0
}

/**
 * xmlSchemaGetBuiltInType:
 * @type:  the type of the built in type
 *
 * Gives you the type struct for a built-in
 * type by its type id.
 *
 * Returns the type if found, NULL otherwise.
 */
pub unsafe extern "C" fn xml_schema_get_built_in_type(typ: XmlSchemaValType) -> XmlSchemaTypePtr {
    if !XML_SCHEMA_TYPES_INITIALIZED.load(Ordering::Relaxed) && xml_schema_init_types() < 0 {
        return null_mut();
    }
    match typ {
        XmlSchemaValType::XmlSchemasAnysimpletype => {
            XML_SCHEMA_TYPE_ANY_SIMPLE_TYPE_DEF.load(Ordering::Relaxed)
        }
        XmlSchemaValType::XmlSchemasString => XML_SCHEMA_TYPE_STRING_DEF.load(Ordering::Relaxed),
        XmlSchemaValType::XmlSchemasNormstring => {
            XML_SCHEMA_TYPE_NORM_STRING_DEF.load(Ordering::Relaxed)
        }
        XmlSchemaValType::XmlSchemasDecimal => XML_SCHEMA_TYPE_DECIMAL_DEF.load(Ordering::Relaxed),
        XmlSchemaValType::XmlSchemasTime => XML_SCHEMA_TYPE_TIME_DEF.load(Ordering::Relaxed),
        XmlSchemaValType::XmlSchemasGday => XML_SCHEMA_TYPE_GDAY_DEF.load(Ordering::Relaxed),
        XmlSchemaValType::XmlSchemasGmonth => XML_SCHEMA_TYPE_GMONTH_DEF.load(Ordering::Relaxed),
        XmlSchemaValType::XmlSchemasGmonthday => {
            XML_SCHEMA_TYPE_GMONTH_DAY_DEF.load(Ordering::Relaxed)
        }
        XmlSchemaValType::XmlSchemasGyear => XML_SCHEMA_TYPE_GYEAR_DEF.load(Ordering::Relaxed),
        XmlSchemaValType::XmlSchemasGyearmonth => {
            XML_SCHEMA_TYPE_GYEAR_MONTH_DEF.load(Ordering::Relaxed)
        }
        XmlSchemaValType::XmlSchemasDate => XML_SCHEMA_TYPE_DATE_DEF.load(Ordering::Relaxed),
        XmlSchemaValType::XmlSchemasDatetime => {
            XML_SCHEMA_TYPE_DATETIME_DEF.load(Ordering::Relaxed)
        }
        XmlSchemaValType::XmlSchemasDuration => {
            XML_SCHEMA_TYPE_DURATION_DEF.load(Ordering::Relaxed)
        }
        XmlSchemaValType::XmlSchemasFloat => XML_SCHEMA_TYPE_FLOAT_DEF.load(Ordering::Relaxed),
        XmlSchemaValType::XmlSchemasDouble => XML_SCHEMA_TYPE_DOUBLE_DEF.load(Ordering::Relaxed),
        XmlSchemaValType::XmlSchemasBoolean => XML_SCHEMA_TYPE_BOOLEAN_DEF.load(Ordering::Relaxed),
        XmlSchemaValType::XmlSchemasToken => XML_SCHEMA_TYPE_TOKEN_DEF.load(Ordering::Relaxed),
        XmlSchemaValType::XmlSchemasLanguage => {
            XML_SCHEMA_TYPE_LANGUAGE_DEF.load(Ordering::Relaxed)
        }
        XmlSchemaValType::XmlSchemasNmtoken => XML_SCHEMA_TYPE_NMTOKEN_DEF.load(Ordering::Relaxed),
        XmlSchemaValType::XmlSchemasNmtokens => {
            XML_SCHEMA_TYPE_NMTOKENS_DEF.load(Ordering::Relaxed)
        }
        XmlSchemaValType::XmlSchemasName => XML_SCHEMA_TYPE_NAME_DEF.load(Ordering::Relaxed),
        XmlSchemaValType::XmlSchemasQname => XML_SCHEMA_TYPE_QNAME_DEF.load(Ordering::Relaxed),
        XmlSchemaValType::XmlSchemasNcname => XML_SCHEMA_TYPE_NCNAME_DEF.load(Ordering::Relaxed),
        XmlSchemaValType::XmlSchemasId => XML_SCHEMA_TYPE_ID_DEF.load(Ordering::Relaxed),
        XmlSchemaValType::XmlSchemasIdref => XML_SCHEMA_TYPE_IDREF_DEF.load(Ordering::Relaxed),
        XmlSchemaValType::XmlSchemasIdrefs => XML_SCHEMA_TYPE_IDREFS_DEF.load(Ordering::Relaxed),
        XmlSchemaValType::XmlSchemasEntity => XML_SCHEMA_TYPE_ENTITY_DEF.load(Ordering::Relaxed),
        XmlSchemaValType::XmlSchemasEntities => {
            XML_SCHEMA_TYPE_ENTITIES_DEF.load(Ordering::Relaxed)
        }
        XmlSchemaValType::XmlSchemasNotation => {
            XML_SCHEMA_TYPE_NOTATION_DEF.load(Ordering::Relaxed)
        }
        XmlSchemaValType::XmlSchemasAnyuri => XML_SCHEMA_TYPE_ANY_URIDEF.load(Ordering::Relaxed),
        XmlSchemaValType::XmlSchemasInteger => XML_SCHEMA_TYPE_INTEGER_DEF.load(Ordering::Relaxed),
        XmlSchemaValType::XmlSchemasNpinteger => {
            XML_SCHEMA_TYPE_NON_POSITIVE_INTEGER_DEF.load(Ordering::Relaxed)
        }
        XmlSchemaValType::XmlSchemasNinteger => {
            XML_SCHEMA_TYPE_NEGATIVE_INTEGER_DEF.load(Ordering::Relaxed)
        }
        XmlSchemaValType::XmlSchemasNninteger => {
            XML_SCHEMA_TYPE_NON_NEGATIVE_INTEGER_DEF.load(Ordering::Relaxed)
        }
        XmlSchemaValType::XmlSchemasPinteger => {
            XML_SCHEMA_TYPE_POSITIVE_INTEGER_DEF.load(Ordering::Relaxed)
        }
        XmlSchemaValType::XmlSchemasInt => XML_SCHEMA_TYPE_INT_DEF.load(Ordering::Relaxed),
        XmlSchemaValType::XmlSchemasUint => {
            XML_SCHEMA_TYPE_UNSIGNED_INT_DEF.load(Ordering::Relaxed)
        }
        XmlSchemaValType::XmlSchemasLong => XML_SCHEMA_TYPE_LONG_DEF.load(Ordering::Relaxed),
        XmlSchemaValType::XmlSchemasUlong => {
            XML_SCHEMA_TYPE_UNSIGNED_LONG_DEF.load(Ordering::Relaxed)
        }
        XmlSchemaValType::XmlSchemasShort => XML_SCHEMA_TYPE_SHORT_DEF.load(Ordering::Relaxed),
        XmlSchemaValType::XmlSchemasUshort => {
            XML_SCHEMA_TYPE_UNSIGNED_SHORT_DEF.load(Ordering::Relaxed)
        }
        XmlSchemaValType::XmlSchemasByte => XML_SCHEMA_TYPE_BYTE_DEF.load(Ordering::Relaxed),
        XmlSchemaValType::XmlSchemasUbyte => {
            XML_SCHEMA_TYPE_UNSIGNED_BYTE_DEF.load(Ordering::Relaxed)
        }
        XmlSchemaValType::XmlSchemasHexbinary => {
            XML_SCHEMA_TYPE_HEX_BINARY_DEF.load(Ordering::Relaxed)
        }
        XmlSchemaValType::XmlSchemasBase64binary => {
            XML_SCHEMA_TYPE_BASE64_BINARY_DEF.load(Ordering::Relaxed)
        }
        XmlSchemaValType::XmlSchemasAnytype => XML_SCHEMA_TYPE_ANY_TYPE_DEF.load(Ordering::Relaxed),
        _ => null_mut(),
    }
}

/**
 * xmlSchemaIsBuiltInTypeFacet:
 * @type: the built-in type
 * @facetType:  the facet type
 *
 * Evaluates if a specific facet can be
 * used in conjunction with a type.
 *
 * Returns 1 if the facet can be used with the given built-in type,
 * 0 otherwise and -1 in case the type is not a built-in type.
 */
pub unsafe extern "C" fn xml_schema_is_built_in_type_facet(
    typ: XmlSchemaTypePtr,
    facet_type: c_int,
) -> c_int {
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
        | Ok(XmlSchemaValType::XmlSchemasQname)
        | Ok(XmlSchemaValType::XmlSchemasAnyuri)
        | Ok(XmlSchemaValType::XmlSchemasBase64binary)
        | Ok(XmlSchemaValType::XmlSchemasHexbinary) => {
            if facet_type == XmlSchemaTypeType::XmlSchemaFacetLength as i32
                || facet_type == XmlSchemaTypeType::XmlSchemaFacetMinlength as i32
                || facet_type == XmlSchemaTypeType::XmlSchemaFacetMaxlength as i32
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
            if facet_type == XmlSchemaTypeType::XmlSchemaFacetTotaldigits as i32
                || facet_type == XmlSchemaTypeType::XmlSchemaFacetFractiondigits as i32
                || facet_type == XmlSchemaTypeType::XmlSchemaFacetPattern as i32
                || facet_type == XmlSchemaTypeType::XmlSchemaFacetWhitespace as i32
                || facet_type == XmlSchemaTypeType::XmlSchemaFacetEnumeration as i32
                || facet_type == XmlSchemaTypeType::XmlSchemaFacetMaxinclusive as i32
                || facet_type == XmlSchemaTypeType::XmlSchemaFacetMaxexclusive as i32
                || facet_type == XmlSchemaTypeType::XmlSchemaFacetMininclusive as i32
                || facet_type == XmlSchemaTypeType::XmlSchemaFacetMinexclusive as i32
            {
                return 1;
            } else {
                return 0;
            }
        }
        Ok(XmlSchemaValType::XmlSchemasTime)
        | Ok(XmlSchemaValType::XmlSchemasGday)
        | Ok(XmlSchemaValType::XmlSchemasGmonth)
        | Ok(XmlSchemaValType::XmlSchemasGmonthday)
        | Ok(XmlSchemaValType::XmlSchemasGyear)
        | Ok(XmlSchemaValType::XmlSchemasGyearmonth)
        | Ok(XmlSchemaValType::XmlSchemasDate)
        | Ok(XmlSchemaValType::XmlSchemasDatetime)
        | Ok(XmlSchemaValType::XmlSchemasDuration)
        | Ok(XmlSchemaValType::XmlSchemasFloat)
        | Ok(XmlSchemaValType::XmlSchemasDouble) => {
            if facet_type == XmlSchemaTypeType::XmlSchemaFacetPattern as i32
                || facet_type == XmlSchemaTypeType::XmlSchemaFacetEnumeration as i32
                || facet_type == XmlSchemaTypeType::XmlSchemaFacetWhitespace as i32
                || facet_type == XmlSchemaTypeType::XmlSchemaFacetMaxinclusive as i32
                || facet_type == XmlSchemaTypeType::XmlSchemaFacetMaxexclusive as i32
                || facet_type == XmlSchemaTypeType::XmlSchemaFacetMininclusive as i32
                || facet_type == XmlSchemaTypeType::XmlSchemaFacetMinexclusive as i32
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

/**
 * xmlSchemaCollapseString:
 * @value: a value
 *
 * Removes and normalize white spaces in the string
 *
 * Returns the new string or NULL if no change was required.
 */
pub unsafe extern "C" fn xml_schema_collapse_string(value: *const XmlChar) -> *mut XmlChar {
    let mut start: *const XmlChar = value;
    let mut end: *const XmlChar;
    let f: *const XmlChar;
    let mut g: *mut XmlChar;
    let mut col: c_int = 0;

    if value.is_null() {
        return null_mut();
    }
    while *start != 0 && IS_BLANK_CH!(*start) {
        start = start.add(1);
    }
    end = start;
    while *end != 0 {
        if (*end == b' ' && IS_BLANK_CH!(*end.add(1)))
            || (*end == 0xa || *end == 0x9 || *end == 0xd)
        {
            col = end.offset_from(start) as _;
            break;
        }
        end = end.add(1);
    }
    if col == 0 {
        f = end;
        end = end.sub(1);
        while end > start && IS_BLANK_CH!(*end) {
            end = end.sub(1);
        }
        end = end.add(1);
        if start == value && f == end {
            return null_mut();
        }
        return xml_strndup(start, end.offset_from(start) as _);
    }
    start = xml_strdup(start);
    if start.is_null() {
        return null_mut();
    }
    g = start.add(col as usize) as _;
    end = g;
    while *end != 0 {
        if IS_BLANK_CH!(*end) {
            end = end.add(1);
            while IS_BLANK_CH!(*end) {
                end = end.add(1);
            }
            if *end != 0 {
                *g = b' ';
                g = g.add(1);
            }
        } else {
            *g = *end;
            g = g.add(1);
            end = end.add(1);
        }
    }
    *g = 0;
    start as _
}

/**
 * xmlSchemaWhiteSpaceReplace:
 * @value: a value
 *
 * Replaces 0xd, 0x9 and 0xa with a space.
 *
 * Returns the new string or NULL if no change was required.
 */
pub unsafe extern "C" fn xml_schema_white_space_replace(value: *const XmlChar) -> *mut XmlChar {
    let mut cur: *const XmlChar = value;
    let mut mcur: *mut XmlChar;

    if value.is_null() {
        return null_mut();
    }

    while *cur != 0 && (*cur != 0xd && *cur != 0x9 && *cur != 0xa) {
        cur = cur.add(1);
    }
    if *cur == 0 {
        return null_mut();
    }
    let ret: *mut XmlChar = xml_strdup(value);
    /* TODO FIXME: I guess gcc will bark at this. */
    mcur = ret.add(cur.offset_from(value) as usize);
    while {
        if *mcur == 0xd || *mcur == 0x9 || *mcur == 0xa {
            *mcur = b' ';
        }
        mcur = mcur.add(1);
        *mcur != 0
    } {}
    ret
}

/**
 * xmlSchemaGetFacetValueAsULong:
 * @facet: an schemas type facet
 *
 * Extract the value of a facet
 *
 * Returns the value as a c_long
 */
pub unsafe extern "C" fn xml_schema_get_facet_value_as_ulong(facet: XmlSchemaFacetPtr) -> c_ulong {
    /*
     * TODO: Check if this is a decimal.
     */
    if facet.is_null() || (*facet).val.is_null() {
        return 0;
    }
    (*(*facet).val).value.decimal.lo as c_ulong
}

/**
 * xmlSchemaValidateLengthFacet:
 * @type:  the built-in type
 * @facet:  the facet to check
 * @value:  the lexical repr. of the value to be validated
 * @val:  the precomputed value
 * @ws: the whitespace type of the value
 * @length: the actual length of the value
 *
 * Checka a value against a "length", "minLength" and "maxLength"
 * facet; sets @length to the computed length of @value.
 *
 * Returns 0 if the value is valid, a positive error code
 * otherwise and -1 in case of an internal or API error.
 */
unsafe extern "C" fn xml_schema_validate_length_facet_internal(
    facet: XmlSchemaFacetPtr,
    val_type: XmlSchemaValType,
    value: *const XmlChar,
    val: XmlSchemaValPtr,
    length: *mut c_ulong,
    ws: XmlSchemaWhitespaceValueType,
) -> c_int {
    let mut len: c_uint = 0;

    if length.is_null() || facet.is_null() {
        return -1;
    }
    *length = 0;
    if !matches!(
        (*facet).typ,
        XmlSchemaTypeType::XmlSchemaFacetLength
            | XmlSchemaTypeType::XmlSchemaFacetMaxlength
            | XmlSchemaTypeType::XmlSchemaFacetMinlength
    ) {
        return -1;
    }

    /*
     * TODO: length, maxLength and minLength must be of type
     * nonNegativeInteger only. Check if decimal is used somehow.
     */
    if (*facet).val.is_null()
        || !matches!(
            (*(*facet).val).typ,
            XmlSchemaValType::XmlSchemasDecimal | XmlSchemaValType::XmlSchemasNninteger
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
            XmlSchemaValType::XmlSchemasString | XmlSchemaValType::XmlSchemasNormstring => {
                if ws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceUnknown {
                    /*
                     * This is to ensure API compatibility with the old
                     * xmlSchemaValidateLengthFacet(). Anyway, this was and
                     * is not the correct handling.
                     * TODO: Get rid of this case somehow.
                     */
                    if val_type == XmlSchemaValType::XmlSchemasString {
                        len = xml_utf8_strlen(value) as _;
                    } else {
                        len = xml_schema_norm_len(value) as _;
                    }
                } else if !value.is_null() {
                    if ws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse {
                        len = xml_schema_norm_len(value) as _;
                    } else {
                        /*
                         * Should be OK for "preserve" as well.
                         */
                        len = xml_utf8_strlen(value) as _;
                    }
                }
            }
            XmlSchemaValType::XmlSchemasIdref
            | XmlSchemaValType::XmlSchemasToken
            | XmlSchemaValType::XmlSchemasLanguage
            | XmlSchemaValType::XmlSchemasNmtoken
            | XmlSchemaValType::XmlSchemasName
            | XmlSchemaValType::XmlSchemasNcname
            | XmlSchemaValType::XmlSchemasId
            /*
             * FIXME: What exactly to do with anyURI?
             */
            | XmlSchemaValType::XmlSchemasAnyuri => {
                if !value.is_null() {
                    len = xml_schema_norm_len(value) as _;
                }
            }
            XmlSchemaValType::XmlSchemasQname | XmlSchemaValType::XmlSchemasNotation => {
                /*
                 * For QName and NOTATION, those facets are
                 * deprecated and should be ignored.
                 */
                return 0;
            }
            _ => {
                // TODO
                todo!()
            }
        }
    }
    *length = len as c_ulong;
    /*
     * TODO: Return the whole expected value, i.e. "lo", "mi" and "hi".
     */
    if (*facet).typ == XmlSchemaTypeType::XmlSchemaFacetLength {
        if len as u64 != (*(*facet).val).value.decimal.lo {
            return XmlParserErrors::XmlSchemavCvcLengthValid as i32;
        }
    } else if (*facet).typ == XmlSchemaTypeType::XmlSchemaFacetMinlength {
        if (len as u64) < (*(*facet).val).value.decimal.lo {
            return XmlParserErrors::XmlSchemavCvcMinLengthValid as i32;
        }
    } else if len as u64 > (*(*facet).val).value.decimal.lo {
        return XmlParserErrors::XmlSchemavCvcMaxLengthValid as i32;
    }

    0
}

/**
 * xmlSchemaValidateLengthFacet:
 * @type:  the built-in type
 * @facet:  the facet to check
 * @value:  the lexical repr. of the value to be validated
 * @val:  the precomputed value
 * @length: the actual length of the value
 *
 * Checka a value against a "length", "minLength" and "maxLength"
 * facet; sets @length to the computed length of @value.
 *
 * Returns 0 if the value is valid, a positive error code
 * otherwise and -1 in case of an internal or API error.
 */
pub unsafe extern "C" fn xml_schema_validate_length_facet(
    typ: XmlSchemaTypePtr,
    facet: XmlSchemaFacetPtr,
    value: *const XmlChar,
    val: XmlSchemaValPtr,
    length: *mut c_ulong,
) -> c_int {
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

/**
 * xmlSchemaValidateLengthFacetWhtsp:
 * @facet:  the facet to check
 * @valType:  the built-in type
 * @value:  the lexical repr. of the value to be validated
 * @val:  the precomputed value
 * @ws: the whitespace type of the value
 * @length: the actual length of the value
 *
 * Checka a value against a "length", "minLength" and "maxLength"
 * facet; sets @length to the computed length of @value.
 *
 * Returns 0 if the value is valid, a positive error code
 * otherwise and -1 in case of an internal or API error.
 */
pub unsafe extern "C" fn xml_schema_validate_length_facet_whtsp(
    facet: XmlSchemaFacetPtr,
    val_type: XmlSchemaValType,
    value: *const XmlChar,
    val: XmlSchemaValPtr,
    length: *mut c_ulong,
    ws: XmlSchemaWhitespaceValueType,
) -> c_int {
    xml_schema_validate_length_facet_internal(facet, val_type, value, val, length, ws)
}

/**
 * xmlSchemaValPredefTypeNodeNoNorm:
 * @type: the predefined type
 * @value: the value to check
 * @val:  the return computed value
 * @node:  the node containing the value
 *
 * Check that a value conforms to the lexical space of the predefined type.
 * if true a value is computed and returned in @val.
 * This one does apply any normalization to the value.
 *
 * Returns 0 if this validates, a positive error code number otherwise
 *         and -1 in case of internal or API error.
 */
pub unsafe extern "C" fn xml_schema_val_predef_type_node_no_norm(
    typ: XmlSchemaTypePtr,
    value: *const XmlChar,
    val: *mut XmlSchemaValPtr,
    node: XmlNodePtr,
) -> c_int {
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

/**
 * xmlSchemaGetCanonValue:
 * @val: the precomputed value
 * @retValue: the returned value
 *
 * Get the canonical lexical representation of the value.
 * The caller has to FREE the returned retValue.
 *
 * WARNING: Some value types are not supported yet, resulting
 * in a @retValue of "???".
 *
 * TODO: XML Schema 1.0 does not define canonical representations
 * for: duration, gYearMonth, gYear, gMonthDay, gMonth, gDay,
 * anyURI, QName, NOTATION. This will be fixed in XML Schema 1.1.
 *
 *
 * Returns 0 if the value could be built, 1 if the value type is
 * not supported yet and -1 in case of API errors.
 */
pub unsafe extern "C" fn xml_schema_get_canon_value(
    val: XmlSchemaValPtr,
    ret_value: *mut *const XmlChar,
) -> c_int {
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
        XmlSchemaValType::XmlSchemasNormstring => {
            if (*val).value.str.is_null() {
                *ret_value = xml_strdup(c"".as_ptr() as _);
            } else {
                *ret_value = xml_schema_white_space_replace((*val).value.str);
                if (*ret_value).is_null() {
                    *ret_value = xml_strdup((*val).value.str);
                }
            }
        }
        XmlSchemaValType::XmlSchemasToken
        | XmlSchemaValType::XmlSchemasLanguage
        | XmlSchemaValType::XmlSchemasNmtoken
        | XmlSchemaValType::XmlSchemasName
        | XmlSchemaValType::XmlSchemasNcname
        | XmlSchemaValType::XmlSchemasId
        | XmlSchemaValType::XmlSchemasIdref
        | XmlSchemaValType::XmlSchemasEntity
        | XmlSchemaValType::XmlSchemasNotation
        | XmlSchemaValType::XmlSchemasAnyuri => {
            if (*val).value.str.is_null() {
                return -1;
            }
            *ret_value = xml_schema_collapse_string((*val).value.str);
            if (*ret_value).is_null() {
                *ret_value = xml_strdup((*val).value.str);
            }
        }
        XmlSchemaValType::XmlSchemasQname => {
            /* TODO: Unclear in XML Schema 1.0. */
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
            /*
             * TODO: Lookout for a more simple implementation.
             */
            if (*val).value.decimal.total == 1 && (*val).value.decimal.lo == 0 {
                *ret_value = xml_strdup(c"0.0".as_ptr() as _);
            } else {
                let mut bufsize: c_int;
                let dec: XmlSchemaValDecimal = (*val).value.decimal;
                let mut offs: *mut c_char;

                /* Add room for the decimal point as well. */
                bufsize = dec.total as i32 + 2;
                if dec.sign != 0 {
                    bufsize += 1;
                }
                /* Add room for leading/trailing zero. */
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
                        let diff: c_int = (dec.total - dec.frac) as i32;
                        /*
                         * Insert the decimal point.
                         */
                        memmove(
                            offs.add(diff as usize + 1) as _,
                            offs.add(diff as usize) as _,
                            dec.frac as usize + 1,
                        );
                        *offs.add(diff as usize) = b'.' as _;
                    } else {
                        let mut i: c_uint = 0;
                        /*
                         * Insert missing zeroes behind the decimal point.
                         */
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
                    /*
                     * Append decimal point and zero.
                     */
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
        | XmlSchemaValType::XmlSchemasPinteger
        | XmlSchemaValType::XmlSchemasNpinteger
        | XmlSchemaValType::XmlSchemasNinteger
        | XmlSchemaValType::XmlSchemasNninteger
        | XmlSchemaValType::XmlSchemasLong
        | XmlSchemaValType::XmlSchemasByte
        | XmlSchemaValType::XmlSchemasShort
        | XmlSchemaValType::XmlSchemasInt
        | XmlSchemaValType::XmlSchemasUint
        | XmlSchemaValType::XmlSchemasUlong
        | XmlSchemaValType::XmlSchemasUshort
        | XmlSchemaValType::XmlSchemasUbyte => {
            if (*val).value.decimal.total == 1 && (*val).value.decimal.lo == 0 {
                *ret_value = xml_strdup(c"0".as_ptr() as _);
            } else {
                let dec: XmlSchemaValDecimal = (*val).value.decimal;
                let mut bufsize: c_int = dec.total as i32 + 1;

                /* Add room for the decimal point as well. */
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
            let mut hour: c_ulong = 0;
            let mut min: c_ulong = 0;
            let mut sec: f64 = 0.;
            let mut left: f64;

            /* TODO: Unclear in XML Schema 1.0 */
            /*
             * TODO: This results in a normalized output of the value
             * - which is NOT conformant to the spec -
             * since the exact values of each property are not
             * recoverable. Think about extending the structure to
             * provide a field for every property.
             */
            let year: c_ulong = FQUOTIENT!((*val).value.dur.mon.abs(), 12) as c_ulong;
            let mon: c_ulong = (*val).value.dur.mon.unsigned_abs() - 12 * year;

            let day: c_ulong = FQUOTIENT!((*val).value.dur.sec.abs(), 86400) as c_ulong;
            left = (*val).value.dur.sec.abs() - (day * 86400) as f64;
            if left > 0. {
                hour = FQUOTIENT!(left, 3600) as c_ulong;
                left -= (hour * 3600) as f64;
                if left > 0. {
                    min = FQUOTIENT!(left, 60) as c_ulong;
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
        XmlSchemaValType::XmlSchemasGyear => {
            let mut buf: [c_char; 30] = [0; 30];
            /* TODO: Unclear in XML Schema 1.0 */
            /* TODO: What to do with the timezone? */
            snprintf(
                buf.as_mut_ptr() as _,
                30,
                c"%04ld".as_ptr() as _,
                (*val).value.date.year,
            );
            *ret_value = xml_strdup(buf.as_ptr() as _);
        }
        XmlSchemaValType::XmlSchemasGmonth => {
            /* TODO: Unclear in XML Schema 1.0 */
            /* TODO: What to do with the timezone? */
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
        XmlSchemaValType::XmlSchemasGday => {
            /* TODO: Unclear in XML Schema 1.0 */
            /* TODO: What to do with the timezone? */
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
        XmlSchemaValType::XmlSchemasGmonthday => {
            /* TODO: Unclear in XML Schema 1.0 */
            /* TODO: What to do with the timezone? */
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
        XmlSchemaValType::XmlSchemasGyearmonth => {
            let mut buf: [c_char; 35] = [0; 35];
            /* TODO: Unclear in XML Schema 1.0 */
            /* TODO: What to do with the timezone? */
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
                /*
                 * TODO: Check if "%.14g" is portable.
                 */
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
                /*
                 * TODO: Append the canonical value of the
                 * recoverable timezone and not "Z".
                 */
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
                /*
                 * TODO: Check if "%.14g" is portable.
                 */
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
            /*
             * TODO: Is the following spec piece implemented?:
             * SPEC: "Note: For some values the canonical form defined
             * above does not conform to [RFC 2045], which requires breaking
             * with linefeeds at appropriate intervals."
             */
            *ret_value = xml_strdup((*val).value.base64.str);
        }
        XmlSchemaValType::XmlSchemasFloat => {
            let mut buf: [c_char; 30] = [0; 30];
            /*
             * |m| < 16777216, -149 <= e <= 104.
             * TODO: Handle, NaN, INF, -INF. The format is not
             * yet conformant. The c type float does not cover
             * the whole range.
             */
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
            /* |m| < 9007199254740992, -1075 <= e <= 970 */
            /*
             * TODO: Handle, NaN, INF, -INF. The format is not
             * yet conformant. The c type float does not cover
             * the whole range.
             */
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

/**
 * xmlSchemaGetCanonValueWhtsp:
 * @val: the precomputed value
 * @retValue: the returned value
 * @ws: the whitespace type of the value
 *
 * Get the canonical representation of the value.
 * The caller has to free the returned @retValue.
 *
 * Returns 0 if the value could be built, 1 if the value type is
 * not supported yet and -1 in case of API errors.
 */
pub unsafe extern "C" fn xml_schema_get_canon_value_whtsp(
    val: XmlSchemaValPtr,
    ret_value: *mut *const XmlChar,
    ws: XmlSchemaWhitespaceValueType,
) -> c_int {
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
                *ret_value = xml_schema_collapse_string((*val).value.str);
            } else if ws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceReplace {
                *ret_value = xml_schema_white_space_replace((*val).value.str);
            }
            if (*ret_value).is_null() {
                *ret_value = xml_strdup((*val).value.str);
            }
        }
        XmlSchemaValType::XmlSchemasNormstring => {
            if (*val).value.str.is_null() {
                *ret_value = xml_strdup(c"".as_ptr() as _);
            } else {
                if ws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse {
                    *ret_value = xml_schema_collapse_string((*val).value.str);
                } else {
                    *ret_value = xml_schema_white_space_replace((*val).value.str);
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

/**
 * xmlSchemaValueAppend:
 * @prev: the value
 * @cur: the value to be appended
 *
 * Appends a next sibling to a list of computed values.
 *
 * Returns 0 if succeeded and -1 on API errors.
 */
pub unsafe extern "C" fn xml_schema_value_append(
    prev: XmlSchemaValPtr,
    cur: XmlSchemaValPtr,
) -> c_int {
    if prev.is_null() || cur.is_null() {
        return -1;
    }
    (*prev).next = cur;
    0
}

/**
 * xmlSchemaValueGetNext:
 * @cur: the value
 *
 * Accessor for the next sibling of a list of computed values.
 *
 * Returns the next value or NULL if there was none, or on
 *         API errors.
 */
pub unsafe extern "C" fn xml_schema_value_get_next(cur: XmlSchemaValPtr) -> XmlSchemaValPtr {
    if cur.is_null() {
        return null_mut();
    }
    (*cur).next
}

/**
 * xmlSchemaValueGetAsString:
 * @val: the value
 *
 * Accessor for the string value of a computed value.
 *
 * Returns the string value or NULL if there was none, or on
 *         API errors.
 */
pub unsafe extern "C" fn xml_schema_value_get_as_string(val: XmlSchemaValPtr) -> *const XmlChar {
    if val.is_null() {
        return null_mut();
    }
    match (*val).typ {
        XmlSchemaValType::XmlSchemasString
        | XmlSchemaValType::XmlSchemasNormstring
        | XmlSchemaValType::XmlSchemasAnysimpletype
        | XmlSchemaValType::XmlSchemasToken
        | XmlSchemaValType::XmlSchemasLanguage
        | XmlSchemaValType::XmlSchemasNmtoken
        | XmlSchemaValType::XmlSchemasName
        | XmlSchemaValType::XmlSchemasNcname
        | XmlSchemaValType::XmlSchemasId
        | XmlSchemaValType::XmlSchemasIdref
        | XmlSchemaValType::XmlSchemasEntity
        | XmlSchemaValType::XmlSchemasAnyuri => {
            return (*val).value.str;
        }
        _ => {}
    }
    null_mut()
}

/**
 * xmlSchemaValueGetAsBoolean:
 * @val: the value
 *
 * Accessor for the boolean value of a computed value.
 *
 * Returns 1 if true and 0 if false, or in case of an error. Hmm.
 */
pub unsafe extern "C" fn xml_schema_value_get_as_boolean(val: XmlSchemaValPtr) -> c_int {
    if val.is_null() || (*val).typ != XmlSchemaValType::XmlSchemasBoolean {
        return 0;
    }
    (*val).value.b
}

/**
 * xmlSchemaNewStringValue:
 * @type:  the value type
 * @value:  the value
 *
 * Allocate a new simple type value. The type can be
 * of XmlSchemaValType::XML_SCHEMAS_STRING.
 * WARNING: This one is intended to be expanded for other
 * string based types. We need this for anySimpleType as well.
 * The given value is consumed and freed with the struct.
 *
 * Returns a pointer to the new value or NULL in case of error
 */
pub unsafe extern "C" fn xml_schema_new_string_value(
    typ: XmlSchemaValType,
    value: *const XmlChar,
) -> XmlSchemaValPtr {
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

/**
 * xmlSchemaNewNOTATIONValue:
 * @name:  the notation name
 * @ns: the notation namespace name or NULL
 *
 * Allocate a new NOTATION value.
 * The given values are consumed and freed with the struct.
 *
 * Returns a pointer to the new value or NULL in case of error
 */
pub unsafe extern "C" fn xml_schema_new_notation_value(
    name: *const XmlChar,
    ns: *const XmlChar,
) -> XmlSchemaValPtr {
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

/**
 * xmlSchemaNewQNameValue:
 * @namespaceName: the namespace name
 * @localName: the local name
 *
 * Allocate a new QName value.
 * The given values are consumed and freed with the struct.
 *
 * Returns a pointer to the new value or NULL in case of an error.
 */
pub unsafe extern "C" fn xml_schema_new_qname_value(
    namespace_name: *const XmlChar,
    local_name: *const XmlChar,
) -> XmlSchemaValPtr {
    let val: XmlSchemaValPtr = xml_schema_new_value(XmlSchemaValType::XmlSchemasQname);
    if val.is_null() {
        return null_mut();
    }

    (*val).value.qname.name = local_name as _;
    (*val).value.qname.uri = namespace_name as _;
    val
}

/**
 * xmlSchemaCompareValuesWhtsp:
 * @x:  a first value
 * @xws: the whitespace value of x
 * @y:  a second value
 * @yws: the whitespace value of y
 *
 * Compare 2 values
 *
 * Returns -1 if x < y, 0 if x == y, 1 if x > y, 2 if x <> y, and -2 in
 * case of error
 */
pub unsafe extern "C" fn xml_schema_compare_values_whtsp(
    x: XmlSchemaValPtr,
    xws: XmlSchemaWhitespaceValueType,
    y: XmlSchemaValPtr,
    yws: XmlSchemaWhitespaceValueType,
) -> c_int {
    if x.is_null() || y.is_null() {
        return -2;
    }
    xml_schema_compare_values_internal((*x).typ, x, null_mut(), xws, (*y).typ, y, null_mut(), yws)
}

/**
 * xmlSchemaCopyValue:
 * @val:  the precomputed value to be copied
 *
 * Copies the precomputed value. This duplicates any string within.
 *
 * Returns the copy or NULL if a copy for a data-type is not implemented.
 */
pub unsafe extern "C" fn xml_schema_copy_value(mut val: XmlSchemaValPtr) -> XmlSchemaValPtr {
    let mut ret: XmlSchemaValPtr = null_mut();
    let mut prev: XmlSchemaValPtr = null_mut();
    let mut cur: XmlSchemaValPtr;

    /*
     * Copy the string values.
     */
    while !val.is_null() {
        match (*val).typ {
            XmlSchemaValType::XmlSchemasAnytype
            | XmlSchemaValType::XmlSchemasIdrefs
            | XmlSchemaValType::XmlSchemasEntities
            | XmlSchemaValType::XmlSchemasNmtokens => {
                xml_schema_free_value(ret);
                return null_mut();
            }
            XmlSchemaValType::XmlSchemasAnysimpletype
            | XmlSchemaValType::XmlSchemasString
            | XmlSchemaValType::XmlSchemasNormstring
            | XmlSchemaValType::XmlSchemasToken
            | XmlSchemaValType::XmlSchemasLanguage
            | XmlSchemaValType::XmlSchemasName
            | XmlSchemaValType::XmlSchemasNcname
            | XmlSchemaValType::XmlSchemasId
            | XmlSchemaValType::XmlSchemasIdref
            | XmlSchemaValType::XmlSchemasEntity
            | XmlSchemaValType::XmlSchemasNmtoken
            | XmlSchemaValType::XmlSchemasAnyuri => {
                cur = xml_schema_dup_val(val);
                if !(*val).value.str.is_null() {
                    (*cur).value.str = xml_strdup((*val).value.str);
                }
            }
            XmlSchemaValType::XmlSchemasQname | XmlSchemaValType::XmlSchemasNotation => {
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

/**
 * xmlSchemaGetValType:
 * @val: a schemas value
 *
 * Accessor for the type of a value
 *
 * Returns the XmlSchemaValType of the value
 */
pub unsafe extern "C" fn xml_schema_get_val_type(val: XmlSchemaValPtr) -> XmlSchemaValType {
    if val.is_null() {
        return XmlSchemaValType::XmlSchemasUnknown;
    }
    (*val).typ
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
    fn test_xml_schema_collapse_string() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let value = gen_const_xml_char_ptr(n_value, 0);

                let ret_val = xml_schema_collapse_string(value as *const XmlChar);
                desret_xml_char_ptr(ret_val);
                des_const_xml_char_ptr(n_value, value, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlSchemaCollapseString",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlSchemaCollapseString()"
                    );
                    eprintln!(" {}", n_value);
                }
            }
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
    fn test_xml_schema_get_predefined_type() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_ns in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let name = gen_const_xml_char_ptr(n_name, 0);
                    let ns = gen_const_xml_char_ptr(n_ns, 1);

                    let ret_val = xml_schema_get_predefined_type(name as *const XmlChar, ns);
                    desret_xml_schema_type_ptr(ret_val);
                    des_const_xml_char_ptr(n_name, name, 0);
                    des_const_xml_char_ptr(n_ns, ns, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlSchemaGetPredefinedType",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlSchemaGetPredefinedType()"
                        );
                        eprint!(" {}", n_name);
                        eprintln!(" {}", n_ns);
                    }
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
    fn test_xml_schema_new_facet() {

        /* missing type support */
    }

    #[test]
    fn test_xml_schema_new_notationvalue() {

        /* missing type support */
    }

    #[test]
    fn test_xml_schema_new_qname_value() {

        /* missing type support */
    }

    #[test]
    fn test_xml_schema_new_string_value() {

        /* missing type support */
    }

    #[test]
    fn test_xml_schema_val_predef_type_node() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_type in 0..GEN_NB_XML_SCHEMA_TYPE_PTR {
                for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_val in 0..GEN_NB_XML_SCHEMA_VAL_PTR_PTR {
                        for n_node in 0..GEN_NB_XML_NODE_PTR {
                            let mem_base = xml_mem_blocks();
                            let typ = gen_xml_schema_type_ptr(n_type, 0);
                            let value = gen_const_xml_char_ptr(n_value, 1);
                            let val = gen_xml_schema_val_ptr_ptr(n_val, 2);
                            let node = gen_xml_node_ptr(n_node, 3);

                            let ret_val = xml_schema_val_predef_type_node(typ, value, val, node);
                            desret_int(ret_val);
                            des_xml_schema_type_ptr(n_type, typ, 0);
                            des_const_xml_char_ptr(n_value, value, 1);
                            des_xml_schema_val_ptr_ptr(n_val, val, 2);
                            des_xml_node_ptr(n_node, node, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlSchemaValPredefTypeNode",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlSchemaValPredefTypeNode()"
                                );
                                eprint!(" {}", n_type);
                                eprint!(" {}", n_value);
                                eprint!(" {}", n_val);
                                eprintln!(" {}", n_node);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_schema_val_predef_type_node_no_norm() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_type in 0..GEN_NB_XML_SCHEMA_TYPE_PTR {
                for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_val in 0..GEN_NB_XML_SCHEMA_VAL_PTR_PTR {
                        for n_node in 0..GEN_NB_XML_NODE_PTR {
                            let mem_base = xml_mem_blocks();
                            let typ = gen_xml_schema_type_ptr(n_type, 0);
                            let value = gen_const_xml_char_ptr(n_value, 1);
                            let val = gen_xml_schema_val_ptr_ptr(n_val, 2);
                            let node = gen_xml_node_ptr(n_node, 3);

                            let ret_val =
                                xml_schema_val_predef_type_node_no_norm(typ, value, val, node);
                            desret_int(ret_val);
                            des_xml_schema_type_ptr(n_type, typ, 0);
                            des_const_xml_char_ptr(n_value, value, 1);
                            des_xml_schema_val_ptr_ptr(n_val, val, 2);
                            des_xml_node_ptr(n_node, node, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlSchemaValPredefTypeNodeNoNorm",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlSchemaValPredefTypeNodeNoNorm()"
                                );
                                eprint!(" {}", n_type);
                                eprint!(" {}", n_value);
                                eprint!(" {}", n_val);
                                eprintln!(" {}", n_node);
                            }
                        }
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
                                        eprint!("Leak of {} blocks found in xmlSchemaValidateFacetWhtsp", xml_mem_blocks() - mem_base);
                                        assert!(leaks == 0, "{leaks} Leaks are found in xmlSchemaValidateFacetWhtsp()");
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
                                        eprint!("Leak of {} blocks found in xmlSchemaValidateLengthFacetWhtsp", xml_mem_blocks() - mem_base);
                                        assert!(leaks == 0, "{leaks} Leaks are found in xmlSchemaValidateLengthFacetWhtsp()");
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
                                eprint!("Leak of {} blocks found in xmlSchemaValidateListSimpleTypeFacet", xml_mem_blocks() - mem_base);
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlSchemaValidateListSimpleTypeFacet()");
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

    #[test]
    fn test_xml_schema_value_get_next() {

        /* missing type support */
    }

    #[test]
    fn test_xml_schema_white_space_replace() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let value = gen_const_xml_char_ptr(n_value, 0);

                let ret_val = xml_schema_white_space_replace(value as *const XmlChar);
                desret_xml_char_ptr(ret_val);
                des_const_xml_char_ptr(n_value, value, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlSchemaWhiteSpaceReplace",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlSchemaWhiteSpaceReplace()"
                    );
                    eprintln!(" {}", n_value);
                }
            }
        }
    }
}
