//! Provide methods and data structures for parsing XML Schemas.  
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

use std::{
    borrow::Cow,
    cell::RefCell,
    ffi::{CStr, CString, c_char},
    mem::{size_of, take},
    os::raw::c_void,
    ptr::{drop_in_place, fn_addr_eq, null, null_mut},
    rc::Rc,
    slice::from_raw_parts,
};

use libc::{memset, strchr};

use crate::{
    encoding::XmlCharEncoding,
    error::{XmlErrorDomain, XmlParserErrors},
    generic_error,
    globals::{GLOBAL_STATE, GenericError, GenericErrorContext, StructuredError},
    io::XmlParserInputBuffer,
    libxml::{
        chvalid::xml_is_blank_char,
        dict::{XmlDictPtr, xml_dict_create, xml_dict_free, xml_dict_lookup, xml_dict_reference},
        globals::{xml_free, xml_malloc, xml_realloc},
        hash::{
            XmlHashTablePtr, xml_hash_add_entry, xml_hash_add_entry2, xml_hash_create,
            xml_hash_free, xml_hash_lookup, xml_hash_lookup2,
        },
        parser::{
            XML_SAX2_MAGIC, XmlParserOption, XmlSAXHandler, XmlSAXHandlerPtr, XmlSAXLocatorPtr,
            xml_new_io_input_stream, xml_parse_document,
        },
        pattern::{
            XmlPatternFlags, XmlPatternPtr, XmlStreamCtxtPtr, xml_free_pattern,
            xml_free_stream_ctxt, xml_pattern_get_stream_ctxt, xml_patterncompile, xml_stream_pop,
            xml_stream_push, xml_stream_push_attr,
        },
        sax2::xml_sax2_get_line_number,
        schemas_internals::{
            XML_SCHEMAS_ANY_LAX, XML_SCHEMAS_ANY_SKIP, XML_SCHEMAS_ANY_STRICT,
            XML_SCHEMAS_ATTR_FIXED, XML_SCHEMAS_ATTR_INTERNAL_RESOLVED,
            XML_SCHEMAS_ATTR_USE_OPTIONAL, XML_SCHEMAS_ATTR_USE_PROHIBITED,
            XML_SCHEMAS_ATTR_USE_REQUIRED, XML_SCHEMAS_ATTRGROUP_GLOBAL,
            XML_SCHEMAS_ATTRGROUP_HAS_REFS, XML_SCHEMAS_ATTRGROUP_MARKED,
            XML_SCHEMAS_ATTRGROUP_REDEFINED, XML_SCHEMAS_ATTRGROUP_WILDCARD_BUILDED,
            XML_SCHEMAS_ELEM_ABSTRACT, XML_SCHEMAS_ELEM_BLOCK_EXTENSION,
            XML_SCHEMAS_ELEM_BLOCK_RESTRICTION, XML_SCHEMAS_ELEM_BLOCK_SUBSTITUTION,
            XML_SCHEMAS_ELEM_CIRCULAR, XML_SCHEMAS_ELEM_FINAL_EXTENSION,
            XML_SCHEMAS_ELEM_FINAL_RESTRICTION, XML_SCHEMAS_ELEM_FIXED, XML_SCHEMAS_ELEM_GLOBAL,
            XML_SCHEMAS_ELEM_INTERNAL_CHECKED, XML_SCHEMAS_ELEM_INTERNAL_RESOLVED,
            XML_SCHEMAS_ELEM_NILLABLE, XML_SCHEMAS_ELEM_SUBST_GROUP_HEAD,
            XML_SCHEMAS_FACET_COLLAPSE, XML_SCHEMAS_FACET_PRESERVE, XML_SCHEMAS_FACET_REPLACE,
            XML_SCHEMAS_INCLUDING_CONVERT_NS, XML_SCHEMAS_QUALIF_ATTR, XML_SCHEMAS_TYPE_ABSTRACT,
            XML_SCHEMAS_TYPE_BLOCK_EXTENSION, XML_SCHEMAS_TYPE_BLOCK_RESTRICTION,
            XML_SCHEMAS_TYPE_BUILTIN_PRIMITIVE, XML_SCHEMAS_TYPE_DERIVATION_METHOD_EXTENSION,
            XML_SCHEMAS_TYPE_DERIVATION_METHOD_RESTRICTION, XML_SCHEMAS_TYPE_FACETSNEEDVALUE,
            XML_SCHEMAS_TYPE_FINAL_EXTENSION, XML_SCHEMAS_TYPE_FINAL_LIST,
            XML_SCHEMAS_TYPE_FINAL_RESTRICTION, XML_SCHEMAS_TYPE_FINAL_UNION,
            XML_SCHEMAS_TYPE_FIXUP_1, XML_SCHEMAS_TYPE_GLOBAL, XML_SCHEMAS_TYPE_HAS_FACETS,
            XML_SCHEMAS_TYPE_INTERNAL_INVALID, XML_SCHEMAS_TYPE_INTERNAL_RESOLVED,
            XML_SCHEMAS_TYPE_MARKED, XML_SCHEMAS_TYPE_MIXED, XML_SCHEMAS_TYPE_NORMVALUENEEDED,
            XML_SCHEMAS_TYPE_REDEFINED, XML_SCHEMAS_TYPE_VARIETY_ATOMIC,
            XML_SCHEMAS_TYPE_VARIETY_LIST, XML_SCHEMAS_TYPE_VARIETY_UNION,
            XML_SCHEMAS_TYPE_WHITESPACE_COLLAPSE, XML_SCHEMAS_TYPE_WHITESPACE_PRESERVE,
            XML_SCHEMAS_TYPE_WHITESPACE_REPLACE, XmlSchemaAnnot, XmlSchemaAnnotPtr,
            XmlSchemaContentType, XmlSchemaFacetLink, XmlSchemaFacetLinkPtr, XmlSchemaFacetPtr,
            XmlSchemaTypeLink, XmlSchemaTypeLinkPtr, XmlSchemaTypeType, XmlSchemaValType,
            XmlSchemaWildcard, XmlSchemaWildcardNs, XmlSchemaWildcardNsPtr, XmlSchemaWildcardPtr,
            xml_schema_free_annot, xml_schema_free_type, xml_schema_free_wildcard,
            xml_schema_free_wildcard_ns_set,
        },
        valid::xml_add_id,
        xmlautomata::{
            XmlAutomataStatePtr, xml_automata_compile, xml_automata_get_init_state,
            xml_automata_new_all_trans, xml_automata_new_count_trans2,
            xml_automata_new_counted_trans, xml_automata_new_counter,
            xml_automata_new_counter_trans, xml_automata_new_epsilon, xml_automata_new_neg_trans,
            xml_automata_new_once_trans2, xml_automata_new_state, xml_automata_new_transition2,
            xml_automata_set_final_state, xml_free_automata, xml_new_automata,
        },
        xmlregexp::{
            XmlRegExecCtxtPtr, xml_reg_exec_err_info, xml_reg_exec_next_values,
            xml_reg_exec_push_string, xml_reg_exec_push_string2, xml_reg_free_exec_ctxt,
            xml_reg_free_regexp, xml_reg_new_exec_ctxt, xml_regexp_exec, xml_regexp_is_determinist,
        },
        xmlschemastypes::{
            XmlSchemaValPtr, XmlSchemaWhitespaceValueType, xml_schema_check_facet,
            xml_schema_compare_values, xml_schema_compare_values_whtsp, xml_schema_copy_value,
            xml_schema_free_facet, xml_schema_free_value, xml_schema_get_built_in_type,
            xml_schema_get_canon_value, xml_schema_get_val_type, xml_schema_is_built_in_type_facet,
            xml_schema_new_facet, xml_schema_new_notation_value, xml_schema_new_qname_value,
            xml_schema_val_predef_type_node, xml_schema_val_predef_type_node_no_norm,
            xml_schema_validate_facet_whtsp, xml_schema_validate_length_facet_whtsp,
            xml_schema_validate_list_simple_type_facet, xml_schema_value_append,
            xml_schema_value_get_as_boolean, xml_schema_value_get_as_string,
            xml_schema_value_get_next,
        },
        xmlstring::{
            XmlChar, xml_str_equal, xml_strcat, xml_strdup, xml_strlen, xml_strncat,
            xml_strncat_new, xml_strndup,
        },
    },
    parser::{
        XmlParserCtxtPtr, XmlParserInputPtr, xml_ctxt_read_file, xml_ctxt_read_memory,
        xml_free_parser_ctxt, xml_new_parser_ctxt, xml_new_sax_parser_ctxt,
    },
    tree::{
        NodeCommon, XmlAttrPtr, XmlAttributeDefault, XmlAttributeType, XmlDocPtr,
        XmlElementContentPtr, XmlElementType, XmlElementTypeVal, XmlEntityPtr, XmlEntityType,
        XmlEnumeration, XmlGenericNodePtr, XmlNodePtr, validate_ncname, validate_qname,
        xml_free_doc, xml_free_node, xml_new_doc_text, xml_new_ns, xml_new_ns_prop, xml_new_prop,
        xml_split_qname2, xml_split_qname3,
    },
    uri::build_uri,
    xmlschemas::{
        context::{
            XmlSchemaParserCtxtPtr, XmlSchemaValidCtxtPtr, xml_schema_free_parser_ctxt,
            xml_schema_new_parser_ctxt, xml_schema_new_parser_ctxt_use_dict,
            xml_schema_new_valid_ctxt,
        },
        error::{
            xml_schema_complex_type_err, xml_schema_custom_err, xml_schema_custom_err4,
            xml_schema_custom_warning, xml_schema_derive_facet_err, xml_schema_facet_err,
            xml_schema_illegal_attr_err, xml_schema_internal_err2, xml_schema_keyref_err,
            xml_schema_pattr_use_err4, xml_schema_pcontent_err, xml_schema_pcustom_attr_err,
            xml_schema_pcustom_err, xml_schema_pcustom_err_ext, xml_schema_perr,
            xml_schema_perr_memory, xml_schema_perr2, xml_schema_pillegal_attr_err,
            xml_schema_pillegal_facet_atomic_err, xml_schema_pillegal_facet_list_union_err,
            xml_schema_pmissing_attr_err, xml_schema_pmutual_excl_attr_err,
            xml_schema_pres_comp_attr_err, xml_schema_psimple_internal_err,
            xml_schema_psimple_type_err, xml_schema_simple_type_err, xml_schema_verr_memory,
        },
        is_schema,
        item_list::{XmlSchemaItemListPtr, xml_schema_item_list_create, xml_schema_item_list_free},
        items::{
            XmlSchemaAnnotItemPtr, XmlSchemaAttribute, XmlSchemaAttributeGroup,
            XmlSchemaAttributeGroupPtr, XmlSchemaAttributePtr, XmlSchemaAttributeUse,
            XmlSchemaAttributeUseProhib, XmlSchemaAttributeUseProhibPtr, XmlSchemaAttributeUsePtr,
            XmlSchemaBasicItemPtr, XmlSchemaElement, XmlSchemaElementPtr, XmlSchemaIDC,
            XmlSchemaIDCPtr, XmlSchemaItem, XmlSchemaModelGroup, XmlSchemaModelGroupDef,
            XmlSchemaModelGroupDefPtr, XmlSchemaModelGroupPtr, XmlSchemaNotation,
            XmlSchemaNotationPtr, XmlSchemaParticle, XmlSchemaParticlePtr, XmlSchemaQNameRef,
            XmlSchemaQNameRefPtr, XmlSchemaTreeItemPtr, XmlSchemaType, XmlSchemaTypePtr,
            xml_schema_free_attribute_use_prohib,
        },
        schema::{XmlSchemaPtr, xml_schema_free},
        wxs_is_any_simple_type, wxs_is_anytype, wxs_is_complex, wxs_is_simple,
    },
    xmlschemastypes::{xml_schema_collapse_string, xml_schema_white_space_replace},
};

/// This error codes are obsolete; not used any more.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlSchemaValidError {
    XmlSchemasErrOK = 0,
    XmlSchemasErrNoRoot = 1,
    XmlSchemasErrUndeclaredElem,
    XmlSchemasErrNotTopLevel,
    XmlSchemasErrMissing,
    XmlSchemasErrWrongElem,
    XmlSchemasErrNoType,
    XmlSchemasErrNoRollback,
    XmlSchemasErrIsAbstract,
    XmlSchemasErrNotEmpty,
    XmlSchemasErrElemCont,
    XmlSchemasErrHaveDefault,
    XmlSchemasErrNotNillable,
    XmlSchemasErrExtraContent,
    XmlSchemasErrInvalidAttr,
    XmlSchemasErrInvalidElem,
    XmlSchemasErrNotDeterminist,
    XmlSchemasErrConstruct,
    XmlSchemasErrInternal,
    XmlSchemasErrNotSimple,
    XmlSchemasErrAttrUnknown,
    XmlSchemasErrAttrInvalid,
    XmlSchemasErrValue,
    XmlSchemasErrFacet,
    XmlSchemasErr,
    XmlSchemasErrXxx,
}

// ATTENTION: Change xmlSchemaSetValidOptions's check
// for invalid values, if adding to the validation
// options below.

/// This is the set of XML Schema validation options.
#[doc(alias = "xmlSchemaValidOption")]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlSchemaValidOption {
    // Default/fixed: create an attribute node
    // or an element's text node on the instance.
    XmlSchemaValVcICreate = 1 << 0,
    // XML_SCHEMA_VAL_XSI_ASSEMBLE			= 1<<1,
    // assemble schemata using
    // xsi:schemaLocation and
    // xsi:noNamespaceSchemaLocation
}

/// Signature of an error callback from an XSD validation
#[doc(alias = "xmlSchemaValidityErrorFunc")]
pub type XmlSchemaValidityErrorFunc = unsafe fn(ctx: *mut c_void, msg: *const c_char);

/// Signature of a warning callback from an XSD validation
#[doc(alias = "xmlSchemaValidityWarningFunc")]
pub type XmlSchemaValidityWarningFunc = unsafe fn(ctx: *mut c_void, msg: *const c_char);

/// A schemas validation locator, a callback called by the validator.
/// This is used when file or node information are not available
/// to find out what file and line number are affected
///
/// Returns: 0 in case of success and -1 in case of error
#[doc(alias = "xmlSchemaValidityLocatorFunc")]
pub type XmlSchemaValidityLocatorFunc =
    unsafe fn(ctx: *mut c_void, file: *mut Option<String>, line: *mut u64) -> i32;

const UNBOUNDED: usize = 1 << 30;

pub(crate) const XML_SCHEMAS_NO_NAMESPACE: &str = "##";

// The XML Schemas namespaces
pub(crate) const XML_SCHEMA_NS: &CStr = c"http://www.w3.org/2001/XMLSchema";

pub(crate) const XML_SCHEMA_INSTANCE_NS: &CStr = c"http://www.w3.org/2001/XMLSchema-instance";

const XML_NAMESPACE_NS: &CStr = c"http://www.w3.org/2000/xmlns/";

// Macros for attribute declarations.
macro_rules! WXS_ATTR_TYPEDEF {
    ($a:expr) => {
        (*$a).subtypes
    };
}
// Macros for attribute uses.
macro_rules! WXS_ATTRUSE_DECL {
    ($au:expr) => {
        (*($au as XmlSchemaAttributeUsePtr)).attr_decl
    };
}

macro_rules! WXS_ATTRUSE_TYPEDEF {
    ($au:expr) => {
        WXS_ATTR_TYPEDEF!(WXS_ATTRUSE_DECL!($au as XmlSchemaAttributeUsePtr))
    };
}

macro_rules! WXS_ATTRUSE_DECL_NAME {
    ($au:expr) => {
        (*WXS_ATTRUSE_DECL!($au)).name
    };
}

macro_rules! WXS_ATTRUSE_DECL_TNS {
    ($au:expr) => {
        (*WXS_ATTRUSE_DECL!($au)).target_namespace
    };
}
// Macros for attribute groups.
macro_rules! WXS_ATTR_GROUP_HAS_REFS {
    ($ag:expr) => {
        (*($ag as XmlSchemaAttributeGroupPtr)).flags & XML_SCHEMAS_ATTRGROUP_HAS_REFS != 0
    };
}
macro_rules! WXS_ATTR_GROUP_EXPANDED {
    ($ag:expr) => {
        (*($ag as XmlSchemaAttributeGroupPtr)).flags & XML_SCHEMAS_ATTRGROUP_WILDCARD_BUILDED != 0
    };
}
// Macros for particles.
macro_rules! WXS_PARTICLE {
    ($p:expr) => {
        $p as XmlSchemaParticlePtr
    };
}

macro_rules! WXS_PARTICLE_TERM {
    ($p:expr) => {
        (*WXS_PARTICLE!($p)).children
    };
}

// Macros for model groups definitions.
macro_rules! WXS_MODELGROUPDEF_MODEL {
    ($mgd:expr) => {
        (*($mgd as XmlSchemaModelGroupPtr)).children
    };
}
// Macros for model groups.
macro_rules! WXS_IS_MODEL_GROUP {
    ($i:expr) => {
        (*$i).typ == XmlSchemaTypeType::XmlSchemaTypeSequence
            || (*$i).typ == XmlSchemaTypeType::XmlSchemaTypeChoice
            || (*$i).typ == XmlSchemaTypeType::XmlSchemaTypeAll
    };
}

macro_rules! WXS_MODELGROUP_PARTICLE {
    ($mg:expr) => {
        (*$mg).children as XmlSchemaParticlePtr
    };
}
// Macros for schema buckets.
macro_rules! WXS_IS_BUCKET_INCREDEF {
    ($t:expr) => {
        $t == XML_SCHEMA_SCHEMA_INCLUDE || $t == XML_SCHEMA_SCHEMA_REDEFINE
    };
}

macro_rules! WXS_IS_BUCKET_IMPMAIN {
    ($t:expr) => {
        $t == XML_SCHEMA_SCHEMA_MAIN || $t == XML_SCHEMA_SCHEMA_IMPORT
    };
}

macro_rules! WXS_IMPBUCKET {
    ($b:expr) => {
        $b as XmlSchemaImportPtr
    };
}

macro_rules! WXS_INCBUCKET {
    ($b:expr) => {
        $b as XmlSchemaIncludePtr
    };
}

macro_rules! WXS_IS_TYPE_NOT_FIXED {
    ($i:expr) => {
        (*$i).typ != XmlSchemaTypeType::XmlSchemaTypeBasic
            && (*$i).flags & XML_SCHEMAS_TYPE_INTERNAL_RESOLVED == 0
    };
}

macro_rules! WXS_IS_TYPE_NOT_FIXED_1 {
    ($item:expr) => {
        (*$item).typ != XmlSchemaTypeType::XmlSchemaTypeBasic
            && (*$item).flags & XML_SCHEMAS_TYPE_FIXUP_1 == 0
    };
}

macro_rules! WXS_HAS_SIMPLE_CONTENT {
    ($item:expr) => {
        (*$item).content_type == XmlSchemaContentType::XmlSchemaContentSimple
            || (*$item).content_type == XmlSchemaContentType::XmlSchemaContentBasic
    };
}

macro_rules! WXS_HAS_MIXED_CONTENT {
    ($item:expr) => {
        (*$item).content_type == XmlSchemaContentType::XmlSchemaContentMixed
    };
}

macro_rules! WXS_EMPTIABLE {
    ($t:expr) => {
        xml_schema_is_particle_emptiable((*$t).subtypes as XmlSchemaParticlePtr) != 0
    };
}

macro_rules! WXS_TYPE_PARTICLE {
    ($t:expr) => {
        (*$t).subtypes as XmlSchemaParticlePtr
    };
}

macro_rules! WXS_TYPE_PARTICLE_TERM {
    ($t:expr) => {
        WXS_PARTICLE_TERM!(WXS_TYPE_PARTICLE!($t))
    };
}

// Misc parser context macros.
macro_rules! WXS_CONSTRUCTOR {
    ($ctx:expr) => {
        (*$ctx).constructor
    };
}

macro_rules! WXS_HAS_BUCKETS {
    ($ctx:expr) => {
        !(*WXS_CONSTRUCTOR!($ctx)).buckets.is_null()
            && (*(*WXS_CONSTRUCTOR!($ctx)).buckets).items.len() > 0
    };
}

macro_rules! WXS_SUBST_GROUPS {
    ($ctx:expr) => {
        (*WXS_CONSTRUCTOR!($ctx)).subst_groups
    };
}

macro_rules! WXS_BUCKET {
    ($ctx:expr) => {
        (*WXS_CONSTRUCTOR!($ctx)).bucket
    };
}

macro_rules! WXS_ADD_LOCAL {
    ($ctx:expr, $item:expr) => {
        if xml_schema_add_item_size(&raw mut (*WXS_BUCKET!($ctx)).locals, 10, $item as _) < 0 {
            xml_free($item as _);
            $item = null_mut();
        }
    };
}

macro_rules! WXS_ADD_GLOBAL {
    ($ctx:expr, $item:expr) => {
        if xml_schema_add_item_size(&raw mut (*WXS_BUCKET!($ctx)).globals, 5, $item as _) < 0 {
            xml_free($item as _);
            $item = null_mut();
        }
    };
}

macro_rules! WXS_ADD_PENDING {
    ($ctx:expr, $item:expr) => {
        xml_schema_add_item_size(&raw mut (*(*$ctx).constructor).pending, 10, $item as _)
    };
}
// xmlSchemaItemList macros.
macro_rules! WXS_ILIST_IS_EMPTY {
    ($l:expr) => {
        $l.is_null() || (*$l).items.is_empty()
    };
}
// Misc macros.

macro_rules! FREE_AND_NULL {
    ($str:expr) => {
        if !$str.is_null() {
            $crate::libxml::globals::xml_free($str as _);
            #[allow(unused_assignments)]
            {
                $str = null_mut();
            }
        }
    };
}

// Since we put the default/fixed values into the dict, we can
// use pointer comparison for those values.
// REMOVED: (xml_str_equal((v1), (v2)))
macro_rules! WXS_ARE_DEFAULT_STR_EQUAL {
    ($v1:expr, $v2:expr) => {
        $v1 == $v2
    };
}

macro_rules! INODE_NILLED {
    ($item:expr) => {
        (*$item).flags & XML_SCHEMA_ELEM_INFO_NILLED != 0
    };
}

pub(crate) unsafe fn can_parse_schema(b: XmlSchemaBucketPtr) -> bool {
    unsafe { (*b).doc.is_some() && (*b).parsed == 0 }
}

/// $label must be `'exit_failure`.
macro_rules! HFAILURE {
    ($res:expr, $label:tt) => {
        if $res == -1 {
            // goto exit_failure;
            break $label;
        }
    };
}

/// $label must be `'exit_error`.
macro_rules! HERROR {
    ($res:expr, $label:tt) => {
        if $res != 0 {
            // goto exit_error;
            break $label;
        }
    };
}

// Some flags used for various schema constraints.
const SUBSET_RESTRICTION: i32 = 1 << 0;
const SUBSET_EXTENSION: i32 = 1 << 1;
const SUBSET_SUBSTITUTION: i32 = 1 << 2;
const SUBSET_LIST: i32 = 1 << 3;
const SUBSET_UNION: i32 = 1 << 4;

pub(crate) const XML_SCHEMA_CTXT_PARSER: i32 = 1;
pub(crate) const XML_SCHEMA_CTXT_VALIDATOR: i32 = 2;

#[doc(alias = "xmlSchemaAbstractCtxtPtr")]
pub type XmlSchemaAbstractCtxtPtr = *mut XmlSchemaAbstractCtxt;
#[doc(alias = "xmlSchemaAbstractCtxt")]
#[repr(C)]
pub struct XmlSchemaAbstractCtxt {
    pub(crate) typ: i32,           /* E.g. XML_SCHEMA_CTXT_VALIDATOR */
    pub(crate) dummy: *mut c_void, /* Fix alignment issues */
}

pub(crate) const XML_SCHEMA_SCHEMA_MAIN: i32 = 0;
pub(crate) const XML_SCHEMA_SCHEMA_IMPORT: i32 = 1;
pub(crate) const XML_SCHEMA_SCHEMA_INCLUDE: i32 = 2;
pub(crate) const XML_SCHEMA_SCHEMA_REDEFINE: i32 = 3;

#[doc(alias = "xmlSchemaSchemaRelationPtr")]
pub type XmlSchemaSchemaRelationPtr = *mut XmlSchemaSchemaRelation;
/// Used to create a graph of schema relationships.
#[doc(alias = "xmlSchemaSchemaRelation")]
#[repr(C)]
pub struct XmlSchemaSchemaRelation {
    next: XmlSchemaSchemaRelationPtr,
    typ: i32, /* E.g. XML_SCHEMA_SCHEMA_IMPORT */
    import_namespace: *const XmlChar,
    bucket: XmlSchemaBucketPtr,
}

const XML_SCHEMA_BUCKET_MARKED: i32 = 1 << 0;
const XML_SCHEMA_BUCKET_COMPS_ADDED: i32 = 1 << 1;

#[doc(alias = "xmlSchemaBucketPtr")]
pub type XmlSchemaBucketPtr = *mut XmlSchemaBucket;
#[doc(alias = "xmlSchemaBucket")]
#[repr(C)]
pub struct XmlSchemaBucket {
    typ: i32,
    flags: i32,
    schema_location: *const XmlChar,
    pub(crate) orig_target_namespace: *const XmlChar,
    pub(crate) target_namespace: *const XmlChar,
    pub(crate) doc: Option<XmlDocPtr>,
    relations: XmlSchemaSchemaRelationPtr,
    located: i32,
    pub(crate) parsed: i32,
    imported: i32,
    preserve_doc: i32,
    globals: XmlSchemaItemListPtr<*mut c_void>, /* Global components. */
    locals: XmlSchemaItemListPtr<*mut c_void>,  /* Local components. */
}

/// Extends xmlSchemaBucket
///
/// Reflects a schema. Holds some information
/// about the schema and its toplevel components. Duplicate
/// toplevel components are not checked at this level.
#[doc(alias = "xmlSchemaImportPtr")]
pub type XmlSchemaImportPtr = *mut XmlSchemaImport;
#[doc(alias = "xmlSchemaImport")]
#[repr(C)]
pub struct XmlSchemaImport {
    typ: i32, /* Main OR import OR include. */
    flags: i32,
    schema_location: *const XmlChar, /* The URI of the schema document. */
    // For chameleon includes, @origTargetNamespace will be NULL
    orig_target_namespace: *const XmlChar,
    // For chameleon includes, @target_namespace will be the
    // target_namespace of the including schema.
    target_namespace: *const XmlChar,
    doc: Option<XmlDocPtr>, /* The schema node-tree. */
    // @relations will hold any included/imported/redefined schemas.
    relations: XmlSchemaSchemaRelationPtr,
    located: i32,
    parsed: i32,
    imported: i32,
    preserve_doc: i32,
    globals: XmlSchemaItemListPtr<*mut c_void>,
    locals: XmlSchemaItemListPtr<*mut c_void>,
    // The imported schema.
    pub(crate) schema: XmlSchemaPtr,
}

// Extends xmlSchemaBucket
#[doc(alias = "xmlSchemaIncludePtr")]
pub type XmlSchemaIncludePtr = *mut XmlSchemaInclude;
#[doc(alias = "xmlSchemaInclude")]
#[repr(C)]
pub struct XmlSchemaInclude {
    typ: i32,
    flags: i32,
    schema_location: *const XmlChar,
    orig_target_namespace: *const XmlChar,
    target_namespace: *const XmlChar,
    doc: Option<XmlDocPtr>,
    relations: XmlSchemaSchemaRelationPtr,
    located: i32,
    parsed: i32,
    imported: i32,
    preserve_doc: i32,
    globals: XmlSchemaItemListPtr<*mut c_void>, /* Global components. */
    locals: XmlSchemaItemListPtr<*mut c_void>,  /* Local components. */

    // The owning main or import schema bucket.
    owner_import: XmlSchemaImportPtr,
}

const XML_SCHEMA_ATTR_USE_FIXED: i32 = 1 << 0;

#[doc(alias = "xmlSchemaRedefPtr")]
pub type XmlSchemaRedefPtr = *mut XmlSchemaRedef;
#[doc(alias = "xmlSchemaRedef")]
#[repr(C)]
pub struct XmlSchemaRedef {
    next: XmlSchemaRedefPtr,
    item: XmlSchemaBasicItemPtr,      /* The redefining component. */
    reference: XmlSchemaBasicItemPtr, /* The referencing component. */
    target: XmlSchemaBasicItemPtr,    /* The to-be-redefined component. */
    ref_name: *const XmlChar,         /* The name of the to-be-redefined component. */
    ref_target_ns: *const XmlChar,    /* The target namespace of the
                                      to-be-redefined comp. */
    target_bucket: XmlSchemaBucketPtr, /* The redefined schema. */
}

#[doc(alias = "xmlSchemaConstructionCtxtPtr")]
pub type XmlSchemaConstructionCtxtPtr = *mut XmlSchemaConstructionCtxt;
#[doc(alias = "xmlSchemaConstructionCtxt")]
#[repr(C)]
pub struct XmlSchemaConstructionCtxt {
    pub(crate) main_schema: XmlSchemaPtr, /* The main schema. */
    pub(crate) main_bucket: XmlSchemaBucketPtr, /* The main schema bucket */
    dict: XmlDictPtr,
    buckets: XmlSchemaItemListPtr<*mut c_void>, /* List of schema buckets. */
    // xmlSchemaItemListPtr relations; /* List of schema relations. */
    pub(crate) bucket: XmlSchemaBucketPtr, /* The current schema bucket */
    pub(crate) pending: XmlSchemaItemListPtr<*mut c_void>, /* All Components of all schemas that
                                           need to be fixed. */
    subst_groups: XmlHashTablePtr,
    redefs: XmlSchemaRedefPtr,
    last_redef: XmlSchemaRedefPtr,
}

const XML_SCHEMAS_PARSE_ERROR: i32 = 1;
const SCHEMAS_PARSE_OPTIONS: i32 = XmlParserOption::XmlParseNoEnt as i32;

const XML_SCHEMA_MODEL_GROUP_DEF_MARKED: i32 = 1 << 0;
const XML_SCHEMA_MODEL_GROUP_DEF_REDEFINED: i32 = 1 << 1;

#[doc(alias = "xmlSchemaIDCSelectPtr")]
pub type XmlSchemaIdcselectPtr = *mut XmlSchemaIdcselect;
/// The identity-constraint "field" and "selector" item, holding the XPath expression.
#[doc(alias = "xmlSchemaIDCSelect")]
#[repr(C)]
pub struct XmlSchemaIdcselect {
    next: XmlSchemaIdcselectPtr,
    idc: XmlSchemaIDCPtr,
    index: i32,              /* an index position if significant for IDC key-sequences */
    xpath: *const XmlChar,   /* the XPath expression */
    xpath_comp: *mut c_void, /* the compiled XPath expression */
}

#[doc(alias = "xmlSchemaIDCAugPtr")]
pub type XmlSchemaIDCAugPtr = *mut XmlSchemaIDCAug;
/// The augmented IDC information used for validation.
#[doc(alias = "xmlSchemaIDCAug")]
#[repr(C)]
pub struct XmlSchemaIDCAug {
    pub(crate) next: XmlSchemaIDCAugPtr, /* next in a list */
    def: XmlSchemaIDCPtr,                /* the IDC definition */
    keyref_depth: i32,                   /* the lowest tree level to which IDC
                                         tables need to be bubbled upwards */
}

#[doc(alias = "xmlSchemaPSVIIDCKeyPtr")]
pub type XmlSchemaPSVIIDCKeyPtr = *mut XmlSchemaPSVIIDCKey;
/// The key sequence of a node table item.
#[doc(alias = "xmlSchemaPSVIIDCKeySequence")]
#[repr(C)]
pub struct XmlSchemaPSVIIDCKey {
    typ: XmlSchemaTypePtr,
    val: XmlSchemaValPtr,
}

#[doc(alias = "xmlSchemaPSVIIDCNodePtr")]
pub type XmlSchemaPSVIIDCNodePtr = *mut XmlSchemaPSVIIDCNode;
/// The node table item of a node table.
#[doc(alias = "xmlSchemaPSVIIDCNode")]
#[repr(C)]
pub struct XmlSchemaPSVIIDCNode {
    node: Option<XmlNodePtr>,
    pub(crate) keys: *mut XmlSchemaPSVIIDCKeyPtr,
    pub(crate) node_line: i32,
    pub(crate) node_qname_id: i32,
}

#[doc(alias = "xmlSchemaPSVIIDCBindingPtr")]
pub type XmlSchemaPSVIIDCBindingPtr = *mut XmlSchemaPSVIIDCBinding;
/// The identity-constraint binding item of the [identity-constraint table].
#[doc(alias = "xmlSchemaPSVIIDCBinding")]
#[repr(C)]
pub struct XmlSchemaPSVIIDCBinding {
    next: XmlSchemaPSVIIDCBindingPtr, /* next binding of a specific node */
    definition: XmlSchemaIDCPtr,      /* the IDC definition */
    node_table: Vec<XmlSchemaPSVIIDCNodePtr>, /* array of key-sequences */
    // nb_nodes: i32,                    /* number of entries in the node table */
    // size_nodes: i32,                  /* size of the node table */
    dupls: XmlSchemaItemListPtr<*mut c_void>,
}

impl Default for XmlSchemaPSVIIDCBinding {
    fn default() -> Self {
        Self {
            next: null_mut(),
            definition: null_mut(),
            node_table: vec![],
            dupls: null_mut(),
        }
    }
}

const XPATH_STATE_OBJ_TYPE_IDC_SELECTOR: i32 = 1;
const XPATH_STATE_OBJ_TYPE_IDC_FIELD: i32 = 2;

const XPATH_STATE_OBJ_MATCHES: i32 = -2;
const XPATH_STATE_OBJ_BLOCKED: i32 = -3;

#[doc(alias = "xmlSchemaIDCStateObjPtr")]
pub type XmlSchemaIDCStateObjPtr = *mut XmlSchemaIDCStateObj;
/// The state object used to evaluate XPath expressions.
#[doc(alias = "xmlSchemaIDCStateObj")]
#[repr(C)]
pub struct XmlSchemaIDCStateObj {
    typ: i32,
    next: XmlSchemaIDCStateObjPtr, /* next if in a list */
    depth: i32,                    /* depth of creation */
    history: *mut i32,             /* list of (depth, state-id) tuples */
    nb_history: i32,
    size_history: i32,
    matcher: XmlSchemaIDCMatcherPtr, /* the correspondent field/selector
                                     matcher */
    sel: XmlSchemaIdcselectPtr,
    xpath_ctxt: *mut c_void,
}

const IDC_MATCHER: i32 = 0;

#[doc(alias = "xmlSchemaIDCMatcherPtr")]
pub type XmlSchemaIDCMatcherPtr = *mut XmlSchemaIDCMatcher;
/// Used to evaluate IDC selectors (and fields).
#[doc(alias = "xmlSchemaIDCMatcher")]
#[repr(C)]
pub struct XmlSchemaIDCMatcher {
    typ: i32,
    depth: i32,                          /* the tree depth at creation time */
    next: XmlSchemaIDCMatcherPtr,        /* next in the list */
    next_cached: XmlSchemaIDCMatcherPtr, /* next in the cache list */
    aidc: XmlSchemaIDCAugPtr,            /* the augmented IDC item */
    idc_type: i32,
    key_seqs: *mut *mut XmlSchemaPSVIIDCKeyPtr, /* the key-sequences of the target
                                                elements */
    size_key_seqs: i32,
    targets: XmlSchemaItemListPtr<*mut c_void>, /* list of target-node
                                                (xmlSchemaPSVIIDCNodePtr) entries */
    htab: XmlHashTablePtr,
}

// Element info flags.
const XML_SCHEMA_NODE_INFO_FLAG_OWNED_NAMES: i32 = 1 << 0;
const XML_SCHEMA_NODE_INFO_FLAG_OWNED_VALUES: i32 = 1 << 1;
const XML_SCHEMA_ELEM_INFO_NILLED: i32 = 1 << 2;
const XML_SCHEMA_ELEM_INFO_LOCAL_TYPE: i32 = 1 << 3;

const XML_SCHEMA_NODE_INFO_VALUE_NEEDED: i32 = 1 << 4;
const XML_SCHEMA_ELEM_INFO_EMPTY: i32 = 1 << 5;
const XML_SCHEMA_ELEM_INFO_HAS_CONTENT: i32 = 1 << 6;

const XML_SCHEMA_ELEM_INFO_HAS_ELEM_CONTENT: i32 = 1 << 7;
const XML_SCHEMA_ELEM_INFO_ERR_BAD_CONTENT: i32 = 1 << 8;
const XML_SCHEMA_NODE_INFO_ERR_NOT_EXPECTED: i32 = 1 << 9;
const XML_SCHEMA_NODE_INFO_ERR_BAD_TYPE: i32 = 1 << 10;

#[doc(alias = "xmlSchemaNodeInfoPtr")]
pub type XmlSchemaNodeInfoPtr = *mut XmlSchemaNodeInfo;
/// Holds information of an element node.
#[doc(alias = "xmlSchemaNodeInfo")]
#[repr(C)]
pub struct XmlSchemaNodeInfo {
    pub(crate) node_type: i32,
    pub(crate) node: Option<XmlNodePtr>,
    node_line: i32,
    pub(crate) local_name: *const XmlChar,
    pub(crate) ns_name: *const XmlChar,
    value: *const XmlChar,
    val: XmlSchemaValPtr,       /* the pre-computed value if any */
    type_def: XmlSchemaTypePtr, /* the complex/simple type definition if any */

    flags: i32, /* combination of node info flags */

    val_needed: i32,
    norm_val: i32,

    decl: XmlSchemaElementPtr, /* the element/attribute declaration */
    depth: i32,
    idc_table: XmlSchemaPSVIIDCBindingPtr, /* the table of PSVI IDC bindings
                                           for the scope element*/
    idc_matchers: XmlSchemaIDCMatcherPtr, /* the IDC matchers for the scope
                                          element */
    regex_ctxt: XmlRegExecCtxtPtr,

    pub(crate) ns_bindings: *mut *const XmlChar, /* Namespace bindings on this element */
    pub(crate) nb_ns_bindings: i32,
    size_ns_bindings: i32,

    has_keyrefs: i32,
    applied_xpath: i32, /* Indicates that an XPath has been applied. */
}

const XML_SCHEMAS_ATTR_UNKNOWN: i32 = 1;
const XML_SCHEMAS_ATTR_ASSESSED: i32 = 2;
const XML_SCHEMAS_ATTR_PROHIBITED: i32 = 3;
const XML_SCHEMAS_ATTR_ERR_MISSING: i32 = 4;
const XML_SCHEMAS_ATTR_INVALID_VALUE: i32 = 5;
const XML_SCHEMAS_ATTR_ERR_NO_TYPE: i32 = 6;
const XML_SCHEMAS_ATTR_ERR_FIXED_VALUE: i32 = 7;
const XML_SCHEMAS_ATTR_DEFAULT: i32 = 8;
const XML_SCHEMAS_ATTR_VALIDATE_VALUE: i32 = 9;
const XML_SCHEMAS_ATTR_ERR_WILD_STRICT_NO_DECL: i32 = 10;
const XML_SCHEMAS_ATTR_HAS_ATTR_USE: i32 = 11;
const XML_SCHEMAS_ATTR_HAS_ATTR_DECL: i32 = 12;
const XML_SCHEMAS_ATTR_WILD_SKIP: i32 = 13;
const XML_SCHEMAS_ATTR_WILD_LAX_NO_DECL: i32 = 14;
const XML_SCHEMAS_ATTR_ERR_WILD_DUPLICATE_ID: i32 = 15;
const XML_SCHEMAS_ATTR_ERR_WILD_AND_USE_ID: i32 = 16;
const XML_SCHEMAS_ATTR_META: i32 = 17;
// @metaType values of xmlSchemaAttrInfo.
const XML_SCHEMA_ATTR_INFO_META_XSI_TYPE: i32 = 1;
const XML_SCHEMA_ATTR_INFO_META_XSI_NIL: i32 = 2;
const XML_SCHEMA_ATTR_INFO_META_XSI_SCHEMA_LOC: i32 = 3;
const XML_SCHEMA_ATTR_INFO_META_XSI_NO_NS_SCHEMA_LOC: i32 = 4;
const XML_SCHEMA_ATTR_INFO_META_XMLNS: i32 = 5;

#[doc(alias = "xmlSchemaAttrInfoPtr")]
pub type XmlSchemaAttrInfoPtr = *mut XmlSchemaAttrInfo;
#[doc(alias = "xmlSchemaAttrInfo")]
#[repr(C)]
pub struct XmlSchemaAttrInfo {
    node_type: i32,
    node: Option<XmlAttrPtr>,
    node_line: i32,
    local_name: *const XmlChar,
    ns_name: *const XmlChar,
    value: *const XmlChar,
    val: XmlSchemaValPtr,       /* the pre-computed value if any */
    type_def: XmlSchemaTypePtr, /* the complex/simple type definition if any */
    flags: i32,                 /* combination of node info flags */

    decl: XmlSchemaAttributePtr,     /* the attribute declaration */
    using: XmlSchemaAttributeUsePtr, /* the attribute use */
    state: i32,
    meta_type: i32,
    vc_value: *const XmlChar, /* the value constraint value */
    parent: XmlSchemaNodeInfoPtr,
}

const XML_SCHEMA_VALID_CTXT_FLAG_STREAM: i32 = 1;

#[doc(alias = "xmlSchemaSubstGroupPtr")]
pub type XmlSchemaSubstGroupPtr = *mut XmlSchemaSubstGroup;
#[doc(alias = "xmlSchemaSubstGroup")]
#[repr(C)]
pub struct XmlSchemaSubstGroup {
    head: XmlSchemaElementPtr,
    members: XmlSchemaItemListPtr<*mut c_void>,
}

#[doc(alias = "xmlIDCHashEntryPtr")]
pub type XmlIDCHashEntryPtr = *mut XmlIDCHashEntry;
/// an entry in hash tables to quickly look up keys/uniques
#[doc(alias = "xmlIDCHashEntry")]
#[repr(C)]
pub struct XmlIDCHashEntry {
    next: XmlIDCHashEntryPtr, /* next item with same hash */
    index: i32,               /* index into associated item list */
}

pub(crate) unsafe fn xml_schema_add_item_size(
    list: *mut XmlSchemaItemListPtr<*mut c_void>,
    _initial_size: i32,
    item: *mut c_void,
) -> i32 {
    unsafe {
        if (*list).is_null() {
            *list = xml_schema_item_list_create::<*mut c_void>();
            if (*list).is_null() {
                return -1;
            }
        }
        (**list).push(item)
    }
}

/// Returns the component name of a schema item.
#[doc(alias = "xmlSchemaItemTypeToStr")]
pub(crate) fn xml_schema_item_type_to_str(typ: XmlSchemaTypeType) -> &'static str {
    match typ {
        XmlSchemaTypeType::XmlSchemaTypeBasic => "simple type definition",
        XmlSchemaTypeType::XmlSchemaTypeSimple => "simple type definition",
        XmlSchemaTypeType::XmlSchemaTypeComplex => "complex type definition",
        XmlSchemaTypeType::XmlSchemaTypeElement => "element declaration",
        XmlSchemaTypeType::XmlSchemaTypeAttributeUse => "attribute use",
        XmlSchemaTypeType::XmlSchemaTypeAttribute => "attribute declaration",
        XmlSchemaTypeType::XmlSchemaTypeGroup => "model group definition",
        XmlSchemaTypeType::XmlSchemaTypeAttributeGroup => "attribute group definition",
        XmlSchemaTypeType::XmlSchemaTypeNotation => "notation declaration",
        XmlSchemaTypeType::XmlSchemaTypeSequence => "model group (sequence)",
        XmlSchemaTypeType::XmlSchemaTypeChoice => "model group (choice)",
        XmlSchemaTypeType::XmlSchemaTypeAll => "model group (all)",
        XmlSchemaTypeType::XmlSchemaTypeParticle => "particle",
        XmlSchemaTypeType::XmlSchemaTypeIDCUnique => {
            "unique identity-constraint"
            // return ("IDC (unique)");
        }
        XmlSchemaTypeType::XmlSchemaTypeIDCKey => {
            "key identity-constraint"
            // return ("IDC (key)");
        }
        XmlSchemaTypeType::XmlSchemaTypeIDCKeyref => {
            "keyref identity-constraint"
            // return ("IDC (keyref)");
        }
        XmlSchemaTypeType::XmlSchemaTypeAny => "wildcard (any)",
        XmlSchemaTypeType::XmlSchemaExtraQNameRef => "[helper component] QName reference",
        XmlSchemaTypeType::XmlSchemaExtraAttrUseProhib => {
            "[helper component] attribute use prohibition"
        }
        _ => "Not a schema component",
    }
}

/// Returns the component name of a schema item.
pub(crate) unsafe fn xml_schema_get_component_type_str(
    item: XmlSchemaBasicItemPtr,
) -> &'static str {
    unsafe {
        match (*item).typ {
            XmlSchemaTypeType::XmlSchemaTypeBasic => {
                if wxs_is_complex(item as XmlSchemaTypePtr) {
                    "complex type definition"
                } else {
                    "simple type definition"
                }
            }
            _ => xml_schema_item_type_to_str((*item).typ),
        }
    }
}

pub(crate) unsafe fn xml_schema_get_component_qname(item: *mut c_void) -> String {
    unsafe {
        let namespace_name = (*(item as XmlSchemaBasicItemPtr)).target_namespace();
        let local_name = (*(item as XmlSchemaBasicItemPtr)).name();
        xml_schema_format_qname(namespace_name.as_deref(), local_name.as_deref())
    }
}

pub(crate) unsafe fn xml_schema_get_component_designation(item: *mut c_void) -> String {
    unsafe {
        let typestr = xml_schema_get_component_type_str(item as _);
        let mut res = typestr.to_owned();
        res.push_str(" '");
        res.push_str(xml_schema_get_component_qname(item).as_str());
        res.push('\'');
        res
    }
}

macro_rules! VERROR {
    ($vctxt:expr, $err:expr, $typ:expr, $msg:expr) => {
        $crate::xmlschemas::error::xml_schema_custom_err(
            $vctxt as XmlSchemaAbstractCtxtPtr,
            $err,
            None,
            $typ,
            $msg,
            None,
            None,
        );
    };
}

macro_rules! VERROR_INT {
    ($vctxt:expr, $func:expr, $msg:expr) => {
        $crate::xmlschemas::error::xml_schema_internal_err(
            $vctxt as XmlSchemaAbstractCtxtPtr,
            $func,
            $msg,
        );
    };
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
macro_rules! PERROR_INT2 {
    ($ctxt:expr, $func:expr, $msg:expr) => {
        $crate::xmlschemas::error::xml_schema_internal_err(
            $ctxt as XmlSchemaAbstractCtxtPtr,
            $func,
            $msg,
        );
    };
}

macro_rules! AERROR_INT {
    ($actxt:expr, $func:expr, $msg:expr) => {
        $crate::xmlschemas::error::xml_schema_internal_err($actxt, $func, $msg);
    };
}

/// Returns node associated with the schema component.
/// NOTE that such a node need not be available; plus, a component's
/// node need not to reflect the component directly, since there is no
/// one-to-one relationship between the XML Schema representation and
/// the component representation.
pub(crate) unsafe fn xml_schema_get_component_node(
    item: XmlSchemaBasicItemPtr,
) -> Option<XmlNodePtr> {
    unsafe {
        match (*item).typ {
            XmlSchemaTypeType::XmlSchemaTypeElement => (*(item as XmlSchemaElementPtr)).node,
            XmlSchemaTypeType::XmlSchemaTypeAttribute => (*(item as XmlSchemaAttributePtr)).node,
            XmlSchemaTypeType::XmlSchemaTypeComplex | XmlSchemaTypeType::XmlSchemaTypeSimple => {
                (*(item as XmlSchemaTypePtr)).node
            }
            XmlSchemaTypeType::XmlSchemaTypeAny | XmlSchemaTypeType::XmlSchemaTypeAnyAttribute => {
                (*(item as XmlSchemaWildcardPtr)).node
            }
            XmlSchemaTypeType::XmlSchemaTypeParticle => (*(item as XmlSchemaParticlePtr)).node,
            XmlSchemaTypeType::XmlSchemaTypeSequence
            | XmlSchemaTypeType::XmlSchemaTypeChoice
            | XmlSchemaTypeType::XmlSchemaTypeAll => Some((*(item as XmlSchemaModelGroupPtr)).node),
            XmlSchemaTypeType::XmlSchemaTypeGroup => {
                Some((*(item as XmlSchemaModelGroupDefPtr)).node)
            }
            XmlSchemaTypeType::XmlSchemaTypeAttributeGroup => {
                (*(item as XmlSchemaAttributeGroupPtr)).node
            }
            XmlSchemaTypeType::XmlSchemaTypeIDCUnique
            | XmlSchemaTypeType::XmlSchemaTypeIDCKey
            | XmlSchemaTypeType::XmlSchemaTypeIDCKeyref => Some((*(item as XmlSchemaIDCPtr)).node),
            XmlSchemaTypeType::XmlSchemaExtraQNameRef => (*(item as XmlSchemaQNameRefPtr)).node,
            // TODO: What to do with NOTATIONs?
            // XmlSchemaTypeType::XML_SCHEMA_TYPE_NOTATION:
            //     return ((*(item as xmlSchemaNotationPtr)).node);
            XmlSchemaTypeType::XmlSchemaTypeAttributeUse => {
                Some((*(item as XmlSchemaAttributeUsePtr)).node)
            }
            _ => None,
        }
    }
}

// This one works on the schema of the validation context.
unsafe fn xml_schema_validate_notation(
    vctxt: XmlSchemaValidCtxtPtr,
    schema: XmlSchemaPtr,
    node: Option<XmlGenericNodePtr>,
    value: *const XmlChar,
    val: *mut XmlSchemaValPtr,
    val_needed: i32,
) -> i32 {
    unsafe {
        let mut ret = 0;

        if !vctxt.is_null() && (*vctxt).schema.is_null() {
            VERROR_INT!(
                vctxt,
                "xmlSchemaValidateNotation",
                "a schema is needed on the validation context"
            );
            return -1;
        }
        if validate_qname::<true>(
            CStr::from_ptr(value as *const i8)
                .to_string_lossy()
                .as_ref(),
        )
        .is_err()
        {
            return 1;
        }
        let mut prefix: *mut XmlChar = null_mut();
        let local_name: *mut XmlChar = xml_split_qname2(value, &raw mut prefix);
        if !prefix.is_null() {
            let mut ns_name: *const XmlChar = null();

            if !vctxt.is_null() {
                ns_name = (*vctxt).lookup_namespace(
                    (!prefix.is_null())
                        .then(|| CStr::from_ptr(prefix as *const i8).to_string_lossy())
                        .as_deref(),
                );
            } else if let Some(node) = node {
                let ns = node.search_ns(
                    node.document(),
                    (!prefix.is_null())
                        .then(|| CStr::from_ptr(prefix as *const i8).to_string_lossy())
                        .as_deref(),
                );
                if let Some(ns) = ns {
                    ns_name = ns.href;
                }
            } else {
                xml_free(prefix as _);
                xml_free(local_name as _);
                return 1;
            }
            if ns_name.is_null() {
                xml_free(prefix as _);
                xml_free(local_name as _);
                return 1;
            }
            if !(*schema)
                .get_notation(
                    CStr::from_ptr(local_name as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    (!ns_name.is_null())
                        .then(|| CStr::from_ptr(ns_name as *const i8).to_string_lossy())
                        .as_deref(),
                )
                .is_null()
            {
                if val_needed != 0 && !val.is_null() {
                    *val =
                        xml_schema_new_notation_value(xml_strdup(local_name), xml_strdup(ns_name));
                    if (*val).is_null() {
                        ret = -1;
                    }
                }
            } else {
                ret = 1;
            }
            xml_free(prefix as _);
            xml_free(local_name as _);
        } else if !(*schema)
            .get_notation(
                CStr::from_ptr(value as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                None,
            )
            .is_null()
        {
            if val_needed != 0 && !val.is_null() {
                *val = xml_schema_new_notation_value(xml_strdup(value), null_mut());
                if (*val).is_null() {
                    ret = -1;
                }
            }
        } else {
            return 1;
        }
        ret
    }
}

unsafe fn xml_schema_validate_qname(
    vctxt: XmlSchemaValidCtxtPtr,
    value: *const XmlChar,
    val: *mut XmlSchemaValPtr,
    val_needed: i32,
) -> i32 {
    unsafe {
        let mut ret = 0;
        let mut local: *mut XmlChar;
        let mut prefix: *mut XmlChar = null_mut();

        if validate_qname::<true>(
            CStr::from_ptr(value as *const i8)
                .to_string_lossy()
                .as_ref(),
        )
        .is_err()
        {
            if ret == -1 {
                VERROR_INT!(
                    vctxt,
                    "xmlSchemaValidateQName",
                    "calling xmlValidateQName()"
                );
                return -1;
            }
            return XmlParserErrors::XmlSchemavCvcDatatypeValid1_2_1 as i32;
        }
        // NOTE: xmlSplitQName2 will always return a duplicated strings.
        // TODO: Export and use xmlSchemaStrip instead
        let stripped: *mut XmlChar = xml_schema_collapse_string(
            CStr::from_ptr(value as *const i8)
                .to_string_lossy()
                .as_ref(),
        )
        .map_or(null_mut(), |res| {
            xml_strndup(res.as_ptr(), res.len() as i32)
        });
        local = xml_split_qname2(
            if !stripped.is_null() { stripped } else { value },
            &raw mut prefix,
        );
        xml_free(stripped as _);
        if local.is_null() {
            local = xml_strdup(value);
        }
        // OPTIMIZE TODO: Use flags for:
        //  - is there any namespace binding?
        //  - is there a default namespace?
        let ns_name: *const XmlChar = (*vctxt).lookup_namespace(
            (!prefix.is_null())
                .then(|| CStr::from_ptr(prefix as *const i8).to_string_lossy())
                .as_deref(),
        );

        if !prefix.is_null() {
            xml_free(prefix as _);
            // A namespace must be found if the prefix is NOT NULL.
            if ns_name.is_null() {
                ret = XmlParserErrors::XmlSchemavCvcDatatypeValid1_2_1 as i32;
                let value = CStr::from_ptr(value as *const i8).to_string_lossy();
                xml_schema_custom_err(
                vctxt as XmlSchemaAbstractCtxtPtr,
                XmlParserErrors::try_from(ret).unwrap(),
                None,
                xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasQName) as _,
                format!(
                    "The QName value '{value}' has no corresponding namespace declaration in scope"
                )
                .as_str(),
                Some(&value),
                None,
            );
                if !local.is_null() {
                    xml_free(local as _);
                }
                return ret;
            }
        }
        if val_needed != 0 && !val.is_null() {
            if !ns_name.is_null() {
                *val = xml_schema_new_qname_value(xml_strdup(ns_name), local);
            } else {
                *val = xml_schema_new_qname_value(null_mut(), local);
            }
        } else {
            xml_free(local as _);
        }
        0
    }
}

/// Returns the primitive type of the given type or NULL in case of error.
#[doc(alias = "xmlSchemaGetPrimitiveType")]
unsafe fn xml_schema_get_primitive_type(mut typ: XmlSchemaTypePtr) -> XmlSchemaTypePtr {
    unsafe {
        while !typ.is_null() {
            // Note that anySimpleType is actually not a primitive type
            // but we need that here.
            if (*typ).built_in_type == XmlSchemaValType::XmlSchemasAnySimpletype as i32
                || (*typ).flags & XML_SCHEMAS_TYPE_BUILTIN_PRIMITIVE != 0
            {
                return typ;
            }
            typ = (*typ).base_type;
        }

        null_mut()
    }
}

/// Get a the canonical representation of the value.
/// The caller has to free the returned retValue.
///
/// Returns 0 if the value could be built and -1 in case of
/// API errors or if the value type is not supported yet.
#[doc(alias = "xmlSchemaGetCanonValueWhtspExt")]
unsafe fn xml_schema_get_canon_value_whtsp_ext_1(
    mut val: XmlSchemaValPtr,
    ws: XmlSchemaWhitespaceValueType,
    ret_value: *mut *mut XmlChar,
    for_hash: i32,
) -> i32 {
    unsafe {
        let mut val_type: XmlSchemaValType;
        let mut value: *const XmlChar;
        let mut value2: *const XmlChar = null();

        if ret_value.is_null() || val.is_null() {
            return -1;
        }
        let list: i32 = (!xml_schema_value_get_next(val).is_null()) as i32;
        *ret_value = null_mut();
        while {
            // value = null_mut();
            val_type = xml_schema_get_val_type(val);
            match val_type {
                XmlSchemaValType::XmlSchemasString
                | XmlSchemaValType::XmlSchemasNormString
                | XmlSchemaValType::XmlSchemasAnySimpletype => {
                    value = xml_schema_value_get_as_string(val);
                    if !value.is_null() {
                        if ws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse {
                            value2 = xml_schema_collapse_string(
                                CStr::from_ptr(value as *const i8)
                                    .to_string_lossy()
                                    .as_ref(),
                            )
                            .map_or(null_mut(), |res| {
                                xml_strndup(res.as_ptr(), res.len() as i32)
                            });
                        } else if ws == XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceReplace {
                            value2 = xml_schema_white_space_replace(
                                CStr::from_ptr(value as *const i8)
                                    .to_string_lossy()
                                    .as_ref(),
                            )
                            .map_or(null_mut(), |res| {
                                xml_strndup(res.as_ptr(), res.len() as i32)
                            });
                        }
                        if !value2.is_null() {
                            value = value2;
                        }
                    }
                }
                _ => {
                    if xml_schema_get_canon_value(val, &raw mut value2) == -1 {
                        if !value2.is_null() {
                            xml_free(value2 as _);
                        }
                        // goto internal_error;
                        if !(*ret_value).is_null() {
                            xml_free(*ret_value as _);
                        }
                        if !value2.is_null() {
                            xml_free(value2 as _);
                        }
                        return -1;
                    }
                    if for_hash != 0 && val_type == XmlSchemaValType::XmlSchemasDecimal {
                        // We can mostly use the canonical value for hashing,
                        // except in the case of decimal.  There the canonical
                        // representation requires a trailing '.0' even for
                        // non-fractional numbers, but for the derived integer
                        // types it forbids any decimal point.  Nevertheless they
                        // compare equal if the value is equal.  We need to generate
                        // the same hash value for this to work, and it's easiest
                        // to just cut off the useless '.0' suffix for the
                        // decimal type.
                        let len: i32 = xml_strlen(value2);
                        if len > 2
                            && *value2.add(len as usize - 1) == b'0'
                            && *value2.add(len as usize - 2) == b'.'
                        {
                            *(value2 as *mut u8).add(len as usize - 2) = 0;
                        }
                    }
                    value = value2;
                }
            }
            if (*ret_value).is_null() {
                if value.is_null() {
                    if list == 0 {
                        *ret_value = xml_strdup(c"".as_ptr() as _);
                    }
                } else {
                    *ret_value = xml_strdup(value);
                }
            } else if !value.is_null() {
                // List.
                *ret_value = xml_strcat(*ret_value, c" ".as_ptr() as _);
                *ret_value = xml_strcat(*ret_value, value);
            }
            FREE_AND_NULL!(value2);
            val = xml_schema_value_get_next(val);

            !val.is_null()
        } {}

        0
        // internal_error:
        //     if !(*retValue).is_null() {
        //         xml_free((*retValue) as _);
        //     }
        //     if !value2.is_null() {
        //         xml_free(value2 as _);
        //     }
        //     return -1;
    }
}

pub(crate) unsafe fn xml_schema_get_canon_value_whtsp_ext(
    val: XmlSchemaValPtr,
    ws: XmlSchemaWhitespaceValueType,
    ret_value: *mut *mut XmlChar,
) -> i32 {
    unsafe { xml_schema_get_canon_value_whtsp_ext_1(val, ws, ret_value, 0) }
}

unsafe fn xml_schema_are_values_equal(mut x: XmlSchemaValPtr, mut y: XmlSchemaValPtr) -> i32 {
    unsafe {
        let mut tx: XmlSchemaTypePtr;
        let mut ty: XmlSchemaTypePtr;
        let mut ptx: XmlSchemaTypePtr;
        let mut pty: XmlSchemaTypePtr;
        let mut ret: i32;

        while !x.is_null() {
            // Same types.
            tx = xml_schema_get_built_in_type(xml_schema_get_val_type(x));
            ty = xml_schema_get_built_in_type(xml_schema_get_val_type(y));
            ptx = xml_schema_get_primitive_type(tx);
            pty = xml_schema_get_primitive_type(ty);
            // (1) if a datatype T' is `derived` by `restriction` from an
            // atomic datatype T then the `value space` of T' is a subset of
            // the `value space` of T.
            // (2) if datatypes T' and T'' are `derived` by `restriction`
            // from a common atomic ancestor T then the `value space`s of T'
            // and T'' may overlap.
            if ptx != pty {
                return 0;
            }
            // We assume computed values to be normalized, so do a fast
            // string comparison for string based types.
            if (*ptx).built_in_type == XmlSchemaValType::XmlSchemasString as i32
                || wxs_is_any_simple_type(ptx)
            {
                if !xml_str_equal(
                    xml_schema_value_get_as_string(x),
                    xml_schema_value_get_as_string(y),
                ) {
                    return 0;
                }
            } else {
                ret = xml_schema_compare_values_whtsp(
                    x,
                    XmlSchemaWhitespaceValueType::XmlSchemaWhitespacePreserve,
                    y,
                    XmlSchemaWhitespaceValueType::XmlSchemaWhitespacePreserve,
                );
                if ret == -2 {
                    return -1;
                }
                if ret != 0 {
                    return 0;
                }
            }
            // Lists.
            x = xml_schema_value_get_next(x);
            if !x.is_null() {
                y = xml_schema_value_get_next(y);
                if y.is_null() {
                    return 0;
                }
            } else if !xml_schema_value_get_next(y).is_null() {
                return 0;
            } else {
                return 1;
            }
        }
        0
    }
}

macro_rules! ACTIVATE_ATTRIBUTE {
    ($vctxt:expr, $item:expr) => {
        (*$vctxt).inode = $item as XmlSchemaNodeInfoPtr;
    };
}
macro_rules! ACTIVATE_ELEM {
    ($vctxt:expr) => {
        (*$vctxt).inode = *(*$vctxt).elem_infos.add((*$vctxt).depth as usize);
    };
}
macro_rules! ACTIVATE_PARENT_ELEM {
    ($vctxt:expr) => {
        (*$vctxt).inode = *(*$vctxt).elem_infos.add((*$vctxt).depth as usize - 1);
    };
}

#[allow(clippy::too_many_arguments)]
unsafe fn xml_schema_validate_facets(
    actxt: XmlSchemaAbstractCtxtPtr,
    node: Option<XmlGenericNodePtr>,
    typ: XmlSchemaTypePtr,
    mut val_type: XmlSchemaValType,
    value: *const XmlChar,
    val: XmlSchemaValPtr,
    length: u64,
    fire_errors: i32,
) -> i32 {
    unsafe {
        let mut ret: i32;
        let mut error: i32 = 0;
        let mut found: i32;
        let mut tmp_type: XmlSchemaTypePtr;
        let mut facet_link: XmlSchemaFacetLinkPtr;
        let mut facet: XmlSchemaFacetPtr;
        let mut len: u64 = 0;
        let ws: XmlSchemaWhitespaceValueType;

        // In Libxml2, derived built-in types have currently no explicit facets.
        if (*typ).typ == XmlSchemaTypeType::XmlSchemaTypeBasic {
            return 0;
        }

        // NOTE: Do not jump away, if the facetSet of the given type is
        // empty: until now, "pattern" and "enumeration" facets of the
        // *base types* need to be checked as well.
        'pattern_and_enum: {
            if (*typ).facet_set.is_null() {
                break 'pattern_and_enum;
            }
            'wxs_is_list: {
                if !(*typ).wxs_is_atomic() {
                    if (*typ).wxs_is_list() {
                        break 'wxs_is_list;
                    } else {
                        break 'pattern_and_enum;
                    }
                }
                // Whitespace handling is only of importance for string-based types.
                tmp_type = xml_schema_get_primitive_type(typ);
                if (*tmp_type).built_in_type == XmlSchemaValType::XmlSchemasString as i32
                    || wxs_is_any_simple_type(tmp_type)
                {
                    ws = (*typ).white_space_facet_value().unwrap();
                } else {
                    ws = XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse;
                }

                // If the value was not computed (for string or
                // anySimpleType based types), then use the provided type.
                if !val.is_null() {
                    val_type = xml_schema_get_val_type(val);
                }

                // ret = 0;
                facet_link = (*typ).facet_set;
                while !facet_link.is_null() {
                    'to_continue: {
                        // Skip the pattern "whiteSpace": it is used to
                        // format the character content beforehand.
                        match (*(*facet_link).facet).typ {
                            XmlSchemaTypeType::XmlSchemaFacetWhitespace
                            | XmlSchemaTypeType::XmlSchemaFacetPattern
                            | XmlSchemaTypeType::XmlSchemaFacetEnumeration => {
                                break 'to_continue;
                            }
                            XmlSchemaTypeType::XmlSchemaFacetLength
                            | XmlSchemaTypeType::XmlSchemaFacetMinLength
                            | XmlSchemaTypeType::XmlSchemaFacetMaxLength => {
                                ret = xml_schema_validate_length_facet_whtsp(
                                    (*facet_link).facet,
                                    val_type,
                                    value,
                                    val,
                                    &raw mut len,
                                    ws,
                                );
                            }
                            _ => {
                                ret = xml_schema_validate_facet_whtsp(
                                    (*facet_link).facet,
                                    ws,
                                    val_type,
                                    value,
                                    val,
                                    ws,
                                );
                            }
                        }
                        match ret.cmp(&0) {
                            std::cmp::Ordering::Less => {
                                AERROR_INT!(
                                    actxt,
                                    "xmlSchemaValidateFacets",
                                    "validating against a atomic type facet"
                                );
                                return -1;
                            }
                            std::cmp::Ordering::Greater => {
                                if fire_errors != 0 {
                                    let value =
                                        CStr::from_ptr(value as *const i8).to_string_lossy();
                                    xml_schema_facet_err(
                                        actxt,
                                        ret.try_into().unwrap(),
                                        node,
                                        &value,
                                        len,
                                        typ,
                                        (*facet_link).facet,
                                        None,
                                        None,
                                        None,
                                    );
                                } else {
                                    return ret;
                                }
                                if error == 0 {
                                    error = ret;
                                }
                            }
                            _ => {}
                        }
                        // ret = 0;
                    }
                    facet_link = (*facet_link).next;
                }
            }

            // WXS_IS_LIST:
            if !(*typ).wxs_is_list() {
                break 'pattern_and_enum;
            }
            // "length", "minLength" and "maxLength" of list types.
            // ret = 0;
            facet_link = (*typ).facet_set;
            while !facet_link.is_null() {
                'to_continue: {
                    match (*(*facet_link).facet).typ {
                        XmlSchemaTypeType::XmlSchemaFacetLength
                        | XmlSchemaTypeType::XmlSchemaFacetMinLength
                        | XmlSchemaTypeType::XmlSchemaFacetMaxLength => {
                            ret = xml_schema_validate_list_simple_type_facet(
                                (*facet_link).facet,
                                value,
                                length,
                                null_mut(),
                            );
                        }
                        _ => {
                            break 'to_continue;
                        }
                    }
                    match ret.cmp(&0) {
                        std::cmp::Ordering::Less => {
                            AERROR_INT!(
                                actxt,
                                "xmlSchemaValidateFacets",
                                "validating against a list type facet"
                            );
                            return -1;
                        }
                        std::cmp::Ordering::Greater => {
                            if fire_errors != 0 {
                                let value = CStr::from_ptr(value as *const i8).to_string_lossy();
                                xml_schema_facet_err(
                                    actxt,
                                    ret.try_into().unwrap(),
                                    node,
                                    &value,
                                    length,
                                    typ,
                                    (*facet_link).facet,
                                    None,
                                    None,
                                    None,
                                );
                            } else {
                                return ret;
                            }
                            if error == 0 {
                                error = ret;
                            }
                        }
                        _ => {}
                    }
                    // ret = 0;
                }
                facet_link = (*facet_link).next;
            }
        }

        // pattern_and_enum:
        found = 0;
        // Process enumerations. Facet values are in the value space
        // of the defining type's base type. This seems to be a bug in the
        // XML Schema 1.0 spec. Use the whitespace type of the base type.
        // Only the first set of enumerations in the ancestor-or-self axis
        // is used for validation.
        ret = 0;
        tmp_type = typ;
        loop {
            facet = (*tmp_type).facets;
            while !facet.is_null() {
                if (*facet).typ != XmlSchemaTypeType::XmlSchemaFacetEnumeration {
                    facet = (*facet).next;
                    continue;
                }
                found = 1;
                ret = xml_schema_are_values_equal((*facet).val, val);
                if ret == 1 {
                    break;
                } else if ret < 0 {
                    AERROR_INT!(
                        actxt,
                        "xmlSchemaValidateFacets",
                        "validating against an enumeration facet"
                    );
                    return -1;
                }
                facet = (*facet).next;
            }
            if ret != 0 {
                break;
            }
            // Break on the first set of enumerations. Any additional
            //  enumerations which might be existent on the ancestors
            //  of the current type are restricted by this set; thus
            //  *must* *not* be taken into account.
            if found != 0 {
                break;
            }
            tmp_type = (*tmp_type).base_type;

            if tmp_type.is_null() || (*tmp_type).typ == XmlSchemaTypeType::XmlSchemaTypeBasic {
                break;
            }
        }
        if found != 0 && ret == 0 {
            ret = XmlParserErrors::XmlSchemavCvcEnumerationValid as i32;
            if fire_errors != 0 {
                let value = CStr::from_ptr(value as *const i8).to_string_lossy();
                xml_schema_facet_err(
                    actxt,
                    ret.try_into().unwrap(),
                    node,
                    &value,
                    0,
                    typ,
                    null_mut(),
                    None,
                    None,
                    None,
                );
            } else {
                return ret;
            }
            if error == 0 {
                error = ret;
            }
        }

        // Process patters. Pattern facets are ORed at type level
        // and ANDed if derived. Walk the base type axis.
        tmp_type = typ;
        facet = null_mut();
        loop {
            found = 0;
            facet_link = (*tmp_type).facet_set;
            while !facet_link.is_null() {
                if (*(*facet_link).facet).typ != XmlSchemaTypeType::XmlSchemaFacetPattern {
                    facet_link = (*facet_link).next;
                    continue;
                }
                found = 1;
                // NOTE that for patterns, @value needs to be the normalized value.
                ret = xml_regexp_exec((*(*facet_link).facet).regexp, value);
                if ret == 1 {
                    break;
                } else if ret < 0 {
                    AERROR_INT!(
                        actxt,
                        "xmlSchemaValidateFacets",
                        "validating against a pattern facet"
                    );
                    return -1;
                } else {
                    // Save the last non-validating facet.
                    facet = (*facet_link).facet;
                }
                facet_link = (*facet_link).next;
            }
            if found != 0 && ret != 1 {
                ret = XmlParserErrors::XmlSchemavCvcPatternValid as i32;
                if fire_errors != 0 {
                    let value = CStr::from_ptr(value as *const i8).to_string_lossy();
                    xml_schema_facet_err(
                        actxt,
                        ret.try_into().unwrap(),
                        node,
                        &value,
                        0,
                        typ,
                        facet,
                        None,
                        None,
                        None,
                    );
                } else {
                    return ret;
                }
                if error == 0 {
                    error = ret;
                }
                break;
            }
            tmp_type = (*tmp_type).base_type;

            if tmp_type.is_null() || (*tmp_type).typ == XmlSchemaTypeType::XmlSchemaTypeBasic {
                break;
            }
        }

        error
    }
}

/// Returns a list of member types of @type if existing,
/// returns NULL otherwise.
#[doc(alias = "xmlSchemaGetUnionSimpleTypeMemberTypes")]
unsafe fn xml_schema_get_union_simple_type_member_types(
    mut typ: XmlSchemaTypePtr,
) -> XmlSchemaTypeLinkPtr {
    unsafe {
        while !typ.is_null() && (*typ).typ == XmlSchemaTypeType::XmlSchemaTypeSimple {
            if !(*typ).member_types.is_null() {
                return (*typ).member_types;
            } else {
                typ = (*typ).base_type;
            }
        }
        null_mut()
    }
}

macro_rules! NORMALIZE {
    ($atype:expr, $value:expr, $norm_value:expr, $typ:expr, $is_normalized:expr, $normalize:expr) => {
        if $is_normalized == 0
            && ($normalize != 0
                || (*$typ).flags
                    & $crate::libxml::schemas_internals::XML_SCHEMAS_TYPE_NORMVALUENEEDED
                    != 0)
        {
            $norm_value = (*$atype)
                .normalize_value(
                    CStr::from_ptr($value as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                )
                .map_or(null_mut(), |res| {
                    xml_strndup(res.as_ptr(), res.len() as i32)
                });
            if !$norm_value.is_null() {
                $value = $norm_value;
            }
            #[allow(unused_assignments)]
            {
                $is_normalized = 1;
            }
        }
    };
}

// cvc-simple-type
#[allow(clippy::too_many_arguments)]
pub(crate) unsafe fn xml_schema_vcheck_cvc_simple_type(
    actxt: XmlSchemaAbstractCtxtPtr,
    node: Option<XmlGenericNodePtr>,
    typ: XmlSchemaTypePtr,
    mut value: *const XmlChar,
    ret_val: *mut XmlSchemaValPtr,
    fire_errors: i32,
    mut normalize: i32,
    mut is_normalized: i32,
) -> i32 {
    unsafe {
        let mut ret: i32 = 0;
        let mut val_needed: i32 = if !ret_val.is_null() { 1 } else { 0 };
        let mut val: XmlSchemaValPtr = null_mut();
        // let ws: xmlSchemaWhitespaceValueType;
        let mut norm_value: *mut XmlChar = null_mut();

        if !ret_val.is_null() && !(*ret_val).is_null() {
            xml_schema_free_value(*ret_val);
            *ret_val = null_mut();
        }

        // 3.14.4 Simple Type Definition Validation Rules
        // Validation Rule: String Valid
        // 1 It is schema-valid with respect to that definition as defined
        // by Datatype Valid in [XML Schemas: Datatypes].
        // 2.1 If The definition is ENTITY or is validly derived from ENTITY given
        // the empty set, as defined in Type Derivation OK (Simple) ($3.14.6), then
        // the string must be a `declared entity name`.
        // 2.2 If The definition is ENTITIES or is validly derived from ENTITIES
        // given the empty set, as defined in Type Derivation OK (Simple) ($3.14.6),
        // then every whitespace-delimited substring of the string must be a `declared
        // entity name`.
        // 2.3 otherwise no further condition applies.
        if val_needed == 0 && (*typ).flags & XML_SCHEMAS_TYPE_FACETSNEEDVALUE != 0 {
            val_needed = 1;
        }
        if value.is_null() {
            value = c"".as_ptr() as _;
        }
        'internal_error: {
            if wxs_is_any_simple_type(typ) || (*typ).wxs_is_atomic() {
                let mut bi_type: XmlSchemaTypePtr; /* The built-in type. */
                // SPEC (1.2.1) "if {variety} is `atomic` then the string must `match`
                // a literal in the `lexical space` of {base type definition}"

                // Whitespace-normalize.
                NORMALIZE!(typ, value, norm_value, typ, is_normalized, normalize);
                if (*typ).typ != XmlSchemaTypeType::XmlSchemaTypeBasic {
                    // Get the built-in type.
                    bi_type = (*typ).base_type;
                    while !bi_type.is_null()
                        && (*bi_type).typ != XmlSchemaTypeType::XmlSchemaTypeBasic
                    {
                        bi_type = (*bi_type).base_type;
                    }

                    if bi_type.is_null() {
                        AERROR_INT!(
                            actxt,
                            "xmlSchemaVCheckCVCSimpleType",
                            "could not get the built-in type"
                        );
                        break 'internal_error;
                    }
                } else {
                    bi_type = typ;
                }
                // NOTATIONs need to be processed here, since they need
                // to lookup in the hashtable of NOTATION declarations of the schema.
                if (*actxt).typ == XML_SCHEMA_CTXT_VALIDATOR {
                    match XmlSchemaValType::try_from((*bi_type).built_in_type) {
                        Ok(XmlSchemaValType::XmlSchemasNotation) => {
                            ret = xml_schema_validate_notation(
                                actxt as XmlSchemaValidCtxtPtr,
                                (*(actxt as XmlSchemaValidCtxtPtr)).schema,
                                None,
                                value,
                                &raw mut val,
                                val_needed,
                            );
                        }
                        Ok(XmlSchemaValType::XmlSchemasQName) => {
                            ret = xml_schema_validate_qname(
                                actxt as XmlSchemaValidCtxtPtr,
                                value,
                                &raw mut val,
                                val_needed,
                            );
                        }
                        _ => {
                            // ws = xmlSchemaGetWhiteSpaceFacetValue(type);
                            if val_needed != 0 {
                                ret = xml_schema_val_predef_type_node_no_norm(
                                    bi_type,
                                    value,
                                    &raw mut val,
                                    node,
                                );
                            } else {
                                ret = xml_schema_val_predef_type_node_no_norm(
                                    bi_type,
                                    value,
                                    null_mut(),
                                    node,
                                );
                            }
                        }
                    }
                } else if (*actxt).typ == XML_SCHEMA_CTXT_PARSER {
                    match XmlSchemaValType::try_from((*bi_type).built_in_type) {
                        Ok(XmlSchemaValType::XmlSchemasNotation) => {
                            ret = xml_schema_validate_notation(
                                null_mut(),
                                (*(actxt as XmlSchemaParserCtxtPtr)).schema,
                                node,
                                value,
                                &raw mut val,
                                val_needed,
                            );
                        }
                        _ => {
                            // ws = xmlSchemaGetWhiteSpaceFacetValue(type);
                            if val_needed != 0 {
                                ret = xml_schema_val_predef_type_node_no_norm(
                                    bi_type,
                                    value,
                                    &raw mut val,
                                    node,
                                );
                            } else {
                                ret = xml_schema_val_predef_type_node_no_norm(
                                    bi_type,
                                    value,
                                    null_mut(),
                                    node,
                                );
                            }
                        }
                    }
                } else {
                    // Validation via a public API is not implemented yet.
                    // TODO
                    break 'internal_error;
                }
                if ret != 0 {
                    if ret < 0 {
                        AERROR_INT!(
                            actxt,
                            "xmlSchemaVCheckCVCSimpleType",
                            "validating against a built-in type"
                        );
                        break 'internal_error;
                    }
                    if (*typ).wxs_is_list() {
                        ret = XmlParserErrors::XmlSchemavCvcDatatypeValid1_2_2 as i32;
                    } else {
                        ret = XmlParserErrors::XmlSchemavCvcDatatypeValid1_2_1 as i32;
                    }
                }
                if ret == 0 && (*typ).flags & XML_SCHEMAS_TYPE_HAS_FACETS != 0 {
                    // Check facets.
                    ret = xml_schema_validate_facets(
                        actxt,
                        node,
                        typ,
                        XmlSchemaValType::try_from((*bi_type).built_in_type).unwrap(),
                        value,
                        val,
                        0,
                        fire_errors,
                    );
                    if ret != 0 {
                        if ret < 0 {
                            AERROR_INT!(
                                actxt,
                                "xmlSchemaVCheckCVCSimpleType",
                                "validating facets of atomic simple type"
                            );
                            break 'internal_error;
                        }
                        if (*typ).wxs_is_list() {
                            ret = XmlParserErrors::XmlSchemavCvcDatatypeValid1_2_2 as i32;
                        } else {
                            ret = XmlParserErrors::XmlSchemavCvcDatatypeValid1_2_1 as i32;
                        }
                    }
                } else if fire_errors != 0 && ret > 0 {
                    let value = CStr::from_ptr(value as *const i8).to_string_lossy();
                    xml_schema_simple_type_err(
                        actxt,
                        ret.try_into().unwrap(),
                        node,
                        &value,
                        typ,
                        1,
                    );
                }
            } else if (*typ).wxs_is_list() {
                let mut cur: *const XmlChar;
                let mut end: *const XmlChar;
                let mut tmp_value: *mut XmlChar = null_mut();
                let mut len: u64 = 0;
                let mut prev_val: XmlSchemaValPtr = null_mut();
                let mut cur_val: XmlSchemaValPtr = null_mut();
                // 1.2.2 if {variety} is `list` then the string must be a sequence
                // of white space separated tokens, each of which `match`es a literal
                // in the `lexical space` of {item type definition}

                // Note that XML_SCHEMAS_TYPE_NORMVALUENEEDED will be set if
                // the list type has an enum or pattern facet.
                NORMALIZE!(typ, value, norm_value, typ, is_normalized, normalize);
                // VAL TODO: Optimize validation of empty values.
                // VAL TODO: We do not have computed values for lists.
                let item_type: XmlSchemaTypePtr = (*typ).subtypes;
                cur = value;
                loop {
                    while xml_is_blank_char(*cur as u32) {
                        cur = cur.add(1);
                    }
                    end = cur;
                    while *end != 0 && !xml_is_blank_char(*end as u32) {
                        end = end.add(1);
                    }
                    if end == cur {
                        break;
                    }
                    tmp_value = xml_strndup(cur, end.offset_from(cur) as _);
                    len += 1;

                    if val_needed != 0 {
                        ret = xml_schema_vcheck_cvc_simple_type(
                            actxt,
                            node,
                            item_type,
                            tmp_value,
                            &raw mut cur_val,
                            fire_errors,
                            0,
                            1,
                        );
                    } else {
                        ret = xml_schema_vcheck_cvc_simple_type(
                            actxt,
                            node,
                            item_type,
                            tmp_value,
                            null_mut(),
                            fire_errors,
                            0,
                            1,
                        );
                    }
                    FREE_AND_NULL!(tmp_value);
                    if !cur_val.is_null() {
                        // Add to list of computed values.
                        if val.is_null() {
                            val = cur_val;
                        } else {
                            xml_schema_value_append(prev_val, cur_val);
                        }
                        prev_val = cur_val;
                        cur_val = null_mut();
                    }
                    if ret != 0 {
                        if ret < 0 {
                            AERROR_INT!(
                                actxt,
                                "xmlSchemaVCheckCVCSimpleType",
                                "validating an item of list simple type"
                            );
                            break 'internal_error;
                        }
                        ret = XmlParserErrors::XmlSchemavCvcDatatypeValid1_2_2 as i32;
                        break;
                    }
                    cur = end;

                    if *cur == 0 {
                        break;
                    }
                }
                FREE_AND_NULL!(tmp_value);
                if ret == 0 && (*typ).flags & XML_SCHEMAS_TYPE_HAS_FACETS != 0 {
                    // Apply facets (pattern, enumeration).
                    ret = xml_schema_validate_facets(
                        actxt,
                        node,
                        typ,
                        XmlSchemaValType::XmlSchemasUnknown,
                        value,
                        val,
                        len,
                        fire_errors,
                    );
                    if ret != 0 {
                        if ret < 0 {
                            AERROR_INT!(
                                actxt,
                                "xmlSchemaVCheckCVCSimpleType",
                                "validating facets of list simple type"
                            );
                            break 'internal_error;
                        }
                        ret = XmlParserErrors::XmlSchemavCvcDatatypeValid1_2_2 as i32;
                    }
                }
                if fire_errors != 0 && ret > 0 {
                    // Report the normalized value.
                    normalize = 1;
                    NORMALIZE!(typ, value, norm_value, typ, is_normalized, normalize);
                    let value = CStr::from_ptr(value as *const i8).to_string_lossy();
                    xml_schema_simple_type_err(
                        actxt,
                        ret.try_into().unwrap(),
                        node,
                        &value,
                        typ,
                        1,
                    );
                }
            } else if (*typ).wxs_is_union() {
                let mut member_link: XmlSchemaTypeLinkPtr;
                // TODO: For all datatypes `derived` by `union`  whiteSpace does
                // not apply directly; however, the normalization behavior of `union`
                // types is controlled by the value of whiteSpace on that one of the
                // `memberTypes` against which the `union` is successfully validated.
                //
                // This means that the value is normalized by the first validating
                // member type, then the facets of the union type are applied. This
                // needs changing of the value!

                // 1.2.3 if {variety} is `union` then the string must `match` a
                // literal in the `lexical space` of at least one member of
                // {member type definitions}
                member_link = xml_schema_get_union_simple_type_member_types(typ);
                if member_link.is_null() {
                    AERROR_INT!(
                        actxt,
                        "xmlSchemaVCheckCVCSimpleType",
                        "union simple type has no member types"
                    );
                    break 'internal_error;
                }
                // Always normalize union type values, since we currently
                // cannot store the whitespace information with the value
                // itself; otherwise a later value-comparison would be not possible.
                while !member_link.is_null() {
                    if val_needed != 0 {
                        ret = xml_schema_vcheck_cvc_simple_type(
                            actxt,
                            node,
                            (*member_link).typ,
                            value,
                            &raw mut val,
                            0,
                            1,
                            0,
                        );
                    } else {
                        ret = xml_schema_vcheck_cvc_simple_type(
                            actxt,
                            node,
                            (*member_link).typ,
                            value,
                            null_mut(),
                            0,
                            1,
                            0,
                        );
                    }
                    if ret <= 0 {
                        break;
                    }
                    member_link = (*member_link).next;
                }
                if ret != 0 {
                    if ret < 0 {
                        AERROR_INT!(
                            actxt,
                            "xmlSchemaVCheckCVCSimpleType",
                            "validating members of union simple type"
                        );
                        break 'internal_error;
                    }
                    ret = XmlParserErrors::XmlSchemavCvcDatatypeValid1_2_3 as i32;
                }
                // Apply facets (pattern, enumeration).
                if ret == 0 && (*typ).flags & XML_SCHEMAS_TYPE_HAS_FACETS != 0 {
                    // The normalization behavior of `union` types is controlled by
                    // the value of whiteSpace on that one of the `memberTypes`
                    // against which the `union` is successfully validated.
                    NORMALIZE!(
                        (*member_link).typ,
                        value,
                        norm_value,
                        typ,
                        is_normalized,
                        normalize
                    );
                    ret = xml_schema_validate_facets(
                        actxt,
                        node,
                        typ,
                        XmlSchemaValType::XmlSchemasUnknown,
                        value,
                        val,
                        0,
                        fire_errors,
                    );
                    if ret != 0 {
                        if ret < 0 {
                            AERROR_INT!(
                                actxt,
                                "xmlSchemaVCheckCVCSimpleType",
                                "validating facets of union simple type"
                            );
                            break 'internal_error;
                        }
                        ret = XmlParserErrors::XmlSchemavCvcDatatypeValid1_2_3 as i32;
                    }
                }
                if fire_errors != 0 && ret > 0 {
                    let value = CStr::from_ptr(value as *const i8).to_string_lossy();
                    xml_schema_simple_type_err(
                        actxt,
                        ret.try_into().unwrap(),
                        node,
                        &value,
                        typ,
                        1,
                    );
                }
            }

            if !norm_value.is_null() {
                xml_free(norm_value as _);
            }
            if ret == 0 {
                if !ret_val.is_null() {
                    *ret_val = val;
                } else if !val.is_null() {
                    xml_schema_free_value(val);
                }
            } else if !val.is_null() {
                xml_schema_free_value(val);
            }
            return ret;
        }
        // internal_error:
        if !norm_value.is_null() {
            xml_free(norm_value as _);
        }
        if !val.is_null() {
            xml_schema_free_value(val);
        }
        -1
    }
}

/// Returns the given QName in the format "{namespaceName}localName" or
/// just "localName" if @namespaceName is NULL.
///
/// Returns the localName if @namespaceName is NULL, a formatted string otherwise.
#[doc(alias = "xmlSchemaFormatQName")]
pub(crate) fn xml_schema_format_qname(
    namespace_name: Option<&str>,
    local_name: Option<&str>,
) -> String {
    let mut res = String::new();
    if let Some(namespace_name) = namespace_name {
        res.push('{');
        res.push_str(namespace_name);
        res.push('}');
    }
    if let Some(local_name) = local_name {
        res.push_str(local_name);
    } else {
        res.push_str("(NULL)");
    }
    res
}

/// Convert the XmlSchemaTypeType to a c_char string.
///
/// Returns the c_char string representation of the facet type if the
/// type is a facet and an "Internal Error" string otherwise.
pub(crate) fn xml_schema_facet_type_to_string(typ: XmlSchemaTypeType) -> &'static str {
    match typ {
        XmlSchemaTypeType::XmlSchemaFacetPattern => "pattern",
        XmlSchemaTypeType::XmlSchemaFacetMaxExclusive => "maxExclusive",
        XmlSchemaTypeType::XmlSchemaFacetMaxInclusive => "maxInclusive",
        XmlSchemaTypeType::XmlSchemaFacetMinExclusive => "minExclusive",
        XmlSchemaTypeType::XmlSchemaFacetMinInclusive => "minInclusive",
        XmlSchemaTypeType::XmlSchemaFacetWhitespace => "whiteSpace",
        XmlSchemaTypeType::XmlSchemaFacetEnumeration => "enumeration",
        XmlSchemaTypeType::XmlSchemaFacetLength => "length",
        XmlSchemaTypeType::XmlSchemaFacetMaxLength => "maxLength",
        XmlSchemaTypeType::XmlSchemaFacetMinLength => "minLength",
        XmlSchemaTypeType::XmlSchemaFacetTotalDigits => "totalDigits",
        XmlSchemaTypeType::XmlSchemaFacetFractionDigits => "fractionDigits",
        _ => "Internal Error",
    }
}

/// Check if any error was detected during validation.
///
/// Returns 1 if valid so far, 0 if errors were detected, and -1 in case of internal error.
#[doc(alias = "xmlSchemaIsValid")]
pub unsafe fn xml_schema_is_valid(ctxt: XmlSchemaValidCtxtPtr) -> i32 {
    unsafe {
        if ctxt.is_null() {
            return -1;
        }
        ((*ctxt).err == 0) as i32
    }
}

unsafe fn xml_schema_subst_group_free(group: XmlSchemaSubstGroupPtr) {
    unsafe {
        if group.is_null() {
            return;
        }
        if !(*group).members.is_null() {
            xml_schema_item_list_free((*group).members);
        }
        xml_free(group as _);
    }
}

extern "C" fn xml_schema_subst_group_free_entry(group: *mut c_void, _name: *const XmlChar) {
    unsafe {
        xml_schema_subst_group_free(group as XmlSchemaSubstGroupPtr);
    }
}

unsafe fn xml_schema_redef_list_free(mut redef: XmlSchemaRedefPtr) {
    unsafe {
        let mut prev: XmlSchemaRedefPtr;

        while !redef.is_null() {
            prev = redef;
            redef = (*redef).next;
            xml_free(prev as _);
        }
    }
}

pub(crate) unsafe fn xml_schema_construction_ctxt_free(con: XmlSchemaConstructionCtxtPtr) {
    unsafe {
        // After the construction context has been freed, there will be
        // no schema graph available any more. Only the schema buckets
        // will stay alive, which are put into the "schemas_imports" and
        // "includes" slots of the xmlSchema.
        if !(*con).buckets.is_null() {
            xml_schema_item_list_free((*con).buckets);
        }
        if !(*con).pending.is_null() {
            xml_schema_item_list_free((*con).pending);
        }
        if !(*con).subst_groups.is_null() {
            xml_hash_free((*con).subst_groups, Some(xml_schema_subst_group_free_entry));
        }
        if !(*con).redefs.is_null() {
            xml_schema_redef_list_free((*con).redefs);
        }
        if !(*con).dict.is_null() {
            xml_dict_free((*con).dict);
        }
        xml_free(con as _);
    }
}

pub(crate) unsafe fn xml_schema_construction_ctxt_create(
    dict: XmlDictPtr,
) -> XmlSchemaConstructionCtxtPtr {
    unsafe {
        let ret: XmlSchemaConstructionCtxtPtr =
            xml_malloc(size_of::<XmlSchemaConstructionCtxt>()) as XmlSchemaConstructionCtxtPtr;
        if ret.is_null() {
            xml_schema_perr_memory(null_mut(), "allocating schema construction context", None);
            return null_mut();
        }
        memset(ret as _, 0, size_of::<XmlSchemaConstructionCtxt>());

        (*ret).buckets = xml_schema_item_list_create::<*mut c_void>();
        if (*ret).buckets.is_null() {
            xml_schema_perr_memory(null_mut(), "allocating list of schema buckets", None);
            xml_free(ret as _);
            return null_mut();
        }
        (*ret).pending = xml_schema_item_list_create::<*mut c_void>();
        if (*ret).pending.is_null() {
            xml_schema_perr_memory(
                null_mut(),
                "allocating list of pending global components",
                None,
            );
            xml_schema_construction_ctxt_free(ret);
            return null_mut();
        }
        (*ret).dict = dict;
        xml_dict_reference(dict);
        ret
    }
}

/// Returns a schema bucket if it was already parsed.
///
/// Returns a schema bucket if it was already parsed from @schemaLocation, NULL otherwise.
#[doc(alias = "xmlSchemaGetSchemaBucket")]
unsafe fn xml_schema_get_schema_bucket(
    pctxt: XmlSchemaParserCtxtPtr,
    schema_location: *const XmlChar,
) -> XmlSchemaBucketPtr {
    unsafe {
        let list: XmlSchemaItemListPtr<*mut c_void> = (*(*pctxt).constructor).buckets;
        for cur in (*list).items.iter().map(|&cur| cur as XmlSchemaBucketPtr) {
            // Pointer comparison!
            if (*cur).schema_location == schema_location {
                return cur;
            }
        }
        null_mut()
    }
}

unsafe fn xml_schema_schema_relation_create() -> XmlSchemaSchemaRelationPtr {
    unsafe {
        let ret: XmlSchemaSchemaRelationPtr =
            xml_malloc(size_of::<XmlSchemaSchemaRelation>()) as XmlSchemaSchemaRelationPtr;
        if ret.is_null() {
            xml_schema_perr_memory(null_mut(), "allocating schema relation", None);
            return null_mut();
        }
        memset(ret as _, 0, size_of::<XmlSchemaSchemaRelation>());
        ret
    }
}

unsafe fn xml_schema_schema_relation_add_child(
    bucket: XmlSchemaBucketPtr,
    rel: XmlSchemaSchemaRelationPtr,
) {
    unsafe {
        let mut cur: XmlSchemaSchemaRelationPtr = (*bucket).relations;

        if cur.is_null() {
            (*bucket).relations = rel;
            return;
        }
        while !(*cur).next.is_null() {
            cur = (*cur).next;
        }
        (*cur).next = rel;
    }
}

macro_rules! IS_BAD_SCHEMA_DOC {
    ($b:expr) => {
        (*$b).doc.is_none() && !(*$b).schema_location.is_null()
    };
}

unsafe fn xml_schema_get_schema_bucket_by_tns(
    pctxt: XmlSchemaParserCtxtPtr,
    target_namespace: *const XmlChar,
    imported: i32,
) -> XmlSchemaBucketPtr {
    unsafe {
        let list: XmlSchemaItemListPtr<*mut c_void> = (*(*pctxt).constructor).buckets;
        for cur in (*list).items.iter().map(|&cur| cur as XmlSchemaBucketPtr) {
            if !IS_BAD_SCHEMA_DOC!(cur)
                && (*cur).orig_target_namespace == target_namespace
                && ((imported != 0 && (*cur).imported != 0)
                    || (imported == 0 && (*cur).imported == 0))
            {
                return cur;
            }
        }
        null_mut()
    }
}

unsafe fn xml_schema_get_chameleon_schema_bucket(
    pctxt: XmlSchemaParserCtxtPtr,
    schema_location: *const XmlChar,
    target_namespace: *const XmlChar,
) -> XmlSchemaBucketPtr {
    unsafe {
        let list: XmlSchemaItemListPtr<*mut c_void> = (*(*pctxt).constructor).buckets;
        for cur in (*list).items.iter().map(|&cur| cur as XmlSchemaBucketPtr) {
            // Pointer comparison!
            if (*cur).orig_target_namespace.is_null()
                && (*cur).schema_location == schema_location
                && (*cur).target_namespace == target_namespace
            {
                return cur;
            }
        }
        null_mut()
    }
}

unsafe fn is_blank_node(node: XmlNodePtr) -> bool {
    unsafe {
        node.element_type() == XmlElementType::XmlTextNode
            && xml_schema_is_blank(node.content, -1) != 0
    }
}

/// Check if a string is ignorable
///
/// Returns 1 if the string is NULL or made of blanks chars, 0 otherwise
unsafe fn xml_schema_is_blank(mut str: *mut XmlChar, mut len: i32) -> i32 {
    unsafe {
        if str.is_null() {
            return 1;
        }
        if len < 0 {
            while *str != 0 {
                if !xml_is_blank_char(*str as u32) {
                    return 0;
                }
                str = str.add(1);
            }
        } else {
            while *str != 0 && len != 0 {
                if !xml_is_blank_char(*str as u32) {
                    return 0;
                }
                str = str.add(1);
                len -= 1;
            }
        }

        1
    }
}

/// Removes unwanted nodes in a schemas document tree
#[doc(alias = "xmlSchemaCleanupDoc")]
unsafe fn xml_schema_cleanup_doc(ctxt: XmlSchemaParserCtxtPtr, root: XmlNodePtr) {
    unsafe {
        if ctxt.is_null() {
            return;
        }

        // Remove all the blank text nodes
        let mut delete: Option<XmlNodePtr> = None;
        let mut cur = Some(root);
        'main: while let Some(cur_node) = cur {
            if let Some(mut delete) = delete.take() {
                delete.unlink();
                xml_free_node(delete);
            }
            'skip_children: {
                if cur_node.element_type() == XmlElementType::XmlTextNode {
                    if is_blank_node(cur_node) && cur_node.get_space_preserve() != 1 {
                        delete = cur;
                    }
                } else if !matches!(
                    cur_node.element_type(),
                    XmlElementType::XmlElementNode | XmlElementType::XmlCDATASectionNode
                ) {
                    delete = cur;
                    // goto skip_children;
                    break 'skip_children;
                }

                // Skip to next node
                if let Some(children) = cur_node
                    .children()
                    .filter(|children| {
                        !matches!(
                            children.element_type(),
                            XmlElementType::XmlEntityDecl
                                | XmlElementType::XmlEntityRefNode
                                | XmlElementType::XmlEntityNode
                        )
                    })
                    .map(|children| XmlNodePtr::try_from(children).unwrap())
                {
                    cur = Some(children);
                    continue 'main;
                }
            }
            //   skip_children:
            if let Some(next) = cur_node
                .next
                .map(|node| XmlNodePtr::try_from(node).unwrap())
            {
                cur = Some(next);
                continue;
            }

            let mut cur_node = cur_node;
            cur = loop {
                let Some(parent) = cur_node
                    .parent
                    .map(|parent| XmlNodePtr::try_from(parent).unwrap())
                else {
                    break None;
                };
                cur_node = parent;
                if cur_node == root {
                    break None;
                }
                if let Some(next) = cur_node
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap())
                {
                    break Some(next);
                }
            }
        }
        if let Some(mut delete) = delete {
            delete.unlink();
            xml_free_node(delete);
        }
    }
}

/// Deallocates an attribute declaration structure.
#[doc(alias = "xmlSchemaFreeAttribute")]
unsafe fn xml_schema_free_attribute(attr: XmlSchemaAttributePtr) {
    unsafe {
        if attr.is_null() {
            return;
        }
        if !(*attr).annot.is_null() {
            xml_schema_free_annot((*attr).annot);
        }
        if !(*attr).def_val.is_null() {
            xml_schema_free_value((*attr).def_val);
        }
        xml_free(attr as _);
    }
}

/// Deallocates an attribute use structure.
#[doc(alias = "xmlSchemaFreeAttributeUse")]
unsafe fn xml_schema_free_attribute_use(using: XmlSchemaAttributeUsePtr) {
    unsafe {
        if using.is_null() {
            return;
        }
        if !(*using).annot.is_null() {
            xml_schema_free_annot((*using).annot);
        }
        if !(*using).def_val.is_null() {
            xml_schema_free_value((*using).def_val);
        }
        xml_free(using as _);
    }
}

/// Deallocate a Schema Element structure.
#[doc(alias = "xmlSchemaFreeElement")]
unsafe fn xml_schema_free_element(elem: XmlSchemaElementPtr) {
    unsafe {
        if elem.is_null() {
            return;
        }
        if !(*elem).annot.is_null() {
            xml_schema_free_annot((*elem).annot);
        }
        if !(*elem).cont_model.is_null() {
            xml_reg_free_regexp((*elem).cont_model);
        }
        if !(*elem).def_val.is_null() {
            xml_schema_free_value((*elem).def_val);
        }
        xml_free(elem as _);
    }
}

/// Deallocates a schema model group structure.
#[doc(alias = "xmlSchemaFreeModelGroup")]
unsafe fn xml_schema_free_model_group(item: XmlSchemaModelGroupPtr) {
    unsafe {
        if !(*item).annot.is_null() {
            xml_schema_free_annot((*item).annot);
        }
        xml_free(item as _);
    }
}

/// Deallocate a Schema Attribute Group structure.
#[doc(alias = "xmlSchemaFreeAttributeGroup")]
unsafe fn xml_schema_free_attribute_group(attr_gr: XmlSchemaAttributeGroupPtr) {
    unsafe {
        if attr_gr.is_null() {
            return;
        }
        if !(*attr_gr).annot.is_null() {
            xml_schema_free_annot((*attr_gr).annot);
        }
        if !(*attr_gr).attr_uses.is_null() {
            xml_schema_item_list_free((*attr_gr).attr_uses as XmlSchemaItemListPtr<*mut c_void>);
        }
        xml_free(attr_gr as _);
    }
}

/// Deallocates a schema model group definition.
#[doc(alias = "xmlSchemaFreeModelGroupDef")]
unsafe fn xml_schema_free_model_group_def(item: XmlSchemaModelGroupDefPtr) {
    unsafe {
        if !(*item).annot.is_null() {
            xml_schema_free_annot((*item).annot);
        }
        xml_free(item as _);
    }
}

/// Deallocates an identity-constraint definition.
#[doc(alias = "xmlSchemaFreeIDC")]
unsafe fn xml_schema_free_idc(idc_def: XmlSchemaIDCPtr) {
    unsafe {
        let mut cur: XmlSchemaIdcselectPtr;
        let mut prev: XmlSchemaIdcselectPtr;

        if idc_def.is_null() {
            return;
        }
        if !(*idc_def).annot.is_null() {
            xml_schema_free_annot((*idc_def).annot);
        }
        // Selector
        if !(*idc_def).selector.is_null() {
            if !(*(*idc_def).selector).xpath_comp.is_null() {
                xml_free_pattern((*(*idc_def).selector).xpath_comp as XmlPatternPtr);
            }
            xml_free((*idc_def).selector as _);
        }
        // Fields
        if !(*idc_def).fields.is_null() {
            cur = (*idc_def).fields;
            while {
                prev = cur;
                cur = (*cur).next;
                if !(*prev).xpath_comp.is_null() {
                    xml_free_pattern((*prev).xpath_comp as XmlPatternPtr);
                }
                xml_free(prev as _);

                !cur.is_null()
            } {}
        }
        xml_free(idc_def as _);
    }
}

/// Deallocate a Schema Notation structure.
#[doc(alias = "xmlSchemaFreeNotation")]
unsafe fn xml_schema_free_notation(nota: XmlSchemaNotationPtr) {
    unsafe {
        if nota.is_null() {
            return;
        }
        xml_free(nota as _);
    }
}

/// Deallocatea a QName reference structure.
#[doc(alias = "xmlSchemaFreeQNameRef")]
unsafe fn xml_schema_free_qname_ref(item: XmlSchemaQNameRefPtr) {
    unsafe {
        xml_free(item as _);
    }
}

unsafe fn xml_schema_component_list_free(list: XmlSchemaItemListPtr<*mut c_void>) {
    unsafe {
        if list.is_null() || (*list).items.is_empty() {
            return;
        }

        for item in (*list)
            .items
            .drain(..)
            .map(|item| item as XmlSchemaTreeItemPtr)
        {
            if item.is_null() {
                continue;
            }
            match (*item).typ {
                XmlSchemaTypeType::XmlSchemaTypeSimple
                | XmlSchemaTypeType::XmlSchemaTypeComplex => {
                    xml_schema_free_type(item as XmlSchemaTypePtr);
                }
                XmlSchemaTypeType::XmlSchemaTypeAttribute => {
                    xml_schema_free_attribute(item as XmlSchemaAttributePtr);
                }
                XmlSchemaTypeType::XmlSchemaTypeAttributeUse => {
                    xml_schema_free_attribute_use(item as XmlSchemaAttributeUsePtr);
                }
                XmlSchemaTypeType::XmlSchemaExtraAttrUseProhib => {
                    xml_schema_free_attribute_use_prohib(item as XmlSchemaAttributeUseProhibPtr);
                }
                XmlSchemaTypeType::XmlSchemaTypeElement => {
                    xml_schema_free_element(item as XmlSchemaElementPtr);
                }
                XmlSchemaTypeType::XmlSchemaTypeParticle => {
                    if !(*item).annot.is_null() {
                        xml_schema_free_annot((*item).annot);
                    }
                    xml_free(item as _);
                }
                XmlSchemaTypeType::XmlSchemaTypeSequence
                | XmlSchemaTypeType::XmlSchemaTypeChoice
                | XmlSchemaTypeType::XmlSchemaTypeAll => {
                    xml_schema_free_model_group(item as XmlSchemaModelGroupPtr);
                }
                XmlSchemaTypeType::XmlSchemaTypeAttributeGroup => {
                    xml_schema_free_attribute_group(item as XmlSchemaAttributeGroupPtr);
                }
                XmlSchemaTypeType::XmlSchemaTypeGroup => {
                    xml_schema_free_model_group_def(item as XmlSchemaModelGroupDefPtr);
                }
                XmlSchemaTypeType::XmlSchemaTypeAny
                | XmlSchemaTypeType::XmlSchemaTypeAnyAttribute => {
                    xml_schema_free_wildcard(item as XmlSchemaWildcardPtr);
                }
                XmlSchemaTypeType::XmlSchemaTypeIDCKey
                | XmlSchemaTypeType::XmlSchemaTypeIDCUnique
                | XmlSchemaTypeType::XmlSchemaTypeIDCKeyref => {
                    xml_schema_free_idc(item as XmlSchemaIDCPtr);
                }
                XmlSchemaTypeType::XmlSchemaTypeNotation => {
                    xml_schema_free_notation(item as XmlSchemaNotationPtr);
                }
                XmlSchemaTypeType::XmlSchemaExtraQNameRef => {
                    xml_schema_free_qname_ref(item as XmlSchemaQNameRefPtr);
                }
                _ => {
                    let name = xml_schema_get_component_type_str(item as _);
                    // TODO: This should never be hit.
                    xml_schema_psimple_internal_err(
                    None,
                    format!(
                        "Internal error: xmlSchemaComponentListFree, unexpected component type '{}'\n",
                        name
                    ).as_str(),
                );
                }
            }
        }
    }
}

pub(crate) unsafe fn xml_schema_bucket_free(bucket: XmlSchemaBucketPtr) {
    unsafe {
        if bucket.is_null() {
            return;
        }
        if !(*bucket).globals.is_null() {
            xml_schema_component_list_free((*bucket).globals);
            xml_schema_item_list_free((*bucket).globals);
        }
        if !(*bucket).locals.is_null() {
            xml_schema_component_list_free((*bucket).locals);
            xml_schema_item_list_free((*bucket).locals);
        }
        if !(*bucket).relations.is_null() {
            let mut prev: XmlSchemaSchemaRelationPtr;
            let mut cur: XmlSchemaSchemaRelationPtr = (*bucket).relations;
            while {
                prev = cur;
                cur = (*cur).next;
                xml_free(prev as _);
                !cur.is_null()
            } {}
        }
        if let Some(doc) = (*bucket).doc.filter(|_| (*bucket).preserve_doc == 0) {
            xml_free_doc(doc);
        }
        if (*bucket).typ == XML_SCHEMA_SCHEMA_IMPORT && !(*WXS_IMPBUCKET!(bucket)).schema.is_null()
        {
            xml_schema_free((*WXS_IMPBUCKET!(bucket)).schema);
        }
        xml_free(bucket as _);
    }
}

unsafe fn xml_schema_bucket_create(
    pctxt: XmlSchemaParserCtxtPtr,
    typ: i32,
    target_namespace: *const XmlChar,
) -> XmlSchemaBucketPtr {
    unsafe {
        if (*WXS_CONSTRUCTOR!(pctxt)).main_schema.is_null() {
            PERROR_INT!(
                pctxt,
                "xmlSchemaBucketCreate",
                "no main schema on constructor"
            );
            return null_mut();
        }
        let main_schema: XmlSchemaPtr = (*WXS_CONSTRUCTOR!(pctxt)).main_schema;
        // Create the schema bucket.
        let size = if WXS_IS_BUCKET_INCREDEF!(typ) {
            size_of::<XmlSchemaInclude>()
        } else {
            size_of::<XmlSchemaImport>()
        };
        let ret: XmlSchemaBucketPtr = xml_malloc(size) as _;
        if ret.is_null() {
            xml_schema_perr_memory(null_mut(), "allocating schema bucket", None);
            return null_mut();
        }
        memset(ret as _, 0, size);
        (*ret).target_namespace = target_namespace;
        (*ret).typ = typ;
        (*ret).globals = xml_schema_item_list_create::<*mut c_void>();
        if (*ret).globals.is_null() {
            xml_schema_bucket_free(ret);
            return null_mut();
        }
        (*ret).locals = xml_schema_item_list_create::<*mut c_void>();
        if (*ret).locals.is_null() {
            xml_schema_bucket_free(ret);
            return null_mut();
        }
        // The following will assure that only the first bucket is marked as
        // XML_SCHEMA_SCHEMA_MAIN and it points to the *main* schema.
        // For each following import buckets an xmlSchema will be created.
        // An xmlSchema will be created for every distinct target_namespace.
        // We assign the target_namespace to the schemata here.
        if !WXS_HAS_BUCKETS!(pctxt) {
            if WXS_IS_BUCKET_INCREDEF!(typ) {
                PERROR_INT!(
                    pctxt,
                    "xmlSchemaBucketCreate",
                    "first bucket but it's an include or redefine"
                );
                xml_schema_bucket_free(ret);
                return null_mut();
            }
            // Force the type to be XML_SCHEMA_SCHEMA_MAIN.
            (*ret).typ = XML_SCHEMA_SCHEMA_MAIN;
            // Point to the *main* schema.
            (*WXS_CONSTRUCTOR!(pctxt)).main_bucket = ret;
            (*WXS_IMPBUCKET!(ret)).schema = main_schema;
            // Ensure that the main schema gets a target_namespace.
            (*main_schema).target_namespace = (!target_namespace.is_null()).then(|| {
                CStr::from_ptr(target_namespace as *const i8)
                    .to_string_lossy()
                    .into_owned()
                    .into()
            });
        } else if typ == XML_SCHEMA_SCHEMA_MAIN {
            PERROR_INT!(
                pctxt,
                "xmlSchemaBucketCreate",
                "main bucket but it's not the first one"
            );
            xml_schema_bucket_free(ret);
            return null_mut();
        } else if typ == XML_SCHEMA_SCHEMA_IMPORT {
            // Create a schema for imports and assign the target_namespace.
            (*WXS_IMPBUCKET!(ret)).schema = (*pctxt).new_schema();
            if (*WXS_IMPBUCKET!(ret)).schema.is_null() {
                xml_schema_bucket_free(ret);
                return null_mut();
            }
            (*(*WXS_IMPBUCKET!(ret)).schema).target_namespace =
                (!target_namespace.is_null()).then(|| {
                    CStr::from_ptr(target_namespace as *const i8)
                        .to_string_lossy()
                        .into_owned()
                        .into()
                });
        }
        if WXS_IS_BUCKET_IMPMAIN!(typ) {
            let duplicate = if target_namespace.is_null() {
                (*main_schema)
                    .schemas_imports
                    .insert(XML_SCHEMAS_NO_NAMESPACE.to_owned(), ret as _)
                    .is_some()
            } else {
                (*main_schema)
                    .schemas_imports
                    .insert(
                        CStr::from_ptr(target_namespace as *const i8)
                            .to_string_lossy()
                            .into_owned(),
                        ret as _,
                    )
                    .is_some()
            };
            if duplicate {
                PERROR_INT!(
                    pctxt,
                    "xmlSchemaBucketCreate",
                    "failed to add the schema bucket to the hash"
                );
                xml_schema_bucket_free(ret);
                return null_mut();
            }
        } else {
            // Set the @ownerImport of an include bucket.
            if WXS_IS_BUCKET_IMPMAIN!((*(*WXS_CONSTRUCTOR!(pctxt)).bucket).typ) {
                (*WXS_INCBUCKET!(ret)).owner_import =
                    WXS_IMPBUCKET!((*WXS_CONSTRUCTOR!(pctxt)).bucket);
            } else {
                (*WXS_INCBUCKET!(ret)).owner_import =
                    (*WXS_INCBUCKET!((*WXS_CONSTRUCTOR!(pctxt)).bucket)).owner_import;
            }

            // Includes got into the "includes" slot of the *main* schema.
            if (*main_schema).includes.is_null() {
                (*main_schema).includes = xml_schema_item_list_create::<*mut c_void>() as _;
                if (*main_schema).includes.is_null() {
                    xml_schema_bucket_free(ret);
                    return null_mut();
                }
            }
            if (*((*main_schema).includes as XmlSchemaItemListPtr<*mut c_void>)).push(ret as _) < 0
            {
                xml_schema_bucket_free(ret);
                return null_mut();
            }
        }
        // Add to list of all buckets; this is used for lookup
        // during schema construction time only.
        if (*(*WXS_CONSTRUCTOR!(pctxt)).buckets).push(ret as _) == -1 {
            return null_mut();
        }
        ret
    }
}

/// Parse an included (and to-be-redefined) XML schema document.
///
/// Returns 0 on success, a positive error code on errors and
/// -1 in case of an internal or API error.
#[doc(alias = "xmlSchemaAddSchemaDoc")]
#[allow(clippy::too_many_arguments)]
pub(crate) unsafe fn xml_schema_add_schema_doc(
    pctxt: XmlSchemaParserCtxtPtr,
    typ: i32, /* import or include or redefine */
    mut schema_location: *const XmlChar,
    schema_doc: Option<XmlDocPtr>,
    schema_buffer: *const c_char,
    schema_buffer_len: i32,
    invoking_node: Option<XmlGenericNodePtr>,
    source_target_namespace: *const XmlChar,
    import_namespace: *const XmlChar,
    bucket: *mut XmlSchemaBucketPtr,
) -> i32 {
    unsafe {
        let mut target_namespace: *const XmlChar = null();
        let mut relation: XmlSchemaSchemaRelationPtr = null_mut();
        let mut doc = None;
        let res: i32;
        let mut err = 0;
        let mut located: i32 = 0;
        let mut preserve_doc: i32 = 0;
        let mut bkt: XmlSchemaBucketPtr = null_mut();

        if !bucket.is_null() {
            *bucket = null_mut();
        }

        match typ {
            _ if typ == XML_SCHEMA_SCHEMA_IMPORT || typ == XML_SCHEMA_SCHEMA_MAIN => {
                err = XmlParserErrors::XmlSchemapSrcImport as i32;
            }
            _ if typ == XML_SCHEMA_SCHEMA_INCLUDE => {
                err = XmlParserErrors::XmlSchemapSrcInclude as i32;
            }
            _ if typ == XML_SCHEMA_SCHEMA_REDEFINE => {
                err = XmlParserErrors::XmlSchemapSrcRedefine as i32;
            }
            _ => {}
        }

        'exit_failure: {
            'exit_error: {
                'exit: {
                    // Special handling for the main schema:
                    // skip the location and relation logic and just parse the doc.
                    // We need just a bucket to be returned in this case.
                    if typ == XML_SCHEMA_SCHEMA_MAIN || !WXS_HAS_BUCKETS!(pctxt) {
                        // goto doc_load;
                    } else {
                        // Note that we expect the location to be an absolute URI.
                        if !schema_location.is_null() {
                            bkt = xml_schema_get_schema_bucket(pctxt, schema_location);
                            if !bkt.is_null() && (*(*pctxt).constructor).bucket == bkt {
                                // Report self-imports/inclusions/redefinitions.

                                xml_schema_custom_err(
                                    pctxt as XmlSchemaAbstractCtxtPtr,
                                    err.try_into().unwrap(),
                                    invoking_node,
                                    null_mut(),
                                    "The schema must not import/include/redefine itself",
                                    None,
                                    None,
                                );
                                break 'exit;
                            }
                        }
                        // Create a relation for the graph of schemas.
                        relation = xml_schema_schema_relation_create();
                        if relation.is_null() {
                            return -1;
                        }
                        xml_schema_schema_relation_add_child(
                            (*(*pctxt).constructor).bucket,
                            relation,
                        );
                        (*relation).typ = typ;

                        // Save the namespace import information.
                        if WXS_IS_BUCKET_IMPMAIN!(typ) {
                            (*relation).import_namespace = import_namespace;
                            if schema_location.is_null() {
                                // No location; this is just an import of the namespace.
                                // Note that we don't assign a bucket to the relation
                                // in this case.
                                break 'exit;
                            }
                            target_namespace = import_namespace;
                        }

                        // Did we already fetch the doc?
                        if !bkt.is_null() {
                            if WXS_IS_BUCKET_IMPMAIN!(typ) && (*bkt).imported == 0 {
                                // We included/redefined and then try to import a schema,
                                // but the new location provided for import was different.
                                if schema_location.is_null() {
                                    schema_location = c"in_memory_buffer".as_ptr() as _;
                                }
                                if !xml_str_equal(schema_location, (*bkt).schema_location) {
                                    let schema_location =
                                        CStr::from_ptr(schema_location as *const i8)
                                            .to_string_lossy();
                                    xml_schema_custom_err(
                                    pctxt as XmlSchemaAbstractCtxtPtr,
                                    err.try_into().unwrap(),
                                    invoking_node,
                                    null_mut(),
                                    format!("The schema document '{schema_location}' cannot be imported, since it was already included or redefined").as_str(),
                                    Some(&schema_location),
                                    None
                                );
                                    break 'exit;
                                }
                            } else if !WXS_IS_BUCKET_IMPMAIN!(typ) && (*bkt).imported != 0 {
                                // We imported and then try to include/redefine a schema,
                                // but the new location provided for the include/redefine was different.
                                if schema_location.is_null() {
                                    schema_location = c"in_memory_buffer".as_ptr() as _;
                                }
                                if !xml_str_equal(schema_location, (*bkt).schema_location) {
                                    let schema_location =
                                        CStr::from_ptr(schema_location as *const i8)
                                            .to_string_lossy();
                                    xml_schema_custom_err(
                                    pctxt as XmlSchemaAbstractCtxtPtr,
                                    err.try_into().unwrap(),
                                    invoking_node,
                                    null_mut(),
                                    format!("The schema document '{schema_location}' cannot be included or redefined, since it was already imported").as_str(),
                                    Some(&schema_location),
                                    None
                                );
                                    break 'exit;
                                }
                            }
                        }

                        if WXS_IS_BUCKET_IMPMAIN!(typ) {
                            // Given that the schemaLocation [attribute] is only a hint, it is open
                            // to applications to ignore all but the first <import> for a given
                            // namespace, regardless of the `actual value` of schemaLocation, but
                            // such a strategy risks missing useful information when new
                            // schemaLocations are offered.
                            //
                            // We will use the first <import> that comes with a location.
                            // Further <import>s *with* a location, will result in an error.
                            // TODO: Better would be to just report a warning here, but
                            // we'll try it this way until someone complains.
                            //
                            // Schema Document Location Strategy:
                            // 3 Based on the namespace name, identify an existing schema document,
                            // either as a resource which is an XML document or a <schema> element
                            // information item, in some local schema repository;
                            // 5 Attempt to resolve the namespace name to locate such a resource.
                            //
                            // NOTE: (3) and (5) are not supported.
                            if !bkt.is_null() {
                                (*relation).bucket = bkt;
                                break 'exit;
                            }
                            bkt = xml_schema_get_schema_bucket_by_tns(pctxt, import_namespace, 1);

                            if !bkt.is_null() {
                                (*relation).bucket = bkt;
                                if (*bkt).schema_location.is_null() {
                                    // First given location of the schema; load the doc.
                                    (*bkt).schema_location = schema_location;
                                } else {
                                    if !xml_str_equal(schema_location, (*bkt).schema_location) {
                                        // Additional location given; just skip it.
                                        // URGENT TODO: We should report a warning here.
                                        // res = XML_SCHEMAP_SRC_IMPORT;
                                        if schema_location.is_null() {
                                            schema_location = c"in_memory_buffer".as_ptr() as _;
                                        }

                                        let schema_location =
                                            CStr::from_ptr(schema_location as *const i8)
                                                .to_string_lossy();
                                        let import_namespace =
                                            CStr::from_ptr(import_namespace as *const i8)
                                                .to_string_lossy();
                                        let bkt_schema_location =
                                            CStr::from_ptr((*bkt).schema_location as *const i8)
                                                .to_string_lossy();

                                        xml_schema_custom_warning(
                                        pctxt as XmlSchemaAbstractCtxtPtr,
                                        XmlParserErrors::XmlSchemapWarnSkipSchema,
                                        invoking_node,
                                        null_mut(),
                                        format!("Skipping import of schema located at '{schema_location}' for the namespace '{import_namespace}', since this namespace was already imported with the schema located at '{bkt_schema_location}'").as_str(),
                                        Some(&schema_location),
                                        Some(&import_namespace),
                                        Some(&bkt_schema_location)
                                    );
                                    }
                                    break 'exit;
                                }
                            }
                            // No bucket + first location: load the doc and create a bucket.
                        } else {
                            // <include> and <redefine>
                            if !bkt.is_null() {
                                if (*bkt).orig_target_namespace.is_null()
                                    && (*bkt).target_namespace != source_target_namespace
                                {
                                    // Chameleon include/redefine: skip loading only if it was
                                    // already build for the target_namespace of the including schema.

                                    // URGENT TODO: If the schema is a chameleon-include then copy
                                    // the components into the including schema and modify the
                                    // target_namespace of those components, do nothing otherwise.
                                    // NOTE: This is currently worked-around by compiling the
                                    // chameleon for every distinct including target_namespace; thus
                                    // not performant at the moment.
                                    // TODO: Check when the namespace in wildcards for chameleons
                                    // needs to be converted: before we built wildcard intersections
                                    // or after.
                                    //   Answer: after!
                                    let chamel: XmlSchemaBucketPtr =
                                        xml_schema_get_chameleon_schema_bucket(
                                            pctxt,
                                            schema_location,
                                            source_target_namespace,
                                        );
                                    if !chamel.is_null() {
                                        // A fitting chameleon was already parsed; NOP.
                                        (*relation).bucket = chamel;
                                        break 'exit;
                                    }
                                    // We need to parse the chameleon again for a different
                                    // target_namespace.
                                    // CHAMELEON TODO: Optimize this by only parsing the
                                    // chameleon once, and then copying the components to
                                    // the new target_namespace.
                                    bkt = null_mut();
                                } else {
                                    (*relation).bucket = bkt;
                                    break 'exit;
                                }
                            }
                        }
                        if !bkt.is_null() && (*bkt).doc.is_some() {
                            PERROR_INT!(
                                pctxt,
                                "xmlSchemaAddSchemaDoc",
                                "trying to load a schema doc, but a doc is already assigned to the schema bucket"
                            );
                            break 'exit_failure;
                        }
                    }

                    //  doc_load:
                    // Load the document.
                    if let Some(schema_doc) = schema_doc {
                        doc = Some(schema_doc);
                        // Don' free this one, since it was provided by the caller.
                        preserve_doc = 1;
                        // TODO: Does the context or the doc hold the location?
                        if let Some(url) = schema_doc.url.as_deref() {
                            let url = CString::new(url).unwrap();
                            schema_location =
                                xml_dict_lookup((*pctxt).dict, url.as_ptr() as *const u8, -1);
                        } else {
                            schema_location = c"in_memory_buffer".as_ptr() as _;
                        }
                    } else if !schema_location.is_null() || !schema_buffer.is_null() {
                        let parser_ctxt: XmlParserCtxtPtr = xml_new_parser_ctxt();
                        if parser_ctxt.is_null() {
                            xml_schema_perr_memory(
                                null_mut(),
                                "xmlSchemaGetDoc, allocating a parser context",
                                None,
                            );
                            break 'exit_failure;
                        }
                        if !(*pctxt).dict.is_null() && !(*parser_ctxt).dict.is_null() {
                            // TODO: Do we have to burden the schema parser dict with all
                            // the content of the schema doc?
                            xml_dict_free((*parser_ctxt).dict);
                            (*parser_ctxt).dict = (*pctxt).dict;
                            xml_dict_reference((*parser_ctxt).dict);
                        }
                        if !schema_location.is_null() {
                            // Parse from file.
                            doc = xml_ctxt_read_file(
                                parser_ctxt,
                                CStr::from_ptr(schema_location as *const i8)
                                    .to_string_lossy()
                                    .as_ref(),
                                None,
                                SCHEMAS_PARSE_OPTIONS,
                            );
                        } else if !schema_buffer.is_null() {
                            let mem = from_raw_parts(
                                schema_buffer as *const u8,
                                schema_buffer_len as usize,
                            )
                            .to_vec();
                            // Parse from memory buffer.
                            doc = xml_ctxt_read_memory(
                                parser_ctxt,
                                mem,
                                None,
                                None,
                                SCHEMAS_PARSE_OPTIONS,
                            );
                            schema_location = c"in_memory_buffer".as_ptr() as _;
                            if let Some(mut doc) = doc {
                                doc.url = Some(
                                    CStr::from_ptr(schema_location as *const i8)
                                        .to_string_lossy()
                                        .into_owned(),
                                );
                            }
                        }
                        // For <import>:
                        // 2.1 The referent is (a fragment of) a resource which is an
                        // XML document (see clause 1.1), which in turn corresponds to
                        // a <schema> element information item in a well-formed information
                        // set, which in turn corresponds to a valid schema.
                        // TODO: (2.1) fragments of XML documents are not supported.
                        //
                        // 2.2 The referent is a <schema> element information item in
                        // a well-formed information set, which in turn corresponds
                        // to a valid schema.
                        // TODO: (2.2) is not supported.
                        if doc.is_none() {
                            let lerr = GLOBAL_STATE.with_borrow(|state| state.last_error.clone());
                            // Check if this a parser error, or if the document could
                            // just not be located.
                            // TODO: Try to find specific error codes to react only on
                            // localisation failures.
                            if lerr.is_ok() || lerr.domain() != XmlErrorDomain::XmlFromIO {
                                // We assume a parser error here.
                                located = 1;
                                // TODO: Error code ??
                                res = XmlParserErrors::XmlSchemapSrcImport2_1 as i32;

                                let schema_location =
                                    CStr::from_ptr(schema_location as *const i8).to_string_lossy();

                                xml_schema_custom_err(
                                    pctxt as XmlSchemaAbstractCtxtPtr,
                                    res.try_into().unwrap(),
                                    invoking_node,
                                    null_mut(),
                                    format!("Failed to parse the XML resource '{schema_location}'")
                                        .as_str(),
                                    Some(&schema_location),
                                    None,
                                );
                            }
                        }
                        xml_free_parser_ctxt(parser_ctxt);
                        if doc.is_none() && located != 0 {
                            break 'exit_error;
                        }
                    } else {
                        xml_schema_perr(
                            pctxt,
                            None,
                            XmlParserErrors::XmlSchemapNothingToParse,
                            "No information for parsing was provided with the given schema parser context.\n",
                            None,
                            None,
                        );
                        break 'exit_failure;
                    }
                    // Preprocess the document.
                    if let Some(doc) = doc {
                        located = 1;
                        let Some(doc_elem) = doc.get_root_element() else {
                            let schema_location =
                                CStr::from_ptr(schema_location as *const i8).to_string_lossy();
                            xml_schema_custom_err(
                                pctxt as XmlSchemaAbstractCtxtPtr,
                                XmlParserErrors::XmlSchemapNoroot,
                                invoking_node,
                                null_mut(),
                                format!("The document '{schema_location}' has no document element")
                                    .as_str(),
                                Some(&schema_location),
                                None,
                            );
                            break 'exit_error;
                        };
                        // Remove all the blank text nodes.
                        xml_schema_cleanup_doc(pctxt, doc_elem);
                        // Check the schema's top level element.
                        if !is_schema(Some(doc_elem), "schema") {
                            let schema_location =
                                CStr::from_ptr(schema_location as *const i8).to_string_lossy();
                            xml_schema_custom_err(
                                pctxt as XmlSchemaAbstractCtxtPtr,
                                XmlParserErrors::XmlSchemapNotSchema,
                                invoking_node,
                                null_mut(),
                                format!(
                                    "The XML document '{schema_location}' is not a schema document"
                                )
                                .as_str(),
                                Some(&schema_location),
                                None,
                            );
                            break 'exit_error;
                        }
                        // Note that we don't apply a type check for the
                        // target_namespace value here.
                        target_namespace = (*pctxt)
                            .get_prop(doc_elem, "targetNamespace")
                            .map_or(null_mut(), |prop| {
                                xml_dict_lookup((*pctxt).dict, prop.as_ptr(), prop.len() as i32)
                            });
                    }

                    // after_doc_loading:
                    if bkt.is_null() && located != 0 {
                        // Only create a bucket if the schema was located.
                        bkt = xml_schema_bucket_create(pctxt, typ, target_namespace);
                        if bkt.is_null() {
                            break 'exit_failure;
                        }
                    }
                    if !bkt.is_null() {
                        (*bkt).schema_location = schema_location;
                        (*bkt).located = located;
                        if doc.is_some() {
                            (*bkt).doc = doc;
                            (*bkt).target_namespace = target_namespace;
                            (*bkt).orig_target_namespace = target_namespace;
                            if preserve_doc != 0 {
                                (*bkt).preserve_doc = 1;
                            }
                        }
                        if WXS_IS_BUCKET_IMPMAIN!(typ) {
                            (*bkt).imported += 1;
                        }
                        // Add it to the graph of schemas.
                        if !relation.is_null() {
                            (*relation).bucket = bkt;
                        }
                    }
                }

                //  exit:
                // Return the bucket explicitly; this is needed for the main schema.
                if !bucket.is_null() {
                    *bucket = bkt;
                }
                return 0;
            }

            //  exit_error:
            if let Some(doc) = doc.filter(|_| preserve_doc == 0) {
                xml_free_doc(doc);
                if !bkt.is_null() {
                    (*bkt).doc = None;
                }
            }
            return (*pctxt).err;
        }
        //  exit_failure:
        if let Some(doc) = doc.filter(|_| preserve_doc == 0) {
            xml_free_doc(doc);
            if !bkt.is_null() {
                (*bkt).doc = None;
            }
        }
        -1
    }
}

/// Seeks an attribute with a name of @name in no namespace.
///
/// Returns the attribute or NULL if not present.
#[doc(alias = "xmlSchemaGetPropNode")]
pub(crate) unsafe fn xml_schema_get_prop_node(node: XmlNodePtr, name: &str) -> Option<XmlAttrPtr> {
    let mut prop = node.properties;
    while let Some(now) = prop {
        if now.ns.is_none() && now.name().as_deref() == Some(name) {
            return Some(now);
        }
        prop = now.next;
    }
    None
}

unsafe fn xml_schema_get_node_content_no_dict(node: XmlGenericNodePtr) -> Option<String> {
    unsafe { node.get_content() }
}

/// Extracts and validates the ID of an attribute value.
///
/// Returns 0, in case the ID is valid, a positive error code
/// if not valid and -1 if an internal error occurs.
#[doc(alias = "xmlSchemaPValAttrID")]
unsafe fn xml_schema_pval_attr_node_id(
    ctxt: XmlSchemaParserCtxtPtr,
    attr: Option<XmlAttrPtr>,
) -> i32 {
    let Some(mut attr) = attr else {
        return 0;
    };
    let Some(value) = (unsafe { xml_schema_get_node_content_no_dict(attr.into()).map(Cow::Owned) })
    else {
        return -1;
    };
    let ret = validate_ncname::<true>(&value);
    match ret {
        Ok(_) => {
            // NOTE: the IDness might have already be declared in the DTD
            if !matches!(attr.atype, Some(XmlAttributeType::XmlAttributeID)) {
                // TODO: Use xmlSchemaStrip here; it's not exported at this moment.
                let res = if let Some(strip) = xml_schema_collapse_string(&value) {
                    unsafe { xml_add_id(null_mut(), attr.doc.unwrap(), &strip, attr) }
                } else {
                    unsafe { xml_add_id(null_mut(), attr.doc.unwrap(), &value, attr) }
                };
                if res.is_none() {
                    unsafe {
                        xml_schema_psimple_type_err(
                            ctxt,
                            XmlParserErrors::XmlSchemapS4sAttrInvalidValue,
                            null_mut(),
                            attr.into(),
                            xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasID),
                            None,
                            None,
                            Some(
                                format!("Duplicate value '{value}' of simple type 'xs:ID'")
                                    .as_str(),
                            ),
                            Some(&value),
                            None,
                        );
                    }
                    return XmlParserErrors::XmlSchemapS4sAttrInvalidValue as i32;
                } else {
                    attr.atype = Some(XmlAttributeType::XmlAttributeID);
                }
            }
            0
        }
        Err(_) => unsafe {
            xml_schema_psimple_type_err(
                ctxt,
                XmlParserErrors::XmlSchemapS4sAttrInvalidValue,
                null_mut(),
                attr.into(),
                xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasID),
                None,
                None,
                Some(
                    format!(
                        "The value '{value}' of simple type 'xs:ID' is not a valid 'xs:NCName'"
                    )
                    .as_str(),
                ),
                Some(&value),
                None,
            );
            XmlParserErrors::XmlSchemapS4sAttrInvalidValue as i32
        },
    }
}

pub(crate) unsafe fn xml_schema_pval_attr_id(
    ctxt: XmlSchemaParserCtxtPtr,
    owner_elem: XmlNodePtr,
    name: &str,
) -> i32 {
    unsafe {
        let Some(attr) = xml_schema_get_prop_node(owner_elem, name) else {
            return 0;
        };
        xml_schema_pval_attr_node_id(ctxt, Some(attr))
    }
}

/// Validates a value against the given built-in type.
/// This one is intended to be used internally for validation
/// of schema attribute values during parsing of the schema.
///
/// Returns 0 if the value is valid, a positive error code
/// number otherwise and -1 in case of an internal or API error.
#[doc(alias = "xmlSchemaPValAttrNodeValue")]
unsafe fn xml_schema_pval_attr_node_value(
    pctxt: XmlSchemaParserCtxtPtr,
    owner_item: XmlSchemaBasicItemPtr,
    attr: XmlAttrPtr,
    value: *const XmlChar,
    typ: XmlSchemaTypePtr,
) -> i32 {
    unsafe {
        let mut ret: i32;

        // NOTE: Should we move this to xmlschematypes.c? Hmm, but this
        // one is really meant to be used internally, so better not.
        if pctxt.is_null() || typ.is_null() {
            return -1;
        }
        if (*typ).typ != XmlSchemaTypeType::XmlSchemaTypeBasic {
            PERROR_INT!(
                pctxt,
                "xmlSchemaPValAttrNodeValue",
                "the given type is not a built-in type"
            );
            return -1;
        }
        match XmlSchemaValType::try_from((*typ).built_in_type) {
            Ok(XmlSchemaValType::XmlSchemasNCName)
            | Ok(XmlSchemaValType::XmlSchemasQName)
            | Ok(XmlSchemaValType::XmlSchemasAnyURI)
            | Ok(XmlSchemaValType::XmlSchemasToken)
            | Ok(XmlSchemaValType::XmlSchemasLanguage) => {
                ret = xml_schema_val_predef_type_node(typ, value, null_mut(), Some(attr.into()));
            }
            _ => {
                PERROR_INT!(
                    pctxt,
                    "xmlSchemaPValAttrNodeValue",
                    "validation using the given type is not supported while parsing a schema"
                );
                return -1;
            }
        }
        // TODO: Should we use the S4S error codes instead?
        match ret.cmp(&0) {
            std::cmp::Ordering::Less => {
                PERROR_INT!(
                    pctxt,
                    "xmlSchemaPValAttrNodeValue",
                    "failed to validate a schema attribute value"
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
                    pctxt,
                    ret.try_into().unwrap(),
                    owner_item,
                    attr.into(),
                    typ,
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

/// Extracts and validates a value against the given built-in type.
/// This one is intended to be used internally for validation
/// of schema attribute values during parsing of the schema.
///
/// Returns 0 if the value is valid, a positive error code
/// number otherwise and -1 in case of an internal or API error.
#[doc(alias = "xmlSchemaPValAttrNode")]
pub(crate) unsafe fn xml_schema_pval_attr_node(
    ctxt: XmlSchemaParserCtxtPtr,
    owner_item: XmlSchemaBasicItemPtr,
    attr: XmlAttrPtr,
    typ: XmlSchemaTypePtr,
    value: *mut *const XmlChar,
) -> i32 {
    unsafe {
        if ctxt.is_null() || typ.is_null() {
            return -1;
        }

        let val = (*ctxt).get_node_content(Some(attr.into()));
        if !value.is_null() {
            *value = xml_dict_lookup((*ctxt).dict, val.as_ptr(), val.len() as i32);
        }

        let val = CString::new(val).unwrap();
        xml_schema_pval_attr_node_value(ctxt, owner_item, attr, val.as_ptr() as *const u8, typ)
    }
}

/// Returns 0 if the value is valid, 1 otherwise.
#[doc(alias = "xmlSchemaPValAttrFormDefault")]
pub(crate) unsafe fn xml_schema_pval_attr_form_default(
    value: *const XmlChar,
    flags: *mut i32,
    flag_qualified: i32,
) -> i32 {
    unsafe {
        if xml_str_equal(value, c"qualified".as_ptr() as _) {
            if *flags & flag_qualified == 0 {
                *flags |= flag_qualified;
            }
        } else if !xml_str_equal(value, c"unqualified".as_ptr() as _) {
            return 1;
        }

        0
    }
}

/// Validates the value of the attribute "final" and "block". The value
/// is converted into the specified flag values and returned in @flags.
///
/// Returns 0 if the value is valid, 1 otherwise.
#[doc(alias = "xmlSchemaPValAttrBlockFinal")]
#[allow(clippy::too_many_arguments)]
pub(crate) unsafe fn xml_schema_pval_attr_block_final(
    value: *const XmlChar,
    flags: *mut i32,
    flag_all: i32,
    flag_extension: i32,
    flag_restriction: i32,
    flag_substitution: i32,
    flag_list: i32,
    flag_union: i32,
) -> i32 {
    unsafe {
        let mut ret: i32 = 0;

        // TODO: This does not check for duplicate entries.
        if flags.is_null() || value.is_null() {
            return -1;
        }
        if *value.add(0) == 0 {
            return 0;
        }
        if xml_str_equal(value, c"#all".as_ptr() as _) {
            if flag_all != -1 {
                *flags |= flag_all;
            } else {
                if flag_extension != -1 {
                    *flags |= flag_extension;
                }
                if flag_restriction != -1 {
                    *flags |= flag_restriction;
                }
                if flag_substitution != -1 {
                    *flags |= flag_substitution;
                }
                if flag_list != -1 {
                    *flags |= flag_list;
                }
                if flag_union != -1 {
                    *flags |= flag_union;
                }
            }
        } else {
            let mut end: *const XmlChar;
            let mut cur: *const XmlChar = value;
            let mut item: *mut XmlChar;

            loop {
                while xml_is_blank_char(*cur as u32) {
                    cur = cur.add(1);
                }
                end = cur;
                while *end != 0 && !xml_is_blank_char(*end as u32) {
                    end = end.add(1);
                }
                if end == cur {
                    break;
                }
                item = xml_strndup(cur, end.offset_from(cur) as _);
                if xml_str_equal(item, c"extension".as_ptr() as _) {
                    if flag_extension != -1 {
                        if *flags & flag_extension == 0 {
                            *flags |= flag_extension;
                        }
                    } else {
                        ret = 1;
                    }
                } else if xml_str_equal(item, c"restriction".as_ptr() as _) {
                    if flag_restriction != -1 {
                        if *flags & flag_restriction == 0 {
                            *flags |= flag_restriction;
                        }
                    } else {
                        ret = 1;
                    }
                } else if xml_str_equal(item, c"substitution".as_ptr() as _) {
                    if flag_substitution != -1 {
                        if *flags & flag_substitution == 0 {
                            *flags |= flag_substitution;
                        }
                    } else {
                        ret = 1;
                    }
                } else if xml_str_equal(item, c"list".as_ptr() as _) {
                    if flag_list != -1 {
                        if *flags & flag_list == 0 {
                            *flags |= flag_list;
                        }
                    } else {
                        ret = 1;
                    }
                } else if xml_str_equal(item, c"union".as_ptr() as _) {
                    if flag_union != -1 {
                        if *flags & flag_union == 0 {
                            *flags |= flag_union;
                        }
                    } else {
                        ret = 1;
                    }
                } else {
                    ret = 1;
                }
                if !item.is_null() {
                    xml_free(item as _);
                }
                cur = end;

                if ret != 0 || *cur == 0 {
                    break;
                }
            }
        }

        ret
    }
}

/// Allocate a new annotation structure.
///
/// Returns the newly allocated structure or NULL in case or error
#[doc(alias = "xmlSchemaNewAnnot")]
pub(crate) unsafe fn xml_schema_new_annot(
    ctxt: XmlSchemaParserCtxtPtr,
    node: XmlNodePtr,
) -> XmlSchemaAnnotPtr {
    unsafe {
        let ret: XmlSchemaAnnotPtr = xml_malloc(size_of::<XmlSchemaAnnot>()) as _;
        if ret.is_null() {
            xml_schema_perr_memory(ctxt, "allocating annotation", Some(node.into()));
            return null_mut();
        }
        std::ptr::write(
            &mut *ret,
            XmlSchemaAnnot {
                next: null_mut(),
                content: node,
            },
        );
        ret
    }
}

/// Extracts and validates a value against the given built-in type.
/// This one is intended to be used internally for validation
/// of schema attribute values during parsing of the schema.
///
/// Returns 0 if the value is valid, a positive error code
/// number otherwise and -1 in case of an internal or API error.
#[doc(alias = "xmlSchemaPValAttr")]
pub(crate) unsafe fn xml_schema_pval_attr(
    ctxt: XmlSchemaParserCtxtPtr,
    owner_item: XmlSchemaBasicItemPtr,
    owner_elem: XmlNodePtr,
    name: &str,
    typ: XmlSchemaTypePtr,
    value: *mut *const XmlChar,
) -> i32 {
    unsafe {
        if ctxt.is_null() || typ.is_null() {
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
            ctxt,
            Some(owner_elem.into()),
            XmlParserErrors::XmlSchemapInternal,
            format!("Internal error: xmlSchemaPValAttr, the given type '{name}' is not a built-in type.\n").as_str(),
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
        xml_schema_pval_attr_node(ctxt, owner_item, attr, typ, value)
    }
}

/// Seeks an attribute with a local name of @name and a namespace URI of @uri.
///
/// Returns the attribute or NULL if not present.
#[doc(alias = "xmlSchemaGetPropNodeNs")]
pub(crate) unsafe fn xml_schema_get_prop_node_ns(
    node: XmlNodePtr,
    uri: *const c_char,
    name: &str,
) -> Option<XmlAttrPtr> {
    unsafe {
        let mut prop = node.properties;
        while let Some(now) = prop {
            if now.name().as_deref() == Some(name)
                && now.ns.is_some_and(|ns| xml_str_equal(ns.href, uri as _))
            {
                return Some(now);
            }
            prop = now.next;
        }
        None
    }
}

pub(crate) unsafe fn xml_schema_build_absolute_uri(
    dict: XmlDictPtr,
    location: *const XmlChar,
    ctxt_node: Option<XmlGenericNodePtr>,
) -> *const XmlChar {
    unsafe {
        // Build an absolute location URI.
        if !location.is_null() {
            if let Some(ctxt_node) = ctxt_node {
                let location = CStr::from_ptr(location as *const i8).to_string_lossy();

                let uri = if let Some(base) = ctxt_node.get_base(ctxt_node.document()) {
                    build_uri(&location, &base)
                } else {
                    ctxt_node.document().as_deref().and_then(|doc| {
                        doc.url
                            .as_deref()
                            .and_then(|base| build_uri(&location, base))
                    })
                };
                if let Some(uri) = uri {
                    let uri = CString::new(uri).unwrap();
                    return xml_dict_lookup(dict, uri.as_ptr() as *const u8, -1);
                }
            } else {
                return location;
            }
        }
        null_mut()
    }
}

pub(crate) unsafe fn xml_schema_parse_new_doc(
    pctxt: XmlSchemaParserCtxtPtr,
    schema: XmlSchemaPtr,
    bucket: XmlSchemaBucketPtr,
) -> i32 {
    unsafe {
        if bucket.is_null() {
            return 0;
        }
        if (*bucket).parsed != 0 {
            PERROR_INT!(pctxt, "xmlSchemaParseNewDoc", "reparsing a schema doc");
            return -1;
        }
        if (*bucket).doc.is_none() {
            PERROR_INT!(
                pctxt,
                "xmlSchemaParseNewDoc",
                "parsing a schema doc, but there's no doc"
            );
            return -1;
        }
        if (*pctxt).constructor.is_null() {
            PERROR_INT!(pctxt, "xmlSchemaParseNewDoc", "no constructor");
            return -1;
        }
        // Create and init the temporary parser context.
        let newpctxt: XmlSchemaParserCtxtPtr = xml_schema_new_parser_ctxt_use_dict(
            (!(*bucket).schema_location.is_null())
                .then(|| CStr::from_ptr((*bucket).schema_location as *const i8).to_string_lossy())
                .as_deref(),
            (*pctxt).dict,
        );
        if newpctxt.is_null() {
            return -1;
        }
        (*newpctxt).constructor = (*pctxt).constructor;
        // TODO: Can we avoid that the parser knows about the main schema?
        // It would be better if he knows about the current schema bucket only.
        (*newpctxt).schema = schema;
        (*newpctxt).set_errors((*pctxt).error, (*pctxt).warning, (*pctxt).err_ctxt.clone());
        (*newpctxt).set_structured_errors((*pctxt).serror, (*pctxt).err_ctxt.clone());
        (*newpctxt).counter = (*pctxt).counter;

        let res: i32 = (*newpctxt).parse_new_doc_with_context(schema, bucket);

        // Channel back errors and cleanup the temporary parser context.
        if res != 0 {
            (*pctxt).err = res;
        }
        (*pctxt).nberrors += (*newpctxt).nberrors;
        (*pctxt).counter = (*newpctxt).counter;
        (*newpctxt).constructor = null_mut();
        // Free the parser context.
        xml_schema_free_parser_ctxt(newpctxt);
        res
    }
}

/// Adds a redefinition information. This is used at a later stage to:
/// resolve references to the redefined components and to check constraints.
#[doc(alias = "xmlSchemaAddRedef")]
unsafe fn xml_schema_add_redef(
    pctxt: XmlSchemaParserCtxtPtr,
    target_bucket: XmlSchemaBucketPtr,
    item: *mut c_void,
    ref_name: *const XmlChar,
    ref_target_ns: *const XmlChar,
) -> XmlSchemaRedefPtr {
    unsafe {
        let ret: XmlSchemaRedefPtr = xml_malloc(size_of::<XmlSchemaRedef>()) as XmlSchemaRedefPtr;
        if ret.is_null() {
            xml_schema_perr_memory(pctxt, "allocating redefinition info", None);
            return null_mut();
        }
        memset(ret as _, 0, size_of::<XmlSchemaRedef>());
        (*ret).item = item as _;
        (*ret).target_bucket = target_bucket;
        (*ret).ref_name = ref_name;
        (*ret).ref_target_ns = ref_target_ns;
        if (*WXS_CONSTRUCTOR!(pctxt)).redefs.is_null() {
            (*WXS_CONSTRUCTOR!(pctxt)).redefs = ret;
        } else {
            (*(*WXS_CONSTRUCTOR!(pctxt)).last_redef).next = ret;
        }
        (*WXS_CONSTRUCTOR!(pctxt)).last_redef = ret;

        ret
    }
}

/// Add an XML schema item
/// *WARNING* this interface is highly subject to change
///
/// Returns the new structure or NULL in case of error
#[doc(alias = "xmlSchemaAddType")]
pub(crate) unsafe fn xml_schema_add_type(
    ctxt: XmlSchemaParserCtxtPtr,
    schema: XmlSchemaPtr,
    typ: XmlSchemaTypeType,
    name: *const XmlChar,
    ns_name: *const XmlChar,
    node: Option<XmlNodePtr>,
    top_level: i32,
) -> XmlSchemaTypePtr {
    unsafe {
        let mut ret: XmlSchemaTypePtr;

        if ctxt.is_null() || schema.is_null() {
            return null_mut();
        }

        ret = xml_malloc(size_of::<XmlSchemaType>()) as XmlSchemaTypePtr;
        if ret.is_null() {
            xml_schema_perr_memory(ctxt, "allocating type", None);
            return null_mut();
        }
        memset(ret as _, 0, size_of::<XmlSchemaType>());
        (*ret).typ = typ;
        (*ret).name = name;
        (*ret).target_namespace = ns_name;
        (*ret).node = node;
        if top_level != 0 {
            if (*ctxt).is_redefine != 0 {
                (*ctxt).redef =
                    xml_schema_add_redef(ctxt, (*ctxt).redefined, ret as _, name, ns_name);
                if (*ctxt).redef.is_null() {
                    xml_free(ret as _);
                    return null_mut();
                }
                (*ctxt).redef_counter = 0;
            }
            WXS_ADD_GLOBAL!(ctxt, ret);
        } else {
            WXS_ADD_LOCAL!(ctxt, ret);
        }
        WXS_ADD_PENDING!(ctxt, ret);
        ret
    }
}

/// Extracts the local name and the URI of a QName value and validates it.
/// This one is intended to be used on attribute values that
/// should resolve to schema components.
///
/// Returns 0, in case the QName is valid, a positive error code
/// if not valid and -1 if an internal error occurs.
#[doc(alias = "xmlSchemaPValAttrNodeQNameValue")]
unsafe fn xml_schema_pval_attr_node_qname_value(
    ctxt: XmlSchemaParserCtxtPtr,
    schema: XmlSchemaPtr,
    owner_item: XmlSchemaBasicItemPtr,
    attr: XmlAttrPtr,
    value: *const XmlChar,
    uri: *mut *const XmlChar,
    local: *mut *const XmlChar,
) -> i32 {
    unsafe {
        let mut len: i32 = 0;

        *uri = null_mut();
        *local = null_mut();
        if validate_qname::<true>(
            CStr::from_ptr(value as *const i8)
                .to_string_lossy()
                .as_ref(),
        )
        .is_err()
        {
            let v = CStr::from_ptr(value as *const i8).to_string_lossy();
            xml_schema_psimple_type_err(
                ctxt,
                XmlParserErrors::XmlSchemapS4sAttrInvalidValue,
                owner_item,
                attr.into(),
                xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasQName),
                None,
                Some(&v),
                None,
                None,
                None,
            );
            *local = value;
            return (*ctxt).err;
        }

        if strchr(value as *mut c_char, b':' as _).is_null() {
            let ns = attr.parent().unwrap().search_ns(attr.doc, None);
            if let Some(ns) = ns.filter(|ns| !ns.href.is_null() && *ns.href.add(0) != 0) {
                *uri = xml_dict_lookup((*ctxt).dict, ns.href, -1);
            } else if (*schema).flags & XML_SCHEMAS_INCLUDING_CONVERT_NS != 0 {
                // TODO: move XML_SCHEMAS_INCLUDING_CONVERT_NS to the parser context.

                // This one takes care of included schemas with no target namespace.
                *uri = (*ctxt).target_namespace;
            }
            *local = xml_dict_lookup((*ctxt).dict, value, -1);
            return 0;
        }
        // At this point xmlSplitQName3 has to return a local name.
        *local = xml_split_qname3(value, &raw mut len);
        *local = xml_dict_lookup((*ctxt).dict, *local, -1);
        let pref: *const XmlChar = xml_dict_lookup((*ctxt).dict, value, len);
        let Some(ns) = attr.parent().unwrap().search_ns(
            attr.doc,
            Some(CStr::from_ptr(pref as *const i8).to_string_lossy()).as_deref(),
        ) else {
            let value = CStr::from_ptr(value as *const i8).to_string_lossy();
            xml_schema_psimple_type_err(
            ctxt,
            XmlParserErrors::XmlSchemapS4sAttrInvalidValue,
            owner_item,
            attr.into(),
            xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasQName),
            None,
            Some(&value),
            Some(format!("The value '{value}' of simple type 'xs:QName' has no corresponding namespace declaration in scope").as_str()),
            Some(&value),
            None
        );
            return (*ctxt).err;
        };
        *uri = xml_dict_lookup((*ctxt).dict, ns.href, -1);
        0
    }
}

/// Extracts and validates the QName of an attribute value.
/// This one is intended to be used on attribute values that
/// should resolve to schema components.
///
/// Returns 0, in case the QName is valid, a positive error code
/// if not valid and -1 if an internal error occurs.
#[doc(alias = "xmlSchemaPValAttrNodeQName")]
pub(crate) unsafe fn xml_schema_pval_attr_node_qname(
    ctxt: XmlSchemaParserCtxtPtr,
    schema: XmlSchemaPtr,
    owner_item: XmlSchemaBasicItemPtr,
    attr: XmlAttrPtr,
    uri: *mut *const XmlChar,
    local: *mut *const XmlChar,
) -> i32 {
    unsafe {
        let value = (*ctxt).get_node_content(Some(attr.into()));
        let value = CString::new(value).unwrap();
        xml_schema_pval_attr_node_qname_value(
            ctxt,
            schema,
            owner_item,
            attr,
            value.as_ptr() as *const u8,
            uri,
            local,
        )
    }
}

/// Extracts and validates the QName of an attribute value.
///
/// Returns 0, in case the QName is valid, a positive error code
/// if not valid and -1 if an internal error occurs.
#[doc(alias = "xmlSchemaPValAttrQName")]
pub(crate) unsafe fn xml_schema_pval_attr_qname(
    ctxt: XmlSchemaParserCtxtPtr,
    schema: XmlSchemaPtr,
    owner_item: XmlSchemaBasicItemPtr,
    owner_elem: XmlNodePtr,
    name: &str,
    uri: *mut *const XmlChar,
    local: *mut *const XmlChar,
) -> i32 {
    unsafe {
        let Some(attr) = xml_schema_get_prop_node(owner_elem, name) else {
            *local = null_mut();
            *uri = null_mut();
            return 0;
        };
        xml_schema_pval_attr_node_qname(ctxt, schema, owner_item, attr, uri, local)
    }
}

macro_rules! ADD_ANNOTATION {
    ($item:expr, $annot:ident) => {
        let mut cur: XmlSchemaAnnotPtr;
        if (*$item).$annot.is_null() {
            (*$item).$annot = $annot;
            return $annot;
        }
        cur = (*$item).$annot;
        if !(*cur).next.is_null() {
            cur = (*cur).next;
        }
        (*cur).next = $annot;
    };
}

/// Adds the annotation to the given schema component.
///
/// Returns the given annotation.
#[doc(alias = "xmlSchemaAssignAnnotation")]
unsafe fn xml_schema_add_annotation(
    ann_item: XmlSchemaAnnotItemPtr,
    annot: XmlSchemaAnnotPtr,
) -> XmlSchemaAnnotPtr {
    unsafe {
        if ann_item.is_null() || annot.is_null() {
            return null_mut();
        }
        match (*ann_item).typ {
            XmlSchemaTypeType::XmlSchemaTypeElement => {
                let item: XmlSchemaElementPtr = ann_item as XmlSchemaElementPtr;
                ADD_ANNOTATION!(item, annot);
            }
            XmlSchemaTypeType::XmlSchemaTypeAttribute => {
                let item: XmlSchemaAttributePtr = ann_item as XmlSchemaAttributePtr;
                ADD_ANNOTATION!(item, annot);
            }
            XmlSchemaTypeType::XmlSchemaTypeAnyAttribute | XmlSchemaTypeType::XmlSchemaTypeAny => {
                let item: XmlSchemaWildcardPtr = ann_item as XmlSchemaWildcardPtr;
                ADD_ANNOTATION!(item, annot);
            }
            XmlSchemaTypeType::XmlSchemaTypeParticle
            | XmlSchemaTypeType::XmlSchemaTypeIDCKey
            | XmlSchemaTypeType::XmlSchemaTypeIDCKeyref
            | XmlSchemaTypeType::XmlSchemaTypeIDCUnique => {
                let item: XmlSchemaAnnotItemPtr = ann_item as XmlSchemaAnnotItemPtr;
                ADD_ANNOTATION!(item, annot);
            }
            XmlSchemaTypeType::XmlSchemaTypeAttributeGroup => {
                let item: XmlSchemaAttributeGroupPtr = ann_item as XmlSchemaAttributeGroupPtr;
                ADD_ANNOTATION!(item, annot);
            }
            XmlSchemaTypeType::XmlSchemaTypeNotation => {
                let item: XmlSchemaNotationPtr = ann_item as XmlSchemaNotationPtr;
                ADD_ANNOTATION!(item, annot);
            }
            XmlSchemaTypeType::XmlSchemaFacetMinInclusive
            | XmlSchemaTypeType::XmlSchemaFacetMinExclusive
            | XmlSchemaTypeType::XmlSchemaFacetMaxInclusive
            | XmlSchemaTypeType::XmlSchemaFacetMaxExclusive
            | XmlSchemaTypeType::XmlSchemaFacetTotalDigits
            | XmlSchemaTypeType::XmlSchemaFacetFractionDigits
            | XmlSchemaTypeType::XmlSchemaFacetPattern
            | XmlSchemaTypeType::XmlSchemaFacetEnumeration
            | XmlSchemaTypeType::XmlSchemaFacetWhitespace
            | XmlSchemaTypeType::XmlSchemaFacetLength
            | XmlSchemaTypeType::XmlSchemaFacetMaxLength
            | XmlSchemaTypeType::XmlSchemaFacetMinLength => {
                let item: XmlSchemaFacetPtr = ann_item as _;
                ADD_ANNOTATION!(item, annot);
            }
            XmlSchemaTypeType::XmlSchemaTypeSimple | XmlSchemaTypeType::XmlSchemaTypeComplex => {
                let item: XmlSchemaTypePtr = ann_item as XmlSchemaTypePtr;
                ADD_ANNOTATION!(item, annot);
            }
            XmlSchemaTypeType::XmlSchemaTypeGroup => {
                let item: XmlSchemaModelGroupDefPtr = ann_item as XmlSchemaModelGroupDefPtr;
                ADD_ANNOTATION!(item, annot);
            }
            XmlSchemaTypeType::XmlSchemaTypeSequence
            | XmlSchemaTypeType::XmlSchemaTypeChoice
            | XmlSchemaTypeType::XmlSchemaTypeAll => {
                let item: XmlSchemaModelGroupPtr = ann_item as XmlSchemaModelGroupPtr;
                ADD_ANNOTATION!(item, annot);
            }
            _ => {
                xml_schema_pcustom_err(
                    null_mut(),
                    XmlParserErrors::XmlSchemapInternal,
                    null_mut(),
                    None,
                    "Internal error: xmlSchemaAddAnnotation, The item is not a annotated schema component",
                    None,
                );
            }
        }
        annot
    }
}

/// Adds a schema model group
/// *WARNING* this interface is highly subject to change
///
/// Returns the new structure or NULL in case of error
#[doc(alias = "xmlSchemaAddModelGroup")]
unsafe fn xml_schema_add_model_group(
    ctxt: XmlSchemaParserCtxtPtr,
    schema: XmlSchemaPtr,
    typ: XmlSchemaTypeType,
    node: XmlNodePtr,
) -> XmlSchemaModelGroupPtr {
    unsafe {
        let mut ret: XmlSchemaModelGroupPtr;

        if ctxt.is_null() || schema.is_null() {
            return null_mut();
        }

        ret = xml_malloc(size_of::<XmlSchemaModelGroup>()) as XmlSchemaModelGroupPtr;
        if ret.is_null() {
            xml_schema_perr_memory(ctxt, "allocating model group component", None);
            return null_mut();
        }
        std::ptr::write(
            &mut *ret,
            XmlSchemaModelGroup {
                typ,
                annot: null_mut(),
                next: null_mut(),
                children: null_mut(),
                node,
            },
        );
        WXS_ADD_LOCAL!(ctxt, ret);
        if matches!(
            typ,
            XmlSchemaTypeType::XmlSchemaTypeSequence | XmlSchemaTypeType::XmlSchemaTypeChoice
        ) {
            WXS_ADD_PENDING!(ctxt, ret);
        }
        ret
    }
}

/// Get the minOccurs property
///
/// Returns the default if not found, or the value
#[doc(alias = "xmlGetMinOccurs")]
pub(crate) unsafe fn xml_get_min_occurs(
    ctxt: XmlSchemaParserCtxtPtr,
    node: XmlNodePtr,
    min: i32,
    max: i32,
    def: i32,
    expected: &str,
) -> i32 {
    unsafe {
        let Some(attr) = xml_schema_get_prop_node(node, "minOccurs") else {
            return def;
        };
        let val = (*ctxt).get_node_content(Some(attr.into()));
        let mut cur = val.as_str();
        cur = cur.trim_start_matches(|c| xml_is_blank_char(c as u32));
        if cur.is_empty() {
            // XML_SCHEMAP_INVALID_MINOCCURS,
            xml_schema_psimple_type_err(
                ctxt,
                XmlParserErrors::XmlSchemapS4sAttrInvalidValue,
                null_mut(),
                attr.into(),
                null_mut(),
                Some(expected),
                Some(&val),
                None,
                None,
                None,
            );
            return def;
        }
        cur = cur.trim_end_matches(|c| xml_is_blank_char(c as u32));
        if cur.bytes().all(|b| b.is_ascii_digit()) {
            // TODO: Restrict the maximal value to Integer.
            if let Some(ret) = cur
                .parse::<i32>()
                .ok()
                .filter(|&ret| min <= ret && (max == -1 || ret <= max))
            {
                return ret;
            }
        }
        // XML_SCHEMAP_INVALID_MINOCCURS,
        xml_schema_psimple_type_err(
            ctxt,
            XmlParserErrors::XmlSchemapS4sAttrInvalidValue,
            null_mut(),
            attr.into(),
            null_mut(),
            Some(expected),
            Some(&val),
            None,
            None,
            None,
        );
        def
    }
}

/// Get the maxOccurs property
///
/// Returns the default if not found, or the value
#[doc(alias = "xmlGetMaxOccurs")]
pub(crate) unsafe fn xml_get_max_occurs(
    ctxt: XmlSchemaParserCtxtPtr,
    node: XmlNodePtr,
    min: i32,
    max: i32,
    def: i32,
    expected: &str,
) -> i32 {
    unsafe {
        let Some(attr) = xml_schema_get_prop_node(node, "maxOccurs") else {
            return def;
        };
        let val = (*ctxt).get_node_content(Some(attr.into()));

        if val == "unbounded" {
            if max != UNBOUNDED as i32 {
                // XML_SCHEMAP_INVALID_MINOCCURS,
                xml_schema_psimple_type_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapS4sAttrInvalidValue,
                    null_mut(),
                    attr.into(),
                    null_mut(),
                    Some(expected),
                    Some(&val),
                    None,
                    None,
                    None,
                );
                return def;
            } else {
                // encoding it with -1 might be another option
                return UNBOUNDED as _;
            }
        }

        let mut cur = val.as_str();
        cur = cur.trim_start_matches(|c| xml_is_blank_char(c as u32));
        if cur.is_empty() {
            // XML_SCHEMAP_INVALID_MINOCCURS,
            xml_schema_psimple_type_err(
                ctxt,
                XmlParserErrors::XmlSchemapS4sAttrInvalidValue,
                null_mut(),
                attr.into(),
                null_mut(),
                Some(expected),
                Some(&val),
                None,
                None,
                None,
            );
            return def;
        }
        cur = cur.trim_end_matches(|c| xml_is_blank_char(c as u32));
        if cur.bytes().all(|b| b.is_ascii_digit()) {
            // TODO: Restrict the maximal value to Integer.
            if let Some(ret) = cur
                .parse::<i32>()
                .ok()
                .filter(|&ret| min <= ret && (max == -1 || ret <= max))
            {
                return ret;
            }
        }
        // XML_SCHEMAP_INVALID_MINOCCURS,
        xml_schema_psimple_type_err(
            ctxt,
            XmlParserErrors::XmlSchemapS4sAttrInvalidValue,
            null_mut(),
            attr.into(),
            null_mut(),
            Some(expected),
            Some(&val),
            None,
            None,
            None,
        );
        def
    }
}

pub(crate) unsafe fn xml_schema_pcheck_particle_correct_2(
    ctxt: XmlSchemaParserCtxtPtr,
    _item: XmlSchemaParticlePtr,
    node: XmlNodePtr,
    min_occurs: i32,
    max_occurs: i32,
) -> i32 {
    unsafe {
        if max_occurs == 0 && min_occurs == 0 {
            return 0;
        }
        if max_occurs != UNBOUNDED as i32 {
            // TODO: Maybe we should better not create the particle,
            // if min/max is invalid, since it could confuse the build of the
            // content model.

            // 3.9.6 Schema Component Constraint: Particle Correct
            if max_occurs < 1 {
                // 2.2 {max occurs} must be greater than or equal to 1.
                xml_schema_pcustom_attr_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapPPropsCorrect2_2,
                    null_mut(),
                    null_mut(),
                    xml_schema_get_prop_node(node, "maxOccurs"),
                    "The value must be greater than or equal to 1",
                );
                return XmlParserErrors::XmlSchemapPPropsCorrect2_2 as i32;
            } else if min_occurs > max_occurs {
                // 2.1 {min occurs} must not be greater than {max occurs}.
                xml_schema_pcustom_attr_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapPPropsCorrect2_1,
                    null_mut(),
                    null_mut(),
                    xml_schema_get_prop_node(node, "minOccurs"),
                    "The value must not be greater than the value of 'maxOccurs'",
                );
                return XmlParserErrors::XmlSchemapPPropsCorrect2_1 as i32;
            }
        }
        0
    }
}

/// Adds an XML schema particle component.
/// *WARNING* this interface is highly subject to change
///
/// Returns the new structure or NULL in case of error
#[doc(alias = "xmlSchemaAddParticle")]
pub(crate) unsafe fn xml_schema_add_particle(
    ctxt: XmlSchemaParserCtxtPtr,
    node: Option<XmlNodePtr>,
    min: i32,
    max: i32,
) -> XmlSchemaParticlePtr {
    unsafe {
        let mut ret: XmlSchemaParticlePtr;
        if ctxt.is_null() {
            return null_mut();
        }

        ret = xml_malloc(size_of::<XmlSchemaParticle>()) as XmlSchemaParticlePtr;
        if ret.is_null() {
            xml_schema_perr_memory(ctxt, "allocating particle component", None);
            return null_mut();
        }
        (*ret).typ = XmlSchemaTypeType::XmlSchemaTypeParticle;
        (*ret).annot = null_mut();
        (*ret).node = node;
        (*ret).min_occurs = min;
        (*ret).max_occurs = max;
        (*ret).next = null_mut();
        (*ret).children = null_mut();

        WXS_ADD_LOCAL!(ctxt, ret);
        // Note that addition to pending components will be done locally
        // to the specific parsing function, since the most particles
        // need not to be fixed up (i.e. the reference to be resolved).
        // REMOVED: WXS_ADD_PENDING!(ctxt, ret);
        ret
    }
}

pub(crate) unsafe fn xml_schema_check_reference(
    pctxt: XmlSchemaParserCtxtPtr,
    _schema: XmlSchemaPtr,
    node: XmlNodePtr,
    attr: Option<XmlAttrPtr>,
    namespace_name: *const XmlChar,
) -> i32 {
    unsafe {
        // TODO: Pointer comparison instead?
        if xml_str_equal((*pctxt).target_namespace, namespace_name) {
            return 0;
        }
        if xml_str_equal(XML_SCHEMA_NS.as_ptr() as _, namespace_name) {
            return 0;
        }
        // Check if the referenced namespace was <import>ed.
        if !(*WXS_BUCKET!(pctxt)).relations.is_null() {
            let mut rel: XmlSchemaSchemaRelationPtr;

            rel = (*WXS_BUCKET!(pctxt)).relations;
            while {
                if WXS_IS_BUCKET_IMPMAIN!((*rel).typ)
                    && xml_str_equal(namespace_name, (*rel).import_namespace)
                {
                    return 0;
                }
                rel = (*rel).next;
                !rel.is_null()
            } {}
        }
        // No matching <import>ed namespace found.
        {
            let n = attr.map_or(Some(XmlGenericNodePtr::from(node)), |attr| {
                Some(attr.into())
            });

            if namespace_name.is_null() {
                xml_schema_custom_err(
                    pctxt as XmlSchemaAbstractCtxtPtr,
                    XmlParserErrors::XmlSchemapSrcResolve,
                    n,
                    null_mut(),
                    "References from this schema to components in no namespace are not allowed, since not indicated by an import statement",
                    None,
                    None,
                );
            } else {
                let namespace_name = CStr::from_ptr(namespace_name as *const i8).to_string_lossy();
                xml_schema_custom_err(
                pctxt as XmlSchemaAbstractCtxtPtr,
                XmlParserErrors::XmlSchemapSrcResolve,
                n,
                null_mut(),
                format!("References from this schema to components in the namespace '{namespace_name}' are not allowed, since not indicated by an import statement").as_str(),
                Some(&namespace_name),
                None
            );
            }
        }
        XmlParserErrors::XmlSchemapSrcResolve as i32
    }
}

pub(crate) unsafe fn xml_schema_new_qname_ref(
    pctxt: XmlSchemaParserCtxtPtr,
    ref_type: XmlSchemaTypeType,
    ref_name: *const XmlChar,
    ref_ns: *const XmlChar,
) -> XmlSchemaQNameRefPtr {
    unsafe {
        let mut ret: XmlSchemaQNameRefPtr;

        ret = xml_malloc(size_of::<XmlSchemaQNameRef>()) as XmlSchemaQNameRefPtr;
        if ret.is_null() {
            xml_schema_perr_memory(pctxt, "allocating QName reference item", None);
            return null_mut();
        }
        (*ret).node = None;
        (*ret).typ = XmlSchemaTypeType::XmlSchemaExtraQNameRef;
        (*ret).name = ref_name;
        (*ret).target_namespace = ref_ns;
        (*ret).item = null_mut();
        (*ret).item_type = ref_type;
        // Store the reference item in the schema.
        WXS_ADD_LOCAL!(pctxt, ret);
        ret
    }
}

/// Add an XML schema Element declaration
/// *WARNING* this interface is highly subject to change
///
/// Returns the new structure or NULL in case of error
#[doc(alias = "xmlSchemaAddElement")]
pub(crate) unsafe fn xml_schema_add_element(
    ctxt: XmlSchemaParserCtxtPtr,
    name: *const XmlChar,
    ns_name: *const XmlChar,
    node: XmlNodePtr,
    top_level: i32,
) -> XmlSchemaElementPtr {
    unsafe {
        let mut ret: XmlSchemaElementPtr;

        if ctxt.is_null() || name.is_null() {
            return null_mut();
        }

        ret = xml_malloc(size_of::<XmlSchemaElement>()) as XmlSchemaElementPtr;
        if ret.is_null() {
            xml_schema_perr_memory(ctxt, "allocating element", None);
            return null_mut();
        }
        memset(ret as _, 0, size_of::<XmlSchemaElement>());
        (*ret).typ = XmlSchemaTypeType::XmlSchemaTypeElement;
        (*ret).name = name;
        (*ret).target_namespace = ns_name;
        (*ret).node = node.into();

        if top_level != 0 {
            WXS_ADD_GLOBAL!(ctxt, ret);
        } else {
            WXS_ADD_LOCAL!(ctxt, ret);
        }
        WXS_ADD_PENDING!(ctxt, ret);
        ret
    }
}

/// Converts a boolean string value into 1 or 0.
///
/// Returns 0 or 1.
#[doc(alias = "xmlSchemaPGetBoolNodeValue")]
pub(crate) unsafe fn xml_schema_pget_bool_node_value(
    ctxt: XmlSchemaParserCtxtPtr,
    owner_item: XmlSchemaBasicItemPtr,
    node: XmlGenericNodePtr,
) -> i32 {
    unsafe {
        let mut res: i32 = 0;

        let value = node.get_content();
        // 3.2.2.1 Lexical representation
        // An instance of a datatype that is defined as `boolean`
        // can have the following legal literals {true, false, 1, 0}.
        if value.as_deref() == Some("true") {
            res = 1;
        } else if value.as_deref() == Some("false") {
            res = 0;
        } else if value.as_deref() == Some("1") {
            res = 1;
        } else if value.as_deref() == Some("0") {
            res = 0;
        } else {
            xml_schema_psimple_type_err(
                ctxt,
                XmlParserErrors::XmlSchemapInvalidBoolean,
                owner_item,
                node,
                xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasBoolean),
                None,
                value.as_deref(),
                None,
                None,
                None,
            );
        }
        res
    }
}

/// Parses a reference to a model group definition.
///
/// We will return a particle component with a qname-component or NULL in case of an error.
#[doc(alias = "xmlSchemaParseModelGroupDefRef")]
pub(crate) unsafe fn xml_schema_parse_model_group_def_ref(
    ctxt: XmlSchemaParserCtxtPtr,
    schema: XmlSchemaPtr,
    node: XmlNodePtr,
) -> XmlSchemaTreeItemPtr {
    unsafe {
        let mut refe: *const XmlChar = null();
        let mut ref_ns: *const XmlChar = null();

        if ctxt.is_null() || schema.is_null() {
            return null_mut();
        }

        let Some(attr) = xml_schema_get_prop_node(node, "ref") else {
            xml_schema_pmissing_attr_err(
                ctxt,
                XmlParserErrors::XmlSchemapS4sAttrMissing,
                null_mut(),
                Some(node.into()),
                Some("ref"),
                None,
            );
            return null_mut();
        };
        if xml_schema_pval_attr_node_qname(
            ctxt,
            schema,
            null_mut(),
            attr,
            &raw mut ref_ns,
            &raw mut refe,
        ) != 0
        {
            return null_mut();
        }
        xml_schema_check_reference(ctxt, schema, node, Some(attr), ref_ns);
        let min: i32 = xml_get_min_occurs(ctxt, node, 0, -1, 1, "xs:nonNegativeInteger");
        let max: i32 = xml_get_max_occurs(
            ctxt,
            node,
            0,
            UNBOUNDED as _,
            1,
            "(xs:nonNegativeInteger | unbounded)",
        );
        // Check for illegal attributes.
        let mut attr = node.properties;
        while let Some(cur_attr) = attr {
            if let Some(ns) = cur_attr.ns {
                if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                    xml_schema_pillegal_attr_err(
                        ctxt,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
            } else if !xml_str_equal(cur_attr.name, c"ref".as_ptr() as _)
                && !xml_str_equal(cur_attr.name, c"id".as_ptr() as _)
                && !xml_str_equal(cur_attr.name, c"minOccurs".as_ptr() as _)
                && !xml_str_equal(cur_attr.name, c"maxOccurs".as_ptr() as _)
            {
                xml_schema_pillegal_attr_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                    null_mut(),
                    cur_attr,
                );
            }
            attr = cur_attr.next;
        }
        xml_schema_pval_attr_id(ctxt, node, "id");
        let item: XmlSchemaParticlePtr = xml_schema_add_particle(ctxt, Some(node), min, max);
        if item.is_null() {
            return null_mut();
        }
        // Create a qname-reference and set as the term; it will be substituted
        // for the model group after the reference has been resolved.
        (*item).children =
            xml_schema_new_qname_ref(ctxt, XmlSchemaTypeType::XmlSchemaTypeGroup, refe, ref_ns)
                as XmlSchemaTreeItemPtr;
        xml_schema_pcheck_particle_correct_2(ctxt, item, node, min, max);
        // And now for the children...
        let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
        // TODO: Is annotation even allowed for a model group reference?
        if is_schema(child, "annotation") {
            // TODO: What to do exactly with the annotation?
            (*item).annot = (*ctxt).parse_annotation(child.unwrap(), 1);
            child = child
                .unwrap()
                .next
                .map(|node| XmlNodePtr::try_from(node).unwrap());
        }
        if let Some(child) = child {
            xml_schema_pcontent_err(
                ctxt,
                XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                null_mut(),
                node,
                Some(child.into()),
                None,
                Some("(annotation?)"),
            );
        }
        // Corresponds to no component at all if minOccurs==maxOccurs==0.
        if min == 0 && max == 0 {
            return null_mut();
        }

        item as XmlSchemaTreeItemPtr
    }
}

/// Add an XML schema Attribute declaration
/// *WARNING* this interface is highly subject to change
///
/// Returns the new structure or NULL in case of error
#[doc(alias = "xmlSchemaAddAttributeUse")]
unsafe fn xml_schema_add_attribute_use(
    pctxt: XmlSchemaParserCtxtPtr,
    node: XmlNodePtr,
) -> XmlSchemaAttributeUsePtr {
    unsafe {
        let mut ret: XmlSchemaAttributeUsePtr;

        if pctxt.is_null() {
            return null_mut();
        }

        ret = xml_malloc(size_of::<XmlSchemaAttributeUse>()) as XmlSchemaAttributeUsePtr;
        if ret.is_null() {
            xml_schema_perr_memory(pctxt, "allocating attribute", None);
            return null_mut();
        }
        std::ptr::write(
            &mut *ret,
            XmlSchemaAttributeUse {
                typ: XmlSchemaTypeType::XmlSchemaTypeAttributeUse,
                node,
                annot: null_mut(),
                next: null_mut(),
                attr_decl: null_mut(),
                flags: 0,
                occurs: 0,
                def_val: null_mut(),
                def_value: null(),
            },
        );

        WXS_ADD_LOCAL!(pctxt, ret);
        ret
    }
}

/// Add an XML schema Attribute declaration
/// *WARNING* this interface is highly subject to change
///
/// Returns the new structure or NULL in case of error
#[doc(alias = "xmlSchemaAddAttribute")]
pub(crate) unsafe fn xml_schema_add_attribute(
    ctxt: XmlSchemaParserCtxtPtr,
    schema: XmlSchemaPtr,
    name: *const XmlChar,
    ns_name: *const XmlChar,
    node: XmlNodePtr,
    top_level: i32,
) -> XmlSchemaAttributePtr {
    unsafe {
        let mut ret: XmlSchemaAttributePtr;

        if ctxt.is_null() || schema.is_null() {
            return null_mut();
        }

        ret = xml_malloc(size_of::<XmlSchemaAttribute>()) as XmlSchemaAttributePtr;
        if ret.is_null() {
            xml_schema_perr_memory(ctxt, "allocating attribute", None);
            return null_mut();
        }
        memset(ret as _, 0, size_of::<XmlSchemaAttribute>());
        (*ret).typ = XmlSchemaTypeType::XmlSchemaTypeAttribute;
        (*ret).node = node.into();
        (*ret).name = name;
        (*ret).target_namespace = ns_name;

        if top_level != 0 {
            WXS_ADD_GLOBAL!(ctxt, ret);
        } else {
            WXS_ADD_LOCAL!(ctxt, ret);
        }
        WXS_ADD_PENDING!(ctxt, ret);
        ret
    }
}

unsafe fn xml_schema_add_attribute_use_prohib(
    pctxt: XmlSchemaParserCtxtPtr,
) -> XmlSchemaAttributeUseProhibPtr {
    unsafe {
        let mut ret: XmlSchemaAttributeUseProhibPtr;

        ret =
            xml_malloc(size_of::<XmlSchemaAttributeUseProhib>()) as XmlSchemaAttributeUseProhibPtr;
        if ret.is_null() {
            xml_schema_perr_memory(pctxt, "allocating attribute use prohibition", None);
            return null_mut();
        }
        memset(ret as _, 0, size_of::<XmlSchemaAttributeUseProhib>());
        (*ret).typ = XmlSchemaTypeType::XmlSchemaExtraAttrUseProhib;
        WXS_ADD_LOCAL!(pctxt, ret);
        ret
    }
}

/// Parse a XML schema Attribute declaration
/// *WARNING* this interface is highly subject to change
///
/// Returns the attribute declaration.
#[doc(alias = "xmlSchemaParseAttribute")]
unsafe fn xml_schema_parse_local_attribute(
    pctxt: XmlSchemaParserCtxtPtr,
    schema: XmlSchemaPtr,
    node: XmlNodePtr,
    uses: XmlSchemaItemListPtr<*mut c_void>,
    parent_type: i32,
) -> XmlSchemaBasicItemPtr {
    unsafe {
        let mut name: *const XmlChar = null();
        let mut ns: *const XmlChar = null();
        let mut using: XmlSchemaAttributeUsePtr = null_mut();
        let mut tmp_ns: *const XmlChar = null();
        let mut tmp_name: *const XmlChar = null();
        let mut def_value = None;
        let mut is_ref: i32 = 0;
        let mut occurs: i32 = XML_SCHEMAS_ATTR_USE_OPTIONAL;

        let mut has_form: i32 = 0;
        let mut def_value_type: i32 = 0;

        const WXS_ATTR_DEF_VAL_DEFAULT: i32 = 1;
        const WXS_ATTR_DEF_VAL_FIXED: i32 = 2;

        // 3.2.3 Constraints on XML Representations of Attribute Declarations

        if pctxt.is_null() || schema.is_null() {
            return null_mut();
        }
        if let Some(attr) = xml_schema_get_prop_node(node, "ref") {
            if xml_schema_pval_attr_node_qname(
                pctxt,
                schema,
                null_mut(),
                attr,
                &raw mut tmp_ns,
                &raw mut tmp_name,
            ) != 0
            {
                return null_mut();
            }
            if xml_schema_check_reference(pctxt, schema, node, Some(attr), tmp_ns) != 0 {
                return null_mut();
            }
            is_ref = 1;
        }
        let nberrors: i32 = (*pctxt).nberrors;
        // Check for illegal attributes.
        let mut attr = node.properties;
        while let Some(cur_attr) = attr {
            'attr_next: {
                if let Some(ns) = cur_attr.ns {
                    if ns.href().as_deref() != Some(XML_SCHEMA_NS.to_str().unwrap()) {
                        break 'attr_next;
                    }
                } else {
                    if is_ref != 0 {
                        if xml_str_equal(cur_attr.name, c"id".as_ptr() as _) {
                            xml_schema_pval_attr_node_id(pctxt, Some(cur_attr));
                            break 'attr_next;
                        } else if xml_str_equal(cur_attr.name, c"ref".as_ptr() as _) {
                            break 'attr_next;
                        }
                    } else if xml_str_equal(cur_attr.name, c"name".as_ptr() as _) {
                        break 'attr_next;
                    } else if xml_str_equal(cur_attr.name, c"id".as_ptr() as _) {
                        xml_schema_pval_attr_node_id(pctxt, Some(cur_attr));
                        break 'attr_next;
                    } else if xml_str_equal(cur_attr.name, c"type".as_ptr() as _) {
                        xml_schema_pval_attr_node_qname(
                            pctxt,
                            schema,
                            null_mut(),
                            cur_attr,
                            &raw mut tmp_ns,
                            &raw mut tmp_name,
                        );
                        break 'attr_next;
                    } else if xml_str_equal(cur_attr.name, c"form".as_ptr() as _) {
                        // Evaluate the target namespace
                        has_form = 1;
                        let attr_value = (*pctxt).get_node_content(Some(cur_attr.into()));
                        if attr_value == "qualified" {
                            ns = (*pctxt).target_namespace;
                        } else if attr_value != "unqualified" {
                            xml_schema_psimple_type_err(
                                pctxt,
                                XmlParserErrors::XmlSchemapS4sAttrInvalidValue,
                                null_mut(),
                                cur_attr.into(),
                                null_mut(),
                                Some("(qualified | unqualified)"),
                                Some(&attr_value),
                                None,
                                None,
                                None,
                            );
                        }
                        break 'attr_next;
                    }
                    if xml_str_equal(cur_attr.name, c"use".as_ptr() as _) {
                        let attr_value = (*pctxt).get_node_content(Some(cur_attr.into()));
                        // TODO: Maybe we need to normalize the value beforehand.
                        if attr_value == "optional" {
                            occurs = XML_SCHEMAS_ATTR_USE_OPTIONAL;
                        } else if attr_value == "prohibited" {
                            occurs = XML_SCHEMAS_ATTR_USE_PROHIBITED;
                        } else if attr_value == "required" {
                            occurs = XML_SCHEMAS_ATTR_USE_REQUIRED;
                        } else {
                            xml_schema_psimple_type_err(
                                pctxt,
                                XmlParserErrors::XmlSchemapInvalidAttrUse,
                                null_mut(),
                                cur_attr.into(),
                                null_mut(),
                                Some("(optional | prohibited | required)"),
                                Some(&attr_value),
                                None,
                                None,
                                None,
                            );
                        }
                        break 'attr_next;
                    } else if xml_str_equal(cur_attr.name, c"default".as_ptr() as _) {
                        // 3.2.3 : 1
                        // default and fixed must not both be present.
                        if def_value.is_some() {
                            xml_schema_pmutual_excl_attr_err(
                                pctxt,
                                XmlParserErrors::XmlSchemapSrcAttribute1,
                                null_mut(),
                                cur_attr,
                                c"default".as_ptr() as _,
                                c"fixed".as_ptr() as _,
                            );
                        } else {
                            def_value = Some((*pctxt).get_node_content(Some(cur_attr.into())));
                            def_value_type = WXS_ATTR_DEF_VAL_DEFAULT;
                        }
                        break 'attr_next;
                    } else if xml_str_equal(cur_attr.name, c"fixed".as_ptr() as _) {
                        // 3.2.3 : 1
                        // default and fixed must not both be present.
                        if def_value.is_some() {
                            xml_schema_pmutual_excl_attr_err(
                                pctxt,
                                XmlParserErrors::XmlSchemapSrcAttribute1,
                                null_mut(),
                                cur_attr,
                                c"default".as_ptr() as _,
                                c"fixed".as_ptr() as _,
                            );
                        } else {
                            def_value = Some((*pctxt).get_node_content(Some(cur_attr.into())));
                            def_value_type = WXS_ATTR_DEF_VAL_FIXED;
                        }
                        break 'attr_next;
                    }
                }

                xml_schema_pillegal_attr_err(
                    pctxt,
                    XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                    null_mut(),
                    cur_attr,
                );
            }

            // attr_next:
            attr = cur_attr.next;
        }
        // 3.2.3 : 2
        // If default and use are both present, use must have
        // the actual value optional.
        if def_value_type == WXS_ATTR_DEF_VAL_DEFAULT && occurs != XML_SCHEMAS_ATTR_USE_OPTIONAL {
            xml_schema_psimple_type_err(
                pctxt,
                XmlParserErrors::XmlSchemapSrcAttribute2,
                null_mut(),
                node.into(),
                null_mut(),
                Some("(optional | prohibited | required)"),
                None,
                Some(
                    "The value of the attribute 'use' must be 'optional' if the attribute 'default' is present",
                ),
                None,
                None,
            );
        }
        // We want correct attributes.
        if nberrors != (*pctxt).nberrors {
            return null_mut();
        }
        if is_ref == 0 {
            let attr_decl: XmlSchemaAttributePtr;

            // TODO: move XML_SCHEMAS_QUALIF_ATTR to the parser.
            if has_form == 0 && (*schema).flags & XML_SCHEMAS_QUALIF_ATTR != 0 {
                ns = (*pctxt).target_namespace;
            }
            // 3.2.6 Schema Component Constraint: xsi: Not Allowed
            // TODO: Move this to the component layer.
            if xml_str_equal(ns, XML_SCHEMA_INSTANCE_NS.as_ptr() as _) {
                let ns = XML_SCHEMA_INSTANCE_NS.to_string_lossy();
                xml_schema_custom_err(
                    pctxt as XmlSchemaAbstractCtxtPtr,
                    XmlParserErrors::XmlSchemapNoXsi,
                    Some(node.into()),
                    null_mut(),
                    format!("The target namespace must not match '{ns}'").as_str(),
                    Some(&ns),
                    None,
                );
            }
            let Some(attr) = xml_schema_get_prop_node(node, "name") else {
                xml_schema_pmissing_attr_err(
                    pctxt,
                    XmlParserErrors::XmlSchemapS4sAttrMissing,
                    null_mut(),
                    Some(node.into()),
                    Some("name"),
                    None,
                );
                return null_mut();
            };
            if xml_schema_pval_attr_node(
                pctxt,
                null_mut(),
                attr,
                xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasNCName),
                &raw mut name,
            ) != 0
            {
                return null_mut();
            }
            // 3.2.6 Schema Component Constraint: xmlns Not Allowed
            // TODO: Move this to the component layer.
            if xml_str_equal(name, c"xmlns".as_ptr() as _) {
                xml_schema_psimple_type_err(
                    pctxt,
                    XmlParserErrors::XmlSchemapNoXmlns,
                    null_mut(),
                    attr.into(),
                    xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasNCName),
                    None,
                    None,
                    Some("The value of the attribute must not match 'xmlns'"),
                    None,
                    None,
                );
                return null_mut();
            }
            if occurs == XML_SCHEMAS_ATTR_USE_PROHIBITED {
                // goto check_children;
            } else {
                // Create the attribute use component.
                using = xml_schema_add_attribute_use(pctxt, node);
                if using.is_null() {
                    return null_mut();
                }
                (*using).occurs = occurs;
                // Create the attribute declaration.
                attr_decl = xml_schema_add_attribute(pctxt, schema, name, ns, node, 0);
                if attr_decl.is_null() {
                    return null_mut();
                }
                if !tmp_name.is_null() {
                    (*attr_decl).type_name = tmp_name;
                    (*attr_decl).type_ns = tmp_ns;
                }
                (*using).attr_decl = attr_decl;
                // Value constraint.
                if let Some(def_value) = def_value {
                    (*attr_decl).def_value =
                        xml_dict_lookup((*pctxt).dict, def_value.as_ptr(), def_value.len() as i32);

                    if def_value_type == WXS_ATTR_DEF_VAL_FIXED {
                        (*attr_decl).flags |= XML_SCHEMAS_ATTR_FIXED;
                    }
                }
            }
        } else if occurs != XML_SCHEMAS_ATTR_USE_PROHIBITED {
            // Create the attribute use component.
            using = xml_schema_add_attribute_use(pctxt, node);
            if using.is_null() {
                return null_mut();
            }
            // We need to resolve the reference at later stage.
            WXS_ADD_PENDING!(pctxt, using);
            (*using).occurs = occurs;
            // Create a QName reference to the attribute declaration.
            let refe: XmlSchemaQNameRefPtr = xml_schema_new_qname_ref(
                pctxt,
                XmlSchemaTypeType::XmlSchemaTypeAttribute,
                tmp_name,
                tmp_ns,
            );
            if refe.is_null() {
                return null_mut();
            }
            // Assign the reference. This will be substituted for the
            // referenced attribute declaration when the QName is resolved.
            (*using).attr_decl = refe as XmlSchemaAttributePtr;
            // Value constraint.
            if let Some(def_value) = def_value {
                (*using).def_value =
                    xml_dict_lookup((*pctxt).dict, def_value.as_ptr(), def_value.len() as i32);
            }
            if def_value_type == WXS_ATTR_DEF_VAL_FIXED {
                (*using).flags |= XML_SCHEMA_ATTR_USE_FIXED;
            }
        }

        // check_children:
        // And now for the children...
        let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
        if occurs == XML_SCHEMAS_ATTR_USE_PROHIBITED {
            if is_schema(child, "annotation") {
                (*pctxt).parse_annotation(child.unwrap(), 0);
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if let Some(child) = child {
                xml_schema_pcontent_err(
                    pctxt,
                    XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                    null_mut(),
                    node,
                    Some(child.into()),
                    None,
                    Some("(annotation?)"),
                );
            }
            // Check for pointlessness of attribute prohibitions.
            if parent_type == XmlSchemaTypeType::XmlSchemaTypeAttributeGroup as i32 {
                xml_schema_custom_warning(
                    pctxt as XmlSchemaAbstractCtxtPtr,
                    XmlParserErrors::XmlSchemapWarnAttrPointlessProh,
                    Some(node.into()),
                    null_mut(),
                    "Skipping attribute use prohibition, since it is pointless inside an <attributeGroup>",
                    None,
                    None,
                    None,
                );
                return null_mut();
            } else if parent_type == XmlSchemaTypeType::XmlSchemaTypeExtension as i32 {
                xml_schema_custom_warning(
                    pctxt as XmlSchemaAbstractCtxtPtr,
                    XmlParserErrors::XmlSchemapWarnAttrPointlessProh,
                    Some(node.into()),
                    null_mut(),
                    "Skipping attribute use prohibition, since it is pointless when extending a type",
                    None,
                    None,
                    None,
                );
                return null_mut();
            }
            if is_ref == 0 {
                tmp_name = name;
                tmp_ns = ns;
            }
            // Check for duplicate attribute prohibitions.
            if !uses.is_null() {
                for using in (*uses)
                    .items
                    .iter()
                    .map(|&using| using as XmlSchemaBasicItemPtr)
                {
                    if (*using).typ == XmlSchemaTypeType::XmlSchemaExtraAttrUseProhib
                        && tmp_name == (*(using as XmlSchemaAttributeUseProhibPtr)).name
                        && tmp_ns == (*(using as XmlSchemaAttributeUseProhibPtr)).target_namespace
                    {
                        let qname = xml_schema_format_qname(
                            Some(
                                CStr::from_ptr(tmp_ns as *const i8)
                                    .to_string_lossy()
                                    .as_ref(),
                            ),
                            Some(
                                CStr::from_ptr(tmp_name as *const i8)
                                    .to_string_lossy()
                                    .as_ref(),
                            ),
                        );
                        xml_schema_custom_warning(
                            pctxt as XmlSchemaAbstractCtxtPtr,
                            XmlParserErrors::XmlSchemapWarnAttrPointlessProh,
                            Some(node.into()),
                            null_mut(),
                            format!("Skipping duplicate attribute use prohibition '{qname}'")
                                .as_str(),
                            Some(&qname),
                            None,
                            None,
                        );
                        return null_mut();
                    }
                }
            }
            // Create the attribute prohibition helper component.
            let prohib: XmlSchemaAttributeUseProhibPtr = xml_schema_add_attribute_use_prohib(pctxt);
            if prohib.is_null() {
                return null_mut();
            }
            (*prohib).node = node.into();
            (*prohib).name = tmp_name;
            (*prohib).target_namespace = tmp_ns;
            if is_ref != 0 {
                // We need at least to resolve to the attribute declaration.
                WXS_ADD_PENDING!(pctxt, prohib);
            }
            return prohib as XmlSchemaBasicItemPtr;
        }
        if is_schema(child, "annotation") {
            // TODO: Should this go into the attr decl?
            (*using).annot = (*pctxt).parse_annotation(child.unwrap(), 1);
            child = child
                .unwrap()
                .next
                .map(|node| XmlNodePtr::try_from(node).unwrap());
        }
        if is_ref != 0 {
            if let Some(child) = child {
                if is_schema(Some(child), "simpleType") {
                    // 3.2.3 : 3.2
                    // If ref is present, then all of <simpleType>,
                    // form and type must be absent.
                    xml_schema_pcontent_err(
                        pctxt,
                        XmlParserErrors::XmlSchemapSrcAttribute3_2,
                        null_mut(),
                        node,
                        Some(child.into()),
                        None,
                        Some("(annotation?)"),
                    );
                } else {
                    xml_schema_pcontent_err(
                        pctxt,
                        XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                        null_mut(),
                        node,
                        Some(child.into()),
                        None,
                        Some("(annotation?)"),
                    );
                }
            }
        } else {
            if is_schema(child, "simpleType") {
                if !(*WXS_ATTRUSE_DECL!(using)).type_name.is_null() {
                    // 3.2.3 : 4
                    // type and <simpleType> must not both be present.
                    xml_schema_pcontent_err(
                        pctxt,
                        XmlParserErrors::XmlSchemapSrcAttribute4,
                        null_mut(),
                        node,
                        child.map(|child| child.into()),
                        Some(
                            "The attribute 'type' and the <simpleType> child are mutually exclusive",
                        ),
                        None,
                    );
                } else {
                    WXS_ATTRUSE_TYPEDEF!(using) =
                        (*pctxt).parse_simple_type(schema, child.unwrap(), 0);
                }
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if let Some(child) = child {
                xml_schema_pcontent_err(
                    pctxt,
                    XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                    null_mut(),
                    node,
                    Some(child.into()),
                    None,
                    Some("(annotation?, simpleType?)"),
                );
            }
        }
        using as XmlSchemaBasicItemPtr
    }
}

/// Parse an attribute group definition reference.
/// Note that a reference to an attribute group does not
/// correspond to any component at all.
/// *WARNING* this interface is highly subject to change
///
/// Returns the attribute group or NULL in case of error.
#[doc(alias = "xmlSchemaParseAttributeGroupRef")]
unsafe fn xml_schema_parse_attribute_group_ref(
    pctxt: XmlSchemaParserCtxtPtr,
    schema: XmlSchemaPtr,
    node: XmlNodePtr,
) -> XmlSchemaQNameRefPtr {
    unsafe {
        let ret: XmlSchemaQNameRefPtr;
        let mut ref_ns: *const XmlChar = null();
        let mut refe: *const XmlChar = null();

        if pctxt.is_null() || schema.is_null() {
            return null_mut();
        }

        let Some(attr) = xml_schema_get_prop_node(node, "ref") else {
            xml_schema_pmissing_attr_err(
                pctxt,
                XmlParserErrors::XmlSchemapS4sAttrMissing,
                null_mut(),
                Some(node.into()),
                Some("ref"),
                None,
            );
            return null_mut();
        };
        xml_schema_pval_attr_node_qname(
            pctxt,
            schema,
            null_mut(),
            attr,
            &raw mut ref_ns,
            &raw mut refe,
        );
        if xml_schema_check_reference(pctxt, schema, node, Some(attr), ref_ns) != 0 {
            return null_mut();
        }

        // Check for illegal attributes.
        let mut attr = node.properties;
        while let Some(cur_attr) = attr {
            if let Some(ns) = cur_attr.ns {
                if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                    xml_schema_pillegal_attr_err(
                        pctxt,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
            } else if !xml_str_equal(cur_attr.name, c"ref".as_ptr() as _)
                && !xml_str_equal(cur_attr.name, c"id".as_ptr() as _)
            {
                xml_schema_pillegal_attr_err(
                    pctxt,
                    XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                    null_mut(),
                    cur_attr,
                );
            }
            attr = cur_attr.next;
        }
        // Attribute ID
        xml_schema_pval_attr_id(pctxt, node, "id");

        // And now for the children...
        let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
        if is_schema(child, "annotation") {
            // TODO: We do not have a place to store the annotation, do we?
            (*pctxt).parse_annotation(child.unwrap(), 0);
            child = child
                .unwrap()
                .next
                .map(|node| XmlNodePtr::try_from(node).unwrap());
        }
        if let Some(child) = child {
            xml_schema_pcontent_err(
                pctxt,
                XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                null_mut(),
                node,
                Some(child.into()),
                None,
                Some("(annotation?)"),
            );
        }

        // Handle attribute group redefinitions.
        if (*pctxt).is_redefine != 0
            && !(*pctxt).redef.is_null()
            && (*(*(*pctxt).redef).item).typ == XmlSchemaTypeType::XmlSchemaTypeAttributeGroup
            && refe == (*(*pctxt).redef).ref_name
            && ref_ns == (*(*pctxt).redef).ref_target_ns
        {
            // SPEC src-redefine:
            // (7.1) "If it has an <attributeGroup> among its contents
            // the `actual value` of whose ref [attribute] is the same
            // as the `actual value` of its own name attribute plus
            // target namespace, then it must have exactly one such group."
            if (*pctxt).redef_counter != 0 {
                let qname = xml_schema_format_qname(
                    Some(
                        CStr::from_ptr(ref_ns as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                    ),
                    Some(CStr::from_ptr(refe as *const i8).to_string_lossy().as_ref()),
                );

                xml_schema_custom_err(
                    pctxt as XmlSchemaAbstractCtxtPtr,
                    XmlParserErrors::XmlSchemapSrcRedefine,
                    Some(node.into()),
                    null_mut(),
                    format!("The redefining attribute group definition '{qname}' must not contain more than one reference to the redefined definition").as_str(),
                    Some(&qname),
                    None
                );
                return null_mut();
            }
            (*pctxt).redef_counter += 1;
            // URGENT TODO: How to ensure that the reference will not be
            // handled by the normal component resolution mechanism?
            ret = xml_schema_new_qname_ref(
                pctxt,
                XmlSchemaTypeType::XmlSchemaTypeAttributeGroup,
                refe,
                ref_ns,
            );
            if ret.is_null() {
                return null_mut();
            }
            (*ret).node = node.into();
            (*(*pctxt).redef).reference = ret as XmlSchemaBasicItemPtr;
        } else {
            // Create a QName-reference helper component. We will substitute this
            // component for the attribute uses of the referenced attribute group
            // definition.
            ret = xml_schema_new_qname_ref(
                pctxt,
                XmlSchemaTypeType::XmlSchemaTypeAttributeGroup,
                refe,
                ref_ns,
            );
            if ret.is_null() {
                return null_mut();
            }
            (*ret).node = node.into();
            // Add to pending items, to be able to resolve the reference.
            WXS_ADD_PENDING!(pctxt, ret);
        }
        ret
    }
}

/// Parses attribute uses and attribute declarations and attribute group references.
#[doc(alias = "xmlSchemaParseLocalAttributes")]
pub(crate) unsafe fn xml_schema_parse_local_attributes(
    ctxt: XmlSchemaParserCtxtPtr,
    schema: XmlSchemaPtr,
    child: &mut Option<XmlNodePtr>,
    list: *mut XmlSchemaItemListPtr<*mut c_void>,
    parent_type: i32,
    has_refs: *mut i32,
) -> i32 {
    unsafe {
        let mut item: *mut c_void;

        while is_schema(*child, "attribute") || is_schema(*child, "attributeGroup") {
            if is_schema(*child, "attribute") {
                item = xml_schema_parse_local_attribute(
                    ctxt,
                    schema,
                    child.unwrap(),
                    *list,
                    parent_type,
                ) as _;
            } else {
                item = xml_schema_parse_attribute_group_ref(ctxt, schema, child.unwrap()) as _;
                if !item.is_null() && !has_refs.is_null() {
                    *has_refs = 1;
                }
            }
            if !item.is_null() {
                if (*list).is_null() {
                    // TODO: Customize grow factor.
                    *list = xml_schema_item_list_create::<*mut c_void>();
                    if (*list).is_null() {
                        return -1;
                    }
                }
                if (**list).push(item) == -1 {
                    return -1;
                }
            }
            *child = child
                .unwrap()
                .next
                .map(|node| XmlNodePtr::try_from(node).unwrap());
        }
        0
    }
}

/// Adds a wildcard.
/// It corresponds to a xsd:anyAttribute and xsd:any.
///
/// Returns the new structure or NULL in case of error
#[doc(alias = "xmlSchemaAddWildcard")]
unsafe fn xml_schema_add_wildcard(
    ctxt: XmlSchemaParserCtxtPtr,
    schema: XmlSchemaPtr,
    typ: XmlSchemaTypeType,
    node: Option<XmlNodePtr>,
) -> XmlSchemaWildcardPtr {
    unsafe {
        let mut ret: XmlSchemaWildcardPtr;

        if ctxt.is_null() || schema.is_null() {
            return null_mut();
        }

        ret = xml_malloc(size_of::<XmlSchemaWildcard>()) as XmlSchemaWildcardPtr;
        if ret.is_null() {
            xml_schema_perr_memory(ctxt, "adding wildcard", None);
            return null_mut();
        }
        memset(ret as _, 0, size_of::<XmlSchemaWildcard>());
        (*ret).typ = typ;
        (*ret).node = node;
        WXS_ADD_LOCAL!(ctxt, ret);
        ret
    }
}

/// Creates a new wildcard namespace constraint.
///
/// Returns the new structure or NULL in case of error
#[doc(alias = "xmlSchemaNewWildcardNs")]
unsafe fn xml_schema_new_wildcard_ns_constraint(
    ctxt: XmlSchemaParserCtxtPtr,
) -> XmlSchemaWildcardNsPtr {
    unsafe {
        let ret: XmlSchemaWildcardNsPtr =
            xml_malloc(size_of::<XmlSchemaWildcardNs>()) as XmlSchemaWildcardNsPtr;
        if ret.is_null() {
            xml_schema_perr_memory(ctxt, "creating wildcard namespace constraint", None);
            return null_mut();
        }
        (*ret).value = null_mut();
        (*ret).next = null_mut();
        ret
    }
}

/// Parses the attribute "processContents" and "namespace"
/// of a xsd:anyAttribute and xsd:any.
/// *WARNING* this interface is highly subject to change
///
/// Returns 0 if everything goes fine, a positive error code
/// if something is not valid and -1 if an internal error occurs.
#[doc(alias = "xmlSchemaParseWildcardNs")]
unsafe fn xml_schema_parse_wildcard_ns(
    ctxt: XmlSchemaParserCtxtPtr,
    _schema: XmlSchemaPtr,
    wildc: XmlSchemaWildcardPtr,
    node: XmlNodePtr,
) -> i32 {
    unsafe {
        let mut dictns_item: *const XmlChar;
        let mut ret: i32 = 0;
        let mut tmp: XmlSchemaWildcardNsPtr;
        let mut last_ns: XmlSchemaWildcardNsPtr = null_mut();

        let pc = (*ctxt).get_prop(node, "processContents");
        if pc.is_none() || pc.as_deref() == Some("strict") {
            (*wildc).process_contents = XML_SCHEMAS_ANY_STRICT;
        } else if pc.as_deref() == Some("skip") {
            (*wildc).process_contents = XML_SCHEMAS_ANY_SKIP;
        } else if pc.as_deref() == Some("lax") {
            (*wildc).process_contents = XML_SCHEMAS_ANY_LAX;
        } else {
            let pc = pc.unwrap();
            xml_schema_psimple_type_err(
                ctxt,
                XmlParserErrors::XmlSchemapS4sAttrInvalidValue,
                null_mut(),
                node.into(),
                null_mut(),
                Some("(strict | skip | lax)"),
                Some(&pc),
                None,
                None,
                None,
            );
            (*wildc).process_contents = XML_SCHEMAS_ANY_STRICT;
            ret = XmlParserErrors::XmlSchemapS4sAttrInvalidValue as i32;
        }
        // Build the namespace constraints.
        let attr = xml_schema_get_prop_node(node, "namespace");
        let ns = (*ctxt).get_node_content(attr.map(|attr| attr.into()));
        if let Some(attr) = attr.filter(|_| ns != "##any") {
            if ns == "##other" {
                (*wildc).neg_ns_set = xml_schema_new_wildcard_ns_constraint(ctxt);
                if (*wildc).neg_ns_set.is_null() {
                    return -1;
                }
                (*(*wildc).neg_ns_set).value = (*ctxt).target_namespace;
            } else {
                let mut cur = ns.as_str();
                while !cur.is_empty() {
                    cur = cur.trim_start_matches(|c| xml_is_blank_char(c as u32));
                    let end = cur.trim_start_matches(|c| !xml_is_blank_char(c as u32));
                    if end.len() == cur.len() {
                        break;
                    }
                    let ns_item = &cur[..cur.len() - end.len()];
                    if ns_item == "##other" || ns_item == "##any" {
                        xml_schema_psimple_type_err(
                            ctxt,
                            XmlParserErrors::XmlSchemapWildcardInvalidNsMember,
                            null_mut(),
                            attr.into(),
                            null_mut(),
                            Some(
                                "((##any | ##other) | List of (xs:anyURI | (##targetNamespace | ##local)))",
                            ),
                            Some(ns_item),
                            None,
                            None,
                            None,
                        );
                        ret = XmlParserErrors::XmlSchemapWildcardInvalidNsMember as i32;
                    } else {
                        if ns_item == "##targetNamespace" {
                            dictns_item = (*ctxt).target_namespace;
                        } else if ns_item == "##local" {
                            dictns_item = null_mut();
                        } else {
                            // Validate the item (anyURI).
                            dictns_item = xml_dict_lookup(
                                (*ctxt).dict,
                                ns_item.as_ptr(),
                                ns_item.len() as i32,
                            );
                            xml_schema_pval_attr_node_value(
                                ctxt,
                                null_mut(),
                                attr,
                                dictns_item,
                                xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasAnyURI),
                            );
                        }
                        // Avoid duplicate namespaces.
                        tmp = (*wildc).ns_set;
                        while !tmp.is_null() {
                            if dictns_item == (*tmp).value {
                                break;
                            }
                            tmp = (*tmp).next;
                        }
                        if tmp.is_null() {
                            tmp = xml_schema_new_wildcard_ns_constraint(ctxt);
                            if tmp.is_null() {
                                return -1;
                            }
                            (*tmp).value = dictns_item;
                            (*tmp).next = null_mut();
                            if (*wildc).ns_set.is_null() {
                                (*wildc).ns_set = tmp;
                            } else if !last_ns.is_null() {
                                (*last_ns).next = tmp;
                            }
                            last_ns = tmp;
                        }
                    }
                    cur = end;
                }
            }
        } else {
            (*wildc).any = 1;
        }

        ret
    }
}

/// parse a XML schema AnyAttribute declaration
/// *WARNING* this interface is highly subject to change
///
/// Returns a wildcard or NULL.
#[doc(alias = "xmlSchemaParseAnyAttribute")]
pub(crate) unsafe fn xml_schema_parse_any_attribute(
    ctxt: XmlSchemaParserCtxtPtr,
    schema: XmlSchemaPtr,
    node: XmlNodePtr,
) -> XmlSchemaWildcardPtr {
    unsafe {
        if ctxt.is_null() || schema.is_null() {
            return null_mut();
        }

        let ret: XmlSchemaWildcardPtr = xml_schema_add_wildcard(
            ctxt,
            schema,
            XmlSchemaTypeType::XmlSchemaTypeAnyAttribute,
            Some(node),
        );
        if ret.is_null() {
            return null_mut();
        }
        // Check for illegal attributes.
        let mut attr = node.properties;
        while let Some(cur_attr) = attr {
            if let Some(ns) = cur_attr.ns {
                if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                    xml_schema_pillegal_attr_err(
                        ctxt,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
            } else if !xml_str_equal(cur_attr.name, c"id".as_ptr() as _)
                && !xml_str_equal(cur_attr.name, c"namespace".as_ptr() as _)
                && !xml_str_equal(cur_attr.name, c"processContents".as_ptr() as _)
            {
                xml_schema_pillegal_attr_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                    null_mut(),
                    cur_attr,
                );
            }
            attr = cur_attr.next;
        }
        xml_schema_pval_attr_id(ctxt, node, "id");
        // Parse the namespace list.
        if xml_schema_parse_wildcard_ns(ctxt, schema, ret, node) != 0 {
            return null_mut();
        }
        // And now for the children...
        let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
        if is_schema(child, "annotation") {
            (*ret).annot = (*ctxt).parse_annotation(child.unwrap(), 1);
            child = child
                .unwrap()
                .next
                .map(|node| XmlNodePtr::try_from(node).unwrap());
        }
        if let Some(child) = child {
            xml_schema_pcontent_err(
                ctxt,
                XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                null_mut(),
                node,
                Some(child.into()),
                None,
                Some("(annotation?)"),
            );
        }

        ret
    }
}

/// Parses an <extension>, which is found inside a
/// <simpleContent> or <complexContent>.
/// *WARNING* this interface is highly subject to change.
///
/// TODO: Returns the type definition or NULL in case of error
#[doc(alias = "xmlSchemaParseExtension")]
unsafe fn xml_schema_parse_extension(
    ctxt: XmlSchemaParserCtxtPtr,
    schema: XmlSchemaPtr,
    node: XmlNodePtr,
    parent_type: XmlSchemaTypeType,
) -> XmlSchemaTypePtr {
    unsafe {
        if ctxt.is_null() || schema.is_null() {
            return null_mut();
        }
        // Not a component, don't create it.
        let typ: XmlSchemaTypePtr = (*ctxt).ctxt_type;
        (*typ).flags |= XML_SCHEMAS_TYPE_DERIVATION_METHOD_EXTENSION;

        // Check for illegal attributes.
        let mut attr = node.properties;
        while let Some(cur_attr) = attr {
            if let Some(ns) = cur_attr.ns {
                if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                    xml_schema_pillegal_attr_err(
                        ctxt,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
            } else if !xml_str_equal(cur_attr.name, c"id".as_ptr() as _)
                && !xml_str_equal(cur_attr.name, c"base".as_ptr() as _)
            {
                xml_schema_pillegal_attr_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                    null_mut(),
                    cur_attr,
                );
            }
            attr = cur_attr.next;
        }

        xml_schema_pval_attr_id(ctxt, node, "id");

        // Attribute "base" - mandatory.
        if xml_schema_pval_attr_qname(
            ctxt,
            schema,
            null_mut(),
            node,
            "base",
            &raw mut (*typ).base_ns,
            &raw mut (*typ).base,
        ) == 0
            && (*typ).base.is_null()
        {
            xml_schema_pmissing_attr_err(
                ctxt,
                XmlParserErrors::XmlSchemapS4sAttrMissing,
                null_mut(),
                Some(node.into()),
                Some("base"),
                None,
            );
        }
        // And now for the children...
        let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
        if is_schema(child, "annotation") {
            // Add the annotation to the type ancestor.
            xml_schema_add_annotation(
                typ as XmlSchemaAnnotItemPtr,
                (*ctxt).parse_annotation(child.unwrap(), 1),
            );
            child = child
                .unwrap()
                .next
                .map(|node| XmlNodePtr::try_from(node).unwrap());
        }
        if parent_type == XmlSchemaTypeType::XmlSchemaTypeComplexContent {
            // Corresponds to <complexType><complexContent><extension>... and:
            //
            // Model groups <all>, <choice>, <sequence> and <group>.
            if is_schema(child, "all") {
                (*typ).subtypes = xml_schema_parse_model_group(
                    ctxt,
                    schema,
                    child.unwrap(),
                    XmlSchemaTypeType::XmlSchemaTypeAll,
                    1,
                ) as XmlSchemaTypePtr;
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            } else if is_schema(child, "choice") {
                (*typ).subtypes = xml_schema_parse_model_group(
                    ctxt,
                    schema,
                    child.unwrap(),
                    XmlSchemaTypeType::XmlSchemaTypeChoice,
                    1,
                ) as XmlSchemaTypePtr;
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            } else if is_schema(child, "sequence") {
                (*typ).subtypes = xml_schema_parse_model_group(
                    ctxt,
                    schema,
                    child.unwrap(),
                    XmlSchemaTypeType::XmlSchemaTypeSequence,
                    1,
                ) as XmlSchemaTypePtr;
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            } else if is_schema(child, "group") {
                (*typ).subtypes = xml_schema_parse_model_group_def_ref(ctxt, schema, child.unwrap())
                    as XmlSchemaTypePtr;
                // Note that the reference will be resolved in
                // xmlSchemaResolveTypeReferences();
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
        }
        if child.is_some() {
            // Attribute uses/declarations.
            if xml_schema_parse_local_attributes(
                ctxt,
                schema,
                &mut child,
                &raw mut (*typ).attr_uses as *mut XmlSchemaItemListPtr<*mut c_void>,
                XmlSchemaTypeType::XmlSchemaTypeExtension as i32,
                null_mut(),
            ) == -1
            {
                return null_mut();
            }
            // Attribute wildcard.
            if is_schema(child, "anyAttribute") {
                (*(*ctxt).ctxt_type).attribute_wildcard =
                    xml_schema_parse_any_attribute(ctxt, schema, child.unwrap());
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
        }
        if let Some(child) = child {
            if parent_type == XmlSchemaTypeType::XmlSchemaTypeComplexContent {
                // Complex content extension.
                xml_schema_pcontent_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                    null_mut(),
                    node,
                    Some(child.into()),
                    None,
                    Some(
                        "(annotation?, ((group | all | choice | sequence)?, ((attribute | attributeGroup)*, anyAttribute?)))",
                    ),
                );
            } else {
                // Simple content extension.
                xml_schema_pcontent_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                    null_mut(),
                    node,
                    Some(child.into()),
                    None,
                    Some("(annotation?, ((attribute | attributeGroup)*, anyAttribute?))"),
                );
            }
        }
        null_mut()
    }
}

/// Parse a XML schema SimpleContent definition
/// *WARNING* this interface is highly subject to change
///
/// Returns the type definition or NULL in case of error
#[doc(alias = "xmlSchemaParseSimpleContent")]
pub(crate) unsafe fn xml_schema_parse_simple_content(
    ctxt: XmlSchemaParserCtxtPtr,
    schema: XmlSchemaPtr,
    node: XmlNodePtr,
    has_restriction_or_extension: *mut i32,
) -> i32 {
    unsafe {
        if ctxt.is_null() || schema.is_null() || has_restriction_or_extension.is_null() {
            return -1;
        }
        *has_restriction_or_extension = 0;
        // Not a component, don't create it.
        let typ: XmlSchemaTypePtr = (*ctxt).ctxt_type;
        (*typ).content_type = XmlSchemaContentType::XmlSchemaContentSimple;
        // Check for illegal attributes.
        let mut attr = node.properties;
        while let Some(cur_attr) = attr {
            if let Some(ns) = cur_attr.ns {
                if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                    xml_schema_pillegal_attr_err(
                        ctxt,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
            } else if !xml_str_equal(cur_attr.name, c"id".as_ptr() as _) {
                xml_schema_pillegal_attr_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                    null_mut(),
                    cur_attr,
                );
            }
            attr = cur_attr.next;
        }

        xml_schema_pval_attr_id(ctxt, node, "id");

        // And now for the children...
        let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
        if is_schema(child, "annotation") {
            // Add the annotation to the complex type ancestor.
            xml_schema_add_annotation(
                typ as XmlSchemaAnnotItemPtr,
                (*ctxt).parse_annotation(child.unwrap(), 1),
            );
            child = child
                .unwrap()
                .next
                .map(|node| XmlNodePtr::try_from(node).unwrap());
        }
        if child.is_none() {
            xml_schema_pcontent_err(
                ctxt,
                XmlParserErrors::XmlSchemapS4sElemMissing,
                null_mut(),
                node,
                None,
                None,
                Some("(annotation?, (restriction | extension))"),
            );
        }
        if child.is_none() {
            xml_schema_pcontent_err(
                ctxt,
                XmlParserErrors::XmlSchemapS4sElemMissing,
                null_mut(),
                node,
                None,
                None,
                Some("(annotation?, (restriction | extension))"),
            );
        }
        if is_schema(child, "restriction") {
            xml_schema_parse_restriction(
                ctxt,
                schema,
                child.unwrap(),
                XmlSchemaTypeType::XmlSchemaTypeSimpleContent,
            );
            *has_restriction_or_extension = 1;
            child = child
                .unwrap()
                .next
                .map(|node| XmlNodePtr::try_from(node).unwrap());
        } else if is_schema(child, "extension") {
            xml_schema_parse_extension(
                ctxt,
                schema,
                child.unwrap(),
                XmlSchemaTypeType::XmlSchemaTypeSimpleContent,
            );
            *has_restriction_or_extension = 1;
            child = child
                .unwrap()
                .next
                .map(|node| XmlNodePtr::try_from(node).unwrap());
        }
        if let Some(child) = child {
            xml_schema_pcontent_err(
                ctxt,
                XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                null_mut(),
                node,
                Some(child.into()),
                None,
                Some("(annotation?, (restriction | extension))"),
            );
        }
        0
    }
}

/// Parse a XML schema ComplexContent definition
/// *WARNING* this interface is highly subject to change
///
/// Returns the type definition or NULL in case of error
#[doc(alias = "xmlSchemaParseComplexContent")]
pub(crate) unsafe fn xml_schema_parse_complex_content(
    ctxt: XmlSchemaParserCtxtPtr,
    schema: XmlSchemaPtr,
    node: XmlNodePtr,
    has_restriction_or_extension: *mut i32,
) -> i32 {
    unsafe {
        if ctxt.is_null() || schema.is_null() || has_restriction_or_extension.is_null() {
            return -1;
        }
        *has_restriction_or_extension = 0;
        // Not a component, don't create it.
        let typ: XmlSchemaTypePtr = (*ctxt).ctxt_type;
        // Check for illegal attributes.
        let mut attr = node.properties;
        while let Some(cur_attr) = attr {
            if let Some(ns) = cur_attr.ns {
                if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                    xml_schema_pillegal_attr_err(
                        ctxt,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
            } else if !xml_str_equal(cur_attr.name, c"id".as_ptr() as _)
                && !xml_str_equal(cur_attr.name, c"mixed".as_ptr() as _)
            {
                xml_schema_pillegal_attr_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                    null_mut(),
                    cur_attr,
                );
            }
            attr = cur_attr.next;
        }

        xml_schema_pval_attr_id(ctxt, node, "id");

        // Set the 'mixed' on the complex type ancestor.
        if xml_get_boolean_prop(ctxt, node, "mixed", 0) != 0
            && (*typ).flags & XML_SCHEMAS_TYPE_MIXED == 0
        {
            (*typ).flags |= XML_SCHEMAS_TYPE_MIXED;
        }
        let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
        if is_schema(child, "annotation") {
            // Add the annotation to the complex type ancestor.
            xml_schema_add_annotation(
                typ as XmlSchemaAnnotItemPtr,
                (*ctxt).parse_annotation(child.unwrap(), 1),
            );
            child = child
                .unwrap()
                .next
                .map(|node| XmlNodePtr::try_from(node).unwrap());
        }
        if child.is_none() {
            xml_schema_pcontent_err(
                ctxt,
                XmlParserErrors::XmlSchemapS4sElemMissing,
                null_mut(),
                node,
                None,
                None,
                Some("(annotation?, (restriction | extension))"),
            );
        }
        if child.is_none() {
            xml_schema_pcontent_err(
                ctxt,
                XmlParserErrors::XmlSchemapS4sElemMissing,
                null_mut(),
                node,
                None,
                None,
                Some("(annotation?, (restriction | extension))"),
            );
        }
        if is_schema(child, "restriction") {
            xml_schema_parse_restriction(
                ctxt,
                schema,
                child.unwrap(),
                XmlSchemaTypeType::XmlSchemaTypeComplexContent,
            );
            *has_restriction_or_extension = 1;
            child = child
                .unwrap()
                .next
                .map(|node| XmlNodePtr::try_from(node).unwrap());
        } else if is_schema(child, "extension") {
            xml_schema_parse_extension(
                ctxt,
                schema,
                child.unwrap(),
                XmlSchemaTypeType::XmlSchemaTypeComplexContent,
            );
            *has_restriction_or_extension = 1;
            child = child
                .unwrap()
                .next
                .map(|node| XmlNodePtr::try_from(node).unwrap());
        }
        if let Some(child) = child {
            xml_schema_pcontent_err(
                ctxt,
                XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                null_mut(),
                node,
                Some(child.into()),
                None,
                Some("(annotation?, (restriction | extension))"),
            );
        }
        0
    }
}

/// Evaluate if a boolean property is set
///
/// Returns the default if not found, 0 if found to be false, 1 if found to be true
#[doc(alias = "xmlGetBooleanProp")]
pub(crate) unsafe fn xml_get_boolean_prop(
    ctxt: XmlSchemaParserCtxtPtr,
    node: XmlNodePtr,
    name: &str,
    mut def: i32,
) -> i32 {
    unsafe {
        let Some(val) = (*ctxt).get_prop(node, name) else {
            return def;
        };
        // 3.2.2.1 Lexical representation
        // An instance of a datatype that is defined as `boolean`
        // can have the following legal literals {true, false, 1, 0}.
        if val == "true" {
            def = 1;
        } else if val == "false" {
            def = 0;
        } else if val == "1" {
            def = 1;
        } else if val == "0" {
            def = 0;
        } else {
            xml_schema_psimple_type_err(
                ctxt,
                XmlParserErrors::XmlSchemapInvalidBoolean,
                null_mut(),
                xml_schema_get_prop_node(node, name).unwrap().into(),
                xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasBoolean) as _,
                None,
                Some(&val),
                None,
                None,
                None,
            );
        }
        def
    }
}

unsafe fn xml_schema_add_idc(
    ctxt: XmlSchemaParserCtxtPtr,
    schema: XmlSchemaPtr,
    name: *const XmlChar,
    ns_name: *const XmlChar,
    category: XmlSchemaTypeType,
    node: XmlNodePtr,
) -> XmlSchemaIDCPtr {
    unsafe {
        let mut ret: XmlSchemaIDCPtr;

        if ctxt.is_null() || schema.is_null() || name.is_null() {
            return null_mut();
        }

        ret = xml_malloc(size_of::<XmlSchemaIDC>()) as XmlSchemaIDCPtr;
        if ret.is_null() {
            xml_schema_perr_memory(ctxt, "allocating an identity-constraint definition", None);
            return null_mut();
        }
        std::ptr::write(
            &mut *ret,
            XmlSchemaIDC {
                typ: category,
                annot: null_mut(),
                next: null_mut(),
                node,
                name,
                target_namespace: ns_name,
                selector: null_mut(),
                fields: null_mut(),
                nb_fields: 0,
                refe: null_mut(),
            },
        );

        WXS_ADD_GLOBAL!(ctxt, ret);
        // Only keyrefs need to be fixup up.
        if category == XmlSchemaTypeType::XmlSchemaTypeIDCKeyref {
            WXS_ADD_PENDING!(ctxt, ret);
        }
        ret
    }
}

unsafe fn xml_schema_check_cselector_xpath(
    ctxt: XmlSchemaParserCtxtPtr,
    idc: XmlSchemaIDCPtr,
    selector: XmlSchemaIdcselectPtr,
    attr: Option<XmlAttrPtr>,
    is_field: i32,
) -> i32 {
    unsafe {
        // c-selector-xpath:
        // Schema Component Constraint: Selector Value OK
        //
        // TODO: 1 The {selector} must be a valid XPath expression, as defined
        // in [XPath].
        if selector.is_null() {
            xml_schema_perr(
                ctxt,
                Some(XmlGenericNodePtr::from((*idc).node)),
                XmlParserErrors::XmlSchemapInternal,
                "Internal error: xmlSchemaCheckCSelectorXPath, the selector is not specified.\n",
                None,
                None,
            );
            return -1;
        }
        let node = attr.map_or(Some(XmlGenericNodePtr::from((*idc).node)), |attr| {
            Some(attr.into())
        });
        if (*selector).xpath.is_null() {
            // TODO: Adjust error code.
            xml_schema_pcustom_err(
                ctxt,
                XmlParserErrors::XmlSchemapS4sAttrInvalidValue,
                null_mut(),
                node,
                "The XPath expression of the selector is not valid",
                None,
            );
            return XmlParserErrors::XmlSchemapS4sAttrInvalidValue as i32;
        } else {
            // Compile the XPath expression.
            // TODO: We need the array of in-scope namespaces for compilation.
            // TODO: Call xmlPatterncompile with different options for selector/field.
            let ns_list = attr.and_then(|attr| attr.parent().unwrap().get_ns_list(attr.doc));
            // Build an array of prefixes and namespaces.
            let mut ns_array = None;
            if let Some(ns_list) = ns_list {
                let count: usize = ns_list.len();
                let ns_array = ns_array.get_or_insert_with(|| vec![(null(), null()); count]);
                for (i, cur) in ns_list.into_iter().enumerate() {
                    ns_array[i] = (cur.href, cur.prefix);
                }
            }
            // TODO: Differentiate between "selector" and "field".
            if is_field != 0 {
                (*selector).xpath_comp = xml_patterncompile(
                    (*selector).xpath,
                    XmlPatternFlags::XmlPatternXsfield as i32,
                    ns_array,
                ) as _;
            } else {
                (*selector).xpath_comp = xml_patterncompile(
                    (*selector).xpath,
                    XmlPatternFlags::XmlPatternXssel as i32,
                    ns_array,
                ) as _;
            }

            if (*selector).xpath_comp.is_null() {
                let xpath = CStr::from_ptr((*selector).xpath as *const i8).to_string_lossy();
                // TODO: Adjust error code?
                xml_schema_pcustom_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapS4sAttrInvalidValue,
                    null_mut(),
                    node,
                    format!("The XPath expression '{xpath}' could not be compiled").as_str(),
                    Some(&xpath),
                );
                return XmlParserErrors::XmlSchemapS4sAttrInvalidValue as i32;
            }
        }
        0
    }
}

/// Parses a XML Schema identity-constraint definition's
/// <selector> and <field> elements.
///
/// Returns the parsed identity-constraint definition.
#[doc(alias = "xmlSchemaParseIDCSelectorAndField")]
unsafe fn xml_schema_parse_idcselector_and_field(
    ctxt: XmlSchemaParserCtxtPtr,
    idc: XmlSchemaIDCPtr,
    node: XmlNodePtr,
    is_field: i32,
) -> XmlSchemaIdcselectPtr {
    unsafe {
        // Check for illegal attributes.
        let mut attr = node.properties;
        while let Some(cur_attr) = attr {
            if let Some(ns) = cur_attr.ns {
                if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                    xml_schema_pillegal_attr_err(
                        ctxt,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
            } else if !xml_str_equal(cur_attr.name, c"id".as_ptr() as _)
                && !xml_str_equal(cur_attr.name, c"xpath".as_ptr() as _)
            {
                xml_schema_pillegal_attr_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                    null_mut(),
                    cur_attr,
                );
            }
            attr = cur_attr.next;
        }
        // Create the item.
        let item: XmlSchemaIdcselectPtr =
            xml_malloc(size_of::<XmlSchemaIdcselect>()) as XmlSchemaIdcselectPtr;
        if item.is_null() {
            xml_schema_perr_memory(
                ctxt,
                "allocating a 'selector' of an identity-constraint definition",
                None,
            );
            return null_mut();
        }
        memset(item as _, 0, size_of::<XmlSchemaIdcselect>());
        // Attribute "xpath" (mandatory).
        if let Some(attr) = xml_schema_get_prop_node(node, "xpath") {
            let xpath = (*ctxt).get_node_content(Some(attr.into()));
            (*item).xpath = xml_dict_lookup((*ctxt).dict, xpath.as_ptr(), xpath.len() as i32);
            // URGENT TODO: "field"s have an other syntax than "selector"s.

            if xml_schema_check_cselector_xpath(ctxt, idc, item, Some(attr), is_field) == -1 {
                xml_schema_perr(
                    ctxt,
                    Some(attr.into()),
                    XmlParserErrors::XmlSchemapInternal,
                    "Internal error: xmlSchemaParseIDCSelectorAndField, validating the XPath expression of a IDC selector.\n",
                    None,
                    None,
                );
            }
        } else {
            xml_schema_pmissing_attr_err(
                ctxt,
                XmlParserErrors::XmlSchemapS4sAttrMissing,
                null_mut(),
                Some(node.into()),
                Some("name"),
                None,
            );
        }
        xml_schema_pval_attr_id(ctxt, node, "id");
        // And now for the children...
        let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
        if is_schema(child, "annotation") {
            // Add the annotation to the parent IDC.
            xml_schema_add_annotation(
                idc as XmlSchemaAnnotItemPtr,
                (*ctxt).parse_annotation(child.unwrap(), 1),
            );
            child = child
                .unwrap()
                .next
                .map(|node| XmlNodePtr::try_from(node).unwrap());
        }
        if let Some(child) = child {
            xml_schema_pcontent_err(
                ctxt,
                XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                null_mut(),
                node,
                Some(child.into()),
                None,
                Some("(annotation?)"),
            );
        }

        item
    }
}

/// Parses a XML Schema identity-constraint definition.
///
/// Returns the parsed identity-constraint definition.
#[doc(alias = "xmlSchemaParseIDC")]
pub(crate) unsafe fn xml_schema_parse_idc(
    ctxt: XmlSchemaParserCtxtPtr,
    schema: XmlSchemaPtr,
    node: XmlNodePtr,
    idc_category: XmlSchemaTypeType,
    target_namespace: *const XmlChar,
) -> XmlSchemaIDCPtr {
    unsafe {
        let mut name: *const XmlChar = null();
        let mut field: XmlSchemaIdcselectPtr;
        let mut last_field: XmlSchemaIdcselectPtr = null_mut();

        // Check for illegal attributes.
        let mut attr = node.properties;
        while let Some(cur_attr) = attr {
            if let Some(ns) = cur_attr.ns {
                if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                    xml_schema_pillegal_attr_err(
                        ctxt,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
            } else if !xml_str_equal(cur_attr.name, c"id".as_ptr() as _)
                && !xml_str_equal(cur_attr.name, c"name".as_ptr() as _)
                && (idc_category != XmlSchemaTypeType::XmlSchemaTypeIDCKeyref
                    || !xml_str_equal(cur_attr.name, c"refer".as_ptr() as _))
            {
                xml_schema_pillegal_attr_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                    null_mut(),
                    cur_attr,
                );
            }
            attr = cur_attr.next;
        }
        // Attribute "name" (mandatory).
        let Some(attr) = xml_schema_get_prop_node(node, "name") else {
            xml_schema_pmissing_attr_err(
                ctxt,
                XmlParserErrors::XmlSchemapS4sAttrMissing,
                null_mut(),
                Some(node.into()),
                Some("name"),
                None,
            );
            return null_mut();
        };
        if xml_schema_pval_attr_node(
            ctxt,
            null_mut(),
            attr,
            xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasNCName),
            &raw mut name,
        ) != 0
        {
            return null_mut();
        }
        // Create the component.
        let item: XmlSchemaIDCPtr =
            xml_schema_add_idc(ctxt, schema, name, target_namespace, idc_category, node);
        if item.is_null() {
            return null_mut();
        }

        xml_schema_pval_attr_id(ctxt, node, "id");
        if idc_category == XmlSchemaTypeType::XmlSchemaTypeIDCKeyref {
            // Attribute "refer" (mandatory).
            if let Some(attr) = xml_schema_get_prop_node(node, "refer") {
                // Create a reference item.
                (*item).refe = xml_schema_new_qname_ref(
                    ctxt,
                    XmlSchemaTypeType::XmlSchemaTypeIDCKey,
                    null_mut(),
                    null_mut(),
                );
                if (*item).refe.is_null() {
                    return null_mut();
                }
                xml_schema_pval_attr_node_qname(
                    ctxt,
                    schema,
                    null_mut(),
                    attr,
                    &raw mut (*(*item).refe).target_namespace,
                    &raw mut (*(*item).refe).name,
                );
                xml_schema_check_reference(
                    ctxt,
                    schema,
                    node,
                    Some(attr),
                    (*(*item).refe).target_namespace,
                );
            } else {
                xml_schema_pmissing_attr_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapS4sAttrMissing,
                    null_mut(),
                    Some(node.into()),
                    Some("refer"),
                    None,
                );
            }
        }
        // And now for the children...
        let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
        if is_schema(child, "annotation") {
            (*item).annot = (*ctxt).parse_annotation(child.unwrap(), 1);
            child = child
                .unwrap()
                .next
                .map(|node| XmlNodePtr::try_from(node).unwrap());
        }
        if child.is_none() {
            xml_schema_pcontent_err(
                ctxt,
                XmlParserErrors::XmlSchemapS4sElemMissing,
                null_mut(),
                node,
                None,
                Some("A child element is missing"),
                Some("(annotation?, (selector, field+))"),
            );
        }
        // Child element <selector>.
        if is_schema(child, "selector") {
            (*item).selector =
                xml_schema_parse_idcselector_and_field(ctxt, item, child.unwrap(), 0);
            child = child
                .unwrap()
                .next
                .map(|node| XmlNodePtr::try_from(node).unwrap());
            // Child elements <field>.
            if is_schema(child, "field") {
                while {
                    field = xml_schema_parse_idcselector_and_field(ctxt, item, child.unwrap(), 1);
                    if !field.is_null() {
                        (*field).index = (*item).nb_fields;
                        (*item).nb_fields += 1;
                        if !last_field.is_null() {
                            (*last_field).next = field;
                        } else {
                            (*item).fields = field;
                        }
                        last_field = field;
                    }
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());

                    is_schema(child, "field")
                } {}
            } else {
                xml_schema_pcontent_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                    null_mut(),
                    node,
                    child.map(|child| child.into()),
                    None,
                    Some("(annotation?, (selector, field+))"),
                );
            }
        }
        if let Some(child) = child {
            xml_schema_pcontent_err(
                ctxt,
                XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                null_mut(),
                node,
                Some(child.into()),
                None,
                Some("(annotation?, (selector, field+))"),
            );
        }

        item
    }
}

unsafe fn xml_schema_get_qname_ref_name(refe: *mut c_void) -> *const XmlChar {
    unsafe { (*(refe as XmlSchemaQNameRefPtr)).name }
}

unsafe fn xml_schema_get_qname_ref_target_ns(refe: *mut c_void) -> *const XmlChar {
    unsafe { (*(refe as XmlSchemaQNameRefPtr)).target_namespace }
}

/// Parsea a XML schema <any> element. A particle and wildcard
/// will be created (except if minOccurs==maxOccurs==0, in this case
/// nothing will be created).
/// *WARNING* this interface is highly subject to change
///
/// Returns the particle or NULL in case of error or if minOccurs==maxOccurs==0
#[doc(alias = "xmlSchemaParseAny")]
unsafe fn xml_schema_parse_any(
    ctxt: XmlSchemaParserCtxtPtr,
    schema: XmlSchemaPtr,
    node: XmlNodePtr,
) -> XmlSchemaParticlePtr {
    unsafe {
        let mut annot: XmlSchemaAnnotPtr = null_mut();

        if ctxt.is_null() || schema.is_null() {
            return null_mut();
        }
        // Check for illegal attributes.
        let mut attr = node.properties;
        while let Some(cur_attr) = attr {
            if let Some(ns) = cur_attr.ns {
                if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                    xml_schema_pillegal_attr_err(
                        ctxt,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
            } else if !xml_str_equal(cur_attr.name, c"id".as_ptr() as _)
                && !xml_str_equal(cur_attr.name, c"minOccurs".as_ptr() as _)
                && !xml_str_equal(cur_attr.name, c"maxOccurs".as_ptr() as _)
                && !xml_str_equal(cur_attr.name, c"namespace".as_ptr() as _)
                && !xml_str_equal(cur_attr.name, c"processContents".as_ptr() as _)
            {
                xml_schema_pillegal_attr_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                    null_mut(),
                    cur_attr,
                );
            }
            attr = cur_attr.next;
        }
        xml_schema_pval_attr_id(ctxt, node, "id");
        // minOccurs/maxOccurs.
        let max: i32 = xml_get_max_occurs(
            ctxt,
            node,
            0,
            UNBOUNDED as _,
            1,
            "(xs:nonNegativeInteger | unbounded)",
        );
        let min: i32 = xml_get_min_occurs(ctxt, node, 0, -1, 1, "xs:nonNegativeInteger");
        xml_schema_pcheck_particle_correct_2(ctxt, null_mut(), node, min, max);
        // Create & parse the wildcard.
        let wild: XmlSchemaWildcardPtr = xml_schema_add_wildcard(
            ctxt,
            schema,
            XmlSchemaTypeType::XmlSchemaTypeAny,
            Some(node),
        );
        if wild.is_null() {
            return null_mut();
        }
        xml_schema_parse_wildcard_ns(ctxt, schema, wild, node);
        // And now for the children...
        let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
        if is_schema(child, "annotation") {
            annot = (*ctxt).parse_annotation(child.unwrap(), 1);
            child = child
                .unwrap()
                .next
                .map(|node| XmlNodePtr::try_from(node).unwrap());
        }
        if let Some(child) = child {
            xml_schema_pcontent_err(
                ctxt,
                XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                null_mut(),
                node,
                Some(child.into()),
                None,
                Some("(annotation?)"),
            );
        }
        // No component if minOccurs==maxOccurs==0.
        if min == 0 && max == 0 {
            // Don't free the wildcard, since it's already on the list.
            return null_mut();
        }
        // Create the particle.
        let particle: XmlSchemaParticlePtr = xml_schema_add_particle(ctxt, Some(node), min, max);
        if particle.is_null() {
            return null_mut();
        }
        (*particle).annot = annot;
        (*particle).children = wild as XmlSchemaTreeItemPtr;

        particle
    }
}

/// Parse a XML schema Sequence definition.
/// Applies parts of:
///   Schema Representation Constraint:
///     Redefinition Constraints and Semantics (src-redefine)
///     (6.1), (6.1.1), (6.1.2)
///
///   Schema Component Constraint:
///     All Group Limited (cos-all-limited) (2)
///     TODO: Actually this should go to component-level checks,
///     but is done here due to performance. Move it to an other layer
///     is schema construction via an API is implemented.
///
/// *WARNING* this interface is highly subject to change
///
/// Returns -1 in case of error, 0 if the declaration is improper and
///         1 in case of success.
#[doc(alias = "xmlSchemaParseModelGroup")]
pub(crate) unsafe fn xml_schema_parse_model_group(
    ctxt: XmlSchemaParserCtxtPtr,
    schema: XmlSchemaPtr,
    node: XmlNodePtr,
    typ: XmlSchemaTypeType,
    with_particle: i32,
) -> XmlSchemaTreeItemPtr {
    unsafe {
        let mut particle: XmlSchemaParticlePtr = null_mut();
        let mut min: i32 = 1;
        let mut max: i32 = 1;
        let mut is_elem_ref: i32 = 0;
        let mut has_refs: i32 = 0;

        if ctxt.is_null() || schema.is_null() {
            return null_mut();
        }
        // Create a model group with the given compositor.
        let item: XmlSchemaModelGroupPtr = xml_schema_add_model_group(ctxt, schema, typ, node);
        if item.is_null() {
            return null_mut();
        }

        if with_particle != 0 {
            if typ == XmlSchemaTypeType::XmlSchemaTypeAll {
                min = xml_get_min_occurs(ctxt, node, 0, 1, 1, "(0 | 1)");
                max = xml_get_max_occurs(ctxt, node, 1, 1, 1, "1");
            } else {
                // choice + sequence
                min = xml_get_min_occurs(ctxt, node, 0, -1, 1, "xs:nonNegativeInteger");
                max = xml_get_max_occurs(
                    ctxt,
                    node,
                    0,
                    UNBOUNDED as _,
                    1,
                    "(xs:nonNegativeInteger | unbounded)",
                );
            }
            xml_schema_pcheck_particle_correct_2(ctxt, null_mut(), node, min, max);
            // Create a particle
            particle = xml_schema_add_particle(ctxt, Some(node), min, max);
            if particle.is_null() {
                return null_mut();
            }
            (*particle).children = item as XmlSchemaTreeItemPtr;
            // Check for illegal attributes.
            let mut attr = node.properties;
            while let Some(cur_attr) = attr {
                if let Some(ns) = cur_attr.ns {
                    if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                        xml_schema_pillegal_attr_err(
                            ctxt,
                            XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                            null_mut(),
                            cur_attr,
                        );
                    }
                } else if !xml_str_equal(cur_attr.name, c"id".as_ptr() as _)
                    && !xml_str_equal(cur_attr.name, c"maxOccurs".as_ptr() as _)
                    && !xml_str_equal(cur_attr.name, c"minOccurs".as_ptr() as _)
                {
                    xml_schema_pillegal_attr_err(
                        ctxt,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
                attr = cur_attr.next;
            }
        } else {
            // Check for illegal attributes.
            let mut attr = node.properties;
            while let Some(cur_attr) = attr {
                if let Some(ns) = cur_attr.ns {
                    if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                        xml_schema_pillegal_attr_err(
                            ctxt,
                            XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                            null_mut(),
                            cur_attr,
                        );
                    }
                } else if !xml_str_equal(cur_attr.name, c"id".as_ptr() as _) {
                    xml_schema_pillegal_attr_err(
                        ctxt,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
                attr = cur_attr.next;
            }
        }

        // Extract and validate attributes.
        xml_schema_pval_attr_id(ctxt, node, "id");
        // And now for the children...
        let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
        if is_schema(child, "annotation") {
            (*item).annot = (*ctxt).parse_annotation(child.unwrap(), 1);
            child = child
                .unwrap()
                .next
                .map(|node| XmlNodePtr::try_from(node).unwrap());
        }
        if typ == XmlSchemaTypeType::XmlSchemaTypeAll {
            let mut part: XmlSchemaParticlePtr;
            let mut last: XmlSchemaParticlePtr = null_mut();

            while is_schema(child, "element") {
                part = (*ctxt).parse_element(schema, child.unwrap(), &raw mut is_elem_ref, 0)
                    as XmlSchemaParticlePtr;
                // SPEC cos-all-limited (2)
                // "The {max occurs} of all the particles in the {particles}
                // of the ('all') group must be 0 or 1.
                if !part.is_null() {
                    if is_elem_ref != 0 {
                        has_refs += 1;
                    }
                    if (*part).min_occurs > 1 {
                        xml_schema_pcustom_err(
                            ctxt,
                            XmlParserErrors::XmlSchemapCosAllLimited,
                            null_mut(),
                            child.map(|child| child.into()),
                            "Invalid value for minOccurs (must be 0 or 1)",
                            None,
                        );
                        // Reset to 1.
                        (*part).min_occurs = 1;
                    }
                    if (*part).max_occurs > 1 {
                        xml_schema_pcustom_err(
                            ctxt,
                            XmlParserErrors::XmlSchemapCosAllLimited,
                            null_mut(),
                            child.map(|child| child.into()),
                            "Invalid value for maxOccurs (must be 0 or 1)",
                            None,
                        );
                        // Reset to 1.
                        (*part).max_occurs = 1;
                    }
                    if last.is_null() {
                        (*item).children = part as XmlSchemaTreeItemPtr;
                    } else {
                        (*last).next = part as XmlSchemaTreeItemPtr;
                    }
                    last = part;
                }
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if let Some(child) = child {
                xml_schema_pcontent_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                    null_mut(),
                    node,
                    Some(child.into()),
                    None,
                    Some("(annotation?, (annotation?, element*)"),
                );
            }
        } else {
            // choice + sequence
            let mut part: XmlSchemaTreeItemPtr = null_mut();
            let mut last: XmlSchemaTreeItemPtr = null_mut();

            while is_schema(child, "element")
                || is_schema(child, "group")
                || is_schema(child, "any")
                || is_schema(child, "choice")
                || is_schema(child, "sequence")
            {
                if is_schema(child, "element") {
                    part = (*ctxt).parse_element(schema, child.unwrap(), &raw mut is_elem_ref, 0)
                        as XmlSchemaTreeItemPtr;
                    if !part.is_null() && is_elem_ref != 0 {
                        has_refs += 1;
                    }
                } else if is_schema(child, "group") {
                    part = xml_schema_parse_model_group_def_ref(ctxt, schema, child.unwrap());
                    if !part.is_null() {
                        has_refs += 1;
                    }
                    // Handle redefinitions.
                    if (*ctxt).is_redefine != 0
                        && !(*ctxt).redef.is_null()
                        && (*(*(*ctxt).redef).item).typ == XmlSchemaTypeType::XmlSchemaTypeGroup
                        && !part.is_null()
                        && !(*part).children.is_null()
                        && xml_schema_get_qname_ref_name((*part).children as _)
                            == (*(*ctxt).redef).ref_name
                        && xml_schema_get_qname_ref_target_ns((*part).children as _)
                            == (*(*ctxt).redef).ref_target_ns
                    {
                        // SPEC src-redefine:
                        // (6.1) "If it has a <group> among its contents at
                        // some level the `actual value` of whose ref
                        // [attribute] is the same as the `actual value` of
                        // its own name attribute plus target namespace, then
                        // all of the following must be true:"
                        // (6.1.1) "It must have exactly one such group."
                        if (*ctxt).redef_counter != 0 {
                            let qname = xml_schema_format_qname(
                                Some(
                                    CStr::from_ptr((*(*ctxt).redef).ref_target_ns as *const i8)
                                        .to_string_lossy()
                                        .as_ref(),
                                ),
                                Some(
                                    CStr::from_ptr((*(*ctxt).redef).ref_name as *const i8)
                                        .to_string_lossy()
                                        .as_ref(),
                                ),
                            );

                            xml_schema_custom_err(
                                ctxt as XmlSchemaAbstractCtxtPtr,
                                XmlParserErrors::XmlSchemapSrcRedefine,
                                child.map(|child| child.into()),
                                null_mut(),
                                format!("The redefining model group definition '{qname}' must not contain more than one reference to the redefined definition").as_str(), 
                                Some(&qname),
                                None
                            );
                            part = null_mut();
                        } else if (*WXS_PARTICLE!(part)).min_occurs != 1
                            || (*WXS_PARTICLE!(part)).max_occurs != 1
                        {
                            let qname = xml_schema_format_qname(
                                Some(
                                    CStr::from_ptr((*(*ctxt).redef).ref_target_ns as *const i8)
                                        .to_string_lossy()
                                        .as_ref(),
                                ),
                                Some(
                                    CStr::from_ptr((*(*ctxt).redef).ref_name as *const i8)
                                        .to_string_lossy()
                                        .as_ref(),
                                ),
                            );

                            // SPEC src-redefine:
                            // (6.1.2) "The `actual value` of both that
                            // group's minOccurs and maxOccurs [attribute]
                            // must be 1 (or `absent`).
                            xml_schema_custom_err(
                                ctxt as XmlSchemaAbstractCtxtPtr,
                                XmlParserErrors::XmlSchemapSrcRedefine,
                                child.map(|child| child.into()),
                                null_mut(),
                                format!("The redefining model group definition '{qname}' must not contain a reference to the redefined definition with a maxOccurs/minOccurs other than 1").as_str(),
                                Some(&qname),
                                None
                            );
                            part = null_mut();
                        }
                        (*(*ctxt).redef).reference = part as XmlSchemaBasicItemPtr;
                        (*ctxt).redef_counter += 1;
                    }
                } else if is_schema(child, "any") {
                    part =
                        xml_schema_parse_any(ctxt, schema, child.unwrap()) as XmlSchemaTreeItemPtr;
                } else if is_schema(child, "choice") {
                    part = xml_schema_parse_model_group(
                        ctxt,
                        schema,
                        child.unwrap(),
                        XmlSchemaTypeType::XmlSchemaTypeChoice,
                        1,
                    );
                } else if is_schema(child, "sequence") {
                    part = xml_schema_parse_model_group(
                        ctxt,
                        schema,
                        child.unwrap(),
                        XmlSchemaTypeType::XmlSchemaTypeSequence,
                        1,
                    );
                }
                if !part.is_null() {
                    if last.is_null() {
                        (*item).children = part;
                    } else {
                        (*last).next = part;
                    }
                    last = part;
                }
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if let Some(child) = child {
                xml_schema_pcontent_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                    null_mut(),
                    node,
                    Some(child.into()),
                    None,
                    Some("(annotation?, (element | group | choice | sequence | any)*)"),
                );
            }
        }
        if max == 0 && min == 0 {
            return null_mut();
        }
        if has_refs != 0 {
            // We need to resolve references.
            WXS_ADD_PENDING!(ctxt, item);
        }
        if with_particle != 0 {
            particle as XmlSchemaTreeItemPtr
        } else {
            item as XmlSchemaTreeItemPtr
        }
    }
}

/// Parse a XML schema Facet declaration
/// *WARNING* this interface is highly subject to change
///
/// Returns the new type structure or NULL in case of error
#[doc(alias = "xmlSchemaParseFacet")]
unsafe fn xml_schema_parse_facet(
    ctxt: XmlSchemaParserCtxtPtr,
    schema: XmlSchemaPtr,
    node: XmlNodePtr,
) -> XmlSchemaFacetPtr {
    unsafe {
        if ctxt.is_null() || schema.is_null() {
            return null_mut();
        }

        let facet: XmlSchemaFacetPtr = xml_schema_new_facet();
        if facet.is_null() {
            xml_schema_perr_memory(ctxt, "allocating facet", Some(node.into()));
            return null_mut();
        }
        (*facet).node = node.into();
        let Some(value) = (*ctxt).get_prop(node, "value") else {
            let name = node.name().unwrap();
            xml_schema_perr2(
                ctxt,
                Some(node.into()),
                None,
                XmlParserErrors::XmlSchemapFacetNoValue,
                format!("Facet {name} has no value\n").as_str(),
                Some(&name),
                None,
            );
            xml_schema_free_facet(facet);
            return null_mut();
        };
        if is_schema(Some(node), "minInclusive") {
            (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetMinInclusive;
        } else if is_schema(Some(node), "minExclusive") {
            (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetMinExclusive;
        } else if is_schema(Some(node), "maxInclusive") {
            (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetMaxInclusive;
        } else if is_schema(Some(node), "maxExclusive") {
            (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetMaxExclusive;
        } else if is_schema(Some(node), "totalDigits") {
            (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetTotalDigits;
        } else if is_schema(Some(node), "fractionDigits") {
            (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetFractionDigits;
        } else if is_schema(Some(node), "pattern") {
            (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetPattern;
        } else if is_schema(Some(node), "enumeration") {
            (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetEnumeration;
        } else if is_schema(Some(node), "whiteSpace") {
            (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetWhitespace;
        } else if is_schema(Some(node), "length") {
            (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetLength;
        } else if is_schema(Some(node), "maxLength") {
            (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetMaxLength;
        } else if is_schema(Some(node), "minLength") {
            (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetMinLength;
        } else {
            let name = node.name().unwrap();
            xml_schema_perr2(
                ctxt,
                Some(node.into()),
                None,
                XmlParserErrors::XmlSchemapUnknownFacetType,
                format!("Unknown facet type {name}\n").as_str(),
                Some(&name),
                None,
            );
            xml_schema_free_facet(facet);
            return null_mut();
        }
        xml_schema_pval_attr_id(ctxt, node, "id");
        (*facet).value = xml_dict_lookup((*ctxt).dict, value.as_ptr(), value.len() as i32);
        if (*facet).typ != XmlSchemaTypeType::XmlSchemaFacetPattern
            && (*facet).typ != XmlSchemaTypeType::XmlSchemaFacetEnumeration
        {
            let fixed = (*ctxt).get_prop(node, "fixed");
            if fixed.as_deref() == Some("true") {
                (*facet).fixed = 1;
            }
        }
        let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());

        if is_schema(child, "annotation") {
            (*facet).annot = (*ctxt).parse_annotation(child.unwrap(), 1);
            child = child
                .unwrap()
                .next
                .map(|node| XmlNodePtr::try_from(node).unwrap());
        }
        if let Some(child) = child {
            let name = node.name().unwrap();
            xml_schema_perr2(
                ctxt,
                Some(node.into()),
                Some(child.into()),
                XmlParserErrors::XmlSchemapUnknownFacetChild,
                format!("Facet {name} has unexpected child content\n").as_str(),
                Some(&name),
                None,
            );
        }
        facet
    }
}

/// Parse a XML schema Restriction definition
/// *WARNING* this interface is highly subject to change
///
/// Returns the type definition or NULL in case of error
#[doc(alias = "xmlSchemaParseRestriction")]
pub(crate) unsafe fn xml_schema_parse_restriction(
    ctxt: XmlSchemaParserCtxtPtr,
    schema: XmlSchemaPtr,
    node: XmlNodePtr,
    parent_type: XmlSchemaTypeType,
) -> XmlSchemaTypePtr {
    unsafe {
        if ctxt.is_null() || schema.is_null() {
            return null_mut();
        }
        // Not a component, don't create it.
        let typ: XmlSchemaTypePtr = (*ctxt).ctxt_type;
        (*typ).flags |= XML_SCHEMAS_TYPE_DERIVATION_METHOD_RESTRICTION;

        // Check for illegal attributes.
        let mut attr = node.properties;
        while let Some(cur_attr) = attr {
            if let Some(ns) = cur_attr.ns {
                if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                    xml_schema_pillegal_attr_err(
                        ctxt,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
            } else if !xml_str_equal(cur_attr.name, c"id".as_ptr() as _)
                && !xml_str_equal(cur_attr.name, c"base".as_ptr() as _)
            {
                xml_schema_pillegal_attr_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                    null_mut(),
                    cur_attr,
                );
            }
            attr = cur_attr.next;
        }
        // Extract and validate attributes.
        xml_schema_pval_attr_id(ctxt, node, "id");

        // Attribute

        // Extract the base type. The "base" attribute is mandatory if inside
        // a complex type or if redefining.
        //
        // SPEC (1.2) "...otherwise (<restriction> has no <simpleType> "
        // among its [children]), the simple type definition which is
        // the {content type} of the type definition `resolved` to by
        // the `actual value` of the base [attribute]"
        if xml_schema_pval_attr_qname(
            ctxt,
            schema,
            null_mut(),
            node,
            "base",
            &raw mut (*typ).base_ns,
            &raw mut (*typ).base,
        ) == 0
        {
            if (*typ).base.is_null() && (*typ).typ == XmlSchemaTypeType::XmlSchemaTypeComplex {
                xml_schema_pmissing_attr_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapS4sAttrMissing,
                    null_mut(),
                    Some(node.into()),
                    Some("base"),
                    None,
                );
            } else if (*ctxt).is_redefine != 0 && (*typ).flags & XML_SCHEMAS_TYPE_GLOBAL != 0 {
                if (*typ).base.is_null() {
                    xml_schema_pmissing_attr_err(
                        ctxt,
                        XmlParserErrors::XmlSchemapS4sAttrMissing,
                        null_mut(),
                        Some(node.into()),
                        Some("base"),
                        None,
                    );
                } else if !xml_str_equal((*typ).base, (*typ).name)
                    || !xml_str_equal((*typ).base_ns, (*typ).target_namespace)
                {
                    let q1 = xml_schema_format_qname(
                        Some(
                            CStr::from_ptr((*typ).base_ns as *const i8)
                                .to_string_lossy()
                                .as_ref(),
                        ),
                        Some(
                            CStr::from_ptr((*typ).base as *const i8)
                                .to_string_lossy()
                                .as_ref(),
                        ),
                    );
                    let q2 = xml_schema_format_qname(
                        Some(
                            CStr::from_ptr((*typ).target_namespace as *const i8)
                                .to_string_lossy()
                                .as_ref(),
                        ),
                        Some(
                            CStr::from_ptr((*typ).name as *const i8)
                                .to_string_lossy()
                                .as_ref(),
                        ),
                    );

                    // REDEFINE: SPEC src-redefine (5)
                    // "Within the [children], each <simpleType> must have a
                    // <restriction> among its [children] ... the `actual value` of
                    // whose base [attribute] must be the same as the `actual value`
                    // of its own name attribute plus target namespace;"
                    xml_schema_pcustom_err_ext(
                        ctxt,
                        XmlParserErrors::XmlSchemapSrcRedefine,
                        null_mut(),
                        Some(node.into()),
                        format!("This is a redefinition, but the QName value '{q1}' of the 'base' attribute does not match the type's designation '{q2}'").as_str(),
                        Some(&q1),
                        Some(&q2),
                        None
                    );
                    // Avoid confusion and erase the values.
                    (*typ).base = null_mut();
                    (*typ).base_ns = null_mut();
                }
            }
        }
        // And now for the children...
        let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
        if is_schema(child, "annotation") {
            // Add the annotation to the simple type ancestor.
            xml_schema_add_annotation(
                typ as XmlSchemaAnnotItemPtr,
                (*ctxt).parse_annotation(child.unwrap(), 1),
            );
            child = child
                .unwrap()
                .next
                .map(|node| XmlNodePtr::try_from(node).unwrap());
        }
        if parent_type == XmlSchemaTypeType::XmlSchemaTypeSimple {
            // Corresponds to <simpleType><restriction><simpleType>.
            if is_schema(child, "simpleType") {
                if !(*typ).base.is_null() {
                    // src-restriction-base-or-simpleType
                    // Either the base [attribute] or the simpleType [child] of the
                    // <restriction> element must be present, but not both.
                    xml_schema_pcontent_err(
                        ctxt,
                        XmlParserErrors::XmlSchemapSrcRestrictionBaseOrSimpletype,
                        null_mut(),
                        node,
                        child.map(|child| child.into()),
                        Some(
                            "The attribute 'base' and the <simpleType> child are mutually exclusive",
                        ),
                        None,
                    );
                } else {
                    (*typ).base_type =
                        (*ctxt).parse_simple_type(schema, child.unwrap(), 0) as XmlSchemaTypePtr;
                }
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            } else if (*typ).base.is_null() {
                xml_schema_pcontent_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapSrcRestrictionBaseOrSimpletype,
                    null_mut(),
                    node,
                    child.map(|child| child.into()),
                    Some("Either the attribute 'base' or a <simpleType> child must be present"),
                    None,
                );
            }
        } else if parent_type == XmlSchemaTypeType::XmlSchemaTypeComplexContent {
            // Corresponds to <complexType><complexContent><restriction>...
            // followed by:
            //
            // Model groups <all>, <choice> and <sequence>.
            if is_schema(child, "all") {
                (*typ).subtypes = xml_schema_parse_model_group(
                    ctxt,
                    schema,
                    child.unwrap(),
                    XmlSchemaTypeType::XmlSchemaTypeAll,
                    1,
                ) as XmlSchemaTypePtr;
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            } else if is_schema(child, "choice") {
                (*typ).subtypes = xml_schema_parse_model_group(
                    ctxt,
                    schema,
                    child.unwrap(),
                    XmlSchemaTypeType::XmlSchemaTypeChoice,
                    1,
                ) as XmlSchemaTypePtr;
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            } else if is_schema(child, "sequence") {
                (*typ).subtypes = xml_schema_parse_model_group(
                    ctxt,
                    schema,
                    child.unwrap(),
                    XmlSchemaTypeType::XmlSchemaTypeSequence,
                    1,
                ) as XmlSchemaTypePtr;
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            // Model group reference <group>.
            } else if is_schema(child, "group") {
                (*typ).subtypes = xml_schema_parse_model_group_def_ref(ctxt, schema, child.unwrap())
                    as XmlSchemaTypePtr;
                // Note that the reference will be resolved in
                // xmlSchemaResolveTypeReferences();
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
        } else if parent_type == XmlSchemaTypeType::XmlSchemaTypeSimpleContent {
            // Corresponds to <complexType><simpleContent><restriction>...
            //
            // "1.1 the simple type definition corresponding to the <simpleType>
            // among the [children] of <restriction> if there is one;"
            if is_schema(child, "simpleType") {
                // We will store the to-be-restricted simple type in
                // (*typ).contentTypeDef *temporarily*.
                (*typ).content_type_def =
                    (*ctxt).parse_simple_type(schema, child.unwrap(), 0) as XmlSchemaTypePtr;
                if (*typ).content_type_def.is_null() {
                    return null_mut();
                }
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
        }

        if parent_type == XmlSchemaTypeType::XmlSchemaTypeSimple
            || parent_type == XmlSchemaTypeType::XmlSchemaTypeSimpleContent
        {
            let mut facet: XmlSchemaFacetPtr;
            let mut lastfacet: XmlSchemaFacetPtr = null_mut();
            // Corresponds to <complexType><simpleContent><restriction>...
            // <simpleType><restriction>...

            // Add the facets to the simple type ancestor.

            // TODO: Datatypes: 4.1.3 Constraints on XML Representation of
            // Simple Type Definition Schema Representation Constraint:
            // *Single Facet Value*
            while is_schema(child, "minInclusive")
                || is_schema(child, "minExclusive")
                || is_schema(child, "maxInclusive")
                || is_schema(child, "maxExclusive")
                || is_schema(child, "totalDigits")
                || is_schema(child, "fractionDigits")
                || is_schema(child, "pattern")
                || is_schema(child, "enumeration")
                || is_schema(child, "whiteSpace")
                || is_schema(child, "length")
                || is_schema(child, "maxLength")
                || is_schema(child, "minLength")
            {
                facet = xml_schema_parse_facet(ctxt, schema, child.unwrap());
                if !facet.is_null() {
                    if lastfacet.is_null() {
                        (*typ).facets = facet;
                    } else {
                        (*lastfacet).next = facet;
                    }
                    lastfacet = facet;
                    (*lastfacet).next = null_mut();
                }
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            // Create links for derivation and validation.
            if !(*typ).facets.is_null() {
                let mut facet_link: XmlSchemaFacetLinkPtr;
                let mut last_facet_link: XmlSchemaFacetLinkPtr = null_mut();

                facet = (*typ).facets;
                while {
                    facet_link =
                        xml_malloc(size_of::<XmlSchemaFacetLink>()) as XmlSchemaFacetLinkPtr;
                    if facet_link.is_null() {
                        xml_schema_perr_memory(ctxt, "allocating a facet link", None);
                        xml_free(facet_link as _);
                        return null_mut();
                    }
                    (*facet_link).facet = facet;
                    (*facet_link).next = null_mut();
                    if last_facet_link.is_null() {
                        (*typ).facet_set = facet_link;
                    } else {
                        (*last_facet_link).next = facet_link;
                    }
                    last_facet_link = facet_link;
                    facet = (*facet).next;

                    !facet.is_null()
                } {}
            }
        }
        if (*typ).typ == XmlSchemaTypeType::XmlSchemaTypeComplex {
            // Attribute uses/declarations.
            if xml_schema_parse_local_attributes(
                ctxt,
                schema,
                &mut child,
                &raw mut (*typ).attr_uses as *mut XmlSchemaItemListPtr<*mut c_void>,
                XmlSchemaTypeType::XmlSchemaTypeRestriction as i32,
                null_mut(),
            ) == -1
            {
                return null_mut();
            }
            // Attribute wildcard.
            if is_schema(child, "anyAttribute") {
                (*typ).attribute_wildcard =
                    xml_schema_parse_any_attribute(ctxt, schema, child.unwrap());
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
        }
        if let Some(child) = child {
            if parent_type == XmlSchemaTypeType::XmlSchemaTypeComplexContent {
                xml_schema_pcontent_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                    null_mut(),
                    node,
                    Some(child.into()),
                    None,
                    Some(
                        "annotation?, (group | all | choice | sequence)?, ((attribute | attributeGroup)*, anyAttribute?))",
                    ),
                );
            } else if parent_type == XmlSchemaTypeType::XmlSchemaTypeSimpleContent {
                xml_schema_pcontent_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                    null_mut(),
                    node,
                    Some(child.into()),
                    None,
                    Some(
                        "(annotation?, (simpleType?, (minExclusive | minInclusive | maxExclusive | maxInclusive | totalDigits | fractionDigits | length | minLength | maxLength | enumeration | whiteSpace | pattern)*)?, ((attribute | attributeGroup)*, anyAttribute?))",
                    ),
                );
            } else {
                // Simple type
                xml_schema_pcontent_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                    null_mut(),
                    node,
                    Some(child.into()),
                    None,
                    Some(
                        "(annotation?, (simpleType?, (minExclusive | minInclusive | maxExclusive | maxInclusive | totalDigits | fractionDigits | length | minLength | maxLength | enumeration | whiteSpace | pattern)*))",
                    ),
                );
            }
        }
        null_mut()
    }
}

/// parse a XML schema List definition
/// *WARNING* this interface is highly subject to change
///
/// Returns -1 in case of error, 0 if the declaration is improper and 1 in case of success.
#[doc(alias = "xmlSchemaParseList")]
pub(crate) unsafe fn xml_schema_parse_list(
    ctxt: XmlSchemaParserCtxtPtr,
    schema: XmlSchemaPtr,
    node: XmlNodePtr,
) -> XmlSchemaTypePtr {
    unsafe {
        if ctxt.is_null() || schema.is_null() {
            return null_mut();
        }
        // Not a component, don't create it.
        let typ: XmlSchemaTypePtr = (*ctxt).ctxt_type;
        // Mark the typ as being of variety "list".
        (*typ).flags |= XML_SCHEMAS_TYPE_VARIETY_LIST;
        // SPEC (Base type) (2) "If the <list> or <union> alternative is chosen,
        // then the `simple ur-type definition`."
        (*typ).base_type = xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasAnySimpletype);
        // Check for illegal attributes.
        let mut attr = node.properties;
        while let Some(cur_attr) = attr {
            if let Some(ns) = cur_attr.ns {
                if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                    xml_schema_pillegal_attr_err(
                        ctxt,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
            } else if !xml_str_equal(cur_attr.name, c"id".as_ptr() as _)
                && !xml_str_equal(cur_attr.name, c"itemType".as_ptr() as _)
            {
                xml_schema_pillegal_attr_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                    null_mut(),
                    cur_attr,
                );
            }
            attr = cur_attr.next;
        }

        xml_schema_pval_attr_id(ctxt, node, "id");

        // Attribute "itemType". NOTE that we will use the "ref" and "refNs"
        // fields for holding the reference to the itemType.
        //
        // REVAMP TODO: Use the "base" and "baseNs" fields, since we will remove
        // the "ref" fields.
        xml_schema_pval_attr_qname(
            ctxt,
            schema,
            null_mut(),
            node,
            "itemType",
            &raw mut (*typ).base_ns,
            &raw mut (*typ).base,
        );
        // And now for the children...
        let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
        if is_schema(child, "annotation") {
            xml_schema_add_annotation(
                typ as XmlSchemaAnnotItemPtr,
                (*ctxt).parse_annotation(child.unwrap(), 1),
            );
            child = child
                .unwrap()
                .next
                .map(|node| XmlNodePtr::try_from(node).unwrap());
        }
        if is_schema(child, "simpleType") {
            // src-list-itemType-or-simpleType
            // Either the itemType [attribute] or the <simpleType> [child] of
            // the <list> element must be present, but not both.
            if !(*typ).base.is_null() {
                xml_schema_pcustom_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapSrcSimpleType1,
                    null_mut(),
                    Some(node.into()),
                    "The attribute 'itemType' and the <simpleType> child are mutually exclusive",
                    None,
                );
            } else {
                (*typ).subtypes = (*ctxt).parse_simple_type(schema, child.unwrap(), 0);
            }
            child = child
                .unwrap()
                .next
                .map(|node| XmlNodePtr::try_from(node).unwrap());
        } else if (*typ).base.is_null() {
            xml_schema_pcustom_err(
                ctxt,
                XmlParserErrors::XmlSchemapSrcSimpleType1,
                null_mut(),
                Some(node.into()),
                "Either the attribute 'itemType' or the <simpleType> child must be present",
                None,
            );
        }
        if let Some(child) = child {
            xml_schema_pcontent_err(
                ctxt,
                XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                null_mut(),
                node,
                Some(child.into()),
                None,
                Some("(annotation?, simpleType?)"),
            );
        }
        if (*typ).base.is_null()
            && (*typ).subtypes.is_null()
            && xml_schema_get_prop_node(node, "itemType").is_none()
        {
            xml_schema_pcustom_err(
                ctxt,
                XmlParserErrors::XmlSchemapSrcSimpleType1,
                null_mut(),
                Some(node.into()),
                "Either the attribute 'itemType' or the <simpleType> child must be present",
                None,
            );
        }
        null_mut()
    }
}

/// Parse a XML schema Union definition
/// *WARNING* this interface is highly subject to change
///
/// Returns -1 in case of internal error, 0 in case of success and a positive
/// error code otherwise.
#[doc(alias = "xmlSchemaParseUnion")]
pub(crate) unsafe fn xml_schema_parse_union(
    ctxt: XmlSchemaParserCtxtPtr,
    schema: XmlSchemaPtr,
    node: XmlNodePtr,
) -> i32 {
    unsafe {
        if ctxt.is_null() || schema.is_null() {
            return -1;
        }
        // Not a component, don't create it.
        let typ: XmlSchemaTypePtr = (*ctxt).ctxt_type;
        // Mark the simple typ as being of variety "union".
        (*typ).flags |= XML_SCHEMAS_TYPE_VARIETY_UNION;
        // SPEC (Base type) (2) "If the <list> or <union> alternative is chosen,
        // then the `simple ur-type definition`."
        (*typ).base_type = xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasAnySimpletype);
        // Check for illegal attributes.
        let mut attr = node.properties;
        while let Some(cur_attr) = attr {
            if let Some(ns) = cur_attr.ns {
                if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                    xml_schema_pillegal_attr_err(
                        ctxt,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
            } else if !xml_str_equal(cur_attr.name, c"id".as_ptr() as _)
                && !xml_str_equal(cur_attr.name, c"memberTypes".as_ptr() as _)
            {
                xml_schema_pillegal_attr_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                    null_mut(),
                    cur_attr,
                );
            }
            attr = cur_attr.next;
        }
        xml_schema_pval_attr_id(ctxt, node, "id");
        // Attribute "memberTypes". This is a list of QNames.
        // TODO: Check the value to contain anything.
        let attr = xml_schema_get_prop_node(node, "memberTypes");
        if let Some(attr) = attr {
            let mut local_name: *const XmlChar = null();
            let mut ns_name: *mut XmlChar = null_mut();
            let mut link: XmlSchemaTypeLinkPtr;
            let mut last_link: XmlSchemaTypeLinkPtr = null_mut();
            let mut refe: XmlSchemaQNameRefPtr;

            let cur = (*ctxt).get_node_content(Some(attr.into()));
            (*typ).base = xml_dict_lookup((*ctxt).dict, cur.as_ptr(), cur.len() as i32);
            let mut cur = cur.as_str();
            while !cur.is_empty() {
                cur = cur.trim_start_matches(|c| xml_is_blank_char(c as u32));
                let end = cur.trim_start_matches(|c| !xml_is_blank_char(c as u32));
                if end.len() == cur.len() {
                    break;
                }
                let tmp = CString::new(&cur[..cur.len() - end.len()]).unwrap();
                if xml_schema_pval_attr_node_qname_value(
                    ctxt,
                    schema,
                    null_mut(),
                    attr,
                    tmp.as_ptr() as *const u8,
                    &raw mut ns_name as _,
                    &raw mut local_name,
                ) == 0
                {
                    // Create the member type link.
                    link = xml_malloc(size_of::<XmlSchemaTypeLink>()) as XmlSchemaTypeLinkPtr;
                    if link.is_null() {
                        xml_schema_perr_memory(
                            ctxt,
                            "xmlSchemaParseUnion, allocating a type link",
                            None,
                        );
                        return -1;
                    }
                    (*link).typ = null_mut();
                    (*link).next = null_mut();
                    if last_link.is_null() {
                        (*typ).member_types = link;
                    } else {
                        (*last_link).next = link;
                    }
                    last_link = link;
                    // Create a reference item.
                    refe = xml_schema_new_qname_ref(
                        ctxt,
                        XmlSchemaTypeType::XmlSchemaTypeSimple,
                        local_name,
                        ns_name,
                    );
                    if refe.is_null() {
                        return -1;
                    }
                    // Assign the reference to the link, it will be resolved
                    // later during fixup of the union simple type.
                    (*link).typ = refe as XmlSchemaTypePtr;
                }
                cur = end;
            }
        }
        // And now for the children...
        let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
        if is_schema(child, "annotation") {
            // Add the annotation to the simple type ancestor.
            xml_schema_add_annotation(
                typ as XmlSchemaAnnotItemPtr,
                (*ctxt).parse_annotation(child.unwrap(), 1),
            );
            child = child
                .unwrap()
                .next
                .map(|node| XmlNodePtr::try_from(node).unwrap());
        }
        if is_schema(child, "simpleType") {
            let mut subtype: XmlSchemaTypePtr;
            let mut last: XmlSchemaTypePtr = null_mut();

            // Anchor the member types in the "subtypes" field of the simple type.
            while is_schema(child, "simpleType") {
                subtype = (*ctxt).parse_simple_type(schema, child.unwrap(), 0) as XmlSchemaTypePtr;
                if !subtype.is_null() {
                    if last.is_null() {
                        (*typ).subtypes = subtype;
                        last = subtype;
                    } else {
                        (*last).next = subtype;
                        last = subtype;
                    }
                    (*last).next = null_mut();
                }
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
        }
        if let Some(child) = child {
            xml_schema_pcontent_err(
                ctxt,
                XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                null_mut(),
                node,
                Some(child.into()),
                None,
                Some("(annotation?, simpleType*)"),
            );
        }
        if attr.is_none() && (*typ).subtypes.is_null() {
            // src-union-memberTypes-or-simpleTypes
            // Either the memberTypes [attribute] of the <union> element must
            // be non-empty or there must be at least one simpleType [child].
            xml_schema_pcustom_err(
                ctxt,
                XmlParserErrors::XmlSchemapSrcUnionMembertypesOrSimpletypes,
                null_mut(),
                Some(node.into()),
                "Either the attribute 'memberTypes' or at least one <simpleType> child must be present",
                None,
            );
        }
        0
    }
}

/// Add an XML schema Group definition
///
/// Returns the new structure or NULL in case of error
#[doc(alias = "xmlSchemaAddModelGroupDefinition")]
pub(crate) unsafe fn xml_schema_add_model_group_definition(
    ctxt: XmlSchemaParserCtxtPtr,
    schema: XmlSchemaPtr,
    name: *const XmlChar,
    ns_name: *const XmlChar,
    node: XmlNodePtr,
) -> XmlSchemaModelGroupDefPtr {
    unsafe {
        let mut ret: XmlSchemaModelGroupDefPtr;

        if ctxt.is_null() || schema.is_null() || name.is_null() {
            return null_mut();
        }

        ret = xml_malloc(size_of::<XmlSchemaModelGroupDef>()) as XmlSchemaModelGroupDefPtr;
        if ret.is_null() {
            xml_schema_perr_memory(ctxt, "adding group", None);
            return null_mut();
        }
        std::ptr::write(
            &mut *ret,
            XmlSchemaModelGroupDef {
                typ: XmlSchemaTypeType::XmlSchemaTypeGroup,
                annot: null_mut(),
                next: null_mut(),
                children: null_mut(),
                name,
                target_namespace: ns_name,
                node,
                flags: 0,
            },
        );

        if (*ctxt).is_redefine != 0 {
            (*ctxt).redef = xml_schema_add_redef(ctxt, (*ctxt).redefined, ret as _, name, ns_name);
            if (*ctxt).redef.is_null() {
                xml_free(ret as _);
                return null_mut();
            }
            (*ctxt).redef_counter = 0;
        }
        WXS_ADD_GLOBAL!(ctxt, ret);
        WXS_ADD_PENDING!(ctxt, ret);
        ret
    }
}

/// Add an XML schema Attribute Group definition.
///
/// Returns the new structure or NULL in case of error
#[doc(alias = "xmlSchemaAddAttributeGroupDefinition")]
pub(crate) unsafe fn xml_schema_add_attribute_group_definition(
    pctxt: XmlSchemaParserCtxtPtr,
    _schema: XmlSchemaPtr,
    name: *const XmlChar,
    ns_name: *const XmlChar,
    node: XmlNodePtr,
) -> XmlSchemaAttributeGroupPtr {
    unsafe {
        let mut ret: XmlSchemaAttributeGroupPtr;

        if pctxt.is_null() || name.is_null() {
            return null_mut();
        }

        ret = xml_malloc(size_of::<XmlSchemaAttributeGroup>()) as XmlSchemaAttributeGroupPtr;
        if ret.is_null() {
            xml_schema_perr_memory(pctxt, "allocating attribute group", None);
            return null_mut();
        }
        memset(ret as _, 0, size_of::<XmlSchemaAttributeGroup>());
        (*ret).typ = XmlSchemaTypeType::XmlSchemaTypeAttributeGroup;
        (*ret).name = name as _;
        (*ret).target_namespace = ns_name;
        (*ret).node = node.into();

        // TODO: Remove the flag.
        (*ret).flags |= XML_SCHEMAS_ATTRGROUP_GLOBAL;
        if (*pctxt).is_redefine != 0 {
            (*pctxt).redef =
                xml_schema_add_redef(pctxt, (*pctxt).redefined, ret as _, name, ns_name);
            if (*pctxt).redef.is_null() {
                xml_free(ret as _);
                return null_mut();
            }
            (*pctxt).redef_counter = 0;
        }
        WXS_ADD_GLOBAL!(pctxt, ret);
        WXS_ADD_PENDING!(pctxt, ret);
        ret
    }
}

/// Add an XML schema annotation declaration
/// *WARNING* this interface is highly subject to change
///
/// Returns the new structure or NULL in case of error
#[doc(alias = "xmlSchemaAddNotation")]
pub(crate) unsafe fn xml_schema_add_notation(
    ctxt: XmlSchemaParserCtxtPtr,
    schema: XmlSchemaPtr,
    name: *const XmlChar,
    ns_name: *const XmlChar,
    _node: XmlNodePtr,
) -> XmlSchemaNotationPtr {
    unsafe {
        let mut ret: XmlSchemaNotationPtr;

        if ctxt.is_null() || schema.is_null() || name.is_null() {
            return null_mut();
        }

        ret = xml_malloc(size_of::<XmlSchemaNotation>()) as XmlSchemaNotationPtr;
        if ret.is_null() {
            xml_schema_perr_memory(ctxt, "add annotation", None);
            return null_mut();
        }
        memset(ret as _, 0, size_of::<XmlSchemaNotation>());
        (*ret).typ = XmlSchemaTypeType::XmlSchemaTypeNotation;
        (*ret).name = name;
        (*ret).target_namespace = ns_name;
        // TODO: do we need the node to be set?
        // (*ret).node = node;
        WXS_ADD_GLOBAL!(ctxt, ret);
        ret
    }
}

macro_rules! WXS_COMP_NAME {
    ($c:expr, $t:ty) => {
        (*($c as $t)).name
    };
}
macro_rules! WXS_COMP_TNS {
    ($c:expr, $t:ty) => {
        (*($c as $t)).target_namespace
    };
}

// ATTENTION TODO: This uses pointer comp. for strings.
#[doc(alias = "xmlSchemaFindRedefCompInGraph")]
unsafe fn xml_schema_find_redef_comp_in_graph(
    bucket: XmlSchemaBucketPtr,
    typ: XmlSchemaTypeType,
    name: *const XmlChar,
    ns_name: *const XmlChar,
) -> XmlSchemaBasicItemPtr {
    unsafe {
        let mut ret: XmlSchemaBasicItemPtr;

        if bucket.is_null() || name.is_null() {
            return null_mut();
        }
        if !(*bucket).globals.is_null() && !(*(*bucket).globals).items.is_empty() {
            // Search in global components.
            for ret in (*(*bucket).globals)
                .items
                .iter()
                .map(|&ret| ret as XmlSchemaBasicItemPtr)
            {
                if (*ret).typ == typ {
                    match typ {
                        XmlSchemaTypeType::XmlSchemaTypeComplex
                        | XmlSchemaTypeType::XmlSchemaTypeSimple => {
                            if WXS_COMP_NAME!(ret, XmlSchemaTypePtr) == name
                                && WXS_COMP_TNS!(ret, XmlSchemaTypePtr) == ns_name
                            {
                                return ret;
                            }
                        }
                        XmlSchemaTypeType::XmlSchemaTypeGroup => {
                            if WXS_COMP_NAME!(ret, XmlSchemaModelGroupDefPtr) == name
                                && WXS_COMP_TNS!(ret, XmlSchemaModelGroupDefPtr) == ns_name
                            {
                                return ret;
                            }
                        }
                        XmlSchemaTypeType::XmlSchemaTypeAttributeGroup => {
                            if WXS_COMP_NAME!(ret, XmlSchemaAttributeGroupPtr) == name as _
                                && WXS_COMP_TNS!(ret, XmlSchemaAttributeGroupPtr) == ns_name
                            {
                                return ret;
                            }
                        }
                        _ => {
                            // Should not be hit.
                            return null_mut();
                        }
                    }
                }
            }
        }
        // subschemas:
        // Process imported/included schemas.
        if !(*bucket).relations.is_null() {
            let mut rel: XmlSchemaSchemaRelationPtr = (*bucket).relations;

            // TODO: Marking the bucket will not avoid multiple searches
            // in the same schema, but avoids at least circularity.
            (*bucket).flags |= XML_SCHEMA_BUCKET_MARKED;
            while {
                if !(*rel).bucket.is_null()
                    && (*(*rel).bucket).flags & XML_SCHEMA_BUCKET_MARKED == 0
                {
                    ret = xml_schema_find_redef_comp_in_graph((*rel).bucket, typ, name, ns_name);
                    if !ret.is_null() {
                        return ret;
                    }
                }
                rel = (*rel).next;

                !rel.is_null()
            } {}
            (*bucket).flags ^= XML_SCHEMA_BUCKET_MARKED;
        }
        null_mut()
    }
}

unsafe fn xml_schema_check_srcredefine_first(pctxt: XmlSchemaParserCtxtPtr) -> i32 {
    unsafe {
        let mut err: i32 = 0;
        let mut redef: XmlSchemaRedefPtr = (*WXS_CONSTRUCTOR!(pctxt)).redefs;
        let mut prev: XmlSchemaBasicItemPtr;
        let mut item: XmlSchemaBasicItemPtr;
        let mut was_redefined: i32;

        if redef.is_null() {
            return 0;
        }

        while !redef.is_null() {
            item = (*redef).item;
            // First try to locate the redefined component in the
            // schema graph starting with the redefined schema.
            // NOTE: According to this schema bug entry:
            //   http://lists.w3.org/Archives/Public/www-xml-schema-comments/2005OctDec/0019.html
            //   it's not clear if the referenced component needs to originate
            //   from the <redefine>d schema _document_ or the schema; the latter
            //   would include all imported and included sub-schemas of the
            //   <redefine>d schema. Currently the latter approach is used.
            //   SUPPLEMENT: It seems that the WG moves towards the latter
            //   approach, so we are doing it right.
            //
            prev = xml_schema_find_redef_comp_in_graph(
                (*redef).target_bucket,
                (*item).typ,
                (*redef).ref_name,
                (*redef).ref_target_ns,
            );
            if prev.is_null() {
                // SPEC src-redefine:
                // (6.2.1) "The `actual value` of its own name attribute plus
                // target namespace must successfully `resolve` to a model
                // group definition in I."
                // (7.2.1) "The `actual value` of its own name attribute plus
                // target namespace must successfully `resolve` to an attribute
                // group definition in I."
                //
                // Note that, if we are redefining with the use of references
                // to components, the spec assumes the src-resolve to be used;
                // but this won't assure that we search only *inside* the
                // redefined schema.
                let node = if !(*redef).reference.is_null() {
                    xml_schema_get_component_node(((*redef).reference) as _)
                } else {
                    xml_schema_get_component_node(item as _)
                };
                // TODO: error code.
                // Probably XmlParserErrors::XML_SCHEMAP_SRC_RESOLVE, if this is using the
                // reference kind.
                let typename = xml_schema_get_component_type_str(item as _);
                let qname = xml_schema_format_qname(
                    Some(
                        CStr::from_ptr((*redef).ref_target_ns as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                    ),
                    Some(
                        CStr::from_ptr((*redef).ref_name as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                    ),
                );
                xml_schema_custom_err(
                    pctxt as XmlSchemaAbstractCtxtPtr,
                    XmlParserErrors::XmlSchemapSrcRedefine,
                    node.map(|node| node.into()),
                    null_mut(),
                    format!("The {typename} '{qname}' to be redefined could not be found in the redefined schema").as_str(),
                    Some(typename),
                    Some(&qname),
                );
                err = (*pctxt).err;
                redef = (*redef).next;
                continue;
            }
            // TODO: Obtaining and setting the redefinition state is really clumsy.
            was_redefined = 0;
            match (*item).typ {
                XmlSchemaTypeType::XmlSchemaTypeComplex
                | XmlSchemaTypeType::XmlSchemaTypeSimple => {
                    if (*(prev as XmlSchemaTypePtr)).flags & XML_SCHEMAS_TYPE_REDEFINED != 0 {
                        was_redefined = 1;
                        // break;
                    } else {
                        // Mark it as redefined.
                        (*(prev as XmlSchemaTypePtr)).flags |= XML_SCHEMAS_TYPE_REDEFINED;
                        // Assign the redefined type to the
                        // base type of the redefining type.
                        // TODO: How
                        (*(item as XmlSchemaTypePtr)).base_type = prev as XmlSchemaTypePtr;
                    }
                }
                XmlSchemaTypeType::XmlSchemaTypeGroup => {
                    if (*(prev as XmlSchemaModelGroupDefPtr)).flags
                        & XML_SCHEMA_MODEL_GROUP_DEF_REDEFINED
                        != 0
                    {
                        was_redefined = 1;
                        // break;
                    } else {
                        // Mark it as redefined.
                        (*(prev as XmlSchemaModelGroupDefPtr)).flags |=
                            XML_SCHEMA_MODEL_GROUP_DEF_REDEFINED;
                        if !(*redef).reference.is_null() {
                            // Overwrite the QName-reference with the
                            // referenced model group def.
                            (*((*redef).reference as XmlSchemaParticlePtr)).children =
                                prev as XmlSchemaTreeItemPtr;
                        }
                        (*redef).target = prev;
                    }
                }
                XmlSchemaTypeType::XmlSchemaTypeAttributeGroup => {
                    if (*(prev as XmlSchemaAttributeGroupPtr)).flags
                        & XML_SCHEMAS_ATTRGROUP_REDEFINED
                        != 0
                    {
                        was_redefined = 1;
                        // break;
                    } else {
                        (*(prev as XmlSchemaAttributeGroupPtr)).flags |=
                            XML_SCHEMAS_ATTRGROUP_REDEFINED;
                        if !(*redef).reference.is_null() {
                            // Assign the redefined attribute group to the
                            // QName-reference component.
                            // This is the easy case, since we will just
                            // expand the redefined group.
                            (*((*redef).reference as XmlSchemaQNameRefPtr)).item = prev;
                            (*redef).target = null_mut();
                        } else {
                            // This is the complicated case: we need
                            // to apply src-redefine (7.2.2) at a later
                            // stage, i.e. when attribute group references
                            // have been expanded and simple types have
                            // been fixed.
                            (*redef).target = prev;
                        }
                    }
                }
                _ => {
                    PERROR_INT!(
                        pctxt,
                        "xmlSchemaResolveRedefReferences",
                        "Unexpected redefined component type"
                    );
                    return -1;
                }
            }
            if was_redefined != 0 {
                let node = if !(*redef).reference.is_null() {
                    xml_schema_get_component_node(((*redef).reference) as _)
                } else {
                    xml_schema_get_component_node(((*redef).item) as _)
                };

                let desig = xml_schema_get_component_designation(prev as _);

                // TODO: error code.
                xml_schema_custom_err(
                    pctxt as XmlSchemaAbstractCtxtPtr,
                    XmlParserErrors::XmlSchemapSrcRedefine,
                    node.map(|node| node.into()),
                    null_mut(),
                    format!("The referenced {desig} was already redefined. Multiple redefinition of the same component is not supported").as_str(),
                    Some(&desig),
                    None
                );
                err = (*pctxt).err;
                redef = (*redef).next;
                continue;
            }
            redef = (*redef).next;
        }

        err
    }
}

macro_rules! WXS_REDEFINED_TYPE {
    ($item:expr) => {
        (*($item as XmlSchemaTypePtr)).flags & XML_SCHEMAS_TYPE_REDEFINED != 0
    };
}
macro_rules! WXS_REDEFINED_MODEL_GROUP_DEF {
    ($item:expr) => {
        (*($item as XmlSchemaModelGroupDefPtr)).flags & XML_SCHEMA_MODEL_GROUP_DEF_REDEFINED != 0
    };
}
macro_rules! WXS_REDEFINED_ATTR_GROUP {
    ($item:expr) => {
        (*($item as XmlSchemaAttributeGroupPtr)).flags & XML_SCHEMAS_ATTRGROUP_REDEFINED != 0
    };
}

unsafe fn xml_schema_add_components(
    pctxt: XmlSchemaParserCtxtPtr,
    bucket: XmlSchemaBucketPtr,
) -> i32 {
    unsafe {
        let mut name: *const XmlChar;

        // Add global components to the schema's hash tables.
        // This is the place where duplicate components will be
        // detected.
        // TODO: I think normally we should support imports of the
        //   same namespace from multiple locations. We don't do currently,
        //   but if we do then according to:
        //   http://www.w3.org/Bugs/Public/show_bug.cgi?id=2224
        //   we would need, if imported directly, to import redefined
        //   components as well to be able to catch clashing components.
        //   (I hope I'll still know what this means after some months :-()
        if bucket.is_null() {
            return -1;
        }
        if (*bucket).flags & XML_SCHEMA_BUCKET_COMPS_ADDED != 0 {
            return 0;
        }
        (*bucket).flags |= XML_SCHEMA_BUCKET_COMPS_ADDED;

        for item in (*(*bucket).globals)
            .items
            .iter()
            .map(|&item| item as XmlSchemaBasicItemPtr)
        {
            let duplicate = match (*item).typ {
                XmlSchemaTypeType::XmlSchemaTypeComplex
                | XmlSchemaTypeType::XmlSchemaTypeSimple => {
                    if WXS_REDEFINED_TYPE!(item) {
                        continue;
                    }
                    name = (*(item as XmlSchemaTypePtr)).name;
                    let table = if WXS_IS_BUCKET_IMPMAIN!((*bucket).typ) {
                        &mut (*(*WXS_IMPBUCKET!(bucket)).schema).type_decl
                    } else {
                        &mut (*(*(*WXS_INCBUCKET!(bucket)).owner_import).schema).type_decl
                    };
                    table
                        .insert(
                            CStr::from_ptr(name as *const i8)
                                .to_string_lossy()
                                .into_owned(),
                            item as _,
                        )
                        .is_some()
                }
                XmlSchemaTypeType::XmlSchemaTypeElement => {
                    name = (*(item as XmlSchemaElementPtr)).name;
                    let table = if WXS_IS_BUCKET_IMPMAIN!((*bucket).typ) {
                        &mut (*(*WXS_IMPBUCKET!(bucket)).schema).elem_decl
                    } else {
                        &mut (*(*(*WXS_INCBUCKET!(bucket)).owner_import).schema).elem_decl
                    };
                    table
                        .insert(
                            CStr::from_ptr(name as *const i8)
                                .to_string_lossy()
                                .into_owned(),
                            item as _,
                        )
                        .is_some()
                }
                XmlSchemaTypeType::XmlSchemaTypeAttribute => {
                    name = (*(item as XmlSchemaAttributePtr)).name;
                    let table = if WXS_IS_BUCKET_IMPMAIN!((*bucket).typ) {
                        &mut (*(*WXS_IMPBUCKET!(bucket)).schema).attr_decl
                    } else {
                        &mut (*(*(*WXS_INCBUCKET!(bucket)).owner_import).schema).attr_decl
                    };
                    table
                        .insert(
                            CStr::from_ptr(name as *const i8)
                                .to_string_lossy()
                                .into_owned(),
                            item as _,
                        )
                        .is_some()
                }
                XmlSchemaTypeType::XmlSchemaTypeGroup => {
                    if WXS_REDEFINED_MODEL_GROUP_DEF!(item) {
                        continue;
                    }
                    name = (*(item as XmlSchemaModelGroupDefPtr)).name;
                    let table = if WXS_IS_BUCKET_IMPMAIN!((*bucket).typ) {
                        &mut (*(*WXS_IMPBUCKET!(bucket)).schema).group_decl
                    } else {
                        &mut (*(*(*WXS_INCBUCKET!(bucket)).owner_import).schema).group_decl
                    };
                    table
                        .insert(
                            CStr::from_ptr(name as *const i8)
                                .to_string_lossy()
                                .into_owned(),
                            item as _,
                        )
                        .is_some()
                }
                XmlSchemaTypeType::XmlSchemaTypeAttributeGroup => {
                    if WXS_REDEFINED_ATTR_GROUP!(item) {
                        continue;
                    }
                    name = (*(item as XmlSchemaAttributeGroupPtr)).name;
                    let table = if WXS_IS_BUCKET_IMPMAIN!((*bucket).typ) {
                        &mut (*(*WXS_IMPBUCKET!(bucket)).schema).attrgrp_decl
                    } else {
                        &mut (*(*(*WXS_INCBUCKET!(bucket)).owner_import).schema).attrgrp_decl
                    };
                    table
                        .insert(
                            CStr::from_ptr(name as *const i8)
                                .to_string_lossy()
                                .into_owned(),
                            item as _,
                        )
                        .is_some()
                }
                XmlSchemaTypeType::XmlSchemaTypeIDCKey
                | XmlSchemaTypeType::XmlSchemaTypeIDCUnique
                | XmlSchemaTypeType::XmlSchemaTypeIDCKeyref => {
                    name = (*(item as XmlSchemaIDCPtr)).name;
                    let table = if WXS_IS_BUCKET_IMPMAIN!((*bucket).typ) {
                        &mut (*(*WXS_IMPBUCKET!(bucket)).schema).idc_def
                    } else {
                        &mut (*(*(*WXS_INCBUCKET!(bucket)).owner_import).schema).idc_def
                    };
                    table
                        .insert(
                            CStr::from_ptr(name as *const i8)
                                .to_string_lossy()
                                .into_owned(),
                            item as _,
                        )
                        .is_some()
                }
                XmlSchemaTypeType::XmlSchemaTypeNotation => {
                    name = (*(item as XmlSchemaNotationPtr)).name;
                    let table = if WXS_IS_BUCKET_IMPMAIN!((*bucket).typ) {
                        &mut (*(*WXS_IMPBUCKET!(bucket)).schema).nota_decl
                    } else {
                        &mut (*(*(*WXS_INCBUCKET!(bucket)).owner_import).schema).nota_decl
                    };
                    table
                        .insert(
                            CStr::from_ptr(name as *const i8)
                                .to_string_lossy()
                                .into_owned(),
                            item as _,
                        )
                        .is_some()
                }
                _ => {
                    PERROR_INT!(
                        pctxt,
                        "xmlSchemaAddComponents",
                        "Unexpected global component type"
                    );
                    continue;
                }
            };
            if duplicate {
                let typename = xml_schema_get_component_type_str(item as _);
                let qname = xml_schema_get_component_qname(item as _);
                xml_schema_custom_err(
                    pctxt as XmlSchemaAbstractCtxtPtr,
                    XmlParserErrors::XmlSchemapRedefinedType,
                    xml_schema_get_component_node(item as _).map(|node| node.into()),
                    item as XmlSchemaBasicItemPtr,
                    format!("A global {typename} '{qname}' does already exist").as_str(),
                    Some(typename),
                    Some(&qname),
                );
            }
        }
        // Process imported/included schemas.
        if !(*bucket).relations.is_null() {
            let mut rel: XmlSchemaSchemaRelationPtr = (*bucket).relations;
            while {
                if !(*rel).bucket.is_null()
                    && (*(*rel).bucket).flags & XML_SCHEMA_BUCKET_COMPS_ADDED == 0
                    && xml_schema_add_components(pctxt, (*rel).bucket) == -1
                {
                    return -1;
                }
                rel = (*rel).next;
                !rel.is_null()
            } {}
        }
        0
    }
}

/// Resolves the references of an element declaration or particle,
/// which has an element declaration as it's term.
#[doc(alias = "xmlSchemaResolveElementReferences")]
unsafe fn xml_schema_resolve_element_references(
    elem_decl: XmlSchemaElementPtr,
    ctxt: XmlSchemaParserCtxtPtr,
) {
    unsafe {
        if ctxt.is_null()
            || elem_decl.is_null()
            || (!elem_decl.is_null()
                && (*elem_decl).flags & XML_SCHEMAS_ELEM_INTERNAL_RESOLVED != 0)
        {
            return;
        }
        (*elem_decl).flags |= XML_SCHEMAS_ELEM_INTERNAL_RESOLVED;

        if (*elem_decl).subtypes.is_null() && !(*elem_decl).named_type.is_null() {
            // (type definition) ... otherwise the type definition `resolved`
            // to by the `actual value` of the type [attribute] ...
            let typ: XmlSchemaTypePtr = (*(*ctxt).schema).get_type(
                CStr::from_ptr((*elem_decl).named_type as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                (!(*elem_decl).named_type_ns.is_null())
                    .then(|| {
                        CStr::from_ptr((*elem_decl).named_type_ns as *const i8).to_string_lossy()
                    })
                    .as_deref(),
            );
            if typ.is_null() {
                xml_schema_pres_comp_attr_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapSrcResolve,
                    elem_decl as XmlSchemaBasicItemPtr,
                    (*elem_decl).node.map(|node| node.into()),
                    "type",
                    (!(*elem_decl).named_type.is_null())
                        .then(|| {
                            CStr::from_ptr((*elem_decl).named_type as *const i8).to_string_lossy()
                        })
                        .as_deref(),
                    (!(*elem_decl).named_type_ns.is_null())
                        .then(|| {
                            CStr::from_ptr((*elem_decl).named_type_ns as *const i8)
                                .to_string_lossy()
                        })
                        .as_deref(),
                    XmlSchemaTypeType::XmlSchemaTypeBasic,
                    Some("type definition"),
                );
            } else {
                (*elem_decl).subtypes = typ;
            }
        }
        if !(*elem_decl).subst_group.is_null() {
            // FIXME TODO: Do we need a new field in _xmlSchemaElement for substitutionGroup?
            let subst_head: XmlSchemaElementPtr = (*(*ctxt).schema).get_elem(
                CStr::from_ptr((*elem_decl).subst_group as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                (!(*elem_decl).subst_group_ns.is_null())
                    .then(|| {
                        CStr::from_ptr((*elem_decl).subst_group_ns as *const i8).to_string_lossy()
                    })
                    .as_deref(),
            );
            if subst_head.is_null() {
                xml_schema_pres_comp_attr_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapSrcResolve,
                    elem_decl as XmlSchemaBasicItemPtr,
                    None,
                    "substitutionGroup",
                    (!(*elem_decl).subst_group.is_null())
                        .then(|| {
                            CStr::from_ptr((*elem_decl).subst_group as *const i8).to_string_lossy()
                        })
                        .as_deref(),
                    (!(*elem_decl).subst_group_ns.is_null())
                        .then(|| {
                            CStr::from_ptr((*elem_decl).subst_group_ns as *const i8)
                                .to_string_lossy()
                        })
                        .as_deref(),
                    XmlSchemaTypeType::XmlSchemaTypeElement,
                    None,
                );
            } else {
                xml_schema_resolve_element_references(subst_head, ctxt);
                // Set the "substitution group affiliation".
                // NOTE that now we use the "ref_decl" field for this.
                (*elem_decl).ref_decl = subst_head;
                // The type definitions is set to:
                // SPEC "...the {type definition} of the element
                // declaration `resolved` to by the `actual value`
                // of the substitutionGroup [attribute], if present"
                if (*elem_decl).subtypes.is_null() {
                    if (*subst_head).subtypes.is_null() {
                        // This can happen with self-referencing substitution
                        // groups. The cycle will be detected later, but we have
                        // to set subtypes to avoid null-pointer dereferences.
                        (*elem_decl).subtypes =
                            xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasAnytype);
                    } else {
                        (*elem_decl).subtypes = (*subst_head).subtypes;
                    }
                }
            }
        }
        // SPEC "The definition of anyType serves as the default type definition
        // for element declarations whose XML representation does not specify one."
        if (*elem_decl).subtypes.is_null()
            && (*elem_decl).named_type.is_null()
            && (*elem_decl).subst_group.is_null()
        {
            (*elem_decl).subtypes =
                xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasAnytype);
        }
    }
}

/// Checks and builds the "member type definitions" property of the union
/// simple type. This handles part (1), part (2) is done in
/// xmlSchemaFinishMemberTypeDefinitionsProperty()
///
/// Returns -1 in case of an internal error, 0 otherwise.
#[doc(alias = "xmlSchemaResolveUnionMemberTypes")]
unsafe fn xml_schema_resolve_union_member_types(
    ctxt: XmlSchemaParserCtxtPtr,
    typ: XmlSchemaTypePtr,
) -> i32 {
    unsafe {
        let mut link: XmlSchemaTypeLinkPtr;
        let mut last_link: XmlSchemaTypeLinkPtr;
        let mut new_link: XmlSchemaTypeLinkPtr;
        let mut member_type: XmlSchemaTypePtr;

        // SPEC (1) "If the <union> alternative is chosen, then [Definition:]
        // define the explicit members as the type definitions `resolved`
        // to by the items in the `actual value` of the memberTypes [attribute],
        // if any, followed by the type definitions corresponding to the
        // <simpleType>s among the [children] of <union>, if any."

        // Resolve references.
        link = (*typ).member_types;
        last_link = null_mut();
        while !link.is_null() {
            let name: *const XmlChar = (*((*link).typ as XmlSchemaQNameRefPtr)).name;
            let ns_name: *const XmlChar = (*((*link).typ as XmlSchemaQNameRefPtr)).target_namespace;

            member_type = (*(*ctxt).schema).get_type(
                CStr::from_ptr(name as *const i8).to_string_lossy().as_ref(),
                (!ns_name.is_null())
                    .then(|| CStr::from_ptr(ns_name as *const i8).to_string_lossy())
                    .as_deref(),
            );
            if member_type.is_null() || !wxs_is_simple(member_type) {
                xml_schema_pres_comp_attr_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapSrcResolve,
                    typ as XmlSchemaBasicItemPtr,
                    (*typ).node.map(|node| node.into()),
                    "memberTypes",
                    (!name.is_null())
                        .then(|| CStr::from_ptr(name as *const i8).to_string_lossy())
                        .as_deref(),
                    (!ns_name.is_null())
                        .then(|| CStr::from_ptr(ns_name as *const i8).to_string_lossy())
                        .as_deref(),
                    XmlSchemaTypeType::XmlSchemaTypeSimple,
                    None,
                );
                // Remove the member type link.
                if last_link.is_null() {
                    (*typ).member_types = (*link).next;
                } else {
                    (*last_link).next = (*link).next;
                }
                new_link = link;
                link = (*link).next;
                xml_free(new_link as _);
            } else {
                (*link).typ = member_type;
                last_link = link;
                link = (*link).next;
            }
        }
        // Add local simple types,
        member_type = (*typ).subtypes;
        while !member_type.is_null() {
            link = xml_malloc(size_of::<XmlSchemaTypeLink>()) as XmlSchemaTypeLinkPtr;
            if link.is_null() {
                xml_schema_perr_memory(ctxt, "allocating a type link", None);
                return -1;
            }
            (*link).typ = member_type;
            (*link).next = null_mut();
            if last_link.is_null() {
                (*typ).member_types = link;
            } else {
                (*last_link).next = link;
            }
            last_link = link;
            member_type = (*member_type).next;
        }
        0
    }
}

/// Resolves type definition references
#[doc(alias = "xmlSchemaResolveTypeReferences")]
unsafe fn xml_schema_resolve_type_references(
    type_def: XmlSchemaTypePtr,
    ctxt: XmlSchemaParserCtxtPtr,
) {
    unsafe {
        if type_def.is_null() {
            return;
        }

        // Resolve the base type.
        if (*type_def).base_type.is_null() {
            (*type_def).base_type = (*(*ctxt).schema).get_type(
                CStr::from_ptr((*type_def).base as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                (!(*type_def).base_ns.is_null())
                    .then(|| CStr::from_ptr((*type_def).base_ns as *const i8).to_string_lossy())
                    .as_deref(),
            );
            if (*type_def).base_type.is_null() {
                xml_schema_pres_comp_attr_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapSrcResolve,
                    type_def as XmlSchemaBasicItemPtr,
                    (*type_def).node.map(|node| node.into()),
                    "base",
                    (!(*type_def).base.is_null())
                        .then(|| CStr::from_ptr((*type_def).base as *const i8).to_string_lossy())
                        .as_deref(),
                    (!(*type_def).base_ns.is_null())
                        .then(|| CStr::from_ptr((*type_def).base_ns as *const i8).to_string_lossy())
                        .as_deref(),
                    XmlSchemaTypeType::XmlSchemaTypeSimple,
                    None,
                );
                return;
            }
        }
        if wxs_is_simple(type_def) {
            if (*type_def).wxs_is_union() {
                // Resolve the memberTypes.
                xml_schema_resolve_union_member_types(ctxt, type_def);
            } else if (*type_def).wxs_is_list() {
                // Resolve the itemType.
                if (*type_def).subtypes.is_null() && !(*type_def).base.is_null() {
                    (*type_def).subtypes = (*(*ctxt).schema).get_type(
                        CStr::from_ptr((*type_def).base as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                        (!(*type_def).base_ns.is_null())
                            .then(|| {
                                CStr::from_ptr((*type_def).base_ns as *const i8).to_string_lossy()
                            })
                            .as_deref(),
                    );

                    if (*type_def).subtypes.is_null() || !wxs_is_simple((*type_def).subtypes) {
                        (*type_def).subtypes = null_mut();
                        xml_schema_pres_comp_attr_err(
                            ctxt,
                            XmlParserErrors::XmlSchemapSrcResolve,
                            type_def as XmlSchemaBasicItemPtr,
                            (*type_def).node.map(|node| node.into()),
                            "itemType",
                            (!(*type_def).base.is_null())
                                .then(|| {
                                    CStr::from_ptr((*type_def).base as *const i8).to_string_lossy()
                                })
                                .as_deref(),
                            (!(*type_def).base_ns.is_null())
                                .then(|| {
                                    CStr::from_ptr((*type_def).base_ns as *const i8)
                                        .to_string_lossy()
                                })
                                .as_deref(),
                            XmlSchemaTypeType::XmlSchemaTypeSimple,
                            None,
                        );
                    }
                }
                return;
            }
        }
        // The ball of letters below means, that if we have a particle
        // which has a QName-helper component as its {term}, we want
        // to resolve it...
        else if !(*type_def).subtypes.is_null()
            && (*(*type_def).subtypes).typ == XmlSchemaTypeType::XmlSchemaTypeParticle
            && !WXS_TYPE_PARTICLE_TERM!(type_def).is_null()
            && (*WXS_TYPE_PARTICLE_TERM!(type_def)).typ == XmlSchemaTypeType::XmlSchemaExtraQNameRef
        {
            let refe: XmlSchemaQNameRefPtr =
                WXS_TYPE_PARTICLE_TERM!(type_def) as XmlSchemaQNameRefPtr;

            // URGENT TODO: Test this.
            WXS_TYPE_PARTICLE_TERM!(type_def) = null_mut();
            // Resolve the MG definition reference.
            let group_def: XmlSchemaModelGroupDefPtr = (*(*ctxt).schema).get_named_component(
                (*refe).item_type,
                CStr::from_ptr((*refe).name as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                (!(*refe).target_namespace.is_null())
                    .then(|| {
                        CStr::from_ptr((*refe).target_namespace as *const i8).to_string_lossy()
                    })
                    .as_deref(),
            ) as XmlSchemaModelGroupDefPtr;
            if group_def.is_null() {
                xml_schema_pres_comp_attr_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapSrcResolve,
                    null_mut(),
                    xml_schema_get_component_node((WXS_TYPE_PARTICLE!(type_def)) as _)
                        .map(|node| node.into()),
                    "ref",
                    (!(*refe).name.is_null())
                        .then(|| CStr::from_ptr((*refe).name as *const i8).to_string_lossy())
                        .as_deref(),
                    (!(*refe).target_namespace.is_null())
                        .then(|| {
                            CStr::from_ptr((*refe).target_namespace as *const i8).to_string_lossy()
                        })
                        .as_deref(),
                    (*refe).item_type,
                    None,
                );
                // Remove the particle.
                (*type_def).subtypes = null_mut();
            } else if WXS_MODELGROUPDEF_MODEL!(group_def).is_null() {
                // Remove the particle.
                (*type_def).subtypes = null_mut();
            } else {
                // Assign the MG definition's {model group} to the particle's {term}.
                WXS_TYPE_PARTICLE_TERM!(type_def) = WXS_MODELGROUPDEF_MODEL!(group_def);

                if (*WXS_MODELGROUPDEF_MODEL!(group_def)).typ == XmlSchemaTypeType::XmlSchemaTypeAll
                {
                    // SPEC cos-all-limited (1.2)
                    // "1.2 the {term} property of a particle with
                    // {max occurs}=1 which is part of a pair which constitutes
                    // the {content type} of a complex type definition."
                    if (*WXS_TYPE_PARTICLE!(type_def)).max_occurs != 1 {
                        // TODO: error code
                        xml_schema_custom_err(
                            ctxt as XmlSchemaAbstractCtxtPtr,
                            XmlParserErrors::XmlSchemapCosAllLimited,
                            xml_schema_get_component_node((WXS_TYPE_PARTICLE!(type_def)) as _)
                                .map(|node| node.into()),
                            null_mut(),
                            "The particle's {max occurs} must be 1, since the reference resolves to an 'all' model group",
                            None,
                            None,
                        );
                    }
                }
            }
        }
    }
}

/// Resolves the referenced type definition component.
#[doc(alias = "xmlSchemaResolveAttrTypeReferences")]
unsafe fn xml_schema_resolve_attr_type_references(
    item: XmlSchemaAttributePtr,
    ctxt: XmlSchemaParserCtxtPtr,
) -> i32 {
    unsafe {
        // The simple type definition corresponding to the <simpleType> element
        // information item in the [children], if present, otherwise the simple
        // type definition `resolved` to by the `actual value` of the type
        // [attribute], if present, otherwise the `simple ur-type definition`.
        if (*item).flags & XML_SCHEMAS_ATTR_INTERNAL_RESOLVED != 0 {
            return 0;
        }
        (*item).flags |= XML_SCHEMAS_ATTR_INTERNAL_RESOLVED;
        if !(*item).subtypes.is_null() {
            return 0;
        }
        if !(*item).type_name.is_null() {
            let typ: XmlSchemaTypePtr = (*(*ctxt).schema).get_type(
                CStr::from_ptr((*item).type_name as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                (!(*item).type_ns.is_null())
                    .then(|| CStr::from_ptr((*item).type_ns as *const i8).to_string_lossy())
                    .as_deref(),
            );
            if typ.is_null() || !wxs_is_simple(typ) {
                xml_schema_pres_comp_attr_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapSrcResolve,
                    item as XmlSchemaBasicItemPtr,
                    (*item).node.map(|node| node.into()),
                    "type",
                    (!(*item).type_name.is_null())
                        .then(|| CStr::from_ptr((*item).type_name as *const i8).to_string_lossy())
                        .as_deref(),
                    (!(*item).type_ns.is_null())
                        .then(|| CStr::from_ptr((*item).type_ns as *const i8).to_string_lossy())
                        .as_deref(),
                    XmlSchemaTypeType::XmlSchemaTypeSimple,
                    None,
                );
                return (*ctxt).err;
            } else {
                (*item).subtypes = typ;
            }
        } else {
            // The type defaults to the xs:anySimpleType.
            (*item).subtypes =
                xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasAnySimpletype);
        }
        0
    }
}

/// Resolves the referenced attribute declaration.
#[doc(alias = "xmlSchemaResolveAttrUseReferences")]
unsafe fn xml_schema_resolve_attr_use_references(
    ause: XmlSchemaAttributeUsePtr,
    ctxt: XmlSchemaParserCtxtPtr,
) -> i32 {
    unsafe {
        if ctxt.is_null() || ause.is_null() {
            return -1;
        }
        if (*ause).attr_decl.is_null()
            || (*(*ause).attr_decl).typ != XmlSchemaTypeType::XmlSchemaExtraQNameRef
        {
            return 0;
        }
        {
            let refe: XmlSchemaQNameRefPtr = (*ause).attr_decl as XmlSchemaQNameRefPtr;

            // TODO: Evaluate, what errors could occur if the declaration is not found.
            (*ause).attr_decl = (*(*ctxt).schema).get_attribute_decl(
                CStr::from_ptr((*refe).name as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                (!(*refe).target_namespace.is_null())
                    .then(|| {
                        CStr::from_ptr((*refe).target_namespace as *const i8).to_string_lossy()
                    })
                    .as_deref(),
            );
            if (*ause).attr_decl.is_null() {
                xml_schema_pres_comp_attr_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapSrcResolve,
                    ause as XmlSchemaBasicItemPtr,
                    Some(XmlGenericNodePtr::from((*ause).node)),
                    "ref",
                    (!(*refe).name.is_null())
                        .then(|| CStr::from_ptr((*refe).name as *const i8).to_string_lossy())
                        .as_deref(),
                    (!(*refe).target_namespace.is_null())
                        .then(|| {
                            CStr::from_ptr((*refe).target_namespace as *const i8).to_string_lossy()
                        })
                        .as_deref(),
                    XmlSchemaTypeType::XmlSchemaTypeAttribute,
                    None,
                );
                return (*ctxt).err;
            }
        }
        0
    }
}

/// Resolves references to attribute group definitions.
#[doc(alias = "xmlSchemaResolveAttrGroupReferences")]
unsafe fn xml_schema_resolve_attr_group_references(
    refe: XmlSchemaQNameRefPtr,
    ctxt: XmlSchemaParserCtxtPtr,
) -> i32 {
    unsafe {
        if !(*refe).item.is_null() {
            return 0;
        }
        let group: XmlSchemaAttributeGroupPtr = (*(*ctxt).schema).get_attribute_group(
            CStr::from_ptr((*refe).name as *const i8)
                .to_string_lossy()
                .as_ref(),
            (!(*refe).target_namespace.is_null())
                .then(|| CStr::from_ptr((*refe).target_namespace as *const i8).to_string_lossy())
                .as_deref(),
        );
        if group.is_null() {
            xml_schema_pres_comp_attr_err(
                ctxt,
                XmlParserErrors::XmlSchemapSrcResolve,
                null_mut(),
                (*refe).node.map(|node| node.into()),
                "ref",
                (!(*refe).name.is_null())
                    .then(|| CStr::from_ptr((*refe).name as *const i8).to_string_lossy())
                    .as_deref(),
                (!(*refe).target_namespace.is_null())
                    .then(|| {
                        CStr::from_ptr((*refe).target_namespace as *const i8).to_string_lossy()
                    })
                    .as_deref(),
                (*refe).item_type,
                None,
            );
            return (*ctxt).err;
        }
        (*refe).item = group as XmlSchemaBasicItemPtr;
        0
    }
}

/// Resolves references of a model group's {particles} to
/// model group definitions and to element declarations.
#[doc(alias = "xmlSchemaResolveModelGroupParticleReferences")]
unsafe fn xml_schema_resolve_model_group_particle_references(
    ctxt: XmlSchemaParserCtxtPtr,
    mg: XmlSchemaModelGroupPtr,
) {
    unsafe {
        let mut particle: XmlSchemaParticlePtr = WXS_MODELGROUP_PARTICLE!(mg);
        let mut refe: XmlSchemaQNameRefPtr;
        let mut ref_item: XmlSchemaBasicItemPtr;

        // URGENT TODO: Test this.
        while !particle.is_null() {
            'next_particle: {
                if WXS_PARTICLE_TERM!(particle).is_null()
                    || (*WXS_PARTICLE_TERM!(particle)).typ
                        != XmlSchemaTypeType::XmlSchemaExtraQNameRef
                {
                    break 'next_particle;
                }
                refe = WXS_PARTICLE_TERM!(particle) as XmlSchemaQNameRefPtr;
                // Resolve the reference.
                // NULL the {term} by default.
                (*particle).children = null_mut();

                ref_item = (*(*ctxt).schema).get_named_component(
                    (*refe).item_type,
                    CStr::from_ptr((*refe).name as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    (!(*refe).target_namespace.is_null())
                        .then(|| {
                            CStr::from_ptr((*refe).target_namespace as *const i8).to_string_lossy()
                        })
                        .as_deref(),
                );
                if ref_item.is_null() {
                    xml_schema_pres_comp_attr_err(
                        ctxt,
                        XmlParserErrors::XmlSchemapSrcResolve,
                        null_mut(),
                        xml_schema_get_component_node(particle as _).map(|node| node.into()),
                        "ref",
                        (!(*refe).name.is_null())
                            .then(|| CStr::from_ptr((*refe).name as *const i8).to_string_lossy())
                            .as_deref(),
                        (!(*refe).target_namespace.is_null())
                            .then(|| {
                                CStr::from_ptr((*refe).target_namespace as *const i8)
                                    .to_string_lossy()
                            })
                            .as_deref(),
                        (*refe).item_type,
                        None,
                    );
                    // TODO: remove the particle.
                    break 'next_particle;
                }
                if (*ref_item).typ == XmlSchemaTypeType::XmlSchemaTypeGroup {
                    if WXS_MODELGROUPDEF_MODEL!(ref_item).is_null() {
                        // TODO: remove the particle.
                        break 'next_particle;
                    }

                    // NOTE that we will assign the model group definition
                    // itself to the "term" of the particle. This will ease
                    // the check for circular model group definitions. After
                    // that the "term" will be assigned the model group of the
                    // model group definition.
                    if (*WXS_MODELGROUPDEF_MODEL!(ref_item)).typ
                        == XmlSchemaTypeType::XmlSchemaTypeAll
                    {
                        // SPEC cos-all-limited (1)
                        // SPEC cos-all-limited (1.2)
                        // "It appears only as the value of one or both of the
                        // following properties:"
                        // (1.1) "the {model group} property of a model group
                        //        definition."
                        // (1.2) "the {term} property of a particle [... of] the "
                        // {content type} of a complex type definition."
                        /* TODO: error code */
                        xml_schema_custom_err(
                            ctxt as XmlSchemaAbstractCtxtPtr,
                            XmlParserErrors::XmlSchemapCosAllLimited,
                            xml_schema_get_component_node(particle as _).map(|node| node.into()),
                            null_mut(),
                            "A model group definition is referenced, but it contains an 'all' model group, which cannot be contained by model groups",
                            None,
                            None,
                        );
                        // TODO: remove the particle.
                        break 'next_particle;
                    }
                    (*particle).children = ref_item as XmlSchemaTreeItemPtr;
                } else {
                    // TODO: Are referenced element declarations the only
                    // other components we expect here?
                    (*particle).children = ref_item as XmlSchemaTreeItemPtr;
                }
            }
            // next_particle:
            particle = (*particle).next as XmlSchemaParticlePtr;
        }
    }
}

/// Resolve keyRef references to key/unique IDCs.
/// Schema Component Constraint:
///   Identity-constraint Definition Properties Correct (c-props-correct)
#[doc(alias = "xmlSchemaResolveIDCKeyReferences")]
unsafe fn xml_schema_resolve_idckey_references(
    idc: XmlSchemaIDCPtr,
    pctxt: XmlSchemaParserCtxtPtr,
) -> i32 {
    unsafe {
        if (*idc).typ != XmlSchemaTypeType::XmlSchemaTypeIDCKeyref {
            return 0;
        }
        if !(*(*idc).refe).name.is_null() {
            (*(*idc).refe).item = (*(*pctxt).schema).get_idc(
                CStr::from_ptr((*(*idc).refe).name as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                (!(*(*idc).refe).target_namespace.is_null())
                    .then(|| {
                        CStr::from_ptr((*(*idc).refe).target_namespace as *const i8)
                            .to_string_lossy()
                    })
                    .as_deref(),
            ) as XmlSchemaBasicItemPtr;
            if (*(*idc).refe).item.is_null() {
                // TODO: It is actually not an error to fail to resolve
                // at this stage. BUT we need to be that strict!
                xml_schema_pres_comp_attr_err(
                    pctxt,
                    XmlParserErrors::XmlSchemapSrcResolve,
                    idc as XmlSchemaBasicItemPtr,
                    Some(XmlGenericNodePtr::from((*idc).node)),
                    "refer",
                    (!(*(*idc).refe).name.is_null())
                        .then(|| CStr::from_ptr((*(*idc).refe).name as *const i8).to_string_lossy())
                        .as_deref(),
                    (!(*(*idc).refe).target_namespace.is_null())
                        .then(|| {
                            CStr::from_ptr((*(*idc).refe).target_namespace as *const i8)
                                .to_string_lossy()
                        })
                        .as_deref(),
                    XmlSchemaTypeType::XmlSchemaTypeIDCKey,
                    None,
                );
                return (*pctxt).err;
            } else if (*(*(*idc).refe).item).typ == XmlSchemaTypeType::XmlSchemaTypeIDCKeyref {
                // SPEC c-props-correct (1)
                xml_schema_custom_err(
                    pctxt as XmlSchemaAbstractCtxtPtr,
                    XmlParserErrors::XmlSchemapCPropsCorrect,
                    None,
                    idc as XmlSchemaBasicItemPtr,
                    "The keyref references a keyref",
                    None,
                    None,
                );
                (*(*idc).refe).item = null_mut();
                return (*pctxt).err;
            } else if (*idc).nb_fields != (*((*(*idc).refe).item as XmlSchemaIDCPtr)).nb_fields {
                let refer: XmlSchemaIDCPtr = (*(*idc).refe).item as XmlSchemaIDCPtr;
                let qname = xml_schema_format_qname(
                    Some(
                        CStr::from_ptr((*refer).target_namespace as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                    ),
                    Some(
                        CStr::from_ptr((*refer).name as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                    ),
                );
                // SPEC c-props-correct(2).
                // "If the {identity-constraint category} is keyref,
                // the cardinality of the {fields} must equal that of
                // the {fields} of the {referenced key}.
                xml_schema_custom_err(
                    pctxt as XmlSchemaAbstractCtxtPtr,
                    XmlParserErrors::XmlSchemapCPropsCorrect,
                    None,
                    idc as XmlSchemaBasicItemPtr,
                    format!("The cardinality of the keyref differs from the cardinality of the referenced key/unique '{qname}'").as_str(),
                    Some(&qname),
                    None
                );
                return (*pctxt).err;
            }
        }
        0
    }
}

unsafe fn xml_schema_resolve_attr_use_prohib_references(
    prohib: XmlSchemaAttributeUseProhibPtr,
    pctxt: XmlSchemaParserCtxtPtr,
) -> i32 {
    unsafe {
        if (*(*pctxt).schema)
            .get_attribute_decl(
                CStr::from_ptr((*prohib).name as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                (!(*prohib).target_namespace.is_null())
                    .then(|| {
                        CStr::from_ptr((*prohib).target_namespace as *const i8).to_string_lossy()
                    })
                    .as_deref(),
            )
            .is_null()
        {
            xml_schema_pres_comp_attr_err(
                pctxt,
                XmlParserErrors::XmlSchemapSrcResolve,
                null_mut(),
                (*prohib).node.map(|node| node.into()),
                "ref",
                (!(*prohib).name.is_null())
                    .then(|| CStr::from_ptr((*prohib).name as *const i8).to_string_lossy())
                    .as_deref(),
                (!(*prohib).target_namespace.is_null())
                    .then(|| {
                        CStr::from_ptr((*prohib).target_namespace as *const i8).to_string_lossy()
                    })
                    .as_deref(),
                XmlSchemaTypeType::XmlSchemaTypeAttribute,
                None,
            );
            return XmlParserErrors::XmlSchemapSrcResolve as i32;
        }
        0
    }
}

/// Checks st-props-correct (2) + ct-props-correct (3).
/// Circular type definitions are not allowed.
///
/// Returns XML_SCHEMAP_ST_PROPS_CORRECT_2 if the given type is circular, 0 otherwise.
#[doc(alias = "xmlSchemaCheckTypeDefCircularInternal")]
unsafe fn xml_schema_check_type_def_circular_internal(
    pctxt: XmlSchemaParserCtxtPtr,
    ctxt_type: XmlSchemaTypePtr,
    ancestor: XmlSchemaTypePtr,
) -> i32 {
    unsafe {
        if ancestor.is_null() || (*ancestor).typ == XmlSchemaTypeType::XmlSchemaTypeBasic {
            return 0;
        }

        if ctxt_type == ancestor {
            xml_schema_pcustom_err(
                pctxt,
                XmlParserErrors::XmlSchemapStPropsCorrect2,
                ctxt_type as XmlSchemaBasicItemPtr,
                xml_schema_get_component_node(ctxt_type as _).map(|node| node.into()),
                "The definition is circular",
                None,
            );
            return XmlParserErrors::XmlSchemapStPropsCorrect2 as i32;
        }
        if (*ancestor).flags & XML_SCHEMAS_TYPE_MARKED != 0 {
            // Avoid infinite recursion on circular types not yet checked.
            return 0;
        }
        (*ancestor).flags |= XML_SCHEMAS_TYPE_MARKED;
        let ret: i32 =
            xml_schema_check_type_def_circular_internal(pctxt, ctxt_type, (*ancestor).base_type);
        (*ancestor).flags ^= XML_SCHEMAS_TYPE_MARKED;
        ret
    }
}

/// Checks for circular type definitions.
#[doc(alias = "xmlSchemaCheckTypeDefCircular")]
unsafe fn xml_schema_check_type_def_circular(item: XmlSchemaTypePtr, ctxt: XmlSchemaParserCtxtPtr) {
    unsafe {
        if item.is_null()
            || (*item).typ == XmlSchemaTypeType::XmlSchemaTypeBasic
            || (*item).base_type.is_null()
        {
            return;
        }
        xml_schema_check_type_def_circular_internal(ctxt, item, (*item).base_type);
    }
}

/// This one is intended to be used by xmlSchemaCheckGroupDefCircular only.
///
/// Returns the particle with the circular model group definition reference, otherwise NULL.
#[doc(alias = "xmlSchemaGetCircModelGrDefRef")]
unsafe fn xml_schema_get_circ_model_gr_def_ref(
    group_def: XmlSchemaModelGroupDefPtr,
    mut particle: XmlSchemaTreeItemPtr,
) -> XmlSchemaTreeItemPtr {
    unsafe {
        let mut circ: XmlSchemaTreeItemPtr;
        let mut term: XmlSchemaTreeItemPtr;
        let mut gdef: XmlSchemaModelGroupDefPtr;

        while !particle.is_null() {
            term = (*particle).children;
            if term.is_null() {
                particle = (*particle).next;
                continue;
            }
            match (*term).typ {
                XmlSchemaTypeType::XmlSchemaTypeGroup => {
                    gdef = term as XmlSchemaModelGroupDefPtr;
                    if gdef == group_def {
                        return particle;
                    }
                    // Mark this model group definition to avoid infinite
                    // recursion on circular references not yet examined.
                    if (*gdef).flags & XML_SCHEMA_MODEL_GROUP_DEF_MARKED != 0 {
                        particle = (*particle).next;
                        continue;
                    }
                    if !(*gdef).children.is_null() {
                        (*gdef).flags |= XML_SCHEMA_MODEL_GROUP_DEF_MARKED;
                        circ = xml_schema_get_circ_model_gr_def_ref(
                            group_def,
                            (*(*gdef).children).children,
                        );
                        (*gdef).flags ^= XML_SCHEMA_MODEL_GROUP_DEF_MARKED;
                        if !circ.is_null() {
                            return circ;
                        }
                    }
                }
                XmlSchemaTypeType::XmlSchemaTypeSequence
                | XmlSchemaTypeType::XmlSchemaTypeChoice
                | XmlSchemaTypeType::XmlSchemaTypeAll => {
                    circ = xml_schema_get_circ_model_gr_def_ref(group_def, (*term).children);
                    if !circ.is_null() {
                        return circ;
                    }
                }
                _ => {}
            }
            particle = (*particle).next;
        }
        null_mut()
    }
}

/// Checks for circular references to model group definitions.
#[doc(alias = "xmlSchemaCheckGroupDefCircular")]
unsafe fn xml_schema_check_group_def_circular(
    item: XmlSchemaModelGroupDefPtr,
    ctxt: XmlSchemaParserCtxtPtr,
) {
    unsafe {
        // Schema Component Constraint: Model Group Correct
        // 2 Circular groups are disallowed. That is, within the {particles}
        // of a group there must not be at any depth a particle whose {term}
        // is the group itself.
        if item.is_null()
            || (*item).typ != XmlSchemaTypeType::XmlSchemaTypeGroup
            || (*item).children.is_null()
        {
            return;
        }
        {
            let circ: XmlSchemaTreeItemPtr =
                xml_schema_get_circ_model_gr_def_ref(item, (*(*item).children).children);
            if !circ.is_null() {
                let q1 = xml_schema_format_qname(
                    Some(
                        CStr::from_ptr((*item).target_namespace as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                    ),
                    Some(
                        CStr::from_ptr((*item).name as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                    ),
                );
                // TODO: The error report is not adequate: this constraint
                // is defined for model groups but not definitions, but since
                // there cannot be any circular model groups without a model group
                // definition (if not using a construction API), we check those
                // definitions only.
                xml_schema_pcustom_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapMgPropsCorrect2,
                    null_mut(),
                    xml_schema_get_component_node(circ as _).map(|node| node.into()),
                    format!("Circular reference to the model group definition '{q1}' defined")
                        .as_str(),
                    Some(&q1),
                );
                // NOTE: We will cut the reference to avoid further
                // confusion of the processor. This is a fatal error.
                (*circ).children = null_mut();
            }
        }
    }
}

/// This one is intended to be used by xmlSchemaCheckAttrGroupCircular only.
///
/// Returns the circular attribute group reference, otherwise NULL.
#[doc(alias = "xmlSchemaCheckAttrGroupCircularRecur")]
unsafe fn xml_schema_check_attr_group_circular_recur(
    ctxt_gr: XmlSchemaAttributeGroupPtr,
    list: XmlSchemaItemListPtr<*mut c_void>,
) -> XmlSchemaQNameRefPtr {
    unsafe {
        let mut gr: XmlSchemaAttributeGroupPtr;
        let mut circ: XmlSchemaQNameRefPtr;
        // We will search for an attribute group reference which
        // references the context attribute group.
        for refe in (*list)
            .items
            .iter()
            .map(|&refe| refe as XmlSchemaQNameRefPtr)
        {
            if (*refe).typ == XmlSchemaTypeType::XmlSchemaExtraQNameRef
                && (*refe).item_type == XmlSchemaTypeType::XmlSchemaTypeAttributeGroup
                && !(*refe).item.is_null()
            {
                gr = (*refe).item as XmlSchemaAttributeGroupPtr;
                if gr == ctxt_gr {
                    return refe;
                }
                if (*gr).flags & XML_SCHEMAS_ATTRGROUP_MARKED != 0 {
                    continue;
                }
                // Mark as visited to avoid infinite recursion on
                // circular references not yet examined.
                if !(*gr).attr_uses.is_null() && (*gr).flags & XML_SCHEMAS_ATTRGROUP_HAS_REFS != 0 {
                    (*gr).flags |= XML_SCHEMAS_ATTRGROUP_MARKED;
                    circ = xml_schema_check_attr_group_circular_recur(
                        ctxt_gr,
                        (*gr).attr_uses as XmlSchemaItemListPtr<*mut c_void>,
                    );
                    (*gr).flags ^= XML_SCHEMAS_ATTRGROUP_MARKED;
                    if !circ.is_null() {
                        return circ;
                    }
                }
            }
        }
        null_mut()
    }
}

/// Checks for circular references of attribute groups.
#[doc(alias = "xmlSchemaCheckAttrGroupCircular")]
unsafe fn xml_schema_check_attr_group_circular(
    attr_gr: XmlSchemaAttributeGroupPtr,
    ctxt: XmlSchemaParserCtxtPtr,
) -> i32 {
    unsafe {
        // Schema Representation Constraint:
        // Attribute Group Definition Representation OK
        // 3 Circular group reference is disallowed outside <redefine>.
        // That is, unless this element information item's parent is
        // <redefine>, then among the [children], if any, there must
        // not be an <attributeGroup> with ref [attribute] which resolves
        // to the component corresponding to this <attributeGroup>. Indirect
        // circularity is also ruled out. That is, when QName resolution
        // (Schema Document) ($3.15.3) is applied to a `QName` arising from
        // any <attributeGroup>s with a ref [attribute] among the [children],
        // it must not be the case that a `QName` is encountered at any depth
        // which resolves to the component corresponding to this <attributeGroup>.
        if (*attr_gr).attr_uses.is_null() || (*attr_gr).flags & XML_SCHEMAS_ATTRGROUP_HAS_REFS == 0
        {
            return 0;
        } else {
            let circ: XmlSchemaQNameRefPtr = xml_schema_check_attr_group_circular_recur(
                attr_gr,
                (*attr_gr).attr_uses as XmlSchemaItemListPtr<*mut c_void>,
            );
            if !circ.is_null() {
                let q1 = xml_schema_get_component_qname(attr_gr as _);
                // TODO: Report the referenced attr group as QName.
                xml_schema_pcustom_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapSrcAttributeGroup3,
                    null_mut(),
                    xml_schema_get_component_node((circ as XmlSchemaBasicItemPtr) as _)
                        .map(|node| node.into()),
                    format!("Circular reference to the attribute group '{q1}' defined").as_str(),
                    Some(&q1),
                );
                // NOTE: We will cut the reference to avoid further confusion of the processor.
                // BADSPEC TODO: The spec should define how to process in this case.
                (*circ).item = null_mut();
                return (*ctxt).err;
            }
        }
        0
    }
}

/// Assigns the model group of model group definitions to the "term"
/// of the referencing particle.
/// In xmlSchemaResolveModelGroupParticleReferences the model group
/// definitions were assigned to the "term", since needed for the
/// circularity check.
///
/// Schema Component Constraint:
///     All Group Limited (cos-all-limited) (1.2)
#[doc(alias = "xmlSchemaModelGroupToModelGroupDefFixup")]
unsafe fn xml_schema_model_group_to_model_group_def_fixup(
    _ctxt: XmlSchemaParserCtxtPtr,
    mg: XmlSchemaModelGroupPtr,
) {
    unsafe {
        let mut particle: XmlSchemaParticlePtr = WXS_MODELGROUP_PARTICLE!(mg);

        while !particle.is_null() {
            if WXS_PARTICLE_TERM!(particle).is_null()
                || (*WXS_PARTICLE_TERM!(particle)).typ != XmlSchemaTypeType::XmlSchemaTypeGroup
            {
                particle = (*particle).next as XmlSchemaParticlePtr;
                continue;
            }
            if WXS_MODELGROUPDEF_MODEL!(WXS_PARTICLE_TERM!(particle)).is_null() {
                // TODO: Remove the particle.
                WXS_PARTICLE_TERM!(particle) = null_mut();
                particle = (*particle).next as XmlSchemaParticlePtr;
                continue;
            }
            // Assign the model group to the {term} of the particle.
            WXS_PARTICLE_TERM!(particle) =
                WXS_MODELGROUPDEF_MODEL!(WXS_PARTICLE_TERM!(particle)) as XmlSchemaTreeItemPtr;

            particle = (*particle).next as XmlSchemaParticlePtr;
        }
    }
}

/// Clones the namespace constraints of source and assigns them to dest.
/// Returns -1 on internal error, 0 otherwise.
#[doc(alias = "xmlSchemaCloneWildcardNsConstraints")]
unsafe fn xml_schema_clone_wildcard_ns_constraints(
    ctxt: XmlSchemaParserCtxtPtr,
    dest: XmlSchemaWildcardPtr,
    source: XmlSchemaWildcardPtr,
) -> i32 {
    unsafe {
        let mut cur: XmlSchemaWildcardNsPtr;
        let mut tmp: XmlSchemaWildcardNsPtr;
        let mut last: XmlSchemaWildcardNsPtr;

        if source.is_null() || dest.is_null() {
            return -1;
        }
        (*dest).any = (*source).any;
        cur = (*source).ns_set;
        last = null_mut();
        while !cur.is_null() {
            tmp = xml_schema_new_wildcard_ns_constraint(ctxt);
            if tmp.is_null() {
                return -1;
            }
            (*tmp).value = (*cur).value;
            if last.is_null() {
                (*dest).ns_set = tmp;
            } else {
                (*last).next = tmp;
            }
            last = tmp;
            cur = (*cur).next;
        }
        if !(*dest).neg_ns_set.is_null() {
            xml_schema_free_wildcard_ns_set((*dest).neg_ns_set);
        }
        if !(*source).neg_ns_set.is_null() {
            (*dest).neg_ns_set = xml_schema_new_wildcard_ns_constraint(ctxt);
            if (*dest).neg_ns_set.is_null() {
                return -1;
            }
            (*(*dest).neg_ns_set).value = (*(*source).neg_ns_set).value;
        } else {
            (*dest).neg_ns_set = null_mut();
        }
        0
    }
}

/// Intersects the namespace constraints of the given wildcards.
/// @completeWild will hold the resulting intersection.
/// Returns a positive error code on failure, -1 in case of an
/// internal error, 0 otherwise.
#[doc(alias = "xmlSchemaIntersectWildcards")]
unsafe fn xml_schema_intersect_wildcards(
    ctxt: XmlSchemaParserCtxtPtr,
    complete_wild: XmlSchemaWildcardPtr,
    cur_wild: XmlSchemaWildcardPtr,
) -> i32 {
    unsafe {
        let mut cur: XmlSchemaWildcardNsPtr;
        let mut cur_b: XmlSchemaWildcardNsPtr;
        let mut prev: XmlSchemaWildcardNsPtr;
        let mut tmp: XmlSchemaWildcardNsPtr;

        // 1 If O1 and O2 are the same value, then that value must be the value.
        if (*complete_wild).any == (*cur_wild).any
            && (*complete_wild).ns_set.is_null() == (*cur_wild).ns_set.is_null()
            && (*complete_wild).neg_ns_set.is_null() == (*cur_wild).neg_ns_set.is_null()
            && ((*complete_wild).neg_ns_set.is_null()
                || (*(*complete_wild).neg_ns_set).value == (*(*cur_wild).neg_ns_set).value)
        {
            if !(*complete_wild).ns_set.is_null() {
                let mut found: i32 = 0;

                // Check equality of sets.
                cur = (*complete_wild).ns_set;
                while !cur.is_null() {
                    found = 0;
                    cur_b = (*cur_wild).ns_set;
                    while !cur_b.is_null() {
                        if (*cur).value == (*cur_b).value {
                            found = 1;
                            break;
                        }
                        cur_b = (*cur_b).next;
                    }
                    if found == 0 {
                        break;
                    }
                    cur = (*cur).next;
                }
                if found != 0 {
                    return 0;
                }
            } else {
                return 0;
            }
        }
        // 2 If either O1 or O2 is any, then the other must be the value.
        if (*complete_wild).any != (*cur_wild).any && (*complete_wild).any != 0 {
            if xml_schema_clone_wildcard_ns_constraints(ctxt, complete_wild, cur_wild) == -1 {
                return -1;
            }
            return 0;
        }
        // 3 If either O1 or O2 is a pair of not and a value (a namespace
        // name or `absent`) and the other is a set of (namespace names or
        // `absent`), then that set, minus the negated value if it was in
        // the set, minus `absent` if it was in the set, must be the value.
        if (!(*complete_wild).neg_ns_set.is_null() && !(*cur_wild).ns_set.is_null())
            || (!(*cur_wild).neg_ns_set.is_null() && !(*complete_wild).ns_set.is_null())
        {
            let neg: *const XmlChar;

            if (*complete_wild).ns_set.is_null() {
                neg = (*(*complete_wild).neg_ns_set).value;
                if xml_schema_clone_wildcard_ns_constraints(ctxt, complete_wild, cur_wild) == -1 {
                    return -1;
                }
            } else {
                neg = (*(*cur_wild).neg_ns_set).value;
            }
            // Remove absent and negated.
            prev = null_mut();
            cur = (*complete_wild).ns_set;
            while !cur.is_null() {
                if (*cur).value.is_null() {
                    if prev.is_null() {
                        (*complete_wild).ns_set = (*cur).next;
                    } else {
                        (*prev).next = (*cur).next;
                    }
                    xml_free(cur as _);
                    break;
                }
                prev = cur;
                cur = (*cur).next;
            }
            if !neg.is_null() {
                prev = null_mut();
                cur = (*complete_wild).ns_set;
                while !cur.is_null() {
                    if (*cur).value == neg {
                        if prev.is_null() {
                            (*complete_wild).ns_set = (*cur).next;
                        } else {
                            (*prev).next = (*cur).next;
                        }
                        xml_free(cur as _);
                        break;
                    }
                    prev = cur;
                    cur = (*cur).next;
                }
            }

            return 0;
        }
        // 4 If both O1 and O2 are sets of (namespace names or `absent`),
        // then the intersection of those sets must be the value.
        if !(*complete_wild).ns_set.is_null() && !(*cur_wild).ns_set.is_null() {
            let mut found: i32;

            cur = (*complete_wild).ns_set;
            prev = null_mut();
            while !cur.is_null() {
                found = 0;
                cur_b = (*cur_wild).ns_set;
                while !cur_b.is_null() {
                    if (*cur).value == (*cur_b).value {
                        found = 1;
                        break;
                    }
                    cur_b = (*cur_b).next;
                }
                if found == 0 {
                    if prev.is_null() {
                        (*complete_wild).ns_set = (*cur).next;
                    } else {
                        (*prev).next = (*cur).next;
                    }
                    tmp = (*cur).next;
                    xml_free(cur as _);
                    cur = tmp;
                    continue;
                }
                prev = cur;
                cur = (*cur).next;
            }

            return 0;
        }
        // 5 If the two are negations of different namespace names,
        // then the intersection is not expressible
        if !(*complete_wild).neg_ns_set.is_null()
            && !(*cur_wild).neg_ns_set.is_null()
            && (*(*complete_wild).neg_ns_set).value != (*(*cur_wild).neg_ns_set).value
            && !(*(*complete_wild).neg_ns_set).value.is_null()
            && !(*(*cur_wild).neg_ns_set).value.is_null()
        {
            xml_schema_perr(
                ctxt,
                (*complete_wild).node.map(|node| node.into()),
                XmlParserErrors::XmlSchemapIntersectionNotExpressible,
                "The intersection of the wildcard is not expressible.\n",
                None,
                None,
            );
            return XmlParserErrors::XmlSchemapIntersectionNotExpressible as i32;
        }
        // 6 If the one is a negation of a namespace name and the other
        // is a negation of `absent`, then the one which is the negation
        // of a namespace name must be the value.
        if !(*complete_wild).neg_ns_set.is_null()
            && !(*cur_wild).neg_ns_set.is_null()
            && (*(*complete_wild).neg_ns_set).value != (*(*cur_wild).neg_ns_set).value
            && (*(*complete_wild).neg_ns_set).value.is_null()
        {
            (*(*complete_wild).neg_ns_set).value = (*(*cur_wild).neg_ns_set).value;
        }
        0
    }
}

/// Substitutes contained attribute group references
/// for their attribute uses. Wildcards are intersected.
/// Attribute use prohibitions are removed from the list
/// and returned via the @prohibs list.
/// Pointlessness of attr. prohibs, if a matching attr. decl is existent a well, are checked.
#[doc(alias = "xmlSchemaExpandAttributeGroupRefs")]
unsafe fn xml_schema_expand_attribute_group_refs(
    pctxt: XmlSchemaParserCtxtPtr,
    item: XmlSchemaBasicItemPtr,
    complete_wild: *mut XmlSchemaWildcardPtr,
    list: XmlSchemaItemListPtr<*mut c_void>,
    prohibs: XmlSchemaItemListPtr<XmlSchemaAttributeUseProhibPtr>,
) -> i32 {
    unsafe {
        let mut gr: XmlSchemaAttributeGroupPtr;
        let mut sublist: XmlSchemaItemListPtr<*mut c_void>;
        let mut created: i32 = (!(*complete_wild).is_null()) as i32;

        if !prohibs.is_null() {
            (*prohibs).items.clear();
        }

        let mut i = 0;
        'main: while i < (*list).items.len() {
            let using = (*list).items[i] as XmlSchemaAttributeUsePtr;

            if (*using).typ == XmlSchemaTypeType::XmlSchemaExtraAttrUseProhib {
                if prohibs.is_null() {
                    PERROR_INT!(
                        pctxt,
                        "xmlSchemaExpandAttributeGroupRefs",
                        "unexpected attr prohibition found"
                    );
                    return -1;
                }
                // Remove from attribute uses.
                if (*list).remove(i) == -1 {
                    return -1;
                }
                // Note that duplicate prohibitions were already
                // handled at parsing time.
                // Add to list of prohibitions.
                (*prohibs).push(using as _);
                continue 'main;
            }
            if (*using).typ == XmlSchemaTypeType::XmlSchemaExtraQNameRef
                && (*(using as XmlSchemaQNameRefPtr)).item_type
                    == XmlSchemaTypeType::XmlSchemaTypeAttributeGroup
            {
                if (*(using as XmlSchemaQNameRefPtr)).item.is_null() {
                    return -1;
                }
                gr = (*(using as XmlSchemaQNameRefPtr)).item as XmlSchemaAttributeGroupPtr;
                // Expand the referenced attr. group.
                // TODO: remove this, this is done in a previous step, so
                // already done here.
                if (*gr).flags & XML_SCHEMAS_ATTRGROUP_WILDCARD_BUILDED == 0
                    && xml_schema_attribute_group_expand_refs(pctxt, gr) == -1
                {
                    return -1;
                }
                // Build the 'complete' wildcard; i.e. intersect multiple wildcards.
                if !(*gr).attribute_wildcard.is_null() {
                    if (*complete_wild).is_null() {
                        *complete_wild = (*gr).attribute_wildcard;
                    } else {
                        if created == 0 {
                            // Copy the first encountered wildcard as context,
                            // except for the annotation.
                            //
                            // Although the complete wildcard might not correspond
                            // to any node in the schema, we will anchor it on
                            // the node of the owner component.
                            let tmp_wild: XmlSchemaWildcardPtr = xml_schema_add_wildcard(
                                pctxt,
                                (*pctxt).schema,
                                XmlSchemaTypeType::XmlSchemaTypeAnyAttribute,
                                xml_schema_get_component_node(item as _),
                            );
                            if tmp_wild.is_null() {
                                return -1;
                            }
                            if xml_schema_clone_wildcard_ns_constraints(
                                pctxt,
                                tmp_wild,
                                *complete_wild,
                            ) == -1
                            {
                                return -1;
                            }
                            (*tmp_wild).process_contents = (*(*complete_wild)).process_contents;
                            *complete_wild = tmp_wild;
                            created = 1;
                        }

                        if xml_schema_intersect_wildcards(
                            pctxt,
                            *complete_wild,
                            (*gr).attribute_wildcard,
                        ) == -1
                        {
                            return -1;
                        }
                    }
                }
                // Just remove the reference if the referenced group does not
                // contain any attribute uses.
                sublist = (*gr).attr_uses as XmlSchemaItemListPtr<*mut c_void>;
                if sublist.is_null() || (*sublist).items.is_empty() {
                    if (*list).remove(i) == -1 {
                        return -1;
                    }
                    continue 'main;
                }
                // Add the attribute uses.
                (*list).items[i] = (*sublist).items[0];
                if (*sublist).items.len() != 1 {
                    for j in 1..(*sublist).items.len() {
                        i += 1;
                        if (*list).insert((*sublist).items[j], i) == -1 {
                            return -1;
                        }
                    }
                }
            }

            i += 1;
        }
        // Handle pointless prohibitions of declared attributes.
        if !prohibs.is_null() && !(*prohibs).items.is_empty() && !(*list).items.is_empty() {
            for &prohib in (*prohibs).items.iter().rev() {
                for using in (*list)
                    .items
                    .iter()
                    .map(|&using| using as XmlSchemaAttributeUsePtr)
                {
                    if (*prohib).name == WXS_ATTRUSE_DECL_NAME!(using)
                        && (*prohib).target_namespace == WXS_ATTRUSE_DECL_TNS!(using)
                    {
                        let qname = xml_schema_format_qname(
                            Some(
                                CStr::from_ptr((*prohib).target_namespace as *const i8)
                                    .to_string_lossy()
                                    .as_ref(),
                            ),
                            Some(
                                CStr::from_ptr((*prohib).name as *const i8)
                                    .to_string_lossy()
                                    .as_ref(),
                            ),
                        );

                        xml_schema_custom_warning(
                            pctxt as XmlSchemaAbstractCtxtPtr,
                            XmlParserErrors::XmlSchemapWarnAttrPointlessProh,
                            (*prohib).node.map(|node| node.into()),
                            null_mut(),
                            format!("Skipping pointless attribute use prohibition '{qname}', since a corresponding attribute use exists already in the type definition").as_str(),
                            Some(&qname),
                            None,
                            None
                        );
                        // Remove the prohibition.
                        if (*prohibs).remove(i) == -1 {
                            return -1;
                        }
                        break;
                    }
                }
            }
        }
        0
    }
}

/// Computation of:
/// {attribute uses} property
/// {attribute wildcard} property
///
/// Substitutes contained attribute group references
/// for their attribute uses. Wildcards are intersected.
#[doc(alias = "xmlSchemaAttributeGroupExpandRefs")]
unsafe fn xml_schema_attribute_group_expand_refs(
    pctxt: XmlSchemaParserCtxtPtr,
    attr_gr: XmlSchemaAttributeGroupPtr,
) -> i32 {
    unsafe {
        if (*attr_gr).attr_uses.is_null()
            || (*attr_gr).flags & XML_SCHEMAS_ATTRGROUP_WILDCARD_BUILDED != 0
        {
            return 0;
        }

        (*attr_gr).flags |= XML_SCHEMAS_ATTRGROUP_WILDCARD_BUILDED;
        if xml_schema_expand_attribute_group_refs(
            pctxt,
            attr_gr as XmlSchemaBasicItemPtr,
            &raw mut (*attr_gr).attribute_wildcard,
            (*attr_gr).attr_uses as _,
            null_mut(),
        ) == -1
        {
            return -1;
        }
        0
    }
}

unsafe fn xml_schema_fixup_simple_type_stage_one(
    pctxt: XmlSchemaParserCtxtPtr,
    typ: XmlSchemaTypePtr,
) -> i32 {
    unsafe {
        if (*typ).typ != XmlSchemaTypeType::XmlSchemaTypeSimple {
            return 0;
        }
        if !WXS_IS_TYPE_NOT_FIXED_1!(typ) {
            return 0;
        }
        (*typ).flags |= XML_SCHEMAS_TYPE_FIXUP_1;

        if (*typ).wxs_is_list() {
            // Corresponds to <simpleType><list>...
            if (*typ).subtypes.is_null() {
                // This one is really needed, so get out.
                PERROR_INT!(
                    pctxt,
                    "xmlSchemaFixupSimpleTypeStageOne",
                    "list type has no item-type assigned"
                );
                return -1;
            }
        } else if (*typ).wxs_is_union() {
            // Corresponds to <simpleType><union>...
            if (*typ).member_types.is_null() {
                // This one is really needed, so get out.
                PERROR_INT!(
                    pctxt,
                    "xmlSchemaFixupSimpleTypeStageOne",
                    "union type has no member-types assigned"
                );
                return -1;
            }
        } else {
            // Corresponds to <simpleType><restriction>...
            if (*typ).base_type.is_null() {
                PERROR_INT!(
                    pctxt,
                    "xmlSchemaFixupSimpleTypeStageOne",
                    "type has no base-type assigned"
                );
                return -1;
            }
            if WXS_IS_TYPE_NOT_FIXED_1!((*typ).base_type)
                && xml_schema_fixup_simple_type_stage_one(pctxt, (*typ).base_type) == -1
            {
                return -1;
            }
            // Variety
            // If the <restriction> alternative is chosen, then the
            // {variety} of the {base type definition}.
            if (*(*typ).base_type).wxs_is_atomic() {
                (*typ).flags |= XML_SCHEMAS_TYPE_VARIETY_ATOMIC;
            } else if (*(*typ).base_type).wxs_is_list() {
                (*typ).flags |= XML_SCHEMAS_TYPE_VARIETY_LIST;
                // Inherit the itemType.
                (*typ).subtypes = (*(*typ).base_type).subtypes;
            } else if (*(*typ).base_type).wxs_is_union() {
                (*typ).flags |= XML_SCHEMAS_TYPE_VARIETY_UNION;
                // NOTE that we won't assign the memberTypes of the base,
                // since this will make trouble when freeing them; we will
                // use a lookup function to access them instead.
            }
        }
        0
    }
}

// Simple Type Definition Representation OK (src-simple-type) 4
//
// "4 Circular union type definition is disallowed. That is, if the
// <union> alternative is chosen, there must not be any entries in the
// memberTypes [attribute] at any depth which resolve to the component
// corresponding to the <simpleType>."
//
// Note that this should work on the *representation* of a component,
// thus assumes any union types in the member types not being yet
// substituted. At this stage we need the variety of the types
// to be already computed.
unsafe fn xml_schema_check_union_type_def_circular_recur(
    pctxt: XmlSchemaParserCtxtPtr,
    ctx_type: XmlSchemaTypePtr,
    members: XmlSchemaTypeLinkPtr,
) -> i32 {
    unsafe {
        let mut member: XmlSchemaTypeLinkPtr;
        let mut member_type: XmlSchemaTypePtr;

        member = members;
        while !member.is_null() {
            member_type = (*member).typ;
            while !member_type.is_null()
                && (*member_type).typ != XmlSchemaTypeType::XmlSchemaTypeBasic
            {
                if member_type == ctx_type {
                    xml_schema_pcustom_err(
                        pctxt,
                        XmlParserErrors::XmlSchemapSrcSimpleType4,
                        ctx_type as XmlSchemaBasicItemPtr,
                        None,
                        "The union type definition is circular",
                        None,
                    );
                    return XmlParserErrors::XmlSchemapSrcSimpleType4 as i32;
                }
                if (*member_type).wxs_is_union()
                    && (*member_type).flags & XML_SCHEMAS_TYPE_MARKED == 0
                {
                    (*member_type).flags |= XML_SCHEMAS_TYPE_MARKED;
                    let res: i32 = xml_schema_check_union_type_def_circular_recur(
                        pctxt,
                        ctx_type,
                        xml_schema_get_union_simple_type_member_types(member_type),
                    );
                    (*member_type).flags ^= XML_SCHEMAS_TYPE_MARKED;
                    if res != 0 {
                        return res;
                    }
                }
                member_type = (*member_type).base_type;
            }
            member = (*member).next;
        }
        0
    }
}

unsafe fn xml_schema_check_union_type_def_circular(
    pctxt: XmlSchemaParserCtxtPtr,
    typ: XmlSchemaTypePtr,
) -> i32 {
    unsafe {
        if !(*typ).wxs_is_union() {
            return 0;
        }
        xml_schema_check_union_type_def_circular_recur(pctxt, typ, (*typ).member_types)
    }
}

/// Unions the namespace constraints of the given wildcards.
/// @completeWild will hold the resulting union.
/// Returns a positive error code on failure, -1 in case of an internal error, 0 otherwise.
#[doc(alias = "xmlSchemaUnionWildcards")]
unsafe fn xml_schema_union_wildcards(
    ctxt: XmlSchemaParserCtxtPtr,
    complete_wild: XmlSchemaWildcardPtr,
    cur_wild: XmlSchemaWildcardPtr,
) -> i32 {
    unsafe {
        let mut cur: XmlSchemaWildcardNsPtr;
        let mut cur_b: XmlSchemaWildcardNsPtr;
        let mut tmp: XmlSchemaWildcardNsPtr;

        // 1 If O1 and O2 are the same value, then that value must be the value.
        if ((*complete_wild).any == (*cur_wild).any
            && (*complete_wild).ns_set.is_null() == (*cur_wild).ns_set.is_null()
            && (*complete_wild).neg_ns_set.is_null() == (*cur_wild).neg_ns_set.is_null())
            && ((*complete_wild).neg_ns_set.is_null()
                || (*(*complete_wild).neg_ns_set).value == (*(*cur_wild).neg_ns_set).value)
        {
            if !(*complete_wild).ns_set.is_null() {
                let mut found: i32 = 0;

                // Check equality of sets.
                cur = (*complete_wild).ns_set;
                while !cur.is_null() {
                    found = 0;
                    cur_b = (*cur_wild).ns_set;
                    while !cur_b.is_null() {
                        if (*cur).value == (*cur_b).value {
                            found = 1;
                            break;
                        }
                        cur_b = (*cur_b).next;
                    }
                    if found == 0 {
                        break;
                    }
                    cur = (*cur).next;
                }
                if found != 0 {
                    return 0;
                }
            } else {
                return 0;
            }
        }
        // 2 If either O1 or O2 is any, then any must be the value
        if (*complete_wild).any != (*cur_wild).any {
            if (*complete_wild).any == 0 {
                (*complete_wild).any = 1;
                if !(*complete_wild).ns_set.is_null() {
                    xml_schema_free_wildcard_ns_set((*complete_wild).ns_set);
                    (*complete_wild).ns_set = null_mut();
                }
                if !(*complete_wild).neg_ns_set.is_null() {
                    xml_free((*complete_wild).neg_ns_set as _);
                    (*complete_wild).neg_ns_set = null_mut();
                }
            }
            return 0;
        }
        // 3 If both O1 and O2 are sets of (namespace names or `absent`),
        // then the union of those sets must be the value.
        if !(*complete_wild).ns_set.is_null() && !(*cur_wild).ns_set.is_null() {
            let mut found: i32;

            cur = (*cur_wild).ns_set;
            let start: XmlSchemaWildcardNsPtr = (*complete_wild).ns_set;
            while !cur.is_null() {
                found = 0;
                cur_b = start;
                while !cur_b.is_null() {
                    if (*cur).value == (*cur_b).value {
                        found = 1;
                        break;
                    }
                    cur_b = (*cur_b).next;
                }
                if found == 0 {
                    tmp = xml_schema_new_wildcard_ns_constraint(ctxt);
                    if tmp.is_null() {
                        return -1;
                    }
                    (*tmp).value = (*cur).value;
                    (*tmp).next = (*complete_wild).ns_set;
                    (*complete_wild).ns_set = tmp;
                }
                cur = (*cur).next;
            }

            return 0;
        }
        // 4 If the two are negations of different values (namespace names or `absent`),
        // then a pair of not and `absent` must be the value.
        if !(*complete_wild).neg_ns_set.is_null()
            && !(*cur_wild).neg_ns_set.is_null()
            && (*(*complete_wild).neg_ns_set).value != (*(*cur_wild).neg_ns_set).value
        {
            (*(*complete_wild).neg_ns_set).value = null_mut();

            return 0;
        }
        // 5.
        if (!(*complete_wild).neg_ns_set.is_null()
            && !(*(*complete_wild).neg_ns_set).value.is_null()
            && !(*cur_wild).ns_set.is_null())
            || (!(*cur_wild).neg_ns_set.is_null()
                && !(*(*cur_wild).neg_ns_set).value.is_null()
                && !(*complete_wild).ns_set.is_null())
        {
            let mut ns_found: i32;
            let mut absent_found: i32 = 0;

            if !(*complete_wild).ns_set.is_null() {
                cur = (*complete_wild).ns_set;
                cur_b = (*cur_wild).neg_ns_set;
            } else {
                cur = (*cur_wild).ns_set;
                cur_b = (*complete_wild).neg_ns_set;
            }
            ns_found = 0;
            while !cur.is_null() {
                if (*cur).value.is_null() {
                    absent_found = 1;
                } else if (*cur).value == (*cur_b).value {
                    ns_found = 1;
                }
                if ns_found != 0 && absent_found != 0 {
                    break;
                }
                cur = (*cur).next;
            }

            if ns_found != 0 && absent_found != 0 {
                // 5.1 If the set S includes both the negated namespace
                // name and `absent`, then any must be the value.
                (*complete_wild).any = 1;
                if !(*complete_wild).ns_set.is_null() {
                    xml_schema_free_wildcard_ns_set((*complete_wild).ns_set);
                    (*complete_wild).ns_set = null_mut();
                }
                if !(*complete_wild).neg_ns_set.is_null() {
                    xml_free((*complete_wild).neg_ns_set as _);
                    (*complete_wild).neg_ns_set = null_mut();
                }
            } else if ns_found != 0 && absent_found == 0 {
                // 5.2 If the set S includes the negated namespace name
                // but not `absent`, then a pair of not and `absent` must be the value.
                if !(*complete_wild).ns_set.is_null() {
                    xml_schema_free_wildcard_ns_set((*complete_wild).ns_set);
                    (*complete_wild).ns_set = null_mut();
                }
                if (*complete_wild).neg_ns_set.is_null() {
                    (*complete_wild).neg_ns_set = xml_schema_new_wildcard_ns_constraint(ctxt);
                    if (*complete_wild).neg_ns_set.is_null() {
                        return -1;
                    }
                }
                (*(*complete_wild).neg_ns_set).value = null_mut();
            } else if ns_found == 0 && absent_found != 0 {
                // 5.3 If the set S includes `absent` but not the negated
                // namespace name, then the union is not expressible.
                xml_schema_perr(
                    ctxt,
                    (*complete_wild).node.map(|node| node.into()),
                    XmlParserErrors::XmlSchemapUnionNotExpressible,
                    "The union of the wildcard is not expressible.\n",
                    None,
                    None,
                );
                return XmlParserErrors::XmlSchemapUnionNotExpressible as i32;
            } else if ns_found == 0 && absent_found == 0 {
                // 5.4 If the set S does not include either the negated namespace
                // name or `absent`, then whichever of O1 or O2 is a pair of not
                // and a namespace name must be the value.
                if (*complete_wild).neg_ns_set.is_null() {
                    if !(*complete_wild).ns_set.is_null() {
                        xml_schema_free_wildcard_ns_set((*complete_wild).ns_set);
                        (*complete_wild).ns_set = null_mut();
                    }
                    (*complete_wild).neg_ns_set = xml_schema_new_wildcard_ns_constraint(ctxt);
                    if (*complete_wild).neg_ns_set.is_null() {
                        return -1;
                    }
                    (*(*complete_wild).neg_ns_set).value = (*(*cur_wild).neg_ns_set).value;
                }
            }
            return 0;
        }
        // 6.
        if (!(*complete_wild).neg_ns_set.is_null()
            && (*(*complete_wild).neg_ns_set).value.is_null()
            && !(*cur_wild).ns_set.is_null())
            || (!(*cur_wild).neg_ns_set.is_null()
                && (*(*cur_wild).neg_ns_set).value.is_null()
                && !(*complete_wild).ns_set.is_null())
        {
            if !(*complete_wild).ns_set.is_null() {
                cur = (*complete_wild).ns_set;
            } else {
                cur = (*cur_wild).ns_set;
            }
            while !cur.is_null() {
                if (*cur).value.is_null() {
                    // 6.1 If the set S includes `absent`, then any must be the value.
                    (*complete_wild).any = 1;
                    if !(*complete_wild).ns_set.is_null() {
                        xml_schema_free_wildcard_ns_set((*complete_wild).ns_set);
                        (*complete_wild).ns_set = null_mut();
                    }
                    if !(*complete_wild).neg_ns_set.is_null() {
                        xml_free((*complete_wild).neg_ns_set as _);
                        (*complete_wild).neg_ns_set = null_mut();
                    }
                    return 0;
                }
                cur = (*cur).next;
            }
            if (*complete_wild).neg_ns_set.is_null() {
                // 6.2 If the set S does not include `absent`, then a pair of not
                // and `absent` must be the value.
                if !(*complete_wild).ns_set.is_null() {
                    xml_schema_free_wildcard_ns_set((*complete_wild).ns_set);
                    (*complete_wild).ns_set = null_mut();
                }
                (*complete_wild).neg_ns_set = xml_schema_new_wildcard_ns_constraint(ctxt);
                if (*complete_wild).neg_ns_set.is_null() {
                    return -1;
                }
                (*(*complete_wild).neg_ns_set).value = null_mut();
            }
            return 0;
        }
        0
    }
}

/// Returns 1 if emptiable, 0 otherwise.
#[doc(alias = "xmlSchemaGetParticleEmptiable")]
unsafe fn xml_schema_get_particle_emptiable(particle: XmlSchemaParticlePtr) -> i32 {
    unsafe {
        let mut part: XmlSchemaParticlePtr;
        let mut emptiable: i32;

        if (*particle).children.is_null() || (*particle).min_occurs == 0 {
            return 1;
        }

        part = (*(*particle).children).children as XmlSchemaParticlePtr;
        if part.is_null() {
            return 1;
        }

        while !part.is_null() {
            if (*(*part).children).typ == XmlSchemaTypeType::XmlSchemaTypeElement
                || (*(*part).children).typ == XmlSchemaTypeType::XmlSchemaTypeAny
            {
                emptiable = ((*part).min_occurs == 0) as i32;
            } else {
                emptiable = xml_schema_get_particle_emptiable(part);
            }
            if (*(*particle).children).typ == XmlSchemaTypeType::XmlSchemaTypeChoice {
                if emptiable != 0 {
                    return 1;
                }
            } else {
                // <all> and <sequence>
                if emptiable == 0 {
                    return 0;
                }
            }
            part = (*part).next as XmlSchemaParticlePtr;
        }

        if (*(*particle).children).typ == XmlSchemaTypeType::XmlSchemaTypeChoice {
            0
        } else {
            1
        }
    }
}

/// Schema Component Constraint: Particle Emptiable
/// Checks whether the given particle is emptiable.
///
/// Returns 1 if emptiable, 0 otherwise.
unsafe fn xml_schema_is_particle_emptiable(particle: XmlSchemaParticlePtr) -> i32 {
    unsafe {
        // SPEC (1) "Its {min occurs} is 0."
        if particle.is_null() || (*particle).min_occurs == 0 || (*particle).children.is_null() {
            return 1;
        }
        // SPEC (2) "Its {term} is a group and the minimum part of the
        // effective total range of that group, [...] is 0."
        if WXS_IS_MODEL_GROUP!((*particle).children) {
            return xml_schema_get_particle_emptiable(particle);
        }
        0
    }
}

/// (3.4.3) Constraints on XML Representations of Complex Type Definitions:
/// Schema Representation Constraint:
/// Complex Type Definition Representation OK (src-ct)
///
/// Returns 0 if the constraints are satisfied, a positive
/// error code if not and -1 if an internal error occurred.
#[doc(alias = "xmlSchemaCheckSRCCT")]
unsafe fn xml_schema_check_srcct(ctxt: XmlSchemaParserCtxtPtr, typ: XmlSchemaTypePtr) -> i32 {
    unsafe {
        let mut ret: i32 = 0;

        // TODO: Adjust the error codes here, as I used XML_SCHEMAP_SRC_CT_1 only yet.
        let base: XmlSchemaTypePtr = (*typ).base_type;
        if !WXS_HAS_SIMPLE_CONTENT!(typ) {
            // 1 If the <complexContent> alternative is chosen, the type definition
            // `resolved` to by the `actual value` of the base [attribute]
            // must be a complex type definition;
            if !wxs_is_complex(base) {
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

                xml_schema_pcustom_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapSrcCt1,
                    typ as XmlSchemaBasicItemPtr,
                    (*typ).node.map(|node| node.into()),
                    format!("If using <complexContent>, the base type is expected to be a complex type. The base type '{qname}' is a simple type").as_str(),
                    Some(&qname)
                );
                return XmlParserErrors::XmlSchemapSrcCt1 as i32;
            }
        } else {
            // SPEC
            // 2 If the <simpleContent> alternative is chosen, all of the
            // following must be true:
            // 2.1 The type definition `resolved` to by the `actual value` of the
            // base [attribute] must be one of the following:
            if wxs_is_simple(base) {
                if !(*typ).wxs_is_extension() {
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

                    // 2.1.3 only if the <extension> alternative is also
                    // chosen, a simple type definition.
                    // TODO: Change error code to ..._SRC_CT_2_1_3.
                    xml_schema_pcustom_err(
                        ctxt,
                        XmlParserErrors::XmlSchemapSrcCt1,
                        typ as XmlSchemaBasicItemPtr,
                        None,
                        format!("If using <simpleContent> and <restriction>, the base type must be a complex type. The base type '{qname}' is a simple type").as_str(),
                        Some(&qname)
                    );
                    return XmlParserErrors::XmlSchemapSrcCt1 as i32;
                }
            } else {
                // Base type is a complex type.
                if (*base).content_type == XmlSchemaContentType::XmlSchemaContentSimple
                    || (*base).content_type == XmlSchemaContentType::XmlSchemaContentBasic
                {
                    // 2.1.1 a complex type definition whose {content type} is a
                    // simple type definition;
                    // PASS
                    if (*base).content_type_def.is_null() {
                        let name = CStr::from_ptr((*typ).name as *const i8).to_string_lossy();
                        xml_schema_pcustom_err(
                        ctxt,
                        XmlParserErrors::XmlSchemapInternal,
                        typ as XmlSchemaBasicItemPtr,
                        None,
                        format!("Internal error: xmlSchemaCheckSRCCT, '{name}', base type has no content type").as_str(),
                        Some(&name),
                    );
                        return -1;
                    }
                } else if (*base).content_type == XmlSchemaContentType::XmlSchemaContentMixed
                    && (*typ).wxs_is_restriction()
                {
                    // 2.1.2 only if the <restriction> alternative is also
                    // chosen, a complex type definition whose {content type}
                    // is mixed and a particle emptiable.
                    if xml_schema_is_particle_emptiable((*base).subtypes as XmlSchemaParticlePtr)
                        == 0
                    {
                        ret = XmlParserErrors::XmlSchemapSrcCt1 as i32;
                    } else if (*typ).content_type_def.is_null() {
                        // Attention: at this point the <simpleType> child is in
                        // ->contentTypeDef (put there during parsing).

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
                        // 2.2 If clause 2.1.2 above is satisfied, then there
                        // must be a <simpleType> among the [children] of
                        // <restriction>.
                        /* TODO: Change error code to ..._SRC_CT_2_2. */
                        xml_schema_pcustom_err(
                            ctxt,
                            XmlParserErrors::XmlSchemapSrcCt1,
                            typ as XmlSchemaBasicItemPtr,
                            None,
                            format!("A <simpleType> is expected among the children of <restriction>, if <simpleContent> is used and the base type '{qname}' is a complex type").as_str(),
                            Some(&qname)
                        );
                        return XmlParserErrors::XmlSchemapSrcCt1 as i32;
                    }
                } else {
                    ret = XmlParserErrors::XmlSchemapSrcCt1 as i32;
                }
            }
            if ret > 0 {
                if (*typ).wxs_is_restriction() {
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
                    xml_schema_pcustom_err(
                        ctxt,
                        XmlParserErrors::XmlSchemapSrcCt1,
                        typ as XmlSchemaBasicItemPtr,
                        None,
                        format!("If <simpleContent> and <restriction> is used, the base type must be a simple type or a complex type with mixed content and particle emptiable. The base type '{qname}' is none of those").as_str(),
                        Some(&qname)
                    );
                } else {
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
                    xml_schema_pcustom_err(
                        ctxt,
                        XmlParserErrors::XmlSchemapSrcCt1,
                        typ as XmlSchemaBasicItemPtr,
                        None,
                        format!("If <simpleContent> and <extension> is used, the base type must be a simple type. The base type '{qname}' is a complex type").as_str(),
                        Some(&qname)
                    );
                }
            }
        }
        // SPEC (3) "The corresponding complex type definition component must
        // satisfy the conditions set out in Constraints on Complex Type
        // Definition Schema Components ($3.4.6);"
        // NOTE (3) will be done in xmlSchemaTypeFixup().

        // SPEC (4) If clause 2.2.1 or clause 2.2.2 in the correspondence specification
        // above for {attribute wildcard} is satisfied, the intensional
        // intersection must be expressible, as defined in Attribute Wildcard
        // Intersection ($3.10.6).
        // NOTE (4) is done in xmlSchemaFixupTypeAttributeUses().
        ret
    }
}

/// Builds the wildcard and the attribute uses on the given complex type.
/// Returns -1 if an internal error occurs, 0 otherwise.
///
/// ATTENTION TODO: Experimentally this uses pointer comparisons for
/// strings, so recheck this if we start to hardcode some schemata, since
/// they might not be in the same dict.
/// NOTE: It is allowed to "extend" the xs:anyType type.
#[doc(alias = "xmlSchemaFixupTypeAttributeUses")]
unsafe fn xml_schema_fixup_type_attribute_uses(
    pctxt: XmlSchemaParserCtxtPtr,
    typ: XmlSchemaTypePtr,
) -> i32 {
    unsafe {
        let mut uses: XmlSchemaItemListPtr<*mut c_void>;
        let mut prohibs: XmlSchemaItemListPtr<XmlSchemaAttributeUseProhibPtr> = null_mut();

        if (*typ).base_type.is_null() {
            PERROR_INT!(pctxt, "xmlSchemaFixupTypeAttributeUses", "no base type");
            return -1;
        }
        let base_type: XmlSchemaTypePtr = (*typ).base_type;
        if WXS_IS_TYPE_NOT_FIXED!(base_type)
            && xml_schema_type_fixup(base_type, pctxt as XmlSchemaAbstractCtxtPtr) == -1
        {
            return -1;
        }

        uses = (*typ).attr_uses as _;
        let base_uses: XmlSchemaItemListPtr<*mut c_void> = (*base_type).attr_uses as _;
        // Expand attribute group references. And build the 'complete'
        // wildcard, i.e. intersect multiple wildcards.
        // Move attribute prohibitions into a separate list.
        if !uses.is_null() {
            if (*typ).wxs_is_restriction() {
                // This one will transfer all attr. prohibitions
                // into (*pctxt).attrProhibs.
                if xml_schema_expand_attribute_group_refs(
                    pctxt,
                    typ as XmlSchemaBasicItemPtr,
                    &raw mut (*typ).attribute_wildcard,
                    uses,
                    (*pctxt).attr_prohibs,
                ) == -1
                {
                    PERROR_INT!(
                        pctxt,
                        "xmlSchemaFixupTypeAttributeUses",
                        "failed to expand attributes"
                    );
                    return -1;
                }
                if !(*(*pctxt).attr_prohibs).items.is_empty() {
                    prohibs = (*pctxt).attr_prohibs;
                }
            } else if xml_schema_expand_attribute_group_refs(
                pctxt,
                typ as XmlSchemaBasicItemPtr,
                &raw mut (*typ).attribute_wildcard,
                uses,
                null_mut(),
            ) == -1
            {
                PERROR_INT!(
                    pctxt,
                    "xmlSchemaFixupTypeAttributeUses",
                    "failed to expand attributes"
                );
                return -1;
            }
        }
        // Inherit the attribute uses of the base type.
        if !base_uses.is_null() {
            if (*typ).wxs_is_restriction() {
                let uses_count = if !uses.is_null() {
                    (*uses).items.len()
                } else {
                    0
                };

                // Restriction.
                'inherit_next: for using in (*base_uses)
                    .items
                    .iter()
                    .map(|&using| using as XmlSchemaAttributeUsePtr)
                {
                    if !prohibs.is_null() {
                        // Filter out prohibited uses.
                        for &pro in &(*prohibs).items {
                            if WXS_ATTRUSE_DECL_NAME!(using) == (*pro).name
                                && WXS_ATTRUSE_DECL_TNS!(using) == (*pro).target_namespace
                            {
                                continue 'inherit_next;
                            }
                        }
                    }
                    if uses_count != 0 {
                        // Filter out existing uses.
                        for tmp in (*uses).items[..uses_count]
                            .iter()
                            .map(|&tmp| tmp as XmlSchemaAttributeUsePtr)
                        {
                            if WXS_ATTRUSE_DECL_NAME!(using) == WXS_ATTRUSE_DECL_NAME!(tmp)
                                && WXS_ATTRUSE_DECL_TNS!(using) == WXS_ATTRUSE_DECL_TNS!(tmp)
                            {
                                continue 'inherit_next;
                            }
                        }
                    }
                    if uses.is_null() {
                        (*typ).attr_uses = xml_schema_item_list_create::<*mut c_void>() as _;
                        if (*typ).attr_uses.is_null() {
                            // goto exit_failure;
                            return -1;
                        }
                        uses = (*typ).attr_uses as _;
                    }
                    (*uses).push(using as _);
                }
            } else {
                // Extension.
                for using in (*base_uses)
                    .items
                    .iter()
                    .map(|&using| using as XmlSchemaAttributeUsePtr)
                {
                    if uses.is_null() {
                        (*typ).attr_uses = xml_schema_item_list_create::<*mut c_void>() as _;
                        if (*typ).attr_uses.is_null() {
                            // goto exit_failure;
                            return -1;
                        }
                        uses = (*typ).attr_uses as _;
                    }

                    (*uses).push(using as _);
                }
            }
        }
        // Shrink attr. uses.
        if !uses.is_null() && (*uses).items.is_empty() {
            xml_schema_item_list_free(uses);
            (*typ).attr_uses = null_mut();
        }
        // Compute the complete wildcard.
        if (*typ).wxs_is_extension() {
            if !(*base_type).attribute_wildcard.is_null() {
                // (3.2.2.1) "If the `base wildcard` is non-`absent`, then
                // the appropriate case among the following:"
                if !(*typ).attribute_wildcard.is_null() {
                    // Union the complete wildcard with the base wildcard.
                    // SPEC {attribute wildcard}
                    // (3.2.2.1.2) "otherwise a wildcard whose {process contents}
                    // and {annotation} are those of the `complete wildcard`,
                    // and whose {namespace constraint} is the intensional union
                    // of the {namespace constraint} of the `complete wildcard`
                    // and of the `base wildcard`, as defined in Attribute
                    // Wildcard Union ($3.10.6)."
                    if xml_schema_union_wildcards(
                        pctxt,
                        (*typ).attribute_wildcard,
                        (*base_type).attribute_wildcard,
                    ) == -1
                    {
                        // goto exit_failure;
                        return -1;
                    }
                } else {
                    // (3.2.2.1.1) "If the `complete wildcard` is `absent`,
                    // then the `base wildcard`."
                    (*typ).attribute_wildcard = (*base_type).attribute_wildcard;
                }
            } else {
                // (3.2.2.2) "otherwise (the `base wildcard` is `absent`) the
                // `complete wildcard`"
                // NOOP
            }
        } else {
            // SPEC {attribute wildcard}
            // (3.1) "If the <restriction> alternative is chosen, then the
            // `complete wildcard`;"
            // NOOP
        }

        0

        // exit_failure:
        // return -1;
    }
}

/// Returns 1 if the type has the given value type, or is derived from such a type.
#[doc(alias = "xmlSchemaIsDerivedFromBuiltInType")]
unsafe fn xml_schema_is_derived_from_built_in_type(typ: XmlSchemaTypePtr, val_type: i32) -> i32 {
    unsafe {
        if typ.is_null() {
            return 0;
        }
        if wxs_is_complex(typ) {
            return 0;
        }
        if (*typ).typ == XmlSchemaTypeType::XmlSchemaTypeBasic {
            if (*typ).built_in_type == val_type {
                return 1;
            }
            if (*typ).built_in_type == XmlSchemaValType::XmlSchemasAnySimpletype as i32
                || (*typ).built_in_type == XmlSchemaValType::XmlSchemasAnytype as i32
            {
                return 0;
            }
            return xml_schema_is_derived_from_built_in_type((*typ).subtypes, val_type);
        }
        xml_schema_is_derived_from_built_in_type((*typ).subtypes, val_type)
    }
}

/// (4.6) Constraints on Complex Type Definition Schema Components
/// Schema Component Constraint:
/// Complex Type Definition Properties Correct (ct-props-correct)
/// STATUS: (seems) complete
///
/// Returns 0 if the constraints are satisfied, a positive
/// error code if not and -1 if an internal error occurred.
#[doc(alias = "xmlSchemaCheckCTPropsCorrect")]
unsafe fn xml_schema_check_ctprops_correct(
    pctxt: XmlSchemaParserCtxtPtr,
    typ: XmlSchemaTypePtr,
) -> i32 {
    unsafe {
        // TODO: Correct the error code; XmlParserErrors::XML_SCHEMAP_SRC_CT_1 is used temporarily.
        //
        // SPEC (1) "The values of the properties of a complex type definition must
        // be as described in the property tableau in The Complex Type Definition
        // Schema Component ($3.4.1), modulo the impact of Missing
        // Sub-components ($5.3)."
        if !(*typ).base_type.is_null()
            && wxs_is_simple((*typ).base_type)
            && !(*typ).wxs_is_extension()
        {
            // SPEC (2) "If the {base type definition} is a simple type definition,
            // the {derivation method} must be extension."
            xml_schema_custom_err(
                pctxt as XmlSchemaAbstractCtxtPtr,
                XmlParserErrors::XmlSchemapSrcCt1,
                None,
                typ as XmlSchemaBasicItemPtr,
                "If the base type is a simple type, the derivation method must be 'extension'",
                None,
                None,
            );
            return XmlParserErrors::XmlSchemapSrcCt1 as i32;
        }
        // SPEC (3) "Circular definitions are disallowed, except for the `ur-type
        // definition`. That is, it must be possible to reach the `ur-type
        // definition` by repeatedly following the {base type definition}."
        //
        // NOTE (3) is done in xmlSchemaCheckTypeDefCircular().
        // NOTE that (4) and (5) need the following:
        //   - attribute uses need to be already inherited (apply attr. prohibitions)
        //   - attribute group references need to be expanded already
        //   - simple types need to be typefixed already
        if !(*typ).attr_uses.is_null()
            && (*((*typ).attr_uses as XmlSchemaItemListPtr<*mut c_void>))
                .items
                .len()
                > 1
        {
            let uses: XmlSchemaItemListPtr<*mut c_void> =
                (*typ).attr_uses as XmlSchemaItemListPtr<*mut c_void>;
            let mut has_id: i32 = 0;

            'next_use: for (i, using) in (*uses)
                .items
                .iter()
                .map(|&using| using as XmlSchemaAttributeUsePtr)
                .enumerate()
                .rev()
            {
                // SPEC ct-props-correct
                // (4) "Two distinct attribute declarations in the
                // {attribute uses} must not have identical {name}s and
                // {target namespace}s."
                if i > 0 {
                    for tmp in (*uses)
                        .items
                        .iter()
                        .take(i)
                        .rev()
                        .map(|&tmp| tmp as XmlSchemaAttributeUsePtr)
                    {
                        if WXS_ATTRUSE_DECL_NAME!(using) == WXS_ATTRUSE_DECL_NAME!(tmp)
                            && WXS_ATTRUSE_DECL_TNS!(using) == WXS_ATTRUSE_DECL_TNS!(tmp)
                        {
                            let desig = xml_schema_get_component_designation(using as _);

                            xml_schema_custom_err(
                                pctxt as XmlSchemaAbstractCtxtPtr,
                                XmlParserErrors::XmlSchemapAgPropsCorrect,
                                None,
                                typ as XmlSchemaBasicItemPtr,
                                format!("Duplicate {desig}").as_str(),
                                Some(&desig),
                                None,
                            );
                            // Remove the duplicate.
                            if (*uses).remove(i) == -1 {
                                // goto exit_failure;
                                return -1;
                            }
                            // goto next_use;
                            continue 'next_use;
                        }
                    }
                }
                // SPEC ct-props-correct
                // (5) "Two distinct attribute declarations in the
                // {attribute uses} must not have {type definition}s which
                // are or are derived from ID."
                if !WXS_ATTRUSE_TYPEDEF!(using).is_null()
                    && xml_schema_is_derived_from_built_in_type(
                        WXS_ATTRUSE_TYPEDEF!(using),
                        XmlSchemaValType::XmlSchemasID as i32,
                    ) != 0
                {
                    if has_id != 0 {
                        let desig = xml_schema_get_component_designation(using as _);

                        xml_schema_custom_err(
                            pctxt as XmlSchemaAbstractCtxtPtr,
                            XmlParserErrors::XmlSchemapAgPropsCorrect,
                            None,
                            typ as XmlSchemaBasicItemPtr,
                            format!("There must not exist more than one attribute declaration of type 'xs:ID' (or derived from 'xs:ID'). The {desig} violates this constraint").as_str(),
                            Some(&desig),
                            None
                        );

                        if (*uses).remove(i) == -1 {
                            // goto exit_failure;
                            return -1;
                        }
                    }

                    has_id = 1;
                }
                // next_use: {}
            }
        }
        0
        // exit_failure:
        //     return -1;
    }
}

/// (3.4.6) Constraints on Complex Type Definition Schema Components
/// Schema Component Constraint:
/// Derivation Valid (Extension) (cos-ct-extends)
///
/// STATUS:
///   missing:
///     (1.5)
///     (1.4.3.2.2.2) "Particle Valid (Extension)"
///
/// Returns 0 if the constraints are satisfied, a positive
/// error code if not and -1 if an internal error occurred.
#[doc(alias = "xmlSchemaCheckCOSCTExtends")]
unsafe fn xml_schema_check_cosctextends(
    ctxt: XmlSchemaParserCtxtPtr,
    typ: XmlSchemaTypePtr,
) -> i32 {
    unsafe {
        let base: XmlSchemaTypePtr = (*typ).base_type;
        // TODO: Correct the error code; XML_SCHEMAP_COS_CT_EXTENDS_1_1 is used
        // temporarily only.

        // SPEC (1) "If the {base type definition} is a complex type definition,
        // then all of the following must be true:"
        if wxs_is_complex(base) {
            // SPEC (1.1) "The {final} of the {base type definition} must not contain extension."
            if (*base).flags & XML_SCHEMAS_TYPE_FINAL_EXTENSION != 0 {
                xml_schema_pcustom_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapCosCtExtends1_1,
                    typ as XmlSchemaBasicItemPtr,
                    None,
                    "The 'final' of the base type definition contains 'extension'",
                    None,
                );
                return XmlParserErrors::XmlSchemapCosCtExtends1_1 as i32;
            }

            // ATTENTION: The constrains (1.2) and (1.3) are not applied,
            // since they are automatically satisfied through the
            // inheriting mechanism.
            // Note that even if redefining components, the inheriting mechanism
            // is used.

            // SPEC (1.4) "One of the following must be true:"
            if !(*typ).content_type_def.is_null()
                && (*typ).content_type_def == (*base).content_type_def
            {
                // SPEC (1.4.1) "The {content type} of the {base type definition}
                // and the {content type} of the complex type definition itself
                // must be the same simple type definition"
                // PASS
            } else if (*typ).content_type == XmlSchemaContentType::XmlSchemaContentEmpty
                && (*base).content_type == XmlSchemaContentType::XmlSchemaContentEmpty
            {
                // SPEC (1.4.2) "The {content type} of both the {base type
                // definition} and the complex type definition itself must
                // be empty."
                // PASS
            } else {
                // SPEC (1.4.3) "All of the following must be true:"
                if (*typ).subtypes.is_null() {
                    // SPEC 1.4.3.1 The {content type} of the complex type
                    // definition itself must specify a particle.
                    xml_schema_pcustom_err(
                        ctxt,
                        XmlParserErrors::XmlSchemapCosCtExtends1_1,
                        typ as XmlSchemaBasicItemPtr,
                        None,
                        "The content type must specify a particle",
                        None,
                    );
                    return XmlParserErrors::XmlSchemapCosCtExtends1_1 as i32;
                }
                // SPEC (1.4.3.2) "One of the following must be true:"
                if (*base).content_type == XmlSchemaContentType::XmlSchemaContentEmpty {
                    // SPEC (1.4.3.2.1) "The {content type} of the {base type
                    // definition} must be empty.
                    // PASS
                } else {
                    // SPEC (1.4.3.2.2) "All of the following must be true:"
                    if (*typ).content_type != (*base).content_type
                        || ((*typ).content_type != XmlSchemaContentType::XmlSchemaContentMixed
                            && (*typ).content_type
                                != XmlSchemaContentType::XmlSchemaContentElements)
                    {
                        // SPEC (1.4.3.2.2.1) "Both {content type}s must be mixed
                        // or both must be element-only."
                        xml_schema_pcustom_err(
                            ctxt,
                            XmlParserErrors::XmlSchemapCosCtExtends1_1,
                            typ as XmlSchemaBasicItemPtr,
                            None,
                            "The content type of both, the type and its base type, must either 'mixed' or 'element-only'",
                            None,
                        );
                        return XmlParserErrors::XmlSchemapCosCtExtends1_1 as i32;
                    }
                    // URGENT TODO SPEC (1.4.3.2.2.2) "The particle of the
                    // complex type definition must be a `valid extension`
                    // of the {base type definition}'s particle, as defined
                    // in Particle Valid (Extension) ($3.9.6)."
                    //
                    // NOTE that we won't check "Particle Valid (Extension)".as_ptr() as _,
                    // since it is ensured by the derivation process in
                    // xmlSchemaTypeFixup(). We need to implement this when heading
                    // for a construction API
                    // TODO: !! This is needed to be checked if redefining a type !!
                }
                // URGENT TODO (1.5)
            }
        } else {
            // SPEC (2) "If the {base type definition} is a simple type definition,
            // then all of the following must be true:"
            if (*typ).content_type_def != base {
                // SPEC (2.1) "The {content type} must be the same simple type definition."
                xml_schema_pcustom_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapCosCtExtends1_1,
                    typ as XmlSchemaBasicItemPtr,
                    None,
                    "The content type must be the simple base type",
                    None,
                );
                return XmlParserErrors::XmlSchemapCosCtExtends1_1 as i32;
            }
            if (*base).flags & XML_SCHEMAS_TYPE_FINAL_EXTENSION != 0 {
                // SPEC (2.2) "The {final} of the {base type definition} must not
                // contain extension"
                // NOTE that this is the same as (1.1).
                xml_schema_pcustom_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapCosCtExtends1_1,
                    typ as XmlSchemaBasicItemPtr,
                    None,
                    "The 'final' of the base type definition contains 'extension'",
                    None,
                );
                return XmlParserErrors::XmlSchemapCosCtExtends1_1 as i32;
            }
        }
        0
    }
}

const XML_SCHEMA_ACTION_DERIVE: i32 = 0;
const XML_SCHEMA_ACTION_REDEFINE: i32 = 1;

macro_rules! WXS_ACTION_STR {
    ($a:expr) => {
        Some(if $a == XML_SCHEMA_ACTION_DERIVE {
            "base"
        } else {
            "redefined"
        })
    };
}

/// Evaluates if a type definition contains the given "final".
/// This does take "finalDefault" into account as well.
///
/// Returns 1 if the type does contain the given "final", 0 otherwise.
#[doc(alias = "xmlSchemaTypeFinalContains")]
unsafe fn xml_schema_type_final_contains(typ: XmlSchemaTypePtr, is_final: i32) -> i32 {
    unsafe {
        if typ.is_null() {
            return 0;
        }
        if (*typ).flags & is_final != 0 { 1 } else { 0 }
    }
}

/// Schema Component Constraint:
/// Type Derivation OK (Simple) (cos-st-derived-OK)
///
/// Checks whether @type can be validly
/// derived from @baseType.
///
/// Returns 0 on success, an positive error code otherwise.
#[doc(alias = "xmlSchemaCheckCOSSTDerivedOK")]
unsafe fn xml_schema_check_cosstderived_ok(
    actxt: XmlSchemaAbstractCtxtPtr,
    typ: XmlSchemaTypePtr,
    base_type: XmlSchemaTypePtr,
    subset: i32,
) -> i32 {
    unsafe {
        // 1 They are the same type definition.
        // TODO: The identity check might have to be more complex than this.
        if typ == base_type {
            return 0;
        }
        // 2.1 restriction is not in the subset, or in the {final}
        // of its own {base type definition};
        //
        // NOTE that this will be used also via "xsi:type".
        //
        // TODO: Revise this, it looks strange. How can the "type"
        // not be fixed or *in* fixing?
        if WXS_IS_TYPE_NOT_FIXED!(typ) && xml_schema_type_fixup(typ, actxt) == -1 {
            return -1;
        }
        if WXS_IS_TYPE_NOT_FIXED!(base_type) && xml_schema_type_fixup(base_type, actxt) == -1 {
            return -1;
        }
        if subset & SUBSET_RESTRICTION != 0
            || xml_schema_type_final_contains((*typ).base_type, XML_SCHEMAS_TYPE_FINAL_RESTRICTION)
                != 0
        {
            return XmlParserErrors::XmlSchemapCosStDerivedOk2_1 as i32;
        }
        // 2.2
        if (*typ).base_type == base_type {
            // 2.2.1 D's `base type definition` is B.
            return 0;
        }
        // 2.2.2 D's `base type definition` is not the `ur-type definition`
        // and is validly derived from B given the subset, as defined by this constraint.
        if !wxs_is_anytype((*typ).base_type)
            && xml_schema_check_cosstderived_ok(actxt, (*typ).base_type, base_type, subset) == 0
        {
            return 0;
        }
        // 2.2.3 D's {variety} is list or union and B is the `simple ur-type definition`.
        if wxs_is_any_simple_type(base_type) && ((*typ).wxs_is_list() || (*typ).wxs_is_union()) {
            return 0;
        }
        // 2.2.4 B's {variety} is union and D is validly derived from a type
        // definition in B's {member type definitions} given the subset, as
        // defined by this constraint.
        //
        // NOTE: This seems not to involve built-in types, since there is no
        // built-in Union Simple Type.
        if (*base_type).wxs_is_union() {
            let mut cur: XmlSchemaTypeLinkPtr;

            cur = (*base_type).member_types;
            while !cur.is_null() {
                if WXS_IS_TYPE_NOT_FIXED!((*cur).typ)
                    && xml_schema_type_fixup((*cur).typ, actxt) == -1
                {
                    return -1;
                }
                if xml_schema_check_cosstderived_ok(actxt, typ, (*cur).typ, subset) == 0 {
                    // It just has to be validly derived from at least one member-type.
                    return 0;
                }
                cur = (*cur).next;
            }
        }
        XmlParserErrors::XmlSchemapCosStDerivedOk2_2 as i32
    }
}

unsafe fn xml_schema_get_effective_value_constraint(
    attruse: XmlSchemaAttributeUsePtr,
    fixed: *mut i32,
    value: *mut *const XmlChar,
    val: *mut XmlSchemaValPtr,
) -> i32 {
    unsafe {
        *fixed = 0;
        *value = null_mut();
        if !val.is_null() {
            *val = null_mut();
        }

        if !(*attruse).def_value.is_null() {
            *value = (*attruse).def_value;
            if !val.is_null() {
                *val = (*attruse).def_val;
            }
            if (*attruse).flags & XML_SCHEMA_ATTR_USE_FIXED != 0 {
                *fixed = 1;
            }
            return 1;
        } else if !(*attruse).attr_decl.is_null() && !(*(*attruse).attr_decl).def_value.is_null() {
            *value = (*(*attruse).attr_decl).def_value;
            if !val.is_null() {
                *val = (*(*attruse).attr_decl).def_val;
            }
            if (*(*attruse).attr_decl).flags & XML_SCHEMAS_ATTR_FIXED != 0 {
                *fixed = 1;
            }
            return 1;
        }
        0
    }
}

/// Validation Rule: Wildcard allows Namespace Name
/// (cvc-wildcard-namespace)
///
/// Returns 0 if the given namespace matches the wildcard,
/// 1 otherwise and -1 on API errors.
#[doc(alias = "xmlSchemaCheckCVCWildcardNamespace")]
unsafe fn xml_schema_check_cvcwildcard_namespace(
    wild: XmlSchemaWildcardPtr,
    ns: *const XmlChar,
) -> i32 {
    unsafe {
        if wild.is_null() {
            return -1;
        }

        if (*wild).any != 0 {
            return 0;
        } else if !(*wild).ns_set.is_null() {
            let mut cur: XmlSchemaWildcardNsPtr;

            cur = (*wild).ns_set;
            while !cur.is_null() {
                if xml_str_equal((*cur).value, ns) {
                    return 0;
                }
                cur = (*cur).next;
            }
        } else if !(*wild).neg_ns_set.is_null()
            && !ns.is_null()
            && !xml_str_equal((*(*wild).neg_ns_set).value, ns)
        {
            return 0;
        }

        1
    }
}

/// Schema Component Constraint: Wildcard Subset (cos-ns-subset)
///
/// Returns 0 if the namespace constraint of @sub is an intensional subset of @super, 1 otherwise.
#[doc(alias = "xmlSchemaIsWildcardNsConstraintSubset")]
unsafe fn xml_schema_check_cosnssubset(
    sub: XmlSchemaWildcardPtr,
    sper: XmlSchemaWildcardPtr,
) -> i32 {
    unsafe {
        // 1 super must be any.
        if (*sper).any != 0 {
            return 0;
        }
        // 2.1 sub must be a pair of not and a namespace name or `absent`.
        // 2.2 super must be a pair of not and the same value.
        if !(*sub).neg_ns_set.is_null()
            && !(*sper).neg_ns_set.is_null()
            && (*(*sub).neg_ns_set).value == (*(*sper).neg_ns_set).value
        {
            return 0;
        }
        // 3.1 sub must be a set whose members are either namespace names or `absent`.
        if !(*sub).ns_set.is_null() {
            // 3.2.1 super must be the same set or a superset thereof.
            if !(*sper).ns_set.is_null() {
                let mut cur: XmlSchemaWildcardNsPtr;
                let mut cur_b: XmlSchemaWildcardNsPtr;
                let mut found: i32 = 0;

                cur = (*sub).ns_set;
                while !cur.is_null() {
                    found = 0;
                    cur_b = (*sper).ns_set;
                    while !cur_b.is_null() {
                        if (*cur).value == (*cur_b).value {
                            found = 1;
                            break;
                        }
                        cur_b = (*cur_b).next;
                    }
                    if found == 0 {
                        return 1;
                    }
                    cur = (*cur).next;
                }
                if found != 0 {
                    return 0;
                }
            } else if !(*sper).neg_ns_set.is_null() {
                let mut cur: XmlSchemaWildcardNsPtr;
                // 3.2.2 super must be a pair of not and a namespace name or
                // `absent` and that value must not be in sub's set.
                cur = (*sub).ns_set;
                while !cur.is_null() {
                    if (*cur).value == (*(*sper).neg_ns_set).value {
                        return 1;
                    }
                    cur = (*cur).next;
                }
                return 0;
            }
        }
        1
    }
}

// Schema Component Constraint:
//   Derivation Valid (Restriction, Complex)
//   derivation-ok-restriction (2) - (4)
//
// ATTENTION:
// In XML Schema 1.1 this will be:
// Validation Rule:
//     Checking complex type subsumption (practicalSubsumption) (1, 2 and 3)
//
#[allow(clippy::too_many_arguments)]
unsafe fn xml_schema_check_derivation_okrestriction2to4(
    pctxt: XmlSchemaParserCtxtPtr,
    action: i32,
    item: XmlSchemaBasicItemPtr,
    base_item: XmlSchemaBasicItemPtr,
    uses: XmlSchemaItemListPtr<*mut c_void>,
    base_uses: XmlSchemaItemListPtr<*mut c_void>,
    wild: XmlSchemaWildcardPtr,
    base_wild: XmlSchemaWildcardPtr,
) -> i32 {
    unsafe {
        let mut found: i32; /* err = 0; */
        let mut b_eff_value: *const XmlChar = null();
        let mut eff_fixed: i32 = 0;

        if !uses.is_null() {
            for cur in (*uses)
                .items
                .iter()
                .map(|&cur| cur as XmlSchemaAttributeUsePtr)
            {
                found = 0;
                if !base_uses.is_null() {
                    // goto not_found;
                    for bcur in (*base_uses)
                        .items
                        .iter()
                        .map(|&bcur| bcur as XmlSchemaAttributeUsePtr)
                    {
                        if WXS_ATTRUSE_DECL_NAME!(cur) == WXS_ATTRUSE_DECL_NAME!(bcur)
                            && WXS_ATTRUSE_DECL_TNS!(cur) == WXS_ATTRUSE_DECL_TNS!(bcur)
                        {
                            // (2.1) "If there is an attribute use in the {attribute
                            // uses} of the {base type definition} (call this B) whose
                            // {attribute declaration} has the same {name} and {target
                            // namespace}, then  all of the following must be true:"
                            found = 1;

                            if (*cur).occurs == XML_SCHEMAS_ATTR_USE_OPTIONAL
                                && (*bcur).occurs == XML_SCHEMAS_ATTR_USE_REQUIRED
                            {
                                let action_str = WXS_ACTION_STR!(action);
                                let desig = xml_schema_get_component_designation(base_item as _);

                                // (2.1.1) "one of the following must be true:"
                                // (2.1.1.1) "B's {required} is false."
                                // (2.1.1.2) "R's {required} is true."
                                xml_schema_pattr_use_err4(
                                    pctxt,
                                    XmlParserErrors::XmlSchemapDerivationOkRestriction2_1_1,
                                    xml_schema_get_component_node(item as _).map(|node| node.into()),
                                    item,
                                    cur,
                                    format!("The 'optional' attribute use is inconsistent with the corresponding 'required' attribute use of the {} {desig}", action_str.unwrap()).as_str(),
                                    action_str,
                                    Some(&desig),
                                    None,
                                    None
                                );

                            // err = (*pctxt).err;
                            } else if xml_schema_check_cosstderived_ok(
                                pctxt as XmlSchemaAbstractCtxtPtr,
                                WXS_ATTRUSE_TYPEDEF!(cur),
                                WXS_ATTRUSE_TYPEDEF!(bcur),
                                0,
                            ) != 0
                            {
                                let desig1 = xml_schema_get_component_designation(
                                    WXS_ATTRUSE_TYPEDEF!(cur) as _,
                                );
                                let desig2 = xml_schema_get_component_designation(
                                    WXS_ATTRUSE_TYPEDEF!(bcur) as _,
                                );
                                let action_str = WXS_ACTION_STR!(action);
                                let desig3 = xml_schema_get_component_designation(base_item as _);

                                // SPEC (2.1.2) "R's {attribute declaration}'s
                                // {type definition} must be validly derived from
                                // B's {type definition} given the empty set as
                                // defined in Type Derivation OK (Simple) ($3.14.6)."
                                xml_schema_pattr_use_err4(
                                    pctxt,
                                    XmlParserErrors::XmlSchemapDerivationOkRestriction2_1_2,
                                    xml_schema_get_component_node(item as _).map(|node| node.into()),
                                    item,
                                    cur,
                                    format!("The attribute declaration's {desig1} is not validly derived from the corresponding {desig2} of the attribute declaration in the {} {desig3}", action_str.unwrap()).as_str(),
                                    Some(&desig1),
                                    Some(&desig2),
                                    action_str,
                                    Some(&desig3),
                                );
                                // xmlSchemaGetComponentDesignation(&raw mut str, baseItem),
                                // err = (*pctxt).err;
                            } else {
                                // 2.1.3 [Definition:]  Let the effective value
                                // constraint of an attribute use be its {value
                                // constraint}, if present, otherwise its {attribute
                                // declaration}'s {value constraint} .
                                xml_schema_get_effective_value_constraint(
                                    bcur,
                                    &raw mut eff_fixed,
                                    &raw mut b_eff_value,
                                    null_mut(),
                                );
                                // 2.1.3 ... one of the following must be true
                                //
                                // 2.1.3.1 B's `effective value constraint` is
                                // `absent` or default.
                                if !b_eff_value.is_null() && eff_fixed == 1 {
                                    let mut r_eff_value: *const XmlChar = null();

                                    xml_schema_get_effective_value_constraint(
                                        bcur,
                                        &raw mut eff_fixed,
                                        &raw mut r_eff_value,
                                        null_mut(),
                                    );
                                    // 2.1.3.2 R's `effective value constraint` is
                                    // fixed with the same string as B's.
                                    // MAYBE TODO: Compare the computed values.
                                    //       Hmm, it says "same string" so
                                    //       string-equality might really be sufficient.
                                    if eff_fixed == 0
                                        || !WXS_ARE_DEFAULT_STR_EQUAL!(r_eff_value, b_eff_value)
                                    {
                                        let action_str = WXS_ACTION_STR!(action);
                                        let desig =
                                            xml_schema_get_component_designation(base_item as _);

                                        xml_schema_pattr_use_err4(
                                            pctxt,
                                            XmlParserErrors::XmlSchemapDerivationOkRestriction2_1_3,
                                            xml_schema_get_component_node(item as _).map(|node| node.into()),
                                            item,
                                            cur,
                                            format!("The effective value constraint of the attribute use is inconsistent with its correspondent in the {} {desig}", action_str.unwrap()).as_str(),
                                            action_str,
                                            Some(&desig),
                                            None,
                                            None
                                        );
                                        // err = (*pctxt).err;
                                    }
                                }
                            }
                            break;
                        }
                    }
                }
                // not_found:
                if found == 0 {
                    // (2.2) "otherwise the {base type definition} must have an
                    // {attribute wildcard} and the {target namespace} of the
                    // R's {attribute declaration} must be `valid` with respect
                    // to that wildcard, as defined in Wildcard allows Namespace
                    // Name ($3.10.4)."
                    if base_wild.is_null()
                        || xml_schema_check_cvcwildcard_namespace(
                            base_wild,
                            (*WXS_ATTRUSE_DECL!(cur)).target_namespace,
                        ) != 0
                    {
                        let action_str = WXS_ACTION_STR!(action);
                        let desig = xml_schema_get_component_designation(base_item as _);

                        xml_schema_pattr_use_err4(
                            pctxt,
                            XmlParserErrors::XmlSchemapDerivationOkRestriction2_2,
                            xml_schema_get_component_node(item as _).map(|node| node.into()),
                            item,
                            cur,
                            format!("Neither a matching attribute use, nor a matching wildcard exists in the {} {desig}", action_str.unwrap()).as_str(),
                            action_str,
                            Some(&desig),
                            None,
                            None
                        );
                        /* err = (*pctxt).err; */
                    }
                }
            }
        }
        // SPEC derivation-ok-restriction (3):
        // (3) "For each attribute use in the {attribute uses} of the {base type
        // definition} whose {required} is true, there must be an attribute
        // use with an {attribute declaration} with the same {name} and
        // {target namespace} as its {attribute declaration} in the {attribute
        // uses} of the complex type definition itself whose {required} is true.
        if !base_uses.is_null() {
            for bcur in (*base_uses)
                .items
                .iter()
                .map(|&bcur| bcur as XmlSchemaAttributeUsePtr)
            {
                if (*bcur).occurs != XML_SCHEMAS_ATTR_USE_REQUIRED {
                    continue;
                }
                found = 0;
                if !uses.is_null() {
                    for cur in (*uses)
                        .items
                        .iter()
                        .map(|&cur| cur as XmlSchemaAttributeUsePtr)
                    {
                        if WXS_ATTRUSE_DECL_NAME!(cur) == WXS_ATTRUSE_DECL_NAME!(bcur)
                            && WXS_ATTRUSE_DECL_TNS!(cur) == WXS_ATTRUSE_DECL_TNS!(bcur)
                        {
                            found = 1;
                            break;
                        }
                    }
                }
                if found == 0 {
                    let desig1 = xml_schema_get_component_designation(bcur as _);
                    let desig2 = xml_schema_get_component_designation(base_item as _);

                    xml_schema_custom_err4(
                        pctxt as XmlSchemaAbstractCtxtPtr,
                        XmlParserErrors::XmlSchemapDerivationOkRestriction3,
                        None,
                        item,
                        format!(
                            "A matching attribute use for the 'required' {desig1} of the {} {desig2} is missing", WXS_ACTION_STR!(action).unwrap()
                        )
                        .as_str(),
                        Some(&desig1),
                        WXS_ACTION_STR!(action),
                        Some(&desig2),
                        None,
                    );
                }
            }
        }
        // derivation-ok-restriction (4)
        if !wild.is_null() {
            // (4) "If there is an {attribute wildcard}, all of the following must be true:"
            if base_wild.is_null() {
                let str1 = xml_schema_get_component_type_str(item as _);
                let str2 = WXS_ACTION_STR!(action);
                let str3 = xml_schema_get_component_type_str(base_item as _);
                let qname = xml_schema_get_component_qname(base_item as _);

                // (4.1) "The {base type definition} must also have one."
                xml_schema_custom_err4(
                    pctxt as XmlSchemaAbstractCtxtPtr,
                    XmlParserErrors::XmlSchemapDerivationOkRestriction4_1,
                    None,
                    item,
                    format!(
                        "The {str1} has an attribute wildcard, but the {} {str3} '{qname}' does not have one",
                        str2.unwrap()
                    )
                    .as_str(),
                    Some(str1),
                    str2,
                    Some(str3),
                    Some(&qname),
                );
                return (*pctxt).err;
            } else if (*base_wild).any == 0 && xml_schema_check_cosnssubset(wild, base_wild) != 0 {
                let str1 = WXS_ACTION_STR!(action);
                let str2 = xml_schema_get_component_type_str(base_item as _);
                let qname = xml_schema_get_component_qname(base_item as _);

                // (4.2) "The complex type definition's {attribute wildcard}'s
                // {namespace constraint} must be a subset of the {base type
                // definition}'s {attribute wildcard}'s {namespace constraint},
                // as defined by Wildcard Subset ($3.10.6)."
                xml_schema_custom_err4(
                    pctxt as XmlSchemaAbstractCtxtPtr,
                    XmlParserErrors::XmlSchemapDerivationOkRestriction4_2,
                    None,
                    item,
                    format!("The attribute wildcard is not a valid subset of the wildcard in the {} {str2} '{qname}'", str1.unwrap()).as_str(),
                    str1,
                    Some(str2),
                    Some(&qname),
                    None,
                );
                return (*pctxt).err;
            }
            // 4.3 Unless the {base type definition} is the `ur-type
            // definition`, the complex type definition's {attribute
            // wildcard}'s {process contents} must be identical to or
            // stronger than the {base type definition}'s {attribute
            // wildcard}'s {process contents}, where strict is stronger
            // than lax is stronger than skip.
            if !wxs_is_anytype(base_item as XmlSchemaTypePtr)
                && (*wild).process_contents < (*base_wild).process_contents
            {
                let str1 = WXS_ACTION_STR!(action);
                let str2 = xml_schema_get_component_type_str(base_item as _);
                let qname = xml_schema_get_component_qname(base_item as _);

                xml_schema_custom_err4(
                    pctxt as XmlSchemaAbstractCtxtPtr,
                    XmlParserErrors::XmlSchemapDerivationOkRestriction4_3,
                    None,
                    base_item,
                    format!("The {{process contents}} of the attribute wildcard is weaker than the one in the {} {str2} '{qname}'", str1.unwrap()).as_str(),
                    str1,
                    Some(str2),
                    Some(&qname),
                    None,
                );
                return (*pctxt).err;
            }
        }
        0
    }
}

/// (3.4.6) Constraints on Complex Type Definition Schema Components
/// Schema Component Constraint:
/// Derivation Valid (Restriction, Complex) (derivation-ok-restriction)
///
/// STATUS:
///   missing:
///     (5.4.2) ???
///
/// ATTENTION:
/// In XML Schema 1.1 this will be:
/// Validation Rule: Checking complex type subsumption
///
/// Returns 0 if the constraints are satisfied, a positive
/// error code if not and -1 if an internal error occurred.
#[doc(alias = "xmlSchemaCheckDerivationOKRestriction")]
unsafe fn xml_schema_check_derivation_okrestriction(
    ctxt: XmlSchemaParserCtxtPtr,
    typ: XmlSchemaTypePtr,
) -> i32 {
    unsafe {
        // TODO: Correct the error code; XML_SCHEMAP_DERIVATION_OK_RESTRICTION_1 is used
        // temporarily only.
        let base: XmlSchemaTypePtr = (*typ).base_type;
        if !wxs_is_complex(base) {
            xml_schema_custom_err(
                ctxt as XmlSchemaAbstractCtxtPtr,
                XmlParserErrors::XmlSchemapDerivationOkRestriction1,
                (*typ).node.map(|node| node.into()),
                typ as XmlSchemaBasicItemPtr,
                "The base type must be a complex type",
                None,
                None,
            );
            return (*ctxt).err;
        }
        if (*base).flags & XML_SCHEMAS_TYPE_FINAL_RESTRICTION != 0 {
            // SPEC (1) "The {base type definition} must be a complex type
            // definition whose {final} does not contain restriction."
            xml_schema_custom_err(
                ctxt as XmlSchemaAbstractCtxtPtr,
                XmlParserErrors::XmlSchemapDerivationOkRestriction1,
                (*typ).node.map(|node| node.into()),
                typ as XmlSchemaBasicItemPtr,
                "The 'final' of the base type definition contains 'restriction'",
                None,
                None,
            );
            return (*ctxt).err;
        }
        // SPEC (2), (3) and (4)
        // Those are handled in a separate function, since the
        // same constraints are needed for redefinition of
        // attribute groups as well.
        if xml_schema_check_derivation_okrestriction2to4(
            ctxt,
            XML_SCHEMA_ACTION_DERIVE,
            typ as XmlSchemaBasicItemPtr,
            base as XmlSchemaBasicItemPtr,
            (*typ).attr_uses as _,
            (*base).attr_uses as _,
            (*typ).attribute_wildcard,
            (*base).attribute_wildcard,
        ) == -1
        {
            return -1;
        }
        // SPEC (5) "One of the following must be true:"
        if (*base).built_in_type == XmlSchemaValType::XmlSchemasAnytype as i32 {
            // SPEC (5.1) "The {base type definition} must be the
            // `ur-type definition`."
            // PASS
        } else if matches!(
            (*typ).content_type,
            XmlSchemaContentType::XmlSchemaContentSimple
                | XmlSchemaContentType::XmlSchemaContentBasic
        ) {
            // SPEC (5.2.1) "The {content type} of the complex type definition
            // must be a simple type definition"
            //
            // SPEC (5.2.2) "One of the following must be true:"
            if matches!(
                (*base).content_type,
                XmlSchemaContentType::XmlSchemaContentSimple
                    | XmlSchemaContentType::XmlSchemaContentBasic
            ) {
                // SPEC (5.2.2.1) "The {content type} of the {base type
                // definition} must be a simple type definition from which
                // the {content type} is validly derived given the empty
                // set as defined in Type Derivation OK (Simple) ($3.14.6)."
                //
                // ATTENTION TODO: This seems not needed if the type implicitly
                // derived from the base type.
                //
                let err: i32 = xml_schema_check_cosstderived_ok(
                    ctxt as XmlSchemaAbstractCtxtPtr,
                    (*typ).content_type_def,
                    (*base).content_type_def,
                    0,
                );
                if err != 0 {
                    if err == -1 {
                        return -1;
                    }
                    let str1 = xml_schema_get_component_designation((*typ).content_type_def as _);
                    let str2 = xml_schema_get_component_designation((*base).content_type_def as _);

                    xml_schema_custom_err(
                        ctxt as XmlSchemaAbstractCtxtPtr,
                        XmlParserErrors::XmlSchemapDerivationOkRestriction1,
                        None,
                        typ as XmlSchemaBasicItemPtr,
                        format!("The {{content type}} {str1} is not validly derived from the base type's {{content type}} {str2}").as_str(),
                        Some(&str1),
                        Some(&str2)
                    );
                    return (*ctxt).err;
                }
            } else if (*base).content_type == XmlSchemaContentType::XmlSchemaContentMixed
                && xml_schema_is_particle_emptiable((*base).subtypes as XmlSchemaParticlePtr) != 0
            {
                // SPEC (5.2.2.2) "The {base type definition} must be mixed
                // and have a particle which is `emptiable` as defined in
                // Particle Emptiable ($3.9.6)."
                // PASS
            } else {
                xml_schema_pcustom_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapDerivationOkRestriction1,
                    typ as XmlSchemaBasicItemPtr,
                    None,
                    "The content type of the base type must be either a simple type or 'mixed' and an emptiable particle",
                    None,
                );
                return (*ctxt).err;
            }
        } else if (*typ).content_type == XmlSchemaContentType::XmlSchemaContentEmpty {
            // SPEC (5.3.1) "The {content type} of the complex type itself must be empty"
            if (*base).content_type == XmlSchemaContentType::XmlSchemaContentEmpty {
                // SPEC (5.3.2.1) "The {content type} of the {base type
                // definition} must also be empty."
                // PASS
            } else if matches!(
                (*base).content_type,
                XmlSchemaContentType::XmlSchemaContentElements
                    | XmlSchemaContentType::XmlSchemaContentMixed
            ) && xml_schema_is_particle_emptiable(
                (*base).subtypes as XmlSchemaParticlePtr,
            ) != 0
            {
                // SPEC (5.3.2.2) "The {content type} of the {base type
                // definition} must be elementOnly or mixed and have a particle
                // which is `emptiable` as defined in Particle Emptiable ($3.9.6)."
                // PASS
            } else {
                xml_schema_pcustom_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapDerivationOkRestriction1,
                    typ as XmlSchemaBasicItemPtr,
                    None,
                    "The content type of the base type must be either empty or 'mixed' (or 'elements-only') and an emptiable particle",
                    None,
                );
                return (*ctxt).err;
            }
        } else if (*typ).content_type == XmlSchemaContentType::XmlSchemaContentElements
            || WXS_HAS_MIXED_CONTENT!(typ)
        {
            // SPEC (5.4.1.1) "The {content type} of the complex type definition
            // itself must be element-only"
            if WXS_HAS_MIXED_CONTENT!(typ) && !WXS_HAS_MIXED_CONTENT!(base) {
                // SPEC (5.4.1.2) "The {content type} of the complex type
                // definition itself and of the {base type definition} must be mixed"
                xml_schema_pcustom_err(
                    ctxt,
                    XmlParserErrors::XmlSchemapDerivationOkRestriction1,
                    typ as XmlSchemaBasicItemPtr,
                    None,
                    "If the content type is 'mixed', then the content type of the base type must also be 'mixed'",
                    None,
                );
                return (*ctxt).err;
            }
        // SPEC (5.4.2) "The particle of the complex type definition itself
        // must be a `valid restriction` of the particle of the {content
        // type} of the {base type definition} as defined in Particle Valid
        // (Restriction) ($3.9.6).
        //
        // URGENT TODO: (5.4.2)
        } else {
            xml_schema_pcustom_err(
                ctxt,
                XmlParserErrors::XmlSchemapDerivationOkRestriction1,
                typ as XmlSchemaBasicItemPtr,
                None,
                "The type is not a valid restriction of its base type",
                None,
            );
            return (*ctxt).err;
        }
        0
    }
}

/// (3.4.6) Constraints on Complex Type Definition Schema Components
///
/// Returns 0 if the constraints are satisfied, a positive
/// error code if not and -1 if an internal error occurred.
#[doc(alias = "xmlSchemaCheckCTComponent")]
unsafe fn xml_schema_check_ctcomponent(ctxt: XmlSchemaParserCtxtPtr, typ: XmlSchemaTypePtr) -> i32 {
    unsafe {
        let mut ret: i32;
        // Complex Type Definition Properties Correct
        ret = xml_schema_check_ctprops_correct(ctxt, typ);
        if ret != 0 {
            return ret;
        }
        if (*typ).wxs_is_extension() {
            ret = xml_schema_check_cosctextends(ctxt, typ);
        } else {
            ret = xml_schema_check_derivation_okrestriction(ctxt, typ);
        }
        ret
    }
}

unsafe fn xml_schema_fixup_complex_type(
    pctxt: XmlSchemaParserCtxtPtr,
    typ: XmlSchemaTypePtr,
) -> i32 {
    unsafe {
        let mut res: i32;
        let olderrs: i32 = (*pctxt).nberrors;
        let base_type: XmlSchemaTypePtr = (*typ).base_type;

        if !WXS_IS_TYPE_NOT_FIXED!(typ) {
            return 0;
        }
        (*typ).flags |= XML_SCHEMAS_TYPE_INTERNAL_RESOLVED;
        'exit_failure: {
            if base_type.is_null() {
                PERROR_INT!(pctxt, "xmlSchemaFixupComplexType", "missing baseType");
                break 'exit_failure;
            }
            // Fixup the base type.
            if WXS_IS_TYPE_NOT_FIXED!(base_type) {
                xml_schema_type_fixup(base_type, pctxt as XmlSchemaAbstractCtxtPtr);
            }
            if (*base_type).flags & XML_SCHEMAS_TYPE_INTERNAL_INVALID != 0 {
                // Skip fixup if the base type is invalid.
                // TODO: Generate a warning!
                return 0;
            }
            // This basically checks if the base type can be derived.
            res = xml_schema_check_srcct(pctxt, typ);
            HFAILURE!(res, 'exit_failure);
            'exit_error: {
                HERROR!(res, 'exit_error);
                // Fixup the content type.
                if (*typ).content_type == XmlSchemaContentType::XmlSchemaContentSimple {
                    // Corresponds to <complexType><simpleContent>...
                    if wxs_is_complex(base_type)
                        && !(*base_type).content_type_def.is_null()
                        && (*typ).wxs_is_restriction()
                    {
                        let content_base: XmlSchemaTypePtr;

                        // #ifdef ENABLE_NAMED_LOCALS
                        //         c_char buf[30];
                        //         const xmlChar *tmpname;
                        // #endif
                        // SPEC (1) If <restriction> + base type is <complexType>,
                        // "whose own {content type} is a simple type..."
                        if !(*typ).content_type_def.is_null() {
                            // SPEC (1.1) "the simple type definition corresponding to the
                            // <simpleType> among the [children] of <restriction> if there
                            // is one;"
                            // Note that this "<simpleType> among the [children]" was put
                            // into ->contentTypeDef during parsing.
                            content_base = (*typ).content_type_def;
                            (*typ).content_type_def = null_mut();
                        } else {
                            // (1.2) "...otherwise (<restriction> has no <simpleType>
                            // among its [children]), the simple type definition which
                            // is the {content type} of the ... base type."
                            content_base = (*base_type).content_type_def;
                        }
                        // SPEC
                        // "... a simple type definition which restricts the simple
                        // type definition identified in clause 1.1 or clause 1.2
                        // with a set of facet components"
                        //
                        // Create the anonymous simple type, which will be the content
                        // type of the complex type.
                        // #ifdef ENABLE_NAMED_LOCALS
                        //         snprintf(buf, 29, "#scST%d".as_ptr() as _, ++((*pctxt).counter));
                        //         tmpname = xmlDictLookup((*pctxt).dict, buf, -1);
                        //         content = xmlSchemaAddType(pctxt, (*pctxt).schema, XmlSchemaTypeType::XmlSchemaTypeSimple, tmpname, (*typ).target_namespace, (*typ).node, 0);
                        // #else
                        let content: XmlSchemaTypePtr = xml_schema_add_type(
                            pctxt,
                            (*pctxt).schema,
                            XmlSchemaTypeType::XmlSchemaTypeSimple,
                            null_mut(),
                            (*typ).target_namespace,
                            (*typ).node,
                            0,
                        );
                        // #endif
                        if content.is_null() {
                            break 'exit_failure;
                        }
                        // We will use the same node as for the <complexType>
                        // to have it somehow anchored in the schema doc.
                        (*content).typ = XmlSchemaTypeType::XmlSchemaTypeSimple;
                        (*content).base_type = content_base;
                        // Move the facets, previously anchored on the
                        // complexType during parsing.
                        (*content).facets = (*typ).facets;
                        (*typ).facets = null_mut();
                        (*content).facet_set = (*typ).facet_set;
                        (*typ).facet_set = null_mut();

                        (*typ).content_type_def = content;
                        if WXS_IS_TYPE_NOT_FIXED!(content_base) {
                            xml_schema_type_fixup(content_base, pctxt as XmlSchemaAbstractCtxtPtr);
                        }
                        // Fixup the newly created type. We don't need to check
                        // for circularity here.
                        res = xml_schema_fixup_simple_type_stage_one(pctxt, content);
                        HFAILURE!(res, 'exit_failure);
                        HERROR!(res, 'exit_error);

                        res = xml_schema_fixup_simple_type_stage_two(pctxt, content);
                        HFAILURE!(res, 'exit_failure);
                        HERROR!(res, 'exit_error);
                    } else if wxs_is_complex(base_type)
                        && (*base_type).content_type == XmlSchemaContentType::XmlSchemaContentMixed
                        && (*typ).wxs_is_restriction()
                    {
                        // SPEC (2) If <restriction> + base is a mixed <complexType> with
                        // an emptiable particle, then a simple type definition which
                        // restricts the <restriction>'s <simpleType> child.
                        if (*typ).content_type_def.is_null()
                            || (*(*typ).content_type_def).base_type.is_null()
                        {
                            let name = CStr::from_ptr((*typ).name as *const i8).to_string_lossy();
                            // TODO: Check if this ever happens.
                            xml_schema_pcustom_err(
                            pctxt,
                            XmlParserErrors::XmlSchemapInternal,
                            typ as XmlSchemaBasicItemPtr,
                            None,
                            format!("Internal error: xmlSchemaTypeFixup, complex type '{name}': the <simpleContent><restriction> is missing a <simpleType> child, but was not caught by xmlSchemaCheckSRCCT()").as_str(),
                            Some(&name)
                        );
                            break 'exit_failure;
                        }
                    } else if wxs_is_complex(base_type) && (*typ).wxs_is_extension() {
                        // SPEC (3) If <extension> + base is <complexType> with
                        // <simpleType> content, "...then the {content type} of that
                        // complex type definition"
                        if (*base_type).content_type_def.is_null() {
                            let name = CStr::from_ptr((*typ).name as *const i8).to_string_lossy();
                            // TODO: Check if this ever happens. xmlSchemaCheckSRCCT
                            // should have caught this already.
                            xml_schema_pcustom_err(
                            pctxt,
                            XmlParserErrors::XmlSchemapInternal,
                            typ as XmlSchemaBasicItemPtr,
                            None,
                            format!("Internal error: xmlSchemaTypeFixup, complex type '{name}': the <extension>ed base type is a complex type with no simple content type").as_str(),
                            Some(&name)
                        );
                            break 'exit_failure;
                        }
                        (*typ).content_type_def = (*base_type).content_type_def;
                    } else if wxs_is_simple(base_type) && (*typ).wxs_is_extension() {
                        // SPEC (4) <extension> + base is <simpleType>
                        // "... then that simple type definition"
                        (*typ).content_type_def = base_type;
                    } else {
                        let name = CStr::from_ptr((*typ).name as *const i8).to_string_lossy();
                        // TODO: Check if this ever happens.
                        xml_schema_pcustom_err(
                        pctxt,
                        XmlParserErrors::XmlSchemapInternal,
                        typ as XmlSchemaBasicItemPtr,
                        None,
                        format!("Internal error: xmlSchemaTypeFixup, complex type '{name}' with <simpleContent>: unhandled derivation case").as_str(),
                        Some(&name)
                    );
                        break 'exit_failure;
                    }
                } else {
                    let mut dummy_sequence: i32 = 0;
                    let mut particle: XmlSchemaParticlePtr =
                        (*typ).subtypes as XmlSchemaParticlePtr;
                    // Corresponds to <complexType><complexContent>...
                    //
                    // NOTE that the effective mixed was already set during parsing of
                    // <complexType> and <complexContent>; its flag value is
                    // XML_SCHEMAS_TYPE_MIXED.
                    //
                    // Compute the "effective content":
                    // (2.1.1) + (2.1.2) + (2.1.3)
                    if particle.is_null()
                        || ((*particle).typ == XmlSchemaTypeType::XmlSchemaTypeParticle
                            && (matches!(
                                (*(*particle).children).typ,
                                XmlSchemaTypeType::XmlSchemaTypeAll
                                    | XmlSchemaTypeType::XmlSchemaTypeSequence
                            ) || ((*(*particle).children).typ
                                == XmlSchemaTypeType::XmlSchemaTypeChoice
                                && (*particle).min_occurs == 0))
                            && (*((*particle).children as XmlSchemaTreeItemPtr))
                                .children
                                .is_null())
                    {
                        if (*typ).flags & XML_SCHEMAS_TYPE_MIXED != 0 {
                            // SPEC (2.1.4) "If the `effective mixed` is true, then
                            // a particle whose properties are as follows:..."
                            //
                            // Empty sequence model group with
                            // minOccurs/maxOccurs = 1 (i.e. a "particle emptiable").
                            // NOTE that we sill assign it the <complexType> node to
                            // somehow anchor it in the doc.
                            if particle.is_null()
                                || (*(*particle).children).typ
                                    != XmlSchemaTypeType::XmlSchemaTypeSequence
                            {
                                // Create the particle.
                                particle = xml_schema_add_particle(pctxt, (*typ).node, 1, 1);
                                if particle.is_null() {
                                    break 'exit_error;
                                }
                                // Create the model group.
                                // URGENT TODO: avoid adding to pending items.
                                (*particle).children = xml_schema_add_model_group(
                                    pctxt,
                                    (*pctxt).schema,
                                    XmlSchemaTypeType::XmlSchemaTypeSequence,
                                    (*typ).node.unwrap(),
                                )
                                    as XmlSchemaTreeItemPtr;
                                if (*particle).children.is_null() {
                                    break 'exit_failure;
                                }

                                (*typ).subtypes = particle as XmlSchemaTypePtr;
                            }
                            dummy_sequence = 1;
                            (*typ).content_type = XmlSchemaContentType::XmlSchemaContentElements;
                        } else {
                            // SPEC (2.1.5) "otherwise empty"
                            (*typ).content_type = XmlSchemaContentType::XmlSchemaContentEmpty;
                        }
                    } else {
                        // SPEC (2.2) "otherwise the particle corresponding to the
                        // <all>, <choice>, <group> or <sequence> among the
                        // [children]."
                        (*typ).content_type = XmlSchemaContentType::XmlSchemaContentElements;
                    }
                    // Compute the "content type".
                    if (*typ).wxs_is_restriction() {
                        // SPEC (3.1) "If <restriction>..."
                        // (3.1.1) + (3.1.2)
                        if (*typ).content_type != XmlSchemaContentType::XmlSchemaContentEmpty
                            && (*typ).flags & XML_SCHEMAS_TYPE_MIXED != 0
                        {
                            (*typ).content_type = XmlSchemaContentType::XmlSchemaContentMixed;
                        }
                    } else {
                        // SPEC (3.2) "If <extension>..."
                        if (*typ).content_type == XmlSchemaContentType::XmlSchemaContentEmpty {
                            // SPEC (3.2.1)
                            // "If the `effective content` is empty, then the
                            //  {content type} of the [...] base ..."
                            (*typ).content_type = (*base_type).content_type;
                            (*typ).subtypes = (*base_type).subtypes;
                            // Fixes bug #347316:
                            // This is the case when the base type has a simple
                            // type definition as content.
                            (*typ).content_type_def = (*base_type).content_type_def;
                        // NOTE that the effective mixed is ignored here.
                        } else if (*base_type).content_type
                            == XmlSchemaContentType::XmlSchemaContentEmpty
                        {
                            // SPEC (3.2.2)
                            if (*typ).flags & XML_SCHEMAS_TYPE_MIXED != 0 {
                                (*typ).content_type = XmlSchemaContentType::XmlSchemaContentMixed;
                            }
                        } else {
                            // SPEC (3.2.3)
                            if (*typ).flags & XML_SCHEMAS_TYPE_MIXED != 0 {
                                (*typ).content_type = XmlSchemaContentType::XmlSchemaContentMixed;
                            }
                            // "A model group whose {compositor} is sequence and whose
                            // {particles} are..."
                            if !WXS_TYPE_PARTICLE!(typ).is_null()
                                && !WXS_TYPE_PARTICLE_TERM!(typ).is_null()
                                && (*WXS_TYPE_PARTICLE_TERM!(typ)).typ
                                    == XmlSchemaTypeType::XmlSchemaTypeAll
                            {
                                // SPEC cos-all-limited (1)
                                // TODO: error code
                                xml_schema_custom_err(
                                    pctxt as XmlSchemaAbstractCtxtPtr,
                                    XmlParserErrors::XmlSchemapCosAllLimited,
                                    xml_schema_get_component_node(typ as _).map(|node| node.into()),
                                    null_mut(),
                                    "The type has an 'all' model group in its {content type} and thus cannot be derived from a non-empty type, since this would produce a 'sequence' model group containing the 'all' model group; 'all' model groups are not allowed to appear inside other model groups",
                                    None,
                                    None,
                                );
                            } else if !WXS_TYPE_PARTICLE!(base_type).is_null()
                                && !WXS_TYPE_PARTICLE_TERM!(base_type).is_null()
                                && (*WXS_TYPE_PARTICLE_TERM!(base_type)).typ
                                    == XmlSchemaTypeType::XmlSchemaTypeAll
                            {
                                // SPEC cos-all-limited (1)
                                // TODO: error code
                                xml_schema_custom_err(
                                    pctxt as XmlSchemaAbstractCtxtPtr,
                                    XmlParserErrors::XmlSchemapCosAllLimited,
                                    xml_schema_get_component_node(typ as _).map(|node| node.into()),
                                    null_mut(),
                                    "A type cannot be derived by extension from a type which has an 'all' model group in its {content type}, since this would produce a 'sequence' model group containing the 'all' model group; 'all' model groups are not allowed to appear inside other model groups",
                                    None,
                                    None,
                                );
                            } else if dummy_sequence == 0 && !(*base_type).subtypes.is_null() {
                                let effective_content: XmlSchemaTreeItemPtr =
                                    (*typ).subtypes as XmlSchemaTreeItemPtr;
                                // Create the particle.
                                particle = xml_schema_add_particle(pctxt, (*typ).node, 1, 1);
                                if particle.is_null() {
                                    break 'exit_failure;
                                }
                                // Create the "sequence" model group.
                                (*particle).children = xml_schema_add_model_group(
                                    pctxt,
                                    (*pctxt).schema,
                                    XmlSchemaTypeType::XmlSchemaTypeSequence,
                                    (*typ).node.unwrap(),
                                )
                                    as XmlSchemaTreeItemPtr;
                                if (*particle).children.is_null() {
                                    break 'exit_failure;
                                }
                                (*typ).subtypes = particle as XmlSchemaTypePtr;
                                // SPEC "the particle of the {content type} of
                                // the ... base ..."
                                // Create a duplicate of the base type's particle
                                // and assign its "term" to it.
                                (*(*particle).children).children = xml_schema_add_particle(
                                    pctxt,
                                    (*typ).node,
                                    (*((*base_type).subtypes as XmlSchemaParticlePtr)).min_occurs,
                                    (*((*base_type).subtypes as XmlSchemaParticlePtr)).max_occurs,
                                )
                                    as XmlSchemaTreeItemPtr;
                                if (*(*particle).children).children.is_null() {
                                    break 'exit_failure;
                                }
                                particle = (*(*particle).children).children as XmlSchemaParticlePtr;
                                (*particle).children =
                                    (*((*base_type).subtypes as XmlSchemaParticlePtr)).children;
                                // SPEC "followed by the `effective content`."
                                (*particle).next = effective_content;
                                // This all will result in:
                                // new-particle
                                //   --> new-sequence(    *         new-particle
                                //           --> base-model,
                                //         this-particle
                                //            --> this-model
                                //        )
                            } else {
                                // This is the case when there is already an empty
                                // <sequence> with minOccurs==maxOccurs==1.
                                // Just add the base types's content type.
                                // NOTE that, although we miss to add an intermediate
                                // <sequence>, this should produce no difference to
                                // neither the regex compilation of the content model,
                                // nor to the complex type constraints.
                                (*(*particle).children).children =
                                    (*base_type).subtypes as XmlSchemaTreeItemPtr;
                            }
                        }
                    }
                }
                // Now fixup attribute uses:
                //   - expand attr. group references
                //     - intersect attribute wildcards
                //   - inherit attribute uses of the base type
                //   - inherit or union attr. wildcards if extending
                //   - apply attr. use prohibitions if restricting
                res = xml_schema_fixup_type_attribute_uses(pctxt, typ);
                HFAILURE!(res, 'exit_failure);
                HERROR!(res, 'exit_error);
                // Apply the complex type component constraints; this will not
                // check attributes, since this is done in
                // xmlSchemaFixupTypeAttributeUses().
                res = xml_schema_check_ctcomponent(pctxt, typ);
                HFAILURE!(res, 'exit_failure);
                HERROR!(res, 'exit_error);

                if olderrs != (*pctxt).nberrors {
                    return (*pctxt).err;
                } else {
                    return 0;
                }
            }

            // exit_error:
            (*typ).flags |= XML_SCHEMAS_TYPE_INTERNAL_INVALID;
            return (*pctxt).err;
        }

        // exit_failure:
        (*typ).flags |= XML_SCHEMAS_TYPE_INTERNAL_INVALID;
        -1
    }
}

/// Fixes the content model of the type.
/// URGENT TODO: We need an i32 result!
#[doc(alias = "xmlSchemaTypeFixup")]
unsafe fn xml_schema_type_fixup(typ: XmlSchemaTypePtr, actxt: XmlSchemaAbstractCtxtPtr) -> i32 {
    unsafe {
        if typ.is_null() {
            return 0;
        }
        if (*actxt).typ != XML_SCHEMA_CTXT_PARSER {
            AERROR_INT!(
                actxt,
                "xmlSchemaTypeFixup",
                "this function needs a parser context"
            );
            return -1;
        }
        if !WXS_IS_TYPE_NOT_FIXED!(typ) {
            return 0;
        }
        if (*typ).typ == XmlSchemaTypeType::XmlSchemaTypeComplex {
            return xml_schema_fixup_complex_type(actxt as XmlSchemaParserCtxtPtr, typ);
        } else if (*typ).typ == XmlSchemaTypeType::XmlSchemaTypeSimple {
            return xml_schema_fixup_simple_type_stage_two(actxt as XmlSchemaParserCtxtPtr, typ);
        }
        0
    }
}

unsafe fn xml_schema_finish_member_type_definitions_property(
    pctxt: XmlSchemaParserCtxtPtr,
    typ: XmlSchemaTypePtr,
) -> i32 {
    unsafe {
        let mut link: XmlSchemaTypeLinkPtr;
        let mut last_link: XmlSchemaTypeLinkPtr;
        let mut prev_link: XmlSchemaTypeLinkPtr;
        let mut sub_link: XmlSchemaTypeLinkPtr;
        let mut new_link: XmlSchemaTypeLinkPtr;
        // The actual value is then formed by replacing any union type
        // definition in the `explicit members` with the members of their
        // {member type definitions}, in order.
        //
        // TODO: There's a bug entry at
        // "http://lists.w3.org/Archives/Public/www-xml-schema-comments/2005JulSep/0287.html"
        // which indicates that we'll keep the union types the future.
        link = (*typ).member_types;
        while !link.is_null() {
            if WXS_IS_TYPE_NOT_FIXED!((*link).typ) {
                xml_schema_type_fixup((*link).typ, pctxt as XmlSchemaAbstractCtxtPtr);
            }

            if (*(*link).typ).wxs_is_union() {
                sub_link = xml_schema_get_union_simple_type_member_types((*link).typ);
                if !sub_link.is_null() {
                    (*link).typ = (*sub_link).typ;
                    if !(*sub_link).next.is_null() {
                        last_link = (*link).next;
                        sub_link = (*sub_link).next;
                        prev_link = link;
                        while !sub_link.is_null() {
                            new_link =
                                xml_malloc(size_of::<XmlSchemaTypeLink>()) as XmlSchemaTypeLinkPtr;
                            if new_link.is_null() {
                                xml_schema_perr_memory(pctxt, "allocating a type link", None);
                                return -1;
                            }
                            (*new_link).typ = (*sub_link).typ;
                            (*prev_link).next = new_link;
                            prev_link = new_link;
                            (*new_link).next = last_link;

                            sub_link = (*sub_link).next;
                        }
                    }
                }
            }
            link = (*link).next;
        }
        0
    }
}

/// Checks st-props-correct.
///
/// Returns 0 if the properties are correct,
/// if not, a positive error code and -1 on internal errors.
#[doc(alias = "xmlSchemaCheckSTPropsCorrect")]
unsafe fn xml_schema_check_st_props_correct(
    ctxt: XmlSchemaParserCtxtPtr,
    typ: XmlSchemaTypePtr,
) -> i32 {
    unsafe {
        let base_type: XmlSchemaTypePtr = (*typ).base_type;

        // STATE: error funcs converted.

        // Schema Component Constraint: Simple Type Definition Properties Correct
        //
        // NOTE: This is somehow redundant, since we actually built a simple type
        // to have all the needed information; this acts as an self test.

        // Base type: If the datatype has been `derived` by `restriction`
        // then the Simple Type Definition component from which it is `derived`,
        // otherwise the Simple Type Definition for anySimpleType ($4.1.6).
        if base_type.is_null() {
            // TODO: Think about: "modulo the impact of Missing
            // Sub-components ($5.3)."
            xml_schema_pcustom_err(
                ctxt,
                XmlParserErrors::XmlSchemapStPropsCorrect1,
                typ as XmlSchemaBasicItemPtr,
                None,
                "No base type existent",
                None,
            );
            return XmlParserErrors::XmlSchemapStPropsCorrect1 as i32;
        }
        if !wxs_is_simple(base_type) {
            let qname = xml_schema_get_component_qname(base_type as _);
            xml_schema_pcustom_err(
                ctxt,
                XmlParserErrors::XmlSchemapStPropsCorrect1,
                typ as XmlSchemaBasicItemPtr,
                None,
                format!("The base type '{qname}' is not a simple type").as_str(),
                Some(&qname),
            );
            return XmlParserErrors::XmlSchemapStPropsCorrect1 as i32;
        }
        if ((*typ).wxs_is_list() || (*typ).wxs_is_union())
            && !(*typ).wxs_is_restriction()
            && (!wxs_is_any_simple_type(base_type)
                && (*base_type).typ != XmlSchemaTypeType::XmlSchemaTypeSimple)
        {
            let qname = xml_schema_get_component_qname(base_type as _);
            xml_schema_pcustom_err(
                ctxt,
                XmlParserErrors::XmlSchemapStPropsCorrect1,
                typ as XmlSchemaBasicItemPtr,
                None,
                format!("A type, derived by list or union, must have the simple ur-type definition as base type, not '{qname}'").as_str(),
                Some(&qname)
            );
            return XmlParserErrors::XmlSchemapStPropsCorrect1 as i32;
        }
        // Variety: One of {atomic, list, union}.
        if !(*typ).wxs_is_atomic() && !(*typ).wxs_is_union() && !(*typ).wxs_is_list() {
            xml_schema_pcustom_err(
                ctxt,
                XmlParserErrors::XmlSchemapStPropsCorrect1,
                typ as XmlSchemaBasicItemPtr,
                None,
                "The variety is absent",
                None,
            );
            return XmlParserErrors::XmlSchemapStPropsCorrect1 as i32;
        }
        // TODO: Finish this. Hmm, is this finished?

        // 3 The {final} of the {base type definition} must not contain restriction.
        if xml_schema_type_final_contains(base_type, XML_SCHEMAS_TYPE_FINAL_RESTRICTION) != 0 {
            let qname = xml_schema_get_component_qname(base_type as _);
            xml_schema_pcustom_err(
                ctxt,
                XmlParserErrors::XmlSchemapStPropsCorrect3,
                typ as XmlSchemaBasicItemPtr,
                None,
                format!("The 'final' of its base type '{qname}' must not contain 'restriction'")
                    .as_str(),
                Some(&qname),
            );
            return XmlParserErrors::XmlSchemapStPropsCorrect3 as i32;
        }

        // 2 All simple type definitions must be derived ultimately from the `simple
        // ur-type definition` (so circular definitions are disallowed). That is, it
        // must be possible to reach a built-in primitive datatype or the `simple
        // ur-type definition` by repeatedly following the {base type definition}.
        //
        // NOTE: this is done in xmlSchemaCheckTypeDefCircular().
        0
    }
}

/// Schema Component Constraint:
/// Derivation Valid (Restriction, Simple) (cos-st-restricts)
///
/// Checks if the given @type (simpleType) is derived validly by restriction.
/// STATUS:
///
/// Returns -1 on internal errors, 0 if the type is validly derived,
/// a positive error code otherwise.
#[doc(alias = "xmlSchemaCheckCOSSTRestricts")]
unsafe fn xml_schema_check_cosstrestricts(
    pctxt: XmlSchemaParserCtxtPtr,
    typ: XmlSchemaTypePtr,
) -> i32 {
    unsafe {
        if (*typ).typ != XmlSchemaTypeType::XmlSchemaTypeSimple {
            PERROR_INT!(
                pctxt,
                "xmlSchemaCheckCOSSTRestricts",
                "given type is not a user-derived simpleType"
            );
            return -1;
        }

        if (*typ).wxs_is_atomic() {
            let primitive: XmlSchemaTypePtr;
            // 1.1 The {base type definition} must be an atomic simple
            // type definition or a built-in primitive datatype.
            if !(*(*typ).base_type).wxs_is_atomic() {
                let qname = xml_schema_get_component_qname((*typ).base_type as _);
                xml_schema_pcustom_err(
                    pctxt,
                    XmlParserErrors::XmlSchemapCosStRestricts1_1,
                    typ as XmlSchemaBasicItemPtr,
                    None,
                    format!("The base type '{qname}' is not an atomic simple type").as_str(),
                    Some(&qname),
                );
                return XmlParserErrors::XmlSchemapCosStRestricts1_1 as i32;
            }
            // 1.2 The {final} of the {base type definition} must not contain restriction.
            // OPTIMIZE TODO : This is already done in xmlSchemaCheckStPropsCorrect
            if xml_schema_type_final_contains((*typ).base_type, XML_SCHEMAS_TYPE_FINAL_RESTRICTION)
                != 0
            {
                let qname = xml_schema_get_component_qname((*typ).base_type as _);
                xml_schema_pcustom_err(
                    pctxt,
                    XmlParserErrors::XmlSchemapCosStRestricts1_2,
                    typ as XmlSchemaBasicItemPtr,
                    None,
                    format!("The final of its base type '{qname}' must not contain 'restriction'")
                        .as_str(),
                    Some(&qname),
                );
                return XmlParserErrors::XmlSchemapCosStRestricts1_2 as i32;
            }

            // 1.3.1 DF must be an allowed constraining facet for the {primitive
            // type definition}, as specified in the appropriate subsection of 3.2
            // Primitive datatypes.
            if !(*typ).facets.is_null() {
                let mut facet: XmlSchemaFacetPtr;
                let mut ok: i32 = 1;

                primitive = xml_schema_get_primitive_type(typ);
                if primitive.is_null() {
                    PERROR_INT!(
                        pctxt,
                        "xmlSchemaCheckCOSSTRestricts",
                        "failed to get primitive type"
                    );
                    return -1;
                }
                facet = (*typ).facets;
                while {
                    if xml_schema_is_built_in_type_facet(primitive, (*facet).typ as _) == 0 {
                        ok = 0;
                        xml_schema_pillegal_facet_atomic_err(
                            pctxt,
                            XmlParserErrors::XmlSchemapCosStRestricts1_3_1,
                            typ,
                            primitive,
                            facet,
                        );
                    }
                    facet = (*facet).next;
                    !facet.is_null()
                } {}
                if ok == 0 {
                    return XmlParserErrors::XmlSchemapCosStRestricts1_3_1 as i32;
                }
            }
        // SPEC (1.3.2) "If there is a facet of the same kind in the {facets}
        // of the {base type definition} (call this BF),then the DF's {value}
        // must be a valid restriction of BF's {value} as defined in
        // [XML Schemas: Datatypes]."
        //
        // NOTE (1.3.2) Facet derivation constraints are currently handled in
        // xmlSchemaDeriveAndValidateFacets()
        } else if (*typ).wxs_is_list() {
            let item_type: XmlSchemaTypePtr = (*typ).subtypes;
            if item_type.is_null() || !wxs_is_simple(item_type) {
                PERROR_INT!(
                    pctxt,
                    "xmlSchemaCheckCOSSTRestricts",
                    "failed to evaluate the item type"
                );
                return -1;
            }
            if WXS_IS_TYPE_NOT_FIXED!(item_type) {
                xml_schema_type_fixup(item_type, pctxt as XmlSchemaAbstractCtxtPtr);
            }
            // 2.1 The {item type definition} must have a {variety} of atomic or
            // union (in which case all the {member type definitions}
            // must be atomic).
            if !(*item_type).wxs_is_atomic() && !(*item_type).wxs_is_union() {
                let qname = xml_schema_get_component_qname(item_type as _);
                xml_schema_pcustom_err(
                    pctxt,
                    XmlParserErrors::XmlSchemapCosStRestricts2_1,
                    typ as XmlSchemaBasicItemPtr,
                    None,
                    format!("The item type '{qname}' does not have a variety of atomic or union")
                        .as_str(),
                    Some(&qname),
                );
                return XmlParserErrors::XmlSchemapCosStRestricts2_1 as i32;
            } else if (*item_type).wxs_is_union() {
                let mut member: XmlSchemaTypeLinkPtr;

                member = (*item_type).member_types;
                while !member.is_null() {
                    if !(*(*member).typ).wxs_is_atomic() {
                        let qname = xml_schema_get_component_qname((*member).typ as _);
                        xml_schema_pcustom_err(
                            pctxt,
                            XmlParserErrors::XmlSchemapCosStRestricts2_1,
                            typ as XmlSchemaBasicItemPtr,
                            None,
                            format!("The item type is a union type, but the member type '{qname}' of this item type is not atomic").as_str(),
                            Some(&qname)
                        );
                        return XmlParserErrors::XmlSchemapCosStRestricts2_1 as i32;
                    }
                    member = (*member).next;
                }
            }

            if wxs_is_any_simple_type((*typ).base_type) {
                let mut facet: XmlSchemaFacetPtr;
                // This is the case if we have: <simpleType><list ..
                // 2.3.1
                // 2.3.1.1 The {final} of the {item type definition} must not contain list.
                if xml_schema_type_final_contains(item_type, XML_SCHEMAS_TYPE_FINAL_LIST) != 0 {
                    let qname = xml_schema_get_component_qname(item_type as _);
                    xml_schema_pcustom_err(
                        pctxt,
                        XmlParserErrors::XmlSchemapCosStRestricts2_3_1_1,
                        typ as XmlSchemaBasicItemPtr,
                        None,
                        format!("The final of its item type '{qname}' must not contain 'list'")
                            .as_str(),
                        Some(&qname),
                    );
                    return XmlParserErrors::XmlSchemapCosStRestricts2_3_1_1 as i32;
                }
                // 2.3.1.2 The {facets} must only contain the whiteSpace
                // facet component.
                // OPTIMIZE TODO: the S4S already disallows any facet
                // to be specified.
                if !(*typ).facets.is_null() {
                    facet = (*typ).facets;
                    while {
                        if (*facet).typ != XmlSchemaTypeType::XmlSchemaFacetWhitespace {
                            xml_schema_pillegal_facet_list_union_err(
                                pctxt,
                                XmlParserErrors::XmlSchemapCosStRestricts2_3_1_2,
                                typ,
                                facet,
                            );
                            return XmlParserErrors::XmlSchemapCosStRestricts2_3_1_2 as i32;
                        }
                        facet = (*facet).next;
                        !facet.is_null()
                    } {}
                }
                // MAYBE TODO: (Hmm, not really) Datatypes states:
                // A `list` datatype can be `derived` from an `atomic` datatype
                // whose `lexical space` allows space (such as string or anyURI)or
                // a `union` datatype any of whose {member type definitions}'s
                // `lexical space` allows space.
            } else {
                // This is the case if we have: <simpleType><restriction ...
                // I.e. the variety of "list" is inherited.
                // 2.3.2
                // 2.3.2.1 The {base type definition} must have a {variety} of list.
                if !(*(*typ).base_type).wxs_is_list() {
                    let qname = xml_schema_get_component_qname((*typ).base_type as _);
                    xml_schema_pcustom_err(
                        pctxt,
                        XmlParserErrors::XmlSchemapCosStRestricts2_3_2_1,
                        typ as XmlSchemaBasicItemPtr,
                        None,
                        format!("The base type '{qname}' must be a list type").as_str(),
                        Some(&qname),
                    );
                    return XmlParserErrors::XmlSchemapCosStRestricts2_3_2_1 as i32;
                }
                // 2.3.2.2 The {final} of the {base type definition} must not
                // contain restriction.
                if xml_schema_type_final_contains(
                    (*typ).base_type,
                    XML_SCHEMAS_TYPE_FINAL_RESTRICTION,
                ) != 0
                {
                    let qname = xml_schema_get_component_qname((*typ).base_type as _);
                    xml_schema_pcustom_err(
                        pctxt,
                        XmlParserErrors::XmlSchemapCosStRestricts2_3_2_2,
                        typ as XmlSchemaBasicItemPtr,
                        None,
                        format!(
                            "The 'final' of the base type '{qname}' must not contain 'restriction'"
                        )
                        .as_str(),
                        Some(&qname),
                    );
                    return XmlParserErrors::XmlSchemapCosStRestricts2_3_2_2 as i32;
                }
                // 2.3.2.3 The {item type definition} must be validly derived
                // from the {base type definition}'s {item type definition} given
                // the empty set, as defined in Type Derivation OK (Simple) ($3.14.6).
                {
                    let base_item_type = (*(*typ).base_type).subtypes;
                    if base_item_type.is_null() || !wxs_is_simple(base_item_type) {
                        PERROR_INT!(
                            pctxt,
                            "xmlSchemaCheckCOSSTRestricts",
                            "failed to eval the item type of a base type"
                        );
                        return -1;
                    }
                    if item_type != base_item_type
                        && xml_schema_check_cosstderived_ok(
                            pctxt as XmlSchemaAbstractCtxtPtr,
                            item_type,
                            base_item_type,
                            0,
                        ) != 0
                    {
                        let q1 = xml_schema_get_component_qname(item_type as _);
                        let q2 = xml_schema_get_component_qname(base_item_type as _);
                        let q3 = xml_schema_get_component_qname((*typ).base_type as _);
                        xml_schema_pcustom_err_ext(
                            pctxt,
                            XmlParserErrors::XmlSchemapCosStRestricts2_3_2_3,
                            typ as XmlSchemaBasicItemPtr,
                            None,
                            format!("The item type '{q1}' is not validly derived from the item type '{q2}' of the base type '{q3}'").as_str(),
                            Some(&q1),
                            Some(&q2),
                            Some(&q3)
                        );
                        return XmlParserErrors::XmlSchemapCosStRestricts2_3_2_3 as i32;
                    }
                }

                if !(*typ).facets.is_null() {
                    let mut facet: XmlSchemaFacetPtr;
                    let mut ok: i32 = 1;
                    // 2.3.2.4 Only length, minLength, maxLength, whiteSpace, pattern
                    // and enumeration facet components are allowed among the {facets}.
                    facet = (*typ).facets;
                    while {
                        match (*facet).typ {
                        XmlSchemaTypeType::XmlSchemaFacetLength
                        | XmlSchemaTypeType::XmlSchemaFacetMinLength
                        | XmlSchemaTypeType::XmlSchemaFacetMaxLength
                        | XmlSchemaTypeType::XmlSchemaFacetWhitespace
                        // TODO: 2.5.1.2 List datatypes
                        // The value of `whiteSpace` is fixed to the value collapse.
                        | XmlSchemaTypeType::XmlSchemaFacetPattern
                        | XmlSchemaTypeType::XmlSchemaFacetEnumeration => {}
                        _ => {
                            xml_schema_pillegal_facet_list_union_err(
                                pctxt,
                                XmlParserErrors::XmlSchemapCosStRestricts2_3_2_4,
                                typ,
                                facet,
                            );
                            // We could return, but it's nicer to report all invalid facets.
                            ok = 0;
                        }
                    }
                        facet = (*facet).next;
                        !facet.is_null()
                    } {}
                    if ok == 0 {
                        return XmlParserErrors::XmlSchemapCosStRestricts2_3_2_4 as i32;
                    }
                    // SPEC (2.3.2.5) (same as 1.3.2)
                    //
                    // NOTE (2.3.2.5) This is currently done in
                    // xmlSchemaDeriveAndValidateFacets()
                }
            }
        } else if (*typ).wxs_is_union() {
            // 3.1 The {member type definitions} must all have {variety} of atomic or list.
            let mut member: XmlSchemaTypeLinkPtr;

            member = (*typ).member_types;
            while !member.is_null() {
                if WXS_IS_TYPE_NOT_FIXED!((*member).typ) {
                    xml_schema_type_fixup((*member).typ, pctxt as XmlSchemaAbstractCtxtPtr);
                }

                if !(*(*member).typ).wxs_is_atomic() && !(*(*member).typ).wxs_is_list() {
                    let qname = xml_schema_get_component_qname((*member).typ as _);
                    xml_schema_pcustom_err(
                        pctxt,
                        XmlParserErrors::XmlSchemapCosStRestricts3_1,
                        typ as XmlSchemaBasicItemPtr,
                        None,
                        format!("The member type '{qname}' is neither an atomic, nor a list type")
                            .as_str(),
                        Some(&qname),
                    );
                    return XmlParserErrors::XmlSchemapCosStRestricts3_1 as i32;
                }
                member = (*member).next;
            }
            // 3.3.1 If the {base type definition} is the `simple ur-type definition`
            if (*(*typ).base_type).built_in_type == XmlSchemaValType::XmlSchemasAnySimpletype as i32
            {
                // 3.3.1.1 All of the {member type definitions} must have a
                // {final} which does not contain union.
                member = (*typ).member_types;
                while !member.is_null() {
                    if xml_schema_type_final_contains((*member).typ, XML_SCHEMAS_TYPE_FINAL_UNION)
                        != 0
                    {
                        let qname = xml_schema_get_component_qname((*member).typ as _);
                        xml_schema_pcustom_err(
                            pctxt,
                            XmlParserErrors::XmlSchemapCosStRestricts3_3_1,
                            typ as XmlSchemaBasicItemPtr,
                            None,
                            format!("The 'final' of member type '{qname}' contains 'union'")
                                .as_str(),
                            Some(&qname),
                        );
                        return XmlParserErrors::XmlSchemapCosStRestricts3_3_1 as i32;
                    }
                    member = (*member).next;
                }
                // 3.3.1.2 The {facets} must be empty.
                if !(*typ).facet_set.is_null() {
                    xml_schema_pcustom_err(
                        pctxt,
                        XmlParserErrors::XmlSchemapCosStRestricts3_3_1_2,
                        typ as XmlSchemaBasicItemPtr,
                        None,
                        "No facets allowed",
                        None,
                    );
                    return XmlParserErrors::XmlSchemapCosStRestricts3_3_1_2 as i32;
                }
            } else {
                // 3.3.2.1 The {base type definition} must have a {variety} of union.
                // I.e. the variety of "list" is inherited.
                if !(*(*typ).base_type).wxs_is_union() {
                    let qname = xml_schema_get_component_qname((*typ).base_type as _);
                    xml_schema_pcustom_err(
                        pctxt,
                        XmlParserErrors::XmlSchemapCosStRestricts3_3_2_1,
                        typ as XmlSchemaBasicItemPtr,
                        None,
                        format!("The base type '{qname}' is not a union type").as_str(),
                        Some(&qname),
                    );
                    return XmlParserErrors::XmlSchemapCosStRestricts3_3_2_1 as i32;
                }
                // 3.3.2.2 The {final} of the {base type definition} must not contain restriction.
                if xml_schema_type_final_contains(
                    (*typ).base_type,
                    XML_SCHEMAS_TYPE_FINAL_RESTRICTION,
                ) != 0
                {
                    let qname = xml_schema_get_component_qname((*typ).base_type as _);
                    xml_schema_pcustom_err(
                        pctxt,
                        XmlParserErrors::XmlSchemapCosStRestricts3_3_2_2,
                        typ as XmlSchemaBasicItemPtr,
                        None,
                        format!(
                            "The 'final' of its base type '{qname}' must not contain 'restriction'"
                        )
                        .as_str(),
                        Some(&qname),
                    );
                    return XmlParserErrors::XmlSchemapCosStRestricts3_3_2_2 as i32;
                }
                // 3.3.2.3 The {member type definitions}, in order, must be validly
                // derived from the corresponding type definitions in the {base
                // type definition}'s {member type definitions} given the empty set,
                // as defined in Type Derivation OK (Simple) ($3.14.6).
                {
                    let mut base_member: XmlSchemaTypeLinkPtr;

                    // OPTIMIZE: if the type is restricting, it has no local defined
                    // member types and inherits the member types of the base type;
                    // thus a check for equality can be skipped.
                    // Even worse: I cannot see a scenario where a restricting
                    // union simple type can have other member types as the member
                    // types of it's base type. This check seems not necessary with
                    // respect to the derivation process in libxml2.
                    // But necessary if constructing types with an API.
                    if !(*typ).member_types.is_null() {
                        member = (*typ).member_types;
                        base_member =
                            xml_schema_get_union_simple_type_member_types((*typ).base_type);
                        if member.is_null() && !base_member.is_null() {
                            PERROR_INT!(
                                pctxt,
                                "xmlSchemaCheckCOSSTRestricts",
                                "different number of member types in base"
                            );
                        }
                        while !member.is_null() {
                            if base_member.is_null() {
                                PERROR_INT!(
                                    pctxt,
                                    "xmlSchemaCheckCOSSTRestricts",
                                    "different number of member types in base"
                                );
                            } else if (*member).typ != (*base_member).typ
                                && xml_schema_check_cosstderived_ok(
                                    pctxt as XmlSchemaAbstractCtxtPtr,
                                    (*member).typ,
                                    (*base_member).typ,
                                    0,
                                ) != 0
                            {
                                let q1 = xml_schema_get_component_qname((*member).typ as _);
                                let q2 = xml_schema_get_component_qname((*base_member).typ as _);
                                let q3 = xml_schema_get_component_qname((*typ).base_type as _);

                                xml_schema_pcustom_err_ext(
                                    pctxt,
                                    XmlParserErrors::XmlSchemapCosStRestricts3_3_2_3,
                                    typ as XmlSchemaBasicItemPtr,
                                    None,
                                    format!("The member type {q1} is not validly derived from its corresponding member type {q2} of the base type {q3}").as_str(),
                                    Some(&q1),
                                    Some(&q2),
                                    Some(&q3)
                                );
                                return XmlParserErrors::XmlSchemapCosStRestricts3_3_2_3 as i32;
                            }
                            member = (*member).next;
                            if !base_member.is_null() {
                                base_member = (*base_member).next;
                            }
                        }
                    }
                }
                // 3.3.2.4 Only pattern and enumeration facet components are
                // allowed among the {facets}.
                if !(*typ).facets.is_null() {
                    let mut facet: XmlSchemaFacetPtr;
                    let mut ok: i32 = 1;

                    facet = (*typ).facets;
                    while {
                        if !matches!(
                            (*facet).typ,
                            XmlSchemaTypeType::XmlSchemaFacetPattern
                                | XmlSchemaTypeType::XmlSchemaFacetEnumeration
                        ) {
                            xml_schema_pillegal_facet_list_union_err(
                                pctxt,
                                XmlParserErrors::XmlSchemapCosStRestricts3_3_2_4,
                                typ,
                                facet,
                            );
                            ok = 0;
                        }
                        facet = (*facet).next;
                        !facet.is_null()
                    } {}
                    if ok == 0 {
                        return XmlParserErrors::XmlSchemapCosStRestricts3_3_2_4 as i32;
                    }
                }
                // SPEC (3.3.2.5) (same as 1.3.2)
                //
                // NOTE (3.3.2.5) This is currently done in
                // xmlSchemaDeriveAndValidateFacets()
            }
        }

        0
    }
}

unsafe fn xml_schema_create_vctxt_on_pctxt(ctxt: XmlSchemaParserCtxtPtr) -> i32 {
    unsafe {
        if (*ctxt).vctxt.is_null() {
            (*ctxt).vctxt = xml_schema_new_valid_ctxt(null_mut());
            if (*ctxt).vctxt.is_null() {
                xml_schema_perr(
                    ctxt,
                    None,
                    XmlParserErrors::XmlSchemapInternal,
                    "Internal error: xmlSchemaCreateVCtxtOnPCtxt, failed to create a temp. validation context.\n",
                    None,
                    None,
                );
                return -1;
            }
            // TODO: Pass user data.
            xml_schema_set_valid_errors(
                (*ctxt).vctxt,
                (*ctxt).error,
                (*ctxt).warning,
                (*ctxt).err_ctxt.clone(),
            );
            xml_schema_set_valid_structured_errors(
                (*ctxt).vctxt,
                (*ctxt).serror,
                (*ctxt).err_ctxt.clone(),
            );
        }
        0
    }
}

/// Checks the default values types, especially for facets
#[doc(alias = "xmlSchemaCheckFacetValues")]
unsafe fn xml_schema_check_facet_values(
    type_decl: XmlSchemaTypePtr,
    pctxt: XmlSchemaParserCtxtPtr,
) -> i32 {
    unsafe {
        let mut res: i32;
        let olderrs: i32 = (*pctxt).nberrors;
        let name: *const XmlChar = (*type_decl).name;
        // NOTE: It is intended to use the facets list, instead of facetSet.
        'exit_failure: {
            if !(*type_decl).facets.is_null() {
                let mut facet: XmlSchemaFacetPtr = (*type_decl).facets;

                // Temporarily assign the "schema" to the validation context
                // of the parser context. This is needed for NOTATION validation.
                if (*pctxt).vctxt.is_null() && xml_schema_create_vctxt_on_pctxt(pctxt) == -1 {
                    return -1;
                }
                (*(*pctxt).vctxt).schema = (*pctxt).schema;
                while !facet.is_null() {
                    res = xml_schema_check_facet(facet, type_decl, pctxt, name);
                    HFAILURE!(res, 'exit_failure);
                    facet = (*facet).next;
                }
                (*(*pctxt).vctxt).schema = null_mut();
            }
            if olderrs != (*pctxt).nberrors {
                return (*pctxt).err;
            }
            return 0;
        }
        // exit_failure:
        -1
    }
}

unsafe fn xml_schema_type_fixup_whitespace(typ: XmlSchemaTypePtr) -> i32 {
    unsafe {
        // Evaluate the whitespace-facet value.
        if (*typ).wxs_is_list() {
            (*typ).flags |= XML_SCHEMAS_TYPE_WHITESPACE_COLLAPSE;
            return 0;
        } else if (*typ).wxs_is_union() {
            return 0;
        }

        if !(*typ).facet_set.is_null() {
            let mut lin: XmlSchemaFacetLinkPtr;

            lin = (*typ).facet_set;
            while !lin.is_null() {
                if (*(*lin).facet).typ == XmlSchemaTypeType::XmlSchemaFacetWhitespace {
                    match (*(*lin).facet).whitespace {
                        w if XML_SCHEMAS_FACET_PRESERVE == w => {
                            (*typ).flags |= XML_SCHEMAS_TYPE_WHITESPACE_PRESERVE;
                        }
                        w if XML_SCHEMAS_FACET_REPLACE == w => {
                            (*typ).flags |= XML_SCHEMAS_TYPE_WHITESPACE_REPLACE;
                        }
                        w if XML_SCHEMAS_FACET_COLLAPSE == w => {
                            (*typ).flags |= XML_SCHEMAS_TYPE_WHITESPACE_COLLAPSE;
                        }
                        _ => {
                            return -1;
                        }
                    }
                    return 0;
                }
                lin = (*lin).next;
            }
        }
        // For all `atomic` datatypes other than string (and types `derived`
        // by `restriction` from it) the value of whiteSpace is fixed to collapse
        {
            let mut anc: XmlSchemaTypePtr;

            anc = (*typ).base_type;
            while !anc.is_null()
                && (*anc).built_in_type != XmlSchemaValType::XmlSchemasAnytype as i32
            {
                if (*anc).typ == XmlSchemaTypeType::XmlSchemaTypeBasic {
                    if (*anc).built_in_type == XmlSchemaValType::XmlSchemasNormString as i32 {
                        (*typ).flags |= XML_SCHEMAS_TYPE_WHITESPACE_REPLACE;
                    } else if (*anc).built_in_type == XmlSchemaValType::XmlSchemasString as i32
                        || (*anc).built_in_type == XmlSchemaValType::XmlSchemasAnySimpletype as i32
                    {
                        (*typ).flags |= XML_SCHEMAS_TYPE_WHITESPACE_PRESERVE;
                    } else {
                        (*typ).flags |= XML_SCHEMAS_TYPE_WHITESPACE_COLLAPSE;
                    }
                    break;
                }

                anc = (*anc).base_type;
            }
        }
        0
    }
}

macro_rules! FACET_RESTR_MUTUAL_ERR {
    ($pctxt:expr, $fac1:expr, $fac2:expr) => {{
        let ft1 = xml_schema_facet_type_to_string((*$fac1).typ);
        let ft2 = xml_schema_facet_type_to_string((*$fac2).typ);
        xml_schema_pcustom_err_ext(
            $pctxt,
            XmlParserErrors::XmlSchemapInvalidFacetValue,
            $fac1 as XmlSchemaBasicItemPtr,
            (*$fac1).node.map(|node| node.into()),
            format!("It is an error for both '{ft1}' and '{ft2}' to be specified on the same type definition").as_str(),
            Some(ft1),
            Some(ft2),
            None,
        )
    }};
}

macro_rules! FACET_RESTR_ERR {
    ($pctxt:expr, $fac1:expr, $msg:expr) => {
        xml_schema_pcustom_err(
            $pctxt,
            XmlParserErrors::XmlSchemapInvalidFacetValue,
            $fac1 as XmlSchemaBasicItemPtr,
            (*$fac1).node.map(|node| node.into()),
            $msg,
            None,
        );
    };
}

macro_rules! FACET_RESTR_FIXED_ERR {
    ($pctxt:expr, $fac:expr) => {
        xml_schema_pcustom_err(
            $pctxt,
            XmlParserErrors::XmlSchemapInvalidFacetValue,
            $fac as XmlSchemaBasicItemPtr,
            (*$fac).node.map(|node| node.into()),
            "The base type's facet is 'fixed', thus the value must not differ",
            None,
        )
    };
}

// Schema Component Constraint: Simple Type Restriction (Facets)
// (st-restrict-facets)
#[doc(alias = "xmlSchemaDeriveAndValidateFacets")]
unsafe fn xml_schema_derive_and_validate_facets(
    pctxt: XmlSchemaParserCtxtPtr,
    typ: XmlSchemaTypePtr,
) -> i32 {
    unsafe {
        let base: XmlSchemaTypePtr = (*typ).base_type;
        let mut link: XmlSchemaFacetLinkPtr;
        let mut cur: XmlSchemaFacetLinkPtr;
        let mut last: XmlSchemaFacetLinkPtr;
        let mut facet: XmlSchemaFacetPtr;
        let mut bfacet: XmlSchemaFacetPtr;
        let mut flength: XmlSchemaFacetPtr = null_mut();
        let mut ftotdig: XmlSchemaFacetPtr = null_mut();
        let mut ffracdig: XmlSchemaFacetPtr = null_mut();
        let mut fmaxlen: XmlSchemaFacetPtr = null_mut();
        let mut fminlen: XmlSchemaFacetPtr = null_mut(); /* facets of the current type */
        let mut fmininc: XmlSchemaFacetPtr = null_mut();
        let mut fmaxinc: XmlSchemaFacetPtr = null_mut();
        let mut fminexc: XmlSchemaFacetPtr = null_mut();
        let mut fmaxexc: XmlSchemaFacetPtr = null_mut();
        let mut bflength: XmlSchemaFacetPtr = null_mut();
        let mut bftotdig: XmlSchemaFacetPtr = null_mut();
        let mut bffracdig: XmlSchemaFacetPtr = null_mut();
        let mut bfmaxlen: XmlSchemaFacetPtr = null_mut();
        let mut bfminlen: XmlSchemaFacetPtr = null_mut(); /* facets of the base type */
        let mut bfmininc: XmlSchemaFacetPtr = null_mut();
        let mut bfmaxinc: XmlSchemaFacetPtr = null_mut();
        let mut bfminexc: XmlSchemaFacetPtr = null_mut();
        let mut bfmaxexc: XmlSchemaFacetPtr = null_mut();
        let mut res: i32; /* err = 0, fixedErr; */

        // SPEC st-restrict-facets 1:
        // "The {variety} of R is the same as that of B."

        // SPEC st-restrict-facets 2:
        // "If {variety} is atomic, the {primitive type definition}
        // of R is the same as that of B."
        //
        // NOTE: we leave 1 & 2 out for now, since this will be
        // satisfied by the derivation process.
        // CONSTRUCTION TODO: Maybe needed if using a construction API.

        // SPEC st-restrict-facets 3:
        // "The {facets} of R are the union of S and the {facets}
        // of B, eliminating duplicates. To eliminate duplicates,
        // when a facet of the same kind occurs in both S and the
        // {facets} of B, the one in the {facets} of B is not
        // included, with the exception of enumeration and pattern
        // facets, for which multiple occurrences with distinct values
        // are allowed."

        if (*typ).facet_set.is_null() && (*base).facet_set.is_null() {
            return 0;
        }

        last = (*typ).facet_set;
        if !last.is_null() {
            while !(*last).next.is_null() {
                last = (*last).next;
            }
        }

        cur = (*typ).facet_set;
        while !cur.is_null() {
            facet = (*cur).facet;
            match (*facet).typ {
                XmlSchemaTypeType::XmlSchemaFacetLength => {
                    flength = facet;
                }
                XmlSchemaTypeType::XmlSchemaFacetMinLength => {
                    fminlen = facet;
                }
                XmlSchemaTypeType::XmlSchemaFacetMinInclusive => {
                    fmininc = facet;
                }
                XmlSchemaTypeType::XmlSchemaFacetMinExclusive => {
                    fminexc = facet;
                }
                XmlSchemaTypeType::XmlSchemaFacetMaxLength => {
                    fmaxlen = facet;
                }
                XmlSchemaTypeType::XmlSchemaFacetMaxInclusive => {
                    fmaxinc = facet;
                }
                XmlSchemaTypeType::XmlSchemaFacetMaxExclusive => {
                    fmaxexc = facet;
                }
                XmlSchemaTypeType::XmlSchemaFacetTotalDigits => {
                    ftotdig = facet;
                }
                XmlSchemaTypeType::XmlSchemaFacetFractionDigits => {
                    ffracdig = facet;
                }
                _ => {}
            }
            cur = (*cur).next;
        }
        cur = (*base).facet_set;
        while !cur.is_null() {
            facet = (*cur).facet;
            match (*facet).typ {
                XmlSchemaTypeType::XmlSchemaFacetLength => {
                    bflength = facet;
                }
                XmlSchemaTypeType::XmlSchemaFacetMinLength => {
                    bfminlen = facet;
                }
                XmlSchemaTypeType::XmlSchemaFacetMinInclusive => {
                    bfmininc = facet;
                }
                XmlSchemaTypeType::XmlSchemaFacetMinExclusive => {
                    bfminexc = facet;
                }
                XmlSchemaTypeType::XmlSchemaFacetMaxLength => {
                    bfmaxlen = facet;
                }
                XmlSchemaTypeType::XmlSchemaFacetMaxInclusive => {
                    bfmaxinc = facet;
                }
                XmlSchemaTypeType::XmlSchemaFacetMaxExclusive => {
                    bfmaxexc = facet;
                }
                XmlSchemaTypeType::XmlSchemaFacetTotalDigits => {
                    bftotdig = facet;
                }
                XmlSchemaTypeType::XmlSchemaFacetFractionDigits => {
                    bffracdig = facet;
                }
                _ => {}
            }
            cur = (*cur).next;
        }
        // length and minLength or maxLength (2.2) + (3.2)
        if !flength.is_null() && (!fminlen.is_null() || !fmaxlen.is_null()) {
            FACET_RESTR_ERR!(
                pctxt,
                flength,
                "It is an error for both 'length' and either of 'minLength' or 'maxLength' to be specified on  the same type definition"
            );
        }
        // Mutual exclusions in the same derivation step.
        if !fmaxinc.is_null() && !fmaxexc.is_null() {
            // SCC "maxInclusive and maxExclusive"
            FACET_RESTR_MUTUAL_ERR!(pctxt, fmaxinc, fmaxexc);
        }
        if !fmininc.is_null() && !fminexc.is_null() {
            // SCC "minInclusive and minExclusive"
            FACET_RESTR_MUTUAL_ERR!(pctxt, fmininc, fminexc)
        }

        'internal_error: {
            if !flength.is_null() && !bflength.is_null() {
                // SCC "length valid restriction"
                // The values have to be equal.
                res = xml_schema_compare_values((*flength).val, (*bflength).val);
                if res == -2 {
                    break 'internal_error;
                }
                if res != 0 {
                    xml_schema_derive_facet_err(pctxt, flength, bflength, 0, 0, 1);
                }
                if res != 0 && (*bflength).fixed != 0 {
                    FACET_RESTR_FIXED_ERR!(pctxt, flength)
                }
            }
            if !fminlen.is_null() && !bfminlen.is_null() {
                // SCC "minLength valid restriction"
                // minLength >= BASE minLength
                res = xml_schema_compare_values((*fminlen).val, (*bfminlen).val);
                if res == -2 {
                    break 'internal_error;
                }
                if res == -1 {
                    xml_schema_derive_facet_err(pctxt, fminlen, bfminlen, 1, 1, 1);
                }
                if res != 0 && (*bfminlen).fixed != 0 {
                    FACET_RESTR_FIXED_ERR!(pctxt, fminlen)
                }
            }
            if !fmaxlen.is_null() && !bfmaxlen.is_null() {
                // SCC "maxLength valid restriction"
                // maxLength <= BASE minLength
                res = xml_schema_compare_values((*fmaxlen).val, (*bfmaxlen).val);
                if res == -2 {
                    break 'internal_error;
                }
                if res == 1 {
                    xml_schema_derive_facet_err(pctxt, fmaxlen, bfmaxlen, -1, 1, 1);
                }
                if res != 0 && (*bfmaxlen).fixed != 0 {
                    FACET_RESTR_FIXED_ERR!(pctxt, fmaxlen)
                }
            }
            // SCC "length and minLength or maxLength"
            if flength.is_null() {
                flength = bflength;
            }
            if !flength.is_null() {
                if fminlen.is_null() {
                    fminlen = bfminlen;
                }
                if !fminlen.is_null() {
                    // (1.1) length >= minLength
                    res = xml_schema_compare_values((*flength).val, (*fminlen).val);
                    if res == -2 {
                        break 'internal_error;
                    }
                    if res == -1 {
                        xml_schema_derive_facet_err(pctxt, flength, fminlen, 1, 1, 0);
                    }
                }
                if fmaxlen.is_null() {
                    fmaxlen = bfmaxlen;
                }
                if !fmaxlen.is_null() {
                    // (2.1) length <= maxLength
                    res = xml_schema_compare_values((*flength).val, (*fmaxlen).val);
                    if res == -2 {
                        break 'internal_error;
                    }
                    if res == 1 {
                        xml_schema_derive_facet_err(pctxt, flength, fmaxlen, -1, 1, 0);
                    }
                }
            }
            if !fmaxinc.is_null() {
                // "maxInclusive"
                if !fmininc.is_null() {
                    // SCC "maxInclusive >= minInclusive"
                    res = xml_schema_compare_values((*fmaxinc).val, (*fmininc).val);
                    if res == -2 {
                        break 'internal_error;
                    }
                    if res == -1 {
                        xml_schema_derive_facet_err(pctxt, fmaxinc, fmininc, 1, 1, 0);
                    }
                }
                // SCC "maxInclusive valid restriction"
                if !bfmaxinc.is_null() {
                    // maxInclusive <= BASE maxInclusive
                    res = xml_schema_compare_values((*fmaxinc).val, (*bfmaxinc).val);
                    if res == -2 {
                        break 'internal_error;
                    }
                    if res == 1 {
                        xml_schema_derive_facet_err(pctxt, fmaxinc, bfmaxinc, -1, 1, 1);
                    }
                    if res != 0 && (*bfmaxinc).fixed != 0 {
                        FACET_RESTR_FIXED_ERR!(pctxt, fmaxinc);
                    }
                }
                if !bfmaxexc.is_null() {
                    // maxInclusive < BASE maxExclusive
                    res = xml_schema_compare_values((*fmaxinc).val, (*bfmaxexc).val);
                    if res == -2 {
                        break 'internal_error;
                    }
                    if res != -1 {
                        xml_schema_derive_facet_err(pctxt, fmaxinc, bfmaxexc, -1, 0, 1);
                    }
                }
                if !bfmininc.is_null() {
                    // maxInclusive >= BASE minInclusive
                    res = xml_schema_compare_values((*fmaxinc).val, (*bfmininc).val);
                    if res == -2 {
                        break 'internal_error;
                    }
                    if res == -1 {
                        xml_schema_derive_facet_err(pctxt, fmaxinc, bfmininc, 1, 1, 1);
                    }
                }
                if !bfminexc.is_null() {
                    // maxInclusive > BASE minExclusive
                    res = xml_schema_compare_values((*fmaxinc).val, (*bfminexc).val);
                    if res == -2 {
                        break 'internal_error;
                    }
                    if res != 1 {
                        xml_schema_derive_facet_err(pctxt, fmaxinc, bfminexc, 1, 0, 1);
                    }
                }
            }
            if !fmaxexc.is_null() {
                // "maxExclusive >= minExclusive"
                if !fminexc.is_null() {
                    res = xml_schema_compare_values((*fmaxexc).val, (*fminexc).val);
                    if res == -2 {
                        break 'internal_error;
                    }
                    if res == -1 {
                        xml_schema_derive_facet_err(pctxt, fmaxexc, fminexc, 1, 1, 0);
                    }
                }
                // "maxExclusive valid restriction"
                if !bfmaxexc.is_null() {
                    // maxExclusive <= BASE maxExclusive
                    res = xml_schema_compare_values((*fmaxexc).val, (*bfmaxexc).val);
                    if res == -2 {
                        break 'internal_error;
                    }
                    if res == 1 {
                        xml_schema_derive_facet_err(pctxt, fmaxexc, bfmaxexc, -1, 1, 1);
                    }
                    if res != 0 && (*bfmaxexc).fixed != 0 {
                        FACET_RESTR_FIXED_ERR!(pctxt, fmaxexc);
                    }
                }
                if !bfmaxinc.is_null() {
                    // maxExclusive <= BASE maxInclusive
                    res = xml_schema_compare_values((*fmaxexc).val, (*bfmaxinc).val);
                    if res == -2 {
                        break 'internal_error;
                    }
                    if res == 1 {
                        xml_schema_derive_facet_err(pctxt, fmaxexc, bfmaxinc, -1, 1, 1);
                    }
                }
                if !bfmininc.is_null() {
                    // maxExclusive > BASE minInclusive
                    res = xml_schema_compare_values((*fmaxexc).val, (*bfmininc).val);
                    if res == -2 {
                        break 'internal_error;
                    }
                    if res != 1 {
                        xml_schema_derive_facet_err(pctxt, fmaxexc, bfmininc, 1, 0, 1);
                    }
                }
                if !bfminexc.is_null() {
                    // maxExclusive > BASE minExclusive
                    res = xml_schema_compare_values((*fmaxexc).val, (*bfminexc).val);
                    if res == -2 {
                        break 'internal_error;
                    }
                    if res != 1 {
                        xml_schema_derive_facet_err(pctxt, fmaxexc, bfminexc, 1, 0, 1);
                    }
                }
            }
            if !fminexc.is_null() {
                // "minExclusive < maxInclusive"
                if !fmaxinc.is_null() {
                    res = xml_schema_compare_values((*fminexc).val, (*fmaxinc).val);
                    if res == -2 {
                        break 'internal_error;
                    }
                    if res != -1 {
                        xml_schema_derive_facet_err(pctxt, fminexc, fmaxinc, -1, 0, 0);
                    }
                }
                // "minExclusive valid restriction"
                if !bfminexc.is_null() {
                    // minExclusive >= BASE minExclusive
                    res = xml_schema_compare_values((*fminexc).val, (*bfminexc).val);
                    if res == -2 {
                        break 'internal_error;
                    }
                    if res == -1 {
                        xml_schema_derive_facet_err(pctxt, fminexc, bfminexc, 1, 1, 1);
                    }
                    if res != 0 && (*bfminexc).fixed != 0 {
                        FACET_RESTR_FIXED_ERR!(pctxt, fminexc)
                    }
                }
                if !bfmaxinc.is_null() {
                    // minExclusive <= BASE maxInclusive
                    res = xml_schema_compare_values((*fminexc).val, (*bfmaxinc).val);
                    if res == -2 {
                        break 'internal_error;
                    }
                    if res == 1 {
                        xml_schema_derive_facet_err(pctxt, fminexc, bfmaxinc, -1, 1, 1);
                    }
                }
                if !bfmininc.is_null() {
                    // minExclusive >= BASE minInclusive
                    res = xml_schema_compare_values((*fminexc).val, (*bfmininc).val);
                    if res == -2 {
                        break 'internal_error;
                    }
                    if res == -1 {
                        xml_schema_derive_facet_err(pctxt, fminexc, bfmininc, 1, 1, 1);
                    }
                }
                if !bfmaxexc.is_null() {
                    // minExclusive < BASE maxExclusive
                    res = xml_schema_compare_values((*fminexc).val, (*bfmaxexc).val);
                    if res == -2 {
                        break 'internal_error;
                    }
                    if res != -1 {
                        xml_schema_derive_facet_err(pctxt, fminexc, bfmaxexc, -1, 0, 1);
                    }
                }
            }
            if !fmininc.is_null() {
                // "minInclusive < maxExclusive"
                if !fmaxexc.is_null() {
                    res = xml_schema_compare_values((*fmininc).val, (*fmaxexc).val);
                    if res == -2 {
                        break 'internal_error;
                    }
                    if res != -1 {
                        xml_schema_derive_facet_err(pctxt, fmininc, fmaxexc, -1, 0, 0);
                    }
                }
                // "minExclusive valid restriction"
                if !bfmininc.is_null() {
                    // minInclusive >= BASE minInclusive
                    res = xml_schema_compare_values((*fmininc).val, (*bfmininc).val);
                    if res == -2 {
                        break 'internal_error;
                    }
                    if res == -1 {
                        xml_schema_derive_facet_err(pctxt, fmininc, bfmininc, 1, 1, 1);
                    }
                    if res != 0 && (*bfmininc).fixed != 0 {
                        FACET_RESTR_FIXED_ERR!(pctxt, fmininc)
                    }
                }
                if !bfmaxinc.is_null() {
                    // minInclusive <= BASE maxInclusive
                    res = xml_schema_compare_values((*fmininc).val, (*bfmaxinc).val);
                    if res == -2 {
                        break 'internal_error;
                    }
                    if res == 1 {
                        xml_schema_derive_facet_err(pctxt, fmininc, bfmaxinc, -1, 1, 1);
                    }
                }
                if !bfminexc.is_null() {
                    // minInclusive > BASE minExclusive
                    res = xml_schema_compare_values((*fmininc).val, (*bfminexc).val);
                    if res == -2 {
                        break 'internal_error;
                    }
                    if res != 1 {
                        xml_schema_derive_facet_err(pctxt, fmininc, bfminexc, 1, 0, 1);
                    }
                }
                if !bfmaxexc.is_null() {
                    // minInclusive < BASE maxExclusive
                    res = xml_schema_compare_values((*fmininc).val, (*bfmaxexc).val);
                    if res == -2 {
                        break 'internal_error;
                    }
                    if res != -1 {
                        xml_schema_derive_facet_err(pctxt, fmininc, bfmaxexc, -1, 0, 1);
                    }
                }
            }
            if !ftotdig.is_null() && !bftotdig.is_null() {
                // SCC " totalDigits valid restriction"
                // totalDigits <= BASE totalDigits
                res = xml_schema_compare_values((*ftotdig).val, (*bftotdig).val);
                if res == -2 {
                    break 'internal_error;
                }
                if res == 1 {
                    xml_schema_derive_facet_err(pctxt, ftotdig, bftotdig, -1, 1, 1);
                }
                if res != 0 && (*bftotdig).fixed != 0 {
                    FACET_RESTR_FIXED_ERR!(pctxt, ftotdig)
                }
            }
            if !ffracdig.is_null() && !bffracdig.is_null() {
                // SCC  "fractionDigits valid restriction"
                // fractionDigits <= BASE fractionDigits
                res = xml_schema_compare_values((*ffracdig).val, (*bffracdig).val);
                if res == -2 {
                    break 'internal_error;
                }
                if res == 1 {
                    xml_schema_derive_facet_err(pctxt, ffracdig, bffracdig, -1, 1, 1);
                }
                if res != 0 && (*bffracdig).fixed != 0 {
                    FACET_RESTR_FIXED_ERR!(pctxt, ffracdig);
                }
            }
            // SCC "fractionDigits less than or equal to totalDigits"
            if ftotdig.is_null() {
                ftotdig = bftotdig;
            }
            if ffracdig.is_null() {
                ffracdig = bffracdig;
            }
            if !ftotdig.is_null() && !ffracdig.is_null() {
                res = xml_schema_compare_values((*ffracdig).val, (*ftotdig).val);
                if res == -2 {
                    break 'internal_error;
                }
                if res == 1 {
                    xml_schema_derive_facet_err(pctxt, ffracdig, ftotdig, -1, 1, 0);
                }
            }
            // *Enumerations* won' be added here, since only the first set
            // of enumerations in the ancestor-or-self axis is used
            // for validation, plus we need to use the base type of those
            // enumerations for whitespace.
            //
            // *Patterns*: won't be add here, since they are ORed at
            // type level and ANDed at ancestor level. This will
            // happen during validation by walking the base axis
            // of the type.
            cur = (*base).facet_set;
            while !cur.is_null() {
                'to_continue: {
                    bfacet = (*cur).facet;
                    // Special handling of enumerations and patterns.
                    // TODO: hmm, they should not appear in the set, so remove this.
                    if matches!(
                        (*bfacet).typ,
                        XmlSchemaTypeType::XmlSchemaFacetPattern
                            | XmlSchemaTypeType::XmlSchemaFacetEnumeration
                    ) {
                        break 'to_continue;
                    }
                    // Search for a duplicate facet in the current type.
                    link = (*typ).facet_set;
                    // err = 0;
                    // fixedErr = 0;
                    while !link.is_null() {
                        facet = (*link).facet;
                        if (*facet).typ == (*bfacet).typ {
                            if (*facet).typ == XmlSchemaTypeType::XmlSchemaFacetWhitespace {
                                // The whitespace must be stronger.
                                if (*facet).whitespace < (*bfacet).whitespace {
                                    FACET_RESTR_ERR!(
                                        pctxt,
                                        facet,
                                        "The 'whitespace' value has to be equal to or stronger than the 'whitespace' value of the base type"
                                    );
                                }
                                if (*bfacet).fixed != 0
                                    && (*facet).whitespace != (*bfacet).whitespace
                                {
                                    FACET_RESTR_FIXED_ERR!(pctxt, facet);
                                }
                            }
                            // Duplicate found.
                            break;
                        }
                        link = (*link).next;
                    }
                    // If no duplicate was found: add the base types's facet to the set.
                    if link.is_null() {
                        link = xml_malloc(size_of::<XmlSchemaFacetLink>()) as XmlSchemaFacetLinkPtr;
                        if link.is_null() {
                            xml_schema_perr_memory(
                                pctxt,
                                "deriving facets, creating a facet link",
                                None,
                            );
                            return -1;
                        }
                        (*link).facet = (*cur).facet;
                        (*link).next = null_mut();
                        if last.is_null() {
                            (*typ).facet_set = link;
                        } else {
                            (*last).next = link;
                        }
                        last = link;
                    }
                }
                cur = (*cur).next;
            }

            return 0;
        }
        // internal_error:
        PERROR_INT!(
            pctxt,
            "xmlSchemaDeriveAndValidateFacets",
            "an error occurred"
        );
        -1
    }
}

unsafe fn xml_schema_type_fixup_optim_facets(typ: XmlSchemaTypePtr) {
    unsafe {
        let mut has: i32;
        let mut need_val: i32 = 0;
        let mut norm_val: i32 = 0;

        has = ((*(*typ).base_type).flags & XML_SCHEMAS_TYPE_HAS_FACETS != 0) as i32;
        if has != 0 {
            need_val = ((*(*typ).base_type).flags & XML_SCHEMAS_TYPE_FACETSNEEDVALUE != 0) as i32;
            norm_val = ((*(*typ).base_type).flags & XML_SCHEMAS_TYPE_NORMVALUENEEDED != 0) as i32;
        }
        if !(*typ).facets.is_null() {
            let mut fac: XmlSchemaFacetPtr;

            fac = (*typ).facets;
            while !fac.is_null() {
                match (*fac).typ {
                    XmlSchemaTypeType::XmlSchemaFacetWhitespace => {}
                    XmlSchemaTypeType::XmlSchemaFacetPattern => {
                        norm_val = 1;
                        has = 1;
                    }
                    XmlSchemaTypeType::XmlSchemaFacetEnumeration => {
                        need_val = 1;
                        norm_val = 1;
                        has = 1;
                    }
                    _ => {
                        has = 1;
                    }
                }
                fac = (*fac).next;
            }
        }
        if norm_val != 0 {
            (*typ).flags |= XML_SCHEMAS_TYPE_NORMVALUENEEDED;
        }
        if need_val != 0 {
            (*typ).flags |= XML_SCHEMAS_TYPE_FACETSNEEDVALUE;
        }
        if has != 0 {
            (*typ).flags |= XML_SCHEMAS_TYPE_HAS_FACETS;
        }

        if has != 0 && need_val == 0 && (*typ).wxs_is_atomic() {
            let prim: XmlSchemaTypePtr = xml_schema_get_primitive_type(typ);
            // OPTIMIZE VAL TODO: Some facets need a computed value.
            if (*prim).built_in_type != XmlSchemaValType::XmlSchemasAnySimpletype as i32
                && (*prim).built_in_type != XmlSchemaValType::XmlSchemasString as i32
            {
                (*typ).flags |= XML_SCHEMAS_TYPE_FACETSNEEDVALUE;
            }
        }
    }
}

// 3.14.6 Constraints on Simple Type Definition Schema Components
unsafe fn xml_schema_fixup_simple_type_stage_two(
    pctxt: XmlSchemaParserCtxtPtr,
    typ: XmlSchemaTypePtr,
) -> i32 {
    unsafe {
        let mut res: i32;
        let olderrs: i32 = (*pctxt).nberrors;

        if (*typ).typ != XmlSchemaTypeType::XmlSchemaTypeSimple {
            return -1;
        }

        if !WXS_IS_TYPE_NOT_FIXED!(typ) {
            return 0;
        }

        (*typ).flags |= XML_SCHEMAS_TYPE_INTERNAL_RESOLVED;
        (*typ).content_type = XmlSchemaContentType::XmlSchemaContentSimple;

        'exit_failure: {
            if (*typ).base_type.is_null() {
                PERROR_INT!(
                    pctxt,
                    "xmlSchemaFixupSimpleTypeStageTwo",
                    "missing baseType"
                );
                break 'exit_failure;
            }
            if WXS_IS_TYPE_NOT_FIXED!((*typ).base_type) {
                xml_schema_type_fixup((*typ).base_type, pctxt as XmlSchemaAbstractCtxtPtr);
            }
            // If a member type of a union is a union itself, we need to substitute
            // that member type for its member types.
            // NOTE that this might change in WXS 1.1; i.e. we will keep the union
            // types in WXS 1.1.
            if !(*typ).member_types.is_null()
                && xml_schema_finish_member_type_definitions_property(pctxt, typ) == -1
            {
                return -1;
            }
            // SPEC src-simple-type 1
            // "The corresponding simple type definition, if any, must satisfy
            // the conditions set out in Constraints on Simple Type Definition
            // Schema Components ($3.14.6)."

            // Schema Component Constraint: Simple Type Definition Properties Correct
            // (st-props-correct)
            res = xml_schema_check_st_props_correct(pctxt, typ);
            'exit_error: {
                HFAILURE!(res, 'exit_failure);
                HERROR!(res, 'exit_error);
                // Schema Component Constraint: Derivation Valid (Restriction, Simple)
                // (cos-st-restricts)
                res = xml_schema_check_cosstrestricts(pctxt, typ);
                HFAILURE!(res, 'exit_failure);
                HERROR!(res, 'exit_error);

                // TODO: Removed the error report, since it got annoying to get an
                // extra error report, if anything failed until now.
                // Enable this if needed.
                //
                // xmlSchemaPErr(ctxt, (*typ).node,
                //    XML_SCHEMAP_SRC_SIMPLE_TYPE_1,
                //    "Simple type '%s' does not satisfy the constraints "
                //    "on simple type definitions.\n".as_ptr() as _,
                //    (*typ).name, null_mut());

                // Schema Component Constraint: Simple Type Restriction (Facets)
                // (st-restrict-facets)
                res = xml_schema_check_facet_values(typ, pctxt);
                HFAILURE!(res, 'exit_failure);
                HERROR!(res, 'exit_error);

                if !(*typ).facet_set.is_null() || !(*(*typ).base_type).facet_set.is_null() {
                    res = xml_schema_derive_and_validate_facets(pctxt, typ);
                    HFAILURE!(res, 'exit_failure);
                    HERROR!(res, 'exit_error);
                }
                // Whitespace value.
                res = xml_schema_type_fixup_whitespace(typ);
                HFAILURE!(res, 'exit_failure);
                HERROR!(res, 'exit_error);

                xml_schema_type_fixup_optim_facets(typ);
            }

            // exit_error:
            if olderrs != (*pctxt).nberrors {
                return (*pctxt).err;
            }
            return 0;
        }

        // exit_failure:
        -1
    }
}

/// Schema Component Constraint:
///    Attribute Declaration Properties Correct (a-props-correct)
///
/// Validates the value constraints of an attribute declaration/use.
/// NOTE that this needs the simple type definitions to be already built and checked.
#[doc(alias = "xmlSchemaCheckAttrPropsCorrect")]
unsafe fn xml_schema_check_attr_props_correct(
    pctxt: XmlSchemaParserCtxtPtr,
    attr: XmlSchemaAttributePtr,
) -> i32 {
    unsafe {
        // SPEC a-props-correct (1)
        // "The values of the properties of an attribute declaration must
        // be as described in the property tableau in The Attribute
        // Declaration Schema Component ($3.2.1), modulo the impact of
        // Missing Sub-components ($5.3)."

        if (*attr).subtypes.is_null() {
            return 0;
        }

        if !(*attr).def_value.is_null() {
            // SPEC a-props-correct (3)
            // "If the {type definition} is or is derived from ID then there
            // must not be a {value constraint}."
            if xml_schema_is_derived_from_built_in_type(
                (*attr).subtypes,
                XmlSchemaValType::XmlSchemasID as i32,
            ) != 0
            {
                xml_schema_custom_err(
                    pctxt as XmlSchemaAbstractCtxtPtr,
                    XmlParserErrors::XmlSchemapAPropsCorrect3,
                    None,
                    attr as XmlSchemaBasicItemPtr,
                    "Value constraints are not allowed if the type definition is or is derived from xs:ID",
                    None,
                    None,
                );
                return (*pctxt).err;
            }
            // SPEC a-props-correct (2)
            // "if there is a {value constraint}, the canonical lexical
            // representation of its value must be `valid` with respect
            // to the {type definition} as defined in String Valid ($3.14.4)."
            // TODO: Don't care about the *canonical* stuff here, this requirement
            // will be removed in WXS 1.1 anyway.
            let ret: i32 = xml_schema_vcheck_cvc_simple_type(
                pctxt as XmlSchemaAbstractCtxtPtr,
                (*attr).node.map(|node| node.into()),
                (*attr).subtypes,
                (*attr).def_value,
                &raw mut (*attr).def_val,
                1,
                1,
                0,
            );
            if ret != 0 {
                if ret < 0 {
                    PERROR_INT!(
                        pctxt,
                        "xmlSchemaCheckAttrPropsCorrect",
                        "calling xmlSchemaVCheckCVCSimpleType()"
                    );
                    return -1;
                }
                xml_schema_custom_err(
                    pctxt as XmlSchemaAbstractCtxtPtr,
                    XmlParserErrors::XmlSchemapAPropsCorrect2,
                    None,
                    attr as XmlSchemaBasicItemPtr,
                    "The value of the value constraint is not valid",
                    None,
                    None,
                );
                return (*pctxt).err;
            }
        }

        0
    }
}

/// Schema Component Constraint:
/// Attribute Use Correct (au-props-correct)
#[doc(alias = "xmlSchemaCheckAttrUsePropsCorrect")]
unsafe fn xml_schema_check_attr_use_props_correct(
    ctxt: XmlSchemaParserCtxtPtr,
    using: XmlSchemaAttributeUsePtr,
) -> i32 {
    unsafe {
        if ctxt.is_null() || using.is_null() {
            return -1;
        }
        if (*using).def_value.is_null()
            || WXS_ATTRUSE_DECL!(using).is_null()
            || (*WXS_ATTRUSE_DECL!(using)).typ != XmlSchemaTypeType::XmlSchemaTypeAttribute
        {
            return 0;
        }

        // SPEC au-props-correct (1)
        // "The values of the properties of an attribute use must be as
        // described in the property tableau in The Attribute Use Schema
        // Component ($3.5.1), modulo the impact of Missing
        // Sub-components ($5.3)."

        if !(*WXS_ATTRUSE_DECL!(using)).def_value.is_null()
            && (*WXS_ATTRUSE_DECL!(using)).flags & XML_SCHEMAS_ATTR_FIXED != 0
            && (*using).flags & XML_SCHEMA_ATTR_USE_FIXED == 0
        {
            xml_schema_pcustom_err(
                ctxt,
                XmlParserErrors::XmlSchemapAuPropsCorrect2,
                using as XmlSchemaBasicItemPtr,
                None,
                "The attribute declaration has a 'fixed' value constraint , thus the attribute use must also have a 'fixed' value constraint",
                None,
            );
            return (*ctxt).err;
        }
        // Compute and check the value constraint's value.
        if !(*using).def_val.is_null() && !WXS_ATTRUSE_TYPEDEF!(using).is_null() {
            // TODO: The spec seems to be missing a check of the
            // value constraint of the attribute use. We will do it here.
            // SPEC a-props-correct (3)
            if xml_schema_is_derived_from_built_in_type(
                WXS_ATTRUSE_TYPEDEF!(using),
                XmlSchemaValType::XmlSchemasID as i32,
            ) != 0
            {
                xml_schema_custom_err(
                    ctxt as XmlSchemaAbstractCtxtPtr,
                    XmlParserErrors::XmlSchemapAuPropsCorrect,
                    None,
                    using as XmlSchemaBasicItemPtr,
                    "Value constraints are not allowed if the type definition is or is derived from xs:ID",
                    None,
                    None,
                );
                return (*ctxt).err;
            }

            let ret: i32 = xml_schema_vcheck_cvc_simple_type(
                ctxt as XmlSchemaAbstractCtxtPtr,
                Some(XmlGenericNodePtr::from((*using).node)),
                WXS_ATTRUSE_TYPEDEF!(using),
                (*using).def_value,
                &raw mut (*using).def_val,
                1,
                1,
                0,
            );
            if ret != 0 {
                if ret < 0 {
                    PERROR_INT2!(
                        ctxt,
                        "xmlSchemaCheckAttrUsePropsCorrect",
                        "calling xmlSchemaVCheckCVCSimpleType()"
                    );
                    return -1;
                }
                xml_schema_custom_err(
                    ctxt as XmlSchemaAbstractCtxtPtr,
                    XmlParserErrors::XmlSchemapAuPropsCorrect,
                    None,
                    using as XmlSchemaBasicItemPtr,
                    "The value of the value constraint is not valid",
                    None,
                    None,
                );
                return (*ctxt).err;
            }
        }
        // SPEC au-props-correct (2)
        // "If the {attribute declaration} has a fixed
        // {value constraint}, then if the attribute use itself has a
        // {value constraint}, it must also be fixed and its value must match
        // that of the {attribute declaration}'s {value constraint}."
        if !(*WXS_ATTRUSE_DECL!(using)).def_val.is_null()
            && (*WXS_ATTRUSE_DECL!(using)).flags & XML_SCHEMA_ATTR_USE_FIXED == 0
        {
            if xml_schema_are_values_equal((*using).def_val, (*WXS_ATTRUSE_DECL!(using)).def_val)
                == 0
            {
                let def_value = CStr::from_ptr((*WXS_ATTRUSE_DECL!(using)).def_value as *const i8)
                    .to_string_lossy();
                xml_schema_pcustom_err(
                ctxt,
                XmlParserErrors::XmlSchemapAuPropsCorrect2,
                using as XmlSchemaBasicItemPtr,
                None,
                format!("The 'fixed' value constraint of the attribute use must match the attribute declaration's value constraint '{def_value}'").as_str(),
                Some(&def_value)
            );
            }
            return (*ctxt).err;
        }
        0
    }
}

/// Substitutes contained attribute group references
/// for their attribute uses. Wildcards are intersected.
///
/// Schema Component Constraint:
///    Attribute Group Definition Properties Correct (ag-props-correct)
#[doc(alias = "xmlSchemaAttributeGroupExpandRefs")]
unsafe fn xml_schema_check_agprops_correct(
    pctxt: XmlSchemaParserCtxtPtr,
    attr_gr: XmlSchemaAttributeGroupPtr,
) -> i32 {
    unsafe {
        // SPEC ag-props-correct
        // (1) "The values of the properties of an attribute group definition
        // must be as described in the property tableau in The Attribute
        // Group Definition Schema Component ($3.6.1), modulo the impact of
        // Missing Sub-components ($5.3);"

        if !(*attr_gr).attr_uses.is_null()
            && (*((*attr_gr).attr_uses as XmlSchemaItemListPtr<*mut c_void>))
                .items
                .len()
                > 1
        {
            let uses: XmlSchemaItemListPtr<*mut c_void> =
                (*attr_gr).attr_uses as XmlSchemaItemListPtr<*mut c_void>;
            let mut has_id: i32 = 0;

            'next_use: for (i, using) in (*uses)
                .items
                .iter()
                .map(|&using| using as XmlSchemaAttributeUsePtr)
                .enumerate()
                .rev()
            {
                // SPEC ag-props-correct
                // (2) "Two distinct members of the {attribute uses} must not have
                // {attribute declaration}s both of whose {name}s match and whose
                // {target namespace}s are identical."
                if i > 0 {
                    for tmp in (*uses)
                        .items
                        .iter()
                        .take(i)
                        .rev()
                        .map(|&tmp| tmp as XmlSchemaAttributeUsePtr)
                    {
                        if WXS_ATTRUSE_DECL_NAME!(using) == WXS_ATTRUSE_DECL_NAME!(tmp)
                            && WXS_ATTRUSE_DECL_TNS!(using) == WXS_ATTRUSE_DECL_TNS!(tmp)
                        {
                            let str1 = xml_schema_get_component_designation(using as _);

                            xml_schema_custom_err(
                                pctxt as XmlSchemaAbstractCtxtPtr,
                                XmlParserErrors::XmlSchemapAgPropsCorrect,
                                (*attr_gr).node.map(|node| node.into()),
                                attr_gr as XmlSchemaBasicItemPtr,
                                format!("Duplicate {str1}").as_str(),
                                Some(&str1),
                                None,
                            );
                            // Remove the duplicate.
                            if (*uses).remove(i) == -1 {
                                return -1;
                            }
                            // goto next_use;
                            continue 'next_use;
                        }
                    }
                }
                // SPEC ag-props-correct
                // (3) "Two distinct members of the {attribute uses} must not have
                // {attribute declaration}s both of whose {type definition}s are or
                // are derived from ID."
                // TODO: Does 'derived' include member-types of unions?
                if !WXS_ATTRUSE_TYPEDEF!(using).is_null()
                    && xml_schema_is_derived_from_built_in_type(
                        WXS_ATTRUSE_TYPEDEF!(using),
                        XmlSchemaValType::XmlSchemasID as i32,
                    ) != 0
                {
                    if has_id != 0 {
                        let str1 = xml_schema_get_component_designation(using as _);

                        xml_schema_custom_err(
                            pctxt as XmlSchemaAbstractCtxtPtr,
                            XmlParserErrors::XmlSchemapAgPropsCorrect,
                            (*attr_gr).node.map(|node| node.into()  ),
                            attr_gr as XmlSchemaBasicItemPtr,
                            format!("There must not exist more than one attribute declaration of type 'xs:ID' (or derived from 'xs:ID'). The {str1} violates this constraint").as_str(),
                            Some(&str1),
                            None
                        );
                        if (*uses).remove(i) == -1 {
                            return -1;
                        }
                    }
                    has_id = 1;
                }
                // next_use: {}
            }
        }
        0
    }
}

unsafe fn xml_schema_check_src_redefine_second(pctxt: XmlSchemaParserCtxtPtr) -> i32 {
    unsafe {
        let mut err: i32;
        let mut redef: XmlSchemaRedefPtr = (*WXS_CONSTRUCTOR!(pctxt)).redefs;
        let mut item: XmlSchemaBasicItemPtr;

        if redef.is_null() {
            return 0;
        }

        while {
            'to_continue: {
                if (*redef).target.is_null() {
                    redef = (*redef).next;
                    break 'to_continue;
                }
                item = (*redef).item;

                match (*item).typ {
                    XmlSchemaTypeType::XmlSchemaTypeSimple
                    | XmlSchemaTypeType::XmlSchemaTypeComplex => {
                        // Since the spec wants the {name} of the redefined
                        // type to be 'absent', we'll NULL it.
                        (*((*redef).target as XmlSchemaTypePtr)).name = null_mut();

                        // TODO: Seems like there's nothing more to do. The normal
                        // inheritance mechanism is used. But not 100% sure.
                    }
                    XmlSchemaTypeType::XmlSchemaTypeGroup => {
                        // URGENT TODO:
                        // SPEC src-redefine:
                        // (6.2.2) "The {model group} of the model group definition
                        // which corresponds to it per XML Representation of Model
                        // Group Definition Schema Components ($3.7.2) must be a
                        // `valid restriction` of the {model group} of that model
                        // group definition in I, as defined in Particle Valid
                        // (Restriction) ($3.9.6)."
                    }
                    XmlSchemaTypeType::XmlSchemaTypeAttributeGroup => {
                        // SPEC src-redefine:
                        // (7.2.2) "The {attribute uses} and {attribute wildcard} of
                        // the attribute group definition which corresponds to it
                        // per XML Representation of Attribute Group Definition Schema
                        // Components ($3.6.2) must be `valid restrictions` of the
                        // {attribute uses} and {attribute wildcard} of that attribute
                        // group definition in I, as defined in clause 2, clause 3 and
                        // clause 4 of Derivation Valid (Restriction, Complex)
                        // ($3.4.6) (where references to the base type definition are
                        // understood as references to the attribute group definition
                        // in I)."
                        err = xml_schema_check_derivation_okrestriction2to4(
                            pctxt,
                            XML_SCHEMA_ACTION_REDEFINE,
                            item,
                            (*redef).target,
                            (*(item as XmlSchemaAttributeGroupPtr)).attr_uses as _,
                            (*((*redef).target as XmlSchemaAttributeGroupPtr)).attr_uses as _,
                            (*(item as XmlSchemaAttributeGroupPtr)).attribute_wildcard,
                            (*((*redef).target as XmlSchemaAttributeGroupPtr)).attribute_wildcard,
                        );
                        if err == -1 {
                            return -1;
                        }
                    }
                    _ => {}
                }
                redef = (*redef).next;
            }
            !redef.is_null()
        } {}
        0
    }
}

unsafe fn xml_schema_check_subst_group_circular(
    elem_decl: XmlSchemaElementPtr,
    ancestor: XmlSchemaElementPtr,
) -> XmlSchemaElementPtr {
    unsafe {
        if (*ancestor).ref_decl.is_null() {
            return null_mut();
        }
        if (*ancestor).ref_decl == elem_decl {
            return ancestor;
        }

        if (*(*ancestor).ref_decl).flags & XML_SCHEMAS_ELEM_CIRCULAR != 0 {
            return null_mut();
        }
        (*(*ancestor).ref_decl).flags |= XML_SCHEMAS_ELEM_CIRCULAR;
        let ret: XmlSchemaElementPtr =
            xml_schema_check_subst_group_circular(elem_decl, (*ancestor).ref_decl);
        (*(*ancestor).ref_decl).flags ^= XML_SCHEMAS_ELEM_CIRCULAR;

        ret
    }
}

unsafe fn xml_schema_are_equal_types(type_a: XmlSchemaTypePtr, type_b: XmlSchemaTypePtr) -> i32 {
    // TODO: This should implement component-identity in the future.
    if type_a.is_null() || type_b.is_null() {
        return 0;
    }
    (type_a == type_b) as i32
}

/// Schema Component Constraint:
/// Type Derivation OK (Complex) (cos-ct-derived-ok)
///
/// STATUS: completed
///
/// Returns 0 if the constraints are satisfied, or 1 if not.
#[doc(alias = "xmlSchemaCheckCOSCTDerivedOK")]
unsafe fn xml_schema_check_cos_ct_derived_ok(
    actxt: XmlSchemaAbstractCtxtPtr,
    typ: XmlSchemaTypePtr,
    base_type: XmlSchemaTypePtr,
    set: i32,
) -> i32 {
    unsafe {
        let equal: i32 = xml_schema_are_equal_types(typ, base_type);
        // TODO: Error codes.

        // SPEC "For a complex type definition (call it D, for derived)
        // to be validly derived from a type definition (call this
        // B, for base) given a subset of {extension, restriction}
        // all of the following must be true:"
        if equal == 0 {
            // SPEC (1) "If B and D are not the same type definition, then the
            // {derivation method} of D must not be in the subset."
            if (set & SUBSET_EXTENSION != 0 && (*typ).wxs_is_extension())
                || (set & SUBSET_RESTRICTION != 0 && (*typ).wxs_is_restriction())
            {
                return 1;
            }
        } else {
            // SPEC (2.1) "B and D must be the same type definition."
            return 0;
        }
        // SPEC (2.2) "B must be D's {base type definition}."
        if (*typ).base_type == base_type {
            return 0;
        }
        // SPEC (2.3.1) "D's {base type definition} must not be the `ur-type definition`."
        if wxs_is_anytype((*typ).base_type) {
            return 1;
        }

        if wxs_is_complex((*typ).base_type) {
            // SPEC (2.3.2.1) "If D's {base type definition} is complex, then it
            // must be validly derived from B given the subset as defined by this constraint."
            xml_schema_check_cos_ct_derived_ok(actxt, (*typ).base_type, base_type, set)
        } else {
            // SPEC (2.3.2.2) "If D's {base type definition} is simple, then it
            // must be validly derived from B given the subset as defined in Type
            // Derivation OK (Simple) ($3.14.6).
            xml_schema_check_cosstderived_ok(actxt, (*typ).base_type, base_type, set)
        }
    }
}

/// Calls:
/// Type Derivation OK (Simple) AND Type Derivation OK (Complex)
///
/// Checks whether @type can be validly derived from @baseType.
///
/// Returns 0 on success, an positive error code otherwise.
#[doc(alias = "xmlSchemaCheckCOSDerivedOK")]
unsafe fn xml_schema_check_cos_derived_ok(
    actxt: XmlSchemaAbstractCtxtPtr,
    typ: XmlSchemaTypePtr,
    base_type: XmlSchemaTypePtr,
    set: i32,
) -> i32 {
    unsafe {
        if wxs_is_simple(typ) {
            xml_schema_check_cosstderived_ok(actxt, typ, base_type, set)
        } else {
            xml_schema_check_cos_ct_derived_ok(actxt, typ, base_type, set)
        }
    }
}

/// Schema Component Constraint: Element Default Valid (Immediate)
/// (cos-valid-default)
/// This will be used by the parser only. For the validator there's an other version.
///
/// Returns 0 if the constraints are satisfied,
/// if not, a positive error code and -1 on internal errors.
#[doc(alias = "xmlSchemaParseCheckCOSValidDefault")]
unsafe fn xml_schema_parse_check_cos_valid_default(
    pctxt: XmlSchemaParserCtxtPtr,
    node: Option<XmlGenericNodePtr>,
    typ: XmlSchemaTypePtr,
    value: *const XmlChar,
    val: *mut XmlSchemaValPtr,
) -> i32 {
    unsafe {
        let mut ret: i32 = 0;

        // cos-valid-default:
        // Schema Component Constraint: Element Default Valid (Immediate)
        // For a string to be a valid default with respect to a type
        // definition the appropriate case among the following must be true:
        if wxs_is_complex(typ) {
            // Complex type.
            //
            // SPEC (2.1) "its {content type} must be a simple type definition
            // or mixed."
            // SPEC (2.2.2) "If the {content type} is mixed, then the {content
            // type}'s particle must be `emptiable` as defined by
            // Particle Emptiable ($3.9.6)."
            if !WXS_HAS_SIMPLE_CONTENT!(typ)
                && (!WXS_HAS_MIXED_CONTENT!(typ) || !WXS_EMPTIABLE!(typ))
            {
                // NOTE that this covers (2.2.2) as well.
                xml_schema_pcustom_err(
                    pctxt,
                    XmlParserErrors::XmlSchemapCosValidDefault2_1,
                    typ as XmlSchemaBasicItemPtr,
                    (*typ).node.map(|node| node.into()),
                    "For a string to be a valid default, the type definition must be a simple type or a complex type with mixed content and a particle emptiable",
                    None,
                );
                return XmlParserErrors::XmlSchemapCosValidDefault2_1 as i32;
            }
        }
        // 1 If the type definition is a simple type definition, then the string
        // must be `valid` with respect to that definition as defined by String
        // Valid ($3.14.4).
        //
        // AND
        //
        // 2.2.1 If the {content type} is a simple type definition, then the
        // string must be `valid` with respect to that simple type definition
        // as defined by String Valid ($3.14.4).
        if wxs_is_simple(typ) {
            ret = xml_schema_vcheck_cvc_simple_type(
                pctxt as XmlSchemaAbstractCtxtPtr,
                node,
                typ,
                value,
                val,
                1,
                1,
                0,
            );
        } else if WXS_HAS_SIMPLE_CONTENT!(typ) {
            ret = xml_schema_vcheck_cvc_simple_type(
                pctxt as XmlSchemaAbstractCtxtPtr,
                node,
                (*typ).content_type_def,
                value,
                val,
                1,
                1,
                0,
            );
        } else {
            return ret;
        }

        if ret < 0 {
            PERROR_INT!(
                pctxt,
                "xmlSchemaParseCheckCOSValidDefault",
                "calling xmlSchemaVCheckCVCSimpleType()"
            );
        }

        ret
    }
}

/// Schema Component Constraint:
/// Element Declaration Properties Correct (e-props-correct)
///
/// STATUS:
///   missing: (6)
#[doc(alias = "xmlSchemaCheckElemPropsCorrect")]
unsafe fn xml_schema_check_elem_props_correct(
    pctxt: XmlSchemaParserCtxtPtr,
    elem_decl: XmlSchemaElementPtr,
) -> i32 {
    unsafe {
        let mut ret: i32 = 0;
        let type_def: XmlSchemaTypePtr = (*elem_decl).subtypes;
        // SPEC (1) "The values of the properties of an element declaration
        // must be as described in the property tableau in The Element
        // Declaration Schema Component ($3.3.1), modulo the impact of Missing
        // Sub-components ($5.3)."
        if !(*elem_decl).ref_decl.is_null() {
            let head: XmlSchemaElementPtr = (*elem_decl).ref_decl;
            let circ: XmlSchemaElementPtr;

            xml_schema_check_element_decl_component(head, pctxt);
            // SPEC (3) "If there is a non-`absent` {substitution group
            // affiliation}, then {scope} must be global."
            if (*elem_decl).flags & XML_SCHEMAS_ELEM_GLOBAL == 0 {
                xml_schema_pcustom_err(
                    pctxt,
                    XmlParserErrors::XmlSchemapEPropsCorrect3,
                    elem_decl as XmlSchemaBasicItemPtr,
                    None,
                    "Only global element declarations can have a substitution group affiliation",
                    None,
                );
                ret = XmlParserErrors::XmlSchemapEPropsCorrect3 as i32;
            }
            // TODO: SPEC (6) "Circular substitution groups are disallowed.
            // That is, it must not be possible to return to an element declaration
            // by repeatedly following the {substitution group affiliation} property."
            if head == elem_decl {
                circ = head;
            } else if !(*head).ref_decl.is_null() {
                circ = xml_schema_check_subst_group_circular(head, head);
            } else {
                circ = null_mut();
            }
            if !circ.is_null() {
                let q1 = xml_schema_get_component_qname(circ as _);
                let q2 = xml_schema_get_component_qname(head as _);

                xml_schema_pcustom_err_ext(
                    pctxt,
                    XmlParserErrors::XmlSchemapEPropsCorrect6,
                    circ as XmlSchemaBasicItemPtr,
                    None,
                    format!("The element declaration '{q1}' defines a circular substitution group to element declaration '{q2}'").as_str(),
                    Some(&q1),
                    Some(&q2),
                    None
                );
                ret = XmlParserErrors::XmlSchemapEPropsCorrect6 as i32;
            }
            // SPEC (4) "If there is a {substitution group affiliation},
            // the {type definition}
            // of the element declaration must be validly derived from the {type
            // definition} of the {substitution group affiliation}, given the value
            // of the {substitution group exclusions} of the {substitution group
            // affiliation}, as defined in Type Derivation OK (Complex) ($3.4.6)
            // (if the {type definition} is complex) or as defined in
            // Type Derivation OK (Simple) ($3.14.6) (if the {type definition} is
            // simple)."
            //
            // NOTE: {substitution group exclusions} means the values of the
            // attribute "final".

            if type_def != (*((*elem_decl).ref_decl)).subtypes {
                let mut set: i32 = 0;

                if (*head).flags & XML_SCHEMAS_ELEM_FINAL_EXTENSION != 0 {
                    set |= SUBSET_EXTENSION;
                }
                if (*head).flags & XML_SCHEMAS_ELEM_FINAL_RESTRICTION != 0 {
                    set |= SUBSET_RESTRICTION;
                }

                if xml_schema_check_cos_derived_ok(
                    pctxt as XmlSchemaAbstractCtxtPtr,
                    type_def,
                    (*head).subtypes,
                    set,
                ) != 0
                {
                    let q1 = xml_schema_get_component_qname(type_def as _);
                    let q2 = xml_schema_get_component_qname(head as _);
                    let q3 = xml_schema_get_component_qname((*head).subtypes as _);

                    ret = XmlParserErrors::XmlSchemapEPropsCorrect4 as i32;
                    xml_schema_pcustom_err_ext(
                        pctxt,
                        XmlParserErrors::XmlSchemapEPropsCorrect4,
                        elem_decl as XmlSchemaBasicItemPtr,
                        None,
                        format!("The type definition '{q1}' was either rejected by the substitution group affiliation '{q2}', or not validly derived from its type definition '{q3}'").as_str(),
                        Some(&q1),
                        Some(&q2),
                        Some(&q3)
                    );
                }
            }
        }
        // SPEC (5) "If the {type definition} or {type definition}'s
        // {content type}
        // is or is derived from ID then there must not be a {value constraint}.
        // Note: The use of ID as a type definition for elements goes beyond
        // XML 1.0, and should be avoided if backwards compatibility is desired"
        if !(*elem_decl).value.is_null()
            && ((wxs_is_simple(type_def)
                && xml_schema_is_derived_from_built_in_type(
                    type_def,
                    XmlSchemaValType::XmlSchemasID as i32,
                ) != 0)
                || (wxs_is_complex(type_def)
                    && WXS_HAS_SIMPLE_CONTENT!(type_def)
                    && xml_schema_is_derived_from_built_in_type(
                        (*type_def).content_type_def,
                        XmlSchemaValType::XmlSchemasID as i32,
                    ) != 0))
        {
            ret = XmlParserErrors::XmlSchemapEPropsCorrect5 as i32;
            xml_schema_pcustom_err(
                pctxt,
                XmlParserErrors::XmlSchemapEPropsCorrect5,
                elem_decl as XmlSchemaBasicItemPtr,
                None,
                "The type definition (or type definition's content type) is or is derived from ID; value constraints are not allowed in conjunction with such a type definition",
                None,
            );
        } else if !(*elem_decl).value.is_null() {
            // SPEC (2) "If there is a {value constraint}, the canonical lexical
            // representation of its value must be `valid` with respect to the
            // {type definition} as defined in Element Default Valid (Immediate) ($3.3.6)."
            if type_def.is_null() {
                xml_schema_perr(
                    pctxt,
                    (*elem_decl).node.map(|node| node.into()),
                    XmlParserErrors::XmlSchemapInternal,
                    "Internal error: xmlSchemaCheckElemPropsCorrect, type is missing... skipping validation of the value constraint",
                    None,
                    None,
                );
                return -1;
            }
            let node = if let Some(node) = (*elem_decl).node {
                let attr = if (*elem_decl).flags & XML_SCHEMAS_ELEM_FIXED != 0 {
                    node.has_prop("fixed")
                } else {
                    node.has_prop("default")
                };
                match attr {
                    Some(Ok(attr)) => Some(XmlGenericNodePtr::from(attr)),
                    Some(Err(attr)) => Some(XmlGenericNodePtr::from(attr)),
                    _ => None,
                }
            } else {
                None
            };
            let vcret: i32 = xml_schema_parse_check_cos_valid_default(
                pctxt,
                node,
                type_def,
                (*elem_decl).value,
                &raw mut (*elem_decl).def_val,
            );
            if vcret != 0 {
                if vcret < 0 {
                    PERROR_INT!(
                        pctxt,
                        "xmlSchemaElemCheckValConstr",
                        "failed to validate the value constraint of an element declaration"
                    );
                    return -1;
                }
                return vcret;
            }
        }

        ret
    }
}

unsafe fn xml_schema_subst_group_get(
    pctxt: XmlSchemaParserCtxtPtr,
    head: XmlSchemaElementPtr,
) -> XmlSchemaSubstGroupPtr {
    unsafe {
        if WXS_SUBST_GROUPS!(pctxt).is_null() {
            return null_mut();
        }
        xml_hash_lookup2(
            WXS_SUBST_GROUPS!(pctxt),
            (*head).name,
            (*head).target_namespace,
        ) as _
    }
}

unsafe fn xml_schema_subst_group_add(
    pctxt: XmlSchemaParserCtxtPtr,
    head: XmlSchemaElementPtr,
) -> XmlSchemaSubstGroupPtr {
    unsafe {
        // Init subst group hash.
        if WXS_SUBST_GROUPS!(pctxt).is_null() {
            WXS_SUBST_GROUPS!(pctxt) = xml_hash_create(10);
            if WXS_SUBST_GROUPS!(pctxt).is_null() {
                return null_mut();
            }
        }
        // Create a new substitution group.
        let ret: XmlSchemaSubstGroupPtr =
            xml_malloc(size_of::<XmlSchemaSubstGroup>()) as XmlSchemaSubstGroupPtr;
        if ret.is_null() {
            xml_schema_perr_memory(
                null_mut(),
                "allocating a substitution group container",
                None,
            );
            return null_mut();
        }
        memset(ret as _, 0, size_of::<XmlSchemaSubstGroup>());
        (*ret).head = head;
        // Create list of members.
        (*ret).members = xml_schema_item_list_create::<*mut c_void>();
        if (*ret).members.is_null() {
            xml_schema_subst_group_free(ret);
            return null_mut();
        }
        // Add subst group to hash.
        if xml_hash_add_entry2(
            WXS_SUBST_GROUPS!(pctxt),
            (*head).name,
            (*head).target_namespace,
            ret as _,
        ) != 0
        {
            PERROR_INT!(
                pctxt,
                "xmlSchemaSubstGroupAdd",
                "failed to add a new substitution container"
            );
            xml_schema_subst_group_free(ret);
            return null_mut();
        }
        ret
    }
}

/// Allocate a new annotation structure.
///
/// Returns the newly allocated structure or NULL in case or error
#[doc(alias = "xmlSchemaAddElementSubstitutionMember")]
unsafe fn xml_schema_add_element_substitution_member(
    pctxt: XmlSchemaParserCtxtPtr,
    head: XmlSchemaElementPtr,
    member: XmlSchemaElementPtr,
) -> i32 {
    unsafe {
        let mut subst_group: XmlSchemaSubstGroupPtr;

        if pctxt.is_null() || head.is_null() || member.is_null() {
            return -1;
        }

        subst_group = xml_schema_subst_group_get(pctxt, head);
        if subst_group.is_null() {
            subst_group = xml_schema_subst_group_add(pctxt, head);
        }
        if subst_group.is_null() {
            return -1;
        }
        if (*(*subst_group).members).push(member as _) == -1 {
            return -1;
        }
        0
    }
}

/// Schema Component Constraint:
/// Substitution Group (cos-equiv-class)
///
/// In Libxml2 the subst. groups will be precomputed, in terms of that
/// a list will be built for each subst. group head, holding all direct
/// referents to this head.
/// NOTE that this function needs:
///   1. circular subst. groups to be checked beforehand
///   2. the declaration's type to be derived from the head's type
///
/// STATUS:
///
#[doc(alias = "xmlSchemaCheckElemSubstGroup")]
unsafe fn xml_schema_check_elem_subst_group(
    ctxt: XmlSchemaParserCtxtPtr,
    elem_decl: XmlSchemaElementPtr,
) {
    unsafe {
        // SPEC (1) "Its {abstract} is false."
        if (*elem_decl).ref_decl.is_null() || (*elem_decl).flags & XML_SCHEMAS_ELEM_ABSTRACT != 0 {
            return;
        }
        {
            let mut head: XmlSchemaElementPtr;
            let mut head_type: XmlSchemaTypePtr;
            let mut typ: XmlSchemaTypePtr;
            let mut set: i32;
            let mut meth_set: i32;
            // SPEC (2) "It is validly substitutable for HEAD subject to HEAD's
            // {disallowed substitutions} as the blocking constraint, as defined in
            // Substitution Group OK (Transitive) ($3.3.6)."
            head = (*elem_decl).ref_decl;
            while !head.is_null() {
                'to_continue: {
                    set = 0;
                    meth_set = 0;
                    // The blocking constraints.
                    if (*head).flags & XML_SCHEMAS_ELEM_BLOCK_SUBSTITUTION != 0 {
                        break 'to_continue;
                    }
                    head_type = (*head).subtypes;
                    typ = (*elem_decl).subtypes;
                    if head_type == typ {
                        // goto add_member;
                    } else {
                        if (*head).flags & XML_SCHEMAS_ELEM_BLOCK_RESTRICTION != 0 {
                            set |= XML_SCHEMAS_TYPE_BLOCK_RESTRICTION;
                        }
                        if (*head).flags & XML_SCHEMAS_ELEM_BLOCK_EXTENSION != 0 {
                            set |= XML_SCHEMAS_TYPE_BLOCK_EXTENSION;
                        }
                        // SPEC: Substitution Group OK (Transitive) (2.3)
                        // "The set of all {derivation method}s involved in the
                        // derivation of D's {type definition} from C's {type definition}
                        // does not intersect with the union of the blocking constraint,
                        // C's {prohibited substitutions} (if C is complex, otherwise the
                        // empty set) and the {prohibited substitutions} (respectively the
                        // empty set) of any intermediate {type definition}s in the
                        // derivation of D's {type definition} from C's {type definition}."

                        // OPTIMIZE TODO: Optimize this a bit, since, if traversing the
                        // subst.head axis, the methSet does not need to be computed for
                        // the full depth over and over.

                        // The set of all {derivation method}s involved in the derivation
                        while !typ.is_null() && typ != head_type && typ != (*typ).base_type {
                            if (*typ).wxs_is_extension()
                                && meth_set & XML_SCHEMAS_TYPE_BLOCK_RESTRICTION == 0
                            {
                                meth_set |= XML_SCHEMAS_TYPE_BLOCK_EXTENSION;
                            }

                            if (*typ).wxs_is_restriction()
                                && meth_set & XML_SCHEMAS_TYPE_BLOCK_RESTRICTION == 0
                            {
                                meth_set |= XML_SCHEMAS_TYPE_BLOCK_RESTRICTION;
                            }

                            typ = (*typ).base_type;
                        }
                        // The {prohibited substitutions} of all intermediate types + the head's type.
                        typ = (*(*elem_decl).subtypes).base_type;
                        while !typ.is_null() {
                            if wxs_is_complex(typ) {
                                if (*typ).flags & XML_SCHEMAS_TYPE_BLOCK_EXTENSION != 0
                                    && set & XML_SCHEMAS_TYPE_BLOCK_EXTENSION == 0
                                {
                                    set |= XML_SCHEMAS_TYPE_BLOCK_EXTENSION;
                                }
                                if (*typ).flags & XML_SCHEMAS_TYPE_BLOCK_RESTRICTION != 0
                                    && set & XML_SCHEMAS_TYPE_BLOCK_RESTRICTION == 0
                                {
                                    set |= XML_SCHEMAS_TYPE_BLOCK_RESTRICTION;
                                }
                            } else {
                                break;
                            }
                            if typ == head_type {
                                break;
                            }
                            typ = (*typ).base_type;
                        }
                        if set != 0
                            && ((set & XML_SCHEMAS_TYPE_BLOCK_EXTENSION != 0
                                && meth_set & XML_SCHEMAS_TYPE_BLOCK_EXTENSION != 0)
                                || (set & XML_SCHEMAS_TYPE_BLOCK_RESTRICTION != 0
                                    && meth_set & XML_SCHEMAS_TYPE_BLOCK_RESTRICTION != 0))
                        {
                            break 'to_continue;
                        }
                    }
                    // add_member:
                    xml_schema_add_element_substitution_member(ctxt, head, elem_decl);
                    if (*head).flags & XML_SCHEMAS_ELEM_SUBST_GROUP_HEAD == 0 {
                        (*head).flags |= XML_SCHEMAS_ELEM_SUBST_GROUP_HEAD;
                    }
                }
                head = (*head).ref_decl;
            }
        }
    }
}

/// Validates the value constraints of an element declaration.
/// Adds substitution group members.
#[doc(alias = "xmlSchemaCheckElementDeclComponent")]
unsafe fn xml_schema_check_element_decl_component(
    elem_decl: XmlSchemaElementPtr,
    ctxt: XmlSchemaParserCtxtPtr,
) {
    unsafe {
        if elem_decl.is_null() {
            return;
        }
        if (*elem_decl).flags & XML_SCHEMAS_ELEM_INTERNAL_CHECKED != 0 {
            return;
        }
        (*elem_decl).flags |= XML_SCHEMAS_ELEM_INTERNAL_CHECKED;
        if xml_schema_check_elem_props_correct(ctxt, elem_decl) == 0 {
            // Adds substitution group members.
            xml_schema_check_elem_subst_group(ctxt, elem_decl);
        }
    }
}

/// Returns 1 if nillable, 0 otherwise
#[doc(alias = "xmlSchemaBuildContentModelForSubstGroup")]
unsafe fn xml_schema_build_content_model_for_subst_group(
    pctxt: XmlSchemaParserCtxtPtr,
    particle: XmlSchemaParticlePtr,
    mut counter: i32,
    mut end: XmlAutomataStatePtr,
) -> i32 {
    unsafe {
        let mut tmp: XmlAutomataStatePtr;
        let mut ret: i32 = 0;
        let elem_decl: XmlSchemaElementPtr = (*particle).children as XmlSchemaElementPtr;

        // Wrap the substitution group with a CHOICE.
        let start: XmlAutomataStatePtr = (*pctxt).state;
        if end.is_null() {
            end = xml_automata_new_state((*pctxt).am);
        }
        let subst_group: XmlSchemaSubstGroupPtr = xml_schema_subst_group_get(pctxt, elem_decl);
        if subst_group.is_null() {
            let name = CStr::from_ptr((*elem_decl).name as *const i8).to_string_lossy();
            xml_schema_perr(
                pctxt,
                xml_schema_get_component_node(particle as _).map(|node| node.into()),
                XmlParserErrors::XmlSchemapInternal,
                "Internal error: xmlSchemaBuildContentModelForSubstGroup, declaration is marked having a subst. group but none available.\n",
                Some(&name),
                None,
            );
            return 0;
        }
        if counter >= 0 {
            // NOTE that we put the declaration in, even if it's abstract.
            // However, an error will be raised during *validation* if an element
            // information item shall be validated against an abstract element
            // declaration.
            tmp = xml_automata_new_counted_trans((*pctxt).am, start, null_mut(), counter);
            xml_automata_new_transition2(
                (*pctxt).am,
                tmp,
                end,
                CStr::from_ptr((*elem_decl).name as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                (!(*elem_decl).target_namespace.is_null())
                    .then(|| {
                        CStr::from_ptr((*elem_decl).target_namespace as *const i8).to_string_lossy()
                    })
                    .as_deref(),
                elem_decl as _,
            );
            // Add subst. group members.
            for member in (*(*subst_group).members)
                .items
                .iter()
                .map(|&member| member as XmlSchemaElementPtr)
            {
                xml_automata_new_transition2(
                    (*pctxt).am,
                    tmp,
                    end,
                    CStr::from_ptr((*member).name as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    (!(*member).target_namespace.is_null())
                        .then(|| {
                            CStr::from_ptr((*member).target_namespace as *const i8)
                                .to_string_lossy()
                        })
                        .as_deref(),
                    member as _,
                );
            }
        } else if (*particle).max_occurs == 1 {
            // NOTE that we put the declaration in, even if it's abstract,
            xml_automata_new_epsilon(
                (*pctxt).am,
                xml_automata_new_transition2(
                    (*pctxt).am,
                    start,
                    null_mut(),
                    CStr::from_ptr((*elem_decl).name as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    (!(*elem_decl).target_namespace.is_null())
                        .then(|| {
                            CStr::from_ptr((*elem_decl).target_namespace as *const i8)
                                .to_string_lossy()
                        })
                        .as_deref(),
                    elem_decl as _,
                ),
                end,
            );
            // Add subst. group members.
            for member in (*(*subst_group).members)
                .items
                .iter()
                .map(|&member| member as XmlSchemaElementPtr)
            {
                // NOTE: This fixes bug #341150. xmlAutomataNewOnceTrans2()
                //  was incorrectly used instead of xmlAutomataNewTransition2()
                //  (seems like a copy&paste bug from the XmlSchemaTypeType::XmlSchemaTypeAll
                //  section in xmlSchemaBuildAContentModel() ).
                // TODO: Check if xmlAutomataNewOnceTrans2() was instead
                //  intended for the above "counter" section originally. I.e.,
                //  check xs:all with subst-groups.
                //
                // tmp = xmlAutomataNewOnceTrans2((*pctxt).am, start, null_mut(),
                //                   (*member).name, (*member).target_namespace,
                //               1, 1, member);
                tmp = xml_automata_new_transition2(
                    (*pctxt).am,
                    start,
                    null_mut(),
                    CStr::from_ptr((*member).name as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    (!(*member).target_namespace.is_null())
                        .then(|| {
                            CStr::from_ptr((*member).target_namespace as *const i8)
                                .to_string_lossy()
                        })
                        .as_deref(),
                    member as _,
                );
                xml_automata_new_epsilon((*pctxt).am, tmp, end);
            }
        } else {
            let max_occurs: i32 = if (*particle).max_occurs == UNBOUNDED as i32 {
                UNBOUNDED as i32
            } else {
                (*particle).max_occurs - 1
            };
            let min_occurs: i32 = if (*particle).min_occurs < 1 {
                0
            } else {
                (*particle).min_occurs - 1
            };

            counter = xml_automata_new_counter((*pctxt).am, min_occurs, max_occurs);
            let hop: XmlAutomataStatePtr = xml_automata_new_state((*pctxt).am);

            xml_automata_new_epsilon(
                (*pctxt).am,
                xml_automata_new_transition2(
                    (*pctxt).am,
                    start,
                    null_mut(),
                    CStr::from_ptr((*elem_decl).name as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    (!(*elem_decl).target_namespace.is_null())
                        .then(|| {
                            CStr::from_ptr((*elem_decl).target_namespace as *const i8)
                                .to_string_lossy()
                        })
                        .as_deref(),
                    elem_decl as _,
                ),
                hop,
            );
            // Add subst. group members.
            for member in (*(*subst_group).members)
                .items
                .iter()
                .map(|&member| member as XmlSchemaElementPtr)
            {
                xml_automata_new_epsilon(
                    (*pctxt).am,
                    xml_automata_new_transition2(
                        (*pctxt).am,
                        start,
                        null_mut(),
                        CStr::from_ptr((*member).name as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                        (!(*member).target_namespace.is_null())
                            .then(|| {
                                CStr::from_ptr((*member).target_namespace as *const i8)
                                    .to_string_lossy()
                            })
                            .as_deref(),
                        member as _,
                    ),
                    hop,
                );
            }
            xml_automata_new_counted_trans((*pctxt).am, hop, start, counter);
            xml_automata_new_counter_trans((*pctxt).am, hop, end, counter);
        }
        if (*particle).min_occurs == 0 {
            xml_automata_new_epsilon((*pctxt).am, start, end);
            ret = 1;
        }
        (*pctxt).state = end;
        ret
    }
}

/// Returns 1 if nillable, 0 otherwise
#[doc(alias = "xmlSchemaBuildContentModelForElement")]
unsafe fn xml_schema_build_content_model_for_element(
    ctxt: XmlSchemaParserCtxtPtr,
    particle: XmlSchemaParticlePtr,
) -> i32 {
    unsafe {
        let mut ret: i32 = 0;

        if (*((*particle).children as XmlSchemaElementPtr)).flags
            & XML_SCHEMAS_ELEM_SUBST_GROUP_HEAD
            != 0
        {
            // Substitution groups.
            ret = xml_schema_build_content_model_for_subst_group(ctxt, particle, -1, null_mut());
        } else {
            let start: XmlAutomataStatePtr;
            let elem_decl: XmlSchemaElementPtr = (*particle).children as XmlSchemaElementPtr;

            if (*elem_decl).flags & XML_SCHEMAS_ELEM_ABSTRACT != 0 {
                return 0;
            }
            if (*particle).max_occurs == 1 {
                start = (*ctxt).state;
                (*ctxt).state = xml_automata_new_transition2(
                    (*ctxt).am,
                    start,
                    null_mut(),
                    CStr::from_ptr((*elem_decl).name as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    (!(*elem_decl).target_namespace.is_null())
                        .then(|| {
                            CStr::from_ptr((*elem_decl).target_namespace as *const i8)
                                .to_string_lossy()
                        })
                        .as_deref(),
                    elem_decl as _,
                );
            } else if (*particle).max_occurs >= UNBOUNDED as i32 && (*particle).min_occurs < 2 {
                // Special case.
                start = (*ctxt).state;
                (*ctxt).state = xml_automata_new_transition2(
                    (*ctxt).am,
                    start,
                    null_mut(),
                    CStr::from_ptr((*elem_decl).name as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    (!(*elem_decl).target_namespace.is_null())
                        .then(|| {
                            CStr::from_ptr((*elem_decl).target_namespace as *const i8)
                                .to_string_lossy()
                        })
                        .as_deref(),
                    elem_decl as _,
                );
                (*ctxt).state = xml_automata_new_transition2(
                    (*ctxt).am,
                    (*ctxt).state,
                    (*ctxt).state,
                    CStr::from_ptr((*elem_decl).name as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    (!(*elem_decl).target_namespace.is_null())
                        .then(|| {
                            CStr::from_ptr((*elem_decl).target_namespace as *const i8)
                                .to_string_lossy()
                        })
                        .as_deref(),
                    elem_decl as _,
                );
            } else {
                let max_occurs: i32 = if (*particle).max_occurs == UNBOUNDED as i32 {
                    UNBOUNDED as i32
                } else {
                    (*particle).max_occurs - 1
                };
                let min_occurs: i32 = if (*particle).min_occurs < 1 {
                    0
                } else {
                    (*particle).min_occurs - 1
                };

                start = xml_automata_new_epsilon((*ctxt).am, (*ctxt).state, null_mut());
                let counter: i32 = xml_automata_new_counter((*ctxt).am, min_occurs, max_occurs);
                (*ctxt).state = xml_automata_new_transition2(
                    (*ctxt).am,
                    start,
                    null_mut(),
                    CStr::from_ptr((*elem_decl).name as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    (!(*elem_decl).target_namespace.is_null())
                        .then(|| {
                            CStr::from_ptr((*elem_decl).target_namespace as *const i8)
                                .to_string_lossy()
                        })
                        .as_deref(),
                    elem_decl as _,
                );
                xml_automata_new_counted_trans((*ctxt).am, (*ctxt).state, start, counter);
                (*ctxt).state =
                    xml_automata_new_counter_trans((*ctxt).am, (*ctxt).state, null_mut(), counter);
            }
            if (*particle).min_occurs == 0 {
                xml_automata_new_epsilon((*ctxt).am, start, (*ctxt).state);
                ret = 1;
            }
        }
        ret
    }
}

/// Create the automaton for the {content type} of a complex type.
///
/// Returns 1 if the content is nillable, 0 otherwise
#[doc(alias = "xmlSchemaBuildAContentModel")]
unsafe fn xml_schema_build_acontent_model(
    pctxt: XmlSchemaParserCtxtPtr,
    particle: XmlSchemaParticlePtr,
) -> i32 {
    unsafe {
        let mut ret: i32 = 0;
        let mut tmp2: i32;

        if particle.is_null() {
            PERROR_INT!(pctxt, "xmlSchemaBuildAContentModel", "particle is NULL");
            return 1;
        }
        if (*particle).children.is_null() {
            // Just return in this case. A missing "term" of the particle
            // might arise due to an invalid "term" component.
            return 1;
        }

        match (*(*particle).children).typ {
            XmlSchemaTypeType::XmlSchemaTypeAny => {
                let mut ns: XmlSchemaWildcardNsPtr;
                let wild: XmlSchemaWildcardPtr = (*particle).children as XmlSchemaWildcardPtr;
                let start: XmlAutomataStatePtr = (*pctxt).state;
                let end: XmlAutomataStatePtr = xml_automata_new_state((*pctxt).am);

                if (*particle).max_occurs == 1 {
                    if (*wild).any == 1 {
                        // We need to add both transitions:
                        //
                        // 1. the {"*", "*"} for elements in a namespace.
                        (*pctxt).state = xml_automata_new_transition2(
                            (*pctxt).am,
                            start,
                            null_mut(),
                            "*",
                            Some("*"),
                            wild as _,
                        );
                        xml_automata_new_epsilon((*pctxt).am, (*pctxt).state, end);
                        // 2. the {"*"} for elements in no namespace.
                        (*pctxt).state = xml_automata_new_transition2(
                            (*pctxt).am,
                            start,
                            null_mut(),
                            "*",
                            None,
                            wild as _,
                        );
                        xml_automata_new_epsilon((*pctxt).am, (*pctxt).state, end);
                    } else if !(*wild).ns_set.is_null() {
                        ns = (*wild).ns_set;
                        while {
                            (*pctxt).state = start;
                            (*pctxt).state = xml_automata_new_transition2(
                                (*pctxt).am,
                                (*pctxt).state,
                                null_mut(),
                                "*",
                                (!(*ns).value.is_null())
                                    .then(|| {
                                        CStr::from_ptr((*ns).value as *const i8).to_string_lossy()
                                    })
                                    .as_deref(),
                                wild as _,
                            );
                            xml_automata_new_epsilon((*pctxt).am, (*pctxt).state, end);
                            ns = (*ns).next;
                            !ns.is_null()
                        } {}
                    } else if !(*wild).neg_ns_set.is_null() {
                        (*pctxt).state = xml_automata_new_neg_trans(
                            (*pctxt).am,
                            start,
                            end,
                            "*",
                            (!(*(*wild).neg_ns_set).value.is_null())
                                .then(|| {
                                    CStr::from_ptr((*(*wild).neg_ns_set).value as *const i8)
                                        .to_string_lossy()
                                })
                                .as_deref(),
                            wild as _,
                        );
                    }
                } else {
                    let max_occurs: i32 = if (*particle).max_occurs == UNBOUNDED as i32 {
                        UNBOUNDED as i32
                    } else {
                        (*particle).max_occurs - 1
                    };
                    let min_occurs: i32 = if (*particle).min_occurs < 1 {
                        0
                    } else {
                        (*particle).min_occurs - 1
                    };

                    let counter: i32 =
                        xml_automata_new_counter((*pctxt).am, min_occurs, max_occurs);
                    let hop: XmlAutomataStatePtr = xml_automata_new_state((*pctxt).am);
                    if (*wild).any == 1 {
                        (*pctxt).state = xml_automata_new_transition2(
                            (*pctxt).am,
                            start,
                            null_mut(),
                            "*",
                            Some("*"),
                            wild as _,
                        );
                        xml_automata_new_epsilon((*pctxt).am, (*pctxt).state, hop);
                        (*pctxt).state = xml_automata_new_transition2(
                            (*pctxt).am,
                            start,
                            null_mut(),
                            "*",
                            None,
                            wild as _,
                        );
                        xml_automata_new_epsilon((*pctxt).am, (*pctxt).state, hop);
                    } else if !(*wild).ns_set.is_null() {
                        ns = (*wild).ns_set;
                        while {
                            (*pctxt).state = xml_automata_new_transition2(
                                (*pctxt).am,
                                start,
                                null_mut(),
                                "*",
                                (!(*ns).value.is_null())
                                    .then(|| {
                                        CStr::from_ptr((*ns).value as *const i8).to_string_lossy()
                                    })
                                    .as_deref(),
                                wild as _,
                            );
                            xml_automata_new_epsilon((*pctxt).am, (*pctxt).state, hop);
                            ns = (*ns).next;
                            !ns.is_null()
                        } {}
                    } else if !(*wild).neg_ns_set.is_null() {
                        (*pctxt).state = xml_automata_new_neg_trans(
                            (*pctxt).am,
                            start,
                            hop,
                            "*",
                            (!(*(*wild).neg_ns_set).value.is_null())
                                .then(|| {
                                    CStr::from_ptr((*(*wild).neg_ns_set).value as *const i8)
                                        .to_string_lossy()
                                })
                                .as_deref(),
                            wild as _,
                        );
                    }
                    xml_automata_new_counted_trans((*pctxt).am, hop, start, counter);
                    xml_automata_new_counter_trans((*pctxt).am, hop, end, counter);
                }
                if (*particle).min_occurs == 0 {
                    xml_automata_new_epsilon((*pctxt).am, start, end);
                    ret = 1;
                }
                (*pctxt).state = end;
            }
            XmlSchemaTypeType::XmlSchemaTypeElement => {
                ret = xml_schema_build_content_model_for_element(pctxt, particle);
            }
            XmlSchemaTypeType::XmlSchemaTypeSequence => {
                let mut sub: XmlSchemaTreeItemPtr;

                ret = 1;
                // If max and min occurrences are default (1) then
                // simply iterate over the particles of the <sequence>.
                if (*particle).min_occurs == 1 && (*particle).max_occurs == 1 {
                    sub = (*(*particle).children).children;

                    while !sub.is_null() {
                        tmp2 = xml_schema_build_acontent_model(pctxt, sub as XmlSchemaParticlePtr);
                        if tmp2 != 1 {
                            ret = 0;
                        }
                        sub = (*sub).next;
                    }
                } else {
                    let mut oldstate: XmlAutomataStatePtr = (*pctxt).state;

                    if (*particle).max_occurs >= UNBOUNDED as i32 {
                        if (*particle).min_occurs > 1 {
                            (*pctxt).state =
                                xml_automata_new_epsilon((*pctxt).am, oldstate, null_mut());
                            oldstate = (*pctxt).state;

                            let counter: i32 = xml_automata_new_counter(
                                (*pctxt).am,
                                (*particle).min_occurs - 1,
                                UNBOUNDED as _,
                            );

                            sub = (*(*particle).children).children;
                            while !sub.is_null() {
                                tmp2 = xml_schema_build_acontent_model(
                                    pctxt,
                                    sub as XmlSchemaParticlePtr,
                                );
                                if tmp2 != 1 {
                                    ret = 0;
                                }
                                sub = (*sub).next;
                            }
                            let tmp: XmlAutomataStatePtr = (*pctxt).state;
                            xml_automata_new_counted_trans((*pctxt).am, tmp, oldstate, counter);
                            (*pctxt).state = xml_automata_new_counter_trans(
                                (*pctxt).am,
                                tmp,
                                null_mut(),
                                counter,
                            );
                            if ret == 1 {
                                xml_automata_new_epsilon((*pctxt).am, oldstate, (*pctxt).state);
                            }
                        } else {
                            (*pctxt).state =
                                xml_automata_new_epsilon((*pctxt).am, oldstate, null_mut());
                            oldstate = (*pctxt).state;

                            sub = (*(*particle).children).children;
                            while !sub.is_null() {
                                tmp2 = xml_schema_build_acontent_model(
                                    pctxt,
                                    sub as XmlSchemaParticlePtr,
                                );
                                if tmp2 != 1 {
                                    ret = 0;
                                }
                                sub = (*sub).next;
                            }
                            xml_automata_new_epsilon((*pctxt).am, (*pctxt).state, oldstate);
                            // epsilon needed to block previous trans from
                            // being allowed to enter back from another construct
                            (*pctxt).state =
                                xml_automata_new_epsilon((*pctxt).am, (*pctxt).state, null_mut());
                            if (*particle).min_occurs == 0 {
                                xml_automata_new_epsilon((*pctxt).am, oldstate, (*pctxt).state);
                                ret = 1;
                            }
                        }
                    } else if (*particle).max_occurs > 1 || (*particle).min_occurs > 1 {
                        (*pctxt).state =
                            xml_automata_new_epsilon((*pctxt).am, oldstate, null_mut());
                        oldstate = (*pctxt).state;

                        let counter: i32 = xml_automata_new_counter(
                            (*pctxt).am,
                            (*particle).min_occurs - 1,
                            (*particle).max_occurs - 1,
                        );

                        sub = (*(*particle).children).children;
                        while !sub.is_null() {
                            tmp2 =
                                xml_schema_build_acontent_model(pctxt, sub as XmlSchemaParticlePtr);
                            if tmp2 != 1 {
                                ret = 0;
                            }
                            sub = (*sub).next;
                        }
                        let tmp: XmlAutomataStatePtr = (*pctxt).state;
                        xml_automata_new_counted_trans((*pctxt).am, tmp, oldstate, counter);
                        (*pctxt).state =
                            xml_automata_new_counter_trans((*pctxt).am, tmp, null_mut(), counter);
                        if (*particle).min_occurs == 0 || ret == 1 {
                            xml_automata_new_epsilon((*pctxt).am, oldstate, (*pctxt).state);
                            ret = 1;
                        }
                    } else {
                        sub = (*(*particle).children).children;
                        while !sub.is_null() {
                            tmp2 =
                                xml_schema_build_acontent_model(pctxt, sub as XmlSchemaParticlePtr);
                            if tmp2 != 1 {
                                ret = 0;
                            }
                            sub = (*sub).next;
                        }

                        // epsilon needed to block previous trans from
                        // being allowed to enter back from another construct
                        (*pctxt).state =
                            xml_automata_new_epsilon((*pctxt).am, (*pctxt).state, null_mut());

                        if (*particle).min_occurs == 0 {
                            xml_automata_new_epsilon((*pctxt).am, oldstate, (*pctxt).state);
                            ret = 1;
                        }
                    }
                }
            }
            XmlSchemaTypeType::XmlSchemaTypeChoice => {
                let mut sub: XmlSchemaTreeItemPtr;

                ret = 0;
                let start: XmlAutomataStatePtr = (*pctxt).state;
                let end: XmlAutomataStatePtr = xml_automata_new_state((*pctxt).am);

                // iterate over the subtypes and remerge the end with an
                // epsilon transition
                if (*particle).max_occurs == 1 {
                    sub = (*(*particle).children).children;
                    while !sub.is_null() {
                        (*pctxt).state = start;
                        tmp2 = xml_schema_build_acontent_model(pctxt, sub as XmlSchemaParticlePtr);
                        if tmp2 == 1 {
                            ret = 1
                        }
                        xml_automata_new_epsilon((*pctxt).am, (*pctxt).state, end);
                        sub = (*sub).next;
                    }
                } else {
                    let max_occurs: i32 = if (*particle).max_occurs == UNBOUNDED as i32 {
                        UNBOUNDED as i32
                    } else {
                        (*particle).max_occurs - 1
                    };
                    let min_occurs: i32 = if (*particle).min_occurs < 1 {
                        0
                    } else {
                        (*particle).min_occurs - 1
                    };

                    // use a counter to keep track of the number of transitions
                    // which went through the choice.
                    let counter: i32 =
                        xml_automata_new_counter((*pctxt).am, min_occurs, max_occurs);
                    let hop: XmlAutomataStatePtr = xml_automata_new_state((*pctxt).am);
                    let base: XmlAutomataStatePtr = xml_automata_new_state((*pctxt).am);

                    sub = (*(*particle).children).children;
                    while !sub.is_null() {
                        (*pctxt).state = base;
                        tmp2 = xml_schema_build_acontent_model(pctxt, sub as XmlSchemaParticlePtr);
                        if tmp2 == 1 {
                            ret = 1
                        }
                        xml_automata_new_epsilon((*pctxt).am, (*pctxt).state, hop);
                        sub = (*sub).next;
                    }
                    xml_automata_new_epsilon((*pctxt).am, start, base);
                    xml_automata_new_counted_trans((*pctxt).am, hop, base, counter);
                    xml_automata_new_counter_trans((*pctxt).am, hop, end, counter);
                    if ret == 1 {
                        xml_automata_new_epsilon((*pctxt).am, base, end);
                    }
                }
                if (*particle).min_occurs == 0 {
                    xml_automata_new_epsilon((*pctxt).am, start, end);
                    ret = 1;
                }
                (*pctxt).state = end;
            }
            XmlSchemaTypeType::XmlSchemaTypeAll => {
                let start: XmlAutomataStatePtr;
                let tmp: XmlAutomataStatePtr;
                let mut sub: XmlSchemaParticlePtr;
                let mut elem_decl: XmlSchemaElementPtr;

                ret = 1;

                sub = (*(*particle).children).children as XmlSchemaParticlePtr;
                if sub.is_null() {
                    // break;
                } else {
                    ret = 0;

                    start = (*pctxt).state;
                    tmp = xml_automata_new_state((*pctxt).am);
                    xml_automata_new_epsilon((*pctxt).am, (*pctxt).state, tmp);
                    (*pctxt).state = tmp;
                    while !sub.is_null() {
                        (*pctxt).state = tmp;

                        elem_decl = (*sub).children as XmlSchemaElementPtr;
                        if elem_decl.is_null() {
                            PERROR_INT!(
                                pctxt,
                                "xmlSchemaBuildAContentModel",
                                "<element> particle has no term"
                            );
                            return ret;
                        };
                        // NOTE: The {max occurs} of all the particles in the
                        // {particles} of the group must be 0 or 1; this is
                        // already ensured during the parse of the content of
                        // <all>.
                        if (*elem_decl).flags & XML_SCHEMAS_ELEM_SUBST_GROUP_HEAD != 0 {
                            // This is an abstract group, we need to share
                            // the same counter for all the element transitions
                            // derived from the group
                            let counter: i32 = xml_automata_new_counter(
                                (*pctxt).am,
                                (*sub).min_occurs,
                                (*sub).max_occurs,
                            );
                            xml_schema_build_content_model_for_subst_group(
                                pctxt,
                                sub,
                                counter,
                                (*pctxt).state,
                            );
                        } else if (*sub).min_occurs == 1 && (*sub).max_occurs == 1 {
                            xml_automata_new_once_trans2(
                                (*pctxt).am,
                                (*pctxt).state,
                                (*pctxt).state,
                                CStr::from_ptr((*elem_decl).name as *const i8)
                                    .to_string_lossy()
                                    .as_ref(),
                                (!(*elem_decl).target_namespace.is_null())
                                    .then(|| {
                                        CStr::from_ptr((*elem_decl).target_namespace as *const i8)
                                            .to_string_lossy()
                                    })
                                    .as_deref(),
                                1,
                                1,
                                elem_decl as _,
                            );
                        } else if (*sub).min_occurs == 0 && (*sub).max_occurs == 1 {
                            xml_automata_new_count_trans2(
                                (*pctxt).am,
                                (*pctxt).state,
                                (*pctxt).state,
                                CStr::from_ptr((*elem_decl).name as *const i8)
                                    .to_string_lossy()
                                    .as_ref(),
                                (!(*elem_decl).target_namespace.is_null())
                                    .then(|| {
                                        CStr::from_ptr((*elem_decl).target_namespace as *const i8)
                                            .to_string_lossy()
                                    })
                                    .as_deref(),
                                0,
                                1,
                                elem_decl as _,
                            );
                        }
                        sub = (*sub).next as XmlSchemaParticlePtr;
                    }
                    (*pctxt).state =
                        xml_automata_new_all_trans((*pctxt).am, (*pctxt).state, null_mut(), 0);
                    if (*particle).min_occurs == 0 {
                        xml_automata_new_epsilon((*pctxt).am, start, (*pctxt).state);
                        ret = 1;
                    }
                }
            }
            XmlSchemaTypeType::XmlSchemaTypeGroup => {
                // If we hit a model group definition, then this means that
                // it was empty, thus was not substituted for the containing
                // model group. Just do nothing in this case.
                // TODO: But the group should be substituted and not occur at
                // all in the content model at this point. Fix this.
                ret = 1;
            }
            _ => {
                let typename = xml_schema_get_component_type_str(((*particle).children) as _);
                xml_schema_internal_err2(
                    pctxt as XmlSchemaAbstractCtxtPtr,
                    "xmlSchemaBuildAContentModel",
                    format!("found unexpected term of type '{typename}' in content model").as_str(),
                    Some(typename),
                    None,
                );
                return ret;
            }
        }
        ret
    }
}

/// Builds the content model of the complex type.
#[doc(alias = "xmlSchemaBuildContentModel")]
unsafe fn xml_schema_build_content_model(typ: XmlSchemaTypePtr, ctxt: XmlSchemaParserCtxtPtr) {
    unsafe {
        if (*typ).typ != XmlSchemaTypeType::XmlSchemaTypeComplex
            || !(*typ).cont_model.is_null()
            || !matches!(
                (*typ).content_type,
                XmlSchemaContentType::XmlSchemaContentElements
                    | XmlSchemaContentType::XmlSchemaContentMixed
            )
        {
            return;
        }

        (*ctxt).am = null_mut();
        (*ctxt).am = xml_new_automata();
        if (*ctxt).am.is_null() {
            generic_error!(
                "Cannot create automata for complex type {}\n",
                CStr::from_ptr((*typ).name as *const i8).to_string_lossy()
            );
            return;
        }
        (*ctxt).state = xml_automata_get_init_state((*ctxt).am);
        // Build the automaton.
        xml_schema_build_acontent_model(ctxt, WXS_TYPE_PARTICLE!(typ));
        xml_automata_set_final_state((*ctxt).am, (*ctxt).state);
        (*typ).cont_model = xml_automata_compile((*ctxt).am);
        if (*typ).cont_model.is_null() {
            xml_schema_pcustom_err(
                ctxt,
                XmlParserErrors::XmlSchemapInternal,
                typ as XmlSchemaBasicItemPtr,
                (*typ).node.map(|node| node.into()),
                "Failed to compile the content model",
                None,
            );
        } else if xml_regexp_is_determinist((*typ).cont_model) != 1 {
            // XML_SCHEMAS_ERR_NOTDETERMINIST,
            xml_schema_pcustom_err(
                ctxt,
                XmlParserErrors::XmlSchemapNotDeterministic,
                typ as XmlSchemaBasicItemPtr,
                (*typ).node.map(|node| node.into()),
                "The content model is not determinist",
                None,
            );
        } else {
            // no-op
        }
        (*ctxt).state = null_mut();
        xml_free_automata((*ctxt).am);
        (*ctxt).am = null_mut();
    }
}

macro_rules! FIXHFAILURE {
    ($pctxt:expr, $label:tt) => {
        if (*$pctxt).err == XmlParserErrors::XmlSchemapInternal as i32 {
            break $label;
        }
    };
}

pub(crate) unsafe fn xml_schema_fixup_components(
    pctxt: XmlSchemaParserCtxtPtr,
    root_bucket: XmlSchemaBucketPtr,
) -> i32 {
    unsafe {
        let con: XmlSchemaConstructionCtxtPtr = (*pctxt).constructor;
        let mut ret: i32 = 0;
        let oldbucket: XmlSchemaBucketPtr = (*con).bucket;
        let mut elem_decl: XmlSchemaElementPtr;

        if (*con).pending.is_null() || (*(*con).pending).items.is_empty() {
            return 0;
        }

        // Since xmlSchemaFixupComplexType() will create new particles
        // (local components), and those particle components need a bucket
        // on the constructor, we'll assure here that the constructor has
        // a bucket.
        // TODO: Think about storing locals _only_ on the main bucket.
        if (*con).bucket.is_null() {
            (*con).bucket = root_bucket;
        }

        // TODO:
        // SPEC (src-redefine):
        // (6.2) "If it has no such self-reference, then all of the
        // following must be true:"
        //
        // (6.2.2) The {model group} of the model group definition which
        // corresponds to it per XML Representation of Model Group
        // Definition Schema Components ($3.7.2) must be a `valid
        // restriction` of the {model group} of that model group definition
        // in I, as defined in Particle Valid (Restriction) ($3.9.6)."
        xml_schema_check_srcredefine_first(pctxt);

        // Add global components to the schemata's hash tables.
        xml_schema_add_components(pctxt, root_bucket);

        (*pctxt).ctxt_type = null_mut();
        let mut nb_items = (*(*con).pending).items.len();

        'exit: {
            'exit_failure: {
                'exit_error: {
                    // Now that we have parsed *all* the schema document(s) and converted
                    // them to schema components, we can resolve references, apply component
                    // constraints, create the FSA from the content model, etc.

                    // Resolve references of..
                    //
                    // 1. element declarations:
                    //   - the type definition
                    //   - the substitution group affiliation
                    // 2. simple/complex types:
                    //   - the base type definition
                    //   - the memberTypes of union types
                    //   - the itemType of list types
                    // 3. attributes declarations and attribute uses:
                    //   - the type definition
                    //   - if an attribute use, then the attribute declaration
                    // 4. attribute group references:
                    //   - the attribute group definition
                    // 5. particles:
                    //   - the term of the particle (e.g. a model group)
                    // 6. IDC key-references:
                    //   - the referenced IDC 'key' or 'unique' definition
                    // 7. Attribute prohibitions which had a "ref" attribute.
                    for item in (*(*con).pending)
                        .items
                        .iter()
                        .take(nb_items)
                        .map(|&item| item as XmlSchemaTreeItemPtr)
                    {
                        match (*item).typ {
                            XmlSchemaTypeType::XmlSchemaTypeElement => {
                                xml_schema_resolve_element_references(
                                    item as XmlSchemaElementPtr,
                                    pctxt,
                                );
                                FIXHFAILURE!(pctxt, 'exit_failure);
                            }
                            XmlSchemaTypeType::XmlSchemaTypeComplex
                            | XmlSchemaTypeType::XmlSchemaTypeSimple => {
                                xml_schema_resolve_type_references(item as XmlSchemaTypePtr, pctxt);
                                FIXHFAILURE!(pctxt, 'exit_failure);
                            }
                            XmlSchemaTypeType::XmlSchemaTypeAttribute => {
                                xml_schema_resolve_attr_type_references(
                                    item as XmlSchemaAttributePtr,
                                    pctxt,
                                );
                                FIXHFAILURE!(pctxt, 'exit_failure);
                            }
                            XmlSchemaTypeType::XmlSchemaTypeAttributeUse => {
                                xml_schema_resolve_attr_use_references(
                                    item as XmlSchemaAttributeUsePtr,
                                    pctxt,
                                );
                                FIXHFAILURE!(pctxt, 'exit_failure);
                            }
                            XmlSchemaTypeType::XmlSchemaExtraQNameRef => {
                                if (*(item as XmlSchemaQNameRefPtr)).item_type
                                    == XmlSchemaTypeType::XmlSchemaTypeAttributeGroup
                                {
                                    xml_schema_resolve_attr_group_references(
                                        item as XmlSchemaQNameRefPtr,
                                        pctxt,
                                    );
                                }
                                FIXHFAILURE!(pctxt, 'exit_failure);
                            }
                            XmlSchemaTypeType::XmlSchemaTypeSequence
                            | XmlSchemaTypeType::XmlSchemaTypeChoice
                            | XmlSchemaTypeType::XmlSchemaTypeAll => {
                                xml_schema_resolve_model_group_particle_references(
                                    pctxt,
                                    item as XmlSchemaModelGroupPtr,
                                );
                                FIXHFAILURE!(pctxt, 'exit_failure);
                            }
                            XmlSchemaTypeType::XmlSchemaTypeIDCKey
                            | XmlSchemaTypeType::XmlSchemaTypeIDCUnique
                            | XmlSchemaTypeType::XmlSchemaTypeIDCKeyref => {
                                xml_schema_resolve_idckey_references(
                                    item as XmlSchemaIDCPtr,
                                    pctxt,
                                );
                                FIXHFAILURE!(pctxt, 'exit_failure);
                            }
                            XmlSchemaTypeType::XmlSchemaExtraAttrUseProhib => {
                                // Handle attribute prohibition which had a "ref" attribute.
                                xml_schema_resolve_attr_use_prohib_references(
                                    item as XmlSchemaAttributeUseProhibPtr,
                                    pctxt,
                                );
                                FIXHFAILURE!(pctxt, 'exit_failure);
                            }
                            _ => {}
                        }
                    }

                    if (*pctxt).nberrors != 0 {
                        break 'exit_error;
                    }

                    // Now that all references are resolved we
                    // can check for circularity of...
                    // 1. the base axis of type definitions
                    // 2. nested model group definitions
                    // 3. nested attribute group definitions
                    // TODO: check for circular substitution groups.
                    for item in (*(*con).pending)
                        .items
                        .iter()
                        .take(nb_items)
                        .map(|&item| item as XmlSchemaTreeItemPtr)
                    {
                        // Let's better stop on the first error here.
                        match (*item).typ {
                            XmlSchemaTypeType::XmlSchemaTypeComplex
                            | XmlSchemaTypeType::XmlSchemaTypeSimple => {
                                xml_schema_check_type_def_circular(item as XmlSchemaTypePtr, pctxt);
                                FIXHFAILURE!(pctxt, 'exit_failure);
                                if (*pctxt).nberrors != 0 {
                                    break 'exit_error;
                                }
                            }
                            XmlSchemaTypeType::XmlSchemaTypeGroup => {
                                xml_schema_check_group_def_circular(
                                    item as XmlSchemaModelGroupDefPtr,
                                    pctxt,
                                );
                                FIXHFAILURE!(pctxt, 'exit_failure);
                                if (*pctxt).nberrors != 0 {
                                    break 'exit_error;
                                }
                            }
                            XmlSchemaTypeType::XmlSchemaTypeAttributeGroup => {
                                xml_schema_check_attr_group_circular(
                                    item as XmlSchemaAttributeGroupPtr,
                                    pctxt,
                                );
                                FIXHFAILURE!(pctxt, 'exit_failure);
                                if (*pctxt).nberrors != 0 {
                                    break 'exit_error;
                                }
                            }
                            _ => {}
                        }
                    }
                    if (*pctxt).nberrors != 0 {
                        break 'exit_error;
                    }
                    // Model group definition references:
                    // Such a reference is reflected by a particle at the component
                    // level. Until now the 'term' of such particles pointed
                    // to the model group definition; this was done, in order to
                    // ease circularity checks. Now we need to set the 'term' of
                    // such particles to the model group of the model group definition.
                    for item in (*(*con).pending)
                        .items
                        .iter()
                        .take(nb_items)
                        .map(|&item| item as XmlSchemaTreeItemPtr)
                    {
                        match (*item).typ {
                            XmlSchemaTypeType::XmlSchemaTypeSequence
                            | XmlSchemaTypeType::XmlSchemaTypeChoice => {
                                xml_schema_model_group_to_model_group_def_fixup(
                                    pctxt,
                                    item as XmlSchemaModelGroupPtr,
                                );
                            }
                            _ => {}
                        }
                    }
                    if (*pctxt).nberrors != 0 {
                        break 'exit_error;
                    }
                    // Expand attribute group references of attribute group definitions.
                    for item in (*(*con).pending)
                        .items
                        .iter()
                        .take(nb_items)
                        .map(|&item| item as XmlSchemaTreeItemPtr)
                    {
                        if (*item).typ == XmlSchemaTypeType::XmlSchemaTypeAttributeGroup
                            && !WXS_ATTR_GROUP_EXPANDED!(item)
                            && WXS_ATTR_GROUP_HAS_REFS!(item)
                        {
                            xml_schema_attribute_group_expand_refs(
                                pctxt,
                                item as XmlSchemaAttributeGroupPtr,
                            );
                            FIXHFAILURE!(pctxt, 'exit_failure);
                        }
                    }
                    if (*pctxt).nberrors != 0 {
                        break 'exit_error;
                    }
                    // First compute the variety of simple types. This is needed as
                    // a separate step, since otherwise we won't be able to detect
                    // circular union types in all cases.
                    for item in (*(*con).pending)
                        .items
                        .iter()
                        .take(nb_items)
                        .map(|&item| item as XmlSchemaTreeItemPtr)
                    {
                        if (*item).typ == XmlSchemaTypeType::XmlSchemaTypeSimple
                            && WXS_IS_TYPE_NOT_FIXED_1!(item as XmlSchemaTypePtr)
                        {
                            xml_schema_fixup_simple_type_stage_one(pctxt, item as XmlSchemaTypePtr);
                            FIXHFAILURE!(pctxt, 'exit_failure);
                        }
                    }
                    if (*pctxt).nberrors != 0 {
                        break 'exit_error;
                    }
                    // Detect circular union types. Note that this needs the variety to
                    // be already computed.
                    for item in (*(*con).pending)
                        .items
                        .iter()
                        .take(nb_items)
                        .map(|&item| item as XmlSchemaTreeItemPtr)
                    {
                        if (*item).typ == XmlSchemaTypeType::XmlSchemaTypeSimple
                            && !(*(item as XmlSchemaTypePtr)).member_types.is_null()
                        {
                            xml_schema_check_union_type_def_circular(
                                pctxt,
                                item as XmlSchemaTypePtr,
                            );
                            FIXHFAILURE!(pctxt, 'exit_failure);
                        }
                    }
                    if (*pctxt).nberrors != 0 {
                        break 'exit_error;
                    }

                    // Do the complete type fixup for simple types.
                    for item in (*(*con).pending)
                        .items
                        .iter()
                        .take(nb_items)
                        .map(|&item| item as XmlSchemaTreeItemPtr)
                    {
                        if (*item).typ == XmlSchemaTypeType::XmlSchemaTypeSimple
                            && WXS_IS_TYPE_NOT_FIXED!(item as XmlSchemaTypePtr)
                        {
                            xml_schema_fixup_simple_type_stage_two(pctxt, item as XmlSchemaTypePtr);
                            FIXHFAILURE!(pctxt, 'exit_failure);
                        }
                    }
                    if (*pctxt).nberrors != 0 {
                        break 'exit_error;
                    }
                    // At this point we need build and check all simple types.

                    // Apply constraints for attribute declarations.
                    for item in (*(*con).pending)
                        .items
                        .iter()
                        .take(nb_items)
                        .map(|&item| item as XmlSchemaTreeItemPtr)
                    {
                        if (*item).typ == XmlSchemaTypeType::XmlSchemaTypeAttribute {
                            xml_schema_check_attr_props_correct(
                                pctxt,
                                item as XmlSchemaAttributePtr,
                            );
                            FIXHFAILURE!(pctxt, 'exit_failure);
                        }
                    }
                    if (*pctxt).nberrors != 0 {
                        break 'exit_error;
                    }
                    // Apply constraints for attribute uses.
                    for item in (*(*con).pending)
                        .items
                        .iter()
                        .take(nb_items)
                        .map(|&item| item as XmlSchemaTreeItemPtr)
                    {
                        if (*item).typ == XmlSchemaTypeType::XmlSchemaTypeAttributeUse
                            && !(*(item as XmlSchemaAttributeUsePtr)).def_value.is_null()
                        {
                            xml_schema_check_attr_use_props_correct(
                                pctxt,
                                item as XmlSchemaAttributeUsePtr,
                            );
                            FIXHFAILURE!(pctxt, 'exit_failure);
                        }
                    }
                    if (*pctxt).nberrors != 0 {
                        break 'exit_error;
                    }

                    // Apply constraints for attribute group definitions.
                    for item in (*(*con).pending)
                        .items
                        .iter()
                        .take(nb_items)
                        .map(|&item| item as XmlSchemaTreeItemPtr)
                    {
                        if (*item).typ == XmlSchemaTypeType::XmlSchemaTypeAttributeGroup
                            && !(*(item as XmlSchemaAttributeGroupPtr)).attr_uses.is_null()
                            && (*((*(item as XmlSchemaAttributeGroupPtr)).attr_uses
                                as XmlSchemaItemListPtr<*mut c_void>))
                                .items
                                .len()
                                > 1
                        {
                            xml_schema_check_agprops_correct(
                                pctxt,
                                item as XmlSchemaAttributeGroupPtr,
                            );
                            FIXHFAILURE!(pctxt, 'exit_failure);
                        }
                    }
                    if (*pctxt).nberrors != 0 {
                        break 'exit_error;
                    }

                    // Apply constraints for redefinitions.
                    if !(*WXS_CONSTRUCTOR!(pctxt)).redefs.is_null() {
                        xml_schema_check_src_redefine_second(pctxt);
                    }
                    if (*pctxt).nberrors != 0 {
                        break 'exit_error;
                    }

                    // Complex types are built and checked.
                    for item in (*(*con).pending)
                        .items
                        .iter()
                        .take(nb_items)
                        .map(|&item| item as XmlSchemaTreeItemPtr)
                    {
                        if (*item).typ == XmlSchemaTypeType::XmlSchemaTypeComplex
                            && WXS_IS_TYPE_NOT_FIXED!(item as XmlSchemaTypePtr)
                        {
                            xml_schema_fixup_complex_type(pctxt, item as XmlSchemaTypePtr);
                            FIXHFAILURE!(pctxt, 'exit_failure);
                        }
                    }
                    if (*pctxt).nberrors != 0 {
                        break 'exit_error;
                    }

                    // The list could have changed, since xmlSchemaFixupComplexType()
                    // will create particles and model groups in some cases.
                    nb_items = (*(*con).pending).items.len();

                    // Apply some constraints for element declarations.
                    for item in (*(*con).pending)
                        .items
                        .iter()
                        .take(nb_items)
                        .map(|&item| item as XmlSchemaTreeItemPtr)
                    {
                        if (*item).typ == XmlSchemaTypeType::XmlSchemaTypeElement {
                            elem_decl = item as XmlSchemaElementPtr;

                            if (*elem_decl).flags & XML_SCHEMAS_ELEM_INTERNAL_CHECKED == 0 {
                                xml_schema_check_element_decl_component(
                                    elem_decl as XmlSchemaElementPtr,
                                    pctxt,
                                );
                                FIXHFAILURE!(pctxt, 'exit_failure);
                            }

                            // #ifdef WXS_ELEM_DECL_CONS_ENABLED
                            // /*
                            // * Schema Component Constraint: Element Declarations Consistent
                            // * Apply this constraint to local types of element declarations.
                            // */
                            // if (!WXS_ELEM_TYPEDEF!(elemDecl).is_null() && (wxs_is_complex(WXS_ELEM_TYPEDEF!(elemDecl))) && (WXS_TYPE_IS_LOCAL(WXS_ELEM_TYPEDEF!(elemDecl)))) {
                            //     xmlSchemaCheckElementDeclConsistent(pctxt, (XmlSchemaBasicItemPtr) elemDecl, WXS_TYPE_PARTICLE!(WXS_ELEM_TYPEDEF!(elemDecl)), null_mut(), null_mut(), 0);
                            // }
                            // #endif
                        }
                    }
                    if (*pctxt).nberrors != 0 {
                        break 'exit_error;
                    }

                    // Finally we can build the automaton from the content model of complex types.
                    for item in (*(*con).pending)
                        .items
                        .iter()
                        .take(nb_items)
                        .map(|&item| item as XmlSchemaTreeItemPtr)
                    {
                        if (*item).typ == XmlSchemaTypeType::XmlSchemaTypeComplex {
                            xml_schema_build_content_model(item as XmlSchemaTypePtr, pctxt);
                            // FIXHFAILURE!(pctxt, con, oldbucket);
                        }
                    }
                    if (*pctxt).nberrors != 0 {
                        break 'exit_error;
                    }
                    // URGENT TODO: cos-element-consistent
                    break 'exit;
                }

                // exit_error:
                ret = (*pctxt).err;
                break 'exit;
            }

            // exit_failure:
            ret = -1;
        }

        // exit:
        // Reset the constructor. This is needed for XSI acquisition, since
        // those items will be processed over and over again for every XSI
        // if not cleared here.
        (*con).bucket = oldbucket;
        (*(*con).pending).items.clear();
        if !(*con).subst_groups.is_null() {
            xml_hash_free((*con).subst_groups, Some(xml_schema_subst_group_free_entry));
            (*con).subst_groups = null_mut();
        }
        if !(*con).redefs.is_null() {
            xml_schema_redef_list_free((*con).redefs);
            (*con).redefs = null_mut();
        }
        ret
    }
}

/// Set the error and warning callback information
#[doc(alias = "xmlSchemaSetValidErrors")]
pub unsafe fn xml_schema_set_valid_errors(
    ctxt: XmlSchemaValidCtxtPtr,
    err: Option<GenericError>,
    warn: Option<GenericError>,
    ctx: Option<GenericErrorContext>,
) {
    unsafe {
        if ctxt.is_null() {
            return;
        }
        (*ctxt).error = err;
        (*ctxt).warning = warn;
        (*ctxt).err_ctxt = ctx.clone();
        if !(*ctxt).pctxt.is_null() {
            (*(*ctxt).pctxt).set_errors(err, warn, ctx);
        }
    }
}

/// Set the structured error callback
#[doc(alias = "xmlSchemaSetValidStructuredErrors")]
pub unsafe fn xml_schema_set_valid_structured_errors(
    ctxt: XmlSchemaValidCtxtPtr,
    serror: Option<StructuredError>,
    ctx: Option<GenericErrorContext>,
) {
    unsafe {
        if ctxt.is_null() {
            return;
        }
        (*ctxt).serror = serror;
        (*ctxt).error = None;
        (*ctxt).warning = None;
        (*ctxt).err_ctxt = ctx.clone();
        if !(*ctxt).pctxt.is_null() {
            (*(*ctxt).pctxt).set_structured_errors(serror, ctx);
        }
    }
}

/// Get the error and warning callback information
///
/// Returns -1 in case of error and 0 otherwise
#[doc(alias = "xmlSchemaGetValidErrors")]
pub unsafe fn xml_schema_get_valid_errors(
    ctxt: XmlSchemaValidCtxtPtr,
    err: *mut Option<GenericError>,
    warn: *mut Option<GenericError>,
    ctx: *mut Option<GenericErrorContext>,
) -> i32 {
    unsafe {
        if ctxt.is_null() {
            return -1;
        }
        if !err.is_null() {
            *err = (*ctxt).error;
        }
        if !warn.is_null() {
            *warn = (*ctxt).warning;
        }
        if !ctx.is_null() {
            *ctx = (*ctxt).err_ctxt.clone();
        }
        0
    }
}

/// Sets the options to be used during the validation.
///
/// Returns 0 in case of success, -1 in case of an API error.
#[doc(alias = "xmlSchemaSetValidOptions")]
pub unsafe fn xml_schema_set_valid_options(ctxt: XmlSchemaValidCtxtPtr, options: i32) -> i32 {
    unsafe {
        if ctxt.is_null() {
            return -1;
        }
        // WARNING: Change the start value if adding to the xmlSchemaValidOption.
        // TODO: Is there an other, more easy to maintain, way?
        for i in 1..i32::BITS as usize {
            if options & (1 << i) != 0 {
                return -1;
            }
        }
        (*ctxt).options = options;
        0
    }
}

/// Workaround to provide file error reporting information when this is
/// not provided by current APIs
#[doc(alias = "xmlSchemaValidateSetFilename")]
pub unsafe fn xml_schema_validate_set_filename(
    vctxt: XmlSchemaValidCtxtPtr,
    filename: *const c_char,
) {
    unsafe {
        if vctxt.is_null() {
            return;
        }
        if !(*vctxt).filename.is_null() {
            xml_free((*vctxt).filename as _);
        }
        if !filename.is_null() {
            (*vctxt).filename = xml_strdup(filename as _) as _;
        } else {
            (*vctxt).filename = null_mut();
        }
    }
}

/// Get the validation context options.
///
/// Returns the option combination or -1 on error.
#[doc(alias = "xmlSchemaValidCtxtGetOptions")]
pub unsafe fn xml_schema_valid_ctxt_get_options(ctxt: XmlSchemaValidCtxtPtr) -> i32 {
    unsafe { if ctxt.is_null() { -1 } else { (*ctxt).options } }
}

/// Frees an IDC key together with its compiled value.
#[doc(alias = "xmlSchemaIDCFreeKey")]
pub(crate) unsafe fn xml_schema_idc_free_key(key: XmlSchemaPSVIIDCKeyPtr) {
    unsafe {
        if !(*key).val.is_null() {
            xml_schema_free_value((*key).val);
        }
        xml_free(key as _);
    }
}

pub(crate) unsafe fn xml_schema_free_idc_state_obj_list(mut sto: XmlSchemaIDCStateObjPtr) {
    unsafe {
        let mut next: XmlSchemaIDCStateObjPtr;
        while !sto.is_null() {
            next = (*sto).next;
            if !(*sto).history.is_null() {
                xml_free((*sto).history as _);
            }
            if !(*sto).xpath_ctxt.is_null() {
                xml_free_stream_ctxt((*sto).xpath_ctxt as XmlStreamCtxtPtr);
            }
            xml_free(sto as _);
            sto = next;
        }
    }
}

// Cleanup currently used attribute infos.
pub(crate) unsafe fn xml_schema_clear_attr_infos(vctxt: XmlSchemaValidCtxtPtr) {
    unsafe {
        let mut attr: XmlSchemaAttrInfoPtr;

        if (*vctxt).nb_attr_infos == 0 {
            return;
        }
        for i in 0..(*vctxt).nb_attr_infos {
            attr = *(*vctxt).attr_infos.add(i as usize);
            if (*attr).flags & XML_SCHEMA_NODE_INFO_FLAG_OWNED_NAMES != 0 {
                if !(*attr).local_name.is_null() {
                    xml_free((*attr).local_name as _);
                }
                if !(*attr).ns_name.is_null() {
                    xml_free((*attr).ns_name as _);
                }
            }
            if (*attr).flags & XML_SCHEMA_NODE_INFO_FLAG_OWNED_VALUES != 0
                && !(*attr).value.is_null()
            {
                xml_free((*attr).value as _);
            }
            if !(*attr).val.is_null() {
                xml_schema_free_value((*attr).val);
                (*attr).val = null_mut();
            }
            memset(attr as _, 0, size_of::<XmlSchemaAttrInfo>());
        }
        (*vctxt).nb_attr_infos = 0;
    }
}

extern "C" fn xml_free_idc_hash_entry(payload: *mut c_void, _name: *const XmlChar) {
    let mut e: XmlIDCHashEntryPtr = payload as _;
    let mut n: XmlIDCHashEntryPtr;
    while !e.is_null() {
        unsafe {
            n = (*e).next;
            xml_free(e as _);
            e = n;
        }
    }
}

/// Caches a list of IDC matchers for reuse.
#[doc(alias = "xmlSchemaIDCReleaseMatcherList")]
unsafe fn xml_schema_idc_release_matcher_list(
    vctxt: XmlSchemaValidCtxtPtr,
    mut matcher: XmlSchemaIDCMatcherPtr,
) {
    unsafe {
        let mut next: XmlSchemaIDCMatcherPtr;

        while !matcher.is_null() {
            next = (*matcher).next;
            if !(*matcher).key_seqs.is_null() {
                // Don't free the array, but only the content.
                for i in 0..(*matcher).size_key_seqs {
                    if !(*(*matcher).key_seqs.add(i as usize)).is_null() {
                        xml_free(*(*matcher).key_seqs.add(i as usize) as _);
                        *(*matcher).key_seqs.add(i as usize) = null_mut();
                    }
                }
            }
            if !(*matcher).targets.is_null() {
                if (*matcher).idc_type == XmlSchemaTypeType::XmlSchemaTypeIDCKeyref as i32 {
                    // Node-table items for keyrefs are not stored globally
                    // to the validation context, since they are not bubbled.
                    // We need to free them here.
                    for idc_node in (*(*matcher).targets)
                        .items
                        .iter()
                        .map(|&idc_node| idc_node as XmlSchemaPSVIIDCNodePtr)
                    {
                        xml_free((*idc_node).keys as _);
                        xml_free(idc_node as _);
                    }
                }
                xml_schema_item_list_free((*matcher).targets);
                (*matcher).targets = null_mut();
            }
            if !(*matcher).htab.is_null() {
                xml_hash_free((*matcher).htab, Some(xml_free_idc_hash_entry));
                (*matcher).htab = null_mut();
            }
            (*matcher).next = null_mut();
            // Cache the matcher.
            if !(*vctxt).idc_matcher_cache.is_null() {
                (*matcher).next_cached = (*vctxt).idc_matcher_cache;
            }
            (*vctxt).idc_matcher_cache = matcher;

            matcher = next;
        }
    }
}

/// Frees an IDC binding. Note that the node table-items are not freed.
#[doc(alias = "xmlSchemaIDCFreeBinding")]
unsafe fn xml_schema_idc_free_binding(bind: XmlSchemaPSVIIDCBindingPtr) {
    unsafe {
        if !(*bind).dupls.is_null() {
            xml_schema_item_list_free((*bind).dupls);
        }
        drop_in_place(bind);
        xml_free(bind as _);
    }
}

/// Frees an IDC table, i.e. all the IDC bindings in the list.
#[doc(alias = "xmlSchemaIDCFreeIDCTable")]
unsafe fn xml_schema_idcfree_idc_table(mut bind: XmlSchemaPSVIIDCBindingPtr) {
    unsafe {
        let mut prev: XmlSchemaPSVIIDCBindingPtr;

        while !bind.is_null() {
            prev = bind;
            bind = (*bind).next;
            xml_schema_idc_free_binding(prev);
        }
    }
}

#[doc(alias = "xmlSchemaClearElemInfo")]
pub(crate) unsafe fn xml_schema_clear_elem_info(
    vctxt: XmlSchemaValidCtxtPtr,
    ielem: XmlSchemaNodeInfoPtr,
) {
    unsafe {
        (*ielem).has_keyrefs = 0;
        (*ielem).applied_xpath = 0;
        if (*ielem).flags & XML_SCHEMA_NODE_INFO_FLAG_OWNED_NAMES != 0 {
            FREE_AND_NULL!((*ielem).local_name);
            FREE_AND_NULL!((*ielem).ns_name);
        } else {
            (*ielem).local_name = null_mut();
            (*ielem).ns_name = null_mut();
        }
        if (*ielem).flags & XML_SCHEMA_NODE_INFO_FLAG_OWNED_VALUES != 0 {
            FREE_AND_NULL!((*ielem).value);
        } else {
            (*ielem).value = null_mut();
        }
        if !(*ielem).val.is_null() {
            // PSVI TODO: Be careful not to free it when the value is exposed via PSVI.
            xml_schema_free_value((*ielem).val);
            (*ielem).val = null_mut();
        }
        if !(*ielem).idc_matchers.is_null() {
            // REVISIT OPTIMIZE TODO: Use a pool of IDC matchers.
            // Does it work?
            xml_schema_idc_release_matcher_list(vctxt, (*ielem).idc_matchers);
            // #if 0
            //     xmlSchemaIDCFreeMatcherList((*ielem).idcMatchers);
            // #endif
            (*ielem).idc_matchers = null_mut();
        }
        if !(*ielem).idc_table.is_null() {
            // OPTIMIZE TODO: Use a pool of IDC tables??.
            xml_schema_idcfree_idc_table((*ielem).idc_table);
            (*ielem).idc_table = null_mut();
        }
        if !(*ielem).regex_ctxt.is_null() {
            xml_reg_free_exec_ctxt((*ielem).regex_ctxt);
            (*ielem).regex_ctxt = null_mut();
        }
        if !(*ielem).ns_bindings.is_null() {
            xml_free((*ielem).ns_bindings as _);
            (*ielem).ns_bindings = null_mut();
            (*ielem).nb_ns_bindings = 0;
            (*ielem).size_ns_bindings = 0;
        }
    }
}

/// Creates an augmented IDC definition for the imported schema.
#[doc(alias = "xmlSchemaAugmentImportedIDC")]
unsafe fn xml_schema_augment_imported_idc(
    imported: XmlSchemaImportPtr,
    vctxt: XmlSchemaValidCtxtPtr,
) {
    unsafe {
        for &idc_def in (*(*imported).schema).idc_def.values() {
            let aidc: XmlSchemaIDCAugPtr =
                xml_malloc(size_of::<XmlSchemaIDCAug>()) as XmlSchemaIDCAugPtr;
            if aidc.is_null() {
                xml_schema_verr_memory(
                    vctxt,
                    "xmlSchemaAugmentIDC: allocating an augmented IDC definition",
                    None,
                );
                continue;
            }
            (*aidc).keyref_depth = -1;
            (*aidc).def = idc_def;
            (*aidc).next = null_mut();
            if (*vctxt).aidcs.is_null() {
                (*vctxt).aidcs = aidc;
            } else {
                (*aidc).next = (*vctxt).aidcs;
                (*vctxt).aidcs = aidc;
            }
            // Save if we have keyrefs at all.
            if (*vctxt).has_keyrefs == 0
                && (*idc_def).typ == XmlSchemaTypeType::XmlSchemaTypeIDCKeyref
            {
                (*vctxt).has_keyrefs = 1;
            }
        }
    }
}

unsafe fn xml_schema_create_pctxt_on_vctxt(vctxt: XmlSchemaValidCtxtPtr) -> i32 {
    unsafe {
        if (*vctxt).pctxt.is_null() {
            if !(*vctxt).schema.is_null() {
                (*vctxt).pctxt =
                    xml_schema_new_parser_ctxt_use_dict(Some("*"), (*(*vctxt).schema).dict);
            } else {
                (*vctxt).pctxt = xml_schema_new_parser_ctxt("*");
            }
            if (*vctxt).pctxt.is_null() {
                VERROR_INT!(
                    vctxt,
                    "xmlSchemaCreatePCtxtOnVCtxt",
                    "failed to create a temp. parser context"
                );
                return -1;
            }
            // TODO: Pass user data.
            (*(*vctxt).pctxt).set_errors(
                (*vctxt).error,
                (*vctxt).warning,
                (*vctxt).err_ctxt.clone(),
            );
            (*(*vctxt).pctxt).set_structured_errors((*vctxt).serror, (*vctxt).err_ctxt.clone());
        }
        0
    }
}

unsafe fn xml_schema_pre_run(vctxt: XmlSchemaValidCtxtPtr) -> i32 {
    unsafe {
        // Some initialization.
        (*vctxt).err = 0;
        (*vctxt).nberrors = 0;
        (*vctxt).depth = -1;
        (*vctxt).skip_depth = -1;
        (*vctxt).has_keyrefs = 0;
        // #ifdef ENABLE_IDC_NODE_TABLES_TEST
        //     (*vctxt).createIDCNodeTables = 1;
        // #else
        (*vctxt).create_idcnode_tables = 0;
        // #endif
        // Create a schema + parser if necessary.
        if (*vctxt).schema.is_null() {
            (*vctxt).xsi_assemble = 1;
            // If not schema was given then we will create a schema
            // dynamically using XSI schema locations.
            //
            // Create the schema parser context.
            if (*vctxt).pctxt.is_null() && xml_schema_create_pctxt_on_vctxt(vctxt) == -1 {
                return -1;
            }
            let pctxt: XmlSchemaParserCtxtPtr = (*vctxt).pctxt;
            (*pctxt).xsi_assemble = 1;
            // Create the schema.
            (*vctxt).schema = (*pctxt).new_schema();
            if (*vctxt).schema.is_null() {
                return -1;
            }
            // Create the schema construction context.
            (*pctxt).constructor = xml_schema_construction_ctxt_create((*pctxt).dict);
            if (*pctxt).constructor.is_null() {
                return -1;
            }
            (*(*pctxt).constructor).main_schema = (*vctxt).schema;
            // Take ownership of the constructor to be able to free it.
            (*pctxt).owns_constructor = 1;
        }
        // Augment the IDC definitions for the main schema and all imported ones
        // NOTE: main schema if the first in the imported list
        for &imported in (*(*vctxt).schema).schemas_imports.values() {
            if !imported.is_null() {
                xml_schema_augment_imported_idc(imported, vctxt);
            }
        }

        0
    }
}

/// Creates/reuses and initializes the element info item for
/// the current tree depth.
///
/// Returns the element info item or NULL on API or internal errors.
#[doc(alias = "xmlSchemaGetFreshElemInfo")]
unsafe fn xml_schema_get_fresh_elem_info(vctxt: XmlSchemaValidCtxtPtr) -> XmlSchemaNodeInfoPtr {
    unsafe {
        let mut info: XmlSchemaNodeInfoPtr = null_mut();

        if (*vctxt).depth > (*vctxt).size_elem_infos {
            VERROR_INT!(
                vctxt,
                "xmlSchemaGetFreshElemInfo",
                "inconsistent depth encountered"
            );
            return null_mut();
        }
        if (*vctxt).elem_infos.is_null() {
            (*vctxt).elem_infos =
                xml_malloc(10 * size_of::<XmlSchemaNodeInfoPtr>()) as *mut XmlSchemaNodeInfoPtr;
            if (*vctxt).elem_infos.is_null() {
                xml_schema_verr_memory(vctxt, "allocating the element info array", None);
                return null_mut();
            }
            memset(
                (*vctxt).elem_infos as _,
                0,
                10 * size_of::<XmlSchemaNodeInfoPtr>(),
            );
            (*vctxt).size_elem_infos = 10;
        } else if (*vctxt).size_elem_infos <= (*vctxt).depth {
            let i: i32 = (*vctxt).size_elem_infos;

            (*vctxt).size_elem_infos *= 2;
            (*vctxt).elem_infos = xml_realloc(
                (*vctxt).elem_infos as _,
                (*vctxt).size_elem_infos as usize * size_of::<XmlSchemaNodeInfoPtr>(),
            ) as *mut XmlSchemaNodeInfoPtr;
            if (*vctxt).elem_infos.is_null() {
                xml_schema_verr_memory(vctxt, "re-allocating the element info array", None);
                return null_mut();
            }
            // We need the new memory to be NULLed.
            // TODO: Use memset instead?
            for i in i..(*vctxt).size_elem_infos {
                *(*vctxt).elem_infos.add(i as usize) = null_mut();
            }
        } else {
            info = *(*vctxt).elem_infos.add((*vctxt).depth as usize);
        }

        if info.is_null() {
            info = xml_malloc(size_of::<XmlSchemaNodeInfo>()) as XmlSchemaNodeInfoPtr;
            if info.is_null() {
                xml_schema_verr_memory(vctxt, "allocating an element info", None);
                return null_mut();
            }
            *(*vctxt).elem_infos.add((*vctxt).depth as usize) = info;
        } else if !(*info).local_name.is_null() {
            VERROR_INT!(
                vctxt,
                "xmlSchemaGetFreshElemInfo",
                "elem info has not been cleared"
            );
            return null_mut();
        }
        memset(info as _, 0, size_of::<XmlSchemaNodeInfo>());
        (*info).node_type = XmlElementType::XmlElementNode as i32;
        (*info).depth = (*vctxt).depth;

        info
    }
}

unsafe fn xml_schema_validator_push_elem(vctxt: XmlSchemaValidCtxtPtr) -> i32 {
    unsafe {
        (*vctxt).inode = xml_schema_get_fresh_elem_info(vctxt);
        if (*vctxt).inode.is_null() {
            VERROR_INT!(
                vctxt,
                "xmlSchemaValidatorPushElem",
                "calling xmlSchemaGetFreshElemInfo()"
            );
            return -1;
        }
        (*vctxt).nb_attr_infos = 0;
        0
    }
}

unsafe fn xml_schema_get_fresh_attr_info(vctxt: XmlSchemaValidCtxtPtr) -> XmlSchemaAttrInfoPtr {
    unsafe {
        let iattr: XmlSchemaAttrInfoPtr;
        // Grow/create list of attribute infos.
        if (*vctxt).attr_infos.is_null() {
            (*vctxt).attr_infos =
                xml_malloc(size_of::<XmlSchemaAttrInfoPtr>()) as *mut XmlSchemaAttrInfoPtr;
            (*vctxt).size_attr_infos = 1;
            if (*vctxt).attr_infos.is_null() {
                xml_schema_verr_memory(vctxt, "allocating attribute info list", None);
                return null_mut();
            }
        } else if (*vctxt).size_attr_infos <= (*vctxt).nb_attr_infos {
            (*vctxt).size_attr_infos += 1;
            (*vctxt).attr_infos = xml_realloc(
                (*vctxt).attr_infos as _,
                (*vctxt).size_attr_infos as usize * size_of::<XmlSchemaAttrInfoPtr>(),
            ) as *mut XmlSchemaAttrInfoPtr;
            if (*vctxt).attr_infos.is_null() {
                xml_schema_verr_memory(vctxt, "re-allocating attribute info list", None);
                return null_mut();
            }
        } else {
            iattr = *(*vctxt).attr_infos.add((*vctxt).nb_attr_infos as usize);
            (*vctxt).nb_attr_infos += 1;
            if !(*iattr).local_name.is_null() {
                VERROR_INT!(vctxt, "xmlSchemaGetFreshAttrInfo", "attr info not cleared");
                return null_mut();
            }
            (*iattr).node_type = XmlElementType::XmlAttributeNode as i32;
            return iattr;
        }
        // Create an attribute info.
        iattr = xml_malloc(size_of::<XmlSchemaAttrInfo>()) as XmlSchemaAttrInfoPtr;
        if iattr.is_null() {
            xml_schema_verr_memory(vctxt, "creating new attribute info", None);
            return null_mut();
        }
        memset(iattr as _, 0, size_of::<XmlSchemaAttrInfo>());
        (*iattr).node_type = XmlElementType::XmlAttributeNode as i32;
        *(*vctxt).attr_infos.add((*vctxt).nb_attr_infos as usize) = iattr;
        (*vctxt).nb_attr_infos += 1;

        iattr
    }
}

#[allow(clippy::too_many_arguments)]
unsafe fn xml_schema_validator_push_attribute(
    vctxt: XmlSchemaValidCtxtPtr,
    attr_node: Option<XmlAttrPtr>,
    node_line: i32,
    local_name: *const XmlChar,
    ns_name: *const XmlChar,
    owned_names: i32,
    value: *mut XmlChar,
    owned_value: i32,
) -> i32 {
    unsafe {
        let attr: XmlSchemaAttrInfoPtr = xml_schema_get_fresh_attr_info(vctxt);
        if attr.is_null() {
            VERROR_INT!(
                vctxt,
                "xmlSchemaPushAttribute",
                "calling xmlSchemaGetFreshAttrInfo()"
            );
            return -1;
        }
        (*attr).node = attr_node;
        (*attr).node_line = node_line;
        (*attr).state = XML_SCHEMAS_ATTR_UNKNOWN;
        (*attr).local_name = local_name;
        (*attr).ns_name = ns_name;
        if owned_names != 0 {
            (*attr).flags |= XML_SCHEMA_NODE_INFO_FLAG_OWNED_NAMES;
        }
        // Evaluate if it's an XSI attribute.
        if !ns_name.is_null() {
            if xml_str_equal(local_name, c"nil".as_ptr() as _) {
                if xml_str_equal((*attr).ns_name, XML_SCHEMA_INSTANCE_NS.as_ptr() as _) {
                    (*attr).meta_type = XML_SCHEMA_ATTR_INFO_META_XSI_NIL;
                }
            } else if xml_str_equal(local_name, c"type".as_ptr() as _) {
                if xml_str_equal((*attr).ns_name, XML_SCHEMA_INSTANCE_NS.as_ptr() as _) {
                    (*attr).meta_type = XML_SCHEMA_ATTR_INFO_META_XSI_TYPE;
                }
            } else if xml_str_equal(local_name, c"schemaLocation".as_ptr() as _) {
                if xml_str_equal((*attr).ns_name, XML_SCHEMA_INSTANCE_NS.as_ptr() as _) {
                    (*attr).meta_type = XML_SCHEMA_ATTR_INFO_META_XSI_SCHEMA_LOC;
                }
            } else if xml_str_equal(local_name, c"noNamespaceSchemaLocation".as_ptr() as _) {
                if xml_str_equal((*attr).ns_name, XML_SCHEMA_INSTANCE_NS.as_ptr() as _) {
                    (*attr).meta_type = XML_SCHEMA_ATTR_INFO_META_XSI_NO_NS_SCHEMA_LOC;
                }
            } else if xml_str_equal((*attr).ns_name, XML_NAMESPACE_NS.as_ptr() as _) {
                (*attr).meta_type = XML_SCHEMA_ATTR_INFO_META_XMLNS;
            }
        }
        (*attr).value = value;
        if owned_value != 0 {
            (*attr).flags |= XML_SCHEMA_NODE_INFO_FLAG_OWNED_VALUES;
        }
        if (*attr).meta_type != 0 {
            (*attr).state = XML_SCHEMAS_ATTR_META;
        }
        0
    }
}

unsafe fn xml_schema_get_meta_attr_info(
    vctxt: XmlSchemaValidCtxtPtr,
    meta_type: i32,
) -> XmlSchemaAttrInfoPtr {
    unsafe {
        if (*vctxt).nb_attr_infos == 0 {
            return null_mut();
        }
        {
            let mut iattr: XmlSchemaAttrInfoPtr;

            for i in 0..(*vctxt).nb_attr_infos {
                iattr = *(*vctxt).attr_infos.add(i as usize) as _;
                if (*iattr).meta_type == meta_type {
                    return iattr;
                }
            }
        }
        null_mut()
    }
}

/// Expands an existing schema by an additional schema.
///
/// Returns 0 if the new schema is correct, a positive error code
/// number otherwise and -1 in case of an internal or API error.
#[doc(alias = "xmlSchemaAssembleByLocation")]
unsafe fn xml_schema_assemble_by_location(
    vctxt: XmlSchemaValidCtxtPtr,
    schema: XmlSchemaPtr,
    node: Option<XmlGenericNodePtr>,
    ns_name: *const XmlChar,
    mut location: *const XmlChar,
) -> i32 {
    unsafe {
        let mut ret: i32;

        let mut bucket: XmlSchemaBucketPtr = null_mut();

        if vctxt.is_null() || schema.is_null() {
            return -1;
        }

        if (*vctxt).pctxt.is_null() {
            VERROR_INT!(
                vctxt,
                "xmlSchemaAssembleByLocation",
                "no parser context available"
            );
            return -1;
        }
        let pctxt: XmlSchemaParserCtxtPtr = (*vctxt).pctxt;
        if (*pctxt).constructor.is_null() {
            PERROR_INT!(pctxt, "xmlSchemaAssembleByLocation", "no constructor");
            return -1;
        }
        // Acquire the schema document.
        location = xml_schema_build_absolute_uri((*pctxt).dict, location, node);
        // Note that we pass XML_SCHEMA_SCHEMA_IMPORT here;
        // the process will automatically change this to
        // XML_SCHEMA_SCHEMA_MAIN if it is the first schema document.
        ret = xml_schema_add_schema_doc(
            pctxt,
            XML_SCHEMA_SCHEMA_IMPORT,
            location,
            None,
            null_mut(),
            0,
            node,
            null_mut(),
            ns_name,
            &raw mut bucket,
        );
        if ret != 0 {
            return ret;
        }
        if bucket.is_null() {
            let location = CStr::from_ptr(location as *const i8).to_string_lossy();
            // Generate a warning that the document could not be located.
            xml_schema_custom_warning(
                vctxt as XmlSchemaAbstractCtxtPtr,
                XmlParserErrors::XmlSchemavMisc,
                node,
                null_mut(),
                format!("The document at location '{location}' could not be acquired").as_str(),
                Some(&location),
                None,
                None,
            );
            return ret;
        }
        // The first located schema will be handled as if all other
        // schemas imported by XSI were imported by this first schema.
        if !bucket.is_null() && (*WXS_CONSTRUCTOR!(pctxt)).bucket.is_null() {
            (*WXS_CONSTRUCTOR!(pctxt)).bucket = bucket;
        }
        // TODO: Is this handled like an import? I.e. is it not an error if the schema cannot be located?
        if bucket.is_null() || !can_parse_schema(bucket) {
            return 0;
        }
        // We will reuse the parser context for every schema imported
        // directly via XSI. So reset the context.
        (*pctxt).nberrors = 0;
        (*pctxt).err = 0;
        (*pctxt).doc = (*bucket).doc;

        ret = (*pctxt).parse_new_doc_with_context(schema, bucket);
        if ret == -1 {
            (*pctxt).doc = None;
            return -1;
        }
        // Paranoid error channelling.
        if ret == 0 && (*pctxt).nberrors != 0 {
            ret = (*pctxt).err;
        }
        if (*pctxt).nberrors == 0 {
            // Only bother to fixup pending components, if there was
            // no error yet.
            // For every XSI acquired schema (and its sub-schemata) we will
            // fixup the components.
            xml_schema_fixup_components(pctxt, bucket);
            ret = (*pctxt).err;
            // Not nice, but we need somehow to channel the schema parser
            // error to the validation context.
            if ret != 0 && (*vctxt).err == 0 {
                (*vctxt).err = ret;
            }
            (*vctxt).nberrors += (*pctxt).nberrors;
        } else {
            // Add to validation error sum.
            (*vctxt).nberrors += (*pctxt).nberrors;
        }
        (*pctxt).doc = None;
        ret
        // exit_failure:
        //     (*pctxt).doc = null_mut();
        //     return -1;
    }
}

/// Expands an existing schema by an additional schema using
/// the xsi:schemaLocation or xsi:noNamespaceSchemaLocation attribute
/// of an instance. If xsi:noNamespaceSchemaLocation is used, @noNamespace
/// must be set to 1.
///
/// Returns 0 if the new schema is correct, a positive error code
/// number otherwise and -1 in case of an internal or API error.
#[doc(alias = "xmlSchemaAssembleByXSI")]
unsafe fn xml_schema_assemble_by_xsi(vctxt: XmlSchemaValidCtxtPtr) -> i32 {
    unsafe {
        let mut cur: *const XmlChar;
        let mut end: *const XmlChar;
        let mut nsname: *const XmlChar = null();
        let mut location: *const XmlChar;
        let mut ret: i32 = 0;
        let mut iattr: XmlSchemaAttrInfoPtr;

        // Parse the value; we will assume an even number of values
        // to be given (this is how Xerces and XSV work).
        //
        // URGENT TODO: !! This needs to work for both
        // @noNamespaceSchemaLocation AND @schemaLocation on the same
        // element !!
        iattr = xml_schema_get_meta_attr_info(vctxt, XML_SCHEMA_ATTR_INFO_META_XSI_SCHEMA_LOC);
        if iattr.is_null() {
            iattr = xml_schema_get_meta_attr_info(
                vctxt,
                XML_SCHEMA_ATTR_INFO_META_XSI_NO_NS_SCHEMA_LOC,
            );
        }
        if iattr.is_null() {
            return 0;
        }
        cur = (*iattr).value;
        loop {
            // TODO: Move the string parsing mechanism away from here.
            if (*iattr).meta_type == XML_SCHEMA_ATTR_INFO_META_XSI_SCHEMA_LOC {
                // Get the namespace name.
                while xml_is_blank_char(*cur as u32) {
                    cur = cur.add(1);
                }
                end = cur;
                while *end != 0 && !xml_is_blank_char(*end as u32) {
                    end = end.add(1);
                }
                if end == cur {
                    break;
                }
                // TODO: Don't use the schema's dict.
                nsname = xml_dict_lookup((*(*vctxt).schema).dict, cur, end.offset_from(cur) as _);
                cur = end;
            }
            // Get the URI.
            while xml_is_blank_char(*cur as u32) {
                cur = cur.add(1);
            }
            end = cur;
            while *end != 0 && !xml_is_blank_char(*end as u32) {
                end = end.add(1);
            }
            if end == cur {
                if (*iattr).meta_type == XML_SCHEMA_ATTR_INFO_META_XSI_SCHEMA_LOC {
                    // If using @schemaLocation then tuples are expected.
                    // I.e. the namespace name *and* the document's URI.
                    xml_schema_custom_warning(
                        vctxt as XmlSchemaAbstractCtxtPtr,
                        XmlParserErrors::XmlSchemavMisc,
                        (*iattr).node.map(|attr| attr.into()),
                        null_mut(),
                        "The value must consist of tuples: the target namespace name and the document's URI",
                        None,
                        None,
                        None,
                    );
                }
                break;
            }
            // TODO: Don't use the schema's dict.
            location = xml_dict_lookup((*(*vctxt).schema).dict, cur, end.offset_from(cur) as _);
            cur = end;
            ret = xml_schema_assemble_by_location(
                vctxt,
                (*vctxt).schema,
                (*iattr).node.map(|attr| attr.into()),
                nsname,
                location,
            );
            if ret == -1 {
                VERROR_INT!(vctxt, "xmlSchemaAssembleByXSI", "assembling schemata");
                return -1;
            }

            if *cur == 0 {
                break;
            }
        }
        ret
    }
}

unsafe fn xml_schema_vexpand_qname(
    vctxt: XmlSchemaValidCtxtPtr,
    value: *const XmlChar,
    ns_name: *mut *const XmlChar,
    local_name: *mut *const XmlChar,
) -> i32 {
    unsafe {
        if ns_name.is_null() || local_name.is_null() {
            return -1;
        }
        *ns_name = null_mut();
        *local_name = null_mut();

        if validate_qname::<true>(
            CStr::from_ptr(value as *const i8)
                .to_string_lossy()
                .as_ref(),
        )
        .is_err()
        {
            let value = CStr::from_ptr(value as *const i8).to_string_lossy();
            xml_schema_simple_type_err(
                vctxt as XmlSchemaAbstractCtxtPtr,
                XmlParserErrors::XmlSchemavCvcDatatypeValid1_2_1,
                None,
                &value,
                xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasQName),
                1,
            );
            return 1;
        }
        let mut prefix: *mut XmlChar = null_mut();

        // NOTE: xmlSplitQName2 will return a duplicated string.
        let local: *mut XmlChar = xml_split_qname2(value, &raw mut prefix);
        if local.is_null() {
            *local_name = xml_dict_lookup((*vctxt).dict, value, -1);
        } else {
            *local_name = xml_dict_lookup((*vctxt).dict, local, -1);
            xml_free(local as _);
        }

        *ns_name = (*vctxt).lookup_namespace(
            (!prefix.is_null())
                .then(|| CStr::from_ptr(prefix as *const i8).to_string_lossy())
                .as_deref(),
        );

        if !prefix.is_null() {
            xml_free(prefix as _);
            // A namespace must be found if the prefix is NOT NULL.
            if (*ns_name).is_null() {
                let value = CStr::from_ptr(value as *const i8).to_string_lossy();
                xml_schema_custom_err(
                vctxt as XmlSchemaAbstractCtxtPtr,
                XmlParserErrors::XmlSchemavCvcDatatypeValid1_2_1,
                None,
                xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasQName)
                    as XmlSchemaBasicItemPtr,
                format!("The QName value '{value}' has no corresponding namespace declaration in scope").as_str(),
                Some(&value),
                None,
            );
                return 2;
            }
        }
        0
    }
}

unsafe fn xml_schema_process_xsi_type(
    vctxt: XmlSchemaValidCtxtPtr,
    iattr: XmlSchemaAttrInfoPtr,
    local_type: *mut XmlSchemaTypePtr,
    elem_decl: XmlSchemaElementPtr,
) -> i32 {
    unsafe {
        let mut ret: i32;
        // cvc-elt (3.3.4) : (4)
        // AND
        // Schema-Validity Assessment (Element) (cvc-assess-elt)
        //   (1.2.1.2.1) - (1.2.1.2.4)
        // Handle 'xsi:type'.
        if local_type.is_null() {
            return -1;
        }
        *local_type = null_mut();
        if iattr.is_null() {
            return 0;
        } else {
            let mut ns_name: *const XmlChar = null();
            let mut local: *const XmlChar = null();
            // TODO: We should report a *warning* that the type was overridden
            // by the instance.
            ACTIVATE_ATTRIBUTE!(vctxt, iattr);
            // (cvc-elt) (3.3.4) : (4.1)
            // (cvc-assess-elt) (1.2.1.2.2)
            ret = xml_schema_vexpand_qname(vctxt, (*iattr).value, &raw mut ns_name, &raw mut local);
            if ret != 0 {
                if ret < 0 {
                    VERROR_INT!(
                        vctxt,
                        "xmlSchemaValidateElementByDeclaration",
                        "calling xmlSchemaQNameExpand() to validate the attribute 'xsi:type'"
                    );
                    // goto internal_error;
                    ACTIVATE_ELEM!(vctxt);
                    return -1;
                }
                // goto exit;
                ACTIVATE_ELEM!(vctxt);
                return ret;
            }
            // (cvc-elt) (3.3.4) : (4.2)
            // (cvc-assess-elt) (1.2.1.2.3)
            *local_type = (*(*vctxt).schema).get_type(
                CStr::from_ptr(local as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                (!ns_name.is_null())
                    .then(|| CStr::from_ptr(ns_name as *const i8).to_string_lossy())
                    .as_deref(),
            );
            if (*local_type).is_null() {
                let qname = xml_schema_format_qname(
                    Some(
                        CStr::from_ptr(ns_name as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                    ),
                    Some(
                        CStr::from_ptr(local as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                    ),
                );

                xml_schema_custom_err(
                    vctxt as XmlSchemaAbstractCtxtPtr,
                    XmlParserErrors::XmlSchemavCvcElt4_2,
                    None,
                    xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasQName) as XmlSchemaBasicItemPtr,
                    format!("The QName value '{qname}' of the xsi:type attribute does not resolve to a type definition").as_str(),
                    Some(&qname),
                    None
                );
                ret = (*vctxt).err;
                // goto exit;
                ACTIVATE_ELEM!(vctxt);
                return ret;
            }
            if !elem_decl.is_null() {
                let mut set: i32 = 0;

                // SPEC cvc-elt (3.3.4) : (4.3) (Type Derivation OK)
                // "The `local type definition` must be validly
                // derived from the {type definition} given the union of
                // the {disallowed substitutions} and the {type definition}'s
                // {prohibited substitutions}, as defined in
                // Type Derivation OK (Complex) ($3.4.6)
                // (if it is a complex type definition),
                // or given {disallowed substitutions} as defined in Type
                // Derivation OK (Simple) ($3.14.6) (if it is a simple type
                // definition)."
                //
                // {disallowed substitutions}: the "block" on the element decl.
                // {prohibited substitutions}: the "block" on the type def.

                // OPTIMIZE TODO: We could map types already evaluated
                // to be validly derived from other types to avoid checking
                // this over and over for the same types.
                if (*elem_decl).flags & XML_SCHEMAS_ELEM_BLOCK_EXTENSION != 0
                    || (*(*elem_decl).subtypes).flags & XML_SCHEMAS_TYPE_BLOCK_EXTENSION != 0
                {
                    set |= SUBSET_EXTENSION;
                }

                if (*elem_decl).flags & XML_SCHEMAS_ELEM_BLOCK_RESTRICTION != 0
                    || (*(*elem_decl).subtypes).flags & XML_SCHEMAS_TYPE_BLOCK_RESTRICTION != 0
                {
                    set |= SUBSET_RESTRICTION;
                }

                // REMOVED and CHANGED since this produced a parser context
                // which adds to the string dict of the schema. So this would
                // change the schema and we don't want this. We don't need
                // the parser context anymore.
                //
                // if ((*vctxt).pctxt.is_null() && (xmlSchemaCreatePCtxtOnVCtxt(vctxt) == -1)) {
                //     return -1;
                // }

                if xml_schema_check_cos_derived_ok(
                    vctxt as XmlSchemaAbstractCtxtPtr,
                    *local_type,
                    (*elem_decl).subtypes,
                    set,
                ) != 0
                {
                    let qname = xml_schema_format_qname(
                        Some(
                            CStr::from_ptr((*(*local_type)).target_namespace as *const i8)
                                .to_string_lossy()
                                .as_ref(),
                        ),
                        Some(
                            CStr::from_ptr((*(*local_type)).name as *const i8)
                                .to_string_lossy()
                                .as_ref(),
                        ),
                    );

                    xml_schema_custom_err(
                        vctxt as XmlSchemaAbstractCtxtPtr,
                        XmlParserErrors::XmlSchemavCvcElt4_3,
                        None,
                        null_mut(),
                        format!("The type definition '{qname}', specified by xsi:type, is blocked or not validly derived from the type definition of the element declaration").as_str(),
                        Some(&qname),
                        None
                    );
                    ret = (*vctxt).err;
                    *local_type = null_mut();
                }
            }
        }
        // exit:
        ACTIVATE_ELEM!(vctxt);
        ret
        // internal_error:
        //     ACTIVATE_ELEM!(vctxt);
        //     return -1;
    }
}

unsafe fn xml_schema_vcontent_model_callback(
    _exec: XmlRegExecCtxtPtr,
    _name: &str,
    transdata: *mut c_void,
    inputdata: *mut c_void,
) {
    unsafe {
        let item: XmlSchemaElementPtr = transdata as XmlSchemaElementPtr;
        let inode: XmlSchemaNodeInfoPtr = inputdata as XmlSchemaNodeInfoPtr;
        (*inode).decl = item;
    }
}

// 3.4.4 Complex Type Definition Validation Rules
// Validation Rule: Element Locally Valid (Complex Type) (cvc-complex-type)
unsafe fn xml_schema_validate_child_elem(vctxt: XmlSchemaValidCtxtPtr) -> i32 {
    unsafe {
        let mut ret: i32 = 0;

        if (*vctxt).depth <= 0 {
            VERROR_INT!(
                vctxt,
                "xmlSchemaValidateChildElem",
                "not intended for the validation root"
            );
            return -1;
        }
        let pielem: XmlSchemaNodeInfoPtr = *(*vctxt).elem_infos.add((*vctxt).depth as usize - 1);
        if (*pielem).flags & XML_SCHEMA_ELEM_INFO_EMPTY != 0 {
            (*pielem).flags ^= XML_SCHEMA_ELEM_INFO_EMPTY;
        }
        'unexpected_elem: {
            // Handle 'nilled' elements.
            if INODE_NILLED!(pielem) {
                // SPEC (cvc-elt) (3.3.4) : (3.2.1)
                ACTIVATE_PARENT_ELEM!(vctxt);
                ret = XmlParserErrors::XmlSchemavCvcElt3_2_1 as i32;
                VERROR!(
                    vctxt,
                    ret.try_into().unwrap(),
                    null_mut(),
                    "Neither character nor element content is allowed, because the element was 'nilled'"
                );
                ACTIVATE_ELEM!(vctxt);
                break 'unexpected_elem;
            }

            let ptype: XmlSchemaTypePtr = (*pielem).type_def;

            if (*ptype).built_in_type == XmlSchemaValType::XmlSchemasAnytype as i32 {
                // Workaround for "anyType": we have currently no content model
                // assigned for "anyType", so handle it explicitly.
                // "anyType" has an unbounded, lax "any" wildcard.
                (*(*vctxt).inode).decl = (*(*vctxt).schema).get_elem(
                    CStr::from_ptr((*(*vctxt).inode).local_name as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    (!(*(*vctxt).inode).ns_name.is_null())
                        .then(|| {
                            CStr::from_ptr((*(*vctxt).inode).ns_name as *const i8).to_string_lossy()
                        })
                        .as_deref(),
                );

                if (*(*vctxt).inode).decl.is_null() {
                    // Process "xsi:type".
                    // SPEC (cvc-assess-elt) (1.2.1.2.1) - (1.2.1.2.3)
                    let iattr: XmlSchemaAttrInfoPtr =
                        xml_schema_get_meta_attr_info(vctxt, XML_SCHEMA_ATTR_INFO_META_XSI_TYPE);
                    if !iattr.is_null() {
                        ret = xml_schema_process_xsi_type(
                            vctxt,
                            iattr,
                            &raw mut (*(*vctxt).inode).type_def,
                            null_mut(),
                        );
                        if ret != 0 {
                            if ret == -1 {
                                VERROR_INT!(
                                    vctxt,
                                    "xmlSchemaValidateChildElem",
                                    "calling xmlSchemaProcessXSIType() to process the attribute 'xsi:nil'"
                                );
                                return -1;
                            }
                            return ret;
                        }
                    } else {
                        // Fallback to "anyType".
                        //
                        // SPEC (cvc-assess-elt)
                        // "If the item cannot be `strictly assessed`, [...]
                        // an element information item's schema validity may be laxly
                        // assessed if its `context-determined declaration` is not
                        // skip by `validating` with respect to the `ur-type
                        // definition` as per Element Locally Valid (Type) ($3.3.4)."
                        (*(*vctxt).inode).type_def =
                            xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasAnytype);
                    }
                }
                return 0;
            }

            match (*ptype).content_type {
                XmlSchemaContentType::XmlSchemaContentEmpty => {
                    // SPEC (2.1) "If the {content type} is empty, then the
                    // element information item has no character or element
                    // information item [children]."
                    ACTIVATE_PARENT_ELEM!(vctxt);
                    ret = XmlParserErrors::XmlSchemavCvcComplexType2_1 as i32;
                    VERROR!(
                        vctxt,
                        ret.try_into().unwrap(),
                        null_mut(),
                        "Element content is not allowed, because the content type is empty"
                    );
                    ACTIVATE_ELEM!(vctxt);
                    break 'unexpected_elem;
                }
                XmlSchemaContentType::XmlSchemaContentMixed
                | XmlSchemaContentType::XmlSchemaContentElements => {
                    let mut regex_ctxt: XmlRegExecCtxtPtr;
                    let mut values = [const { Cow::Borrowed("") }; 10];
                    let mut terminal: i32 = 0;

                    // VAL TODO: Optimized "anyType" validation.

                    if (*ptype).cont_model.is_null() {
                        VERROR_INT!(
                            vctxt,
                            "xmlSchemaValidateChildElem",
                            "type has elem content but no content model"
                        );
                        return -1;
                    }
                    // Safety belt for evaluation if the cont. model was already
                    // examined to be invalid.
                    if (*pielem).flags & XML_SCHEMA_ELEM_INFO_ERR_BAD_CONTENT != 0 {
                        VERROR_INT!(
                            vctxt,
                            "xmlSchemaValidateChildElem",
                            "validating elem, but elem content is already invalid"
                        );
                        return -1;
                    }

                    regex_ctxt = (*pielem).regex_ctxt;
                    if regex_ctxt.is_null() {
                        // Create the regex context.
                        regex_ctxt = xml_reg_new_exec_ctxt(
                            (*ptype).cont_model,
                            Some(xml_schema_vcontent_model_callback),
                            vctxt as _,
                        );
                        if regex_ctxt.is_null() {
                            VERROR_INT!(
                                vctxt,
                                "xmlSchemaValidateChildElem",
                                "failed to create a regex context"
                            );
                            return -1;
                        }
                        (*pielem).regex_ctxt = regex_ctxt;
                    }

                    // SPEC (2.4) "If the {content type} is element-only or mixed,
                    // then the sequence of the element information item's
                    // element information item [children], if any, taken in
                    // order, is `valid` with respect to the {content type}'s
                    // particle, as defined in Element Sequence Locally Valid
                    // (Particle) ($3.9.4)."
                    ret = xml_reg_exec_push_string2(
                        regex_ctxt,
                        (*(*vctxt).inode).local_name,
                        (*(*vctxt).inode).ns_name,
                        (*vctxt).inode as _,
                    );
                    if (*vctxt).err == XmlParserErrors::XmlSchemavInternal as i32 {
                        VERROR_INT!(
                            vctxt,
                            "xmlSchemaValidateChildElem",
                            "calling xmlRegExecPushString2()"
                        );
                        return -1;
                    }
                    if ret < 0 {
                        if let Some((nbval, nbneg, values)) = xml_reg_exec_err_info(
                            regex_ctxt,
                            null_mut(),
                            &mut values,
                            &raw mut terminal,
                        ) {
                            xml_schema_complex_type_err(
                                vctxt as XmlSchemaAbstractCtxtPtr,
                                XmlParserErrors::XmlSchemavElementContent,
                                None,
                                null_mut(),
                                "This element is not expected",
                                nbval,
                                nbneg,
                                values,
                            );
                        }
                        ret = (*vctxt).err;
                        break 'unexpected_elem;
                    } else {
                        ret = 0;
                    }
                }
                XmlSchemaContentType::XmlSchemaContentSimple
                | XmlSchemaContentType::XmlSchemaContentBasic => {
                    ACTIVATE_PARENT_ELEM!(vctxt);
                    if wxs_is_complex(ptype) {
                        // SPEC (cvc-complex-type) (2.2)
                        // "If the {content type} is a simple type definition, then
                        // the element information item has no element information
                        // item [children], ..."
                        ret = XmlParserErrors::XmlSchemavCvcComplexType2_2 as i32;
                        VERROR!(
                            vctxt,
                            ret.try_into().unwrap(),
                            null_mut(),
                            "Element content is not allowed, because the content type is a simple type definition"
                        );
                    } else {
                        // SPEC (cvc-type) (3.1.2) "The element information item must
                        // have no element information item [children]."
                        ret = XmlParserErrors::XmlSchemavCvcType3_1_2 as i32;
                        VERROR!(
                            vctxt,
                            ret.try_into().unwrap(),
                            null_mut(),
                            "Element content is not allowed, because the type definition is simple"
                        );
                    }
                    ACTIVATE_ELEM!(vctxt);
                    ret = (*vctxt).err;
                    break 'unexpected_elem;
                }
                _ => {}
            }
            return ret;
        }
        // unexpected_elem:
        // Pop this element and set the skipDepth to skip
        // all further content of the parent element.
        (*vctxt).skip_depth = (*vctxt).depth;
        (*(*vctxt).inode).flags |= XML_SCHEMA_NODE_INFO_ERR_NOT_EXPECTED;
        (*pielem).flags |= XML_SCHEMA_ELEM_INFO_ERR_BAD_CONTENT;
        ret
    }
}

unsafe fn xml_schema_validate_elem_wildcard(vctxt: XmlSchemaValidCtxtPtr, skip: *mut i32) -> i32 {
    unsafe {
        let wild: XmlSchemaWildcardPtr = (*(*vctxt).inode).decl as XmlSchemaWildcardPtr;
        // The namespace of the element was already identified to be
        // matching the wildcard.
        if skip.is_null() || wild.is_null() || (*wild).typ != XmlSchemaTypeType::XmlSchemaTypeAny {
            VERROR_INT!(vctxt, "xmlSchemaValidateElemWildcard", "bad arguments");
            return -1;
        }
        *skip = 0;
        if (*wild).process_contents == XML_SCHEMAS_ANY_SKIP {
            // URGENT VAL TODO: Either we need to position the stream to the
            // next sibling, or walk the whole subtree.
            *skip = 1;
            return 0;
        }
        {
            let decl: XmlSchemaElementPtr = (*(*vctxt).schema).get_elem(
                CStr::from_ptr((*(*vctxt).inode).local_name as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                (!(*(*vctxt).inode).ns_name.is_null())
                    .then(|| {
                        CStr::from_ptr((*(*vctxt).inode).ns_name as *const i8).to_string_lossy()
                    })
                    .as_deref(),
            );
            if !decl.is_null() {
                (*(*vctxt).inode).decl = decl;
                return 0;
            }
        }
        if (*wild).process_contents == XML_SCHEMAS_ANY_STRICT {
            // VAL TODO: Change to proper error code.
            VERROR!(
                vctxt,
                XmlParserErrors::XmlSchemavCvcElt1,
                null_mut(),
                // (XmlSchemaBasicItemPtr) wild
                "No matching global element declaration available, but demanded by the strict wildcard"
            );
            return (*vctxt).err;
        }
        if (*vctxt).nb_attr_infos != 0 {
            // SPEC Validation Rule: Schema-Validity Assessment (Element)
            // (1.2.1.2.1) - (1.2.1.2.3 )
            //
            // Use the xsi:type attribute for the type definition.
            let iattr: XmlSchemaAttrInfoPtr =
                xml_schema_get_meta_attr_info(vctxt, XML_SCHEMA_ATTR_INFO_META_XSI_TYPE);
            if !iattr.is_null() {
                if xml_schema_process_xsi_type(
                    vctxt,
                    iattr,
                    &raw mut (*(*vctxt).inode).type_def,
                    null_mut(),
                ) == -1
                {
                    VERROR_INT!(
                        vctxt,
                        "xmlSchemaValidateElemWildcard",
                        "calling xmlSchemaProcessXSIType() to process the attribute 'xsi:nil'"
                    );
                    return -1;
                }
                // Don't return an error on purpose.
                return 0;
            }
        }
        // SPEC Validation Rule: Schema-Validity Assessment (Element)
        //
        // Fallback to "anyType".
        (*(*vctxt).inode).type_def =
            xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasAnytype);
        0
    }
}

/// Creates/reuses and activates state objects for the given
/// XPath information; if the XPath expression consists of unions,
/// multiple state objects are created for every unioned expression.
///
/// Returns 0 on success and -1 on internal errors.
#[doc(alias = "xmlSchemaIDCAddStateObject")]
unsafe fn xml_schema_idc_add_state_object(
    vctxt: XmlSchemaValidCtxtPtr,
    matcher: XmlSchemaIDCMatcherPtr,
    sel: XmlSchemaIdcselectPtr,
    typ: i32,
) -> i32 {
    unsafe {
        let sto: XmlSchemaIDCStateObjPtr;

        // Reuse the state objects from the pool.
        if !(*vctxt).xpath_state_pool.is_null() {
            sto = (*vctxt).xpath_state_pool;
            (*vctxt).xpath_state_pool = (*sto).next;
            (*sto).next = null_mut();
        } else {
            // Create a new state object.
            sto = xml_malloc(size_of::<XmlSchemaIDCStateObj>()) as XmlSchemaIDCStateObjPtr;
            if sto.is_null() {
                xml_schema_verr_memory(null_mut(), "allocating an IDC state object", None);
                return -1;
            }
            memset(sto as _, 0, size_of::<XmlSchemaIDCStateObj>());
        }
        // Add to global list.
        if !(*vctxt).xpath_states.is_null() {
            (*sto).next = (*vctxt).xpath_states;
        }
        (*vctxt).xpath_states = sto;

        // Free the old xpath validation context.
        if !(*sto).xpath_ctxt.is_null() {
            xml_free_stream_ctxt((*sto).xpath_ctxt as XmlStreamCtxtPtr);
        }

        // Create a new XPath (pattern) validation context.
        (*sto).xpath_ctxt =
            xml_pattern_get_stream_ctxt((*sel).xpath_comp as XmlPatternPtr) as *mut c_void;
        if (*sto).xpath_ctxt.is_null() {
            VERROR_INT!(
                vctxt,
                "xmlSchemaIDCAddStateObject",
                "failed to create an XPath validation context"
            );
            return -1;
        }
        (*sto).typ = typ;
        (*sto).depth = (*vctxt).depth;
        (*sto).matcher = matcher;
        (*sto).sel = sel;
        (*sto).nb_history = 0;

        0
    }
}

/// Creates helper objects to evaluate IDC selectors/fields successively.
///
/// Returns 0 if OK and -1 on internal errors.
#[doc(alias = "xmlSchemaIDCRegisterMatchers")]
unsafe fn xml_schema_idc_register_matchers(
    vctxt: XmlSchemaValidCtxtPtr,
    elem_decl: XmlSchemaElementPtr,
) -> i32 {
    unsafe {
        let mut matcher: XmlSchemaIDCMatcherPtr;
        let mut last: XmlSchemaIDCMatcherPtr = null_mut();
        let mut idc: XmlSchemaIDCPtr;
        let mut ref_idc: XmlSchemaIDCPtr;
        let mut aidc: XmlSchemaIDCAugPtr;

        idc = (*elem_decl).idcs as XmlSchemaIDCPtr;
        if idc.is_null() {
            return 0;
        }

        if !(*(*vctxt).inode).idc_matchers.is_null() {
            VERROR_INT!(
                vctxt,
                "xmlSchemaIDCRegisterMatchers",
                "The chain of IDC matchers is expected to be empty"
            );
            return -1;
        }
        while {
            if (*idc).typ == XmlSchemaTypeType::XmlSchemaTypeIDCKeyref {
                // Since IDCs bubbles are expensive we need to know the
                // depth at which the bubbles should stop; this will be
                // the depth of the top-most keyref IDC. If no keyref
                // references a key/unique IDC, the keyrefDepth will
                // be -1, indicating that no bubbles are needed.
                ref_idc = (*(*idc).refe).item as XmlSchemaIDCPtr;
                if !ref_idc.is_null() {
                    // Remember that we have keyrefs on this node.
                    (*(*vctxt).inode).has_keyrefs = 1;
                    // Lookup the referenced augmented IDC info.
                    aidc = (*vctxt).aidcs;
                    while !aidc.is_null() {
                        if (*aidc).def == ref_idc {
                            break;
                        }
                        aidc = (*aidc).next;
                    }
                    if aidc.is_null() {
                        VERROR_INT!(
                            vctxt,
                            "xmlSchemaIDCRegisterMatchers",
                            "Could not find an augmented IDC item for an IDC definition"
                        );
                        return -1;
                    }
                    if (*aidc).keyref_depth == -1 || (*vctxt).depth < (*aidc).keyref_depth {
                        (*aidc).keyref_depth = (*vctxt).depth;
                    }
                }
            }
            // Lookup the augmented IDC item for the IDC definition.
            aidc = (*vctxt).aidcs;
            while !aidc.is_null() {
                if (*aidc).def == idc {
                    break;
                }
                aidc = (*aidc).next;
            }
            if aidc.is_null() {
                VERROR_INT!(
                    vctxt,
                    "xmlSchemaIDCRegisterMatchers",
                    "Could not find an augmented IDC item for an IDC definition"
                );
                return -1;
            }
            // Create an IDC matcher for every IDC definition.
            if !(*vctxt).idc_matcher_cache.is_null() {
                // Reuse a cached matcher.
                matcher = (*vctxt).idc_matcher_cache;
                (*vctxt).idc_matcher_cache = (*matcher).next_cached;
                (*matcher).next_cached = null_mut();
            } else {
                matcher = xml_malloc(size_of::<XmlSchemaIDCMatcher>()) as XmlSchemaIDCMatcherPtr;
                if matcher.is_null() {
                    xml_schema_verr_memory(vctxt, "allocating an IDC matcher", None);
                    return -1;
                }
                memset(matcher as _, 0, size_of::<XmlSchemaIDCMatcher>());
            }
            if last.is_null() {
                (*(*vctxt).inode).idc_matchers = matcher;
            } else {
                (*last).next = matcher;
            }
            last = matcher;

            (*matcher).typ = IDC_MATCHER;
            (*matcher).depth = (*vctxt).depth;
            (*matcher).aidc = aidc;
            (*matcher).idc_type = (*(*aidc).def).typ as _;
            // Init the automaton state object.
            if xml_schema_idc_add_state_object(
                vctxt,
                matcher,
                (*idc).selector,
                XPATH_STATE_OBJ_TYPE_IDC_SELECTOR,
            ) == -1
            {
                return -1;
            }

            idc = (*idc).next;
            !idc.is_null()
        } {}
        0
    }
}

unsafe fn xml_schema_validate_elem_decl(vctxt: XmlSchemaValidCtxtPtr) -> i32 {
    unsafe {
        let elem_decl: XmlSchemaElementPtr = (*(*vctxt).inode).decl;
        let mut actual_type: XmlSchemaTypePtr;

        // cvc-elt (3.3.4) : 1
        if elem_decl.is_null() {
            VERROR!(
                vctxt,
                XmlParserErrors::XmlSchemavCvcElt1,
                null_mut(),
                "No matching declaration available"
            );
            return (*vctxt).err;
        }
        actual_type = (*elem_decl).subtypes;
        // cvc-elt (3.3.4) : 2
        if (*elem_decl).flags & XML_SCHEMAS_ELEM_ABSTRACT != 0 {
            VERROR!(
                vctxt,
                XmlParserErrors::XmlSchemavCvcElt2,
                null_mut(),
                "The element declaration is abstract"
            );
            return (*vctxt).err;
        }
        if actual_type.is_null() {
            VERROR!(
                vctxt,
                XmlParserErrors::XmlSchemavCvcType1,
                null_mut(),
                "The type definition is absent"
            );
            return XmlParserErrors::XmlSchemavCvcType1 as i32;
        }
        if (*vctxt).nb_attr_infos != 0 {
            let mut ret: i32;
            let mut iattr: XmlSchemaAttrInfoPtr;
            // cvc-elt (3.3.4) : 3
            // Handle 'xsi:nil'.
            iattr = xml_schema_get_meta_attr_info(vctxt, XML_SCHEMA_ATTR_INFO_META_XSI_NIL);
            if !iattr.is_null() {
                ACTIVATE_ATTRIBUTE!(vctxt, iattr);
                // Validate the value.
                ret = xml_schema_vcheck_cvc_simple_type(
                    vctxt as XmlSchemaAbstractCtxtPtr,
                    None,
                    xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasBoolean),
                    (*iattr).value,
                    &raw mut (*iattr).val,
                    1,
                    0,
                    0,
                );
                ACTIVATE_ELEM!(vctxt);
                if ret < 0 {
                    VERROR_INT!(
                        vctxt,
                        "xmlSchemaValidateElemDecl",
                        "calling xmlSchemaVCheckCVCSimpleType() to validate the attribute 'xsi:nil'"
                    );
                    return -1;
                }
                if ret == 0 {
                    if (*elem_decl).flags & XML_SCHEMAS_ELEM_NILLABLE == 0 {
                        // cvc-elt (3.3.4) : 3.1
                        VERROR!(
                            vctxt,
                            XmlParserErrors::XmlSchemavCvcElt3_1,
                            null_mut(),
                            "The element is not 'nillable'"
                        );
                        // Does not return an error on purpose.
                    } else if xml_schema_value_get_as_boolean((*iattr).val) != 0 {
                        // cvc-elt (3.3.4) : 3.2.2
                        if (*elem_decl).flags & XML_SCHEMAS_ELEM_FIXED != 0
                            && !(*elem_decl).value.is_null()
                        {
                            VERROR!(
                                vctxt,
                                XmlParserErrors::XmlSchemavCvcElt3_2_2,
                                null_mut(),
                                "The element cannot be 'nilled' because there is a fixed value constraint defined for it"
                            );
                            // Does not return an error on purpose.
                        } else {
                            (*(*vctxt).inode).flags |= XML_SCHEMA_ELEM_INFO_NILLED;
                        }
                    }
                }
            }
            // cvc-elt (3.3.4) : 4
            // Handle 'xsi:type'.
            iattr = xml_schema_get_meta_attr_info(vctxt, XML_SCHEMA_ATTR_INFO_META_XSI_TYPE);
            if !iattr.is_null() {
                let mut local_type: XmlSchemaTypePtr = null_mut();

                ret = xml_schema_process_xsi_type(vctxt, iattr, &raw mut local_type, elem_decl);
                if ret != 0 && ret == -1 {
                    VERROR_INT!(
                        vctxt,
                        "xmlSchemaValidateElemDecl",
                        "calling xmlSchemaProcessXSIType() to process the attribute 'xsi:type'"
                    );
                    return -1;
                    // Does not return an error on purpose.
                }
                if !local_type.is_null() {
                    (*(*vctxt).inode).flags |= XML_SCHEMA_ELEM_INFO_LOCAL_TYPE;
                    actual_type = local_type;
                }
            }
        }
        // IDC: Register identity-constraint XPath matchers.
        if !(*elem_decl).idcs.is_null() && xml_schema_idc_register_matchers(vctxt, elem_decl) == -1
        {
            return -1;
        }
        // No actual type definition.
        if actual_type.is_null() {
            VERROR!(
                vctxt,
                XmlParserErrors::XmlSchemavCvcType1,
                null_mut(),
                "The type definition is absent"
            );
            return XmlParserErrors::XmlSchemavCvcType1 as i32;
        }
        // Remember the actual type definition.
        (*(*vctxt).inode).type_def = actual_type;

        0
    }
}

/// Evaluates all active XPath state objects.
///
/// Returns the number of IC "field" state objects which resolved to
/// this node, 0 if none resolved and -1 on internal errors.
#[doc(alias = "xmlSchemaXPathEvaluate")]
unsafe fn xml_schema_xpath_evaluate(
    vctxt: XmlSchemaValidCtxtPtr,
    node_type: XmlElementType,
) -> i32 {
    unsafe {
        let mut sto: XmlSchemaIDCStateObjPtr;
        let mut head: XmlSchemaIDCStateObjPtr = null_mut();
        let mut res: i32;
        let mut resolved: i32 = 0;
        let mut depth: i32 = (*vctxt).depth;

        if (*vctxt).xpath_states.is_null() {
            return 0;
        }

        if node_type == XmlElementType::XmlAttributeNode {
            depth += 1;
        }
        // Process all active XPath state objects.
        let first: XmlSchemaIDCStateObjPtr = (*vctxt).xpath_states;
        sto = first;
        while sto != head {
            if node_type == XmlElementType::XmlElementNode {
                res = xml_stream_push(
                    (*sto).xpath_ctxt as XmlStreamCtxtPtr,
                    (*(*vctxt).inode).local_name,
                    (*(*vctxt).inode).ns_name,
                );
            } else {
                res = xml_stream_push_attr(
                    (*sto).xpath_ctxt as XmlStreamCtxtPtr,
                    (*(*vctxt).inode).local_name,
                    (*(*vctxt).inode).ns_name,
                );
            }

            if res == -1 {
                VERROR_INT!(vctxt, "xmlSchemaXPathEvaluate", "calling xmlStreamPush()");
                return -1;
            }
            if res == 0 {
                // goto next_sto;
            } else {
                // Full match.

                // Register a match in the state object history.
                if (*sto).history.is_null() {
                    (*sto).history = xml_malloc(5 * size_of::<i32>()) as *mut i32;
                    if (*sto).history.is_null() {
                        xml_schema_verr_memory(
                            null_mut(),
                            "allocating the state object history",
                            None,
                        );
                        return -1;
                    }
                    (*sto).size_history = 5;
                } else if (*sto).size_history <= (*sto).nb_history {
                    (*sto).size_history *= 2;
                    (*sto).history = xml_realloc(
                        (*sto).history as _,
                        (*sto).size_history as usize * size_of::<i32>(),
                    ) as *mut i32;
                    if (*sto).history.is_null() {
                        xml_schema_verr_memory(
                            null_mut(),
                            "re-allocating the state object history",
                            None,
                        );
                        return -1;
                    }
                }
                *(*sto).history.add((*sto).nb_history as usize) = depth;
                (*sto).nb_history += 1;

                if (*sto).typ == XPATH_STATE_OBJ_TYPE_IDC_SELECTOR {
                    let mut sel: XmlSchemaIdcselectPtr;
                    // Activate state objects for the IDC fields of the IDC selector.
                    sel = (*(*(*(*sto).matcher).aidc).def).fields;
                    while !sel.is_null() {
                        if xml_schema_idc_add_state_object(
                            vctxt,
                            (*sto).matcher,
                            sel,
                            XPATH_STATE_OBJ_TYPE_IDC_FIELD,
                        ) == -1
                        {
                            return -1;
                        }
                        sel = (*sel).next;
                    }
                } else if (*sto).typ == XPATH_STATE_OBJ_TYPE_IDC_FIELD {
                    // An IDC key node was found by the IDC field.

                    // Notify that the character value of this node is needed.
                    if resolved == 0
                        && (*(*vctxt).inode).flags & XML_SCHEMA_NODE_INFO_VALUE_NEEDED == 0
                    {
                        (*(*vctxt).inode).flags |= XML_SCHEMA_NODE_INFO_VALUE_NEEDED;
                    }
                    resolved += 1;
                }
            }
            // next_sto:
            if (*sto).next.is_null() {
                // Evaluate field state objects created on this node as well.
                head = first;
                sto = (*vctxt).xpath_states;
            } else {
                sto = (*sto).next;
            }
        }
        resolved
    }
}

unsafe fn xml_schema_get_idc_designation(idc: XmlSchemaIDCPtr) -> String {
    unsafe { xml_schema_get_component_designation(idc as _) }
}

/// The validation context is used to store an IDC key.
///
/// Returns 0 if succeeded, -1 on internal errors.
#[doc(alias = "xmlSchemaIDCStoreKey")]
unsafe fn xml_schema_idc_store_key(
    vctxt: XmlSchemaValidCtxtPtr,
    key: XmlSchemaPSVIIDCKeyPtr,
) -> i32 {
    unsafe {
        // Add to global list.
        if (*vctxt).idc_keys.is_null() {
            (*vctxt).idc_keys =
                xml_malloc(40 * size_of::<XmlSchemaPSVIIDCKeyPtr>()) as *mut XmlSchemaPSVIIDCKeyPtr;
            if (*vctxt).idc_keys.is_null() {
                xml_schema_verr_memory(vctxt, "allocating the IDC key storage list", None);
                return -1;
            }
            (*vctxt).size_idc_keys = 40;
        } else if (*vctxt).size_idc_keys <= (*vctxt).nb_idc_keys {
            (*vctxt).size_idc_keys *= 2;
            (*vctxt).idc_keys = xml_realloc(
                (*vctxt).idc_keys as _,
                (*vctxt).size_idc_keys as usize * size_of::<XmlSchemaPSVIIDCKeyPtr>(),
            ) as *mut XmlSchemaPSVIIDCKeyPtr;
            if (*vctxt).idc_keys.is_null() {
                xml_schema_verr_memory(vctxt, "re-allocating the IDC key storage list", None);
                return -1;
            }
        }
        *(*vctxt).idc_keys.add((*vctxt).nb_idc_keys as usize) = key;
        (*vctxt).nb_idc_keys += 1;

        0
    }
}

unsafe fn xml_schema_idc_acquire_target_list(
    _vctxt: XmlSchemaValidCtxtPtr,
    matcher: XmlSchemaIDCMatcherPtr,
) -> XmlSchemaItemListPtr<*mut c_void> {
    unsafe {
        if (*matcher).targets.is_null() {
            (*matcher).targets = xml_schema_item_list_create::<*mut c_void>();
        }
        (*matcher).targets
    }
}

unsafe fn xml_schema_get_canon_value_hash(
    val: XmlSchemaValPtr,
    ret_value: *mut *mut XmlChar,
) -> i32 {
    unsafe {
        xml_schema_get_canon_value_whtsp_ext_1(
            val,
            XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse,
            ret_value,
            1,
        )
    }
}

unsafe fn xml_schema_format_idc_key_sequence_1(
    vctxt: XmlSchemaValidCtxtPtr,
    seq: *mut XmlSchemaPSVIIDCKeyPtr,
    count: i32,
    for_hash: i32,
) -> String {
    unsafe {
        let mut res: i32;
        let mut value: *mut XmlChar = null_mut();

        let mut buf = "[".to_owned();
        for i in 0..count {
            buf.push('\'');
            if for_hash == 0 {
                res = xml_schema_get_canon_value_whtsp_ext(
                    (*(*seq.add(i as usize))).val,
                    (*(*(*seq.add(i as usize))).typ)
                        .white_space_facet_value()
                        .unwrap(),
                    &raw mut value,
                );
            } else {
                res =
                    xml_schema_get_canon_value_hash((*(*seq.add(i as usize))).val, &raw mut value);
            }
            if res == 0 {
                buf.push_str(
                    CStr::from_ptr(value as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                );
            } else {
                VERROR_INT!(
                    vctxt,
                    "xmlSchemaFormatIDCKeySequence",
                    "failed to compute a canonical value"
                );
                buf.push_str("???");
            }
            if i < count - 1 {
                buf.push_str("', ");
            } else {
                buf.push('\'');
            }
            if !value.is_null() {
                xml_free(value as _);
                value = null_mut();
            }
        }
        buf.push(']');
        buf
    }
}

unsafe fn xml_schema_hash_key_sequence(
    vctxt: XmlSchemaValidCtxtPtr,
    seq: *mut XmlSchemaPSVIIDCKeyPtr,
    count: i32,
) -> String {
    unsafe { xml_schema_format_idc_key_sequence_1(vctxt, seq, count, 1) }
}

unsafe fn xml_schema_format_idc_key_sequence(
    vctxt: XmlSchemaValidCtxtPtr,
    seq: *mut XmlSchemaPSVIIDCKeyPtr,
    count: i32,
) -> String {
    unsafe { xml_schema_format_idc_key_sequence_1(vctxt, seq, count, 0) }
}

/// The validation context is used to store IDC node table items.
/// They are stored to avoid copying them if IDC node-tables are merged
/// with corresponding parent IDC node-tables (bubbling).
///
/// Returns 0 if succeeded, -1 on internal errors.
#[doc(alias = "xmlSchemaIDCStoreNodeTableItem")]
unsafe fn xml_schema_idc_store_node_table_item(
    vctxt: XmlSchemaValidCtxtPtr,
    item: XmlSchemaPSVIIDCNodePtr,
) -> i32 {
    unsafe {
        // Add to global list.
        if (*vctxt).idc_nodes.is_null() {
            (*vctxt).idc_nodes = xml_malloc(20 * size_of::<XmlSchemaPSVIIDCNodePtr>())
                as *mut XmlSchemaPSVIIDCNodePtr;
            if (*vctxt).idc_nodes.is_null() {
                xml_schema_verr_memory(vctxt, "allocating the IDC node table item list", None);
                return -1;
            }
            (*vctxt).size_idc_nodes = 20;
        } else if (*vctxt).size_idc_nodes <= (*vctxt).nb_idc_nodes {
            (*vctxt).size_idc_nodes *= 2;
            (*vctxt).idc_nodes = xml_realloc(
                (*vctxt).idc_nodes as _,
                (*vctxt).size_idc_nodes as usize * size_of::<XmlSchemaPSVIIDCNodePtr>(),
            ) as *mut XmlSchemaPSVIIDCNodePtr;
            if (*vctxt).idc_nodes.is_null() {
                xml_schema_verr_memory(vctxt, "re-allocating the IDC node table item list", None);
                return -1;
            }
        }
        *(*vctxt).idc_nodes.add((*vctxt).nb_idc_nodes as usize) = item;
        (*vctxt).nb_idc_nodes += 1;

        0
    }
}

unsafe fn xml_schema_vadd_node_qname(
    vctxt: XmlSchemaValidCtxtPtr,
    mut lname: *const XmlChar,
    mut nsname: *const XmlChar,
) -> i32 {
    unsafe {
        lname = xml_dict_lookup((*vctxt).dict, lname, -1);
        if lname.is_null() {
            return -1;
        }
        if !nsname.is_null() {
            nsname = xml_dict_lookup((*vctxt).dict, nsname, -1);
            if nsname.is_null() {
                return -1;
            }
        }
        for i in (0..(*(*vctxt).node_qnames).items.len()).step_by(2) {
            if (*(*vctxt).node_qnames).items[i] == lname as _
                && (*(*vctxt).node_qnames).items[i] == nsname as _
            {
                // Already there
                return i as i32;
            }
        }
        /* Add new entry. */
        let i: i32 = (*(*vctxt).node_qnames).items.len() as i32;
        (*(*vctxt).node_qnames).push(lname as _);
        (*(*vctxt).node_qnames).push(nsname as _);
        i
    }
}

/// Processes and pops the history items of the IDC state objects.
/// IDC key-sequences are validated/created on IDC bindings.
///
/// Returns 0 on success and -1 on internal errors.
#[doc(alias = "xmlSchemaXPathProcessHistory")]
unsafe fn xml_schema_xpath_process_history(vctxt: XmlSchemaValidCtxtPtr, depth: i32) -> i32 {
    unsafe {
        let mut sto: XmlSchemaIDCStateObjPtr;
        let mut nextsto: XmlSchemaIDCStateObjPtr;
        let mut res: i32;
        let mut match_depth: i32;
        let mut key: XmlSchemaPSVIIDCKeyPtr = null_mut();
        let typ: XmlSchemaTypePtr = (*(*vctxt).inode).type_def;
        let mut simple_type: XmlSchemaTypePtr;

        if (*vctxt).xpath_states.is_null() {
            return 0;
        }
        sto = (*vctxt).xpath_states;

        // Evaluate the state objects.
        'main: while !sto.is_null() {
            res = xml_stream_pop((*sto).xpath_ctxt as XmlStreamCtxtPtr);
            if res == -1 {
                VERROR_INT!(
                    vctxt,
                    "xmlSchemaXPathProcessHistory",
                    "calling xmlStreamPop()"
                );
                return -1;
            }
            'deregister_check: {
                if (*sto).nb_history == 0 {
                    break 'deregister_check;
                }
                match_depth = *(*sto).history.add((*sto).nb_history as usize - 1);

                // Only matches at the current depth are of interest.
                if match_depth != depth {
                    sto = (*sto).next;
                    continue 'main;
                }
                if (*sto).typ == XPATH_STATE_OBJ_TYPE_IDC_FIELD {
                    // NOTE: According to
                    //   http://www.w3.org/Bugs/Public/show_bug.cgi?id=2198
                    //   ... the simple-content of complex types is also allowed.

                    if wxs_is_complex(typ) {
                        if WXS_HAS_SIMPLE_CONTENT!(typ) {
                            // Sanity check for complex types with simple content.
                            simple_type = (*typ).content_type_def;
                            if simple_type.is_null() {
                                VERROR_INT!(
                                    vctxt,
                                    "xmlSchemaXPathProcessHistory",
                                    "field resolves to a CT with simple content but the CT is missing the ST definition"
                                );
                                return -1;
                            }
                        } else {
                            simple_type = null_mut();
                        }
                    } else {
                        simple_type = typ;
                    }
                    if simple_type.is_null() {
                        let xpath =
                            CStr::from_ptr((*(*sto).sel).xpath as *const i8).to_string_lossy();
                        let desig = xml_schema_get_idc_designation((*(*(*sto).matcher).aidc).def);

                        // Not qualified if the field resolves to a node of non simple type.
                        xml_schema_custom_err(
                            vctxt as XmlSchemaAbstractCtxtPtr,
                            XmlParserErrors::XmlSchemavCvcIdc,
                            None,
                            (*(*(*sto).matcher).aidc).def as XmlSchemaBasicItemPtr,
                            format!("The XPath '{xpath}' of a field of {desig} does evaluate to a node of non-simple type").as_str(),
                            Some(&xpath),
                            Some(&desig)
                        );
                        (*sto).nb_history -= 1;
                        break 'deregister_check;
                    }
                    if key.is_null() && (*(*vctxt).inode).val.is_null() {
                        // Failed to provide the normalized value; maybe the value was invalid.
                        VERROR!(
                            vctxt,
                            XmlParserErrors::XmlSchemavCvcIdc,
                            (*(*(*sto).matcher).aidc).def as XmlSchemaBasicItemPtr,
                            "Warning: No precomputed value available, the value was either invalid or something strange happened"
                        );
                        (*sto).nb_history -= 1;
                        break 'deregister_check;
                    } else {
                        let matcher: XmlSchemaIDCMatcherPtr = (*sto).matcher;
                        let mut key_seq: *mut XmlSchemaPSVIIDCKeyPtr;

                        // The key will be anchored on the matcher's list of
                        // key-sequences. The position in this list is determined
                        // by the target node's depth relative to the matcher's
                        // depth of creation (i.e. the depth of the scope element).
                        //
                        // Element        Depth    Pos   List-entries
                        // <scope>          0              NULL
                        //   <bar>          1              NULL
                        //     <target/>    2       2      target
                        //   <bar>
                        // </scope>
                        //
                        // The size of the list is only dependent on the depth of
                        // the tree.
                        // An entry will be NULLed in selector_leave, i.e. when
                        // we hit the target's
                        let pos: i32 = (*sto).depth - (*matcher).depth;
                        let idx: i32 = (*(*sto).sel).index;

                        // Create/grow the array of key-sequences.
                        if (*matcher).key_seqs.is_null() {
                            if pos > 9 {
                                (*matcher).size_key_seqs = pos * 2;
                            } else {
                                (*matcher).size_key_seqs = 10;
                            }
                            (*matcher).key_seqs = xml_malloc(
                                (*matcher).size_key_seqs as usize
                                    * size_of::<*mut XmlSchemaPSVIIDCKeyPtr>(),
                            )
                                as *mut *mut XmlSchemaPSVIIDCKeyPtr;
                            if (*matcher).key_seqs.is_null() {
                                xml_schema_verr_memory(
                                    null_mut(),
                                    "allocating an array of key-sequences",
                                    None,
                                );
                                return -1;
                            }
                            memset(
                                (*matcher).key_seqs as _,
                                0,
                                (*matcher).size_key_seqs as usize
                                    * size_of::<*mut XmlSchemaPSVIIDCKeyPtr>(),
                            );
                        } else if pos >= (*matcher).size_key_seqs {
                            let i: i32 = (*matcher).size_key_seqs;

                            (*matcher).size_key_seqs = pos * 2;
                            (*matcher).key_seqs = xml_realloc(
                                (*matcher).key_seqs as _,
                                (*matcher).size_key_seqs as usize
                                    * size_of::<*mut XmlSchemaPSVIIDCKeyPtr>(),
                            )
                                as *mut *mut XmlSchemaPSVIIDCKeyPtr;
                            if (*matcher).key_seqs.is_null() {
                                xml_schema_verr_memory(
                                    null_mut(),
                                    "reallocating an array of key-sequences",
                                    None,
                                );
                                return -1;
                            }
                            // The array needs to be NULLed.
                            // TODO: Use memset?
                            for i in i..(*matcher).size_key_seqs {
                                *(*matcher).key_seqs.add(i as usize) = null_mut();
                            }
                        }

                        // Get/create the key-sequence.
                        'create_key: {
                            'create_sequence: {
                                key_seq = *(*matcher).key_seqs.add(pos as usize);
                                if key_seq.is_null() {
                                    break 'create_sequence;
                                } else if !(*key_seq.add(idx as usize)).is_null() {
                                    let xpath = CStr::from_ptr((*(*sto).sel).xpath as *const i8)
                                        .to_string_lossy();
                                    let desig =
                                        xml_schema_get_idc_designation((*(*matcher).aidc).def);

                                    // cvc-identity-constraint:
                                    // 3 For each node in the `target node set` all
                                    // of the {fields}, with that node as the context
                                    // node, evaluate to either an empty node-set or
                                    // a node-set with exactly one member, which must
                                    // have a simple type.
                                    //
                                    // The key was already set; report an error.
                                    xml_schema_custom_err(
                                        vctxt as XmlSchemaAbstractCtxtPtr,
                                        XmlParserErrors::XmlSchemavCvcIdc,
                                        None,
                                        (*(*matcher).aidc).def as XmlSchemaBasicItemPtr,
                                        format!("The XPath '{xpath}' of a field of {desig} evaluates to a node-set with more than one member").as_str(),
                                        Some(&xpath),
                                        Some(&desig)
                                    );
                                    (*sto).nb_history -= 1;
                                    break 'deregister_check;
                                } else {
                                    break 'create_key;
                                }
                            }
                            // create_sequence:
                            // Create a key-sequence.
                            key_seq = xml_malloc(
                                (*(*(*matcher).aidc).def).nb_fields as usize
                                    * size_of::<XmlSchemaPSVIIDCKeyPtr>(),
                            ) as *mut XmlSchemaPSVIIDCKeyPtr;
                            if key_seq.is_null() {
                                xml_schema_verr_memory(
                                    null_mut(),
                                    "allocating an IDC key-sequence",
                                    None,
                                );
                                return -1;
                            }
                            memset(
                                key_seq as _,
                                0,
                                (*(*(*matcher).aidc).def).nb_fields as usize
                                    * size_of::<XmlSchemaPSVIIDCKeyPtr>(),
                            );
                            *(*matcher).key_seqs.add(pos as usize) = key_seq;
                        }
                        // create_key:
                        // Create a key once per node only.
                        if key.is_null() {
                            key = xml_malloc(size_of::<XmlSchemaPSVIIDCKey>())
                                as XmlSchemaPSVIIDCKeyPtr;
                            if key.is_null() {
                                xml_schema_verr_memory(null_mut(), "allocating a IDC key", None);
                                xml_free(key_seq as _);
                                *(*matcher).key_seqs.add(pos as usize) = null_mut();
                                return -1;
                            }
                            // Consume the compiled value.
                            (*key).typ = simple_type;
                            (*key).val = (*(*vctxt).inode).val;
                            (*(*vctxt).inode).val = null_mut();
                            // Store the key in a global list.
                            if xml_schema_idc_store_key(vctxt, key) == -1 {
                                xml_schema_idc_free_key(key);
                                return -1;
                            }
                        }
                        *key_seq.add(idx as usize) = key;
                    }
                } else if (*sto).typ == XPATH_STATE_OBJ_TYPE_IDC_SELECTOR {
                    let mut key_seq: *mut *mut XmlSchemaPSVIIDCKeyPtr = null_mut();
                    // let bind: xmlSchemaPSVIIDCBindingPtr;
                    let nt_item: XmlSchemaPSVIIDCNodePtr;
                    let targets: XmlSchemaItemListPtr<*mut c_void>;

                    // Here we have the following scenario:
                    // An IDC 'selector' state object resolved to a target node,
                    // during the time this target node was in the
                    // ancestor-or-self axis, the 'field' state object(s) looked
                    // out for matching nodes to create a key-sequence for this
                    // target node. Now we are back to this target node and need
                    // to put the key-sequence, together with the target node
                    // itself, into the node-table of the corresponding IDC binding.
                    let matcher: XmlSchemaIDCMatcherPtr = (*sto).matcher;
                    let idc: XmlSchemaIDCPtr = (*(*matcher).aidc).def;
                    let nb_keys: i32 = (*idc).nb_fields;
                    let pos: i32 = depth - (*matcher).depth;
                    // Check if the matcher has any key-sequences at all, plus
                    // if it has a key-sequence for the current target node.
                    'selector_leave: {
                        'selector_key_error: {
                            if (*matcher).key_seqs.is_null() || (*matcher).size_key_seqs <= pos {
                                if (*idc).typ == XmlSchemaTypeType::XmlSchemaTypeIDCKey {
                                    break 'selector_key_error;
                                } else {
                                    break 'selector_leave;
                                }
                            }

                            key_seq = (*matcher).key_seqs.add(pos as usize);
                            if (*key_seq).is_null() {
                                if (*idc).typ == XmlSchemaTypeType::XmlSchemaTypeIDCKey {
                                    break 'selector_key_error;
                                } else {
                                    break 'selector_leave;
                                }
                            }
                            for i in 0..nb_keys {
                                if (*(*key_seq).add(i as usize)).is_null() {
                                    // Not qualified, if not all fields did resolve.
                                    if (*idc).typ == XmlSchemaTypeType::XmlSchemaTypeIDCKey {
                                        // All fields of a "key" IDC must resolve.
                                        break 'selector_key_error;
                                    }
                                    break 'selector_leave;
                                }
                            }

                            // All fields did resolve.

                            // 4.1 If the {identity-constraint category} is unique(/key),
                            // then no two members of the `qualified node set` have
                            // `key-sequences` whose members are pairwise equal, as
                            // defined by Equal in [XML Schemas: Datatypes].
                            //
                            // Get the IDC binding from the matcher and check for
                            // duplicate key-sequences.
                            // #if 0
                            // bind = xmlSchemaIDCAcquireBinding(vctxt, matcher);
                            // #endif
                            targets = xml_schema_idc_acquire_target_list(vctxt, matcher);
                            if (*idc).typ != XmlSchemaTypeType::XmlSchemaTypeIDCKeyref
                                && !(*targets).items.is_empty()
                            {
                                let mut ckey: XmlSchemaPSVIIDCKeyPtr;
                                let mut bkey: XmlSchemaPSVIIDCKeyPtr;
                                let mut bkey_seq: *mut XmlSchemaPSVIIDCKeyPtr;
                                let mut e: XmlIDCHashEntryPtr;

                                res = 0;

                                if (*matcher).htab.is_null() {
                                    e = null_mut();
                                } else {
                                    let value = CString::new(xml_schema_hash_key_sequence(
                                        vctxt, *key_seq, nb_keys,
                                    ))
                                    .unwrap();
                                    e = xml_hash_lookup(
                                        (*matcher).htab,
                                        value.as_ptr() as *const u8,
                                    ) as _;
                                }

                                // Compare the key-sequences, key by key.
                                while !e.is_null() {
                                    bkey_seq = (*(((*targets).items[(*e).index as usize])
                                        as XmlSchemaPSVIIDCNodePtr))
                                        .keys;
                                    for j in 0..nb_keys {
                                        ckey = *(*key_seq).add(j as usize);
                                        bkey = *bkey_seq.add(j as usize);
                                        res = xml_schema_are_values_equal((*ckey).val, (*bkey).val);
                                        if res == -1 {
                                            return -1;
                                        } else if res == 0 {
                                            // One of the keys differs, so the key-sequence
                                            // won't be equal; get out.
                                            break;
                                        }
                                    }
                                    if res == 1 {
                                        // Duplicate key-sequence found.
                                        break;
                                    }
                                    e = (*e).next;
                                }
                                if !e.is_null() {
                                    let seq = xml_schema_format_idc_key_sequence(
                                        vctxt, *key_seq, nb_keys,
                                    );
                                    let desig = xml_schema_get_idc_designation(idc);
                                    // TODO: Try to report the key-sequence.
                                    xml_schema_custom_err(
                                        vctxt as XmlSchemaAbstractCtxtPtr,
                                        XmlParserErrors::XmlSchemavCvcIdc,
                                        None,
                                        idc as XmlSchemaBasicItemPtr,
                                        format!("Duplicate key-sequence {seq} in {desig}").as_str(),
                                        Some(&seq),
                                        Some(&desig),
                                    );
                                    break 'selector_leave;
                                }
                            }

                            // Add a node-table item to the IDC binding.
                            nt_item = xml_malloc(size_of::<XmlSchemaPSVIIDCNode>())
                                as XmlSchemaPSVIIDCNodePtr;
                            if nt_item.is_null() {
                                xml_schema_verr_memory(
                                    null_mut(),
                                    "allocating an IDC node-table item",
                                    None,
                                );
                                xml_free(*key_seq as _);
                                *key_seq = null_mut();
                                return -1;
                            }
                            memset(nt_item as _, 0, size_of::<XmlSchemaPSVIIDCNode>());

                            // Store the node-table item in a global list.
                            if (*idc).typ != XmlSchemaTypeType::XmlSchemaTypeIDCKeyref {
                                if xml_schema_idc_store_node_table_item(vctxt, nt_item) == -1 {
                                    xml_free(nt_item as _);
                                    xml_free(*key_seq as _);
                                    *key_seq = null_mut();
                                    return -1;
                                }
                                (*nt_item).node_qname_id = -1;
                            } else {
                                // Save a cached QName for this node on the IDC node, to be
                                // able to report it, even if the node is not saved.
                                (*nt_item).node_qname_id = xml_schema_vadd_node_qname(
                                    vctxt,
                                    (*(*vctxt).inode).local_name,
                                    (*(*vctxt).inode).ns_name,
                                );
                                if (*nt_item).node_qname_id == -1 {
                                    xml_free(nt_item as _);
                                    xml_free(*key_seq as _);
                                    *key_seq = null_mut();
                                    return -1;
                                }
                            }
                            // Init the node-table item: Save the node, position and
                            // consume the key-sequence.
                            (*nt_item).node = (*vctxt).node;
                            (*nt_item).node_line = (*(*vctxt).inode).node_line;
                            (*nt_item).keys = *key_seq;
                            *key_seq = null_mut();

                            if (*targets).push(nt_item as _) == -1 {
                                if (*idc).typ == XmlSchemaTypeType::XmlSchemaTypeIDCKeyref {
                                    // Free the item, since keyref items won't be
                                    // put on a global list.
                                    xml_free((*nt_item).keys as _);
                                    xml_free(nt_item as _);
                                }
                                return -1;
                            }
                            if (*idc).typ != XmlSchemaTypeType::XmlSchemaTypeIDCKeyref {
                                if (*matcher).htab.is_null() {
                                    (*matcher).htab = xml_hash_create(4);
                                }
                                let value = CString::new(xml_schema_hash_key_sequence(
                                    vctxt,
                                    (*nt_item).keys,
                                    nb_keys,
                                ))
                                .unwrap();
                                let e: XmlIDCHashEntryPtr =
                                    xml_malloc(size_of::<XmlIDCHashEntry>()) as _;
                                (*e).index = (*targets).items.len() as i32 - 1;
                                let r: XmlIDCHashEntryPtr =
                                    xml_hash_lookup((*matcher).htab, value.as_ptr() as *const u8)
                                        as _;
                                if !r.is_null() {
                                    (*e).next = (*r).next;
                                    (*r).next = e;
                                } else {
                                    (*e).next = null_mut();
                                    xml_hash_add_entry(
                                        (*matcher).htab,
                                        value.as_ptr() as *const u8,
                                        e as _,
                                    );
                                }
                            }

                            break 'selector_leave;
                        }
                        // selector_key_error:
                        {
                            let desig = xml_schema_get_idc_designation(idc);

                            // 4.2.1 (KEY) The `target node set` and the
                            // `qualified node set` are equal, that is, every
                            // member of the `target node set` is also a member
                            // of the `qualified node set` and vice versa.
                            xml_schema_custom_err(
                                vctxt as XmlSchemaAbstractCtxtPtr,
                                XmlParserErrors::XmlSchemavCvcIdc,
                                None,
                                idc as XmlSchemaBasicItemPtr,
                                format!("Not all fields of {desig} evaluate to a node").as_str(),
                                Some(&desig),
                                None,
                            );
                        }
                    }
                    // selector_leave:
                    // Free the key-sequence if not added to the IDC table.
                    if !key_seq.is_null() && !(*key_seq).is_null() {
                        xml_free(*key_seq as _);
                        *key_seq = null_mut();
                    }
                } /* if selector */

                (*sto).nb_history -= 1;
            }

            // deregister_check:
            // Deregister state objects if they reach the depth of creation.
            if (*sto).nb_history == 0 && (*sto).depth == depth {
                if (*vctxt).xpath_states != sto {
                    VERROR_INT!(
                        vctxt,
                        "xmlSchemaXPathProcessHistory",
                        "The state object to be removed is not the first in the list"
                    );
                }
                nextsto = (*sto).next;
                // Unlink from the list of active XPath state objects.
                (*vctxt).xpath_states = (*sto).next;
                (*sto).next = (*vctxt).xpath_state_pool;
                // Link it to the pool of reusable state objects.
                (*vctxt).xpath_state_pool = sto;
                sto = nextsto;
            } else {
                sto = (*sto).next;
            }
        } /* while !sto.is_null() */
        0
    }
}

/// Pops all XPath states.
///
/// Returns 0 on success and -1 on internal errors.
#[doc(alias = "xmlSchemaXPathPop")]
unsafe fn xml_schema_xpath_pop(vctxt: XmlSchemaValidCtxtPtr) -> i32 {
    unsafe {
        let mut sto: XmlSchemaIDCStateObjPtr;
        let mut res: i32;

        if (*vctxt).xpath_states.is_null() {
            return 0;
        }
        sto = (*vctxt).xpath_states;
        while {
            res = xml_stream_pop((*sto).xpath_ctxt as XmlStreamCtxtPtr);
            if res == -1 {
                return -1;
            }
            sto = (*sto).next;
            !sto.is_null()
        } {}
        0
    }
}

// 3.4.4 Complex Type Definition Validation Rules
//   Element Locally Valid (Complex Type) (cvc-complex-type)
// 3.2.4 Attribute Declaration Validation Rules
//   Validation Rule: Attribute Locally Valid (cvc-attribute)
//   Attribute Locally Valid (Use) (cvc-au)
//
// Only "assessed" attribute information items will be visible to
// IDCs. I.e. not "lax" (without declaration) and "skip" wild attributes.
unsafe fn xml_schema_vattributes_complex(vctxt: XmlSchemaValidCtxtPtr) -> i32 {
    unsafe {
        let typ: XmlSchemaTypePtr = (*(*vctxt).inode).type_def;
        let mut attr_use: XmlSchemaAttributeUsePtr;
        let mut attr_decl: XmlSchemaAttributePtr;
        let mut iattr: XmlSchemaAttrInfoPtr;
        let mut tmpiattr: XmlSchemaAttrInfoPtr;
        let mut found: i32;
        let mut xpath_res: i32;
        let mut res: i32;
        let mut wild_ids = 0;
        let mut fixed: i32;

        // SPEC (cvc-attribute)
        // (1) "The declaration must not be `absent` (see Missing
        // Sub-components ($5.3) for how this can fail to be
        // the case)."
        // (2) "Its {type definition} must not be absent."
        //
        // NOTE (1) + (2): This is not handled here, since we currently do not
        // allow validation against schemas which have missing sub-components.
        //
        // SPEC (cvc-complex-type)
        // (3) "For each attribute information item in the element information
        // item's [attributes] excepting those whose [namespace name] is
        // identical to http://www.w3.org/2001/XMLSchema-instance and whose
        // [local name] is one of type, nil, schemaLocation or
        // noNamespaceSchemaLocation, the appropriate case among the following
        // must be true:
        //
        let attr_use_list: XmlSchemaItemListPtr<*mut c_void> =
            (*typ).attr_uses as XmlSchemaItemListPtr<*mut c_void>;
        // @nbAttrs is the number of attributes present in the instance.
        let nb_attrs: i32 = (*vctxt).nb_attr_infos;
        let nb_uses = if !attr_use_list.is_null() {
            (*attr_use_list).items.len()
        } else {
            0
        };
        for i in 0..nb_uses {
            found = 0;
            attr_use = (*attr_use_list).items[i] as _;
            attr_decl = WXS_ATTRUSE_DECL!(attr_use);
            for j in 0..nb_attrs {
                iattr = *(*vctxt).attr_infos.add(j as usize);
                // SPEC (cvc-complex-type) (3)
                // Skip meta attributes.
                if (*iattr).meta_type != 0 {
                    continue;
                }
                if *(*iattr).local_name.add(0) != *(*attr_decl).name.add(0) {
                    continue;
                }
                if !xml_str_equal((*iattr).local_name, (*attr_decl).name) {
                    continue;
                }
                if !xml_str_equal((*iattr).ns_name, (*attr_decl).target_namespace) {
                    continue;
                }
                found = 1;
                // SPEC (cvc-complex-type)
                // (3.1) "If there is among the {attribute uses} an attribute
                // use with an {attribute declaration} whose {name} matches
                // the attribute information item's [local name] and whose
                // {target namespace} is identical to the attribute information
                // item's [namespace name] (where an `absent` {target namespace}
                // is taken to be identical to a [namespace name] with no value),
                // then the attribute information must be `valid` with respect
                // to that attribute use as per Attribute Locally Valid (Use)
                // ($3.5.4). In this case the {attribute declaration} of that
                // attribute use is the `context-determined declaration` for the
                // attribute information item with respect to Schema-Validity
                // Assessment (Attribute) ($3.2.4) and
                // Assessment Outcome (Attribute) ($3.2.5).
                (*iattr).state = XML_SCHEMAS_ATTR_ASSESSED;
                (*iattr).using = attr_use;
                // Context-determined declaration.
                (*iattr).decl = attr_decl;
                (*iattr).type_def = (*attr_decl).subtypes;
                break;
            }

            if found != 0 {
                continue;
            }

            if (*attr_use).occurs == XML_SCHEMAS_ATTR_USE_REQUIRED {
                // Handle non-existent, required attributes.
                //
                // SPEC (cvc-complex-type)
                // (4) "The {attribute declaration} of each attribute use in
                // the {attribute uses} whose {required} is true matches one
                // of the attribute information items in the element information
                // item's [attributes] as per clause 3.1 above."
                tmpiattr = xml_schema_get_fresh_attr_info(vctxt);
                if tmpiattr.is_null() {
                    VERROR_INT!(
                        vctxt,
                        "xmlSchemaVAttributesComplex",
                        "calling xmlSchemaGetFreshAttrInfo()"
                    );
                    return -1;
                }
                (*tmpiattr).state = XML_SCHEMAS_ATTR_ERR_MISSING;
                (*tmpiattr).using = attr_use;
                (*tmpiattr).decl = attr_decl;
            } else if (*attr_use).occurs == XML_SCHEMAS_ATTR_USE_OPTIONAL
                && (!(*attr_use).def_value.is_null() || !(*attr_decl).def_value.is_null())
            {
                // Handle non-existent, optional, default/fixed attributes.
                tmpiattr = xml_schema_get_fresh_attr_info(vctxt);
                if tmpiattr.is_null() {
                    VERROR_INT!(
                        vctxt,
                        "xmlSchemaVAttributesComplex",
                        "calling xmlSchemaGetFreshAttrInfo()"
                    );
                    return -1;
                }
                (*tmpiattr).state = XML_SCHEMAS_ATTR_DEFAULT;
                (*tmpiattr).using = attr_use;
                (*tmpiattr).decl = attr_decl;
                (*tmpiattr).type_def = (*attr_decl).subtypes;
                (*tmpiattr).local_name = (*attr_decl).name;
                (*tmpiattr).ns_name = (*attr_decl).target_namespace;
            }
        }

        if (*vctxt).nb_attr_infos == 0 {
            return 0;
        }
        // Validate against the wildcard.
        if !(*typ).attribute_wildcard.is_null() {
            // SPEC (cvc-complex-type)
            // (3.2.1) "There must be an {attribute wildcard}."
            for i in 0..nb_attrs {
                iattr = *(*vctxt).attr_infos.add(i as usize);
                // SPEC (cvc-complex-type) (3)
                // Skip meta attributes.
                if (*iattr).state != XML_SCHEMAS_ATTR_UNKNOWN {
                    continue;
                }
                // SPEC (cvc-complex-type)
                // (3.2.2) "The attribute information item must be `valid` with
                // respect to it as defined in Item Valid (Wildcard) ($3.10.4)."
                //
                // SPEC Item Valid (Wildcard) (cvc-wildcard)
                // "... its [namespace name] must be `valid` with respect to
                // the wildcard constraint, as defined in Wildcard allows
                // Namespace Name ($3.10.4)."
                if xml_schema_check_cvcwildcard_namespace(
                    (*typ).attribute_wildcard,
                    (*iattr).ns_name,
                ) == 0
                {
                    // Handle processContents.
                    //
                    // SPEC (cvc-wildcard):
                    // processContents | context-determined declaration:
                    // "strict"          "mustFind"
                    // "lax"             "none"
                    // "skip"            "skip"
                    if (*(*typ).attribute_wildcard).process_contents == XML_SCHEMAS_ANY_SKIP {
                        // context-determined declaration = "skip"
                        //
                        // SPEC PSVI Assessment Outcome (Attribute)
                        // [validity] = "notKnown"
                        // [validation attempted] = "none"
                        (*iattr).state = XML_SCHEMAS_ATTR_WILD_SKIP;
                        continue;
                    }
                    // Find an attribute declaration.
                    (*iattr).decl = (*(*vctxt).schema).get_attribute_decl(
                        CStr::from_ptr((*iattr).local_name as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                        (!(*iattr).ns_name.is_null())
                            .then(|| {
                                CStr::from_ptr((*iattr).ns_name as *const i8).to_string_lossy()
                            })
                            .as_deref(),
                    );
                    if !(*iattr).decl.is_null() {
                        (*iattr).state = XML_SCHEMAS_ATTR_ASSESSED;
                        // SPEC (cvc-complex-type)
                        // (5) "Let [Definition:]  the wild IDs be the set of
                        // all attribute information item to which clause 3.2
                        // applied and whose `validation` resulted in a
                        // `context-determined declaration` of mustFind or no
                        // `context-determined declaration` at all, and whose
                        // [local name] and [namespace name] resolve (as
                        // defined by QName resolution (Instance) ($3.15.4)) to
                        // an attribute declaration whose {type definition} is
                        // or is derived from ID. Then all of the following
                        // must be true:"
                        (*iattr).type_def = (*((*iattr).decl)).subtypes;
                        if xml_schema_is_derived_from_built_in_type(
                            (*iattr).type_def,
                            XmlSchemaValType::XmlSchemasID as i32,
                        ) != 0
                        {
                            // SPEC (5.1) "There must be no more than one item in `wild IDs`."
                            if wild_ids != 0 {
                                // VAL TODO
                                (*iattr).state = XML_SCHEMAS_ATTR_ERR_WILD_DUPLICATE_ID;
                                // TODO
                                continue;
                            }
                            wild_ids += 1;
                            // SPEC (cvc-complex-type)
                            // (5.2) "If `wild IDs` is non-empty, there must not
                            // be any attribute uses among the {attribute uses}
                            // whose {attribute declaration}'s {type definition}
                            // is or is derived from ID."
                            if !attr_use_list.is_null() {
                                for j in 0..(*attr_use_list).items.len() {
                                    if xml_schema_is_derived_from_built_in_type(
                                        WXS_ATTRUSE_TYPEDEF!((*attr_use_list).items[j]),
                                        XmlSchemaValType::XmlSchemasID as i32,
                                    ) != 0
                                    {
                                        // URGENT VAL TODO: implement
                                        (*iattr).state = XML_SCHEMAS_ATTR_ERR_WILD_AND_USE_ID;
                                        // TODO
                                        break;
                                    }
                                }
                            }
                        }
                    } else if (*(*typ).attribute_wildcard).process_contents == XML_SCHEMAS_ANY_LAX {
                        (*iattr).state = XML_SCHEMAS_ATTR_WILD_LAX_NO_DECL;
                        // SPEC PSVI Assessment Outcome (Attribute)
                        // [validity] = "notKnown"
                        // [validation attempted] = "none"
                    } else {
                        (*iattr).state = XML_SCHEMAS_ATTR_ERR_WILD_STRICT_NO_DECL;
                    }
                }
            }
        }

        if (*vctxt).nb_attr_infos == 0 {
            return 0;
        }

        let mut def_attr_owner_elem = None;
        // Get the owner element; needed for creation of default attributes.
        // This fixes bug #341337, reported by David Grohmann.
        if (*vctxt).options & XmlSchemaValidOption::XmlSchemaValVcICreate as i32 != 0 {
            let ielem: XmlSchemaNodeInfoPtr = *(*vctxt).elem_infos.add((*vctxt).depth as usize);
            if !ielem.is_null() && (*ielem).node.is_some_and(|node| node.doc.is_some()) {
                def_attr_owner_elem = (*ielem).node;
            }
        }
        // Validate values, create default attributes, evaluate IDCs.
        'internal_error: {
            for i in 0..(*vctxt).nb_attr_infos {
                iattr = *(*vctxt).attr_infos.add(i as usize);
                // VAL TODO: Note that we won't try to resolve IDCs to
                // "lax" and "skip" validated attributes. Check what to
                // do in this case.
                if (*iattr).state != XML_SCHEMAS_ATTR_ASSESSED
                    && (*iattr).state != XML_SCHEMAS_ATTR_DEFAULT
                {
                    continue;
                }
                // VAL TODO: What to do if the type definition is missing?
                if (*iattr).type_def.is_null() {
                    (*iattr).state = XML_SCHEMAS_ATTR_ERR_NO_TYPE;
                    continue;
                }

                ACTIVATE_ATTRIBUTE!(vctxt, iattr);
                // fixed = 0;
                xpath_res = 0;

                if !(*vctxt).xpath_states.is_null() {
                    // Evaluate IDCs.
                    xpath_res = xml_schema_xpath_evaluate(vctxt, XmlElementType::XmlAttributeNode);
                    if xpath_res == -1 {
                        VERROR_INT!(
                            vctxt,
                            "xmlSchemaVAttributesComplex",
                            "calling xmlSchemaXPathEvaluate()"
                        );
                        break 'internal_error;
                    }
                }

                'eval_idcs: {
                    if (*iattr).state == XML_SCHEMAS_ATTR_DEFAULT {
                        // Default/fixed attributes.
                        // We need the value only if we need to resolve IDCs or
                        // will create default attributes.
                        if xpath_res != 0 || def_attr_owner_elem.is_some() {
                            if !(*(*iattr).using).def_value.is_null() {
                                (*iattr).value = (*(*iattr).using).def_value;
                                (*iattr).val = (*(*iattr).using).def_val;
                            } else {
                                (*iattr).value = (*(*iattr).decl).def_value;
                                (*iattr).val = (*(*iattr).decl).def_val;
                            }
                            // IDCs will consume the precomputed default value,
                            // so we need to clone it.
                            if (*iattr).val.is_null() {
                                VERROR_INT!(
                                    vctxt,
                                    "xmlSchemaVAttributesComplex",
                                    "default/fixed value on an attribute use was not precomputed"
                                );
                                break 'internal_error;
                            }
                            (*iattr).val = xml_schema_copy_value((*iattr).val);
                            if (*iattr).val.is_null() {
                                VERROR_INT!(
                                    vctxt,
                                    "xmlSchemaVAttributesComplex",
                                    "calling xmlSchemaCopyValue()"
                                );
                                break 'internal_error;
                            }
                        }
                        // PSVI: Add the default attribute to the current element.
                        // VAL TODO: Should we use the *normalized* value? This currently
                        //   uses the *initial* value.

                        if let Some(mut def_attr_owner_elem) = def_attr_owner_elem {
                            let mut value: *const XmlChar;

                            value = (*iattr).value;
                            // Normalize the value.
                            let norm_value = (*(*iattr).type_def)
                                .normalize_value(
                                    CStr::from_ptr((*iattr).value as *const i8)
                                        .to_string_lossy()
                                        .as_ref(),
                                )
                                .map_or(null_mut(), |res| {
                                    xml_strndup(res.as_ptr(), res.len() as i32)
                                });
                            if !norm_value.is_null() {
                                value = norm_value;
                            }

                            if (*iattr).ns_name.is_null() {
                                if xml_new_prop(
                                    Some(def_attr_owner_elem),
                                    (*iattr).local_name,
                                    value,
                                )
                                .is_none()
                                {
                                    VERROR_INT!(
                                        vctxt,
                                        "xmlSchemaVAttributesComplex",
                                        "calling xmlNewProp()"
                                    );
                                    if !norm_value.is_null() {
                                        xml_free(norm_value as _);
                                    }
                                    break 'internal_error;
                                }
                            } else {
                                let doc = def_attr_owner_elem.doc;
                                let mut ns = def_attr_owner_elem.search_ns_by_href(
                                    doc,
                                    CStr::from_ptr((*iattr).ns_name as *const i8)
                                        .to_string_lossy()
                                        .as_ref(),
                                );
                                if ns.is_none() {
                                    let mut counter: i32 = 0;
                                    let mut prefix;

                                    // Create a namespace declaration on the validation
                                    // root node if no namespace declaration is in scope.
                                    while {
                                        prefix = format!("p{counter}");
                                        counter += 1;
                                        let doc = def_attr_owner_elem.doc;
                                        ns = def_attr_owner_elem
                                            .search_ns(doc, Some(prefix.as_str()));
                                        if counter > 1000 {
                                            VERROR_INT!(
                                                vctxt,
                                                "xmlSchemaVAttributesComplex",
                                                "could not compute a ns prefix for a default/fixed attribute"
                                            );
                                            if !norm_value.is_null() {
                                                xml_free(norm_value as _);
                                            }
                                            break 'internal_error;
                                        }

                                        ns.is_some()
                                    } {}
                                    ns = xml_new_ns(
                                        (*vctxt).validation_root,
                                        (*iattr).ns_name,
                                        Some(&prefix),
                                    );
                                }
                                // TODO:
                                // http://lists.w3.org/Archives/Public/www-xml-schema-comments/2005JulSep/0406.html
                                // If we have QNames: do we need to ensure there's a
                                // prefix defined for the QName?
                                xml_new_ns_prop(
                                    Some(def_attr_owner_elem),
                                    ns,
                                    &CStr::from_ptr((*iattr).local_name as *const i8)
                                        .to_string_lossy(),
                                    value,
                                );
                            }
                            if !norm_value.is_null() {
                                xml_free(norm_value as _);
                            }
                        }
                        // Go directly to IDC evaluation.
                        break 'eval_idcs;
                    }
                    // Validate the value.
                    if !(*vctxt).value.is_null() {
                        // Free last computed value; just for safety reasons.
                        xml_schema_free_value((*vctxt).value);
                        (*vctxt).value = null_mut();
                    }
                    // Note that the attribute *use* can be unavailable,
                    // if the attribute was a wild attribute.
                    if (*(*iattr).decl).flags & XML_SCHEMAS_ATTR_FIXED != 0
                        || (!(*iattr).using.is_null()
                            && (*(*iattr).using).flags & XML_SCHEMAS_ATTR_FIXED != 0)
                    {
                        fixed = 1;
                    } else {
                        fixed = 0;
                    }
                    // SPEC (cvc-attribute)
                    // (3) "The item's `normalized value` must be locally `valid`
                    // with respect to that {type definition} as per
                    // String Valid ($3.14.4)."
                    //
                    // VAL TODO: Do we already have the
                    // "normalized attribute value" here?
                    if xpath_res != 0 || fixed != 0 {
                        (*iattr).flags |= XML_SCHEMA_NODE_INFO_VALUE_NEEDED;
                        // Request a computed value.
                        res = xml_schema_vcheck_cvc_simple_type(
                            vctxt as XmlSchemaAbstractCtxtPtr,
                            (*iattr).node.map(|attr| attr.into()),
                            (*iattr).type_def,
                            (*iattr).value,
                            &raw mut (*iattr).val,
                            1,
                            1,
                            0,
                        );
                    } else {
                        res = xml_schema_vcheck_cvc_simple_type(
                            vctxt as XmlSchemaAbstractCtxtPtr,
                            (*iattr).node.map(|attr| attr.into()),
                            (*iattr).type_def,
                            (*iattr).value,
                            null_mut(),
                            1,
                            0,
                            0,
                        );
                    }

                    if res != 0 {
                        if res == -1 {
                            VERROR_INT!(
                                vctxt,
                                "xmlSchemaVAttributesComplex",
                                "calling xmlSchemaStreamValidateSimpleTypeValue()"
                            );
                            break 'internal_error;
                        }
                        (*iattr).state = XML_SCHEMAS_ATTR_INVALID_VALUE;
                        // SPEC PSVI Assessment Outcome (Attribute)
                        // [validity] = "invalid"
                        break 'eval_idcs;
                    }
                    if fixed != 0 {
                        // SPEC Attribute Locally Valid (Use) (cvc-au)
                        // "For an attribute information item to be `valid`
                        // with respect to an attribute use its *normalized*
                        // value must match the *canonical* lexical
                        // representation of the attribute use's {value
                        // constraint}value, if it is present and fixed."
                        //
                        // VAL TODO: The requirement for the *canonical* value
                        // will be removed in XML Schema 1.1.

                        // SPEC Attribute Locally Valid (cvc-attribute)
                        // (4) "The item's *actual* value must match the *value* of
                        // the {value constraint}, if it is present and fixed."
                        if (*iattr).val.is_null() {
                            // VAL TODO: A value was not precomputed.
                            // TODO
                            break 'eval_idcs;
                        }
                        if !(*iattr).using.is_null() && !(*(*iattr).using).def_value.is_null() {
                            if (*(*iattr).using).def_val.is_null() {
                                // VAL TODO: A default value was not precomputed.
                                // TODO
                                break 'eval_idcs;
                            }
                            (*iattr).vc_value = (*(*iattr).using).def_value;
                            // // if (xmlSchemaCompareValuesWhtsp((*attr).val, (xmlSchemaWhitespaceValueType) ws, (*attr).(*use).defVal, (xmlSchemaWhitespaceValueType) ws) != 0) {
                            if xml_schema_are_values_equal((*iattr).val, (*(*iattr).using).def_val)
                                == 0
                            {
                                (*iattr).state = XML_SCHEMAS_ATTR_ERR_FIXED_VALUE;
                            }
                        } else {
                            if (*(*iattr).decl).def_val.is_null() {
                                // VAL TODO: A default value was not precomputed.
                                // TODO
                                break 'eval_idcs;
                            }
                            (*iattr).vc_value = (*(*iattr).decl).def_value;
                            // // if (xmlSchemaCompareValuesWhtsp((*attr).val, (xmlSchemaWhitespaceValueType) ws, (*attrDecl).defVal, (xmlSchemaWhitespaceValueType) ws) != 0) {
                            if xml_schema_are_values_equal((*iattr).val, (*(*iattr).decl).def_val)
                                == 0
                            {
                                (*iattr).state = XML_SCHEMAS_ATTR_ERR_FIXED_VALUE;
                            }
                        }
                    }
                }
                // eval_idcs:
                // Evaluate IDCs.
                if xpath_res != 0 {
                    if xml_schema_xpath_process_history(vctxt, (*vctxt).depth + 1) == -1 {
                        VERROR_INT!(
                            vctxt,
                            "xmlSchemaVAttributesComplex",
                            "calling xmlSchemaXPathEvaluate()"
                        );
                        break 'internal_error;
                    }
                } else if !(*vctxt).xpath_states.is_null() {
                    xml_schema_xpath_pop(vctxt);
                }
            }

            // Report errors.
            for i in 0..(*vctxt).nb_attr_infos {
                iattr = *(*vctxt).attr_infos.add(i as usize);
                if (*iattr).state == XML_SCHEMAS_ATTR_META
                    || (*iattr).state == XML_SCHEMAS_ATTR_ASSESSED
                    || (*iattr).state == XML_SCHEMAS_ATTR_WILD_SKIP
                    || (*iattr).state == XML_SCHEMAS_ATTR_WILD_LAX_NO_DECL
                {
                    continue;
                }
                ACTIVATE_ATTRIBUTE!(vctxt, iattr);
                match (*iattr).state {
                    w if XML_SCHEMAS_ATTR_ERR_MISSING == w => {
                        ACTIVATE_ELEM!(vctxt);
                        let namespace_name = (*(*iattr).decl).target_namespace as *const i8;
                        let qname = xml_schema_format_qname(
                            (!namespace_name.is_null())
                                .then(|| CStr::from_ptr(namespace_name).to_string_lossy())
                                .as_deref(),
                            Some(
                                CStr::from_ptr((*(*iattr).decl).name as *const i8)
                                    .to_string_lossy()
                                    .as_ref(),
                            ),
                        );
                        xml_schema_custom_err(
                            vctxt as XmlSchemaAbstractCtxtPtr,
                            XmlParserErrors::XmlSchemavCvcComplexType4,
                            None,
                            null_mut(),
                            format!("The attribute '{qname}' is required but missing").as_str(),
                            Some(&qname),
                            None,
                        );
                    }
                    w if XML_SCHEMAS_ATTR_ERR_NO_TYPE == w => {
                        VERROR!(
                            vctxt,
                            XmlParserErrors::XmlSchemavCvcAttribute2,
                            null_mut(),
                            "The type definition is absent"
                        );
                    }
                    w if XML_SCHEMAS_ATTR_ERR_FIXED_VALUE == w => {
                        let value = CStr::from_ptr((*iattr).value as *const i8).to_string_lossy();
                        let vc_value =
                            CStr::from_ptr((*iattr).vc_value as *const i8).to_string_lossy();
                        xml_schema_custom_err(
                        vctxt as XmlSchemaAbstractCtxtPtr,
                        XmlParserErrors::XmlSchemavCvcAu,
                        None,
                        null_mut(),
                        format!(
                            "The value '{value}' does not match the fixed value constraint '{vc_value}'"
                        )
                        .as_str(),
                        Some(&value),
                        Some(&vc_value),
                    );
                    }
                    w if XML_SCHEMAS_ATTR_ERR_WILD_STRICT_NO_DECL == w => {
                        VERROR!(
                            vctxt,
                            XmlParserErrors::XmlSchemavCvcWildcard,
                            null_mut(),
                            "No matching global attribute declaration available, but demanded by the strict wildcard"
                        );
                    }
                    w if XML_SCHEMAS_ATTR_UNKNOWN == w => {
                        if (*iattr).meta_type != 0 {
                            // break;
                        } else {
                            // MAYBE VAL TODO: One might report different error messages
                            // for the following errors.
                            if (*typ).attribute_wildcard.is_null() {
                                xml_schema_illegal_attr_err(
                                    vctxt as XmlSchemaAbstractCtxtPtr,
                                    XmlParserErrors::XmlSchemavCvcComplexType3_2_1,
                                    iattr,
                                    None,
                                );
                            } else {
                                xml_schema_illegal_attr_err(
                                    vctxt as XmlSchemaAbstractCtxtPtr,
                                    XmlParserErrors::XmlSchemavCvcComplexType3_2_2,
                                    iattr,
                                    None,
                                );
                            }
                        }
                    }
                    _ => {}
                }
            }

            ACTIVATE_ELEM!(vctxt);
            return 0;
        }
        // internal_error:
        ACTIVATE_ELEM!(vctxt);
        -1
    }
}

unsafe fn xml_schema_vattributes_simple(vctxt: XmlSchemaValidCtxtPtr) -> i32 {
    unsafe {
        let mut iattr: XmlSchemaAttrInfoPtr;
        let mut ret: i32 = 0;

        // SPEC cvc-type (3.1.1)
        // "The attributes of must be empty, excepting those whose namespace
        // name is identical to http://www.w3.org/2001/XMLSchema-instance and
        // whose local name is one of type, nil, schemaLocation or
        // noNamespaceSchemaLocation."
        if (*vctxt).nb_attr_infos == 0 {
            return 0;
        }
        for i in 0..(*vctxt).nb_attr_infos {
            iattr = *(*vctxt).attr_infos.add(i as usize) as _;
            if (*iattr).meta_type == 0 {
                ACTIVATE_ATTRIBUTE!(vctxt, iattr);
                xml_schema_illegal_attr_err(
                    vctxt as XmlSchemaAbstractCtxtPtr,
                    XmlParserErrors::XmlSchemavCvcType3_1_1,
                    iattr,
                    None,
                );
                ret = XmlParserErrors::XmlSchemavCvcType3_1_1 as i32;
            }
        }
        ACTIVATE_ELEM!(vctxt);
        ret
    }
}

unsafe fn xml_schema_validate_elem(vctxt: XmlSchemaValidCtxtPtr) -> i32 {
    unsafe {
        let mut ret: i32 = 0;

        if (*vctxt).skip_depth != -1 && (*vctxt).depth >= (*vctxt).skip_depth {
            VERROR_INT!(vctxt, "xmlSchemaValidateElem", "in skip-state");
            // goto internal_error;
            return -1;
        }
        if (*vctxt).xsi_assemble != 0 {
            // We will stop validation if there was an error during
            // dynamic schema construction.
            // Note that we simply set @skipDepth to 0, this could
            // mean that a streaming document via SAX would be
            // still read to the end but it won't be validated any more.
            // TODO: If we are sure how to stop the validation at once
            //   for all input scenarios, then this should be changed to
            //   instantly stop the validation.
            ret = xml_schema_assemble_by_xsi(vctxt);
            if ret != 0 {
                if ret == -1 {
                    // goto internal_error;
                    return -1;
                }
                (*vctxt).skip_depth = 0;
                return ret;
            }
            // Augment the IDC definitions for the main schema and all imported ones
            // NOTE: main schema is the first in the imported list
            for &imported in (*(*vctxt).schema).schemas_imports.values() {
                if !imported.is_null() {
                    xml_schema_augment_imported_idc(imported, vctxt);
                }
            }
        }
        'goto_exit: {
            if (*vctxt).depth > 0 {
                // Validate this element against the content model of the parent.
                ret = xml_schema_validate_child_elem(vctxt);
                if ret != 0 {
                    if ret < 0 {
                        VERROR_INT!(
                            vctxt,
                            "xmlSchemaValidateElem",
                            "calling xmlSchemaStreamValidateChildElement()"
                        );
                        // goto internal_error;
                        return -1;
                    }
                    break 'goto_exit;
                }
                if (*vctxt).depth == (*vctxt).skip_depth {
                    break 'goto_exit;
                }
                if (*(*vctxt).inode).decl.is_null() && (*(*vctxt).inode).type_def.is_null() {
                    VERROR_INT!(
                        vctxt,
                        "xmlSchemaValidateElem",
                        "the child element was valid but neither the declaration nor the type was set"
                    );
                    // goto internal_error;
                    return -1;
                }
            } else {
                // Get the declaration of the validation root.
                (*(*vctxt).inode).decl = (*(*vctxt).schema).get_elem(
                    CStr::from_ptr((*(*vctxt).inode).local_name as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    (!(*(*vctxt).inode).ns_name.is_null())
                        .then(|| {
                            CStr::from_ptr((*(*vctxt).inode).ns_name as *const i8).to_string_lossy()
                        })
                        .as_deref(),
                );
                if (*(*vctxt).inode).decl.is_null() {
                    ret = XmlParserErrors::XmlSchemavCvcElt1 as i32;
                    VERROR!(
                        vctxt,
                        ret.try_into().unwrap(),
                        null_mut(),
                        "No matching global declaration available for the validation root"
                    );
                    break 'goto_exit;
                }
            }

            'type_validation: {
                if (*(*vctxt).inode).decl.is_null() {
                    break 'type_validation;
                }
                if (*(*(*vctxt).inode).decl).typ == XmlSchemaTypeType::XmlSchemaTypeAny {
                    let mut skip: i32 = 0;
                    // Wildcards.
                    ret = xml_schema_validate_elem_wildcard(vctxt, &raw mut skip);
                    if ret != 0 {
                        if ret < 0 {
                            VERROR_INT!(
                                vctxt,
                                "xmlSchemaValidateElem",
                                "calling xmlSchemaValidateElemWildcard()"
                            );
                            // goto internal_error;
                            return -1;
                        }
                        break 'goto_exit;
                    }
                    if skip != 0 {
                        (*vctxt).skip_depth = (*vctxt).depth;
                        break 'goto_exit;
                    }
                    // The declaration might be set by the wildcard validation,
                    // when the processContents is "lax" or "strict".
                    if (*(*(*vctxt).inode).decl).typ != XmlSchemaTypeType::XmlSchemaTypeElement {
                        // Clear the "decl" field to not confuse further processing.
                        (*(*vctxt).inode).decl = null_mut();
                        break 'type_validation;
                    }
                }

                // Validate against the declaration.
                ret = xml_schema_validate_elem_decl(vctxt);
                if ret != 0 {
                    if ret < 0 {
                        VERROR_INT!(
                            vctxt,
                            "xmlSchemaValidateElem",
                            "calling xmlSchemaValidateElemDecl()"
                        );
                        // goto internal_error;
                        return -1;
                    }
                    break 'goto_exit;
                }
            }

            // Validate against the type definition.
            // type_validation:

            if (*(*vctxt).inode).type_def.is_null() {
                (*(*vctxt).inode).flags |= XML_SCHEMA_NODE_INFO_ERR_BAD_TYPE;
                ret = XmlParserErrors::XmlSchemavCvcType1 as i32;
                VERROR!(
                    vctxt,
                    ret.try_into().unwrap(),
                    null_mut(),
                    "The type definition is absent"
                );
                break 'goto_exit;
            }
            if (*(*(*vctxt).inode).type_def).flags & XML_SCHEMAS_TYPE_ABSTRACT != 0 {
                (*(*vctxt).inode).flags |= XML_SCHEMA_NODE_INFO_ERR_BAD_TYPE;
                ret = XmlParserErrors::XmlSchemavCvcType2 as i32;
                VERROR!(
                    vctxt,
                    ret.try_into().unwrap(),
                    null_mut(),
                    "The type definition is abstract"
                );
                break 'goto_exit;
            }
            // Evaluate IDCs. Do it here, since new IDC matchers are registered
            // during validation against the declaration. This must be done
            // _before_ attribute validation.
            if !(*vctxt).xpath_states.is_null() {
                ret = xml_schema_xpath_evaluate(vctxt, XmlElementType::XmlElementNode);
                (*(*vctxt).inode).applied_xpath = 1;
                if ret == -1 {
                    VERROR_INT!(
                        vctxt,
                        "xmlSchemaValidateElem",
                        "calling xmlSchemaXPathEvaluate()"
                    );
                    // goto internal_error;
                    return -1;
                }
            }
            // Validate attributes.
            if wxs_is_complex((*(*vctxt).inode).type_def) {
                if (*vctxt).nb_attr_infos != 0 || !(*(*(*vctxt).inode).type_def).attr_uses.is_null()
                {
                    ret = xml_schema_vattributes_complex(vctxt);
                }
            } else if (*vctxt).nb_attr_infos != 0 {
                ret = xml_schema_vattributes_simple(vctxt);
            }
            // Clear registered attributes.
            if (*vctxt).nb_attr_infos != 0 {
                xml_schema_clear_attr_infos(vctxt);
            }
            if ret == -1 {
                VERROR_INT!(
                    vctxt,
                    "xmlSchemaValidateElem",
                    "calling attributes validation"
                );
                // goto internal_error;
                return -1;
            }
            // Don't return an error if attributes are invalid on purpose.
            ret = 0;
        }

        // exit:
        if ret != 0 {
            (*vctxt).skip_depth = (*vctxt).depth;
        }
        ret
        // internal_error:
        //     return -1;
    }
}

const XML_SCHEMA_PUSH_TEXT_PERSIST: i32 = 1;
const XML_SCHEMA_PUSH_TEXT_CREATED: i32 = 2;
const XML_SCHEMA_PUSH_TEXT_VOLATILE: i32 = 3;

unsafe fn xml_schema_vpush_text(
    vctxt: XmlSchemaValidCtxtPtr,
    node_type: i32,
    value: *const XmlChar,
    mut len: i32,
    mode: i32,
    consumed: *mut i32,
) -> i32 {
    unsafe {
        // Unfortunately we have to duplicate the text sometimes.
        // OPTIMIZE: Maybe we could skip it, if:
        //   1. content type is simple
        //   2. whitespace is "collapse"
        //   3. it consists of whitespace only
        //
        // Process character content.
        if !consumed.is_null() {
            *consumed = 0;
        }
        if INODE_NILLED!((*vctxt).inode) {
            // SPEC cvc-elt (3.3.4 - 3.2.1)
            // "The element information item must have no character or
            // element information item [children]."
            VERROR!(
                vctxt,
                XmlParserErrors::XmlSchemavCvcElt3_2_1,
                null_mut(),
                "Neither character nor element content is allowed because the element is 'nilled'"
            );
            return (*vctxt).err;
        }
        // SPEC (2.1) "If the {content type} is empty, then the
        // element information item has no character or element
        // information item [children]."
        if (*(*(*vctxt).inode).type_def).content_type == XmlSchemaContentType::XmlSchemaContentEmpty
        {
            VERROR!(
                vctxt,
                XmlParserErrors::XmlSchemavCvcComplexType2_1,
                null_mut(),
                "Character content is not allowed, because the content type is empty"
            );
            return (*vctxt).err;
        }

        if (*(*(*vctxt).inode).type_def).content_type
            == XmlSchemaContentType::XmlSchemaContentElements
        {
            if node_type != XmlElementType::XmlTextNode as i32
                || xml_schema_is_blank(value as _, len) == 0
            {
                // SPEC cvc-complex-type (2.3)
                // "If the {content type} is element-only, then the
                // element information item has no character information
                // item [children] other than those whose [character
                // code] is defined as a white space in [XML 1.0 (Second Edition)]."
                VERROR!(
                    vctxt,
                    XmlParserErrors::XmlSchemavCvcComplexType2_3,
                    null_mut(),
                    "Character content other than whitespace is not allowed because the content type is 'element-only'"
                );
                return (*vctxt).err;
            }
            return 0;
        }

        if value.is_null() || *value.add(0) == 0 {
            return 0;
        }
        // Save the value.
        // NOTE that even if the content type is *mixed*, we need the
        // *initial value* for default/fixed value constraints.
        if (*(*(*vctxt).inode).type_def).content_type == XmlSchemaContentType::XmlSchemaContentMixed
            && ((*(*vctxt).inode).decl.is_null() || (*(*(*vctxt).inode).decl).value.is_null())
        {
            return 0;
        }

        if (*(*vctxt).inode).value.is_null() {
            // Set the value.
            match mode {
                _ if mode == XML_SCHEMA_PUSH_TEXT_PERSIST => {
                    // When working on a tree.
                    (*(*vctxt).inode).value = value;
                }
                _ if mode == XML_SCHEMA_PUSH_TEXT_CREATED => {
                    // When working with the reader.
                    // The value will be freed by the element info.
                    (*(*vctxt).inode).value = value;
                    if !consumed.is_null() {
                        *consumed = 1;
                    }
                    (*(*vctxt).inode).flags |= XML_SCHEMA_NODE_INFO_FLAG_OWNED_VALUES;
                }
                _ if mode == XML_SCHEMA_PUSH_TEXT_VOLATILE => {
                    // When working with SAX.
                    // The value will be freed by the element info.
                    if len != -1 {
                        (*(*vctxt).inode).value = xml_strndup(value, len);
                    } else {
                        (*(*vctxt).inode).value = xml_strdup(value);
                    }
                    (*(*vctxt).inode).flags |= XML_SCHEMA_NODE_INFO_FLAG_OWNED_VALUES;
                }
                _ => {}
            }
        } else {
            if len < 0 {
                len = xml_strlen(value);
            }
            // Concat the value.
            if (*(*vctxt).inode).flags & XML_SCHEMA_NODE_INFO_FLAG_OWNED_VALUES != 0 {
                (*(*vctxt).inode).value = xml_strncat((*(*vctxt).inode).value as _, value, len);
            } else {
                (*(*vctxt).inode).value = xml_strncat_new((*(*vctxt).inode).value, value, len);
                (*(*vctxt).inode).flags |= XML_SCHEMA_NODE_INFO_FLAG_OWNED_VALUES;
            }
        }

        0
    }
}

unsafe fn xml_schema_vcheck_inode_data_type(
    vctxt: XmlSchemaValidCtxtPtr,
    inode: XmlSchemaNodeInfoPtr,
    typ: XmlSchemaTypePtr,
    value: *const XmlChar,
) -> i32 {
    unsafe {
        if (*inode).flags & XML_SCHEMA_NODE_INFO_VALUE_NEEDED != 0 {
            xml_schema_vcheck_cvc_simple_type(
                vctxt as XmlSchemaAbstractCtxtPtr,
                None,
                typ,
                value,
                &raw mut (*inode).val,
                1,
                1,
                0,
            )
        } else {
            xml_schema_vcheck_cvc_simple_type(
                vctxt as XmlSchemaAbstractCtxtPtr,
                None,
                typ,
                value,
                null_mut(),
                1,
                0,
                0,
            )
        }
    }
}

// This will be called if: not nilled, no content and a default/fixed value is provided.
#[doc(alias = "xmlSchemaCheckCOSValidDefault")]
unsafe fn xml_schema_check_cos_valid_default(
    vctxt: XmlSchemaValidCtxtPtr,
    value: *const XmlChar,
    val: *mut XmlSchemaValPtr,
) -> i32 {
    unsafe {
        let mut ret: i32 = 0;
        let inode: XmlSchemaNodeInfoPtr = (*vctxt).inode;

        // cos-valid-default:
        // Schema Component Constraint: Element Default Valid (Immediate)
        // For a string to be a valid default with respect to a type
        // definition the appropriate case among the following must be true:
        if wxs_is_complex((*inode).type_def) {
            // Complex type.
            //
            // SPEC (2.1) "its {content type} must be a simple type definition
            // or mixed."
            // SPEC (2.2.2) "If the {content type} is mixed, then the {content
            // type}'s particle must be `emptiable` as defined by
            // Particle Emptiable ($3.9.6)."
            if !WXS_HAS_SIMPLE_CONTENT!((*inode).type_def)
                && (!WXS_HAS_MIXED_CONTENT!((*inode).type_def)
                    || !WXS_EMPTIABLE!((*inode).type_def))
            {
                ret = XmlParserErrors::XmlSchemapCosValidDefault2_1 as i32;
                // NOTE that this covers (2.2.2) as well.
                VERROR!(
                    vctxt,
                    ret.try_into().unwrap(),
                    null_mut(),
                    "For a string to be a valid default, the type definition must be a simple type or a complex type with simple content or mixed content and a particle emptiable"
                );
                return ret;
            }
        }
        // 1 If the type definition is a simple type definition, then the string
        // must be `valid` with respect to that definition as defined by String
        // Valid ($3.14.4).
        //
        // AND
        //
        // 2.2.1 If the {content type} is a simple type definition, then the
        // string must be `valid` with respect to that simple type definition
        // as defined by String Valid ($3.14.4).
        if wxs_is_simple((*inode).type_def) {
            ret = xml_schema_vcheck_cvc_simple_type(
                vctxt as XmlSchemaAbstractCtxtPtr,
                None,
                (*inode).type_def,
                value,
                val,
                1,
                1,
                0,
            );
        } else if WXS_HAS_SIMPLE_CONTENT!((*inode).type_def) {
            ret = xml_schema_vcheck_cvc_simple_type(
                vctxt as XmlSchemaAbstractCtxtPtr,
                None,
                (*(*inode).type_def).content_type_def,
                value,
                val,
                1,
                1,
                0,
            );
        }
        if ret < 0 {
            VERROR_INT!(
                vctxt,
                "xmlSchemaCheckCOSValidDefault",
                "calling xmlSchemaVCheckCVCSimpleType()"
            );
        }
        ret
    }
}

/// Creates a new IDC binding.
///
/// Returns the new IDC binding, NULL on internal errors.
#[doc(alias = "xmlSchemaIDCNewBinding")]
unsafe fn xml_schema_idc_new_binding(idc_def: XmlSchemaIDCPtr) -> XmlSchemaPSVIIDCBindingPtr {
    unsafe {
        let ret: XmlSchemaPSVIIDCBindingPtr =
            xml_malloc(size_of::<XmlSchemaPSVIIDCBinding>()) as XmlSchemaPSVIIDCBindingPtr;
        if ret.is_null() {
            xml_schema_verr_memory(null_mut(), "allocating a PSVI IDC binding item", None);
            return null_mut();
        }
        std::ptr::write(&mut *ret, Default::default());
        (*ret).definition = idc_def;
        ret
    }
}

/// Looks up an PSVI IDC binding, for the IDC definition and
/// of the given matcher. If none found, a new one is created
/// and added to the IDC table.
///
/// Returns an IDC binding or NULL on internal errors.
#[doc(alias = "xmlSchemaIDCAcquireBinding")]
unsafe fn xml_schema_idc_acquire_binding(
    vctxt: XmlSchemaValidCtxtPtr,
    matcher: XmlSchemaIDCMatcherPtr,
) -> XmlSchemaPSVIIDCBindingPtr {
    unsafe {
        let ielem: XmlSchemaNodeInfoPtr = *(*vctxt).elem_infos.add((*matcher).depth as usize);

        if (*ielem).idc_table.is_null() {
            (*ielem).idc_table = xml_schema_idc_new_binding((*(*matcher).aidc).def);
            if (*ielem).idc_table.is_null() {
                return null_mut();
            }
            return (*ielem).idc_table;
        } else {
            let mut bind: XmlSchemaPSVIIDCBindingPtr;

            bind = (*ielem).idc_table;
            while {
                if (*bind).definition == (*(*matcher).aidc).def {
                    return bind;
                }
                if (*bind).next.is_null() {
                    (*bind).next = xml_schema_idc_new_binding((*(*matcher).aidc).def);
                    if (*bind).next.is_null() {
                        return null_mut();
                    }
                    return (*bind).next;
                }
                bind = (*bind).next;
                !bind.is_null()
            } {}
        }
        null_mut()
    }
}

/// Appends the IDC node-table item to the binding.
///
/// Returns 0 on success and -1 on internal errors.
#[doc(alias = "xmlSchemaIDCAppendNodeTableItem")]
unsafe fn xml_schema_idc_append_node_table_item(
    bind: XmlSchemaPSVIIDCBindingPtr,
    nt_item: XmlSchemaPSVIIDCNodePtr,
) -> i32 {
    unsafe {
        (*bind).node_table.push(nt_item);
        0
    }
}

unsafe fn xml_schema_idc_fill_node_tables(
    vctxt: XmlSchemaValidCtxtPtr,
    ielem: XmlSchemaNodeInfoPtr,
) -> i32 {
    unsafe {
        let mut bind: XmlSchemaPSVIIDCBindingPtr;
        let mut res: i32;
        let mut keys: *mut XmlSchemaPSVIIDCKeyPtr;
        let mut ntkeys: *mut XmlSchemaPSVIIDCKeyPtr;

        let mut matcher: XmlSchemaIDCMatcherPtr = (*ielem).idc_matchers;
        // (*vctxt).createIDCNodeTables
        while !matcher.is_null() {
            // Skip keyref IDCs and empty IDC target-lists.
            if (*(*(*matcher).aidc).def).typ == XmlSchemaTypeType::XmlSchemaTypeIDCKeyref
                || WXS_ILIST_IS_EMPTY!((*matcher).targets)
            {
                matcher = (*matcher).next;
                continue;
            }
            // If we _want_ the IDC node-table to be created in any case then do so.
            // Otherwise create them only if keyrefs need them.
            if (*vctxt).create_idcnode_tables == 0
                && ((*(*matcher).aidc).keyref_depth == -1
                    || (*(*matcher).aidc).keyref_depth > (*vctxt).depth)
            {
                matcher = (*matcher).next;
                continue;
            }
            // Get/create the IDC binding on this element for the IDC definition.
            bind = xml_schema_idc_acquire_binding(vctxt, matcher);
            if bind.is_null() {
                // goto internal_error;
                return -1;
            }

            let (dupls, nb_dupls) = if !WXS_ILIST_IS_EMPTY!((*bind).dupls) {
                (&(*(*bind).dupls).items[..], (*(*bind).dupls).items.len()) // as *mut XmlSchemaPSVIIDCNodePtr;
            } else {
                (&[][..], 0)
            };
            let nb_node_table = (*bind).node_table.len();

            if nb_node_table == 0 && nb_dupls == 0 {
                // Transfer all IDC target-nodes to the IDC node-table.
                (*bind).node_table = (*(*matcher).targets)
                    .items
                    .drain(..)
                    .map(|item| item as XmlSchemaPSVIIDCNodePtr)
                    .collect();
                // (*bind).size_nodes = (*(*matcher).targets).size_items;
                // (*bind).nb_nodes = (*(*matcher).targets).nb_items;

                // (*(*matcher).targets).items = null_mut();
                // (*(*matcher).targets).size_items = 0;
                // (*(*matcher).targets).nb_items = 0;
                if !(*matcher).htab.is_null() {
                    xml_hash_free((*matcher).htab, Some(xml_free_idc_hash_entry));
                    (*matcher).htab = null_mut();
                }
            } else {
                // Compare the key-sequences and add to the IDC node-table.
                let nb_targets = (*(*matcher).targets).items.len();
                let targets = &(*(*matcher).targets).items[..]; // as *mut XmlSchemaPSVIIDCNodePtr;
                let nb_fields = (*(*(*matcher).aidc).def).nb_fields;
                let mut i = 0;
                while i < nb_targets {
                    'next_target: {
                        keys = (*(targets[i] as XmlSchemaPSVIIDCNodePtr)).keys;
                        if nb_dupls != 0 {
                            // Search in already found duplicates first.
                            let mut j = 0;
                            'lj: while {
                                if nb_fields == 1 {
                                    res = xml_schema_are_values_equal(
                                        (*(*keys.add(0))).val,
                                        (*(*(*(dupls[j] as XmlSchemaPSVIIDCNodePtr)).keys.add(0)))
                                            .val,
                                    );
                                    if res == -1 {
                                        // goto internal_error;
                                        return -1;
                                    }
                                    if res == 1 {
                                        // Equal key-sequence.
                                        break 'next_target;
                                    }
                                } else {
                                    res = 0;
                                    ntkeys = (*(dupls[j] as XmlSchemaPSVIIDCNodePtr)).keys;
                                    for k in 0..nb_fields {
                                        res = xml_schema_are_values_equal(
                                            (*(*keys.add(k as usize))).val,
                                            (*(*ntkeys.add(k as usize))).val,
                                        );
                                        if res == -1 {
                                            // goto internal_error;
                                            return -1;
                                        }
                                        if res == 0 {
                                            // One of the keys differs.
                                            break 'lj;
                                        }
                                    }
                                    if res == 1 {
                                        // Equal key-sequence found.
                                        break 'next_target;
                                    }
                                }
                                j += 1;
                                j < nb_dupls
                            } {}
                        }
                        if nb_node_table != 0 {
                            let mut j = 0;
                            while {
                                'next_node_table_entry: {
                                    if nb_fields == 1 {
                                        res = xml_schema_are_values_equal(
                                            (*(*keys.add(0))).val,
                                            (*(*(*(*bind).node_table[j]).keys.add(0))).val,
                                        );
                                        if res == -1 {
                                            // goto internal_error;
                                            return -1;
                                        }
                                        if res == 0 {
                                            // The key-sequence differs.
                                            break 'next_node_table_entry;
                                        }
                                    } else {
                                        // res = 0;
                                        ntkeys = (*(*bind).node_table[j]).keys;
                                        for k in 0..nb_fields {
                                            res = xml_schema_are_values_equal(
                                                (*(*keys.add(k as usize))).val,
                                                (*(*ntkeys.add(k as usize))).val,
                                            );
                                            if res == -1 {
                                                // goto internal_error;
                                                return -1;
                                            }
                                            if res == 0 {
                                                // One of the keys differs.
                                                break 'next_node_table_entry;
                                            }
                                        }
                                    }
                                    // Add the duplicate to the list of duplicates.
                                    if (*bind).dupls.is_null() {
                                        (*bind).dupls =
                                            xml_schema_item_list_create::<*mut c_void>();
                                        if (*bind).dupls.is_null() {
                                            // goto internal_error;
                                            return -1;
                                        }
                                    }
                                    if (*(*bind).dupls).push((*bind).node_table[j] as _) == -1 {
                                        // goto internal_error;
                                        return -1;
                                    }
                                    // Remove the duplicate entry from the IDC node-table.
                                    (*bind).node_table[j] = (*bind).node_table.pop().unwrap();

                                    break 'next_target;
                                }
                                j += 1;
                                j < nb_node_table
                            } {}
                        }
                        // If everything is fine, then add the IDC target-node to the IDC node-table.
                        if xml_schema_idc_append_node_table_item(
                            bind,
                            targets[i] as XmlSchemaPSVIIDCNodePtr,
                        ) == -1
                        {
                            // goto internal_error;
                            return -1;
                        }
                    }
                    i += 1;
                }
            }
            matcher = (*matcher).next;
        }
        0

        // internal_error:
        //     return -1;
    }
}

/// Check the cvc-idc-keyref constraints.
#[doc(alias = "xmlSchemaCheckCVCIDCKeyRef")]
unsafe fn xml_schema_check_cvc_idc_key_ref(vctxt: XmlSchemaValidCtxtPtr) -> i32 {
    unsafe {
        let mut matcher: XmlSchemaIDCMatcherPtr;
        let mut bind: XmlSchemaPSVIIDCBindingPtr;

        matcher = (*(*vctxt).inode).idc_matchers;
        // Find a keyref.
        while !matcher.is_null() {
            if (*matcher).idc_type == XmlSchemaTypeType::XmlSchemaTypeIDCKeyref as i32
                && !(*matcher).targets.is_null()
                && !(*(*matcher).targets).items.is_empty()
            {
                let mut res: i32;
                let mut ref_keys: *mut XmlSchemaPSVIIDCKeyPtr;
                let mut keys: *mut XmlSchemaPSVIIDCKeyPtr;
                let mut ref_node: XmlSchemaPSVIIDCNodePtr;
                let mut table: XmlHashTablePtr = null_mut();

                let nb_fields: i32 = (*(*(*matcher).aidc).def).nb_fields;

                // Find the IDC node-table for the referenced IDC key/unique.
                bind = (*(*vctxt).inode).idc_table;
                while !bind.is_null() {
                    if (*(*(*(*matcher).aidc).def).refe).item as XmlSchemaIDCPtr
                        == (*bind).definition
                    {
                        break;
                    }
                    bind = (*bind).next;
                }
                let has_dupls: i32 = (!bind.is_null()
                    && !(*bind).dupls.is_null()
                    && !(*(*bind).dupls).items.is_empty())
                    as i32;
                // Search for a matching key-sequences.
                if !bind.is_null() {
                    table = xml_hash_create((*bind).node_table.len() as i32 * 2);
                    for j in 0..(*bind).node_table.len() {
                        keys = (*(*bind).node_table[j]).keys;
                        let value =
                            CString::new(xml_schema_hash_key_sequence(vctxt, keys, nb_fields))
                                .unwrap();
                        let e: XmlIDCHashEntryPtr = xml_malloc(size_of::<XmlIDCHashEntry>()) as _;
                        (*e).index = j as i32;
                        let r: XmlIDCHashEntryPtr =
                            xml_hash_lookup(table, value.as_ptr() as *const u8) as _;
                        if !r.is_null() {
                            (*e).next = (*r).next;
                            (*r).next = e;
                        } else {
                            (*e).next = null_mut();
                            xml_hash_add_entry(table, value.as_ptr() as *const u8, e as _);
                        }
                    }
                }
                for i in 0..(*(*matcher).targets).items.len() {
                    res = 0;
                    ref_node = (*(*matcher).targets).items[i] as _;
                    if !bind.is_null() {
                        let mut e: XmlIDCHashEntryPtr;
                        ref_keys = (*ref_node).keys;
                        let value =
                            CString::new(xml_schema_hash_key_sequence(vctxt, ref_keys, nb_fields))
                                .unwrap();
                        e = xml_hash_lookup(table, value.as_ptr() as *const u8) as _;
                        res = 0;
                        while !e.is_null() {
                            keys = (*(*bind).node_table[(*e).index as usize]).keys;
                            for k in 0..nb_fields {
                                res = xml_schema_are_values_equal(
                                    (*(*keys.add(k as usize))).val,
                                    (*(*ref_keys.add(k as usize))).val,
                                );
                                if res == 0 {
                                    break;
                                } else if res == -1 {
                                    return -1;
                                }
                            }
                            if res == 1 {
                                // Match found.
                                break;
                            }
                            e = (*e).next;
                        }
                        if res == 0 && has_dupls != 0 {
                            // Search in duplicates
                            for j in 0..(*(*bind).dupls).items.len() {
                                keys = (*(((*(*bind).dupls).items[j]) as XmlSchemaPSVIIDCNodePtr))
                                    .keys;
                                for k in 0..nb_fields {
                                    res = xml_schema_are_values_equal(
                                        (*(*keys.add(k as usize))).val,
                                        (*(*ref_keys.add(k as usize))).val,
                                    );
                                    if res == 0 {
                                        break;
                                    } else if res == -1 {
                                        return -1;
                                    }
                                }
                                if res == 1 {
                                    // Match in duplicates found.
                                    let seq = xml_schema_format_idc_key_sequence(
                                        vctxt,
                                        (*ref_node).keys,
                                        nb_fields,
                                    );
                                    let qname =
                                        xml_schema_get_component_qname((*(*matcher).aidc).def as _);

                                    xml_schema_keyref_err(
                                        vctxt,
                                        XmlParserErrors::XmlSchemavCvcIdc,
                                        ref_node,
                                        (*(*matcher).aidc).def as XmlSchemaTypePtr,
                                        format!("More than one match found for key-sequence {seq} of keyref '{qname}'").as_str(),
                                        Some(&seq),
                                        Some(&qname),
                                    );
                                    break;
                                }
                            }
                        }
                    }

                    if res == 0 {
                        let seq =
                            xml_schema_format_idc_key_sequence(vctxt, (*ref_node).keys, nb_fields);
                        let qname = xml_schema_get_component_qname((*(*matcher).aidc).def as _);

                        xml_schema_keyref_err(
                            vctxt,
                            XmlParserErrors::XmlSchemavCvcIdc,
                            ref_node,
                            (*(*matcher).aidc).def as XmlSchemaTypePtr,
                            format!("No match found for key-sequence {seq} of keyref '{qname}'")
                                .as_str(),
                            Some(&seq),
                            Some(&qname),
                        );
                    }
                }
                if !table.is_null() {
                    xml_hash_free(table, Some(xml_free_idc_hash_entry));
                }
            }
            matcher = (*matcher).next;
        }
        /* TODO: Return an error if any error encountered. */
        0
    }
}

/// Merges IDC bindings of an element at @depth into the corresponding IDC
/// bindings of its parent element. If a duplicate note-table entry is found,
/// both, the parent node-table entry and child entry are discarded from the
/// node-table of the parent.
///
/// Returns 0 if OK and -1 on internal errors.
#[doc(alias = "xmlSchemaBubbleIDCNodeTables")]
unsafe fn xml_schema_bubble_idc_node_tables(vctxt: XmlSchemaValidCtxtPtr) -> i32 {
    unsafe {
        let mut bind: XmlSchemaPSVIIDCBindingPtr; /* IDC bindings of the current node. */
        let mut par_bind: XmlSchemaPSVIIDCBindingPtr = null_mut(); /* parent IDC bindings. */
        let mut node: XmlSchemaPSVIIDCNodePtr;
        let mut par_node: XmlSchemaPSVIIDCNodePtr = null_mut();
        let mut aidc: XmlSchemaIDCAugPtr;
        let mut ret: i32 = 0;

        bind = (*(*vctxt).inode).idc_table;
        if bind.is_null() {
            // Fine, no table, no bubbles.
            return 0;
        }

        let par_table: *mut XmlSchemaPSVIIDCBindingPtr =
            &raw mut (*(*(*vctxt).elem_infos.add((*vctxt).depth as usize - 1))).idc_table;
        // Walk all bindings; create new or add to existing bindings.
        // Remove duplicate key-sequences.
        while !bind.is_null() {
            'next_binding: {
                if (*bind).node_table.is_empty() && WXS_ILIST_IS_EMPTY!((*bind).dupls) {
                    break 'next_binding;
                }
                // Check if the key/unique IDC table needs to be bubbled.
                if (*vctxt).create_idcnode_tables == 0 {
                    aidc = (*vctxt).aidcs;
                    loop {
                        if (*aidc).def == (*bind).definition {
                            if (*aidc).keyref_depth == -1 || (*aidc).keyref_depth >= (*vctxt).depth
                            {
                                break 'next_binding;
                            }
                            break;
                        }
                        aidc = (*aidc).next;

                        if aidc.is_null() {
                            break;
                        }
                    }
                }

                if !par_table.is_null() {
                    par_bind = *par_table;
                }
                // Search a matching parent binding for the IDC definition.
                while !par_bind.is_null() {
                    if (*par_bind).definition == (*bind).definition {
                        break;
                    }
                    par_bind = (*par_bind).next;
                }

                if !par_bind.is_null() {
                    // Compare every node-table entry of the child node,
                    // i.e. the key-sequence within, ...
                    let mut old_num = (*par_bind).node_table.len(); /* Skip newly added items. */

                    let (old_dupls, dupls) = if !WXS_ILIST_IS_EMPTY!((*par_bind).dupls) {
                        (
                            (*(*par_bind).dupls).items.len(),
                            &(*(*par_bind).dupls).items[..],
                        ) // as *mut XmlSchemaPSVIIDCNodePtr;
                    } else {
                        (0, &[][..])
                    };

                    let mut par_nodes = &mut (*par_bind).node_table;
                    let nb_fields = (*(*bind).definition).nb_fields;

                    for i in 0..(*bind).node_table.len() {
                        node = (*bind).node_table[i];
                        if node.is_null() {
                            continue;
                        }
                        // ...with every key-sequence of the parent node, already
                        // evaluated to be a duplicate key-sequence.
                        if old_dupls != 0 {
                            let mut j = 0;
                            while j < old_dupls {
                                if nb_fields == 1 {
                                    ret = xml_schema_are_values_equal(
                                        (*(*(*node).keys.add(0))).val,
                                        (*(*(*(dupls[j] as XmlSchemaPSVIIDCNodePtr)).keys.add(0)))
                                            .val,
                                    );
                                    if ret == -1 {
                                        // goto internal_error;
                                        return -1;
                                    }
                                    if ret == 0 {
                                        j += 1;
                                        continue;
                                    }
                                } else {
                                    par_node = dupls[j] as XmlSchemaPSVIIDCNodePtr;
                                    for k in 0..nb_fields {
                                        ret = xml_schema_are_values_equal(
                                            (*(*(*node).keys.add(k as usize))).val,
                                            (*(*(*par_node).keys.add(k as usize))).val,
                                        );
                                        if ret == -1 {
                                            // goto internal_error;
                                            return -1;
                                        }
                                        if ret == 0 {
                                            break;
                                        }
                                    }
                                }
                                if ret == 1 {
                                    // Duplicate found.
                                    break;
                                }
                                j += 1;
                            }
                            if j != old_dupls {
                                // Duplicate found. Skip this entry.
                                continue;
                            }
                        }
                        // ... and with every key-sequence of the parent node.
                        if old_num != 0 {
                            let mut j = 0;
                            while j < old_num {
                                par_node = par_nodes[j];
                                if nb_fields == 1 {
                                    ret = xml_schema_are_values_equal(
                                        (*(*(*node).keys.add(0))).val,
                                        (*(*(*par_node).keys.add(0))).val,
                                    );
                                    if ret == -1 {
                                        // goto internal_error;
                                        return -1;
                                    }
                                    if ret == 0 {
                                        j += 1;
                                        continue;
                                    }
                                } else {
                                    for k in 0..nb_fields {
                                        ret = xml_schema_are_values_equal(
                                            (*(*(*node).keys.add(k as usize))).val,
                                            (*(*(*par_node).keys.add(k as usize))).val,
                                        );
                                        if ret == -1 {
                                            // goto internal_error;
                                            return -1;
                                        }
                                        if ret == 0 {
                                            break;
                                        }
                                    }
                                }
                                if ret == 1 {
                                    // Duplicate found.
                                    break;
                                }
                                j += 1;
                            }
                            if j != old_num {
                                // Handle duplicates. Move the duplicate in
                                // the parent's node-table to the list of duplicates.
                                old_num -= 1;
                                par_nodes.swap_remove(j);
                                if (*par_bind).dupls.is_null() {
                                    (*par_bind).dupls =
                                        xml_schema_item_list_create::<*mut c_void>();
                                    if (*par_bind).dupls.is_null() {
                                        // goto internal_error;
                                        return -1;
                                    }
                                }
                                (*(*par_bind).dupls).push(par_node as _);
                            } else {
                                // Add the node-table entry (node and key-sequence) of
                                // the child node to the node table of the parent node.
                                par_nodes = &mut (*par_bind).node_table;
                                // Append the new node-table entry to the 'new node-table entries' section.
                                par_nodes.push(node);
                            }
                        }
                    }
                } else {
                    // No binding for the IDC was found: create a new one and copy all node-tables.
                    par_bind = xml_schema_idc_new_binding((*bind).definition);
                    if par_bind.is_null() {
                        // goto internal_error;
                        return -1;
                    }

                    // TODO: Hmm, how to optimize the initial number of allocated entries?
                    if !(*bind).node_table.is_empty() {
                        // Add all IDC node-table entries.
                        if (*vctxt).psvi_expose_idcnode_tables == 0 {
                            // Just move the entries.
                            // NOTE: this is quite save here, since
                            // all the keyref lookups have already been
                            // performed.
                            (*par_bind).node_table = take(&mut (*bind).node_table);
                        } else {
                            // Copy the entries.
                            (*par_bind).node_table = (*bind).node_table.clone();
                        }
                    }
                    if !(*bind).dupls.is_null() {
                        // Move the duplicates.
                        if !(*par_bind).dupls.is_null() {
                            xml_schema_item_list_free((*par_bind).dupls);
                        }
                        (*par_bind).dupls = (*bind).dupls;
                        (*bind).dupls = null_mut();
                    }
                    if !par_table.is_null() {
                        if (*par_table).is_null() {
                            *par_table = par_bind;
                        } else {
                            (*par_bind).next = *par_table;
                            *par_table = par_bind;
                        }
                    }
                }
            }

            // next_binding:
            bind = (*bind).next;
        }
        0

        // internal_error:
        //     return -1;
    }
}

// Process END of element.
unsafe fn xml_schema_validator_pop_elem(vctxt: XmlSchemaValidCtxtPtr) -> i32 {
    unsafe {
        let mut ret: i32 = 0;
        let inode: XmlSchemaNodeInfoPtr = (*vctxt).inode;

        if (*vctxt).nb_attr_infos != 0 {
            xml_schema_clear_attr_infos(vctxt);
        }

        'internal_error: {
            'end_elem: {
                if (*inode).flags & XML_SCHEMA_NODE_INFO_ERR_NOT_EXPECTED != 0 {
                    // This element was not expected;
                    // we will not validate child elements of broken parents.
                    // Skip validation of all content of the parent.
                    (*vctxt).skip_depth = (*vctxt).depth - 1;
                    break 'end_elem;
                }
                if (*inode).type_def.is_null()
                    || (*inode).flags & XML_SCHEMA_NODE_INFO_ERR_BAD_TYPE != 0
                {
                    // 1. the type definition might be missing if the element was
                    //    error prone
                    // 2. it might be abstract.
                    break 'end_elem;
                }
                // Check the content model.
                'character_content: {
                    'skip_nilled: {
                        if matches!(
                            (*(*inode).type_def).content_type,
                            XmlSchemaContentType::XmlSchemaContentMixed
                                | XmlSchemaContentType::XmlSchemaContentElements
                        ) {
                            // Workaround for "anyType".
                            if (*(*inode).type_def).built_in_type
                                == XmlSchemaValType::XmlSchemasAnytype as i32
                            {
                                break 'character_content;
                            }

                            if (*inode).flags & XML_SCHEMA_ELEM_INFO_ERR_BAD_CONTENT == 0 {
                                let mut values = [const { Cow::Borrowed("") }; 10];
                                let mut terminal: i32 = 0;

                                if (*inode).regex_ctxt.is_null() {
                                    // Create the regex context.
                                    (*inode).regex_ctxt = xml_reg_new_exec_ctxt(
                                        (*(*inode).type_def).cont_model,
                                        Some(xml_schema_vcontent_model_callback),
                                        vctxt as _,
                                    );
                                    if (*inode).regex_ctxt.is_null() {
                                        VERROR_INT!(
                                            vctxt,
                                            "xmlSchemaValidatorPopElem",
                                            "failed to create a regex context"
                                        );
                                        break 'internal_error;
                                    }
                                }

                                // Do not check further content if the node has been nilled
                                if INODE_NILLED!(inode) {
                                    ret = 0;
                                    break 'skip_nilled;
                                }
                                // Get hold of the still expected content, since a further
                                // call to xmlRegExecPushString() will lose this information.
                                let values = xml_reg_exec_next_values(
                                    (*inode).regex_ctxt,
                                    &mut values,
                                    &raw mut terminal,
                                );
                                ret = xml_reg_exec_push_string(
                                    (*inode).regex_ctxt,
                                    null_mut(),
                                    null_mut(),
                                );
                                if ret < 0 || (ret == 0 && !INODE_NILLED!(inode)) {
                                    // Still missing something.
                                    ret = 1;
                                    (*inode).flags |= XML_SCHEMA_ELEM_INFO_ERR_BAD_CONTENT;
                                    if let Some((nbval, nbneg, values)) = values {
                                        xml_schema_complex_type_err(
                                            vctxt as XmlSchemaAbstractCtxtPtr,
                                            XmlParserErrors::XmlSchemavElementContent,
                                            None,
                                            null_mut(),
                                            "Missing child element(s)",
                                            nbval,
                                            nbneg,
                                            values,
                                        );
                                    }
                                } else {
                                    // Content model is satisfied.
                                    ret = 0;
                                }
                            }
                        }
                    }

                    // skip_nilled:

                    if (*(*inode).type_def).content_type
                        == XmlSchemaContentType::XmlSchemaContentElements
                    {
                        break 'end_elem;
                    }
                }
                // character_content:

                if !(*vctxt).value.is_null() {
                    xml_schema_free_value((*vctxt).value);
                    (*vctxt).value = null_mut();
                }
                // Check character content.
                if (*inode).decl.is_null() {
                    // Speedup if no declaration exists.
                    if wxs_is_simple((*inode).type_def) {
                        ret = xml_schema_vcheck_inode_data_type(
                            vctxt,
                            inode,
                            (*inode).type_def,
                            (*inode).value,
                        );
                    } else if WXS_HAS_SIMPLE_CONTENT!((*inode).type_def) {
                        ret = xml_schema_vcheck_inode_data_type(
                            vctxt,
                            inode,
                            (*(*inode).type_def).content_type_def,
                            (*inode).value,
                        );
                    }
                    if ret < 0 {
                        VERROR_INT!(
                            vctxt,
                            "xmlSchemaValidatorPopElem",
                            "calling xmlSchemaVCheckCVCSimpleType()"
                        );
                        break 'internal_error;
                    }
                    break 'end_elem;
                }
                // cvc-elt (3.3.4) : 5
                // The appropriate case among the following must be true:

                // cvc-elt (3.3.4) : 5.1
                // If the declaration has a {value constraint},
                // the item has neither element nor character [children] and
                // clause 3.2 has not applied, then all of the following must be true:
                if !(*(*inode).decl).value.is_null()
                    && (*inode).flags & XML_SCHEMA_ELEM_INFO_EMPTY != 0
                    && !INODE_NILLED!(inode)
                {
                    // cvc-elt (3.3.4) : 5.1.1
                    // If the `actual type definition` is a `local type definition`
                    // then the canonical lexical representation of the {value constraint}
                    // value must be a valid default for the `actual type definition` as
                    // defined in Element Default Valid (Immediate) ($3.3.6).

                    // NOTE: 'local' above means types acquired by xsi:type.
                    // NOTE: Although the *canonical* value is stated, it is not
                    // relevant if canonical or not. Additionally XML Schema 1.1
                    // will removed this requirement as well.
                    'default_psvi: {
                        if (*inode).flags & XML_SCHEMA_ELEM_INFO_LOCAL_TYPE != 0 {
                            ret = xml_schema_check_cos_valid_default(
                                vctxt,
                                (*(*inode).decl).value,
                                &raw mut (*inode).val,
                            );
                            if ret != 0 {
                                if ret < 0 {
                                    VERROR_INT!(
                                        vctxt,
                                        "xmlSchemaValidatorPopElem",
                                        "calling xmlSchemaCheckCOSValidDefault()"
                                    );
                                    break 'internal_error;
                                }
                                break 'end_elem;
                            }
                            // Stop here, to avoid redundant validation of the value (see following).
                            break 'default_psvi;
                        }
                        // cvc-elt (3.3.4) : 5.1.2
                        // The element information item with the canonical lexical
                        // representation of the {value constraint} value used as its
                        // `normalized value` must be `valid` with respect to the
                        // `actual type definition` as defined by Element Locally Valid (Type)
                        // ($3.3.4).
                        if wxs_is_simple((*inode).type_def) {
                            ret = xml_schema_vcheck_inode_data_type(
                                vctxt,
                                inode,
                                (*inode).type_def,
                                (*(*inode).decl).value,
                            );
                        } else if WXS_HAS_SIMPLE_CONTENT!((*inode).type_def) {
                            ret = xml_schema_vcheck_inode_data_type(
                                vctxt,
                                inode,
                                (*(*inode).type_def).content_type_def,
                                (*(*inode).decl).value,
                            );
                        }
                        if ret != 0 {
                            if ret < 0 {
                                VERROR_INT!(
                                    vctxt,
                                    "xmlSchemaValidatorPopElem",
                                    "calling xmlSchemaVCheckCVCSimpleType()"
                                );
                                break 'internal_error;
                            }
                            break 'end_elem;
                        }
                    }

                    // default_psvi:
                    // PSVI: Create a text node on the instance element.
                    if (*vctxt).options & XmlSchemaValidOption::XmlSchemaValVcICreate as i32 != 0 {
                        if let Some(mut node) = (*inode).node {
                            // VAL TODO: Normalize the value.
                            let norm_value = (*(*inode).type_def)
                                .normalize_value(
                                    CStr::from_ptr((*(*inode).decl).value as *const i8)
                                        .to_string_lossy()
                                        .as_ref(),
                                )
                                .map_or(null_mut(), |res| {
                                    xml_strndup(res.as_ptr(), res.len() as i32)
                                });
                            let text_child = if !norm_value.is_null() {
                                let text_child = xml_new_doc_text(node.doc, norm_value);
                                xml_free(norm_value as _);
                                text_child
                            } else {
                                xml_new_doc_text(node.doc, (*(*inode).decl).value)
                            };
                            if let Some(text_child) = text_child {
                                node.add_child(text_child.into());
                            } else {
                                VERROR_INT!(
                                    vctxt,
                                    "xmlSchemaValidatorPopElem",
                                    "calling xmlNewDocText()"
                                );
                                break 'internal_error;
                            }
                        }
                    }
                } else if !INODE_NILLED!(inode) {
                    // 5.2.1 The element information item must be `valid` with respect
                    // to the `actual type definition` as defined by Element Locally
                    // Valid (Type) ($3.3.4).
                    if wxs_is_simple((*inode).type_def) {
                        // SPEC (cvc-type) (3.1)
                        // "If the type definition is a simple type definition, ..."
                        // (3.1.3) "If clause 3.2 of Element Locally Valid
                        // (Element) ($3.3.4) did not apply, then the `normalized value`
                        // must be `valid` with respect to the type definition as defined
                        // by String Valid ($3.14.4).
                        ret = xml_schema_vcheck_inode_data_type(
                            vctxt,
                            inode,
                            (*inode).type_def,
                            (*inode).value,
                        );
                    } else if WXS_HAS_SIMPLE_CONTENT!((*inode).type_def) {
                        // SPEC (cvc-type) (3.2) "If the type definition is a complex type
                        // definition, then the element information item must be
                        // `valid` with respect to the type definition as per
                        // Element Locally Valid (Complex Type) ($3.4.4);"
                        //
                        // SPEC (cvc-complex-type) (2.2)
                        // "If the {content type} is a simple type definition, ...
                        // the `normalized value` of the element information item is
                        // `valid` with respect to that simple type definition as
                        // defined by String Valid ($3.14.4)."
                        ret = xml_schema_vcheck_inode_data_type(
                            vctxt,
                            inode,
                            (*(*inode).type_def).content_type_def,
                            (*inode).value,
                        );
                    }
                    if ret != 0 {
                        if ret < 0 {
                            VERROR_INT!(
                                vctxt,
                                "xmlSchemaValidatorPopElem",
                                "calling xmlSchemaVCheckCVCSimpleType()"
                            );
                            break 'internal_error;
                        }
                        break 'end_elem;
                    }
                    // 5.2.2 If there is a fixed {value constraint} and clause 3.2 has
                    // not applied, all of the following must be true:
                    if !(*(*inode).decl).value.is_null()
                        && (*(*inode).decl).flags & XML_SCHEMAS_ELEM_FIXED != 0
                    {
                        // TODO: We will need a computed value, when comparison is
                        // done on computed values.

                        // 5.2.2.1 The element information item must have no element
                        // information item [children].
                        if (*inode).flags & XML_SCHEMA_ELEM_INFO_HAS_ELEM_CONTENT != 0 {
                            ret = XmlParserErrors::XmlSchemavCvcElt5_2_2_1 as i32;
                            VERROR!(
                                vctxt,
                                ret.try_into().unwrap(),
                                null_mut(),
                                "The content must not contain element nodes since there is a fixed value constraint"
                            );
                            break 'end_elem;
                        } else {
                            // 5.2.2.2 The appropriate case among the following must be true:
                            if WXS_HAS_MIXED_CONTENT!((*inode).type_def) {
                                // 5.2.2.2.1 If the {content type} of the `actual type
                                // definition` is mixed, then the *initial value* of the
                                // item must match the canonical lexical representation
                                // of the {value constraint} value.
                                //
                                // ... the *initial value* of an element information
                                // item is the string composed of, in order, the
                                // [character code] of each character information item in
                                // the [children] of that element information item.
                                if !xml_str_equal((*inode).value, (*(*inode).decl).value) {
                                    let value = CStr::from_ptr((*inode).value as *const i8)
                                        .to_string_lossy();
                                    let vc_value =
                                        CStr::from_ptr((*(*inode).decl).value as *const i8)
                                            .to_string_lossy();
                                    // VAL TODO: Report invalid & expected values as well.
                                    // VAL TODO: Implement the canonical stuff.
                                    ret = XmlParserErrors::XmlSchemavCvcElt5_2_2_2_1 as i32;
                                    xml_schema_custom_err(
                                    vctxt as XmlSchemaAbstractCtxtPtr,
                                    ret.try_into().unwrap(),
                                    None,
                                    null_mut(),
                                    format!("The initial value '{value}' does not match the fixed value constraint '{vc_value}'").as_str(),
                                    Some(&value),
                                    Some(&vc_value)
                                );
                                    break 'end_elem;
                                }
                            } else if WXS_HAS_SIMPLE_CONTENT!((*inode).type_def) {
                                let value =
                                    CStr::from_ptr((*inode).value as *const i8).to_string_lossy();
                                let vc_value = CStr::from_ptr((*(*inode).decl).value as *const i8)
                                    .to_string_lossy();

                                // 5.2.2.2.2 If the {content type} of the `actual type
                                // definition` is a simple type definition, then the
                                // *actual value* of the item must match the canonical
                                // lexical representation of the {value constraint} value.
                                // VAL TODO: *actual value* is the normalized value, impl.
                                //           this.
                                // VAL TODO: Report invalid & expected values as well.
                                // VAL TODO: Implement a comparison with the computed values.
                                if !xml_str_equal((*inode).value, (*(*inode).decl).value) {
                                    ret = XmlParserErrors::XmlSchemavCvcElt5_2_2_2_2 as i32;
                                    xml_schema_custom_err(
                                    vctxt as XmlSchemaAbstractCtxtPtr,
                                    ret.try_into().unwrap(),
                                    None,
                                    null_mut(),
                                    format!("The actual value '{value}' does not match the fixed value constraint '{vc_value}'").as_str(),
                                    Some(&value),
                                    Some(&vc_value)
                                );
                                    break 'end_elem;
                                }
                            }
                        }
                    }
                }
            }

            // end_elem:
            if (*vctxt).depth < 0 {
                // TODO: raise error?
                return 0;
            }
            if (*vctxt).depth == (*vctxt).skip_depth {
                (*vctxt).skip_depth = -1;
            }
            // Evaluate the history of XPath state objects.
            if (*inode).applied_xpath != 0
                && xml_schema_xpath_process_history(vctxt, (*vctxt).depth) == -1
            {
                break 'internal_error;
            }
            // MAYBE TODO:
            // SPEC (6) "The element information item must be `valid` with
            // respect to each of the {identity-constraint definitions} as per
            // Identity-constraint Satisfied ($3.11.4)."

            // PSVI TODO: If we expose IDC node-tables via PSVI then the tables
            //   need to be built in any case.
            //   We will currently build IDC node-tables and bubble them only if
            //   keyrefs do exist.

            // Add the current IDC target-nodes to the IDC node-tables.
            if !(*inode).idc_matchers.is_null()
                && ((*vctxt).has_keyrefs != 0 || (*vctxt).create_idcnode_tables != 0)
                && xml_schema_idc_fill_node_tables(vctxt, inode) == -1
            {
                break 'internal_error;
            }
            // Validate IDC keyrefs.
            if (*(*vctxt).inode).has_keyrefs != 0 && xml_schema_check_cvc_idc_key_ref(vctxt) == -1 {
                break 'internal_error;
            }
            // Merge/free the IDC table.
            if !(*inode).idc_table.is_null()
                && (*vctxt).depth > 0
                && ((*vctxt).has_keyrefs != 0 || (*vctxt).create_idcnode_tables != 0)
            {
                // Merge the IDC node table with the table of the parent node.
                if xml_schema_bubble_idc_node_tables(vctxt) == -1 {
                    break 'internal_error;
                }
            }
            // Clear the current ielem.
            // VAL TODO: Don't free the PSVI IDC tables if they are
            // requested for the PSVI.
            xml_schema_clear_elem_info(vctxt, inode);
            // Skip further processing if we are on the validation root.
            if (*vctxt).depth == 0 {
                (*vctxt).depth -= 1;
                (*vctxt).inode = null_mut();
                return 0;
            }
            // Reset the keyrefDepth if needed.
            if !(*vctxt).aidcs.is_null() {
                let mut aidc: XmlSchemaIDCAugPtr = (*vctxt).aidcs;
                while {
                    if (*aidc).keyref_depth == (*vctxt).depth {
                        // A 'keyrefDepth' of a key/unique IDC matches the current
                        // depth, this means that we are leaving the scope of the
                        // top-most keyref IDC which refers to this IDC.
                        (*aidc).keyref_depth = -1;
                    }
                    aidc = (*aidc).next;
                    !aidc.is_null()
                } {}
            }
            (*vctxt).depth -= 1;
            (*vctxt).inode = *(*vctxt).elem_infos.add((*vctxt).depth as usize);
            // VAL TODO: 7 If the element information item is the `validation root`, it must be
            // `valid` per Validation Root Valid (ID/IDREF) ($3.3.4).
            return ret;
        }

        // internal_error:
        (*vctxt).err = -1;
        -1
    }
}

unsafe fn xml_schema_vdoc_walk(vctxt: XmlSchemaValidCtxtPtr) -> i32 {
    unsafe {
        let mut ret: i32 = 0;
        let mut ielem: XmlSchemaNodeInfoPtr = null_mut();
        let mut ns_name: *const XmlChar;

        // DOC VAL TODO: Move this to the start function.
        let val_root = if let Some(validation_root) = (*vctxt).validation_root {
            Some(validation_root)
        } else if let Some(doc) = (*vctxt).doc {
            doc.get_root_element()
        } else {
            None
        };
        if val_root.is_none() {
            // VAL TODO: Error code?
            VERROR!(
                vctxt,
                1i32.try_into().unwrap(),
                null_mut(),
                "The document has no document element"
            );
            return 1;
        }
        (*vctxt).depth = -1;
        (*vctxt).validation_root = val_root;
        let mut node = val_root;
        'main: while let Some(cur_node) = node {
            'goto_leave_node: {
                if (*vctxt).skip_depth != -1 && (*vctxt).depth >= (*vctxt).skip_depth {
                    // goto next_sibling;
                    if let Some(next) = cur_node
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap())
                    {
                        node = Some(next);
                        continue 'main;
                    } else {
                        node = cur_node.parent().map(|p| XmlNodePtr::try_from(p).unwrap());
                        break 'goto_leave_node;
                    }
                }
                if cur_node.element_type() == XmlElementType::XmlElementNode {
                    // Init the node-info.
                    (*vctxt).depth += 1;
                    if xml_schema_validator_push_elem(vctxt) == -1 {
                        // goto internal_error;
                        return -1;
                    }
                    ielem = (*vctxt).inode;
                    (*ielem).node = cur_node.into();
                    (*ielem).node_line = cur_node.line as _;
                    (*ielem).local_name = cur_node.name;
                    if let Some(ns) = cur_node.ns {
                        (*ielem).ns_name = ns.href;
                    }
                    (*ielem).flags |= XML_SCHEMA_ELEM_INFO_EMPTY;
                    // Register attributes.
                    // DOC VAL TODO: We do not register namespace declaration attributes yet.
                    (*vctxt).nb_attr_infos = 0;
                    if cur_node.properties.is_some() {
                        let mut attr = cur_node.properties;
                        while let Some(cur_attr) = attr {
                            if let Some(ns) = cur_attr.ns {
                                ns_name = ns.href;
                            } else {
                                ns_name = null_mut();
                            }
                            // Note that we give it the line number of the parent element.
                            let value = cur_attr
                                .children()
                                .and_then(|c| c.get_string(cur_attr.doc, 1))
                                .map(|c| CString::new(c).unwrap());
                            let value = xml_strdup(
                                value
                                    .as_ref()
                                    .map_or(null_mut(), |v| v.as_ptr() as *const u8),
                            );
                            ret = xml_schema_validator_push_attribute(
                                vctxt,
                                Some(cur_attr),
                                (*ielem).node_line,
                                cur_attr.name,
                                ns_name,
                                0,
                                value,
                                1,
                            );
                            if ret == -1 {
                                VERROR_INT!(
                                    vctxt,
                                    "xmlSchemaDocWalk",
                                    "calling xmlSchemaValidatorPushAttribute()"
                                );
                                // goto internal_error;
                                return -1;
                            }
                            attr = cur_attr.next;
                        }
                    }
                    // Validate the element.
                    ret = xml_schema_validate_elem(vctxt);
                    if ret != 0 {
                        if ret == -1 {
                            VERROR_INT!(
                                vctxt,
                                "xmlSchemaDocWalk",
                                "calling xmlSchemaValidateElem()"
                            );
                            // goto internal_error;
                            return -1;
                        }
                        // Don't stop validation; just skip the content of this element.
                        break 'goto_leave_node;
                    }
                    if (*vctxt).skip_depth != -1 && (*vctxt).depth >= (*vctxt).skip_depth {
                        break 'goto_leave_node;
                    }
                } else if matches!(
                    cur_node.element_type(),
                    XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
                ) {
                    // Process character content.
                    if !ielem.is_null() && (*ielem).flags & XML_SCHEMA_ELEM_INFO_EMPTY != 0 {
                        (*ielem).flags ^= XML_SCHEMA_ELEM_INFO_EMPTY;
                    }
                    ret = xml_schema_vpush_text(
                        vctxt,
                        cur_node.element_type() as _,
                        cur_node.content,
                        -1,
                        XML_SCHEMA_PUSH_TEXT_PERSIST,
                        null_mut(),
                    );
                    if ret < 0 {
                        VERROR_INT!(vctxt, "xmlSchemaVDocWalk", "calling xmlSchemaVPushText()");
                        // goto internal_error;
                        return -1;
                    }
                    // DOC VAL TODO: Should we skip further validation of the element content here?
                } else if matches!(
                    cur_node.element_type(),
                    XmlElementType::XmlEntityNode | XmlElementType::XmlEntityRefNode
                ) {
                    // DOC VAL TODO: What to do with entities?
                    VERROR_INT!(
                        vctxt,
                        "xmlSchemaVDocWalk",
                        "there is at least one entity reference in the node-tree currently being validated. Processing of entities with this XML Schema processor is not supported (yet). Please substitute entities before validation."
                    );
                    // goto internal_error;
                    return -1;
                } else {
                    break 'goto_leave_node;
                    // DOC VAL TODO: XInclude nodes, etc.
                }
                // Walk the doc.
                if let Some(children) = cur_node
                    .children
                    .map(|children| XmlNodePtr::try_from(children).unwrap())
                {
                    node = Some(children);
                    continue 'main;
                }
            }
            // leave_node:
            'leave_node: while let Some(cur_node) = node {
                if cur_node.element_type() == XmlElementType::XmlElementNode {
                    // Leaving the scope of an element.
                    if Some(cur_node) != (*(*vctxt).inode).node {
                        VERROR_INT!(vctxt, "xmlSchemaVDocWalk", "element position mismatch");
                        // goto internal_error;
                        return -1;
                    }
                    ret = xml_schema_validator_pop_elem(vctxt);
                    if ret != 0 && ret < 0 {
                        VERROR_INT!(
                            vctxt,
                            "xmlSchemaVDocWalk",
                            "calling xmlSchemaValidatorPopElem()"
                        );
                        // goto internal_error;
                        return -1;
                    }
                    if Some(cur_node) == val_root {
                        break 'main;
                    }
                }
                // next_sibling:
                if let Some(next) = cur_node
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap())
                {
                    node = Some(next);
                    continue 'main;
                } else {
                    node = cur_node.parent.map(|p| XmlNodePtr::try_from(p).unwrap());
                    // goto leave_node;
                    continue 'leave_node;
                }
            }
        }

        // exit:
        ret
        // internal_error:
        // return -1;
    }
}

/// Frees a list of IDC matchers.
#[doc(alias = "xmlSchemaIDCFreeMatcherList")]
unsafe fn xml_schema_idc_free_matcher_list(mut matcher: XmlSchemaIDCMatcherPtr) {
    unsafe {
        let mut next: XmlSchemaIDCMatcherPtr;

        while !matcher.is_null() {
            next = (*matcher).next;
            if !(*matcher).key_seqs.is_null() {
                for i in 0..(*matcher).size_key_seqs {
                    if !(*(*matcher).key_seqs.add(i as usize)).is_null() {
                        xml_free(*(*matcher).key_seqs.add(i as usize) as _);
                    }
                }
                xml_free((*matcher).key_seqs as _);
            }
            if !(*matcher).targets.is_null() {
                if (*matcher).idc_type == XmlSchemaTypeType::XmlSchemaTypeIDCKeyref as i32 {
                    let mut idc_node: XmlSchemaPSVIIDCNodePtr;
                    // Node-table items for keyrefs are not stored globally
                    // to the validation context, since they are not bubbled.
                    // We need to free them here.
                    for i in 0..(*(*matcher).targets).items.len() {
                        idc_node = (*(*matcher).targets).items[i] as XmlSchemaPSVIIDCNodePtr;
                        xml_free((*idc_node).keys as _);
                        xml_free(idc_node as _);
                    }
                }
                xml_schema_item_list_free((*matcher).targets);
            }
            if !(*matcher).htab.is_null() {
                xml_hash_free((*matcher).htab, Some(xml_free_idc_hash_entry));
            }
            xml_free(matcher as _);
            matcher = next;
        }
    }
}

/// Free the resources associated to the schema validation context;
/// leaves some fields alive intended for reuse of the context.
#[doc(alias = "xmlSchemaClearValidCtxt")]
unsafe fn xml_schema_clear_valid_ctxt(vctxt: XmlSchemaValidCtxtPtr) {
    unsafe {
        if vctxt.is_null() {
            return;
        }

        // TODO: Should we clear the flags?
        //   Might be problematic if one reuses the context
        //   and assumes that the options remain the same.
        (*vctxt).flags = 0;
        (*vctxt).validation_root = None;
        (*vctxt).doc = None;
        #[cfg(feature = "libxml_reader")]
        {
            (*vctxt).reader = null_mut();
        }
        (*vctxt).has_keyrefs = 0;

        if !(*vctxt).value.is_null() {
            xml_schema_free_value((*vctxt).value);
            (*vctxt).value = null_mut();
        }
        // Augmented IDC information.
        if !(*vctxt).aidcs.is_null() {
            let mut cur: XmlSchemaIDCAugPtr = (*vctxt).aidcs;
            let mut next: XmlSchemaIDCAugPtr;
            while {
                next = (*cur).next;
                xml_free(cur as _);
                cur = next;
                !cur.is_null()
            } {}
            (*vctxt).aidcs = null_mut();
        }

        if !(*vctxt).idc_nodes.is_null() {
            let mut item: XmlSchemaPSVIIDCNodePtr;

            for i in 0..(*vctxt).nb_idc_nodes {
                item = *(*vctxt).idc_nodes.add(i as usize);
                xml_free((*item).keys as _);
                xml_free(item as _);
            }
            xml_free((*vctxt).idc_nodes as _);
            (*vctxt).idc_nodes = null_mut();
            (*vctxt).nb_idc_nodes = 0;
            (*vctxt).size_idc_nodes = 0;
        }

        if !(*vctxt).idc_keys.is_null() {
            for i in 0..(*vctxt).nb_idc_keys {
                xml_schema_idc_free_key(*(*vctxt).idc_keys.add(i as usize));
            }
            xml_free((*vctxt).idc_keys as _);
            (*vctxt).idc_keys = null_mut();
            (*vctxt).nb_idc_keys = 0;
            (*vctxt).size_idc_keys = 0;
        }

        // Note that we won't delete the XPath state pool here.
        if !(*vctxt).xpath_states.is_null() {
            xml_schema_free_idc_state_obj_list((*vctxt).xpath_states);
            (*vctxt).xpath_states = null_mut();
        }
        // Attribute info.
        if (*vctxt).nb_attr_infos != 0 {
            xml_schema_clear_attr_infos(vctxt);
        }
        // Element info.
        if !(*vctxt).elem_infos.is_null() {
            let mut ei: XmlSchemaNodeInfoPtr;

            for i in 0..(*vctxt).size_elem_infos {
                ei = *(*vctxt).elem_infos.add(i as usize);
                if ei.is_null() {
                    break;
                }
                xml_schema_clear_elem_info(vctxt, ei);
            }
        }
        (*(*vctxt).node_qnames).clear();
        // Recreate the dict.
        xml_dict_free((*vctxt).dict);
        // TODO: Is is save to recreate it? Do we have a scenario
        // where the user provides the dict?
        (*vctxt).dict = xml_dict_create();

        if !(*vctxt).filename.is_null() {
            xml_free((*vctxt).filename as _);
            (*vctxt).filename = null_mut();
        }

        // Note that some cleanup functions can move items to the cache,
        // so the cache shouldn't be freed too early.
        if !(*vctxt).idc_matcher_cache.is_null() {
            let mut matcher: XmlSchemaIDCMatcherPtr = (*vctxt).idc_matcher_cache;
            let mut tmp: XmlSchemaIDCMatcherPtr;

            while !matcher.is_null() {
                tmp = matcher;
                matcher = (*matcher).next_cached;
                xml_schema_idc_free_matcher_list(tmp);
            }
            (*vctxt).idc_matcher_cache = null_mut();
        }
    }
}

unsafe fn xml_schema_post_run(vctxt: XmlSchemaValidCtxtPtr) {
    unsafe {
        if (*vctxt).xsi_assemble != 0 && !(*vctxt).schema.is_null() {
            xml_schema_free((*vctxt).schema);
            (*vctxt).schema = null_mut();
        }
        xml_schema_clear_valid_ctxt(vctxt);
    }
}

unsafe fn xml_schema_vstart(vctxt: XmlSchemaValidCtxtPtr) -> i32 {
    unsafe {
        let mut ret: i32 = 0;

        if xml_schema_pre_run(vctxt) < 0 {
            return -1;
        }

        #[cfg(feature = "libxml_reader")]
        let f = !(*vctxt).reader.is_null();
        #[cfg(not(feature = "libxml_reader"))]
        let f = false;
        if (*vctxt).doc.is_some() {
            // Tree validation.
            ret = xml_schema_vdoc_walk(vctxt);
        } else if f {
            // XML Reader validation.
            // #ifdef XML_SCHEMA_READER_ENABLED
            //     ret = xmlSchemaVReaderWalk(vctxt);
            // #endif
        } else if !(*vctxt).parser_ctxt.is_null() && (*(*vctxt).parser_ctxt).sax.is_some() {
            // SAX validation.
            ret = xml_parse_document((*vctxt).parser_ctxt);
        } else {
            VERROR_INT!(vctxt, "xmlSchemaVStart", "no instance to validate");
            ret = -1;
        }

        xml_schema_post_run(vctxt);
        if ret == 0 {
            ret = (*vctxt).err;
        }
        ret
    }
}

/// Validate a document tree in memory.
///
/// Returns 0 if the document is schemas valid, a positive error code
/// number otherwise and -1 in case of internal or API error.
#[doc(alias = "xmlSchemaValidateDoc")]
pub unsafe fn xml_schema_validate_doc(ctxt: XmlSchemaValidCtxtPtr, doc: XmlDocPtr) -> i32 {
    unsafe {
        if ctxt.is_null() {
            return -1;
        }

        (*ctxt).doc = Some(doc);
        (*ctxt).node = doc.get_root_element();
        if (*ctxt).node.is_none() {
            xml_schema_custom_err(
                ctxt as XmlSchemaAbstractCtxtPtr,
                XmlParserErrors::XmlSchemavDocumentElementMissing,
                Some(doc.into()),
                null_mut(),
                "The document has no document element",
                None,
                None,
            );
            return (*ctxt).err;
        }
        (*ctxt).validation_root = (*ctxt).node;
        xml_schema_vstart(ctxt)
    }
}

/// Validate a branch of a tree, starting with the given @elem.
///
/// Returns 0 if the element and its subtree is valid, a positive error
/// code number otherwise and -1 in case of an internal or API error.
#[doc(alias = "xmlSchemaValidateOneElement")]
pub unsafe fn xml_schema_validate_one_element(
    ctxt: XmlSchemaValidCtxtPtr,
    elem: XmlNodePtr,
) -> i32 {
    unsafe {
        if ctxt.is_null() || elem.element_type() != XmlElementType::XmlElementNode {
            return -1;
        }

        if (*ctxt).schema.is_null() {
            return -1;
        }

        (*ctxt).doc = elem.doc;
        (*ctxt).node = elem.into();
        (*ctxt).validation_root = elem.into();
        xml_schema_vstart(ctxt)
    }
}

/// Internal locator function for the readers
///
/// Returns 0 in case the Schema validation could be (de)activated and -1 in case of error.
#[doc(alias = "xmlSchemaValidateStreamLocator")]
unsafe fn xml_schema_validate_stream_locator(
    ctx: *mut c_void,
    file: *mut Option<String>,
    line: *mut u64,
) -> i32 {
    unsafe {
        if ctx.is_null() || (file.is_null() && line.is_null()) {
            return -1;
        }

        if !file.is_null() {
            *file = None;
        }
        if !line.is_null() {
            *line = 0;
        }

        let ctxt: XmlParserCtxtPtr = ctx as XmlParserCtxtPtr;
        if !(*ctxt).input.is_null() {
            if !file.is_null() {
                *file = (*(*ctxt).input).filename.clone();
            }
            if !line.is_null() {
                *line = (*(*ctxt).input).line as _;
            }
            return 0;
        }
        -1
    }
}

/// Validate an input based on a flow of SAX event from the parser
/// and forward the events to the @sax handler with the provided @user_data
/// the user provided @sax handler must be a SAX2 one.
///
/// Returns 0 if the document is schemas valid, a positive error code
/// number otherwise and -1 in case of internal or API error.
#[doc(alias = "xmlSchemaValidateStream")]
pub unsafe fn xml_schema_validate_stream(
    ctxt: XmlSchemaValidCtxtPtr,
    input: XmlParserInputBuffer,
    enc: XmlCharEncoding,
    sax: Option<Box<XmlSAXHandler>>,
    user_data: Option<GenericErrorContext>,
) -> i32 {
    unsafe {
        let mut plug: XmlSchemaSAXPlugPtr = null_mut();
        let pctxt: XmlParserCtxtPtr;

        let mut ret: i32;

        if ctxt.is_null() {
            return -1;
        }

        // prepare the parser
        if sax.is_some() {
            let Ok(new) = xml_new_sax_parser_ctxt(sax, user_data) else {
                return -1;
            };
            pctxt = new;
        } else {
            pctxt = xml_new_parser_ctxt();
            if pctxt.is_null() {
                return -1;
            }
            // We really want (*pctxt).sax to be NULL here.
            (*pctxt).sax = None;
        }
        (*pctxt).linenumbers = 1;
        xml_schema_validate_set_locator(ctxt, Some(xml_schema_validate_stream_locator), pctxt as _);

        let input = Rc::new(RefCell::new(input));
        let input_stream: XmlParserInputPtr =
            xml_new_io_input_stream(pctxt, Rc::clone(&input), enc);
        if input_stream.is_null() {
            ret = -1;
        // goto done;
        } else {
            (*pctxt).input_push(input_stream);
            (*ctxt).parser_ctxt = pctxt;
            (*ctxt).input = Some(Rc::clone(&input));

            // Plug the validation and launch the parsing
            plug = xml_schema_sax_plug(ctxt, &mut (*pctxt).sax, &raw mut (*pctxt).user_data);
            if plug.is_null() {
                ret = -1;
                // goto done;
            } else {
                (*ctxt).input = Some(input);
                (*ctxt).enc = enc;
                // (*ctxt).sax = (*pctxt).sax;
                (*ctxt).flags |= XML_SCHEMA_VALID_CTXT_FLAG_STREAM;
                ret = xml_schema_vstart(ctxt);

                if ret == 0 && (*(*ctxt).parser_ctxt).well_formed == 0 {
                    ret = (*(*ctxt).parser_ctxt).err_no;
                    if ret == 0 {
                        ret = 1;
                    }
                }
            }
        }

        // done:
        (*ctxt).parser_ctxt = null_mut();
        // (*ctxt).sax = null_mut();
        (*ctxt).input = None;
        if !plug.is_null() {
            xml_schema_sax_unplug(plug);
        }
        // cleanup
        if !pctxt.is_null() {
            xml_free_parser_ctxt(pctxt);
        }
        ret
    }
}

/// Do a schemas validation of the given resource, it will use the
/// SAX streamable validation internally.
///
/// Returns 0 if the document is valid, a positive error code
/// number otherwise and -1 in case of an internal or API error.
#[doc(alias = "xmlSchemaValidateFile")]
pub unsafe fn xml_schema_validate_file(
    ctxt: XmlSchemaValidCtxtPtr,
    filename: *const c_char,
    _options: i32,
) -> i32 {
    unsafe {
        if ctxt.is_null() || filename.is_null() {
            return -1;
        }

        let Some(input) = XmlParserInputBuffer::from_uri(
            CStr::from_ptr(filename).to_string_lossy().as_ref(),
            XmlCharEncoding::None,
        ) else {
            return -1;
        };
        let ret: i32 = xml_schema_validate_stream(ctxt, input, XmlCharEncoding::None, None, None);
        ret
    }
}

/// Allow access to the parser context of the schema validation context
///
/// Returns the parser context of the schema validation context or NULL in case of error.
#[doc(alias = "xmlSchemaValidCtxtGetParserCtxt")]
pub unsafe fn xml_schema_valid_ctxt_get_parser_ctxt(
    ctxt: XmlSchemaValidCtxtPtr,
) -> XmlParserCtxtPtr {
    unsafe {
        if ctxt.is_null() {
            return null_mut();
        }
        (*ctxt).parser_ctxt
    }
}

const XML_SAX_PLUG_MAGIC: u32 = 0xdc43ba21;

pub type XmlSchemaSAXPlugPtr = *mut XmlSchemaSAXPlugStruct;
#[repr(C)]
pub struct XmlSchemaSAXPlugStruct {
    magic: u32,

    /* the original callbacks information */
    user_sax_ptr: *mut XmlSAXHandlerPtr,
    user_sax: Option<Box<XmlSAXHandler>>,
    user_data_ptr: *mut Option<GenericErrorContext>,
    user_data: Option<GenericErrorContext>,

    /* the block plugged back and validation information */
    schemas_sax: XmlSAXHandler,
    ctxt: XmlSchemaValidCtxtPtr,
}

// #[allow(clippy::too_many_arguments)]
unsafe fn xml_schema_sax_handle_start_element_ns(
    ctx: Option<GenericErrorContext>,
    localname: &str,
    _prefix: Option<&str>,
    uri: Option<&str>,
    namespaces: &[(Option<String>, String)],
    _nb_defaulted: usize,
    attributes: &[(String, Option<String>, Option<String>, String)],
) {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let vctxt = *lock.downcast_ref::<XmlSchemaValidCtxtPtr>().unwrap();
        let mut ret: i32;

        // SAX VAL TODO: What to do with nb_defaulted?
        // Skip elements if inside a "skip" wildcard or invalid.
        (*vctxt).depth += 1;
        if (*vctxt).skip_depth != -1 && (*vctxt).depth >= (*vctxt).skip_depth {
            return;
        }
        // Push the element.
        if xml_schema_validator_push_elem(vctxt) == -1 {
            VERROR_INT!(
                vctxt,
                "xmlSchemaSAXHandleStartElementNs",
                "calling xmlSchemaValidatorPushElem()"
            );
            // goto internal_error;
            (*vctxt).err = -1;
            (*(*vctxt).parser_ctxt).stop();
            return;
        }
        let ielem: XmlSchemaNodeInfoPtr = (*vctxt).inode;
        // TODO: Is this OK?
        (*ielem).node_line = xml_sax2_get_line_number((*vctxt).parser_ctxt as _);
        let localname = CString::new(localname).unwrap();
        (*ielem).local_name = localname.as_ptr() as *const u8;
        let uri = uri.map(|u| CString::new(u).unwrap());
        (*ielem).ns_name = uri.as_deref().map_or(null(), |u| u.as_ptr() as *const u8);
        (*ielem).flags |= XML_SCHEMA_ELEM_INFO_EMPTY;
        // Register namespaces on the elem info.
        // Although the parser builds its own namespace list,
        // we have no access to it, so we'll use an own one.
        let namespaces = namespaces
            .iter()
            .map(|(pre, loc)| {
                let pre = pre.as_deref().map(|p| CString::new(p).unwrap());
                let loc = CString::new(loc.as_str()).unwrap();
                (pre, loc)
            })
            .collect::<Vec<_>>(); // temporary workaround.
        for (pre, href) in &namespaces {
            // Store prefix and namespace name.
            if (*ielem).ns_bindings.is_null() {
                (*ielem).ns_bindings = xml_malloc(10 * size_of::<*const XmlChar>()) as _;
                if (*ielem).ns_bindings.is_null() {
                    xml_schema_verr_memory(
                        vctxt,
                        "allocating namespace bindings for SAX validation",
                        None,
                    );
                    // goto internal_error;
                    (*vctxt).err = -1;
                    (*(*vctxt).parser_ctxt).stop();
                    return;
                }
                (*ielem).nb_ns_bindings = 0;
                (*ielem).size_ns_bindings = 5;
            } else if (*ielem).size_ns_bindings <= (*ielem).nb_ns_bindings {
                (*ielem).size_ns_bindings *= 2;
                (*ielem).ns_bindings = xml_realloc(
                    (*ielem).ns_bindings as *mut c_void,
                    (*ielem).size_ns_bindings as usize * 2 * size_of::<*const XmlChar>(),
                ) as _;
                if (*ielem).ns_bindings.is_null() {
                    xml_schema_verr_memory(
                        vctxt,
                        "re-allocating namespace bindings for SAX validation",
                        None,
                    );
                    // goto internal_error;
                    (*vctxt).err = -1;
                    (*(*vctxt).parser_ctxt).stop();
                    return;
                }
            }

            *(*ielem)
                .ns_bindings
                .add((*ielem).nb_ns_bindings as usize * 2) =
                pre.as_deref().map_or(null(), |p| p.as_ptr() as *const u8);
            if !href.is_empty() {
                // Handle xmlns="".
                *(*ielem)
                    .ns_bindings
                    .add((*ielem).nb_ns_bindings as usize * 2 + 1) = null_mut();
            } else {
                *(*ielem)
                    .ns_bindings
                    .add((*ielem).nb_ns_bindings as usize * 2 + 1) = href.as_ptr() as *const u8;
            }
            (*ielem).nb_ns_bindings += 1;
        }
        // Register attributes.
        // SAX VAL TODO: We are not adding namespace declaration attributes yet.
        let attributes = attributes
            .iter()
            .map(|attr| {
                let loc = CString::new(attr.0.as_str()).unwrap();
                let pre = attr.1.as_deref().map(|pre| CString::new(pre).unwrap());
                let url = attr.2.as_deref().map(|url| CString::new(url).unwrap());
                // Duplicate the value, changing any &#38; to a literal ampersand.
                //
                // libxml2 differs from normal SAX here in that it escapes all ampersands
                // as &#38; instead of delivering the raw converted string. Changing the
                // behavior at this point would break applications that use this API, so
                // we are forced to work around it.
                let val = CString::new(attr.3.replace("&#38;", "&").as_str()).unwrap();
                (loc, pre, url, val)
            })
            .collect::<Vec<_>>();
        for attr in &attributes {
            // TODO: Set the node line.
            ret = xml_schema_validator_push_attribute(
                vctxt,
                None,
                (*ielem).node_line,
                attr.0.as_ptr() as *const u8,
                attr.2
                    .as_deref()
                    .map_or(null(), |ns_name| ns_name.as_ptr() as *const u8),
                0,
                attr.3.as_ptr() as *mut u8,
                1,
            );
            if ret == -1 {
                VERROR_INT!(
                    vctxt,
                    "xmlSchemaSAXHandleStartElementNs",
                    "calling xmlSchemaValidatorPushAttribute()"
                );
                // goto internal_error;
                (*vctxt).err = -1;
                (*(*vctxt).parser_ctxt).stop();
                return;
            }
        }
        // Validate the element.
        ret = xml_schema_validate_elem(vctxt);
        if ret != 0 && ret == -1 {
            VERROR_INT!(
                vctxt,
                "xmlSchemaSAXHandleStartElementNs",
                "calling xmlSchemaValidateElem()"
            );
            // goto internal_error;
            (*vctxt).err = -1;
            (*(*vctxt).parser_ctxt).stop();
        }

        // `namespaces` must live until this point at least.
        // It must not be dropped before execute `xml_schema_validate_elem`.
        drop(namespaces);
        drop(attributes);

        // exit:
        // internal_error:
        //     (*vctxt).err = -1;
        //     xmlStopParser((*vctxt).parserCtxt);
        //     return;
    }
}

unsafe fn xml_schema_sax_handle_end_element_ns(
    ctx: Option<GenericErrorContext>,
    localname: &str,
    _prefix: Option<&str>,
    uri: Option<&str>,
) {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let vctxt = *lock.downcast_ref::<XmlSchemaValidCtxtPtr>().unwrap();

        // Skip elements if inside a "skip" wildcard or if invalid.
        if (*vctxt).skip_depth != -1 {
            if (*vctxt).depth > (*vctxt).skip_depth {
                (*vctxt).depth -= 1;
                return;
            } else {
                (*vctxt).skip_depth = -1;
            }
        }
        // SAX VAL TODO: Just a temporary check.
        let localname = CString::new(localname).unwrap();
        let uri = uri.map(|u| CString::new(u).unwrap());
        if !xml_str_equal(
            (*(*vctxt).inode).local_name,
            localname.as_ptr() as *const u8,
        ) || !xml_str_equal(
            (*(*vctxt).inode).ns_name,
            uri.as_deref().map_or(null(), |u| u.as_ptr() as *const u8),
        ) {
            VERROR_INT!(vctxt, "xmlSchemaSAXHandleEndElementNs", "elem pop mismatch");
        }
        let res: i32 = xml_schema_validator_pop_elem(vctxt);
        if res != 0 && res < 0 {
            VERROR_INT!(
                vctxt,
                "xmlSchemaSAXHandleEndElementNs",
                "calling xmlSchemaValidatorPopElem()"
            );
            // goto internal_error;
            (*vctxt).err = -1;
            (*(*vctxt).parser_ctxt).stop();
        }
        // exit:
        // internal_error:
        //     (*vctxt).err = -1;
        //     xmlStopParser((*vctxt).parserCtxt);
        //     return;
    }
}

// Process text content.
unsafe fn xml_schema_sax_handle_text(ctx: Option<GenericErrorContext>, ch: &str) {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let vctxt = *lock.downcast_ref::<XmlSchemaValidCtxtPtr>().unwrap();

        if (*vctxt).depth < 0 {
            return;
        }
        if (*vctxt).skip_depth != -1 && (*vctxt).depth >= (*vctxt).skip_depth {
            return;
        }
        if (*(*vctxt).inode).flags & XML_SCHEMA_ELEM_INFO_EMPTY != 0 {
            (*(*vctxt).inode).flags ^= XML_SCHEMA_ELEM_INFO_EMPTY;
        }
        let len = ch.len();
        let ch = CString::new(ch).unwrap();
        if xml_schema_vpush_text(
            vctxt,
            XmlElementType::XmlTextNode as i32,
            ch.as_ptr() as *const u8,
            len as i32,
            XML_SCHEMA_PUSH_TEXT_VOLATILE,
            null_mut(),
        ) == -1
        {
            VERROR_INT!(
                vctxt,
                "xmlSchemaSAXHandleCDataSection",
                "calling xmlSchemaVPushText()"
            );
            (*vctxt).err = -1;
            (*(*vctxt).parser_ctxt).stop();
        }
    }
}

// Process CDATA content.
unsafe fn xml_schema_sax_handle_cdata_section(ctx: Option<GenericErrorContext>, ch: &str) {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let vctxt = *lock.downcast_ref::<XmlSchemaValidCtxtPtr>().unwrap();

        if (*vctxt).depth < 0 {
            return;
        }
        if (*vctxt).skip_depth != -1 && (*vctxt).depth >= (*vctxt).skip_depth {
            return;
        }
        if (*(*vctxt).inode).flags & XML_SCHEMA_ELEM_INFO_EMPTY != 0 {
            (*(*vctxt).inode).flags ^= XML_SCHEMA_ELEM_INFO_EMPTY;
        }
        let len = ch.len();
        let ch = CString::new(ch).unwrap();
        if xml_schema_vpush_text(
            vctxt,
            XmlElementType::XmlCDATASectionNode as i32,
            ch.as_ptr() as *const u8,
            len as i32,
            XML_SCHEMA_PUSH_TEXT_VOLATILE,
            null_mut(),
        ) == -1
        {
            VERROR_INT!(
                vctxt,
                "xmlSchemaSAXHandleCDataSection",
                "calling xmlSchemaVPushText()"
            );
            (*vctxt).err = -1;
            (*(*vctxt).parser_ctxt).stop();
        }
    }
}

unsafe fn xml_schema_sax_handle_reference(ctx: Option<GenericErrorContext>, _name: &str) {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let vctxt = *lock.downcast_ref::<XmlSchemaValidCtxtPtr>().unwrap();

        if (*vctxt).depth < 0 {
            return;
        }
        if (*vctxt).skip_depth != -1 && (*vctxt).depth >= (*vctxt).skip_depth {
            // return;
        }
        /* SAX VAL TODO: What to do here? */
        // TODO
    }
}

// All those functions just bounces to the user provided SAX handlers
unsafe fn internal_subset_split(
    ctx: Option<GenericErrorContext>,
    name: Option<&str>,
    external_id: Option<&str>,
    system_id: Option<&str>,
) {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let ctxt = *lock.downcast_ref::<XmlSchemaSAXPlugPtr>().unwrap();
        if !ctxt.is_null() {
            if let Some(internal_subset) = (*ctxt)
                .user_sax
                .as_deref_mut()
                .and_then(|sax| sax.internal_subset)
            {
                internal_subset((*ctxt).user_data.clone(), name, external_id, system_id);
            }
        }
    }
}

unsafe fn is_standalone_split(ctx: Option<GenericErrorContext>) -> i32 {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let ctxt = *lock.downcast_ref::<XmlSchemaSAXPlugPtr>().unwrap();
        if !ctxt.is_null() {
            if let Some(is_standalone) = (*ctxt)
                .user_sax
                .as_deref_mut()
                .and_then(|sax| sax.is_standalone)
            {
                return is_standalone((*ctxt).user_data.clone());
            }
        }
        0
    }
}

unsafe fn has_internal_subset_split(ctx: Option<GenericErrorContext>) -> i32 {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let ctxt = *lock.downcast_ref::<XmlSchemaSAXPlugPtr>().unwrap();
        if !ctxt.is_null() {
            if let Some(has_internal_subset) = (*ctxt)
                .user_sax
                .as_deref_mut()
                .and_then(|sax| sax.has_internal_subset)
            {
                return has_internal_subset((*ctxt).user_data.clone());
            }
        }
        0
    }
}

unsafe fn has_external_subset_split(ctx: Option<GenericErrorContext>) -> i32 {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let ctxt = *lock.downcast_ref::<XmlSchemaSAXPlugPtr>().unwrap();
        if !ctxt.is_null() {
            if let Some(has_external_subset) = (*ctxt)
                .user_sax
                .as_deref_mut()
                .and_then(|sax| sax.has_external_subset)
            {
                return has_external_subset((*ctxt).user_data.clone());
            }
        }
        0
    }
}

unsafe fn external_subset_split(
    ctx: Option<GenericErrorContext>,
    name: Option<&str>,
    external_id: Option<&str>,
    system_id: Option<&str>,
) {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let ctxt = *lock.downcast_ref::<XmlSchemaSAXPlugPtr>().unwrap();
        if !ctxt.is_null() {
            if let Some(external_subset) = (*ctxt)
                .user_sax
                .as_deref_mut()
                .and_then(|sax| sax.external_subset)
            {
                external_subset((*ctxt).user_data.clone(), name, external_id, system_id);
            }
        }
    }
}

unsafe fn resolve_entity_split(
    ctx: Option<GenericErrorContext>,
    public_id: Option<&str>,
    system_id: Option<&str>,
) -> XmlParserInputPtr {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let ctxt = *lock.downcast_ref::<XmlSchemaSAXPlugPtr>().unwrap();
        if !ctxt.is_null() {
            if let Some(resolve_entity) = (*ctxt)
                .user_sax
                .as_deref_mut()
                .and_then(|sax| sax.resolve_entity)
            {
                return resolve_entity((*ctxt).user_data.clone(), public_id, system_id);
            }
        }
        null_mut()
    }
}

unsafe fn get_entity_split(ctx: Option<GenericErrorContext>, name: &str) -> Option<XmlEntityPtr> {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let ctxt = *lock.downcast_ref::<XmlSchemaSAXPlugPtr>().unwrap();
        if !ctxt.is_null() {
            if let Some(get_entity) = (*ctxt)
                .user_sax
                .as_deref_mut()
                .and_then(|sax| sax.get_entity)
            {
                return get_entity((*ctxt).user_data.clone(), name);
            }
        }
        None
    }
}

unsafe fn get_parameter_entity_split(
    ctx: Option<GenericErrorContext>,
    name: &str,
) -> Option<XmlEntityPtr> {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let ctxt = *lock.downcast_ref::<XmlSchemaSAXPlugPtr>().unwrap();
        if !ctxt.is_null() {
            if let Some(get_parameter_entity) = (*ctxt)
                .user_sax
                .as_deref_mut()
                .and_then(|sax| sax.get_parameter_entity)
            {
                return get_parameter_entity((*ctxt).user_data.clone(), name);
            }
        }
        None
    }
}

unsafe fn entity_decl_split(
    ctx: Option<GenericErrorContext>,
    name: &str,
    typ: XmlEntityType,
    public_id: Option<&str>,
    system_id: Option<&str>,
    content: Option<&str>,
) {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let ctxt = *lock.downcast_ref::<XmlSchemaSAXPlugPtr>().unwrap();
        if !ctxt.is_null() {
            if let Some(entity_decl) = (*ctxt)
                .user_sax
                .as_deref_mut()
                .and_then(|sax| sax.entity_decl)
            {
                entity_decl(
                    (*ctxt).user_data.clone(),
                    name,
                    typ,
                    public_id,
                    system_id,
                    content,
                );
            }
        }
    }
}

unsafe fn attribute_decl_split(
    ctx: Option<GenericErrorContext>,
    elem: &str,
    name: &str,
    typ: XmlAttributeType,
    def: XmlAttributeDefault,
    default_value: Option<&str>,
    tree: Option<Box<XmlEnumeration>>,
) {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let ctxt = *lock.downcast_ref::<XmlSchemaSAXPlugPtr>().unwrap();
        if !ctxt.is_null() {
            if let Some(attribute_decl) = (*ctxt)
                .user_sax
                .as_deref_mut()
                .and_then(|sax| sax.attribute_decl)
            {
                attribute_decl(
                    (*ctxt).user_data.clone(),
                    elem,
                    name,
                    typ,
                    def,
                    default_value,
                    tree,
                );
            }
        }
    }
}

unsafe fn element_decl_split(
    ctx: Option<GenericErrorContext>,
    name: &str,
    typ: Option<XmlElementTypeVal>,
    content: XmlElementContentPtr,
) {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let ctxt = *lock.downcast_ref::<XmlSchemaSAXPlugPtr>().unwrap();
        if !ctxt.is_null() {
            if let Some(element_decl) = (*ctxt)
                .user_sax
                .as_deref_mut()
                .and_then(|sax| sax.element_decl)
            {
                element_decl((*ctxt).user_data.clone(), name, typ, content);
            }
        }
    }
}

unsafe fn notation_decl_split(
    ctx: Option<GenericErrorContext>,
    name: &str,
    public_id: Option<&str>,
    system_id: Option<&str>,
) {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let ctxt = *lock.downcast_ref::<XmlSchemaSAXPlugPtr>().unwrap();
        if !ctxt.is_null() {
            if let Some(notation_decl) = (*ctxt)
                .user_sax
                .as_deref_mut()
                .and_then(|sax| sax.notation_decl)
            {
                notation_decl((*ctxt).user_data.clone(), name, public_id, system_id);
            }
        }
    }
}

unsafe fn unparsed_entity_decl_split(
    ctx: Option<GenericErrorContext>,
    name: &str,
    public_id: Option<&str>,
    system_id: Option<&str>,
    notation_name: Option<&str>,
) {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let ctxt = *lock.downcast_ref::<XmlSchemaSAXPlugPtr>().unwrap();
        if !ctxt.is_null() {
            if let Some(unparsed_entity_decl) = (*ctxt)
                .user_sax
                .as_deref_mut()
                .and_then(|sax| sax.unparsed_entity_decl)
            {
                unparsed_entity_decl(
                    (*ctxt).user_data.clone(),
                    name,
                    public_id,
                    system_id,
                    notation_name,
                );
            }
        }
    }
}

unsafe fn set_document_locator_split(ctx: Option<GenericErrorContext>, loc: XmlSAXLocatorPtr) {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let ctxt = *lock.downcast_ref::<XmlSchemaSAXPlugPtr>().unwrap();
        if !ctxt.is_null() {
            if let Some(set_document_locator) = (*ctxt)
                .user_sax
                .as_deref_mut()
                .and_then(|sax| sax.set_document_locator)
            {
                set_document_locator((*ctxt).user_data.clone(), loc);
            }
        }
    }
}

unsafe fn start_document_split(ctx: Option<GenericErrorContext>) {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let ctxt = *lock.downcast_ref::<XmlSchemaSAXPlugPtr>().unwrap();
        if !ctxt.is_null() {
            if let Some(start_document) = (*ctxt)
                .user_sax
                .as_deref_mut()
                .and_then(|sax| sax.start_document)
            {
                start_document((*ctxt).user_data.clone());
            }
        }
    }
}

unsafe fn end_document_split(ctx: Option<GenericErrorContext>) {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let ctxt = *lock.downcast_ref::<XmlSchemaSAXPlugPtr>().unwrap();
        if !ctxt.is_null() {
            if let Some(end_document) = (*ctxt)
                .user_sax
                .as_deref_mut()
                .and_then(|sax| sax.end_document)
            {
                end_document((*ctxt).user_data.clone());
            }
        }
    }
}

unsafe fn processing_instruction_split(
    ctx: Option<GenericErrorContext>,
    target: &str,
    data: Option<&str>,
) {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let ctxt = *lock.downcast_ref::<XmlSchemaSAXPlugPtr>().unwrap();
        if !ctxt.is_null() {
            if let Some(processing_instruction) = (*ctxt)
                .user_sax
                .as_deref_mut()
                .and_then(|sax| sax.processing_instruction)
            {
                processing_instruction((*ctxt).user_data.clone(), target, data);
            }
        }
    }
}

unsafe fn comment_split(ctx: Option<GenericErrorContext>, value: &str) {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let ctxt = *lock.downcast_ref::<XmlSchemaSAXPlugPtr>().unwrap();
        if !ctxt.is_null() {
            if let Some(comment) = (*ctxt).user_sax.as_deref_mut().and_then(|sax| sax.comment) {
                comment((*ctxt).user_data.clone(), value);
            }
        }
    }
}

// Varargs error callbacks to the user application, harder ...

fn warning_split(_ctx: Option<GenericErrorContext>, _msg: &str) {
    // let ctxt: XmlSchemaSAXPlugPtr = ctx as XmlSchemaSAXPlugPtr;
    // if !ctxt.is_null() && !(*ctxt).user_sax.is_null() && (*(*ctxt).user_sax).warning.is_some() {
    //     // TODO
    // }
}
fn error_split(_ctx: Option<GenericErrorContext>, _msg: &str) {
    // let ctxt: XmlSchemaSAXPlugPtr = ctx as XmlSchemaSAXPlugPtr;
    // if !ctxt.is_null() && !(*ctxt).user_sax.is_null() && (*(*ctxt).user_sax).error.is_some() {
    //     // TODO
    // }
}
fn fatal_error_split(_ctx: Option<GenericErrorContext>, _msg: &str) {
    // let ctxt: XmlSchemaSAXPlugPtr = ctx as XmlSchemaSAXPlugPtr;
    // if !ctxt.is_null() && !(*ctxt).user_sax.is_null() && (*(*ctxt).user_sax).fatal_error.is_some() {
    //     // TODO
    // }
}

// Those are function where both the user handler and the schemas handler need to be called.
unsafe fn characters_split(ctx: Option<GenericErrorContext>, ch: &str) {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let ctxt = *lock.downcast_ref::<XmlSchemaSAXPlugPtr>().unwrap();
        if ctxt.is_null() {
            return;
        }
        if let Some(characters) = (*ctxt)
            .user_sax
            .as_deref_mut()
            .and_then(|sax| sax.characters)
        {
            characters((*ctxt).user_data.clone(), ch);
        }
        if !(*ctxt).ctxt.is_null() {
            xml_schema_sax_handle_text(Some(GenericErrorContext::new((*ctxt).ctxt)), ch);
        }
    }
}

unsafe fn ignorable_whitespace_split(ctx: Option<GenericErrorContext>, ch: &str) {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let ctxt = *lock.downcast_ref::<XmlSchemaSAXPlugPtr>().unwrap();
        if ctxt.is_null() {
            return;
        }
        if let Some(ignorable_whitespace) = (*ctxt)
            .user_sax
            .as_deref_mut()
            .and_then(|sax| sax.ignorable_whitespace)
        {
            ignorable_whitespace((*ctxt).user_data.clone(), ch);
        }
        if !(*ctxt).ctxt.is_null() {
            xml_schema_sax_handle_text(Some(GenericErrorContext::new((*ctxt).ctxt)), ch);
        }
    }
}

unsafe fn cdata_block_split(ctx: Option<GenericErrorContext>, value: &str) {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let ctxt = *lock.downcast_ref::<XmlSchemaSAXPlugPtr>().unwrap();
        if ctxt.is_null() {
            return;
        }
        if let Some(cdata_block) = (*ctxt)
            .user_sax
            .as_deref_mut()
            .and_then(|sax| sax.cdata_block)
        {
            cdata_block((*ctxt).user_data.clone(), value);
        }
        if !(*ctxt).ctxt.is_null() {
            xml_schema_sax_handle_cdata_section(
                Some(GenericErrorContext::new((*ctxt).ctxt)),
                value,
            );
        }
    }
}

unsafe fn reference_split(ctx: Option<GenericErrorContext>, name: &str) {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let ctxt = *lock.downcast_ref::<XmlSchemaSAXPlugPtr>().unwrap();
        if ctxt.is_null() {
            return;
        }
        if let Some(reference) = (*ctxt)
            .user_sax
            .as_deref_mut()
            .and_then(|sax| sax.reference)
        {
            reference((*ctxt).user_data.clone(), name);
        }
        if !(*ctxt).ctxt.is_null() {
            xml_schema_sax_handle_reference((*ctxt).user_data.clone(), name);
        }
    }
}

unsafe fn start_element_ns_split(
    ctx: Option<GenericErrorContext>,
    localname: &str,
    prefix: Option<&str>,
    uri: Option<&str>,
    namespaces: &[(Option<String>, String)],
    nb_defaulted: usize,
    attributes: &[(String, Option<String>, Option<String>, String)],
) {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let ctxt = *lock.downcast_ref::<XmlSchemaSAXPlugPtr>().unwrap();
        if ctxt.is_null() {
            return;
        }
        if let Some(start_element_ns) = (*ctxt)
            .user_sax
            .as_deref_mut()
            .and_then(|sax| sax.start_element_ns)
        {
            start_element_ns(
                (*ctxt).user_data.clone(),
                localname,
                prefix,
                uri,
                namespaces,
                nb_defaulted,
                attributes,
            );
        }
        if !(*ctxt).ctxt.is_null() {
            xml_schema_sax_handle_start_element_ns(
                Some(GenericErrorContext::new((*ctxt).ctxt)),
                localname,
                prefix,
                uri,
                namespaces,
                nb_defaulted,
                attributes,
            );
        }
    }
}

unsafe fn end_element_ns_split(
    ctx: Option<GenericErrorContext>,
    localname: &str,
    prefix: Option<&str>,
    uri: Option<&str>,
) {
    unsafe {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        let ctxt = *lock.downcast_ref::<XmlSchemaSAXPlugPtr>().unwrap();
        if ctxt.is_null() {
            return;
        }
        if let Some(end_element_ns) = (*ctxt)
            .user_sax
            .as_deref_mut()
            .and_then(|sax| sax.end_element_ns)
        {
            end_element_ns((*ctxt).user_data.clone(), localname, prefix, uri);
        }
        if !(*ctxt).ctxt.is_null() {
            xml_schema_sax_handle_end_element_ns(
                Some(GenericErrorContext::new((*ctxt).ctxt)),
                localname,
                prefix,
                uri,
            );
        }
    }
}

/// Plug a SAX based validation layer in a SAX parsing event flow.
/// The original @saxptr and @dataptr data are replaced by new pointers
/// but the calls to the original will be maintained.
///
/// Returns a pointer to a data structure needed to unplug the validation layer
/// or NULL in case of errors.
#[doc(alias = "xmlSchemaSAXPlug")]
pub unsafe fn xml_schema_sax_plug(
    ctxt: XmlSchemaValidCtxtPtr,
    sax: &mut Option<Box<XmlSAXHandler>>,
    user_data: *mut Option<GenericErrorContext>,
) -> XmlSchemaSAXPlugPtr {
    unsafe {
        if ctxt.is_null() || user_data.is_null() {
            return null_mut();
        }

        // We only allow to plug into SAX2 event streams
        let mut old_sax = sax.take();
        if old_sax
            .as_deref()
            .is_some_and(|sax| sax.initialized != XML_SAX2_MAGIC as u32)
        {
            return null_mut();
        }
        if old_sax.as_deref().is_some_and(|sax| {
            sax.start_element_ns.is_none()
                && sax.end_element_ns.is_none()
                && (sax.start_element.is_some() || sax.end_element.is_some())
        }) {
            return null_mut();
        }

        // everything seems right allocate the local data needed for that layer
        let ret: XmlSchemaSAXPlugPtr =
            xml_malloc(size_of::<XmlSchemaSAXPlugStruct>()) as XmlSchemaSAXPlugPtr;
        if ret.is_null() {
            return null_mut();
        }
        memset(ret as _, 0, size_of::<XmlSchemaSAXPlugStruct>());
        (*ret).magic = XML_SAX_PLUG_MAGIC as _;
        (*ret).schemas_sax.initialized = XML_SAX2_MAGIC as _;
        (*ret).ctxt = ctxt;
        // (*ret).user_sax_ptr = sax;
        if let Some(old_sax) = old_sax.as_deref_mut() {
            // for each callback unused by Schemas initialize it to the Split
            // routine only if non NULL in the user block, this can speed up
            // things at the SAX level.
            if old_sax.internal_subset.is_some() {
                (*ret).schemas_sax.internal_subset = Some(internal_subset_split);
            }
            if old_sax.is_standalone.is_some() {
                (*ret).schemas_sax.is_standalone = Some(is_standalone_split);
            }
            if old_sax.has_internal_subset.is_some() {
                (*ret).schemas_sax.has_internal_subset = Some(has_internal_subset_split);
            }
            if old_sax.has_external_subset.is_some() {
                (*ret).schemas_sax.has_external_subset = Some(has_external_subset_split);
            }
            if old_sax.resolve_entity.is_some() {
                (*ret).schemas_sax.resolve_entity = Some(resolve_entity_split);
            }
            if old_sax.get_entity.is_some() {
                (*ret).schemas_sax.get_entity = Some(get_entity_split);
            }
            if old_sax.entity_decl.is_some() {
                (*ret).schemas_sax.entity_decl = Some(entity_decl_split);
            }
            if old_sax.notation_decl.is_some() {
                (*ret).schemas_sax.notation_decl = Some(notation_decl_split);
            }
            if old_sax.attribute_decl.is_some() {
                (*ret).schemas_sax.attribute_decl = Some(attribute_decl_split);
            }
            if old_sax.element_decl.is_some() {
                (*ret).schemas_sax.element_decl = Some(element_decl_split);
            }
            if old_sax.unparsed_entity_decl.is_some() {
                (*ret).schemas_sax.unparsed_entity_decl = Some(unparsed_entity_decl_split);
            }
            if old_sax.set_document_locator.is_some() {
                (*ret).schemas_sax.set_document_locator = Some(set_document_locator_split);
            }
            if old_sax.start_document.is_some() {
                (*ret).schemas_sax.start_document = Some(start_document_split);
            }
            if old_sax.end_document.is_some() {
                (*ret).schemas_sax.end_document = Some(end_document_split);
            }
            if old_sax.processing_instruction.is_some() {
                (*ret).schemas_sax.processing_instruction = Some(processing_instruction_split);
            }
            if old_sax.comment.is_some() {
                (*ret).schemas_sax.comment = Some(comment_split);
            }
            if old_sax.warning.is_some() {
                (*ret).schemas_sax.warning = Some(warning_split);
            }
            if old_sax.error.is_some() {
                (*ret).schemas_sax.error = Some(error_split);
            }
            if old_sax.fatal_error.is_some() {
                (*ret).schemas_sax.fatal_error = Some(fatal_error_split);
            }
            if old_sax.get_parameter_entity.is_some() {
                (*ret).schemas_sax.get_parameter_entity = Some(get_parameter_entity_split);
            }
            if old_sax.external_subset.is_some() {
                (*ret).schemas_sax.external_subset = Some(external_subset_split);
            }

            // the 6 schemas callback have to go to the splitter functions
            // Note that we use the same text-function for ignorableWhitespace
            // if possible, to prevent the parser from testing for ignorable whitespace.
            (*ret).schemas_sax.characters = Some(characters_split);
            if (old_sax.ignorable_whitespace.is_some() || old_sax.characters.is_some())
                && old_sax
                    .ignorable_whitespace
                    .zip(old_sax.characters)
                    .is_none_or(|(l, r)| !fn_addr_eq(l, r))
            {
                (*ret).schemas_sax.ignorable_whitespace = Some(ignorable_whitespace_split);
            } else {
                (*ret).schemas_sax.ignorable_whitespace = Some(characters_split);
            }
            (*ret).schemas_sax.cdata_block = Some(cdata_block_split);
            (*ret).schemas_sax.reference = Some(reference_split);
            (*ret).schemas_sax.start_element_ns = Some(start_element_ns_split);
            (*ret).schemas_sax.end_element_ns = Some(end_element_ns_split);

            (*ret).user_data_ptr = user_data;
            (*ret).user_data = (*user_data).clone();
            *user_data = Some(GenericErrorContext::new(ret));
        } else {
            // go direct, no need for the split block and functions.
            (*ret).schemas_sax.start_element_ns = Some(xml_schema_sax_handle_start_element_ns);
            (*ret).schemas_sax.end_element_ns = Some(xml_schema_sax_handle_end_element_ns);
            // Note that we use the same text-function for both, to prevent
            // the parser from testing for ignorable whitespace.
            (*ret).schemas_sax.ignorable_whitespace = Some(xml_schema_sax_handle_text);
            (*ret).schemas_sax.characters = Some(xml_schema_sax_handle_text);

            (*ret).schemas_sax.cdata_block = Some(xml_schema_sax_handle_cdata_section);
            (*ret).schemas_sax.reference = Some(xml_schema_sax_handle_reference);

            (*ret).user_data = Some(GenericErrorContext::new(ctxt));
            *user_data = (*ret).user_data.clone();
        }
        (*ret).user_sax = old_sax;

        // plug the pointers back.
        *sax = Some(Box::new(take(&mut (*ret).schemas_sax)));
        // (*ctxt).sax = *sax;
        (*ctxt).flags |= XML_SCHEMA_VALID_CTXT_FLAG_STREAM;
        xml_schema_pre_run(ctxt);
        ret
    }
}

/// Unplug a SAX based validation layer in a SAX parsing event flow.
/// The original pointers used in the call are restored.
///
/// If unplug successfully, return the original SAX handler and user data wrapped `Ok`.  
/// Otherwise, return `Err`.
///
/// - the original SAX handler.
/// - the original user data.
/// - 0 in case of success and -1 in case of failure.
#[doc(alias = "xmlSchemaSAXUnplug")]
pub unsafe fn xml_schema_sax_unplug(
    plug: XmlSchemaSAXPlugPtr,
) -> Result<(Option<Box<XmlSAXHandler>>, Option<GenericErrorContext>), i32> {
    unsafe {
        if plug.is_null() || (*plug).magic != XML_SAX_PLUG_MAGIC {
            return Err(-1);
        }
        (*plug).magic = 0;

        xml_schema_post_run((*plug).ctxt);
        // restore the data
        let user_sax = (*plug).user_sax.take();
        let user_data = (*plug).user_data.clone();

        // free and return
        xml_free(plug as _);
        Ok((user_sax, user_data))
    }
}

/// Allows to set a locator function to the validation context,
/// which will be used to provide file and line information since
/// those are not provided as part of the SAX validation flow
/// Setting @f to NULL disable the locator.
#[doc(alias = "xmlSchemaValidateSetLocator")]
pub unsafe fn xml_schema_validate_set_locator(
    vctxt: XmlSchemaValidCtxtPtr,
    f: Option<XmlSchemaValidityLocatorFunc>,
    ctxt: *mut c_void,
) {
    unsafe {
        if vctxt.is_null() {
            return;
        }
        (*vctxt).loc_func = f;
        (*vctxt).loc_ctxt = ctxt;
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        globals::reset_last_error,
        libxml::xmlmemory::xml_mem_blocks,
        test_util::*,
        xmlschemas::{context::xml_schema_new_mem_parser_ctxt, dump::xml_schema_dump},
    };

    use super::*;

    #[test]
    fn test_xml_schema_dump() {
        #[cfg(all(feature = "schema", feature = "libxml_output"))]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_FILE_PTR {
                for n_schema in 0..GEN_NB_XML_SCHEMA_PTR {
                    let mem_base = xml_mem_blocks();
                    let mut output = gen_file_ptr(n_output, 0).unwrap();
                    let schema = gen_xml_schema_ptr(n_schema, 1);

                    xml_schema_dump(&mut output, schema);
                    des_xml_schema_ptr(n_schema, schema, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlSchemaDump",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlSchemaDump()");
                        eprint!(" {}", n_output);
                        eprintln!(" {}", n_schema);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_schema_is_valid() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SCHEMA_VALID_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_schema_valid_ctxt_ptr(n_ctxt, 0);

                let ret_val = xml_schema_is_valid(ctxt);
                desret_int(ret_val);
                des_xml_schema_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlSchemaIsValid",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlSchemaIsValid()");
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_schema_new_mem_parser_ctxt() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_buffer in 0..GEN_NB_CONST_CHAR_PTR {
                for n_size in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let buffer = gen_const_char_ptr(n_buffer, 0);
                    let mut size = gen_int(n_size, 1);
                    if !buffer.is_null() && size > xml_strlen(buffer as _) {
                        size = 0;
                    }

                    let ret_val = xml_schema_new_mem_parser_ctxt(buffer, size);
                    desret_xml_schema_parser_ctxt_ptr(ret_val);
                    des_const_char_ptr(n_buffer, buffer, 0);
                    des_int(n_size, size, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlSchemaNewMemParserCtxt",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlSchemaNewMemParserCtxt()"
                        );
                        eprint!(" {}", n_buffer);
                        eprintln!(" {}", n_size);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_schema_set_valid_options() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SCHEMA_VALID_CTXT_PTR {
                for n_options in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_schema_valid_ctxt_ptr(n_ctxt, 0);
                    let options = gen_int(n_options, 1);

                    let ret_val = xml_schema_set_valid_options(ctxt, options);
                    desret_int(ret_val);
                    des_xml_schema_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_int(n_options, options, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlSchemaSetValidOptions",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlSchemaSetValidOptions()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_options);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_schema_valid_ctxt_get_options() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SCHEMA_VALID_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_schema_valid_ctxt_ptr(n_ctxt, 0);

                let ret_val = xml_schema_valid_ctxt_get_options(ctxt);
                desret_int(ret_val);
                des_xml_schema_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlSchemaValidCtxtGetOptions",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlSchemaValidCtxtGetOptions()"
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_schema_valid_ctxt_get_parser_ctxt() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SCHEMA_VALID_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_schema_valid_ctxt_ptr(n_ctxt, 0);

                let ret_val = xml_schema_valid_ctxt_get_parser_ctxt(ctxt);
                desret_xml_parser_ctxt_ptr(ret_val);
                des_xml_schema_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlSchemaValidCtxtGetParserCtxt",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlSchemaValidCtxtGetParserCtxt()"
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_schema_validate_file() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SCHEMA_VALID_CTXT_PTR {
                for n_filename in 0..GEN_NB_FILEPATH {
                    for n_options in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let ctxt = gen_xml_schema_valid_ctxt_ptr(n_ctxt, 0);
                        let filename = gen_filepath(n_filename, 1);
                        let options = gen_int(n_options, 2);

                        let ret_val = xml_schema_validate_file(ctxt, filename, options);
                        desret_int(ret_val);
                        des_xml_schema_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                        des_filepath(n_filename, filename, 1);
                        des_int(n_options, options, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlSchemaValidateFile",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlSchemaValidateFile()"
                            );
                            eprint!(" {}", n_ctxt);
                            eprint!(" {}", n_filename);
                            eprintln!(" {}", n_options);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_schema_validate_set_filename() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_vctxt in 0..GEN_NB_XML_SCHEMA_VALID_CTXT_PTR {
                for n_filename in 0..GEN_NB_FILEPATH {
                    let mem_base = xml_mem_blocks();
                    let vctxt = gen_xml_schema_valid_ctxt_ptr(n_vctxt, 0);
                    let filename = gen_filepath(n_filename, 1);

                    xml_schema_validate_set_filename(vctxt, filename);
                    des_xml_schema_valid_ctxt_ptr(n_vctxt, vctxt, 0);
                    des_filepath(n_filename, filename, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlSchemaValidateSetFilename",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlSchemaValidateSetFilename()"
                        );
                        eprint!(" {}", n_vctxt);
                        eprintln!(" {}", n_filename);
                    }
                }
            }
        }
    }
}
