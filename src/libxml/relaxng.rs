//! Provide methods and data structures for parsing RelaxNG schemas.  
//! This module is based on `libxml/relaxng.h`, `relaxng.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    any::type_name,
    ffi::{c_char, CStr},
    mem::{size_of, size_of_val, zeroed},
    os::raw::c_void,
    ptr::{addr_of_mut, null_mut},
    slice::from_raw_parts,
    sync::atomic::{AtomicBool, AtomicPtr, Ordering},
};

use libc::{fprintf, memcpy, memset, ptrdiff_t, snprintf, FILE};

use crate::{
    __xml_raise_error,
    error::XmlParserErrors,
    generic_error,
    globals::{GenericError, GenericErrorContext, StructuredError, GLOBAL_STATE},
    libxml::{
        globals::{xml_free, xml_malloc, xml_malloc_atomic, xml_realloc},
        hash::{
            xml_hash_add_entry, xml_hash_add_entry2, xml_hash_create, xml_hash_free,
            xml_hash_lookup, xml_hash_lookup2, xml_hash_scan, XmlHashTable, XmlHashTablePtr,
        },
        parser::{xml_read_file, xml_read_memory},
        schemas_internals::{XmlSchemaFacetPtr, XmlSchemaTypePtr, XmlSchemaTypeType},
        uri::{xml_build_uri, xml_free_uri, xml_parse_uri, xml_uri_escape_str, XmlURIPtr},
        valid::{xml_validate_document_final, XmlValidCtxt},
        xmlautomata::{
            xml_automata_compile, xml_automata_get_init_state, xml_automata_is_determinist,
            xml_automata_new_epsilon, xml_automata_new_transition, xml_automata_new_transition2,
            xml_automata_set_final_state, xml_automata_set_flags, xml_free_automata,
            xml_new_automata, XmlAutomataPtr, XmlAutomataStatePtr,
        },
        xmlregexp::{
            xml_reg_exec_push_string, xml_reg_exec_push_string2, xml_reg_free_exec_ctxt,
            xml_reg_free_regexp, xml_reg_new_exec_ctxt, xml_regexp_is_determinist,
            XmlRegExecCtxtPtr, XmlRegexpPtr,
        },
        xmlschemastypes::{
            xml_schema_check_facet, xml_schema_cleanup_types, xml_schema_compare_values,
            xml_schema_free_facet, xml_schema_free_value, xml_schema_get_predefined_type,
            xml_schema_new_facet, xml_schema_val_predef_type_node, xml_schema_validate_facet,
            XmlSchemaValPtr,
        },
        xmlstring::{
            xml_char_strdup, xml_escape_format_string, xml_str_equal, xml_strcat, xml_strdup,
            xml_strlen, XmlChar,
        },
    },
    tree::{
        xml_add_child, xml_add_next_sibling, xml_copy_doc, xml_free_doc, xml_free_node,
        xml_get_prop, xml_has_prop, xml_new_child, xml_new_doc_node, xml_new_doc_text,
        xml_node_get_base, xml_node_get_content, xml_node_list_get_string, xml_node_set_content,
        xml_search_ns, xml_set_prop, xml_split_qname2, xml_unlink_node, xml_unset_prop,
        xml_validate_ncname, XmlAttrPtr, XmlDocPtr, XmlElementType, XmlNode, XmlNodePtr, XmlNs,
        XmlNsPtr,
    },
};

use super::{chvalid::xml_is_blank_char, hash::CVoidWrapper};

/**
 * xmlRelaxNGValidityErrorFunc:
 * @ctx: the validation context
 * @msg: the message
 * @...: extra arguments
 *
 * Signature of an error callback from a Relax-NG validation
 */
pub type XmlRelaxNGValidityErrorFunc = unsafe extern "C" fn(ctx: *mut c_void, msg: *const c_char);

/**
 * xmlRelaxNGValidityWarningFunc:
 * @ctx: the validation context
 * @msg: the message
 * @...: extra arguments
 *
 * Signature of a warning callback from a Relax-NG validation
 */
pub type XmlRelaxNGValidityWarningFunc = unsafe extern "C" fn(ctx: *mut c_void, msg: *const c_char);

/*
 * xmlRelaxNGValidErr:
 *
 * List of possible Relax NG validation errors
 */
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlRelaxNGValidErr {
    XmlRelaxngOk = 0,
    XmlRelaxngErrMemory,
    XmlRelaxngErrType,
    XmlRelaxngErrTypeval,
    XmlRelaxngErrDupid,
    XmlRelaxngErrTypecmp,
    XmlRelaxngErrNostate,
    XmlRelaxngErrNodefine,
    XmlRelaxngErrListextra,
    XmlRelaxngErrListempty,
    XmlRelaxngErrInternodata,
    XmlRelaxngErrInterseq,
    XmlRelaxngErrInterextra,
    XmlRelaxngErrElemname,
    XmlRelaxngErrAttrname,
    XmlRelaxngErrElemnons,
    XmlRelaxngErrAttrnons,
    XmlRelaxngErrElemwrongns,
    XmlRelaxngErrAttrwrongns,
    XmlRelaxngErrElemextrans,
    XmlRelaxngErrAttrextrans,
    XmlRelaxngErrElemnotempty,
    XmlRelaxngErrNoelem,
    XmlRelaxngErrNotelem,
    XmlRelaxngErrAttrvalid,
    XmlRelaxngErrContentvalid,
    XmlRelaxngErrExtracontent,
    XmlRelaxngErrInvalidattr,
    XmlRelaxngErrDataelem,
    XmlRelaxngErrValelem,
    XmlRelaxngErrListelem,
    XmlRelaxngErrDatatype,
    XmlRelaxngErrValue,
    XmlRelaxngErrList,
    XmlRelaxngErrNogrammar,
    XmlRelaxngErrExtradata,
    XmlRelaxngErrLackdata,
    XmlRelaxngErrInternal,
    XmlRelaxngErrElemwrong,
    XmlRelaxngErrTextwrong,
}

/*
 * xmlRelaxNGParserFlags:
 *
 * List of possible Relax NG Parser flags
 */
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum XmlRelaxNGParserFlag {
    #[allow(unused)]
    None = 0,
    FreeDoc = 1,
    Crng = 2,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum XmlRelaxNGCombine {
    #[allow(unused)]
    Undefined = 0, /* undefined */
    #[allow(unused)]
    Choice, /* choice */
    #[allow(unused)]
    Interleave, /* interleave */
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum XmlRelaxNGContentType {
    Error = -1,
    Empty = 0,
    Simple,
    Complex,
}

impl TryFrom<i16> for XmlRelaxNGContentType {
    type Error = anyhow::Error;
    fn try_from(value: i16) -> Result<Self, anyhow::Error> {
        if value == Self::Error as i16 {
            Ok(Self::Error)
        } else if value == Self::Empty as i16 {
            Ok(Self::Empty)
        } else if value == Self::Simple as i16 {
            Ok(Self::Simple)
        } else if value == Self::Complex as i16 {
            Ok(Self::Complex)
        } else {
            Err(anyhow::anyhow!(
                "Invalid convert from value '{value}' to {}",
                type_name::<Self>()
            ))
        }
    }
}

pub type XmlRelaxNGGrammarPtr = *mut XmlRelaxNGGrammar;

#[repr(C)]
pub struct XmlRelaxNGGrammar {
    parent: XmlRelaxNGGrammarPtr,    /* the parent grammar if any */
    children: XmlRelaxNGGrammarPtr,  /* the children grammar if any */
    next: XmlRelaxNGGrammarPtr,      /* the next grammar if any */
    start: XmlRelaxNGDefinePtr,      /* <start> content */
    combine: XmlRelaxNGCombine,      /* the default combine value */
    start_list: XmlRelaxNGDefinePtr, /* list of <start> definitions */
    defs: XmlHashTablePtr,           /* define* */
    refs: XmlHashTablePtr,           /* references */
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum XmlRelaxNGType {
    Noop = -1,   /* a no operation from simplification  */
    Empty = 0,   /* an empty pattern */
    NotAllowed,  /* not allowed top */
    Except,      /* except present in nameclass defs */
    Text,        /* textual content */
    Element,     /* an element */
    Datatype,    /* external data type definition */
    Param,       /* external data type parameter */
    Value,       /* value from an external data type definition */
    List,        /* a list of patterns */
    Attribute,   /* an attribute following a pattern */
    Def,         /* a definition */
    Ref,         /* reference to a definition */
    Externalref, /* reference to an external def */
    Parentref,   /* reference to a def in the parent grammar */
    Optional,    /* optional patterns */
    Zeroormore,  /* zero or more non empty patterns */
    Oneormore,   /* one or more non empty patterns */
    Choice,      /* a choice between non empty patterns */
    Group,       /* a pair/group of non empty patterns */
    Interleave,  /* interleaving choice of non-empty patterns */
    Start,       /* Used to keep track of starts on grammars */
}

const IS_NULLABLE: i32 = 1 << 0;
const IS_NOT_NULLABLE: i32 = 1 << 1;
const IS_INDETERMINIST: i32 = 1 << 2;
const IS_MIXED: i32 = 1 << 3;
const IS_TRIABLE: i32 = 1 << 4;
const IS_PROCESSED: i32 = 1 << 5;
const IS_COMPILABLE: i32 = 1 << 6;
const IS_NOT_COMPILABLE: i32 = 1 << 7;
const IS_EXTERNAL_REF: i32 = 1 << 8;

pub type XmlRelaxNGDefinePtr = *mut XmlRelaxNGDefine;

#[repr(C)]
pub struct XmlRelaxNGDefine {
    typ: XmlRelaxNGType,             /* the type of definition */
    node: XmlNodePtr,                /* the node in the source */
    name: *mut XmlChar,              /* the element local name if present */
    ns: *mut XmlChar,                /* the namespace local name if present */
    value: *mut XmlChar,             /* value when available */
    data: *mut c_void,               /* data lib or specific pointer */
    content: XmlRelaxNGDefinePtr,    /* the expected content */
    parent: XmlRelaxNGDefinePtr,     /* the parent definition, if any */
    next: XmlRelaxNGDefinePtr,       /* list within grouping sequences */
    attrs: XmlRelaxNGDefinePtr,      /* list of attributes for elements */
    name_class: XmlRelaxNGDefinePtr, /* the nameClass definition if any */
    next_hash: XmlRelaxNGDefinePtr,  /* next define in defs/refs hash tables */
    depth: i16,                      /* used for the cycle detection */
    dflags: i16,                     /* define related flags */
    cont_model: XmlRegexpPtr,        /* a compiled content model if available */
}

/**
 * xmlRelaxNG:
 *
 * A RelaxNGs definition
 */
pub type XmlRelaxNGPtr = *mut XmlRelaxNG;
#[repr(C)]
pub struct XmlRelaxNG {
    _private: *mut c_void, /* unused by the library for users or bindings */
    topgrammar: XmlRelaxNGGrammarPtr,
    doc: XmlDocPtr,

    idref: i32, /* requires idref checking */

    defs: XmlHashTablePtr,             /* define */
    refs: XmlHashTablePtr,             /* references */
    documents: XmlRelaxNGDocumentPtr,  /* all the documents loaded */
    includes: XmlRelaxNGIncludePtr,    /* all the includes loaded */
    def_nr: i32,                       /* number of defines used */
    def_tab: *mut XmlRelaxNGDefinePtr, /* pointer to the allocated definitions */
}

const XML_RELAXNG_IN_ATTRIBUTE: i32 = 1 << 0;
const XML_RELAXNG_IN_ONEORMORE: i32 = 1 << 1;
const XML_RELAXNG_IN_LIST: i32 = 1 << 2;
const XML_RELAXNG_IN_DATAEXCEPT: i32 = 1 << 3;
const XML_RELAXNG_IN_START: i32 = 1 << 4;
const XML_RELAXNG_IN_OOMGROUP: i32 = 1 << 5;
const XML_RELAXNG_IN_OOMINTERLEAVE: i32 = 1 << 6;
const XML_RELAXNG_IN_EXTERNALREF: i32 = 1 << 7;
const XML_RELAXNG_IN_ANYEXCEPT: i32 = 1 << 8;
const XML_RELAXNG_IN_NSEXCEPT: i32 = 1 << 9;

pub type XmlRelaxNGParserCtxtPtr = *mut XmlRelaxNGParserCtxt;
#[repr(C)]
pub struct XmlRelaxNGParserCtxt {
    user_data: Option<GenericErrorContext>, /* user specific data block */
    error: Option<GenericError>,            /* the callback in case of errors */
    warning: Option<GenericError>,          /* the callback in case of warning */
    serror: Option<StructuredError>,
    err: XmlRelaxNGValidErr,

    schema: XmlRelaxNGPtr,               /* The schema in use */
    grammar: XmlRelaxNGGrammarPtr,       /* the current grammar */
    parentgrammar: XmlRelaxNGGrammarPtr, /* the parent grammar */
    flags: i32,                          /* parser flags */
    nb_errors: i32,                      /* number of errors at parse time */
    nb_warnings: i32,                    /* number of warnings at parse time */
    define: *const XmlChar,              /* the current define scope */
    def: XmlRelaxNGDefinePtr,            /* the current define */

    nb_interleaves: i32,
    interleaves: XmlHashTablePtr, /* keep track of all the interleaves */

    documents: XmlRelaxNGDocumentPtr, /* all the documents loaded */
    includes: XmlRelaxNGIncludePtr,   /* all the includes loaded */
    url: *mut XmlChar,
    document: XmlDocPtr,

    def_nr: i32,                       /* number of defines used */
    def_max: i32,                      /* number of defines allocated */
    def_tab: *mut XmlRelaxNGDefinePtr, /* pointer to the allocated definitions */

    buffer: *const c_char,
    size: i32,

    /* the document stack */
    doc: XmlRelaxNGDocumentPtr,          /* Current parsed external ref */
    doc_nr: i32,                         /* Depth of the parsing stack */
    doc_max: i32,                        /* Max depth of the parsing stack */
    doc_tab: *mut XmlRelaxNGDocumentPtr, /* array of docs */

    /* the include stack */
    inc: XmlRelaxNGIncludePtr,          /* Current parsed include */
    inc_nr: i32,                        /* Depth of the include parsing stack */
    inc_max: i32,                       /* Max depth of the parsing stack */
    inc_tab: *mut XmlRelaxNGIncludePtr, /* array of incs */

    idref: i32, /* requires idref checking */

    /* used to compile content models */
    am: XmlAutomataPtr,         /* the automata */
    state: XmlAutomataStatePtr, /* used to build the automata */

    crng: i32,    /* compact syntax and other flags */
    freedoc: i32, /* need to free the document */
}

const FLAGS_IGNORABLE: i32 = 1;
const FLAGS_NEGATIVE: i32 = 2;
const FLAGS_MIXED_CONTENT: i32 = 4;
const FLAGS_NOERROR: i32 = 8;

/**
 * xmlRelaxNGInterleaveGroup:
 *
 * A RelaxNGs partition set associated to lists of definitions
 */
pub type XmlRelaxNGInterleaveGroupPtr = *mut XmlRelaxNGInterleaveGroup;
#[repr(C)]
pub struct XmlRelaxNGInterleaveGroup {
    rule: XmlRelaxNGDefinePtr,       /* the rule to satisfy */
    defs: *mut XmlRelaxNGDefinePtr,  /* the array of element definitions */
    attrs: *mut XmlRelaxNGDefinePtr, /* the array of attributes definitions */
}

const IS_DETERMINIST: i32 = 1;
const IS_NEEDCHECK: i32 = 2;

/**
 * xmlRelaxNGPartitions:
 *
 * A RelaxNGs partition associated to an interleave group
 */
pub type XmlRelaxNGPartitionPtr = *mut XmlRelaxNGPartition;
#[repr(C)]
pub struct XmlRelaxNGPartition {
    nbgroups: i32, /* number of groups in the partitions */
    triage: XmlHashTablePtr, /* hash table used to direct nodes to the
                    * right group when possible */
    flags: i32, /* determinist ? */
    groups: *mut XmlRelaxNGInterleaveGroupPtr,
}

/**
 * xmlRelaxNGValidState:
 *
 * A RelaxNGs validation state
 */
const MAX_ATTR: usize = 20;
pub type XmlRelaxNGValidStatePtr = *mut XmlRelaxNGValidState;
#[repr(C)]
pub struct XmlRelaxNGValidState {
    node: XmlNodePtr,       /* the current node */
    seq: XmlNodePtr,        /* the sequence of children left to validate */
    nb_attrs: i32,          /* the number of attributes */
    max_attrs: i32,         /* the size of attrs */
    nb_attr_left: i32,      /* the number of attributes left to validate */
    value: *mut XmlChar,    /* the value when operating on string */
    endvalue: *mut XmlChar, /* the end value when operating on string */
    attrs: *mut XmlAttrPtr, /* the array of attributes */
}

/**
 * xmlRelaxNGStates:
 *
 * A RelaxNGs container for validation state
 */
pub type XmlRelaxNGStatesPtr = *mut XmlRelaxNGStates;
#[repr(C)]
pub struct XmlRelaxNGStates {
    nb_state: i32,  /* the number of states */
    max_state: i32, /* the size of the array */
    tab_state: *mut XmlRelaxNGValidStatePtr,
}

const ERROR_IS_DUP: i32 = 1;

/**
 * xmlRelaxNGValidError:
 *
 * A RelaxNGs validation error
 */
pub type XmlRelaxNGValidErrorPtr = *mut XmlRelaxNGValidError;
#[repr(C)]
pub struct XmlRelaxNGValidError {
    err: XmlRelaxNGValidErr, /* the error number */
    flags: i32,              /* flags */
    node: XmlNodePtr,        /* the current node */
    seq: XmlNodePtr,         /* the current child */
    arg1: *const XmlChar,    /* first arg */
    arg2: *const XmlChar,    /* second arg */
}

/**
 * xmlRelaxNGValidCtxt:
 *
 * A RelaxNGs validation context
 */
/**
 * A schemas validation context
 */
pub type XmlRelaxNGValidCtxtPtr = *mut XmlRelaxNGValidCtxt;
#[repr(C)]
pub struct XmlRelaxNGValidCtxt {
    user_data: Option<GenericErrorContext>, /* user specific data block */
    error: Option<GenericError>,            /* the callback in case of errors */
    warning: Option<GenericError>,          /* the callback in case of warning */
    serror: Option<StructuredError>,
    nb_errors: i32, /* number of errors in validation */

    schema: XmlRelaxNGPtr, /* The schema in use */
    doc: XmlDocPtr,        /* the document being validated */
    flags: i32,            /* validation flags */
    depth: i32,            /* validation depth */
    idref: i32,            /* requires idref checking */
    err_no: i32,           /* the first error found */

    /*
     * Errors accumulated in branches may have to be stacked to be
     * provided back when it's sure they affect validation.
     */
    err: XmlRelaxNGValidErrorPtr,     /* Last error */
    err_nr: i32,                      /* Depth of the error stack */
    err_max: i32,                     /* Max depth of the error stack */
    err_tab: XmlRelaxNGValidErrorPtr, /* stack of errors */

    state: XmlRelaxNGValidStatePtr, /* the current validation state */
    states: XmlRelaxNGStatesPtr,    /* the accumulated state list */

    free_state: XmlRelaxNGStatesPtr, /* the pool of free valid states */
    free_states_nr: i32,
    free_states_max: i32,
    free_states: *mut XmlRelaxNGStatesPtr, /* the pool of free state groups */

    /*
     * This is used for "progressive" validation
     */
    elem: XmlRegExecCtxtPtr,          /* the current element regexp */
    elem_nr: i32,                     /* the number of element validated */
    elem_max: i32,                    /* the max depth of elements */
    elem_tab: *mut XmlRegExecCtxtPtr, /* the stack of regexp runtime */
    pstate: i32,                      /* progressive state */
    pnode: XmlNodePtr,                /* the current node */
    pdef: XmlRelaxNGDefinePtr,        /* the non-streamable definition */
    perr: i32,                        /* signal error in content model
                                       * outside the regexp */
}

// Both definition and declaration of this type are not found in any sourse codes of original libxml2...what is this ???
// pub type xmlRelaxNGSchema = _xmlRelaxNGSchema;
// pub type xmlRelaxNGSchemaPtr = *mut xmlRelaxNGSchema;

/**
 * xmlRelaxNGInclude:
 *
 * Structure associated to a RelaxNGs document element
 */
pub type XmlRelaxNGIncludePtr = *mut XmlRelaxNGInclude;
#[repr(C)]
pub struct XmlRelaxNGInclude {
    next: XmlRelaxNGIncludePtr,   /* keep a chain of includes */
    href: *mut XmlChar,           /* the normalized href value */
    doc: XmlDocPtr,               /* the associated XML document */
    content: XmlRelaxNGDefinePtr, /* the definitions */
    schema: XmlRelaxNGPtr,        /* the schema */
}

/**
 * xmlRelaxNGDocument:
 *
 * Structure associated to a RelaxNGs document element
 */
pub type XmlRelaxNGDocumentPtr = *mut XmlRelaxNGDocument;
#[repr(C)]
pub struct XmlRelaxNGDocument {
    next: XmlRelaxNGDocumentPtr,  /* keep a chain of documents */
    href: *mut XmlChar,           /* the normalized href value */
    doc: XmlDocPtr,               /* the associated XML document */
    content: XmlRelaxNGDefinePtr, /* the definitions */
    schema: XmlRelaxNGPtr,        /* the schema */
    external_ref: i32,            /* 1 if an external ref */
}

/**
 * xmlRelaxNGTypeHave:
 * @data:  data needed for the library
 * @type:  the type name
 * @value:  the value to check
 *
 * Function provided by a type library to check if a type is exported
 *
 * Returns 1 if yes, 0 if no and -1 in case of error.
 */
type XmlRelaxNGTypeHave = unsafe extern "C" fn(data: *mut c_void, typ: *const XmlChar) -> i32;

/**
 * xmlRelaxNGTypeCheck:
 * @data:  data needed for the library
 * @type:  the type name
 * @value:  the value to check
 * @result:  place to store the result if needed
 *
 * Function provided by a type library to check if a value match a type
 *
 * Returns 1 if yes, 0 if no and -1 in case of error.
 */
type XmlRelaxNGTypeCheck = unsafe extern "C" fn(
    data: *mut c_void,
    typ: *const XmlChar,
    value: *const XmlChar,
    result: *mut *mut c_void,
    node: XmlNodePtr,
) -> i32;

/**
 * xmlRelaxNGFacetCheck:
 * @data:  data needed for the library
 * @type:  the type name
 * @facet:  the facet name
 * @val:  the facet value
 * @strval:  the string value
 * @value:  the value to check
 *
 * Function provided by a type library to check a value facet
 *
 * Returns 1 if yes, 0 if no and -1 in case of error.
 */
type XmlRelaxNGFacetCheck = unsafe extern "C" fn(
    data: *mut c_void,
    typ: *const XmlChar,
    facet: *const XmlChar,
    val: *const XmlChar,
    strval: *const XmlChar,
    value: *mut c_void,
) -> i32;

/**
 * xmlRelaxNGTypeFree:
 * @data:  data needed for the library
 * @result:  the value to free
 *
 * Function provided by a type library to free a returned result
 */
type XmlRelaxNGTypeFree = unsafe extern "C" fn(data: *mut c_void, result: *mut c_void);

/**
 * xmlRelaxNGTypeCompare:
 * @data:  data needed for the library
 * @type:  the type name
 * @value1:  the first value
 * @value2:  the second value
 *
 * Function provided by a type library to compare two values accordingly
 * to a type.
 *
 * Returns 1 if yes, 0 if no and -1 in case of error.
 */
type XmlRelaxNGTypeCompare = unsafe extern "C" fn(
    data: *mut c_void,
    typ: *const XmlChar,
    value1: *const XmlChar,
    ctxt1: XmlNodePtr,
    comp1: *mut c_void,
    value2: *const XmlChar,
    ctxt2: XmlNodePtr,
) -> i32;
pub type XmlRelaxNGTypeLibraryPtr = *mut XmlRelaxNGTypeLibrary;
#[repr(C)]
pub struct XmlRelaxNGTypeLibrary {
    namespace: *const XmlChar,           /* the datatypeLibrary value */
    data: *mut c_void,                   /* data needed for the library */
    have: Option<XmlRelaxNGTypeHave>,    /* the export function */
    check: Option<XmlRelaxNGTypeCheck>,  /* the checking function */
    comp: Option<XmlRelaxNGTypeCompare>, /* the compare function */
    facet: Option<XmlRelaxNGFacetCheck>, /* the facet check function */
    freef: Option<XmlRelaxNGTypeFree>,   /* the freeing function */
}

static XML_RELAXNG_TYPE_INITIALIZED: AtomicBool = AtomicBool::new(false);
static XML_RELAXNG_REGISTERED_TYPES: AtomicPtr<XmlHashTable<'static, CVoidWrapper>> =
    AtomicPtr::new(null_mut());

macro_rules! VALID_ERR {
    ($ctxt:expr, $a:expr) => {
        xml_relaxng_add_valid_error($ctxt, $a, null_mut(), null_mut(), 0);
    };
}
macro_rules! VALID_ERR2 {
    ($ctxt:expr, $a:expr, $b:expr) => {
        xml_relaxng_add_valid_error($ctxt, $a, $b, null_mut(), 0);
    };
}
macro_rules! VALID_ERR3 {
    ($ctxt:expr, $a:expr, $b:expr, $c:expr) => {
        xml_relaxng_add_valid_error($ctxt, $a, $b, $c, 0);
    };
}
macro_rules! VALID_ERR2P {
    ($ctxt:expr, $a:expr, $b:expr) => {
        xml_relaxng_add_valid_error($ctxt, $a, $b, null_mut(), 1);
    };
}
macro_rules! VALID_ERR3P {
    ($ctxt:expr, $a:expr, $b:expr, $c:expr) => {
        xml_relaxng_add_valid_error($ctxt, $a, $b, $c, 1);
    };
}

/**
 * xmlRelaxNGGetErrorString:
 * @err:  the error code
 * @arg1:  the first string argument
 * @arg2:  the second string argument
 *
 * computes a formatted error string for the given error code and args
 *
 * Returns the error string, it must be deallocated by the caller
 */
unsafe extern "C" fn xml_relaxng_get_error_string(
    err: XmlRelaxNGValidErr,
    mut arg1: *const XmlChar,
    mut arg2: *const XmlChar,
) -> *mut XmlChar {
    let mut msg: [c_char; 1000] = [0; 1000];
    let mut result: *mut XmlChar;

    if arg1.is_null() {
        arg1 = c"".as_ptr() as _;
    }
    if arg2.is_null() {
        arg2 = c"".as_ptr() as _;
    }

    msg[0] = 0;
    match err {
        XmlRelaxNGValidErr::XmlRelaxngOk => return null_mut(),
        XmlRelaxNGValidErr::XmlRelaxngErrMemory => {
            return xml_char_strdup(c"out of memory\n".as_ptr() as _)
        }
        XmlRelaxNGValidErr::XmlRelaxngErrType => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"failed to validate type %s\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrTypeval => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Type %s doesn't allow value '%s'\n".as_ptr() as _,
                arg1,
                arg2,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrDupid => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"ID %s redefined\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrTypecmp => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"failed to compare type %s\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrNostate => {
            return xml_char_strdup(c"Internal error: no state\n".as_ptr() as _)
        }
        XmlRelaxNGValidErr::XmlRelaxngErrNodefine => {
            return xml_char_strdup(c"Internal error: no define\n".as_ptr() as _)
        }
        XmlRelaxNGValidErr::XmlRelaxngErrInternal => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Internal error: %s\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrListextra => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Extra data in list: %s\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrInternodata => {
            return xml_char_strdup(c"Internal: interleave block has no data\n".as_ptr() as _)
        }
        XmlRelaxNGValidErr::XmlRelaxngErrInterseq => {
            return xml_char_strdup(c"Invalid sequence in interleave\n".as_ptr() as _)
        }
        XmlRelaxNGValidErr::XmlRelaxngErrInterextra => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Extra element %s in interleave\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrElemname => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Expecting element %s, got %s\n".as_ptr() as _,
                arg1,
                arg2,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrElemnons => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Expecting a namespace for element %s\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrElemwrongns => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Element %s has wrong namespace: expecting %s\n".as_ptr() as _,
                arg1,
                arg2,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrElemwrong => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Did not expect element %s there\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrTextwrong => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Did not expect text in element %s content\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrElemextrans => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Expecting no namespace for element %s\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrElemnotempty => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Expecting element %s to be empty\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrNoelem => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Expecting an element %s, got nothing\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrNotelem => {
            return xml_char_strdup(c"Expecting an element got text\n".as_ptr() as _)
        }
        XmlRelaxNGValidErr::XmlRelaxngErrAttrvalid => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Element %s failed to validate attributes\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrContentvalid => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Element %s failed to validate content\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrExtracontent => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Element %s has extra content: %s\n".as_ptr() as _,
                arg1,
                arg2,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrInvalidattr => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Invalid attribute %s for element %s\n".as_ptr() as _,
                arg1,
                arg2,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrLackdata => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Datatype element %s contains no data\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrDataelem => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Datatype element %s has child elements\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrValelem => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Value element %s has child elements\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrListelem => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"List element %s has child elements\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrDatatype => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Error validating datatype %s\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrValue => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Error validating value %s\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrList => {
            return xml_char_strdup(c"Error validating list\n".as_ptr() as _)
        }
        XmlRelaxNGValidErr::XmlRelaxngErrNogrammar => {
            return xml_char_strdup(c"No top grammar defined\n".as_ptr() as _)
        }
        XmlRelaxNGValidErr::XmlRelaxngErrExtradata => {
            return xml_char_strdup(c"Extra data in the document\n".as_ptr() as _)
        }
        _ => return xml_char_strdup(c"Unknown error !\n".as_ptr() as _),
    }
    if msg[0] == 0 {
        snprintf(
            msg.as_mut_ptr() as _,
            1000,
            c"Unknown error code %d\n".as_ptr() as _,
            err,
        );
    }
    msg[1000 - 1] = 0;
    result = xml_char_strdup(msg.as_ptr() as _);
    xml_escape_format_string(addr_of_mut!(result))
}

/**
 * xmlRngVErr:
 * @ctxt:  a Relax-NG validation context
 * @node:  the node raising the error
 * @error:  the error code
 * @msg:  message
 * @str1:  extra info
 * @str2:  extra info
 *
 * Handle a Relax NG Validation error
 */
unsafe extern "C" fn xml_rng_verr(
    ctxt: XmlRelaxNGValidCtxtPtr,
    node: XmlNodePtr,
    error: i32,
    msg: *const c_char,
    str1: *const XmlChar,
    str2: *const XmlChar,
) {
    let mut schannel: Option<StructuredError> = None;
    let mut channel: Option<GenericError> = None;
    let mut data = None;

    if !ctxt.is_null() {
        if (*ctxt).serror.is_some() {
            schannel = (*ctxt).serror;
        } else {
            channel = (*ctxt).error;
        }
        data = (*ctxt).user_data.clone();
        (*ctxt).nb_errors += 1;
    }
    let error = XmlParserErrors::try_from(error).unwrap();
    __xml_raise_error!(
        schannel,
        channel,
        data,
        null_mut(),
        node as _,
        XmlErrorDomain::XmlFromRelaxngv,
        error,
        XmlErrorLevel::XmlErrError,
        null_mut(),
        0,
        (!str1.is_null()).then(|| CStr::from_ptr(str1 as *const i8)
            .to_string_lossy()
            .into_owned()
            .into()),
        (!str2.is_null()).then(|| CStr::from_ptr(str2 as *const i8)
            .to_string_lossy()
            .into_owned()
            .into()),
        None,
        0,
        0,
        msg,
        str1,
        str2
    );
}

/**
 * xmlRelaxNGShowValidError:
 * @ctxt:  the validation context
 * @err:  the error number
 * @node:  the node
 * @child:  the node child generating the problem.
 * @arg1:  the first argument
 * @arg2:  the second argument
 *
 * Show a validation error.
 */
unsafe extern "C" fn xml_relaxng_show_valid_error(
    ctxt: XmlRelaxNGValidCtxtPtr,
    err: XmlRelaxNGValidErr,
    node: XmlNodePtr,
    child: XmlNodePtr,
    arg1: *const XmlChar,
    arg2: *const XmlChar,
) {
    if (*ctxt).flags & FLAGS_NOERROR != 0 {
        return;
    }

    let msg: *mut XmlChar = xml_relaxng_get_error_string(err, arg1, arg2);
    if msg.is_null() {
        return;
    }

    if (*ctxt).err_no == XmlRelaxNGValidErr::XmlRelaxngOk as i32 {
        (*ctxt).err_no = err as i32;
    }
    xml_rng_verr(
        ctxt,
        if child.is_null() { node } else { child },
        err as _,
        msg as _,
        arg1,
        arg2,
    );
    xml_free(msg as _);
}

const MAX_ERROR: usize = 5;

/**
 * xmlRelaxNGDumpValidError:
 * @ctxt:  the validation context
 *
 * Show all validation error over a given index.
 */
unsafe extern "C" fn xml_relaxng_dump_valid_error(ctxt: XmlRelaxNGValidCtxtPtr) {
    let mut k: usize;
    let mut err: XmlRelaxNGValidErrorPtr;
    let mut dup: XmlRelaxNGValidErrorPtr;

    k = 0;
    'main: for i in 0..(*ctxt).err_nr {
        err = (*ctxt).err_tab.add(i as usize);
        if k < MAX_ERROR {
            for j in 0..i {
                dup = (*ctxt).err_tab.add(j as usize);
                if (*err).err == (*dup).err
                    && (*err).node == (*dup).node
                    && xml_str_equal((*err).arg1, (*dup).arg1)
                    && xml_str_equal((*err).arg2, (*dup).arg2)
                {
                    if (*err).flags & ERROR_IS_DUP != 0 {
                        if !(*err).arg1.is_null() {
                            xml_free((*err).arg1 as _);
                        }
                        (*err).arg1 = null_mut();
                        if !(*err).arg2.is_null() {
                            xml_free((*err).arg2 as _);
                        }
                        (*err).arg2 = null_mut();
                        (*err).flags = 0;
                    }
                    continue 'main;
                }
            }
            xml_relaxng_show_valid_error(
                ctxt,
                (*err).err,
                (*err).node,
                (*err).seq,
                (*err).arg1,
                (*err).arg2,
            );
            k += 1;
        }
    }
    (*ctxt).err_nr = 0;
}

/**
 * xmlRelaxNGValidErrorPush:
 * @ctxt:  the validation context
 * @err:  the error code
 * @arg1:  the first string argument
 * @arg2:  the second string argument
 * @dup:  arg need to be duplicated
 *
 * Pushes a new error on top of the error stack
 *
 * Returns 0 in case of error, the index in the stack otherwise
 */
unsafe extern "C" fn xml_relaxng_valid_error_push(
    ctxt: XmlRelaxNGValidCtxtPtr,
    err: XmlRelaxNGValidErr,
    arg1: *const XmlChar,
    arg2: *const XmlChar,
    dup: i32,
) -> i32 {
    if (*ctxt).err_tab.is_null() {
        (*ctxt).err_max = 8;
        (*ctxt).err_nr = 0;
        (*ctxt).err_tab =
            xml_malloc((*ctxt).err_max as usize * size_of::<XmlRelaxNGValidError>()) as _;
        if (*ctxt).err_tab.is_null() {
            xml_rng_verr_memory(ctxt, c"pushing error\n".as_ptr() as _);
            return 0;
        }
        (*ctxt).err = null_mut();
    }
    if (*ctxt).err_nr >= (*ctxt).err_max {
        (*ctxt).err_max *= 2;
        (*ctxt).err_tab = xml_realloc(
            (*ctxt).err_tab as _,
            (*ctxt).err_max as usize * size_of::<XmlRelaxNGValidError>(),
        ) as _;
        if (*ctxt).err_tab.is_null() {
            xml_rng_verr_memory(ctxt, c"pushing error\n".as_ptr() as _);
            return 0;
        }
        (*ctxt).err = (*ctxt).err_tab.add((*ctxt).err_nr as usize - 1);
    }
    if !(*ctxt).err.is_null()
        && !(*ctxt).state.is_null()
        && (*(*ctxt).err).node == (*(*ctxt).state).node
        && (*(*ctxt).err).err == err
    {
        return (*ctxt).err_nr;
    }
    let cur: XmlRelaxNGValidErrorPtr = (*ctxt).err_tab.add((*ctxt).err_nr as usize);
    (*cur).err = err;
    if dup != 0 {
        (*cur).arg1 = xml_strdup(arg1);
        (*cur).arg2 = xml_strdup(arg2);
        (*cur).flags = ERROR_IS_DUP;
    } else {
        (*cur).arg1 = arg1;
        (*cur).arg2 = arg2;
        (*cur).flags = 0;
    }
    if !(*ctxt).state.is_null() {
        (*cur).node = (*(*ctxt).state).node;
        (*cur).seq = (*(*ctxt).state).seq;
    } else {
        (*cur).node = null_mut();
        (*cur).seq = null_mut();
    }
    (*ctxt).err = cur;
    (*ctxt).err_nr += 1;
    (*ctxt).err_nr - 1
}

/**
 * xml_relaxng_add_valid_error:
 * @ctxt:  the validation context
 * @err:  the error number
 * @arg1:  the first argument
 * @arg2:  the second argument
 * @dup:  need to dup the args
 *
 * Register a validation error, either generating it if it's sure
 * or stacking it for later handling if unsure.
 */
unsafe extern "C" fn xml_relaxng_add_valid_error(
    ctxt: XmlRelaxNGValidCtxtPtr,
    err: XmlRelaxNGValidErr,
    arg1: *const XmlChar,
    arg2: *const XmlChar,
    dup: i32,
) {
    if ctxt.is_null() {
        return;
    }
    if (*ctxt).flags & FLAGS_NOERROR != 0 {
        return;
    }

    /*
     * generate the error directly
     */
    if (*ctxt).flags & FLAGS_IGNORABLE == 0 || (*ctxt).flags & FLAGS_NEGATIVE != 0 {
        let mut node: XmlNodePtr;
        let seq: XmlNodePtr;

        /*
         * Flush first any stacked error which might be the
         * real cause of the problem.
         */
        if (*ctxt).err_nr != 0 {
            xml_relaxng_dump_valid_error(ctxt);
        }
        if !(*ctxt).state.is_null() {
            node = (*(*ctxt).state).node;
            seq = (*(*ctxt).state).seq;
        } else {
            node = null_mut();
            seq = node;
        }
        if node.is_null() && seq.is_null() {
            node = (*ctxt).pnode;
        }
        xml_relaxng_show_valid_error(ctxt, err, node, seq, arg1, arg2);
    } else {
        /*
         * Stack the error for later processing if needed
         */
        xml_relaxng_valid_error_push(ctxt, err, arg1, arg2, dup);
    }
}

/**
 * xmlRngVErrMemory:
 * @ctxt:  a Relax-NG validation context
 * @extra:  extra information
 *
 * Handle a redefinition of attribute error
 */
unsafe extern "C" fn xml_rng_verr_memory(ctxt: XmlRelaxNGValidCtxtPtr, extra: *const c_char) {
    let mut schannel: Option<StructuredError> = None;
    let mut channel: Option<GenericError> = None;
    let mut data = None;

    if !ctxt.is_null() {
        if let Some(serror) = (*ctxt).serror {
            schannel = Some(serror);
        } else {
            channel = (*ctxt).error;
        }
        data = (*ctxt).user_data.clone();
        (*ctxt).nb_errors += 1;
    }
    if !extra.is_null() {
        __xml_raise_error!(
            schannel,
            channel,
            data,
            null_mut(),
            null_mut(),
            XmlErrorDomain::XmlFromRelaxngv,
            XmlParserErrors::XmlErrNoMemory,
            XmlErrorLevel::XmlErrFatal,
            null_mut(),
            0,
            (!extra.is_null()).then(|| CStr::from_ptr(extra).to_string_lossy().into_owned().into()),
            None,
            None,
            0,
            0,
            c"Memory allocation failed : %s\n".as_ptr() as _,
            extra
        );
    } else {
        __xml_raise_error!(
            schannel,
            channel,
            data,
            null_mut(),
            null_mut(),
            XmlErrorDomain::XmlFromRelaxngv,
            XmlParserErrors::XmlErrNoMemory,
            XmlErrorLevel::XmlErrFatal,
            null_mut(),
            0,
            None,
            None,
            None,
            0,
            0,
            c"Memory allocation failed\n".as_ptr() as _,
        );
    }
}

/**
 * xmlRelaxNGRegisterTypeLibrary:
 * @namespace:  the URI bound to the library
 * @data:  data associated to the library
 * @have:  the provide function
 * @check:  the checking function
 * @comp:  the comparison function
 *
 * Register a new type library
 *
 * Returns 0 in case of success and -1 in case of error.
 */
unsafe extern "C" fn xml_relaxng_register_type_library(
    namespace: *const XmlChar,
    data: *mut c_void,
    have: Option<XmlRelaxNGTypeHave>,
    check: Option<XmlRelaxNGTypeCheck>,
    comp: Option<XmlRelaxNGTypeCompare>,
    facet: Option<XmlRelaxNGFacetCheck>,
    freef: Option<XmlRelaxNGTypeFree>,
) -> i32 {
    let registered_types = XML_RELAXNG_REGISTERED_TYPES.load(Ordering::Acquire);
    if registered_types.is_null() || namespace.is_null() || check.is_none() || comp.is_none() {
        return -1;
    }
    if !xml_hash_lookup(registered_types, namespace).is_null() {
        generic_error!(
            "Relax-NG types library '{}' already registered\n",
            CStr::from_ptr(namespace as *const i8).to_string_lossy()
        );
        return -1;
    }
    let lib: XmlRelaxNGTypeLibraryPtr = xml_malloc(size_of::<XmlRelaxNGTypeLibrary>()) as _;
    if lib.is_null() {
        xml_rng_verr_memory(null_mut(), c"adding types library\n".as_ptr() as _);
        return -1;
    }
    memset(lib as _, 0, size_of::<XmlRelaxNGTypeLibrary>());
    (*lib).namespace = xml_strdup(namespace);
    (*lib).data = data;
    (*lib).have = have;
    (*lib).comp = comp;
    (*lib).check = check;
    (*lib).facet = facet;
    (*lib).freef = freef;
    let ret: i32 = xml_hash_add_entry(registered_types, namespace, lib as _);
    if ret < 0 {
        generic_error!(
            "Relax-NG types library failed to register '{}'\n",
            CStr::from_ptr(namespace as *const i8).to_string_lossy()
        );
        xml_relaxng_free_type_library(lib as _, namespace);
        return -1;
    }
    0
}

/**
 * xmlRelaxNGSchemaTypeHave:
 * @data:  data needed for the library
 * @type:  the type name
 *
 * Check if the given type is provided by
 * the W3C XMLSchema Datatype library.
 *
 * Returns 1 if yes, 0 if no and -1 in case of error.
 */
unsafe extern "C" fn xml_relaxng_schema_type_have(_data: *mut c_void, typ: *const XmlChar) -> i32 {
    if typ.is_null() {
        return -1;
    }
    let _typ: XmlSchemaTypePtr =
        xml_schema_get_predefined_type(typ, c"http://www.w3.org/2001/XMLSchema".as_ptr() as _);
    if _typ.is_null() {
        return 0;
    }
    1
}

/**
 * xmlRelaxNGSchemaTypeCheck:
 * @data:  data needed for the library
 * @type:  the type name
 * @value:  the value to check
 * @node:  the node
 *
 * Check if the given type and value are validated by
 * the W3C XMLSchema Datatype library.
 *
 * Returns 1 if yes, 0 if no and -1 in case of error.
 */
unsafe extern "C" fn xml_relaxng_schema_type_check(
    _data: *mut c_void,
    r#type: *const XmlChar,
    value: *const XmlChar,
    result: *mut *mut c_void,
    node: XmlNodePtr,
) -> i32 {
    if r#type.is_null() || value.is_null() {
        return -1;
    }
    let typ: XmlSchemaTypePtr = xml_schema_get_predefined_type(
        r#type as _,
        c"http://www.w3.org/2001/XMLSchema".as_ptr() as _,
    );
    if typ.is_null() {
        return -1;
    }
    let ret: i32 = xml_schema_val_predef_type_node(typ, value, result as _, node);
    if ret == 2 {
        /* special ID error code */
        return 2;
    }
    if ret == 0 {
        return 1;
    }
    if ret > 0 {
        return 0;
    }
    -1
}

/**
 * xmlRelaxNGSchemaTypeCompare:
 * @data:  data needed for the library
 * @type:  the type name
 * @value1:  the first value
 * @value2:  the second value
 *
 * Compare two values for equality accordingly a type from the W3C XMLSchema
 * Datatype library.
 *
 * Returns 1 if equal, 0 if no and -1 in case of error.
 */
unsafe extern "C" fn xml_relaxng_schema_type_compare(
    _data: *mut c_void,
    r#type: *const XmlChar,
    value1: *const XmlChar,
    ctxt1: XmlNodePtr,
    comp1: *mut c_void,
    value2: *const XmlChar,
    ctxt2: XmlNodePtr,
) -> i32 {
    let mut ret: i32;
    let mut res1: XmlSchemaValPtr = null_mut();
    let mut res2: XmlSchemaValPtr = null_mut();

    if r#type.is_null() || value1.is_null() || value2.is_null() {
        return -1;
    }
    let typ: XmlSchemaTypePtr = xml_schema_get_predefined_type(
        r#type as _,
        c"http://www.w3.org/2001/XMLSchema".as_ptr() as _,
    );
    if typ.is_null() {
        return -1;
    }
    if comp1.is_null() {
        ret = xml_schema_val_predef_type_node(typ, value1, addr_of_mut!(res1), ctxt1);
        if ret != 0 {
            return -1;
        }
        if res1.is_null() {
            return -1;
        }
    } else {
        res1 = comp1 as _;
    }
    ret = xml_schema_val_predef_type_node(typ, value2, addr_of_mut!(res2), ctxt2);
    if ret != 0 {
        if res1 != comp1 as _ {
            xml_schema_free_value(res1);
        }
        return -1;
    }
    ret = xml_schema_compare_values(res1, res2);
    if res1 != comp1 as _ {
        xml_schema_free_value(res1);
    }
    xml_schema_free_value(res2);
    if ret == -2 {
        return -1;
    }
    if ret == 0 {
        return 1;
    }
    0
}

/**
 * xmlRelaxNGSchemaFacetCheck:
 * @data:  data needed for the library
 * @type:  the type name
 * @facet:  the facet name
 * @val:  the facet value
 * @strval:  the string value
 * @value:  the value to check
 *
 * Function provided by a type library to check a value facet
 *
 * Returns 1 if yes, 0 if no and -1 in case of error.
 */
unsafe extern "C" fn xml_relaxng_schema_facet_check(
    _data: *mut c_void,
    r#type: *const XmlChar,
    facetname: *const XmlChar,
    val: *const XmlChar,
    strval: *const XmlChar,
    value: *mut c_void,
) -> i32 {
    let mut ret: i32;

    if r#type.is_null() || strval.is_null() {
        return -1;
    }
    let typ: XmlSchemaTypePtr = xml_schema_get_predefined_type(
        r#type as _,
        c"http://www.w3.org/2001/XMLSchema".as_ptr() as _,
    );
    if typ.is_null() {
        return -1;
    }

    let facet: XmlSchemaFacetPtr = xml_schema_new_facet();
    if facet.is_null() {
        return -1;
    }

    if xml_str_equal(facetname, c"minInclusive".as_ptr() as _) {
        (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetMininclusive;
    } else if xml_str_equal(facetname, c"minExclusive".as_ptr() as _) {
        (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetMinexclusive;
    } else if xml_str_equal(facetname, c"maxInclusive".as_ptr() as _) {
        (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetMaxinclusive;
    } else if xml_str_equal(facetname, c"maxExclusive".as_ptr() as _) {
        (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetMaxexclusive;
    } else if xml_str_equal(facetname, c"totalDigits".as_ptr() as _) {
        (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetTotaldigits;
    } else if xml_str_equal(facetname, c"fractionDigits".as_ptr() as _) {
        (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetFractiondigits;
    } else if xml_str_equal(facetname, c"pattern".as_ptr() as _) {
        (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetPattern;
    } else if xml_str_equal(facetname, c"enumeration".as_ptr() as _) {
        (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetEnumeration;
    } else if xml_str_equal(facetname, c"whiteSpace".as_ptr() as _) {
        (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetWhitespace;
    } else if xml_str_equal(facetname, c"length".as_ptr() as _) {
        (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetLength;
    } else if xml_str_equal(facetname, c"maxLength".as_ptr() as _) {
        (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetMaxlength;
    } else if xml_str_equal(facetname, c"minLength".as_ptr() as _) {
        (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetMinlength;
    } else {
        xml_schema_free_facet(facet);
        return -1;
    }
    (*facet).value = val;
    ret = xml_schema_check_facet(facet, typ, null_mut(), typ as _);
    if ret != 0 {
        xml_schema_free_facet(facet);
        return -1;
    }
    ret = xml_schema_validate_facet(typ, facet, strval, value as _);
    xml_schema_free_facet(facet);
    if ret != 0 {
        return -1;
    }
    0
}

/**
 * xmlRelaxNGSchemaFreeValue:
 * @data:  data needed for the library
 * @value:  the value to free
 *
 * Function provided by a type library to free a Schemas value
 *
 * Returns 1 if yes, 0 if no and -1 in case of error.
 */
unsafe extern "C" fn xml_relaxng_schema_free_value(_data: *mut c_void, value: *mut c_void) {
    xml_schema_free_value(value as _);
}

/*
 * The Relax-NG namespace
 */
const XML_RELAXNG_NS: &CStr = c"http://relaxng.org/ns/structure/1.0";

macro_rules! IS_RELAXNG {
    ($node:expr, $typ:expr) => {
        !$node.is_null()
            && !(*$node).ns.is_null()
            && (*$node).typ == XmlElementType::XmlElementNode
            && xml_str_equal((*$node).name, $typ)
            && xml_str_equal(
                (*(*$node).ns).href.load(Ordering::Relaxed),
                XML_RELAXNG_NS.as_ptr() as _,
            )
    };
}

/**
 * xmlRelaxNGDefaultTypeHave:
 * @data:  data needed for the library
 * @type:  the type name
 *
 * Check if the given type is provided by
 * the default datatype library.
 *
 * Returns 1 if yes, 0 if no and -1 in case of error.
 */
unsafe extern "C" fn xml_relaxng_default_type_have(_data: *mut c_void, typ: *const XmlChar) -> i32 {
    if typ.is_null() {
        return -1;
    }
    if xml_str_equal(typ, c"string".as_ptr() as _) {
        return 1;
    }
    if xml_str_equal(typ, c"token".as_ptr() as _) {
        return 1;
    }
    0
}

/**
 * xmlRelaxNGDefaultTypeCheck:
 * @data:  data needed for the library
 * @type:  the type name
 * @value:  the value to check
 * @node:  the node
 *
 * Check if the given type and value are validated by
 * the default datatype library.
 *
 * Returns 1 if yes, 0 if no and -1 in case of error.
 */
unsafe extern "C" fn xml_relaxng_default_type_check(
    _data: *mut c_void,
    typ: *const XmlChar,
    value: *const XmlChar,
    _result: *mut *mut c_void,
    _node: XmlNodePtr,
) -> i32 {
    if value.is_null() {
        return -1;
    }
    if xml_str_equal(typ, c"string".as_ptr() as _) {
        return 1;
    }
    if xml_str_equal(typ, c"token".as_ptr() as _) {
        return 1;
    }

    0
}

/**
 * xmlRelaxNGNormalize:
 * @ctxt:  a schema validation context
 * @str:  the string to normalize
 *
 * Implements the  normalizeWhiteSpace( s ) function from
 * section 6.2.9 of the spec
 *
 * Returns the new string or NULL in case of error.
 */
unsafe extern "C" fn xml_relaxng_normalize(
    ctxt: XmlRelaxNGValidCtxtPtr,
    mut str: *const XmlChar,
) -> *mut XmlChar {
    let mut p: *mut XmlChar;
    let mut tmp: *const XmlChar;

    if str.is_null() {
        return null_mut();
    }
    tmp = str;
    while *tmp != 0 {
        tmp = tmp.add(1);
    }
    let len: i32 = tmp.offset_from(str) as _;

    let ret: *mut XmlChar = xml_malloc_atomic(len as usize + 1) as _;
    if ret.is_null() {
        xml_rng_verr_memory(ctxt, c"validating\n".as_ptr() as _);
        return null_mut();
    }
    p = ret;
    while xml_is_blank_char(*str as u32) {
        str = str.add(1);
    }
    while *str != 0 {
        if xml_is_blank_char(*str as u32) {
            while xml_is_blank_char(*str as u32) {
                str = str.add(1);
            }
            if *str == 0 {
                break;
            }
            *p = b' ';
            p = p.add(1);
        } else {
            *p = *str;
            p = p.add(1);
            str = str.add(1);
        }
    }
    *p = 0;
    ret
}

/**
 * xmlRelaxNGDefaultTypeCompare:
 * @data:  data needed for the library
 * @type:  the type name
 * @value1:  the first value
 * @value2:  the second value
 *
 * Compare two values accordingly a type from the default
 * datatype library.
 *
 * Returns 1 if yes, 0 if no and -1 in case of error.
 */
unsafe extern "C" fn xml_relaxng_default_type_compare(
    _data: *mut c_void,
    typ: *const XmlChar,
    value1: *const XmlChar,
    _ctxt1: XmlNodePtr,
    _comp1: *mut c_void,
    value2: *const XmlChar,
    _ctxt2: XmlNodePtr,
) -> i32 {
    let mut ret: i32 = -1;

    if xml_str_equal(typ, c"string".as_ptr() as _) {
        ret = xml_str_equal(value1, value2) as i32;
    } else if xml_str_equal(typ, c"token".as_ptr() as _) {
        if !xml_str_equal(value1, value2) {
            /*
             * TODO: trivial optimizations are possible by
             * computing at compile-time
             */
            let nval: *mut XmlChar = xml_relaxng_normalize(null_mut(), value1);
            let nvalue: *mut XmlChar = xml_relaxng_normalize(null_mut(), value2);

            if nval.is_null() || nvalue.is_null() {
                ret = -1;
            } else if xml_str_equal(nval, nvalue) {
                ret = 1;
            } else {
                ret = 0;
            }
            if !nval.is_null() {
                xml_free(nval as _);
            }
            if !nvalue.is_null() {
                xml_free(nvalue as _);
            }
        } else {
            ret = 1;
        }
    }
    ret
}

/**
 * xmlRelaxNGInitTypes:
 *
 * Initialize the default type libraries.
 *
 * Returns 0 in case of success and -1 in case of error.
 */
pub unsafe extern "C" fn xml_relaxng_init_types() -> i32 {
    if XML_RELAXNG_TYPE_INITIALIZED.load(Ordering::Acquire) {
        return 0;
    }

    let registered_types = xml_hash_create(10);
    if registered_types.is_null() {
        generic_error!("Failed to allocate sh table for Relax-NG types\n");
        return -1;
    }

    XML_RELAXNG_REGISTERED_TYPES.store(registered_types, Ordering::Release);
    xml_relaxng_register_type_library(
        c"http://www.w3.org/2001/XMLSchema-datatypes".as_ptr() as _,
        null_mut(),
        Some(xml_relaxng_schema_type_have),
        Some(xml_relaxng_schema_type_check),
        Some(xml_relaxng_schema_type_compare),
        Some(xml_relaxng_schema_facet_check),
        Some(xml_relaxng_schema_free_value),
    );
    xml_relaxng_register_type_library(
        XML_RELAXNG_NS.as_ptr() as _,
        null_mut(),
        Some(xml_relaxng_default_type_have),
        Some(xml_relaxng_default_type_check),
        Some(xml_relaxng_default_type_compare),
        None,
        None,
    );
    XML_RELAXNG_TYPE_INITIALIZED.store(true, Ordering::Release);
    0
}

/**
 * xmlRelaxNGFreeTypeLibrary:
 * @lib:  the type library structure
 * @namespace:  the URI bound to the library
 *
 * Free the structure associated to the type library
 */
extern "C" fn xml_relaxng_free_type_library(payload: *mut c_void, _namespace: *const XmlChar) {
    let lib: XmlRelaxNGTypeLibraryPtr = payload as _;
    if lib.is_null() {
        return;
    }
    unsafe {
        if !(*lib).namespace.is_null() {
            xml_free((*lib).namespace as _);
        }
        xml_free(lib as _);
    }
}

/**
 * xmlRelaxNGCleanupTypes:
 *
 * DEPRECATED: This function will be made private. Call xmlCleanupParser
 * to free global state but see the warnings there. xmlCleanupParser
 * should be only called once at program exit. In most cases, you don't
 * have call cleanup functions at all.
 *
 * Cleanup the default Schemas type library associated to RelaxNG
 */
pub(crate) unsafe extern "C" fn xml_relaxng_cleanup_types() {
    xml_schema_cleanup_types();
    if !XML_RELAXNG_TYPE_INITIALIZED.load(Ordering::Acquire) {
        return;
    }
    xml_hash_free(
        XML_RELAXNG_REGISTERED_TYPES.load(Ordering::Relaxed),
        Some(xml_relaxng_free_type_library),
    );
    XML_RELAXNG_TYPE_INITIALIZED.store(false, Ordering::Release);
}

/**
 * xmlRngPErrMemory:
 * @ctxt:  an Relax-NG parser context
 * @extra:  extra information
 *
 * Handle a redefinition of attribute error
 */
unsafe extern "C" fn xml_rng_perr_memory(ctxt: XmlRelaxNGParserCtxtPtr, extra: *const c_char) {
    let mut schannel: Option<StructuredError> = None;
    let mut channel: Option<GenericError> = None;
    let mut data = None;

    if !ctxt.is_null() {
        if (*ctxt).serror.is_some() {
            schannel = (*ctxt).serror;
        } else {
            channel = (*ctxt).error;
        }
        data = (*ctxt).user_data.clone();
        (*ctxt).nb_errors += 1;
    }
    if !extra.is_null() {
        __xml_raise_error!(
            schannel,
            channel,
            data,
            null_mut(),
            null_mut(),
            XmlErrorDomain::XmlFromRelaxngp,
            XmlParserErrors::XmlErrNoMemory,
            XmlErrorLevel::XmlErrFatal,
            null_mut(),
            0,
            (!extra.is_null()).then(|| CStr::from_ptr(extra).to_string_lossy().into_owned().into()),
            None,
            None,
            0,
            0,
            c"Memory allocation failed : %s\n".as_ptr() as _,
            extra
        );
    } else {
        __xml_raise_error!(
            schannel,
            channel,
            data,
            null_mut(),
            null_mut(),
            XmlErrorDomain::XmlFromRelaxngp,
            XmlParserErrors::XmlErrNoMemory,
            XmlErrorLevel::XmlErrFatal,
            null_mut(),
            0,
            None,
            None,
            None,
            0,
            0,
            c"Memory allocation failed\n".as_ptr() as _,
        );
    }
}

/*
 * Interfaces for parsing.
 */
/**
 * xmlRelaxNGNewParserCtxt:
 * @URL:  the location of the schema
 *
 * Create an XML RelaxNGs parse context for that file/resource expected
 * to contain an XML RelaxNGs file.
 *
 * Returns the parser context or NULL in case of error
 */
pub unsafe extern "C" fn xml_relaxng_new_parser_ctxt(
    url: *const c_char,
) -> XmlRelaxNGParserCtxtPtr {
    if url.is_null() {
        return null_mut();
    }

    let ret: XmlRelaxNGParserCtxtPtr = xml_malloc(size_of::<XmlRelaxNGParserCtxt>()) as _;
    if ret.is_null() {
        xml_rng_perr_memory(null_mut(), c"building parser\n".as_ptr() as _);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlRelaxNGParserCtxt>());
    (*ret).url = xml_strdup(url as _) as _;
    GLOBAL_STATE.with_borrow(|state| {
        (*ret).error = Some(state.generic_error);
        (*ret).user_data = state.generic_error_context.clone();
    });
    ret
}

/**
 * xmlRelaxNGNewMemParserCtxt:
 * @buffer:  a pointer to a c_char array containing the schemas
 * @size:  the size of the array
 *
 * Create an XML RelaxNGs parse context for that memory buffer expected
 * to contain an XML RelaxNGs file.
 *
 * Returns the parser context or NULL in case of error
 */
pub unsafe extern "C" fn xml_relaxng_new_mem_parser_ctxt(
    buffer: *const c_char,
    size: i32,
) -> XmlRelaxNGParserCtxtPtr {
    if buffer.is_null() || size <= 0 {
        return null_mut();
    }

    let ret: XmlRelaxNGParserCtxtPtr = xml_malloc(size_of::<XmlRelaxNGParserCtxt>()) as _;
    if ret.is_null() {
        xml_rng_perr_memory(null_mut(), c"building parser\n".as_ptr() as _);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlRelaxNGParserCtxt>());
    (*ret).buffer = buffer;
    (*ret).size = size;
    GLOBAL_STATE.with_borrow(|state| {
        (*ret).error = Some(state.generic_error);
        (*ret).user_data = state.generic_error_context.clone();
    });
    ret
}

/**
 * xmlRelaxNGNewDocParserCtxt:
 * @doc:  a preparsed document tree
 *
 * Create an XML RelaxNGs parser context for that document.
 * Note: since the process of compiling a RelaxNG schemas modifies the
 *       document, the @doc parameter is duplicated internally.
 *
 * Returns the parser context or NULL in case of error
 */
pub unsafe extern "C" fn xml_relaxng_new_doc_parser_ctxt(
    doc: XmlDocPtr,
) -> XmlRelaxNGParserCtxtPtr {
    if doc.is_null() {
        return null_mut();
    }
    let copy: XmlDocPtr = xml_copy_doc(doc, 1);
    if copy.is_null() {
        return null_mut();
    }

    let ret: XmlRelaxNGParserCtxtPtr = xml_malloc(size_of::<XmlRelaxNGParserCtxt>()) as _;
    if ret.is_null() {
        xml_rng_perr_memory(null_mut(), c"building parser\n".as_ptr() as _);
        xml_free_doc(copy);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlRelaxNGParserCtxt>());
    (*ret).document = copy;
    (*ret).freedoc = 1;
    GLOBAL_STATE.with_borrow(|state| {
        (*ret).user_data = state.generic_error_context.clone();
    });
    ret
}

/**
 * xmlRelaxParserSetFlag:
 * @ctxt: a RelaxNG parser context
 * @flags: a set of flags values
 *
 * Semi private function used to pass information to a parser context
 * which are a combination of xmlRelaxNGParserFlag .
 *
 * Returns 0 if success and -1 in case of error
 */
pub unsafe extern "C" fn xml_relax_parser_set_flag(
    ctxt: XmlRelaxNGParserCtxtPtr,
    mut flags: i32,
) -> i32 {
    if ctxt.is_null() {
        return -1;
    }
    if flags & XmlRelaxNGParserFlag::FreeDoc as i32 != 0 {
        (*ctxt).crng |= XmlRelaxNGParserFlag::FreeDoc as i32;
        flags -= XmlRelaxNGParserFlag::FreeDoc as i32;
    }
    if flags & XmlRelaxNGParserFlag::Crng as i32 != 0 {
        (*ctxt).crng |= XmlRelaxNGParserFlag::Crng as i32;
        flags -= XmlRelaxNGParserFlag::Crng as i32;
    }
    if flags != 0 {
        return -1;
    }
    0
}

/**
 * xmlRelaxNGFreePartition:
 * @partitions:  a partition set structure
 *
 * Deallocate RelaxNG partition set structures.
 */
unsafe extern "C" fn xml_relaxng_free_partition(partitions: XmlRelaxNGPartitionPtr) {
    let mut group: XmlRelaxNGInterleaveGroupPtr;

    if !partitions.is_null() {
        if !(*partitions).groups.is_null() {
            for j in 0..(*partitions).nbgroups {
                group = *(*partitions).groups.add(j as usize);
                if !group.is_null() {
                    if !(*group).defs.is_null() {
                        xml_free((*group).defs as _);
                    }
                    if !(*group).attrs.is_null() {
                        xml_free((*group).attrs as _);
                    }
                    xml_free(group as _);
                }
            }
            xml_free((*partitions).groups as _);
        }
        if !(*partitions).triage.is_null() {
            xml_hash_free((*partitions).triage, None);
        }
        xml_free(partitions as _);
    }
}

/**
 * xmlRelaxNGFreeDefine:
 * @define:  a define structure
 *
 * Deallocate a RelaxNG define structure.
 */
unsafe extern "C" fn xml_relaxng_free_define(define: XmlRelaxNGDefinePtr) {
    if define.is_null() {
        return;
    }

    if (*define).typ == XmlRelaxNGType::Value && !(*define).attrs.is_null() {
        let lib: XmlRelaxNGTypeLibraryPtr = (*define).data as _;
        if !lib.is_null() {
            if let Some(freef) = (*lib).freef {
                freef((*lib).data, (*define).attrs as _);
            }
        }
    }
    if !(*define).data.is_null() && (*define).typ == XmlRelaxNGType::Interleave {
        xml_relaxng_free_partition((*define).data as XmlRelaxNGPartitionPtr);
    }
    if !(*define).data.is_null() && (*define).typ == XmlRelaxNGType::Choice {
        xml_hash_free((*define).data as XmlHashTablePtr, None);
    }
    if !(*define).name.is_null() {
        xml_free((*define).name as _);
    }
    if !(*define).ns.is_null() {
        xml_free((*define).ns as _);
    }
    if !(*define).value.is_null() {
        xml_free((*define).value as _);
    }
    if !(*define).cont_model.is_null() {
        xml_reg_free_regexp((*define).cont_model);
    }
    xml_free(define as _);
}

/**
 * xmlRelaxNGFreeInnerSchema:
 * @schema:  a schema structure
 *
 * Deallocate a RelaxNG schema structure.
 */
unsafe extern "C" fn xml_relaxng_free_inner_schema(schema: XmlRelaxNGPtr) {
    if schema.is_null() {
        return;
    }

    if !(*schema).doc.is_null() {
        xml_free_doc((*schema).doc);
    }
    if !(*schema).def_tab.is_null() {
        for i in 0..(*schema).def_nr {
            xml_relaxng_free_define(*(*schema).def_tab.add(i as usize));
        }
        xml_free((*schema).def_tab as _);
    }

    xml_free(schema as _);
}

/**
 * xmlRelaxNGFreeDocument:
 * @docu:  a document structure
 *
 * Deallocate a RelaxNG document structure.
 */
unsafe extern "C" fn xml_relaxng_free_document(docu: XmlRelaxNGDocumentPtr) {
    if docu.is_null() {
        return;
    }

    if !(*docu).href.is_null() {
        xml_free((*docu).href as _);
    }
    if !(*docu).doc.is_null() {
        xml_free_doc((*docu).doc);
    }
    if !(*docu).schema.is_null() {
        xml_relaxng_free_inner_schema((*docu).schema);
    }
    xml_free(docu as _);
}

/**
 * xmlRelaxNGFreeDocumentList:
 * @docu:  a list of  document structure
 *
 * Deallocate a RelaxNG document structures.
 */
unsafe extern "C" fn xml_relaxng_free_document_list(mut docu: XmlRelaxNGDocumentPtr) {
    let mut next: XmlRelaxNGDocumentPtr;

    while !docu.is_null() {
        next = (*docu).next;
        xml_relaxng_free_document(docu);
        docu = next;
    }
}

/**
 * xmlRelaxNGFreeInclude:
 * @incl:  a include structure
 *
 * Deallocate a RelaxNG include structure.
 */
unsafe extern "C" fn xml_relaxng_free_include(incl: XmlRelaxNGIncludePtr) {
    if incl.is_null() {
        return;
    }

    if !(*incl).href.is_null() {
        xml_free((*incl).href as _);
    }
    if !(*incl).doc.is_null() {
        xml_free_doc((*incl).doc);
    }
    if !(*incl).schema.is_null() {
        xml_relaxng_free((*incl).schema);
    }
    xml_free(incl as _);
}

/**
 * xmlRelaxNGFreeIncludeList:
 * @incl:  a include structure list
 *
 * Deallocate a RelaxNG include structure.
 */
unsafe extern "C" fn xml_relaxng_free_include_list(mut incl: XmlRelaxNGIncludePtr) {
    let mut next: XmlRelaxNGIncludePtr;

    while !incl.is_null() {
        next = (*incl).next;
        xml_relaxng_free_include(incl);
        incl = next;
    }
}

/**
 * xmlRelaxNGFreeParserCtxt:
 * @ctxt:  the schema parser context
 *
 * Free the resources associated to the schema parser context
 */
pub unsafe extern "C" fn xml_relaxng_free_parser_ctxt(ctxt: XmlRelaxNGParserCtxtPtr) {
    if ctxt.is_null() {
        return;
    }
    if !(*ctxt).url.is_null() {
        xml_free((*ctxt).url as _);
    }
    if !(*ctxt).doc.is_null() {
        xml_relaxng_free_document((*ctxt).doc);
    }
    if !(*ctxt).interleaves.is_null() {
        xml_hash_free((*ctxt).interleaves, None);
    }
    if !(*ctxt).documents.is_null() {
        xml_relaxng_free_document_list((*ctxt).documents);
    }
    if !(*ctxt).includes.is_null() {
        xml_relaxng_free_include_list((*ctxt).includes);
    }
    if !(*ctxt).doc_tab.is_null() {
        xml_free((*ctxt).doc_tab as _);
    }
    if !(*ctxt).inc_tab.is_null() {
        xml_free((*ctxt).inc_tab as _);
    }
    if !(*ctxt).def_tab.is_null() {
        for i in 0..(*ctxt).def_nr {
            xml_relaxng_free_define(*(*ctxt).def_tab.add(i as usize));
        }
        xml_free((*ctxt).def_tab as _);
    }
    if !(*ctxt).document.is_null() && (*ctxt).freedoc != 0 {
        xml_free_doc((*ctxt).document);
    }
    xml_free(ctxt as _);
}

/**
 * xmlRelaxNGSetParserErrors:
 * @ctxt:  a Relax-NG validation context
 * @err:  the error callback
 * @warn:  the warning callback
 * @ctx:  contextual data for the callbacks
 *
 * Set the callback functions used to handle errors for a validation context
 */
pub unsafe fn xml_relaxng_set_parser_errors(
    ctxt: XmlRelaxNGParserCtxtPtr,
    err: Option<GenericError>,
    warn: Option<GenericError>,
    ctx: Option<GenericErrorContext>,
) {
    if ctxt.is_null() {
        return;
    }
    (*ctxt).error = err;
    (*ctxt).warning = warn;
    (*ctxt).serror = None;
    (*ctxt).user_data = ctx;
}

/**
 * xmlRelaxNGGetParserErrors:
 * @ctxt:  a Relax-NG validation context
 * @err:  the error callback result
 * @warn:  the warning callback result
 * @ctx:  contextual data for the callbacks result
 *
 * Get the callback information used to handle errors for a validation context
 *
 * Returns -1 in case of failure, 0 otherwise.
 */
pub unsafe fn xml_relaxng_get_parser_errors(
    ctxt: XmlRelaxNGParserCtxtPtr,
    err: *mut Option<GenericError>,
    warn: *mut Option<GenericError>,
    ctx: *mut Option<GenericErrorContext>,
) -> i32 {
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
        *ctx = (*ctxt).user_data.clone();
    }
    0
}

/**
 * xmlRelaxNGSetParserStructuredErrors:
 * @ctxt:  a Relax-NG parser context
 * @serror:  the error callback
 * @ctx:  contextual data for the callbacks
 *
 * Set the callback functions used to handle errors for a parsing context
 */
pub unsafe fn xml_relaxng_set_parser_structured_errors(
    ctxt: XmlRelaxNGParserCtxtPtr,
    serror: Option<StructuredError>,
    ctx: Option<GenericErrorContext>,
) {
    if ctxt.is_null() {
        return;
    }
    (*ctxt).serror = serror;
    (*ctxt).error = None;
    (*ctxt).warning = None;
    (*ctxt).user_data = ctx;
}

/**
 * xmlRelaxNGGetElements:
 * @ctxt:  a Relax-NG parser context
 * @def:  the definition definition
 * @eora:  gather elements (0), attributes (1) or elements and text (2)
 *
 * Compute the list of top elements a definition can generate
 *
 * Returns a list of elements or NULL if none was found.
 */
unsafe extern "C" fn xml_relaxng_get_elements(
    ctxt: XmlRelaxNGParserCtxtPtr,
    def: XmlRelaxNGDefinePtr,
    eora: i32,
) -> *mut XmlRelaxNGDefinePtr {
    let mut ret: *mut XmlRelaxNGDefinePtr = null_mut();
    let mut parent: XmlRelaxNGDefinePtr;
    let mut cur: XmlRelaxNGDefinePtr;
    let mut tmp: XmlRelaxNGDefinePtr;
    let mut len: i32 = 0;
    let mut max: i32 = 0;

    /*
     * Don't run that check in case of error. Infinite recursion
     * becomes possible.
     */
    if (*ctxt).nb_errors != 0 {
        return null_mut();
    }

    // parent = null_mut();
    cur = def;
    while !cur.is_null() {
        if (eora == 0 && matches!((*cur).typ, XmlRelaxNGType::Element | XmlRelaxNGType::Text))
            || (eora == 1 && (*cur).typ == XmlRelaxNGType::Attribute)
            || (eora == 2
                && matches!(
                    (*cur).typ,
                    XmlRelaxNGType::Datatype
                        | XmlRelaxNGType::Element
                        | XmlRelaxNGType::List
                        | XmlRelaxNGType::Text
                        | XmlRelaxNGType::Value
                ))
        {
            if ret.is_null() {
                max = 10;
                ret = xml_malloc((max as usize + 1) * size_of::<XmlRelaxNGDefinePtr>()) as _;
                if ret.is_null() {
                    xml_rng_perr_memory(ctxt, c"getting element list\n".as_ptr() as _);
                    return null_mut();
                }
            } else if max <= len {
                max *= 2;
                let temp: *mut XmlRelaxNGDefinePtr = xml_realloc(
                    ret as _,
                    (max as usize + 1) * size_of::<XmlRelaxNGDefinePtr>(),
                ) as _;
                if temp.is_null() {
                    xml_rng_perr_memory(ctxt, c"getting element list\n".as_ptr() as _);
                    xml_free(ret as _);
                    return null_mut();
                }
                ret = temp;
            }
            *ret.add(len as usize) = cur;
            len += 1;
            *ret.add(len as usize) = null_mut();
        } else if matches!(
            (*cur).typ,
            XmlRelaxNGType::Choice
                | XmlRelaxNGType::Interleave
                | XmlRelaxNGType::Group
                | XmlRelaxNGType::Oneormore
                | XmlRelaxNGType::Zeroormore
                | XmlRelaxNGType::Optional
                | XmlRelaxNGType::Parentref
                | XmlRelaxNGType::Ref
                | XmlRelaxNGType::Def
                | XmlRelaxNGType::Externalref
        ) {
            /*
             * Don't go within elements or attributes or string values.
             * Just gather the element top list
             */
            if !(*cur).content.is_null() {
                parent = cur;
                cur = (*cur).content;
                tmp = cur;
                while !tmp.is_null() {
                    (*tmp).parent = parent;
                    tmp = (*tmp).next;
                }
                continue;
            }
        }
        if cur == def {
            break;
        }
        if !(*cur).next.is_null() {
            cur = (*cur).next;
            continue;
        }
        loop {
            cur = (*cur).parent;
            if cur.is_null() {
                break;
            }
            if cur == def {
                return ret;
            }
            if !(*cur).next.is_null() {
                cur = (*cur).next;
                break;
            }

            if cur.is_null() {
                break;
            }
        }
    }
    ret
}

/**
 * xmlRelaxNGPopErrors:
 * @ctxt:  the validation context
 * @level:  the error level in the stack
 *
 * pop and discard all errors until the given level is reached
 */
unsafe extern "C" fn xml_relaxng_pop_errors(ctxt: XmlRelaxNGValidCtxtPtr, level: i32) {
    let mut err: XmlRelaxNGValidErrorPtr;

    for i in level..(*ctxt).err_nr {
        err = (*ctxt).err_tab.add(i as usize);
        if (*err).flags & ERROR_IS_DUP != 0 {
            if !(*err).arg1.is_null() {
                xml_free((*err).arg1 as _);
            }
            (*err).arg1 = null_mut();
            if !(*err).arg2.is_null() {
                xml_free((*err).arg2 as _);
            }
            (*err).arg2 = null_mut();
            (*err).flags = 0;
        }
    }
    (*ctxt).err_nr = level;
    if (*ctxt).err_nr <= 0 {
        (*ctxt).err = null_mut();
    }
}

const INVALID_NAME: &CStr = c"\u{1}";

/**
 * xmlRelaxNGElementMatch:
 * @ctxt:  a Relax-NG validation context
 * @define:  the definition to check
 * @elem:  the element
 *
 * Check if the element matches the definition nameClass
 *
 * Returns 1 if the element matches, 0 if no, or -1 in case of error
 */
unsafe extern "C" fn xml_relaxng_element_match(
    ctxt: XmlRelaxNGValidCtxtPtr,
    mut define: XmlRelaxNGDefinePtr,
    elem: XmlNodePtr,
) -> i32 {
    let mut ret: i32 = 0;
    let mut oldflags: i32 = 0;

    if !(*define).name.is_null() && !xml_str_equal((*elem).name, (*define).name) {
        VALID_ERR3!(
            ctxt,
            XmlRelaxNGValidErr::XmlRelaxngErrElemname,
            (*define).name,
            (*elem).name
        );
        return 0;
    }
    if !(*define).ns.is_null() && *(*define).ns.add(0) != 0 {
        if (*elem).ns.is_null() {
            VALID_ERR2!(
                ctxt,
                XmlRelaxNGValidErr::XmlRelaxngErrElemnons,
                (*elem).name
            );
            return 0;
        } else if !xml_str_equal((*(*elem).ns).href.load(Ordering::Relaxed), (*define).ns) {
            VALID_ERR3!(
                ctxt,
                XmlRelaxNGValidErr::XmlRelaxngErrElemwrongns,
                (*elem).name,
                (*define).ns
            );
            return 0;
        }
    } else if !(*elem).ns.is_null() && !(*define).ns.is_null() && (*define).name.is_null() {
        VALID_ERR2!(
            ctxt,
            XmlRelaxNGValidErr::XmlRelaxngErrElemextrans,
            (*elem).name
        );
        return 0;
    } else if !(*elem).ns.is_null() && !(*define).name.is_null() {
        VALID_ERR2!(
            ctxt,
            XmlRelaxNGValidErr::XmlRelaxngErrElemextrans,
            (*define).name
        );
        return 0;
    }

    if (*define).name_class.is_null() {
        return 1;
    }

    define = (*define).name_class;
    if (*define).typ == XmlRelaxNGType::Except {
        let mut list: XmlRelaxNGDefinePtr;

        if !ctxt.is_null() {
            oldflags = (*ctxt).flags;
            (*ctxt).flags |= FLAGS_IGNORABLE;
        }

        list = (*define).content;
        while !list.is_null() {
            ret = xml_relaxng_element_match(ctxt, list, elem);
            if ret == 1 {
                if !ctxt.is_null() {
                    (*ctxt).flags = oldflags;
                }
                return 0;
            }
            if ret < 0 {
                if !ctxt.is_null() {
                    (*ctxt).flags = oldflags;
                }
                return ret;
            }
            list = (*list).next;
        }
        ret = 1;
        if !ctxt.is_null() {
            (*ctxt).flags = oldflags;
        }
    } else if (*define).typ == XmlRelaxNGType::Choice {
        let mut list: XmlRelaxNGDefinePtr;

        if !ctxt.is_null() {
            oldflags = (*ctxt).flags;
            (*ctxt).flags |= FLAGS_IGNORABLE;
        }

        list = (*define).name_class;
        while !list.is_null() {
            ret = xml_relaxng_element_match(ctxt, list, elem);
            if ret == 1 {
                if !ctxt.is_null() {
                    (*ctxt).flags = oldflags;
                }
                return 1;
            }
            if ret < 0 {
                if !ctxt.is_null() {
                    (*ctxt).flags = oldflags;
                }
                return ret;
            }
            list = (*list).next;
        }
        if !ctxt.is_null() {
            if ret != 0 {
                if (*ctxt).flags & FLAGS_IGNORABLE == 0 {
                    xml_relaxng_dump_valid_error(ctxt);
                }
            } else if (*ctxt).err_nr > 0 {
                xml_relaxng_pop_errors(ctxt, 0);
            }
        }
        ret = 0;
        if !ctxt.is_null() {
            (*ctxt).flags = oldflags;
        }
    } else {
        // TODO
        ret = -1;
    }
    ret
}

/**
 * xmlRelaxNGCompareNameClasses:
 * @defs1:  the first element/attribute defs
 * @defs2:  the second element/attribute defs
 * @name:  the restriction on the name
 * @ns:  the restriction on the namespace
 *
 * Compare the 2 lists of element definitions. The comparison is
 * that if both lists do not accept the same QNames, it returns 1
 * If the 2 lists can accept the same QName the comparison returns 0
 *
 * Returns 1 distinct, 0 if equal
 */
unsafe extern "C" fn xml_relaxng_compare_name_classes(
    def1: XmlRelaxNGDefinePtr,
    def2: XmlRelaxNGDefinePtr,
) -> i32 {
    let mut ret: i32;
    let mut node: XmlNode = unsafe { zeroed() };
    let mut ns: XmlNs = unsafe { zeroed() };
    let mut ctxt: XmlRelaxNGValidCtxt = unsafe { zeroed() };

    memset(addr_of_mut!(ctxt) as _, 0, size_of::<XmlRelaxNGValidCtxt>());

    ctxt.flags = FLAGS_IGNORABLE | FLAGS_NOERROR;

    if matches!(
        (*def1).typ,
        XmlRelaxNGType::Element | XmlRelaxNGType::Attribute
    ) {
        if (*def2).typ == XmlRelaxNGType::Text {
            return 1;
        }
        if !(*def1).name.is_null() {
            node.name = (*def1).name;
        } else {
            node.name = INVALID_NAME.as_ptr() as _;
        }
        if !(*def1).ns.is_null() {
            if *(*def1).ns.add(0) == 0 {
                node.ns = null_mut();
            } else {
                node.ns = addr_of_mut!(ns);
                ns.href.store((*def1).ns, Ordering::Relaxed);
            }
        } else {
            node.ns = null_mut();
        }
        if xml_relaxng_element_match(addr_of_mut!(ctxt), def2, addr_of_mut!(node)) != 0 {
            if !(*def1).name_class.is_null() {
                ret = xml_relaxng_compare_name_classes((*def1).name_class, def2);
            } else {
                ret = 0;
            }
        } else {
            ret = 1;
        }
    } else if (*def1).typ == XmlRelaxNGType::Text {
        if (*def2).typ == XmlRelaxNGType::Text {
            return 0;
        }
        return 1;
    } else if (*def1).typ == XmlRelaxNGType::Except {
        ret = xml_relaxng_compare_name_classes((*def1).content, def2);
        if ret == 0 {
            ret = 1;
        } else if ret == 1 {
            ret = 0;
        }
    } else {
        // TODO
        ret = 0;
    }
    if ret == 0 {
        return ret;
    }
    if matches!(
        (*def2).typ,
        XmlRelaxNGType::Element | XmlRelaxNGType::Attribute
    ) {
        if !(*def2).name.is_null() {
            node.name = (*def2).name;
        } else {
            node.name = INVALID_NAME.as_ptr() as _;
        }
        node.ns = addr_of_mut!(ns);
        if !(*def2).ns.is_null() {
            if *(*def2).ns.add(0) == 0 {
                node.ns = null_mut();
            } else {
                ns.href.store((*def2).ns, Ordering::Relaxed);
            }
        } else {
            ns.href.store(INVALID_NAME.as_ptr() as _, Ordering::Relaxed);
        }
        if xml_relaxng_element_match(addr_of_mut!(ctxt), def1, addr_of_mut!(node)) != 0 {
            if !(*def2).name_class.is_null() {
                ret = xml_relaxng_compare_name_classes((*def2).name_class, def1);
            } else {
                ret = 0;
            }
        } else {
            ret = 1;
        }
    } else {
        // TODO
        ret = 0;
    }

    ret
}

/**
 * xmlRelaxNGCompareElemDefLists:
 * @ctxt:  a Relax-NG parser context
 * @defs1:  the first list of element/attribute defs
 * @defs2:  the second list of element/attribute defs
 *
 * Compare the 2 lists of element or attribute definitions. The comparison
 * is that if both lists do not accept the same QNames, it returns 1
 * If the 2 lists can accept the same QName the comparison returns 0
 *
 * Returns 1 distinct, 0 if equal
 */
unsafe extern "C" fn xml_relaxng_compare_elem_def_lists(
    _ctxt: XmlRelaxNGParserCtxtPtr,
    mut def1: *mut XmlRelaxNGDefinePtr,
    mut def2: *mut XmlRelaxNGDefinePtr,
) -> i32 {
    let basedef2: *mut XmlRelaxNGDefinePtr = def2;

    if def1.is_null() || def2.is_null() {
        return 1;
    }
    if (*def1).is_null() || (*def2).is_null() {
        return 1;
    }
    while !(*def1).is_null() {
        while !(*def2).is_null() {
            if xml_relaxng_compare_name_classes(*def1, *def2) == 0 {
                return 0;
            }
            def2 = def2.add(1);
        }
        def2 = basedef2;
        def1 = def1.add(1);
    }
    1
}

/**
 * xmlRngPErr:
 * @ctxt:  a Relax-NG parser context
 * @node:  the node raising the error
 * @error:  the error code
 * @msg:  message
 * @str1:  extra info
 * @str2:  extra info
 *
 * Handle a Relax NG Parsing error
 */
unsafe extern "C" fn xml_rng_perr(
    ctxt: XmlRelaxNGParserCtxtPtr,
    node: XmlNodePtr,
    error: XmlParserErrors,
    msg: *const c_char,
    str1: *const XmlChar,
    str2: *const XmlChar,
) {
    let mut schannel: Option<StructuredError> = None;
    let mut channel: Option<GenericError> = None;
    let mut data = None;

    if !ctxt.is_null() {
        if (*ctxt).serror.is_some() {
            schannel = (*ctxt).serror;
        } else {
            channel = (*ctxt).error;
        }
        data = (*ctxt).user_data.clone();
        (*ctxt).nb_errors += 1;
    }
    __xml_raise_error!(
        schannel,
        channel,
        data,
        null_mut(),
        node as _,
        XmlErrorDomain::XmlFromRelaxngp,
        error,
        XmlErrorLevel::XmlErrError,
        null_mut(),
        0,
        (!str1.is_null()).then(|| CStr::from_ptr(str1 as *const i8)
            .to_string_lossy()
            .into_owned()
            .into()),
        (!str2.is_null()).then(|| CStr::from_ptr(str2 as *const i8)
            .to_string_lossy()
            .into_owned()
            .into()),
        None,
        0,
        0,
        msg,
        str1,
        str2
    );
}

/**
 * xmlRelaxNGComputeInterleaves:
 * @def:  the interleave definition
 * @ctxt:  a Relax-NG parser context
 * @name:  the definition name
 *
 * A lot of work for preprocessing interleave definitions
 * is potentially needed to get a decent execution speed at runtime
 *   - trying to get a total order on the element nodes generated
 *     by the interleaves, order the list of interleave definitions
 *     following that order.
 *   - if <text/> is used to handle mixed content, it is better to
 *     flag this in the define and simplify the runtime checking
 *     algorithm
 */
extern "C" fn xml_relaxng_compute_interleaves(
    payload: *mut c_void,
    data: *mut c_void,
    _name: *const XmlChar,
) {
    let def: XmlRelaxNGDefinePtr = payload as _;
    let ctxt: XmlRelaxNGParserCtxtPtr = data as _;
    let mut cur: XmlRelaxNGDefinePtr;
    let mut tmp: *mut XmlRelaxNGDefinePtr;
    let mut partitions: XmlRelaxNGPartitionPtr = null_mut();
    let mut group: XmlRelaxNGInterleaveGroupPtr;
    let mut ret: i32;
    let mut res: i32;
    let mut nbgroups: i32 = 0;
    let mut nbchild: i32 = 0;
    let mut is_mixed: i32 = 0;
    let mut is_determinist: i32 = 1;

    unsafe {
        /*
         * Don't run that check in case of error. Infinite recursion
         * becomes possible.
         */
        if (*ctxt).nb_errors != 0 {
            return;
        }

        cur = (*def).content;
        while !cur.is_null() {
            nbchild += 1;
            cur = (*cur).next;
        }

        let groups: *mut XmlRelaxNGInterleaveGroupPtr =
            xml_malloc(nbchild as usize * size_of::<XmlRelaxNGInterleaveGroupPtr>()) as _;
        'goto_error: {
            if groups.is_null() {
                break 'goto_error;
            }
            cur = (*def).content;
            while !cur.is_null() {
                *groups.add(nbgroups as usize) =
                    xml_malloc(size_of::<XmlRelaxNGInterleaveGroup>()) as _;
                if (*groups.add(nbgroups as usize)).is_null() {
                    break 'goto_error;
                }
                if (*cur).typ == XmlRelaxNGType::Text {
                    is_mixed += 1;
                }
                (*(*groups.add(nbgroups as usize))).rule = cur;
                (*(*groups.add(nbgroups as usize))).defs = xml_relaxng_get_elements(ctxt, cur, 2);
                (*(*groups.add(nbgroups as usize))).attrs = xml_relaxng_get_elements(ctxt, cur, 1);
                nbgroups += 1;
                cur = (*cur).next;
            }

            /*
             * Let's check that all rules makes a partitions according to 7.4
             */
            partitions = xml_malloc(size_of::<XmlRelaxNGPartition>()) as _;
            if partitions.is_null() {
                break 'goto_error;
            }
            memset(partitions as _, 0, size_of::<XmlRelaxNGPartition>());
            (*partitions).nbgroups = nbgroups;
            (*partitions).triage = xml_hash_create(nbgroups);
            for i in 0..nbgroups {
                group = *groups.add(i as usize);
                for j in i + 1..nbgroups {
                    if (*groups.add(j as usize)).is_null() {
                        continue;
                    }

                    ret = xml_relaxng_compare_elem_def_lists(
                        ctxt,
                        (*group).defs,
                        (*(*groups.add(j as usize))).defs,
                    );
                    if ret == 0 {
                        xml_rng_perr(
                            ctxt,
                            (*def).node,
                            XmlParserErrors::XmlRngpElemTextConflict,
                            c"Element or text conflicts in interleave\n".as_ptr() as _,
                            null_mut(),
                            null_mut(),
                        );
                    }
                    ret = xml_relaxng_compare_elem_def_lists(
                        ctxt,
                        (*group).attrs,
                        (*(*groups.add(j as usize))).attrs,
                    );
                    if ret == 0 {
                        xml_rng_perr(
                            ctxt,
                            (*def).node,
                            XmlParserErrors::XmlRngpAttrConflict,
                            c"Attributes conflicts in interleave\n".as_ptr() as _,
                            null_mut(),
                            null_mut(),
                        );
                    }
                }
                tmp = (*group).defs;
                if !tmp.is_null() && !(*tmp).is_null() {
                    while !(*tmp).is_null() {
                        if (*(*tmp)).typ == XmlRelaxNGType::Text {
                            res = xml_hash_add_entry2(
                                (*partitions).triage,
                                c"#text".as_ptr() as _,
                                null_mut(),
                                (i + 1) as _,
                            );
                            if res != 0 {
                                is_determinist = -1;
                            }
                        } else if (*(*tmp)).typ == XmlRelaxNGType::Element
                            && !(*(*tmp)).name.is_null()
                        {
                            if (*(*tmp)).ns.is_null() || *(*(*tmp)).ns.add(0) == 0 {
                                res = xml_hash_add_entry2(
                                    (*partitions).triage,
                                    (*(*tmp)).name,
                                    null_mut(),
                                    (i + 1) as _,
                                );
                            } else {
                                res = xml_hash_add_entry2(
                                    (*partitions).triage,
                                    (*(*tmp)).name,
                                    (*(*tmp)).ns,
                                    (i + 1) as _,
                                );
                            }
                            if res != 0 {
                                is_determinist = -1;
                            }
                        } else if (*(*tmp)).typ == XmlRelaxNGType::Element {
                            if (*(*tmp)).ns.is_null() || *(*(*tmp)).ns.add(0) == 0 {
                                res = xml_hash_add_entry2(
                                    (*partitions).triage,
                                    c"#any".as_ptr() as _,
                                    null_mut(),
                                    (i + 1) as _,
                                );
                            } else {
                                res = xml_hash_add_entry2(
                                    (*partitions).triage,
                                    c"#any".as_ptr() as _,
                                    (*(*tmp)).ns,
                                    (i + 1) as _,
                                );
                            }
                            if !(*(*tmp)).name_class.is_null() {
                                is_determinist = 2;
                            }
                            if res != 0 {
                                is_determinist = -1;
                            }
                        } else {
                            is_determinist = -1;
                        }
                        tmp = tmp.add(1);
                    }
                } else {
                    is_determinist = 0;
                }
            }
            (*partitions).groups = groups;

            /*
             * and save the partition list back in the def
             */
            (*def).data = partitions as _;
            if is_mixed != 0 {
                (*def).dflags |= IS_MIXED as i16;
            }
            if is_determinist == 1 {
                (*partitions).flags = IS_DETERMINIST;
            }
            if is_determinist == 2 {
                (*partitions).flags = IS_DETERMINIST | IS_NEEDCHECK;
            }
            return;
        }

        //   error:
        xml_rng_perr_memory(ctxt, c"in interleave computation\n".as_ptr() as _);
        if !groups.is_null() {
            for i in 0..nbgroups {
                if !(*groups.add(i as usize)).is_null() {
                    if !(*(*groups.add(i as usize))).defs.is_null() {
                        xml_free((*(*groups.add(i as usize))).defs as _);
                    }
                    xml_free(*groups.add(i as usize) as _);
                }
            }
            xml_free(groups as _);
        }
        xml_relaxng_free_partition(partitions);
    }
}

macro_rules! IS_BLANK_NODE {
    ($n:expr) => {
        xml_relaxng_is_blank((*$n).content) != 0
    };
}

/**
 * xmlRelaxNGIsBlank:
 * @str:  a string
 *
 * Check if a string is ignorable c.f. 4.2. Whitespace
 *
 * Returns 1 if the string is NULL or made of blanks chars, 0 otherwise
 */
unsafe extern "C" fn xml_relaxng_is_blank(mut str: *mut XmlChar) -> i32 {
    if str.is_null() {
        return 1;
    }
    while *str != 0 {
        if !xml_is_blank_char(*str as u32) {
            return 0;
        }
        str = str.add(1);
    }
    1
}

/**
 * xmlRelaxNGCleanupAttributes:
 * @ctxt:  a Relax-NG parser context
 * @node:  a Relax-NG node
 *
 * Check all the attributes on the given node
 */
unsafe extern "C" fn xml_relaxng_cleanup_attributes(
    ctxt: XmlRelaxNGParserCtxtPtr,
    node: XmlNodePtr,
) {
    let mut cur: XmlAttrPtr;
    let mut next: XmlAttrPtr;

    cur = (*node).properties;
    while !cur.is_null() {
        next = (*cur).next;
        if (*cur).ns.is_null()
            || xml_str_equal(
                (*(*cur).ns).href.load(Ordering::Relaxed),
                XML_RELAXNG_NS.as_ptr() as _,
            )
        {
            if xml_str_equal((*cur).name, c"name".as_ptr() as _) {
                if !xml_str_equal((*node).name, c"element".as_ptr() as _)
                    && !xml_str_equal((*node).name, c"attribute".as_ptr() as _)
                    && !xml_str_equal((*node).name, c"ref".as_ptr() as _)
                    && !xml_str_equal((*node).name, c"parentRef".as_ptr() as _)
                    && !xml_str_equal((*node).name, c"param".as_ptr() as _)
                    && !xml_str_equal((*node).name, c"define".as_ptr() as _)
                {
                    xml_rng_perr(
                        ctxt,
                        node,
                        XmlParserErrors::XmlRngpForbiddenAttribute,
                        c"Attribute %s is not allowed on %s\n".as_ptr() as _,
                        (*cur).name,
                        (*node).name,
                    );
                }
            } else if xml_str_equal((*cur).name, c"type".as_ptr() as _) {
                if !xml_str_equal((*node).name, c"value".as_ptr() as _)
                    && !xml_str_equal((*node).name, c"data".as_ptr() as _)
                {
                    xml_rng_perr(
                        ctxt,
                        node,
                        XmlParserErrors::XmlRngpForbiddenAttribute,
                        c"Attribute %s is not allowed on %s\n".as_ptr() as _,
                        (*cur).name,
                        (*node).name,
                    );
                }
            } else if xml_str_equal((*cur).name, c"href".as_ptr() as _) {
                if !xml_str_equal((*node).name, c"externalRef".as_ptr() as _)
                    && !xml_str_equal((*node).name, c"include".as_ptr() as _)
                {
                    xml_rng_perr(
                        ctxt,
                        node,
                        XmlParserErrors::XmlRngpForbiddenAttribute,
                        c"Attribute %s is not allowed on %s\n".as_ptr() as _,
                        (*cur).name,
                        (*node).name,
                    );
                }
            } else if xml_str_equal((*cur).name, c"combine".as_ptr() as _) {
                if !xml_str_equal((*node).name, c"start".as_ptr() as _)
                    && !xml_str_equal((*node).name, c"define".as_ptr() as _)
                {
                    xml_rng_perr(
                        ctxt,
                        node,
                        XmlParserErrors::XmlRngpForbiddenAttribute,
                        c"Attribute %s is not allowed on %s\n".as_ptr() as _,
                        (*cur).name,
                        (*node).name,
                    );
                }
            } else if xml_str_equal((*cur).name, c"datatypeLibrary".as_ptr() as _) {
                let uri: XmlURIPtr;

                let val: *mut XmlChar = xml_node_list_get_string((*node).doc, (*cur).children, 1);
                if !val.is_null() {
                    if *val.add(0) != 0 {
                        uri = xml_parse_uri(val as _);
                        if uri.is_null() {
                            xml_rng_perr(
                                ctxt,
                                node,
                                XmlParserErrors::XmlRngpInvalidURI,
                                c"Attribute %s contains invalid URI %s\n".as_ptr() as _,
                                (*cur).name,
                                val,
                            );
                        } else {
                            if (*uri).scheme.is_null() {
                                xml_rng_perr(
                                    ctxt,
                                    node,
                                    XmlParserErrors::XmlRngpURINotAbsolute,
                                    c"Attribute %s URI %s is not absolute\n".as_ptr() as _,
                                    (*cur).name,
                                    val,
                                );
                            }
                            if !(*uri).fragment.is_null() {
                                xml_rng_perr(
                                    ctxt,
                                    node,
                                    XmlParserErrors::XmlRngpURIFragment,
                                    c"Attribute %s URI %s has a fragment ID\n".as_ptr() as _,
                                    (*cur).name,
                                    val,
                                );
                            }
                            xml_free_uri(uri);
                        }
                    }
                    xml_free(val as _);
                }
            } else if !xml_str_equal((*cur).name, c"ns".as_ptr() as _) {
                xml_rng_perr(
                    ctxt,
                    node,
                    XmlParserErrors::XmlRngpUnknownAttribute,
                    c"Unknown attribute %s on %s\n".as_ptr() as _,
                    (*cur).name,
                    (*node).name,
                );
            }
        }
        cur = next;
    }
}

/**
 * xmlRelaxNGDocumentPush:
 * @ctxt:  the parser context
 * @value:  the element doc
 *
 * Pushes a new doc on top of the doc stack
 *
 * Returns 0 in case of error, the index in the stack otherwise
 */
unsafe extern "C" fn xml_relaxng_document_push(
    ctxt: XmlRelaxNGParserCtxtPtr,
    value: XmlRelaxNGDocumentPtr,
) -> i32 {
    if (*ctxt).doc_tab.is_null() {
        (*ctxt).doc_max = 4;
        (*ctxt).doc_nr = 0;
        (*ctxt).doc_tab =
            xml_malloc((*ctxt).doc_max as usize * size_of_val(&*(*ctxt).doc_tab.add(0))) as _;
        if (*ctxt).doc_tab.is_null() {
            xml_rng_perr_memory(ctxt, c"adding document\n".as_ptr() as _);
            return 0;
        }
    }
    if (*ctxt).doc_nr >= (*ctxt).doc_max {
        (*ctxt).doc_max *= 2;
        (*ctxt).doc_tab = xml_realloc(
            (*ctxt).doc_tab as _,
            (*ctxt).doc_max as usize * size_of_val(&*(*ctxt).doc_tab.add(0)),
        ) as _;
        if (*ctxt).doc_tab.is_null() {
            xml_rng_perr_memory(ctxt, c"adding document\n".as_ptr() as _);
            return 0;
        }
    }
    *(*ctxt).doc_tab.add((*ctxt).doc_nr as usize) = value;
    (*ctxt).doc = value;
    (*ctxt).doc_nr += 1;
    (*ctxt).doc_nr - 1
}

/**
 * xmlRelaxNGDocumentPop:
 * @ctxt: the parser context
 *
 * Pops the top doc from the doc stack
 *
 * Returns the doc just removed
 */
unsafe extern "C" fn xml_relaxng_document_pop(
    ctxt: XmlRelaxNGParserCtxtPtr,
) -> XmlRelaxNGDocumentPtr {
    if (*ctxt).doc_nr <= 0 {
        return null_mut();
    }
    (*ctxt).doc_nr -= 1;
    if (*ctxt).doc_nr > 0 {
        (*ctxt).doc = *(*ctxt).doc_tab.add((*ctxt).doc_nr as usize - 1);
    } else {
        (*ctxt).doc = null_mut();
    }
    let ret: XmlRelaxNGDocumentPtr = *(*ctxt).doc_tab.add((*ctxt).doc_nr as usize);
    *(*ctxt).doc_tab.add((*ctxt).doc_nr as usize) = null_mut();
    ret
}

/**
 * xmlRelaxNGLoadExternalRef:
 * @ctxt: the parser context
 * @URL:  the normalized URL
 * @ns:  the inherited ns if any
 *
 * First lookup if the document is already loaded into the parser context,
 * check against recursion. If not found the resource is loaded and
 * the content is preprocessed before being returned back to the caller.
 *
 * Returns the xmlRelaxNGDocumentPtr or NULL in case of error
 */
unsafe extern "C" fn xml_relaxng_load_external_ref(
    ctxt: XmlRelaxNGParserCtxtPtr,
    url: *const XmlChar,
    ns: *const XmlChar,
) -> XmlRelaxNGDocumentPtr {
    let mut doc: XmlDocPtr;
    let root: XmlNodePtr;

    /*
     * check against recursion in the stack
     */
    for i in 0..(*ctxt).doc_nr {
        if xml_str_equal((*(*(*ctxt).doc_tab.add(i as usize))).href, url) {
            xml_rng_perr(
                ctxt,
                null_mut(),
                XmlParserErrors::XmlRngpExternalRefRecurse,
                c"Detected an externalRef recursion for %s\n".as_ptr() as _,
                url,
                null_mut(),
            );
            return null_mut();
        }
    }

    /*
     * load the document
     */
    doc = xml_read_file(url as _, None, 0);
    if doc.is_null() {
        xml_rng_perr(
            ctxt,
            null_mut(),
            XmlParserErrors::XmlRngpParseError,
            c"xmlRelaxNG: could not load %s\n".as_ptr() as _,
            url,
            null_mut(),
        );
        return null_mut();
    }

    /*
     * Allocate the document structures and register it first.
     */
    let ret: XmlRelaxNGDocumentPtr = xml_malloc(size_of::<XmlRelaxNGDocument>()) as _;
    if ret.is_null() {
        xml_rng_perr(
            ctxt,
            doc as _,
            XmlParserErrors::XmlErrNoMemory,
            c"xmlRelaxNG: allocate memory for doc %s\n".as_ptr() as _,
            url,
            null_mut(),
        );
        xml_free_doc(doc);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlRelaxNGDocument>());
    (*ret).doc = doc;
    (*ret).href = xml_strdup(url);
    (*ret).next = (*ctxt).documents;
    (*ret).external_ref = 1;
    (*ctxt).documents = ret;

    /*
     * transmit the ns if needed
     */
    if !ns.is_null() {
        root = (*doc).get_root_element();
        if !root.is_null() && xml_has_prop(root, c"ns".as_ptr() as _).is_null() {
            xml_set_prop(root, c"ns".as_ptr() as _, ns);
        }
    }

    /*
     * push it on the stack and register it in the hash table
     */
    xml_relaxng_document_push(ctxt, ret);

    /*
     * Some preprocessing of the document content
     */
    doc = xml_relaxng_cleanup_doc(ctxt, doc);
    if doc.is_null() {
        (*ctxt).doc = null_mut();
        return null_mut();
    }

    xml_relaxng_document_pop(ctxt);

    ret
}

/**
 * xmlRelaxNGIncludePush:
 * @ctxt:  the parser context
 * @value:  the element doc
 *
 * Pushes a new include on top of the include stack
 *
 * Returns 0 in case of error, the index in the stack otherwise
 */
unsafe extern "C" fn xml_relaxng_include_push(
    ctxt: XmlRelaxNGParserCtxtPtr,
    value: XmlRelaxNGIncludePtr,
) -> i32 {
    if (*ctxt).inc_tab.is_null() {
        (*ctxt).inc_max = 4;
        (*ctxt).inc_nr = 0;
        (*ctxt).inc_tab =
            xml_malloc((*ctxt).inc_max as usize * size_of_val(&*(*ctxt).inc_tab.add(0))) as _;
        if (*ctxt).inc_tab.is_null() {
            xml_rng_perr_memory(ctxt, c"allocating include\n".as_ptr() as _);
            return 0;
        }
    }
    if (*ctxt).inc_nr >= (*ctxt).inc_max {
        (*ctxt).inc_max *= 2;
        (*ctxt).inc_tab = xml_realloc(
            (*ctxt).inc_tab as _,
            (*ctxt).inc_max as usize * size_of_val(&*(*ctxt).inc_tab.add(0)),
        ) as _;
        if (*ctxt).inc_tab.is_null() {
            xml_rng_perr_memory(ctxt, c"allocating include\n".as_ptr() as _);
            return 0;
        }
    }
    *(*ctxt).inc_tab.add((*ctxt).inc_nr as usize) = value;
    (*ctxt).inc = value;
    (*ctxt).inc_nr += 1;
    (*ctxt).inc_nr - 1
}

/**
 * xmlRelaxNGIncludePop:
 * @ctxt: the parser context
 *
 * Pops the top include from the include stack
 *
 * Returns the include just removed
 */
unsafe extern "C" fn xml_relaxng_include_pop(
    ctxt: XmlRelaxNGParserCtxtPtr,
) -> XmlRelaxNGIncludePtr {
    if (*ctxt).inc_nr <= 0 {
        return null_mut();
    }
    (*ctxt).inc_nr -= 1;
    if (*ctxt).inc_nr > 0 {
        (*ctxt).inc = *(*ctxt).inc_tab.add((*ctxt).inc_nr as usize - 1);
    } else {
        (*ctxt).inc = null_mut();
    }
    let ret: XmlRelaxNGIncludePtr = *(*ctxt).inc_tab.add((*ctxt).inc_nr as usize);
    *(*ctxt).inc_tab.add((*ctxt).inc_nr as usize) = null_mut();
    ret
}

/**
 * xmlRelaxNGNormExtSpace:
 * @value:  a value
 *
 * Removes the leading and ending spaces of the value
 * The string is modified "in situ"
 */
unsafe extern "C" fn xml_relaxng_norm_ext_space(value: *mut XmlChar) {
    let mut start: *mut XmlChar = value;
    let mut cur: *mut XmlChar = value;

    if value.is_null() {
        return;
    }

    while xml_is_blank_char(*cur as u32) {
        cur = cur.add(1);
    }
    if cur == start {
        loop {
            while *cur != 0 && !xml_is_blank_char(*cur as u32) {
                cur = cur.add(1);
            }
            if *cur == 0 {
                return;
            }
            start = cur;
            while xml_is_blank_char(*cur as u32) {
                cur = cur.add(1);
            }
            if *cur == 0 {
                *start = 0;
                return;
            }
        }
    } else {
        loop {
            while *cur != 0 && !xml_is_blank_char(*cur as u32) {
                *start = *cur;
                start = start.add(1);
                cur = cur.add(1);
            }
            if *cur == 0 {
                *start = 0;
                return;
            }
            /* don't try to normalize the inner spaces */
            while xml_is_blank_char(*cur as u32) {
                cur = cur.add(1);
            }
            if *cur == 0 {
                *start = 0;
                return;
            }
            *start = *cur;
            start = start.add(1);
            cur = cur.add(1);
        }
    }
}

/**
 * xmlRelaxNGRemoveRedefine:
 * @ctxt: the parser context
 * @URL:  the normalized URL
 * @target:  the included target
 * @name:  the define name to eliminate
 *
 * Applies the elimination algorithm of 4.7
 *
 * Returns 0 in case of error, 1 in case of success.
 */
unsafe extern "C" fn xml_relaxng_remove_redefine(
    _ctxt: XmlRelaxNGParserCtxtPtr,
    _url: *const XmlChar,
    target: XmlNodePtr,
    name: *const XmlChar,
) -> i32 {
    let mut found: i32 = 0;
    let mut tmp: XmlNodePtr;
    let mut tmp2: XmlNodePtr;
    let mut name2: *mut XmlChar;

    tmp = target;
    while !tmp.is_null() {
        tmp2 = (*tmp).next;
        if name.is_null() && IS_RELAXNG!(tmp, c"start".as_ptr() as _) {
            found = 1;
            xml_unlink_node(tmp);
            xml_free_node(tmp);
        } else if !name.is_null() && IS_RELAXNG!(tmp, c"define".as_ptr() as _) {
            name2 = xml_get_prop(tmp, c"name".as_ptr() as _);
            xml_relaxng_norm_ext_space(name2);
            if !name2.is_null() {
                if xml_str_equal(name, name2) {
                    found = 1;
                    xml_unlink_node(tmp);
                    xml_free_node(tmp);
                }
                xml_free(name2 as _);
            }
        } else if IS_RELAXNG!(tmp, c"include".as_ptr() as _) {
            let href: *mut XmlChar = null_mut();
            let inc: XmlRelaxNGDocumentPtr = (*tmp).psvi as _;

            if (!inc.is_null() && !(*inc).doc.is_null() && !(*(*inc).doc).children.is_null())
                && xml_str_equal((*(*(*inc).doc).children).name, c"grammar".as_ptr() as _)
                && xml_relaxng_remove_redefine(
                    _ctxt,
                    href,
                    (*(*(*inc).doc).get_root_element()).children,
                    name,
                ) == 1
            {
                found = 1;
            }
            if xml_relaxng_remove_redefine(_ctxt, _url, (*tmp).children, name) == 1 {
                found = 1;
            }
        }
        tmp = tmp2;
    }
    found
}

/**
 * xmlRelaxNGLoadInclude:
 * @ctxt: the parser context
 * @URL:  the normalized URL
 * @node: the include node.
 * @ns:  the namespace passed from the context.
 *
 * First lookup if the document is already loaded into the parser context,
 * check against recursion. If not found the resource is loaded and
 * the content is preprocessed before being returned back to the caller.
 *
 * Returns the xmlRelaxNGIncludePtr or NULL in case of error
 */
unsafe extern "C" fn xml_relaxng_load_include(
    ctxt: XmlRelaxNGParserCtxtPtr,
    url: *const XmlChar,
    node: XmlNodePtr,
    ns: *const XmlChar,
) -> XmlRelaxNGIncludePtr {
    let mut doc: XmlDocPtr;
    let mut root: XmlNodePtr;
    let mut cur: XmlNodePtr;

    /*
     * check against recursion in the stack
     */
    for i in 0..(*ctxt).inc_nr {
        if xml_str_equal((*(*(*ctxt).inc_tab.add(i as usize))).href, url) {
            xml_rng_perr(
                ctxt,
                null_mut(),
                XmlParserErrors::XmlRngpIncludeRecurse,
                c"Detected an Include recursion for %s\n".as_ptr() as _,
                url,
                null_mut(),
            );
            return null_mut();
        }
    }

    /*
     * load the document
     */
    doc = xml_read_file(url as _, None, 0);
    if doc.is_null() {
        xml_rng_perr(
            ctxt,
            node,
            XmlParserErrors::XmlRngpParseError,
            c"xmlRelaxNG: could not load %s\n".as_ptr() as _,
            url,
            null_mut(),
        );
        return null_mut();
    }

    /*
     * Allocate the document structures and register it first.
     */
    let ret: XmlRelaxNGIncludePtr = xml_malloc(size_of::<XmlRelaxNGInclude>()) as _;
    if ret.is_null() {
        xml_rng_perr_memory(ctxt, c"allocating include\n".as_ptr() as _);
        xml_free_doc(doc);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlRelaxNGInclude>());
    (*ret).doc = doc;
    (*ret).href = xml_strdup(url);
    (*ret).next = (*ctxt).includes;
    (*ctxt).includes = ret;

    /*
     * transmit the ns if needed
     */
    if !ns.is_null() {
        root = (*doc).get_root_element();
        if !root.is_null() && xml_has_prop(root, c"ns".as_ptr() as _).is_null() {
            xml_set_prop(root, c"ns".as_ptr() as _, ns);
        }
    }

    /*
     * push it on the stack
     */
    xml_relaxng_include_push(ctxt, ret);

    /*
     * Some preprocessing of the document content, this include recursing
     * in the include stack.
     */

    doc = xml_relaxng_cleanup_doc(ctxt, doc);
    if doc.is_null() {
        (*ctxt).inc = null_mut();
        return null_mut();
    }

    /*
     * Pop up the include from the stack
     */
    xml_relaxng_include_pop(ctxt);

    /*
     * Check that the top element is a grammar
     */
    root = (*doc).get_root_element();
    if root.is_null() {
        xml_rng_perr(
            ctxt,
            node,
            XmlParserErrors::XmlRngpEmpty,
            c"xmlRelaxNG: included document is empty %s\n".as_ptr() as _,
            url,
            null_mut(),
        );
        return null_mut();
    }
    if !IS_RELAXNG!(root, c"grammar".as_ptr() as _) {
        xml_rng_perr(
            ctxt,
            node,
            XmlParserErrors::XmlRngpGrammarMissing,
            c"xmlRelaxNG: included document %s root is not a grammar\n".as_ptr() as _,
            url,
            null_mut(),
        );
        return null_mut();
    }

    /*
     * Elimination of redefined rules in the include.
     */
    cur = (*node).children;
    while !cur.is_null() {
        if IS_RELAXNG!(cur, c"start".as_ptr() as _) {
            let found: i32 = xml_relaxng_remove_redefine(ctxt, url, (*root).children, null_mut());
            if found == 0 {
                xml_rng_perr(
                    ctxt,
                    node,
                    XmlParserErrors::XmlRngpStartMissing,
                    c"xmlRelaxNG: include %s has a start but not the included grammar\n".as_ptr()
                        as _,
                    url,
                    null_mut(),
                );
            }
        } else if IS_RELAXNG!(cur, c"define".as_ptr() as _) {
            let name: *mut XmlChar = xml_get_prop(cur, c"name".as_ptr() as _);
            if name.is_null() {
                xml_rng_perr(
                    ctxt,
                    node,
                    XmlParserErrors::XmlRngpNameMissing,
                    c"xmlRelaxNG: include %s has define without name\n".as_ptr() as _,
                    url,
                    null_mut(),
                );
            } else {
                xml_relaxng_norm_ext_space(name);
                let found: i32 = xml_relaxng_remove_redefine(ctxt, url, (*root).children, name);
                if found == 0 {
                    xml_rng_perr(
                        ctxt,
                        node,
                        XmlParserErrors::XmlRngpDefineMissing,
                        c"xmlRelaxNG: include %s has a define %s but not the included grammar\n"
                            .as_ptr() as _,
                        url,
                        name,
                    );
                }
                xml_free(name as _);
            }
        }
        if IS_RELAXNG!(cur, c"div".as_ptr() as _) && !(*cur).children.is_null() {
            cur = (*cur).children;
        } else if !(*cur).next.is_null() {
            cur = (*cur).next;
        } else {
            while (*cur).parent != node && (*(*cur).parent).next.is_null() {
                cur = (*cur).parent;
            }
            cur = if (*cur).parent != node {
                (*(*cur).parent).next
            } else {
                null_mut()
            };
        }
    }

    ret
}

/**
 * xmlRelaxNGCleanupTree:
 * @ctxt:  a Relax-NG parser context
 * @root:  an xmlNodePtr subtree
 *
 * Cleanup the subtree from unwanted nodes for parsing, resolve
 * Include and externalRef lookups.
 */
unsafe extern "C" fn xml_relaxng_cleanup_tree(ctxt: XmlRelaxNGParserCtxtPtr, root: XmlNodePtr) {
    let mut cur: XmlNodePtr;
    let mut delete: XmlNodePtr;

    delete = null_mut();
    cur = root;
    'main: while !cur.is_null() {
        if !delete.is_null() {
            xml_unlink_node(delete);
            xml_free_node(delete);
            delete = null_mut();
        }

        'skip_children: {
            if (*cur).typ == XmlElementType::XmlElementNode {
                /*
                 * Simplification 4.1. Annotations
                 */
                if (*cur).ns.is_null()
                    || !xml_str_equal(
                        (*(*cur).ns).href.load(Ordering::Relaxed),
                        XML_RELAXNG_NS.as_ptr() as _,
                    )
                {
                    if !(*cur).parent.is_null()
                        && (*(*cur).parent).typ == XmlElementType::XmlElementNode
                        && (xml_str_equal((*(*cur).parent).name, c"name".as_ptr() as _)
                            || xml_str_equal((*(*cur).parent).name, c"value".as_ptr() as _)
                            || xml_str_equal((*(*cur).parent).name, c"param".as_ptr() as _))
                    {
                        xml_rng_perr(
                            ctxt,
                            cur,
                            XmlParserErrors::XmlRngpForeignElement,
                            c"element %s doesn't allow foreign elements\n".as_ptr() as _,
                            (*(*cur).parent).name,
                            null_mut(),
                        );
                    }
                    delete = cur;
                    break 'skip_children;
                } else {
                    xml_relaxng_cleanup_attributes(ctxt, cur);
                    if xml_str_equal((*cur).name, c"externalRef".as_ptr() as _) {
                        let mut ns: *mut XmlChar;
                        let mut tmp: XmlNodePtr;

                        ns = xml_get_prop(cur, c"ns".as_ptr() as _);
                        if ns.is_null() {
                            tmp = (*cur).parent;
                            while !tmp.is_null() && (*tmp).typ == XmlElementType::XmlElementNode {
                                ns = xml_get_prop(tmp, c"ns".as_ptr() as _);
                                if !ns.is_null() {
                                    break;
                                }
                                tmp = (*tmp).parent;
                            }
                        }
                        let href: *mut XmlChar = xml_get_prop(cur, c"href".as_ptr() as _);
                        if href.is_null() {
                            xml_rng_perr(
                                ctxt,
                                cur,
                                XmlParserErrors::XmlRngpMissingHref,
                                c"xmlRelaxNGParse: externalRef has no href attribute\n".as_ptr()
                                    as _,
                                null_mut(),
                                null_mut(),
                            );
                            if !ns.is_null() {
                                xml_free(ns as _);
                            }
                            delete = cur;
                            break 'skip_children;
                        }
                        let uri: XmlURIPtr = xml_parse_uri(href as _);
                        if uri.is_null() {
                            xml_rng_perr(
                                ctxt,
                                cur,
                                XmlParserErrors::XmlRngpHrefError,
                                c"Incorrect URI for externalRef %s\n".as_ptr() as _,
                                href,
                                null_mut(),
                            );
                            if !ns.is_null() {
                                xml_free(ns as _);
                            }
                            if !href.is_null() {
                                xml_free(href as _);
                            }
                            delete = cur;
                            break 'skip_children;
                        }
                        if !(*uri).fragment.is_null() {
                            xml_rng_perr(
                                ctxt,
                                cur,
                                XmlParserErrors::XmlRngpHrefError,
                                c"Fragment forbidden in URI for externalRef %s\n".as_ptr() as _,
                                href,
                                null_mut(),
                            );
                            if !ns.is_null() {
                                xml_free(ns as _);
                            }
                            xml_free_uri(uri);
                            if !href.is_null() {
                                xml_free(href as _);
                            }
                            delete = cur;
                            break 'skip_children;
                        }
                        xml_free_uri(uri);
                        let base: *mut XmlChar = xml_node_get_base((*cur).doc, cur);
                        let url: *mut XmlChar = xml_build_uri(href, base);
                        if url.is_null() {
                            xml_rng_perr(
                                ctxt,
                                cur,
                                XmlParserErrors::XmlRngpHrefError,
                                c"Failed to compute URL for externalRef %s\n".as_ptr() as _,
                                href,
                                null_mut(),
                            );
                            if !ns.is_null() {
                                xml_free(ns as _);
                            }
                            if !href.is_null() {
                                xml_free(href as _);
                            }
                            if !base.is_null() {
                                xml_free(base as _);
                            }
                            delete = cur;
                            break 'skip_children;
                        }
                        if !href.is_null() {
                            xml_free(href as _);
                        }
                        if !base.is_null() {
                            xml_free(base as _);
                        }
                        let docu: XmlRelaxNGDocumentPtr =
                            xml_relaxng_load_external_ref(ctxt, url, ns);
                        if docu.is_null() {
                            xml_rng_perr(
                                ctxt,
                                cur,
                                XmlParserErrors::XmlRngpExternalRefFailure,
                                c"Failed to load externalRef %s\n".as_ptr() as _,
                                url,
                                null_mut(),
                            );
                            if !ns.is_null() {
                                xml_free(ns as _);
                            }
                            xml_free(url as _);
                            delete = cur;
                            break 'skip_children;
                        }
                        if !ns.is_null() {
                            xml_free(ns as _);
                        }
                        xml_free(url as _);
                        (*cur).psvi = docu as _;
                    } else if xml_str_equal((*cur).name, c"include".as_ptr() as _) {
                        let mut ns: *mut XmlChar;
                        let mut tmp: XmlNodePtr;

                        let href: *mut XmlChar = xml_get_prop(cur, c"href".as_ptr() as _);
                        if href.is_null() {
                            xml_rng_perr(
                                ctxt,
                                cur,
                                XmlParserErrors::XmlRngpMissingHref,
                                c"xmlRelaxNGParse: include has no href attribute\n".as_ptr() as _,
                                null_mut(),
                                null_mut(),
                            );
                            delete = cur;
                            break 'skip_children;
                        }
                        let base: *mut XmlChar = xml_node_get_base((*cur).doc, cur);
                        let url: *mut XmlChar = xml_build_uri(href, base);
                        if url.is_null() {
                            xml_rng_perr(
                                ctxt,
                                cur,
                                XmlParserErrors::XmlRngpHrefError,
                                c"Failed to compute URL for include %s\n".as_ptr() as _,
                                href,
                                null_mut(),
                            );
                            if !href.is_null() {
                                xml_free(href as _);
                            }
                            if !base.is_null() {
                                xml_free(base as _);
                            }
                            delete = cur;
                            break 'skip_children;
                        }
                        if !href.is_null() {
                            xml_free(href as _);
                        }
                        if !base.is_null() {
                            xml_free(base as _);
                        }
                        ns = xml_get_prop(cur, c"ns".as_ptr() as _);
                        if ns.is_null() {
                            tmp = (*cur).parent;
                            while !tmp.is_null() && (*tmp).typ == XmlElementType::XmlElementNode {
                                ns = xml_get_prop(tmp, c"ns".as_ptr() as _);
                                if !ns.is_null() {
                                    break;
                                }
                                tmp = (*tmp).parent;
                            }
                        }
                        let incl: XmlRelaxNGIncludePtr =
                            xml_relaxng_load_include(ctxt, url, cur, ns);
                        if !ns.is_null() {
                            xml_free(ns as _);
                        }
                        if incl.is_null() {
                            xml_rng_perr(
                                ctxt,
                                cur,
                                XmlParserErrors::XmlRngpIncludeFailure,
                                c"Failed to load include %s\n".as_ptr() as _,
                                url,
                                null_mut(),
                            );
                            xml_free(url as _);
                            delete = cur;
                            break 'skip_children;
                        }
                        xml_free(url as _);
                        (*cur).psvi = incl as _;
                    } else if xml_str_equal((*cur).name, c"element".as_ptr() as _)
                        || xml_str_equal((*cur).name, c"attribute".as_ptr() as _)
                    {
                        let ns: *mut XmlChar;
                        let mut text: XmlNodePtr = null_mut();

                        /*
                         * Simplification 4.8. name attribute of element
                         * and attribute elements
                         */
                        let name: *mut XmlChar = xml_get_prop(cur, c"name".as_ptr() as _);
                        if !name.is_null() {
                            if (*cur).children.is_null() {
                                text = xml_new_child(cur, (*cur).ns, c"name".as_ptr() as _, name);
                            } else {
                                let node: XmlNodePtr = xml_new_doc_node(
                                    (*cur).doc,
                                    (*cur).ns,
                                    c"name".as_ptr() as _,
                                    null_mut(),
                                );
                                if !node.is_null() {
                                    (*(*cur).children).add_prev_sibling(node);
                                    text = xml_new_doc_text((*node).doc, name);
                                    xml_add_child(node, text);
                                    text = node;
                                }
                            }
                            if text.is_null() {
                                xml_rng_perr(
                                    ctxt,
                                    cur,
                                    XmlParserErrors::XmlRngpCreateFailure,
                                    c"Failed to create a name %s element\n".as_ptr() as _,
                                    name,
                                    null_mut(),
                                );
                            }
                            xml_unset_prop(cur, c"name".as_ptr() as _);
                            xml_free(name as _);
                            ns = xml_get_prop(cur, c"ns".as_ptr() as _);
                            if !ns.is_null() {
                                if !text.is_null() {
                                    xml_set_prop(text, c"ns".as_ptr() as _, ns);
                                    /* xmlUnsetProp(cur, c"ns".as_ptr() as _); */
                                }
                                xml_free(ns as _);
                            } else if xml_str_equal((*cur).name, c"attribute".as_ptr() as _) {
                                xml_set_prop(text, c"ns".as_ptr() as _, c"".as_ptr() as _);
                            }
                        }
                    } else if xml_str_equal((*cur).name, c"name".as_ptr() as _)
                        || xml_str_equal((*cur).name, c"nsName".as_ptr() as _)
                        || xml_str_equal((*cur).name, c"value".as_ptr() as _)
                    {
                        /*
                         * Simplification 4.8. name attribute of element
                         * and attribute elements
                         */
                        if xml_has_prop(cur, c"ns".as_ptr() as _).is_null() {
                            let mut node: XmlNodePtr;
                            let mut ns: *mut XmlChar = null_mut();

                            node = (*cur).parent;
                            while !node.is_null() && (*node).typ == XmlElementType::XmlElementNode {
                                ns = xml_get_prop(node, c"ns".as_ptr() as _);
                                if !ns.is_null() {
                                    break;
                                }
                                node = (*node).parent;
                            }
                            if ns.is_null() {
                                xml_set_prop(cur, c"ns".as_ptr() as _, c"".as_ptr() as _);
                            } else {
                                xml_set_prop(cur, c"ns".as_ptr() as _, ns);
                                xml_free(ns as _);
                            }
                        }
                        if xml_str_equal((*cur).name, c"name".as_ptr() as _) {
                            let local: *mut XmlChar;
                            let mut prefix: *mut XmlChar = null_mut();

                            /*
                             * Simplification: 4.10. QNames
                             */
                            let name: *mut XmlChar = xml_node_get_content(cur);
                            if !name.is_null() {
                                local = xml_split_qname2(name, addr_of_mut!(prefix));
                                if !local.is_null() {
                                    let ns: XmlNsPtr = xml_search_ns((*cur).doc, cur, prefix);
                                    if ns.is_null() {
                                        xml_rng_perr(
                                            ctxt,
                                            cur,
                                            XmlParserErrors::XmlRngpPrefixUndefined,
                                            c"xmlRelaxNGParse: no namespace for prefix %s\n"
                                                .as_ptr()
                                                as _,
                                            prefix,
                                            null_mut(),
                                        );
                                    } else {
                                        xml_set_prop(
                                            cur,
                                            c"ns".as_ptr() as _,
                                            (*ns).href.load(Ordering::Relaxed),
                                        );
                                        xml_node_set_content(cur, local);
                                    }
                                    xml_free(local as _);
                                    xml_free(prefix as _);
                                }
                                xml_free(name as _);
                            }
                        }
                        /*
                         * 4.16
                         */
                        if xml_str_equal((*cur).name, c"nsName".as_ptr() as _)
                            && (*ctxt).flags & XML_RELAXNG_IN_NSEXCEPT != 0
                        {
                            xml_rng_perr(
                                ctxt,
                                cur,
                                XmlParserErrors::XmlRngpPatNsNameExceptNsName,
                                c"Found nsName/except//nsName forbidden construct\n".as_ptr() as _,
                                null_mut(),
                                null_mut(),
                            );
                        }
                    } else if xml_str_equal((*cur).name, c"except".as_ptr() as _) && cur != root {
                        let oldflags: i32 = (*ctxt).flags;

                        /*
                         * 4.16
                         */
                        if !(*cur).parent.is_null()
                            && xml_str_equal((*(*cur).parent).name, c"anyName".as_ptr() as _)
                        {
                            (*ctxt).flags |= XML_RELAXNG_IN_ANYEXCEPT;
                            xml_relaxng_cleanup_tree(ctxt, cur);
                            (*ctxt).flags = oldflags;
                            break 'skip_children;
                        } else if !(*cur).parent.is_null()
                            && xml_str_equal((*(*cur).parent).name, c"nsName".as_ptr() as _)
                        {
                            (*ctxt).flags |= XML_RELAXNG_IN_NSEXCEPT;
                            xml_relaxng_cleanup_tree(ctxt, cur);
                            (*ctxt).flags = oldflags;
                            break 'skip_children;
                        }
                    } else if xml_str_equal((*cur).name, c"anyName".as_ptr() as _) {
                        /*
                         * 4.16
                         */
                        if (*ctxt).flags & XML_RELAXNG_IN_ANYEXCEPT != 0 {
                            xml_rng_perr(
                                ctxt,
                                cur,
                                XmlParserErrors::XmlRngpPatAnynameExceptAnyname,
                                c"Found anyName/except//anyName forbidden construct\n".as_ptr()
                                    as _,
                                null_mut(),
                                null_mut(),
                            );
                        } else if (*ctxt).flags & XML_RELAXNG_IN_NSEXCEPT != 0 {
                            xml_rng_perr(
                                ctxt,
                                cur,
                                XmlParserErrors::XmlRngpPatNsNameExceptAnyName,
                                c"Found nsName/except//anyName forbidden construct\n".as_ptr() as _,
                                null_mut(),
                                null_mut(),
                            );
                        }
                    }
                    /*
                     * This is not an else since "include" is transformed
                     * into a div
                     */
                    if xml_str_equal((*cur).name, c"div".as_ptr() as _) {
                        let mut child: XmlNodePtr;
                        let mut ins: XmlNodePtr;
                        let mut tmp: XmlNodePtr;

                        /*
                         * implements rule 4.11
                         */

                        let ns: *mut XmlChar = xml_get_prop(cur, c"ns".as_ptr() as _);

                        child = (*cur).children;
                        ins = cur;
                        while !child.is_null() {
                            if !ns.is_null() && xml_has_prop(child, c"ns".as_ptr() as _).is_null() {
                                xml_set_prop(child, c"ns".as_ptr() as _, ns);
                            }
                            tmp = (*child).next;
                            xml_unlink_node(child);
                            ins = xml_add_next_sibling(ins, child);
                            child = tmp;
                        }
                        if !ns.is_null() {
                            xml_free(ns as _);
                        }
                        /*
                         * Since we are about to delete cur, if its nsDef is non-NULL we
                         * need to preserve it (it contains the ns definitions for the
                         * children we just moved).  We'll just stick it on to the end
                         * of (*cur).parent's list, since it's never going to be re-serialized
                         * (bug 143738).
                         */
                        if !(*cur).ns_def.is_null() && !(*cur).parent.is_null() {
                            let mut par_def: XmlNsPtr =
                                addr_of_mut!((*(*cur).parent).ns_def) as XmlNsPtr;
                            while !(*par_def).next.load(Ordering::Relaxed).is_null() {
                                par_def = (*par_def).next.load(Ordering::Relaxed);
                            }
                            (*par_def).next.store((*cur).ns_def, Ordering::Relaxed);
                            (*cur).ns_def = null_mut();
                        }
                        delete = cur;
                        break 'skip_children;
                    }
                }
            }
            /*
             * Simplification 4.2 whitespaces
             */
            else if matches!(
                (*cur).typ,
                XmlElementType::XmlTextNode | XmlElementType::XmlCdataSectionNode
            ) {
                if IS_BLANK_NODE!(cur) {
                    if !(*cur).parent.is_null()
                        && (*(*cur).parent).typ == XmlElementType::XmlElementNode
                    {
                        if !xml_str_equal((*(*cur).parent).name, c"value".as_ptr() as _)
                            && !xml_str_equal((*(*cur).parent).name, c"param".as_ptr() as _)
                        {
                            delete = cur;
                        }
                    } else {
                        delete = cur;
                        break 'skip_children;
                    }
                }
            } else {
                delete = cur;
                break 'skip_children;
            }

            /*
             * Skip to next node
             */
            if !(*cur).children.is_null()
                && !matches!(
                    (*(*cur).children).typ,
                    XmlElementType::XmlEntityDecl
                        | XmlElementType::XmlEntityRefNode
                        | XmlElementType::XmlEntityNode
                )
            {
                cur = (*cur).children;
                continue 'main;
            }
        }
        // skip_children:
        if !(*cur).next.is_null() {
            cur = (*cur).next;
            continue;
        }

        loop {
            cur = (*cur).parent;
            if cur.is_null() {
                break;
            }
            if cur == root {
                cur = null_mut();
                break;
            }
            if !(*cur).next.is_null() {
                cur = (*cur).next;
                break;
            }

            if cur.is_null() {
                break;
            }
        }
    }
    if !delete.is_null() {
        xml_unlink_node(delete);
        xml_free_node(delete);
        // delete = null_mut();
    }
}

/**
 * xmlRelaxNGCleanupDoc:
 * @ctxt:  a Relax-NG parser context
 * @doc:  an xmldocPtr document pointer
 *
 * Cleanup the document from unwanted nodes for parsing, resolve
 * Include and externalRef lookups.
 *
 * Returns the cleaned up document or NULL in case of error
 */
unsafe extern "C" fn xml_relaxng_cleanup_doc(
    ctxt: XmlRelaxNGParserCtxtPtr,
    doc: XmlDocPtr,
) -> XmlDocPtr {
    /*
     * Extract the root
     */
    let root: XmlNodePtr = if doc.is_null() {
        null_mut()
    } else {
        (*doc).get_root_element()
    };
    if root.is_null() {
        xml_rng_perr(
            ctxt,
            doc as _,
            XmlParserErrors::XmlRngpEmpty,
            c"xmlRelaxNGParse: %s is empty\n".as_ptr() as _,
            (*ctxt).url,
            null_mut(),
        );
        return null_mut();
    }
    xml_relaxng_cleanup_tree(ctxt, root);
    doc
}

/**
 * xmlRelaxNGNewRelaxNG:
 * @ctxt:  a Relax-NG validation context (optional)
 *
 * Allocate a new RelaxNG structure.
 *
 * Returns the newly allocated structure or NULL in case or error
 */
unsafe extern "C" fn xml_relaxng_new_relaxng(ctxt: XmlRelaxNGParserCtxtPtr) -> XmlRelaxNGPtr {
    let ret: XmlRelaxNGPtr = xml_malloc(size_of::<XmlRelaxNG>()) as _;
    if ret.is_null() {
        xml_rng_perr_memory(ctxt, null_mut());
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlRelaxNG>());

    ret
}

/**
 * xmlRelaxNGNewDefine:
 * @ctxt:  a Relax-NG validation context
 * @node:  the node in the input document.
 *
 * Allocate a new RelaxNG define.
 *
 * Returns the newly allocated structure or NULL in case or error
 */
unsafe extern "C" fn xml_relaxng_new_define(
    ctxt: XmlRelaxNGParserCtxtPtr,
    node: XmlNodePtr,
) -> XmlRelaxNGDefinePtr {
    if (*ctxt).def_max == 0 {
        (*ctxt).def_max = 16;
        (*ctxt).def_nr = 0;
        (*ctxt).def_tab =
            xml_malloc((*ctxt).def_max as usize * size_of::<XmlRelaxNGDefinePtr>()) as _;
        if (*ctxt).def_tab.is_null() {
            xml_rng_perr_memory(ctxt, c"allocating define\n".as_ptr() as _);
            return null_mut();
        }
    } else if (*ctxt).def_max <= (*ctxt).def_nr {
        (*ctxt).def_max *= 2;
        let tmp: *mut XmlRelaxNGDefinePtr = xml_realloc(
            (*ctxt).def_tab as _,
            (*ctxt).def_max as usize * size_of::<XmlRelaxNGDefinePtr>(),
        ) as _;
        if tmp.is_null() {
            xml_rng_perr_memory(ctxt, c"allocating define\n".as_ptr() as _);
            return null_mut();
        }
        (*ctxt).def_tab = tmp;
    }
    let ret: XmlRelaxNGDefinePtr = xml_malloc(size_of::<XmlRelaxNGDefine>()) as _;
    if ret.is_null() {
        xml_rng_perr_memory(ctxt, c"allocating define\n".as_ptr() as _);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlRelaxNGDefine>());
    *(*ctxt).def_tab.add((*ctxt).def_nr as usize) = ret;
    (*ctxt).def_nr += 1;
    (*ret).node = node;
    (*ret).depth = -1;
    ret
}

/**
 * xmlRelaxNGCheckCombine:
 * @define:  the define(s) list
 * @ctxt:  a Relax-NG parser context
 * @name:  the name associated to the defines
 *
 * Applies the 4.17. combine attribute rule for all the define
 * element of a given grammar using the same name.
 */
extern "C" fn xml_relaxng_check_combine(
    payload: *mut c_void,
    data: *mut c_void,
    name: *const XmlChar,
) {
    let define: XmlRelaxNGDefinePtr = payload as _;
    let ctxt: XmlRelaxNGParserCtxtPtr = data as _;
    let mut combine: *mut XmlChar;
    let mut choice_or_interleave: i32 = -1;
    let mut missing: i32 = 0;
    let mut cur: XmlRelaxNGDefinePtr;
    let mut last: XmlRelaxNGDefinePtr;
    let mut tmp: XmlRelaxNGDefinePtr;
    let mut tmp2: XmlRelaxNGDefinePtr;

    unsafe {
        if (*define).next_hash.is_null() {
            return;
        }
        cur = define;
        while !cur.is_null() {
            combine = xml_get_prop((*cur).node, c"combine".as_ptr() as _);
            if !combine.is_null() {
                if xml_str_equal(combine, c"choice".as_ptr() as _) {
                    if choice_or_interleave == -1 {
                        choice_or_interleave = 1;
                    } else if choice_or_interleave == 0 {
                        xml_rng_perr(
                            ctxt,
                            (*define).node,
                            XmlParserErrors::XmlRngpDefChoiceAndInterleave,
                            c"Defines for %s use both 'choice' and 'interleave'\n".as_ptr() as _,
                            name,
                            null_mut(),
                        );
                    }
                } else if xml_str_equal(combine, c"interleave".as_ptr() as _) {
                    if choice_or_interleave == -1 {
                        choice_or_interleave = 0;
                    } else if choice_or_interleave == 1 {
                        xml_rng_perr(
                            ctxt,
                            (*define).node,
                            XmlParserErrors::XmlRngpDefChoiceAndInterleave,
                            c"Defines for %s use both 'choice' and 'interleave'\n".as_ptr() as _,
                            name,
                            null_mut(),
                        );
                    }
                } else {
                    xml_rng_perr(
                        ctxt,
                        (*define).node,
                        XmlParserErrors::XmlRngpUnknownCombine,
                        c"Defines for %s use unknown combine value '%s''\n".as_ptr() as _,
                        name,
                        combine,
                    );
                }
                xml_free(combine as _);
            } else if missing == 0 {
                missing = 1;
            } else {
                xml_rng_perr(
                    ctxt,
                    (*define).node,
                    XmlParserErrors::XmlRngpNeedCombine,
                    c"Some defines for %s needs the combine attribute\n".as_ptr() as _,
                    name,
                    null_mut(),
                );
            }

            cur = (*cur).next_hash;
        }
        if choice_or_interleave == -1 {
            choice_or_interleave = 0;
        }
        cur = xml_relaxng_new_define(ctxt, (*define).node);
        if cur.is_null() {
            return;
        }
        if choice_or_interleave == 0 {
            (*cur).typ = XmlRelaxNGType::Interleave;
        } else {
            (*cur).typ = XmlRelaxNGType::Choice;
        }
        tmp = define;
        last = null_mut();
        while !tmp.is_null() {
            if !(*tmp).content.is_null() {
                if !(*(*tmp).content).next.is_null() {
                    /*
                     * we need first to create a wrapper.
                     */
                    tmp2 = xml_relaxng_new_define(ctxt, (*(*tmp).content).node);
                    if tmp2.is_null() {
                        break;
                    }
                    (*tmp2).typ = XmlRelaxNGType::Group;
                    (*tmp2).content = (*tmp).content;
                } else {
                    tmp2 = (*tmp).content;
                }
                if last.is_null() {
                    (*cur).content = tmp2;
                } else {
                    (*last).next = tmp2;
                }
                last = tmp2;
            }
            (*tmp).content = cur;
            tmp = (*tmp).next_hash;
        }
        (*define).content = cur;
        if choice_or_interleave == 0 {
            if (*ctxt).interleaves.is_null() {
                (*ctxt).interleaves = xml_hash_create(10);
            }
            if (*ctxt).interleaves.is_null() {
                xml_rng_perr(
                    ctxt,
                    (*define).node,
                    XmlParserErrors::XmlRngpInterleaveCreateFailed,
                    c"Failed to create interleaves hash table\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            } else {
                let mut tmpname: [c_char; 32] = [0; 32];

                snprintf(
                    tmpname.as_mut_ptr(),
                    32,
                    c"interleave%d".as_ptr() as _,
                    (*ctxt).nb_interleaves,
                );
                (*ctxt).nb_interleaves += 1;
                if xml_hash_add_entry((*ctxt).interleaves, tmpname.as_ptr() as _, cur as _) < 0 {
                    xml_rng_perr(
                        ctxt,
                        (*define).node,
                        XmlParserErrors::XmlRngpInterleaveCreateFailed,
                        c"Failed to add %s to hash table\n".as_ptr() as _,
                        tmpname.as_ptr() as _,
                        null_mut(),
                    );
                }
            }
        }
    }
}

/**
 * xmlRelaxNGCheckReference:
 * @ref:  the ref
 * @ctxt:  a Relax-NG parser context
 * @name:  the name associated to the defines
 *
 * Applies the 4.17. combine attribute rule for all the define
 * element of a given grammar using the same name.
 */
extern "C" fn xml_relaxng_check_reference(
    payload: *mut c_void,
    data: *mut c_void,
    name: *const XmlChar,
) {
    let refe: XmlRelaxNGDefinePtr = payload as _;
    let ctxt: XmlRelaxNGParserCtxtPtr = data as _;
    let def: XmlRelaxNGDefinePtr;
    let mut cur: XmlRelaxNGDefinePtr;

    /*
     * Those rules don't apply to imported ref from xmlRelaxNGParseImportRef
     */
    unsafe {
        if (*refe).dflags & IS_EXTERNAL_REF as i16 != 0 {
            return;
        }

        let grammar: XmlRelaxNGGrammarPtr = (*ctxt).grammar;
        if grammar.is_null() {
            xml_rng_perr(
                ctxt,
                (*refe).node,
                XmlParserErrors::XmlErrInternalError,
                c"Internal error: no grammar in CheckReference %s\n".as_ptr() as _,
                name,
                null_mut(),
            );
            return;
        }
        if !(*refe).content.is_null() {
            xml_rng_perr(
                ctxt,
                (*refe).node,
                XmlParserErrors::XmlErrInternalError,
                c"Internal error: reference has content in CheckReference %s\n".as_ptr() as _,
                name,
                null_mut(),
            );
            return;
        }
        if !(*grammar).defs.is_null() {
            def = xml_hash_lookup((*grammar).defs, name) as _;
            if !def.is_null() {
                cur = refe;
                while !cur.is_null() {
                    (*cur).content = def;
                    cur = (*cur).next_hash;
                }
            } else {
                xml_rng_perr(
                    ctxt,
                    (*refe).node,
                    XmlParserErrors::XmlRngpRefNoDef,
                    c"Reference %s has no matching definition\n".as_ptr() as _,
                    name,
                    null_mut(),
                );
            }
        } else {
            xml_rng_perr(
                ctxt,
                (*refe).node,
                XmlParserErrors::XmlRngpRefNoDef,
                c"Reference %s has no matching definition\n".as_ptr() as _,
                name,
                null_mut(),
            );
        }
    }
}

/**
 * xmlRelaxNGNewGrammar:
 * @ctxt:  a Relax-NG validation context (optional)
 *
 * Allocate a new RelaxNG grammar.
 *
 * Returns the newly allocated structure or NULL in case or error
 */
unsafe extern "C" fn xml_relaxng_new_grammar(
    ctxt: XmlRelaxNGParserCtxtPtr,
) -> XmlRelaxNGGrammarPtr {
    let ret: XmlRelaxNGGrammarPtr = xml_malloc(size_of::<XmlRelaxNGGrammar>()) as _;
    if ret.is_null() {
        xml_rng_perr_memory(ctxt, null_mut());
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlRelaxNGGrammar>());

    ret
}

/**
 * xmlRelaxNGParseExceptNameClass:
 * @ctxt:  a Relax-NG parser context
 * @node:  the except node
 * @attr:  1 if within an attribute, 0 if within an element
 *
 * parse the content of a RelaxNG nameClass node.
 *
 * Returns the definition pointer or NULL in case of error.
 */
unsafe extern "C" fn xml_relaxng_parse_except_name_class(
    ctxt: XmlRelaxNGParserCtxtPtr,
    node: XmlNodePtr,
    attr: i32,
) -> XmlRelaxNGDefinePtr {
    let mut cur: XmlRelaxNGDefinePtr;
    let mut last: XmlRelaxNGDefinePtr = null_mut();
    let mut child: XmlNodePtr;

    if !IS_RELAXNG!(node, c"except".as_ptr() as _) {
        xml_rng_perr(
            ctxt,
            node,
            XmlParserErrors::XmlRngpExceptMissing,
            c"Expecting an except node\n".as_ptr() as _,
            null_mut(),
            null_mut(),
        );
        return null_mut();
    }
    if !(*node).next.is_null() {
        xml_rng_perr(
            ctxt,
            node,
            XmlParserErrors::XmlRngpExceptMultiple,
            c"exceptNameClass allows only a single except node\n".as_ptr() as _,
            null_mut(),
            null_mut(),
        );
    }
    if (*node).children.is_null() {
        xml_rng_perr(
            ctxt,
            node,
            XmlParserErrors::XmlRngpExceptEmpty,
            c"except has no content\n".as_ptr() as _,
            null_mut(),
            null_mut(),
        );
        return null_mut();
    }

    let ret: XmlRelaxNGDefinePtr = xml_relaxng_new_define(ctxt, node);
    if ret.is_null() {
        return null_mut();
    }
    (*ret).typ = XmlRelaxNGType::Except;
    child = (*node).children;
    while !child.is_null() {
        cur = xml_relaxng_new_define(ctxt, child);
        if cur.is_null() {
            break;
        }
        if attr != 0 {
            (*cur).typ = XmlRelaxNGType::Attribute;
        } else {
            (*cur).typ = XmlRelaxNGType::Element;
        }

        if !xml_relaxng_parse_name_class(ctxt, child, cur).is_null() {
            if last.is_null() {
                (*ret).content = cur;
            } else {
                (*last).next = cur;
            }
            last = cur;
        }
        child = (*child).next;
    }

    ret
}

/**
 * xmlRelaxNGParseNameClass:
 * @ctxt:  a Relax-NG parser context
 * @node:  the nameClass node
 * @def:  the current definition
 *
 * parse the content of a RelaxNG nameClass node.
 *
 * Returns the definition pointer or NULL in case of error.
 */
unsafe extern "C" fn xml_relaxng_parse_name_class(
    ctxt: XmlRelaxNGParserCtxtPtr,
    node: XmlNodePtr,
    def: XmlRelaxNGDefinePtr,
) -> XmlRelaxNGDefinePtr {
    let mut ret: XmlRelaxNGDefinePtr;
    let mut tmp: XmlRelaxNGDefinePtr;
    let mut val: *mut XmlChar;

    ret = def;
    if (IS_RELAXNG!(node, c"name".as_ptr() as _)
        || IS_RELAXNG!(node, c"anyName".as_ptr() as _)
        || IS_RELAXNG!(node, c"nsName".as_ptr() as _))
        && !matches!(
            (*def).typ,
            XmlRelaxNGType::Element | XmlRelaxNGType::Attribute
        )
    {
        ret = xml_relaxng_new_define(ctxt, node);
        if ret.is_null() {
            return null_mut();
        }
        (*ret).parent = def;
        if (*ctxt).flags & XML_RELAXNG_IN_ATTRIBUTE != 0 {
            (*ret).typ = XmlRelaxNGType::Attribute;
        } else {
            (*ret).typ = XmlRelaxNGType::Element;
        }
    }
    if IS_RELAXNG!(node, c"name".as_ptr() as _) {
        val = xml_node_get_content(node);
        xml_relaxng_norm_ext_space(val);
        if xml_validate_ncname(val, 0) != 0 {
            if !(*node).parent.is_null() {
                xml_rng_perr(
                    ctxt,
                    node,
                    XmlParserErrors::XmlRngpElementName,
                    c"Element %s name '%s' is not an NCName\n".as_ptr() as _,
                    (*(*node).parent).name,
                    val,
                );
            } else {
                xml_rng_perr(
                    ctxt,
                    node,
                    XmlParserErrors::XmlRngpElementName,
                    c"name '%s' is not an NCName\n".as_ptr() as _,
                    val,
                    null_mut(),
                );
            }
        }
        (*ret).name = val;
        val = xml_get_prop(node, c"ns".as_ptr() as _);
        (*ret).ns = val;
        if (*ctxt).flags & XML_RELAXNG_IN_ATTRIBUTE != 0
            && !val.is_null()
            && xml_str_equal(val, c"http://www.w3.org/2000/xmlns".as_ptr() as _)
        {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpXmlNs,
                c"Attribute with namespace '%s' is not allowed\n".as_ptr() as _,
                val,
                null_mut(),
            );
        }
        if (*ctxt).flags & XML_RELAXNG_IN_ATTRIBUTE != 0
            && !val.is_null()
            && *val.add(0) == 0
            && xml_str_equal((*ret).name, c"xmlns".as_ptr() as _)
        {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpXmlNsName,
                c"Attribute with QName 'xmlns' is not allowed\n".as_ptr() as _,
                val,
                null_mut(),
            );
        }
    } else if IS_RELAXNG!(node, c"anyName".as_ptr() as _) {
        (*ret).name = null_mut();
        (*ret).ns = null_mut();
        if !(*node).children.is_null() {
            (*ret).name_class = xml_relaxng_parse_except_name_class(
                ctxt,
                (*node).children,
                ((*def).typ == XmlRelaxNGType::Attribute) as i32,
            );
        }
    } else if IS_RELAXNG!(node, c"nsName".as_ptr() as _) {
        (*ret).name = null_mut();
        (*ret).ns = xml_get_prop(node, c"ns".as_ptr() as _);
        if (*ret).ns.is_null() {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpNsNameNoNs,
                c"nsName has no ns attribute\n".as_ptr() as _,
                null_mut(),
                null_mut(),
            );
        }
        if (*ctxt).flags & XML_RELAXNG_IN_ATTRIBUTE != 0
            && !(*ret).ns.is_null()
            && xml_str_equal((*ret).ns, c"http://www.w3.org/2000/xmlns".as_ptr() as _)
        {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpXmlNs,
                c"Attribute with namespace '%s' is not allowed\n".as_ptr() as _,
                (*ret).ns,
                null_mut(),
            );
        }
        if !(*node).children.is_null() {
            (*ret).name_class = xml_relaxng_parse_except_name_class(
                ctxt,
                (*node).children,
                ((*def).typ == XmlRelaxNGType::Attribute) as i32,
            );
        }
    } else if IS_RELAXNG!(node, c"choice".as_ptr() as _) {
        let mut child: XmlNodePtr;
        let mut last: XmlRelaxNGDefinePtr = null_mut();

        if (*def).typ == XmlRelaxNGType::Choice {
            ret = def;
        } else {
            ret = xml_relaxng_new_define(ctxt, node);
            if ret.is_null() {
                return null_mut();
            }
            (*ret).parent = def;
            (*ret).typ = XmlRelaxNGType::Choice;
        }

        if (*node).children.is_null() {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpChoiceEmpty,
                c"Element choice is empty\n".as_ptr() as _,
                null_mut(),
                null_mut(),
            );
        } else {
            child = (*node).children;
            while !child.is_null() {
                tmp = xml_relaxng_parse_name_class(ctxt, child, ret);
                if !tmp.is_null() {
                    if last.is_null() {
                        last = tmp;
                    } else {
                        (*last).next = tmp;
                        last = tmp;
                    }
                }
                child = (*child).next;
            }
        }
    } else {
        xml_rng_perr(
            ctxt,
            node,
            XmlParserErrors::XmlRngpChoiceContent,
            c"expecting name, anyName, nsName or choice : got %s\n".as_ptr() as _,
            if node.is_null() {
                c"nothing".as_ptr() as _
            } else {
                (*node).name
            },
            null_mut(),
        );
        return null_mut();
    }
    if ret != def {
        if (*def).name_class.is_null() {
            (*def).name_class = ret;
        } else {
            tmp = (*def).name_class;
            while !(*tmp).next.is_null() {
                tmp = (*tmp).next;
            }
            (*tmp).next = ret;
        }
    }
    ret
}

/**
 * xmlRelaxNGGetDataTypeLibrary:
 * @ctxt:  a Relax-NG parser context
 * @node:  the current data or value element
 *
 * Applies algorithm from 4.3. datatypeLibrary attribute
 *
 * Returns the datatypeLibrary value or NULL if not found
 */
unsafe extern "C" fn xml_relaxng_get_data_type_library(
    _ctxt: XmlRelaxNGParserCtxtPtr,
    mut node: XmlNodePtr,
) -> *mut XmlChar {
    let mut ret: *mut XmlChar;
    let escape: *mut XmlChar;

    if node.is_null() {
        return null_mut();
    }

    if IS_RELAXNG!(node, c"data".as_ptr() as _) || IS_RELAXNG!(node, c"value".as_ptr() as _) {
        ret = xml_get_prop(node, c"datatypeLibrary".as_ptr() as _);
        if !ret.is_null() {
            if *ret.add(0) == 0 {
                xml_free(ret as _);
                return null_mut();
            }
            escape = xml_uri_escape_str(ret, c":/#?".as_ptr() as _);
            if escape.is_null() {
                return ret;
            }
            xml_free(ret as _);
            return escape;
        }
    }
    node = (*node).parent;
    while !node.is_null() && (*node).typ == XmlElementType::XmlElementNode {
        ret = xml_get_prop(node, c"datatypeLibrary".as_ptr() as _);
        if !ret.is_null() {
            if *ret.add(0) == 0 {
                xml_free(ret as _);
                return null_mut();
            }
            escape = xml_uri_escape_str(ret, c":/#?".as_ptr() as _);
            if escape.is_null() {
                return ret;
            }
            xml_free(ret as _);
            return escape;
        }
        node = (*node).parent;
    }
    null_mut()
}

/**
 * xmlRelaxNGParseData:
 * @ctxt:  a Relax-NG parser context
 * @node:  the data node.
 *
 * parse the content of a RelaxNG data node.
 *
 * Returns the definition pointer or NULL in case of error
 */
unsafe extern "C" fn xml_relaxng_parse_data(
    ctxt: XmlRelaxNGParserCtxtPtr,
    node: XmlNodePtr,
) -> XmlRelaxNGDefinePtr {
    let except: XmlRelaxNGDefinePtr;
    let mut param: XmlRelaxNGDefinePtr;
    let mut lastparam: XmlRelaxNGDefinePtr = null_mut();
    let mut library: *mut XmlChar;
    let mut content: XmlNodePtr;
    let tmp: i32;

    let typ: *mut XmlChar = xml_get_prop(node, c"type".as_ptr() as _);
    if typ.is_null() {
        xml_rng_perr(
            ctxt,
            node,
            XmlParserErrors::XmlRngpTypeMissing,
            c"data has no type\n".as_ptr() as _,
            null_mut(),
            null_mut(),
        );
        return null_mut();
    }
    xml_relaxng_norm_ext_space(typ);
    if xml_validate_ncname(typ, 0) != 0 {
        xml_rng_perr(
            ctxt,
            node,
            XmlParserErrors::XmlRngpTypeValue,
            c"data type '%s' is not an NCName\n".as_ptr() as _,
            typ,
            null_mut(),
        );
    }
    library = xml_relaxng_get_data_type_library(ctxt, node);
    if library.is_null() {
        library = xml_strdup(c"http://relaxng.org/ns/structure/1.0".as_ptr() as _);
    }

    let def: XmlRelaxNGDefinePtr = xml_relaxng_new_define(ctxt, node);
    if def.is_null() {
        xml_free(library as _);
        xml_free(typ as _);
        return null_mut();
    }
    (*def).typ = XmlRelaxNGType::Datatype;
    (*def).name = typ;
    (*def).ns = library;

    let lib: XmlRelaxNGTypeLibraryPtr = xml_hash_lookup(
        XML_RELAXNG_REGISTERED_TYPES.load(Ordering::Relaxed),
        library,
    ) as _;
    if lib.is_null() {
        xml_rng_perr(
            ctxt,
            node,
            XmlParserErrors::XmlRngpUnknownTypeLib,
            c"Use of unregistered type library '%s'\n".as_ptr() as _,
            library,
            null_mut(),
        );
        (*def).data = null_mut();
    } else {
        (*def).data = lib as _;
        if let Some(have) = (*lib).have {
            tmp = have((*lib).data, (*def).name);
            if tmp != 1 {
                xml_rng_perr(
                    ctxt,
                    node,
                    XmlParserErrors::XmlRngpTypeNotFound,
                    c"Error type '%s' is not exported by type library '%s'\n".as_ptr() as _,
                    (*def).name,
                    library,
                );
            } else if xml_str_equal(
                library,
                c"http://www.w3.org/2001/XMLSchema-datatypes".as_ptr() as _,
            ) && (xml_str_equal((*def).name, c"IDREF".as_ptr() as _)
                || xml_str_equal((*def).name, c"IDREFS".as_ptr() as _))
            {
                (*ctxt).idref = 1;
            }
        } else {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpErrorTypeLib,
                c"Internal error with type library '%s': no 'have'\n".as_ptr() as _,
                library,
                null_mut(),
            );
        }
    }
    content = (*node).children;

    /*
     * Handle optional params
     */
    while !content.is_null() {
        if !xml_str_equal((*content).name, c"param".as_ptr() as _) {
            break;
        }
        if xml_str_equal(
            library,
            c"http://relaxng.org/ns/structure/1.0".as_ptr() as _,
        ) {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpParamForbidden,
                c"Type library '%s' does not allow type parameters\n".as_ptr() as _,
                library,
                null_mut(),
            );
            content = (*content).next;
            while !content.is_null() && xml_str_equal((*content).name, c"param".as_ptr() as _) {
                content = (*content).next;
            }
        } else {
            param = xml_relaxng_new_define(ctxt, node);
            if !param.is_null() {
                (*param).typ = XmlRelaxNGType::Param;
                (*param).name = xml_get_prop(content, c"name".as_ptr() as _);
                if (*param).name.is_null() {
                    xml_rng_perr(
                        ctxt,
                        node,
                        XmlParserErrors::XmlRngpParamNameMissing,
                        c"param has no name\n".as_ptr() as _,
                        null_mut(),
                        null_mut(),
                    );
                }
                (*param).value = xml_node_get_content(content);
                if lastparam.is_null() {
                    (*def).attrs = param;
                    lastparam = param;
                } else {
                    (*lastparam).next = param;
                    lastparam = param;
                }
                if !lib.is_null() {}
            }
            content = (*content).next;
        }
    }
    /*
     * Handle optional except
     */
    if !content.is_null() && xml_str_equal((*content).name, c"except".as_ptr() as _) {
        let mut child: XmlNodePtr;
        let mut tmp2: XmlRelaxNGDefinePtr;
        let mut last: XmlRelaxNGDefinePtr = null_mut();

        except = xml_relaxng_new_define(ctxt, node);
        if except.is_null() {
            return def;
        }
        (*except).typ = XmlRelaxNGType::Except;
        child = (*content).children;
        (*def).content = except;
        if child.is_null() {
            xml_rng_perr(
                ctxt,
                content,
                XmlParserErrors::XmlRngpExceptNoContent,
                c"except has no content\n".as_ptr() as _,
                null_mut(),
                null_mut(),
            );
        }
        while !child.is_null() {
            tmp2 = xml_relaxng_parse_pattern(ctxt, child);
            if !tmp2.is_null() {
                if last.is_null() {
                    (*except).content = tmp2;
                    last = tmp2;
                } else {
                    (*last).next = tmp2;
                    last = tmp2;
                }
            }
            child = (*child).next;
        }
        content = (*content).next;
    }
    /*
     * Check there is no unhandled data
     */
    if !content.is_null() {
        xml_rng_perr(
            ctxt,
            content,
            XmlParserErrors::XmlRngpDataContent,
            c"Element data has unexpected content %s\n".as_ptr() as _,
            (*content).name,
            null_mut(),
        );
    }

    def
}

/**
 * xmlRelaxNGParseAttribute:
 * @ctxt:  a Relax-NG parser context
 * @node:  the element node
 *
 * parse the content of a RelaxNG attribute node.
 *
 * Returns the definition pointer or NULL in case of error.
 */
unsafe extern "C" fn xml_relaxng_parse_attribute(
    ctxt: XmlRelaxNGParserCtxtPtr,
    node: XmlNodePtr,
) -> XmlRelaxNGDefinePtr {
    let mut cur: XmlRelaxNGDefinePtr;
    let mut child: XmlNodePtr;

    let ret: XmlRelaxNGDefinePtr = xml_relaxng_new_define(ctxt, node);
    if ret.is_null() {
        return null_mut();
    }
    (*ret).typ = XmlRelaxNGType::Attribute;
    (*ret).parent = (*ctxt).def;
    child = (*node).children;
    if child.is_null() {
        xml_rng_perr(
            ctxt,
            node,
            XmlParserErrors::XmlRngpAttributeEmpty,
            c"xmlRelaxNGParseattribute: attribute has no children\n".as_ptr() as _,
            null_mut(),
            null_mut(),
        );
        return ret;
    }
    let old_flags: i32 = (*ctxt).flags;
    (*ctxt).flags |= XML_RELAXNG_IN_ATTRIBUTE;
    cur = xml_relaxng_parse_name_class(ctxt, child, ret);
    if !cur.is_null() {
        child = (*child).next;
    }

    if !child.is_null() {
        cur = xml_relaxng_parse_pattern(ctxt, child);
        if !cur.is_null() {
            match (*cur).typ {
                XmlRelaxNGType::Empty
                | XmlRelaxNGType::NotAllowed
                | XmlRelaxNGType::Text
                | XmlRelaxNGType::Element
                | XmlRelaxNGType::Datatype
                | XmlRelaxNGType::Value
                | XmlRelaxNGType::List
                | XmlRelaxNGType::Ref
                | XmlRelaxNGType::Parentref
                | XmlRelaxNGType::Externalref
                | XmlRelaxNGType::Def
                | XmlRelaxNGType::Oneormore
                | XmlRelaxNGType::Zeroormore
                | XmlRelaxNGType::Optional
                | XmlRelaxNGType::Choice
                | XmlRelaxNGType::Group
                | XmlRelaxNGType::Interleave
                | XmlRelaxNGType::Attribute => {
                    (*ret).content = cur;
                    (*cur).parent = ret;
                }
                XmlRelaxNGType::Start | XmlRelaxNGType::Param | XmlRelaxNGType::Except => {
                    xml_rng_perr(
                        ctxt,
                        node,
                        XmlParserErrors::XmlRngpAttributeContent,
                        c"attribute has invalid content\n".as_ptr() as _,
                        null_mut(),
                        null_mut(),
                    );
                }
                XmlRelaxNGType::Noop => {
                    xml_rng_perr(
                        ctxt,
                        node,
                        XmlParserErrors::XmlRngpAttributeNoop,
                        c"RNG Internal error, noop found in attribute\n".as_ptr() as _,
                        null_mut(),
                        null_mut(),
                    );
                }
            }
        }
        child = (*child).next;
    }
    if !child.is_null() {
        xml_rng_perr(
            ctxt,
            node,
            XmlParserErrors::XmlRngpAttributeChildren,
            c"attribute has multiple children\n".as_ptr() as _,
            null_mut(),
            null_mut(),
        );
    }
    (*ctxt).flags = old_flags;
    ret
}

/**
 * xmlRelaxNGParseValue:
 * @ctxt:  a Relax-NG parser context
 * @node:  the data node.
 *
 * parse the content of a RelaxNG value node.
 *
 * Returns the definition pointer or NULL in case of error
 */
unsafe extern "C" fn xml_relaxng_parse_value(
    ctxt: XmlRelaxNGParserCtxtPtr,
    node: XmlNodePtr,
) -> XmlRelaxNGDefinePtr {
    let mut lib: XmlRelaxNGTypeLibraryPtr = null_mut();
    let mut library: *mut XmlChar;
    let mut success: i32 = 0;

    let def: XmlRelaxNGDefinePtr = xml_relaxng_new_define(ctxt, node);
    if def.is_null() {
        return null_mut();
    }
    (*def).typ = XmlRelaxNGType::Value;

    let typ: *mut XmlChar = xml_get_prop(node, c"type".as_ptr() as _);
    if !typ.is_null() {
        xml_relaxng_norm_ext_space(typ);
        if xml_validate_ncname(typ, 0) != 0 {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpTypeValue,
                c"value typ '%s' is not an NCName\n".as_ptr() as _,
                typ,
                null_mut(),
            );
        }
        library = xml_relaxng_get_data_type_library(ctxt, node);
        if library.is_null() {
            library = xml_strdup(c"http://relaxng.org/ns/structure/1.0".as_ptr() as _);
        }

        (*def).name = typ;
        (*def).ns = library;

        lib = xml_hash_lookup(
            XML_RELAXNG_REGISTERED_TYPES.load(Ordering::Relaxed),
            library,
        ) as _;
        if lib.is_null() {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpUnknownTypeLib,
                c"Use of unregistered type library '%s'\n".as_ptr() as _,
                library,
                null_mut(),
            );
            (*def).data = null_mut();
        } else {
            (*def).data = lib as _;
            if let Some(have) = (*lib).have {
                success = have((*lib).data, (*def).name);
                if success != 1 {
                    xml_rng_perr(
                        ctxt,
                        node,
                        XmlParserErrors::XmlRngpTypeNotFound,
                        c"Error type '%s' is not exported by type library '%s'\n".as_ptr() as _,
                        (*def).name,
                        library,
                    );
                }
            } else {
                xml_rng_perr(
                    ctxt,
                    node,
                    XmlParserErrors::XmlRngpErrorTypeLib,
                    c"Internal error with type library '%s': no 'have'\n".as_ptr() as _,
                    library,
                    null_mut(),
                );
            }
        }
    }
    if (*node).children.is_null() {
        (*def).value = xml_strdup(c"".as_ptr() as _);
    } else if !matches!(
        (*(*node).children).typ,
        XmlElementType::XmlTextNode | XmlElementType::XmlCdataSectionNode
    ) || !(*(*node).children).next.is_null()
    {
        xml_rng_perr(
            ctxt,
            node,
            XmlParserErrors::XmlRngpTextExpected,
            c"Expecting a single text value for <value>content\n".as_ptr() as _,
            null_mut(),
            null_mut(),
        );
    } else if !def.is_null() {
        (*def).value = xml_node_get_content(node);
        if (*def).value.is_null() {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpValueNoContent,
                c"Element <value> has no content\n".as_ptr() as _,
                null_mut(),
                null_mut(),
            );
        } else if !lib.is_null() && (*lib).check.is_some() && success == 1 {
            let mut val: *mut c_void = null_mut();

            success = (*lib).check.unwrap()(
                (*lib).data,
                (*def).name,
                (*def).value,
                addr_of_mut!(val),
                node,
            );
            if success != 1 {
                xml_rng_perr(
                    ctxt,
                    node,
                    XmlParserErrors::XmlRngpInvalidValue,
                    c"Value '%s' is not acceptable for type '%s'\n".as_ptr() as _,
                    (*def).value,
                    (*def).name,
                );
            } else if !val.is_null() {
                (*def).attrs = val as _;
            }
        }
    }
    def
}

/**
 * xmlRelaxNGParseInterleave:
 * @ctxt:  a Relax-NG parser context
 * @node:  the data node.
 *
 * parse the content of a RelaxNG interleave node.
 *
 * Returns the definition pointer or NULL in case of error
 */
unsafe extern "C" fn xml_relaxng_parse_interleave(
    ctxt: XmlRelaxNGParserCtxtPtr,
    node: XmlNodePtr,
) -> XmlRelaxNGDefinePtr {
    let mut last: XmlRelaxNGDefinePtr = null_mut();
    let mut cur: XmlRelaxNGDefinePtr;
    let mut child: XmlNodePtr;

    let def: XmlRelaxNGDefinePtr = xml_relaxng_new_define(ctxt, node);
    if def.is_null() {
        return null_mut();
    }
    (*def).typ = XmlRelaxNGType::Interleave;

    if (*ctxt).interleaves.is_null() {
        (*ctxt).interleaves = xml_hash_create(10);
    }
    if (*ctxt).interleaves.is_null() {
        xml_rng_perr_memory(ctxt, c"create interleaves\n".as_ptr() as _);
    } else {
        let mut name: [c_char; 32] = [0; 32];

        snprintf(
            name.as_mut_ptr() as _,
            32,
            c"interleave%d".as_ptr() as _,
            (*ctxt).nb_interleaves,
        );
        (*ctxt).nb_interleaves += 1;
        if xml_hash_add_entry((*ctxt).interleaves, name.as_ptr() as _, def as _) < 0 {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpInterleaveAdd,
                c"Failed to add %s to hash table\n".as_ptr() as _,
                name.as_ptr() as _,
                null_mut(),
            );
        }
    }
    child = (*node).children;
    if child.is_null() {
        xml_rng_perr(
            ctxt,
            node,
            XmlParserErrors::XmlRngpInterleaveNoContent,
            c"Element interleave is empty\n".as_ptr() as _,
            null_mut(),
            null_mut(),
        );
    }
    while !child.is_null() {
        if IS_RELAXNG!(child, c"element".as_ptr() as _) {
            cur = xml_relaxng_parse_element(ctxt, child);
        } else {
            cur = xml_relaxng_parse_pattern(ctxt, child);
        }
        if !cur.is_null() {
            (*cur).parent = def;
            if last.is_null() {
                (*def).content = cur;
                last = cur;
            } else {
                (*last).next = cur;
                last = cur;
            }
        }
        child = (*child).next;
    }

    def
}

/**
 * xmlRelaxNGParseImportRef:
 * @payload: the parser context
 * @data: the current grammar
 * @name: the reference name
 *
 * Import import one references into the current grammar
 */
extern "C" fn xml_relaxng_parse_import_ref(
    payload: *mut c_void,
    data: *mut c_void,
    name: *const XmlChar,
) {
    let ctxt: XmlRelaxNGParserCtxtPtr = data as _;
    let def: XmlRelaxNGDefinePtr = payload as _;

    unsafe {
        (*def).dflags |= IS_EXTERNAL_REF as i16;

        let tmp: i32 = xml_hash_add_entry((*(*ctxt).grammar).refs, name, def as _);
        if tmp < 0 {
            let prev: XmlRelaxNGDefinePtr =
                xml_hash_lookup((*(*ctxt).grammar).refs, (*def).name) as _;
            if prev.is_null() {
                if !(*def).name.is_null() {
                    xml_rng_perr(
                        ctxt,
                        null_mut(),
                        XmlParserErrors::XmlRngpRefCreateFailed,
                        c"Error refs definitions '%s'\n".as_ptr() as _,
                        (*def).name,
                        null_mut(),
                    );
                } else {
                    xml_rng_perr(
                        ctxt,
                        null_mut(),
                        XmlParserErrors::XmlRngpRefCreateFailed,
                        c"Error refs definitions\n".as_ptr() as _,
                        null_mut(),
                        null_mut(),
                    );
                }
            } else {
                (*def).next_hash = (*prev).next_hash;
                (*prev).next_hash = def;
            }
        }
    }
}

/**
 * xmlRelaxNGParseImportRefs:
 * @ctxt: the parser context
 * @grammar: the sub grammar
 *
 * Import references from the subgrammar into the current grammar
 *
 * Returns 0 in case of success, -1 in case of failure
 */
unsafe extern "C" fn xml_relaxng_parse_import_refs(
    ctxt: XmlRelaxNGParserCtxtPtr,
    grammar: XmlRelaxNGGrammarPtr,
) -> i32 {
    if ctxt.is_null() || grammar.is_null() || (*ctxt).grammar.is_null() {
        return -1;
    }
    if (*grammar).refs.is_null() {
        return 0;
    }
    if (*(*ctxt).grammar).refs.is_null() {
        (*(*ctxt).grammar).refs = xml_hash_create(10);
    }
    if (*(*ctxt).grammar).refs.is_null() {
        xml_rng_perr(
            ctxt,
            null_mut(),
            XmlParserErrors::XmlRngpRefCreateFailed,
            c"Could not create references hash\n".as_ptr() as _,
            null_mut(),
            null_mut(),
        );
        return -1;
    }
    xml_hash_scan(
        (*grammar).refs,
        Some(xml_relaxng_parse_import_ref),
        ctxt as _,
    );
    0
}

/**
 * xmlRelaxNGProcessExternalRef:
 * @ctxt: the parser context
 * @node:  the externalRef node
 *
 * Process and compile an externalRef node
 *
 * Returns the xmlRelaxNGDefinePtr or NULL in case of error
 */
unsafe extern "C" fn xml_relaxng_process_external_ref(
    ctxt: XmlRelaxNGParserCtxtPtr,
    node: XmlNodePtr,
) -> XmlRelaxNGDefinePtr {
    let root: XmlNodePtr;
    let mut tmp: XmlNodePtr;
    let mut ns: *mut XmlChar;
    let mut new_ns: i32 = 0;
    let oldflags: i32;
    let def: XmlRelaxNGDefinePtr;

    let docu: XmlRelaxNGDocumentPtr = (*node).psvi as _;
    if !docu.is_null() {
        def = xml_relaxng_new_define(ctxt, node);
        if def.is_null() {
            return null_mut();
        }
        (*def).typ = XmlRelaxNGType::Externalref;

        if (*docu).content.is_null() {
            /*
             * Then do the parsing for good
             */
            root = if (*docu).doc.is_null() {
                null_mut()
            } else {
                (*(*docu).doc).get_root_element()
            };
            if root.is_null() {
                xml_rng_perr(
                    ctxt,
                    node,
                    XmlParserErrors::XmlRngpExternalRefEmtpy,
                    c"xmlRelaxNGParse: %s is empty\n".as_ptr() as _,
                    (*ctxt).url,
                    null_mut(),
                );
                return null_mut();
            }
            /*
             * ns transmission rules
             */
            ns = xml_get_prop(root, c"ns".as_ptr() as _);
            if ns.is_null() {
                tmp = node;
                while !tmp.is_null() && (*tmp).typ == XmlElementType::XmlElementNode {
                    ns = xml_get_prop(tmp, c"ns".as_ptr() as _);
                    if !ns.is_null() {
                        break;
                    }
                    tmp = (*tmp).parent;
                }
                if !ns.is_null() {
                    xml_set_prop(root, c"ns".as_ptr() as _, ns);
                    new_ns = 1;
                    xml_free(ns as _);
                }
            } else {
                xml_free(ns as _);
            }

            /*
             * Parsing to get a precompiled schemas.
             */
            oldflags = (*ctxt).flags;
            (*ctxt).flags |= XML_RELAXNG_IN_EXTERNALREF;
            (*docu).schema = xml_relaxng_parse_document(ctxt, root);
            (*ctxt).flags = oldflags;
            if !(*docu).schema.is_null() && !(*(*docu).schema).topgrammar.is_null() {
                (*docu).content = (*(*(*docu).schema).topgrammar).start;
                if !(*(*(*docu).schema).topgrammar).refs.is_null() {
                    xml_relaxng_parse_import_refs(ctxt, (*(*docu).schema).topgrammar);
                }
            }

            /*
             * the externalRef may be reused in a different ns context
             */
            if new_ns == 1 {
                xml_unset_prop(root, c"ns".as_ptr() as _);
            }
        }
        (*def).content = (*docu).content;
    } else {
        def = null_mut();
    }
    def
}

/**
 * xmlRelaxNGParsePattern:
 * @ctxt:  a Relax-NG parser context
 * @node:  the pattern node.
 *
 * parse the content of a RelaxNG pattern node.
 *
 * Returns the definition pointer or NULL in case of error or if no
 *     pattern is generated.
 */
unsafe extern "C" fn xml_relaxng_parse_pattern(
    ctxt: XmlRelaxNGParserCtxtPtr,
    node: XmlNodePtr,
) -> XmlRelaxNGDefinePtr {
    let mut def: XmlRelaxNGDefinePtr;

    if node.is_null() {
        return null_mut();
    }
    if IS_RELAXNG!(node, c"element".as_ptr() as _) {
        def = xml_relaxng_parse_element(ctxt, node);
    } else if IS_RELAXNG!(node, c"attribute".as_ptr() as _) {
        def = xml_relaxng_parse_attribute(ctxt, node);
    } else if IS_RELAXNG!(node, c"empty".as_ptr() as _) {
        def = xml_relaxng_new_define(ctxt, node);
        if def.is_null() {
            return null_mut();
        }
        (*def).typ = XmlRelaxNGType::Empty;
        if !(*node).children.is_null() {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpEmptyNotEmpty,
                c"empty: had a child node\n".as_ptr() as _,
                null_mut(),
                null_mut(),
            );
        }
    } else if IS_RELAXNG!(node, c"text".as_ptr() as _) {
        def = xml_relaxng_new_define(ctxt, node);
        if def.is_null() {
            return null_mut();
        }
        (*def).typ = XmlRelaxNGType::Text;
        if !(*node).children.is_null() {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpTextHasChild,
                c"text: had a child node\n".as_ptr() as _,
                null_mut(),
                null_mut(),
            );
        }
    } else if IS_RELAXNG!(node, c"zeroOrMore".as_ptr() as _) {
        def = xml_relaxng_new_define(ctxt, node);
        if def.is_null() {
            return null_mut();
        }
        (*def).typ = XmlRelaxNGType::Zeroormore;
        if (*node).children.is_null() {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpEmptyConstruct,
                c"Element %s is empty\n".as_ptr() as _,
                (*node).name,
                null_mut(),
            );
        } else {
            (*def).content = xml_relaxng_parse_patterns(ctxt, (*node).children, 1);
        }
    } else if IS_RELAXNG!(node, c"oneOrMore".as_ptr() as _) {
        def = xml_relaxng_new_define(ctxt, node);
        if def.is_null() {
            return null_mut();
        }
        (*def).typ = XmlRelaxNGType::Oneormore;
        if (*node).children.is_null() {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpEmptyConstruct,
                c"Element %s is empty\n".as_ptr() as _,
                (*node).name,
                null_mut(),
            );
        } else {
            (*def).content = xml_relaxng_parse_patterns(ctxt, (*node).children, 1);
        }
    } else if IS_RELAXNG!(node, c"optional".as_ptr() as _) {
        def = xml_relaxng_new_define(ctxt, node);
        if def.is_null() {
            return null_mut();
        }
        (*def).typ = XmlRelaxNGType::Optional;
        if (*node).children.is_null() {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpEmptyConstruct,
                c"Element %s is empty\n".as_ptr() as _,
                (*node).name,
                null_mut(),
            );
        } else {
            (*def).content = xml_relaxng_parse_patterns(ctxt, (*node).children, 1);
        }
    } else if IS_RELAXNG!(node, c"choice".as_ptr() as _) {
        def = xml_relaxng_new_define(ctxt, node);
        if def.is_null() {
            return null_mut();
        }
        (*def).typ = XmlRelaxNGType::Choice;
        if (*node).children.is_null() {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpEmptyConstruct,
                c"Element %s is empty\n".as_ptr() as _,
                (*node).name,
                null_mut(),
            );
        } else {
            (*def).content = xml_relaxng_parse_patterns(ctxt, (*node).children, 0);
        }
    } else if IS_RELAXNG!(node, c"group".as_ptr() as _) {
        def = xml_relaxng_new_define(ctxt, node);
        if def.is_null() {
            return null_mut();
        }
        (*def).typ = XmlRelaxNGType::Group;
        if (*node).children.is_null() {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpEmptyConstruct,
                c"Element %s is empty\n".as_ptr() as _,
                (*node).name,
                null_mut(),
            );
        } else {
            (*def).content = xml_relaxng_parse_patterns(ctxt, (*node).children, 0);
        }
    } else if IS_RELAXNG!(node, c"ref".as_ptr() as _) {
        def = xml_relaxng_new_define(ctxt, node);
        if def.is_null() {
            return null_mut();
        }
        (*def).typ = XmlRelaxNGType::Ref;
        (*def).name = xml_get_prop(node, c"name".as_ptr() as _);
        if (*def).name.is_null() {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpRefNoName,
                c"ref has no name\n".as_ptr() as _,
                null_mut(),
                null_mut(),
            );
        } else {
            xml_relaxng_norm_ext_space((*def).name);
            if xml_validate_ncname((*def).name, 0) != 0 {
                xml_rng_perr(
                    ctxt,
                    node,
                    XmlParserErrors::XmlRngpRefNameInvalid,
                    c"ref name '%s' is not an NCName\n".as_ptr() as _,
                    (*def).name,
                    null_mut(),
                );
            }
        }
        if !(*node).children.is_null() {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpRefNotEmpty,
                c"ref is not empty\n".as_ptr() as _,
                null_mut(),
                null_mut(),
            );
        }
        if (*(*ctxt).grammar).refs.is_null() {
            (*(*ctxt).grammar).refs = xml_hash_create(10);
        }
        if (*(*ctxt).grammar).refs.is_null() {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpRefCreateFailed,
                c"Could not create references hash\n".as_ptr() as _,
                null_mut(),
                null_mut(),
            );
            def = null_mut();
        } else {
            let tmp: i32 = xml_hash_add_entry((*(*ctxt).grammar).refs, (*def).name, def as _);
            if tmp < 0 {
                let prev: XmlRelaxNGDefinePtr =
                    xml_hash_lookup((*(*ctxt).grammar).refs, (*def).name) as _;
                if prev.is_null() {
                    if !(*def).name.is_null() {
                        xml_rng_perr(
                            ctxt,
                            node,
                            XmlParserErrors::XmlRngpRefCreateFailed,
                            c"Error refs definitions '%s'\n".as_ptr() as _,
                            (*def).name,
                            null_mut(),
                        );
                    } else {
                        xml_rng_perr(
                            ctxt,
                            node,
                            XmlParserErrors::XmlRngpRefCreateFailed,
                            c"Error refs definitions\n".as_ptr() as _,
                            null_mut(),
                            null_mut(),
                        );
                    }
                    def = null_mut();
                } else {
                    (*def).next_hash = (*prev).next_hash;
                    (*prev).next_hash = def;
                }
            }
        }
    } else if IS_RELAXNG!(node, c"data".as_ptr() as _) {
        def = xml_relaxng_parse_data(ctxt, node);
    } else if IS_RELAXNG!(node, c"value".as_ptr() as _) {
        def = xml_relaxng_parse_value(ctxt, node);
    } else if IS_RELAXNG!(node, c"list".as_ptr() as _) {
        def = xml_relaxng_new_define(ctxt, node);
        if def.is_null() {
            return null_mut();
        }
        (*def).typ = XmlRelaxNGType::List;
        if (*node).children.is_null() {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpEmptyConstruct,
                c"Element %s is empty\n".as_ptr() as _,
                (*node).name,
                null_mut(),
            );
        } else {
            (*def).content = xml_relaxng_parse_patterns(ctxt, (*node).children, 0);
        }
    } else if IS_RELAXNG!(node, c"interleave".as_ptr() as _) {
        def = xml_relaxng_parse_interleave(ctxt, node);
    } else if IS_RELAXNG!(node, c"externalRef".as_ptr() as _) {
        def = xml_relaxng_process_external_ref(ctxt, node);
    } else if IS_RELAXNG!(node, c"notAllowed".as_ptr() as _) {
        def = xml_relaxng_new_define(ctxt, node);
        if def.is_null() {
            return null_mut();
        }
        (*def).typ = XmlRelaxNGType::NotAllowed;
        if !(*node).children.is_null() {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpNotAllowedNotEmpty,
                c"xmlRelaxNGParse: notAllowed element is not empty\n".as_ptr() as _,
                null_mut(),
                null_mut(),
            );
        }
    } else if IS_RELAXNG!(node, c"grammar".as_ptr() as _) {
        let oldparent: XmlRelaxNGGrammarPtr = (*ctxt).parentgrammar;
        let old: XmlRelaxNGGrammarPtr = (*ctxt).grammar;
        (*ctxt).parentgrammar = old;
        let grammar: XmlRelaxNGGrammarPtr = xml_relaxng_parse_grammar(ctxt, (*node).children);
        if !old.is_null() {
            (*ctxt).grammar = old;
            (*ctxt).parentgrammar = oldparent;
        }
        if !grammar.is_null() {
            def = (*grammar).start;
        } else {
            def = null_mut();
        }
    } else if IS_RELAXNG!(node, c"parentRef".as_ptr() as _) {
        if (*ctxt).parentgrammar.is_null() {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpParentRefNoParent,
                c"Use of parentRef without a parent grammar\n".as_ptr() as _,
                null_mut(),
                null_mut(),
            );
            return null_mut();
        }
        def = xml_relaxng_new_define(ctxt, node);
        if def.is_null() {
            return null_mut();
        }
        (*def).typ = XmlRelaxNGType::Parentref;
        (*def).name = xml_get_prop(node, c"name".as_ptr() as _);
        if (*def).name.is_null() {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpParentRefNoName,
                c"parentRef has no name\n".as_ptr() as _,
                null_mut(),
                null_mut(),
            );
        } else {
            xml_relaxng_norm_ext_space((*def).name);
            if xml_validate_ncname((*def).name, 0) != 0 {
                xml_rng_perr(
                    ctxt,
                    node,
                    XmlParserErrors::XmlRngpParentRefNameInvalid,
                    c"parentRef name '%s' is not an NCName\n".as_ptr() as _,
                    (*def).name,
                    null_mut(),
                );
            }
        }
        if !(*node).children.is_null() {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpParentRefNotEmpty,
                c"parentRef is not empty\n".as_ptr() as _,
                null_mut(),
                null_mut(),
            );
        }
        if (*(*ctxt).parentgrammar).refs.is_null() {
            (*(*ctxt).parentgrammar).refs = xml_hash_create(10);
        }
        if (*(*ctxt).parentgrammar).refs.is_null() {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpParentRefCreateFailed,
                c"Could not create references hash\n".as_ptr() as _,
                null_mut(),
                null_mut(),
            );
            def = null_mut();
        } else if !(*def).name.is_null() {
            let tmp: i32 = xml_hash_add_entry((*(*ctxt).parentgrammar).refs, (*def).name, def as _);
            if tmp < 0 {
                let prev: XmlRelaxNGDefinePtr =
                    xml_hash_lookup((*(*ctxt).parentgrammar).refs, (*def).name) as _;
                if prev.is_null() {
                    xml_rng_perr(
                        ctxt,
                        node,
                        XmlParserErrors::XmlRngpParentRefCreateFailed,
                        c"Internal error parentRef definitions '%s'\n".as_ptr() as _,
                        (*def).name,
                        null_mut(),
                    );
                    def = null_mut();
                } else {
                    (*def).next_hash = (*prev).next_hash;
                    (*prev).next_hash = def;
                }
            }
        }
    } else if IS_RELAXNG!(node, c"mixed".as_ptr() as _) {
        if (*node).children.is_null() {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpEmptyConstruct,
                c"Mixed is empty\n".as_ptr() as _,
                null_mut(),
                null_mut(),
            );
            def = null_mut();
        } else {
            def = xml_relaxng_parse_interleave(ctxt, node);
            if !def.is_null() {
                let mut tmp: XmlRelaxNGDefinePtr;

                if !(*def).content.is_null() && !(*(*def).content).next.is_null() {
                    tmp = xml_relaxng_new_define(ctxt, node);
                    if !tmp.is_null() {
                        (*tmp).typ = XmlRelaxNGType::Group;
                        (*tmp).content = (*def).content;
                        (*def).content = tmp;
                    }
                }

                tmp = xml_relaxng_new_define(ctxt, node);
                if tmp.is_null() {
                    return def;
                }
                (*tmp).typ = XmlRelaxNGType::Text;
                (*tmp).next = (*def).content;
                (*def).content = tmp;
            }
        }
    } else {
        xml_rng_perr(
            ctxt,
            node,
            XmlParserErrors::XmlRngpUnknownConstruct,
            c"Unexpected node %s is not a pattern\n".as_ptr() as _,
            (*node).name,
            null_mut(),
        );
        def = null_mut();
    }
    def
}

/**
 * xmlRelaxNGParseElement:
 * @ctxt:  a Relax-NG parser context
 * @node:  the element node
 *
 * parse the content of a RelaxNG element node.
 *
 * Returns the definition pointer or NULL in case of error.
 */
unsafe extern "C" fn xml_relaxng_parse_element(
    ctxt: XmlRelaxNGParserCtxtPtr,
    node: XmlNodePtr,
) -> XmlRelaxNGDefinePtr {
    let mut cur: XmlRelaxNGDefinePtr;
    let mut last: XmlRelaxNGDefinePtr;
    let mut child: XmlNodePtr;

    let ret: XmlRelaxNGDefinePtr = xml_relaxng_new_define(ctxt, node);
    if ret.is_null() {
        return null_mut();
    }
    (*ret).typ = XmlRelaxNGType::Element;
    (*ret).parent = (*ctxt).def;
    child = (*node).children;
    if child.is_null() {
        xml_rng_perr(
            ctxt,
            node,
            XmlParserErrors::XmlRngpElementEmpty,
            c"xmlRelaxNGParseElement: element has no children\n".as_ptr() as _,
            null_mut(),
            null_mut(),
        );
        return ret;
    }
    cur = xml_relaxng_parse_name_class(ctxt, child, ret);
    if !cur.is_null() {
        child = (*child).next;
    }

    if child.is_null() {
        xml_rng_perr(
            ctxt,
            node,
            XmlParserErrors::XmlRngpElementNoContent,
            c"xmlRelaxNGParseElement: element has no content\n".as_ptr() as _,
            null_mut(),
            null_mut(),
        );
        return ret;
    }
    let olddefine: *const XmlChar = (*ctxt).define;
    (*ctxt).define = null_mut();
    last = null_mut();
    while !child.is_null() {
        cur = xml_relaxng_parse_pattern(ctxt, child);
        if !cur.is_null() {
            (*cur).parent = ret;
            match (*cur).typ {
                XmlRelaxNGType::Empty
                | XmlRelaxNGType::NotAllowed
                | XmlRelaxNGType::Text
                | XmlRelaxNGType::Element
                | XmlRelaxNGType::Datatype
                | XmlRelaxNGType::Value
                | XmlRelaxNGType::List
                | XmlRelaxNGType::Ref
                | XmlRelaxNGType::Parentref
                | XmlRelaxNGType::Externalref
                | XmlRelaxNGType::Def
                | XmlRelaxNGType::Zeroormore
                | XmlRelaxNGType::Oneormore
                | XmlRelaxNGType::Optional
                | XmlRelaxNGType::Choice
                | XmlRelaxNGType::Group
                | XmlRelaxNGType::Interleave => {
                    if last.is_null() {
                        (*ret).content = cur;
                        last = cur;
                    } else {
                        if (*last).typ == XmlRelaxNGType::Element && (*ret).content == last {
                            (*ret).content = xml_relaxng_new_define(ctxt, node);
                            if !(*ret).content.is_null() {
                                (*(*ret).content).typ = XmlRelaxNGType::Group;
                                (*(*ret).content).content = last;
                            } else {
                                (*ret).content = last;
                            }
                        }
                        (*last).next = cur;
                        last = cur;
                    }
                }
                XmlRelaxNGType::Attribute => {
                    (*cur).next = (*ret).attrs;
                    (*ret).attrs = cur;
                }
                XmlRelaxNGType::Start => {
                    xml_rng_perr(
                        ctxt,
                        node,
                        XmlParserErrors::XmlRngpElementContent,
                        c"RNG Internal error, start found in element\n".as_ptr() as _,
                        null_mut(),
                        null_mut(),
                    );
                }
                XmlRelaxNGType::Param => {
                    xml_rng_perr(
                        ctxt,
                        node,
                        XmlParserErrors::XmlRngpElementContent,
                        c"RNG Internal error, param found in element\n".as_ptr() as _,
                        null_mut(),
                        null_mut(),
                    );
                }
                XmlRelaxNGType::Except => {
                    xml_rng_perr(
                        ctxt,
                        node,
                        XmlParserErrors::XmlRngpElementContent,
                        c"RNG Internal error, except found in element\n".as_ptr() as _,
                        null_mut(),
                        null_mut(),
                    );
                }
                XmlRelaxNGType::Noop => {
                    xml_rng_perr(
                        ctxt,
                        node,
                        XmlParserErrors::XmlRngpElementContent,
                        c"RNG Internal error, noop found in element\n".as_ptr() as _,
                        null_mut(),
                        null_mut(),
                    );
                }
            }
        }
        child = (*child).next;
    }
    (*ctxt).define = olddefine;
    ret
}

/**
 * xmlRelaxNGParsePatterns:
 * @ctxt:  a Relax-NG parser context
 * @nodes:  list of nodes
 * @group:  use an implicit <group> for elements
 *
 * parse the content of a RelaxNG start node.
 *
 * Returns the definition pointer or NULL in case of error.
 */
unsafe extern "C" fn xml_relaxng_parse_patterns(
    ctxt: XmlRelaxNGParserCtxtPtr,
    mut nodes: XmlNodePtr,
    group: i32,
) -> XmlRelaxNGDefinePtr {
    let mut def: XmlRelaxNGDefinePtr = null_mut();
    let mut last: XmlRelaxNGDefinePtr = null_mut();
    let mut cur: XmlRelaxNGDefinePtr;

    let parent: XmlRelaxNGDefinePtr = (*ctxt).def;
    while !nodes.is_null() {
        if IS_RELAXNG!(nodes, c"element".as_ptr() as _) {
            cur = xml_relaxng_parse_element(ctxt, nodes);
            if cur.is_null() {
                return null_mut();
            }
            if def.is_null() {
                def = cur;
                last = cur;
            } else {
                if group == 1 && (*def).typ == XmlRelaxNGType::Element && def == last {
                    def = xml_relaxng_new_define(ctxt, nodes);
                    if def.is_null() {
                        return null_mut();
                    }
                    (*def).typ = XmlRelaxNGType::Group;
                    (*def).content = last;
                }
                (*last).next = cur;
                last = cur;
            }
            (*cur).parent = parent;
        } else {
            cur = xml_relaxng_parse_pattern(ctxt, nodes);
            if !cur.is_null() {
                if def.is_null() {
                    def = cur;
                    last = cur;
                } else {
                    (*last).next = cur;
                    last = cur;
                }
            }
        }
        nodes = (*nodes).next;
    }
    def
}

/**
 * xmlRelaxNGParseStart:
 * @ctxt:  a Relax-NG parser context
 * @nodes:  start children nodes
 *
 * parse the content of a RelaxNG start node.
 *
 * Returns 0 in case of success, -1 in case of error
 */
unsafe extern "C" fn xml_relaxng_parse_start(
    ctxt: XmlRelaxNGParserCtxtPtr,
    mut nodes: XmlNodePtr,
) -> i32 {
    let ret: i32 = 0;
    let def: XmlRelaxNGDefinePtr;
    let mut last: XmlRelaxNGDefinePtr;

    if nodes.is_null() {
        xml_rng_perr(
            ctxt,
            nodes,
            XmlParserErrors::XmlRngpStartEmpty,
            c"start has no children\n".as_ptr() as _,
            null_mut(),
            null_mut(),
        );
        return -1;
    }
    if IS_RELAXNG!(nodes, c"empty".as_ptr() as _) {
        def = xml_relaxng_new_define(ctxt, nodes);
        if def.is_null() {
            return -1;
        }
        (*def).typ = XmlRelaxNGType::Empty;
        if !(*nodes).children.is_null() {
            xml_rng_perr(
                ctxt,
                nodes,
                XmlParserErrors::XmlRngpEmptyContent,
                c"element empty is not empty\n".as_ptr() as _,
                null_mut(),
                null_mut(),
            );
        }
    } else if IS_RELAXNG!(nodes, c"notAllowed".as_ptr() as _) {
        def = xml_relaxng_new_define(ctxt, nodes);
        if def.is_null() {
            return -1;
        }
        (*def).typ = XmlRelaxNGType::NotAllowed;
        if !(*nodes).children.is_null() {
            xml_rng_perr(
                ctxt,
                nodes,
                XmlParserErrors::XmlRngpNotAllowedNotEmpty,
                c"element notAllowed is not empty\n".as_ptr() as _,
                null_mut(),
                null_mut(),
            );
        }
    } else {
        def = xml_relaxng_parse_patterns(ctxt, nodes, 1);
    }
    if !(*(*ctxt).grammar).start.is_null() {
        last = (*(*ctxt).grammar).start;
        while !(*last).next.is_null() {
            last = (*last).next;
        }
        (*last).next = def;
    } else {
        (*(*ctxt).grammar).start = def;
    }
    nodes = (*nodes).next;
    if !nodes.is_null() {
        xml_rng_perr(
            ctxt,
            nodes,
            XmlParserErrors::XmlRngpStartContent,
            c"start more than one children\n".as_ptr() as _,
            null_mut(),
            null_mut(),
        );
        return -1;
    }
    ret
}

/**
 * xmlRelaxNGParseDefine:
 * @ctxt:  a Relax-NG parser context
 * @node:  the define node
 *
 * parse the content of a RelaxNG define element node.
 *
 * Returns 0 in case of success or -1 in case of error
 */
unsafe extern "C" fn xml_relaxng_parse_define(
    ctxt: XmlRelaxNGParserCtxtPtr,
    node: XmlNodePtr,
) -> i32 {
    let mut ret: i32 = 0;
    let tmp: i32;
    let def: XmlRelaxNGDefinePtr;
    let olddefine: *const XmlChar;

    let name: *mut XmlChar = xml_get_prop(node, c"name".as_ptr() as _);
    if name.is_null() {
        xml_rng_perr(
            ctxt,
            node,
            XmlParserErrors::XmlRngpDefineNameMissing,
            c"define has no name\n".as_ptr() as _,
            null_mut(),
            null_mut(),
        );
    } else {
        xml_relaxng_norm_ext_space(name);
        if xml_validate_ncname(name, 0) != 0 {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpInvalidDefineName,
                c"define name '%s' is not an NCName\n".as_ptr() as _,
                name,
                null_mut(),
            );
        }
        def = xml_relaxng_new_define(ctxt, node);
        if def.is_null() {
            xml_free(name as _);
            return -1;
        }
        (*def).typ = XmlRelaxNGType::Def;
        (*def).name = name;
        if (*node).children.is_null() {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpDefineEmpty,
                c"define has no children\n".as_ptr() as _,
                null_mut(),
                null_mut(),
            );
        } else {
            olddefine = (*ctxt).define;
            (*ctxt).define = name;
            (*def).content = xml_relaxng_parse_patterns(ctxt, (*node).children, 0);
            (*ctxt).define = olddefine;
        }
        if (*(*ctxt).grammar).defs.is_null() {
            (*(*ctxt).grammar).defs = xml_hash_create(10);
        }
        if (*(*ctxt).grammar).defs.is_null() {
            xml_rng_perr(
                ctxt,
                node,
                XmlParserErrors::XmlRngpDefineCreateFailed,
                c"Could not create definition hash\n".as_ptr() as _,
                null_mut(),
                null_mut(),
            );
            ret = -1;
        } else {
            tmp = xml_hash_add_entry((*(*ctxt).grammar).defs, name, def as _);
            if tmp < 0 {
                let mut prev: XmlRelaxNGDefinePtr;

                prev = xml_hash_lookup((*(*ctxt).grammar).defs, name) as _;
                if prev.is_null() {
                    xml_rng_perr(
                        ctxt,
                        node,
                        XmlParserErrors::XmlRngpDefineCreateFailed,
                        c"Internal error on define aggregation of %s\n".as_ptr() as _,
                        name,
                        null_mut(),
                    );
                    ret = -1;
                } else {
                    while !(*prev).next_hash.is_null() {
                        prev = (*prev).next_hash;
                    }
                    (*prev).next_hash = def;
                }
            }
        }
    }
    ret
}

/**
 * xmlRelaxNGParseInclude:
 * @ctxt:  a Relax-NG parser context
 * @node:  the include node
 *
 * Integrate the content of an include node in the current grammar
 *
 * Returns 0 in case of success or -1 in case of error
 */
unsafe extern "C" fn xml_relaxng_parse_include(
    ctxt: XmlRelaxNGParserCtxtPtr,
    node: XmlNodePtr,
) -> i32 {
    let mut ret: i32 = 0;
    let mut tmp: i32;

    let incl: XmlRelaxNGIncludePtr = (*node).psvi as _;
    if incl.is_null() {
        xml_rng_perr(
            ctxt,
            node,
            XmlParserErrors::XmlRngpIncludeEmpty,
            c"Include node has no data\n".as_ptr() as _,
            null_mut(),
            null_mut(),
        );
        return -1;
    }
    let root: XmlNodePtr = if (*incl).doc.is_null() {
        null_mut()
    } else {
        (*(*incl).doc).get_root_element()
    };
    if root.is_null() {
        xml_rng_perr(
            ctxt,
            node,
            XmlParserErrors::XmlRngpEmpty,
            c"Include document is empty\n".as_ptr() as _,
            null_mut(),
            null_mut(),
        );
        return -1;
    }
    if !xml_str_equal((*root).name, c"grammar".as_ptr() as _) {
        xml_rng_perr(
            ctxt,
            node,
            XmlParserErrors::XmlRngpGrammarMissing,
            c"Include document root is not a grammar\n".as_ptr() as _,
            null_mut(),
            null_mut(),
        );
        return -1;
    }

    /*
     * Merge the definition from both the include and the internal list
     */
    if !(*root).children.is_null() {
        tmp = xml_relaxng_parse_grammar_content(ctxt, (*root).children);
        if tmp != 0 {
            ret = -1;
        }
    }
    if !(*node).children.is_null() {
        tmp = xml_relaxng_parse_grammar_content(ctxt, (*node).children);
        if tmp != 0 {
            ret = -1;
        }
    }
    ret
}

/**
 * xmlRelaxNGParseGrammarContent:
 * @ctxt:  a Relax-NG parser context
 * @nodes:  grammar children nodes
 *
 * parse the content of a RelaxNG grammar node.
 *
 * Returns 0 in case of success, -1 in case of error
 */
unsafe extern "C" fn xml_relaxng_parse_grammar_content(
    ctxt: XmlRelaxNGParserCtxtPtr,
    mut nodes: XmlNodePtr,
) -> i32 {
    let mut ret: i32 = 0;
    let mut tmp: i32;

    if nodes.is_null() {
        xml_rng_perr(
            ctxt,
            nodes,
            XmlParserErrors::XmlRngpGrammarEmpty,
            c"grammar has no children\n".as_ptr() as _,
            null_mut(),
            null_mut(),
        );
        return -1;
    }
    while !nodes.is_null() {
        if IS_RELAXNG!(nodes, c"start".as_ptr() as _) {
            if (*nodes).children.is_null() {
                xml_rng_perr(
                    ctxt,
                    nodes,
                    XmlParserErrors::XmlRngpStartEmpty,
                    c"start has no children\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            } else {
                tmp = xml_relaxng_parse_start(ctxt, (*nodes).children);
                if tmp != 0 {
                    ret = -1;
                }
            }
        } else if IS_RELAXNG!(nodes, c"define".as_ptr() as _) {
            tmp = xml_relaxng_parse_define(ctxt, nodes);
            if tmp != 0 {
                ret = -1;
            }
        } else if IS_RELAXNG!(nodes, c"include".as_ptr() as _) {
            tmp = xml_relaxng_parse_include(ctxt, nodes);
            if tmp != 0 {
                ret = -1;
            }
        } else {
            xml_rng_perr(
                ctxt,
                nodes,
                XmlParserErrors::XmlRngpGrammarContent,
                c"grammar has unexpected child %s\n".as_ptr() as _,
                (*nodes).name,
                null_mut(),
            );
            ret = -1;
        }
        nodes = (*nodes).next;
    }
    ret
}

/**
 * xmlRelaxNGCombineStart:
 * @ctxt:  a Relax-NG parser context
 * @grammar:  the grammar
 *
 * Applies the 4.17. combine rule for all the start
 * element of a given grammar.
 */
unsafe extern "C" fn xml_relaxng_combine_start(
    ctxt: XmlRelaxNGParserCtxtPtr,
    grammar: XmlRelaxNGGrammarPtr,
) {
    let mut combine: *mut XmlChar;
    let mut choice_or_interleave: i32 = -1;
    let mut missing: i32 = 0;
    let mut cur: XmlRelaxNGDefinePtr;

    let starts: XmlRelaxNGDefinePtr = (*grammar).start;
    if starts.is_null() || (*starts).next.is_null() {
        return;
    }
    cur = starts;
    while !cur.is_null() {
        if (*cur).node.is_null()
            || (*(*cur).node).parent.is_null()
            || !xml_str_equal((*(*(*cur).node).parent).name, c"start".as_ptr() as _)
        {
            combine = null_mut();
            xml_rng_perr(
                ctxt,
                (*cur).node,
                XmlParserErrors::XmlRngpStartMissing,
                c"Internal error: start element not found\n".as_ptr() as _,
                null_mut(),
                null_mut(),
            );
        } else {
            combine = xml_get_prop((*(*cur).node).parent, c"combine".as_ptr() as _);
        }

        if !combine.is_null() {
            if xml_str_equal(combine, c"choice".as_ptr() as _) {
                if choice_or_interleave == -1 {
                    choice_or_interleave = 1;
                } else if choice_or_interleave == 0 {
                    xml_rng_perr(
                        ctxt,
                        (*cur).node,
                        XmlParserErrors::XmlRngpStartChoiceAndInterleave,
                        c"<start> use both 'choice' and 'interleave'\n".as_ptr() as _,
                        null_mut(),
                        null_mut(),
                    );
                }
            } else if xml_str_equal(combine, c"interleave".as_ptr() as _) {
                if choice_or_interleave == -1 {
                    choice_or_interleave = 0;
                } else if choice_or_interleave == 1 {
                    xml_rng_perr(
                        ctxt,
                        (*cur).node,
                        XmlParserErrors::XmlRngpStartChoiceAndInterleave,
                        c"<start> use both 'choice' and 'interleave'\n".as_ptr() as _,
                        null_mut(),
                        null_mut(),
                    );
                }
            } else {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpUnknownCombine,
                    c"<start> uses unknown combine value '%s''\n".as_ptr() as _,
                    combine,
                    null_mut(),
                );
            }
            xml_free(combine as _);
        } else if missing == 0 {
            missing = 1;
        } else {
            xml_rng_perr(
                ctxt,
                (*cur).node,
                XmlParserErrors::XmlRngpNeedCombine,
                c"Some <start> element miss the combine attribute\n".as_ptr() as _,
                null_mut(),
                null_mut(),
            );
        }

        cur = (*cur).next;
    }
    if choice_or_interleave == -1 {
        choice_or_interleave = 0;
    }
    cur = xml_relaxng_new_define(ctxt, (*starts).node);
    if cur.is_null() {
        return;
    }
    if choice_or_interleave == 0 {
        (*cur).typ = XmlRelaxNGType::Interleave;
    } else {
        (*cur).typ = XmlRelaxNGType::Choice;
    }
    (*cur).content = (*grammar).start;
    (*grammar).start = cur;
    if choice_or_interleave == 0 {
        if (*ctxt).interleaves.is_null() {
            (*ctxt).interleaves = xml_hash_create(10);
        }
        if (*ctxt).interleaves.is_null() {
            xml_rng_perr(
                ctxt,
                (*cur).node,
                XmlParserErrors::XmlRngpInterleaveCreateFailed,
                c"Failed to create interleaves hash table\n".as_ptr() as _,
                null_mut(),
                null_mut(),
            );
        } else {
            let mut tmpname: [c_char; 32] = [0; 32];

            snprintf(
                tmpname.as_mut_ptr() as _,
                32,
                c"interleave%d".as_ptr() as _,
                (*ctxt).nb_interleaves,
            );
            (*ctxt).nb_interleaves += 1;
            if xml_hash_add_entry((*ctxt).interleaves, tmpname.as_ptr() as _, cur as _) < 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpInterleaveCreateFailed,
                    c"Failed to add %s to hash table\n".as_ptr() as _,
                    tmpname.as_ptr() as _,
                    null_mut(),
                );
            }
        }
    }
}

/**
 * xmlRelaxNGParseGrammar:
 * @ctxt:  a Relax-NG parser context
 * @nodes:  grammar children nodes
 *
 * parse a Relax-NG <grammar> node
 *
 * Returns the internal xmlRelaxNGGrammarPtr built or
 *         NULL in case of error
 */
unsafe extern "C" fn xml_relaxng_parse_grammar(
    ctxt: XmlRelaxNGParserCtxtPtr,
    nodes: XmlNodePtr,
) -> XmlRelaxNGGrammarPtr {
    let mut tmp: XmlRelaxNGGrammarPtr;

    let ret: XmlRelaxNGGrammarPtr = xml_relaxng_new_grammar(ctxt);
    if ret.is_null() {
        return null_mut();
    }

    /*
     * Link the new grammar in the tree
     */
    (*ret).parent = (*ctxt).grammar;
    if !(*ctxt).grammar.is_null() {
        tmp = (*(*ctxt).grammar).children;
        if tmp.is_null() {
            (*(*ctxt).grammar).children = ret;
        } else {
            while !(*tmp).next.is_null() {
                tmp = (*tmp).next;
            }
            (*tmp).next = ret;
        }
    }

    let old: XmlRelaxNGGrammarPtr = (*ctxt).grammar;
    (*ctxt).grammar = ret;
    xml_relaxng_parse_grammar_content(ctxt, nodes);
    (*ctxt).grammar = ret;
    if (*ctxt).grammar.is_null() {
        xml_rng_perr(
            ctxt,
            nodes,
            XmlParserErrors::XmlRngpGrammarContent,
            c"Failed to parse <grammar> content\n".as_ptr() as _,
            null_mut(),
            null_mut(),
        );
    } else if (*(*ctxt).grammar).start.is_null() {
        xml_rng_perr(
            ctxt,
            nodes,
            XmlParserErrors::XmlRngpGrammarNoStart,
            c"Element <grammar> has no <start>\n".as_ptr() as _,
            null_mut(),
            null_mut(),
        );
    }

    /*
     * Apply 4.17 merging rules to defines and starts
     */
    xml_relaxng_combine_start(ctxt, ret);
    if !(*ret).defs.is_null() {
        xml_hash_scan((*ret).defs, Some(xml_relaxng_check_combine), ctxt as _);
    }

    /*
     * link together defines and refs in this grammar
     */
    if !(*ret).refs.is_null() {
        xml_hash_scan((*ret).refs, Some(xml_relaxng_check_reference), ctxt as _);
    }

    /* @@@@ */

    (*ctxt).grammar = old;
    ret
}

/**
 * xmlRelaxNGCheckCycles:
 * @ctxt:  a Relax-NG parser context
 * @nodes:  grammar children nodes
 * @depth:  the counter
 *
 * Check for cycles.
 *
 * Returns 0 if check passed, and -1 in case of error
 */
unsafe extern "C" fn xml_relaxng_check_cycles(
    ctxt: XmlRelaxNGParserCtxtPtr,
    mut cur: XmlRelaxNGDefinePtr,
    depth: i32,
) -> i32 {
    let mut ret: i32 = 0;

    while ret == 0 && !cur.is_null() {
        if matches!((*cur).typ, XmlRelaxNGType::Ref | XmlRelaxNGType::Parentref) {
            if (*cur).depth == -1 {
                (*cur).depth = depth as _;
                ret = xml_relaxng_check_cycles(ctxt, (*cur).content, depth);
                (*cur).depth = -2;
            } else if depth == (*cur).depth as i32 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpRefCycle,
                    c"Detected a cycle in %s references\n".as_ptr() as _,
                    (*cur).name,
                    null_mut(),
                );
                return -1;
            }
        } else if (*cur).typ == XmlRelaxNGType::Element {
            ret = xml_relaxng_check_cycles(ctxt, (*cur).content, depth + 1);
        } else {
            ret = xml_relaxng_check_cycles(ctxt, (*cur).content, depth);
        }
        cur = (*cur).next;
    }
    ret
}

/**
 * xmlRelaxNGTryUnlink:
 * @ctxt:  a Relax-NG parser context
 * @cur:  the definition to unlink
 * @parent:  the parent definition
 * @prev:  the previous sibling definition
 *
 * Try to unlink a definition. If not possible make it a NOOP
 *
 * Returns the new prev definition
 */
unsafe extern "C" fn xml_relaxng_try_unlink(
    _ctxt: XmlRelaxNGParserCtxtPtr,
    cur: XmlRelaxNGDefinePtr,
    parent: XmlRelaxNGDefinePtr,
    mut prev: XmlRelaxNGDefinePtr,
) -> XmlRelaxNGDefinePtr {
    if !prev.is_null() {
        (*prev).next = (*cur).next;
    } else if !parent.is_null() {
        if (*parent).content == cur {
            (*parent).content = (*cur).next;
        } else if (*parent).attrs == cur {
            (*parent).attrs = (*cur).next;
        } else if (*parent).name_class == cur {
            (*parent).name_class = (*cur).next;
        }
    } else {
        (*cur).typ = XmlRelaxNGType::Noop;
        prev = cur;
    }
    prev
}

/**
 * xmlRelaxNGGenerateAttributes:
 * @ctxt:  a Relax-NG parser context
 * @def:  the definition definition
 *
 * Check if the definition can only generate attributes
 *
 * Returns 1 if yes, 0 if no and -1 in case of error.
 */
unsafe extern "C" fn xml_relaxng_generate_attributes(
    ctxt: XmlRelaxNGParserCtxtPtr,
    def: XmlRelaxNGDefinePtr,
) -> i32 {
    let mut parent: XmlRelaxNGDefinePtr;
    let mut cur: XmlRelaxNGDefinePtr;
    let mut tmp: XmlRelaxNGDefinePtr;

    /*
     * Don't run that check in case of error. Infinite recursion
     * becomes possible.
     */
    if (*ctxt).nb_errors != 0 {
        return -1;
    }

    // parent = null_mut();
    cur = def;
    while !cur.is_null() {
        if matches!(
            (*cur).typ,
            XmlRelaxNGType::Element
                | XmlRelaxNGType::Text
                | XmlRelaxNGType::Datatype
                | XmlRelaxNGType::Param
                | XmlRelaxNGType::List
                | XmlRelaxNGType::Value
                | XmlRelaxNGType::Empty
        ) {
            return 0;
        }
        if matches!(
            (*cur).typ,
            XmlRelaxNGType::Choice
                | XmlRelaxNGType::Interleave
                | XmlRelaxNGType::Group
                | XmlRelaxNGType::Oneormore
                | XmlRelaxNGType::Zeroormore
                | XmlRelaxNGType::Optional
                | XmlRelaxNGType::Parentref
                | XmlRelaxNGType::Externalref
                | XmlRelaxNGType::Ref
                | XmlRelaxNGType::Def
        ) && !(*cur).content.is_null()
        {
            parent = cur;
            cur = (*cur).content;
            tmp = cur;
            while !tmp.is_null() {
                (*tmp).parent = parent;
                tmp = (*tmp).next;
            }
            continue;
        }
        if cur == def {
            break;
        }
        if !(*cur).next.is_null() {
            cur = (*cur).next;
            continue;
        }
        loop {
            cur = (*cur).parent;
            if cur.is_null() {
                break;
            }
            if cur == def {
                return 1;
            }
            if !(*cur).next.is_null() {
                cur = (*cur).next;
                break;
            }

            if cur.is_null() {
                break;
            }
        }
    }
    1
}

/**
 * xmlRelaxNGSimplify:
 * @ctxt:  a Relax-NG parser context
 * @nodes:  grammar children nodes
 *
 * Check for simplification of empty and notAllowed
 */
unsafe extern "C" fn xml_relaxng_simplify(
    ctxt: XmlRelaxNGParserCtxtPtr,
    mut cur: XmlRelaxNGDefinePtr,
    parent: XmlRelaxNGDefinePtr,
) {
    let mut prev: XmlRelaxNGDefinePtr = null_mut();

    while !cur.is_null() {
        if matches!((*cur).typ, XmlRelaxNGType::Ref | XmlRelaxNGType::Parentref) {
            if (*cur).depth != -3 {
                (*cur).depth = -3;
                xml_relaxng_simplify(ctxt, (*cur).content, cur);
            }
        } else if (*cur).typ == XmlRelaxNGType::NotAllowed {
            (*cur).parent = parent;
            if !parent.is_null()
                && matches!(
                    (*parent).typ,
                    XmlRelaxNGType::Attribute
                        | XmlRelaxNGType::List
                        | XmlRelaxNGType::Group
                        | XmlRelaxNGType::Interleave
                        | XmlRelaxNGType::Oneormore
                        | XmlRelaxNGType::Zeroormore
                )
            {
                (*parent).typ = XmlRelaxNGType::NotAllowed;
                break;
            }
            if !parent.is_null() && (*parent).typ == XmlRelaxNGType::Choice {
                prev = xml_relaxng_try_unlink(ctxt, cur, parent, prev);
            } else {
                prev = cur;
            }
        } else if (*cur).typ == XmlRelaxNGType::Empty {
            (*cur).parent = parent;
            if !parent.is_null()
                && matches!(
                    (*parent).typ,
                    XmlRelaxNGType::Oneormore | XmlRelaxNGType::Zeroormore
                )
            {
                (*parent).typ = XmlRelaxNGType::Empty;
                break;
            }
            if !parent.is_null()
                && matches!(
                    (*parent).typ,
                    XmlRelaxNGType::Group | XmlRelaxNGType::Interleave
                )
            {
                prev = xml_relaxng_try_unlink(ctxt, cur, parent, prev);
            } else {
                prev = cur;
            }
        } else {
            (*cur).parent = parent;
            if !(*cur).content.is_null() {
                xml_relaxng_simplify(ctxt, (*cur).content, cur);
            }
            if (*cur).typ != XmlRelaxNGType::Value && !(*cur).attrs.is_null() {
                xml_relaxng_simplify(ctxt, (*cur).attrs, cur);
            }
            if !(*cur).name_class.is_null() {
                xml_relaxng_simplify(ctxt, (*cur).name_class, cur);
            }
            /*
             * On Elements, try to move attribute only generating rules on
             * the attrs rules.
             */
            if (*cur).typ == XmlRelaxNGType::Element {
                let mut attronly: i32;
                let mut tmp: XmlRelaxNGDefinePtr;
                let mut pre: XmlRelaxNGDefinePtr;

                while !(*cur).content.is_null() {
                    attronly = xml_relaxng_generate_attributes(ctxt, (*cur).content);
                    if attronly == 1 {
                        /*
                         * migrate (*cur).content to attrs
                         */
                        tmp = (*cur).content;
                        (*cur).content = (*tmp).next;
                        (*tmp).next = (*cur).attrs;
                        (*cur).attrs = tmp;
                    } else {
                        /*
                         * (*cur).content can generate elements or text
                         */
                        break;
                    }
                }
                pre = (*cur).content;
                while !pre.is_null() && !(*pre).next.is_null() {
                    tmp = (*pre).next;
                    attronly = xml_relaxng_generate_attributes(ctxt, tmp);
                    if attronly == 1 {
                        /*
                         * migrate tmp to attrs
                         */
                        (*pre).next = (*tmp).next;
                        (*tmp).next = (*cur).attrs;
                        (*cur).attrs = tmp;
                    } else {
                        pre = tmp;
                    }
                }
            }
            /*
             * This may result in a simplification
             */
            if matches!(
                (*cur).typ,
                XmlRelaxNGType::Group | XmlRelaxNGType::Interleave
            ) {
                if (*cur).content.is_null() {
                    (*cur).typ = XmlRelaxNGType::Empty;
                } else if (*(*cur).content).next.is_null() {
                    if parent.is_null() && prev.is_null() {
                        (*cur).typ = XmlRelaxNGType::Noop;
                    } else if prev.is_null() {
                        (*parent).content = (*cur).content;
                        (*(*cur).content).next = (*cur).next;
                        cur = (*cur).content;
                    } else {
                        (*(*cur).content).next = (*cur).next;
                        (*prev).next = (*cur).content;
                        cur = (*cur).content;
                    }
                }
            }
            /*
             * the current node may have been transformed back
             */
            if (*cur).typ == XmlRelaxNGType::Except
                && !(*cur).content.is_null()
                && (*(*cur).content).typ == XmlRelaxNGType::NotAllowed
            {
                prev = xml_relaxng_try_unlink(ctxt, cur, parent, prev);
            } else if (*cur).typ == XmlRelaxNGType::NotAllowed {
                if !parent.is_null()
                    && matches!(
                        (*parent).typ,
                        XmlRelaxNGType::Attribute
                            | XmlRelaxNGType::List
                            | XmlRelaxNGType::Group
                            | XmlRelaxNGType::Interleave
                            | XmlRelaxNGType::Oneormore
                            | XmlRelaxNGType::Zeroormore
                    )
                {
                    (*parent).typ = XmlRelaxNGType::NotAllowed;
                    break;
                }
                if !parent.is_null() && (*parent).typ == XmlRelaxNGType::Choice {
                    prev = xml_relaxng_try_unlink(ctxt, cur, parent, prev);
                } else {
                    prev = cur;
                }
            } else if (*cur).typ == XmlRelaxNGType::Empty {
                if !parent.is_null()
                    && matches!(
                        (*parent).typ,
                        XmlRelaxNGType::Oneormore | XmlRelaxNGType::Zeroormore
                    )
                {
                    (*parent).typ = XmlRelaxNGType::Empty;
                    break;
                }
                if !parent.is_null()
                    && matches!(
                        (*parent).typ,
                        XmlRelaxNGType::Group | XmlRelaxNGType::Interleave | XmlRelaxNGType::Choice
                    )
                {
                    prev = xml_relaxng_try_unlink(ctxt, cur, parent, prev);
                } else {
                    prev = cur;
                }
            } else {
                prev = cur;
            }
        }
        cur = (*cur).next;
    }
}

/**
 * xmlRelaxNGCheckGroupAttrs:
 * @ctxt:  a Relax-NG parser context
 * @def:  the group definition
 *
 * Detects violations of rule 7.3
 */
unsafe extern "C" fn xml_relaxng_check_group_attrs(
    ctxt: XmlRelaxNGParserCtxtPtr,
    def: XmlRelaxNGDefinePtr,
) {
    let mut cur: XmlRelaxNGDefinePtr;
    let mut nbchild: usize = 0;
    let mut i: usize;
    let mut ret: i32;

    if def.is_null() || !matches!((*def).typ, XmlRelaxNGType::Group | XmlRelaxNGType::Element) {
        return;
    }

    if (*def).dflags & IS_PROCESSED as i16 != 0 {
        return;
    }

    /*
     * Don't run that check in case of error. Infinite recursion
     * becomes possible.
     */
    if (*ctxt).nb_errors != 0 {
        return;
    }

    cur = (*def).attrs;
    while !cur.is_null() {
        nbchild += 1;
        cur = (*cur).next;
    }
    cur = (*def).content;
    while !cur.is_null() {
        nbchild += 1;
        cur = (*cur).next;
    }

    let list: *mut *mut XmlRelaxNGDefinePtr =
        xml_malloc(nbchild * size_of::<*mut XmlRelaxNGDefinePtr>()) as _;
    if list.is_null() {
        xml_rng_perr_memory(ctxt, c"building group\n".as_ptr() as _);
        return;
    }
    i = 0;
    cur = (*def).attrs;
    while !cur.is_null() {
        *list.add(i) = xml_relaxng_get_elements(ctxt, cur, 1);
        i += 1;
        cur = (*cur).next;
    }
    cur = (*def).content;
    while !cur.is_null() {
        *list.add(i) = xml_relaxng_get_elements(ctxt, cur, 1);
        i += 1;
        cur = (*cur).next;
    }

    for i in 0..nbchild {
        if (*list.add(i)).is_null() {
            continue;
        }
        for j in 0..i {
            if (*list.add(j)).is_null() {
                continue;
            }
            ret = xml_relaxng_compare_elem_def_lists(ctxt, *list.add(i), *list.add(j));
            if ret == 0 {
                xml_rng_perr(
                    ctxt,
                    (*def).node,
                    XmlParserErrors::XmlRngpGroupAttrConflict,
                    c"Attributes conflicts in group\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
        }
    }
    for i in 0..nbchild {
        if !(*list.add(i)).is_null() {
            xml_free(*list.add(i) as _);
        }
    }

    xml_free(list as _);
    (*def).dflags |= IS_PROCESSED as i16;
}

/**
 * xmlRelaxNGGroupContentType:
 * @ct1:  the first content type
 * @ct2:  the second content type
 *
 * Try to group 2 content types
 *
 * Returns the content type
 */
unsafe extern "C" fn xml_relaxng_group_content_type(
    ct1: XmlRelaxNGContentType,
    ct2: XmlRelaxNGContentType,
) -> XmlRelaxNGContentType {
    if ct1 == XmlRelaxNGContentType::Error || ct2 == XmlRelaxNGContentType::Error {
        return XmlRelaxNGContentType::Error;
    }
    if ct1 == XmlRelaxNGContentType::Empty {
        return ct2;
    }
    if ct2 == XmlRelaxNGContentType::Empty {
        return ct1;
    }
    if ct1 == XmlRelaxNGContentType::Complex && ct2 == XmlRelaxNGContentType::Complex {
        return XmlRelaxNGContentType::Complex;
    }
    XmlRelaxNGContentType::Error
}

/**
 * xmlRelaxNGIsNullable:
 * @define:  the definition to verify
 *
 * Check if a definition is nullable.
 *
 * Returns 1 if yes, 0 if no and -1 in case of error
 */
unsafe extern "C" fn xml_relaxng_is_nullable(define: XmlRelaxNGDefinePtr) -> i32 {
    let mut ret: i32;

    if define.is_null() {
        return -1;
    }

    if (*define).dflags & IS_NULLABLE as i16 != 0 {
        return 1;
    }
    if (*define).dflags & IS_NOT_NULLABLE as i16 != 0 {
        return 0;
    }
    match (*define).typ {
        XmlRelaxNGType::Empty | XmlRelaxNGType::Text => ret = 1,
        XmlRelaxNGType::Noop
        | XmlRelaxNGType::Def
        | XmlRelaxNGType::Ref
        | XmlRelaxNGType::Externalref
        | XmlRelaxNGType::Parentref
        | XmlRelaxNGType::Oneormore => ret = xml_relaxng_is_nullable((*define).content),
        XmlRelaxNGType::Except
        | XmlRelaxNGType::NotAllowed
        | XmlRelaxNGType::Element
        | XmlRelaxNGType::Datatype
        | XmlRelaxNGType::Param
        | XmlRelaxNGType::Value
        | XmlRelaxNGType::List
        | XmlRelaxNGType::Attribute => ret = 0,
        XmlRelaxNGType::Choice => 'done: {
            let mut list: XmlRelaxNGDefinePtr = (*define).content;

            while !list.is_null() {
                ret = xml_relaxng_is_nullable(list);
                if ret != 0 {
                    break 'done;
                }
                list = (*list).next;
            }
            ret = 0;
        }
        XmlRelaxNGType::Start | XmlRelaxNGType::Interleave | XmlRelaxNGType::Group => 'done: {
            let mut list: XmlRelaxNGDefinePtr = (*define).content;

            while !list.is_null() {
                ret = xml_relaxng_is_nullable(list);
                if ret != 1 {
                    break 'done;
                }
                list = (*list).next;
            }
            return 1;
        }
        _ => return -1,
    }
    //   done:
    if ret == 0 {
        (*define).dflags |= IS_NOT_NULLABLE as i16;
    }
    if ret == 1 {
        (*define).dflags |= IS_NULLABLE as i16;
    }
    ret
}

/**
 * xmlRelaxNGCheckChoiceDeterminism:
 * @ctxt:  a Relax-NG parser context
 * @def:  the choice definition
 *
 * Also used to find indeterministic pattern in choice
 */
unsafe extern "C" fn xml_relaxng_check_choice_determinism(
    ctxt: XmlRelaxNGParserCtxtPtr,
    def: XmlRelaxNGDefinePtr,
) {
    let mut nbchild: i32 = 0;
    let mut i: i32;
    let mut ret: i32;
    let mut cur: XmlRelaxNGDefinePtr;
    let mut is_indeterminist: i32 = 0;
    let mut triage: XmlHashTablePtr = null_mut();
    let mut is_triable: i32 = 1;

    if def.is_null() || (*def).typ != XmlRelaxNGType::Choice {
        return;
    }

    if (*def).dflags & IS_PROCESSED as i16 != 0 {
        return;
    }

    /*
     * Don't run that check in case of error. Infinite recursion
     * becomes possible.
     */
    if (*ctxt).nb_errors != 0 {
        return;
    }

    let is_nullable: i32 = xml_relaxng_is_nullable(def);

    cur = (*def).content;
    while !cur.is_null() {
        nbchild += 1;
        cur = (*cur).next;
    }

    let list: *mut *mut XmlRelaxNGDefinePtr =
        xml_malloc(nbchild as usize * size_of::<*mut XmlRelaxNGDefinePtr>()) as _;
    if list.is_null() {
        xml_rng_perr_memory(ctxt, c"building choice\n".as_ptr() as _);
        return;
    }
    i = 0;
    /*
     * a bit strong but safe
     */
    if is_nullable == 0 {
        triage = xml_hash_create(10);
    } else {
        is_triable = 0;
    }
    cur = (*def).content;
    while !cur.is_null() {
        *list.add(i as usize) = xml_relaxng_get_elements(ctxt, cur, 0);
        if (*list.add(i as usize)).is_null() || (*(*list.add(i as usize)).add(0)).is_null() {
            is_triable = 0;
        } else if is_triable == 1 {
            let mut tmp: *mut XmlRelaxNGDefinePtr;
            let mut res: i32;

            tmp = *list.add(i as usize);
            while !(*tmp).is_null() && is_triable == 1 {
                if (*(*tmp)).typ == XmlRelaxNGType::Text {
                    res = xml_hash_add_entry2(triage, c"#text".as_ptr() as _, null_mut(), cur as _);
                    if res != 0 {
                        is_triable = -1;
                    }
                } else if (*(*tmp)).typ == XmlRelaxNGType::Element && !(*(*tmp)).name.is_null() {
                    if (*(*tmp)).ns.is_null() || *(*(*tmp)).ns.add(0) == 0 {
                        res = xml_hash_add_entry2(triage, (*(*tmp)).name, null_mut(), cur as _);
                    } else {
                        res = xml_hash_add_entry2(triage, (*(*tmp)).name, (*(*tmp)).ns, cur as _);
                    }
                    if res != 0 {
                        is_triable = -1;
                    }
                } else if (*(*tmp)).typ == XmlRelaxNGType::Element {
                    if (*(*tmp)).ns.is_null() || *(*(*tmp)).ns.add(0) == 0 {
                        res = xml_hash_add_entry2(
                            triage,
                            c"#any".as_ptr() as _,
                            null_mut(),
                            cur as _,
                        );
                    } else {
                        res = xml_hash_add_entry2(
                            triage,
                            c"#any".as_ptr() as _,
                            (*(*tmp)).ns,
                            cur as _,
                        );
                    }
                    if res != 0 {
                        is_triable = -1;
                    }
                } else {
                    is_triable = -1;
                }
                tmp = tmp.add(1);
            }
        }
        i += 1;
        cur = (*cur).next;
    }

    for i in 0..nbchild {
        if (*list.add(i as usize)).is_null() {
            continue;
        }
        for j in 0..i {
            if (*list.add(j as usize)).is_null() {
                continue;
            }
            ret = xml_relaxng_compare_elem_def_lists(
                ctxt,
                *list.add(i as usize),
                *list.add(j as usize),
            );
            if ret == 0 {
                is_indeterminist = 1;
            }
        }
    }
    for i in 0..nbchild {
        if !(*list.add(i as usize)).is_null() {
            xml_free(*list.add(i as usize) as _);
        }
    }

    xml_free(list as _);
    if is_indeterminist != 0 {
        (*def).dflags |= IS_INDETERMINIST as i16;
    }
    if is_triable == 1 {
        (*def).dflags |= IS_TRIABLE as i16;
        (*def).data = triage as _;
    } else if !triage.is_null() {
        xml_hash_free(triage, None);
    }
    (*def).dflags |= IS_PROCESSED as i16;
}

/**
 * xmlRelaxNGMaxContentType:
 * @ct1:  the first content type
 * @ct2:  the second content type
 *
 * Compute the max content-type
 *
 * Returns the content type
 */
unsafe extern "C" fn xml_relaxng_max_content_type(
    ct1: XmlRelaxNGContentType,
    ct2: XmlRelaxNGContentType,
) -> XmlRelaxNGContentType {
    if ct1 == XmlRelaxNGContentType::Error || ct2 == XmlRelaxNGContentType::Error {
        return XmlRelaxNGContentType::Error;
    }
    if ct1 == XmlRelaxNGContentType::Simple || ct2 == XmlRelaxNGContentType::Simple {
        return XmlRelaxNGContentType::Simple;
    }
    if ct1 == XmlRelaxNGContentType::Complex || ct2 == XmlRelaxNGContentType::Complex {
        return XmlRelaxNGContentType::Complex;
    }
    XmlRelaxNGContentType::Empty
}

/**
 * xmlRelaxNGCheckRules:
 * @ctxt:  a Relax-NG parser context
 * @cur:  the current definition
 * @flags:  some accumulated flags
 * @ptype:  the parent type
 *
 * Check for rules in section 7.1 and 7.2
 *
 * Returns the content type of @cur
 */
unsafe extern "C" fn xml_relaxng_check_rules(
    ctxt: XmlRelaxNGParserCtxtPtr,
    mut cur: XmlRelaxNGDefinePtr,
    flags: i32,
    ptype: XmlRelaxNGType,
) -> XmlRelaxNGContentType {
    let mut nflags: i32;
    let mut ret: XmlRelaxNGContentType;
    let mut tmp: XmlRelaxNGContentType;
    let mut val: XmlRelaxNGContentType = XmlRelaxNGContentType::Empty;

    while !cur.is_null() {
        // ret = XmlRelaxNGContentType::Empty;
        if matches!((*cur).typ, XmlRelaxNGType::Ref | XmlRelaxNGType::Parentref) {
            /*
             * This should actually be caught by list//element(ref) at the
             * element boundaries, c.f. Bug #159968 local refs are dropped
             * in step 4.19.
             */
            if flags & XML_RELAXNG_IN_DATAEXCEPT != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatDataExceptRef,
                    c"Found forbidden pattern data/except//ref\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            if (*cur).content.is_null() {
                if (*cur).typ == XmlRelaxNGType::Parentref {
                    xml_rng_perr(
                        ctxt,
                        (*cur).node,
                        XmlParserErrors::XmlRngpRefNoDef,
                        c"Internal found no define for parent refs\n".as_ptr() as _,
                        null_mut(),
                        null_mut(),
                    );
                } else {
                    xml_rng_perr(
                        ctxt,
                        (*cur).node,
                        XmlParserErrors::XmlRngpRefNoDef,
                        c"Internal found no define for ref %s\n".as_ptr() as _,
                        if !(*cur).name.is_null() {
                            (*cur).name
                        } else {
                            c"null".as_ptr() as _
                        },
                        null_mut(),
                    );
                }
            }
            match (*cur).depth.cmp(&-4) {
                std::cmp::Ordering::Greater => {
                    (*cur).depth = -4;
                    ret = xml_relaxng_check_rules(ctxt, (*cur).content, flags, (*cur).typ);
                    (*cur).depth = ret as i16 - 15;
                }
                std::cmp::Ordering::Equal => {
                    ret = XmlRelaxNGContentType::Complex;
                }
                std::cmp::Ordering::Less => {
                    ret = XmlRelaxNGContentType::try_from((*cur).depth + 15).unwrap();
                }
            }
        } else if (*cur).typ == XmlRelaxNGType::Element {
            /*
             * The 7.3 Attribute derivation rule for groups is plugged there
             */
            xml_relaxng_check_group_attrs(ctxt, cur);
            if flags & XML_RELAXNG_IN_DATAEXCEPT != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatDataExceptElem,
                    c"Found forbidden pattern data/except//element(ref)\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            if flags & XML_RELAXNG_IN_LIST != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatListElem,
                    c"Found forbidden pattern list//element(ref)\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            if flags & XML_RELAXNG_IN_ATTRIBUTE != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatAttrElem,
                    c"Found forbidden pattern attribute//element(ref)\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            if flags & XML_RELAXNG_IN_ATTRIBUTE != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatAttrElem,
                    c"Found forbidden pattern attribute//element(ref)\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            /*
             * reset since in the simple form elements are only child
             * of grammar/define
             */
            nflags = 0;
            ret = xml_relaxng_check_rules(ctxt, (*cur).attrs, nflags, (*cur).typ);
            if ret != XmlRelaxNGContentType::Empty {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpElemContentEmpty,
                    c"Element %s attributes have a content type error\n".as_ptr() as _,
                    (*cur).name,
                    null_mut(),
                );
            }
            ret = xml_relaxng_check_rules(ctxt, (*cur).content, nflags, (*cur).typ);
            if ret == XmlRelaxNGContentType::Error {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpElemContentError,
                    c"Element %s has a content type error\n".as_ptr() as _,
                    (*cur).name,
                    null_mut(),
                );
            } else {
                ret = XmlRelaxNGContentType::Complex;
            }
        } else if (*cur).typ == XmlRelaxNGType::Attribute {
            if flags & XML_RELAXNG_IN_ATTRIBUTE != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatAttrAttr,
                    c"Found forbidden pattern attribute//attribute\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            if flags & XML_RELAXNG_IN_LIST != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatListAttr,
                    c"Found forbidden pattern list//attribute\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            if flags & XML_RELAXNG_IN_OOMGROUP != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatOnemoreGroupAttr,
                    c"Found forbidden pattern oneOrMore//group//attribute\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            if flags & XML_RELAXNG_IN_OOMINTERLEAVE != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatOnemoreInterleaveAttr,
                    c"Found forbidden pattern oneOrMore//interleave//attribute\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            if flags & XML_RELAXNG_IN_DATAEXCEPT != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatDataExceptAttr,
                    c"Found forbidden pattern data/except//attribute\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            if flags & XML_RELAXNG_IN_START != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatStartAttr,
                    c"Found forbidden pattern start//attribute\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            if flags & XML_RELAXNG_IN_ONEORMORE == 0
                && (*cur).name.is_null()
                /* following is checking alternative name class readiness
                   in case it went the "choice" route */
                && (*cur).name_class.is_null()
            {
                if (*cur).ns.is_null() {
                    xml_rng_perr(
                        ctxt,
                        (*cur).node,
                        XmlParserErrors::XmlRngpAnynameAttrAncestor,
                        c"Found anyName attribute without oneOrMore ancestor\n".as_ptr() as _,
                        null_mut(),
                        null_mut(),
                    );
                } else {
                    xml_rng_perr(
                        ctxt,
                        (*cur).node,
                        XmlParserErrors::XmlRngpNsNameAttrAncestor,
                        c"Found nsName attribute without oneOrMore ancestor\n".as_ptr() as _,
                        null_mut(),
                        null_mut(),
                    );
                }
            }
            nflags = flags | XML_RELAXNG_IN_ATTRIBUTE;
            xml_relaxng_check_rules(ctxt, (*cur).content, nflags, (*cur).typ);
            ret = XmlRelaxNGContentType::Empty;
        } else if matches!(
            (*cur).typ,
            XmlRelaxNGType::Oneormore | XmlRelaxNGType::Zeroormore
        ) {
            if flags & XML_RELAXNG_IN_DATAEXCEPT != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatDataExceptOnemore,
                    c"Found forbidden pattern data/except//oneOrMore\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            if flags & XML_RELAXNG_IN_START != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatStartOnemore,
                    c"Found forbidden pattern start//oneOrMore\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            nflags = flags | XML_RELAXNG_IN_ONEORMORE;
            ret = xml_relaxng_check_rules(ctxt, (*cur).content, nflags, (*cur).typ);
            ret = xml_relaxng_group_content_type(ret, ret);
        } else if (*cur).typ == XmlRelaxNGType::List {
            if flags & XML_RELAXNG_IN_LIST != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatListList,
                    c"Found forbidden pattern list//list\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            if flags & XML_RELAXNG_IN_DATAEXCEPT != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatDataExceptList,
                    c"Found forbidden pattern data/except//list\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            if flags & XML_RELAXNG_IN_START != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatStartList,
                    c"Found forbidden pattern start//list\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            nflags = flags | XML_RELAXNG_IN_LIST;
            ret = xml_relaxng_check_rules(ctxt, (*cur).content, nflags, (*cur).typ);
        } else if (*cur).typ == XmlRelaxNGType::Group {
            if flags & XML_RELAXNG_IN_DATAEXCEPT != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatDataExceptGroup,
                    c"Found forbidden pattern data/except//group\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            if flags & XML_RELAXNG_IN_START != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatStartGroup,
                    c"Found forbidden pattern start//group\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            if flags & XML_RELAXNG_IN_ONEORMORE != 0 {
                nflags = flags | XML_RELAXNG_IN_OOMGROUP;
            } else {
                nflags = flags;
            }
            ret = xml_relaxng_check_rules(ctxt, (*cur).content, nflags, (*cur).typ);
            /*
             * The 7.3 Attribute derivation rule for groups is plugged there
             */
            xml_relaxng_check_group_attrs(ctxt, cur);
        } else if (*cur).typ == XmlRelaxNGType::Interleave {
            if flags & XML_RELAXNG_IN_LIST != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatListInterleave,
                    c"Found forbidden pattern list//interleave\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            if flags & XML_RELAXNG_IN_DATAEXCEPT != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatDataExceptInterleave,
                    c"Found forbidden pattern data/except//interleave\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            if flags & XML_RELAXNG_IN_START != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatDataExceptInterleave,
                    c"Found forbidden pattern start//interleave\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            if flags & XML_RELAXNG_IN_ONEORMORE != 0 {
                nflags = flags | XML_RELAXNG_IN_OOMINTERLEAVE;
            } else {
                nflags = flags;
            }
            ret = xml_relaxng_check_rules(ctxt, (*cur).content, nflags, (*cur).typ);
        } else if (*cur).typ == XmlRelaxNGType::Except {
            if !(*cur).parent.is_null() && (*(*cur).parent).typ == XmlRelaxNGType::Datatype {
                nflags = flags | XML_RELAXNG_IN_DATAEXCEPT;
            } else {
                nflags = flags;
            }
            ret = xml_relaxng_check_rules(ctxt, (*cur).content, nflags, (*cur).typ);
        } else if (*cur).typ == XmlRelaxNGType::Datatype {
            if flags & XML_RELAXNG_IN_START != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatStartData,
                    c"Found forbidden pattern start//data\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            xml_relaxng_check_rules(ctxt, (*cur).content, flags, (*cur).typ);
            ret = XmlRelaxNGContentType::Simple;
        } else if (*cur).typ == XmlRelaxNGType::Value {
            if flags & XML_RELAXNG_IN_START != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatStartValue,
                    c"Found forbidden pattern start//value\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            xml_relaxng_check_rules(ctxt, (*cur).content, flags, (*cur).typ);
            ret = XmlRelaxNGContentType::Simple;
        } else if (*cur).typ == XmlRelaxNGType::Text {
            if flags & XML_RELAXNG_IN_LIST != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatListText,
                    c"Found forbidden pattern list//text\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            if flags & XML_RELAXNG_IN_DATAEXCEPT != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatDataExceptText,
                    c"Found forbidden pattern data/except//text\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            if flags & XML_RELAXNG_IN_START != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatStartText,
                    c"Found forbidden pattern start//text\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            ret = XmlRelaxNGContentType::Complex;
        } else if (*cur).typ == XmlRelaxNGType::Empty {
            if flags & XML_RELAXNG_IN_DATAEXCEPT != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatDataExceptEmpty,
                    c"Found forbidden pattern data/except//empty\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            if flags & XML_RELAXNG_IN_START != 0 {
                xml_rng_perr(
                    ctxt,
                    (*cur).node,
                    XmlParserErrors::XmlRngpPatStartEmpty,
                    c"Found forbidden pattern start//empty\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                );
            }
            ret = XmlRelaxNGContentType::Empty;
        } else if (*cur).typ == XmlRelaxNGType::Choice {
            xml_relaxng_check_choice_determinism(ctxt, cur);
            ret = xml_relaxng_check_rules(ctxt, (*cur).content, flags, (*cur).typ);
        } else {
            ret = xml_relaxng_check_rules(ctxt, (*cur).content, flags, (*cur).typ);
        }
        cur = (*cur).next;
        if ptype == XmlRelaxNGType::Group {
            val = xml_relaxng_group_content_type(val, ret);
        } else if ptype == XmlRelaxNGType::Interleave {
            /*
             * TODO: scan complain that tmp is never used, seems on purpose
             *       need double-checking
             */
            tmp = xml_relaxng_group_content_type(val, ret);
            if tmp != XmlRelaxNGContentType::Error {
                // tmp =
                xml_relaxng_max_content_type(val, ret);
            }
        } else if ptype == XmlRelaxNGType::Choice {
            val = xml_relaxng_max_content_type(val, ret);
        } else if ptype == XmlRelaxNGType::List {
            val = XmlRelaxNGContentType::Simple;
        } else if ptype == XmlRelaxNGType::Except {
            if ret == XmlRelaxNGContentType::Error {
                val = XmlRelaxNGContentType::Error;
            } else {
                val = XmlRelaxNGContentType::Simple;
            }
        } else {
            val = xml_relaxng_group_content_type(val, ret);
        }
    }
    val
}

/**
 * xmlRelaxNGParseDocument:
 * @ctxt:  a Relax-NG parser context
 * @node:  the root node of the RelaxNG schema
 *
 * parse a Relax-NG definition resource and build an internal
 * xmlRelaxNG structure which can be used to validate instances.
 *
 * Returns the internal XML RelaxNG structure built or
 *         NULL in case of error
 */
unsafe extern "C" fn xml_relaxng_parse_document(
    ctxt: XmlRelaxNGParserCtxtPtr,
    node: XmlNodePtr,
) -> XmlRelaxNGPtr {
    let old: XmlRelaxNGGrammarPtr;

    if ctxt.is_null() || node.is_null() {
        return null_mut();
    }

    let schema: XmlRelaxNGPtr = xml_relaxng_new_relaxng(ctxt);
    if schema.is_null() {
        return null_mut();
    }

    let olddefine: *const XmlChar = (*ctxt).define;
    (*ctxt).define = null_mut();
    if IS_RELAXNG!(node, c"grammar".as_ptr() as _) {
        (*schema).topgrammar = xml_relaxng_parse_grammar(ctxt, (*node).children);
        if (*schema).topgrammar.is_null() {
            xml_relaxng_free(schema);
            return null_mut();
        }
    } else {
        let mut tmp: XmlRelaxNGGrammarPtr;

        (*schema).topgrammar = xml_relaxng_new_grammar(ctxt);
        let ret: XmlRelaxNGGrammarPtr = (*schema).topgrammar;
        if (*schema).topgrammar.is_null() {
            xml_relaxng_free(schema);
            return null_mut();
        }
        /*
         * Link the new grammar in the tree
         */
        (*ret).parent = (*ctxt).grammar;
        if !(*ctxt).grammar.is_null() {
            tmp = (*(*ctxt).grammar).children;
            if tmp.is_null() {
                (*(*ctxt).grammar).children = ret;
            } else {
                while !(*tmp).next.is_null() {
                    tmp = (*tmp).next;
                }
                (*tmp).next = ret;
            }
        }
        old = (*ctxt).grammar;
        (*ctxt).grammar = ret;
        xml_relaxng_parse_start(ctxt, node);
        if !old.is_null() {
            (*ctxt).grammar = old;
        }
    }
    (*ctxt).define = olddefine;
    if !(*(*schema).topgrammar).start.is_null() {
        xml_relaxng_check_cycles(ctxt, (*(*schema).topgrammar).start, 0);
        if (*ctxt).flags & XML_RELAXNG_IN_EXTERNALREF == 0 {
            xml_relaxng_simplify(ctxt, (*(*schema).topgrammar).start, null_mut());
            while !(*(*schema).topgrammar).start.is_null()
                && (*(*(*schema).topgrammar).start).typ == XmlRelaxNGType::Noop
                && !(*(*(*schema).topgrammar).start).next.is_null()
            {
                (*(*schema).topgrammar).start = (*(*(*schema).topgrammar).start).content;
            }
            xml_relaxng_check_rules(
                ctxt,
                (*(*schema).topgrammar).start,
                XML_RELAXNG_IN_START,
                XmlRelaxNGType::Noop,
            );
        }
    }

    schema
}

/**
 * xmlRelaxNGIsCompilable:
 * @define:  the definition to check
 *
 * Check if a definition is nullable.
 *
 * Returns 1 if yes, 0 if no and -1 in case of error
 */
unsafe extern "C" fn xml_relaxng_is_compilable(def: XmlRelaxNGDefinePtr) -> i32 {
    let mut ret: i32 = -1;

    if def.is_null() {
        return -1;
    }
    if (*def).typ != XmlRelaxNGType::Element && (*def).dflags & IS_COMPILABLE as i16 != 0 {
        return 1;
    }
    if (*def).typ != XmlRelaxNGType::Element && (*def).dflags & IS_NOT_COMPILABLE as i16 != 0 {
        return 0;
    }
    match (*def).typ {
        XmlRelaxNGType::Noop => ret = xml_relaxng_is_compilable((*def).content),
        XmlRelaxNGType::Text | XmlRelaxNGType::Empty => ret = 1,
        XmlRelaxNGType::Element => {
            /*
             * Check if the element content is compilable
             */
            if (*def).dflags & IS_NOT_COMPILABLE as i16 == 0
                && (*def).dflags & IS_COMPILABLE as i16 == 0
            {
                let mut list: XmlRelaxNGDefinePtr;

                list = (*def).content;
                while !list.is_null() {
                    ret = xml_relaxng_is_compilable(list);
                    if ret != 1 {
                        break;
                    }
                    list = (*list).next;
                }
                /*
                 * Because the routine is recursive, we must guard against
                 * discovering both COMPILABLE and NOT_COMPILABLE
                 */
                if ret == 0 {
                    (*def).dflags &= !IS_COMPILABLE as i16;
                    (*def).dflags |= IS_NOT_COMPILABLE as i16;
                }
                if ret == 1 && {
                    (*def).dflags &= IS_NOT_COMPILABLE as i16;
                    (*def).dflags == 0
                } {
                    (*def).dflags |= IS_COMPILABLE as i16;
                }
            }
            /*
             * All elements return a compilable status unless they
             * are generic like anyName
             */
            if !(*def).name_class.is_null() || (*def).name.is_null() {
                ret = 0;
            } else {
                ret = 1;
            }
            return ret;
        }
        XmlRelaxNGType::Ref | XmlRelaxNGType::Externalref | XmlRelaxNGType::Parentref => {
            if (*def).depth == -20 {
                return 1;
            } else {
                let mut list: XmlRelaxNGDefinePtr;

                (*def).depth = -20;
                list = (*def).content;
                while !list.is_null() {
                    ret = xml_relaxng_is_compilable(list);
                    if ret != 1 {
                        break;
                    }
                    list = (*list).next;
                }
            }
        }
        XmlRelaxNGType::Start
        | XmlRelaxNGType::Optional
        | XmlRelaxNGType::Zeroormore
        | XmlRelaxNGType::Oneormore
        | XmlRelaxNGType::Choice
        | XmlRelaxNGType::Group
        | XmlRelaxNGType::Def => {
            let mut list: XmlRelaxNGDefinePtr;

            list = (*def).content;
            while !list.is_null() {
                ret = xml_relaxng_is_compilable(list);
                if ret != 1 {
                    break;
                }
                list = (*list).next;
            }
        }
        XmlRelaxNGType::Except
        | XmlRelaxNGType::Attribute
        | XmlRelaxNGType::Interleave
        | XmlRelaxNGType::Datatype
        | XmlRelaxNGType::List
        | XmlRelaxNGType::Param
        | XmlRelaxNGType::Value
        | XmlRelaxNGType::NotAllowed => ret = 0,
    }
    if ret == 0 {
        (*def).dflags |= IS_NOT_COMPILABLE as i16;
    }
    if ret == 1 {
        (*def).dflags |= IS_COMPILABLE as i16;
    }
    ret
}

unsafe extern "C" fn xml_relaxng_def_name(def: XmlRelaxNGDefinePtr) -> *const c_char {
    if def.is_null() {
        return c"none".as_ptr() as _;
    }
    match (*def).typ {
        XmlRelaxNGType::Empty => c"empty".as_ptr() as _,
        XmlRelaxNGType::NotAllowed => c"notAllowed".as_ptr() as _,
        XmlRelaxNGType::Except => c"except".as_ptr() as _,
        XmlRelaxNGType::Text => c"text".as_ptr() as _,
        XmlRelaxNGType::Element => c"element".as_ptr() as _,
        XmlRelaxNGType::Datatype => c"datatype".as_ptr() as _,
        XmlRelaxNGType::Value => c"value".as_ptr() as _,
        XmlRelaxNGType::List => c"list".as_ptr() as _,
        XmlRelaxNGType::Attribute => c"attribute".as_ptr() as _,
        XmlRelaxNGType::Def => c"def".as_ptr() as _,
        XmlRelaxNGType::Ref => c"ref".as_ptr() as _,
        XmlRelaxNGType::Externalref => c"externalRef".as_ptr() as _,
        XmlRelaxNGType::Parentref => c"parentRef".as_ptr() as _,
        XmlRelaxNGType::Optional => c"optional".as_ptr() as _,
        XmlRelaxNGType::Zeroormore => c"zeroOrMore".as_ptr() as _,
        XmlRelaxNGType::Oneormore => c"oneOrMore".as_ptr() as _,
        XmlRelaxNGType::Choice => c"choice".as_ptr() as _,
        XmlRelaxNGType::Group => c"group".as_ptr() as _,
        XmlRelaxNGType::Interleave => c"interleave".as_ptr() as _,
        XmlRelaxNGType::Start => c"start".as_ptr() as _,
        XmlRelaxNGType::Noop => c"noop".as_ptr() as _,
        XmlRelaxNGType::Param => c"param".as_ptr() as _,
    }
    // c"unknown".as_ptr() as _
}

/**
 * xmlRelaxNGCompile:
 * ctxt:  the RelaxNG parser context
 * @define:  the definition tree to compile
 *
 * Compile the set of definitions, it works recursively, till the
 * element boundaries, where it tries to compile the content if possible
 *
 * Returns 0 if success and -1 in case of error
 */
unsafe extern "C" fn xml_relaxng_compile(
    ctxt: XmlRelaxNGParserCtxtPtr,
    def: XmlRelaxNGDefinePtr,
) -> i32 {
    let mut ret: i32 = 0;
    let mut list: XmlRelaxNGDefinePtr;

    if ctxt.is_null() || def.is_null() {
        return -1;
    }

    match (*def).typ {
        XmlRelaxNGType::Start => {
            if xml_relaxng_is_compilable(def) == 1 && (*def).depth != -25 {
                let oldam: XmlAutomataPtr = (*ctxt).am;
                let oldstate: XmlAutomataStatePtr = (*ctxt).state;

                (*def).depth = -25;

                list = (*def).content;
                (*ctxt).am = xml_new_automata();
                if (*ctxt).am.is_null() {
                    return -1;
                }

                /*
                 * assume identical strings but not same pointer are different
                 * atoms, needed for non-determinism detection
                 * That way if 2 elements with the same name are in a choice
                 * branch the automata is found non-deterministic and
                 * we fallback to the normal validation which does the right
                 * thing of exploring both choices.
                 */
                xml_automata_set_flags((*ctxt).am, 1);

                (*ctxt).state = xml_automata_get_init_state((*ctxt).am);
                while !list.is_null() {
                    xml_relaxng_compile(ctxt, list);
                    list = (*list).next;
                }
                xml_automata_set_final_state((*ctxt).am, (*ctxt).state);
                if xml_automata_is_determinist((*ctxt).am) != 0 {
                    (*def).cont_model = xml_automata_compile((*ctxt).am);
                }

                xml_free_automata((*ctxt).am);
                (*ctxt).state = oldstate;
                (*ctxt).am = oldam;
            }
        }
        XmlRelaxNGType::Element => {
            if !(*ctxt).am.is_null() && !(*def).name.is_null() {
                (*ctxt).state = xml_automata_new_transition2(
                    (*ctxt).am,
                    (*ctxt).state,
                    null_mut(),
                    (*def).name,
                    (*def).ns,
                    def as _,
                );
            }
            if (*def).dflags & IS_COMPILABLE as i16 != 0 && (*def).depth != -25 {
                let oldam: XmlAutomataPtr = (*ctxt).am;
                let oldstate: XmlAutomataStatePtr = (*ctxt).state;

                (*def).depth = -25;

                list = (*def).content;
                (*ctxt).am = xml_new_automata();
                if (*ctxt).am.is_null() {
                    return -1;
                }
                xml_automata_set_flags((*ctxt).am, 1);
                (*ctxt).state = xml_automata_get_init_state((*ctxt).am);
                while !list.is_null() {
                    xml_relaxng_compile(ctxt, list);
                    list = (*list).next;
                }
                xml_automata_set_final_state((*ctxt).am, (*ctxt).state);
                (*def).cont_model = xml_automata_compile((*ctxt).am);
                if xml_regexp_is_determinist((*def).cont_model) == 0 {
                    /*
                     * we can only use the automata if it is determinist
                     */
                    xml_reg_free_regexp((*def).cont_model);
                    (*def).cont_model = null_mut();
                }
                xml_free_automata((*ctxt).am);
                (*ctxt).state = oldstate;
                (*ctxt).am = oldam;
            } else {
                let oldam: XmlAutomataPtr = (*ctxt).am;

                /*
                 * we can't build the content model for this element content
                 * but it still might be possible to build it for some of its
                 * children, recurse.
                 */
                ret = xml_relaxng_try_compile(ctxt, def);
                (*ctxt).am = oldam;
            }
        }
        XmlRelaxNGType::Noop => ret = xml_relaxng_compile(ctxt, (*def).content),
        XmlRelaxNGType::Optional => {
            let oldstate: XmlAutomataStatePtr = (*ctxt).state;

            list = (*def).content;
            while !list.is_null() {
                xml_relaxng_compile(ctxt, list);
                list = (*list).next;
            }
            xml_automata_new_epsilon((*ctxt).am, oldstate, (*ctxt).state);
        }
        XmlRelaxNGType::Zeroormore => {
            (*ctxt).state = xml_automata_new_epsilon((*ctxt).am, (*ctxt).state, null_mut());
            let oldstate: XmlAutomataStatePtr = (*ctxt).state;
            list = (*def).content;
            while !list.is_null() {
                xml_relaxng_compile(ctxt, list);
                list = (*list).next;
            }
            xml_automata_new_epsilon((*ctxt).am, (*ctxt).state, oldstate);
            (*ctxt).state = xml_automata_new_epsilon((*ctxt).am, oldstate, null_mut());
        }
        XmlRelaxNGType::Oneormore => {
            list = (*def).content;
            while !list.is_null() {
                xml_relaxng_compile(ctxt, list);
                list = (*list).next;
            }
            let oldstate: XmlAutomataStatePtr = (*ctxt).state;
            list = (*def).content;
            while !list.is_null() {
                xml_relaxng_compile(ctxt, list);
                list = (*list).next;
            }
            xml_automata_new_epsilon((*ctxt).am, (*ctxt).state, oldstate);
            (*ctxt).state = xml_automata_new_epsilon((*ctxt).am, oldstate, null_mut());
        }
        XmlRelaxNGType::Choice => {
            let mut target: XmlAutomataStatePtr = null_mut();
            let oldstate: XmlAutomataStatePtr = (*ctxt).state;

            list = (*def).content;
            while !list.is_null() {
                (*ctxt).state = oldstate;
                ret = xml_relaxng_compile(ctxt, list);
                if ret != 0 {
                    break;
                }
                if target.is_null() {
                    target = (*ctxt).state;
                } else {
                    xml_automata_new_epsilon((*ctxt).am, (*ctxt).state, target);
                }
                list = (*list).next;
            }
            (*ctxt).state = target;
        }
        XmlRelaxNGType::Ref
        | XmlRelaxNGType::Externalref
        | XmlRelaxNGType::Parentref
        | XmlRelaxNGType::Group
        | XmlRelaxNGType::Def => {
            list = (*def).content;
            while !list.is_null() {
                ret = xml_relaxng_compile(ctxt, list);
                if ret != 0 {
                    break;
                }
                list = (*list).next;
            }
        }
        XmlRelaxNGType::Text => {
            (*ctxt).state = xml_automata_new_epsilon((*ctxt).am, (*ctxt).state, null_mut());
            let oldstate: XmlAutomataStatePtr = (*ctxt).state;
            xml_relaxng_compile(ctxt, (*def).content);
            xml_automata_new_transition(
                (*ctxt).am,
                (*ctxt).state,
                (*ctxt).state,
                c"#text".as_ptr() as _,
                null_mut(),
            );
            (*ctxt).state = xml_automata_new_epsilon((*ctxt).am, oldstate, null_mut());
        }
        XmlRelaxNGType::Empty => {
            (*ctxt).state = xml_automata_new_epsilon((*ctxt).am, (*ctxt).state, null_mut())
        }
        XmlRelaxNGType::Except
        | XmlRelaxNGType::Attribute
        | XmlRelaxNGType::Interleave
        | XmlRelaxNGType::NotAllowed
        | XmlRelaxNGType::Datatype
        | XmlRelaxNGType::List
        | XmlRelaxNGType::Param
        | XmlRelaxNGType::Value => {
            /* This should not happen and generate an internal error */
            eprintln!(
                "RNG internal error trying to compile {}",
                CStr::from_ptr(xml_relaxng_def_name(def))
                    .to_string_lossy()
                    .as_ref()
            );
        }
    }
    ret
}

/**
 * xmlRelaxNGTryCompile:
 * ctxt:  the RelaxNG parser context
 * @define:  the definition tree to compile
 *
 * Try to compile the set of definitions, it works recursively,
 * possibly ignoring parts which cannot be compiled.
 *
 * Returns 0 if success and -1 in case of error
 */
unsafe extern "C" fn xml_relaxng_try_compile(
    ctxt: XmlRelaxNGParserCtxtPtr,
    def: XmlRelaxNGDefinePtr,
) -> i32 {
    let mut ret: i32 = 0;
    let mut list: XmlRelaxNGDefinePtr;

    if ctxt.is_null() || def.is_null() {
        return -1;
    }

    if matches!((*def).typ, XmlRelaxNGType::Start | XmlRelaxNGType::Element) {
        ret = xml_relaxng_is_compilable(def);
        if (*def).dflags & IS_COMPILABLE as i16 != 0 && (*def).depth != -25 {
            (*ctxt).am = null_mut();
            ret = xml_relaxng_compile(ctxt, def);
            return ret;
        }
    }
    match (*def).typ {
        XmlRelaxNGType::Noop => ret = xml_relaxng_try_compile(ctxt, (*def).content),
        XmlRelaxNGType::Text
        | XmlRelaxNGType::Datatype
        | XmlRelaxNGType::List
        | XmlRelaxNGType::Param
        | XmlRelaxNGType::Value
        | XmlRelaxNGType::Empty
        | XmlRelaxNGType::Element => ret = 0,
        XmlRelaxNGType::Optional
        | XmlRelaxNGType::Zeroormore
        | XmlRelaxNGType::Oneormore
        | XmlRelaxNGType::Choice
        | XmlRelaxNGType::Group
        | XmlRelaxNGType::Def
        | XmlRelaxNGType::Start
        | XmlRelaxNGType::Ref
        | XmlRelaxNGType::Externalref
        | XmlRelaxNGType::Parentref => {
            list = (*def).content;
            while !list.is_null() {
                ret = xml_relaxng_try_compile(ctxt, list);
                if ret != 0 {
                    break;
                }
                list = (*list).next;
            }
        }
        XmlRelaxNGType::Except
        | XmlRelaxNGType::Attribute
        | XmlRelaxNGType::Interleave
        | XmlRelaxNGType::NotAllowed => ret = 0,
    }
    ret
}

/**
 * xmlRelaxNGParse:
 * @ctxt:  a Relax-NG parser context
 *
 * parse a schema definition resource and build an internal
 * XML Schema structure which can be used to validate instances.
 *
 * Returns the internal XML RelaxNG structure built from the resource or
 *         NULL in case of error
 */
pub unsafe extern "C" fn xml_relaxng_parse(ctxt: XmlRelaxNGParserCtxtPtr) -> XmlRelaxNGPtr {
    let mut doc: XmlDocPtr;

    xml_relaxng_init_types();

    if ctxt.is_null() {
        return null_mut();
    }

    /*
     * First step is to parse the input document into an DOM/Infoset
     */
    if !(*ctxt).url.is_null() {
        doc = xml_read_file((*ctxt).url as _, None, 0);
        if doc.is_null() {
            xml_rng_perr(
                ctxt,
                null_mut(),
                XmlParserErrors::XmlRngpParseError,
                c"xmlRelaxNGParse: could not load %s\n".as_ptr() as _,
                (*ctxt).url,
                null_mut(),
            );
            return null_mut();
        }
    } else if !(*ctxt).buffer.is_null() {
        let mem = from_raw_parts((*ctxt).buffer as *const u8, (*ctxt).size as usize).to_vec();
        doc = xml_read_memory(mem, None, None, 0);
        if doc.is_null() {
            xml_rng_perr(
                ctxt,
                null_mut(),
                XmlParserErrors::XmlRngpParseError,
                c"xmlRelaxNGParse: could not parse schemas\n".as_ptr() as _,
                null_mut(),
                null_mut(),
            );
            return null_mut();
        }
        (*doc).url = Some("in_memory_buffer".to_owned());
        (*ctxt).url = xml_strdup(c"in_memory_buffer".as_ptr() as _);
    } else if !(*ctxt).document.is_null() {
        doc = (*ctxt).document;
    } else {
        xml_rng_perr(
            ctxt,
            null_mut(),
            XmlParserErrors::XmlRngpEmpty,
            c"xmlRelaxNGParse: nothing to parse\n".as_ptr() as _,
            null_mut(),
            null_mut(),
        );
        return null_mut();
    }
    (*ctxt).document = doc;

    /*
     * Some preprocessing of the document content
     */
    doc = xml_relaxng_cleanup_doc(ctxt, doc);
    if doc.is_null() {
        xml_free_doc((*ctxt).document);
        (*ctxt).document = null_mut();
        return null_mut();
    }

    /*
     * Then do the parsing for good
     */
    let root: XmlNodePtr = (*doc).get_root_element();
    if root.is_null() {
        xml_rng_perr(
            ctxt,
            doc as _,
            XmlParserErrors::XmlRngpEmpty,
            c"xmlRelaxNGParse: %s is empty\n".as_ptr() as _,
            if !(*ctxt).url.is_null() {
                (*ctxt).url
            } else {
                c"schemas".as_ptr() as _
            },
            null_mut(),
        );

        xml_free_doc((*ctxt).document);
        (*ctxt).document = null_mut();
        return null_mut();
    }
    let ret: XmlRelaxNGPtr = xml_relaxng_parse_document(ctxt, root);
    if ret.is_null() {
        xml_free_doc((*ctxt).document);
        (*ctxt).document = null_mut();
        return null_mut();
    }

    /*
     * Check the ref/defines links
     */
    /*
     * try to preprocess interleaves
     */
    if !(*ctxt).interleaves.is_null() {
        xml_hash_scan(
            (*ctxt).interleaves,
            Some(xml_relaxng_compute_interleaves),
            ctxt as _,
        );
    }

    /*
     * if there was a parsing error return NULL
     */
    if (*ctxt).nb_errors > 0 {
        xml_relaxng_free(ret);
        (*ctxt).document = null_mut();
        xml_free_doc(doc);
        return null_mut();
    }

    /*
     * try to compile (parts of) the schemas
     */
    if !(*ret).topgrammar.is_null() && !(*(*ret).topgrammar).start.is_null() {
        if (*(*(*ret).topgrammar).start).typ != XmlRelaxNGType::Start {
            let def: XmlRelaxNGDefinePtr = xml_relaxng_new_define(ctxt, null_mut());
            if !def.is_null() {
                (*def).typ = XmlRelaxNGType::Start;
                (*def).content = (*(*ret).topgrammar).start;
                (*(*ret).topgrammar).start = def;
            }
        }
        xml_relaxng_try_compile(ctxt, (*(*ret).topgrammar).start);
    }

    /*
     * Transfer the pointer for cleanup at the schema level.
     */
    (*ret).doc = doc;
    (*ctxt).document = null_mut();
    (*ret).documents = (*ctxt).documents;
    (*ctxt).documents = null_mut();

    (*ret).includes = (*ctxt).includes;
    (*ctxt).includes = null_mut();
    (*ret).def_nr = (*ctxt).def_nr;
    (*ret).def_tab = (*ctxt).def_tab;
    (*ctxt).def_tab = null_mut();
    if (*ctxt).idref == 1 {
        (*ret).idref = 1;
    }

    ret
}

/**
 * xmlRelaxNGFreeGrammar:
 * @grammar:  a grammar structure
 *
 * Deallocate a RelaxNG grammar structure.
 */
unsafe extern "C" fn xml_relaxng_free_grammar(grammar: XmlRelaxNGGrammarPtr) {
    if grammar.is_null() {
        return;
    }

    if !(*grammar).children.is_null() {
        xml_relaxng_free_grammar((*grammar).children);
    }
    if !(*grammar).next.is_null() {
        xml_relaxng_free_grammar((*grammar).next);
    }
    if !(*grammar).refs.is_null() {
        xml_hash_free((*grammar).refs, None);
    }
    if !(*grammar).defs.is_null() {
        xml_hash_free((*grammar).defs, None);
    }

    xml_free(grammar as _);
}

/**
 * xmlRelaxNGFree:
 * @schema:  a schema structure
 *
 * Deallocate a RelaxNG structure.
 */
pub unsafe extern "C" fn xml_relaxng_free(schema: XmlRelaxNGPtr) {
    if schema.is_null() {
        return;
    }

    if !(*schema).topgrammar.is_null() {
        xml_relaxng_free_grammar((*schema).topgrammar);
    }
    if !(*schema).doc.is_null() {
        xml_free_doc((*schema).doc);
    }
    if !(*schema).documents.is_null() {
        xml_relaxng_free_document_list((*schema).documents);
    }
    if !(*schema).includes.is_null() {
        xml_relaxng_free_include_list((*schema).includes);
    }
    if !(*schema).def_tab.is_null() {
        for i in 0..(*schema).def_nr {
            xml_relaxng_free_define(*(*schema).def_tab.add(i as usize));
        }
        xml_free((*schema).def_tab as _);
    }

    xml_free(schema as _);
}

/**
 * xmlRelaxNGDumpDefines:
 * @output:  the file output
 * @defines:  a list of define structures
 *
 * Dump a RelaxNG structure back
 */
#[cfg(feature = "output")]
unsafe extern "C" fn xml_relaxng_dump_defines(output: *mut FILE, mut defines: XmlRelaxNGDefinePtr) {
    while !defines.is_null() {
        xml_relaxng_dump_define(output, defines);
        defines = (*defines).next;
    }
}

/**
 * xmlRelaxNGDumpDefine:
 * @output:  the file output
 * @define:  a define structure
 *
 * Dump a RelaxNG structure back
 */
#[cfg(feature = "output")]
unsafe extern "C" fn xml_relaxng_dump_define(output: *mut FILE, define: XmlRelaxNGDefinePtr) {
    if define.is_null() {
        return;
    }
    match (*define).typ {
        XmlRelaxNGType::Empty => {
            fprintf(output, c"<empty/>\n".as_ptr() as _);
        }
        XmlRelaxNGType::NotAllowed => {
            fprintf(output, c"<notAllowed/>\n".as_ptr() as _);
        }
        XmlRelaxNGType::Text => {
            fprintf(output, c"<text/>\n".as_ptr() as _);
        }
        XmlRelaxNGType::Element => {
            fprintf(output, c"<element>\n".as_ptr() as _);
            if !(*define).name.is_null() {
                fprintf(output, c"<name".as_ptr() as _);
                if !(*define).ns.is_null() {
                    fprintf(output, c" ns=\"%s\"".as_ptr() as _, (*define).ns);
                }
                fprintf(output, c">%s</name>\n".as_ptr() as _, (*define).name);
            }
            xml_relaxng_dump_defines(output, (*define).attrs);
            xml_relaxng_dump_defines(output, (*define).content);
            fprintf(output, c"</element>\n".as_ptr() as _);
        }
        XmlRelaxNGType::List => {
            fprintf(output, c"<list>\n".as_ptr() as _);
            xml_relaxng_dump_defines(output, (*define).content);
            fprintf(output, c"</list>\n".as_ptr() as _);
        }
        XmlRelaxNGType::Oneormore => {
            fprintf(output, c"<oneOrMore>\n".as_ptr() as _);
            xml_relaxng_dump_defines(output, (*define).content);
            fprintf(output, c"</oneOrMore>\n".as_ptr() as _);
        }
        XmlRelaxNGType::Zeroormore => {
            fprintf(output, c"<zeroOrMore>\n".as_ptr() as _);
            xml_relaxng_dump_defines(output, (*define).content);
            fprintf(output, c"</zeroOrMore>\n".as_ptr() as _);
        }
        XmlRelaxNGType::Choice => {
            fprintf(output, c"<choice>\n".as_ptr() as _);
            xml_relaxng_dump_defines(output, (*define).content);
            fprintf(output, c"</choice>\n".as_ptr() as _);
        }
        XmlRelaxNGType::Group => {
            fprintf(output, c"<group>\n".as_ptr() as _);
            xml_relaxng_dump_defines(output, (*define).content);
            fprintf(output, c"</group>\n".as_ptr() as _);
        }
        XmlRelaxNGType::Interleave => {
            fprintf(output, c"<interleave>\n".as_ptr() as _);
            xml_relaxng_dump_defines(output, (*define).content);
            fprintf(output, c"</interleave>\n".as_ptr() as _);
        }
        XmlRelaxNGType::Optional => {
            fprintf(output, c"<optional>\n".as_ptr() as _);
            xml_relaxng_dump_defines(output, (*define).content);
            fprintf(output, c"</optional>\n".as_ptr() as _);
        }
        XmlRelaxNGType::Attribute => {
            fprintf(output, c"<attribute>\n".as_ptr() as _);
            xml_relaxng_dump_defines(output, (*define).content);
            fprintf(output, c"</attribute>\n".as_ptr() as _);
        }
        XmlRelaxNGType::Def => {
            fprintf(output, c"<define".as_ptr() as _);
            if !(*define).name.is_null() {
                fprintf(output, c" name=\"%s\"".as_ptr() as _, (*define).name);
            }
            fprintf(output, c">\n".as_ptr() as _);
            xml_relaxng_dump_defines(output, (*define).content);
            fprintf(output, c"</define>\n".as_ptr() as _);
        }
        XmlRelaxNGType::Ref => {
            fprintf(output, c"<ref".as_ptr() as _);
            if !(*define).name.is_null() {
                fprintf(output, c" name=\"%s\"".as_ptr() as _, (*define).name);
            }
            fprintf(output, c">\n".as_ptr() as _);
            xml_relaxng_dump_defines(output, (*define).content);
            fprintf(output, c"</ref>\n".as_ptr() as _);
        }
        XmlRelaxNGType::Parentref => {
            fprintf(output, c"<parentRef".as_ptr() as _);
            if !(*define).name.is_null() {
                fprintf(output, c" name=\"%s\"".as_ptr() as _, (*define).name);
            }
            fprintf(output, c">\n".as_ptr() as _);
            xml_relaxng_dump_defines(output, (*define).content);
            fprintf(output, c"</parentRef>\n".as_ptr() as _);
        }
        XmlRelaxNGType::Externalref => {
            fprintf(output, c"<externalRef>".as_ptr() as _);
            xml_relaxng_dump_defines(output, (*define).content);
            fprintf(output, c"</externalRef>\n".as_ptr() as _);
        }
        XmlRelaxNGType::Datatype | XmlRelaxNGType::Value => {
            // TODO
        }
        XmlRelaxNGType::Start | XmlRelaxNGType::Except | XmlRelaxNGType::Param => {
            // TODO
        }
        XmlRelaxNGType::Noop => {
            xml_relaxng_dump_defines(output, (*define).content);
        }
    }
}

/**
 * xmlRelaxNGDumpGrammar:
 * @output:  the file output
 * @grammar:  a grammar structure
 * @top:  is this a top grammar
 *
 * Dump a RelaxNG structure back
 */
#[cfg(feature = "output")]
unsafe extern "C" fn xml_relaxng_dump_grammar(
    output: *mut FILE,
    grammar: XmlRelaxNGGrammarPtr,
    top: i32,
) {
    if grammar.is_null() {
        return;
    }

    fprintf(output, c"<grammar".as_ptr() as _);
    if top != 0 {
        fprintf(
            output,
            c" xmlns=\"http://relaxng.org/ns/structure/1.0\"".as_ptr() as _,
        );
    }
    match (*grammar).combine {
        XmlRelaxNGCombine::Undefined => {}
        XmlRelaxNGCombine::Choice => {
            fprintf(output, c" combine=\"choice\"".as_ptr() as _);
        }
        XmlRelaxNGCombine::Interleave => {
            fprintf(output, c" combine=\"interleave\"".as_ptr() as _);
        } // _ => {
          //     fprintf(output, c" <!-- invalid combine value -->".as_ptr() as _);
          // }
    }
    fprintf(output, c">\n".as_ptr() as _);
    if (*grammar).start.is_null() {
        fprintf(output, c" <!-- grammar had no start -->".as_ptr() as _);
    } else {
        fprintf(output, c"<start>\n".as_ptr() as _);
        xml_relaxng_dump_define(output, (*grammar).start);
        fprintf(output, c"</start>\n".as_ptr() as _);
    }
    /* TODO ? Dump the defines ? */
    fprintf(output, c"</grammar>\n".as_ptr() as _);
}

/**
 * xmlRelaxNGDump:
 * @output:  the file output
 * @schema:  a schema structure
 *
 * Dump a RelaxNG structure back
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_relaxng_dump(output: *mut FILE, schema: XmlRelaxNGPtr) {
    use std::ffi::CString;

    if output.is_null() {
        return;
    }
    if schema.is_null() {
        fprintf(
            output,
            c"RelaxNG empty or failed to compile\n".as_ptr() as _,
        );
        return;
    }
    fprintf(output, c"RelaxNG: ".as_ptr() as _);
    if (*schema).doc.is_null() {
        fprintf(output, c"no document\n".as_ptr() as _);
    } else if let Some(url) = (*(*schema).doc).url.as_deref() {
        let url = CString::new(url).unwrap();
        fprintf(output, c"%s\n".as_ptr() as _, url.as_ptr());
    } else {
        fprintf(output, c"\n".as_ptr() as _);
    }
    if (*schema).topgrammar.is_null() {
        fprintf(output, c"RelaxNG has no top grammar\n".as_ptr() as _);
        return;
    }
    xml_relaxng_dump_grammar(output, (*schema).topgrammar, 1);
}

/**
 * xmlRelaxNGDumpTree:
 * @output:  the file output
 * @schema:  a schema structure
 *
 * Dump the transformed RelaxNG tree.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_relaxng_dump_tree(output: *mut FILE, schema: XmlRelaxNGPtr) {
    use crate::tree::xml_doc_dump;

    if output.is_null() {
        return;
    }
    if schema.is_null() {
        fprintf(
            output,
            c"RelaxNG empty or failed to compile\n".as_ptr() as _,
        );
        return;
    }
    if (*schema).doc.is_null() {
        fprintf(output, c"no document\n".as_ptr() as _);
    } else {
        xml_doc_dump(output, (*schema).doc);
    }
}

/*
 * Interfaces for validating
 */
/**
 * xmlRelaxNGSetValidErrors:
 * @ctxt:  a Relax-NG validation context
 * @err:  the error function
 * @warn: the warning function
 * @ctx: the functions context
 *
 * Set the error and warning callback information
 */
pub unsafe fn xml_relaxng_set_valid_errors(
    ctxt: XmlRelaxNGValidCtxtPtr,
    err: Option<GenericError>,
    warn: Option<GenericError>,
    ctx: Option<GenericErrorContext>,
) {
    if ctxt.is_null() {
        return;
    }
    (*ctxt).error = err;
    (*ctxt).warning = warn;
    (*ctxt).user_data = ctx;
    (*ctxt).serror = None;
}

/**
 * xmlRelaxNGGetValidErrors:
 * @ctxt:  a Relax-NG validation context
 * @err:  the error function result
 * @warn: the warning function result
 * @ctx: the functions context result
 *
 * Get the error and warning callback information
 *
 * Returns -1 in case of error and 0 otherwise
 */
pub unsafe extern "C" fn xml_relaxng_get_valid_errors(
    ctxt: XmlRelaxNGValidCtxtPtr,
    err: *mut Option<GenericError>,
    warn: *mut Option<GenericError>,
    ctx: *mut Option<GenericErrorContext>,
) -> i32 {
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
        *ctx = (*ctxt).user_data.clone();
    }
    0
}

/**
 * xmlRelaxNGSetValidStructuredErrors:
 * @ctxt:  a Relax-NG validation context
 * @serror:  the structured error function
 * @ctx: the functions context
 *
 * Set the structured error callback
 */
pub unsafe fn xml_relaxng_set_valid_structured_errors(
    ctxt: XmlRelaxNGValidCtxtPtr,
    serror: Option<StructuredError>,
    ctx: Option<GenericErrorContext>,
) {
    if ctxt.is_null() {
        return;
    }
    (*ctxt).serror = serror;
    (*ctxt).error = None;
    (*ctxt).warning = None;
    (*ctxt).user_data = ctx;
}

/**
 * xmlRelaxNGFreeStates:
 * @ctxt:  a Relax-NG validation context
 * @states:  the container
 *
 * Free a RelaxNG validation state container
 */
unsafe extern "C" fn xml_relaxng_free_states(
    ctxt: XmlRelaxNGValidCtxtPtr,
    states: XmlRelaxNGStatesPtr,
) {
    if states.is_null() {
        return;
    }
    if !ctxt.is_null() && (*ctxt).free_states.is_null() {
        (*ctxt).free_states_max = 40;
        (*ctxt).free_states_nr = 0;
        (*ctxt).free_states =
            xml_malloc((*ctxt).free_states_max as usize * size_of::<XmlRelaxNGStatesPtr>()) as _;
        if (*ctxt).free_states.is_null() {
            xml_rng_verr_memory(ctxt, c"storing states\n".as_ptr() as _);
        }
    } else if !ctxt.is_null() && (*ctxt).free_states_nr >= (*ctxt).free_states_max {
        let tmp: *mut XmlRelaxNGStatesPtr = xml_realloc(
            (*ctxt).free_states as _,
            2 * (*ctxt).free_states_max as usize * size_of::<XmlRelaxNGStatesPtr>(),
        ) as _;
        if tmp.is_null() {
            xml_rng_verr_memory(ctxt, c"storing states\n".as_ptr() as _);
            xml_free((*states).tab_state as _);
            xml_free(states as _);
            return;
        }
        (*ctxt).free_states = tmp;
        (*ctxt).free_states_max *= 2;
    }
    if ctxt.is_null() || (*ctxt).free_states.is_null() {
        xml_free((*states).tab_state as _);
        xml_free(states as _);
    } else {
        *(*ctxt).free_states.add((*ctxt).free_states_nr as usize) = states;
        (*ctxt).free_states_nr += 1;
    }
}

/**
 * xmlRelaxNGNewValidCtxt:
 * @schema:  a precompiled XML RelaxNGs
 *
 * Create an XML RelaxNGs validation context based on the given schema
 *
 * Returns the validation context or NULL in case of error
 */
pub unsafe extern "C" fn xml_relaxng_new_valid_ctxt(
    schema: XmlRelaxNGPtr,
) -> XmlRelaxNGValidCtxtPtr {
    let ret: XmlRelaxNGValidCtxtPtr = xml_malloc(size_of::<XmlRelaxNGValidCtxt>()) as _;
    if ret.is_null() {
        xml_rng_verr_memory(null_mut(), c"building context\n".as_ptr() as _);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlRelaxNGValidCtxt>());
    (*ret).schema = schema;
    GLOBAL_STATE.with_borrow(|state| {
        (*ret).error = Some(state.generic_error);
        (*ret).user_data = state.generic_error_context.clone();
    });
    (*ret).err_nr = 0;
    (*ret).err_max = 0;
    (*ret).err = null_mut();
    (*ret).err_tab = null_mut();
    if !schema.is_null() {
        (*ret).idref = (*schema).idref;
    }
    (*ret).states = null_mut();
    (*ret).free_state = null_mut();
    (*ret).free_states = null_mut();
    (*ret).err_no = XmlRelaxNGValidErr::XmlRelaxngOk as i32;
    ret
}

/**
 * xmlRelaxNGNewStates:
 * @ctxt:  a Relax-NG validation context
 * @size:  the default size for the container
 *
 * Allocate a new RelaxNG validation state container
 *
 * Returns the newly allocated structure or NULL in case or error
 */
unsafe extern "C" fn xml_relaxng_new_states(
    ctxt: XmlRelaxNGValidCtxtPtr,
    mut size: i32,
) -> XmlRelaxNGStatesPtr {
    let ret: XmlRelaxNGStatesPtr;

    if !ctxt.is_null() && !(*ctxt).free_states.is_null() && (*ctxt).free_states_nr > 0 {
        (*ctxt).free_states_nr -= 1;
        ret = *(*ctxt).free_states.add((*ctxt).free_states_nr as usize);
        (*ret).nb_state = 0;
        return ret;
    }
    if size < 16 {
        size = 16;
    }

    ret = xml_malloc(
        size_of::<XmlRelaxNGStates>() + (size as usize - 1) * size_of::<XmlRelaxNGValidStatePtr>(),
    ) as _;
    if ret.is_null() {
        xml_rng_verr_memory(ctxt, c"allocating states\n".as_ptr() as _);
        return null_mut();
    }
    (*ret).nb_state = 0;
    (*ret).max_state = size;
    (*ret).tab_state = xml_malloc(size as usize * size_of::<XmlRelaxNGValidStatePtr>()) as _;
    if (*ret).tab_state.is_null() {
        xml_rng_verr_memory(ctxt, c"allocating states\n".as_ptr() as _);
        xml_free(ret as _);
        return null_mut();
    }
    ret
}

/**
 * xmlRelaxNGAddStateUniq:
 * @ctxt:  a Relax-NG validation context
 * @states:  the states container
 * @state:  the validation state
 *
 * Add a RelaxNG validation state to the container without checking
 * for unicity.
 *
 * Return 1 in case of success and 0 if this is a duplicate and -1 on error
 */
unsafe extern "C" fn xml_relaxng_add_states_uniq(
    ctxt: XmlRelaxNGValidCtxtPtr,
    states: XmlRelaxNGStatesPtr,
    state: XmlRelaxNGValidStatePtr,
) -> i32 {
    if state.is_null() {
        return -1;
    }
    if (*states).nb_state >= (*states).max_state {
        let size: i32 = (*states).max_state * 2;
        let tmp: *mut XmlRelaxNGValidStatePtr = xml_realloc(
            (*states).tab_state as _,
            size as usize * size_of::<XmlRelaxNGValidStatePtr>(),
        ) as _;
        if tmp.is_null() {
            xml_rng_verr_memory(ctxt, c"adding states\n".as_ptr() as _);
            return -1;
        }
        (*states).tab_state = tmp;
        (*states).max_state = size;
    }
    *(*states).tab_state.add((*states).nb_state as usize) = state;
    (*states).nb_state += 1;
    1
}

/**
 * xmlRelaxNGFreeValidState:
 * @state:  a validation state structure
 *
 * Deallocate a RelaxNG validation state structure.
 */
unsafe extern "C" fn xml_relaxng_free_valid_state(
    ctxt: XmlRelaxNGValidCtxtPtr,
    state: XmlRelaxNGValidStatePtr,
) {
    if state.is_null() {
        return;
    }

    if !ctxt.is_null() && (*ctxt).free_state.is_null() {
        (*ctxt).free_state = xml_relaxng_new_states(ctxt, 40);
    }
    if ctxt.is_null() || (*ctxt).free_state.is_null() {
        if !(*state).attrs.is_null() {
            xml_free((*state).attrs as _);
        }
        xml_free(state as _);
    } else {
        xml_relaxng_add_states_uniq(ctxt, (*ctxt).free_state, state);
    }
}

/**
 * xmlRelaxNGElemPop:
 * @ctxt:  the validation context
 *
 * Pop the regexp of the current node content model from the stack
 *
 * Returns the exec or NULL if empty
 */
unsafe extern "C" fn xml_relaxng_elem_pop(ctxt: XmlRelaxNGValidCtxtPtr) -> XmlRegExecCtxtPtr {
    if (*ctxt).elem_nr <= 0 {
        return null_mut();
    }
    (*ctxt).elem_nr -= 1;
    let ret: XmlRegExecCtxtPtr = *(*ctxt).elem_tab.add((*ctxt).elem_nr as usize);
    *(*ctxt).elem_tab.add((*ctxt).elem_nr as usize) = null_mut();
    if (*ctxt).elem_nr > 0 {
        (*ctxt).elem = *(*ctxt).elem_tab.add((*ctxt).elem_nr as usize - 1);
    } else {
        (*ctxt).elem = null_mut();
    }
    ret
}

/**
 * xmlRelaxNGFreeValidCtxt:
 * @ctxt:  the schema validation context
 *
 * Free the resources associated to the schema validation context
 */
pub unsafe extern "C" fn xml_relaxng_free_valid_ctxt(ctxt: XmlRelaxNGValidCtxtPtr) {
    if ctxt.is_null() {
        return;
    }
    if !(*ctxt).states.is_null() {
        xml_relaxng_free_states(null_mut(), (*ctxt).states);
    }
    if !(*ctxt).free_state.is_null() {
        for k in 0..(*(*ctxt).free_state).nb_state {
            xml_relaxng_free_valid_state(
                null_mut(),
                *(*(*ctxt).free_state).tab_state.add(k as usize),
            );
        }
        xml_relaxng_free_states(null_mut(), (*ctxt).free_state);
    }
    if !(*ctxt).free_states.is_null() {
        for k in 0..(*ctxt).free_states_nr {
            xml_relaxng_free_states(null_mut(), *(*ctxt).free_states.add(k as usize));
        }
        xml_free((*ctxt).free_states as _);
    }
    if !(*ctxt).err_tab.is_null() {
        xml_free((*ctxt).err_tab as _);
    }
    if !(*ctxt).elem_tab.is_null() {
        let mut exec: XmlRegExecCtxtPtr;

        exec = xml_relaxng_elem_pop(ctxt);
        while !exec.is_null() {
            xml_reg_free_exec_ctxt(exec);
            exec = xml_relaxng_elem_pop(ctxt);
        }
        xml_free((*ctxt).elem_tab as _);
    }
    xml_free(ctxt as _);
}

/**
 * xmlRelaxNGNewValidState:
 * @ctxt:  a Relax-NG validation context
 * @node:  the current node or NULL for the document
 *
 * Allocate a new RelaxNG validation state
 *
 * Returns the newly allocated structure or NULL in case or error
 */
unsafe extern "C" fn xml_relaxng_new_valid_state(
    ctxt: XmlRelaxNGValidCtxtPtr,
    node: XmlNodePtr,
) -> XmlRelaxNGValidStatePtr {
    let ret: XmlRelaxNGValidStatePtr;
    let mut attr: XmlAttrPtr;
    let mut attrs: [XmlAttrPtr; MAX_ATTR] = [null_mut(); MAX_ATTR];
    let mut nb_attrs: usize = 0;
    let mut root: XmlNodePtr = null_mut();

    if node.is_null() {
        root = if (*ctxt).doc.is_null() {
            null_mut()
        } else {
            (*(*ctxt).doc).get_root_element()
        };
        if root.is_null() {
            return null_mut();
        }
    } else {
        attr = (*node).properties;
        while !attr.is_null() {
            if nb_attrs < MAX_ATTR {
                attrs[nb_attrs] = attr;
                nb_attrs += 1;
            } else {
                nb_attrs += 1;
            }
            attr = (*attr).next;
        }
    }
    if !(*ctxt).free_state.is_null() && (*(*ctxt).free_state).nb_state > 0 {
        (*(*ctxt).free_state).nb_state -= 1;
        ret = *(*(*ctxt).free_state)
            .tab_state
            .add((*(*ctxt).free_state).nb_state as usize);
    } else {
        ret = xml_malloc(size_of::<XmlRelaxNGValidState>()) as _;
        if ret.is_null() {
            xml_rng_verr_memory(ctxt, c"allocating states\n".as_ptr() as _);
            return null_mut();
        }
        memset(ret as _, 0, size_of::<XmlRelaxNGValidState>());
    }
    (*ret).value = null_mut();
    (*ret).endvalue = null_mut();
    if node.is_null() {
        (*ret).node = (*ctxt).doc as _;
        (*ret).seq = root;
    } else {
        (*ret).node = node;
        (*ret).seq = (*node).children;
    }
    (*ret).nb_attrs = 0;
    if nb_attrs > 0 {
        if (*ret).attrs.is_null() {
            if nb_attrs < 4 {
                (*ret).max_attrs = 4;
            } else {
                (*ret).max_attrs = nb_attrs as _;
            }
            (*ret).attrs = xml_malloc((*ret).max_attrs as usize * size_of::<XmlAttrPtr>()) as _;
            if (*ret).attrs.is_null() {
                xml_rng_verr_memory(ctxt, c"allocating states\n".as_ptr() as _);
                return ret;
            }
        } else if (*ret).max_attrs < nb_attrs as i32 {
            let tmp: *mut XmlAttrPtr =
                xml_realloc((*ret).attrs as _, nb_attrs * size_of::<XmlAttrPtr>()) as _;
            if tmp.is_null() {
                xml_rng_verr_memory(ctxt, c"allocating states\n".as_ptr() as _);
                return ret;
            }
            (*ret).attrs = tmp;
            (*ret).max_attrs = nb_attrs as _;
        }
        (*ret).nb_attrs = nb_attrs as _;
        if nb_attrs < MAX_ATTR {
            memcpy(
                (*ret).attrs as _,
                attrs.as_ptr() as _,
                size_of::<XmlAttrPtr>() * nb_attrs,
            );
        } else {
            attr = (*node).properties;
            nb_attrs = 0;
            while !attr.is_null() {
                *(*ret).attrs.add(nb_attrs) = attr;
                nb_attrs += 1;
                attr = (*attr).next;
            }
        }
    }
    (*ret).nb_attr_left = (*ret).nb_attrs;
    ret
}

/**
 * xmlRelaxNGSkipIgnored:
 * @ctxt:  a schema validation context
 * @node:  the top node.
 *
 * Skip ignorable nodes in that context
 *
 * Returns the new sibling or NULL in case of error.
 */
unsafe extern "C" fn xml_relaxng_skip_ignored(
    ctxt: XmlRelaxNGValidCtxtPtr,
    mut node: XmlNodePtr,
) -> XmlNodePtr {
    /*
     * TODO complete and handle entities
     */
    while !node.is_null()
        && (matches!(
            (*node).typ,
            XmlElementType::XmlCommentNode
                | XmlElementType::XmlPiNode
                | XmlElementType::XmlXincludeStart
                | XmlElementType::XmlXincludeEnd
        ) || (matches!(
            (*node).typ,
            XmlElementType::XmlTextNode | XmlElementType::XmlCdataSectionNode
        ) && ((*ctxt).flags & FLAGS_MIXED_CONTENT != 0 || IS_BLANK_NODE!(node))))
    {
        node = (*node).next;
    }
    node
}

/**
 * xmlRelaxNGValidErrorPop:
 * @ctxt: the validation context
 *
 * Pops the top error from the error stack
 */
unsafe extern "C" fn xml_relaxng_valid_error_pop(ctxt: XmlRelaxNGValidCtxtPtr) {
    if (*ctxt).err_nr <= 0 {
        (*ctxt).err = null_mut();
        return;
    }
    (*ctxt).err_nr -= 1;
    if (*ctxt).err_nr > 0 {
        (*ctxt).err = (*ctxt).err_tab.add((*ctxt).err_nr as usize - 1);
    } else {
        (*ctxt).err = null_mut();
    }
    let cur: XmlRelaxNGValidErrorPtr = (*ctxt).err_tab.add((*ctxt).err_nr as usize);
    if (*cur).flags & ERROR_IS_DUP != 0 {
        if !(*cur).arg1.is_null() {
            xml_free((*cur).arg1 as _);
        }
        (*cur).arg1 = null_mut();
        if !(*cur).arg2.is_null() {
            xml_free((*cur).arg2 as _);
        }
        (*cur).arg2 = null_mut();
        (*cur).flags = 0;
    }
}

/**
 * xmlRelaxNGNextValue:
 * @ctxt:  a Relax-NG validation context
 *
 * Skip to the next value when validating within a list
 *
 * Returns 0 if the operation succeeded or an error code.
 */
unsafe extern "C" fn xml_relaxng_next_value(ctxt: XmlRelaxNGValidCtxtPtr) -> i32 {
    let mut cur: *mut XmlChar;

    cur = (*(*ctxt).state).value;
    if cur.is_null() || (*(*ctxt).state).endvalue.is_null() {
        (*(*ctxt).state).value = null_mut();
        (*(*ctxt).state).endvalue = null_mut();
        return 0;
    }
    while *cur != 0 {
        cur = cur.add(1);
    }
    while cur != (*(*ctxt).state).endvalue && *cur == 0 {
        cur = cur.add(1);
    }
    if cur == (*(*ctxt).state).endvalue {
        (*(*ctxt).state).value = null_mut();
    } else {
        (*(*ctxt).state).value = cur;
    }
    0
}

/**
 * xmlRelaxNGValidateValueList:
 * @ctxt:  a Relax-NG validation context
 * @defines:  the list of definitions to verify
 *
 * Validate the given set of definitions for the current value
 *
 * Returns 0 if the validation succeeded or an error code.
 */
unsafe extern "C" fn xml_relaxng_validate_value_list(
    ctxt: XmlRelaxNGValidCtxtPtr,
    mut defines: XmlRelaxNGDefinePtr,
) -> i32 {
    let mut ret: i32 = 0;

    while !defines.is_null() {
        ret = xml_relaxng_validate_value(ctxt, defines);
        if ret != 0 {
            break;
        }
        defines = (*defines).next;
    }
    ret
}

/**
 * xmlRelaxNGValidateDatatype:
 * @ctxt:  a Relax-NG validation context
 * @value:  the string value
 * @type:  the datatype definition
 * @node:  the node
 *
 * Validate the given value against the datatype
 *
 * Returns 0 if the validation succeeded or an error code.
 */
unsafe extern "C" fn xml_relaxng_validate_datatype(
    ctxt: XmlRelaxNGValidCtxtPtr,
    value: *const XmlChar,
    define: XmlRelaxNGDefinePtr,
    node: XmlNodePtr,
) -> i32 {
    let mut ret: i32;
    let mut tmp: i32;
    let mut result: *mut c_void = null_mut();
    let mut cur: XmlRelaxNGDefinePtr;

    if define.is_null() || (*define).data.is_null() {
        return -1;
    }
    let lib: XmlRelaxNGTypeLibraryPtr = (*define).data as _;
    if let Some(check) = (*lib).check {
        if !(*define).attrs.is_null() && (*(*define).attrs).typ == XmlRelaxNGType::Param {
            ret = check(
                (*lib).data,
                (*define).name,
                value,
                addr_of_mut!(result),
                node,
            );
        } else {
            ret = check((*lib).data, (*define).name, value, null_mut(), node);
        }
    } else {
        ret = -1;
    }
    if ret < 0 {
        VALID_ERR2!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrType, (*define).name);
        if !result.is_null() && !lib.is_null() && (*lib).freef.is_some() {
            (*lib).freef.unwrap()((*lib).data, result);
        }
        return -1;
    } else if ret == 1 {
        ret = 0;
    } else if ret == 2 {
        VALID_ERR2P!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrDupid, value);
    } else {
        VALID_ERR3P!(
            ctxt,
            XmlRelaxNGValidErr::XmlRelaxngErrTypeval,
            (*define).name,
            value
        );
        ret = -1;
    }
    cur = (*define).attrs;
    while ret == 0 && !cur.is_null() && (*cur).typ == XmlRelaxNGType::Param {
        if let Some(facet) = (*lib).facet {
            tmp = facet(
                (*lib).data,
                (*define).name,
                (*cur).name,
                (*cur).value,
                value,
                result,
            );
            if tmp != 0 {
                ret = -1;
            }
        }
        cur = (*cur).next;
    }
    if ret == 0 && !(*define).content.is_null() {
        let oldvalue: *const XmlChar = (*(*ctxt).state).value;
        let oldendvalue: *const XmlChar = (*(*ctxt).state).endvalue;
        (*(*ctxt).state).value = value as _;
        (*(*ctxt).state).endvalue = null_mut();
        ret = xml_relaxng_validate_value(ctxt, (*define).content);
        (*(*ctxt).state).value = oldvalue as _;
        (*(*ctxt).state).endvalue = oldendvalue as _;
    }
    if !result.is_null() && !lib.is_null() && (*lib).freef.is_some() {
        (*lib).freef.unwrap()((*lib).data, result);
    }
    ret
}

/**
 * xmlRelaxNGValidateValue:
 * @ctxt:  a Relax-NG validation context
 * @define:  the definition to verify
 *
 * Validate the given definition for the current value
 *
 * Returns 0 if the validation succeeded or an error code.
 */
unsafe extern "C" fn xml_relaxng_validate_value(
    ctxt: XmlRelaxNGValidCtxtPtr,
    define: XmlRelaxNGDefinePtr,
) -> i32 {
    let mut ret: i32 = 0;
    let oldflags: i32;

    let value: *mut XmlChar = (*(*ctxt).state).value;
    match (*define).typ {
        XmlRelaxNGType::Empty => {
            if !value.is_null() && *value.add(0) != 0 {
                let mut idx: i32 = 0;

                while xml_is_blank_char(*value.add(idx as usize) as u32) {
                    idx += 1;
                }
                if *value.add(idx as usize) != 0 {
                    ret = -1;
                }
            }
        }
        XmlRelaxNGType::Text => {}
        XmlRelaxNGType::Value => {
            if !xml_str_equal(value, (*define).value) {
                if !(*define).name.is_null() {
                    let lib: XmlRelaxNGTypeLibraryPtr = (*define).data as _;
                    if !lib.is_null() && (*lib).comp.is_some() {
                        ret = (*lib).comp.unwrap()(
                            (*lib).data,
                            (*define).name,
                            (*define).value,
                            (*define).node,
                            (*define).attrs as _,
                            value,
                            (*(*ctxt).state).node,
                        );
                    } else {
                        ret = -1;
                    }
                    if ret < 0 {
                        VALID_ERR2!(
                            ctxt,
                            XmlRelaxNGValidErr::XmlRelaxngErrTypecmp,
                            (*define).name
                        );
                        return -1;
                    } else if ret == 1 {
                        ret = 0;
                    } else {
                        ret = -1;
                    }
                } else {
                    /*
                     * TODO: trivial optimizations are possible by
                     * computing at compile-time
                     */
                    let nval: *mut XmlChar = xml_relaxng_normalize(ctxt, (*define).value);
                    let nvalue: *mut XmlChar = xml_relaxng_normalize(ctxt, value);

                    if nval.is_null() || nvalue.is_null() || !xml_str_equal(nval, nvalue) {
                        ret = -1;
                    }
                    if !nval.is_null() {
                        xml_free(nval as _);
                    }
                    if !nvalue.is_null() {
                        xml_free(nvalue as _);
                    }
                }
            }
            if ret == 0 {
                xml_relaxng_next_value(ctxt);
            }
        }
        XmlRelaxNGType::Datatype => {
            ret = xml_relaxng_validate_datatype(ctxt, value, define, (*(*ctxt).state).seq);
            if ret == 0 {
                xml_relaxng_next_value(ctxt);
            }
        }
        XmlRelaxNGType::Choice => {
            let mut list: XmlRelaxNGDefinePtr = (*define).content;

            oldflags = (*ctxt).flags;
            (*ctxt).flags |= FLAGS_IGNORABLE;

            let oldvalue: *mut XmlChar = (*(*ctxt).state).value;
            while !list.is_null() {
                ret = xml_relaxng_validate_value(ctxt, list);
                if ret == 0 {
                    break;
                }
                (*(*ctxt).state).value = oldvalue;
                list = (*list).next;
            }
            (*ctxt).flags = oldflags;
            if ret != 0 {
                if (*ctxt).flags & FLAGS_IGNORABLE == 0 {
                    xml_relaxng_dump_valid_error(ctxt);
                }
            } else if (*ctxt).err_nr > 0 {
                xml_relaxng_pop_errors(ctxt, 0);
            }
        }
        XmlRelaxNGType::List => {
            let mut list: XmlRelaxNGDefinePtr = (*define).content;
            let mut val: *mut XmlChar;
            let mut cur: *mut XmlChar;
            let oldvalue: *mut XmlChar = (*(*ctxt).state).value;
            let oldend: *mut XmlChar = (*(*ctxt).state).endvalue;

            val = xml_strdup(oldvalue);
            if val.is_null() {
                val = xml_strdup(c"".as_ptr() as _);
            }
            if val.is_null() {
                VALID_ERR!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrNostate);
                return -1;
            }
            cur = val;
            while *cur != 0 {
                if xml_is_blank_char(*cur as u32) {
                    *cur = 0;
                    cur = cur.add(1);
                    while xml_is_blank_char(*cur as u32) {
                        *cur = 0;
                        cur = cur.add(1);
                    }
                } else {
                    cur = cur.add(1);
                }
            }
            (*(*ctxt).state).endvalue = cur;
            cur = val;
            while *cur == 0 && cur != (*(*ctxt).state).endvalue {
                cur = cur.add(1);
            }

            (*(*ctxt).state).value = cur;

            while !list.is_null() {
                if (*(*ctxt).state).value == (*(*ctxt).state).endvalue {
                    (*(*ctxt).state).value = null_mut();
                }
                ret = xml_relaxng_validate_value(ctxt, list);
                if ret != 0 {
                    break;
                }
                list = (*list).next;
            }

            if ret == 0
                && !(*(*ctxt).state).value.is_null()
                && (*(*ctxt).state).value != (*(*ctxt).state).endvalue
            {
                VALID_ERR2!(
                    ctxt,
                    XmlRelaxNGValidErr::XmlRelaxngErrListextra,
                    (*(*ctxt).state).value
                );
                ret = -1;
            }
            xml_free(val as _);
            (*(*ctxt).state).value = oldvalue;
            (*(*ctxt).state).endvalue = oldend;
        }
        ty @ XmlRelaxNGType::Oneormore | ty @ XmlRelaxNGType::Zeroormore => 'to_break: {
            if matches!(ty, XmlRelaxNGType::Oneormore) {
                ret = xml_relaxng_validate_value_list(ctxt, (*define).content);
                if ret != 0 {
                    break 'to_break;
                }
            }

            let mut cur: *mut XmlChar;
            let mut temp: *mut XmlChar;

            if (*(*ctxt).state).value.is_null() || *(*(*ctxt).state).value == 0 {
                ret = 0;
                break 'to_break;
            }
            oldflags = (*ctxt).flags;
            (*ctxt).flags |= FLAGS_IGNORABLE;
            cur = (*(*ctxt).state).value as _;
            temp = null_mut();
            while !cur.is_null() && cur != (*(*ctxt).state).endvalue && temp != cur {
                temp = cur;
                ret = xml_relaxng_validate_value_list(ctxt, (*define).content);
                if ret != 0 {
                    (*(*ctxt).state).value = temp;
                    ret = 0;
                    break;
                }
                cur = (*(*ctxt).state).value;
            }
            (*ctxt).flags = oldflags;
            if (*ctxt).err_nr > 0 {
                xml_relaxng_pop_errors(ctxt, 0);
            }
        }
        XmlRelaxNGType::Optional => 'to_break: {
            if (*(*ctxt).state).value.is_null() || *(*(*ctxt).state).value == 0 {
                ret = 0;
                break 'to_break;
            }
            oldflags = (*ctxt).flags;
            (*ctxt).flags |= FLAGS_IGNORABLE;
            let temp: *mut XmlChar = (*(*ctxt).state).value;
            ret = xml_relaxng_validate_value(ctxt, (*define).content);
            (*ctxt).flags = oldflags;
            if ret != 0 {
                (*(*ctxt).state).value = temp;
                if (*ctxt).err_nr > 0 {
                    xml_relaxng_pop_errors(ctxt, 0);
                }
                ret = 0;
                break 'to_break;
            }
            if (*ctxt).err_nr > 0 {
                xml_relaxng_pop_errors(ctxt, 0);
            }
        }
        XmlRelaxNGType::Except => {
            let mut list: XmlRelaxNGDefinePtr;

            list = (*define).content;
            while !list.is_null() {
                ret = xml_relaxng_validate_value(ctxt, list);
                if ret == 0 {
                    ret = -1;
                    break;
                } else {
                    ret = 0;
                }
                list = (*list).next;
            }
        }
        XmlRelaxNGType::Def | XmlRelaxNGType::Group => {
            let mut list: XmlRelaxNGDefinePtr;

            list = (*define).content;
            while !list.is_null() {
                ret = xml_relaxng_validate_value(ctxt, list);
                if ret != 0 {
                    ret = -1;
                    break;
                } else {
                    ret = 0;
                }
                list = (*list).next;
            }
        }
        XmlRelaxNGType::Ref | XmlRelaxNGType::Parentref => {
            if (*define).content.is_null() {
                VALID_ERR!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrNodefine);
                ret = -1;
            } else {
                ret = xml_relaxng_validate_value(ctxt, (*define).content);
            }
        }
        _ => {
            // TODO
            ret = -1;
        }
    }
    ret
}

/**
 * xmlRelaxNGValidateValueContent:
 * @ctxt:  a Relax-NG validation context
 * @defines:  the list of definitions to verify
 *
 * Validate the given definitions for the current value
 *
 * Returns 0 if the validation succeeded or an error code.
 */
unsafe extern "C" fn xml_relaxng_validate_value_content(
    ctxt: XmlRelaxNGValidCtxtPtr,
    mut defines: XmlRelaxNGDefinePtr,
) -> i32 {
    let mut ret: i32 = 0;

    while !defines.is_null() {
        ret = xml_relaxng_validate_value(ctxt, defines);
        if ret != 0 {
            break;
        }
        defines = (*defines).next;
    }
    ret
}

/**
 * xmlRelaxNGAttributeMatch:
 * @ctxt:  a Relax-NG validation context
 * @define:  the definition to check
 * @prop:  the attribute
 *
 * Check if the attribute matches the definition nameClass
 *
 * Returns 1 if the attribute matches, 0 if no, or -1 in case of error
 */
unsafe extern "C" fn xml_relaxng_attribute_match(
    _ctxt: XmlRelaxNGValidCtxtPtr,
    mut define: XmlRelaxNGDefinePtr,
    prop: XmlAttrPtr,
) -> i32 {
    let mut ret: i32;

    if !(*define).name.is_null() && !xml_str_equal((*define).name, (*prop).name) {
        return 0;
    }
    if !(*define).ns.is_null() {
        if *(*define).ns.add(0) == 0 {
            if !(*prop).ns.is_null() {
                return 0;
            }
        } else if (*prop).ns.is_null()
            || !xml_str_equal((*define).ns, (*(*prop).ns).href.load(Ordering::Relaxed))
        {
            return 0;
        }
    }
    if (*define).name_class.is_null() {
        return 1;
    }
    define = (*define).name_class;
    if (*define).typ == XmlRelaxNGType::Except {
        let mut list: XmlRelaxNGDefinePtr;

        list = (*define).content;
        while !list.is_null() {
            ret = xml_relaxng_attribute_match(_ctxt, list, prop);
            if ret == 1 {
                return 0;
            }
            if ret < 0 {
                return ret;
            }
            list = (*list).next;
        }
    } else if (*define).typ == XmlRelaxNGType::Choice {
        let mut list: XmlRelaxNGDefinePtr;

        list = (*define).name_class;
        while !list.is_null() {
            ret = xml_relaxng_attribute_match(_ctxt, list, prop);
            if ret == 1 {
                return 1;
            }
            if ret < 0 {
                return ret;
            }
            list = (*list).next;
        }
        return 0;
    } else {
        // TODO
    }
    1
}

/**
 * xmlRelaxNGValidateAttribute:
 * @ctxt:  a Relax-NG validation context
 * @define:  the definition to verify
 *
 * Validate the given attribute definition for that node
 *
 * Returns 0 if the validation succeeded or an error code.
 */
unsafe extern "C" fn xml_relaxng_validate_attribute(
    ctxt: XmlRelaxNGValidCtxtPtr,
    define: XmlRelaxNGDefinePtr,
) -> i32 {
    let ret: i32;
    let mut value: *mut XmlChar;
    let oldvalue: *mut XmlChar;
    let mut prop: XmlAttrPtr = null_mut();
    let mut tmp: XmlAttrPtr;
    let oldseq: XmlNodePtr;

    if (*(*ctxt).state).nb_attr_left <= 0 {
        return -1;
    }
    if !(*define).name.is_null() {
        let mut j = (*(*ctxt).state).nb_attrs;
        for i in 0..(*(*ctxt).state).nb_attrs {
            tmp = *(*(*ctxt).state).attrs.add(i as usize);
            if !tmp.is_null()
                && xml_str_equal((*define).name, (*tmp).name)
                && ((((*define).ns.is_null() || *(*define).ns.add(0) == 0) && (*tmp).ns.is_null())
                    || (!(*tmp).ns.is_null()
                        && xml_str_equal((*define).ns, (*(*tmp).ns).href.load(Ordering::Relaxed))))
            {
                prop = tmp;
                j = i;
                break;
            }
        }
        if !prop.is_null() {
            value = xml_node_list_get_string((*prop).doc, (*prop).children, 1);
            oldvalue = (*(*ctxt).state).value;
            oldseq = (*(*ctxt).state).seq;
            (*(*ctxt).state).seq = prop as _;
            (*(*ctxt).state).value = value;
            (*(*ctxt).state).endvalue = null_mut();
            ret = xml_relaxng_validate_value_content(ctxt, (*define).content);
            if !(*(*ctxt).state).value.is_null() {
                value = (*(*ctxt).state).value;
            }
            if !value.is_null() {
                xml_free(value as _);
            }
            (*(*ctxt).state).value = oldvalue;
            (*(*ctxt).state).seq = oldseq;
            if ret == 0 {
                /*
                 * flag the attribute as processed
                 */
                *(*(*ctxt).state).attrs.add(j as usize) = null_mut();
                (*(*ctxt).state).nb_attr_left -= 1;
            }
        } else {
            ret = -1;
        }
    } else {
        let mut j = (*(*ctxt).state).nb_attrs;
        for i in 0..(*(*ctxt).state).nb_attrs {
            tmp = *(*(*ctxt).state).attrs.add(i as usize);
            if !tmp.is_null() && xml_relaxng_attribute_match(ctxt, define, tmp) == 1 {
                prop = tmp;
                j = i;
                break;
            }
        }
        if !prop.is_null() {
            value = xml_node_list_get_string((*prop).doc, (*prop).children, 1);
            oldvalue = (*(*ctxt).state).value;
            oldseq = (*(*ctxt).state).seq;
            (*(*ctxt).state).seq = prop as _;
            (*(*ctxt).state).value = value;
            ret = xml_relaxng_validate_value_content(ctxt, (*define).content);
            if !(*(*ctxt).state).value.is_null() {
                value = (*(*ctxt).state).value;
            }
            if !value.is_null() {
                xml_free(value as _);
            }
            (*(*ctxt).state).value = oldvalue;
            (*(*ctxt).state).seq = oldseq;
            if ret == 0 {
                /*
                 * flag the attribute as processed
                 */
                *(*(*ctxt).state).attrs.add(j as usize) = null_mut();
                (*(*ctxt).state).nb_attr_left -= 1;
            }
        } else {
            ret = -1;
        }
    }

    ret
}

/**
 * xmlRelaxNGValidateAttributeList:
 * @ctxt:  a Relax-NG validation context
 * @define:  the list of definition to verify
 *
 * Validate the given node against the list of attribute definitions
 *
 * Returns 0 if the validation succeeded or an error code.
 */
unsafe extern "C" fn xml_relaxng_validate_attribute_list(
    ctxt: XmlRelaxNGValidCtxtPtr,
    defines: XmlRelaxNGDefinePtr,
) -> i32 {
    let mut ret: i32 = 0;
    let mut res: i32;
    let mut needmore: i32 = 0;
    let mut cur: XmlRelaxNGDefinePtr;

    cur = defines;
    while !cur.is_null() {
        if (*cur).typ == XmlRelaxNGType::Attribute {
            if xml_relaxng_validate_attribute(ctxt, cur) != 0 {
                ret = -1;
            }
        } else {
            needmore = 1;
        }
        cur = (*cur).next;
    }
    if needmore == 0 {
        return ret;
    }
    cur = defines;
    while !cur.is_null() {
        if (*cur).typ != XmlRelaxNGType::Attribute {
            if !(*ctxt).state.is_null() || !(*ctxt).states.is_null() {
                res = xml_relaxng_validate_definition(ctxt, cur);
                if res < 0 {
                    ret = -1;
                }
            } else {
                VALID_ERR!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrNostate);
                return -1;
            }
            if res == -1 {
                /* continues on -2 */
                break;
            }
        }
        cur = (*cur).next;
    }

    ret
}

/**
* xmlRelaxNGValidateCompiledCallback:
* @exec:  the regular expression instance
* @token:  the token which matched
* @transdata:  callback data, the define for the subelement if available
* @inputdata:  callback data, the Relax NG validation context
*
* Handle the callback and if needed validate the element children.
*/
unsafe extern "C" fn xml_relaxng_validate_compiled_callback(
    _exec: XmlRegExecCtxtPtr,
    token: *const XmlChar,
    transdata: *mut c_void,
    inputdata: *mut c_void,
) {
    let ctxt: XmlRelaxNGValidCtxtPtr = inputdata as _;
    let define: XmlRelaxNGDefinePtr = transdata as _;

    if ctxt.is_null() {
        let token = CStr::from_ptr(token as *const i8).to_string_lossy();
        eprintln!("callback on {token} missing context");
        return;
    }
    if define.is_null() {
        if *token.add(0) == b'#' {
            return;
        }
        let token = CStr::from_ptr(token as *const i8).to_string_lossy();
        eprintln!("callback on {token} missing define");
        if !ctxt.is_null() && (*ctxt).err_no == XmlRelaxNGValidErr::XmlRelaxngOk as i32 {
            (*ctxt).err_no = XmlRelaxNGValidErr::XmlRelaxngErrInternal as i32;
        }
        return;
    }
    if (*define).typ != XmlRelaxNGType::Element {
        let token = CStr::from_ptr(token as *const i8).to_string_lossy();
        eprintln!("callback on {token} define is not element");
        if (*ctxt).err_no == XmlRelaxNGValidErr::XmlRelaxngOk as i32 {
            (*ctxt).err_no = XmlRelaxNGValidErr::XmlRelaxngErrInternal as i32;
        }
        return;
    }
    let ret: i32 = xml_relaxng_validate_definition(ctxt, define);
    if ret != 0 {
        (*ctxt).perr = ret;
    }
}

/**
 * xmlRelaxNGValidateCompiledContent:
 * @ctxt:  the RelaxNG validation context
 * @regexp:  the regular expression as compiled
 * @content:  list of children to test against the regexp
 *
 * Validate the content model of an element or start using the regexp
 *
 * Returns 0 in case of success, -1 in case of error.
 */
unsafe extern "C" fn xml_relaxng_validate_compiled_content(
    ctxt: XmlRelaxNGValidCtxtPtr,
    regexp: XmlRegexpPtr,
    content: XmlNodePtr,
) -> i32 {
    let mut cur: XmlNodePtr;
    let mut ret: i32 = 0;

    if ctxt.is_null() || regexp.is_null() {
        return -1;
    }
    let oldperr: i32 = (*ctxt).perr;
    let exec: XmlRegExecCtxtPtr = xml_reg_new_exec_ctxt(
        regexp,
        Some(xml_relaxng_validate_compiled_callback),
        ctxt as _,
    );
    (*ctxt).perr = 0;
    cur = content;
    while !cur.is_null() {
        (*(*ctxt).state).seq = cur;
        match (*cur).typ {
            XmlElementType::XmlTextNode | XmlElementType::XmlCdataSectionNode => {
                if (*cur).is_blank_node() {
                    // break;
                } else {
                    ret = xml_reg_exec_push_string(exec, c"#text".as_ptr() as _, ctxt as _);
                    if ret < 0 {
                        VALID_ERR2!(
                            ctxt,
                            XmlRelaxNGValidErr::XmlRelaxngErrTextwrong,
                            (*(*cur).parent).name
                        );
                    }
                }
            }
            XmlElementType::XmlElementNode => {
                if !(*cur).ns.is_null() {
                    ret = xml_reg_exec_push_string2(
                        exec,
                        (*cur).name,
                        (*(*cur).ns).href.load(Ordering::Relaxed),
                        ctxt as _,
                    );
                } else {
                    ret = xml_reg_exec_push_string(exec, (*cur).name, ctxt as _);
                }
                if ret < 0 {
                    VALID_ERR2!(
                        ctxt,
                        XmlRelaxNGValidErr::XmlRelaxngErrElemwrong,
                        (*cur).name
                    );
                }
            }
            _ => {}
        }
        if ret < 0 {
            break;
        }
        /*
         * Switch to next element
         */
        cur = (*cur).next;
    }
    ret = xml_reg_exec_push_string(exec, null_mut(), null_mut());
    if ret == 1 {
        ret = 0;
        (*(*ctxt).state).seq = null_mut();
    } else if ret == 0 {
        /*
         * TODO: get some of the names needed to exit the current state of exec
         */
        VALID_ERR2!(
            ctxt,
            XmlRelaxNGValidErr::XmlRelaxngErrNoelem,
            c"".as_ptr() as _
        );
        ret = -1;
        if (*ctxt).flags & FLAGS_IGNORABLE == 0 {
            xml_relaxng_dump_valid_error(ctxt);
        }
    } else {
        ret = -1;
    }
    xml_reg_free_exec_ctxt(exec);
    /*
     * There might be content model errors outside of the pure
     * regexp validation, e.g. for attribute values.
     */
    if ret == 0 && (*ctxt).perr != 0 {
        ret = (*ctxt).perr;
    }
    (*ctxt).perr = oldperr;
    ret
}

/**
 * xmlRelaxNGValidateElementEnd:
 * @ctxt:  a Relax-NG validation context
 * @dolog:  indicate that error logging should be done
 *
 * Validate the end of the element, implements check that
 * there is nothing left not consumed in the element content
 * or in the attribute list.
 *
 * Returns 0 if the validation succeeded or an error code.
 */
unsafe extern "C" fn xml_relaxng_validate_element_end(
    ctxt: XmlRelaxNGValidCtxtPtr,
    dolog: i32,
) -> i32 {
    let state: XmlRelaxNGValidStatePtr = (*ctxt).state;
    if !(*state).seq.is_null() {
        (*state).seq = xml_relaxng_skip_ignored(ctxt, (*state).seq);
        if !(*state).seq.is_null() {
            if dolog != 0 {
                VALID_ERR3!(
                    ctxt,
                    XmlRelaxNGValidErr::XmlRelaxngErrExtracontent,
                    (*(*state).node).name,
                    (*(*state).seq).name
                );
            }
            return -1;
        }
    }
    for i in 0..(*state).nb_attrs {
        if !(*(*state).attrs.add(i as usize)).is_null() {
            if dolog != 0 {
                VALID_ERR3!(
                    ctxt,
                    XmlRelaxNGValidErr::XmlRelaxngErrInvalidattr,
                    (*(*(*state).attrs.add(i as usize))).name,
                    (*(*state).node).name
                );
            }
            return -1 - i;
        }
    }
    0
}

/**
 * xmlRelaxNGBestState:
 * @ctxt:  a Relax-NG validation context
 *
 * Find the "best" state in the (*ctxt).states list of states to report
 * errors about. I.e. a state with no element left in the child list
 * or the one with the less attributes left.
 * This is called only if a validation error was detected
 *
 * Returns the index of the "best" state or -1 in case of error
 */
unsafe extern "C" fn xml_relaxng_best_state(ctxt: XmlRelaxNGValidCtxtPtr) -> i32 {
    let mut state: XmlRelaxNGValidStatePtr;

    let mut tmp: i32;
    let mut best: i32 = -1;
    let mut value: i32 = 1000000;

    if ctxt.is_null() || (*ctxt).states.is_null() || (*(*ctxt).states).nb_state <= 0 {
        return -1;
    }

    for i in 0..(*(*ctxt).states).nb_state {
        state = *(*(*ctxt).states).tab_state.add(i as usize);
        if state.is_null() {
            continue;
        }
        if !(*state).seq.is_null() {
            if best == -1 || value > 100000 {
                value = 100000;
                best = i;
            }
        } else {
            tmp = (*state).nb_attr_left;
            if best == -1 || value > tmp {
                value = tmp;
                best = i;
            }
        }
    }
    best
}

/**
 * xmlRelaxNGLogBestError:
 * @ctxt:  a Relax-NG validation context
 *
 * Find the "best" state in the (*ctxt).states list of states to report
 * errors about and log it.
 */
unsafe extern "C" fn xml_relaxng_log_best_error(ctxt: XmlRelaxNGValidCtxtPtr) {
    if ctxt.is_null() || (*ctxt).states.is_null() || (*(*ctxt).states).nb_state <= 0 {
        return;
    }

    let best: i32 = xml_relaxng_best_state(ctxt);
    if best >= 0 && best < (*(*ctxt).states).nb_state {
        (*ctxt).state = *(*(*ctxt).states).tab_state.add(best as usize);

        xml_relaxng_validate_element_end(ctxt, 1);
    }
}

/**
 * xmlRelaxNGValidateDefinitionList:
 * @ctxt:  a Relax-NG validation context
 * @define:  the list of definition to verify
 *
 * Validate the given node content against the (list) of definitions
 *
 * Returns 0 if the validation succeeded or an error code.
 */
unsafe extern "C" fn xml_relaxng_validate_definition_list(
    ctxt: XmlRelaxNGValidCtxtPtr,
    mut defines: XmlRelaxNGDefinePtr,
) -> i32 {
    let mut ret: i32 = 0;
    let mut res: i32;

    if defines.is_null() {
        VALID_ERR2!(
            ctxt,
            XmlRelaxNGValidErr::XmlRelaxngErrInternal,
            c"NULL definition list".as_ptr() as _
        );
        return -1;
    }
    while !defines.is_null() {
        if !(*ctxt).state.is_null() || !(*ctxt).states.is_null() {
            res = xml_relaxng_validate_definition(ctxt, defines);
            if res < 0 {
                ret = -1;
            }
        } else {
            VALID_ERR!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrNostate);
            return -1;
        }
        if res == -1 {
            /* continues on -2 */
            break;
        }
        defines = (*defines).next;
    }

    ret
}

/**
 * xmlRelaxNGCopyValidState:
 * @ctxt:  a Relax-NG validation context
 * @state:  a validation state
 *
 * Copy the validation state
 *
 * Returns the newly allocated structure or NULL in case or error
 */
unsafe extern "C" fn xml_relaxng_copy_valid_state(
    ctxt: XmlRelaxNGValidCtxtPtr,
    state: XmlRelaxNGValidStatePtr,
) -> XmlRelaxNGValidStatePtr {
    let ret: XmlRelaxNGValidStatePtr;

    if state.is_null() {
        return null_mut();
    }
    if !(*ctxt).free_state.is_null() && (*(*ctxt).free_state).nb_state > 0 {
        (*(*ctxt).free_state).nb_state -= 1;
        ret = *(*(*ctxt).free_state)
            .tab_state
            .add((*(*ctxt).free_state).nb_state as usize);
    } else {
        ret = xml_malloc(size_of::<XmlRelaxNGValidState>()) as _;
        if ret.is_null() {
            xml_rng_verr_memory(ctxt, c"allocating states\n".as_ptr() as _);
            return null_mut();
        }
        memset(ret as _, 0, size_of::<XmlRelaxNGValidState>());
    }
    let attrs: *mut XmlAttrPtr = (*ret).attrs;
    let max_attrs: u32 = (*ret).max_attrs as _;
    memcpy(ret as _, state as _, size_of::<XmlRelaxNGValidState>());
    (*ret).attrs = attrs;
    (*ret).max_attrs = max_attrs as _;
    if (*state).nb_attrs > 0 {
        if (*ret).attrs.is_null() {
            (*ret).max_attrs = (*state).max_attrs;
            (*ret).attrs = xml_malloc((*ret).max_attrs as usize * size_of::<XmlAttrPtr>()) as _;
            if (*ret).attrs.is_null() {
                xml_rng_verr_memory(ctxt, c"allocating states\n".as_ptr() as _);
                (*ret).nb_attrs = 0;
                return ret;
            }
        } else if (*ret).max_attrs < (*state).nb_attrs {
            let tmp: *mut XmlAttrPtr = xml_realloc(
                (*ret).attrs as _,
                (*state).max_attrs as usize * size_of::<XmlAttrPtr>(),
            ) as _;
            if tmp.is_null() {
                xml_rng_verr_memory(ctxt, c"allocating states\n".as_ptr() as _);
                (*ret).nb_attrs = 0;
                return ret;
            }
            (*ret).max_attrs = (*state).max_attrs;
            (*ret).attrs = tmp;
        }
        memcpy(
            (*ret).attrs as _,
            (*state).attrs as _,
            (*state).nb_attrs as usize * size_of::<XmlAttrPtr>(),
        );
    }
    ret
}

/**
 * xmlRelaxNGEqualValidState:
 * @ctxt:  a Relax-NG validation context
 * @state1:  a validation state
 * @state2:  a validation state
 *
 * Compare the validation states for equality
 *
 * Returns 1 if equal, 0 otherwise
 */
unsafe extern "C" fn xml_relaxng_equal_valid_state(
    _ctxt: XmlRelaxNGValidCtxtPtr,
    state1: XmlRelaxNGValidStatePtr,
    state2: XmlRelaxNGValidStatePtr,
) -> i32 {
    if state1.is_null() || state2.is_null() {
        return 0;
    }
    if state1 == state2 {
        return 1;
    }
    if (*state1).node != (*state2).node {
        return 0;
    }
    if (*state1).seq != (*state2).seq {
        return 0;
    }
    if (*state1).nb_attr_left != (*state2).nb_attr_left {
        return 0;
    }
    if (*state1).nb_attrs != (*state2).nb_attrs {
        return 0;
    }
    if (*state1).endvalue != (*state2).endvalue {
        return 0;
    }
    if (*state1).value != (*state2).value && !xml_str_equal((*state1).value, (*state2).value) {
        return 0;
    }
    for i in 0..(*state1).nb_attrs {
        if *(*state1).attrs.add(i as usize) != *(*state2).attrs.add(i as usize) {
            return 0;
        }
    }
    1
}

/**
 * xmlRelaxNGAddState:
 * @ctxt:  a Relax-NG validation context
 * @states:  the states container
 * @state:  the validation state
 *
 * Add a RelaxNG validation state to the container
 *
 * Return 1 in case of success and 0 if this is a duplicate and -1 on error
 */
unsafe extern "C" fn xml_relaxng_add_states(
    ctxt: XmlRelaxNGValidCtxtPtr,
    states: XmlRelaxNGStatesPtr,
    state: XmlRelaxNGValidStatePtr,
) -> i32 {
    if state.is_null() || states.is_null() {
        return -1;
    }
    if (*states).nb_state >= (*states).max_state {
        let size: i32 = (*states).max_state * 2;
        let tmp: *mut XmlRelaxNGValidStatePtr = xml_realloc(
            (*states).tab_state as _,
            size as usize * size_of::<XmlRelaxNGValidStatePtr>(),
        ) as _;
        if tmp.is_null() {
            xml_rng_verr_memory(ctxt, c"adding states\n".as_ptr() as _);
            return -1;
        }
        (*states).tab_state = tmp;
        (*states).max_state = size;
    }
    for i in 0..(*states).nb_state {
        if xml_relaxng_equal_valid_state(ctxt, state, *(*states).tab_state.add(i as usize)) != 0 {
            xml_relaxng_free_valid_state(ctxt, state);
            return 0;
        }
    }
    *(*states).tab_state.add((*states).nb_state as usize) = state;
    (*states).nb_state += 1;
    1
}

/**
 * xmlRelaxNGNodeMatchesList:
 * @node:  the node
 * @list:  a NULL terminated array of definitions
 *
 * Check if a node can be matched by one of the definitions
 *
 * Returns 1 if matches 0 otherwise
 */
unsafe extern "C" fn xml_relaxng_node_matches_list(
    node: XmlNodePtr,
    list: *mut XmlRelaxNGDefinePtr,
) -> i32 {
    let mut cur: XmlRelaxNGDefinePtr;
    let mut i: i32 = 0;
    let mut tmp: i32;

    if node.is_null() || list.is_null() {
        return 0;
    }

    cur = *list.add(i as usize);
    i += 1;
    while !cur.is_null() {
        if (*node).typ == XmlElementType::XmlElementNode && (*cur).typ == XmlRelaxNGType::Element {
            tmp = xml_relaxng_element_match(null_mut(), cur, node);
            if tmp == 1 {
                return 1;
            }
        } else if matches!(
            (*node).typ,
            XmlElementType::XmlTextNode | XmlElementType::XmlCdataSectionNode
        ) && matches!(
            (*cur).typ,
            XmlRelaxNGType::Datatype
                | XmlRelaxNGType::List
                | XmlRelaxNGType::Text
                | XmlRelaxNGType::Value
        ) {
            return 1;
        }
        cur = *list.add(i as usize);
        i += 1;
    }
    0
}

/**
 * xmlRelaxNGValidateInterleave:
 * @ctxt:  a Relax-NG validation context
 * @define:  the definition to verify
 *
 * Validate an interleave definition for a node.
 *
 * Returns 0 if the validation succeeded or an error code.
 */
unsafe extern "C" fn xml_relaxng_validate_interleave(
    ctxt: XmlRelaxNGValidCtxtPtr,
    define: XmlRelaxNGDefinePtr,
) -> i32 {
    let mut ret: i32 = 0;
    let mut i: i32;
    let nbgroups: i32;
    let err_nr: i32 = (*ctxt).err_nr;
    let mut oldstate: XmlRelaxNGValidStatePtr;
    let partitions: XmlRelaxNGPartitionPtr;
    let mut group: XmlRelaxNGInterleaveGroupPtr;
    let mut cur: XmlNodePtr;
    let mut last: XmlNodePtr = null_mut();
    let mut lastchg: XmlNodePtr = null_mut();
    let lastelem: XmlNodePtr;

    if !(*define).data.is_null() {
        partitions = (*define).data as _;
        nbgroups = (*partitions).nbgroups;
    } else {
        VALID_ERR!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrInternodata);
        return -1;
    }
    /*
     * Optimizations for MIXED
     */
    let oldflags: i32 = (*ctxt).flags;
    if (*define).dflags & IS_MIXED as i16 != 0 {
        (*ctxt).flags |= FLAGS_MIXED_CONTENT;
        if nbgroups == 2 {
            /*
             * this is a pure <mixed> case
             */
            if !(*ctxt).state.is_null() {
                (*(*ctxt).state).seq = xml_relaxng_skip_ignored(ctxt, (*(*ctxt).state).seq);
            }
            if (*(*(*(*partitions).groups.add(0))).rule).typ == XmlRelaxNGType::Text {
                ret = xml_relaxng_validate_definition(ctxt, (*(*(*partitions).groups.add(1))).rule);
            } else {
                ret = xml_relaxng_validate_definition(ctxt, (*(*(*partitions).groups.add(0))).rule);
            }
            if ret == 0 && !(*ctxt).state.is_null() {
                (*(*ctxt).state).seq = xml_relaxng_skip_ignored(ctxt, (*(*ctxt).state).seq);
            }
            (*ctxt).flags = oldflags;
            return ret;
        }
    }

    /*
     * Build arrays to store the first and last node of the chain
     * pertaining to each group
     */
    let list: *mut XmlNodePtr = xml_malloc(nbgroups as usize * size_of::<XmlNodePtr>()) as _;
    if list.is_null() {
        xml_rng_verr_memory(ctxt, c"validating\n".as_ptr() as _);
        return -1;
    }
    memset(list as _, 0, nbgroups as usize * size_of::<XmlNodePtr>());
    let lasts: *mut XmlNodePtr = xml_malloc(nbgroups as usize * size_of::<XmlNodePtr>()) as _;
    if lasts.is_null() {
        xml_rng_verr_memory(ctxt, c"validating\n".as_ptr() as _);
        return -1;
    }
    memset(lasts as _, 0, nbgroups as usize * size_of::<XmlNodePtr>());

    /*
     * Walk the sequence of children finding the right group and
     * sorting them in sequences.
     */
    cur = (*(*ctxt).state).seq;
    cur = xml_relaxng_skip_ignored(ctxt, cur);
    let start: XmlNodePtr = cur;
    while !cur.is_null() {
        (*(*ctxt).state).seq = cur;
        if !(*partitions).triage.is_null() && (*partitions).flags & IS_DETERMINIST != 0 {
            let mut tmp: *mut c_void = null_mut();

            if matches!(
                (*cur).typ,
                XmlElementType::XmlTextNode | XmlElementType::XmlCdataSectionNode
            ) {
                tmp = xml_hash_lookup2((*partitions).triage, c"#text".as_ptr() as _, null_mut());
            } else if (*cur).typ == XmlElementType::XmlElementNode {
                if !(*cur).ns.is_null() {
                    tmp = xml_hash_lookup2(
                        (*partitions).triage,
                        (*cur).name,
                        (*(*cur).ns).href.load(Ordering::Relaxed),
                    );
                    if tmp.is_null() {
                        tmp = xml_hash_lookup2(
                            (*partitions).triage,
                            c"#any".as_ptr() as _,
                            (*(*cur).ns).href.load(Ordering::Relaxed),
                        );
                    }
                } else {
                    tmp = xml_hash_lookup2((*partitions).triage, (*cur).name, null_mut());
                }
                if tmp.is_null() {
                    tmp = xml_hash_lookup2((*partitions).triage, c"#any".as_ptr() as _, null_mut());
                }
            }

            if tmp.is_null() {
                i = nbgroups;
            } else {
                i = (tmp as ptrdiff_t - 1) as _;
                if (*partitions).flags & IS_NEEDCHECK != 0 {
                    group = *(*partitions).groups.add(i as usize);
                    if xml_relaxng_node_matches_list(cur, (*group).defs) == 0 {
                        i = nbgroups;
                    }
                }
            }
        } else {
            i = 0;
            'main: while i < nbgroups {
                'to_continue: {
                    group = *(*partitions).groups.add(i as usize);
                    if group.is_null() {
                        break 'to_continue;
                    }
                    if xml_relaxng_node_matches_list(cur, (*group).defs) != 0 {
                        break 'main;
                    }
                }
                i += 1;
            }
        }
        /*
         * We break as soon as an element not matched is found
         */
        if i >= nbgroups {
            break;
        }
        if !(*lasts.add(i as usize)).is_null() {
            (*(*lasts.add(i as usize))).next = cur;
            *lasts.add(i as usize) = cur;
        } else {
            *list.add(i as usize) = cur;
            *lasts.add(i as usize) = cur;
        }
        if !(*cur).next.is_null() {
            lastchg = (*cur).next;
        } else {
            lastchg = cur;
        }
        cur = xml_relaxng_skip_ignored(ctxt, (*cur).next);
    }
    'done: {
        if ret != 0 {
            VALID_ERR!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrInterseq);
            ret = -1;
            break 'done;
        }

        lastelem = cur;
        oldstate = (*ctxt).state;
        for i in 0..nbgroups {
            (*ctxt).state = xml_relaxng_copy_valid_state(ctxt, oldstate);
            if (*ctxt).state.is_null() {
                ret = -1;
                break;
            }
            group = *(*partitions).groups.add(i as usize);
            if !(*lasts.add(i as usize)).is_null() {
                last = (*(*lasts.add(i as usize))).next;
                (*(*lasts.add(i as usize))).next = null_mut();
            }
            (*(*ctxt).state).seq = *list.add(i as usize);
            ret = xml_relaxng_validate_definition(ctxt, (*group).rule);
            if ret != 0 {
                break;
            }
            if !(*ctxt).state.is_null() {
                cur = (*(*ctxt).state).seq;
                cur = xml_relaxng_skip_ignored(ctxt, cur);
                xml_relaxng_free_valid_state(ctxt, oldstate);
                oldstate = (*ctxt).state;
                (*ctxt).state = null_mut();
                if !cur.is_null()
                        /* there's a nasty violation of context-free unambiguities,
                           since in open-name-class context, interleave in the
                           production shall finish without caring about anything
                           else that is OK to follow in that case -- it would
                           otherwise get marked as "extra content" and would
                           hence fail the validation, hence this perhaps
                           dirty attempt to rectify such a situation */
                        && ((*(*define).parent).typ != XmlRelaxNGType::Def
                            || !xml_str_equal((*(*define).parent).name,
                                            c"open-name-class".as_ptr() as _))
                {
                    VALID_ERR2!(
                        ctxt,
                        XmlRelaxNGValidErr::XmlRelaxngErrInterextra,
                        (*cur).name
                    );
                    ret = -1;
                    (*ctxt).state = oldstate;
                    break 'done;
                }
            } else if !(*ctxt).states.is_null() {
                let mut found: i32 = 0;
                let mut best: i32 = -1;
                let mut lowattr: i32 = -1;

                /*
                 * PBM: what happen if there is attributes checks in the interleaves
                 */
                for j in 0..(*(*ctxt).states).nb_state {
                    cur = (*(*(*(*ctxt).states).tab_state.add(j as usize))).seq;
                    cur = xml_relaxng_skip_ignored(ctxt, cur);
                    if cur.is_null() {
                        if found == 0 {
                            lowattr =
                                (*(*(*(*ctxt).states).tab_state.add(j as usize))).nb_attr_left;
                            best = j;
                        }
                        found = 1;
                        if (*(*(*(*ctxt).states).tab_state.add(j as usize))).nb_attr_left <= lowattr
                        {
                            /* try  to keep the latest one to mach old heuristic */
                            lowattr =
                                (*(*(*(*ctxt).states).tab_state.add(j as usize))).nb_attr_left;
                            best = j;
                        }
                        if lowattr == 0 {
                            break;
                        }
                    } else if found == 0 {
                        if lowattr == -1 {
                            lowattr =
                                (*(*(*(*ctxt).states).tab_state.add(j as usize))).nb_attr_left;
                            best = j;
                        } else if (*(*(*(*ctxt).states).tab_state.add(j as usize))).nb_attr_left
                            <= lowattr
                        {
                            /* try  to keep the latest one to mach old heuristic */
                            lowattr =
                                (*(*(*(*ctxt).states).tab_state.add(j as usize))).nb_attr_left;
                            best = j;
                        }
                    }
                }
                /*
                 * BIG PBM: here we pick only one restarting point :-(
                 */
                if (*(*ctxt).states).nb_state > 0 {
                    xml_relaxng_free_valid_state(ctxt, oldstate);
                    if best != -1 {
                        oldstate = *(*(*ctxt).states).tab_state.add(best as usize);
                        *(*(*ctxt).states).tab_state.add(best as usize) = null_mut();
                    } else {
                        oldstate = *(*(*ctxt).states)
                            .tab_state
                            .add((*(*ctxt).states).nb_state as usize - 1);
                        *(*(*ctxt).states)
                            .tab_state
                            .add((*(*ctxt).states).nb_state as usize - 1) = null_mut();
                        (*(*ctxt).states).nb_state -= 1;
                    }
                }
                for j in 0..(*(*ctxt).states).nb_state {
                    xml_relaxng_free_valid_state(
                        ctxt,
                        *(*(*ctxt).states).tab_state.add(j as usize),
                    );
                }
                xml_relaxng_free_states(ctxt, (*ctxt).states);
                (*ctxt).states = null_mut();
                if found == 0 {
                    if cur.is_null() {
                        VALID_ERR2!(
                            ctxt,
                            XmlRelaxNGValidErr::XmlRelaxngErrInterextra,
                            c"noname".as_ptr() as _
                        );
                    } else {
                        VALID_ERR2!(
                            ctxt,
                            XmlRelaxNGValidErr::XmlRelaxngErrInterextra,
                            (*cur).name
                        );
                    }
                    ret = -1;
                    (*ctxt).state = oldstate;
                    break 'done;
                }
            } else {
                ret = -1;
                break;
            }
            if !(*lasts.add(i as usize)).is_null() {
                (*(*lasts.add(i as usize))).next = last;
            }
        }
        if !(*ctxt).state.is_null() {
            xml_relaxng_free_valid_state(ctxt, (*ctxt).state);
        }
        (*ctxt).state = oldstate;
        (*(*ctxt).state).seq = lastelem;
        if ret != 0 {
            VALID_ERR!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrInterseq);
            ret = -1;
            break 'done;
        }
    }

    //   done:
    (*ctxt).flags = oldflags;
    /*
     * builds the next links chain from the prev one
     */
    cur = lastchg;
    while !cur.is_null() {
        if cur == start || (*cur).prev.is_null() {
            break;
        }
        (*(*cur).prev).next = cur;
        cur = (*cur).prev;
    }
    if ret == 0 && (*ctxt).err_nr > err_nr {
        xml_relaxng_pop_errors(ctxt, err_nr);
    }

    xml_free(list as _);
    xml_free(lasts as _);
    ret
}

/**
 * xmlRelaxNGValidateState:
 * @ctxt:  a Relax-NG validation context
 * @define:  the definition to verify
 *
 * Validate the current state against the definition
 *
 * Returns 0 if the validation succeeded or an error code.
 */
unsafe extern "C" fn xml_relaxng_validate_state(
    ctxt: XmlRelaxNGValidCtxtPtr,
    define: XmlRelaxNGDefinePtr,
) -> i32 {
    let mut node: XmlNodePtr;
    let mut ret: i32 = 0;

    let mut tmp: i32;
    let oldflags: i32;
    let mut err_nr: i32;
    let mut oldstate: XmlRelaxNGValidStatePtr = null_mut();
    let mut state: XmlRelaxNGValidStatePtr;

    if define.is_null() {
        VALID_ERR!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrNodefine);
        return -1;
    }

    if !(*ctxt).state.is_null() {
        node = (*(*ctxt).state).seq;
    } else {
        node = null_mut();
    }
    (*ctxt).depth += 1;
    match (*define).typ {
        XmlRelaxNGType::Empty => {
            ret = 0;
        }
        XmlRelaxNGType::NotAllowed => {
            ret = -1;
        }
        XmlRelaxNGType::Text => {
            while !node.is_null()
                && matches!(
                    (*node).typ,
                    XmlElementType::XmlTextNode
                        | XmlElementType::XmlCommentNode
                        | XmlElementType::XmlPiNode
                        | XmlElementType::XmlCdataSectionNode
                )
            {
                node = (*node).next;
            }
            (*(*ctxt).state).seq = node;
        }
        XmlRelaxNGType::Element => 'to_break: {
            err_nr = (*ctxt).err_nr;
            node = xml_relaxng_skip_ignored(ctxt, node);
            if node.is_null() {
                VALID_ERR2!(
                    ctxt,
                    XmlRelaxNGValidErr::XmlRelaxngErrNoelem,
                    (*define).name
                );
                ret = -1;
                if (*ctxt).flags & FLAGS_IGNORABLE == 0 {
                    xml_relaxng_dump_valid_error(ctxt);
                }
                break 'to_break;
            }
            if (*node).typ != XmlElementType::XmlElementNode {
                VALID_ERR!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrNotelem);
                ret = -1;
                if (*ctxt).flags & FLAGS_IGNORABLE == 0 {
                    xml_relaxng_dump_valid_error(ctxt);
                }
                break 'to_break;
            }
            /*
             * This node was already validated successfully against
             * this definition.
             */
            if (*node).psvi == define as _ {
                (*(*ctxt).state).seq = xml_relaxng_skip_ignored(ctxt, (*node).next);
                if (*ctxt).err_nr > err_nr {
                    xml_relaxng_pop_errors(ctxt, err_nr);
                }
                if (*ctxt).err_nr != 0 {
                    while !(*ctxt).err.is_null()
                        && (((*(*ctxt).err).err == XmlRelaxNGValidErr::XmlRelaxngErrElemname
                            && xml_str_equal((*(*ctxt).err).arg2, (*node).name))
                            || ((*(*ctxt).err).err == XmlRelaxNGValidErr::XmlRelaxngErrElemextrans
                                && xml_str_equal((*(*ctxt).err).arg1, (*node).name))
                            || (*(*ctxt).err).err == XmlRelaxNGValidErr::XmlRelaxngErrNoelem
                            || (*(*ctxt).err).err == XmlRelaxNGValidErr::XmlRelaxngErrNotelem)
                    {
                        xml_relaxng_valid_error_pop(ctxt);
                    }
                }
                break 'to_break;
            }
            ret = xml_relaxng_element_match(ctxt, define, node);
            if ret <= 0 {
                ret = -1;
                if (*ctxt).flags & FLAGS_IGNORABLE == 0 {
                    xml_relaxng_dump_valid_error(ctxt);
                }
                break 'to_break;
            }
            ret = 0;
            if (*ctxt).err_nr != 0 {
                if (*ctxt).err_nr > err_nr {
                    xml_relaxng_pop_errors(ctxt, err_nr);
                }
                while !(*ctxt).err.is_null()
                    && (((*(*ctxt).err).err == XmlRelaxNGValidErr::XmlRelaxngErrElemname
                        && xml_str_equal((*(*ctxt).err).arg2, (*node).name))
                        || ((*(*ctxt).err).err == XmlRelaxNGValidErr::XmlRelaxngErrElemextrans
                            && xml_str_equal((*(*ctxt).err).arg1, (*node).name))
                        || (*(*ctxt).err).err == XmlRelaxNGValidErr::XmlRelaxngErrNoelem
                        || (*(*ctxt).err).err == XmlRelaxNGValidErr::XmlRelaxngErrNotelem)
                {
                    xml_relaxng_valid_error_pop(ctxt);
                }
            }
            err_nr = (*ctxt).err_nr;

            oldflags = (*ctxt).flags;
            if (*ctxt).flags & FLAGS_MIXED_CONTENT != 0 {
                (*ctxt).flags -= FLAGS_MIXED_CONTENT;
            }
            state = xml_relaxng_new_valid_state(ctxt, node);
            if state.is_null() {
                ret = -1;
                if (*ctxt).flags & FLAGS_IGNORABLE == 0 {
                    xml_relaxng_dump_valid_error(ctxt);
                }
                break 'to_break;
            }
            oldstate = (*ctxt).state;
            (*ctxt).state = state;
            if !(*define).attrs.is_null() {
                tmp = xml_relaxng_validate_attribute_list(ctxt, (*define).attrs);
                if tmp != 0 {
                    ret = -1;
                    VALID_ERR2!(
                        ctxt,
                        XmlRelaxNGValidErr::XmlRelaxngErrAttrvalid,
                        (*node).name
                    );
                }
            }
            if !(*define).cont_model.is_null() {
                let tmpstate: XmlRelaxNGValidStatePtr = (*ctxt).state;
                let tmpstates: XmlRelaxNGStatesPtr = (*ctxt).states;

                let nstate: XmlRelaxNGValidStatePtr = xml_relaxng_new_valid_state(ctxt, node);
                (*ctxt).state = nstate;
                (*ctxt).states = null_mut();

                tmp = xml_relaxng_validate_compiled_content(
                    ctxt,
                    (*define).cont_model,
                    (*(*ctxt).state).seq,
                );
                let nseq: XmlNodePtr = (*(*ctxt).state).seq;
                (*ctxt).state = tmpstate;
                (*ctxt).states = tmpstates;
                xml_relaxng_free_valid_state(ctxt, nstate);

                if tmp != 0 {
                    ret = -1;
                }

                if !(*ctxt).states.is_null() {
                    tmp = -1;

                    for i in 0..(*(*ctxt).states).nb_state {
                        state = *(*(*ctxt).states).tab_state.add(i as usize);
                        (*ctxt).state = state;
                        (*(*ctxt).state).seq = nseq;

                        if xml_relaxng_validate_element_end(ctxt, 0) == 0 {
                            tmp = 0;
                            break;
                        }
                    }
                    if tmp != 0 {
                        /*
                         * validation error, log the message for the "best" one
                         */
                        (*ctxt).flags |= FLAGS_IGNORABLE;
                        xml_relaxng_log_best_error(ctxt);
                    }
                    for i in 0..(*(*ctxt).states).nb_state {
                        xml_relaxng_free_valid_state(
                            ctxt,
                            *(*(*ctxt).states).tab_state.add(i as usize),
                        );
                    }
                    xml_relaxng_free_states(ctxt, (*ctxt).states);
                    (*ctxt).flags = oldflags;
                    (*ctxt).states = null_mut();
                    if ret == 0 && tmp == -1 {
                        ret = -1;
                    }
                } else {
                    state = (*ctxt).state;
                    if !(*ctxt).state.is_null() {
                        (*(*ctxt).state).seq = nseq;
                    }
                    if ret == 0 {
                        ret = xml_relaxng_validate_element_end(ctxt, 1);
                    }
                    xml_relaxng_free_valid_state(ctxt, state);
                }
            } else {
                if !(*define).content.is_null() {
                    tmp = xml_relaxng_validate_definition_list(ctxt, (*define).content);
                    if tmp != 0 {
                        ret = -1;
                        if (*ctxt).state.is_null() {
                            (*ctxt).state = oldstate;
                            VALID_ERR2!(
                                ctxt,
                                XmlRelaxNGValidErr::XmlRelaxngErrContentvalid,
                                (*node).name
                            );
                            (*ctxt).state = null_mut();
                        } else {
                            VALID_ERR2!(
                                ctxt,
                                XmlRelaxNGValidErr::XmlRelaxngErrContentvalid,
                                (*node).name
                            );
                        }
                    }
                }
                if !(*ctxt).states.is_null() {
                    tmp = -1;

                    for i in 0..(*(*ctxt).states).nb_state {
                        state = *(*(*ctxt).states).tab_state.add(i as usize);
                        (*ctxt).state = state;

                        if xml_relaxng_validate_element_end(ctxt, 0) == 0 {
                            tmp = 0;
                            break;
                        }
                    }
                    if tmp != 0 {
                        /*
                         * validation error, log the message for the "best" one
                         */
                        (*ctxt).flags |= FLAGS_IGNORABLE;
                        xml_relaxng_log_best_error(ctxt);
                    }
                    for i in 0..(*(*ctxt).states).nb_state {
                        xml_relaxng_free_valid_state(
                            ctxt,
                            *(*(*ctxt).states).tab_state.add(i as usize),
                        );
                        *(*(*ctxt).states).tab_state.add(i as usize) = null_mut();
                    }
                    xml_relaxng_free_states(ctxt, (*ctxt).states);
                    (*ctxt).flags = oldflags;
                    (*ctxt).states = null_mut();
                    if ret == 0 && tmp == -1 {
                        ret = -1;
                    }
                } else {
                    state = (*ctxt).state;
                    if ret == 0 {
                        ret = xml_relaxng_validate_element_end(ctxt, 1);
                    }
                    xml_relaxng_free_valid_state(ctxt, state);
                }
            }
            if ret == 0 {
                (*node).psvi = define as _;
            }
            (*ctxt).flags = oldflags;
            (*ctxt).state = oldstate;
            if !oldstate.is_null() {
                (*oldstate).seq = xml_relaxng_skip_ignored(ctxt, (*node).next);
            }
            if ret != 0 {
                if (*ctxt).flags & FLAGS_IGNORABLE == 0 {
                    xml_relaxng_dump_valid_error(ctxt);
                    ret = 0;
                    // #if 0
                    //                 } else {
                    //                     ret = -2;
                    // #endif
                }
            } else if (*ctxt).err_nr > err_nr {
                xml_relaxng_pop_errors(ctxt, err_nr);
            }
        }
        XmlRelaxNGType::Optional => 'to_break: {
            err_nr = (*ctxt).err_nr;
            oldflags = (*ctxt).flags;
            (*ctxt).flags |= FLAGS_IGNORABLE;
            oldstate = xml_relaxng_copy_valid_state(ctxt, (*ctxt).state);
            ret = xml_relaxng_validate_definition_list(ctxt, (*define).content);
            if ret != 0 {
                if !(*ctxt).state.is_null() {
                    xml_relaxng_free_valid_state(ctxt, (*ctxt).state);
                }
                (*ctxt).state = oldstate;
                (*ctxt).flags = oldflags;
                ret = 0;
                if (*ctxt).err_nr > err_nr {
                    xml_relaxng_pop_errors(ctxt, err_nr);
                }
                break 'to_break;
            }
            if !(*ctxt).states.is_null() {
                xml_relaxng_add_states(ctxt, (*ctxt).states, oldstate);
            } else {
                (*ctxt).states = xml_relaxng_new_states(ctxt, 1);
                if (*ctxt).states.is_null() {
                    xml_relaxng_free_valid_state(ctxt, oldstate);
                    (*ctxt).flags = oldflags;
                    ret = -1;
                    if (*ctxt).err_nr > err_nr {
                        xml_relaxng_pop_errors(ctxt, err_nr);
                    }
                    break 'to_break;
                }
                xml_relaxng_add_states(ctxt, (*ctxt).states, oldstate);
                xml_relaxng_add_states(ctxt, (*ctxt).states, (*ctxt).state);
                (*ctxt).state = null_mut();
            }
            (*ctxt).flags = oldflags;
            ret = 0;
            if (*ctxt).err_nr > err_nr {
                xml_relaxng_pop_errors(ctxt, err_nr);
            }
        }
        ty @ XmlRelaxNGType::Oneormore | ty @ XmlRelaxNGType::Zeroormore => 'to_break: {
            if matches!(ty, XmlRelaxNGType::Oneormore) {
                err_nr = (*ctxt).err_nr;
                ret = xml_relaxng_validate_definition_list(ctxt, (*define).content);
                if ret != 0 {
                    break 'to_break;
                }
                if (*ctxt).err_nr > err_nr {
                    xml_relaxng_pop_errors(ctxt, err_nr);
                }
            }

            let mut progress: i32;
            let mut states: XmlRelaxNGStatesPtr = null_mut();
            let mut base: i32;

            // err_nr = (*ctxt).err_nr;
            let res: XmlRelaxNGStatesPtr = xml_relaxng_new_states(ctxt, 1);
            if res.is_null() {
                ret = -1;
                break 'to_break;
            }
            /*
             * All the input states are also exit states
             */
            if !(*ctxt).state.is_null() {
                xml_relaxng_add_states(
                    ctxt,
                    res,
                    xml_relaxng_copy_valid_state(ctxt, (*ctxt).state),
                );
            } else {
                for j in 0..(*(*ctxt).states).nb_state {
                    xml_relaxng_add_states(
                        ctxt,
                        res,
                        xml_relaxng_copy_valid_state(
                            ctxt,
                            *(*(*ctxt).states).tab_state.add(j as usize),
                        ),
                    );
                }
            }
            oldflags = (*ctxt).flags;
            (*ctxt).flags |= FLAGS_IGNORABLE;
            'lp: while {
                progress = 0;
                base = (*res).nb_state;

                if !(*ctxt).states.is_null() {
                    states = (*ctxt).states;
                    for i in 0..(*states).nb_state {
                        (*ctxt).state = *(*states).tab_state.add(i as usize);
                        (*ctxt).states = null_mut();
                        ret = xml_relaxng_validate_definition_list(ctxt, (*define).content);
                        if ret == 0 {
                            if !(*ctxt).state.is_null() {
                                tmp = xml_relaxng_add_states(ctxt, res, (*ctxt).state);
                                (*ctxt).state = null_mut();
                                if tmp == 1 {
                                    progress = 1;
                                }
                            } else if !(*ctxt).states.is_null() {
                                for j in 0..(*(*ctxt).states).nb_state {
                                    tmp = xml_relaxng_add_states(
                                        ctxt,
                                        res,
                                        *(*(*ctxt).states).tab_state.add(j as usize),
                                    );
                                    if tmp == 1 {
                                        progress = 1;
                                    }
                                }
                                xml_relaxng_free_states(ctxt, (*ctxt).states);
                                (*ctxt).states = null_mut();
                            }
                        } else if !(*ctxt).state.is_null() {
                            xml_relaxng_free_valid_state(ctxt, (*ctxt).state);
                            (*ctxt).state = null_mut();
                        }
                    }
                } else {
                    ret = xml_relaxng_validate_definition_list(ctxt, (*define).content);
                    if ret != 0 {
                        xml_relaxng_free_valid_state(ctxt, (*ctxt).state);
                        (*ctxt).state = null_mut();
                    } else {
                        base = (*res).nb_state;
                        if !(*ctxt).state.is_null() {
                            tmp = xml_relaxng_add_states(ctxt, res, (*ctxt).state);
                            (*ctxt).state = null_mut();
                            if tmp == 1 {
                                progress = 1;
                            }
                        } else if !(*ctxt).states.is_null() {
                            for j in 0..(*(*ctxt).states).nb_state {
                                tmp = xml_relaxng_add_states(
                                    ctxt,
                                    res,
                                    *(*(*ctxt).states).tab_state.add(j as usize),
                                );
                                if tmp == 1 {
                                    progress = 1;
                                }
                            }
                            if states.is_null() {
                                states = (*ctxt).states;
                            } else {
                                xml_relaxng_free_states(ctxt, (*ctxt).states);
                            }
                            (*ctxt).states = null_mut();
                        }
                    }
                }
                if progress != 0 {
                    /*
                     * Collect all the new nodes added at that step
                     * and make them the new node set
                     */
                    if (*res).nb_state - base == 1 {
                        (*ctxt).state = xml_relaxng_copy_valid_state(
                            ctxt,
                            *(*res).tab_state.add(base as usize),
                        );
                    } else {
                        if states.is_null() {
                            xml_relaxng_new_states(ctxt, (*res).nb_state - base);
                            states = (*ctxt).states;
                            if states.is_null() {
                                // progress = 0;
                                break 'lp;
                            }
                        }
                        (*states).nb_state = 0;
                        for i in base..(*res).nb_state {
                            xml_relaxng_add_states(
                                ctxt,
                                states,
                                xml_relaxng_copy_valid_state(
                                    ctxt,
                                    *(*res).tab_state.add(i as usize),
                                ),
                            );
                        }
                        (*ctxt).states = states;
                    }
                }
                progress == 1
            } {}
            if !states.is_null() {
                xml_relaxng_free_states(ctxt, states);
            }
            (*ctxt).states = res;
            (*ctxt).flags = oldflags;
            // #if 0
            //                 /*
            //                  * errors may have to be propagated back...
            //                  */
            //                 if (*ctxt).errNr > errNr {
            //                     xmlRelaxNGPopErrors(ctxt, errNr);
            //                 }
            // #endif
            ret = 0;
        }
        XmlRelaxNGType::Choice => 'to_break: {
            let mut list: XmlRelaxNGDefinePtr = null_mut();
            let mut states: XmlRelaxNGStatesPtr = null_mut();

            node = xml_relaxng_skip_ignored(ctxt, node);

            err_nr = (*ctxt).err_nr;
            if (*define).dflags & IS_TRIABLE as i16 != 0
                && !(*define).data.is_null()
                && !node.is_null()
            {
                /*
                 * node.is_null() can't be optimized since IS_TRIABLE
                 * doesn't account for choice which may lead to
                 * only attributes.
                 */
                let triage: XmlHashTablePtr = (*define).data as _;

                /*
                 * Something we can optimize cleanly there is only one
                 * possible branch out !
                 */
                if matches!(
                    (*node).typ,
                    XmlElementType::XmlTextNode | XmlElementType::XmlCdataSectionNode
                ) {
                    list = xml_hash_lookup2(triage, c"#text".as_ptr() as _, null_mut()) as _;
                } else if (*node).typ == XmlElementType::XmlElementNode {
                    if !(*node).ns.is_null() {
                        list = xml_hash_lookup2(
                            triage,
                            (*node).name,
                            (*(*node).ns).href.load(Ordering::Relaxed),
                        ) as _;
                        if list.is_null() {
                            list = xml_hash_lookup2(
                                triage,
                                c"#any".as_ptr() as _,
                                (*(*node).ns).href.load(Ordering::Relaxed),
                            ) as _;
                        }
                    } else {
                        list = xml_hash_lookup2(triage, (*node).name, null_mut()) as _;
                    }
                    if list.is_null() {
                        list = xml_hash_lookup2(triage, c"#any".as_ptr() as _, null_mut()) as _;
                    }
                }
                if list.is_null() {
                    ret = -1;
                    VALID_ERR2!(
                        ctxt,
                        XmlRelaxNGValidErr::XmlRelaxngErrElemwrong,
                        (*node).name
                    );
                    break 'to_break;
                }
                ret = xml_relaxng_validate_definition(ctxt, list);
                // Is This correct ??????
                // if ret == 0 {}
                break 'to_break;
            }
            list = (*define).content;
            oldflags = (*ctxt).flags;
            (*ctxt).flags |= FLAGS_IGNORABLE;

            while !list.is_null() {
                oldstate = xml_relaxng_copy_valid_state(ctxt, (*ctxt).state);
                ret = xml_relaxng_validate_definition(ctxt, list);
                if ret == 0 {
                    if states.is_null() {
                        states = xml_relaxng_new_states(ctxt, 1);
                    }
                    if !(*ctxt).state.is_null() {
                        xml_relaxng_add_states(ctxt, states, (*ctxt).state);
                    } else if !(*ctxt).states.is_null() {
                        for i in 0..(*(*ctxt).states).nb_state {
                            xml_relaxng_add_states(
                                ctxt,
                                states,
                                *(*(*ctxt).states).tab_state.add(i as usize),
                            );
                        }
                        xml_relaxng_free_states(ctxt, (*ctxt).states);
                        (*ctxt).states = null_mut();
                    }
                } else {
                    xml_relaxng_free_valid_state(ctxt, (*ctxt).state);
                }
                (*ctxt).state = oldstate;
                list = (*list).next;
            }
            if !states.is_null() {
                xml_relaxng_free_valid_state(ctxt, oldstate);
                (*ctxt).states = states;
                (*ctxt).state = null_mut();
                ret = 0;
            } else {
                (*ctxt).states = null_mut();
            }
            (*ctxt).flags = oldflags;
            if ret != 0 {
                if (*ctxt).flags & FLAGS_IGNORABLE == 0 {
                    xml_relaxng_dump_valid_error(ctxt);
                }
            } else if (*ctxt).err_nr > err_nr {
                xml_relaxng_pop_errors(ctxt, err_nr);
            }
        }
        XmlRelaxNGType::Def | XmlRelaxNGType::Group => {
            ret = xml_relaxng_validate_definition_list(ctxt, (*define).content);
        }
        XmlRelaxNGType::Interleave => {
            ret = xml_relaxng_validate_interleave(ctxt, define);
        }
        XmlRelaxNGType::Attribute => {
            ret = xml_relaxng_validate_attribute(ctxt, define);
        }
        XmlRelaxNGType::Start
        | XmlRelaxNGType::Noop
        | XmlRelaxNGType::Ref
        | XmlRelaxNGType::Externalref
        | XmlRelaxNGType::Parentref => {
            ret = xml_relaxng_validate_definition(ctxt, (*define).content);
        }
        XmlRelaxNGType::Datatype => 'to_break: {
            let mut child: XmlNodePtr;
            let mut content: *mut XmlChar = null_mut();

            child = node;
            while !child.is_null() {
                if (*child).typ == XmlElementType::XmlElementNode {
                    VALID_ERR2!(
                        ctxt,
                        XmlRelaxNGValidErr::XmlRelaxngErrDataelem,
                        (*(*node).parent).name
                    );
                    ret = -1;
                    break;
                } else if matches!(
                    (*child).typ,
                    XmlElementType::XmlTextNode | XmlElementType::XmlCdataSectionNode
                ) {
                    content = xml_strcat(content, (*child).content);
                }
                /* TODO: handle entities ... */
                child = (*child).next;
            }
            if ret == -1 {
                if !content.is_null() {
                    xml_free(content as _);
                }
                break 'to_break;
            }
            if content.is_null() {
                content = xml_strdup(c"".as_ptr() as _);
                if content.is_null() {
                    xml_rng_verr_memory(ctxt, c"validating\n".as_ptr() as _);
                    ret = -1;
                    break 'to_break;
                }
            }
            ret = xml_relaxng_validate_datatype(ctxt, content, define, (*(*ctxt).state).seq);
            if ret == -1 {
                VALID_ERR2!(
                    ctxt,
                    XmlRelaxNGValidErr::XmlRelaxngErrDatatype,
                    (*define).name
                );
            } else if ret == 0 {
                (*(*ctxt).state).seq = null_mut();
            }
            if !content.is_null() {
                xml_free(content as _);
            }
        }
        XmlRelaxNGType::Value => 'to_break: {
            let mut content: *mut XmlChar = null_mut();
            let mut child: XmlNodePtr;

            child = node;
            while !child.is_null() {
                if (*child).typ == XmlElementType::XmlElementNode {
                    VALID_ERR2!(
                        ctxt,
                        XmlRelaxNGValidErr::XmlRelaxngErrValelem,
                        (*(*node).parent).name
                    );
                    ret = -1;
                    break;
                } else if matches!(
                    (*child).typ,
                    XmlElementType::XmlTextNode | XmlElementType::XmlCdataSectionNode
                ) {
                    content = xml_strcat(content, (*child).content);
                }
                /* TODO: handle entities ... */
                child = (*child).next;
            }
            if ret == -1 {
                if !content.is_null() {
                    xml_free(content as _);
                }
                break 'to_break;
            }
            if content.is_null() {
                content = xml_strdup(c"".as_ptr() as _);
                if content.is_null() {
                    xml_rng_verr_memory(ctxt, c"validating\n".as_ptr() as _);
                    ret = -1;
                    break 'to_break;
                }
            }
            let oldvalue: *mut XmlChar = (*(*ctxt).state).value;
            (*(*ctxt).state).value = content;
            ret = xml_relaxng_validate_value(ctxt, define);
            (*(*ctxt).state).value = oldvalue;
            if ret == -1 {
                VALID_ERR2!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrValue, (*define).name);
            } else if ret == 0 {
                (*(*ctxt).state).seq = null_mut();
            }
            if !content.is_null() {
                xml_free(content as _);
            }
        }
        XmlRelaxNGType::List => 'to_break: {
            let mut content: *mut XmlChar;
            let mut child: XmlNodePtr;

            /*
             * Make sure it's only text nodes
             */

            content = null_mut();
            child = node;
            while !child.is_null() {
                if (*child).typ == XmlElementType::XmlElementNode {
                    VALID_ERR2!(
                        ctxt,
                        XmlRelaxNGValidErr::XmlRelaxngErrListelem,
                        (*(*node).parent).name
                    );
                    ret = -1;
                    break;
                } else if matches!(
                    (*child).typ,
                    XmlElementType::XmlTextNode | XmlElementType::XmlCdataSectionNode
                ) {
                    content = xml_strcat(content, (*child).content);
                }
                /* TODO: handle entities ... */
                child = (*child).next;
            }
            if ret == -1 {
                if !content.is_null() {
                    xml_free(content as _);
                }
                break 'to_break;
            }
            if content.is_null() {
                content = xml_strdup(c"".as_ptr() as _);
                if content.is_null() {
                    xml_rng_verr_memory(ctxt, c"validating\n".as_ptr() as _);
                    ret = -1;
                    break 'to_break;
                }
            }
            let len: i32 = xml_strlen(content);
            let oldvalue: *mut XmlChar = (*(*ctxt).state).value;
            let oldendvalue: *mut XmlChar = (*(*ctxt).state).endvalue;
            (*(*ctxt).state).value = content;
            (*(*ctxt).state).endvalue = content.add(len as usize);
            ret = xml_relaxng_validate_value(ctxt, define);
            (*(*ctxt).state).value = oldvalue;
            (*(*ctxt).state).endvalue = oldendvalue;
            if ret == -1 {
                VALID_ERR!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrList);
            } else if ret == 0 && !node.is_null() {
                (*(*ctxt).state).seq = (*node).next;
            }
            if !content.is_null() {
                xml_free(content as _);
            }
        }
        XmlRelaxNGType::Except | XmlRelaxNGType::Param => {
            // TODO
            ret = -1;
        }
    }
    (*ctxt).depth -= 1;
    ret
}

/**
 * xmlRelaxNGValidateDefinition:
 * @ctxt:  a Relax-NG validation context
 * @define:  the definition to verify
 *
 * Validate the current node lists against the definition
 *
 * Returns 0 if the validation succeeded or an error code.
 */
unsafe extern "C" fn xml_relaxng_validate_definition(
    ctxt: XmlRelaxNGValidCtxtPtr,
    define: XmlRelaxNGDefinePtr,
) -> i32 {
    let mut res: XmlRelaxNGStatesPtr;
    let mut ret: i32;
    let mut j: i32;

    /*
     * We should NOT have both (*ctxt).state and (*ctxt).states
     */
    if !(*ctxt).state.is_null() && !(*ctxt).states.is_null() {
        // TODO
        xml_relaxng_free_valid_state(ctxt, (*ctxt).state);
        (*ctxt).state = null_mut();
    }

    if (*ctxt).states.is_null() || (*(*ctxt).states).nb_state == 1 {
        if !(*ctxt).states.is_null() {
            (*ctxt).state = *(*(*ctxt).states).tab_state.add(0);
            xml_relaxng_free_states(ctxt, (*ctxt).states);
            (*ctxt).states = null_mut();
        }
        ret = xml_relaxng_validate_state(ctxt, define);
        if !(*ctxt).state.is_null() && !(*ctxt).states.is_null() {
            // TODO
            xml_relaxng_free_valid_state(ctxt, (*ctxt).state);
            (*ctxt).state = null_mut();
        }
        if !(*ctxt).states.is_null() && (*(*ctxt).states).nb_state == 1 {
            (*ctxt).state = *(*(*ctxt).states).tab_state.add(0);
            xml_relaxng_free_states(ctxt, (*ctxt).states);
            (*ctxt).states = null_mut();
        }
        return ret;
    }

    let states: XmlRelaxNGStatesPtr = (*ctxt).states;
    (*ctxt).states = null_mut();
    res = null_mut();
    j = 0;
    let oldflags: i32 = (*ctxt).flags;
    (*ctxt).flags |= FLAGS_IGNORABLE;
    for i in 0..(*states).nb_state {
        (*ctxt).state = *(*states).tab_state.add(i as usize);
        (*ctxt).states = null_mut();
        ret = xml_relaxng_validate_state(ctxt, define);
        /*
         * We should NOT have both (*ctxt).state and (*ctxt).states
         */
        if !(*ctxt).state.is_null() && !(*ctxt).states.is_null() {
            // TODO
            xml_relaxng_free_valid_state(ctxt, (*ctxt).state);
            (*ctxt).state = null_mut();
        }
        if ret == 0 {
            if (*ctxt).states.is_null() {
                if !res.is_null() {
                    /* add the state to the container */
                    xml_relaxng_add_states(ctxt, res, (*ctxt).state);
                    (*ctxt).state = null_mut();
                } else {
                    /* add the state directly in states */
                    *(*states).tab_state.add(j as usize) = (*ctxt).state;
                    j += 1;
                    (*ctxt).state = null_mut();
                }
            } else if res.is_null() {
                /* make it the new container and copy other results */
                res = (*ctxt).states;
                (*ctxt).states = null_mut();
                for k in 0..j {
                    xml_relaxng_add_states(ctxt, res, *(*states).tab_state.add(k as usize));
                }
            } else {
                /* add all the new results to res and reff the container */
                for k in 0..(*(*ctxt).states).nb_state {
                    xml_relaxng_add_states(ctxt, res, *(*(*ctxt).states).tab_state.add(k as usize));
                }
                xml_relaxng_free_states(ctxt, (*ctxt).states);
                (*ctxt).states = null_mut();
            }
        } else if !(*ctxt).state.is_null() {
            xml_relaxng_free_valid_state(ctxt, (*ctxt).state);
            (*ctxt).state = null_mut();
        } else if !(*ctxt).states.is_null() {
            for k in 0..(*(*ctxt).states).nb_state {
                xml_relaxng_free_valid_state(ctxt, *(*(*ctxt).states).tab_state.add(k as usize));
            }
            xml_relaxng_free_states(ctxt, (*ctxt).states);
            (*ctxt).states = null_mut();
        }
    }
    (*ctxt).flags = oldflags;
    if !res.is_null() {
        xml_relaxng_free_states(ctxt, states);
        (*ctxt).states = res;
        ret = 0;
    } else if j > 1 {
        (*states).nb_state = j;
        (*ctxt).states = states;
        ret = 0;
    } else if j == 1 {
        (*ctxt).state = *(*states).tab_state.add(0);
        xml_relaxng_free_states(ctxt, states);
        ret = 0;
    } else {
        ret = -1;
        xml_relaxng_free_states(ctxt, states);
        if !(*ctxt).states.is_null() {
            xml_relaxng_free_states(ctxt, (*ctxt).states);
            (*ctxt).states = null_mut();
        }
    }
    if !(*ctxt).state.is_null() && !(*ctxt).states.is_null() {
        // TODO
        xml_relaxng_free_valid_state(ctxt, (*ctxt).state);
        (*ctxt).state = null_mut();
    }
    ret
}

/**
 * xmlRelaxNGValidateDocument:
 * @ctxt:  a Relax-NG validation context
 * @doc:  the document
 *
 * Validate the given document
 *
 * Returns 0 if the validation succeeded or an error code.
 */
unsafe extern "C" fn xml_relaxng_validate_document(
    ctxt: XmlRelaxNGValidCtxtPtr,
    doc: XmlDocPtr,
) -> i32 {
    let mut ret: i32;
    let mut state: XmlRelaxNGValidStatePtr;
    let mut node: XmlNodePtr;

    if ctxt.is_null() || (*ctxt).schema.is_null() || doc.is_null() {
        return -1;
    }

    (*ctxt).err_no = XmlRelaxNGValidErr::XmlRelaxngOk as i32;
    let schema: XmlRelaxNGPtr = (*ctxt).schema;
    let grammar: XmlRelaxNGGrammarPtr = (*schema).topgrammar;
    if grammar.is_null() {
        VALID_ERR!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrNogrammar);
        return -1;
    }
    state = xml_relaxng_new_valid_state(ctxt, null_mut());
    (*ctxt).state = state;
    ret = xml_relaxng_validate_definition(ctxt, (*grammar).start);
    if !(*ctxt).state.is_null() && !(*state).seq.is_null() {
        state = (*ctxt).state;
        node = (*state).seq;
        node = xml_relaxng_skip_ignored(ctxt, node);
        if !node.is_null() && ret != -1 {
            VALID_ERR!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrExtradata);
            ret = -1;
        }
    } else if !(*ctxt).states.is_null() {
        let mut tmp: i32 = -1;

        for i in 0..(*(*ctxt).states).nb_state {
            state = *(*(*ctxt).states).tab_state.add(i as usize);
            node = (*state).seq;
            node = xml_relaxng_skip_ignored(ctxt, node);
            if node.is_null() {
                tmp = 0;
            }
            xml_relaxng_free_valid_state(ctxt, state);
        }
        if tmp == -1 && ret != -1 {
            VALID_ERR!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrExtradata);
            ret = -1;
        }
    }
    if !(*ctxt).state.is_null() {
        xml_relaxng_free_valid_state(ctxt, (*ctxt).state);
        (*ctxt).state = null_mut();
    }
    if ret != 0 {
        xml_relaxng_dump_valid_error(ctxt);
    }
    #[cfg(feature = "valid")]
    if (*ctxt).idref == 1 {
        let mut vctxt = XmlValidCtxt {
            user_data: (*ctxt).user_data.clone(),
            error: (*ctxt).error,
            warning: (*ctxt).warning,
            valid: 1,
            ..Default::default()
        };
        // memset(addr_of_mut!(vctxt) as _, 0, size_of::<XmlValidCtxt>());
        vctxt.valid = 1;
        vctxt.error = (*ctxt).error;
        vctxt.warning = (*ctxt).warning;
        vctxt.user_data = (*ctxt).user_data.clone();

        if xml_validate_document_final(addr_of_mut!(vctxt), doc) != 1 {
            ret = -1;
        }
    }
    if ret == 0 && (*ctxt).err_no != XmlRelaxNGValidErr::XmlRelaxngOk as i32 {
        ret = -1;
    }

    ret
}

/**
 * xmlRelaxNGCleanPSVI:
 * @node:  an input element or document
 *
 * Call this routine to speed up XPath computation on static documents.
 * This stamps all the element nodes with the document order
 * Like for line information, the order is kept in the element->content
 * field, the value stored is actually - the node number (starting at -1)
 * to be able to differentiate from line numbers.
 *
 * Returns the number of elements found in the document or -1 in case
 *    of error.
 */
#[cfg(feature = "schema")]
unsafe extern "C" fn xml_relaxng_clean_psvi(node: XmlNodePtr) {
    let mut cur: XmlNodePtr;

    if node.is_null()
        || !matches!(
            (*node).typ,
            XmlElementType::XmlElementNode
                | XmlElementType::XmlDocumentNode
                | XmlElementType::XmlHtmlDocumentNode
        )
    {
        return;
    }
    if (*node).typ == XmlElementType::XmlElementNode {
        (*node).psvi = null_mut();
    }

    cur = (*node).children;
    while !cur.is_null() {
        if (*cur).typ == XmlElementType::XmlElementNode {
            (*cur).psvi = null_mut();
            if !(*cur).children.is_null() {
                cur = (*cur).children;
                continue;
            }
        }
        if !(*cur).next.is_null() {
            cur = (*cur).next;
            continue;
        }
        loop {
            cur = (*cur).parent;
            if cur.is_null() {
                break;
            }
            if cur == node {
                cur = null_mut();
                break;
            }
            if !(*cur).next.is_null() {
                cur = (*cur).next;
                break;
            }

            if cur.is_null() {
                break;
            }
        }
    }
}

/**
 * xmlRelaxNGValidateDoc:
 * @ctxt:  a Relax-NG validation context
 * @doc:  a parsed document tree
 *
 * Validate a document tree in memory.
 *
 * Returns 0 if the document is valid, a positive error code
 *     number otherwise and -1 in case of internal or API error.
 */
pub unsafe extern "C" fn xml_relaxng_validate_doc(
    ctxt: XmlRelaxNGValidCtxtPtr,
    doc: XmlDocPtr,
) -> i32 {
    if ctxt.is_null() || doc.is_null() {
        return -1;
    }

    (*ctxt).doc = doc;

    let ret: i32 = xml_relaxng_validate_document(ctxt, doc);
    /*
     * Remove all left PSVI
     */
    xml_relaxng_clean_psvi(doc as _);

    /*
     * TODO: build error codes
     */
    if ret == -1 {
        return 1;
    }
    ret
}

/**
* xmlRelaxNGValidateProgressiveCallback:
* @exec:  the regular expression instance
* @token:  the token which matched
* @transdata:  callback data, the define for the subelement if available
* @inputdata:  callback data, the Relax NG validation context
*
* Handle the callback and if needed validate the element children.
* some of the in/out information are passed via the context in @inputdata.
*/
unsafe extern "C" fn xml_relaxng_validate_progressive_callback(
    _exec: XmlRegExecCtxtPtr,
    token: *const XmlChar,
    transdata: *mut c_void,
    inputdata: *mut c_void,
) {
    let ctxt: XmlRelaxNGValidCtxtPtr = inputdata as _;
    let define: XmlRelaxNGDefinePtr = transdata as _;
    let mut state: XmlRelaxNGValidStatePtr;
    let mut ret: i32 = 0;
    let oldflags: i32;

    if ctxt.is_null() {
        let token = CStr::from_ptr(token as *const i8).to_string_lossy();
        eprintln!("callback on {token} missing context");
        return;
    }
    let node: XmlNodePtr = (*ctxt).pnode;
    (*ctxt).pstate = 1;
    if define.is_null() {
        if *token.add(0) == b'#' {
            return;
        }
        let token = CStr::from_ptr(token as *const i8).to_string_lossy();
        eprintln!("callback on {token} missing define");
        if !ctxt.is_null() && (*ctxt).err_no == XmlRelaxNGValidErr::XmlRelaxngOk as i32 {
            (*ctxt).err_no = XmlRelaxNGValidErr::XmlRelaxngErrInternal as i32;
        }
        (*ctxt).pstate = -1;
        return;
    }
    if ctxt.is_null() || define.is_null() {
        let token = CStr::from_ptr(token as *const i8).to_string_lossy();
        eprintln!("callback on {token} missing info");
        if !ctxt.is_null() && (*ctxt).err_no == XmlRelaxNGValidErr::XmlRelaxngOk as i32 {
            (*ctxt).err_no = XmlRelaxNGValidErr::XmlRelaxngErrInternal as i32;
        }
        (*ctxt).pstate = -1;
        return;
    } else if (*define).typ != XmlRelaxNGType::Element {
        let token = CStr::from_ptr(token as *const i8).to_string_lossy();
        eprintln!("callback on {token} define is not element");
        if (*ctxt).err_no == XmlRelaxNGValidErr::XmlRelaxngOk as i32 {
            (*ctxt).err_no = XmlRelaxNGValidErr::XmlRelaxngErrInternal as i32;
        }
        (*ctxt).pstate = -1;
        return;
    }
    if !matches!((*node).typ, XmlElementType::XmlElementNode) {
        VALID_ERR!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrNotelem);
        if (*ctxt).flags & FLAGS_IGNORABLE == 0 {
            xml_relaxng_dump_valid_error(ctxt);
        }
        (*ctxt).pstate = -1;
        return;
    }
    if (*define).cont_model.is_null() {
        /*
         * this node cannot be validated in a streamable fashion
         */
        (*ctxt).pstate = 0;
        (*ctxt).pdef = define;
        return;
    }
    let exec = xml_reg_new_exec_ctxt(
        (*define).cont_model,
        Some(xml_relaxng_validate_progressive_callback),
        ctxt as _,
    );
    if exec.is_null() {
        (*ctxt).pstate = -1;
        return;
    }
    xml_relaxng_elem_push(ctxt, exec);

    /*
     * Validate the attributes part of the content.
     */
    state = xml_relaxng_new_valid_state(ctxt, node);
    if state.is_null() {
        (*ctxt).pstate = -1;
        return;
    }
    let oldstate: XmlRelaxNGValidStatePtr = (*ctxt).state;
    (*ctxt).state = state;
    if !(*define).attrs.is_null() {
        ret = xml_relaxng_validate_attribute_list(ctxt, (*define).attrs);
        if ret != 0 {
            (*ctxt).pstate = -1;
            VALID_ERR2!(
                ctxt,
                XmlRelaxNGValidErr::XmlRelaxngErrAttrvalid,
                (*node).name
            );
        }
    }
    if !(*ctxt).state.is_null() {
        (*(*ctxt).state).seq = null_mut();
        ret = xml_relaxng_validate_element_end(ctxt, 1);
        if ret != 0 {
            (*ctxt).pstate = -1;
        }
        xml_relaxng_free_valid_state(ctxt, (*ctxt).state);
    } else if !(*ctxt).states.is_null() {
        let mut tmp: i32 = -1;

        oldflags = (*ctxt).flags;

        for i in 0..(*(*ctxt).states).nb_state {
            state = *(*(*ctxt).states).tab_state.add(i as usize);
            (*ctxt).state = state;
            (*(*ctxt).state).seq = null_mut();

            if xml_relaxng_validate_element_end(ctxt, 0) == 0 {
                tmp = 0;
                break;
            }
        }
        if tmp != 0 {
            /*
             * validation error, log the message for the "best" one
             */
            (*ctxt).flags |= FLAGS_IGNORABLE;
            xml_relaxng_log_best_error(ctxt);
        }
        for i in 0..(*(*ctxt).states).nb_state {
            xml_relaxng_free_valid_state(ctxt, *(*(*ctxt).states).tab_state.add(i as usize));
        }
        xml_relaxng_free_states(ctxt, (*ctxt).states);
        (*ctxt).states = null_mut();
        if ret == 0 && tmp == -1 {
            (*ctxt).pstate = -1;
        }
        (*ctxt).flags = oldflags;
    }
    if (*ctxt).pstate == -1 && (*ctxt).flags & FLAGS_IGNORABLE == 0 {
        xml_relaxng_dump_valid_error(ctxt);
    }
    (*ctxt).state = oldstate;
}

/**
 * xmlRelaxNGElemPush:
 * @ctxt:  the validation context
 * @exec:  the regexp runtime for the new content model
 *
 * Push a new regexp for the current node content model on the stack
 *
 * Returns 0 in case of success and -1 in case of error.
 */
unsafe extern "C" fn xml_relaxng_elem_push(
    ctxt: XmlRelaxNGValidCtxtPtr,
    exec: XmlRegExecCtxtPtr,
) -> i32 {
    if (*ctxt).elem_tab.is_null() {
        (*ctxt).elem_max = 10;
        (*ctxt).elem_tab =
            xml_malloc((*ctxt).elem_max as usize * size_of::<XmlRegExecCtxtPtr>()) as _;
        if (*ctxt).elem_tab.is_null() {
            xml_rng_verr_memory(ctxt, c"validating\n".as_ptr() as _);
            return -1;
        }
    }
    if (*ctxt).elem_nr >= (*ctxt).elem_max {
        (*ctxt).elem_max *= 2;
        (*ctxt).elem_tab = xml_realloc(
            (*ctxt).elem_tab as _,
            (*ctxt).elem_max as usize * size_of::<XmlRegExecCtxtPtr>(),
        ) as _;
        if (*ctxt).elem_tab.is_null() {
            xml_rng_verr_memory(ctxt, c"validating\n".as_ptr() as _);
            return -1;
        }
    }
    *(*ctxt).elem_tab.add((*ctxt).elem_nr as usize) = exec;
    (*ctxt).elem_nr += 1;
    (*ctxt).elem = exec;
    0
}

/*
 * Interfaces for progressive validation when possible
 */
/**
 * xmlRelaxNGValidatePushElement:
 * @ctxt:  the validation context
 * @doc:  a document instance
 * @elem:  an element instance
 *
 * Push a new element start on the RelaxNG validation stack.
 *
 * returns 1 if no validation problem was found or 0 if validating the
 *         element requires a full node, and -1 in case of error.
 */
pub unsafe extern "C" fn xml_relaxng_validate_push_element(
    ctxt: XmlRelaxNGValidCtxtPtr,
    _doc: XmlDocPtr,
    elem: XmlNodePtr,
) -> i32 {
    let mut ret: i32;

    if ctxt.is_null() || elem.is_null() {
        return -1;
    }

    if (*ctxt).elem.is_null() {
        let schema: XmlRelaxNGPtr = (*ctxt).schema;
        if schema.is_null() {
            VALID_ERR!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrNogrammar);
            return -1;
        }
        let grammar: XmlRelaxNGGrammarPtr = (*schema).topgrammar;
        if grammar.is_null() || (*grammar).start.is_null() {
            VALID_ERR!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrNogrammar);
            return -1;
        }
        let define: XmlRelaxNGDefinePtr = (*grammar).start;
        if (*define).cont_model.is_null() {
            (*ctxt).pdef = define;
            return 0;
        }
        let exec: XmlRegExecCtxtPtr = xml_reg_new_exec_ctxt(
            (*define).cont_model,
            Some(xml_relaxng_validate_progressive_callback),
            ctxt as _,
        );
        if exec.is_null() {
            return -1;
        }
        xml_relaxng_elem_push(ctxt, exec);
    }
    (*ctxt).pnode = elem;
    (*ctxt).pstate = 0;
    if !(*elem).ns.is_null() {
        ret = xml_reg_exec_push_string2(
            (*ctxt).elem,
            (*elem).name,
            (*(*elem).ns).href.load(Ordering::Relaxed),
            ctxt as _,
        );
    } else {
        ret = xml_reg_exec_push_string((*ctxt).elem, (*elem).name, ctxt as _);
    }
    if ret < 0 {
        VALID_ERR2!(
            ctxt,
            XmlRelaxNGValidErr::XmlRelaxngErrElemwrong,
            (*elem).name
        );
    } else if (*ctxt).pstate == 0 {
        ret = 0;
    } else if (*ctxt).pstate < 0 {
        ret = -1;
    } else {
        ret = 1;
    }
    ret
}

/**
 * xmlRelaxNGValidatePushCData:
 * @ctxt:  the RelaxNG validation context
 * @data:  some character data read
 * @len:  the length of the data
 *
 * check the CData parsed for validation in the current stack
 *
 * returns 1 if no validation problem was found or -1 otherwise
 */
pub unsafe extern "C" fn xml_relaxng_validate_push_cdata(
    ctxt: XmlRelaxNGValidCtxtPtr,
    mut data: *const XmlChar,
    _len: i32,
) -> i32 {
    if ctxt.is_null() || (*ctxt).elem.is_null() || data.is_null() {
        return -1;
    }

    while *data != 0 {
        if !xml_is_blank_char(*data as u32) {
            break;
        }
        data = data.add(1);
    }
    if *data == 0 {
        return 1;
    }

    let ret: i32 = xml_reg_exec_push_string((*ctxt).elem, c"#text".as_ptr() as _, ctxt as _);
    if ret < 0 {
        VALID_ERR2!(
            ctxt,
            XmlRelaxNGValidErr::XmlRelaxngErrTextwrong,
            c" TODO ".as_ptr() as _
        );

        return -1;
    }
    1
}

/**
 * xmlRelaxNGValidatePopElement:
 * @ctxt:  the RelaxNG validation context
 * @doc:  a document instance
 * @elem:  an element instance
 *
 * Pop the element end from the RelaxNG validation stack.
 *
 * returns 1 if no validation problem was found or 0 otherwise
 */
pub unsafe extern "C" fn xml_relaxng_validate_pop_element(
    ctxt: XmlRelaxNGValidCtxtPtr,
    _doc: XmlDocPtr,
    elem: XmlNodePtr,
) -> i32 {
    let mut ret: i32;

    if ctxt.is_null() || (*ctxt).elem.is_null() || elem.is_null() {
        return -1;
    }
    /*
     * verify that we reached a terminal state of the content model.
     */
    let exec: XmlRegExecCtxtPtr = xml_relaxng_elem_pop(ctxt);
    ret = xml_reg_exec_push_string(exec, null_mut(), null_mut());
    match ret.cmp(&0) {
        std::cmp::Ordering::Equal => {
            /*
             * TODO: get some of the names needed to exit the current state of exec
             */
            VALID_ERR2!(
                ctxt,
                XmlRelaxNGValidErr::XmlRelaxngErrNoelem,
                c"".as_ptr() as _
            );
            ret = -1;
        }
        std::cmp::Ordering::Less => {
            ret = -1;
        }
        std::cmp::Ordering::Greater => {
            ret = 1;
        }
    }
    xml_reg_free_exec_ctxt(exec);
    ret
}

/**
 * xmlRelaxNGValidateFullElement:
 * @ctxt:  the validation context
 * @doc:  a document instance
 * @elem:  an element instance
 *
 * Validate a full subtree when xmlRelaxNGValidatePushElement() returned
 * 0 and the content of the node has been expanded.
 *
 * returns 1 if no validation problem was found or -1 in case of error.
 */
pub unsafe extern "C" fn xml_relaxng_validate_full_element(
    ctxt: XmlRelaxNGValidCtxtPtr,
    _doc: XmlDocPtr,
    elem: XmlNodePtr,
) -> i32 {
    let mut ret: i32;

    if ctxt.is_null() || (*ctxt).pdef.is_null() || elem.is_null() {
        return -1;
    }
    let state: XmlRelaxNGValidStatePtr = xml_relaxng_new_valid_state(ctxt, (*elem).parent);
    if state.is_null() {
        return -1;
    }
    (*state).seq = elem;
    (*ctxt).state = state;
    (*ctxt).err_no = XmlRelaxNGValidErr::XmlRelaxngOk as i32;
    ret = xml_relaxng_validate_definition(ctxt, (*ctxt).pdef);
    if ret != 0 || (*ctxt).err_no != XmlRelaxNGValidErr::XmlRelaxngOk as i32 {
        ret = -1;
    } else {
        ret = 1;
    }
    xml_relaxng_free_valid_state(ctxt, (*ctxt).state);
    (*ctxt).state = null_mut();
    ret
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_relaxng_dump() {
        #[cfg(all(feature = "schema", feature = "output"))]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_FILE_PTR {
                for n_schema in 0..GEN_NB_XML_RELAXNG_PTR {
                    let mem_base = xml_mem_blocks();
                    let output = gen_file_ptr(n_output, 0);
                    let schema = gen_xml_relaxng_ptr(n_schema, 1);

                    xml_relaxng_dump(output, schema);
                    des_file_ptr(n_output, output, 0);
                    des_xml_relaxng_ptr(n_schema, schema, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlRelaxNGDump",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlRelaxNGDump()");
                        eprint!(" {}", n_output);
                        eprintln!(" {}", n_schema);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_relaxng_dump_tree() {
        #[cfg(all(feature = "schema", feature = "output"))]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_FILE_PTR {
                for n_schema in 0..GEN_NB_XML_RELAXNG_PTR {
                    let mem_base = xml_mem_blocks();
                    let output = gen_file_ptr(n_output, 0);
                    let schema = gen_xml_relaxng_ptr(n_schema, 1);

                    xml_relaxng_dump_tree(output, schema);
                    des_file_ptr(n_output, output, 0);
                    des_xml_relaxng_ptr(n_schema, schema, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlRelaxNGDumpTree",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlRelaxNGDumpTree()"
                        );
                        eprint!(" {}", n_output);
                        eprintln!(" {}", n_schema);
                    }
                }
            }
        }
    }

    // #[test]
    // fn test_xml_relaxng_get_parser_errors() {
    //     #[cfg(feature = "schema")]
    //     unsafe {
    //         let mut leaks = 0;

    //         for n_ctxt in 0..GEN_NB_XML_RELAXNG_PARSER_CTXT_PTR {
    //             for n_err in 0..GEN_NB_XML_RELAXNG_VALIDITY_ERROR_FUNC_PTR {
    //                 for n_warn in 0..GEN_NB_XML_RELAXNG_VALIDITY_WARNING_FUNC_PTR {
    //                     for n_ctx in 0..GEN_NB_VOID_PTR_PTR {
    //                         let mem_base = xml_mem_blocks();
    //                         let ctxt = gen_xml_relaxng_parser_ctxt_ptr(n_ctxt, 0);
    //                         let err = gen_xml_relaxng_validity_error_func_ptr(n_err, 1);
    //                         let warn = gen_xml_relaxng_validity_warning_func_ptr(n_warn, 2);
    //                         let ctx = gen_void_ptr_ptr(n_ctx, 3);

    //                         let ret_val = xml_relaxng_get_parser_errors(ctxt, err, warn, ctx);
    //                         desret_int(ret_val);
    //                         des_xml_relaxng_parser_ctxt_ptr(n_ctxt, ctxt, 0);
    //                         des_xml_relaxng_validity_error_func_ptr(n_err, err, 1);
    //                         des_xml_relaxng_validity_warning_func_ptr(n_warn, warn, 2);
    //                         des_void_ptr_ptr(n_ctx, ctx, 3);
    //                         reset_last_error();
    //                         if mem_base != xml_mem_blocks() {
    //                             leaks += 1;
    //                             eprint!(
    //                                 "Leak of {} blocks found in xmlRelaxNGGetParserErrors",
    //                                 xml_mem_blocks() - mem_base
    //                             );
    //                             assert!(
    //                                 leaks == 0,
    //                                 "{leaks} Leaks are found in xmlRelaxNGGetParserErrors()"
    //                             );
    //                             eprint!(" {}", n_ctxt);
    //                             eprint!(" {}", n_err);
    //                             eprint!(" {}", n_warn);
    //                             eprintln!(" {}", n_ctx);
    //                         }
    //                     }
    //                 }
    //             }
    //         }
    //     }
    // }

    // #[test]
    // fn test_xml_relaxng_get_valid_errors() {
    //     #[cfg(feature = "schema")]
    //     unsafe {
    //         let mut leaks = 0;

    //         for n_ctxt in 0..GEN_NB_XML_RELAXNG_VALID_CTXT_PTR {
    //             for n_err in 0..GEN_NB_XML_RELAXNG_VALIDITY_ERROR_FUNC_PTR {
    //                 for n_warn in 0..GEN_NB_XML_RELAXNG_VALIDITY_WARNING_FUNC_PTR {
    //                     for n_ctx in 0..GEN_NB_VOID_PTR_PTR {
    //                         let mem_base = xml_mem_blocks();
    //                         let ctxt = gen_xml_relaxng_valid_ctxt_ptr(n_ctxt, 0);
    //                         let err = gen_xml_relaxng_validity_error_func_ptr(n_err, 1);
    //                         let warn = gen_xml_relaxng_validity_warning_func_ptr(n_warn, 2);
    //                         let ctx = gen_void_ptr_ptr(n_ctx, 3);

    //                         let ret_val = xml_relaxng_get_valid_errors(ctxt, err, warn, ctx);
    //                         desret_int(ret_val);
    //                         des_xml_relaxng_valid_ctxt_ptr(n_ctxt, ctxt, 0);
    //                         des_xml_relaxng_validity_error_func_ptr(n_err, err, 1);
    //                         des_xml_relaxng_validity_warning_func_ptr(n_warn, warn, 2);
    //                         des_void_ptr_ptr(n_ctx, ctx, 3);
    //                         reset_last_error();
    //                         if mem_base != xml_mem_blocks() {
    //                             leaks += 1;
    //                             eprint!(
    //                                 "Leak of {} blocks found in xmlRelaxNGGetValidErrors",
    //                                 xml_mem_blocks() - mem_base
    //                             );
    //                             assert!(
    //                                 leaks == 0,
    //                                 "{leaks} Leaks are found in xmlRelaxNGGetValidErrors()"
    //                             );
    //                             eprint!(" {}", n_ctxt);
    //                             eprint!(" {}", n_err);
    //                             eprint!(" {}", n_warn);
    //                             eprintln!(" {}", n_ctx);
    //                         }
    //                     }
    //                 }
    //             }
    //         }
    //     }
    // }

    #[test]
    fn test_xml_relaxng_init_types() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            let mem_base = xml_mem_blocks();

            let ret_val = xml_relaxng_init_types();
            desret_int(ret_val);
            reset_last_error();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in xmlRelaxNGInitTypes",
                    xml_mem_blocks() - mem_base
                );
                assert!(
                    leaks == 0,
                    "{leaks} Leaks are found in xmlRelaxNGInitTypes()"
                );
            }
        }
    }

    #[test]
    fn test_xml_relaxng_new_doc_parser_ctxt() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                let mem_base = xml_mem_blocks();
                let doc = gen_xml_doc_ptr(n_doc, 0);

                let ret_val = xml_relaxng_new_doc_parser_ctxt(doc);
                desret_xml_relaxng_parser_ctxt_ptr(ret_val);
                des_xml_doc_ptr(n_doc, doc, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlRelaxNGNewDocParserCtxt",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlRelaxNGNewDocParserCtxt()"
                    );
                    eprintln!(" {}", n_doc);
                }
            }
        }
    }

    #[test]
    fn test_xml_relaxng_new_mem_parser_ctxt() {
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

                    let ret_val = xml_relaxng_new_mem_parser_ctxt(buffer, size);
                    desret_xml_relaxng_parser_ctxt_ptr(ret_val);
                    des_const_char_ptr(n_buffer, buffer, 0);
                    des_int(n_size, size, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlRelaxNGNewMemParserCtxt",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlRelaxNGNewMemParserCtxt()"
                        );
                        eprint!(" {}", n_buffer);
                        eprintln!(" {}", n_size);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_relaxng_new_parser_ctxt() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_url in 0..GEN_NB_CONST_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let url = gen_const_char_ptr(n_url, 0);

                let ret_val = xml_relaxng_new_parser_ctxt(url);
                desret_xml_relaxng_parser_ctxt_ptr(ret_val);
                des_const_char_ptr(n_url, url, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlRelaxNGNewParserCtxt",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlRelaxNGNewParserCtxt()"
                    );
                    eprintln!(" {}", n_url);
                }
            }
        }
    }

    #[test]
    fn test_xml_relaxng_new_valid_ctxt() {

        /* missing type support */
    }

    #[test]
    fn test_xml_relaxng_parse() {

        /* missing type support */
    }

    #[test]
    fn test_xml_relaxng_set_parser_errors() {

        /* missing type support */
    }

    #[test]
    fn test_xml_relaxng_set_parser_structured_errors() {

        /* missing type support */
    }

    #[test]
    fn test_xml_relaxng_set_valid_errors() {

        /* missing type support */
    }

    #[test]
    fn test_xml_relaxng_set_valid_structured_errors() {

        /* missing type support */
    }

    #[test]
    fn test_xml_relaxng_validate_doc() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_RELAXNG_VALID_CTXT_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_relaxng_valid_ctxt_ptr(n_ctxt, 0);
                    let doc = gen_xml_doc_ptr(n_doc, 1);

                    let ret_val = xml_relaxng_validate_doc(ctxt, doc);
                    desret_int(ret_val);
                    des_xml_relaxng_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_xml_doc_ptr(n_doc, doc, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlRelaxNGValidateDoc",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlRelaxNGValidateDoc()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_doc);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_relaxng_validate_full_element() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_RELAXNG_VALID_CTXT_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_elem in 0..GEN_NB_XML_NODE_PTR {
                        let mem_base = xml_mem_blocks();
                        let ctxt = gen_xml_relaxng_valid_ctxt_ptr(n_ctxt, 0);
                        let doc = gen_xml_doc_ptr(n_doc, 1);
                        let elem = gen_xml_node_ptr(n_elem, 2);

                        let ret_val = xml_relaxng_validate_full_element(ctxt, doc, elem);
                        desret_int(ret_val);
                        des_xml_relaxng_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                        des_xml_doc_ptr(n_doc, doc, 1);
                        des_xml_node_ptr(n_elem, elem, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlRelaxNGValidateFullElement",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlRelaxNGValidateFullElement()"
                            );
                            eprint!(" {}", n_ctxt);
                            eprint!(" {}", n_doc);
                            eprintln!(" {}", n_elem);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_relaxng_validate_pop_element() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_RELAXNG_VALID_CTXT_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_elem in 0..GEN_NB_XML_NODE_PTR {
                        let mem_base = xml_mem_blocks();
                        let ctxt = gen_xml_relaxng_valid_ctxt_ptr(n_ctxt, 0);
                        let doc = gen_xml_doc_ptr(n_doc, 1);
                        let elem = gen_xml_node_ptr(n_elem, 2);

                        let ret_val = xml_relaxng_validate_pop_element(ctxt, doc, elem);
                        desret_int(ret_val);
                        des_xml_relaxng_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                        des_xml_doc_ptr(n_doc, doc, 1);
                        des_xml_node_ptr(n_elem, elem, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlRelaxNGValidatePopElement",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlRelaxNGValidatePopElement()"
                            );
                            eprint!(" {}", n_ctxt);
                            eprint!(" {}", n_doc);
                            eprintln!(" {}", n_elem);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_relaxng_validate_push_cdata() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_RELAXNG_VALID_CTXT_PTR {
                for n_data in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_len in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let ctxt = gen_xml_relaxng_valid_ctxt_ptr(n_ctxt, 0);
                        let data = gen_const_xml_char_ptr(n_data, 1);
                        let mut len = gen_int(n_len, 2);
                        if !data.is_null() && len > xml_strlen(data) {
                            len = 0;
                        }

                        let ret_val = xml_relaxng_validate_push_cdata(ctxt, data, len);
                        desret_int(ret_val);
                        des_xml_relaxng_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                        des_const_xml_char_ptr(n_data, data, 1);
                        des_int(n_len, len, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlRelaxNGValidatePushCData",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlRelaxNGValidatePushCData()"
                            );
                            eprint!(" {}", n_ctxt);
                            eprint!(" {}", n_data);
                            eprintln!(" {}", n_len);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_relaxng_validate_push_element() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_RELAXNG_VALID_CTXT_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_elem in 0..GEN_NB_XML_NODE_PTR {
                        let mem_base = xml_mem_blocks();
                        let ctxt = gen_xml_relaxng_valid_ctxt_ptr(n_ctxt, 0);
                        let doc = gen_xml_doc_ptr(n_doc, 1);
                        let elem = gen_xml_node_ptr(n_elem, 2);

                        let ret_val = xml_relaxng_validate_push_element(ctxt, doc, elem);
                        desret_int(ret_val);
                        des_xml_relaxng_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                        des_xml_doc_ptr(n_doc, doc, 1);
                        des_xml_node_ptr(n_elem, elem, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlRelaxNGValidatePushElement",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlRelaxNGValidatePushElement()"
                            );
                            eprint!(" {}", n_ctxt);
                            eprint!(" {}", n_doc);
                            eprintln!(" {}", n_elem);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_relax_parser_set_flag() {
        #[cfg(feature = "schema")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_RELAXNG_PARSER_CTXT_PTR {
                for n_flags in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_relaxng_parser_ctxt_ptr(n_ctxt, 0);
                    let flags = gen_int(n_flags, 1);

                    let ret_val = xml_relax_parser_set_flag(ctxt, flags);
                    desret_int(ret_val);
                    des_xml_relaxng_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_int(n_flags, flags, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlRelaxParserSetFlag",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlRelaxParserSetFlag()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_flags);
                    }
                }
            }
        }
    }
}
