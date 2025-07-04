//! Provide methods and data structures for parsing RelaxNG schemas.
//!
//! This module is based on `libxml/relaxng.h`, `relaxng.c`, and so on in `libxml2-v2.11.8`.  
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: implementation of the Relax-NG validation
// Description: implementation of the Relax-NG validation
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// relaxng.c : implementation of the Relax-NG handling and validity checking
//
// See Copyright for the status of this software.
//
// Daniel Veillard <veillard@redhat.com>

#[cfg(feature = "libxml_output")]
use std::io::Write;
use std::{
    any::type_name,
    borrow::Cow,
    ffi::{CStr, CString, c_char},
    mem::{size_of, take},
    os::raw::c_void,
    ptr::{addr_of_mut, drop_in_place, null_mut},
    rc::Rc,
    slice::from_raw_parts,
};

use libc::{memcpy, memset, snprintf};

#[cfg(feature = "libxml_regexp")]
use crate::libxml::xmlregexp::{XmlRegExecCtxt, XmlRegExecCtxtPtr, XmlRegexp};
use crate::{
    chvalid::XmlCharValid,
    error::XmlParserErrors,
    globals::{GenericError, GenericErrorContext, StructuredError},
    hash::XmlHashTableRef,
    libxml::{
        globals::{xml_free, xml_malloc, xml_realloc},
        hash::{
            XmlHashTablePtr, xml_hash_add_entry2, xml_hash_create, xml_hash_free, xml_hash_lookup2,
        },
        xmlstring::{XmlChar, xml_str_equal, xml_strdup, xml_strlen},
    },
    parser::{split_qname2, xml_read_file, xml_read_memory},
    relaxng::{
        VALID_ERR, VALID_ERR2, VALID_ERR2P, VALID_ERR3, VALID_ERR3P, XML_RELAXNG_NS,
        XML_RELAXNG_REGISTERED_TYPES, XmlRelaxNGDefinePtr, XmlRelaxNGParserCtxtPtr,
        XmlRelaxNGStatesPtr, XmlRelaxNGTypeLibraryPtr, XmlRelaxNGValidCtxt, XmlRelaxNGValidCtxtPtr,
        XmlRelaxNGValidState, XmlRelaxNGValidStatePtr, is_relaxng, normalize_external_space,
        relaxng_normalize, xml_relaxng_dump_valid_error, xml_relaxng_free_define,
        xml_relaxng_free_states, xml_relaxng_free_valid_state, xml_relaxng_init_types,
        xml_relaxng_new_define, xml_relaxng_new_states, xml_relaxng_new_valid_state,
        xml_relaxng_pop_errors, xml_relaxng_valid_error_pop, xml_rng_perr, xml_rng_perr_memory,
        xml_rng_verr_memory,
    },
    tree::{
        NodeCommon, XmlAttrPtr, XmlDocPtr, XmlElementType, XmlGenericNodePtr, XmlNode, XmlNodePtr,
        XmlNs, XmlNsPtr, validate_ncname, xml_free_doc, xml_free_node, xml_new_child,
        xml_new_doc_node, xml_new_doc_text,
    },
    uri::{XmlURI, build_uri, escape_url_except},
    valid::{XmlValidCtxt, xml_validate_document_final},
};

use super::{
    xmlautomata::XmlAutomata,
    xmlstring::{xml_strncat, xml_strndup},
};

/// Signature of an error callback from a Relax-NG validation
#[doc(alias = "xmlRelaxNGValidityErrorFunc")]
pub type XmlRelaxNGValidityErrorFunc = unsafe fn(ctx: *mut c_void, msg: *const c_char);

/// Signature of a warning callback from a Relax-NG validation
#[doc(alias = "xmlRelaxNGValidityWarningFunc")]
pub type XmlRelaxNGValidityWarningFunc = unsafe fn(ctx: *mut c_void, msg: *const c_char);

// List of possible Relax NG validation errors
#[doc(alias = "xmlRelaxNGValidErr")]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum XmlRelaxNGValidErr {
    #[default]
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

/// List of possible Relax NG Parser flags
#[doc(alias = "xmlRelaxNGParserFlags")]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlRelaxNGParserFlag {
    #[allow(unused)]
    None = 0,
    FreeDoc = 1,
    Crng = 2,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum XmlRelaxNGCombine {
    Undefined = 0, // undefined
    Choice,        // choice
    Interleave,    // interleave
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
    parent: XmlRelaxNGGrammarPtr,          // the parent grammar if any
    children: XmlRelaxNGGrammarPtr,        // the children grammar if any
    next: XmlRelaxNGGrammarPtr,            // the next grammar if any
    pub(crate) start: XmlRelaxNGDefinePtr, // <start> content
    combine: XmlRelaxNGCombine,            // the default combine value
    start_list: XmlRelaxNGDefinePtr,       // list of <start> definitions
    defs: Option<XmlHashTableRef<'static, XmlRelaxNGDefinePtr>>, // define
    refs: Option<XmlHashTableRef<'static, XmlRelaxNGDefinePtr>>, // references
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub(crate) enum XmlRelaxNGType {
    Noop = -1, // a no operation from simplification
    #[default]
    Empty = 0, // an empty pattern
    NotAllowed, // not allowed top
    Except,    // except present in nameclass defs
    Text,      // textual content
    Element,   // an element
    Datatype,  // external data type definition
    Param,     // external data type parameter
    Value,     // value from an external data type definition
    List,      // a list of patterns
    Attribute, // an attribute following a pattern
    Def,       // a definition
    Ref,       // reference to a definition
    Externalref, // reference to an external def
    Parentref, // reference to a def in the parent grammar
    Optional,  // optional patterns
    Zeroormore, // zero or more non empty patterns
    Oneormore, // one or more non empty patterns
    Choice,    // a choice between non empty patterns
    Group,     // a pair/group of non empty patterns
    Interleave, // interleaving choice of non-empty patterns
    Start,     // Used to keep track of starts on grammars
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

pub type XmlRelaxNGPtr = *mut XmlRelaxNG;
/// A RelaxNGs definition
#[doc(alias = "xmlRelaxNG")]
#[repr(C)]
pub struct XmlRelaxNG {
    _private: *mut c_void, // unused by the library for users or bindings
    pub(crate) topgrammar: XmlRelaxNGGrammarPtr,
    doc: Option<XmlDocPtr>,

    pub(crate) idref: i32, // requires idref checking

    // It seems that these tables are unused...
    defs: Option<XmlHashTableRef<'static, ()>>, // define
    refs: Option<XmlHashTableRef<'static, ()>>, // references
    documents: XmlRelaxNGDocumentPtr,           // all the documents loaded
    includes: XmlRelaxNGIncludePtr,             // all the includes loaded
    def_tab: Vec<XmlRelaxNGDefinePtr>,          // pointer to the allocated definitions
}

impl Default for XmlRelaxNG {
    fn default() -> Self {
        Self {
            _private: null_mut(),
            topgrammar: null_mut(),
            doc: None,
            idref: 0,
            defs: None,
            refs: None,
            documents: null_mut(),
            includes: null_mut(),
            def_tab: vec![],
        }
    }
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

pub(crate) const FLAGS_IGNORABLE: i32 = 1;
pub(crate) const FLAGS_NEGATIVE: i32 = 2;
const FLAGS_MIXED_CONTENT: i32 = 4;
pub(crate) const FLAGS_NOERROR: i32 = 8;

pub type XmlRelaxNGInterleaveGroupPtr = *mut XmlRelaxNGInterleaveGroup;
/// A RelaxNGs partition set associated to lists of definitions
#[doc(alias = "xmlRelaxNGInterleaveGroup")]
#[repr(C)]
pub struct XmlRelaxNGInterleaveGroup {
    rule: XmlRelaxNGDefinePtr,       // the rule to satisfy
    defs: *mut XmlRelaxNGDefinePtr,  // the array of element definitions
    attrs: *mut XmlRelaxNGDefinePtr, // the array of attributes definitions
}

const IS_DETERMINIST: i32 = 1;
const IS_NEEDCHECK: i32 = 2;

pub type XmlRelaxNGPartitionPtr = *mut XmlRelaxNGPartition;
/// A RelaxNGs partition associated to an interleave group
#[doc(alias = "xmlRelaxNGPartitions")]
#[repr(C)]
pub struct XmlRelaxNGPartition {
    nbgroups: i32,                                 // number of groups in the partitions
    triage: Option<XmlHashTableRef<'static, i32>>, // hash table used to direct nodes to the right group when possible
    flags: i32,                                    // determinist ?
    groups: *mut XmlRelaxNGInterleaveGroupPtr,
}

pub type XmlRelaxNGValidErrorPtr = *mut XmlRelaxNGValidError;
/// A RelaxNGs validation error
// TODO: all fieleds are used in only relaxng module.
#[doc(alias = "xmlRelaxNGValidError")]
#[repr(C)]
#[derive(Default)]
pub struct XmlRelaxNGValidError {
    pub(crate) err: XmlRelaxNGValidErr,         // the error number
    pub(crate) flags: i32,                      // flags
    pub(crate) node: Option<XmlGenericNodePtr>, // the current node
    pub(crate) seq: Option<XmlGenericNodePtr>,  // the current child
    pub(crate) arg1: Option<String>,            // first arg
    pub(crate) arg2: Option<String>,            // second arg
}

pub type XmlRelaxNGIncludePtr = *mut XmlRelaxNGInclude;
/// Structure associated to a RelaxNGs document element
#[doc(alias = "xmlRelaxNGInclude")]
#[repr(C)]
pub struct XmlRelaxNGInclude {
    next: XmlRelaxNGIncludePtr,   // keep a chain of includes
    href: *mut XmlChar,           // the normalized href value
    doc: Option<XmlDocPtr>,       // the associated XML document
    content: XmlRelaxNGDefinePtr, // the definitions
    schema: XmlRelaxNGPtr,        // the schema
}

pub type XmlRelaxNGDocumentPtr = *mut XmlRelaxNGDocument;
/// Structure associated to a RelaxNGs document element
#[doc(alias = "xmlRelaxNGDocument")]
#[repr(C)]
pub struct XmlRelaxNGDocument {
    next: XmlRelaxNGDocumentPtr,  // keep a chain of documents
    href: *mut XmlChar,           // the normalized href value
    doc: Option<XmlDocPtr>,       // the associated XML document
    content: XmlRelaxNGDefinePtr, // the definitions
    schema: XmlRelaxNGPtr,        // the schema
    external_ref: i32,            // 1 if an external ref
}

/// Deallocate RelaxNG partition set structures.
#[doc(alias = "xmlRelaxNGFreePartition")]
pub(crate) unsafe fn xml_relaxng_free_partition(partitions: XmlRelaxNGPartitionPtr) {
    unsafe {
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
            if let Some(mut table) = (*partitions).triage.take().map(|t| t.into_inner()) {
                table.clear();
            }
            xml_free(partitions as _);
        }
    }
}

/// Deallocate a RelaxNG schema structure.
#[doc(alias = "xmlRelaxNGFreeInnerSchema")]
unsafe fn xml_relaxng_free_inner_schema(schema: XmlRelaxNGPtr) {
    unsafe {
        if schema.is_null() {
            return;
        }

        if let Some(doc) = (*schema).doc {
            xml_free_doc(doc);
        }
        for def in (*schema).def_tab.drain(..) {
            xml_relaxng_free_define(def);
        }
        drop_in_place(schema);
        xml_free(schema as _);
    }
}

/// Deallocate a RelaxNG document structure.
#[doc(alias = "xmlRelaxNGFreeDocument")]
pub(crate) unsafe fn xml_relaxng_free_document(docu: XmlRelaxNGDocumentPtr) {
    unsafe {
        if docu.is_null() {
            return;
        }

        if !(*docu).href.is_null() {
            xml_free((*docu).href as _);
        }
        if let Some(doc) = (*docu).doc.take() {
            xml_free_doc(doc);
        }
        if !(*docu).schema.is_null() {
            xml_relaxng_free_inner_schema((*docu).schema);
        }
        xml_free(docu as _);
    }
}

/// Deallocate a RelaxNG document structures.
#[doc(alias = "xmlRelaxNGFreeDocumentList")]
pub(crate) unsafe fn xml_relaxng_free_document_list(mut docu: XmlRelaxNGDocumentPtr) {
    unsafe {
        let mut next: XmlRelaxNGDocumentPtr;

        while !docu.is_null() {
            next = (*docu).next;
            xml_relaxng_free_document(docu);
            docu = next;
        }
    }
}

/// Deallocate a RelaxNG include structure.
#[doc(alias = "xmlRelaxNGFreeInclude")]
unsafe fn xml_relaxng_free_include(incl: XmlRelaxNGIncludePtr) {
    unsafe {
        if incl.is_null() {
            return;
        }

        if !(*incl).href.is_null() {
            xml_free((*incl).href as _);
        }
        if let Some(doc) = (*incl).doc {
            xml_free_doc(doc);
        }
        if !(*incl).schema.is_null() {
            xml_relaxng_free((*incl).schema);
        }
        xml_free(incl as _);
    }
}

/// Deallocate a RelaxNG include structure.
#[doc(alias = "xmlRelaxNGFreeIncludeList")]
pub(crate) unsafe fn xml_relaxng_free_include_list(mut incl: XmlRelaxNGIncludePtr) {
    unsafe {
        let mut next: XmlRelaxNGIncludePtr;

        while !incl.is_null() {
            next = (*incl).next;
            xml_relaxng_free_include(incl);
            incl = next;
        }
    }
}

/// Compute the list of top elements a definition can generate
///
/// Returns a list of elements or NULL if none was found.
#[doc(alias = "xmlRelaxNGGetElements")]
unsafe fn xml_relaxng_get_elements(
    ctxt: XmlRelaxNGParserCtxtPtr,
    def: XmlRelaxNGDefinePtr,
    eora: i32,
) -> *mut XmlRelaxNGDefinePtr {
    unsafe {
        let mut ret: *mut XmlRelaxNGDefinePtr = null_mut();
        let mut parent: XmlRelaxNGDefinePtr;
        let mut cur: XmlRelaxNGDefinePtr;
        let mut tmp: XmlRelaxNGDefinePtr;
        let mut len: i32 = 0;
        let mut max: i32 = 0;

        // Don't run that check in case of error. Infinite recursion becomes possible.
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
                        xml_rng_perr_memory(ctxt, Some("getting element list\n"));
                        return null_mut();
                    }
                } else if max <= len {
                    max *= 2;
                    let temp: *mut XmlRelaxNGDefinePtr = xml_realloc(
                        ret as _,
                        (max as usize + 1) * size_of::<XmlRelaxNGDefinePtr>(),
                    ) as _;
                    if temp.is_null() {
                        xml_rng_perr_memory(ctxt, Some("getting element list\n"));
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
                // Don't go within elements or attributes or string values.
                // Just gather the element top list
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
}

const INVALID_NAME: &str = "\u{1}";

/// Check if the element matches the definition nameClass
///
/// Returns 1 if the element matches, 0 if no, or -1 in case of error
#[doc(alias = "xmlRelaxNGElementMatch")]
unsafe fn xml_relaxng_element_match(
    ctxt: XmlRelaxNGValidCtxtPtr,
    mut define: XmlRelaxNGDefinePtr,
    elem: XmlNodePtr,
) -> i32 {
    unsafe {
        let mut ret: i32 = 0;
        let mut oldflags: i32 = 0;

        if !(*define).name.is_null()
            && elem.name != CStr::from_ptr((*define).name as *const i8).to_string_lossy()
        {
            VALID_ERR3!(
                ctxt,
                XmlRelaxNGValidErr::XmlRelaxngErrElemname,
                Some(
                    CStr::from_ptr((*define).name as *const i8)
                        .to_string_lossy()
                        .as_ref()
                ),
                elem.name().as_deref()
            );
            return 0;
        }
        if !(*define).ns.is_null() && *(*define).ns.add(0) != 0 {
            if let Some(ns) = elem.ns {
                if ns.href() != Some(CStr::from_ptr((*define).ns as *const i8).to_string_lossy()) {
                    VALID_ERR3!(
                        ctxt,
                        XmlRelaxNGValidErr::XmlRelaxngErrElemwrongns,
                        elem.name().as_deref(),
                        Some(
                            CStr::from_ptr((*define).ns as *const i8)
                                .to_string_lossy()
                                .as_ref()
                        )
                    );
                    return 0;
                }
            } else {
                VALID_ERR2!(
                    ctxt,
                    XmlRelaxNGValidErr::XmlRelaxngErrElemnons,
                    elem.name().as_deref()
                );
                return 0;
            }
        } else if elem.ns.is_some() && !(*define).ns.is_null() && (*define).name.is_null() {
            VALID_ERR2!(
                ctxt,
                XmlRelaxNGValidErr::XmlRelaxngErrElemextrans,
                elem.name().as_deref()
            );
            return 0;
        } else if elem.ns.is_some() && !(*define).name.is_null() {
            VALID_ERR2!(
                ctxt,
                XmlRelaxNGValidErr::XmlRelaxngErrElemextrans,
                Some(
                    CStr::from_ptr((*define).name as *const i8)
                        .to_string_lossy()
                        .as_ref()
                )
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
                } else if !(*ctxt).err_tab.is_empty() {
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
}

/// Compare the 2 lists of element definitions. The comparison is
/// that if both lists do not accept the same QNames, it returns 1
/// If the 2 lists can accept the same QName the comparison returns 0
///
/// Returns 1 distinct, 0 if equal
#[doc(alias = "xmlRelaxNGCompareNameClasses")]
unsafe fn xml_relaxng_compare_name_classes(
    def1: XmlRelaxNGDefinePtr,
    def2: XmlRelaxNGDefinePtr,
) -> i32 {
    unsafe {
        let mut ret: i32;
        let mut node = XmlNode::default();
        let mut ns = XmlNs::default();
        let mut ctxt: XmlRelaxNGValidCtxt = XmlRelaxNGValidCtxt::default();

        ctxt.flags = FLAGS_IGNORABLE | FLAGS_NOERROR;

        if matches!(
            (*def1).typ,
            XmlRelaxNGType::Element | XmlRelaxNGType::Attribute
        ) {
            if (*def2).typ == XmlRelaxNGType::Text {
                return 1;
            }
            if !(*def1).name.is_null() {
                node.name = CStr::from_ptr((*def1).name as *const i8)
                    .to_string_lossy()
                    .into_owned()
                    .into();
            } else {
                node.name = Cow::Borrowed(INVALID_NAME);
            }
            if !(*def1).ns.is_null() {
                if *(*def1).ns.add(0) == 0 {
                    node.ns = None;
                } else {
                    node.ns = XmlNsPtr::from_raw(addr_of_mut!(ns)).unwrap();
                    ns.href = Some(
                        CStr::from_ptr((*def1).ns as *const i8)
                            .to_string_lossy()
                            .into_owned()
                            .into(),
                    );
                }
            } else {
                node.ns = None;
            }
            if xml_relaxng_element_match(
                addr_of_mut!(ctxt),
                def2,
                XmlNodePtr::from_raw(addr_of_mut!(node)).unwrap().unwrap(),
            ) != 0
            {
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
                node.name = CStr::from_ptr((*def2).name as *const i8)
                    .to_string_lossy()
                    .into_owned()
                    .into();
            } else {
                node.name = Cow::Borrowed(INVALID_NAME);
            }
            node.ns = XmlNsPtr::from_raw(addr_of_mut!(ns)).unwrap();
            if !(*def2).ns.is_null() {
                if *(*def2).ns.add(0) == 0 {
                    node.ns = None;
                } else {
                    ns.href = Some(
                        CStr::from_ptr((*def2).ns as *const i8)
                            .to_string_lossy()
                            .into_owned()
                            .into(),
                    );
                }
            } else {
                ns.href = Some(INVALID_NAME.into());
            }
            if xml_relaxng_element_match(
                addr_of_mut!(ctxt),
                def1,
                XmlNodePtr::from_raw(addr_of_mut!(node)).unwrap().unwrap(),
            ) != 0
            {
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
}

/// Compare the 2 lists of element or attribute definitions. The comparison
/// is that if both lists do not accept the same QNames, it returns 1
/// If the 2 lists can accept the same QName the comparison returns 0
///
/// Returns 1 distinct, 0 if equal
#[doc(alias = "xmlRelaxNGCompareElemDefLists")]
unsafe fn xml_relaxng_compare_elem_def_lists(
    _ctxt: XmlRelaxNGParserCtxtPtr,
    mut def1: *mut XmlRelaxNGDefinePtr,
    mut def2: *mut XmlRelaxNGDefinePtr,
) -> i32 {
    unsafe {
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
}

/// A lot of work for preprocessing interleave definitions
/// is potentially needed to get a decent execution speed at runtime
///   - trying to get a total order on the element nodes generated
///     by the interleaves, order the list of interleave definitions
///     following that order.
///   - if <text/> is used to handle mixed content, it is better to
///     flag this in the define and simplify the runtime checking
///     algorithm
#[doc(alias = "xmlRelaxNGComputeInterleaves")]
fn xml_relaxng_compute_interleaves(def: XmlRelaxNGDefinePtr, ctxt: XmlRelaxNGParserCtxtPtr) {
    let mut cur: XmlRelaxNGDefinePtr;
    let mut tmp: *mut XmlRelaxNGDefinePtr;
    let mut partitions: XmlRelaxNGPartitionPtr = null_mut();
    let mut group: XmlRelaxNGInterleaveGroupPtr;
    let mut ret: i32;
    let mut nbgroups: i32 = 0;
    let mut nbchild: i32 = 0;
    let mut is_mixed: i32 = 0;
    let mut is_determinist: i32 = 1;

    unsafe {
        // Don't run that check in case of error. Infinite recursion becomes possible.
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

            // Let's check that all rules makes a partitions according to 7.4
            partitions = xml_malloc(size_of::<XmlRelaxNGPartition>()) as _;
            if partitions.is_null() {
                break 'goto_error;
            }
            memset(partitions as _, 0, size_of::<XmlRelaxNGPartition>());
            (*partitions).nbgroups = nbgroups;
            (*partitions).triage = XmlHashTableRef::with_capacity(nbgroups as usize);
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
                        xml_rng_perr!(
                            ctxt,
                            (*def).node.map(|node| node.into()),
                            XmlParserErrors::XmlRngpElemTextConflict,
                            "Element or text conflicts in interleave\n"
                        );
                    }
                    ret = xml_relaxng_compare_elem_def_lists(
                        ctxt,
                        (*group).attrs,
                        (*(*groups.add(j as usize))).attrs,
                    );
                    if ret == 0 {
                        xml_rng_perr!(
                            ctxt,
                            (*def).node.map(|node| node.into()),
                            XmlParserErrors::XmlRngpAttrConflict,
                            "Attributes conflicts in interleave\n"
                        );
                    }
                }
                tmp = (*group).defs;
                if !tmp.is_null() && !(*tmp).is_null() {
                    while !(*tmp).is_null() {
                        if (*(*tmp)).typ == XmlRelaxNGType::Text {
                            if (*partitions)
                                .triage
                                .and_then(|mut t| t.add_entry2("#text", None, i + 1).ok())
                                .is_none()
                            {
                                is_determinist = -1;
                            }
                        } else if (*(*tmp)).typ == XmlRelaxNGType::Element
                            && !(*(*tmp)).name.is_null()
                        {
                            let res = if (*(*tmp)).ns.is_null() || *(*(*tmp)).ns.add(0) == 0 {
                                (*partitions)
                                    .triage
                                    .and_then(|mut t| {
                                        t.add_entry2(
                                            CStr::from_ptr((*(*tmp)).name as *const i8)
                                                .to_string_lossy()
                                                .as_ref(),
                                            None,
                                            i + 1,
                                        )
                                        .ok()
                                    })
                                    .is_none()
                            } else {
                                (*partitions)
                                    .triage
                                    .and_then(|mut t| {
                                        t.add_entry2(
                                            CStr::from_ptr((*(*tmp)).name as *const i8)
                                                .to_string_lossy()
                                                .as_ref(),
                                            (!(*(*tmp)).ns.is_null())
                                                .then(|| {
                                                    CStr::from_ptr((*(*tmp)).ns as *const i8)
                                                        .to_string_lossy()
                                                })
                                                .as_deref(),
                                            i + 1,
                                        )
                                        .ok()
                                    })
                                    .is_none()
                            };
                            if res {
                                is_determinist = -1;
                            }
                        } else if (*(*tmp)).typ == XmlRelaxNGType::Element {
                            let res = if (*(*tmp)).ns.is_null() || *(*(*tmp)).ns.add(0) == 0 {
                                (*partitions)
                                    .triage
                                    .and_then(|mut t| t.add_entry2("#any", None, i + 1).ok())
                                    .is_none()
                            } else {
                                (*partitions)
                                    .triage
                                    .and_then(|mut t| {
                                        t.add_entry2(
                                            "#any",
                                            (!(*(*tmp)).ns.is_null())
                                                .then(|| {
                                                    CStr::from_ptr((*(*tmp)).ns as *const i8)
                                                        .to_string_lossy()
                                                })
                                                .as_deref(),
                                            i + 1,
                                        )
                                        .ok()
                                    })
                                    .is_none()
                            };
                            if !(*(*tmp)).name_class.is_null() {
                                is_determinist = 2;
                            }
                            if res {
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

            // and save the partition list back in the def
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
        xml_rng_perr_memory(ctxt, Some("in interleave computation\n"));
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

/// Check if a string is ignorable c.f. 4.2. Whitespace
///
/// Returns 1 if the string is NULL or made of blanks chars, 0 otherwise
#[doc(alias = "xmlRelaxNGIsBlank")]
fn xml_relaxng_is_blank(s: Option<&str>) -> bool {
    s.is_none_or(|s| s.chars().all(|c| c.is_xml_blank_char()))
}

/// Check all the attributes on the given node
#[doc(alias = "xmlRelaxNGCleanupAttributes")]
unsafe fn xml_relaxng_cleanup_attributes(ctxt: XmlRelaxNGParserCtxtPtr, node: XmlNodePtr) {
    unsafe {
        let mut cur = node.properties;
        while let Some(cur_attr) = cur {
            let next = cur_attr.next;
            if cur_attr
                .ns
                .is_none_or(|ns| ns.href().as_deref() == Some(XML_RELAXNG_NS))
            {
                if cur_attr.name().as_deref() == Some("name") {
                    if node.name().as_deref() != Some("element")
                        && node.name().as_deref() != Some("attribute")
                        && node.name().as_deref() != Some("ref")
                        && node.name().as_deref() != Some("parentRef")
                        && node.name().as_deref() != Some("param")
                        && node.name().as_deref() != Some("define")
                    {
                        xml_rng_perr!(
                            ctxt,
                            Some(node.into()),
                            XmlParserErrors::XmlRngpForbiddenAttribute,
                            "Attribute {} is not allowed on {}\n",
                            cur_attr.name().unwrap().into_owned(),
                            node.name().unwrap().into_owned()
                        );
                    }
                } else if cur_attr.name().as_deref() == Some("type") {
                    if node.name().as_deref() != Some("value")
                        && node.name().as_deref() != Some("data")
                    {
                        xml_rng_perr!(
                            ctxt,
                            Some(node.into()),
                            XmlParserErrors::XmlRngpForbiddenAttribute,
                            "Attribute {} is not allowed on {}\n",
                            cur_attr.name().unwrap().into_owned(),
                            node.name().unwrap().into_owned()
                        );
                    }
                } else if cur_attr.name().as_deref() == Some("href") {
                    if node.name().as_deref() != Some("externalRef")
                        && node.name().as_deref() != Some("include")
                    {
                        xml_rng_perr!(
                            ctxt,
                            Some(node.into()),
                            XmlParserErrors::XmlRngpForbiddenAttribute,
                            "Attribute {} is not allowed on {}\n",
                            cur_attr.name().unwrap().into_owned(),
                            node.name().unwrap().into_owned()
                        );
                    }
                } else if cur_attr.name().as_deref() == Some("combine") {
                    if node.name().as_deref() != Some("start")
                        && node.name().as_deref() != Some("define")
                    {
                        xml_rng_perr!(
                            ctxt,
                            Some(node.into()),
                            XmlParserErrors::XmlRngpForbiddenAttribute,
                            "Attribute {} is not allowed on {}\n",
                            cur_attr.name().unwrap().into_owned(),
                            node.name().unwrap().into_owned()
                        );
                    }
                } else if cur_attr.name().as_deref() == Some("datatypeLibrary") {
                    if let Some(val) = cur_attr.children().and_then(|c| c.get_string(node.doc, 1)) {
                        if !val.is_empty() {
                            if let Some(uri) = XmlURI::parse(&val) {
                                if uri.scheme.is_none() {
                                    xml_rng_perr!(
                                        ctxt,
                                        Some(node.into()),
                                        XmlParserErrors::XmlRngpURINotAbsolute,
                                        "Attribute {} URI {} is not absolute\n",
                                        cur_attr.name().unwrap().into_owned(),
                                        val
                                    );
                                }
                                if uri.fragment.is_some() {
                                    xml_rng_perr!(
                                        ctxt,
                                        Some(node.into()),
                                        XmlParserErrors::XmlRngpURIFragment,
                                        "Attribute {} URI {} has a fragment ID\n",
                                        cur_attr.name().unwrap().into_owned(),
                                        val
                                    );
                                }
                            } else {
                                xml_rng_perr!(
                                    ctxt,
                                    Some(node.into()),
                                    XmlParserErrors::XmlRngpInvalidURI,
                                    "Attribute {} contains invalid URI {}\n",
                                    cur_attr.name().unwrap().into_owned(),
                                    val
                                );
                            }
                        }
                    }
                } else if cur_attr.name().as_deref() != Some("ns") {
                    xml_rng_perr!(
                        ctxt,
                        Some(node.into()),
                        XmlParserErrors::XmlRngpUnknownAttribute,
                        "Unknown attribute {} on {}\n",
                        cur_attr.name().unwrap().into_owned(),
                        node.name().unwrap().into_owned()
                    );
                }
            }
            cur = next;
        }
    }
}

/// First lookup if the document is already loaded into the parser context,
/// check against recursion. If not found the resource is loaded and
/// the content is preprocessed before being returned back to the caller.
///
/// Returns the xmlRelaxNGDocumentPtr or NULL in case of error
#[doc(alias = "xmlRelaxNGLoadExternalRef")]
unsafe fn xml_relaxng_load_external_ref(
    ctxt: XmlRelaxNGParserCtxtPtr,
    url: &str,
    ns: Option<&str>,
) -> XmlRelaxNGDocumentPtr {
    unsafe {
        // check against recursion in the stack
        for &doc in &(*ctxt).doc_tab {
            let curl = CString::new(url).unwrap();
            if xml_str_equal((*doc).href, curl.as_ptr() as *const u8) {
                xml_rng_perr!(
                    ctxt,
                    None,
                    XmlParserErrors::XmlRngpExternalRefRecurse,
                    "Detected an externalRef recursion for {}\n",
                    url
                );
                return null_mut();
            }
        }

        // load the document
        let Some(doc) = xml_read_file(url, None, 0) else {
            xml_rng_perr!(
                ctxt,
                None,
                XmlParserErrors::XmlRngpParseError,
                "xmlRelaxNG: could not load {}\n",
                url
            );
            return null_mut();
        };

        // Allocate the document structures and register it first.
        let ret: XmlRelaxNGDocumentPtr = xml_malloc(size_of::<XmlRelaxNGDocument>()) as _;
        if ret.is_null() {
            xml_rng_perr!(
                ctxt,
                Some(doc.into()),
                XmlParserErrors::XmlErrNoMemory,
                "xmlRelaxNG: allocate memory for doc {}\n",
                url
            );
            xml_free_doc(doc);
            return null_mut();
        }
        memset(ret as _, 0, size_of::<XmlRelaxNGDocument>());
        (*ret).doc = Some(doc);
        let url = CString::new(url).unwrap();
        (*ret).href = xml_strdup(url.as_ptr() as *const u8);
        (*ret).next = (*ctxt).documents;
        (*ret).external_ref = 1;
        (*ctxt).documents = ret;

        // transmit the ns if needed
        if let Some(ns) = ns {
            if let Some(mut root) = doc
                .get_root_element()
                .filter(|root| root.has_prop("ns").is_none())
            {
                root.set_prop("ns", Some(ns));
            }
        }

        // push it on the stack and register it in the hash table
        (*ctxt).document_push(ret);

        // Some preprocessing of the document content
        if xml_relaxng_cleanup_doc(ctxt, doc).is_none() {
            (*ctxt).doc = null_mut();
            return null_mut();
        };

        (*ctxt).document_pop();

        ret
    }
}

/// Applies the elimination algorithm of 4.7
///
/// Returns 0 in case of error, 1 in case of success.
#[doc(alias = "xmlRelaxNGRemoveRedefine")]
unsafe fn xml_relaxng_remove_redefine(
    _ctxt: XmlRelaxNGParserCtxtPtr,
    _url: *const XmlChar,
    target: Option<XmlNodePtr>,
    name: *const XmlChar,
) -> i32 {
    unsafe {
        let mut found: i32 = 0;
        let mut tmp = target;
        while let Some(mut now) = tmp {
            let tmp2 = now.next;
            if name.is_null() && is_relaxng(now, "start") {
                found = 1;
                now.unlink();
                xml_free_node(now);
            } else if !name.is_null() && is_relaxng(now, "define") {
                if let Some(name2) = now.get_prop("name") {
                    let name2 = normalize_external_space(&name2);
                    if CStr::from_ptr(name as *const i8).to_string_lossy().as_ref() == name2 {
                        found = 1;
                        now.unlink();
                        xml_free_node(now);
                    }
                }
            } else if is_relaxng(now, "include") {
                let href: *mut XmlChar = null_mut();
                let inc: XmlRelaxNGDocumentPtr = now.psvi as _;

                if !inc.is_null()
                    && (*inc).doc.is_some_and(|doc| {
                        doc.children.is_some()
                            && doc.children().unwrap().name().as_deref() == Some("grammar")
                            && xml_relaxng_remove_redefine(
                                _ctxt,
                                href,
                                doc.get_root_element()
                                    .and_then(|root| root.children)
                                    .map(|c| XmlNodePtr::try_from(c).unwrap()),
                                name,
                            ) == 1
                    })
                {
                    found = 1;
                }
                if xml_relaxng_remove_redefine(
                    _ctxt,
                    _url,
                    now.children.map(|c| XmlNodePtr::try_from(c).unwrap()),
                    name,
                ) == 1
                {
                    found = 1;
                }
            }
            tmp = tmp2.map(|node| XmlNodePtr::try_from(node).unwrap());
        }
        found
    }
}

/// First lookup if the document is already loaded into the parser context,
/// check against recursion. If not found the resource is loaded and
/// the content is preprocessed before being returned back to the caller.
///
/// Returns the xmlRelaxNGIncludePtr or NULL in case of error
#[doc(alias = "xmlRelaxNGLoadInclude")]
unsafe fn xml_relaxng_load_include(
    ctxt: XmlRelaxNGParserCtxtPtr,
    url: &str,
    node: XmlNodePtr,
    ns: Option<&str>,
) -> XmlRelaxNGIncludePtr {
    unsafe {
        // check against recursion in the stack
        for &inc in &(*ctxt).inc_tab {
            let curl = CString::new(url).unwrap();
            if xml_str_equal((*inc).href, curl.as_ptr() as *const u8) {
                xml_rng_perr!(
                    ctxt,
                    None,
                    XmlParserErrors::XmlRngpIncludeRecurse,
                    "Detected an Include recursion for {}\n",
                    url
                );
                return null_mut();
            }
        }

        // load the document
        let Some(doc) = xml_read_file(url, None, 0) else {
            xml_rng_perr!(
                ctxt,
                Some(node.into()),
                XmlParserErrors::XmlRngpParseError,
                "xmlRelaxNG: could not load {}\n",
                url
            );
            return null_mut();
        };

        // Allocate the document structures and register it first.
        let ret: XmlRelaxNGIncludePtr = xml_malloc(size_of::<XmlRelaxNGInclude>()) as _;
        if ret.is_null() {
            xml_rng_perr_memory(ctxt, Some("allocating include\n"));
            xml_free_doc(doc);
            return null_mut();
        }
        memset(ret as _, 0, size_of::<XmlRelaxNGInclude>());
        (*ret).doc = Some(doc);
        let curl = CString::new(url).unwrap();
        (*ret).href = xml_strdup(curl.as_ptr() as *const u8);
        (*ret).next = (*ctxt).includes;
        (*ctxt).includes = ret;

        // transmit the ns if needed
        if let Some(ns) = ns {
            if let Some(mut root) = doc
                .get_root_element()
                .filter(|root| root.has_prop("ns").is_none())
            {
                root.set_prop("ns", Some(ns));
            }
        }

        // push it on the stack
        (*ctxt).include_push(ret);

        // Some preprocessing of the document content, this include recursing
        // in the include stack.

        let Some(doc) = xml_relaxng_cleanup_doc(ctxt, doc) else {
            (*ctxt).inc = null_mut();
            return null_mut();
        };

        // Pop up the include from the stack
        (*ctxt).include_pop();

        // Check that the top element is a grammar
        let Some(root) = doc.get_root_element() else {
            xml_rng_perr!(
                ctxt,
                Some(node.into()),
                XmlParserErrors::XmlRngpEmpty,
                "xmlRelaxNG: included document is empty {}\n",
                url
            );
            return null_mut();
        };
        if !is_relaxng(root, "grammar") {
            xml_rng_perr!(
                ctxt,
                Some(node.into()),
                XmlParserErrors::XmlRngpGrammarMissing,
                "xmlRelaxNG: included document {} root is not a grammar\n",
                url
            );
            return null_mut();
        }

        // Elimination of redefined rules in the include.
        let mut cur = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
        while let Some(cur_node) = cur {
            if is_relaxng(cur_node, "start") {
                let found: i32 = xml_relaxng_remove_redefine(
                    ctxt,
                    curl.as_ptr() as *const u8,
                    root.children.map(|c| XmlNodePtr::try_from(c).unwrap()),
                    null_mut(),
                );
                if found == 0 {
                    xml_rng_perr!(
                        ctxt,
                        Some(node.into()),
                        XmlParserErrors::XmlRngpStartMissing,
                        "xmlRelaxNG: include {} has a start but not the included grammar\n",
                        url
                    );
                }
            } else if is_relaxng(cur_node, "define") {
                if let Some(name) = cur_node.get_prop("name") {
                    let name = normalize_external_space(&name);
                    let cname = CString::new(name).unwrap();
                    let found: i32 = xml_relaxng_remove_redefine(
                        ctxt,
                        curl.as_ptr() as *const u8,
                        root.children.map(|c| XmlNodePtr::try_from(c).unwrap()),
                        cname.as_ptr() as *const u8,
                    );
                    if found == 0 {
                        xml_rng_perr!(
                            ctxt,
                            Some(node.into()),
                            XmlParserErrors::XmlRngpDefineMissing,
                            "xmlRelaxNG: include {} has a define {} but not the included grammar\n",
                            url,
                            name
                        );
                    }
                } else {
                    xml_rng_perr!(
                        ctxt,
                        Some(node.into()),
                        XmlParserErrors::XmlRngpNameMissing,
                        "xmlRelaxNG: include {} has define without name\n",
                        url
                    );
                }
            }
            if let Some(children) = cur_node
                .children()
                .filter(|_| is_relaxng(cur_node, "div"))
                .map(|children| XmlNodePtr::try_from(children).unwrap())
            {
                cur = Some(children);
            } else if let Some(next) = cur_node
                .next
                .map(|node| XmlNodePtr::try_from(node).unwrap())
            {
                cur = Some(next);
            } else {
                let mut cur_node = cur_node;
                while cur_node.parent() != Some(node.into())
                    && cur_node.parent().unwrap().next().is_none()
                {
                    cur_node = cur_node
                        .parent()
                        .map(|p| XmlNodePtr::try_from(p).unwrap())
                        .unwrap();
                }
                cur = if cur_node.parent() != Some(node.into()) {
                    cur_node
                        .parent()
                        .unwrap()
                        .next()
                        .map(|n| XmlNodePtr::try_from(n).unwrap())
                } else {
                    None
                };
            }
        }

        ret
    }
}

/// Cleanup the subtree from unwanted nodes for parsing, resolve
/// Include and externalRef lookups.
#[doc(alias = "xmlRelaxNGCleanupTree")]
unsafe fn xml_relaxng_cleanup_tree(ctxt: XmlRelaxNGParserCtxtPtr, root: XmlNodePtr) {
    unsafe {
        let mut delete: Option<XmlGenericNodePtr> = None;
        let mut cur: Option<XmlGenericNodePtr> = Some(root.into());
        'main: while let Some(now) = cur {
            if let Some(mut delete) = delete.take() {
                delete.unlink();
                xml_free_node(delete);
            }

            'skip_children: {
                match XmlNodePtr::try_from(now) {
                    Ok(mut now) if now.element_type() == XmlElementType::XmlElementNode => {
                        // Simplification 4.1. Annotations
                        if now
                            .ns
                            .is_none_or(|ns| ns.href().as_deref() != Some(XML_RELAXNG_NS))
                        {
                            if let Some(parent) = now.parent().filter(|p| {
                                p.element_type() == XmlElementType::XmlElementNode
                                    && (p.name().as_deref() == Some("name")
                                        || p.name().as_deref() == Some("value")
                                        || p.name().as_deref() == Some("param"))
                            }) {
                                xml_rng_perr!(
                                    ctxt,
                                    Some(now.into()),
                                    XmlParserErrors::XmlRngpForeignElement,
                                    "element {} doesn't allow foreign elements\n",
                                    parent.name().unwrap().into_owned()
                                );
                            }
                            delete = Some(now.into());
                            break 'skip_children;
                        } else {
                            xml_relaxng_cleanup_attributes(ctxt, now);
                            if now.name().as_deref() == Some("externalRef") {
                                let mut ns = now.get_prop("ns");
                                if ns.is_none() {
                                    let mut tmp =
                                        now.parent().and_then(|p| XmlNodePtr::try_from(p).ok());
                                    while let Some(now) = tmp.filter(|tmp| {
                                        tmp.element_type() == XmlElementType::XmlElementNode
                                    }) {
                                        ns = now.get_prop("ns");
                                        if ns.is_some() {
                                            break;
                                        }
                                        tmp =
                                            now.parent().and_then(|p| XmlNodePtr::try_from(p).ok());
                                    }
                                }
                                let Some(href) = now.get_prop("href") else {
                                    xml_rng_perr!(
                                        ctxt,
                                        Some(now.into()),
                                        XmlParserErrors::XmlRngpMissingHref,
                                        "xmlRelaxNGParse: externalRef has no href attribute\n"
                                    );
                                    delete = Some(now.into());
                                    break 'skip_children;
                                };
                                let Some(uri) = XmlURI::parse(&href) else {
                                    xml_rng_perr!(
                                        ctxt,
                                        Some(now.into()),
                                        XmlParserErrors::XmlRngpHrefError,
                                        "Incorrect URI for externalRef {}\n",
                                        href
                                    );
                                    delete = Some(now.into());
                                    break 'skip_children;
                                };
                                if uri.fragment.is_some() {
                                    xml_rng_perr!(
                                        ctxt,
                                        Some(now.into()),
                                        XmlParserErrors::XmlRngpHrefError,
                                        "Fragment forbidden in URI for externalRef {}\n",
                                        href
                                    );
                                    delete = Some(now.into());
                                    break 'skip_children;
                                }
                                let Some(url) = now
                                    .get_base(now.doc)
                                    .and_then(|base| build_uri(&href, &base))
                                else {
                                    xml_rng_perr!(
                                        ctxt,
                                        Some(now.into()),
                                        XmlParserErrors::XmlRngpHrefError,
                                        "Failed to compute URL for externalRef {}\n",
                                        href
                                    );
                                    delete = Some(now.into());
                                    break 'skip_children;
                                };
                                let docu: XmlRelaxNGDocumentPtr =
                                    xml_relaxng_load_external_ref(ctxt, &url, ns.as_deref());
                                if docu.is_null() {
                                    xml_rng_perr!(
                                        ctxt,
                                        Some(now.into()),
                                        XmlParserErrors::XmlRngpExternalRefFailure,
                                        "Failed to load externalRef {}\n",
                                        url
                                    );
                                    delete = Some(now.into());
                                    break 'skip_children;
                                }
                                now.psvi = docu as _;
                            } else if now.name().as_deref() == Some("include") {
                                let Some(href) = now.get_prop("href") else {
                                    xml_rng_perr!(
                                        ctxt,
                                        Some(now.into()),
                                        XmlParserErrors::XmlRngpMissingHref,
                                        "xmlRelaxNGParse: include has no href attribute\n"
                                    );
                                    delete = Some(now.into());
                                    break 'skip_children;
                                };

                                let Some(url) = now
                                    .get_base(now.doc)
                                    .and_then(|base| build_uri(&href, &base))
                                else {
                                    xml_rng_perr!(
                                        ctxt,
                                        Some(now.into()),
                                        XmlParserErrors::XmlRngpHrefError,
                                        "Failed to compute URL for include {}\n",
                                        href
                                    );
                                    delete = Some(now.into());
                                    break 'skip_children;
                                };
                                let mut ns = now.get_prop("ns");
                                if ns.is_none() {
                                    let mut tmp =
                                        now.parent().and_then(|p| XmlNodePtr::try_from(p).ok());
                                    while let Some(now) = tmp.filter(|tmp| {
                                        tmp.element_type() == XmlElementType::XmlElementNode
                                    }) {
                                        ns = now.get_prop("ns");
                                        if ns.is_some() {
                                            break;
                                        }
                                        tmp =
                                            now.parent().and_then(|p| XmlNodePtr::try_from(p).ok());
                                    }
                                }
                                let incl: XmlRelaxNGIncludePtr =
                                    xml_relaxng_load_include(ctxt, &url, now, ns.as_deref());
                                if incl.is_null() {
                                    xml_rng_perr!(
                                        ctxt,
                                        Some(now.into()),
                                        XmlParserErrors::XmlRngpIncludeFailure,
                                        "Failed to load include {}\n",
                                        url
                                    );
                                    delete = Some(now.into());
                                    break 'skip_children;
                                }
                                now.psvi = incl as _;
                            } else if now.name().as_deref() == Some("element")
                                || now.name().as_deref() == Some("attribute")
                            {
                                // Simplification 4.8. name attribute of element
                                // and attribute elements
                                if let Some(name) = now.get_prop("name") {
                                    let text = if let Some(children) = now.children() {
                                        xml_new_doc_node(now.doc, now.ns, "name", None).map(
                                            |mut node| {
                                                children.add_prev_sibling(node.into());
                                                let text = xml_new_doc_text(node.doc, Some(&name));
                                                node.add_child(text.unwrap().into());
                                                node
                                            },
                                        )
                                    } else {
                                        xml_new_child(now.into(), now.ns, "name", Some(&name))
                                    };
                                    if text.is_none() {
                                        xml_rng_perr!(
                                            ctxt,
                                            Some(now.into()),
                                            XmlParserErrors::XmlRngpCreateFailure,
                                            "Failed to create a name {} element\n",
                                            name
                                        );
                                    }
                                    now.unset_prop("name");
                                    if let Some(ns) = now.get_prop("ns") {
                                        if let Some(mut text) = text {
                                            text.set_prop("ns", Some(ns.as_str()));
                                            // xmlUnsetProp(cur, c"ns".as_ptr() as _);
                                        }
                                    } else if now.name == "attribute" {
                                        if let Some(mut text) = text {
                                            text.set_prop("ns", Some(""));
                                        }
                                    }
                                }
                            } else if now.name().as_deref() == Some("name")
                                || now.name().as_deref() == Some("nsName")
                                || now.name().as_deref() == Some("value")
                            {
                                // Simplification 4.8. name attribute of element
                                // and attribute elements
                                if now.has_prop("ns").is_none() {
                                    let mut ns = None;

                                    let mut node = now.parent();
                                    while let Some(cur) = node
                                        .filter(|node| {
                                            node.element_type() == XmlElementType::XmlElementNode
                                        })
                                        .map(|node| XmlNodePtr::try_from(node).unwrap())
                                    {
                                        ns = cur.get_prop("ns");
                                        if ns.is_some() {
                                            break;
                                        }
                                        node = cur.parent();
                                    }
                                    if let Some(ns) = ns {
                                        now.set_prop("ns", Some(ns.as_str()));
                                    } else {
                                        now.set_prop("ns", Some(""));
                                    }
                                }
                                if now.name().as_deref() == Some("name") {
                                    // Simplification: 4.10. QNames
                                    if let Some(name) = now.get_content() {
                                        if let Some((prefix, local)) = split_qname2(&name) {
                                            let doc = now.doc;
                                            if let Some(ns) = now.search_ns(doc, Some(prefix)) {
                                                now.set_prop("ns", ns.href().as_deref());
                                                now.set_content(local);
                                            } else {
                                                xml_rng_perr!(
                                                    ctxt,
                                                    Some(now.into()),
                                                    XmlParserErrors::XmlRngpPrefixUndefined,
                                                    "xmlRelaxNGParse: no namespace for prefix {}\n",
                                                    prefix
                                                );
                                            }
                                        }
                                    }
                                }
                                // 4.16
                                if now.name().as_deref() == Some("nsName")
                                    && (*ctxt).flags & XML_RELAXNG_IN_NSEXCEPT != 0
                                {
                                    xml_rng_perr!(
                                        ctxt,
                                        Some(now.into()),
                                        XmlParserErrors::XmlRngpPatNsNameExceptNsName,
                                        "Found nsName/except//nsName forbidden construct\n"
                                    );
                                }
                            } else if now.name().as_deref() == Some("except") && now != root {
                                let oldflags: i32 = (*ctxt).flags;

                                // 4.16
                                if now
                                    .parent()
                                    .filter(|p| p.name().as_deref() == Some("anyName"))
                                    .is_some()
                                {
                                    (*ctxt).flags |= XML_RELAXNG_IN_ANYEXCEPT;
                                    xml_relaxng_cleanup_tree(ctxt, now);
                                    (*ctxt).flags = oldflags;
                                    break 'skip_children;
                                } else if now
                                    .parent()
                                    .filter(|p| p.name().as_deref() == Some("nsName"))
                                    .is_some()
                                {
                                    (*ctxt).flags |= XML_RELAXNG_IN_NSEXCEPT;
                                    xml_relaxng_cleanup_tree(ctxt, now);
                                    (*ctxt).flags = oldflags;
                                    break 'skip_children;
                                }
                            } else if now.name().as_deref() == Some("anyName") {
                                // 4.16
                                if (*ctxt).flags & XML_RELAXNG_IN_ANYEXCEPT != 0 {
                                    xml_rng_perr!(
                                        ctxt,
                                        Some(now.into()),
                                        XmlParserErrors::XmlRngpPatAnynameExceptAnyname,
                                        "Found anyName/except//anyName forbidden construct\n"
                                    );
                                } else if (*ctxt).flags & XML_RELAXNG_IN_NSEXCEPT != 0 {
                                    xml_rng_perr!(
                                        ctxt,
                                        Some(now.into()),
                                        XmlParserErrors::XmlRngpPatNsNameExceptAnyName,
                                        "Found nsName/except//anyName forbidden construct\n"
                                    );
                                }
                            }
                            // This is not an else since "include" is transformed into a div
                            if now.name().as_deref() == Some("div") {
                                // implements rule 4.11
                                let ns = now.get_prop("ns");
                                let mut child = now.children();
                                let mut ins = now;
                                while let Some(mut cur_node) = child {
                                    if let Some((mut cur_node, ns)) =
                                        XmlNodePtr::try_from(cur_node).ok().and_then(|cur_node| {
                                            Some(cur_node).zip(
                                                ns.as_deref()
                                                    .filter(|_| cur_node.has_prop("ns").is_none()),
                                            )
                                        })
                                    {
                                        cur_node.set_prop("ns", Some(ns));
                                    }
                                    let tmp = cur_node.next();
                                    cur_node.unlink();
                                    ins = XmlNodePtr::try_from(
                                        ins.add_next_sibling(cur_node).unwrap(),
                                    )
                                    .unwrap();
                                    child = tmp;
                                }
                                // Since we are about to delete cur, if its nsDef is non-NULL we
                                // need to preserve it (it contains the ns definitions for the
                                // children we just moved).  We'll just stick it on to the end
                                // of now.parent's list, since it's never going to be re-serialized
                                // (bug 143738).
                                if let Some(mut parent) = now
                                    .parent()
                                    .map(|parent| XmlNodePtr::try_from(parent).unwrap())
                                {
                                    if let Some(ns_def) = now.ns_def.take() {
                                        if let Some(par_def) = parent.ns_def {
                                            let mut last = par_def;
                                            while let Some(next) = last.next {
                                                last = next;
                                            }
                                            last.next = Some(ns_def);
                                        } else {
                                            parent.ns_def = Some(ns_def);
                                        }
                                    }
                                }
                                delete = Some(now.into());
                                break 'skip_children;
                            }
                        }
                    }
                    Ok(now)
                        if matches!(
                            now.element_type(),
                            XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
                        ) =>
                    {
                        // Simplification 4.2 whitespaces
                        if xml_relaxng_is_blank(now.content.as_deref()) {
                            if let Some(parent) = now
                                .parent()
                                .filter(|p| p.element_type() == XmlElementType::XmlElementNode)
                            {
                                if parent.name().as_deref() != Some("value")
                                    && parent.name().as_deref() != Some("param")
                                {
                                    delete = Some(now.into());
                                }
                            } else {
                                delete = Some(now.into());
                                break 'skip_children;
                            }
                        }
                    }
                    Ok(now) => {
                        delete = Some(now.into());
                        break 'skip_children;
                    }
                    _ => {
                        delete = Some(now);
                        break 'skip_children;
                    }
                }

                // Skip to next node
                if let Some(children) = now.children().filter(|children| {
                    !matches!(
                        children.element_type(),
                        XmlElementType::XmlEntityDecl
                            | XmlElementType::XmlEntityRefNode
                            | XmlElementType::XmlEntityNode
                    )
                }) {
                    cur = Some(XmlNodePtr::try_from(children).unwrap().into());
                    continue 'main;
                }
            }
            // skip_children:
            if let Some(next) = now.next() {
                cur = Some(XmlNodePtr::try_from(next).unwrap().into());
                continue;
            }

            cur = now.parent();
            while let Some(parent) = cur {
                if parent == XmlGenericNodePtr::from(root) {
                    cur = None;
                    break;
                }

                if let Some(next) = parent.next() {
                    cur = Some(next);
                    break;
                }
                cur = parent.parent();
            }
        }
        if let Some(mut delete) = delete.take() {
            delete.unlink();
            xml_free_node(delete);
        }
    }
}

/// Cleanup the document from unwanted nodes for parsing, resolve
/// Include and externalRef lookups.
///
/// Returns the cleaned up document or NULL in case of error
#[doc(alias = "xmlRelaxNGCleanupDoc")]
unsafe fn xml_relaxng_cleanup_doc(
    ctxt: XmlRelaxNGParserCtxtPtr,
    doc: XmlDocPtr,
) -> Option<XmlDocPtr> {
    unsafe {
        // Extract the root
        let Some(root) = doc.get_root_element() else {
            xml_rng_perr!(
                ctxt,
                Some(doc.into()),
                XmlParserErrors::XmlRngpEmpty,
                "xmlRelaxNGParse: {} is empty\n",
                (*ctxt).url.as_deref().unwrap()
            );
            return None;
        };
        xml_relaxng_cleanup_tree(ctxt, root);
        Some(doc)
    }
}

/// Allocate a new RelaxNG structure.
///
/// Returns the newly allocated structure or NULL in case or error
#[doc(alias = "xmlRelaxNGNewRelaxNG")]
unsafe fn xml_relaxng_new_relaxng(ctxt: XmlRelaxNGParserCtxtPtr) -> XmlRelaxNGPtr {
    unsafe {
        let ret: XmlRelaxNGPtr = xml_malloc(size_of::<XmlRelaxNG>()) as _;
        if ret.is_null() {
            xml_rng_perr_memory(ctxt, None);
            return null_mut();
        }
        std::ptr::write(&mut *ret, XmlRelaxNG::default());

        ret
    }
}

/// Applies the 4.17. combine attribute rule for all the define
/// element of a given grammar using the same name.
#[doc(alias = "xmlRelaxNGCheckCombine")]
unsafe fn xml_relaxng_check_combine(
    define: XmlRelaxNGDefinePtr,
    ctxt: XmlRelaxNGParserCtxtPtr,
    name: &str,
) {
    unsafe {
        let mut choice_or_interleave: i32 = -1;
        let mut missing: i32 = 0;
        let mut cur: XmlRelaxNGDefinePtr;
        let mut last: XmlRelaxNGDefinePtr;
        let mut tmp: XmlRelaxNGDefinePtr;
        let mut tmp2: XmlRelaxNGDefinePtr;

        if (*define).next_hash.is_null() {
            return;
        }
        cur = define;
        while !cur.is_null() {
            if let Some(combine) = (*cur).node.unwrap().get_prop("combine") {
                if combine == "choice" {
                    if choice_or_interleave == -1 {
                        choice_or_interleave = 1;
                    } else if choice_or_interleave == 0 {
                        xml_rng_perr!(
                            ctxt,
                            (*define).node.map(|node| node.into()),
                            XmlParserErrors::XmlRngpDefChoiceAndInterleave,
                            "Defines for {} use both 'choice' and 'interleave'\n",
                            name
                        );
                    }
                } else if combine == "interleave" {
                    if choice_or_interleave == -1 {
                        choice_or_interleave = 0;
                    } else if choice_or_interleave == 1 {
                        xml_rng_perr!(
                            ctxt,
                            (*define).node.map(|node| node.into()),
                            XmlParserErrors::XmlRngpDefChoiceAndInterleave,
                            "Defines for {} use both 'choice' and 'interleave'\n",
                            name
                        );
                    }
                } else {
                    xml_rng_perr!(
                        ctxt,
                        (*define).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpUnknownCombine,
                        "Defines for {} use unknown combine value '{}''\n",
                        name,
                        combine
                    );
                }
            } else if missing == 0 {
                missing = 1;
            } else {
                xml_rng_perr!(
                    ctxt,
                    (*define).node.map(|node| node.into()),
                    XmlParserErrors::XmlRngpNeedCombine,
                    "Some defines for {} needs the combine attribute\n",
                    name
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
                    // we need first to create a wrapper.
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
            if (*ctxt).interleaves.is_none() {
                (*ctxt).interleaves = XmlHashTableRef::with_capacity(10);
            }
            if let Some(mut table) = (*ctxt).interleaves {
                let mut tmpname: [c_char; 32] = [0; 32];

                snprintf(
                    tmpname.as_mut_ptr(),
                    32,
                    c"interleave%d".as_ptr() as _,
                    (*ctxt).nb_interleaves,
                );
                (*ctxt).nb_interleaves += 1;
                if table
                    .add_entry(
                        CStr::from_ptr(tmpname.as_ptr()).to_string_lossy().as_ref(),
                        cur,
                    )
                    .is_err()
                {
                    let tmpname = CStr::from_ptr(tmpname.as_ptr()).to_string_lossy();
                    xml_rng_perr!(
                        ctxt,
                        (*define).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpInterleaveCreateFailed,
                        "Failed to add {} to hash table\n",
                        tmpname
                    );
                }
            } else {
                xml_rng_perr!(
                    ctxt,
                    (*define).node.map(|node| node.into()),
                    XmlParserErrors::XmlRngpInterleaveCreateFailed,
                    "Failed to create interleaves hash table\n"
                );
            }
        }
    }
}

/// Applies the 4.17. combine attribute rule for all the define
/// element of a given grammar using the same name.
#[doc(alias = "xmlRelaxNGCheckReference")]
fn xml_relaxng_check_reference(
    refe: XmlRelaxNGDefinePtr,
    ctxt: XmlRelaxNGParserCtxtPtr,
    name: *const XmlChar,
) {
    let mut cur: XmlRelaxNGDefinePtr;

    // Those rules don't apply to imported ref from xmlRelaxNGParseImportRef
    unsafe {
        if (*refe).dflags & IS_EXTERNAL_REF as i16 != 0 {
            return;
        }

        let grammar: XmlRelaxNGGrammarPtr = (*ctxt).grammar;
        if grammar.is_null() {
            let name = CStr::from_ptr(name as *const i8).to_string_lossy();
            xml_rng_perr!(
                ctxt,
                (*refe).node.map(|node| node.into()),
                XmlParserErrors::XmlErrInternalError,
                "Internal error: no grammar in CheckReference {}\n",
                name
            );
            return;
        }
        if !(*refe).content.is_null() {
            let name = CStr::from_ptr(name as *const i8).to_string_lossy();
            xml_rng_perr!(
                ctxt,
                (*refe).node.map(|node| node.into()),
                XmlParserErrors::XmlErrInternalError,
                "Internal error: reference has content in CheckReference {}\n",
                name
            );
            return;
        }
        if let Some(defs) = (*grammar).defs {
            let def = defs
                .lookup(CStr::from_ptr(name as *const i8).to_string_lossy().as_ref())
                .copied()
                .unwrap_or(null_mut());
            if !def.is_null() {
                cur = refe;
                while !cur.is_null() {
                    (*cur).content = def;
                    cur = (*cur).next_hash;
                }
            } else {
                let name = CStr::from_ptr(name as *const i8).to_string_lossy();
                xml_rng_perr!(
                    ctxt,
                    (*refe).node.map(|node| node.into()),
                    XmlParserErrors::XmlRngpRefNoDef,
                    "Reference {} has no matching definition\n",
                    name
                );
            }
        } else {
            let name = CStr::from_ptr(name as *const i8).to_string_lossy();
            xml_rng_perr!(
                ctxt,
                (*refe).node.map(|node| node.into()),
                XmlParserErrors::XmlRngpRefNoDef,
                "Reference {} has no matching definition\n",
                name
            );
        }
    }
}

/// Allocate a new RelaxNG grammar.
///
/// Returns the newly allocated structure or NULL in case or error
#[doc(alias = "xmlRelaxNGNewGrammar")]
unsafe fn xml_relaxng_new_grammar(ctxt: XmlRelaxNGParserCtxtPtr) -> XmlRelaxNGGrammarPtr {
    unsafe {
        let ret: XmlRelaxNGGrammarPtr = xml_malloc(size_of::<XmlRelaxNGGrammar>()) as _;
        if ret.is_null() {
            xml_rng_perr_memory(ctxt, None);
            return null_mut();
        }
        memset(ret as _, 0, size_of::<XmlRelaxNGGrammar>());

        ret
    }
}

/// Parse the content of a RelaxNG nameClass node.
///
/// Returns the definition pointer or NULL in case of error.
#[doc(alias = "xmlRelaxNGParseExceptNameClass")]
unsafe fn xml_relaxng_parse_except_name_class(
    ctxt: XmlRelaxNGParserCtxtPtr,
    node: XmlNodePtr,
    attr: i32,
) -> XmlRelaxNGDefinePtr {
    unsafe {
        let mut cur: XmlRelaxNGDefinePtr;
        let mut last: XmlRelaxNGDefinePtr = null_mut();

        if !is_relaxng(node, "except") {
            xml_rng_perr!(
                ctxt,
                Some(node.into()),
                XmlParserErrors::XmlRngpExceptMissing,
                "Expecting an except node\n"
            );
            return null_mut();
        }
        if node.next.is_some() {
            xml_rng_perr!(
                ctxt,
                Some(node.into()),
                XmlParserErrors::XmlRngpExceptMultiple,
                "exceptNameClass allows only a single except node\n"
            );
        }
        let Some(children) = node.children() else {
            xml_rng_perr!(
                ctxt,
                Some(node.into()),
                XmlParserErrors::XmlRngpExceptEmpty,
                "except has no content\n"
            );
            return null_mut();
        };

        let ret: XmlRelaxNGDefinePtr = xml_relaxng_new_define(ctxt, Some(node));
        if ret.is_null() {
            return null_mut();
        }
        (*ret).typ = XmlRelaxNGType::Except;
        let mut child = Some(children);
        while let Some(cur_node) = child {
            cur = xml_relaxng_new_define(ctxt, Some(XmlNodePtr::try_from(cur_node).unwrap()));
            if cur.is_null() {
                break;
            }
            if attr != 0 {
                (*cur).typ = XmlRelaxNGType::Attribute;
            } else {
                (*cur).typ = XmlRelaxNGType::Element;
            }

            if !xml_relaxng_parse_name_class(ctxt, XmlNodePtr::try_from(cur_node).unwrap(), cur)
                .is_null()
            {
                if last.is_null() {
                    (*ret).content = cur;
                } else {
                    (*last).next = cur;
                }
                last = cur;
            }
            child = cur_node.next();
        }

        ret
    }
}

/// Parse the content of a RelaxNG nameClass node.
///
/// Returns the definition pointer or NULL in case of error.
#[doc(alias = "xmlRelaxNGParseNameClass")]
unsafe fn xml_relaxng_parse_name_class(
    ctxt: XmlRelaxNGParserCtxtPtr,
    node: XmlNodePtr,
    def: XmlRelaxNGDefinePtr,
) -> XmlRelaxNGDefinePtr {
    unsafe {
        let mut ret: XmlRelaxNGDefinePtr;
        let mut tmp: XmlRelaxNGDefinePtr;

        ret = def;
        if (is_relaxng(node, "name") || is_relaxng(node, "anyName") || is_relaxng(node, "nsName"))
            && !matches!(
                (*def).typ,
                XmlRelaxNGType::Element | XmlRelaxNGType::Attribute
            )
        {
            ret = xml_relaxng_new_define(ctxt, Some(node));
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
        if is_relaxng(node, "name") {
            if let Some(val) = (*node).get_content() {
                let val = normalize_external_space(&val);
                let cval = xml_strndup(val.as_ptr(), val.len() as i32);
                if validate_ncname::<false>(val).is_err() {
                    if let Some(parent) = (*node).parent() {
                        xml_rng_perr!(
                            ctxt,
                            Some(node.into()),
                            XmlParserErrors::XmlRngpElementName,
                            "Element {} name '{}' is not an NCName\n",
                            parent.name().unwrap().into_owned(),
                            val
                        );
                    } else {
                        xml_rng_perr!(
                            ctxt,
                            Some(node.into()),
                            XmlParserErrors::XmlRngpElementName,
                            "name '{}' is not an NCName\n",
                            val
                        );
                    }
                }
                (*ret).name = cval;
            }
            let val = (*node).get_prop("ns").map(|v| CString::new(v).unwrap());
            let val = val
                .as_ref()
                .map_or(null_mut(), |v| xml_strdup(v.as_ptr() as *const u8));
            (*ret).ns = val;
            if (*ctxt).flags & XML_RELAXNG_IN_ATTRIBUTE != 0
                && !val.is_null()
                && xml_str_equal(val, c"http://www.w3.org/2000/xmlns".as_ptr() as _)
            {
                let val = CStr::from_ptr(val as *const i8).to_string_lossy();
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpXmlNs,
                    "Attribute with namespace '{}' is not allowed\n",
                    val
                );
            }
            if (*ctxt).flags & XML_RELAXNG_IN_ATTRIBUTE != 0
                && !val.is_null()
                && *val.add(0) == 0
                && xml_str_equal((*ret).name, c"xmlns".as_ptr() as _)
            {
                let val = CStr::from_ptr(val as *const i8).to_string_lossy();
                // `val` should be received, but format specifier of original code is not sufficient.
                // Since `val` is an empty string,
                // there is no problem to insert a format specifier where appropriate.
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpXmlNsName,
                    "Attribute with QName 'xmlns{}' is not allowed\n",
                    val
                );
            }
        } else if is_relaxng(node, "anyName") {
            (*ret).name = null_mut();
            (*ret).ns = null_mut();
            if let Some(children) = (*node).children() {
                (*ret).name_class = xml_relaxng_parse_except_name_class(
                    ctxt,
                    XmlNodePtr::try_from(children).unwrap(),
                    ((*def).typ == XmlRelaxNGType::Attribute) as i32,
                );
            }
        } else if is_relaxng(node, "nsName") {
            (*ret).name = null_mut();
            let tmp = (*node).get_prop("ns").map(|n| CString::new(n).unwrap());
            (*ret).ns = tmp
                .as_ref()
                .map_or(null_mut(), |n| xml_strdup(n.as_ptr() as *const u8));
            if (*ret).ns.is_null() {
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpNsNameNoNs,
                    "nsName has no ns attribute\n"
                );
            }
            if (*ctxt).flags & XML_RELAXNG_IN_ATTRIBUTE != 0
                && !(*ret).ns.is_null()
                && xml_str_equal((*ret).ns, c"http://www.w3.org/2000/xmlns".as_ptr() as _)
            {
                let ns = CStr::from_ptr((*ret).ns as *const i8).to_string_lossy();
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpXmlNs,
                    "Attribute with namespace '{}' is not allowed\n",
                    ns
                );
            }
            if let Some(children) = (*node).children() {
                (*ret).name_class = xml_relaxng_parse_except_name_class(
                    ctxt,
                    XmlNodePtr::try_from(children).unwrap(),
                    ((*def).typ == XmlRelaxNGType::Attribute) as i32,
                );
            }
        } else if is_relaxng(node, "choice") {
            let mut last: XmlRelaxNGDefinePtr = null_mut();

            if (*def).typ == XmlRelaxNGType::Choice {
                ret = def;
            } else {
                ret = xml_relaxng_new_define(ctxt, Some(node));
                if ret.is_null() {
                    return null_mut();
                }
                (*ret).parent = def;
                (*ret).typ = XmlRelaxNGType::Choice;
            }

            if node.children().is_some() {
                let mut child = node.children();
                while let Some(now) = child {
                    tmp =
                        xml_relaxng_parse_name_class(ctxt, XmlNodePtr::try_from(now).unwrap(), ret);
                    if !tmp.is_null() {
                        if last.is_null() {
                            last = tmp;
                        } else {
                            (*last).next = tmp;
                            last = tmp;
                        }
                    }
                    child = now.next();
                }
            } else {
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpChoiceEmpty,
                    "Element choice is empty\n"
                );
            }
        } else {
            xml_rng_perr!(
                ctxt,
                Some(node.into()),
                XmlParserErrors::XmlRngpChoiceContent,
                "expecting name, anyName, nsName or choice : got {}\n",
                node.name().unwrap().into_owned()
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
}

/// Applies algorithm from 4.3. datatypeLibrary attribute
///
/// Returns the datatypeLibrary value or NULL if not found
#[doc(alias = "xmlRelaxNGGetDataTypeLibrary")]
unsafe fn xml_relaxng_get_data_type_library(
    _ctxt: XmlRelaxNGParserCtxtPtr,
    node: XmlNodePtr,
) -> *mut XmlChar {
    unsafe {
        // if node.is_null() {
        //     return null_mut();
        // }

        if is_relaxng(node, "data") || is_relaxng(node, "value") {
            if let Some(ret) = node.get_prop("datatypeLibrary") {
                if ret.is_empty() {
                    return null_mut();
                }
                let escape = escape_url_except(&ret, b":/#?");
                let escape = CString::new(escape.as_ref()).unwrap();
                return xml_strdup(escape.as_ptr() as *const u8);
            }
        }
        let mut node = node.parent.and_then(|p| XmlNodePtr::try_from(p).ok());
        while let Some(now) =
            node.filter(|node| node.element_type() == XmlElementType::XmlElementNode)
        {
            if let Some(ret) = now.get_prop("datatypeLibrary") {
                if ret.is_empty() {
                    return null_mut();
                }
                let escape = escape_url_except(&ret, b":/#?");
                let escape = CString::new(escape.as_ref()).unwrap();
                return xml_strdup(escape.as_ptr() as *const u8);
            }
            node = now.parent.and_then(|p| XmlNodePtr::try_from(p).ok());
        }
        null_mut()
    }
}

/// Parse the content of a RelaxNG data node.
///
/// Returns the definition pointer or NULL in case of error
#[doc(alias = "xmlRelaxNGParseData")]
unsafe fn xml_relaxng_parse_data(
    ctxt: XmlRelaxNGParserCtxtPtr,
    node: XmlNodePtr,
) -> XmlRelaxNGDefinePtr {
    unsafe {
        let except: XmlRelaxNGDefinePtr;
        let mut param: XmlRelaxNGDefinePtr;
        let mut lastparam: XmlRelaxNGDefinePtr = null_mut();
        let mut library: *mut XmlChar;
        let tmp: i32;

        let Some(typ) = node.get_prop("type") else {
            xml_rng_perr!(
                ctxt,
                Some(node.into()),
                XmlParserErrors::XmlRngpTypeMissing,
                "data has no type\n"
            );
            return null_mut();
        };
        let typ = normalize_external_space(&typ);
        let ctyp = xml_strndup(typ.as_ptr(), typ.len() as i32);
        if validate_ncname::<false>(typ).is_err() {
            xml_rng_perr!(
                ctxt,
                Some(node.into()),
                XmlParserErrors::XmlRngpTypeValue,
                "data type '{}' is not an NCName\n",
                typ
            );
        }
        library = xml_relaxng_get_data_type_library(ctxt, node);
        if library.is_null() {
            library = xml_strdup(c"http://relaxng.org/ns/structure/1.0".as_ptr() as _);
        }

        let def: XmlRelaxNGDefinePtr = xml_relaxng_new_define(ctxt, Some(node));
        if def.is_null() {
            xml_free(library as _);
            xml_free(ctyp as _);
            return null_mut();
        }
        (*def).typ = XmlRelaxNGType::Datatype;
        (*def).name = ctyp;
        (*def).ns = library;

        let lib = XML_RELAXNG_REGISTERED_TYPES
            .get()
            .and_then(|table| {
                table
                    .lookup(
                        CStr::from_ptr(library as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                    )
                    .copied()
            })
            .unwrap_or(null_mut());
        if lib.is_null() {
            let library = CStr::from_ptr(library as *const i8).to_string_lossy();
            xml_rng_perr!(
                ctxt,
                Some(node.into()),
                XmlParserErrors::XmlRngpUnknownTypeLib,
                "Use of unregistered type library '{}'\n",
                library
            );
            (*def).data = null_mut();
        } else {
            (*def).data = lib as _;
            if let Some(have) = (*lib).have {
                tmp = have(
                    (*lib).data,
                    CStr::from_ptr((*def).name as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                ) as i32;
                if tmp != 1 {
                    let name = CStr::from_ptr((*def).name as *const i8).to_string_lossy();
                    let library = CStr::from_ptr(library as *const i8).to_string_lossy();
                    xml_rng_perr!(
                        ctxt,
                        Some(node.into()),
                        XmlParserErrors::XmlRngpTypeNotFound,
                        "Error type '{}' is not exported by type library '{}'\n",
                        name,
                        library
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
                let library = CStr::from_ptr(library as *const i8).to_string_lossy();
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpErrorTypeLib,
                    "Internal error with type library '{}': no 'have'\n",
                    library
                );
            }
        }

        let mut content = node.children();
        // Handle optional params
        while let Some(cur_node) = content {
            if cur_node.name().as_deref() != Some("param") {
                break;
            }
            if xml_str_equal(
                library,
                c"http://relaxng.org/ns/structure/1.0".as_ptr() as _,
            ) {
                let library = CStr::from_ptr(library as *const i8).to_string_lossy();
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpParamForbidden,
                    "Type library '{}' does not allow type parameters\n",
                    library
                );
                content = cur_node.next();
                while let Some(cur_node) =
                    content.filter(|cur_node| cur_node.name().as_deref() == Some("param"))
                {
                    content = cur_node.next();
                }
            } else {
                param = xml_relaxng_new_define(ctxt, Some(node));
                if !param.is_null() {
                    (*param).typ = XmlRelaxNGType::Param;
                    let tmp = cur_node.get_prop("name").map(|n| CString::new(n).unwrap());
                    (*param).name = tmp
                        .as_ref()
                        .map_or(null_mut(), |t| xml_strdup(t.as_ptr() as *const u8));
                    if (*param).name.is_null() {
                        xml_rng_perr!(
                            ctxt,
                            Some(node.into()),
                            XmlParserErrors::XmlRngpParamNameMissing,
                            "param has no name\n"
                        );
                    }
                    let tmp = cur_node.get_content().map(|c| CString::new(c).unwrap());
                    (*param).value = tmp
                        .as_ref()
                        .map_or(null_mut(), |c| xml_strdup(c.as_ptr() as *const u8));
                    if lastparam.is_null() {
                        (*def).attrs = param;
                        lastparam = param;
                    } else {
                        (*lastparam).next = param;
                        lastparam = param;
                    }
                    if !lib.is_null() {}
                }
                content = cur_node.next();
            }
        }
        // Handle optional except
        if let Some(cur_node) =
            content.filter(|cur_node| cur_node.name().as_deref() == Some("except"))
        {
            let mut tmp2: XmlRelaxNGDefinePtr;
            let mut last: XmlRelaxNGDefinePtr = null_mut();

            except = xml_relaxng_new_define(ctxt, Some(node));
            if except.is_null() {
                return def;
            }
            (*except).typ = XmlRelaxNGType::Except;
            let mut child = cur_node.children();
            (*def).content = except;
            if child.is_none() {
                xml_rng_perr!(
                    ctxt,
                    Some(cur_node),
                    XmlParserErrors::XmlRngpExceptNoContent,
                    "except has no content\n"
                );
            }
            while let Some(cur_node) = child {
                tmp2 = xml_relaxng_parse_pattern(ctxt, XmlNodePtr::try_from(cur_node).unwrap());
                if !tmp2.is_null() {
                    if last.is_null() {
                        (*except).content = tmp2;
                        last = tmp2;
                    } else {
                        (*last).next = tmp2;
                        last = tmp2;
                    }
                }
                child = cur_node.next();
            }
            content = cur_node.next();
        }
        // Check there is no unhandled data
        if let Some(content) = content {
            let name = content.name().unwrap().into_owned();
            xml_rng_perr!(
                ctxt,
                Some(content),
                XmlParserErrors::XmlRngpDataContent,
                "Element data has unexpected content {}\n",
                name
            );
        }

        def
    }
}

/// Parse the content of a RelaxNG attribute node.
///
/// Returns the definition pointer or NULL in case of error.
#[doc(alias = "xmlRelaxNGParseAttribute")]
unsafe fn xml_relaxng_parse_attribute(
    ctxt: XmlRelaxNGParserCtxtPtr,
    node: XmlNodePtr,
) -> XmlRelaxNGDefinePtr {
    unsafe {
        let mut cur: XmlRelaxNGDefinePtr;

        let ret: XmlRelaxNGDefinePtr = xml_relaxng_new_define(ctxt, Some(node));
        if ret.is_null() {
            return null_mut();
        }
        (*ret).typ = XmlRelaxNGType::Attribute;
        (*ret).parent = (*ctxt).def;
        let Some(child) = node.children.map(|c| XmlNodePtr::try_from(c).unwrap()) else {
            xml_rng_perr!(
                ctxt,
                Some(node.into()),
                XmlParserErrors::XmlRngpAttributeEmpty,
                "xmlRelaxNGParseattribute: attribute has no children\n"
            );
            return ret;
        };
        let old_flags: i32 = (*ctxt).flags;
        (*ctxt).flags |= XML_RELAXNG_IN_ATTRIBUTE;
        cur = xml_relaxng_parse_name_class(ctxt, child, ret);
        let mut child = if !cur.is_null() {
            child.next.map(|node| XmlNodePtr::try_from(node).unwrap())
        } else {
            Some(child)
        };

        if let Some(cur_node) = child {
            cur = xml_relaxng_parse_pattern(ctxt, cur_node);
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
                        xml_rng_perr!(
                            ctxt,
                            Some(node.into()),
                            XmlParserErrors::XmlRngpAttributeContent,
                            "attribute has invalid content\n"
                        );
                    }
                    XmlRelaxNGType::Noop => {
                        xml_rng_perr!(
                            ctxt,
                            Some(node.into()),
                            XmlParserErrors::XmlRngpAttributeNoop,
                            "RNG Internal error, noop found in attribute\n"
                        );
                    }
                }
            }
            child = cur_node
                .next
                .map(|node| XmlNodePtr::try_from(node).unwrap());
        }
        if child.is_some() {
            xml_rng_perr!(
                ctxt,
                Some(node.into()),
                XmlParserErrors::XmlRngpAttributeChildren,
                "attribute has multiple children\n"
            );
        }
        (*ctxt).flags = old_flags;
        ret
    }
}

/// Parse the content of a RelaxNG value node.
///
/// Returns the definition pointer or NULL in case of error
#[doc(alias = "xmlRelaxNGParseValue")]
unsafe fn xml_relaxng_parse_value(
    ctxt: XmlRelaxNGParserCtxtPtr,
    node: XmlNodePtr,
) -> XmlRelaxNGDefinePtr {
    unsafe {
        let mut lib: XmlRelaxNGTypeLibraryPtr = null_mut();
        let mut library: *mut XmlChar;
        let mut success: i32 = 0;

        let def: XmlRelaxNGDefinePtr = xml_relaxng_new_define(ctxt, Some(node));
        if def.is_null() {
            return null_mut();
        }
        (*def).typ = XmlRelaxNGType::Value;

        if let Some(typ) = (*node).get_prop("type") {
            let typ = normalize_external_space(&typ);
            let ctyp = xml_strndup(typ.as_ptr(), typ.len() as i32);
            if validate_ncname::<false>(typ).is_err() {
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpTypeValue,
                    "value typ '{}' is not an NCName\n",
                    typ
                );
            }
            library = xml_relaxng_get_data_type_library(ctxt, node);
            if library.is_null() {
                library = xml_strdup(c"http://relaxng.org/ns/structure/1.0".as_ptr() as _);
            }

            (*def).name = ctyp;
            (*def).ns = library;

            lib = XML_RELAXNG_REGISTERED_TYPES
                .get()
                .and_then(|table| {
                    table
                        .lookup(
                            CStr::from_ptr(library as *const i8)
                                .to_string_lossy()
                                .as_ref(),
                        )
                        .copied()
                })
                .unwrap_or(null_mut());
            if lib.is_null() {
                let library = CStr::from_ptr(library as *const i8).to_string_lossy();
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpUnknownTypeLib,
                    "Use of unregistered type library '{}'\n",
                    library
                );
                (*def).data = null_mut();
            } else {
                (*def).data = lib as _;
                if let Some(have) = (*lib).have {
                    success = have(
                        (*lib).data,
                        CStr::from_ptr((*def).name as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                    ) as i32;
                    if success != 1 {
                        let name = CStr::from_ptr((*def).name as *const i8).to_string_lossy();
                        let library = CStr::from_ptr(library as *const i8).to_string_lossy();
                        xml_rng_perr!(
                            ctxt,
                            Some(node.into()),
                            XmlParserErrors::XmlRngpTypeNotFound,
                            "Error type '{}' is not exported by type library '{}'\n",
                            name,
                            library
                        );
                    }
                } else {
                    let library = CStr::from_ptr(library as *const i8).to_string_lossy();
                    xml_rng_perr!(
                        ctxt,
                        Some(node.into()),
                        XmlParserErrors::XmlRngpErrorTypeLib,
                        "Internal error with type library '{}': no 'have'\n",
                        library
                    );
                }
            }
        }
        if node
            .children()
            .filter(|children| {
                !matches!(
                    children.element_type(),
                    XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
                ) || children.next().is_some()
            })
            .is_some()
        {
            xml_rng_perr!(
                ctxt,
                Some(node.into()),
                XmlParserErrors::XmlRngpTextExpected,
                "Expecting a single text value for <value>content\n"
            );
        } else if node.children.is_none() {
            (*def).value = xml_strdup(c"".as_ptr() as _);
        } else if !def.is_null() {
            let tmp = node.get_content().map(|c| CString::new(c).unwrap());
            (*def).value = tmp
                .as_ref()
                .map_or(null_mut(), |t| xml_strdup(t.as_ptr() as *const u8));
            if (*def).value.is_null() {
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpValueNoContent,
                    "Element <value> has no content\n"
                );
            } else if !lib.is_null() && (*lib).check.is_some() && success == 1 {
                let mut val: *mut c_void = null_mut();

                success = (*lib).check.unwrap()(
                    (*lib).data,
                    CStr::from_ptr((*def).name as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    (*def).value,
                    addr_of_mut!(val),
                    Some(node.into()),
                );
                if success != 1 {
                    let value = CStr::from_ptr((*def).value as *const i8).to_string_lossy();
                    let name = CStr::from_ptr((*def).name as *const i8).to_string_lossy();
                    xml_rng_perr!(
                        ctxt,
                        Some(node.into()),
                        XmlParserErrors::XmlRngpInvalidValue,
                        "Value '{}' is not acceptable for type '{}'\n",
                        value,
                        name
                    );
                } else if !val.is_null() {
                    (*def).attrs = val as _;
                }
            }
        }
        def
    }
}

/// Parse the content of a RelaxNG interleave node.
///
/// Returns the definition pointer or NULL in case of error
#[doc(alias = "xmlRelaxNGParseInterleave")]
unsafe fn xml_relaxng_parse_interleave(
    ctxt: XmlRelaxNGParserCtxtPtr,
    node: XmlNodePtr,
) -> XmlRelaxNGDefinePtr {
    unsafe {
        let mut last: XmlRelaxNGDefinePtr = null_mut();
        let mut cur: XmlRelaxNGDefinePtr;

        let def: XmlRelaxNGDefinePtr = xml_relaxng_new_define(ctxt, Some(node));
        if def.is_null() {
            return null_mut();
        }
        (*def).typ = XmlRelaxNGType::Interleave;

        if (*ctxt).interleaves.is_none() {
            (*ctxt).interleaves = XmlHashTableRef::with_capacity(10);
        }
        if let Some(mut table) = (*ctxt).interleaves {
            let mut name: [c_char; 32] = [0; 32];

            snprintf(
                name.as_mut_ptr() as _,
                32,
                c"interleave%d".as_ptr() as _,
                (*ctxt).nb_interleaves,
            );
            (*ctxt).nb_interleaves += 1;
            if table
                .add_entry(
                    CStr::from_ptr(name.as_ptr()).to_string_lossy().as_ref(),
                    def,
                )
                .is_err()
            {
                let name = CStr::from_ptr(name.as_ptr()).to_string_lossy();
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpInterleaveAdd,
                    "Failed to add {} to hash table\n",
                    name
                );
            }
        } else {
            xml_rng_perr_memory(ctxt, Some("create interleaves\n"));
        }
        let mut child = node.children;
        if child.is_none() {
            xml_rng_perr!(
                ctxt,
                Some(node.into()),
                XmlParserErrors::XmlRngpInterleaveNoContent,
                "Element interleave is empty\n"
            );
        }
        while let Some(cur_node) = child {
            if is_relaxng(XmlNodePtr::try_from(cur_node).unwrap(), "element") {
                cur = xml_relaxng_parse_element(ctxt, XmlNodePtr::try_from(cur_node).unwrap());
            } else {
                cur = xml_relaxng_parse_pattern(ctxt, XmlNodePtr::try_from(cur_node).unwrap());
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
            child = cur_node.next();
        }

        def
    }
}

/// Import import one references into the current grammar
#[doc(alias = "xmlRelaxNGParseImportRef")]
fn xml_relaxng_parse_import_ref(
    def: XmlRelaxNGDefinePtr,
    ctxt: XmlRelaxNGParserCtxtPtr,
    name: *const XmlChar,
) {
    unsafe {
        (*def).dflags |= IS_EXTERNAL_REF as i16;

        if (*(*ctxt).grammar)
            .refs
            .and_then(|mut table| {
                table
                    .add_entry(
                        CStr::from_ptr(name as *const i8).to_string_lossy().as_ref(),
                        def,
                    )
                    .ok()
            })
            .is_none()
        {
            let prev = (*(*ctxt).grammar)
                .refs
                .and_then(|table| {
                    table
                        .lookup(
                            CStr::from_ptr((*def).name as *const i8)
                                .to_string_lossy()
                                .as_ref(),
                        )
                        .copied()
                })
                .unwrap_or(null_mut());
            if prev.is_null() {
                if !(*def).name.is_null() {
                    let name = CStr::from_ptr((*def).name as *const i8).to_string_lossy();
                    xml_rng_perr!(
                        ctxt,
                        None,
                        XmlParserErrors::XmlRngpRefCreateFailed,
                        "Error refs definitions '{}'\n",
                        name
                    );
                } else {
                    xml_rng_perr!(
                        ctxt,
                        None,
                        XmlParserErrors::XmlRngpRefCreateFailed,
                        "Error refs definitions\n"
                    );
                }
            } else {
                (*def).next_hash = (*prev).next_hash;
                (*prev).next_hash = def;
            }
        }
    }
}

/// Import references from the subgrammar into the current grammar
///
/// Returns 0 in case of success, -1 in case of failure
#[doc(alias = "xmlRelaxNGParseImportRefs")]
unsafe fn xml_relaxng_parse_import_refs(
    ctxt: XmlRelaxNGParserCtxtPtr,
    grammar: XmlRelaxNGGrammarPtr,
) -> i32 {
    unsafe {
        if ctxt.is_null() || grammar.is_null() || (*ctxt).grammar.is_null() {
            return -1;
        }
        if (*grammar).refs.is_none() {
            return 0;
        }
        if (*(*ctxt).grammar).refs.is_none() {
            (*(*ctxt).grammar).refs = XmlHashTableRef::with_capacity(10);
        }

        let Some(refs) = (*(*ctxt).grammar).refs else {
            xml_rng_perr!(
                ctxt,
                None,
                XmlParserErrors::XmlRngpRefCreateFailed,
                "Could not create references hash\n"
            );
            return -1;
        };
        refs.scan(|data, name, _, _| {
            let name = name.map(|n| CString::new(n.as_ref()).unwrap());
            xml_relaxng_parse_import_ref(
                *data,
                ctxt,
                name.as_deref()
                    .map_or(null_mut(), |p| p.as_ptr() as *const u8),
            );
        });
        0
    }
}

/// Process and compile an externalRef node
///
/// Returns the xmlRelaxNGDefinePtr or NULL in case of error
#[doc(alias = "xmlRelaxNGProcessExternalRef")]
unsafe fn xml_relaxng_process_external_ref(
    ctxt: XmlRelaxNGParserCtxtPtr,
    node: XmlNodePtr,
) -> XmlRelaxNGDefinePtr {
    unsafe {
        let mut new_ns: i32 = 0;
        let oldflags: i32;
        let def: XmlRelaxNGDefinePtr;

        let docu: XmlRelaxNGDocumentPtr = node.psvi as _;
        if !docu.is_null() {
            def = xml_relaxng_new_define(ctxt, Some(node));
            if def.is_null() {
                return null_mut();
            }
            (*def).typ = XmlRelaxNGType::Externalref;

            if (*docu).content.is_null() {
                // Then do the parsing for good
                let Some(mut root) = (*docu).doc.and_then(|doc| doc.get_root_element()) else {
                    let url = (*ctxt).url.as_deref().unwrap();
                    xml_rng_perr!(
                        ctxt,
                        Some(node.into()),
                        XmlParserErrors::XmlRngpExternalRefEmtpy,
                        "xmlRelaxNGParse: {} is empty\n",
                        url
                    );
                    return null_mut();
                };
                // ns transmission rules
                let mut ns = root.get_prop("ns");
                if ns.is_none() {
                    let mut tmp = Some(node);
                    while let Some(now) =
                        tmp.filter(|tmp| tmp.element_type() == XmlElementType::XmlElementNode)
                    {
                        ns = now.get_prop("ns");
                        if ns.is_some() {
                            break;
                        }
                        tmp = now.parent.and_then(|p| XmlNodePtr::try_from(p).ok());
                    }
                    if let Some(ns) = ns {
                        root.set_prop("ns", Some(ns.as_str()));
                        new_ns = 1;
                    }
                }

                // Parsing to get a precompiled schemas.
                oldflags = (*ctxt).flags;
                (*ctxt).flags |= XML_RELAXNG_IN_EXTERNALREF;
                (*docu).schema = xml_relaxng_parse_document(ctxt, root);
                (*ctxt).flags = oldflags;
                if !(*docu).schema.is_null() && !(*(*docu).schema).topgrammar.is_null() {
                    (*docu).content = (*(*(*docu).schema).topgrammar).start;
                    if (*(*(*docu).schema).topgrammar).refs.is_some() {
                        xml_relaxng_parse_import_refs(ctxt, (*(*docu).schema).topgrammar);
                    }
                }

                // the externalRef may be reused in a different ns context
                if new_ns == 1 {
                    root.unset_prop("ns");
                }
            }
            (*def).content = (*docu).content;
        } else {
            def = null_mut();
        }
        def
    }
}

/// Parse the content of a RelaxNG pattern node.
///
/// Returns the definition pointer or NULL in case of error or if no pattern is generated.
#[doc(alias = "xmlRelaxNGParsePattern")]
unsafe fn xml_relaxng_parse_pattern(
    ctxt: XmlRelaxNGParserCtxtPtr,
    node: XmlNodePtr,
) -> XmlRelaxNGDefinePtr {
    unsafe {
        let mut def: XmlRelaxNGDefinePtr;

        if is_relaxng(node, "element") {
            def = xml_relaxng_parse_element(ctxt, node);
        } else if is_relaxng(node, "attribute") {
            def = xml_relaxng_parse_attribute(ctxt, node);
        } else if is_relaxng(node, "empty") {
            def = xml_relaxng_new_define(ctxt, Some(node));
            if def.is_null() {
                return null_mut();
            }
            (*def).typ = XmlRelaxNGType::Empty;
            if node.children.is_some() {
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpEmptyNotEmpty,
                    "empty: had a child node\n"
                );
            }
        } else if is_relaxng(node, "text") {
            def = xml_relaxng_new_define(ctxt, Some(node));
            if def.is_null() {
                return null_mut();
            }
            (*def).typ = XmlRelaxNGType::Text;
            if node.children.is_some() {
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpTextHasChild,
                    "text: had a child node\n"
                );
            }
        } else if is_relaxng(node, "zeroOrMore") {
            def = xml_relaxng_new_define(ctxt, Some(node));
            if def.is_null() {
                return null_mut();
            }
            (*def).typ = XmlRelaxNGType::Zeroormore;
            if let Some(children) = node
                .children
                .map(|children| XmlNodePtr::try_from(children).unwrap())
            {
                (*def).content = xml_relaxng_parse_patterns(ctxt, children, 1);
            } else {
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpEmptyConstruct,
                    "Element {} is empty\n",
                    node.name().unwrap().into_owned()
                );
            }
        } else if is_relaxng(node, "oneOrMore") {
            def = xml_relaxng_new_define(ctxt, Some(node));
            if def.is_null() {
                return null_mut();
            }
            (*def).typ = XmlRelaxNGType::Oneormore;
            if let Some(children) = node
                .children
                .map(|children| XmlNodePtr::try_from(children).unwrap())
            {
                (*def).content = xml_relaxng_parse_patterns(ctxt, children, 1);
            } else {
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpEmptyConstruct,
                    "Element {} is empty\n",
                    node.name().unwrap().into_owned()
                );
            }
        } else if is_relaxng(node, "optional") {
            def = xml_relaxng_new_define(ctxt, Some(node));
            if def.is_null() {
                return null_mut();
            }
            (*def).typ = XmlRelaxNGType::Optional;
            if let Some(children) = node
                .children
                .map(|children| XmlNodePtr::try_from(children).unwrap())
            {
                (*def).content = xml_relaxng_parse_patterns(ctxt, children, 1);
            } else {
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpEmptyConstruct,
                    "Element {} is empty\n",
                    node.name().unwrap().into_owned()
                );
            }
        } else if is_relaxng(node, "choice") {
            def = xml_relaxng_new_define(ctxt, Some(node));
            if def.is_null() {
                return null_mut();
            }
            (*def).typ = XmlRelaxNGType::Choice;
            if let Some(children) = node
                .children
                .map(|children| XmlNodePtr::try_from(children).unwrap())
            {
                (*def).content = xml_relaxng_parse_patterns(ctxt, children, 0);
            } else {
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpEmptyConstruct,
                    "Element {} is empty\n",
                    node.name().unwrap().into_owned()
                );
            }
        } else if is_relaxng(node, "group") {
            def = xml_relaxng_new_define(ctxt, Some(node));
            if def.is_null() {
                return null_mut();
            }
            (*def).typ = XmlRelaxNGType::Group;
            if let Some(children) = node
                .children
                .map(|children| XmlNodePtr::try_from(children).unwrap())
            {
                (*def).content = xml_relaxng_parse_patterns(ctxt, children, 0);
            } else {
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpEmptyConstruct,
                    "Element {} is empty\n",
                    node.name().unwrap().into_owned()
                );
            }
        } else if is_relaxng(node, "ref") {
            def = xml_relaxng_new_define(ctxt, Some(node));
            if def.is_null() {
                return null_mut();
            }
            (*def).typ = XmlRelaxNGType::Ref;
            let tmp = (*node)
                .get_prop("name")
                .map(|n| CString::new(normalize_external_space(&n)).unwrap());
            (*def).name = tmp
                .as_ref()
                .map_or(null_mut(), |t| xml_strdup(t.as_ptr() as *const u8));
            if (*def).name.is_null() {
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpRefNoName,
                    "ref has no name\n"
                );
            } else if validate_ncname::<false>(
                CStr::from_ptr((*def).name as *const i8)
                    .to_string_lossy()
                    .as_ref(),
            )
            .is_err()
            {
                let name = CStr::from_ptr((*def).name as *const i8).to_string_lossy();
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpRefNameInvalid,
                    "ref name '{}' is not an NCName\n",
                    name
                );
            }
            if node.children().is_some() {
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpRefNotEmpty,
                    "ref is not empty\n"
                );
            }
            if (*(*ctxt).grammar).refs.is_none() {
                (*(*ctxt).grammar).refs = XmlHashTableRef::with_capacity(10);
            }
            if let Some(mut refs) = (*(*ctxt).grammar).refs {
                if !(*def).name.is_null()
                    && refs
                        .add_entry(
                            CStr::from_ptr((*def).name as *const i8)
                                .to_string_lossy()
                                .as_ref(),
                            def,
                        )
                        .is_err()
                {
                    let prev = refs
                        .lookup(
                            CStr::from_ptr((*def).name as *const i8)
                                .to_string_lossy()
                                .as_ref(),
                        )
                        .copied()
                        .unwrap_or(null_mut());
                    if prev.is_null() {
                        let name = CStr::from_ptr((*def).name as *const i8).to_string_lossy();
                        if !(*def).name.is_null() {
                            xml_rng_perr!(
                                ctxt,
                                Some(node.into()),
                                XmlParserErrors::XmlRngpRefCreateFailed,
                                "Error refs definitions '{}'\n",
                                name
                            );
                        } else {
                            xml_rng_perr!(
                                ctxt,
                                Some(node.into()),
                                XmlParserErrors::XmlRngpRefCreateFailed,
                                "Error refs definitions\n"
                            );
                        }
                        def = null_mut();
                    } else {
                        (*def).next_hash = (*prev).next_hash;
                        (*prev).next_hash = def;
                    }
                }
            } else {
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpRefCreateFailed,
                    "Could not create references hash\n"
                );
                def = null_mut();
            }
        } else if is_relaxng(node, "data") {
            def = xml_relaxng_parse_data(ctxt, node);
        } else if is_relaxng(node, "value") {
            def = xml_relaxng_parse_value(ctxt, node);
        } else if is_relaxng(node, "list") {
            def = xml_relaxng_new_define(ctxt, Some(node));
            if def.is_null() {
                return null_mut();
            }
            (*def).typ = XmlRelaxNGType::List;
            if let Some(children) = node
                .children
                .map(|children| XmlNodePtr::try_from(children).unwrap())
            {
                (*def).content = xml_relaxng_parse_patterns(ctxt, children, 0);
            } else {
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpEmptyConstruct,
                    "Element {} is empty\n",
                    node.name().unwrap().into_owned()
                );
            }
        } else if is_relaxng(node, "interleave") {
            def = xml_relaxng_parse_interleave(ctxt, node);
        } else if is_relaxng(node, "externalRef") {
            def = xml_relaxng_process_external_ref(ctxt, node);
        } else if is_relaxng(node, "notAllowed") {
            def = xml_relaxng_new_define(ctxt, Some(node));
            if def.is_null() {
                return null_mut();
            }
            (*def).typ = XmlRelaxNGType::NotAllowed;
            if node.children().is_some() {
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpNotAllowedNotEmpty,
                    "xmlRelaxNGParse: notAllowed element is not empty\n"
                );
            }
        } else if is_relaxng(node, "grammar") {
            let oldparent: XmlRelaxNGGrammarPtr = (*ctxt).parentgrammar;
            let old: XmlRelaxNGGrammarPtr = (*ctxt).grammar;
            (*ctxt).parentgrammar = old;
            let grammar: XmlRelaxNGGrammarPtr = xml_relaxng_parse_grammar(
                ctxt,
                node.children.map(|c| XmlNodePtr::try_from(c).unwrap()),
            );
            if !old.is_null() {
                (*ctxt).grammar = old;
                (*ctxt).parentgrammar = oldparent;
            }
            if !grammar.is_null() {
                def = (*grammar).start;
            } else {
                def = null_mut();
            }
        } else if is_relaxng(node, "parentRef") {
            if (*ctxt).parentgrammar.is_null() {
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpParentRefNoParent,
                    "Use of parentRef without a parent grammar\n"
                );
                return null_mut();
            }
            def = xml_relaxng_new_define(ctxt, Some(node));
            if def.is_null() {
                return null_mut();
            }
            (*def).typ = XmlRelaxNGType::Parentref;
            let tmp = node
                .get_prop("name")
                .map(|n| CString::new(normalize_external_space(&n)).unwrap());
            (*def).name = tmp
                .as_ref()
                .map_or(null_mut(), |n| xml_strdup(n.as_ptr() as *const u8));
            if (*def).name.is_null() {
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpParentRefNoName,
                    "parentRef has no name\n"
                );
            } else if validate_ncname::<false>(
                CStr::from_ptr((*def).name as *const i8)
                    .to_string_lossy()
                    .as_ref(),
            )
            .is_err()
            {
                let name = CStr::from_ptr((*def).name as *const i8).to_string_lossy();
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpParentRefNameInvalid,
                    "parentRef name '{}' is not an NCName\n",
                    name
                );
            }
            if node.children.is_some() {
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpParentRefNotEmpty,
                    "parentRef is not empty\n"
                );
            }
            if (*(*ctxt).parentgrammar).refs.is_none() {
                (*(*ctxt).parentgrammar).refs = XmlHashTableRef::with_capacity(10);
            }
            if let Some(mut refs) = (*(*ctxt).parentgrammar).refs {
                if !(*def).name.is_null()
                    && refs
                        .add_entry(
                            CStr::from_ptr((*def).name as *const i8)
                                .to_string_lossy()
                                .as_ref(),
                            def,
                        )
                        .is_err()
                {
                    let prev = refs
                        .lookup(
                            CStr::from_ptr((*def).name as *const i8)
                                .to_string_lossy()
                                .as_ref(),
                        )
                        .copied()
                        .unwrap_or(null_mut());
                    if prev.is_null() {
                        let name = CStr::from_ptr((*def).name as *const i8).to_string_lossy();
                        xml_rng_perr!(
                            ctxt,
                            Some(node.into()),
                            XmlParserErrors::XmlRngpParentRefCreateFailed,
                            "Internal error parentRef definitions '{}'\n",
                            name
                        );
                        def = null_mut();
                    } else {
                        (*def).next_hash = (*prev).next_hash;
                        (*prev).next_hash = def;
                    }
                }
            } else {
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpParentRefCreateFailed,
                    "Could not create references hash\n"
                );
                def = null_mut();
            }
        } else if is_relaxng(node, "mixed") {
            if node.children.is_none() {
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpEmptyConstruct,
                    "Mixed is empty\n"
                );
                def = null_mut();
            } else {
                def = xml_relaxng_parse_interleave(ctxt, node);
                if !def.is_null() {
                    let mut tmp: XmlRelaxNGDefinePtr;

                    if !(*def).content.is_null() && !(*(*def).content).next.is_null() {
                        tmp = xml_relaxng_new_define(ctxt, Some(node));
                        if !tmp.is_null() {
                            (*tmp).typ = XmlRelaxNGType::Group;
                            (*tmp).content = (*def).content;
                            (*def).content = tmp;
                        }
                    }

                    tmp = xml_relaxng_new_define(ctxt, Some(node));
                    if tmp.is_null() {
                        return def;
                    }
                    (*tmp).typ = XmlRelaxNGType::Text;
                    (*tmp).next = (*def).content;
                    (*def).content = tmp;
                }
            }
        } else {
            xml_rng_perr!(
                ctxt,
                Some(node.into()),
                XmlParserErrors::XmlRngpUnknownConstruct,
                "Unexpected node {} is not a pattern\n",
                node.name().unwrap().into_owned()
            );
            def = null_mut();
        }
        def
    }
}

/// Parse the content of a RelaxNG element node.
///
/// Returns the definition pointer or NULL in case of error.
#[doc(alias = "xmlRelaxNGParseElement")]
unsafe fn xml_relaxng_parse_element(
    ctxt: XmlRelaxNGParserCtxtPtr,
    node: XmlNodePtr,
) -> XmlRelaxNGDefinePtr {
    unsafe {
        let mut cur: XmlRelaxNGDefinePtr;
        let mut last: XmlRelaxNGDefinePtr;

        let ret: XmlRelaxNGDefinePtr = xml_relaxng_new_define(ctxt, Some(node));
        if ret.is_null() {
            return null_mut();
        }
        (*ret).typ = XmlRelaxNGType::Element;
        (*ret).parent = (*ctxt).def;
        let Some(mut child) = node.children().map(|c| XmlNodePtr::try_from(c).unwrap()) else {
            xml_rng_perr!(
                ctxt,
                Some(node.into()),
                XmlParserErrors::XmlRngpElementEmpty,
                "xmlRelaxNGParseElement: element has no children\n"
            );
            return ret;
        };
        cur = xml_relaxng_parse_name_class(ctxt, child, ret);
        if !cur.is_null() {
            let Some(next) = child.next.map(|node| XmlNodePtr::try_from(node).unwrap()) else {
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpElementNoContent,
                    "xmlRelaxNGParseElement: element has no content\n"
                );
                return ret;
            };
            child = next;
        }

        let olddefine: *const XmlChar = (*ctxt).define;
        (*ctxt).define = null_mut();
        last = null_mut();
        let mut child = Some(child);
        while let Some(cur_node) = child {
            cur = xml_relaxng_parse_pattern(ctxt, cur_node);
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
                                (*ret).content = xml_relaxng_new_define(ctxt, Some(node));
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
                        xml_rng_perr!(
                            ctxt,
                            Some(node.into()),
                            XmlParserErrors::XmlRngpElementContent,
                            "RNG Internal error, start found in element\n"
                        );
                    }
                    XmlRelaxNGType::Param => {
                        xml_rng_perr!(
                            ctxt,
                            Some(node.into()),
                            XmlParserErrors::XmlRngpElementContent,
                            "RNG Internal error, param found in element\n"
                        );
                    }
                    XmlRelaxNGType::Except => {
                        xml_rng_perr!(
                            ctxt,
                            Some(node.into()),
                            XmlParserErrors::XmlRngpElementContent,
                            "RNG Internal error, except found in element\n"
                        );
                    }
                    XmlRelaxNGType::Noop => {
                        xml_rng_perr!(
                            ctxt,
                            Some(node.into()),
                            XmlParserErrors::XmlRngpElementContent,
                            "RNG Internal error, noop found in element\n"
                        );
                    }
                }
            }
            child = cur_node
                .next
                .map(|node| XmlNodePtr::try_from(node).unwrap());
        }
        (*ctxt).define = olddefine;
        ret
    }
}

/// Parse the content of a RelaxNG start node.
///
/// Returns the definition pointer or NULL in case of error.
#[doc(alias = "xmlRelaxNGParsePatterns")]
unsafe fn xml_relaxng_parse_patterns(
    ctxt: XmlRelaxNGParserCtxtPtr,
    nodes: XmlNodePtr,
    group: i32,
) -> XmlRelaxNGDefinePtr {
    unsafe {
        let mut def: XmlRelaxNGDefinePtr = null_mut();
        let mut last: XmlRelaxNGDefinePtr = null_mut();
        let mut cur: XmlRelaxNGDefinePtr;

        let parent: XmlRelaxNGDefinePtr = (*ctxt).def;
        let mut nodes = Some(nodes);
        while let Some(node) = nodes {
            if is_relaxng(node, "element") {
                cur = xml_relaxng_parse_element(ctxt, node);
                if cur.is_null() {
                    return null_mut();
                }
                if def.is_null() {
                    def = cur;
                    last = cur;
                } else {
                    if group == 1 && (*def).typ == XmlRelaxNGType::Element && def == last {
                        def = xml_relaxng_new_define(ctxt, Some(node));
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
                cur = xml_relaxng_parse_pattern(ctxt, node);
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
            nodes = node.next.map(|node| XmlNodePtr::try_from(node).unwrap());
        }
        def
    }
}

/// Parse the content of a RelaxNG start node.
///
/// Returns 0 in case of success, -1 in case of error
#[doc(alias = "xmlRelaxNGParseStart")]
unsafe fn xml_relaxng_parse_start(ctxt: XmlRelaxNGParserCtxtPtr, nodes: XmlNodePtr) -> i32 {
    unsafe {
        let ret: i32 = 0;
        let def: XmlRelaxNGDefinePtr;
        let mut last: XmlRelaxNGDefinePtr;

        if is_relaxng(nodes, "empty") {
            def = xml_relaxng_new_define(ctxt, Some(nodes));
            if def.is_null() {
                return -1;
            }
            (*def).typ = XmlRelaxNGType::Empty;
            if nodes.children.is_some() {
                xml_rng_perr!(
                    ctxt,
                    Some(nodes.into()),
                    XmlParserErrors::XmlRngpEmptyContent,
                    "element empty is not empty\n"
                );
            }
        } else if is_relaxng(nodes, "notAllowed") {
            def = xml_relaxng_new_define(ctxt, Some(nodes));
            if def.is_null() {
                return -1;
            }
            (*def).typ = XmlRelaxNGType::NotAllowed;
            if nodes.children.is_some() {
                xml_rng_perr!(
                    ctxt,
                    Some(nodes.into()),
                    XmlParserErrors::XmlRngpNotAllowedNotEmpty,
                    "element notAllowed is not empty\n"
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
        let nodes = nodes.next;
        if nodes.is_some() {
            xml_rng_perr!(
                ctxt,
                nodes,
                XmlParserErrors::XmlRngpStartContent,
                "start more than one children\n"
            );
            return -1;
        }
        ret
    }
}

/// Parse the content of a RelaxNG define element node.
///
/// Returns 0 in case of success or -1 in case of error
#[doc(alias = "xmlRelaxNGParseDefine")]
unsafe fn xml_relaxng_parse_define(ctxt: XmlRelaxNGParserCtxtPtr, node: XmlNodePtr) -> i32 {
    unsafe {
        let mut ret: i32 = 0;
        let def: XmlRelaxNGDefinePtr;
        let olddefine: *const XmlChar;

        if let Some(name) = node.get_prop("name") {
            let name = normalize_external_space(&name);
            let cname = xml_strndup(name.as_ptr(), name.len() as i32);
            if validate_ncname::<false>(name).is_err() {
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpInvalidDefineName,
                    "define name '{}' is not an NCName\n",
                    name
                );
            }
            def = xml_relaxng_new_define(ctxt, Some(node));
            if def.is_null() {
                xml_free(cname as _);
                return -1;
            }
            (*def).typ = XmlRelaxNGType::Def;
            (*def).name = cname;
            if let Some(children) = node
                .children
                .map(|children| XmlNodePtr::try_from(children).unwrap())
            {
                olddefine = (*ctxt).define;
                (*ctxt).define = cname;
                (*def).content = xml_relaxng_parse_patterns(ctxt, children, 0);
                (*ctxt).define = olddefine;
            } else {
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpDefineEmpty,
                    "define has no children\n"
                );
            }
            if (*(*ctxt).grammar).defs.is_none() {
                (*(*ctxt).grammar).defs = XmlHashTableRef::with_capacity(10);
            }
            if let Some(mut defs) = (*(*ctxt).grammar).defs {
                if defs
                    .add_entry(&CStr::from_ptr(cname as *const i8).to_string_lossy(), def)
                    .is_err()
                {
                    let mut prev = defs
                        .lookup(&CStr::from_ptr(cname as *const i8).to_string_lossy())
                        .copied()
                        .unwrap_or(null_mut());
                    if prev.is_null() {
                        xml_rng_perr!(
                            ctxt,
                            Some(node.into()),
                            XmlParserErrors::XmlRngpDefineCreateFailed,
                            "Internal error on define aggregation of {}\n",
                            name
                        );
                        ret = -1;
                    } else {
                        while !(*prev).next_hash.is_null() {
                            prev = (*prev).next_hash;
                        }
                        (*prev).next_hash = def;
                    }
                }
            } else {
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpDefineCreateFailed,
                    "Could not create definition hash\n"
                );
                ret = -1;
            }
        } else {
            xml_rng_perr!(
                ctxt,
                Some(node.into()),
                XmlParserErrors::XmlRngpDefineNameMissing,
                "define has no name\n"
            );
        }
        ret
    }
}

/// Integrate the content of an include node in the current grammar
///
/// Returns 0 in case of success or -1 in case of error
#[doc(alias = "xmlRelaxNGParseInclude")]
unsafe fn xml_relaxng_parse_include(ctxt: XmlRelaxNGParserCtxtPtr, node: XmlNodePtr) -> i32 {
    unsafe {
        let mut ret: i32 = 0;
        let mut tmp: i32;

        let incl: XmlRelaxNGIncludePtr = node.psvi as _;
        if incl.is_null() {
            xml_rng_perr!(
                ctxt,
                Some(node.into()),
                XmlParserErrors::XmlRngpIncludeEmpty,
                "Include node has no data\n"
            );
            return -1;
        }
        let Some(root) = (*incl).doc.and_then(|doc| doc.get_root_element()) else {
            xml_rng_perr!(
                ctxt,
                Some(node.into()),
                XmlParserErrors::XmlRngpEmpty,
                "Include document is empty\n"
            );
            return -1;
        };
        if root.name != "grammar" {
            xml_rng_perr!(
                ctxt,
                Some(node.into()),
                XmlParserErrors::XmlRngpGrammarMissing,
                "Include document root is not a grammar\n"
            );
            return -1;
        }

        // Merge the definition from both the include and the internal list
        if let Some(children) = root
            .children
            .map(|children| XmlNodePtr::try_from(children).unwrap())
        {
            tmp = xml_relaxng_parse_grammar_content(ctxt, Some(children));
            if tmp != 0 {
                ret = -1;
            }
        }
        if let Some(children) = node
            .children
            .map(|children| XmlNodePtr::try_from(children).unwrap())
        {
            tmp = xml_relaxng_parse_grammar_content(ctxt, Some(children));
            if tmp != 0 {
                ret = -1;
            }
        }
        ret
    }
}

/// Parse the content of a RelaxNG grammar node.
///
/// Returns 0 in case of success, -1 in case of error
#[doc(alias = "xmlRelaxNGParseGrammarContent")]
unsafe fn xml_relaxng_parse_grammar_content(
    ctxt: XmlRelaxNGParserCtxtPtr,
    mut nodes: Option<XmlNodePtr>,
) -> i32 {
    unsafe {
        let mut ret: i32 = 0;
        let mut tmp: i32;

        if nodes.is_none() {
            xml_rng_perr!(
                ctxt,
                None,
                XmlParserErrors::XmlRngpGrammarEmpty,
                "grammar has no children\n"
            );
            return -1;
        }
        while let Some(node) = nodes {
            if is_relaxng(node, "start") {
                if let Some(children) = node
                    .children
                    .map(|children| XmlNodePtr::try_from(children).unwrap())
                {
                    tmp = xml_relaxng_parse_start(ctxt, children);
                    if tmp != 0 {
                        ret = -1;
                    }
                } else {
                    xml_rng_perr!(
                        ctxt,
                        Some(node.into()),
                        XmlParserErrors::XmlRngpStartEmpty,
                        "start has no children\n"
                    );
                }
            } else if is_relaxng(node, "define") {
                tmp = xml_relaxng_parse_define(ctxt, node);
                if tmp != 0 {
                    ret = -1;
                }
            } else if is_relaxng(node, "include") {
                tmp = xml_relaxng_parse_include(ctxt, node);
                if tmp != 0 {
                    ret = -1;
                }
            } else {
                xml_rng_perr!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlRngpGrammarContent,
                    "grammar has unexpected child {}\n",
                    node.name().unwrap().into_owned()
                );
                ret = -1;
            }
            nodes = node.next.map(|node| XmlNodePtr::try_from(node).unwrap());
        }
        ret
    }
}

/// Applies the 4.17. combine rule for all the start element of a given grammar.
#[doc(alias = "xmlRelaxNGCombineStart")]
unsafe fn xml_relaxng_combine_start(ctxt: XmlRelaxNGParserCtxtPtr, grammar: XmlRelaxNGGrammarPtr) {
    unsafe {
        let mut choice_or_interleave: i32 = -1;
        let mut missing: i32 = 0;
        let mut cur: XmlRelaxNGDefinePtr;

        let starts: XmlRelaxNGDefinePtr = (*grammar).start;
        if starts.is_null() || (*starts).next.is_null() {
            return;
        }
        cur = starts;
        while !cur.is_null() {
            let combine = if let Some(parent) = (*cur)
                .node
                .and_then(|node| node.parent)
                .filter(|parent| parent.name().as_deref() == Some("start"))
            {
                parent.get_prop("combine")
            } else {
                xml_rng_perr!(
                    ctxt,
                    (*cur).node.map(|node| node.into()),
                    XmlParserErrors::XmlRngpStartMissing,
                    "Internal error: start element not found\n"
                );
                None
            };

            if let Some(combine) = combine {
                if combine == "choice" {
                    if choice_or_interleave == -1 {
                        choice_or_interleave = 1;
                    } else if choice_or_interleave == 0 {
                        xml_rng_perr!(
                            ctxt,
                            (*cur).node.map(|node| node.into()),
                            XmlParserErrors::XmlRngpStartChoiceAndInterleave,
                            "<start> use both 'choice' and 'interleave'\n"
                        );
                    }
                } else if combine == "interleave" {
                    if choice_or_interleave == -1 {
                        choice_or_interleave = 0;
                    } else if choice_or_interleave == 1 {
                        xml_rng_perr!(
                            ctxt,
                            (*cur).node.map(|node| node.into()),
                            XmlParserErrors::XmlRngpStartChoiceAndInterleave,
                            "<start> use both 'choice' and 'interleave'\n"
                        );
                    }
                } else {
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpUnknownCombine,
                        "<start> uses unknown combine value '{}''\n",
                        combine
                    );
                }
            } else if missing == 0 {
                missing = 1;
            } else {
                xml_rng_perr!(
                    ctxt,
                    (*cur).node.map(|node| node.into()),
                    XmlParserErrors::XmlRngpNeedCombine,
                    "Some <start> element miss the combine attribute\n"
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
            if (*ctxt).interleaves.is_none() {
                (*ctxt).interleaves = XmlHashTableRef::with_capacity(10);
            }
            if let Some(mut table) = (*ctxt).interleaves {
                let mut tmpname: [c_char; 32] = [0; 32];

                snprintf(
                    tmpname.as_mut_ptr() as _,
                    32,
                    c"interleave%d".as_ptr() as _,
                    (*ctxt).nb_interleaves,
                );
                (*ctxt).nb_interleaves += 1;
                if table
                    .add_entry(
                        CStr::from_ptr(tmpname.as_ptr()).to_string_lossy().as_ref(),
                        cur,
                    )
                    .is_err()
                {
                    let tmpname = CStr::from_ptr(tmpname.as_ptr()).to_string_lossy();
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpInterleaveCreateFailed,
                        "Failed to add {} to hash table\n",
                        tmpname
                    );
                }
            } else {
                xml_rng_perr!(
                    ctxt,
                    (*cur).node.map(|node| node.into()),
                    XmlParserErrors::XmlRngpInterleaveCreateFailed,
                    "Failed to create interleaves hash table\n"
                );
            }
        }
    }
}

/// Parse a Relax-NG <grammar> node
///
/// Returns the internal xmlRelaxNGGrammarPtr built or NULL in case of error
#[doc(alias = "xmlRelaxNGParseGrammar")]
unsafe fn xml_relaxng_parse_grammar(
    ctxt: XmlRelaxNGParserCtxtPtr,
    nodes: Option<XmlNodePtr>,
) -> XmlRelaxNGGrammarPtr {
    unsafe {
        let mut tmp: XmlRelaxNGGrammarPtr;

        let ret: XmlRelaxNGGrammarPtr = xml_relaxng_new_grammar(ctxt);
        if ret.is_null() {
            return null_mut();
        }

        // Link the new grammar in the tree
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
            xml_rng_perr!(
                ctxt,
                nodes.map(|nodes| nodes.into()),
                XmlParserErrors::XmlRngpGrammarContent,
                "Failed to parse <grammar> content\n"
            );
        } else if (*(*ctxt).grammar).start.is_null() {
            xml_rng_perr!(
                ctxt,
                nodes.map(|nodes| nodes.into()),
                XmlParserErrors::XmlRngpGrammarNoStart,
                "Element <grammar> has no <start>\n"
            );
        }

        // Apply 4.17 merging rules to defines and starts
        xml_relaxng_combine_start(ctxt, ret);
        if let Some(defs) = (*ret).defs {
            defs.scan(|data, name, _, _| xml_relaxng_check_combine(*data, ctxt, name.unwrap()));
        }

        // link together defines and refs in this grammar
        if let Some(refs) = (*ret).refs {
            refs.scan(|data, name, _, _| {
                let name = name.map(|n| CString::new(n.as_ref()).unwrap());
                xml_relaxng_check_reference(
                    *data,
                    ctxt,
                    name.as_deref()
                        .map_or(null_mut(), |p| p.as_ptr() as *const u8),
                )
            });
        }

        (*ctxt).grammar = old;
        ret
    }
}

/// Check for cycles.
///
/// Returns 0 if check passed, and -1 in case of error
#[doc(alias = "xmlRelaxNGCheckCycles")]
unsafe fn xml_relaxng_check_cycles(
    ctxt: XmlRelaxNGParserCtxtPtr,
    mut cur: XmlRelaxNGDefinePtr,
    depth: i32,
) -> i32 {
    unsafe {
        let mut ret: i32 = 0;

        while ret == 0 && !cur.is_null() {
            if matches!((*cur).typ, XmlRelaxNGType::Ref | XmlRelaxNGType::Parentref) {
                if (*cur).depth == -1 {
                    (*cur).depth = depth as _;
                    ret = xml_relaxng_check_cycles(ctxt, (*cur).content, depth);
                    (*cur).depth = -2;
                } else if depth == (*cur).depth as i32 {
                    let name = CStr::from_ptr((*cur).name as *const i8).to_string_lossy();
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpRefCycle,
                        "Detected a cycle in {} references\n",
                        name
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
}

/// Try to unlink a definition. If not possible make it a NOOP
///
/// Returns the new prev definition
#[doc(alias = "xmlRelaxNGTryUnlink")]
unsafe fn xml_relaxng_try_unlink(
    _ctxt: XmlRelaxNGParserCtxtPtr,
    cur: XmlRelaxNGDefinePtr,
    parent: XmlRelaxNGDefinePtr,
    mut prev: XmlRelaxNGDefinePtr,
) -> XmlRelaxNGDefinePtr {
    unsafe {
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
}

/// Check if the definition can only generate attributes
///
/// Returns 1 if yes, 0 if no and -1 in case of error.
#[doc(alias = "xmlRelaxNGGenerateAttributes")]
unsafe fn xml_relaxng_generate_attributes(
    ctxt: XmlRelaxNGParserCtxtPtr,
    def: XmlRelaxNGDefinePtr,
) -> i32 {
    unsafe {
        let mut parent: XmlRelaxNGDefinePtr;
        let mut cur: XmlRelaxNGDefinePtr;
        let mut tmp: XmlRelaxNGDefinePtr;

        // Don't run that check in case of error. Infinite recursion
        // becomes possible.
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
}

/// Check for simplification of empty and notAllowed
#[doc(alias = "xmlRelaxNGSimplify")]
unsafe fn xml_relaxng_simplify(
    ctxt: XmlRelaxNGParserCtxtPtr,
    mut cur: XmlRelaxNGDefinePtr,
    parent: XmlRelaxNGDefinePtr,
) {
    unsafe {
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
                // On Elements, try to move attribute only generating rules on
                // the attrs rules.
                if (*cur).typ == XmlRelaxNGType::Element {
                    let mut attronly: i32;
                    let mut tmp: XmlRelaxNGDefinePtr;
                    let mut pre: XmlRelaxNGDefinePtr;

                    while !(*cur).content.is_null() {
                        attronly = xml_relaxng_generate_attributes(ctxt, (*cur).content);
                        if attronly == 1 {
                            // migrate (*cur).content to attrs
                            tmp = (*cur).content;
                            (*cur).content = (*tmp).next;
                            (*tmp).next = (*cur).attrs;
                            (*cur).attrs = tmp;
                        } else {
                            // (*cur).content can generate elements or text
                            break;
                        }
                    }
                    pre = (*cur).content;
                    while !pre.is_null() && !(*pre).next.is_null() {
                        tmp = (*pre).next;
                        attronly = xml_relaxng_generate_attributes(ctxt, tmp);
                        if attronly == 1 {
                            // migrate tmp to attrs
                            (*pre).next = (*tmp).next;
                            (*tmp).next = (*cur).attrs;
                            (*cur).attrs = tmp;
                        } else {
                            pre = tmp;
                        }
                    }
                }
                // This may result in a simplification
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
                // the current node may have been transformed back
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
                            XmlRelaxNGType::Group
                                | XmlRelaxNGType::Interleave
                                | XmlRelaxNGType::Choice
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
}

/// Detects violations of rule 7.3
#[doc(alias = "xmlRelaxNGCheckGroupAttrs")]
unsafe fn xml_relaxng_check_group_attrs(ctxt: XmlRelaxNGParserCtxtPtr, def: XmlRelaxNGDefinePtr) {
    unsafe {
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

        // Don't run that check in case of error. Infinite recursion becomes possible.
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
            xml_rng_perr_memory(ctxt, Some("building group\n"));
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
                    xml_rng_perr!(
                        ctxt,
                        (*def).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpGroupAttrConflict,
                        "Attributes conflicts in group\n"
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
}

/// Try to group 2 content types
///
/// Returns the content type
#[doc(alias = "xmlRelaxNGGroupContentType")]
fn xml_relaxng_group_content_type(
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

/// Check if a definition is nullable.
///
/// Returns 1 if yes, 0 if no and -1 in case of error
#[doc(alias = "xmlRelaxNGIsNullable")]
unsafe fn xml_relaxng_is_nullable(define: XmlRelaxNGDefinePtr) -> i32 {
    unsafe {
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
}

/// Also used to find indeterministic pattern in choice
#[doc(alias = "xmlRelaxNGCheckChoiceDeterminism")]
unsafe fn xml_relaxng_check_choice_determinism(
    ctxt: XmlRelaxNGParserCtxtPtr,
    def: XmlRelaxNGDefinePtr,
) {
    unsafe {
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

        // Don't run that check in case of error. Infinite recursion becomes possible.
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
            xml_rng_perr_memory(ctxt, Some("building choice\n"));
            return;
        }
        i = 0;

        // a bit strong but safe
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
                        res = xml_hash_add_entry2(
                            triage,
                            c"#text".as_ptr() as _,
                            null_mut(),
                            cur as _,
                        );
                        if res != 0 {
                            is_triable = -1;
                        }
                    } else if (*(*tmp)).typ == XmlRelaxNGType::Element && !(*(*tmp)).name.is_null()
                    {
                        if (*(*tmp)).ns.is_null() || *(*(*tmp)).ns.add(0) == 0 {
                            res = xml_hash_add_entry2(triage, (*(*tmp)).name, null_mut(), cur as _);
                        } else {
                            res =
                                xml_hash_add_entry2(triage, (*(*tmp)).name, (*(*tmp)).ns, cur as _);
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
}

/// Compute the max content-type
///
/// Returns the content type
#[doc(alias = "xmlRelaxNGMaxContentType")]
fn xml_relaxng_max_content_type(
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

/// Check for rules in section 7.1 and 7.2
///
/// Returns the content type of @cur
#[doc(alias = "xmlRelaxNGCheckRules")]
unsafe fn xml_relaxng_check_rules(
    ctxt: XmlRelaxNGParserCtxtPtr,
    mut cur: XmlRelaxNGDefinePtr,
    flags: i32,
    ptype: XmlRelaxNGType,
) -> XmlRelaxNGContentType {
    unsafe {
        let mut nflags: i32;
        let mut ret: XmlRelaxNGContentType;
        let mut tmp: XmlRelaxNGContentType;
        let mut val: XmlRelaxNGContentType = XmlRelaxNGContentType::Empty;

        while !cur.is_null() {
            // ret = XmlRelaxNGContentType::Empty;
            if matches!((*cur).typ, XmlRelaxNGType::Ref | XmlRelaxNGType::Parentref) {
                // This should actually be caught by list//element(ref) at the
                // element boundaries, c.f. Bug #159968 local refs are dropped in step 4.19.
                if flags & XML_RELAXNG_IN_DATAEXCEPT != 0 {
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatDataExceptRef,
                        "Found forbidden pattern data/except//ref\n"
                    );
                }
                if (*cur).content.is_null() {
                    if (*cur).typ == XmlRelaxNGType::Parentref {
                        xml_rng_perr!(
                            ctxt,
                            (*cur).node.map(|node| node.into()),
                            XmlParserErrors::XmlRngpRefNoDef,
                            "Internal found no define for parent refs\n"
                        );
                    } else {
                        xml_rng_perr!(
                            ctxt,
                            (*cur).node.map(|node| node.into()),
                            XmlParserErrors::XmlRngpRefNoDef,
                            "Internal found no define for ref {}\n",
                            if !(*cur).name.is_null() {
                                CStr::from_ptr((*cur).name as *const i8)
                                    .to_string_lossy()
                                    .into_owned()
                            } else {
                                "null".to_owned()
                            }
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
                // The 7.3 Attribute derivation rule for groups is plugged there
                xml_relaxng_check_group_attrs(ctxt, cur);
                if flags & XML_RELAXNG_IN_DATAEXCEPT != 0 {
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatDataExceptElem,
                        "Found forbidden pattern data/except//element(ref)\n"
                    );
                }
                if flags & XML_RELAXNG_IN_LIST != 0 {
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatListElem,
                        "Found forbidden pattern list//element(ref)\n"
                    );
                }
                if flags & XML_RELAXNG_IN_ATTRIBUTE != 0 {
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatAttrElem,
                        "Found forbidden pattern attribute//element(ref)\n"
                    );
                }
                if flags & XML_RELAXNG_IN_ATTRIBUTE != 0 {
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatAttrElem,
                        "Found forbidden pattern attribute//element(ref)\n"
                    );
                }
                // reset since in the simple form elements are only child of grammar/define
                nflags = 0;
                ret = xml_relaxng_check_rules(ctxt, (*cur).attrs, nflags, (*cur).typ);
                if ret != XmlRelaxNGContentType::Empty {
                    let name = CStr::from_ptr((*cur).name as *const i8).to_string_lossy();
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpElemContentEmpty,
                        "Element {} attributes have a content type error\n",
                        name
                    );
                }
                ret = xml_relaxng_check_rules(ctxt, (*cur).content, nflags, (*cur).typ);
                if ret == XmlRelaxNGContentType::Error {
                    let name = CStr::from_ptr((*cur).name as *const i8).to_string_lossy();
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpElemContentError,
                        "Element {} has a content type error\n",
                        name
                    );
                } else {
                    ret = XmlRelaxNGContentType::Complex;
                }
            } else if (*cur).typ == XmlRelaxNGType::Attribute {
                if flags & XML_RELAXNG_IN_ATTRIBUTE != 0 {
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatAttrAttr,
                        "Found forbidden pattern attribute//attribute\n"
                    );
                }
                if flags & XML_RELAXNG_IN_LIST != 0 {
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatListAttr,
                        "Found forbidden pattern list//attribute\n"
                    );
                }
                if flags & XML_RELAXNG_IN_OOMGROUP != 0 {
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatOnemoreGroupAttr,
                        "Found forbidden pattern oneOrMore//group//attribute\n"
                    );
                }
                if flags & XML_RELAXNG_IN_OOMINTERLEAVE != 0 {
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatOnemoreInterleaveAttr,
                        "Found forbidden pattern oneOrMore//interleave//attribute\n"
                    );
                }
                if flags & XML_RELAXNG_IN_DATAEXCEPT != 0 {
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatDataExceptAttr,
                        "Found forbidden pattern data/except//attribute\n"
                    );
                }
                if flags & XML_RELAXNG_IN_START != 0 {
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatStartAttr,
                        "Found forbidden pattern start//attribute\n"
                    );
                }
                if flags & XML_RELAXNG_IN_ONEORMORE == 0
                && (*cur).name.is_null()
                /* following is checking alternative name class readiness
                   in case it went the "choice" route */
                && (*cur).name_class.is_null()
                {
                    if (*cur).ns.is_null() {
                        xml_rng_perr!(
                            ctxt,
                            (*cur).node.map(|node| node.into()),
                            XmlParserErrors::XmlRngpAnynameAttrAncestor,
                            "Found anyName attribute without oneOrMore ancestor\n"
                        );
                    } else {
                        xml_rng_perr!(
                            ctxt,
                            (*cur).node.map(|node| node.into()),
                            XmlParserErrors::XmlRngpNsNameAttrAncestor,
                            "Found nsName attribute without oneOrMore ancestor\n"
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
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatDataExceptOnemore,
                        "Found forbidden pattern data/except//oneOrMore\n"
                    );
                }
                if flags & XML_RELAXNG_IN_START != 0 {
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatStartOnemore,
                        "Found forbidden pattern start//oneOrMore\n"
                    );
                }
                nflags = flags | XML_RELAXNG_IN_ONEORMORE;
                ret = xml_relaxng_check_rules(ctxt, (*cur).content, nflags, (*cur).typ);
                ret = xml_relaxng_group_content_type(ret, ret);
            } else if (*cur).typ == XmlRelaxNGType::List {
                if flags & XML_RELAXNG_IN_LIST != 0 {
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatListList,
                        "Found forbidden pattern list//list\n"
                    );
                }
                if flags & XML_RELAXNG_IN_DATAEXCEPT != 0 {
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatDataExceptList,
                        "Found forbidden pattern data/except//list\n"
                    );
                }
                if flags & XML_RELAXNG_IN_START != 0 {
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatStartList,
                        "Found forbidden pattern start//list\n"
                    );
                }
                nflags = flags | XML_RELAXNG_IN_LIST;
                ret = xml_relaxng_check_rules(ctxt, (*cur).content, nflags, (*cur).typ);
            } else if (*cur).typ == XmlRelaxNGType::Group {
                if flags & XML_RELAXNG_IN_DATAEXCEPT != 0 {
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatDataExceptGroup,
                        "Found forbidden pattern data/except//group\n"
                    );
                }
                if flags & XML_RELAXNG_IN_START != 0 {
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatStartGroup,
                        "Found forbidden pattern start//group\n"
                    );
                }
                if flags & XML_RELAXNG_IN_ONEORMORE != 0 {
                    nflags = flags | XML_RELAXNG_IN_OOMGROUP;
                } else {
                    nflags = flags;
                }
                ret = xml_relaxng_check_rules(ctxt, (*cur).content, nflags, (*cur).typ);
                // The 7.3 Attribute derivation rule for groups is plugged there
                xml_relaxng_check_group_attrs(ctxt, cur);
            } else if (*cur).typ == XmlRelaxNGType::Interleave {
                if flags & XML_RELAXNG_IN_LIST != 0 {
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatListInterleave,
                        "Found forbidden pattern list//interleave\n"
                    );
                }
                if flags & XML_RELAXNG_IN_DATAEXCEPT != 0 {
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatDataExceptInterleave,
                        "Found forbidden pattern data/except//interleave\n"
                    );
                }
                if flags & XML_RELAXNG_IN_START != 0 {
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatDataExceptInterleave,
                        "Found forbidden pattern start//interleave\n"
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
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatStartData,
                        "Found forbidden pattern start//data\n"
                    );
                }
                xml_relaxng_check_rules(ctxt, (*cur).content, flags, (*cur).typ);
                ret = XmlRelaxNGContentType::Simple;
            } else if (*cur).typ == XmlRelaxNGType::Value {
                if flags & XML_RELAXNG_IN_START != 0 {
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatStartValue,
                        "Found forbidden pattern start//value\n"
                    );
                }
                xml_relaxng_check_rules(ctxt, (*cur).content, flags, (*cur).typ);
                ret = XmlRelaxNGContentType::Simple;
            } else if (*cur).typ == XmlRelaxNGType::Text {
                if flags & XML_RELAXNG_IN_LIST != 0 {
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatListText,
                        "Found forbidden pattern list//text\n"
                    );
                }
                if flags & XML_RELAXNG_IN_DATAEXCEPT != 0 {
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatDataExceptText,
                        "Found forbidden pattern data/except//text\n"
                    );
                }
                if flags & XML_RELAXNG_IN_START != 0 {
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatStartText,
                        "Found forbidden pattern start//text\n"
                    );
                }
                ret = XmlRelaxNGContentType::Complex;
            } else if (*cur).typ == XmlRelaxNGType::Empty {
                if flags & XML_RELAXNG_IN_DATAEXCEPT != 0 {
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatDataExceptEmpty,
                        "Found forbidden pattern data/except//empty\n"
                    );
                }
                if flags & XML_RELAXNG_IN_START != 0 {
                    xml_rng_perr!(
                        ctxt,
                        (*cur).node.map(|node| node.into()),
                        XmlParserErrors::XmlRngpPatStartEmpty,
                        "Found forbidden pattern start//empty\n"
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
                // TODO: scan complain that tmp is never used, seems on purpose
                //       need double-checking
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
}

/// parse a Relax-NG definition resource and build an internal
/// xmlRelaxNG structure which can be used to validate instances.
///
/// Returns the internal XML RelaxNG structure built or NULL in case of error
#[doc(alias = "xmlRelaxNGParseDocument")]
unsafe fn xml_relaxng_parse_document(
    ctxt: XmlRelaxNGParserCtxtPtr,
    node: XmlNodePtr,
) -> XmlRelaxNGPtr {
    unsafe {
        let old: XmlRelaxNGGrammarPtr;

        // if node.is_null() {
        //     return null_mut();
        // }
        if ctxt.is_null() {
            return null_mut();
        }

        let schema: XmlRelaxNGPtr = xml_relaxng_new_relaxng(ctxt);
        if schema.is_null() {
            return null_mut();
        }

        let olddefine: *const XmlChar = (*ctxt).define;
        (*ctxt).define = null_mut();
        if is_relaxng(node, "grammar") {
            (*schema).topgrammar = xml_relaxng_parse_grammar(
                ctxt,
                node.children.map(|c| XmlNodePtr::try_from(c).unwrap()),
            );
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
            // Link the new grammar in the tree
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
}

/// Check if a definition is nullable.
///
/// Returns 1 if yes, 0 if no and -1 in case of error
#[doc(alias = "xmlRelaxNGIsCompilable")]
unsafe fn xml_relaxng_is_compilable(def: XmlRelaxNGDefinePtr) -> i32 {
    unsafe {
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
                // Check if the element content is compilable
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
                    // Because the routine is recursive, we must guard against
                    // discovering both COMPILABLE and NOT_COMPILABLE
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
                // All elements return a compilable status unless they
                // are generic like anyName
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
}

/// Compile the set of definitions, it works recursively, till the
/// element boundaries, where it tries to compile the content if possible
///
/// Returns 0 if success and -1 in case of error
#[doc(alias = "xmlRelaxNGCompile")]
unsafe fn xml_relaxng_compile(ctxt: XmlRelaxNGParserCtxtPtr, def: XmlRelaxNGDefinePtr) -> i32 {
    unsafe {
        let mut ret: i32 = 0;
        let mut list: XmlRelaxNGDefinePtr;

        if ctxt.is_null() || def.is_null() {
            return -1;
        }

        match (*def).typ {
            XmlRelaxNGType::Start => {
                if xml_relaxng_is_compilable(def) == 1 && (*def).depth != -25 {
                    let oldam = (*ctxt).am.take();
                    let oldstate = (*ctxt).state;

                    (*def).depth = -25;

                    list = (*def).content;
                    (*ctxt).am = XmlAutomata::new();
                    if (*ctxt).am.is_none() {
                        return -1;
                    }

                    // assume identical strings but not same pointer are different
                    // atoms, needed for non-determinism detection
                    // That way if 2 elements with the same name are in a choice
                    // branch the automata is found non-deterministic and
                    // we fallback to the normal validation which does the right
                    // thing of exploring both choices.
                    (*ctxt).am.as_mut().unwrap().set_flags(1);

                    (*ctxt).state = (*ctxt).am.as_ref().unwrap().get_init_state();
                    while !list.is_null() {
                        xml_relaxng_compile(ctxt, list);
                        list = (*list).next;
                    }
                    (*ctxt)
                        .am
                        .as_mut()
                        .unwrap()
                        .get_state_mut((*ctxt).state)
                        .unwrap()
                        .set_final_state();
                    if (*ctxt).am.as_mut().unwrap().is_determinist() != 0 {
                        (*def).cont_model = (*ctxt).am.as_mut().unwrap().compile().map(Rc::new);
                    }

                    (*ctxt).state = oldstate;
                    (*ctxt).am = oldam;
                }
            }
            XmlRelaxNGType::Element => {
                if let Some(am) = (*ctxt).am.as_mut() {
                    if !(*def).name.is_null() {
                        (*ctxt).state = am.new_transition2(
                            (*ctxt).state,
                            usize::MAX,
                            CStr::from_ptr((*def).name as *const i8)
                                .to_string_lossy()
                                .as_ref(),
                            (!(*def).ns.is_null())
                                .then(|| CStr::from_ptr((*def).ns as *const i8).to_string_lossy())
                                .as_deref(),
                            def as _,
                        );
                    }
                }
                if (*def).dflags & IS_COMPILABLE as i16 != 0 && (*def).depth != -25 {
                    let oldam = (*ctxt).am.take();
                    let oldstate = (*ctxt).state;

                    (*def).depth = -25;

                    list = (*def).content;
                    (*ctxt).am = XmlAutomata::new();
                    if (*ctxt).am.is_none() {
                        return -1;
                    }
                    (*ctxt).am.as_mut().unwrap().set_flags(1);
                    (*ctxt).state = (*ctxt).am.as_ref().unwrap().get_init_state();
                    while !list.is_null() {
                        xml_relaxng_compile(ctxt, list);
                        list = (*list).next;
                    }
                    (*ctxt)
                        .am
                        .as_mut()
                        .unwrap()
                        .get_state_mut((*ctxt).state)
                        .unwrap()
                        .set_final_state();
                    if let Some(mut model) = (*ctxt).am.as_mut().unwrap().compile() {
                        model.computes_determinism();
                        // we can only use the automata if it is determinist
                        if model.is_determinist() != 0 {
                            (*def).cont_model = Some(Rc::new(model));
                        } else {
                            (*def).cont_model = None;
                        }
                    } else {
                        (*def).cont_model = None;
                    }
                    (*ctxt).state = oldstate;
                    (*ctxt).am = oldam;
                } else {
                    let oldam = (*ctxt).am.take();

                    // we can't build the content model for this element content
                    // but it still might be possible to build it for some of its
                    // children, recurse.
                    ret = xml_relaxng_try_compile(ctxt, def);
                    (*ctxt).am = oldam;
                }
            }
            XmlRelaxNGType::Noop => ret = xml_relaxng_compile(ctxt, (*def).content),
            XmlRelaxNGType::Optional => {
                let oldstate = (*ctxt).state;

                list = (*def).content;
                while !list.is_null() {
                    xml_relaxng_compile(ctxt, list);
                    list = (*list).next;
                }
                (*ctxt)
                    .am
                    .as_mut()
                    .unwrap()
                    .new_epsilon(oldstate, (*ctxt).state);
            }
            XmlRelaxNGType::Zeroormore => {
                (*ctxt).state = (*ctxt)
                    .am
                    .as_mut()
                    .unwrap()
                    .new_epsilon((*ctxt).state, usize::MAX);
                let oldstate = (*ctxt).state;
                list = (*def).content;
                while !list.is_null() {
                    xml_relaxng_compile(ctxt, list);
                    list = (*list).next;
                }
                (*ctxt)
                    .am
                    .as_mut()
                    .unwrap()
                    .new_epsilon((*ctxt).state, oldstate);
                (*ctxt).state = (*ctxt)
                    .am
                    .as_mut()
                    .unwrap()
                    .new_epsilon(oldstate, usize::MAX);
            }
            XmlRelaxNGType::Oneormore => {
                list = (*def).content;
                while !list.is_null() {
                    xml_relaxng_compile(ctxt, list);
                    list = (*list).next;
                }
                let oldstate = (*ctxt).state;
                list = (*def).content;
                while !list.is_null() {
                    xml_relaxng_compile(ctxt, list);
                    list = (*list).next;
                }
                (*ctxt)
                    .am
                    .as_mut()
                    .unwrap()
                    .new_epsilon((*ctxt).state, oldstate);
                (*ctxt).state = (*ctxt)
                    .am
                    .as_mut()
                    .unwrap()
                    .new_epsilon(oldstate, usize::MAX);
            }
            XmlRelaxNGType::Choice => {
                let mut target = usize::MAX;
                let oldstate = (*ctxt).state;

                list = (*def).content;
                while !list.is_null() {
                    (*ctxt).state = oldstate;
                    ret = xml_relaxng_compile(ctxt, list);
                    if ret != 0 {
                        break;
                    }
                    if target == usize::MAX {
                        target = (*ctxt).state;
                    } else {
                        (*ctxt)
                            .am
                            .as_mut()
                            .unwrap()
                            .new_epsilon((*ctxt).state, target);
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
                (*ctxt).state = (*ctxt)
                    .am
                    .as_mut()
                    .unwrap()
                    .new_epsilon((*ctxt).state, usize::MAX);
                let oldstate = (*ctxt).state;
                xml_relaxng_compile(ctxt, (*def).content);
                (*ctxt).am.as_mut().unwrap().new_transition(
                    (*ctxt).state,
                    (*ctxt).state,
                    "#text",
                    null_mut(),
                );
                (*ctxt).state = (*ctxt)
                    .am
                    .as_mut()
                    .unwrap()
                    .new_epsilon(oldstate, usize::MAX);
            }
            XmlRelaxNGType::Empty => {
                (*ctxt).state = (*ctxt)
                    .am
                    .as_mut()
                    .unwrap()
                    .new_epsilon((*ctxt).state, usize::MAX)
            }
            XmlRelaxNGType::Except
            | XmlRelaxNGType::Attribute
            | XmlRelaxNGType::Interleave
            | XmlRelaxNGType::NotAllowed
            | XmlRelaxNGType::Datatype
            | XmlRelaxNGType::List
            | XmlRelaxNGType::Param
            | XmlRelaxNGType::Value => {
                // This should not happen and generate an internal error
                eprintln!("RNG internal error trying to compile {}", (*def).name());
            }
        }
        ret
    }
}

/// Try to compile the set of definitions, it works recursively,
/// possibly ignoring parts which cannot be compiled.
///
/// Returns 0 if success and -1 in case of error
#[doc(alias = "xmlRelaxNGTryCompile")]
unsafe fn xml_relaxng_try_compile(ctxt: XmlRelaxNGParserCtxtPtr, def: XmlRelaxNGDefinePtr) -> i32 {
    unsafe {
        let mut ret: i32 = 0;
        let mut list: XmlRelaxNGDefinePtr;

        if ctxt.is_null() || def.is_null() {
            return -1;
        }

        if matches!((*def).typ, XmlRelaxNGType::Start | XmlRelaxNGType::Element) {
            ret = xml_relaxng_is_compilable(def);
            if (*def).dflags & IS_COMPILABLE as i16 != 0 && (*def).depth != -25 {
                (*ctxt).am = None;
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
}

/// Parse a schema definition resource and build an internal
/// XML Schema structure which can be used to validate instances.
///
/// Returns the internal XML RelaxNG structure built from the resource or NULL in case of error
#[doc(alias = "xmlRelaxNGParse")]
pub unsafe fn xml_relaxng_parse(ctxt: XmlRelaxNGParserCtxtPtr) -> XmlRelaxNGPtr {
    unsafe {
        xml_relaxng_init_types();

        if ctxt.is_null() {
            return null_mut();
        }

        // First step is to parse the input document into an DOM/Infoset
        let doc = if let Some(url) = (*ctxt).url.as_deref() {
            let Some(doc) = xml_read_file(url, None, 0) else {
                xml_rng_perr!(
                    ctxt,
                    None,
                    XmlParserErrors::XmlRngpParseError,
                    "xmlRelaxNGParse: could not load {}\n",
                    url
                );
                return null_mut();
            };
            doc
        } else if !(*ctxt).buffer.is_null() {
            let mem = from_raw_parts((*ctxt).buffer as *const u8, (*ctxt).size as usize);
            let Some(mut doc) = xml_read_memory(mem, None, None, 0) else {
                xml_rng_perr!(
                    ctxt,
                    None,
                    XmlParserErrors::XmlRngpParseError,
                    "xmlRelaxNGParse: could not parse schemas\n"
                );
                return null_mut();
            };
            doc.url = Some("in_memory_buffer".to_owned());
            (*ctxt).url = Some("in_memory_buffer".to_owned());
            doc
        } else if let Some(document) = (*ctxt).document {
            document
        } else {
            xml_rng_perr!(
                ctxt,
                None,
                XmlParserErrors::XmlRngpEmpty,
                "xmlRelaxNGParse: nothing to parse\n"
            );
            return null_mut();
        };
        (*ctxt).document = Some(doc);

        // Some preprocessing of the document content
        let Some(doc) = xml_relaxng_cleanup_doc(ctxt, doc) else {
            if let Some(document) = (*ctxt).document.take() {
                xml_free_doc(document);
            }
            return null_mut();
        };

        // Then do the parsing for good
        let Some(root) = doc.get_root_element() else {
            xml_rng_perr!(
                ctxt,
                Some(doc.into()),
                XmlParserErrors::XmlRngpEmpty,
                "xmlRelaxNGParse: {} is empty\n",
                (*ctxt).url.as_deref().unwrap_or("schemas")
            );

            if let Some(document) = (*ctxt).document.take() {
                xml_free_doc(document);
            }
            return null_mut();
        };
        let ret: XmlRelaxNGPtr = xml_relaxng_parse_document(ctxt, root);
        if ret.is_null() {
            if let Some(document) = (*ctxt).document.take() {
                xml_free_doc(document);
            }
            return null_mut();
        }

        // Check the ref/defines links
        // try to preprocess interleaves
        if let Some(interleaves) = (*ctxt).interleaves {
            interleaves.scan(|data, _, _, _| {
                xml_relaxng_compute_interleaves(*data, ctxt);
            });
        }

        // if there was a parsing error return NULL
        if (*ctxt).nb_errors > 0 {
            xml_relaxng_free(ret);
            (*ctxt).document = None;
            xml_free_doc(doc);
            return null_mut();
        }

        // try to compile (parts of) the schemas
        if !(*ret).topgrammar.is_null() && !(*(*ret).topgrammar).start.is_null() {
            if (*(*(*ret).topgrammar).start).typ != XmlRelaxNGType::Start {
                let def: XmlRelaxNGDefinePtr = xml_relaxng_new_define(ctxt, None);
                if !def.is_null() {
                    (*def).typ = XmlRelaxNGType::Start;
                    (*def).content = (*(*ret).topgrammar).start;
                    (*(*ret).topgrammar).start = def;
                }
            }
            xml_relaxng_try_compile(ctxt, (*(*ret).topgrammar).start);
        }

        // Transfer the pointer for cleanup at the schema level.
        (*ret).doc = Some(doc);
        (*ctxt).document = None;
        (*ret).documents = (*ctxt).documents;
        (*ctxt).documents = null_mut();

        (*ret).includes = (*ctxt).includes;
        (*ctxt).includes = null_mut();
        (*ret).def_tab = take(&mut (*ctxt).def_tab);
        if (*ctxt).idref == 1 {
            (*ret).idref = 1;
        }

        ret
    }
}

/// Deallocate a RelaxNG grammar structure.
#[doc(alias = "xmlRelaxNGFreeGrammar")]
unsafe fn xml_relaxng_free_grammar(grammar: XmlRelaxNGGrammarPtr) {
    unsafe {
        if grammar.is_null() {
            return;
        }

        if !(*grammar).children.is_null() {
            xml_relaxng_free_grammar((*grammar).children);
        }
        if !(*grammar).next.is_null() {
            xml_relaxng_free_grammar((*grammar).next);
        }
        if let Some(mut refs) = (*grammar).refs.take().map(|t| t.into_inner()) {
            refs.clear();
        }
        if let Some(mut defs) = (*grammar).defs.take().map(|t| t.into_inner()) {
            defs.clear();
        }

        xml_free(grammar as _);
    }
}

/// Deallocate a RelaxNG structure.
#[doc(alias = "xmlRelaxNGFree")]
pub unsafe fn xml_relaxng_free(schema: XmlRelaxNGPtr) {
    unsafe {
        if schema.is_null() {
            return;
        }

        if !(*schema).topgrammar.is_null() {
            xml_relaxng_free_grammar((*schema).topgrammar);
        }
        if let Some(doc) = (*schema).doc.take() {
            xml_free_doc(doc);
        }
        if !(*schema).documents.is_null() {
            xml_relaxng_free_document_list((*schema).documents);
        }
        if !(*schema).includes.is_null() {
            xml_relaxng_free_include_list((*schema).includes);
        }
        for def in (*schema).def_tab.drain(..) {
            xml_relaxng_free_define(def);
        }

        drop_in_place(schema);
        xml_free(schema as _);
    }
}

/// Dump a RelaxNG structure back
#[doc(alias = "xmlRelaxNGDumpDefines")]
#[cfg(feature = "libxml_output")]
unsafe fn xml_relaxng_dump_defines<'a>(
    output: &mut (impl Write + 'a),
    mut defines: XmlRelaxNGDefinePtr,
) {
    unsafe {
        while !defines.is_null() {
            xml_relaxng_dump_define(output, defines);
            defines = (*defines).next;
        }
    }
}

/// Dump a RelaxNG structure back
#[doc(alias = "xmlRelaxNGDumpDefine")]
#[cfg(feature = "libxml_output")]
unsafe fn xml_relaxng_dump_define<'a>(output: &mut (impl Write + 'a), define: XmlRelaxNGDefinePtr) {
    unsafe {
        if define.is_null() {
            return;
        }
        match (*define).typ {
            XmlRelaxNGType::Empty => {
                writeln!(output, "<empty/>").ok();
            }
            XmlRelaxNGType::NotAllowed => {
                writeln!(output, "<notAllowed/>").ok();
            }
            XmlRelaxNGType::Text => {
                writeln!(output, "<text/>").ok();
            }
            XmlRelaxNGType::Element => {
                writeln!(output, "<element>").ok();
                if !(*define).name.is_null() {
                    write!(output, "<name").ok();
                    if !(*define).ns.is_null() {
                        let ns = CStr::from_ptr((*define).ns as *const i8).to_string_lossy();
                        write!(output, " ns=\"{ns}\"").ok();
                    }
                    let name = CStr::from_ptr((*define).name as *const i8).to_string_lossy();
                    writeln!(output, ">{name}</name>").ok();
                }
                xml_relaxng_dump_defines(output, (*define).attrs);
                xml_relaxng_dump_defines(output, (*define).content);
                writeln!(output, "</element>").ok();
            }
            XmlRelaxNGType::List => {
                writeln!(output, "<list>").ok();
                xml_relaxng_dump_defines(output, (*define).content);
                writeln!(output, "</list>").ok();
            }
            XmlRelaxNGType::Oneormore => {
                writeln!(output, "<oneOrMore>").ok();
                xml_relaxng_dump_defines(output, (*define).content);
                writeln!(output, "</oneOrMore>").ok();
            }
            XmlRelaxNGType::Zeroormore => {
                writeln!(output, "<zeroOrMore>").ok();
                xml_relaxng_dump_defines(output, (*define).content);
                writeln!(output, "</zeroOrMore>").ok();
            }
            XmlRelaxNGType::Choice => {
                writeln!(output, "<choice>").ok();
                xml_relaxng_dump_defines(output, (*define).content);
                writeln!(output, "</choice>").ok();
            }
            XmlRelaxNGType::Group => {
                writeln!(output, "<group>").ok();
                xml_relaxng_dump_defines(output, (*define).content);
                writeln!(output, "</group>").ok();
            }
            XmlRelaxNGType::Interleave => {
                writeln!(output, "<interleave>").ok();
                xml_relaxng_dump_defines(output, (*define).content);
                writeln!(output, "</interleave>").ok();
            }
            XmlRelaxNGType::Optional => {
                writeln!(output, "<optional>").ok();
                xml_relaxng_dump_defines(output, (*define).content);
                writeln!(output, "</optional>").ok();
            }
            XmlRelaxNGType::Attribute => {
                writeln!(output, "<attribute>").ok();
                xml_relaxng_dump_defines(output, (*define).content);
                writeln!(output, "</attribute>").ok();
            }
            XmlRelaxNGType::Def => {
                write!(output, "<define").ok();
                if !(*define).name.is_null() {
                    let name = CStr::from_ptr((*define).name as *const i8).to_string_lossy();
                    write!(output, " name=\"{name}\"").ok();
                }
                writeln!(output, ">").ok();
                xml_relaxng_dump_defines(output, (*define).content);
                writeln!(output, "</define>").ok();
            }
            XmlRelaxNGType::Ref => {
                write!(output, "<ref").ok();
                if !(*define).name.is_null() {
                    let name = CStr::from_ptr((*define).name as *const i8).to_string_lossy();
                    write!(output, " name=\"{name}\"").ok();
                }
                writeln!(output, ">").ok();
                xml_relaxng_dump_defines(output, (*define).content);
                writeln!(output, "</ref>").ok();
            }
            XmlRelaxNGType::Parentref => {
                write!(output, "<parentRef").ok();
                if !(*define).name.is_null() {
                    let name = CStr::from_ptr((*define).name as *const i8).to_string_lossy();
                    write!(output, " name=\"{name}\"").ok();
                }
                writeln!(output, ">").ok();
                xml_relaxng_dump_defines(output, (*define).content);
                writeln!(output, "</parentRef>").ok();
            }
            XmlRelaxNGType::Externalref => {
                write!(output, "<externalRef>").ok();
                xml_relaxng_dump_defines(output, (*define).content);
                writeln!(output, "</externalRef>").ok();
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
}

/// Dump a RelaxNG structure back
#[doc(alias = "xmlRelaxNGDumpGrammar")]
#[cfg(feature = "libxml_output")]
unsafe fn xml_relaxng_dump_grammar<'a>(
    output: &mut (impl Write + 'a),
    grammar: XmlRelaxNGGrammarPtr,
    top: i32,
) {
    unsafe {
        if grammar.is_null() {
            return;
        }

        write!(output, "<grammar").ok();
        if top != 0 {
            write!(output, " xmlns=\"http://relaxng.org/ns/structure/1.0\"").ok();
        }
        match (*grammar).combine {
            XmlRelaxNGCombine::Undefined => {}
            XmlRelaxNGCombine::Choice => {
                write!(output, " combine=\"choice\"").ok();
            }
            XmlRelaxNGCombine::Interleave => {
                write!(output, " combine=\"interleave\"").ok();
            } // _ => {
              //     write!(output, " <!-- invalid combine value -->").ok();
              // }
        }
        writeln!(output, ">").ok();
        if (*grammar).start.is_null() {
            write!(output, " <!-- grammar had no start -->").ok();
        } else {
            writeln!(output, "<start>").ok();
            xml_relaxng_dump_define(output, (*grammar).start);
            writeln!(output, "</start>").ok();
        }
        /* TODO ? Dump the defines ? */
        writeln!(output, "</grammar>").ok();
    }
}

/// Dump a RelaxNG structure back
#[doc(alias = "xmlRelaxNGDump")]
#[cfg(feature = "libxml_output")]
pub unsafe fn xml_relaxng_dump<'a>(output: &mut (impl Write + 'a), schema: XmlRelaxNGPtr) {
    unsafe {
        if schema.is_null() {
            writeln!(output, "RelaxNG empty or failed to compile").ok();
            return;
        }
        write!(output, "RelaxNG: ").ok();
        if let Some(doc) = (*schema).doc {
            if let Some(url) = doc.url.as_deref() {
                writeln!(output, "{url}").ok();
            } else {
                writeln!(output).ok();
            }
        } else {
            writeln!(output, "no document").ok();
        }
        if (*schema).topgrammar.is_null() {
            writeln!(output, "RelaxNG has no top grammar").ok();
            return;
        }
        xml_relaxng_dump_grammar(output, (*schema).topgrammar, 1);
    }
}

/// Dump the transformed RelaxNG tree.
#[doc(alias = "xmlRelaxNGDumpTree")]
#[cfg(feature = "libxml_output")]
pub unsafe fn xml_relaxng_dump_tree(output: &mut impl Write, schema: XmlRelaxNGPtr) {
    unsafe {
        if schema.is_null() {
            writeln!(output, "RelaxNG empty or failed to compile").ok();
            return;
        }
        if let Some(mut doc) = (*schema).doc {
            doc.dump_file(output);
        } else {
            writeln!(output, "no document").ok();
        }
    }
}

/// Set the error and warning callback information
#[doc(alias = "xmlRelaxNGSetValidErrors")]
pub unsafe fn xml_relaxng_set_valid_errors(
    ctxt: XmlRelaxNGValidCtxtPtr,
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
        (*ctxt).user_data = ctx;
        (*ctxt).serror = None;
    }
}

/// Get the error and warning callback information
///
/// Returns -1 in case of error and 0 otherwise
#[doc(alias = "xmlRelaxNGGetValidErrors")]
pub unsafe fn xml_relaxng_get_valid_errors(
    ctxt: XmlRelaxNGValidCtxtPtr,
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
            *ctx = (*ctxt).user_data.clone();
        }
        0
    }
}

/// Set the structured error callback
#[doc(alias = "xmlRelaxNGSetValidStructuredErrors")]
pub unsafe fn xml_relaxng_set_valid_structured_errors(
    ctxt: XmlRelaxNGValidCtxtPtr,
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
        (*ctxt).user_data = ctx;
    }
}

/// Add a RelaxNG validation state to the container without checking for unicity.
///
/// Return 1 in case of success and 0 if this is a duplicate and -1 on error
#[doc(alias = "xmlRelaxNGAddStateUniq")]
pub(crate) unsafe fn xml_relaxng_add_states_uniq(
    _ctxt: XmlRelaxNGValidCtxtPtr,
    states: XmlRelaxNGStatesPtr,
    state: XmlRelaxNGValidStatePtr,
) -> i32 {
    unsafe {
        if state.is_null() {
            return -1;
        }
        (*states).tab_state.push(state);
        1
    }
}

/// Skip ignorable nodes in that context
///
/// Returns the new sibling or NULL in case of error.
#[doc(alias = "xmlRelaxNGSkipIgnored")]
unsafe fn xml_relaxng_skip_ignored(
    ctxt: XmlRelaxNGValidCtxtPtr,
    mut node: Option<XmlNodePtr>,
) -> Option<XmlNodePtr> {
    unsafe {
        // TODO complete and handle entities
        while let Some(now) = node.filter(|node| {
            matches!(
                node.element_type(),
                XmlElementType::XmlCommentNode
                    | XmlElementType::XmlPINode
                    | XmlElementType::XmlXIncludeStart
                    | XmlElementType::XmlXIncludeEnd
            ) || (matches!(
                node.element_type(),
                XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
            ) && ((*ctxt).flags & FLAGS_MIXED_CONTENT != 0
                || xml_relaxng_is_blank(node.content.as_deref())))
        }) {
            node = now.next.map(|node| XmlNodePtr::try_from(node).unwrap());
        }
        node
    }
}

/// Skip to the next value when validating within a list
///
/// Returns 0 if the operation succeeded or an error code.
#[doc(alias = "xmlRelaxNGNextValue")]
unsafe fn xml_relaxng_next_value(ctxt: XmlRelaxNGValidCtxtPtr) -> i32 {
    unsafe {
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
}

/// Validate the given set of definitions for the current value
///
/// Returns 0 if the validation succeeded or an error code.
#[doc(alias = "xmlRelaxNGValidateValueList")]
unsafe fn xml_relaxng_validate_value_list(
    ctxt: XmlRelaxNGValidCtxtPtr,
    mut defines: XmlRelaxNGDefinePtr,
) -> i32 {
    unsafe {
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
}

/// Validate the given value against the datatype
///
/// Returns 0 if the validation succeeded or an error code.
#[doc(alias = "xmlRelaxNGValidateDatatype")]
unsafe fn xml_relaxng_validate_datatype(
    ctxt: XmlRelaxNGValidCtxtPtr,
    value: *const XmlChar,
    define: XmlRelaxNGDefinePtr,
    node: Option<XmlGenericNodePtr>,
) -> i32 {
    unsafe {
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
                    CStr::from_ptr((*define).name as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    value,
                    addr_of_mut!(result),
                    node,
                );
            } else {
                ret = check(
                    (*lib).data,
                    CStr::from_ptr((*define).name as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    value,
                    null_mut(),
                    node,
                );
            }
        } else {
            ret = -1;
        }
        if ret < 0 {
            VALID_ERR2!(
                ctxt,
                XmlRelaxNGValidErr::XmlRelaxngErrType,
                Some(
                    CStr::from_ptr((*define).name as *const i8)
                        .to_string_lossy()
                        .as_ref()
                )
            );
            if !result.is_null() && !lib.is_null() && (*lib).freef.is_some() {
                (*lib).freef.unwrap()((*lib).data, result);
            }
            return -1;
        } else if ret == 1 {
            ret = 0;
        } else if ret == 2 {
            VALID_ERR2P!(
                ctxt,
                XmlRelaxNGValidErr::XmlRelaxngErrDupid,
                Some(
                    CStr::from_ptr(value as *const i8)
                        .to_string_lossy()
                        .as_ref()
                )
            );
        } else {
            VALID_ERR3P!(
                ctxt,
                XmlRelaxNGValidErr::XmlRelaxngErrTypeval,
                Some(
                    CStr::from_ptr((*define).name as *const i8)
                        .to_string_lossy()
                        .as_ref()
                ),
                Some(
                    CStr::from_ptr(value as *const i8)
                        .to_string_lossy()
                        .as_ref()
                )
            );
            ret = -1;
        }
        cur = (*define).attrs;
        while ret == 0 && !cur.is_null() && (*cur).typ == XmlRelaxNGType::Param {
            if let Some(facet) = (*lib).facet {
                tmp = facet(
                    (*lib).data,
                    CStr::from_ptr((*define).name as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    CStr::from_ptr((*cur).name as *const i8)
                        .to_string_lossy()
                        .as_ref(),
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
}

/// Validate the given definition for the current value
///
/// Returns 0 if the validation succeeded or an error code.
#[doc(alias = "xmlRelaxNGValidateValue")]
unsafe fn xml_relaxng_validate_value(
    ctxt: XmlRelaxNGValidCtxtPtr,
    define: XmlRelaxNGDefinePtr,
) -> i32 {
    unsafe {
        let mut ret: i32 = 0;
        let oldflags: i32;

        let value: *mut XmlChar = (*(*ctxt).state).value;
        match (*define).typ {
            XmlRelaxNGType::Empty => {
                if !value.is_null() && *value.add(0) != 0 {
                    let mut idx: i32 = 0;

                    while (*value.add(idx as usize)).is_xml_blank_char() {
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
                                CStr::from_ptr((*define).name as *const i8)
                                    .to_string_lossy()
                                    .as_ref(),
                                (*define).value,
                                (*define).node.map(|node| node.into()),
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
                                Some(
                                    CStr::from_ptr((*define).name as *const i8)
                                        .to_string_lossy()
                                        .as_ref()
                                )
                            );
                            return -1;
                        } else if ret == 1 {
                            ret = 0;
                        } else {
                            ret = -1;
                        }
                    } else {
                        // TODO: trivial optimizations are possible by
                        // computing at compile-time
                        let defval = CStr::from_ptr((*define).value as *const i8).to_string_lossy();
                        if value.is_null() {
                            ret = -1;
                        } else {
                            let value = CStr::from_ptr(value as *const i8).to_string_lossy();
                            let nval = relaxng_normalize(&defval);
                            let nvalue = relaxng_normalize(&value);

                            if nval != nvalue {
                                ret = -1;
                            }
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
                } else if !(*ctxt).err_tab.is_empty() {
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
                    if (*cur).is_xml_blank_char() {
                        *cur = 0;
                        cur = cur.add(1);
                        while (*cur).is_xml_blank_char() {
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
                        Some(
                            CStr::from_ptr((*(*ctxt).state).value as *const i8)
                                .to_string_lossy()
                                .as_ref()
                        )
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
                if !(*ctxt).err_tab.is_empty() {
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
                    if !(*ctxt).err_tab.is_empty() {
                        xml_relaxng_pop_errors(ctxt, 0);
                    }
                    ret = 0;
                    break 'to_break;
                }
                if !(*ctxt).err_tab.is_empty() {
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
}

/// Validate the given definitions for the current value
///
/// Returns 0 if the validation succeeded or an error code.
#[doc(alias = "xmlRelaxNGValidateValueContent")]
unsafe fn xml_relaxng_validate_value_content(
    ctxt: XmlRelaxNGValidCtxtPtr,
    mut defines: XmlRelaxNGDefinePtr,
) -> i32 {
    unsafe {
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
}

/// Check if the attribute matches the definition nameClass
///
/// Returns 1 if the attribute matches, 0 if no, or -1 in case of error
#[doc(alias = "xmlRelaxNGAttributeMatch")]
unsafe fn xml_relaxng_attribute_match(
    _ctxt: XmlRelaxNGValidCtxtPtr,
    mut define: XmlRelaxNGDefinePtr,
    prop: XmlAttrPtr,
) -> i32 {
    unsafe {
        let mut ret: i32;

        if !(*define).name.is_null()
            && *CStr::from_ptr((*define).name as *const i8)
                .to_string_lossy()
                .as_ref()
                != *prop.name
        {
            return 0;
        }
        if !(*define).ns.is_null() {
            if *(*define).ns.add(0) == 0 {
                if prop.ns.is_some() {
                    return 0;
                }
            } else if prop
                .ns
                .as_deref()
                .and_then(|ns| ns.href())
                .is_none_or(|href| {
                    CStr::from_ptr((*define).ns as *const i8).to_string_lossy() != href
                })
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
}

/// Validate the given attribute definition for that node
///
/// Returns 0 if the validation succeeded or an error code.
#[doc(alias = "xmlRelaxNGValidateAttribute")]
unsafe fn xml_relaxng_validate_attribute(
    ctxt: XmlRelaxNGValidCtxtPtr,
    define: XmlRelaxNGDefinePtr,
) -> i32 {
    unsafe {
        let ret: i32;
        let oldvalue: *mut XmlChar;

        if (*(*ctxt).state).nb_attr_left <= 0 {
            return -1;
        }
        if !(*define).name.is_null() {
            if let Some((j, prop)) = (*(*ctxt).state)
                .attrs
                .iter()
                .enumerate()
                .filter_map(|(i, a)| a.map(|a| (i, a)))
                .find(|&(_, tmp)| {
                    *CStr::from_ptr((*define).name as *const i8)
                        .to_string_lossy()
                        .as_ref()
                        == *tmp.name
                        && ((((*define).ns.is_null() || *(*define).ns.add(0) == 0)
                            && tmp.ns.is_none())
                            || tmp
                                .ns
                                .as_deref()
                                .and_then(|ns| ns.href())
                                .is_some_and(|href| {
                                    CStr::from_ptr((*define).ns as *const i8).to_string_lossy()
                                        == href
                                }))
                })
            {
                let value = prop
                    .children()
                    .and_then(|c| c.get_string(prop.doc, 1))
                    .map(|c| CString::new(c).unwrap());
                let mut value = value
                    .as_ref()
                    .map_or(null_mut(), |c| xml_strdup(c.as_ptr() as *const u8));
                oldvalue = (*(*ctxt).state).value;
                let oldseq = (*(*ctxt).state).seq;
                (*(*ctxt).state).seq = Some(prop.into());
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
                    // flag the attribute as processed
                    (*(*ctxt).state).attrs[j] = None;
                    (*(*ctxt).state).nb_attr_left -= 1;
                }
            } else {
                ret = -1;
            }
        } else if let Some((j, prop)) = (*(*ctxt).state)
            .attrs
            .iter()
            .copied()
            .enumerate()
            .filter_map(|(i, a)| a.map(|a| (i, a)))
            .find(|&(_, tmp)| xml_relaxng_attribute_match(ctxt, define, tmp) == 1)
        {
            let value = prop
                .children()
                .and_then(|c| c.get_string(prop.doc, 1))
                .map(|c| CString::new(c).unwrap());
            let mut value = value
                .as_ref()
                .map_or(null_mut(), |c| xml_strdup(c.as_ptr() as *const u8));
            oldvalue = (*(*ctxt).state).value;
            let oldseq = (*(*ctxt).state).seq;
            (*(*ctxt).state).seq = Some(prop.into());
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
                // flag the attribute as processed
                (*(*ctxt).state).attrs[j] = None;
                (*(*ctxt).state).nb_attr_left -= 1;
            }
        } else {
            ret = -1;
        }

        ret
    }
}

/// Validate the given node against the list of attribute definitions
///
/// Returns 0 if the validation succeeded or an error code.
#[doc(alias = "xmlRelaxNGValidateAttributeList")]
unsafe fn xml_relaxng_validate_attribute_list(
    ctxt: XmlRelaxNGValidCtxtPtr,
    defines: XmlRelaxNGDefinePtr,
) -> i32 {
    unsafe {
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
                    // continues on -2
                    break;
                }
            }
            cur = (*cur).next;
        }

        ret
    }
}

/// Handle the callback and if needed validate the element children.
#[doc(alias = "xmlRelaxNGValidateCompiledCallback")]
fn xml_relaxng_validate_compiled_callback(
    _exec: XmlRegExecCtxtPtr,
    token: &str,
    transdata: *mut c_void,
    inputdata: *mut c_void,
) {
    unsafe {
        let ctxt: XmlRelaxNGValidCtxtPtr = inputdata as _;
        let define: XmlRelaxNGDefinePtr = transdata as _;

        if ctxt.is_null() {
            eprintln!("callback on {token} missing context");
            return;
        }
        if define.is_null() {
            if token.starts_with('#') {
                return;
            }
            eprintln!("callback on {token} missing define");
            if !ctxt.is_null() && (*ctxt).err_no == XmlRelaxNGValidErr::XmlRelaxngOk as i32 {
                (*ctxt).err_no = XmlRelaxNGValidErr::XmlRelaxngErrInternal as i32;
            }
            return;
        }
        if (*define).typ != XmlRelaxNGType::Element {
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
}

/// Validate the content model of an element or start using the regexp
///
/// Returns 0 in case of success, -1 in case of error.
#[doc(alias = "xmlRelaxNGValidateCompiledContent")]
unsafe fn xml_relaxng_validate_compiled_content(
    ctxt: XmlRelaxNGValidCtxtPtr,
    regexp: Rc<XmlRegexp>,
    content: Option<XmlNodePtr>,
) -> i32 {
    unsafe {
        let mut ret: i32 = 0;

        if ctxt.is_null() {
            return -1;
        }
        let oldperr: i32 = (*ctxt).perr;
        let mut exec = XmlRegExecCtxt::new(
            regexp,
            Some(xml_relaxng_validate_compiled_callback),
            ctxt as _,
        );
        (*ctxt).perr = 0;
        let mut cur = content;
        while let Some(now) = cur {
            (*(*ctxt).state).seq = Some(now.into());
            match now.element_type() {
                XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode => {
                    if !now.is_blank_node() {
                        ret = exec.push_string(Some("#text"), ctxt as _);
                        if ret < 0 {
                            VALID_ERR2!(
                                ctxt,
                                XmlRelaxNGValidErr::XmlRelaxngErrTextwrong,
                                now.parent().unwrap().name().as_deref()
                            );
                        }
                    }
                }
                XmlElementType::XmlElementNode => {
                    if let Some(ns) = now.ns {
                        ret = exec.push_string2(
                            now.name().as_deref().unwrap(),
                            ns.href().as_deref(),
                            ctxt as _,
                        );
                    } else {
                        ret = exec.push_string(now.name().as_deref(), ctxt as _);
                    }
                    if ret < 0 {
                        VALID_ERR2!(
                            ctxt,
                            XmlRelaxNGValidErr::XmlRelaxngErrElemwrong,
                            now.name().as_deref()
                        );
                    }
                }
                _ => {}
            }
            if ret < 0 {
                break;
            }
            // Switch to next element
            cur = now.next.map(|node| XmlNodePtr::try_from(node).unwrap());
        }
        ret = exec.push_string(None, null_mut());
        if ret == 1 {
            ret = 0;
            (*(*ctxt).state).seq = None;
        } else if ret == 0 {
            // TODO: get some of the names needed to exit the current state of exec
            VALID_ERR2!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrNoelem, Some(""));
            ret = -1;
            if (*ctxt).flags & FLAGS_IGNORABLE == 0 {
                xml_relaxng_dump_valid_error(ctxt);
            }
        } else {
            ret = -1;
        }

        // There might be content model errors outside of the pure
        // regexp validation, e.g. for attribute values.
        if ret == 0 && (*ctxt).perr != 0 {
            ret = (*ctxt).perr;
        }
        (*ctxt).perr = oldperr;
        ret
    }
}

/// Validate the end of the element, implements check that
/// there is nothing left not consumed in the element content or in the attribute list.
///
/// Returns 0 if the validation succeeded or an error code.
#[doc(alias = "xmlRelaxNGValidateElementEnd")]
unsafe fn xml_relaxng_validate_element_end(ctxt: XmlRelaxNGValidCtxtPtr, dolog: i32) -> i32 {
    unsafe {
        let state: XmlRelaxNGValidStatePtr = (*ctxt).state;
        if let Some(seq) = (*state).seq.map(|seq| XmlNodePtr::try_from(seq).unwrap()) {
            let next = xml_relaxng_skip_ignored(ctxt, Some(seq));
            (*state).seq = next.map(|next| next.into());
            if let Some(seq) = next {
                if dolog != 0 {
                    VALID_ERR3!(
                        ctxt,
                        XmlRelaxNGValidErr::XmlRelaxngErrExtracontent,
                        (*state).node.unwrap().name().as_deref(),
                        seq.name().as_deref()
                    );
                }
                return -1;
            }
        }
        for (i, &attr) in (*state).attrs.iter().enumerate() {
            if let Some(attr) = attr {
                if dolog != 0 {
                    VALID_ERR3!(
                        ctxt,
                        XmlRelaxNGValidErr::XmlRelaxngErrInvalidattr,
                        Some(&attr.name),
                        (*state).node.unwrap().name().as_deref()
                    );
                }
                return -1 - i as i32;
            }
        }
        0
    }
}

/// Find the "best" state in the (*ctxt).states list of states to report
/// errors about. I.e. a state with no element left in the child list
/// or the one with the less attributes left.
/// This is called only if a validation error was detected
///
/// Returns the index of the "best" state or -1 in case of error
#[doc(alias = "xmlRelaxNGBestState")]
unsafe fn xml_relaxng_best_state(ctxt: XmlRelaxNGValidCtxtPtr) -> i32 {
    unsafe {
        let mut best: i32 = -1;
        let mut value: i32 = 1000000;

        if ctxt.is_null() || (*ctxt).states.is_null() || (*(*ctxt).states).tab_state.is_empty() {
            return -1;
        }

        for (i, &state) in (*(*ctxt).states).tab_state.iter().enumerate() {
            if state.is_null() {
                continue;
            }
            if (*state).seq.is_some() {
                if best == -1 || value > 100000 {
                    value = 100000;
                    best = i as i32;
                }
            } else {
                let tmp = (*state).nb_attr_left;
                if best == -1 || value > tmp {
                    value = tmp;
                    best = i as i32;
                }
            }
        }
        best
    }
}

/// Find the "best" state in the (*ctxt).states list of states to report
/// errors about and log it.
#[doc(alias = "xmlRelaxNGLogBestError")]
unsafe fn xml_relaxng_log_best_error(ctxt: XmlRelaxNGValidCtxtPtr) {
    unsafe {
        if ctxt.is_null() || (*ctxt).states.is_null() || (*(*ctxt).states).tab_state.is_empty() {
            return;
        }

        let best: i32 = xml_relaxng_best_state(ctxt);
        if best >= 0 && best < (*(*ctxt).states).tab_state.len() as i32 {
            (*ctxt).state = (*(*ctxt).states).tab_state[best as usize];

            xml_relaxng_validate_element_end(ctxt, 1);
        }
    }
}

/// Validate the given node content against the (list) of definitions
///
/// Returns 0 if the validation succeeded or an error code.
#[doc(alias = "xmlRelaxNGValidateDefinitionList")]
unsafe fn xml_relaxng_validate_definition_list(
    ctxt: XmlRelaxNGValidCtxtPtr,
    mut defines: XmlRelaxNGDefinePtr,
) -> i32 {
    unsafe {
        let mut ret: i32 = 0;
        let mut res: i32;

        if defines.is_null() {
            VALID_ERR2!(
                ctxt,
                XmlRelaxNGValidErr::XmlRelaxngErrInternal,
                Some("NULL definition list")
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
                // continues on -2
                break;
            }
            defines = (*defines).next;
        }

        ret
    }
}

/// Copy the validation state
///
/// Returns the newly allocated structure or NULL in case or error
#[doc(alias = "xmlRelaxNGCopyValidState")]
unsafe fn xml_relaxng_copy_valid_state(
    ctxt: XmlRelaxNGValidCtxtPtr,
    state: XmlRelaxNGValidStatePtr,
) -> XmlRelaxNGValidStatePtr {
    unsafe {
        let ret: XmlRelaxNGValidStatePtr;

        if state.is_null() {
            return null_mut();
        }
        if !(*ctxt).free_state.is_null() && !(*(*ctxt).free_state).tab_state.is_empty() {
            ret = (*(*ctxt).free_state).tab_state.pop().unwrap();
        } else {
            ret = xml_malloc(size_of::<XmlRelaxNGValidState>()) as _;
            if ret.is_null() {
                xml_rng_verr_memory(ctxt, "allocating states\n");
                return null_mut();
            }
            std::ptr::write(&mut *ret, XmlRelaxNGValidState::default());
        }
        let attrs = take(&mut (*ret).attrs);
        memcpy(ret as _, state as _, size_of::<XmlRelaxNGValidState>());
        std::ptr::write(&mut (*ret).attrs, attrs);
        (*ret).attrs.clear();
        (*ret).attrs.extend((*state).attrs.iter().cloned());
        ret
    }
}

/// Compare the validation states for equality
///
/// Returns 1 if equal, 0 otherwise
#[doc(alias = "xmlRelaxNGEqualValidState")]
unsafe fn xml_relaxng_equal_valid_state(
    _ctxt: XmlRelaxNGValidCtxtPtr,
    state1: XmlRelaxNGValidStatePtr,
    state2: XmlRelaxNGValidStatePtr,
) -> i32 {
    unsafe {
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
        if (*state1).attrs.len() != (*state2).attrs.len() {
            return 0;
        }
        if (*state1).endvalue != (*state2).endvalue {
            return 0;
        }
        if (*state1).value != (*state2).value && !xml_str_equal((*state1).value, (*state2).value) {
            return 0;
        }
        (*state1)
            .attrs
            .iter()
            .zip((*state2).attrs.iter())
            .all(|(a1, a2)| a1 == a2) as i32
    }
}

/// Add a RelaxNG validation state to the container
///
/// Return 1 in case of success and 0 if this is a duplicate and -1 on error
#[doc(alias = "xmlRelaxNGAddState")]
unsafe fn xml_relaxng_add_states(
    ctxt: XmlRelaxNGValidCtxtPtr,
    states: XmlRelaxNGStatesPtr,
    state: XmlRelaxNGValidStatePtr,
) -> i32 {
    unsafe {
        if state.is_null() || states.is_null() {
            return -1;
        }
        for &state2 in &(*states).tab_state {
            if xml_relaxng_equal_valid_state(ctxt, state, state2) != 0 {
                xml_relaxng_free_valid_state(ctxt, state);
                return 0;
            }
        }
        (*states).tab_state.push(state);
        1
    }
}

/// Check if a node can be matched by one of the definitions
///
/// Returns 1 if matches 0 otherwise
#[doc(alias = "xmlRelaxNGNodeMatchesList")]
unsafe fn xml_relaxng_node_matches_list(node: XmlNodePtr, list: *mut XmlRelaxNGDefinePtr) -> i32 {
    unsafe {
        let mut cur: XmlRelaxNGDefinePtr;
        let mut i: i32 = 0;
        let mut tmp: i32;

        if list.is_null() {
            return 0;
        }

        cur = *list.add(i as usize);
        i += 1;
        while !cur.is_null() {
            if node.element_type() == XmlElementType::XmlElementNode
                && (*cur).typ == XmlRelaxNGType::Element
            {
                tmp = xml_relaxng_element_match(null_mut(), cur, node);
                if tmp == 1 {
                    return 1;
                }
            } else if matches!(
                node.element_type(),
                XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
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
}

/// Validate an interleave definition for a node.
///
/// Returns 0 if the validation succeeded or an error code.
#[doc(alias = "xmlRelaxNGValidateInterleave")]
unsafe fn xml_relaxng_validate_interleave(
    ctxt: XmlRelaxNGValidCtxtPtr,
    define: XmlRelaxNGDefinePtr,
) -> i32 {
    unsafe {
        let mut ret: i32 = 0;
        let mut i: i32;
        let nbgroups: i32;
        let err_nr = (*ctxt).err_tab.len();
        let mut oldstate: XmlRelaxNGValidStatePtr;
        let partitions: XmlRelaxNGPartitionPtr;
        let mut group: XmlRelaxNGInterleaveGroupPtr;

        if !(*define).data.is_null() {
            partitions = (*define).data as _;
            nbgroups = (*partitions).nbgroups;
        } else {
            VALID_ERR!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrInternodata);
            return -1;
        }
        // Optimizations for MIXED
        let oldflags: i32 = (*ctxt).flags;
        if (*define).dflags & IS_MIXED as i16 != 0 {
            (*ctxt).flags |= FLAGS_MIXED_CONTENT;
            if nbgroups == 2 {
                // this is a pure <mixed> case
                if !(*ctxt).state.is_null() {
                    (*(*ctxt).state).seq = xml_relaxng_skip_ignored(
                        ctxt,
                        (*(*ctxt).state)
                            .seq
                            .map(|seq| XmlNodePtr::try_from(seq).unwrap()),
                    )
                    .map(|node| node.into());
                }
                if (*(*(*(*partitions).groups.add(0))).rule).typ == XmlRelaxNGType::Text {
                    ret = xml_relaxng_validate_definition(
                        ctxt,
                        (*(*(*partitions).groups.add(1))).rule,
                    );
                } else {
                    ret = xml_relaxng_validate_definition(
                        ctxt,
                        (*(*(*partitions).groups.add(0))).rule,
                    );
                }
                if ret == 0 && !(*ctxt).state.is_null() {
                    (*(*ctxt).state).seq = xml_relaxng_skip_ignored(
                        ctxt,
                        (*(*ctxt).state)
                            .seq
                            .map(|seq| XmlNodePtr::try_from(seq).unwrap()),
                    )
                    .map(|node| node.into());
                }
                (*ctxt).flags = oldflags;
                return ret;
            }
        }

        // Build arrays to store the first and last node of the chain pertaining to each group
        let mut list = vec![None; nbgroups as usize];
        let mut lasts: Vec<Option<XmlNodePtr>> = vec![None; nbgroups as usize];

        // Walk the sequence of children finding the right group and sorting them in sequences.
        let mut cur = xml_relaxng_skip_ignored(
            ctxt,
            (*(*ctxt).state)
                .seq
                .map(|seq| XmlNodePtr::try_from(seq).unwrap()),
        );
        let start = cur;
        let mut lastchg = None;
        while let Some(cur_node) = cur {
            (*(*ctxt).state).seq = Some(cur_node.into());
            if let Some(triage) = (*partitions)
                .triage
                .filter(|_| (*partitions).flags & IS_DETERMINIST != 0)
            {
                let mut tmp = None;

                if matches!(
                    cur_node.element_type(),
                    XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
                ) {
                    tmp = triage.lookup2("#text", None).copied();
                } else if cur_node.element_type() == XmlElementType::XmlElementNode {
                    if let Some(ns) = cur_node.ns {
                        tmp = triage
                            .lookup2(&cur_node.name().unwrap(), ns.href().as_deref())
                            .or_else(|| triage.lookup2("#any", ns.href().as_deref()))
                            .copied();
                    } else {
                        tmp = triage.lookup2(&cur_node.name().unwrap(), None).copied();
                    }
                    if tmp.is_none() {
                        tmp = triage.lookup2("#any", None).copied();
                    }
                }

                if let Some(t) = tmp {
                    i = t - 1;
                    if (*partitions).flags & IS_NEEDCHECK != 0 {
                        group = *(*partitions).groups.add(i as usize);
                        if xml_relaxng_node_matches_list(cur_node, (*group).defs) == 0 {
                            i = nbgroups;
                        }
                    }
                } else {
                    i = nbgroups;
                }
            } else {
                i = 0;
                'main: while i < nbgroups {
                    'to_continue: {
                        group = *(*partitions).groups.add(i as usize);
                        if group.is_null() {
                            break 'to_continue;
                        }
                        if xml_relaxng_node_matches_list(cur_node, (*group).defs) != 0 {
                            break 'main;
                        }
                    }
                    i += 1;
                }
            }
            // We break as soon as an element not matched is found
            if i >= nbgroups {
                break;
            }
            if let Some(mut last) = lasts[i as usize].take() {
                last.next = Some(cur_node.into());
                lasts[i as usize] = Some(cur_node);
            } else {
                list[i as usize] = Some(cur_node);
                lasts[i as usize] = Some(cur_node);
            }
            if let Some(next) = cur_node.next {
                lastchg = Some(next);
            } else {
                lastchg = Some(cur_node.into());
            }
            cur = xml_relaxng_skip_ignored(
                ctxt,
                cur_node
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap()),
            );
        }
        'done: {
            if ret != 0 {
                VALID_ERR!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrInterseq);
                ret = -1;
                break 'done;
            }

            let lastelem = cur;
            let mut last = None;
            oldstate = (*ctxt).state;
            for i in 0..nbgroups {
                (*ctxt).state = xml_relaxng_copy_valid_state(ctxt, oldstate);
                if (*ctxt).state.is_null() {
                    ret = -1;
                    break;
                }
                group = *(*partitions).groups.add(i as usize);
                if let Some(mut l) = lasts[i as usize] {
                    last = l.next.take();
                }
                (*(*ctxt).state).seq = list[i as usize].map(|node| node.into());
                ret = xml_relaxng_validate_definition(ctxt, (*group).rule);
                if ret != 0 {
                    break;
                }
                if !(*ctxt).state.is_null() {
                    let cur = xml_relaxng_skip_ignored(
                        ctxt,
                        (*(*ctxt).state)
                            .seq
                            .map(|seq| XmlNodePtr::try_from(seq).unwrap()),
                    );
                    xml_relaxng_free_valid_state(ctxt, oldstate);
                    oldstate = (*ctxt).state;
                    (*ctxt).state = null_mut();
                    // there's a nasty violation of context-free unambiguities,
                    // since in open-name-class context, interleave in the
                    // production shall finish without caring about anything
                    // else that is OK to follow in that case -- it would
                    // otherwise get marked as "extra content" and would
                    // hence fail the validation, hence this perhaps
                    // dirty attempt to rectify such a situation
                    if let Some(cur) = cur.filter(|_| {
                        (*(*define).parent).typ != XmlRelaxNGType::Def
                            || !xml_str_equal(
                                (*(*define).parent).name,
                                c"open-name-class".as_ptr() as _,
                            )
                    }) {
                        VALID_ERR2!(
                            ctxt,
                            XmlRelaxNGValidErr::XmlRelaxngErrInterextra,
                            cur.name().as_deref()
                        );
                        ret = -1;
                        (*ctxt).state = oldstate;
                        break 'done;
                    }
                } else if !(*ctxt).states.is_null() {
                    let mut found: i32 = 0;
                    let mut best = usize::MAX;
                    let mut lowattr: i32 = -1;

                    // PBM: what happen if there is attributes checks in the interleaves
                    for (j, &state) in (*(*ctxt).states).tab_state.iter().enumerate() {
                        cur = xml_relaxng_skip_ignored(
                            ctxt,
                            (*state).seq.map(|seq| XmlNodePtr::try_from(seq).unwrap()),
                        );
                        if cur.is_none() {
                            if found == 0 {
                                lowattr = (*state).nb_attr_left;
                                best = j;
                            }
                            found = 1;
                            if (*state).nb_attr_left <= lowattr {
                                // try to keep the latest one to mach old heuristic
                                lowattr = (*state).nb_attr_left;
                                best = j;
                            }
                            if lowattr == 0 {
                                break;
                            }
                        } else if found == 0 {
                            if lowattr == -1 {
                                lowattr = (*state).nb_attr_left;
                                best = j;
                            } else if (*state).nb_attr_left <= lowattr {
                                // try to keep the latest one to mach old heuristic
                                lowattr = (*state).nb_attr_left;
                                best = j;
                            }
                        }
                    }
                    // BIG PBM: here we pick only one restarting point :-(
                    if !(*(*ctxt).states).tab_state.is_empty() {
                        xml_relaxng_free_valid_state(ctxt, oldstate);
                        if best != usize::MAX {
                            oldstate = (*(*ctxt).states).tab_state[best];
                            (*(*ctxt).states).tab_state[best] = null_mut();
                        } else {
                            oldstate = (*(*ctxt).states).tab_state.pop().unwrap();
                        }
                    }
                    for &state in &(*(*ctxt).states).tab_state {
                        xml_relaxng_free_valid_state(ctxt, state);
                    }
                    xml_relaxng_free_states(ctxt, (*ctxt).states);
                    (*ctxt).states = null_mut();
                    if found == 0 {
                        if let Some(cur) = cur {
                            VALID_ERR2!(
                                ctxt,
                                XmlRelaxNGValidErr::XmlRelaxngErrInterextra,
                                cur.name().as_deref()
                            );
                        } else {
                            VALID_ERR2!(
                                ctxt,
                                XmlRelaxNGValidErr::XmlRelaxngErrInterextra,
                                Some("noname")
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
                if let Some(mut l) = lasts[i as usize] {
                    l.next = last;
                }
            }
            if !(*ctxt).state.is_null() {
                xml_relaxng_free_valid_state(ctxt, (*ctxt).state);
            }
            (*ctxt).state = oldstate;
            (*(*ctxt).state).seq = lastelem.map(|node| node.into());
            if ret != 0 {
                VALID_ERR!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrInterseq);
                ret = -1;
                break 'done;
            }
        }

        //   done:
        (*ctxt).flags = oldflags;
        // builds the next links chain from the prev one
        let mut cur = lastchg;
        while let Some(cur_node) = cur.map(|node| XmlNodePtr::try_from(node).unwrap()) {
            if Some(cur_node) == start {
                break;
            }
            let Some(mut prev) = cur_node
                .prev
                .map(|prev| XmlNodePtr::try_from(prev).unwrap())
            else {
                break;
            };
            prev.next = Some(cur_node.into());
            cur = Some(prev.into());
        }
        if ret == 0 && (*ctxt).err_tab.len() > err_nr {
            xml_relaxng_pop_errors(ctxt, err_nr as i32);
        }

        ret
    }
}

/// Validate the current state against the definition
///
/// Returns 0 if the validation succeeded or an error code.
#[doc(alias = "xmlRelaxNGValidateState")]
unsafe fn xml_relaxng_validate_state(
    ctxt: XmlRelaxNGValidCtxtPtr,
    define: XmlRelaxNGDefinePtr,
) -> i32 {
    unsafe {
        let mut ret: i32 = 0;
        let mut tmp: i32;
        let oldflags: i32;
        let mut oldstate: XmlRelaxNGValidStatePtr = null_mut();
        let mut state: XmlRelaxNGValidStatePtr;

        if define.is_null() {
            VALID_ERR!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrNodefine);
            return -1;
        }

        let mut node = if !(*ctxt).state.is_null() {
            (*(*ctxt).state)
                .seq
                .map(|seq| XmlNodePtr::try_from(seq).unwrap())
        } else {
            None
        };
        (*ctxt).depth += 1;
        match (*define).typ {
            XmlRelaxNGType::Empty => {
                ret = 0;
            }
            XmlRelaxNGType::NotAllowed => {
                ret = -1;
            }
            XmlRelaxNGType::Text => {
                while let Some(now) = node.filter(|node| {
                    matches!(
                        node.element_type(),
                        XmlElementType::XmlTextNode
                            | XmlElementType::XmlCommentNode
                            | XmlElementType::XmlPINode
                            | XmlElementType::XmlCDATASectionNode
                    )
                }) {
                    node = now.next().map(|n| XmlNodePtr::try_from(n).unwrap());
                }
                (*(*ctxt).state).seq = node.map(|node| node.into());
            }
            XmlRelaxNGType::Element => 'to_break: {
                let err_nr = (*ctxt).err_tab.len();
                node = xml_relaxng_skip_ignored(ctxt, node);
                let Some(mut node) = node else {
                    VALID_ERR2!(
                        ctxt,
                        XmlRelaxNGValidErr::XmlRelaxngErrNoelem,
                        (!(*define).name.is_null())
                            .then(|| CStr::from_ptr((*define).name as *const i8).to_string_lossy())
                            .as_deref()
                    );
                    ret = -1;
                    if (*ctxt).flags & FLAGS_IGNORABLE == 0 {
                        xml_relaxng_dump_valid_error(ctxt);
                    }
                    break 'to_break;
                };
                if node.element_type() != XmlElementType::XmlElementNode {
                    VALID_ERR!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrNotelem);
                    ret = -1;
                    if (*ctxt).flags & FLAGS_IGNORABLE == 0 {
                        xml_relaxng_dump_valid_error(ctxt);
                    }
                    break 'to_break;
                }
                // This node was already validated successfully against this definition.
                if node.psvi == define as _ {
                    (*(*ctxt).state).seq = xml_relaxng_skip_ignored(
                        ctxt,
                        node.next().map(|n| XmlNodePtr::try_from(n).unwrap()),
                    )
                    .map(|node| node.into());
                    if (*ctxt).err_tab.len() > err_nr {
                        xml_relaxng_pop_errors(ctxt, err_nr as i32);
                    }
                    if !(*ctxt).err_tab.is_empty() {
                        while (*ctxt).err_tab.last().is_some_and(|err| {
                            (err.err == XmlRelaxNGValidErr::XmlRelaxngErrElemname
                                && err.arg2.as_deref() == node.name().as_deref())
                                || (err.err == XmlRelaxNGValidErr::XmlRelaxngErrElemextrans
                                    && err.arg1.as_deref() == node.name().as_deref())
                                || err.err == XmlRelaxNGValidErr::XmlRelaxngErrNoelem
                                || err.err == XmlRelaxNGValidErr::XmlRelaxngErrNotelem
                        }) {
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
                if !(*ctxt).err_tab.is_empty() {
                    if (*ctxt).err_tab.len() > err_nr {
                        xml_relaxng_pop_errors(ctxt, err_nr as i32);
                    }
                    while (*ctxt).err_tab.last().is_some_and(|err| {
                        (err.err == XmlRelaxNGValidErr::XmlRelaxngErrElemname
                            && err.arg2.as_deref() == node.name().as_deref())
                            || (err.err == XmlRelaxNGValidErr::XmlRelaxngErrElemextrans
                                && err.arg1.as_deref() == node.name().as_deref())
                            || err.err == XmlRelaxNGValidErr::XmlRelaxngErrNoelem
                            || err.err == XmlRelaxNGValidErr::XmlRelaxngErrNotelem
                    }) {
                        xml_relaxng_valid_error_pop(ctxt);
                    }
                }
                let err_nr = (*ctxt).err_tab.len();

                oldflags = (*ctxt).flags;
                if (*ctxt).flags & FLAGS_MIXED_CONTENT != 0 {
                    (*ctxt).flags -= FLAGS_MIXED_CONTENT;
                }
                state = xml_relaxng_new_valid_state(ctxt, Some(node.into()));
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
                            node.name().as_deref()
                        );
                    }
                }
                if let Some(cont_model) = (*define).cont_model.clone() {
                    let tmpstate: XmlRelaxNGValidStatePtr = (*ctxt).state;
                    let tmpstates: XmlRelaxNGStatesPtr = (*ctxt).states;

                    let nstate: XmlRelaxNGValidStatePtr =
                        xml_relaxng_new_valid_state(ctxt, Some(node.into()));
                    (*ctxt).state = nstate;
                    (*ctxt).states = null_mut();

                    tmp = xml_relaxng_validate_compiled_content(
                        ctxt,
                        cont_model,
                        (*(*ctxt).state)
                            .seq
                            .map(|seq| XmlNodePtr::try_from(seq).unwrap()),
                    );
                    let nseq = (*(*ctxt).state).seq;
                    (*ctxt).state = tmpstate;
                    (*ctxt).states = tmpstates;
                    xml_relaxng_free_valid_state(ctxt, nstate);

                    if tmp != 0 {
                        ret = -1;
                    }

                    if !(*ctxt).states.is_null() {
                        tmp = -1;

                        for &state in &(*(*ctxt).states).tab_state {
                            (*ctxt).state = state;
                            (*(*ctxt).state).seq = nseq;

                            if xml_relaxng_validate_element_end(ctxt, 0) == 0 {
                                tmp = 0;
                                break;
                            }
                        }
                        if tmp != 0 {
                            // validation error, log the message for the "best" one
                            (*ctxt).flags |= FLAGS_IGNORABLE;
                            xml_relaxng_log_best_error(ctxt);
                        }
                        for &state in &(*(*ctxt).states).tab_state {
                            xml_relaxng_free_valid_state(ctxt, state);
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
                                    node.name().as_deref()
                                );
                                (*ctxt).state = null_mut();
                            } else {
                                VALID_ERR2!(
                                    ctxt,
                                    XmlRelaxNGValidErr::XmlRelaxngErrContentvalid,
                                    node.name().as_deref()
                                );
                            }
                        }
                    }
                    if !(*ctxt).states.is_null() {
                        tmp = -1;

                        for &state in &(*(*ctxt).states).tab_state {
                            (*ctxt).state = state;

                            if xml_relaxng_validate_element_end(ctxt, 0) == 0 {
                                tmp = 0;
                                break;
                            }
                        }
                        if tmp != 0 {
                            // validation error, log the message for the "best" one
                            (*ctxt).flags |= FLAGS_IGNORABLE;
                            xml_relaxng_log_best_error(ctxt);
                        }
                        for state in (*(*ctxt).states).tab_state.iter_mut() {
                            xml_relaxng_free_valid_state(ctxt, *state);
                            *state = null_mut();
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
                    node.psvi = define as _;
                }
                (*ctxt).flags = oldflags;
                (*ctxt).state = oldstate;
                if !oldstate.is_null() {
                    (*oldstate).seq = xml_relaxng_skip_ignored(
                        ctxt,
                        node.next.map(|node| XmlNodePtr::try_from(node).unwrap()),
                    )
                    .map(|node| node.into());
                }
                if ret != 0 {
                    if (*ctxt).flags & FLAGS_IGNORABLE == 0 {
                        xml_relaxng_dump_valid_error(ctxt);
                        ret = 0;
                    }
                } else if (*ctxt).err_tab.len() > err_nr {
                    xml_relaxng_pop_errors(ctxt, err_nr as i32);
                }
            }
            XmlRelaxNGType::Optional => 'to_break: {
                let err_nr = (*ctxt).err_tab.len();
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
                    if (*ctxt).err_tab.len() > err_nr {
                        xml_relaxng_pop_errors(ctxt, err_nr as i32);
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
                        if (*ctxt).err_tab.len() > err_nr {
                            xml_relaxng_pop_errors(ctxt, err_nr as i32);
                        }
                        break 'to_break;
                    }
                    xml_relaxng_add_states(ctxt, (*ctxt).states, oldstate);
                    xml_relaxng_add_states(ctxt, (*ctxt).states, (*ctxt).state);
                    (*ctxt).state = null_mut();
                }
                (*ctxt).flags = oldflags;
                ret = 0;
                if (*ctxt).err_tab.len() > err_nr {
                    xml_relaxng_pop_errors(ctxt, err_nr as i32);
                }
            }
            ty @ XmlRelaxNGType::Oneormore | ty @ XmlRelaxNGType::Zeroormore => 'to_break: {
                if matches!(ty, XmlRelaxNGType::Oneormore) {
                    let err_nr = (*ctxt).err_tab.len();
                    ret = xml_relaxng_validate_definition_list(ctxt, (*define).content);
                    if ret != 0 {
                        break 'to_break;
                    }
                    if (*ctxt).err_tab.len() > err_nr {
                        xml_relaxng_pop_errors(ctxt, err_nr as i32);
                    }
                }

                let mut progress: i32;
                let mut states: XmlRelaxNGStatesPtr = null_mut();

                // err_nr = (*ctxt).err_nr;
                let res: XmlRelaxNGStatesPtr = xml_relaxng_new_states(ctxt, 1);
                if res.is_null() {
                    ret = -1;
                    break 'to_break;
                }
                // All the input states are also exit states
                if !(*ctxt).state.is_null() {
                    xml_relaxng_add_states(
                        ctxt,
                        res,
                        xml_relaxng_copy_valid_state(ctxt, (*ctxt).state),
                    );
                } else {
                    for &state in &(*(*ctxt).states).tab_state {
                        xml_relaxng_add_states(
                            ctxt,
                            res,
                            xml_relaxng_copy_valid_state(ctxt, state),
                        );
                    }
                }
                oldflags = (*ctxt).flags;
                (*ctxt).flags |= FLAGS_IGNORABLE;
                'lp: while {
                    progress = 0;
                    let mut base = (*res).tab_state.len();

                    if !(*ctxt).states.is_null() {
                        states = (*ctxt).states;
                        for &state in &(*states).tab_state {
                            (*ctxt).state = state;
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
                                    for &in_state in &(*(*ctxt).states).tab_state {
                                        tmp = xml_relaxng_add_states(ctxt, res, in_state);
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
                            base = (*res).tab_state.len();
                            if !(*ctxt).state.is_null() {
                                tmp = xml_relaxng_add_states(ctxt, res, (*ctxt).state);
                                (*ctxt).state = null_mut();
                                if tmp == 1 {
                                    progress = 1;
                                }
                            } else if !(*ctxt).states.is_null() {
                                for &state in &(*(*ctxt).states).tab_state {
                                    tmp = xml_relaxng_add_states(ctxt, res, state);
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
                        // Collect all the new nodes added at that step
                        // and make them the new node set
                        if (*res).tab_state.len() - base == 1 {
                            (*ctxt).state =
                                xml_relaxng_copy_valid_state(ctxt, (*res).tab_state[base]);
                        } else {
                            if states.is_null() {
                                xml_relaxng_new_states(
                                    ctxt,
                                    (*res).tab_state.len() as i32 - base as i32,
                                );
                                states = (*ctxt).states;
                                if states.is_null() {
                                    // progress = 0;
                                    break 'lp;
                                }
                            }
                            (*states).tab_state.clear();
                            for &state in &(*res).tab_state[base..] {
                                xml_relaxng_add_states(
                                    ctxt,
                                    states,
                                    xml_relaxng_copy_valid_state(ctxt, state),
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
                // /*
                //  * errors may have to be propagated back...
                //  */
                // if (*ctxt).errNr > errNr {
                //     xmlRelaxNGPopErrors(ctxt, errNr);
                // }
                // #endif
                ret = 0;
            }
            XmlRelaxNGType::Choice => 'to_break: {
                let mut list: XmlRelaxNGDefinePtr = null_mut();
                let mut states: XmlRelaxNGStatesPtr = null_mut();

                node = xml_relaxng_skip_ignored(ctxt, node);

                let err_nr = (*ctxt).err_tab.len();
                if (*define).dflags & IS_TRIABLE as i16 != 0 && !(*define).data.is_null() {
                    if let Some(node) = node {
                        // node.is_null() can't be optimized since IS_TRIABLE
                        // doesn't account for choice which may lead to
                        // only attributes.
                        let triage: XmlHashTablePtr = (*define).data as _;

                        // Something we can optimize cleanly there is only one
                        // possible branch out !
                        if matches!(
                            node.element_type(),
                            XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
                        ) {
                            list =
                                xml_hash_lookup2(triage, c"#text".as_ptr() as _, null_mut()) as _;
                        } else if node.element_type() == XmlElementType::XmlElementNode {
                            let nodename = CString::new(node.name.as_ref()).unwrap();
                            if let Some(ns) = node.ns {
                                let href =
                                    ns.href.as_deref().map(|href| CString::new(href).unwrap());
                                list = xml_hash_lookup2(
                                    triage,
                                    nodename.as_ptr() as *const u8,
                                    href.as_deref()
                                        .map_or(null_mut(), |href| href.as_ptr() as *const u8),
                                ) as _;
                                if list.is_null() {
                                    list = xml_hash_lookup2(
                                        triage,
                                        c"#any".as_ptr() as _,
                                        href.as_deref()
                                            .map_or(null_mut(), |href| href.as_ptr() as *const u8),
                                    ) as _;
                                }
                            } else {
                                list = xml_hash_lookup2(
                                    triage,
                                    nodename.as_ptr() as *const u8,
                                    null_mut(),
                                ) as _;
                            }
                            if list.is_null() {
                                list = xml_hash_lookup2(triage, c"#any".as_ptr() as _, null_mut())
                                    as _;
                            }
                        }
                        if list.is_null() {
                            ret = -1;
                            VALID_ERR2!(
                                ctxt,
                                XmlRelaxNGValidErr::XmlRelaxngErrElemwrong,
                                node.name().as_deref()
                            );
                            break 'to_break;
                        }
                        ret = xml_relaxng_validate_definition(ctxt, list);
                        // Is This correct ??????
                        // if ret == 0 {}
                        break 'to_break;
                    }
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
                            for &state in &(*(*ctxt).states).tab_state {
                                xml_relaxng_add_states(ctxt, states, state);
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
                } else if (*ctxt).err_tab.len() > err_nr {
                    xml_relaxng_pop_errors(ctxt, err_nr as i32);
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
                let mut content: *mut XmlChar = null_mut();
                let mut child = node;
                while let Some(cur_node) = child {
                    if cur_node.element_type() == XmlElementType::XmlElementNode {
                        VALID_ERR2!(
                            ctxt,
                            XmlRelaxNGValidErr::XmlRelaxngErrDataelem,
                            node.unwrap().parent().unwrap().name().as_deref()
                        );
                        ret = -1;
                        break;
                    } else if matches!(
                        cur_node.element_type(),
                        XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
                    ) {
                        let cont = cur_node.content.as_deref().unwrap();
                        content = xml_strncat(content, cont.as_ptr(), cont.len() as i32);
                    }
                    // TODO: handle entities ...
                    child = cur_node
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
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
                        xml_rng_verr_memory(ctxt, "validating\n");
                        ret = -1;
                        break 'to_break;
                    }
                }
                ret = xml_relaxng_validate_datatype(ctxt, content, define, (*(*ctxt).state).seq);
                if ret == -1 {
                    VALID_ERR2!(
                        ctxt,
                        XmlRelaxNGValidErr::XmlRelaxngErrDatatype,
                        Some(
                            CStr::from_ptr((*define).name as *const i8)
                                .to_string_lossy()
                                .as_ref()
                        )
                    );
                } else if ret == 0 {
                    (*(*ctxt).state).seq = None;
                }
                if !content.is_null() {
                    xml_free(content as _);
                }
            }
            XmlRelaxNGType::Value => 'to_break: {
                let mut content: *mut XmlChar = null_mut();
                let mut child = node;
                while let Some(cur_node) = child {
                    if cur_node.element_type() == XmlElementType::XmlElementNode {
                        VALID_ERR2!(
                            ctxt,
                            XmlRelaxNGValidErr::XmlRelaxngErrValelem,
                            node.unwrap().parent().unwrap().name().as_deref()
                        );
                        ret = -1;
                        break;
                    } else if matches!(
                        cur_node.element_type(),
                        XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
                    ) {
                        let cont = cur_node.content.as_deref().unwrap();
                        content = xml_strncat(content, cont.as_ptr(), cont.len() as i32);
                    }
                    // TODO: handle entities ...
                    child = cur_node
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
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
                        xml_rng_verr_memory(ctxt, "validating\n");
                        ret = -1;
                        break 'to_break;
                    }
                }
                let oldvalue: *mut XmlChar = (*(*ctxt).state).value;
                (*(*ctxt).state).value = content;
                ret = xml_relaxng_validate_value(ctxt, define);
                (*(*ctxt).state).value = oldvalue;
                if ret == -1 {
                    VALID_ERR2!(
                        ctxt,
                        XmlRelaxNGValidErr::XmlRelaxngErrValue,
                        (!(*define).name.is_null())
                            .then(|| CStr::from_ptr((*define).name as *const i8).to_string_lossy())
                            .as_deref()
                    );
                } else if ret == 0 {
                    (*(*ctxt).state).seq = None;
                }
                if !content.is_null() {
                    xml_free(content as _);
                }
            }
            XmlRelaxNGType::List => 'to_break: {
                // Make sure it's only text nodes
                let mut content = null_mut();
                let mut child = node;
                while let Some(cur_node) = child {
                    if cur_node.element_type() == XmlElementType::XmlElementNode {
                        VALID_ERR2!(
                            ctxt,
                            XmlRelaxNGValidErr::XmlRelaxngErrListelem,
                            node.unwrap().parent().unwrap().name().as_deref()
                        );
                        ret = -1;
                        break;
                    } else if matches!(
                        cur_node.element_type(),
                        XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
                    ) {
                        let cont = cur_node.content.as_deref().unwrap();
                        content = xml_strncat(content, cont.as_ptr(), cont.len() as i32);
                    }
                    // TODO: handle entities ...
                    child = cur_node
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
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
                        xml_rng_verr_memory(ctxt, "validating\n");
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
                } else if let Some(node) = node.filter(|_| ret == 0) {
                    (*(*ctxt).state).seq = node.next;
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
}

/// Validate the current node lists against the definition
///
/// Returns 0 if the validation succeeded or an error code.
#[doc(alias = "xmlRelaxNGValidateDefinition")]
unsafe fn xml_relaxng_validate_definition(
    ctxt: XmlRelaxNGValidCtxtPtr,
    define: XmlRelaxNGDefinePtr,
) -> i32 {
    unsafe {
        let mut res: XmlRelaxNGStatesPtr;
        let mut ret: i32;

        // We should NOT have both (*ctxt).state and (*ctxt).states
        if !(*ctxt).state.is_null() && !(*ctxt).states.is_null() {
            // TODO
            xml_relaxng_free_valid_state(ctxt, (*ctxt).state);
            (*ctxt).state = null_mut();
        }

        if (*ctxt).states.is_null() || (*(*ctxt).states).tab_state.len() == 1 {
            if !(*ctxt).states.is_null() {
                (*ctxt).state = (*(*ctxt).states).tab_state[0];
                xml_relaxng_free_states(ctxt, (*ctxt).states);
                (*ctxt).states = null_mut();
            }
            let ret = xml_relaxng_validate_state(ctxt, define);
            if !(*ctxt).state.is_null() && !(*ctxt).states.is_null() {
                // TODO
                xml_relaxng_free_valid_state(ctxt, (*ctxt).state);
                (*ctxt).state = null_mut();
            }
            if !(*ctxt).states.is_null() && (*(*ctxt).states).tab_state.len() == 1 {
                (*ctxt).state = (*(*ctxt).states).tab_state[0];
                xml_relaxng_free_states(ctxt, (*ctxt).states);
                (*ctxt).states = null_mut();
            }
            return ret;
        }

        let states: XmlRelaxNGStatesPtr = (*ctxt).states;
        (*ctxt).states = null_mut();
        res = null_mut();
        let mut j = 0;
        let oldflags: i32 = (*ctxt).flags;
        (*ctxt).flags |= FLAGS_IGNORABLE;
        for i in 0..(*states).tab_state.len() {
            (*ctxt).state = (*states).tab_state[i];
            (*ctxt).states = null_mut();
            ret = xml_relaxng_validate_state(ctxt, define);
            // We should NOT have both (*ctxt).state and (*ctxt).states
            if !(*ctxt).state.is_null() && !(*ctxt).states.is_null() {
                // TODO
                xml_relaxng_free_valid_state(ctxt, (*ctxt).state);
                (*ctxt).state = null_mut();
            }
            if ret == 0 {
                if (*ctxt).states.is_null() {
                    if !res.is_null() {
                        // add the state to the container
                        xml_relaxng_add_states(ctxt, res, (*ctxt).state);
                        (*ctxt).state = null_mut();
                    } else {
                        // add the state directly in states
                        (*states).tab_state[j] = (*ctxt).state;
                        j += 1;
                        (*ctxt).state = null_mut();
                    }
                } else if res.is_null() {
                    // make it the new container and copy other results
                    res = (*ctxt).states;
                    (*ctxt).states = null_mut();
                    for k in 0..j {
                        xml_relaxng_add_states(ctxt, res, (*states).tab_state[k]);
                    }
                } else {
                    // add all the new results to res and reff the container
                    for k in 0..(*(*ctxt).states).tab_state.len() {
                        xml_relaxng_add_states(ctxt, res, (*(*ctxt).states).tab_state[k]);
                    }
                    xml_relaxng_free_states(ctxt, (*ctxt).states);
                    (*ctxt).states = null_mut();
                }
            } else if !(*ctxt).state.is_null() {
                xml_relaxng_free_valid_state(ctxt, (*ctxt).state);
                (*ctxt).state = null_mut();
            } else if !(*ctxt).states.is_null() {
                for k in 0..(*(*ctxt).states).tab_state.len() {
                    xml_relaxng_free_valid_state(ctxt, (*(*ctxt).states).tab_state[k]);
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
            (*states).tab_state.truncate(j);
            (*ctxt).states = states;
            ret = 0;
        } else if j == 1 {
            (*ctxt).state = (*states).tab_state[0];
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
}

/// Validate the given document
///
/// Returns 0 if the validation succeeded or an error code.
#[doc(alias = "xmlRelaxNGValidateDocument")]
unsafe fn xml_relaxng_validate_document(ctxt: XmlRelaxNGValidCtxtPtr, doc: XmlDocPtr) -> i32 {
    unsafe {
        let mut ret: i32;
        let mut state: XmlRelaxNGValidStatePtr;

        if ctxt.is_null() || (*ctxt).schema.is_null() {
            return -1;
        }

        (*ctxt).err_no = XmlRelaxNGValidErr::XmlRelaxngOk as i32;
        let schema: XmlRelaxNGPtr = (*ctxt).schema;
        let grammar: XmlRelaxNGGrammarPtr = (*schema).topgrammar;
        if grammar.is_null() {
            VALID_ERR!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrNogrammar);
            return -1;
        }
        state = xml_relaxng_new_valid_state(ctxt, None);
        (*ctxt).state = state;
        ret = xml_relaxng_validate_definition(ctxt, (*grammar).start);
        if !(*ctxt).state.is_null() && (*state).seq.is_some() {
            state = (*ctxt).state;
            let node = xml_relaxng_skip_ignored(
                ctxt,
                (*state).seq.map(|seq| XmlNodePtr::try_from(seq).unwrap()),
            );
            if node.is_some() && ret != -1 {
                VALID_ERR!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrExtradata);
                ret = -1;
            }
        } else if !(*ctxt).states.is_null() {
            let mut tmp: i32 = -1;

            for &state in &(*(*ctxt).states).tab_state {
                let node = xml_relaxng_skip_ignored(
                    ctxt,
                    (*state).seq.map(|seq| XmlNodePtr::try_from(seq).unwrap()),
                );
                if node.is_none() {
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
        #[cfg(feature = "libxml_valid")]
        if (*ctxt).idref == 1 {
            let mut vctxt = XmlValidCtxt {
                user_data: (*ctxt).user_data.clone(),
                error: (*ctxt).error,
                warning: (*ctxt).warning,
                valid: 1,
                ..Default::default()
            };
            vctxt.valid = 1;
            vctxt.error = (*ctxt).error;
            vctxt.warning = (*ctxt).warning;
            vctxt.user_data = (*ctxt).user_data.clone();

            if xml_validate_document_final(&mut vctxt, doc) != 1 {
                ret = -1;
            }
        }
        if ret == 0 && (*ctxt).err_no != XmlRelaxNGValidErr::XmlRelaxngOk as i32 {
            ret = -1;
        }

        ret
    }
}

/// Call this routine to speed up XPath computation on static documents.
/// This stamps all the element nodes with the document order
/// Like for line information, the order is kept in the element->content
/// field, the value stored is actually - the node number (starting at -1)
/// to be able to differentiate from line numbers.
///
/// Returns the number of elements found in the document or -1 in case of error.
#[doc(alias = "xmlRelaxNGCleanPSVI")]
#[cfg(feature = "schema")]
fn xml_relaxng_clean_psvi(node: XmlGenericNodePtr) {
    if !matches!(
        node.element_type(),
        XmlElementType::XmlElementNode
            | XmlElementType::XmlDocumentNode
            | XmlElementType::XmlHTMLDocumentNode
    ) {
        return;
    }
    if node.element_type() == XmlElementType::XmlElementNode {
        let mut node = XmlNodePtr::try_from(node).unwrap();
        node.psvi = null_mut();
    }

    let mut cur = node.children();
    while let Some(mut cur_node) = cur {
        if cur_node.element_type() == XmlElementType::XmlElementNode {
            let mut cur_node = XmlNodePtr::try_from(cur_node).unwrap();
            cur_node.psvi = null_mut();
            if let Some(children) = cur_node.children() {
                cur = Some(children);
                continue;
            }
        }
        if let Some(next) = cur_node.next() {
            cur = Some(next);
            continue;
        }
        cur = loop {
            let Some(parent) = cur_node.parent() else {
                break None;
            };
            cur_node = parent;
            if cur_node == node {
                break None;
            }
            if let Some(next) = cur_node.next() {
                break Some(next);
            }
        }
    }
}

/// Validate a document tree in memory.
///
/// Returns 0 if the document is valid, a positive error code
/// number otherwise and -1 in case of internal or API error.
#[doc(alias = "xmlRelaxNGValidateDoc")]
pub unsafe fn xml_relaxng_validate_doc(ctxt: XmlRelaxNGValidCtxtPtr, doc: XmlDocPtr) -> i32 {
    unsafe {
        if ctxt.is_null() {
            return -1;
        }

        (*ctxt).doc = Some(doc);

        let ret: i32 = xml_relaxng_validate_document(ctxt, doc);
        // Remove all left PSVI
        xml_relaxng_clean_psvi(doc.into());

        // TODO: build error codes
        if ret == -1 {
            return 1;
        }
        ret
    }
}

/// Handle the callback and if needed validate the element children.
/// some of the in/out information are passed via the context in @inputdata.
#[doc(alias = "xmlRelaxNGValidateProgressiveCallback")]
pub(crate) fn xml_relaxng_validate_progressive_callback(
    _exec: XmlRegExecCtxtPtr,
    token: &str,
    transdata: *mut c_void,
    inputdata: *mut c_void,
) {
    unsafe {
        let ctxt: XmlRelaxNGValidCtxtPtr = inputdata as _;
        let define: XmlRelaxNGDefinePtr = transdata as _;
        let mut ret: i32 = 0;
        let oldflags: i32;

        if ctxt.is_null() {
            eprintln!("callback on {token} missing context");
            return;
        }
        let node = (*ctxt).pnode.unwrap();
        (*ctxt).pstate = 1;
        if define.is_null() {
            if token.starts_with('#') {
                return;
            }
            eprintln!("callback on {token} missing define");
            if !ctxt.is_null() && (*ctxt).err_no == XmlRelaxNGValidErr::XmlRelaxngOk as i32 {
                (*ctxt).err_no = XmlRelaxNGValidErr::XmlRelaxngErrInternal as i32;
            }
            (*ctxt).pstate = -1;
            return;
        }
        if ctxt.is_null() || define.is_null() {
            eprintln!("callback on {token} missing info");
            if !ctxt.is_null() && (*ctxt).err_no == XmlRelaxNGValidErr::XmlRelaxngOk as i32 {
                (*ctxt).err_no = XmlRelaxNGValidErr::XmlRelaxngErrInternal as i32;
            }
            (*ctxt).pstate = -1;
            return;
        } else if (*define).typ != XmlRelaxNGType::Element {
            eprintln!("callback on {token} define is not element");
            if (*ctxt).err_no == XmlRelaxNGValidErr::XmlRelaxngOk as i32 {
                (*ctxt).err_no = XmlRelaxNGValidErr::XmlRelaxngErrInternal as i32;
            }
            (*ctxt).pstate = -1;
            return;
        }
        if !matches!(node.element_type(), XmlElementType::XmlElementNode) {
            VALID_ERR!(ctxt, XmlRelaxNGValidErr::XmlRelaxngErrNotelem);
            if (*ctxt).flags & FLAGS_IGNORABLE == 0 {
                xml_relaxng_dump_valid_error(ctxt);
            }
            (*ctxt).pstate = -1;
            return;
        }
        let Some(cont_model) = (*define).cont_model.clone() else {
            // this node cannot be validated in a streamable fashion
            (*ctxt).pstate = 0;
            (*ctxt).pdef = define;
            return;
        };
        let exec = XmlRegExecCtxt::new(
            cont_model,
            Some(xml_relaxng_validate_progressive_callback),
            ctxt as _,
        );
        (*ctxt).elem_push(exec);

        // Validate the attributes part of the content.
        let state = xml_relaxng_new_valid_state(ctxt, Some(node.into()));
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
                    node.name().as_deref()
                );
            }
        }
        if !(*ctxt).state.is_null() {
            (*(*ctxt).state).seq = None;
            ret = xml_relaxng_validate_element_end(ctxt, 1);
            if ret != 0 {
                (*ctxt).pstate = -1;
            }
            xml_relaxng_free_valid_state(ctxt, (*ctxt).state);
        } else if !(*ctxt).states.is_null() {
            let mut tmp: i32 = -1;

            oldflags = (*ctxt).flags;

            for &state in &(*(*ctxt).states).tab_state {
                (*ctxt).state = state;
                (*(*ctxt).state).seq = None;

                if xml_relaxng_validate_element_end(ctxt, 0) == 0 {
                    tmp = 0;
                    break;
                }
            }
            if tmp != 0 {
                // validation error, log the message for the "best" one
                (*ctxt).flags |= FLAGS_IGNORABLE;
                xml_relaxng_log_best_error(ctxt);
            }
            for &state in &(*(*ctxt).states).tab_state {
                xml_relaxng_free_valid_state(ctxt, state);
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
}

/// Check the CData parsed for validation in the current stack
///
/// Returns 1 if no validation problem was found or -1 otherwise
#[doc(alias = "xmlRelaxNGValidatePushCData")]
pub unsafe fn xml_relaxng_validate_push_cdata(ctxt: XmlRelaxNGValidCtxtPtr, mut data: &str) -> i32 {
    unsafe {
        if ctxt.is_null() || (*ctxt).elem().is_none() {
            return -1;
        }

        data = data.trim_start_matches(|c: char| c.is_xml_blank_char());
        if data.is_empty() {
            return 1;
        }

        let ret: i32 = (*ctxt)
            .elem_mut()
            .unwrap()
            .push_string(Some("#text"), ctxt as _);
        if ret < 0 {
            VALID_ERR2!(
                ctxt,
                XmlRelaxNGValidErr::XmlRelaxngErrTextwrong,
                Some(" TODO ")
            );

            return -1;
        }
        1
    }
}

/// Validate a full subtree when xmlRelaxNGValidatePushElement() returned
/// 0 and the content of the node has been expanded.
///
/// Returns 1 if no validation problem was found or -1 in case of error.
#[doc(alias = "xmlRelaxNGValidateFullElement")]
pub unsafe fn xml_relaxng_validate_full_element(
    ctxt: XmlRelaxNGValidCtxtPtr,
    _doc: Option<XmlDocPtr>,
    elem: XmlNodePtr,
) -> i32 {
    unsafe {
        let mut ret: i32;

        if ctxt.is_null() || (*ctxt).pdef.is_null() {
            return -1;
        }
        let state: XmlRelaxNGValidStatePtr = xml_relaxng_new_valid_state(ctxt, elem.parent());
        if state.is_null() {
            return -1;
        }
        (*state).seq = Some(elem.into());
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
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_relaxng_dump() {
        #[cfg(all(feature = "schema", feature = "libxml_output"))]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_FILE_PTR {
                for n_schema in 0..GEN_NB_XML_RELAXNG_PTR {
                    let mem_base = xml_mem_blocks();
                    let mut output = gen_file_ptr(n_output, 0).unwrap();
                    let schema = gen_xml_relaxng_ptr(n_schema, 1);

                    xml_relaxng_dump(&mut output, schema);
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
        #[cfg(all(feature = "schema", feature = "libxml_output"))]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_FILE_PTR {
                for n_schema in 0..GEN_NB_XML_RELAXNG_PTR {
                    let mem_base = xml_mem_blocks();
                    let mut output = gen_file_ptr(n_output, 0).unwrap();
                    let schema = gen_xml_relaxng_ptr(n_schema, 1);

                    xml_relaxng_dump_tree(&mut output, schema);
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
}
