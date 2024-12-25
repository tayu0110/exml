//! Provide methods and data structures for handling DTD validation.  
//! This module is based on `libxml/valid.h`, `valid.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: The DTD validation
// Description: API for the DTD handling and the validity checking
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// valid.c : part of the code use to do the DTD handling and the validity checking
//
// See Copyright for the status of this software.
//
// daniel@veillard.com

use std::{
    ffi::{c_char, CStr, CString},
    mem::{size_of, size_of_val, zeroed},
    os::raw::c_void,
    ptr::{addr_of_mut, null, null_mut, NonNull},
    sync::atomic::Ordering,
};

use libc::{memset, strcat, strcmp, strlen};

#[cfg(feature = "libxml_regexp")]
use crate::libxml::xmlautomata::{
    xml_automata_compile, xml_automata_get_init_state, xml_automata_set_final_state,
    xml_free_automata, xml_new_automata, XmlAutomataPtr,
};
#[cfg(feature = "libxml_regexp")]
use crate::libxml::xmlregexp::{
    xml_reg_exec_push_string, xml_reg_free_exec_ctxt, xml_reg_free_regexp, xml_reg_new_exec_ctxt,
    xml_regexp_is_determinist, XmlRegExecCtxtPtr,
};
#[cfg(feature = "libxml_regexp")]
use crate::libxml::xmlstring::xml_strncmp;
#[cfg(not(feature = "libxml_regexp"))]
use crate::tree::xml_free_node_list;
use crate::{
    buf::libxml_api::XmlBufPtr,
    error::{XmlParserErrors, __xml_raise_error},
    globals::{GenericError, GenericErrorContext, StructuredError},
    hash::XmlHashTableRef,
    libxml::{
        dict::{xml_dict_lookup, xml_dict_owns, XmlDictPtr},
        entities::{xml_get_doc_entity, XmlEntityPtr, XmlEntityType},
        globals::{xml_free, xml_malloc, xml_realloc},
        hash::{
            xml_hash_add_entry, xml_hash_add_entry2, xml_hash_copy, xml_hash_create_dict,
            xml_hash_free, xml_hash_lookup, xml_hash_lookup2, xml_hash_remove_entry,
            xml_hash_remove_entry2, xml_hash_scan, xml_hash_update_entry, XmlHashTable,
        },
        list::{
            xml_list_append, xml_list_create, xml_list_delete, xml_list_empty,
            xml_list_remove_first, xml_list_walk, XmlListPtr,
        },
        parser::{XmlParserCtxtPtr, XmlParserMode},
        parser_internals::xml_string_current_char,
        xmlautomata::{
            xml_automata_new_epsilon, xml_automata_new_state, xml_automata_new_transition,
            XmlAutomataStatePtr,
        },
        xmlstring::{xml_str_equal, xml_strdup, xml_strlen, xml_strndup, XmlChar},
    },
    tree::{
        xml_build_qname, xml_free_node, xml_new_doc_node, xml_split_qname2, xml_split_qname3,
        NodeCommon, NodePtr, XmlAttrPtr, XmlAttribute, XmlAttributeDefault, XmlAttributePtr,
        XmlAttributeType, XmlDocProperties, XmlDocPtr, XmlDtdPtr, XmlElement, XmlElementContent,
        XmlElementContentOccur, XmlElementContentPtr, XmlElementContentType, XmlElementPtr,
        XmlElementType, XmlElementTypeVal, XmlEnumeration, XmlEnumerationPtr, XmlID, XmlIDPtr,
        XmlNode, XmlNodePtr, XmlNotation, XmlNotationPtr, XmlNsPtr, XmlRef, XmlRefPtr,
    },
};

use super::{
    chvalid::xml_is_blank_char, dict::XmlDictRef, hash::CVoidWrapper,
    parser_internals::XML_VCTXT_USE_PCTXT,
};

// Validation state added for non-determinist content model.
pub type XmlValidStatePtr = *mut XmlValidState;
// If regexp are enabled we can do continuous validation without the
// need of a tree to validate the content model. this is done in each
// callbacks.
// Each xmlValidState represent the validation state associated to the
// set of nodes currently open from the document root to the current element.
#[cfg(feature = "libxml_regexp")]
#[repr(C)]
pub struct XmlValidState {
    elem_decl: XmlElementPtr, /* pointer to the content model */
    node: XmlNodePtr,         /* pointer to the current node */
    exec: XmlRegExecCtxtPtr,  /* regexp runtime */
}
#[cfg(not(feature = "libxml_regexp"))]
#[repr(C)]
pub struct XmlValidState {
    cont: XmlElementContentPtr, /* pointer to the content model subtree */
    node: XmlNodePtr,           /* pointer to the current node in the list */
    occurs: c_long,             /* bitfield for multiple occurrences */
    depth: c_uchar,             /* current depth in the overall tree */
    state: c_uchar,             /* ROLLBACK_XXX */
}

/// Callback called when a validity error is found. This is a message
/// oriented function similar to an *printf function.
#[doc(alias = "xmlValidityErrorFunc")]
pub type XmlValidityErrorFunc = unsafe extern "C" fn(ctx: *mut c_void, msg: *const c_char);

/// Callback called when a validity warning is found. This is a message
/// oriented function similar to an *printf function.
#[doc(alias = "xmlValidityWarningFunc")]
pub type XmlValidityWarningFunc = unsafe extern "C" fn(ctx: *mut c_void, msg: *const c_char);

pub type XmlValidCtxtPtr = *mut XmlValidCtxt;
/// An xmlValidCtxt is used for error reporting when validating.
#[doc(alias = "xmlValidCtxt")]
#[repr(C)]
pub struct XmlValidCtxt {
    pub(crate) user_data: Option<GenericErrorContext>, /* user specific data block */
    pub error: Option<GenericError>,                   /* the callback in case of errors */
    pub warning: Option<GenericError>,                 /* the callback in case of warning */

    /* Node analysis stack used when validating within entities */
    pub(crate) node: XmlNodePtr,          /* Current parsed Node */
    pub(crate) node_nr: i32,              /* Depth of the parsing stack */
    pub(crate) node_max: i32,             /* Max depth of the parsing stack */
    pub(crate) node_tab: *mut XmlNodePtr, /* array of nodes */

    pub(crate) flags: u32,     /* internal flags */
    pub(crate) doc: XmlDocPtr, /* the document */
    pub(crate) valid: i32,     /* temporary validity check result */

    /* state state used for non-determinist content validation */
    pub(crate) vstate: *mut XmlValidState, /* current state */
    pub(crate) vstate_nr: i32,             /* Depth of the validation stack */
    pub(crate) vstate_max: i32,            /* Max depth of the validation stack */
    pub(crate) vstate_tab: *mut XmlValidState, /* array of validation states */

    #[cfg(feature = "libxml_regexp")]
    pub(crate) am: XmlAutomataPtr, /* the automata */
    #[cfg(feature = "libxml_regexp")]
    pub(crate) state: XmlAutomataStatePtr, /* used to build the automata */
    #[cfg(not(feature = "libxml_regexp"))]
    pub(crate) am: *mut c_void,
    #[cfg(not(feature = "libxml_regexp"))]
    pub(crate) state: *mut c_void,
}

impl Default for XmlValidCtxt {
    fn default() -> Self {
        Self {
            user_data: None,
            error: None,
            warning: None,
            node: null_mut(),
            node_nr: 0,
            node_max: 0,
            node_tab: null_mut(),
            flags: 0,
            doc: null_mut(),
            valid: 0,
            vstate: null_mut(),
            vstate_nr: 0,
            vstate_max: 0,
            vstate_tab: null_mut(),
            am: null_mut(),
            state: null_mut(),
        }
    }
}

// ALL notation declarations are stored in a table.
// There is one table per DTD.
pub type XmlNotationTable = XmlHashTable<'static, CVoidWrapper>;
pub type XmlNotationTablePtr = *mut XmlNotationTable;

// ALL element declarations are stored in a table.
// There is one table per DTD.
pub type XmlElementTable = XmlHashTable<'static, CVoidWrapper>;
pub type XmlElementTablePtr = *mut XmlElementTable;

// ALL attribute declarations are stored in a table.
// There is one table per DTD.
pub type XmlAttributeTable = XmlHashTable<'static, CVoidWrapper>;
pub type XmlAttributeTablePtr = *mut XmlAttributeTable;

// ALL IDs attributes are stored in a table.
// There is one table per document.
pub type XmlIDTable = XmlHashTable<'static, CVoidWrapper>;
pub type XmlIDTablePtr = *mut XmlIDTable;

// ALL Refs attributes are stored in a table.
// There is one table per document.
pub type XmlRefTable = XmlHashTable<'static, CVoidWrapper>;
pub type XmlRefTablePtr = *mut XmlRefTable;

/// Handle a validation error
#[doc(alias = "xmlErrValid")]
macro_rules! xml_err_valid {
    ($ctxt:expr, $error:expr, $msg:expr) => {
        xml_err_valid!(@inner, $ctxt, $error, $msg, None);
    };
    ($ctxt:expr, $error:expr, $msg:expr, $extra:expr) => {
        let msg = format!($msg, $extra);
        xml_err_valid!(@inner, $ctxt, $error, &msg, Some($extra.to_owned().into()));
    };
    (@inner, $ctxt:expr, $error:expr, $msg:expr, $extra:expr) => {
        let ctxt = $ctxt as *mut XmlValidCtxt;
        let mut channel: Option<GenericError> = None;
        let mut pctxt: XmlParserCtxtPtr = null_mut();
        let mut data = None;

        if !ctxt.is_null() {
            channel = (*ctxt).error;
            data = (*ctxt).user_data.clone();
            /* Look up flag to detect if it is part of a parsing
            context */
            if (*ctxt).flags & XML_VCTXT_USE_PCTXT as u32 != 0 {
                pctxt = (*ctxt)
                    .user_data
                    .as_ref()
                    .and_then(|d| {
                        let lock = d.lock();
                        lock.downcast_ref::<XmlParserCtxtPtr>().copied()
                    })
                    .unwrap_or(null_mut());
            }
        }
        __xml_raise_error!(
            None,
            channel,
            data,
            pctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromValid,
            $error,
            XmlErrorLevel::XmlErrError,
            None,
            0,
            $extra,
            None,
            None,
            0,
            0,
            $msg,
        );
    };
}

/// Handle an out of memory error
#[doc(alias = "xmlVErrMemory")]
unsafe fn xml_verr_memory(ctxt: XmlValidCtxtPtr, extra: Option<&str>) {
    let mut channel: Option<GenericError> = None;
    let mut pctxt: XmlParserCtxtPtr = null_mut();
    let mut data = None;

    if !ctxt.is_null() {
        channel = (*ctxt).error;
        data = (*ctxt).user_data.clone();
        // Look up flag to detect if it is part of a parsing context
        if (*ctxt).flags & XML_VCTXT_USE_PCTXT as u32 != 0 {
            pctxt = (*ctxt)
                .user_data
                .as_ref()
                .and_then(|d| {
                    let lock = d.lock();
                    lock.downcast_ref::<XmlParserCtxtPtr>().copied()
                })
                .unwrap_or(null_mut());
        }
    }
    if let Some(extra) = extra {
        __xml_raise_error!(
            None,
            channel,
            data,
            pctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromValid,
            XmlParserErrors::XmlErrNoMemory,
            XmlErrorLevel::XmlErrFatal,
            None,
            0,
            Some(extra.to_owned().into()),
            None,
            None,
            0,
            0,
            "Memory allocation failed : {}\n",
            extra
        );
    } else {
        __xml_raise_error!(
            None,
            channel,
            data,
            pctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromValid,
            XmlParserErrors::XmlErrNoMemory,
            XmlErrorLevel::XmlErrFatal,
            None,
            0,
            None,
            None,
            None,
            0,
            0,
            "Memory allocation failed\n",
        );
    }
}

/// Deallocate the memory used by an notation definition
#[doc(alias = "xmlFreeNotation")]
unsafe extern "C" fn xml_free_notation(nota: XmlNotationPtr) {
    if nota.is_null() {
        return;
    }
    (*nota).name = None;
    (*nota).public_id = None;
    (*nota).system_id = None;
    xml_free(nota as _);
}

/// Register a new notation declaration
///
/// Returns null_mut() if not, otherwise the entity
#[doc(alias = "xmlAddNotationDecl")]
pub unsafe extern "C" fn xml_add_notation_decl(
    ctxt: XmlValidCtxtPtr,
    dtd: XmlDtdPtr,
    name: *const XmlChar,
    public_id: *const XmlChar,
    system_id: *const XmlChar,
) -> XmlNotationPtr {
    let mut table: XmlNotationTablePtr;

    if dtd.is_null() {
        return null_mut();
    }
    if name.is_null() {
        return null_mut();
    }
    if public_id.is_null() && system_id.is_null() {
        return null_mut();
    }

    /*
     * Create the Notation table if needed.
     */
    table = (*dtd).notations as XmlNotationTablePtr;
    if table.is_null() {
        let mut dict: XmlDictPtr = null_mut();
        if !(*dtd).doc.is_null() {
            dict = (*(*dtd).doc).dict;
        }

        (*dtd).notations = xml_hash_create_dict(0, dict) as _;
        table = (*dtd).notations as _;
    }
    if table.is_null() {
        xml_verr_memory(ctxt, Some("xmlAddNotationDecl: Table creation failed!\n"));
        return null_mut();
    }

    let ret: XmlNotationPtr = xml_malloc(size_of::<XmlNotation>()) as XmlNotationPtr;
    if ret.is_null() {
        xml_verr_memory(ctxt as _, Some("malloc failed"));
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlNotation>());
    std::ptr::write(&mut *ret, XmlNotation::default());

    /*
     * fill the structure.
     */
    (*ret).name = Some(
        CStr::from_ptr(name as *const i8)
            .to_string_lossy()
            .into_owned(),
    );
    if !system_id.is_null() {
        (*ret).system_id = Some(
            CStr::from_ptr(system_id as *const i8)
                .to_string_lossy()
                .into_owned(),
        );
    }
    if !public_id.is_null() {
        (*ret).public_id = Some(
            CStr::from_ptr(public_id as *const i8)
                .to_string_lossy()
                .into_owned(),
        );
    }

    // Validity Check:
    // Check the DTD for previous declarations of the ATTLIST
    if xml_hash_add_entry(table, name, ret as _) != 0 {
        #[cfg(feature = "libxml_valid")]
        {
            xml_err_valid!(
                null_mut(),
                XmlParserErrors::XmlDTDNotationRedefined,
                "xmlAddNotationDecl: {} already defined\n",
                (*ret).name.as_deref().unwrap()
            );
        }
        xml_free_notation(ret);
        return null_mut();
    }
    ret
}

/// Build a copy of a notation.
///
/// Returns the new xmlNotationPtr or null_mut() in case of error.
#[doc(alias = "xmlCopyNotation")]
#[cfg(feature = "libxml_tree")]
extern "C" fn xml_copy_notation(payload: *mut c_void, _name: *const XmlChar) -> *mut c_void {
    let nota: XmlNotationPtr = payload as XmlNotationPtr;

    unsafe {
        let cur: XmlNotationPtr = xml_malloc(size_of::<XmlNotation>()) as XmlNotationPtr;
        if cur.is_null() {
            xml_verr_memory(null_mut(), Some("malloc failed"));
            return null_mut();
        }
        std::ptr::write(&mut *cur, XmlNotation::default());
        (*cur).name = (*nota).name.clone();
        (*cur).public_id = (*nota).public_id.clone();
        (*cur).system_id = (*nota).system_id.clone();
        cur as _
    }
}

/// Build a copy of a notation table.
///
/// Returns the new xmlNotationTablePtr or null_mut() in case of error.
#[doc(alias = "xmlCopyNotationTable")]
#[cfg(feature = "libxml_tree")]
pub unsafe extern "C" fn xml_copy_notation_table(
    table: XmlNotationTablePtr,
) -> XmlNotationTablePtr {
    xml_hash_copy(table, Some(xml_copy_notation)) as XmlNotationTablePtr
}

extern "C" fn xml_free_notation_table_entry(nota: *mut c_void, _name: *const XmlChar) {
    unsafe {
        xml_free_notation(nota as XmlNotationPtr);
    }
}

/// Deallocate the memory used by an entities hash table.
#[doc(alias = "xmlFreeNotationTable")]
pub unsafe extern "C" fn xml_free_notation_table(table: XmlNotationTablePtr) {
    xml_hash_free(table, Some(xml_free_notation_table_entry));
}

/// This will dump the content the notation declaration as an XML DTD definition
#[doc(alias = "xmlDumpNotationDecl")]
#[cfg(feature = "libxml_output")]
pub unsafe extern "C" fn xml_dump_notation_decl(buf: XmlBufPtr, nota: XmlNotationPtr) {
    use crate::buf::libxml_api::{xml_buf_cat, xml_buf_write_quoted_string};

    if buf.is_null() || nota.is_null() {
        return;
    }
    xml_buf_cat(buf, c"<!NOTATION ".as_ptr() as _);
    let name = CString::new((*nota).name.as_deref().unwrap()).unwrap();
    xml_buf_cat(buf, name.as_ptr() as *const u8);
    if let Some(public_id) = (*nota).public_id.as_deref() {
        let public_id = CString::new(public_id).unwrap();
        xml_buf_cat(buf, c" PUBLIC ".as_ptr() as _);
        xml_buf_write_quoted_string(buf, public_id.as_ptr() as *const u8);
        if let Some(system_id) = (*nota).system_id.as_deref() {
            let system_id = CString::new(system_id).unwrap();
            xml_buf_cat(buf, c" ".as_ptr() as _);
            xml_buf_write_quoted_string(buf, system_id.as_ptr() as *const u8);
        }
    } else {
        let system_id = CString::new((*nota).system_id.as_deref().unwrap()).unwrap();
        xml_buf_cat(buf, c" SYSTEM ".as_ptr() as _);
        xml_buf_write_quoted_string(buf, system_id.as_ptr() as *const u8);
    }
    xml_buf_cat(buf, c" >\n".as_ptr() as _);
}

/// This is called with the hash scan function, and just reverses args
#[doc(alias = "xmlDumpNotationDeclScan")]
#[cfg(feature = "libxml_output")]
extern "C" fn xml_dump_notation_decl_scan(
    nota: *mut c_void,
    buf: *mut c_void,
    _name: *const XmlChar,
) {
    unsafe {
        xml_dump_notation_decl(buf as XmlBufPtr, nota as XmlNotationPtr);
    }
}

/// This will dump the content of the notation table as an XML DTD definition
#[doc(alias = "xmlDumpNotationTable")]
#[cfg(feature = "libxml_output")]
pub unsafe extern "C" fn xml_dump_notation_table(buf: XmlBufPtr, table: XmlNotationTablePtr) {
    if buf.is_null() || table.is_null() {
        return;
    }
    xml_hash_scan(table, Some(xml_dump_notation_decl_scan), buf as _);
}

/// Allocate an element content structure.
/// Deprecated in favor of xmlNewDocElementContent
///
/// Returns null_mut() if not, otherwise the new element content structure
#[doc(alias = "xmlNewElementContent")]
pub unsafe extern "C" fn xml_new_element_content(
    name: *const XmlChar,
    typ: XmlElementContentType,
) -> XmlElementContentPtr {
    xml_new_doc_element_content(null_mut(), name, typ)
}

/// Build a copy of an element content description.
/// Deprecated, use xmlCopyDocElementContent instead
///
/// Returns the new xmlElementContentPtr or null_mut() in case of error.
#[doc(alias = "xmlCopyElementContent")]
pub unsafe extern "C" fn xml_copy_element_content(
    content: XmlElementContentPtr,
) -> XmlElementContentPtr {
    xml_copy_doc_element_content(null_mut(), content)
}

/// Free an element content structure. The whole subtree is removed.
/// Deprecated, use xmlFreeDocElementContent instead
#[doc(alias = "xmlFreeElementContent")]
pub unsafe extern "C" fn xml_free_element_content(cur: XmlElementContentPtr) {
    xml_free_doc_element_content(null_mut(), cur);
}

/// Allocate an element content structure for the document.
///
/// Returns null_mut() if not, otherwise the new element content structure
#[doc(alias = "xmlNewDocElementContent")]
pub unsafe extern "C" fn xml_new_doc_element_content(
    doc: XmlDocPtr,
    name: *const XmlChar,
    typ: XmlElementContentType,
) -> XmlElementContentPtr {
    let mut dict: XmlDictPtr = null_mut();

    if !doc.is_null() {
        dict = (*doc).dict;
    }

    match typ {
        XmlElementContentType::XmlElementContentElement => {
            if name.is_null() {
                xml_err_valid!(
                    null_mut(),
                    XmlParserErrors::XmlErrInternalError,
                    "xmlNewElementContent : name == NULL !\n"
                );
            }
        }
        XmlElementContentType::XmlElementContentPCDATA
        | XmlElementContentType::XmlElementContentSeq
        | XmlElementContentType::XmlElementContentOr => {
            if !name.is_null() {
                xml_err_valid!(
                    null_mut(),
                    XmlParserErrors::XmlErrInternalError,
                    "xmlNewElementContent : name != NULL !\n"
                );
            }
        } // _ => {
          //     xml_err_valid!(
          //         null_mut(),
          //         XmlParserErrors::XmlErrInternalError,
          //         c"Internal: ELEMENT content corrupted invalid type\n".as_ptr() as _,
          //         null_mut(),
          //     );
          //     return null_mut();
          // }
    }
    let ret: XmlElementContentPtr =
        xml_malloc(size_of::<XmlElementContent>()) as XmlElementContentPtr;
    if ret.is_null() {
        xml_verr_memory(null_mut(), Some("malloc failed"));
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlElementContent>());
    (*ret).typ = typ;
    (*ret).ocur = XmlElementContentOccur::XmlElementContentOnce;
    if !name.is_null() {
        let mut l: i32 = 0;

        let tmp: *const XmlChar = xml_split_qname3(name, addr_of_mut!(l) as _);
        if tmp.is_null() {
            if dict.is_null() {
                (*ret).name = xml_strdup(name);
            } else {
                (*ret).name = xml_dict_lookup(dict, name, -1);
            }
        } else if dict.is_null() {
            (*ret).prefix = xml_strndup(name, l);
            (*ret).name = xml_strdup(tmp);
        } else {
            (*ret).prefix = xml_dict_lookup(dict, name, l);
            (*ret).name = xml_dict_lookup(dict, tmp, -1);
        }
    }
    ret
}

/// Build a copy of an element content description.
///
/// Returns the new xmlElementContentPtr or null_mut() in case of error.
#[doc(alias = "xmlCopyDocElementContent")]
pub unsafe extern "C" fn xml_copy_doc_element_content(
    doc: XmlDocPtr,
    mut cur: XmlElementContentPtr,
) -> XmlElementContentPtr {
    let mut prev: XmlElementContentPtr;
    let mut tmp: XmlElementContentPtr;
    let mut dict: XmlDictPtr = null_mut();

    if cur.is_null() {
        return null_mut();
    }

    if !doc.is_null() {
        dict = (*doc).dict;
    }

    let ret: XmlElementContentPtr =
        xml_malloc(size_of::<XmlElementContent>()) as XmlElementContentPtr;
    if ret.is_null() {
        xml_verr_memory(null_mut(), Some("malloc failed"));
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlElementContent>());
    (*ret).typ = (*cur).typ;
    (*ret).ocur = (*cur).ocur;
    if !(*cur).name.is_null() {
        if !dict.is_null() {
            (*ret).name = xml_dict_lookup(dict, (*cur).name, -1);
        } else {
            (*ret).name = xml_strdup((*cur).name);
        }
    }

    if !(*cur).prefix.is_null() {
        if !dict.is_null() {
            (*ret).prefix = xml_dict_lookup(dict, (*cur).prefix, -1);
        } else {
            (*ret).prefix = xml_strdup((*cur).prefix);
        }
    }
    if !(*cur).c1.is_null() {
        (*ret).c1 = xml_copy_doc_element_content(doc, (*cur).c1);
    }
    if !(*ret).c1.is_null() {
        (*(*ret).c1).parent = ret;
    }
    if !(*cur).c2.is_null() {
        prev = ret;
        cur = (*cur).c2;
        while !cur.is_null() {
            tmp = xml_malloc(size_of::<XmlElementContent>()) as XmlElementContentPtr;
            if tmp.is_null() {
                xml_verr_memory(null_mut(), Some("malloc failed"));
                return ret;
            }
            memset(tmp as _, 0, size_of::<XmlElementContent>());
            (*tmp).typ = (*cur).typ;
            (*tmp).ocur = (*cur).ocur;
            (*prev).c2 = tmp;
            (*tmp).parent = prev;
            if !(*cur).name.is_null() {
                if !dict.is_null() {
                    (*tmp).name = xml_dict_lookup(dict, (*cur).name, -1);
                } else {
                    (*tmp).name = xml_strdup((*cur).name);
                }
            }

            if !(*cur).prefix.is_null() {
                if !dict.is_null() {
                    (*tmp).prefix = xml_dict_lookup(dict, (*cur).prefix, -1);
                } else {
                    (*tmp).prefix = xml_strdup((*cur).prefix);
                }
            }
            if !(*cur).c1.is_null() {
                (*tmp).c1 = xml_copy_doc_element_content(doc, (*cur).c1);
            }
            if !(*tmp).c1.is_null() {
                (*(*tmp).c1).parent = tmp;
            }
            prev = tmp;
            cur = (*cur).c2;
        }
    }
    ret
}

/// Free an element content structure. The whole subtree is removed.
#[doc(alias = "xmlFreeDocElementContent")]
pub unsafe extern "C" fn xml_free_doc_element_content(
    doc: XmlDocPtr,
    mut cur: XmlElementContentPtr,
) {
    let mut dict: XmlDictPtr = null_mut();
    let mut depth: usize = 0;

    if cur.is_null() {
        return;
    }
    if !doc.is_null() {
        dict = (*doc).dict;
    }

    loop {
        while !(*cur).c1.is_null() || !(*cur).c2.is_null() {
            cur = if !(*cur).c1.is_null() {
                (*cur).c1
            } else {
                (*cur).c2
            };
            depth += 1;
        }

        match (*cur).typ {
            XmlElementContentType::XmlElementContentPCDATA
            | XmlElementContentType::XmlElementContentElement
            | XmlElementContentType::XmlElementContentSeq
            | XmlElementContentType::XmlElementContentOr => {} // _ => {
                                                               //     xml_err_valid!(
                                                               //         null_mut(),
                                                               //         XmlParserErrors::XmlErrInternalError,
                                                               //         c"Internal: ELEMENT content corrupted invalid type\n".as_ptr() as _,
                                                               //         null_mut(),
                                                               //     );
                                                               //     return;
                                                               // }
        }
        if !dict.is_null() {
            if !(*cur).name.is_null() && xml_dict_owns(dict, (*cur).name) == 0 {
                xml_free((*cur).name as _);
            }
            if !(*cur).prefix.is_null() && xml_dict_owns(dict, (*cur).prefix) == 0 {
                xml_free((*cur).prefix as _);
            }
        } else {
            if !(*cur).name.is_null() {
                xml_free((*cur).name as _);
            }
            if !(*cur).prefix.is_null() {
                xml_free((*cur).prefix as _);
            }
        }
        let parent: XmlElementContentPtr = (*cur).parent;
        if depth == 0 || parent.is_null() {
            xml_free(cur as _);
            break;
        }
        if cur == (*parent).c1 {
            (*parent).c1 = null_mut();
        } else {
            (*parent).c2 = null_mut();
        }
        xml_free(cur as _);

        if !(*parent).c2.is_null() {
            cur = (*parent).c2;
        } else {
            depth -= 1;
            cur = parent;
        }
    }
}

/// This will dump the content of the element content definition
/// Intended just for the debug routine
#[doc(alias = "xmlSnprintfElementContent")]
pub unsafe extern "C" fn xml_snprintf_element_content(
    buf: *mut c_char,
    size: i32,
    content: XmlElementContentPtr,
    englob: i32,
) {
    let mut len: i32;

    if content.is_null() {
        return;
    }
    len = strlen(buf as _) as _;
    if size - len < 50 {
        if size - len > 4 && *buf.add(len as usize - 1) != b'.' as i8 {
            strcat(buf, c" ...".as_ptr() as _);
        }
        return;
    }
    if englob != 0 {
        strcat(buf, c"(".as_ptr() as _);
    }
    match (*content).typ {
        XmlElementContentType::XmlElementContentPCDATA => {
            strcat(buf, c"#PCDATA".as_ptr() as _);
        }
        XmlElementContentType::XmlElementContentElement => {
            let mut qname_len: i32 = xml_strlen((*content).name);

            if !(*content).prefix.is_null() {
                qname_len += xml_strlen((*content).prefix) + 1;
            }
            if size - len < qname_len + 10 {
                strcat(buf, c" ...".as_ptr() as _);
                return;
            }
            if !(*content).prefix.is_null() {
                strcat(buf, (*content).prefix as _);
                strcat(buf, c":".as_ptr() as _);
            }
            if !(*content).name.is_null() {
                strcat(buf, (*content).name as _);
            }
        }
        XmlElementContentType::XmlElementContentSeq => {
            if matches!(
                (*(*content).c1).typ,
                XmlElementContentType::XmlElementContentOr
                    | XmlElementContentType::XmlElementContentSeq
            ) {
                xml_snprintf_element_content(buf, size, (*content).c1, 1);
            } else {
                xml_snprintf_element_content(buf, size, (*content).c1, 0);
            }
            len = strlen(buf as _) as _;
            if size - len < 50 {
                if size - len > 4 && *buf.add(len as usize - 1) != b'.' as i8 {
                    strcat(buf, c" ...".as_ptr() as _);
                }
                return;
            }
            strcat(buf, c" , ".as_ptr() as _);
            if (matches!(
                (*(*content).c2).typ,
                XmlElementContentType::XmlElementContentOr
            ) || !matches!(
                (*(*content).c2).ocur,
                XmlElementContentOccur::XmlElementContentOnce
            )) && !matches!(
                (*(*content).c2).typ,
                XmlElementContentType::XmlElementContentElement
            ) {
                xml_snprintf_element_content(buf, size, (*content).c2, 1);
            } else {
                xml_snprintf_element_content(buf, size, (*content).c2, 0);
            }
        }
        XmlElementContentType::XmlElementContentOr => {
            if matches!(
                (*(*content).c1).typ,
                XmlElementContentType::XmlElementContentOr
                    | XmlElementContentType::XmlElementContentSeq
            ) {
                xml_snprintf_element_content(buf, size, (*content).c1, 1);
            } else {
                xml_snprintf_element_content(buf, size, (*content).c1, 0);
            }
            len = strlen(buf as _) as _;
            if size - len < 50 {
                if size - len > 4 && *buf.add(len as usize - 1) != b'.' as i8 {
                    strcat(buf, c" ...".as_ptr() as _);
                }
                return;
            }
            strcat(buf, c" | ".as_ptr() as _);
            if (matches!(
                (*(*content).c2).typ,
                XmlElementContentType::XmlElementContentSeq
            ) || !matches!(
                (*(*content).c2).ocur,
                XmlElementContentOccur::XmlElementContentOnce
            )) && !matches!(
                (*(*content).c2).typ,
                XmlElementContentType::XmlElementContentElement
            ) {
                xml_snprintf_element_content(buf, size, (*content).c2, 1);
            } else {
                xml_snprintf_element_content(buf, size, (*content).c2, 0);
            }
        }
    }
    if size as usize - strlen(buf as _) <= 2 {
        return;
    }
    if englob != 0 {
        strcat(buf, c")".as_ptr() as _);
    }
    match (*content).ocur {
        XmlElementContentOccur::XmlElementContentOnce => {}
        XmlElementContentOccur::XmlElementContentOpt => {
            strcat(buf, c"?".as_ptr() as _);
        }
        XmlElementContentOccur::XmlElementContentMult => {
            strcat(buf, c"*".as_ptr() as _);
        }
        XmlElementContentOccur::XmlElementContentPlus => {
            strcat(buf, c"+".as_ptr() as _);
        }
    }
}

#[doc(alias = "xmlSprintfElementContent")]
#[deprecated = "unsafe, use xmlSnprintfElementContent"]
#[cfg(feature = "libxml_output")]
pub unsafe extern "C" fn xml_sprintf_element_content(
    _buf: *mut c_char,
    _content: XmlElementContentPtr,
    _englob: i32,
) {
}

/// Deallocate the memory used by an element definition
#[doc(alias = "xmlFreeElement")]
unsafe extern "C" fn xml_free_element(elem: XmlElementPtr) {
    if elem.is_null() {
        return;
    }
    (*elem).unlink();
    xml_free_doc_element_content((*elem).doc, (*elem).content);
    (*elem).name = None;
    (*elem).prefix = None;
    #[cfg(feature = "libxml_regexp")]
    if !(*elem).cont_model.is_null() {
        xml_reg_free_regexp((*elem).cont_model);
    }
    xml_free(elem as _);
}

/// Handle a validation error, provide contextual information
///
/// # Note
/// This function does not format the string.
#[doc(alias = "xmlErrValidNode")]
#[cfg(any(feature = "libxml_valid", feature = "schema"))]
unsafe fn xml_err_valid_node(
    ctxt: XmlValidCtxtPtr,
    node: XmlNodePtr,
    error: XmlParserErrors,
    msg: &str,
    str1: Option<&str>,
    str2: Option<&str>,
    str3: Option<&str>,
) {
    use crate::globals::StructuredError;

    let schannel: Option<StructuredError> = None;
    let mut channel: Option<GenericError> = None;
    let mut pctxt: XmlParserCtxtPtr = null_mut();
    let mut data = None;

    if !ctxt.is_null() {
        channel = (*ctxt).error;
        data = (*ctxt).user_data.clone();
        // Look up flag to detect if it is part of a parsing context
        if (*ctxt).flags & XML_VCTXT_USE_PCTXT as u32 != 0 {
            pctxt = (*ctxt)
                .user_data
                .as_ref()
                .and_then(|d| {
                    let lock = d.lock();
                    lock.downcast_ref::<XmlParserCtxtPtr>().copied()
                })
                .unwrap_or(null_mut());
        }
    }
    __xml_raise_error!(
        schannel,
        channel,
        data,
        pctxt as _,
        node as _,
        XmlErrorDomain::XmlFromValid,
        error,
        XmlErrorLevel::XmlErrError,
        None,
        0,
        str1.map(|s| s.to_owned().into()),
        str2.map(|s| s.to_owned().into()),
        str3.map(|s| s.to_owned().into()),
        0,
        0,
        msg,
    );
}

/// Register a new element declaration
///
/// Returns null_mut() if not, otherwise the entity
#[doc(alias = "xmlAddElementDecl")]
pub unsafe extern "C" fn xml_add_element_decl(
    ctxt: XmlValidCtxtPtr,
    dtd: XmlDtdPtr,
    name: *const XmlChar,
    typ: XmlElementTypeVal,
    content: XmlElementContentPtr,
) -> XmlElementPtr {
    let mut ret: XmlElementPtr;
    let mut table: XmlElementTablePtr;
    let mut old_attributes: XmlAttributePtr = null_mut();

    if dtd.is_null() {
        return null_mut();
    }
    if name.is_null() {
        return null_mut();
    }

    let name = CStr::from_ptr(name as *const i8).to_string_lossy();
    let mut name = CString::new(name.as_ref()).unwrap();
    match typ {
        XmlElementTypeVal::XmlElementTypeEmpty => {
            if !content.is_null() {
                xml_err_valid!(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    "xmlAddElementDecl: content != NULL for EMPTY\n"
                );
                return null_mut();
            }
        }
        XmlElementTypeVal::XmlElementTypeAny => {
            if !content.is_null() {
                xml_err_valid!(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    "xmlAddElementDecl: content != NULL for ANY\n"
                );
                return null_mut();
            }
        }
        XmlElementTypeVal::XmlElementTypeMixed => {
            if content.is_null() {
                xml_err_valid!(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    "xmlAddElementDecl: content == NULL for MIXED\n"
                );
                return null_mut();
            }
        }
        XmlElementTypeVal::XmlElementTypeElement => {
            if content.is_null() {
                xml_err_valid!(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    "xmlAddElementDecl: content == NULL for ELEMENT\n"
                );
                return null_mut();
            }
        }
        _ => {
            xml_err_valid!(
                ctxt,
                XmlParserErrors::XmlErrInternalError,
                "Internal: ELEMENT decl corrupted invalid type\n"
            );
            return null_mut();
        }
    }

    // check if name is a QName
    let mut ns = null_mut();
    let uqname: *mut XmlChar = xml_split_qname2(name.as_ptr() as *const u8, addr_of_mut!(ns));
    if !uqname.is_null() {
        name = CString::new(
            CStr::from_ptr(uqname as *const i8)
                .to_string_lossy()
                .as_ref(),
        )
        .unwrap();
    }

    // Create the Element table if needed.
    table = (*dtd).elements as XmlElementTablePtr;
    if table.is_null() {
        let mut dict: XmlDictPtr = null_mut();

        if !(*dtd).doc.is_null() {
            dict = (*(*dtd).doc).dict;
        }
        table = xml_hash_create_dict(0, dict);
        (*dtd).elements = table as *mut c_void;
    }
    if table.is_null() {
        xml_verr_memory(ctxt, Some("xmlAddElementDecl: Table creation failed!\n"));
        if !uqname.is_null() {
            xml_free(uqname as _);
        }
        if !ns.is_null() {
            xml_free(ns as _);
        }
        return null_mut();
    }

    // lookup old attributes inserted on an undefined element in the internal subset.
    if !(*dtd).doc.is_null() && !(*(*dtd).doc).int_subset.is_null() {
        ret = xml_hash_lookup2(
            (*(*(*dtd).doc).int_subset).elements as _,
            name.as_ptr() as *const u8,
            ns,
        ) as _;
        if !ret.is_null() && matches!((*ret).etype, XmlElementTypeVal::XmlElementTypeUndefined) {
            old_attributes = (*ret).attributes;
            (*ret).attributes = null_mut();
            xml_hash_remove_entry2(
                (*(*(*dtd).doc).int_subset).elements as _,
                name.as_ptr() as *const u8,
                ns,
                None,
            );
            xml_free_element(ret);
        }
    }

    // The element may already be present if one of its attribute was registered first
    ret = xml_hash_lookup2(table, name.as_ptr() as *const u8, ns) as _;
    if !ret.is_null() {
        if !matches!((*ret).etype, XmlElementTypeVal::XmlElementTypeUndefined) {
            #[cfg(feature = "libxml_valid")]
            {
                let name = name.to_string_lossy();
                // The element is already defined in this DTD.
                xml_err_valid_node(
                    ctxt,
                    dtd as XmlNodePtr,
                    XmlParserErrors::XmlDTDElemRedefined,
                    format!("Redefinition of element {name}\n").as_str(),
                    Some(&name),
                    None,
                    None,
                );
            }
            if !uqname.is_null() {
                xml_free(uqname as _);
            }
            if !ns.is_null() {
                xml_free(ns as _);
            }
            return null_mut();
        }
        if !ns.is_null() {
            xml_free(ns as _);
            // ns = null_mut();
        }
    } else {
        ret = xml_malloc(size_of::<XmlElement>()) as XmlElementPtr;
        if ret.is_null() {
            xml_verr_memory(ctxt as _, Some("malloc failed"));
            if !uqname.is_null() {
                xml_free(uqname as _);
            }
            if !ns.is_null() {
                xml_free(ns as _);
            }
            return null_mut();
        }
        memset(ret as _, 0, size_of::<XmlElement>());
        std::ptr::write(&mut *ret, XmlElement::default());
        (*ret).typ = XmlElementType::XmlElementDecl;

        // fill the structure.
        (*ret).name = Some(Box::new(name.to_string_lossy().into_owned()));
        (*ret).prefix = (!ns.is_null()).then(|| {
            CStr::from_ptr(ns as *const i8)
                .to_string_lossy()
                .into_owned()
        });

        // Validity Check:
        // Insertion must not fail
        if xml_hash_add_entry2(table, name.as_ptr() as *const u8, ns, ret as _) != 0 {
            #[cfg(feature = "libxml_valid")]
            {
                let name = name.to_string_lossy();
                // The element is already defined in this DTD.
                xml_err_valid_node(
                    ctxt,
                    dtd as XmlNodePtr,
                    XmlParserErrors::XmlDTDElemRedefined,
                    format!("Redefinition of element {name}\n").as_str(),
                    Some(&name),
                    None,
                    None,
                );
            }
            xml_free_element(ret);
            if !uqname.is_null() {
                xml_free(uqname as _);
            }
            return null_mut();
        }
        // For new element, may have attributes from earlier
        // definition in internal subset
        (*ret).attributes = old_attributes;
    }

    // Finish to fill the structure.
    (*ret).etype = typ;
    // Avoid a stupid copy when called by the parser
    // and flag it by setting a special parent value
    // so the parser doesn't unallocate it.
    if !ctxt.is_null() && (*ctxt).flags & XML_VCTXT_USE_PCTXT as u32 != 0 {
        (*ret).content = content;
        if !content.is_null() {
            (*content).parent = 1 as XmlElementContentPtr;
        }
    } else {
        (*ret).content = xml_copy_doc_element_content((*dtd).doc, content);
    }

    // Link it to the DTD
    (*ret).parent = dtd;
    (*ret).doc = (*dtd).doc;
    if let Some(mut last) = (*dtd).last {
        last.next = NodePtr::from_ptr(ret as *mut XmlNode);
        (*ret).prev = Some(last);
        (*dtd).last = NodePtr::from_ptr(ret as *mut XmlNode);
    } else {
        (*dtd).children = NodePtr::from_ptr(ret as *mut XmlNode);
        (*dtd).last = (*dtd).children;
    }
    if !uqname.is_null() {
        xml_free(uqname as _);
    }
    if !ns.is_null() {
        xml_free(ns as _);
    }
    ret
}

/// Build a copy of an element.
///
/// Returns the new xmlElementPtr or null_mut() in case of error.
#[doc(alias = "xmlCopyElement")]
#[cfg(feature = "libxml_tree")]
extern "C" fn xml_copy_element(payload: *mut c_void, _name: *const XmlChar) -> *mut c_void {
    let elem: XmlElementPtr = payload as XmlElementPtr;

    unsafe {
        let cur: XmlElementPtr = xml_malloc(size_of::<XmlElement>()) as XmlElementPtr;
        if cur.is_null() {
            xml_verr_memory(null_mut(), Some("malloc failed"));
            return null_mut();
        }
        memset(cur as _, 0, size_of::<XmlElement>());
        std::ptr::write(&mut *cur, XmlElement::default());
        (*cur).typ = XmlElementType::XmlElementDecl;
        (*cur).etype = (*elem).etype;
        (*cur).name = (*elem).name.clone();
        (*cur).prefix = (*elem).prefix.clone();
        (*cur).content = xml_copy_element_content((*elem).content);
        /* TODO : rebuild the attribute list on the copy */
        (*cur).attributes = null_mut();
        cur as _
    }
}

/// Build a copy of an element table.
///
/// Returns the new xmlElementTablePtr or null_mut() in case of error.
#[doc(alias = "xmlCopyElementTable")]
#[cfg(feature = "libxml_tree")]
pub unsafe extern "C" fn xml_copy_element_table(table: XmlElementTablePtr) -> XmlElementTablePtr {
    xml_hash_copy(table, Some(xml_copy_element)) as XmlElementTablePtr
}

extern "C" fn xml_free_element_table_entry(elem: *mut c_void, _name: *const XmlChar) {
    unsafe {
        xml_free_element(elem as XmlElementPtr);
    }
}

/// Deallocate the memory used by an element hash table.
#[doc(alias = "xmlFreeElementTable")]
pub unsafe extern "C" fn xml_free_element_table(table: XmlElementTablePtr) {
    xml_hash_free(table, Some(xml_free_element_table_entry));
}

/// This routine is used by the hash scan function.  
/// It just reverses the arguments.
#[doc(alias = "xmlDumpElementDeclScan")]
#[cfg(feature = "libxml_output")]
extern "C" fn xml_dump_element_decl_scan(
    elem: *mut c_void,
    buf: *mut c_void,
    _name: *const XmlChar,
) {
    unsafe {
        xml_dump_element_decl(buf as XmlBufPtr, elem as XmlElementPtr);
    }
}

/// This will dump the content of the element table as an XML DTD definition
#[doc(alias = "xmlDumpElementTable")]
#[cfg(feature = "libxml_output")]
pub unsafe extern "C" fn xml_dump_element_table(buf: XmlBufPtr, table: XmlElementTablePtr) {
    if buf.is_null() || table.is_null() {
        return;
    }
    xml_hash_scan(table, Some(xml_dump_element_decl_scan), buf as _);
}

/// Dump the occurrence operator of an element.
#[doc(alias = "xmlDumpElementOccur")]
#[cfg(feature = "libxml_output")]
unsafe extern "C" fn xml_dump_element_occur(buf: XmlBufPtr, cur: XmlElementContentPtr) {
    use crate::buf::libxml_api::xml_buf_ccat;

    match (*cur).ocur {
        XmlElementContentOccur::XmlElementContentOnce => {}
        XmlElementContentOccur::XmlElementContentOpt => {
            xml_buf_ccat(buf, c"?".as_ptr() as _);
        }
        XmlElementContentOccur::XmlElementContentMult => {
            xml_buf_ccat(buf, c"*".as_ptr() as _);
        }
        XmlElementContentOccur::XmlElementContentPlus => {
            xml_buf_ccat(buf, c"+".as_ptr() as _);
        }
    }
}

/// This will dump the content of the element table as an XML DTD definition
#[doc(alias = "xmlDumpElementContent")]
#[cfg(feature = "libxml_output")]
unsafe extern "C" fn xml_dump_element_content(buf: XmlBufPtr, content: XmlElementContentPtr) {
    use crate::buf::libxml_api::{xml_buf_cat, xml_buf_ccat};

    let mut cur: XmlElementContentPtr;

    if content.is_null() {
        return;
    }

    xml_buf_ccat(buf, c"(".as_ptr() as _);
    cur = content;

    while {
        'to_continue: {
            if cur.is_null() {
                return;
            }

            match (*cur).typ {
                XmlElementContentType::XmlElementContentPCDATA => {
                    xml_buf_ccat(buf, c"#PCDATA".as_ptr() as _);
                }
                XmlElementContentType::XmlElementContentElement => {
                    if !(*cur).prefix.is_null() {
                        xml_buf_cat(buf, (*cur).prefix);
                        xml_buf_ccat(buf, c":".as_ptr() as _);
                    }
                    xml_buf_cat(buf, (*cur).name);
                }
                XmlElementContentType::XmlElementContentSeq
                | XmlElementContentType::XmlElementContentOr => {
                    if cur != content
                        && !(*cur).parent.is_null()
                        && ((*cur).typ != (*(*cur).parent).typ
                            || !matches!(
                                (*cur).ocur,
                                XmlElementContentOccur::XmlElementContentOnce
                            ))
                    {
                        xml_buf_ccat(buf, c"(".as_ptr() as _);
                    }
                    cur = (*cur).c1;
                    break 'to_continue;
                } // _ => {
                  //     xml_err_valid!(
                  //         null_mut(),
                  //         XmlParserErrors::XmlErrInternalError,
                  //         c"Internal: ELEMENT cur corrupted invalid type\n".as_ptr() as _,
                  //         null_mut(),
                  //     );
                  // }
            }

            while cur != content {
                let parent: XmlElementContentPtr = (*cur).parent;

                if parent.is_null() {
                    return;
                }

                if matches!(
                    (*cur).typ,
                    XmlElementContentType::XmlElementContentOr
                        | XmlElementContentType::XmlElementContentSeq
                ) && ((*cur).typ != (*parent).typ
                    || !matches!((*cur).ocur, XmlElementContentOccur::XmlElementContentOnce))
                {
                    xml_buf_ccat(buf, c")".as_ptr() as _);
                }
                xml_dump_element_occur(buf, cur);

                if cur == (*parent).c1 {
                    if (*parent).typ == XmlElementContentType::XmlElementContentSeq {
                        xml_buf_ccat(buf, c" , ".as_ptr() as _);
                    } else if (*parent).typ == XmlElementContentType::XmlElementContentOr {
                        xml_buf_ccat(buf, c" | ".as_ptr() as _);
                    }

                    cur = (*parent).c2;
                    break;
                }

                cur = parent;
            }
        }
        cur != content
    } {}

    xml_buf_ccat(buf, c")".as_ptr() as _);
    xml_dump_element_occur(buf, content);
}

/// This will dump the content of the element declaration as an XML DTD definition
#[doc(alias = "xmlDumpElementDecl")]
#[cfg(feature = "libxml_output")]
pub unsafe extern "C" fn xml_dump_element_decl(buf: XmlBufPtr, elem: XmlElementPtr) {
    use std::ffi::CString;

    use crate::buf::libxml_api::{xml_buf_cat, xml_buf_ccat};

    if buf.is_null() || elem.is_null() {
        return;
    }
    let name = (*elem)
        .name
        .as_ref()
        .map(|n| CString::new(n.as_str()).unwrap());
    match (*elem).etype {
        XmlElementTypeVal::XmlElementTypeEmpty => {
            xml_buf_ccat(buf, c"<!ELEMENT ".as_ptr() as _);
            if let Some(prefix) = (*elem).prefix.as_deref() {
                let prefix = CString::new(prefix).unwrap();
                xml_buf_cat(buf, prefix.as_ptr() as *const u8);
                xml_buf_ccat(buf, c":".as_ptr() as _);
            }
            xml_buf_cat(
                buf,
                name.as_ref().map_or(null(), |n| n.as_ptr() as *const u8),
            );
            xml_buf_ccat(buf, c" EMPTY>\n".as_ptr() as _);
        }
        XmlElementTypeVal::XmlElementTypeAny => {
            xml_buf_ccat(buf, c"<!ELEMENT ".as_ptr() as _);
            if let Some(prefix) = (*elem).prefix.as_deref() {
                let prefix = CString::new(prefix).unwrap();
                xml_buf_cat(buf, prefix.as_ptr() as *const u8);
                xml_buf_ccat(buf, c":".as_ptr() as _);
            }
            xml_buf_cat(
                buf,
                name.as_ref().map_or(null(), |n| n.as_ptr() as *const u8),
            );
            xml_buf_ccat(buf, c" ANY>\n".as_ptr() as _);
        }
        XmlElementTypeVal::XmlElementTypeMixed => {
            xml_buf_ccat(buf, c"<!ELEMENT ".as_ptr() as _);
            if let Some(prefix) = (*elem).prefix.as_deref() {
                let prefix = CString::new(prefix).unwrap();
                xml_buf_cat(buf, prefix.as_ptr() as *const u8);
                xml_buf_ccat(buf, c":".as_ptr() as _);
            }
            xml_buf_cat(
                buf,
                name.as_ref().map_or(null(), |n| n.as_ptr() as *const u8),
            );
            xml_buf_ccat(buf, c" ".as_ptr() as _);
            xml_dump_element_content(buf, (*elem).content);
            xml_buf_ccat(buf, c">\n".as_ptr() as _);
        }
        XmlElementTypeVal::XmlElementTypeElement => {
            xml_buf_ccat(buf, c"<!ELEMENT ".as_ptr() as _);
            if let Some(prefix) = (*elem).prefix.as_deref() {
                let prefix = CString::new(prefix).unwrap();
                xml_buf_cat(buf, prefix.as_ptr() as *const u8);
                xml_buf_ccat(buf, c":".as_ptr() as _);
            }
            xml_buf_cat(
                buf,
                name.as_ref().map_or(null(), |n| n.as_ptr() as *const u8),
            );
            xml_buf_ccat(buf, c" ".as_ptr() as _);
            xml_dump_element_content(buf, (*elem).content);
            xml_buf_ccat(buf, c">\n".as_ptr() as _);
        }
        _ => {
            xml_err_valid!(
                null_mut(),
                XmlParserErrors::XmlErrInternalError,
                "Internal: ELEMENT struct corrupted invalid type\n"
            );
        }
    }
}

/// create and initialize an enumeration attribute node.
///
/// Returns the xmlEnumerationPtr just created or null_mut() in case of error.
#[doc(alias = "xmlCreateEnumeration")]
pub unsafe fn xml_create_enumeration(name: Option<&str>) -> XmlEnumerationPtr {
    let ret: XmlEnumerationPtr = xml_malloc(size_of::<XmlEnumeration>()) as XmlEnumerationPtr;
    if ret.is_null() {
        xml_verr_memory(null_mut(), Some("malloc failed"));
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlEnumeration>());
    std::ptr::write(&mut *ret, XmlEnumeration::default());

    (*ret).name = name.map(|n| n.to_owned());
    ret
}

/// free an enumeration attribute node (recursive).
#[doc(alias = "xmlFreeEnumeration")]
pub unsafe extern "C" fn xml_free_enumeration(cur: XmlEnumerationPtr) {
    if cur.is_null() {
        return;
    }

    if !(*cur).next.is_null() {
        xml_free_enumeration((*cur).next);
    }

    (*cur).name = None;
    xml_free(cur as _);
}

/// Copy an enumeration attribute node (recursive).
///
/// Returns the xmlEnumerationPtr just created or null_mut() in case of error.
#[doc(alias = "xmlCopyEnumeration")]
#[cfg(feature = "libxml_tree")]
pub unsafe extern "C" fn xml_copy_enumeration(cur: XmlEnumerationPtr) -> XmlEnumerationPtr {
    if cur.is_null() {
        return null_mut();
    }
    let ret: XmlEnumerationPtr = xml_create_enumeration((*cur).name.as_deref());
    if ret.is_null() {
        return null_mut();
    }

    if !(*cur).next.is_null() {
        (*ret).next = xml_copy_enumeration((*cur).next);
    } else {
        (*ret).next = null_mut();
    }

    ret
}

#[cfg(feature = "libxml_valid")]
unsafe extern "C" fn xml_is_doc_name_start_char(doc: XmlDocPtr, c: i32) -> i32 {
    use super::parser_internals::xml_is_letter;

    if doc.is_null() || (*doc).properties & XmlDocProperties::XmlDocOld10 as i32 == 0 {
        /*
         * Use the new checks of production [4] [4a] amd [5] of the
         * Update 5 of XML-1.0
         */
        if (c >= b'a' as i32 && c <= b'z' as i32)
            || (c >= b'A' as i32 && c <= b'Z' as i32)
            || c == b'_' as i32
            || c == b':' as i32
            || (0xC0..=0xD6).contains(&c)
            || (0xD8..=0xF6).contains(&c)
            || (0xF8..=0x2FF).contains(&c)
            || (0x370..=0x37D).contains(&c)
            || (0x37F..=0x1FFF).contains(&c)
            || (0x200C..=0x200D).contains(&c)
            || (0x2070..=0x218F).contains(&c)
            || (0x2C00..=0x2FEF).contains(&c)
            || (0x3001..=0xD7FF).contains(&c)
            || (0xF900..=0xFDCF).contains(&c)
            || (0xFDF0..=0xFFFD).contains(&c)
            || (0x10000..=0xEFFFF).contains(&c)
        {
            return 1;
        }
    } else if xml_is_letter(c as u32) || c == b'_' as i32 || c == b':' as i32 {
        return 1;
    }
    0
}

#[cfg(feature = "libxml_valid")]
unsafe extern "C" fn xml_is_doc_name_char(doc: XmlDocPtr, c: i32) -> i32 {
    use crate::libxml::{
        chvalid::{xml_is_digit, xml_is_extender},
        parser_internals::xml_is_letter,
    };

    use super::chvalid::xml_is_combining;

    if doc.is_null() || (*doc).properties & XmlDocProperties::XmlDocOld10 as i32 == 0 {
        /*
         * Use the new checks of production [4] [4a] amd [5] of the
         * Update 5 of XML-1.0
         */
        if (c >= b'a' as i32 && c <= b'z' as i32)
            || (c >= b'A' as i32 && c <= b'Z' as i32)
            || (c >= b'0' as i32 && c <= b'9' as i32)
            || c == b'_' as i32
            || c == b':' as i32
            || c == b'-' as i32
            || c == b'.' as i32
            || c == 0xB7
            || (0xC0..=0xD6).contains(&c)
            || (0xD8..=0xF6).contains(&c)
            || (0xF8..=0x2FF).contains(&c)
            || (0x300..=0x36F).contains(&c)
            || (0x370..=0x37D).contains(&c)
            || (0x37F..=0x1FFF).contains(&c)
            || (0x200C..=0x200D).contains(&c)
            || (0x203F..=0x2040).contains(&c)
            || (0x2070..=0x218F).contains(&c)
            || (0x2C00..=0x2FEF).contains(&c)
            || (0x3001..=0xD7FF).contains(&c)
            || (0xF900..=0xFDCF).contains(&c)
            || (0xFDF0..=0xFFFD).contains(&c)
            || (0x10000..=0xEFFFF).contains(&c)
        {
            return 1;
        }
    } else if xml_is_letter(c as u32)
        || xml_is_digit(c as u32)
        || c == b'.' as i32
        || c == b'-' as i32
        || c == b'_' as i32
        || c == b':' as i32
        || xml_is_combining(c as u32)
        || xml_is_extender(c as u32)
    {
        return 1;
    }
    0
}

/// Validate that the given value match Names production
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateNamesValueInternal")]
#[cfg(feature = "libxml_valid")]
unsafe extern "C" fn xml_validate_names_value_internal(
    doc: XmlDocPtr,
    value: *const XmlChar,
) -> i32 {
    let mut cur: *const XmlChar;
    let mut val: i32;
    let mut len: i32 = 0;

    if value.is_null() {
        return 0;
    }
    cur = value;
    val = xml_string_current_char(null_mut(), cur, addr_of_mut!(len));
    cur = cur.add(len as usize);

    if xml_is_doc_name_start_char(doc, val) == 0 {
        return 0;
    }

    val = xml_string_current_char(null_mut(), cur, addr_of_mut!(len));
    cur = cur.add(len as usize);
    while xml_is_doc_name_char(doc, val) != 0 {
        val = xml_string_current_char(null_mut(), cur, addr_of_mut!(len));
        cur = cur.add(len as usize);
    }

    /* Should not test IS_BLANK(val) here -- see erratum E20*/
    while val == 0x20 {
        while val == 0x20 {
            val = xml_string_current_char(null_mut(), cur, addr_of_mut!(len));
            cur = cur.add(len as usize);
        }

        if xml_is_doc_name_start_char(doc, val) == 0 {
            return 0;
        }

        val = xml_string_current_char(null_mut(), cur, addr_of_mut!(len));
        cur = cur.add(len as usize);

        while xml_is_doc_name_char(doc, val) != 0 {
            val = xml_string_current_char(null_mut(), cur, addr_of_mut!(len));
            cur = cur.add(len as usize);
        }
    }

    if val != 0 {
        return 0;
    }

    1
}

#[cfg(feature = "libxml_valid")]
unsafe extern "C" fn xml_validate_name_value_internal(
    doc: XmlDocPtr,
    value: *const XmlChar,
) -> i32 {
    let mut cur: *const XmlChar;
    let mut val: i32;
    let mut len: i32 = 0;

    if value.is_null() {
        return 0;
    }
    cur = value;
    val = xml_string_current_char(null_mut(), cur, addr_of_mut!(len));
    cur = cur.add(len as usize);
    if xml_is_doc_name_start_char(doc, val) == 0 {
        return 0;
    }

    val = xml_string_current_char(null_mut(), cur, addr_of_mut!(len));
    cur = cur.add(len as usize);
    while xml_is_doc_name_char(doc, val) != 0 {
        val = xml_string_current_char(null_mut(), cur, addr_of_mut!(len));
        cur = cur.add(len as usize);
    }

    if val != 0 {
        return 0;
    }

    1
}

/// Validate that the given value match Nmtokens production
///
/// `[ VC: Name Token ]`
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateNmtokensValueInternal")]
#[cfg(feature = "libxml_valid")]
unsafe extern "C" fn xml_validate_nmtokens_value_internal(
    doc: XmlDocPtr,
    value: *const XmlChar,
) -> i32 {
    use super::chvalid::xml_is_blank_char;

    let mut cur: *const XmlChar;
    let mut val: i32;
    let mut len: i32 = 0;

    if value.is_null() {
        return 0;
    }
    cur = value;
    val = xml_string_current_char(null_mut(), cur, addr_of_mut!(len));
    cur = cur.add(len as usize);

    while xml_is_blank_char(val as u32) {
        val = xml_string_current_char(null_mut(), cur, addr_of_mut!(len));
        cur = cur.add(len as usize);
    }

    if xml_is_doc_name_char(doc, val) == 0 {
        return 0;
    }

    while xml_is_doc_name_char(doc, val) != 0 {
        val = xml_string_current_char(null_mut(), cur, addr_of_mut!(len));
        cur = cur.add(len as usize);
    }

    /* Should not test IS_BLANK(val) here -- see erratum E20*/
    while val == 0x20 {
        while val == 0x20 {
            val = xml_string_current_char(null_mut(), cur, addr_of_mut!(len));
            cur = cur.add(len as usize);
        }
        if val == 0 {
            return 1;
        }

        if xml_is_doc_name_char(doc, val) == 0 {
            return 0;
        }

        val = xml_string_current_char(null_mut(), cur, addr_of_mut!(len));
        cur = cur.add(len as usize);

        while xml_is_doc_name_char(doc, val) != 0 {
            val = xml_string_current_char(null_mut(), cur, addr_of_mut!(len));
            cur = cur.add(len as usize);
        }
    }

    if val != 0 {
        return 0;
    }

    1
}

/// Validate that the given value match Nmtoken production
///
/// `[ VC: Name Token ]`
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateNmtokenValueInternal")]
#[cfg(feature = "libxml_valid")]
unsafe extern "C" fn xml_validate_nmtoken_value_internal(
    doc: XmlDocPtr,
    value: *const XmlChar,
) -> i32 {
    let mut cur: *const XmlChar;
    let mut val: i32;
    let mut len: i32 = 0;

    if value.is_null() {
        return 0;
    }
    cur = value;
    val = xml_string_current_char(null_mut(), cur, addr_of_mut!(len));
    cur = cur.add(len as usize);

    if xml_is_doc_name_char(doc, val) == 0 {
        return 0;
    }

    val = xml_string_current_char(null_mut(), cur, addr_of_mut!(len));
    cur = cur.add(len as usize);
    while xml_is_doc_name_char(doc, val) != 0 {
        val = xml_string_current_char(null_mut(), cur, addr_of_mut!(len));
        cur = cur.add(len as usize);
    }

    if val != 0 {
        return 0;
    }

    1
}

/// Validate that the given attribute value match  the proper production
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateAttributeValueInternal")]
#[cfg(feature = "libxml_valid")]
unsafe extern "C" fn xml_validate_attribute_value_internal(
    doc: XmlDocPtr,
    typ: XmlAttributeType,
    value: *const XmlChar,
) -> i32 {
    match typ {
        XmlAttributeType::XmlAttributeEntities | XmlAttributeType::XmlAttributeIDREFS => {
            return xml_validate_names_value_internal(doc, value);
        }
        XmlAttributeType::XmlAttributeEntity
        | XmlAttributeType::XmlAttributeIDREF
        | XmlAttributeType::XmlAttributeID
        | XmlAttributeType::XmlAttributeNotation => {
            return xml_validate_name_value_internal(doc, value);
        }
        XmlAttributeType::XmlAttributeNmtokens | XmlAttributeType::XmlAttributeEnumeration => {
            return xml_validate_nmtokens_value_internal(doc, value);
        }
        XmlAttributeType::XmlAttributeNmtoken => {
            return xml_validate_nmtoken_value_internal(doc, value);
        }
        XmlAttributeType::XmlAttributeCDATA => {} // _ => {}
    }
    1
}

/// Handle a validation error, provide contextual information
#[doc(alias = "xmlErrValidWarning")]
macro_rules! xml_err_valid_warning {
    ($ctxt:expr, $node:expr, $error:expr, $msg:literal, $str1:expr) => {
        xml_err_valid_warning!(
            @inner,
            $ctxt,
            $node,
            $error,
            $msg,
            None,
            None,
            None
        )
    };
    ($ctxt:expr, $node:expr, $error:expr, $msg:literal, $str1:expr) => {
        let msg = format!($msg, $str1);
        xml_err_valid_warning!(
            @inner,
            $ctxt,
            $node,
            $error,
            &msg,
            Some($str1.to_owned().into()),
            None,
            None
        )
    };
    ($ctxt:expr, $node:expr, $error:expr, $msg:literal, $str1:expr, $str2:expr) => {
        let msg = format!($msg, $str1, $str2);
        xml_err_valid_warning!(
            @inner,
            $ctxt,
            $node,
            $error,
            &msg,
            Some($str1.to_owned().into()),
            Some($str2.to_owned().into()),
            None
        )
    };
    ($ctxt:expr, $node:expr, $error:expr, $msg:literal, $str1:expr, $str2:expr, $str3:expr) => {
        let msg = format!($msg, $str1, $str2, $str3);
        xml_err_valid_warning!(
            @inner,
            $ctxt,
            $node,
            $error,
            &msg,
            Some($str1.to_owned().into()),
            Some($str2.to_owned().into()),
            Some($str3.to_owned().into())
        )
    };
    (@inner, $ctxt:expr, $node:expr, $error:expr, $msg:expr, $str1:expr, $str2:expr, $str3:expr) => {
        let ctxt = $ctxt as *mut XmlValidCtxt;
        let schannel: Option<StructuredError> = None;
        let mut channel: Option<GenericError> = None;
        let mut pctxt: XmlParserCtxtPtr = null_mut();
        let mut data = None;

        if !ctxt.is_null() {
            channel = (*ctxt).warning;
            data = (*ctxt).user_data.clone();
            // Look up flag to detect if it is part of a parsing context
            if (*ctxt).flags & XML_VCTXT_USE_PCTXT as u32 != 0 {
                pctxt = (*ctxt)
                    .user_data
                    .as_ref()
                    .and_then(|d| {
                        let lock = d.lock();
                        lock.downcast_ref::<XmlParserCtxtPtr>().copied()
                    })
                    .unwrap_or(null_mut());
            }
        }
        __xml_raise_error!(
            schannel,
            channel,
            data,
            pctxt as _,
            $node as _,
            XmlErrorDomain::XmlFromValid,
            $error,
            XmlErrorLevel::XmlErrWarning,
            None,
            0,
            $str1,
            $str2,
            $str3,
            0,
            0,
            $msg,
        );
    };
}

/// Deallocate the memory used by an attribute definition
#[doc(alias = "xmlFreeAttribute")]
unsafe extern "C" fn xml_free_attribute(attr: XmlAttributePtr) {
    if attr.is_null() {
        return;
    }
    let dict = if !(*attr).doc.is_null() {
        (*(*attr).doc).dict
    } else {
        null_mut()
    };
    (*attr).unlink();
    if !(*attr).tree.is_null() {
        xml_free_enumeration((*attr).tree);
    }
    if !dict.is_null() {
        (*attr).elem = None;
        if !(*attr).name.is_null() && xml_dict_owns(dict, (*attr).name) == 0 {
            xml_free((*attr).name as _);
        }
        (*attr).prefix = None;
        if !(*attr).default_value.is_null() && xml_dict_owns(dict, (*attr).default_value) == 0 {
            xml_free((*attr).default_value as _);
        }
    } else {
        (*attr).elem = None;
        if !(*attr).name.is_null() {
            xml_free((*attr).name as _);
        }
        if !(*attr).default_value.is_null() {
            xml_free((*attr).default_value as _);
        }
        (*attr).prefix = None;
    }
    xml_free(attr as _);
}

/// Search the DTD for the description of this element
///
/// returns the xmlElementPtr if found or null_mut()
#[doc(alias = "xmlGetDtdElementDesc2")]
unsafe extern "C" fn xml_get_dtd_element_desc2(
    ctxt: XmlValidCtxtPtr,
    dtd: XmlDtdPtr,
    mut name: *const XmlChar,
    create: i32,
) -> XmlElementPtr {
    let mut table: XmlElementTablePtr;
    let mut cur: XmlElementPtr;

    let mut prefix: *mut XmlChar = null_mut();

    if dtd.is_null() {
        return null_mut();
    }
    if (*dtd).elements.is_null() {
        let mut dict: XmlDictPtr = null_mut();

        if !(*dtd).doc.is_null() {
            dict = (*(*dtd).doc).dict;
        }

        if create == 0 {
            return null_mut();
        }
        /*
         * Create the Element table if needed.
         */
        table = (*dtd).elements as XmlElementTablePtr;
        if table.is_null() {
            table = xml_hash_create_dict(0, dict);
            (*dtd).elements = table as *mut c_void;
        }
        if table.is_null() {
            xml_verr_memory(ctxt, Some("element table allocation failed"));
            return null_mut();
        }
    }
    table = (*dtd).elements as XmlElementTablePtr;

    let uqname: *mut XmlChar = xml_split_qname2(name, addr_of_mut!(prefix));
    if !uqname.is_null() {
        name = uqname;
    }
    let name = CString::new(CStr::from_ptr(name as *const i8).to_string_lossy().as_ref()).unwrap();
    cur = xml_hash_lookup2(table, name.as_ptr() as *const u8, prefix) as _;
    if cur.is_null() && create != 0 {
        cur = xml_malloc(size_of::<XmlElement>()) as XmlElementPtr;
        if cur.is_null() {
            xml_verr_memory(ctxt as _, Some("malloc failed"));
            //  goto error;
            if !prefix.is_null() {
                xml_free(prefix as _);
            }
            if !uqname.is_null() {
                xml_free(uqname as _);
            }
            return cur;
        }
        memset(cur as _, 0, size_of::<XmlElement>());
        std::ptr::write(&mut *cur, XmlElement::default());
        (*cur).typ = XmlElementType::XmlElementDecl;

        /*
         * fill the structure.
         */
        (*cur).name = Some(Box::new(name.to_string_lossy().into_owned()));
        (*cur).prefix = (!prefix.is_null()).then(|| {
            CStr::from_ptr(prefix as *const i8)
                .to_string_lossy()
                .into_owned()
        });
        (*cur).etype = XmlElementTypeVal::XmlElementTypeUndefined;

        if xml_hash_add_entry2(table, name.as_ptr() as *const u8, prefix, cur as _) < 0 {
            xml_verr_memory(ctxt, Some("adding entry failed"));
            xml_free_element(cur);
            cur = null_mut();
        }
    }
    //  error:
    if !prefix.is_null() {
        xml_free(prefix as _);
    }
    if !uqname.is_null() {
        xml_free(uqname as _);
    }
    cur
}

/// Verify that the element don't have too many ID attributes
/// declared.
///
/// Returns the number of ID attributes found.
#[doc(alias = "xmlScanIDAttributeDecl")]
#[cfg(feature = "libxml_valid")]
unsafe extern "C" fn xml_scan_id_attribute_decl(
    ctxt: XmlValidCtxtPtr,
    elem: XmlElementPtr,
    err: i32,
) -> i32 {
    let mut cur: XmlAttributePtr;
    let mut ret: i32 = 0;

    if elem.is_null() {
        return 0;
    }
    cur = (*elem).attributes;
    while !cur.is_null() {
        if matches!((*cur).atype, XmlAttributeType::XmlAttributeID) {
            ret += 1;
            if ret > 1 && err != 0 {
                let elem_name = (*elem).name.as_deref().unwrap();
                let cur_name = (*cur).name().unwrap();
                xml_err_valid_node(
                    ctxt,
                    elem as XmlNodePtr,
                    XmlParserErrors::XmlDTDMultipleID,
                    format!(
                        "Element {elem_name} has too many ID attributes defined : {cur_name}\n"
                    )
                    .as_str(),
                    Some(elem_name.as_str()),
                    Some(&cur_name),
                    None,
                );
            }
        }
        cur = (*cur).nexth;
    }
    ret
}

/// Register a new attribute declaration
/// Note that @tree becomes the ownership of the DTD
///
/// Returns null_mut() if not new, otherwise the attribute decl
#[allow(clippy::too_many_arguments)]
#[doc(alias = "xmlAddAttributeDecl")]
pub unsafe fn xml_add_attribute_decl(
    ctxt: XmlValidCtxtPtr,
    dtd: XmlDtdPtr,
    elem: &str,
    name: &str,
    ns: *const XmlChar,
    typ: XmlAttributeType,
    def: XmlAttributeDefault,
    mut default_value: *const XmlChar,
    tree: XmlEnumerationPtr,
) -> XmlAttributePtr {
    let mut ret: XmlAttributePtr;
    let mut dict: XmlDictPtr = null_mut();

    if dtd.is_null() {
        xml_free_enumeration(tree);
        return null_mut();
    }
    if !(*dtd).doc.is_null() {
        dict = (*(*dtd).doc).dict;
    }

    #[cfg(feature = "libxml_valid")]
    {
        // Check the type and possibly the default value.
        match typ {
            XmlAttributeType::XmlAttributeCDATA => {}
            XmlAttributeType::XmlAttributeID => {}
            XmlAttributeType::XmlAttributeIDREF => {}
            XmlAttributeType::XmlAttributeIDREFS => {}
            XmlAttributeType::XmlAttributeEntity => {}
            XmlAttributeType::XmlAttributeEntities => {}
            XmlAttributeType::XmlAttributeNmtoken => {}
            XmlAttributeType::XmlAttributeNmtokens => {}
            XmlAttributeType::XmlAttributeEnumeration => {}
            XmlAttributeType::XmlAttributeNotation => {} // _ => {
                                                         //     xml_err_valid!(
                                                         //         ctxt,
                                                         //         XmlParserErrors::XmlErrInternalError,
                                                         //         c"Internal: ATTRIBUTE struct corrupted invalid type\n".as_ptr() as _,
                                                         //         null_mut(),
                                                         //     );
                                                         //     xml_free_enumeration(tree);
                                                         //     return null_mut();
                                                         // }
        }
        if !default_value.is_null()
            && xml_validate_attribute_value_internal((*dtd).doc, typ, default_value) == 0
        {
            xml_err_valid_node(
                ctxt,
                dtd as XmlNodePtr,
                XmlParserErrors::XmlDTDAttributeDefault,
                format!("Attribute {elem} of {name}: invalid default value\n").as_str(),
                Some(elem),
                Some(name),
                Some(
                    CStr::from_ptr(default_value as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                ),
            );
            default_value = null_mut();
            if !ctxt.is_null() {
                (*ctxt).valid = 0;
            }
        }
    }

    // Check first that an attribute defined in the external subset wasn't
    // already defined in the internal subset
    if !(*dtd).doc.is_null()
        && (*(*dtd).doc).ext_subset == dtd
        && !(*(*dtd).doc).int_subset.is_null()
    {
        if let Some(attributes) = (*(*(*dtd).doc).int_subset).attributes {
            ret = attributes
                .lookup3(
                    CString::new(name).unwrap().as_c_str(),
                    (!ns.is_null()).then(|| CStr::from_ptr(ns as *const i8)),
                    Some(CString::new(elem).unwrap().as_c_str()),
                )
                .copied()
                .unwrap_or(null_mut());
            if !ret.is_null() {
                xml_free_enumeration(tree);
                return null_mut();
            }
        }
    }

    // Create the Attribute table if needed.
    let mut table = if let Some(table) = (*dtd).attributes {
        table
    } else {
        let dict = XmlDictRef::from_raw(dict);
        let table = XmlHashTable::with_capacity_dict(0, dict);
        let Some(table) = XmlHashTableRef::from_table(table) else {
            xml_verr_memory(ctxt, Some("xmlAddAttributeDecl: Table creation failed!\n"));
            xml_free_enumeration(tree as _);
            return null_mut();
        };
        (*dtd).attributes = Some(table);
        table
    };

    ret = xml_malloc(size_of::<XmlAttribute>()) as XmlAttributePtr;
    if ret.is_null() {
        xml_verr_memory(ctxt as _, Some("malloc failed"));
        xml_free_enumeration(tree);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlAttribute>());
    std::ptr::write(&mut *ret, XmlAttribute::default());
    (*ret).typ = XmlElementType::XmlAttributeDecl;

    // fill the structure.
    (*ret).atype = typ;
    // doc must be set before possible error causes call
    // to xmlFreeAttribute (because it's used to check on dict use)
    (*ret).doc = (*dtd).doc;
    if !dict.is_null() {
        let name = CString::new(name).unwrap();
        (*ret).name = xml_dict_lookup(dict, name.as_ptr() as *const u8, -1);
        let prefix = xml_dict_lookup(dict, ns, -1);
        (*ret).prefix = (!prefix.is_null()).then(|| {
            CStr::from_ptr(prefix as *const i8)
                .to_string_lossy()
                .into_owned()
        });
        let elem = CString::new(elem).unwrap();
        let elem = xml_dict_lookup(dict, elem.as_ptr() as *const u8, -1);
        (*ret).elem = (!elem.is_null()).then(|| {
            CStr::from_ptr(elem as *const i8)
                .to_string_lossy()
                .into_owned()
        });
    } else {
        let name = CString::new(name).unwrap();
        (*ret).name = xml_strdup(name.as_ptr() as *const u8);
        (*ret).prefix = (!ns.is_null()).then(|| {
            CStr::from_ptr(ns as *const i8)
                .to_string_lossy()
                .into_owned()
        });
        (*ret).elem = Some(elem.to_owned());
    }
    (*ret).def = def;
    (*ret).tree = tree;
    if !default_value.is_null() {
        if !dict.is_null() {
            (*ret).default_value = xml_dict_lookup(dict, default_value, -1);
        } else {
            (*ret).default_value = xml_strdup(default_value);
        }
    }

    // Validity Check:
    // Search the DTD for previous declarations of the ATTLIST
    let prefix = (*ret).prefix.as_deref().map(|p| CString::new(p).unwrap());
    let ret_elem = (*ret).elem.as_deref().map(|e| CString::new(e).unwrap());
    if table
        .add_entry3(
            CString::new((*ret).name().unwrap().as_ref())
                .unwrap()
                .as_c_str(),
            prefix.as_deref(),
            ret_elem.as_deref(),
            ret as _,
        )
        .is_err()
    {
        #[cfg(feature = "libxml_valid")]
        {
            // The attribute is already defined in this DTD.
            xml_err_valid_warning!(
                ctxt,
                dtd as XmlNodePtr,
                XmlParserErrors::XmlDTDAttributeRedefined,
                "Attribute {} of element {}: already defined\n",
                name,
                elem
            );
        }
        xml_free_attribute(ret);
        return null_mut();
    }

    let celem = CString::new(elem).unwrap();
    // Validity Check:
    // Multiple ID per element
    let elem_def: XmlElementPtr =
        xml_get_dtd_element_desc2(ctxt, dtd, celem.as_ptr() as *const u8, 1);
    if !elem_def.is_null() {
        #[cfg(feature = "libxml_valid")]
        {
            if matches!(typ, XmlAttributeType::XmlAttributeID)
                && xml_scan_id_attribute_decl(null_mut(), elem_def, 1) != 0
            {
                xml_err_valid_node(
                    ctxt,
                    dtd as XmlNodePtr,
                    XmlParserErrors::XmlDTDMultipleID,
                    format!("Element {elem} has too may ID attributes defined : {name}\n").as_str(),
                    Some(elem),
                    Some(name),
                    None,
                );
                if !ctxt.is_null() {
                    (*ctxt).valid = 0;
                }
            }
        }

        // Insert namespace default def first they need to be processed first.
        if xml_str_equal((*ret).name, c"xmlns".as_ptr() as _)
            || (*ret).prefix.as_deref() == Some("xmlns")
        {
            (*ret).nexth = (*elem_def).attributes;
            (*elem_def).attributes = ret;
        } else {
            let mut tmp: XmlAttributePtr = (*elem_def).attributes;

            while !tmp.is_null()
                && (xml_str_equal((*tmp).name, c"xmlns".as_ptr() as _)
                    || (*ret).prefix.as_deref() == Some("xmlns"))
            {
                if (*tmp).nexth.is_null() {
                    break;
                }
                tmp = (*tmp).nexth;
            }
            if !tmp.is_null() {
                (*ret).nexth = (*tmp).nexth;
                (*tmp).nexth = ret;
            } else {
                (*ret).nexth = (*elem_def).attributes;
                (*elem_def).attributes = ret;
            }
        }
    }

    // Link it to the DTD
    (*ret).parent = dtd;
    if let Some(mut last) = (*dtd).last {
        last.next = NodePtr::from_ptr(ret as *mut XmlNode);
        (*ret).prev = Some(last);
        (*dtd).last = NodePtr::from_ptr(ret as *mut XmlNode);
    } else {
        (*dtd).children = NodePtr::from_ptr(ret as *mut XmlNode);
        (*dtd).last = NodePtr::from_ptr(ret as *mut XmlNode);
    }
    ret
}

/// Build a copy of an attribute.
///
/// Returns the new xmlAttributePtr or null_mut() in case of error.
#[doc(alias = "xmlCopyAttribute")]
#[cfg(feature = "libxml_tree")]
extern "C" fn xml_copy_attribute(attr: *mut XmlAttribute) -> *mut XmlAttribute {
    unsafe {
        let cur: XmlAttributePtr = xml_malloc(size_of::<XmlAttribute>()) as XmlAttributePtr;
        if cur.is_null() {
            xml_verr_memory(null_mut(), Some("malloc failed"));
            return null_mut();
        }
        memset(cur as _, 0, size_of::<XmlAttribute>());
        std::ptr::write(&mut *cur, XmlAttribute::default());
        (*cur).typ = XmlElementType::XmlAttributeDecl;
        (*cur).atype = (*attr).atype;
        (*cur).def = (*attr).def;
        (*cur).tree = xml_copy_enumeration((*attr).tree);
        (*cur).elem = (*attr).elem.clone();
        if !(*attr).name.is_null() {
            (*cur).name = xml_strdup((*attr).name);
        }
        (*cur).prefix = (*attr).prefix.clone();
        if !(*attr).default_value.is_null() {
            (*cur).default_value = xml_strdup((*attr).default_value);
        }
        cur
    }
}

/// Build a copy of an attribute table.
///
/// Returns the new xmlAttributeTablePtr or null_mut() in case of error.
#[doc(alias = "xmlCopyAttributeTable")]
#[cfg(feature = "libxml_tree")]
pub unsafe fn xml_copy_attribute_table(
    table: XmlHashTableRef<'static, *mut XmlAttribute>,
) -> Option<XmlHashTableRef<'_, *mut XmlAttribute>> {
    let new = table.clone_with(|payload, _| xml_copy_attribute(*payload));
    XmlHashTableRef::from_table(new)
}

/// Deallocate the memory used by an entities hash table.
#[doc(alias = "xmlFreeAttributeTable")]
pub unsafe fn xml_free_attribute_table(mut table: XmlHashTable<'static, *mut XmlAttribute>) {
    table.clear_with(|payload, _| {
        xml_free_attribute(payload);
    });
}

/// This is used with the hash scan function - just reverses arguments
#[doc(alias = "xmlDumpAttributeDeclScan")]
#[cfg(feature = "libxml_output")]
extern "C" fn xml_dump_attribute_decl_scan(
    attr: *mut c_void,
    buf: *mut c_void,
    _name: *const XmlChar,
) {
    unsafe {
        xml_dump_attribute_decl(buf as XmlBufPtr, attr as XmlAttributePtr);
    }
}

/// This will dump the content of the attribute table as an XML DTD definition
#[doc(alias = "xmlDumpAttributeTable")]
#[cfg(feature = "libxml_output")]
pub unsafe extern "C" fn xml_dump_attribute_table(buf: XmlBufPtr, table: XmlAttributeTablePtr) {
    if buf.is_null() || table.is_null() {
        return;
    }
    xml_hash_scan(table, Some(xml_dump_attribute_decl_scan), buf as _);
}

/// This will dump the content of the enumeration
#[doc(alias = "xmlDumpEnumeration")]
#[cfg(feature = "libxml_output")]
unsafe extern "C" fn xml_dump_enumeration(buf: XmlBufPtr, cur: XmlEnumerationPtr) {
    use std::ffi::CString;

    use crate::buf::libxml_api::{xml_buf_cat, xml_buf_ccat};

    if buf.is_null() || cur.is_null() {
        return;
    }

    let name = (*cur).name.as_deref().map(|n| CString::new(n).unwrap());
    xml_buf_cat(
        buf,
        name.as_ref()
            .map_or(null_mut(), |n| n.as_ptr() as *const u8),
    );
    if (*cur).next.is_null() {
        xml_buf_ccat(buf, c")".as_ptr() as _);
    } else {
        xml_buf_ccat(buf, c" | ".as_ptr() as _);
        xml_dump_enumeration(buf, (*cur).next);
    }
}

/// This will dump the content of the attribute declaration as an XML DTD definition
#[doc(alias = "xmlDumpAttributeDecl")]
#[cfg(feature = "libxml_output")]
pub unsafe extern "C" fn xml_dump_attribute_decl(buf: XmlBufPtr, attr: XmlAttributePtr) {
    use crate::buf::libxml_api::{xml_buf_cat, xml_buf_ccat, xml_buf_write_quoted_string};

    if buf.is_null() || attr.is_null() {
        return;
    }
    xml_buf_ccat(buf, c"<!ATTLIST ".as_ptr() as _);
    let elem = (*attr).elem.as_deref().map(|e| CString::new(e).unwrap());
    xml_buf_cat(
        buf,
        elem.as_ref().map_or(null(), |e| e.as_ptr() as *const u8),
    );
    xml_buf_ccat(buf, c" ".as_ptr() as _);
    if let Some(prefix) = (*attr).prefix.as_deref() {
        let prefix = CString::new(prefix).unwrap();
        xml_buf_cat(buf, prefix.as_ptr() as *const u8);
        xml_buf_ccat(buf, c":".as_ptr() as _);
    }
    xml_buf_cat(buf, (*attr).name);
    match (*attr).atype {
        XmlAttributeType::XmlAttributeCDATA => {
            xml_buf_ccat(buf, c" CDATA".as_ptr() as _);
        }
        XmlAttributeType::XmlAttributeID => {
            xml_buf_ccat(buf, c" ID".as_ptr() as _);
        }
        XmlAttributeType::XmlAttributeIDREF => {
            xml_buf_ccat(buf, c" IDREF".as_ptr() as _);
        }
        XmlAttributeType::XmlAttributeIDREFS => {
            xml_buf_ccat(buf, c" IDREFS".as_ptr() as _);
        }
        XmlAttributeType::XmlAttributeEntity => {
            xml_buf_ccat(buf, c" ENTITY".as_ptr() as _);
        }
        XmlAttributeType::XmlAttributeEntities => {
            xml_buf_ccat(buf, c" ENTITIES".as_ptr() as _);
        }
        XmlAttributeType::XmlAttributeNmtoken => {
            xml_buf_ccat(buf, c" NMTOKEN".as_ptr() as _);
        }
        XmlAttributeType::XmlAttributeNmtokens => {
            xml_buf_ccat(buf, c" NMTOKENS".as_ptr() as _);
        }
        XmlAttributeType::XmlAttributeEnumeration => {
            xml_buf_ccat(buf, c" (".as_ptr() as _);
            xml_dump_enumeration(buf, (*attr).tree);
        }
        XmlAttributeType::XmlAttributeNotation => {
            xml_buf_ccat(buf, c" NOTATION (".as_ptr() as _);
            xml_dump_enumeration(buf, (*attr).tree);
        } // _ => {
          //     xml_err_valid!(
          //         null_mut(),
          //         XmlParserErrors::XmlErrInternalError,
          //         c"Internal: ATTRIBUTE struct corrupted invalid type\n".as_ptr() as _,
          //         null_mut(),
          //     );
          // }
    }
    match (*attr).def {
        XmlAttributeDefault::XmlAttributeNone => {}
        XmlAttributeDefault::XmlAttributeRequired => {
            xml_buf_ccat(buf, c" #REQUIRED".as_ptr() as _);
        }
        XmlAttributeDefault::XmlAttributeImplied => {
            xml_buf_ccat(buf, c" #IMPLIED".as_ptr() as _);
        }
        XmlAttributeDefault::XmlAttributeFixed => {
            xml_buf_ccat(buf, c" #FIXED".as_ptr() as _);
        } // _ => {
          //     xml_err_valid!(
          //         null_mut(),
          //         XmlParserErrors::XmlErrInternalError,
          //         c"Internal: ATTRIBUTE struct corrupted invalid def\n".as_ptr() as _,
          //         null_mut(),
          //     );
          // }
    }
    if !(*attr).default_value.is_null() {
        xml_buf_ccat(buf, c" ".as_ptr() as _);
        xml_buf_write_quoted_string(buf, (*attr).default_value);
    }
    xml_buf_ccat(buf, c">\n".as_ptr() as _);
}

unsafe extern "C" fn xml_is_streaming(ctxt: XmlValidCtxtPtr) -> i32 {
    if ctxt.is_null() {
        return 0;
    }
    if (*ctxt).flags & XML_VCTXT_USE_PCTXT as u32 == 0 {
        return 0;
    }
    let pctxt = (*ctxt)
        .user_data
        .as_ref()
        .and_then(|d| {
            let lock = d.lock();
            lock.downcast_ref::<XmlParserCtxtPtr>().copied()
        })
        .unwrap_or(null_mut());
    matches!((*pctxt).parse_mode, XmlParserMode::XmlParseReader) as i32
}

/// Free a string if it is not owned by the "dict" dictionary in the current scope
macro_rules! DICT_FREE {
    ($str:expr, $dict:expr) => {
        if !$str.is_null() && ($dict.is_null() || xml_dict_owns($dict, $str as *const XmlChar) == 0)
        {
            xml_free($str as _);
        }
    };
}

/// Deallocate the memory used by an id definition
#[doc(alias = "xmlFreeID")]
unsafe extern "C" fn xml_free_id(id: XmlIDPtr) {
    let mut dict: XmlDictPtr = null_mut();

    if id.is_null() {
        return;
    }

    if !(*id).doc.is_null() {
        dict = (*(*id).doc).dict;
    }

    if !(*id).value.is_null() {
        DICT_FREE!((*id).value, dict);
    }
    if !(*id).name.is_null() {
        DICT_FREE!((*id).name, dict);
    }
    xml_free(id as _);
}

/// Register a new id declaration
///
/// Returns null_mut() if not, otherwise the new xmlIDPtr
#[doc(alias = "xmlAddID")]
pub unsafe extern "C" fn xml_add_id(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    value: *const XmlChar,
    attr: XmlAttrPtr,
) -> XmlIDPtr {
    let mut table: XmlIDTablePtr;

    if doc.is_null() {
        return null_mut();
    }
    if value.is_null() || *value.add(0) == 0 {
        return null_mut();
    }
    if attr.is_null() {
        return null_mut();
    }

    /*
     * Create the ID table if needed.
     */
    table = (*doc).ids as XmlIDTablePtr;
    if table.is_null() {
        (*doc).ids = xml_hash_create_dict(0, (*doc).dict) as _;
        table = (*doc).ids as _;
    }
    if table.is_null() {
        xml_verr_memory(ctxt, Some("xmlAddID: Table creation failed!\n"));
        return null_mut();
    }

    let ret: XmlIDPtr = xml_malloc(size_of::<XmlID>()) as XmlIDPtr;
    if ret.is_null() {
        xml_verr_memory(ctxt as _, Some("malloc failed"));
        return null_mut();
    }

    // fill the structure.
    (*ret).value = xml_strdup(value);
    (*ret).doc = doc;
    if xml_is_streaming(ctxt) != 0 {
        // Operating in streaming mode, attr is gonna disappear
        if !(*doc).dict.is_null() {
            (*ret).name = xml_dict_lookup((*doc).dict, (*attr).name, -1);
        } else {
            (*ret).name = xml_strdup((*attr).name);
        }
        (*ret).attr = null_mut();
    } else {
        (*ret).attr = attr;
        (*ret).name = null_mut();
    }
    (*ret).lineno = (*attr).parent.map_or(-1, |p| p.get_line_no() as i32);

    if xml_hash_add_entry(table, value, ret as _) < 0 {
        // The id is already defined in this DTD.
        #[cfg(feature = "libxml_valid")]
        if !ctxt.is_null() {
            let value = CStr::from_ptr(value as *const i8).to_string_lossy();
            xml_err_valid_node(
                ctxt,
                (*attr).parent.map_or(null_mut(), |p| p.as_ptr()),
                XmlParserErrors::XmlDTDIDRedefined,
                format!("ID {value} already defined\n").as_str(),
                Some(&value),
                None,
                None,
            );
        }
        xml_free_id(ret);
        return null_mut();
    }
    if !attr.is_null() {
        (*attr).atype = Some(XmlAttributeType::XmlAttributeID);
    }
    ret
}

extern "C" fn xml_free_id_table_entry(id: *mut c_void, _name: *const XmlChar) {
    unsafe {
        xml_free_id(id as XmlIDPtr);
    }
}

/// Deallocate the memory used by an ID hash table.
#[doc(alias = "xmlFreeIDTable")]
pub unsafe extern "C" fn xml_free_id_table(table: XmlIDTablePtr) {
    xml_hash_free(table, Some(xml_free_id_table_entry));
}

/// Search the attribute declaring the given ID
///
/// Returns null_mut() if not found,
/// otherwise the xmlAttrPtr defining the ID or XmlDocPtr as `*mut dyn NodeCommon`.
#[doc(alias = "xmlGetID")]
pub unsafe fn xml_get_id(doc: XmlDocPtr, id: *const XmlChar) -> Option<NonNull<dyn NodeCommon>> {
    if doc.is_null() {
        return None;
    }

    if id.is_null() {
        return None;
    }

    let table: XmlIDTablePtr = (*doc).ids as XmlIDTablePtr;
    if table.is_null() {
        return None;
    }

    let id_ptr: XmlIDPtr = xml_hash_lookup(table, id) as _;
    if id_ptr.is_null() {
        return None;
    }
    if (*id_ptr).attr.is_null() {
        // We are operating on a stream, return a well known reference
        // since the attribute node doesn't exist anymore
        return NonNull::new(doc as *mut dyn NodeCommon);
    }
    NonNull::new((*id_ptr).attr as *mut dyn NodeCommon)
}

/// Determine whether an attribute is of type ID. In case we have DTD(s)
/// then this is done if DTD loading has been requested. In the case
/// of HTML documents parsed with the HTML parser, then ID detection is
/// done systematically.
///
/// Returns 0 or 1 depending on the lookup result
#[doc(alias = "xmlIsID")]
pub unsafe extern "C" fn xml_is_id(doc: XmlDocPtr, elem: XmlNodePtr, attr: XmlAttrPtr) -> i32 {
    if attr.is_null() || (*attr).name.is_null() {
        return 0;
    }
    if !(*attr).ns.is_null()
        && !(*(*attr).ns).prefix.is_null()
        && strcmp((*attr).name as _, c"id".as_ptr() as _) == 0
        && strcmp((*(*attr).ns).prefix as *const i8, c"xml".as_ptr() as _) == 0
    {
        return 1;
    }
    if doc.is_null() {
        return 0;
    }
    if (*doc).int_subset.is_null()
        && (*doc).ext_subset.is_null()
        && !matches!((*doc).typ, XmlElementType::XmlHTMLDocumentNode)
    {
        return 0;
    } else if matches!((*doc).typ, XmlElementType::XmlHTMLDocumentNode) {
        if xml_str_equal(c"id".as_ptr() as _, (*attr).name)
            || (xml_str_equal(c"name".as_ptr() as _, (*attr).name)
                && (elem.is_null() || xml_str_equal((*elem).name, c"a".as_ptr() as _)))
        {
            return 1;
        }
        return 0;
    } else if elem.is_null() {
        return 0;
    } else {
        let mut attr_decl: XmlAttributePtr = null_mut();

        let felem: [XmlChar; 50] = [0; 50];
        let fattr: [XmlChar; 50] = [0; 50];

        let fullelemname: *mut XmlChar = if !(*elem).ns.is_null() && !(*(*elem).ns).prefix.is_null()
        {
            xml_build_qname((*elem).name, (*(*elem).ns).prefix, felem.as_ptr() as _, 50)
        } else {
            (*elem).name as *mut XmlChar
        };

        let fullattrname: *mut XmlChar = if !(*attr).ns.is_null() && !(*(*attr).ns).prefix.is_null()
        {
            xml_build_qname((*attr).name, (*(*attr).ns).prefix, fattr.as_ptr() as _, 50)
        } else {
            (*attr).name as *mut XmlChar
        };

        if !fullelemname.is_null() && !fullattrname.is_null() {
            attr_decl = (*(*doc).int_subset).get_attr_desc(
                CStr::from_ptr(fullelemname as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                CStr::from_ptr(fullattrname as *const i8)
                    .to_string_lossy()
                    .as_ref(),
            );
            if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                attr_decl = (*(*doc).ext_subset).get_attr_desc(
                    CStr::from_ptr(fullelemname as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    CStr::from_ptr(fullattrname as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                );
            }
        }

        if fullattrname != fattr.as_ptr() as _ && fullattrname != (*attr).name as _ {
            xml_free(fullattrname as _);
        }
        if fullelemname != felem.as_ptr() as _ && fullelemname != (*elem).name as _ {
            xml_free(fullelemname as _);
        }

        if !attr_decl.is_null() && matches!((*attr_decl).atype, XmlAttributeType::XmlAttributeID) {
            return 1;
        }
    }
    0
}

/// Normalize a string in-place.
#[doc(alias = "xmlValidNormalizeString")]
unsafe extern "C" fn xml_valid_normalize_string(str: *mut XmlChar) {
    let mut dst: *mut XmlChar;
    let mut src: *const XmlChar;

    if str.is_null() {
        return;
    }
    src = str;
    dst = str;

    while *src == 0x20 {
        src = src.add(1);
    }
    while *src != 0 {
        if *src == 0x20 {
            while *src == 0x20 {
                src = src.add(1);
            }
            if *src != 0 {
                *dst = 0x20;
                dst = dst.add(1);
            }
        } else {
            *dst = *src;
            dst = dst.add(1);
            src = src.add(1);
        }
    }
    *dst = 0;
}

/// Remove the given attribute from the ID table maintained internally.
///
/// Returns -1 if the lookup failed and 0 otherwise
#[doc(alias = "xmlRemoveID")]
pub unsafe extern "C" fn xml_remove_id(doc: XmlDocPtr, attr: XmlAttrPtr) -> i32 {
    if doc.is_null() {
        return -1;
    }
    if attr.is_null() {
        return -1;
    }

    let table: XmlIDTablePtr = (*doc).ids as XmlIDTablePtr;
    if table.is_null() {
        return -1;
    }

    let Some(id) = (*attr).children.and_then(|c| c.get_string(doc, 1)) else {
        return -1;
    };
    let id = CString::new(id).unwrap();
    let id = xml_strdup(id.as_ptr() as *const u8);
    xml_valid_normalize_string(id);

    let id_ptr: XmlIDPtr = xml_hash_lookup(table, id) as _;
    if id_ptr.is_null() || (*id_ptr).attr != attr {
        xml_free(id as _);
        return -1;
    }

    xml_hash_remove_entry(table, id, Some(xml_free_id_table_entry));
    xml_free(id as _);
    (*attr).atype = None;
    0
}

/// Deallocate the memory used by a ref definition
#[doc(alias = "xmlFreeRef")]
extern "C" fn xml_free_ref(data: *mut c_void) {
    let refe: XmlRefPtr = data as XmlRefPtr;
    if refe.is_null() {
        return;
    }
    unsafe {
        if !(*refe).value.is_null() {
            xml_free((*refe).value as _);
        }
        if !(*refe).name.is_null() {
            xml_free((*refe).name as _);
        }
        xml_free(refe as _);
    }
}

/// Do nothing, return 0. Used to create unordered lists.
#[doc(alias = "xmlDummyCompare")]
extern "C" fn xml_dummy_compare(_data0: *const c_void, _data1: *const c_void) -> i32 {
    0
}

/// Register a new ref declaration
///
/// Returns null_mut() if not, otherwise the new xmlRefPtr
#[doc(alias = "xmlAddRef")]
pub(crate) unsafe extern "C" fn xml_add_ref(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    value: *const XmlChar,
    attr: XmlAttrPtr,
) -> XmlRefPtr {
    let mut table: XmlRefTablePtr;
    let mut ref_list: XmlListPtr;

    if doc.is_null() {
        return null_mut();
    }
    if value.is_null() {
        return null_mut();
    }
    if attr.is_null() {
        return null_mut();
    }

    // Create the Ref table if needed.
    table = (*doc).refs as XmlRefTablePtr;
    if table.is_null() {
        (*doc).refs = xml_hash_create_dict(0, (*doc).dict) as _;
        table = (*doc).refs as _;
    }
    if table.is_null() {
        xml_verr_memory(ctxt, Some("xmlAddRef: Table creation failed!\n"));
        return null_mut();
    }

    let ret: XmlRefPtr = xml_malloc(size_of::<XmlRef>()) as XmlRefPtr;
    if ret.is_null() {
        xml_verr_memory(ctxt as _, Some("malloc failed"));
        return null_mut();
    }

    // fill the structure.
    (*ret).value = xml_strdup(value);
    if xml_is_streaming(ctxt) != 0 {
        /*
         * Operating in streaming mode, attr is gonna disappear
         */
        (*ret).name = xml_strdup((*attr).name);
        (*ret).attr = null_mut();
    } else {
        (*ret).name = null_mut();
        (*ret).attr = attr;
    }
    (*ret).lineno = (*attr).parent.map_or(-1, |p| p.get_line_no() as i32);

    /* To add a reference :-
     * References are maintained as a list of references,
     * Lookup the entry, if no entry create new nodelist
     * Add the owning node to the NodeList
     * Return the ref
     */

    ref_list = xml_hash_lookup(table, value) as _;
    'failed: {
        if ref_list.is_null() {
            ref_list = xml_list_create(Some(xml_free_ref), Some(xml_dummy_compare));
            if ref_list.is_null() {
                xml_err_valid!(
                    null_mut(),
                    XmlParserErrors::XmlErrInternalError,
                    "xmlAddRef: Reference list creation failed!\n"
                );
                break 'failed;
            }
            if xml_hash_add_entry(table, value, ref_list as _) < 0 {
                xml_list_delete(ref_list);
                xml_err_valid!(
                    null_mut(),
                    XmlParserErrors::XmlErrInternalError,
                    "xmlAddRef: Reference list insertion failed!\n"
                );
                break 'failed;
            }
        }
        if xml_list_append(ref_list, ret as _) != 0 {
            xml_err_valid!(
                null_mut(),
                XmlParserErrors::XmlErrInternalError,
                "xmlAddRef: Reference list insertion failed!\n"
            );
            break 'failed;
        }
        return ret;
    }
    // failed:
    if !ret.is_null() {
        if !(*ret).value.is_null() {
            xml_free((*ret).value as _);
        }
        if !(*ret).name.is_null() {
            xml_free((*ret).name as _);
        }
        xml_free(ret as _);
    }
    null_mut()
}

/// Deallocate the memory used by a list of references
#[doc(alias = "xmlFreeRefTableEntry")]
extern "C" fn xml_free_ref_table_entry(payload: *mut c_void, _name: *const XmlChar) {
    let list_ref: XmlListPtr = payload as XmlListPtr;
    if list_ref.is_null() {
        return;
    }
    xml_list_delete(list_ref);
}

/// Deallocate the memory used by an Ref hash table.
#[doc(alias = "xmlFreeRefTable")]
pub(crate) unsafe extern "C" fn xml_free_ref_table(table: XmlRefTablePtr) {
    xml_hash_free(table, Some(xml_free_ref_table_entry));
}

/// Determine whether an attribute is of type Ref. In case we have DTD(s)
/// then this is simple, otherwise we use an heuristic: name Ref (upper or lowercase).
///
/// Returns 0 or 1 depending on the lookup result
#[doc(alias = "xmlIsRef")]
pub(crate) unsafe extern "C" fn xml_is_ref(
    mut doc: XmlDocPtr,
    elem: XmlNodePtr,
    attr: XmlAttrPtr,
) -> i32 {
    if attr.is_null() {
        return 0;
    }
    if doc.is_null() {
        doc = (*attr).doc;
        if doc.is_null() {
            return 0;
        }
    }

    if (*doc).int_subset.is_null() && (*doc).ext_subset.is_null() {
        return 0;
    } else if matches!((*doc).typ, XmlElementType::XmlHTMLDocumentNode) {
        /* TODO @@@ */
        return 0;
    } else {
        let mut attr_decl: XmlAttributePtr;

        if elem.is_null() {
            return 0;
        }
        attr_decl = (*(*doc).int_subset).get_attr_desc(
            (*elem).name().as_deref().unwrap(),
            (*attr).name().as_deref().unwrap(),
        );
        if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
            attr_decl = (*(*doc).ext_subset).get_attr_desc(
                (*elem).name().as_deref().unwrap(),
                (*attr).name().as_deref().unwrap(),
            );
        }

        if !attr_decl.is_null()
            && matches!(
                (*attr_decl).atype,
                XmlAttributeType::XmlAttributeIDREF | XmlAttributeType::XmlAttributeIDREFS
            )
        {
            return 1;
        }
    }
    0
}

#[repr(C)]
pub struct XmlRemoveMemo {
    l: XmlListPtr,
    ap: XmlAttrPtr,
}
pub type XmlRemoveMemoPtr = *mut XmlRemoveMemo;

/// Returns 0 to abort the walk or 1 to continue
#[doc(alias = "xmlWalkRemoveRef")]
extern "C" fn xml_walk_remove_ref(data: *const c_void, user: *mut c_void) -> i32 {
    unsafe {
        let attr0: XmlAttrPtr = (*(data as XmlRefPtr)).attr;
        let attr1: XmlAttrPtr = (*(user as XmlRemoveMemoPtr)).ap;
        let ref_list: XmlListPtr = (*(user as XmlRemoveMemoPtr)).l;

        if attr0 == attr1 {
            /* Matched: remove and terminate walk */
            xml_list_remove_first(ref_list, data as *mut c_void);
            return 0;
        }
        1
    }
}

/// Remove the given attribute from the Ref table maintained internally.
///
/// Returns -1 if the lookup failed and 0 otherwise
#[doc(alias = "xmlRemoveRef")]
pub(crate) unsafe extern "C" fn xml_remove_ref(doc: XmlDocPtr, attr: XmlAttrPtr) -> i32 {
    let mut target: XmlRemoveMemo = unsafe { zeroed() };

    if doc.is_null() {
        return -1;
    }
    if attr.is_null() {
        return -1;
    }

    let table: XmlRefTablePtr = (*doc).refs as XmlRefTablePtr;
    if table.is_null() {
        return -1;
    }

    let Some(id) = (*attr).children.and_then(|c| c.get_string(doc, 1)) else {
        return -1;
    };
    let id = CString::new(id).unwrap();

    let ref_list: XmlListPtr = xml_hash_lookup(table, id.as_ptr() as *const u8) as _;
    if ref_list.is_null() {
        return -1;
    }

    // At this point, ref_list refers to a list of references which
    // have the same key as the supplied attr. Our list of references
    // is ordered by reference address and we don't have that information
    // here to use when removing. We'll have to walk the list and
    // check for a matching attribute, when we find one stop the walk
    // and remove the entry.
    // The list is ordered by reference, so that means we don't have the
    // key. Passing the list and the reference to the walker means we
    // will have enough data to be able to remove the entry.
    target.l = ref_list;
    target.ap = attr;

    // Remove the supplied attr from our list
    xml_list_walk(
        ref_list,
        Some(xml_walk_remove_ref),
        addr_of_mut!(target) as _,
    );

    // If the list is empty then remove the list entry in the hash
    if xml_list_empty(ref_list) != 0 {
        xml_hash_update_entry(
            table,
            id.as_ptr() as *const u8,
            null_mut(),
            Some(xml_free_ref_table_entry),
        );
    }
    0
}

/// Find the set of references for the supplied ID.
///
/// Returns null_mut() if not found, otherwise node set for the ID.
#[doc(alias = "xmlGetRefs")]
pub(crate) unsafe extern "C" fn xml_get_refs(doc: XmlDocPtr, id: *const XmlChar) -> XmlListPtr {
    if doc.is_null() {
        return null_mut();
    }

    if id.is_null() {
        return null_mut();
    }

    let table: XmlRefTablePtr = (*doc).refs as XmlRefTablePtr;
    if table.is_null() {
        return null_mut();
    }

    xml_hash_lookup(table, id) as _
}

/// Allocate a validation context structure.
///
/// Returns null_mut() if not, otherwise the new validation context structure
#[doc(alias = "xmlNewValidCtxt")]
#[cfg(feature = "libxml_valid")]
pub unsafe extern "C" fn xml_new_valid_ctxt() -> XmlValidCtxtPtr {
    let ret: XmlValidCtxtPtr = xml_malloc(size_of::<XmlValidCtxt>()) as _;
    if ret.is_null() {
        xml_verr_memory(null_mut(), Some("malloc failed"));
        return null_mut();
    }

    memset(ret as _, 0, size_of::<XmlValidCtxt>());

    ret
}

/// Free a validation context structure.
#[doc(alias = "xmlFreeValidCtxt")]
#[cfg(feature = "libxml_valid")]
pub unsafe extern "C" fn xml_free_valid_ctxt(cur: XmlValidCtxtPtr) {
    if cur.is_null() {
        return;
    }
    if !(*cur).vstate_tab.is_null() {
        xml_free((*cur).vstate_tab as _);
    }
    if !(*cur).node_tab.is_null() {
        xml_free((*cur).node_tab as _);
    }
    xml_free(cur as _);
}

/// Try to validate a the root element
/// basically it does the following check as described by the
/// XML-1.0 recommendation:
///  - [ VC: Root Element Type ]
///    it doesn't try to recurse or apply other check to the element
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateRoot")]
#[cfg(feature = "libxml_valid")]
pub unsafe extern "C" fn xml_validate_root(ctxt: XmlValidCtxtPtr, doc: XmlDocPtr) -> i32 {
    let ret: i32;

    if doc.is_null() {
        return 0;
    }

    let root: XmlNodePtr = (*doc).get_root_element();
    if root.is_null() || (*root).name.is_null() {
        xml_err_valid!(ctxt, XmlParserErrors::XmlDTDNoRoot, "no root element\n");
        return 0;
    }

    // When doing post validation against a separate DTD, those may
    // no internal subset has been generated
    if !(*doc).int_subset.is_null() && !(*(*doc).int_subset).name.is_null() {
        // Check first the document root against the NQName
        if !xml_str_equal((*(*doc).int_subset).name, (*root).name) {
            if !(*root).ns.is_null() && !(*(*root).ns).prefix.is_null() {
                let mut fname: [XmlChar; 50] = [0; 50];

                let fullname: *mut XmlChar = xml_build_qname(
                    (*root).name,
                    (*(*root).ns).prefix as _,
                    fname.as_mut_ptr(),
                    50,
                );
                if fullname.is_null() {
                    xml_verr_memory(ctxt, None);
                    return 0;
                }
                ret = xml_str_equal((*(*doc).int_subset).name, fullname) as i32;
                if fullname != fname.as_ptr() as _ && fullname != (*root).name as _ {
                    xml_free(fullname as _);
                }
                if ret == 1 {
                    // goto name_ok;
                    return 1;
                }
            }
            if xml_str_equal((*(*doc).int_subset).name, c"HTML".as_ptr() as _)
                && xml_str_equal((*root).name, c"html".as_ptr() as _)
            {
                // goto name_ok;
                return 1;
            }

            let root_name = (*root).name().unwrap();
            let subset_name = (*(*doc).int_subset).name().unwrap();
            xml_err_valid_node(
                ctxt,
                root,
                XmlParserErrors::XmlDTDRootName,
                format!("root and DTD name do not match '{root_name}' and '{subset_name}'\n")
                    .as_str(),
                Some(&root_name),
                Some(&subset_name),
                None,
            );
            return 0;
        }
    }
    // name_ok:
    1
}

macro_rules! CHECK_DTD {
    ($doc:expr) => {
        if $doc.is_null() {
            return 0;
        } else if (*$doc).int_subset.is_null() && (*$doc).ext_subset.is_null() {
            return 0;
        }
    };
}

/// Try to validate a single element definition
/// basically it does the following checks as described by the
/// XML-1.0 recommendation:
///  - [ VC: One ID per Element Type ]
///  - [ VC: No Duplicate Types ]
///  - [ VC: Unique Element Type Declaration ]
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateElementDecl")]
#[cfg(feature = "libxml_valid")]
pub unsafe extern "C" fn xml_validate_element_decl(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    elem: XmlElementPtr,
) -> i32 {
    let mut ret: i32 = 1;
    let mut tst: XmlElementPtr;

    CHECK_DTD!(doc);

    if elem.is_null() {
        return 1;
    }

    // #if 0
    // #ifdef LIBXML_REGEXP_ENABLED
    //     /* Build the regexp associated to the content model */
    //     ret = xmlValidBuildContentModel(ctxt, elem);
    // #endif
    // #endif

    /* No Duplicate Types */
    if matches!((*elem).etype, XmlElementTypeVal::XmlElementTypeMixed) {
        let mut cur: XmlElementContentPtr;
        let mut next: XmlElementContentPtr;
        let mut name: *const XmlChar;

        cur = (*elem).content;
        while !cur.is_null() {
            if !matches!((*cur).typ, XmlElementContentType::XmlElementContentOr) {
                break;
            }
            if (*cur).c1.is_null() {
                break;
            }
            if matches!(
                (*(*cur).c1).typ,
                XmlElementContentType::XmlElementContentElement
            ) {
                name = (*(*cur).c1).name;
                next = (*cur).c2;
                while !next.is_null() {
                    if matches!((*next).typ, XmlElementContentType::XmlElementContentElement) {
                        if xml_str_equal((*next).name, name)
                            && xml_str_equal((*next).prefix, (*(*cur).c1).prefix)
                        {
                            let elem_name = (*elem).name.as_deref().unwrap();
                            let name = CStr::from_ptr(name as *const i8).to_string_lossy();
                            if (*(*cur).c1).prefix.is_null() {
                                xml_err_valid_node(
                                    ctxt,
                                    elem as XmlNodePtr,
                                    XmlParserErrors::XmlDTDContentError,
                                    format!("Definition of {elem_name} has duplicate references of {name}\n")
                                        .as_str(),
                                    Some(elem_name.as_str()),
                                    Some(&name),
                                    None,
                                );
                            } else {
                                let prefix = CStr::from_ptr((*(*cur).c1).prefix as *const i8)
                                    .to_string_lossy();
                                xml_err_valid_node(
                                    ctxt,
                                    elem as XmlNodePtr,
                                    XmlParserErrors::XmlDTDContentError,
                                    format!("Definition of {elem_name} has duplicate references of {prefix}:{name}\n").as_str(),
                                    Some(elem_name.as_str()),
                                    Some(&prefix),
                                    Some(&name),
                                );
                            }
                            ret = 0;
                        }
                        break;
                    }
                    if (*next).c1.is_null() {
                        break;
                    }
                    if !matches!(
                        (*(*next).c1).typ,
                        XmlElementContentType::XmlElementContentElement
                    ) {
                        break;
                    }
                    if xml_str_equal((*(*next).c1).name, name)
                        && xml_str_equal((*(*next).c1).prefix, (*(*cur).c1).prefix)
                    {
                        let elem_name = (*elem).name.as_deref().unwrap();
                        let name = CStr::from_ptr(name as *const i8).to_string_lossy();
                        if (*(*cur).c1).prefix.is_null() {
                            xml_err_valid_node(
                                ctxt,
                                elem as XmlNodePtr,
                                XmlParserErrors::XmlDTDContentError,
                                format!(
                                    "Definition of {elem_name} has duplicate references to {name}\n"
                                )
                                .as_str(),
                                Some(elem_name.as_str()),
                                Some(&name),
                                None,
                            );
                        } else {
                            let prefix =
                                CStr::from_ptr((*(*cur).c1).prefix as *const i8).to_string_lossy();
                            xml_err_valid_node(
                                ctxt,
                                elem as XmlNodePtr,
                                XmlParserErrors::XmlDTDContentError,
                                format!(
                                    "Definition of {elem_name} has duplicate references to {prefix}:{name}\n"
                                )
                                .as_str(),
                                Some(elem_name.as_str()),
                                Some(&prefix),
                                Some(&name),
                            );
                        }
                        ret = 0;
                    }
                    next = (*next).c2;
                }
            }
            cur = (*cur).c2;
        }
    }

    let elem_name = (*elem)
        .name
        .as_ref()
        .map(|n| CString::new(n.as_str()).unwrap());
    // VC: Unique Element Type Declaration
    tst = xml_get_dtd_element_desc(
        (*doc).int_subset,
        elem_name
            .as_ref()
            .map_or(null(), |n| n.as_ptr() as *const u8),
    );
    if !tst.is_null()
        && tst != elem
        && (*tst).prefix == (*elem).prefix
        && !matches!((*tst).etype, XmlElementTypeVal::XmlElementTypeUndefined)
    {
        let elem_name = elem_name.as_deref().unwrap().to_string_lossy();
        xml_err_valid_node(
            ctxt,
            elem as XmlNodePtr,
            XmlParserErrors::XmlDTDElemRedefined,
            format!("Redefinition of element {elem_name}\n").as_str(),
            Some(&elem_name),
            None,
            None,
        );
        ret = 0;
    }
    tst = xml_get_dtd_element_desc(
        (*doc).ext_subset,
        elem_name
            .as_ref()
            .map_or(null(), |n| n.as_ptr() as *const u8),
    );
    if !tst.is_null()
        && tst != elem
        && (*tst).prefix == (*elem).prefix
        && !matches!((*tst).etype, XmlElementTypeVal::XmlElementTypeUndefined)
    {
        let elem_name = elem_name.as_deref().unwrap().to_string_lossy();
        xml_err_valid_node(
            ctxt,
            elem as XmlNodePtr,
            XmlParserErrors::XmlDTDElemRedefined,
            format!("Redefinition of element {elem_name}\n").as_str(),
            Some(&elem_name),
            None,
            None,
        );
        ret = 0;
    }
    /* One ID per Element Type
     * already done when registering the attribute
    if (xmlScanIDAttributeDecl(ctxt, elem) > 1) {
        ret = 0;
    } */
    ret
}

/// Does the validation related extra step of the normalization of attribute values:
///
/// If the declared value is not CDATA, then the XML processor must further
/// process the normalized attribute value by discarding any leading and
/// trailing space (#x20) characters, and by replacing sequences of space
/// (#x20) characters by single space (#x20) character.
///
/// Returns a new normalized string if normalization is needed, null_mut() otherwise
/// the caller must free the returned value.
#[doc(alias = "xmlValidNormalizeAttributeValue")]
#[cfg(feature = "libxml_valid")]
pub unsafe fn xml_valid_normalize_attribute_value(
    doc: XmlDocPtr,
    elem: XmlNodePtr,
    name: &str,
    value: *const XmlChar,
) -> *mut XmlChar {
    let mut attr_decl: XmlAttributePtr;

    if doc.is_null() {
        return null_mut();
    }
    if elem.is_null() {
        return null_mut();
    }
    if value.is_null() {
        return null_mut();
    }

    if !(*elem).ns.is_null() && !(*(*elem).ns).prefix.is_null() {
        let mut fname: [XmlChar; 50] = [0; 50];

        let fullname: *mut XmlChar = xml_build_qname(
            (*elem).name,
            (*(*elem).ns).prefix as _,
            fname.as_mut_ptr(),
            50,
        );
        if fullname.is_null() {
            return null_mut();
        }
        if fullname != fname.as_ptr() as _ && fullname != (*elem).name as _ {
            xml_free(fullname as _);
        }
    }
    attr_decl = (*(*doc).int_subset).get_attr_desc((*elem).name().as_deref().unwrap(), name);
    if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
        attr_decl = (*(*doc).ext_subset).get_attr_desc((*elem).name().as_deref().unwrap(), name);
    }

    if attr_decl.is_null() {
        return null_mut();
    }
    if matches!((*attr_decl).atype, XmlAttributeType::XmlAttributeCDATA) {
        return null_mut();
    }

    let ret: *mut XmlChar = xml_strdup(value);
    if ret.is_null() {
        return null_mut();
    }
    xml_valid_normalize_string(ret);
    ret
}

/// Does the validation related extra step of the normalization of attribute values:
///
/// If the declared value is not CDATA, then the XML processor must further
/// process the normalized attribute value by discarding any leading and
/// trailing space (#x20) characters, and by replacing sequences of space
/// (#x20) characters by single space (#x20) character.
///
/// Also  check VC: Standalone Document Declaration in P32, and update
///  (*ctxt).valid accordingly
///
/// Returns a new normalized string if normalization is needed, null_mut() otherwise
/// the caller must free the returned value.
#[doc(alias = "xmlValidCtxtNormalizeAttributeValue")]
#[cfg(feature = "libxml_valid")]
pub unsafe fn xml_valid_ctxt_normalize_attribute_value(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    elem: XmlNodePtr,
    name: &str,
    value: *const XmlChar,
) -> *mut XmlChar {
    let mut attr_decl: XmlAttributePtr = null_mut();
    let mut extsubset: i32 = 0;

    if doc.is_null() {
        return null_mut();
    }
    if elem.is_null() {
        return null_mut();
    }
    if value.is_null() {
        return null_mut();
    }

    if !(*elem).ns.is_null() && !(*(*elem).ns).prefix.is_null() {
        let mut fname: [XmlChar; 50] = [0; 50];

        let fullname: *mut XmlChar = xml_build_qname(
            (*elem).name,
            (*(*elem).ns).prefix as _,
            fname.as_mut_ptr(),
            50,
        );
        if fullname.is_null() {
            return null_mut();
        }
        attr_decl = (*(*doc).int_subset).get_attr_desc(
            CStr::from_ptr(fullname as *const i8)
                .to_string_lossy()
                .as_ref(),
            name,
        );
        if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
            attr_decl = (*(*doc).ext_subset).get_attr_desc(
                CStr::from_ptr(fullname as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                name,
            );
            if !attr_decl.is_null() {
                extsubset = 1;
            }
        }
        if fullname != fname.as_ptr() as _ && fullname != (*elem).name as _ {
            xml_free(fullname as _);
        }
    }
    if attr_decl.is_null() && !(*doc).int_subset.is_null() {
        attr_decl = (*(*doc).int_subset).get_attr_desc((*elem).name().as_deref().unwrap(), name);
    }
    if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
        attr_decl = (*(*doc).ext_subset).get_attr_desc((*elem).name().as_deref().unwrap(), name);
        if !attr_decl.is_null() {
            extsubset = 1;
        }
    }

    if attr_decl.is_null() {
        return null_mut();
    }
    if matches!((*attr_decl).atype, XmlAttributeType::XmlAttributeCDATA) {
        return null_mut();
    }

    let ret: *mut XmlChar = xml_strdup(value);
    if ret.is_null() {
        return null_mut();
    }
    xml_valid_normalize_string(ret);
    if (*doc).standalone != 0 && extsubset == 1 && !xml_str_equal(value, ret) {
        let elem_name = (*elem).name().unwrap();
        xml_err_valid_node(
            ctxt,
            elem,
            XmlParserErrors::XmlDTDNotStandalone,
            format!("standalone: {name} on {elem_name} value had to be normalized based on external subset declaration\n").as_str(),
            Some(name),
            Some(&elem_name),
            None
        );
        (*ctxt).valid = 0;
    }
    ret
}

extern "C" fn xml_validate_attribute_id_callback(attr: XmlAttributePtr, count: *mut i32) {
    unsafe {
        if matches!((*attr).atype, XmlAttributeType::XmlAttributeID) {
            (*count) += 1;
        }
    }
}

/// Handle a validation error, provide contextual information
#[doc(alias = "xmlErrValidNodeNr")]
macro_rules! xml_err_valid_node_nr {
    ($ctxt:expr, $node:expr, $error:expr, $msg:literal, $str1:expr, $int2:expr, $str3:expr) => {
        let ctxt = $ctxt as *mut XmlValidCtxt;
        let schannel: Option<StructuredError> = None;
        let mut channel: Option<GenericError> = None;
        let mut pctxt: XmlParserCtxtPtr = null_mut();
        let mut data = None;

        if !ctxt.is_null() {
            channel = (*ctxt).error;
            data = (*ctxt).user_data.clone();
            // Look up flag to detect if it is part of a parsing context
            if (*ctxt).flags & XML_VCTXT_USE_PCTXT as u32 != 0 {
                pctxt = (*ctxt)
                    .user_data
                    .as_ref()
                    .and_then(|d| {
                        let lock = d.lock();
                        lock.downcast_ref::<XmlParserCtxtPtr>().copied()
                    })
                    .unwrap_or(null_mut());
            }
        }
        __xml_raise_error!(
            schannel,
            channel,
            data,
            pctxt as _,
            $node as _,
            XmlErrorDomain::XmlFromValid,
            $error,
            XmlErrorLevel::XmlErrError,
            None,
            0,
            Some($str1.to_owned().into()),
            Some($str3.to_owned().into()),
            None,
            $int2,
            0,
            format!($msg, $str1, $int2, $str3).as_str(),
        );
    };
}

/// Try to validate a single attribute definition
/// basically it does the following checks as described by the
/// XML-1.0 recommendation:
///  - [ VC: Attribute Default Legal ]
///  - [ VC: Enumeration ]
///  - [ VC: ID Attribute Default ]
///
/// The ID/IDREF uniqueness and matching are done separately
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateAttributeDecl")]
#[cfg(feature = "libxml_valid")]
pub unsafe extern "C" fn xml_validate_attribute_decl(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    attr: XmlAttributePtr,
) -> i32 {
    let mut ret: i32 = 1;
    let val: i32;
    CHECK_DTD!(doc);
    if attr.is_null() {
        return 1;
    }

    let attr_elem = (*attr).elem.as_deref().map(|e| CString::new(e).unwrap());
    // Attribute Default Legal
    // Enumeration
    if !(*attr).default_value.is_null() {
        val = xml_validate_attribute_value_internal(doc, (*attr).atype, (*attr).default_value);
        if val == 0 {
            let attr_name = (*attr).name().unwrap();
            let attr_elem = attr_elem.as_deref().unwrap().to_string_lossy();
            xml_err_valid_node(
                ctxt,
                attr as XmlNodePtr,
                XmlParserErrors::XmlDTDAttributeDefault,
                format!("Syntax of default value for attribute {attr_name} of {attr_elem} is not valid\n")
                    .as_str(),
                Some(&attr_name),
                Some(&attr_elem),
                None,
            );
        }
        ret &= val;
    }

    // ID Attribute Default
    if matches!((*attr).atype, XmlAttributeType::XmlAttributeID)
        && !matches!(
            (*attr).def,
            XmlAttributeDefault::XmlAttributeImplied | XmlAttributeDefault::XmlAttributeRequired
        )
    {
        let attr_name = (*attr).name().unwrap();
        let attr_elem = attr_elem.as_deref().unwrap().to_string_lossy();
        xml_err_valid_node(
            ctxt,
            attr as XmlNodePtr,
            XmlParserErrors::XmlDTDIDFixed,
            format!("ID attribute {attr_name} of {attr_elem} is not valid must be #IMPLIED or #REQUIRED\n").as_str(),
            Some(&attr_name),
            Some(&attr_elem),
            None,
        );
        ret = 0;
    }

    // One ID per Element Type
    if matches!((*attr).atype, XmlAttributeType::XmlAttributeID) {
        let mut nb_id: i32;

        // the trick is that we parse DtD as their own internal subset
        let mut elem: XmlElementPtr = xml_get_dtd_element_desc(
            (*doc).int_subset,
            attr_elem
                .as_ref()
                .map_or(null(), |e| e.as_ptr() as *const u8),
        );
        if !elem.is_null() {
            nb_id = xml_scan_id_attribute_decl(null_mut(), elem, 0);
        } else {
            // The attribute may be declared in the internal subset and the
            // element in the external subset.
            nb_id = 0;
            if !(*doc).int_subset.is_null() {
                if let Some(table) = (*(*doc).int_subset).attributes {
                    table.scan(|&payload, _, _, name3| {
                        if !payload.is_null() && name3.map(|n| n.as_ref()) == attr_elem.as_deref() {
                            xml_validate_attribute_id_callback(payload, &raw mut nb_id);
                        }
                    });
                }
            }
        }
        if nb_id > 1 {
            xml_err_valid_node_nr!(
                ctxt,
                attr as XmlNodePtr,
                XmlParserErrors::XmlDTDIDSubset,
                "Element {} has {} ID attribute defined in the internal subset : {}\n",
                attr_elem.as_deref().unwrap().to_string_lossy().into_owned(),
                nb_id,
                (*attr).name().unwrap()
            );
        } else if !(*doc).ext_subset.is_null() {
            let mut ext_id: i32 = 0;
            elem = xml_get_dtd_element_desc(
                (*doc).ext_subset,
                attr_elem
                    .as_ref()
                    .map_or(null(), |e| e.as_ptr() as *const u8),
            );
            if !elem.is_null() {
                ext_id = xml_scan_id_attribute_decl(null_mut(), elem, 0);
            }
            if ext_id > 1 {
                xml_err_valid_node_nr!(
                    ctxt,
                    attr as XmlNodePtr,
                    XmlParserErrors::XmlDTDIDSubset,
                    "Element {} has {} ID attribute defined in the external subset : {}\n",
                    attr_elem.as_deref().unwrap().to_string_lossy().into_owned(),
                    ext_id,
                    (*attr).name().unwrap()
                );
            } else if ext_id + nb_id > 1 {
                let attr_elem = attr_elem.as_deref().unwrap().to_string_lossy();
                let attr_name = (*attr).name().unwrap();
                xml_err_valid_node(
                    ctxt,
                    attr as XmlNodePtr,
                    XmlParserErrors::XmlDTDIDSubset,
                    format!("Element {attr_elem} has ID attributes defined in the internal and external subset : {attr_name}\n").as_str(),
                    Some(&attr_elem),
                    Some(&attr_name),
                    None
                );
            }
        }
    }

    // Validity Constraint: Enumeration
    if !(*attr).default_value.is_null() && !(*attr).tree.is_null() {
        let mut tree: XmlEnumerationPtr = (*attr).tree;
        while !tree.is_null() {
            if (*tree).name.as_deref()
                == Some(CStr::from_ptr((*attr).default_value as *const i8).to_string_lossy())
                    .as_deref()
            {
                break;
            }
            tree = (*tree).next;
        }
        if tree.is_null() {
            let attr_name = (*attr).name().unwrap();
            let attr_elem = attr_elem.as_deref().unwrap().to_string_lossy();
            let attr_def = CStr::from_ptr((*attr).default_value as *const i8).to_string_lossy();
            xml_err_valid_node(
                ctxt,
                attr as XmlNodePtr,
                XmlParserErrors::XmlDTDAttributeValue,
                format!(
                    "Default value \"{attr_def}\" for attribute {attr_name} of {attr_elem} is not among the enumerated set\n"
                )
                .as_str(),
                Some(&attr_def),
                Some(&attr_name),
                Some(&attr_elem),
            );
            ret = 0;
        }
    }

    ret
}

/// Validate that the given attribute value match  the proper production
///
/// `[ VC: ID ]`  
/// Values of type ID must match the Name production....
///
/// `[ VC: IDREF ]`  
/// Values of type IDREF must match the Name production, and values
/// of type IDREFS must match Names ...
///
/// `[ VC: Entity Name ]`  
/// Values of type ENTITY must match the Name production, values
/// of type ENTITIES must match Names ...
///
/// `[ VC: Name Token ]`
/// Values of type NMTOKEN must match the Nmtoken production; values
/// of type NMTOKENS must match Nmtokens.
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateAttributeValue")]
#[cfg(feature = "libxml_valid")]
pub unsafe extern "C" fn xml_validate_attribute_value(
    typ: XmlAttributeType,
    value: *const XmlChar,
) -> i32 {
    xml_validate_attribute_value_internal(null_mut(), typ, value)
}

/// Try to validate a single notation definition
/// basically it does the following checks as described by the
/// XML-1.0 recommendation:
///  - it seems that no validity constraint exists on notation declarations
///    But this function get called anyway ...
///
/// Returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateNotationDecl")]
#[cfg(feature = "libxml_valid")]
pub unsafe extern "C" fn xml_validate_notation_decl(
    _ctxt: XmlValidCtxtPtr,
    _doc: XmlDocPtr,
    _nota: XmlNotationPtr,
) -> i32 {
    let ret: i32 = 1;

    ret
}

/// Try to validate the document against the dtd instance
///
/// Basically it does check all the definitions in the DtD.
/// Note the the internal subset (if present) is de-coupled
/// (i.e. not used), which could give problems if ID or IDREF is present.
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateDtd")]
#[cfg(feature = "libxml_valid")]
pub unsafe extern "C" fn xml_validate_dtd(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    dtd: XmlDtdPtr,
) -> i32 {
    let mut ret: i32;

    if dtd.is_null() {
        return 0;
    }
    if doc.is_null() {
        return 0;
    }
    let old_ext: XmlDtdPtr = (*doc).ext_subset;
    let old_int: XmlDtdPtr = (*doc).int_subset;
    (*doc).ext_subset = dtd;
    (*doc).int_subset = null_mut();
    ret = xml_validate_root(ctxt, doc);
    if ret == 0 {
        (*doc).ext_subset = old_ext;
        (*doc).int_subset = old_int;
        return ret;
    }
    if !(*doc).ids.is_null() {
        xml_free_id_table((*doc).ids as _);
        (*doc).ids = null_mut();
    }
    if !(*doc).refs.is_null() {
        xml_free_ref_table((*doc).refs as _);
        (*doc).refs = null_mut();
    }
    let root: XmlNodePtr = (*doc).get_root_element();
    ret = xml_validate_element(ctxt, doc, root);
    ret &= xml_validate_document_final(ctxt, doc);
    (*doc).ext_subset = old_ext;
    (*doc).int_subset = old_int;
    ret
}

/// Validate that the given attribute value match a given type.
/// This typically cannot be done before having finished parsing the subsets.
///
/// `[ VC: IDREF ]`  
/// Values of type IDREF must match one of the declared IDs
/// Values of type IDREFS must match a sequence of the declared IDs
/// each Name must match the value of an ID attribute on some element
/// in the XML document; i.e. IDREF values must match the value of some ID attribute
///
/// `[ VC: Entity Name ]`  
/// Values of type ENTITY must match one declared entity
/// Values of type ENTITIES must match a sequence of declared entities
///
/// `[ VC: Notation Attributes ]`  
/// All notation names in the declaration must be declared.
///
/// Returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateAttributeValue2")]
unsafe fn xml_validate_attribute_value2(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    name: *const XmlChar,
    typ: XmlAttributeType,
    value: &str,
) -> i32 {
    let mut ret: i32 = 1;
    match typ {
        XmlAttributeType::XmlAttributeIDREFS
        | XmlAttributeType::XmlAttributeIDREF
        | XmlAttributeType::XmlAttributeID
        | XmlAttributeType::XmlAttributeNmtokens
        | XmlAttributeType::XmlAttributeEnumeration
        | XmlAttributeType::XmlAttributeNmtoken
        | XmlAttributeType::XmlAttributeCDATA => {}
        XmlAttributeType::XmlAttributeEntity => {
            let mut ent = xml_get_doc_entity(doc, value);
            /* yeah it's a bit messy... */
            if ent.is_null() && (*doc).standalone == 1 {
                (*doc).standalone = 0;
                ent = xml_get_doc_entity(doc, value);
            }
            if ent.is_null() {
                let name = CStr::from_ptr(name as *const i8).to_string_lossy();
                xml_err_valid_node(
                    ctxt,
                    doc as XmlNodePtr,
                    XmlParserErrors::XmlDTDUnknownEntity,
                    format!("ENTITY attribute {name} reference an unknown entity \"{value}\"\n")
                        .as_str(),
                    Some(&name),
                    Some(value),
                    None,
                );
                ret = 0;
            } else if !matches!(
                (*ent).etype,
                XmlEntityType::XmlExternalGeneralUnparsedEntity
            ) {
                let name = CStr::from_ptr(name as *const i8).to_string_lossy();
                xml_err_valid_node(
                    ctxt,
                    doc as XmlNodePtr,
                    XmlParserErrors::XmlDTDEntityType,
                    format!(
                        "ENTITY attribute {name} reference an entity \"{value}\" of wrong type\n"
                    )
                    .as_str(),
                    Some(&name),
                    Some(value),
                    None,
                );
                ret = 0;
            }
        }
        XmlAttributeType::XmlAttributeEntities => {
            let mut cur: *mut XmlChar;
            let mut save: XmlChar;
            let mut ent: XmlEntityPtr;
            let value = CString::new(value).unwrap();
            let value = value.as_ptr() as *const u8;

            let dup: *mut XmlChar = xml_strdup(value);
            if dup.is_null() {
                return 0;
            }
            cur = dup;
            while *cur != 0 {
                let nam = cur;
                while *cur != 0 && !xml_is_blank_char(*cur as u32) {
                    cur = cur.add(1);
                }
                save = *cur;
                *cur = 0;
                let nam = CStr::from_ptr(nam as *const i8).to_string_lossy();
                ent = xml_get_doc_entity(doc, &nam);
                if ent.is_null() {
                    let name = CStr::from_ptr(name as *const i8).to_string_lossy();
                    xml_err_valid_node(
                        ctxt,
                        doc as XmlNodePtr,
                        XmlParserErrors::XmlDTDUnknownEntity,
                        format!(
                            "ENTITIES attribute {name} reference an unknown entity \"{nam}\"\n"
                        )
                        .as_str(),
                        Some(&name),
                        Some(&nam),
                        None,
                    );
                    ret = 0;
                } else if !matches!(
                    (*ent).etype,
                    XmlEntityType::XmlExternalGeneralUnparsedEntity
                ) {
                    let name = CStr::from_ptr(name as *const i8).to_string_lossy();
                    xml_err_valid_node(
                        ctxt,
                        doc as XmlNodePtr,
                        XmlParserErrors::XmlDTDEntityType,
                        format!("ENTITIES attribute {name} reference an entity \"{nam}\" of wrong type\n")
                            .as_str(),
                        Some(&name),
                        Some(&nam),
                        None,
                    );
                    ret = 0;
                }
                if save == 0 {
                    break;
                }
                *cur = save;
                while xml_is_blank_char(*cur as u32) {
                    cur = cur.add(1);
                }
            }
            xml_free(dup as _);
        }
        XmlAttributeType::XmlAttributeNotation => {
            let mut nota: XmlNotationPtr;
            let cvalue = CString::new(value).unwrap();
            let cvalue = cvalue.as_ptr() as *const u8;

            nota = xml_get_dtd_notation_desc((*doc).int_subset, cvalue);
            if nota.is_null() && !(*doc).ext_subset.is_null() {
                nota = xml_get_dtd_notation_desc((*doc).ext_subset, cvalue);
            }

            if nota.is_null() {
                let name = CStr::from_ptr(name as *const i8).to_string_lossy();
                xml_err_valid_node(
                    ctxt,
                    doc as XmlNodePtr,
                    XmlParserErrors::XmlDTDUnknownNotation,
                    format!(
                        "NOTATION attribute {name} reference an unknown notation \"{value}\"\n"
                    )
                    .as_str(),
                    Some(&name),
                    Some(value),
                    None,
                );
                ret = 0;
            }
        }
    }
    ret
}

extern "C" fn xml_validate_attribute_callback(cur: XmlAttributePtr, ctxt: XmlValidCtxtPtr) {
    let mut ret: i32;
    let doc: XmlDocPtr;
    let mut elem: XmlElementPtr = null_mut();

    if cur.is_null() {
        return;
    }
    unsafe {
        match (*cur).atype {
            XmlAttributeType::XmlAttributeCDATA
            | XmlAttributeType::XmlAttributeID
            | XmlAttributeType::XmlAttributeIDREF
            | XmlAttributeType::XmlAttributeIDREFS
            | XmlAttributeType::XmlAttributeNmtoken
            | XmlAttributeType::XmlAttributeNmtokens
            | XmlAttributeType::XmlAttributeEnumeration => {}
            XmlAttributeType::XmlAttributeEntity
            | XmlAttributeType::XmlAttributeEntities
            | XmlAttributeType::XmlAttributeNotation => {
                if !(*cur).default_value.is_null() {
                    ret = xml_validate_attribute_value2(
                        ctxt,
                        (*ctxt).doc,
                        (*cur).name,
                        (*cur).atype,
                        &CStr::from_ptr((*cur).default_value as *const i8).to_string_lossy(),
                    );
                    if ret == 0 && (*ctxt).valid == 1 {
                        (*ctxt).valid = 0;
                    }
                }
                if !(*cur).tree.is_null() {
                    let mut tree: XmlEnumerationPtr = (*cur).tree;
                    while !tree.is_null() {
                        ret = xml_validate_attribute_value2(
                            ctxt,
                            (*ctxt).doc,
                            (*cur).name,
                            (*cur).atype,
                            (*tree).name.as_deref().unwrap(),
                        );
                        if ret == 0 && (*ctxt).valid == 1 {
                            (*ctxt).valid = 0;
                        }
                        tree = (*tree).next;
                    }
                }
            }
        }
        if matches!((*cur).atype, XmlAttributeType::XmlAttributeNotation) {
            doc = (*cur).doc;
            let Some(cur_elem) = (*cur).elem.as_deref().map(|e| CString::new(e).unwrap()) else {
                xml_err_valid!(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    "xmlValidateAttributeCallback({}): internal error\n",
                    (*cur).name().as_deref().unwrap()
                );
                return;
            };

            if !doc.is_null() {
                elem = xml_get_dtd_element_desc((*doc).int_subset, cur_elem.as_ptr() as *const u8);
            }
            if elem.is_null() && !doc.is_null() {
                elem = xml_get_dtd_element_desc((*doc).ext_subset, cur_elem.as_ptr() as *const u8);
            }
            if elem.is_null()
                && !(*cur).parent.is_null()
                && matches!((*(*cur).parent).element_type(), XmlElementType::XmlDTDNode)
            {
                elem = xml_get_dtd_element_desc(
                    (*(*cur).parent).as_dtd_node().unwrap().as_ptr(),
                    cur_elem.as_ptr() as *const u8,
                );
            }
            if elem.is_null() {
                let name = (*cur).name().unwrap();
                let cur_elem = cur_elem.to_string_lossy();
                xml_err_valid_node(
                    ctxt,
                    null_mut(),
                    XmlParserErrors::XmlDTDUnknownElem,
                    format!("attribute {name}: could not find decl for element {cur_elem}\n")
                        .as_str(),
                    Some(&name),
                    Some(&cur_elem),
                    None,
                );
                return;
            }
            if matches!((*elem).etype, XmlElementTypeVal::XmlElementTypeEmpty) {
                let name = (*cur).name().unwrap();
                let cur_elem = cur_elem.to_string_lossy();
                xml_err_valid_node(
                    ctxt,
                    null_mut(),
                    XmlParserErrors::XmlDTDEmptyNotation,
                    format!("NOTATION attribute {name} declared for EMPTY element {cur_elem}\n")
                        .as_str(),
                    Some(&name),
                    Some(&cur_elem),
                    None,
                );
                (*ctxt).valid = 0;
            }
        }
    }
}

extern "C" fn xml_validate_notation_callback(cur: XmlEntityPtr, ctxt: XmlValidCtxtPtr) {
    if cur.is_null() {
        return;
    }
    unsafe {
        if matches!(
            (*cur).etype,
            XmlEntityType::XmlExternalGeneralUnparsedEntity
        ) {
            let notation: *mut XmlChar = (*cur).content.load(Ordering::Relaxed) as _;

            if !notation.is_null() {
                let ret: i32 = xml_validate_notation_use(
                    ctxt,
                    (*cur).doc.load(Ordering::Relaxed) as _,
                    notation,
                );
                if ret != 1 {
                    (*ctxt).valid = 0;
                }
            }
        }
    }
}

/// Does the final step for the dtds validation once all the subsets have been parsed
///
/// basically it does the following checks described by the XML Rec
/// - check that ENTITY and ENTITIES type attributes default or
///   possible values matches one of the defined entities.
/// - check that NOTATION type attributes default or
///   possible values matches one of the defined notations.
///
/// Returns 1 if valid or 0 if invalid and -1 if not well-formed
#[doc(alias = "xmlValidateDtdFinal")]
#[cfg(feature = "libxml_valid")]
pub unsafe extern "C" fn xml_validate_dtd_final(ctxt: XmlValidCtxtPtr, doc: XmlDocPtr) -> i32 {
    let mut dtd: XmlDtdPtr;

    if doc.is_null() || ctxt.is_null() {
        return 0;
    }
    if (*doc).int_subset.is_null() && (*doc).ext_subset.is_null() {
        return 0;
    }
    (*ctxt).doc = doc;
    (*ctxt).valid = 1;
    dtd = (*doc).int_subset;
    if !dtd.is_null() {
        if let Some(table) = (*dtd).attributes {
            table.scan(|&payload, _, _, _| {
                xml_validate_attribute_callback(payload, ctxt);
            });
        }
    }
    if !dtd.is_null() {
        if let Some(entities) = (*dtd).entities {
            entities.scan(|payload, _, _, _| {
                xml_validate_notation_callback(*payload, ctxt);
            });
        }
    }
    dtd = (*doc).ext_subset;
    if !dtd.is_null() {
        if let Some(table) = (*dtd).attributes {
            table.scan(|payload, _, _, _| {
                xml_validate_attribute_callback(*payload, ctxt);
            });
        }
    }
    if !dtd.is_null() {
        if let Some(entities) = (*dtd).entities {
            entities.scan(|entity, _, _, _| {
                xml_validate_notation_callback(*entity, ctxt);
            });
        }
    }
    (*ctxt).valid
}

/// Try to validate the document instance
///
/// basically it does the all the checks described by the XML Rec
/// i.e. validates the internal and external subset (if present)
/// and validate the document tree.
///
/// Returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateDocument")]
#[cfg(feature = "libxml_valid")]
pub unsafe fn xml_validate_document(ctxt: XmlValidCtxtPtr, doc: XmlDocPtr) -> i32 {
    use crate::{libxml::parser::xml_parse_dtd, uri::build_uri};

    let mut ret: i32;

    if doc.is_null() {
        return 0;
    }
    if (*doc).int_subset.is_null() && (*doc).ext_subset.is_null() {
        xml_err_valid!(ctxt, XmlParserErrors::XmlDTDNoDTD, "no DTD found!\n");
        return 0;
    }
    if !(*doc).int_subset.is_null()
        && ((*(*doc).int_subset).system_id.is_some() || (*(*doc).int_subset).external_id.is_some())
        && (*doc).ext_subset.is_null()
    {
        let sys_id = if let Some(system_id) = (*(*doc).int_subset).system_id.as_deref() {
            let Some(sys_id) = (*doc)
                .url
                .as_deref()
                .and_then(|base| build_uri(system_id, base))
            else {
                xml_err_valid!(
                    ctxt,
                    XmlParserErrors::XmlDTDLoadError,
                    "Could not build URI for external subset \"{}\"\n",
                    system_id
                );
                return 0;
            };
            Some(sys_id)
        } else {
            None
        };
        let external_id = (*(*doc).int_subset).external_id.as_deref();
        (*doc).ext_subset = xml_parse_dtd(external_id, sys_id.as_deref());
        if (*doc).ext_subset.is_null() {
            if let Some(system_id) = (*(*doc).int_subset).system_id.as_deref() {
                xml_err_valid!(
                    ctxt,
                    XmlParserErrors::XmlDTDLoadError,
                    "Could not load the external subset \"{}\"\n",
                    system_id
                );
            } else {
                xml_err_valid!(
                    ctxt,
                    XmlParserErrors::XmlDTDLoadError,
                    "Could not load the external subset \"{}\"\n",
                    external_id.unwrap()
                );
            }
            return 0;
        }
    }

    if !(*doc).ids.is_null() {
        xml_free_id_table((*doc).ids as _);
        (*doc).ids = null_mut();
    }
    if !(*doc).refs.is_null() {
        xml_free_ref_table((*doc).refs as _);
        (*doc).refs = null_mut();
    }
    ret = xml_validate_dtd_final(ctxt, doc);
    if xml_validate_root(ctxt, doc) == 0 {
        return 0;
    }

    let root: XmlNodePtr = (*doc).get_root_element();
    ret &= xml_validate_element(ctxt, doc, root);
    ret &= xml_validate_document_final(ctxt, doc);
    ret
}

/// Try to validate the subtree under an element
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateElement")]
#[cfg(feature = "libxml_valid")]
pub unsafe extern "C" fn xml_validate_element(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    root: XmlNodePtr,
) -> i32 {
    let mut elem: XmlNodePtr;
    let mut attr: XmlAttrPtr;
    let mut ns: XmlNsPtr;
    let mut ret: i32 = 1;

    if root.is_null() {
        return 0;
    }

    CHECK_DTD!(doc);

    elem = root;
    loop {
        ret &= xml_validate_one_element(ctxt, doc, elem);

        if matches!((*elem).element_type(), XmlElementType::XmlElementNode) {
            attr = (*elem).properties;
            while !attr.is_null() {
                let value = (*attr)
                    .children
                    .and_then(|c| c.get_string(doc, 0))
                    .map(|c| CString::new(c).unwrap());
                ret &= xml_validate_one_attribute(
                    ctxt,
                    doc,
                    elem,
                    attr,
                    value
                        .as_ref()
                        .map_or(null_mut(), |c| c.as_ptr() as *const u8),
                );
                attr = (*attr).next;
            }

            ns = (*elem).ns_def;
            while !ns.is_null() {
                if (*elem).ns.is_null() {
                    ret &= xml_validate_one_namespace(ctxt, doc, elem, null_mut(), ns, (*ns).href);
                } else {
                    ret &= xml_validate_one_namespace(
                        ctxt,
                        doc,
                        elem,
                        (*(*elem).ns).prefix,
                        ns,
                        (*ns).href,
                    );
                }
                ns = (*ns).next;
            }

            if let Some(children) = (*elem).children() {
                elem = children.as_ptr();
                continue;
            }
        }

        loop {
            if elem == root {
                // goto done;
                return ret;
            }
            if (*elem).next.is_some() {
                break;
            }
            elem = (*elem).parent().map_or(null_mut(), |p| p.as_ptr());
        }
        elem = (*elem).next.map_or(null_mut(), |n| n.as_ptr());
    }

    // done:
    // return ret;
}

/// Finds a declaration associated to an element in the document.
///
/// returns the pointer to the declaration or null_mut() if not found.
#[doc(alias = "xmlValidGetElemDecl")]
unsafe extern "C" fn xml_valid_get_elem_decl(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    elem: XmlNodePtr,
    extsubset: *mut i32,
) -> XmlElementPtr {
    let mut elem_decl: XmlElementPtr = null_mut();
    let mut prefix: *const XmlChar = null_mut();

    if ctxt.is_null() || doc.is_null() || elem.is_null() || (*elem).name.is_null() {
        return null_mut();
    }
    if !extsubset.is_null() {
        *extsubset = 0;
    }

    /*
     * Fetch the declaration for the qualified name
     */
    if !(*elem).ns.is_null() && !(*(*elem).ns).prefix.is_null() {
        prefix = (*(*elem).ns).prefix;
    }

    if !prefix.is_null() {
        elem_decl = xml_get_dtd_qelement_desc((*doc).int_subset, (*elem).name, prefix);
        if elem_decl.is_null() && !(*doc).ext_subset.is_null() {
            elem_decl = xml_get_dtd_qelement_desc((*doc).ext_subset, (*elem).name, prefix);
            if !elem_decl.is_null() && !extsubset.is_null() {
                *extsubset = 1;
            }
        }
    }

    /*
     * Fetch the declaration for the non qualified name
     * This is "non-strict" validation should be done on the
     * full QName but in that case being flexible makes sense.
     */
    if elem_decl.is_null() {
        elem_decl = xml_get_dtd_element_desc((*doc).int_subset, (*elem).name);
        if elem_decl.is_null() && !(*doc).ext_subset.is_null() {
            elem_decl = xml_get_dtd_element_desc((*doc).ext_subset, (*elem).name);
            if !elem_decl.is_null() && !extsubset.is_null() {
                *extsubset = 1;
            }
        }
    }
    if elem_decl.is_null() {
        let name = (*elem).name().unwrap();
        xml_err_valid_node(
            ctxt,
            elem,
            XmlParserErrors::XmlDTDUnknownElem,
            format!("No declaration for element {name}\n").as_str(),
            Some(&name),
            None,
            None,
        );
    }
    elem_decl
}

unsafe extern "C" fn node_vpush(ctxt: XmlValidCtxtPtr, value: XmlNodePtr) -> i32 {
    if (*ctxt).node_max <= 0 {
        (*ctxt).node_max = 4;
        (*ctxt).node_tab =
            xml_malloc((*ctxt).node_max as usize * size_of_val(&*(*ctxt).node_tab.add(0)))
                as *mut XmlNodePtr;
        if (*ctxt).node_tab.is_null() {
            xml_verr_memory(ctxt as _, Some("malloc failed"));
            (*ctxt).node_max = 0;
            return 0;
        }
    }
    if (*ctxt).node_nr >= (*ctxt).node_max {
        let tmp: *mut XmlNodePtr = xml_realloc(
            (*ctxt).node_tab as _,
            (*ctxt).node_max as usize * 2 * size_of_val(&*(*ctxt).node_tab.add(0)),
        ) as *mut XmlNodePtr;
        if tmp.is_null() {
            xml_verr_memory(ctxt as _, Some("realloc failed"));
            return 0;
        }
        (*ctxt).node_max *= 2;
        (*ctxt).node_tab = tmp;
    }
    *(*ctxt).node_tab.add((*ctxt).node_nr as usize) = value;
    (*ctxt).node = value;
    let res = (*ctxt).node_nr;
    (*ctxt).node_nr += 1;
    res
}

unsafe extern "C" fn node_vpop(ctxt: XmlValidCtxtPtr) -> XmlNodePtr {
    if (*ctxt).node_nr <= 0 {
        return null_mut();
    }
    (*ctxt).node_nr -= 1;
    if (*ctxt).node_nr > 0 {
        (*ctxt).node = *(*ctxt).node_tab.add((*ctxt).node_nr as usize - 1);
    } else {
        (*ctxt).node = null_mut();
    }
    let ret: XmlNodePtr = *(*ctxt).node_tab.add((*ctxt).node_nr as usize);
    *(*ctxt).node_tab.add((*ctxt).node_nr as usize) = null_mut();
    ret
}

/// Check that an element follows #CDATA
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateCdataElement")]
unsafe extern "C" fn xml_validate_one_cdata_element(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    elem: XmlNodePtr,
) -> i32 {
    let mut ret: i32 = 1;

    if ctxt.is_null()
        || doc.is_null()
        || elem.is_null()
        || !matches!((*elem).element_type(), XmlElementType::XmlElementNode)
    {
        return 0;
    }

    let child = (*elem).children();

    let mut cur = child;
    'done: while let Some(now) = cur {
        match now.element_type() {
            XmlElementType::XmlEntityRefNode => {
                // Push the current node to be able to roll back
                // and process within the entity
                if let Some(children) = now
                    .children()
                    .filter(|children| children.children().is_some())
                {
                    node_vpush(ctxt, now.as_ptr());
                    cur = children.children();
                    continue;
                }
            }
            XmlElementType::XmlCommentNode
            | XmlElementType::XmlPINode
            | XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode => {}
            _ => {
                ret = 0;
                // goto done;
                break 'done;
            }
        }
        /*
         * Switch to next element
         */
        cur = now.next;
        while cur.is_none() {
            cur = NodePtr::from_ptr(node_vpop(ctxt));
            let Some(now) = cur else {
                break;
            };
            cur = now.next;
        }
    }
    // done:
    (*ctxt).node_max = 0;
    (*ctxt).node_nr = 0;
    if !(*ctxt).node_tab.is_null() {
        xml_free((*ctxt).node_tab as _);
        (*ctxt).node_tab = null_mut();
    }
    ret
}

#[cfg(not(feature = "libxml_regexp"))]
macro_rules! DEBUG_VALID_MSG {
    ($m:expr) => {
        $crate::generic_error!("{}\n", $m);
    };
}

/// This will dump the list of elements to the buffer
/// Intended just for the debug routine
#[doc(alias = "xmlSnprintfElements")]
unsafe extern "C" fn xml_snprintf_elements(
    buf: *mut c_char,
    size: i32,
    node: XmlNodePtr,
    glob: i32,
) {
    let mut cur: XmlNodePtr;
    let mut len: i32;

    if node.is_null() {
        return;
    }
    if glob != 0 {
        strcat(buf, c"(".as_ptr() as _);
    }
    cur = node;
    while !cur.is_null() {
        len = strlen(buf) as _;
        if size - len < 50 {
            if size - len > 4 && *buf.add(len as usize - 1) != b'.' as i8 {
                strcat(buf, c" ...".as_ptr() as _);
            }
            return;
        }
        match (*cur).element_type() {
            XmlElementType::XmlElementNode => {
                if !(*cur).ns.is_null() && !(*(*cur).ns).prefix.is_null() {
                    if size - len < xml_strlen((*(*cur).ns).prefix as _) + 10 {
                        if size - len > 4 && *buf.add(len as usize - 1) != b'.' as i8 {
                            strcat(buf, c" ...".as_ptr() as _);
                        }
                        return;
                    }
                    strcat(buf, (*(*cur).ns).prefix as *mut c_char);
                    strcat(buf, c":".as_ptr() as _);
                }
                if size - len < xml_strlen((*cur).name) + 10 {
                    if size - len > 4 && *buf.add(len as usize - 1) != b'.' as i8 {
                        strcat(buf, c" ...".as_ptr() as _);
                    }
                    return;
                }
                strcat(buf, (*cur).name as *mut c_char);
                if (*cur).next.is_some() {
                    strcat(buf, c" ".as_ptr() as _);
                }
            }
            ty @ XmlElementType::XmlTextNode
            | ty @ XmlElementType::XmlCDATASectionNode
            | ty @ XmlElementType::XmlEntityRefNode => 'to_break: {
                if matches!(ty, XmlElementType::XmlTextNode) && (*cur).is_blank_node() {
                    break 'to_break;
                }
                strcat(buf, c"CDATA".as_ptr() as _);
                if (*cur).next.is_some() {
                    strcat(buf, c" ".as_ptr() as _);
                }
            }
            XmlElementType::XmlAttributeNode
            | XmlElementType::XmlDocumentNode
            | XmlElementType::XmlHTMLDocumentNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlNamespaceDecl => {
                strcat(buf, c"???".as_ptr() as _);
                if (*cur).next.is_some() {
                    strcat(buf, c" ".as_ptr() as _);
                }
            }
            XmlElementType::XmlEntityNode
            | XmlElementType::XmlPINode
            | XmlElementType::XmlDTDNode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlElementDecl
            | XmlElementType::XmlAttributeDecl
            | XmlElementType::XmlEntityDecl
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => {}
            _ => unreachable!(),
        }
        cur = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
    }
    if glob != 0 {
        strcat(buf, c")".as_ptr() as _);
    }
}

#[cfg(not(feature = "libxml_regexp"))]
const ROLLBACK_OR: usize = 0;
#[cfg(not(feature = "libxml_regexp"))]
const ROLLBACK_PARENT: usize = 1;

/// Try to validate the content model of an element internal function
///
/// returns 1 if valid or 0 ,-1 in case of error, -2 if an entity
/// reference is found and -3 if the validation succeeded but
/// the content model is not determinist.
#[doc(alias = "xmlValidateElementType")]
#[cfg(not(feature = "libxml_regexp"))]
unsafe extern "C" fn xmlValidateElementType(ctxt: XmlValidCtxtPtr) -> i32 {
    let mut ret: i32 = -1;
    let mut determinist: i32 = 1;

    (*(*ctxt).vstate).node = xmlValidateSkipIgnorable((*(*ctxt).vstate).node);
    if (*(*ctxt).vstate).node.is_null() && (*(*ctxt).vstate).cont.is_null() {
        return 1;
    }
    if (*(*ctxt).vstate).node.is_null()
        && matches!(
            (*(*(*ctxt).vstate).cont).ocur,
            XmlElementContentOccur::XmlElementContentMult
                | XmlElementContentOccur::XmlElementContentOpt
        )
    {
        return 1;
    }
    if (*(*ctxt).vstate).cont.is_null() {
        return -1;
    }
    if !(*(*ctxt).vstate).node.is_null()
        && matches!(
            (*(*(*ctxt).vstate).node).typ,
            XmlElementType::XmlEntityRefNode
        )
    {
        return -2;
    }

    /*
     * We arrive here when more states need to be examined
     */
    //  cont:
    'cont: loop {
        /*
         * We just recovered from a rollback generated by a possible
         * epsilon transition, go directly to the analysis phase
         */
        if (*(*ctxt).vstate).state == ROLLBACK_PARENT as u8 {
            DEBUG_VALID_MSG!(c"restored parent branch".as_ptr());
            ret = 1;
            // goto analyze;
        } else {
            /*
             * we may have to save a backup state here. This is the equivalent
             * of handling epsilon transition in NFAs.
             */
            if !(*(*ctxt).vstate).cont.is_null()
                && ((*(*(*ctxt).vstate).cont).parent.is_null()
                    || (*(*(*ctxt).vstate).cont).parent == 1 as XmlElementContentPtr
                    || ((*(*(*(*ctxt).vstate).cont).parent).typ
                        != XmlElementContentType::XmlElementContentOr))
                && (matches!(
                    (*(*(*ctxt).vstate).cont).ocur,
                    XmlElementContentOccur::XmlElementContentMult
                ) || matches!(
                    (*(*(*ctxt).vstate).cont).ocur,
                    XmlElementContentOccur::XmlElementContentOpt
                ) || (matches!(
                    (*(*(*ctxt).vstate).cont).ocur,
                    XmlElementContentOccur::XmlElementContentPlus
                ) && (*(*ctxt).vstate).occurs & (1 << (*(*ctxt).vstate).depth) != 0))
            {
                DEBUG_VALID_MSG!(c"saving parent branch".as_ptr());
                if vstate_vpush(
                    ctxt,
                    (*(*ctxt).vstate).cont,
                    (*(*ctxt).vstate).node,
                    (*(*ctxt).vstate).depth,
                    (*(*ctxt).vstate).occurs,
                    ROLLBACK_PARENT as _,
                ) < 0
                {
                    return 0;
                }
            }

            /*
             * Check first if the content matches
             */
            match (*(*(*ctxt).vstate).cont).typ {
                XmlElementContentType::XmlElementContentPCDATA => {
                    if (*(*ctxt).vstate).node.is_null() {
                        DEBUG_VALID_MSG!(c"pcdata failed no node".as_ptr());
                        ret = 0;
                        // break;
                    } else {
                        if matches!((*(*(*ctxt).vstate).node).typ, XmlElementType::XmlTextNode) {
                            DEBUG_VALID_MSG!(c"pcdata found, skip to next".as_ptr());
                            /*
                             * go to next element in the content model
                             * skipping ignorable elems
                             */
                            loop {
                                (*(*ctxt).vstate).node = (*(*(*ctxt).vstate).node).next;
                                (*(*ctxt).vstate).node =
                                    xmlValidateSkipIgnorable((*(*ctxt).vstate).node);
                                if !(*(*ctxt).vstate).node.is_null()
                                    && matches!(
                                        (*(*(*ctxt).vstate).node).typ,
                                        XmlElementType::XmlEntityRefNode
                                    )
                                {
                                    return -2;
                                }

                                if !(!(*(*ctxt).vstate).node.is_null()
                                    && (!matches!(
                                        (*(*(*ctxt).vstate).node).typ,
                                        XmlElementType::XmlElementNode
                                    ) && !matches!(
                                        (*(*(*ctxt).vstate).node).typ,
                                        XmlElementType::XmlTextNode
                                    ) && !matches!(
                                        (*(*(*ctxt).vstate).node).typ,
                                        XmlElementType::XmlCDATASectionNode
                                    )))
                                {
                                    break;
                                }
                            }
                            ret = 1;
                            // break;
                        } else {
                            DEBUG_VALID_MSG!(c"pcdata failed".as_ptr());
                            ret = 0;
                            // break;
                        }
                    }
                }
                XmlElementContentType::XmlElementContentElement => {
                    if (*(*ctxt).vstate).node.is_null() {
                        DEBUG_VALID_MSG!(c"element failed no node".as_ptr());
                        ret = 0;
                        // break;
                    } else {
                        ret = (matches!(
                            (*(*(*ctxt).vstate).node).typ,
                            XmlElementType::XmlElementNode
                        ) && xml_str_equal(
                            (*(*(*ctxt).vstate).node).name,
                            (*(*(*ctxt).vstate).cont).name,
                        ) != 0) as i32;
                        if ret == 1 {
                            if (*(*(*ctxt).vstate).node).ns.is_null()
                                || (*(*(*(*ctxt).vstate).node).ns)
                                    .prefix
                                    .load(Ordering::Relaxed)
                                    .is_null()
                            {
                                ret = (*(*(*ctxt).vstate).cont).prefix.is_null() as i32;
                            } else if (*(*(*ctxt).vstate).cont).prefix.is_null() {
                                ret = 0;
                            } else {
                                ret = (xml_str_equal(
                                    (*(*(*(*ctxt).vstate).node).ns)
                                        .prefix
                                        .load(Ordering::Relaxed)
                                        as _,
                                    (*(*(*ctxt).vstate).cont).prefix,
                                ) != 0) as i32;
                            }
                        }
                        if ret == 1 {
                            DEBUG_VALID_MSG!(c"element found, skip to next".as_ptr());
                            /*
                             * go to next element in the content model
                             * skipping ignorable elems
                             */
                            loop {
                                (*(*ctxt).vstate).node = (*(*(*ctxt).vstate).node).next;
                                (*(*ctxt).vstate).node =
                                    xmlValidateSkipIgnorable((*(*ctxt).vstate).node);
                                if !(*(*ctxt).vstate).node.is_null()
                                    && matches!(
                                        (*(*(*ctxt).vstate).node).typ,
                                        XmlElementType::XmlEntityRefNode
                                    )
                                {
                                    return -2;
                                }

                                if !(!(*(*ctxt).vstate).node.is_null()
                                    && (!matches!(
                                        (*(*(*ctxt).vstate).node).typ,
                                        XmlElementType::XmlElementNode
                                    ) && !matches!(
                                        (*(*(*ctxt).vstate).node).typ,
                                        XmlElementType::XmlTextNode
                                    ) && !matches!(
                                        (*(*(*ctxt).vstate).node).typ,
                                        XmlElementType::XmlCDATASectionNode
                                    )))
                                {
                                    break;
                                }
                            }
                        } else {
                            DEBUG_VALID_MSG!(c"element failed".as_ptr());
                            ret = 0;
                            // break;
                        }
                    }
                }
                XmlElementContentType::XmlElementContentOr => {
                    /*
                     * Small optimization.
                     */
                    if matches!(
                        (*(*(*(*ctxt).vstate).cont).c1).typ,
                        XmlElementContentType::XmlElementContentElement
                    ) {
                        if (*(*ctxt).vstate).node.is_null()
                            || xml_str_equal(
                                (*(*(*ctxt).vstate).node).name,
                                (*(*(*(*ctxt).vstate).cont).c1).name,
                            ) == 0
                        {
                            (*(*ctxt).vstate).depth += 1;
                            (*(*ctxt).vstate).cont = (*(*(*ctxt).vstate).cont).c2;
                            // goto cont;
                            continue 'cont;
                        }
                        if ((*(*(*ctxt).vstate).node).ns.is_null())
                            || (*(*(*(*ctxt).vstate).node).ns)
                                .prefix
                                .load(Ordering::Relaxed)
                                .is_null()
                        {
                            ret = (*(*(*(*ctxt).vstate).cont).c1).prefix.is_null() as i32;
                        } else if (*(*(*(*ctxt).vstate).cont).c1).prefix.is_null() {
                            ret = 0;
                        } else {
                            ret = (xml_str_equal(
                                (*(*(*(*ctxt).vstate).node).ns)
                                    .prefix
                                    .load(Ordering::Relaxed) as _,
                                (*(*(*(*ctxt).vstate).cont).c1).prefix,
                            ) != 0) as i32;
                        }
                        if ret == 0 {
                            (*(*ctxt).vstate).depth += 1;
                            (*(*ctxt).vstate).cont = (*(*(*ctxt).vstate).cont).c2;
                            // goto cont;
                            continue 'cont;
                        }
                    }

                    /*
                     * save the second branch 'or' branch
                     */
                    DEBUG_VALID_MSG!(c"saving 'or' branch".as_ptr());
                    if vstate_vpush(
                        ctxt,
                        (*(*(*ctxt).vstate).cont).c2,
                        (*(*ctxt).vstate).node,
                        (*(*ctxt).vstate).depth + 1,
                        (*(*ctxt).vstate).occurs,
                        ROLLBACK_OR as _,
                    ) < 0
                    {
                        return -1;
                    }
                    (*(*ctxt).vstate).depth += 1;
                    (*(*ctxt).vstate).cont = (*(*(*ctxt).vstate).cont).c1;
                    // goto cont;
                    continue 'cont;
                }
                XmlElementContentType::XmlElementContentSeq => {
                    /*
                     * Small optimization.
                     */
                    if matches!(
                        (*(*(*(*ctxt).vstate).cont).c1).typ,
                        XmlElementContentType::XmlElementContentElement
                    ) && (matches!(
                        (*(*(*(*ctxt).vstate).cont).c1).ocur,
                        XmlElementContentOccur::XmlElementContentOpt
                    ) || matches!(
                        (*(*(*(*ctxt).vstate).cont).c1).ocur,
                        XmlElementContentOccur::XmlElementContentMult
                    )) {
                        if ((*(*ctxt).vstate).node.is_null())
                            || xml_str_equal(
                                (*(*(*ctxt).vstate).node).name,
                                (*(*(*(*ctxt).vstate).cont).c1).name,
                            ) == 0
                        {
                            (*(*ctxt).vstate).depth += 1;
                            (*(*ctxt).vstate).cont = (*(*(*ctxt).vstate).cont).c2;
                            // goto cont;
                            continue 'cont;
                        }
                        if ((*(*(*ctxt).vstate).node).ns.is_null())
                            || ((*(*(*(*ctxt).vstate).node).ns)
                                .prefix
                                .load(Ordering::Relaxed)
                                .is_null())
                        {
                            ret = ((*(*(*(*ctxt).vstate).cont).c1).prefix.is_null()) as i32;
                        } else if (*(*(*(*ctxt).vstate).cont).c1).prefix.is_null() {
                            ret = 0;
                        } else {
                            ret = xml_str_equal(
                                (*(*(*(*ctxt).vstate).node).ns)
                                    .prefix
                                    .load(Ordering::Relaxed),
                                (*(*(*(*ctxt).vstate).cont).c1).prefix,
                            );
                        }
                        if ret == 0 {
                            (*(*ctxt).vstate).depth += 1;
                            (*(*ctxt).vstate).cont = (*(*(*ctxt).vstate).cont).c2;
                            // goto cont;
                            continue 'cont;
                        }
                    }
                    (*(*ctxt).vstate).depth += 1;
                    (*(*ctxt).vstate).cont = (*(*(*ctxt).vstate).cont).c1;
                    // goto cont;
                    continue 'cont;
                }
            }

            /*
             * At this point handle going up in the tree
             */
            if ret == -1 {
                DEBUG_VALID_MSG!(c"error found returning".as_ptr());
                return ret;
            }
        }
        // analyze:
        while !(*(*ctxt).vstate).cont.is_null() {
            /*
             * First do the analysis depending on the occurrence model at
             * this level.
             */
            if ret == 0 {
                let cur: XmlNodePtr;
                match (*(*(*ctxt).vstate).cont).ocur {
                    XmlElementContentOccur::XmlElementContentOnce => {
                        cur = (*(*ctxt).vstate).node;
                        DEBUG_VALID_MSG!(c"Once branch failed, rollback".as_ptr());
                        if vstate_vpop(ctxt) < 0 {
                            DEBUG_VALID_MSG!(c"exhaustion, failed".as_ptr());
                            return 0;
                        }
                        if cur != (*(*ctxt).vstate).node {
                            determinist = -3;
                        }
                        // goto cont;
                        continue 'cont;
                    }
                    XmlElementContentOccur::XmlElementContentPlus => {
                        if ((*(*ctxt).vstate).occurs & (1 << (*(*ctxt).vstate).depth)) == 0 {
                            cur = (*(*ctxt).vstate).node;
                            DEBUG_VALID_MSG!(c"Plus branch failed, rollback".as_ptr());
                            if vstate_vpop(ctxt) < 0 {
                                DEBUG_VALID_MSG!(c"exhaustion, failed".as_ptr());
                                return 0;
                            }
                            if cur != (*(*ctxt).vstate).node {
                                determinist = -3;
                            }
                            // goto cont;
                            continue 'cont;
                        }
                        DEBUG_VALID_MSG!(c"Plus branch found".as_ptr());
                        ret = 1;
                    }
                    XmlElementContentOccur::XmlElementContentMult => {
                        ret = 1;
                    }
                    XmlElementContentOccur::XmlElementContentOpt => {
                        DEBUG_VALID_MSG!(c"Option branch failed".as_ptr());
                        ret = 1;
                    }
                }
            } else {
                match (*(*(*ctxt).vstate).cont).ocur {
                    XmlElementContentOccur::XmlElementContentOpt => {
                        DEBUG_VALID_MSG!(c"Option branch succeeded".as_ptr());
                        ret = 1;
                    }
                    XmlElementContentOccur::XmlElementContentOnce => {
                        DEBUG_VALID_MSG!(c"Once branch succeeded".as_ptr());
                        ret = 1;
                    }
                    XmlElementContentOccur::XmlElementContentPlus => {
                        if (*(*ctxt).vstate).state == ROLLBACK_PARENT as u8 {
                            DEBUG_VALID_MSG!(c"Plus branch rollback".as_ptr());
                            ret = 1;
                            // break;
                        } else {
                            if (*(*ctxt).vstate).node.is_null() {
                                DEBUG_VALID_MSG!(c"Plus branch exhausted".as_ptr());
                                ret = 1;
                                // break;
                            } else {
                                DEBUG_VALID_MSG!(c"Plus branch succeeded, continuing".as_ptr());
                                (*(*ctxt).vstate).occurs |= 1 << (*(*ctxt).vstate).depth;
                                // goto cont;
                                continue 'cont;
                            }
                        }
                    }
                    XmlElementContentOccur::XmlElementContentMult => {
                        if (*(*ctxt).vstate).state == ROLLBACK_PARENT as u8 {
                            DEBUG_VALID_MSG!(c"Mult branch rollback".as_ptr());
                            ret = 1;
                            // break;
                        } else {
                            if (*(*ctxt).vstate).node.is_null() {
                                DEBUG_VALID_MSG!(c"Mult branch exhausted".as_ptr());
                                ret = 1;
                                // break;
                            } else {
                                DEBUG_VALID_MSG!(c"Mult branch succeeded, continuing".as_ptr());
                                /* (*(*ctxt).vstate).occurs |= 1 << (*(*ctxt).vstate).depth; */
                                // goto cont;
                                continue 'cont;
                            }
                        }
                    }
                }
            }
            (*(*ctxt).vstate).state = 0;

            /*
             * Then act accordingly at the parent level
             */
            (*(*ctxt).vstate).occurs &= (1 << (*(*ctxt).vstate).depth) - 1;
            if ((*(*(*ctxt).vstate).cont).parent.is_null())
                || ((*(*(*ctxt).vstate).cont).parent == 1 as XmlElementContentPtr)
            {
                break;
            }

            match (*(*(*(*ctxt).vstate).cont).parent).typ {
                XmlElementContentType::XmlElementContentPCDATA => {
                    DEBUG_VALID_MSG!(c"Error: parent pcdata".as_ptr());
                    return -1;
                }
                XmlElementContentType::XmlElementContentElement => {
                    DEBUG_VALID_MSG!(c"Error: parent element".as_ptr());
                    return -1;
                }
                XmlElementContentType::XmlElementContentOr => {
                    if ret == 1 {
                        DEBUG_VALID_MSG!(c"Or succeeded".as_ptr());
                        (*(*ctxt).vstate).cont = (*(*(*ctxt).vstate).cont).parent;
                        (*(*ctxt).vstate).depth -= 1;
                    } else {
                        DEBUG_VALID_MSG!(c"Or failed".as_ptr());
                        (*(*ctxt).vstate).cont = (*(*(*ctxt).vstate).cont).parent;
                        (*(*ctxt).vstate).depth -= 1;
                    }
                }
                XmlElementContentType::XmlElementContentSeq => {
                    if ret == 0 {
                        DEBUG_VALID_MSG!(c"Sequence failed".as_ptr());
                        (*(*ctxt).vstate).cont = (*(*(*ctxt).vstate).cont).parent;
                        (*(*ctxt).vstate).depth -= 1;
                    } else if (*(*ctxt).vstate).cont == (*(*(*(*ctxt).vstate).cont).parent).c1 {
                        DEBUG_VALID_MSG!(c"Sequence testing 2nd branch".as_ptr());
                        (*(*ctxt).vstate).cont = (*(*(*(*ctxt).vstate).cont).parent).c2;
                        // goto cont;
                        continue 'cont;
                    } else {
                        DEBUG_VALID_MSG!(c"Sequence succeeded".as_ptr());
                        (*(*ctxt).vstate).cont = (*(*(*ctxt).vstate).cont).parent;
                        (*(*ctxt).vstate).depth -= 1;
                    }
                }
            }
        }
        if !(*(*ctxt).vstate).node.is_null() {
            let cur: XmlNodePtr;

            cur = (*(*ctxt).vstate).node;
            DEBUG_VALID_MSG!(c"Failed, remaining input, rollback".as_ptr());
            if vstate_vpop(ctxt) < 0 {
                DEBUG_VALID_MSG!(c"exhaustion, failed".as_ptr());
                return 0;
            }
            if cur != (*(*ctxt).vstate).node {
                determinist = -3;
            }
            // goto cont;
            continue 'cont;
        }
        if ret == 0 {
            let cur: XmlNodePtr;

            cur = (*(*ctxt).vstate).node;
            DEBUG_VALID_MSG!(c"Failure, rollback".as_ptr());
            if vstate_vpop(ctxt) < 0 {
                DEBUG_VALID_MSG!(c"exhaustion, failed".as_ptr());
                return 0;
            }
            if cur != (*(*ctxt).vstate).node {
                determinist = -3;
            }
            // goto cont;
        }
    }
    determinist
}

/// Try to validate the content model of an element
///
/// returns 1 if valid or 0 if not and -1 in case of error
#[doc(alias = "xmlValidateElementContent")]
unsafe extern "C" fn xml_validate_element_content(
    ctxt: XmlValidCtxtPtr,
    child: XmlNodePtr,
    elem_decl: XmlElementPtr,
    warn: i32,
    parent: XmlNodePtr,
) -> i32 {
    let mut ret: i32 = 1;
    #[cfg(not(feature = "libxml_regexp"))]
    let mut repl: XmlNodePtr = null_mut();
    #[cfg(not(feature = "libxml_regexp"))]
    let mut last: XmlNodePtr = null_mut();
    #[cfg(not(feature = "libxml_regexp"))]
    let mut tmp: XmlNodePtr;
    let mut cur: XmlNodePtr;

    if elem_decl.is_null() || parent.is_null() || ctxt.is_null() {
        return -1;
    }
    let cont: XmlElementContentPtr = (*elem_decl).content;
    let name = (*elem_decl)
        .name
        .as_ref()
        .map(|n| CString::new(n.as_str()).unwrap());

    #[cfg(feature = "libxml_regexp")]
    {
        /* Build the regexp associated to the content model */
        if (*elem_decl).cont_model.is_null() {
            ret = xml_valid_build_content_model(ctxt, elem_decl);
        }
        if (*elem_decl).cont_model.is_null() {
            return -1;
        } else {
            if xml_regexp_is_determinist((*elem_decl).cont_model) == 0 {
                return -1;
            }
            (*ctxt).node_max = 0;
            (*ctxt).node_nr = 0;
            (*ctxt).node_tab = null_mut();
            let exec: XmlRegExecCtxtPtr =
                xml_reg_new_exec_ctxt((*elem_decl).cont_model, None, null_mut());
            if !exec.is_null() {
                cur = child;
                'fail: {
                    while !cur.is_null() {
                        match (*cur).element_type() {
                            XmlElementType::XmlEntityRefNode => {
                                // Push the current node to be able to roll back
                                // and process within the entity
                                if let Some(children) = (*cur)
                                    .children()
                                    .filter(|children| children.children().is_some())
                                {
                                    node_vpush(ctxt, cur);
                                    cur = children.children().map_or(null_mut(), |c| c.as_ptr());
                                    continue;
                                }
                            }
                            XmlElementType::XmlTextNode => {
                                if (*cur).is_blank_node() {
                                    //  break;
                                } else {
                                    ret = 0;
                                    break 'fail;
                                }
                            }
                            XmlElementType::XmlCDATASectionNode => {
                                /* TODO */
                                ret = 0;
                                break 'fail;
                            }
                            XmlElementType::XmlElementNode => {
                                if !(*cur).ns.is_null() && !(*(*cur).ns).prefix.is_null() {
                                    let mut fname: [XmlChar; 50] = [0; 50];

                                    let fullname: *mut XmlChar = xml_build_qname(
                                        (*cur).name,
                                        (*(*cur).ns).prefix as _,
                                        fname.as_mut_ptr(),
                                        50,
                                    );
                                    if fullname.is_null() {
                                        ret = -1;
                                        break 'fail;
                                    }
                                    // ret =
                                    xml_reg_exec_push_string(exec, fullname, null_mut());
                                    if fullname != fname.as_ptr() as _
                                        && fullname != (*cur).name as _
                                    {
                                        xml_free(fullname as _);
                                    }
                                } else {
                                    // ret =
                                    xml_reg_exec_push_string(exec, (*cur).name, null_mut());
                                }
                            }
                            _ => {}
                        }
                        /*
                         * Switch to next element
                         */
                        cur = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
                        while cur.is_null() {
                            cur = node_vpop(ctxt);
                            if cur.is_null() {
                                break;
                            }
                            cur = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
                        }
                    }

                    ret = xml_reg_exec_push_string(exec, null_mut(), null_mut());
                }
                // fail:
                xml_reg_free_exec_ctxt(exec);
            }
        }
    }

    #[cfg_attr(feature = "libxml_regexp", allow(unused_labels))]
    // label `'done` is used just only when 'regexp' is disabled.
    'done: {
        #[cfg(not(feature = "libxml_regexp"))]
        {
            /*
             * Allocate the stack
             */
            (*ctxt).vstateMax = 8;
            (*ctxt).vstateTab =
                xml_malloc((*ctxt).vstateMax as usize * size_of_val(&*(*ctxt).vstateTab.add(0)))
                    as *mut XmlValidState;
            if (*ctxt).vstateTab.is_null() {
                xml_verr_memory(ctxt as _, c"malloc failed".as_ptr() as _);
                return -1;
            }
            /*
             * The first entry in the stack is reserved to the current state
             */
            (*ctxt).nodeMax = 0;
            (*ctxt).nodeNr = 0;
            (*ctxt).nodeTab = null_mut();
            (*ctxt).vstate = (*ctxt).vstateTab.add(0);
            (*ctxt).vstateNr = 1;
            (*(*ctxt).vstate).cont = cont;
            (*(*ctxt).vstate).node = child;
            (*(*ctxt).vstate).depth = 0;
            (*(*ctxt).vstate).occurs = 0;
            (*(*ctxt).vstate).state = 0;
            ret = xmlValidateElementType(ctxt);
            if ret == -3 && warn != 0 {
                let mut expr: [c_char; 5000];
                expr[0] = 0;
                xml_snprintf_element_content(expr.as_mut_ptr() as _, 5000, (*elem_decl).content, 1);
                xml_err_valid_node(
                    ctxt,
                    elem_decl as XmlNodePtr,
                    XmlParserErrors::XmlDTDContentNotDeterminist,
                    c"Content model of %s is not deterministic: %s\n".as_ptr() as _,
                    name,
                    expr.as_ptr() as _,
                    null_mut(),
                );
            } else if ret == -2 {
                /*
                 * An entities reference appeared at this level.
                 * Build a minimal representation of this node content
                 * sufficient to run the validation process on it
                 */
                DEBUG_VALID_MSG!(c"Found an entity reference, linearizing".as_ptr());
                cur = child;
                while !cur.is_null() {
                    match (*cur).element_type() {
                        XmlElementType::XmlEntityRefNode => {
                            /*
                             * Push the current node to be able to roll back
                             * and process within the entity
                             */
                            if !(*cur).children.is_null() && !(*(*cur).children).children.is_null()
                            {
                                node_vpush(ctxt, cur);
                                cur = (*(*cur).children).children;
                                continue;
                            }
                        }
                        ty @ XmlElementType::XmlTextNode
                        | ty @ XmlElementType::XmlCDATASectionNode
                        | ty @ XmlElementType::XmlElementNode => {
                            if matches!(ty, XmlElementType::XmlTextNode)
                                && xml_is_blank_node(cur) != 0
                            {
                                // break;
                            } else {
                                /*
                                 * Allocate a new node and minimally fills in
                                 * what's required
                                 */
                                tmp = xml_malloc(size_of::<XmlNode>()) as XmlNodePtr;
                                if tmp.is_null() {
                                    xml_verr_memory(ctxt as _, c"malloc failed".as_ptr() as _);
                                    xmlFreeNodeList(repl);
                                    ret = -1;
                                    break 'done;
                                }
                                (*tmp).typ = (*cur).typ;
                                (*tmp).name = (*cur).name;
                                (*tmp).ns = (*cur).ns;
                                (*tmp).next = null_mut();
                                (*tmp).content = null_mut();
                                if repl.is_null() {
                                    repl = tmp;
                                    last = tmp;
                                } else {
                                    (*last).next = tmp;
                                    last = tmp;
                                }
                                if matches!((*cur).typ, XmlElementType::XmlCDATASectionNode) {
                                    /*
                                     * E59 spaces in CDATA does not match the
                                     * nonterminal S
                                     */
                                    (*tmp).content = xml_strdup(c"CDATA".as_ptr() as _);
                                }
                            }
                        }
                        _ => {}
                    }
                    /*
                     * Switch to next element
                     */
                    cur = (*cur).next;
                    while cur.is_null() {
                        cur = node_vpop(ctxt);
                        if cur.is_null() {
                            break;
                        }
                        cur = (*cur).next;
                    }
                }

                /*
                 * Relaunch the validation
                 */
                (*ctxt).vstate = (*ctxt).vstateTab.add(0);
                (*ctxt).vstateNr = 1;
                (*(*ctxt).vstate).cont = cont;
                (*(*ctxt).vstate).node = repl;
                (*(*ctxt).vstate).depth = 0;
                (*(*ctxt).vstate).occurs = 0;
                (*(*ctxt).vstate).state = 0;
                ret = xmlValidateElementType(ctxt);
            }
        }

        if warn != 0 && (ret != 1 && ret != -3) {
            if !ctxt.is_null() {
                let mut expr: [c_char; 5000] = [0; 5000];
                let mut list: [c_char; 5000] = [0; 5000];

                expr[0] = 0;
                xml_snprintf_element_content(expr.as_mut_ptr().add(0) as _, 5000, cont, 1);
                list[0] = 0;
                #[cfg(not(feature = "libxml_regexp"))]
                {
                    if !repl.is_null() {
                        xml_snprintf_elements(list.as_mut_ptr().add(0) as _, 5000, repl, 1);
                    } else {
                        xml_snprintf_elements(list.as_mut_ptr().add(0) as _, 5000, child, 1);
                    }
                }
                #[cfg(feature = "libxml_regexp")]
                {
                    xml_snprintf_elements(list.as_mut_ptr().add(0) as _, 5000, child, 1);
                }

                if let Some(name) = name.as_deref() {
                    let name = name.to_string_lossy();
                    let expr = CStr::from_ptr(expr.as_ptr()).to_string_lossy();
                    let list = CStr::from_ptr(list.as_ptr()).to_string_lossy();
                    xml_err_valid_node(
                        ctxt,
                        parent,
                        XmlParserErrors::XmlDTDContentModel,
                        format!(
                            "Element {name} content does not follow the DTD, expecting {expr}, got {list}\n"
                        )
                        .as_str(),
                        Some(&name),
                        Some(&expr),
                        Some(&list),
                    );
                } else {
                    let expr = CStr::from_ptr(expr.as_ptr()).to_string_lossy();
                    let list = CStr::from_ptr(list.as_ptr()).to_string_lossy();
                    xml_err_valid_node(
                        ctxt,
                        parent,
                        XmlParserErrors::XmlDTDContentModel,
                        format!("Element content does not follow the DTD, expecting {expr}, got {list}\n")
                            .as_str(),
                        Some(&expr),
                        Some(&list),
                        None,
                    );
                }
            } else if let Some(name) = name.as_deref() {
                let name = name.to_string_lossy();
                xml_err_valid_node(
                    ctxt,
                    parent,
                    XmlParserErrors::XmlDTDContentModel,
                    format!("Element {name} content does not follow the DTD\n").as_str(),
                    Some(&name),
                    None,
                    None,
                );
            } else {
                xml_err_valid_node(
                    ctxt,
                    parent,
                    XmlParserErrors::XmlDTDContentModel,
                    "Element content does not follow the DTD\n",
                    None,
                    None,
                    None,
                );
            }
            ret = 0;
        }
        if ret == -3 {
            ret = 1;
        }
    }

    #[cfg(not(feature = "libxml_regexp"))]
    {
        // done:
        /*
         * Deallocate the copy if done, and free up the validation stack
         */
        while !repl.is_null() {
            tmp = (*repl).next;
            xml_free(repl as _);
            repl = tmp;
        }
        (*ctxt).vstateMax = 0;
        if !(*ctxt).vstateTab.is_null() {
            xml_free((*ctxt).vstateTab as _);
            (*ctxt).vstateTab = null_mut();
        }
    }
    (*ctxt).node_max = 0;
    (*ctxt).node_nr = 0;
    if !(*ctxt).node_tab.is_null() {
        xml_free((*ctxt).node_tab as _);
        (*ctxt).node_tab = null_mut();
    }
    ret
}

/// Try to validate a single element and it's attributes,
/// basically it does the following checks as described by the
/// XML-1.0 recommendation:
///  - [ VC: Element Valid ]
///  - [ VC: Required Attribute ]
///    Then call xmlValidateOneAttribute() for each attribute present.
///
/// The ID/IDREF checkings are done separately
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateOneElement")]
#[cfg(feature = "libxml_valid")]
pub unsafe extern "C" fn xml_validate_one_element(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    elem: XmlNodePtr,
) -> i32 {
    let mut cont: XmlElementContentPtr;
    let mut attr: XmlAttributePtr;
    let mut child: XmlNodePtr;
    let mut ret: i32 = 1;
    let tmp: i32;
    let mut name: *const XmlChar;
    let mut extsubset: i32 = 0;

    CHECK_DTD!(doc);

    if elem.is_null() {
        return 0;
    }
    match (*elem).element_type() {
        XmlElementType::XmlAttributeNode => {
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlErrInternalError,
                "Attribute element not expected\n",
                None,
                None,
                None,
            );
            return 0;
        }
        XmlElementType::XmlTextNode => {
            if (*elem).children().is_some() {
                xml_err_valid_node(
                    ctxt,
                    elem,
                    XmlParserErrors::XmlErrInternalError,
                    "Text element has children !\n",
                    None,
                    None,
                    None,
                );
                return 0;
            }
            if !(*elem).ns.is_null() {
                xml_err_valid_node(
                    ctxt,
                    elem,
                    XmlParserErrors::XmlErrInternalError,
                    "Text element has namespace !\n",
                    None,
                    None,
                    None,
                );
                return 0;
            }
            if (*elem).content.is_null() {
                xml_err_valid_node(
                    ctxt,
                    elem,
                    XmlParserErrors::XmlErrInternalError,
                    "Text element has no content !\n",
                    None,
                    None,
                    None,
                );
                return 0;
            }
            return 1;
        }
        XmlElementType::XmlXIncludeStart | XmlElementType::XmlXIncludeEnd => {
            return 1;
        }
        XmlElementType::XmlCDATASectionNode
        | XmlElementType::XmlEntityRefNode
        | XmlElementType::XmlPINode
        | XmlElementType::XmlCommentNode => {
            return 1;
        }
        XmlElementType::XmlEntityNode => {
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlErrInternalError,
                "Entity element not expected\n",
                None,
                None,
                None,
            );
            return 0;
        }
        XmlElementType::XmlNotationNode => {
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlErrInternalError,
                "Notation element not expected\n",
                None,
                None,
                None,
            );
            return 0;
        }
        XmlElementType::XmlDocumentNode
        | XmlElementType::XmlDocumentTypeNode
        | XmlElementType::XmlDocumentFragNode => {
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlErrInternalError,
                "Document element not expected\n",
                None,
                None,
                None,
            );
            return 0;
        }
        XmlElementType::XmlHTMLDocumentNode => {
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlErrInternalError,
                "HTML Document not expected\n",
                None,
                None,
                None,
            );
            return 0;
        }
        XmlElementType::XmlElementNode => {}
        _ => {
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlErrInternalError,
                "unknown element type\n",
                None,
                None,
                None,
            );
            return 0;
        }
    }

    // Fetch the declaration
    let elem_decl: XmlElementPtr =
        xml_valid_get_elem_decl(ctxt, doc, elem, addr_of_mut!(extsubset));
    if elem_decl.is_null() {
        return 0;
    }

    // If vstateNr is not zero that means continuous validation is
    // activated, do not try to check the content model at that level.
    if (*ctxt).vstate_nr == 0 {
        /* Check that the element content matches the definition */
        match (*elem_decl).etype {
            XmlElementTypeVal::XmlElementTypeUndefined => {
                let name = (*elem).name().unwrap();
                xml_err_valid_node(
                    ctxt,
                    elem,
                    XmlParserErrors::XmlDTDUnknownElem,
                    format!("No declaration for element {name}\n").as_str(),
                    Some(&name),
                    None,
                    None,
                );
                return 0;
            }
            XmlElementTypeVal::XmlElementTypeEmpty => {
                if (*elem).children().is_some() {
                    let name = (*elem).name().unwrap();
                    xml_err_valid_node(
                        ctxt,
                        elem,
                        XmlParserErrors::XmlDTDNotEmpty,
                        format!("Element {name} was declared EMPTY this one has content\n")
                            .as_str(),
                        Some(&name),
                        None,
                        None,
                    );
                    ret = 0;
                }
            }
            XmlElementTypeVal::XmlElementTypeAny => {
                // I don't think anything is required then
            }
            XmlElementTypeVal::XmlElementTypeMixed => {
                // simple case of declared as #PCDATA
                if !(*elem_decl).content.is_null()
                    && (*(*elem_decl).content).typ == XmlElementContentType::XmlElementContentPCDATA
                {
                    ret = xml_validate_one_cdata_element(ctxt, doc, elem);
                    if ret == 0 {
                        let name = (*elem).name().unwrap();
                        xml_err_valid_node(
                            ctxt,
                            elem,
                            XmlParserErrors::XmlDTDNotPCDATA,
                            format!(
                                "Element {name} was declared #PCDATA but contains non text nodes\n"
                            )
                            .as_str(),
                            Some(&name),
                            None,
                            None,
                        );
                    }
                } else {
                    child = (*elem).children().map_or(null_mut(), |c| c.as_ptr());
                    // Hum, this start to get messy
                    while !child.is_null() {
                        'child_ok: {
                            if matches!((*child).element_type(), XmlElementType::XmlElementNode) {
                                name = (*child).name;
                                if !(*child).ns.is_null() && !(*(*child).ns).prefix.is_null() {
                                    let mut fname: [XmlChar; 50] = [0; 50];

                                    let fullname: *mut XmlChar = xml_build_qname(
                                        (*child).name,
                                        (*(*child).ns).prefix,
                                        fname.as_mut_ptr() as _,
                                        50,
                                    );
                                    if fullname.is_null() {
                                        return 0;
                                    }
                                    cont = (*elem_decl).content;
                                    while !cont.is_null() {
                                        if matches!(
                                            (*cont).typ,
                                            XmlElementContentType::XmlElementContentElement
                                        ) {
                                            if xml_str_equal((*cont).name, fullname) {
                                                break;
                                            }
                                        } else if matches!(
                                            (*cont).typ,
                                            XmlElementContentType::XmlElementContentOr
                                        ) && !(*cont).c1.is_null()
                                            && (*(*cont).c1).typ
                                                == XmlElementContentType::XmlElementContentElement
                                        {
                                            if xml_str_equal((*(*cont).c1).name, fullname) {
                                                break;
                                            }
                                        } else if !matches!(
                                            (*cont).typ,
                                            XmlElementContentType::XmlElementContentOr
                                        ) || (*cont).c1.is_null()
                                            || !matches!(
                                                (*(*cont).c1).typ,
                                                XmlElementContentType::XmlElementContentPCDATA
                                            )
                                        {
                                            xml_err_valid!(
                                                null_mut(),
                                                XmlParserErrors::XmlDTDMixedCorrupt,
                                                "Internal: MIXED struct corrupted\n"
                                            );
                                            break;
                                        }
                                        cont = (*cont).c2;
                                    }
                                    if fullname != fname.as_ptr() as _
                                        && fullname != (*child).name as _
                                    {
                                        xml_free(fullname as _);
                                    }
                                    if !cont.is_null() {
                                        break 'child_ok;
                                    }
                                }

                                cont = (*elem_decl).content;
                                while !cont.is_null() {
                                    if matches!(
                                        (*cont).typ,
                                        XmlElementContentType::XmlElementContentElement
                                    ) {
                                        if xml_str_equal((*cont).name, name) {
                                            break;
                                        }
                                    } else if matches!(
                                        (*cont).typ,
                                        XmlElementContentType::XmlElementContentOr
                                    ) && !(*cont).c1.is_null()
                                        && matches!(
                                            (*(*cont).c1).typ,
                                            XmlElementContentType::XmlElementContentElement
                                        )
                                    {
                                        if xml_str_equal((*(*cont).c1).name, name) {
                                            break;
                                        }
                                    } else if !matches!(
                                        (*cont).typ,
                                        XmlElementContentType::XmlElementContentOr
                                    ) || (*cont).c1.is_null()
                                        || !matches!(
                                            (*(*cont).c1).typ,
                                            XmlElementContentType::XmlElementContentPCDATA
                                        )
                                    {
                                        xml_err_valid!(
                                            ctxt,
                                            XmlParserErrors::XmlDTDMixedCorrupt,
                                            "Internal: MIXED struct corrupted\n"
                                        );
                                        break;
                                    }
                                    cont = (*cont).c2;
                                }
                                if cont.is_null() {
                                    let name = CStr::from_ptr(name as *const i8).to_string_lossy();
                                    let elem_name = (*elem).name().unwrap();
                                    xml_err_valid_node(
                                        ctxt,
                                        elem,
                                        XmlParserErrors::XmlDTDInvalidChild,
                                        format!("Element {name} is not declared in {elem_name} list of possible children\n").as_str(),
                                        Some(&name),
                                        Some(&elem_name),
                                        None
                                    );
                                    ret = 0;
                                }
                            }
                        }
                        // child_ok:
                        child = (*child).next.map_or(null_mut(), |n| n.as_ptr());
                    }
                }
            }
            XmlElementTypeVal::XmlElementTypeElement => {
                if (*doc).standalone == 1 && extsubset == 1 {
                    // VC: Standalone Document Declaration
                    //     - element types with element content, if white space
                    //       occurs directly within any instance of those types.
                    child = (*elem).children().map_or(null_mut(), |c| c.as_ptr());
                    while !child.is_null() {
                        if matches!((*child).element_type(), XmlElementType::XmlTextNode) {
                            let mut content: *const XmlChar = (*child).content;

                            while xml_is_blank_char(*content as u32) {
                                content = content.add(1);
                            }
                            if *content == 0 {
                                let name = (*elem).name().unwrap();
                                xml_err_valid_node(
                                    ctxt,
                                    elem,
                                    XmlParserErrors::XmlDTDStandaloneWhiteSpace,
                                    format!("standalone: {name} declared in the external subset contains white spaces nodes\n").as_str(),
                                    Some(&name),
                                    None,
                                    None
                                );
                                ret = 0;
                                break;
                            }
                        }
                        child = (*child).next.map_or(null_mut(), |n| n.as_ptr());
                    }
                }
                child = (*elem).children().map_or(null_mut(), |c| c.as_ptr());
                // cont = (*elem_decl).content;
                tmp = xml_validate_element_content(ctxt, child, elem_decl, 1, elem);
                if tmp <= 0 {
                    ret = tmp;
                }
            }
        }
    } /* not continuous */

    // [ VC: Required Attribute ]
    attr = (*elem_decl).attributes;
    while !attr.is_null() {
        'found: {
            if matches!((*attr).def, XmlAttributeDefault::XmlAttributeRequired) {
                let mut qualified: i32 = -1;

                if (*attr).prefix.is_none() && xml_str_equal((*attr).name, c"xmlns".as_ptr() as _) {
                    let mut ns: XmlNsPtr;

                    ns = (*elem).ns_def;
                    while !ns.is_null() {
                        if (*ns).prefix.is_null() {
                            break 'found;
                        }
                        ns = (*ns).next;
                    }
                } else if (*attr).prefix.as_deref() == Some("xmlns") {
                    let mut ns: XmlNsPtr;

                    ns = (*elem).ns_def;
                    while !ns.is_null() {
                        if xml_str_equal((*attr).name, (*ns).prefix) {
                            break 'found;
                        }
                        ns = (*ns).next;
                    }
                } else {
                    let mut attrib: XmlAttrPtr;

                    attrib = (*elem).properties;
                    while !attrib.is_null() {
                        if xml_str_equal((*attrib).name, (*attr).name) {
                            if let Some(prefix) = (*attr).prefix.as_deref() {
                                let prefix = CString::new(prefix).unwrap();
                                let mut name_space: XmlNsPtr = (*attrib).ns;

                                if name_space.is_null() {
                                    name_space = (*elem).ns;
                                }
                                // qualified names handling is problematic, having a
                                // different prefix should be possible but DTDs don't
                                // allow to define the URI instead of the prefix :-(
                                if name_space.is_null() {
                                    if qualified < 0 {
                                        qualified = 0;
                                    }
                                } else if !xml_str_equal(
                                    (*name_space).prefix,
                                    prefix.as_ptr() as *const u8,
                                ) {
                                    if qualified < 1 {
                                        qualified = 1;
                                    }
                                } else {
                                    break 'found;
                                }
                            } else {
                                // We should allow applications to define namespaces
                                // for their application even if the DTD doesn't
                                // carry one, otherwise, basically we would always break.
                                break 'found;
                            }
                        }
                        attrib = (*attrib).next;
                    }
                }
                if qualified == -1 {
                    if (*attr).prefix.is_none() {
                        let elem_name = (*elem).name().unwrap();
                        let attr_name = (*attr).name().unwrap();
                        xml_err_valid_node(
                            ctxt,
                            elem,
                            XmlParserErrors::XmlDTDMissingAttribute,
                            format!("Element {elem_name} does not carry attribute {attr_name}\n")
                                .as_str(),
                            Some(&elem_name),
                            Some(&attr_name),
                            None,
                        );
                        ret = 0;
                    } else {
                        let elem_name = (*elem).name().unwrap();
                        let prefix = (*attr).prefix.as_deref().unwrap();
                        let attr_name = (*attr).name().unwrap();
                        xml_err_valid_node(
                            ctxt,
                            elem,
                            XmlParserErrors::XmlDTDMissingAttribute,
                            format!("Element {elem_name} does not carry attribute {prefix}:{attr_name}\n").as_str(),
                            Some(&elem_name),
                            Some(prefix),
                            Some(&attr_name),
                        );
                        ret = 0;
                    }
                } else if qualified == 0 {
                    xml_err_valid_warning!(
                        ctxt,
                        elem,
                        XmlParserErrors::XmlDTDNoPrefix,
                        "Element {} required attribute {}:{} has no prefix\n",
                        (*elem).name().unwrap(),
                        (*attr).prefix.as_deref().unwrap(),
                        (*attr).name().unwrap()
                    );
                } else if qualified == 1 {
                    xml_err_valid_warning!(
                        ctxt,
                        elem,
                        XmlParserErrors::XmlDTDDifferentPrefix,
                        "Element {} required attribute {}:{} has different prefix\n",
                        (*elem).name().unwrap(),
                        (*attr).prefix.as_deref().unwrap(),
                        (*attr).name().unwrap()
                    );
                }
            } else if matches!((*attr).def, XmlAttributeDefault::XmlAttributeFixed) {
                // Special tests checking #FIXED namespace declarations
                // have the right value since this is not done as an
                // attribute checking
                if (*attr).prefix.is_none() && xml_str_equal((*attr).name, c"xmlns".as_ptr() as _) {
                    let mut ns: XmlNsPtr;

                    ns = (*elem).ns_def;
                    while !ns.is_null() {
                        if (*ns).prefix.is_null() {
                            if !xml_str_equal((*attr).default_value, (*ns).href) {
                                let elem_name = (*elem).name().unwrap();
                                xml_err_valid_node(
                                    ctxt,
                                    elem,
                                    XmlParserErrors::XmlDTDElemDefaultNamespace,
                                    format!("Element {elem_name} namespace name for default namespace does not match the DTD\n").as_str(),
                                    Some(&elem_name),
                                    None,
                                    None
                                );
                                ret = 0;
                            }
                            break 'found;
                        }
                        ns = (*ns).next;
                    }
                } else if (*attr).prefix.as_deref() == Some("xmlns") {
                    let mut ns: XmlNsPtr;

                    ns = (*elem).ns_def;
                    while !ns.is_null() {
                        if xml_str_equal((*attr).name, (*ns).prefix) {
                            if !xml_str_equal((*attr).default_value, (*ns).href) {
                                let elem_name = (*elem).name().unwrap();
                                let prefix =
                                    CStr::from_ptr((*ns).prefix as *const i8).to_string_lossy();
                                xml_err_valid_node(
                                    ctxt,
                                    elem,
                                    XmlParserErrors::XmlDTDElemNamespace,
                                    format!(
                                        "Element {elem_name} namespace name for {prefix} does not match the DTD\n"
                                    )
                                    .as_str(),
                                    Some(&elem_name),
                                    Some(&prefix),
                                    None,
                                );
                                ret = 0;
                            }
                            break 'found;
                        }
                        ns = (*ns).next;
                    }
                }
            }
        }
        // found:
        attr = (*attr).nexth;
    }
    ret
}

/// Try to validate a single attribute for an element
/// basically it does the following checks as described by the
/// XML-1.0 recommendation:
///  - [ VC: Attribute Value Type ]
///  - [ VC: Fixed Attribute Default ]
///  - [ VC: Entity Name ]
///  - [ VC: Name Token ]
///  - [ VC: ID ]
///  - [ VC: IDREF ]
///  - [ VC: Entity Name ]
///  - [ VC: Notation Attributes ]
///
/// The ID/IDREF uniqueness and matching are done separately
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateOneAttribute")]
#[cfg(feature = "libxml_valid")]
pub unsafe extern "C" fn xml_validate_one_attribute(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    elem: XmlNodePtr,
    attr: XmlAttrPtr,
    value: *const XmlChar,
) -> i32 {
    let mut attr_decl: XmlAttributePtr = null_mut();
    let mut ret: i32 = 1;

    CHECK_DTD!(doc);
    if elem.is_null() || (*elem).name.is_null() {
        return 0;
    }
    if attr.is_null() || (*attr).name.is_null() {
        return 0;
    }

    if !(*elem).ns.is_null() && !(*(*elem).ns).prefix.is_null() {
        let mut fname: [XmlChar; 50] = [0; 50];

        let fullname: *mut XmlChar = xml_build_qname(
            (*elem).name,
            (*(*elem).ns).prefix as _,
            fname.as_mut_ptr(),
            50,
        );
        if fullname.is_null() {
            return 0;
        }
        if !(*attr).ns.is_null() {
            attr_decl = (*(*doc).int_subset).get_qattr_desc(
                CStr::from_ptr(fullname as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                (*attr).name().as_deref().unwrap(),
                (!(*(*attr).ns).prefix.is_null())
                    .then(|| CStr::from_ptr((*(*attr).ns).prefix as *const i8).to_string_lossy())
                    .as_deref(),
            );
            if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                attr_decl = (*(*doc).ext_subset).get_qattr_desc(
                    CStr::from_ptr(fullname as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    (*attr).name().as_deref().unwrap(),
                    (!(*(*attr).ns).prefix.is_null())
                        .then(|| {
                            CStr::from_ptr((*(*attr).ns).prefix as *const i8).to_string_lossy()
                        })
                        .as_deref(),
                );
            }
        } else {
            attr_decl = (*(*doc).int_subset).get_attr_desc(
                CStr::from_ptr(fullname as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                (*attr).name().as_deref().unwrap(),
            );
            if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                attr_decl = (*(*doc).ext_subset).get_attr_desc(
                    CStr::from_ptr(fullname as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    (*attr).name().as_deref().unwrap(),
                );
            }
        }
        if fullname != fname.as_ptr() as _ && fullname != (*elem).name as _ {
            xml_free(fullname as _);
        }
    }
    if attr_decl.is_null() {
        if !(*attr).ns.is_null() {
            attr_decl = (*(*doc).int_subset).get_qattr_desc(
                (*elem).name().unwrap().as_ref(),
                (*attr).name().as_deref().unwrap(),
                (!(*(*attr).ns).prefix.is_null())
                    .then(|| CStr::from_ptr((*(*attr).ns).prefix as *const i8).to_string_lossy())
                    .as_deref(),
            );
            if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                attr_decl = (*(*doc).ext_subset).get_qattr_desc(
                    (*elem).name().unwrap().as_ref(),
                    (*attr).name().as_deref().unwrap(),
                    (!(*(*attr).ns).prefix.is_null())
                        .then(|| {
                            CStr::from_ptr((*(*attr).ns).prefix as *const i8).to_string_lossy()
                        })
                        .as_deref(),
                );
            }
        } else {
            attr_decl = (*(*doc).int_subset).get_attr_desc(
                (*elem).name().as_deref().unwrap(),
                (*attr).name().as_deref().unwrap(),
            );
            if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                attr_decl = (*(*doc).ext_subset).get_attr_desc(
                    (*elem).name().as_deref().unwrap(),
                    (*attr).name().as_deref().unwrap(),
                );
            }
        }
    }

    /* Validity Constraint: Attribute Value Type */
    if attr_decl.is_null() {
        let attr_name = (*attr).name().unwrap();
        let elem_name = (*elem).name().unwrap();
        xml_err_valid_node(
            ctxt,
            elem,
            XmlParserErrors::XmlDTDUnknownAttribute,
            format!("No declaration for attribute {attr_name} of element {elem_name}\n").as_str(),
            Some(&attr_name),
            Some(&elem_name),
            None,
        );
        return 0;
    }
    (*attr).atype = Some((*attr_decl).atype);

    let val: i32 = xml_validate_attribute_value_internal(doc, (*attr_decl).atype, value);
    if val == 0 {
        let attr_name = (*attr).name().unwrap();
        let elem_name = (*elem).name().unwrap();
        xml_err_valid_node(
            ctxt,
            elem,
            XmlParserErrors::XmlDTDAttributeValue,
            format!("Syntax of value for attribute {attr_name} of {elem_name} is not valid\n")
                .as_str(),
            Some(&attr_name),
            Some(&elem_name),
            None,
        );
        ret = 0;
    }

    /* Validity constraint: Fixed Attribute Default */
    if matches!((*attr_decl).def, XmlAttributeDefault::XmlAttributeFixed)
        && !xml_str_equal(value, (*attr_decl).default_value)
    {
        let attr_name = (*attr).name().unwrap();
        let elem_name = (*elem).name().unwrap();
        let def_value = CStr::from_ptr((*attr_decl).default_value as *const i8).to_string_lossy();
        xml_err_valid_node(
            ctxt,
            elem,
            XmlParserErrors::XmlDTDAttributeDefault,
            format!(
                "Value for attribute {attr_name} of {elem_name} is different from default \"{def_value}\"\n"
            )
            .as_str(),
            Some(&attr_name),
            Some(&elem_name),
            Some(&def_value),
        );
        ret = 0;
    }

    /* Validity Constraint: ID uniqueness */
    if matches!((*attr_decl).atype, XmlAttributeType::XmlAttributeID)
        && xml_add_id(ctxt, doc, value, attr).is_null()
    {
        ret = 0;
    }

    if matches!(
        (*attr_decl).atype,
        XmlAttributeType::XmlAttributeIDREF | XmlAttributeType::XmlAttributeIDREFS
    ) && xml_add_ref(ctxt, doc, value, attr).is_null()
    {
        ret = 0;
    }

    /* Validity Constraint: Notation Attributes */
    if matches!((*attr_decl).atype, XmlAttributeType::XmlAttributeNotation) {
        let mut tree: XmlEnumerationPtr = (*attr_decl).tree;
        let mut nota: XmlNotationPtr;

        /* First check that the given NOTATION was declared */
        nota = xml_get_dtd_notation_desc((*doc).int_subset, value);
        if nota.is_null() {
            nota = xml_get_dtd_notation_desc((*doc).ext_subset, value);
        }

        if nota.is_null() {
            let value = CStr::from_ptr(value as *const i8).to_string_lossy();
            let attr_name = (*attr).name().unwrap();
            let elem_name = (*elem).name().unwrap();
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlDTDUnknownNotation,
                format!("Value \"{value}\" for attribute {attr_name} of {elem_name} is not a declared Notation\n").as_str(),
                Some(&value),
                Some(&attr_name),
                Some(&elem_name),
            );
            ret = 0;
        }

        // Second, verify that it's among the list
        while !tree.is_null() {
            if (*tree).name.as_deref()
                == Some(CStr::from_ptr(value as *const i8).to_string_lossy()).as_deref()
            {
                break;
            }
            tree = (*tree).next;
        }
        if tree.is_null() {
            let value = CStr::from_ptr(value as *const i8).to_string_lossy();
            let attr_name = (*attr).name().unwrap();
            let elem_name = (*elem).name().unwrap();
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlDTDNotationValue,
                format!("Value \"{value}\" for attribute {attr_name} of {elem_name} is not among the enumerated notations\n").as_str(),
                Some(&value),
                Some(&attr_name),
                Some(&elem_name),
            );
            ret = 0;
        }
    }

    // Validity Constraint: Enumeration
    if matches!(
        (*attr_decl).atype,
        XmlAttributeType::XmlAttributeEnumeration
    ) {
        let mut tree: XmlEnumerationPtr = (*attr_decl).tree;
        while !tree.is_null() {
            if (*tree).name.as_deref()
                == Some(CStr::from_ptr(value as *const i8).to_string_lossy()).as_deref()
            {
                break;
            }
            tree = (*tree).next;
        }
        if tree.is_null() {
            let value = CStr::from_ptr(value as *const i8).to_string_lossy();
            let attr_name = (*attr).name().unwrap();
            let elem_name = (*elem).name().unwrap();
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlDTDAttributeValue,
                format!("Value \"{value}\" for attribute {attr_name} of {elem_name} is not among the enumerated set\n")
                    .as_str(),
                Some(&value),
                Some(&attr_name),
                Some(&elem_name),
            );
            ret = 0;
        }
    }

    // Fixed Attribute Default
    if matches!((*attr_decl).def, XmlAttributeDefault::XmlAttributeFixed)
        && !xml_str_equal((*attr_decl).default_value, value)
    {
        let attr_name = (*attr).name().unwrap();
        let elem_name = (*elem).name().unwrap();
        let def_value = CStr::from_ptr((*attr_decl).default_value as *const i8).to_string_lossy();
        xml_err_valid_node(
            ctxt,
            elem,
            XmlParserErrors::XmlDTDAttributeValue,
            format!("Value for attribute {attr_name} of {elem_name} must be \"{def_value}\"\n")
                .as_str(),
            Some(&attr_name),
            Some(&elem_name),
            Some(&def_value),
        );
        ret = 0;
    }

    // Extra check for the attribute value
    ret &= xml_validate_attribute_value2(
        ctxt,
        doc,
        (*attr).name,
        (*attr_decl).atype,
        &CStr::from_ptr(value as *const i8).to_string_lossy(),
    );

    ret
}

/// Try to validate a single namespace declaration for an element
/// basically it does the following checks as described by the
/// XML-1.0 recommendation:
///  - [ VC: Attribute Value Type ]
///  - [ VC: Fixed Attribute Default ]
///  - [ VC: Entity Name ]
///  - [ VC: Name Token ]
///  - [ VC: ID ]
///  - [ VC: IDREF ]
///  - [ VC: Entity Name ]
///  - [ VC: Notation Attributes ]
///
/// The ID/IDREF uniqueness and matching are done separately
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateOneNamespace")]
#[cfg(feature = "libxml_valid")]
pub unsafe extern "C" fn xml_validate_one_namespace(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    elem: XmlNodePtr,
    prefix: *const XmlChar,
    ns: XmlNsPtr,
    value: *const XmlChar,
) -> i32 {
    /* let elemDecl: xmlElementPtr; */
    let mut attr_decl: XmlAttributePtr = null_mut();
    let mut ret: i32 = 1;

    CHECK_DTD!(doc);
    if elem.is_null() || (*elem).name.is_null() {
        return 0;
    }
    if ns.is_null() || (*ns).href.is_null() {
        return 0;
    }

    if !prefix.is_null() {
        let mut fname: [XmlChar; 50] = [0; 50];

        let fullname: *mut XmlChar = xml_build_qname((*elem).name, prefix, fname.as_mut_ptr(), 50);
        if fullname.is_null() {
            xml_verr_memory(ctxt, Some("Validating namespace"));
            return 0;
        }
        if !(*ns).prefix.is_null() {
            attr_decl = (*(*doc).int_subset).get_qattr_desc(
                CStr::from_ptr(fullname as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                CStr::from_ptr((*ns).prefix as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                Some("xmlns"),
            );
            if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                attr_decl = (*(*doc).ext_subset).get_qattr_desc(
                    CStr::from_ptr(fullname as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    CStr::from_ptr((*ns).prefix as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    Some("xmlns"),
                );
            }
        } else {
            attr_decl = (*(*doc).int_subset).get_attr_desc(
                CStr::from_ptr(fullname as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                "xmlns",
            );
            if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                attr_decl = (*(*doc).ext_subset).get_attr_desc(
                    CStr::from_ptr(fullname as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    "xmlns",
                );
            }
        }
        if fullname != fname.as_ptr() as _ && fullname != (*elem).name as _ {
            xml_free(fullname as _);
        }
    }
    if attr_decl.is_null() {
        if !(*ns).prefix.is_null() {
            attr_decl = (*(*doc).int_subset).get_qattr_desc(
                (*elem).name().unwrap().as_ref(),
                CStr::from_ptr((*ns).prefix as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                Some("xmlns"),
            );
            if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                attr_decl = (*(*doc).ext_subset).get_qattr_desc(
                    (*elem).name().unwrap().as_ref(),
                    CStr::from_ptr((*ns).prefix as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    Some("xmlns"),
                );
            }
        } else {
            attr_decl =
                (*(*doc).int_subset).get_attr_desc((*elem).name().as_deref().unwrap(), "xmlns");
            if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                attr_decl =
                    (*(*doc).ext_subset).get_attr_desc((*elem).name().as_deref().unwrap(), "xmlns");
            }
        }
    }

    /* Validity Constraint: Attribute Value Type */
    if attr_decl.is_null() {
        if !(*ns).prefix.is_null() {
            let prefix = CStr::from_ptr((*ns).prefix as *const i8).to_string_lossy();
            let elem_name = (*elem).name().unwrap();
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlDTDUnknownAttribute,
                format!("No declaration for attribute xmlns:{prefix} of element {elem_name}\n")
                    .as_str(),
                Some(&prefix),
                Some(&elem_name),
                None,
            );
        } else {
            let elem_name = (*elem).name().unwrap();
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlDTDUnknownAttribute,
                format!("No declaration for attribute xmlns of element {elem_name}\n").as_str(),
                Some(&elem_name),
                None,
                None,
            );
        }
        return 0;
    }

    let val: i32 = xml_validate_attribute_value_internal(doc, (*attr_decl).atype, value);
    if val == 0 {
        if !(*ns).prefix.is_null() {
            let prefix = CStr::from_ptr((*ns).prefix as *const i8).to_string_lossy();
            let elem_name = (*elem).name().unwrap();
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlDTDInvalidDefault,
                format!(
                    "Syntax of value for attribute xmlns:{prefix} of {elem_name} is not valid\n"
                )
                .as_str(),
                Some(&prefix),
                Some(&elem_name),
                None,
            );
        } else {
            let elem_name = (*elem).name().unwrap();
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlDTDInvalidDefault,
                format!("Syntax of value for attribute xmlns of {elem_name} is not valid\n")
                    .as_str(),
                Some(&elem_name),
                None,
                None,
            );
        }
        ret = 0;
    }

    // Validity constraint: Fixed Attribute Default
    if matches!((*attr_decl).def, XmlAttributeDefault::XmlAttributeFixed)
        && !xml_str_equal(value, (*attr_decl).default_value)
    {
        if !(*ns).prefix.is_null() {
            let prefix = CStr::from_ptr((*ns).prefix as *const i8).to_string_lossy();
            let elem_name = (*elem).name().unwrap();
            let def_value =
                CStr::from_ptr((*attr_decl).default_value as *const i8).to_string_lossy();
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlDTDAttributeDefault,
                format!("Value for attribute xmlns:{prefix} of {elem_name} is different from default \"{def_value}\"\n").as_str(),
                Some(&prefix),
                Some(&elem_name),
                Some(&def_value),
            );
        } else {
            let elem_name = (*elem).name().unwrap();
            let def_value =
                CStr::from_ptr((*attr_decl).default_value as *const i8).to_string_lossy();
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlDTDAttributeDefault,
                format!("Value for attribute xmlns of {elem_name} is different from default \"{def_value}\"\n")
                    .as_str(),
                Some(&elem_name),
                Some(&def_value),
                None,
            );
        }
        ret = 0;
    }

    /*
     * Casting ns to xmlAttrPtr is wrong. We'd need separate functions
     * xmlAddID and xmlAddRef for namespace declarations, but it makes
     * no practical sense to use ID types anyway.
     */
    // #if 0
    // /* Validity Constraint: ID uniqueness */
    // if ((*attrDecl).atype == XML_ATTRIBUTE_ID) {
    //     if (xmlAddID(ctxt, doc, value, (xmlAttrPtr) ns).is_null())
    //         ret = 0;
    // }
    // if (((*attrDecl).atype == XML_ATTRIBUTE_IDREF) || ((*attrDecl).atype == XML_ATTRIBUTE_IDREFS)) {
    //     if (xmlAddRef(ctxt, doc, value, (xmlAttrPtr) ns).is_null())
    // 	       ret = 0;
    // }
    // #endif

    // Validity Constraint: Notation Attributes
    if matches!((*attr_decl).atype, XmlAttributeType::XmlAttributeNotation) {
        let mut tree: XmlEnumerationPtr = (*attr_decl).tree;
        let mut nota: XmlNotationPtr;

        // First check that the given NOTATION was declared
        nota = xml_get_dtd_notation_desc((*doc).int_subset, value);
        if nota.is_null() {
            nota = xml_get_dtd_notation_desc((*doc).ext_subset, value);
        }

        if nota.is_null() {
            if !(*ns).prefix.is_null() {
                let value = CStr::from_ptr(value as *const i8).to_string_lossy();
                let prefix = CStr::from_ptr((*ns).prefix as *const i8).to_string_lossy();
                let elem_name = (*elem).name().unwrap();
                xml_err_valid_node(
                    ctxt,
                    elem,
                    XmlParserErrors::XmlDTDUnknownNotation,
                    format!("Value \"{value}\" for attribute xmlns:{prefix} of {elem_name} is not a declared Notation\n").as_str(),
                    Some(&value),
                    Some(&prefix),
                    Some(&elem_name),
                );
            } else {
                let value = CStr::from_ptr(value as *const i8).to_string_lossy();
                let elem_name = (*elem).name().unwrap();
                xml_err_valid_node(
                    ctxt,
                    elem,
                    XmlParserErrors::XmlDTDUnknownNotation,
                    format!("Value \"{value}\" for attribute xmlns of {elem_name} is not a declared Notation\n").as_str(),
                    Some(&value),
                    Some(&elem_name),
                    None,
                );
            }
            ret = 0;
        }

        // Second, verify that it's among the list
        while !tree.is_null() {
            if (*tree).name.as_deref()
                == Some(CStr::from_ptr(value as *const i8).to_string_lossy()).as_deref()
            {
                break;
            }
            tree = (*tree).next;
        }
        if tree.is_null() {
            let value = CStr::from_ptr(value as *const i8).to_string_lossy();
            let prefix = CStr::from_ptr((*ns).prefix as *const i8).to_string_lossy();
            let elem_name = (*elem).name().unwrap();
            if !(*ns).prefix.is_null() {
                xml_err_valid_node(
                    ctxt,
                    elem,
                    XmlParserErrors::XmlDTDNotationValue,
                    format!("Value \"{value}\" for attribute xmlns:{prefix} of {elem_name} is not among the enumerated notations\n").as_str(),
                    Some(&value),
                    Some(&prefix),
                    Some(&elem_name),
                );
            } else {
                xml_err_valid_node(
                    ctxt,
                    elem,
                    XmlParserErrors::XmlDTDNotationValue,
                    format!("Value \"{value}\" for attribute xmlns of {elem_name} is not among the enumerated notations\n").as_str(),
                    Some(&value),
                    Some(&elem_name),
                    None,
                );
            }
            ret = 0;
        }
    }

    // Validity Constraint: Enumeration
    if matches!(
        (*attr_decl).atype,
        XmlAttributeType::XmlAttributeEnumeration
    ) {
        let mut tree: XmlEnumerationPtr = (*attr_decl).tree;
        while !tree.is_null() {
            if (*tree).name.as_deref()
                == Some(CStr::from_ptr(value as *const i8).to_string_lossy()).as_deref()
            {
                break;
            }
            tree = (*tree).next;
        }
        if tree.is_null() {
            if !(*ns).prefix.is_null() {
                let value = CStr::from_ptr(value as *const i8).to_string_lossy();
                let prefix = CStr::from_ptr((*ns).prefix as *const i8).to_string_lossy();
                let elem_name = (*elem).name().unwrap();
                xml_err_valid_node(
                    ctxt,
                    elem,
                    XmlParserErrors::XmlDTDAttributeValue,
                    format!("Value \"{value}\" for attribute xmlns:{prefix} of {elem_name} is not among the enumerated set\n").as_str(),
                    Some(&value),
                    Some(&prefix),
                    Some(&elem_name),
                );
            } else {
                let value = CStr::from_ptr(value as *const i8).to_string_lossy();
                let elem_name = (*elem).name().unwrap();
                xml_err_valid_node(
                    ctxt,
                    elem,
                    XmlParserErrors::XmlDTDAttributeValue,
                    format!(
                        "Value \"{value}\" for attribute xmlns of {elem_name} is not among the enumerated set\n"
                    )
                    .as_str(),
                    Some(&value),
                    Some(&elem_name),
                    None,
                );
            }
            ret = 0;
        }
    }

    /* Fixed Attribute Default */
    if matches!((*attr_decl).def, XmlAttributeDefault::XmlAttributeFixed)
        && !xml_str_equal((*attr_decl).default_value, value)
    {
        if !(*ns).prefix.is_null() {
            let prefix = CStr::from_ptr((*ns).prefix as *const i8).to_string_lossy();
            let elem_name = (*elem).name().unwrap();
            let def_value =
                CStr::from_ptr((*attr_decl).default_value as *const i8).to_string_lossy();
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlDTDElemNamespace,
                format!(
                    "Value for attribute xmlns:{prefix} of {elem_name} must be \"{def_value}\"\n"
                )
                .as_str(),
                Some(&prefix),
                Some(&elem_name),
                Some(&def_value),
            );
        } else {
            let elem_name = (*elem).name().unwrap();
            let def_value =
                CStr::from_ptr((*attr_decl).default_value as *const i8).to_string_lossy();
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlDTDElemNamespace,
                format!("Value for attribute xmlns of {elem_name} must be \"{def_value}\"\n")
                    .as_str(),
                Some(&elem_name),
                Some(&def_value),
                None,
            );
        }
        ret = 0;
    }

    // Extra check for the attribute value
    let value = CStr::from_ptr(value as *const i8).to_string_lossy();
    if !(*ns).prefix.is_null() {
        ret &=
            xml_validate_attribute_value2(ctxt, doc, (*ns).prefix as _, (*attr_decl).atype, &value);
    } else {
        ret &= xml_validate_attribute_value2(
            ctxt,
            doc,
            c"xmlns".as_ptr() as _,
            (*attr_decl).atype,
            &value,
        );
    }

    ret
}

pub type XmlValidateMemoPtr = *mut XmlValidateMemo;
pub struct XmlValidateMemo {
    ctxt: XmlValidCtxtPtr,
    name: *const XmlChar,
}

#[doc(alias = "xmlValidateRef")]
unsafe extern "C" fn xml_validate_ref(
    refe: XmlRefPtr,
    ctxt: XmlValidCtxtPtr,
    name: *const XmlChar,
) {
    if refe.is_null() {
        return;
    }
    if (*refe).attr.is_null() && (*refe).name.is_null() {
        return;
    }
    let attr: XmlAttrPtr = (*refe).attr;
    if attr.is_null() {
        let mut str: *mut XmlChar;
        let mut cur: *mut XmlChar;
        let mut save: XmlChar;

        let dup: *mut XmlChar = xml_strdup(name);
        if dup.is_null() {
            (*ctxt).valid = 0;
            return;
        }
        cur = dup;
        while *cur != 0 {
            str = cur;
            while *cur != 0 && !xml_is_blank_char(*cur as u32) {
                cur = cur.add(1);
            }
            save = *cur;
            *cur = 0;
            if xml_get_id((*ctxt).doc, str).is_none() {
                xml_err_valid_node_nr!(
                    ctxt,
                    null_mut(),
                    XmlParserErrors::XmlDTDUnknownID,
                    "attribute {} line {} references an unknown ID \"{}\"\n",
                    CStr::from_ptr((*refe).name as *const i8).to_string_lossy(),
                    (*refe).lineno,
                    CStr::from_ptr(str as *const i8).to_string_lossy()
                );
                (*ctxt).valid = 0;
            }
            if save == 0 {
                break;
            }
            *cur = save;
            while xml_is_blank_char(*cur as u32) {
                cur = cur.add(1);
            }
        }
        xml_free(dup as _);
    } else if matches!((*attr).atype, Some(XmlAttributeType::XmlAttributeIDREF)) {
        if xml_get_id((*ctxt).doc, name).is_none() {
            let attr_name = (*attr).name().unwrap();
            let name = CStr::from_ptr(name as *const i8).to_string_lossy();
            xml_err_valid_node(
                ctxt,
                (*attr).parent.map_or(null_mut(), |p| p.as_ptr()),
                XmlParserErrors::XmlDTDUnknownID,
                format!("IDREF attribute {attr_name} references an unknown ID \"{name}\"\n")
                    .as_str(),
                Some(&attr_name),
                Some(&name),
                None,
            );
            (*ctxt).valid = 0;
        }
    } else if matches!((*attr).atype, Some(XmlAttributeType::XmlAttributeIDREFS)) {
        let mut str: *mut XmlChar;
        let mut cur: *mut XmlChar;
        let mut save: XmlChar;

        let dup: *mut XmlChar = xml_strdup(name);
        if dup.is_null() {
            xml_verr_memory(ctxt, Some("IDREFS split"));
            (*ctxt).valid = 0;
            return;
        }
        cur = dup;
        while *cur != 0 {
            str = cur;
            while *cur != 0 && !xml_is_blank_char(*cur as u32) {
                cur = cur.add(1);
            }
            save = *cur;
            *cur = 0;
            if xml_get_id((*ctxt).doc, str).is_none() {
                let attr_name = (*attr).name().unwrap();
                let str = CStr::from_ptr(str as *const i8).to_string_lossy();
                xml_err_valid_node(
                    ctxt,
                    (*attr).parent.map_or(null_mut(), |p| p.as_ptr()),
                    XmlParserErrors::XmlDTDUnknownID,
                    format!("IDREFS attribute {attr_name} references an unknown ID \"{str}\"\n")
                        .as_str(),
                    Some(&attr_name),
                    Some(&str),
                    None,
                );
                (*ctxt).valid = 0;
            }
            if save == 0 {
                break;
            }
            *cur = save;
            while xml_is_blank_char(*cur as u32) {
                cur = cur.add(1);
            }
        }
        xml_free(dup as _);
    }
}

/// Returns 0 to abort the walk or 1 to continue
#[doc(alias = "xmlWalkValidateList")]
extern "C" fn xml_walk_validate_list(data: *const c_void, user: *mut c_void) -> i32 {
    unsafe {
        let memo: XmlValidateMemoPtr = user as XmlValidateMemoPtr;
        xml_validate_ref(data as XmlRefPtr, (*memo).ctxt, (*memo).name);
    }
    1
}

#[doc(alias = "xmlValidateCheckRefCallback")]
extern "C" fn xml_validate_check_ref_callback(
    payload: *mut c_void,
    data: *mut c_void,
    name: *const XmlChar,
) {
    let ref_list: XmlListPtr = payload as XmlListPtr;
    let ctxt: XmlValidCtxtPtr = data as XmlValidCtxtPtr;
    let mut memo: XmlValidateMemo = unsafe { zeroed() };

    if ref_list.is_null() {
        return;
    }
    memo.ctxt = ctxt;
    memo.name = name;

    xml_list_walk(
        ref_list,
        Some(xml_walk_validate_list),
        addr_of_mut!(memo) as _,
    );
}

/// Does the final step for the document validation once all the
/// incremental validation steps have been completed
///
/// basically it does the following checks described by the XML Rec
///
/// Check all the IDREF/IDREFS attributes definition for validity
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateDocumentFinal")]
#[cfg(feature = "libxml_valid")]
pub unsafe extern "C" fn xml_validate_document_final(ctxt: XmlValidCtxtPtr, doc: XmlDocPtr) -> i32 {
    if ctxt.is_null() {
        return 0;
    }
    if doc.is_null() {
        xml_err_valid!(
            ctxt,
            XmlParserErrors::XmlDTDNoDoc,
            "xmlValidateDocumentFinal: doc == NULL\n"
        );
        return 0;
    }

    /* trick to get correct line id report */
    let save: u32 = (*ctxt).flags;
    (*ctxt).flags &= !XML_VCTXT_USE_PCTXT as u32;

    // Check all the NOTATION/NOTATIONS attributes
    // Check all the ENTITY/ENTITIES attributes definition for validity
    // Check all the IDREF/IDREFS attributes definition for validity
    let table: XmlRefTablePtr = (*doc).refs as XmlRefTablePtr;
    (*ctxt).doc = doc;
    (*ctxt).valid = 1;
    xml_hash_scan(table, Some(xml_validate_check_ref_callback), ctxt as _);

    (*ctxt).flags = save;
    (*ctxt).valid
}

/// Validate that the given name match a notation declaration.
/// - [ VC: Notation Declared ]
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateNotationUse")]
#[cfg(any(feature = "libxml_valid", feature = "schema"))]
pub unsafe extern "C" fn xml_validate_notation_use(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    notation_name: *const XmlChar,
) -> i32 {
    let mut nota_decl: XmlNotationPtr;
    if doc.is_null() || (*doc).int_subset.is_null() || notation_name.is_null() {
        return -1;
    }

    nota_decl = xml_get_dtd_notation_desc((*doc).int_subset, notation_name);
    if nota_decl.is_null() && !(*doc).ext_subset.is_null() {
        nota_decl = xml_get_dtd_notation_desc((*doc).ext_subset, notation_name);
    }

    if nota_decl.is_null() && !ctxt.is_null() {
        let notation_name = CStr::from_ptr(notation_name as *const i8).to_string_lossy();
        xml_err_valid_node(
            ctxt,
            doc as XmlNodePtr,
            XmlParserErrors::XmlDTDUnknownNotation,
            format!("NOTATION {notation_name} is not declared\n").as_str(),
            Some(&notation_name),
            None,
            None,
        );
        return 0;
    }
    1
}

/// Search in the DtDs whether an element accept Mixed content (or ANY)
/// basically if it is supposed to accept text childs
///
/// returns 0 if no, 1 if yes, and -1 if no element description is available
#[doc(alias = "xmlIsMixedElement")]
pub unsafe extern "C" fn xml_is_mixed_element(doc: XmlDocPtr, name: *const XmlChar) -> i32 {
    let mut elem_decl: XmlElementPtr;

    if doc.is_null() || (*doc).int_subset.is_null() {
        return -1;
    }

    elem_decl = xml_get_dtd_element_desc((*doc).int_subset, name);
    if elem_decl.is_null() && !(*doc).ext_subset.is_null() {
        elem_decl = xml_get_dtd_element_desc((*doc).ext_subset, name);
    }
    if elem_decl.is_null() {
        return -1;
    }
    match (*elem_decl).etype {
        XmlElementTypeVal::XmlElementTypeUndefined => {
            -1
        }
        XmlElementTypeVal::XmlElementTypeElement => {
            0
        }
        XmlElementTypeVal::XmlElementTypeEmpty
        /*
         * return 1 for EMPTY since we want VC error to pop up
         * on <empty>     </empty> for example
         */
        | XmlElementTypeVal::XmlElementTypeAny
        | XmlElementTypeVal::XmlElementTypeMixed => {
            1
        }
    }
}

/// Search the DTD for the description of this notation
///
/// returns the xmlNotationPtr if found or null_mut()
#[doc(alias = "xmlGetDtdNotationDesc")]
pub unsafe extern "C" fn xml_get_dtd_notation_desc(
    dtd: XmlDtdPtr,
    name: *const XmlChar,
) -> XmlNotationPtr {
    if dtd.is_null() {
        return null_mut();
    }
    if (*dtd).notations.is_null() {
        return null_mut();
    }
    let table: XmlNotationTablePtr = (*dtd).notations as XmlNotationTablePtr;

    xml_hash_lookup(table, name) as _
}

/// Search the DTD for the description of this element
///
/// returns the xmlElementPtr if found or null_mut()
#[doc(alias = "xmlGetDtdQElementDesc")]
pub unsafe extern "C" fn xml_get_dtd_qelement_desc(
    dtd: XmlDtdPtr,
    name: *const XmlChar,
    prefix: *const XmlChar,
) -> XmlElementPtr {
    if dtd.is_null() {
        return null_mut();
    }
    if (*dtd).elements.is_null() {
        return null_mut();
    }
    let table: XmlElementTablePtr = (*dtd).elements as XmlElementTablePtr;

    xml_hash_lookup2(table, name, prefix) as _
}

/// Search the DTD for the description of this element
///
/// returns the xmlElementPtr if found or null_mut()
#[doc(alias = "xmlGetDtdElementDesc")]
pub unsafe extern "C" fn xml_get_dtd_element_desc(
    dtd: XmlDtdPtr,
    mut name: *const XmlChar,
) -> XmlElementPtr {
    let mut prefix: *mut XmlChar = null_mut();

    if dtd.is_null() || name.is_null() {
        return null_mut();
    }
    if (*dtd).elements.is_null() {
        return null_mut();
    }
    let table: XmlElementTablePtr = (*dtd).elements as XmlElementTablePtr;

    let uqname: *mut XmlChar = xml_split_qname2(name, addr_of_mut!(prefix));
    if !uqname.is_null() {
        name = uqname;
    }
    let cur: XmlElementPtr = xml_hash_lookup2(table, name, prefix) as _;
    if !prefix.is_null() {
        xml_free(prefix as _);
    }
    if !uqname.is_null() {
        xml_free(uqname as _);
    }
    cur
}

/// Build/extend a list of  potential children allowed by the content tree
///
/// Returns the number of element in the list, or -1 in case of error.
#[doc(alias = "xmlValidGetPotentialChildren")]
#[cfg(feature = "libxml_valid")]
pub unsafe extern "C" fn xml_valid_get_potential_children(
    ctree: *mut XmlElementContent,
    names: *mut *const XmlChar,
    len: *mut i32,
    max: i32,
) -> i32 {
    if ctree.is_null() || names.is_null() || len.is_null() {
        return -1;
    }
    if *len >= max {
        return *len;
    }

    match (*ctree).typ {
        XmlElementContentType::XmlElementContentPCDATA => {
            for i in 0..*len {
                if xml_str_equal(c"#PCDATA".as_ptr() as _, *names.add(i as usize)) {
                    return *len;
                }
            }
            *names.add(*len as usize) = c"#PCDATA".as_ptr() as _;
            *len += 1;
        }
        XmlElementContentType::XmlElementContentElement => {
            for i in 0..*len {
                if xml_str_equal((*ctree).name, *names.add(i as usize)) {
                    return *len;
                }
            }
            *names.add(*len as usize) = (*ctree).name;
            *len += 1;
        }
        XmlElementContentType::XmlElementContentSeq => {
            xml_valid_get_potential_children((*ctree).c1, names, len, max);
            xml_valid_get_potential_children((*ctree).c2, names, len, max);
        }
        XmlElementContentType::XmlElementContentOr => {
            xml_valid_get_potential_children((*ctree).c1, names, len, max);
            xml_valid_get_potential_children((*ctree).c2, names, len, max);
        }
    }

    *len
}

// Dummy function to suppress messages while we try out valid elements
fn xml_no_validity_err(_ctx: Option<GenericErrorContext>, _msg: &str) {}

/// This function returns the list of authorized children to insert
/// within an existing tree while respecting the validity constraints
/// forced by the Dtd. The insertion point is defined using @prev and
/// @next in the following ways:
///  to insert before 'node': xmlValidGetValidElements((*node).prev, node, ...
///  to insert next 'node': xmlValidGetValidElements(node, (*node).next, ...
///  to replace 'node': xmlValidGetValidElements((*node).prev, (*node).next, ...
///  to prepend a child to 'node': xmlValidGetValidElements(null_mut(), (*node).childs,
///  to append a child to 'node': xmlValidGetValidElements((*node).last, null_mut(), ...
///
/// pointers to the element names are inserted at the beginning of the array
/// and do not need to be freed.
///
/// returns the number of element in the list, or -1 in case of error. If
/// the function returns the value @max the caller is invited to grow the
/// receiving array and retry.
#[doc(alias = "xmlValidGetValidElements")]
#[cfg(feature = "libxml_valid")]
pub unsafe extern "C" fn xml_valid_get_valid_elements(
    prev: *mut XmlNode,
    next: *mut XmlNode,
    names: *mut *const XmlChar,
    max: i32,
) -> i32 {
    use crate::tree::NodePtr;

    let mut vctxt: XmlValidCtxt = unsafe { zeroed() };
    let mut nb_valid_elements: i32;
    let mut elements: [*const XmlChar; 256] = [null(); 256];
    let mut nb_elements: i32 = 0;
    let mut element_desc: *mut XmlElement;

    if prev.is_null() && next.is_null() {
        return -1;
    }

    if names.is_null() {
        return -1;
    }
    if max <= 0 {
        return -1;
    }

    memset(addr_of_mut!(vctxt) as _, 0, size_of::<XmlValidCtxt>());
    vctxt.error = Some(xml_no_validity_err); /* this suppresses err/warn output */

    nb_valid_elements = 0;
    let ref_node: *mut XmlNode = if !prev.is_null() { prev } else { next };
    let parent: *mut XmlNode = (*ref_node).parent().map_or(null_mut(), |p| p.as_ptr());

    // Retrieves the parent element declaration
    element_desc = xml_get_dtd_element_desc((*(*parent).doc).int_subset, (*parent).name);
    if element_desc.is_null() && !(*(*parent).doc).ext_subset.is_null() {
        element_desc = xml_get_dtd_element_desc((*(*parent).doc).ext_subset, (*parent).name);
    }
    if element_desc.is_null() {
        return -1;
    }

    // Do a backup of the current tree structure
    let prev_next: *mut XmlNode = if !prev.is_null() {
        (*prev).next.map_or(null_mut(), |n| n.as_ptr())
    } else {
        null_mut()
    };
    let next_prev: *mut XmlNode = if !next.is_null() {
        (*next).prev.map_or(null_mut(), |p| p.as_ptr())
    } else {
        null_mut()
    };
    let parent_childs: *mut XmlNode = (*parent).children().map_or(null_mut(), |c| c.as_ptr());
    let parent_last: *mut XmlNode = (*parent).last().map_or(null_mut(), |l| l.as_ptr());

    // Creates a dummy node and insert it into the tree
    let test_node: *mut XmlNode = xml_new_doc_node(
        (*ref_node).doc,
        null_mut(),
        c"<!dummy?>".as_ptr() as _,
        null_mut(),
    );
    if test_node.is_null() {
        return -1;
    }

    (*test_node).set_parent(NodePtr::from_ptr(parent));
    (*test_node).prev = NodePtr::from_ptr(prev);
    (*test_node).next = NodePtr::from_ptr(next);
    let name: *const XmlChar = (*test_node).name;

    if !prev.is_null() {
        (*prev).next = NodePtr::from_ptr(test_node);
    } else {
        (*parent).set_children(NodePtr::from_ptr(test_node));
    }

    if !next.is_null() {
        (*next).prev = NodePtr::from_ptr(test_node);
    } else {
        (*parent).set_last(NodePtr::from_ptr(test_node));
    }

    // Insert each potential child node and check if the parent is still valid
    nb_elements = xml_valid_get_potential_children(
        (*element_desc).content,
        elements.as_mut_ptr(),
        addr_of_mut!(nb_elements),
        256,
    );

    for i in 0..nb_elements {
        (*test_node).name = elements[i as usize];
        if xml_validate_one_element(addr_of_mut!(vctxt) as _, (*parent).doc, parent) != 0 {
            for j in 0..nb_valid_elements {
                if xml_str_equal(elements[i as usize], *names.add(j as usize)) {
                    break;
                }
            }
            *names.add(nb_valid_elements as usize) = elements[i as usize];
            nb_valid_elements += 1;
            if nb_valid_elements >= max {
                break;
            }
        }
    }

    // Restore the tree structure
    if !prev.is_null() {
        (*prev).next = NodePtr::from_ptr(prev_next);
    }
    if !next.is_null() {
        (*next).prev = NodePtr::from_ptr(next_prev);
    }
    (*parent).set_children(NodePtr::from_ptr(parent_childs));
    (*parent).set_last(NodePtr::from_ptr(parent_last));

    // Free up the dummy node
    (*test_node).name = name;
    xml_free_node(test_node);

    nb_valid_elements
}

/// Validate that the given value match Name production
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateNameValue")]
#[cfg(feature = "libxml_valid")]
pub unsafe extern "C" fn xml_validate_name_value(value: *const XmlChar) -> i32 {
    xml_validate_name_value_internal(null_mut(), value)
}

/// Validate that the given value match Names production
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateNamesValue")]
#[cfg(feature = "libxml_valid")]
pub unsafe extern "C" fn xml_validate_names_value(value: *const XmlChar) -> i32 {
    xml_validate_names_value_internal(null_mut(), value)
}

/// Validate that the given value match Nmtoken production
///
/// `[ VC: Name Token ]`
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateNmtokenValue")]
#[cfg(feature = "libxml_valid")]
pub unsafe extern "C" fn xml_validate_nmtoken_value(value: *const XmlChar) -> i32 {
    xml_validate_nmtoken_value_internal(null_mut(), value)
}

/// Validate that the given value match Nmtokens production
///
/// `[ VC: Name Token ]`
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateNmtokensValue")]
#[cfg(feature = "libxml_valid")]
pub unsafe extern "C" fn xml_validate_nmtokens_value(value: *const XmlChar) -> i32 {
    xml_validate_nmtokens_value_internal(null_mut(), value)
}

/// Generate the automata sequence needed for that type
///
/// Returns 1 if successful or 0 in case of error.
#[doc(alias = "xmlValidBuildAContentModel")]
unsafe extern "C" fn xml_valid_build_acontent_model(
    mut content: XmlElementContentPtr,
    ctxt: XmlValidCtxtPtr,
    name: *const XmlChar,
) -> i32 {
    if content.is_null() {
        let name = CStr::from_ptr(name as *const i8).to_string_lossy();
        xml_err_valid_node(
            ctxt,
            null_mut(),
            XmlParserErrors::XmlErrInternalError,
            format!("Found NULL content in content model of {name}\n").as_str(),
            Some(&name),
            None,
            None,
        );
        return 0;
    }
    match (*content).typ {
        XmlElementContentType::XmlElementContentPCDATA => {
            let name = CStr::from_ptr(name as *const i8).to_string_lossy();
            xml_err_valid_node(
                ctxt,
                null_mut(),
                XmlParserErrors::XmlErrInternalError,
                format!("Found PCDATA in content model of {name}\n").as_str(),
                Some(&name),
                None,
                None,
            );
            return 0;
        }
        XmlElementContentType::XmlElementContentElement => {
            let oldstate: XmlAutomataStatePtr = (*ctxt).state;
            let mut fname: [XmlChar; 50] = [0; 50];

            let fullname: *mut XmlChar = xml_build_qname(
                (*content).name,
                (*content).prefix,
                fname.as_mut_ptr() as _,
                50,
            );
            if fullname.is_null() {
                xml_verr_memory(ctxt, Some("Building content model"));
                return 0;
            }

            match (*content).ocur {
                XmlElementContentOccur::XmlElementContentOnce => {
                    (*ctxt).state = xml_automata_new_transition(
                        (*ctxt).am,
                        (*ctxt).state,
                        null_mut(),
                        fullname,
                        null_mut(),
                    );
                }
                XmlElementContentOccur::XmlElementContentOpt => {
                    (*ctxt).state = xml_automata_new_transition(
                        (*ctxt).am,
                        (*ctxt).state,
                        null_mut(),
                        fullname,
                        null_mut(),
                    );
                    xml_automata_new_epsilon((*ctxt).am, oldstate, (*ctxt).state);
                }
                XmlElementContentOccur::XmlElementContentPlus => {
                    (*ctxt).state = xml_automata_new_transition(
                        (*ctxt).am,
                        (*ctxt).state,
                        null_mut(),
                        fullname,
                        null_mut(),
                    );
                    xml_automata_new_transition(
                        (*ctxt).am,
                        (*ctxt).state,
                        (*ctxt).state,
                        fullname,
                        null_mut(),
                    );
                }
                XmlElementContentOccur::XmlElementContentMult => {
                    (*ctxt).state = xml_automata_new_epsilon((*ctxt).am, (*ctxt).state, null_mut());
                    xml_automata_new_transition(
                        (*ctxt).am,
                        (*ctxt).state,
                        (*ctxt).state,
                        fullname,
                        null_mut(),
                    );
                }
            }
            if fullname != fname.as_ptr() as _ && fullname != (*content).name as _ {
                xml_free(fullname as _);
            }
        }
        XmlElementContentType::XmlElementContentSeq => {
            let mut oldstate: XmlAutomataStatePtr;

            /*
             * Simply iterate over the content
             */
            oldstate = (*ctxt).state;
            let ocur: XmlElementContentOccur = (*content).ocur;
            if !matches!(ocur, XmlElementContentOccur::XmlElementContentOnce) {
                (*ctxt).state = xml_automata_new_epsilon((*ctxt).am, oldstate, null_mut());
                oldstate = (*ctxt).state;
            }
            while {
                xml_valid_build_acontent_model((*content).c1, ctxt, name);
                content = (*content).c2;
                matches!((*content).typ, XmlElementContentType::XmlElementContentSeq)
                    && matches!(
                        (*content).ocur,
                        XmlElementContentOccur::XmlElementContentOnce
                    )
            } {}
            xml_valid_build_acontent_model(content, ctxt, name);
            let oldend: XmlAutomataStatePtr = (*ctxt).state;
            (*ctxt).state = xml_automata_new_epsilon((*ctxt).am, oldend, null_mut());
            match ocur {
                XmlElementContentOccur::XmlElementContentOnce => {}
                XmlElementContentOccur::XmlElementContentOpt => {
                    xml_automata_new_epsilon((*ctxt).am, oldstate, (*ctxt).state);
                }
                XmlElementContentOccur::XmlElementContentMult => {
                    xml_automata_new_epsilon((*ctxt).am, oldstate, (*ctxt).state);
                    xml_automata_new_epsilon((*ctxt).am, oldend, oldstate);
                }
                XmlElementContentOccur::XmlElementContentPlus => {
                    xml_automata_new_epsilon((*ctxt).am, oldend, oldstate);
                }
            }
        }
        XmlElementContentType::XmlElementContentOr => {
            let ocur: XmlElementContentOccur = (*content).ocur;
            if matches!(
                ocur,
                XmlElementContentOccur::XmlElementContentPlus
                    | XmlElementContentOccur::XmlElementContentMult
            ) {
                (*ctxt).state = xml_automata_new_epsilon((*ctxt).am, (*ctxt).state, null_mut());
            }
            let oldstate: XmlAutomataStatePtr = (*ctxt).state;
            let oldend: XmlAutomataStatePtr = xml_automata_new_state((*ctxt).am);

            /*
             * iterate over the subtypes and remerge the end with an
             * epsilon transition
             */
            while {
                (*ctxt).state = oldstate;
                xml_valid_build_acontent_model((*content).c1, ctxt, name);
                xml_automata_new_epsilon((*ctxt).am, (*ctxt).state, oldend);
                content = (*content).c2;
                (*content).typ == XmlElementContentType::XmlElementContentOr
                    && matches!(
                        (*content).ocur,
                        XmlElementContentOccur::XmlElementContentOnce
                    )
            } {}
            (*ctxt).state = oldstate;
            xml_valid_build_acontent_model(content, ctxt, name);
            xml_automata_new_epsilon((*ctxt).am, (*ctxt).state, oldend);
            (*ctxt).state = xml_automata_new_epsilon((*ctxt).am, oldend, null_mut());
            match ocur {
                XmlElementContentOccur::XmlElementContentOnce => {}
                XmlElementContentOccur::XmlElementContentOpt => {
                    xml_automata_new_epsilon((*ctxt).am, oldstate, (*ctxt).state);
                }
                XmlElementContentOccur::XmlElementContentMult => {
                    xml_automata_new_epsilon((*ctxt).am, oldstate, (*ctxt).state);
                    xml_automata_new_epsilon((*ctxt).am, oldend, oldstate);
                }
                XmlElementContentOccur::XmlElementContentPlus => {
                    xml_automata_new_epsilon((*ctxt).am, oldend, oldstate);
                }
            }
        } // _ => {
          //     xml_err_valid!(
          //         ctxt,
          //         XmlParserErrors::XmlErrInternalError,
          //         c"ContentModel broken for element %s\n".as_ptr() as _,
          //         name as *const c_char,
          //     );
          //     return 0;
          // }
    }
    1
}

/// (Re)Build the automata associated to the content model of this element
///
/// Returns 1 in case of success, 0 in case of error
#[doc(alias = "xmlValidBuildContentModel")]
#[cfg(all(feature = "libxml_valid", feature = "libxml_regexp"))]
pub unsafe extern "C" fn xml_valid_build_content_model(
    ctxt: XmlValidCtxtPtr,
    elem: XmlElementPtr,
) -> i32 {
    if ctxt.is_null() || elem.is_null() {
        return 0;
    }
    if !matches!((*elem).element_type(), XmlElementType::XmlElementDecl) {
        return 0;
    }
    if !matches!((*elem).etype, XmlElementTypeVal::XmlElementTypeElement) {
        return 1;
    }
    /* TODO: should we rebuild in this case ? */
    if !(*elem).cont_model.is_null() {
        if xml_regexp_is_determinist((*elem).cont_model) == 0 {
            (*ctxt).valid = 0;
            return 0;
        }
        return 1;
    }

    (*ctxt).am = xml_new_automata();
    let name = (*elem)
        .name
        .as_ref()
        .map(|n| CString::new(n.as_str()).unwrap());
    if (*ctxt).am.is_null() {
        let name = (*elem).name.as_deref().unwrap();
        xml_err_valid_node(
            ctxt,
            elem as XmlNodePtr,
            XmlParserErrors::XmlErrInternalError,
            format!("Cannot create automata for element {name}\n").as_str(),
            Some(name.as_str()),
            None,
            None,
        );
        return 0;
    }
    (*ctxt).state = xml_automata_get_init_state((*ctxt).am);
    xml_valid_build_acontent_model(
        (*elem).content,
        ctxt,
        name.as_ref().map_or(null(), |n| n.as_ptr() as *const u8),
    );
    xml_automata_set_final_state((*ctxt).am, (*ctxt).state);
    (*elem).cont_model = xml_automata_compile((*ctxt).am);
    if xml_regexp_is_determinist((*elem).cont_model) != 1 {
        let mut expr: [c_char; 5000] = [0; 5000];
        expr[0] = 0;
        xml_snprintf_element_content(expr.as_mut_ptr() as _, 5000, (*elem).content, 1);
        let name = (*elem).name.as_deref().unwrap();
        let expr = CStr::from_ptr(expr.as_ptr()).to_string_lossy();
        xml_err_valid_node(
            ctxt,
            elem as XmlNodePtr,
            XmlParserErrors::XmlDTDContentNotDeterminist,
            format!("Content model of {name} is not deterministic: {expr}\n").as_str(),
            Some(name.as_str()),
            Some(&expr),
            None,
        );
        (*ctxt).valid = 0;
        (*ctxt).state = null_mut();
        xml_free_automata((*ctxt).am);
        (*ctxt).am = null_mut();
        return 0;
    }
    (*ctxt).state = null_mut();
    xml_free_automata((*ctxt).am);
    (*ctxt).am = null_mut();
    1
}

/// Check if the given node is part of the content model.
///
/// Returns 1 if yes, 0 if no, -1 in case of error
#[doc(alias = "xmlValidateCheckMixed")]
#[cfg(feature = "libxml_regexp")]
unsafe extern "C" fn xml_validate_check_mixed(
    ctxt: XmlValidCtxtPtr,
    mut cont: XmlElementContentPtr,
    qname: *const XmlChar,
) -> i32 {
    let mut plen: i32 = 0;
    let name: *const XmlChar = xml_split_qname3(qname, addr_of_mut!(plen));

    if name.is_null() {
        while !cont.is_null() {
            if matches!((*cont).typ, XmlElementContentType::XmlElementContentElement) {
                if (*cont).prefix.is_null() && xml_str_equal((*cont).name, qname) {
                    return 1;
                }
            } else if matches!((*cont).typ, XmlElementContentType::XmlElementContentOr)
                && !(*cont).c1.is_null()
                && matches!(
                    (*(*cont).c1).typ,
                    XmlElementContentType::XmlElementContentElement
                )
            {
                if (*(*cont).c1).prefix.is_null() && xml_str_equal((*(*cont).c1).name, qname) {
                    return 1;
                }
            } else if !matches!((*cont).typ, XmlElementContentType::XmlElementContentOr)
                || (*cont).c1.is_null()
                || !matches!(
                    (*(*cont).c1).typ,
                    XmlElementContentType::XmlElementContentPCDATA
                )
            {
                xml_err_valid!(
                    null_mut(),
                    XmlParserErrors::XmlDTDMixedCorrupt,
                    "Internal: MIXED struct corrupted\n"
                );
                break;
            }
            cont = (*cont).c2;
        }
    } else {
        while !cont.is_null() {
            if matches!((*cont).typ, XmlElementContentType::XmlElementContentElement) {
                if !(*cont).prefix.is_null()
                    && xml_strncmp((*cont).prefix, qname, plen) == 0
                    && xml_str_equal((*cont).name, name)
                {
                    return 1;
                }
            } else if matches!((*cont).typ, XmlElementContentType::XmlElementContentOr)
                && !(*cont).c1.is_null()
                && matches!(
                    (*(*cont).c1).typ,
                    XmlElementContentType::XmlElementContentElement
                )
            {
                if !(*(*cont).c1).prefix.is_null()
                    && xml_strncmp((*(*cont).c1).prefix, qname, plen) == 0
                    && xml_str_equal((*(*cont).c1).name, name)
                {
                    return 1;
                }
            } else if !matches!((*cont).typ, XmlElementContentType::XmlElementContentOr)
                || (*cont).c1.is_null()
                || !matches!(
                    (*(*cont).c1).typ,
                    XmlElementContentType::XmlElementContentPCDATA
                )
            {
                xml_err_valid!(
                    ctxt,
                    XmlParserErrors::XmlDTDMixedCorrupt,
                    "Internal: MIXED struct corrupted\n"
                );
                break;
            }
            cont = (*cont).c2;
        }
    }
    0
}

#[cfg(feature = "libxml_regexp")]
unsafe extern "C" fn vstate_vpush(
    ctxt: XmlValidCtxtPtr,
    elem_decl: XmlElementPtr,
    node: XmlNodePtr,
) -> i32 {
    if (*ctxt).vstate_max == 0 || (*ctxt).vstate_tab.is_null() {
        (*ctxt).vstate_max = 10;
        (*ctxt).vstate_tab =
            xml_malloc((*ctxt).vstate_max as usize * size_of_val(&*(*ctxt).vstate_tab.add(0)))
                as *mut XmlValidState;
        if (*ctxt).vstate_tab.is_null() {
            xml_verr_memory(ctxt as _, Some("malloc failed"));
            return -1;
        }
    }

    if (*ctxt).vstate_nr >= (*ctxt).vstate_max {
        let tmp: *mut XmlValidState = xml_realloc(
            (*ctxt).vstate_tab as _,
            2 * (*ctxt).vstate_max as usize * size_of_val(&*(*ctxt).vstate_tab.add(0)),
        ) as *mut XmlValidState;
        if tmp.is_null() {
            xml_verr_memory(ctxt as _, Some("realloc failed"));
            return -1;
        }
        (*ctxt).vstate_max *= 2;
        (*ctxt).vstate_tab = tmp;
    }
    (*ctxt).vstate = (*ctxt).vstate_tab.add((*ctxt).vstate_nr as usize);
    (*(*ctxt).vstate_tab.add((*ctxt).vstate_nr as usize)).elem_decl = elem_decl;
    (*(*ctxt).vstate_tab.add((*ctxt).vstate_nr as usize)).node = node;
    if !elem_decl.is_null()
        && matches!((*elem_decl).etype, XmlElementTypeVal::XmlElementTypeElement)
    {
        if (*elem_decl).cont_model.is_null() {
            xml_valid_build_content_model(ctxt, elem_decl);
        }
        if !(*elem_decl).cont_model.is_null() {
            (*(*ctxt).vstate_tab.add((*ctxt).vstate_nr as usize)).exec =
                xml_reg_new_exec_ctxt((*elem_decl).cont_model, None, null_mut());
        } else {
            (*(*ctxt).vstate_tab.add((*ctxt).vstate_nr as usize)).exec = null_mut();
            let node_name = (*node).name().unwrap();
            xml_err_valid_node(
                ctxt,
                elem_decl as XmlNodePtr,
                XmlParserErrors::XmlErrInternalError,
                format!("Failed to build content model regexp for {node_name}\n").as_str(),
                Some(&node_name),
                None,
                None,
            );
        }
    }

    let res = (*ctxt).vstate_nr;
    (*ctxt).vstate_nr += 1;
    res
}

#[cfg(not(feature = "libxml_regexp"))]
const MAX_RECURSE: usize = 25000;

#[cfg(not(feature = "libxml_regexp"))]
unsafe extern "C" fn vstate_vpush(
    ctxt: XmlValidCtxtPtr,
    cont: XmlElementContentPtr,
    node: XmlNodePtr,
    depth: c_uchar,
    occurs: c_long,
    state: c_uchar,
) -> i32 {
    let i: i32 = (*ctxt).vstateNr - 1;

    if (*ctxt).vstateNr > MAX_RECURSE as i32 {
        return -1;
    }
    if (*ctxt).vstateTab.is_null() {
        (*ctxt).vstateMax = 8;
        (*ctxt).vstateTab =
            xml_malloc((*ctxt).vstateMax as usize * size_of_val(&*(*ctxt).vstateTab.add(0)))
                as *mut XmlValidState;
        if (*ctxt).vstateTab.is_null() {
            xml_verr_memory(ctxt as _, c"malloc failed".as_ptr() as _);
            return -1;
        }
    }
    if (*ctxt).vstateNr >= (*ctxt).vstateMax {
        let tmp: *mut XmlValidState;

        tmp = xml_realloc(
            (*ctxt).vstateTab as _,
            2 * (*ctxt).vstateMax as usize * size_of_val(&*(*ctxt).vstateTab.add(0)),
        ) as *mut XmlValidState;
        if tmp.is_null() {
            xml_verr_memory(ctxt as _, c"malloc failed".as_ptr() as _);
            return -1;
        }
        (*ctxt).vstateMax *= 2;
        (*ctxt).vstateTab = tmp;
        (*ctxt).vstate = (*ctxt).vstateTab.add(0);
    }
    /*
     * Don't push on the stack a state already here
     */
    if i >= 0
        && (*(*ctxt).vstateTab.add(i as usize)).cont == cont
        && (*(*ctxt).vstateTab.add(i as usize)).node == node
        && (*(*ctxt).vstateTab.add(i as usize)).depth == depth
        && (*(*ctxt).vstateTab.add(i as usize)).occurs == occurs
        && (*(*ctxt).vstateTab.add(i as usize)).state == state
    {
        return (*ctxt).vstateNr;
    }
    (*(*ctxt).vstateTab.add((*ctxt).vstateNr as usize)).cont = cont;
    (*(*ctxt).vstateTab.add((*ctxt).vstateNr as usize)).node = node;
    (*(*ctxt).vstateTab.add((*ctxt).vstateNr as usize)).depth = depth;
    (*(*ctxt).vstateTab.add((*ctxt).vstateNr as usize)).occurs = occurs;
    (*(*ctxt).vstateTab.add((*ctxt).vstateNr as usize)).state = state;

    let res = (*ctxt).vstateNr;
    (*ctxt).vstateNr += 1;
    res
}

/// Push a new element start on the validation stack.
///
/// returns 1 if no validation problem was found or 0 otherwise
#[doc(alias = "xmlValidatePushElement")]
#[cfg(all(feature = "libxml_valid", feature = "libxml_regexp"))]
pub unsafe extern "C" fn xml_validate_push_element(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    elem: XmlNodePtr,
    qname: *const XmlChar,
) -> i32 {
    let mut ret: i32 = 1;
    let mut extsubset: i32 = 0;

    if ctxt.is_null() {
        return 0;
    }

    if (*ctxt).vstate_nr > 0 && !(*ctxt).vstate.is_null() {
        let state: XmlValidStatePtr = (*ctxt).vstate;
        let elem_decl: XmlElementPtr;

        // Check the new element against the content model of the new elem.
        if !(*state).elem_decl.is_null() {
            elem_decl = (*state).elem_decl;

            match (*elem_decl).etype {
                XmlElementTypeVal::XmlElementTypeUndefined => {
                    ret = 0;
                }
                XmlElementTypeVal::XmlElementTypeEmpty => {
                    let name = (*(*state).node).name().unwrap();
                    xml_err_valid_node(
                        ctxt,
                        (*state).node,
                        XmlParserErrors::XmlDTDNotEmpty,
                        format!("Element {name} was declared EMPTY this one has content\n")
                            .as_str(),
                        Some(&name),
                        None,
                        None,
                    );
                    ret = 0;
                }
                XmlElementTypeVal::XmlElementTypeAny => {
                    // I don't think anything is required then
                }
                XmlElementTypeVal::XmlElementTypeMixed => {
                    // simple case of declared as #PCDATA
                    if !(*elem_decl).content.is_null()
                        && (*(*elem_decl).content).typ
                            == XmlElementContentType::XmlElementContentPCDATA
                    {
                        let name = (*(*state).node).name().unwrap();
                        xml_err_valid_node(
                            ctxt,
                            (*state).node,
                            XmlParserErrors::XmlDTDNotPCDATA,
                            format!(
                                "Element {name} was declared #PCDATA but contains non text nodes\n"
                            )
                            .as_str(),
                            Some(&name),
                            None,
                            None,
                        );
                        ret = 0;
                    } else {
                        ret = xml_validate_check_mixed(ctxt, (*elem_decl).content, qname);
                        if ret != 1 {
                            let qname = CStr::from_ptr(qname as *const i8).to_string_lossy();
                            let name = (*(*state).node).name().unwrap();
                            xml_err_valid_node(
                                ctxt,
                                (*state).node,
                                XmlParserErrors::XmlDTDInvalidChild,
                                format!("Element {qname} is not declared in {name} list of possible children\n").as_str(),
                                Some(&qname),
                                Some(&name),
                                None,
                            );
                        }
                    }
                }
                XmlElementTypeVal::XmlElementTypeElement => {
                    // TODO:
                    // VC: Standalone Document Declaration
                    //     - element types with element content, if white space
                    //       occurs directly within any instance of those types.
                    if !(*state).exec.is_null() {
                        ret = xml_reg_exec_push_string((*state).exec, qname, null_mut());
                        if ret < 0 {
                            let name = (*(*state).node).name().unwrap();
                            let qname = CStr::from_ptr(qname as *const i8).to_string_lossy();
                            xml_err_valid_node(
                                ctxt,
                                (*state).node,
                                XmlParserErrors::XmlDTDContentModel,
                                format!("Element {name} content does not follow the DTD, Misplaced {qname}\n").as_str(),
                                Some(&name),
                                Some(&qname),
                                None,
                            );
                            ret = 0;
                        } else {
                            ret = 1;
                        }
                    }
                }
            }
        }
    }
    let e_decl: XmlElementPtr = xml_valid_get_elem_decl(ctxt, doc, elem, addr_of_mut!(extsubset));
    vstate_vpush(ctxt, e_decl, elem);
    ret
}

/// Check the CData parsed for validation in the current stack
///
/// Returns 1 if no validation problem was found or 0 otherwise
#[doc(alias = "xmlValidatePushCData")]
#[cfg(all(feature = "libxml_valid", feature = "libxml_regexp"))]
pub unsafe extern "C" fn xml_validate_push_cdata(
    ctxt: XmlValidCtxtPtr,
    data: *const XmlChar,
    len: i32,
) -> i32 {
    let mut ret: i32 = 1;

    /* printf("CDATA %s %d\n", data, len); */
    if ctxt.is_null() {
        return 0;
    }
    if len <= 0 {
        return ret;
    }
    if (*ctxt).vstate_nr > 0 && !(*ctxt).vstate.is_null() {
        let state: XmlValidStatePtr = (*ctxt).vstate;
        let elem_decl: XmlElementPtr;

        /*
         * Check the new element against the content model of the new elem.
         */
        if !(*state).elem_decl.is_null() {
            elem_decl = (*state).elem_decl;

            match (*elem_decl).etype {
                XmlElementTypeVal::XmlElementTypeUndefined => {
                    ret = 0;
                }
                XmlElementTypeVal::XmlElementTypeEmpty => {
                    let name = (*(*state).node).name().unwrap();
                    xml_err_valid_node(
                        ctxt,
                        (*state).node,
                        XmlParserErrors::XmlDTDNotEmpty,
                        format!("Element {name} was declared EMPTY this one has content\n")
                            .as_str(),
                        Some(&name),
                        None,
                        None,
                    );
                    ret = 0;
                }
                XmlElementTypeVal::XmlElementTypeAny => {}
                XmlElementTypeVal::XmlElementTypeMixed => {}
                XmlElementTypeVal::XmlElementTypeElement => {
                    for i in 0..len {
                        if !xml_is_blank_char(*data.add(i as usize) as u32) {
                            let name = (*(*state).node).name().unwrap();
                            xml_err_valid_node(
                                ctxt,
                                (*state).node,
                                XmlParserErrors::XmlDTDContentModel,
                                format!("Element {name} content does not follow the DTD, Text not allowed\n").as_str(),
                                Some(&name),
                                None,
                                None,
                            );
                            ret = 0;
                            // goto done;
                            return ret;
                        }
                    }
                    // TODO:
                    // VC: Standalone Document Declaration
                    //  element types with element content, if white space
                    //  occurs directly within any instance of those types.
                }
            }
        }
    }
    // done:
    ret
}

#[cfg(feature = "libxml_regexp")]
unsafe extern "C" fn vstate_vpop(ctxt: XmlValidCtxtPtr) -> i32 {
    if (*ctxt).vstate_nr < 1 {
        return -1;
    }
    (*ctxt).vstate_nr -= 1;
    let elem_decl: XmlElementPtr = (*(*ctxt).vstate_tab.add((*ctxt).vstate_nr as usize)).elem_decl;
    (*(*ctxt).vstate_tab.add((*ctxt).vstate_nr as usize)).elem_decl = null_mut();
    (*(*ctxt).vstate_tab.add((*ctxt).vstate_nr as usize)).node = null_mut();
    if !elem_decl.is_null()
        && matches!((*elem_decl).etype, XmlElementTypeVal::XmlElementTypeElement)
    {
        xml_reg_free_exec_ctxt((*(*ctxt).vstate_tab.add((*ctxt).vstate_nr as usize)).exec);
    }
    (*(*ctxt).vstate_tab.add((*ctxt).vstate_nr as usize)).exec = null_mut();
    if (*ctxt).vstate_nr >= 1 {
        (*ctxt).vstate = (*ctxt).vstate_tab.add((*ctxt).vstate_nr as usize - 1);
    } else {
        (*ctxt).vstate = null_mut();
    }
    (*ctxt).vstate_nr
}

#[cfg(not(feature = "libxml_regexp"))]
unsafe extern "C" fn vstateVPop(ctxt: XmlValidCtxtPtr) -> i32 {
    if (*ctxt).vstateNr <= 1 {
        return -1;
    }
    (*ctxt).vstateNr -= 1;
    (*ctxt).vstate = (*ctxt).vstateTab.add(0);
    (*(*ctxt).vstate).cont = (*(*ctxt).vstateTab.add((*ctxt).vstateNr as usize)).cont;
    (*(*ctxt).vstate).node = (*(*ctxt).vstateTab.add((*ctxt).vstateNr as usize)).node;
    (*(*ctxt).vstate).depth = (*(*ctxt).vstateTab.add((*ctxt).vstateNr as usize)).depth;
    (*(*ctxt).vstate).occurs = (*(*ctxt).vstateTab.add((*ctxt).vstateNr as usize)).occurs;
    (*(*ctxt).vstate).state = (*(*ctxt).vstateTab.add((*ctxt).vstateNr as usize)).state;
    return (*ctxt).vstateNr;
}

/// Pop the element end from the validation stack.
///
/// Returns 1 if no validation problem was found or 0 otherwise
#[doc(alias = "xmlValidatePopElement")]
#[cfg(all(feature = "libxml_valid", feature = "libxml_regexp"))]
pub unsafe extern "C" fn xml_validate_pop_element(
    ctxt: XmlValidCtxtPtr,
    _doc: XmlDocPtr,
    _elem: XmlNodePtr,
    _qname: *const XmlChar,
) -> i32 {
    let mut ret: i32 = 1;

    if ctxt.is_null() {
        return 0;
    }
    /* printf("PopElem %s\n", qname); */
    if (*ctxt).vstate_nr > 0 && !(*ctxt).vstate.is_null() {
        let state: XmlValidStatePtr = (*ctxt).vstate;
        let elem_decl: XmlElementPtr;

        /*
         * Check the new element against the content model of the new elem.
         */
        if !(*state).elem_decl.is_null() {
            elem_decl = (*state).elem_decl;

            if matches!((*elem_decl).etype, XmlElementTypeVal::XmlElementTypeElement)
                && !(*state).exec.is_null()
            {
                ret = xml_reg_exec_push_string((*state).exec, null_mut(), null_mut());
                if ret <= 0 {
                    let name = (*(*state).node).name().unwrap();
                    xml_err_valid_node(
                        ctxt,
                        (*state).node,
                        XmlParserErrors::XmlDTDContentModel,
                        format!(
                            "Element {name} content does not follow the DTD, Expecting more children\n"
                        )
                        .as_str(),
                        Some(&name),
                        None,
                        None,
                    );
                    ret = 0;
                } else {
                    // previous validation errors should not generate a new one here
                    ret = 1;
                }
            }
        }
        vstate_vpop(ctxt);
    }
    ret
}

/// Skip ignorable elements w.r.t. the validation process
///
/// Returns the first element to consider for validation of the content model
#[doc(alias = "xmlValidateSkipIgnorable")]
#[cfg(not(feature = "libxml_regexp"))]
unsafe extern "C" fn xmlValidateSkipIgnorable(mut child: XmlNodePtr) -> XmlNodePtr {
    while !child.is_null() {
        match (*child).typ {
            /* These things are ignored (skipped) during validation.  */
            XmlElementType::XmlPINode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => {
                child = (*child).next;
            }
            XmlElementType::XmlTextNode => {
                if xml_is_blank_node(child) != 0 {
                    child = (*child).next;
                } else {
                    return child;
                }
            }
            /* keep current node */
            _ => {
                return child;
            }
        }
    }
    return child;
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_add_element_decl() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_VALID_CTXT_PTR {
                for n_dtd in 0..GEN_NB_XML_DTD_PTR {
                    for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_type in 0..GEN_NB_XML_ELEMENT_TYPE_VAL {
                            for n_content in 0..GEN_NB_XML_ELEMENT_CONTENT_PTR {
                                let mem_base = xml_mem_blocks();
                                let ctxt = gen_xml_valid_ctxt_ptr(n_ctxt, 0);
                                let dtd = gen_xml_dtd_ptr(n_dtd, 1);
                                let name = gen_const_xml_char_ptr(n_name, 2);
                                let typ = gen_xml_element_type_val(n_type, 3);
                                let content = gen_xml_element_content_ptr(n_content, 4);

                                let ret_val = xml_add_element_decl(ctxt, dtd, name, typ, content);
                                desret_xml_element_ptr(ret_val);
                                des_xml_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                                des_xml_dtd_ptr(n_dtd, dtd, 1);
                                des_const_xml_char_ptr(n_name, name, 2);
                                des_xml_element_type_val(n_type, typ, 3);
                                des_xml_element_content_ptr(n_content, content, 4);
                                reset_last_error();
                                if mem_base != xml_mem_blocks() {
                                    leaks += 1;
                                    eprint!(
                                        "Leak of {} blocks found in xmlAddElementDecl",
                                        xml_mem_blocks() - mem_base
                                    );
                                    assert!(
                                        leaks == 0,
                                        "{leaks} Leaks are found in xmlAddElementDecl()"
                                    );
                                    eprint!(" {}", n_ctxt);
                                    eprint!(" {}", n_dtd);
                                    eprint!(" {}", n_name);
                                    eprint!(" {}", n_type);
                                    eprintln!(" {}", n_content);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_add_id() {

        /* missing type support */
    }

    #[test]
    fn test_xml_add_notation_decl() {

        /* missing type support */
    }

    #[test]
    fn test_xml_add_ref() {

        /* missing type support */
    }

    #[test]
    fn test_xml_copy_attribute_table() {

        /* missing type support */
    }

    #[test]
    fn test_xml_copy_doc_element_content() {
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_cur in 0..GEN_NB_XML_ELEMENT_CONTENT_PTR {
                    let mem_base = xml_mem_blocks();
                    let doc = gen_xml_doc_ptr(n_doc, 0);
                    let cur = gen_xml_element_content_ptr(n_cur, 1);

                    let ret_val = xml_copy_doc_element_content(doc, cur);
                    desret_xml_element_content_ptr(ret_val);
                    des_xml_doc_ptr(n_doc, doc, 0);
                    des_xml_element_content_ptr(n_cur, cur, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlCopyDocElementContent",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlCopyDocElementContent()"
                        );
                        eprint!(" {}", n_doc);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_copy_element_content() {
        unsafe {
            let mut leaks = 0;

            for n_cur in 0..GEN_NB_XML_ELEMENT_CONTENT_PTR {
                let mem_base = xml_mem_blocks();
                let cur = gen_xml_element_content_ptr(n_cur, 0);

                let ret_val = xml_copy_element_content(cur);
                desret_xml_element_content_ptr(ret_val);
                des_xml_element_content_ptr(n_cur, cur, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlCopyElementContent",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlCopyElementContent()"
                    );
                    eprintln!(" {}", n_cur);
                }
            }
        }
    }

    #[test]
    fn test_xml_copy_element_table() {

        /* missing type support */
    }

    #[test]
    fn test_xml_copy_enumeration() {

        /* missing type support */
    }

    #[test]
    fn test_xml_copy_notation_table() {

        /* missing type support */
    }

    #[test]
    fn test_xml_create_enumeration() {

        /* missing type support */
    }

    #[test]
    fn test_xml_dump_attribute_decl() {
        #[cfg(feature = "libxml_output")]
        unsafe {
            let mut leaks = 0;

            for n_buf in 0..GEN_NB_XML_BUFFER_PTR {
                for n_attr in 0..GEN_NB_XML_ATTRIBUTE_PTR {
                    let mem_base = xml_mem_blocks();
                    let buf = gen_const_xml_buf_ptr(n_buf, 0) as _;
                    let attr = gen_xml_attribute_ptr(n_attr, 1);

                    xml_dump_attribute_decl(buf, attr);
                    des_const_xml_buf_ptr(n_buf, buf as _, 0);
                    des_xml_attribute_ptr(n_attr, attr, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlDumpAttributeDecl",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlDumpAttributeDecl()"
                        );
                        eprint!(" {}", n_buf);
                        eprintln!(" {}", n_attr);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_dump_attribute_table() {
        #[cfg(feature = "libxml_output")]
        unsafe {
            let mut leaks = 0;

            for n_buf in 0..GEN_NB_XML_BUFFER_PTR {
                for n_table in 0..GEN_NB_XML_ATTRIBUTE_TABLE_PTR {
                    let mem_base = xml_mem_blocks();
                    let buf = gen_const_xml_buf_ptr(n_buf, 0) as _;
                    let table = gen_xml_attribute_table_ptr(n_table, 1);

                    xml_dump_attribute_table(buf, table);
                    des_const_xml_buf_ptr(n_buf, buf as _, 0);
                    des_xml_attribute_table_ptr(n_table, table, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlDumpAttributeTable",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlDumpAttributeTable()"
                        );
                        eprint!(" {}", n_buf);
                        eprintln!(" {}", n_table);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_dump_element_decl() {
        #[cfg(feature = "libxml_output")]
        unsafe {
            let mut leaks = 0;

            for n_buf in 0..GEN_NB_XML_BUFFER_PTR {
                for n_elem in 0..GEN_NB_XML_ELEMENT_PTR {
                    let mem_base = xml_mem_blocks();
                    let buf = gen_const_xml_buf_ptr(n_buf, 0) as _;
                    let elem = gen_xml_element_ptr(n_elem, 1);

                    xml_dump_element_decl(buf, elem);
                    des_const_xml_buf_ptr(n_buf, buf as _, 0);
                    des_xml_element_ptr(n_elem, elem, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlDumpElementDecl",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlDumpElementDecl()"
                        );
                        eprint!(" {}", n_buf);
                        eprintln!(" {}", n_elem);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_dump_element_table() {
        #[cfg(feature = "libxml_output")]
        unsafe {
            let mut leaks = 0;

            for n_buf in 0..GEN_NB_XML_BUFFER_PTR {
                for n_table in 0..GEN_NB_XML_ELEMENT_TABLE_PTR {
                    let mem_base = xml_mem_blocks();
                    let buf = gen_const_xml_buf_ptr(n_buf, 0) as _;
                    let table = gen_xml_element_table_ptr(n_table, 1);

                    xml_dump_element_table(buf, table);
                    des_const_xml_buf_ptr(n_buf, buf as _, 0);
                    des_xml_element_table_ptr(n_table, table, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlDumpElementTable",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlDumpElementTable()"
                        );
                        eprint!(" {}", n_buf);
                        eprintln!(" {}", n_table);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_dump_notation_decl() {
        #[cfg(feature = "libxml_output")]
        unsafe {
            let mut leaks = 0;

            for n_buf in 0..GEN_NB_XML_BUFFER_PTR {
                for n_nota in 0..GEN_NB_XML_NOTATION_PTR {
                    let mem_base = xml_mem_blocks();
                    let buf = gen_const_xml_buf_ptr(n_buf, 0) as _;
                    let nota = gen_xml_notation_ptr(n_nota, 1);

                    xml_dump_notation_decl(buf, nota);
                    des_const_xml_buf_ptr(n_buf, buf as _, 0);
                    des_xml_notation_ptr(n_nota, nota, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlDumpNotationDecl",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlDumpNotationDecl()"
                        );
                        eprint!(" {}", n_buf);
                        eprintln!(" {}", n_nota);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_dump_notation_table() {
        #[cfg(feature = "libxml_output")]
        unsafe {
            let mut leaks = 0;

            for n_buf in 0..GEN_NB_XML_BUFFER_PTR {
                for n_table in 0..GEN_NB_XML_NOTATION_TABLE_PTR {
                    let mem_base = xml_mem_blocks();
                    let buf = gen_const_xml_buf_ptr(n_buf, 0) as _;
                    let table = gen_xml_notation_table_ptr(n_table, 1);

                    xml_dump_notation_table(buf, table);
                    des_const_xml_buf_ptr(n_buf, buf as _, 0);
                    des_xml_notation_table_ptr(n_table, table, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlDumpNotationTable",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlDumpNotationTable()"
                        );
                        eprint!(" {}", n_buf);
                        eprintln!(" {}", n_table);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_get_dtd_element_desc() {
        unsafe {
            let mut leaks = 0;

            for n_dtd in 0..GEN_NB_XML_DTD_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let dtd = gen_xml_dtd_ptr(n_dtd, 0);
                    let name = gen_const_xml_char_ptr(n_name, 1);

                    let ret_val = xml_get_dtd_element_desc(dtd, name);
                    desret_xml_element_ptr(ret_val);
                    des_xml_dtd_ptr(n_dtd, dtd, 0);
                    des_const_xml_char_ptr(n_name, name, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlGetDtdElementDesc",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlGetDtdElementDesc()"
                        );
                        eprint!(" {}", n_dtd);
                        eprintln!(" {}", n_name);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_get_dtd_qelement_desc() {
        unsafe {
            let mut leaks = 0;

            for n_dtd in 0..GEN_NB_XML_DTD_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_prefix in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let dtd = gen_xml_dtd_ptr(n_dtd, 0);
                        let name = gen_const_xml_char_ptr(n_name, 1);
                        let prefix = gen_const_xml_char_ptr(n_prefix, 2);

                        let ret_val = xml_get_dtd_qelement_desc(dtd, name, prefix);
                        desret_xml_element_ptr(ret_val);
                        des_xml_dtd_ptr(n_dtd, dtd, 0);
                        des_const_xml_char_ptr(n_name, name, 1);
                        des_const_xml_char_ptr(n_prefix, prefix, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlGetDtdQElementDesc",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlGetDtdQElementDesc()"
                            );
                            eprint!(" {}", n_dtd);
                            eprint!(" {}", n_name);
                            eprintln!(" {}", n_prefix);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_get_id() {
        unsafe {
            let mut leaks = 0;
            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let doc = gen_xml_doc_ptr(n_doc, 0);
                    let id = gen_const_xml_char_ptr(n_id, 1);

                    let ret_val = xml_get_id(doc, id);
                    desret_xml_attr_ptr(ret_val.map_or(null_mut(), |p| p.as_ptr() as XmlAttrPtr));
                    des_xml_doc_ptr(n_doc, doc, 0);
                    des_const_xml_char_ptr(n_id, id, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlGetID",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlGetID()");
                        eprint!(" {}", n_doc);
                        eprintln!(" {}", n_id);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_get_refs() {

        /* missing type support */
    }

    #[test]
    fn test_xml_is_id() {
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_elem in 0..GEN_NB_XML_NODE_PTR {
                    for n_attr in 0..GEN_NB_XML_ATTR_PTR {
                        let mem_base = xml_mem_blocks();
                        let doc = gen_xml_doc_ptr(n_doc, 0);
                        let elem = gen_xml_node_ptr(n_elem, 1);
                        let attr = gen_xml_attr_ptr(n_attr, 2);

                        let ret_val = xml_is_id(doc, elem, attr);
                        desret_int(ret_val);
                        des_xml_doc_ptr(n_doc, doc, 0);
                        des_xml_node_ptr(n_elem, elem, 1);
                        des_xml_attr_ptr(n_attr, attr, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlIsID",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlIsID()");
                            eprint!(" {}", n_doc);
                            eprint!(" {}", n_elem);
                            eprintln!(" {}", n_attr);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_is_mixed_element() {
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let doc = gen_xml_doc_ptr(n_doc, 0);
                    let name = gen_const_xml_char_ptr(n_name, 1);

                    let ret_val = xml_is_mixed_element(doc, name);
                    desret_int(ret_val);
                    des_xml_doc_ptr(n_doc, doc, 0);
                    des_const_xml_char_ptr(n_name, name, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlIsMixedElement",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlIsMixedElement()");
                        eprint!(" {}", n_doc);
                        eprintln!(" {}", n_name);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_is_ref() {
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_elem in 0..GEN_NB_XML_NODE_PTR {
                    for n_attr in 0..GEN_NB_XML_ATTR_PTR {
                        let mem_base = xml_mem_blocks();
                        let doc = gen_xml_doc_ptr(n_doc, 0);
                        let elem = gen_xml_node_ptr(n_elem, 1);
                        let attr = gen_xml_attr_ptr(n_attr, 2);

                        let ret_val = xml_is_ref(doc, elem, attr);
                        desret_int(ret_val);
                        des_xml_doc_ptr(n_doc, doc, 0);
                        des_xml_node_ptr(n_elem, elem, 1);
                        des_xml_attr_ptr(n_attr, attr, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlIsRef",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlIsRef()");
                            eprint!(" {}", n_doc);
                            eprint!(" {}", n_elem);
                            eprintln!(" {}", n_attr);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_new_doc_element_content() {
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_type in 0..GEN_NB_XML_ELEMENT_CONTENT_TYPE {
                        let mem_base = xml_mem_blocks();
                        let doc = gen_xml_doc_ptr(n_doc, 0);
                        let name = gen_const_xml_char_ptr(n_name, 1);
                        let typ = gen_xml_element_content_type(n_type, 2);

                        let ret_val = xml_new_doc_element_content(doc, name, typ);
                        xml_free_doc_element_content(doc, ret_val);
                        let ret_val = null_mut();
                        desret_xml_element_content_ptr(ret_val);
                        des_xml_doc_ptr(n_doc, doc, 0);
                        des_const_xml_char_ptr(n_name, name, 1);
                        des_xml_element_content_type(n_type, typ, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlNewDocElementContent",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlNewDocElementContent()"
                            );
                            eprint!(" {}", n_doc);
                            eprint!(" {}", n_name);
                            eprintln!(" {}", n_type);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_new_element_content() {
        unsafe {
            let mut leaks = 0;

            for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_type in 0..GEN_NB_XML_ELEMENT_CONTENT_TYPE {
                    let mem_base = xml_mem_blocks();
                    let name = gen_const_xml_char_ptr(n_name, 0);
                    let typ = gen_xml_element_content_type(n_type, 1);

                    let ret_val = xml_new_element_content(name as *const XmlChar, typ);
                    desret_xml_element_content_ptr(ret_val);
                    des_const_xml_char_ptr(n_name, name, 0);
                    des_xml_element_content_type(n_type, typ, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlNewElementContent",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlNewElementContent()"
                        );
                        eprint!(" {}", n_name);
                        eprintln!(" {}", n_type);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_new_valid_ctxt() {

        /* missing type support */
    }

    #[test]
    fn test_xml_remove_id() {
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_attr in 0..GEN_NB_XML_ATTR_PTR {
                    let mem_base = xml_mem_blocks();
                    let doc = gen_xml_doc_ptr(n_doc, 0);
                    let attr = gen_xml_attr_ptr(n_attr, 1);

                    let ret_val = xml_remove_id(doc, attr);
                    desret_int(ret_val);
                    des_xml_doc_ptr(n_doc, doc, 0);
                    des_xml_attr_ptr(n_attr, attr, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlRemoveID",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlRemoveID()");
                        eprint!(" {}", n_doc);
                        eprintln!(" {}", n_attr);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_remove_ref() {
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_attr in 0..GEN_NB_XML_ATTR_PTR {
                    let mem_base = xml_mem_blocks();
                    let doc = gen_xml_doc_ptr(n_doc, 0);
                    let attr = gen_xml_attr_ptr(n_attr, 1);

                    let ret_val = xml_remove_ref(doc, attr);
                    desret_int(ret_val);
                    des_xml_doc_ptr(n_doc, doc, 0);
                    des_xml_attr_ptr(n_attr, attr, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlRemoveRef",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlRemoveRef()");
                        eprint!(" {}", n_doc);
                        eprintln!(" {}", n_attr);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_snprintf_element_content() {
        unsafe {
            let mut leaks = 0;

            for n_buf in 0..GEN_NB_CHAR_PTR {
                for n_size in 0..GEN_NB_INT {
                    for n_content in 0..GEN_NB_XML_ELEMENT_CONTENT_PTR {
                        for n_englob in 0..GEN_NB_INT {
                            let mem_base = xml_mem_blocks();
                            let buf = gen_char_ptr(n_buf, 0);
                            let size = gen_int(n_size, 1);
                            let content = gen_xml_element_content_ptr(n_content, 2);
                            let englob = gen_int(n_englob, 3);

                            xml_snprintf_element_content(buf, size, content, englob);
                            des_char_ptr(n_buf, buf, 0);
                            des_int(n_size, size, 1);
                            des_xml_element_content_ptr(n_content, content, 2);
                            des_int(n_englob, englob, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlSnprintfElementContent",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlSnprintfElementContent()"
                                );
                                eprint!(" {}", n_buf);
                                eprint!(" {}", n_size);
                                eprint!(" {}", n_content);
                                eprintln!(" {}", n_englob);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_sprintf_element_content() {
        #[cfg(feature = "libxml_output")]
        unsafe {
            let mut leaks = 0;

            for n_buf in 0..GEN_NB_CHAR_PTR {
                for n_content in 0..GEN_NB_XML_ELEMENT_CONTENT_PTR {
                    for n_englob in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let buf = gen_char_ptr(n_buf, 0);
                        let content = gen_xml_element_content_ptr(n_content, 1);
                        let englob = gen_int(n_englob, 2);

                        xml_sprintf_element_content(buf, content, englob);
                        des_char_ptr(n_buf, buf, 0);
                        des_xml_element_content_ptr(n_content, content, 1);
                        des_int(n_englob, englob, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlSprintfElementContent",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlSprintfElementContent()"
                            );
                            eprint!(" {}", n_buf);
                            eprint!(" {}", n_content);
                            eprintln!(" {}", n_englob);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_valid_build_content_model() {
        #[cfg(all(feature = "libxml_valid", feature = "libxml_regexp"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_VALID_CTXT_PTR {
                for n_elem in 0..GEN_NB_XML_ELEMENT_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_valid_ctxt_ptr(n_ctxt, 0);
                    let elem = gen_xml_element_ptr(n_elem, 1);

                    let ret_val = xml_valid_build_content_model(ctxt, elem);
                    desret_int(ret_val);
                    des_xml_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_xml_element_ptr(n_elem, elem, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlValidBuildContentModel",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlValidBuildContentModel()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_elem);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_valid_get_potential_children() {
        #[cfg(feature = "libxml_valid")]
        unsafe {
            let mut leaks = 0;

            for n_ctree in 0..GEN_NB_XML_ELEMENT_CONTENT_PTR {
                for n_names in 0..GEN_NB_CONST_XML_CHAR_PTR_PTR {
                    for n_len in 0..GEN_NB_INT_PTR {
                        for n_max in 0..GEN_NB_INT {
                            let mem_base = xml_mem_blocks();
                            let ctree = gen_xml_element_content_ptr(n_ctree, 0);
                            let names = gen_const_xml_char_ptr_ptr(n_names, 1);
                            let len = gen_int_ptr(n_len, 2);
                            let max = gen_int(n_max, 3);

                            let ret_val = xml_valid_get_potential_children(
                                ctree,
                                names as *mut *const XmlChar,
                                len,
                                max,
                            );
                            desret_int(ret_val);
                            des_xml_element_content_ptr(n_ctree, ctree, 0);
                            des_const_xml_char_ptr_ptr(n_names, names as *mut *const XmlChar, 1);
                            des_int_ptr(n_len, len, 2);
                            des_int(n_max, max, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlValidGetPotentialChildren",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlValidGetPotentialChildren()"
                                );
                                eprint!(" {}", n_ctree);
                                eprint!(" {}", n_names);
                                eprint!(" {}", n_len);
                                eprintln!(" {}", n_max);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_valid_get_valid_elements() {
        #[cfg(feature = "libxml_valid")]
        unsafe {
            let mut leaks = 0;

            for n_prev in 0..GEN_NB_XML_NODE_PTR {
                for n_next in 0..GEN_NB_XML_NODE_PTR {
                    for n_names in 0..GEN_NB_CONST_XML_CHAR_PTR_PTR {
                        for n_max in 0..GEN_NB_INT {
                            let mem_base = xml_mem_blocks();
                            let prev = gen_xml_node_ptr(n_prev, 0);
                            let next = gen_xml_node_ptr(n_next, 1);
                            let names = gen_const_xml_char_ptr_ptr(n_names, 2);
                            let max = gen_int(n_max, 3);

                            let ret_val = xml_valid_get_valid_elements(
                                prev,
                                next,
                                names as *mut *const XmlChar,
                                max,
                            );
                            desret_int(ret_val);
                            des_xml_node_ptr(n_prev, prev, 0);
                            des_xml_node_ptr(n_next, next, 1);
                            des_const_xml_char_ptr_ptr(n_names, names as *mut *const XmlChar, 2);
                            des_int(n_max, max, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlValidGetValidElements",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlValidGetValidElements()"
                                );
                                eprint!(" {}", n_prev);
                                eprint!(" {}", n_next);
                                eprint!(" {}", n_names);
                                eprintln!(" {}", n_max);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_validate_attribute_decl() {
        #[cfg(feature = "libxml_valid")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_VALID_CTXT_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_attr in 0..GEN_NB_XML_ATTRIBUTE_PTR {
                        let mem_base = xml_mem_blocks();
                        let ctxt = gen_xml_valid_ctxt_ptr(n_ctxt, 0);
                        let doc = gen_xml_doc_ptr(n_doc, 1);
                        let attr = gen_xml_attribute_ptr(n_attr, 2);

                        let ret_val = xml_validate_attribute_decl(ctxt, doc, attr);
                        desret_int(ret_val);
                        des_xml_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                        des_xml_doc_ptr(n_doc, doc, 1);
                        des_xml_attribute_ptr(n_attr, attr, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlValidateAttributeDecl",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlValidateAttributeDecl()"
                            );
                            eprint!(" {}", n_ctxt);
                            eprint!(" {}", n_doc);
                            eprintln!(" {}", n_attr);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_validate_attribute_value() {
        #[cfg(feature = "libxml_valid")]
        unsafe {
            let mut leaks = 0;

            for n_type in 0..GEN_NB_XML_ATTRIBUTE_TYPE {
                for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let typ = gen_xml_attribute_type(n_type, 0);
                    let value = gen_const_xml_char_ptr(n_value, 1);

                    let ret_val = xml_validate_attribute_value(typ, value);
                    desret_int(ret_val);
                    des_xml_attribute_type(n_type, typ, 0);
                    des_const_xml_char_ptr(n_value, value, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlValidateAttributeValue",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlValidateAttributeValue()"
                        );
                        eprint!(" {}", n_type);
                        eprintln!(" {}", n_value);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_validate_document() {
        #[cfg(feature = "libxml_valid")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_VALID_CTXT_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_valid_ctxt_ptr(n_ctxt, 0);
                    let doc = gen_xml_doc_ptr(n_doc, 1);

                    let ret_val = xml_validate_document(ctxt, doc);
                    desret_int(ret_val);
                    des_xml_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_xml_doc_ptr(n_doc, doc, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlValidateDocument",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlValidateDocument()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_doc);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_validate_document_final() {
        #[cfg(feature = "libxml_valid")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_VALID_CTXT_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_valid_ctxt_ptr(n_ctxt, 0);
                    let doc = gen_xml_doc_ptr(n_doc, 1);

                    let ret_val = xml_validate_document_final(ctxt, doc);
                    desret_int(ret_val);
                    des_xml_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_xml_doc_ptr(n_doc, doc, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlValidateDocumentFinal",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlValidateDocumentFinal()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_doc);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_validate_dtd() {
        #[cfg(feature = "libxml_valid")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_VALID_CTXT_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_dtd in 0..GEN_NB_XML_DTD_PTR {
                        let mem_base = xml_mem_blocks();
                        let ctxt = gen_xml_valid_ctxt_ptr(n_ctxt, 0);
                        let doc = gen_xml_doc_ptr(n_doc, 1);
                        let dtd = gen_xml_dtd_ptr(n_dtd, 2);

                        let ret_val = xml_validate_dtd(ctxt, doc, dtd);
                        desret_int(ret_val);
                        des_xml_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                        des_xml_doc_ptr(n_doc, doc, 1);
                        des_xml_dtd_ptr(n_dtd, dtd, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlValidateDtd",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlValidateDtd()");
                            eprint!(" {}", n_ctxt);
                            eprint!(" {}", n_doc);
                            eprintln!(" {}", n_dtd);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_validate_dtd_final() {
        #[cfg(feature = "libxml_valid")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_VALID_CTXT_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_valid_ctxt_ptr(n_ctxt, 0);
                    let doc = gen_xml_doc_ptr(n_doc, 1);

                    let ret_val = xml_validate_dtd_final(ctxt, doc);
                    desret_int(ret_val);
                    des_xml_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_xml_doc_ptr(n_doc, doc, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlValidateDtdFinal",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlValidateDtdFinal()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_doc);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_validate_element() {
        #[cfg(feature = "libxml_valid")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_VALID_CTXT_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_root in 0..GEN_NB_XML_NODE_PTR {
                        let mem_base = xml_mem_blocks();
                        let ctxt = gen_xml_valid_ctxt_ptr(n_ctxt, 0);
                        let doc = gen_xml_doc_ptr(n_doc, 1);
                        let root = gen_xml_node_ptr(n_root, 2);

                        let ret_val = xml_validate_element(ctxt, doc, root);
                        desret_int(ret_val);
                        des_xml_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                        des_xml_doc_ptr(n_doc, doc, 1);
                        des_xml_node_ptr(n_root, root, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlValidateElement",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlValidateElement()"
                            );
                            eprint!(" {}", n_ctxt);
                            eprint!(" {}", n_doc);
                            eprintln!(" {}", n_root);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_validate_element_decl() {
        #[cfg(feature = "libxml_valid")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_VALID_CTXT_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_elem in 0..GEN_NB_XML_ELEMENT_PTR {
                        let mem_base = xml_mem_blocks();
                        let ctxt = gen_xml_valid_ctxt_ptr(n_ctxt, 0);
                        let doc = gen_xml_doc_ptr(n_doc, 1);
                        let elem = gen_xml_element_ptr(n_elem, 2);

                        let ret_val = xml_validate_element_decl(ctxt, doc, elem);
                        desret_int(ret_val);
                        des_xml_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                        des_xml_doc_ptr(n_doc, doc, 1);
                        des_xml_element_ptr(n_elem, elem, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlValidateElementDecl",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlValidateElementDecl()"
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
    fn test_xml_validate_name_value() {
        #[cfg(feature = "libxml_valid")]
        unsafe {
            let mut leaks = 0;

            for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let value = gen_const_xml_char_ptr(n_value, 0);

                let ret_val = xml_validate_name_value(value as *const XmlChar);
                desret_int(ret_val);
                des_const_xml_char_ptr(n_value, value, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlValidateNameValue",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlValidateNameValue()"
                    );
                    eprintln!(" {}", n_value);
                }
            }
        }
    }

    #[test]
    fn test_xml_validate_names_value() {
        #[cfg(feature = "libxml_valid")]
        unsafe {
            let mut leaks = 0;

            for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let value = gen_const_xml_char_ptr(n_value, 0);

                let ret_val = xml_validate_names_value(value as *const XmlChar);
                desret_int(ret_val);
                des_const_xml_char_ptr(n_value, value, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlValidateNamesValue",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlValidateNamesValue()"
                    );
                    eprintln!(" {}", n_value);
                }
            }
        }
    }

    #[test]
    fn test_xml_validate_nmtoken_value() {
        #[cfg(feature = "libxml_valid")]
        unsafe {
            let mut leaks = 0;

            for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let value = gen_const_xml_char_ptr(n_value, 0);

                let ret_val = xml_validate_nmtoken_value(value as *const XmlChar);
                desret_int(ret_val);
                des_const_xml_char_ptr(n_value, value, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlValidateNmtokenValue",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlValidateNmtokenValue()"
                    );
                    eprintln!(" {}", n_value);
                }
            }
        }
    }

    #[test]
    fn test_xml_validate_nmtokens_value() {
        #[cfg(feature = "libxml_valid")]
        unsafe {
            let mut leaks = 0;

            for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let value = gen_const_xml_char_ptr(n_value, 0);

                let ret_val = xml_validate_nmtokens_value(value as *const XmlChar);
                desret_int(ret_val);
                des_const_xml_char_ptr(n_value, value, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlValidateNmtokensValue",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlValidateNmtokensValue()"
                    );
                    eprintln!(" {}", n_value);
                }
            }
        }
    }

    #[test]
    fn test_xml_validate_notation_decl() {
        #[cfg(feature = "libxml_valid")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_VALID_CTXT_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_nota in 0..GEN_NB_XML_NOTATION_PTR {
                        let mem_base = xml_mem_blocks();
                        let ctxt = gen_xml_valid_ctxt_ptr(n_ctxt, 0);
                        let doc = gen_xml_doc_ptr(n_doc, 1);
                        let nota = gen_xml_notation_ptr(n_nota, 2);

                        let ret_val = xml_validate_notation_decl(ctxt, doc, nota);
                        desret_int(ret_val);
                        des_xml_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                        des_xml_doc_ptr(n_doc, doc, 1);
                        des_xml_notation_ptr(n_nota, nota, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlValidateNotationDecl",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlValidateNotationDecl()"
                            );
                            eprint!(" {}", n_ctxt);
                            eprint!(" {}", n_doc);
                            eprintln!(" {}", n_nota);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_validate_notation_use() {
        #[cfg(any(feature = "libxml_valid", feature = "schema"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_VALID_CTXT_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_notation_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let ctxt = gen_xml_valid_ctxt_ptr(n_ctxt, 0);
                        let doc = gen_xml_doc_ptr(n_doc, 1);
                        let notation_name = gen_const_xml_char_ptr(n_notation_name, 2);

                        let ret_val = xml_validate_notation_use(ctxt, doc, notation_name);
                        desret_int(ret_val);
                        des_xml_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                        des_xml_doc_ptr(n_doc, doc, 1);
                        des_const_xml_char_ptr(n_notation_name, notation_name, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlValidateNotationUse",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlValidateNotationUse()"
                            );
                            eprint!(" {}", n_ctxt);
                            eprint!(" {}", n_doc);
                            eprintln!(" {}", n_notation_name);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_validate_one_attribute() {
        #[cfg(feature = "libxml_valid")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_VALID_CTXT_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_elem in 0..GEN_NB_XML_NODE_PTR {
                        for n_attr in 0..GEN_NB_XML_ATTR_PTR {
                            for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                let mem_base = xml_mem_blocks();
                                let ctxt = gen_xml_valid_ctxt_ptr(n_ctxt, 0);
                                let doc = gen_xml_doc_ptr(n_doc, 1);
                                let elem = gen_xml_node_ptr(n_elem, 2);
                                let attr = gen_xml_attr_ptr(n_attr, 3);
                                let value = gen_const_xml_char_ptr(n_value, 4);

                                let ret_val =
                                    xml_validate_one_attribute(ctxt, doc, elem, attr, value);
                                desret_int(ret_val);
                                des_xml_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                                des_xml_doc_ptr(n_doc, doc, 1);
                                des_xml_node_ptr(n_elem, elem, 2);
                                des_xml_attr_ptr(n_attr, attr, 3);
                                des_const_xml_char_ptr(n_value, value, 4);
                                reset_last_error();
                                if mem_base != xml_mem_blocks() {
                                    leaks += 1;
                                    eprint!(
                                        "Leak of {} blocks found in xmlValidateOneAttribute",
                                        xml_mem_blocks() - mem_base
                                    );
                                    assert!(
                                        leaks == 0,
                                        "{leaks} Leaks are found in xmlValidateOneAttribute()"
                                    );
                                    eprint!(" {}", n_ctxt);
                                    eprint!(" {}", n_doc);
                                    eprint!(" {}", n_elem);
                                    eprint!(" {}", n_attr);
                                    eprintln!(" {}", n_value);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_validate_one_element() {
        #[cfg(feature = "libxml_valid")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_VALID_CTXT_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_elem in 0..GEN_NB_XML_NODE_PTR {
                        let mem_base = xml_mem_blocks();
                        let ctxt = gen_xml_valid_ctxt_ptr(n_ctxt, 0);
                        let doc = gen_xml_doc_ptr(n_doc, 1);
                        let elem = gen_xml_node_ptr(n_elem, 2);

                        let ret_val = xml_validate_one_element(ctxt, doc, elem);
                        desret_int(ret_val);
                        des_xml_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                        des_xml_doc_ptr(n_doc, doc, 1);
                        des_xml_node_ptr(n_elem, elem, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlValidateOneElement",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlValidateOneElement()"
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
    fn test_xml_validate_one_namespace() {
        #[cfg(feature = "libxml_valid")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_VALID_CTXT_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_elem in 0..GEN_NB_XML_NODE_PTR {
                        for n_prefix in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            for n_ns in 0..GEN_NB_XML_NS_PTR {
                                for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                    let mem_base = xml_mem_blocks();
                                    let ctxt = gen_xml_valid_ctxt_ptr(n_ctxt, 0);
                                    let doc = gen_xml_doc_ptr(n_doc, 1);
                                    let elem = gen_xml_node_ptr(n_elem, 2);
                                    let prefix = gen_const_xml_char_ptr(n_prefix, 3);
                                    let ns = gen_xml_ns_ptr(n_ns, 4);
                                    let value = gen_const_xml_char_ptr(n_value, 5);

                                    let ret_val = xml_validate_one_namespace(
                                        ctxt, doc, elem, prefix, ns, value,
                                    );
                                    desret_int(ret_val);
                                    des_xml_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                                    des_xml_doc_ptr(n_doc, doc, 1);
                                    des_xml_node_ptr(n_elem, elem, 2);
                                    des_const_xml_char_ptr(n_prefix, prefix, 3);
                                    des_xml_ns_ptr(n_ns, ns, 4);
                                    des_const_xml_char_ptr(n_value, value, 5);
                                    reset_last_error();
                                    if mem_base != xml_mem_blocks() {
                                        leaks += 1;
                                        eprint!(
                                            "Leak of {} blocks found in xmlValidateOneNamespace",
                                            xml_mem_blocks() - mem_base
                                        );
                                        assert!(
                                            leaks == 0,
                                            "{leaks} Leaks are found in xmlValidateOneNamespace()"
                                        );
                                        eprint!(" {}", n_ctxt);
                                        eprint!(" {}", n_doc);
                                        eprint!(" {}", n_elem);
                                        eprint!(" {}", n_prefix);
                                        eprint!(" {}", n_ns);
                                        eprintln!(" {}", n_value);
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
    fn test_xml_validate_pop_element() {
        #[cfg(all(feature = "libxml_valid", feature = "libxml_regexp"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_VALID_CTXT_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_elem in 0..GEN_NB_XML_NODE_PTR {
                        for n_qname in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_valid_ctxt_ptr(n_ctxt, 0);
                            let doc = gen_xml_doc_ptr(n_doc, 1);
                            let elem = gen_xml_node_ptr(n_elem, 2);
                            let qname = gen_const_xml_char_ptr(n_qname, 3);

                            let ret_val = xml_validate_pop_element(ctxt, doc, elem, qname);
                            desret_int(ret_val);
                            des_xml_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                            des_xml_doc_ptr(n_doc, doc, 1);
                            des_xml_node_ptr(n_elem, elem, 2);
                            des_const_xml_char_ptr(n_qname, qname, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlValidatePopElement",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlValidatePopElement()"
                                );
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_doc);
                                eprint!(" {}", n_elem);
                                eprintln!(" {}", n_qname);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_validate_push_cdata() {
        #[cfg(all(feature = "libxml_valid", feature = "libxml_regexp"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_VALID_CTXT_PTR {
                for n_data in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_len in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let ctxt = gen_xml_valid_ctxt_ptr(n_ctxt, 0);
                        let data = gen_const_xml_char_ptr(n_data, 1);
                        let mut len = gen_int(n_len, 2);
                        if !data.is_null() && len > xml_strlen(data) {
                            len = 0;
                        }

                        let ret_val = xml_validate_push_cdata(ctxt, data, len);
                        desret_int(ret_val);
                        des_xml_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                        des_const_xml_char_ptr(n_data, data, 1);
                        des_int(n_len, len, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlValidatePushCData",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlValidatePushCData()"
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
    fn test_xml_validate_push_element() {
        #[cfg(all(feature = "libxml_valid", feature = "libxml_regexp"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_VALID_CTXT_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_elem in 0..GEN_NB_XML_NODE_PTR {
                        for n_qname in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_valid_ctxt_ptr(n_ctxt, 0);
                            let doc = gen_xml_doc_ptr(n_doc, 1);
                            let elem = gen_xml_node_ptr(n_elem, 2);
                            let qname = gen_const_xml_char_ptr(n_qname, 3);

                            let ret_val = xml_validate_push_element(ctxt, doc, elem, qname);
                            desret_int(ret_val);
                            des_xml_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                            des_xml_doc_ptr(n_doc, doc, 1);
                            des_xml_node_ptr(n_elem, elem, 2);
                            des_const_xml_char_ptr(n_qname, qname, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlValidatePushElement",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlValidatePushElement()"
                                );
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_doc);
                                eprint!(" {}", n_elem);
                                eprintln!(" {}", n_qname);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_validate_root() {
        #[cfg(feature = "libxml_valid")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_VALID_CTXT_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_valid_ctxt_ptr(n_ctxt, 0);
                    let doc = gen_xml_doc_ptr(n_doc, 1);

                    let ret_val = xml_validate_root(ctxt, doc);
                    desret_int(ret_val);
                    des_xml_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_xml_doc_ptr(n_doc, doc, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlValidateRoot",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlValidateRoot()");
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_doc);
                    }
                }
            }
        }
    }
}
