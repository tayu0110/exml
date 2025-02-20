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

#[cfg(feature = "libxml_output")]
use std::io::Write;
use std::{
    collections::HashMap,
    ffi::{c_char, CStr, CString},
    mem::size_of,
    os::raw::c_void,
    ptr::{addr_of_mut, null, null_mut, NonNull},
    rc::Rc,
    sync::atomic::Ordering,
};

use libc::{memset, strcat, strlen, strncat};

#[cfg(feature = "libxml_regexp")]
use crate::libxml::xmlautomata::{
    xml_automata_compile, xml_automata_get_init_state, xml_automata_set_final_state,
    xml_free_automata, xml_new_automata, XmlAutomataPtr,
};
#[cfg(feature = "libxml_regexp")]
use crate::libxml::xmlregexp::{
    xml_reg_exec_push_string, xml_reg_free_exec_ctxt, xml_reg_new_exec_ctxt,
    xml_regexp_is_determinist, XmlRegExecCtxtPtr,
};
#[cfg(feature = "libxml_regexp")]
use crate::libxml::xmlstring::xml_strncmp;
#[cfg(not(feature = "libxml_regexp"))]
use crate::tree::{xml_free_node_list, XmlNodePtr};
#[cfg(feature = "libxml_valid")]
use crate::tree::{XmlElementPtr, XmlGenericNodePtr, XmlNsPtr};
use crate::{
    error::{XmlParserErrors, __xml_raise_error},
    globals::{GenericError, GenericErrorContext, StructuredError},
    hash::XmlHashTableRef,
    libxml::{
        globals::{xml_free, xml_malloc},
        hash::XmlHashTable,
        parser::XmlParserMode,
        parser_internals::xml_string_current_char,
        xmlautomata::{
            xml_automata_new_epsilon, xml_automata_new_state, xml_automata_new_transition,
            XmlAutomataStatePtr,
        },
        xmlstring::{xml_str_equal, xml_strdup, xml_strlen, xml_strndup, XmlChar},
    },
    list::XmlList,
    parser::{split_qname2, XmlParserCtxtPtr},
    tree::{
        xml_build_qname, xml_free_attribute, xml_free_element, xml_free_node, xml_get_doc_entity,
        xml_new_doc_node, xml_split_qname2, xml_split_qname3, NodeCommon, NodePtr, XmlAttrPtr,
        XmlAttribute, XmlAttributeDefault, XmlAttributePtr, XmlAttributeType, XmlDoc,
        XmlDocProperties, XmlDocPtr, XmlDtd, XmlDtdPtr, XmlElement, XmlElementContent,
        XmlElementContentOccur, XmlElementContentPtr, XmlElementContentType, XmlElementType,
        XmlElementTypeVal, XmlEntityPtr, XmlEntityType, XmlEnumeration, XmlID, XmlNode, XmlNodePtr,
        XmlNotation, XmlRef,
    },
};

use super::{chvalid::xml_is_blank_char, parser_internals::XML_VCTXT_USE_PCTXT};

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
    elem_decl: Option<XmlElementPtr>, /* pointer to the content model */
    node: *mut XmlNode,               /* pointer to the current node */
    exec: XmlRegExecCtxtPtr,          /* regexp runtime */
}
#[cfg(not(feature = "libxml_regexp"))]
#[repr(C)]
pub struct XmlValidState {
    cont: XmlElementContentPtr, /* pointer to the content model subtree */
    node: *mut XmlNode,         /* pointer to the current node in the list */
    occurs: i64,                /* bitfield for multiple occurrences */
    depth: u8,                  /* current depth in the overall tree */
    state: u8,                  /* ROLLBACK_XXX */
}

/// Callback called when a validity error is found. This is a message
/// oriented function similar to an *printf function.
#[doc(alias = "xmlValidityErrorFunc")]
pub type XmlValidityErrorFunc = unsafe fn(ctx: *mut c_void, msg: *const i8);

/// Callback called when a validity warning is found. This is a message
/// oriented function similar to an *printf function.
#[doc(alias = "xmlValidityWarningFunc")]
pub type XmlValidityWarningFunc = unsafe fn(ctx: *mut c_void, msg: *const i8);

pub type XmlValidCtxtPtr = *mut XmlValidCtxt;
/// An xmlValidCtxt is used for error reporting when validating.
#[doc(alias = "xmlValidCtxt")]
#[repr(C)]
pub struct XmlValidCtxt {
    pub(crate) user_data: Option<GenericErrorContext>, /* user specific data block */
    pub error: Option<GenericError>,                   /* the callback in case of errors */
    pub warning: Option<GenericError>,                 /* the callback in case of warning */

    /* Node analysis stack used when validating within entities */
    pub(crate) node: *mut XmlNode,          /* Current parsed Node */
    pub(crate) node_tab: Vec<*mut XmlNode>, /* array of nodes */

    pub(crate) flags: u32,             /* internal flags */
    pub(crate) doc: Option<XmlDocPtr>, /* the document */
    pub(crate) valid: i32,             /* temporary validity check result */

    // state state used for non-determinist content validation
    pub(crate) vstate_tab: Vec<XmlValidState>, /* array of validation states */

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
            // node_nr: 0,
            // node_max: 0,
            node_tab: vec![],
            flags: 0,
            doc: None,
            valid: 0,
            vstate_tab: vec![],
            am: null_mut(),
            state: null_mut(),
        }
    }
}

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
pub(crate) unsafe fn xml_verr_memory(ctxt: XmlValidCtxtPtr, extra: Option<&str>) {
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

/// Register a new notation declaration
///
/// Returns null_mut() if not, otherwise the entity
#[doc(alias = "xmlAddNotationDecl")]
pub unsafe fn xml_add_notation_decl<'a>(
    _ctxt: XmlValidCtxtPtr,
    dtd: Option<&'a mut XmlDtd>,
    name: &str,
    public_id: Option<&str>,
    system_id: Option<&str>,
) -> Option<&'a XmlNotation> {
    if public_id.is_none() && system_id.is_none() {
        return None;
    }

    // Create the Notation table if needed.
    let table = dtd?
        .notations
        .get_or_insert_with(|| Box::new(XmlHashTable::with_capacity(0)));

    let ret = XmlNotation::new(name, public_id, system_id);

    // Validity Check:
    // Check the DTD for previous declarations of the ATTLIST
    if table.add_entry(name, ret).is_err() {
        #[cfg(feature = "libxml_valid")]
        {
            xml_err_valid!(
                null_mut(),
                XmlParserErrors::XmlDTDNotationRedefined,
                "xmlAddNotationDecl: {} already defined\n",
                name
            );
        }
        return None;
    }
    table.lookup(name)
}

/// Build a copy of a notation table.
///
/// Returns the new xmlNotationTablePtr or null_mut() in case of error.
#[doc(alias = "xmlCopyNotationTable")]
#[cfg(feature = "libxml_tree")]
pub unsafe fn xml_copy_notation_table<'a>(
    table: &XmlHashTable<'a, XmlNotation>,
) -> XmlHashTable<'a, XmlNotation> {
    table.clone_with(|data, _| data.clone())
}

/// This will dump the content of the notation table as an XML DTD definition
#[doc(alias = "xmlDumpNotationTable")]
#[cfg(feature = "libxml_output")]
pub fn xml_dump_notation_table<'a>(
    out: &mut (impl Write + 'a),
    table: &XmlHashTable<'_, XmlNotation>,
) {
    use crate::tree::xml_dump_notation_decl;

    table.scan(|notation, _, _, _| {
        xml_dump_notation_decl(out, notation);
    });
}

/// Allocate an element content structure.
/// Deprecated in favor of xmlNewDocElementContent
///
/// Returns null_mut() if not, otherwise the new element content structure
#[doc(alias = "xmlNewElementContent")]
pub unsafe fn xml_new_element_content(
    name: Option<&str>,
    typ: XmlElementContentType,
) -> XmlElementContentPtr {
    xml_new_doc_element_content(None, name, typ)
}

/// Build a copy of an element content description.
/// Deprecated, use xmlCopyDocElementContent instead
///
/// Returns the new xmlElementContentPtr or null_mut() in case of error.
#[doc(alias = "xmlCopyElementContent")]
pub unsafe fn xml_copy_element_content(content: XmlElementContentPtr) -> XmlElementContentPtr {
    xml_copy_doc_element_content(None, content)
}

/// Free an element content structure. The whole subtree is removed.
/// Deprecated, use xmlFreeDocElementContent instead
#[doc(alias = "xmlFreeElementContent")]
pub unsafe fn xml_free_element_content(cur: XmlElementContentPtr) {
    xml_free_doc_element_content(None, cur);
}

/// Allocate an element content structure for the document.
///
/// Returns null_mut() if not, otherwise the new element content structure
#[doc(alias = "xmlNewDocElementContent")]
pub unsafe fn xml_new_doc_element_content(
    _doc: Option<XmlDocPtr>,
    name: Option<&str>,
    typ: XmlElementContentType,
) -> XmlElementContentPtr {
    match typ {
        XmlElementContentType::XmlElementContentElement => {
            if name.is_none() {
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
            if name.is_some() {
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
    if let Some(name) = name {
        if let Some((prefix, local)) = split_qname2(name) {
            (*ret).prefix = xml_strndup(prefix.as_ptr(), prefix.len() as i32);
            (*ret).name = xml_strndup(local.as_ptr(), local.len() as i32);
        } else {
            (*ret).name = xml_strndup(name.as_ptr(), name.len() as i32);
        }
    }
    ret
}

/// Build a copy of an element content description.
///
/// Returns the new xmlElementContentPtr or null_mut() in case of error.
#[doc(alias = "xmlCopyDocElementContent")]
pub unsafe fn xml_copy_doc_element_content(
    _doc: Option<XmlDocPtr>,
    mut cur: XmlElementContentPtr,
) -> XmlElementContentPtr {
    let mut prev: XmlElementContentPtr;
    let mut tmp: XmlElementContentPtr;

    if cur.is_null() {
        return null_mut();
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
        (*ret).name = xml_strdup((*cur).name);
    }

    if !(*cur).prefix.is_null() {
        (*ret).prefix = xml_strdup((*cur).prefix);
    }
    if !(*cur).c1.is_null() {
        (*ret).c1 = xml_copy_doc_element_content(_doc, (*cur).c1);
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
                (*tmp).name = xml_strdup((*cur).name);
            }

            if !(*cur).prefix.is_null() {
                (*tmp).prefix = xml_strdup((*cur).prefix);
            }
            if !(*cur).c1.is_null() {
                (*tmp).c1 = xml_copy_doc_element_content(_doc, (*cur).c1);
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
pub unsafe fn xml_free_doc_element_content(_doc: Option<XmlDocPtr>, mut cur: XmlElementContentPtr) {
    let mut depth: usize = 0;

    if cur.is_null() {
        return;
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
        if !(*cur).name.is_null() {
            xml_free((*cur).name as _);
        }
        if !(*cur).prefix.is_null() {
            xml_free((*cur).prefix as _);
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
pub unsafe fn xml_snprintf_element_content(
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
pub unsafe fn xml_sprintf_element_content(
    _buf: *mut c_char,
    _content: XmlElementContentPtr,
    _englob: i32,
) {
}

/// Handle a validation error, provide contextual information
///
/// # Note
/// This function does not format the string.
#[doc(alias = "xmlErrValidNode")]
#[cfg(any(feature = "libxml_valid", feature = "schema"))]
unsafe fn xml_err_valid_node(
    ctxt: XmlValidCtxtPtr,
    node: Option<XmlGenericNodePtr>,
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
        node.map_or(null_mut(), |node| node.as_ptr()) as _,
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
pub unsafe fn xml_add_element_decl(
    ctxt: XmlValidCtxtPtr,
    dtd: Option<XmlDtdPtr>,
    mut name: &str,
    typ: Option<XmlElementTypeVal>,
    content: XmlElementContentPtr,
) -> Option<XmlElementPtr> {
    let mut dtd = dtd?;
    match typ {
        Some(XmlElementTypeVal::XmlElementTypeEmpty) => {
            if !content.is_null() {
                xml_err_valid!(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    "xmlAddElementDecl: content != NULL for EMPTY\n"
                );
                return None;
            }
        }
        Some(XmlElementTypeVal::XmlElementTypeAny) => {
            if !content.is_null() {
                xml_err_valid!(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    "xmlAddElementDecl: content != NULL for ANY\n"
                );
                return None;
            }
        }
        Some(XmlElementTypeVal::XmlElementTypeMixed) => {
            if content.is_null() {
                xml_err_valid!(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    "xmlAddElementDecl: content == NULL for MIXED\n"
                );
                return None;
            }
        }
        Some(XmlElementTypeVal::XmlElementTypeElement) => {
            if content.is_null() {
                xml_err_valid!(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    "xmlAddElementDecl: content == NULL for ELEMENT\n"
                );
                return None;
            }
        }
        _ => {
            xml_err_valid!(
                ctxt,
                XmlParserErrors::XmlErrInternalError,
                "Internal: ELEMENT decl corrupted invalid type\n"
            );
            return None;
        }
    }

    // check if name is a QName
    let mut ns = None;
    if let Some((prefix, localname)) = split_qname2(name) {
        ns = Some(prefix);
        name = localname;
    }

    let mut old_attributes = None;
    // lookup old attributes inserted on an undefined element in the internal subset.
    if let Some(mut dtd) = dtd.doc.and_then(|doc| doc.int_subset) {
        let ret = dtd
            .elements
            .as_ref()
            .and_then(|table| table.lookup2(name, ns))
            .cloned();
        if let Some(mut ret) =
            ret.filter(|ret| ret.etype == XmlElementTypeVal::XmlElementTypeUndefined)
        {
            old_attributes = ret.attributes.take();
            dtd.elements
                .as_mut()
                .unwrap()
                .remove_entry2(name, ns, |_, _| {});
            xml_free_element(Some(ret));
        }
    }

    // Create the Element table if needed.
    let table = dtd
        .elements
        .get_or_insert_with(|| XmlHashTable::with_capacity(0));
    // The element may already be present if one of its attribute was registered first
    let mut ret = if let Some(ret) = table.lookup2(name, ns).cloned() {
        if !matches!(ret.etype, XmlElementTypeVal::XmlElementTypeUndefined) {
            #[cfg(feature = "libxml_valid")]
            {
                // The element is already defined in this DTD.
                xml_err_valid_node(
                    ctxt,
                    Some(dtd.into()),
                    XmlParserErrors::XmlDTDElemRedefined,
                    format!("Redefinition of element {name}\n").as_str(),
                    Some(name),
                    None,
                    None,
                );
            }
            return None;
        }
        ret
    } else {
        let Some(mut ret) = XmlElementPtr::new(XmlElement {
            typ: XmlElementType::XmlElementDecl,
            name: Some(Box::new(name.to_owned())),
            prefix: ns.map(|ns| ns.to_owned()),
            ..Default::default()
        }) else {
            xml_verr_memory(ctxt as _, Some("malloc failed"));
            return None;
        };

        // Validity Check:
        // Insertion must not fail
        if table.add_entry2(name, ns, ret).is_err() {
            #[cfg(feature = "libxml_valid")]
            {
                // The element is already defined in this DTD.
                xml_err_valid_node(
                    ctxt,
                    Some(dtd.into()),
                    XmlParserErrors::XmlDTDElemRedefined,
                    format!("Redefinition of element {name}\n").as_str(),
                    Some(name),
                    None,
                    None,
                );
            }
            ret.free();
            return None;
        }
        // For new element, may have attributes from earlier
        // definition in internal subset
        ret.attributes = old_attributes;
        ret
    };

    // Finish to fill the structure.
    ret.etype = typ.unwrap();
    // Avoid a stupid copy when called by the parser
    // and flag it by setting a special parent value
    // so the parser doesn't unallocate it.
    if !ctxt.is_null() && (*ctxt).flags & XML_VCTXT_USE_PCTXT as u32 != 0 {
        ret.content = content;
        if !content.is_null() {
            (*content).parent = 1 as XmlElementContentPtr;
        }
    } else {
        ret.content = xml_copy_doc_element_content(dtd.doc, content);
    }

    // Link it to the DTD
    ret.parent = Some(dtd);
    ret.doc = dtd.doc;
    if let Some(mut last) = dtd.last {
        last.next = NodePtr::from_ptr(ret.as_ptr() as *mut XmlNode);
        ret.prev = Some(last);
        dtd.last = NodePtr::from_ptr(ret.as_ptr() as *mut XmlNode);
    } else {
        dtd.children = NodePtr::from_ptr(ret.as_ptr() as *mut XmlNode);
        dtd.last = dtd.children;
    }
    Some(ret)
}

/// This will dump the content of the element table as an XML DTD definition
#[doc(alias = "xmlDumpElementTable")]
#[cfg(feature = "libxml_output")]
pub unsafe fn xml_dump_element_table<'a>(
    buf: &mut (impl Write + 'a),
    table: &XmlHashTable<'static, XmlElementPtr>,
) {
    table.scan(|data, _, _, _| xml_dump_element_decl(buf, *data));
}

/// Dump the occurrence operator of an element.
#[doc(alias = "xmlDumpElementOccur")]
#[cfg(feature = "libxml_output")]
unsafe fn xml_dump_element_occur<'a>(buf: &mut (impl Write + 'a), cur: XmlElementContentPtr) {
    match (*cur).ocur {
        XmlElementContentOccur::XmlElementContentOnce => {}
        XmlElementContentOccur::XmlElementContentOpt => {
            write!(buf, "?");
        }
        XmlElementContentOccur::XmlElementContentMult => {
            write!(buf, "*");
        }
        XmlElementContentOccur::XmlElementContentPlus => {
            write!(buf, "+");
        }
    }
}

/// This will dump the content of the element table as an XML DTD definition
#[doc(alias = "xmlDumpElementContent")]
#[cfg(feature = "libxml_output")]
unsafe fn xml_dump_element_content<'a>(buf: &mut (impl Write + 'a), content: XmlElementContentPtr) {
    if content.is_null() {
        return;
    }

    write!(buf, "(");
    let mut cur = content;

    while {
        'to_continue: {
            if cur.is_null() {
                return;
            }

            match (*cur).typ {
                XmlElementContentType::XmlElementContentPCDATA => {
                    write!(buf, "#PCDATA");
                }
                XmlElementContentType::XmlElementContentElement => {
                    if !(*cur).prefix.is_null() {
                        write!(
                            buf,
                            "{}:",
                            CStr::from_ptr((*cur).prefix as *const i8)
                                .to_string_lossy()
                                .as_ref()
                        );
                    }
                    write!(
                        buf,
                        "{}",
                        CStr::from_ptr((*cur).name as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                    );
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
                        write!(buf, "(");
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
                    write!(buf, ")");
                }
                xml_dump_element_occur(buf, cur);

                if cur == (*parent).c1 {
                    if (*parent).typ == XmlElementContentType::XmlElementContentSeq {
                        write!(buf, " , ");
                    } else if (*parent).typ == XmlElementContentType::XmlElementContentOr {
                        write!(buf, " | ");
                    }

                    cur = (*parent).c2;
                    break;
                }

                cur = parent;
            }
        }
        cur != content
    } {}

    write!(buf, ")");
    xml_dump_element_occur(buf, content);
}

/// This will dump the content of the element declaration as an XML DTD definition
#[doc(alias = "xmlDumpElementDecl")]
#[cfg(feature = "libxml_output")]
pub unsafe fn xml_dump_element_decl<'a>(buf: &mut (impl Write + 'a), elem: XmlElementPtr) {
    let name = elem.name.as_deref().unwrap();
    match elem.etype {
        XmlElementTypeVal::XmlElementTypeEmpty => {
            write!(buf, "<!ELEMENT ");
            if let Some(prefix) = elem.prefix.as_deref() {
                write!(buf, "{prefix}:");
            }
            writeln!(buf, "{} EMPTY>", name);
        }
        XmlElementTypeVal::XmlElementTypeAny => {
            write!(buf, "<!ELEMENT ");
            if let Some(prefix) = elem.prefix.as_deref() {
                write!(buf, "{prefix}:");
            }
            writeln!(buf, "{} ANY>", name);
        }
        XmlElementTypeVal::XmlElementTypeMixed => {
            write!(buf, "<!ELEMENT ");
            if let Some(prefix) = elem.prefix.as_deref() {
                write!(buf, "{prefix}:");
            }
            write!(buf, "{} ", name);
            xml_dump_element_content(buf, elem.content);
            writeln!(buf, ">",);
        }
        XmlElementTypeVal::XmlElementTypeElement => {
            write!(buf, "<!ELEMENT ");
            if let Some(prefix) = elem.prefix.as_deref() {
                write!(buf, "{prefix}:");
            }
            write!(buf, "{} ", name);
            xml_dump_element_content(buf, elem.content);
            writeln!(buf, ">",);
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

#[cfg(feature = "libxml_valid")]
unsafe fn xml_is_doc_name_start_char(doc: Option<XmlDocPtr>, c: i32) -> i32 {
    use super::parser_internals::xml_is_letter;

    if doc.map_or(true, |doc| {
        doc.properties & XmlDocProperties::XmlDocOld10 as i32 == 0
    }) {
        // Use the new checks of production [4] [4a] amd [5] of the
        // Update 5 of XML-1.0
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
unsafe fn xml_is_doc_name_char(doc: Option<XmlDocPtr>, c: i32) -> i32 {
    use crate::libxml::{
        chvalid::{xml_is_digit, xml_is_extender},
        parser_internals::xml_is_letter,
    };

    use super::chvalid::xml_is_combining;

    if doc.map_or(true, |doc| {
        doc.properties & XmlDocProperties::XmlDocOld10 as i32 == 0
    }) {
        // Use the new checks of production [4] [4a] amd [5] of the
        // Update 5 of XML-1.0
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
unsafe fn xml_validate_names_value_internal(doc: Option<XmlDocPtr>, value: *const XmlChar) -> i32 {
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

    // Should not test IS_BLANK(val) here -- see erratum E20
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
unsafe fn xml_validate_name_value_internal(doc: Option<XmlDocPtr>, value: *const XmlChar) -> i32 {
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
unsafe fn xml_validate_nmtokens_value_internal(
    doc: Option<XmlDocPtr>,
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

    // Should not test IS_BLANK(val) here -- see erratum E20
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
unsafe fn xml_validate_nmtoken_value_internal(
    doc: Option<XmlDocPtr>,
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
unsafe fn xml_validate_attribute_value_internal(
    doc: Option<XmlDocPtr>,
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

/// Search the DTD for the description of this element
///
/// returns the xmlElementPtr if found or null_mut()
#[doc(alias = "xmlGetDtdElementDesc2")]
unsafe fn xml_get_dtd_element_desc2(
    ctxt: XmlValidCtxtPtr,
    mut dtd: XmlDtdPtr,
    mut name: *const XmlChar,
    create: i32,
) -> Option<XmlElementPtr> {
    let mut prefix: *mut XmlChar = null_mut();

    if dtd.elements.is_none() && create == 0 {
        return None;
    }
    let table = dtd
        .elements
        .get_or_insert_with(|| XmlHashTable::with_capacity(0));
    let uqname: *mut XmlChar = xml_split_qname2(name, addr_of_mut!(prefix));
    if !uqname.is_null() {
        name = uqname;
    }
    let name = CStr::from_ptr(name as *const i8).to_string_lossy();
    let mut cur = table
        .lookup2(
            &name,
            (!prefix.is_null())
                .then(|| CStr::from_ptr(prefix as *const i8).to_string_lossy())
                .as_deref(),
        )
        .cloned();
    if cur.is_none() && create != 0 {
        let Some(res) = XmlElementPtr::new(XmlElement {
            typ: XmlElementType::XmlElementDecl,
            name: Some(Box::new(name.clone().into_owned())),
            prefix: (!prefix.is_null()).then(|| {
                CStr::from_ptr(prefix as *const i8)
                    .to_string_lossy()
                    .into_owned()
            }),
            etype: XmlElementTypeVal::XmlElementTypeUndefined,
            ..Default::default()
        }) else {
            xml_verr_memory(ctxt as _, Some("malloc failed"));
            //  goto error;
            if !prefix.is_null() {
                xml_free(prefix as _);
            }
            if !uqname.is_null() {
                xml_free(uqname as _);
            }
            return None;
        };
        cur = Some(res);
        if table
            .add_entry2(
                &name,
                (!prefix.is_null())
                    .then(|| CStr::from_ptr(prefix as *const i8).to_string_lossy())
                    .as_deref(),
                res,
            )
            .is_err()
        {
            xml_verr_memory(ctxt, Some("adding entry failed"));
            xml_free_element(cur);
            cur = None;
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
unsafe fn xml_scan_id_attribute_decl(ctxt: XmlValidCtxtPtr, elem: XmlElementPtr, err: i32) -> i32 {
    let mut ret: i32 = 0;

    let mut cur = elem.attributes;
    while let Some(now) = cur {
        if matches!(now.atype, XmlAttributeType::XmlAttributeID) {
            ret += 1;
            if ret > 1 && err != 0 {
                let elem_name = elem.name.as_deref().unwrap();
                let cur_name = now.name().unwrap();
                xml_err_valid_node(
                    ctxt,
                    Some(elem.into()),
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
        cur = now.nexth;
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
    dtd: Option<XmlDtdPtr>,
    elem: &str,
    name: &str,
    ns: Option<&str>,
    typ: XmlAttributeType,
    def: XmlAttributeDefault,
    mut default_value: Option<&str>,
    tree: Option<Box<XmlEnumeration>>,
) -> Option<XmlAttributePtr> {
    let mut dtd = dtd?;

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
            XmlAttributeType::XmlAttributeNotation => {}
        }
        if let Some(def) = default_value.filter(|&default_value| {
            let default_value = CString::new(default_value).unwrap();
            xml_validate_attribute_value_internal(dtd.doc, typ, default_value.as_ptr() as *const u8)
                == 0
        }) {
            xml_err_valid_node(
                ctxt,
                Some(dtd.into()),
                XmlParserErrors::XmlDTDAttributeDefault,
                format!("Attribute {elem} of {name}: invalid default value\n").as_str(),
                Some(elem),
                Some(name),
                Some(def),
            );
            default_value = None;
            if !ctxt.is_null() {
                (*ctxt).valid = 0;
            }
        }
    }

    // Check first that an attribute defined in the external subset wasn't
    // already defined in the internal subset
    if let Some(doc) = dtd.doc.filter(|doc| doc.ext_subset == Some(dtd)) {
        if let Some(int_subset) = doc.int_subset {
            if let Some(attributes) = int_subset.attributes {
                let ret = attributes.lookup3(name, ns, Some(elem)).copied();
                if ret.is_some() {
                    return None;
                }
            }
        }
    }

    // Create the Attribute table if needed.
    let mut table = if let Some(table) = dtd.attributes {
        table
    } else {
        let table = XmlHashTable::with_capacity(0);
        let Some(table) = XmlHashTableRef::from_table(table) else {
            xml_verr_memory(ctxt, Some("xmlAddAttributeDecl: Table creation failed!\n"));
            return None;
        };
        dtd.attributes = Some(table);
        table
    };

    let Some(mut ret) = XmlAttributePtr::new(XmlAttribute {
        typ: XmlElementType::XmlAttributeDecl,
        atype: typ,
        // doc must be set before possible error causes call
        // to xmlFreeAttribute (because it's used to check on dict use)
        doc: dtd.doc,
        name: xml_strndup(name.as_ptr(), name.len() as i32),
        prefix: ns.map(|ns| ns.to_owned()),
        elem: Some(elem.to_owned()),
        def,
        tree,
        ..Default::default()
    }) else {
        xml_verr_memory(ctxt as _, Some("malloc failed"));
        return None;
    };

    if let Some(default_value) = default_value {
        let default_value = CString::new(default_value).unwrap();
        ret.default_value = xml_strdup(default_value.as_ptr() as *const u8);
    }

    // Validity Check:
    // Search the DTD for previous declarations of the ATTLIST
    if table
        .add_entry3(
            (*ret).name().unwrap().as_ref(),
            ret.prefix.as_deref(),
            ret.elem.as_deref(),
            ret as _,
        )
        .is_err()
    {
        #[cfg(feature = "libxml_valid")]
        {
            // The attribute is already defined in this DTD.
            xml_err_valid_warning!(
                ctxt,
                dtd.as_ptr() as *mut XmlNode,
                XmlParserErrors::XmlDTDAttributeRedefined,
                "Attribute {} of element {}: already defined\n",
                name,
                elem
            );
        }
        xml_free_attribute(ret);
        return None;
    }

    let celem = CString::new(elem).unwrap();
    // Validity Check:
    // Multiple ID per element
    let elem_def = xml_get_dtd_element_desc2(ctxt, dtd, celem.as_ptr() as *const u8, 1);
    if let Some(mut elem_def) = elem_def {
        #[cfg(feature = "libxml_valid")]
        {
            if matches!(typ, XmlAttributeType::XmlAttributeID)
                && xml_scan_id_attribute_decl(null_mut(), elem_def, 1) != 0
            {
                xml_err_valid_node(
                    ctxt,
                    Some(dtd.into()),
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
        if (*ret).name().as_deref() == Some("xmlns") || ret.prefix.as_deref() == Some("xmlns") {
            ret.nexth = elem_def.attributes;
            elem_def.attributes = Some(ret);
        } else {
            let mut tmp = elem_def.attributes;

            while let Some(now) = tmp.filter(|tmp| {
                tmp.name().as_deref() == Some("xmlns") || ret.prefix.as_deref() == Some("xmlns")
            }) {
                if now.nexth.is_none() {
                    break;
                }
                tmp = now.nexth;
            }
            if let Some(mut tmp) = tmp {
                ret.nexth = tmp.nexth;
                tmp.nexth = Some(ret);
            } else {
                ret.nexth = elem_def.attributes;
                elem_def.attributes = Some(ret);
            }
        }
    }

    // Link it to the DTD
    ret.parent = Some(dtd);
    if let Some(mut last) = dtd.last {
        last.next = NodePtr::from_ptr(ret.as_ptr() as *mut XmlNode);
        ret.prev = Some(last);
        dtd.last = NodePtr::from_ptr(ret.as_ptr() as *mut XmlNode);
    } else {
        dtd.children = NodePtr::from_ptr(ret.as_ptr() as *mut XmlNode);
        dtd.last = NodePtr::from_ptr(ret.as_ptr() as *mut XmlNode);
    }
    Some(ret)
}

/// Build a copy of an attribute table.
///
/// Returns the new xmlAttributeTablePtr or null_mut() in case of error.
#[doc(alias = "xmlCopyAttributeTable")]
#[cfg(feature = "libxml_tree")]
pub unsafe fn xml_copy_attribute_table(
    table: XmlHashTableRef<'static, XmlAttributePtr>,
) -> Option<XmlHashTableRef<'_, XmlAttributePtr>> {
    let new = table.clone_with(|attr, _| {
        let mut cur = XmlAttributePtr::new(XmlAttribute {
            typ: XmlElementType::XmlAttributeDecl,
            atype: attr.atype,
            def: attr.def,
            tree: attr.tree.clone(),
            elem: attr.elem.clone(),
            prefix: attr.prefix.clone(),
            ..Default::default()
        })
        .unwrap();
        if !attr.name.is_null() {
            cur.name = xml_strdup(attr.name);
        }
        if !attr.default_value.is_null() {
            cur.default_value = xml_strdup(attr.default_value);
        }
        cur
    });
    XmlHashTableRef::from_table(new)
}

/// Deallocate the memory used by an entities hash table.
#[doc(alias = "xmlFreeAttributeTable")]
pub unsafe fn xml_free_attribute_table(mut table: XmlHashTable<'static, XmlAttributePtr>) {
    table.clear_with(|payload, _| {
        xml_free_attribute(payload);
    });
}

/// This will dump the content of the attribute table as an XML DTD definition
#[doc(alias = "xmlDumpAttributeTable")]
#[cfg(feature = "libxml_output")]
pub unsafe fn xml_dump_attribute_table<'a>(
    buf: &mut (impl Write + 'a),
    table: XmlHashTableRef<'static, XmlAttributePtr>,
) {
    table.scan(|data, _, _, _| xml_dump_attribute_decl(buf, *data));
}

/// This will dump the content of the attribute declaration as an XML DTD definition
#[doc(alias = "xmlDumpAttributeDecl")]
#[cfg(feature = "libxml_output")]
pub unsafe fn xml_dump_attribute_decl<'a>(buf: &mut (impl Write + 'a), attr: XmlAttributePtr) {
    use crate::{io::write_quoted, tree::xml_dump_enumeration};

    write!(buf, "<!ATTLIST ");
    let elem = attr.elem.as_deref().unwrap();
    write!(buf, "{elem}");
    write!(buf, " ");
    if let Some(prefix) = attr.prefix.as_deref() {
        write!(buf, "{}:", prefix);
    }
    write!(buf, "{}", (*attr).name().unwrap());
    match attr.atype {
        XmlAttributeType::XmlAttributeCDATA => write!(buf, " CDATA").ok(),
        XmlAttributeType::XmlAttributeID => write!(buf, " ID").ok(),
        XmlAttributeType::XmlAttributeIDREF => write!(buf, " IDREF").ok(),
        XmlAttributeType::XmlAttributeIDREFS => write!(buf, " IDREFS").ok(),
        XmlAttributeType::XmlAttributeEntity => write!(buf, " ENTITY").ok(),
        XmlAttributeType::XmlAttributeEntities => write!(buf, " ENTITIES").ok(),
        XmlAttributeType::XmlAttributeNmtoken => write!(buf, " NMTOKEN").ok(),
        XmlAttributeType::XmlAttributeNmtokens => write!(buf, " NMTOKENS").ok(),
        XmlAttributeType::XmlAttributeEnumeration => {
            write!(buf, " (");
            xml_dump_enumeration(buf, attr.tree.as_deref().unwrap());
            Some(())
        }
        XmlAttributeType::XmlAttributeNotation => {
            write!(buf, " NOTATION (");
            xml_dump_enumeration(buf, attr.tree.as_deref().unwrap());
            Some(())
        }
    };
    match attr.def {
        XmlAttributeDefault::XmlAttributeNone => None,
        XmlAttributeDefault::XmlAttributeRequired => write!(buf, " #REQUIRED").ok(),
        XmlAttributeDefault::XmlAttributeImplied => write!(buf, " #IMPLIED").ok(),
        XmlAttributeDefault::XmlAttributeFixed => write!(buf, " #FIXED").ok(),
    };
    if !attr.default_value.is_null() {
        write!(buf, " ");
        write_quoted(
            buf,
            CStr::from_ptr(attr.default_value as *const i8)
                .to_string_lossy()
                .as_ref(),
        );
    }
    writeln!(buf, ">");
}

unsafe fn xml_is_streaming(ctxt: XmlValidCtxtPtr) -> i32 {
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

/// Register a new id declaration
///
/// Returns null_mut() if not, otherwise the new xmlIDPtr
#[doc(alias = "xmlAddID")]
pub unsafe fn xml_add_id(
    ctxt: XmlValidCtxtPtr,
    mut doc: XmlDocPtr,
    value: &str,
    mut attr: XmlAttrPtr,
) -> Option<()> {
    if value.is_empty() {
        return None;
    }

    let mut ret = XmlID {
        value: value.to_owned(),
        doc: Some(doc),
        ..Default::default()
    };
    if xml_is_streaming(ctxt) != 0 {
        // Operating in streaming mode, attr is gonna disappear
        ret.name = attr.name().map(|n| n.into_owned());
        ret.attr = None;
    } else {
        ret.attr = Some(attr);
        ret.name = None;
    }
    ret.lineno = attr.parent.map_or(-1, |p| p.get_line_no() as i32);

    // Create the ID table if needed.
    doc.ids
        .get_or_insert(Box::new(XmlHashTable::with_capacity(0)));
    let table = doc.ids.as_deref_mut().unwrap();
    if table.add_entry(value, ret).is_err() {
        // The id is already defined in this DTD.
        #[cfg(feature = "libxml_valid")]
        if !ctxt.is_null() {
            xml_err_valid_node(
                ctxt,
                attr.parent
                    .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr())),
                XmlParserErrors::XmlDTDIDRedefined,
                format!("ID {value} already defined\n").as_str(),
                Some(value),
                None,
                None,
            );
        }
        return None;
    }
    attr.atype = Some(XmlAttributeType::XmlAttributeID);
    Some(())
}

/// Search the attribute declaring the given ID
///
/// Returns null_mut() if not found,
/// otherwise the xmlAttrPtr defining the ID or XmlDocPtr as `*mut dyn NodeCommon`.
#[doc(alias = "xmlGetID")]
pub unsafe fn xml_get_id(doc: XmlDocPtr, id: *const XmlChar) -> Option<NonNull<dyn NodeCommon>> {
    if id.is_null() {
        return None;
    }

    let table = doc.ids.as_deref()?;
    let id_ptr = table.lookup(CStr::from_ptr(id as *const i8).to_string_lossy().as_ref())?;
    match id_ptr.attr {
        Some(attr) => NonNull::new(attr.as_ptr() as *mut dyn NodeCommon),
        None => {
            // We are operating on a stream, return a well known reference
            // since the attribute node doesn't exist anymore
            NonNull::new(doc.as_ptr() as *mut dyn NodeCommon)
        }
    }
}

/// Determine whether an attribute is of type ID. In case we have DTD(s)
/// then this is done if DTD loading has been requested. In the case
/// of HTML documents parsed with the HTML parser, then ID detection is
/// done systematically.
///
/// Returns 0 or 1 depending on the lookup result
#[doc(alias = "xmlIsID")]
pub unsafe fn xml_is_id(
    doc: Option<XmlDocPtr>,
    elem: Option<XmlNodePtr>,
    attr: Option<XmlAttrPtr>,
) -> i32 {
    let Some(attr) = attr.filter(|a| !a.name.is_null()) else {
        return 0;
    };
    if attr.name().as_deref() == Some("id")
        && attr
            .ns
            .map_or(false, |ns| ns.prefix().as_deref() == Some("xml"))
    {
        return 1;
    }
    let Some(doc) = doc else {
        return 0;
    };
    if doc.int_subset.is_none()
        && doc.ext_subset.is_none()
        && !matches!(doc.typ, XmlElementType::XmlHTMLDocumentNode)
    {
        return 0;
    } else if matches!(doc.typ, XmlElementType::XmlHTMLDocumentNode) {
        if xml_str_equal(c"id".as_ptr() as _, attr.name)
            || (xml_str_equal(c"name".as_ptr() as _, attr.name)
                && elem.map_or(true, |elem| xml_str_equal(elem.name, c"a".as_ptr() as _)))
        {
            return 1;
        }
        return 0;
    } else if let Some(elem) = elem {
        let felem: [XmlChar; 50] = [0; 50];
        let fattr: [XmlChar; 50] = [0; 50];

        let fullelemname: *mut XmlChar =
            if let Some(prefix) = elem.ns.map(|ns| ns.prefix).filter(|p| !p.is_null()) {
                xml_build_qname(elem.name, prefix, felem.as_ptr() as _, 50)
            } else {
                elem.name as *mut XmlChar
            };

        let fullattrname: *mut XmlChar =
            if let Some(prefix) = attr.ns.map(|ns| ns.prefix).filter(|pre| !pre.is_null()) {
                xml_build_qname(attr.name, prefix, fattr.as_ptr() as _, 50)
            } else {
                attr.name as *mut XmlChar
            };

        let mut attr_decl = None;
        if !fullelemname.is_null() && !fullattrname.is_null() {
            attr_decl = doc.int_subset.and_then(|dtd| {
                dtd.get_attr_desc(
                    CStr::from_ptr(fullelemname as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    CStr::from_ptr(fullattrname as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                )
            });
            if attr_decl.is_none() {
                if let Some(ext_subset) = doc.ext_subset {
                    attr_decl = ext_subset.get_attr_desc(
                        CStr::from_ptr(fullelemname as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                        CStr::from_ptr(fullattrname as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                    );
                }
            }
        }

        if fullattrname != fattr.as_ptr() as _ && fullattrname != attr.name as _ {
            xml_free(fullattrname as _);
        }
        if fullelemname != felem.as_ptr() as _ && fullelemname != elem.name as _ {
            xml_free(fullelemname as _);
        }

        if attr_decl.map_or(false, |attr_decl| {
            matches!(attr_decl.atype, XmlAttributeType::XmlAttributeID)
        }) {
            return 1;
        }
    } else {
        return 0;
    }
    0
}

/// Normalize a string in-place.
#[doc(alias = "xmlValidNormalizeString")]
unsafe fn xml_valid_normalize_string(str: *mut XmlChar) {
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
pub unsafe fn xml_remove_id(mut doc: XmlDocPtr, mut attr: XmlAttrPtr) -> i32 {
    if doc.ids.is_none() {
        return -1;
    }
    let Some(id) = attr.children.and_then(|c| c.get_string(Some(doc), 1)) else {
        return -1;
    };
    let id = CString::new(id).unwrap();
    let id = xml_strdup(id.as_ptr() as *const u8);
    xml_valid_normalize_string(id);

    let table = doc.ids.as_deref_mut().unwrap();
    let Some(id_ptr) = table.lookup(CStr::from_ptr(id as *const i8).to_string_lossy().as_ref())
    else {
        xml_free(id as _);
        return -1;
    };
    if id_ptr.attr != Some(attr) {
        xml_free(id as _);
        return -1;
    }

    table.remove_entry(
        CStr::from_ptr(id as *const i8).to_string_lossy().as_ref(),
        |_, _| {},
    );
    xml_free(id as _);
    attr.atype = None;
    0
}

/// Register a new ref declaration
///
/// Returns `None` if not, otherwise `Some(())`
///
/// # Note
/// This function in original libxml2 returns new `xmlRefPtr`.  
/// However, this function cannot returns `Option<&XmlRef>`.
#[doc(alias = "xmlAddRef")]
pub(crate) unsafe fn xml_add_ref(
    ctxt: XmlValidCtxtPtr,
    mut doc: XmlDocPtr,
    value: &str,
    attr: XmlAttrPtr,
) -> Option<()> {
    // Create the Ref table if needed.
    let table = doc.refs.get_or_insert_with(HashMap::new);
    let mut ret = XmlRef {
        value: value.to_owned(),
        ..Default::default()
    };
    // fill the structure.
    if xml_is_streaming(ctxt) != 0 {
        // Operating in streaming mode, attr is gonna disappear
        ret.name = attr.name().map(|n| n.into_owned());
        ret.attr = None;
    } else {
        ret.name = None;
        ret.attr = Some(attr);
    }
    ret.lineno = attr.parent.map_or(-1, |p| p.get_line_no() as i32);

    // To add a reference :-
    // References are maintained as a list of references,
    // Lookup the entry, if no entry create new nodelist
    // Add the owning node to the NodeList
    // Return the ref

    let ref_list = table
        .entry(value.to_owned())
        .or_insert_with(|| XmlList::new(None, Rc::new(|_, _| std::cmp::Ordering::Equal)));
    ref_list.insert_upper_bound(Box::new(ret));
    Some(())
}

/// Determine whether an attribute is of type Ref. In case we have DTD(s)
/// then this is simple, otherwise we use an heuristic: name Ref (upper or lowercase).
///
/// Returns 0 or 1 depending on the lookup result
#[doc(alias = "xmlIsRef")]
pub(crate) unsafe fn xml_is_ref(
    doc: Option<XmlDocPtr>,
    elem: *mut XmlNode,
    attr: Option<XmlAttrPtr>,
) -> i32 {
    let Some(attr) = attr else {
        return 0;
    };
    let Some(doc) = doc.or(attr.doc) else {
        return 0;
    };

    if doc.int_subset.is_none() && doc.ext_subset.is_none() {
        return 0;
    } else if matches!(doc.typ, XmlElementType::XmlHTMLDocumentNode) {
        // TODO @@@
        return 0;
    } else {
        if elem.is_null() {
            return 0;
        }
        let mut attr_decl = doc.int_subset.and_then(|dtd| {
            dtd.get_attr_desc(
                (*elem).name().as_deref().unwrap(),
                (*attr).name().as_deref().unwrap(),
            )
        });
        if attr_decl.is_none() {
            if let Some(ext_subset) = doc.ext_subset {
                attr_decl = ext_subset.get_attr_desc(
                    (*elem).name().as_deref().unwrap(),
                    (*attr).name().as_deref().unwrap(),
                );
            }
        }

        if attr_decl.map_or(false, |attr_decl| {
            matches!(
                attr_decl.atype,
                XmlAttributeType::XmlAttributeIDREF | XmlAttributeType::XmlAttributeIDREFS
            )
        }) {
            return 1;
        }
    }
    0
}

/// Remove the given attribute from the Ref table maintained internally.
///
/// Returns -1 if the lookup failed and 0 otherwise
#[doc(alias = "xmlRemoveRef")]
pub(crate) unsafe fn xml_remove_ref(mut doc: XmlDocPtr, attr: XmlAttrPtr) -> i32 {
    if doc.refs.is_none() {
        return -1;
    }

    let Some(id) = attr.children.and_then(|c| c.get_string(Some(doc), 1)) else {
        return -1;
    };

    let table = doc.refs.as_mut().unwrap();
    let Some(ref_list) = table.get_mut(&id) else {
        return -1;
    };

    // At this point, ref_list refers to a list of references which
    // have the same key as the supplied attr. Our list of references
    // is ordered by reference address and we don't have that information
    // here to use when removing. We'll have to walk the list and
    // check for a matching attribute, when we find one stop the walk
    // and remove the entry.
    // The list is ordered by reference, so that means we don't have the
    // key. Passing the list and the reference to the walker means we
    // will have enough data to be able to remove the entry.

    // Remove the supplied attr from our list
    ref_list.remove_first_by(|refe| refe.attr == Some(attr));

    // If the list is empty then remove the list entry in the hash
    if ref_list.is_empty() {
        table.remove(&id);
    }
    0
}

/// Find the set of references for the supplied ID.
///
/// Returns `None` if not found, otherwise node set for the ID.
#[doc(alias = "xmlGetRefs")]
pub(crate) unsafe fn xml_get_refs<'a>(
    doc: &'a XmlDoc,
    id: &str,
) -> Option<&'a XmlList<Box<XmlRef>>> {
    doc.refs.as_ref()?.get(id)
}

/// Allocate a validation context structure.
///
/// Returns null_mut() if not, otherwise the new validation context structure
#[doc(alias = "xmlNewValidCtxt")]
#[cfg(feature = "libxml_valid")]
pub unsafe fn xml_new_valid_ctxt() -> XmlValidCtxtPtr {
    let ret: XmlValidCtxtPtr = xml_malloc(size_of::<XmlValidCtxt>()) as _;
    if ret.is_null() {
        xml_verr_memory(null_mut(), Some("malloc failed"));
        return null_mut();
    }

    std::ptr::write(&mut *ret, XmlValidCtxt::default());

    ret
}

/// Free a validation context structure.
#[doc(alias = "xmlFreeValidCtxt")]
#[cfg(feature = "libxml_valid")]
pub unsafe fn xml_free_valid_ctxt(cur: XmlValidCtxtPtr) {
    use std::ptr::drop_in_place;

    if cur.is_null() {
        return;
    }
    drop_in_place(cur);
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
pub unsafe fn xml_validate_root(ctxt: XmlValidCtxtPtr, doc: XmlDocPtr) -> i32 {
    let ret: i32;

    // if doc.is_null() {
    //     return 0;
    // }

    let Some(root) = doc.get_root_element().filter(|root| !root.name.is_null()) else {
        xml_err_valid!(ctxt, XmlParserErrors::XmlDTDNoRoot, "no root element\n");
        return 0;
    };

    // When doing post validation against a separate DTD, those may
    // no internal subset has been generated
    if let Some(int_subset) = doc.int_subset.filter(|dtd| !dtd.name.is_null()) {
        // Check first the document root against the NQName
        if !xml_str_equal(int_subset.name, root.name) {
            if let Some(prefix) = root.ns.map(|ns| ns.prefix).filter(|p| !p.is_null()) {
                let mut fname: [XmlChar; 50] = [0; 50];

                let fullname: *mut XmlChar =
                    xml_build_qname(root.name, prefix, fname.as_mut_ptr(), 50);
                if fullname.is_null() {
                    xml_verr_memory(ctxt, None);
                    return 0;
                }
                ret = xml_str_equal(int_subset.name, fullname) as i32;
                if fullname != fname.as_ptr() as _ && fullname != root.name as _ {
                    xml_free(fullname as _);
                }
                if ret == 1 {
                    // goto name_ok;
                    return 1;
                }
            }
            if xml_str_equal(int_subset.name, c"HTML".as_ptr() as _)
                && xml_str_equal(root.name, c"html".as_ptr() as _)
            {
                // goto name_ok;
                return 1;
            }

            let root_name = root.name().unwrap();
            let subset_name = int_subset.name().unwrap();
            xml_err_valid_node(
                ctxt,
                Some(root.into()),
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
        } else if (*$doc).int_subset.is_none() && (*$doc).ext_subset.is_none() {
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
pub unsafe fn xml_validate_element_decl(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    elem: Option<XmlElementPtr>,
) -> i32 {
    let mut ret: i32 = 1;

    // if doc.is_null() {
    //     return 0;
    // }
    if doc.int_subset.is_none() && doc.ext_subset.is_none() {
        return 0;
    };

    let Some(elem) = elem else {
        return 1;
    };

    // #if 0
    // #ifdef LIBXML_REGEXP_ENABLED
    //     /* Build the regexp associated to the content model */
    //     ret = xmlValidBuildContentModel(ctxt, elem);
    // #endif
    // #endif

    // No Duplicate Types
    if matches!(elem.etype, XmlElementTypeVal::XmlElementTypeMixed) {
        let mut cur: XmlElementContentPtr;
        let mut next: XmlElementContentPtr;
        let mut name: *const XmlChar;

        cur = elem.content;
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
                            let elem_name = elem.name.as_deref().unwrap();
                            let name = CStr::from_ptr(name as *const i8).to_string_lossy();
                            if (*(*cur).c1).prefix.is_null() {
                                xml_err_valid_node(
                                    ctxt,
                                    Some(elem.into()),
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
                                    Some(elem.into()),
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
                        let elem_name = elem.name.as_deref().unwrap();
                        let name = CStr::from_ptr(name as *const i8).to_string_lossy();
                        if (*(*cur).c1).prefix.is_null() {
                            xml_err_valid_node(
                                ctxt,
                                Some(elem.into()),
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
                                Some(elem.into()),
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

    let elem_name = elem
        .name
        .as_ref()
        .map(|n| CString::new(n.as_str()).unwrap());
    // VC: Unique Element Type Declaration
    let tst = xml_get_dtd_element_desc(
        doc.int_subset,
        elem_name
            .as_ref()
            .map_or(null(), |n| n.as_ptr() as *const u8),
    );
    if tst.map_or(false, |tst| {
        tst != elem
            && tst.prefix == elem.prefix
            && !matches!(tst.etype, XmlElementTypeVal::XmlElementTypeUndefined)
    }) {
        let elem_name = elem_name.as_deref().unwrap().to_string_lossy();
        xml_err_valid_node(
            ctxt,
            Some(elem.into()),
            XmlParserErrors::XmlDTDElemRedefined,
            format!("Redefinition of element {elem_name}\n").as_str(),
            Some(&elem_name),
            None,
            None,
        );
        ret = 0;
    }
    let tst = xml_get_dtd_element_desc(
        doc.ext_subset,
        elem_name
            .as_ref()
            .map_or(null(), |n| n.as_ptr() as *const u8),
    );
    if tst.map_or(false, |tst| {
        tst != elem
            && tst.prefix == elem.prefix
            && !matches!(tst.etype, XmlElementTypeVal::XmlElementTypeUndefined)
    }) {
        let elem_name = elem_name.as_deref().unwrap().to_string_lossy();
        xml_err_valid_node(
            ctxt,
            Some(elem.into()),
            XmlParserErrors::XmlDTDElemRedefined,
            format!("Redefinition of element {elem_name}\n").as_str(),
            Some(&elem_name),
            None,
            None,
        );
        ret = 0;
    }
    // One ID per Element Type
    // already done when registering the attribute
    // if (xmlScanIDAttributeDecl(ctxt, elem) > 1) {
    //     ret = 0;
    // }
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
    elem: *mut XmlNode,
    name: &str,
    value: *const XmlChar,
) -> *mut XmlChar {
    // if doc.is_null() {
    //     return null_mut();
    // }
    if elem.is_null() {
        return null_mut();
    }
    if value.is_null() {
        return null_mut();
    }

    if let Some(prefix) = (*elem).ns.map(|ns| ns.prefix).filter(|p| !p.is_null()) {
        let mut fname: [XmlChar; 50] = [0; 50];

        let fullname: *mut XmlChar = xml_build_qname((*elem).name, prefix, fname.as_mut_ptr(), 50);
        if fullname.is_null() {
            return null_mut();
        }
        if fullname != fname.as_ptr() as _ && fullname != (*elem).name as _ {
            xml_free(fullname as _);
        }
    }
    let mut attr_decl = doc
        .int_subset
        .and_then(|dtd| dtd.get_attr_desc((*elem).name().as_deref().unwrap(), name));
    if attr_decl.is_none() {
        if let Some(ext_subset) = doc.ext_subset {
            attr_decl = ext_subset.get_attr_desc((*elem).name().as_deref().unwrap(), name);
        }
    }

    let Some(attr_decl) = attr_decl else {
        return null_mut();
    };
    if matches!(attr_decl.atype, XmlAttributeType::XmlAttributeCDATA) {
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
    elem: *mut XmlNode,
    name: &str,
    value: *const XmlChar,
) -> *mut XmlChar {
    let mut extsubset: i32 = 0;

    // if doc.is_null() {
    //     return null_mut();
    // }
    if elem.is_null() {
        return null_mut();
    }
    if value.is_null() {
        return null_mut();
    }

    let mut attr_decl = None;
    if let Some(prefix) = (*elem).ns.map(|ns| ns.prefix).filter(|p| !p.is_null()) {
        let mut fname: [XmlChar; 50] = [0; 50];

        let fullname: *mut XmlChar = xml_build_qname((*elem).name, prefix, fname.as_mut_ptr(), 50);
        if fullname.is_null() {
            return null_mut();
        }
        attr_decl = doc.int_subset.and_then(|dtd| {
            dtd.get_attr_desc(
                CStr::from_ptr(fullname as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                name,
            )
        });
        if attr_decl.is_none() {
            if let Some(ext_subset) = doc.ext_subset {
                attr_decl = ext_subset.get_attr_desc(
                    CStr::from_ptr(fullname as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    name,
                );
                if attr_decl.is_some() {
                    extsubset = 1;
                }
            }
        }
        if fullname != fname.as_ptr() as _ && fullname != (*elem).name as _ {
            xml_free(fullname as _);
        }
    }
    if attr_decl.is_none() {
        if let Some(int_subset) = doc.int_subset {
            attr_decl = int_subset.get_attr_desc((*elem).name().as_deref().unwrap(), name);
        }
    }
    if attr_decl.is_none() {
        if let Some(ext_subset) = doc.ext_subset {
            attr_decl = ext_subset.get_attr_desc((*elem).name().as_deref().unwrap(), name);
            if attr_decl.is_some() {
                extsubset = 1;
            }
        }
    }

    let Some(attr_decl) = attr_decl else {
        return null_mut();
    };
    if matches!(attr_decl.atype, XmlAttributeType::XmlAttributeCDATA) {
        return null_mut();
    }

    let ret: *mut XmlChar = xml_strdup(value);
    if ret.is_null() {
        return null_mut();
    }
    xml_valid_normalize_string(ret);
    if doc.standalone != 0 && extsubset == 1 && !xml_str_equal(value, ret) {
        let elem_name = (*elem).name().unwrap();
        xml_err_valid_node(
            ctxt,
            XmlGenericNodePtr::from_raw(elem),
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
pub unsafe fn xml_validate_attribute_decl(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    attr: XmlAttributePtr,
) -> i32 {
    let mut ret: i32 = 1;
    let val: i32;
    // if doc.is_null() {
    //     return 0;
    // }
    if doc.int_subset.is_none() && doc.ext_subset.is_none() {
        return 0;
    };

    let attr_elem = attr.elem.as_deref().map(|e| CString::new(e).unwrap());
    // Attribute Default Legal
    // Enumeration
    if !attr.default_value.is_null() {
        val = xml_validate_attribute_value_internal(Some(doc), attr.atype, attr.default_value);
        if val == 0 {
            let attr_name = (*attr).name().unwrap();
            let attr_elem = attr_elem.as_deref().unwrap().to_string_lossy();
            xml_err_valid_node(
                ctxt,
                Some(attr.into()),
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
    if matches!(attr.atype, XmlAttributeType::XmlAttributeID)
        && !matches!(
            attr.def,
            XmlAttributeDefault::XmlAttributeImplied | XmlAttributeDefault::XmlAttributeRequired
        )
    {
        let attr_name = attr.name().unwrap();
        let attr_elem = attr_elem.as_deref().unwrap().to_string_lossy();
        xml_err_valid_node(
            ctxt,
            Some(attr.into()),
            XmlParserErrors::XmlDTDIDFixed,
            format!("ID attribute {attr_name} of {attr_elem} is not valid must be #IMPLIED or #REQUIRED\n").as_str(),
            Some(&attr_name),
            Some(&attr_elem),
            None,
        );
        ret = 0;
    }

    // One ID per Element Type
    if matches!(attr.atype, XmlAttributeType::XmlAttributeID) {
        let mut nb_id: i32;

        // the trick is that we parse DtD as their own internal subset
        let mut elem = xml_get_dtd_element_desc(
            doc.int_subset,
            attr_elem
                .as_ref()
                .map_or(null(), |e| e.as_ptr() as *const u8),
        );
        if let Some(elem) = elem {
            nb_id = xml_scan_id_attribute_decl(null_mut(), elem, 0);
        } else {
            // The attribute may be declared in the internal subset and the
            // element in the external subset.
            nb_id = 0;
            if let Some(int_subset) = doc.int_subset {
                if let Some(table) = int_subset.attributes {
                    table.scan(|&payload, _, _, name3| {
                        if matches!(payload.atype, XmlAttributeType::XmlAttributeID)
                            && name3.map(|n| n.as_ref())
                                == attr_elem.as_deref().map(|a| a.to_string_lossy()).as_deref()
                        {
                            nb_id += 1;
                        }
                    });
                }
            }
        }
        if nb_id > 1 {
            xml_err_valid_node_nr!(
                ctxt,
                attr.as_ptr() as *mut XmlNode,
                XmlParserErrors::XmlDTDIDSubset,
                "Element {} has {} ID attribute defined in the internal subset : {}\n",
                attr_elem.as_deref().unwrap().to_string_lossy().into_owned(),
                nb_id,
                (*attr).name().unwrap().into_owned()
            );
        } else if doc.ext_subset.is_some() {
            let mut ext_id: i32 = 0;
            elem = xml_get_dtd_element_desc(
                doc.ext_subset,
                attr_elem
                    .as_ref()
                    .map_or(null(), |e| e.as_ptr() as *const u8),
            );
            if let Some(elem) = elem {
                ext_id = xml_scan_id_attribute_decl(null_mut(), elem, 0);
            }
            if ext_id > 1 {
                xml_err_valid_node_nr!(
                    ctxt,
                    attr.as_ptr() as *mut XmlNode,
                    XmlParserErrors::XmlDTDIDSubset,
                    "Element {} has {} ID attribute defined in the external subset : {}\n",
                    attr_elem.as_deref().unwrap().to_string_lossy().into_owned(),
                    ext_id,
                    (*attr).name().unwrap().into_owned()
                );
            } else if ext_id + nb_id > 1 {
                let attr_elem = attr_elem.as_deref().unwrap().to_string_lossy();
                let attr_name = (*attr).name().unwrap();
                xml_err_valid_node(
                    ctxt,
                    Some(attr.into()),
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
    if !attr.default_value.is_null() && attr.tree.is_some() {
        let mut tree = attr.tree.as_deref();
        while let Some(now) = tree {
            if now.name == CStr::from_ptr(attr.default_value as *const i8).to_string_lossy() {
                break;
            }
            tree = now.next.as_deref();
        }
        if tree.is_none() {
            let attr_name = attr.name().unwrap();
            let attr_elem = attr_elem.as_deref().unwrap().to_string_lossy();
            let attr_def = CStr::from_ptr(attr.default_value as *const i8).to_string_lossy();
            xml_err_valid_node(
                ctxt,
                Some(attr.into()),
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
/// ```text
/// [ VC: ID ]
/// Values of type ID must match the Name production....
///
/// [ VC: IDREF ]
/// Values of type IDREF must match the Name production, and values
/// of type IDREFS must match Names ...
///
/// [ VC: Entity Name ]
/// Values of type ENTITY must match the Name production, values
/// of type ENTITIES must match Names ...
///
/// [ VC: Name Token ]
/// Values of type NMTOKEN must match the Nmtoken production; values
/// of type NMTOKENS must match Nmtokens.
/// ```
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateAttributeValue")]
#[cfg(feature = "libxml_valid")]
pub unsafe fn xml_validate_attribute_value(typ: XmlAttributeType, value: *const XmlChar) -> i32 {
    xml_validate_attribute_value_internal(None, typ, value)
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
pub unsafe fn xml_validate_notation_decl(
    _ctxt: XmlValidCtxtPtr,
    _doc: Option<XmlDocPtr>,
    _nota: Option<&XmlNotation>,
) -> i32 {
    1
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
pub unsafe fn xml_validate_dtd(ctxt: XmlValidCtxtPtr, mut doc: XmlDocPtr, dtd: XmlDtdPtr) -> i32 {
    // if doc.is_null() {
    //     return 0;
    // }
    let old_ext = doc.ext_subset;
    let old_int = doc.int_subset;
    doc.ext_subset = Some(dtd);
    doc.int_subset = None;
    let mut ret = xml_validate_root(ctxt, doc);
    if ret == 0 {
        doc.ext_subset = old_ext;
        doc.int_subset = old_int;
        return ret;
    }
    doc.ids.take();
    doc.refs.take();
    let root = doc.get_root_element();
    ret = xml_validate_element(ctxt, doc, root.map(|root| root.into()));
    ret &= xml_validate_document_final(ctxt, doc);
    doc.ext_subset = old_ext;
    doc.int_subset = old_int;
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
    mut doc: XmlDocPtr,
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
            let mut ent = xml_get_doc_entity(Some(doc), value);
            // yeah it's a bit messy...
            if ent.is_none() && doc.standalone == 1 {
                doc.standalone = 0;
                ent = xml_get_doc_entity(Some(doc), value);
            }
            if let Some(ent) = ent {
                if !matches!(ent.etype, XmlEntityType::XmlExternalGeneralUnparsedEntity) {
                    let name = CStr::from_ptr(name as *const i8).to_string_lossy();
                    xml_err_valid_node(
                        ctxt,
                        Some(doc.into()),
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
            } else {
                let name = CStr::from_ptr(name as *const i8).to_string_lossy();
                xml_err_valid_node(
                    ctxt,
                    Some(doc.into()),
                    XmlParserErrors::XmlDTDUnknownEntity,
                    format!("ENTITY attribute {name} reference an unknown entity \"{value}\"\n")
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
                if let Some(ent) = xml_get_doc_entity(Some(doc), &nam) {
                    if !matches!(ent.etype, XmlEntityType::XmlExternalGeneralUnparsedEntity) {
                        let name = CStr::from_ptr(name as *const i8).to_string_lossy();
                        xml_err_valid_node(
                            ctxt,
                            Some(doc.into()),
                            XmlParserErrors::XmlDTDEntityType,
                            format!("ENTITIES attribute {name} reference an entity \"{nam}\" of wrong type\n")
                                .as_str(),
                            Some(&name),
                            Some(&nam),
                            None,
                        );
                        ret = 0;
                    }
                } else {
                    let name = CStr::from_ptr(name as *const i8).to_string_lossy();
                    xml_err_valid_node(
                        ctxt,
                        Some(doc.into()),
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
            let nota = xml_get_dtd_notation_desc(doc.int_subset.as_deref(), value)
                .or_else(|| xml_get_dtd_notation_desc(doc.ext_subset.as_deref(), value));

            if nota.is_none() {
                let name = CStr::from_ptr(name as *const i8).to_string_lossy();
                xml_err_valid_node(
                    ctxt,
                    Some(doc.into()),
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

unsafe fn xml_validate_attribute_callback(cur: XmlAttributePtr, ctxt: XmlValidCtxtPtr) {
    let mut ret: i32;

    match cur.atype {
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
            if !cur.default_value.is_null() {
                ret = xml_validate_attribute_value2(
                    ctxt,
                    (*ctxt).doc.unwrap(),
                    cur.name,
                    cur.atype,
                    &CStr::from_ptr(cur.default_value as *const i8).to_string_lossy(),
                );
                if ret == 0 && (*ctxt).valid == 1 {
                    (*ctxt).valid = 0;
                }
            }
            if cur.tree.is_some() {
                let mut tree = cur.tree.as_deref();
                while let Some(now) = tree {
                    ret = xml_validate_attribute_value2(
                        ctxt,
                        (*ctxt).doc.unwrap(),
                        cur.name,
                        cur.atype,
                        &now.name,
                    );
                    if ret == 0 && (*ctxt).valid == 1 {
                        (*ctxt).valid = 0;
                    }
                    tree = now.next.as_deref();
                }
            }
        }
    }
    if matches!(cur.atype, XmlAttributeType::XmlAttributeNotation) {
        let doc = cur.doc;
        let Some(cur_elem) = cur.elem.as_deref().map(|e| CString::new(e).unwrap()) else {
            xml_err_valid!(
                ctxt,
                XmlParserErrors::XmlErrInternalError,
                "xmlValidateAttributeCallback({}): internal error\n",
                (*cur).name().as_deref().unwrap()
            );
            return;
        };

        let mut elem = None;
        if let Some(doc) = doc {
            elem = xml_get_dtd_element_desc(doc.int_subset, cur_elem.as_ptr() as *const u8);
            if elem.is_none() {
                elem = xml_get_dtd_element_desc(doc.ext_subset, cur_elem.as_ptr() as *const u8);
            }
        }
        if elem.is_none() {
            if let Some(dtd) = cur
                .parent
                .filter(|dtd| dtd.element_type() == XmlElementType::XmlDTDNode)
            {
                elem = xml_get_dtd_element_desc(Some(dtd), cur_elem.as_ptr() as *const u8);
            }
        }
        let Some(elem) = elem else {
            let name = (*cur).name().unwrap();
            let cur_elem = cur_elem.to_string_lossy();
            xml_err_valid_node(
                ctxt,
                None,
                XmlParserErrors::XmlDTDUnknownElem,
                format!("attribute {name}: could not find decl for element {cur_elem}\n").as_str(),
                Some(&name),
                Some(&cur_elem),
                None,
            );
            return;
        };
        if matches!(elem.etype, XmlElementTypeVal::XmlElementTypeEmpty) {
            let name = (*cur).name().unwrap();
            let cur_elem = cur_elem.to_string_lossy();
            xml_err_valid_node(
                ctxt,
                None,
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

unsafe fn xml_validate_notation_callback(cur: XmlEntityPtr, ctxt: XmlValidCtxtPtr) {
    if matches!(cur.etype, XmlEntityType::XmlExternalGeneralUnparsedEntity) {
        let notation: *mut XmlChar = cur.content.load(Ordering::Relaxed) as _;

        if !notation.is_null() {
            let ret: i32 = xml_validate_notation_use(
                ctxt,
                XmlDocPtr::from_raw(cur.doc.load(Ordering::Relaxed) as _)
                    .unwrap()
                    .unwrap(),
                CStr::from_ptr(notation as *const i8)
                    .to_string_lossy()
                    .as_ref(),
            );
            if ret != 1 {
                (*ctxt).valid = 0;
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
pub unsafe fn xml_validate_dtd_final(ctxt: XmlValidCtxtPtr, doc: XmlDocPtr) -> i32 {
    // if doc.is_null() {
    //     return 0;
    // }
    if ctxt.is_null() {
        return 0;
    }
    if doc.int_subset.is_none() && doc.ext_subset.is_none() {
        return 0;
    }
    (*ctxt).doc = Some(doc);
    (*ctxt).valid = 1;
    let dtd = doc.int_subset;
    if let Some(dtd) = dtd {
        if let Some(table) = dtd.attributes {
            table.scan(|&payload, _, _, _| {
                xml_validate_attribute_callback(payload, ctxt);
            });
        }
        if let Some(entities) = dtd.entities {
            entities.scan(|payload, _, _, _| {
                xml_validate_notation_callback(*payload, ctxt);
            });
        }
    }
    let dtd = doc.ext_subset;
    if let Some(dtd) = dtd {
        if let Some(table) = dtd.attributes {
            table.scan(|payload, _, _, _| {
                xml_validate_attribute_callback(*payload, ctxt);
            });
        }
        if let Some(entities) = dtd.entities {
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
pub unsafe fn xml_validate_document(ctxt: XmlValidCtxtPtr, mut doc: XmlDocPtr) -> i32 {
    use crate::{libxml::parser::xml_parse_dtd, uri::build_uri};

    let mut ret: i32;

    // if doc.is_null() {
    //     return 0;
    // }
    if doc.int_subset.is_none() && doc.ext_subset.is_none() {
        xml_err_valid!(ctxt, XmlParserErrors::XmlDTDNoDTD, "no DTD found!\n");
        return 0;
    }
    if let Some(int_subset) = doc.int_subset.filter(|dtd| {
        (dtd.system_id.is_some() || dtd.external_id.is_some()) && doc.ext_subset.is_none()
    }) {
        let sys_id = if let Some(system_id) = int_subset.system_id.as_deref() {
            let Some(sys_id) = doc
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
        let external_id = int_subset.external_id.as_deref();
        doc.ext_subset = xml_parse_dtd(external_id, sys_id.as_deref());
        if doc.ext_subset.is_none() {
            if let Some(system_id) = int_subset.system_id.as_deref() {
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

    doc.ids.take();
    doc.refs.take();
    ret = xml_validate_dtd_final(ctxt, doc);
    if xml_validate_root(ctxt, doc) == 0 {
        return 0;
    }

    let root = doc.get_root_element();
    ret &= xml_validate_element(ctxt, doc, root.map(|root| root.into()));
    ret &= xml_validate_document_final(ctxt, doc);
    ret
}

/// Try to validate the subtree under an element
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateElement")]
#[cfg(feature = "libxml_valid")]
pub unsafe fn xml_validate_element(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    root: Option<XmlGenericNodePtr>,
) -> i32 {
    let mut ret: i32 = 1;

    let Some(root) = root else {
        return 0;
    };

    // if doc.is_null() {
    //     return 0;
    // }
    if doc.int_subset.is_none() && doc.ext_subset.is_none() {
        return 0;
    };

    let mut elem = root;
    loop {
        ret &= xml_validate_one_element(ctxt, doc, Some(elem));

        if let Some(node) = XmlNodePtr::try_from(elem)
            .ok()
            .filter(|elem| elem.element_type() == XmlElementType::XmlElementNode)
        {
            let mut attr = node.properties;
            while let Some(now) = attr {
                let value = now
                    .children
                    .and_then(|c| c.get_string(Some(doc), 0))
                    .map(|c| CString::new(c).unwrap());
                ret &= xml_validate_one_attribute(
                    ctxt,
                    doc,
                    node,
                    Some(now),
                    value
                        .as_ref()
                        .map_or(null_mut(), |c| c.as_ptr() as *const u8),
                );
                attr = now.next;
            }

            let mut ns = node.ns_def;
            while let Some(now) = ns {
                if let Some(elem_ns) = node.ns {
                    ret &= xml_validate_one_namespace(
                        ctxt,
                        doc,
                        node,
                        elem_ns.prefix().as_deref(),
                        now,
                        now.href,
                    );
                } else {
                    ret &= xml_validate_one_namespace(ctxt, doc, node, None, now, now.href);
                }
                ns = now.next;
            }

            if let Some(children) = elem.children() {
                elem = XmlGenericNodePtr::from_raw(children.as_ptr()).unwrap();
                continue;
            }
        }

        loop {
            if elem == root {
                // goto done;
                return ret;
            }
            if elem.next().is_some() {
                break;
            }
            elem = elem
                .parent()
                .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()))
                .unwrap();
        }
        elem = elem
            .next()
            .and_then(|n| XmlGenericNodePtr::from_raw(n.as_ptr()))
            .unwrap();
    }

    // done:
    // return ret;
}

/// Finds a declaration associated to an element in the document.
///
/// returns the pointer to the declaration or null_mut() if not found.
#[doc(alias = "xmlValidGetElemDecl")]
unsafe fn xml_valid_get_elem_decl(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    elem: XmlNodePtr,
    extsubset: *mut i32,
) -> Option<XmlElementPtr> {
    // if elem.is_null() {
    //     return None;
    // }
    if ctxt.is_null() || elem.name.is_null() {
        return None;
    }
    if !extsubset.is_null() {
        *extsubset = 0;
    }

    // Fetch the declaration for the qualified name
    let prefix = elem.ns.as_deref().and_then(|ns| ns.prefix());
    let mut elem_decl = None;
    if let Some(prefix) = prefix {
        elem_decl = xml_get_dtd_qelement_desc(doc.int_subset, &elem.name().unwrap(), Some(&prefix));
        if elem_decl.is_none() && doc.ext_subset.is_some() {
            elem_decl =
                xml_get_dtd_qelement_desc(doc.ext_subset, &elem.name().unwrap(), Some(&prefix));
            if elem_decl.is_some() && !extsubset.is_null() {
                *extsubset = 1;
            }
        }
    }

    // Fetch the declaration for the non qualified name
    // This is "non-strict" validation should be done on the
    // full QName but in that case being flexible makes sense.
    if elem_decl.is_none() {
        elem_decl = xml_get_dtd_element_desc(doc.int_subset, elem.name);
        if elem_decl.is_none() && doc.ext_subset.is_some() {
            elem_decl = xml_get_dtd_element_desc(doc.ext_subset, elem.name);
            if elem_decl.is_some() && !extsubset.is_null() {
                *extsubset = 1;
            }
        }
    }
    if elem_decl.is_none() {
        let name = elem.name().unwrap();
        xml_err_valid_node(
            ctxt,
            Some(elem.into()),
            XmlParserErrors::XmlDTDUnknownElem,
            format!("No declaration for element {name}\n").as_str(),
            Some(&name),
            None,
            None,
        );
    }
    elem_decl
}

unsafe fn node_vpush(ctxt: XmlValidCtxtPtr, value: *mut XmlNode) -> i32 {
    (*ctxt).node_tab.push(value);
    (*ctxt).node = value;
    (*ctxt).node_tab.len() as i32 - 1
}

unsafe fn node_vpop(ctxt: XmlValidCtxtPtr) -> *mut XmlNode {
    let Some(res) = (*ctxt).node_tab.pop() else {
        return null_mut();
    };
    (*ctxt).node = (*ctxt).node_tab.last().cloned().unwrap_or(null_mut());
    res
}

/// Check that an element follows #CDATA
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateCdataElement")]
unsafe fn xml_validate_one_cdata_element(
    ctxt: XmlValidCtxtPtr,
    _doc: XmlDocPtr,
    elem: XmlNodePtr,
) -> i32 {
    let mut ret: i32 = 1;

    if ctxt.is_null()
        // || doc.is_null()
        // || elem.is_null()
        || !matches!(elem.element_type(), XmlElementType::XmlElementNode)
    {
        return 0;
    }

    let child = elem.children();

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
        // Switch to next element
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
    (*ctxt).node_tab.clear();
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
unsafe fn xml_snprintf_elements(buf: *mut c_char, size: i32, node: *mut XmlNode, glob: i32) {
    let mut cur: *mut XmlNode;
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
                if let Some(prefix) = (*cur).ns.as_deref().and_then(|ns| ns.prefix()) {
                    if size - len < prefix.len() as i32 + 10 {
                        if size - len > 4 && *buf.add(len as usize - 1) != b'.' as i8 {
                            strcat(buf, c" ...".as_ptr() as _);
                        }
                        return;
                    }
                    strncat(buf, prefix.as_ptr() as *const i8, prefix.len());
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
unsafe fn xmlValidateElementType(ctxt: XmlValidCtxtPtr) -> i32 {
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

    // We arrive here when more states need to be examined
    //  cont:
    'cont: loop {
        // We just recovered from a rollback generated by a possible
        // epsilon transition, go directly to the analysis phase
        if (*(*ctxt).vstate).state == ROLLBACK_PARENT as u8 {
            DEBUG_VALID_MSG!(c"restored parent branch".as_ptr());
            ret = 1;
            // goto analyze;
        } else {
            // we may have to save a backup state here. This is the equivalent
            // of handling epsilon transition in NFAs.
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
                    XmlNodePtr::from_raw((*(*ctxt).vstate).node).unwrap(),
                    (*(*ctxt).vstate).depth,
                    (*(*ctxt).vstate).occurs,
                    ROLLBACK_PARENT as _,
                ) < 0
                {
                    return 0;
                }
            }

            // Check first if the content matches
            match (*(*(*ctxt).vstate).cont).typ {
                XmlElementContentType::XmlElementContentPCDATA => {
                    if (*(*ctxt).vstate).node.is_null() {
                        DEBUG_VALID_MSG!(c"pcdata failed no node".as_ptr());
                        ret = 0;
                        // break;
                    } else {
                        if matches!((*(*(*ctxt).vstate).node).typ, XmlElementType::XmlTextNode) {
                            DEBUG_VALID_MSG!(c"pcdata found, skip to next".as_ptr());
                            // go to next element in the content model
                            // skipping ignorable elems
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
                            // go to next element in the content model
                            // skipping ignorable elems
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
                    // Small optimization.
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

                    // save the second branch 'or' branch
                    DEBUG_VALID_MSG!(c"saving 'or' branch".as_ptr());
                    if vstate_vpush(
                        ctxt,
                        (*(*(*ctxt).vstate).cont).c2,
                        XmlNodePtr::from_raw((*(*ctxt).vstate).node).unwrap(),
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
                    // Small optimization.
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

            // At this point handle going up in the tree
            if ret == -1 {
                DEBUG_VALID_MSG!(c"error found returning".as_ptr());
                return ret;
            }
        }
        // analyze:
        while !(*(*ctxt).vstate).cont.is_null() {
            // First do the analysis depending on the occurrence model at this level.
            if ret == 0 {
                let cur: *mut XmlNode;
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
                                // (*(*ctxt).vstate).occurs |= 1 << (*(*ctxt).vstate).depth;
                                // goto cont;
                                continue 'cont;
                            }
                        }
                    }
                }
            }
            (*(*ctxt).vstate).state = 0;

            // Then act accordingly at the parent level
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
            let cur: *mut XmlNode;

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
            let cur: *mut XmlNode;

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
unsafe fn xml_validate_element_content(
    ctxt: XmlValidCtxtPtr,
    child: *mut XmlNode,
    elem_decl: XmlElementPtr,
    warn: i32,
    parent: *mut XmlNode,
) -> i32 {
    let mut ret: i32 = 1;
    #[cfg(not(feature = "libxml_regexp"))]
    let mut repl: *mut XmlNode = null_mut();
    #[cfg(not(feature = "libxml_regexp"))]
    let mut last: *mut XmlNode = null_mut();
    #[cfg(not(feature = "libxml_regexp"))]
    let mut tmp: *mut XmlNode;
    let mut cur: *mut XmlNode;

    if parent.is_null() || ctxt.is_null() {
        return -1;
    }
    let cont: XmlElementContentPtr = elem_decl.content;
    let name = elem_decl
        .name
        .as_ref()
        .map(|n| CString::new(n.as_str()).unwrap());

    #[cfg(feature = "libxml_regexp")]
    {
        // Build the regexp associated to the content model
        if elem_decl.cont_model.is_null() {
            ret = xml_valid_build_content_model(ctxt, elem_decl);
        }
        if elem_decl.cont_model.is_null() {
            return -1;
        } else {
            if xml_regexp_is_determinist(elem_decl.cont_model) == 0 {
                return -1;
            }
            (*ctxt).node_tab.clear();
            let exec: XmlRegExecCtxtPtr =
                xml_reg_new_exec_ctxt(elem_decl.cont_model, None, null_mut());
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
                                // TODO
                                ret = 0;
                                break 'fail;
                            }
                            XmlElementType::XmlElementNode => {
                                if let Some(prefix) =
                                    (*cur).ns.map(|ns| ns.prefix).filter(|p| !p.is_null())
                                {
                                    let mut fname: [XmlChar; 50] = [0; 50];

                                    let fullname: *mut XmlChar = xml_build_qname(
                                        (*cur).name,
                                        prefix as _,
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
                        // Switch to next element
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
            // Allocate the stack
            (*ctxt).vstateMax = 8;
            (*ctxt).vstate_tab =
                xml_malloc((*ctxt).vstateMax as usize * size_of_val(&*(*ctxt).vstate_tab.add(0)))
                    as *mut XmlValidState;
            if (*ctxt).vstate_tab.is_null() {
                xml_verr_memory(ctxt as _, c"malloc failed".as_ptr() as _);
                return -1;
            }
            // The first entry in the stack is reserved to the current state
            (*ctxt).nodeMax = 0;
            (*ctxt).nodeNr = 0;
            (*ctxt).nodeTab = null_mut();
            (*ctxt).vstate = (*ctxt).vstate_tab.add(0);
            (*ctxt).vstate_nr = 1;
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
                    elem_decl as *mut XmlNode,
                    XmlParserErrors::XmlDTDContentNotDeterminist,
                    c"Content model of %s is not deterministic: %s\n".as_ptr() as _,
                    name,
                    expr.as_ptr() as _,
                    null_mut(),
                );
            } else if ret == -2 {
                // An entities reference appeared at this level.
                // Build a minimal representation of this node content
                // sufficient to run the validation process on it
                DEBUG_VALID_MSG!(c"Found an entity reference, linearizing".as_ptr());
                cur = child;
                while !cur.is_null() {
                    match (*cur).element_type() {
                        XmlElementType::XmlEntityRefNode => {
                            // Push the current node to be able to roll back
                            // and process within the entity
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
                                // Allocate a new node and minimally fills in
                                // what's required
                                let Some(tmp) = XmlNodePtr::new(XmlNode {
                                    typ: (*cur).typ,
                                    name: (*cur).name,
                                    ns: (*cur).ns,
                                    next: null_mut(),
                                    content: null_mut(),
                                    ..Default::default()
                                }) else {
                                    xml_verr_memory(ctxt as _, c"malloc failed".as_ptr() as _);
                                    xmlFreeNodeList(repl);
                                    ret = -1;
                                    break 'done;
                                };
                                if repl.is_null() {
                                    repl = tmp;
                                    last = tmp;
                                } else {
                                    (*last).next = tmp;
                                    last = tmp;
                                }
                                if matches!((*cur).typ, XmlElementType::XmlCDATASectionNode) {
                                    // E59 spaces in CDATA does not match the nonterminal S
                                    (*tmp).content = xml_strdup(c"CDATA".as_ptr() as _);
                                }
                            }
                        }
                        _ => {}
                    }
                    // Switch to next element
                    cur = (*cur).next;
                    while cur.is_null() {
                        cur = node_vpop(ctxt);
                        if cur.is_null() {
                            break;
                        }
                        cur = (*cur).next;
                    }
                }

                // Relaunch the validation
                (*ctxt).vstate = (*ctxt).vstate_tab.add(0);
                (*ctxt).vstate_nr = 1;
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
                        XmlGenericNodePtr::from_raw(parent),
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
                        XmlGenericNodePtr::from_raw(parent),
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
                    XmlGenericNodePtr::from_raw(parent),
                    XmlParserErrors::XmlDTDContentModel,
                    format!("Element {name} content does not follow the DTD\n").as_str(),
                    Some(&name),
                    None,
                    None,
                );
            } else {
                xml_err_valid_node(
                    ctxt,
                    XmlGenericNodePtr::from_raw(parent),
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
        // Deallocate the copy if done, and free up the validation stack
        while !repl.is_null() {
            tmp = (*repl).next;
            xml_free(repl as _);
            repl = tmp;
        }
        (*ctxt).vstateMax = 0;
        if !(*ctxt).vstate_tab.is_null() {
            xml_free((*ctxt).vstate_tab as _);
            (*ctxt).vstate_tab = null_mut();
        }
    }
    (*ctxt).node_tab.clear();
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
pub unsafe fn xml_validate_one_element(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    elem: Option<XmlGenericNodePtr>,
) -> i32 {
    let mut cont: XmlElementContentPtr;
    let mut child: *mut XmlNode;
    let mut ret: i32 = 1;
    let tmp: i32;
    let mut name: *const XmlChar;
    let mut extsubset: i32 = 0;

    // if doc.is_null() {
    //     return 0;
    // }
    if doc.int_subset.is_none() && doc.ext_subset.is_none() {
        return 0;
    };

    let Some(elem) = elem else {
        return 0;
    };
    match elem.element_type() {
        XmlElementType::XmlAttributeNode => {
            xml_err_valid_node(
                ctxt,
                Some(elem),
                XmlParserErrors::XmlErrInternalError,
                "Attribute element not expected\n",
                None,
                None,
                None,
            );
            return 0;
        }
        XmlElementType::XmlTextNode => {
            let elem = XmlNodePtr::try_from(elem).unwrap();
            if elem.children().is_some() {
                xml_err_valid_node(
                    ctxt,
                    Some(elem.into()),
                    XmlParserErrors::XmlErrInternalError,
                    "Text element has children !\n",
                    None,
                    None,
                    None,
                );
                return 0;
            }
            if elem.ns.is_some() {
                xml_err_valid_node(
                    ctxt,
                    Some(elem.into()),
                    XmlParserErrors::XmlErrInternalError,
                    "Text element has namespace !\n",
                    None,
                    None,
                    None,
                );
                return 0;
            }
            if elem.content.is_null() {
                xml_err_valid_node(
                    ctxt,
                    Some(elem.into()),
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
                Some(elem),
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
                Some(elem),
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
                Some(elem),
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
                Some(elem),
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
                Some(elem),
                XmlParserErrors::XmlErrInternalError,
                "unknown element type\n",
                None,
                None,
                None,
            );
            return 0;
        }
    }

    // At this point, `elem` is just `XmlElementNode`.
    let elem = XmlNodePtr::try_from(elem).unwrap();

    // Fetch the declaration
    let Some(elem_decl) = xml_valid_get_elem_decl(ctxt, doc, elem, addr_of_mut!(extsubset)) else {
        return 0;
    };

    // If vstate_nr is not zero that means continuous validation is
    // activated, do not try to check the content model at that level.
    if (*ctxt).vstate_tab.is_empty() {
        // Check that the element content matches the definition
        match elem_decl.etype {
            XmlElementTypeVal::XmlElementTypeUndefined => {
                let name = elem.name().unwrap();
                xml_err_valid_node(
                    ctxt,
                    Some(elem.into()),
                    XmlParserErrors::XmlDTDUnknownElem,
                    format!("No declaration for element {name}\n").as_str(),
                    Some(&name),
                    None,
                    None,
                );
                return 0;
            }
            XmlElementTypeVal::XmlElementTypeEmpty => {
                if elem.children().is_some() {
                    let name = elem.name().unwrap();
                    xml_err_valid_node(
                        ctxt,
                        Some(elem.into()),
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
                if !elem_decl.content.is_null()
                    && (*elem_decl.content).typ == XmlElementContentType::XmlElementContentPCDATA
                {
                    ret = xml_validate_one_cdata_element(ctxt, doc, elem);
                    if ret == 0 {
                        let name = elem.name().unwrap();
                        xml_err_valid_node(
                            ctxt,
                            Some(elem.into()),
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
                    child = elem.children().map_or(null_mut(), |c| c.as_ptr());
                    // Hum, this start to get messy
                    while !child.is_null() {
                        'child_ok: {
                            if matches!((*child).element_type(), XmlElementType::XmlElementNode) {
                                name = (*child).name;
                                if let Some(prefix) =
                                    (*child).ns.map(|ns| ns.prefix).filter(|p| !p.is_null())
                                {
                                    let mut fname: [XmlChar; 50] = [0; 50];

                                    let fullname: *mut XmlChar = xml_build_qname(
                                        (*child).name,
                                        prefix,
                                        fname.as_mut_ptr() as _,
                                        50,
                                    );
                                    if fullname.is_null() {
                                        return 0;
                                    }
                                    cont = elem_decl.content;
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

                                cont = elem_decl.content;
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
                                    let elem_name = elem.name().unwrap();
                                    xml_err_valid_node(
                                        ctxt,
                                        Some(elem.into()),
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
                if doc.standalone == 1 && extsubset == 1 {
                    // VC: Standalone Document Declaration
                    //     - element types with element content, if white space
                    //       occurs directly within any instance of those types.
                    child = elem.children().map_or(null_mut(), |c| c.as_ptr());
                    while !child.is_null() {
                        if matches!((*child).element_type(), XmlElementType::XmlTextNode) {
                            let mut content: *const XmlChar = (*child).content;

                            while xml_is_blank_char(*content as u32) {
                                content = content.add(1);
                            }
                            if *content == 0 {
                                let name = elem.name().unwrap();
                                xml_err_valid_node(
                                    ctxt,
                                    Some(elem.into()),
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
                child = elem.children().map_or(null_mut(), |c| c.as_ptr());
                // cont = (*elem_decl).content;
                tmp = xml_validate_element_content(ctxt, child, elem_decl, 1, elem.as_ptr());
                if tmp <= 0 {
                    ret = tmp;
                }
            }
        }
    }

    // [ VC: Required Attribute ]
    let mut attr = elem_decl.attributes;
    while let Some(cur_attr) = attr {
        'found: {
            if matches!(cur_attr.def, XmlAttributeDefault::XmlAttributeRequired) {
                let mut qualified: i32 = -1;

                if cur_attr.prefix.is_none() && xml_str_equal(cur_attr.name, c"xmlns".as_ptr() as _)
                {
                    let mut ns = elem.ns_def;
                    while let Some(now) = ns {
                        if now.prefix().is_none() {
                            break 'found;
                        }
                        ns = now.next;
                    }
                } else if cur_attr.prefix.as_deref() == Some("xmlns") {
                    let mut ns = elem.ns_def;
                    while let Some(now) = ns {
                        if cur_attr.name() == now.prefix() {
                            break 'found;
                        }
                        ns = now.next;
                    }
                } else {
                    let mut attrib = elem.properties;
                    while let Some(attr) = attrib {
                        if xml_str_equal(attr.name, cur_attr.name) {
                            if let Some(prefix) = cur_attr.prefix.as_deref() {
                                let name_space = attr.ns.or(elem.ns);

                                // qualified names handling is problematic, having a
                                // different prefix should be possible but DTDs don't
                                // allow to define the URI instead of the prefix :-(
                                if let Some(name_space) = name_space {
                                    if (*name_space).prefix().as_deref() != Some(prefix) {
                                        if qualified < 1 {
                                            qualified = 1;
                                        }
                                    } else {
                                        break 'found;
                                    }
                                } else if qualified < 0 {
                                    qualified = 0;
                                }
                            } else {
                                // We should allow applications to define namespaces
                                // for their application even if the DTD doesn't
                                // carry one, otherwise, basically we would always break.
                                break 'found;
                            }
                        }
                        attrib = attr.next;
                    }
                }
                if qualified == -1 {
                    if cur_attr.prefix.is_none() {
                        let elem_name = elem.name().unwrap();
                        let attr_name = cur_attr.name().unwrap();
                        xml_err_valid_node(
                            ctxt,
                            Some(elem.into()),
                            XmlParserErrors::XmlDTDMissingAttribute,
                            format!("Element {elem_name} does not carry attribute {attr_name}\n")
                                .as_str(),
                            Some(&elem_name),
                            Some(&attr_name),
                            None,
                        );
                        ret = 0;
                    } else {
                        let elem_name = elem.name().unwrap();
                        let prefix = cur_attr.prefix.as_deref().unwrap();
                        let attr_name = cur_attr.name().unwrap();
                        xml_err_valid_node(
                            ctxt,
                            Some(elem.into()),
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
                        elem.as_ptr(),
                        XmlParserErrors::XmlDTDNoPrefix,
                        "Element {} required attribute {}:{} has no prefix\n",
                        elem.name().unwrap().into_owned(),
                        cur_attr.prefix.as_deref().unwrap(),
                        cur_attr.name().unwrap().into_owned()
                    );
                } else if qualified == 1 {
                    xml_err_valid_warning!(
                        ctxt,
                        elem.as_ptr(),
                        XmlParserErrors::XmlDTDDifferentPrefix,
                        "Element {} required attribute {}:{} has different prefix\n",
                        elem.name().unwrap().into_owned(),
                        cur_attr.prefix.as_deref().unwrap(),
                        cur_attr.name().unwrap().into_owned()
                    );
                }
            } else if matches!(cur_attr.def, XmlAttributeDefault::XmlAttributeFixed) {
                // Special tests checking #FIXED namespace declarations
                // have the right value since this is not done as an
                // attribute checking
                if cur_attr.prefix.is_none() && xml_str_equal(cur_attr.name, c"xmlns".as_ptr() as _)
                {
                    let mut ns = elem.ns_def;
                    while let Some(now) = ns {
                        if now.prefix().is_none() {
                            if !xml_str_equal(cur_attr.default_value, now.href) {
                                let elem_name = elem.name().unwrap();
                                xml_err_valid_node(
                                    ctxt,
                                    Some(elem.into()),
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
                        ns = now.next;
                    }
                } else if cur_attr.prefix.as_deref() == Some("xmlns") {
                    let mut ns = elem.ns_def;
                    while let Some(now) = ns {
                        if cur_attr.name() == now.prefix() {
                            if !xml_str_equal(cur_attr.default_value, now.href) {
                                let elem_name = elem.name().unwrap();
                                let prefix = now.prefix().unwrap();
                                xml_err_valid_node(
                                    ctxt,
                                    Some(elem.into()),
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
                        ns = now.next;
                    }
                }
            }
        }
        // found:
        attr = cur_attr.nexth;
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
pub unsafe fn xml_validate_one_attribute(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    elem: XmlNodePtr,
    attr: Option<XmlAttrPtr>,
    value: *const XmlChar,
) -> i32 {
    let mut ret: i32 = 1;

    // if doc.is_null() {
    //     return 0;
    // }
    if doc.int_subset.is_none() && doc.ext_subset.is_none() {
        return 0;
    };
    // if elem.is_null() {
    //     return 0;
    // }
    if elem.name.is_null() {
        return 0;
    }
    let Some(mut attr) = attr.filter(|a| !a.name.is_null()) else {
        return 0;
    };

    let mut attr_decl = None;
    if let Some(prefix) = elem.ns.map(|ns| ns.prefix).filter(|p| !p.is_null()) {
        let mut fname: [XmlChar; 50] = [0; 50];

        let fullname: *mut XmlChar =
            xml_build_qname(elem.name, prefix as _, fname.as_mut_ptr(), 50);
        if fullname.is_null() {
            return 0;
        }
        if let Some(attr_ns) = attr.ns {
            attr_decl = doc.int_subset.and_then(|dtd| {
                dtd.get_qattr_desc(
                    CStr::from_ptr(fullname as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    attr.name().as_deref().unwrap(),
                    attr_ns.prefix().as_deref(),
                )
            });
            if attr_decl.is_none() && doc.ext_subset.is_some() {
                attr_decl = doc.ext_subset.and_then(|dtd| {
                    dtd.get_qattr_desc(
                        CStr::from_ptr(fullname as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                        attr.name().as_deref().unwrap(),
                        attr_ns.prefix().as_deref(),
                    )
                });
            }
        } else {
            attr_decl = doc.int_subset.and_then(|dtd| {
                dtd.get_attr_desc(
                    CStr::from_ptr(fullname as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    attr.name().as_deref().unwrap(),
                )
            });
            if attr_decl.is_none() && doc.ext_subset.is_some() {
                attr_decl = doc.ext_subset.and_then(|dtd| {
                    dtd.get_attr_desc(
                        CStr::from_ptr(fullname as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                        attr.name().as_deref().unwrap(),
                    )
                });
            }
        }
        if fullname != fname.as_ptr() as _ && fullname != elem.name as _ {
            xml_free(fullname as _);
        }
    }
    if attr_decl.is_none() {
        if let Some(attr_ns) = attr.ns {
            attr_decl = doc.int_subset.and_then(|dtd| {
                dtd.get_qattr_desc(
                    elem.name().unwrap().as_ref(),
                    attr.name().as_deref().unwrap(),
                    attr_ns.prefix().as_deref(),
                )
            });
            if attr_decl.is_none() && doc.ext_subset.is_some() {
                attr_decl = doc.ext_subset.and_then(|dtd| {
                    dtd.get_qattr_desc(
                        elem.name().unwrap().as_ref(),
                        attr.name().as_deref().unwrap(),
                        attr_ns.prefix().as_deref(),
                    )
                });
            }
        } else {
            attr_decl = doc.int_subset.and_then(|dtd| {
                dtd.get_attr_desc(
                    elem.name().as_deref().unwrap(),
                    attr.name().as_deref().unwrap(),
                )
            });
            if attr_decl.is_none() && doc.ext_subset.is_some() {
                attr_decl = doc.ext_subset.and_then(|dtd| {
                    dtd.get_attr_desc(
                        elem.name().as_deref().unwrap(),
                        attr.name().as_deref().unwrap(),
                    )
                });
            }
        }
    }

    // Validity Constraint: Attribute Value Type
    let Some(attr_decl) = attr_decl else {
        let attr_name = attr.name().unwrap();
        let elem_name = elem.name().unwrap();
        xml_err_valid_node(
            ctxt,
            Some(elem.into()),
            XmlParserErrors::XmlDTDUnknownAttribute,
            format!("No declaration for attribute {attr_name} of element {elem_name}\n").as_str(),
            Some(&attr_name),
            Some(&elem_name),
            None,
        );
        return 0;
    };
    attr.atype = Some(attr_decl.atype);

    let val: i32 = xml_validate_attribute_value_internal(Some(doc), attr_decl.atype, value);
    if val == 0 {
        let attr_name = attr.name().unwrap();
        let elem_name = elem.name().unwrap();
        xml_err_valid_node(
            ctxt,
            Some(elem.into()),
            XmlParserErrors::XmlDTDAttributeValue,
            format!("Syntax of value for attribute {attr_name} of {elem_name} is not valid\n")
                .as_str(),
            Some(&attr_name),
            Some(&elem_name),
            None,
        );
        ret = 0;
    }

    // Validity constraint: Fixed Attribute Default
    if matches!(attr_decl.def, XmlAttributeDefault::XmlAttributeFixed)
        && !xml_str_equal(value, attr_decl.default_value)
    {
        let attr_name = attr.name().unwrap();
        let elem_name = elem.name().unwrap();
        let def_value = CStr::from_ptr(attr_decl.default_value as *const i8).to_string_lossy();
        xml_err_valid_node(
            ctxt,
            Some(elem.into()),
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

    // Validity Constraint: ID uniqueness
    if matches!(attr_decl.atype, XmlAttributeType::XmlAttributeID)
        && xml_add_id(
            ctxt,
            doc,
            CStr::from_ptr(value as *const i8)
                .to_string_lossy()
                .as_ref(),
            attr,
        )
        .is_none()
    {
        ret = 0;
    }

    if matches!(
        attr_decl.atype,
        XmlAttributeType::XmlAttributeIDREF | XmlAttributeType::XmlAttributeIDREFS
    ) && xml_add_ref(
        ctxt,
        doc,
        CStr::from_ptr(value as *const i8)
            .to_string_lossy()
            .as_ref(),
        attr,
    )
    .is_none()
    {
        ret = 0;
    }

    // Validity Constraint: Notation Attributes
    if matches!(attr_decl.atype, XmlAttributeType::XmlAttributeNotation) {
        let mut tree = attr_decl.tree.as_deref();
        let value = CStr::from_ptr(value as *const i8).to_string_lossy();

        // First check that the given NOTATION was declared
        let nota = xml_get_dtd_notation_desc(doc.int_subset.as_deref(), &value)
            .or_else(|| xml_get_dtd_notation_desc(doc.ext_subset.as_deref(), &value));

        if nota.is_none() {
            let attr_name = attr.name().unwrap();
            let elem_name = elem.name().unwrap();
            xml_err_valid_node(
                ctxt,
                Some(elem.into()),
                XmlParserErrors::XmlDTDUnknownNotation,
                format!("Value \"{value}\" for attribute {attr_name} of {elem_name} is not a declared Notation\n").as_str(),
                Some(&value),
                Some(&attr_name),
                Some(&elem_name),
            );
            ret = 0;
        }

        // Second, verify that it's among the list
        while let Some(now) = tree {
            if now.name == value {
                break;
            }
            tree = now.next.as_deref();
        }
        if tree.is_none() {
            let attr_name = attr.name().unwrap();
            let elem_name = elem.name().unwrap();
            xml_err_valid_node(
                ctxt,
                Some(elem.into()),
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
    if matches!(attr_decl.atype, XmlAttributeType::XmlAttributeEnumeration) {
        let mut tree = attr_decl.tree.as_deref();
        while let Some(now) = tree {
            if now.name == CStr::from_ptr(value as *const i8).to_string_lossy() {
                break;
            }
            tree = now.next.as_deref();
        }
        if tree.is_none() {
            let value = CStr::from_ptr(value as *const i8).to_string_lossy();
            let attr_name = attr.name().unwrap();
            let elem_name = elem.name().unwrap();
            xml_err_valid_node(
                ctxt,
                Some(elem.into()),
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
    if matches!(attr_decl.def, XmlAttributeDefault::XmlAttributeFixed)
        && !xml_str_equal(attr_decl.default_value, value)
    {
        let attr_name = attr.name().unwrap();
        let elem_name = elem.name().unwrap();
        let def_value = CStr::from_ptr(attr_decl.default_value as *const i8).to_string_lossy();
        xml_err_valid_node(
            ctxt,
            Some(elem.into()),
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
        attr.name,
        attr_decl.atype,
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
pub unsafe fn xml_validate_one_namespace(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    elem: XmlNodePtr,
    prefix: Option<&str>,
    ns: XmlNsPtr,
    value: *const XmlChar,
) -> i32 {
    // let elemDecl: xmlElementPtr;

    let mut ret: i32 = 1;

    // if doc.is_null() {
    //     return 0;
    // }
    if doc.int_subset.is_none() && doc.ext_subset.is_none() {
        return 0;
    };
    // if elem.is_null() {
    //     return 0;
    // }
    if elem.name.is_null() {
        return 0;
    }
    if ns.href.is_null() {
        return 0;
    }

    let mut attr_decl = None;
    if let Some(prefix) = prefix {
        let mut fname: [XmlChar; 50] = [0; 50];
        let prefix = CString::new(prefix).unwrap();

        let fullname: *mut XmlChar = xml_build_qname(
            elem.name,
            prefix.as_ptr() as *const u8,
            fname.as_mut_ptr(),
            50,
        );
        if fullname.is_null() {
            xml_verr_memory(ctxt, Some("Validating namespace"));
            return 0;
        }
        if let Some(prefix) = (*ns).prefix() {
            attr_decl = doc.int_subset.and_then(|dtd| {
                dtd.get_qattr_desc(
                    CStr::from_ptr(fullname as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    &prefix,
                    Some("xmlns"),
                )
            });
            if attr_decl.is_none() && doc.ext_subset.is_some() {
                attr_decl = doc.ext_subset.and_then(|dtd| {
                    dtd.get_qattr_desc(
                        CStr::from_ptr(fullname as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                        &prefix,
                        Some("xmlns"),
                    )
                });
            }
        } else {
            attr_decl = doc.int_subset.and_then(|dtd| {
                dtd.get_attr_desc(
                    CStr::from_ptr(fullname as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    "xmlns",
                )
            });
            if attr_decl.is_none() && doc.ext_subset.is_some() {
                attr_decl = doc.ext_subset.and_then(|dtd| {
                    dtd.get_attr_desc(
                        CStr::from_ptr(fullname as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                        "xmlns",
                    )
                });
            }
        }
        if fullname != fname.as_ptr() as _ && fullname != elem.name as _ {
            xml_free(fullname as _);
        }
    }
    if attr_decl.is_none() {
        if let Some(prefix) = (*ns).prefix() {
            attr_decl = doc.int_subset.and_then(|dtd| {
                dtd.get_qattr_desc(elem.name().unwrap().as_ref(), &prefix, Some("xmlns"))
            });
            if attr_decl.is_none() && doc.ext_subset.is_some() {
                attr_decl = doc.ext_subset.and_then(|dtd| {
                    dtd.get_qattr_desc(elem.name().unwrap().as_ref(), &prefix, Some("xmlns"))
                });
            }
        } else {
            attr_decl = doc
                .int_subset
                .and_then(|dtd| dtd.get_attr_desc(elem.name().as_deref().unwrap(), "xmlns"));
            if attr_decl.is_none() && doc.ext_subset.is_some() {
                attr_decl = doc
                    .ext_subset
                    .and_then(|dtd| dtd.get_attr_desc(elem.name().as_deref().unwrap(), "xmlns"));
            }
        }
    }

    // Validity Constraint: Attribute Value Type
    let Some(attr_decl) = attr_decl else {
        if let Some(prefix) = (*ns).prefix() {
            let elem_name = elem.name().unwrap();
            xml_err_valid_node(
                ctxt,
                Some(elem.into()),
                XmlParserErrors::XmlDTDUnknownAttribute,
                format!("No declaration for attribute xmlns:{prefix} of element {elem_name}\n")
                    .as_str(),
                Some(&prefix),
                Some(&elem_name),
                None,
            );
        } else {
            let elem_name = elem.name().unwrap();
            xml_err_valid_node(
                ctxt,
                Some(elem.into()),
                XmlParserErrors::XmlDTDUnknownAttribute,
                format!("No declaration for attribute xmlns of element {elem_name}\n").as_str(),
                Some(&elem_name),
                None,
                None,
            );
        }
        return 0;
    };

    let val: i32 = xml_validate_attribute_value_internal(Some(doc), attr_decl.atype, value);
    if val == 0 {
        if let Some(prefix) = (*ns).prefix() {
            let elem_name = elem.name().unwrap();
            xml_err_valid_node(
                ctxt,
                Some(elem.into()),
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
            let elem_name = elem.name().unwrap();
            xml_err_valid_node(
                ctxt,
                Some(elem.into()),
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
    if matches!(attr_decl.def, XmlAttributeDefault::XmlAttributeFixed)
        && !xml_str_equal(value, attr_decl.default_value)
    {
        if let Some(prefix) = (*ns).prefix() {
            let elem_name = elem.name().unwrap();
            let def_value = CStr::from_ptr(attr_decl.default_value as *const i8).to_string_lossy();
            xml_err_valid_node(
                ctxt,
                Some(elem.into()),
                XmlParserErrors::XmlDTDAttributeDefault,
                format!("Value for attribute xmlns:{prefix} of {elem_name} is different from default \"{def_value}\"\n").as_str(),
                Some(&prefix),
                Some(&elem_name),
                Some(&def_value),
            );
        } else {
            let elem_name = elem.name().unwrap();
            let def_value = CStr::from_ptr(attr_decl.default_value as *const i8).to_string_lossy();
            xml_err_valid_node(
                ctxt,
                Some(elem.into()),
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

    // Casting ns to xmlAttrPtr is wrong. We'd need separate functions
    // xmlAddID and xmlAddRef for namespace declarations, but it makes
    // no practical sense to use ID types anyway.
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
    if matches!(attr_decl.atype, XmlAttributeType::XmlAttributeNotation) {
        let mut tree = attr_decl.tree.as_deref();
        let value = CStr::from_ptr(value as *const i8).to_string_lossy();

        // First check that the given NOTATION was declared
        let nota = xml_get_dtd_notation_desc(doc.int_subset.as_deref(), &value)
            .or_else(|| xml_get_dtd_notation_desc(doc.ext_subset.as_deref(), &value));

        if nota.is_none() {
            if let Some(prefix) = (*ns).prefix() {
                let elem_name = elem.name().unwrap();
                xml_err_valid_node(
                    ctxt,
                    Some(elem.into()),
                    XmlParserErrors::XmlDTDUnknownNotation,
                    format!("Value \"{value}\" for attribute xmlns:{prefix} of {elem_name} is not a declared Notation\n").as_str(),
                    Some(&value),
                    Some(&prefix),
                    Some(&elem_name),
                );
            } else {
                let elem_name = elem.name().unwrap();
                xml_err_valid_node(
                    ctxt,
                    Some(elem.into()),
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
        while let Some(now) = tree {
            if now.name == value {
                break;
            }
            tree = now.next.as_deref();
        }
        if tree.is_none() {
            let elem_name = elem.name().unwrap();
            if let Some(prefix) = (*ns).prefix() {
                xml_err_valid_node(
                    ctxt,
                    Some(elem.into()),
                    XmlParserErrors::XmlDTDNotationValue,
                    format!("Value \"{value}\" for attribute xmlns:{prefix} of {elem_name} is not among the enumerated notations\n").as_str(),
                    Some(&value),
                    Some(&prefix),
                    Some(&elem_name),
                );
            } else {
                xml_err_valid_node(
                    ctxt,
                    Some(elem.into()),
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
    if matches!(attr_decl.atype, XmlAttributeType::XmlAttributeEnumeration) {
        let mut tree = attr_decl.tree.as_deref();
        while let Some(now) = tree {
            if now.name == CStr::from_ptr(value as *const i8).to_string_lossy() {
                break;
            }
            tree = now.next.as_deref();
        }
        if tree.is_none() {
            if let Some(prefix) = (*ns).prefix() {
                let value = CStr::from_ptr(value as *const i8).to_string_lossy();
                let elem_name = elem.name().unwrap();
                xml_err_valid_node(
                    ctxt,
                    Some(elem.into()),
                    XmlParserErrors::XmlDTDAttributeValue,
                    format!("Value \"{value}\" for attribute xmlns:{prefix} of {elem_name} is not among the enumerated set\n").as_str(),
                    Some(&value),
                    Some(&prefix),
                    Some(&elem_name),
                );
            } else {
                let value = CStr::from_ptr(value as *const i8).to_string_lossy();
                let elem_name = elem.name().unwrap();
                xml_err_valid_node(
                    ctxt,
                    Some(elem.into()),
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

    // Fixed Attribute Default
    if matches!(attr_decl.def, XmlAttributeDefault::XmlAttributeFixed)
        && !xml_str_equal(attr_decl.default_value, value)
    {
        if let Some(prefix) = (*ns).prefix() {
            let elem_name = elem.name().unwrap();
            let def_value = CStr::from_ptr(attr_decl.default_value as *const i8).to_string_lossy();
            xml_err_valid_node(
                ctxt,
                Some(elem.into()),
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
            let elem_name = elem.name().unwrap();
            let def_value = CStr::from_ptr(attr_decl.default_value as *const i8).to_string_lossy();
            xml_err_valid_node(
                ctxt,
                Some(elem.into()),
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
    if let Some(prefix) = (*ns).prefix() {
        let prefix = CString::new(prefix.as_ref()).unwrap();
        ret &= xml_validate_attribute_value2(
            ctxt,
            doc,
            prefix.as_ptr() as *const u8,
            attr_decl.atype,
            &value,
        );
    } else {
        ret &= xml_validate_attribute_value2(
            ctxt,
            doc,
            c"xmlns".as_ptr() as _,
            attr_decl.atype,
            &value,
        );
    }

    ret
}

#[doc(alias = "xmlValidateRef")]
unsafe fn xml_validate_ref(refe: &XmlRef, ctxt: XmlValidCtxtPtr, name: *const XmlChar) {
    if refe.attr.is_none() && refe.name.is_none() {
        return;
    }
    if let Some(attr) = refe.attr {
        if matches!(attr.atype, Some(XmlAttributeType::XmlAttributeIDREF)) {
            if xml_get_id((*ctxt).doc.unwrap(), name).is_none() {
                let attr_name = attr.name().unwrap();
                let name = CStr::from_ptr(name as *const i8).to_string_lossy();
                xml_err_valid_node(
                    ctxt,
                    attr.parent
                        .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr())),
                    XmlParserErrors::XmlDTDUnknownID,
                    format!("IDREF attribute {attr_name} references an unknown ID \"{name}\"\n")
                        .as_str(),
                    Some(&attr_name),
                    Some(&name),
                    None,
                );
                (*ctxt).valid = 0;
            }
        } else if matches!(attr.atype, Some(XmlAttributeType::XmlAttributeIDREFS)) {
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
                if xml_get_id((*ctxt).doc.unwrap(), str).is_none() {
                    let attr_name = attr.name().unwrap();
                    let str = CStr::from_ptr(str as *const i8).to_string_lossy();
                    xml_err_valid_node(
                        ctxt,
                        attr.parent
                            .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr())),
                        XmlParserErrors::XmlDTDUnknownID,
                        format!(
                            "IDREFS attribute {attr_name} references an unknown ID \"{str}\"\n"
                        )
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
    } else {
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
            if xml_get_id((*ctxt).doc.unwrap(), str).is_none() {
                xml_err_valid_node_nr!(
                    ctxt,
                    null_mut(),
                    XmlParserErrors::XmlDTDUnknownID,
                    "attribute {} line {} references an unknown ID \"{}\"\n",
                    refe.name.as_deref().unwrap(),
                    refe.lineno,
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
    }
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
pub unsafe fn xml_validate_document_final(ctxt: XmlValidCtxtPtr, doc: XmlDocPtr) -> i32 {
    if ctxt.is_null() {
        return 0;
    }
    // if doc.is_null() {
    //     xml_err_valid!(
    //         ctxt,
    //         XmlParserErrors::XmlDTDNoDoc,
    //         "xmlValidateDocumentFinal: doc == NULL\n"
    //     );
    //     return 0;
    // }

    // trick to get correct line id report
    let save: u32 = (*ctxt).flags;
    (*ctxt).flags &= !XML_VCTXT_USE_PCTXT as u32;

    // Check all the NOTATION/NOTATIONS attributes
    // Check all the ENTITY/ENTITIES attributes definition for validity
    // Check all the IDREF/IDREFS attributes definition for validity
    (*ctxt).doc = Some(doc);
    (*ctxt).valid = 1;
    if let Some(table) = doc.refs.as_ref() {
        for (name, ref_list) in table.iter() {
            let name = CString::new(name.as_str()).unwrap();

            ref_list.walk(|data| {
                xml_validate_ref(data.as_ref(), ctxt, name.as_ptr() as *const u8);
                true
            });
        }
    }

    (*ctxt).flags = save;
    (*ctxt).valid
}

/// Validate that the given name match a notation declaration.
/// - [ VC: Notation Declared ]
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateNotationUse")]
#[cfg(any(feature = "libxml_valid", feature = "schema"))]
pub unsafe fn xml_validate_notation_use(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    notation_name: &str,
) -> i32 {
    // if doc.is_null() {
    //     return -1;
    // }
    if doc.int_subset.is_none() {
        return -1;
    }

    let nota_decl = xml_get_dtd_notation_desc(doc.int_subset.as_deref(), notation_name)
        .or_else(|| xml_get_dtd_notation_desc(doc.ext_subset.as_deref(), notation_name));

    if nota_decl.is_none() && !ctxt.is_null() {
        xml_err_valid_node(
            ctxt,
            Some(doc.into()),
            XmlParserErrors::XmlDTDUnknownNotation,
            format!("NOTATION {notation_name} is not declared\n").as_str(),
            Some(notation_name),
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
pub unsafe fn xml_is_mixed_element(doc: XmlDocPtr, name: *const XmlChar) -> i32 {
    // if doc.is_null() {
    //     return -1;
    // }
    if doc.int_subset.is_none() {
        return -1;
    }

    let mut elem_decl = xml_get_dtd_element_desc(doc.int_subset, name);
    if elem_decl.is_none() && doc.ext_subset.is_some() {
        elem_decl = xml_get_dtd_element_desc(doc.ext_subset, name);
    }
    let Some(elem_decl) = elem_decl else {
        return -1;
    };
    match elem_decl.etype {
        XmlElementTypeVal::XmlElementTypeUndefined => {
            -1
        }
        XmlElementTypeVal::XmlElementTypeElement => {
            0
        }
        XmlElementTypeVal::XmlElementTypeEmpty
        // return 1 for EMPTY since we want VC error to pop up
        // on <empty>     </empty> for example
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
pub unsafe fn xml_get_dtd_notation_desc<'a>(
    dtd: Option<&'a XmlDtd>,
    name: &str,
) -> Option<&'a XmlNotation> {
    dtd.and_then(|dtd| dtd.notations.as_deref())
        .and_then(|notations| notations.lookup(name))
}

/// Search the DTD for the description of this element
///
/// returns the xmlElementPtr if found or null_mut()
#[doc(alias = "xmlGetDtdQElementDesc")]
pub unsafe fn xml_get_dtd_qelement_desc(
    dtd: Option<XmlDtdPtr>,
    name: &str,
    prefix: Option<&str>,
) -> Option<XmlElementPtr> {
    dtd?.elements.as_ref()?.lookup2(name, prefix).cloned()
}

/// Search the DTD for the description of this element
///
/// returns the xmlElementPtr if found or null_mut()
#[doc(alias = "xmlGetDtdElementDesc")]
pub unsafe fn xml_get_dtd_element_desc(
    dtd: Option<XmlDtdPtr>,
    mut name: *const XmlChar,
) -> Option<XmlElementPtr> {
    let mut prefix: *mut XmlChar = null_mut();

    if name.is_null() {
        return None;
    }
    let dtd = dtd?;
    let table = dtd.elements.as_ref()?;

    let uqname: *mut XmlChar = xml_split_qname2(name, addr_of_mut!(prefix));
    if !uqname.is_null() {
        name = uqname;
    }
    let cur = table
        .lookup2(
            CStr::from_ptr(name as *const i8).to_string_lossy().as_ref(),
            (!prefix.is_null())
                .then(|| CStr::from_ptr(prefix as *const i8).to_string_lossy())
                .as_deref(),
        )
        .cloned();
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
pub unsafe fn xml_valid_get_potential_children(
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
pub unsafe fn xml_valid_get_valid_elements(
    prev: *mut XmlNode,
    next: *mut XmlNode,
    names: *mut *const XmlChar,
    max: i32,
) -> i32 {
    use crate::tree::NodePtr;

    let mut vctxt = XmlValidCtxt::default();
    let mut nb_valid_elements: i32;
    let mut elements: [*const XmlChar; 256] = [null(); 256];
    let mut nb_elements: i32 = 0;

    if prev.is_null() && next.is_null() {
        return -1;
    }

    if names.is_null() {
        return -1;
    }
    if max <= 0 {
        return -1;
    }

    std::ptr::write(&mut vctxt, XmlValidCtxt::default());
    vctxt.error = Some(xml_no_validity_err); /* this suppresses err/warn output */

    nb_valid_elements = 0;
    let ref_node: *mut XmlNode = if !prev.is_null() { prev } else { next };
    let parent: *mut XmlNode = (*ref_node).parent().map_or(null_mut(), |p| p.as_ptr());

    // Retrieves the parent element declaration
    let mut element_desc =
        xml_get_dtd_element_desc((*parent).doc.unwrap().int_subset, (*parent).name);
    if element_desc.is_none() && (*parent).doc.unwrap().ext_subset.is_some() {
        element_desc = xml_get_dtd_element_desc((*parent).doc.unwrap().ext_subset, (*parent).name);
    }
    let Some(element_desc) = element_desc else {
        return -1;
    };

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
    let Some(mut test_node) = xml_new_doc_node((*ref_node).doc, None, "<!dummy?>", null_mut())
    else {
        return -1;
    };

    test_node.parent = NodePtr::from_ptr(parent);
    test_node.prev = NodePtr::from_ptr(prev);
    test_node.next = NodePtr::from_ptr(next);
    let name: *const XmlChar = test_node.name;

    if !prev.is_null() {
        (*prev).next = NodePtr::from_ptr(test_node.as_ptr());
    } else {
        (*parent).set_children(NodePtr::from_ptr(test_node.as_ptr()));
    }

    if !next.is_null() {
        (*next).prev = NodePtr::from_ptr(test_node.as_ptr());
    } else {
        (*parent).set_last(NodePtr::from_ptr(test_node.as_ptr()));
    }

    // Insert each potential child node and check if the parent is still valid
    nb_elements = xml_valid_get_potential_children(
        element_desc.content,
        elements.as_mut_ptr(),
        addr_of_mut!(nb_elements),
        256,
    );

    for i in 0..nb_elements {
        test_node.name = elements[i as usize];
        if xml_validate_one_element(
            addr_of_mut!(vctxt) as _,
            (*parent).doc.unwrap(),
            XmlGenericNodePtr::from_raw(parent),
        ) != 0
        {
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
    test_node.name = name;
    xml_free_node(test_node.as_ptr());

    nb_valid_elements
}

/// Validate that the given value match Name production
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateNameValue")]
#[cfg(feature = "libxml_valid")]
pub unsafe fn xml_validate_name_value(value: *const XmlChar) -> i32 {
    xml_validate_name_value_internal(None, value)
}

/// Validate that the given value match Names production
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateNamesValue")]
#[cfg(feature = "libxml_valid")]
pub unsafe fn xml_validate_names_value(value: *const XmlChar) -> i32 {
    xml_validate_names_value_internal(None, value)
}

/// Validate that the given value match Nmtoken production
///
/// `[ VC: Name Token ]`
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateNmtokenValue")]
#[cfg(feature = "libxml_valid")]
pub unsafe fn xml_validate_nmtoken_value(value: *const XmlChar) -> i32 {
    xml_validate_nmtoken_value_internal(None, value)
}

/// Validate that the given value match Nmtokens production
///
/// `[ VC: Name Token ]`
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateNmtokensValue")]
#[cfg(feature = "libxml_valid")]
pub unsafe fn xml_validate_nmtokens_value(value: *const XmlChar) -> i32 {
    xml_validate_nmtokens_value_internal(None, value)
}

/// Generate the automata sequence needed for that type
///
/// Returns 1 if successful or 0 in case of error.
#[doc(alias = "xmlValidBuildAContentModel")]
unsafe fn xml_valid_build_acontent_model(
    mut content: XmlElementContentPtr,
    ctxt: XmlValidCtxtPtr,
    name: *const XmlChar,
) -> i32 {
    if content.is_null() {
        let name = CStr::from_ptr(name as *const i8).to_string_lossy();
        xml_err_valid_node(
            ctxt,
            None,
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
                None,
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
                        CStr::from_ptr(fullname as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                        null_mut(),
                    );
                }
                XmlElementContentOccur::XmlElementContentOpt => {
                    (*ctxt).state = xml_automata_new_transition(
                        (*ctxt).am,
                        (*ctxt).state,
                        null_mut(),
                        CStr::from_ptr(fullname as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                        null_mut(),
                    );
                    xml_automata_new_epsilon((*ctxt).am, oldstate, (*ctxt).state);
                }
                XmlElementContentOccur::XmlElementContentPlus => {
                    (*ctxt).state = xml_automata_new_transition(
                        (*ctxt).am,
                        (*ctxt).state,
                        null_mut(),
                        CStr::from_ptr(fullname as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                        null_mut(),
                    );
                    xml_automata_new_transition(
                        (*ctxt).am,
                        (*ctxt).state,
                        (*ctxt).state,
                        CStr::from_ptr(fullname as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                        null_mut(),
                    );
                }
                XmlElementContentOccur::XmlElementContentMult => {
                    (*ctxt).state = xml_automata_new_epsilon((*ctxt).am, (*ctxt).state, null_mut());
                    xml_automata_new_transition(
                        (*ctxt).am,
                        (*ctxt).state,
                        (*ctxt).state,
                        CStr::from_ptr(fullname as *const i8)
                            .to_string_lossy()
                            .as_ref(),
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

            // Simply iterate over the content
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

            // iterate over the subtypes and remerge the end with an
            // epsilon transition
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
pub unsafe fn xml_valid_build_content_model(ctxt: XmlValidCtxtPtr, mut elem: XmlElementPtr) -> i32 {
    if ctxt.is_null() {
        return 0;
    }
    if !matches!(elem.element_type(), XmlElementType::XmlElementDecl) {
        return 0;
    }
    if !matches!(elem.etype, XmlElementTypeVal::XmlElementTypeElement) {
        return 1;
    }
    // TODO: should we rebuild in this case ?
    if !elem.cont_model.is_null() {
        if xml_regexp_is_determinist(elem.cont_model) == 0 {
            (*ctxt).valid = 0;
            return 0;
        }
        return 1;
    }

    (*ctxt).am = xml_new_automata();
    let name = elem
        .name
        .as_ref()
        .map(|n| CString::new(n.as_str()).unwrap());
    if (*ctxt).am.is_null() {
        let name = elem.name.as_deref().unwrap();
        xml_err_valid_node(
            ctxt,
            Some(elem.into()),
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
        elem.content,
        ctxt,
        name.as_ref().map_or(null(), |n| n.as_ptr() as *const u8),
    );
    xml_automata_set_final_state((*ctxt).am, (*ctxt).state);
    elem.cont_model = xml_automata_compile((*ctxt).am);
    if xml_regexp_is_determinist(elem.cont_model) != 1 {
        let mut expr: [c_char; 5000] = [0; 5000];
        expr[0] = 0;
        xml_snprintf_element_content(expr.as_mut_ptr() as _, 5000, elem.content, 1);
        let name = elem.name.as_deref().unwrap();
        let expr = CStr::from_ptr(expr.as_ptr()).to_string_lossy();
        xml_err_valid_node(
            ctxt,
            Some(elem.into()),
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
unsafe fn xml_validate_check_mixed(
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
unsafe fn vstate_vpush(
    ctxt: XmlValidCtxtPtr,
    elem_decl: Option<XmlElementPtr>,
    node: XmlNodePtr,
) -> usize {
    // (*ctxt).vstate = (*ctxt).vstate_tab.add((*ctxt).vstate_nr as usize);
    (*ctxt).vstate_tab.push(XmlValidState {
        elem_decl,
        node: node.as_ptr(),
        exec: null_mut(),
    });
    if let Some(elem_decl) =
        elem_decl.filter(|decl| matches!(decl.etype, XmlElementTypeVal::XmlElementTypeElement))
    {
        if elem_decl.cont_model.is_null() {
            xml_valid_build_content_model(ctxt, elem_decl);
        }
        if !elem_decl.cont_model.is_null() {
            (*ctxt).vstate_tab.last_mut().unwrap().exec =
                xml_reg_new_exec_ctxt(elem_decl.cont_model, None, null_mut());
        } else {
            (*ctxt).vstate_tab.last_mut().unwrap().exec = null_mut();
            let node_name = (*node).name().unwrap();
            xml_err_valid_node(
                ctxt,
                Some(elem_decl.into()),
                XmlParserErrors::XmlErrInternalError,
                format!("Failed to build content model regexp for {node_name}\n").as_str(),
                Some(&node_name),
                None,
                None,
            );
        }
    }
    (*ctxt).vstate_tab.len() - 1
}

#[cfg(not(feature = "libxml_regexp"))]
const MAX_RECURSE: usize = 25000;

#[cfg(not(feature = "libxml_regexp"))]
unsafe fn vstate_vpush(
    ctxt: XmlValidCtxtPtr,
    cont: XmlElementContentPtr,
    node: XmlNodePtr,
    depth: c_uchar,
    occurs: c_long,
    state: c_uchar,
) -> i32 {
    let i: i32 = (*ctxt).vstate_nr - 1;

    if (*ctxt).vstate_nr > MAX_RECURSE as i32 {
        return -1;
    }
    if (*ctxt).vstate_tab.is_null() {
        (*ctxt).vstateMax = 8;
        (*ctxt).vstate_tab =
            xml_malloc((*ctxt).vstateMax as usize * size_of_val(&*(*ctxt).vstate_tab.add(0)))
                as *mut XmlValidState;
        if (*ctxt).vstate_tab.is_null() {
            xml_verr_memory(ctxt as _, c"malloc failed".as_ptr() as _);
            return -1;
        }
    }
    if (*ctxt).vstate_nr >= (*ctxt).vstateMax {
        let tmp: *mut XmlValidState;

        tmp = xml_realloc(
            (*ctxt).vstate_tab as _,
            2 * (*ctxt).vstateMax as usize * size_of_val(&*(*ctxt).vstate_tab.add(0)),
        ) as *mut XmlValidState;
        if tmp.is_null() {
            xml_verr_memory(ctxt as _, c"malloc failed".as_ptr() as _);
            return -1;
        }
        (*ctxt).vstateMax *= 2;
        (*ctxt).vstate_tab = tmp;
        (*ctxt).vstate = (*ctxt).vstate_tab.add(0);
    }
    // Don't push on the stack a state already here
    if i >= 0
        && (*(*ctxt).vstate_tab.add(i as usize)).cont == cont
        && (*(*ctxt).vstate_tab.add(i as usize)).node == node.as_ptr()
        && (*(*ctxt).vstate_tab.add(i as usize)).depth == depth
        && (*(*ctxt).vstate_tab.add(i as usize)).occurs == occurs
        && (*(*ctxt).vstate_tab.add(i as usize)).state == state
    {
        return (*ctxt).vstate_nr;
    }
    (*(*ctxt).vstate_tab.add((*ctxt).vstate_nr as usize)).cont = cont;
    (*(*ctxt).vstate_tab.add((*ctxt).vstate_nr as usize)).node = node;
    (*(*ctxt).vstate_tab.add((*ctxt).vstate_nr as usize)).depth = depth;
    (*(*ctxt).vstate_tab.add((*ctxt).vstate_nr as usize)).occurs = occurs;
    (*(*ctxt).vstate_tab.add((*ctxt).vstate_nr as usize)).state = state;

    let res = (*ctxt).vstate_nr;
    (*ctxt).vstate_nr += 1;
    res
}

/// Push a new element start on the validation stack.
///
/// returns 1 if no validation problem was found or 0 otherwise
#[doc(alias = "xmlValidatePushElement")]
#[cfg(all(feature = "libxml_valid", feature = "libxml_regexp"))]
pub unsafe fn xml_validate_push_element(
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

    if let Some(state) = (*ctxt).vstate_tab.last() {
        // Check the new element against the content model of the new elem.
        if let Some(elem_decl) = state.elem_decl {
            match elem_decl.etype {
                XmlElementTypeVal::XmlElementTypeUndefined => {
                    ret = 0;
                }
                XmlElementTypeVal::XmlElementTypeEmpty => {
                    let name = (*state.node).name().unwrap();
                    xml_err_valid_node(
                        ctxt,
                        XmlGenericNodePtr::from_raw(state.node),
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
                    if !elem_decl.content.is_null()
                        && (*elem_decl.content).typ
                            == XmlElementContentType::XmlElementContentPCDATA
                    {
                        let name = (*state.node).name().unwrap();
                        xml_err_valid_node(
                            ctxt,
                            XmlGenericNodePtr::from_raw(state.node),
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
                        ret = xml_validate_check_mixed(ctxt, elem_decl.content, qname);
                        if ret != 1 {
                            let qname = CStr::from_ptr(qname as *const i8).to_string_lossy();
                            let name = (*state.node).name().unwrap();
                            xml_err_valid_node(
                                ctxt,
                                XmlGenericNodePtr::from_raw(state.node),
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
                    if !state.exec.is_null() {
                        ret = xml_reg_exec_push_string(state.exec, qname, null_mut());
                        if ret < 0 {
                            let name = (*state.node).name().unwrap();
                            let qname = CStr::from_ptr(qname as *const i8).to_string_lossy();
                            xml_err_valid_node(
                                ctxt,
                                XmlGenericNodePtr::from_raw(state.node),
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
    let e_decl = xml_valid_get_elem_decl(ctxt, doc, elem, addr_of_mut!(extsubset));
    vstate_vpush(ctxt, e_decl, elem);
    ret
}

/// Check the CData parsed for validation in the current stack
///
/// Returns 1 if no validation problem was found or 0 otherwise
#[doc(alias = "xmlValidatePushCData")]
#[cfg(all(feature = "libxml_valid", feature = "libxml_regexp"))]
pub unsafe fn xml_validate_push_cdata(
    ctxt: XmlValidCtxtPtr,
    data: *const XmlChar,
    len: i32,
) -> i32 {
    let mut ret: i32 = 1;

    // printf("CDATA %s %d\n", data, len);
    if ctxt.is_null() {
        return 0;
    }
    if len <= 0 {
        return ret;
    }
    if let Some(state) = (*ctxt).vstate_tab.last() {
        // Check the new element against the content model of the new elem.
        if let Some(elem_decl) = state.elem_decl {
            match elem_decl.etype {
                XmlElementTypeVal::XmlElementTypeUndefined => {
                    ret = 0;
                }
                XmlElementTypeVal::XmlElementTypeEmpty => {
                    let name = (*state.node).name().unwrap();
                    xml_err_valid_node(
                        ctxt,
                        XmlGenericNodePtr::from_raw(state.node),
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
                            let name = (*state.node).name().unwrap();
                            xml_err_valid_node(
                                ctxt,
                                XmlGenericNodePtr::from_raw(state.node),
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
unsafe fn vstate_vpop(ctxt: XmlValidCtxtPtr) -> i32 {
    if (*ctxt).vstate_tab.is_empty() {
        return -1;
    }
    let state = (*ctxt).vstate_tab.pop().unwrap();
    let elem_decl = state.elem_decl;
    if elem_decl.map_or(false, |elem_decl| {
        matches!(elem_decl.etype, XmlElementTypeVal::XmlElementTypeElement)
    }) {
        xml_reg_free_exec_ctxt(state.exec);
    }
    (*ctxt).vstate_tab.len() as i32
}

#[cfg(not(feature = "libxml_regexp"))]
unsafe fn vstateVPop(ctxt: XmlValidCtxtPtr) -> i32 {
    if (*ctxt).vstate_nr <= 1 {
        return -1;
    }
    (*ctxt).vstate_nr -= 1;
    (*ctxt).vstate = (*ctxt).vstate_tab.add(0);
    (*(*ctxt).vstate).cont = (*(*ctxt).vstate_tab.add((*ctxt).vstate_nr as usize)).cont;
    (*(*ctxt).vstate).node = (*(*ctxt).vstate_tab.add((*ctxt).vstate_nr as usize)).node;
    (*(*ctxt).vstate).depth = (*(*ctxt).vstate_tab.add((*ctxt).vstate_nr as usize)).depth;
    (*(*ctxt).vstate).occurs = (*(*ctxt).vstate_tab.add((*ctxt).vstate_nr as usize)).occurs;
    (*(*ctxt).vstate).state = (*(*ctxt).vstate_tab.add((*ctxt).vstate_nr as usize)).state;
    return (*ctxt).vstate_nr;
}

/// Pop the element end from the validation stack.
///
/// Returns 1 if no validation problem was found or 0 otherwise
#[doc(alias = "xmlValidatePopElement")]
#[cfg(all(feature = "libxml_valid", feature = "libxml_regexp"))]
pub unsafe fn xml_validate_pop_element(
    ctxt: XmlValidCtxtPtr,
    _doc: Option<XmlDocPtr>,
    _elem: Option<XmlNodePtr>,
    _qname: *const XmlChar,
) -> i32 {
    let mut ret: i32 = 1;

    if ctxt.is_null() {
        return 0;
    }
    // printf("PopElem %s\n", qname);
    if let Some(state) = (*ctxt).vstate_tab.last() {
        // Check the new element against the content model of the new elem.
        if let Some(elem_decl) = state.elem_decl {
            if matches!(elem_decl.etype, XmlElementTypeVal::XmlElementTypeElement)
                && !state.exec.is_null()
            {
                ret = xml_reg_exec_push_string(state.exec, null_mut(), null_mut());
                if ret <= 0 {
                    let name = (*state.node).name().unwrap();
                    xml_err_valid_node(
                        ctxt,
                        XmlGenericNodePtr::from_raw(state.node),
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
unsafe fn xmlValidateSkipIgnorable(mut child: *mut XmlNode) -> *mut XmlNode {
    while !child.is_null() {
        match (*child).typ {
            // These things are ignored (skipped) during validation.
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
            // keep current node
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
}
