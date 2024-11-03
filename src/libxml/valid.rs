//! Provide methods and data structures for handling DTD validation.  
//! This module is based on `libxml/valid.h`, `valid.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    ffi::{c_char, c_int, c_uint},
    mem::{size_of, size_of_val, zeroed},
    os::raw::c_void,
    ptr::{addr_of_mut, null, null_mut},
    sync::atomic::Ordering,
};

use libc::{memset, size_t, strcat, strcmp, strlen};

#[cfg(not(feature = "regexp"))]
use crate::libxml::tree::xml_free_node_list;
#[cfg(feature = "regexp")]
use crate::libxml::xmlautomata::{
    xml_automata_compile, xml_automata_get_init_state, xml_automata_set_final_state,
    xml_free_automata, xml_new_automata, XmlAutomataPtr,
};
#[cfg(feature = "regexp")]
use crate::libxml::xmlregexp::{
    xml_reg_exec_push_string, xml_reg_free_exec_ctxt, xml_reg_free_regexp, xml_reg_new_exec_ctxt,
    xml_regexp_is_determinist, XmlRegExecCtxtPtr,
};
#[cfg(feature = "regexp")]
use crate::libxml::xmlstring::xml_strncmp;
use crate::{
    __xml_raise_error,
    buf::libxml_api::XmlBufPtr,
    globals::{GenericError, GenericErrorContext, StructuredError},
    libxml::{
        dict::{xml_dict_lookup, xml_dict_owns, XmlDictPtr},
        entities::{xml_get_doc_entity, XmlEntitiesTablePtr, XmlEntityPtr, XmlEntityType},
        globals::{xml_free, xml_malloc, xml_realloc},
        hash::{
            xml_hash_add_entry, xml_hash_add_entry2, xml_hash_add_entry3, xml_hash_copy,
            xml_hash_create_dict, xml_hash_free, xml_hash_lookup, xml_hash_lookup2,
            xml_hash_lookup3, xml_hash_remove_entry, xml_hash_remove_entry2, xml_hash_scan,
            xml_hash_update_entry, XmlHashTable,
        },
        list::{
            xml_list_append, xml_list_create, xml_list_delete, xml_list_empty,
            xml_list_remove_first, xml_list_walk, XmlListPtr,
        },
        parser::{XmlParserCtxtPtr, XmlParserMode},
        parser_internals::xml_string_current_char,
        tree::{
            xml_build_qname, xml_doc_get_root_element, xml_free_node, xml_get_line_no,
            xml_is_blank_node, xml_new_doc_node, xml_node_list_get_string, xml_split_qname2,
            xml_split_qname3, xml_unlink_node, XmlAttrPtr, XmlAttribute, XmlAttributeDefault,
            XmlAttributePtr, XmlAttributeType, XmlDocProperties, XmlDocPtr, XmlDtdPtr, XmlElement,
            XmlElementContent, XmlElementContentOccur, XmlElementContentPtr, XmlElementContentType,
            XmlElementPtr, XmlElementType, XmlElementTypeVal, XmlEnumeration, XmlEnumerationPtr,
            XmlID, XmlIDPtr, XmlNode, XmlNodePtr, XmlNotation, XmlNotationPtr, XmlNsPtr, XmlRef,
            XmlRefPtr,
        },
        xmlautomata::{
            xml_automata_new_epsilon, xml_automata_new_state, xml_automata_new_transition,
            XmlAutomataStatePtr,
        },
        xmlerror::XmlParserErrors,
        xmlstring::{xml_str_equal, xml_strdup, xml_strlen, xml_strndup, XmlChar},
    },
    private::parser::XML_VCTXT_USE_PCTXT,
    IS_COMBINING, IS_DIGIT, IS_EXTENDER, IS_LETTER,
};

use super::{chvalid::xml_is_blank_char, hash::CVoidWrapper};

/*
 * Validation state added for non-determinist content model.
 */
pub type XmlValidStatePtr = *mut XmlValidState;
/*
 * If regexp are enabled we can do continuous validation without the
 * need of a tree to validate the content model. this is done in each
 * callbacks.
 * Each xmlValidState represent the validation state associated to the
 * set of nodes currently open from the document root to the current element.
 */
#[cfg(feature = "regexp")]
#[repr(C)]
pub struct XmlValidState {
    elem_decl: XmlElementPtr, /* pointer to the content model */
    node: XmlNodePtr,         /* pointer to the current node */
    exec: XmlRegExecCtxtPtr,  /* regexp runtime */
}
#[cfg(not(feature = "regexp"))]
#[repr(C)]
pub struct XmlValidState {
    cont: XmlElementContentPtr, /* pointer to the content model subtree */
    node: XmlNodePtr,           /* pointer to the current node in the list */
    occurs: c_long,             /* bitfield for multiple occurrences */
    depth: c_uchar,             /* current depth in the overall tree */
    state: c_uchar,             /* ROLLBACK_XXX */
}

/**
 * xmlValidityErrorFunc:
 * @ctx:  usually an xmlValidCtxtPtr to a validity error context,
 *        but comes from ctxt->userData (which normally contains such
 *        a pointer); ctxt->userData can be changed by the user.
 * @msg:  the string to format *printf like vararg
 * @...:  remaining arguments to the format
 *
 * Callback called when a validity error is found. This is a message
 * oriented function similar to an *printf function.
 */
pub type XmlValidityErrorFunc = unsafe extern "C" fn(ctx: *mut c_void, msg: *const c_char);

/**
 * xmlValidityWarningFunc:
 * @ctx:  usually an xmlValidCtxtPtr to a validity error context,
 *        but comes from ctxt->userData (which normally contains such
 *        a pointer); ctxt->userData can be changed by the user.
 * @msg:  the string to format *printf like vararg
 * @...:  remaining arguments to the format
 *
 * Callback called when a validity warning is found. This is a message
 * oriented function similar to an *printf function.
 */
pub type XmlValidityWarningFunc = unsafe extern "C" fn(ctx: *mut c_void, msg: *const c_char);

/*
 * xmlValidCtxt:
 * An xmlValidCtxt is used for error reporting when validating.
 */
pub type XmlValidCtxtPtr = *mut XmlValidCtxt;
#[repr(C)]
pub struct XmlValidCtxt {
    pub(crate) user_data: Option<GenericErrorContext>, /* user specific data block */
    pub error: Option<GenericError>,                   /* the callback in case of errors */
    pub warning: Option<GenericError>,                 /* the callback in case of warning */

    /* Node analysis stack used when validating within entities */
    pub(crate) node: XmlNodePtr,          /* Current parsed Node */
    pub(crate) node_nr: c_int,            /* Depth of the parsing stack */
    pub(crate) node_max: c_int,           /* Max depth of the parsing stack */
    pub(crate) node_tab: *mut XmlNodePtr, /* array of nodes */

    pub(crate) flags: c_uint,  /* internal flags */
    pub(crate) doc: XmlDocPtr, /* the document */
    pub(crate) valid: c_int,   /* temporary validity check result */

    /* state state used for non-determinist content validation */
    pub(crate) vstate: *mut XmlValidState, /* current state */
    pub(crate) vstate_nr: c_int,           /* Depth of the validation stack */
    pub(crate) vstate_max: c_int,          /* Max depth of the validation stack */
    pub(crate) vstate_tab: *mut XmlValidState, /* array of validation states */

    #[cfg(feature = "regexp")]
    pub(crate) am: XmlAutomataPtr, /* the automata */
    #[cfg(feature = "regexp")]
    pub(crate) state: XmlAutomataStatePtr, /* used to build the automata */
    #[cfg(not(feature = "regexp"))]
    pub(crate) am: *mut c_void,
    #[cfg(not(feature = "regexp"))]
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

/*
 * ALL notation declarations are stored in a table.
 * There is one table per DTD.
 */

pub type XmlNotationTable = XmlHashTable<'static, CVoidWrapper>;
pub type XmlNotationTablePtr = *mut XmlNotationTable;

/*
 * ALL element declarations are stored in a table.
 * There is one table per DTD.
 */

pub type XmlElementTable = XmlHashTable<'static, CVoidWrapper>;
pub type XmlElementTablePtr = *mut XmlElementTable;

/*
 * ALL attribute declarations are stored in a table.
 * There is one table per DTD.
 */

pub type XmlAttributeTable = XmlHashTable<'static, CVoidWrapper>;
pub type XmlAttributeTablePtr = *mut XmlAttributeTable;

/*
 * ALL IDs attributes are stored in a table.
 * There is one table per document.
 */

pub type XmlIDTable = XmlHashTable<'static, CVoidWrapper>;
pub type XmlIDTablePtr = *mut XmlIDTable;

/*
 * ALL Refs attributes are stored in a table.
 * There is one table per document.
 */

pub type XmlRefTable = XmlHashTable<'static, CVoidWrapper>;
pub type XmlRefTablePtr = *mut XmlRefTable;

/**
 * xmlErrValid:
 * @ctxt:  an XML validation parser context
 * @error:  the error number
 * @extra:  extra information
 *
 * Handle a validation error
 */
unsafe extern "C" fn xml_err_valid(
    ctxt: XmlValidCtxtPtr,
    error: XmlParserErrors,
    msg: *const c_char,
    extra: *const c_char,
) {
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
    if !extra.is_null() {
        __xml_raise_error!(
            None,
            channel,
            data,
            pctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromValid,
            error,
            XmlErrorLevel::XmlErrError,
            null_mut(),
            0,
            (!extra.is_null()).then(|| CStr::from_ptr(extra).to_string_lossy().into_owned().into()),
            None,
            None,
            0,
            0,
            msg,
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
            error,
            XmlErrorLevel::XmlErrError,
            null_mut(),
            0,
            None,
            None,
            None,
            0,
            0,
            c"%s".as_ptr() as _,
            msg
        );
    }
}

/**
 * xmlVErrMemory:
 * @ctxt:  an XML validation parser context
 * @extra:  extra information
 *
 * Handle an out of memory error
 */
unsafe extern "C" fn xml_verr_memory(ctxt: XmlValidCtxtPtr, extra: *const c_char) {
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
    if !extra.is_null() {
        __xml_raise_error!(
            None,
            channel,
            data,
            pctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromValid,
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
            None,
            channel,
            data,
            pctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromValid,
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
 * xmlFreeNotation:
 * @not:  A notation
 *
 * Deallocate the memory used by an notation definition
 */
unsafe extern "C" fn xml_free_notation(nota: XmlNotationPtr) {
    if nota.is_null() {
        return;
    }
    if !(*nota).name.is_null() {
        xml_free((*nota).name as _);
    }
    if !(*nota).public_id.is_null() {
        xml_free((*nota).public_id as _);
    }
    if !(*nota).system_id.is_null() {
        xml_free((*nota).system_id as _);
    }
    xml_free(nota as _);
}

/* Notation */
/**
 * xmlAddNotationDecl:
 * @dtd:  pointer to the DTD
 * @ctxt:  the validation context
 * @name:  the entity name
 * @PublicID:  the public identifier or null_mut()
 * @SystemID:  the system identifier or null_mut()
 *
 * Register a new notation declaration
 *
 * Returns null_mut() if not, otherwise the entity
 */
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
        xml_verr_memory(
            ctxt,
            c"xmlAddNotationDecl: Table creation failed!\n".as_ptr() as _,
        );
        return null_mut();
    }

    let ret: XmlNotationPtr = xml_malloc(size_of::<XmlNotation>()) as XmlNotationPtr;
    if ret.is_null() {
        xml_verr_memory(ctxt as _, c"malloc failed".as_ptr() as _);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlNotation>());

    /*
     * fill the structure.
     */
    (*ret).name = xml_strdup(name);
    if !system_id.is_null() {
        (*ret).system_id = xml_strdup(system_id);
    }
    if !public_id.is_null() {
        (*ret).public_id = xml_strdup(public_id);
    }

    /*
     * Validity Check:
     * Check the DTD for previous declarations of the ATTLIST
     */
    if xml_hash_add_entry(table, name, ret as _) != 0 {
        #[cfg(feature = "valid")]
        {
            xml_err_valid(
                null_mut(),
                XmlParserErrors::XmlDTDNotationRedefined,
                c"xmlAddNotationDecl: %s already defined\n".as_ptr() as _,
                name as *const c_char,
            );
        }
        xml_free_notation(ret);
        return null_mut();
    }
    ret
}

/**
 * xmlCopyNotation:
 * @nota:  A notation
 *
 * Build a copy of a notation.
 *
 * Returns the new xmlNotationPtr or null_mut() in case of error.
 */
#[cfg(feature = "tree")]
extern "C" fn xml_copy_notation(payload: *mut c_void, _name: *const XmlChar) -> *mut c_void {
    let nota: XmlNotationPtr = payload as XmlNotationPtr;

    unsafe {
        let cur: XmlNotationPtr = xml_malloc(size_of::<XmlNotation>()) as XmlNotationPtr;
        if cur.is_null() {
            xml_verr_memory(null_mut(), c"malloc failed".as_ptr() as _);
            return null_mut();
        }
        if !(*nota).name.is_null() {
            (*cur).name = xml_strdup((*nota).name);
        } else {
            (*cur).name = null_mut();
        }
        if !(*nota).public_id.is_null() {
            (*cur).public_id = xml_strdup((*nota).public_id);
        } else {
            (*cur).public_id = null_mut();
        }
        if !(*nota).system_id.is_null() {
            (*cur).system_id = xml_strdup((*nota).system_id);
        } else {
            (*cur).system_id = null_mut();
        }
        cur as _
    }
}

/**
 * xmlCopyNotationTable:
 * @table:  A notation table
 *
 * Build a copy of a notation table.
 *
 * Returns the new xmlNotationTablePtr or null_mut() in case of error.
 */
#[cfg(feature = "tree")]
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

/**
 * xmlFreeNotationTable:
 * @table:  An notation table
 *
 * Deallocate the memory used by an entities hash table.
 */
pub unsafe extern "C" fn xml_free_notation_table(table: XmlNotationTablePtr) {
    xml_hash_free(table, Some(xml_free_notation_table_entry));
}

/**
 * xmlDumpNotationDecl:
 * @buf:  the XML buffer output
 * @nota:  A notation declaration
 *
 * This will dump the content the notation declaration as an XML DTD definition
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_dump_notation_decl(buf: XmlBufPtr, nota: XmlNotationPtr) {
    use crate::buf::libxml_api::{xml_buf_cat, xml_buf_write_quoted_string};

    if buf.is_null() || nota.is_null() {
        return;
    }
    xml_buf_cat(buf, c"<!NOTATION ".as_ptr() as _);
    xml_buf_cat(buf, (*nota).name as _);
    if !(*nota).public_id.is_null() {
        xml_buf_cat(buf, c" PUBLIC ".as_ptr() as _);
        xml_buf_write_quoted_string(buf, (*nota).public_id as _);
        if !(*nota).system_id.is_null() {
            xml_buf_cat(buf, c" ".as_ptr() as _);
            xml_buf_write_quoted_string(buf, (*nota).system_id as _);
        }
    } else {
        xml_buf_cat(buf, c" SYSTEM ".as_ptr() as _);
        xml_buf_write_quoted_string(buf, (*nota).system_id as _);
    }
    xml_buf_cat(buf, c" >\n".as_ptr() as _);
}

/**
 * xmlDumpNotationDeclScan:
 * @nota:  A notation declaration
 * @buf:  the XML buffer output
 *
 * This is called with the hash scan function, and just reverses args
 */
#[cfg(feature = "output")]
extern "C" fn xml_dump_notation_decl_scan(
    nota: *mut c_void,
    buf: *mut c_void,
    _name: *const XmlChar,
) {
    unsafe {
        xml_dump_notation_decl(buf as XmlBufPtr, nota as XmlNotationPtr);
    }
}

/**
 * xmlDumpNotationTable:
 * @buf:  the XML buffer output
 * @table:  A notation table
 *
 * This will dump the content of the notation table as an XML DTD definition
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_dump_notation_table(buf: XmlBufPtr, table: XmlNotationTablePtr) {
    if buf.is_null() || table.is_null() {
        return;
    }
    xml_hash_scan(table, Some(xml_dump_notation_decl_scan), buf as _);
}

/* Element Content */
/* the non Doc version are being deprecated */
/**
 * xmlNewElementContent:
 * @name:  the subelement name or null_mut()
 * @type:  the type of element content decl
 *
 * Allocate an element content structure.
 * Deprecated in favor of xmlNewDocElementContent
 *
 * Returns null_mut() if not, otherwise the new element content structure
 */
pub unsafe extern "C" fn xml_new_element_content(
    name: *const XmlChar,
    typ: XmlElementContentType,
) -> XmlElementContentPtr {
    xml_new_doc_element_content(null_mut(), name, typ)
}

/**
 * xmlCopyElementContent:
 * @cur:  An element content pointer.
 *
 * Build a copy of an element content description.
 * Deprecated, use xmlCopyDocElementContent instead
 *
 * Returns the new xmlElementContentPtr or null_mut() in case of error.
 */
pub unsafe extern "C" fn xml_copy_element_content(
    content: XmlElementContentPtr,
) -> XmlElementContentPtr {
    xml_copy_doc_element_content(null_mut(), content)
}

/**
 * xmlFreeElementContent:
 * @cur:  the element content tree to free
 *
 * Free an element content structure. The whole subtree is removed.
 * Deprecated, use xmlFreeDocElementContent instead
 */
pub unsafe extern "C" fn xml_free_element_content(cur: XmlElementContentPtr) {
    xml_free_doc_element_content(null_mut(), cur);
}

/* the new versions with doc argument */
/**
 * xmlNewDocElementContent:
 * @doc:  the document
 * @name:  the subelement name or null_mut()
 * @type:  the type of element content decl
 *
 * Allocate an element content structure for the document.
 *
 * Returns null_mut() if not, otherwise the new element content structure
 */
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
                xml_err_valid(
                    null_mut(),
                    XmlParserErrors::XmlErrInternalError,
                    c"xmlNewElementContent : name == NULL !\n".as_ptr() as _,
                    null_mut(),
                );
            }
        }
        XmlElementContentType::XmlElementContentPcdata
        | XmlElementContentType::XmlElementContentSeq
        | XmlElementContentType::XmlElementContentOr => {
            if !name.is_null() {
                xml_err_valid(
                    null_mut(),
                    XmlParserErrors::XmlErrInternalError,
                    c"xmlNewElementContent : name != NULL !\n".as_ptr() as _,
                    null_mut(),
                );
            }
        } // _ => {
          //     xml_err_valid(
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
        xml_verr_memory(null_mut(), c"malloc failed".as_ptr() as _);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlElementContent>());
    (*ret).typ = typ;
    (*ret).ocur = XmlElementContentOccur::XmlElementContentOnce;
    if !name.is_null() {
        let mut l: c_int = 0;

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

/**
 * xmlCopyDocElementContent:
 * @doc:  the document owning the element declaration
 * @cur:  An element content pointer.
 *
 * Build a copy of an element content description.
 *
 * Returns the new xmlElementContentPtr or null_mut() in case of error.
 */
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
        xml_verr_memory(null_mut(), c"malloc failed".as_ptr() as _);
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
                xml_verr_memory(null_mut(), c"malloc failed".as_ptr() as _);
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

/**
 * xmlFreeDocElementContent:
 * @doc: the document owning the element declaration
 * @cur:  the element content tree to free
 *
 * Free an element content structure. The whole subtree is removed.
 */
pub unsafe extern "C" fn xml_free_doc_element_content(
    doc: XmlDocPtr,
    mut cur: XmlElementContentPtr,
) {
    let mut dict: XmlDictPtr = null_mut();
    let mut depth: size_t = 0;

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
            XmlElementContentType::XmlElementContentPcdata
            | XmlElementContentType::XmlElementContentElement
            | XmlElementContentType::XmlElementContentSeq
            | XmlElementContentType::XmlElementContentOr => {} // _ => {
                                                               //     xml_err_valid(
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

/**
 * xmlSnprintfElementContent:
 * @buf:  an output buffer
 * @size:  the buffer size
 * @content:  An element table
 * @englob: 1 if one must print the englobing parenthesis, 0 otherwise
 *
 * This will dump the content of the element content definition
 * Intended just for the debug routine
 */
pub unsafe extern "C" fn xml_snprintf_element_content(
    buf: *mut c_char,
    size: c_int,
    content: XmlElementContentPtr,
    englob: c_int,
) {
    let mut len: c_int;

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
        XmlElementContentType::XmlElementContentPcdata => {
            strcat(buf, c"#PCDATA".as_ptr() as _);
        }
        XmlElementContentType::XmlElementContentElement => {
            let mut qname_len: c_int = xml_strlen((*content).name);

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

/* DEPRECATED */
/**
 * xmlSprintfElementContent:
 * @buf:  an output buffer
 * @content:  An element table
 * @englob: 1 if one must print the englobing parenthesis, 0 otherwise
 *
 * Deprecated, unsafe, use xmlSnprintfElementContent
 */
#[deprecated]
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_sprintf_element_content(
    _buf: *mut c_char,
    _content: XmlElementContentPtr,
    _englob: c_int,
) {
}
/* DEPRECATED */

/**
 * xmlFreeElement:
 * @elem:  An element
 *
 * Deallocate the memory used by an element definition
 */
unsafe extern "C" fn xml_free_element(elem: XmlElementPtr) {
    if elem.is_null() {
        return;
    }
    xml_unlink_node(elem as XmlNodePtr);
    xml_free_doc_element_content((*elem).doc, (*elem).content);
    if !(*elem).name.is_null() {
        xml_free((*elem).name as _);
    }
    if !(*elem).prefix.is_null() {
        xml_free((*elem).prefix as _);
    }
    #[cfg(feature = "regexp")]
    if !(*elem).cont_model.is_null() {
        xml_reg_free_regexp((*elem).cont_model);
    }
    xml_free(elem as _);
}

/**
 * xmlErrValidNode:
 * @ctxt:  an XML validation parser context
 * @node:  the node raising the error
 * @error:  the error number
 * @str1:  extra information
 * @str2:  extra information
 * @str3:  extra information
 *
 * Handle a validation error, provide contextual information
 */
#[cfg(any(feature = "valid", feature = "schema"))]
unsafe extern "C" fn xml_err_valid_node(
    ctxt: XmlValidCtxtPtr,
    node: XmlNodePtr,
    error: XmlParserErrors,
    msg: *const c_char,
    str1: *const XmlChar,
    str2: *const XmlChar,
    str3: *const XmlChar,
) {
    use crate::globals::StructuredError;

    let schannel: Option<StructuredError> = None;
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
        schannel,
        channel,
        data,
        pctxt as _,
        node as _,
        XmlErrorDomain::XmlFromValid,
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
        (!str3.is_null()).then(|| CStr::from_ptr(str3 as *const i8)
            .to_string_lossy()
            .into_owned()
            .into()),
        0,
        0,
        msg,
        str1,
        str2,
        str3
    );
}

/* Element */
/**
 * xmlAddElementDecl:
 * @ctxt:  the validation context
 * @dtd:  pointer to the DTD
 * @name:  the entity name
 * @type:  the element type
 * @content:  the element content tree or null_mut()
 *
 * Register a new element declaration
 *
 * Returns null_mut() if not, otherwise the entity
 */
pub unsafe extern "C" fn xml_add_element_decl(
    ctxt: XmlValidCtxtPtr,
    dtd: XmlDtdPtr,
    mut name: *const XmlChar,
    typ: XmlElementTypeVal,
    content: XmlElementContentPtr,
) -> XmlElementPtr {
    let mut ret: XmlElementPtr;
    let mut table: XmlElementTablePtr;
    let mut old_attributes: XmlAttributePtr = null_mut();
    let mut ns: *mut XmlChar = null_mut();

    if dtd.is_null() {
        return null_mut();
    }
    if name.is_null() {
        return null_mut();
    }

    match typ {
        XmlElementTypeVal::XmlElementTypeEmpty => {
            if !content.is_null() {
                xml_err_valid(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    c"xmlAddElementDecl: content != NULL for EMPTY\n".as_ptr() as _,
                    null_mut(),
                );
                return null_mut();
            }
        }
        XmlElementTypeVal::XmlElementTypeAny => {
            if !content.is_null() {
                xml_err_valid(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    c"xmlAddElementDecl: content != NULL for ANY\n".as_ptr() as _,
                    null_mut(),
                );
                return null_mut();
            }
        }
        XmlElementTypeVal::XmlElementTypeMixed => {
            if content.is_null() {
                xml_err_valid(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    c"xmlAddElementDecl: content == NULL for MIXED\n".as_ptr() as _,
                    null_mut(),
                );
                return null_mut();
            }
        }
        XmlElementTypeVal::XmlElementTypeElement => {
            if content.is_null() {
                xml_err_valid(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    c"xmlAddElementDecl: content == NULL for ELEMENT\n".as_ptr() as _,
                    null_mut(),
                );
                return null_mut();
            }
        }
        _ => {
            xml_err_valid(
                ctxt,
                XmlParserErrors::XmlErrInternalError,
                c"Internal: ELEMENT decl corrupted invalid type\n".as_ptr() as _,
                null_mut(),
            );
            return null_mut();
        }
    }

    /*
     * check if name is a QName
     */
    let uqname: *mut XmlChar = xml_split_qname2(name, addr_of_mut!(ns));
    if !uqname.is_null() {
        name = uqname;
    }

    /*
     * Create the Element table if needed.
     */
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
        xml_verr_memory(
            ctxt,
            c"xmlAddElementDecl: Table creation failed!\n".as_ptr() as _,
        );
        if !uqname.is_null() {
            xml_free(uqname as _);
        }
        if !ns.is_null() {
            xml_free(ns as _);
        }
        return null_mut();
    }

    /*
     * lookup old attributes inserted on an undefined element in the
     * internal subset.
     */
    if !(*dtd).doc.is_null() && !(*(*dtd).doc).int_subset.is_null() {
        ret = xml_hash_lookup2((*(*(*dtd).doc).int_subset).elements as _, name, ns) as _;
        if !ret.is_null() && matches!((*ret).etype, XmlElementTypeVal::XmlElementTypeUndefined) {
            old_attributes = (*ret).attributes;
            (*ret).attributes = null_mut();
            xml_hash_remove_entry2((*(*(*dtd).doc).int_subset).elements as _, name, ns, None);
            xml_free_element(ret);
        }
    }

    /*
     * The element may already be present if one of its attribute
     * was registered first
     */
    ret = xml_hash_lookup2(table, name, ns) as _;
    if !ret.is_null() {
        if !matches!((*ret).etype, XmlElementTypeVal::XmlElementTypeUndefined) {
            #[cfg(feature = "valid")]
            {
                /*
                 * The element is already defined in this DTD.
                 */
                xml_err_valid_node(
                    ctxt,
                    dtd as XmlNodePtr,
                    XmlParserErrors::XmlDTDElemRedefined,
                    c"Redefinition of element %s\n".as_ptr() as _,
                    name,
                    null_mut(),
                    null_mut(),
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
            xml_verr_memory(ctxt as _, c"malloc failed".as_ptr() as _);
            if !uqname.is_null() {
                xml_free(uqname as _);
            }
            if !ns.is_null() {
                xml_free(ns as _);
            }
            return null_mut();
        }
        memset(ret as _, 0, size_of::<XmlElement>());
        (*ret).typ = XmlElementType::XmlElementDecl;

        /*
         * fill the structure.
         */
        (*ret).name = xml_strdup(name);
        if (*ret).name.is_null() {
            xml_verr_memory(ctxt as _, c"malloc failed".as_ptr() as _);
            if !uqname.is_null() {
                xml_free(uqname as _);
            }
            if !ns.is_null() {
                xml_free(ns as _);
            }
            xml_free(ret as _);
            return null_mut();
        }
        (*ret).prefix = ns;

        /*
         * Validity Check:
         * Insertion must not fail
         */
        if xml_hash_add_entry2(table, name, ns, ret as _) != 0 {
            #[cfg(feature = "valid")]
            {
                /*
                 * The element is already defined in this DTD.
                 */
                xml_err_valid_node(
                    ctxt,
                    dtd as XmlNodePtr,
                    XmlParserErrors::XmlDTDElemRedefined,
                    c"Redefinition of element %s\n".as_ptr() as _,
                    name,
                    null_mut(),
                    null_mut(),
                );
            }
            xml_free_element(ret);
            if !uqname.is_null() {
                xml_free(uqname as _);
            }
            return null_mut();
        }
        /*
         * For new element, may have attributes from earlier
         * definition in internal subset
         */
        (*ret).attributes = old_attributes;
    }

    /*
     * Finish to fill the structure.
     */
    (*ret).etype = typ;
    /*
     * Avoid a stupid copy when called by the parser
     * and flag it by setting a special parent value
     * so the parser doesn't unallocate it.
     */
    if !ctxt.is_null() && (*ctxt).flags & XML_VCTXT_USE_PCTXT as u32 != 0 {
        (*ret).content = content;
        if !content.is_null() {
            (*content).parent = 1 as XmlElementContentPtr;
        }
    } else {
        (*ret).content = xml_copy_doc_element_content((*dtd).doc, content);
    }

    /*
     * Link it to the DTD
     */
    (*ret).parent = dtd;
    (*ret).doc = (*dtd).doc;
    if (*dtd).last.is_null() {
        (*dtd).children = ret as XmlNodePtr;
        (*dtd).last = (*dtd).children;
    } else {
        (*(*dtd).last).next = ret as XmlNodePtr;
        (*ret).prev = (*dtd).last;
        (*dtd).last = ret as XmlNodePtr;
    }
    if !uqname.is_null() {
        xml_free(uqname as _);
    }
    ret
}

/**
 * xmlCopyElement:
 * @elem:  An element
 *
 * Build a copy of an element.
 *
 * Returns the new xmlElementPtr or null_mut() in case of error.
 */
#[cfg(feature = "tree")]
extern "C" fn xml_copy_element(payload: *mut c_void, _name: *const XmlChar) -> *mut c_void {
    let elem: XmlElementPtr = payload as XmlElementPtr;

    unsafe {
        let cur: XmlElementPtr = xml_malloc(size_of::<XmlElement>()) as XmlElementPtr;
        if cur.is_null() {
            xml_verr_memory(null_mut(), c"malloc failed".as_ptr() as _);
            return null_mut();
        }
        memset(cur as _, 0, size_of::<XmlElement>());
        (*cur).typ = XmlElementType::XmlElementDecl;
        (*cur).etype = (*elem).etype;
        if !(*elem).name.is_null() {
            (*cur).name = xml_strdup((*elem).name);
        } else {
            (*cur).name = null_mut();
        }
        if !(*elem).prefix.is_null() {
            (*cur).prefix = xml_strdup((*elem).prefix);
        } else {
            (*cur).prefix = null_mut();
        }
        (*cur).content = xml_copy_element_content((*elem).content);
        /* TODO : rebuild the attribute list on the copy */
        (*cur).attributes = null_mut();
        cur as _
    }
}

/**
 * xmlCopyElementTable:
 * @table:  An element table
 *
 * Build a copy of an element table.
 *
 * Returns the new xmlElementTablePtr or null_mut() in case of error.
 */
#[cfg(feature = "tree")]
pub unsafe extern "C" fn xml_copy_element_table(table: XmlElementTablePtr) -> XmlElementTablePtr {
    xml_hash_copy(table, Some(xml_copy_element)) as XmlElementTablePtr
}

extern "C" fn xml_free_element_table_entry(elem: *mut c_void, _name: *const XmlChar) {
    unsafe {
        xml_free_element(elem as XmlElementPtr);
    }
}

/**
 * xmlFreeElementTable:
 * @table:  An element table
 *
 * Deallocate the memory used by an element hash table.
 */
pub unsafe extern "C" fn xml_free_element_table(table: XmlElementTablePtr) {
    xml_hash_free(table, Some(xml_free_element_table_entry));
}

/**
 * xmlDumpElementDeclScan:
 * @elem:  An element table
 * @buf:  the XML buffer output
 *
 * This routine is used by the hash scan function.  It just reverses
 * the arguments.
 */
#[cfg(feature = "output")]
extern "C" fn xml_dump_element_decl_scan(
    elem: *mut c_void,
    buf: *mut c_void,
    _name: *const XmlChar,
) {
    unsafe {
        xml_dump_element_decl(buf as XmlBufPtr, elem as XmlElementPtr);
    }
}

/**
 * xmlDumpElementTable:
 * @buf:  the XML buffer output
 * @table:  An element table
 *
 * This will dump the content of the element table as an XML DTD definition
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_dump_element_table(buf: XmlBufPtr, table: XmlElementTablePtr) {
    if buf.is_null() || table.is_null() {
        return;
    }
    xml_hash_scan(table, Some(xml_dump_element_decl_scan), buf as _);
}

/**
 * xmlDumpElementOccur:
 * @buf:  An XML buffer
 * @cur:  An element table
 *
 * Dump the occurrence operator of an element.
 */
#[cfg(feature = "output")]
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

/**
 * xmlDumpElementContent:
 * @buf:  An XML buffer
 * @content:  An element table
 *
 * This will dump the content of the element table as an XML DTD definition
 */
#[cfg(feature = "output")]
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
                XmlElementContentType::XmlElementContentPcdata => {
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
                  //     xml_err_valid(
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

/**
 * xmlDumpElementDecl:
 * @buf:  the XML buffer output
 * @elem:  An element table
 *
 * This will dump the content of the element declaration as an XML
 * DTD definition
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_dump_element_decl(buf: XmlBufPtr, elem: XmlElementPtr) {
    use crate::buf::libxml_api::{xml_buf_cat, xml_buf_ccat};

    if buf.is_null() || elem.is_null() {
        return;
    }
    match (*elem).etype {
        XmlElementTypeVal::XmlElementTypeEmpty => {
            xml_buf_ccat(buf, c"<!ELEMENT ".as_ptr() as _);
            if !(*elem).prefix.is_null() {
                xml_buf_cat(buf, (*elem).prefix);
                xml_buf_ccat(buf, c":".as_ptr() as _);
            }
            xml_buf_cat(buf, (*elem).name);
            xml_buf_ccat(buf, c" EMPTY>\n".as_ptr() as _);
        }
        XmlElementTypeVal::XmlElementTypeAny => {
            xml_buf_ccat(buf, c"<!ELEMENT ".as_ptr() as _);
            if !(*elem).prefix.is_null() {
                xml_buf_cat(buf, (*elem).prefix);
                xml_buf_ccat(buf, c":".as_ptr() as _);
            }
            xml_buf_cat(buf, (*elem).name);
            xml_buf_ccat(buf, c" ANY>\n".as_ptr() as _);
        }
        XmlElementTypeVal::XmlElementTypeMixed => {
            xml_buf_ccat(buf, c"<!ELEMENT ".as_ptr() as _);
            if !(*elem).prefix.is_null() {
                xml_buf_cat(buf, (*elem).prefix);
                xml_buf_ccat(buf, c":".as_ptr() as _);
            }
            xml_buf_cat(buf, (*elem).name);
            xml_buf_ccat(buf, c" ".as_ptr() as _);
            xml_dump_element_content(buf, (*elem).content);
            xml_buf_ccat(buf, c">\n".as_ptr() as _);
        }
        XmlElementTypeVal::XmlElementTypeElement => {
            xml_buf_ccat(buf, c"<!ELEMENT ".as_ptr() as _);
            if !(*elem).prefix.is_null() {
                xml_buf_cat(buf, (*elem).prefix);
                xml_buf_ccat(buf, c":".as_ptr() as _);
            }
            xml_buf_cat(buf, (*elem).name);
            xml_buf_ccat(buf, c" ".as_ptr() as _);
            xml_dump_element_content(buf, (*elem).content);
            xml_buf_ccat(buf, c">\n".as_ptr() as _);
        }
        _ => {
            xml_err_valid(
                null_mut(),
                XmlParserErrors::XmlErrInternalError,
                c"Internal: ELEMENT struct corrupted invalid type\n".as_ptr() as _,
                null_mut(),
            );
        }
    }
}

/* Enumeration */
/**
 * xmlCreateEnumeration:
 * @name:  the enumeration name or null_mut()
 *
 * create and initialize an enumeration attribute node.
 *
 * Returns the xmlEnumerationPtr just created or null_mut() in case
 *                of error.
 */
pub unsafe extern "C" fn xml_create_enumeration(name: *const XmlChar) -> XmlEnumerationPtr {
    let ret: XmlEnumerationPtr = xml_malloc(size_of::<XmlEnumeration>()) as XmlEnumerationPtr;
    if ret.is_null() {
        xml_verr_memory(null_mut(), c"malloc failed".as_ptr() as _);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlEnumeration>());

    if !name.is_null() {
        (*ret).name = xml_strdup(name);
    }
    ret
}

/**
 * xmlFreeEnumeration:
 * @cur:  the tree to free.
 *
 * free an enumeration attribute node (recursive).
 */
pub unsafe extern "C" fn xml_free_enumeration(cur: XmlEnumerationPtr) {
    if cur.is_null() {
        return;
    }

    if !(*cur).next.is_null() {
        xml_free_enumeration((*cur).next);
    }

    if !(*cur).name.is_null() {
        xml_free((*cur).name as _);
    }
    xml_free(cur as _);
}

/**
 * xmlCopyEnumeration:
 * @cur:  the tree to copy.
 *
 * Copy an enumeration attribute node (recursive).
 *
 * Returns the xmlEnumerationPtr just created or null_mut() in case
 *                of error.
 */
#[cfg(feature = "tree")]
pub unsafe extern "C" fn xml_copy_enumeration(cur: XmlEnumerationPtr) -> XmlEnumerationPtr {
    if cur.is_null() {
        return null_mut();
    }
    let ret: XmlEnumerationPtr = xml_create_enumeration((*cur).name as *mut XmlChar);
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

#[cfg(feature = "valid")]
unsafe extern "C" fn xml_is_doc_name_start_char(doc: XmlDocPtr, c: c_int) -> c_int {
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
    } else if IS_LETTER!(c as u32) || c == b'_' as i32 || c == b':' as i32 {
        return 1;
    }
    0
}

#[cfg(feature = "valid")]
unsafe extern "C" fn xml_is_doc_name_char(doc: XmlDocPtr, c: c_int) -> c_int {
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
    } else if IS_LETTER!(c as u32)
        || IS_DIGIT!(c as u32)
        || c == b'.' as i32
        || c == b'-' as i32
        || c == b'_' as i32
        || c == b':' as i32
        || IS_COMBINING!(c as u32)
        || IS_EXTENDER!(c as u32)
    {
        return 1;
    }
    0
}

/**
 * xmlValidateNamesValueInternal:
 * @doc:  pointer to the document or null_mut()
 * @value:  an Names value
 *
 * Validate that the given value match Names production
 *
 * returns 1 if valid or 0 otherwise
 */
#[cfg(feature = "valid")]
unsafe extern "C" fn xml_validate_names_value_internal(
    doc: XmlDocPtr,
    value: *const XmlChar,
) -> c_int {
    let mut cur: *const XmlChar;
    let mut val: c_int;
    let mut len: c_int = 0;

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

#[cfg(feature = "valid")]
unsafe extern "C" fn xml_validate_name_value_internal(
    doc: XmlDocPtr,
    value: *const XmlChar,
) -> c_int {
    let mut cur: *const XmlChar;
    let mut val: c_int;
    let mut len: c_int = 0;

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

/**
 * xmlValidateNmtokensValueInternal:
 * @doc:  pointer to the document or null_mut()
 * @value:  an Nmtokens value
 *
 * Validate that the given value match Nmtokens production
 *
 * [ VC: Name Token ]
 *
 * returns 1 if valid or 0 otherwise
 */
#[cfg(feature = "valid")]
unsafe extern "C" fn xml_validate_nmtokens_value_internal(
    doc: XmlDocPtr,
    value: *const XmlChar,
) -> c_int {
    use super::chvalid::xml_is_blank_char;

    let mut cur: *const XmlChar;
    let mut val: c_int;
    let mut len: c_int = 0;

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

/**
 * xmlValidateNmtokenValueInternal:
 * @doc:  pointer to the document or null_mut()
 * @value:  an Nmtoken value
 *
 * Validate that the given value match Nmtoken production
 *
 * [ VC: Name Token ]
 *
 * returns 1 if valid or 0 otherwise
 */
#[cfg(feature = "valid")]
unsafe extern "C" fn xml_validate_nmtoken_value_internal(
    doc: XmlDocPtr,
    value: *const XmlChar,
) -> c_int {
    let mut cur: *const XmlChar;
    let mut val: c_int;
    let mut len: c_int = 0;

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

/**
 * xmlValidateAttributeValueInternal:
 * @doc: the document
 * @type:  an attribute type
 * @value:  an attribute value
 *
 * Validate that the given attribute value match  the proper production
 *
 * returns 1 if valid or 0 otherwise
 */
#[cfg(feature = "valid")]
unsafe extern "C" fn xml_validate_attribute_value_internal(
    doc: XmlDocPtr,
    typ: XmlAttributeType,
    value: *const XmlChar,
) -> c_int {
    match typ {
        XmlAttributeType::XmlAttributeEntities | XmlAttributeType::XmlAttributeIdrefs => {
            return xml_validate_names_value_internal(doc, value);
        }
        XmlAttributeType::XmlAttributeEntity
        | XmlAttributeType::XmlAttributeIdref
        | XmlAttributeType::XmlAttributeId
        | XmlAttributeType::XmlAttributeNotation => {
            return xml_validate_name_value_internal(doc, value);
        }
        XmlAttributeType::XmlAttributeNmtokens | XmlAttributeType::XmlAttributeEnumeration => {
            return xml_validate_nmtokens_value_internal(doc, value);
        }
        XmlAttributeType::XmlAttributeNmtoken => {
            return xml_validate_nmtoken_value_internal(doc, value);
        }
        XmlAttributeType::XmlAttributeCdata => {} // _ => {}
    }
    1
}

/**
 * xmlErrValidWarning:
 * @ctxt:  an XML validation parser context
 * @node:  the node raising the error
 * @error:  the error number
 * @str1:  extra information
 * @str2:  extra information
 * @str3:  extra information
 *
 * Handle a validation error, provide contextual information
 */
unsafe extern "C" fn xml_err_valid_warning(
    ctxt: XmlValidCtxtPtr,
    node: XmlNodePtr,
    error: XmlParserErrors,
    msg: *const c_char,
    str1: *const XmlChar,
    str2: *const XmlChar,
    str3: *const XmlChar,
) {
    let schannel: Option<StructuredError> = None;
    let mut channel: Option<GenericError> = None;
    let mut pctxt: XmlParserCtxtPtr = null_mut();
    let mut data = None;

    if !ctxt.is_null() {
        channel = (*ctxt).warning;
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
        schannel,
        channel,
        data,
        pctxt as _,
        node as _,
        XmlErrorDomain::XmlFromValid,
        error,
        XmlErrorLevel::XmlErrWarning,
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
        (!str3.is_null()).then(|| CStr::from_ptr(str3 as *const i8)
            .to_string_lossy()
            .into_owned()
            .into()),
        0,
        0,
        msg,
        str1,
        str2,
        str3
    );
}

/**
 * xmlFreeAttribute:
 * @elem:  An attribute
 *
 * Deallocate the memory used by an attribute definition
 */
unsafe extern "C" fn xml_free_attribute(attr: XmlAttributePtr) {
    if attr.is_null() {
        return;
    }
    let dict = if !(*attr).doc.is_null() {
        (*(*attr).doc).dict
    } else {
        null_mut()
    };
    xml_unlink_node(attr as XmlNodePtr);
    if !(*attr).tree.is_null() {
        xml_free_enumeration((*attr).tree);
    }
    if !dict.is_null() {
        if !(*attr).elem.is_null() && xml_dict_owns(dict, (*attr).elem) == 0 {
            xml_free((*attr).elem as _);
        }
        if !(*attr).name.is_null() && xml_dict_owns(dict, (*attr).name) == 0 {
            xml_free((*attr).name as _);
        }
        if !(*attr).prefix.is_null() && xml_dict_owns(dict, (*attr).prefix) == 0 {
            xml_free((*attr).prefix as _);
        }
        if !(*attr).default_value.is_null() && xml_dict_owns(dict, (*attr).default_value) == 0 {
            xml_free((*attr).default_value as _);
        }
    } else {
        if !(*attr).elem.is_null() {
            xml_free((*attr).elem as _);
        }
        if !(*attr).name.is_null() {
            xml_free((*attr).name as _);
        }
        if !(*attr).default_value.is_null() {
            xml_free((*attr).default_value as _);
        }
        if !(*attr).prefix.is_null() {
            xml_free((*attr).prefix as _);
        }
    }
    xml_free(attr as _);
}

/**
 * xmlGetDtdElementDesc2:
 * @dtd:  a pointer to the DtD to search
 * @name:  the element name
 * @create:  create an empty description if not found
 *
 * Search the DTD for the description of this element
 *
 * returns the xmlElementPtr if found or null_mut()
 */
unsafe extern "C" fn xml_get_dtd_element_desc2(
    ctxt: XmlValidCtxtPtr,
    dtd: XmlDtdPtr,
    mut name: *const XmlChar,
    create: c_int,
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
            xml_verr_memory(ctxt, c"element table allocation failed".as_ptr() as _);
            return null_mut();
        }
    }
    table = (*dtd).elements as XmlElementTablePtr;

    let uqname: *mut XmlChar = xml_split_qname2(name, addr_of_mut!(prefix));
    if !uqname.is_null() {
        name = uqname;
    }
    cur = xml_hash_lookup2(table, name, prefix) as _;
    if cur.is_null() && create != 0 {
        cur = xml_malloc(size_of::<XmlElement>()) as XmlElementPtr;
        if cur.is_null() {
            xml_verr_memory(ctxt as _, c"malloc failed".as_ptr() as _);
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
        (*cur).typ = XmlElementType::XmlElementDecl;

        /*
         * fill the structure.
         */
        (*cur).name = xml_strdup(name);
        (*cur).prefix = xml_strdup(prefix);
        (*cur).etype = XmlElementTypeVal::XmlElementTypeUndefined;

        if xml_hash_add_entry2(table, name, prefix, cur as _) < 0 {
            xml_verr_memory(ctxt, c"adding entry failed".as_ptr() as _);
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

/**
 * xmlScanIDAttributeDecl:
 * @ctxt:  the validation context
 * @elem:  the element name
 * @err: whether to raise errors here
 *
 * Verify that the element don't have too many ID attributes
 * declared.
 *
 * Returns the number of ID attributes found.
 */
#[cfg(feature = "valid")]
unsafe extern "C" fn xml_scan_id_attribute_decl(
    ctxt: XmlValidCtxtPtr,
    elem: XmlElementPtr,
    err: c_int,
) -> c_int {
    let mut cur: XmlAttributePtr;
    let mut ret: c_int = 0;

    if elem.is_null() {
        return 0;
    }
    cur = (*elem).attributes;
    while !cur.is_null() {
        if matches!((*cur).atype, XmlAttributeType::XmlAttributeId) {
            ret += 1;
            if ret > 1 && err != 0 {
                xml_err_valid_node(
                    ctxt,
                    elem as XmlNodePtr,
                    XmlParserErrors::XmlDTDMultipleID,
                    c"Element %s has too many ID attributes defined : %s\n".as_ptr() as _,
                    (*elem).name,
                    (*cur).name,
                    null_mut(),
                );
            }
        }
        cur = (*cur).nexth;
    }
    ret
}

/* Attribute */
/**
 * xmlAddAttributeDecl:
 * @ctxt:  the validation context
 * @dtd:  pointer to the DTD
 * @elem:  the element name
 * @name:  the attribute name
 * @ns:  the attribute namespace prefix
 * @type:  the attribute type
 * @def:  the attribute default type
 * @defaultValue:  the attribute default value
 * @tree:  if it's an enumeration, the associated list
 *
 * Register a new attribute declaration
 * Note that @tree becomes the ownership of the DTD
 *
 * Returns null_mut() if not new, otherwise the attribute decl
 */
pub unsafe extern "C" fn xml_add_attribute_decl(
    ctxt: XmlValidCtxtPtr,
    dtd: XmlDtdPtr,
    elem: *const XmlChar,
    name: *const XmlChar,
    ns: *const XmlChar,
    typ: XmlAttributeType,
    def: XmlAttributeDefault,
    mut default_value: *const XmlChar,
    tree: XmlEnumerationPtr,
) -> XmlAttributePtr {
    let mut ret: XmlAttributePtr;
    let mut table: XmlAttributeTablePtr;
    let mut dict: XmlDictPtr = null_mut();

    if dtd.is_null() {
        xml_free_enumeration(tree);
        return null_mut();
    }
    if name.is_null() {
        xml_free_enumeration(tree);
        return null_mut();
    }
    if elem.is_null() {
        xml_free_enumeration(tree);
        return null_mut();
    }
    if !(*dtd).doc.is_null() {
        dict = (*(*dtd).doc).dict;
    }

    #[cfg(feature = "valid")]
    {
        /*
         * Check the type and possibly the default value.
         */
        match typ {
            XmlAttributeType::XmlAttributeCdata => {}
            XmlAttributeType::XmlAttributeId => {}
            XmlAttributeType::XmlAttributeIdref => {}
            XmlAttributeType::XmlAttributeIdrefs => {}
            XmlAttributeType::XmlAttributeEntity => {}
            XmlAttributeType::XmlAttributeEntities => {}
            XmlAttributeType::XmlAttributeNmtoken => {}
            XmlAttributeType::XmlAttributeNmtokens => {}
            XmlAttributeType::XmlAttributeEnumeration => {}
            XmlAttributeType::XmlAttributeNotation => {} // _ => {
                                                         //     xml_err_valid(
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
                c"Attribute %s of %s: invalid default value\n".as_ptr() as _,
                elem,
                name,
                default_value,
            );
            default_value = null_mut();
            if !ctxt.is_null() {
                (*ctxt).valid = 0;
            }
        }
    }

    /*
     * Check first that an attribute defined in the external subset wasn't
     * already defined in the internal subset
     */
    if !(*dtd).doc.is_null()
        && (*(*dtd).doc).ext_subset == dtd
        && !(*(*dtd).doc).int_subset.is_null()
        && !(*(*(*dtd).doc).int_subset).attributes.is_null()
    {
        ret = xml_hash_lookup3((*(*(*dtd).doc).int_subset).attributes as _, name, ns, elem) as _;
        if !ret.is_null() {
            xml_free_enumeration(tree);
            return null_mut();
        }
    }

    /*
     * Create the Attribute table if needed.
     */
    table = (*dtd).attributes as XmlAttributeTablePtr;
    if table.is_null() {
        table = xml_hash_create_dict(0, dict);
        (*dtd).attributes = table as *mut c_void;
    }
    if table.is_null() {
        xml_verr_memory(
            ctxt,
            c"xmlAddAttributeDecl: Table creation failed!\n".as_ptr() as _,
        );
        xml_free_enumeration(tree as _);
        return null_mut();
    }

    ret = xml_malloc(size_of::<XmlAttribute>()) as XmlAttributePtr;
    if ret.is_null() {
        xml_verr_memory(ctxt as _, c"malloc failed".as_ptr() as _);
        xml_free_enumeration(tree);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlAttribute>());
    (*ret).typ = XmlElementType::XmlAttributeDecl;

    /*
     * fill the structure.
     */
    (*ret).atype = typ;
    /*
     * doc must be set before possible error causes call
     * to xmlFreeAttribute (because it's used to check on
     * dict use)
     */
    (*ret).doc = (*dtd).doc;
    if !dict.is_null() {
        (*ret).name = xml_dict_lookup(dict, name, -1);
        (*ret).prefix = xml_dict_lookup(dict, ns, -1);
        (*ret).elem = xml_dict_lookup(dict, elem, -1);
    } else {
        (*ret).name = xml_strdup(name);
        (*ret).prefix = xml_strdup(ns);
        (*ret).elem = xml_strdup(elem);
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

    /*
     * Validity Check:
     * Search the DTD for previous declarations of the ATTLIST
     */
    if xml_hash_add_entry3(table, (*ret).name, (*ret).prefix, (*ret).elem, ret as _) < 0 {
        #[cfg(feature = "valid")]
        {
            /*
             * The attribute is already defined in this DTD.
             */
            xml_err_valid_warning(
                ctxt,
                dtd as XmlNodePtr,
                XmlParserErrors::XmlDTDAttributeRedefined,
                c"Attribute %s of element %s: already defined\n".as_ptr() as _,
                name,
                elem,
                null_mut(),
            );
        }
        xml_free_attribute(ret);
        return null_mut();
    }

    /*
     * Validity Check:
     * Multiple ID per element
     */
    let elem_def: XmlElementPtr = xml_get_dtd_element_desc2(ctxt, dtd, elem, 1);
    if !elem_def.is_null() {
        #[cfg(feature = "valid")]
        {
            if matches!(typ, XmlAttributeType::XmlAttributeId)
                && xml_scan_id_attribute_decl(null_mut(), elem_def, 1) != 0
            {
                xml_err_valid_node(
                    ctxt,
                    dtd as XmlNodePtr,
                    XmlParserErrors::XmlDTDMultipleID,
                    c"Element %s has too may ID attributes defined : %s\n".as_ptr() as _,
                    elem,
                    name,
                    null_mut(),
                );
                if !ctxt.is_null() {
                    (*ctxt).valid = 0;
                }
            }
        }

        /*
         * Insert namespace default def first they need to be
         * processed first.
         */
        if xml_str_equal((*ret).name, c"xmlns".as_ptr() as _)
            || (!(*ret).prefix.is_null() && xml_str_equal((*ret).prefix, c"xmlns".as_ptr() as _))
        {
            (*ret).nexth = (*elem_def).attributes;
            (*elem_def).attributes = ret;
        } else {
            let mut tmp: XmlAttributePtr = (*elem_def).attributes;

            while !tmp.is_null()
                && (xml_str_equal((*tmp).name, c"xmlns".as_ptr() as _)
                    || (!(*ret).prefix.is_null()
                        && xml_str_equal((*ret).prefix, c"xmlns".as_ptr() as _)))
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

    /*
     * Link it to the DTD
     */
    (*ret).parent = dtd;
    if (*dtd).last.is_null() {
        (*dtd).children = ret as XmlNodePtr;
        (*dtd).last = ret as XmlNodePtr;
    } else {
        (*(*dtd).last).next = ret as XmlNodePtr;
        (*ret).prev = (*dtd).last;
        (*dtd).last = ret as XmlNodePtr;
    }
    ret
}

/**
 * xmlCopyAttribute:
 * @attr:  An attribute
 *
 * Build a copy of an attribute.
 *
 * Returns the new xmlAttributePtr or null_mut() in case of error.
 */
#[cfg(feature = "tree")]
extern "C" fn xml_copy_attribute(payload: *mut c_void, _name: *const XmlChar) -> *mut c_void {
    let attr: XmlAttributePtr = payload as XmlAttributePtr;

    unsafe {
        let cur: XmlAttributePtr = xml_malloc(size_of::<XmlAttribute>()) as XmlAttributePtr;
        if cur.is_null() {
            xml_verr_memory(null_mut(), c"malloc failed".as_ptr() as _);
            return null_mut();
        }
        memset(cur as _, 0, size_of::<XmlAttribute>());
        (*cur).typ = XmlElementType::XmlAttributeDecl;
        (*cur).atype = (*attr).atype;
        (*cur).def = (*attr).def;
        (*cur).tree = xml_copy_enumeration((*attr).tree);
        if !(*attr).elem.is_null() {
            (*cur).elem = xml_strdup((*attr).elem);
        }
        if !(*attr).name.is_null() {
            (*cur).name = xml_strdup((*attr).name);
        }
        if !(*attr).prefix.is_null() {
            (*cur).prefix = xml_strdup((*attr).prefix);
        }
        if !(*attr).default_value.is_null() {
            (*cur).default_value = xml_strdup((*attr).default_value);
        }
        cur as _
    }
}

/**
 * xmlCopyAttributeTable:
 * @table:  An attribute table
 *
 * Build a copy of an attribute table.
 *
 * Returns the new xmlAttributeTablePtr or null_mut() in case of error.
 */
#[cfg(feature = "tree")]
pub unsafe extern "C" fn xml_copy_attribute_table(
    table: XmlAttributeTablePtr,
) -> XmlAttributeTablePtr {
    xml_hash_copy(table, Some(xml_copy_attribute)) as XmlAttributeTablePtr
}

extern "C" fn xml_free_attribute_table_entry(attr: *mut c_void, _name: *const XmlChar) {
    unsafe {
        xml_free_attribute(attr as XmlAttributePtr);
    }
}

/**
 * xmlFreeAttributeTable:
 * @table:  An attribute table
 *
 * Deallocate the memory used by an entities hash table.
 */
pub unsafe extern "C" fn xml_free_attribute_table(table: XmlAttributeTablePtr) {
    xml_hash_free(table, Some(xml_free_attribute_table_entry));
}

/**
 * xmlDumpAttributeDeclScan:
 * @attr:  An attribute declaration
 * @buf:  the XML buffer output
 *
 * This is used with the hash scan function - just reverses arguments
 */
#[cfg(feature = "output")]
extern "C" fn xml_dump_attribute_decl_scan(
    attr: *mut c_void,
    buf: *mut c_void,
    _name: *const XmlChar,
) {
    unsafe {
        xml_dump_attribute_decl(buf as XmlBufPtr, attr as XmlAttributePtr);
    }
}

/**
 * xmlDumpAttributeTable:
 * @buf:  the XML buffer output
 * @table:  An attribute table
 *
 * This will dump the content of the attribute table as an XML DTD definition
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_dump_attribute_table(buf: XmlBufPtr, table: XmlAttributeTablePtr) {
    if buf.is_null() || table.is_null() {
        return;
    }
    xml_hash_scan(table, Some(xml_dump_attribute_decl_scan), buf as _);
}

/**
 * xmlDumpEnumeration:
 * @buf:  the XML buffer output
 * @enum:  An enumeration
 *
 * This will dump the content of the enumeration
 */
#[cfg(feature = "output")]
unsafe extern "C" fn xml_dump_enumeration(buf: XmlBufPtr, cur: XmlEnumerationPtr) {
    use crate::buf::libxml_api::{xml_buf_cat, xml_buf_ccat};

    if buf.is_null() || cur.is_null() {
        return;
    }

    xml_buf_cat(buf, (*cur).name);
    if (*cur).next.is_null() {
        xml_buf_ccat(buf, c")".as_ptr() as _);
    } else {
        xml_buf_ccat(buf, c" | ".as_ptr() as _);
        xml_dump_enumeration(buf, (*cur).next);
    }
}

/**
 * xmlDumpAttributeDecl:
 * @buf:  the XML buffer output
 * @attr:  An attribute declaration
 *
 * This will dump the content of the attribute declaration as an XML
 * DTD definition
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_dump_attribute_decl(buf: XmlBufPtr, attr: XmlAttributePtr) {
    use crate::buf::libxml_api::{xml_buf_cat, xml_buf_ccat, xml_buf_write_quoted_string};

    if buf.is_null() || attr.is_null() {
        return;
    }
    xml_buf_ccat(buf, c"<!ATTLIST ".as_ptr() as _);
    xml_buf_cat(buf, (*attr).elem);
    xml_buf_ccat(buf, c" ".as_ptr() as _);
    if !(*attr).prefix.is_null() {
        xml_buf_cat(buf, (*attr).prefix);
        xml_buf_ccat(buf, c":".as_ptr() as _);
    }
    xml_buf_cat(buf, (*attr).name);
    match (*attr).atype {
        XmlAttributeType::XmlAttributeCdata => {
            xml_buf_ccat(buf, c" CDATA".as_ptr() as _);
        }
        XmlAttributeType::XmlAttributeId => {
            xml_buf_ccat(buf, c" ID".as_ptr() as _);
        }
        XmlAttributeType::XmlAttributeIdref => {
            xml_buf_ccat(buf, c" IDREF".as_ptr() as _);
        }
        XmlAttributeType::XmlAttributeIdrefs => {
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
          //     xml_err_valid(
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
          //     xml_err_valid(
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

unsafe extern "C" fn xml_is_streaming(ctxt: XmlValidCtxtPtr) -> c_int {
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

/**
 * DICT_FREE:
 * @str:  a string
 *
 * Free a string if it is not owned by the "dict" dictionary in the
 * current scope
 */
macro_rules! DICT_FREE {
    ($str:expr, $dict:expr) => {
        if !$str.is_null() && ($dict.is_null() || xml_dict_owns($dict, $str as *const XmlChar) == 0)
        {
            xml_free($str as _);
        }
    };
}

/**
 * xmlFreeID:
 * @not:  A id
 *
 * Deallocate the memory used by an id definition
 */
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

/* IDs */
/**
 * xmlAddID:
 * @ctxt:  the validation context
 * @doc:  pointer to the document
 * @value:  the value name
 * @attr:  the attribute holding the ID
 *
 * Register a new id declaration
 *
 * Returns null_mut() if not, otherwise the new xmlIDPtr
 */
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
        xml_verr_memory(ctxt, c"xmlAddID: Table creation failed!\n".as_ptr() as _);
        return null_mut();
    }

    let ret: XmlIDPtr = xml_malloc(size_of::<XmlID>()) as XmlIDPtr;
    if ret.is_null() {
        xml_verr_memory(ctxt as _, c"malloc failed".as_ptr() as _);
        return null_mut();
    }

    /*
     * fill the structure.
     */
    (*ret).value = xml_strdup(value);
    (*ret).doc = doc;
    if xml_is_streaming(ctxt) != 0 {
        /*
         * Operating in streaming mode, attr is gonna disappear
         */
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
    (*ret).lineno = xml_get_line_no((*attr).parent) as _;

    if xml_hash_add_entry(table, value, ret as _) < 0 {
        /*
         * The id is already defined in this DTD.
         */
        #[cfg(feature = "valid")]
        if !ctxt.is_null() {
            xml_err_valid_node(
                ctxt,
                (*attr).parent,
                XmlParserErrors::XmlDTDIDRedefined,
                c"ID %s already defined\n".as_ptr() as _,
                value,
                null_mut(),
                null_mut(),
            );
        }
        xml_free_id(ret);
        return null_mut();
    }
    if !attr.is_null() {
        (*attr).atype = Some(XmlAttributeType::XmlAttributeId);
    }
    ret
}

extern "C" fn xml_free_id_table_entry(id: *mut c_void, _name: *const XmlChar) {
    unsafe {
        xml_free_id(id as XmlIDPtr);
    }
}

/**
 * xmlFreeIDTable:
 * @table:  An id table
 *
 * Deallocate the memory used by an ID hash table.
 */
pub unsafe extern "C" fn xml_free_id_table(table: XmlIDTablePtr) {
    xml_hash_free(table, Some(xml_free_id_table_entry));
}

/**
 * xmlGetID:
 * @doc:  pointer to the document
 * @ID:  the ID value
 *
 * Search the attribute declaring the given ID
 *
 * Returns null_mut() if not found, otherwise the xmlAttrPtr defining the ID
 */
pub unsafe extern "C" fn xml_get_id(doc: XmlDocPtr, id: *const XmlChar) -> XmlAttrPtr {
    if doc.is_null() {
        return null_mut();
    }

    if id.is_null() {
        return null_mut();
    }

    let table: XmlIDTablePtr = (*doc).ids as XmlIDTablePtr;
    if table.is_null() {
        return null_mut();
    }

    let id_ptr: XmlIDPtr = xml_hash_lookup(table, id) as _;
    if id_ptr.is_null() {
        return null_mut();
    }
    if (*id_ptr).attr.is_null() {
        /*
         * We are operating on a stream, return a well known reference
         * since the attribute node doesn't exist anymore
         */
        return doc as XmlAttrPtr;
    }
    (*id_ptr).attr
}

/**
 * xmlIsID:
 * @doc:  the document
 * @elem:  the element carrying the attribute
 * @attr:  the attribute
 *
 * Determine whether an attribute is of type ID. In case we have DTD(s)
 * then this is done if DTD loading has been requested. In the case
 * of HTML documents parsed with the HTML parser, then ID detection is
 * done systematically.
 *
 * Returns 0 or 1 depending on the lookup result
 */
pub unsafe extern "C" fn xml_is_id(doc: XmlDocPtr, elem: XmlNodePtr, attr: XmlAttrPtr) -> c_int {
    if attr.is_null() || (*attr).name.is_null() {
        return 0;
    }
    if !(*attr).ns.is_null()
        && !(*(*attr).ns).prefix.load(Ordering::Relaxed).is_null()
        && strcmp((*attr).name as _, c"id".as_ptr() as _) == 0
        && strcmp(
            (*(*attr).ns).prefix.load(Ordering::Acquire) as _,
            c"xml".as_ptr() as _,
        ) == 0
    {
        return 1;
    }
    if doc.is_null() {
        return 0;
    }
    if (*doc).int_subset.is_null()
        && (*doc).ext_subset.is_null()
        && !matches!((*doc).typ, XmlElementType::XmlHtmlDocumentNode)
    {
        return 0;
    } else if matches!((*doc).typ, XmlElementType::XmlHtmlDocumentNode) {
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

        let fullelemname: *mut XmlChar =
            if !(*elem).ns.is_null() && !(*(*elem).ns).prefix.load(Ordering::Relaxed).is_null() {
                xml_build_qname(
                    (*elem).name,
                    (*(*elem).ns).prefix.load(Ordering::Relaxed),
                    felem.as_ptr() as _,
                    50,
                )
            } else {
                (*elem).name as *mut XmlChar
            };

        let fullattrname: *mut XmlChar =
            if !(*attr).ns.is_null() && !(*(*attr).ns).prefix.load(Ordering::Relaxed).is_null() {
                xml_build_qname(
                    (*attr).name,
                    (*(*attr).ns).prefix.load(Ordering::Relaxed),
                    fattr.as_ptr() as _,
                    50,
                )
            } else {
                (*attr).name as *mut XmlChar
            };

        if !fullelemname.is_null() && !fullattrname.is_null() {
            attr_decl = xml_get_dtd_attr_desc((*doc).int_subset, fullelemname, fullattrname);
            if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                attr_decl = xml_get_dtd_attr_desc((*doc).ext_subset, fullelemname, fullattrname);
            }
        }

        if fullattrname != fattr.as_ptr() as _ && fullattrname != (*attr).name as _ {
            xml_free(fullattrname as _);
        }
        if fullelemname != felem.as_ptr() as _ && fullelemname != (*elem).name as _ {
            xml_free(fullelemname as _);
        }

        if !attr_decl.is_null() && matches!((*attr_decl).atype, XmlAttributeType::XmlAttributeId) {
            return 1;
        }
    }
    0
}

/**
 * xmlValidNormalizeString:
 * @str: a string
 *
 * Normalize a string in-place.
 */
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

/**
 * xmlRemoveID:
 * @doc:  the document
 * @attr:  the attribute
 *
 * Remove the given attribute from the ID table maintained internally.
 *
 * Returns -1 if the lookup failed and 0 otherwise
 */
pub unsafe extern "C" fn xml_remove_id(doc: XmlDocPtr, attr: XmlAttrPtr) -> c_int {
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

    let id: *mut XmlChar = xml_node_list_get_string(doc, (*attr).children, 1);
    if id.is_null() {
        return -1;
    }
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

/**
 * xmlFreeRef:
 * @lk:  A list link
 *
 * Deallocate the memory used by a ref definition
 */
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

/**
 * xmlDummyCompare
 * @data0:  Value supplied by the user
 * @data1:  Value supplied by the user
 *
 * Do nothing, return 0. Used to create unordered lists.
 */
extern "C" fn xml_dummy_compare(_data0: *const c_void, _data1: *const c_void) -> c_int {
    0
}

/* IDREFs */
/**
 * xmlAddRef:
 * @ctxt:  the validation context
 * @doc:  pointer to the document
 * @value:  the value name
 * @attr:  the attribute holding the Ref
 *
 * DEPRECATED, do not use. This function will be removed from the public API.
 *
 * Register a new ref declaration
 *
 * Returns null_mut() if not, otherwise the new xmlRefPtr
 */
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

    /*
     * Create the Ref table if needed.
     */
    table = (*doc).refs as XmlRefTablePtr;
    if table.is_null() {
        (*doc).refs = xml_hash_create_dict(0, (*doc).dict) as _;
        table = (*doc).refs as _;
    }
    if table.is_null() {
        xml_verr_memory(ctxt, c"xmlAddRef: Table creation failed!\n".as_ptr() as _);
        return null_mut();
    }

    let ret: XmlRefPtr = xml_malloc(size_of::<XmlRef>()) as XmlRefPtr;
    if ret.is_null() {
        xml_verr_memory(ctxt as _, c"malloc failed".as_ptr() as _);
        return null_mut();
    }

    /*
     * fill the structure.
     */
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
    (*ret).lineno = xml_get_line_no((*attr).parent) as _;

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
                xml_err_valid(
                    null_mut(),
                    XmlParserErrors::XmlErrInternalError,
                    c"xmlAddRef: Reference list creation failed!\n".as_ptr() as _,
                    null_mut(),
                );
                break 'failed;
            }
            if xml_hash_add_entry(table, value, ref_list as _) < 0 {
                xml_list_delete(ref_list);
                xml_err_valid(
                    null_mut(),
                    XmlParserErrors::XmlErrInternalError,
                    c"xmlAddRef: Reference list insertion failed!\n".as_ptr() as _,
                    null_mut(),
                );
                break 'failed;
            }
        }
        if xml_list_append(ref_list, ret as _) != 0 {
            xml_err_valid(
                null_mut(),
                XmlParserErrors::XmlErrInternalError,
                c"xmlAddRef: Reference list insertion failed!\n".as_ptr() as _,
                null_mut(),
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

/**
 * xmlFreeRefTableEntry:
 * @list_ref:  A list of references.
 *
 * Deallocate the memory used by a list of references
 */
extern "C" fn xml_free_ref_table_entry(payload: *mut c_void, _name: *const XmlChar) {
    let list_ref: XmlListPtr = payload as XmlListPtr;
    if list_ref.is_null() {
        return;
    }
    xml_list_delete(list_ref);
}

/**
 * xmlFreeRefTable:
 * @table:  An ref table
 *
 * DEPRECATED, do not use. This function will be removed from the public API.
 *
 * Deallocate the memory used by an Ref hash table.
 */
pub(crate) unsafe extern "C" fn xml_free_ref_table(table: XmlRefTablePtr) {
    xml_hash_free(table, Some(xml_free_ref_table_entry));
}

/**
 * xmlIsRef:
 * @doc:  the document
 * @elem:  the element carrying the attribute
 * @attr:  the attribute
 *
 * DEPRECATED, do not use. This function will be removed from the public API.
 *
 * Determine whether an attribute is of type Ref. In case we have DTD(s)
 * then this is simple, otherwise we use an heuristic: name Ref (upper
 * or lowercase).
 *
 * Returns 0 or 1 depending on the lookup result
 */
pub(crate) unsafe extern "C" fn xml_is_ref(
    mut doc: XmlDocPtr,
    elem: XmlNodePtr,
    attr: XmlAttrPtr,
) -> c_int {
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
    } else if matches!((*doc).typ, XmlElementType::XmlHtmlDocumentNode) {
        /* TODO @@@ */
        return 0;
    } else {
        let mut attr_decl: XmlAttributePtr;

        if elem.is_null() {
            return 0;
        }
        attr_decl = xml_get_dtd_attr_desc((*doc).int_subset, (*elem).name, (*attr).name);
        if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
            attr_decl = xml_get_dtd_attr_desc((*doc).ext_subset, (*elem).name, (*attr).name);
        }

        if !attr_decl.is_null()
            && matches!(
                (*attr_decl).atype,
                XmlAttributeType::XmlAttributeIdref | XmlAttributeType::XmlAttributeIdrefs
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

/**
 * xmlWalkRemoveRef:
 * @data:  Contents of current link
 * @user:  Value supplied by the user
 *
 * Returns 0 to abort the walk or 1 to continue
 */
extern "C" fn xml_walk_remove_ref(data: *const c_void, user: *mut c_void) -> c_int {
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

/**
 * xmlRemoveRef:
 * @doc:  the document
 * @attr:  the attribute
 *
 * DEPRECATED, do not use. This function will be removed from the public API.
 *
 * Remove the given attribute from the Ref table maintained internally.
 *
 * Returns -1 if the lookup failed and 0 otherwise
 */
pub(crate) unsafe extern "C" fn xml_remove_ref(doc: XmlDocPtr, attr: XmlAttrPtr) -> c_int {
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

    let id: *mut XmlChar = xml_node_list_get_string(doc, (*attr).children, 1);
    if id.is_null() {
        return -1;
    }

    let ref_list: XmlListPtr = xml_hash_lookup(table, id) as _;
    if ref_list.is_null() {
        xml_free(id as _);
        return -1;
    }

    /* At this point, ref_list refers to a list of references which
     * have the same key as the supplied attr. Our list of references
     * is ordered by reference address and we don't have that information
     * here to use when removing. We'll have to walk the list and
     * check for a matching attribute, when we find one stop the walk
     * and remove the entry.
     * The list is ordered by reference, so that means we don't have the
     * key. Passing the list and the reference to the walker means we
     * will have enough data to be able to remove the entry.
     */
    target.l = ref_list;
    target.ap = attr;

    /* Remove the supplied attr from our list */
    xml_list_walk(
        ref_list,
        Some(xml_walk_remove_ref),
        addr_of_mut!(target) as _,
    );

    /*If the list is empty then remove the list entry in the hash */
    if xml_list_empty(ref_list) != 0 {
        xml_hash_update_entry(table, id, null_mut(), Some(xml_free_ref_table_entry));
    }
    xml_free(id as _);
    0
}

/**
 * xmlGetRefs:
 * @doc:  pointer to the document
 * @ID:  the ID value
 *
 * DEPRECATED, do not use. This function will be removed from the public API.
 *
 * Find the set of references for the supplied ID.
 *
 * Returns null_mut() if not found, otherwise node set for the ID.
 */
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

/**
 * The public function calls related to validity checking.
 */
/* Allocate/Release Validation Contexts */
/**
 * xmlNewValidCtxt:
 *
 * Allocate a validation context structure.
 *
 * Returns null_mut() if not, otherwise the new validation context structure
 */
#[cfg(feature = "valid")]
pub unsafe extern "C" fn xml_new_valid_ctxt() -> XmlValidCtxtPtr {
    let ret: XmlValidCtxtPtr = xml_malloc(size_of::<XmlValidCtxt>()) as _;
    if ret.is_null() {
        xml_verr_memory(null_mut(), c"malloc failed".as_ptr() as _);
        return null_mut();
    }

    memset(ret as _, 0, size_of::<XmlValidCtxt>());

    ret
}

/**
 * xmlFreeValidCtxt:
 * @cur:  the validation context to free
 *
 * Free a validation context structure.
 */
#[cfg(feature = "valid")]
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

/**
 * xmlValidateRoot:
 * @ctxt:  the validation context
 * @doc:  a document instance
 *
 * Try to validate a the root element
 * basically it does the following check as described by the
 * XML-1.0 recommendation:
 *  - [ VC: Root Element Type ]
 *    it doesn't try to recurse or apply other check to the element
 *
 * returns 1 if valid or 0 otherwise
 */
#[cfg(feature = "valid")]
pub unsafe extern "C" fn xml_validate_root(ctxt: XmlValidCtxtPtr, doc: XmlDocPtr) -> c_int {
    let ret: c_int;

    if doc.is_null() {
        return 0;
    }

    let root: XmlNodePtr = xml_doc_get_root_element(doc);
    if root.is_null() || (*root).name.is_null() {
        xml_err_valid(
            ctxt,
            XmlParserErrors::XmlDTDNoRoot,
            c"no root element\n".as_ptr() as _,
            null_mut(),
        );
        return 0;
    }

    /*
     * When doing post validation against a separate DTD, those may
     * no internal subset has been generated
     */
    if !(*doc).int_subset.is_null() && !(*(*doc).int_subset).name.is_null() {
        /*
         * Check first the document root against the NQName
         */
        if !xml_str_equal((*(*doc).int_subset).name, (*root).name) {
            if !(*root).ns.is_null() && !(*(*root).ns).prefix.load(Ordering::Relaxed).is_null() {
                let mut fname: [XmlChar; 50] = [0; 50];

                let fullname: *mut XmlChar = xml_build_qname(
                    (*root).name,
                    (*(*root).ns).prefix.load(Ordering::Relaxed) as _,
                    fname.as_mut_ptr(),
                    50,
                );
                if fullname.is_null() {
                    xml_verr_memory(ctxt, null_mut());
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

            xml_err_valid_node(
                ctxt,
                root,
                XmlParserErrors::XmlDTDRootName,
                c"root and DTD name do not match '%s' and '%s'\n".as_ptr() as _,
                (*root).name,
                (*(*doc).int_subset).name,
                null_mut(),
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

/**
 * xmlValidateElementDecl:
 * @ctxt:  the validation context
 * @doc:  a document instance
 * @elem:  an element definition
 *
 * Try to validate a single element definition
 * basically it does the following checks as described by the
 * XML-1.0 recommendation:
 *  - [ VC: One ID per Element Type ]
 *  - [ VC: No Duplicate Types ]
 *  - [ VC: Unique Element Type Declaration ]
 *
 * returns 1 if valid or 0 otherwise
 */
#[cfg(feature = "valid")]
pub unsafe extern "C" fn xml_validate_element_decl(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    elem: XmlElementPtr,
) -> c_int {
    let mut ret: c_int = 1;
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
                            if (*(*cur).c1).prefix.is_null() {
                                xml_err_valid_node(
                                    ctxt,
                                    elem as XmlNodePtr,
                                    XmlParserErrors::XmlDTDContentError,
                                    c"Definition of %s has duplicate references of %s\n".as_ptr()
                                        as _,
                                    (*elem).name,
                                    name,
                                    null_mut(),
                                );
                            } else {
                                xml_err_valid_node(
                                    ctxt,
                                    elem as XmlNodePtr,
                                    XmlParserErrors::XmlDTDContentError,
                                    c"Definition of %s has duplicate references of %s:%s\n".as_ptr()
                                        as _,
                                    (*elem).name,
                                    (*(*cur).c1).prefix,
                                    name,
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
                        if (*(*cur).c1).prefix.is_null() {
                            xml_err_valid_node(
                                ctxt,
                                elem as XmlNodePtr,
                                XmlParserErrors::XmlDTDContentError,
                                c"Definition of %s has duplicate references to %s\n".as_ptr() as _,
                                (*elem).name,
                                name,
                                null_mut(),
                            );
                        } else {
                            xml_err_valid_node(
                                ctxt,
                                elem as XmlNodePtr,
                                XmlParserErrors::XmlDTDContentError,
                                c"Definition of %s has duplicate references to %s:%s\n".as_ptr()
                                    as _,
                                (*elem).name,
                                (*(*cur).c1).prefix,
                                name,
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

    /* VC: Unique Element Type Declaration */
    tst = xml_get_dtd_element_desc((*doc).int_subset, (*elem).name);
    if !tst.is_null()
        && tst != elem
        && ((*tst).prefix == (*elem).prefix || xml_str_equal((*tst).prefix, (*elem).prefix))
        && !matches!((*tst).etype, XmlElementTypeVal::XmlElementTypeUndefined)
    {
        xml_err_valid_node(
            ctxt,
            elem as XmlNodePtr,
            XmlParserErrors::XmlDTDElemRedefined,
            c"Redefinition of element %s\n".as_ptr() as _,
            (*elem).name,
            null_mut(),
            null_mut(),
        );
        ret = 0;
    }
    tst = xml_get_dtd_element_desc((*doc).ext_subset, (*elem).name);
    if !tst.is_null()
        && tst != elem
        && ((*tst).prefix == (*elem).prefix || xml_str_equal((*tst).prefix, (*elem).prefix))
        && !matches!((*tst).etype, XmlElementTypeVal::XmlElementTypeUndefined)
    {
        xml_err_valid_node(
            ctxt,
            elem as XmlNodePtr,
            XmlParserErrors::XmlDTDElemRedefined,
            c"Redefinition of element %s\n".as_ptr() as _,
            (*elem).name,
            null_mut(),
            null_mut(),
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

/**
 * xmlValidNormalizeAttributeValue:
 * @doc:  the document
 * @elem:  the parent
 * @name:  the attribute name
 * @value:  the attribute value
 *
 * Does the validation related extra step of the normalization of attribute
 * values:
 *
 * If the declared value is not CDATA, then the XML processor must further
 * process the normalized attribute value by discarding any leading and
 * trailing space (#x20) characters, and by replacing sequences of space
 * (#x20) characters by single space (#x20) character.
 *
 * Returns a new normalized string if normalization is needed, null_mut() otherwise
 *      the caller must free the returned value.
 */
#[cfg(feature = "valid")]
pub unsafe extern "C" fn xml_valid_normalize_attribute_value(
    doc: XmlDocPtr,
    elem: XmlNodePtr,
    name: *const XmlChar,
    value: *const XmlChar,
) -> *mut XmlChar {
    let mut attr_decl: XmlAttributePtr;

    if doc.is_null() {
        return null_mut();
    }
    if elem.is_null() {
        return null_mut();
    }
    if name.is_null() {
        return null_mut();
    }
    if value.is_null() {
        return null_mut();
    }

    if !(*elem).ns.is_null() && !(*(*elem).ns).prefix.load(Ordering::Relaxed).is_null() {
        let mut fname: [XmlChar; 50] = [0; 50];

        let fullname: *mut XmlChar = xml_build_qname(
            (*elem).name,
            (*(*elem).ns).prefix.load(Ordering::Relaxed) as _,
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
    attr_decl = xml_get_dtd_attr_desc((*doc).int_subset, (*elem).name, name);
    if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
        attr_decl = xml_get_dtd_attr_desc((*doc).ext_subset, (*elem).name, name);
    }

    if attr_decl.is_null() {
        return null_mut();
    }
    if matches!((*attr_decl).atype, XmlAttributeType::XmlAttributeCdata) {
        return null_mut();
    }

    let ret: *mut XmlChar = xml_strdup(value);
    if ret.is_null() {
        return null_mut();
    }
    xml_valid_normalize_string(ret);
    ret
}

/**
 * xmlValidCtxtNormalizeAttributeValue:
 * @ctxt: the validation context
 * @doc:  the document
 * @elem:  the parent
 * @name:  the attribute name
 * @value:  the attribute value
 * @ctxt:  the validation context or null_mut()
 *
 * Does the validation related extra step of the normalization of attribute
 * values:
 *
 * If the declared value is not CDATA, then the XML processor must further
 * process the normalized attribute value by discarding any leading and
 * trailing space (#x20) characters, and by replacing sequences of space
 * (#x20) characters by single space (#x20) character.
 *
 * Also  check VC: Standalone Document Declaration in P32, and update
 *  (*ctxt).valid accordingly
 *
 * returns a new normalized string if normalization is needed, null_mut() otherwise
 *      the caller must free the returned value.
 */
#[cfg(feature = "valid")]
pub unsafe extern "C" fn xml_valid_ctxt_normalize_attribute_value(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    elem: XmlNodePtr,
    name: *const XmlChar,
    value: *const XmlChar,
) -> *mut XmlChar {
    let mut attr_decl: XmlAttributePtr = null_mut();
    let mut extsubset: c_int = 0;

    if doc.is_null() {
        return null_mut();
    }
    if elem.is_null() {
        return null_mut();
    }
    if name.is_null() {
        return null_mut();
    }
    if value.is_null() {
        return null_mut();
    }

    if !(*elem).ns.is_null() && !(*(*elem).ns).prefix.load(Ordering::Relaxed).is_null() {
        let mut fname: [XmlChar; 50] = [0; 50];

        let fullname: *mut XmlChar = xml_build_qname(
            (*elem).name,
            (*(*elem).ns).prefix.load(Ordering::Relaxed) as _,
            fname.as_mut_ptr(),
            50,
        );
        if fullname.is_null() {
            return null_mut();
        }
        attr_decl = xml_get_dtd_attr_desc((*doc).int_subset, fullname, name);
        if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
            attr_decl = xml_get_dtd_attr_desc((*doc).ext_subset, fullname, name);
            if !attr_decl.is_null() {
                extsubset = 1;
            }
        }
        if fullname != fname.as_ptr() as _ && fullname != (*elem).name as _ {
            xml_free(fullname as _);
        }
    }
    if attr_decl.is_null() && !(*doc).int_subset.is_null() {
        attr_decl = xml_get_dtd_attr_desc((*doc).int_subset, (*elem).name, name);
    }
    if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
        attr_decl = xml_get_dtd_attr_desc((*doc).ext_subset, (*elem).name, name);
        if !attr_decl.is_null() {
            extsubset = 1;
        }
    }

    if attr_decl.is_null() {
        return null_mut();
    }
    if matches!((*attr_decl).atype, XmlAttributeType::XmlAttributeCdata) {
        return null_mut();
    }

    let ret: *mut XmlChar = xml_strdup(value);
    if ret.is_null() {
        return null_mut();
    }
    xml_valid_normalize_string(ret);
    if (*doc).standalone != 0 && extsubset == 1 && !xml_str_equal(value, ret) {
        xml_err_valid_node(ctxt, elem, XmlParserErrors::XmlDTDNotStandalone,
c"standalone: %s on %s value had to be normalized based on external subset declaration\n".as_ptr() as _,
	       name, (*elem).name, null_mut());
        (*ctxt).valid = 0;
    }
    ret
}

extern "C" fn xml_validate_attribute_id_callback(
    payload: *mut c_void,
    data: *mut c_void,
    _name: *const XmlChar,
) {
    let attr: XmlAttributePtr = payload as XmlAttributePtr;
    let count: *mut c_int = data as *mut c_int;
    unsafe {
        if matches!((*attr).atype, XmlAttributeType::XmlAttributeId) {
            (*count) += 1;
        }
    }
}

/**
 * xmlErrValidNodeNr:
 * @ctxt:  an XML validation parser context
 * @node:  the node raising the error
 * @error:  the error number
 * @str1:  extra information
 * @int2:  extra information
 * @str3:  extra information
 *
 * Handle a validation error, provide contextual information
 */
unsafe extern "C" fn xml_err_valid_node_nr(
    ctxt: XmlValidCtxtPtr,
    node: XmlNodePtr,
    error: XmlParserErrors,
    msg: *const c_char,
    str1: *const XmlChar,
    int2: c_int,
    str3: *const XmlChar,
) {
    let schannel: Option<StructuredError> = None;
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
        schannel,
        channel,
        data,
        pctxt as _,
        node as _,
        XmlErrorDomain::XmlFromValid,
        error,
        XmlErrorLevel::XmlErrError,
        null_mut(),
        0,
        (!str1.is_null()).then(|| CStr::from_ptr(str1 as *const i8)
            .to_string_lossy()
            .into_owned()
            .into()),
        (!str3.is_null()).then(|| CStr::from_ptr(str3 as *const i8)
            .to_string_lossy()
            .into_owned()
            .into()),
        None,
        int2,
        0,
        msg,
        str1,
        int2,
        str3
    );
}

/**
 * xmlValidateAttributeDecl:
 * @ctxt:  the validation context
 * @doc:  a document instance
 * @attr:  an attribute definition
 *
 * Try to validate a single attribute definition
 * basically it does the following checks as described by the
 * XML-1.0 recommendation:
 *  - [ VC: Attribute Default Legal ]
 *  - [ VC: Enumeration ]
 *  - [ VC: ID Attribute Default ]
 *
 * The ID/IDREF uniqueness and matching are done separately
 *
 * returns 1 if valid or 0 otherwise
 */
#[cfg(feature = "valid")]
pub unsafe extern "C" fn xml_validate_attribute_decl(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    attr: XmlAttributePtr,
) -> c_int {
    use crate::libxml::hash::xml_hash_scan3;

    let mut ret: c_int = 1;
    let val: c_int;
    CHECK_DTD!(doc);
    if attr.is_null() {
        return 1;
    }

    /* Attribute Default Legal */
    /* Enumeration */
    if !(*attr).default_value.is_null() {
        val = xml_validate_attribute_value_internal(doc, (*attr).atype, (*attr).default_value);
        if val == 0 {
            xml_err_valid_node(
                ctxt,
                attr as XmlNodePtr,
                XmlParserErrors::XmlDTDAttributeDefault,
                c"Syntax of default value for attribute %s of %s is not valid\n".as_ptr() as _,
                (*attr).name,
                (*attr).elem,
                null_mut(),
            );
        }
        ret &= val;
    }

    /* ID Attribute Default */
    if matches!((*attr).atype, XmlAttributeType::XmlAttributeId)
        && !matches!(
            (*attr).def,
            XmlAttributeDefault::XmlAttributeImplied | XmlAttributeDefault::XmlAttributeRequired
        )
    {
        xml_err_valid_node(
            ctxt,
            attr as XmlNodePtr,
            XmlParserErrors::XmlDTDIDFixed,
            c"ID attribute %s of %s is not valid must be #IMPLIED or #REQUIRED\n".as_ptr() as _,
            (*attr).name,
            (*attr).elem,
            null_mut(),
        );
        ret = 0;
    }

    /* One ID per Element Type */
    if matches!((*attr).atype, XmlAttributeType::XmlAttributeId) {
        let mut nb_id: c_int;

        /* the trick is that we parse DtD as their own internal subset */
        let mut elem: XmlElementPtr = xml_get_dtd_element_desc((*doc).int_subset, (*attr).elem);
        if !elem.is_null() {
            nb_id = xml_scan_id_attribute_decl(null_mut(), elem, 0);
        } else {
            let table: XmlAttributeTablePtr;

            /*
             * The attribute may be declared in the internal subset and the
             * element in the external subset.
             */
            nb_id = 0;
            if !(*doc).int_subset.is_null() {
                table = (*(*doc).int_subset).attributes as XmlAttributeTablePtr;
                xml_hash_scan3(
                    table,
                    null_mut(),
                    null_mut(),
                    (*attr).elem,
                    Some(xml_validate_attribute_id_callback),
                    addr_of_mut!(nb_id) as _,
                );
            }
        }
        if nb_id > 1 {
            xml_err_valid_node_nr(
                ctxt,
                attr as XmlNodePtr,
                XmlParserErrors::XmlDTDIDSubset,
                c"Element %s has %d ID attribute defined in the internal subset : %s\n".as_ptr()
                    as _,
                (*attr).elem,
                nb_id,
                (*attr).name,
            );
        } else if !(*doc).ext_subset.is_null() {
            let mut ext_id: c_int = 0;
            elem = xml_get_dtd_element_desc((*doc).ext_subset, (*attr).elem);
            if !elem.is_null() {
                ext_id = xml_scan_id_attribute_decl(null_mut(), elem, 0);
            }
            if ext_id > 1 {
                xml_err_valid_node_nr(
                    ctxt,
                    attr as XmlNodePtr,
                    XmlParserErrors::XmlDTDIDSubset,
                    c"Element %s has %d ID attribute defined in the external subset : %s\n".as_ptr()
                        as _,
                    (*attr).elem,
                    ext_id,
                    (*attr).name,
                );
            } else if ext_id + nb_id > 1 {
                xml_err_valid_node(ctxt, attr as XmlNodePtr, XmlParserErrors::XmlDTDIDSubset, c"Element %s has ID attributes defined in the internal and external subset : %s\n".as_ptr() as _,
		       (*attr).elem, (*attr).name, null_mut());
            }
        }
    }

    /* Validity Constraint: Enumeration */
    if !(*attr).default_value.is_null() && !(*attr).tree.is_null() {
        let mut tree: XmlEnumerationPtr = (*attr).tree;
        while !tree.is_null() {
            if xml_str_equal((*tree).name, (*attr).default_value) {
                break;
            }
            tree = (*tree).next;
        }
        if tree.is_null() {
            xml_err_valid_node(
                ctxt,
                attr as XmlNodePtr,
                XmlParserErrors::XmlDTDAttributeValue,
                c"Default value \"%s\" for attribute %s of %s is not among the enumerated set\n"
                    .as_ptr() as _,
                (*attr).default_value,
                (*attr).name,
                (*attr).elem,
            );
            ret = 0;
        }
    }

    ret
}

/**
 * xmlValidateAttributeValue:
 * @type:  an attribute type
 * @value:  an attribute value
 *
 * Validate that the given attribute value match  the proper production
 *
 * [ VC: ID ]
 * Values of type ID must match the Name production....
 *
 * [ VC: IDREF ]
 * Values of type IDREF must match the Name production, and values
 * of type IDREFS must match Names ...
 *
 * [ VC: Entity Name ]
 * Values of type ENTITY must match the Name production, values
 * of type ENTITIES must match Names ...
 *
 * [ VC: Name Token ]
 * Values of type NMTOKEN must match the Nmtoken production; values
 * of type NMTOKENS must match Nmtokens.
 *
 * returns 1 if valid or 0 otherwise
 */
#[cfg(feature = "valid")]
pub unsafe extern "C" fn xml_validate_attribute_value(
    typ: XmlAttributeType,
    value: *const XmlChar,
) -> c_int {
    xml_validate_attribute_value_internal(null_mut(), typ, value)
}

/**
 * xmlValidateNotationDecl:
 * @ctxt:  the validation context
 * @doc:  a document instance
 * @nota:  a notation definition
 *
 * Try to validate a single notation definition
 * basically it does the following checks as described by the
 * XML-1.0 recommendation:
 *  - it seems that no validity constraint exists on notation declarations
 *    But this function get called anyway ...
 *
 * returns 1 if valid or 0 otherwise
 */
#[cfg(feature = "valid")]
pub unsafe extern "C" fn xml_validate_notation_decl(
    _ctxt: XmlValidCtxtPtr,
    _doc: XmlDocPtr,
    _nota: XmlNotationPtr,
) -> c_int {
    let ret: c_int = 1;

    ret
}

/**
 * xmlValidateDtd:
 * @ctxt:  the validation context
 * @doc:  a document instance
 * @dtd:  a dtd instance
 *
 * Try to validate the document against the dtd instance
 *
 * Basically it does check all the definitions in the DtD.
 * Note the the internal subset (if present) is de-coupled
 * (i.e. not used), which could give problems if ID or IDREF
 * is present.
 *
 * returns 1 if valid or 0 otherwise
 */
#[cfg(feature = "valid")]
pub unsafe extern "C" fn xml_validate_dtd(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    dtd: XmlDtdPtr,
) -> c_int {
    let mut ret: c_int;

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
    let root: XmlNodePtr = xml_doc_get_root_element(doc);
    ret = xml_validate_element(ctxt, doc, root);
    ret &= xml_validate_document_final(ctxt, doc);
    (*doc).ext_subset = old_ext;
    (*doc).int_subset = old_int;
    ret
}

/**
 * xmlValidateAttributeValue2:
 * @ctxt:  the validation context
 * @doc:  the document
 * @name:  the attribute name (used for error reporting only)
 * @type:  the attribute type
 * @value:  the attribute value
 *
 * Validate that the given attribute value match a given type.
 * This typically cannot be done before having finished parsing
 * the subsets.
 *
 * [ VC: IDREF ]
 * Values of type IDREF must match one of the declared IDs
 * Values of type IDREFS must match a sequence of the declared IDs
 * each Name must match the value of an ID attribute on some element
 * in the XML document; i.e. IDREF values must match the value of
 * some ID attribute
 *
 * [ VC: Entity Name ]
 * Values of type ENTITY must match one declared entity
 * Values of type ENTITIES must match a sequence of declared entities
 *
 * [ VC: Notation Attributes ]
 * all notation names in the declaration must be declared.
 *
 * returns 1 if valid or 0 otherwise
 */
unsafe extern "C" fn xml_validate_attribute_value2(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    name: *const XmlChar,
    typ: XmlAttributeType,
    value: *const XmlChar,
) -> c_int {
    let mut ret: c_int = 1;
    match typ {
        XmlAttributeType::XmlAttributeIdrefs
        | XmlAttributeType::XmlAttributeIdref
        | XmlAttributeType::XmlAttributeId
        | XmlAttributeType::XmlAttributeNmtokens
        | XmlAttributeType::XmlAttributeEnumeration
        | XmlAttributeType::XmlAttributeNmtoken
        | XmlAttributeType::XmlAttributeCdata => {}
        XmlAttributeType::XmlAttributeEntity => {
            let mut ent: XmlEntityPtr;

            ent = xml_get_doc_entity(doc, value);
            /* yeah it's a bit messy... */
            if ent.is_null() && (*doc).standalone == 1 {
                (*doc).standalone = 0;
                ent = xml_get_doc_entity(doc, value);
            }
            if ent.is_null() {
                xml_err_valid_node(
                    ctxt,
                    doc as XmlNodePtr,
                    XmlParserErrors::XmlDTDUnknownEntity,
                    c"ENTITY attribute %s reference an unknown entity \"%s\"\n".as_ptr() as _,
                    name,
                    value,
                    null_mut(),
                );
                ret = 0;
            } else if !matches!(
                (*ent).etype,
                Some(XmlEntityType::XmlExternalGeneralUnparsedEntity)
            ) {
                xml_err_valid_node(
                    ctxt,
                    doc as XmlNodePtr,
                    XmlParserErrors::XmlDTDEntityType,
                    c"ENTITY attribute %s reference an entity \"%s\" of wrong type\n".as_ptr() as _,
                    name,
                    value,
                    null_mut(),
                );
                ret = 0;
            }
        }
        XmlAttributeType::XmlAttributeEntities => {
            let mut nam: *mut XmlChar;
            let mut cur: *mut XmlChar;
            let mut save: XmlChar;
            let mut ent: XmlEntityPtr;

            let dup: *mut XmlChar = xml_strdup(value);
            if dup.is_null() {
                return 0;
            }
            cur = dup;
            while *cur != 0 {
                nam = cur;
                while *cur != 0 && !xml_is_blank_char(*cur as u32) {
                    cur = cur.add(1);
                }
                save = *cur;
                *cur = 0;
                ent = xml_get_doc_entity(doc, nam);
                if ent.is_null() {
                    xml_err_valid_node(
                        ctxt,
                        doc as XmlNodePtr,
                        XmlParserErrors::XmlDTDUnknownEntity,
                        c"ENTITIES attribute %s reference an unknown entity \"%s\"\n".as_ptr() as _,
                        name,
                        nam,
                        null_mut(),
                    );
                    ret = 0;
                } else if !matches!(
                    (*ent).etype,
                    Some(XmlEntityType::XmlExternalGeneralUnparsedEntity)
                ) {
                    xml_err_valid_node(
                        ctxt,
                        doc as XmlNodePtr,
                        XmlParserErrors::XmlDTDEntityType,
                        c"ENTITIES attribute %s reference an entity \"%s\" of wrong type\n".as_ptr()
                            as _,
                        name,
                        nam,
                        null_mut(),
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

            nota = xml_get_dtd_notation_desc((*doc).int_subset, value);
            if nota.is_null() && !(*doc).ext_subset.is_null() {
                nota = xml_get_dtd_notation_desc((*doc).ext_subset, value);
            }

            if nota.is_null() {
                xml_err_valid_node(
                    ctxt,
                    doc as XmlNodePtr,
                    XmlParserErrors::XmlDTDUnknownNotation,
                    c"NOTATION attribute %s reference an unknown notation \"%s\"\n".as_ptr() as _,
                    name,
                    value,
                    null_mut(),
                );
                ret = 0;
            }
        }
    }
    ret
}

extern "C" fn xml_validate_attribute_callback(
    payload: *mut c_void,
    data: *mut c_void,
    _name: *const XmlChar,
) {
    let cur: XmlAttributePtr = payload as XmlAttributePtr;
    let ctxt: XmlValidCtxtPtr = data as XmlValidCtxtPtr;
    let mut ret: c_int;
    let doc: XmlDocPtr;
    let mut elem: XmlElementPtr = null_mut();

    if cur.is_null() {
        return;
    }
    unsafe {
        match (*cur).atype {
            XmlAttributeType::XmlAttributeCdata
            | XmlAttributeType::XmlAttributeId
            | XmlAttributeType::XmlAttributeIdref
            | XmlAttributeType::XmlAttributeIdrefs
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
                        (*cur).default_value,
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
                            (*tree).name,
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
            if (*cur).elem.is_null() {
                xml_err_valid(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    c"xmlValidateAttributeCallback(%s): internal error\n".as_ptr() as _,
                    (*cur).name as *const c_char,
                );
                return;
            }

            if !doc.is_null() {
                elem = xml_get_dtd_element_desc((*doc).int_subset, (*cur).elem);
            }
            if elem.is_null() && !doc.is_null() {
                elem = xml_get_dtd_element_desc((*doc).ext_subset, (*cur).elem);
            }
            if elem.is_null()
                && !(*cur).parent.is_null()
                && matches!((*(*cur).parent).typ, XmlElementType::XmlDtdNode)
            {
                elem = xml_get_dtd_element_desc((*cur).parent as XmlDtdPtr, (*cur).elem);
            }
            if elem.is_null() {
                xml_err_valid_node(
                    ctxt,
                    null_mut(),
                    XmlParserErrors::XmlDTDUnknownElem,
                    c"attribute %s: could not find decl for element %s\n".as_ptr() as _,
                    (*cur).name,
                    (*cur).elem,
                    null_mut(),
                );
                return;
            }
            if matches!((*elem).etype, XmlElementTypeVal::XmlElementTypeEmpty) {
                xml_err_valid_node(
                    ctxt,
                    null_mut(),
                    XmlParserErrors::XmlDTDEmptyNotation,
                    c"NOTATION attribute %s declared for EMPTY element %s\n".as_ptr() as _,
                    (*cur).name,
                    (*cur).elem,
                    null_mut(),
                );
                (*ctxt).valid = 0;
            }
        }
    }
}

extern "C" fn xml_validate_notation_callback(
    payload: *mut c_void,
    data: *mut c_void,
    _name: *const XmlChar,
) {
    let cur: XmlEntityPtr = payload as XmlEntityPtr;
    let ctxt: XmlValidCtxtPtr = data as XmlValidCtxtPtr;
    if cur.is_null() {
        return;
    }
    unsafe {
        if matches!(
            (*cur).etype,
            Some(XmlEntityType::XmlExternalGeneralUnparsedEntity)
        ) {
            let notation: *mut XmlChar = (*cur).content.load(Ordering::Relaxed) as _;

            if !notation.is_null() {
                let ret: c_int = xml_validate_notation_use(
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

/**
 * xmlValidateDtdFinal:
 * @ctxt:  the validation context
 * @doc:  a document instance
 *
 * Does the final step for the dtds validation once all the
 * subsets have been parsed
 *
 * basically it does the following checks described by the XML Rec
 * - check that ENTITY and ENTITIES type attributes default or
 *   possible values matches one of the defined entities.
 * - check that NOTATION type attributes default or
 *   possible values matches one of the defined notations.
 *
 * returns 1 if valid or 0 if invalid and -1 if not well-formed
 */
#[cfg(feature = "valid")]
pub unsafe extern "C" fn xml_validate_dtd_final(ctxt: XmlValidCtxtPtr, doc: XmlDocPtr) -> c_int {
    let mut dtd: XmlDtdPtr;
    let mut table: XmlAttributeTablePtr;
    let mut entities: XmlEntitiesTablePtr;

    if doc.is_null() || ctxt.is_null() {
        return 0;
    }
    if (*doc).int_subset.is_null() && (*doc).ext_subset.is_null() {
        return 0;
    }
    (*ctxt).doc = doc;
    (*ctxt).valid = 1;
    dtd = (*doc).int_subset;
    if !dtd.is_null() && !(*dtd).attributes.is_null() {
        table = (*dtd).attributes as XmlAttributeTablePtr;
        xml_hash_scan(table, Some(xml_validate_attribute_callback), ctxt as _);
    }
    if !dtd.is_null() && !(*dtd).entities.is_null() {
        entities = (*dtd).entities as XmlEntitiesTablePtr;
        xml_hash_scan(entities, Some(xml_validate_notation_callback), ctxt as _);
    }
    dtd = (*doc).ext_subset;
    if !dtd.is_null() && !(*dtd).attributes.is_null() {
        table = (*dtd).attributes as XmlAttributeTablePtr;
        xml_hash_scan(table, Some(xml_validate_attribute_callback), ctxt as _);
    }
    if !dtd.is_null() && !(*dtd).entities.is_null() {
        entities = (*dtd).entities as XmlEntitiesTablePtr;
        xml_hash_scan(entities, Some(xml_validate_notation_callback), ctxt as _);
    }
    (*ctxt).valid
}

/**
 * xmlValidateDocument:
 * @ctxt:  the validation context
 * @doc:  a document instance
 *
 * Try to validate the document instance
 *
 * basically it does the all the checks described by the XML Rec
 * i.e. validates the internal and external subset (if present)
 * and validate the document tree.
 *
 * returns 1 if valid or 0 otherwise
 */
#[cfg(feature = "valid")]
pub unsafe extern "C" fn xml_validate_document(ctxt: XmlValidCtxtPtr, doc: XmlDocPtr) -> c_int {
    use crate::libxml::parser::xml_parse_dtd;

    use super::uri::xml_build_uri;

    let mut ret: c_int;

    if doc.is_null() {
        return 0;
    }
    if (*doc).int_subset.is_null() && (*doc).ext_subset.is_null() {
        xml_err_valid(
            ctxt,
            XmlParserErrors::XmlDTDNoDTD,
            c"no DTD found!\n".as_ptr() as _,
            null_mut(),
        );
        return 0;
    }
    if !(*doc).int_subset.is_null()
        && (!(*(*doc).int_subset).system_id.is_null()
            || !(*(*doc).int_subset).external_id.is_null())
        && (*doc).ext_subset.is_null()
    {
        let sys_id: *mut XmlChar;
        if !(*(*doc).int_subset).system_id.is_null() {
            sys_id = xml_build_uri((*(*doc).int_subset).system_id, (*doc).url);
            if sys_id.is_null() {
                xml_err_valid(
                    ctxt,
                    XmlParserErrors::XmlDTDLoadError,
                    c"Could not build URI for external subset \"%s\"\n".as_ptr() as _,
                    (*(*doc).int_subset).system_id as *const c_char,
                );
                return 0;
            }
        } else {
            sys_id = null_mut();
        }
        (*doc).ext_subset =
            xml_parse_dtd((*(*doc).int_subset).external_id, sys_id as *const XmlChar);
        if !sys_id.is_null() {
            xml_free(sys_id as _);
        }
        if (*doc).ext_subset.is_null() {
            if !(*(*doc).int_subset).system_id.is_null() {
                xml_err_valid(
                    ctxt,
                    XmlParserErrors::XmlDTDLoadError,
                    c"Could not load the external subset \"%s\"\n".as_ptr() as _,
                    (*(*doc).int_subset).system_id as *const c_char,
                );
            } else {
                xml_err_valid(
                    ctxt,
                    XmlParserErrors::XmlDTDLoadError,
                    c"Could not load the external subset \"%s\"\n".as_ptr() as _,
                    (*(*doc).int_subset).external_id as *const c_char,
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

    let root: XmlNodePtr = xml_doc_get_root_element(doc);
    ret &= xml_validate_element(ctxt, doc, root);
    ret &= xml_validate_document_final(ctxt, doc);
    ret
}

/**
 * xmlValidateElement:
 * @ctxt:  the validation context
 * @doc:  a document instance
 * @elem:  an element instance
 *
 * Try to validate the subtree under an element
 *
 * returns 1 if valid or 0 otherwise
 */
#[cfg(feature = "valid")]
pub unsafe extern "C" fn xml_validate_element(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    root: XmlNodePtr,
) -> c_int {
    let mut elem: XmlNodePtr;
    let mut attr: XmlAttrPtr;
    let mut ns: XmlNsPtr;
    let mut value: *const XmlChar;
    let mut ret: c_int = 1;

    if root.is_null() {
        return 0;
    }

    CHECK_DTD!(doc);

    elem = root;
    loop {
        ret &= xml_validate_one_element(ctxt, doc, elem);

        if matches!((*elem).typ, XmlElementType::XmlElementNode) {
            attr = (*elem).properties;
            while !attr.is_null() {
                value = xml_node_list_get_string(doc, (*attr).children, 0);
                ret &= xml_validate_one_attribute(ctxt, doc, elem, attr, value);
                if !value.is_null() {
                    xml_free(value as _);
                }
                attr = (*attr).next;
            }

            ns = (*elem).ns_def;
            while !ns.is_null() {
                if (*elem).ns.is_null() {
                    ret &= xml_validate_one_namespace(
                        ctxt,
                        doc,
                        elem,
                        null_mut(),
                        ns,
                        (*ns).href.load(Ordering::Relaxed),
                    );
                } else {
                    ret &= xml_validate_one_namespace(
                        ctxt,
                        doc,
                        elem,
                        (*(*elem).ns).prefix.load(Ordering::Relaxed),
                        ns,
                        (*ns).href.load(Ordering::Relaxed),
                    );
                }
                ns = (*ns).next.load(Ordering::Relaxed);
            }

            if !(*elem).children.is_null() {
                elem = (*elem).children;
                continue;
            }
        }

        loop {
            if elem == root {
                // goto done;
                return ret;
            }
            if !(*elem).next.is_null() {
                break;
            }
            elem = (*elem).parent;
        }
        elem = (*elem).next;
    }

    // done:
    // return ret;
}

/**
 * xmlValidGetElemDecl:
 * @ctxt:  the validation context
 * @doc:  a document instance
 * @elem:  an element instance
 * @extsubset:  pointer, (out) indicate if the declaration was found
 *              in the external subset.
 *
 * Finds a declaration associated to an element in the document.
 *
 * returns the pointer to the declaration or null_mut() if not found.
 */
unsafe extern "C" fn xml_valid_get_elem_decl(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    elem: XmlNodePtr,
    extsubset: *mut c_int,
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
    if !(*elem).ns.is_null() && !(*(*elem).ns).prefix.load(Ordering::Relaxed).is_null() {
        prefix = (*(*elem).ns).prefix.load(Ordering::Relaxed);
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
        xml_err_valid_node(
            ctxt,
            elem,
            XmlParserErrors::XmlDTDUnknownElem,
            c"No declaration for element %s\n".as_ptr() as _,
            (*elem).name,
            null_mut(),
            null_mut(),
        );
    }
    elem_decl
}

unsafe extern "C" fn node_vpush(ctxt: XmlValidCtxtPtr, value: XmlNodePtr) -> c_int {
    if (*ctxt).node_max <= 0 {
        (*ctxt).node_max = 4;
        (*ctxt).node_tab =
            xml_malloc((*ctxt).node_max as usize * size_of_val(&*(*ctxt).node_tab.add(0)))
                as *mut XmlNodePtr;
        if (*ctxt).node_tab.is_null() {
            xml_verr_memory(ctxt as _, c"malloc failed".as_ptr() as _);
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
            xml_verr_memory(ctxt as _, c"realloc failed".as_ptr() as _);
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

/**
 * xmlValidateCdataElement:
 * @ctxt:  the validation context
 * @doc:  a document instance
 * @elem:  an element instance
 *
 * Check that an element follows #CDATA
 *
 * returns 1 if valid or 0 otherwise
 */
unsafe extern "C" fn xml_validate_one_cdata_element(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    elem: XmlNodePtr,
) -> c_int {
    let mut ret: c_int = 1;
    let mut cur: XmlNodePtr;

    if ctxt.is_null()
        || doc.is_null()
        || elem.is_null()
        || !matches!((*elem).typ, XmlElementType::XmlElementNode)
    {
        return 0;
    }

    let child: XmlNodePtr = (*elem).children;

    cur = child;
    'done: while !cur.is_null() {
        match (*cur).typ {
            XmlElementType::XmlEntityRefNode => {
                /*
                 * Push the current node to be able to roll back
                 * and process within the entity
                 */
                if !(*cur).children.is_null() && !(*(*cur).children).children.is_null() {
                    node_vpush(ctxt, cur);
                    cur = (*(*cur).children).children;
                    continue;
                }
            }
            XmlElementType::XmlCommentNode
            | XmlElementType::XmlPiNode
            | XmlElementType::XmlTextNode
            | XmlElementType::XmlCdataSectionNode => {}
            _ => {
                ret = 0;
                // goto done;
                break 'done;
            }
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
    // done:
    (*ctxt).node_max = 0;
    (*ctxt).node_nr = 0;
    if !(*ctxt).node_tab.is_null() {
        xml_free((*ctxt).node_tab as _);
        (*ctxt).node_tab = null_mut();
    }
    ret
}

#[cfg(not(feature = "regexp"))]
macro_rules! DEBUG_VALID_MSG {
    ($m:expr) => {
        $crate::generic_error!("{}\n", $m);
    };
}

/**
 * xmlSnprintfElements:
 * @buf:  an output buffer
 * @size:  the size of the buffer
 * @content:  An element
 * @glob: 1 if one must print the englobing parenthesis, 0 otherwise
 *
 * This will dump the list of elements to the buffer
 * Intended just for the debug routine
 */
unsafe extern "C" fn xml_snprintf_elements(
    buf: *mut c_char,
    size: c_int,
    node: XmlNodePtr,
    glob: c_int,
) {
    let mut cur: XmlNodePtr;
    let mut len: c_int;

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
        match (*cur).typ {
            XmlElementType::XmlElementNode => {
                if !(*cur).ns.is_null() && !(*(*cur).ns).prefix.load(Ordering::Relaxed).is_null() {
                    if size - len
                        < xml_strlen((*(*cur).ns).prefix.load(Ordering::Relaxed) as _) + 10
                    {
                        if size - len > 4 && *buf.add(len as usize - 1) != b'.' as i8 {
                            strcat(buf, c" ...".as_ptr() as _);
                        }
                        return;
                    }
                    strcat(
                        buf,
                        (*(*cur).ns).prefix.load(Ordering::Relaxed) as *mut c_char,
                    );
                    strcat(buf, c":".as_ptr() as _);
                }
                if size - len < xml_strlen((*cur).name) + 10 {
                    if size - len > 4 && *buf.add(len as usize - 1) != b'.' as i8 {
                        strcat(buf, c" ...".as_ptr() as _);
                    }
                    return;
                }
                strcat(buf, (*cur).name as *mut c_char);
                if !(*cur).next.is_null() {
                    strcat(buf, c" ".as_ptr() as _);
                }
            }
            ty @ XmlElementType::XmlTextNode
            | ty @ XmlElementType::XmlCdataSectionNode
            | ty @ XmlElementType::XmlEntityRefNode => 'to_break: {
                if matches!(ty, XmlElementType::XmlTextNode) && xml_is_blank_node(cur) != 0 {
                    break 'to_break;
                }
                strcat(buf, c"CDATA".as_ptr() as _);
                if !(*cur).next.is_null() {
                    strcat(buf, c" ".as_ptr() as _);
                }
            }
            XmlElementType::XmlAttributeNode
            | XmlElementType::XmlDocumentNode
            | XmlElementType::XmlHtmlDocumentNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlNamespaceDecl => {
                strcat(buf, c"???".as_ptr() as _);
                if !(*cur).next.is_null() {
                    strcat(buf, c" ".as_ptr() as _);
                }
            }
            XmlElementType::XmlEntityNode
            | XmlElementType::XmlPiNode
            | XmlElementType::XmlDtdNode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlElementDecl
            | XmlElementType::XmlAttributeDecl
            | XmlElementType::XmlEntityDecl
            | XmlElementType::XmlXincludeStart
            | XmlElementType::XmlXincludeEnd => {}
            _ => unreachable!(),
        }
        cur = (*cur).next;
    }
    if glob != 0 {
        strcat(buf, c")".as_ptr() as _);
    }
}

#[cfg(not(feature = "regexp"))]
const ROLLBACK_OR: usize = 0;
#[cfg(not(feature = "regexp"))]
const ROLLBACK_PARENT: usize = 1;

/**
 * xmlValidateElementType:
 * @ctxt:  the validation context
 *
 * Try to validate the content model of an element internal function
 *
 * returns 1 if valid or 0 ,-1 in case of error, -2 if an entity
 *           reference is found and -3 if the validation succeeded but
 *           the content model is not determinist.
 */
#[cfg(not(feature = "regexp"))]
unsafe extern "C" fn xmlValidateElementType(ctxt: XmlValidCtxtPtr) -> c_int {
    let mut ret: c_int = -1;
    let mut determinist: c_int = 1;

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
                XmlElementContentType::XmlElementContentPcdata => {
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
                                        XmlElementType::XmlCdataSectionNode
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
                                        XmlElementType::XmlCdataSectionNode
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
                        //  #ifdef DEBUG_VALID_ALGO
                        //              if (((*(*ctxt).vstate).occurs & (1 << (*(*ctxt).vstate).depth)) == 0) {
                        //              DEBUG_VALID_MSG!(c"Mult branch failed".as_ptr());
                        //              } else {
                        //              DEBUG_VALID_MSG!(c"Mult branch found".as_ptr());
                        //              }
                        //  #endif
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
                XmlElementContentType::XmlElementContentPcdata => {
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

/**
 * xmlValidateElementContent:
 * @ctxt:  the validation context
 * @child:  the child list
 * @elemDecl:  pointer to the element declaration
 * @warn:  emit the error message
 * @parent: the parent element (for error reporting)
 *
 * Try to validate the content model of an element
 *
 * returns 1 if valid or 0 if not and -1 in case of error
 */

unsafe extern "C" fn xml_validate_element_content(
    ctxt: XmlValidCtxtPtr,
    child: XmlNodePtr,
    elem_decl: XmlElementPtr,
    warn: c_int,
    parent: XmlNodePtr,
) -> c_int {
    let mut ret: c_int = 1;
    #[cfg(not(feature = "regexp"))]
    let mut repl: XmlNodePtr = null_mut();
    #[cfg(not(feature = "regexp"))]
    let mut last: XmlNodePtr = null_mut();
    #[cfg(not(feature = "regexp"))]
    let mut tmp: XmlNodePtr;
    let mut cur: XmlNodePtr;

    if elem_decl.is_null() || parent.is_null() || ctxt.is_null() {
        return -1;
    }
    let cont: XmlElementContentPtr = (*elem_decl).content;
    let name: *const XmlChar = (*elem_decl).name;

    #[cfg(feature = "regexp")]
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
                        match (*cur).typ {
                            XmlElementType::XmlEntityRefNode => {
                                /*
                                 * Push the current node to be able to roll back
                                 * and process within the entity
                                 */
                                if !(*cur).children.is_null()
                                    && !(*(*cur).children).children.is_null()
                                {
                                    node_vpush(ctxt, cur);
                                    cur = (*(*cur).children).children;
                                    continue;
                                }
                            }
                            XmlElementType::XmlTextNode => {
                                if xml_is_blank_node(cur) != 0 {
                                    //  break;
                                } else {
                                    ret = 0;
                                    break 'fail;
                                }
                            }
                            XmlElementType::XmlCdataSectionNode => {
                                /* TODO */
                                ret = 0;
                                break 'fail;
                            }
                            XmlElementType::XmlElementNode => {
                                if !(*cur).ns.is_null()
                                    && !(*(*cur).ns).prefix.load(Ordering::Relaxed).is_null()
                                {
                                    let mut fname: [XmlChar; 50] = [0; 50];

                                    let fullname: *mut XmlChar = xml_build_qname(
                                        (*cur).name,
                                        (*(*cur).ns).prefix.load(Ordering::Relaxed) as _,
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
                        cur = (*cur).next;
                        while cur.is_null() {
                            cur = node_vpop(ctxt);
                            if cur.is_null() {
                                break;
                            }
                            cur = (*cur).next;
                        }
                    }

                    ret = xml_reg_exec_push_string(exec, null_mut(), null_mut());
                }
                // fail:
                xml_reg_free_exec_ctxt(exec);
            }
        }
    }

    #[cfg_attr(feature = "regexp", allow(unused_labels))]
    // label `'done` is used just only when 'regexp' is disabled.
    'done: {
        #[cfg(not(feature = "regexp"))]
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
                    match (*cur).typ {
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
                        | ty @ XmlElementType::XmlCdataSectionNode
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
                                if matches!((*cur).typ, XmlElementType::XmlCdataSectionNode) {
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
                #[cfg(not(feature = "regexp"))]
                {
                    if !repl.is_null() {
                        xml_snprintf_elements(list.as_mut_ptr().add(0) as _, 5000, repl, 1);
                    } else {
                        xml_snprintf_elements(list.as_mut_ptr().add(0) as _, 5000, child, 1);
                    }
                }
                #[cfg(feature = "regexp")]
                {
                    xml_snprintf_elements(list.as_mut_ptr().add(0) as _, 5000, child, 1);
                }

                if !name.is_null() {
                    xml_err_valid_node(
                        ctxt,
                        parent,
                        XmlParserErrors::XmlDTDContentModel,
                        c"Element %s content does not follow the DTD, expecting %s, got %s\n"
                            .as_ptr() as _,
                        name,
                        expr.as_mut_ptr() as _,
                        list.as_ptr() as _,
                    );
                } else {
                    xml_err_valid_node(
                        ctxt,
                        parent,
                        XmlParserErrors::XmlDTDContentModel,
                        c"Element content does not follow the DTD, expecting %s, got %s\n".as_ptr()
                            as _,
                        expr.as_ptr() as _,
                        list.as_ptr() as _,
                        null_mut(),
                    );
                }
            } else if !name.is_null() {
                xml_err_valid_node(
                    ctxt,
                    parent,
                    XmlParserErrors::XmlDTDContentModel,
                    c"Element %s content does not follow the DTD\n".as_ptr() as _,
                    name,
                    null_mut(),
                    null_mut(),
                );
            } else {
                xml_err_valid_node(
                    ctxt,
                    parent,
                    XmlParserErrors::XmlDTDContentModel,
                    c"Element content does not follow the DTD\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                    null_mut(),
                );
            }
            ret = 0;
        }
        if ret == -3 {
            ret = 1;
        }
    }

    #[cfg(not(feature = "regexp"))]
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

/**
 * xmlValidateOneElement:
 * @ctxt:  the validation context
 * @doc:  a document instance
 * @elem:  an element instance
 *
 * Try to validate a single element and it's attributes,
 * basically it does the following checks as described by the
 * XML-1.0 recommendation:
 *  - [ VC: Element Valid ]
 *  - [ VC: Required Attribute ]
 *    Then call xmlValidateOneAttribute() for each attribute present.
 *
 * The ID/IDREF checkings are done separately
 *
 * returns 1 if valid or 0 otherwise
 */
#[cfg(feature = "valid")]
pub unsafe extern "C" fn xml_validate_one_element(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    elem: XmlNodePtr,
) -> c_int {
    let mut cont: XmlElementContentPtr;
    let mut attr: XmlAttributePtr;
    let mut child: XmlNodePtr;
    let mut ret: c_int = 1;
    let tmp: c_int;
    let mut name: *const XmlChar;
    let mut extsubset: c_int = 0;

    CHECK_DTD!(doc);

    if elem.is_null() {
        return 0;
    }
    match (*elem).typ {
        XmlElementType::XmlAttributeNode => {
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlErrInternalError,
                c"Attribute element not expected\n".as_ptr() as _,
                null_mut(),
                null_mut(),
                null_mut(),
            );
            return 0;
        }
        XmlElementType::XmlTextNode => {
            if !(*elem).children.is_null() {
                xml_err_valid_node(
                    ctxt,
                    elem,
                    XmlParserErrors::XmlErrInternalError,
                    c"Text element has children !\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                    null_mut(),
                );
                return 0;
            }
            if !(*elem).ns.is_null() {
                xml_err_valid_node(
                    ctxt,
                    elem,
                    XmlParserErrors::XmlErrInternalError,
                    c"Text element has namespace !\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                    null_mut(),
                );
                return 0;
            }
            if (*elem).content.is_null() {
                xml_err_valid_node(
                    ctxt,
                    elem,
                    XmlParserErrors::XmlErrInternalError,
                    c"Text element has no content !\n".as_ptr() as _,
                    null_mut(),
                    null_mut(),
                    null_mut(),
                );
                return 0;
            }
            return 1;
        }
        XmlElementType::XmlXincludeStart | XmlElementType::XmlXincludeEnd => {
            return 1;
        }
        XmlElementType::XmlCdataSectionNode
        | XmlElementType::XmlEntityRefNode
        | XmlElementType::XmlPiNode
        | XmlElementType::XmlCommentNode => {
            return 1;
        }
        XmlElementType::XmlEntityNode => {
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlErrInternalError,
                c"Entity element not expected\n".as_ptr() as _,
                null_mut(),
                null_mut(),
                null_mut(),
            );
            return 0;
        }
        XmlElementType::XmlNotationNode => {
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlErrInternalError,
                c"Notation element not expected\n".as_ptr() as _,
                null_mut(),
                null_mut(),
                null_mut(),
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
                c"Document element not expected\n".as_ptr() as _,
                null_mut(),
                null_mut(),
                null_mut(),
            );
            return 0;
        }
        XmlElementType::XmlHtmlDocumentNode => {
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlErrInternalError,
                c"HTML Document not expected\n".as_ptr() as _,
                null_mut(),
                null_mut(),
                null_mut(),
            );
            return 0;
        }
        XmlElementType::XmlElementNode => {}
        _ => {
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlErrInternalError,
                c"unknown element type\n".as_ptr() as _,
                null_mut(),
                null_mut(),
                null_mut(),
            );
            return 0;
        }
    }

    /*
     * Fetch the declaration
     */
    let elem_decl: XmlElementPtr =
        xml_valid_get_elem_decl(ctxt, doc, elem, addr_of_mut!(extsubset));
    if elem_decl.is_null() {
        return 0;
    }

    /*
     * If vstateNr is not zero that means continuous validation is
     * activated, do not try to check the content model at that level.
     */
    if (*ctxt).vstate_nr == 0 {
        /* Check that the element content matches the definition */
        match (*elem_decl).etype {
            XmlElementTypeVal::XmlElementTypeUndefined => {
                xml_err_valid_node(
                    ctxt,
                    elem,
                    XmlParserErrors::XmlDTDUnknownElem,
                    c"No declaration for element %s\n".as_ptr() as _,
                    (*elem).name,
                    null_mut(),
                    null_mut(),
                );
                return 0;
            }
            XmlElementTypeVal::XmlElementTypeEmpty => {
                if !(*elem).children.is_null() {
                    xml_err_valid_node(
                        ctxt,
                        elem,
                        XmlParserErrors::XmlDTDNotEmpty,
                        c"Element %s was declared EMPTY this one has content\n".as_ptr() as _,
                        (*elem).name,
                        null_mut(),
                        null_mut(),
                    );
                    ret = 0;
                }
            }
            XmlElementTypeVal::XmlElementTypeAny => {
                /* I don't think anything is required then */
            }
            XmlElementTypeVal::XmlElementTypeMixed => {
                /* simple case of declared as #PCDATA */
                if !(*elem_decl).content.is_null()
                    && (*(*elem_decl).content).typ == XmlElementContentType::XmlElementContentPcdata
                {
                    ret = xml_validate_one_cdata_element(ctxt, doc, elem);
                    if ret == 0 {
                        xml_err_valid_node(
                            ctxt,
                            elem,
                            XmlParserErrors::XmlDTDNotPCDATA,
                            c"Element %s was declared #PCDATA but contains non text nodes\n"
                                .as_ptr() as _,
                            (*elem).name,
                            null_mut(),
                            null_mut(),
                        );
                    }
                } else {
                    child = (*elem).children;
                    /* Hum, this start to get messy */
                    while !child.is_null() {
                        'child_ok: {
                            if matches!((*child).typ, XmlElementType::XmlElementNode) {
                                name = (*child).name;
                                if !(*child).ns.is_null()
                                    && !(*(*child).ns).prefix.load(Ordering::Relaxed).is_null()
                                {
                                    let mut fname: [XmlChar; 50] = [0; 50];

                                    let fullname: *mut XmlChar = xml_build_qname(
                                        (*child).name,
                                        (*(*child).ns).prefix.load(Ordering::Relaxed),
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
                                                XmlElementContentType::XmlElementContentPcdata
                                            )
                                        {
                                            xml_err_valid(
                                                null_mut(),
                                                XmlParserErrors::XmlDTDMixedCorrupt,
                                                c"Internal: MIXED struct corrupted\n".as_ptr() as _,
                                                null_mut(),
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
                                            XmlElementContentType::XmlElementContentPcdata
                                        )
                                    {
                                        xml_err_valid(
                                            ctxt,
                                            XmlParserErrors::XmlDTDMixedCorrupt,
                                            c"Internal: MIXED struct corrupted\n".as_ptr() as _,
                                            null_mut(),
                                        );
                                        break;
                                    }
                                    cont = (*cont).c2;
                                }
                                if cont.is_null() {
                                    xml_err_valid_node(ctxt, elem, XmlParserErrors::XmlDTDInvalidChild,
                                c"Element %s is not declared in %s list of possible children\n".as_ptr() as _,
                                       name, (*elem).name, null_mut());
                                    ret = 0;
                                }
                            }
                        }
                        // child_ok:
                        child = (*child).next;
                    }
                }
            }
            XmlElementTypeVal::XmlElementTypeElement => {
                if (*doc).standalone == 1 && extsubset == 1 {
                    /*
                     * VC: Standalone Document Declaration
                     *     - element types with element content, if white space
                     *       occurs directly within any instance of those types.
                     */
                    child = (*elem).children;
                    while !child.is_null() {
                        if matches!((*child).typ, XmlElementType::XmlTextNode) {
                            let mut content: *const XmlChar = (*child).content;

                            while xml_is_blank_char(*content as u32) {
                                content = content.add(1);
                            }
                            if *content == 0 {
                                xml_err_valid_node(ctxt, elem, XmlParserErrors::XmlDTDStandaloneWhiteSpace, c"standalone: %s declared in the external subset contains white spaces nodes\n".as_ptr() as _, (*elem).name, null_mut(), null_mut());
                                ret = 0;
                                break;
                            }
                        }
                        child = (*child).next;
                    }
                }
                child = (*elem).children;
                // cont = (*elem_decl).content;
                tmp = xml_validate_element_content(ctxt, child, elem_decl, 1, elem);
                if tmp <= 0 {
                    ret = tmp;
                }
            }
        }
    } /* not continuous */

    /* [ VC: Required Attribute ] */
    attr = (*elem_decl).attributes;
    while !attr.is_null() {
        'found: {
            if matches!((*attr).def, XmlAttributeDefault::XmlAttributeRequired) {
                let mut qualified: c_int = -1;

                if (*attr).prefix.is_null() && xml_str_equal((*attr).name, c"xmlns".as_ptr() as _) {
                    let mut ns: XmlNsPtr;

                    ns = (*elem).ns_def;
                    while !ns.is_null() {
                        if (*ns).prefix.load(Ordering::Relaxed).is_null() {
                            break 'found;
                        }
                        ns = (*ns).next.load(Ordering::Relaxed);
                    }
                } else if xml_str_equal((*attr).prefix, c"xmlns".as_ptr() as _) {
                    let mut ns: XmlNsPtr;

                    ns = (*elem).ns_def;
                    while !ns.is_null() {
                        if xml_str_equal((*attr).name, (*ns).prefix.load(Ordering::Relaxed)) {
                            break 'found;
                        }
                        ns = (*ns).next.load(Ordering::Relaxed);
                    }
                } else {
                    let mut attrib: XmlAttrPtr;

                    attrib = (*elem).properties;
                    while !attrib.is_null() {
                        if xml_str_equal((*attrib).name, (*attr).name) {
                            if !(*attr).prefix.is_null() {
                                let mut name_space: XmlNsPtr = (*attrib).ns;

                                if name_space.is_null() {
                                    name_space = (*elem).ns;
                                }
                                /*
                                 * qualified names handling is problematic, having a
                                 * different prefix should be possible but DTDs don't
                                 * allow to define the URI instead of the prefix :-(
                                 */
                                if name_space.is_null() {
                                    if qualified < 0 {
                                        qualified = 0;
                                    }
                                } else if !xml_str_equal(
                                    (*name_space).prefix.load(Ordering::Relaxed),
                                    (*attr).prefix,
                                ) {
                                    if qualified < 1 {
                                        qualified = 1;
                                    }
                                } else {
                                    break 'found;
                                }
                            } else {
                                /*
                                 * We should allow applications to define namespaces
                                 * for their application even if the DTD doesn't
                                 * carry one, otherwise, basically we would always
                                 * break.
                                 */
                                break 'found;
                            }
                        }
                        attrib = (*attrib).next;
                    }
                }
                if qualified == -1 {
                    if (*attr).prefix.is_null() {
                        xml_err_valid_node(
                            ctxt,
                            elem,
                            XmlParserErrors::XmlDTDMissingAttribute,
                            c"Element %s does not carry attribute %s\n".as_ptr() as _,
                            (*elem).name,
                            (*attr).name,
                            null_mut(),
                        );
                        ret = 0;
                    } else {
                        xml_err_valid_node(
                            ctxt,
                            elem,
                            XmlParserErrors::XmlDTDMissingAttribute,
                            c"Element %s does not carry attribute %s:%s\n".as_ptr() as _,
                            (*elem).name,
                            (*attr).prefix,
                            (*attr).name,
                        );
                        ret = 0;
                    }
                } else if qualified == 0 {
                    xml_err_valid_warning(
                        ctxt,
                        elem,
                        XmlParserErrors::XmlDTDNoPrefix,
                        c"Element %s required attribute %s:%s has no prefix\n".as_ptr() as _,
                        (*elem).name,
                        (*attr).prefix,
                        (*attr).name,
                    );
                } else if qualified == 1 {
                    xml_err_valid_warning(
                        ctxt,
                        elem,
                        XmlParserErrors::XmlDTDDifferentPrefix,
                        c"Element %s required attribute %s:%s has different prefix\n".as_ptr() as _,
                        (*elem).name,
                        (*attr).prefix,
                        (*attr).name,
                    );
                }
            } else if matches!((*attr).def, XmlAttributeDefault::XmlAttributeFixed) {
                /*
                 * Special tests checking #FIXED namespace declarations
                 * have the right value since this is not done as an
                 * attribute checking
                 */
                if (*attr).prefix.is_null() && xml_str_equal((*attr).name, c"xmlns".as_ptr() as _) {
                    let mut ns: XmlNsPtr;

                    ns = (*elem).ns_def;
                    while !ns.is_null() {
                        if (*ns).prefix.load(Ordering::Relaxed).is_null() {
                            if !xml_str_equal(
                                (*attr).default_value,
                                (*ns).href.load(Ordering::Relaxed),
                            ) {
                                xml_err_valid_node(ctxt, elem,
                                    XmlParserErrors::XmlDTDElemDefaultNamespace,
                                c"Element %s namespace name for default namespace does not match the DTD\n".as_ptr() as _,
                                   (*elem).name, null_mut(), null_mut());
                                ret = 0;
                            }
                            break 'found;
                        }
                        ns = (*ns).next.load(Ordering::Relaxed);
                    }
                } else if xml_str_equal((*attr).prefix, c"xmlns".as_ptr() as _) {
                    let mut ns: XmlNsPtr;

                    ns = (*elem).ns_def;
                    while !ns.is_null() {
                        if xml_str_equal((*attr).name, (*ns).prefix.load(Ordering::Relaxed)) {
                            if !xml_str_equal(
                                (*attr).default_value,
                                (*ns).href.load(Ordering::Relaxed),
                            ) {
                                xml_err_valid_node(
                                    ctxt,
                                    elem,
                                    XmlParserErrors::XmlDTDElemNamespace,
                                    c"Element %s namespace name for %s does not match the DTD\n"
                                        .as_ptr() as _,
                                    (*elem).name,
                                    (*ns).prefix.load(Ordering::Relaxed) as _,
                                    null_mut(),
                                );
                                ret = 0;
                            }
                            break 'found;
                        }
                        ns = (*ns).next.load(Ordering::Relaxed);
                    }
                }
            }
        }
        // found:
        attr = (*attr).nexth;
    }
    ret
}

/**
 * xmlValidateOneAttribute:
 * @ctxt:  the validation context
 * @doc:  a document instance
 * @elem:  an element instance
 * @attr:  an attribute instance
 * @value:  the attribute value (without entities processing)
 *
 * Try to validate a single attribute for an element
 * basically it does the following checks as described by the
 * XML-1.0 recommendation:
 *  - [ VC: Attribute Value Type ]
 *  - [ VC: Fixed Attribute Default ]
 *  - [ VC: Entity Name ]
 *  - [ VC: Name Token ]
 *  - [ VC: ID ]
 *  - [ VC: IDREF ]
 *  - [ VC: Entity Name ]
 *  - [ VC: Notation Attributes ]
 *
 * The ID/IDREF uniqueness and matching are done separately
 *
 * returns 1 if valid or 0 otherwise
 */
#[cfg(feature = "valid")]
pub unsafe extern "C" fn xml_validate_one_attribute(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    elem: XmlNodePtr,
    attr: XmlAttrPtr,
    value: *const XmlChar,
) -> c_int {
    let mut attr_decl: XmlAttributePtr = null_mut();
    let mut ret: c_int = 1;

    CHECK_DTD!(doc);
    if elem.is_null() || (*elem).name.is_null() {
        return 0;
    }
    if attr.is_null() || (*attr).name.is_null() {
        return 0;
    }

    if !(*elem).ns.is_null() && !(*(*elem).ns).prefix.load(Ordering::Relaxed).is_null() {
        let mut fname: [XmlChar; 50] = [0; 50];

        let fullname: *mut XmlChar = xml_build_qname(
            (*elem).name,
            (*(*elem).ns).prefix.load(Ordering::Relaxed) as _,
            fname.as_mut_ptr(),
            50,
        );
        if fullname.is_null() {
            return 0;
        }
        if !(*attr).ns.is_null() {
            attr_decl = xml_get_dtd_qattr_desc(
                (*doc).int_subset,
                fullname,
                (*attr).name,
                (*(*attr).ns).prefix.load(Ordering::Relaxed) as _,
            );
            if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                attr_decl = xml_get_dtd_qattr_desc(
                    (*doc).ext_subset,
                    fullname,
                    (*attr).name,
                    (*(*attr).ns).prefix.load(Ordering::Relaxed) as _,
                );
            }
        } else {
            attr_decl = xml_get_dtd_attr_desc((*doc).int_subset, fullname, (*attr).name);
            if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                attr_decl = xml_get_dtd_attr_desc((*doc).ext_subset, fullname, (*attr).name);
            }
        }
        if fullname != fname.as_ptr() as _ && fullname != (*elem).name as _ {
            xml_free(fullname as _);
        }
    }
    if attr_decl.is_null() {
        if !(*attr).ns.is_null() {
            attr_decl = xml_get_dtd_qattr_desc(
                (*doc).int_subset,
                (*elem).name,
                (*attr).name,
                (*(*attr).ns).prefix.load(Ordering::Relaxed),
            );
            if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                attr_decl = xml_get_dtd_qattr_desc(
                    (*doc).ext_subset,
                    (*elem).name,
                    (*attr).name,
                    (*(*attr).ns).prefix.load(Ordering::Relaxed),
                );
            }
        } else {
            attr_decl = xml_get_dtd_attr_desc((*doc).int_subset, (*elem).name, (*attr).name);
            if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                attr_decl = xml_get_dtd_attr_desc((*doc).ext_subset, (*elem).name, (*attr).name);
            }
        }
    }

    /* Validity Constraint: Attribute Value Type */
    if attr_decl.is_null() {
        xml_err_valid_node(
            ctxt,
            elem,
            XmlParserErrors::XmlDTDUnknownAttribute,
            c"No declaration for attribute %s of element %s\n".as_ptr() as _,
            (*attr).name,
            (*elem).name,
            null_mut(),
        );
        return 0;
    }
    (*attr).atype = Some((*attr_decl).atype);

    let val: c_int = xml_validate_attribute_value_internal(doc, (*attr_decl).atype, value);
    if val == 0 {
        xml_err_valid_node(
            ctxt,
            elem,
            XmlParserErrors::XmlDTDAttributeValue,
            c"Syntax of value for attribute %s of %s is not valid\n".as_ptr() as _,
            (*attr).name,
            (*elem).name,
            null_mut(),
        );
        ret = 0;
    }

    /* Validity constraint: Fixed Attribute Default */
    if matches!((*attr_decl).def, XmlAttributeDefault::XmlAttributeFixed)
        && !xml_str_equal(value, (*attr_decl).default_value)
    {
        xml_err_valid_node(
            ctxt,
            elem,
            XmlParserErrors::XmlDTDAttributeDefault,
            c"Value for attribute %s of %s is different from default \"%s\"\n".as_ptr() as _,
            (*attr).name,
            (*elem).name,
            (*attr_decl).default_value,
        );
        ret = 0;
    }

    /* Validity Constraint: ID uniqueness */
    if matches!((*attr_decl).atype, XmlAttributeType::XmlAttributeId)
        && xml_add_id(ctxt, doc, value, attr).is_null()
    {
        ret = 0;
    }

    if matches!(
        (*attr_decl).atype,
        XmlAttributeType::XmlAttributeIdref | XmlAttributeType::XmlAttributeIdrefs
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
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlDTDUnknownNotation,
                c"Value \"%s\" for attribute %s of %s is not a declared Notation\n".as_ptr() as _,
                value,
                (*attr).name,
                (*elem).name,
            );
            ret = 0;
        }

        /* Second, verify that it's among the list */
        while !tree.is_null() {
            if xml_str_equal((*tree).name, value) {
                break;
            }
            tree = (*tree).next;
        }
        if tree.is_null() {
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlDTDNotationValue,
                c"Value \"%s\" for attribute %s of %s is not among the enumerated notations\n"
                    .as_ptr() as _,
                value,
                (*attr).name,
                (*elem).name,
            );
            ret = 0;
        }
    }

    /* Validity Constraint: Enumeration */
    if matches!(
        (*attr_decl).atype,
        XmlAttributeType::XmlAttributeEnumeration
    ) {
        let mut tree: XmlEnumerationPtr = (*attr_decl).tree;
        while !tree.is_null() {
            if xml_str_equal((*tree).name, value) {
                break;
            }
            tree = (*tree).next;
        }
        if tree.is_null() {
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlDTDAttributeValue,
                c"Value \"%s\" for attribute %s of %s is not among the enumerated set\n".as_ptr()
                    as _,
                value,
                (*attr).name,
                (*elem).name,
            );
            ret = 0;
        }
    }

    /* Fixed Attribute Default */
    if matches!((*attr_decl).def, XmlAttributeDefault::XmlAttributeFixed)
        && !xml_str_equal((*attr_decl).default_value, value)
    {
        xml_err_valid_node(
            ctxt,
            elem,
            XmlParserErrors::XmlDTDAttributeValue,
            c"Value for attribute %s of %s must be \"%s\"\n".as_ptr() as _,
            (*attr).name,
            (*elem).name,
            (*attr_decl).default_value,
        );
        ret = 0;
    }

    /* Extra check for the attribute value */
    ret &= xml_validate_attribute_value2(ctxt, doc, (*attr).name, (*attr_decl).atype, value);

    ret
}

/**
 * xmlValidateOneNamespace:
 * @ctxt:  the validation context
 * @doc:  a document instance
 * @elem:  an element instance
 * @prefix:  the namespace prefix
 * @ns:  an namespace declaration instance
 * @value:  the attribute value (without entities processing)
 *
 * Try to validate a single namespace declaration for an element
 * basically it does the following checks as described by the
 * XML-1.0 recommendation:
 *  - [ VC: Attribute Value Type ]
 *  - [ VC: Fixed Attribute Default ]
 *  - [ VC: Entity Name ]
 *  - [ VC: Name Token ]
 *  - [ VC: ID ]
 *  - [ VC: IDREF ]
 *  - [ VC: Entity Name ]
 *  - [ VC: Notation Attributes ]
 *
 * The ID/IDREF uniqueness and matching are done separately
 *
 * returns 1 if valid or 0 otherwise
 */
#[cfg(feature = "valid")]
pub unsafe extern "C" fn xml_validate_one_namespace(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    elem: XmlNodePtr,
    prefix: *const XmlChar,
    ns: XmlNsPtr,
    value: *const XmlChar,
) -> c_int {
    /* let elemDecl: xmlElementPtr; */
    let mut attr_decl: XmlAttributePtr = null_mut();
    let mut ret: c_int = 1;

    CHECK_DTD!(doc);
    if elem.is_null() || (*elem).name.is_null() {
        return 0;
    }
    if ns.is_null() || (*ns).href.load(Ordering::Relaxed).is_null() {
        return 0;
    }

    if !prefix.is_null() {
        let mut fname: [XmlChar; 50] = [0; 50];

        let fullname: *mut XmlChar = xml_build_qname((*elem).name, prefix, fname.as_mut_ptr(), 50);
        if fullname.is_null() {
            xml_verr_memory(ctxt, c"Validating namespace".as_ptr() as _);
            return 0;
        }
        if !(*ns).prefix.load(Ordering::Relaxed).is_null() {
            attr_decl = xml_get_dtd_qattr_desc(
                (*doc).int_subset,
                fullname,
                (*ns).prefix.load(Ordering::Relaxed) as _,
                c"xmlns".as_ptr() as _,
            );
            if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                attr_decl = xml_get_dtd_qattr_desc(
                    (*doc).ext_subset,
                    fullname,
                    (*ns).prefix.load(Ordering::Relaxed) as _,
                    c"xmlns".as_ptr() as _,
                );
            }
        } else {
            attr_decl = xml_get_dtd_attr_desc((*doc).int_subset, fullname, c"xmlns".as_ptr() as _);
            if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                attr_decl =
                    xml_get_dtd_attr_desc((*doc).ext_subset, fullname, c"xmlns".as_ptr() as _);
            }
        }
        if fullname != fname.as_ptr() as _ && fullname != (*elem).name as _ {
            xml_free(fullname as _);
        }
    }
    if attr_decl.is_null() {
        if !(*ns).prefix.load(Ordering::Relaxed).is_null() {
            attr_decl = xml_get_dtd_qattr_desc(
                (*doc).int_subset,
                (*elem).name,
                (*ns).prefix.load(Ordering::Relaxed) as _,
                c"xmlns".as_ptr() as _,
            );
            if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                attr_decl = xml_get_dtd_qattr_desc(
                    (*doc).ext_subset,
                    (*elem).name,
                    (*ns).prefix.load(Ordering::Relaxed) as _,
                    c"xmlns".as_ptr() as _,
                );
            }
        } else {
            attr_decl =
                xml_get_dtd_attr_desc((*doc).int_subset, (*elem).name, c"xmlns".as_ptr() as _);
            if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                attr_decl =
                    xml_get_dtd_attr_desc((*doc).ext_subset, (*elem).name, c"xmlns".as_ptr() as _);
            }
        }
    }

    /* Validity Constraint: Attribute Value Type */
    if attr_decl.is_null() {
        if !(*ns).prefix.load(Ordering::Relaxed).is_null() {
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlDTDUnknownAttribute,
                c"No declaration for attribute xmlns:%s of element %s\n".as_ptr() as _,
                (*ns).prefix.load(Ordering::Relaxed) as _,
                (*elem).name,
                null_mut(),
            );
        } else {
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlDTDUnknownAttribute,
                c"No declaration for attribute xmlns of element %s\n".as_ptr() as _,
                (*elem).name,
                null_mut(),
                null_mut(),
            );
        }
        return 0;
    }

    let val: c_int = xml_validate_attribute_value_internal(doc, (*attr_decl).atype, value);
    if val == 0 {
        if !(*ns).prefix.load(Ordering::Relaxed).is_null() {
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlDTDInvalidDefault,
                c"Syntax of value for attribute xmlns:%s of %s is not valid\n".as_ptr() as _,
                (*ns).prefix.load(Ordering::Relaxed) as _,
                (*elem).name,
                null_mut(),
            );
        } else {
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlDTDInvalidDefault,
                c"Syntax of value for attribute xmlns of %s is not valid\n".as_ptr() as _,
                (*elem).name,
                null_mut(),
                null_mut(),
            );
        }
        ret = 0;
    }

    /* Validity constraint: Fixed Attribute Default */
    if matches!((*attr_decl).def, XmlAttributeDefault::XmlAttributeFixed)
        && !xml_str_equal(value, (*attr_decl).default_value)
    {
        if !(*ns).prefix.load(Ordering::Relaxed).is_null() {
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlDTDAttributeDefault,
                c"Value for attribute xmlns:%s of %s is different from default \"%s\"\n".as_ptr()
                    as _,
                (*ns).prefix.load(Ordering::Relaxed) as _,
                (*elem).name,
                (*attr_decl).default_value,
            );
        } else {
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlDTDAttributeDefault,
                c"Value for attribute xmlns of %s is different from default \"%s\"\n".as_ptr() as _,
                (*elem).name,
                (*attr_decl).default_value,
                null_mut(),
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
    //     /* Validity Constraint: ID uniqueness */
    //     if ((*attrDecl).atype == XML_ATTRIBUTE_ID) {
    //         if (xmlAddID(ctxt, doc, value, (xmlAttrPtr) ns).is_null())
    // 	    ret = 0;
    //     }

    //     if (((*attrDecl).atype == XML_ATTRIBUTE_IDREF) ||
    // 	((*attrDecl).atype == XML_ATTRIBUTE_IDREFS)) {
    //         if (xmlAddRef(ctxt, doc, value, (xmlAttrPtr) ns).is_null())
    // 	    ret = 0;
    //     }
    // #endif

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
            if !(*ns).prefix.load(Ordering::Relaxed).is_null() {
                xml_err_valid_node(
                    ctxt,
                    elem,
                    XmlParserErrors::XmlDTDUnknownNotation,
                    c"Value \"%s\" for attribute xmlns:%s of %s is not a declared Notation\n"
                        .as_ptr() as _,
                    value,
                    (*ns).prefix.load(Ordering::Relaxed) as _,
                    (*elem).name,
                );
            } else {
                xml_err_valid_node(
                    ctxt,
                    elem,
                    XmlParserErrors::XmlDTDUnknownNotation,
                    c"Value \"%s\" for attribute xmlns of %s is not a declared Notation\n".as_ptr()
                        as _,
                    value,
                    (*elem).name,
                    null_mut(),
                );
            }
            ret = 0;
        }

        /* Second, verify that it's among the list */
        while !tree.is_null() {
            if xml_str_equal((*tree).name, value) {
                break;
            }
            tree = (*tree).next;
        }
        if tree.is_null() {
            if !(*ns).prefix.load(Ordering::Relaxed).is_null() {
                xml_err_valid_node(ctxt, elem, XmlParserErrors::XmlDTDNotationValue, c"Value \"%s\" for attribute xmlns:%s of %s is not among the enumerated notations\n".as_ptr() as _, value, (*ns).prefix.load(Ordering::Relaxed) as _, (*elem).name);
            } else {
                xml_err_valid_node(ctxt, elem, XmlParserErrors::XmlDTDNotationValue, c"Value \"%s\" for attribute xmlns of %s is not among the enumerated notations\n".as_ptr() as _, value, (*elem).name, null_mut());
            }
            ret = 0;
        }
    }

    /* Validity Constraint: Enumeration */
    if matches!(
        (*attr_decl).atype,
        XmlAttributeType::XmlAttributeEnumeration
    ) {
        let mut tree: XmlEnumerationPtr = (*attr_decl).tree;
        while !tree.is_null() {
            if xml_str_equal((*tree).name, value) {
                break;
            }
            tree = (*tree).next;
        }
        if tree.is_null() {
            if !(*ns).prefix.load(Ordering::Relaxed).is_null() {
                xml_err_valid_node(
                    ctxt,
                    elem,
                    XmlParserErrors::XmlDTDAttributeValue,
                    c"Value \"%s\" for attribute xmlns:%s of %s is not among the enumerated set\n"
                        .as_ptr() as _,
                    value,
                    (*ns).prefix.load(Ordering::Relaxed) as _,
                    (*elem).name,
                );
            } else {
                xml_err_valid_node(
                    ctxt,
                    elem,
                    XmlParserErrors::XmlDTDAttributeValue,
                    c"Value \"%s\" for attribute xmlns of %s is not among the enumerated set\n"
                        .as_ptr() as _,
                    value,
                    (*elem).name,
                    null_mut(),
                );
            }
            ret = 0;
        }
    }

    /* Fixed Attribute Default */
    if matches!((*attr_decl).def, XmlAttributeDefault::XmlAttributeFixed)
        && !xml_str_equal((*attr_decl).default_value, value)
    {
        if !(*ns).prefix.load(Ordering::Relaxed).is_null() {
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlDTDElemNamespace,
                c"Value for attribute xmlns:%s of %s must be \"%s\"\n".as_ptr() as _,
                (*ns).prefix.load(Ordering::Relaxed) as _,
                (*elem).name,
                (*attr_decl).default_value,
            );
        } else {
            xml_err_valid_node(
                ctxt,
                elem,
                XmlParserErrors::XmlDTDElemNamespace,
                c"Value for attribute xmlns of %s must be \"%s\"\n".as_ptr() as _,
                (*elem).name,
                (*attr_decl).default_value,
                null_mut(),
            );
        }
        ret = 0;
    }

    /* Extra check for the attribute value */
    if !(*ns).prefix.load(Ordering::Relaxed).is_null() {
        ret &= xml_validate_attribute_value2(
            ctxt,
            doc,
            (*ns).prefix.load(Ordering::Relaxed) as _,
            (*attr_decl).atype,
            value,
        );
    } else {
        ret &= xml_validate_attribute_value2(
            ctxt,
            doc,
            c"xmlns".as_ptr() as _,
            (*attr_decl).atype,
            value,
        );
    }

    ret
}

pub type XmlValidateMemoPtr = *mut XmlValidateMemo;
pub struct XmlValidateMemo {
    ctxt: XmlValidCtxtPtr,
    name: *const XmlChar,
}

/**
 * xmlValidateRef:
 * @ref:   A reference to be validated
 * @ctxt:  Validation context
 * @name:  Name of ID we are searching for
 *
 */
unsafe extern "C" fn xml_validate_ref(
    refe: XmlRefPtr,
    ctxt: XmlValidCtxtPtr,
    name: *const XmlChar,
) {
    let mut id: XmlAttrPtr;

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
            id = xml_get_id((*ctxt).doc, str);
            if id.is_null() {
                xml_err_valid_node_nr(
                    ctxt,
                    null_mut(),
                    XmlParserErrors::XmlDTDUnknownID,
                    c"attribute %s line %d references an unknown ID \"%s\"\n".as_ptr() as _,
                    (*refe).name,
                    (*refe).lineno,
                    str,
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
    } else if matches!((*attr).atype, Some(XmlAttributeType::XmlAttributeIdref)) {
        id = xml_get_id((*ctxt).doc, name);
        if id.is_null() {
            xml_err_valid_node(
                ctxt,
                (*attr).parent,
                XmlParserErrors::XmlDTDUnknownID,
                c"IDREF attribute %s references an unknown ID \"%s\"\n".as_ptr() as _,
                (*attr).name,
                name,
                null_mut(),
            );
            (*ctxt).valid = 0;
        }
    } else if matches!((*attr).atype, Some(XmlAttributeType::XmlAttributeIdrefs)) {
        let mut str: *mut XmlChar;
        let mut cur: *mut XmlChar;
        let mut save: XmlChar;

        let dup: *mut XmlChar = xml_strdup(name);
        if dup.is_null() {
            xml_verr_memory(ctxt, c"IDREFS split".as_ptr() as _);
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
            id = xml_get_id((*ctxt).doc, str);
            if id.is_null() {
                xml_err_valid_node(
                    ctxt,
                    (*attr).parent,
                    XmlParserErrors::XmlDTDUnknownID,
                    c"IDREFS attribute %s references an unknown ID \"%s\"\n".as_ptr() as _,
                    (*attr).name,
                    str,
                    null_mut(),
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

/**
 * xmlWalkValidateList:
 * @data:  Contents of current link
 * @user:  Value supplied by the user
 *
 * Returns 0 to abort the walk or 1 to continue
 */
extern "C" fn xml_walk_validate_list(data: *const c_void, user: *mut c_void) -> c_int {
    unsafe {
        let memo: XmlValidateMemoPtr = user as XmlValidateMemoPtr;
        xml_validate_ref(data as XmlRefPtr, (*memo).ctxt, (*memo).name);
    }
    1
}

/**
 * xmlValidateCheckRefCallback:
 * @ref_list:  List of references
 * @ctxt:  Validation context
 * @name:  Name of ID we are searching for
 *
 */
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

/**
 * xmlValidateDocumentFinal:
 * @ctxt:  the validation context
 * @doc:  a document instance
 *
 * Does the final step for the document validation once all the
 * incremental validation steps have been completed
 *
 * basically it does the following checks described by the XML Rec
 *
 * Check all the IDREF/IDREFS attributes definition for validity
 *
 * returns 1 if valid or 0 otherwise
 */
#[cfg(feature = "valid")]
pub unsafe extern "C" fn xml_validate_document_final(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
) -> c_int {
    if ctxt.is_null() {
        return 0;
    }
    if doc.is_null() {
        xml_err_valid(
            ctxt,
            XmlParserErrors::XmlDTDNoDoc,
            c"xmlValidateDocumentFinal: doc == NULL\n".as_ptr() as _,
            null_mut(),
        );
        return 0;
    }

    /* trick to get correct line id report */
    let save: c_uint = (*ctxt).flags;
    (*ctxt).flags &= !XML_VCTXT_USE_PCTXT as u32;

    /*
     * Check all the NOTATION/NOTATIONS attributes
     */
    /*
     * Check all the ENTITY/ENTITIES attributes definition for validity
     */
    /*
     * Check all the IDREF/IDREFS attributes definition for validity
     */
    let table: XmlRefTablePtr = (*doc).refs as XmlRefTablePtr;
    (*ctxt).doc = doc;
    (*ctxt).valid = 1;
    xml_hash_scan(table, Some(xml_validate_check_ref_callback), ctxt as _);

    (*ctxt).flags = save;
    (*ctxt).valid
}

/**
 * xmlValidateNotationUse:
 * @ctxt:  the validation context
 * @doc:  the document
 * @notationName:  the notation name to check
 *
 * Validate that the given name match a notation declaration.
 * - [ VC: Notation Declared ]
 *
 * returns 1 if valid or 0 otherwise
 */
#[cfg(any(feature = "valid", feature = "schema"))]
pub unsafe extern "C" fn xml_validate_notation_use(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    notation_name: *const XmlChar,
) -> c_int {
    let mut nota_decl: XmlNotationPtr;
    if doc.is_null() || (*doc).int_subset.is_null() || notation_name.is_null() {
        return -1;
    }

    nota_decl = xml_get_dtd_notation_desc((*doc).int_subset, notation_name);
    if nota_decl.is_null() && !(*doc).ext_subset.is_null() {
        nota_decl = xml_get_dtd_notation_desc((*doc).ext_subset, notation_name);
    }

    if nota_decl.is_null() && !ctxt.is_null() {
        xml_err_valid_node(
            ctxt,
            doc as XmlNodePtr,
            XmlParserErrors::XmlDTDUnknownNotation,
            c"NOTATION %s is not declared\n".as_ptr() as _,
            notation_name,
            null_mut(),
            null_mut(),
        );
        return 0;
    }
    1
}

/**
 * xmlIsMixedElement:
 * @doc:  the document
 * @name:  the element name
 *
 * Search in the DtDs whether an element accept Mixed content (or ANY)
 * basically if it is supposed to accept text childs
 *
 * returns 0 if no, 1 if yes, and -1 if no element description is available
 */
pub unsafe extern "C" fn xml_is_mixed_element(doc: XmlDocPtr, name: *const XmlChar) -> c_int {
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

/**
 * xmlGetDtdAttrDesc:
 * @dtd:  a pointer to the DtD to search
 * @elem:  the element name
 * @name:  the attribute name
 *
 * Search the DTD for the description of this attribute on
 * this element.
 *
 * returns the xmlAttributePtr if found or null_mut()
 */
pub unsafe extern "C" fn xml_get_dtd_attr_desc(
    dtd: XmlDtdPtr,
    elem: *const XmlChar,
    name: *const XmlChar,
) -> XmlAttributePtr {
    let cur: XmlAttributePtr;

    let mut prefix: *mut XmlChar = null_mut();

    if dtd.is_null() {
        return null_mut();
    }
    if (*dtd).attributes.is_null() {
        return null_mut();
    }

    let table: XmlAttributeTablePtr = (*dtd).attributes as XmlAttributeTablePtr;
    if table.is_null() {
        return null_mut();
    }

    let uqname: *mut XmlChar = xml_split_qname2(name, addr_of_mut!(prefix) as _) as _;

    if !uqname.is_null() {
        cur = xml_hash_lookup3(table, uqname, prefix, elem) as _;
        if !prefix.is_null() {
            xml_free(prefix as _);
        }
        if !uqname.is_null() {
            xml_free(uqname as _);
        }
    } else {
        cur = xml_hash_lookup3(table, name, null_mut(), elem) as _;
    }
    cur
}

/**
 * xmlGetDtdQAttrDesc:
 * @dtd:  a pointer to the DtD to search
 * @elem:  the element name
 * @name:  the attribute name
 * @prefix:  the attribute namespace prefix
 *
 * Search the DTD for the description of this qualified attribute on
 * this element.
 *
 * returns the xmlAttributePtr if found or null_mut()
 */
pub unsafe extern "C" fn xml_get_dtd_qattr_desc(
    dtd: XmlDtdPtr,
    elem: *const XmlChar,
    name: *const XmlChar,
    prefix: *const XmlChar,
) -> XmlAttributePtr {
    if dtd.is_null() {
        return null_mut();
    }
    if (*dtd).attributes.is_null() {
        return null_mut();
    }
    let table: XmlAttributeTablePtr = (*dtd).attributes as XmlAttributeTablePtr;

    xml_hash_lookup3(table, name, prefix, elem) as _
}

/**
 * xmlGetDtdNotationDesc:
 * @dtd:  a pointer to the DtD to search
 * @name:  the notation name
 *
 * Search the DTD for the description of this notation
 *
 * returns the xmlNotationPtr if found or null_mut()
 */
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

/**
 * xmlGetDtdQElementDesc:
 * @dtd:  a pointer to the DtD to search
 * @name:  the element name
 * @prefix:  the element namespace prefix
 *
 * Search the DTD for the description of this element
 *
 * returns the xmlElementPtr if found or null_mut()
 */
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

/**
 * xmlGetDtdElementDesc:
 * @dtd:  a pointer to the DtD to search
 * @name:  the element name
 *
 * Search the DTD for the description of this element
 *
 * returns the xmlElementPtr if found or null_mut()
 */
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

/**
 * xmlValidGetPotentialChildren:
 * @ctree:  an element content tree
 * @names:  an array to store the list of child names
 * @len:  a pointer to the number of element in the list
 * @max:  the size of the array
 *
 * Build/extend a list of  potential children allowed by the content tree
 *
 * returns the number of element in the list, or -1 in case of error.
 */
#[cfg(feature = "valid")]
pub unsafe extern "C" fn xml_valid_get_potential_children(
    ctree: *mut XmlElementContent,
    names: *mut *const XmlChar,
    len: *mut c_int,
    max: c_int,
) -> c_int {
    if ctree.is_null() || names.is_null() || len.is_null() {
        return -1;
    }
    if *len >= max {
        return *len;
    }

    match (*ctree).typ {
        XmlElementContentType::XmlElementContentPcdata => {
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

/*
 * Dummy function to suppress messages while we try out valid elements
 */
fn xml_no_validity_err(_ctx: Option<GenericErrorContext>, _msg: &str) {}

/**
 * xmlValidGetValidElements:
 * @prev:  an element to insert after
 * @next:  an element to insert next
 * @names:  an array to store the list of child names
 * @max:  the size of the array
 *
 * This function returns the list of authorized children to insert
 * within an existing tree while respecting the validity constraints
 * forced by the Dtd. The insertion point is defined using @prev and
 * @next in the following ways:
 *  to insert before 'node': xmlValidGetValidElements((*node).prev, node, ...
 *  to insert next 'node': xmlValidGetValidElements(node, (*node).next, ...
 *  to replace 'node': xmlValidGetValidElements((*node).prev, (*node).next, ...
 *  to prepend a child to 'node': xmlValidGetValidElements(null_mut(), (*node).childs,
 *  to append a child to 'node': xmlValidGetValidElements((*node).last, null_mut(), ...
 *
 * pointers to the element names are inserted at the beginning of the array
 * and do not need to be freed.
 *
 * returns the number of element in the list, or -1 in case of error. If
 *    the function returns the value @max the caller is invited to grow the
 *    receiving array and retry.
 */
#[cfg(feature = "valid")]
pub unsafe extern "C" fn xml_valid_get_valid_elements(
    prev: *mut XmlNode,
    next: *mut XmlNode,
    names: *mut *const XmlChar,
    max: c_int,
) -> c_int {
    let mut vctxt: XmlValidCtxt = unsafe { zeroed() };
    let mut nb_valid_elements: c_int;
    let mut elements: [*const XmlChar; 256] = [null(); 256];
    let mut nb_elements: c_int = 0;
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
    let parent: *mut XmlNode = (*ref_node).parent;

    /*
     * Retrieves the parent element declaration
     */
    element_desc = xml_get_dtd_element_desc((*(*parent).doc).int_subset, (*parent).name);
    if element_desc.is_null() && !(*(*parent).doc).ext_subset.is_null() {
        element_desc = xml_get_dtd_element_desc((*(*parent).doc).ext_subset, (*parent).name);
    }
    if element_desc.is_null() {
        return -1;
    }

    /*
     * Do a backup of the current tree structure
     */
    let prev_next: *mut XmlNode = if !prev.is_null() {
        (*prev).next
    } else {
        null_mut()
    };
    let next_prev: *mut XmlNode = if !next.is_null() {
        (*next).prev
    } else {
        null_mut()
    };
    let parent_childs: *mut XmlNode = (*parent).children;
    let parent_last: *mut XmlNode = (*parent).last;

    /*
     * Creates a dummy node and insert it into the tree
     */
    let test_node: *mut XmlNode = xml_new_doc_node(
        (*ref_node).doc,
        null_mut(),
        c"<!dummy?>".as_ptr() as _,
        null_mut(),
    );
    if test_node.is_null() {
        return -1;
    }

    (*test_node).parent = parent;
    (*test_node).prev = prev;
    (*test_node).next = next;
    let name: *const XmlChar = (*test_node).name;

    if !prev.is_null() {
        (*prev).next = test_node;
    } else {
        (*parent).children = test_node;
    }

    if !next.is_null() {
        (*next).prev = test_node;
    } else {
        (*parent).last = test_node;
    }

    /*
     * Insert each potential child node and check if the parent is
     * still valid
     */
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

    /*
     * Restore the tree structure
     */
    if !prev.is_null() {
        (*prev).next = prev_next;
    }
    if !next.is_null() {
        (*next).prev = next_prev;
    }
    (*parent).children = parent_childs;
    (*parent).last = parent_last;

    /*
     * Free up the dummy node
     */
    (*test_node).name = name;
    xml_free_node(test_node);

    nb_valid_elements
}

/**
 * xmlValidateNameValue:
 * @doc:  pointer to the document or null_mut()
 * @value:  an Name value
 *
 * Validate that the given value match Name production
 *
 * returns 1 if valid or 0 otherwise
 */

/**
 * xmlValidateNameValue:
 * @value:  an Name value
 *
 * Validate that the given value match Name production
 *
 * returns 1 if valid or 0 otherwise
 */
#[cfg(feature = "valid")]
pub unsafe extern "C" fn xml_validate_name_value(value: *const XmlChar) -> c_int {
    xml_validate_name_value_internal(null_mut(), value)
}
/**
 * xmlValidateNamesValue:
 * @value:  an Names value
 *
 * Validate that the given value match Names production
 *
 * returns 1 if valid or 0 otherwise
 */
#[cfg(feature = "valid")]
pub unsafe extern "C" fn xml_validate_names_value(value: *const XmlChar) -> c_int {
    xml_validate_names_value_internal(null_mut(), value)
}

/**
 * xmlValidateNmtokenValue:
 * @value:  an Nmtoken value
 *
 * Validate that the given value match Nmtoken production
 *
 * [ VC: Name Token ]
 *
 * returns 1 if valid or 0 otherwise
 */
#[cfg(feature = "valid")]
pub unsafe extern "C" fn xml_validate_nmtoken_value(value: *const XmlChar) -> c_int {
    xml_validate_nmtoken_value_internal(null_mut(), value)
}

/**
 * xmlValidateNmtokensValue:
 * @value:  an Nmtokens value
 *
 * Validate that the given value match Nmtokens production
 *
 * [ VC: Name Token ]
 *
 * returns 1 if valid or 0 otherwise
 */
#[cfg(feature = "valid")]
pub unsafe extern "C" fn xml_validate_nmtokens_value(value: *const XmlChar) -> c_int {
    xml_validate_nmtokens_value_internal(null_mut(), value)
}

/**
 * xmlValidBuildAContentModel:
 * @content:  the content model
 * @ctxt:  the schema parser context
 * @name:  the element name whose content is being built
 *
 * Generate the automata sequence needed for that type
 *
 * Returns 1 if successful or 0 in case of error.
 */
unsafe extern "C" fn xml_valid_build_acontent_model(
    mut content: XmlElementContentPtr,
    ctxt: XmlValidCtxtPtr,
    name: *const XmlChar,
) -> c_int {
    if content.is_null() {
        xml_err_valid_node(
            ctxt,
            null_mut(),
            XmlParserErrors::XmlErrInternalError,
            c"Found NULL content in content model of %s\n".as_ptr() as _,
            name,
            null_mut(),
            null_mut(),
        );
        return 0;
    }
    match (*content).typ {
        XmlElementContentType::XmlElementContentPcdata => {
            xml_err_valid_node(
                ctxt,
                null_mut(),
                XmlParserErrors::XmlErrInternalError,
                c"Found PCDATA in content model of %s\n".as_ptr() as _,
                name,
                null_mut(),
                null_mut(),
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
                xml_verr_memory(ctxt, c"Building content model".as_ptr() as _);
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
          //     xml_err_valid(
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

/*
 * Validation based on the regexp support
 */
/**
 * xmlValidBuildContentModel:
 * @ctxt:  a validation context
 * @elem:  an element declaration node
 *
 * (Re)Build the automata associated to the content model of this
 * element
 *
 * Returns 1 in case of success, 0 in case of error
 */
#[cfg(all(feature = "valid", feature = "regexp"))]
pub unsafe extern "C" fn xml_valid_build_content_model(
    ctxt: XmlValidCtxtPtr,
    elem: XmlElementPtr,
) -> c_int {
    if ctxt.is_null() || elem.is_null() {
        return 0;
    }
    if !matches!((*elem).typ, XmlElementType::XmlElementDecl) {
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
    if (*ctxt).am.is_null() {
        xml_err_valid_node(
            ctxt,
            elem as XmlNodePtr,
            XmlParserErrors::XmlErrInternalError,
            c"Cannot create automata for element %s\n".as_ptr() as _,
            (*elem).name,
            null_mut(),
            null_mut(),
        );
        return 0;
    }
    (*ctxt).state = xml_automata_get_init_state((*ctxt).am);
    xml_valid_build_acontent_model((*elem).content, ctxt, (*elem).name);
    xml_automata_set_final_state((*ctxt).am, (*ctxt).state);
    (*elem).cont_model = xml_automata_compile((*ctxt).am);
    if xml_regexp_is_determinist((*elem).cont_model) != 1 {
        let mut expr: [c_char; 5000] = [0; 5000];
        expr[0] = 0;
        xml_snprintf_element_content(expr.as_mut_ptr() as _, 5000, (*elem).content, 1);
        xml_err_valid_node(
            ctxt,
            elem as XmlNodePtr,
            XmlParserErrors::XmlDTDContentNotDeterminist,
            c"Content model of %s is not deterministic: %s\n".as_ptr() as _,
            (*elem).name,
            expr.as_ptr() as _,
            null_mut(),
        );
        // #ifdef DEBUG_REGEXP_ALGO
        //         xmlRegexpPrint(stderr, (*elem).contModel);
        // #endif
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
/**
 * xmlValidateCheckMixed:
 * @ctxt:  the validation context
 * @cont:  the mixed content model
 * @qname:  the qualified name as appearing in the serialization
 *
 * Check if the given node is part of the content model.
 *
 * Returns 1 if yes, 0 if no, -1 in case of error
 */
#[cfg(feature = "regexp")]
unsafe extern "C" fn xml_validate_check_mixed(
    ctxt: XmlValidCtxtPtr,
    mut cont: XmlElementContentPtr,
    qname: *const XmlChar,
) -> c_int {
    let mut plen: c_int = 0;
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
                    XmlElementContentType::XmlElementContentPcdata
                )
            {
                xml_err_valid(
                    null_mut(),
                    XmlParserErrors::XmlDTDMixedCorrupt,
                    c"Internal: MIXED struct corrupted\n".as_ptr() as _,
                    null_mut(),
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
                    XmlElementContentType::XmlElementContentPcdata
                )
            {
                xml_err_valid(
                    ctxt,
                    XmlParserErrors::XmlDTDMixedCorrupt,
                    c"Internal: MIXED struct corrupted\n".as_ptr() as _,
                    null_mut(),
                );
                break;
            }
            cont = (*cont).c2;
        }
    }
    0
}

#[cfg(feature = "regexp")]
unsafe extern "C" fn vstate_vpush(
    ctxt: XmlValidCtxtPtr,
    elem_decl: XmlElementPtr,
    node: XmlNodePtr,
) -> c_int {
    if (*ctxt).vstate_max == 0 || (*ctxt).vstate_tab.is_null() {
        (*ctxt).vstate_max = 10;
        (*ctxt).vstate_tab =
            xml_malloc((*ctxt).vstate_max as usize * size_of_val(&*(*ctxt).vstate_tab.add(0)))
                as *mut XmlValidState;
        if (*ctxt).vstate_tab.is_null() {
            xml_verr_memory(ctxt as _, c"malloc failed".as_ptr() as _);
            return -1;
        }
    }

    if (*ctxt).vstate_nr >= (*ctxt).vstate_max {
        let tmp: *mut XmlValidState = xml_realloc(
            (*ctxt).vstate_tab as _,
            2 * (*ctxt).vstate_max as usize * size_of_val(&*(*ctxt).vstate_tab.add(0)),
        ) as *mut XmlValidState;
        if tmp.is_null() {
            xml_verr_memory(ctxt as _, c"realloc failed".as_ptr() as _);
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
            xml_err_valid_node(
                ctxt,
                elem_decl as XmlNodePtr,
                XmlParserErrors::XmlErrInternalError,
                c"Failed to build content model regexp for %s\n".as_ptr() as _,
                (*node).name,
                null_mut(),
                null_mut(),
            );
        }
    }

    let res = (*ctxt).vstate_nr;
    (*ctxt).vstate_nr += 1;
    res
}

#[cfg(not(feature = "regexp"))]
const MAX_RECURSE: usize = 25000;

#[cfg(not(feature = "regexp"))]
unsafe extern "C" fn vstateVPush(
    ctxt: XmlValidCtxtPtr,
    cont: XmlElementContentPtr,
    node: XmlNodePtr,
    depth: c_uchar,
    occurs: c_long,
    state: c_uchar,
) -> c_int {
    let i: c_int = (*ctxt).vstateNr - 1;

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

/**
 * xmlValidatePushElement:
 * @ctxt:  the validation context
 * @doc:  a document instance
 * @elem:  an element instance
 * @qname:  the qualified name as appearing in the serialization
 *
 * Push a new element start on the validation stack.
 *
 * returns 1 if no validation problem was found or 0 otherwise
 */
#[cfg(all(feature = "valid", feature = "regexp"))]
pub unsafe extern "C" fn xml_validate_push_element(
    ctxt: XmlValidCtxtPtr,
    doc: XmlDocPtr,
    elem: XmlNodePtr,
    qname: *const XmlChar,
) -> c_int {
    let mut ret: c_int = 1;
    let mut extsubset: c_int = 0;

    if ctxt.is_null() {
        return 0;
    }
    /* printf("PushElem %s\n", qname); */
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
                    xml_err_valid_node(
                        ctxt,
                        (*state).node,
                        XmlParserErrors::XmlDTDNotEmpty,
                        c"Element %s was declared EMPTY this one has content\n".as_ptr() as _,
                        (*(*state).node).name,
                        null_mut(),
                        null_mut(),
                    );
                    ret = 0;
                }
                XmlElementTypeVal::XmlElementTypeAny => {
                    /* I don't think anything is required then */
                }
                XmlElementTypeVal::XmlElementTypeMixed => {
                    /* simple case of declared as #PCDATA */
                    if !(*elem_decl).content.is_null()
                        && (*(*elem_decl).content).typ
                            == XmlElementContentType::XmlElementContentPcdata
                    {
                        xml_err_valid_node(
                            ctxt,
                            (*state).node,
                            XmlParserErrors::XmlDTDNotPCDATA,
                            c"Element %s was declared #PCDATA but contains non text nodes\n"
                                .as_ptr() as _,
                            (*(*state).node).name,
                            null_mut(),
                            null_mut(),
                        );
                        ret = 0;
                    } else {
                        ret = xml_validate_check_mixed(ctxt, (*elem_decl).content, qname);
                        if ret != 1 {
                            xml_err_valid_node(
                                ctxt,
                                (*state).node,
                                XmlParserErrors::XmlDTDInvalidChild,
                                c"Element %s is not declared in %s list of possible children\n"
                                    .as_ptr() as _,
                                qname,
                                (*(*state).node).name,
                                null_mut(),
                            );
                        }
                    }
                }
                XmlElementTypeVal::XmlElementTypeElement => {
                    /*
                     * TODO:
                     * VC: Standalone Document Declaration
                     *     - element types with element content, if white space
                     *       occurs directly within any instance of those types.
                     */
                    if !(*state).exec.is_null() {
                        ret = xml_reg_exec_push_string((*state).exec, qname, null_mut());
                        if ret < 0 {
                            xml_err_valid_node(
                                ctxt,
                                (*state).node,
                                XmlParserErrors::XmlDTDContentModel,
                                c"Element %s content does not follow the DTD, Misplaced %s\n"
                                    .as_ptr() as _,
                                (*(*state).node).name,
                                qname,
                                null_mut(),
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

/**
 * xmlValidatePushCData:
 * @ctxt:  the validation context
 * @data:  some character data read
 * @len:  the length of the data
 *
 * check the CData parsed for validation in the current stack
 *
 * returns 1 if no validation problem was found or 0 otherwise
 */
#[cfg(all(feature = "valid", feature = "regexp"))]
pub unsafe extern "C" fn xml_validate_push_cdata(
    ctxt: XmlValidCtxtPtr,
    data: *const XmlChar,
    len: c_int,
) -> c_int {
    let mut ret: c_int = 1;

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
                    xml_err_valid_node(
                        ctxt,
                        (*state).node,
                        XmlParserErrors::XmlDTDNotEmpty,
                        c"Element %s was declared EMPTY this one has content\n".as_ptr() as _,
                        (*(*state).node).name,
                        null_mut(),
                        null_mut(),
                    );
                    ret = 0;
                }
                XmlElementTypeVal::XmlElementTypeAny => {}
                XmlElementTypeVal::XmlElementTypeMixed => {}
                XmlElementTypeVal::XmlElementTypeElement => {
                    for i in 0..len {
                        if !xml_is_blank_char(*data.add(i as usize) as u32) {
                            xml_err_valid_node(
                                ctxt,
                                (*state).node,
                                XmlParserErrors::XmlDTDContentModel,
                                c"Element %s content does not follow the DTD, Text not allowed\n"
                                    .as_ptr() as _,
                                (*(*state).node).name,
                                null_mut(),
                                null_mut(),
                            );
                            ret = 0;
                            // goto done;
                            return ret;
                        }
                    }
                    /*
                     * TODO:
                     * VC: Standalone Document Declaration
                     *  element types with element content, if white space
                     *  occurs directly within any instance of those types.
                     */
                }
            }
        }
    }
    // done:
    ret
}

#[cfg(feature = "regexp")]
unsafe extern "C" fn vstate_vpop(ctxt: XmlValidCtxtPtr) -> c_int {
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

#[cfg(not(feature = "regexp"))]
unsafe extern "C" fn vstateVPop(ctxt: XmlValidCtxtPtr) -> c_int {
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

/**
 * xmlValidatePopElement:
 * @ctxt:  the validation context
 * @doc:  a document instance
 * @elem:  an element instance
 * @qname:  the qualified name as appearing in the serialization
 *
 * Pop the element end from the validation stack.
 *
 * returns 1 if no validation problem was found or 0 otherwise
 */
#[cfg(all(feature = "valid", feature = "regexp"))]
pub unsafe extern "C" fn xml_validate_pop_element(
    ctxt: XmlValidCtxtPtr,
    _doc: XmlDocPtr,
    _elem: XmlNodePtr,
    _qname: *const XmlChar,
) -> c_int {
    let mut ret: c_int = 1;

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
                    xml_err_valid_node(
                        ctxt,
                        (*state).node,
                        XmlParserErrors::XmlDTDContentModel,
                        c"Element %s content does not follow the DTD, Expecting more children\n"
                            .as_ptr() as _,
                        (*(*state).node).name,
                        null_mut(),
                        null_mut(),
                    );
                    ret = 0;
                } else {
                    /*
                     * previous validation errors should not generate
                     * a new one here
                     */
                    ret = 1;
                }
            }
        }
        vstate_vpop(ctxt);
    }
    ret
}

/**
 * xmlValidateSkipIgnorable:
 * @ctxt:  the validation context
 * @child:  the child list
 *
 * Skip ignorable elements w.r.t. the validation process
 *
 * returns the first element to consider for validation of the content model
 */
#[cfg(not(feature = "regexp"))]
unsafe extern "C" fn xmlValidateSkipIgnorable(mut child: XmlNodePtr) -> XmlNodePtr {
    while !child.is_null() {
        match (*child).typ {
            /* These things are ignored (skipped) during validation.  */
            XmlElementType::XmlPiNode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlXincludeStart
            | XmlElementType::XmlXincludeEnd => {
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
    fn test_xml_add_attribute_decl() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_VALID_CTXT_PTR {
                for n_dtd in 0..GEN_NB_XML_DTD_PTR {
                    for n_elem in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            for n_ns in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                for n_type in 0..GEN_NB_XML_ATTRIBUTE_TYPE {
                                    for n_def in 0..GEN_NB_XML_ATTRIBUTE_DEFAULT {
                                        for n_default_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                            for n_tree in 0..GEN_NB_XML_ENUMERATION_PTR {
                                                let mem_base = xml_mem_blocks();
                                                let ctxt = gen_xml_valid_ctxt_ptr(n_ctxt, 0);
                                                let dtd = gen_xml_dtd_ptr(n_dtd, 1);
                                                let elem = gen_const_xml_char_ptr(n_elem, 2);
                                                let name = gen_const_xml_char_ptr(n_name, 3);
                                                let ns = gen_const_xml_char_ptr(n_ns, 4);
                                                let typ = gen_xml_attribute_type(n_type, 5);
                                                let def = gen_xml_attribute_default(n_def, 6);
                                                let default_value =
                                                    gen_const_xml_char_ptr(n_default_value, 7);
                                                let tree = gen_xml_enumeration_ptr(n_tree, 8);

                                                let ret_val = xml_add_attribute_decl(
                                                    ctxt,
                                                    dtd,
                                                    elem,
                                                    name,
                                                    ns,
                                                    typ,
                                                    def,
                                                    default_value,
                                                    tree,
                                                );
                                                desret_xml_attribute_ptr(ret_val);
                                                des_xml_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                                                des_xml_dtd_ptr(n_dtd, dtd, 1);
                                                des_const_xml_char_ptr(n_elem, elem, 2);
                                                des_const_xml_char_ptr(n_name, name, 3);
                                                des_const_xml_char_ptr(n_ns, ns, 4);
                                                des_xml_attribute_type(n_type, typ, 5);
                                                des_xml_attribute_default(n_def, def, 6);
                                                des_const_xml_char_ptr(
                                                    n_default_value,
                                                    default_value,
                                                    7,
                                                );
                                                des_xml_enumeration_ptr(n_tree, tree, 8);
                                                reset_last_error();
                                                if mem_base != xml_mem_blocks() {
                                                    leaks += 1;
                                                    eprint!("Leak of {} blocks found in xmlAddAttributeDecl", xml_mem_blocks() - mem_base);
                                                    assert!(leaks == 0, "{leaks} Leaks are found in xmlAddAttributeDecl()");
                                                    eprint!(" {}", n_ctxt);
                                                    eprint!(" {}", n_dtd);
                                                    eprint!(" {}", n_elem);
                                                    eprint!(" {}", n_name);
                                                    eprint!(" {}", n_ns);
                                                    eprint!(" {}", n_type);
                                                    eprint!(" {}", n_def);
                                                    eprint!(" {}", n_default_value);
                                                    eprintln!(" {}", n_tree);
                                                }
                                            }
                                        }
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
        #[cfg(feature = "output")]
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
        #[cfg(feature = "output")]
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
        #[cfg(feature = "output")]
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
        #[cfg(feature = "output")]
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
        #[cfg(feature = "output")]
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
        #[cfg(feature = "output")]
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
    fn test_xml_get_dtd_attr_desc() {
        unsafe {
            let mut leaks = 0;

            for n_dtd in 0..GEN_NB_XML_DTD_PTR {
                for n_elem in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let dtd = gen_xml_dtd_ptr(n_dtd, 0);
                        let elem = gen_const_xml_char_ptr(n_elem, 1);
                        let name = gen_const_xml_char_ptr(n_name, 2);

                        let ret_val = xml_get_dtd_attr_desc(dtd, elem, name);
                        desret_xml_attribute_ptr(ret_val);
                        des_xml_dtd_ptr(n_dtd, dtd, 0);
                        des_const_xml_char_ptr(n_elem, elem, 1);
                        des_const_xml_char_ptr(n_name, name, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlGetDtdAttrDesc",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlGetDtdAttrDesc()");
                            eprint!(" {}", n_dtd);
                            eprint!(" {}", n_elem);
                            eprintln!(" {}", n_name);
                        }
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
    fn test_xml_get_dtd_notation_desc() {

        /* missing type support */
    }

    #[test]
    fn test_xml_get_dtd_qattr_desc() {
        unsafe {
            let mut leaks = 0;

            for n_dtd in 0..GEN_NB_XML_DTD_PTR {
                for n_elem in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_prefix in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            let mem_base = xml_mem_blocks();
                            let dtd = gen_xml_dtd_ptr(n_dtd, 0);
                            let elem = gen_const_xml_char_ptr(n_elem, 1);
                            let name = gen_const_xml_char_ptr(n_name, 2);
                            let prefix = gen_const_xml_char_ptr(n_prefix, 3);

                            let ret_val = xml_get_dtd_qattr_desc(dtd, elem, name, prefix);
                            desret_xml_attribute_ptr(ret_val);
                            des_xml_dtd_ptr(n_dtd, dtd, 0);
                            des_const_xml_char_ptr(n_elem, elem, 1);
                            des_const_xml_char_ptr(n_name, name, 2);
                            des_const_xml_char_ptr(n_prefix, prefix, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlGetDtdQAttrDesc",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlGetDtdQAttrDesc()"
                                );
                                eprint!(" {}", n_dtd);
                                eprint!(" {}", n_elem);
                                eprint!(" {}", n_name);
                                eprintln!(" {}", n_prefix);
                            }
                        }
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
                    desret_xml_attr_ptr(ret_val);
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
        #[cfg(feature = "output")]
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
        #[cfg(all(feature = "valid", feature = "regexp"))]
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
    fn test_xml_valid_ctxt_normalize_attribute_value() {
        #[cfg(feature = "valid")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_VALID_CTXT_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_elem in 0..GEN_NB_XML_NODE_PTR {
                        for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                let mem_base = xml_mem_blocks();
                                let ctxt = gen_xml_valid_ctxt_ptr(n_ctxt, 0);
                                let doc = gen_xml_doc_ptr(n_doc, 1);
                                let elem = gen_xml_node_ptr(n_elem, 2);
                                let name = gen_const_xml_char_ptr(n_name, 3);
                                let value = gen_const_xml_char_ptr(n_value, 4);

                                let ret_val = xml_valid_ctxt_normalize_attribute_value(
                                    ctxt, doc, elem, name, value,
                                );
                                desret_xml_char_ptr(ret_val);
                                des_xml_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                                des_xml_doc_ptr(n_doc, doc, 1);
                                des_xml_node_ptr(n_elem, elem, 2);
                                des_const_xml_char_ptr(n_name, name, 3);
                                des_const_xml_char_ptr(n_value, value, 4);
                                reset_last_error();
                                if mem_base != xml_mem_blocks() {
                                    leaks += 1;
                                    eprint!("Leak of {} blocks found in xmlValidCtxtNormalizeAttributeValue", xml_mem_blocks() - mem_base);
                                    assert!(leaks == 0, "{leaks} Leaks are found in xmlValidCtxtNormalizeAttributeValue()");
                                    eprint!(" {}", n_ctxt);
                                    eprint!(" {}", n_doc);
                                    eprint!(" {}", n_elem);
                                    eprint!(" {}", n_name);
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
    fn test_xml_valid_get_potential_children() {
        #[cfg(feature = "valid")]
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
        #[cfg(feature = "valid")]
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
    fn test_xml_valid_normalize_attribute_value() {
        #[cfg(feature = "valid")]
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_elem in 0..GEN_NB_XML_NODE_PTR {
                    for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            let mem_base = xml_mem_blocks();
                            let doc = gen_xml_doc_ptr(n_doc, 0);
                            let elem = gen_xml_node_ptr(n_elem, 1);
                            let name = gen_const_xml_char_ptr(n_name, 2);
                            let value = gen_const_xml_char_ptr(n_value, 3);

                            let ret_val =
                                xml_valid_normalize_attribute_value(doc, elem, name, value);
                            desret_xml_char_ptr(ret_val);
                            des_xml_doc_ptr(n_doc, doc, 0);
                            des_xml_node_ptr(n_elem, elem, 1);
                            des_const_xml_char_ptr(n_name, name, 2);
                            des_const_xml_char_ptr(n_value, value, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlValidNormalizeAttributeValue",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlValidNormalizeAttributeValue()"
                                );
                                eprint!(" {}", n_doc);
                                eprint!(" {}", n_elem);
                                eprint!(" {}", n_name);
                                eprintln!(" {}", n_value);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_validate_attribute_decl() {
        #[cfg(feature = "valid")]
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
        #[cfg(feature = "valid")]
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
        #[cfg(feature = "valid")]
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
        #[cfg(feature = "valid")]
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
        #[cfg(feature = "valid")]
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
        #[cfg(feature = "valid")]
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
        #[cfg(feature = "valid")]
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
        #[cfg(feature = "valid")]
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
        #[cfg(feature = "valid")]
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
        #[cfg(feature = "valid")]
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
        #[cfg(feature = "valid")]
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
        #[cfg(feature = "valid")]
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
        #[cfg(feature = "valid")]
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
        #[cfg(any(feature = "valid", feature = "schema"))]
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
        #[cfg(feature = "valid")]
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
        #[cfg(feature = "valid")]
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
        #[cfg(feature = "valid")]
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
        #[cfg(all(feature = "valid", feature = "regexp"))]
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
        #[cfg(all(feature = "valid", feature = "regexp"))]
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
        #[cfg(all(feature = "valid", feature = "regexp"))]
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
        #[cfg(feature = "valid")]
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
