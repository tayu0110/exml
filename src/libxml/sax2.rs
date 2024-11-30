//! Provide methods and data structures for SAX2 handlers.  
//! This module is based on `libxml/SAX2.h`, `SAX2.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: SAX2 parser interface used to build the DOM tree
// Description: those are the default SAX2 interfaces used by
//              the library when building DOM tree.
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// SAX2.c : Default SAX2 handler to build a tree.
//
// See Copyright for the status of this software.
//
// Daniel Veillard <daniel@veillard.com>

use std::{
    ffi::{c_char, c_void, CStr, CString},
    mem::{replace, size_of},
    ptr::{addr_of_mut, null, null_mut},
    slice::from_raw_parts,
    sync::atomic::Ordering,
};

use libc::{memcpy, memset, INT_MAX};

use crate::{
    __xml_raise_error,
    encoding::{detect_encoding, XmlCharEncoding},
    error::{parser_error, parser_warning, XmlParserErrors},
    globals::{GenericErrorContext, StructuredError},
    libxml::{
        entities::XmlEntityPtr,
        htmltree::html_new_doc_no_dtd,
        parser::{XmlParserCtxtPtr, XmlParserInputPtr, XmlParserInputState, XmlSAXLocatorPtr},
        valid::{
            xml_validate_attribute_decl, xml_validate_document_final, xml_validate_one_element,
        },
        xmlstring::XmlChar,
    },
    tree::{
        xml_build_qname, xml_create_int_subset, xml_free_dtd, xml_free_node, xml_new_cdata_block,
        xml_new_char_ref, xml_new_doc, xml_new_doc_comment, xml_new_doc_node,
        xml_new_doc_node_eat_name, xml_new_doc_pi, xml_new_doc_text, xml_new_dtd, xml_new_ns,
        xml_new_ns_prop, xml_new_ns_prop_eat_name, xml_new_reference, xml_text_concat,
        xml_validate_ncname, NodeCommon, NodePtr, XmlAttr, XmlAttrPtr, XmlAttributeDefault,
        XmlAttributePtr, XmlAttributeType, XmlDocProperties, XmlDocPtr, XmlDtdPtr,
        XmlElementContentPtr, XmlElementPtr, XmlElementType, XmlElementTypeVal, XmlEnumerationPtr,
        XmlNode, XmlNodePtr, XmlNotationPtr, XmlNsPtr, __XML_REGISTER_CALLBACKS,
    },
};

use super::{
    chvalid::xml_is_blank_char,
    dict::{xml_dict_lookup, xml_dict_owns, xml_dict_qlookup, xml_dict_reference},
    entities::{
        xml_add_doc_entity, xml_add_dtd_entity, xml_get_doc_entity, xml_get_parameter_entity,
        xml_get_predefined_entity, XmlEntityType,
    },
    globals::{xml_free, xml_malloc, xml_realloc, xml_register_node_default_value},
    parser::{
        xml_load_external_entity, xml_pop_input, XmlParserOption, XmlSAXHandler,
        XML_COMPLETE_ATTRS, XML_SAX2_MAGIC, XML_SKIP_IDS,
    },
    parser_internals::{
        xml_err_memory, xml_free_input_stream, xml_parse_external_subset, xml_push_input,
        xml_split_qname, xml_string_decode_entities, xml_string_len_decode_entities,
        xml_switch_encoding, XML_MAX_TEXT_LENGTH, XML_STRING_TEXT, XML_SUBSTITUTE_REF,
        XML_VCTXT_DTD_VALIDATED,
    },
    uri::{
        xml_build_uri, xml_canonic_path, xml_free_uri, xml_parse_uri, xml_path_to_uri, XmlURIPtr,
    },
    valid::{
        xml_add_attribute_decl, xml_add_element_decl, xml_add_id, xml_add_notation_decl,
        xml_add_ref, xml_free_enumeration, xml_get_dtd_qattr_desc, xml_get_dtd_qelement_desc,
        xml_is_id, xml_is_ref, xml_valid_ctxt_normalize_attribute_value,
        xml_valid_normalize_attribute_value, xml_validate_dtd_final, xml_validate_element_decl,
        xml_validate_notation_decl, xml_validate_one_attribute, xml_validate_one_namespace,
        xml_validate_root,
    },
    xmlstring::{xml_str_equal, xml_strcat, xml_strdup, xml_strlen, xml_strndup},
};

/// Provides the public ID e.g. "-//SGMLSOURCE//DTD DEMO//EN"
///
/// Returns a xmlChar *
#[doc(alias = "xmlSAX2GetPublicId")]
pub unsafe extern "C" fn xml_sax2_get_public_id(_ctx: *mut c_void) -> *const XmlChar {
    /* let ctxt: xmlParserCtxtPtr = ctx as xmlParserCtxtPtr; */
    null()
}

/// Provides the system ID, basically URL or filename e.g.
/// http://www.sgmlsource.com/dtds/memo.dtd
///
/// Returns a xmlChar *
#[doc(alias = "xmlSAX2GetSystemId")]
pub unsafe fn xml_sax2_get_system_id(ctx: *mut c_void) -> Option<String> {
    let ctxt: XmlParserCtxtPtr = ctx as _;
    if ctx.is_null() || (*ctxt).input.is_null() {
        return None;
    };
    (*(*ctxt).input).filename.clone()
}

/// Receive the document locator at startup, actually xmlDefaultSAXLocator
/// Everything is available on the context, so this is useless in our case.
#[doc(alias = "xmlSAX2SetDocumentLocator")]
pub unsafe fn xml_sax2_set_document_locator(
    _ctx: Option<GenericErrorContext>,
    _loc: XmlSAXLocatorPtr,
) {
    /* let ctxt: xmlParserCtxtPtr = ctx as xmlParserCtxtPtr; */
}

/// Provide the line number of the current parsing point.
///
/// Returns an int
#[doc(alias = "xmlSAX2GetLineNumber")]
pub unsafe extern "C" fn xml_sax2_get_line_number(ctx: *mut c_void) -> i32 {
    let ctxt: XmlParserCtxtPtr = ctx as XmlParserCtxtPtr;
    if ctx.is_null() || (*ctxt).input.is_null() {
        return 0;
    }
    (*(*ctxt).input).line
}

/// Provide the column number of the current parsing point.
///
/// Returns an int
#[doc(alias = "xmlSAX2GetColumnNumber")]
pub unsafe extern "C" fn xml_sax2_get_column_number(ctx: *mut c_void) -> i32 {
    let ctxt: XmlParserCtxtPtr = ctx as XmlParserCtxtPtr;
    if ctx.is_null() || (*ctxt).input.is_null() {
        return 0;
    }
    (*(*ctxt).input).col
}

/// Is this document tagged standalone ?
///
/// Returns 1 if true
#[doc(alias = "xmlSAX2IsStandalone")]
pub unsafe fn xml_sax2_is_standalone(ctx: Option<GenericErrorContext>) -> i32 {
    if ctx.is_none() {
        return 0;
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    if (*ctxt).my_doc.is_null() {
        return 0;
    }
    ((*(*ctxt).my_doc).standalone == 1) as i32
}

/// Does this document has an internal subset
///
/// Returns 1 if true
#[doc(alias = "xmlSAX2HasInternalSubset")]
pub unsafe fn xml_sax2_has_internal_subset(ctx: Option<GenericErrorContext>) -> i32 {
    if ctx.is_none() {
        return 0;
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    if (*ctxt).my_doc.is_null() {
        return 0;
    }
    (!(*(*ctxt).my_doc).int_subset.is_null()) as i32
}

/// Does this document has an external subset
///
/// Returns 1 if true
#[doc(alias = "xmlSAX2HasExternalSubset")]
pub unsafe fn xml_sax2_has_external_subset(ctx: Option<GenericErrorContext>) -> i32 {
    if ctx.is_none() {
        return 0;
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    if (*ctxt).my_doc.is_null() {
        return 0;
    }
    (!(*(*ctxt).my_doc).ext_subset.is_null()) as i32
}

#[doc(alias = "xmlSAX2ErrMemory")]
unsafe extern "C" fn xml_sax2_err_memory(ctxt: XmlParserCtxtPtr, msg: *const c_char) {
    let mut schannel: Option<StructuredError> = None;
    let str1: *const c_char = c"out of memory\n".as_ptr() as _;

    if !ctxt.is_null() {
        (*ctxt).err_no = XmlParserErrors::XmlErrNoMemory as i32;
        if !(*ctxt).sax.is_null() && (*(*ctxt).sax).initialized == XML_SAX2_MAGIC as u32 {
            schannel = (*(*ctxt).sax).serror;
        }
        __xml_raise_error!(
            schannel,
            (*ctxt).vctxt.error,
            (*ctxt).vctxt.user_data.clone(),
            ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromParser,
            XmlParserErrors::XmlErrNoMemory,
            XmlErrorLevel::XmlErrError,
            null_mut(),
            0,
            (!str1.is_null()).then(|| CStr::from_ptr(str1).to_string_lossy().into_owned().into()),
            None,
            None,
            0,
            0,
            msg,
            str1,
            null::<c_char>()
        );
        (*ctxt).err_no = XmlParserErrors::XmlErrNoMemory as i32;
        (*ctxt).instate = XmlParserInputState::XmlParserEOF;
        (*ctxt).disable_sax = 1;
    } else {
        __xml_raise_error!(
            schannel,
            None,
            None,
            ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromParser,
            XmlParserErrors::XmlErrNoMemory,
            XmlErrorLevel::XmlErrError,
            null_mut(),
            0,
            (!str1.is_null()).then(|| CStr::from_ptr(str1).to_string_lossy().into_owned().into()),
            None,
            None,
            0,
            0,
            msg,
            str1,
            null::<c_char>()
        );
    }
}

/// Callback on internal subset declaration.
#[doc(alias = "xmlSAX2InternalSubset")]
pub unsafe fn xml_sax2_internal_subset(
    ctx: Option<GenericErrorContext>,
    name: *const XmlChar,
    external_id: *const XmlChar,
    system_id: *const XmlChar,
) {
    if ctx.is_none() {
        return;
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };

    if (*ctxt).my_doc.is_null() {
        return;
    }
    let dtd: XmlDtdPtr = (*(*ctxt).my_doc).get_int_subset();
    if !dtd.is_null() {
        if (*ctxt).html != 0 {
            return;
        }
        (*dtd).unlink();
        xml_free_dtd(dtd);
        (*(*ctxt).my_doc).int_subset = null_mut();
    }
    (*(*ctxt).my_doc).int_subset = xml_create_int_subset(
        (*ctxt).my_doc,
        name,
        (!external_id.is_null())
            .then(|| CStr::from_ptr(external_id as *const i8).to_string_lossy())
            .as_deref(),
        (!system_id.is_null())
            .then(|| CStr::from_ptr(system_id as *const i8).to_string_lossy())
            .as_deref(),
    );
    if (*(*ctxt).my_doc).int_subset.is_null() {
        xml_sax2_err_memory(ctxt, c"xmlSAX2InternalSubset".as_ptr() as _);
    }
}

/// Callback on external subset declaration.
#[doc(alias = "xmlSAX2ExternalSubset")]
pub unsafe fn xml_sax2_external_subset(
    ctx: Option<GenericErrorContext>,
    name: *const XmlChar,
    external_id: *const XmlChar,
    system_id: *const XmlChar,
) {
    if ctx.is_none() {
        return;
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    if (!external_id.is_null() || !system_id.is_null())
        && (((*ctxt).validate != 0 || (*ctxt).loadsubset != 0)
            && ((*ctxt).well_formed != 0 && !(*ctxt).my_doc.is_null()))
    {
        let mut input: XmlParserInputPtr = null_mut();
        let mut consumed: u64;

        /*
         * Ask the Entity resolver to load the damn thing
         */
        if !(*ctxt).sax.is_null() && (*(*ctxt).sax).resolve_entity.is_some() {
            input = ((*(*ctxt).sax).resolve_entity.unwrap())(
                (*ctxt).user_data.clone(),
                external_id,
                system_id,
            );
        }
        if input.is_null() {
            return;
        }

        xml_new_dtd(
            (*ctxt).my_doc,
            name,
            (!external_id.is_null())
                .then(|| CStr::from_ptr(external_id as *const i8).to_string_lossy())
                .as_deref(),
            (!system_id.is_null())
                .then(|| CStr::from_ptr(system_id as *const i8).to_string_lossy())
                .as_deref(),
        );

        /*
         * make sure we won't destroy the main document context
         */
        /*
         * Try to fetch and parse the external subset.
         */
        let oldinput: XmlParserInputPtr = (*ctxt).input;
        let oldinput_tab = replace(&mut (*ctxt).input_tab, Vec::with_capacity(5));
        let oldcharset = (*ctxt).charset;
        let oldencoding = (*ctxt).encoding.take();
        let oldprogressive: i32 = (*ctxt).progressive;
        (*ctxt).progressive = 0;
        (*ctxt).input = null_mut();
        xml_push_input(ctxt, input);

        /*
         * On the fly encoding conversion if needed
         */
        if (*(*ctxt).input).length >= 4 {
            let input = from_raw_parts((*(*ctxt).input).cur, 4);
            let enc = detect_encoding(input);
            xml_switch_encoding(ctxt, enc);
        }

        if (*input).filename.is_none() {
            let canonic = xml_canonic_path(system_id);
            if !canonic.is_null() {
                (*input).filename = Some(
                    CStr::from_ptr(canonic as *const i8)
                        .to_string_lossy()
                        .into_owned(),
                );
                xml_free(canonic as _);
            }
        }
        (*input).line = 1;
        (*input).col = 1;
        (*input).base = (*(*ctxt).input).cur;
        (*input).cur = (*(*ctxt).input).cur;
        (*input).free = None;

        /*
         * let's parse that entity knowing it's an external subset.
         */
        xml_parse_external_subset(ctxt, external_id, system_id);

        /*
         * Free up the external entities
         */
        #[allow(clippy::while_immutable_condition)]
        while (*ctxt).input_tab.len() > 1 {
            xml_pop_input(ctxt);
        }

        consumed = (*(*ctxt).input).consumed;
        let buffered: usize = (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as _;
        if buffered as u64 > u64::MAX - consumed {
            consumed = u64::MAX;
        } else {
            consumed += buffered as u64;
        }
        if consumed > u64::MAX - (*ctxt).sizeentities {
            (*ctxt).sizeentities = u64::MAX;
        } else {
            (*ctxt).sizeentities += consumed;
        }

        xml_free_input_stream((*ctxt).input);

        /*
         * Restore the parsing context of the main entity
         */
        (*ctxt).input = oldinput;
        (*ctxt).input_tab = oldinput_tab;
        (*ctxt).charset = oldcharset;
        (*ctxt).encoding = oldencoding;
        (*ctxt).progressive = oldprogressive;
        /* (*ctxt).wellFormed = oldwellFormed; */
    }
}

/// Handle a fatal parser error, i.e. violating Well-Formedness constraints
#[doc(alias = "xmlFatalErrMsg")]
unsafe extern "C" fn xml_fatal_err_msg(
    ctxt: XmlParserCtxtPtr,
    error: XmlParserErrors,
    msg: *const c_char,
    str1: *const XmlChar,
    str2: *const XmlChar,
) {
    if !ctxt.is_null()
        && ((*ctxt).disable_sax != 0)
        && matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        return;
    }
    if !ctxt.is_null() {
        (*ctxt).err_no = error as i32;
    }
    __xml_raise_error!(
        None,
        None,
        None,
        ctxt as _,
        null_mut(),
        XmlErrorDomain::XmlFromParser,
        error,
        XmlErrorLevel::XmlErrFatal,
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
    if !ctxt.is_null() {
        (*ctxt).well_formed = 0;
        (*ctxt).valid = 0;
        if (*ctxt).recovery == 0 {
            (*ctxt).disable_sax = 1;
        }
    }
}

/// Get an entity by name
///
/// Returns the xmlEntityPtr if found.
#[doc(alias = "xmlSAX2GetEntity")]
pub unsafe fn xml_sax2_get_entity(
    ctx: Option<GenericErrorContext>,
    name: *const XmlChar,
) -> XmlEntityPtr {
    let mut ret: XmlEntityPtr;

    if ctx.is_none() {
        return null_mut();
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };

    if (*ctxt).in_subset == 0 {
        ret = xml_get_predefined_entity(name);
        if !ret.is_null() {
            return ret;
        }
    }
    if !(*ctxt).my_doc.is_null() && (*(*ctxt).my_doc).standalone == 1 {
        if (*ctxt).in_subset == 2 {
            (*(*ctxt).my_doc).standalone = 0;
            ret = xml_get_doc_entity((*ctxt).my_doc, name);
            (*(*ctxt).my_doc).standalone = 1;
        } else {
            ret = xml_get_doc_entity((*ctxt).my_doc, name);
            if ret.is_null() {
                (*(*ctxt).my_doc).standalone = 0;
                ret = xml_get_doc_entity((*ctxt).my_doc, name);
                if !ret.is_null() {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrNotStandalone,
                        c"Entity(%s) document marked standalone but requires external subset\n"
                            .as_ptr() as _,
                        name,
                        null(),
                    );
                }
                (*(*ctxt).my_doc).standalone = 1;
            }
        }
    } else {
        ret = xml_get_doc_entity((*ctxt).my_doc, name);
    }
    ret
}

/// Get a parameter entity by name
///
/// Returns the xmlEntityPtr if found.
#[doc(alias = "xmlSAX2GetParameterEntity")]
pub unsafe fn xml_sax2_get_parameter_entity(
    ctx: Option<GenericErrorContext>,
    name: *const XmlChar,
) -> XmlEntityPtr {
    if ctx.is_none() {
        return null_mut();
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };

    xml_get_parameter_entity((*ctxt).my_doc, name)
}

/// The entity loader, to control the loading of external entities,
/// the application can either:
///    - override this xmlSAX2ResolveEntity() callback in the SAX block
///    - or better use the xmlSetExternalEntityLoader() function to
///      set up it's own entity resolution routine
///
/// Returns the xmlParserInputPtr if inlined or NULL for DOM behaviour.
#[doc(alias = "xmlSAX2ResolveEntity")]
pub unsafe fn xml_sax2_resolve_entity(
    ctx: Option<GenericErrorContext>,
    public_id: *const XmlChar,
    system_id: *const XmlChar,
) -> XmlParserInputPtr {
    let mut base: *const c_char = null();

    if ctx.is_none() {
        return null_mut();
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    // temporary use.
    // Currently, filename is Option<String> and base is *const c_char.
    // Even if we make c-string pointer in if statement, it does not alive sufficient term.
    // So, make dummy out of if statement.
    #[allow(unused_assignments)]
    let mut dummy = c"".to_owned();
    if !(*ctxt).input.is_null() {
        if let Some(filename) = (*(*ctxt).input).filename.as_deref() {
            dummy = CString::new(filename).unwrap();
            base = dummy.as_ptr();
        }
    }
    if base.is_null() {
        if let Some(dir) = (*ctxt).directory.as_deref() {
            dummy = CString::new(dir).unwrap();
            base = dummy.as_ptr();
        }
    }

    let uri: *mut XmlChar = xml_build_uri(system_id, base as _);
    let ret: XmlParserInputPtr = xml_load_external_entity(uri as _, public_id as _, ctxt);
    if !uri.is_null() {
        xml_free(uri as _);
    }
    ret
}

/// Handle a parser warning
#[doc(alias = "xmlWarnMsg")]
unsafe extern "C" fn xml_warn_msg(
    ctxt: XmlParserCtxtPtr,
    error: XmlParserErrors,
    msg: *const c_char,
    str1: *const XmlChar,
) {
    if !ctxt.is_null()
        && (*ctxt).disable_sax != 0
        && matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        return;
    }
    if !ctxt.is_null() {
        (*ctxt).err_no = error as i32;
    }
    __xml_raise_error!(
        None,
        None,
        None,
        ctxt as _,
        null_mut(),
        XmlErrorDomain::XmlFromParser,
        error,
        XmlErrorLevel::XmlErrWarning,
        null_mut(),
        0,
        (!str1.is_null()).then(|| CStr::from_ptr(str1 as *const i8)
            .to_string_lossy()
            .into_owned()
            .into()),
        None,
        None,
        0,
        0,
        msg,
        str1
    );
}

/// An entity definition has been parsed
#[doc(alias = "xmlSAX2EntityDecl")]
pub unsafe fn xml_sax2_entity_decl(
    ctx: Option<GenericErrorContext>,
    name: *const XmlChar,
    typ: i32,
    public_id: *const XmlChar,
    system_id: *const XmlChar,
    content: *mut XmlChar,
) {
    let ent: XmlEntityPtr;

    if ctx.is_none() {
        return;
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    if (*ctxt).in_subset == 1 {
        ent = xml_add_doc_entity((*ctxt).my_doc, name, typ, public_id, system_id, content);
        if ent.is_null() && (*ctxt).pedantic != 0 {
            xml_warn_msg(
                ctxt,
                XmlParserErrors::XmlWarEntityRedefined,
                c"Entity(%s) already defined in the internal subset\n".as_ptr() as _,
                name,
            );
        }
        if !ent.is_null() && (*ent).uri.load(Ordering::Relaxed).is_null() && !system_id.is_null() {
            let mut base: *const c_char = null();
            #[allow(unused_assignments)]
            let mut dummy = c"".to_owned();

            if !(*ctxt).input.is_null() {
                if let Some(filename) = (*(*ctxt).input).filename.as_deref() {
                    dummy = CString::new(filename).unwrap();
                    base = dummy.as_ptr();
                }
            }
            if base.is_null() {
                if let Some(dir) = (*ctxt).directory.as_deref() {
                    dummy = CString::new(dir).unwrap();
                    base = dummy.as_ptr();
                }
            }

            let uri: *mut XmlChar = xml_build_uri(system_id, base as _);
            (*ent).uri.store(uri, Ordering::Relaxed);
        }
    } else if (*ctxt).in_subset == 2 {
        ent = xml_add_dtd_entity((*ctxt).my_doc, name, typ, public_id, system_id, content);
        if ent.is_null()
            && (*ctxt).pedantic != 0
            && !(*ctxt).sax.is_null()
            && (*(*ctxt).sax).warning.is_some()
        {
            (*(*ctxt).sax).warning.unwrap()(
                (*ctxt).user_data.clone(),
                format!(
                    "Entity({}) already defined in the external subset\n",
                    CStr::from_ptr(name as *const i8).to_string_lossy()
                )
                .as_str(),
            );
        }
        if !ent.is_null() && (*ent).uri.load(Ordering::Relaxed).is_null() && !system_id.is_null() {
            let mut base: *const c_char = null();
            #[allow(unused_assignments)]
            let mut dummy = c"".to_owned();

            if !(*ctxt).input.is_null() {
                if let Some(filename) = (*(*ctxt).input).filename.as_deref() {
                    dummy = CString::new(filename).unwrap();
                    base = dummy.as_ptr();
                }
            }
            if base.is_null() {
                if let Some(dir) = (*ctxt).directory.as_deref() {
                    dummy = CString::new(dir).unwrap();
                    base = dummy.as_ptr();
                }
            }

            let uri: *mut XmlChar = xml_build_uri(system_id, base as _);
            (*ent).uri.store(uri, Ordering::Relaxed);
        }
    } else {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrEntityProcessing,
            c"SAX.xmlSAX2EntityDecl(%s) called while not in subset\n".as_ptr() as _,
            name,
            null(),
        );
    }
}

/// Handle a validation error
#[doc(alias = "xmlValidError")]
unsafe extern "C" fn xml_err_valid(
    ctxt: XmlParserCtxtPtr,
    error: XmlParserErrors,
    msg: *const c_char,
    str1: *const c_char,
    str2: *const c_char,
) {
    let mut schannel: Option<StructuredError> = None;

    if !ctxt.is_null()
        && (*ctxt).disable_sax != 0
        && matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        return;
    }
    if !ctxt.is_null() {
        (*ctxt).err_no = error as i32;
        if !(*ctxt).sax.is_null() && (*(*ctxt).sax).initialized == XML_SAX2_MAGIC as u32 {
            schannel = (*(*ctxt).sax).serror;
        }
        __xml_raise_error!(
            schannel,
            (*ctxt).vctxt.error,
            (*ctxt).vctxt.user_data.clone(),
            ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromDTD,
            error,
            XmlErrorLevel::XmlErrError,
            null_mut(),
            0,
            (!str1.is_null()).then(|| CStr::from_ptr(str1).to_string_lossy().into_owned().into()),
            (!str2.is_null()).then(|| CStr::from_ptr(str2).to_string_lossy().into_owned().into()),
            None,
            0,
            0,
            msg,
            str1,
            str2
        );
        (*ctxt).valid = 0;
    } else {
        __xml_raise_error!(
            schannel,
            None,
            None,
            ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromDTD,
            error,
            XmlErrorLevel::XmlErrError,
            null_mut(),
            0,
            (!str1.is_null()).then(|| CStr::from_ptr(str1).to_string_lossy().into_owned().into()),
            (!str2.is_null()).then(|| CStr::from_ptr(str2).to_string_lossy().into_owned().into()),
            None,
            0,
            0,
            msg,
            str1,
            str2
        );
    }
}

/// An attribute definition has been parsed
#[doc(alias = "xmlSAX2AttributeDecl")]
pub unsafe fn xml_sax2_attribute_decl(
    ctx: Option<GenericErrorContext>,
    elem: *const XmlChar,
    fullname: *const XmlChar,
    typ: i32,
    def: i32,
    default_value: *const XmlChar,
    tree: XmlEnumerationPtr,
) {
    let attr: XmlAttributePtr;

    let mut prefix: *mut XmlChar = null_mut();

    if ctx.is_none() {
        return;
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    if (*ctxt).my_doc.is_null() {
        return;
    }

    if xml_str_equal(fullname, c"xml:id".as_ptr() as _)
        && typ != XmlAttributeType::XmlAttributeID as i32
    {
        /*
         * Raise the error but keep the validity flag
         */
        let tmp: i32 = (*ctxt).valid;
        xml_err_valid(
            ctxt,
            XmlParserErrors::XmlDTDXmlidType,
            c"xml:id : attribute type should be ID\n".as_ptr() as _,
            null(),
            null(),
        );
        (*ctxt).valid = tmp;
    }
    /* TODO: optimize name/prefix allocation */
    let name: *mut XmlChar = xml_split_qname(ctxt, fullname, &raw mut prefix as _);
    (*ctxt).vctxt.valid = 1;
    if (*ctxt).in_subset == 1 {
        attr = xml_add_attribute_decl(
            &raw mut (*ctxt).vctxt as _,
            (*(*ctxt).my_doc).int_subset,
            elem,
            name,
            prefix,
            XmlAttributeType::try_from(typ).unwrap(),
            XmlAttributeDefault::try_from(def).unwrap(),
            default_value,
            tree,
        );
    } else if (*ctxt).in_subset == 2 {
        attr = xml_add_attribute_decl(
            &raw mut (*ctxt).vctxt as _,
            (*(*ctxt).my_doc).ext_subset,
            elem,
            name,
            prefix,
            XmlAttributeType::try_from(typ).unwrap(),
            XmlAttributeDefault::try_from(def).unwrap(),
            default_value,
            tree,
        );
    } else {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            c"SAX.xmlSAX2AttributeDecl(%s) called while not in subset\n".as_ptr() as _,
            name,
            null(),
        );
        xml_free(name as _);
        xml_free_enumeration(tree);
        return;
    }
    #[cfg(feature = "libxml_valid")]
    {
        if (*ctxt).vctxt.valid == 0 {
            (*ctxt).valid = 0;
        }
        if !attr.is_null()
            && (*ctxt).validate != 0
            && (*ctxt).well_formed != 0
            && !(*(*ctxt).my_doc).int_subset.is_null()
        {
            (*ctxt).valid &=
                xml_validate_attribute_decl(addr_of_mut!((*ctxt).vctxt) as _, (*ctxt).my_doc, attr);
        }
    }
    if !prefix.is_null() {
        xml_free(prefix as _);
    }
    if !name.is_null() {
        xml_free(name as _);
    }
}

/// An element definition has been parsed
#[doc(alias = "xmlSAX2ElementDecl")]
pub unsafe fn xml_sax2_element_decl(
    ctx: Option<GenericErrorContext>,
    name: *const XmlChar,
    typ: i32,
    content: XmlElementContentPtr,
) {
    let elem: XmlElementPtr;

    if ctx.is_none() {
        return;
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    if (*ctxt).my_doc.is_null() {
        return;
    }

    if (*ctxt).in_subset == 1 {
        elem = xml_add_element_decl(
            addr_of_mut!((*ctxt).vctxt) as _,
            (*(*ctxt).my_doc).int_subset,
            name,
            XmlElementTypeVal::try_from(typ).unwrap(),
            content,
        );
    } else if (*ctxt).in_subset == 2 {
        elem = xml_add_element_decl(
            addr_of_mut!((*ctxt).vctxt) as _,
            (*(*ctxt).my_doc).ext_subset,
            name,
            XmlElementTypeVal::try_from(typ).unwrap(),
            content,
        );
    } else {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            c"SAX.xmlSAX2ElementDecl(%s) called while not in subset\n".as_ptr() as _,
            name,
            null(),
        );
        return;
    }
    #[cfg(feature = "libxml_valid")]
    {
        if elem.is_null() {
            (*ctxt).valid = 0;
        }
        if (*ctxt).validate != 0
            && (*ctxt).well_formed != 0
            && !(*ctxt).my_doc.is_null()
            && !(*(*ctxt).my_doc).int_subset.is_null()
        {
            (*ctxt).valid &=
                xml_validate_element_decl(addr_of_mut!((*ctxt).vctxt) as _, (*ctxt).my_doc, elem);
        }
    }
}

/// What to do when a notation declaration has been parsed.
#[doc(alias = "xmlSAX2NotationDecl")]
pub unsafe fn xml_sax2_notation_decl(
    ctx: Option<GenericErrorContext>,
    name: *const XmlChar,
    public_id: *const XmlChar,
    system_id: *const XmlChar,
) {
    let nota: XmlNotationPtr;

    if ctx.is_none() {
        return;
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    if (*ctxt).my_doc.is_null() {
        return;
    }

    if public_id.is_null() && system_id.is_null() {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrNotationProcessing,
            c"SAX.xmlSAX2NotationDecl(%s) externalID or PublicID missing\n".as_ptr() as _,
            name,
            null(),
        );
        return;
    } else if (*ctxt).in_subset == 1 {
        nota = xml_add_notation_decl(
            addr_of_mut!((*ctxt).vctxt) as _,
            (*(*ctxt).my_doc).int_subset,
            name,
            public_id,
            system_id,
        );
    } else if (*ctxt).in_subset == 2 {
        nota = xml_add_notation_decl(
            addr_of_mut!((*ctxt).vctxt) as _,
            (*(*ctxt).my_doc).ext_subset,
            name,
            public_id,
            system_id,
        );
    } else {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrNotationProcessing,
            c"SAX.xmlSAX2NotationDecl(%s) called while not in subset\n".as_ptr() as _,
            name,
            null(),
        );
        return;
    }
    #[cfg(feature = "libxml_valid")]
    {
        if nota.is_null() {
            (*ctxt).valid = 0;
        }
        if (*ctxt).validate != 0
            && (*ctxt).well_formed != 0
            && !(*(*ctxt).my_doc).int_subset.is_null()
        {
            (*ctxt).valid &=
                xml_validate_notation_decl(addr_of_mut!((*ctxt).vctxt) as _, (*ctxt).my_doc, nota);
        }
    }
}

/// What to do when an unparsed entity declaration is parsed
#[doc(alias = "xmlSAX2UnparsedEntityDecl")]
pub unsafe fn xml_sax2_unparsed_entity_decl(
    ctx: Option<GenericErrorContext>,
    name: *const XmlChar,
    public_id: *const XmlChar,
    system_id: *const XmlChar,
    notation_name: *const XmlChar,
) {
    let ent: XmlEntityPtr;
    if ctx.is_none() {
        return;
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };

    if (*ctxt).in_subset == 1 {
        ent = xml_add_doc_entity(
            (*ctxt).my_doc,
            name,
            XmlEntityType::XmlExternalGeneralUnparsedEntity as i32,
            public_id,
            system_id,
            notation_name,
        );
        if ent.is_null()
            && (*ctxt).pedantic != 0
            && !(*ctxt).sax.is_null()
            && (*(*ctxt).sax).warning.is_some()
        {
            (*(*ctxt).sax).warning.unwrap()(
                (*ctxt).user_data.clone(),
                format!(
                    "Entity({}) already defined in the internal subset\n",
                    CStr::from_ptr(name as *const i8).to_string_lossy()
                )
                .as_str(),
            )
        }
        if !ent.is_null() && (*ent).uri.load(Ordering::Relaxed).is_null() && !system_id.is_null() {
            let mut base: *const c_char = null();
            #[allow(unused_assignments)]
            let mut dummy = c"".to_owned();

            if !(*ctxt).input.is_null() {
                if let Some(filename) = (*(*ctxt).input).filename.as_deref() {
                    dummy = CString::new(filename).unwrap();
                    base = dummy.as_ptr();
                }
            }
            if base.is_null() {
                if let Some(dir) = (*ctxt).directory.as_deref() {
                    dummy = CString::new(dir).unwrap();
                    base = dummy.as_ptr();
                }
            }

            let uri: *mut XmlChar = xml_build_uri(system_id, base as _);
            (*ent).uri.store(uri, Ordering::Relaxed);
        }
    } else if (*ctxt).in_subset == 2 {
        ent = xml_add_dtd_entity(
            (*ctxt).my_doc,
            name,
            XmlEntityType::XmlExternalGeneralUnparsedEntity as i32,
            public_id,
            system_id,
            notation_name,
        );
        if ent.is_null()
            && (*ctxt).pedantic != 0
            && !(*ctxt).sax.is_null()
            && (*(*ctxt).sax).warning.is_some()
        {
            (*(*ctxt).sax).warning.unwrap()(
                (*ctxt).user_data.clone(),
                format!(
                    "Entity({}) already defined in the external subset\n",
                    CStr::from_ptr(name as *const i8).to_string_lossy()
                )
                .as_str(),
            )
        }
        if !ent.is_null() && (*ent).uri.load(Ordering::Relaxed).is_null() && !system_id.is_null() {
            let mut base: *const c_char = null();
            #[allow(unused_assignments)]
            let mut dummy = c"".to_owned();

            if !(*ctxt).input.is_null() {
                if let Some(filename) = (*(*ctxt).input).filename.as_deref() {
                    dummy = CString::new(filename).unwrap();
                    base = dummy.as_ptr();
                }
            }
            if base.is_null() {
                if let Some(dir) = (*ctxt).directory.as_deref() {
                    dummy = CString::new(dir).unwrap();
                    base = dummy.as_ptr();
                }
            }

            let uri: *mut XmlChar = xml_build_uri(system_id, base as _);
            (*ent).uri.store(uri, Ordering::Relaxed);
        }
    } else {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            c"SAX.xmlSAX2UnparsedEntityDecl(%s) called while not in subset\n".as_ptr() as _,
            name,
            null(),
        );
    }
}

/// called when the document start being processed.
#[doc(alias = "xmlSAX2StartDocument")]
pub unsafe fn xml_sax2_start_document(ctx: Option<GenericErrorContext>) {
    let doc: XmlDocPtr;

    if ctx.is_none() {
        return;
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };

    if (*ctxt).html != 0 {
        #[cfg(feature = "html")]
        {
            if (*ctxt).my_doc.is_null() {
                (*ctxt).my_doc = html_new_doc_no_dtd(null(), null());
            }
            if (*ctxt).my_doc.is_null() {
                xml_sax2_err_memory(ctxt, c"xmlSAX2StartDocument".as_ptr() as _);
                return;
            }
            (*(*ctxt).my_doc).properties = XmlDocProperties::XmlDocHTML as i32;
            (*(*ctxt).my_doc).parse_flags = (*ctxt).options;
        }
        #[cfg(not(feature = "html"))]
        {
            xmlGenericError(
                xmlGenericErrorContext,
                c"libxml2 built without HTML support\n",
            );
            (*ctxt).errNo = XmlParserErrors::XmlErrInternalError as i32;
            (*ctxt).instate = XmlParserInputState::XmlParserEOF;
            (*ctxt).disableSAX = 1;
            return;
        }
    } else {
        doc = xml_new_doc((*ctxt).version.as_deref());
        (*ctxt).my_doc = doc;
        if !doc.is_null() {
            (*doc).properties = 0;
            if (*ctxt).options & XmlParserOption::XmlParseOld10 as i32 != 0 {
                (*doc).properties |= XmlDocProperties::XmlDocOld10 as i32;
            }
            (*doc).parse_flags = (*ctxt).options;
            (*doc).encoding = (*ctxt).encoding().map(|e| e.to_owned());
            (*doc).standalone = (*ctxt).standalone;
        } else {
            xml_sax2_err_memory(ctxt, c"xmlSAX2StartDocument".as_ptr() as _);
            return;
        }
        if (*ctxt).dict_names != 0 && !doc.is_null() {
            (*doc).dict = (*ctxt).dict;
            xml_dict_reference((*doc).dict);
        }
    }
    if !(*ctxt).my_doc.is_null()
        && (*(*ctxt).my_doc).url.is_none()
        && !(*ctxt).input.is_null()
        && (*(*ctxt).input).filename.is_some()
    {
        let filename = CString::new((*(*ctxt).input).filename.as_deref().unwrap()).unwrap();
        let url = xml_path_to_uri(filename.as_ptr() as _);
        if !url.is_null() {
            (*(*ctxt).my_doc).url = Some(
                CStr::from_ptr(url as *const i8)
                    .to_string_lossy()
                    .into_owned(),
            );
            xml_free(url as _);
        } else {
            (*(*ctxt).my_doc).url = None;
        }
        if (*(*ctxt).my_doc).url.is_none() {
            xml_sax2_err_memory(ctxt, c"xmlSAX2StartDocument".as_ptr() as _);
        }
    }
}

/// called when the document end has been detected.
#[doc(alias = "xmlSAX2EndDocument")]
pub unsafe fn xml_sax2_end_document(ctx: Option<GenericErrorContext>) {
    if ctx.is_none() {
        return;
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    #[cfg(feature = "libxml_valid")]
    {
        if (*ctxt).validate != 0
            && (*ctxt).well_formed != 0
            && !(*ctxt).my_doc.is_null()
            && !(*(*ctxt).my_doc).int_subset.is_null()
        {
            (*ctxt).valid &=
                xml_validate_document_final(addr_of_mut!((*ctxt).vctxt) as _, (*ctxt).my_doc);
        }
    }

    /*
     * Grab the encoding if it was added on-the-fly
     */
    if (*ctxt).encoding.is_some()
        && !(*ctxt).my_doc.is_null()
        && (*(*ctxt).my_doc).encoding.is_none()
    {
        if let Some(enc) = (*ctxt).encoding.take() {
            (*(*ctxt).my_doc).encoding = Some(enc);
        }
    }
    if !(*ctxt).input_tab.is_empty()
        && !(*ctxt).input_tab[0].is_null()
        && (*(*ctxt).input_tab[0]).encoding.is_some()
        && !(*ctxt).my_doc.is_null()
        && (*(*ctxt).my_doc).encoding.is_none()
    {
        (*(*ctxt).my_doc).encoding = (*(*ctxt).input_tab[0]).encoding.clone();
    }
    if (*ctxt).charset != XmlCharEncoding::None
        && !(*ctxt).my_doc.is_null()
        && (*(*ctxt).my_doc).charset == XmlCharEncoding::None
    {
        (*(*ctxt).my_doc).charset = (*ctxt).charset;
    }
}

/// Handle a namespace warning
#[doc(alias = "xmlNsWarnMsg")]
unsafe extern "C" fn xml_ns_warn_msg(
    ctxt: XmlParserCtxtPtr,
    error: XmlParserErrors,
    msg: *const c_char,
    str1: *const XmlChar,
    str2: *const XmlChar,
) {
    if !ctxt.is_null()
        && (*ctxt).disable_sax != 0
        && matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        return;
    }
    if !ctxt.is_null() {
        (*ctxt).err_no = error as i32;
    }
    __xml_raise_error!(
        None,
        None,
        None,
        ctxt as _,
        null_mut(),
        XmlErrorDomain::XmlFromNamespace,
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
        None,
        0,
        0,
        msg,
        str1,
        str2
    );
}

/// Handle a namespace error
#[doc(alias = "xmlNsErrMsg")]
#[cfg(any(
    feature = "sax1",
    feature = "html",
    feature = "libxml_writer",
    feature = "libxml_legacy"
))]
unsafe extern "C" fn xml_ns_err_msg(
    ctxt: XmlParserCtxtPtr,
    error: XmlParserErrors,
    msg: *const c_char,
    str1: *const XmlChar,
    str2: *const XmlChar,
) {
    if !ctxt.is_null()
        && (*ctxt).disable_sax != 0
        && matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        return;
    }
    if !ctxt.is_null() {
        (*ctxt).err_no = error as i32;
    }
    __xml_raise_error!(
        None,
        None,
        None,
        ctxt as _,
        null_mut(),
        XmlErrorDomain::XmlFromNamespace,
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

/// Handle an attribute that has been read by the parser.
/// The default handling is to convert the attribute into an
/// DOM subtree and past it in a new xmlAttr element added to
/// the element.
#[doc(alias = "xmlSAX2AttributeInternal")]
#[cfg(any(
    feature = "sax1",
    feature = "html",
    feature = "libxml_writer",
    feature = "libxml_legacy"
))]
unsafe fn xml_sax2_attribute_internal(
    ctx: Option<GenericErrorContext>,
    fullname: *const XmlChar,
    mut value: *const XmlChar,
    prefix: *const XmlChar,
) {
    use super::htmltree::html_is_boolean_attr;

    let mut name: *mut XmlChar;
    let mut ns: *mut XmlChar = null_mut();
    let nval: *mut XmlChar;
    let namespace: XmlNsPtr;

    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };

    if (*ctxt).html != 0 {
        name = xml_strdup(fullname);
        ns = null_mut();
        // namespace = null_mut();
    } else {
        /*
         * Split the full name into a namespace prefix and the tag name
         */
        name = xml_split_qname(ctxt, fullname, addr_of_mut!(ns));
        if !name.is_null() && *name.add(0) == 0 {
            if xml_str_equal(ns, c"xmlns".as_ptr() as _) {
                xml_ns_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrNsDeclError,
                    c"invalid namespace declaration '%s'\n".as_ptr() as _,
                    fullname,
                    null(),
                );
            } else {
                xml_ns_warn_msg(
                    ctxt,
                    XmlParserErrors::XmlWarNsColumn,
                    c"Avoid attribute ending with ':' like '%s'\n".as_ptr() as _,
                    fullname,
                    null(),
                );
            }
            if !ns.is_null() {
                xml_free(ns as _);
            }
            ns = null_mut();
            xml_free(name as _);
            name = xml_strdup(fullname);
        }
    }
    if name.is_null() {
        xml_sax2_err_memory(ctxt, c"xmlSAX2StartElement".as_ptr() as _);
        if !ns.is_null() {
            xml_free(ns as _);
        }
        return;
    }

    #[cfg(not(feature = "html"))]
    let f = false;
    #[cfg(feature = "html")]
    let f = (*ctxt).html != 0 && value.is_null() && html_is_boolean_attr(fullname) != 0;
    if f {
        nval = xml_strdup(fullname);
        value = nval;
    } else {
        #[cfg(feature = "libxml_valid")]
        {
            /*
             * Do the last stage of the attribute normalization
             * Needed for HTML too:
             *   http://www.w3.org/TR/html4/types.html#h-6.2
             */
            (*ctxt).vctxt.valid = 1;
            nval = xml_valid_ctxt_normalize_attribute_value(
                addr_of_mut!((*ctxt).vctxt) as _,
                (*ctxt).my_doc,
                (*ctxt).node,
                fullname,
                value,
            );
            if (*ctxt).vctxt.valid != 1 {
                (*ctxt).valid = 0;
            }
            if !nval.is_null() {
                value = nval;
            }
        }
        #[cfg(not(feature = "libxml_valid"))]
        {
            nval = null_mut();
        }
    }

    /*
     * Check whether it's a namespace definition
     */
    if (*ctxt).html == 0
        && ns.is_null()
        && *name.add(0) == b'x'
        && *name.add(1) == b'm'
        && *name.add(2) == b'l'
        && *name.add(3) == b'n'
        && *name.add(4) == b's'
        && *name.add(5) == 0
    {
        let val: *mut XmlChar;

        if (*ctxt).replace_entities == 0 {
            (*ctxt).depth += 1;
            val = xml_string_decode_entities(ctxt, value, XML_SUBSTITUTE_REF as _, 0, 0, 0);
            (*ctxt).depth -= 1;
            if val.is_null() {
                xml_sax2_err_memory(ctxt, c"xmlSAX2StartElement".as_ptr() as _);
                if !name.is_null() {
                    xml_free(name as _);
                }
                if !nval.is_null() {
                    xml_free(nval as _);
                }
                return;
            }
        } else {
            val = value as _;
        }

        if *val.add(0) != 0 {
            let uri: XmlURIPtr = xml_parse_uri(val as _);
            if uri.is_null() {
                if !(*ctxt).sax.is_null() && (*(*ctxt).sax).warning.is_some() {
                    (*(*ctxt).sax).warning.unwrap()(
                        (*ctxt).user_data.clone(),
                        format!(
                            "xmlns: {} not a valid URI\n",
                            CStr::from_ptr(val as *const i8).to_string_lossy()
                        )
                        .as_str(),
                    );
                }
            } else {
                if (*uri).scheme.is_null()
                    && (!(*ctxt).sax.is_null() && (*(*ctxt).sax).warning.is_some())
                {
                    (*(*ctxt).sax).warning.unwrap()(
                        (*ctxt).user_data.clone(),
                        format!(
                            "xmlns: URI {} is not absolute\n",
                            CStr::from_ptr(val as *const i8).to_string_lossy()
                        )
                        .as_str(),
                    );
                }
                xml_free_uri(uri);
            }
        }

        /* a default namespace definition */
        let nsret: XmlNsPtr = xml_new_ns((*ctxt).node, val, null_mut());

        #[cfg(feature = "libxml_valid")]
        {
            /*
             * Validate also for namespace decls, they are attributes from
             * an XML-1.0 perspective
             */
            if !nsret.is_null()
                && (*ctxt).validate != 0
                && (*ctxt).well_formed != 0
                && !(*ctxt).my_doc.is_null()
                && !(*(*ctxt).my_doc).int_subset.is_null()
            {
                (*ctxt).valid &= xml_validate_one_namespace(
                    addr_of_mut!((*ctxt).vctxt) as _,
                    (*ctxt).my_doc,
                    (*ctxt).node,
                    prefix,
                    nsret,
                    val,
                );
            }
        }
        if !name.is_null() {
            xml_free(name as _);
        }
        if !nval.is_null() {
            xml_free(nval as _);
        }
        if val != value as _ {
            xml_free(val as _);
        }
        return;
    }
    if (*ctxt).html != 0
        && !ns.is_null()
        && *ns.add(0) == b'x'
        && *ns.add(1) == b'm'
        && *ns.add(2) == b'l'
        && *ns.add(3) == b'n'
        && *ns.add(4) == b's'
        && *ns.add(5) == 0
    {
        let val: *mut XmlChar;

        if !(*ctxt).replace_entities != 0 {
            (*ctxt).depth += 1;
            val = xml_string_decode_entities(ctxt, value, XML_SUBSTITUTE_REF as _, 0, 0, 0);
            (*ctxt).depth -= 1;
            if val.is_null() {
                xml_sax2_err_memory(ctxt, c"xmlSAX2StartElement".as_ptr() as _);
                xml_free(ns as _);
                if !name.is_null() {
                    xml_free(name as _);
                }
                if !nval.is_null() {
                    xml_free(nval as _);
                }
                return;
            }
        } else {
            val = value as _;
        }

        if *val.add(0) == 0 {
            xml_ns_err_msg(
                ctxt,
                XmlParserErrors::XmlNsErrEmpty,
                c"Empty namespace name for prefix %s\n".as_ptr() as _,
                name,
                null(),
            );
        }
        if (*ctxt).pedantic != 0 && (*val.add(0) != 0) {
            let uri: XmlURIPtr = xml_parse_uri(val as _);
            if uri.is_null() {
                xml_ns_warn_msg(
                    ctxt,
                    XmlParserErrors::XmlWarNsURI,
                    c"xmlns:%s: %s not a valid URI\n".as_ptr() as _,
                    name,
                    value,
                );
            } else {
                if (*uri).scheme.is_null() {
                    xml_ns_warn_msg(
                        ctxt,
                        XmlParserErrors::XmlWarNsURIRelative,
                        "xmlns:%s: URI %s is not absolute\n".as_ptr() as _,
                        name,
                        value,
                    );
                }
                xml_free_uri(uri);
            }
        }

        /* a standard namespace definition */
        let nsret: XmlNsPtr = xml_new_ns((*ctxt).node, val, name);
        xml_free(ns as _);
        #[cfg(feature = "libxml_valid")]
        {
            /*
             * Validate also for namespace decls, they are attributes from
             * an XML-1.0 perspective
             */
            if !nsret.is_null()
                && (*ctxt).validate != 0
                && (*ctxt).well_formed != 0
                && !(*ctxt).my_doc.is_null()
                && !(*(*ctxt).my_doc).int_subset.is_null()
            {
                (*ctxt).valid &= xml_validate_one_namespace(
                    addr_of_mut!((*ctxt).vctxt) as _,
                    (*ctxt).my_doc,
                    (*ctxt).node,
                    prefix,
                    nsret,
                    value,
                );
            }
        }
        if !name.is_null() {
            xml_free(name as _);
        }
        if !nval.is_null() {
            xml_free(nval as _);
        }
        if val != value as _ {
            xml_free(val as _);
        }
        return;
    }

    if !ns.is_null() {
        namespace = (*(*ctxt).node).search_ns(
            (*ctxt).my_doc,
            Some(CStr::from_ptr(ns as *const i8).to_string_lossy().as_ref()),
        );

        if namespace.is_null() {
            xml_ns_err_msg(
                ctxt,
                XmlParserErrors::XmlNsErrUndefinedNamespace,
                "Namespace prefix %s of attribute %s is not defined\n".as_ptr() as _,
                ns,
                name,
            );
        } else {
            let mut prop: XmlAttrPtr;

            prop = (*(*ctxt).node).properties;
            while !prop.is_null() {
                if !(*prop).ns.is_null()
                    && (xml_str_equal(name, (*prop).name)
                        && (namespace == (*prop).ns
                            || xml_str_equal((*namespace).href as _, (*(*prop).ns).href as _)))
                {
                    xml_ns_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrAttributeRedefined,
                        c"Attribute %s in %s redefined\n".as_ptr() as _,
                        name,
                        (*namespace).href as _,
                    );
                    (*ctxt).well_formed = 0;
                    if (*ctxt).recovery == 0 {
                        (*ctxt).disable_sax = 1;
                    }
                    if !name.is_null() {
                        xml_free(name as _);
                    }
                    // goto error;
                    if !nval.is_null() {
                        xml_free(nval as _);
                    }
                    if !ns.is_null() {
                        xml_free(ns as _);
                    }
                }
                prop = (*prop).next;
            }
        }
    } else {
        namespace = null_mut();
    }

    /* !!!!!! <a toto:arg="" xmlns:toto="http://toto.com"> */
    let ret: XmlAttrPtr = xml_new_ns_prop_eat_name((*ctxt).node, namespace, name, null());
    if ret.is_null() {
        // goto error;
        if !nval.is_null() {
            xml_free(nval as _);
        }
        if !ns.is_null() {
            xml_free(ns as _);
        }
    }

    if (*ctxt).replace_entities == 0 && (*ctxt).html == 0 {
        (*ret).children = if (*ctxt).my_doc.is_null() {
            None
        } else {
            NodePtr::from_ptr((*(*ctxt).my_doc).get_node_list(value))
        };
        let mut tmp = (*ret).children;
        while let Some(mut now) = tmp {
            now.parent = NodePtr::from_ptr(ret as *mut XmlNode);
            if now.next.is_none() {
                (*ret).last = Some(now);
            }
            tmp = now.next;
        }
    } else if !value.is_null() {
        (*ret).children = NodePtr::from_ptr(xml_new_doc_text((*ctxt).my_doc, value));
        (*ret).last = (*ret).children;
        if let Some(mut children) = (*ret).children {
            children.parent = NodePtr::from_ptr(ret as *mut XmlNode);
        }
    }

    #[cfg(not(feature = "libxml_valid"))]
    let f = false;
    #[cfg(feature = "libxml_valid")]
    let f = (*ctxt).html == 0
        && (*ctxt).validate != 0
        && (*ctxt).well_formed != 0
        && !(*ctxt).my_doc.is_null()
        && !(*(*ctxt).my_doc).int_subset.is_null();
    if f {
        /*
         * If we don't substitute entities, the validation should be
         * done on a value with replaced entities anyway.
         */
        if (*ctxt).replace_entities == 0 {
            let mut val: *mut XmlChar;

            (*ctxt).depth += 1;
            val = xml_string_decode_entities(ctxt, value, XML_SUBSTITUTE_REF as _, 0, 0, 0);
            (*ctxt).depth -= 1;

            if val.is_null() {
                (*ctxt).valid &= xml_validate_one_attribute(
                    addr_of_mut!((*ctxt).vctxt) as _,
                    (*ctxt).my_doc,
                    (*ctxt).node,
                    ret,
                    value,
                );
            } else {
                /*
                 * Do the last stage of the attribute normalization
                 * It need to be done twice ... it's an extra burden related
                 * to the ability to keep xmlSAX2References in attributes
                 */
                let nvalnorm: *mut XmlChar = xml_valid_normalize_attribute_value(
                    (*ctxt).my_doc,
                    (*ctxt).node,
                    fullname,
                    val,
                );
                if !nvalnorm.is_null() {
                    xml_free(val as _);
                    val = nvalnorm;
                }

                (*ctxt).valid &= xml_validate_one_attribute(
                    addr_of_mut!((*ctxt).vctxt) as _,
                    (*ctxt).my_doc,
                    (*ctxt).node,
                    ret,
                    val,
                );
                xml_free(val as _);
            }
        } else {
            (*ctxt).valid &= xml_validate_one_attribute(
                addr_of_mut!((*ctxt).vctxt) as _,
                (*ctxt).my_doc,
                (*ctxt).node,
                ret,
                value,
            );
        }
    } else if (*ctxt).loadsubset & XML_SKIP_IDS as i32 == 0
        && (((*ctxt).replace_entities == 0 && (*ctxt).external != 2)
            || ((*ctxt).replace_entities != 0 && (*ctxt).in_subset == 0))
            // Don't create IDs containing entity references
        && (*ret)
            .children
            .filter(|c| matches!(c.element_type(), XmlElementType::XmlTextNode) && c.next.is_none())
            .is_some()
    {
        let content: *mut XmlChar = (*ret).children.unwrap().content;
        // when validating, the ID registration is done at the attribute
        // validation level. Otherwise we have to do specific handling here.
        if xml_str_equal(fullname, c"xml:id".as_ptr() as _) {
            // Add the xml:id value
            //
            // Open issue: normalization of the value.
            if xml_validate_ncname(content, 1) != 0 {
                xml_err_valid(
                    ctxt,
                    XmlParserErrors::XmlDTDXmlidValue,
                    c"xml:id : attribute value %s is not an NCName\n".as_ptr() as _,
                    content as _,
                    null(),
                );
            }
            xml_add_id(
                addr_of_mut!((*ctxt).vctxt) as _,
                (*ctxt).my_doc,
                content,
                ret,
            );
        } else if xml_is_id((*ctxt).my_doc, (*ctxt).node, ret) != 0 {
            xml_add_id(
                addr_of_mut!((*ctxt).vctxt) as _,
                (*ctxt).my_doc,
                content,
                ret,
            );
        } else if xml_is_ref((*ctxt).my_doc, (*ctxt).node, ret) != 0 {
            xml_add_ref(
                addr_of_mut!((*ctxt).vctxt) as _,
                (*ctxt).my_doc,
                content,
                ret,
            );
        }
    }

    // error:
    if !nval.is_null() {
        xml_free(nval as _);
    }
    if !ns.is_null() {
        xml_free(ns as _);
    }
}

/// Check defaulted attributes from the DTD
#[doc(alias = "xmlCheckDefaultedAttributes")]
#[cfg(any(
    feature = "sax1",
    feature = "html",
    feature = "libxml_writer",
    feature = "libxml_legacy"
))]
unsafe extern "C" fn xml_check_defaulted_attributes(
    ctxt: XmlParserCtxtPtr,
    name: *const XmlChar,
    prefix: *const XmlChar,
    atts: *mut *const XmlChar,
) {
    let mut elem_decl: XmlElementPtr;
    let mut att: *const XmlChar;
    let mut internal: i32 = 1;
    let mut i: i32;

    elem_decl = xml_get_dtd_qelement_desc((*(*ctxt).my_doc).int_subset, name, prefix);
    if elem_decl.is_null() {
        elem_decl = xml_get_dtd_qelement_desc((*(*ctxt).my_doc).ext_subset, name, prefix);
        internal = 0;
    }

    // process_external_subset:
    'process_external_subset: loop {
        if !elem_decl.is_null() {
            let mut attr: XmlAttributePtr = (*elem_decl).attributes;
            /*
             * Check against defaulted attributes from the external subset
             * if the document is stamped as standalone
             */
            if (*(*ctxt).my_doc).standalone == 1
                && !(*(*ctxt).my_doc).ext_subset.is_null()
                && (*ctxt).validate != 0
            {
                while !attr.is_null() {
                    let prefix = (*attr).prefix.as_deref().map(|p| CString::new(p).unwrap());
                    let elem = (*attr).elem.as_deref().map(|p| CString::new(p).unwrap());
                    if !(*attr).default_value.is_null()
                        && xml_get_dtd_qattr_desc(
                            (*(*ctxt).my_doc).ext_subset,
                            elem.as_ref().map_or(null(), |e| e.as_ptr() as *const u8),
                            (*attr).name,
                            prefix.as_ref().map_or(null(), |p| p.as_ptr() as *const u8),
                        ) == attr
                        && xml_get_dtd_qattr_desc(
                            (*(*ctxt).my_doc).int_subset,
                            elem.as_ref().map_or(null(), |e| e.as_ptr() as *const u8),
                            (*attr).name,
                            prefix.as_ref().map_or(null(), |p| p.as_ptr() as *const u8),
                        )
                        .is_null()
                    {
                        let mut fulln: *mut XmlChar;

                        if let Some(prefix) = (*attr).prefix.as_deref() {
                            let prefix = CString::new(prefix).unwrap();
                            fulln = xml_strdup(prefix.as_ptr() as *const u8);
                            fulln = xml_strcat(fulln, c":".as_ptr() as _);
                            fulln = xml_strcat(fulln, (*attr).name);
                        } else {
                            fulln = xml_strdup((*attr).name);
                        }
                        if fulln.is_null() {
                            xml_sax2_err_memory(ctxt, c"xmlSAX2StartElement".as_ptr() as _);
                            break;
                        }

                        /*
                         * Check that the attribute is not declared in the
                         * serialization
                         */
                        att = null_mut();
                        if !atts.is_null() {
                            i = 0;
                            att = *atts.add(i as usize);
                            while !att.is_null() {
                                if xml_str_equal(att, fulln) {
                                    break;
                                }
                                i += 2;
                                att = *atts.add(i as usize);
                            }
                        }
                        if att.is_null() {
                            xml_err_valid(
                                ctxt,
                                XmlParserErrors::XmlDTDStandaloneDefaulted,
                                c"standalone: attribute %s on %s defaulted from external subset\n"
                                    .as_ptr() as _,
                                fulln as _,
                                elem.as_ref().map_or(null(), |e| e.as_ptr()),
                            );
                        }
                        xml_free(fulln as _);
                    }
                    attr = (*attr).nexth;
                }
            }

            /*
             * Actually insert defaulted values when needed
             */
            attr = (*elem_decl).attributes;
            while !attr.is_null() {
                /*
                 * Make sure that attributes redefinition occurring in the
                 * internal subset are not overridden by definitions in the
                 * external subset.
                 */
                if !(*attr).default_value.is_null() {
                    /*
                     * the element should be instantiated in the tree if:
                     *  - this is a namespace prefix
                     *  - the user required for completion in the tree
                     *    like XSLT
                     *  - there isn't already an attribute definition
                     *    in the internal subset overriding it.
                     */
                    if (*attr).prefix.as_deref() == Some("xmlns")
                        || ((*attr).prefix.is_none()
                            && xml_str_equal((*attr).name, c"xmlns".as_ptr() as _))
                        || (*ctxt).loadsubset & XML_COMPLETE_ATTRS as i32 != 0
                    {
                        let pre = (*attr).prefix.as_deref().map(|p| CString::new(p).unwrap());
                        let elem = (*attr).elem.as_deref().map(|p| CString::new(p).unwrap());
                        let tst: XmlAttributePtr = xml_get_dtd_qattr_desc(
                            (*(*ctxt).my_doc).int_subset,
                            elem.as_ref().map_or(null(), |e| e.as_ptr() as *const u8),
                            (*attr).name,
                            pre.as_ref().map_or(null(), |p| p.as_ptr() as *const u8),
                        );
                        if tst == attr || tst.is_null() {
                            let mut fname: [XmlChar; 50] = [0; 50];

                            let fulln: *mut XmlChar = xml_build_qname(
                                (*attr).name,
                                pre.as_ref().map_or(null(), |p| p.as_ptr() as *const u8),
                                fname.as_mut_ptr() as _,
                                50,
                            );
                            if fulln.is_null() {
                                xml_sax2_err_memory(ctxt, c"xmlSAX2StartElement".as_ptr() as _);
                                return;
                            }

                            /*
                             * Check that the attribute is not declared in the
                             * serialization
                             */
                            att = null_mut();
                            if !atts.is_null() {
                                i = 0;
                                att = *atts.add(i as usize);
                                while !att.is_null() {
                                    if xml_str_equal(att, fulln) {
                                        break;
                                    }
                                    i += 2;
                                    att = *atts.add(i as usize);
                                }
                            }
                            if att.is_null() {
                                xml_sax2_attribute_internal(
                                    Some(GenericErrorContext::new(ctxt)),
                                    fulln,
                                    (*attr).default_value,
                                    prefix,
                                );
                            }
                            if fulln != fname.as_ptr() as _ && fulln != (*attr).name as _ {
                                xml_free(fulln as _);
                            }
                        }
                    }
                }
                attr = (*attr).nexth;
            }
            if internal == 1 {
                elem_decl = xml_get_dtd_qelement_desc((*(*ctxt).my_doc).ext_subset, name, prefix);
                internal = 0;
                // goto process_external_subset;
                continue 'process_external_subset;
            }
        }

        break;
    }
}

/// called when an opening tag has been processed.
#[doc(alias = "xmlSAX2StartElement")]
#[cfg(any(
    feature = "sax1",
    feature = "html",
    feature = "libxml_writer",
    feature = "libxml_legacy"
))]
pub unsafe fn xml_sax2_start_element(
    ctx: Option<GenericErrorContext>,
    fullname: *const XmlChar,
    atts: *mut *const XmlChar,
) {
    use crate::libxml::parser_internals::XML_VCTXT_DTD_VALIDATED;

    let mut parent: XmlNodePtr;
    let mut ns: XmlNsPtr;
    let name: *mut XmlChar;
    let mut prefix: *mut XmlChar = null_mut();
    let mut att: *const XmlChar;
    let mut value: *const XmlChar;
    let mut i: i32;

    if ctx.is_none() || fullname.is_null() {
        return;
    }
    let ctxt = {
        let ctx = ctx.as_ref().unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    if (*ctxt).my_doc.is_null() {
        return;
    }
    parent = (*ctxt).node;

    /*
     * First check on validity:
     */
    if (*ctxt).validate != 0
        && (*(*ctxt).my_doc).ext_subset.is_null()
        && ((*(*ctxt).my_doc).int_subset.is_null()
            || ((*(*(*ctxt).my_doc).int_subset).notations.is_null()
                && (*(*(*ctxt).my_doc).int_subset).elements.is_null()
                && (*(*(*ctxt).my_doc).int_subset).attributes.is_null()
                && (*(*(*ctxt).my_doc).int_subset).entities.is_none()))
    {
        xml_err_valid(
            ctxt,
            XmlParserErrors::XmlErrNoDTD,
            c"Validation failed: no DTD found !".as_ptr() as _,
            null(),
            null(),
        );
        (*ctxt).validate = 0;
    }

    if (*ctxt).html != 0 {
        prefix = null_mut();
        name = xml_strdup(fullname);
    } else {
        /*
         * Split the full name into a namespace prefix and the tag name
         */
        name = xml_split_qname(ctxt, fullname, addr_of_mut!(prefix));
    }

    /*
     * Note : the namespace resolution is deferred until the end of the
     *        attributes parsing, since local namespace can be defined as
     *        an attribute at this level.
     */
    let ret: XmlNodePtr = xml_new_doc_node_eat_name((*ctxt).my_doc, null_mut(), name, null_mut());
    if ret.is_null() {
        if !prefix.is_null() {
            xml_free(prefix as _);
        }
        xml_sax2_err_memory(ctxt, c"xmlSAX2StartElement".as_ptr() as _);
        return;
    }
    if let Some(children) = (*(*ctxt).my_doc).children {
        if parent.is_null() {
            parent = children.as_ptr();
        }
    } else {
        (*(*ctxt).my_doc).add_child(ret as XmlNodePtr);
    }
    (*ctxt).nodemem = -1;
    if (*ctxt).linenumbers != 0 && !(*ctxt).input.is_null() {
        if ((*(*ctxt).input).line as u32) < u16::MAX as u32 {
            (*ret).line = (*(*ctxt).input).line as _;
        } else {
            (*ret).line = u16::MAX;
        }
    }

    /*
     * We are parsing a new node.
     */
    if (*ctxt).node_push(ret) < 0 {
        (*ret).unlink();
        xml_free_node(ret);
        if !prefix.is_null() {
            xml_free(prefix as _);
        }
        return;
    }

    // Link the child element
    if !parent.is_null() {
        if matches!((*parent).element_type(), XmlElementType::XmlElementNode) {
            (*parent).add_child(ret);
        } else {
            (*parent).add_sibling(ret);
        }
    }

    if (*ctxt).html == 0 {
        // Insert all the defaulted attributes from the DTD especially namespaces
        if !(*(*ctxt).my_doc).int_subset.is_null() || !(*(*ctxt).my_doc).ext_subset.is_null() {
            xml_check_defaulted_attributes(ctxt, name, prefix, atts);
        }

        // process all the attributes whose name start with "xmlns"
        if !atts.is_null() {
            i = 0;
            att = *atts.add(i as usize);
            i += 1;
            value = *atts.add(i as usize);
            i += 1;
            while !att.is_null() && !value.is_null() {
                if (*att.add(0) == b'x')
                    && (*att.add(1) == b'm')
                    && (*att.add(2) == b'l')
                    && (*att.add(3) == b'n')
                    && (*att.add(4) == b's')
                {
                    xml_sax2_attribute_internal(ctx.clone(), att, value, prefix);
                }

                att = *atts.add(i as usize);
                i += 1;
                value = *atts.add(i as usize);
                i += 1;
            }
        }

        // Search the namespace, note that since the attributes have been
        // processed, the local namespaces are available.
        let pre =
            (!prefix.is_null()).then(|| CStr::from_ptr(prefix as *const i8).to_string_lossy());
        ns = (*ret).search_ns((*ctxt).my_doc, pre.as_deref());
        if ns.is_null() && !parent.is_null() {
            ns = (*parent).search_ns((*ctxt).my_doc, pre.as_deref());
        }
        if !prefix.is_null() && ns.is_null() {
            ns = xml_new_ns(ret, null_mut(), prefix);
            xml_ns_warn_msg(
                ctxt,
                XmlParserErrors::XmlNsErrUndefinedNamespace,
                c"Namespace prefix %s is not defined\n".as_ptr() as _,
                prefix,
                null(),
            );
        }

        // set the namespace node, making sure that if the default namespace
        // is unbound on a parent we simply keep it NULL
        if !ns.is_null()
            && !(*ns).href.is_null()
            && (*(*ns).href.add(0) != 0 || !(*ns).prefix.is_null())
        {
            (*ret).set_ns(ns);
        }
    }

    // process all the other attributes
    if !atts.is_null() {
        i = 0;
        att = *atts.add(i as usize);
        i += 1;
        value = *atts.add(i as usize);
        i += 1;
        if (*ctxt).html != 0 {
            while !att.is_null() {
                xml_sax2_attribute_internal(ctx.clone(), att, value, null_mut());
                att = *atts.add(i as usize);
                i += 1;
                value = *atts.add(i as usize);
                i += 1;
            }
        } else {
            while !att.is_null() && !value.is_null() {
                if (*att.add(0) != b'x')
                    || (*att.add(1) != b'm')
                    || (*att.add(2) != b'l')
                    || (*att.add(3) != b'n')
                    || (*att.add(4) != b's')
                {
                    xml_sax2_attribute_internal(ctx.clone(), att, value, null_mut());
                }

                /*
                 * Next ones
                 */
                att = *atts.add(i as usize);
                i += 1;
                value = *atts.add(i as usize);
                i += 1;
            }
        }
    }

    #[cfg(feature = "libxml_valid")]
    {
        /*
         * If it's the Document root, finish the DTD validation and
         * check the document root element for validity
         */
        if (*ctxt).validate != 0 && (*ctxt).vctxt.flags & XML_VCTXT_DTD_VALIDATED as u32 == 0 {
            let chk: i32 = xml_validate_dtd_final(addr_of_mut!((*ctxt).vctxt) as _, (*ctxt).my_doc);
            if chk <= 0 {
                (*ctxt).valid = 0;
            }
            if chk < 0 {
                (*ctxt).well_formed = 0;
            }
            (*ctxt).valid &= xml_validate_root(addr_of_mut!((*ctxt).vctxt) as _, (*ctxt).my_doc);
            (*ctxt).vctxt.flags |= XML_VCTXT_DTD_VALIDATED as u32;
        }
    }

    if !prefix.is_null() {
        xml_free(prefix as _);
    }
}

/// called when the end of an element has been detected.
#[doc(alias = "xmlSAX2EndElement")]
#[cfg(any(
    feature = "sax1",
    feature = "html",
    feature = "libxml_writer",
    feature = "libxml_legacy"
))]
pub unsafe fn xml_sax2_end_element(ctx: Option<GenericErrorContext>, _name: *const XmlChar) {
    if ctx.is_none() {
        return;
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    let cur: XmlNodePtr = (*ctxt).node;

    (*ctxt).nodemem = -1;

    #[cfg(feature = "libxml_valid")]
    {
        if (*ctxt).validate != 0
            && (*ctxt).well_formed != 0
            && !(*ctxt).my_doc.is_null()
            && !(*(*ctxt).my_doc).int_subset.is_null()
        {
            (*ctxt).valid &=
                xml_validate_one_element(addr_of_mut!((*ctxt).vctxt) as _, (*ctxt).my_doc, cur);
        }
    }

    /*
     * end of parsing of this node.
     */
    (*ctxt).node_pop();
}

/// SAX2 callback when an element start has been detected by the parser.
/// It provides the namespace information for the element, as well as
/// the new namespace declarations on the element.
#[doc(alias = "xmlSAX2StartElementNs")]
#[allow(clippy::too_many_arguments)]
pub unsafe fn xml_sax2_start_element_ns(
    ctx: Option<GenericErrorContext>,
    mut localname: *const XmlChar,
    prefix: *const XmlChar,
    // I want to rename to `uri`, but it also appears as a local variable....
    orig_uri: *const XmlChar,
    nb_namespaces: i32,
    namespaces: *mut *const XmlChar,
    mut nb_attributes: i32,
    nb_defaulted: i32,
    attributes: *mut *const XmlChar,
) {
    let ret: XmlNodePtr;
    let mut last: XmlNsPtr = null_mut();
    let mut ns: XmlNsPtr;
    let mut uri: *const XmlChar;
    let mut pref: *const XmlChar;
    let mut lname: *mut XmlChar = null_mut();
    let mut i: i32;

    if ctx.is_none() {
        return;
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    let parent: XmlNodePtr = (*ctxt).node;
    /*
     * First check on validity:
     */
    if (*ctxt).validate != 0
        && (*(*ctxt).my_doc).ext_subset.is_null()
        && ((*(*ctxt).my_doc).int_subset.is_null()
            || ((*(*(*ctxt).my_doc).int_subset).notations.is_null()
                && (*(*(*ctxt).my_doc).int_subset).elements.is_null()
                && (*(*(*ctxt).my_doc).int_subset).attributes.is_null()
                && (*(*(*ctxt).my_doc).int_subset).entities.is_none()))
    {
        xml_err_valid(
            ctxt,
            XmlParserErrors::XmlDTDNoDTD,
            c"Validation failed: no DTD found !".as_ptr() as _,
            null(),
            null(),
        );
        (*ctxt).validate = 0;
    }

    // Take care of the rare case of an undefined namespace prefix
    if !prefix.is_null() && orig_uri.is_null() {
        if (*ctxt).dict_names != 0 {
            let fullname: *const XmlChar = xml_dict_qlookup((*ctxt).dict, prefix, localname);
            if !fullname.is_null() {
                localname = fullname;
            }
        } else {
            lname = xml_build_qname(localname, prefix, null_mut(), 0);
        }
    }
    // allocate the node
    if !(*ctxt).free_elems.is_null() {
        ret = (*ctxt).free_elems;
        (*ctxt).free_elems = (*ret).next.map_or(null_mut(), |n| n.as_ptr());
        (*ctxt).free_elems_nr -= 1;
        memset(ret as _, 0, size_of::<XmlNode>());
        (*ret).doc = (*ctxt).my_doc;
        (*ret).typ = XmlElementType::XmlElementNode;

        if (*ctxt).dict_names != 0 {
            (*ret).name = localname;
        } else {
            if lname.is_null() {
                (*ret).name = xml_strdup(localname);
            } else {
                (*ret).name = lname;
            }
            if (*ret).name.is_null() {
                xml_sax2_err_memory(ctxt, c"xmlSAX2StartElementNs".as_ptr() as _);
                xml_free(ret as _);
                return;
            }
        }
        if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
        // && xmlRegisterNodeDefaultValue.is_some()
        {
            xml_register_node_default_value(ret);
        }
    } else {
        if (*ctxt).dict_names != 0 {
            ret = xml_new_doc_node_eat_name((*ctxt).my_doc, null_mut(), localname as _, null_mut());
        } else if lname.is_null() {
            ret = xml_new_doc_node((*ctxt).my_doc, null_mut(), localname, null_mut());
        } else {
            ret = xml_new_doc_node_eat_name((*ctxt).my_doc, null_mut(), lname, null_mut());
        }
        if ret.is_null() {
            xml_sax2_err_memory(ctxt, c"xmlSAX2StartElementNs".as_ptr() as _);
            return;
        }
    }
    if (*ctxt).linenumbers != 0 && !(*ctxt).input.is_null() {
        if ((*(*ctxt).input).line as u32) < u16::MAX as u32 {
            (*ret).line = (*(*ctxt).input).line as _;
        } else {
            (*ret).line = u16::MAX;
        }
    }

    if parent.is_null() {
        (*(*ctxt).my_doc).add_child(ret as XmlNodePtr);
    }
    /*
     * Build the namespace list
     */
    i = 0;
    for _ in 0..nb_namespaces {
        pref = *namespaces.add(i as usize);
        i += 1;
        uri = *namespaces.add(i as usize);
        i += 1;
        ns = xml_new_ns(null_mut(), uri, pref);
        if !ns.is_null() {
            if last.is_null() {
                (*ret).ns_def = ns;
                last = ns;
            } else {
                (*last).next = ns;
                last = ns;
            }
            if !orig_uri.is_null() && prefix == pref {
                (*ret).ns = ns;
            }
        } else {
            /*
             * any out of memory error would already have been raised
             * but we can't be guaranteed it's the actual error due to the
             * API, best is to skip in this case
             */
            continue;
        }
        #[cfg(feature = "libxml_valid")]
        {
            if (*ctxt).html == 0
                && (*ctxt).validate != 0
                && (*ctxt).well_formed != 0
                && !(*ctxt).my_doc.is_null()
                && !(*(*ctxt).my_doc).int_subset.is_null()
            {
                (*ctxt).valid &= xml_validate_one_namespace(
                    addr_of_mut!((*ctxt).vctxt) as _,
                    (*ctxt).my_doc,
                    ret,
                    prefix,
                    ns,
                    uri,
                );
            }
        }
    }
    (*ctxt).nodemem = -1;

    /*
     * We are parsing a new node.
     */
    if (*ctxt).node_push(ret) < 0 {
        (*ret).unlink();
        xml_free_node(ret);
        return;
    }

    /*
     * Link the child element
     */
    if !parent.is_null() {
        if matches!((*parent).element_type(), XmlElementType::XmlElementNode) {
            (*parent).add_child(ret);
        } else {
            (*parent).add_sibling(ret);
        }
    }

    /*
     * Insert the defaulted attributes from the DTD only if requested:
     */
    if nb_defaulted != 0 && (*ctxt).loadsubset & XML_COMPLETE_ATTRS as i32 == 0 {
        nb_attributes -= nb_defaulted;
    }

    /*
     * Search the namespace if it wasn't already found
     * Note that, if prefix is NULL, this searches for the default Ns
     */
    if !orig_uri.is_null() && (*ret).ns.is_null() {
        let pre =
            (!prefix.is_null()).then(|| CStr::from_ptr(prefix as *const i8).to_string_lossy());
        (*ret).ns = if !parent.is_null() {
            (*parent).search_ns((*ctxt).my_doc, pre.as_deref())
        } else {
            null_mut()
        };
        if (*ret).ns.is_null() && xml_str_equal(prefix, c"xml".as_ptr() as _) {
            (*ret).ns = (*ret).search_ns((*ctxt).my_doc, pre.as_deref());
        }
        if (*ret).ns.is_null() {
            ns = xml_new_ns(ret, null_mut(), prefix);
            if ns.is_null() {
                xml_sax2_err_memory(ctxt, c"xmlSAX2StartElementNs".as_ptr() as _);
                return;
            }
            if !prefix.is_null() {
                xml_ns_warn_msg(
                    ctxt,
                    XmlParserErrors::XmlNsErrUndefinedNamespace,
                    c"Namespace prefix %s was not found\n".as_ptr() as _,
                    prefix,
                    null(),
                );
            } else {
                xml_ns_warn_msg(
                    ctxt,
                    XmlParserErrors::XmlNsErrUndefinedNamespace,
                    c"Namespace default prefix was not found\n".as_ptr() as _,
                    null(),
                    null(),
                );
            }
        }
    }

    /*
     * process all the other attributes
     */
    if nb_attributes > 0 {
        for (_, j) in (0..nb_attributes as usize).zip((0..).step_by(5)) {
            /*
             * Handle the rare case of an undefined attribute prefix
             */
            if !(*attributes.add(j + 1)).is_null() && (*attributes.add(j + 2)).is_null() {
                if (*ctxt).dict_names != 0 {
                    let fullname: *const XmlChar =
                        xml_dict_qlookup((*ctxt).dict, *attributes.add(j + 1), *attributes.add(j));
                    if !fullname.is_null() {
                        xml_sax2_attribute_ns(
                            ctxt,
                            fullname,
                            null_mut(),
                            *attributes.add(j + 3),
                            *attributes.add(j + 4),
                        );
                        continue;
                    }
                } else {
                    lname =
                        xml_build_qname(*attributes.add(j), *attributes.add(j + 1), null_mut(), 0);
                    if !lname.is_null() {
                        xml_sax2_attribute_ns(
                            ctxt,
                            lname,
                            null_mut(),
                            *attributes.add(j + 3),
                            *attributes.add(j + 4),
                        );
                        xml_free(lname as _);
                        continue;
                    }
                }
            }
            xml_sax2_attribute_ns(
                ctxt,
                *attributes.add(j),
                *attributes.add(j + 1),
                *attributes.add(j + 3),
                *attributes.add(j + 4),
            );
        }
    }

    #[cfg(feature = "libxml_valid")]
    {
        // If it's the Document root, finish the DTD validation and
        // check the document root element for validity
        if (*ctxt).validate != 0 && (*ctxt).vctxt.flags & XML_VCTXT_DTD_VALIDATED as u32 == 0 {
            let chk: i32 = xml_validate_dtd_final(addr_of_mut!((*ctxt).vctxt) as _, (*ctxt).my_doc);
            if chk <= 0 {
                (*ctxt).valid = 0;
            }
            if chk < 0 {
                (*ctxt).well_formed = 0;
            }
            (*ctxt).valid &= xml_validate_root(addr_of_mut!((*ctxt).vctxt) as _, (*ctxt).my_doc);
            (*ctxt).vctxt.flags |= XML_VCTXT_DTD_VALIDATED as u32;
        }
    }
}

/// Returns the newly allocated string or NULL if not needed or error
#[doc(alias = "xmlSAX2TextNode")]
unsafe extern "C" fn xml_sax2_text_node(
    ctxt: XmlParserCtxtPtr,
    str: *const XmlChar,
    len: i32,
) -> XmlNodePtr {
    let mut intern: *const XmlChar = null();

    /*
     * Allocate
     */
    let ret: XmlNodePtr = if !(*ctxt).free_elems.is_null() {
        let ret = (*ctxt).free_elems;
        (*ctxt).free_elems = (*ret).next.map_or(null_mut(), |n| n.as_ptr());
        (*ctxt).free_elems_nr -= 1;
        ret
    } else {
        xml_malloc(size_of::<XmlNode>()) as XmlNodePtr
    };
    if ret.is_null() {
        xml_err_memory(ctxt, c"xmlSAX2Characters".as_ptr() as _);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlNode>());
    /*
     * intern the formatting blanks found between tags, or the
     * very short strings
     */
    if (*ctxt).dict_names != 0 {
        let cur: XmlChar = *str.add(len as usize);

        if len < 2 * size_of::<*mut c_void>() as i32
            && (*ctxt).options & XmlParserOption::XmlParseCompact as i32 != 0
        {
            /* store the string in the node overriding properties and nsDef */
            let tmp: *mut XmlChar = addr_of_mut!((*ret).properties) as _;
            memcpy(tmp as _, str as _, len as _);
            *tmp.add(len as usize) = 0;
            intern = tmp;
        } else if len <= 3
            && (cur == b'"' || cur == b'\'' || (cur == b'<' && *str.add(len as usize + 1) != b'!'))
        {
            intern = xml_dict_lookup((*ctxt).dict, str, len);
        } else if xml_is_blank_char(*str as u32)
            && len < 60
            && cur == b'<'
            && *str.add(len as usize + 1) != b'!'
        {
            let mut skip = false;
            for i in 1..len {
                if !xml_is_blank_char(*str.add(i as usize) as u32) {
                    // goto skip;
                    skip = true;
                    break;
                }
            }
            if !skip {
                intern = xml_dict_lookup((*ctxt).dict, str, len);
            }
        }
    }
    // skip:
    (*ret).typ = XmlElementType::XmlTextNode;

    (*ret).name = XML_STRING_TEXT.as_ptr() as _;
    if intern.is_null() {
        (*ret).content = xml_strndup(str, len);
        if (*ret).content.is_null() {
            xml_sax2_err_memory(ctxt, c"xmlSAX2TextNode".as_ptr() as _);
            xml_free(ret as _);
            return null_mut();
        }
    } else {
        (*ret).content = intern as _;
    }

    if (*ctxt).linenumbers != 0 && !(*ctxt).input.is_null() {
        if ((*(*ctxt).input).line as u32) < u32::MAX {
            (*ret).line = (*(*ctxt).input).line as _;
        } else {
            (*ret).line = u16::MAX;
            if (*ctxt).options & XmlParserOption::XmlParseBigLines as i32 != 0 {
                (*ret).psvi = (*(*ctxt).input).line as isize as *mut c_void;
            }
        }
    }

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    //  && xmlRegisterNodeDefaultValue.is_some()
    {
        xml_register_node_default_value(ret);
    }
    ret
}

/// Remove the entities from an attribute value
///
/// Returns the newly allocated string or NULL if not needed or error
#[doc(alias = "xmlSAX2DecodeAttrEntities")]
#[cfg(feature = "libxml_valid")]
unsafe extern "C" fn xml_sax2_decode_attr_entities(
    ctxt: XmlParserCtxtPtr,
    str: *const XmlChar,
    end: *const XmlChar,
) -> *mut XmlChar {
    let mut input: *const XmlChar;
    let ret: *mut XmlChar;

    input = str;
    while input < end {
        let res = {
            let f = *input == b'&';
            input = input.add(1);
            f
        };
        if res {
            (*ctxt).depth += 1;
            ret = xml_string_len_decode_entities(
                ctxt,
                str,
                end.offset_from(str) as _,
                XML_SUBSTITUTE_REF as _,
                0,
                0,
                0,
            );
            (*ctxt).depth -= 1;
            return ret;
        }
    }
    null_mut()
}

/// Handle an attribute that has been read by the parser.
/// The default handling is to convert the attribute into an
/// DOM subtree and past it in a new xmlAttr element added to the element.
#[doc(alias = "xmlSAX2AttributeNs")]
unsafe extern "C" fn xml_sax2_attribute_ns(
    ctxt: XmlParserCtxtPtr,
    localname: *const XmlChar,
    prefix: *const XmlChar,
    value: *const XmlChar,
    valueend: *const XmlChar,
) {
    let ret: XmlAttrPtr;
    let mut namespace: XmlNsPtr = null_mut();
    let mut dup: *mut XmlChar = null_mut();

    /*
     * Note: if prefix.is_null(), the attribute is not in the default namespace
     */
    if !prefix.is_null() {
        namespace = (*(*ctxt).node).search_ns(
            (*ctxt).my_doc,
            Some(
                CStr::from_ptr(prefix as *const i8)
                    .to_string_lossy()
                    .as_ref(),
            ),
        );
    }

    /*
     * allocate the node
     */
    if !(*ctxt).free_attrs.is_null() {
        ret = (*ctxt).free_attrs;
        (*ctxt).free_attrs = (*ret).next;
        (*ctxt).free_attrs_nr -= 1;
        memset(ret as _, 0, size_of::<XmlAttr>());
        (*ret).typ = XmlElementType::XmlAttributeNode;

        (*ret).parent = NodePtr::from_ptr((*ctxt).node);
        (*ret).doc = (*ctxt).my_doc;
        (*ret).ns = namespace;

        if (*ctxt).dict_names != 0 {
            (*ret).name = localname;
        } else {
            (*ret).name = xml_strdup(localname);
        }

        /* link at the end to preserve order, TODO speed up with a last */
        if (*(*ctxt).node).properties.is_null() {
            (*(*ctxt).node).properties = ret;
        } else {
            let mut prev: XmlAttrPtr = (*(*ctxt).node).properties;

            while !(*prev).next.is_null() {
                prev = (*prev).next;
            }
            (*prev).next = ret;
            (*ret).prev = prev;
        }

        if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
        // && xmlRegisterNodeDefaultValue.is_some()
        {
            xml_register_node_default_value(ret as XmlNodePtr);
        }
    } else {
        if (*ctxt).dict_names != 0 {
            ret = xml_new_ns_prop_eat_name((*ctxt).node, namespace, localname as _, null_mut());
        } else {
            ret = xml_new_ns_prop((*ctxt).node, namespace, localname, null_mut());
        }
        if ret.is_null() {
            xml_err_memory(ctxt, c"xmlSAX2AttributeNs".as_ptr() as _);
            return;
        }
    }

    if (*ctxt).replace_entities == 0 && (*ctxt).html == 0 {
        /*
         * We know that if there is an entity reference, then
         * the string has been dup'ed and terminates with 0
         * otherwise with ' or "
         */
        if *valueend != 0 {
            let tmp = xml_sax2_text_node(ctxt, value, valueend.offset_from(value) as _);
            (*ret).children = NodePtr::from_ptr(tmp);
            (*ret).last = NodePtr::from_ptr(tmp);
            if !tmp.is_null() {
                (*tmp).doc = (*ret).doc;
                (*tmp).parent = NodePtr::from_ptr(ret as *mut XmlNode);
            }
        } else {
            (*ret).children = if (*ctxt).my_doc.is_null() {
                None
            } else {
                NodePtr::from_ptr(
                    (*(*ctxt).my_doc)
                        .get_node_list_with_strlen(value, valueend.offset_from(value) as _),
                )
            };
            let mut tmp = (*ret).children;
            while let Some(mut now) = tmp {
                now.doc = (*ret).doc;
                now.parent = NodePtr::from_ptr(ret as *mut XmlNode);
                if now.next.is_none() {
                    (*ret).last = Some(now);
                }
                tmp = now.next;
            }
        }
    } else if !value.is_null() {
        let tmp: XmlNodePtr = xml_sax2_text_node(ctxt, value, valueend.offset_from(value) as _);
        (*ret).children = NodePtr::from_ptr(tmp);
        (*ret).last = NodePtr::from_ptr(tmp);
        if !tmp.is_null() {
            (*tmp).doc = (*ret).doc;
            (*tmp).parent = NodePtr::from_ptr(ret as *mut XmlNode);
        }
    }

    #[cfg(not(feature = "libxml_valid"))]
    let f = false;
    #[cfg(feature = "libxml_valid")]
    let f = (*ctxt).html == 0
        && (*ctxt).validate != 0
        && (*ctxt).well_formed != 0
        && !(*ctxt).my_doc.is_null()
        && !(*(*ctxt).my_doc).int_subset.is_null();
    if f {
        #[cfg(feature = "libxml_valid")]
        {
            /*
             * If we don't substitute entities, the validation should be
             * done on a value with replaced entities anyway.
             */
            if (*ctxt).replace_entities == 0 {
                dup = xml_sax2_decode_attr_entities(ctxt, value, valueend);
                if dup.is_null() {
                    if *valueend == 0 {
                        (*ctxt).valid &= xml_validate_one_attribute(
                            addr_of_mut!((*ctxt).vctxt) as _,
                            (*ctxt).my_doc,
                            (*ctxt).node,
                            ret,
                            value,
                        );
                    } else {
                        /*
                         * That should already be normalized.
                         * cheaper to finally allocate here than duplicate
                         * entry points in the full validation code
                         */
                        dup = xml_strndup(value, valueend.offset_from(value) as _);

                        (*ctxt).valid &= xml_validate_one_attribute(
                            addr_of_mut!((*ctxt).vctxt) as _,
                            (*ctxt).my_doc,
                            (*ctxt).node,
                            ret,
                            dup,
                        );
                    }
                } else {
                    /*
                     * dup now contains a string of the flattened attribute
                     * content with entities substituted. Check if we need to
                     * apply an extra layer of normalization.
                     * It need to be done twice ... it's an extra burden related
                     * to the ability to keep references in attributes
                     */
                    if (*ctxt).atts_special.is_some() {
                        let nvalnorm: *mut XmlChar;
                        let mut fname: [XmlChar; 50] = [0; 50];

                        let fullname: *mut XmlChar =
                            xml_build_qname(localname, prefix, fname.as_mut_ptr() as _, 50);
                        if !fullname.is_null() {
                            (*ctxt).vctxt.valid = 1;
                            nvalnorm = xml_valid_ctxt_normalize_attribute_value(
                                addr_of_mut!((*ctxt).vctxt) as _,
                                (*ctxt).my_doc,
                                (*ctxt).node,
                                fullname,
                                dup,
                            );
                            if (*ctxt).vctxt.valid != 1 {
                                (*ctxt).valid = 0;
                            }

                            if fullname != fname.as_mut_ptr() && fullname != localname as _ {
                                xml_free(fullname as _);
                            }
                            if !nvalnorm.is_null() {
                                xml_free(dup as _);
                                dup = nvalnorm;
                            }
                        }
                    }

                    (*ctxt).valid &= xml_validate_one_attribute(
                        addr_of_mut!((*ctxt).vctxt) as _,
                        (*ctxt).my_doc,
                        (*ctxt).node,
                        ret,
                        dup,
                    );
                }
            } else {
                /*
                 * if entities already have been substituted, then
                 * the attribute as passed is already normalized
                 */
                dup = xml_strndup(value, valueend.offset_from(value) as _);

                (*ctxt).valid &= xml_validate_one_attribute(
                    addr_of_mut!((*ctxt).vctxt) as _,
                    (*ctxt).my_doc,
                    (*ctxt).node,
                    ret,
                    dup,
                );
            }
        }
    } else if (*ctxt).loadsubset & XML_SKIP_IDS as i32 == 0
        && (((*ctxt).replace_entities == 0 && (*ctxt).external != 2)
            || ((*ctxt).replace_entities != 0 && (*ctxt).in_subset == 0))
            /* Don't create IDs containing entity references */
        && (*ret)
            .children
            .filter(|c| matches!(c.element_type(), XmlElementType::XmlTextNode) && c.next.is_none())
            .is_some()
    {
        let content: *mut XmlChar = (*ret).children.unwrap().content;
        /*
         * when validating, the ID registration is done at the attribute
         * validation level. Otherwise we have to do specific handling here.
         */
        if prefix == (*ctxt).str_xml
            && *localname.add(0) == b'i'
            && *localname.add(1) == b'd'
            && *localname.add(2) == 0
        {
            /*
             * Add the xml:id value
             *
             * Open issue: normalization of the value.
             */
            #[cfg(any(
                feature = "sax1",
                feature = "html",
                feature = "libxml_writer",
                feature = "libxml_legacy"
            ))]
            {
                #[cfg(feature = "libxml_valid")]
                if xml_validate_ncname(content, 1) != 0 {
                    xml_err_valid(
                        ctxt,
                        XmlParserErrors::XmlDTDXmlidValue,
                        c"xml:id : attribute value %s is not an NCName\n".as_ptr() as _,
                        content as _,
                        null(),
                    );
                }
            }
            xml_add_id(
                addr_of_mut!((*ctxt).vctxt) as _,
                (*ctxt).my_doc,
                content,
                ret,
            );
        } else if xml_is_id((*ctxt).my_doc, (*ctxt).node, ret) != 0 {
            xml_add_id(
                addr_of_mut!((*ctxt).vctxt) as _,
                (*ctxt).my_doc,
                content,
                ret,
            );
        } else if xml_is_ref((*ctxt).my_doc, (*ctxt).node, ret) != 0 {
            xml_add_ref(
                addr_of_mut!((*ctxt).vctxt) as _,
                (*ctxt).my_doc,
                content,
                ret,
            );
        }
    }
    if !dup.is_null() {
        xml_free(dup as _);
    }
}

/// SAX2 callback when an element end has been detected by the parser.
/// It provides the namespace information for the element.
#[doc(alias = "xmlSAX2EndElementNs")]
pub unsafe fn xml_sax2_end_element_ns(
    ctx: Option<GenericErrorContext>,
    _localname: *const XmlChar,
    _prefix: *const XmlChar,
    _uri: *const XmlChar,
) {
    if ctx.is_none() {
        return;
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    (*ctxt).nodemem = -1;

    #[cfg(feature = "libxml_valid")]
    {
        if (*ctxt).validate != 0
            && (*ctxt).well_formed != 0
            && !(*ctxt).my_doc.is_null()
            && !(*(*ctxt).my_doc).int_subset.is_null()
        {
            (*ctxt).valid &= xml_validate_one_element(
                addr_of_mut!((*ctxt).vctxt) as _,
                (*ctxt).my_doc,
                (*ctxt).node,
            );
        }
    }

    /*
     * end of parsing of this node.
     */
    (*ctxt).node_pop();
}

/// Called when an entity xmlSAX2Reference is detected.
#[doc(alias = "xmlSAX2Reference")]
pub unsafe fn xml_sax2_reference(ctx: Option<GenericErrorContext>, name: *const XmlChar) {
    if ctx.is_none() {
        return;
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };

    let ret = if *name.add(0) == b'#' {
        xml_new_char_ref((*ctxt).my_doc, name)
    } else {
        xml_new_reference((*ctxt).my_doc, name)
    };
    if (*ctxt).node.is_null() || (*(*ctxt).node).add_child(ret).is_null() {
        xml_free_node(ret);
    }
}

/// Append characters.
#[doc(alias = "xmlSAX2Text")]
unsafe extern "C" fn xml_sax2_text(
    ctxt: XmlParserCtxtPtr,
    ch: *const XmlChar,
    len: i32,
    typ: XmlElementType,
) {
    if ctxt.is_null() {
        return;
    }
    /*
     * Handle the data if any. If there is no child
     * add it as content, otherwise if the last child is text,
     * concatenate it, else create a new node of type text.
     */
    if (*ctxt).node.is_null() {
        return;
    }
    let mut last_child = (*(*ctxt).node).last.map_or(null_mut(), |l| l.as_ptr());

    /*
     * Here we needed an accelerator mechanism in case of very large
     * elements. Use an attribute in the structure !!!
     */
    if last_child.is_null() {
        if matches!(typ, XmlElementType::XmlTextNode) {
            last_child = xml_sax2_text_node(ctxt, ch, len);
        } else {
            last_child = xml_new_cdata_block((*ctxt).my_doc, ch, len);
        }
        if !last_child.is_null() {
            (*(*ctxt).node).set_children(NodePtr::from_ptr(last_child));
            (*(*ctxt).node).last = NodePtr::from_ptr(last_child);
            (*last_child).parent = NodePtr::from_ptr((*ctxt).node);
            (*last_child).doc = (*(*ctxt).node).doc;
            (*ctxt).nodelen = len;
            (*ctxt).nodemem = len + 1;
        } else {
            xml_sax2_err_memory(ctxt, c"xmlSAX2Characters".as_ptr() as _);
        }
    } else {
        let coalesce_text: i32 = (!last_child.is_null()
            && (*last_child).element_type() == typ
            && (!matches!(typ, XmlElementType::XmlTextNode)
                || ((*last_child).name == XML_STRING_TEXT.as_ptr() as _)))
            as i32;
        if coalesce_text != 0 && (*ctxt).nodemem != 0 {
            /*
             * The whole point of maintaining nodelen and nodemem,
             * xmlTextConcat is too costly, i.e. compute length,
             * reallocate a new buffer, move data, append ch. Here
             * We try to minimize realloc() uses and avoid copying
             * and recomputing length over and over.
             */
            if (*last_child).content == addr_of_mut!((*last_child).properties) as *mut XmlChar {
                (*last_child).content = xml_strdup((*last_child).content);
                (*last_child).properties = null_mut();
            } else if (*ctxt).nodemem == (*ctxt).nodelen + 1
                && xml_dict_owns((*ctxt).dict, (*last_child).content) != 0
            {
                (*last_child).content = xml_strdup((*last_child).content);
            }
            if (*last_child).content.is_null() {
                xml_sax2_err_memory(
                    ctxt,
                    c"xmlSAX2Characters: xmlStrdup returned NULL".as_ptr() as _,
                );
                return;
            }
            if (*ctxt).nodelen > INT_MAX - len {
                xml_sax2_err_memory(ctxt, c"xmlSAX2Characters overflow prevented".as_ptr() as _);
                return;
            }
            if (*ctxt).nodelen + len > XML_MAX_TEXT_LENGTH as i32
                && (*ctxt).options & XmlParserOption::XmlParseHuge as i32 == 0
            {
                xml_sax2_err_memory(ctxt, c"xmlSAX2Characters: huge text node".as_ptr() as _);
                return;
            }
            if (*ctxt).nodelen + len >= (*ctxt).nodemem {
                let mut size: i32;

                size = if (*ctxt).nodemem > INT_MAX - len {
                    INT_MAX
                } else {
                    (*ctxt).nodemem + len
                };
                size = if size > INT_MAX / 2 {
                    INT_MAX
                } else {
                    size * 2
                };
                let newbuf: *mut XmlChar = xml_realloc((*last_child).content as _, size as _) as _;
                if newbuf.is_null() {
                    xml_sax2_err_memory(ctxt, c"xmlSAX2Characters".as_ptr() as _);
                    return;
                }
                (*ctxt).nodemem = size;
                (*last_child).content = newbuf;
            }
            memcpy(
                (*last_child).content.add((*ctxt).nodelen as usize) as _,
                ch as _,
                len as _,
            );
            (*ctxt).nodelen += len;
            *(*last_child).content.add((*ctxt).nodelen as usize) = 0;
        } else if coalesce_text != 0 {
            if xml_text_concat(last_child, ch, len) != 0 {
                xml_sax2_err_memory(ctxt, c"xmlSAX2Characters".as_ptr() as _);
            }
            if (*(*ctxt).node).children().is_some() {
                (*ctxt).nodelen = xml_strlen((*last_child).content);
                (*ctxt).nodemem = (*ctxt).nodelen + 1;
            }
        } else {
            /* Mixed content, first time */
            if matches!(typ, XmlElementType::XmlTextNode) {
                last_child = xml_sax2_text_node(ctxt, ch, len);
                if !last_child.is_null() {
                    (*last_child).doc = (*ctxt).my_doc;
                }
            } else {
                last_child = xml_new_cdata_block((*ctxt).my_doc, ch, len);
            }
            if !last_child.is_null() {
                (*(*ctxt).node).add_child(last_child);
                if (*(*ctxt).node).children().is_some() {
                    (*ctxt).nodelen = len;
                    (*ctxt).nodemem = len + 1;
                }
            }
        }
    }
}

/// Receiving some chars from the parser.
#[doc(alias = "xmlSAX2Characters")]
pub unsafe fn xml_sax2_characters(ctx: Option<GenericErrorContext>, ch: *const XmlChar, len: i32) {
    let ctxt = if let Some(ctx) = ctx {
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    } else {
        null_mut()
    };
    xml_sax2_text(
        ctxt as XmlParserCtxtPtr,
        ch,
        len,
        XmlElementType::XmlTextNode,
    );
}

/// Receiving some ignorable whitespaces from the parser.
/// UNUSED: by default the DOM building will use xmlSAX2Characters
#[doc(alias = "xmlSAX2IgnorableWhitespace")]
pub unsafe fn xml_sax2_ignorable_whitespace(
    _ctx: Option<GenericErrorContext>,
    _ch: *const XmlChar,
    _len: i32,
) {
    /* let ctxt: xmlParserCtxtPtr = ctx as xmlParserCtxtPtr; */
}

/// A processing instruction has been parsed.
#[doc(alias = "xmlSAX2ProcessingInstruction")]
pub unsafe fn xml_sax2_processing_instruction(
    ctx: Option<GenericErrorContext>,
    target: *const XmlChar,
    data: *const XmlChar,
) {
    if ctx.is_none() {
        return;
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };

    let parent: XmlNodePtr = (*ctxt).node;

    let ret: XmlNodePtr = xml_new_doc_pi((*ctxt).my_doc, target, data);
    if ret.is_null() {
        return;
    }

    if (*ctxt).linenumbers != 0 && !(*ctxt).input.is_null() {
        if ((*(*ctxt).input).line as u32) < u16::MAX as u32 {
            (*ret).line = (*(*ctxt).input).line as _;
        } else {
            (*ret).line = u16::MAX;
        }
    }
    if (*ctxt).in_subset == 1 {
        (*(*(*ctxt).my_doc).int_subset).add_child(ret);
        return;
    } else if (*ctxt).in_subset == 2 {
        (*(*(*ctxt).my_doc).ext_subset).add_child(ret);
        return;
    }
    if parent.is_null() {
        (*(*ctxt).my_doc).add_child(ret as XmlNodePtr);
        return;
    }
    if matches!((*parent).element_type(), XmlElementType::XmlElementNode) {
        (*parent).add_child(ret);
    } else {
        (*parent).add_sibling(ret);
    }
}

/// A xmlSAX2Comment has been parsed.
#[doc(alias = "xmlSAX2Comment")]
pub unsafe fn xml_sax2_comment(ctx: Option<GenericErrorContext>, value: *const XmlChar) {
    if ctx.is_none() {
        return;
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    let parent: XmlNodePtr = (*ctxt).node;
    let ret: XmlNodePtr = xml_new_doc_comment((*ctxt).my_doc, value);
    if ret.is_null() {
        return;
    }
    if (*ctxt).linenumbers != 0 && !(*ctxt).input.is_null() {
        if ((*(*ctxt).input).line as u32) < u16::MAX as u32 {
            (*ret).line = (*(*ctxt).input).line as _;
        } else {
            (*ret).line = u16::MAX;
        }
    }

    if (*ctxt).in_subset == 1 {
        (*(*(*ctxt).my_doc).int_subset).add_child(ret);
        return;
    } else if (*ctxt).in_subset == 2 {
        (*(*(*ctxt).my_doc).ext_subset).add_child(ret);
        return;
    }
    if parent.is_null() {
        (*(*ctxt).my_doc).add_child(ret as XmlNodePtr);
        return;
    }
    if matches!((*parent).element_type(), XmlElementType::XmlElementNode) {
        (*parent).add_child(ret);
    } else {
        (*parent).add_sibling(ret);
    }
}

/// Called when a pcdata block has been parsed
#[doc(alias = "xmlSAX2CDataBlock")]
pub unsafe fn xml_sax2_cdata_block(
    ctx: Option<GenericErrorContext>,
    value: *const XmlChar,
    len: i32,
) {
    let ctxt = if let Some(ctx) = ctx {
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    } else {
        null_mut()
    };
    xml_sax2_text(ctxt, value, len, XmlElementType::XmlCDATASectionNode);
}

static mut XML_SAX2_DEFAULT_VERSION_VALUE: i32 = 2;

/// Set the default version of SAX used globally by the library.
/// By default, during initialization the default is set to 2.
/// Note that it is generally a better coding style to use
/// xmlSAXVersion() to set up the version explicitly for a given
/// parsing context.
///
/// Returns the previous value in case of success and -1 in case of error.
#[doc(alias = "xmlSAXDefaultVersion")]
#[deprecated = "Use parser option XML_PARSE_SAX1"]
#[cfg(feature = "sax1")]
pub unsafe extern "C" fn xml_sax_default_version(version: i32) -> i32 {
    let ret: i32 = XML_SAX2_DEFAULT_VERSION_VALUE;

    if version != 1 && version != 2 {
        return -1;
    }
    XML_SAX2_DEFAULT_VERSION_VALUE = version;
    ret
}

/// Initialize the default XML SAX handler according to the version
///
/// Returns 0 in case of success and -1 in case of error.
#[doc(alias = "xmlSAXVersion")]
pub unsafe extern "C" fn xml_sax_version(hdlr: *mut XmlSAXHandler, version: i32) -> i32 {
    if hdlr.is_null() {
        return -1;
    }
    if version == 2 {
        (*hdlr).start_element = None;
        (*hdlr).end_element = None;
        (*hdlr).start_element_ns = Some(xml_sax2_start_element_ns);
        (*hdlr).end_element_ns = Some(xml_sax2_end_element_ns);
        (*hdlr).serror = None;
        (*hdlr).initialized = XML_SAX2_MAGIC as _;
    } else if cfg!(feature = "sax1") && version == 1 {
        #[cfg(feature = "sax1")]
        {
            (*hdlr).start_element = Some(xml_sax2_start_element);
            (*hdlr).end_element = Some(xml_sax2_end_element);
            (*hdlr).initialized = 1;
        }
    } else {
        return -1;
    }
    (*hdlr).internal_subset = Some(xml_sax2_internal_subset);
    (*hdlr).external_subset = Some(xml_sax2_external_subset);
    (*hdlr).is_standalone = Some(xml_sax2_is_standalone);
    (*hdlr).has_internal_subset = Some(xml_sax2_has_internal_subset);
    (*hdlr).has_external_subset = Some(xml_sax2_has_external_subset);
    (*hdlr).resolve_entity = Some(xml_sax2_resolve_entity);
    (*hdlr).get_entity = Some(xml_sax2_get_entity);
    (*hdlr).get_parameter_entity = Some(xml_sax2_get_parameter_entity);
    (*hdlr).entity_decl = Some(xml_sax2_entity_decl);
    (*hdlr).attribute_decl = Some(xml_sax2_attribute_decl);
    (*hdlr).element_decl = Some(xml_sax2_element_decl);
    (*hdlr).notation_decl = Some(xml_sax2_notation_decl);
    (*hdlr).unparsed_entity_decl = Some(xml_sax2_unparsed_entity_decl);
    (*hdlr).set_document_locator = Some(xml_sax2_set_document_locator);
    (*hdlr).start_document = Some(xml_sax2_start_document);
    (*hdlr).end_document = Some(xml_sax2_end_document);
    (*hdlr).reference = Some(xml_sax2_reference);
    (*hdlr).characters = Some(xml_sax2_characters);
    (*hdlr).cdata_block = Some(xml_sax2_cdata_block);
    (*hdlr).ignorable_whitespace = Some(xml_sax2_characters);
    (*hdlr).processing_instruction = Some(xml_sax2_processing_instruction);
    (*hdlr).comment = Some(xml_sax2_comment);
    (*hdlr).warning = Some(parser_warning);
    (*hdlr).error = Some(parser_error);
    (*hdlr).fatal_error = Some(parser_error);

    0
}

/// Initialize the default XML SAX2 handler
#[doc(alias = "xmlSAX2InitDefaultSAXHandler")]
pub unsafe extern "C" fn xml_sax2_init_default_sax_handler(hdlr: *mut XmlSAXHandler, warning: i32) {
    if hdlr.is_null() || (*hdlr).initialized != 0 {
        return;
    }

    xml_sax_version(hdlr, XML_SAX2_DEFAULT_VERSION_VALUE);
    if warning == 0 {
        (*hdlr).warning = None;
    } else {
        (*hdlr).warning = Some(parser_warning);
    }
}

/// Initialize the default HTML SAX2 handler
#[doc(alias = "xmlSAX2InitHtmlDefaultSAXHandler")]
#[cfg(feature = "html")]
pub unsafe extern "C" fn xml_sax2_init_html_default_sax_handler(hdlr: *mut XmlSAXHandler) {
    if hdlr.is_null() || (*hdlr).initialized != 0 {
        return;
    }

    (*hdlr).internal_subset = Some(xml_sax2_internal_subset);
    (*hdlr).external_subset = None;
    (*hdlr).is_standalone = None;
    (*hdlr).has_internal_subset = None;
    (*hdlr).has_external_subset = None;
    (*hdlr).resolve_entity = None;
    (*hdlr).get_entity = Some(xml_sax2_get_entity);
    (*hdlr).get_parameter_entity = None;
    (*hdlr).entity_decl = None;
    (*hdlr).attribute_decl = None;
    (*hdlr).element_decl = None;
    (*hdlr).notation_decl = None;
    (*hdlr).unparsed_entity_decl = None;
    (*hdlr).set_document_locator = Some(xml_sax2_set_document_locator);
    (*hdlr).start_document = Some(xml_sax2_start_document);
    (*hdlr).end_document = Some(xml_sax2_end_document);
    (*hdlr).start_element = Some(xml_sax2_start_element);
    (*hdlr).end_element = Some(xml_sax2_end_element);
    (*hdlr).reference = None;
    (*hdlr).characters = Some(xml_sax2_characters);
    (*hdlr).cdata_block = Some(xml_sax2_cdata_block);
    (*hdlr).ignorable_whitespace = Some(xml_sax2_ignorable_whitespace);
    (*hdlr).processing_instruction = Some(xml_sax2_processing_instruction);
    (*hdlr).comment = Some(xml_sax2_comment);
    (*hdlr).warning = Some(parser_warning);
    (*hdlr).error = Some(parser_error);
    (*hdlr).fatal_error = Some(parser_error);

    (*hdlr).initialized = 1;
}

#[doc(alias = "htmlDefaultSAXHandlerInit")]
#[deprecated = "This function is a no-op. Call xmlInitParser to initialize the library"]
#[cfg(feature = "html")]
pub unsafe extern "C" fn html_default_sax_handler_init() {}

/// Initialize the default SAX2 handler
#[doc(alias = "xmlDefaultSAXHandlerInit")]
#[deprecated = "This function is a no-op. Call xmlInitParser to initialize the library"]
pub unsafe extern "C" fn xml_default_sax_handler_init() {}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_html_default_saxhandler_init() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;
            #[cfg(feature = "html")]
            {
                let mem_base = xml_mem_blocks();

                html_default_sax_handler_init();
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprintln!(
                        "Leak of {} blocks found in htmlDefaultSAXHandlerInit",
                        xml_mem_blocks() - mem_base
                    );
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in htmlDefaultSAXHandlerInit()"
            );
        }
    }

    #[test]
    fn test_xml_default_saxhandler_init() {
        unsafe {
            let mut leaks = 0;
            let mem_base = xml_mem_blocks();

            xml_default_sax_handler_init();
            reset_last_error();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in xmlDefaultSAXHandlerInit",
                    xml_mem_blocks() - mem_base
                );
                assert!(
                    leaks == 0,
                    "{leaks} Leaks are found in xmlDefaultSAXHandlerInit()"
                );
            }
        }
    }

    #[test]
    fn test_xml_sax2_get_column_number() {
        let mut leaks = 0;

        unsafe {
            for n_ctx in 0..GEN_NB_VOID_PTR {
                let mem_base = xml_mem_blocks();
                let ctx = gen_void_ptr(n_ctx, 0);

                let ret_val = xml_sax2_get_column_number(ctx);
                desret_int(ret_val);
                des_void_ptr(n_ctx, ctx, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlSAX2GetColumnNumber",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_ctx);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlSAX2GetColumnNumber()"
            );
        }
    }

    #[test]
    fn test_xml_sax2_get_line_number() {
        let mut leaks = 0;

        unsafe {
            for n_ctx in 0..GEN_NB_VOID_PTR {
                let mem_base = xml_mem_blocks();
                let ctx = gen_void_ptr(n_ctx, 0);

                let ret_val = xml_sax2_get_line_number(ctx);
                desret_int(ret_val);
                des_void_ptr(n_ctx, ctx, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlSAX2GetLineNumber",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_ctx);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlSAX2GetLineNumber()"
            );
        }
    }

    #[test]
    fn test_xml_sax2_get_public_id() {
        let mut leaks = 0;

        unsafe {
            for n_ctx in 0..GEN_NB_VOID_PTR {
                let mem_base = xml_mem_blocks();
                let ctx = gen_void_ptr(n_ctx, 0);

                let ret_val = xml_sax2_get_public_id(ctx);
                desret_const_xml_char_ptr(ret_val);
                des_void_ptr(n_ctx, ctx, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlSAX2GetPublicId",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_ctx);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlSAX2GetPublicId()"
            );
        }
    }

    #[test]
    fn test_xml_sax2_init_default_saxhandler() {
        let mut leaks = 0;

        unsafe {
            for n_hdlr in 0..GEN_NB_XML_SAXHANDLER_PTR {
                for n_warning in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let hdlr = gen_xml_saxhandler_ptr(n_hdlr, 0);
                    let warning = gen_int(n_warning, 1);

                    xml_sax2_init_default_sax_handler(hdlr, warning);
                    des_xml_saxhandler_ptr(n_hdlr, hdlr, 0);
                    des_int(n_warning, warning, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlSAX2InitDefaultSAXHandler",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_hdlr);
                        eprintln!(" {}", n_warning);
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlSAX2InitDefaultSAXHandler()"
            );
        }
    }

    #[test]
    fn test_xml_sax2_init_html_default_saxhandler() {
        #[cfg(feature = "html")]
        let mut leaks = 0;

        unsafe {
            for n_hdlr in 0..GEN_NB_XML_SAXHANDLER_PTR {
                let mem_base = xml_mem_blocks();
                let hdlr = gen_xml_saxhandler_ptr(n_hdlr, 0);

                xml_sax2_init_html_default_sax_handler(hdlr);
                des_xml_saxhandler_ptr(n_hdlr, hdlr, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlSAX2InitHtmlDefaultSAXHandler",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlSAX2InitHtmlDefaultSAXHandler()"
                    );
                    eprintln!(" {}", n_hdlr);
                }
            }
        }
    }

    #[test]
    fn test_xml_saxdefault_version() {
        #[cfg(feature = "sax1")]
        unsafe {
            let mut leaks = 0;
            #[cfg(feature = "sax1")]
            {
                for n_version in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let version = gen_int(n_version, 0);

                    let ret_val = xml_sax_default_version(version);
                    desret_int(ret_val);
                    des_int(n_version, version, 0);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlSAXDefaultVersion",
                            xml_mem_blocks() - mem_base
                        );
                        eprintln!(" {}", n_version);
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlSAXDefaultVersion()"
            );
        }
    }

    #[test]
    fn test_xml_saxversion() {
        let mut leaks = 0;

        unsafe {
            for n_hdlr in 0..GEN_NB_XML_SAXHANDLER_PTR {
                for n_version in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let hdlr = gen_xml_saxhandler_ptr(n_hdlr, 0);
                    let version = gen_int(n_version, 1);

                    let ret_val = xml_sax_version(hdlr, version);
                    desret_int(ret_val);
                    des_xml_saxhandler_ptr(n_hdlr, hdlr, 0);
                    des_int(n_version, version, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlSAXVersion",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_hdlr);
                        eprintln!(" {}", n_version);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlSAXVersion()");
        }
    }
}
