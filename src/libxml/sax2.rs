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
    ffi::{c_void, CStr, CString},
    mem::{replace, size_of},
    ptr::{addr_of_mut, null, null_mut},
    slice::from_raw_parts,
    sync::atomic::Ordering,
};

use libc::memcpy;

use crate::{
    encoding::{detect_encoding, XmlCharEncoding},
    error::{__xml_raise_error, parser_error, parser_warning, XmlParserErrors},
    globals::{GenericErrorContext, StructuredError},
    libxml::{
        htmltree::html_new_doc_no_dtd,
        parser::{XmlParserInputState, XmlSAXLocatorPtr},
        valid::{
            xml_validate_attribute_decl, xml_validate_document_final, xml_validate_one_element,
        },
        xmlstring::XmlChar,
    },
    parser::{
        build_qname, split_qname, xml_err_memory, xml_free_input_stream, XmlParserCtxtPtr,
        XmlParserInputPtr,
    },
    tree::{
        xml_add_doc_entity, xml_add_dtd_entity, xml_build_qname, xml_create_int_subset,
        xml_free_dtd, xml_free_node, xml_get_doc_entity, xml_get_parameter_entity,
        xml_get_predefined_entity, xml_new_cdata_block, xml_new_char_ref, xml_new_doc,
        xml_new_doc_comment, xml_new_doc_node, xml_new_doc_pi, xml_new_doc_text, xml_new_dtd,
        xml_new_ns, xml_new_ns_prop, xml_new_reference, xml_text_concat, xml_validate_ncname,
        NodeCommon, NodePtr, XmlAttr, XmlAttrPtr, XmlAttributeDefault, XmlAttributeType, XmlDoc,
        XmlDocProperties, XmlElementContentPtr, XmlElementType, XmlElementTypeVal, XmlEntityPtr,
        XmlEntityType, XmlEnumeration, XmlNode, XmlNsPtr, __XML_REGISTER_CALLBACKS,
    },
    uri::{build_uri, canonic_path, path_to_uri},
};

use super::{
    dict::xml_dict_owns,
    globals::{xml_free, xml_malloc, xml_realloc, xml_register_node_default_value},
    parser::{
        xml_load_external_entity, XmlParserOption, XmlSAXHandler, XML_COMPLETE_ATTRS,
        XML_SAX2_MAGIC, XML_SKIP_IDS,
    },
    parser_internals::{
        xml_parse_external_subset, xml_string_decode_entities, xml_string_len_decode_entities,
        XML_MAX_TEXT_LENGTH, XML_STRING_TEXT, XML_SUBSTITUTE_REF, XML_VCTXT_DTD_VALIDATED,
    },
    uri::{xml_free_uri, xml_parse_uri, XmlURIPtr},
    valid::{
        xml_add_attribute_decl, xml_add_element_decl, xml_add_id, xml_add_notation_decl,
        xml_add_ref, xml_get_dtd_qelement_desc, xml_is_id, xml_is_ref,
        xml_valid_ctxt_normalize_attribute_value, xml_valid_normalize_attribute_value,
        xml_validate_dtd_final, xml_validate_element_decl, xml_validate_notation_decl,
        xml_validate_one_attribute, xml_validate_one_namespace, xml_validate_root,
    },
    xmlstring::{xml_strcat, xml_strdup, xml_strlen, xml_strndup},
};

/// Provides the public ID e.g. "-//SGMLSOURCE//DTD DEMO//EN"
///
/// Returns a xmlChar *
#[doc(alias = "xmlSAX2GetPublicId")]
pub unsafe fn xml_sax2_get_public_id(_ctx: *mut c_void) -> *const XmlChar {
    /* let ctxt: xmlParserCtxtPtr = ctx as xmlParserCtxtPtr; */
    null()
}

/// Provides the system ID, basically URL or filename e.g.  
/// <http://www.sgmlsource.com/dtds/memo.dtd>
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
pub unsafe fn xml_sax2_get_line_number(ctx: *mut c_void) -> i32 {
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
pub unsafe fn xml_sax2_get_column_number(ctx: *mut c_void) -> i32 {
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
    (*(*ctxt).my_doc).int_subset.is_some() as i32
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
    (*(*ctxt).my_doc).ext_subset.is_some() as i32
}

#[doc(alias = "xmlSAX2ErrMemory")]
unsafe fn xml_sax2_err_memory(ctxt: XmlParserCtxtPtr, msg: &str) {
    let mut schannel: Option<StructuredError> = None;
    const MSG: &str = "out of memory\n";

    if !ctxt.is_null() {
        (*ctxt).err_no = XmlParserErrors::XmlErrNoMemory as i32;
        if let Some(sax) = (*ctxt)
            .sax
            .as_deref()
            .filter(|sax| sax.initialized == XML_SAX2_MAGIC as u32)
        {
            schannel = sax.serror;
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
            None,
            0,
            Some(MSG.into()),
            None,
            None,
            0,
            0,
            msg,
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
            None,
            0,
            Some(MSG.into()),
            None,
            None,
            0,
            0,
            msg,
        );
    }
}

/// Callback on internal subset declaration.
#[doc(alias = "xmlSAX2InternalSubset")]
pub unsafe fn xml_sax2_internal_subset(
    ctx: Option<GenericErrorContext>,
    name: Option<&str>,
    external_id: Option<&str>,
    system_id: Option<&str>,
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
    let dtd = (*(*ctxt).my_doc).get_int_subset();
    if let Some(mut dtd) = dtd {
        if (*ctxt).html != 0 {
            return;
        }
        (*dtd).unlink();
        xml_free_dtd(dtd);
        (*(*ctxt).my_doc).int_subset = None;
    }
    (*(*ctxt).my_doc).int_subset =
        xml_create_int_subset((*ctxt).my_doc, name, external_id, system_id);
    if (*(*ctxt).my_doc).int_subset.is_none() {
        xml_sax2_err_memory(ctxt, "xmlSAX2InternalSubset");
    }
}

/// Callback on external subset declaration.
#[doc(alias = "xmlSAX2ExternalSubset")]
pub unsafe fn xml_sax2_external_subset(
    ctx: Option<GenericErrorContext>,
    name: Option<&str>,
    external_id: Option<&str>,
    system_id: Option<&str>,
) {
    if ctx.is_none() {
        return;
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    if (external_id.is_some() || system_id.is_some())
        && (((*ctxt).validate != 0 || (*ctxt).loadsubset != 0)
            && ((*ctxt).well_formed != 0 && !(*ctxt).my_doc.is_null()))
    {
        let mut input: XmlParserInputPtr = null_mut();
        let mut consumed: u64;

        // Ask the Entity resolver to load the damn thing
        if let Some(resolve_entity) = (*ctxt)
            .sax
            .as_deref_mut()
            .and_then(|sax| sax.resolve_entity)
        {
            input = resolve_entity((*ctxt).user_data.clone(), external_id, system_id);
        }
        if input.is_null() {
            return;
        }

        xml_new_dtd((*ctxt).my_doc, name, external_id, system_id);

        // make sure we won't destroy the main document context

        // Try to fetch and parse the external subset.
        let oldinput: XmlParserInputPtr = (*ctxt).input;
        let oldinput_tab = replace(&mut (*ctxt).input_tab, Vec::with_capacity(5));
        let oldcharset = (*ctxt).charset;
        let oldencoding = (*ctxt).encoding.take();
        let oldprogressive: i32 = (*ctxt).progressive;
        (*ctxt).progressive = 0;
        (*ctxt).input = null_mut();
        (*ctxt).push_input(input);

        // On the fly encoding conversion if needed
        if (*(*ctxt).input).length >= 4 {
            let input = from_raw_parts((*(*ctxt).input).cur, 4);
            let enc = detect_encoding(input);
            (*ctxt).switch_encoding(enc);
        }

        if (*input).filename.is_none() {
            if let Some(system_id) = system_id {
                let canonic = canonic_path(system_id);
                (*input).filename = Some(canonic.into_owned());
            }
        }
        (*input).line = 1;
        (*input).col = 1;
        (*input).base = (*(*ctxt).input).cur;
        (*input).cur = (*(*ctxt).input).cur;
        (*input).free = None;

        // let's parse that entity knowing it's an external subset.
        xml_parse_external_subset(ctxt, external_id, system_id);

        // Free up the external entities
        while (*ctxt).input_tab.len() > 1 {
            (*ctxt).pop_input();
        }

        consumed = (*(*ctxt).input).consumed;
        let buffered = (*(*ctxt).input).offset_from_base();
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

        // Restore the parsing context of the main entity
        (*ctxt).input = oldinput;
        (*ctxt).input_tab = oldinput_tab;
        (*ctxt).charset = oldcharset;
        (*ctxt).encoding = oldencoding;
        (*ctxt).progressive = oldprogressive;
        // (*ctxt).wellFormed = oldwellFormed;
    }
}

/// Handle a fatal parser error, i.e. violating Well-Formedness constraints
#[doc(alias = "xmlFatalErrMsg")]
macro_rules! xml_fatal_err_msg {
    ($ctxt:expr, $error:expr, $msg:literal, $str1:expr) => {
        let msg = format!($msg, $str1);
        xml_fatal_err_msg!(
            @inner,
            $ctxt,
            $error,
            &msg,
            Some($str1.to_owned().into()),
            None
        );
    };
    ($ctxt:expr, $error:expr, $msg:literal, $str1:expr, $str2:expr) => {
        let msg = format!($msg, $str1, $str2);
        xml_fatal_err_msg!(
            @inner,
            $ctxt,
            $error,
            &msg,
            Some($str1.to_owned().into()),
            Some($str2.to_owned().into())
        );
    };
    (@inner, $ctxt:expr, $error:expr, $msg:expr, $str1:expr, $str2:expr) => {
        let ctxt = $ctxt as *mut $crate::parser::XmlParserCtxt;
        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = $error as i32;
            }
            __xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                null_mut(),
                XmlErrorDomain::XmlFromParser,
                $error,
                XmlErrorLevel::XmlErrFatal,
                None,
                0,
                $str1,
                $str2,
                None,
                0,
                0,
                $msg,
            );
            if !ctxt.is_null() {
                (*ctxt).well_formed = 0;
                (*ctxt).valid = 0;
                if (*ctxt).recovery == 0 {
                    (*ctxt).disable_sax = 1;
                }
            }
        }
    };
}

/// Get an entity by name
///
/// Returns the xmlEntityPtr if found.
#[doc(alias = "xmlSAX2GetEntity")]
pub unsafe fn xml_sax2_get_entity(
    ctx: Option<GenericErrorContext>,
    name: &str,
) -> Option<XmlEntityPtr> {
    let ctxt = {
        let ctx = ctx?;
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };

    if (*ctxt).in_subset == 0 {
        let ret = xml_get_predefined_entity(name);
        if ret.is_some() {
            return ret;
        }
    }
    if !(*ctxt).my_doc.is_null() && (*(*ctxt).my_doc).standalone == 1 {
        if (*ctxt).in_subset == 2 {
            (*(*ctxt).my_doc).standalone = 0;
            let ret = xml_get_doc_entity((*ctxt).my_doc, name);
            (*(*ctxt).my_doc).standalone = 1;
            ret
        } else {
            let mut ret = xml_get_doc_entity((*ctxt).my_doc, name);
            if ret.is_none() {
                (*(*ctxt).my_doc).standalone = 0;
                ret = xml_get_doc_entity((*ctxt).my_doc, name);
                if ret.is_some() {
                    xml_fatal_err_msg!(
                        ctxt,
                        XmlParserErrors::XmlErrNotStandalone,
                        "Entity({}) document marked standalone but requires external subset\n",
                        name
                    );
                }
                (*(*ctxt).my_doc).standalone = 1;
            }
            ret
        }
    } else {
        xml_get_doc_entity((*ctxt).my_doc, name)
    }
}

/// Get a parameter entity by name
///
/// Returns the xmlEntityPtr if found.
#[doc(alias = "xmlSAX2GetParameterEntity")]
pub unsafe fn xml_sax2_get_parameter_entity(
    ctx: Option<GenericErrorContext>,
    name: &str,
) -> Option<XmlEntityPtr> {
    let ctxt = {
        let ctx = ctx?;
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
    public_id: Option<&str>,
    system_id: Option<&str>,
) -> XmlParserInputPtr {
    if ctx.is_none() {
        return null_mut();
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    let base = if !(*ctxt).input.is_null() {
        (*(*ctxt).input)
            .filename
            .as_deref()
            .or((*ctxt).directory.as_deref())
            .map(|b| b.to_owned())
    } else {
        (*ctxt).directory.as_deref().map(|d| d.to_owned())
    };

    let uri = system_id.zip(base).and_then(|(s, b)| build_uri(s, &b));
    xml_load_external_entity(uri.as_deref(), public_id, ctxt)
}

/// Handle a parser warning
#[doc(alias = "xmlWarnMsg")]
macro_rules! xml_warn_msg {
    ($ctxt:expr, $error:expr, $msg:literal, $str1:expr) => {
        let ctxt = $ctxt as *mut $crate::parser::XmlParserCtxt;
        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = $error as i32;
            }
            __xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                null_mut(),
                XmlErrorDomain::XmlFromParser,
                $error,
                XmlErrorLevel::XmlErrWarning,
                None,
                0,
                Some($str1.to_owned().into()),
                None,
                None,
                0,
                0,
                format!($msg, $str1).as_str(),
            );
        }
    };
}

/// An entity definition has been parsed
#[doc(alias = "xmlSAX2EntityDecl")]
pub unsafe fn xml_sax2_entity_decl(
    ctx: Option<GenericErrorContext>,
    name: &str,
    typ: XmlEntityType,
    public_id: Option<&str>,
    system_id: Option<&str>,
    content: Option<&str>,
) {
    if ctx.is_none() {
        return;
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    if (*ctxt).in_subset == 1 {
        let ent = xml_add_doc_entity((*ctxt).my_doc, name, typ, public_id, system_id, content);
        if ent.is_none() && (*ctxt).pedantic != 0 {
            xml_warn_msg!(
                ctxt,
                XmlParserErrors::XmlWarEntityRedefined,
                "Entity({}) already defined in the internal subset\n",
                name
            );
        }
        if let Some(ent) = ent.filter(|ent| ent.uri.load(Ordering::Relaxed).is_null()) {
            if let Some(system_id) = system_id {
                let base = if !(*ctxt).input.is_null() {
                    (*(*ctxt).input)
                        .filename
                        .as_deref()
                        .or((*ctxt).directory.as_deref())
                        .map(|b| b.to_owned())
                } else {
                    (*ctxt).directory.clone()
                };

                if let Some(uri) = base.and_then(|b| build_uri(system_id, &b)) {
                    let uri = CString::new(uri).unwrap();
                    ent.uri
                        .store(xml_strdup(uri.as_ptr() as *const u8), Ordering::Relaxed);
                } else {
                    ent.uri.store(null_mut(), Ordering::Relaxed);
                }
            }
        }
    } else if (*ctxt).in_subset == 2 {
        let ent = xml_add_dtd_entity((*ctxt).my_doc, name, typ, public_id, system_id, content);
        if ent.is_none() && (*ctxt).pedantic != 0 {
            if let Some(warning) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.warning) {
                warning(
                    (*ctxt).user_data.clone(),
                    format!("Entity({name}) already defined in the external subset\n",).as_str(),
                );
            }
        }
        if let Some(ent) = ent.filter(|ent| ent.uri.load(Ordering::Relaxed).is_null()) {
            if let Some(system_id) = system_id {
                let base = if !(*ctxt).input.is_null() {
                    (*(*ctxt).input)
                        .filename
                        .as_deref()
                        .or((*ctxt).directory.as_deref())
                        .map(|b| b.to_owned())
                } else {
                    (*ctxt).directory.clone()
                };

                if let Some(uri) = base.and_then(|b| build_uri(system_id, &b)) {
                    let uri = CString::new(uri).unwrap();
                    ent.uri
                        .store(xml_strdup(uri.as_ptr() as *const u8), Ordering::Relaxed);
                } else {
                    ent.uri.store(null_mut(), Ordering::Relaxed);
                }
            }
        }
    } else {
        xml_fatal_err_msg!(
            ctxt,
            XmlParserErrors::XmlErrEntityProcessing,
            "SAX.xmlSAX2EntityDecl({}) called while not in subset\n",
            name
        );
    }
}

/// Handle a validation error
#[doc(alias = "xmlValidError")]
macro_rules! xml_err_valid {
    ($ctxt:expr, $error:expr, $msg:literal) => {
        xml_err_valid!(@inner $ctxt, $error, $msg, None, None);
    };
    ($ctxt:expr, $error:expr, $msg:literal, $str1:expr) => {
        let msg = format!($msg, $str1);
        xml_err_valid!(@inner $ctxt, $error, &msg, Some($str1.to_owned().into()), None);
    };
    ($ctxt:expr, $error:expr, $msg:literal, $str1:expr, $str2:expr) => {
        let msg = format!($msg, $str1, $str2);
        xml_err_valid!(@inner $ctxt, $error, &msg, Some($str1.to_owned().into()), Some($str2.to_owned().into()));
    };
    (@inner $ctxt:expr, $error:expr, $msg:expr, $str1:expr, $str2:expr) => {
        let ctxt = $ctxt as *mut $crate::parser::XmlParserCtxt;
        let mut schannel: Option<StructuredError> = None;

        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = $error as i32;
                if let Some(sax) = (*ctxt).sax.as_deref().filter(|sax| sax.initialized == XML_SAX2_MAGIC as u32) {
                    schannel = sax.serror;
                }
                __xml_raise_error!(
                    schannel,
                    (*ctxt).vctxt.error,
                    (*ctxt).vctxt.user_data.clone(),
                    ctxt as _,
                    null_mut(),
                    XmlErrorDomain::XmlFromDTD,
                    $error,
                    XmlErrorLevel::XmlErrError,
                    None,
                    0,
                    $str1,
                    $str2,
                    None,
                    0,
                    0,
                    $msg,
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
                    $error,
                    XmlErrorLevel::XmlErrError,
                    None,
                    0,
                    $str1,
                    $str2,
                    None,
                    0,
                    0,
                    $msg,
                );
            }
        }
    };
}

/// An attribute definition has been parsed
#[doc(alias = "xmlSAX2AttributeDecl")]
pub unsafe fn xml_sax2_attribute_decl(
    ctx: Option<GenericErrorContext>,
    elem: &str,
    fullname: &str,
    typ: XmlAttributeType,
    def: XmlAttributeDefault,
    default_value: Option<&str>,
    tree: Option<Box<XmlEnumeration>>,
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

    if fullname == "xml:id" && typ != XmlAttributeType::XmlAttributeID {
        // Raise the error but keep the validity flag
        let tmp: i32 = (*ctxt).valid;
        xml_err_valid!(
            ctxt,
            XmlParserErrors::XmlDTDXmlidType,
            "xml:id : attribute type should be ID\n"
        );
        (*ctxt).valid = tmp;
    }
    // TODO: optimize name/prefix allocation
    let (prefix, name) = split_qname(&mut *ctxt, fullname);
    (*ctxt).vctxt.valid = 1;
    let attr = if (*ctxt).in_subset == 1 {
        xml_add_attribute_decl(
            &raw mut (*ctxt).vctxt as _,
            (*(*ctxt).my_doc).int_subset,
            elem,
            name,
            prefix,
            typ,
            def,
            default_value,
            tree,
        )
    } else if (*ctxt).in_subset == 2 {
        xml_add_attribute_decl(
            &raw mut (*ctxt).vctxt as _,
            (*(*ctxt).my_doc).ext_subset,
            elem,
            name,
            prefix,
            typ,
            def,
            default_value,
            tree,
        )
    } else {
        xml_fatal_err_msg!(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            "SAX.xmlSAX2AttributeDecl({}) called while not in subset\n",
            name
        );
        return;
    };
    #[cfg(feature = "libxml_valid")]
    {
        if (*ctxt).vctxt.valid == 0 {
            (*ctxt).valid = 0;
        }
        if (*ctxt).validate != 0
            && (*ctxt).well_formed != 0
            && (*(*ctxt).my_doc).int_subset.is_some()
        {
            if let Some(attr) = attr {
                (*ctxt).valid &= xml_validate_attribute_decl(
                    addr_of_mut!((*ctxt).vctxt) as _,
                    (*ctxt).my_doc,
                    attr,
                );
            }
        }
    }
}

/// An element definition has been parsed
#[doc(alias = "xmlSAX2ElementDecl")]
pub unsafe fn xml_sax2_element_decl(
    ctx: Option<GenericErrorContext>,
    name: &str,
    typ: Option<XmlElementTypeVal>,
    content: XmlElementContentPtr,
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

    let elem = if (*ctxt).in_subset == 1 {
        xml_add_element_decl(
            addr_of_mut!((*ctxt).vctxt) as _,
            (*(*ctxt).my_doc).int_subset,
            name,
            typ,
            content,
        )
    } else if (*ctxt).in_subset == 2 {
        xml_add_element_decl(
            addr_of_mut!((*ctxt).vctxt) as _,
            (*(*ctxt).my_doc).ext_subset,
            name,
            typ,
            content,
        )
    } else {
        xml_fatal_err_msg!(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            "SAX.xmlSAX2ElementDecl({}) called while not in subset\n",
            name
        );
        return;
    };
    #[cfg(feature = "libxml_valid")]
    {
        if elem.is_none() {
            (*ctxt).valid = 0;
        }
        if (*ctxt).validate != 0
            && (*ctxt).well_formed != 0
            && !(*ctxt).my_doc.is_null()
            && (*(*ctxt).my_doc).int_subset.is_some()
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
    name: &str,
    public_id: Option<&str>,
    system_id: Option<&str>,
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

    let nota = if public_id.is_none() && system_id.is_none() {
        xml_fatal_err_msg!(
            ctxt,
            XmlParserErrors::XmlErrNotationProcessing,
            "SAX.xmlSAX2NotationDecl({}) externalID or PublicID missing\n",
            name
        );
        return;
    } else if (*ctxt).in_subset == 1 {
        xml_add_notation_decl(
            addr_of_mut!((*ctxt).vctxt) as _,
            (*(*ctxt).my_doc).int_subset.as_deref_mut(),
            name,
            public_id,
            system_id,
        )
    } else if (*ctxt).in_subset == 2 {
        xml_add_notation_decl(
            addr_of_mut!((*ctxt).vctxt) as _,
            (*(*ctxt).my_doc).ext_subset.as_deref_mut(),
            name,
            public_id,
            system_id,
        )
    } else {
        xml_fatal_err_msg!(
            ctxt,
            XmlParserErrors::XmlErrNotationProcessing,
            "SAX.xmlSAX2NotationDecl({}) called while not in subset\n",
            name
        );
        return;
    };
    #[cfg(feature = "libxml_valid")]
    {
        if nota.is_none() {
            (*ctxt).valid = 0;
        }
        if (*ctxt).validate != 0
            && (*ctxt).well_formed != 0
            && (*(*ctxt).my_doc).int_subset.is_some()
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
    name: &str,
    public_id: Option<&str>,
    system_id: Option<&str>,
    notation_name: Option<&str>,
) {
    if ctx.is_none() {
        return;
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };

    if (*ctxt).in_subset == 1 {
        let ent = xml_add_doc_entity(
            (*ctxt).my_doc,
            name,
            XmlEntityType::XmlExternalGeneralUnparsedEntity,
            public_id,
            system_id,
            notation_name,
        );
        if ent.is_none() && (*ctxt).pedantic != 0 {
            if let Some(warning) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.warning) {
                warning(
                    (*ctxt).user_data.clone(),
                    format!("Entity({name}) already defined in the internal subset\n").as_str(),
                )
            }
        }
        if let Some(ent) = ent.filter(|ent| ent.uri.load(Ordering::Relaxed).is_null()) {
            if let Some(system_id) = system_id {
                let base = if !(*ctxt).input.is_null() {
                    (*(*ctxt).input)
                        .filename
                        .as_deref()
                        .or((*ctxt).directory.as_deref())
                        .map(|b| b.to_owned())
                } else {
                    (*ctxt).directory.clone()
                };

                if let Some(uri) = base.and_then(|b| build_uri(system_id, &b)) {
                    let uri = CString::new(uri).unwrap();
                    ent.uri
                        .store(xml_strdup(uri.as_ptr() as *const u8), Ordering::Relaxed);
                } else {
                    ent.uri.store(null_mut(), Ordering::Relaxed);
                }
            }
        }
    } else if (*ctxt).in_subset == 2 {
        let ent = xml_add_dtd_entity(
            (*ctxt).my_doc,
            name,
            XmlEntityType::XmlExternalGeneralUnparsedEntity,
            public_id,
            system_id,
            notation_name,
        );
        if ent.is_none() && (*ctxt).pedantic != 0 {
            if let Some(warning) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.warning) {
                warning(
                    (*ctxt).user_data.clone(),
                    format!("Entity({name}) already defined in the external subset\n").as_str(),
                )
            }
        }

        if let Some(ent) = ent.filter(|ent| ent.uri.load(Ordering::Relaxed).is_null()) {
            if let Some(system_id) = system_id {
                let base = if !(*ctxt).input.is_null() {
                    (*(*ctxt).input)
                        .filename
                        .as_deref()
                        .or((*ctxt).directory.as_deref())
                        .map(|b| b.to_owned())
                } else {
                    (*ctxt).directory.clone()
                };

                if let Some(uri) = base.and_then(|b| build_uri(system_id, &b)) {
                    let uri = CString::new(uri).unwrap();
                    ent.uri
                        .store(xml_strdup(uri.as_ptr() as *const u8), Ordering::Relaxed);
                } else {
                    ent.uri.store(null_mut(), Ordering::Relaxed);
                }
            }
        }
    } else {
        xml_fatal_err_msg!(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            "SAX.xmlSAX2UnparsedEntityDecl({}) called while not in subset\n",
            name
        );
    }
}

/// called when the document start being processed.
#[doc(alias = "xmlSAX2StartDocument")]
pub unsafe fn xml_sax2_start_document(ctx: Option<GenericErrorContext>) {
    let doc: *mut XmlDoc;

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
                xml_sax2_err_memory(ctxt, "xmlSAX2StartDocument");
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
            xml_sax2_err_memory(ctxt, "xmlSAX2StartDocument");
            return;
        }
    }
    if !(*ctxt).my_doc.is_null() && (*(*ctxt).my_doc).url.is_none() && !(*ctxt).input.is_null() {
        if let Some(filename) = (*(*ctxt).input).filename.as_deref() {
            let url = path_to_uri(filename);
            (*(*ctxt).my_doc).url = Some(url.into_owned());
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
            && (*(*ctxt).my_doc).int_subset.is_some()
        {
            (*ctxt).valid &=
                xml_validate_document_final(addr_of_mut!((*ctxt).vctxt) as _, (*ctxt).my_doc);
        }
    }

    // Grab the encoding if it was added on-the-fly
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
macro_rules! xml_ns_warn_msg {
    ($ctxt:expr, $error:expr, $msg:literal) => {
        xml_ns_warn_msg!(
            @inner,
            $ctxt,
            $error,
            $msg,
            None,
            None
        );
    };
    ($ctxt:expr, $error:expr, $msg:literal, $str1:expr) => {
        let msg = format!($msg, $str1);
        xml_ns_warn_msg!(
            @inner,
            $ctxt,
            $error,
            &msg,
            Some($str1.to_owned().into()),
            None
        );
    };
    ($ctxt:expr, $error:expr, $msg:literal, $str1:expr, $str2:expr) => {
        let msg = format!($msg, $str1, $str2);
        xml_ns_warn_msg!(
            @inner,
            $ctxt,
            $error,
            &msg,
            Some($str1.to_owned().into()),
            Some($str2.to_owned().into())
        );
    };
    (@inner, $ctxt:expr, $error:expr, $msg:expr, $str1:expr, $str2:expr) => {
        let ctxt = $ctxt as *mut $crate::parser::XmlParserCtxt;
        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = $error as i32;
            }
            __xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                null_mut(),
                XmlErrorDomain::XmlFromNamespace,
                $error,
                XmlErrorLevel::XmlErrWarning,
                None,
                0,
                $str1,
                $str2,
                None,
                0,
                0,
                $msg,
            );
        }
    };
}

/// Handle a namespace error
#[doc(alias = "xmlNsErrMsg")]
#[cfg(any(feature = "sax1", feature = "html", feature = "libxml_writer",))]
macro_rules! xml_ns_err_msg {
    ($ctxt:expr, $error:expr, $msg:literal) => {
        xml_ns_err_msg!(
            @inner,
            $ctxt,
            $error,
            $msg,
            None,
            None
        );
    };
    ($ctxt:expr, $error:expr, $msg:literal, $str1:expr) => {
        let msg = format!($msg, $str1);
        xml_ns_err_msg!(
            @inner,
            $ctxt,
            $error,
            &msg,
            Some($str1.to_owned().into()),
            None
        );
    };
    ($ctxt:expr, $error:expr, $msg:literal, $str1:expr, $str2:expr) => {
        let msg = format!($msg, $str1, $str2);
        xml_ns_err_msg!(
            @inner,
            $ctxt,
            $error,
            &msg,
            Some($str1.to_owned().into()),
            Some($str2.to_owned().into())
        );
    };
    (@inner, $ctxt:expr, $error:expr, $msg:expr, $str1:expr, $str2:expr) => {
        let ctxt = $ctxt as *mut $crate::parser::XmlParserCtxt;
        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = $error as i32;
            }
            __xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                null_mut(),
                XmlErrorDomain::XmlFromNamespace,
                $error,
                XmlErrorLevel::XmlErrError,
                None,
                0,
                $str1,
                $str2,
                None,
                0,
                0,
                $msg,
            );
        }
    };
}

/// Handle an attribute that has been read by the parser.
/// The default handling is to convert the attribute into an
/// DOM subtree and past it in a new xmlAttr element added to
/// the element.
#[doc(alias = "xmlSAX2AttributeInternal")]
#[cfg(any(feature = "sax1", feature = "html", feature = "libxml_writer",))]
unsafe fn xml_sax2_attribute_internal(
    ctx: Option<GenericErrorContext>,
    fullname: &str,
    value: Option<&str>,
    prefix: Option<&str>,
) {
    use super::htmltree::html_is_boolean_attr;

    let nval: *mut XmlChar;
    let value = value.map(|v| CString::new(v).unwrap());
    let mut value = value.as_deref().map_or(null(), |v| v.as_ptr() as *const u8);

    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };

    let cfullname = CString::new(fullname).unwrap();
    let (ns, name) = if (*ctxt).html != 0 {
        (None, fullname)
    } else {
        // Split the full name into a namespace prefix and the tag name
        let (prefix, mut local) = split_qname(&mut *ctxt, fullname);
        let mut ns = prefix;
        if let Some(prefix) = prefix.filter(|_| local.is_empty()) {
            if prefix == "xmlns" {
                xml_ns_err_msg!(
                    ctxt,
                    XmlParserErrors::XmlErrNsDeclError,
                    "invalid namespace declaration '{}'\n",
                    fullname
                );
            } else {
                xml_ns_warn_msg!(
                    ctxt,
                    XmlParserErrors::XmlWarNsColumn,
                    "Avoid attribute ending with ':' like '{}'\n",
                    fullname
                );
            }
            ns = None;
            local = fullname;
        }
        (ns, local)
    };

    #[cfg(not(feature = "html"))]
    let f = false;
    #[cfg(feature = "html")]
    let f = (*ctxt).html != 0
        && value.is_null()
        && html_is_boolean_attr(cfullname.as_ptr() as *const u8) != 0;
    if f {
        nval = xml_strdup(cfullname.as_ptr() as *const u8);
        value = nval;
    } else {
        #[cfg(feature = "libxml_valid")]
        {
            // Do the last stage of the attribute normalization
            // Needed for HTML too:
            //   http://www.w3.org/TR/html4/types.html#h-6.2
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

    // Check whether it's a namespace definition
    if (*ctxt).html == 0 && ns.is_none() && name == "xmlns" {
        let val: *mut XmlChar;

        if (*ctxt).replace_entities == 0 {
            (*ctxt).depth += 1;
            val = xml_string_decode_entities(ctxt, value, XML_SUBSTITUTE_REF as _, 0, 0, 0);
            (*ctxt).depth -= 1;
            if val.is_null() {
                xml_sax2_err_memory(ctxt, "xmlSAX2StartElement");
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
                if let Some(warning) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.warning) {
                    warning(
                        (*ctxt).user_data.clone(),
                        format!(
                            "xmlns: {} not a valid URI\n",
                            CStr::from_ptr(val as *const i8).to_string_lossy()
                        )
                        .as_str(),
                    );
                }
            } else {
                if (*uri).scheme.is_null() {
                    if let Some(warning) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.warning) {
                        warning(
                            (*ctxt).user_data.clone(),
                            format!(
                                "xmlns: URI {} is not absolute\n",
                                CStr::from_ptr(val as *const i8).to_string_lossy()
                            )
                            .as_str(),
                        );
                    }
                }
                xml_free_uri(uri);
            }
        }

        // a default namespace definition
        let nsret = xml_new_ns((*ctxt).node, val, None);

        #[cfg(feature = "libxml_valid")]
        {
            // Validate also for namespace decls, they are attributes from an XML-1.0 perspective
            if nsret.is_some()
                && (*ctxt).validate != 0
                && (*ctxt).well_formed != 0
                && !(*ctxt).my_doc.is_null()
                && (*(*ctxt).my_doc).int_subset.is_some()
            {
                (*ctxt).valid &= xml_validate_one_namespace(
                    addr_of_mut!((*ctxt).vctxt) as _,
                    (*ctxt).my_doc,
                    (*ctxt).node,
                    prefix,
                    nsret.unwrap(),
                    val,
                );
            }
        }
        if !nval.is_null() {
            xml_free(nval as _);
        }
        if val != value as _ {
            xml_free(val as _);
        }
        return;
    }
    if (*ctxt).html != 0 && ns == Some("xmlns") {
        let val: *mut XmlChar;

        if !(*ctxt).replace_entities != 0 {
            (*ctxt).depth += 1;
            val = xml_string_decode_entities(ctxt, value, XML_SUBSTITUTE_REF as _, 0, 0, 0);
            (*ctxt).depth -= 1;
            if val.is_null() {
                xml_sax2_err_memory(ctxt, "xmlSAX2StartElement");
                if !nval.is_null() {
                    xml_free(nval as _);
                }
                return;
            }
        } else {
            val = value as _;
        }

        if *val.add(0) == 0 {
            xml_ns_err_msg!(
                ctxt,
                XmlParserErrors::XmlNsErrEmpty,
                "Empty namespace name for prefix {}\n",
                name
            );
        }
        if (*ctxt).pedantic != 0 && (*val.add(0) != 0) {
            let uri: XmlURIPtr = xml_parse_uri(val as _);
            if uri.is_null() {
                let value = CStr::from_ptr(value as *const i8).to_string_lossy();
                xml_ns_warn_msg!(
                    ctxt,
                    XmlParserErrors::XmlWarNsURI,
                    "xmlns:{}: {} not a valid URI\n",
                    name,
                    value
                );
            } else {
                if (*uri).scheme.is_null() {
                    let value = CStr::from_ptr(value as *const i8).to_string_lossy();
                    xml_ns_warn_msg!(
                        ctxt,
                        XmlParserErrors::XmlWarNsURIRelative,
                        "xmlns:{}: URI {} is not absolute\n",
                        name,
                        value
                    );
                }
                xml_free_uri(uri);
            }
        }

        // a standard namespace definition
        let nsret = xml_new_ns((*ctxt).node, val, Some(name));
        #[cfg(feature = "libxml_valid")]
        {
            // Validate also for namespace decls, they are attributes from an XML-1.0 perspective
            if nsret.is_some()
                && (*ctxt).validate != 0
                && (*ctxt).well_formed != 0
                && !(*ctxt).my_doc.is_null()
                && (*(*ctxt).my_doc).int_subset.is_some()
            {
                (*ctxt).valid &= xml_validate_one_namespace(
                    addr_of_mut!((*ctxt).vctxt) as _,
                    (*ctxt).my_doc,
                    (*ctxt).node,
                    prefix,
                    nsret.unwrap(),
                    value,
                );
            }
        }
        if !nval.is_null() {
            xml_free(nval as _);
        }
        if val != value as _ {
            xml_free(val as _);
        }
        return;
    }

    let namespace = if let Some(ns) = ns {
        let namespace = (*(*ctxt).node).search_ns((*ctxt).my_doc, Some(ns));

        if let Some(namespace) = namespace {
            let mut prop = (*(*ctxt).node).properties;
            while let Some(now) = prop {
                if Some(name) == now.name().as_deref()
                    && now
                        .ns
                        .map_or(false, |ns| namespace == ns || namespace.href() == ns.href())
                {
                    xml_ns_err_msg!(
                        ctxt,
                        XmlParserErrors::XmlErrAttributeRedefined,
                        "Attribute {} in {} redefined\n",
                        name,
                        namespace.href().unwrap().into_owned()
                    );
                    (*ctxt).well_formed = 0;
                    if (*ctxt).recovery == 0 {
                        (*ctxt).disable_sax = 1;
                    }
                    // goto error;
                    if !nval.is_null() {
                        xml_free(nval as _);
                    }
                }
                prop = now.next;
            }
        } else {
            xml_ns_err_msg!(
                ctxt,
                XmlParserErrors::XmlNsErrUndefinedNamespace,
                "Namespace prefix {} of attribute {} is not defined\n",
                ns,
                name
            );
        }
        namespace
    } else {
        None
    };

    // !!!!!! <a toto:arg="" xmlns:toto="http://toto.com">
    let Some(mut ret) = xml_new_ns_prop((*ctxt).node, namespace, name, null()) else {
        // goto error;
        if !nval.is_null() {
            xml_free(nval as _);
        }
        return;
    };

    if (*ctxt).replace_entities == 0 && (*ctxt).html == 0 {
        ret.children = if (*ctxt).my_doc.is_null() {
            None
        } else {
            NodePtr::from_ptr((*(*ctxt).my_doc).get_node_list(value))
        };
        let mut tmp = ret.children;
        while let Some(mut now) = tmp {
            now.set_parent(NodePtr::from_ptr(ret.as_ptr() as *mut XmlNode));
            if now.next.is_none() {
                ret.last = Some(now);
            }
            tmp = now.next;
        }
    } else if !value.is_null() {
        ret.children = NodePtr::from_ptr(xml_new_doc_text((*ctxt).my_doc, value));
        ret.last = ret.children;
        if let Some(mut children) = ret.children {
            children.set_parent(NodePtr::from_ptr(ret.as_ptr() as *mut XmlNode));
        }
    }

    #[cfg(not(feature = "libxml_valid"))]
    let f = false;
    #[cfg(feature = "libxml_valid")]
    let f = (*ctxt).html == 0
        && (*ctxt).validate != 0
        && (*ctxt).well_formed != 0
        && !(*ctxt).my_doc.is_null()
        && (*(*ctxt).my_doc).int_subset.is_some();
    if f {
        // If we don't substitute entities, the validation should be
        // done on a value with replaced entities anyway.
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
                    Some(ret),
                    value,
                );
            } else {
                // Do the last stage of the attribute normalization
                // It need to be done twice ... it's an extra burden related
                // to the ability to keep xmlSAX2References in attributes
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
                    Some(ret),
                    val,
                );
                xml_free(val as _);
            }
        } else {
            (*ctxt).valid &= xml_validate_one_attribute(
                addr_of_mut!((*ctxt).vctxt) as _,
                (*ctxt).my_doc,
                (*ctxt).node,
                Some(ret),
                value,
            );
        }
    } else if (*ctxt).loadsubset & XML_SKIP_IDS as i32 == 0
        && (((*ctxt).replace_entities == 0 && (*ctxt).external != 2)
            || ((*ctxt).replace_entities != 0 && (*ctxt).in_subset == 0))
            // Don't create IDs containing entity references
        && ret
            .children
            .filter(|c| matches!(c.element_type(), XmlElementType::XmlTextNode) && c.next.is_none())
            .is_some()
    {
        let content: *mut XmlChar = ret.children.unwrap().content;
        // when validating, the ID registration is done at the attribute
        // validation level. Otherwise we have to do specific handling here.
        if fullname == "xml:id" {
            // Add the xml:id value
            //
            // Open issue: normalization of the value.
            if xml_validate_ncname(content, 1) != 0 {
                let content = CStr::from_ptr(content as *const i8).to_string_lossy();
                xml_err_valid!(
                    ctxt,
                    XmlParserErrors::XmlDTDXmlidValue,
                    "xml:id : attribute value {} is not an NCName\n",
                    content
                );
            }
            xml_add_id(
                addr_of_mut!((*ctxt).vctxt) as _,
                (*ctxt).my_doc,
                CStr::from_ptr(content as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                ret,
            );
        } else if xml_is_id((*ctxt).my_doc, (*ctxt).node, Some(ret)) != 0 {
            xml_add_id(
                addr_of_mut!((*ctxt).vctxt) as _,
                (*ctxt).my_doc,
                CStr::from_ptr(content as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                ret,
            );
        } else if xml_is_ref((*ctxt).my_doc, (*ctxt).node, Some(ret)) != 0 {
            xml_add_ref(
                addr_of_mut!((*ctxt).vctxt) as _,
                (*ctxt).my_doc,
                CStr::from_ptr(content as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                ret,
            );
        }
    }

    // error:
    if !nval.is_null() {
        xml_free(nval as _);
    }
}

/// Check defaulted attributes from the DTD
#[doc(alias = "xmlCheckDefaultedAttributes")]
#[cfg(any(feature = "sax1", feature = "html", feature = "libxml_writer",))]
unsafe fn xml_check_defaulted_attributes(
    ctxt: XmlParserCtxtPtr,
    name: &str,
    prefix: Option<&str>,
    atts: &[(String, Option<String>)],
) {
    use crate::parser::build_qname;

    let decls = [
        xml_get_dtd_qelement_desc((*(*ctxt).my_doc).int_subset, name, prefix),
        xml_get_dtd_qelement_desc((*(*ctxt).my_doc).ext_subset, name, prefix),
    ];
    for decl in decls {
        let Some(elem_decl) = decl else {
            continue;
        };
        let mut attr = elem_decl.attributes;
        // Check against defaulted attributes from the external subset
        // if the document is stamped as standalone
        if (*(*ctxt).my_doc).standalone == 1
            && (*(*ctxt).my_doc).ext_subset.is_some()
            && (*ctxt).validate != 0
        {
            while let Some(cur_attr) = attr {
                let elem = cur_attr.elem.as_deref().map(|p| CString::new(p).unwrap());
                if !cur_attr.default_value.is_null()
                    && (*(*ctxt).my_doc).ext_subset.and_then(|dtd| {
                        dtd.get_qattr_desc(
                            cur_attr.elem.as_deref().unwrap(),
                            cur_attr.name().as_deref().unwrap(),
                            cur_attr.prefix.as_deref(),
                        )
                    }) == Some(cur_attr)
                    && (*(*ctxt).my_doc)
                        .int_subset
                        .and_then(|dtd| {
                            dtd.get_qattr_desc(
                                cur_attr.elem.as_deref().unwrap(),
                                cur_attr.name().as_deref().unwrap(),
                                cur_attr.prefix.as_deref(),
                            )
                        })
                        .is_none()
                {
                    let mut fulln: *mut XmlChar;

                    if let Some(prefix) = cur_attr.prefix.as_deref() {
                        let prefix = CString::new(prefix).unwrap();
                        fulln = xml_strdup(prefix.as_ptr() as *const u8);
                        fulln = xml_strcat(fulln, c":".as_ptr() as _);
                        fulln = xml_strcat(fulln, cur_attr.name);
                    } else {
                        fulln = xml_strdup(cur_attr.name);
                    }
                    if fulln.is_null() {
                        xml_sax2_err_memory(ctxt, "xmlSAX2StartElement");
                        break;
                    }

                    // Check that the attribute is not declared in the serialization
                    if !atts.iter().any(|(att, _)| {
                        att.as_str()
                            == CStr::from_ptr(fulln as *const i8)
                                .to_string_lossy()
                                .as_ref()
                    }) {
                        let fulln = CStr::from_ptr(fulln as *const i8).to_string_lossy();
                        xml_err_valid!(
                            ctxt,
                            XmlParserErrors::XmlDTDStandaloneDefaulted,
                            "standalone: attribute {} on {} defaulted from external subset\n",
                            fulln,
                            elem.as_deref().unwrap().to_string_lossy().into_owned()
                        );
                    }
                    xml_free(fulln as _);
                }
                attr = cur_attr.nexth;
            }
        }

        // Actually insert defaulted values when needed
        let mut attr = elem_decl.attributes;
        while let Some(cur_attr) = attr {
            // Make sure that attributes redefinition occurring in the
            // internal subset are not overridden by definitions in the external subset.
            if !cur_attr.default_value.is_null() {
                // the element should be instantiated in the tree if:
                //  - this is a namespace prefix
                //  - the user required for completion in the tree
                //    like XSLT
                //  - there isn't already an attribute definition
                //    in the internal subset overriding it.
                if cur_attr.prefix.as_deref() == Some("xmlns")
                    || (cur_attr.prefix.is_none() && cur_attr.name().as_deref() == Some("xmlns"))
                    || (*ctxt).loadsubset & XML_COMPLETE_ATTRS as i32 != 0
                {
                    let tst = (*(*ctxt).my_doc).int_subset.and_then(|dtd| {
                        dtd.get_qattr_desc(
                            cur_attr.elem.as_deref().unwrap(),
                            cur_attr.name().as_deref().unwrap(),
                            cur_attr.prefix.as_deref(),
                        )
                    });
                    if tst == Some(cur_attr) || tst.is_none() {
                        let name = cur_attr.name().unwrap();
                        let fulln = build_qname(&name, cur_attr.prefix.as_deref());

                        // Check that the attribute is not declared in the serialization
                        if !atts.iter().any(|(att, _)| att.as_str() == fulln) {
                            let value = cur_attr.default_value;
                            xml_sax2_attribute_internal(
                                Some(GenericErrorContext::new(ctxt)),
                                &fulln,
                                (!value.is_null())
                                    .then(|| CStr::from_ptr(value as *const i8).to_string_lossy())
                                    .as_deref(),
                                prefix,
                            );
                        }
                    }
                }
            }
            attr = cur_attr.nexth;
        }
    }
}

/// called when an opening tag has been processed.
#[doc(alias = "xmlSAX2StartElement")]
#[cfg(any(feature = "sax1", feature = "html", feature = "libxml_writer",))]
pub unsafe fn xml_sax2_start_element(
    ctx: Option<GenericErrorContext>,
    fullname: &str,
    atts: &[(String, Option<String>)],
) {
    use crate::libxml::parser_internals::XML_VCTXT_DTD_VALIDATED;

    let mut parent: *mut XmlNode;

    if ctx.is_none() {
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

    // First check on validity:
    if (*ctxt).validate != 0
        && (*(*ctxt).my_doc).ext_subset.is_none()
        && (*(*ctxt).my_doc).int_subset.map_or(true, |int_subset| {
            int_subset.notations.is_none()
                && int_subset.elements.is_none()
                && int_subset.attributes.is_none()
                && int_subset.entities.is_none()
        })
    {
        xml_err_valid!(
            ctxt,
            XmlParserErrors::XmlErrNoDTD,
            "Validation failed: no DTD found !"
        );
        (*ctxt).validate = 0;
    }

    let (prefix, name) = if (*ctxt).html != 0 {
        (None, fullname)
    } else {
        // Split the full name into a namespace prefix and the tag name
        split_qname(&mut *ctxt, fullname)
    };

    // Note : the namespace resolution is deferred until the end of the
    //        attributes parsing, since local namespace can be defined as
    //        an attribute at this level.
    let ret: *mut XmlNode = xml_new_doc_node((*ctxt).my_doc, None, name, null_mut());
    if ret.is_null() {
        xml_sax2_err_memory(ctxt, "xmlSAX2StartElement");
        return;
    }
    if let Some(children) = (*(*ctxt).my_doc).children {
        if parent.is_null() {
            parent = children.as_ptr();
        }
    } else {
        (*(*ctxt).my_doc).add_child(ret);
    }
    (*ctxt).nodemem = -1;
    if (*ctxt).linenumbers != 0 && !(*ctxt).input.is_null() {
        if ((*(*ctxt).input).line as u32) < u16::MAX as u32 {
            (*ret).line = (*(*ctxt).input).line as _;
        } else {
            (*ret).line = u16::MAX;
        }
    }

    // We are parsing a new node.
    if (*ctxt).node_push(ret) < 0 {
        (*ret).unlink();
        xml_free_node(ret);
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
        if (*(*ctxt).my_doc).int_subset.is_some() || (*(*ctxt).my_doc).ext_subset.is_some() {
            xml_check_defaulted_attributes(ctxt, name, prefix, atts);
        }

        // process all the attributes whose name start with "xmlns"
        for (att, value) in atts {
            if att.starts_with("xmlns") {
                xml_sax2_attribute_internal(ctx.clone(), att, value.as_deref(), prefix);
            }
        }

        // Search the namespace, note that since the attributes have been
        // processed, the local namespaces are available.
        let mut ns = (*ret).search_ns((*ctxt).my_doc, prefix).or_else(|| {
            (!parent.is_null())
                .then(|| (*parent).search_ns((*ctxt).my_doc, prefix))
                .flatten()
        });
        if ns.is_none() {
            if let Some(prefix) = prefix {
                ns = xml_new_ns(ret, null_mut(), Some(prefix));
                xml_ns_warn_msg!(
                    ctxt,
                    XmlParserErrors::XmlNsErrUndefinedNamespace,
                    "Namespace prefix {} is not defined\n",
                    prefix
                );
            }
        }

        // set the namespace node, making sure that if the default namespace
        // is unbound on a parent we simply keep it NULL
        if let Some(ns) =
            ns.filter(|ns| !ns.href.is_null() && (*ns.href.add(0) != 0 || ns.prefix().is_some()))
        {
            (*ret).set_ns(Some(ns));
        }
    }

    // process all the other attributes
    if (*ctxt).html != 0 {
        for (att, value) in atts {
            xml_sax2_attribute_internal(ctx.clone(), att, value.as_deref(), None);
        }
    } else {
        for (att, value) in atts {
            if !att.starts_with("xmlns") {
                xml_sax2_attribute_internal(ctx.clone(), att, value.as_deref(), None);
            }
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

/// called when the end of an element has been detected.
#[doc(alias = "xmlSAX2EndElement")]
#[cfg(any(feature = "sax1", feature = "html", feature = "libxml_writer",))]
pub unsafe fn xml_sax2_end_element(ctx: Option<GenericErrorContext>, _name: &str) {
    if ctx.is_none() {
        return;
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    let cur: *mut XmlNode = (*ctxt).node;

    (*ctxt).nodemem = -1;

    #[cfg(feature = "libxml_valid")]
    {
        if (*ctxt).validate != 0
            && (*ctxt).well_formed != 0
            && !(*ctxt).my_doc.is_null()
            && (*(*ctxt).my_doc).int_subset.is_some()
        {
            (*ctxt).valid &=
                xml_validate_one_element(addr_of_mut!((*ctxt).vctxt) as _, (*ctxt).my_doc, cur);
        }
    }

    // end of parsing of this node.
    (*ctxt).node_pop();
}

/// SAX2 callback when an element start has been detected by the parser.
/// It provides the namespace information for the element, as well as
/// the new namespace declarations on the element.
#[doc(alias = "xmlSAX2StartElementNs")]
pub unsafe fn xml_sax2_start_element_ns(
    ctx: Option<GenericErrorContext>,
    localname: &str,
    prefix: Option<&str>,
    // I want to rename to `uri`, but it also appears as a local variable....
    orig_uri: Option<&str>,
    namespaces: &[(Option<String>, String)],
    nb_defaulted: usize,
    attributes: &[(String, Option<String>, Option<String>, String)],
) {
    let ret: *mut XmlNode;

    if ctx.is_none() {
        return;
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    let parent: *mut XmlNode = (*ctxt).node;
    // First check on validity:
    if (*ctxt).validate != 0
        && (*(*ctxt).my_doc).ext_subset.is_none()
        && (*(*ctxt).my_doc).int_subset.map_or(true, |int_subset| {
            int_subset.notations.is_none()
                && int_subset.elements.is_none()
                && int_subset.attributes.is_none()
                && int_subset.entities.is_none()
        })
    {
        xml_err_valid!(
            ctxt,
            XmlParserErrors::XmlDTDNoDTD,
            "Validation failed: no DTD found !"
        );
        (*ctxt).validate = 0;
    }

    // Take care of the rare case of an undefined namespace prefix
    let mut lname = None;
    if orig_uri.is_none() {
        if let Some(prefix) = prefix {
            lname = Some(build_qname(localname, Some(prefix)));
        }
    }
    // allocate the node
    if !(*ctxt).free_elems.is_null() {
        ret = (*ctxt).free_elems;
        (*ctxt).free_elems = (*ret).next().map_or(null_mut(), |n| n.as_ptr());
        (*ctxt).free_elems_nr -= 1;
        std::ptr::write(&mut *ret, XmlNode::default());
        (*ret).doc = (*ctxt).my_doc;
        (*ret).typ = XmlElementType::XmlElementNode;

        if let Some(lname) = lname {
            (*ret).name = xml_strndup(lname.as_ptr(), lname.len() as i32);
        } else {
            (*ret).name = xml_strndup(localname.as_ptr(), localname.len() as i32);
        }
        if (*ret).name.is_null() {
            xml_sax2_err_memory(ctxt, "xmlSAX2StartElementNs");
            xml_free(ret as _);
            return;
        }
        if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
        // && xmlRegisterNodeDefaultValue.is_some()
        {
            xml_register_node_default_value(ret);
        }
    } else {
        if let Some(lname) = lname {
            ret = xml_new_doc_node((*ctxt).my_doc, None, &lname, null_mut());
        } else {
            ret = xml_new_doc_node((*ctxt).my_doc, None, localname, null_mut());
        }
        if ret.is_null() {
            xml_sax2_err_memory(ctxt, "xmlSAX2StartElementNs");
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
        (*(*ctxt).my_doc).add_child(ret);
    }
    // Build the namespace list
    let mut last = None::<XmlNsPtr>;
    for (pref, uri) in namespaces {
        let uri = CString::new(uri.as_str()).unwrap();
        let Some(ns) = xml_new_ns(null_mut(), uri.as_ptr() as *const u8, pref.as_deref()) else {
            // any out of memory error would already have been raised
            // but we can't be guaranteed it's the actual error due to the
            // API, best is to skip in this case
            continue;
        };
        if let Some(mut l) = last {
            l.next = ns.as_ptr();
            last = Some(ns);
        } else {
            (*ret).ns_def = Some(ns);
            last = Some(ns);
        }
        if orig_uri.is_some() && prefix == pref.as_deref() {
            (*ret).ns = Some(ns);
        }
        #[cfg(feature = "libxml_valid")]
        {
            if (*ctxt).html == 0
                && (*ctxt).validate != 0
                && (*ctxt).well_formed != 0
                && !(*ctxt).my_doc.is_null()
                && (*(*ctxt).my_doc).int_subset.is_some()
            {
                (*ctxt).valid &= xml_validate_one_namespace(
                    addr_of_mut!((*ctxt).vctxt) as _,
                    (*ctxt).my_doc,
                    ret,
                    prefix,
                    ns,
                    uri.as_ptr() as *const u8,
                );
            }
        }
    }
    (*ctxt).nodemem = -1;

    // We are parsing a new node.
    if (*ctxt).node_push(ret) < 0 {
        (*ret).unlink();
        xml_free_node(ret);
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

    // Insert the defaulted attributes from the DTD only if requested:
    let mut nb_attributes = attributes.len();
    if nb_defaulted != 0 && (*ctxt).loadsubset & XML_COMPLETE_ATTRS as i32 == 0 {
        nb_attributes -= nb_defaulted;
    }

    // Search the namespace if it wasn't already found
    // Note that, if prefix is NULL, this searches for the default Ns
    if orig_uri.is_some() && (*ret).ns.is_none() {
        (*ret).ns = if !parent.is_null() {
            (*parent).search_ns((*ctxt).my_doc, prefix)
        } else {
            None
        };
        if (*ret).ns.is_none() && prefix == Some("xml") {
            (*ret).ns = (*ret).search_ns((*ctxt).my_doc, prefix);
        }
        if (*ret).ns.is_none() {
            if xml_new_ns(ret, null_mut(), prefix).is_none() {
                xml_sax2_err_memory(ctxt, "xmlSAX2StartElementNs");
                return;
            }
            if let Some(prefix) = prefix {
                xml_ns_warn_msg!(
                    ctxt,
                    XmlParserErrors::XmlNsErrUndefinedNamespace,
                    "Namespace prefix {} was not found\n",
                    prefix
                );
            } else {
                xml_ns_warn_msg!(
                    ctxt,
                    XmlParserErrors::XmlNsErrUndefinedNamespace,
                    "Namespace default prefix was not found\n"
                );
            }
        }
    }

    // process all the other attributes
    let attributes = attributes
        .iter()
        .take(nb_attributes)
        .map(|attr| {
            let loc = CString::new(attr.0.as_str()).unwrap();
            let pre = attr.1.as_deref().map(|pre| CString::new(pre).unwrap());
            let url = attr.2.as_deref().map(|url| CString::new(url).unwrap());
            let val = CString::new(attr.3.as_str()).unwrap();
            (loc, pre, url, val)
        })
        .collect::<Vec<_>>();
    for attr in &attributes {
        // Handle the rare case of an undefined attribute prefix
        if attr.1.is_some() && attr.2.is_none() {
            let lname = xml_build_qname(
                attr.0.as_ptr() as *const u8,
                attr.1
                    .as_deref()
                    .map_or(null(), |p| p.as_ptr() as *const u8),
                null_mut(),
                0,
            );
            if !lname.is_null() {
                xml_sax2_attribute_ns(ctxt, lname, null_mut(), attr.3.as_ptr() as *const u8);
                xml_free(lname as _);
                continue;
            }
        }

        xml_sax2_attribute_ns(
            ctxt,
            attr.0.as_ptr() as *const u8,
            attr.1
                .as_deref()
                .map_or(null(), |p| p.as_ptr() as *const u8),
            attr.3.as_ptr() as *const u8,
        );
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

    drop(attributes);
}

/// Returns the newly allocated string or NULL if not needed or error
#[doc(alias = "xmlSAX2TextNode")]
unsafe fn xml_sax2_text_node(ctxt: XmlParserCtxtPtr, s: &str) -> *mut XmlNode {
    // Allocate
    let ret: *mut XmlNode = if !(*ctxt).free_elems.is_null() {
        let ret = (*ctxt).free_elems;
        (*ctxt).free_elems = (*ret).next.map_or(null_mut(), |n| n.as_ptr());
        (*ctxt).free_elems_nr -= 1;
        ret
    } else {
        xml_malloc(size_of::<XmlNode>()) as *mut XmlNode
    };
    if ret.is_null() {
        xml_err_memory(ctxt, Some("xmlSAX2Characters"));
        return null_mut();
    }
    std::ptr::write(&mut *ret, XmlNode::default());
    (*ret).typ = XmlElementType::XmlTextNode;
    (*ret).name = XML_STRING_TEXT.as_ptr() as _;
    (*ret).content = xml_strndup(s.as_ptr(), s.len() as i32);
    if (*ret).content.is_null() {
        xml_sax2_err_memory(ctxt, "xmlSAX2TextNode");
        xml_free(ret as _);
        return null_mut();
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
unsafe fn xml_sax2_decode_attr_entities(
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
unsafe fn xml_sax2_attribute_ns(
    ctxt: XmlParserCtxtPtr,
    localname: *const XmlChar,
    prefix: *const XmlChar,
    value: *const XmlChar,
) {
    let mut namespace = None;
    let mut dup: *mut XmlChar = null_mut();

    // Note: if prefix.is_null(), the attribute is not in the default namespace
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

    // allocate the node
    let mut ret = if !(*ctxt).free_attrs.is_null() {
        let mut ret = XmlAttrPtr::from_raw((*ctxt).free_attrs).unwrap().unwrap();
        (*ctxt).free_attrs = ret.next.map_or(null_mut(), |next| next.as_ptr());
        (*ctxt).free_attrs_nr -= 1;
        std::ptr::write(&mut *ret, XmlAttr::default());
        ret.typ = XmlElementType::XmlAttributeNode;
        ret.parent = NodePtr::from_ptr((*ctxt).node);
        ret.doc = (*ctxt).my_doc;
        ret.ns = namespace;
        ret.name = xml_strdup(localname);

        // link at the end to preserve order, TODO speed up with a last
        if let Some(mut prev) = (*(*ctxt).node).properties {
            while let Some(next) = prev.next {
                prev = next;
            }
            prev.next = Some(ret);
            ret.prev = Some(prev);
        } else {
            (*(*ctxt).node).properties = Some(ret);
        }

        if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
        // && xmlRegisterNodeDefaultValue.is_some()
        {
            xml_register_node_default_value(ret.as_ptr() as *mut XmlNode);
        }
        ret
    } else {
        let Some(ret) = xml_new_ns_prop(
            (*ctxt).node,
            namespace,
            &CStr::from_ptr(localname as *const i8).to_string_lossy(),
            null_mut(),
        ) else {
            xml_err_memory(ctxt, Some("xmlSAX2AttributeNs"));
            return;
        };
        ret
    };

    if (*ctxt).replace_entities == 0 && (*ctxt).html == 0 {
        // We know that if there is an entity reference, then
        // the string has been dup'ed and terminates with 0
        // otherwise with ' or "
        ret.children = if (*ctxt).my_doc.is_null() {
            None
        } else {
            let len = CStr::from_ptr(value as *const i8).to_bytes().len();
            NodePtr::from_ptr((*(*ctxt).my_doc).get_node_list_with_strlen(value, len as i32))
        };
        let mut tmp = ret.children;
        while let Some(mut now) = tmp {
            now.doc = ret.doc;
            now.set_parent(NodePtr::from_ptr(ret.as_ptr() as *mut XmlNode));
            if now.next.is_none() {
                ret.last = Some(now);
            }
            tmp = now.next;
        }
    } else if !value.is_null() {
        let value = CStr::from_ptr(value as *const i8)
            .to_string_lossy()
            .into_owned();
        let tmp: *mut XmlNode = xml_sax2_text_node(ctxt, &value);
        ret.children = NodePtr::from_ptr(tmp);
        ret.last = NodePtr::from_ptr(tmp);
        if !tmp.is_null() {
            (*tmp).doc = ret.doc;
            (*tmp).set_parent(NodePtr::from_ptr(ret.as_ptr() as *mut XmlNode));
        }
    }

    #[cfg(not(feature = "libxml_valid"))]
    let f = false;
    #[cfg(feature = "libxml_valid")]
    let f = (*ctxt).html == 0
        && (*ctxt).validate != 0
        && (*ctxt).well_formed != 0
        && !(*ctxt).my_doc.is_null()
        && (*(*ctxt).my_doc).int_subset.is_some();
    if f {
        #[cfg(feature = "libxml_valid")]
        {
            let value = CStr::from_ptr(value as *const i8);
            let len = value.to_bytes().len();
            let valueend = value.as_ptr().add(len) as *const u8;
            // If we don't substitute entities, the validation should be
            // done on a value with replaced entities anyway.
            if (*ctxt).replace_entities == 0 {
                dup = xml_sax2_decode_attr_entities(ctxt, value.as_ptr() as *const u8, valueend);
                if dup.is_null() {
                    (*ctxt).valid &= xml_validate_one_attribute(
                        addr_of_mut!((*ctxt).vctxt) as _,
                        (*ctxt).my_doc,
                        (*ctxt).node,
                        Some(ret),
                        value.as_ptr() as *const u8,
                    );
                } else {
                    // dup now contains a string of the flattened attribute
                    // content with entities substituted. Check if we need to
                    // apply an extra layer of normalization.
                    // It need to be done twice ... it's an extra burden related
                    // to the ability to keep references in attributes
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
                                CStr::from_ptr(fullname as *const i8)
                                    .to_string_lossy()
                                    .as_ref(),
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
                        Some(ret),
                        dup,
                    );
                }
            } else {
                // if entities already have been substituted, then
                // the attribute as passed is already normalized
                dup = xml_strndup(value.as_ptr() as *const u8, len as i32);

                (*ctxt).valid &= xml_validate_one_attribute(
                    addr_of_mut!((*ctxt).vctxt) as _,
                    (*ctxt).my_doc,
                    (*ctxt).node,
                    Some(ret),
                    dup,
                );
            }
        }
    } else if (*ctxt).loadsubset & XML_SKIP_IDS as i32 == 0
        && (((*ctxt).replace_entities == 0 && (*ctxt).external != 2)
            || ((*ctxt).replace_entities != 0 && (*ctxt).in_subset == 0))
            // Don't create IDs containing entity references
        && ret
            .children
            .filter(|c| matches!(c.element_type(), XmlElementType::XmlTextNode) && c.next.is_none())
            .is_some()
    {
        let content: *mut XmlChar = ret.children.unwrap().content;
        // when validating, the ID registration is done at the attribute
        // validation level. Otherwise we have to do specific handling here.
        if (!prefix.is_null()).then(|| CStr::from_ptr(prefix as *const i8).to_string_lossy())
            == (*ctxt).str_xml
            && *localname.add(0) == b'i'
            && *localname.add(1) == b'd'
            && *localname.add(2) == 0
        {
            // Add the xml:id value
            //
            // Open issue: normalization of the value.
            #[cfg(any(feature = "sax1", feature = "html", feature = "libxml_writer",))]
            {
                #[cfg(feature = "libxml_valid")]
                if xml_validate_ncname(content, 1) != 0 {
                    let content = CStr::from_ptr(content as *const i8).to_string_lossy();
                    xml_err_valid!(
                        ctxt,
                        XmlParserErrors::XmlDTDXmlidValue,
                        "xml:id : attribute value {} is not an NCName\n",
                        content
                    );
                }
            }
            xml_add_id(
                addr_of_mut!((*ctxt).vctxt) as _,
                (*ctxt).my_doc,
                CStr::from_ptr(content as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                ret,
            );
        } else if xml_is_id((*ctxt).my_doc, (*ctxt).node, Some(ret)) != 0 {
            xml_add_id(
                addr_of_mut!((*ctxt).vctxt) as _,
                (*ctxt).my_doc,
                CStr::from_ptr(content as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                ret,
            );
        } else if xml_is_ref((*ctxt).my_doc, (*ctxt).node, Some(ret)) != 0 {
            xml_add_ref(
                addr_of_mut!((*ctxt).vctxt) as _,
                (*ctxt).my_doc,
                CStr::from_ptr(content as *const i8)
                    .to_string_lossy()
                    .as_ref(),
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
    _localname: &str,
    _prefix: Option<&str>,
    _uri: Option<&str>,
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
            && (*(*ctxt).my_doc).int_subset.is_some()
        {
            (*ctxt).valid &= xml_validate_one_element(
                addr_of_mut!((*ctxt).vctxt) as _,
                (*ctxt).my_doc,
                (*ctxt).node,
            );
        }
    }

    // end of parsing of this node.
    (*ctxt).node_pop();
}

/// Called when an entity xmlSAX2Reference is detected.
#[doc(alias = "xmlSAX2Reference")]
pub unsafe fn xml_sax2_reference(ctx: Option<GenericErrorContext>, name: &str) {
    if ctx.is_none() {
        return;
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };

    let ret = if name.starts_with('#') {
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
unsafe fn xml_sax2_text(ctxt: XmlParserCtxtPtr, ch: &str, typ: XmlElementType) {
    if ctxt.is_null() {
        return;
    }
    // Handle the data if any. If there is no child
    // add it as content, otherwise if the last child is text,
    // concatenate it, else create a new node of type text.
    if (*ctxt).node.is_null() {
        return;
    }
    let mut last_child = (*(*ctxt).node).last().map_or(null_mut(), |l| l.as_ptr());

    // Here we needed an accelerator mechanism in case of very large elements.
    // Use an attribute in the structure !!!
    if last_child.is_null() {
        if matches!(typ, XmlElementType::XmlTextNode) {
            last_child = xml_sax2_text_node(ctxt, ch);
        } else {
            last_child = xml_new_cdata_block((*ctxt).my_doc, ch);
        }
        if !last_child.is_null() {
            (*(*ctxt).node).set_children(NodePtr::from_ptr(last_child));
            (*(*ctxt).node).set_last(NodePtr::from_ptr(last_child));
            (*last_child).set_parent(NodePtr::from_ptr((*ctxt).node));
            (*last_child).doc = (*(*ctxt).node).doc;
            (*ctxt).nodelen = ch.len() as i32;
            (*ctxt).nodemem = ch.len() as i32 + 1;
        } else {
            xml_sax2_err_memory(ctxt, "xmlSAX2Characters");
        }
    } else {
        let coalesce_text: i32 = (!last_child.is_null()
            && (*last_child).element_type() == typ
            && (!matches!(typ, XmlElementType::XmlTextNode)
                || ((*last_child).name == XML_STRING_TEXT.as_ptr() as _)))
            as i32;
        if coalesce_text != 0 && (*ctxt).nodemem != 0 {
            // The whole point of maintaining nodelen and nodemem,
            // xmlTextConcat is too costly, i.e. compute length,
            // reallocate a new buffer, move data, append ch. Here
            // We try to minimize realloc() uses and avoid copying
            // and recomputing length over and over.
            if (*ctxt).nodemem == (*ctxt).nodelen + 1
                && xml_dict_owns((*ctxt).dict, (*last_child).content) != 0
            {
                (*last_child).content = xml_strdup((*last_child).content);
            }
            if (*last_child).content.is_null() {
                xml_sax2_err_memory(ctxt, "xmlSAX2Characters: xmlStrdup returned NULL");
                return;
            }
            let (nodelen, overflowed) = (*ctxt).nodelen.overflowing_add(ch.len() as i32);
            if overflowed {
                xml_sax2_err_memory(ctxt, "xmlSAX2Characters overflow prevented");
                return;
            }
            if nodelen > XML_MAX_TEXT_LENGTH as i32
                && (*ctxt).options & XmlParserOption::XmlParseHuge as i32 == 0
            {
                xml_sax2_err_memory(ctxt, "xmlSAX2Characters: huge text node");
                return;
            }
            if nodelen >= (*ctxt).nodemem {
                let size = (*ctxt)
                    .nodemem
                    .saturating_add(ch.len() as i32)
                    .saturating_mul(2);
                let newbuf: *mut XmlChar = xml_realloc((*last_child).content as _, size as _) as _;
                if newbuf.is_null() {
                    xml_sax2_err_memory(ctxt, "xmlSAX2Characters");
                    return;
                }
                (*ctxt).nodemem = size;
                (*last_child).content = newbuf;
            }
            memcpy(
                (*last_child).content.add((*ctxt).nodelen as usize) as _,
                ch.as_ptr() as _,
                ch.len(),
            );
            (*ctxt).nodelen = nodelen;
            *(*last_child).content.add((*ctxt).nodelen as usize) = 0;
        } else if coalesce_text != 0 {
            if xml_text_concat(last_child, ch) != 0 {
                xml_sax2_err_memory(ctxt, "xmlSAX2Characters");
            }
            if (*(*ctxt).node).children().is_some() {
                (*ctxt).nodelen = xml_strlen((*last_child).content);
                (*ctxt).nodemem = (*ctxt).nodelen + 1;
            }
        } else {
            // Mixed content, first time
            if matches!(typ, XmlElementType::XmlTextNode) {
                last_child = xml_sax2_text_node(ctxt, ch);
                if !last_child.is_null() {
                    (*last_child).doc = (*ctxt).my_doc;
                }
            } else {
                last_child = xml_new_cdata_block((*ctxt).my_doc, ch);
            }
            if !last_child.is_null() {
                (*(*ctxt).node).add_child(last_child);
                if (*(*ctxt).node).children().is_some() {
                    (*ctxt).nodelen = ch.len() as i32;
                    (*ctxt).nodemem = ch.len() as i32 + 1;
                }
            }
        }
    }
}

/// Receiving some chars from the parser.
#[doc(alias = "xmlSAX2Characters")]
pub unsafe fn xml_sax2_characters(ctx: Option<GenericErrorContext>, ch: &str) {
    let ctxt = if let Some(ctx) = ctx {
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    } else {
        null_mut()
    };
    xml_sax2_text(ctxt as XmlParserCtxtPtr, ch, XmlElementType::XmlTextNode);
}

/// Receiving some ignorable whitespaces from the parser.
/// UNUSED: by default the DOM building will use xmlSAX2Characters
#[doc(alias = "xmlSAX2IgnorableWhitespace")]
pub unsafe fn xml_sax2_ignorable_whitespace(_ctx: Option<GenericErrorContext>, _ch: &str) {
    /* let ctxt: xmlParserCtxtPtr = ctx as xmlParserCtxtPtr; */
}

/// A processing instruction has been parsed.
#[doc(alias = "xmlSAX2ProcessingInstruction")]
pub unsafe fn xml_sax2_processing_instruction(
    ctx: Option<GenericErrorContext>,
    target: &str,
    data: Option<&str>,
) {
    if ctx.is_none() {
        return;
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };

    let parent: *mut XmlNode = (*ctxt).node;

    let ret: *mut XmlNode = xml_new_doc_pi((*ctxt).my_doc, target, data);
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
        (*(*ctxt).my_doc).int_subset.unwrap().add_child(ret);
        return;
    } else if (*ctxt).in_subset == 2 {
        (*(*ctxt).my_doc).ext_subset.unwrap().add_child(ret);
        return;
    }
    if parent.is_null() {
        (*(*ctxt).my_doc).add_child(ret);
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
pub unsafe fn xml_sax2_comment(ctx: Option<GenericErrorContext>, value: &str) {
    if ctx.is_none() {
        return;
    }
    let ctxt = {
        let ctx = ctx.unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    let parent: *mut XmlNode = (*ctxt).node;
    let ret: *mut XmlNode = xml_new_doc_comment((*ctxt).my_doc, value);
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
        (*(*ctxt).my_doc).int_subset.unwrap().add_child(ret);
        return;
    } else if (*ctxt).in_subset == 2 {
        (*(*ctxt).my_doc).ext_subset.unwrap().add_child(ret);
        return;
    }
    if parent.is_null() {
        (*(*ctxt).my_doc).add_child(ret);
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
pub unsafe fn xml_sax2_cdata_block(ctx: Option<GenericErrorContext>, value: &str) {
    let ctxt = if let Some(ctx) = ctx {
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    } else {
        null_mut()
    };
    xml_sax2_text(ctxt, value, XmlElementType::XmlCDATASectionNode);
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
pub unsafe fn xml_sax_default_version(version: i32) -> i32 {
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
pub fn xml_sax_version(hdlr: &mut XmlSAXHandler, version: i32) -> i32 {
    if version == 2 {
        hdlr.start_element = None;
        hdlr.end_element = None;
        hdlr.start_element_ns = Some(xml_sax2_start_element_ns);
        hdlr.end_element_ns = Some(xml_sax2_end_element_ns);
        hdlr.serror = None;
        hdlr.initialized = XML_SAX2_MAGIC as _;
    } else if cfg!(feature = "sax1") && version == 1 {
        #[cfg(feature = "sax1")]
        {
            hdlr.start_element = Some(xml_sax2_start_element);
            hdlr.end_element = Some(xml_sax2_end_element);
            hdlr.initialized = 1;
        }
    } else {
        return -1;
    }
    hdlr.internal_subset = Some(xml_sax2_internal_subset);
    hdlr.external_subset = Some(xml_sax2_external_subset);
    hdlr.is_standalone = Some(xml_sax2_is_standalone);
    hdlr.has_internal_subset = Some(xml_sax2_has_internal_subset);
    hdlr.has_external_subset = Some(xml_sax2_has_external_subset);
    hdlr.resolve_entity = Some(xml_sax2_resolve_entity);
    hdlr.get_entity = Some(xml_sax2_get_entity);
    hdlr.get_parameter_entity = Some(xml_sax2_get_parameter_entity);
    hdlr.entity_decl = Some(xml_sax2_entity_decl);
    hdlr.attribute_decl = Some(xml_sax2_attribute_decl);
    hdlr.element_decl = Some(xml_sax2_element_decl);
    hdlr.notation_decl = Some(xml_sax2_notation_decl);
    hdlr.unparsed_entity_decl = Some(xml_sax2_unparsed_entity_decl);
    hdlr.set_document_locator = Some(xml_sax2_set_document_locator);
    hdlr.start_document = Some(xml_sax2_start_document);
    hdlr.end_document = Some(xml_sax2_end_document);
    hdlr.reference = Some(xml_sax2_reference);
    hdlr.characters = Some(xml_sax2_characters);
    hdlr.cdata_block = Some(xml_sax2_cdata_block);
    hdlr.ignorable_whitespace = Some(xml_sax2_characters);
    hdlr.processing_instruction = Some(xml_sax2_processing_instruction);
    hdlr.comment = Some(xml_sax2_comment);
    hdlr.warning = Some(parser_warning);
    hdlr.error = Some(parser_error);
    hdlr.fatal_error = Some(parser_error);

    0
}

/// Initialize the default XML SAX2 handler
#[doc(alias = "xmlSAX2InitDefaultSAXHandler")]
pub unsafe fn xml_sax2_init_default_sax_handler(hdlr: &mut XmlSAXHandler, warning: i32) {
    if hdlr.initialized != 0 {
        return;
    }

    xml_sax_version(hdlr, XML_SAX2_DEFAULT_VERSION_VALUE);
    if warning == 0 {
        hdlr.warning = None;
    } else {
        hdlr.warning = Some(parser_warning);
    }
}

/// Initialize the default HTML SAX2 handler
#[doc(alias = "xmlSAX2InitHtmlDefaultSAXHandler")]
#[cfg(feature = "html")]
pub unsafe fn xml_sax2_init_html_default_sax_handler(hdlr: &mut XmlSAXHandler) {
    if hdlr.initialized != 0 {
        return;
    }

    hdlr.internal_subset = Some(xml_sax2_internal_subset);
    hdlr.external_subset = None;
    hdlr.is_standalone = None;
    hdlr.has_internal_subset = None;
    hdlr.has_external_subset = None;
    hdlr.resolve_entity = None;
    hdlr.get_entity = Some(xml_sax2_get_entity);
    hdlr.get_parameter_entity = None;
    hdlr.entity_decl = None;
    hdlr.attribute_decl = None;
    hdlr.element_decl = None;
    hdlr.notation_decl = None;
    hdlr.unparsed_entity_decl = None;
    hdlr.set_document_locator = Some(xml_sax2_set_document_locator);
    hdlr.start_document = Some(xml_sax2_start_document);
    hdlr.end_document = Some(xml_sax2_end_document);
    hdlr.start_element = Some(xml_sax2_start_element);
    hdlr.end_element = Some(xml_sax2_end_element);
    hdlr.reference = None;
    hdlr.characters = Some(xml_sax2_characters);
    hdlr.cdata_block = Some(xml_sax2_cdata_block);
    hdlr.ignorable_whitespace = Some(xml_sax2_ignorable_whitespace);
    hdlr.processing_instruction = Some(xml_sax2_processing_instruction);
    hdlr.comment = Some(xml_sax2_comment);
    hdlr.warning = Some(parser_warning);
    hdlr.error = Some(parser_error);
    hdlr.fatal_error = Some(parser_error);

    hdlr.initialized = 1;
}

#[doc(alias = "htmlDefaultSAXHandlerInit")]
#[deprecated = "This function is a no-op. Call xmlInitParser to initialize the library"]
#[cfg(feature = "html")]
pub unsafe fn html_default_sax_handler_init() {}

/// Initialize the default SAX2 handler
#[doc(alias = "xmlDefaultSAXHandlerInit")]
#[deprecated = "This function is a no-op. Call xmlInitParser to initialize the library"]
pub unsafe fn xml_default_sax_handler_init() {}

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
}
