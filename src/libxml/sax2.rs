//! Provide methods and data structures for SAX2 handlers.
//!
//! This module is based on `libxml/SAX2.h`, `SAX2.c`, and so on in `libxml2-v2.11.8`.  
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
    cell::RefCell,
    ffi::c_void,
    mem::replace,
    rc::Rc,
    sync::atomic::{AtomicI32, Ordering},
};

#[cfg(not(feature = "html"))]
use crate::generic_error;
#[cfg(feature = "html")]
use crate::html::tree::html_new_doc_no_dtd;
use crate::{
    encoding::{XmlCharEncoding, detect_encoding},
    error::{
        __xml_raise_error, XmlErrorDomain, XmlErrorLevel, XmlParserErrors, parser_error,
        parser_warning,
    },
    globals::{StructuredError, get_register_node_func},
    libxml::valid::xml_validate_document_final,
    parser::{
        XML_COMPLETE_ATTRS, XML_MAX_TEXT_LENGTH, XML_SAX2_MAGIC, XML_SKIP_IDS, XML_STRING_TEXT,
        XML_SUBSTITUTE_REF, XML_VCTXT_DTD_VALIDATED, XmlParserCtxt, XmlParserInput,
        XmlParserInputState, XmlParserOption, XmlSAXHandler, XmlSAXLocator, build_qname,
        split_qname, xml_err_memory, xml_load_external_entity,
    },
    tree::{
        NodeCommon, XmlAttr, XmlAttributeDefault, XmlAttributeType, XmlDocProperties,
        XmlElementContent, XmlElementType, XmlElementTypeVal, XmlEntityPtr, XmlEntityType,
        XmlEnumeration, XmlNode, XmlNodePtr, XmlNsPtr, validate_ncname, xml_add_doc_entity,
        xml_add_dtd_entity, xml_create_int_subset, xml_free_dtd, xml_free_node, xml_get_doc_entity,
        xml_get_parameter_entity, xml_get_predefined_entity, xml_new_cdata_block, xml_new_char_ref,
        xml_new_doc, xml_new_doc_comment, xml_new_doc_node, xml_new_doc_pi, xml_new_doc_text,
        xml_new_dtd, xml_new_ns, xml_new_ns_prop, xml_new_reference, xml_text_concat,
    },
    uri::{build_uri, canonic_path, path_to_uri},
};

use super::valid::{
    xml_add_notation_decl, xml_get_dtd_qelement_desc, xml_is_id, xml_is_ref,
    xml_valid_normalize_attribute_value, xml_validate_notation_decl,
};

/// Provides the public ID e.g. "-//SGMLSOURCE//DTD DEMO//EN"
///
/// Returns a xmlChar *
#[doc(alias = "xmlSAX2GetPublicId")]
pub fn xml_sax2_get_public_id(_ctx: &XmlParserCtxt) -> Option<String> {
    /* let ctxt: xmlParserCtxtPtr = ctx as xmlParserCtxtPtr; */
    None
}

/// Provides the system ID, basically URL or filename e.g.  
/// <http://www.sgmlsource.com/dtds/memo.dtd>
///
/// Returns a xmlChar *
#[doc(alias = "xmlSAX2GetSystemId")]
pub fn xml_sax2_get_system_id(ctx: &XmlParserCtxt) -> Option<String> {
    ctx.input()?.filename.clone()
}

/// Receive the document locator at startup, actually xmlDefaultSAXLocator
/// Everything is available on the context, so this is useless in our case.
#[doc(alias = "xmlSAX2SetDocumentLocator")]
pub fn xml_sax2_set_document_locator(_ctxt: &mut XmlParserCtxt, _loc: XmlSAXLocator) {
    /* let ctxt: xmlParserCtxtPtr = ctx as xmlParserCtxtPtr; */
}

/// Provide the line number of the current parsing point.
///
/// Returns an int
#[doc(alias = "xmlSAX2GetLineNumber")]
pub fn xml_sax2_get_line_number(ctx: &XmlParserCtxt) -> i32 {
    if ctx.input().is_none() {
        return 0;
    }
    ctx.input().unwrap().line
}

/// Provide the column number of the current parsing point.
///
/// Returns an int
#[doc(alias = "xmlSAX2GetColumnNumber")]
pub fn xml_sax2_get_column_number(ctx: &XmlParserCtxt) -> i32 {
    if ctx.input().is_none() {
        return 0;
    }
    ctx.input().unwrap().col
}

/// Is this document tagged standalone ?
///
/// Returns 1 if true
#[doc(alias = "xmlSAX2IsStandalone")]
pub fn xml_sax2_is_standalone(ctxt: &mut XmlParserCtxt) -> i32 {
    let Some(my_doc) = ctxt.my_doc else {
        return 0;
    };
    (my_doc.standalone == 1) as i32
}

/// Does this document has an internal subset
///
/// Returns 1 if true
#[doc(alias = "xmlSAX2HasInternalSubset")]
pub fn xml_sax2_has_internal_subset(ctxt: &mut XmlParserCtxt) -> i32 {
    let Some(my_doc) = ctxt.my_doc else {
        return 0;
    };
    my_doc.int_subset.is_some() as i32
}

/// Does this document has an external subset
///
/// Returns 1 if true
#[doc(alias = "xmlSAX2HasExternalSubset")]
pub fn xml_sax2_has_external_subset(ctxt: &mut XmlParserCtxt) -> i32 {
    let Some(my_doc) = ctxt.my_doc else {
        return 0;
    };
    my_doc.ext_subset.is_some() as i32
}

#[doc(alias = "xmlSAX2ErrMemory")]
fn xml_sax2_err_memory(ctxt: &mut XmlParserCtxt, msg: &str) {
    let mut schannel: Option<StructuredError> = None;
    const MSG: &str = "out of memory\n";

    ctxt.err_no = XmlParserErrors::XmlErrNoMemory as i32;
    if let Some(sax) = ctxt
        .sax
        .as_deref()
        .filter(|sax| sax.initialized == XML_SAX2_MAGIC as u32)
    {
        schannel = sax.serror;
    }
    __xml_raise_error!(
        schannel,
        ctxt.vctxt.error,
        ctxt.vctxt.user_data.clone(),
        ctxt as *mut XmlParserCtxt as _,
        None,
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
        Some(msg),
    );
    ctxt.err_no = XmlParserErrors::XmlErrNoMemory as i32;
    ctxt.instate = XmlParserInputState::XmlParserEOF;
    ctxt.disable_sax = 1;
}

/// Callback on internal subset declaration.
#[doc(alias = "xmlSAX2InternalSubset")]
pub fn xml_sax2_internal_subset(
    ctxt: &mut XmlParserCtxt,
    name: Option<&str>,
    external_id: Option<&str>,
    system_id: Option<&str>,
) {
    unsafe {
        let Some(mut my_doc) = ctxt.my_doc else {
            return;
        };
        let dtd = my_doc.get_int_subset();
        if let Some(mut dtd) = dtd {
            if ctxt.html != 0 {
                return;
            }
            dtd.unlink();
            xml_free_dtd(dtd);
            my_doc.int_subset = None;
        }
        my_doc.int_subset = xml_create_int_subset(ctxt.my_doc, name, external_id, system_id);
        if my_doc.int_subset.is_none() {
            xml_sax2_err_memory(ctxt, "xmlSAX2InternalSubset");
        }
    }
}

/// Callback on external subset declaration.
#[doc(alias = "xmlSAX2ExternalSubset")]
pub fn xml_sax2_external_subset(
    ctxt: &mut XmlParserCtxt,
    name: Option<&str>,
    external_id: Option<&str>,
    system_id: Option<&str>,
) {
    if (external_id.is_some() || system_id.is_some())
        && ((ctxt.validate || ctxt.loadsubset != 0) && (ctxt.well_formed && ctxt.my_doc.is_some()))
    {
        let mut consumed: u64;

        // Ask the Entity resolver to load the damn thing
        let Some(input) = ctxt
            .sax
            .as_deref_mut()
            .and_then(|sax| sax.resolve_entity)
            .and_then(|resolve_entity| resolve_entity(ctxt, external_id, system_id))
        else {
            return;
        };

        xml_new_dtd(ctxt.my_doc, name, external_id, system_id);

        // make sure we won't destroy the main document context

        // Try to fetch and parse the external subset.
        let oldinput_tab = replace(&mut ctxt.input_tab, Vec::with_capacity(5));
        let oldcharset = ctxt.charset;
        let oldencoding = ctxt.encoding.take();
        let oldprogressive: i32 = ctxt.progressive;
        ctxt.progressive = 0;
        ctxt.push_input(input);

        // On the fly encoding conversion if needed
        if ctxt.input().unwrap().length >= 4 {
            let enc = detect_encoding(&ctxt.content_bytes()[..4]);
            ctxt.switch_encoding(enc);
        }

        if let Some(input) = ctxt.input_mut() {
            if input.filename.is_none() {
                if let Some(system_id) = system_id {
                    let canonic = canonic_path(system_id);
                    input.filename = Some(canonic.into_owned());
                }
            }
            input.line = 1;
            input.col = 1;
            input.base += input.cur;
            input.cur = 0;
        }

        // let's parse that entity knowing it's an external subset.
        ctxt.parse_external_subset(external_id, system_id);

        // Free up the external entities
        while ctxt.input_tab.len() > 1 {
            ctxt.pop_input();
        }

        consumed = ctxt.input().unwrap().consumed;
        let buffered = ctxt.input().unwrap().offset_from_base();
        if buffered as u64 > u64::MAX - consumed {
            consumed = u64::MAX;
        } else {
            consumed += buffered as u64;
        }
        if consumed > u64::MAX - ctxt.sizeentities {
            ctxt.sizeentities = u64::MAX;
        } else {
            ctxt.sizeentities += consumed;
        }

        // Restore the parsing context of the main entity
        ctxt.input_tab = oldinput_tab;
        ctxt.charset = oldcharset;
        ctxt.encoding = oldencoding;
        ctxt.progressive = oldprogressive;
        // ctxt.wellFormed = oldwellFormed;
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
                None,
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
                Some($msg),
            );
            if !ctxt.is_null() {
                (*ctxt).well_formed = false;
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
pub fn xml_sax2_get_entity(ctxt: &mut XmlParserCtxt, name: &str) -> Option<XmlEntityPtr> {
    unsafe {
        if ctxt.in_subset == 0 {
            let ret = xml_get_predefined_entity(name);
            if ret.is_some() {
                return ret;
            }
        }
        if let Some(mut my_doc) = ctxt.my_doc.filter(|doc| doc.standalone == 1) {
            if ctxt.in_subset == 2 {
                my_doc.standalone = 0;
                let ret = xml_get_doc_entity(Some(my_doc), name);
                my_doc.standalone = 1;
                ret
            } else {
                let mut ret = xml_get_doc_entity(Some(my_doc), name);
                if ret.is_none() {
                    my_doc.standalone = 0;
                    ret = xml_get_doc_entity(Some(my_doc), name);
                    if ret.is_some() {
                        xml_fatal_err_msg!(
                            ctxt,
                            XmlParserErrors::XmlErrNotStandalone,
                            "Entity({}) document marked standalone but requires external subset\n",
                            name
                        );
                    }
                    my_doc.standalone = 1;
                }
                ret
            }
        } else {
            xml_get_doc_entity(ctxt.my_doc, name)
        }
    }
}

/// Get a parameter entity by name
///
/// Returns the xmlEntityPtr if found.
#[doc(alias = "xmlSAX2GetParameterEntity")]
pub fn xml_sax2_get_parameter_entity(ctxt: &mut XmlParserCtxt, name: &str) -> Option<XmlEntityPtr> {
    xml_get_parameter_entity(ctxt.my_doc.unwrap(), name)
}

/// The entity loader, to control the loading of external entities,
/// the application can either:
///    - override this xmlSAX2ResolveEntity() callback in the SAX block
///    - or better use the xmlSetExternalEntityLoader() function to
///      set up it's own entity resolution routine
///
/// Returns the xmlParserInputPtr if inlined or NULL for DOM behaviour.
#[doc(alias = "xmlSAX2ResolveEntity")]
pub fn xml_sax2_resolve_entity(
    ctxt: &mut XmlParserCtxt,
    public_id: Option<&str>,
    system_id: Option<&str>,
) -> Option<XmlParserInput> {
    let base = if let Some(input) = ctxt.input() {
        input
            .filename
            .as_deref()
            .or(ctxt.directory.as_deref())
            .map(|b| b.to_owned())
    } else {
        ctxt.directory.as_deref().map(|d| d.to_owned())
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
                None,
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
                Some(format!($msg, $str1).as_str()),
            );
        }
    };
}

/// An entity definition has been parsed
#[doc(alias = "xmlSAX2EntityDecl")]
pub fn xml_sax2_entity_decl(
    ctxt: &mut XmlParserCtxt,
    name: &str,
    typ: XmlEntityType,
    public_id: Option<&str>,
    system_id: Option<&str>,
    content: Option<&str>,
) {
    unsafe {
        if ctxt.in_subset == 1 {
            let ent = xml_add_doc_entity(
                ctxt.my_doc.unwrap(),
                name,
                typ,
                public_id,
                system_id,
                content,
            );
            if ent.is_none() && ctxt.pedantic != 0 {
                xml_warn_msg!(
                    ctxt,
                    XmlParserErrors::XmlWarEntityRedefined,
                    "Entity({}) already defined in the internal subset\n",
                    name
                );
            }
            if let Some(mut ent) = ent.filter(|ent| ent.uri.is_none()) {
                if let Some(system_id) = system_id {
                    let base = if let Some(input) = ctxt.input() {
                        input
                            .filename
                            .as_deref()
                            .or(ctxt.directory.as_deref())
                            .map(|b| b.to_owned())
                    } else {
                        ctxt.directory.clone()
                    };

                    ent.uri = base
                        .and_then(|base| build_uri(system_id, &base))
                        .map(|base| base.into_boxed_str());
                }
            }
        } else if ctxt.in_subset == 2 {
            let ent = xml_add_dtd_entity(
                ctxt.my_doc.unwrap(),
                name,
                typ,
                public_id,
                system_id,
                content,
            );
            if ent.is_none() && ctxt.pedantic != 0 {
                if let Some(warning) = ctxt.sax.as_deref_mut().and_then(|sax| sax.warning) {
                    warning(
                        ctxt.user_data.clone(),
                        format!("Entity({name}) already defined in the external subset\n",)
                            .as_str(),
                    );
                }
            }
            if let Some(mut ent) = ent.filter(|ent| ent.uri.is_none()) {
                if let Some(system_id) = system_id {
                    let base = if let Some(input) = ctxt.input() {
                        input
                            .filename
                            .as_deref()
                            .or(ctxt.directory.as_deref())
                            .map(|b| b.to_owned())
                    } else {
                        ctxt.directory.clone()
                    };

                    ent.uri = base
                        .and_then(|base| build_uri(system_id, &base))
                        .map(|base| base.into_boxed_str());
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
                    None,
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
                    Some($msg),
                );
                (*ctxt).valid = 0;
            } else {
                __xml_raise_error!(
                    schannel,
                    None,
                    None,
                    ctxt as _,
                    None,
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
                    Some($msg),
                );
            }
        }
    };
}

/// An attribute definition has been parsed
#[doc(alias = "xmlSAX2AttributeDecl")]
pub fn xml_sax2_attribute_decl(
    ctxt: &mut XmlParserCtxt,
    elem: &str,
    fullname: &str,
    typ: XmlAttributeType,
    def: XmlAttributeDefault,
    default_value: Option<&str>,
    tree: Option<Box<XmlEnumeration>>,
) {
    unsafe {
        let Some(my_doc) = ctxt.my_doc else {
            return;
        };

        if fullname == "xml:id" && typ != XmlAttributeType::XmlAttributeID {
            // Raise the error but keep the validity flag
            let tmp: i32 = ctxt.valid;
            xml_err_valid!(
                ctxt,
                XmlParserErrors::XmlDTDXmlidType,
                "xml:id : attribute type should be ID\n"
            );
            ctxt.valid = tmp;
        }
        // TODO: optimize name/prefix allocation
        let (prefix, name) = split_qname(&mut *ctxt, fullname);
        ctxt.vctxt.valid = 1;
        let attr = if ctxt.in_subset == 1 {
            ctxt.add_attribute_decl(
                my_doc.int_subset,
                elem,
                name,
                prefix,
                typ,
                def,
                default_value,
                tree,
            )
        } else if ctxt.in_subset == 2 {
            ctxt.add_attribute_decl(
                my_doc.ext_subset,
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
            if ctxt.vctxt.valid == 0 {
                ctxt.valid = 0;
            }
            if ctxt.validate && ctxt.well_formed && my_doc.int_subset.is_some() {
                if let Some(attr) = attr {
                    ctxt.valid &= ctxt.validate_attribute_decl(ctxt.my_doc.unwrap(), attr);
                }
            }
        }
    }
}

/// An element definition has been parsed
#[doc(alias = "xmlSAX2ElementDecl")]
#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn xml_sax2_element_decl(
    ctxt: &mut XmlParserCtxt,
    name: &str,
    typ: Option<XmlElementTypeVal>,
    content: Option<Rc<RefCell<XmlElementContent>>>,
) {
    unsafe {
        let Some(my_doc) = ctxt.my_doc else {
            return;
        };

        let elem = if ctxt.in_subset == 1 {
            ctxt.add_element_decl(my_doc.int_subset, name, typ, content)
        } else if ctxt.in_subset == 2 {
            ctxt.add_element_decl(my_doc.ext_subset, name, typ, content)
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
                ctxt.valid = 0;
            }
            if ctxt.validate && ctxt.well_formed && my_doc.int_subset.is_some() {
                ctxt.valid &= ctxt.validate_element_decl(ctxt.my_doc.unwrap(), elem);
            }
        }
    }
}

/// What to do when a notation declaration has been parsed.
#[doc(alias = "xmlSAX2NotationDecl")]
pub fn xml_sax2_notation_decl(
    ctxt: &mut XmlParserCtxt,
    name: &str,
    public_id: Option<&str>,
    system_id: Option<&str>,
) {
    unsafe {
        let Some(mut my_doc) = ctxt.my_doc else {
            return;
        };
        let have_int_subset = my_doc.int_subset.is_some();
        let nota = if public_id.is_none() && system_id.is_none() {
            xml_fatal_err_msg!(
                ctxt,
                XmlParserErrors::XmlErrNotationProcessing,
                "SAX.xmlSAX2NotationDecl({}) externalID or PublicID missing\n",
                name
            );
            return;
        } else if ctxt.in_subset == 1 {
            xml_add_notation_decl(
                Some(&mut ctxt.vctxt),
                my_doc.int_subset.as_deref_mut(),
                name,
                public_id,
                system_id,
            )
        } else if ctxt.in_subset == 2 {
            xml_add_notation_decl(
                Some(&mut ctxt.vctxt),
                my_doc.ext_subset.as_deref_mut(),
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
                ctxt.valid = 0;
            }
            if ctxt.validate && ctxt.well_formed && have_int_subset {
                ctxt.valid &= xml_validate_notation_decl(&mut ctxt.vctxt, None, nota);
            }
        }
    }
}

/// What to do when an unparsed entity declaration is parsed
#[doc(alias = "xmlSAX2UnparsedEntityDecl")]
pub fn xml_sax2_unparsed_entity_decl(
    ctxt: &mut XmlParserCtxt,
    name: &str,
    public_id: Option<&str>,
    system_id: Option<&str>,
    notation_name: Option<&str>,
) {
    unsafe {
        if ctxt.in_subset == 1 {
            let ent = xml_add_doc_entity(
                ctxt.my_doc.unwrap(),
                name,
                XmlEntityType::XmlExternalGeneralUnparsedEntity,
                public_id,
                system_id,
                notation_name,
            );
            if ent.is_none() && ctxt.pedantic != 0 {
                if let Some(warning) = ctxt.sax.as_deref_mut().and_then(|sax| sax.warning) {
                    warning(
                        ctxt.user_data.clone(),
                        format!("Entity({name}) already defined in the internal subset\n").as_str(),
                    )
                }
            }
            if let Some(mut ent) = ent.filter(|ent| ent.uri.is_none()) {
                if let Some(system_id) = system_id {
                    let base = if let Some(input) = ctxt.input() {
                        input.filename.as_deref().or(ctxt.directory.as_deref())
                    } else {
                        ctxt.directory.as_deref()
                    };

                    ent.uri = base
                        .and_then(|base| build_uri(system_id, base))
                        .map(|base| base.into_boxed_str());
                }
            }
        } else if ctxt.in_subset == 2 {
            let ent = xml_add_dtd_entity(
                ctxt.my_doc.unwrap(),
                name,
                XmlEntityType::XmlExternalGeneralUnparsedEntity,
                public_id,
                system_id,
                notation_name,
            );
            if ent.is_none() && ctxt.pedantic != 0 {
                if let Some(warning) = ctxt.sax.as_deref_mut().and_then(|sax| sax.warning) {
                    warning(
                        ctxt.user_data.clone(),
                        format!("Entity({name}) already defined in the external subset\n").as_str(),
                    )
                }
            }

            if let Some(mut ent) = ent.filter(|ent| ent.uri.is_none()) {
                if let Some(system_id) = system_id {
                    let base = if let Some(input) = ctxt.input() {
                        input
                            .filename
                            .as_deref()
                            .or(ctxt.directory.as_deref())
                            .map(|b| b.to_owned())
                    } else {
                        ctxt.directory.clone()
                    };

                    ent.uri = base
                        .and_then(|base| build_uri(system_id, &base))
                        .map(|base| base.into_boxed_str());
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
}

/// called when the document start being processed.
#[doc(alias = "xmlSAX2StartDocument")]
pub fn xml_sax2_start_document(ctxt: &mut XmlParserCtxt) {
    if ctxt.html != 0 {
        #[cfg(feature = "html")]
        {
            if ctxt.my_doc.is_none() {
                ctxt.my_doc = html_new_doc_no_dtd(None, None);
            }
            let Some(mut my_doc) = ctxt.my_doc else {
                xml_sax2_err_memory(ctxt, "xmlSAX2StartDocument");
                return;
            };
            my_doc.properties = XmlDocProperties::XmlDocHTML as i32;
            my_doc.parse_flags = ctxt.options;
        }
        #[cfg(not(feature = "html"))]
        {
            generic_error!("libxml2 built without HTML support\n",);
            ctxt.err_no = XmlParserErrors::XmlErrInternalError as i32;
            ctxt.instate = XmlParserInputState::XmlParserEOF;
            ctxt.disable_sax = 1;
            return;
        }
    } else {
        let doc = xml_new_doc(ctxt.version.as_deref());
        ctxt.my_doc = doc;
        if let Some(mut doc) = doc {
            doc.properties = 0;
            if ctxt.options & XmlParserOption::XmlParseOld10 as i32 != 0 {
                doc.properties |= XmlDocProperties::XmlDocOld10 as i32;
            }
            doc.parse_flags = ctxt.options;
            doc.encoding = ctxt.encoding().map(|e| e.to_owned());
            doc.standalone = ctxt.standalone;
        } else {
            xml_sax2_err_memory(ctxt, "xmlSAX2StartDocument");
            return;
        }
    }
    if let Some(input) = ctxt.input() {
        if let Some(mut my_doc) = ctxt.my_doc.filter(|doc| doc.url.is_none()) {
            if let Some(filename) = input.filename.as_deref() {
                let url = path_to_uri(filename);
                my_doc.url = Some(url.into_owned());
            }
        }
    }
}

/// called when the document end has been detected.
#[doc(alias = "xmlSAX2EndDocument")]
pub fn xml_sax2_end_document(ctxt: &mut XmlParserCtxt) {
    let Some(mut my_doc) = ctxt.my_doc else {
        return;
    };
    #[cfg(feature = "libxml_valid")]
    if ctxt.validate && ctxt.well_formed && my_doc.int_subset.is_some() {
        ctxt.valid &= xml_validate_document_final(&mut ctxt.vctxt, my_doc);
    }

    // Grab the encoding if it was added on-the-fly
    if ctxt.encoding.is_some() && my_doc.encoding.is_none() {
        if let Some(enc) = ctxt.encoding.take() {
            my_doc.encoding = Some(enc);
        }
    }
    if !ctxt.input_tab.is_empty()
        && ctxt.input_tab[0].encoding.is_some()
        && my_doc.encoding.is_none()
    {
        my_doc.encoding = ctxt.input_tab[0].encoding.clone();
    }
    if ctxt.charset != XmlCharEncoding::None && my_doc.charset == XmlCharEncoding::None {
        my_doc.charset = ctxt.charset;
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
                None,
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
                Some($msg),
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
                None,
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
                Some($msg),
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
    ctxt: &mut XmlParserCtxt,
    fullname: &str,
    value: Option<&str>,
    prefix: Option<&str>,
) {
    #[cfg(feature = "html")]
    use crate::html::tree::html_is_boolean_attr;
    use crate::{parser::XML_SKIP_IDS, uri::XmlURI};

    unsafe {
        let (ns, name) = if ctxt.html != 0 {
            (None, fullname)
        } else {
            // Split the full name into a namespace prefix and the tag name
            let (prefix, mut local) = split_qname(ctxt, fullname);
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

        let value = match value {
            #[cfg(feature = "html")]
            None if ctxt.html != 0 && html_is_boolean_attr(fullname) => Some(fullname.to_owned()),
            #[cfg(feature = "libxml_valid")]
            Some(value) => {
                // Do the last stage of the attribute normalization
                // Needed for HTML too:
                //   http://www.w3.org/TR/html4/types.html#h-6.2
                ctxt.vctxt.valid = 1;
                let val = ctxt.normalize_attribute_value(
                    ctxt.my_doc.unwrap(),
                    ctxt.node.unwrap(),
                    fullname,
                    value,
                );
                if ctxt.vctxt.valid != 1 {
                    ctxt.valid = 0;
                }
                if let Some(val) = val {
                    Some(val.into_owned())
                } else {
                    Some(value.to_owned())
                }
            }
            value => value.map(|value| value.to_owned()),
        };

        // Check whether it's a namespace definition
        if ctxt.html == 0 && ns.is_none() && name == "xmlns" {
            let mut val = None;

            if !ctxt.replace_entities {
                ctxt.depth += 1;
                let value = value.as_deref().and_then(|val| {
                    ctxt.string_decode_entities(val, XML_SUBSTITUTE_REF as _, '\0', '\0', '\0')
                });
                ctxt.depth -= 1;
                let Some(value) = value else {
                    xml_sax2_err_memory(ctxt, "xmlSAX2StartElement");
                    return;
                };
                val = Some(value);
            }

            if let Some(val) = val.as_deref().filter(|val| !val.is_empty()) {
                if let Some(uri) = XmlURI::parse(val) {
                    if uri.scheme.is_none() {
                        if let Some(warning) = ctxt.sax.as_deref_mut().and_then(|sax| sax.warning) {
                            warning(
                                ctxt.user_data.clone(),
                                format!("xmlns: URI {} is not absolute\n", val).as_str(),
                            );
                        }
                    }
                } else if let Some(warning) = ctxt.sax.as_deref_mut().and_then(|sax| sax.warning) {
                    warning(
                        ctxt.user_data.clone(),
                        format!("xmlns: {} not a valid URI\n", val).as_str(),
                    );
                }
            }

            // a default namespace definition
            let nsret = xml_new_ns(ctxt.node, val.as_deref(), None);

            // Validate also for namespace decls, they are attributes from an XML-1.0 perspective
            #[cfg(feature = "libxml_valid")]
            if nsret.is_some() && ctxt.validate && ctxt.well_formed {
                if let Some(my_doc) = ctxt.my_doc.filter(|doc| doc.int_subset.is_some()) {
                    ctxt.valid &= ctxt.validate_one_namespace(
                        my_doc,
                        ctxt.node.unwrap(),
                        prefix,
                        nsret.unwrap(),
                        val.as_deref().unwrap(),
                    );
                }
            }
            return;
        }
        if ctxt.html != 0 && ns == Some("xmlns") {
            let mut val = None;
            if !ctxt.replace_entities {
                ctxt.depth += 1;
                let value = value.as_deref().and_then(|val| {
                    ctxt.string_decode_entities(val, XML_SUBSTITUTE_REF as i32, '\0', '\0', '\0')
                });
                ctxt.depth -= 1;
                let Some(value) = value else {
                    xml_sax2_err_memory(ctxt, "xmlSAX2StartElement");
                    return;
                };
                val = Some(value);
            }

            if val.as_deref().is_none_or(|val| val.is_empty()) {
                xml_ns_err_msg!(
                    ctxt,
                    XmlParserErrors::XmlNsErrEmpty,
                    "Empty namespace name for prefix {}\n",
                    name
                );
            }
            if let Some(val) = val
                .as_deref()
                .filter(|val| ctxt.pedantic != 0 && !val.is_empty())
            {
                if let Some(uri) = XmlURI::parse(val) {
                    if uri.scheme.is_none() {
                        xml_ns_warn_msg!(
                            ctxt,
                            XmlParserErrors::XmlWarNsURIRelative,
                            "xmlns:{}: URI {} is not absolute\n",
                            name,
                            value.as_deref().unwrap()
                        );
                    }
                } else {
                    xml_ns_warn_msg!(
                        ctxt,
                        XmlParserErrors::XmlWarNsURI,
                        "xmlns:{}: {} not a valid URI\n",
                        name,
                        value.as_deref().unwrap()
                    );
                }
            }

            // a standard namespace definition
            let nsret = xml_new_ns(ctxt.node, val.as_deref(), Some(name));
            #[cfg(feature = "libxml_valid")]
            // Validate also for namespace decls, they are attributes from an XML-1.0 perspective
            if nsret.is_some() && ctxt.validate && ctxt.well_formed {
                if let Some(my_doc) = ctxt.my_doc.filter(|doc| doc.int_subset.is_some()) {
                    ctxt.valid &= ctxt.validate_one_namespace(
                        my_doc,
                        ctxt.node.unwrap(),
                        prefix,
                        nsret.unwrap(),
                        value.as_deref().unwrap(),
                    );
                }
            }
            return;
        }

        let namespace = if let Some(ns) = ns {
            let namespace = ctxt.node.unwrap().search_ns(ctxt.my_doc, Some(ns));

            if let Some(namespace) = namespace {
                let mut prop = ctxt.node.unwrap().properties;
                while let Some(now) = prop {
                    if Some(name) == now.name().as_deref()
                        && now
                            .ns
                            .is_some_and(|ns| namespace == ns || namespace.href() == ns.href())
                    {
                        xml_ns_err_msg!(
                            ctxt,
                            XmlParserErrors::XmlErrAttributeRedefined,
                            "Attribute {} in {} redefined\n",
                            name,
                            namespace.href().unwrap().into_owned()
                        );
                        ctxt.well_formed = false;
                        if ctxt.recovery == 0 {
                            ctxt.disable_sax = 1;
                        }
                        return;
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
        let Some(mut ret) = xml_new_ns_prop(ctxt.node, namespace, name, None) else {
            return;
        };

        if !ctxt.replace_entities && ctxt.html == 0 {
            ret.children = ctxt
                .my_doc
                .and_then(|doc| doc.get_node_list(value.as_deref().unwrap()));
            let mut tmp = ret.children();
            while let Some(mut now) = tmp {
                now.set_parent(Some(ret.into()));
                if now.next().is_none() {
                    ret.set_last(Some(now));
                }
                tmp = now.next();
            }
        } else if let Some(value) = value.as_deref() {
            ret.children = xml_new_doc_text(ctxt.my_doc, Some(value));
            ret.last = ret.children;
            if let Some(mut children) = ret.children() {
                children.set_parent(Some(ret.into()));
            }
        }

        match ctxt.my_doc {
            #[cfg(feature = "libxml_valid")]
            Some(my_doc)
                if ctxt.html == 0
                    && ctxt.validate
                    && ctxt.well_formed
                    && my_doc.int_subset.is_some() =>
            {
                // If we don't substitute entities, the validation should be
                // done on a value with replaced entities anyway.
                if !ctxt.replace_entities {
                    ctxt.depth += 1;
                    let val = value.as_deref().and_then(|value| {
                        ctxt.string_decode_entities(
                            value,
                            XML_SUBSTITUTE_REF as i32,
                            '\0',
                            '\0',
                            '\0',
                        )
                    });
                    ctxt.depth -= 1;

                    if let Some(mut val) = val {
                        // Do the last stage of the attribute normalization
                        // It need to be done twice ... it's an extra burden related
                        // to the ability to keep xmlSAX2References in attributes
                        if let Some(nvalnorm) = xml_valid_normalize_attribute_value(
                            my_doc,
                            ctxt.node.unwrap(),
                            fullname,
                            &val,
                        ) {
                            val = nvalnorm.into_owned();
                        }

                        ctxt.valid &= ctxt.validate_one_attribute(
                            my_doc,
                            ctxt.node.unwrap(),
                            Some(ret),
                            &val,
                        );
                    } else {
                        ctxt.valid &= ctxt.validate_one_attribute(
                            my_doc,
                            ctxt.node.unwrap(),
                            Some(ret),
                            value.as_deref().unwrap(),
                        );
                    }
                } else {
                    ctxt.valid &= ctxt.validate_one_attribute(
                        my_doc,
                        ctxt.node.unwrap(),
                        Some(ret),
                        value.as_deref().unwrap(),
                    );
                }
            }
            _ => {
                // Don't create IDs containing entity references
                if ctxt.loadsubset & XML_SKIP_IDS as i32 == 0
                    && ((!ctxt.replace_entities && ctxt.external != 2)
                        || (ctxt.replace_entities && ctxt.in_subset == 0))
                    && ret
                        .children()
                        .filter(|c| {
                            matches!(c.element_type(), XmlElementType::XmlTextNode)
                                && c.next().is_none()
                        })
                        .is_some()
                {
                    let node = XmlNodePtr::try_from(ret.children().unwrap()).unwrap();
                    let content = node.content.as_deref().unwrap();
                    // when validating, the ID registration is done at the attribute
                    // validation level. Otherwise we have to do specific handling here.
                    if fullname == "xml:id" {
                        // Add the xml:id value
                        //
                        // Open issue: normalization of the value.
                        if validate_ncname::<true>(content).is_err() {
                            xml_err_valid!(
                                ctxt,
                                XmlParserErrors::XmlDTDXmlidValue,
                                "xml:id : attribute value {} is not an NCName\n",
                                content
                            );
                        }
                        ctxt.add_id(ctxt.my_doc.unwrap(), content, ret);
                    } else if xml_is_id(ctxt.my_doc, ctxt.node, Some(ret)) != 0 {
                        ctxt.add_id(ctxt.my_doc.unwrap(), content, ret);
                    } else if xml_is_ref(ctxt.my_doc, ctxt.node, Some(ret)) != 0 {
                        ctxt.add_ref(ctxt.my_doc.unwrap(), content, ret);
                    }
                }
            }
        }
    }
}

/// Check defaulted attributes from the DTD
#[doc(alias = "xmlCheckDefaultedAttributes")]
#[cfg(any(feature = "sax1", feature = "html", feature = "libxml_writer",))]
unsafe fn xml_check_defaulted_attributes(
    ctxt: &mut XmlParserCtxt,
    name: &str,
    prefix: Option<&str>,
    atts: &[(String, Option<String>)],
) {
    use crate::parser::XML_COMPLETE_ATTRS;

    unsafe {
        use crate::parser::build_qname;

        let my_doc = ctxt.my_doc.unwrap();
        let decls = [
            xml_get_dtd_qelement_desc(my_doc.int_subset, name, prefix),
            xml_get_dtd_qelement_desc(my_doc.ext_subset, name, prefix),
        ];
        for decl in decls {
            let Some(elem_decl) = decl else {
                continue;
            };
            let mut attr = elem_decl.attributes;
            // Check against defaulted attributes from the external subset
            // if the document is stamped as standalone
            if my_doc.standalone == 1 && my_doc.ext_subset.is_some() && ctxt.validate {
                while let Some(cur_attr) = attr {
                    if cur_attr.default_value.is_some()
                        && my_doc.ext_subset.and_then(|dtd| {
                            dtd.get_qattr_desc(
                                cur_attr.elem.as_deref().unwrap(),
                                cur_attr.name().as_deref().unwrap(),
                                cur_attr.prefix.as_deref(),
                            )
                        }) == Some(cur_attr)
                        && my_doc
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
                        let fulln = cur_attr
                            .prefix
                            .as_deref()
                            .map(|prefix| format!("{prefix}:{}", cur_attr.name.as_deref().unwrap()))
                            .unwrap_or_else(|| cur_attr.name.clone().unwrap());

                        // Check that the attribute is not declared in the serialization
                        if !atts.iter().any(|(att, _)| att.as_str() == fulln) {
                            xml_err_valid!(
                                ctxt,
                                XmlParserErrors::XmlDTDStandaloneDefaulted,
                                "standalone: attribute {} on {} defaulted from external subset\n",
                                fulln,
                                cur_attr.elem.as_deref().unwrap()
                            );
                        }
                    }
                    attr = cur_attr.nexth;
                }
            }

            // Actually insert defaulted values when needed
            let mut attr = elem_decl.attributes;
            while let Some(cur_attr) = attr {
                // Make sure that attributes redefinition occurring in the
                // internal subset are not overridden by definitions in the external subset.
                if let Some(def) = cur_attr.default_value.as_deref() {
                    // the element should be instantiated in the tree if:
                    //  - this is a namespace prefix
                    //  - the user required for completion in the tree
                    //    like XSLT
                    //  - there isn't already an attribute definition
                    //    in the internal subset overriding it.
                    if cur_attr.prefix.as_deref() == Some("xmlns")
                        || (cur_attr.prefix.is_none()
                            && cur_attr.name().as_deref() == Some("xmlns"))
                        || ctxt.loadsubset & XML_COMPLETE_ATTRS as i32 != 0
                    {
                        let tst = my_doc.int_subset.and_then(|dtd| {
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
                                xml_sax2_attribute_internal(ctxt, &fulln, Some(def), prefix);
                            }
                        }
                    }
                }
                attr = cur_attr.nexth;
            }
        }
    }
}

/// called when an opening tag has been processed.
#[doc(alias = "xmlSAX2StartElement")]
#[cfg(any(feature = "sax1", feature = "html", feature = "libxml_writer",))]
pub fn xml_sax2_start_element(
    ctxt: &mut XmlParserCtxt,
    fullname: &str,
    atts: &[(String, Option<String>)],
) {
    use crate::parser::XML_VCTXT_DTD_VALIDATED;

    unsafe {
        use crate::tree::XmlGenericNodePtr;

        let Some(mut my_doc) = ctxt.my_doc else {
            return;
        };
        let mut parent: Option<XmlGenericNodePtr> = ctxt.node.map(|node| node.into());

        // First check on validity:
        if ctxt.validate
            && my_doc.ext_subset.is_none()
            && my_doc.int_subset.is_none_or(|int_subset| {
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
            ctxt.validate = false;
        }

        let (prefix, name) = if ctxt.html != 0 {
            (None, fullname)
        } else {
            // Split the full name into a namespace prefix and the tag name
            split_qname(&mut *ctxt, fullname)
        };

        // Note : the namespace resolution is deferred until the end of the
        //        attributes parsing, since local namespace can be defined as
        //        an attribute at this level.
        let Some(mut ret) = xml_new_doc_node(ctxt.my_doc, None, name, None) else {
            xml_sax2_err_memory(ctxt, "xmlSAX2StartElement");
            return;
        };
        if let Some(children) = my_doc.children {
            if parent.is_none() {
                parent = Some(children);
            }
        } else {
            my_doc.add_child(ret.into());
        }
        ctxt.nodemem = -1;
        if ctxt.linenumbers != 0 && ctxt.input().is_some() {
            if (ctxt.input().unwrap().line as u32) < u16::MAX as u32 {
                ret.line = ctxt.input().unwrap().line as _;
            } else {
                ret.line = u16::MAX;
            }
        }

        // We are parsing a new node.
        if ctxt.node_push(ret) < 0 {
            ret.unlink();
            xml_free_node(ret);
            return;
        }

        // Link the child element
        if let Some(mut parent) = parent {
            if matches!(parent.element_type(), XmlElementType::XmlElementNode) {
                parent.add_child(ret.into());
            } else {
                parent.add_sibling(ret.into());
            }
        }

        if ctxt.html == 0 {
            // Insert all the defaulted attributes from the DTD especially namespaces
            if my_doc.int_subset.is_some() || my_doc.ext_subset.is_some() {
                xml_check_defaulted_attributes(ctxt, name, prefix, atts);
            }

            // process all the attributes whose name start with "xmlns"
            for (att, value) in atts {
                if att.starts_with("xmlns") {
                    xml_sax2_attribute_internal(ctxt, att, value.as_deref(), prefix);
                }
            }

            // Search the namespace, note that since the attributes have been
            // processed, the local namespaces are available.
            let mut ns = ret
                .search_ns(ctxt.my_doc, prefix)
                .or_else(|| parent.and_then(|parent| parent.search_ns(ctxt.my_doc, prefix)));
            if ns.is_none() {
                if let Some(prefix) = prefix {
                    ns = xml_new_ns(Some(ret), None, Some(prefix));
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
            if let Some(ns) = ns.filter(|ns| {
                ns.href()
                    .is_some_and(|href| !href.is_empty() || ns.prefix().is_some())
            }) {
                ret.set_ns(Some(ns));
            }
        }

        // process all the other attributes
        if ctxt.html != 0 {
            for (att, value) in atts {
                xml_sax2_attribute_internal(ctxt, att, value.as_deref(), None);
            }
        } else {
            for (att, value) in atts {
                if !att.starts_with("xmlns") {
                    xml_sax2_attribute_internal(ctxt, att, value.as_deref(), None);
                }
            }
        }

        #[cfg(feature = "libxml_valid")]
        {
            // If it's the Document root, finish the DTD validation and
            // check the document root element for validity
            if ctxt.validate && ctxt.vctxt.flags & XML_VCTXT_DTD_VALIDATED as u32 == 0 {
                let chk: i32 = ctxt.validate_dtd_final(ctxt.my_doc.unwrap());
                if chk <= 0 {
                    ctxt.valid = 0;
                }
                if chk < 0 {
                    ctxt.well_formed = false;
                }
                ctxt.valid &= ctxt.validate_root(ctxt.my_doc.unwrap());
                ctxt.vctxt.flags |= XML_VCTXT_DTD_VALIDATED as u32;
            }
        }
    }
}

/// called when the end of an element has been detected.
#[doc(alias = "xmlSAX2EndElement")]
#[cfg(any(feature = "sax1", feature = "html", feature = "libxml_writer",))]
pub fn xml_sax2_end_element(ctxt: &mut XmlParserCtxt, _name: &str) {
    let cur = ctxt.node;

    ctxt.nodemem = -1;

    #[cfg(feature = "libxml_valid")]
    if ctxt.validate && ctxt.well_formed {
        if let Some(my_doc) = ctxt.my_doc.filter(|doc| doc.int_subset.is_some()) {
            ctxt.valid &= ctxt.validate_one_element(my_doc, cur.map(|cur| cur.into()));
        }
    }

    // end of parsing of this node.
    ctxt.node_pop();
}

/// SAX2 callback when an element start has been detected by the parser.
/// It provides the namespace information for the element, as well as
/// the new namespace declarations on the element.
#[doc(alias = "xmlSAX2StartElementNs")]
pub fn xml_sax2_start_element_ns(
    ctxt: &mut XmlParserCtxt,
    localname: &str,
    prefix: Option<&str>,
    // I want to rename to `uri`, but it also appears as a local variable....
    orig_uri: Option<&str>,
    namespaces: &[(Option<String>, String)],
    nb_defaulted: usize,
    attributes: &[(String, Option<String>, Option<String>, String)],
) {
    unsafe {
        let mut my_doc = ctxt.my_doc.unwrap();
        let parent = ctxt.node;
        // First check on validity:
        if ctxt.validate
            && my_doc.ext_subset.is_none()
            && my_doc.int_subset.is_none_or(|int_subset| {
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
            ctxt.validate = false;
        }

        // Take care of the rare case of an undefined namespace prefix
        let mut lname = None;
        if orig_uri.is_none() {
            if let Some(prefix) = prefix {
                lname = Some(build_qname(localname, Some(prefix)));
            }
        }
        // allocate the node
        let mut ret = if let Some(mut ret) = ctxt.free_elems {
            ctxt.free_elems = ret.next().map(|n| XmlNodePtr::try_from(n).unwrap());
            ctxt.free_elems_nr -= 1;
            std::ptr::write(&mut *ret, XmlNode::default());
            ret.doc = Some(my_doc);
            ret.typ = XmlElementType::XmlElementNode;

            if let Some(lname) = lname {
                ret.name = lname.into_owned().into();
            } else {
                ret.name = localname.to_owned().into();
            }
            if let Some(register) = get_register_node_func() {
                register(ret.into());
            }
            ret
        } else {
            let ret = if let Some(lname) = lname {
                xml_new_doc_node(ctxt.my_doc, None, &lname, None)
            } else {
                xml_new_doc_node(ctxt.my_doc, None, localname, None)
            };
            let Some(ret) = ret else {
                xml_sax2_err_memory(ctxt, "xmlSAX2StartElementNs");
                return;
            };
            ret
        };
        if ctxt.linenumbers != 0 && ctxt.input().is_some() {
            if (ctxt.input().unwrap().line as u32) < u16::MAX as u32 {
                ret.line = ctxt.input().unwrap().line as _;
            } else {
                ret.line = u16::MAX;
            }
        }

        if parent.is_none() {
            my_doc.add_child(ret.into());
        }
        // Build the namespace list
        let mut last = None::<XmlNsPtr>;
        for (pref, uri) in namespaces {
            let Some(ns) = xml_new_ns(None, Some(uri.as_str()), pref.as_deref()) else {
                // any out of memory error would already have been raised
                // but we can't be guaranteed it's the actual error due to the
                // API, best is to skip in this case
                continue;
            };
            if let Some(mut l) = last {
                l.next = Some(ns);
                last = Some(ns);
            } else {
                ret.ns_def = Some(ns);
                last = Some(ns);
            }
            if orig_uri.is_some() && prefix == pref.as_deref() {
                ret.ns = Some(ns);
            }
            #[cfg(feature = "libxml_valid")]
            {
                if ctxt.html == 0
                    && ctxt.validate
                    && ctxt.well_formed
                    && my_doc.int_subset.is_some()
                {
                    ctxt.valid &= ctxt.validate_one_namespace(my_doc, ret, prefix, ns, uri);
                }
            }
        }
        ctxt.nodemem = -1;

        // We are parsing a new node.
        if ctxt.node_push(ret) < 0 {
            ret.unlink();
            xml_free_node(ret);
            return;
        }

        // Link the child element
        if let Some(mut parent) = parent {
            if matches!(parent.element_type(), XmlElementType::XmlElementNode) {
                parent.add_child(ret.into());
            } else {
                parent.add_sibling(ret.into());
            }
        }

        // Insert the defaulted attributes from the DTD only if requested:
        let mut nb_attributes = attributes.len();
        if nb_defaulted != 0 && ctxt.loadsubset & XML_COMPLETE_ATTRS as i32 == 0 {
            nb_attributes -= nb_defaulted;
        }

        // Search the namespace if it wasn't already found
        // Note that, if prefix is NULL, this searches for the default Ns
        if orig_uri.is_some() && ret.ns.is_none() {
            ret.ns = parent.and_then(|mut parent| parent.search_ns(ctxt.my_doc, prefix));
            if ret.ns.is_none() && prefix == Some("xml") {
                ret.ns = ret.search_ns(ctxt.my_doc, prefix);
            }
            if ret.ns.is_none() {
                if xml_new_ns(Some(ret), None, prefix).is_none() {
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
        for attr in attributes.iter().take(nb_attributes) {
            // Handle the rare case of an undefined attribute prefix
            if attr.1.is_some() && attr.2.is_none() {
                let lname = build_qname(&attr.0, attr.1.as_deref());
                xml_sax2_attribute_ns(ctxt, &lname, None, &attr.3);
                continue;
            }

            xml_sax2_attribute_ns(ctxt, &attr.0, attr.1.as_deref(), &attr.3);
        }

        #[cfg(feature = "libxml_valid")]
        {
            // If it's the Document root, finish the DTD validation and
            // check the document root element for validity
            if ctxt.validate && ctxt.vctxt.flags & XML_VCTXT_DTD_VALIDATED as u32 == 0 {
                let chk: i32 = ctxt.validate_dtd_final(ctxt.my_doc.unwrap());
                if chk <= 0 {
                    ctxt.valid = 0;
                }
                if chk < 0 {
                    ctxt.well_formed = false;
                }
                ctxt.valid &= ctxt.validate_root(ctxt.my_doc.unwrap());
                ctxt.vctxt.flags |= XML_VCTXT_DTD_VALIDATED as u32;
            }
        }
    }
}

/// Returns the newly allocated string or NULL if not needed or error
#[doc(alias = "xmlSAX2TextNode")]
fn xml_sax2_text_node(ctxt: &mut XmlParserCtxt, s: &str) -> Option<XmlNodePtr> {
    unsafe {
        // Allocate
        let ret = if let Some(mut ret) = ctxt.free_elems {
            ctxt.free_elems = ret.next.map(|node| XmlNodePtr::try_from(node).unwrap());
            ctxt.free_elems_nr -= 1;
            std::ptr::write(&mut *ret, XmlNode::default());
            ret.typ = XmlElementType::XmlTextNode;
            Some(ret)
        } else {
            XmlNodePtr::new(XmlNode::default())
        };
        let Some(mut ret) = ret else {
            xml_err_memory(Some(&mut *ctxt), Some("xmlSAX2Characters"));
            return None;
        };
        ret.typ = XmlElementType::XmlTextNode;
        ret.name = XML_STRING_TEXT.into();
        ret.content = Some(s.to_owned());

        if ctxt.linenumbers != 0 && ctxt.input().is_some() {
            if (ctxt.input().unwrap().line as u32) < u32::MAX {
                ret.line = ctxt.input().unwrap().line as _;
            } else {
                ret.line = u16::MAX;
                if ctxt.options & XmlParserOption::XmlParseBigLines as i32 != 0 {
                    ret.psvi = ctxt.input().unwrap().line as isize as *mut c_void;
                }
            }
        }

        if let Some(register) = get_register_node_func() {
            register(ret.into());
        }
        Some(ret)
    }
}

/// Remove the entities from an attribute value
///
/// Returns the newly allocated string or NULL if not needed or error
#[doc(alias = "xmlSAX2DecodeAttrEntities")]
#[cfg(feature = "libxml_valid")]
fn xml_sax2_decode_attr_entities(ctxt: &mut XmlParserCtxt, s: &str) -> Option<String> {
    let pos = s.find('&')?;
    let s = &s[pos..];
    ctxt.depth += 1;
    let ret = ctxt.string_decode_entities(s, XML_SUBSTITUTE_REF as i32, '\0', '\0', '\0');
    ctxt.depth -= 1;
    ret
}

/// Handle an attribute that has been read by the parser.
/// The default handling is to convert the attribute into an
/// DOM subtree and past it in a new xmlAttr element added to the element.
#[doc(alias = "xmlSAX2AttributeNs")]
unsafe fn xml_sax2_attribute_ns(
    ctxt: &mut XmlParserCtxt,
    localname: &str,
    prefix: Option<&str>,
    value: &str,
) {
    unsafe {
        let mut namespace = None;

        // Note: if prefix.is_null(), the attribute is not in the default namespace
        if let Some(prefix) = prefix {
            namespace = ctxt.node.unwrap().search_ns(ctxt.my_doc, Some(prefix));
        }

        // allocate the node
        let mut ret = if let Some(mut ret) = ctxt.free_attrs {
            ctxt.free_attrs = ret.next;
            ctxt.free_attrs_nr -= 1;
            std::ptr::write(&mut *ret, XmlAttr::default());
            ret.typ = XmlElementType::XmlAttributeNode;
            ret.parent = ctxt.node;
            ret.doc = ctxt.my_doc;
            ret.ns = namespace;
            ret.name = localname.into();

            // link at the end to preserve order, TODO speed up with a last
            if let Some(mut prev) = ctxt.node.unwrap().properties {
                while let Some(next) = prev.next {
                    prev = next;
                }
                prev.next = Some(ret);
                ret.prev = Some(prev);
            } else {
                ctxt.node.unwrap().properties = Some(ret);
            }

            if let Some(register) = get_register_node_func() {
                register(ret.into());
            }
            ret
        } else {
            let Some(ret) = xml_new_ns_prop(ctxt.node, namespace, localname, None) else {
                xml_err_memory(Some(&mut *ctxt), Some("xmlSAX2AttributeNs"));
                return;
            };
            ret
        };

        if !ctxt.replace_entities && ctxt.html == 0 {
            // We know that if there is an entity reference, then
            // the string has been dup'ed and terminates with 0
            // otherwise with ' or "
            ret.children = ctxt.my_doc.and_then(|doc| doc.get_node_list(value));
            let mut tmp = ret.children();
            while let Some(mut now) = tmp {
                now.set_document(ret.doc);
                now.set_parent(Some(ret.into()));
                if now.next().is_none() {
                    ret.set_last(Some(now));
                }
                tmp = now.next();
            }
        } else {
            let tmp = xml_sax2_text_node(ctxt, value);
            ret.children = tmp;
            ret.last = tmp;
            if let Some(mut tmp) = tmp {
                tmp.doc = ret.doc;
                tmp.set_parent(Some(ret.into()));
            }
        }

        match ctxt.my_doc {
            #[cfg(feature = "libxml_valid")]
            Some(my_doc)
                if ctxt.html == 0
                    && ctxt.validate
                    && ctxt.well_formed
                    && my_doc.int_subset.is_some() =>
            {
                // If we don't substitute entities, the validation should be
                // done on a value with replaced entities anyway.
                if !ctxt.replace_entities {
                    if let Some(mut dup) = xml_sax2_decode_attr_entities(ctxt, value) {
                        // dup now contains a string of the flattened attribute
                        // content with entities substituted. Check if we need to
                        // apply an extra layer of normalization.
                        // It need to be done twice ... it's an extra burden related
                        // to the ability to keep references in attributes
                        if ctxt.atts_special.is_some() {
                            let fullname = build_qname(localname, prefix);
                            ctxt.vctxt.valid = 1;
                            let nvalnorm = ctxt.normalize_attribute_value(
                                ctxt.my_doc.unwrap(),
                                ctxt.node.unwrap(),
                                &fullname,
                                &dup,
                            );
                            if ctxt.vctxt.valid != 1 {
                                ctxt.valid = 0;
                            }
                            if let Some(nvalnorm) = nvalnorm {
                                dup = nvalnorm.into_owned();
                            }
                        }

                        ctxt.valid &= ctxt.validate_one_attribute(
                            ctxt.my_doc.unwrap(),
                            ctxt.node.unwrap(),
                            Some(ret),
                            &dup,
                        );
                    } else {
                        ctxt.valid &= ctxt.validate_one_attribute(
                            ctxt.my_doc.unwrap(),
                            ctxt.node.unwrap(),
                            Some(ret),
                            value,
                        );
                    }
                } else {
                    // if entities already have been substituted, then
                    // the attribute as passed is already normalized
                    ctxt.valid &= ctxt.validate_one_attribute(
                        ctxt.my_doc.unwrap(),
                        ctxt.node.unwrap(),
                        Some(ret),
                        value,
                    );
                }
            }
            // Don't create IDs containing entity references
            _ => {
                if ctxt.loadsubset & XML_SKIP_IDS as i32 == 0
                    && ((!ctxt.replace_entities && ctxt.external != 2)
                        || (ctxt.replace_entities && ctxt.in_subset == 0))
                    && ret
                        .children()
                        .filter(|c| {
                            matches!(c.element_type(), XmlElementType::XmlTextNode)
                                && c.next().is_none()
                        })
                        .is_some()
                {
                    let node = XmlNodePtr::try_from(ret.children().unwrap()).unwrap();
                    let content = node.content.as_deref().unwrap();
                    // when validating, the ID registration is done at the attribute
                    // validation level. Otherwise we have to do specific handling here.
                    if prefix == ctxt.str_xml.as_deref() && localname == "id" {
                        // Add the xml:id value
                        //
                        // Open issue: normalization of the value.
                        #[cfg(any(feature = "sax1", feature = "html", feature = "libxml_writer",))]
                        {
                            #[cfg(feature = "libxml_valid")]
                            if validate_ncname::<true>(content).is_err() {
                                xml_err_valid!(
                                    ctxt,
                                    XmlParserErrors::XmlDTDXmlidValue,
                                    "xml:id : attribute value {} is not an NCName\n",
                                    content
                                );
                            }
                        }
                        ctxt.add_id(ctxt.my_doc.unwrap(), content, ret);
                    } else if xml_is_id(ctxt.my_doc, ctxt.node, Some(ret)) != 0 {
                        ctxt.add_id(ctxt.my_doc.unwrap(), content, ret);
                    } else if xml_is_ref(ctxt.my_doc, ctxt.node, Some(ret)) != 0 {
                        ctxt.add_ref(ctxt.my_doc.unwrap(), content, ret);
                    }
                }
            }
        }
    }
}

/// SAX2 callback when an element end has been detected by the parser.
/// It provides the namespace information for the element.
#[doc(alias = "xmlSAX2EndElementNs")]
pub fn xml_sax2_end_element_ns(
    ctxt: &mut XmlParserCtxt,
    _localname: &str,
    _prefix: Option<&str>,
    _uri: Option<&str>,
) {
    ctxt.nodemem = -1;

    #[cfg(feature = "libxml_valid")]
    if ctxt.validate && ctxt.well_formed {
        if let Some(my_doc) = ctxt.my_doc.filter(|doc| doc.int_subset.is_some()) {
            ctxt.valid &= ctxt.validate_one_element(my_doc, ctxt.node.map(|node| node.into()));
        }
    }

    // end of parsing of this node.
    ctxt.node_pop();
}

/// Called when an entity xmlSAX2Reference is detected.
#[doc(alias = "xmlSAX2Reference")]
pub fn xml_sax2_reference(ctxt: &mut XmlParserCtxt, name: &str) {
    unsafe {
        let ret = if name.starts_with('#') {
            xml_new_char_ref(ctxt.my_doc, name)
        } else {
            xml_new_reference(ctxt.my_doc, name)
        };
        if ctxt
            .node
            .is_none_or(|mut node| node.add_child(ret.unwrap().into()).is_none())
        {
            if let Some(ret) = ret {
                xml_free_node(ret);
            }
        }
    }
}

/// Append characters.
#[doc(alias = "xmlSAX2Text")]
unsafe fn xml_sax2_text(ctxt: &mut XmlParserCtxt, ch: &str, typ: XmlElementType) {
    unsafe {
        // Handle the data if any. If there is no child
        // add it as content, otherwise if the last child is text,
        // concatenate it, else create a new node of type text.
        if ctxt.node.is_none() {
            return;
        }
        let last_child = ctxt
            .node
            .unwrap()
            .last()
            .map(|l| XmlNodePtr::try_from(l).unwrap());

        // Here we needed an accelerator mechanism in case of very large elements.
        // Use an attribute in the structure !!!
        if let Some(mut last_child) = last_child {
            let coalesce_text = last_child.element_type() == typ
                && (!matches!(typ, XmlElementType::XmlTextNode)
                    || last_child.name == XML_STRING_TEXT);
            if coalesce_text && ctxt.nodemem != 0 {
                // The whole point of maintaining nodelen and nodemem,
                // xmlTextConcat is too costly, i.e. compute length,
                // reallocate a new buffer, move data, append ch. Here
                // We try to minimize realloc() uses and avoid copying
                // and recomputing length over and over.
                let (nodelen, overflowed) = ctxt.nodelen.overflowing_add(ch.len() as i32);
                if overflowed {
                    xml_sax2_err_memory(ctxt, "xmlSAX2Characters overflow prevented");
                    return;
                }
                if nodelen > XML_MAX_TEXT_LENGTH as i32
                    && ctxt.options & XmlParserOption::XmlParseHuge as i32 == 0
                {
                    xml_sax2_err_memory(ctxt, "xmlSAX2Characters: huge text node");
                    return;
                }
                let buffer = last_child.content.get_or_insert_default();
                buffer.push_str(ch);
                ctxt.nodelen = nodelen;
            } else if coalesce_text {
                if xml_text_concat(last_child, ch) != 0 {
                    xml_sax2_err_memory(ctxt, "xmlSAX2Characters");
                }
                if ctxt.node.unwrap().children().is_some() {
                    ctxt.nodelen = last_child.content.as_deref().unwrap().len() as i32;
                    ctxt.nodemem = ctxt.nodelen + 1;
                }
            } else {
                // Mixed content, first time
                let last_child = if matches!(typ, XmlElementType::XmlTextNode) {
                    let last_child = xml_sax2_text_node(ctxt, ch);
                    if let Some(mut last_child) = last_child {
                        last_child.doc = ctxt.my_doc;
                    }
                    last_child
                } else {
                    xml_new_cdata_block(ctxt.my_doc, ch)
                };
                if let Some(last_child) = last_child {
                    ctxt.node.unwrap().add_child(last_child.into());
                    if ctxt.node.unwrap().children().is_some() {
                        ctxt.nodelen = ch.len() as i32;
                        ctxt.nodemem = ch.len() as i32 + 1;
                    }
                }
            }
        } else {
            let last_child = if matches!(typ, XmlElementType::XmlTextNode) {
                xml_sax2_text_node(ctxt, ch)
            } else {
                xml_new_cdata_block(ctxt.my_doc, ch)
            };
            if let Some(mut last_child) = last_child {
                ctxt.node.unwrap().set_children(Some(last_child.into()));
                ctxt.node.unwrap().set_last(Some(last_child.into()));
                last_child.parent = ctxt.node.map(|node| node.into());
                last_child.doc = ctxt.node.unwrap().doc;
                ctxt.nodelen = ch.len() as i32;
                ctxt.nodemem = ch.len() as i32 + 1;
            } else {
                xml_sax2_err_memory(ctxt, "xmlSAX2Characters");
            }
        }
    }
}

/// Receiving some chars from the parser.
#[doc(alias = "xmlSAX2Characters")]
pub fn xml_sax2_characters(ctxt: &mut XmlParserCtxt, ch: &str) {
    unsafe {
        xml_sax2_text(ctxt, ch, XmlElementType::XmlTextNode);
    }
}

/// Receiving some ignorable whitespaces from the parser.
/// UNUSED: by default the DOM building will use xmlSAX2Characters
#[doc(alias = "xmlSAX2IgnorableWhitespace")]
pub fn xml_sax2_ignorable_whitespace(_ctx: &mut XmlParserCtxt, _ch: &str) {
    /* let ctxt: xmlParserCtxtPtr = ctx as xmlParserCtxtPtr; */
}

/// A processing instruction has been parsed.
#[doc(alias = "xmlSAX2ProcessingInstruction")]
pub fn xml_sax2_processing_instruction(ctxt: &mut XmlParserCtxt, target: &str, data: Option<&str>) {
    unsafe {
        let parent = ctxt.node;

        let Some(mut ret) = xml_new_doc_pi(ctxt.my_doc, target, data) else {
            return;
        };

        if ctxt.linenumbers != 0 && ctxt.input().is_some() {
            if (ctxt.input().unwrap().line as u32) < u16::MAX as u32 {
                ret.line = ctxt.input().unwrap().line as _;
            } else {
                ret.line = u16::MAX;
            }
        }
        let mut my_doc = ctxt.my_doc.unwrap();
        if ctxt.in_subset == 1 {
            my_doc.int_subset.unwrap().add_child(ret.into());
            return;
        } else if ctxt.in_subset == 2 {
            my_doc.ext_subset.unwrap().add_child(ret.into());
            return;
        }
        let Some(mut parent) = parent else {
            my_doc.add_child(ret.into());
            return;
        };
        if matches!(parent.element_type(), XmlElementType::XmlElementNode) {
            parent.add_child(ret.into());
        } else {
            parent.add_sibling(ret.into());
        }
    }
}

/// A xmlSAX2Comment has been parsed.
#[doc(alias = "xmlSAX2Comment")]
pub fn xml_sax2_comment(ctxt: &mut XmlParserCtxt, value: &str) {
    unsafe {
        let parent = ctxt.node;
        let Some(mut ret) = xml_new_doc_comment(ctxt.my_doc, value) else {
            return;
        };
        if ctxt.linenumbers != 0 && ctxt.input().is_some() {
            if (ctxt.input().unwrap().line as u32) < u16::MAX as u32 {
                ret.line = ctxt.input().unwrap().line as _;
            } else {
                ret.line = u16::MAX;
            }
        }

        let mut my_doc = ctxt.my_doc.unwrap();
        if ctxt.in_subset == 1 {
            my_doc.int_subset.unwrap().add_child(ret.into());
            return;
        } else if ctxt.in_subset == 2 {
            my_doc.ext_subset.unwrap().add_child(ret.into());
            return;
        }
        let Some(mut parent) = parent else {
            my_doc.add_child(ret.into());
            return;
        };
        if matches!(parent.element_type(), XmlElementType::XmlElementNode) {
            parent.add_child(ret.into());
        } else {
            parent.add_sibling(ret.into());
        }
    }
}

/// Called when a pcdata block has been parsed
#[doc(alias = "xmlSAX2CDataBlock")]
pub fn xml_sax2_cdata_block(ctxt: &mut XmlParserCtxt, value: &str) {
    unsafe {
        xml_sax2_text(ctxt, value, XmlElementType::XmlCDATASectionNode);
    }
}

static XML_SAX2_DEFAULT_VERSION_VALUE: AtomicI32 = AtomicI32::new(2);

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
pub fn xml_sax_default_version(version: i32) -> i32 {
    if version != 1 && version != 2 {
        return -1;
    }
    let ret = XML_SAX2_DEFAULT_VERSION_VALUE.load(Ordering::Relaxed);
    XML_SAX2_DEFAULT_VERSION_VALUE.store(version, Ordering::Relaxed);
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
pub fn xml_sax2_init_default_sax_handler(hdlr: &mut XmlSAXHandler, warning: i32) {
    if hdlr.initialized != 0 {
        return;
    }

    xml_sax_version(hdlr, XML_SAX2_DEFAULT_VERSION_VALUE.load(Ordering::Relaxed));
    if warning == 0 {
        hdlr.warning = None;
    } else {
        hdlr.warning = Some(parser_warning);
    }
}

/// Initialize the default HTML SAX2 handler
#[doc(alias = "xmlSAX2InitHtmlDefaultSAXHandler")]
#[cfg(feature = "html")]
pub fn xml_sax2_init_html_default_sax_handler(hdlr: &mut XmlSAXHandler) {
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
pub fn html_default_sax_handler_init() {}

/// Initialize the default SAX2 handler
#[doc(alias = "xmlDefaultSAXHandlerInit")]
#[deprecated = "This function is a no-op. Call xmlInitParser to initialize the library"]
pub fn xml_default_sax_handler_init() {}

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
