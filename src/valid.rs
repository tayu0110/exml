//! Provide methods and data structures for handling DTD validation.
//!
//! This module is based on `libxml/valid.h`, `valid.c`, and so on in `libxml2-v2.11.8`.
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
    borrow::Cow,
    cell::RefCell,
    collections::hash_map::Entry,
    os::raw::c_void,
    ptr::null_mut,
    rc::{Rc, Weak},
};
#[cfg(feature = "libxml_output")]
use std::{collections::HashMap, io::Write};

#[cfg(feature = "libxml_regexp")]
use crate::libxml::{xmlautomata::XmlAutomata, xmlregexp::XmlRegExecCtxt};
#[cfg(feature = "libxml_valid")]
use crate::tree::{XmlElementPtr, XmlGenericNodePtr, XmlNsPtr};
use crate::{
    chvalid::XmlCharValid,
    dom::elementdecl::{
        ElementContent, ElementContentOccur, ElementContentType, deep_clone_element_content,
    },
    error::{__xml_raise_error, XmlErrorDomain, XmlErrorLevel, XmlParserErrors},
    globals::{GenericError, GenericErrorContext, StructuredError},
    list::XmlList,
    parser::{XML_VCTXT_USE_PCTXT, build_qname, split_qname2},
    tree::{
        NodeCommon, XmlAttrPtr, XmlAttribute, XmlAttributeDefault, XmlAttributePtr,
        XmlAttributeType, XmlDocProperties, XmlDocPtr, XmlDtd, XmlDtdPtr, XmlElement,
        XmlElementContent, XmlElementContentOccur, XmlElementContentType, XmlElementType,
        XmlElementTypeVal, XmlEntityPtr, XmlEntityType, XmlEnumeration, XmlID, XmlNodePtr,
        XmlNotation, XmlRef, xml_free_attribute, xml_free_element, xml_free_node,
        xml_get_doc_entity, xml_new_doc_node,
    },
};

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
        let mut channel: Option<GenericError> = None;
        let mut data = None;

        if let Some(ctxt) = $ctxt {
            channel = ctxt.error;
            data = ctxt.user_data.clone();
        }
        __xml_raise_error!(
            None,
            channel,
            data,
            null_mut(),
            None,
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
            Some($msg),
        );
    };
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
        let schannel: Option<StructuredError> = None;
        let mut channel: Option<GenericError> = None;
        let mut data = None;

        if let Some(ctxt) = $ctxt {
            channel = (*ctxt).warning;
            data = (*ctxt).user_data.clone();
        }
        __xml_raise_error!(
            schannel,
            channel,
            data,
            null_mut(),
            $node,
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
            Some($msg),
        );
    };
}

// Validation state added for non-determinist content model.
//
// If regexp are enabled we can do continuous validation without the
// need of a tree to validate the content model. this is done in each
// callbacks.
// Each xmlValidState represent the validation state associated to the
// set of nodes currently open from the document root to the current element.
#[cfg(feature = "libxml_regexp")]
#[repr(C)]
pub struct XmlValidState {
    pub(crate) elem_decl: Option<XmlElementPtr>, /* pointer to the content model */
    pub(crate) node: XmlNodePtr,                 /* pointer to the current node */
    pub(crate) exec: Option<Box<XmlRegExecCtxt>>, /* regexp runtime */
}

/// Callback called when a validity error is found. This is a message
/// oriented function similar to an *printf function.
#[doc(alias = "xmlValidityErrorFunc")]
pub type XmlValidityErrorFunc = unsafe fn(ctx: *mut c_void, msg: *const i8);

/// Callback called when a validity warning is found. This is a message
/// oriented function similar to an *printf function.
#[doc(alias = "xmlValidityWarningFunc")]
pub type XmlValidityWarningFunc = unsafe fn(ctx: *mut c_void, msg: *const i8);

/// An xmlValidCtxt is used for error reporting when validating.
#[doc(alias = "xmlValidCtxt")]
#[repr(C)]
pub struct XmlValidCtxt {
    pub(crate) user_data: Option<GenericErrorContext>, /* user specific data block */
    pub error: Option<GenericError>,                   /* the callback in case of errors */
    pub warning: Option<GenericError>,                 /* the callback in case of warning */

    /* Node analysis stack used when validating within entities */
    pub(crate) node: Option<XmlNodePtr>, /* Current parsed Node */
    pub(crate) node_tab: Vec<XmlNodePtr>, /* array of nodes */

    pub(crate) flags: u32,             /* internal flags */
    pub(crate) doc: Option<XmlDocPtr>, /* the document */
    pub(crate) valid: i32,             /* temporary validity check result */

    // state state used for non-determinist content validation
    pub(crate) vstate_tab: Vec<XmlValidState>, /* array of validation states */

    #[cfg(feature = "libxml_regexp")]
    pub(crate) am: Option<XmlAutomata>, /* the automata */
    #[cfg(feature = "libxml_regexp")]
    pub(crate) state: usize, /* used to build the automata */
}

impl XmlValidCtxt {
    /// Allocate a validation context structure.
    ///
    /// Returns the new validation context structure
    #[doc(alias = "xmlNewValidCtxt")]
    #[cfg(feature = "libxml_valid")]
    pub fn new() -> XmlValidCtxt {
        XmlValidCtxt::default()
    }

    /// Search the DTD for the description of this element
    ///
    /// returns the xmlElementPtr if found or null_mut()
    #[doc(alias = "xmlGetDtdElementDesc2")]
    unsafe fn get_dtd_element_desc2(
        &mut self,
        mut dtd: XmlDtdPtr,
        mut name: &str,
        create: i32,
    ) -> Option<XmlElementPtr> {
        unsafe {
            if create == 0 {
                return None;
            }
            let mut prefix = None;
            if let Some((pref, local)) = split_qname2(name) {
                name = local;
                prefix = Some(pref);
            }
            let mut cur = dtd
                .elements
                .get(&(Cow::Borrowed(name), prefix.map(Cow::Borrowed)))
                .cloned();
            if cur.is_none() && create != 0 {
                let Some(res) = XmlElementPtr::new(XmlElement {
                    typ: XmlElementType::XmlElementDecl,
                    name: Some(name.to_owned()),
                    prefix: prefix.map(|pref| pref.to_owned()),
                    etype: XmlElementTypeVal::XmlElementTypeUndefined,
                    ..Default::default()
                }) else {
                    xml_verr_memory(Some(self), Some("malloc failed"));
                    return None;
                };
                cur = Some(res);
                let name = Cow::Owned(name.to_owned());
                let prefix = prefix.map(|pre| Cow::Owned(pre.to_owned()));
                match dtd.elements.entry((name, prefix)) {
                    Entry::Occupied(_) => {
                        xml_verr_memory(Some(self), Some("adding entry failed"));
                        xml_free_element(cur);
                        cur = None;
                    }
                    Entry::Vacant(entry) => {
                        entry.insert(res);
                    }
                }
            }
            cur
        }
    }

    /// Register a new attribute declaration
    /// Note that @tree becomes the ownership of the DTD
    ///
    /// Returns null_mut() if not new, otherwise the attribute decl
    #[allow(clippy::too_many_arguments)]
    #[doc(alias = "xmlAddAttributeDecl")]
    pub unsafe fn add_attribute_decl(
        &mut self,
        dtd: Option<XmlDtdPtr>,
        elem: &str,
        name: &str,
        ns: Option<&str>,
        typ: XmlAttributeType,
        def: XmlAttributeDefault,
        mut default_value: Option<&str>,
        tree: Option<Box<XmlEnumeration>>,
    ) -> Option<XmlAttributePtr> {
        unsafe {
            let mut dtd = dtd?;

            #[cfg(feature = "libxml_valid")]
            {
                // Check the type and possibly the default value.
                // match typ {
                //     XmlAttributeType::XmlAttributeCDATA => {}
                //     XmlAttributeType::XmlAttributeID => {}
                //     XmlAttributeType::XmlAttributeIDREF => {}
                //     XmlAttributeType::XmlAttributeIDREFS => {}
                //     XmlAttributeType::XmlAttributeEntity => {}
                //     XmlAttributeType::XmlAttributeEntities => {}
                //     XmlAttributeType::XmlAttributeNmtoken => {}
                //     XmlAttributeType::XmlAttributeNmtokens => {}
                //     XmlAttributeType::XmlAttributeEnumeration => {}
                //     XmlAttributeType::XmlAttributeNotation => {}
                // }
                if let Some(def) = default_value.filter(|&default_value| {
                    xml_validate_attribute_value_internal(dtd.doc, typ, default_value) == 0
                }) {
                    xml_err_valid_node(
                        Some(self),
                        Some(dtd.into()),
                        XmlParserErrors::XmlDTDAttributeDefault,
                        format!("Attribute {elem} of {name}: invalid default value\n").as_str(),
                        Some(elem),
                        Some(name),
                        Some(def),
                    );
                    default_value = None;
                    self.valid = 0;
                }
            }

            // Check first that an attribute defined in the external subset wasn't
            // already defined in the internal subset
            if let Some(doc) = dtd.doc.filter(|doc| doc.ext_subset == Some(dtd)) {
                if let Some(int_subset) = doc.int_subset {
                    let ret = int_subset
                        .attributes
                        .get(&(
                            Cow::Borrowed(name),
                            ns.map(Cow::Borrowed),
                            Some(Cow::Borrowed(elem)),
                        ))
                        .copied();
                    if ret.is_some() {
                        return None;
                    }
                }
            }

            let Some(mut ret) = XmlAttributePtr::new(XmlAttribute {
                typ: XmlElementType::XmlAttributeDecl,
                atype: typ,
                // doc must be set before possible error causes call
                // to xmlFreeAttribute (because it's used to check on dict use)
                doc: dtd.doc,
                name: Some(name.to_owned()),
                prefix: ns.map(|ns| ns.to_owned()),
                elem: Some(elem.to_owned()),
                def,
                tree,
                default_value: default_value.map(|def| def.into()),
                ..Default::default()
            }) else {
                xml_verr_memory(Some(self), Some("malloc failed"));
                return None;
            };

            // Validity Check:
            // Search the DTD for previous declarations of the ATTLIST
            {
                let attr = ret
                    .name
                    .as_deref()
                    .map(|name| Cow::Owned(name.to_owned()))
                    .unwrap();
                let prefix = ret
                    .prefix
                    .as_deref()
                    .map(|prefix| Cow::Owned(prefix.to_owned()));
                let val = ret.elem.as_deref().map(|elem| Cow::Owned(elem.to_owned()));
                match dtd.attributes.entry((attr, prefix, val)) {
                    Entry::Occupied(_) => {
                        #[cfg(feature = "libxml_valid")]
                        {
                            // The attribute is already defined in this DTD.
                            xml_err_valid_warning!(
                                Some(self),
                                Some(dtd.into()),
                                XmlParserErrors::XmlDTDAttributeRedefined,
                                "Attribute {} of element {}: already defined\n",
                                name,
                                elem
                            );
                        }
                        xml_free_attribute(ret);
                        return None;
                    }
                    Entry::Vacant(entry) => {
                        entry.insert(ret);
                    }
                }
            }

            // Validity Check:
            // Multiple ID per element
            let elem_def = self.get_dtd_element_desc2(dtd, elem, 1);
            if let Some(mut elem_def) = elem_def {
                #[cfg(feature = "libxml_valid")]
                {
                    if matches!(typ, XmlAttributeType::XmlAttributeID)
                        && xml_scan_id_attribute_decl(None, elem_def, 1) != 0
                    {
                        xml_err_valid_node(
                            Some(self),
                            Some(dtd.into()),
                            XmlParserErrors::XmlDTDMultipleID,
                            format!("Element {elem} has too may ID attributes defined : {name}\n")
                                .as_str(),
                            Some(elem),
                            Some(name),
                            None,
                        );
                        self.valid = 0;
                    }
                }

                // Insert namespace default def first they need to be processed first.
                if ret.name().as_deref() == Some("xmlns") || ret.prefix.as_deref() == Some("xmlns")
                {
                    ret.nexth = elem_def.attributes;
                    elem_def.attributes = Some(ret);
                } else {
                    let mut tmp = elem_def.attributes;

                    while let Some(now) = tmp.filter(|tmp| {
                        tmp.name().as_deref() == Some("xmlns")
                            || ret.prefix.as_deref() == Some("xmlns")
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
            if let Some(mut last) = dtd.last() {
                last.set_next(Some(ret.into()));
                ret.set_prev(Some(last));
                dtd.set_last(Some(ret.into()));
            } else {
                dtd.set_children(Some(ret.into()));
                dtd.set_last(Some(ret.into()));
            }
            Some(ret)
        }
    }

    #[cfg(feature = "libxml_regexp")]
    fn vstate_vpush(&mut self, elem_decl: Option<XmlElementPtr>, node: XmlNodePtr) -> usize {
        // self.vstate = self.vstate_tab.add(self.vstate_nr as usize);
        self.vstate_tab.push(XmlValidState {
            elem_decl,
            node,
            exec: None,
        });
        if let Some(elem_decl) =
            elem_decl.filter(|decl| matches!(decl.etype, XmlElementTypeVal::XmlElementTypeElement))
        {
            if elem_decl.cont_model.is_none() {
                xml_valid_build_content_model(self, elem_decl);
            }
            if let Some(cont_model) = elem_decl.cont_model.clone() {
                self.vstate_tab.last_mut().unwrap().exec =
                    Some(Box::new(XmlRegExecCtxt::new(cont_model, None, null_mut())));
            } else {
                self.vstate_tab.last_mut().unwrap().exec = None;
                let node_name = node.name().unwrap();
                xml_err_valid_node(
                    Some(self),
                    Some(elem_decl.into()),
                    XmlParserErrors::XmlErrInternalError,
                    format!("Failed to build content model regexp for {}\n", node_name).as_str(),
                    Some(&node_name),
                    None,
                    None,
                );
            }
        }
        self.vstate_tab.len() - 1
    }

    #[cfg(feature = "libxml_regexp")]
    fn vstate_vpop(&mut self) -> i32 {
        if self.vstate_tab.is_empty() {
            return -1;
        }
        let mut state = self.vstate_tab.pop().unwrap();
        let elem_decl = state.elem_decl;
        if elem_decl.is_some_and(|elem_decl| {
            matches!(elem_decl.etype, XmlElementTypeVal::XmlElementTypeElement)
        }) {
            state.exec.take();
        }
        self.vstate_tab.len() as i32
    }

    /// Push a new element start on the validation stack.
    ///
    /// returns 1 if no validation problem was found or 0 otherwise
    #[doc(alias = "xmlValidatePushElement")]
    #[cfg(all(feature = "libxml_valid", feature = "libxml_regexp"))]
    pub fn push_element(&mut self, doc: XmlDocPtr, elem: XmlNodePtr, qname: &str) -> i32 {
        let mut ret: i32 = 1;
        let mut extsubset: i32 = 0;

        if let Some(state) = self.vstate_tab.last_mut() {
            // Check the new element against the content model of the new elem.
            if let Some(elem_decl) = state.elem_decl {
                match elem_decl.etype {
                    XmlElementTypeVal::XmlElementTypeUndefined => {
                        ret = 0;
                    }
                    XmlElementTypeVal::XmlElementTypeEmpty => {
                        let name = state.node.name().unwrap().into_owned();
                        let node = state.node.into();
                        xml_err_valid_node(
                            Some(self),
                            Some(node),
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
                        if elem_decl
                            .content
                            .as_deref()
                            .is_some_and(|content| content.r#type() == ElementContentType::PCDATA)
                        {
                            let name = state.node.name().unwrap().into_owned();
                            let node = state.node.into();
                            xml_err_valid_node(
                                Some(self),
                                Some(node),
                                XmlParserErrors::XmlDTDNotPCDATA,
                                format!(
                                    "Element {} was declared #PCDATA but contains non text nodes\n",
                                    name
                                )
                                .as_str(),
                                Some(&name),
                                None,
                                None,
                            );
                            ret = 0;
                        } else {
                            let name = state.node.name().unwrap().into_owned();
                            let node = state.node.into();
                            ret = xml_validate_check_mixed(elem_decl.content.clone(), qname);
                            if ret != 1 {
                                xml_err_valid_node(
                                    Some(self),
                                    Some(node),
                                    XmlParserErrors::XmlDTDInvalidChild,
                                    format!(
                                        "Element {} is not declared in {} list of possible children\n",
                                        qname,
                                        name,
                                    ).as_str(),
                                    Some(qname),
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
                        if state.exec.is_some() {
                            ret = state
                                .exec
                                .as_mut()
                                .unwrap()
                                .push_string(Some(qname), null_mut());
                            if ret < 0 {
                                let name = state.node.name().unwrap().into_owned();
                                let node = state.node.into();
                                xml_err_valid_node(
                                    Some(self),
                                    Some(node),
                                    XmlParserErrors::XmlDTDContentModel,
                                    format!("Element {name} content does not follow the DTD, Misplaced {qname}\n").as_str(),
                                    Some(&name),
                                    Some(qname),
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
        let e_decl = xml_valid_get_elem_decl(self, doc, elem, &mut extsubset);
        self.vstate_vpush(e_decl, elem);
        ret
    }

    /// Pop the element end from the validation stack.
    ///
    /// Returns 1 if no validation problem was found or 0 otherwise
    #[doc(alias = "xmlValidatePopElement")]
    #[cfg(all(feature = "libxml_valid", feature = "libxml_regexp"))]
    pub fn pop_element(
        &mut self,
        _doc: Option<XmlDocPtr>,
        _elem: Option<XmlNodePtr>,
        _qname: &str,
    ) -> i32 {
        let mut ret: i32 = 1;

        // printf("PopElem %s\n", qname);
        if let Some(state) = self.vstate_tab.last_mut() {
            // Check the new element against the content model of the new elem.
            if let Some(elem_decl) = state.elem_decl {
                if matches!(elem_decl.etype, XmlElementTypeVal::XmlElementTypeElement)
                    && state.exec.is_some()
                {
                    ret = state.exec.as_mut().unwrap().push_string(None, null_mut());
                    if ret <= 0 {
                        let name = state.node.name().unwrap().into_owned();
                        let node = state.node.into();
                        xml_err_valid_node(
                            Some(self),
                            Some(node),
                            XmlParserErrors::XmlDTDContentModel,
                            format!(
                                "Element {} content does not follow the DTD, Expecting more children\n",
                                name
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
            self.vstate_vpop();
        }
        ret
    }

    /// Check the CData parsed for validation in the current stack
    ///
    /// Returns 1 if no validation problem was found or 0 otherwise
    #[doc(alias = "xmlValidatePushCData")]
    #[cfg(all(feature = "libxml_valid", feature = "libxml_regexp"))]
    pub fn push_cdata(&mut self, data: &str) -> i32 {
        let mut ret: i32 = 1;

        if data.is_empty() {
            return ret;
        }

        if let Some(state) = self.vstate_tab.last() {
            // Check the new element against the content model of the new elem.
            if let Some(elem_decl) = state.elem_decl {
                match elem_decl.etype {
                    XmlElementTypeVal::XmlElementTypeUndefined => {
                        ret = 0;
                    }
                    XmlElementTypeVal::XmlElementTypeEmpty => {
                        let name = state.node.name().unwrap().into_owned();
                        let node = state.node.into();
                        xml_err_valid_node(
                            Some(self),
                            Some(node),
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
                        if data.contains(|c: char| !c.is_xml_blank_char()) {
                            let name = state.node.name().unwrap().into_owned();
                            let node = state.node.into();
                            xml_err_valid_node(
                                Some(self),
                                Some(node),
                                XmlParserErrors::XmlDTDContentModel,
                                format!(
                                    "Element {} content does not follow the DTD, Text not allowed\n",
                                    name
                                ).as_str(),
                                Some(&name),
                                None,
                                None,
                            );
                            return 0;
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
}

impl Default for XmlValidCtxt {
    fn default() -> Self {
        Self {
            user_data: None,
            error: None,
            warning: None,
            node: None,
            node_tab: vec![],
            flags: 0,
            doc: None,
            valid: 0,
            vstate_tab: vec![],
            am: None,
            state: usize::MAX,
        }
    }
}

/// Handle an out of memory error
#[doc(alias = "xmlVErrMemory")]
pub(crate) fn xml_verr_memory(ctxt: Option<&mut XmlValidCtxt>, extra: Option<&str>) {
    let mut channel: Option<GenericError> = None;
    let mut data = None;

    if let Some(ctxt) = ctxt {
        channel = ctxt.error;
        data = ctxt.user_data.clone();
    }
    if let Some(extra) = extra {
        __xml_raise_error!(
            None,
            channel,
            data,
            null_mut(),
            None,
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
            null_mut(),
            None,
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
pub fn xml_add_notation_decl<'a>(
    _ctxt: Option<&mut XmlValidCtxt>,
    dtd: Option<&'a mut XmlDtd>,
    name: &str,
    public_id: Option<&str>,
    system_id: Option<&str>,
) -> Option<&'a XmlNotation> {
    if public_id.is_none() && system_id.is_none() {
        return None;
    }

    let dtd = dtd?;
    let ret = XmlNotation::new(name, public_id, system_id);

    // Validity Check:
    // Check the DTD for previous declarations of the ATTLIST
    match dtd.notations.entry(name.to_owned()) {
        Entry::Occupied(_) => {
            #[cfg(feature = "libxml_valid")]
            {
                xml_err_valid!(
                    None::<&mut XmlValidCtxt>,
                    XmlParserErrors::XmlDTDNotationRedefined,
                    "xmlAddNotationDecl: {} already defined\n",
                    name
                );
            }
            None
        }
        Entry::Vacant(entry) => Some(entry.insert(ret)),
    }
}

/// This will dump the content of the notation table as an XML DTD definition
#[doc(alias = "xmlDumpNotationTable")]
#[cfg(feature = "libxml_output")]
pub fn xml_dump_notation_table<'a>(
    out: &mut (impl Write + 'a),
    table: &HashMap<String, XmlNotation>,
) {
    use crate::tree::xml_dump_notation_decl;

    for notation in table.values() {
        xml_dump_notation_decl(out, notation);
    }
}

/// Allocate an element content structure.
/// Deprecated in favor of xmlNewDocElementContent
///
/// Returns null_mut() if not, otherwise the new element content structure
#[doc(alias = "xmlNewElementContent")]
pub fn xml_new_element_content(
    name: Option<&str>,
    typ: XmlElementContentType,
) -> XmlElementContent {
    xml_new_doc_element_content(None, name, typ)
}

/// Build a copy of an element content description.
/// Deprecated, use xmlCopyDocElementContent instead
///
/// Returns the new xmlElementContentPtr or null_mut() in case of error.
#[doc(alias = "xmlCopyElementContent")]
pub fn xml_copy_element_content(
    content: Option<Rc<RefCell<XmlElementContent>>>,
) -> Option<Rc<RefCell<XmlElementContent>>> {
    xml_copy_doc_element_content(None, content)
}

/// Allocate an element content structure for the document.
///
/// Returns null_mut() if not, otherwise the new element content structure
#[doc(alias = "xmlNewDocElementContent")]
pub fn xml_new_doc_element_content(
    _doc: Option<XmlDocPtr>,
    name: Option<&str>,
    typ: XmlElementContentType,
) -> XmlElementContent {
    match typ {
        XmlElementContentType::XmlElementContentElement => {
            if name.is_none() {
                xml_err_valid!(
                    None::<&mut XmlValidCtxt>,
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
                    None::<&mut XmlValidCtxt>,
                    XmlParserErrors::XmlErrInternalError,
                    "xmlNewElementContent : name != NULL !\n"
                );
            }
        }
    }
    let mut ret = XmlElementContent {
        typ,
        ocur: XmlElementContentOccur::XmlElementContentOnce,
        ..Default::default()
    };
    if let Some(name) = name {
        if let Some((prefix, local)) = split_qname2(name) {
            ret.prefix = Some(prefix.into());
            ret.name = Some(local.into());
        } else {
            ret.name = Some(name.into());
        }
    }
    ret
}

/// Build a copy of an element content description.
///
/// Returns the new xmlElementContentPtr or null_mut() in case of error.
#[doc(alias = "xmlCopyDocElementContent")]
pub fn xml_copy_doc_element_content(
    _doc: Option<XmlDocPtr>,
    cur: Option<Rc<RefCell<XmlElementContent>>>,
) -> Option<Rc<RefCell<XmlElementContent>>> {
    let cur = cur?;
    let cur = cur.borrow();
    let ret = XmlElementContent {
        typ: cur.typ,
        ocur: cur.ocur,
        name: cur.name.clone(),
        prefix: cur.prefix.clone(),
        c1: xml_copy_doc_element_content(_doc, cur.c1.clone()),
        ..Default::default()
    };
    let ret = Rc::new(RefCell::new(ret));
    if cur.c2.is_some() {
        let mut prev = ret.clone();
        let mut cur = cur.c2.clone();
        while let Some(now) = cur {
            let mut tmp = XmlElementContent::default();
            let now = now.borrow();
            tmp.typ = now.typ;
            tmp.ocur = now.ocur;
            tmp.parent = Rc::downgrade(&prev);
            tmp.name = now.name.clone();
            tmp.prefix = now.prefix.clone();
            tmp.c1 = xml_copy_doc_element_content(_doc, now.c1.clone());

            let tmp = Rc::new(RefCell::new(tmp));
            prev.borrow_mut().c2 = Some(tmp.clone());

            if let Some(c1) = tmp.borrow().c1.clone() {
                c1.borrow_mut().parent = Rc::downgrade(&tmp);
            }
            prev = tmp;
            cur = now.c2.clone();
        }
    }
    if let Some(c1) = ret.borrow().c1.clone() {
        c1.borrow_mut().parent = Rc::downgrade(&ret);
    }
    Some(ret)
}

/// This will dump the content of the element content definition
/// Intended just for the debug routine
#[doc(alias = "xmlSnprintfElementContent")]
pub fn xml_snprintf_element_content(
    buf: &mut String,
    size: usize,
    content: Rc<RefCell<XmlElementContent>>,
    englob: i32,
) {
    let len = buf.len();
    if size - len < 50 {
        if size - len > 4 && !buf.ends_with('.') {
            buf.push_str(" ...");
        }
        return;
    }
    if englob != 0 {
        buf.push('(');
    }
    let content = content.borrow();
    match content.typ {
        XmlElementContentType::XmlElementContentPCDATA => {
            buf.push_str("#PCDATA");
        }
        XmlElementContentType::XmlElementContentElement => {
            let mut qname_len = content.name.as_deref().map_or(0, |name| name.len());

            if let Some(prefix) = content.prefix.as_deref() {
                qname_len += prefix.len() + 1;
            }
            if size - len < qname_len + 10 {
                buf.push_str(" ...");
                return;
            }
            if let Some(prefix) = content.prefix.as_deref() {
                buf.push_str(prefix);
                buf.push(':');
            }
            if let Some(name) = content.name.as_deref() {
                buf.push_str(name);
            }
        }
        XmlElementContentType::XmlElementContentSeq => {
            let c1 = content.c1.clone().unwrap();
            if matches!(
                c1.borrow().typ,
                XmlElementContentType::XmlElementContentOr
                    | XmlElementContentType::XmlElementContentSeq
            ) {
                xml_snprintf_element_content(buf, size, c1, 1);
            } else {
                xml_snprintf_element_content(buf, size, c1, 0);
            }

            let len = buf.len();
            if size - len < 50 {
                if size - len > 4 && !buf.ends_with('.') {
                    buf.push_str(" ...");
                }
                return;
            }
            buf.push_str(" , ");
            let c2 = content.c2.clone().unwrap();
            let typ = c2.borrow().typ;
            let ocur = c2.borrow().ocur;
            if (matches!(typ, XmlElementContentType::XmlElementContentOr)
                || !matches!(ocur, XmlElementContentOccur::XmlElementContentOnce))
                && !matches!(typ, XmlElementContentType::XmlElementContentElement)
            {
                xml_snprintf_element_content(buf, size, c2, 1);
            } else {
                xml_snprintf_element_content(buf, size, c2, 0);
            }
        }
        XmlElementContentType::XmlElementContentOr => {
            let c1 = content.c1.clone().unwrap();
            if matches!(
                c1.borrow().typ,
                XmlElementContentType::XmlElementContentOr
                    | XmlElementContentType::XmlElementContentSeq
            ) {
                xml_snprintf_element_content(buf, size, c1, 1);
            } else {
                xml_snprintf_element_content(buf, size, c1, 0);
            }
            let len = buf.len();
            if size - len < 50 {
                if size - len > 4 && !buf.ends_with('.') {
                    buf.push_str(" ...");
                }
                return;
            }
            let c2 = content.c2.clone().unwrap();
            let typ = c2.borrow().typ;
            let ocur = c2.borrow().ocur;
            buf.push_str(" | ");
            if (matches!(typ, XmlElementContentType::XmlElementContentSeq)
                || !matches!(ocur, XmlElementContentOccur::XmlElementContentOnce))
                && !matches!(typ, XmlElementContentType::XmlElementContentElement)
            {
                xml_snprintf_element_content(buf, size, c2, 1);
            } else {
                xml_snprintf_element_content(buf, size, c2, 0);
            }
        }
    }
    if size - buf.len() <= 2 {
        return;
    }
    if englob != 0 {
        buf.push(')');
    }
    match content.ocur {
        XmlElementContentOccur::XmlElementContentOnce => {}
        XmlElementContentOccur::XmlElementContentOpt => buf.push('?'),
        XmlElementContentOccur::XmlElementContentMult => buf.push('*'),
        XmlElementContentOccur::XmlElementContentPlus => buf.push('+'),
    }
}

/// Handle a validation error, provide contextual information
///
/// # Note
/// This function does not format the string.
#[doc(alias = "xmlErrValidNode")]
#[cfg(any(feature = "libxml_valid", feature = "schema"))]
fn xml_err_valid_node(
    ctxt: Option<&mut XmlValidCtxt>,
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
    let mut data = None;

    if let Some(ctxt) = ctxt {
        channel = ctxt.error;
        data = ctxt.user_data.clone();
    }
    __xml_raise_error!(
        schannel,
        channel,
        data,
        null_mut(),
        node,
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
        Some(msg),
    );
}

/// Register a new element declaration
///
/// Returns null_mut() if not, otherwise the entity
#[doc(alias = "xmlAddElementDecl")]
pub unsafe fn xml_add_element_decl(
    ctxt: Option<&mut XmlValidCtxt>,
    dtd: Option<XmlDtdPtr>,
    mut name: &str,
    typ: Option<XmlElementTypeVal>,
    content: Option<Rc<ElementContent>>,
) -> Option<XmlElementPtr> {
    unsafe {
        let mut dtd = dtd?;
        match typ {
            Some(XmlElementTypeVal::XmlElementTypeEmpty) => {
                if content.is_some() {
                    xml_err_valid!(
                        ctxt,
                        XmlParserErrors::XmlErrInternalError,
                        "xmlAddElementDecl: content != NULL for EMPTY\n"
                    );
                    return None;
                }
            }
            Some(XmlElementTypeVal::XmlElementTypeAny) => {
                if content.is_some() {
                    xml_err_valid!(
                        ctxt,
                        XmlParserErrors::XmlErrInternalError,
                        "xmlAddElementDecl: content != NULL for ANY\n"
                    );
                    return None;
                }
            }
            Some(XmlElementTypeVal::XmlElementTypeMixed) => {
                if content.is_none() {
                    xml_err_valid!(
                        ctxt,
                        XmlParserErrors::XmlErrInternalError,
                        "xmlAddElementDecl: content == NULL for MIXED\n"
                    );
                    return None;
                }
            }
            Some(XmlElementTypeVal::XmlElementTypeElement) => {
                if content.is_none() {
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
                .get(&(Cow::Borrowed(name), ns.map(Cow::Borrowed)))
                .cloned();
            if let Some(mut ret) =
                ret.filter(|ret| ret.etype == XmlElementTypeVal::XmlElementTypeUndefined)
            {
                old_attributes = ret.attributes.take();
                dtd.elements.remove(&(
                    Cow::Owned(name.to_owned()),
                    ns.map(|ns| Cow::Owned(ns.to_owned())),
                ));
                xml_free_element(Some(ret));
            }
        }

        // The element may already be present if one of its attribute was registered first
        let mut ret = if let Some(ret) = dtd
            .elements
            .get(&(Cow::Borrowed(name), ns.map(Cow::Borrowed)))
            .cloned()
        {
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
                name: Some(name.to_owned()),
                prefix: ns.map(|ns| ns.to_owned()),
                ..Default::default()
            }) else {
                xml_verr_memory(ctxt, Some("malloc failed"));
                return None;
            };

            // Validity Check:
            // Insertion must not fail
            match dtd.elements.entry((
                Cow::Owned(name.to_owned()),
                ns.map(|ns| Cow::Owned(ns.to_owned())),
            )) {
                Entry::Occupied(_) => {
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
                Entry::Vacant(entry) => {
                    entry.insert(ret);
                }
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
        ret.content = deep_clone_element_content(content, Weak::new());

        // Link it to the DTD
        ret.parent = Some(dtd);
        ret.doc = dtd.doc;
        if let Some(mut last) = dtd.last() {
            last.set_next(Some(ret.into()));
            ret.set_prev(Some(last));
            dtd.set_last(Some(ret.into()));
        } else {
            dtd.set_children(Some(ret.into()));
            let children = dtd.children();
            dtd.set_last(children);
        }
        Some(ret)
    }
}

/// Dump the occurrence operator of an element.
#[doc(alias = "xmlDumpElementOccur")]
#[cfg(feature = "libxml_output")]
fn xml_dump_element_occur<'a>(buf: &mut (impl Write + 'a), cur: &XmlElementContent) {
    match cur.ocur {
        XmlElementContentOccur::XmlElementContentOnce => {}
        XmlElementContentOccur::XmlElementContentOpt => {
            write!(buf, "?").ok();
        }
        XmlElementContentOccur::XmlElementContentMult => {
            write!(buf, "*").ok();
        }
        XmlElementContentOccur::XmlElementContentPlus => {
            write!(buf, "+").ok();
        }
    }
}

/// This will dump the content of the element table as an XML DTD definition
#[doc(alias = "xmlDumpElementContent")]
#[cfg(feature = "libxml_output")]
fn xml_dump_element_content<'a>(
    buf: &mut (impl Write + 'a),
    content: Rc<RefCell<XmlElementContent>>,
) {
    write!(buf, "(").ok();
    let mut cur = content.clone();
    let mut init = false;

    while !Rc::ptr_eq(&cur, &content) || !init {
        init = true;

        let now = cur.borrow();
        match now.typ {
            XmlElementContentType::XmlElementContentPCDATA => {
                write!(buf, "#PCDATA").ok();
            }
            XmlElementContentType::XmlElementContentElement => {
                if let Some(prefix) = now.prefix.as_deref() {
                    write!(buf, "{prefix}:").ok();
                }
                write!(buf, "{}", now.name.as_deref().unwrap()).ok();
            }
            XmlElementContentType::XmlElementContentSeq
            | XmlElementContentType::XmlElementContentOr => {
                if now.parent.upgrade().is_some_and(|par| {
                    now.typ != par.borrow().typ
                        || !matches!(now.ocur, XmlElementContentOccur::XmlElementContentOnce)
                }) {
                    write!(buf, "(").ok();
                }
                if now.c1.is_none() {
                    return;
                }
                let next = now.c1.clone().unwrap();
                drop(now);
                cur = next;
                continue;
            }
        }
        drop(now);

        while !Rc::ptr_eq(&cur, &content) {
            let now = cur.borrow();
            let Some(parent) = now.parent.upgrade() else {
                return;
            };

            if matches!(
                now.typ,
                XmlElementContentType::XmlElementContentOr
                    | XmlElementContentType::XmlElementContentSeq
            ) && (now.typ != parent.borrow().typ
                || !matches!(now.ocur, XmlElementContentOccur::XmlElementContentOnce))
            {
                write!(buf, ")").ok();
            }
            xml_dump_element_occur(buf, &cur.borrow());

            if parent
                .borrow()
                .c1
                .as_ref()
                .is_some_and(|c1| Rc::ptr_eq(&cur, c1))
            {
                if parent.borrow().typ == XmlElementContentType::XmlElementContentSeq {
                    write!(buf, " , ").ok();
                } else if parent.borrow().typ == XmlElementContentType::XmlElementContentOr {
                    write!(buf, " | ").ok();
                }

                drop(now);
                cur = parent.borrow().c2.clone().unwrap();
                break;
            }

            drop(now);
            cur = parent;
        }
    }

    write!(buf, ")").ok();
    xml_dump_element_occur(buf, &content.borrow());
}

/// This will dump the content of the element declaration as an XML DTD definition
#[doc(alias = "xmlDumpElementDecl")]
#[cfg(feature = "libxml_output")]
pub fn xml_dump_element_decl<'a>(buf: &mut (impl Write + 'a), elem: XmlElementPtr) {
    let name = elem.name.as_deref().unwrap();
    match elem.etype {
        XmlElementTypeVal::XmlElementTypeEmpty => {
            write!(buf, "<!ELEMENT ").ok();
            if let Some(prefix) = elem.prefix.as_deref() {
                write!(buf, "{prefix}:").ok();
            }
            writeln!(buf, "{} EMPTY>", name).ok();
        }
        XmlElementTypeVal::XmlElementTypeAny => {
            write!(buf, "<!ELEMENT ").ok();
            if let Some(prefix) = elem.prefix.as_deref() {
                write!(buf, "{prefix}:").ok();
            }
            writeln!(buf, "{} ANY>", name).ok();
        }
        XmlElementTypeVal::XmlElementTypeMixed => {
            write!(buf, "<!ELEMENT ").ok();
            if let Some(prefix) = elem.prefix.as_deref() {
                write!(buf, "{prefix}:").ok();
            }
            writeln!(buf, "{} {}>", name, elem.content.as_ref().unwrap()).ok();
        }
        XmlElementTypeVal::XmlElementTypeElement => {
            write!(buf, "<!ELEMENT ").ok();
            if let Some(prefix) = elem.prefix.as_deref() {
                write!(buf, "{prefix}:").ok();
            }
            writeln!(buf, "{} {}>", name, elem.content.as_ref().unwrap()).ok();
        }
        _ => {
            xml_err_valid!(
                None::<&mut XmlValidCtxt>,
                XmlParserErrors::XmlErrInternalError,
                "Internal: ELEMENT struct corrupted invalid type\n"
            );
        }
    }
}

#[cfg(feature = "libxml_valid")]
fn xml_is_doc_name_start_char(doc: Option<XmlDocPtr>, c: i32) -> i32 {
    use crate::parser::xml_is_letter;

    if doc.is_none_or(|doc| doc.properties & XmlDocProperties::XmlDocOld10 as i32 == 0) {
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
fn xml_is_doc_name_char(doc: Option<XmlDocPtr>, c: i32) -> i32 {
    use crate::parser::xml_is_letter;

    if doc.is_none_or(|doc| doc.properties & XmlDocProperties::XmlDocOld10 as i32 == 0) {
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
        || (c as u32).is_xml_digit()
        || c == b'.' as i32
        || c == b'-' as i32
        || c == b'_' as i32
        || c == b':' as i32
        || (c as u32).is_xml_combining()
        || (c as u32).is_xml_extender()
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
fn xml_validate_names_value_internal(doc: Option<XmlDocPtr>, value: &str) -> i32 {
    if !value.starts_with(|c: char| xml_is_doc_name_start_char(doc, c as i32) != 0) {
        return 0;
    }

    let mut is_last_empty = false;
    for name in value.split('\x20') {
        is_last_empty = name.is_empty();
        if is_last_empty {
            continue;
        }

        let mut cur = name.chars();
        if xml_is_doc_name_start_char(doc, cur.next().unwrap() as i32) == 0 {
            return 0;
        }
        if cur.any(|c: char| xml_is_doc_name_char(doc, c as i32) == 0) {
            return 0;
        }
    }

    (!is_last_empty) as i32
}

#[cfg(feature = "libxml_valid")]
fn xml_validate_name_value_internal(doc: Option<XmlDocPtr>, value: &str) -> i32 {
    let mut cur = value.chars();
    if cur
        .next()
        .is_none_or(|c: char| xml_is_doc_name_start_char(doc, c as i32) == 0)
    {
        return 0;
    }

    cur.all(|c| xml_is_doc_name_char(doc, c as i32) != 0) as i32
}

/// Validate that the given value match Nmtokens production
///
/// `[ VC: Name Token ]`
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateNmtokensValueInternal")]
#[cfg(feature = "libxml_valid")]
fn xml_validate_nmtokens_value_internal(doc: Option<XmlDocPtr>, mut value: &str) -> i32 {
    value = value.trim_matches(|c: char| c.is_xml_blank_char());
    if value
        .chars()
        .next()
        .is_none_or(|c| xml_is_doc_name_char(doc, c as i32) == 0)
    {
        return 0;
    }
    value
        .split('\x20')
        .filter(|value| !value.is_empty())
        .all(|nmtoken| {
            nmtoken
                .chars()
                .all(|c| xml_is_doc_name_char(doc, c as i32) != 0)
        }) as i32
}

/// Validate that the given value match Nmtoken production
///
/// `[ VC: Name Token ]`
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateNmtokenValueInternal")]
#[cfg(feature = "libxml_valid")]
fn xml_validate_nmtoken_value_internal(doc: Option<XmlDocPtr>, value: &str) -> i32 {
    (!value.is_empty()
        && value
            .chars()
            .all(|c| xml_is_doc_name_char(doc, c as i32) != 0)) as i32
}

/// Validate that the given attribute value match  the proper production
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateAttributeValueInternal")]
#[cfg(feature = "libxml_valid")]
pub(crate) fn xml_validate_attribute_value_internal(
    doc: Option<XmlDocPtr>,
    typ: XmlAttributeType,
    value: &str,
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

/// Verify that the element don't have too many ID attributes
/// declared.
///
/// Returns the number of ID attributes found.
#[doc(alias = "xmlScanIDAttributeDecl")]
#[cfg(feature = "libxml_valid")]
pub(crate) fn xml_scan_id_attribute_decl(
    mut ctxt: Option<&mut XmlValidCtxt>,
    elem: XmlElementPtr,
    err: i32,
) -> i32 {
    let mut ret: i32 = 0;

    let mut cur = elem.attributes;
    while let Some(now) = cur {
        if matches!(now.atype, XmlAttributeType::XmlAttributeID) {
            ret += 1;
            if ret > 1 && err != 0 {
                let elem_name = elem.name.as_deref().unwrap();
                let cur_name = now.name().unwrap();
                xml_err_valid_node(
                    ctxt.as_deref_mut(),
                    Some(elem.into()),
                    XmlParserErrors::XmlDTDMultipleID,
                    format!(
                        "Element {elem_name} has too many ID attributes defined : {cur_name}\n"
                    )
                    .as_str(),
                    Some(elem_name),
                    Some(&cur_name),
                    None,
                );
            }
        }
        cur = now.nexth;
    }
    ret
}

/// This will dump the content of the attribute declaration as an XML DTD definition
#[doc(alias = "xmlDumpAttributeDecl")]
#[cfg(feature = "libxml_output")]
pub fn xml_dump_attribute_decl<'a>(buf: &mut (impl Write + 'a), attr: XmlAttributePtr) {
    use crate::{io::write_quoted, tree::xml_dump_enumeration};

    write!(buf, "<!ATTLIST ").ok();
    let elem = attr.elem.as_deref().unwrap();
    write!(buf, "{elem}").ok();
    write!(buf, " ").ok();
    if let Some(prefix) = attr.prefix.as_deref() {
        write!(buf, "{}:", prefix).ok();
    }
    write!(buf, "{}", attr.name().unwrap()).ok();
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
            write!(buf, " (").ok();
            xml_dump_enumeration(buf, attr.tree.as_deref().unwrap());
            Some(())
        }
        XmlAttributeType::XmlAttributeNotation => {
            write!(buf, " NOTATION (").ok();
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
    if let Some(def) = attr.default_value.as_deref() {
        write!(buf, " ").ok();
        write_quoted(buf, def).ok();
    }
    writeln!(buf, ">").ok();
}

/// Register a new id declaration
///
/// Returns null_mut() if not, otherwise the new xmlIDPtr
#[doc(alias = "xmlAddID")]
pub fn xml_add_id(
    ctxt: Option<&mut XmlValidCtxt>,
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
    ret.attr = Some(attr);
    ret.name = None;
    ret.lineno = attr.parent().map_or(-1, |p| p.get_line_no() as i32);

    // Create the ID table if needed.
    let entry = doc.ids.entry(value.to_owned());
    match entry {
        Entry::Occupied(_) => {
            // The id is already defined in this DTD.
            #[cfg(feature = "libxml_valid")]
            if let Some(ctxt) = ctxt {
                xml_err_valid_node(
                    Some(ctxt),
                    attr.parent(),
                    XmlParserErrors::XmlDTDIDRedefined,
                    format!("ID {value} already defined\n").as_str(),
                    Some(value),
                    None,
                    None,
                );
            }
            return None;
        }
        Entry::Vacant(entry) => entry.insert(ret),
    };
    attr.atype = Some(XmlAttributeType::XmlAttributeID);
    Some(())
}

/// Search the attribute declaring the given ID
///
/// Returns null_mut() if not found,
/// otherwise the xmlAttrPtr defining the ID or XmlDocPtr as `*mut dyn NodeCommon`.
#[doc(alias = "xmlGetID")]
pub fn xml_get_id(doc: XmlDocPtr, id: &str) -> Option<Result<XmlAttrPtr, XmlDocPtr>> {
    match doc.ids.get(id)?.attr {
        Some(attr) => Some(Ok(attr)),
        None => {
            // We are operating on a stream, return a well known reference
            // since the attribute node doesn't exist anymore
            Some(Err(doc))
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
pub fn xml_is_id(
    doc: Option<XmlDocPtr>,
    elem: Option<XmlNodePtr>,
    attr: Option<XmlAttrPtr>,
) -> i32 {
    let Some(attr) = attr else {
        return 0;
    };
    if attr.name().as_deref() == Some("id")
        && attr
            .ns
            .is_some_and(|ns| ns.prefix().as_deref() == Some("xml"))
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
        if "id" == attr.name.as_ref()
            || ("name" == attr.name.as_ref() && elem.is_none_or(|elem| elem.name == "a"))
        {
            return 1;
        }
        return 0;
    } else if let Some(elem) = elem {
        let elemname = elem.name().unwrap();
        let attrname = attr.name().unwrap();
        let fullelemname = build_qname(
            &elemname,
            elem.ns.as_deref().and_then(|ns| ns.prefix()).as_deref(),
        );
        let fullattrname = build_qname(
            &attrname,
            attr.ns.as_deref().and_then(|ns| ns.prefix()).as_deref(),
        );

        let mut attr_decl = doc
            .int_subset
            .and_then(|dtd| dtd.get_attr_desc(&fullelemname, &fullattrname));
        if attr_decl.is_none() {
            if let Some(ext_subset) = doc.ext_subset {
                attr_decl = ext_subset.get_attr_desc(&fullelemname, &fullattrname);
            }
        }

        if attr_decl
            .is_some_and(|attr_decl| matches!(attr_decl.atype, XmlAttributeType::XmlAttributeID))
        {
            return 1;
        }
    } else {
        return 0;
    }
    0
}

/// Normalize a string in-place.
#[doc(alias = "xmlValidNormalizeString")]
pub(crate) fn xml_valid_normalize_string(s: &str) -> Cow<'_, str> {
    if !s.contains("\x20\x20") {
        return Cow::Borrowed(s);
    }

    let mut buf = String::with_capacity(s.len());
    let mut s = s.split('\x20');
    buf.push_str(s.next().unwrap());
    for s in s {
        buf.push('\x20');
        buf.push_str(s);
    }
    Cow::Owned(buf)
}

/// Remove the given attribute from the ID table maintained internally.
///
/// Returns -1 if the lookup failed and 0 otherwise
#[doc(alias = "xmlRemoveID")]
pub fn xml_remove_id(mut doc: XmlDocPtr, mut attr: XmlAttrPtr) -> i32 {
    let Some(id) = attr.children().and_then(|c| c.get_string(Some(doc), 1)) else {
        return -1;
    };
    let id = xml_valid_normalize_string(&id);

    let Some(id_ptr) = doc.ids.get(id.as_ref()) else {
        return -1;
    };
    if id_ptr.attr != Some(attr) {
        return -1;
    }

    doc.ids.remove(id.as_ref());
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
pub(crate) fn xml_add_ref(
    _ctxt: Option<&mut XmlValidCtxt>,
    mut doc: XmlDocPtr,
    value: &str,
    attr: XmlAttrPtr,
) -> Option<()> {
    let mut ret = XmlRef {
        value: value.to_owned(),
        ..Default::default()
    };
    ret.name = None;
    ret.attr = Some(attr);
    ret.lineno = attr.parent().map_or(-1, |p| p.get_line_no() as i32);

    // To add a reference :-
    // References are maintained as a list of references,
    // Lookup the entry, if no entry create new nodelist
    // Add the owning node to the NodeList
    // Return the ref

    let ref_list = doc
        .refs
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
pub(crate) fn xml_is_ref(
    doc: Option<XmlDocPtr>,
    elem: Option<XmlNodePtr>,
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
        let Some(elem) = elem else {
            return 0;
        };
        let mut attr_decl = doc.int_subset.and_then(|dtd| {
            dtd.get_attr_desc(
                elem.name().as_deref().unwrap(),
                attr.name().as_deref().unwrap(),
            )
        });
        if attr_decl.is_none() {
            if let Some(ext_subset) = doc.ext_subset {
                attr_decl = ext_subset.get_attr_desc(
                    elem.name().as_deref().unwrap(),
                    attr.name().as_deref().unwrap(),
                );
            }
        }

        if attr_decl.is_some_and(|attr_decl| {
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

// /// Remove the given attribute from the Ref table maintained internally.
// ///
// /// Returns -1 if the lookup failed and 0 otherwise
// #[doc(alias = "xmlRemoveRef")]
// pub(crate) unsafe fn xml_remove_ref(mut doc: XmlDocPtr, attr: XmlAttrPtr) -> i32 {
//     unsafe {
//         if doc.refs.is_none() {
//             return -1;
//         }

//         let Some(id) = attr.children().and_then(|c| c.get_string(Some(doc), 1)) else {
//             return -1;
//         };

//         let table = doc.refs.as_mut().unwrap();
//         let Some(ref_list) = table.get_mut(&id) else {
//             return -1;
//         };

//         // At this point, ref_list refers to a list of references which
//         // have the same key as the supplied attr. Our list of references
//         // is ordered by reference address and we don't have that information
//         // here to use when removing. We'll have to walk the list and
//         // check for a matching attribute, when we find one stop the walk
//         // and remove the entry.
//         // The list is ordered by reference, so that means we don't have the
//         // key. Passing the list and the reference to the walker means we
//         // will have enough data to be able to remove the entry.

//         // Remove the supplied attr from our list
//         ref_list.remove_first_by(|refe| refe.attr == Some(attr));

//         // If the list is empty then remove the list entry in the hash
//         if ref_list.is_empty() {
//             table.remove(&id);
//         }
//         0
//     }
// }

// /// Find the set of references for the supplied ID.
// ///
// /// Returns `None` if not found, otherwise node set for the ID.
// #[doc(alias = "xmlGetRefs")]
// pub(crate) unsafe fn xml_get_refs<'a>(
//     doc: &'a XmlDoc,
//     id: &str,
// ) -> Option<&'a XmlList<Box<XmlRef>>> {
//     doc.refs.as_ref()?.get(id)
// }

/// Try to validate a the root element
/// basically it does the following check as described by the
/// XML-1.0 recommendation:
///  - [ VC: Root Element Type ]
///    it doesn't try to recurse or apply other check to the element
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateRoot")]
#[cfg(feature = "libxml_valid")]
pub fn xml_validate_root(ctxt: Option<&mut XmlValidCtxt>, doc: XmlDocPtr) -> i32 {
    use crate::parser::build_qname;

    let Some(root) = doc.get_root_element() else {
        xml_err_valid!(ctxt, XmlParserErrors::XmlDTDNoRoot, "no root element\n");
        return 0;
    };

    // When doing post validation against a separate DTD, those may
    // no internal subset has been generated
    if let Some(int_subset) = doc.int_subset.filter(|dtd| dtd.name.is_some()) {
        // Check first the document root against the NQName
        if int_subset.name() != root.name() {
            if let Some(prefix) = root.ns.as_deref().and_then(|ns| ns.prefix()) {
                let root_name = root.name();
                let fullname = build_qname(root_name.as_deref().unwrap(), Some(&prefix));
                if int_subset.name() == Some(fullname) {
                    return 1;
                }
            }
            if int_subset.name.as_deref() == Some("HTML") && root.name == "html" {
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
pub fn xml_validate_element_decl(
    ctxt: &mut XmlValidCtxt,
    doc: XmlDocPtr,
    elem: Option<XmlElementPtr>,
) -> i32 {
    let mut ret: i32 = 1;

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
        let mut cur = elem.content.clone();
        while let Some(now) = cur {
            if now.r#type() != ElementContentType::Or {
                break;
            }
            let Some(c1) = now.first_child() else {
                break;
            };
            if c1.r#type() == ElementContentType::Element {
                let mut next = now.second_child();
                while let Some(nx) = next {
                    if nx.r#type() == ElementContentType::Element {
                        if nx.name() == c1.name() {
                            let elem_name = elem.name.as_deref().unwrap();
                            if let Some(prefix) = c1.prefix().as_deref() {
                                xml_err_valid_node(
                                    Some(&mut *ctxt),
                                    Some(elem.into()),
                                    XmlParserErrors::XmlDTDContentError,
                                    format!(
                                        "Definition of {} has duplicate references of {}\n",
                                        elem_name,
                                        c1.name().unwrap()
                                    )
                                    .as_str(),
                                    Some(elem_name),
                                    Some(prefix),
                                    Some(&c1.name().unwrap()),
                                );
                            } else {
                                xml_err_valid_node(
                                    Some(&mut *ctxt),
                                    Some(elem.into()),
                                    XmlParserErrors::XmlDTDContentError,
                                    format!(
                                        "Definition of {} has duplicate references of {}\n",
                                        elem_name,
                                        c1.name().unwrap()
                                    )
                                    .as_str(),
                                    Some(elem_name),
                                    Some(&c1.name().unwrap()),
                                    None,
                                );
                            }
                            ret = 0;
                        }
                        break;
                    }
                    let Some(nx_c1) = nx.first_child() else {
                        break;
                    };
                    if nx_c1.r#type() != ElementContentType::Element {
                        break;
                    }
                    if nx_c1.name() == c1.name() {
                        let elem_name = elem.name.as_deref().unwrap();
                        if let Some(prefix) = c1.prefix().as_deref() {
                            xml_err_valid_node(
                                Some(&mut *ctxt),
                                Some(elem.into()),
                                XmlParserErrors::XmlDTDContentError,
                                format!(
                                    "Definition of {} has duplicate references to {}:{}\n",
                                    elem_name,
                                    prefix,
                                    c1.name().unwrap()
                                )
                                .as_str(),
                                Some(elem_name),
                                Some(prefix),
                                Some(&c1.name().unwrap()),
                            );
                        } else {
                            xml_err_valid_node(
                                Some(&mut *ctxt),
                                Some(elem.into()),
                                XmlParserErrors::XmlDTDContentError,
                                format!(
                                    "Definition of {} has duplicate references to {}\n",
                                    elem_name,
                                    c1.name().unwrap()
                                )
                                .as_str(),
                                Some(elem_name),
                                Some(&c1.name().unwrap()),
                                None,
                            );
                        }
                        ret = 0;
                    }
                    next = nx.second_child();
                }
            }
            cur = now.second_child();
        }
    }

    let elem_name = elem.name.as_deref();
    // VC: Unique Element Type Declaration
    let tst = xml_get_dtd_element_desc(doc.int_subset, elem_name.unwrap());
    if tst.is_some_and(|tst| {
        tst != elem
            && tst.prefix == elem.prefix
            && !matches!(tst.etype, XmlElementTypeVal::XmlElementTypeUndefined)
    }) {
        xml_err_valid_node(
            Some(&mut *ctxt),
            Some(elem.into()),
            XmlParserErrors::XmlDTDElemRedefined,
            format!("Redefinition of element {}\n", elem_name.unwrap()).as_str(),
            elem_name,
            None,
            None,
        );
        ret = 0;
    }
    let tst = xml_get_dtd_element_desc(doc.ext_subset, elem_name.unwrap());
    if tst.is_some_and(|tst| {
        tst != elem
            && tst.prefix == elem.prefix
            && !matches!(tst.etype, XmlElementTypeVal::XmlElementTypeUndefined)
    }) {
        xml_err_valid_node(
            Some(&mut *ctxt),
            Some(elem.into()),
            XmlParserErrors::XmlDTDElemRedefined,
            format!("Redefinition of element {}\n", elem_name.unwrap()).as_str(),
            elem_name,
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
pub fn xml_valid_normalize_attribute_value<'a>(
    doc: XmlDocPtr,
    elem: XmlNodePtr,
    name: &str,
    value: &'a str,
) -> Option<Cow<'a, str>> {
    let mut attr_decl = doc
        .int_subset
        .and_then(|dtd| dtd.get_attr_desc(elem.name().as_deref().unwrap(), name));
    if attr_decl.is_none() {
        if let Some(ext_subset) = doc.ext_subset {
            attr_decl = ext_subset.get_attr_desc(elem.name().as_deref().unwrap(), name);
        }
    }

    if matches!(attr_decl?.atype, XmlAttributeType::XmlAttributeCDATA) {
        return None;
    }
    Some(xml_valid_normalize_string(value))
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
pub fn xml_valid_ctxt_normalize_attribute_value<'a>(
    ctxt: &mut XmlValidCtxt,
    doc: XmlDocPtr,
    elem: XmlNodePtr,
    name: &str,
    value: &'a str,
) -> Option<Cow<'a, str>> {
    let mut extsubset: i32 = 0;

    let mut attr_decl = None;
    if let Some(prefix) = elem.ns.as_deref().and_then(|ns| ns.prefix()) {
        let elemname = elem.name().unwrap();
        let fullname = build_qname(&elemname, Some(&prefix));
        attr_decl = doc
            .int_subset
            .and_then(|dtd| dtd.get_attr_desc(&fullname, name));
        if attr_decl.is_none() {
            if let Some(ext_subset) = doc.ext_subset {
                attr_decl = ext_subset.get_attr_desc(&fullname, name);
                if attr_decl.is_some() {
                    extsubset = 1;
                }
            }
        }
    }
    if attr_decl.is_none() {
        if let Some(int_subset) = doc.int_subset {
            attr_decl = int_subset.get_attr_desc(elem.name().as_deref().unwrap(), name);
        }
    }
    if attr_decl.is_none() {
        if let Some(ext_subset) = doc.ext_subset {
            attr_decl = ext_subset.get_attr_desc(elem.name().as_deref().unwrap(), name);
            if attr_decl.is_some() {
                extsubset = 1;
            }
        }
    }

    if matches!(attr_decl?.atype, XmlAttributeType::XmlAttributeCDATA) {
        return None;
    }

    let ret = xml_valid_normalize_string(value);
    if doc.standalone != 0 && extsubset == 1 && value != ret {
        let elem_name = elem.name().unwrap();
        xml_err_valid_node(
            Some(&mut *ctxt),
            Some(XmlGenericNodePtr::from(elem)),
            XmlParserErrors::XmlDTDNotStandalone,
            format!(
                "standalone: {} on {} value had to be normalized based on external subset declaration\n",
                name,
                elem_name
            ).as_str(),
            Some(name),
            Some(&elem_name),
            None
        );
        ctxt.valid = 0;
    }
    Some(ret)
}

/// Handle a validation error, provide contextual information
#[doc(alias = "xmlErrValidNodeNr")]
macro_rules! xml_err_valid_node_nr {
    ($ctxt:expr, $node:expr, $error:expr, $msg:literal, $str1:expr, $int2:expr, $str3:expr) => {
        let schannel: Option<StructuredError> = None;
        let mut channel: Option<GenericError> = None;
        let mut data = None;

        if let Some(ctxt) = $ctxt {
            channel = (*ctxt).error;
            data = (*ctxt).user_data.clone();
        }
        __xml_raise_error!(
            schannel,
            channel,
            data,
            null_mut(),
            $node,
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
            Some(format!($msg, $str1, $int2, $str3).as_str()),
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
pub fn xml_validate_attribute_decl(
    ctxt: &mut XmlValidCtxt,
    doc: XmlDocPtr,
    attr: XmlAttributePtr,
) -> i32 {
    let mut ret: i32 = 1;
    let val: i32;
    if doc.int_subset.is_none() && doc.ext_subset.is_none() {
        return 0;
    };

    let attr_elem = attr.elem.as_deref();
    // Attribute Default Legal
    // Enumeration
    if let Some(def) = attr.default_value.as_deref() {
        val = xml_validate_attribute_value_internal(Some(doc), attr.atype, def);
        if val == 0 {
            let attr_name = attr.name().unwrap();
            xml_err_valid_node(
                Some(&mut *ctxt),
                Some(attr.into()),
                XmlParserErrors::XmlDTDAttributeDefault,
                format!(
                    "Syntax of default value for attribute {} of {} is not valid\n",
                    attr_name,
                    attr_elem.unwrap()
                )
                .as_str(),
                Some(&attr_name),
                attr_elem,
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
        xml_err_valid_node(
            Some(&mut *ctxt),
            Some(attr.into()),
            XmlParserErrors::XmlDTDIDFixed,
            format!(
                "ID attribute {} of {} is not valid must be #IMPLIED or #REQUIRED\n",
                attr_name,
                attr_elem.unwrap()
            )
            .as_str(),
            Some(&attr_name),
            attr_elem,
            None,
        );
        ret = 0;
    }

    // One ID per Element Type
    if matches!(attr.atype, XmlAttributeType::XmlAttributeID) {
        let mut nb_id: i32;

        // the trick is that we parse DtD as their own internal subset
        let mut elem = xml_get_dtd_element_desc(doc.int_subset, attr_elem.unwrap());
        if let Some(elem) = elem {
            nb_id = xml_scan_id_attribute_decl(None, elem, 0);
        } else {
            // The attribute may be declared in the internal subset and the
            // element in the external subset.
            nb_id = 0;
            if let Some(int_subset) = doc.int_subset {
                for ((_, _, value), attr) in &int_subset.attributes {
                    if matches!(attr.atype, XmlAttributeType::XmlAttributeID)
                        && value.as_deref() == attr_elem
                    {
                        nb_id += 1;
                    }
                }
            }
        }
        if nb_id > 1 {
            xml_err_valid_node_nr!(
                Some(&mut *ctxt),
                Some(attr.into()),
                XmlParserErrors::XmlDTDIDSubset,
                "Element {} has {} ID attribute defined in the internal subset : {}\n",
                attr_elem.unwrap(),
                nb_id,
                attr.name().unwrap().into_owned()
            );
        } else if doc.ext_subset.is_some() {
            let mut ext_id: i32 = 0;
            elem = xml_get_dtd_element_desc(doc.ext_subset, attr_elem.unwrap());
            if let Some(elem) = elem {
                ext_id = xml_scan_id_attribute_decl(None, elem, 0);
            }
            if ext_id > 1 {
                xml_err_valid_node_nr!(
                    Some(&mut *ctxt),
                    Some(attr.into()),
                    XmlParserErrors::XmlDTDIDSubset,
                    "Element {} has {} ID attribute defined in the external subset : {}\n",
                    attr_elem.unwrap(),
                    ext_id,
                    attr.name().unwrap().into_owned()
                );
            } else if ext_id + nb_id > 1 {
                let attr_name = attr.name().unwrap();
                xml_err_valid_node(
                    Some(&mut *ctxt),
                    Some(attr.into()),
                    XmlParserErrors::XmlDTDIDSubset,
                    format!("Element {} has ID attributes defined in the internal and external subset : {}\n", attr_elem.unwrap(), attr_name).as_str(),
                    attr_elem,
                    Some(&attr_name),
                    None
                );
            }
        }
    }

    // Validity Constraint: Enumeration
    if attr.default_value.is_some() && attr.tree.is_some() {
        let mut tree = attr.tree.as_deref();
        while let Some(now) = tree {
            if Some(now.name.as_str()) == attr.default_value.as_deref() {
                break;
            }
            tree = now.next.as_deref();
        }
        if tree.is_none() {
            let attr_def = attr.default_value.as_deref().unwrap();
            let attr_name = attr.name().unwrap();
            xml_err_valid_node(
                Some(&mut *ctxt),
                Some(attr.into()),
                XmlParserErrors::XmlDTDAttributeValue,
                format!(
                    "Default value \"{}\" for attribute {} of {} is not among the enumerated set\n",
                    attr_def,
                    attr_name,
                    attr_elem.unwrap()
                )
                .as_str(),
                Some(attr_def),
                Some(&attr_name),
                attr_elem,
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
pub fn xml_validate_attribute_value(typ: XmlAttributeType, value: &str) -> i32 {
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
pub fn xml_validate_notation_decl(
    _ctxt: &mut XmlValidCtxt,
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
pub fn xml_validate_dtd(ctxt: &mut XmlValidCtxt, mut doc: XmlDocPtr, dtd: XmlDtdPtr) -> i32 {
    let old_ext = doc.ext_subset;
    let old_int = doc.int_subset;
    doc.ext_subset = Some(dtd);
    doc.int_subset = None;
    let mut ret = xml_validate_root(Some(ctxt), doc);
    if ret == 0 {
        doc.ext_subset = old_ext;
        doc.int_subset = old_int;
        return ret;
    }
    doc.ids.clear();
    doc.refs.clear();
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
fn xml_validate_attribute_value2(
    ctxt: &mut XmlValidCtxt,
    mut doc: XmlDocPtr,
    name: &str,
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
                    xml_err_valid_node(
                        Some(ctxt),
                        Some(doc.into()),
                        XmlParserErrors::XmlDTDEntityType,
                        format!(
                            "ENTITY attribute {} reference an entity \"{}\" of wrong type\n",
                            name, value
                        )
                        .as_str(),
                        Some(name),
                        Some(value),
                        None,
                    );
                    ret = 0;
                }
            } else {
                xml_err_valid_node(
                    Some(ctxt),
                    Some(doc.into()),
                    XmlParserErrors::XmlDTDUnknownEntity,
                    format!(
                        "ENTITY attribute {} reference an unknown entity \"{}\"\n",
                        name, value
                    )
                    .as_str(),
                    Some(name),
                    Some(value),
                    None,
                );
                ret = 0;
            }
        }
        XmlAttributeType::XmlAttributeEntities => {
            for nam in value
                .split(|c: char| c.is_xml_blank_char())
                .filter(|nam| !nam.is_empty())
            {
                if let Some(ent) = xml_get_doc_entity(Some(doc), nam) {
                    if !matches!(ent.etype, XmlEntityType::XmlExternalGeneralUnparsedEntity) {
                        xml_err_valid_node(
                            Some(ctxt),
                            Some(doc.into()),
                            XmlParserErrors::XmlDTDEntityType,
                            format!(
                                "ENTITIES attribute {} reference an entity \"{}\" of wrong type\n",
                                name, nam
                            )
                            .as_str(),
                            Some(name),
                            Some(nam),
                            None,
                        );
                        ret = 0;
                    }
                } else {
                    xml_err_valid_node(
                        Some(ctxt),
                        Some(doc.into()),
                        XmlParserErrors::XmlDTDUnknownEntity,
                        format!(
                            "ENTITIES attribute {name} reference an unknown entity \"{nam}\"\n"
                        )
                        .as_str(),
                        Some(name),
                        Some(nam),
                        None,
                    );
                    ret = 0;
                }
            }
        }
        XmlAttributeType::XmlAttributeNotation => {
            let nota = xml_get_dtd_notation_desc(doc.int_subset.as_deref(), value)
                .or_else(|| xml_get_dtd_notation_desc(doc.ext_subset.as_deref(), value));

            if nota.is_none() {
                xml_err_valid_node(
                    Some(ctxt),
                    Some(doc.into()),
                    XmlParserErrors::XmlDTDUnknownNotation,
                    format!(
                        "NOTATION attribute {name} reference an unknown notation \"{value}\"\n"
                    )
                    .as_str(),
                    Some(name),
                    Some(value),
                    None,
                );
                ret = 0;
            }
        }
    }
    ret
}

fn xml_validate_attribute_callback(cur: XmlAttributePtr, ctxt: &mut XmlValidCtxt) {
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
            if let Some(def) = cur.default_value.as_deref() {
                ret = xml_validate_attribute_value2(
                    ctxt,
                    ctxt.doc.unwrap(),
                    cur.name.as_deref().unwrap(),
                    cur.atype,
                    def,
                );
                if ret == 0 && ctxt.valid == 1 {
                    ctxt.valid = 0;
                }
            }
            if cur.tree.is_some() {
                let mut tree = cur.tree.as_deref();
                while let Some(now) = tree {
                    ret = xml_validate_attribute_value2(
                        ctxt,
                        ctxt.doc.unwrap(),
                        cur.name.as_deref().unwrap(),
                        cur.atype,
                        &now.name,
                    );
                    if ret == 0 && ctxt.valid == 1 {
                        ctxt.valid = 0;
                    }
                    tree = now.next.as_deref();
                }
            }
        }
    }
    if matches!(cur.atype, XmlAttributeType::XmlAttributeNotation) {
        let doc = cur.doc;
        let Some(cur_elem) = cur.elem.as_deref() else {
            xml_err_valid!(
                Some(ctxt),
                XmlParserErrors::XmlErrInternalError,
                "xmlValidateAttributeCallback({}): internal error\n",
                cur.name().as_deref().unwrap()
            );
            return;
        };

        let mut elem = None;
        if let Some(doc) = doc {
            elem = xml_get_dtd_element_desc(doc.int_subset, cur_elem);
            if elem.is_none() {
                elem = xml_get_dtd_element_desc(doc.ext_subset, cur_elem);
            }
        }
        if elem.is_none() {
            if let Some(dtd) = cur
                .parent
                .filter(|dtd| dtd.element_type() == XmlElementType::XmlDTDNode)
            {
                elem = xml_get_dtd_element_desc(Some(dtd), cur_elem);
            }
        }
        let Some(elem) = elem else {
            let name = cur.name().unwrap();
            xml_err_valid_node(
                Some(ctxt),
                None,
                XmlParserErrors::XmlDTDUnknownElem,
                format!(
                    "attribute {}: could not find decl for element {}\n",
                    name, cur_elem
                )
                .as_str(),
                Some(&name),
                Some(cur_elem),
                None,
            );
            return;
        };
        if matches!(elem.etype, XmlElementTypeVal::XmlElementTypeEmpty) {
            let name = cur.name().unwrap();
            xml_err_valid_node(
                Some(ctxt),
                None,
                XmlParserErrors::XmlDTDEmptyNotation,
                format!(
                    "NOTATION attribute {} declared for EMPTY element {}\n",
                    name, cur_elem
                )
                .as_str(),
                Some(&name),
                Some(cur_elem),
                None,
            );
            ctxt.valid = 0;
        }
    }
}

fn xml_validate_notation_callback(cur: XmlEntityPtr, ctxt: &mut XmlValidCtxt) {
    if matches!(cur.etype, XmlEntityType::XmlExternalGeneralUnparsedEntity) {
        if let Some(notation) = cur.content.as_deref() {
            let ret: i32 = xml_validate_notation_use(Some(ctxt), cur.doc.unwrap(), notation);
            if ret != 1 {
                ctxt.valid = 0;
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
pub fn xml_validate_dtd_final(ctxt: &mut XmlValidCtxt, doc: XmlDocPtr) -> i32 {
    if doc.int_subset.is_none() && doc.ext_subset.is_none() {
        return 0;
    }
    ctxt.doc = Some(doc);
    ctxt.valid = 1;
    let dtd = doc.int_subset;
    if let Some(dtd) = dtd {
        for &attr in dtd.attributes.values() {
            xml_validate_attribute_callback(attr, ctxt);
        }
        for &entity in dtd.entities.values() {
            xml_validate_notation_callback(entity, ctxt);
        }
    }
    let dtd = doc.ext_subset;
    if let Some(dtd) = dtd {
        for &attr in dtd.attributes.values() {
            xml_validate_attribute_callback(attr, ctxt);
        }
        for &entity in dtd.entities.values() {
            xml_validate_notation_callback(entity, ctxt);
        }
    }
    ctxt.valid
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
pub fn xml_validate_document(ctxt: &mut XmlValidCtxt, mut doc: XmlDocPtr) -> i32 {
    use crate::{parser::xml_parse_dtd, uri::build_uri};

    let mut ret: i32;

    if doc.int_subset.is_none() && doc.ext_subset.is_none() {
        xml_err_valid!(Some(ctxt), XmlParserErrors::XmlDTDNoDTD, "no DTD found!\n");
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
                    Some(ctxt),
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
                    Some(ctxt),
                    XmlParserErrors::XmlDTDLoadError,
                    "Could not load the external subset \"{}\"\n",
                    system_id
                );
            } else {
                xml_err_valid!(
                    Some(ctxt),
                    XmlParserErrors::XmlDTDLoadError,
                    "Could not load the external subset \"{}\"\n",
                    external_id.unwrap()
                );
            }
            return 0;
        }
    }

    doc.ids.clear();
    doc.refs.clear();
    ret = xml_validate_dtd_final(ctxt, doc);
    if xml_validate_root(Some(ctxt), doc) == 0 {
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
pub fn xml_validate_element(
    ctxt: &mut XmlValidCtxt,
    doc: XmlDocPtr,
    root: Option<XmlGenericNodePtr>,
) -> i32 {
    let mut ret: i32 = 1;

    let Some(root) = root else {
        return 0;
    };

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
                let value = now.children().and_then(|c| c.get_string(Some(doc), 0));
                ret &= xml_validate_one_attribute(ctxt, doc, node, Some(now), &value.unwrap());
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
                        &now.href().unwrap(),
                    );
                } else {
                    ret &= xml_validate_one_namespace(
                        ctxt,
                        doc,
                        node,
                        None,
                        now,
                        &now.href().unwrap(),
                    );
                }
                ns = now.next;
            }

            if let Some(children) = elem.children() {
                elem = children;
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
            elem = elem.parent().unwrap();
        }
        elem = elem.next().unwrap();
    }

    // done:
    // return ret;
}

/// Finds a declaration associated to an element in the document.
///
/// returns the pointer to the declaration or null_mut() if not found.
#[doc(alias = "xmlValidGetElemDecl")]
fn xml_valid_get_elem_decl(
    ctxt: &mut XmlValidCtxt,
    doc: XmlDocPtr,
    elem: XmlNodePtr,
    extsubset: &mut i32,
) -> Option<XmlElementPtr> {
    *extsubset = 0;

    // Fetch the declaration for the qualified name
    let prefix = elem.ns.as_deref().and_then(|ns| ns.prefix());
    let mut elem_decl = None;
    if let Some(prefix) = prefix {
        elem_decl = xml_get_dtd_qelement_desc(doc.int_subset, &elem.name().unwrap(), Some(&prefix));
        if elem_decl.is_none() && doc.ext_subset.is_some() {
            elem_decl =
                xml_get_dtd_qelement_desc(doc.ext_subset, &elem.name().unwrap(), Some(&prefix));
            if elem_decl.is_some() {
                *extsubset = 1;
            }
        }
    }

    // Fetch the declaration for the non qualified name
    // This is "non-strict" validation should be done on the
    // full QName but in that case being flexible makes sense.
    if elem_decl.is_none() {
        elem_decl = xml_get_dtd_element_desc(doc.int_subset, &elem.name().unwrap());
        if elem_decl.is_none() && doc.ext_subset.is_some() {
            elem_decl = xml_get_dtd_element_desc(doc.ext_subset, &elem.name().unwrap());
            if elem_decl.is_some() {
                *extsubset = 1;
            }
        }
    }
    if elem_decl.is_none() {
        let name = elem.name().unwrap();
        xml_err_valid_node(
            Some(&mut *ctxt),
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

fn node_vpush(ctxt: &mut XmlValidCtxt, value: XmlNodePtr) -> i32 {
    ctxt.node_tab.push(value);
    ctxt.node = Some(value);
    ctxt.node_tab.len() as i32 - 1
}

fn node_vpop(ctxt: &mut XmlValidCtxt) -> Option<XmlNodePtr> {
    let res = ctxt.node_tab.pop()?;
    ctxt.node = ctxt.node_tab.last().cloned();
    Some(res)
}

/// Check that an element follows #CDATA
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateCdataElement")]
fn xml_validate_one_cdata_element(
    ctxt: &mut XmlValidCtxt,
    _doc: XmlDocPtr,
    elem: XmlNodePtr,
) -> i32 {
    let mut ret: i32 = 1;

    if !matches!(elem.element_type(), XmlElementType::XmlElementNode) {
        return 0;
    }

    let child = elem.children;

    let mut cur = child;
    'done: while let Some(now) = cur {
        match now.element_type() {
            XmlElementType::XmlEntityRefNode => {
                let now = XmlNodePtr::try_from(now).unwrap();
                // Push the current node to be able to roll back
                // and process within the entity
                if let Some(children) = now
                    .children
                    .filter(|children| children.children().is_some())
                {
                    node_vpush(ctxt, now);
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
        cur = now.next();
        while cur.is_none() {
            cur = node_vpop(ctxt).map(|node| node.into());
            let Some(now) = cur else {
                break;
            };
            cur = now.next();
        }
    }
    // done:
    ctxt.node_tab.clear();
    ret
}

/// This will dump the list of elements to the buffer
/// Intended just for the debug routine
#[doc(alias = "xmlSnprintfElements")]
pub(crate) fn xml_snprintf_elements(
    buf: &mut String,
    size: usize,
    node: Option<XmlGenericNodePtr>,
    glob: i32,
) {
    if node.is_none() {
        return;
    }
    if glob != 0 {
        buf.push('(');
    }
    let mut cur = node;
    while let Some(cur_node) = cur {
        if size - buf.len() < 50 {
            if size - buf.len() > 4 && !buf.ends_with('.') {
                buf.push_str(" ...");
            }
            return;
        }
        match cur_node.element_type() {
            XmlElementType::XmlElementNode => {
                let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                if let Some(prefix) = cur_node.ns.as_deref().and_then(|ns| ns.prefix()) {
                    if size - buf.len() < prefix.len() + 10 {
                        if size - buf.len() > 4 && !buf.ends_with('.') {
                            buf.push_str(" ...");
                        }
                        return;
                    }
                    buf.push_str(&prefix);
                    buf.push(':');
                }
                if size - buf.len() < cur_node.name.len() + 10 {
                    if size - buf.len() > 4 && !buf.ends_with('.') {
                        buf.push_str(" ...");
                    }
                    return;
                }
                buf.push_str(&cur_node.name);
                if cur_node.next.is_some() {
                    buf.push(' ');
                }
            }
            ty @ XmlElementType::XmlTextNode
            | ty @ XmlElementType::XmlCDATASectionNode
            | ty @ XmlElementType::XmlEntityRefNode => 'to_break: {
                let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                if matches!(ty, XmlElementType::XmlTextNode) && cur_node.is_blank_node() {
                    break 'to_break;
                }
                buf.push_str("CDATA");
                if cur_node.next.is_some() {
                    buf.push(' ');
                }
            }
            XmlElementType::XmlAttributeNode
            | XmlElementType::XmlDocumentNode
            | XmlElementType::XmlHTMLDocumentNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlNamespaceDecl => {
                buf.push_str("???");
                if cur_node.next().is_some() {
                    buf.push(' ');
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
        cur = cur_node.next();
    }
    if glob != 0 {
        buf.push(')');
    }
}

/// Try to validate the content model of an element
///
/// returns 1 if valid or 0 if not and -1 in case of error
#[doc(alias = "xmlValidateElementContent")]
fn xml_validate_element_content(
    ctxt: &mut XmlValidCtxt,
    child: Option<XmlGenericNodePtr>,
    elem_decl: XmlElementPtr,
    warn: i32,
    parent: XmlNodePtr,
) -> i32 {
    let mut ret: i32;

    // let cont = elem_decl.content.clone();
    let name = elem_decl.name.as_deref();

    #[cfg(feature = "libxml_regexp")]
    {
        // Build the regexp associated to the content model
        if elem_decl.cont_model.is_none() {
            xml_valid_build_content_model(ctxt, elem_decl);
        }
        let Some(cont_model) = elem_decl.cont_model.clone() else {
            return -1;
        };
        if cont_model.is_determinist() == 0 {
            return -1;
        }
        ctxt.node_tab.clear();
        let mut exec = XmlRegExecCtxt::new(cont_model, None, null_mut());
        let mut cur = child;
        'fail: {
            while let Some(cur_node) = cur {
                match cur_node.element_type() {
                    XmlElementType::XmlEntityRefNode => {
                        let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                        // Push the current node to be able to roll back
                        // and process within the entity
                        if let Some(children) = cur_node
                            .children
                            .filter(|children| children.children().is_some())
                        {
                            node_vpush(ctxt, cur_node);
                            cur = children.children();
                            continue;
                        }
                    }
                    XmlElementType::XmlTextNode => {
                        let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                        if cur_node.is_blank_node() {
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
                        let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                        if let Some(prefix) = cur_node.ns.as_deref().and_then(|ns| ns.prefix()) {
                            let name = cur_node.name().unwrap();
                            let fullname = build_qname(&name, Some(&prefix));
                            // ret =
                            exec.push_string(Some(&fullname), null_mut());
                        } else {
                            // ret =
                            exec.push_string(cur_node.name().as_deref(), null_mut());
                        }
                    }
                    _ => {}
                }
                // Switch to next element
                cur = cur_node.next();
                while cur.is_none() {
                    cur = node_vpop(ctxt).map(|node| node.into());
                    if cur.is_none() {
                        break;
                    }
                    cur = cur_node.next();
                }
            }

            ret = exec.push_string(None, null_mut());
        }
    }

    if warn != 0 && (ret != 1 && ret != -3) {
        let expr = format!("{:?}", elem_decl.content.as_ref().unwrap());
        let mut list = String::with_capacity(5000);

        #[cfg(feature = "libxml_regexp")]
        {
            xml_snprintf_elements(&mut list, 5000, child, 1);
        }

        if let Some(name) = name {
            xml_err_valid_node(
                Some(&mut *ctxt),
                Some(parent.into()),
                XmlParserErrors::XmlDTDContentModel,
                format!(
                    "Element {} content does not follow the DTD, expecting {}, got {}\n",
                    name, expr, list
                )
                .as_str(),
                Some(name),
                Some(&expr),
                Some(&list),
            );
        } else {
            xml_err_valid_node(
                Some(&mut *ctxt),
                Some(parent.into()),
                XmlParserErrors::XmlDTDContentModel,
                format!(
                    "Element content does not follow the DTD, expecting {}, got {}\n",
                    expr, list
                )
                .as_str(),
                Some(&expr),
                Some(&list),
                None,
            );
        }
        ret = 0;
    }
    if ret == -3 {
        ret = 1;
    }

    ctxt.node_tab.clear();
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
pub fn xml_validate_one_element(
    ctxt: &mut XmlValidCtxt,
    doc: XmlDocPtr,
    elem: Option<XmlGenericNodePtr>,
) -> i32 {
    let mut ret: i32 = 1;
    let tmp: i32;
    let mut extsubset: i32 = 0;

    if doc.int_subset.is_none() && doc.ext_subset.is_none() {
        return 0;
    };

    let Some(elem) = elem else {
        return 0;
    };
    match elem.element_type() {
        XmlElementType::XmlAttributeNode => {
            xml_err_valid_node(
                Some(&mut *ctxt),
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
                    Some(&mut *ctxt),
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
                    Some(&mut *ctxt),
                    Some(elem.into()),
                    XmlParserErrors::XmlErrInternalError,
                    "Text element has namespace !\n",
                    None,
                    None,
                    None,
                );
                return 0;
            }
            if elem.content.is_none() {
                xml_err_valid_node(
                    Some(&mut *ctxt),
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
                Some(&mut *ctxt),
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
                Some(&mut *ctxt),
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
                Some(&mut *ctxt),
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
                Some(&mut *ctxt),
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
                Some(&mut *ctxt),
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
    let Some(elem_decl) = xml_valid_get_elem_decl(ctxt, doc, elem, &mut extsubset) else {
        return 0;
    };

    // If vstate_nr is not zero that means continuous validation is
    // activated, do not try to check the content model at that level.
    if ctxt.vstate_tab.is_empty() {
        // Check that the element content matches the definition
        match elem_decl.etype {
            XmlElementTypeVal::XmlElementTypeUndefined => {
                let name = elem.name().unwrap();
                xml_err_valid_node(
                    Some(&mut *ctxt),
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
                        Some(&mut *ctxt),
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
                if elem_decl
                    .content
                    .as_deref()
                    .is_some_and(|cont| cont.r#type() == ElementContentType::PCDATA)
                {
                    ret = xml_validate_one_cdata_element(ctxt, doc, elem);
                    if ret == 0 {
                        let name = elem.name().unwrap();
                        xml_err_valid_node(
                            Some(&mut *ctxt),
                            Some(elem.into()),
                            XmlParserErrors::XmlDTDNotPCDATA,
                            format!(
                                "Element {} was declared #PCDATA but contains non text nodes\n",
                                name
                            )
                            .as_str(),
                            Some(&name),
                            None,
                            None,
                        );
                    }
                } else {
                    let mut child = elem.children;
                    // Hum, this start to get messy
                    while let Some(cur_node) = child {
                        if matches!(cur_node.element_type(), XmlElementType::XmlElementNode) {
                            let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                            let name = cur_node.name.clone();
                            if let Some(prefix) = cur_node.ns.as_deref().and_then(|ns| ns.prefix())
                            {
                                let fullname = build_qname(&cur_node.name, Some(&prefix));
                                let mut cont = elem_decl.content.clone();
                                while let Some(now) = cont.clone() {
                                    if now.r#type() == ElementContentType::Element {
                                        if now.name().as_deref() == Some(&fullname) {
                                            break;
                                        }
                                    } else if let Some(c1) = now.first_child().filter(|c1| {
                                        c1.r#type() == ElementContentType::Element
                                            && now.r#type() == ElementContentType::Or
                                    }) {
                                        if c1.name().as_deref() == Some(&fullname) {
                                            break;
                                        }
                                    } else if now.r#type() != ElementContentType::Or
                                        || now.first_child().is_none_or(|c1| {
                                            c1.r#type() != ElementContentType::PCDATA
                                        })
                                    {
                                        xml_err_valid!(
                                            None::<&mut XmlValidCtxt>,
                                            XmlParserErrors::XmlDTDMixedCorrupt,
                                            "Internal: MIXED struct corrupted\n"
                                        );
                                        break;
                                    }
                                    cont = now.second_child();
                                }
                                if cont.is_some() {
                                    child = cur_node.next();
                                    continue;
                                }
                            }

                            let mut cont = elem_decl.content.clone();
                            while let Some(now) = cont.clone() {
                                if now.r#type() == ElementContentType::Element {
                                    if now.name().as_deref() == Some(&name) {
                                        break;
                                    }
                                } else if let Some(c1) = now.first_child().filter(|c1| {
                                    c1.r#type() == ElementContentType::Element
                                        && now.r#type() == ElementContentType::Or
                                }) {
                                    if c1.name().as_deref() == Some(&name) {
                                        break;
                                    }
                                } else if now.r#type() != ElementContentType::Or
                                    || now
                                        .first_child()
                                        .is_none_or(|c1| c1.r#type() != ElementContentType::PCDATA)
                                {
                                    xml_err_valid!(
                                        Some(&mut *ctxt),
                                        XmlParserErrors::XmlDTDMixedCorrupt,
                                        "Internal: MIXED struct corrupted\n"
                                    );
                                    break;
                                }
                                cont = now.second_child();
                            }
                            if cont.is_none() {
                                let elem_name = elem.name().unwrap();
                                xml_err_valid_node(
                                    Some(&mut *ctxt),
                                    Some(elem.into()),
                                    XmlParserErrors::XmlDTDInvalidChild,
                                    format!(
                                        "Element {} is not declared in {} list of possible children\n",
                                        name,
                                        elem_name
                                    ).as_str(),
                                    Some(&name),
                                    Some(&elem_name),
                                    None
                                );
                                ret = 0;
                            }
                        }
                        // child_ok:
                        child = cur_node.next();
                    }
                }
            }
            XmlElementTypeVal::XmlElementTypeElement => {
                if doc.standalone == 1 && extsubset == 1 {
                    // VC: Standalone Document Declaration
                    //     - element types with element content, if white space
                    //       occurs directly within any instance of those types.
                    let mut child = elem.children();
                    while let Some(cur_node) = child {
                        if matches!(cur_node.element_type(), XmlElementType::XmlTextNode) {
                            let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                            let mut content = cur_node.content.as_deref().unwrap();
                            content = content.trim_start_matches(|c: char| c.is_xml_blank_char());
                            if content.is_empty() {
                                let name = elem.name().unwrap();
                                xml_err_valid_node(
                                    Some(&mut *ctxt),
                                    Some(elem.into()),
                                    XmlParserErrors::XmlDTDStandaloneWhiteSpace,
                                    format!(
                                        "standalone: {} declared in the external subset contains white spaces nodes\n",
                                        name
                                    ).as_str(),
                                    Some(&name),
                                    None,
                                    None
                                );
                                ret = 0;
                                break;
                            }
                        }
                        child = cur_node.next();
                    }
                }
                let child = elem.children;
                // cont = (*elem_decl).content;
                tmp = xml_validate_element_content(ctxt, child, elem_decl, 1, elem);
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

                if cur_attr.prefix.is_none() && cur_attr.name.as_deref() == Some("xmlns") {
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
                        if attr.name().as_deref() == cur_attr.name.as_deref() {
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
                            Some(&mut *ctxt),
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
                            Some(&mut *ctxt),
                            Some(elem.into()),
                            XmlParserErrors::XmlDTDMissingAttribute,
                            format!(
                                "Element {} does not carry attribute {}:{}\n",
                                elem_name, prefix, attr_name
                            )
                            .as_str(),
                            Some(&elem_name),
                            Some(prefix),
                            Some(&attr_name),
                        );
                        ret = 0;
                    }
                } else if qualified == 0 {
                    xml_err_valid_warning!(
                        Some(&mut *ctxt),
                        Some(elem.into()),
                        XmlParserErrors::XmlDTDNoPrefix,
                        "Element {} required attribute {}:{} has no prefix\n",
                        elem.name().unwrap().into_owned(),
                        cur_attr.prefix.as_deref().unwrap(),
                        cur_attr.name().unwrap().into_owned()
                    );
                } else if qualified == 1 {
                    xml_err_valid_warning!(
                        Some(&mut *ctxt),
                        Some(elem.into()),
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
                if cur_attr.prefix.is_none() && cur_attr.name.as_deref() == Some("xmlns") {
                    let mut ns = elem.ns_def;
                    while let Some(now) = ns {
                        if now.prefix().is_none() {
                            if cur_attr.default_value.as_deref() != now.href.as_deref() {
                                let elem_name = elem.name().unwrap();
                                xml_err_valid_node(
                                    Some(&mut *ctxt),
                                    Some(elem.into()),
                                    XmlParserErrors::XmlDTDElemDefaultNamespace,
                                    format!("Element {} namespace name for default namespace does not match the DTD\n",elem_name).as_str(),
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
                            if cur_attr.default_value.as_deref() != now.href.as_deref() {
                                let elem_name = elem.name().unwrap();
                                let prefix = now.prefix().unwrap();
                                xml_err_valid_node(
                                    Some(&mut *ctxt),
                                    Some(elem.into()),
                                    XmlParserErrors::XmlDTDElemNamespace,
                                    format!(
                                        "Element {} namespace name for {} does not match the DTD\n",
                                        elem_name, prefix
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
pub fn xml_validate_one_attribute(
    ctxt: &mut XmlValidCtxt,
    doc: XmlDocPtr,
    elem: XmlNodePtr,
    attr: Option<XmlAttrPtr>,
    value: &str,
) -> i32 {
    let mut ret: i32 = 1;

    if doc.int_subset.is_none() && doc.ext_subset.is_none() {
        return 0;
    };
    let Some(mut attr) = attr else {
        return 0;
    };

    let mut attr_decl = None;
    if let Some(prefix) = elem.ns.as_deref().and_then(|ns| ns.prefix()) {
        let name = elem.name().unwrap();
        let fullname = build_qname(&name, Some(&prefix));

        if let Some(attr_ns) = attr.ns {
            attr_decl = doc.int_subset.and_then(|dtd| {
                dtd.get_qattr_desc(
                    &fullname,
                    attr.name().as_deref().unwrap(),
                    attr_ns.prefix().as_deref(),
                )
            });
            if attr_decl.is_none() && doc.ext_subset.is_some() {
                attr_decl = doc.ext_subset.and_then(|dtd| {
                    dtd.get_qattr_desc(
                        &fullname,
                        attr.name().as_deref().unwrap(),
                        attr_ns.prefix().as_deref(),
                    )
                });
            }
        } else {
            attr_decl = doc
                .int_subset
                .and_then(|dtd| dtd.get_attr_desc(&fullname, attr.name().as_deref().unwrap()));
            if attr_decl.is_none() && doc.ext_subset.is_some() {
                attr_decl = doc
                    .ext_subset
                    .and_then(|dtd| dtd.get_attr_desc(&fullname, attr.name().as_deref().unwrap()));
            }
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
            Some(&mut *ctxt),
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
            Some(&mut *ctxt),
            Some(elem.into()),
            XmlParserErrors::XmlDTDAttributeValue,
            format!(
                "Syntax of value for attribute {} of {} is not valid\n",
                attr_name, elem_name
            )
            .as_str(),
            Some(&attr_name),
            Some(&elem_name),
            None,
        );
        ret = 0;
    }

    // Validity constraint: Fixed Attribute Default
    if matches!(attr_decl.def, XmlAttributeDefault::XmlAttributeFixed)
        && Some(value) != attr_decl.default_value.as_deref()
    {
        let attr_name = attr.name().unwrap();
        let elem_name = elem.name().unwrap();
        let def_value = attr_decl.default_value.as_deref().unwrap();
        xml_err_valid_node(
            Some(ctxt),
            Some(elem.into()),
            XmlParserErrors::XmlDTDAttributeDefault,
            format!(
                "Value for attribute {} of {} is different from default \"{}\"\n",
                attr_name, elem_name, def_value
            )
            .as_str(),
            Some(&attr_name),
            Some(&elem_name),
            Some(def_value),
        );
        ret = 0;
    }

    // Validity Constraint: ID uniqueness
    if matches!(attr_decl.atype, XmlAttributeType::XmlAttributeID)
        && xml_add_id(Some(ctxt), doc, value, attr).is_none()
    {
        ret = 0;
    }

    if matches!(
        attr_decl.atype,
        XmlAttributeType::XmlAttributeIDREF | XmlAttributeType::XmlAttributeIDREFS
    ) && xml_add_ref(Some(ctxt), doc, value, attr).is_none()
    {
        ret = 0;
    }

    // Validity Constraint: Notation Attributes
    if matches!(attr_decl.atype, XmlAttributeType::XmlAttributeNotation) {
        let mut tree = attr_decl.tree.as_deref();

        // First check that the given NOTATION was declared
        let nota = xml_get_dtd_notation_desc(doc.int_subset.as_deref(), value)
            .or_else(|| xml_get_dtd_notation_desc(doc.ext_subset.as_deref(), value));

        if nota.is_none() {
            let attr_name = attr.name().unwrap();
            let elem_name = elem.name().unwrap();
            xml_err_valid_node(
                Some(&mut *ctxt),
                Some(elem.into()),
                XmlParserErrors::XmlDTDUnknownNotation,
                format!(
                    "Value \"{}\" for attribute {} of {} is not a declared Notation\n",
                    value, attr_name, elem_name
                )
                .as_str(),
                Some(value),
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
                Some(ctxt),
                Some(elem.into()),
                XmlParserErrors::XmlDTDNotationValue,
                format!(
                    "Value \"{}\" for attribute {} of {} is not among the enumerated notations\n",
                    value, attr_name, elem_name,
                )
                .as_str(),
                Some(value),
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
            if now.name == value {
                break;
            }
            tree = now.next.as_deref();
        }
        if tree.is_none() {
            let attr_name = attr.name().unwrap();
            let elem_name = elem.name().unwrap();
            xml_err_valid_node(
                Some(ctxt),
                Some(elem.into()),
                XmlParserErrors::XmlDTDAttributeValue,
                format!(
                    "Value \"{}\" for attribute {} of {} is not among the enumerated set\n",
                    value, attr_name, elem_name
                )
                .as_str(),
                Some(value),
                Some(&attr_name),
                Some(&elem_name),
            );
            ret = 0;
        }
    }

    // Fixed Attribute Default
    if matches!(attr_decl.def, XmlAttributeDefault::XmlAttributeFixed)
        && attr_decl.default_value.as_deref() != Some(value)
    {
        let attr_name = attr.name().unwrap();
        let elem_name = elem.name().unwrap();
        let def_value = attr_decl.default_value.as_deref().unwrap();
        xml_err_valid_node(
            Some(ctxt),
            Some(elem.into()),
            XmlParserErrors::XmlDTDAttributeValue,
            format!(
                "Value for attribute {} of {} must be \"{}\"\n",
                attr_name, elem_name, def_value
            )
            .as_str(),
            Some(&attr_name),
            Some(&elem_name),
            Some(def_value),
        );
        ret = 0;
    }

    // Extra check for the attribute value
    ret &= xml_validate_attribute_value2(
        ctxt,
        doc,
        attr.name().as_deref().unwrap(),
        attr_decl.atype,
        value,
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
pub fn xml_validate_one_namespace(
    ctxt: &mut XmlValidCtxt,
    doc: XmlDocPtr,
    elem: XmlNodePtr,
    prefix: Option<&str>,
    ns: XmlNsPtr,
    value: &str,
) -> i32 {
    let mut ret: i32 = 1;

    if doc.int_subset.is_none() && doc.ext_subset.is_none() {
        return 0;
    };
    if ns.href.is_none() {
        return 0;
    }

    let mut attr_decl = None;
    if let Some(prefix) = prefix {
        let fullname = build_qname(&elem.name, Some(prefix));

        if let Some(prefix) = ns.prefix() {
            attr_decl = doc
                .int_subset
                .and_then(|dtd| dtd.get_qattr_desc(&fullname, &prefix, Some("xmlns")));
            if attr_decl.is_none() && doc.ext_subset.is_some() {
                attr_decl = doc
                    .ext_subset
                    .and_then(|dtd| dtd.get_qattr_desc(&fullname, &prefix, Some("xmlns")));
            }
        } else {
            attr_decl = doc
                .int_subset
                .and_then(|dtd| dtd.get_attr_desc(&fullname, "xmlns"));
            if attr_decl.is_none() && doc.ext_subset.is_some() {
                attr_decl = doc
                    .ext_subset
                    .and_then(|dtd| dtd.get_attr_desc(&fullname, "xmlns"));
            }
        }
    }
    if attr_decl.is_none() {
        if let Some(prefix) = ns.prefix() {
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
        if let Some(prefix) = ns.prefix() {
            let elem_name = elem.name().unwrap();
            xml_err_valid_node(
                Some(&mut *ctxt),
                Some(elem.into()),
                XmlParserErrors::XmlDTDUnknownAttribute,
                format!(
                    "No declaration for attribute xmlns:{} of element {}\n",
                    prefix, elem_name
                )
                .as_str(),
                Some(&prefix),
                Some(&elem_name),
                None,
            );
        } else {
            let elem_name = elem.name().unwrap();
            xml_err_valid_node(
                Some(&mut *ctxt),
                Some(elem.into()),
                XmlParserErrors::XmlDTDUnknownAttribute,
                format!(
                    "No declaration for attribute xmlns of element {}\n",
                    elem_name
                )
                .as_str(),
                Some(&elem_name),
                None,
                None,
            );
        }
        return 0;
    };

    let val: i32 = xml_validate_attribute_value_internal(Some(doc), attr_decl.atype, value);
    if val == 0 {
        if let Some(prefix) = ns.prefix() {
            let elem_name = elem.name().unwrap();
            xml_err_valid_node(
                Some(&mut *ctxt),
                Some(elem.into()),
                XmlParserErrors::XmlDTDInvalidDefault,
                format!(
                    "Syntax of value for attribute xmlns:{} of {} is not valid\n",
                    prefix, elem_name
                )
                .as_str(),
                Some(&prefix),
                Some(&elem_name),
                None,
            );
        } else {
            let elem_name = elem.name().unwrap();
            xml_err_valid_node(
                Some(&mut *ctxt),
                Some(elem.into()),
                XmlParserErrors::XmlDTDInvalidDefault,
                format!(
                    "Syntax of value for attribute xmlns of {} is not valid\n",
                    elem_name
                )
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
        && Some(value) != attr_decl.default_value.as_deref()
    {
        if let Some(prefix) = ns.prefix() {
            let elem_name = elem.name().unwrap();
            let def_value = attr_decl.default_value.as_deref().unwrap();
            xml_err_valid_node(
            Some(&mut *ctxt),
            Some(elem.into()),
            XmlParserErrors::XmlDTDAttributeDefault,
            format!("Value for attribute xmlns:{prefix} of {elem_name} is different from default \"{def_value}\"\n").as_str(),
            Some(&prefix),
            Some(&elem_name),
            Some(def_value),
        );
        } else {
            let elem_name = elem.name().unwrap();
            let def_value = attr_decl.default_value.as_deref().unwrap();
            xml_err_valid_node(
                Some(&mut *ctxt),
                Some(elem.into()),
                XmlParserErrors::XmlDTDAttributeDefault,
                format!(
                    "Value for attribute xmlns of {} is different from default \"{}\"\n",
                    elem_name, def_value
                )
                .as_str(),
                Some(&elem_name),
                Some(def_value),
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

        // First check that the given NOTATION was declared
        let nota = xml_get_dtd_notation_desc(doc.int_subset.as_deref(), value)
            .or_else(|| xml_get_dtd_notation_desc(doc.ext_subset.as_deref(), value));

        if nota.is_none() {
            if let Some(prefix) = ns.prefix() {
                let elem_name = elem.name().unwrap();
                xml_err_valid_node(
                    Some(&mut *ctxt),
                    Some(elem.into()),
                    XmlParserErrors::XmlDTDUnknownNotation,
                    format!(
                        "Value \"{}\" for attribute xmlns:{} of {} is not a declared Notation\n",
                        value, prefix, elem_name,
                    )
                    .as_str(),
                    Some(value),
                    Some(&prefix),
                    Some(&elem_name),
                );
            } else {
                let elem_name = elem.name().unwrap();
                xml_err_valid_node(
                    Some(&mut *ctxt),
                    Some(elem.into()),
                    XmlParserErrors::XmlDTDUnknownNotation,
                    format!(
                        "Value \"{}\" for attribute xmlns of {} is not a declared Notation\n",
                        value, elem_name,
                    )
                    .as_str(),
                    Some(value),
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
            if let Some(prefix) = ns.prefix() {
                xml_err_valid_node(
                    Some(&mut *ctxt),
                    Some(elem.into()),
                    XmlParserErrors::XmlDTDNotationValue,
                    format!(
                        "Value \"{}\" for attribute xmlns:{} of {} is not among the enumerated notations\n",
                        value,
                        prefix,
                        elem_name,
                    ).as_str(),
                    Some(value),
                    Some(&prefix),
                    Some(&elem_name),
                );
            } else {
                xml_err_valid_node(
                    Some(&mut *ctxt),
                    Some(elem.into()),
                    XmlParserErrors::XmlDTDNotationValue,
                    format!(
                        "Value \"{}\" for attribute xmlns of {} is not among the enumerated notations\n",
                        value,
                        elem_name,
                    ).as_str(),
                    Some(value),
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
            if now.name == value {
                break;
            }
            tree = now.next.as_deref();
        }
        if tree.is_none() {
            if let Some(prefix) = ns.prefix() {
                let elem_name = elem.name().unwrap();
                xml_err_valid_node(
                    Some(&mut *ctxt),
                    Some(elem.into()),
                    XmlParserErrors::XmlDTDAttributeValue,
                    format!(
                        "Value \"{}\" for attribute xmlns:{} of {} is not among the enumerated set\n",
                        value,
                        prefix,
                        elem_name,
                    ).as_str(),
                    Some(value),
                    Some(&prefix),
                    Some(&elem_name),
                );
            } else {
                let elem_name = elem.name().unwrap();
                xml_err_valid_node(
                    Some(&mut *ctxt),
                    Some(elem.into()),
                    XmlParserErrors::XmlDTDAttributeValue,
                    format!(
                        "Value \"{}\" for attribute xmlns of {} is not among the enumerated set\n",
                        value, elem_name,
                    )
                    .as_str(),
                    Some(value),
                    Some(&elem_name),
                    None,
                );
            }
            ret = 0;
        }
    }

    // Fixed Attribute Default
    if matches!(attr_decl.def, XmlAttributeDefault::XmlAttributeFixed)
        && attr_decl.default_value.as_deref() != Some(value)
    {
        if let Some(prefix) = ns.prefix() {
            let elem_name = elem.name().unwrap();
            let def_value = attr_decl.default_value.as_deref().unwrap();
            xml_err_valid_node(
                Some(&mut *ctxt),
                Some(elem.into()),
                XmlParserErrors::XmlDTDElemNamespace,
                format!(
                    "Value for attribute xmlns:{} of {} must be \"{}\"\n",
                    prefix, elem_name, def_value
                )
                .as_str(),
                Some(&prefix),
                Some(&elem_name),
                Some(def_value),
            );
        } else {
            let elem_name = elem.name().unwrap();
            let def_value = attr_decl.default_value.as_deref().unwrap();
            xml_err_valid_node(
                Some(&mut *ctxt),
                Some(elem.into()),
                XmlParserErrors::XmlDTDElemNamespace,
                format!(
                    "Value for attribute xmlns of {} must be \"{}\"\n",
                    elem_name, def_value
                )
                .as_str(),
                Some(&elem_name),
                Some(def_value),
                None,
            );
        }
        ret = 0;
    }

    // Extra check for the attribute value
    if let Some(prefix) = ns.prefix() {
        ret &= xml_validate_attribute_value2(ctxt, doc, &prefix, attr_decl.atype, value);
    } else {
        ret &= xml_validate_attribute_value2(ctxt, doc, "xmlns", attr_decl.atype, value);
    }

    ret
}

#[doc(alias = "xmlValidateRef")]
fn xml_validate_ref(refe: &XmlRef, ctxt: &mut XmlValidCtxt, name: &str) {
    if refe.attr.is_none() && refe.name.is_none() {
        return;
    }
    if let Some(attr) = refe.attr {
        if matches!(attr.atype, Some(XmlAttributeType::XmlAttributeIDREF)) {
            if xml_get_id(ctxt.doc.unwrap(), name).is_none() {
                let attr_name = attr.name().unwrap();
                xml_err_valid_node(
                    Some(ctxt),
                    attr.parent.map(|p| p.into()),
                    XmlParserErrors::XmlDTDUnknownID,
                    format!(
                        "IDREF attribute {} references an unknown ID \"{}\"\n",
                        attr_name, name
                    )
                    .as_str(),
                    Some(&attr_name),
                    Some(name),
                    None,
                );
                ctxt.valid = 0;
            }
        } else if matches!(attr.atype, Some(XmlAttributeType::XmlAttributeIDREFS)) {
            for s in name
                .split(|c: char| c.is_xml_blank_char())
                .filter(|s| !s.is_empty())
            {
                if xml_get_id(ctxt.doc.unwrap(), s).is_none() {
                    let attr_name = attr.name().unwrap();
                    xml_err_valid_node(
                        Some(ctxt),
                        attr.parent.map(|p| p.into()),
                        XmlParserErrors::XmlDTDUnknownID,
                        format!(
                            "IDREFS attribute {} references an unknown ID \"{}\"\n",
                            attr_name, s
                        )
                        .as_str(),
                        Some(&attr_name),
                        Some(s),
                        None,
                    );
                    ctxt.valid = 0;
                }
            }
        }
    } else {
        for s in name
            .split(|c: char| c.is_xml_blank_char())
            .filter(|s| !s.is_empty())
        {
            if xml_get_id(ctxt.doc.unwrap(), s).is_none() {
                xml_err_valid_node_nr!(
                    Some(&mut *ctxt),
                    None::<XmlGenericNodePtr>,
                    XmlParserErrors::XmlDTDUnknownID,
                    "attribute {} line {} references an unknown ID \"{}\"\n",
                    refe.name.as_deref().unwrap(),
                    refe.lineno,
                    s
                );
                ctxt.valid = 0;
            }
        }
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
pub fn xml_validate_document_final(ctxt: &mut XmlValidCtxt, doc: XmlDocPtr) -> i32 {
    // trick to get correct line id report
    let save: u32 = ctxt.flags;
    ctxt.flags &= !XML_VCTXT_USE_PCTXT as u32;

    // Check all the NOTATION/NOTATIONS attributes
    // Check all the ENTITY/ENTITIES attributes definition for validity
    // Check all the IDREF/IDREFS attributes definition for validity
    ctxt.doc = Some(doc);
    ctxt.valid = 1;
    for (name, ref_list) in doc.refs.iter() {
        ref_list.walk(|data| {
            xml_validate_ref(data.as_ref(), ctxt, name.as_str());
            true
        });
    }

    ctxt.flags = save;
    ctxt.valid
}

/// Validate that the given name match a notation declaration.
/// - [ VC: Notation Declared ]
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateNotationUse")]
#[cfg(any(feature = "libxml_valid", feature = "schema"))]
pub fn xml_validate_notation_use(
    ctxt: Option<&mut XmlValidCtxt>,
    doc: XmlDocPtr,
    notation_name: &str,
) -> i32 {
    if doc.int_subset.is_none() {
        return -1;
    }

    let nota_decl = xml_get_dtd_notation_desc(doc.int_subset.as_deref(), notation_name)
        .or_else(|| xml_get_dtd_notation_desc(doc.ext_subset.as_deref(), notation_name));

    if nota_decl.is_none() && ctxt.is_some() {
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
pub fn xml_is_mixed_element(doc: XmlDocPtr, name: &str) -> i32 {
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
        XmlElementTypeVal::XmlElementTypeUndefined => -1,
        XmlElementTypeVal::XmlElementTypeElement => 0,
        XmlElementTypeVal::XmlElementTypeEmpty
        | XmlElementTypeVal::XmlElementTypeAny
        | XmlElementTypeVal::XmlElementTypeMixed => {
            // return 1 for EMPTY since we want VC error to pop up
            // on <empty>     </empty> for example
            1
        }
    }
}

/// Search the DTD for the description of this notation
///
/// returns the xmlNotationPtr if found or null_mut()
#[doc(alias = "xmlGetDtdNotationDesc")]
pub fn xml_get_dtd_notation_desc<'a>(
    dtd: Option<&'a XmlDtd>,
    name: &str,
) -> Option<&'a XmlNotation> {
    dtd.and_then(|dtd| dtd.notations.get(name))
}

/// Search the DTD for the description of this element
///
/// returns the xmlElementPtr if found or null_mut()
#[doc(alias = "xmlGetDtdQElementDesc")]
pub fn xml_get_dtd_qelement_desc(
    dtd: Option<XmlDtdPtr>,
    name: &str,
    prefix: Option<&str>,
) -> Option<XmlElementPtr> {
    dtd?.elements
        .get(&(Cow::Borrowed(name), prefix.map(Cow::Borrowed)))
        .cloned()
}

/// Search the DTD for the description of this element
///
/// returns the xmlElementPtr if found or null_mut()
#[doc(alias = "xmlGetDtdElementDesc")]
pub fn xml_get_dtd_element_desc(dtd: Option<XmlDtdPtr>, name: &str) -> Option<XmlElementPtr> {
    let dtd = dtd?;

    let (prefix, name) = split_qname2(name)
        .map(|(pre, loc)| (Some(pre), loc))
        .unwrap_or((None, name));
    let cur = dtd
        .elements
        .get(&(Cow::Borrowed(name), prefix.map(Cow::Borrowed)))
        .cloned();
    cur
}

/// Build/extend a list of  potential children allowed by the content tree
///
/// Returns the number of element in the list, or -1 in case of error.
#[doc(alias = "xmlValidGetPotentialChildren")]
#[cfg(feature = "libxml_valid")]
pub fn xml_valid_get_potential_children(
    ctree: Option<Rc<ElementContent>>,
    names: &mut [Cow<'static, str>],
    len: &mut usize,
) -> usize {
    let Some(ctree) = ctree else {
        return usize::MAX;
    };

    if *len >= names.len() {
        return *len;
    }

    match ctree.r#type() {
        ElementContentType::PCDATA => {
            if names.iter().take(*len).any(|name| name == "#PCDATA") {
                return *len;
            }
            names[*len] = "#PCDATA".into();
            *len += 1;
        }
        ElementContentType::Element => {
            if names
                .iter()
                .take(*len)
                .any(|name| Some(name.as_ref()) == ctree.name().as_deref())
            {
                return *len;
            }
            names[*len] = ctree.name().as_deref().unwrap().to_owned().into();
            *len += 1;
        }
        ElementContentType::Seq => {
            xml_valid_get_potential_children(ctree.first_child(), names, len);
            xml_valid_get_potential_children(ctree.second_child(), names, len);
        }
        ElementContentType::Or => {
            xml_valid_get_potential_children(ctree.first_child(), names, len);
            xml_valid_get_potential_children(ctree.second_child(), names, len);
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
pub fn xml_valid_get_valid_elements(
    prev: Option<XmlGenericNodePtr>,
    next: Option<XmlGenericNodePtr>,
    names: &mut [Cow<'static, str>],
) -> i32 {
    if prev.is_none() && next.is_none() {
        return -1;
    }

    if names.is_empty() {
        return -1;
    }

    let mut vctxt = XmlValidCtxt {
        error: Some(xml_no_validity_err), // this suppresses err/warn output
        ..Default::default()
    };

    let ref_node = prev.or(next).unwrap();
    // Why can I do `unwrap` for parent without checking ????????
    let mut parent = ref_node.parent().unwrap();
    let parname = parent.name().unwrap();

    // Retrieves the parent element declaration
    let mut element_desc =
        xml_get_dtd_element_desc(parent.document().unwrap().int_subset, &parname);
    if element_desc.is_none() && parent.document().unwrap().ext_subset.is_some() {
        element_desc = xml_get_dtd_element_desc(parent.document().unwrap().ext_subset, &parname);
    }
    let Some(element_desc) = element_desc else {
        return -1;
    };

    // Do a backup of the current tree structure
    let prev_next = prev.and_then(|prev| prev.next());
    let next_prev = next.and_then(|next| next.prev());

    let parent_childs = parent.children();
    let parent_last = parent.last();

    // Creates a dummy node and insert it into the tree
    let Some(mut test_node) = xml_new_doc_node(ref_node.document(), None, "<!dummy?>", None) else {
        return -1;
    };

    test_node.parent = Some(parent);
    test_node.prev = prev;
    test_node.next = next;
    let name = test_node.name.clone();

    if let Some(mut prev) = prev {
        prev.set_next(Some(test_node.into()));
    } else {
        parent.set_children(Some(test_node.into()));
    }

    if let Some(mut next) = next {
        next.set_prev(Some(test_node.into()));
    } else {
        parent.set_last(Some(test_node.into()));
    }

    // Insert each potential child node and check if the parent is still valid
    let mut elements = [const { Cow::Borrowed("") }; 256];
    let mut nb_elements = 0;
    let nb_elements = xml_valid_get_potential_children(
        element_desc.content.clone(),
        &mut elements[..],
        &mut nb_elements,
    );

    let mut nb_valid_elements = 0;
    for element in elements.into_iter().take(nb_elements) {
        test_node.name = element.clone();
        if xml_validate_one_element(&mut vctxt, parent.document().unwrap(), Some(parent)) != 0 {
            for j in 0..nb_valid_elements {
                if element == names[j] {
                    break;
                }
            }
            names[nb_elements] = element;
            nb_valid_elements += 1;
            if nb_valid_elements >= names.len() {
                break;
            }
        }
    }

    // Restore the tree structure
    if let Some(mut prev) = prev {
        prev.set_next(prev_next);
    }
    if let Some(mut next) = next {
        next.set_prev(next_prev);
    }
    parent.set_children(parent_childs);
    parent.set_last(parent_last);

    // Free up the dummy node
    test_node.name = name;
    unsafe {
        xml_free_node(test_node);
    }

    nb_valid_elements as i32
}

/// Validate that the given value match Name production
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateNameValue")]
#[cfg(feature = "libxml_valid")]
pub fn xml_validate_name_value(value: &str) -> i32 {
    xml_validate_name_value_internal(None, value)
}

/// Validate that the given value match Names production
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateNamesValue")]
#[cfg(feature = "libxml_valid")]
pub fn xml_validate_names_value(value: &str) -> i32 {
    xml_validate_names_value_internal(None, value)
}

/// Validate that the given value match Nmtoken production
///
/// `[ VC: Name Token ]`
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateNmtokenValue")]
#[cfg(feature = "libxml_valid")]
pub fn xml_validate_nmtoken_value(value: &str) -> i32 {
    xml_validate_nmtoken_value_internal(None, value)
}

/// Validate that the given value match Nmtokens production
///
/// `[ VC: Name Token ]`
///
/// returns 1 if valid or 0 otherwise
#[doc(alias = "xmlValidateNmtokensValue")]
#[cfg(feature = "libxml_valid")]
pub fn xml_validate_nmtokens_value(value: &str) -> i32 {
    xml_validate_nmtokens_value_internal(None, value)
}

/// Generate the automata sequence needed for that type
///
/// Returns 1 if successful or 0 in case of error.
#[doc(alias = "xmlValidBuildAContentModel")]
fn xml_valid_build_a_content_model(
    content: Option<Rc<ElementContent>>,
    ctxt: &mut XmlValidCtxt,
    name: &str,
) -> i32 {
    let Some(mut cont) = content else {
        xml_err_valid_node(
            Some(&mut *ctxt),
            None,
            XmlParserErrors::XmlErrInternalError,
            format!("Found NULL content in content model of {name}\n").as_str(),
            Some(name),
            None,
            None,
        );
        return 0;
    };
    match cont.r#type() {
        ElementContentType::PCDATA => {
            xml_err_valid_node(
                Some(&mut *ctxt),
                None,
                XmlParserErrors::XmlErrInternalError,
                format!("Found PCDATA in content model of {name}\n").as_str(),
                Some(name),
                None,
                None,
            );
            return 0;
        }
        ElementContentType::Element => {
            let oldstate = ctxt.state;
            let fullname = cont.name().unwrap();

            match cont.occur() {
                ElementContentOccur::Once => {
                    ctxt.state = ctxt.am.as_mut().unwrap().new_transition(
                        ctxt.state,
                        usize::MAX,
                        &fullname,
                        null_mut(),
                    );
                }
                ElementContentOccur::Opt => {
                    ctxt.state = ctxt.am.as_mut().unwrap().new_transition(
                        ctxt.state,
                        usize::MAX,
                        &fullname,
                        null_mut(),
                    );
                    ctxt.am.as_mut().unwrap().new_epsilon(oldstate, ctxt.state);
                }
                ElementContentOccur::Plus => {
                    ctxt.state = ctxt.am.as_mut().unwrap().new_transition(
                        ctxt.state,
                        usize::MAX,
                        &fullname,
                        null_mut(),
                    );
                    ctxt.am.as_mut().unwrap().new_transition(
                        ctxt.state,
                        ctxt.state,
                        &fullname,
                        null_mut(),
                    );
                }
                ElementContentOccur::Mult => {
                    ctxt.state = ctxt
                        .am
                        .as_mut()
                        .unwrap()
                        .new_epsilon(ctxt.state, usize::MAX);
                    ctxt.am.as_mut().unwrap().new_transition(
                        ctxt.state,
                        ctxt.state,
                        &fullname,
                        null_mut(),
                    );
                }
            }
        }
        ElementContentType::Seq => {
            // Simply iterate over the content
            let mut oldstate = ctxt.state;
            let ocur = cont.occur();
            if ocur != ElementContentOccur::Once {
                ctxt.state = ctxt.am.as_mut().unwrap().new_epsilon(oldstate, usize::MAX);
                oldstate = ctxt.state;
            }
            while {
                xml_valid_build_a_content_model(cont.first_child(), ctxt, name);
                cont = cont.second_child().unwrap();
                cont.r#type() == ElementContentType::Seq
                    && cont.occur() == ElementContentOccur::Once
            } {}
            xml_valid_build_a_content_model(Some(cont.clone()), ctxt, name);
            let oldend = ctxt.state;
            ctxt.state = ctxt.am.as_mut().unwrap().new_epsilon(oldend, usize::MAX);
            match ocur {
                ElementContentOccur::Once => {}
                ElementContentOccur::Opt => {
                    ctxt.am.as_mut().unwrap().new_epsilon(oldstate, ctxt.state);
                }
                ElementContentOccur::Mult => {
                    ctxt.am.as_mut().unwrap().new_epsilon(oldstate, ctxt.state);
                    ctxt.am.as_mut().unwrap().new_epsilon(oldend, oldstate);
                }
                ElementContentOccur::Plus => {
                    ctxt.am.as_mut().unwrap().new_epsilon(oldend, oldstate);
                }
            }
        }
        ElementContentType::Or => {
            let ocur = cont.occur();
            if matches!(ocur, ElementContentOccur::Plus | ElementContentOccur::Mult) {
                ctxt.state = ctxt
                    .am
                    .as_mut()
                    .unwrap()
                    .new_epsilon(ctxt.state, usize::MAX);
            }
            let oldstate = ctxt.state;
            let oldend = ctxt.am.as_mut().unwrap().new_state();

            // iterate over the subtypes and remerge the end with an
            // epsilon transition
            while {
                ctxt.state = oldstate;
                xml_valid_build_a_content_model(cont.first_child(), ctxt, name);
                ctxt.am.as_mut().unwrap().new_epsilon(ctxt.state, oldend);
                cont = cont.second_child().unwrap();
                cont.r#type() == ElementContentType::Or && cont.occur() == ElementContentOccur::Once
            } {}
            ctxt.state = oldstate;
            xml_valid_build_a_content_model(Some(cont.clone()), ctxt, name);
            ctxt.am.as_mut().unwrap().new_epsilon(ctxt.state, oldend);
            ctxt.state = ctxt.am.as_mut().unwrap().new_epsilon(oldend, usize::MAX);
            match ocur {
                ElementContentOccur::Once => {}
                ElementContentOccur::Opt => {
                    ctxt.am.as_mut().unwrap().new_epsilon(oldstate, ctxt.state);
                }
                ElementContentOccur::Mult => {
                    ctxt.am.as_mut().unwrap().new_epsilon(oldstate, ctxt.state);
                    ctxt.am.as_mut().unwrap().new_epsilon(oldend, oldstate);
                }
                ElementContentOccur::Plus => {
                    ctxt.am.as_mut().unwrap().new_epsilon(oldend, oldstate);
                }
            }
        }
    }
    1
}

/// (Re)Build the automata associated to the content model of this element
///
/// Returns 1 in case of success, 0 in case of error
#[doc(alias = "xmlValidBuildContentModel")]
#[cfg(all(feature = "libxml_valid", feature = "libxml_regexp"))]
pub fn xml_valid_build_content_model(ctxt: &mut XmlValidCtxt, mut elem: XmlElementPtr) -> i32 {
    if !matches!(elem.element_type(), XmlElementType::XmlElementDecl) {
        return 0;
    }
    if !matches!(elem.etype, XmlElementTypeVal::XmlElementTypeElement) {
        return 1;
    }
    // TODO: should we rebuild in this case ?
    if let Some(cont_model) = elem.cont_model.as_deref() {
        if cont_model.is_determinist() == 0 {
            ctxt.valid = 0;
            return 0;
        }
        return 1;
    }

    ctxt.am = XmlAutomata::new();

    if ctxt.am.is_none() {
        let name = elem.name.as_deref().unwrap();
        xml_err_valid_node(
            Some(&mut *ctxt),
            Some(elem.into()),
            XmlParserErrors::XmlErrInternalError,
            format!("Cannot create automata for element {name}\n").as_str(),
            Some(name),
            None,
            None,
        );
        return 0;
    }
    ctxt.state = ctxt.am.as_mut().unwrap().get_init_state();
    xml_valid_build_a_content_model(elem.content.clone(), ctxt, elem.name.as_deref().unwrap());
    ctxt.am
        .as_mut()
        .unwrap()
        .get_state_mut(ctxt.state)
        .unwrap()
        .set_final_state();
    elem.cont_model = ctxt.am.as_mut().unwrap().compile().map(Rc::new);
    if elem
        .cont_model
        .as_deref()
        .is_none_or(|cont_model| cont_model.is_determinist() != 1)
    {
        let expr = format!("{}", elem.content.as_ref().unwrap());
        let name = elem.name.as_deref().unwrap();
        xml_err_valid_node(
            Some(&mut *ctxt),
            Some(elem.into()),
            XmlParserErrors::XmlDTDContentNotDeterminist,
            format!("Content model of {} is not deterministic: {}\n", name, expr).as_str(),
            Some(name),
            Some(&expr),
            None,
        );
        ctxt.valid = 0;
        ctxt.state = usize::MAX;
        ctxt.am.take();
        return 0;
    }
    ctxt.state = usize::MAX;
    ctxt.am.take();
    1
}

/// Check if the given node is part of the content model.
///
/// Returns 1 if yes, 0 if no, -1 in case of error
#[doc(alias = "xmlValidateCheckMixed")]
#[cfg(feature = "libxml_regexp")]
pub(crate) fn xml_validate_check_mixed(mut cont: Option<Rc<ElementContent>>, qname: &str) -> i32 {
    while let Some(now) = cont {
        if now.r#type() == ElementContentType::Element {
            if now.name().as_deref() == Some(qname) {
                return 1;
            }
        } else if let Some(c1) = now.first_child().filter(|c1| {
            c1.r#type() == ElementContentType::Element && now.r#type() == ElementContentType::Or
        }) {
            if c1.name().as_deref() == Some(qname) {
                return 1;
            }
        } else if now.r#type() != ElementContentType::Or
            || now
                .first_child()
                .is_none_or(|c1| c1.r#type() != ElementContentType::PCDATA)
        {
            xml_err_valid!(
                None::<&mut XmlValidCtxt>,
                XmlParserErrors::XmlDTDMixedCorrupt,
                "Internal: MIXED struct corrupted\n"
            );
            break;
        }
        cont = now.second_child();
    }
    0
}
