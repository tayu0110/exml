//! Provides methods for `XmlValidCtxt` owned by `XmlParserCtxt`.
//!
//! In the original libxml2, `xmlValidCtxt` owns the parent `xmlParserCtxtPtr` as `userData`,
//! but I have no good idea to implement this safely in Rust.  
//! As a workaround, I decided to implement it as a method of `XmlParserCtxt`
//! so that `XmlValidCtxt` does not own a pointer to the parent.

use std::{cell::RefCell, collections::HashMap, ptr::null_mut, rc::Rc};

use crate::{
    error::{__xml_raise_error, XmlErrorDomain, XmlErrorLevel, XmlParserErrors},
    hash::{XmlHashTable, XmlHashTableRef},
    libxml::{
        chvalid::xml_is_blank_char,
        valid::{
            xml_get_dtd_element_desc, xml_get_dtd_notation_desc, xml_get_dtd_qelement_desc,
            xml_snprintf_element_content, xml_snprintf_elements,
            xml_validate_attribute_value_internal,
        },
        xmlregexp::XmlRegExecCtxt,
    },
    list::XmlList,
    parser::build_qname,
    tree::{
        NodeCommon, XmlAttrPtr, XmlAttribute, XmlAttributeDefault, XmlAttributePtr,
        XmlAttributeType, XmlDocPtr, XmlDtdPtr, XmlElement, XmlElementContent,
        XmlElementContentOccur, XmlElementContentType, XmlElementPtr, XmlElementType,
        XmlElementTypeVal, XmlEntityPtr, XmlEntityType, XmlEnumeration, XmlGenericNodePtr, XmlID,
        XmlNodePtr, XmlNsPtr, XmlRef, xml_free_attribute, xml_free_element, xml_get_doc_entity,
    },
};

use super::{XmlParserCtxt, XmlParserCtxtPtr, XmlParserMode, split_qname2};

impl XmlParserCtxt {
    #[doc(alias = "xmlIsStreaming")]
    fn is_streaming(&self) -> bool {
        matches!(self.parse_mode, XmlParserMode::XmlParseReader)
    }

    /// Register a new id declaration
    ///
    /// Returns null_mut() if not, otherwise the new xmlIDPtr
    #[doc(alias = "xmlAddID")]
    pub fn add_id(&mut self, mut doc: XmlDocPtr, value: &str, mut attr: XmlAttrPtr) -> Option<()> {
        if value.is_empty() {
            return None;
        }

        let mut ret = XmlID {
            value: value.to_owned(),
            doc: Some(doc),
            ..Default::default()
        };
        if self.is_streaming() {
            // Operating in streaming mode, attr is gonna disappear
            ret.name = Some(attr.name.as_ref().to_owned());
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
            {
                xml_err_valid_node(
                    Some(self),
                    attr.parent.map(|par| par.into()),
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

    /// Register a new ref declaration
    ///
    /// Returns `None` if not, otherwise `Some(())`
    ///
    /// # Note
    /// This function in original libxml2 returns new `xmlRefPtr`.  
    /// However, this function cannot returns `Option<&XmlRef>`.
    #[doc(alias = "xmlAddRef")]
    pub(crate) fn add_ref(
        &mut self,
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
        if self.is_streaming() {
            // Operating in streaming mode, attr is gonna disappear
            ret.name = Some(attr.name.as_ref().to_owned());
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

    /// Register a new element declaration
    ///
    /// Returns null_mut() if not, otherwise the entity
    #[doc(alias = "xmlAddElementDecl")]
    pub fn add_element_decl(
        &mut self,
        dtd: Option<XmlDtdPtr>,
        mut name: &str,
        typ: Option<XmlElementTypeVal>,
        content: Option<Rc<RefCell<XmlElementContent>>>,
    ) -> Option<XmlElementPtr> {
        let mut dtd = dtd?;
        match typ {
            Some(XmlElementTypeVal::XmlElementTypeEmpty) => {
                if content.is_some() {
                    xml_err_valid(
                        Some(self),
                        XmlParserErrors::XmlErrInternalError,
                        "xmlAddElementDecl: content != NULL for EMPTY\n",
                        None,
                    );
                    return None;
                }
            }
            Some(XmlElementTypeVal::XmlElementTypeAny) => {
                if content.is_some() {
                    xml_err_valid(
                        Some(self),
                        XmlParserErrors::XmlErrInternalError,
                        "xmlAddElementDecl: content != NULL for ANY\n",
                        None,
                    );
                    return None;
                }
            }
            Some(XmlElementTypeVal::XmlElementTypeMixed) => {
                if content.is_none() {
                    xml_err_valid(
                        Some(self),
                        XmlParserErrors::XmlErrInternalError,
                        "xmlAddElementDecl: content == NULL for MIXED\n",
                        None,
                    );
                    return None;
                }
            }
            Some(XmlElementTypeVal::XmlElementTypeElement) => {
                if content.is_none() {
                    xml_err_valid(
                        Some(self),
                        XmlParserErrors::XmlErrInternalError,
                        "xmlAddElementDecl: content == NULL for ELEMENT\n",
                        None,
                    );
                    return None;
                }
            }
            _ => {
                xml_err_valid(
                    Some(self),
                    XmlParserErrors::XmlErrInternalError,
                    "Internal: ELEMENT decl corrupted invalid type\n",
                    None,
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
                    .remove_entry2(name, ns, |_, _| {})
                    .ok();
                unsafe {
                    xml_free_element(Some(ret));
                }
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
                        Some(self),
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
                xml_verr_memory(Some(self), Some("malloc failed"));
                return None;
            };

            // Validity Check:
            // Insertion must not fail
            if table.add_entry2(name, ns, ret).is_err() {
                #[cfg(feature = "libxml_valid")]
                {
                    // The element is already defined in this DTD.
                    xml_err_valid_node(
                        Some(self),
                        Some(dtd.into()),
                        XmlParserErrors::XmlDTDElemRedefined,
                        format!("Redefinition of element {name}\n").as_str(),
                        Some(name),
                        None,
                        None,
                    );
                }
                unsafe {
                    ret.free();
                }
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
        ret.content = content;

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

    /// Register a new attribute declaration
    /// Note that @tree becomes the ownership of the DTD
    ///
    /// Returns null_mut() if not new, otherwise the attribute decl
    #[allow(clippy::too_many_arguments)]
    #[doc(alias = "xmlAddAttributeDecl")]
    pub fn add_attribute_decl(
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
        let mut dtd = dtd?;

        #[cfg(feature = "libxml_valid")]
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
                xml_verr_memory(
                    Some(self),
                    Some("xmlAddAttributeDecl: Table creation failed!\n"),
                );
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
        if table
            .add_entry3(
                ret.name.as_deref().unwrap(),
                ret.prefix.as_deref(),
                ret.elem.as_deref(),
                ret as _,
            )
            .is_err()
        {
            #[cfg(feature = "libxml_valid")]
            {
                // The attribute is already defined in this DTD.
                xml_err_valid_warning(
                    Some(self),
                    Some(dtd.into()),
                    XmlParserErrors::XmlDTDAttributeRedefined,
                    format!("Attribute {} of element {}: already defined\n", name, elem).as_str(),
                    Some(name),
                    Some(elem),
                    None,
                );
            }
            unsafe {
                xml_free_attribute(ret);
            }
            return None;
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
                        format!(
                            "Element {} has too may ID attributes defined : {}\n",
                            elem, name
                        )
                        .as_str(),
                        Some(elem),
                        Some(name),
                        None,
                    );
                    self.valid = 0;
                }
            }

            // Insert namespace default def first they need to be processed first.
            if ret.name.as_deref() == Some("xmlns") || ret.prefix.as_deref() == Some("xmlns") {
                ret.nexth = elem_def.attributes;
                elem_def.attributes = Some(ret);
            } else {
                let mut tmp = elem_def.attributes;

                while let Some(now) = tmp.filter(|tmp| {
                    tmp.name.as_deref() == Some("xmlns") || ret.prefix.as_deref() == Some("xmlns")
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

    /// Finds a declaration associated to an element in the document.
    ///
    /// returns the pointer to the declaration or null_mut() if not found.
    #[doc(alias = "xmlValidGetElemDecl")]
    fn get_elem_decl(
        &mut self,
        doc: XmlDocPtr,
        elem: XmlNodePtr,
        extsubset: &mut i32,
    ) -> Option<XmlElementPtr> {
        *extsubset = 0;

        // Fetch the declaration for the qualified name
        let prefix = elem.ns.as_deref().and_then(|ns| ns.prefix());
        let mut elem_decl = None;
        if let Some(prefix) = prefix {
            elem_decl =
                xml_get_dtd_qelement_desc(doc.int_subset, &elem.name().unwrap(), Some(&prefix));
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
                Some(self),
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

    /// Search the DTD for the description of this element
    ///
    /// returns the xmlElementPtr if found or null_mut()
    #[doc(alias = "xmlGetDtdElementDesc2")]
    fn get_dtd_element_desc2(
        &mut self,
        mut dtd: XmlDtdPtr,
        mut name: &str,
        create: i32,
    ) -> Option<XmlElementPtr> {
        if dtd.elements.is_none() && create == 0 {
            return None;
        }
        let table = dtd
            .elements
            .get_or_insert_with(|| XmlHashTable::with_capacity(0));
        let mut prefix = None;
        if let Some((pref, local)) = split_qname2(name) {
            name = local;
            prefix = Some(pref);
        }
        let mut cur = table.lookup2(name, prefix).cloned();
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
            if table.add_entry2(name, prefix, res).is_err() {
                xml_verr_memory(Some(self), Some("adding entry failed"));
                unsafe {
                    xml_free_element(cur);
                }
                cur = None;
            }
        }
        cur
    }

    #[doc(alias = "nodeVPush")]
    fn node_vpush(&mut self, value: XmlNodePtr) -> i32 {
        self.vctxt.node_tab.push(value);
        self.vctxt.node = Some(value);
        self.vctxt.node_tab.len() as i32 - 1
    }

    #[doc(alias = "nodeVPop")]
    fn node_vpop(&mut self) -> Option<XmlNodePtr> {
        let res = self.vctxt.node_tab.pop()?;
        self.vctxt.node = self.vctxt.node_tab.last().cloned();
        Some(res)
    }

    #[cfg(feature = "libxml_regexp")]
    fn vstate_vpush(&mut self, elem_decl: Option<XmlElementPtr>, node: XmlNodePtr) -> usize {
        use crate::libxml::valid::XmlValidState;

        // self.vctxt.vstate = self.vctxt.vstate_tab.add(self.vctxt.vstate_nr as usize);
        self.vctxt.vstate_tab.push(XmlValidState {
            elem_decl,
            node,
            exec: None,
        });
        if let Some(elem_decl) =
            elem_decl.filter(|decl| matches!(decl.etype, XmlElementTypeVal::XmlElementTypeElement))
        {
            if elem_decl.cont_model.is_none() {
                self.build_content_model(elem_decl);
            }
            if let Some(cont_model) = elem_decl.cont_model.clone() {
                self.vctxt.vstate_tab.last_mut().unwrap().exec =
                    Some(Box::new(XmlRegExecCtxt::new(cont_model, None, null_mut())));
            } else {
                self.vctxt.vstate_tab.last_mut().unwrap().exec = None;
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
        self.vctxt.vstate_tab.len() - 1
    }

    #[cfg(feature = "libxml_regexp")]
    fn vstate_vpop(&mut self) -> i32 {
        if self.vctxt.vstate_tab.is_empty() {
            return -1;
        }
        let mut state = self.vctxt.vstate_tab.pop().unwrap();
        let elem_decl = state.elem_decl;
        if elem_decl.is_some_and(|elem_decl| {
            matches!(elem_decl.etype, XmlElementTypeVal::XmlElementTypeElement)
        }) {
            state.exec.take();
        }
        self.vctxt.vstate_tab.len() as i32
    }

    /// Push a new element start on the validation stack.
    ///
    /// returns 1 if no validation problem was found or 0 otherwise
    #[doc(alias = "xmlValidatePushElement")]
    #[cfg(all(feature = "libxml_valid", feature = "libxml_regexp"))]
    pub fn vpush_element(&mut self, doc: XmlDocPtr, elem: XmlNodePtr, qname: &str) -> i32 {
        let mut ret: i32 = 1;
        let mut extsubset: i32 = 0;

        if let Some(state) = self.vctxt.vstate_tab.last_mut() {
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
                        if elem_decl.content.as_deref().is_some_and(|content| {
                            content.borrow().typ == XmlElementContentType::XmlElementContentPCDATA
                        }) {
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
                            ret = self.validate_check_mixed(elem_decl.content.clone(), qname);
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
                                    format!("Element {} content does not follow the DTD, Misplaced {}\n", name, qname).as_str(),
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
        let e_decl = self.get_elem_decl(doc, elem, &mut extsubset);
        self.vstate_vpush(e_decl, elem);
        ret
    }

    /// Pop the element end from the validation stack.
    ///
    /// Returns 1 if no validation problem was found or 0 otherwise
    #[doc(alias = "xmlValidatePopElement")]
    #[cfg(all(feature = "libxml_valid", feature = "libxml_regexp"))]
    pub fn vpop_element(
        &mut self,
        _doc: Option<XmlDocPtr>,
        _elem: Option<XmlNodePtr>,
        _qname: &str,
    ) -> i32 {
        let mut ret: i32 = 1;

        // printf("PopElem %s\n", qname);
        if let Some(state) = self.vctxt.vstate_tab.last_mut() {
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
    pub fn vpush_cdata(&mut self, data: &str) -> i32 {
        let mut ret: i32 = 1;

        if data.is_empty() {
            return ret;
        }

        if let Some(state) = self.vctxt.vstate_tab.last() {
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
                        if data.contains(|c: char| !xml_is_blank_char(c as u32)) {
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

    /// Generate the automata sequence needed for that type
    ///
    /// Returns 1 if successful or 0 in case of error.
    #[doc(alias = "xmlValidBuildAContentModel")]
    fn build_a_content_model(
        &mut self,
        content: Option<Rc<RefCell<XmlElementContent>>>,
        name: &str,
    ) -> i32 {
        let Some(mut content) = content else {
            xml_err_valid_node(
                Some(self),
                None,
                XmlParserErrors::XmlErrInternalError,
                format!("Found NULL content in content model of {name}\n").as_str(),
                Some(name),
                None,
                None,
            );
            return 0;
        };
        let mut cont = content.borrow();
        match cont.typ {
            XmlElementContentType::XmlElementContentPCDATA => {
                xml_err_valid_node(
                    Some(self),
                    None,
                    XmlParserErrors::XmlErrInternalError,
                    format!("Found PCDATA in content model of {name}\n").as_str(),
                    Some(name),
                    None,
                    None,
                );
                return 0;
            }
            XmlElementContentType::XmlElementContentElement => {
                let oldstate = self.vctxt.state;
                let fullname = build_qname(cont.name.as_deref().unwrap(), cont.prefix.as_deref());

                match cont.ocur {
                    XmlElementContentOccur::XmlElementContentOnce => {
                        self.vctxt.state = self.vctxt.am.as_mut().unwrap().new_transition(
                            self.vctxt.state,
                            usize::MAX,
                            &fullname,
                            null_mut(),
                        );
                    }
                    XmlElementContentOccur::XmlElementContentOpt => {
                        self.vctxt.state = self.vctxt.am.as_mut().unwrap().new_transition(
                            self.vctxt.state,
                            usize::MAX,
                            &fullname,
                            null_mut(),
                        );
                        self.vctxt
                            .am
                            .as_mut()
                            .unwrap()
                            .new_epsilon(oldstate, self.vctxt.state);
                    }
                    XmlElementContentOccur::XmlElementContentPlus => {
                        self.vctxt.state = self.vctxt.am.as_mut().unwrap().new_transition(
                            self.vctxt.state,
                            usize::MAX,
                            &fullname,
                            null_mut(),
                        );
                        self.vctxt.am.as_mut().unwrap().new_transition(
                            self.vctxt.state,
                            self.vctxt.state,
                            &fullname,
                            null_mut(),
                        );
                    }
                    XmlElementContentOccur::XmlElementContentMult => {
                        self.vctxt.state = self
                            .vctxt
                            .am
                            .as_mut()
                            .unwrap()
                            .new_epsilon(self.vctxt.state, usize::MAX);
                        self.vctxt.am.as_mut().unwrap().new_transition(
                            self.vctxt.state,
                            self.vctxt.state,
                            &fullname,
                            null_mut(),
                        );
                    }
                }
            }
            XmlElementContentType::XmlElementContentSeq => {
                // Simply iterate over the content
                let mut oldstate = self.vctxt.state;
                let ocur: XmlElementContentOccur = cont.ocur;
                if !matches!(ocur, XmlElementContentOccur::XmlElementContentOnce) {
                    self.vctxt.state = self
                        .vctxt
                        .am
                        .as_mut()
                        .unwrap()
                        .new_epsilon(oldstate, usize::MAX);
                    oldstate = self.vctxt.state;
                }
                while {
                    self.build_a_content_model(cont.c1.clone(), name);
                    let next = cont.c2.clone().unwrap();
                    drop(cont);
                    content = next;
                    cont = content.borrow();
                    matches!(cont.typ, XmlElementContentType::XmlElementContentSeq)
                        && matches!(cont.ocur, XmlElementContentOccur::XmlElementContentOnce)
                } {}
                self.build_a_content_model(Some(content.clone()), name);
                let oldend = self.vctxt.state;
                self.vctxt.state = self
                    .vctxt
                    .am
                    .as_mut()
                    .unwrap()
                    .new_epsilon(oldend, usize::MAX);
                match ocur {
                    XmlElementContentOccur::XmlElementContentOnce => {}
                    XmlElementContentOccur::XmlElementContentOpt => {
                        self.vctxt
                            .am
                            .as_mut()
                            .unwrap()
                            .new_epsilon(oldstate, self.vctxt.state);
                    }
                    XmlElementContentOccur::XmlElementContentMult => {
                        self.vctxt
                            .am
                            .as_mut()
                            .unwrap()
                            .new_epsilon(oldstate, self.vctxt.state);
                        self.vctxt
                            .am
                            .as_mut()
                            .unwrap()
                            .new_epsilon(oldend, oldstate);
                    }
                    XmlElementContentOccur::XmlElementContentPlus => {
                        self.vctxt
                            .am
                            .as_mut()
                            .unwrap()
                            .new_epsilon(oldend, oldstate);
                    }
                }
            }
            XmlElementContentType::XmlElementContentOr => {
                let ocur: XmlElementContentOccur = cont.ocur;
                if matches!(
                    ocur,
                    XmlElementContentOccur::XmlElementContentPlus
                        | XmlElementContentOccur::XmlElementContentMult
                ) {
                    self.vctxt.state = self
                        .vctxt
                        .am
                        .as_mut()
                        .unwrap()
                        .new_epsilon(self.vctxt.state, usize::MAX);
                }
                let oldstate = self.vctxt.state;
                let oldend = self.vctxt.am.as_mut().unwrap().new_state();

                // iterate over the subtypes and remerge the end with an
                // epsilon transition
                while {
                    self.vctxt.state = oldstate;
                    self.build_a_content_model(cont.c1.clone(), name);
                    self.vctxt
                        .am
                        .as_mut()
                        .unwrap()
                        .new_epsilon(self.vctxt.state, oldend);
                    let next = cont.c2.clone().unwrap();
                    drop(cont);
                    content = next;
                    cont = content.borrow();
                    cont.typ == XmlElementContentType::XmlElementContentOr
                        && matches!(cont.ocur, XmlElementContentOccur::XmlElementContentOnce)
                } {}
                self.vctxt.state = oldstate;
                self.build_a_content_model(Some(content.clone()), name);
                self.vctxt
                    .am
                    .as_mut()
                    .unwrap()
                    .new_epsilon(self.vctxt.state, oldend);
                self.vctxt.state = self
                    .vctxt
                    .am
                    .as_mut()
                    .unwrap()
                    .new_epsilon(oldend, usize::MAX);
                match ocur {
                    XmlElementContentOccur::XmlElementContentOnce => {}
                    XmlElementContentOccur::XmlElementContentOpt => {
                        self.vctxt
                            .am
                            .as_mut()
                            .unwrap()
                            .new_epsilon(oldstate, self.vctxt.state);
                    }
                    XmlElementContentOccur::XmlElementContentMult => {
                        self.vctxt
                            .am
                            .as_mut()
                            .unwrap()
                            .new_epsilon(oldstate, self.vctxt.state);
                        self.vctxt
                            .am
                            .as_mut()
                            .unwrap()
                            .new_epsilon(oldend, oldstate);
                    }
                    XmlElementContentOccur::XmlElementContentPlus => {
                        self.vctxt
                            .am
                            .as_mut()
                            .unwrap()
                            .new_epsilon(oldend, oldstate);
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
    pub fn build_content_model(&mut self, mut elem: XmlElementPtr) -> i32 {
        use crate::libxml::{valid::xml_snprintf_element_content, xmlautomata::XmlAutomata};

        if !matches!(elem.element_type(), XmlElementType::XmlElementDecl) {
            return 0;
        }
        if !matches!(elem.etype, XmlElementTypeVal::XmlElementTypeElement) {
            return 1;
        }
        // TODO: should we rebuild in this case ?
        if let Some(cont_model) = elem.cont_model.as_deref() {
            if cont_model.is_determinist() == 0 {
                self.vctxt.valid = 0;
                return 0;
            }
            return 1;
        }

        self.vctxt.am = XmlAutomata::new();

        if self.vctxt.am.is_none() {
            let name = elem.name.as_deref().unwrap();
            xml_err_valid_node(
                Some(self),
                Some(elem.into()),
                XmlParserErrors::XmlErrInternalError,
                format!("Cannot create automata for element {name}\n").as_str(),
                Some(name),
                None,
                None,
            );
            return 0;
        }
        self.vctxt.state = self.vctxt.am.as_mut().unwrap().get_init_state();
        self.build_a_content_model(elem.content.clone(), elem.name.as_deref().unwrap());
        self.vctxt
            .am
            .as_mut()
            .unwrap()
            .get_state_mut(self.vctxt.state)
            .unwrap()
            .set_final_state();
        elem.cont_model = self.vctxt.am.as_mut().unwrap().compile().map(Rc::new);
        if elem
            .cont_model
            .as_deref()
            .is_none_or(|cont_model| cont_model.is_determinist() != 1)
        {
            let mut expr = String::with_capacity(5000);
            xml_snprintf_element_content(&mut expr, 5000, elem.content.clone().unwrap(), 1);
            let name = elem.name.as_deref().unwrap();
            xml_err_valid_node(
                Some(self),
                Some(elem.into()),
                XmlParserErrors::XmlDTDContentNotDeterminist,
                format!("Content model of {} is not deterministic: {}\n", name, expr).as_str(),
                Some(name),
                Some(&expr),
                None,
            );
            self.vctxt.valid = 0;
            self.vctxt.state = usize::MAX;
            self.vctxt.am.take();
            return 0;
        }
        self.vctxt.state = usize::MAX;
        self.vctxt.am.take();
        1
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
    pub fn validate_root(&mut self, doc: XmlDocPtr) -> i32 {
        use crate::parser::build_qname;

        let Some(root) = doc.get_root_element() else {
            xml_err_valid(
                Some(self),
                XmlParserErrors::XmlDTDNoRoot,
                "no root element\n",
                None,
            );
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
                    Some(self),
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

    /// Try to validate the content model of an element
    ///
    /// returns 1 if valid or 0 if not and -1 in case of error
    #[doc(alias = "xmlValidateElementContent")]
    fn validate_element_content(
        &mut self,
        child: Option<XmlGenericNodePtr>,
        elem_decl: XmlElementPtr,
        warn: i32,
        parent: XmlNodePtr,
    ) -> i32 {
        let mut ret: i32;

        // let cont: XmlElementContentPtr = elem_decl.content;
        let name = elem_decl.name.as_deref();

        #[cfg(feature = "libxml_regexp")]
        {
            // Build the regexp associated to the content model
            if elem_decl.cont_model.is_none() {
                self.build_content_model(elem_decl);
            }
            let Some(cont_model) = elem_decl.cont_model.clone() else {
                return -1;
            };
            if cont_model.is_determinist() == 0 {
                return -1;
            }
            self.vctxt.node_tab.clear();
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
                                self.node_vpush(cur_node);
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
                            if let Some(prefix) = cur_node.ns.as_deref().and_then(|ns| ns.prefix())
                            {
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
                        cur = self.node_vpop().map(|node| node.into());
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
            let mut expr = String::with_capacity(5000);
            let mut list = String::with_capacity(5000);

            xml_snprintf_element_content(&mut expr, 5000, elem_decl.content.clone().unwrap(), 1);
            #[cfg(feature = "libxml_regexp")]
            {
                xml_snprintf_elements(&mut list, 5000, child, 1);
            }

            if let Some(name) = name {
                xml_err_valid_node(
                    Some(self),
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
                    Some(self),
                    Some(parent.into()),
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

        self.vctxt.node_tab.clear();
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
    pub fn validate_one_element(&mut self, doc: XmlDocPtr, elem: Option<XmlGenericNodePtr>) -> i32 {
        use crate::libxml::chvalid::xml_is_blank_char;

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
                    Some(self),
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
                        Some(self),
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
                        Some(self),
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
                        Some(self),
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
                    Some(self),
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
                    Some(self),
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
                    Some(self),
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
                    Some(self),
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
                    Some(self),
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
        let Some(elem_decl) = self.get_elem_decl(doc, elem, &mut extsubset) else {
            return 0;
        };

        // If vstate_nr is not zero that means continuous validation is
        // activated, do not try to check the content model at that level.
        if self.vctxt.vstate_tab.is_empty() {
            // Check that the element content matches the definition
            match elem_decl.etype {
                XmlElementTypeVal::XmlElementTypeUndefined => {
                    let name = elem.name().unwrap();
                    xml_err_valid_node(
                        Some(self),
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
                            Some(self),
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
                    if elem_decl.content.as_deref().is_some_and(|cont| {
                        cont.borrow().typ == XmlElementContentType::XmlElementContentPCDATA
                    }) {
                        ret = self.validate_one_cdata_element(doc, elem);
                        if ret == 0 {
                            let name = elem.name().unwrap();
                            xml_err_valid_node(
                        Some(self),
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
                        let mut child = elem.children;
                        // Hum, this start to get messy
                        while let Some(cur_node) = child {
                            if matches!(cur_node.element_type(), XmlElementType::XmlElementNode) {
                                let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                                let name = cur_node.name.clone();
                                if let Some(prefix) =
                                    cur_node.ns.as_deref().and_then(|ns| ns.prefix())
                                {
                                    let fullname = build_qname(&cur_node.name, Some(&prefix));
                                    let mut cont = elem_decl.content.clone();
                                    while let Some(now) = cont.clone() {
                                        let now = now.borrow();
                                        if matches!(
                                            now.typ,
                                            XmlElementContentType::XmlElementContentElement
                                        ) {
                                            if now.name.as_deref() == Some(&fullname) {
                                                break;
                                            }
                                        } else if matches!(
                                            now.typ,
                                            XmlElementContentType::XmlElementContentOr
                                        ) && now.c1.as_deref().is_some_and(|c1| {
                                            c1.borrow().typ
                                                == XmlElementContentType::XmlElementContentElement
                                        }) {
                                            if now.c1.as_deref().unwrap().borrow().name.as_deref()
                                                == Some(&fullname)
                                            {
                                                break;
                                            }
                                        } else if !matches!(
                                            now.typ,
                                            XmlElementContentType::XmlElementContentOr
                                        ) || now.c1.as_deref().is_none_or(|c1| {
                                            !matches!(
                                                c1.borrow().typ,
                                                XmlElementContentType::XmlElementContentPCDATA
                                            )
                                        }) {
                                            xml_err_valid(
                                                None,
                                                XmlParserErrors::XmlDTDMixedCorrupt,
                                                "Internal: MIXED struct corrupted\n",
                                                None,
                                            );
                                            break;
                                        }
                                        cont = now.c2.clone();
                                    }
                                    if cont.is_some() {
                                        child = cur_node.next();
                                        continue;
                                    }
                                }

                                let mut cont = elem_decl.content.clone();
                                while let Some(now) = cont.clone() {
                                    let now = now.borrow();
                                    if matches!(
                                        now.typ,
                                        XmlElementContentType::XmlElementContentElement
                                    ) {
                                        if now.name.as_deref() == Some(&name) {
                                            break;
                                        }
                                    } else if matches!(
                                        now.typ,
                                        XmlElementContentType::XmlElementContentOr
                                    ) && now.c1.as_deref().is_some_and(|c1| {
                                        matches!(
                                            c1.borrow().typ,
                                            XmlElementContentType::XmlElementContentElement
                                        )
                                    }) {
                                        if now.c1.as_deref().unwrap().borrow().name.as_deref()
                                            == Some(&name)
                                        {
                                            break;
                                        }
                                    } else if !matches!(
                                        now.typ,
                                        XmlElementContentType::XmlElementContentOr
                                    ) || now.c1.as_deref().is_none_or(|c1| {
                                        !matches!(
                                            c1.borrow().typ,
                                            XmlElementContentType::XmlElementContentPCDATA
                                        )
                                    }) {
                                        xml_err_valid(
                                            Some(self),
                                            XmlParserErrors::XmlDTDMixedCorrupt,
                                            "Internal: MIXED struct corrupted\n",
                                            None,
                                        );
                                        break;
                                    }
                                    cont = now.c2.clone();
                                }
                                if cont.is_none() {
                                    let elem_name = elem.name().unwrap();
                                    xml_err_valid_node(
                                    Some(self),
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
                                content = content
                                    .trim_start_matches(|c: char| xml_is_blank_char(c as u32));
                                if content.is_empty() {
                                    let name = elem.name().unwrap();
                                    xml_err_valid_node(
                                    Some(self),
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
                    tmp = self.validate_element_content(child, elem_decl, 1, elem);
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
                                Some(self),
                                Some(elem.into()),
                                XmlParserErrors::XmlDTDMissingAttribute,
                                format!(
                                    "Element {} does not carry attribute {}\n",
                                    elem_name, attr_name
                                )
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
                                Some(self),
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
                        xml_err_valid_warning(
                            Some(self),
                            Some(elem.into()),
                            XmlParserErrors::XmlDTDNoPrefix,
                            format!(
                                "Element {} required attribute {}:{} has no prefix\n",
                                elem.name,
                                cur_attr.prefix.as_deref().unwrap(),
                                cur_attr.name.as_deref().unwrap()
                            )
                            .as_str(),
                            Some(&elem.name),
                            cur_attr.prefix.as_deref(),
                            cur_attr.name.as_deref(),
                        );
                    } else if qualified == 1 {
                        xml_err_valid_warning(
                            Some(self),
                            Some(elem.into()),
                            XmlParserErrors::XmlDTDDifferentPrefix,
                            format!(
                                "Element {} required attribute {}:{} has different prefix\n",
                                elem.name,
                                cur_attr.prefix.as_deref().unwrap(),
                                cur_attr.name.as_deref().unwrap()
                            )
                            .as_str(),
                            Some(&elem.name),
                            cur_attr.prefix.as_deref(),
                            cur_attr.name.as_deref(),
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
                                Some(self),
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
                                if cur_attr.default_value.as_deref() != now.href.as_deref() {
                                    let elem_name = elem.name().unwrap();
                                    let prefix = now.prefix().unwrap();
                                    xml_err_valid_node(
                                        Some(self),
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

    /// Try to validate the subtree under an element
    ///
    /// returns 1 if valid or 0 otherwise
    #[doc(alias = "xmlValidateElement")]
    #[cfg(feature = "libxml_valid")]
    pub fn validate_element(&mut self, doc: XmlDocPtr, root: Option<XmlGenericNodePtr>) -> i32 {
        let mut ret: i32 = 1;

        let Some(root) = root else {
            return 0;
        };

        if doc.int_subset.is_none() && doc.ext_subset.is_none() {
            return 0;
        };

        let mut elem = root;
        loop {
            ret &= self.validate_one_element(doc, Some(elem));

            if let Some(node) = XmlNodePtr::try_from(elem)
                .ok()
                .filter(|elem| elem.element_type() == XmlElementType::XmlElementNode)
            {
                let mut attr = node.properties;
                while let Some(now) = attr {
                    let value = now.children().and_then(|c| c.get_string(Some(doc), 0));
                    ret &= self.validate_one_attribute(doc, node, Some(now), &value.unwrap());
                    attr = now.next;
                }

                let mut ns = node.ns_def;
                while let Some(now) = ns {
                    if let Some(elem_ns) = node.ns {
                        ret &= self.validate_one_namespace(
                            doc,
                            node,
                            elem_ns.prefix().as_deref(),
                            now,
                            &now.href().unwrap(),
                        );
                    } else {
                        ret &=
                            self.validate_one_namespace(doc, node, None, now, &now.href().unwrap());
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
    pub fn validate_one_attribute(
        &mut self,
        doc: XmlDocPtr,
        elem: XmlNodePtr,
        attr: Option<XmlAttrPtr>,
        value: &str,
    ) -> i32 {
        use crate::libxml::valid::xml_get_dtd_notation_desc;

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
                    attr_decl = doc.ext_subset.and_then(|dtd| {
                        dtd.get_attr_desc(&fullname, attr.name().as_deref().unwrap())
                    });
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
                Some(self),
                Some(elem.into()),
                XmlParserErrors::XmlDTDUnknownAttribute,
                format!(
                    "No declaration for attribute {} of element {}\n",
                    attr_name, elem_name
                )
                .as_str(),
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
                Some(self),
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
                Some(self),
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
            && self.add_id(doc, value, attr).is_none()
        {
            ret = 0;
        }

        if matches!(
            attr_decl.atype,
            XmlAttributeType::XmlAttributeIDREF | XmlAttributeType::XmlAttributeIDREFS
        ) && self.add_ref(doc, value, attr).is_none()
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
                    Some(self),
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
                Some(self),
                Some(elem.into()),
                XmlParserErrors::XmlDTDNotationValue,
                format!(
                    "Value \"{}\" for attribute {} of {} is not among the enumerated notations\n",
                    value,
                    attr_name,
                    elem_name,
                ).as_str(),
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
                    Some(self),
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
                Some(self),
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
        ret &= self.validate_attribute_value2(
            doc,
            attr.name().as_deref().unwrap(),
            attr_decl.atype,
            value,
        );

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
    fn validate_attribute_value2(
        &mut self,
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
                            Some(self),
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
                        Some(self),
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
                    .split(|c: char| xml_is_blank_char(c as u32))
                    .filter(|nam| !nam.is_empty())
                {
                    if let Some(ent) = xml_get_doc_entity(Some(doc), nam) {
                        if !matches!(ent.etype, XmlEntityType::XmlExternalGeneralUnparsedEntity) {
                            xml_err_valid_node(
                            Some(self),
                            Some(doc.into()),
                            XmlParserErrors::XmlDTDEntityType,
                            format!(
                                "ENTITIES attribute {} reference an entity \"{}\" of wrong type\n",
                                name, nam
                            ).as_str(),
                            Some(name),
                            Some(nam),
                            None,
                        );
                            ret = 0;
                        }
                    } else {
                        xml_err_valid_node(
                            Some(self),
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
                        Some(self),
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

    /// Check that an element follows #CDATA
    ///
    /// returns 1 if valid or 0 otherwise
    #[doc(alias = "xmlValidateCdataElement")]
    fn validate_one_cdata_element(&mut self, _doc: XmlDocPtr, elem: XmlNodePtr) -> i32 {
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
                        self.node_vpush(now);
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
                cur = self.node_vpop().map(|node| node.into());
                let Some(now) = cur else {
                    break;
                };
                cur = now.next();
            }
        }
        // done:
        self.vctxt.node_tab.clear();
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
    pub fn validate_one_namespace(
        &mut self,
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
                    attr_decl = doc.ext_subset.and_then(|dtd| {
                        dtd.get_attr_desc(elem.name().as_deref().unwrap(), "xmlns")
                    });
                }
            }
        }

        // Validity Constraint: Attribute Value Type
        let Some(attr_decl) = attr_decl else {
            if let Some(prefix) = ns.prefix() {
                let elem_name = elem.name().unwrap();
                xml_err_valid_node(
                    Some(self),
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
                    Some(self),
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
                    Some(self),
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
                    Some(self),
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
                    Some(self),
                    Some(elem.into()),
                    XmlParserErrors::XmlDTDAttributeDefault,
                    format!(
                        "Value for attribute xmlns:{} of {} is different from default \"{}\"\n",
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
                    Some(self),
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
                    Some(self),
                    Some(elem.into()),
                    XmlParserErrors::XmlDTDUnknownNotation,
                    format!(
                        "Value \"{}\" for attribute xmlns:{} of {} is not a declared Notation\n",
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
                        Some(self),
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
                    Some(self),
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
                    Some(self),
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
                    Some(self),
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
                    Some(self),
                    Some(elem.into()),
                    XmlParserErrors::XmlDTDAttributeValue,
                    format!(
                        "Value \"{}\" for attribute xmlns of {} is not among the enumerated set\n",
                        value,
                        elem_name,
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
                    Some(self),
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
                    Some(self),
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
            ret &= self.validate_attribute_value2(doc, &prefix, attr_decl.atype, value);
        } else {
            ret &= self.validate_attribute_value2(doc, "xmlns", attr_decl.atype, value);
        }

        ret
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
    pub fn validate_dtd_final(&mut self, doc: XmlDocPtr) -> i32 {
        if doc.int_subset.is_none() && doc.ext_subset.is_none() {
            return 0;
        }
        self.vctxt.doc = Some(doc);
        self.vctxt.valid = 1;
        let dtd = doc.int_subset;
        if let Some(dtd) = dtd {
            if let Some(table) = dtd.attributes {
                table.scan(|&payload, _, _, _| {
                    self.validate_attribute_callback(payload);
                });
            }
            if let Some(entities) = dtd.entities {
                entities.scan(|payload, _, _, _| {
                    self.validate_notation_callback(*payload);
                });
            }
        }
        let dtd = doc.ext_subset;
        if let Some(dtd) = dtd {
            if let Some(table) = dtd.attributes {
                table.scan(|payload, _, _, _| {
                    self.validate_attribute_callback(*payload);
                });
            }
            if let Some(entities) = dtd.entities {
                entities.scan(|entity, _, _, _| {
                    self.validate_notation_callback(*entity);
                });
            }
        }
        self.vctxt.valid
    }

    fn validate_attribute_callback(&mut self, cur: XmlAttributePtr) {
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
                    ret = self.validate_attribute_value2(
                        self.vctxt.doc.unwrap(),
                        cur.name.as_deref().unwrap(),
                        cur.atype,
                        def,
                    );
                    if ret == 0 && self.vctxt.valid == 1 {
                        self.vctxt.valid = 0;
                    }
                }
                if cur.tree.is_some() {
                    let mut tree = cur.tree.as_deref();
                    while let Some(now) = tree {
                        ret = self.validate_attribute_value2(
                            self.vctxt.doc.unwrap(),
                            cur.name.as_deref().unwrap(),
                            cur.atype,
                            &now.name,
                        );
                        if ret == 0 && self.vctxt.valid == 1 {
                            self.vctxt.valid = 0;
                        }
                        tree = now.next.as_deref();
                    }
                }
            }
        }
        if matches!(cur.atype, XmlAttributeType::XmlAttributeNotation) {
            let doc = cur.doc;
            let Some(cur_elem) = cur.elem.as_deref() else {
                xml_err_valid(
                    Some(self),
                    XmlParserErrors::XmlErrInternalError,
                    format!(
                        "xmlValidateAttributeCallback({}): internal error\n",
                        cur.name.as_deref().unwrap()
                    )
                    .as_str(),
                    cur.name().as_deref(),
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
                    Some(self),
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
                    Some(self),
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
                self.vctxt.valid = 0;
            }
        }
    }

    fn validate_notation_callback(&mut self, cur: XmlEntityPtr) {
        if matches!(cur.etype, XmlEntityType::XmlExternalGeneralUnparsedEntity) {
            if let Some(notation) = cur.content.as_deref() {
                let ret: i32 = self.validate_notation_use(cur.doc.unwrap(), notation);
                if ret != 1 {
                    self.vctxt.valid = 0;
                }
            }
        }
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
    pub fn validate_element_decl(&mut self, doc: XmlDocPtr, elem: Option<XmlElementPtr>) -> i32 {
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
                let now = now.borrow();
                if !matches!(now.typ, XmlElementContentType::XmlElementContentOr) {
                    break;
                }
                let Some(c1) = now.c1.as_deref().map(|c1| c1.borrow()) else {
                    break;
                };
                if matches!(c1.typ, XmlElementContentType::XmlElementContentElement) {
                    let name = c1.name.as_deref().unwrap();
                    let mut next = now.c2.clone();
                    while let Some(nx) = next {
                        let nx = nx.borrow();
                        if matches!(nx.typ, XmlElementContentType::XmlElementContentElement) {
                            if nx.name.as_deref() == Some(name) && nx.prefix == c1.prefix {
                                let elem_name = elem.name.as_deref().unwrap();
                                if let Some(prefix) = c1.prefix.as_deref() {
                                    xml_err_valid_node(
                                        Some(self),
                                        Some(elem.into()),
                                        XmlParserErrors::XmlDTDContentError,
                                        format!(
                                            "Definition of {} has duplicate references of {}:{}\n",
                                            elem_name, prefix, name
                                        )
                                        .as_str(),
                                        Some(elem_name),
                                        Some(prefix),
                                        Some(name),
                                    );
                                } else {
                                    xml_err_valid_node(
                                        Some(self),
                                        Some(elem.into()),
                                        XmlParserErrors::XmlDTDContentError,
                                        format!(
                                            "Definition of {} has duplicate references of {}\n",
                                            elem_name, name
                                        )
                                        .as_str(),
                                        Some(elem_name),
                                        Some(name),
                                        None,
                                    );
                                }
                                ret = 0;
                            }
                            break;
                        }
                        let Some(nx_c1) = nx.c1.as_deref().map(|c1| c1.borrow()) else {
                            break;
                        };
                        if !matches!(nx_c1.typ, XmlElementContentType::XmlElementContentElement) {
                            break;
                        }
                        if nx_c1.name.as_deref() == Some(name) && nx_c1.prefix == c1.prefix {
                            let elem_name = elem.name.as_deref().unwrap();
                            if let Some(prefix) = c1.prefix.as_deref() {
                                xml_err_valid_node(
                                    Some(self),
                                    Some(elem.into()),
                                    XmlParserErrors::XmlDTDContentError,
                                    format!(
                                        "Definition of {} has duplicate references to {}:{}\n",
                                        elem_name, prefix, name
                                    )
                                    .as_str(),
                                    Some(elem_name),
                                    Some(prefix),
                                    Some(name),
                                );
                            } else {
                                xml_err_valid_node(
                                    Some(self),
                                    Some(elem.into()),
                                    XmlParserErrors::XmlDTDContentError,
                                    format!(
                                        "Definition of {} has duplicate references to {}\n",
                                        elem_name, name
                                    )
                                    .as_str(),
                                    Some(elem_name),
                                    Some(name),
                                    None,
                                );
                            }
                            ret = 0;
                        }
                        next = nx.c2.clone();
                    }
                }
                cur = now.c2.clone();
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
                Some(self),
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
                Some(self),
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
    pub fn validate_attribute_decl(&mut self, doc: XmlDocPtr, attr: XmlAttributePtr) -> i32 {
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
                    Some(self),
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
                XmlAttributeDefault::XmlAttributeImplied
                    | XmlAttributeDefault::XmlAttributeRequired
            )
        {
            let attr_name = attr.name().unwrap();
            xml_err_valid_node(
                Some(self),
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
                    if let Some(table) = int_subset.attributes {
                        table.scan(|&payload, _, _, name3| {
                            if matches!(payload.atype, XmlAttributeType::XmlAttributeID)
                                && name3.map(|n| n.as_ref()) == attr_elem
                            {
                                nb_id += 1;
                            }
                        });
                    }
                }
            }
            if nb_id > 1 {
                xml_err_valid_node_nr(
                    Some(self),
                    Some(attr.into()),
                    XmlParserErrors::XmlDTDIDSubset,
                    format!(
                        "Element {} has {} ID attribute defined in the internal subset : {}\n",
                        attr_elem.unwrap(),
                        nb_id,
                        attr.name.as_deref().unwrap()
                    )
                    .as_str(),
                    attr_elem.unwrap(),
                    nb_id,
                    attr.name.as_deref().unwrap(),
                );
            } else if doc.ext_subset.is_some() {
                let mut ext_id: i32 = 0;
                elem = xml_get_dtd_element_desc(doc.ext_subset, attr_elem.unwrap());
                if let Some(elem) = elem {
                    ext_id = xml_scan_id_attribute_decl(None, elem, 0);
                }
                if ext_id > 1 {
                    xml_err_valid_node_nr(
                        Some(self),
                        Some(attr.into()),
                        XmlParserErrors::XmlDTDIDSubset,
                        format!(
                            "Element {} has {} ID attribute defined in the external subset : {}\n",
                            attr_elem.unwrap(),
                            ext_id,
                            attr.name.as_deref().unwrap()
                        )
                        .as_str(),
                        attr_elem.unwrap(),
                        ext_id,
                        attr.name.as_deref().unwrap(),
                    );
                } else if ext_id + nb_id > 1 {
                    let attr_name = attr.name().unwrap();
                    xml_err_valid_node(
                    Some(self),
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
                Some(self),
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

    /// Validate that the given name match a notation declaration.
    /// - [ VC: Notation Declared ]
    ///
    /// returns 1 if valid or 0 otherwise
    #[doc(alias = "xmlValidateNotationUse")]
    #[cfg(any(feature = "libxml_valid", feature = "schema"))]
    pub fn validate_notation_use(&mut self, doc: XmlDocPtr, notation_name: &str) -> i32 {
        if doc.int_subset.is_none() {
            return -1;
        }

        let nota_decl = xml_get_dtd_notation_desc(doc.int_subset.as_deref(), notation_name)
            .or_else(|| xml_get_dtd_notation_desc(doc.ext_subset.as_deref(), notation_name));

        if nota_decl.is_none() {
            xml_err_valid_node(
                Some(self),
                Some(doc.into()),
                XmlParserErrors::XmlDTDUnknownNotation,
                format!("NOTATION {} is not declared\n", notation_name).as_str(),
                Some(notation_name),
                None,
                None,
            );
            return 0;
        }
        1
    }

    /// Check if the given node is part of the content model.
    ///
    /// Returns 1 if yes, 0 if no, -1 in case of error
    #[doc(alias = "xmlValidateCheckMixed")]
    #[cfg(feature = "libxml_regexp")]
    fn validate_check_mixed(
        &mut self,
        mut cont: Option<Rc<RefCell<XmlElementContent>>>,
        qname: &str,
    ) -> i32 {
        if let Some((prefix, local)) = split_qname2(qname) {
            while let Some(now) = cont {
                let now = now.borrow();
                if matches!(now.typ, XmlElementContentType::XmlElementContentElement) {
                    if now.prefix.as_deref().is_some_and(|pre| pre == prefix)
                        && now.name.as_deref() == Some(local)
                    {
                        return 1;
                    }
                } else if matches!(now.typ, XmlElementContentType::XmlElementContentOr)
                    && now.c1.as_deref().is_some_and(|c1| {
                        c1.borrow().typ == XmlElementContentType::XmlElementContentElement
                    })
                {
                    let c1 = now.c1.as_deref().unwrap();
                    if c1
                        .borrow()
                        .prefix
                        .as_deref()
                        .is_some_and(|pre| pre == prefix)
                        && c1.borrow().name.as_deref() == Some(local)
                    {
                        return 1;
                    }
                } else if !matches!(now.typ, XmlElementContentType::XmlElementContentOr)
                    || now.c1.as_deref().is_none_or(|c1| {
                        c1.borrow().typ != XmlElementContentType::XmlElementContentPCDATA
                    })
                {
                    xml_err_valid(
                        Some(self),
                        XmlParserErrors::XmlDTDMixedCorrupt,
                        "Internal: MIXED struct corrupted\n",
                        None,
                    );
                    break;
                }
                cont = now.c2.clone();
            }
        } else {
            while let Some(now) = cont {
                let now = now.borrow();
                if matches!(now.typ, XmlElementContentType::XmlElementContentElement) {
                    if now.prefix.is_none() && now.name.as_deref() == Some(qname) {
                        return 1;
                    }
                } else if matches!(now.typ, XmlElementContentType::XmlElementContentOr)
                    && now.c1.as_deref().is_some_and(|c1| {
                        c1.borrow().typ == XmlElementContentType::XmlElementContentElement
                    })
                {
                    let c1 = now.c1.as_deref().unwrap();
                    if c1.borrow().prefix.is_none() && c1.borrow().name.as_deref() == Some(qname) {
                        return 1;
                    }
                } else if !matches!(now.typ, XmlElementContentType::XmlElementContentOr)
                    || now.c1.as_deref().is_none_or(|c1| {
                        c1.borrow().typ != XmlElementContentType::XmlElementContentPCDATA
                    })
                {
                    xml_err_valid(
                        None,
                        XmlParserErrors::XmlDTDMixedCorrupt,
                        "Internal: MIXED struct corrupted\n",
                        None,
                    );
                    break;
                }
                cont = now.c2.clone();
            }
        }
        0
    }
}

/// Verify that the element don't have too many ID attributes
/// declared.
///
/// Returns the number of ID attributes found.
#[doc(alias = "xmlScanIDAttributeDecl")]
#[cfg(feature = "libxml_valid")]
fn xml_scan_id_attribute_decl(
    mut ctxt: Option<&mut XmlParserCtxt>,
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
                    if let Some(ctxt) = ctxt.as_mut() {
                        Some(&mut *ctxt)
                    } else {
                        None
                    },
                    Some(elem.into()),
                    XmlParserErrors::XmlDTDMultipleID,
                    format!(
                        "Element {} has too many ID attributes defined : {}\n",
                        elem_name, cur_name
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

/// Handle a validation error, provide contextual information
///
/// # Note
/// This function does not format the string.
#[doc(alias = "xmlErrValidNode")]
#[cfg(any(feature = "libxml_valid", feature = "schema"))]
fn xml_err_valid_node(
    ctxt: Option<&mut XmlParserCtxt>,
    node: Option<XmlGenericNodePtr>,
    error: XmlParserErrors,
    msg: &str,
    str1: Option<&str>,
    str2: Option<&str>,
    str3: Option<&str>,
) {
    let channel = ctxt.as_ref().and_then(|ctxt| ctxt.vctxt.error);
    let data = ctxt.as_ref().and_then(|ctxt| ctxt.vctxt.user_data.clone());
    let pctxt = ctxt.map_or(null_mut(), |ctxt| ctxt as XmlParserCtxtPtr);
    __xml_raise_error!(
        None,
        channel,
        data,
        pctxt as _,
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

/// Handle a validation error, provide contextual information
#[doc(alias = "xmlErrValidNodeNr")]
fn xml_err_valid_node_nr(
    ctxt: Option<&mut XmlParserCtxt>,
    node: Option<XmlGenericNodePtr>,
    error: XmlParserErrors,
    msg: &str,
    str1: &str,
    int2: i32,
    str3: &str,
) {
    let channel = ctxt.as_ref().and_then(|ctxt| ctxt.vctxt.error);
    let data = ctxt.as_ref().and_then(|ctxt| ctxt.vctxt.user_data.clone());
    let pctxt = ctxt.map_or(null_mut(), |ctxt| ctxt as XmlParserCtxtPtr);
    __xml_raise_error!(
        None,
        channel,
        data,
        pctxt as _,
        node,
        XmlErrorDomain::XmlFromValid,
        error,
        XmlErrorLevel::XmlErrError,
        None,
        0,
        Some(str1.to_owned().into()),
        Some(str3.to_owned().into()),
        None,
        int2,
        0,
        Some(msg),
    );
}

/// Handle an out of memory error
#[doc(alias = "xmlVErrMemory")]
fn xml_verr_memory(ctxt: Option<&mut XmlParserCtxt>, extra: Option<&str>) {
    let channel = ctxt.as_ref().and_then(|ctxt| ctxt.vctxt.error);
    let data = ctxt.as_ref().and_then(|ctxt| ctxt.vctxt.user_data.clone());
    let pctxt = ctxt.map_or(null_mut(), |ctxt| ctxt as XmlParserCtxtPtr);
    if let Some(extra) = extra {
        __xml_raise_error!(
            None,
            channel,
            data,
            pctxt as _,
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
            pctxt as _,
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

/// Handle a validation error, provide contextual information
#[doc(alias = "xmlErrValidWarning")]
fn xml_err_valid_warning(
    ctxt: Option<&mut XmlParserCtxt>,
    node: Option<XmlGenericNodePtr>,
    error: XmlParserErrors,
    msg: &str,
    str1: Option<&str>,
    str2: Option<&str>,
    str3: Option<&str>,
) {
    let channel = ctxt.as_ref().and_then(|ctxt| ctxt.vctxt.warning);
    let data = ctxt.as_ref().and_then(|ctxt| ctxt.vctxt.user_data.clone());
    let pctxt = ctxt.map_or(null_mut(), |ctxt| ctxt as XmlParserCtxtPtr);
    __xml_raise_error!(
        None,
        channel,
        data,
        pctxt as _,
        node,
        XmlErrorDomain::XmlFromValid,
        error,
        XmlErrorLevel::XmlErrWarning,
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

/// Handle a validation error
#[doc(alias = "xmlErrValid")]
fn xml_err_valid(
    ctxt: Option<&mut XmlParserCtxt>,
    error: XmlParserErrors,
    msg: &str,
    extra: Option<&str>,
) {
    let channel = ctxt.as_ref().and_then(|ctxt| ctxt.vctxt.error);
    let data = ctxt.as_ref().and_then(|ctxt| ctxt.vctxt.user_data.clone());
    let pctxt = ctxt.map_or(null_mut(), |ctxt| ctxt as XmlParserCtxtPtr);
    __xml_raise_error!(
        None,
        channel,
        data,
        pctxt as _,
        None,
        XmlErrorDomain::XmlFromValid,
        error,
        XmlErrorLevel::XmlErrError,
        None,
        0,
        extra.map(|extra| extra.to_owned().into()),
        None,
        None,
        0,
        0,
        Some(msg),
    );
}
