use std::{
    ffi::{CStr, CString, c_void},
    ptr::{null, null_mut},
};

use libc::memset;

use crate::{
    dict::xml_dict_lookup,
    error::XmlParserErrors,
    libxml::{
        chvalid::xml_is_blank_char,
        globals::{xml_free, xml_malloc},
        schemas_internals::{
            XML_SCHEMAS_ANY_LAX, XML_SCHEMAS_ANY_SKIP, XML_SCHEMAS_ANY_STRICT,
            XML_SCHEMAS_ATTR_FIXED, XML_SCHEMAS_ATTR_GLOBAL, XML_SCHEMAS_ATTR_USE_OPTIONAL,
            XML_SCHEMAS_ATTR_USE_PROHIBITED, XML_SCHEMAS_ATTR_USE_REQUIRED,
            XML_SCHEMAS_ATTRGROUP_HAS_REFS, XML_SCHEMAS_BLOCK_DEFAULT_EXTENSION,
            XML_SCHEMAS_BLOCK_DEFAULT_RESTRICTION, XML_SCHEMAS_BLOCK_DEFAULT_SUBSTITUTION,
            XML_SCHEMAS_ELEM_ABSTRACT, XML_SCHEMAS_ELEM_BLOCK_EXTENSION,
            XML_SCHEMAS_ELEM_BLOCK_RESTRICTION, XML_SCHEMAS_ELEM_BLOCK_SUBSTITUTION,
            XML_SCHEMAS_ELEM_FINAL_EXTENSION, XML_SCHEMAS_ELEM_FINAL_RESTRICTION,
            XML_SCHEMAS_ELEM_FIXED, XML_SCHEMAS_ELEM_GLOBAL, XML_SCHEMAS_ELEM_NILLABLE,
            XML_SCHEMAS_ELEM_TOPLEVEL, XML_SCHEMAS_FINAL_DEFAULT_EXTENSION,
            XML_SCHEMAS_FINAL_DEFAULT_LIST, XML_SCHEMAS_FINAL_DEFAULT_RESTRICTION,
            XML_SCHEMAS_FINAL_DEFAULT_UNION, XML_SCHEMAS_INCLUDING_CONVERT_NS,
            XML_SCHEMAS_QUALIF_ATTR, XML_SCHEMAS_QUALIF_ELEM, XML_SCHEMAS_TYPE_ABSTRACT,
            XML_SCHEMAS_TYPE_BLOCK_EXTENSION, XML_SCHEMAS_TYPE_BLOCK_RESTRICTION,
            XML_SCHEMAS_TYPE_DERIVATION_METHOD_EXTENSION,
            XML_SCHEMAS_TYPE_DERIVATION_METHOD_RESTRICTION, XML_SCHEMAS_TYPE_FINAL_EXTENSION,
            XML_SCHEMAS_TYPE_FINAL_LIST, XML_SCHEMAS_TYPE_FINAL_RESTRICTION,
            XML_SCHEMAS_TYPE_FINAL_UNION, XML_SCHEMAS_TYPE_GLOBAL, XML_SCHEMAS_TYPE_MIXED,
            XML_SCHEMAS_TYPE_VARIETY_LIST, XML_SCHEMAS_TYPE_VARIETY_UNION, XmlSchemaAnnotPtr,
            XmlSchemaContentType, XmlSchemaFacetLink, XmlSchemaFacetLinkPtr, XmlSchemaFacetPtr,
            XmlSchemaTypeLink, XmlSchemaTypeLinkPtr, XmlSchemaTypeType, XmlSchemaValType,
            XmlSchemaWildcardNsPtr, XmlSchemaWildcardPtr, xml_schema_free_annot,
        },
        xmlschemas::{
            XML_SCHEMA_ATTR_USE_FIXED, XML_SCHEMA_INSTANCE_NS, XML_SCHEMA_NS,
            XML_SCHEMA_SCHEMA_IMPORT, XML_SCHEMA_SCHEMA_INCLUDE, XML_SCHEMA_SCHEMA_MAIN,
            XML_SCHEMA_SCHEMA_REDEFINE, XmlSchemaAbstractCtxtPtr, XmlSchemaBucketPtr,
            XmlSchemaIdcselect, XmlSchemaIdcselectPtr, can_parse_schema, xml_get_boolean_prop,
            xml_get_max_occurs, xml_get_min_occurs, xml_schema_add_annotation,
            xml_schema_add_attribute, xml_schema_add_attribute_group_definition,
            xml_schema_add_attribute_use, xml_schema_add_attribute_use_prohib,
            xml_schema_add_element, xml_schema_add_idc, xml_schema_add_item_size,
            xml_schema_add_model_group, xml_schema_add_model_group_definition,
            xml_schema_add_notation, xml_schema_add_particle, xml_schema_add_schema_doc,
            xml_schema_add_type, xml_schema_add_wildcard, xml_schema_build_absolute_uri,
            xml_schema_check_cselector_xpath, xml_schema_check_reference,
            xml_schema_construction_ctxt_create, xml_schema_construction_ctxt_free,
            xml_schema_fixup_components, xml_schema_format_qname, xml_schema_get_prop_node,
            xml_schema_get_prop_node_ns, xml_schema_get_qname_ref_name,
            xml_schema_get_qname_ref_target_ns, xml_schema_new_annot, xml_schema_new_qname_ref,
            xml_schema_new_wildcard_ns_constraint, xml_schema_pcheck_particle_correct_2,
            xml_schema_pget_bool_node_value, xml_schema_pval_attr,
            xml_schema_pval_attr_block_final, xml_schema_pval_attr_form_default,
            xml_schema_pval_attr_id, xml_schema_pval_attr_node, xml_schema_pval_attr_node_id,
            xml_schema_pval_attr_node_qname, xml_schema_pval_attr_node_qname_value,
            xml_schema_pval_attr_node_value, xml_schema_pval_attr_qname,
        },
        xmlschemastypes::{
            xml_schema_free_facet, xml_schema_get_built_in_type, xml_schema_get_predefined_type,
            xml_schema_init_types, xml_schema_new_facet,
        },
        xmlstring::xml_str_equal,
    },
    tree::{NodeCommon, XML_XML_NAMESPACE, XmlNodePtr},
    uri::build_uri,
    xmlschemas::{
        UNBOUNDED,
        error::{
            xml_schema_custom_err, xml_schema_internal_err, xml_schema_pcustom_attr_err,
            xml_schema_pmutual_excl_attr_err, xml_schema_psimple_type_err,
        },
        items::{
            XmlSchemaAttributeUseProhibPtr, XmlSchemaAttributeUsePtr, XmlSchemaElementPtr,
            XmlSchemaIDCPtr, XmlSchemaParticlePtr, XmlSchemaQNameRefPtr, XmlSchemaTreeItemPtr,
        },
        schema::xml_schema_free,
    },
};

use super::{
    context::{
        XmlSchemaParserCtxt, XmlSchemaParserCtxtPtr, xml_schema_free_parser_ctxt,
        xml_schema_new_parser_ctxt_use_dict,
    },
    error::{
        xml_schema_custom_warning, xml_schema_pcontent_err, xml_schema_pcustom_err,
        xml_schema_pcustom_err_ext, xml_schema_perr, xml_schema_perr_memory, xml_schema_perr2,
        xml_schema_pillegal_attr_err, xml_schema_pmissing_attr_err,
    },
    is_schema,
    item_list::{XmlSchemaItemListPtr, xml_schema_item_list_create},
    items::{
        XmlSchemaAnnotItemPtr, XmlSchemaAttributeGroupPtr, XmlSchemaAttributePtr,
        XmlSchemaBasicItemPtr, XmlSchemaModelGroupDefPtr, XmlSchemaModelGroupPtr,
        XmlSchemaNotationPtr, XmlSchemaTypePtr,
    },
    schema::XmlSchemaPtr,
};

impl XmlSchemaParserCtxt {
    /// parse a schema definition resource and build an internal
    /// XML Schema structure which can be used to validate instances.
    ///
    /// Returns the internal XML Schema structure built from the resource or NULL in case of error
    #[doc(alias = "xmlSchemaParse")]
    pub unsafe fn parse(&mut self) -> XmlSchemaPtr {
        unsafe {
            let mut main_schema: XmlSchemaPtr;
            let mut bucket: XmlSchemaBucketPtr = null_mut();
            let res: i32;

            // This one is used if the schema to be parsed was specified via
            // the API; i.e. not automatically by the validated instance document.

            if xml_schema_init_types() < 0 {
                return null_mut();
            }

            // TODO: Init the context. Is this all we need?
            self.nberrors = 0;
            self.err = 0;
            self.counter = 0;

            // Create the *main* schema.
            'exit_failure: {
                main_schema = self.new_schema();
                if main_schema.is_null() {
                    break 'exit_failure;
                }
                // Create the schema constructor.
                if self.constructor.is_null() {
                    self.constructor = xml_schema_construction_ctxt_create(self.dict);
                    if self.constructor.is_null() {
                        break 'exit_failure;
                    }
                    // Take ownership of the constructor to be able to free it.
                    self.owns_constructor = 1;
                }
                (*self.constructor).main_schema = main_schema;
                let url = self.url.as_deref().map(|url| CString::new(url).unwrap());
                // Locate and add the schema document.
                res = xml_schema_add_schema_doc(
                    self,
                    XML_SCHEMA_SCHEMA_MAIN,
                    url.as_deref()
                        .map_or(null_mut(), |url| url.as_ptr() as *const u8),
                    self.doc,
                    self.buffer,
                    self.size,
                    None,
                    null_mut(),
                    null_mut(),
                    &raw mut bucket,
                );
                if res == -1 {
                    break 'exit_failure;
                }
                'exit: {
                    if res != 0 {
                        break 'exit;
                    }
                    if bucket.is_null() {
                        let ctxt = self as *mut Self as XmlSchemaAbstractCtxtPtr;
                        // TODO: Error code, actually we failed to *locate* the schema.
                        if let Some(url) = self.url.as_deref() {
                            xml_schema_custom_err(
                                ctxt,
                                XmlParserErrors::XmlSchemapFailedLoad,
                                None,
                                null_mut(),
                                format!("Failed to locate the main schema resource at '{url}'")
                                    .as_str(),
                                Some(url),
                                None,
                            );
                        } else {
                            xml_schema_custom_err(
                                ctxt,
                                XmlParserErrors::XmlSchemapFailedLoad,
                                None,
                                null_mut(),
                                "Failed to locate themain schema resource",
                                None,
                                None,
                            );
                        }
                        break 'exit;
                    }
                    // Then do the parsing for good.
                    if self.parse_new_doc_with_context(main_schema, bucket) == -1 {
                        break 'exit_failure;
                    }
                    if self.nberrors != 0 {
                        break 'exit;
                    }

                    (*main_schema).doc = (*bucket).doc;
                    (*main_schema).preserve = self.preserve;

                    self.schema = main_schema;

                    if xml_schema_fixup_components(self, (*self.constructor).main_bucket) == -1 {
                        break 'exit_failure;
                    }
                }

                // TODO: This is not nice, since we cannot distinguish from the
                // result if there was an internal error or not.
                // exit:
                if self.nberrors != 0 {
                    if !main_schema.is_null() {
                        xml_schema_free(main_schema);
                        main_schema = null_mut();
                    }
                    if !self.constructor.is_null() {
                        xml_schema_construction_ctxt_free(self.constructor);
                        self.constructor = null_mut();
                        self.owns_constructor = 0;
                    }
                }
                self.schema = null_mut();
                return main_schema;
            }
            // exit_failure:
            // Quite verbose, but should catch internal errors, which were not communicated.
            if !main_schema.is_null() {
                xml_schema_free(main_schema);
                // main_schema = null_mut();
            }
            if !self.constructor.is_null() {
                xml_schema_construction_ctxt_free(self.constructor);
                self.constructor = null_mut();
                self.owns_constructor = 0;
            }
            xml_schema_internal_err(
                self as *mut Self as _,
                "xmlSchemaParse",
                "An internal error occurred",
            );
            self.schema = null_mut();
            null_mut()
        }
    }

    #[doc(alias = "xmlSchemaParseNewDoc")]
    unsafe fn parse_new_doc(&mut self, schema: XmlSchemaPtr, bucket: XmlSchemaBucketPtr) -> i32 {
        unsafe {
            if bucket.is_null() {
                return 0;
            }
            if (*bucket).parsed != 0 {
                xml_schema_internal_err(
                    self as *mut Self as XmlSchemaAbstractCtxtPtr,
                    "xmlSchemaParseNewDoc",
                    "reparsing a schema doc",
                );
                return -1;
            }
            if (*bucket).doc.is_none() {
                xml_schema_internal_err(
                    self as *mut Self as XmlSchemaAbstractCtxtPtr,
                    "xmlSchemaParseNewDoc",
                    "parsing a schema doc, but there's no doc",
                );
                return -1;
            }
            if self.constructor.is_null() {
                xml_schema_internal_err(
                    self as *mut Self as XmlSchemaAbstractCtxtPtr,
                    "xmlSchemaParseNewDoc",
                    "no constructor",
                );
                return -1;
            }
            // Create and init the temporary parser context.
            let newpctxt: XmlSchemaParserCtxtPtr = xml_schema_new_parser_ctxt_use_dict(
                (!(*bucket).schema_location.is_null())
                    .then(|| {
                        CStr::from_ptr((*bucket).schema_location as *const i8).to_string_lossy()
                    })
                    .as_deref(),
                self.dict,
            );
            if newpctxt.is_null() {
                return -1;
            }
            (*newpctxt).constructor = self.constructor;
            // TODO: Can we avoid that the parser knows about the main schema?
            // It would be better if he knows about the current schema bucket only.
            (*newpctxt).schema = schema;
            (*newpctxt).set_errors(self.error, self.warning, self.err_ctxt.clone());
            (*newpctxt).set_structured_errors(self.serror, self.err_ctxt.clone());
            (*newpctxt).counter = self.counter;

            let res: i32 = (*newpctxt).parse_new_doc_with_context(schema, bucket);

            // Channel back errors and cleanup the temporary parser context.
            if res != 0 {
                self.err = res;
            }
            self.nberrors += (*newpctxt).nberrors;
            self.counter = (*newpctxt).counter;
            (*newpctxt).constructor = null_mut();
            // Free the parser context.
            xml_schema_free_parser_ctxt(newpctxt);
            res
        }
    }

    #[doc(alias = "xmlSchemaParseNewDocWithContext")]
    pub(crate) unsafe fn parse_new_doc_with_context(
        &mut self,
        schema: XmlSchemaPtr,
        bucket: XmlSchemaBucketPtr,
    ) -> i32 {
        unsafe {
            let mut ret: i32;
            let old_errs: i32;
            let oldbucket: XmlSchemaBucketPtr = (*self.constructor).bucket;

            // Save old values; reset the *main* schema.
            // URGENT TODO: This is not good; move the per-document information
            // to the parser. Get rid of passing the main schema to the
            // parsing functions.
            let old_flags: i32 = (*schema).flags;
            let old_doc = (*schema).doc;
            if (*schema).flags != 0 {
                (*schema).clear_flags();
            }
            (*schema).doc = (*bucket).doc;
            self.schema = schema;
            // Keep the current target namespace on the parser *not* on the main schema.
            self.target_namespace = (*bucket).target_namespace;
            (*self.constructor).bucket = bucket;

            if !(*bucket).target_namespace.is_null()
                && xml_str_equal((*bucket).target_namespace, XML_SCHEMA_NS.as_ptr() as _)
            {
                // We are parsing the schema for schemas!
                self.is_s4s = 1;
            }
            // Mark it as parsed, even if parsing fails.
            (*bucket).parsed += 1;
            // Compile the schema doc.
            let node = (*bucket)
                .doc
                .and_then(|doc| doc.get_root_element())
                .unwrap();
            ret = self.parse_schema_element(schema, node);
            if ret == 0 {
                // An empty schema; just get out.
                if let Some(children) = node
                    .children
                    .map(|children| XmlNodePtr::try_from(children).unwrap())
                {
                    old_errs = self.nberrors;
                    ret = self.parse_schema_top_level(schema, children);
                    if ret == 0 {
                        // TODO: Not nice, but I'm not 100% sure we will get always an error
                        // as a result of the above functions; so better rely on self.err
                        // as well.
                        if ret == 0 && old_errs != self.nberrors {
                            ret = self.err;
                            // goto exit;
                        }
                    }
                }
            }

            // exit:
            (*self.constructor).bucket = oldbucket;
            // Restore schema values.
            (*schema).doc = old_doc;
            (*schema).flags = old_flags;
            ret
        }
    }

    #[doc(alias = "xmlSchemaParseSchemaElement")]
    unsafe fn parse_schema_element(&mut self, schema: XmlSchemaPtr, node: XmlNodePtr) -> i32 {
        unsafe {
            let mut res: i32;
            let old_errs: i32 = self.nberrors;

            // Those flags should be moved to the parser context flags,
            // since they are not visible at the component level. I.e.
            // they are used if processing schema *documents* only.
            'exit_failure: {
                res = xml_schema_pval_attr_id(self, node, "id");
                if res == -1 {
                    break 'exit_failure;
                };

                // Since the version is of type xs:token, we won't bother to check it.
                // REMOVED:
                // attr = xmlSchemaGetPropNode(node, c"version".as_ptr() as _);
                // if !attr.is_null() {
                //     res = xmlSchemaPValAttrNode(self, null_mut(), NULL, attr, xmlSchemaGetBuiltInType(XML_SCHEMAS_TOKEN), &val);
                //     if res == -1 {
                //         goto exit_failure;
                //     }
                // }
                'exit: {
                    if let Some(attr) = xml_schema_get_prop_node(node, "targetNamespace") {
                        res = xml_schema_pval_attr_node(
                            self,
                            null_mut(),
                            attr,
                            xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasAnyURI),
                            null_mut(),
                        );
                        if res == -1 {
                            break 'exit_failure;
                        };
                        if res != 0 {
                            self.stop = XmlParserErrors::XmlSchemapS4sAttrInvalidValue as i32;
                            break 'exit;
                        }
                    }
                    if let Some(attr) = xml_schema_get_prop_node(node, "elementFormDefault") {
                        let val = self.get_node_content(Some(attr.into()));
                        let cval = xml_dict_lookup(self.dict, val.as_ptr(), val.len() as i32);
                        res = xml_schema_pval_attr_form_default(
                            cval,
                            &raw mut (*schema).flags,
                            XML_SCHEMAS_QUALIF_ELEM,
                        );
                        if res == -1 {
                            break 'exit_failure;
                        };
                        if res != 0 {
                            xml_schema_psimple_type_err(
                                self,
                                XmlParserErrors::XmlSchemapElemFormDefaultValue,
                                null_mut(),
                                attr.into(),
                                null_mut(),
                                Some("(qualified | unqualified)"),
                                Some(&val),
                                None,
                                None,
                                None,
                            );
                        }
                    }
                    if let Some(attr) = xml_schema_get_prop_node(node, "attributeFormDefault") {
                        let val = self.get_node_content(Some(attr.into()));
                        let cval = xml_dict_lookup(self.dict, val.as_ptr(), val.len() as i32);
                        res = xml_schema_pval_attr_form_default(
                            cval,
                            &raw mut (*schema).flags,
                            XML_SCHEMAS_QUALIF_ATTR,
                        );
                        if res == -1 {
                            break 'exit_failure;
                        };
                        if res != 0 {
                            xml_schema_psimple_type_err(
                                self,
                                XmlParserErrors::XmlSchemapAttrFormDefaultValue,
                                null_mut(),
                                attr.into(),
                                null_mut(),
                                Some("(qualified | unqualified)"),
                                Some(&val),
                                None,
                                None,
                                None,
                            );
                        }
                    }
                    if let Some(attr) = xml_schema_get_prop_node(node, "finalDefault") {
                        let val = self.get_node_content(Some(attr.into()));
                        let cval = xml_dict_lookup(self.dict, val.as_ptr(), val.len() as i32);
                        res = xml_schema_pval_attr_block_final(
                            cval,
                            &raw mut (*schema).flags,
                            -1,
                            XML_SCHEMAS_FINAL_DEFAULT_EXTENSION,
                            XML_SCHEMAS_FINAL_DEFAULT_RESTRICTION,
                            -1,
                            XML_SCHEMAS_FINAL_DEFAULT_LIST,
                            XML_SCHEMAS_FINAL_DEFAULT_UNION,
                        );
                        if res == -1 {
                            break 'exit_failure;
                        };
                        if res != 0 {
                            xml_schema_psimple_type_err(
                                self,
                                XmlParserErrors::XmlSchemapS4sAttrInvalidValue,
                                null_mut(),
                                attr.into(),
                                null_mut(),
                                Some("(#all | List of (extension | restriction | list | union))"),
                                Some(&val),
                                None,
                                None,
                                None,
                            );
                        }
                    }
                    if let Some(attr) = xml_schema_get_prop_node(node, "blockDefault") {
                        let val = self.get_node_content(Some(attr.into()));
                        let cval = xml_dict_lookup(self.dict, val.as_ptr(), val.len() as i32);
                        res = xml_schema_pval_attr_block_final(
                            cval,
                            &raw mut (*schema).flags,
                            -1,
                            XML_SCHEMAS_BLOCK_DEFAULT_EXTENSION,
                            XML_SCHEMAS_BLOCK_DEFAULT_RESTRICTION,
                            XML_SCHEMAS_BLOCK_DEFAULT_SUBSTITUTION,
                            -1,
                            -1,
                        );
                        if res == -1 {
                            break 'exit_failure;
                        };
                        if res != 0 {
                            xml_schema_psimple_type_err(
                                self,
                                XmlParserErrors::XmlSchemapS4sAttrInvalidValue,
                                null_mut(),
                                attr.into(),
                                null_mut(),
                                Some("(#all | List of (extension | restriction | substitution))"),
                                Some(&val),
                                None,
                                None,
                                None,
                            );
                        }
                    }
                }

                // exit:
                if old_errs != self.nberrors {
                    res = self.err;
                }
                return res;
            }
            // exit_failure:
            -1
        }
    }

    /// Returns the internal XML Schema structure built from the resource or NULL in case of error
    #[doc(alias = "xmlSchemaParseSchemaTopLevel")]
    unsafe fn parse_schema_top_level(&mut self, schema: XmlSchemaPtr, nodes: XmlNodePtr) -> i32 {
        unsafe {
            let mut annot: XmlSchemaAnnotPtr;
            let mut res: i32 = 0;
            let mut tmp_old_errs: i32;

            if schema.is_null() {
                return -1;
            }

            let old_errs: i32 = self.nberrors;
            let mut child = Some(nodes);
            'exit: {
                while is_schema(child, "include")
                    || is_schema(child, "import")
                    || is_schema(child, "redefine")
                    || is_schema(child, "annotation")
                {
                    if is_schema(child, "annotation") {
                        annot = self.parse_annotation(child.unwrap(), 1);
                        if (*schema).annot.is_null() {
                            (*schema).annot = annot;
                        } else {
                            xml_schema_free_annot(annot);
                        }
                    } else if is_schema(child, "import") {
                        tmp_old_errs = self.nberrors;
                        res = self.parse_import(schema, child.unwrap());
                        if res == -1 {
                            return -1;
                        };
                        if self.stop != 0 {
                            break 'exit;
                        }
                        if tmp_old_errs != self.nberrors {
                            break 'exit;
                        }
                    } else if is_schema(child, "include") {
                        tmp_old_errs = self.nberrors;
                        res = self.parse_include(schema, child.unwrap());
                        if res == -1 {
                            return -1;
                        };
                        if self.stop != 0 {
                            break 'exit;
                        }
                        if tmp_old_errs != self.nberrors {
                            break 'exit;
                        }
                    } else if is_schema(child, "redefine") {
                        tmp_old_errs = self.nberrors;
                        res = self.parse_redefine(schema, child.unwrap());
                        if res == -1 {
                            return -1;
                        };
                        if self.stop != 0 {
                            break 'exit;
                        }
                        if tmp_old_errs != self.nberrors {
                            break 'exit;
                        }
                    }
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                }
                // URGENT TODO: Change the functions to return i32 results.
                // We need especially to catch internal errors.
                while let Some(cur_node) = child {
                    if is_schema(child, "complexType") {
                        self.parse_complex_type(schema, cur_node, 1);
                        child = cur_node
                            .next
                            .map(|node| XmlNodePtr::try_from(node).unwrap());
                    } else if is_schema(child, "simpleType") {
                        self.parse_simple_type(schema, cur_node, 1);
                        child = cur_node
                            .next
                            .map(|node| XmlNodePtr::try_from(node).unwrap());
                    } else if is_schema(child, "element") {
                        self.parse_element(schema, cur_node, null_mut(), 1);
                        child = cur_node
                            .next
                            .map(|node| XmlNodePtr::try_from(node).unwrap());
                    } else if is_schema(child, "attribute") {
                        self.parse_global_attribute(schema, cur_node);
                        child = cur_node
                            .next
                            .map(|node| XmlNodePtr::try_from(node).unwrap());
                    } else if is_schema(child, "attributeGroup") {
                        self.parse_attribute_group_definition(schema, cur_node);
                        child = cur_node
                            .next
                            .map(|node| XmlNodePtr::try_from(node).unwrap());
                    } else if is_schema(child, "group") {
                        self.parse_model_group_definition(schema, cur_node);
                        child = cur_node
                            .next
                            .map(|node| XmlNodePtr::try_from(node).unwrap());
                    } else if is_schema(child, "notation") {
                        self.parse_notation(schema, cur_node);
                        child = cur_node
                            .next
                            .map(|node| XmlNodePtr::try_from(node).unwrap());
                    } else {
                        xml_schema_pcontent_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                            null_mut(),
                            cur_node
                                .parent
                                .map(|p| XmlNodePtr::try_from(p).unwrap())
                                .unwrap(),
                            Some(cur_node.into()),
                            None,
                            Some(
                                "((include | import | redefine | annotation)*, (((simpleType | complexType | group | attributeGroup) | element | attribute | notation), annotation*)*)",
                            ),
                        );
                        child = cur_node
                            .next
                            .map(|node| XmlNodePtr::try_from(node).unwrap());
                    }
                    while is_schema(child, "annotation") {
                        // TODO: We should add all annotations.
                        annot = self.parse_annotation(child.unwrap(), 1);
                        if (*schema).annot.is_null() {
                            (*schema).annot = annot;
                        } else {
                            xml_schema_free_annot(annot);
                        }
                        child = child
                            .unwrap()
                            .next
                            .map(|node| XmlNodePtr::try_from(node).unwrap());
                    }
                }
            }
            // exit:
            self.ctxt_type = null_mut();
            if old_errs != self.nberrors {
                res = self.err;
            }
            res
        }
    }

    /// parse a XML schema Attribute declaration
    /// *WARNING* this interface is highly subject to change
    ///
    /// Returns -1 in case of error, 0 if the declaration is improper and 1 in case of success.
    #[doc(alias = "xmlSchemaParseAnnotation")]
    unsafe fn parse_annotation(&mut self, node: XmlNodePtr, needed: i32) -> XmlSchemaAnnotPtr {
        unsafe {
            let mut barked: i32 = 0;

            // INFO: S4S completed.

            // id = ID
            // {any attributes with non-schema namespace . . .}>
            // Content: (appinfo | documentation)*
            let ret = if needed != 0 {
                xml_schema_new_annot(self, node)
            } else {
                null_mut()
            };
            let mut attr = node.properties;
            while let Some(now) = attr {
                if (now.ns.is_none() && now.name.as_ref() != "id")
                    || now.ns.is_some_and(|ns| {
                        ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap())
                    })
                {
                    xml_schema_pillegal_attr_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        now,
                    );
                }
                attr = now.next;
            }
            xml_schema_pval_attr_id(self, node, "id");
            // And now for the children...
            let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
            while let Some(cur_node) = child {
                if is_schema(Some(cur_node), "appinfo") {
                    // TODO: make available the content of "appinfo".

                    // source = anyURI
                    // {any attributes with non-schema namespace . . .}>
                    // Content: ({any})*
                    let mut attr = cur_node.properties;
                    while let Some(now) = attr {
                        if (now.ns.is_none() && now.name.as_ref() != "source")
                            || now.ns.is_some_and(|ns| {
                                ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap())
                            })
                        {
                            xml_schema_pillegal_attr_err(
                                self,
                                XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                                null_mut(),
                                now,
                            );
                        }
                        attr = now.next;
                    }
                    xml_schema_pval_attr(
                        self,
                        null_mut(),
                        cur_node,
                        "source",
                        xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasAnyURI),
                        null_mut(),
                    );
                    child = cur_node
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                } else if is_schema(Some(cur_node), "documentation") {
                    // TODO: make available the content of "documentation".

                    // source = anyURI
                    // {any attributes with non-schema namespace . . .}>
                    // Content: ({any})*
                    let mut attr = cur_node.properties;
                    while let Some(cur_attr) = attr {
                        if let Some(ns) = cur_attr.ns {
                            if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap())
                                || (cur_attr.name.as_ref() == "lang"
                                    && ns.href.as_deref() != Some(XML_XML_NAMESPACE))
                            {
                                xml_schema_pillegal_attr_err(
                                    self,
                                    XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                                    null_mut(),
                                    cur_attr,
                                );
                            }
                        } else if cur_attr.name.as_ref() != "source" {
                            xml_schema_pillegal_attr_err(
                                self,
                                XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                                null_mut(),
                                cur_attr,
                            );
                        }
                        attr = cur_attr.next;
                    }
                    // Attribute "xml:lang".
                    if let Some(attr) =
                        xml_schema_get_prop_node_ns(cur_node, XML_XML_NAMESPACE, "lang")
                    {
                        xml_schema_pval_attr_node(
                            self,
                            null_mut(),
                            attr,
                            xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasLanguage),
                            null_mut(),
                        );
                    }
                    child = cur_node
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                } else {
                    if barked == 0 {
                        xml_schema_pcontent_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                            null_mut(),
                            node,
                            Some(cur_node.into()),
                            None,
                            Some("(appinfo | documentation)*"),
                        );
                    }
                    barked = 1;
                    child = cur_node
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                }
            }

            ret
        }
    }

    /// Parse a XML schema Import definition
    /// *WARNING* this interface is highly subject to change
    ///
    /// Returns 0 in case of success, a positive error code if
    /// not valid and -1 in case of an internal error.
    #[doc(alias = "xmlSchemaParseImport")]
    unsafe fn parse_import(&mut self, schema: XmlSchemaPtr, node: XmlNodePtr) -> i32 {
        unsafe {
            let mut namespace_name: *const u8 = null();
            let mut schema_location: *const u8 = null();
            let mut ret: i32;
            let mut bucket: XmlSchemaBucketPtr = null_mut();

            if schema.is_null() {
                return -1;
            }

            // Check for illegal attributes.
            let mut attr = node.properties;
            while let Some(cur_attr) = attr {
                if let Some(ns) = cur_attr.ns {
                    if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                        xml_schema_pillegal_attr_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                            null_mut(),
                            cur_attr,
                        );
                    }
                } else if cur_attr.name.as_ref() != "id"
                    && cur_attr.name.as_ref() != "namespace"
                    && cur_attr.name.as_ref() != "schemaLocation"
                {
                    xml_schema_pillegal_attr_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
                attr = cur_attr.next;
            }
            // Extract and validate attributes.
            if xml_schema_pval_attr(
                self,
                null_mut(),
                node,
                "namespace",
                xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasAnyURI),
                &raw mut namespace_name,
            ) != 0
            {
                let namespace_name = CStr::from_ptr(namespace_name as *const i8).to_string_lossy();
                xml_schema_psimple_type_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sAttrInvalidValue,
                    null_mut(),
                    node.into(),
                    xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasAnyURI),
                    None,
                    Some(&namespace_name),
                    None,
                    None,
                    None,
                );
                return self.err;
            }

            if xml_schema_pval_attr(
                self,
                null_mut(),
                node,
                "schemaLocation",
                xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasAnyURI),
                &raw mut schema_location,
            ) != 0
            {
                let schema_location =
                    CStr::from_ptr(schema_location as *const i8).to_string_lossy();
                xml_schema_psimple_type_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sAttrInvalidValue,
                    null_mut(),
                    node.into(),
                    xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasAnyURI),
                    None,
                    Some(&schema_location),
                    None,
                    None,
                    None,
                );
                return self.err;
            }
            // And now for the children...
            let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
            if is_schema(child, "annotation") {
                // the annotation here is simply discarded ...
                // TODO: really?
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if let Some(child) = child {
                xml_schema_pcontent_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                    null_mut(),
                    node,
                    Some(child.into()),
                    None,
                    Some("(annotation?)"),
                );
            }
            // Apply additional constraints.
            //
            // Note that it is important to use the original @target_namespace
            // (or none at all), to rule out imports of schemas _with_ a
            // @target_namespace if the importing schema is a chameleon schema
            // (with no @target_namespace).
            let this_target_namespace: *const u8 =
                (*(*self.constructor).bucket).orig_target_namespace;
            if !namespace_name.is_null() {
                // 1.1 If the namespace [attribute] is present, then its `actual value`
                // must not match the `actual value` of the enclosing <schema>'s
                // target_namespace [attribute].
                if xml_str_equal(this_target_namespace, namespace_name) {
                    let this_target_namespace =
                        CStr::from_ptr(this_target_namespace as *const i8).to_string_lossy();
                    xml_schema_pcustom_err(
                        self,
                        XmlParserErrors::XmlSchemapSrcImport1_1,
                        null_mut(),
                        Some(node.into()),
                        format!("The value of the attribute 'namespace' must not match the target namespace '{this_target_namespace}' of the importing schema").as_str(),
                        Some(&this_target_namespace)
                    );
                    return self.err;
                }
            } else {
                // 1.2 If the namespace [attribute] is not present, then the enclosing
                // <schema> must have a target_namespace [attribute].
                if this_target_namespace.is_null() {
                    xml_schema_pcustom_err(
                        self,
                        XmlParserErrors::XmlSchemapSrcImport1_2,
                        null_mut(),
                        Some(node.into()),
                        "The attribute 'namespace' must be existent if the importing schema has no target namespace",
                        None,
                    );
                    return self.err;
                }
            }
            // Locate and acquire the schema document.
            if !schema_location.is_null() {
                schema_location =
                    xml_schema_build_absolute_uri(self.dict, schema_location, Some(node.into()));
            }
            ret = xml_schema_add_schema_doc(
                self,
                XML_SCHEMA_SCHEMA_IMPORT,
                schema_location,
                None,
                null_mut(),
                0,
                Some(node.into()),
                this_target_namespace,
                namespace_name,
                &raw mut bucket,
            );

            if ret != 0 {
                return ret;
            }

            // For <import>: "It is *not* an error for the application
            // schema reference strategy to fail."
            // So just don't parse if no schema document was found.
            // Note that we will get no bucket if the schema could not be
            // located or if there was no schemaLocation.
            if bucket.is_null() && !schema_location.is_null() {
                let schema_location =
                    CStr::from_ptr(schema_location as *const i8).to_string_lossy();
                xml_schema_custom_warning(
                self as *mut Self as XmlSchemaAbstractCtxtPtr,
                XmlParserErrors::XmlSchemapWarnUnlocatedSchema,
                Some(node.into()),
                null_mut(),
                format!(
                    "Failed to locate a schema at location '{schema_location}'. Skipping the import"
                )
                .as_str(),
                Some(&schema_location),
                None,
                None,
            );
            }

            if !bucket.is_null() && can_parse_schema(bucket) {
                ret = self.parse_new_doc(schema, bucket);
            }

            ret
        }
    }

    #[doc(alias = "xmlSchemaParseIncludeOrRedefine")]
    unsafe fn parse_include_or_redefine(
        &mut self,
        schema: XmlSchemaPtr,
        node: XmlNodePtr,
        typ: i32,
    ) -> i32 {
        unsafe {
            let mut schema_location: *const u8 = null();
            let mut res: i32; /* hasRedefinitions = 0 */
            let mut is_chameleon: i32 = 0;
            let mut was_chameleon: i32 = 0;
            let mut bucket: XmlSchemaBucketPtr = null_mut();

            if schema.is_null() {
                return -1;
            }

            // Parse attributes. Note that the returned schemaLocation will
            // be already converted to an absolute URI.
            res = self.parse_include_or_redefine_attrs(
                schema,
                node,
                &raw mut schema_location as _,
                typ,
            );
            if res != 0 {
                return res;
            }
            // Load and add the schema document.
            res = xml_schema_add_schema_doc(
                self,
                typ,
                schema_location,
                None,
                null_mut(),
                0,
                Some(node.into()),
                self.target_namespace,
                null_mut(),
                &raw mut bucket,
            );
            if res != 0 {
                return res;
            }
            // If we get no schema bucket back, then this means that the schema
            // document could not be located or was broken XML or was not
            // a schema document.
            if bucket.is_null() || (*bucket).doc.is_none() {
                if typ == XML_SCHEMA_SCHEMA_INCLUDE {
                    // WARNING for <include>:
                    // We will raise an error if the schema cannot be located
                    // for inclusions, since the that was the feedback from the
                    // schema people. I.e. the following spec piece will *not* be
                    // satisfied:
                    // SPEC src-include: "It is not an error for the `actual value` of the
                    // schemaLocation [attribute] to fail to resolve it all, in which
                    // case no corresponding inclusion is performed.
                    // So do we need a warning report here?"
                    res = XmlParserErrors::XmlSchemapSrcInclude as i32;
                    let schema_location =
                        CStr::from_ptr(schema_location as *const i8).to_string_lossy();
                    xml_schema_custom_err(
                        self as *mut Self as XmlSchemaAbstractCtxtPtr,
                        res.try_into().unwrap(),
                        Some(node.into()),
                        null_mut(),
                        format!("Failed to load the document '{schema_location}' for inclusion")
                            .as_str(),
                        Some(&schema_location),
                        None,
                    );
                } else {
                    // NOTE: This was changed to raise an error even if no redefinitions
                    // are specified.
                    //
                    // SPEC src-redefine (1)
                    // "If there are any element information items among the [children]
                    // other than <annotation> then the `actual value` of the
                    // schemaLocation [attribute] must successfully resolve."
                    // TODO: Ask the WG if a the location has always to resolve
                    // here as well!
                    res = XmlParserErrors::XmlSchemapSrcRedefine as i32;
                    let schema_location =
                        CStr::from_ptr(schema_location as *const i8).to_string_lossy();
                    xml_schema_custom_err(
                        self as *mut Self as XmlSchemaAbstractCtxtPtr,
                        res.try_into().unwrap(),
                        Some(node.into()),
                        null_mut(),
                        format!("Failed to load the document '{schema_location}' for redefinition")
                            .as_str(),
                        Some(&schema_location),
                        None,
                    );
                }
            } else {
                // Check target_namespace sanity before parsing the new schema.
                // TODO: Note that we won't check further content if the
                // target_namespace was bad.
                if !(*bucket).orig_target_namespace.is_null() {
                    // SPEC src-include (2.1)
                    // "SII has a target_namespace [attribute], and its `actual
                    // value` is identical to the `actual value` of the target_namespace
                    // [attribute] of SII' (which must have such an [attribute])."
                    if self.target_namespace.is_null() {
                        let schema_location =
                            CStr::from_ptr(schema_location as *const i8).to_string_lossy();
                        xml_schema_custom_err(
                            self as *mut Self as XmlSchemaAbstractCtxtPtr,
                            XmlParserErrors::XmlSchemapSrcInclude,
                            Some(node.into()),
                            null_mut(),
                            format!("The target namespace of the included/redefined schema '{schema_location}' has to be absent, since the including/redefining schema has no target namespace").as_str(),
                            Some(&schema_location),
                            None
                        );
                        // goto exit_error;
                        return self.err;
                    } else if !xml_str_equal((*bucket).orig_target_namespace, self.target_namespace)
                    {
                        let bkt_ns = CStr::from_ptr((*bucket).orig_target_namespace as *const i8)
                            .to_string_lossy();
                        let schema_location =
                            CStr::from_ptr(schema_location as *const i8).to_string_lossy();
                        let ctxt_ns =
                            CStr::from_ptr(self.target_namespace as *const i8).to_string_lossy();
                        // TODO: Change error function.
                        xml_schema_pcustom_err_ext(
                            self,
                            XmlParserErrors::XmlSchemapSrcInclude,
                            null_mut(),
                            Some(node.into()),
                            format!("The target namespace '{bkt_ns}' of the included/redefined schema '{schema_location}' differs from '{ctxt_ns}' of the including/redefining schema").as_str(),
                            Some(&bkt_ns),
                            Some(&schema_location),
                            Some(&ctxt_ns)
                        );
                        // goto exit_error;
                        return self.err;
                    }
                } else if !self.target_namespace.is_null() {
                    // Chameleons: the original target namespace will
                    // differ from the resulting namespace.
                    is_chameleon = 1;
                    (*bucket).target_namespace = self.target_namespace;
                }
            }
            // Parse the schema.
            if !bucket.is_null() && (*bucket).parsed == 0 && (*bucket).doc.is_some() {
                if is_chameleon != 0 {
                    // TODO: Get rid of this flag on the schema itself.
                    if (*schema).flags & XML_SCHEMAS_INCLUDING_CONVERT_NS == 0 {
                        (*schema).flags |= XML_SCHEMAS_INCLUDING_CONVERT_NS;
                    } else {
                        was_chameleon = 1;
                    }
                }
                self.parse_new_doc(schema, bucket);
                // Restore chameleon flag.
                if is_chameleon != 0 && was_chameleon == 0 {
                    (*schema).flags ^= XML_SCHEMAS_INCLUDING_CONVERT_NS;
                }
            }
            // And now for the children...
            let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
            if typ == XML_SCHEMA_SCHEMA_REDEFINE {
                // Parse (simpleType | complexType | group | attributeGroup))*
                self.redefined = bucket;
                // How to proceed if the redefined schema was not located?
                self.is_redefine = 1;
                while is_schema(child, "annotation")
                    || is_schema(child, "simpleType")
                    || is_schema(child, "complexType")
                    || is_schema(child, "group")
                    || is_schema(child, "attributeGroup")
                {
                    if is_schema(child, "annotation") {
                        // TODO: discard or not?
                    } else if is_schema(child, "simpleType") {
                        self.parse_simple_type(schema, child.unwrap(), 1);
                    } else if is_schema(child, "complexType") {
                        self.parse_complex_type(schema, child.unwrap(), 1);
                    // hasRedefinitions = 1;
                    } else if is_schema(child, "group") {
                        // hasRedefinitions = 1;
                        self.parse_model_group_definition(schema, child.unwrap());
                    } else if is_schema(child, "attributeGroup") {
                        // hasRedefinitions = 1;
                        self.parse_attribute_group_definition(schema, child.unwrap());
                    }
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                }
                self.redefined = null_mut();
                self.is_redefine = 0;
            } else if is_schema(child, "annotation") {
                // TODO: discard or not?
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if let Some(child) = child {
                res = XmlParserErrors::XmlSchemapS4sElemNotAllowed as i32;
                if typ == XML_SCHEMA_SCHEMA_REDEFINE {
                    xml_schema_pcontent_err(
                        self,
                        res.try_into().unwrap(),
                        null_mut(),
                        node,
                        Some(child.into()),
                        None,
                        Some("(annotation | (simpleType | complexType | group | attributeGroup))*"),
                    );
                } else {
                    xml_schema_pcontent_err(
                        self,
                        res.try_into().unwrap(),
                        null_mut(),
                        node,
                        Some(child.into()),
                        None,
                        Some("(annotation?)"),
                    );
                }
            }
            res
        }
    }

    #[doc(alias = "xmlSchemaParseIncludeOrRedefineAttrs")]
    unsafe fn parse_include_or_redefine_attrs(
        &mut self,
        schema: XmlSchemaPtr,
        node: XmlNodePtr,
        schema_location: *mut *mut u8,
        typ: i32,
    ) -> i32 {
        unsafe {
            if schema.is_null() || schema_location.is_null() {
                return -1;
            }

            *schema_location = null_mut();
            // Check for illegal attributes.
            // Applies for both <include> and <redefine>.
            let mut attr = node.properties;
            while let Some(cur_attr) = attr {
                if let Some(ns) = cur_attr.ns {
                    if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                        xml_schema_pillegal_attr_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                            null_mut(),
                            cur_attr,
                        );
                    }
                } else if cur_attr.name.as_ref() != "id"
                    && cur_attr.name.as_ref() != "schemaLocation"
                {
                    xml_schema_pillegal_attr_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
                attr = cur_attr.next;
            }
            xml_schema_pval_attr_id(self, node, "id");
            // Preliminary step, extract the URI-Reference and make an URI from the base.
            // Attribute "schemaLocation" is mandatory.
            if let Some(attr) = xml_schema_get_prop_node(node, "schemaLocation") {
                if xml_schema_pval_attr_node(
                    self,
                    null_mut(),
                    attr,
                    xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasAnyURI),
                    schema_location as _,
                ) != 0
                {
                    // goto exit_error;
                    return self.err;
                }
                let uri = if let Some(base) = node.get_base(node.doc) {
                    (!(*schema_location).is_null())
                        .then(|| {
                            let schema_location =
                                CStr::from_ptr(*schema_location as *const i8).to_string_lossy();
                            build_uri(&schema_location, &base)
                        })
                        .flatten()
                } else {
                    (!(*schema_location).is_null())
                        .then(|| {
                            let schema_location =
                                CStr::from_ptr(*schema_location as *const i8).to_string_lossy();
                            node.doc.as_deref().and_then(|doc| {
                                doc.url
                                    .as_deref()
                                    .and_then(|base| build_uri(&schema_location, base))
                            })
                        })
                        .flatten()
                };
                let Some(uri) = uri else {
                    xml_schema_internal_err(
                        self as *mut Self as XmlSchemaAbstractCtxtPtr,
                        "xmlSchemaParseIncludeOrRedefine",
                        "could not build an URI from the schemaLocation",
                    );
                    // goto exit_failure;
                    return -1;
                };
                let uri = CString::new(uri).unwrap();
                *schema_location = xml_dict_lookup(self.dict, uri.as_ptr() as *const u8, -1) as _;
            } else {
                xml_schema_pmissing_attr_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sAttrMissing,
                    null_mut(),
                    Some(node.into()),
                    Some("schemaLocation"),
                    None,
                );
                // goto exit_error;
                return self.err;
            }
            // Report self-inclusion and self-redefinition.
            if (!(*schema_location).is_null())
                .then(|| CStr::from_ptr(*schema_location as *const i8).to_string_lossy())
                .as_deref()
                == self.url.as_deref()
            {
                let schema_location =
                    CStr::from_ptr(*schema_location as *const i8).to_string_lossy();
                if typ == XML_SCHEMA_SCHEMA_REDEFINE {
                    xml_schema_pcustom_err(
                        self,
                        XmlParserErrors::XmlSchemapSrcRedefine,
                        null_mut(),
                        Some(node.into()),
                        format!("The schema document '{schema_location}' cannot redefine itself.")
                            .as_str(),
                        Some(&schema_location),
                    );
                } else {
                    xml_schema_pcustom_err(
                        self,
                        XmlParserErrors::XmlSchemapSrcInclude,
                        null_mut(),
                        Some(node.into()),
                        format!("The schema document '{schema_location}' cannot include itself.")
                            .as_str(),
                        Some(&schema_location),
                    );
                }
                // goto exit_error;
                return self.err;
            }

            0
        }
    }

    #[doc(alias = "xmlSchemaParseInclude")]
    unsafe fn parse_include(&mut self, schema: XmlSchemaPtr, node: XmlNodePtr) -> i32 {
        unsafe { self.parse_include_or_redefine(schema, node, XML_SCHEMA_SCHEMA_INCLUDE) }
    }

    #[doc(alias = "xmlSchemaParseRedefine")]
    unsafe fn parse_redefine(&mut self, schema: XmlSchemaPtr, node: XmlNodePtr) -> i32 {
        unsafe { self.parse_include_or_redefine(schema, node, XML_SCHEMA_SCHEMA_REDEFINE) }
    }

    /// Parse a XML schema Complex Type definition
    /// *WARNING* this interface is highly subject to change
    ///
    /// Returns the type definition or NULL in case of error
    #[doc(alias = "xmlSchemaParseComplexType")]
    unsafe fn parse_complex_type(
        &mut self,
        schema: XmlSchemaPtr,
        node: XmlNodePtr,
        top_level: i32,
    ) -> XmlSchemaTypePtr {
        unsafe {
            let typ: XmlSchemaTypePtr;
            let mut name: *const u8 = null();
            // #ifdef ENABLE_NAMED_LOCALS
            //     let buf: [c_char; 40];
            // #endif
            let mut is_final: i32 = 0;
            let mut block: i32 = 0;
            let mut has_restriction_or_extension: i32 = 0;

            if schema.is_null() {
                return null_mut();
            }

            let ctxt_type: XmlSchemaTypePtr = self.ctxt_type;

            if top_level != 0 {
                let Some(attr) = xml_schema_get_prop_node(node, "name") else {
                    xml_schema_pmissing_attr_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sAttrMissing,
                        null_mut(),
                        Some(node.into()),
                        Some("name"),
                        None,
                    );
                    return null_mut();
                };
                if xml_schema_pval_attr_node(
                    self,
                    null_mut(),
                    attr,
                    xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasNCName),
                    &raw mut name,
                ) != 0
                {
                    return null_mut();
                }
            }

            if top_level == 0 {
                // Parse as local complex type definition.
                // #ifdef ENABLE_NAMED_LOCALS
                //     snprintf(buf, 39, "#CT%d".as_ptr() as _, self.counter++ + 1);
                //     typ = xmlSchemaAddType(self, schema, XmlSchemaTypeType::XmlSchemaTypeComplex, xmlDictLookup(self.dict, buf, -1), self.target_namespace, node, 0);
                // #else
                typ = xml_schema_add_type(
                    self,
                    schema,
                    XmlSchemaTypeType::XmlSchemaTypeComplex,
                    null_mut(),
                    self.target_namespace,
                    node.into(),
                    0,
                );
                // #endif
                if typ.is_null() {
                    return null_mut();
                }
                // name = (*typ).name;
                (*typ).node = node.into();
                (*typ).typ = XmlSchemaTypeType::XmlSchemaTypeComplex;
            // TODO: We need the target namespace.
            } else {
                // Parse as global complex type definition.
                typ = xml_schema_add_type(
                    self,
                    schema,
                    XmlSchemaTypeType::XmlSchemaTypeComplex,
                    name,
                    self.target_namespace,
                    node.into(),
                    1,
                );
                if typ.is_null() {
                    return null_mut();
                }
                (*typ).node = node.into();
                (*typ).typ = XmlSchemaTypeType::XmlSchemaTypeComplex;
                (*typ).flags |= XML_SCHEMAS_TYPE_GLOBAL;
            }
            (*typ).target_namespace = self.target_namespace;
            // Handle attributes.
            let mut attr = node.properties;
            while let Some(cur_attr) = attr {
                if let Some(ns) = cur_attr.ns {
                    if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                        xml_schema_pillegal_attr_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                            null_mut(),
                            cur_attr,
                        );
                    }
                } else if cur_attr.name.as_ref() == "id" {
                    // Attribute "id".
                    xml_schema_pval_attr_id(self, node, "id");
                } else if cur_attr.name.as_ref() == "mixed" {
                    // Attribute "mixed".
                    if xml_schema_pget_bool_node_value(self, null_mut(), cur_attr.into()) != 0 {
                        (*typ).flags |= XML_SCHEMAS_TYPE_MIXED;
                    }
                } else if top_level != 0 {
                    // Attributes of global complex type definitions.
                    if cur_attr.name.as_ref() == "name" {
                        // Pass.
                    } else if cur_attr.name.as_ref() == "abstract" {
                        // Attribute "abstract".
                        if xml_schema_pget_bool_node_value(self, null_mut(), cur_attr.into()) != 0 {
                            (*typ).flags |= XML_SCHEMAS_TYPE_ABSTRACT;
                        }
                    } else if cur_attr.name.as_ref() == "final" {
                        // Attribute "final".
                        let attr_value = self.get_node_content(Some(cur_attr.into()));
                        let cattr_value = xml_dict_lookup(
                            self.dict,
                            attr_value.as_ptr(),
                            attr_value.len() as i32,
                        );
                        if xml_schema_pval_attr_block_final(
                            cattr_value,
                            &raw mut (*typ).flags,
                            -1,
                            XML_SCHEMAS_TYPE_FINAL_EXTENSION,
                            XML_SCHEMAS_TYPE_FINAL_RESTRICTION,
                            -1,
                            -1,
                            -1,
                        ) != 0
                        {
                            xml_schema_psimple_type_err(
                                self,
                                XmlParserErrors::XmlSchemapS4sAttrInvalidValue,
                                null_mut(),
                                cur_attr.into(),
                                null_mut(),
                                Some("(#all | List of (extension | restriction))"),
                                Some(&attr_value),
                                None,
                                None,
                                None,
                            );
                        } else {
                            is_final = 1;
                        }
                    } else if cur_attr.name.as_ref() == "block" {
                        // Attribute "block".
                        let attr_value = self.get_node_content(Some(cur_attr.into()));
                        let cattr_value = xml_dict_lookup(
                            self.dict,
                            attr_value.as_ptr(),
                            attr_value.len() as i32,
                        );
                        if xml_schema_pval_attr_block_final(
                            cattr_value,
                            &raw mut (*typ).flags,
                            -1,
                            XML_SCHEMAS_TYPE_BLOCK_EXTENSION,
                            XML_SCHEMAS_TYPE_BLOCK_RESTRICTION,
                            -1,
                            -1,
                            -1,
                        ) != 0
                        {
                            xml_schema_psimple_type_err(
                                self,
                                XmlParserErrors::XmlSchemapS4sAttrInvalidValue,
                                null_mut(),
                                cur_attr.into(),
                                null_mut(),
                                Some("(#all | List of (extension | restriction)) "),
                                Some(&attr_value),
                                None,
                                None,
                                None,
                            );
                        } else {
                            block = 1;
                        }
                    } else {
                        xml_schema_pillegal_attr_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                            null_mut(),
                            cur_attr,
                        );
                    }
                } else {
                    xml_schema_pillegal_attr_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }

                attr = cur_attr.next;
            }
            if block == 0 {
                // Apply default "block" values.
                if (*schema).flags & XML_SCHEMAS_BLOCK_DEFAULT_RESTRICTION != 0 {
                    (*typ).flags |= XML_SCHEMAS_TYPE_BLOCK_RESTRICTION;
                }
                if (*schema).flags & XML_SCHEMAS_BLOCK_DEFAULT_EXTENSION != 0 {
                    (*typ).flags |= XML_SCHEMAS_TYPE_BLOCK_EXTENSION;
                }
            }
            if is_final == 0 {
                // Apply default "block" values.
                if (*schema).flags & XML_SCHEMAS_FINAL_DEFAULT_RESTRICTION != 0 {
                    (*typ).flags |= XML_SCHEMAS_TYPE_FINAL_RESTRICTION;
                }
                if (*schema).flags & XML_SCHEMAS_FINAL_DEFAULT_EXTENSION != 0 {
                    (*typ).flags |= XML_SCHEMAS_TYPE_FINAL_EXTENSION;
                }
            }
            // And now for the children...
            let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
            if is_schema(child, "annotation") {
                (*typ).annot = self.parse_annotation(child.unwrap(), 1);
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            self.ctxt_type = typ;
            if is_schema(child, "simpleContent") {
                // <complexType><simpleContent>...
                // 3.4.3 : 2.2
                // Specifying mixed='true' when the <simpleContent>
                // alternative is chosen has no effect
                if (*typ).flags & XML_SCHEMAS_TYPE_MIXED != 0 {
                    (*typ).flags ^= XML_SCHEMAS_TYPE_MIXED;
                }
                self.parse_simple_content(
                    schema,
                    child.unwrap(),
                    &raw mut has_restriction_or_extension,
                );
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            } else if is_schema(child, "complexContent") {
                // <complexType><complexContent>...
                (*typ).content_type = XmlSchemaContentType::XmlSchemaContentEmpty;
                self.parse_complex_content(
                    schema,
                    child.unwrap(),
                    &raw mut has_restriction_or_extension,
                );
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            } else {
                // E.g <complexType><sequence>... or <complexType><attribute>... etc.
                //
                // SPEC
                // "...the third alternative (neither <simpleContent> nor
                // <complexContent>) is chosen. This case is understood as shorthand
                // for complex content restricting the `ur-type definition`, and the
                // details of the mappings should be modified as necessary.
                (*typ).base_type =
                    xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasAnytype);
                (*typ).flags |= XML_SCHEMAS_TYPE_DERIVATION_METHOD_RESTRICTION;
                // Parse model groups.
                if is_schema(child, "all") {
                    (*typ).subtypes = self.parse_model_group(
                        schema,
                        child.unwrap(),
                        XmlSchemaTypeType::XmlSchemaTypeAll,
                        1,
                    ) as XmlSchemaTypePtr;
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                } else if is_schema(child, "choice") {
                    (*typ).subtypes = self.parse_model_group(
                        schema,
                        child.unwrap(),
                        XmlSchemaTypeType::XmlSchemaTypeChoice,
                        1,
                    ) as XmlSchemaTypePtr;
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                } else if is_schema(child, "sequence") {
                    (*typ).subtypes = self.parse_model_group(
                        schema,
                        child.unwrap(),
                        XmlSchemaTypeType::XmlSchemaTypeSequence,
                        1,
                    ) as XmlSchemaTypePtr;
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                } else if is_schema(child, "group") {
                    (*typ).subtypes =
                        self.parse_model_group_def_ref(schema, child.unwrap()) as XmlSchemaTypePtr;
                    // Note that the reference will be resolved in
                    // xmlSchemaResolveTypeReferences();
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                }
                // Parse attribute decls/refs.
                if self.parse_local_attributes(
                    schema,
                    &mut child,
                    &raw mut (*typ).attr_uses as *mut XmlSchemaItemListPtr<*mut c_void>,
                    XmlSchemaTypeType::XmlSchemaTypeRestriction as i32,
                    null_mut(),
                ) == -1
                {
                    return null_mut();
                }
                // Parse attribute wildcard.
                if is_schema(child, "anyAttribute") {
                    (*typ).attribute_wildcard = self.parse_any_attribute(schema, child.unwrap());
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                }
            }
            if let Some(child) = child {
                xml_schema_pcontent_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                    null_mut(),
                    node,
                    Some(child.into()),
                    None,
                    Some(
                        "(annotation?, (simpleContent | complexContent | ((group | all | choice | sequence)?, ((attribute | attributeGroup)*, anyAttribute?))))",
                    ),
                );
            }
            // REDEFINE: SPEC src-redefine (5)
            if top_level != 0 && self.is_redefine != 0 && has_restriction_or_extension == 0 {
                xml_schema_pcustom_err(
                    self,
                    XmlParserErrors::XmlSchemapSrcRedefine,
                    null_mut(),
                    Some(node.into()),
                    "This is a redefinition, thus the <complexType> must have a <restriction> or <extension> grand-child",
                    None,
                );
            }
            self.ctxt_type = ctxt_type;
            typ
        }
    }

    /// Parse a XML schema Simple Type definition
    /// *WARNING* this interface is highly subject to change
    ///
    /// Returns -1 in case of error, 0 if the declaration is improper and
    /// 1 in case of success.
    #[doc(alias = "xmlSchemaParseSimpleType")]
    unsafe fn parse_simple_type(
        &mut self,
        schema: XmlSchemaPtr,
        node: XmlNodePtr,
        top_level: i32,
    ) -> XmlSchemaTypePtr {
        unsafe {
            let typ: XmlSchemaTypePtr;
            let mut attr_value: *const u8 = null();
            let mut has_restriction: i32 = 0;

            if schema.is_null() {
                return null_mut();
            }

            if top_level != 0 {
                let Some(attr) = xml_schema_get_prop_node(node, "name") else {
                    xml_schema_pmissing_attr_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sAttrMissing,
                        null_mut(),
                        Some(node.into()),
                        Some("name"),
                        None,
                    );
                    return null_mut();
                };
                if xml_schema_pval_attr_node(
                    self,
                    null_mut(),
                    attr,
                    xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasNCName),
                    &raw mut attr_value,
                ) != 0
                {
                    return null_mut();
                }
                // Skip built-in types.
                if self.is_s4s != 0 {
                    if self.is_redefine != 0 {
                        // REDEFINE: Disallow redefinition of built-in-types.
                        // TODO: It seems that the spec does not say anything
                        // about this case.
                        xml_schema_pcustom_err(
                            self,
                            XmlParserErrors::XmlSchemapSrcRedefine,
                            null_mut(),
                            Some(node.into()),
                            "Redefinition of built-in simple types is not supported",
                            None,
                        );
                        return null_mut();
                    }
                    let bi_type: XmlSchemaTypePtr = xml_schema_get_predefined_type(
                        CStr::from_ptr(attr_value as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                        XML_SCHEMA_NS.to_str().unwrap(),
                    );
                    if !bi_type.is_null() {
                        return bi_type;
                    }
                }
            }
            // TargetNamespace:
            // SPEC "The `actual value` of the target_namespace [attribute]
            // of the <schema> ancestor element information item if present,
            // otherwise `absent`.
            if top_level == 0 {
                // #ifdef ENABLE_NAMED_LOCALS
                //         let buf: [c_char; 40];
                // #endif
                // Parse as local simple type definition.
                // #ifdef ENABLE_NAMED_LOCALS
                //         snprintf(buf, 39, "#ST%d".as_ptr() as _, self.counter++ + 1);
                //     	typ = xmlSchemaAddType(self, schema, XmlSchemaTypeType::XmlSchemaTypeSimple, xmlDictLookup(self.dict, buf, -1), self.target_namespace, node, 0);
                // #else
                typ = xml_schema_add_type(
                    self,
                    schema,
                    XmlSchemaTypeType::XmlSchemaTypeSimple,
                    null_mut(),
                    self.target_namespace,
                    node.into(),
                    0,
                );
                // #endif
                if typ.is_null() {
                    return null_mut();
                }
                (*typ).typ = XmlSchemaTypeType::XmlSchemaTypeSimple;
                (*typ).content_type = XmlSchemaContentType::XmlSchemaContentSimple;
                // Check for illegal attributes.
                let mut attr = node.properties;
                while let Some(cur_attr) = attr {
                    if let Some(ns) = cur_attr.ns {
                        if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                            xml_schema_pillegal_attr_err(
                                self,
                                XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                                null_mut(),
                                cur_attr,
                            );
                        }
                    } else if cur_attr.name.as_ref() != "id" {
                        xml_schema_pillegal_attr_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                            null_mut(),
                            cur_attr,
                        );
                    }
                    attr = cur_attr.next;
                }
            } else {
                // Parse as global simple type definition.
                //
                // Note that attrValue is the value of the attribute "name" here.
                typ = xml_schema_add_type(
                    self,
                    schema,
                    XmlSchemaTypeType::XmlSchemaTypeSimple,
                    attr_value,
                    self.target_namespace,
                    node.into(),
                    1,
                );
                if typ.is_null() {
                    return null_mut();
                }
                (*typ).typ = XmlSchemaTypeType::XmlSchemaTypeSimple;
                (*typ).content_type = XmlSchemaContentType::XmlSchemaContentSimple;
                (*typ).flags |= XML_SCHEMAS_TYPE_GLOBAL;
                // Check for illegal attributes.
                let mut attr = node.properties;
                while let Some(cur_attr) = attr {
                    if let Some(ns) = cur_attr.ns {
                        if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                            xml_schema_pillegal_attr_err(
                                self,
                                XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                                null_mut(),
                                cur_attr,
                            );
                        }
                    } else if cur_attr.name.as_ref() != "id"
                        && cur_attr.name.as_ref() != "name"
                        && cur_attr.name.as_ref() != "final"
                    {
                        xml_schema_pillegal_attr_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                            null_mut(),
                            cur_attr,
                        );
                    }
                    attr = cur_attr.next;
                }
                // Attribute "final".
                if let Some(attr) = xml_schema_get_prop_node(node, "final") {
                    let attr_value = self.get_prop(node, "final");
                    let cattr_value = attr_value.as_deref().map(|s| CString::new(s).unwrap());
                    if xml_schema_pval_attr_block_final(
                        cattr_value
                            .as_deref()
                            .map_or(null_mut(), |s| s.as_ptr() as *const u8),
                        &raw mut (*typ).flags,
                        -1,
                        -1,
                        XML_SCHEMAS_TYPE_FINAL_RESTRICTION,
                        -1,
                        XML_SCHEMAS_TYPE_FINAL_LIST,
                        XML_SCHEMAS_TYPE_FINAL_UNION,
                    ) != 0
                    {
                        let attr_value = attr_value.unwrap();
                        xml_schema_psimple_type_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sAttrInvalidValue,
                            typ as XmlSchemaBasicItemPtr,
                            attr.into(),
                            null_mut(),
                            Some("(#all | List of (list | union | restriction)"),
                            Some(&attr_value),
                            None,
                            None,
                            None,
                        );
                    }
                } else {
                    if (*schema).flags & XML_SCHEMAS_FINAL_DEFAULT_RESTRICTION != 0 {
                        (*typ).flags |= XML_SCHEMAS_TYPE_FINAL_RESTRICTION;
                    }
                    if (*schema).flags & XML_SCHEMAS_FINAL_DEFAULT_LIST != 0 {
                        (*typ).flags |= XML_SCHEMAS_TYPE_FINAL_LIST;
                    }
                    if (*schema).flags & XML_SCHEMAS_FINAL_DEFAULT_UNION != 0 {
                        (*typ).flags |= XML_SCHEMAS_TYPE_FINAL_UNION;
                    }
                }
            }
            (*typ).target_namespace = self.target_namespace;
            xml_schema_pval_attr_id(self, node, "id");
            // And now for the children...
            let old_ctxt_type: XmlSchemaTypePtr = self.ctxt_type;

            self.ctxt_type = typ;

            let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
            if is_schema(child, "annotation") {
                (*typ).annot = self.parse_annotation(child.unwrap(), 1);
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if child.is_none() {
                xml_schema_pcontent_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sElemMissing,
                    null_mut(),
                    node,
                    None,
                    None,
                    Some("(annotation?, (restriction | list | union))"),
                );
            } else if is_schema(child, "restriction") {
                self.parse_restriction(
                    schema,
                    child.unwrap(),
                    XmlSchemaTypeType::XmlSchemaTypeSimple,
                );
                has_restriction = 1;
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            } else if is_schema(child, "list") {
                self.parse_list(schema, child.unwrap());
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            } else if is_schema(child, "union") {
                self.parse_union(schema, child.unwrap());
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if let Some(child) = child {
                xml_schema_pcontent_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                    null_mut(),
                    node,
                    Some(child.into()),
                    None,
                    Some("(annotation?, (restriction | list | union))"),
                );
            }
            // REDEFINE: SPEC src-redefine (5)
            // "Within the [children], each <simpleType> must have a
            // <restriction> among its [children] ... the `actual value` of whose
            // base [attribute] must be the same as the `actual value` of its own
            // name attribute plus target namespace;"
            if top_level != 0 && self.is_redefine != 0 && has_restriction == 0 {
                xml_schema_pcustom_err(
                    self,
                    XmlParserErrors::XmlSchemapSrcRedefine,
                    null_mut(),
                    Some(node.into()),
                    "This is a redefinition, thus the <simpleType> must have a <restriction> child",
                    None,
                );
            }

            self.ctxt_type = old_ctxt_type;
            typ
        }
    }

    /// Parses a XML schema element declaration.
    /// *WARNING* this interface is highly subject to change
    ///
    /// Returns the element declaration or a particle; NULL in case
    /// of an error or if the particle has minOccurs==maxOccurs==0.
    #[doc(alias = "xmlSchemaParseElement")]
    unsafe fn parse_element(
        &mut self,
        schema: XmlSchemaPtr,
        node: XmlNodePtr,
        is_elem_ref: *mut i32,
        top_level: i32,
    ) -> XmlSchemaBasicItemPtr {
        unsafe {
            let mut decl: XmlSchemaElementPtr = null_mut();
            let mut particle: XmlSchemaParticlePtr = null_mut();
            let mut annot: XmlSchemaAnnotPtr = null_mut();
            let min: i32;
            let max: i32;
            let mut is_ref: i32 = 0;

            // 3.3.3 Constraints on XML Representations of Element Declarations
            // TODO: Complete implementation of 3.3.6

            if schema.is_null() {
                return null_mut();
            }

            if !is_elem_ref.is_null() {
                *is_elem_ref = 0;
            }
            // If we get a "ref" attribute on a local <element> we will assume it's
            // a reference - even if there's a "name" attribute; this seems to be more
            // robust.
            let name_attr = xml_schema_get_prop_node(node, "name");
            let attr = xml_schema_get_prop_node(node, "ref");
            if top_level != 0 || attr.is_none() {
                if name_attr.is_none() {
                    xml_schema_pmissing_attr_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sAttrMissing,
                        null_mut(),
                        Some(node.into()),
                        Some("name"),
                        None,
                    );
                    return null_mut();
                }
            } else {
                is_ref = 1;
            }

            xml_schema_pval_attr_id(self, node, "id");
            let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
            if is_schema(child, "annotation") {
                annot = self.parse_annotation(child.unwrap(), 1);
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            // Skip particle part if a global declaration.
            'return_null: {
                'declaration_part: {
                    if top_level != 0 {
                        break 'declaration_part;
                    }
                    // The particle part ==================================================
                    min = xml_get_min_occurs(self, node, 0, -1, 1, "xs:nonNegativeInteger");
                    max = xml_get_max_occurs(
                        self,
                        node,
                        0,
                        UNBOUNDED as i32,
                        1,
                        "(xs:nonNegativeInteger | unbounded)",
                    );
                    xml_schema_pcheck_particle_correct_2(self, null_mut(), node, min, max);
                    particle = xml_schema_add_particle(self, Some(node), min, max);
                    if particle.is_null() {
                        break 'return_null;
                    }

                    // (*ret).flags |= XML_SCHEMAS_ELEM_REF;

                    if is_ref != 0 {
                        let mut ref_ns: *const u8 = null();
                        let mut refe: *const u8 = null();

                        // The reference part =============================================
                        if !is_elem_ref.is_null() {
                            *is_elem_ref = 1;
                        }

                        xml_schema_pval_attr_node_qname(
                            self,
                            schema,
                            null_mut(),
                            attr.unwrap(),
                            &raw mut ref_ns,
                            &raw mut refe,
                        );
                        xml_schema_check_reference(self, schema, node, Some(attr.unwrap()), ref_ns);
                        // SPEC (3.3.3 : 2.1) "One of ref or name must be present, but not both"
                        if let Some(name_attr) = name_attr {
                            xml_schema_pmutual_excl_attr_err(
                                self,
                                XmlParserErrors::XmlSchemapSrcElement2_1,
                                null_mut(),
                                name_attr,
                                c"ref".as_ptr() as _,
                                c"name".as_ptr() as _,
                            );
                        }
                        // Check for illegal attributes.
                        let mut attr = node.properties;
                        while let Some(cur_attr) = attr {
                            if let Some(ns) = cur_attr.ns {
                                if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                                    xml_schema_pillegal_attr_err(
                                        self,
                                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                                        null_mut(),
                                        cur_attr,
                                    );
                                }
                            } else if cur_attr.name.as_ref() == "ref"
                                || cur_attr.name.as_ref() == "name"
                                || cur_attr.name.as_ref() == "id"
                                || cur_attr.name.as_ref() == "maxOccurs"
                                || cur_attr.name.as_ref() == "minOccurs"
                            {
                                attr = cur_attr.next;
                                continue;
                            } else {
                                // SPEC (3.3.3 : 2.2)
                                xml_schema_pcustom_attr_err(
                                    self,
                                    XmlParserErrors::XmlSchemapSrcElement2_2,
                                    null_mut(),
                                    null_mut(),
                                    Some(cur_attr),
                                    "Only the attributes 'minOccurs', 'maxOccurs' and 'id' are allowed in addition to 'ref'",
                                );
                                break;
                            }
                            attr = cur_attr.next;
                        }
                        // No children except <annotation> expected.
                        if let Some(child) = child {
                            xml_schema_pcontent_err(
                                self,
                                XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                                null_mut(),
                                node,
                                Some(child.into()),
                                None,
                                Some("(annotation?)"),
                            );
                        }
                        if min == 0 && max == 0 {
                            break 'return_null;
                        }
                        // Create the reference item and attach it to the particle.
                        let refer: XmlSchemaQNameRefPtr = xml_schema_new_qname_ref(
                            self,
                            XmlSchemaTypeType::XmlSchemaTypeElement,
                            refe,
                            ref_ns,
                        );
                        if refer.is_null() {
                            break 'return_null;
                        }
                        (*particle).children = refer as XmlSchemaTreeItemPtr;
                        (*particle).annot = annot;
                        // Add the particle to pending components, since the reference
                        // need to be resolved.
                        xml_schema_add_item_size(
                            &raw mut (*self.constructor).pending,
                            10,
                            particle as _,
                        );
                        return particle as XmlSchemaBasicItemPtr;
                    }
                }
                // The declaration part ===============================================
                // declaration_part:
                let mut ns: *const u8 = null();
                let mut name: *const u8 = null();
                let mut cur_idc: XmlSchemaIDCPtr = null_mut();
                let mut last_idc: XmlSchemaIDCPtr = null_mut();

                if xml_schema_pval_attr_node(
                    self,
                    null_mut(),
                    // Is this `unwrap` OK ???
                    name_attr.unwrap(),
                    xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasNCName),
                    &raw mut name,
                ) != 0
                {
                    break 'return_null;
                }
                // Evaluate the target namespace.
                if top_level != 0 {
                    ns = self.target_namespace;
                } else if let Some(attr) = xml_schema_get_prop_node(node, "form") {
                    let attr_value = self.get_node_content(Some(attr.into()));
                    if attr_value == "qualified" {
                        ns = self.target_namespace;
                    } else if attr_value != "unqualified" {
                        xml_schema_psimple_type_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sAttrInvalidValue,
                            null_mut(),
                            attr.into(),
                            null_mut(),
                            Some("(qualified | unqualified)"),
                            Some(&attr_value),
                            None,
                            None,
                            None,
                        );
                    }
                } else if (*schema).flags & XML_SCHEMAS_QUALIF_ELEM != 0 {
                    ns = self.target_namespace;
                }
                decl = xml_schema_add_element(self, name, ns, node, top_level);
                if decl.is_null() {
                    break 'return_null;
                }

                // Check for illegal attributes.
                let mut attr = node.properties;
                while let Some(cur_attr) = attr {
                    if let Some(ns) = cur_attr.ns {
                        if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                            xml_schema_pillegal_attr_err(
                                self,
                                XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                                null_mut(),
                                cur_attr,
                            );
                        }
                    } else if cur_attr.name.as_ref() != "name"
                        && cur_attr.name.as_ref() != "type"
                        && cur_attr.name.as_ref() != "id"
                        && cur_attr.name.as_ref() != "default"
                        && cur_attr.name.as_ref() != "fixed"
                        && cur_attr.name.as_ref() != "block"
                        && cur_attr.name.as_ref() != "nillable"
                    {
                        if top_level == 0 {
                            if cur_attr.name.as_ref() != "maxOccurs"
                                && cur_attr.name.as_ref() != "minOccurs"
                                && cur_attr.name.as_ref() != "form"
                            {
                                xml_schema_pillegal_attr_err(
                                    self,
                                    XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                                    null_mut(),
                                    cur_attr,
                                );
                            }
                        } else if cur_attr.name.as_ref() != "final"
                            && cur_attr.name.as_ref() != "abstract"
                            && cur_attr.name.as_ref() != "substitutionGroup"
                        {
                            xml_schema_pillegal_attr_err(
                                self,
                                XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                                null_mut(),
                                cur_attr,
                            );
                        }
                    }
                    attr = cur_attr.next;
                }
                // Extract/validate attributes.
                if top_level != 0 {
                    // Process top attributes of global element declarations here.
                    (*decl).flags |= XML_SCHEMAS_ELEM_GLOBAL;
                    (*decl).flags |= XML_SCHEMAS_ELEM_TOPLEVEL;
                    xml_schema_pval_attr_qname(
                        self,
                        schema,
                        null_mut(),
                        node,
                        "substitutionGroup",
                        &raw mut (*decl).subst_group_ns,
                        &raw mut (*decl).subst_group,
                    );
                    if xml_get_boolean_prop(self, node, "abstract", 0) != 0 {
                        (*decl).flags |= XML_SCHEMAS_ELEM_ABSTRACT;
                    }
                    // Attribute "final".
                    if let Some(attr) = xml_schema_get_prop_node(node, "final") {
                        let attr_value = self.get_node_content(Some(attr.into()));
                        let cattr_value = xml_dict_lookup(
                            self.dict,
                            attr_value.as_ptr(),
                            attr_value.len() as i32,
                        );
                        if xml_schema_pval_attr_block_final(
                            cattr_value,
                            &raw mut (*decl).flags,
                            -1,
                            XML_SCHEMAS_ELEM_FINAL_EXTENSION,
                            XML_SCHEMAS_ELEM_FINAL_RESTRICTION,
                            -1,
                            -1,
                            -1,
                        ) != 0
                        {
                            xml_schema_psimple_type_err(
                                self,
                                XmlParserErrors::XmlSchemapS4sAttrInvalidValue,
                                null_mut(),
                                attr.into(),
                                null_mut(),
                                Some("(#all | List of (extension | restriction))"),
                                Some(&attr_value),
                                None,
                                None,
                                None,
                            );
                        }
                    } else {
                        if (*schema).flags & XML_SCHEMAS_FINAL_DEFAULT_EXTENSION != 0 {
                            (*decl).flags |= XML_SCHEMAS_ELEM_FINAL_EXTENSION;
                        }
                        if (*schema).flags & XML_SCHEMAS_FINAL_DEFAULT_RESTRICTION != 0 {
                            (*decl).flags |= XML_SCHEMAS_ELEM_FINAL_RESTRICTION;
                        }
                    }
                }
                // Attribute "block".
                if let Some(attr) = xml_schema_get_prop_node(node, "block") {
                    let attr_value = self.get_node_content(Some(attr.into()));
                    let cattr_value =
                        xml_dict_lookup(self.dict, attr_value.as_ptr(), attr_value.len() as i32);
                    if xml_schema_pval_attr_block_final(
                        cattr_value,
                        &raw mut (*decl).flags,
                        -1,
                        XML_SCHEMAS_ELEM_BLOCK_EXTENSION,
                        XML_SCHEMAS_ELEM_BLOCK_RESTRICTION,
                        XML_SCHEMAS_ELEM_BLOCK_SUBSTITUTION,
                        -1,
                        -1,
                    ) != 0
                    {
                        xml_schema_psimple_type_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sAttrInvalidValue,
                            null_mut(),
                            attr.into(),
                            null_mut(),
                            Some("(#all | List of (extension | restriction | substitution))"),
                            Some(&attr_value),
                            None,
                            None,
                            None,
                        );
                    }
                } else {
                    // Apply default "block" values.
                    if (*schema).flags & XML_SCHEMAS_BLOCK_DEFAULT_RESTRICTION != 0 {
                        (*decl).flags |= XML_SCHEMAS_ELEM_BLOCK_RESTRICTION;
                    }
                    if (*schema).flags & XML_SCHEMAS_BLOCK_DEFAULT_EXTENSION != 0 {
                        (*decl).flags |= XML_SCHEMAS_ELEM_BLOCK_EXTENSION;
                    }
                    if (*schema).flags & XML_SCHEMAS_BLOCK_DEFAULT_SUBSTITUTION != 0 {
                        (*decl).flags |= XML_SCHEMAS_ELEM_BLOCK_SUBSTITUTION;
                    }
                }
                if xml_get_boolean_prop(self, node, "nillable", 0) != 0 {
                    (*decl).flags |= XML_SCHEMAS_ELEM_NILLABLE;
                }

                if let Some(attr) = xml_schema_get_prop_node(node, "type") {
                    xml_schema_pval_attr_node_qname(
                        self,
                        schema,
                        null_mut(),
                        attr,
                        &raw mut (*decl).named_type_ns,
                        &raw mut (*decl).named_type,
                    );
                    xml_schema_check_reference(
                        self,
                        schema,
                        node,
                        Some(attr),
                        (*decl).named_type_ns,
                    );
                }
                (*decl).value = self.get_prop(node, "default").map_or(null_mut(), |prop| {
                    xml_dict_lookup(self.dict, prop.as_ptr(), prop.len() as i32)
                });
                if let Some(attr) = xml_schema_get_prop_node(node, "fixed") {
                    let fixed = self.get_node_content(Some(attr.into()));
                    let fixed = xml_dict_lookup(self.dict, fixed.as_ptr(), fixed.len() as i32);
                    if !(*decl).value.is_null() {
                        // 3.3.3 : 1
                        // default and fixed must not both be present.
                        xml_schema_pmutual_excl_attr_err(
                            self,
                            XmlParserErrors::XmlSchemapSrcElement1,
                            null_mut(),
                            attr,
                            c"default".as_ptr() as _,
                            c"fixed".as_ptr() as _,
                        );
                    } else {
                        (*decl).flags |= XML_SCHEMAS_ELEM_FIXED;
                        (*decl).value = fixed;
                    }
                }
                // And now for the children...
                if is_schema(child, "complexType") {
                    // 3.3.3 : 3
                    // "type" and either <simpleType> or <complexType> are mutually
                    // exclusive
                    if !(*decl).named_type.is_null() {
                        xml_schema_pcontent_err(
                            self,
                            XmlParserErrors::XmlSchemapSrcElement3,
                            null_mut(),
                            node,
                            child.map(|child| child.into()),
                            Some(
                                "The attribute 'type' and the <complexType> child are mutually exclusive",
                            ),
                            None,
                        );
                    } else {
                        (*decl).subtypes = self.parse_complex_type(schema, child.unwrap(), 0);
                    }
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                } else if is_schema(child, "simpleType") {
                    // 3.3.3 : 3
                    // "type" and either <simpleType> or <complexType> are
                    // mutually exclusive
                    if !(*decl).named_type.is_null() {
                        xml_schema_pcontent_err(
                            self,
                            XmlParserErrors::XmlSchemapSrcElement3,
                            null_mut(),
                            node,
                            child.map(|child| child.into()),
                            Some(
                                "The attribute 'type' and the <simpleType> child are mutually exclusive",
                            ),
                            None,
                        );
                    } else {
                        (*decl).subtypes = self.parse_simple_type(schema, child.unwrap(), 0);
                    }
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                }
                while is_schema(child, "unique")
                    || is_schema(child, "key")
                    || is_schema(child, "keyref")
                {
                    if is_schema(child, "unique") {
                        cur_idc = self.parse_idc(
                            schema,
                            child.unwrap(),
                            XmlSchemaTypeType::XmlSchemaTypeIDCUnique,
                            (*decl).target_namespace,
                        );
                    } else if is_schema(child, "key") {
                        cur_idc = self.parse_idc(
                            schema,
                            child.unwrap(),
                            XmlSchemaTypeType::XmlSchemaTypeIDCKey,
                            (*decl).target_namespace,
                        );
                    } else if is_schema(child, "keyref") {
                        cur_idc = self.parse_idc(
                            schema,
                            child.unwrap(),
                            XmlSchemaTypeType::XmlSchemaTypeIDCKeyref,
                            (*decl).target_namespace,
                        );
                    }
                    if !last_idc.is_null() {
                        (*last_idc).next = cur_idc;
                    } else {
                        (*decl).idcs = cur_idc as _;
                    }
                    last_idc = cur_idc;
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                }
                if let Some(child) = child {
                    xml_schema_pcontent_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                        null_mut(),
                        node,
                        Some(child.into()),
                        None,
                        Some(
                            "(annotation?, ((simpleType | complexType)?, (unique | key | keyref)*))",
                        ),
                    );
                }
                (*decl).annot = annot;
                // NOTE: Element Declaration Representation OK 4. will be checked at a different layer.
                if top_level != 0 {
                    return decl as XmlSchemaBasicItemPtr;
                } else {
                    (*particle).children = decl as XmlSchemaTreeItemPtr;
                    return particle as XmlSchemaBasicItemPtr;
                }
            }

            // return_null:
            if !annot.is_null() {
                if !particle.is_null() {
                    (*particle).annot = null_mut();
                }
                if !decl.is_null() {
                    (*decl).annot = null_mut();
                }
                xml_schema_free_annot(annot);
            }
            null_mut()
        }
    }

    #[doc(alias = "xmlSchemaParseGlobalAttribute")]
    unsafe fn parse_global_attribute(
        &mut self,
        schema: XmlSchemaPtr,
        node: XmlNodePtr,
    ) -> XmlSchemaAttributePtr {
        unsafe {
            let mut attr_value: *const u8 = null();

            // Note that the w3c spec assumes the schema to be validated with schema
            // for schemas beforehand.
            //
            // 3.2.3 Constraints on XML Representations of Attribute Declarations
            if schema.is_null() {
                return null_mut();
            }
            // 3.2.3 : 3.1
            // One of ref or name must be present, but not both
            let Some(attr) = xml_schema_get_prop_node(node, "name") else {
                xml_schema_pmissing_attr_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sAttrMissing,
                    null_mut(),
                    Some(node.into()),
                    Some("name"),
                    None,
                );
                return null_mut();
            };
            if xml_schema_pval_attr_node(
                self,
                null_mut(),
                attr,
                xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasNCName),
                &raw mut attr_value,
            ) != 0
            {
                return null_mut();
            }
            // 3.2.6 Schema Component Constraint: xmlns Not Allowed
            // TODO: Move this to the component layer.
            if xml_str_equal(attr_value, c"xmlns".as_ptr() as _) {
                xml_schema_psimple_type_err(
                    self,
                    XmlParserErrors::XmlSchemapNoXmlns,
                    null_mut(),
                    attr.into(),
                    xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasNCName),
                    None,
                    None,
                    Some("The value of the attribute must not match 'xmlns'"),
                    None,
                    None,
                );
                return null_mut();
            }
            // 3.2.6 Schema Component Constraint: xsi: Not Allowed
            // TODO: Move this to the component layer.
            //       Or better leave it here and add it to the component layer
            //       if we have a schema construction API.
            if xml_str_equal(self.target_namespace, XML_SCHEMA_INSTANCE_NS.as_ptr() as _) {
                let ns = XML_SCHEMA_INSTANCE_NS.to_string_lossy();
                xml_schema_custom_err(
                    self as *mut Self as XmlSchemaAbstractCtxtPtr,
                    XmlParserErrors::XmlSchemapNoXsi,
                    Some(node.into()),
                    null_mut(),
                    format!("The target namespace must not match '{ns}'").as_str(),
                    Some(&ns),
                    None,
                );
            }

            let ret: XmlSchemaAttributePtr =
                xml_schema_add_attribute(self, schema, attr_value, self.target_namespace, node, 1);
            if ret.is_null() {
                return null_mut();
            }
            (*ret).flags |= XML_SCHEMAS_ATTR_GLOBAL;

            // Check for illegal attributes.
            let mut attr = node.properties;
            while let Some(cur_attr) = attr {
                if let Some(ns) = cur_attr.ns {
                    if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                        xml_schema_pillegal_attr_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                            null_mut(),
                            cur_attr,
                        );
                    }
                } else if cur_attr.name().as_deref() != Some("id")
                    && cur_attr.name().as_deref() != Some("default")
                    && cur_attr.name().as_deref() != Some("fixed")
                    && cur_attr.name().as_deref() != Some("name")
                    && cur_attr.name().as_deref() != Some("type")
                {
                    xml_schema_pillegal_attr_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
                attr = cur_attr.next;
            }
            xml_schema_pval_attr_qname(
                self,
                schema,
                null_mut(),
                node,
                "type",
                &raw mut (*ret).type_ns,
                &raw mut (*ret).type_name,
            );

            xml_schema_pval_attr_id(self, node, "id");
            // Attribute "fixed".
            (*ret).def_value = self.get_prop(node, "fixed").map_or(null_mut(), |prop| {
                xml_dict_lookup(self.dict, prop.as_ptr(), prop.len() as i32)
            });
            if !(*ret).def_value.is_null() {
                (*ret).flags |= XML_SCHEMAS_ATTR_FIXED;
            }
            // Attribute "default".
            if let Some(attr) = xml_schema_get_prop_node(node, "default") {
                // 3.2.3 : 1
                // default and fixed must not both be present.
                if (*ret).flags & XML_SCHEMAS_ATTR_FIXED != 0 {
                    xml_schema_pmutual_excl_attr_err(
                        self,
                        XmlParserErrors::XmlSchemapSrcAttribute1,
                        ret as XmlSchemaBasicItemPtr,
                        attr,
                        c"default".as_ptr() as _,
                        c"fixed".as_ptr() as _,
                    );
                } else {
                    let def_value = self.get_node_content(Some(attr.into()));
                    (*ret).def_value =
                        xml_dict_lookup(self.dict, def_value.as_ptr(), def_value.len() as i32);
                }
            }
            // And now for the children...
            let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
            if is_schema(child, "annotation") {
                (*ret).annot = self.parse_annotation(child.unwrap(), 1);
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if is_schema(child, "simpleType") {
                if !(*ret).type_name.is_null() {
                    // 3.2.3 : 4
                    // type and <simpleType> must not both be present.
                    xml_schema_pcontent_err(
                        self,
                        XmlParserErrors::XmlSchemapSrcAttribute4,
                        null_mut(),
                        node,
                        child.map(|child| child.into()),
                        Some(
                            "The attribute 'type' and the <simpleType> child are mutually exclusive",
                        ),
                        None,
                    );
                } else {
                    (*ret).subtypes = self.parse_simple_type(schema, child.unwrap(), 0);
                }
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if let Some(child) = child {
                xml_schema_pcontent_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                    null_mut(),
                    node,
                    Some(child.into()),
                    None,
                    Some("(annotation?, simpleType?)"),
                );
            }

            ret
        }
    }

    /// Parse a XML schema Attribute declaration
    /// *WARNING* this interface is highly subject to change
    ///
    /// Returns the attribute declaration.
    #[doc(alias = "xmlSchemaParseAttribute")]
    unsafe fn parse_local_attribute(
        &mut self,
        schema: XmlSchemaPtr,
        node: XmlNodePtr,
        uses: XmlSchemaItemListPtr<*mut c_void>,
        parent_type: i32,
    ) -> XmlSchemaBasicItemPtr {
        unsafe {
            let mut name: *const u8 = null();
            let mut ns: *const u8 = null();
            let mut using: XmlSchemaAttributeUsePtr = null_mut();
            let mut tmp_ns: *const u8 = null();
            let mut tmp_name: *const u8 = null();
            let mut def_value = None;
            let mut is_ref: i32 = 0;
            let mut occurs: i32 = XML_SCHEMAS_ATTR_USE_OPTIONAL;

            let mut has_form: i32 = 0;
            let mut def_value_type: i32 = 0;

            const WXS_ATTR_DEF_VAL_DEFAULT: i32 = 1;
            const WXS_ATTR_DEF_VAL_FIXED: i32 = 2;

            // 3.2.3 Constraints on XML Representations of Attribute Declarations

            if schema.is_null() {
                return null_mut();
            }
            if let Some(attr) = xml_schema_get_prop_node(node, "ref") {
                if xml_schema_pval_attr_node_qname(
                    self,
                    schema,
                    null_mut(),
                    attr,
                    &raw mut tmp_ns,
                    &raw mut tmp_name,
                ) != 0
                {
                    return null_mut();
                }
                if xml_schema_check_reference(self, schema, node, Some(attr), tmp_ns) != 0 {
                    return null_mut();
                }
                is_ref = 1;
            }
            let nberrors: i32 = self.nberrors;
            // Check for illegal attributes.
            let mut attr = node.properties;
            while let Some(cur_attr) = attr {
                'attr_next: {
                    if let Some(ns) = cur_attr.ns {
                        if ns.href().as_deref() != Some(XML_SCHEMA_NS.to_str().unwrap()) {
                            break 'attr_next;
                        }
                    } else {
                        if is_ref != 0 {
                            if cur_attr.name.as_ref() == "id" {
                                xml_schema_pval_attr_node_id(self, Some(cur_attr));
                                break 'attr_next;
                            } else if cur_attr.name.as_ref() == "ref" {
                                break 'attr_next;
                            }
                        } else if cur_attr.name.as_ref() == "name" {
                            break 'attr_next;
                        } else if cur_attr.name.as_ref() == "id" {
                            xml_schema_pval_attr_node_id(self, Some(cur_attr));
                            break 'attr_next;
                        } else if cur_attr.name.as_ref() == "type" {
                            xml_schema_pval_attr_node_qname(
                                self,
                                schema,
                                null_mut(),
                                cur_attr,
                                &raw mut tmp_ns,
                                &raw mut tmp_name,
                            );
                            break 'attr_next;
                        } else if cur_attr.name.as_ref() == "form" {
                            // Evaluate the target namespace
                            has_form = 1;
                            let attr_value = self.get_node_content(Some(cur_attr.into()));
                            if attr_value == "qualified" {
                                ns = self.target_namespace;
                            } else if attr_value != "unqualified" {
                                xml_schema_psimple_type_err(
                                    self,
                                    XmlParserErrors::XmlSchemapS4sAttrInvalidValue,
                                    null_mut(),
                                    cur_attr.into(),
                                    null_mut(),
                                    Some("(qualified | unqualified)"),
                                    Some(&attr_value),
                                    None,
                                    None,
                                    None,
                                );
                            }
                            break 'attr_next;
                        }
                        if cur_attr.name.as_ref() == "use" {
                            let attr_value = self.get_node_content(Some(cur_attr.into()));
                            // TODO: Maybe we need to normalize the value beforehand.
                            if attr_value == "optional" {
                                occurs = XML_SCHEMAS_ATTR_USE_OPTIONAL;
                            } else if attr_value == "prohibited" {
                                occurs = XML_SCHEMAS_ATTR_USE_PROHIBITED;
                            } else if attr_value == "required" {
                                occurs = XML_SCHEMAS_ATTR_USE_REQUIRED;
                            } else {
                                xml_schema_psimple_type_err(
                                    self,
                                    XmlParserErrors::XmlSchemapInvalidAttrUse,
                                    null_mut(),
                                    cur_attr.into(),
                                    null_mut(),
                                    Some("(optional | prohibited | required)"),
                                    Some(&attr_value),
                                    None,
                                    None,
                                    None,
                                );
                            }
                            break 'attr_next;
                        } else if cur_attr.name.as_ref() == "default" {
                            // 3.2.3 : 1
                            // default and fixed must not both be present.
                            if def_value.is_some() {
                                xml_schema_pmutual_excl_attr_err(
                                    self,
                                    XmlParserErrors::XmlSchemapSrcAttribute1,
                                    null_mut(),
                                    cur_attr,
                                    c"default".as_ptr() as _,
                                    c"fixed".as_ptr() as _,
                                );
                            } else {
                                def_value = Some(self.get_node_content(Some(cur_attr.into())));
                                def_value_type = WXS_ATTR_DEF_VAL_DEFAULT;
                            }
                            break 'attr_next;
                        } else if cur_attr.name.as_ref() == "fixed" {
                            // 3.2.3 : 1
                            // default and fixed must not both be present.
                            if def_value.is_some() {
                                xml_schema_pmutual_excl_attr_err(
                                    self,
                                    XmlParserErrors::XmlSchemapSrcAttribute1,
                                    null_mut(),
                                    cur_attr,
                                    c"default".as_ptr() as _,
                                    c"fixed".as_ptr() as _,
                                );
                            } else {
                                def_value = Some(self.get_node_content(Some(cur_attr.into())));
                                def_value_type = WXS_ATTR_DEF_VAL_FIXED;
                            }
                            break 'attr_next;
                        }
                    }

                    xml_schema_pillegal_attr_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }

                // attr_next:
                attr = cur_attr.next;
            }
            // 3.2.3 : 2
            // If default and use are both present, use must have
            // the actual value optional.
            if def_value_type == WXS_ATTR_DEF_VAL_DEFAULT && occurs != XML_SCHEMAS_ATTR_USE_OPTIONAL
            {
                xml_schema_psimple_type_err(
                    self,
                    XmlParserErrors::XmlSchemapSrcAttribute2,
                    null_mut(),
                    node.into(),
                    null_mut(),
                    Some("(optional | prohibited | required)"),
                    None,
                    Some(
                        "The value of the attribute 'use' must be 'optional' if the attribute 'default' is present",
                    ),
                    None,
                    None,
                );
            }
            // We want correct attributes.
            if nberrors != self.nberrors {
                return null_mut();
            }
            if is_ref == 0 {
                let attr_decl: XmlSchemaAttributePtr;

                // TODO: move XML_SCHEMAS_QUALIF_ATTR to the parser.
                if has_form == 0 && (*schema).flags & XML_SCHEMAS_QUALIF_ATTR != 0 {
                    ns = self.target_namespace;
                }
                // 3.2.6 Schema Component Constraint: xsi: Not Allowed
                // TODO: Move this to the component layer.
                if xml_str_equal(ns, XML_SCHEMA_INSTANCE_NS.as_ptr() as _) {
                    let ns = XML_SCHEMA_INSTANCE_NS.to_string_lossy();
                    xml_schema_custom_err(
                        self as *mut Self as XmlSchemaAbstractCtxtPtr,
                        XmlParserErrors::XmlSchemapNoXsi,
                        Some(node.into()),
                        null_mut(),
                        format!("The target namespace must not match '{ns}'").as_str(),
                        Some(&ns),
                        None,
                    );
                }
                let Some(attr) = xml_schema_get_prop_node(node, "name") else {
                    xml_schema_pmissing_attr_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sAttrMissing,
                        null_mut(),
                        Some(node.into()),
                        Some("name"),
                        None,
                    );
                    return null_mut();
                };
                if xml_schema_pval_attr_node(
                    self,
                    null_mut(),
                    attr,
                    xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasNCName),
                    &raw mut name,
                ) != 0
                {
                    return null_mut();
                }
                // 3.2.6 Schema Component Constraint: xmlns Not Allowed
                // TODO: Move this to the component layer.
                if xml_str_equal(name, c"xmlns".as_ptr() as _) {
                    xml_schema_psimple_type_err(
                        self,
                        XmlParserErrors::XmlSchemapNoXmlns,
                        null_mut(),
                        attr.into(),
                        xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasNCName),
                        None,
                        None,
                        Some("The value of the attribute must not match 'xmlns'"),
                        None,
                        None,
                    );
                    return null_mut();
                }
                if occurs == XML_SCHEMAS_ATTR_USE_PROHIBITED {
                    // goto check_children;
                } else {
                    // Create the attribute use component.
                    using = xml_schema_add_attribute_use(self, node);
                    if using.is_null() {
                        return null_mut();
                    }
                    (*using).occurs = occurs;
                    // Create the attribute declaration.
                    attr_decl = xml_schema_add_attribute(self, schema, name, ns, node, 0);
                    if attr_decl.is_null() {
                        return null_mut();
                    }
                    if !tmp_name.is_null() {
                        (*attr_decl).type_name = tmp_name;
                        (*attr_decl).type_ns = tmp_ns;
                    }
                    (*using).attr_decl = attr_decl;
                    // Value constraint.
                    if let Some(def_value) = def_value {
                        (*attr_decl).def_value =
                            xml_dict_lookup(self.dict, def_value.as_ptr(), def_value.len() as i32);

                        if def_value_type == WXS_ATTR_DEF_VAL_FIXED {
                            (*attr_decl).flags |= XML_SCHEMAS_ATTR_FIXED;
                        }
                    }
                }
            } else if occurs != XML_SCHEMAS_ATTR_USE_PROHIBITED {
                // Create the attribute use component.
                using = xml_schema_add_attribute_use(self, node);
                if using.is_null() {
                    return null_mut();
                }
                // We need to resolve the reference at later stage.
                xml_schema_add_item_size(&raw mut (*self.constructor).pending, 10, using as _);
                (*using).occurs = occurs;
                // Create a QName reference to the attribute declaration.
                let refe: XmlSchemaQNameRefPtr = xml_schema_new_qname_ref(
                    self,
                    XmlSchemaTypeType::XmlSchemaTypeAttribute,
                    tmp_name,
                    tmp_ns,
                );
                if refe.is_null() {
                    return null_mut();
                }
                // Assign the reference. This will be substituted for the
                // referenced attribute declaration when the QName is resolved.
                (*using).attr_decl = refe as XmlSchemaAttributePtr;
                // Value constraint.
                if let Some(def_value) = def_value {
                    (*using).def_value =
                        xml_dict_lookup(self.dict, def_value.as_ptr(), def_value.len() as i32);
                }
                if def_value_type == WXS_ATTR_DEF_VAL_FIXED {
                    (*using).flags |= XML_SCHEMA_ATTR_USE_FIXED;
                }
            }

            // check_children:
            // And now for the children...
            let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
            if occurs == XML_SCHEMAS_ATTR_USE_PROHIBITED {
                if is_schema(child, "annotation") {
                    self.parse_annotation(child.unwrap(), 0);
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                }
                if let Some(child) = child {
                    xml_schema_pcontent_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                        null_mut(),
                        node,
                        Some(child.into()),
                        None,
                        Some("(annotation?)"),
                    );
                }
                // Check for pointlessness of attribute prohibitions.
                if parent_type == XmlSchemaTypeType::XmlSchemaTypeAttributeGroup as i32 {
                    xml_schema_custom_warning(
                        self as *mut Self as XmlSchemaAbstractCtxtPtr,
                        XmlParserErrors::XmlSchemapWarnAttrPointlessProh,
                        Some(node.into()),
                        null_mut(),
                        "Skipping attribute use prohibition, since it is pointless inside an <attributeGroup>",
                        None,
                        None,
                        None,
                    );
                    return null_mut();
                } else if parent_type == XmlSchemaTypeType::XmlSchemaTypeExtension as i32 {
                    xml_schema_custom_warning(
                        self as *mut Self as XmlSchemaAbstractCtxtPtr,
                        XmlParserErrors::XmlSchemapWarnAttrPointlessProh,
                        Some(node.into()),
                        null_mut(),
                        "Skipping attribute use prohibition, since it is pointless when extending a type",
                        None,
                        None,
                        None,
                    );
                    return null_mut();
                }
                if is_ref == 0 {
                    tmp_name = name;
                    tmp_ns = ns;
                }
                // Check for duplicate attribute prohibitions.
                if !uses.is_null() {
                    for using in (*uses)
                        .items
                        .iter()
                        .map(|&using| using as XmlSchemaBasicItemPtr)
                    {
                        if (*using).typ == XmlSchemaTypeType::XmlSchemaExtraAttrUseProhib
                            && tmp_name == (*(using as XmlSchemaAttributeUseProhibPtr)).name
                            && tmp_ns
                                == (*(using as XmlSchemaAttributeUseProhibPtr)).target_namespace
                        {
                            let qname = xml_schema_format_qname(
                                Some(
                                    CStr::from_ptr(tmp_ns as *const i8)
                                        .to_string_lossy()
                                        .as_ref(),
                                ),
                                Some(
                                    CStr::from_ptr(tmp_name as *const i8)
                                        .to_string_lossy()
                                        .as_ref(),
                                ),
                            );
                            xml_schema_custom_warning(
                                self as *mut Self as XmlSchemaAbstractCtxtPtr,
                                XmlParserErrors::XmlSchemapWarnAttrPointlessProh,
                                Some(node.into()),
                                null_mut(),
                                format!("Skipping duplicate attribute use prohibition '{qname}'")
                                    .as_str(),
                                Some(&qname),
                                None,
                                None,
                            );
                            return null_mut();
                        }
                    }
                }
                // Create the attribute prohibition helper component.
                let prohib: XmlSchemaAttributeUseProhibPtr =
                    xml_schema_add_attribute_use_prohib(self);
                if prohib.is_null() {
                    return null_mut();
                }
                (*prohib).node = node.into();
                (*prohib).name = tmp_name;
                (*prohib).target_namespace = tmp_ns;
                if is_ref != 0 {
                    // We need at least to resolve to the attribute declaration.
                    xml_schema_add_item_size(&raw mut (*self.constructor).pending, 10, prohib as _);
                }
                return prohib as XmlSchemaBasicItemPtr;
            }
            if is_schema(child, "annotation") {
                // TODO: Should this go into the attr decl?
                (*using).annot = self.parse_annotation(child.unwrap(), 1);
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if is_ref != 0 {
                if let Some(child) = child {
                    if is_schema(Some(child), "simpleType") {
                        // 3.2.3 : 3.2
                        // If ref is present, then all of <simpleType>,
                        // form and type must be absent.
                        xml_schema_pcontent_err(
                            self,
                            XmlParserErrors::XmlSchemapSrcAttribute3_2,
                            null_mut(),
                            node,
                            Some(child.into()),
                            None,
                            Some("(annotation?)"),
                        );
                    } else {
                        xml_schema_pcontent_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                            null_mut(),
                            node,
                            Some(child.into()),
                            None,
                            Some("(annotation?)"),
                        );
                    }
                }
            } else {
                if is_schema(child, "simpleType") {
                    if !(*(*(using as XmlSchemaAttributeUsePtr)).attr_decl)
                        .type_name
                        .is_null()
                    {
                        // 3.2.3 : 4
                        // type and <simpleType> must not both be present.
                        xml_schema_pcontent_err(
                            self,
                            XmlParserErrors::XmlSchemapSrcAttribute4,
                            null_mut(),
                            node,
                            child.map(|child| child.into()),
                            Some(
                                "The attribute 'type' and the <simpleType> child are mutually exclusive",
                            ),
                            None,
                        );
                    } else {
                        (*(*(using as XmlSchemaAttributeUsePtr)).attr_decl).subtypes =
                            self.parse_simple_type(schema, child.unwrap(), 0);
                    }
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                }
                if let Some(child) = child {
                    xml_schema_pcontent_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                        null_mut(),
                        node,
                        Some(child.into()),
                        None,
                        Some("(annotation?, simpleType?)"),
                    );
                }
            }
            using as XmlSchemaBasicItemPtr
        }
    }

    /// Parses attribute uses and attribute declarations and attribute group references.
    #[doc(alias = "xmlSchemaParseLocalAttributes")]
    unsafe fn parse_local_attributes(
        &mut self,
        schema: XmlSchemaPtr,
        child: &mut Option<XmlNodePtr>,
        list: *mut XmlSchemaItemListPtr<*mut c_void>,
        parent_type: i32,
        has_refs: *mut i32,
    ) -> i32 {
        unsafe {
            let mut item: *mut c_void;

            while is_schema(*child, "attribute") || is_schema(*child, "attributeGroup") {
                if is_schema(*child, "attribute") {
                    item =
                        self.parse_local_attribute(schema, child.unwrap(), *list, parent_type) as _;
                } else {
                    item = self.parse_attribute_group_ref(schema, child.unwrap()) as _;
                    if !item.is_null() && !has_refs.is_null() {
                        *has_refs = 1;
                    }
                }
                if !item.is_null() {
                    if (*list).is_null() {
                        // TODO: Customize grow factor.
                        *list = xml_schema_item_list_create::<*mut c_void>();
                        if (*list).is_null() {
                            return -1;
                        }
                    }
                    if (**list).push(item) == -1 {
                        return -1;
                    }
                }
                *child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            0
        }
    }

    /// parse a XML schema AnyAttribute declaration
    /// *WARNING* this interface is highly subject to change
    ///
    /// Returns a wildcard or NULL.
    #[doc(alias = "xmlSchemaParseAnyAttribute")]
    unsafe fn parse_any_attribute(
        &mut self,
        schema: XmlSchemaPtr,
        node: XmlNodePtr,
    ) -> XmlSchemaWildcardPtr {
        unsafe {
            if schema.is_null() {
                return null_mut();
            }

            let ret: XmlSchemaWildcardPtr = xml_schema_add_wildcard(
                self,
                schema,
                XmlSchemaTypeType::XmlSchemaTypeAnyAttribute,
                Some(node),
            );
            if ret.is_null() {
                return null_mut();
            }
            // Check for illegal attributes.
            let mut attr = node.properties;
            while let Some(cur_attr) = attr {
                if let Some(ns) = cur_attr.ns {
                    if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                        xml_schema_pillegal_attr_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                            null_mut(),
                            cur_attr,
                        );
                    }
                } else if cur_attr.name.as_ref() != "id"
                    && cur_attr.name.as_ref() != "namespace"
                    && cur_attr.name.as_ref() != "processContents"
                {
                    xml_schema_pillegal_attr_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
                attr = cur_attr.next;
            }
            xml_schema_pval_attr_id(self, node, "id");
            // Parse the namespace list.
            if self.parse_wildcard_ns(schema, ret, node) != 0 {
                return null_mut();
            }
            // And now for the children...
            let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
            if is_schema(child, "annotation") {
                (*ret).annot = self.parse_annotation(child.unwrap(), 1);
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if let Some(child) = child {
                xml_schema_pcontent_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                    null_mut(),
                    node,
                    Some(child.into()),
                    None,
                    Some("(annotation?)"),
                );
            }

            ret
        }
    }

    /// Parse a XML schema Attribute Group declaration
    /// *WARNING* this interface is highly subject to change
    ///
    /// Returns the attribute group definition or NULL in case of error.
    #[doc(alias = "xmlSchemaParseAttributeGroupDefinition")]
    unsafe fn parse_attribute_group_definition(
        &mut self,
        schema: XmlSchemaPtr,
        node: XmlNodePtr,
    ) -> XmlSchemaAttributeGroupPtr {
        unsafe {
            let mut name: *const u8 = null();
            let mut has_refs: i32 = 0;

            if schema.is_null() {
                return null_mut();
            }

            let Some(attr) = xml_schema_get_prop_node(node, "name") else {
                xml_schema_pmissing_attr_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sAttrMissing,
                    null_mut(),
                    Some(node.into()),
                    Some("name"),
                    None,
                );
                return null_mut();
            };
            // The name is crucial, exit if invalid.
            if xml_schema_pval_attr_node(
                self,
                null_mut(),
                attr,
                xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasNCName),
                &raw mut name,
            ) != 0
            {
                return null_mut();
            }
            let ret: XmlSchemaAttributeGroupPtr = xml_schema_add_attribute_group_definition(
                self,
                schema,
                name,
                self.target_namespace,
                node,
            );
            if ret.is_null() {
                return null_mut();
            }
            // Check for illegal attributes.
            let mut attr = node.properties;
            while let Some(cur_attr) = attr {
                if let Some(ns) = cur_attr.ns {
                    if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                        xml_schema_pillegal_attr_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                            null_mut(),
                            cur_attr,
                        );
                    }
                } else if cur_attr.name.as_ref() != "name" && cur_attr.name.as_ref() != "id" {
                    xml_schema_pillegal_attr_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
                attr = cur_attr.next;
            }
            // Attribute ID
            xml_schema_pval_attr_id(self, node, "id");
            // And now for the children...
            let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
            if is_schema(child, "annotation") {
                (*ret).annot = self.parse_annotation(child.unwrap(), 1);
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            // Parse contained attribute decls/refs.
            if self.parse_local_attributes(
                schema,
                &mut child,
                &raw mut (*ret).attr_uses as *mut XmlSchemaItemListPtr<*mut c_void>,
                XmlSchemaTypeType::XmlSchemaTypeAttributeGroup as _,
                &raw mut has_refs,
            ) == -1
            {
                return null_mut();
            }
            if has_refs != 0 {
                (*ret).flags |= XML_SCHEMAS_ATTRGROUP_HAS_REFS;
            }
            // Parse the attribute wildcard.
            if is_schema(child, "anyAttribute") {
                (*ret).attribute_wildcard = self.parse_any_attribute(schema, child.unwrap());
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if let Some(child) = child {
                xml_schema_pcontent_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                    null_mut(),
                    node,
                    Some(child.into()),
                    None,
                    Some("(annotation?, ((attribute | attributeGroup)*, anyAttribute?))"),
                );
            }
            ret
        }
    }

    /// Parse an attribute group definition reference.
    /// Note that a reference to an attribute group does not
    /// correspond to any component at all.
    /// *WARNING* this interface is highly subject to change
    ///
    /// Returns the attribute group or NULL in case of error.
    #[doc(alias = "xmlSchemaParseAttributeGroupRef")]
    unsafe fn parse_attribute_group_ref(
        &mut self,
        schema: XmlSchemaPtr,
        node: XmlNodePtr,
    ) -> XmlSchemaQNameRefPtr {
        unsafe {
            let ret: XmlSchemaQNameRefPtr;
            let mut ref_ns: *const u8 = null();
            let mut refe: *const u8 = null();

            if schema.is_null() {
                return null_mut();
            }

            let Some(attr) = xml_schema_get_prop_node(node, "ref") else {
                xml_schema_pmissing_attr_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sAttrMissing,
                    null_mut(),
                    Some(node.into()),
                    Some("ref"),
                    None,
                );
                return null_mut();
            };
            xml_schema_pval_attr_node_qname(
                self,
                schema,
                null_mut(),
                attr,
                &raw mut ref_ns,
                &raw mut refe,
            );
            if xml_schema_check_reference(self, schema, node, Some(attr), ref_ns) != 0 {
                return null_mut();
            }

            // Check for illegal attributes.
            let mut attr = node.properties;
            while let Some(cur_attr) = attr {
                if let Some(ns) = cur_attr.ns {
                    if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                        xml_schema_pillegal_attr_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                            null_mut(),
                            cur_attr,
                        );
                    }
                } else if cur_attr.name.as_ref() != "ref" && cur_attr.name.as_ref() != "id" {
                    xml_schema_pillegal_attr_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
                attr = cur_attr.next;
            }
            // Attribute ID
            xml_schema_pval_attr_id(self, node, "id");

            // And now for the children...
            let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
            if is_schema(child, "annotation") {
                // TODO: We do not have a place to store the annotation, do we?
                self.parse_annotation(child.unwrap(), 0);
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if let Some(child) = child {
                xml_schema_pcontent_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                    null_mut(),
                    node,
                    Some(child.into()),
                    None,
                    Some("(annotation?)"),
                );
            }

            // Handle attribute group redefinitions.
            if self.is_redefine != 0
                && !self.redef.is_null()
                && (*(*self.redef).item).typ == XmlSchemaTypeType::XmlSchemaTypeAttributeGroup
                && refe == (*self.redef).ref_name
                && ref_ns == (*self.redef).ref_target_ns
            {
                // SPEC src-redefine:
                // (7.1) "If it has an <attributeGroup> among its contents
                // the `actual value` of whose ref [attribute] is the same
                // as the `actual value` of its own name attribute plus
                // target namespace, then it must have exactly one such group."
                if self.redef_counter != 0 {
                    let qname = xml_schema_format_qname(
                        Some(
                            CStr::from_ptr(ref_ns as *const i8)
                                .to_string_lossy()
                                .as_ref(),
                        ),
                        Some(CStr::from_ptr(refe as *const i8).to_string_lossy().as_ref()),
                    );

                    xml_schema_custom_err(
                    self as *mut Self as XmlSchemaAbstractCtxtPtr,
                    XmlParserErrors::XmlSchemapSrcRedefine,
                    Some(node.into()),
                    null_mut(),
                    format!("The redefining attribute group definition '{qname}' must not contain more than one reference to the redefined definition").as_str(),
                    Some(&qname),
                    None
                );
                    return null_mut();
                }
                self.redef_counter += 1;
                // URGENT TODO: How to ensure that the reference will not be
                // handled by the normal component resolution mechanism?
                ret = xml_schema_new_qname_ref(
                    self,
                    XmlSchemaTypeType::XmlSchemaTypeAttributeGroup,
                    refe,
                    ref_ns,
                );
                if ret.is_null() {
                    return null_mut();
                }
                (*ret).node = node.into();
                (*self.redef).reference = ret as XmlSchemaBasicItemPtr;
            } else {
                // Create a QName-reference helper component. We will substitute this
                // component for the attribute uses of the referenced attribute group
                // definition.
                ret = xml_schema_new_qname_ref(
                    self,
                    XmlSchemaTypeType::XmlSchemaTypeAttributeGroup,
                    refe,
                    ref_ns,
                );
                if ret.is_null() {
                    return null_mut();
                }
                (*ret).node = node.into();
                // Add to pending items, to be able to resolve the reference.
                xml_schema_add_item_size(&raw mut (*self.constructor).pending, 10, ret as _);
            }
            ret
        }
    }

    /// Parse a XML schema Notation declaration
    ///
    /// Returns the new structure or NULL in case of error
    #[doc(alias = "xmlSchemaParseNotation")]
    unsafe fn parse_notation(
        &mut self,
        schema: XmlSchemaPtr,
        node: XmlNodePtr,
    ) -> XmlSchemaNotationPtr {
        unsafe {
            if schema.is_null() {
                return null_mut();
            }
            let Some(name) = self.get_prop(node, "name") else {
                xml_schema_perr2(
                    self,
                    Some(node.into()),
                    None,
                    XmlParserErrors::XmlSchemapNotationNoName,
                    "Notation has no name\n",
                    None,
                    None,
                );
                return null_mut();
            };
            let name = CString::new(name).unwrap();
            let ret: XmlSchemaNotationPtr = xml_schema_add_notation(
                self,
                schema,
                name.as_ptr() as *const u8,
                self.target_namespace,
                node,
            );
            if ret.is_null() {
                return null_mut();
            }
            xml_schema_pval_attr_id(self, node, "id");

            let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
            if is_schema(child, "annotation") {
                (*ret).annot = self.parse_annotation(child.unwrap(), 1);
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if let Some(child) = child {
                xml_schema_pcontent_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                    null_mut(),
                    node,
                    Some(child.into()),
                    None,
                    Some("(annotation?)"),
                );
            }

            ret
        }
    }

    /// Parse a XML schema ComplexContent definition
    /// *WARNING* this interface is highly subject to change
    ///
    /// Returns the type definition or NULL in case of error
    #[doc(alias = "xmlSchemaParseComplexContent")]
    unsafe fn parse_complex_content(
        &mut self,
        schema: XmlSchemaPtr,
        node: XmlNodePtr,
        has_restriction_or_extension: *mut i32,
    ) -> i32 {
        unsafe {
            if schema.is_null() || has_restriction_or_extension.is_null() {
                return -1;
            }
            *has_restriction_or_extension = 0;
            // Not a component, don't create it.
            let typ: XmlSchemaTypePtr = self.ctxt_type;
            // Check for illegal attributes.
            let mut attr = node.properties;
            while let Some(cur_attr) = attr {
                if let Some(ns) = cur_attr.ns {
                    if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                        xml_schema_pillegal_attr_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                            null_mut(),
                            cur_attr,
                        );
                    }
                } else if cur_attr.name.as_ref() != "id" && cur_attr.name.as_ref() != "mixed" {
                    xml_schema_pillegal_attr_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
                attr = cur_attr.next;
            }

            xml_schema_pval_attr_id(self, node, "id");

            // Set the 'mixed' on the complex type ancestor.
            if xml_get_boolean_prop(self, node, "mixed", 0) != 0
                && (*typ).flags & XML_SCHEMAS_TYPE_MIXED == 0
            {
                (*typ).flags |= XML_SCHEMAS_TYPE_MIXED;
            }
            let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
            if is_schema(child, "annotation") {
                // Add the annotation to the complex type ancestor.
                xml_schema_add_annotation(
                    typ as XmlSchemaAnnotItemPtr,
                    self.parse_annotation(child.unwrap(), 1),
                );
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if child.is_none() {
                xml_schema_pcontent_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sElemMissing,
                    null_mut(),
                    node,
                    None,
                    None,
                    Some("(annotation?, (restriction | extension))"),
                );
            }
            if child.is_none() {
                xml_schema_pcontent_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sElemMissing,
                    null_mut(),
                    node,
                    None,
                    None,
                    Some("(annotation?, (restriction | extension))"),
                );
            }
            if is_schema(child, "restriction") {
                self.parse_restriction(
                    schema,
                    child.unwrap(),
                    XmlSchemaTypeType::XmlSchemaTypeComplexContent,
                );
                *has_restriction_or_extension = 1;
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            } else if is_schema(child, "extension") {
                self.parse_extension(
                    schema,
                    child.unwrap(),
                    XmlSchemaTypeType::XmlSchemaTypeComplexContent,
                );
                *has_restriction_or_extension = 1;
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if let Some(child) = child {
                xml_schema_pcontent_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                    null_mut(),
                    node,
                    Some(child.into()),
                    None,
                    Some("(annotation?, (restriction | extension))"),
                );
            }
            0
        }
    }

    /// Parse a XML schema SimpleContent definition
    /// *WARNING* this interface is highly subject to change
    ///
    /// Returns the type definition or NULL in case of error
    #[doc(alias = "xmlSchemaParseSimpleContent")]
    unsafe fn parse_simple_content(
        &mut self,
        schema: XmlSchemaPtr,
        node: XmlNodePtr,
        has_restriction_or_extension: *mut i32,
    ) -> i32 {
        unsafe {
            if schema.is_null() || has_restriction_or_extension.is_null() {
                return -1;
            }
            *has_restriction_or_extension = 0;
            // Not a component, don't create it.
            let typ: XmlSchemaTypePtr = self.ctxt_type;
            (*typ).content_type = XmlSchemaContentType::XmlSchemaContentSimple;
            // Check for illegal attributes.
            let mut attr = node.properties;
            while let Some(cur_attr) = attr {
                if let Some(ns) = cur_attr.ns {
                    if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                        xml_schema_pillegal_attr_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                            null_mut(),
                            cur_attr,
                        );
                    }
                } else if cur_attr.name.as_ref() != "id" {
                    xml_schema_pillegal_attr_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
                attr = cur_attr.next;
            }

            xml_schema_pval_attr_id(self, node, "id");

            // And now for the children...
            let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
            if is_schema(child, "annotation") {
                // Add the annotation to the complex type ancestor.
                xml_schema_add_annotation(
                    typ as XmlSchemaAnnotItemPtr,
                    self.parse_annotation(child.unwrap(), 1),
                );
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if child.is_none() {
                xml_schema_pcontent_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sElemMissing,
                    null_mut(),
                    node,
                    None,
                    None,
                    Some("(annotation?, (restriction | extension))"),
                );
            }
            if child.is_none() {
                xml_schema_pcontent_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sElemMissing,
                    null_mut(),
                    node,
                    None,
                    None,
                    Some("(annotation?, (restriction | extension))"),
                );
            }
            if is_schema(child, "restriction") {
                self.parse_restriction(
                    schema,
                    child.unwrap(),
                    XmlSchemaTypeType::XmlSchemaTypeSimpleContent,
                );
                *has_restriction_or_extension = 1;
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            } else if is_schema(child, "extension") {
                self.parse_extension(
                    schema,
                    child.unwrap(),
                    XmlSchemaTypeType::XmlSchemaTypeSimpleContent,
                );
                *has_restriction_or_extension = 1;
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if let Some(child) = child {
                xml_schema_pcontent_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                    null_mut(),
                    node,
                    Some(child.into()),
                    None,
                    Some("(annotation?, (restriction | extension))"),
                );
            }
            0
        }
    }

    /// Parse a XML schema Sequence definition.
    /// Applies parts of:
    ///   Schema Representation Constraint:
    ///     Redefinition Constraints and Semantics (src-redefine)
    ///     (6.1), (6.1.1), (6.1.2)
    ///
    ///   Schema Component Constraint:
    ///     All Group Limited (cos-all-limited) (2)
    ///     TODO: Actually this should go to component-level checks,
    ///     but is done here due to performance. Move it to an other layer
    ///     is schema construction via an API is implemented.
    ///
    /// *WARNING* this interface is highly subject to change
    ///
    /// Returns -1 in case of error, 0 if the declaration is improper and
    ///         1 in case of success.
    #[doc(alias = "xmlSchemaParseModelGroup")]
    unsafe fn parse_model_group(
        &mut self,
        schema: XmlSchemaPtr,
        node: XmlNodePtr,
        typ: XmlSchemaTypeType,
        with_particle: i32,
    ) -> XmlSchemaTreeItemPtr {
        unsafe {
            let mut particle: XmlSchemaParticlePtr = null_mut();
            let mut min: i32 = 1;
            let mut max: i32 = 1;
            let mut is_elem_ref: i32 = 0;
            let mut has_refs: i32 = 0;

            if schema.is_null() {
                return null_mut();
            }
            // Create a model group with the given compositor.
            let item: XmlSchemaModelGroupPtr = xml_schema_add_model_group(self, schema, typ, node);
            if item.is_null() {
                return null_mut();
            }

            if with_particle != 0 {
                if typ == XmlSchemaTypeType::XmlSchemaTypeAll {
                    min = xml_get_min_occurs(self, node, 0, 1, 1, "(0 | 1)");
                    max = xml_get_max_occurs(self, node, 1, 1, 1, "1");
                } else {
                    // choice + sequence
                    min = xml_get_min_occurs(self, node, 0, -1, 1, "xs:nonNegativeInteger");
                    max = xml_get_max_occurs(
                        self,
                        node,
                        0,
                        UNBOUNDED as _,
                        1,
                        "(xs:nonNegativeInteger | unbounded)",
                    );
                }
                xml_schema_pcheck_particle_correct_2(self, null_mut(), node, min, max);
                // Create a particle
                particle = xml_schema_add_particle(self, Some(node), min, max);
                if particle.is_null() {
                    return null_mut();
                }
                (*particle).children = item as XmlSchemaTreeItemPtr;
                // Check for illegal attributes.
                let mut attr = node.properties;
                while let Some(cur_attr) = attr {
                    if let Some(ns) = cur_attr.ns {
                        if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                            xml_schema_pillegal_attr_err(
                                self,
                                XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                                null_mut(),
                                cur_attr,
                            );
                        }
                    } else if cur_attr.name.as_ref() != "id"
                        && cur_attr.name.as_ref() != "maxOccurs"
                        && cur_attr.name.as_ref() != "minOccurs"
                    {
                        xml_schema_pillegal_attr_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                            null_mut(),
                            cur_attr,
                        );
                    }
                    attr = cur_attr.next;
                }
            } else {
                // Check for illegal attributes.
                let mut attr = node.properties;
                while let Some(cur_attr) = attr {
                    if let Some(ns) = cur_attr.ns {
                        if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                            xml_schema_pillegal_attr_err(
                                self,
                                XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                                null_mut(),
                                cur_attr,
                            );
                        }
                    } else if cur_attr.name.as_ref() != "id" {
                        xml_schema_pillegal_attr_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                            null_mut(),
                            cur_attr,
                        );
                    }
                    attr = cur_attr.next;
                }
            }

            // Extract and validate attributes.
            xml_schema_pval_attr_id(self, node, "id");
            // And now for the children...
            let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
            if is_schema(child, "annotation") {
                (*item).annot = self.parse_annotation(child.unwrap(), 1);
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if typ == XmlSchemaTypeType::XmlSchemaTypeAll {
                let mut part: XmlSchemaParticlePtr;
                let mut last: XmlSchemaParticlePtr = null_mut();

                while is_schema(child, "element") {
                    part = self.parse_element(schema, child.unwrap(), &raw mut is_elem_ref, 0)
                        as XmlSchemaParticlePtr;
                    // SPEC cos-all-limited (2)
                    // "The {max occurs} of all the particles in the {particles}
                    // of the ('all') group must be 0 or 1.
                    if !part.is_null() {
                        if is_elem_ref != 0 {
                            has_refs += 1;
                        }
                        if (*part).min_occurs > 1 {
                            xml_schema_pcustom_err(
                                self,
                                XmlParserErrors::XmlSchemapCosAllLimited,
                                null_mut(),
                                child.map(|child| child.into()),
                                "Invalid value for minOccurs (must be 0 or 1)",
                                None,
                            );
                            // Reset to 1.
                            (*part).min_occurs = 1;
                        }
                        if (*part).max_occurs > 1 {
                            xml_schema_pcustom_err(
                                self,
                                XmlParserErrors::XmlSchemapCosAllLimited,
                                null_mut(),
                                child.map(|child| child.into()),
                                "Invalid value for maxOccurs (must be 0 or 1)",
                                None,
                            );
                            // Reset to 1.
                            (*part).max_occurs = 1;
                        }
                        if last.is_null() {
                            (*item).children = part as XmlSchemaTreeItemPtr;
                        } else {
                            (*last).next = part as XmlSchemaTreeItemPtr;
                        }
                        last = part;
                    }
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                }
                if let Some(child) = child {
                    xml_schema_pcontent_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                        null_mut(),
                        node,
                        Some(child.into()),
                        None,
                        Some("(annotation?, (annotation?, element*)"),
                    );
                }
            } else {
                // choice + sequence
                let mut part: XmlSchemaTreeItemPtr = null_mut();
                let mut last: XmlSchemaTreeItemPtr = null_mut();

                while is_schema(child, "element")
                    || is_schema(child, "group")
                    || is_schema(child, "any")
                    || is_schema(child, "choice")
                    || is_schema(child, "sequence")
                {
                    if is_schema(child, "element") {
                        part = self.parse_element(schema, child.unwrap(), &raw mut is_elem_ref, 0)
                            as XmlSchemaTreeItemPtr;
                        if !part.is_null() && is_elem_ref != 0 {
                            has_refs += 1;
                        }
                    } else if is_schema(child, "group") {
                        part = self.parse_model_group_def_ref(schema, child.unwrap());
                        if !part.is_null() {
                            has_refs += 1;
                        }
                        // Handle redefinitions.
                        if self.is_redefine != 0
                            && !self.redef.is_null()
                            && (*(*self.redef).item).typ == XmlSchemaTypeType::XmlSchemaTypeGroup
                            && !part.is_null()
                            && !(*part).children.is_null()
                            && xml_schema_get_qname_ref_name((*part).children as _)
                                == (*self.redef).ref_name
                            && xml_schema_get_qname_ref_target_ns((*part).children as _)
                                == (*self.redef).ref_target_ns
                        {
                            // SPEC src-redefine:
                            // (6.1) "If it has a <group> among its contents at
                            // some level the `actual value` of whose ref
                            // [attribute] is the same as the `actual value` of
                            // its own name attribute plus target namespace, then
                            // all of the following must be true:"
                            // (6.1.1) "It must have exactly one such group."
                            if self.redef_counter != 0 {
                                let qname = xml_schema_format_qname(
                                    Some(
                                        CStr::from_ptr((*self.redef).ref_target_ns as *const i8)
                                            .to_string_lossy()
                                            .as_ref(),
                                    ),
                                    Some(
                                        CStr::from_ptr((*self.redef).ref_name as *const i8)
                                            .to_string_lossy()
                                            .as_ref(),
                                    ),
                                );

                                xml_schema_custom_err(
                                self as *mut Self as XmlSchemaAbstractCtxtPtr,
                                XmlParserErrors::XmlSchemapSrcRedefine,
                                child.map(|child| child.into()),
                                null_mut(),
                                format!("The redefining model group definition '{qname}' must not contain more than one reference to the redefined definition").as_str(), 
                                Some(&qname),
                                None
                            );
                                part = null_mut();
                            } else if (*(part as XmlSchemaParticlePtr)).min_occurs != 1
                                || (*(part as XmlSchemaParticlePtr)).max_occurs != 1
                            {
                                let qname = xml_schema_format_qname(
                                    Some(
                                        CStr::from_ptr((*self.redef).ref_target_ns as *const i8)
                                            .to_string_lossy()
                                            .as_ref(),
                                    ),
                                    Some(
                                        CStr::from_ptr((*self.redef).ref_name as *const i8)
                                            .to_string_lossy()
                                            .as_ref(),
                                    ),
                                );

                                // SPEC src-redefine:
                                // (6.1.2) "The `actual value` of both that
                                // group's minOccurs and maxOccurs [attribute]
                                // must be 1 (or `absent`).
                                xml_schema_custom_err(
                                    self as *mut Self as XmlSchemaAbstractCtxtPtr,
                                XmlParserErrors::XmlSchemapSrcRedefine,
                                child.map(|child| child.into()),
                                null_mut(),
                                format!("The redefining model group definition '{qname}' must not contain a reference to the redefined definition with a maxOccurs/minOccurs other than 1").as_str(),
                                Some(&qname),
                                None
                            );
                                part = null_mut();
                            }
                            (*self.redef).reference = part as XmlSchemaBasicItemPtr;
                            self.redef_counter += 1;
                        }
                    } else if is_schema(child, "any") {
                        part = self.parse_any(schema, child.unwrap()) as XmlSchemaTreeItemPtr;
                    } else if is_schema(child, "choice") {
                        part = self.parse_model_group(
                            schema,
                            child.unwrap(),
                            XmlSchemaTypeType::XmlSchemaTypeChoice,
                            1,
                        );
                    } else if is_schema(child, "sequence") {
                        part = self.parse_model_group(
                            schema,
                            child.unwrap(),
                            XmlSchemaTypeType::XmlSchemaTypeSequence,
                            1,
                        );
                    }
                    if !part.is_null() {
                        if last.is_null() {
                            (*item).children = part;
                        } else {
                            (*last).next = part;
                        }
                        last = part;
                    }
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                }
                if let Some(child) = child {
                    xml_schema_pcontent_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                        null_mut(),
                        node,
                        Some(child.into()),
                        None,
                        Some("(annotation?, (element | group | choice | sequence | any)*)"),
                    );
                }
            }
            if max == 0 && min == 0 {
                return null_mut();
            }
            if has_refs != 0 {
                // We need to resolve references.
                xml_schema_add_item_size(&raw mut (*self.constructor).pending, 10, item as _);
            }
            if with_particle != 0 {
                particle as XmlSchemaTreeItemPtr
            } else {
                item as XmlSchemaTreeItemPtr
            }
        }
    }

    /// Parses a XML schema model group definition.
    ///
    /// Note that the constraint src-redefine (6.2) can't be applied until
    /// references have been resolved. So we will do this at the
    /// component fixup level.
    ///
    /// *WARNING* this interface is highly subject to change
    ///
    /// Returns -1 in case of error, 0 if the declaration is improper and 1 in case of success.
    #[doc(alias = "xmlSchemaParseModelGroupDefinition")]
    unsafe fn parse_model_group_definition(
        &mut self,
        schema: XmlSchemaPtr,
        node: XmlNodePtr,
    ) -> XmlSchemaModelGroupDefPtr {
        unsafe {
            let mut name: *const u8 = null();

            if schema.is_null() {
                return null_mut();
            }

            let Some(attr) = xml_schema_get_prop_node(node, "name") else {
                xml_schema_pmissing_attr_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sAttrMissing,
                    null_mut(),
                    Some(node.into()),
                    Some("name"),
                    None,
                );
                return null_mut();
            };
            if xml_schema_pval_attr_node(
                self,
                null_mut(),
                attr,
                xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasNCName),
                &raw mut name,
            ) != 0
            {
                return null_mut();
            }
            let item: XmlSchemaModelGroupDefPtr = xml_schema_add_model_group_definition(
                self,
                schema,
                name,
                self.target_namespace,
                node,
            );
            if item.is_null() {
                return null_mut();
            }
            // Check for illegal attributes.
            let mut attr = node.properties;
            while let Some(cur_attr) = attr {
                if let Some(ns) = cur_attr.ns {
                    if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                        xml_schema_pillegal_attr_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                            null_mut(),
                            cur_attr,
                        );
                    }
                } else if cur_attr.name.as_ref() != "name" && cur_attr.name.as_ref() != "id" {
                    xml_schema_pillegal_attr_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
                attr = cur_attr.next;
            }
            xml_schema_pval_attr_id(self, node, "id");
            // And now for the children...
            let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
            if is_schema(child, "annotation") {
                (*item).annot = self.parse_annotation(child.unwrap(), 1);
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if is_schema(child, "all") {
                (*item).children = self.parse_model_group(
                    schema,
                    child.unwrap(),
                    XmlSchemaTypeType::XmlSchemaTypeAll,
                    0,
                );
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            } else if is_schema(child, "choice") {
                (*item).children = self.parse_model_group(
                    schema,
                    child.unwrap(),
                    XmlSchemaTypeType::XmlSchemaTypeChoice,
                    0,
                );
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            } else if is_schema(child, "sequence") {
                (*item).children = self.parse_model_group(
                    schema,
                    child.unwrap(),
                    XmlSchemaTypeType::XmlSchemaTypeSequence,
                    0,
                );
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }

            if let Some(child) = child {
                xml_schema_pcontent_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                    null_mut(),
                    node,
                    Some(child.into()),
                    None,
                    Some("(annotation?, (all | choice | sequence)?)"),
                );
            }
            item
        }
    }

    /// Parses a reference to a model group definition.
    ///
    /// We will return a particle component with a qname-component or NULL in case of an error.
    #[doc(alias = "xmlSchemaParseModelGroupDefRef")]
    unsafe fn parse_model_group_def_ref(
        &mut self,
        schema: XmlSchemaPtr,
        node: XmlNodePtr,
    ) -> XmlSchemaTreeItemPtr {
        unsafe {
            let mut refe: *const u8 = null();
            let mut ref_ns: *const u8 = null();

            if schema.is_null() {
                return null_mut();
            }

            let Some(attr) = xml_schema_get_prop_node(node, "ref") else {
                xml_schema_pmissing_attr_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sAttrMissing,
                    null_mut(),
                    Some(node.into()),
                    Some("ref"),
                    None,
                );
                return null_mut();
            };
            if xml_schema_pval_attr_node_qname(
                self,
                schema,
                null_mut(),
                attr,
                &raw mut ref_ns,
                &raw mut refe,
            ) != 0
            {
                return null_mut();
            }
            xml_schema_check_reference(self, schema, node, Some(attr), ref_ns);
            let min: i32 = xml_get_min_occurs(self, node, 0, -1, 1, "xs:nonNegativeInteger");
            let max: i32 = xml_get_max_occurs(
                self,
                node,
                0,
                UNBOUNDED as _,
                1,
                "(xs:nonNegativeInteger | unbounded)",
            );
            // Check for illegal attributes.
            let mut attr = node.properties;
            while let Some(cur_attr) = attr {
                if let Some(ns) = cur_attr.ns {
                    if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                        xml_schema_pillegal_attr_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                            null_mut(),
                            cur_attr,
                        );
                    }
                } else if cur_attr.name.as_ref() != "ref"
                    && cur_attr.name.as_ref() != "id"
                    && cur_attr.name.as_ref() != "minOccurs"
                    && cur_attr.name.as_ref() != "maxOccurs"
                {
                    xml_schema_pillegal_attr_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
                attr = cur_attr.next;
            }
            xml_schema_pval_attr_id(self, node, "id");
            let item: XmlSchemaParticlePtr = xml_schema_add_particle(self, Some(node), min, max);
            if item.is_null() {
                return null_mut();
            }
            // Create a qname-reference and set as the term; it will be substituted
            // for the model group after the reference has been resolved.
            (*item).children =
                xml_schema_new_qname_ref(self, XmlSchemaTypeType::XmlSchemaTypeGroup, refe, ref_ns)
                    as XmlSchemaTreeItemPtr;
            xml_schema_pcheck_particle_correct_2(self, item, node, min, max);
            // And now for the children...
            let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
            // TODO: Is annotation even allowed for a model group reference?
            if is_schema(child, "annotation") {
                // TODO: What to do exactly with the annotation?
                (*item).annot = self.parse_annotation(child.unwrap(), 1);
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if let Some(child) = child {
                xml_schema_pcontent_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                    null_mut(),
                    node,
                    Some(child.into()),
                    None,
                    Some("(annotation?)"),
                );
            }
            // Corresponds to no component at all if minOccurs==maxOccurs==0.
            if min == 0 && max == 0 {
                return null_mut();
            }

            item as XmlSchemaTreeItemPtr
        }
    }

    /// Parse a XML schema Restriction definition
    /// *WARNING* this interface is highly subject to change
    ///
    /// Returns the type definition or NULL in case of error
    #[doc(alias = "xmlSchemaParseRestriction")]
    unsafe fn parse_restriction(
        &mut self,
        schema: XmlSchemaPtr,
        node: XmlNodePtr,
        parent_type: XmlSchemaTypeType,
    ) -> XmlSchemaTypePtr {
        unsafe {
            if schema.is_null() {
                return null_mut();
            }
            // Not a component, don't create it.
            let typ: XmlSchemaTypePtr = self.ctxt_type;
            (*typ).flags |= XML_SCHEMAS_TYPE_DERIVATION_METHOD_RESTRICTION;

            // Check for illegal attributes.
            let mut attr = node.properties;
            while let Some(cur_attr) = attr {
                if let Some(ns) = cur_attr.ns {
                    if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                        xml_schema_pillegal_attr_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                            null_mut(),
                            cur_attr,
                        );
                    }
                } else if cur_attr.name.as_ref() != "id" && cur_attr.name.as_ref() != "base" {
                    xml_schema_pillegal_attr_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
                attr = cur_attr.next;
            }
            // Extract and validate attributes.
            xml_schema_pval_attr_id(self, node, "id");

            // Attribute

            // Extract the base type. The "base" attribute is mandatory if inside
            // a complex type or if redefining.
            //
            // SPEC (1.2) "...otherwise (<restriction> has no <simpleType> "
            // among its [children]), the simple type definition which is
            // the {content type} of the type definition `resolved` to by
            // the `actual value` of the base [attribute]"
            if xml_schema_pval_attr_qname(
                self,
                schema,
                null_mut(),
                node,
                "base",
                &raw mut (*typ).base_ns,
                &raw mut (*typ).base,
            ) == 0
            {
                if (*typ).base.is_null() && (*typ).typ == XmlSchemaTypeType::XmlSchemaTypeComplex {
                    xml_schema_pmissing_attr_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sAttrMissing,
                        null_mut(),
                        Some(node.into()),
                        Some("base"),
                        None,
                    );
                } else if self.is_redefine != 0 && (*typ).flags & XML_SCHEMAS_TYPE_GLOBAL != 0 {
                    if (*typ).base.is_null() {
                        xml_schema_pmissing_attr_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sAttrMissing,
                            null_mut(),
                            Some(node.into()),
                            Some("base"),
                            None,
                        );
                    } else if !xml_str_equal((*typ).base, (*typ).name)
                        || !xml_str_equal((*typ).base_ns, (*typ).target_namespace)
                    {
                        let q1 = xml_schema_format_qname(
                            Some(
                                CStr::from_ptr((*typ).base_ns as *const i8)
                                    .to_string_lossy()
                                    .as_ref(),
                            ),
                            Some(
                                CStr::from_ptr((*typ).base as *const i8)
                                    .to_string_lossy()
                                    .as_ref(),
                            ),
                        );
                        let q2 = xml_schema_format_qname(
                            Some(
                                CStr::from_ptr((*typ).target_namespace as *const i8)
                                    .to_string_lossy()
                                    .as_ref(),
                            ),
                            Some(
                                CStr::from_ptr((*typ).name as *const i8)
                                    .to_string_lossy()
                                    .as_ref(),
                            ),
                        );

                        // REDEFINE: SPEC src-redefine (5)
                        // "Within the [children], each <simpleType> must have a
                        // <restriction> among its [children] ... the `actual value` of
                        // whose base [attribute] must be the same as the `actual value`
                        // of its own name attribute plus target namespace;"
                        xml_schema_pcustom_err_ext(
                        self,
                        XmlParserErrors::XmlSchemapSrcRedefine,
                        null_mut(),
                        Some(node.into()),
                        format!("This is a redefinition, but the QName value '{q1}' of the 'base' attribute does not match the type's designation '{q2}'").as_str(),
                        Some(&q1),
                        Some(&q2),
                        None
                    );
                        // Avoid confusion and erase the values.
                        (*typ).base = null_mut();
                        (*typ).base_ns = null_mut();
                    }
                }
            }
            // And now for the children...
            let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
            if is_schema(child, "annotation") {
                // Add the annotation to the simple type ancestor.
                xml_schema_add_annotation(
                    typ as XmlSchemaAnnotItemPtr,
                    self.parse_annotation(child.unwrap(), 1),
                );
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if parent_type == XmlSchemaTypeType::XmlSchemaTypeSimple {
                // Corresponds to <simpleType><restriction><simpleType>.
                if is_schema(child, "simpleType") {
                    if !(*typ).base.is_null() {
                        // src-restriction-base-or-simpleType
                        // Either the base [attribute] or the simpleType [child] of the
                        // <restriction> element must be present, but not both.
                        xml_schema_pcontent_err(
                            self,
                            XmlParserErrors::XmlSchemapSrcRestrictionBaseOrSimpletype,
                            null_mut(),
                            node,
                            child.map(|child| child.into()),
                            Some(
                                "The attribute 'base' and the <simpleType> child are mutually exclusive",
                            ),
                            None,
                        );
                    } else {
                        (*typ).base_type =
                            self.parse_simple_type(schema, child.unwrap(), 0) as XmlSchemaTypePtr;
                    }
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                } else if (*typ).base.is_null() {
                    xml_schema_pcontent_err(
                        self,
                        XmlParserErrors::XmlSchemapSrcRestrictionBaseOrSimpletype,
                        null_mut(),
                        node,
                        child.map(|child| child.into()),
                        Some("Either the attribute 'base' or a <simpleType> child must be present"),
                        None,
                    );
                }
            } else if parent_type == XmlSchemaTypeType::XmlSchemaTypeComplexContent {
                // Corresponds to <complexType><complexContent><restriction>...
                // followed by:
                //
                // Model groups <all>, <choice> and <sequence>.
                if is_schema(child, "all") {
                    (*typ).subtypes = self.parse_model_group(
                        schema,
                        child.unwrap(),
                        XmlSchemaTypeType::XmlSchemaTypeAll,
                        1,
                    ) as XmlSchemaTypePtr;
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                } else if is_schema(child, "choice") {
                    (*typ).subtypes = self.parse_model_group(
                        schema,
                        child.unwrap(),
                        XmlSchemaTypeType::XmlSchemaTypeChoice,
                        1,
                    ) as XmlSchemaTypePtr;
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                } else if is_schema(child, "sequence") {
                    (*typ).subtypes = self.parse_model_group(
                        schema,
                        child.unwrap(),
                        XmlSchemaTypeType::XmlSchemaTypeSequence,
                        1,
                    ) as XmlSchemaTypePtr;
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                // Model group reference <group>.
                } else if is_schema(child, "group") {
                    (*typ).subtypes =
                        self.parse_model_group_def_ref(schema, child.unwrap()) as XmlSchemaTypePtr;
                    // Note that the reference will be resolved in
                    // xmlSchemaResolveTypeReferences();
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                }
            } else if parent_type == XmlSchemaTypeType::XmlSchemaTypeSimpleContent {
                // Corresponds to <complexType><simpleContent><restriction>...
                //
                // "1.1 the simple type definition corresponding to the <simpleType>
                // among the [children] of <restriction> if there is one;"
                if is_schema(child, "simpleType") {
                    // We will store the to-be-restricted simple type in
                    // (*typ).contentTypeDef *temporarily*.
                    (*typ).content_type_def =
                        self.parse_simple_type(schema, child.unwrap(), 0) as XmlSchemaTypePtr;
                    if (*typ).content_type_def.is_null() {
                        return null_mut();
                    }
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                }
            }

            if parent_type == XmlSchemaTypeType::XmlSchemaTypeSimple
                || parent_type == XmlSchemaTypeType::XmlSchemaTypeSimpleContent
            {
                let mut facet: XmlSchemaFacetPtr;
                let mut lastfacet: XmlSchemaFacetPtr = null_mut();
                // Corresponds to <complexType><simpleContent><restriction>...
                // <simpleType><restriction>...

                // Add the facets to the simple type ancestor.

                // TODO: Datatypes: 4.1.3 Constraints on XML Representation of
                // Simple Type Definition Schema Representation Constraint:
                // *Single Facet Value*
                while is_schema(child, "minInclusive")
                    || is_schema(child, "minExclusive")
                    || is_schema(child, "maxInclusive")
                    || is_schema(child, "maxExclusive")
                    || is_schema(child, "totalDigits")
                    || is_schema(child, "fractionDigits")
                    || is_schema(child, "pattern")
                    || is_schema(child, "enumeration")
                    || is_schema(child, "whiteSpace")
                    || is_schema(child, "length")
                    || is_schema(child, "maxLength")
                    || is_schema(child, "minLength")
                {
                    facet = self.parse_facet(schema, child.unwrap());
                    if !facet.is_null() {
                        if lastfacet.is_null() {
                            (*typ).facets = facet;
                        } else {
                            (*lastfacet).next = facet;
                        }
                        lastfacet = facet;
                        (*lastfacet).next = null_mut();
                    }
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                }
                // Create links for derivation and validation.
                if !(*typ).facets.is_null() {
                    let mut facet_link: XmlSchemaFacetLinkPtr;
                    let mut last_facet_link: XmlSchemaFacetLinkPtr = null_mut();

                    facet = (*typ).facets;
                    while {
                        facet_link =
                            xml_malloc(size_of::<XmlSchemaFacetLink>()) as XmlSchemaFacetLinkPtr;
                        if facet_link.is_null() {
                            xml_schema_perr_memory(self, "allocating a facet link", None);
                            xml_free(facet_link as _);
                            return null_mut();
                        }
                        (*facet_link).facet = facet;
                        (*facet_link).next = null_mut();
                        if last_facet_link.is_null() {
                            (*typ).facet_set = facet_link;
                        } else {
                            (*last_facet_link).next = facet_link;
                        }
                        last_facet_link = facet_link;
                        facet = (*facet).next;

                        !facet.is_null()
                    } {}
                }
            }
            if (*typ).typ == XmlSchemaTypeType::XmlSchemaTypeComplex {
                // Attribute uses/declarations.
                if self.parse_local_attributes(
                    schema,
                    &mut child,
                    &raw mut (*typ).attr_uses as *mut XmlSchemaItemListPtr<*mut c_void>,
                    XmlSchemaTypeType::XmlSchemaTypeRestriction as i32,
                    null_mut(),
                ) == -1
                {
                    return null_mut();
                }
                // Attribute wildcard.
                if is_schema(child, "anyAttribute") {
                    (*typ).attribute_wildcard = self.parse_any_attribute(schema, child.unwrap());
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                }
            }
            if let Some(child) = child {
                if parent_type == XmlSchemaTypeType::XmlSchemaTypeComplexContent {
                    xml_schema_pcontent_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                        null_mut(),
                        node,
                        Some(child.into()),
                        None,
                        Some(
                            "annotation?, (group | all | choice | sequence)?, ((attribute | attributeGroup)*, anyAttribute?))",
                        ),
                    );
                } else if parent_type == XmlSchemaTypeType::XmlSchemaTypeSimpleContent {
                    xml_schema_pcontent_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                        null_mut(),
                        node,
                        Some(child.into()),
                        None,
                        Some(
                            "(annotation?, (simpleType?, (minExclusive | minInclusive | maxExclusive | maxInclusive | totalDigits | fractionDigits | length | minLength | maxLength | enumeration | whiteSpace | pattern)*)?, ((attribute | attributeGroup)*, anyAttribute?))",
                        ),
                    );
                } else {
                    // Simple type
                    xml_schema_pcontent_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                        null_mut(),
                        node,
                        Some(child.into()),
                        None,
                        Some(
                            "(annotation?, (simpleType?, (minExclusive | minInclusive | maxExclusive | maxInclusive | totalDigits | fractionDigits | length | minLength | maxLength | enumeration | whiteSpace | pattern)*))",
                        ),
                    );
                }
            }
            null_mut()
        }
    }

    /// Parses an <extension>, which is found inside a
    /// <simpleContent> or <complexContent>.
    /// *WARNING* this interface is highly subject to change.
    ///
    /// TODO: Returns the type definition or NULL in case of error
    #[doc(alias = "xmlSchemaParseExtension")]
    unsafe fn parse_extension(
        &mut self,
        schema: XmlSchemaPtr,
        node: XmlNodePtr,
        parent_type: XmlSchemaTypeType,
    ) -> XmlSchemaTypePtr {
        unsafe {
            if schema.is_null() {
                return null_mut();
            }
            // Not a component, don't create it.
            let typ: XmlSchemaTypePtr = self.ctxt_type;
            (*typ).flags |= XML_SCHEMAS_TYPE_DERIVATION_METHOD_EXTENSION;

            // Check for illegal attributes.
            let mut attr = node.properties;
            while let Some(cur_attr) = attr {
                if let Some(ns) = cur_attr.ns {
                    if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                        xml_schema_pillegal_attr_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                            null_mut(),
                            cur_attr,
                        );
                    }
                } else if cur_attr.name.as_ref() != "id" && cur_attr.name.as_ref() != "base" {
                    xml_schema_pillegal_attr_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
                attr = cur_attr.next;
            }

            xml_schema_pval_attr_id(self, node, "id");

            // Attribute "base" - mandatory.
            if xml_schema_pval_attr_qname(
                self,
                schema,
                null_mut(),
                node,
                "base",
                &raw mut (*typ).base_ns,
                &raw mut (*typ).base,
            ) == 0
                && (*typ).base.is_null()
            {
                xml_schema_pmissing_attr_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sAttrMissing,
                    null_mut(),
                    Some(node.into()),
                    Some("base"),
                    None,
                );
            }
            // And now for the children...
            let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
            if is_schema(child, "annotation") {
                // Add the annotation to the type ancestor.
                xml_schema_add_annotation(
                    typ as XmlSchemaAnnotItemPtr,
                    self.parse_annotation(child.unwrap(), 1),
                );
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if parent_type == XmlSchemaTypeType::XmlSchemaTypeComplexContent {
                // Corresponds to <complexType><complexContent><extension>... and:
                //
                // Model groups <all>, <choice>, <sequence> and <group>.
                if is_schema(child, "all") {
                    (*typ).subtypes = self.parse_model_group(
                        schema,
                        child.unwrap(),
                        XmlSchemaTypeType::XmlSchemaTypeAll,
                        1,
                    ) as XmlSchemaTypePtr;
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                } else if is_schema(child, "choice") {
                    (*typ).subtypes = self.parse_model_group(
                        schema,
                        child.unwrap(),
                        XmlSchemaTypeType::XmlSchemaTypeChoice,
                        1,
                    ) as XmlSchemaTypePtr;
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                } else if is_schema(child, "sequence") {
                    (*typ).subtypes = self.parse_model_group(
                        schema,
                        child.unwrap(),
                        XmlSchemaTypeType::XmlSchemaTypeSequence,
                        1,
                    ) as XmlSchemaTypePtr;
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                } else if is_schema(child, "group") {
                    (*typ).subtypes =
                        self.parse_model_group_def_ref(schema, child.unwrap()) as XmlSchemaTypePtr;
                    // Note that the reference will be resolved in
                    // xmlSchemaResolveTypeReferences();
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                }
            }
            if child.is_some() {
                // Attribute uses/declarations.
                if self.parse_local_attributes(
                    schema,
                    &mut child,
                    &raw mut (*typ).attr_uses as *mut XmlSchemaItemListPtr<*mut c_void>,
                    XmlSchemaTypeType::XmlSchemaTypeExtension as i32,
                    null_mut(),
                ) == -1
                {
                    return null_mut();
                }
                // Attribute wildcard.
                if is_schema(child, "anyAttribute") {
                    (*self.ctxt_type).attribute_wildcard =
                        self.parse_any_attribute(schema, child.unwrap());
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                }
            }
            if let Some(child) = child {
                if parent_type == XmlSchemaTypeType::XmlSchemaTypeComplexContent {
                    // Complex content extension.
                    xml_schema_pcontent_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                        null_mut(),
                        node,
                        Some(child.into()),
                        None,
                        Some(
                            "(annotation?, ((group | all | choice | sequence)?, ((attribute | attributeGroup)*, anyAttribute?)))",
                        ),
                    );
                } else {
                    // Simple content extension.
                    xml_schema_pcontent_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                        null_mut(),
                        node,
                        Some(child.into()),
                        None,
                        Some("(annotation?, ((attribute | attributeGroup)*, anyAttribute?))"),
                    );
                }
            }
            null_mut()
        }
    }

    /// parse a XML schema List definition
    /// *WARNING* this interface is highly subject to change
    ///
    /// Returns -1 in case of error, 0 if the declaration is improper and 1 in case of success.
    #[doc(alias = "xmlSchemaParseList")]
    unsafe fn parse_list(&mut self, schema: XmlSchemaPtr, node: XmlNodePtr) -> XmlSchemaTypePtr {
        unsafe {
            if schema.is_null() {
                return null_mut();
            }
            // Not a component, don't create it.
            let typ: XmlSchemaTypePtr = self.ctxt_type;
            // Mark the typ as being of variety "list".
            (*typ).flags |= XML_SCHEMAS_TYPE_VARIETY_LIST;
            // SPEC (Base type) (2) "If the <list> or <union> alternative is chosen,
            // then the `simple ur-type definition`."
            (*typ).base_type =
                xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasAnySimpletype);
            // Check for illegal attributes.
            let mut attr = node.properties;
            while let Some(cur_attr) = attr {
                if let Some(ns) = cur_attr.ns {
                    if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                        xml_schema_pillegal_attr_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                            null_mut(),
                            cur_attr,
                        );
                    }
                } else if cur_attr.name.as_ref() != "id" && cur_attr.name.as_ref() != "itemType" {
                    xml_schema_pillegal_attr_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
                attr = cur_attr.next;
            }

            xml_schema_pval_attr_id(self, node, "id");

            // Attribute "itemType". NOTE that we will use the "ref" and "refNs"
            // fields for holding the reference to the itemType.
            //
            // REVAMP TODO: Use the "base" and "baseNs" fields, since we will remove
            // the "ref" fields.
            xml_schema_pval_attr_qname(
                self,
                schema,
                null_mut(),
                node,
                "itemType",
                &raw mut (*typ).base_ns,
                &raw mut (*typ).base,
            );
            // And now for the children...
            let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
            if is_schema(child, "annotation") {
                xml_schema_add_annotation(
                    typ as XmlSchemaAnnotItemPtr,
                    self.parse_annotation(child.unwrap(), 1),
                );
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if is_schema(child, "simpleType") {
                // src-list-itemType-or-simpleType
                // Either the itemType [attribute] or the <simpleType> [child] of
                // the <list> element must be present, but not both.
                if !(*typ).base.is_null() {
                    xml_schema_pcustom_err(
                        self,
                        XmlParserErrors::XmlSchemapSrcSimpleType1,
                        null_mut(),
                        Some(node.into()),
                        "The attribute 'itemType' and the <simpleType> child are mutually exclusive",
                        None,
                    );
                } else {
                    (*typ).subtypes = self.parse_simple_type(schema, child.unwrap(), 0);
                }
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            } else if (*typ).base.is_null() {
                xml_schema_pcustom_err(
                    self,
                    XmlParserErrors::XmlSchemapSrcSimpleType1,
                    null_mut(),
                    Some(node.into()),
                    "Either the attribute 'itemType' or the <simpleType> child must be present",
                    None,
                );
            }
            if let Some(child) = child {
                xml_schema_pcontent_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                    null_mut(),
                    node,
                    Some(child.into()),
                    None,
                    Some("(annotation?, simpleType?)"),
                );
            }
            if (*typ).base.is_null()
                && (*typ).subtypes.is_null()
                && xml_schema_get_prop_node(node, "itemType").is_none()
            {
                xml_schema_pcustom_err(
                    self,
                    XmlParserErrors::XmlSchemapSrcSimpleType1,
                    null_mut(),
                    Some(node.into()),
                    "Either the attribute 'itemType' or the <simpleType> child must be present",
                    None,
                );
            }
            null_mut()
        }
    }

    /// Parse a XML schema Union definition
    /// *WARNING* this interface is highly subject to change
    ///
    /// Returns -1 in case of internal error, 0 in case of success and a positive
    /// error code otherwise.
    #[doc(alias = "xmlSchemaParseUnion")]
    unsafe fn parse_union(&mut self, schema: XmlSchemaPtr, node: XmlNodePtr) -> i32 {
        unsafe {
            if schema.is_null() {
                return -1;
            }
            // Not a component, don't create it.
            let typ: XmlSchemaTypePtr = self.ctxt_type;
            // Mark the simple typ as being of variety "union".
            (*typ).flags |= XML_SCHEMAS_TYPE_VARIETY_UNION;
            // SPEC (Base type) (2) "If the <list> or <union> alternative is chosen,
            // then the `simple ur-type definition`."
            (*typ).base_type =
                xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasAnySimpletype);
            // Check for illegal attributes.
            let mut attr = node.properties;
            while let Some(cur_attr) = attr {
                if let Some(ns) = cur_attr.ns {
                    if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                        xml_schema_pillegal_attr_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                            null_mut(),
                            cur_attr,
                        );
                    }
                } else if cur_attr.name.as_ref() != "id" && cur_attr.name.as_ref() != "memberTypes"
                {
                    xml_schema_pillegal_attr_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
                attr = cur_attr.next;
            }
            xml_schema_pval_attr_id(self, node, "id");
            // Attribute "memberTypes". This is a list of QNames.
            // TODO: Check the value to contain anything.
            let attr = xml_schema_get_prop_node(node, "memberTypes");
            if let Some(attr) = attr {
                let mut local_name: *const u8 = null();
                let mut ns_name: *mut u8 = null_mut();
                let mut link: XmlSchemaTypeLinkPtr;
                let mut last_link: XmlSchemaTypeLinkPtr = null_mut();
                let mut refe: XmlSchemaQNameRefPtr;

                let cur = self.get_node_content(Some(attr.into()));
                (*typ).base = xml_dict_lookup(self.dict, cur.as_ptr(), cur.len() as i32);
                let mut cur = cur.as_str();
                while !cur.is_empty() {
                    cur = cur.trim_start_matches(|c| xml_is_blank_char(c as u32));
                    let end = cur.trim_start_matches(|c| !xml_is_blank_char(c as u32));
                    if end.len() == cur.len() {
                        break;
                    }
                    let tmp = CString::new(&cur[..cur.len() - end.len()]).unwrap();
                    if xml_schema_pval_attr_node_qname_value(
                        self,
                        schema,
                        null_mut(),
                        attr,
                        tmp.as_ptr() as *const u8,
                        &raw mut ns_name as _,
                        &raw mut local_name,
                    ) == 0
                    {
                        // Create the member type link.
                        link = xml_malloc(size_of::<XmlSchemaTypeLink>()) as XmlSchemaTypeLinkPtr;
                        if link.is_null() {
                            xml_schema_perr_memory(
                                self,
                                "xmlSchemaParseUnion, allocating a type link",
                                None,
                            );
                            return -1;
                        }
                        (*link).typ = null_mut();
                        (*link).next = null_mut();
                        if last_link.is_null() {
                            (*typ).member_types = link;
                        } else {
                            (*last_link).next = link;
                        }
                        last_link = link;
                        // Create a reference item.
                        refe = xml_schema_new_qname_ref(
                            self,
                            XmlSchemaTypeType::XmlSchemaTypeSimple,
                            local_name,
                            ns_name,
                        );
                        if refe.is_null() {
                            return -1;
                        }
                        // Assign the reference to the link, it will be resolved
                        // later during fixup of the union simple type.
                        (*link).typ = refe as XmlSchemaTypePtr;
                    }
                    cur = end;
                }
            }
            // And now for the children...
            let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
            if is_schema(child, "annotation") {
                // Add the annotation to the simple type ancestor.
                xml_schema_add_annotation(
                    typ as XmlSchemaAnnotItemPtr,
                    self.parse_annotation(child.unwrap(), 1),
                );
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if is_schema(child, "simpleType") {
                let mut subtype: XmlSchemaTypePtr;
                let mut last: XmlSchemaTypePtr = null_mut();

                // Anchor the member types in the "subtypes" field of the simple type.
                while is_schema(child, "simpleType") {
                    subtype = self.parse_simple_type(schema, child.unwrap(), 0) as XmlSchemaTypePtr;
                    if !subtype.is_null() {
                        if last.is_null() {
                            (*typ).subtypes = subtype;
                            last = subtype;
                        } else {
                            (*last).next = subtype;
                            last = subtype;
                        }
                        (*last).next = null_mut();
                    }
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                }
            }
            if let Some(child) = child {
                xml_schema_pcontent_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                    null_mut(),
                    node,
                    Some(child.into()),
                    None,
                    Some("(annotation?, simpleType*)"),
                );
            }
            if attr.is_none() && (*typ).subtypes.is_null() {
                // src-union-memberTypes-or-simpleTypes
                // Either the memberTypes [attribute] of the <union> element must
                // be non-empty or there must be at least one simpleType [child].
                xml_schema_pcustom_err(
                    self,
                    XmlParserErrors::XmlSchemapSrcUnionMembertypesOrSimpletypes,
                    null_mut(),
                    Some(node.into()),
                    "Either the attribute 'memberTypes' or at least one <simpleType> child must be present",
                    None,
                );
            }
            0
        }
    }

    /// Parses a XML Schema identity-constraint definition.
    ///
    /// Returns the parsed identity-constraint definition.
    #[doc(alias = "xmlSchemaParseIDC")]
    unsafe fn parse_idc(
        &mut self,
        schema: XmlSchemaPtr,
        node: XmlNodePtr,
        idc_category: XmlSchemaTypeType,
        target_namespace: *const u8,
    ) -> XmlSchemaIDCPtr {
        unsafe {
            let mut name: *const u8 = null();
            let mut field: XmlSchemaIdcselectPtr;
            let mut last_field: XmlSchemaIdcselectPtr = null_mut();

            // Check for illegal attributes.
            let mut attr = node.properties;
            while let Some(cur_attr) = attr {
                if let Some(ns) = cur_attr.ns {
                    if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                        xml_schema_pillegal_attr_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                            null_mut(),
                            cur_attr,
                        );
                    }
                } else if cur_attr.name.as_ref() != "id"
                    && cur_attr.name.as_ref() != "name"
                    && (idc_category != XmlSchemaTypeType::XmlSchemaTypeIDCKeyref
                        || cur_attr.name.as_ref() != "refer")
                {
                    xml_schema_pillegal_attr_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
                attr = cur_attr.next;
            }
            // Attribute "name" (mandatory).
            let Some(attr) = xml_schema_get_prop_node(node, "name") else {
                xml_schema_pmissing_attr_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sAttrMissing,
                    null_mut(),
                    Some(node.into()),
                    Some("name"),
                    None,
                );
                return null_mut();
            };
            if xml_schema_pval_attr_node(
                self,
                null_mut(),
                attr,
                xml_schema_get_built_in_type(XmlSchemaValType::XmlSchemasNCName),
                &raw mut name,
            ) != 0
            {
                return null_mut();
            }
            // Create the component.
            let item: XmlSchemaIDCPtr =
                xml_schema_add_idc(self, schema, name, target_namespace, idc_category, node);
            if item.is_null() {
                return null_mut();
            }

            xml_schema_pval_attr_id(self, node, "id");
            if idc_category == XmlSchemaTypeType::XmlSchemaTypeIDCKeyref {
                // Attribute "refer" (mandatory).
                if let Some(attr) = xml_schema_get_prop_node(node, "refer") {
                    // Create a reference item.
                    (*item).refe = xml_schema_new_qname_ref(
                        self,
                        XmlSchemaTypeType::XmlSchemaTypeIDCKey,
                        null_mut(),
                        null_mut(),
                    );
                    if (*item).refe.is_null() {
                        return null_mut();
                    }
                    xml_schema_pval_attr_node_qname(
                        self,
                        schema,
                        null_mut(),
                        attr,
                        &raw mut (*(*item).refe).target_namespace,
                        &raw mut (*(*item).refe).name,
                    );
                    xml_schema_check_reference(
                        self,
                        schema,
                        node,
                        Some(attr),
                        (*(*item).refe).target_namespace,
                    );
                } else {
                    xml_schema_pmissing_attr_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sAttrMissing,
                        null_mut(),
                        Some(node.into()),
                        Some("refer"),
                        None,
                    );
                }
            }
            // And now for the children...
            let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
            if is_schema(child, "annotation") {
                (*item).annot = self.parse_annotation(child.unwrap(), 1);
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if child.is_none() {
                xml_schema_pcontent_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sElemMissing,
                    null_mut(),
                    node,
                    None,
                    Some("A child element is missing"),
                    Some("(annotation?, (selector, field+))"),
                );
            }
            // Child element <selector>.
            if is_schema(child, "selector") {
                (*item).selector = self.parse_idcselector_and_field(item, child.unwrap(), 0);
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
                // Child elements <field>.
                if is_schema(child, "field") {
                    while {
                        field = self.parse_idcselector_and_field(item, child.unwrap(), 1);
                        if !field.is_null() {
                            (*field).index = (*item).nb_fields;
                            (*item).nb_fields += 1;
                            if !last_field.is_null() {
                                (*last_field).next = field;
                            } else {
                                (*item).fields = field;
                            }
                            last_field = field;
                        }
                        child = child
                            .unwrap()
                            .next
                            .map(|node| XmlNodePtr::try_from(node).unwrap());

                        is_schema(child, "field")
                    } {}
                } else {
                    xml_schema_pcontent_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                        null_mut(),
                        node,
                        child.map(|child| child.into()),
                        None,
                        Some("(annotation?, (selector, field+))"),
                    );
                }
            }
            if let Some(child) = child {
                xml_schema_pcontent_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                    null_mut(),
                    node,
                    Some(child.into()),
                    None,
                    Some("(annotation?, (selector, field+))"),
                );
            }

            item
        }
    }

    /// Parses a XML Schema identity-constraint definition's
    /// <selector> and <field> elements.
    ///
    /// Returns the parsed identity-constraint definition.
    #[doc(alias = "xmlSchemaParseIDCSelectorAndField")]
    unsafe fn parse_idcselector_and_field(
        &mut self,
        idc: XmlSchemaIDCPtr,
        node: XmlNodePtr,
        is_field: i32,
    ) -> XmlSchemaIdcselectPtr {
        unsafe {
            // Check for illegal attributes.
            let mut attr = node.properties;
            while let Some(cur_attr) = attr {
                if let Some(ns) = cur_attr.ns {
                    if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                        xml_schema_pillegal_attr_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                            null_mut(),
                            cur_attr,
                        );
                    }
                } else if cur_attr.name.as_ref() != "id" && cur_attr.name.as_ref() != "xpath" {
                    xml_schema_pillegal_attr_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
                attr = cur_attr.next;
            }
            // Create the item.
            let item: XmlSchemaIdcselectPtr =
                xml_malloc(size_of::<XmlSchemaIdcselect>()) as XmlSchemaIdcselectPtr;
            if item.is_null() {
                xml_schema_perr_memory(
                    self,
                    "allocating a 'selector' of an identity-constraint definition",
                    None,
                );
                return null_mut();
            }
            memset(item as _, 0, size_of::<XmlSchemaIdcselect>());
            // Attribute "xpath" (mandatory).
            if let Some(attr) = xml_schema_get_prop_node(node, "xpath") {
                let xpath = self.get_node_content(Some(attr.into()));
                (*item).xpath = xml_dict_lookup(self.dict, xpath.as_ptr(), xpath.len() as i32);
                // URGENT TODO: "field"s have an other syntax than "selector"s.

                if xml_schema_check_cselector_xpath(self, idc, item, Some(attr), is_field) == -1 {
                    xml_schema_perr(
                        self,
                        Some(attr.into()),
                        XmlParserErrors::XmlSchemapInternal,
                        "Internal error: xmlSchemaParseIDCSelectorAndField, validating the XPath expression of a IDC selector.\n",
                        None,
                        None,
                    );
                }
            } else {
                xml_schema_pmissing_attr_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sAttrMissing,
                    null_mut(),
                    Some(node.into()),
                    Some("name"),
                    None,
                );
            }
            xml_schema_pval_attr_id(self, node, "id");
            // And now for the children...
            let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
            if is_schema(child, "annotation") {
                // Add the annotation to the parent IDC.
                xml_schema_add_annotation(
                    idc as XmlSchemaAnnotItemPtr,
                    self.parse_annotation(child.unwrap(), 1),
                );
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if let Some(child) = child {
                xml_schema_pcontent_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                    null_mut(),
                    node,
                    Some(child.into()),
                    None,
                    Some("(annotation?)"),
                );
            }

            item
        }
    }

    /// Parses the attribute "processContents" and "namespace"
    /// of a xsd:anyAttribute and xsd:any.
    /// *WARNING* this interface is highly subject to change
    ///
    /// Returns 0 if everything goes fine, a positive error code
    /// if something is not valid and -1 if an internal error occurs.
    #[doc(alias = "xmlSchemaParseWildcardNs")]
    unsafe fn parse_wildcard_ns(
        &mut self,
        _schema: XmlSchemaPtr,
        wildc: XmlSchemaWildcardPtr,
        node: XmlNodePtr,
    ) -> i32 {
        unsafe {
            let mut dictns_item: *const u8;
            let mut ret: i32 = 0;
            let mut tmp: XmlSchemaWildcardNsPtr;
            let mut last_ns: XmlSchemaWildcardNsPtr = null_mut();

            let pc = self.get_prop(node, "processContents");
            if pc.is_none() || pc.as_deref() == Some("strict") {
                (*wildc).process_contents = XML_SCHEMAS_ANY_STRICT;
            } else if pc.as_deref() == Some("skip") {
                (*wildc).process_contents = XML_SCHEMAS_ANY_SKIP;
            } else if pc.as_deref() == Some("lax") {
                (*wildc).process_contents = XML_SCHEMAS_ANY_LAX;
            } else {
                let pc = pc.unwrap();
                xml_schema_psimple_type_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sAttrInvalidValue,
                    null_mut(),
                    node.into(),
                    null_mut(),
                    Some("(strict | skip | lax)"),
                    Some(&pc),
                    None,
                    None,
                    None,
                );
                (*wildc).process_contents = XML_SCHEMAS_ANY_STRICT;
                ret = XmlParserErrors::XmlSchemapS4sAttrInvalidValue as i32;
            }
            // Build the namespace constraints.
            let attr = xml_schema_get_prop_node(node, "namespace");
            let ns = self.get_node_content(attr.map(|attr| attr.into()));
            if let Some(attr) = attr.filter(|_| ns != "##any") {
                if ns == "##other" {
                    (*wildc).neg_ns_set = xml_schema_new_wildcard_ns_constraint(self);
                    if (*wildc).neg_ns_set.is_null() {
                        return -1;
                    }
                    (*(*wildc).neg_ns_set).value = self.target_namespace;
                } else {
                    let mut cur = ns.as_str();
                    while !cur.is_empty() {
                        cur = cur.trim_start_matches(|c| xml_is_blank_char(c as u32));
                        let end = cur.trim_start_matches(|c| !xml_is_blank_char(c as u32));
                        if end.len() == cur.len() {
                            break;
                        }
                        let ns_item = &cur[..cur.len() - end.len()];
                        if ns_item == "##other" || ns_item == "##any" {
                            xml_schema_psimple_type_err(
                                self,
                                XmlParserErrors::XmlSchemapWildcardInvalidNsMember,
                                null_mut(),
                                attr.into(),
                                null_mut(),
                                Some(
                                    "((##any | ##other) | List of (xs:anyURI | (##targetNamespace | ##local)))",
                                ),
                                Some(ns_item),
                                None,
                                None,
                                None,
                            );
                            ret = XmlParserErrors::XmlSchemapWildcardInvalidNsMember as i32;
                        } else {
                            if ns_item == "##targetNamespace" {
                                dictns_item = self.target_namespace;
                            } else if ns_item == "##local" {
                                dictns_item = null_mut();
                            } else {
                                // Validate the item (anyURI).
                                dictns_item = xml_dict_lookup(
                                    self.dict,
                                    ns_item.as_ptr(),
                                    ns_item.len() as i32,
                                );
                                xml_schema_pval_attr_node_value(
                                    self,
                                    null_mut(),
                                    attr,
                                    dictns_item,
                                    xml_schema_get_built_in_type(
                                        XmlSchemaValType::XmlSchemasAnyURI,
                                    ),
                                );
                            }
                            // Avoid duplicate namespaces.
                            tmp = (*wildc).ns_set;
                            while !tmp.is_null() {
                                if dictns_item == (*tmp).value {
                                    break;
                                }
                                tmp = (*tmp).next;
                            }
                            if tmp.is_null() {
                                tmp = xml_schema_new_wildcard_ns_constraint(self);
                                if tmp.is_null() {
                                    return -1;
                                }
                                (*tmp).value = dictns_item;
                                (*tmp).next = null_mut();
                                if (*wildc).ns_set.is_null() {
                                    (*wildc).ns_set = tmp;
                                } else if !last_ns.is_null() {
                                    (*last_ns).next = tmp;
                                }
                                last_ns = tmp;
                            }
                        }
                        cur = end;
                    }
                }
            } else {
                (*wildc).any = 1;
            }

            ret
        }
    }

    /// Parse a XML schema Facet declaration
    /// *WARNING* this interface is highly subject to change
    ///
    /// Returns the new type structure or NULL in case of error
    #[doc(alias = "xmlSchemaParseFacet")]
    unsafe fn parse_facet(&mut self, schema: XmlSchemaPtr, node: XmlNodePtr) -> XmlSchemaFacetPtr {
        unsafe {
            if schema.is_null() {
                return null_mut();
            }

            let facet: XmlSchemaFacetPtr = xml_schema_new_facet();
            if facet.is_null() {
                xml_schema_perr_memory(self, "allocating facet", Some(node.into()));
                return null_mut();
            }
            (*facet).node = node.into();
            let Some(value) = self.get_prop(node, "value") else {
                let name = node.name().unwrap();
                xml_schema_perr2(
                    self,
                    Some(node.into()),
                    None,
                    XmlParserErrors::XmlSchemapFacetNoValue,
                    format!("Facet {name} has no value\n").as_str(),
                    Some(&name),
                    None,
                );
                xml_schema_free_facet(facet);
                return null_mut();
            };
            if is_schema(Some(node), "minInclusive") {
                (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetMinInclusive;
            } else if is_schema(Some(node), "minExclusive") {
                (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetMinExclusive;
            } else if is_schema(Some(node), "maxInclusive") {
                (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetMaxInclusive;
            } else if is_schema(Some(node), "maxExclusive") {
                (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetMaxExclusive;
            } else if is_schema(Some(node), "totalDigits") {
                (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetTotalDigits;
            } else if is_schema(Some(node), "fractionDigits") {
                (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetFractionDigits;
            } else if is_schema(Some(node), "pattern") {
                (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetPattern;
            } else if is_schema(Some(node), "enumeration") {
                (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetEnumeration;
            } else if is_schema(Some(node), "whiteSpace") {
                (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetWhitespace;
            } else if is_schema(Some(node), "length") {
                (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetLength;
            } else if is_schema(Some(node), "maxLength") {
                (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetMaxLength;
            } else if is_schema(Some(node), "minLength") {
                (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetMinLength;
            } else {
                let name = node.name().unwrap();
                xml_schema_perr2(
                    self,
                    Some(node.into()),
                    None,
                    XmlParserErrors::XmlSchemapUnknownFacetType,
                    format!("Unknown facet type {name}\n").as_str(),
                    Some(&name),
                    None,
                );
                xml_schema_free_facet(facet);
                return null_mut();
            }
            xml_schema_pval_attr_id(self, node, "id");
            (*facet).value = xml_dict_lookup(self.dict, value.as_ptr(), value.len() as i32);
            if (*facet).typ != XmlSchemaTypeType::XmlSchemaFacetPattern
                && (*facet).typ != XmlSchemaTypeType::XmlSchemaFacetEnumeration
            {
                let fixed = self.get_prop(node, "fixed");
                if fixed.as_deref() == Some("true") {
                    (*facet).fixed = 1;
                }
            }
            let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());

            if is_schema(child, "annotation") {
                (*facet).annot = self.parse_annotation(child.unwrap(), 1);
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if let Some(child) = child {
                let name = node.name().unwrap();
                xml_schema_perr2(
                    self,
                    Some(node.into()),
                    Some(child.into()),
                    XmlParserErrors::XmlSchemapUnknownFacetChild,
                    format!("Facet {name} has unexpected child content\n").as_str(),
                    Some(&name),
                    None,
                );
            }
            facet
        }
    }

    /// Parsea a XML schema <any> element. A particle and wildcard
    /// will be created (except if minOccurs==maxOccurs==0, in this case
    /// nothing will be created).
    /// *WARNING* this interface is highly subject to change
    ///
    /// Returns the particle or NULL in case of error or if minOccurs==maxOccurs==0
    #[doc(alias = "xmlSchemaParseAny")]
    unsafe fn parse_any(&mut self, schema: XmlSchemaPtr, node: XmlNodePtr) -> XmlSchemaParticlePtr {
        unsafe {
            let mut annot: XmlSchemaAnnotPtr = null_mut();

            if schema.is_null() {
                return null_mut();
            }
            // Check for illegal attributes.
            let mut attr = node.properties;
            while let Some(cur_attr) = attr {
                if let Some(ns) = cur_attr.ns {
                    if ns.href().as_deref() == Some(XML_SCHEMA_NS.to_str().unwrap()) {
                        xml_schema_pillegal_attr_err(
                            self,
                            XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                            null_mut(),
                            cur_attr,
                        );
                    }
                } else if cur_attr.name.as_ref() != "id"
                    && cur_attr.name.as_ref() != "minOccurs"
                    && cur_attr.name.as_ref() != "maxOccurs"
                    && cur_attr.name.as_ref() != "namespace"
                    && cur_attr.name.as_ref() != "processContents"
                {
                    xml_schema_pillegal_attr_err(
                        self,
                        XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                        null_mut(),
                        cur_attr,
                    );
                }
                attr = cur_attr.next;
            }
            xml_schema_pval_attr_id(self, node, "id");
            // minOccurs/maxOccurs.
            let max: i32 = xml_get_max_occurs(
                self,
                node,
                0,
                UNBOUNDED as _,
                1,
                "(xs:nonNegativeInteger | unbounded)",
            );
            let min: i32 = xml_get_min_occurs(self, node, 0, -1, 1, "xs:nonNegativeInteger");
            xml_schema_pcheck_particle_correct_2(self, null_mut(), node, min, max);
            // Create & parse the wildcard.
            let wild: XmlSchemaWildcardPtr = xml_schema_add_wildcard(
                self,
                schema,
                XmlSchemaTypeType::XmlSchemaTypeAny,
                Some(node),
            );
            if wild.is_null() {
                return null_mut();
            }
            self.parse_wildcard_ns(schema, wild, node);
            // And now for the children...
            let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
            if is_schema(child, "annotation") {
                annot = self.parse_annotation(child.unwrap(), 1);
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            if let Some(child) = child {
                xml_schema_pcontent_err(
                    self,
                    XmlParserErrors::XmlSchemapS4sElemNotAllowed,
                    null_mut(),
                    node,
                    Some(child.into()),
                    None,
                    Some("(annotation?)"),
                );
            }
            // No component if minOccurs==maxOccurs==0.
            if min == 0 && max == 0 {
                // Don't free the wildcard, since it's already on the list.
                return null_mut();
            }
            // Create the particle.
            let particle: XmlSchemaParticlePtr =
                xml_schema_add_particle(self, Some(node), min, max);
            if particle.is_null() {
                return null_mut();
            }
            (*particle).annot = annot;
            (*particle).children = wild as XmlSchemaTreeItemPtr;

            particle
        }
    }
}
