use std::{
    ffi::{CStr, CString, c_void},
    ptr::{null, null_mut},
};

use crate::{
    dict::xml_dict_lookup,
    error::XmlParserErrors,
    libxml::{
        schemas_internals::{
            XML_SCHEMAS_ATTR_FIXED, XML_SCHEMAS_ATTR_GLOBAL, XML_SCHEMAS_ATTRGROUP_HAS_REFS,
            XML_SCHEMAS_BLOCK_DEFAULT_EXTENSION, XML_SCHEMAS_BLOCK_DEFAULT_RESTRICTION,
            XML_SCHEMAS_BLOCK_DEFAULT_SUBSTITUTION, XML_SCHEMAS_ELEM_ABSTRACT,
            XML_SCHEMAS_ELEM_BLOCK_EXTENSION, XML_SCHEMAS_ELEM_BLOCK_RESTRICTION,
            XML_SCHEMAS_ELEM_BLOCK_SUBSTITUTION, XML_SCHEMAS_ELEM_FINAL_EXTENSION,
            XML_SCHEMAS_ELEM_FINAL_RESTRICTION, XML_SCHEMAS_ELEM_FIXED, XML_SCHEMAS_ELEM_GLOBAL,
            XML_SCHEMAS_ELEM_NILLABLE, XML_SCHEMAS_ELEM_TOPLEVEL,
            XML_SCHEMAS_FINAL_DEFAULT_EXTENSION, XML_SCHEMAS_FINAL_DEFAULT_LIST,
            XML_SCHEMAS_FINAL_DEFAULT_RESTRICTION, XML_SCHEMAS_FINAL_DEFAULT_UNION,
            XML_SCHEMAS_INCLUDING_CONVERT_NS, XML_SCHEMAS_QUALIF_ATTR, XML_SCHEMAS_QUALIF_ELEM,
            XML_SCHEMAS_TYPE_ABSTRACT, XML_SCHEMAS_TYPE_BLOCK_EXTENSION,
            XML_SCHEMAS_TYPE_BLOCK_RESTRICTION, XML_SCHEMAS_TYPE_DERIVATION_METHOD_RESTRICTION,
            XML_SCHEMAS_TYPE_FINAL_EXTENSION, XML_SCHEMAS_TYPE_FINAL_LIST,
            XML_SCHEMAS_TYPE_FINAL_RESTRICTION, XML_SCHEMAS_TYPE_FINAL_UNION,
            XML_SCHEMAS_TYPE_GLOBAL, XML_SCHEMAS_TYPE_MIXED, XmlSchemaAnnotPtr,
            XmlSchemaContentType, XmlSchemaTypeType, XmlSchemaValType, xml_schema_free_annot,
        },
        xmlschemas::{
            XML_SCHEMA_INSTANCE_NS, XML_SCHEMA_NS, XML_SCHEMA_SCHEMA_IMPORT,
            XML_SCHEMA_SCHEMA_INCLUDE, XML_SCHEMA_SCHEMA_MAIN, XML_SCHEMA_SCHEMA_REDEFINE,
            XmlSchemaAbstractCtxtPtr, XmlSchemaBucketPtr, can_parse_schema, xml_get_boolean_prop,
            xml_get_max_occurs, xml_get_min_occurs, xml_schema_add_attribute,
            xml_schema_add_attribute_group_definition, xml_schema_add_element,
            xml_schema_add_item_size, xml_schema_add_model_group_definition,
            xml_schema_add_notation, xml_schema_add_particle, xml_schema_add_schema_doc,
            xml_schema_add_type, xml_schema_build_absolute_uri, xml_schema_check_reference,
            xml_schema_construction_ctxt_create, xml_schema_construction_ctxt_free,
            xml_schema_fixup_components, xml_schema_get_prop_node, xml_schema_get_prop_node_ns,
            xml_schema_new_annot, xml_schema_new_qname_ref, xml_schema_parse_any_attribute,
            xml_schema_parse_complex_content, xml_schema_parse_idc, xml_schema_parse_list,
            xml_schema_parse_local_attributes, xml_schema_parse_model_group,
            xml_schema_parse_model_group_def_ref, xml_schema_parse_new_doc,
            xml_schema_parse_restriction, xml_schema_parse_simple_content, xml_schema_parse_union,
            xml_schema_pcheck_particle_correct_2, xml_schema_pget_bool_node_value,
            xml_schema_pval_attr, xml_schema_pval_attr_block_final,
            xml_schema_pval_attr_form_default, xml_schema_pval_attr_id, xml_schema_pval_attr_node,
            xml_schema_pval_attr_node_qname, xml_schema_pval_attr_qname,
        },
        xmlschemastypes::{
            xml_schema_get_built_in_type, xml_schema_get_predefined_type, xml_schema_init_types,
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
            XmlSchemaElementPtr, XmlSchemaIDCPtr, XmlSchemaParticlePtr, XmlSchemaQNameRefPtr,
            XmlSchemaTreeItemPtr,
        },
        schema::xml_schema_free,
    },
};

use super::{
    context::XmlSchemaParserCtxt,
    error::{
        xml_schema_custom_warning, xml_schema_pcontent_err, xml_schema_pcustom_err,
        xml_schema_pcustom_err_ext, xml_schema_perr2, xml_schema_pillegal_attr_err,
        xml_schema_pmissing_attr_err,
    },
    is_schema,
    item_list::XmlSchemaItemListPtr,
    items::{
        XmlSchemaAttributeGroupPtr, XmlSchemaAttributePtr, XmlSchemaBasicItemPtr,
        XmlSchemaModelGroupDefPtr, XmlSchemaNotationPtr, XmlSchemaTypePtr,
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
    pub(crate) unsafe fn parse_annotation(
        &mut self,
        node: XmlNodePtr,
        needed: i32,
    ) -> XmlSchemaAnnotPtr {
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
                if (now.ns.is_none() && !xml_str_equal(now.name, c"id".as_ptr() as _))
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
                        if (now.ns.is_none() && !xml_str_equal(now.name, c"source".as_ptr() as _))
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
                                || (xml_str_equal(cur_attr.name, c"lang".as_ptr() as _)
                                    && !xml_str_equal(ns.href, XML_XML_NAMESPACE.as_ptr() as _))
                            {
                                xml_schema_pillegal_attr_err(
                                    self,
                                    XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                                    null_mut(),
                                    cur_attr,
                                );
                            }
                        } else if !xml_str_equal(cur_attr.name, c"source".as_ptr() as _) {
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
                    if let Some(attr) = xml_schema_get_prop_node_ns(
                        cur_node,
                        XML_XML_NAMESPACE.as_ptr() as _,
                        "lang",
                    ) {
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
    pub(crate) unsafe fn parse_import(&mut self, schema: XmlSchemaPtr, node: XmlNodePtr) -> i32 {
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
                } else if !xml_str_equal(cur_attr.name, c"id".as_ptr() as _)
                    && !xml_str_equal(cur_attr.name, c"namespace".as_ptr() as _)
                    && !xml_str_equal(cur_attr.name, c"schemaLocation".as_ptr() as _)
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
                ret = xml_schema_parse_new_doc(self, schema, bucket);
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
                xml_schema_parse_new_doc(self, schema, bucket);
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

            // exit_error:
            // return (self.err);
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
                } else if !xml_str_equal(cur_attr.name, c"id".as_ptr() as _)
                    && !xml_str_equal(cur_attr.name, c"schemaLocation".as_ptr() as _)
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
            // exit_error:
            //     return (self.err);
            // exit_failure:
            //     return -1;
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
    pub(crate) unsafe fn parse_complex_type(
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
                } else if xml_str_equal(cur_attr.name, c"id".as_ptr() as _) {
                    // Attribute "id".
                    xml_schema_pval_attr_id(self, node, "id");
                } else if xml_str_equal(cur_attr.name, c"mixed".as_ptr() as _) {
                    // Attribute "mixed".
                    if xml_schema_pget_bool_node_value(self, null_mut(), cur_attr.into()) != 0 {
                        (*typ).flags |= XML_SCHEMAS_TYPE_MIXED;
                    }
                } else if top_level != 0 {
                    // Attributes of global complex type definitions.
                    if xml_str_equal(cur_attr.name, c"name".as_ptr() as _) {
                        // Pass.
                    } else if xml_str_equal(cur_attr.name, c"abstract".as_ptr() as _) {
                        // Attribute "abstract".
                        if xml_schema_pget_bool_node_value(self, null_mut(), cur_attr.into()) != 0 {
                            (*typ).flags |= XML_SCHEMAS_TYPE_ABSTRACT;
                        }
                    } else if xml_str_equal(cur_attr.name, c"final".as_ptr() as _) {
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
                    } else if xml_str_equal(cur_attr.name, c"block".as_ptr() as _) {
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
                xml_schema_parse_simple_content(
                    self,
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
                xml_schema_parse_complex_content(
                    self,
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
                    (*typ).subtypes = xml_schema_parse_model_group(
                        self,
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
                    (*typ).subtypes = xml_schema_parse_model_group(
                        self,
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
                    (*typ).subtypes = xml_schema_parse_model_group(
                        self,
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
                        xml_schema_parse_model_group_def_ref(self, schema, child.unwrap())
                            as XmlSchemaTypePtr;
                    // Note that the reference will be resolved in
                    // xmlSchemaResolveTypeReferences();
                    child = child
                        .unwrap()
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                }
                // Parse attribute decls/refs.
                if xml_schema_parse_local_attributes(
                    self,
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
                    (*typ).attribute_wildcard =
                        xml_schema_parse_any_attribute(self, schema, child.unwrap());
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
    pub(crate) unsafe fn parse_simple_type(
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
                    } else if !xml_str_equal(cur_attr.name, c"id".as_ptr() as _) {
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
                    } else if !xml_str_equal(cur_attr.name, c"id".as_ptr() as _)
                        && !xml_str_equal(cur_attr.name, c"name".as_ptr() as _)
                        && !xml_str_equal(cur_attr.name, c"final".as_ptr() as _)
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
                xml_schema_parse_restriction(
                    self,
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
                xml_schema_parse_list(self, schema, child.unwrap());
                child = child
                    .unwrap()
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            } else if is_schema(child, "union") {
                xml_schema_parse_union(self, schema, child.unwrap());
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
    pub(crate) unsafe fn parse_element(
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
                            } else if xml_str_equal(cur_attr.name, c"ref".as_ptr() as _)
                                || xml_str_equal(cur_attr.name, c"name".as_ptr() as _)
                                || xml_str_equal(cur_attr.name, c"id".as_ptr() as _)
                                || xml_str_equal(cur_attr.name, c"maxOccurs".as_ptr() as _)
                                || xml_str_equal(cur_attr.name, c"minOccurs".as_ptr() as _)
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
                    } else if !xml_str_equal(cur_attr.name, c"name".as_ptr() as _)
                        && !xml_str_equal(cur_attr.name, c"type".as_ptr() as _)
                        && !xml_str_equal(cur_attr.name, c"id".as_ptr() as _)
                        && !xml_str_equal(cur_attr.name, c"default".as_ptr() as _)
                        && !xml_str_equal(cur_attr.name, c"fixed".as_ptr() as _)
                        && !xml_str_equal(cur_attr.name, c"block".as_ptr() as _)
                        && !xml_str_equal(cur_attr.name, c"nillable".as_ptr() as _)
                    {
                        if top_level == 0 {
                            if !xml_str_equal(cur_attr.name, c"maxOccurs".as_ptr() as _)
                                && !xml_str_equal(cur_attr.name, c"minOccurs".as_ptr() as _)
                                && !xml_str_equal(cur_attr.name, c"form".as_ptr() as _)
                            {
                                xml_schema_pillegal_attr_err(
                                    self,
                                    XmlParserErrors::XmlSchemapS4sAttrNotAllowed,
                                    null_mut(),
                                    cur_attr,
                                );
                            }
                        } else if !xml_str_equal(cur_attr.name, c"final".as_ptr() as _)
                            && !xml_str_equal(cur_attr.name, c"abstract".as_ptr() as _)
                            && !xml_str_equal(cur_attr.name, c"substitutionGroup".as_ptr() as _)
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
                        cur_idc = xml_schema_parse_idc(
                            self,
                            schema,
                            child.unwrap(),
                            XmlSchemaTypeType::XmlSchemaTypeIDCUnique,
                            (*decl).target_namespace,
                        );
                    } else if is_schema(child, "key") {
                        cur_idc = xml_schema_parse_idc(
                            self,
                            schema,
                            child.unwrap(),
                            XmlSchemaTypeType::XmlSchemaTypeIDCKey,
                            (*decl).target_namespace,
                        );
                    } else if is_schema(child, "keyref") {
                        cur_idc = xml_schema_parse_idc(
                            self,
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
                } else if !xml_str_equal(cur_attr.name, c"name".as_ptr() as _)
                    && !xml_str_equal(cur_attr.name, c"id".as_ptr() as _)
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
            if xml_schema_parse_local_attributes(
                self,
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
                (*ret).attribute_wildcard =
                    xml_schema_parse_any_attribute(self, schema, child.unwrap());
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
                } else if !xml_str_equal(cur_attr.name, c"name".as_ptr() as _)
                    && !xml_str_equal(cur_attr.name, c"id".as_ptr() as _)
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
                (*item).children = xml_schema_parse_model_group(
                    self,
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
                (*item).children = xml_schema_parse_model_group(
                    self,
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
                (*item).children = xml_schema_parse_model_group(
                    self,
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
}
