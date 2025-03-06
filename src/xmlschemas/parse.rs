use std::{
    ffi::{CStr, CString},
    ptr::null_mut,
};

use crate::{
    error::XmlParserErrors,
    libxml::{
        schemas_internals::{
            XML_SCHEMAS_BLOCK_DEFAULT_EXTENSION, XML_SCHEMAS_BLOCK_DEFAULT_RESTRICTION,
            XML_SCHEMAS_BLOCK_DEFAULT_SUBSTITUTION, XML_SCHEMAS_FINAL_DEFAULT_EXTENSION,
            XML_SCHEMAS_FINAL_DEFAULT_LIST, XML_SCHEMAS_FINAL_DEFAULT_RESTRICTION,
            XML_SCHEMAS_FINAL_DEFAULT_UNION, XML_SCHEMAS_QUALIF_ATTR, XML_SCHEMAS_QUALIF_ELEM,
            XmlSchemaAnnotPtr, XmlSchemaValType, xml_schema_free_annot,
        },
        xmlschemas::{
            XML_SCHEMA_NS, XML_SCHEMA_SCHEMA_MAIN, XmlSchemaAbstractCtxtPtr, XmlSchemaBucketPtr,
            xml_schema_add_schema_doc, xml_schema_construction_ctxt_create,
            xml_schema_construction_ctxt_free, xml_schema_fixup_components,
            xml_schema_get_prop_node, xml_schema_parse_annotation,
            xml_schema_parse_attribute_group_definition, xml_schema_parse_complex_type,
            xml_schema_parse_element, xml_schema_parse_global_attribute, xml_schema_parse_import,
            xml_schema_parse_include, xml_schema_parse_model_group_definition,
            xml_schema_parse_notation, xml_schema_parse_redefine, xml_schema_parse_simple_type,
            xml_schema_pval_attr_block_final, xml_schema_pval_attr_form_default,
            xml_schema_pval_attr_id, xml_schema_pval_attr_node,
        },
        xmlschemastypes::{xml_schema_get_built_in_type, xml_schema_init_types},
        xmlstring::xml_str_equal,
    },
    tree::XmlNodePtr,
    xmlschemas::{
        error::{xml_schema_custom_err, xml_schema_internal_err, xml_schema_psimple_type_err},
        schema::xml_schema_free,
    },
};

use super::{
    context::XmlSchemaParserCtxt, error::xml_schema_pcontent_err, is_schema, schema::XmlSchemaPtr,
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
            let mut val: *const u8;
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
                //     res = xmlSchemaPValAttrNode(ctxt, null_mut(), NULL, attr, xmlSchemaGetBuiltInType(XML_SCHEMAS_TOKEN), &val);
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
                        val = self.get_node_content(Some(attr.into()));
                        res = xml_schema_pval_attr_form_default(
                            val,
                            &raw mut (*schema).flags,
                            XML_SCHEMAS_QUALIF_ELEM,
                        );
                        if res == -1 {
                            break 'exit_failure;
                        };
                        if res != 0 {
                            let val = CStr::from_ptr(val as *const i8).to_string_lossy();
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
                        val = self.get_node_content(Some(attr.into()));
                        res = xml_schema_pval_attr_form_default(
                            val,
                            &raw mut (*schema).flags,
                            XML_SCHEMAS_QUALIF_ATTR,
                        );
                        if res == -1 {
                            break 'exit_failure;
                        };
                        if res != 0 {
                            let val = CStr::from_ptr(val as *const i8).to_string_lossy();
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
                        val = self.get_node_content(Some(attr.into()));
                        res = xml_schema_pval_attr_block_final(
                            val,
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
                            let val = CStr::from_ptr(val as *const i8).to_string_lossy();
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
                        val = self.get_node_content(Some(attr.into()));
                        res = xml_schema_pval_attr_block_final(
                            val,
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
                            let val = CStr::from_ptr(val as *const i8).to_string_lossy();
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
            'exit_failure: {
                'exit: {
                    while is_schema(child, "include")
                        || is_schema(child, "import")
                        || is_schema(child, "redefine")
                        || is_schema(child, "annotation")
                    {
                        if is_schema(child, "annotation") {
                            annot = xml_schema_parse_annotation(self, child.unwrap(), 1);
                            if (*schema).annot.is_null() {
                                (*schema).annot = annot;
                            } else {
                                xml_schema_free_annot(annot);
                            }
                        } else if is_schema(child, "import") {
                            tmp_old_errs = self.nberrors;
                            res = xml_schema_parse_import(self, schema, child.unwrap());
                            if res == -1 {
                                break 'exit_failure;
                            };
                            if self.stop != 0 {
                                break 'exit;
                            }
                            if tmp_old_errs != self.nberrors {
                                break 'exit;
                            }
                        } else if is_schema(child, "include") {
                            tmp_old_errs = self.nberrors;
                            res = xml_schema_parse_include(self, schema, child.unwrap());
                            if res == -1 {
                                break 'exit_failure;
                            };
                            if self.stop != 0 {
                                break 'exit;
                            }
                            if tmp_old_errs != self.nberrors {
                                break 'exit;
                            }
                        } else if is_schema(child, "redefine") {
                            tmp_old_errs = self.nberrors;
                            res = xml_schema_parse_redefine(self, schema, child.unwrap());
                            if res == -1 {
                                break 'exit_failure;
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
                            xml_schema_parse_complex_type(self, schema, cur_node, 1);
                            child = cur_node
                                .next
                                .map(|node| XmlNodePtr::try_from(node).unwrap());
                        } else if is_schema(child, "simpleType") {
                            xml_schema_parse_simple_type(self, schema, cur_node, 1);
                            child = cur_node
                                .next
                                .map(|node| XmlNodePtr::try_from(node).unwrap());
                        } else if is_schema(child, "element") {
                            xml_schema_parse_element(self, schema, cur_node, null_mut(), 1);
                            child = cur_node
                                .next
                                .map(|node| XmlNodePtr::try_from(node).unwrap());
                        } else if is_schema(child, "attribute") {
                            xml_schema_parse_global_attribute(self, schema, cur_node);
                            child = cur_node
                                .next
                                .map(|node| XmlNodePtr::try_from(node).unwrap());
                        } else if is_schema(child, "attributeGroup") {
                            xml_schema_parse_attribute_group_definition(self, schema, cur_node);
                            child = cur_node
                                .next
                                .map(|node| XmlNodePtr::try_from(node).unwrap());
                        } else if is_schema(child, "group") {
                            xml_schema_parse_model_group_definition(self, schema, cur_node);
                            child = cur_node
                                .next
                                .map(|node| XmlNodePtr::try_from(node).unwrap());
                        } else if is_schema(child, "notation") {
                            xml_schema_parse_notation(self, schema, cur_node);
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
                            annot = xml_schema_parse_annotation(self, child.unwrap(), 1);
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
                return res;
            }
            // exit_failure:
            -1
        }
    }
}
