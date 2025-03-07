use std::{
    ffi::{CStr, CString},
    ptr::{null, null_mut},
};

use crate::{
    dict::xml_dict_lookup,
    error::XmlParserErrors,
    libxml::{
        schemas_internals::{
            XML_SCHEMAS_BLOCK_DEFAULT_EXTENSION, XML_SCHEMAS_BLOCK_DEFAULT_RESTRICTION,
            XML_SCHEMAS_BLOCK_DEFAULT_SUBSTITUTION, XML_SCHEMAS_FINAL_DEFAULT_EXTENSION,
            XML_SCHEMAS_FINAL_DEFAULT_LIST, XML_SCHEMAS_FINAL_DEFAULT_RESTRICTION,
            XML_SCHEMAS_FINAL_DEFAULT_UNION, XML_SCHEMAS_INCLUDING_CONVERT_NS,
            XML_SCHEMAS_QUALIF_ATTR, XML_SCHEMAS_QUALIF_ELEM, XmlSchemaAnnotPtr, XmlSchemaValType,
            xml_schema_free_annot,
        },
        xmlschemas::{
            XML_SCHEMA_NS, XML_SCHEMA_SCHEMA_IMPORT, XML_SCHEMA_SCHEMA_INCLUDE,
            XML_SCHEMA_SCHEMA_MAIN, XML_SCHEMA_SCHEMA_REDEFINE, XmlSchemaAbstractCtxtPtr,
            XmlSchemaBucketPtr, can_parse_schema, xml_schema_add_schema_doc,
            xml_schema_build_absolute_uri, xml_schema_construction_ctxt_create,
            xml_schema_construction_ctxt_free, xml_schema_fixup_components,
            xml_schema_get_prop_node, xml_schema_get_prop_node_ns, xml_schema_new_annot,
            xml_schema_parse_attribute_group_definition, xml_schema_parse_complex_type,
            xml_schema_parse_element, xml_schema_parse_global_attribute,
            xml_schema_parse_model_group_definition, xml_schema_parse_new_doc,
            xml_schema_parse_notation, xml_schema_parse_simple_type, xml_schema_pval_attr,
            xml_schema_pval_attr_block_final, xml_schema_pval_attr_form_default,
            xml_schema_pval_attr_id, xml_schema_pval_attr_node,
        },
        xmlschemastypes::{xml_schema_get_built_in_type, xml_schema_init_types},
        xmlstring::xml_str_equal,
    },
    tree::{NodeCommon, XML_XML_NAMESPACE, XmlNodePtr},
    uri::build_uri,
    xmlschemas::{
        error::{xml_schema_custom_err, xml_schema_internal_err, xml_schema_psimple_type_err},
        schema::xml_schema_free,
    },
};

use super::{
    context::XmlSchemaParserCtxt,
    error::{
        xml_schema_custom_warning, xml_schema_pcontent_err, xml_schema_pcustom_err,
        xml_schema_pcustom_err_ext, xml_schema_pillegal_attr_err, xml_schema_pmissing_attr_err,
    },
    is_schema,
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
                        xml_schema_parse_simple_type(self, schema, child.unwrap(), 1);
                    } else if is_schema(child, "complexType") {
                        xml_schema_parse_complex_type(self, schema, child.unwrap(), 1);
                    // hasRedefinitions = 1;
                    } else if is_schema(child, "group") {
                        // hasRedefinitions = 1;
                        xml_schema_parse_model_group_definition(self, schema, child.unwrap());
                    } else if is_schema(child, "attributeGroup") {
                        // hasRedefinitions = 1;
                        xml_schema_parse_attribute_group_definition(self, schema, child.unwrap());
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
}
