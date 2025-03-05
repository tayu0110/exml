use std::{
    ffi::{CStr, c_void},
    ptr::{drop_in_place, null, null_mut},
};

use crate::{
    dict::{XmlDictPtr, xml_dict_free, xml_dict_reference},
    hash::{XmlHashTablePtr, xml_hash_free, xml_hash_lookup, xml_hash_size},
    libxml::{
        schemas_internals::{
            XML_SCHEMAS_BLOCK_DEFAULT_EXTENSION, XML_SCHEMAS_BLOCK_DEFAULT_RESTRICTION,
            XML_SCHEMAS_BLOCK_DEFAULT_SUBSTITUTION, XML_SCHEMAS_FINAL_DEFAULT_EXTENSION,
            XML_SCHEMAS_FINAL_DEFAULT_LIST, XML_SCHEMAS_FINAL_DEFAULT_RESTRICTION,
            XML_SCHEMAS_FINAL_DEFAULT_UNION, XML_SCHEMAS_QUALIF_ATTR, XML_SCHEMAS_QUALIF_ELEM,
            XmlSchemaAnnotPtr, XmlSchemaTypeType, xml_schema_free_annot,
        },
        xmlschemas::{
            XML_SCHEMA_NS, XML_SCHEMAS_NO_NAMESPACE, XmlSchemaBucketPtr, XmlSchemaImportPtr,
            xml_schema_bucket_free,
        },
        xmlschemastypes::xml_schema_get_predefined_type,
        xmlstring::xml_str_equal,
    },
    tree::XmlDocPtr,
    xmlschemas::item_list::{XmlSchemaItemListPtr, xml_schema_item_list_free},
};

use super::{
    context::XmlSchemaParserCtxt,
    items::{
        XmlSchemaAttributeGroupPtr, XmlSchemaAttributePtr, XmlSchemaBasicItemPtr,
        XmlSchemaElementPtr, XmlSchemaIDCPtr, XmlSchemaModelGroupDefPtr, XmlSchemaNotationPtr,
        XmlSchemaTypePtr,
    },
};

#[doc(alias = "xmlSchemaPtr")]
pub type XmlSchemaPtr = *mut XmlSchema;
/// A Schemas definition
#[doc(alias = "xmlSchema")]
#[repr(C)]
pub struct XmlSchema {
    pub(crate) name: *const u8,             /* schema name */
    pub(crate) target_namespace: *const u8, /* the target namespace */
    pub(crate) version: *const u8,
    pub(crate) id: *const u8, /* Obsolete */
    pub(crate) doc: Option<XmlDocPtr>,
    pub(crate) annot: XmlSchemaAnnotPtr,
    pub(crate) flags: i32,

    pub(crate) type_decl: XmlHashTablePtr,
    pub(crate) attr_decl: XmlHashTablePtr,
    pub(crate) attrgrp_decl: XmlHashTablePtr,
    pub(crate) elem_decl: XmlHashTablePtr,
    pub(crate) nota_decl: XmlHashTablePtr,

    pub(crate) schemas_imports: XmlHashTablePtr,

    pub(crate) _private: *mut c_void, /* unused by the library for users or bindings */
    pub(crate) group_decl: XmlHashTablePtr,
    pub(crate) dict: XmlDictPtr,
    pub(crate) includes: *mut c_void, /* the includes, this is opaque for now */
    pub(crate) preserve: i32,         /* whether to free the document */
    pub(crate) counter: i32,          /* used to give anonymous components unique names */
    pub(crate) idc_def: XmlHashTablePtr, /* All identity-constraint defs. */
    pub(crate) volatiles: *mut c_void, /* Obsolete */
}

impl XmlSchema {
    /// Lookup a type in the schemas or the predefined types
    ///
    /// Returns the group definition or NULL if not found.
    #[doc(alias = "xmlSchemaGetType")]
    pub(crate) unsafe fn get_type(&self, name: *const u8, ns_name: *const u8) -> XmlSchemaTypePtr {
        unsafe {
            let mut ret: XmlSchemaTypePtr = null_mut();

            if name.is_null() {
                return null_mut();
            }
            // First try the built-in types.
            if !ns_name.is_null() && xml_str_equal(ns_name, XML_SCHEMA_NS.as_ptr() as _) {
                ret = xml_schema_get_predefined_type(
                    CStr::from_ptr(name as *const i8).to_string_lossy().as_ref(),
                    CStr::from_ptr(ns_name as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                );
                if !ret.is_null() {
                    return ret;
                }
                // Note that we try the parsed schemas as well here
                // since one might have parsed the S4S, which contain more
                // than the built-in types.
                // TODO: Can we optimize this?
            }
            if xml_str_equal(ns_name, self.target_namespace) {
                ret = xml_hash_lookup(self.type_decl, name) as _;
                if !ret.is_null() {
                    return ret;
                }
            }
            if xml_hash_size(self.schemas_imports) > 1 {
                let import: XmlSchemaImportPtr = if ns_name.is_null() {
                    xml_hash_lookup(self.schemas_imports, XML_SCHEMAS_NO_NAMESPACE.as_ptr() as _)
                        as _
                } else {
                    xml_hash_lookup(self.schemas_imports, ns_name) as _
                };
                if import.is_null() {
                    return ret;
                }
                ret = xml_hash_lookup((*(*import).schema).type_decl, name) as _;
            };

            ret
        }
    }

    pub(crate) unsafe fn get_idc(&self, name: *const u8, ns_name: *const u8) -> XmlSchemaIDCPtr {
        unsafe {
            let mut ret: XmlSchemaIDCPtr = null_mut();

            if name.is_null() {
                return null_mut();
            }
            if xml_str_equal(ns_name, self.target_namespace) {
                ret = xml_hash_lookup(self.idc_def, name) as _;
                if !ret.is_null() {
                    return ret;
                }
            }
            if xml_hash_size(self.schemas_imports) > 1 {
                let import: XmlSchemaImportPtr = if ns_name.is_null() {
                    xml_hash_lookup(self.schemas_imports, XML_SCHEMAS_NO_NAMESPACE.as_ptr() as _)
                        as _
                } else {
                    xml_hash_lookup(self.schemas_imports, ns_name) as _
                };
                if import.is_null() {
                    return ret;
                }
                ret = xml_hash_lookup((*(*import).schema).idc_def, name) as _;
            };
            ret
        }
    }

    /// Lookup a global element declaration in the schema.
    ///
    /// Returns the element declaration or NULL if not found.
    #[doc(alias = "xmlSchemaGetElem")]
    pub(crate) unsafe fn get_elem(
        &self,
        name: *const u8,
        ns_name: *const u8,
    ) -> XmlSchemaElementPtr {
        unsafe {
            let mut ret: XmlSchemaElementPtr = null_mut();

            if name.is_null() {
                return null_mut();
            }
            if xml_str_equal(ns_name, self.target_namespace) {
                ret = xml_hash_lookup(self.elem_decl, name) as _;
                if !ret.is_null() {
                    return ret;
                }
            }
            if xml_hash_size(self.schemas_imports) > 1 {
                let import: XmlSchemaImportPtr = if ns_name.is_null() {
                    xml_hash_lookup(self.schemas_imports, XML_SCHEMAS_NO_NAMESPACE.as_ptr() as _)
                        as _
                } else {
                    xml_hash_lookup(self.schemas_imports, ns_name) as _
                };
                if import.is_null() {
                    return ret;
                }
                ret = xml_hash_lookup((*(*import).schema).elem_decl, name) as _;
            };

            ret
        }
    }

    /// Lookup a an attribute in the schema or imported schemas
    ///
    /// Returns the attribute declaration or NULL if not found.
    #[doc(alias = "xmlSchemaGetAttributeDecl")]
    pub(crate) unsafe fn get_attribute_decl(
        &self,
        name: *const u8,
        ns_name: *const u8,
    ) -> XmlSchemaAttributePtr {
        unsafe {
            let mut ret: XmlSchemaAttributePtr = null_mut();

            if name.is_null() {
                return null_mut();
            }
            if xml_str_equal(ns_name, self.target_namespace) {
                ret = xml_hash_lookup(self.attr_decl, name) as _;
                if !ret.is_null() {
                    return ret;
                }
            }
            if xml_hash_size(self.schemas_imports) > 1 {
                let import: XmlSchemaImportPtr = if ns_name.is_null() {
                    xml_hash_lookup(self.schemas_imports, XML_SCHEMAS_NO_NAMESPACE.as_ptr() as _)
                        as _
                } else {
                    xml_hash_lookup(self.schemas_imports, ns_name) as _
                };
                if import.is_null() {
                    return ret;
                }
                ret = xml_hash_lookup((*(*import).schema).attr_decl, name) as _;
            };

            ret
        }
    }

    /// Lookup a group in the schema or imported schemas
    ///
    /// Returns the group definition or NULL if not found.
    #[doc(alias = "xmlSchemaGetGroup")]
    pub(crate) unsafe fn get_group(
        &self,
        name: *const u8,
        ns_name: *const u8,
    ) -> XmlSchemaModelGroupDefPtr {
        unsafe {
            let mut ret: XmlSchemaModelGroupDefPtr = null_mut();

            if name.is_null() {
                return null_mut();
            }
            if xml_str_equal(ns_name, self.target_namespace) {
                ret = xml_hash_lookup(self.group_decl, name) as _;
                if !ret.is_null() {
                    return ret;
                }
            }
            if xml_hash_size(self.schemas_imports) > 1 {
                let import: XmlSchemaImportPtr = if ns_name.is_null() {
                    xml_hash_lookup(self.schemas_imports, XML_SCHEMAS_NO_NAMESPACE.as_ptr() as _)
                        as _
                } else {
                    xml_hash_lookup(self.schemas_imports, ns_name) as _
                };
                if import.is_null() {
                    return ret;
                }
                ret = xml_hash_lookup((*(*import).schema).group_decl, name) as _;
            };

            ret
        }
    }

    /// Lookup a an attribute group in the schema or imported schemas
    ///
    /// Returns the attribute group definition or NULL if not found.
    #[doc(alias = "xmlSchemaGetAttributeGroup")]
    pub(crate) unsafe fn get_attribute_group(
        &self,
        name: *const u8,
        ns_name: *const u8,
    ) -> XmlSchemaAttributeGroupPtr {
        unsafe {
            let mut ret: XmlSchemaAttributeGroupPtr = null_mut();

            if name.is_null() {
                return null_mut();
            }
            if xml_str_equal(ns_name, self.target_namespace) {
                ret = xml_hash_lookup(self.attrgrp_decl, name) as _;
                if !ret.is_null() {
                    return ret;
                }
            }
            if xml_hash_size(self.schemas_imports) > 1 {
                let import: XmlSchemaImportPtr = if ns_name.is_null() {
                    xml_hash_lookup(self.schemas_imports, XML_SCHEMAS_NO_NAMESPACE.as_ptr() as _)
                        as _
                } else {
                    xml_hash_lookup(self.schemas_imports, ns_name) as _
                };
                if import.is_null() {
                    return ret;
                }
                ret = xml_hash_lookup((*(*import).schema).attrgrp_decl, name) as _;
            };
            // TODO:
            // if (!ret.is_null() && ((*ret).redef != null_mut())) {
            //     // Return the last redefinition.
            //     ret = (*ret).redef;
            // }

            ret
        }
    }

    #[doc(alias = "xmlSchemaGetNotation")]
    pub(crate) unsafe fn get_notation(
        &self,
        name: *const u8,
        ns_name: *const u8,
    ) -> XmlSchemaNotationPtr {
        unsafe {
            let mut ret: XmlSchemaNotationPtr = null_mut();

            if name.is_null() {
                return null_mut();
            }
            if xml_str_equal(ns_name, self.target_namespace) {
                ret = xml_hash_lookup(self.nota_decl, name) as _;
                if !ret.is_null() {
                    return ret;
                }
            }
            if xml_hash_size(self.schemas_imports) > 1 {
                let import: XmlSchemaImportPtr = if ns_name.is_null() {
                    xml_hash_lookup(self.schemas_imports, XML_SCHEMAS_NO_NAMESPACE.as_ptr() as _)
                        as _
                } else {
                    xml_hash_lookup(self.schemas_imports, ns_name) as _
                };
                if import.is_null() {
                    return ret;
                }
                ret = xml_hash_lookup((*(*import).schema).nota_decl, name) as _;
            };
            ret
        }
    }

    /// Lookup a group in the schema or imported schemas
    ///
    /// Returns the group definition or NULL if not found.
    #[doc(alias = "xmlSchemaGetNamedComponent")]
    pub(crate) unsafe fn get_named_component(
        &self,
        item_type: XmlSchemaTypeType,
        name: *const u8,
        target_ns: *const u8,
    ) -> XmlSchemaBasicItemPtr {
        unsafe {
            match item_type {
                XmlSchemaTypeType::XmlSchemaTypeGroup => {
                    self.get_group(name, target_ns) as XmlSchemaBasicItemPtr
                }
                XmlSchemaTypeType::XmlSchemaTypeElement => {
                    self.get_elem(name, target_ns) as XmlSchemaBasicItemPtr
                }
                _ => {
                    // TODO
                    null_mut()
                }
            }
        }
    }

    #[doc(alias = "xmlSchemaClearSchemaDefaults")]
    pub(crate) fn clear_flags(&mut self) {
        self.flags &= !XML_SCHEMAS_QUALIF_ELEM;
        self.flags &= !XML_SCHEMAS_QUALIF_ATTR;
        self.flags &= !XML_SCHEMAS_FINAL_DEFAULT_EXTENSION;
        self.flags &= !XML_SCHEMAS_FINAL_DEFAULT_RESTRICTION;
        self.flags &= !XML_SCHEMAS_FINAL_DEFAULT_LIST;
        self.flags &= !XML_SCHEMAS_FINAL_DEFAULT_UNION;
        self.flags &= !XML_SCHEMAS_BLOCK_DEFAULT_EXTENSION;
        self.flags &= !XML_SCHEMAS_BLOCK_DEFAULT_RESTRICTION;
        self.flags &= !XML_SCHEMAS_BLOCK_DEFAULT_SUBSTITUTION;
    }
}

impl Default for XmlSchema {
    fn default() -> Self {
        Self {
            name: null(),
            target_namespace: null(),
            version: null(),
            id: null(),
            doc: None,
            annot: null_mut(),
            flags: 0,
            type_decl: null_mut(),
            attr_decl: null_mut(),
            attrgrp_decl: null_mut(),
            elem_decl: null_mut(),
            nota_decl: null_mut(),
            schemas_imports: null_mut(),
            _private: null_mut(),
            group_decl: null_mut(),
            dict: null_mut(),
            includes: null_mut(),
            preserve: 0,
            counter: 0,
            idc_def: null_mut(),
            volatiles: null_mut(),
        }
    }
}

impl XmlSchemaParserCtxt {
    /// Allocate a new Schema structure.
    ///
    /// Returns the newly allocated structure or NULL in case or error
    #[doc(alias = "xmlSchemaNewSchema")]
    pub(crate) fn new_schema(&mut self) -> XmlSchemaPtr {
        let mut ret = Box::new(XmlSchema::default());
        ret.dict = self.dict;
        xml_dict_reference(ret.dict);
        Box::leak(ret)
    }
}

/// Deallocate a Schema structure.
#[doc(alias = "xmlSchemaFree")]
pub unsafe fn xml_schema_free(schema: XmlSchemaPtr) {
    unsafe {
        if schema.is_null() {
            return;
        }
        // @volatiles is not used anymore :-/
        if !(*schema).volatiles.is_null() {
            // TODO
            todo!()
        }
        // Note that those slots are not responsible for freeing
        // schema components anymore; this will now be done by the schema buckets.
        if !(*schema).nota_decl.is_null() {
            xml_hash_free((*schema).nota_decl, None);
        }
        if !(*schema).attr_decl.is_null() {
            xml_hash_free((*schema).attr_decl, None);
        }
        if !(*schema).attrgrp_decl.is_null() {
            xml_hash_free((*schema).attrgrp_decl, None);
        }
        if !(*schema).elem_decl.is_null() {
            xml_hash_free((*schema).elem_decl, None);
        }
        if !(*schema).type_decl.is_null() {
            xml_hash_free((*schema).type_decl, None);
        }
        if !(*schema).group_decl.is_null() {
            xml_hash_free((*schema).group_decl, None);
        }
        if !(*schema).idc_def.is_null() {
            xml_hash_free((*schema).idc_def, None);
        }

        extern "C" fn xml_schema_bucket_free_entry(bucket: *mut c_void, _name: *const u8) {
            unsafe {
                xml_schema_bucket_free(bucket as XmlSchemaBucketPtr);
            }
        }

        if !(*schema).schemas_imports.is_null() {
            xml_hash_free(
                (*schema).schemas_imports,
                Some(xml_schema_bucket_free_entry),
            );
        }
        if !(*schema).includes.is_null() {
            let list: XmlSchemaItemListPtr<*mut c_void> =
                (*schema).includes as XmlSchemaItemListPtr<*mut c_void>;
            for &item in &(*list).items {
                xml_schema_bucket_free(item as XmlSchemaBucketPtr);
            }
            xml_schema_item_list_free(list);
        }
        if !(*schema).annot.is_null() {
            xml_schema_free_annot((*schema).annot);
        }
        // Never free the doc here, since this will be done by the buckets.

        xml_dict_free((*schema).dict);
        drop_in_place(schema);
        let _ = Box::from_raw(schema);
    }
}
