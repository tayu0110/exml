use std::{
    borrow::Cow,
    collections::HashMap,
    ffi::c_void,
    ptr::{null, null_mut},
};

use crate::{
    dict::{XmlDictPtr, xml_dict_free, xml_dict_reference},
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
    pub(crate) name: *const u8,                             /* schema name */
    pub(crate) target_namespace: Option<Cow<'static, str>>, /* the target namespace */
    pub(crate) version: *const u8,
    pub(crate) id: *const u8, /* Obsolete */
    pub(crate) doc: Option<XmlDocPtr>,
    pub(crate) annot: XmlSchemaAnnotPtr,
    pub(crate) flags: i32,

    pub(crate) type_decl: HashMap<String, XmlSchemaTypePtr>,
    pub(crate) attr_decl: HashMap<String, XmlSchemaAttributePtr>,
    pub(crate) attrgrp_decl: HashMap<String, XmlSchemaAttributeGroupPtr>,
    pub(crate) elem_decl: HashMap<String, XmlSchemaElementPtr>,
    pub(crate) nota_decl: HashMap<String, XmlSchemaNotationPtr>,

    pub(crate) schemas_imports: HashMap<String, XmlSchemaImportPtr>,

    pub(crate) _private: *mut c_void, /* unused by the library for users or bindings */
    pub(crate) group_decl: HashMap<String, XmlSchemaModelGroupDefPtr>,
    pub(crate) dict: XmlDictPtr,
    pub(crate) includes: *mut c_void, /* the includes, this is opaque for now */
    pub(crate) preserve: i32,         /* whether to free the document */
    pub(crate) counter: i32,          /* used to give anonymous components unique names */
    pub(crate) idc_def: HashMap<String, XmlSchemaIDCPtr>, /* All identity-constraint defs. */
    pub(crate) volatiles: *mut c_void, /* Obsolete */
}

impl XmlSchema {
    /// Lookup a type in the schemas or the predefined types
    ///
    /// Returns the group definition or NULL if not found.
    #[doc(alias = "xmlSchemaGetType")]
    pub(crate) unsafe fn get_type(&self, name: &str, ns_name: Option<&str>) -> XmlSchemaTypePtr {
        unsafe {
            // First try the built-in types.
            if let Some(ns_name) =
                ns_name.filter(|&ns_name| ns_name == XML_SCHEMA_NS.to_str().unwrap())
            {
                let ret = xml_schema_get_predefined_type(name, ns_name);
                if !ret.is_null() {
                    return ret;
                }
                // Note that we try the parsed schemas as well here
                // since one might have parsed the S4S, which contain more
                // than the built-in types.
                // TODO: Can we optimize this?
            }
            if ns_name == self.target_namespace.as_deref() {
                if let Some(&ret) = self.type_decl.get(name) {
                    return ret;
                }
            }
            let import: XmlSchemaImportPtr = if let Some(ns_name) = ns_name {
                self.schemas_imports
                    .get(ns_name)
                    .copied()
                    .unwrap_or(null_mut())
            } else {
                self.schemas_imports
                    .get(XML_SCHEMAS_NO_NAMESPACE)
                    .copied()
                    .unwrap_or(null_mut())
            };
            if import.is_null() {
                return null_mut();
            }
            *(*(*import).schema)
                .type_decl
                .get(name)
                .unwrap_or(&null_mut())
        }
    }

    #[doc(alias = "xmlSchemaGetIDC")]
    pub(crate) unsafe fn get_idc(&self, name: &str, ns_name: Option<&str>) -> XmlSchemaIDCPtr {
        unsafe {
            if ns_name == self.target_namespace.as_deref() {
                if let Some(&ret) = self.idc_def.get(name) {
                    return ret;
                }
            }
            let import: XmlSchemaImportPtr = if let Some(ns_name) = ns_name {
                self.schemas_imports
                    .get(ns_name)
                    .copied()
                    .unwrap_or(null_mut())
            } else {
                self.schemas_imports
                    .get(XML_SCHEMAS_NO_NAMESPACE)
                    .copied()
                    .unwrap_or(null_mut())
            };
            if import.is_null() {
                return null_mut();
            }
            *(*(*import).schema).idc_def.get(name).unwrap_or(&null_mut())
        }
    }

    /// Lookup a global element declaration in the schema.
    ///
    /// Returns the element declaration or NULL if not found.
    #[doc(alias = "xmlSchemaGetElem")]
    pub(crate) unsafe fn get_elem(&self, name: &str, ns_name: Option<&str>) -> XmlSchemaElementPtr {
        unsafe {
            if ns_name == self.target_namespace.as_deref() {
                if let Some(&ret) = self.elem_decl.get(name) {
                    return ret;
                }
            }
            let import: XmlSchemaImportPtr = if let Some(ns_name) = ns_name {
                self.schemas_imports
                    .get(ns_name)
                    .copied()
                    .unwrap_or(null_mut())
            } else {
                self.schemas_imports
                    .get(XML_SCHEMAS_NO_NAMESPACE)
                    .copied()
                    .unwrap_or(null_mut())
            };
            if import.is_null() {
                return null_mut();
            }
            (*(*import).schema)
                .elem_decl
                .get(name)
                .copied()
                .unwrap_or(null_mut())
        }
    }

    /// Lookup a an attribute in the schema or imported schemas
    ///
    /// Returns the attribute declaration or NULL if not found.
    #[doc(alias = "xmlSchemaGetAttributeDecl")]
    pub(crate) unsafe fn get_attribute_decl(
        &self,
        name: &str,
        ns_name: Option<&str>,
    ) -> XmlSchemaAttributePtr {
        unsafe {
            if ns_name == self.target_namespace.as_deref() {
                if let Some(&ret) = self.attr_decl.get(name) {
                    return ret;
                }
            }
            let import: XmlSchemaImportPtr = if let Some(ns_name) = ns_name {
                self.schemas_imports
                    .get(ns_name)
                    .copied()
                    .unwrap_or(null_mut())
            } else {
                self.schemas_imports
                    .get(XML_SCHEMAS_NO_NAMESPACE)
                    .copied()
                    .unwrap_or(null_mut())
            };
            if import.is_null() {
                return null_mut();
            }
            (*(*import).schema)
                .attr_decl
                .get(name)
                .copied()
                .unwrap_or(null_mut())
        }
    }

    /// Lookup a group in the schema or imported schemas
    ///
    /// Returns the group definition or NULL if not found.
    #[doc(alias = "xmlSchemaGetGroup")]
    pub(crate) unsafe fn get_group(
        &self,
        name: &str,
        ns_name: Option<&str>,
    ) -> XmlSchemaModelGroupDefPtr {
        unsafe {
            if ns_name == self.target_namespace.as_deref() {
                if let Some(&ret) = self.group_decl.get(name) {
                    return ret;
                }
            }
            let import: XmlSchemaImportPtr = if let Some(ns_name) = ns_name {
                self.schemas_imports
                    .get(ns_name)
                    .copied()
                    .unwrap_or(null_mut())
            } else {
                self.schemas_imports
                    .get(XML_SCHEMAS_NO_NAMESPACE)
                    .copied()
                    .unwrap_or(null_mut())
            };
            if import.is_null() {
                return null_mut();
            }
            (*(*import).schema)
                .group_decl
                .get(name)
                .copied()
                .unwrap_or(null_mut())
        }
    }

    /// Lookup a an attribute group in the schema or imported schemas
    ///
    /// Returns the attribute group definition or NULL if not found.
    #[doc(alias = "xmlSchemaGetAttributeGroup")]
    pub(crate) unsafe fn get_attribute_group(
        &self,
        name: &str,
        ns_name: Option<&str>,
    ) -> XmlSchemaAttributeGroupPtr {
        unsafe {
            if ns_name == self.target_namespace.as_deref() {
                if let Some(&ret) = self.attrgrp_decl.get(name) {
                    return ret;
                }
            }
            let import: XmlSchemaImportPtr = if let Some(ns_name) = ns_name {
                self.schemas_imports
                    .get(ns_name)
                    .copied()
                    .unwrap_or(null_mut())
            } else {
                self.schemas_imports
                    .get(XML_SCHEMAS_NO_NAMESPACE)
                    .copied()
                    .unwrap_or(null_mut())
            };
            if import.is_null() {
                return null_mut();
            }
            (*(*import).schema)
                .attrgrp_decl
                .get(name)
                .copied()
                .unwrap_or(null_mut())
        }
    }

    #[doc(alias = "xmlSchemaGetNotation")]
    pub(crate) unsafe fn get_notation(
        &self,
        name: &str,
        ns_name: Option<&str>,
    ) -> XmlSchemaNotationPtr {
        unsafe {
            if ns_name == self.target_namespace.as_deref() {
                if let Some(&ret) = self.nota_decl.get(name) {
                    return ret;
                }
            }
            let import: XmlSchemaImportPtr = if let Some(ns_name) = ns_name {
                self.schemas_imports
                    .get(ns_name)
                    .copied()
                    .unwrap_or(null_mut())
            } else {
                self.schemas_imports
                    .get(XML_SCHEMAS_NO_NAMESPACE)
                    .copied()
                    .unwrap_or(null_mut())
            };
            if import.is_null() {
                return null_mut();
            }
            (*(*import).schema)
                .nota_decl
                .get(name)
                .copied()
                .unwrap_or(null_mut())
        }
    }

    /// Lookup a group in the schema or imported schemas
    ///
    /// Returns the group definition or NULL if not found.
    #[doc(alias = "xmlSchemaGetNamedComponent")]
    pub(crate) unsafe fn get_named_component(
        &self,
        item_type: XmlSchemaTypeType,
        name: &str,
        target_ns: Option<&str>,
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
            target_namespace: None,
            version: null(),
            id: null(),
            doc: None,
            annot: null_mut(),
            flags: 0,
            type_decl: HashMap::new(),
            attr_decl: HashMap::new(),
            attrgrp_decl: HashMap::new(),
            elem_decl: HashMap::new(),
            nota_decl: HashMap::new(),
            schemas_imports: HashMap::new(),
            _private: null_mut(),
            group_decl: HashMap::new(),
            dict: null_mut(),
            includes: null_mut(),
            preserve: 0,
            counter: 0,
            idc_def: HashMap::new(),
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

        for (_, entry) in (*schema).schemas_imports.drain() {
            xml_schema_bucket_free(entry as XmlSchemaBucketPtr);
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
        let _ = Box::from_raw(schema);
    }
}
