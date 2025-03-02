use std::ffi::c_void;

use crate::{
    libxml::{
        schemas_internals::{XmlSchemaAnnotPtr, XmlSchemaAttributePtr, XmlSchemaTypeType},
        xmlschemas::XmlSchemaIdcselectPtr,
        xmlschemastypes::XmlSchemaValPtr,
    },
    tree::XmlNodePtr,
};

/// The abstract base type for schema components.
#[doc(alias = "xmlSchemaBasicItemPtr")]
pub type XmlSchemaBasicItemPtr = *mut XmlSchemaBasicItem;
#[doc(alias = "xmlSchemaBasicItem")]
#[repr(C)]
pub struct XmlSchemaBasicItem {
    pub(crate) typ: XmlSchemaTypeType,
    pub(crate) dummy: *mut c_void, /* Fix alignment issues */
}

/// The abstract base type for annotated schema components.
/// (Extends xmlSchemaBasicItem)
#[doc(alias = "xmlSchemaAnnotItemPtr")]
pub type XmlSchemaAnnotItemPtr = *mut XmlSchemaAnnotItem;
#[doc(alias = "xmlSchemaAnnotItem")]
#[repr(C)]
pub struct XmlSchemaAnnotItem {
    pub(crate) typ: XmlSchemaTypeType,
    pub(crate) annot: XmlSchemaAnnotPtr,
}

/// The abstract base type for tree-like structured schema components.
/// (Extends xmlSchemaAnnotItem)
#[doc(alias = "xmlSchemaTreeItemPtr")]
pub type XmlSchemaTreeItemPtr = *mut XmlSchemaTreeItem;
#[doc(alias = "xmlSchemaTreeItem")]
#[repr(C)]
pub struct XmlSchemaTreeItem {
    pub(crate) typ: XmlSchemaTypeType,
    pub(crate) annot: XmlSchemaAnnotPtr,
    pub(crate) next: XmlSchemaTreeItemPtr,
    pub(crate) children: XmlSchemaTreeItemPtr,
}

/// The abstract base type for tree-like structured schema components.
/// (Extends xmlSchemaTreeItem)
#[doc(alias = "xmlSchemaAttributeUsePtr")]
pub type XmlSchemaAttributeUsePtr = *mut XmlSchemaAttributeUse;
#[doc(alias = "xmlSchemaAttributeUse")]
#[repr(C)]
pub struct XmlSchemaAttributeUse {
    pub(crate) typ: XmlSchemaTypeType,
    pub(crate) annot: XmlSchemaAnnotPtr,
    pub(crate) next: XmlSchemaAttributeUsePtr, /* The next attr. use. */
    // The attr. decl. OR a QName-ref. to an attr. decl. OR
    // a QName-ref. to an attribute group definition.
    pub(crate) attr_decl: XmlSchemaAttributePtr,

    pub(crate) flags: i32,
    pub(crate) node: XmlNodePtr,
    pub(crate) occurs: i32, /* required, optional */
    pub(crate) def_value: *const u8,
    pub(crate) def_val: XmlSchemaValPtr,
}

/// A helper component to reflect attribute prohibitions.
/// (Extends xmlSchemaBasicItem)
#[doc(alias = "xmlSchemaAttributeUseProhibPtr")]
pub type XmlSchemaAttributeUseProhibPtr = *mut XmlSchemaAttributeUseProhib;
#[doc(alias = "xmlSchemaAttributeUseProhib")]
#[repr(C)]
pub struct XmlSchemaAttributeUseProhib {
    pub(crate) typ: XmlSchemaTypeType, /* == XML_SCHEMA_EXTRA_ATTR_USE_PROHIB */
    pub(crate) node: Option<XmlNodePtr>,
    pub(crate) name: *const u8,
    pub(crate) target_namespace: *const u8,
    is_ref: i32,
}

#[doc(alias = "xmlSchemaQNameRefPtr")]
pub type XmlSchemaQnameRefPtr = *mut XmlSchemaQnameRef;
/// A component reference item (not a schema component)
/// (Extends xmlSchemaBasicItem)
#[doc(alias = "xmlSchemaQNameRef")]
#[repr(C)]
pub struct XmlSchemaQnameRef {
    pub(crate) typ: XmlSchemaTypeType,
    pub(crate) item: XmlSchemaBasicItemPtr, /* The resolved referenced item. */
    pub(crate) item_type: XmlSchemaTypeType,
    pub(crate) name: *const u8,
    pub(crate) target_namespace: *const u8,
    pub(crate) node: Option<XmlNodePtr>,
}

#[doc(alias = "xmlSchemaParticlePtr")]
pub type XmlSchemaParticlePtr = *mut XmlSchemaParticle;
/// A particle component.
/// (Extends xmlSchemaTreeItem)
#[doc(alias = "xmlSchemaParticle")]
#[repr(C)]
pub struct XmlSchemaParticle {
    pub(crate) typ: XmlSchemaTypeType,
    pub(crate) annot: XmlSchemaAnnotPtr,
    pub(crate) next: XmlSchemaTreeItemPtr, /* next particle */
    // the "term" (e.g. a model group,
    // a group definition, a XML_SCHEMA_EXTRA_QNAMEREF (if a reference), etc.)
    pub(crate) children: XmlSchemaTreeItemPtr,
    pub(crate) min_occurs: i32,
    pub(crate) max_occurs: i32,
    pub(crate) node: Option<XmlNodePtr>,
}

#[doc(alias = "xmlSchemaModelGroupPtr")]
pub type XmlSchemaModelGroupPtr = *mut XmlSchemaModelGroup;
/// A model group component.
/// (Extends xmlSchemaTreeItem)
#[doc(alias = "xmlSchemaModelGroup")]
#[repr(C)]
pub struct XmlSchemaModelGroup {
    pub(crate) typ: XmlSchemaTypeType, /* XmlSchemaTypeSequence, XmlSchemaTypeChoice, XmlSchemaTypeAll */
    pub(crate) annot: XmlSchemaAnnotPtr,
    pub(crate) next: XmlSchemaTreeItemPtr,     /* not used */
    pub(crate) children: XmlSchemaTreeItemPtr, /* first particle (OR "element decl" OR "wildcard") */
    pub(crate) node: XmlNodePtr,
}

#[doc(alias = "xmlSchemaModelGroupDefPtr")]
pub type XmlSchemaModelGroupDefPtr = *mut XmlSchemaModelGroupDef;
/// A model group definition component.
/// (Extends xmlSchemaTreeItem)
#[doc(alias = "xmlSchemaModelGroupDef")]
#[repr(C)]
pub struct XmlSchemaModelGroupDef {
    pub(crate) typ: XmlSchemaTypeType, /* XML_SCHEMA_TYPE_GROUP */
    pub(crate) annot: XmlSchemaAnnotPtr,
    pub(crate) next: XmlSchemaTreeItemPtr,     /* not used */
    pub(crate) children: XmlSchemaTreeItemPtr, /* the "model group" */
    pub(crate) name: *const u8,
    pub(crate) target_namespace: *const u8,
    pub(crate) node: XmlNodePtr,
    pub(crate) flags: i32,
}

#[doc(alias = "xmlSchemaIDCPtr")]
pub type XmlSchemaIDCPtr = *mut XmlSchemaIDC;
/// The identity-constraint definition component.
/// (Extends xmlSchemaAnnotItem)
#[doc(alias = "xmlSchemaIDC")]
#[repr(C)]
pub struct XmlSchemaIDC {
    pub(crate) typ: XmlSchemaTypeType,
    pub(crate) annot: XmlSchemaAnnotPtr,
    pub(crate) next: XmlSchemaIDCPtr,
    pub(crate) node: XmlNodePtr,
    pub(crate) name: *const u8,
    pub(crate) target_namespace: *const u8,
    pub(crate) selector: XmlSchemaIdcselectPtr,
    pub(crate) fields: XmlSchemaIdcselectPtr,
    pub(crate) nb_fields: i32,
    pub(crate) refe: XmlSchemaQnameRefPtr,
}
