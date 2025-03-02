use std::{
    borrow::Cow,
    ffi::{CStr, c_void},
};

use crate::{
    libxml::{
        schemas_internals::{
            XmlSchemaAnnotPtr, XmlSchemaContentType, XmlSchemaFacetLinkPtr, XmlSchemaFacetPtr,
            XmlSchemaTypeLinkPtr, XmlSchemaTypeType, XmlSchemaWildcardPtr,
        },
        xmlregexp::XmlRegexpPtr,
        xmlschemas::XmlSchemaIdcselectPtr,
        xmlschemastypes::XmlSchemaValPtr,
    },
    tree::XmlNodePtr,
};

pub(crate) trait XmlSchemaItem {
    #[doc(alias = "xmlSchemaGetComponentTargetNs")]
    fn target_namespace(&self) -> Option<Cow<'static, str>>;
}

macro_rules! impl_xml_schema_item {
    {
        type: $t:ty
        $(, @self: $self:tt )?
        $(, @expected: $( $expected:tt )+ )?
        $(, @fallback: $( $fallback:tt )+ )?
    } => {
        impl XmlSchemaItem for $t {
            fn target_namespace(&self) -> Option<Cow<'static, str>> {
                $( let $self = self; )?
                match self.typ {
                    $($( $expected )+ => {
                        let ns = self.target_namespace;
                        unsafe {
                            (!ns.is_null()).then(|| CStr::from_ptr(ns as *const i8).to_string_lossy().into_owned().into())
                        }
                    })?
                    XmlSchemaTypeType::XmlSchemaTypeBasic => {
                        Some("http://www.w3.org/2001/XMLSchema".into())
                    }
                    $($( $fallback )+)?
                    _ => {
                        unreachable!("Unexpected schema item type: {:?}", self.typ);
                    }
                }
            }
        }
    };
}

/// The abstract base type for schema components.
#[doc(alias = "xmlSchemaBasicItemPtr")]
pub type XmlSchemaBasicItemPtr = *mut XmlSchemaBasicItem;
#[doc(alias = "xmlSchemaBasicItem")]
#[repr(C)]
pub struct XmlSchemaBasicItem {
    pub(crate) typ: XmlSchemaTypeType,
    pub(crate) dummy: *mut c_void, /* Fix alignment issues */
}

impl XmlSchemaItem for XmlSchemaBasicItem {
    fn target_namespace(&self) -> Option<Cow<'static, str>> {
        let item = self as *const Self;
        unsafe {
            match self.typ {
                XmlSchemaTypeType::XmlSchemaTypeElement => {
                    let ns = (*(item as XmlSchemaElementPtr)).target_namespace;
                    (!ns.is_null()).then(|| {
                        CStr::from_ptr(ns as *const i8)
                            .to_string_lossy()
                            .into_owned()
                            .into()
                    })
                }
                XmlSchemaTypeType::XmlSchemaTypeAttribute => {
                    let ns = (*(item as XmlSchemaAttributePtr)).target_namespace;
                    (!ns.is_null()).then(|| {
                        CStr::from_ptr(ns as *const i8)
                            .to_string_lossy()
                            .into_owned()
                            .into()
                    })
                }
                XmlSchemaTypeType::XmlSchemaTypeAttributegroup => {
                    let ns = (*(item as XmlSchemaAttributeGroupPtr)).target_namespace;
                    (!ns.is_null()).then(|| {
                        CStr::from_ptr(ns as *const i8)
                            .to_string_lossy()
                            .into_owned()
                            .into()
                    })
                }
                XmlSchemaTypeType::XmlSchemaTypeBasic => {
                    Some("http://www.w3.org/2001/XMLSchema".into())
                }
                XmlSchemaTypeType::XmlSchemaTypeSimple
                | XmlSchemaTypeType::XmlSchemaTypeComplex => {
                    let ns = (*(item as XmlSchemaTypePtr)).target_namespace;
                    (!ns.is_null()).then(|| {
                        CStr::from_ptr(ns as *const i8)
                            .to_string_lossy()
                            .into_owned()
                            .into()
                    })
                }
                XmlSchemaTypeType::XmlSchemaTypeGroup => {
                    let ns = (*(item as XmlSchemaModelGroupDefPtr)).target_namespace;
                    (!ns.is_null()).then(|| {
                        CStr::from_ptr(ns as *const i8)
                            .to_string_lossy()
                            .into_owned()
                            .into()
                    })
                }
                XmlSchemaTypeType::XmlSchemaTypeIDCKey
                | XmlSchemaTypeType::XmlSchemaTypeIDCUnique
                | XmlSchemaTypeType::XmlSchemaTypeIDCKeyref => {
                    let ns = (*(item as XmlSchemaIDCPtr)).target_namespace;
                    (!ns.is_null()).then(|| {
                        CStr::from_ptr(ns as *const i8)
                            .to_string_lossy()
                            .into_owned()
                            .into()
                    })
                }
                XmlSchemaTypeType::XmlSchemaTypeAttributeUse => {
                    let decl = (*(item as XmlSchemaAttributeUsePtr)).attr_decl;
                    if !decl.is_null() {
                        return (*(decl as *mut Self)).target_namespace();
                    }
                    // TODO: Will returning NULL break something?
                    None
                }
                XmlSchemaTypeType::XmlSchemaExtraQnameref => {
                    let ns = (*(item as XmlSchemaQnameRefPtr)).target_namespace;
                    (!ns.is_null()).then(|| {
                        CStr::from_ptr(ns as *const i8)
                            .to_string_lossy()
                            .into_owned()
                            .into()
                    })
                }
                XmlSchemaTypeType::XmlSchemaTypeNotation => {
                    let ns = (*(item as XmlSchemaNotationPtr)).target_namespace;
                    (!ns.is_null()).then(|| {
                        CStr::from_ptr(ns as *const i8)
                            .to_string_lossy()
                            .into_owned()
                            .into()
                    })
                }
                _ => {
                    // Other components cannot have names.
                    None
                }
            }
        }
    }
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

impl_xml_schema_item! {
    type: XmlSchemaAnnotItem,
    @fallback: _ => None,
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

impl_xml_schema_item! {
    type: XmlSchemaTreeItem,
    @fallback: _ => None,
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

impl_xml_schema_item! {
    type: XmlSchemaAttributeUse,
    @self: item,
    @fallback:
        XmlSchemaTypeType::XmlSchemaTypeAttributeUse => {
            let decl = item.attr_decl;
            if !decl.is_null() {
                unsafe {
                    (*decl).target_namespace()
                }
            } else {
                None
            }
        },
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

impl_xml_schema_item! {
    type: XmlSchemaAttributeUseProhib,
    @fallback: _ => None,
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

impl_xml_schema_item! {
    type: XmlSchemaQnameRef,
    @expected: XmlSchemaTypeType::XmlSchemaExtraQnameref
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

impl_xml_schema_item! {
    type: XmlSchemaParticle,
    @fallback: _ => None,
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

impl_xml_schema_item! {
    type: XmlSchemaModelGroup,
    @fallback: _ => None,
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

impl_xml_schema_item! {
    type: XmlSchemaModelGroupDef,
    @expected: XmlSchemaTypeType::XmlSchemaTypeGroup
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

impl_xml_schema_item! {
    type: XmlSchemaIDC,
    @expected:
        XmlSchemaTypeType::XmlSchemaTypeIDCKey
        | XmlSchemaTypeType::XmlSchemaTypeIDCUnique
        | XmlSchemaTypeType::XmlSchemaTypeIDCKeyref
}

pub type XmlSchemaElementPtr = *mut XmlSchemaElement;
/// An element definition.
///
/// xmlSchemaType, xmlSchemaFacet and xmlSchemaElement start of
/// structures must be kept similar
#[doc(alias = "xmlSchemaElement")]
#[repr(C)]
pub struct XmlSchemaElement {
    pub(crate) typ: XmlSchemaTypeType,   /* The kind of type */
    pub(crate) next: *mut XmlSchemaType, /* Not used? */
    pub(crate) name: *const u8,
    pub(crate) annot: XmlSchemaAnnotPtr,
    pub(crate) subtypes: XmlSchemaTypePtr, /* the type definition */
    pub(crate) attributes: XmlSchemaAttributePtr,
    pub(crate) node: Option<XmlNodePtr>,
    pub(crate) flags: i32,
    pub(crate) target_namespace: *const u8,
    pub(crate) named_type: *const u8,
    pub(crate) named_type_ns: *const u8,
    pub(crate) subst_group: *const u8,
    pub(crate) subst_group_ns: *const u8,
    pub(crate) scope: *const u8,
    pub(crate) value: *const u8, /* The original value of the value constraint. */
    pub(crate) ref_decl: *mut XmlSchemaElement, /* This will now be used for the
                                 substitution group affiliation */
    pub(crate) cont_model: XmlRegexpPtr, /* Obsolete for WXS, maybe used for RelaxNG */
    pub(crate) content_type: XmlSchemaContentType,
    pub(crate) def_val: XmlSchemaValPtr, /* The compiled value constraint. */
    pub(crate) idcs: *mut c_void,        /* The identity-constraint defs */
}

impl_xml_schema_item! {
    type: XmlSchemaElement,
    @expected: XmlSchemaTypeType::XmlSchemaTypeElement
}

/// An attribute definition.
#[doc(alias = "xmlSchemaAttribute")]
pub type XmlSchemaAttributePtr = *mut XmlSchemaAttribute;
#[repr(C)]
pub struct XmlSchemaAttribute {
    pub(crate) typ: XmlSchemaTypeType,
    pub(crate) next: *mut XmlSchemaAttribute, /* the next attribute (not used?) */
    pub(crate) name: *const u8,               /* the name of the declaration */
    pub(crate) type_name: *const u8,          /* the local name of the type definition */
    pub(crate) type_ns: *const u8,            /* the ns URI of the type definition */
    pub(crate) annot: XmlSchemaAnnotPtr,

    pub(crate) def_value: *const u8, /* The initial value of the value constraint */
    pub(crate) subtypes: XmlSchemaTypePtr, /* the type definition */
    pub(crate) node: Option<XmlNodePtr>,
    pub(crate) target_namespace: *const u8,
    pub(crate) flags: i32,
    pub(crate) def_val: XmlSchemaValPtr, /* The compiled value constraint */
}

impl_xml_schema_item! {
    type: XmlSchemaAttribute,
    @expected: XmlSchemaTypeType::XmlSchemaTypeAttribute
}

pub type XmlSchemaAttributeGroupPtr = *mut XmlSchemaAttributeGroup;
/// xmlSchemaAttribute and xmlSchemaAttributeGroup start of structures must be kept similar
#[repr(C)]
pub struct XmlSchemaAttributeGroup {
    pub(crate) typ: XmlSchemaTypeType,        /* The kind of type */
    pub(crate) next: *mut XmlSchemaAttribute, /* the next attribute if in a group ... */
    pub(crate) name: *mut u8,
    pub(crate) id: *const u8,
    pub(crate) annot: XmlSchemaAnnotPtr,

    pub(crate) node: Option<XmlNodePtr>,
    pub(crate) flags: i32,
    pub(crate) attribute_wildcard: XmlSchemaWildcardPtr,
    pub(crate) target_namespace: *const u8,
    pub(crate) attr_uses: *mut c_void,
}

impl_xml_schema_item! {
    type: XmlSchemaAttributeGroup,
    @expected: XmlSchemaTypeType::XmlSchemaTypeAttributegroup
}

pub type XmlSchemaTypePtr = *mut XmlSchemaType;
/// Schemas type definition.
#[doc(alias = "xmlSchemaType")]
#[repr(C)]
pub struct XmlSchemaType {
    pub(crate) typ: XmlSchemaTypeType,   /* The kind of type */
    pub(crate) next: *mut XmlSchemaType, /* the next type if in a sequence ... */
    pub(crate) name: *const u8,
    pub(crate) annot: XmlSchemaAnnotPtr,
    pub(crate) subtypes: XmlSchemaTypePtr,
    pub(crate) node: Option<XmlNodePtr>,
    pub(crate) flags: i32,
    pub(crate) content_type: XmlSchemaContentType,
    pub(crate) base: *const u8,             /* Base type's local name */
    pub(crate) base_ns: *const u8,          /* Base type's target namespace */
    pub(crate) base_type: XmlSchemaTypePtr, /* The base type component */
    pub(crate) facets: XmlSchemaFacetPtr,   /* Local facets */
    pub(crate) recurse: i32,                /* Obsolete */
    pub(crate) attribute_wildcard: XmlSchemaWildcardPtr,
    pub(crate) built_in_type: i32, /* Type of built-in types. */
    pub(crate) member_types: XmlSchemaTypeLinkPtr, /* member-types if a union type. */
    pub(crate) facet_set: XmlSchemaFacetLinkPtr, /* All facets (incl. inherited) */
    pub(crate) content_type_def: XmlSchemaTypePtr, /* Used for the simple content of complex types.
                                   Could we use @subtypes for this? */
    pub(crate) cont_model: XmlRegexpPtr, /* Holds the automaton of the content model */
    pub(crate) target_namespace: *const u8,
    pub(crate) attr_uses: *mut c_void,
}

impl_xml_schema_item! {
    type: XmlSchemaType,
    @expected: XmlSchemaTypeType::XmlSchemaTypeSimple | XmlSchemaTypeType::XmlSchemaTypeComplex
}

#[doc(alias = "xmlSchemaNotationPtr")]
pub type XmlSchemaNotationPtr = *mut XmlSchemaNotation;
/// A notation definition.
#[doc(alias = "xmlSchemaNotation")]
#[repr(C)]
pub struct XmlSchemaNotation {
    pub(crate) typ: XmlSchemaTypeType, /* The kind of type */
    pub(crate) name: *const u8,
    pub(crate) annot: XmlSchemaAnnotPtr,
    pub(crate) identifier: *const u8,
    pub(crate) target_namespace: *const u8,
}

impl_xml_schema_item! {
    type: XmlSchemaNotation,
    @expected: XmlSchemaTypeType::XmlSchemaTypeNotation
}
