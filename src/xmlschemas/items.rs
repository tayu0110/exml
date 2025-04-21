use std::{
    borrow::Cow,
    ffi::{CStr, c_void},
    ptr::null,
    rc::Rc,
};

use crate::{
    libxml::{
        globals::xml_free,
        schemas_internals::{
            XML_SCHEMAS_ATTR_GLOBAL, XML_SCHEMAS_ELEM_GLOBAL,
            XML_SCHEMAS_TYPE_DERIVATION_METHOD_EXTENSION,
            XML_SCHEMAS_TYPE_DERIVATION_METHOD_RESTRICTION, XML_SCHEMAS_TYPE_GLOBAL,
            XML_SCHEMAS_TYPE_VARIETY_ATOMIC, XML_SCHEMAS_TYPE_VARIETY_LIST,
            XML_SCHEMAS_TYPE_VARIETY_UNION, XML_SCHEMAS_TYPE_WHITESPACE_PRESERVE,
            XML_SCHEMAS_TYPE_WHITESPACE_REPLACE, XmlSchemaAnnotPtr, XmlSchemaContentType,
            XmlSchemaFacetLinkPtr, XmlSchemaFacetPtr, XmlSchemaTypeLinkPtr, XmlSchemaTypeType,
            XmlSchemaValType, XmlSchemaWildcardPtr,
        },
        xmlregexp::XmlRegexp,
        xmlschemas::XmlSchemaIdcselectPtr,
        xmlschemastypes::{XmlSchemaValPtr, XmlSchemaWhitespaceValueType},
    },
    tree::XmlNodePtr,
    xmlschemastypes::{xml_schema_collapse_string, xml_schema_white_space_replace},
};

pub(crate) trait XmlSchemaItem {
    #[doc(alias = "xmlSchemaGetComponentName")]
    fn name(&self) -> Option<Cow<'static, str>>;
    #[doc(alias = "xmlSchemaGetComponentTargetNs")]
    fn target_namespace(&self) -> Option<Cow<'static, str>>;
}

macro_rules! impl_xml_schema_item {
    {
        type: $t:ty
        $(, @self: $self:tt )?
        $(, @expected: $( $expected:pat )+ )?
        $(, @fallback_name: $( $fallback_name:pat => $fname_block:block )+ )?
        $(, @fallback_ns: $( $fallback_ns:pat => $fns_block:block )+ )?
    } => {
        impl XmlSchemaItem for $t {
            fn name(&self) -> Option<Cow<'static, str>> {
                $(
                    #[allow(unused_variables)]
                    let $self = self;
                )?
                match self.typ {
                    $($( $expected )+ => {
                        let name = self.name;
                        unsafe {
                            (!name.is_null()).then(|| CStr::from_ptr(name as *const i8).to_string_lossy().into_owned().into())
                        }
                    })?
                    $($( $fallback_name => $fname_block )+)?
                    _ => {
                        unreachable!("Unexpected schema item type: {:?}", self.typ);
                    }
                }
            }
            fn target_namespace(&self) -> Option<Cow<'static, str>> {
                $(
                    #[allow(unused_variables)]
                    let $self = self;
                )?
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
                    $($( $fallback_ns => $fns_block )+)?
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
    fn name(&self) -> Option<Cow<'static, str>> {
        let item = self as *const Self;
        unsafe {
            match (*item).typ {
                XmlSchemaTypeType::XmlSchemaTypeElement => {
                    let name = (*(item as XmlSchemaElementPtr)).name;
                    (!name.is_null()).then(|| {
                        CStr::from_ptr(name as *const i8)
                            .to_string_lossy()
                            .into_owned()
                            .into()
                    })
                }
                XmlSchemaTypeType::XmlSchemaTypeAttribute => {
                    let name = (*(item as XmlSchemaAttributePtr)).name;
                    (!name.is_null()).then(|| {
                        CStr::from_ptr(name as *const i8)
                            .to_string_lossy()
                            .into_owned()
                            .into()
                    })
                }
                XmlSchemaTypeType::XmlSchemaTypeAttributeGroup => {
                    let name = (*(item as XmlSchemaAttributeGroupPtr)).name;
                    (!name.is_null()).then(|| {
                        CStr::from_ptr(name as *const i8)
                            .to_string_lossy()
                            .into_owned()
                            .into()
                    })
                }
                XmlSchemaTypeType::XmlSchemaTypeBasic
                | XmlSchemaTypeType::XmlSchemaTypeSimple
                | XmlSchemaTypeType::XmlSchemaTypeComplex => {
                    let name = (*(item as XmlSchemaTypePtr)).name;
                    (!name.is_null()).then(|| {
                        CStr::from_ptr(name as *const i8)
                            .to_string_lossy()
                            .into_owned()
                            .into()
                    })
                }
                XmlSchemaTypeType::XmlSchemaTypeGroup => {
                    let name = (*(item as XmlSchemaModelGroupDefPtr)).name;
                    (!name.is_null()).then(|| {
                        CStr::from_ptr(name as *const i8)
                            .to_string_lossy()
                            .into_owned()
                            .into()
                    })
                }
                XmlSchemaTypeType::XmlSchemaTypeIDCKey
                | XmlSchemaTypeType::XmlSchemaTypeIDCUnique
                | XmlSchemaTypeType::XmlSchemaTypeIDCKeyref => {
                    let name = (*(item as XmlSchemaIDCPtr)).name;
                    (!name.is_null()).then(|| {
                        CStr::from_ptr(name as *const i8)
                            .to_string_lossy()
                            .into_owned()
                            .into()
                    })
                }
                XmlSchemaTypeType::XmlSchemaTypeAttributeUse => {
                    let decl = (*(item as XmlSchemaAttributeUsePtr)).attr_decl;
                    if !decl.is_null() {
                        (*decl).name()
                    } else {
                        None
                    }
                }
                XmlSchemaTypeType::XmlSchemaExtraQNameRef => {
                    let name = (*(item as XmlSchemaQNameRefPtr)).name;
                    (!name.is_null()).then(|| {
                        CStr::from_ptr(name as *const i8)
                            .to_string_lossy()
                            .into_owned()
                            .into()
                    })
                }
                XmlSchemaTypeType::XmlSchemaTypeNotation => {
                    let name = (*(item as XmlSchemaNotationPtr)).name;
                    (!name.is_null()).then(|| {
                        CStr::from_ptr(name as *const i8)
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
                XmlSchemaTypeType::XmlSchemaTypeAttributeGroup => {
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
                XmlSchemaTypeType::XmlSchemaExtraQNameRef => {
                    let ns = (*(item as XmlSchemaQNameRefPtr)).target_namespace;
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
    @fallback_name: _ => { None },
    @fallback_ns: _ => { None }
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
    @fallback_name: _ => { None },
    @fallback_ns: _ => { None }
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
    @fallback_name:
        XmlSchemaTypeType::XmlSchemaTypeAttributeUse => {
            let decl = item.attr_decl;
            if !decl.is_null() {
                unsafe {
                    (*decl).name()
                }
            } else {
                None
            }
        },
    @fallback_ns:
        XmlSchemaTypeType::XmlSchemaTypeAttributeUse => {
            let decl = item.attr_decl;
            if !decl.is_null() {
                unsafe {
                    (*decl).target_namespace()
                }
            } else {
                None
            }
        }
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

impl Default for XmlSchemaAttributeUseProhib {
    fn default() -> Self {
        Self {
            typ: XmlSchemaTypeType::XmlSchemaExtraAttrUseProhib,
            node: None,
            name: null(),
            target_namespace: null(),
            is_ref: 0,
        }
    }
}

impl_xml_schema_item! {
    type: XmlSchemaAttributeUseProhib
    // @fallback_name: _ => { None },
    // @fallback_ns: _ => { None }
}

/// Deallocates an attribute use structure.
#[doc(alias = "xmlSchemaFreeAttributeUseProhib")]
pub(crate) unsafe fn xml_schema_free_attribute_use_prohib(prohib: XmlSchemaAttributeUseProhibPtr) {
    unsafe {
        if prohib.is_null() {
            return;
        }
        xml_free(prohib as _);
    }
}

#[doc(alias = "xmlSchemaQNameRefPtr")]
pub type XmlSchemaQNameRefPtr = *mut XmlSchemaQNameRef;
/// A component reference item (not a schema component)
/// (Extends xmlSchemaBasicItem)
#[doc(alias = "xmlSchemaQNameRef")]
#[repr(C)]
pub struct XmlSchemaQNameRef {
    pub(crate) typ: XmlSchemaTypeType,
    pub(crate) item: XmlSchemaBasicItemPtr, /* The resolved referenced item. */
    pub(crate) item_type: XmlSchemaTypeType,
    pub(crate) name: *const u8,
    pub(crate) target_namespace: *const u8,
    pub(crate) node: Option<XmlNodePtr>,
}

impl_xml_schema_item! {
    type: XmlSchemaQNameRef,
    @expected: XmlSchemaTypeType::XmlSchemaExtraQNameRef
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
    @fallback_name: _ => { None },
    @fallback_ns: _ => { None }
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
    @fallback_name: _ => { None },
    @fallback_ns: _ => { None }
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
    pub(crate) refe: XmlSchemaQNameRefPtr,
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
    pub(crate) cont_model: Option<Rc<XmlRegexp>>, /* Obsolete for WXS, maybe used for RelaxNG */
    pub(crate) content_type: XmlSchemaContentType,
    pub(crate) def_val: XmlSchemaValPtr, /* The compiled value constraint. */
    pub(crate) idcs: *mut c_void,        /* The identity-constraint defs */
}

impl XmlSchemaElement {
    pub(crate) fn is_global_item(&self) -> bool {
        match self.typ {
            XmlSchemaTypeType::XmlSchemaTypeElement => self.flags & XML_SCHEMAS_ELEM_GLOBAL != 0,
            // Note that attribute groups are always global.
            _ => true,
        }
    }
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

impl XmlSchemaAttribute {
    pub(crate) fn is_global_item(&self) -> bool {
        match self.typ {
            XmlSchemaTypeType::XmlSchemaTypeAttribute => self.flags & XML_SCHEMAS_ATTR_GLOBAL != 0,
            // Note that attribute groups are always global.
            _ => true,
        }
    }
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
    @expected: XmlSchemaTypeType::XmlSchemaTypeAttributeGroup
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
    pub(crate) built_in_type: XmlSchemaValType, /* Type of built-in types. */
    pub(crate) member_types: XmlSchemaTypeLinkPtr, /* member-types if a union type. */
    pub(crate) facet_set: XmlSchemaFacetLinkPtr, /* All facets (incl. inherited) */
    pub(crate) content_type_def: XmlSchemaTypePtr, /* Used for the simple content of complex types.
                                                Could we use @subtypes for this? */
    pub(crate) cont_model: Option<Rc<XmlRegexp>>, /* Holds the automaton of the content model */
    pub(crate) target_namespace: *const u8,
    pub(crate) attr_uses: *mut c_void,
}

impl XmlSchemaType {
    pub(crate) fn wxs_is_atomic(&self) -> bool {
        self.flags & XML_SCHEMAS_TYPE_VARIETY_ATOMIC != 0
    }

    pub(crate) fn wxs_is_list(&self) -> bool {
        self.flags & XML_SCHEMAS_TYPE_VARIETY_LIST != 0
    }

    pub(crate) fn wxs_is_union(&self) -> bool {
        self.flags & XML_SCHEMAS_TYPE_VARIETY_UNION != 0
    }

    pub(crate) fn wxs_is_restriction(&self) -> bool {
        self.flags & XML_SCHEMAS_TYPE_DERIVATION_METHOD_RESTRICTION != 0
    }

    pub(crate) fn wxs_is_extension(&self) -> bool {
        self.flags & XML_SCHEMAS_TYPE_DERIVATION_METHOD_EXTENSION != 0
    }

    pub(crate) fn is_global_item(&self) -> bool {
        match self.typ {
            XmlSchemaTypeType::XmlSchemaTypeComplex | XmlSchemaTypeType::XmlSchemaTypeSimple => {
                self.flags & XML_SCHEMAS_TYPE_GLOBAL != 0
            }
            XmlSchemaTypeType::XmlSchemaTypeGroup => true,
            // Note that attribute groups are always global.
            _ => true,
        }
    }

    #[doc(alias = "xmlSchemaGetWhiteSpaceFacetValue")]
    pub(crate) fn white_space_facet_value(&self) -> Option<XmlSchemaWhitespaceValueType> {
        // The normalization type can be changed only for types which are derived
        // from xsd:string.
        if self.typ == XmlSchemaTypeType::XmlSchemaTypeBasic {
            // Note that we assume a whitespace of preserve for anySimpleType.
            if self.built_in_type == XmlSchemaValType::XmlSchemasString
                || self.built_in_type == XmlSchemaValType::XmlSchemasAnySimpletype
            {
                Some(XmlSchemaWhitespaceValueType::XmlSchemaWhitespacePreserve)
            } else if self.built_in_type == XmlSchemaValType::XmlSchemasNormString {
                Some(XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceReplace)
            } else {
                // For all `atomic` datatypes other than string (and types `derived`
                // by `restriction` from it) the value of whiteSpace is fixed to
                // collapse
                // Note that this includes built-in list datatypes.
                Some(XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse)
            }
        } else if self.wxs_is_list() {
            // For list types the facet "whiteSpace" is fixed to "collapse".
            Some(XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse)
        } else if self.wxs_is_union() {
            Some(XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceUnknown)
        } else if self.wxs_is_atomic() {
            if self.flags & XML_SCHEMAS_TYPE_WHITESPACE_PRESERVE != 0 {
                Some(XmlSchemaWhitespaceValueType::XmlSchemaWhitespacePreserve)
            } else if self.flags & XML_SCHEMAS_TYPE_WHITESPACE_REPLACE != 0 {
                Some(XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceReplace)
            } else {
                Some(XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse)
            }
        } else {
            None
        }
    }

    #[doc(alias = "xmlSchemaNormalizeValue")]
    pub(crate) fn normalize_value<'a>(&self, value: &'a str) -> Option<Cow<'a, str>> {
        match self.white_space_facet_value() {
            Some(XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceCollapse) => {
                xml_schema_collapse_string(value)
            }
            Some(XmlSchemaWhitespaceValueType::XmlSchemaWhitespaceReplace) => {
                xml_schema_white_space_replace(value).map(Cow::Owned)
            }
            _ => None,
        }
    }
}

impl_xml_schema_item! {
    type: XmlSchemaType,
    @self: item,
    @expected: XmlSchemaTypeType::XmlSchemaTypeSimple | XmlSchemaTypeType::XmlSchemaTypeComplex,
    @fallback_name:
        XmlSchemaTypeType::XmlSchemaTypeBasic => {
            let name = item.name;
            unsafe {
                (!name.is_null()).then(|| CStr::from_ptr(name as *const i8).to_string_lossy().into_owned().into())
            }
        }
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
