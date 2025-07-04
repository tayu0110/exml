//! Provide methods and data structures for handling XML entities.
//!
//! This module is based on `libxml/entities.h`, `entities.c`, and so on in `libxml2-v2.11.8`.  
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: interface for the XML entities handling
// Description: this module provides some of the entity API needed
//              for the parser and applications.
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// entities.c : implementation for the XML entities handling
//
// See Copyright for the status of this software.
//
// daniel@veillard.com

#[cfg(feature = "libxml_output")]
use std::io::Write;
use std::{
    any::type_name,
    borrow::Cow,
    collections::hash_map::Entry,
    ops::{Deref, DerefMut},
    os::raw::c_void,
    ptr::{NonNull, null_mut},
};

use crate::{
    chvalid::XmlCharValid,
    error::{
        __xml_raise_error, __xml_simple_error, __xml_simple_oom_error, XmlErrorDomain,
        XmlParserErrors,
    },
    tree::{NodeCommon, XmlElementType, xml_free_node_list},
};

use super::{InvalidNodePointerCastError, XmlDocPtr, XmlDtdPtr, XmlGenericNodePtr, XmlNodePtr};

/// The different valid entity types.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum XmlEntityType {
    #[default]
    XmlInternalGeneralEntity = 1,
    XmlExternalGeneralParsedEntity = 2,
    XmlExternalGeneralUnparsedEntity = 3,
    XmlInternalParameterEntity = 4,
    XmlExternalParameterEntity = 5,
    XmlInternalPredefinedEntity = 6,
}

#[repr(C)]
pub struct XmlEntity {
    // application data
    pub _private: *mut c_void,
    // XML_ENTITY_DECL, must be second !
    pub(crate) typ: XmlElementType,
    // Entity name
    pub(crate) name: Cow<'static, str>,
    // First child link
    pub(crate) children: Option<XmlNodePtr>,
    // Last child link
    pub(crate) last: Option<XmlNodePtr>,
    // -> DTD
    pub(crate) parent: Option<XmlDtdPtr>,
    // next sibling link
    pub(crate) next: Option<XmlGenericNodePtr>,
    // previous sibling link
    pub(crate) prev: Option<XmlGenericNodePtr>,
    // the containing document
    pub(crate) doc: Option<XmlDocPtr>,

    // content without ref substitution
    pub(crate) orig: Option<Cow<'static, str>>,
    // content or ndata if unparsed
    pub(crate) content: Option<Cow<'static, str>>,
    // the content length
    pub(crate) length: i32,
    // The entity type
    pub(crate) etype: XmlEntityType,
    // External identifier for PUBLIC
    pub(crate) external_id: Option<Box<str>>,
    // URI for a SYSTEM or PUBLIC Entity
    pub(crate) system_id: Option<Box<str>>,

    // unused
    pub(crate) nexte: Option<XmlEntityPtr>,
    // the full URI as computed
    pub(crate) uri: Option<Box<str>>,
    // does the entity own the childrens
    pub(crate) owner: i32,
    // various flags
    pub(crate) flags: i32,
    // expanded size
    pub expanded_size: u64,
}

impl NodeCommon for XmlEntity {
    fn document(&self) -> Option<XmlDocPtr> {
        self.doc
    }
    fn set_document(&mut self, doc: Option<XmlDocPtr>) {
        self.doc = doc;
    }
    fn element_type(&self) -> XmlElementType {
        self.typ
    }
    fn name(&self) -> Option<Cow<'_, str>> {
        Some(Cow::Borrowed(self.name.as_ref()))
    }
    fn children(&self) -> Option<XmlGenericNodePtr> {
        self.children.map(|node| node.into())
    }
    fn set_children(&mut self, children: Option<XmlGenericNodePtr>) {
        self.children = children.map(|c| XmlNodePtr::try_from(c).unwrap());
    }
    fn last(&self) -> Option<XmlGenericNodePtr> {
        self.last.map(|node| node.into())
    }
    fn set_last(&mut self, last: Option<XmlGenericNodePtr>) {
        self.last = last.map(|l| XmlNodePtr::try_from(l).unwrap());
    }
    fn next(&self) -> Option<XmlGenericNodePtr> {
        self.next
    }
    fn set_next(&mut self, next: Option<XmlGenericNodePtr>) {
        self.next = next;
    }
    fn prev(&self) -> Option<XmlGenericNodePtr> {
        self.prev
    }
    fn set_prev(&mut self, prev: Option<XmlGenericNodePtr>) {
        self.prev = prev;
    }
    fn parent(&self) -> Option<XmlGenericNodePtr> {
        self.parent.map(|node| node.into())
    }
    fn set_parent(&mut self, parent: Option<XmlGenericNodePtr>) {
        self.parent = parent.map(|p| XmlDtdPtr::try_from(p).unwrap());
    }

    fn unlink(&mut self) {
        if let Some(doc) = self.document() {
            let entity = unsafe {
                // # Safety
                // Please see the document of `XmlEntityPtr::from_raw`.
                // In addition, this pointer is not leaked to the out of this function.
                XmlEntityPtr::from_raw(self).unwrap()
            };
            if let Some(mut int_subset) = doc.int_subset {
                if int_subset.entities.get(self.name.as_ref()).copied() == entity {
                    int_subset.entities.remove(self.name.as_ref());
                }
                if int_subset.pentities.get(self.name.as_ref()).copied() == entity {
                    int_subset.pentities.remove(self.name.as_ref());
                }
            }
            if let Some(mut ext_subset) = doc.ext_subset {
                if ext_subset.entities.get(self.name.as_ref()).copied() == entity {
                    ext_subset.entities.remove_entry(self.name.as_ref());
                }
                if ext_subset.pentities.get(self.name.as_ref()).copied() == entity {
                    ext_subset.pentities.remove(self.name.as_ref());
                }
            }
        }
        if let Some(mut parent) = self.parent() {
            if parent.children() == XmlGenericNodePtr::from_raw(self) {
                parent.set_children(self.next());
            }
            if parent.last() == XmlGenericNodePtr::from_raw(self) {
                parent.set_last(self.prev());
            }
            self.set_parent(None);
        }
        if let Some(mut next) = self.next() {
            next.set_prev(self.prev());
        }
        if let Some(mut prev) = self.prev() {
            prev.set_next(self.next());
        }
        self.set_next(None);
        self.set_prev(None);
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub struct XmlEntityPtr(NonNull<XmlEntity>);

impl XmlEntityPtr {
    /// Allocate new memory and create new `XmlEntityPtr` from an owned xml node.
    ///
    /// This method leaks allocated memory.  
    /// Users can use `free` method for deallocating memory.
    pub(crate) fn new(node: XmlEntity) -> Option<Self> {
        let boxed = Box::new(node);
        NonNull::new(Box::leak(boxed)).map(Self)
    }

    /// Create `XmlEntityPtr` from a raw pointer.  
    ///
    /// If `ptr` is a NULL pointer, return `Ok(None)`.  
    /// If `ptr` is a valid pointer of `XmlEntity`, return `Ok(Some(Self))`.  
    /// Otherwise, return `Err`.
    ///
    /// # Safety
    /// - `ptr` must be a pointer of types that is implemented `NodeCommon` at least.
    pub(crate) unsafe fn from_raw(
        ptr: *mut XmlEntity,
    ) -> Result<Option<Self>, InvalidNodePointerCastError> {
        unsafe {
            if ptr.is_null() {
                return Ok(None);
            }
            match (*ptr).element_type() {
                XmlElementType::XmlEntityDecl => Ok(Some(Self(NonNull::new_unchecked(ptr)))),
                _ => Err(InvalidNodePointerCastError {
                    from: (*ptr).element_type(),
                    to: type_name::<Self>(),
                }),
            }
        }
    }

    // pub(crate) fn as_ptr(self) -> *mut XmlEntity {
    //     self.0.as_ptr()
    // }

    /// Deallocate memory.
    ///
    /// # Safety
    /// This method should be called only once.  
    /// If called more than twice, the behavior is undefined.
    pub(crate) unsafe fn free(self) {
        unsafe {
            let _ = *Box::from_raw(self.0.as_ptr());
        }
    }

    // /// Acquire the ownership of the inner value.
    // /// As a result, `self` will be invalid. `self` must not be used after performs this method.
    // ///
    // /// # Safety
    // /// This method should be called only once.
    // /// If called more than twice, the behavior is undefined.
    // pub(crate) unsafe fn into_inner(self) -> Box<XmlEntity> {
    //     unsafe { Box::from_raw(self.0.as_ptr()) }
    // }
}

impl Default for XmlEntity {
    fn default() -> Self {
        Self {
            _private: null_mut(),
            typ: XmlElementType::XmlEntityDecl,
            name: "".into(),
            children: Default::default(),
            last: Default::default(),
            parent: Default::default(),
            next: Default::default(),
            prev: Default::default(),
            doc: Default::default(),
            orig: None,
            content: None,
            length: Default::default(),
            etype: Default::default(),
            external_id: None,
            system_id: None,
            nexte: Default::default(),
            uri: None,
            owner: Default::default(),
            flags: Default::default(),
            expanded_size: Default::default(),
        }
    }
}

impl Clone for XmlEntityPtr {
    fn clone(&self) -> Self {
        *self
    }
}

impl Copy for XmlEntityPtr {}

impl Deref for XmlEntityPtr {
    type Target = XmlEntity;
    fn deref(&self) -> &Self::Target {
        // # Safety
        // I don't implement the pointer casting and addition/subtraction methods
        // and don't expose the inner `NonNull` for `*mut XmlEntity`.
        // Therefore, as long as the constructor is correctly implemented,
        // the pointer dereference is valid.
        unsafe { self.0.as_ref() }
    }
}

impl DerefMut for XmlEntityPtr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // # Safety
        // I don't implement the pointer casting and addition/subtraction methods
        // and don't expose the inner `NonNull` for `*mut XmlEntity`.
        // Therefore, as long as the constructor is correctly implemented,
        // the pointer dereference is valid.
        unsafe { self.0.as_mut() }
    }
}

impl TryFrom<XmlGenericNodePtr> for XmlEntityPtr {
    type Error = InvalidNodePointerCastError;

    fn try_from(value: XmlGenericNodePtr) -> Result<Self, Self::Error> {
        match value.element_type() {
            XmlElementType::XmlEntityDecl => Ok(Self(value.0.cast())),
            _ => Err(InvalidNodePointerCastError {
                from: value.element_type(),
                to: type_name::<Self>(),
            }),
        }
    }
}

impl From<XmlEntityPtr> for XmlGenericNodePtr {
    fn from(value: XmlEntityPtr) -> Self {
        Self(value.0 as NonNull<dyn NodeCommon>)
    }
}

impl From<XmlEntityPtr> for *mut XmlEntity {
    fn from(value: XmlEntityPtr) -> Self {
        value.0.as_ptr()
    }
}

/// Handle an out of memory condition
#[doc(alias = "xmlEntitiesErrMemory")]
fn xml_entities_err_memory(extra: &str) {
    __xml_simple_oom_error(XmlErrorDomain::XmlFromTree, None, Some(extra));
}

/// internal routine doing the entity node structures allocations
#[doc(alias = "xmlCreateEntity")]
fn xml_create_entity(
    name: &str,
    typ: XmlEntityType,
    external_id: Option<&str>,
    system_id: Option<&str>,
    content: Option<&str>,
) -> Option<XmlEntityPtr> {
    let Some(mut ret) = XmlEntityPtr::new(XmlEntity {
        typ: XmlElementType::XmlEntityDecl,
        etype: typ,
        name: Cow::Owned(name.to_owned()),
        ..Default::default()
    }) else {
        xml_entities_err_memory("xmlCreateEntity: malloc failed");
        return None;
    };

    // fill the structure.
    if let Some(external_id) = external_id {
        ret.external_id = Some(external_id.to_owned().into_boxed_str());
    }
    if let Some(system_id) = system_id {
        ret.system_id = Some(system_id.to_owned().into_boxed_str());
    }
    if let Some(content) = content {
        ret.length = content.len() as i32;
        ret.content = Some(Cow::Owned(content.to_owned()));
    } else {
        ret.length = 0;
        ret.content = None;
    }
    // to be computed by the layer knowing the defining entity
    ret.uri = None;
    ret.orig = None;
    ret.owner = 0;

    Some(ret)
}

/// Create a new entity, this differs from xmlAddDocEntity() that if
/// the document is NULL or has no internal subset defined, then an
/// unlinked entity structure will be returned, it is then the responsibility
/// of the caller to link it to the document later or free it when not needed
/// anymore.
///
/// Returns a pointer to the entity or NULL in case of error
#[doc(alias = "xmlNewEntity")]
pub unsafe fn xml_new_entity(
    doc: Option<XmlDocPtr>,
    name: &str,
    typ: XmlEntityType,
    external_id: Option<&str>,
    system_id: Option<&str>,
    content: Option<&str>,
) -> Option<XmlEntityPtr> {
    unsafe {
        if let Some(doc) = doc.filter(|doc| doc.int_subset.is_some()) {
            return xml_add_doc_entity(doc, name, typ, external_id, system_id, content);
        }
        let mut ret = xml_create_entity(name, typ, external_id, system_id, content)?;
        ret.doc = doc;
        Some(ret)
    }
}

/// Raise an error.
#[doc(alias = "xmlEntitiesErr")]
fn xml_entities_err(code: XmlParserErrors, msg: &str) {
    __xml_simple_error!(XmlErrorDomain::XmlFromTree, code, None, msg);
}

/// Raise a warning.
#[doc(alias = "xmlEntitiesWarn")]
macro_rules! xml_entities_warn {
    ($code:expr, $msg:literal) => {
        xml_entities_warn!(@inner $code, $msg, None);
    };
    ($code:expr, $msg:literal, $str1:expr) => {
        let msg = format!($msg, $str1);
        xml_entities_warn!(@inner $code, &msg, Some($str1.to_owned().into()));
    };
    (@inner $code:expr, $msg:expr, $str1:expr) => {
        __xml_raise_error!(
            None,
            None,
            None,
            null_mut(),
            None,
            XmlErrorDomain::XmlFromTree,
            $code,
            $crate::error::XmlErrorLevel::XmlErrWarning,
            None,
            0,
            $str1,
            None,
            None,
            0,
            0,
            Some($msg),
        );
    };
}

/// clean-up an entity record.
#[doc(alias = "xmlFreeEntity")]
pub unsafe fn xml_free_entity(mut entity: XmlEntityPtr) {
    unsafe {
        if entity.owner == 1 {
            if let Some(children) = entity
                .children
                .filter(|&children| children.parent() == Some(entity.into()))
            {
                xml_free_node_list(Some(children));
                entity.children = None;
            }
        }
        entity.free();
    }
}

/// Register a new entity for an entities table.
#[doc(alias = "xmlAddEntity")]
unsafe fn xml_add_entity(
    mut dtd: XmlDtdPtr,
    name: &str,
    typ: XmlEntityType,
    external_id: Option<&str>,
    system_id: Option<&str>,
    content: Option<&str>,
) -> Option<XmlEntityPtr> {
    unsafe {
        let table = match typ {
            XmlEntityType::XmlInternalGeneralEntity
            | XmlEntityType::XmlExternalGeneralParsedEntity
            | XmlEntityType::XmlExternalGeneralUnparsedEntity => {
                if let Some(predef) = xml_get_predefined_entity(name) {
                    let mut valid: i32 = 0;

                    // 4.6 Predefined Entities
                    if typ == XmlEntityType::XmlInternalGeneralEntity {
                        if let Some(content) = content {
                            let c = predef.content.as_deref().unwrap().as_bytes()[0];
                            if content.as_bytes() == [c]
                                && (content == ">" || content == "\'" || content == "\"")
                            {
                                valid = 1;
                            } else if let Some(content) = content.strip_prefix("&#") {
                                if content.starts_with('x') {
                                    let hex = b"0123456789ABCDEF";
                                    let refe = [hex[c as usize >> 4], hex[c as usize & 0xF], b';'];
                                    if content.as_bytes()[1..].eq_ignore_ascii_case(&refe) {
                                        valid = 1;
                                    }
                                } else {
                                    let refe = [b'0' + (c / 10 % 10), b'0' + (c % 10), b';'];
                                    if content.as_bytes() == refe {
                                        valid = 1;
                                    }
                                }
                            }
                        }
                    }
                    if valid == 0 {
                        xml_entities_warn!(
                            XmlParserErrors::XmlErrEntityProcessing,
                            "xmlAddEntity: invalid redeclaration of predefined entity '{}'",
                            name
                        );
                        return None;
                    }
                }
                &mut dtd.entities
            }
            XmlEntityType::XmlInternalParameterEntity
            | XmlEntityType::XmlExternalParameterEntity => &mut dtd.pentities,
            XmlEntityType::XmlInternalPredefinedEntity => {
                return None;
            }
        };
        let mut ret = xml_create_entity(name, typ, external_id, system_id, content)?;

        match table.entry(name.to_owned()) {
            Entry::Occupied(_) => {
                // entity was already defined at another level.
                xml_free_entity(ret);
                return None;
            }
            Entry::Vacant(entry) => {
                entry.insert(ret);
            }
        }
        ret.doc = dtd.doc;
        Some(ret)
    }
}

/// Register a new entity for this document.
///
/// Returns a pointer to the entity or NULL in case of error
#[doc(alias = "xmlAddDocEntity")]
pub unsafe fn xml_add_doc_entity(
    doc: XmlDocPtr,
    name: &str,
    typ: XmlEntityType,
    external_id: Option<&str>,
    system_id: Option<&str>,
    content: Option<&str>,
) -> Option<XmlEntityPtr> {
    unsafe {
        let Some(mut dtd) = doc.int_subset else {
            xml_entities_err(
                XmlParserErrors::XmlDTDNoDTD,
                "xmlAddDocEntity: document without internal subset",
            );
            return None;
        };
        let mut ret = xml_add_entity(dtd, name, typ, external_id, system_id, content)?;

        // Link it to the DTD
        ret.set_parent(Some(dtd.into()));
        ret.set_document(dtd.doc);
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
}

/// Register a new entity for this document DTD external subset.
///
/// Returns a pointer to the entity or NULL in case of error
#[doc(alias = "xmlAddDtdEntity")]
pub unsafe fn xml_add_dtd_entity(
    doc: XmlDocPtr,
    name: &str,
    typ: XmlEntityType,
    external_id: Option<&str>,
    system_id: Option<&str>,
    content: Option<&str>,
) -> Option<XmlEntityPtr> {
    unsafe {
        let Some(mut dtd) = doc.ext_subset else {
            xml_entities_err(
                XmlParserErrors::XmlDTDNoDTD,
                "xmlAddDtdEntity: document without external subset",
            );
            return None;
        };
        let mut ret = xml_add_entity(dtd, name, typ, external_id, system_id, content)?;
        // Link it to the DTD
        ret.set_parent(Some(dtd.into()));
        ret.set_document(dtd.doc);
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
}

// The XML predefined entities.
thread_local! {
    static XML_ENTITY_LT: XmlEntity = const { XmlEntity {
        _private: null_mut(),
        typ: XmlElementType::XmlEntityDecl,
        name: Cow::Borrowed("lt"),
        children: None,
        last: None,
        parent: None,
        next: None,
        prev: None,
        doc: None,
        orig: Some(Cow::Borrowed("<")),
        content: Some(Cow::Borrowed("<")),
        length: 1,
        etype: XmlEntityType::XmlInternalPredefinedEntity,
        external_id: None,
        system_id: None,
        nexte: None,
        uri: None,
        owner: 0,
        flags: 0,
        expanded_size: 0,
    } };
    static XML_ENTITY_GT: XmlEntity = const { XmlEntity {
        _private: null_mut(),
        typ: XmlElementType::XmlEntityDecl,
        name: Cow::Borrowed("gt"),
        children: None,
        last: None,
        parent: None,
        next: None,
        prev: None,
        doc: None,
        orig: Some(Cow::Borrowed(">")),
        content: Some(Cow::Borrowed(">")),
        length: 1,
        etype: XmlEntityType::XmlInternalPredefinedEntity,
        external_id: None,
        system_id: None,
        nexte: None,
        uri: None,
        owner: 0,
        flags: 0,
        expanded_size: 0,
    } };
    static XML_ENTITY_AMP: XmlEntity = const { XmlEntity {
        _private: null_mut(),
        typ: XmlElementType::XmlEntityDecl,
        name: Cow::Borrowed("amp"),
        children: None,
        last: None,
        parent: None,
        next: None,
        prev: None,
        doc: None,
        orig: Some(Cow::Borrowed("&")),
        content: Some(Cow::Borrowed("&")),
        length: 1,
        etype: XmlEntityType::XmlInternalPredefinedEntity,
        external_id: None,
        system_id: None,
        nexte: None,
        uri: None,
        owner: 0,
        flags: 0,
        expanded_size: 0,
    } };
    static XML_ENTITY_QUOT: XmlEntity = const { XmlEntity {
        _private: null_mut(),
        typ: XmlElementType::XmlEntityDecl,
        name: Cow::Borrowed("quot"),
        children: None,
        last: None,
        parent: None,
        next: None,
        prev: None,
        doc: None,
        orig: Some(Cow::Borrowed("\"")),
        content: Some(Cow::Borrowed("\"")),
        length: 1,
        etype: XmlEntityType::XmlInternalPredefinedEntity,
        external_id: None,
        system_id: None,
        nexte: None,
        uri: None,
        owner: 0,
        flags: 0,
        expanded_size: 0,
    } };
    static XML_ENTITY_APOS: XmlEntity = const { XmlEntity {
        _private: null_mut(),
        typ: XmlElementType::XmlEntityDecl,
        name: Cow::Borrowed("apos"),
        children: None,
        last: None,
        parent: None,
        next: None,
        prev: None,
        doc: None,
        orig: Some(Cow::Borrowed("'")),
        content: Some(Cow::Borrowed("'")),
        length: 1,
        etype: XmlEntityType::XmlInternalPredefinedEntity,
        external_id: None,
        system_id: None,
        nexte: None,
        uri: None,
        owner: 0,
        flags: 0,
        expanded_size: 0,
    } };
}

/// Check whether this name is an predefined entity.
///
/// Returns NULL if not, otherwise the entity
#[doc(alias = "xmlGetPredefinedEntity")]
pub fn xml_get_predefined_entity(name: &str) -> Option<XmlEntityPtr> {
    unsafe {
        match name {
            "lt" => XML_ENTITY_LT.with(|ptr| {
                XmlEntityPtr::from_raw(ptr as *const XmlEntity as *mut XmlEntity).unwrap()
            }),
            "gt" => XML_ENTITY_GT.with(|ptr| {
                XmlEntityPtr::from_raw(ptr as *const XmlEntity as *mut XmlEntity).unwrap()
            }),
            "amp" => XML_ENTITY_AMP.with(|ptr| {
                XmlEntityPtr::from_raw(ptr as *const XmlEntity as *mut XmlEntity).unwrap()
            }),
            "apos" => XML_ENTITY_APOS.with(|ptr| {
                XmlEntityPtr::from_raw(ptr as *const XmlEntity as *mut XmlEntity).unwrap()
            }),
            "quot" => XML_ENTITY_QUOT.with(|ptr| {
                XmlEntityPtr::from_raw(ptr as *const XmlEntity as *mut XmlEntity).unwrap()
            }),
            _ => None,
        }
    }
}

/// Do an entity lookup in the document entity hash table and
/// returns the corresponding entity, otherwise a lookup is done
/// in the predefined entities too.
///
/// Returns A pointer to the entity structure or NULL if not found.
#[doc(alias = "xmlGetDocEntity")]
pub fn xml_get_doc_entity(doc: Option<XmlDocPtr>, name: &str) -> Option<XmlEntityPtr> {
    if let Some(doc) = doc {
        if let Some(int_subset) = doc.int_subset {
            let cur = int_subset.entities.get(name).copied();
            if cur.is_some() {
                return cur;
            }
        }
        if doc.standalone != 1 {
            if let Some(ext_subset) = doc.ext_subset {
                let cur = ext_subset.entities.get(name).copied();
                if cur.is_some() {
                    return cur;
                }
            }
        }
    }
    xml_get_predefined_entity(name)
}

/// Do an entity lookup in the DTD entity hash table and
/// returns the corresponding entity, if found.
/// Note: the first argument is the document node, not the DTD node.
///
/// Returns A pointer to the entity structure or NULL if not found.
#[doc(alias = "xmlGetDtdEntity")]
pub fn xml_get_dtd_entity(doc: XmlDocPtr, name: &str) -> Option<XmlEntityPtr> {
    doc.ext_subset
        .and_then(|ext| ext.entities.get(name).copied())
}

/// Do an entity lookup in the internal and external subsets and
/// returns the corresponding parameter entity, if found.
///
/// Returns A pointer to the entity structure or NULL if not found.
#[doc(alias = "xmlGetParameterEntity")]
pub fn xml_get_parameter_entity(doc: XmlDocPtr, name: &str) -> Option<XmlEntityPtr> {
    if let Some(int_subset) = doc.int_subset {
        let ret = int_subset.pentities.get(name).copied();
        if ret.is_some() {
            return ret;
        }
    }
    doc.ext_subset
        .and_then(|ext| ext.pentities.get(name).copied())
}

/// Do a global encoding of a string, replacing the predefined entities
/// and non ASCII values with their entities and CharRef counterparts.
/// Contrary to xmlEncodeEntities, this routine is reentrant, and result
/// must be deallocated.
///
/// Returns A newly allocated string with the substitution done.
#[doc(alias = "xmlEncodeEntitiesInternal")]
fn xml_encode_entities_internal(doc: Option<XmlDocPtr>, input: &str, attr: i32) -> String {
    let mut html = false;

    if let Some(doc) = doc {
        html = matches!(doc.typ, XmlElementType::XmlHTMLDocumentNode);
    }

    // allocate an translation buffer.
    let mut cur = input;
    let mut out = String::with_capacity(1000);
    while !cur.is_empty() {
        // By default one have to encode at least '<', '>', '"' and '&' !
        if cur.starts_with('<') {
            // Special handling of server side include in HTML attributes
            if html && attr != 0 && cur.starts_with("<!--") {
                if let Some(pos) = cur.find("-->") {
                    out.push_str(&cur[..pos + 3]);
                    cur = &cur[pos + 3..];
                    continue;
                }
            }
            out.push_str("&lt;");
        } else if cur.starts_with('>') {
            out.push_str("&gt;");
        } else if cur.starts_with('&') {
            // Special handling of &{...} construct from HTML 4, see
            // http://www.w3.org/TR/html401/appendix/notes.html#h-B.7.1
            if html && attr != 0 && cur[1..].starts_with('{') {
                if let Some(pos) = cur.find('}') {
                    out.push_str(&cur[..pos + 1]);
                    cur = &cur[pos + 1..];
                    continue;
                }
            }
            out.push_str("&amp;");
        } else if matches!(cur.as_bytes()[0], 0x20..0x80 | b'\n' | b'\t')
            || (html && cur.starts_with('\r'))
        {
            // default case, just copy !
            out.push(cur.as_bytes()[0] as char);
        } else if matches!(cur.as_bytes()[0], 0x80..) {
            if doc.is_some_and(|doc| doc.encoding.is_some()) || html {
                // Bjørn Reese <br@sseusa.com> provided the patch
                // XmlChar xc;
                // xc = (*cur & 0x3F) << 6;
                // if (*cur.add(1) != 0) {
                //     xc += *(++cur) & 0x3F;
                //     *out++ = xc;
                // } else
                let c = cur.chars().next().unwrap();
                out.push(c);
                cur = &cur[c.len_utf8()..];
                continue;
            } else {
                let val = cur.chars().next().unwrap();
                if !val.is_xml_char() {
                    xml_entities_err(
                        XmlParserErrors::XmlErrInvalidChar,
                        "xmlEncodeEntities: char out of range\n",
                    );
                    if let Some(mut doc) = doc {
                        doc.encoding = Some("ISO-8859-1".to_owned());
                    }
                    out.push_str(format!("&#{}", cur.as_bytes()[0]).as_str());
                    cur = &cur[1..];
                    continue;
                }
                // We could do multiple things here. Just save as a c_char ref
                out.push_str(format!("&#x{:X}", val as u32).as_str());
                cur = &cur[val.len_utf8()..];
                continue;
            }
        } else if cur.as_bytes()[0].is_xml_char() {
            out.push_str(format!("&#{};", cur.as_bytes()[0]).as_str());
        }
        cur = &cur[1..];
    }
    out
}

/// Do a global encoding of a string, replacing the predefined entities
/// and non ASCII values with their entities and CharRef counterparts.
/// Contrary to xmlEncodeEntities, this routine is reentrant, and result
/// must be deallocated.
///
/// Returns A newly allocated string with the substitution done.
#[doc(alias = "xmlEncodeEntitiesReentrant")]
pub fn xml_encode_entities_reentrant(doc: Option<XmlDocPtr>, input: &str) -> String {
    xml_encode_entities_internal(doc, input, 0)
}

/// Do a global encoding of a string, replacing the predefined entities
/// this routine is reentrant, and result must be deallocated.
///
/// Returns A newly allocated string with the substitution done.
#[doc(alias = "xmlEncodeSpecialChars")]
pub fn xml_encode_special_chars(_doc: Option<XmlDocPtr>, input: &str) -> String {
    // allocate an translation buffer.
    let mut out = String::with_capacity(1000);
    for cur in input.chars() {
        // By default one have to encode at least '<', '>', '"' and '&' !
        if cur == '<' {
            out.push_str("&lt;");
        } else if cur == '>' {
            out.push_str("&gt;");
        } else if cur == '&' {
            out.push_str("&amp;");
        } else if cur == '"' {
            out.push_str("&quot;");
        } else if cur == '\r' {
            out.push_str("&#13;");
        } else {
            out.push(cur);
        }
    }
    out
}

/// Build a copy of an entity
///
/// Returns the new xmlEntitiesPtr or NULL in case of error.
#[doc(alias = "xmlCopyEntity")]
#[cfg(feature = "libxml_tree")]
pub fn xml_copy_entity(ent: XmlEntityPtr) -> Option<XmlEntityPtr> {
    let mut cur = XmlEntityPtr::new(XmlEntity {
        typ: XmlElementType::XmlEntityDecl,
        etype: ent.etype,
        ..Default::default()
    })
    .unwrap();
    cur.name = ent.name.clone();
    cur.external_id = ent.external_id.clone();
    cur.system_id = ent.system_id.clone();
    cur.content = ent.content.clone();
    cur.orig = ent.orig.clone();
    cur.uri = ent.uri.clone();
    Some(cur)
}

/// This will dump the quoted string value, taking care of the special
/// treatment required by %
#[doc(alias = "xmlDumpEntityContent")]
#[cfg(feature = "libxml_output")]
fn xml_dump_entity_content<'a>(buf: &mut (impl Write + 'a), content: &str) {
    use crate::io::write_quoted;

    if content.contains('%') {
        write!(buf, "\"").ok();
        let mut cur = content;
        while let Some(pos) = cur.find(['"', '%']) {
            let (head, tail) = cur.split_at(pos);
            write!(buf, "{head}").ok();
            if tail.starts_with('"') {
                write!(buf, "&quot;").ok();
            } else {
                write!(buf, "&#x25;").ok();
            }
            cur = &tail[1..];
        }
        if !cur.is_empty() {
            write!(buf, "{cur}").ok();
        }

        write!(buf, "\"").ok();
    } else {
        write_quoted(buf, content).ok();
    }
}

/// This will dump the content of the entity table as an XML DTD definition
#[doc(alias = "xmlDumpEntityDecl")]
#[cfg(feature = "libxml_output")]
pub fn xml_dump_entity_decl<'a>(buf: &mut (impl Write + 'a), ent: XmlEntityPtr) {
    use crate::io::write_quoted;

    let name = &ent.name;
    match ent.etype {
        XmlEntityType::XmlInternalGeneralEntity => {
            write!(buf, "<!ENTITY {name} ").ok();
            if let Some(orig) = ent.orig.as_deref() {
                write_quoted(buf, orig).ok();
            } else {
                xml_dump_entity_content(buf, ent.content.as_deref().unwrap());
            }
            writeln!(buf, ">").ok();
        }
        XmlEntityType::XmlExternalGeneralParsedEntity => {
            write!(buf, "<!ENTITY {name}").ok();
            if let Some(external_id) = ent.external_id.as_deref() {
                write!(buf, " PUBLIC ").ok();
                write_quoted(buf, external_id).ok();
                write!(buf, " ").ok();
                write_quoted(buf, ent.system_id.as_deref().unwrap()).ok();
            } else {
                write!(buf, " SYSTEM ").ok();
                write_quoted(buf, ent.system_id.as_deref().unwrap()).ok();
            }
            writeln!(buf, ">").ok();
        }
        XmlEntityType::XmlExternalGeneralUnparsedEntity => {
            write!(buf, "<!ENTITY {name}").ok();
            if let Some(external_id) = ent.external_id.as_deref() {
                write!(buf, " PUBLIC ").ok();
                write_quoted(buf, external_id).ok();
                write!(buf, " ").ok();
                write_quoted(buf, ent.system_id.as_deref().unwrap()).ok();
            } else {
                write!(buf, " SYSTEM ").ok();
                write_quoted(buf, ent.system_id.as_deref().unwrap()).ok();
            }
            if let Some(content) = ent.content.as_deref() {
                // Should be true !
                write!(buf, " NDATA ").ok();
                if let Some(orig) = ent.orig.as_deref() {
                    write!(buf, "{}", orig).ok();
                } else {
                    write!(buf, "{}", content).ok();
                }
            }
            writeln!(buf, ">").ok();
        }
        XmlEntityType::XmlInternalParameterEntity => {
            write!(buf, "<!ENTITY % {name} ").ok();
            if let Some(orig) = ent.orig.as_deref() {
                write_quoted(buf, orig).ok();
            } else {
                xml_dump_entity_content(buf, ent.content.as_deref().unwrap());
            }
            writeln!(buf, ">").ok();
        }
        XmlEntityType::XmlExternalParameterEntity => {
            write!(buf, "<!ENTITY % {name}").ok();
            if let Some(external_id) = ent.external_id.as_deref() {
                write!(buf, " PUBLIC ").ok();
                write_quoted(buf, external_id).ok();
                write!(buf, " ").ok();
                write_quoted(buf, ent.system_id.as_deref().unwrap()).ok();
            } else {
                write!(buf, " SYSTEM ").ok();
                write_quoted(buf, ent.system_id.as_deref().unwrap()).ok();
            }
            writeln!(buf, ">").ok();
        }
        _ => {
            xml_entities_err(
                XmlParserErrors::XmlDTDUnknownEntity,
                "xmlDumpEntitiesDecl: internal: unknown type entity type",
            );
        }
    }
}

// Entity flags
//
// XML_ENT_PARSED: The entity was parsed and `children` points to the content.
// XML_ENT_CHECKED: The entity was checked for loops.
pub(crate) const XML_ENT_PARSED: usize = 1 << 0;
pub(crate) const XML_ENT_CHECKED: usize = 1 << 1;
pub(crate) const XML_ENT_EXPANDING: usize = 1 << 2;
pub(crate) const XML_ENT_CHECKED_LT: usize = 1 << 3;
pub(crate) const XML_ENT_CONTAINS_LT: usize = 1 << 4;

/// Do a global encoding of a string, replacing the predefined entities
/// and non ASCII values with their entities and CharRef counterparts for attribute values.
///
/// Returns A newly allocated string with the substitution done.
#[doc(alias = "xmlEncodeAttributeEntities")]
pub(crate) fn xml_encode_attribute_entities(doc: Option<XmlDocPtr>, input: &str) -> String {
    xml_encode_entities_internal(doc, input, 1)
}
