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
    ffi::{CStr, CString, c_char},
    ops::{Deref, DerefMut},
    os::raw::c_void,
    ptr::{NonNull, null_mut},
};

use libc::{size_t, snprintf, strchr};

use crate::{
    error::{
        __xml_raise_error, __xml_simple_error, __xml_simple_oom_error, XmlErrorDomain,
        XmlParserErrors,
    },
    hash::{CVoidWrapper, XmlHashTableRef},
    libxml::{
        chvalid::xml_is_char,
        globals::{xml_free, xml_malloc},
        hash::{XmlHashTable, xml_hash_create},
        xmlstring::{XmlChar, xml_strchr, xml_strdup, xml_strndup, xml_strstr},
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

impl TryFrom<i32> for XmlEntityType {
    type Error = anyhow::Error;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        if value == 1 {
            Ok(Self::XmlInternalGeneralEntity)
        } else if value == 2 {
            Ok(Self::XmlExternalGeneralParsedEntity)
        } else if value == 3 {
            Ok(Self::XmlExternalGeneralUnparsedEntity)
        } else if value == 4 {
            Ok(Self::XmlInternalParameterEntity)
        } else if value == 5 {
            Ok(Self::XmlExternalParameterEntity)
        } else if value == 6 {
            Ok(Self::XmlInternalPredefinedEntity)
        } else {
            Err(anyhow::anyhow!(
                "Invalid convert from value '{value}' to {}",
                type_name::<Self>()
            ))
        }
    }
}

#[repr(C)]
pub struct XmlEntity {
    // application data
    pub _private: *mut c_void,
    // XML_ENTITY_DECL, must be second !
    pub(crate) typ: XmlElementType,
    // Entity name
    pub(crate) name: *mut u8,
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
    pub(crate) orig: *mut u8,
    // content or ndata if unparsed
    pub(crate) content: *mut u8,
    // the content length
    pub(crate) length: i32,
    // The entity type
    pub(crate) etype: XmlEntityType,
    // External identifier for PUBLIC
    pub(crate) external_id: Option<Box<str>>,
    // URI for a SYSTEM or PUBLIC Entity
    pub(crate) system_id: *mut u8,

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
        let name = self.name;
        (!name.is_null()).then(|| unsafe { CStr::from_ptr(name as *const i8).to_string_lossy() })
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

    /// Acquire the ownership of the inner value.  
    /// As a result, `self` will be invalid. `self` must not be used after performs this method.
    ///
    /// # Safety
    /// This method should be called only once.  
    /// If called more than twice, the behavior is undefined.
    pub(crate) unsafe fn into_inner(self) -> Box<XmlEntity> {
        unsafe { Box::from_raw(self.0.as_ptr()) }
    }
}

impl Default for XmlEntity {
    fn default() -> Self {
        Self {
            _private: null_mut(),
            typ: XmlElementType::XmlEntityDecl,
            name: null_mut(),
            children: Default::default(),
            last: Default::default(),
            parent: Default::default(),
            next: Default::default(),
            prev: Default::default(),
            doc: Default::default(),
            orig: null_mut(),
            content: null_mut(),
            length: Default::default(),
            etype: Default::default(),
            external_id: None,
            system_id: null_mut(),
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

/// All entities are stored in an hash table.
/// There is 2 separate hash tables for global and parameter entities.
pub type XmlEntitiesTable = XmlHashTable<'static, CVoidWrapper>;
pub type XmlEntitiesTablePtr = *mut XmlEntitiesTable;

/// Handle an out of memory condition
#[doc(alias = "xmlEntitiesErrMemory")]
fn xml_entities_err_memory(extra: &str) {
    __xml_simple_oom_error(XmlErrorDomain::XmlFromTree, None, Some(extra));
}

/// internal routine doing the entity node structures allocations
#[doc(alias = "xmlCreateEntity")]
unsafe fn xml_create_entity(
    name: &str,
    typ: XmlEntityType,
    external_id: Option<&str>,
    system_id: Option<&str>,
    content: Option<&str>,
) -> Option<XmlEntityPtr> {
    unsafe {
        let Some(mut ret) = XmlEntityPtr::new(XmlEntity {
            typ: XmlElementType::XmlEntityDecl,
            etype: typ,
            name: xml_strndup(name.as_ptr(), name.len() as i32),
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
            let system_id = CString::new(system_id).unwrap();
            ret.system_id = xml_strdup(system_id.as_ptr() as *const u8);
        }
        if let Some(content) = content {
            ret.length = content.len() as i32;
            let content = CString::new(content).unwrap();
            ret.content = xml_strndup(content.as_ptr() as *const u8, ret.length);
        } else {
            ret.length = 0;
            ret.content = null_mut();
        }
        // to be computed by the layer knowing the defining entity
        ret.uri = None;
        ret.orig = null_mut();
        ret.owner = 0;

        Some(ret)
    }
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
unsafe fn xml_free_entity(mut entity: XmlEntityPtr) {
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
        if !entity.name.is_null() {
            xml_free(entity.name as _);
            entity.name = null_mut();
        }
        if !entity.system_id.is_null() {
            xml_free(entity.system_id as _);
            entity.system_id = null_mut();
        }
        if !entity.content.is_null() {
            xml_free(entity.content as _);
            entity.content = null_mut();
        }
        if !entity.orig.is_null() {
            xml_free(entity.orig as _);
            entity.orig = null_mut();
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
                            let c = *predef.content.add(0);
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
                if dtd.entities.is_none() {
                    let table = XmlHashTable::with_capacity(0);
                    dtd.entities = XmlHashTableRef::from_table(table);
                }
                dtd.entities
            }
            XmlEntityType::XmlInternalParameterEntity
            | XmlEntityType::XmlExternalParameterEntity => {
                if dtd.pentities.is_none() {
                    let table = XmlHashTable::with_capacity(0);
                    dtd.pentities = XmlHashTableRef::from_table(table);
                }
                dtd.pentities
            }
            XmlEntityType::XmlInternalPredefinedEntity => {
                return None;
            }
        };
        let mut table = table?;
        let mut ret = xml_create_entity(name, typ, external_id, system_id, content)?;
        ret.doc = dtd.doc;

        if table.add_entry(name, ret).is_err() {
            // entity was already defined at another level.
            xml_free_entity(ret);
            return None;
        }
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
        name: c"lt".as_ptr() as _,
        children: None,
        last: None,
        parent: None,
        next: None,
        prev: None,
        doc: None,
        orig: c"<".as_ptr() as _,
        content: c"<".as_ptr() as _,
        length: 1,
        etype: XmlEntityType::XmlInternalPredefinedEntity,
        external_id: None,
        system_id: null_mut(),
        nexte: None,
        uri: None,
        owner: 0,
        flags: 0,
        expanded_size: 0,
    } };
    static XML_ENTITY_GT: XmlEntity = const { XmlEntity {
        _private: null_mut(),
        typ: XmlElementType::XmlEntityDecl,
        name: c"gt".as_ptr() as _,
        children: None,
        last: None,
        parent: None,
        next: None,
        prev: None,
        doc: None,
        orig: c">".as_ptr() as _,
        content: c">".as_ptr() as _,
        length: 1,
        etype: XmlEntityType::XmlInternalPredefinedEntity,
        external_id: None,
        system_id: null_mut(),
        nexte: None,
        uri: None,
        owner: 0,
        flags: 0,
        expanded_size: 0,
    } };
    static XML_ENTITY_AMP: XmlEntity = const { XmlEntity {
        _private: null_mut(),
        typ: XmlElementType::XmlEntityDecl,
        name: c"amp".as_ptr() as _,
        children: None,
        last: None,
        parent: None,
        next: None,
        prev: None,
        doc: None,
        orig: c"&".as_ptr() as _,
        content: c"&".as_ptr() as _,
        length: 1,
        etype: XmlEntityType::XmlInternalPredefinedEntity,
        external_id: None,
        system_id: null_mut(),
        nexte: None,
        uri: None,
        owner: 0,
        flags: 0,
        expanded_size: 0,
    } };
    static XML_ENTITY_QUOT: XmlEntity = const { XmlEntity {
        _private: null_mut(),
        typ: XmlElementType::XmlEntityDecl,
        name: c"quot".as_ptr() as _,
        children: None,
        last: None,
        parent: None,
        next: None,
        prev: None,
        doc: None,
        orig: c"\"".as_ptr() as _,
        content: c"\"".as_ptr() as _,
        length: 1,
        etype: XmlEntityType::XmlInternalPredefinedEntity,
        external_id: None,
        system_id: null_mut(),
        nexte: None,
        uri: None,
        owner: 0,
        flags: 0,
        expanded_size: 0,
    } };
    static XML_ENTITY_APOS: XmlEntity = const { XmlEntity {
        _private: null_mut(),
        typ: XmlElementType::XmlEntityDecl,
        name: c"apos".as_ptr() as _,
        children: None,
        last: None,
        parent: None,
        next: None,
        prev: None,
        doc: None,
        orig: c"'".as_ptr() as _,
        content: c"'".as_ptr() as _,
        length: 1,
        etype: XmlEntityType::XmlInternalPredefinedEntity,
        external_id: None,
        system_id: null_mut(),
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
pub unsafe fn xml_get_predefined_entity(name: &str) -> Option<XmlEntityPtr> {
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

/// Do an entity lookup in the table.
/// returns the corresponding parameter entity, if found.
///
/// Returns A pointer to the entity structure or NULL if not found.
#[doc(alias = "xmlGetEntityFromTable")]
unsafe fn xml_get_entity_from_table(
    table: XmlHashTableRef<'static, XmlEntityPtr>,
    name: &str,
) -> Option<XmlEntityPtr> {
    table.lookup(name).copied()
}

/// Do an entity lookup in the document entity hash table and
/// returns the corresponding entity, otherwise a lookup is done
/// in the predefined entities too.
///
/// Returns A pointer to the entity structure or NULL if not found.
#[doc(alias = "xmlGetDocEntity")]
pub unsafe fn xml_get_doc_entity(doc: Option<XmlDocPtr>, name: &str) -> Option<XmlEntityPtr> {
    unsafe {
        if let Some(doc) = doc {
            if let Some(int_subset) = doc.int_subset {
                if let Some(table) = int_subset.entities {
                    let cur = xml_get_entity_from_table(table, name);
                    if cur.is_some() {
                        return cur;
                    }
                }
            }
            if doc.standalone != 1 {
                if let Some(ext_subset) = doc.ext_subset {
                    if let Some(table) = ext_subset.entities {
                        let cur = xml_get_entity_from_table(table, name);
                        if cur.is_some() {
                            return cur;
                        }
                    }
                }
            }
        }
        xml_get_predefined_entity(name)
    }
}

/// Do an entity lookup in the DTD entity hash table and
/// returns the corresponding entity, if found.
/// Note: the first argument is the document node, not the DTD node.
///
/// Returns A pointer to the entity structure or NULL if not found.
#[doc(alias = "xmlGetDtdEntity")]
pub unsafe fn xml_get_dtd_entity(doc: XmlDocPtr, name: &str) -> Option<XmlEntityPtr> {
    unsafe {
        // if doc.is_null() {
        //     return None;
        // }
        if let Some(ext_subset) = doc.ext_subset {
            if let Some(table) = ext_subset.entities {
                return xml_get_entity_from_table(table, name);
            }
        }
        None
    }
}

/// Do an entity lookup in the internal and external subsets and
/// returns the corresponding parameter entity, if found.
///
/// Returns A pointer to the entity structure or NULL if not found.
#[doc(alias = "xmlGetParameterEntity")]
pub unsafe fn xml_get_parameter_entity(doc: XmlDocPtr, name: &str) -> Option<XmlEntityPtr> {
    unsafe {
        // if doc.is_null() {
        //     return None;
        // }
        if let Some(int_subset) = doc.int_subset {
            if let Some(table) = int_subset.pentities {
                let ret = xml_get_entity_from_table(table, name);
                if ret.is_some() {
                    return ret;
                }
            }
        }
        if let Some(ext_subset) = doc.ext_subset {
            if let Some(table) = ext_subset.pentities {
                return xml_get_entity_from_table(table, name);
            }
        }
        None
    }
}

// Macro used to grow the current buffer.
macro_rules! grow_buffer_reentrant {
    ($buffer:expr, $buffer_size:expr, $mem_error:tt) => {
        let tmp: *mut XmlChar;
        let new_size: size_t = $buffer_size * 2;
        if new_size < $buffer_size {
            break $mem_error;
        }
        tmp = $crate::libxml::globals::xml_realloc($buffer as _, new_size) as *mut XmlChar;
        if tmp.is_null() {
            break $mem_error;
        }
        $buffer = tmp;
        $buffer_size = new_size;
    };
}

/// Do a global encoding of a string, replacing the predefined entities
/// and non ASCII values with their entities and CharRef counterparts.
/// Contrary to xmlEncodeEntities, this routine is reentrant, and result
/// must be deallocated.
///
/// Returns A newly allocated string with the substitution done.
#[doc(alias = "xmlEncodeEntitiesInternal")]
pub(crate) unsafe fn xml_encode_entities_internal(
    doc: Option<XmlDocPtr>,
    input: *const XmlChar,
    attr: i32,
) -> *mut XmlChar {
    unsafe {
        let mut cur: *const XmlChar = input;
        let mut buffer: *mut XmlChar;
        let mut out: *mut XmlChar;
        let mut buffer_size: size_t;
        let mut html: i32 = 0;

        if input.is_null() {
            return null_mut();
        }
        if let Some(doc) = doc {
            html = matches!(doc.typ, XmlElementType::XmlHTMLDocumentNode) as i32;
        }

        // allocate an translation buffer.
        buffer_size = 1000;
        buffer = xml_malloc(buffer_size) as *mut XmlChar;
        if buffer.is_null() {
            xml_entities_err_memory("xmlEncodeEntities: malloc failed");
            return null_mut();
        }
        out = buffer;

        'mem_error: {
            while *cur != b'\0' {
                let mut indx: size_t = out.offset_from(buffer) as _;
                if indx + 100 > buffer_size {
                    grow_buffer_reentrant!(buffer, buffer_size, 'mem_error);
                    out = buffer.add(indx);
                }

                // By default one have to encode at least '<', '>', '"' and '&' !
                if *cur == b'<' {
                    let end: *const XmlChar;

                    // Special handling of server side include in HTML attributes
                    if html != 0
                        && attr != 0
                        && *cur.add(1) == b'!'
                        && *cur.add(2) == b'-'
                        && *cur.add(3) == b'-'
                        && {
                            end = xml_strstr(cur, c"-->".as_ptr() as _);
                            !end.is_null()
                        }
                    {
                        while cur != end {
                            *out = *cur;
                            cur = cur.add(1);
                            out = out.add(1);
                            indx = out.offset_from(buffer) as _;
                            if indx + 100 > buffer_size {
                                grow_buffer_reentrant!(buffer, buffer_size, 'mem_error);
                                out = buffer.add(indx);
                            }
                        }
                        *out = *cur;
                        cur = cur.add(1);
                        out = out.add(1);
                        *out = *cur;
                        cur = cur.add(1);
                        out = out.add(1);
                        *out = *cur;
                        cur = cur.add(1);
                        out = out.add(1);
                        continue;
                    }
                    *out = b'&';
                    out = out.add(1);
                    *out = b'l';
                    out = out.add(1);
                    *out = b't';
                    out = out.add(1);
                    *out = b';';
                    out = out.add(1);
                } else if *cur == b'>' {
                    *out = b'&';
                    out = out.add(1);
                    *out = b'g';
                    out = out.add(1);
                    *out = b't';
                    out = out.add(1);
                    *out = b';';
                    out = out.add(1);
                } else if *cur == b'&' {
                    // Special handling of &{...} construct from HTML 4, see
                    // http://www.w3.org/TR/html401/appendix/notes.html#h-B.7.1
                    if html != 0
                        && attr != 0
                        && *cur.add(1) == b'{'
                        && !strchr(cur as _, b'}' as _).is_null()
                    {
                        while *cur != b'}' {
                            *out = *cur;
                            cur = cur.add(1);
                            out = out.add(1);
                            indx = out.offset_from(buffer) as _;
                            if indx + 100 > buffer_size {
                                grow_buffer_reentrant!(buffer, buffer_size, 'mem_error);
                                out = buffer.add(indx as usize);
                            }
                        }
                        *out = *cur;
                        cur = cur.add(1);
                        out = out.add(1);
                        continue;
                    }
                    *out = b'&';
                    out = out.add(1);
                    *out = b'a';
                    out = out.add(1);
                    *out = b'm';
                    out = out.add(1);
                    *out = b'p';
                    out = out.add(1);
                    *out = b';';
                    out = out.add(1);
                } else if (*cur >= 0x20 && *cur < 0x80)
                    || *cur == b'\n'
                    || *cur == b'\t'
                    || (html != 0 && *cur == b'\r')
                {
                    // default case, just copy !
                    *out = *cur;
                    out = out.add(1);
                } else if *cur >= 0x80 {
                    if doc.is_some_and(|doc| doc.encoding.is_some()) || html != 0 {
                        // Bjørn Reese <br@sseusa.com> provided the patch
                        // XmlChar xc;
                        // xc = (*cur & 0x3F) << 6;
                        // if (*cur.add(1) != 0) {
                        //     xc += *(++cur) & 0x3F;
                        //     *out++ = xc;
                        // } else
                        *out = *cur;
                        out = out.add(1);
                    } else {
                        // We assume we have UTF-8 input.
                        // It must match either:
                        //   110xxxxx 10xxxxxx
                        //   1110xxxx 10xxxxxx 10xxxxxx
                        //   11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
                        // That is:
                        //   *cur.add(0) is 11xxxxxx
                        //   *cur.add(1) is 10xxxxxx
                        //   *cur.add(2) is 10xxxxxx if *cur.add(0) is 111xxxxx
                        //   *cur.add(3) is 10xxxxxx if *cur.add(0) is 1111xxxx
                        //   *cur.add(0) is not 11111xxx
                        let mut buf: [c_char; 11] = [0; 11];
                        let mut ptr: *mut c_char;
                        let mut val: i32 = 0;
                        let mut l: i32 = 1;

                        if *cur.add(0) & 0xC0 != 0xC0
                            || *cur.add(1) & 0xC0 != 0x80
                            || (*cur.add(0) & 0xE0 == 0xE0 && *cur.add(2) & 0xC0 != 0x80)
                            || (*cur.add(0) & 0xF0 == 0xF0 && *cur.add(3) & 0xC0 != 0x80)
                            || *cur.add(0) & 0xF8 == 0xF8
                        {
                            xml_entities_err(
                                XmlParserErrors::XmlCheckNotUTF8,
                                "xmlEncodeEntities: input not UTF-8",
                            );
                            if let Some(mut doc) = doc {
                                doc.encoding = Some("ISO-8859-1".to_owned());
                            }
                            snprintf(
                                buf.as_mut_ptr() as _,
                                buf.len(),
                                c"&#%d;".as_ptr() as _,
                                *cur as u32,
                            );
                            *buf.last_mut().unwrap() = 0;
                            ptr = buf.as_ptr() as _;
                            while *ptr != 0 {
                                *out = *ptr as _;
                                out = out.add(1);
                                ptr = ptr.add(1);
                            }
                            cur = cur.add(1);
                            continue;
                        } else if *cur < 0xE0 {
                            val = *cur.add(0) as i32 & 0x1F;
                            val <<= 6;
                            val |= *cur.add(1) as i32 & 0x3F;
                            l = 2;
                        } else if *cur < 0xF0 {
                            val = *cur.add(0) as i32 & 0x0F;
                            val <<= 6;
                            val |= *cur.add(1) as i32 & 0x3F;
                            val <<= 6;
                            val |= *cur.add(2) as i32 & 0x3F;
                            l = 3;
                        } else if *cur < 0xF8 {
                            val = *cur.add(0) as i32 & 0x07;
                            val <<= 6;
                            val |= *cur.add(1) as i32 & 0x3F;
                            val <<= 6;
                            val |= *cur.add(2) as i32 & 0x3F;
                            val <<= 6;
                            val |= *cur.add(3) as i32 & 0x3F;
                            l = 4;
                        }
                        if l == 1 || !xml_is_char(val as u32) {
                            xml_entities_err(
                                XmlParserErrors::XmlErrInvalidChar,
                                "xmlEncodeEntities: char out of range\n",
                            );
                            if let Some(mut doc) = doc {
                                doc.encoding = Some("ISO-8859-1".to_owned());
                            }
                            snprintf(
                                buf.as_mut_ptr() as _,
                                buf.len(),
                                c"&#%d;".as_ptr() as _,
                                *cur as u32,
                            );
                            buf[buf.len() - 1] = 0;
                            ptr = buf.as_ptr() as _;
                            while *ptr != 0 {
                                *out = *ptr as _;
                                out = out.add(1);
                                ptr = ptr.add(1);
                            }
                            cur = cur.add(1);
                            continue;
                        }
                        // We could do multiple things here. Just save as a c_char ref
                        snprintf(
                            buf.as_mut_ptr() as _,
                            buf.len(),
                            c"&#x%X;".as_ptr() as _,
                            val,
                        );
                        buf[buf.len() - 1] = 0;
                        ptr = buf.as_ptr() as _;
                        while *ptr != 0 {
                            *out = *ptr as _;
                            out = out.add(1);
                            ptr = ptr.add(1);
                        }
                        cur = cur.add(l as usize);
                        continue;
                    }
                } else if xml_is_char(*cur as u32) {
                    let mut buf: [c_char; 11] = [0; 11];
                    let mut ptr: *mut c_char;

                    snprintf(
                        buf.as_mut_ptr() as _,
                        buf.len(),
                        c"&#%d;".as_ptr() as _,
                        *cur as u32,
                    );
                    buf[buf.len() - 1] = 0;
                    ptr = buf.as_ptr() as _;
                    while *ptr != 0 {
                        *out = *ptr as _;
                        out = out.add(1);
                        ptr = ptr.add(1);
                    }
                }
                cur = cur.add(1);
            }
            *out = 0;
            return buffer;
        }

        // mem_error:
        xml_entities_err_memory("xmlEncodeEntities: realloc failed");
        xml_free(buffer as _);
        null_mut()
    }
}

/// Do a global encoding of a string, replacing the predefined entities
/// and non ASCII values with their entities and CharRef counterparts.
/// Contrary to xmlEncodeEntities, this routine is reentrant, and result
/// must be deallocated.
///
/// Returns A newly allocated string with the substitution done.
#[doc(alias = "xmlEncodeEntitiesReentrant")]
pub unsafe fn xml_encode_entities_reentrant(
    doc: Option<XmlDocPtr>,
    input: *const XmlChar,
) -> *mut XmlChar {
    unsafe { xml_encode_entities_internal(doc, input, 0) }
}

/// Do a global encoding of a string, replacing the predefined entities
/// this routine is reentrant, and result must be deallocated.
///
/// Returns A newly allocated string with the substitution done.
#[doc(alias = "xmlEncodeSpecialChars")]
pub unsafe fn xml_encode_special_chars(
    _doc: Option<XmlDocPtr>,
    input: *const XmlChar,
) -> *mut XmlChar {
    unsafe {
        let mut cur: *const XmlChar = input;
        let mut buffer: *mut XmlChar;
        let mut out: *mut XmlChar;
        let mut buffer_size: size_t;
        if input.is_null() {
            return null_mut();
        }

        // allocate an translation buffer.
        buffer_size = 1000;
        buffer = xml_malloc(buffer_size) as *mut XmlChar;
        if buffer.is_null() {
            xml_entities_err_memory("xmlEncodeSpecialChars: malloc failed");
            return null_mut();
        }
        out = buffer;

        'mem_error: {
            while *cur != b'\0' {
                let indx: size_t = out.offset_from(buffer) as _;
                if indx + 10 > buffer_size {
                    grow_buffer_reentrant!(buffer, buffer_size, 'mem_error);
                    out = buffer.add(indx as usize);
                }

                // By default one have to encode at least '<', '>', '"' and '&' !
                if *cur == b'<' {
                    *out = b'&';
                    out = out.add(1);
                    *out = b'l';
                    out = out.add(1);
                    *out = b't';
                    out = out.add(1);
                    *out = b';';
                    out = out.add(1);
                } else if *cur == b'>' {
                    *out = b'&';
                    out = out.add(1);
                    *out = b'g';
                    out = out.add(1);
                    *out = b't';
                    out = out.add(1);
                    *out = b';';
                    out = out.add(1);
                } else if *cur == b'&' {
                    *out = b'&';
                    out = out.add(1);
                    *out = b'a';
                    out = out.add(1);
                    *out = b'm';
                    out = out.add(1);
                    *out = b'p';
                    out = out.add(1);
                    *out = b';';
                    out = out.add(1);
                } else if *cur == b'"' {
                    *out = b'&';
                    out = out.add(1);
                    *out = b'q';
                    out = out.add(1);
                    *out = b'u';
                    out = out.add(1);
                    *out = b'o';
                    out = out.add(1);
                    *out = b't';
                    out = out.add(1);
                    *out = b';';
                    out = out.add(1);
                } else if *cur == b'\r' {
                    *out = b'&';
                    out = out.add(1);
                    *out = b'#';
                    out = out.add(1);
                    *out = b'1';
                    out = out.add(1);
                    *out = b'3';
                    out = out.add(1);
                    *out = b';';
                    out = out.add(1);
                } else {
                    // Works because on UTF-8, all extended sequences cannot
                    // result in bytes in the ASCII range.
                    *out = *cur;
                    out = out.add(1);
                }
                cur = cur.add(1);
            }
            *out = 0;
            return buffer;
        }

        // mem_error:
        xml_entities_err_memory("xmlEncodeSpecialChars: realloc failed");
        xml_free(buffer as _);
        null_mut()
    }
}

/// create and initialize an empty entities hash table.
/// This really doesn't make sense and should be deprecated
///
/// Returns the xmlEntitiesTablePtr just created or NULL in case of error.
#[doc(alias = "xmlCreateEntitiesTable")]
pub unsafe fn xml_create_entities_table() -> XmlEntitiesTablePtr {
    xml_hash_create(0) as XmlEntitiesTablePtr
}

/// Build a copy of an entity
///
/// Returns the new xmlEntitiesPtr or NULL in case of error.
#[doc(alias = "xmlCopyEntity")]
#[cfg(feature = "libxml_tree")]
unsafe fn xml_copy_entity(ent: XmlEntityPtr) -> Option<XmlEntityPtr> {
    unsafe {
        let mut cur = XmlEntityPtr::new(XmlEntity {
            typ: XmlElementType::XmlEntityDecl,
            etype: ent.etype,
            ..Default::default()
        })
        .unwrap();
        if !ent.name.is_null() {
            cur.name = xml_strdup(ent.name);
        }
        cur.external_id = ent.external_id.clone();
        if !ent.system_id.is_null() {
            cur.system_id = xml_strdup(ent.system_id);
        }
        if !ent.content.is_null() {
            cur.content = xml_strdup(ent.content);
        }
        if !ent.orig.is_null() {
            cur.orig = xml_strdup(ent.orig);
        }
        cur.uri = ent.uri.clone();
        Some(cur)
    }
}

/// Build a copy of an entity table.
///
/// Returns the new xmlEntitiesTablePtr or NULL in case of error.
#[doc(alias = "xmlCopyEntitiesTable")]
#[cfg(feature = "libxml_tree")]
pub unsafe fn xml_copy_entities_table(
    table: XmlHashTableRef<'static, XmlEntityPtr>,
) -> Option<XmlHashTableRef<'static, XmlEntityPtr>> {
    unsafe {
        let new = table.clone_with(|&ent, _| xml_copy_entity(ent).unwrap());
        XmlHashTableRef::from_table(new)
    }
}

/// Deallocate the memory used by an entities hash table.
#[doc(alias = "xmlFreeEntitiesTable")]
pub unsafe fn xml_free_entities_table(table: XmlHashTableRef<'static, XmlEntityPtr>) {
    unsafe {
        let mut table = table.into_inner();
        table.clear_with(|payload, _| {
            xml_free_entity(payload);
        });
    }
}

/// This will dump the content of the entity table as an XML DTD definition
#[doc(alias = "xmlDumpEntitiesTable")]
#[cfg(feature = "libxml_output")]
pub unsafe fn xml_dump_entities_table<'a>(buf: &mut (impl Write + 'a), table: XmlEntitiesTablePtr) {
    unsafe {
        let Some(table) = XmlHashTableRef::from_raw(table) else {
            return;
        };
        table.scan(|data, _, _, _| {
            xml_dump_entity_decl(
                buf,
                XmlEntityPtr::from_raw(data.0 as *mut XmlEntity)
                    .unwrap()
                    .unwrap(),
            )
        });
    }
}

/// This will dump the quoted string value, taking care of the special
/// treatment required by %
#[doc(alias = "xmlDumpEntityContent")]
#[cfg(feature = "libxml_output")]
unsafe fn xml_dump_entity_content<'a>(buf: &mut (impl Write + 'a), content: *const XmlChar) {
    unsafe {
        use std::{slice::from_raw_parts, str::from_utf8};

        use crate::io::write_quoted;

        if !xml_strchr(content, b'%').is_null() {
            let mut base: *const XmlChar;
            let mut cur: *const XmlChar;

            write!(buf, "\"").ok();
            base = content;
            cur = content;
            while *cur != 0 {
                if *cur == b'"' {
                    if base != cur {
                        write!(
                            buf,
                            "{}",
                            from_utf8(from_raw_parts(base, cur.offset_from(base) as _)).unwrap()
                        )
                        .ok();
                    }
                    write!(buf, "&quot;").ok();
                    cur = cur.add(1);
                    base = cur;
                } else if *cur == b'%' {
                    if base != cur {
                        write!(
                            buf,
                            "{}",
                            from_utf8(from_raw_parts(base, cur.offset_from(base) as _)).unwrap()
                        )
                        .ok();
                    }
                    write!(buf, "&#x25;").ok();
                    cur = cur.add(1);
                    base = cur;
                } else {
                    cur = cur.add(1);
                }
            }
            if base != cur {
                write!(
                    buf,
                    "{}",
                    from_utf8(from_raw_parts(base, cur.offset_from(base) as _)).unwrap()
                )
                .ok();
            }
            write!(buf, "\"").ok();
        } else {
            write_quoted(
                buf,
                CStr::from_ptr(content as *const i8)
                    .to_string_lossy()
                    .as_ref(),
            )
            .ok();
        }
    }
}

/// This will dump the content of the entity table as an XML DTD definition
#[doc(alias = "xmlDumpEntityDecl")]
#[cfg(feature = "libxml_output")]
pub unsafe fn xml_dump_entity_decl<'a>(buf: &mut (impl Write + 'a), ent: XmlEntityPtr) {
    unsafe {
        use crate::io::write_quoted;

        let name = CStr::from_ptr(ent.name as _).to_string_lossy();
        match ent.etype {
            XmlEntityType::XmlInternalGeneralEntity => {
                write!(buf, "<!ENTITY {name} ").ok();
                if !ent.orig.is_null() {
                    write_quoted(
                        buf,
                        CStr::from_ptr(ent.orig as _).to_string_lossy().as_ref(),
                    )
                    .ok();
                } else {
                    xml_dump_entity_content(buf, ent.content as _);
                }
                writeln!(buf, ">").ok();
            }
            XmlEntityType::XmlExternalGeneralParsedEntity => {
                write!(buf, "<!ENTITY {name}").ok();
                if let Some(external_id) = ent.external_id.as_deref() {
                    write!(buf, " PUBLIC ").ok();
                    write_quoted(buf, external_id).ok();
                    write!(buf, " ").ok();
                    write_quoted(
                        buf,
                        CStr::from_ptr(ent.system_id as _)
                            .to_string_lossy()
                            .as_ref(),
                    )
                    .ok();
                } else {
                    write!(buf, " SYSTEM ").ok();
                    write_quoted(
                        buf,
                        CStr::from_ptr(ent.system_id as _)
                            .to_string_lossy()
                            .as_ref(),
                    )
                    .ok();
                }
                writeln!(buf, ">").ok();
            }
            XmlEntityType::XmlExternalGeneralUnparsedEntity => {
                write!(buf, "<!ENTITY {name}").ok();
                if let Some(external_id) = ent.external_id.as_deref() {
                    write!(buf, " PUBLIC ").ok();
                    write_quoted(buf, external_id).ok();
                    write!(buf, " ").ok();
                    write_quoted(
                        buf,
                        CStr::from_ptr(ent.system_id as _)
                            .to_string_lossy()
                            .as_ref(),
                    )
                    .ok();
                } else {
                    write!(buf, " SYSTEM ").ok();
                    write_quoted(
                        buf,
                        CStr::from_ptr(ent.system_id as _)
                            .to_string_lossy()
                            .as_ref(),
                    )
                    .ok();
                }
                if !ent.content.is_null() {
                    /* Should be true ! */
                    write!(buf, " NDATA ").ok();
                    if !ent.orig.is_null() {
                        write!(
                            buf,
                            "{}",
                            CStr::from_ptr(ent.orig as _).to_string_lossy().as_ref()
                        )
                        .ok();
                    } else {
                        write!(
                            buf,
                            "{}",
                            CStr::from_ptr(ent.content as _).to_string_lossy().as_ref()
                        )
                        .ok();
                    }
                }
                writeln!(buf, ">").ok();
            }
            XmlEntityType::XmlInternalParameterEntity => {
                write!(buf, "<!ENTITY % {name} ").ok();
                if ent.orig.is_null() {
                    xml_dump_entity_content(buf, ent.content as _);
                } else {
                    write_quoted(
                        buf,
                        CStr::from_ptr(ent.orig as _).to_string_lossy().as_ref(),
                    )
                    .ok();
                }
                writeln!(buf, ">").ok();
            }
            XmlEntityType::XmlExternalParameterEntity => {
                write!(buf, "<!ENTITY % {name}").ok();
                if let Some(external_id) = ent.external_id.as_deref() {
                    write!(buf, " PUBLIC ").ok();
                    write_quoted(buf, external_id).ok();
                    write!(buf, " ").ok();
                    write_quoted(
                        buf,
                        CStr::from_ptr(ent.system_id as _)
                            .to_string_lossy()
                            .as_ref(),
                    )
                    .ok();
                } else {
                    write!(buf, " SYSTEM ").ok();
                    write_quoted(
                        buf,
                        CStr::from_ptr(ent.system_id as _)
                            .to_string_lossy()
                            .as_ref(),
                    )
                    .ok();
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
pub(crate) unsafe fn xml_encode_attribute_entities(
    doc: Option<XmlDocPtr>,
    input: *const XmlChar,
) -> *mut XmlChar {
    unsafe { xml_encode_entities_internal(doc, input, 1) }
}
