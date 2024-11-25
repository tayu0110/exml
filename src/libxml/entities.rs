//! Provide methods and data structures for handling XML entities.  
//! This module is based on `libxml/entities.h`, `entities.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// entities.c : implementation for the XML entities handling
//
// See Copyright for the status of this software.
//
// daniel@veillard.com

use std::{
    any::type_name,
    borrow::Cow,
    ffi::{c_char, CStr, CString},
    mem::size_of,
    os::raw::c_void,
    ptr::{addr_of_mut, null, null_mut},
    sync::atomic::{AtomicPtr, Ordering},
};

use libc::{memset, size_t, snprintf, strchr};

#[cfg(feature = "libxml_legacy")]
use crate::generic_error;
use crate::{
    __xml_raise_error,
    buf::libxml_api::{
        xml_buf_add, xml_buf_cat, xml_buf_ccat, xml_buf_write_quoted_string, XmlBufPtr,
    },
    error::{XmlErrorDomain, XmlParserErrors, __xml_simple_error},
    libxml::{
        dict::{xml_dict_lookup, xml_dict_owns, XmlDictPtr},
        globals::{xml_free, xml_malloc},
        hash::{
            xml_hash_add_entry, xml_hash_copy, xml_hash_create, xml_hash_create_dict,
            xml_hash_free, xml_hash_lookup, xml_hash_scan, XmlHashTable,
        },
        xmlstring::{
            xml_str_equal, xml_strcasecmp, xml_strchr, xml_strdup, xml_strlen, xml_strndup,
            xml_strstr, XmlChar,
        },
    },
    tree::{
        xml_free_node_list, NodeCommon, NodePtr, XmlDoc, XmlDocPtr, XmlDtd, XmlDtdPtr,
        XmlElementType, XmlNode,
    },
};

use super::{chvalid::xml_is_char, hash::CVoidWrapper};

/// The different valid entity types.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlEntityType {
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

/// An unit of storage for an entity, contains the string,
/// the value and the linkind data needed for the linking in the hash table.
pub type XmlEntityPtr = *mut XmlEntity;
#[repr(C)]
pub struct XmlEntity {
    pub(crate) _private: AtomicPtr<c_void>,  /* application data */
    pub(crate) typ: XmlElementType,          /* XML_ENTITY_DECL, must be second ! */
    pub(crate) name: AtomicPtr<XmlChar>,     /* Entity name */
    pub(crate) children: AtomicPtr<XmlNode>, /* First child link */
    pub(crate) last: AtomicPtr<XmlNode>,     /* Last child link */
    pub(crate) parent: AtomicPtr<XmlDtd>,    /* -> DTD */
    pub(crate) next: AtomicPtr<XmlNode>,     /* next sibling link  */
    pub(crate) prev: AtomicPtr<XmlNode>,     /* previous sibling link  */
    pub(crate) doc: AtomicPtr<XmlDoc>,       /* the containing document */

    pub(crate) orig: AtomicPtr<XmlChar>, /* content without ref substitution */
    pub(crate) content: AtomicPtr<XmlChar>, /* content or ndata if unparsed */
    pub(crate) length: i32,              /* the content length */
    pub(crate) etype: Option<XmlEntityType>, /* The entity type */
    pub(crate) external_id: AtomicPtr<XmlChar>, /* External identifier for PUBLIC */
    pub(crate) system_id: AtomicPtr<XmlChar>, /* URI for a SYSTEM or PUBLIC Entity */

    pub(crate) nexte: AtomicPtr<XmlEntity>, /* unused */
    pub(crate) uri: AtomicPtr<XmlChar>,     /* the full URI as computed */
    pub(crate) owner: i32,                  /* does the entity own the childrens */
    pub(crate) flags: i32,                  /* various flags */
    pub expanded_size: u64,                 /* expanded size */
}

impl NodeCommon for XmlEntity {
    fn document(&self) -> *mut XmlDoc {
        self.doc.load(Ordering::Relaxed)
    }
    fn element_type(&self) -> XmlElementType {
        self.typ
    }
    fn name(&self) -> Option<Cow<'_, str>> {
        let name = self.name.load(Ordering::Relaxed);
        (!name.is_null()).then(|| unsafe { CStr::from_ptr(name as *const i8).to_string_lossy() })
    }
    fn children(&self) -> Option<NodePtr> {
        NodePtr::from_ptr(self.children.load(Ordering::Relaxed))
    }
    fn set_children(&mut self, children: Option<NodePtr>) {
        self.children.store(
            children.map_or(null_mut(), |c| c.as_ptr()),
            Ordering::Relaxed,
        );
    }
    fn last(&self) -> Option<NodePtr> {
        NodePtr::from_ptr(self.last.load(Ordering::Relaxed))
    }
    fn set_last(&mut self, last: Option<NodePtr>) {
        self.last
            .store(last.map_or(null_mut(), |l| l.as_ptr()), Ordering::Relaxed);
    }
    fn next(&self) -> Option<NodePtr> {
        NodePtr::from_ptr(self.next.load(Ordering::Relaxed))
    }
    fn set_next(&mut self, next: Option<NodePtr>) {
        self.next
            .store(next.map_or(null_mut(), |n| n.as_ptr()), Ordering::Relaxed);
    }
    fn prev(&self) -> Option<NodePtr> {
        NodePtr::from_ptr(self.prev.load(Ordering::Relaxed))
    }
    fn set_prev(&mut self, prev: Option<NodePtr>) {
        self.prev
            .store(prev.map_or(null_mut(), |p| p.as_ptr()), Ordering::Relaxed);
    }
    fn parent(&self) -> Option<NodePtr> {
        NodePtr::from_ptr(self.parent.load(Ordering::Relaxed) as *mut XmlNode)
    }
    fn set_parent(&mut self, parent: Option<NodePtr>) {
        self.parent.store(
            parent.map_or(null_mut(), |p| p.as_ptr()) as *mut XmlDtd,
            Ordering::Relaxed,
        );
    }
}

/// All entities are stored in an hash table.
/// There is 2 separate hash tables for global and parameter entities.
pub type XmlEntitiesTable = XmlHashTable<'static, CVoidWrapper>;
pub type XmlEntitiesTablePtr = *mut XmlEntitiesTable;

/// Handle an out of memory condition
#[doc(alias = "xmlEntitiesErrMemory")]
unsafe extern "C" fn xml_entities_err_memory(extra: *const c_char) {
    __xml_simple_error(
        XmlErrorDomain::XmlFromTree,
        XmlParserErrors::XmlErrNoMemory,
        null_mut(),
        null(),
        extra as _,
    );
}

/// internal routine doing the entity node structures allocations
#[doc(alias = "xmlCreateEntity")]
unsafe extern "C" fn xml_create_entity(
    dict: XmlDictPtr,
    name: *const XmlChar,
    typ: i32,
    external_id: *const XmlChar,
    system_id: *const XmlChar,
    content: *const XmlChar,
) -> XmlEntityPtr {
    let ret: XmlEntityPtr = xml_malloc(size_of::<XmlEntity>()) as XmlEntityPtr;
    if ret.is_null() {
        xml_entities_err_memory(c"xmlCreateEntity: malloc failed".as_ptr() as _);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlEntity>());
    (*ret).typ = XmlElementType::XmlEntityDecl;

    /*
     * fill the structure.
     */
    (*ret).etype = typ.try_into().ok();
    if dict.is_null() {
        (*ret).name = AtomicPtr::new(xml_strdup(name) as _);
        if !external_id.is_null() {
            (*ret).external_id = AtomicPtr::new(xml_strdup(external_id) as _);
        }
        if !system_id.is_null() {
            (*ret).system_id = AtomicPtr::new(xml_strdup(system_id) as _);
        }
    } else {
        (*ret).name = AtomicPtr::new(xml_dict_lookup(dict, name, -1) as _);
        (*ret).external_id = AtomicPtr::new(xml_strdup(external_id) as _);
        (*ret).system_id = AtomicPtr::new(xml_strdup(system_id) as _);
    }
    if !content.is_null() {
        (*ret).length = xml_strlen(content);
        (*ret).content = AtomicPtr::new(xml_strndup(content, (*ret).length));
    } else {
        (*ret).length = 0;
        (*ret).content = AtomicPtr::new(null_mut());
    }
    (*ret).uri = AtomicPtr::new(null_mut()); /* to be computed by the layer knowing
                                             the defining entity */
    (*ret).orig = AtomicPtr::new(null_mut());
    (*ret).owner = 0;

    ret
}

/// Create a new entity, this differs from xmlAddDocEntity() that if
/// the document is NULL or has no internal subset defined, then an
/// unlinked entity structure will be returned, it is then the responsibility
/// of the caller to link it to the document later or free it when not needed
/// anymore.
///
/// Returns a pointer to the entity or NULL in case of error
#[doc(alias = "xmlNewEntity")]
pub unsafe extern "C" fn xml_new_entity(
    doc: XmlDocPtr,
    name: *const XmlChar,
    typ: i32,
    external_id: *const XmlChar,
    system_id: *const XmlChar,
    content: *const XmlChar,
) -> XmlEntityPtr {
    if !doc.is_null() && !(*doc).int_subset.is_null() {
        return xml_add_doc_entity(doc, name, typ, external_id, system_id, content);
    }
    let dict = if !doc.is_null() {
        (*doc).dict
    } else {
        null_mut()
    };
    let ret: XmlEntityPtr = xml_create_entity(dict, name, typ, external_id, system_id, content);
    if ret.is_null() {
        return null_mut();
    }
    (*ret).doc = AtomicPtr::new(doc as _);
    ret
}

/// Raise an error.
#[doc(alias = "xmlEntitiesErr")]
unsafe extern "C" fn xml_entities_err(code: XmlParserErrors, msg: *const c_char) {
    __xml_simple_error(
        XmlErrorDomain::XmlFromTree,
        code,
        null_mut(),
        msg as _,
        null(),
    );
}

/// Raise a warning.
#[doc(alias = "xmlEntitiesWarn")]
unsafe extern "C" fn xml_entities_warn(
    code: XmlParserErrors,
    msg: *const c_char,
    str1: *const XmlChar,
) {
    __xml_raise_error!(
        None,
        None,
        None,
        null_mut(),
        null_mut(),
        XmlErrorDomain::XmlFromTree,
        code,
        XmlErrorLevel::XmlErrWarning,
        null_mut(),
        0,
        (!str1.is_null()).then(|| CStr::from_ptr(str1 as *const i8)
            .to_string_lossy()
            .into_owned()
            .into()),
        None,
        None,
        0,
        0,
        msg,
        str1,
        null::<c_char>()
    );
}

/// clean-up an entity record.
#[doc(alias = "xmlFreeEntity")]
unsafe extern "C" fn xml_free_entity(entity: XmlEntityPtr) {
    let mut dict: XmlDictPtr = null_mut();

    if entity.is_null() {
        return;
    }

    if !(*entity).doc.load(Ordering::Relaxed).is_null() {
        dict = (*(*entity).doc.load(Ordering::Relaxed)).dict as _;
    }

    if !(*entity).children.load(Ordering::Relaxed).is_null()
        && (*entity).owner == 1
        && NodePtr::from_ptr(entity as *mut XmlNode)
            == (*(*entity).children.load(Ordering::Relaxed)).parent
    {
        xml_free_node_list((*entity).children.load(Ordering::Relaxed));
        (*entity).children.store(null_mut(), Ordering::Relaxed);
    }
    if !(*entity).name.load(Ordering::Relaxed).is_null()
        && (dict.is_null() || xml_dict_owns(dict, (*entity).name.load(Ordering::Relaxed)) == 0)
    {
        xml_free((*entity).name.load(Ordering::Relaxed) as _);
        (*entity).name.store(null_mut(), Ordering::Relaxed);
    }
    if !(*entity).external_id.load(Ordering::Relaxed).is_null() {
        xml_free((*entity).external_id.load(Ordering::Relaxed) as _);
        (*entity).external_id.store(null_mut(), Ordering::Relaxed);
    }
    if !(*entity).system_id.load(Ordering::Relaxed).is_null() {
        xml_free((*entity).system_id.load(Ordering::Relaxed) as _);
        (*entity).system_id.store(null_mut(), Ordering::Relaxed);
    }
    if !(*entity).uri.load(Ordering::Relaxed).is_null() {
        xml_free((*entity).uri.load(Ordering::Relaxed) as _);
        (*entity).uri.store(null_mut(), Ordering::Relaxed);
    }
    if !(*entity).content.load(Ordering::Relaxed).is_null() {
        xml_free((*entity).content.load(Ordering::Relaxed) as _);
        (*entity).content.store(null_mut(), Ordering::Relaxed);
    }
    if !(*entity).orig.load(Ordering::Relaxed).is_null() {
        xml_free((*entity).orig.load(Ordering::Relaxed) as _);
        (*entity).orig.store(null_mut(), Ordering::Relaxed);
    }
    xml_free(entity as _);
}

/// Register a new entity for an entities table.
#[doc(alias = "xmlAddEntity")]
unsafe extern "C" fn xml_add_entity(
    dtd: XmlDtdPtr,
    name: *const XmlChar,
    typ: i32,
    external_id: *const XmlChar,
    system_id: *const XmlChar,
    content: *const XmlChar,
) -> XmlEntityPtr {
    let mut dict: XmlDictPtr = null_mut();
    let mut table: XmlEntitiesTablePtr = null_mut();
    let predef: XmlEntityPtr;

    if name.is_null() {
        return null_mut();
    }
    if dtd.is_null() {
        return null_mut();
    }
    if !(*dtd).doc.is_null() {
        dict = (*(*dtd).doc).dict;
    }

    let name = CString::new(CStr::from_ptr(name as *const i8).to_string_lossy().as_ref()).unwrap();
    match XmlEntityType::try_from(typ) {
        Ok(XmlEntityType::XmlInternalGeneralEntity)
        | Ok(XmlEntityType::XmlExternalGeneralParsedEntity)
        | Ok(XmlEntityType::XmlExternalGeneralUnparsedEntity) => {
            predef = xml_get_predefined_entity(name.as_ptr() as *const u8);
            if !predef.is_null() {
                let mut valid: i32 = 0;

                /* 4.6 Predefined Entities */
                if typ == XmlEntityType::XmlInternalGeneralEntity as i32 && !content.is_null() {
                    let c: i32 = *(*predef).content.load(Ordering::Relaxed).add(0) as _;

                    if (*content.add(0) as i32 == c && *content.add(1) == 0)
                        && (c == b'>' as i32 || c == b'\'' as i32 || c == b'"' as i32)
                    {
                        valid = 1;
                    } else if *content.add(0) == b'&' && *content.add(1) == b'#' {
                        if *content.add(2) == b'x' {
                            let hex: *mut XmlChar = c"0123456789ABCDEF".as_ptr() as _;
                            let refe = [
                                *hex.add(c as usize / 16 % 16),
                                *hex.add(c as usize % 16),
                                b';',
                                0,
                            ];
                            if xml_strcasecmp(content.add(3) as _, refe.as_ptr()) == 0 {
                                valid = 1;
                            }
                        } else {
                            let refe = [b'0' + (c / 10 % 10) as u8, b'0' + (c % 10) as u8, b';', 0];
                            if xml_str_equal(content.add(2) as _, refe.as_ptr()) {
                                valid = 1;
                            }
                        }
                    }
                }
                if valid == 0 {
                    xml_entities_warn(
                        XmlParserErrors::XmlErrEntityProcessing,
                        c"xmlAddEntity: invalid redeclaration of predefined entity '%s'".as_ptr()
                            as _,
                        name.as_ptr() as *const u8,
                    );
                    return null_mut();
                }
            }
            if (*dtd).entities.is_null() {
                (*dtd).entities = xml_hash_create_dict(0, dict) as _;
            }
            table = (*dtd).entities as _;
        }
        Ok(XmlEntityType::XmlInternalParameterEntity)
        | Ok(XmlEntityType::XmlExternalParameterEntity) => {
            if (*dtd).pentities.is_null() {
                (*dtd).pentities = xml_hash_create_dict(0, dict) as _;
            }
            table = (*dtd).pentities as _;
        }
        Ok(XmlEntityType::XmlInternalPredefinedEntity) => {
            return null_mut();
        }
        _ => {}
    }
    if table.is_null() {
        return null_mut();
    }
    let ret: XmlEntityPtr = xml_create_entity(
        dict,
        name.as_ptr() as *const u8,
        typ,
        external_id,
        system_id,
        content,
    );
    if ret.is_null() {
        return null_mut();
    }
    (*ret).doc.store((*dtd).doc, Ordering::Relaxed);

    if xml_hash_add_entry(table, name.as_ptr() as *const u8, ret as _) != 0 {
        /*
         * entity was already defined at another level.
         */
        xml_free_entity(ret);
        return null_mut();
    }
    ret
}

/// Register a new entity for this document.
///
/// Returns a pointer to the entity or NULL in case of error
#[doc(alias = "xmlAddDocEntity")]
pub unsafe extern "C" fn xml_add_doc_entity(
    doc: XmlDocPtr,
    name: *const XmlChar,
    typ: i32,
    external_id: *const XmlChar,
    system_id: *const XmlChar,
    content: *const XmlChar,
) -> XmlEntityPtr {
    if doc.is_null() {
        xml_entities_err(
            XmlParserErrors::XmlDTDNoDoc,
            c"xmlAddDocEntity: document is NULL".as_ptr() as _,
        );
        return null_mut();
    }
    if (*doc).int_subset.is_null() {
        xml_entities_err(
            XmlParserErrors::XmlDTDNoDTD,
            c"xmlAddDocEntity: document without internal subset".as_ptr() as _,
        );
        return null_mut();
    }
    let dtd: XmlDtdPtr = (*doc).int_subset;
    let ret: XmlEntityPtr = xml_add_entity(dtd, name, typ, external_id, system_id, content);
    if ret.is_null() {
        return null_mut();
    }

    /*
     * Link it to the DTD
     */
    (*ret).parent.store(dtd as _, Ordering::Relaxed);
    (*ret).doc.store((*dtd).doc, Ordering::Relaxed);
    if let Some(mut last) = (*dtd).last {
        last.next = NodePtr::from_ptr(ret as *mut XmlNode);
        (*ret).prev.store(last.as_ptr(), Ordering::Relaxed);
        (*dtd).last = NodePtr::from_ptr(ret as *mut XmlNode);
    } else {
        (*dtd).children = NodePtr::from_ptr(ret as *mut XmlNode);
        (*dtd).last = NodePtr::from_ptr(ret as *mut XmlNode);
    }
    ret
}

/// Register a new entity for this document DTD external subset.
///
/// Returns a pointer to the entity or NULL in case of error
#[doc(alias = "xmlAddDtdEntity")]
pub unsafe extern "C" fn xml_add_dtd_entity(
    doc: XmlDocPtr,
    name: *const XmlChar,
    typ: i32,
    external_id: *const XmlChar,
    system_id: *const XmlChar,
    content: *const XmlChar,
) -> XmlEntityPtr {
    if doc.is_null() {
        xml_entities_err(
            XmlParserErrors::XmlDTDNoDoc,
            c"xmlAddDtdEntity: document is NULL".as_ptr() as _,
        );
        return null_mut();
    }
    if (*doc).ext_subset.is_null() {
        xml_entities_err(
            XmlParserErrors::XmlDTDNoDTD,
            c"xmlAddDtdEntity: document without external subset".as_ptr() as _,
        );
        return null_mut();
    }
    let dtd: XmlDtdPtr = (*doc).ext_subset;
    let ret: XmlEntityPtr = xml_add_entity(dtd, name, typ, external_id, system_id, content);
    if ret.is_null() {
        return null_mut();
    }

    /*
     * Link it to the DTD
     */
    (*ret).parent = dtd.into();
    (*ret).doc = (*dtd).doc.into();
    if let Some(mut last) = (*dtd).last {
        last.next = NodePtr::from_ptr(ret as *mut XmlNode);
        (*ret).prev.store(last.as_ptr(), Ordering::Relaxed);
        (*dtd).last = NodePtr::from_ptr(ret as *mut XmlNode);
    } else {
        (*dtd).children = NodePtr::from_ptr(ret as *mut XmlNode);
        (*dtd).last = NodePtr::from_ptr(ret as *mut XmlNode);
    }
    ret
}

// The XML predefined entities.

static mut XML_ENTITY_LT: XmlEntity = XmlEntity {
    _private: AtomicPtr::new(null_mut()),
    typ: XmlElementType::XmlEntityDecl,
    name: AtomicPtr::new(c"lt".as_ptr() as _),
    children: AtomicPtr::new(null_mut()),
    last: AtomicPtr::new(null_mut()),
    parent: AtomicPtr::new(null_mut()),
    next: AtomicPtr::new(null_mut()),
    prev: AtomicPtr::new(null_mut()),
    doc: AtomicPtr::new(null_mut()),
    orig: AtomicPtr::new(c"<".as_ptr() as _),
    content: AtomicPtr::new(c"<".as_ptr() as _),
    length: 1,
    etype: Some(XmlEntityType::XmlInternalPredefinedEntity),
    external_id: AtomicPtr::new(null_mut()),
    system_id: AtomicPtr::new(null_mut()),
    nexte: AtomicPtr::new(null_mut()),
    uri: AtomicPtr::new(null_mut()),
    owner: 0,
    flags: 0,
    expanded_size: 0,
};
static mut XML_ENTITY_GT: XmlEntity = XmlEntity {
    _private: AtomicPtr::new(null_mut()),
    typ: XmlElementType::XmlEntityDecl,
    name: AtomicPtr::new(c"gt".as_ptr() as _),
    children: AtomicPtr::new(null_mut()),
    last: AtomicPtr::new(null_mut()),
    parent: AtomicPtr::new(null_mut()),
    next: AtomicPtr::new(null_mut()),
    prev: AtomicPtr::new(null_mut()),
    doc: AtomicPtr::new(null_mut()),
    orig: AtomicPtr::new(c">".as_ptr() as _),
    content: AtomicPtr::new(c">".as_ptr() as _),
    length: 1,
    etype: Some(XmlEntityType::XmlInternalPredefinedEntity),
    external_id: AtomicPtr::new(null_mut()),
    system_id: AtomicPtr::new(null_mut()),
    nexte: AtomicPtr::new(null_mut()),
    uri: AtomicPtr::new(null_mut()),
    owner: 0,
    flags: 0,
    expanded_size: 0,
};
static mut XML_ENTITY_AMP: XmlEntity = XmlEntity {
    _private: AtomicPtr::new(null_mut()),
    typ: XmlElementType::XmlEntityDecl,
    name: AtomicPtr::new(c"amp".as_ptr() as _),
    children: AtomicPtr::new(null_mut()),
    last: AtomicPtr::new(null_mut()),
    parent: AtomicPtr::new(null_mut()),
    next: AtomicPtr::new(null_mut()),
    prev: AtomicPtr::new(null_mut()),
    doc: AtomicPtr::new(null_mut()),
    orig: AtomicPtr::new(c"&".as_ptr() as _),
    content: AtomicPtr::new(c"&".as_ptr() as _),
    length: 1,
    etype: Some(XmlEntityType::XmlInternalPredefinedEntity),
    external_id: AtomicPtr::new(null_mut()),
    system_id: AtomicPtr::new(null_mut()),
    nexte: AtomicPtr::new(null_mut()),
    uri: AtomicPtr::new(null_mut()),
    owner: 0,
    flags: 0,
    expanded_size: 0,
};
static mut XML_ENTITY_QUOT: XmlEntity = XmlEntity {
    _private: AtomicPtr::new(null_mut()),
    typ: XmlElementType::XmlEntityDecl,
    name: AtomicPtr::new(c"quot".as_ptr() as _),
    children: AtomicPtr::new(null_mut()),
    last: AtomicPtr::new(null_mut()),
    parent: AtomicPtr::new(null_mut()),
    next: AtomicPtr::new(null_mut()),
    prev: AtomicPtr::new(null_mut()),
    doc: AtomicPtr::new(null_mut()),
    orig: AtomicPtr::new(c"\"".as_ptr() as _),
    content: AtomicPtr::new(c"\"".as_ptr() as _),
    length: 1,
    etype: Some(XmlEntityType::XmlInternalPredefinedEntity),
    external_id: AtomicPtr::new(null_mut()),
    system_id: AtomicPtr::new(null_mut()),
    nexte: AtomicPtr::new(null_mut()),
    uri: AtomicPtr::new(null_mut()),
    owner: 0,
    flags: 0,
    expanded_size: 0,
};
static mut XML_ENTITY_APOS: XmlEntity = XmlEntity {
    _private: AtomicPtr::new(null_mut()),
    typ: XmlElementType::XmlEntityDecl,
    name: AtomicPtr::new(c"apos".as_ptr() as _),
    children: AtomicPtr::new(null_mut()),
    last: AtomicPtr::new(null_mut()),
    parent: AtomicPtr::new(null_mut()),
    next: AtomicPtr::new(null_mut()),
    prev: AtomicPtr::new(null_mut()),
    doc: AtomicPtr::new(null_mut()),
    orig: AtomicPtr::new(c"'".as_ptr() as _),
    content: AtomicPtr::new(c"'".as_ptr() as _),
    length: 1,
    etype: Some(XmlEntityType::XmlInternalPredefinedEntity),
    external_id: AtomicPtr::new(null_mut()),
    system_id: AtomicPtr::new(null_mut()),
    nexte: AtomicPtr::new(null_mut()),
    uri: AtomicPtr::new(null_mut()),
    owner: 0,
    flags: 0,
    expanded_size: 0,
};

/// Check whether this name is an predefined entity.
///
/// Returns NULL if not, otherwise the entity
#[doc(alias = "xmlGetPredefinedEntity")]
pub unsafe extern "C" fn xml_get_predefined_entity(name: *const XmlChar) -> XmlEntityPtr {
    if name.is_null() {
        return null_mut();
    }
    match *name.add(0) {
        b'l' => {
            if xml_str_equal(name, c"lt".as_ptr() as _) {
                return addr_of_mut!(XML_ENTITY_LT);
            }
        }
        b'g' => {
            if xml_str_equal(name, c"gt".as_ptr() as _) {
                return addr_of_mut!(XML_ENTITY_GT);
            }
        }
        b'a' => {
            if xml_str_equal(name, c"amp".as_ptr() as _) {
                return addr_of_mut!(XML_ENTITY_AMP);
            }
            if xml_str_equal(name, c"apos".as_ptr() as _) {
                return addr_of_mut!(XML_ENTITY_APOS);
            }
        }
        b'q' => {
            if xml_str_equal(name, c"quot".as_ptr() as _) {
                return addr_of_mut!(XML_ENTITY_QUOT);
            }
        }
        _ => {}
    }
    null_mut()
}

/// Do an entity lookup in the table.
/// returns the corresponding parameter entity, if found.
///
/// Returns A pointer to the entity structure or NULL if not found.
#[doc(alias = "xmlGetEntityFromTable")]
unsafe extern "C" fn xml_get_entity_from_table(
    table: XmlEntitiesTablePtr,
    name: *const XmlChar,
) -> XmlEntityPtr {
    xml_hash_lookup(table, name) as XmlEntityPtr
}

/// Do an entity lookup in the document entity hash table and
/// returns the corresponding entity, otherwise a lookup is done
/// in the predefined entities too.
///
/// Returns A pointer to the entity structure or NULL if not found.
#[doc(alias = "xmlGetDocEntity")]
pub unsafe extern "C" fn xml_get_doc_entity(
    doc: *const XmlDoc,
    name: *const XmlChar,
) -> XmlEntityPtr {
    let mut cur: XmlEntityPtr;
    let mut table: XmlEntitiesTablePtr;

    if !doc.is_null() {
        if !(*doc).int_subset.is_null() && !(*(*doc).int_subset).entities.is_null() {
            table = (*(*doc).int_subset).entities as XmlEntitiesTablePtr;
            cur = xml_get_entity_from_table(table, name);
            if !cur.is_null() {
                return cur;
            }
        }
        if (*doc).standalone != 1
            && (!(*doc).ext_subset.is_null() && !(*(*doc).ext_subset).entities.is_null())
        {
            table = (*(*doc).ext_subset).entities as XmlEntitiesTablePtr;
            cur = xml_get_entity_from_table(table, name);
            if !cur.is_null() {
                return cur;
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
pub unsafe extern "C" fn xml_get_dtd_entity(doc: XmlDocPtr, name: *const XmlChar) -> XmlEntityPtr {
    let table: XmlEntitiesTablePtr;

    if doc.is_null() {
        return null_mut();
    }
    if !(*doc).ext_subset.is_null() && !(*(*doc).ext_subset).entities.is_null() {
        table = (*(*doc).ext_subset).entities as XmlEntitiesTablePtr;
        return xml_get_entity_from_table(table, name);
    }
    null_mut()
}

/// Do an entity lookup in the internal and external subsets and
/// returns the corresponding parameter entity, if found.
///
/// Returns A pointer to the entity structure or NULL if not found.
#[doc(alias = "xmlGetParameterEntity")]
pub unsafe extern "C" fn xml_get_parameter_entity(
    doc: XmlDocPtr,
    name: *const XmlChar,
) -> XmlEntityPtr {
    let mut table: XmlEntitiesTablePtr;
    let ret: XmlEntityPtr;

    if doc.is_null() {
        return null_mut();
    }
    if !(*doc).int_subset.is_null() && !(*(*doc).int_subset).pentities.is_null() {
        table = (*(*doc).int_subset).pentities as XmlEntitiesTablePtr;
        ret = xml_get_entity_from_table(table, name);
        if !ret.is_null() {
            return ret;
        }
    }
    if !(*doc).ext_subset.is_null() && !(*(*doc).ext_subset).pentities.is_null() {
        table = (*(*doc).ext_subset).pentities as XmlEntitiesTablePtr;
        return xml_get_entity_from_table(table, name);
    }
    null_mut()
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
pub(crate) unsafe extern "C" fn xml_encode_entities_internal(
    doc: XmlDocPtr,
    input: *const XmlChar,
    attr: i32,
) -> *mut XmlChar {
    let mut cur: *const XmlChar = input;
    let mut buffer: *mut XmlChar;
    let mut out: *mut XmlChar;
    let mut buffer_size: size_t;
    let mut html: i32 = 0;

    if input.is_null() {
        return null_mut();
    }
    if !doc.is_null() {
        html = matches!((*doc).typ, XmlElementType::XmlHTMLDocumentNode) as i32;
    }

    /*
     * allocate an translation buffer.
     */
    buffer_size = 1000;
    buffer = xml_malloc(buffer_size) as *mut XmlChar;
    if buffer.is_null() {
        xml_entities_err_memory(c"xmlEncodeEntities: malloc failed".as_ptr() as _);
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

            /*
             * By default one have to encode at least '<', '>', '"' and '&' !
             */
            if *cur == b'<' {
                let end: *const XmlChar;

                /*
                 * Special handling of server side include in HTML attributes
                 */
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
                /*
                 * Special handling of &{...} construct from HTML 4, see
                 * http://www.w3.org/TR/html401/appendix/notes.html#h-B.7.1
                 */
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
                /*
                 * default case, just copy !
                 */
                *out = *cur;
                out = out.add(1);
            } else if *cur >= 0x80 {
                if (!doc.is_null() && (*doc).encoding.is_some()) || html != 0 {
                    /*
                    * Bjørn Reese <br@sseusa.com> provided the patch
                       XmlChar xc;
                       xc = (*cur & 0x3F) << 6;
                       if (*cur.add(1) != 0) {
                       xc += *(++cur) & 0x3F;
                       *out++ = xc;
                       } else
                    */
                    *out = *cur;
                    out = out.add(1);
                } else {
                    /*
                     * We assume we have UTF-8 input.
                     * It must match either:
                     *   110xxxxx 10xxxxxx
                     *   1110xxxx 10xxxxxx 10xxxxxx
                     *   11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
                     * That is:
                     *   *cur.add(0) is 11xxxxxx
                     *   *cur.add(1) is 10xxxxxx
                     *   *cur.add(2) is 10xxxxxx if *cur.add(0) is 111xxxxx
                     *   *cur.add(3) is 10xxxxxx if *cur.add(0) is 1111xxxx
                     *   *cur.add(0) is not 11111xxx
                     */
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
                            c"xmlEncodeEntities: input not UTF-8".as_ptr() as _,
                        );
                        if !doc.is_null() {
                            (*doc).encoding = Some("ISO-8859-1".to_owned());
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
                            c"xmlEncodeEntities: char out of range\n".as_ptr() as _,
                        );
                        if !doc.is_null() {
                            (*doc).encoding = Some("ISO-8859-1".to_owned());
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
                    /*
                     * We could do multiple things here. Just save as a c_char ref
                     */
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
    xml_entities_err_memory(c"xmlEncodeEntities: realloc failed".as_ptr());
    xml_free(buffer as _);
    null_mut()
}

/// Do a global encoding of a string, replacing the predefined entities
/// and non ASCII values with their entities and CharRef counterparts.
/// Contrary to xmlEncodeEntities, this routine is reentrant, and result
/// must be deallocated.
///
/// Returns A newly allocated string with the substitution done.
#[doc(alias = "xmlEncodeEntitiesReentrant")]
pub unsafe extern "C" fn xml_encode_entities_reentrant(
    doc: XmlDocPtr,
    input: *const XmlChar,
) -> *mut XmlChar {
    xml_encode_entities_internal(doc, input, 0)
}

/// Do a global encoding of a string, replacing the predefined entities
/// this routine is reentrant, and result must be deallocated.
///
/// Returns A newly allocated string with the substitution done.
#[doc(alias = "xmlEncodeSpecialChars")]
pub unsafe extern "C" fn xml_encode_special_chars(
    _doc: *const XmlDoc,
    input: *const XmlChar,
) -> *mut XmlChar {
    let mut cur: *const XmlChar = input;
    let mut buffer: *mut XmlChar;
    let mut out: *mut XmlChar;
    let mut buffer_size: size_t;
    if input.is_null() {
        return null_mut();
    }

    /*
     * allocate an translation buffer.
     */
    buffer_size = 1000;
    buffer = xml_malloc(buffer_size) as *mut XmlChar;
    if buffer.is_null() {
        xml_entities_err_memory(c"xmlEncodeSpecialChars: malloc failed".as_ptr() as _);
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

            /*
             * By default one have to encode at least '<', '>', '"' and '&' !
             */
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
                /*
                 * Works because on UTF-8, all extended sequences cannot
                 * result in bytes in the ASCII range.
                 */
                *out = *cur;
                out = out.add(1);
            }
            cur = cur.add(1);
        }
        *out = 0;
        return buffer;
    }

    // mem_error:
    xml_entities_err_memory(c"xmlEncodeSpecialChars: realloc failed".as_ptr());
    xml_free(buffer as _);
    null_mut()
}

/// create and initialize an empty entities hash table.
/// This really doesn't make sense and should be deprecated
///
/// Returns the xmlEntitiesTablePtr just created or NULL in case of error.
#[doc(alias = "xmlCreateEntitiesTable")]
pub unsafe extern "C" fn xml_create_entities_table() -> XmlEntitiesTablePtr {
    xml_hash_create(0) as XmlEntitiesTablePtr
}

/// Build a copy of an entity
///
/// Returns the new xmlEntitiesPtr or NULL in case of error.
#[doc(alias = "xmlCopyEntity")]
#[cfg(feature = "libxml_tree")]
extern "C" fn xml_copy_entity(payload: *mut c_void, _name: *const XmlChar) -> *mut c_void {
    let ent: XmlEntityPtr = payload as XmlEntityPtr;

    unsafe {
        let cur: XmlEntityPtr = xml_malloc(size_of::<XmlEntity>()) as XmlEntityPtr;
        if cur.is_null() {
            xml_entities_err_memory(c"xmlCopyEntity:: malloc failed".as_ptr() as _);
            return null_mut();
        }
        memset(cur as _, 0, size_of::<XmlEntity>());
        (*cur).typ = XmlElementType::XmlEntityDecl;

        (*cur).etype = (*ent).etype;
        if !(*ent).name.load(Ordering::Relaxed).is_null() {
            (*cur).name = AtomicPtr::new(xml_strdup((*ent).name.load(Ordering::Relaxed)) as _);
        }
        if !(*ent).external_id.load(Ordering::Relaxed).is_null() {
            (*cur).external_id =
                AtomicPtr::new(xml_strdup((*ent).external_id.load(Ordering::Relaxed)) as _);
        }
        if !(*ent).system_id.load(Ordering::Relaxed).is_null() {
            (*cur).system_id =
                AtomicPtr::new(xml_strdup((*ent).system_id.load(Ordering::Relaxed)) as _);
        }
        if !(*ent).content.load(Ordering::Relaxed).is_null() {
            (*cur).content =
                AtomicPtr::new(xml_strdup((*ent).content.load(Ordering::Relaxed)) as _);
        }
        if !(*ent).orig.load(Ordering::Relaxed).is_null() {
            (*cur).orig = AtomicPtr::new(xml_strdup((*ent).orig.load(Ordering::Relaxed)) as _);
        }
        if !(*ent).uri.load(Ordering::Relaxed).is_null() {
            (*cur).uri = AtomicPtr::new(xml_strdup((*ent).uri.load(Ordering::Relaxed)) as _);
        }
        cur as _
    }
}

/// Build a copy of an entity table.
///
/// Returns the new xmlEntitiesTablePtr or NULL in case of error.
#[doc(alias = "xmlCopyEntitiesTable")]
#[cfg(feature = "libxml_tree")]
pub unsafe extern "C" fn xml_copy_entities_table(
    table: XmlEntitiesTablePtr,
) -> XmlEntitiesTablePtr {
    xml_hash_copy(table, Some(xml_copy_entity))
}

/// Deallocate the memory used by an entities in the hash table.
#[doc(alias = "xmlFreeEntityWrapper")]
extern "C" fn xml_free_entity_wrapper(entity: *mut c_void, _name: *const XmlChar) {
    unsafe {
        if !entity.is_null() {
            xml_free_entity(entity as XmlEntityPtr);
        }
    }
}

/// Deallocate the memory used by an entities hash table.
#[doc(alias = "xmlFreeEntitiesTable")]
pub unsafe extern "C" fn xml_free_entities_table(table: XmlEntitiesTablePtr) {
    xml_hash_free(table, Some(xml_free_entity_wrapper));
}

/// When using the hash table scan function, arguments need to be reversed
#[doc(alias = "xmlDumpEntityDeclScan")]
#[cfg(feature = "libxml_output")]
extern "C" fn xml_dump_entity_decl_scan(ent: *mut c_void, buf: *mut c_void, _name: *const XmlChar) {
    unsafe {
        xml_dump_entity_decl(buf as XmlBufPtr, ent as XmlEntityPtr);
    }
}

/// This will dump the content of the entity table as an XML DTD definition
#[doc(alias = "xmlDumpEntitiesTable")]
#[cfg(feature = "libxml_output")]
pub unsafe extern "C" fn xml_dump_entities_table(buf: XmlBufPtr, table: XmlEntitiesTablePtr) {
    xml_hash_scan(table, Some(xml_dump_entity_decl_scan), buf as _);
}

/// This will dump the quoted string value, taking care of the special
/// treatment required by %
#[doc(alias = "xmlDumpEntityContent")]
#[cfg(feature = "libxml_output")]
unsafe extern "C" fn xml_dump_entity_content(buf: XmlBufPtr, content: *const XmlChar) {
    if !xml_strchr(content, b'%').is_null() {
        let mut base: *const XmlChar;
        let mut cur: *const XmlChar;

        xml_buf_ccat(buf, c"\"".as_ptr() as _);
        base = content;
        cur = content;
        while *cur != 0 {
            if *cur == b'"' {
                if base != cur {
                    xml_buf_add(buf, base, cur.offset_from(base) as _);
                }
                xml_buf_add(buf, c"&quot;".as_ptr() as _, 6);
                cur = cur.add(1);
                base = cur;
            } else if *cur == b'%' {
                if base != cur {
                    xml_buf_add(buf, base, cur.offset_from(base) as _);
                }
                xml_buf_add(buf, c"&#x25;".as_ptr() as _, 6);
                cur = cur.add(1);
                base = cur;
            } else {
                cur = cur.add(1);
            }
        }
        if base != cur {
            xml_buf_add(buf, base, cur.offset_from(base) as _);
        }
        xml_buf_ccat(buf, c"\"".as_ptr() as _);
    } else {
        xml_buf_write_quoted_string(buf, content);
    }
}

/// This will dump the content of the entity table as an XML DTD definition
#[doc(alias = "xmlDumpEntityDecl")]
#[cfg(feature = "libxml_output")]
pub unsafe extern "C" fn xml_dump_entity_decl(buf: XmlBufPtr, ent: XmlEntityPtr) {
    if buf.is_null() || ent.is_null() {
        return;
    }
    match (*ent).etype {
        Some(XmlEntityType::XmlInternalGeneralEntity) => {
            xml_buf_ccat(buf, c"<!ENTITY ".as_ptr() as _);
            xml_buf_cat(buf, (*ent).name.load(Ordering::Relaxed) as _);
            xml_buf_ccat(buf, c" ".as_ptr() as _);
            if !(*ent).orig.load(Ordering::Relaxed).is_null() {
                xml_buf_write_quoted_string(buf, (*ent).orig.load(Ordering::Relaxed) as _);
            } else {
                xml_dump_entity_content(buf, (*ent).content.load(Ordering::Relaxed) as _);
            }
            xml_buf_ccat(buf, c">\n".as_ptr() as _);
        }
        Some(XmlEntityType::XmlExternalGeneralParsedEntity) => {
            xml_buf_ccat(buf, c"<!ENTITY ".as_ptr() as _);
            xml_buf_cat(buf, (*ent).name.load(Ordering::Relaxed) as _);
            if !(*ent).external_id.load(Ordering::Relaxed).is_null() {
                xml_buf_ccat(buf, c" PUBLIC ".as_ptr() as _);
                xml_buf_write_quoted_string(buf, (*ent).external_id.load(Ordering::Relaxed) as _);
                xml_buf_ccat(buf, c" ".as_ptr() as _);
                xml_buf_write_quoted_string(buf, (*ent).system_id.load(Ordering::Relaxed) as _);
            } else {
                xml_buf_ccat(buf, c" SYSTEM ".as_ptr() as _);
                xml_buf_write_quoted_string(buf, (*ent).system_id.load(Ordering::Relaxed) as _);
            }
            xml_buf_ccat(buf, c">\n".as_ptr() as _);
        }
        Some(XmlEntityType::XmlExternalGeneralUnparsedEntity) => {
            xml_buf_ccat(buf, c"<!ENTITY ".as_ptr() as _);
            xml_buf_cat(buf, (*ent).name.load(Ordering::Relaxed) as _);
            if !(*ent).external_id.load(Ordering::Relaxed).is_null() {
                xml_buf_ccat(buf, c" PUBLIC ".as_ptr() as _);
                xml_buf_write_quoted_string(buf, (*ent).external_id.load(Ordering::Relaxed) as _);
                xml_buf_ccat(buf, c" ".as_ptr() as _);
                xml_buf_write_quoted_string(buf, (*ent).system_id.load(Ordering::Relaxed) as _);
            } else {
                xml_buf_ccat(buf, c" SYSTEM ".as_ptr() as _);
                xml_buf_write_quoted_string(buf, (*ent).system_id.load(Ordering::Relaxed) as _);
            }
            if !(*ent).content.load(Ordering::Relaxed).is_null() {
                /* Should be true ! */
                xml_buf_ccat(buf, c" NDATA ".as_ptr() as _);
                if !(*ent).orig.load(Ordering::Relaxed).is_null() {
                    xml_buf_cat(buf, (*ent).orig.load(Ordering::Acquire) as _);
                } else {
                    xml_buf_cat(buf, (*ent).content.load(Ordering::Acquire) as _);
                }
            }
            xml_buf_ccat(buf, c">\n".as_ptr() as _);
        }
        Some(XmlEntityType::XmlInternalParameterEntity) => {
            xml_buf_ccat(buf, c"<!ENTITY % ".as_ptr() as _);
            xml_buf_cat(buf, (*ent).name.load(Ordering::Relaxed) as _);
            xml_buf_ccat(buf, c" ".as_ptr() as _);
            if (*ent).orig.load(Ordering::Relaxed).is_null() {
                xml_dump_entity_content(buf, (*ent).content.load(Ordering::Relaxed) as _);
            } else {
                xml_buf_write_quoted_string(buf, (*ent).orig.load(Ordering::Relaxed) as _);
            }
            xml_buf_ccat(buf, c">\n".as_ptr() as _);
        }
        Some(XmlEntityType::XmlExternalParameterEntity) => {
            xml_buf_ccat(buf, c"<!ENTITY % ".as_ptr() as _);
            xml_buf_cat(buf, (*ent).name.load(Ordering::Relaxed) as _);
            if !(*ent).external_id.load(Ordering::Relaxed).is_null() {
                xml_buf_ccat(buf, c" PUBLIC ".as_ptr() as _);
                xml_buf_write_quoted_string(buf, (*ent).external_id.load(Ordering::Relaxed) as _);
                xml_buf_ccat(buf, c" ".as_ptr() as _);
                xml_buf_write_quoted_string(buf, (*ent).system_id.load(Ordering::Relaxed) as _);
            } else {
                xml_buf_ccat(buf, c" SYSTEM ".as_ptr() as _);
                xml_buf_write_quoted_string(buf, (*ent).system_id.load(Ordering::Relaxed) as _);
            }
            xml_buf_ccat(buf, c">\n".as_ptr() as _);
        }
        _ => {
            xml_entities_err(
                XmlParserErrors::XmlDTDUnknownEntity,
                c"xmlDumpEntitiesDecl: internal: unknown type entity type".as_ptr() as _,
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_add_doc_entity() {
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_type in 0..GEN_NB_INT {
                        for n_external_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            for n_system_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                    let mem_base = xml_mem_blocks();
                                    let doc = gen_xml_doc_ptr(n_doc, 0);
                                    let name = gen_const_xml_char_ptr(n_name, 1);
                                    let typ = gen_int(n_type, 2);
                                    let external_id = gen_const_xml_char_ptr(n_external_id, 3);
                                    let system_id = gen_const_xml_char_ptr(n_system_id, 4);
                                    let content = gen_const_xml_char_ptr(n_content, 5);

                                    let ret_val = xml_add_doc_entity(
                                        doc,
                                        name,
                                        typ,
                                        external_id,
                                        system_id,
                                        content,
                                    );
                                    desret_xml_entity_ptr(ret_val);
                                    des_xml_doc_ptr(n_doc, doc, 0);
                                    des_const_xml_char_ptr(n_name, name, 1);
                                    des_int(n_type, typ, 2);
                                    des_const_xml_char_ptr(n_external_id, external_id, 3);
                                    des_const_xml_char_ptr(n_system_id, system_id, 4);
                                    des_const_xml_char_ptr(n_content, content, 5);
                                    reset_last_error();
                                    if mem_base != xml_mem_blocks() {
                                        leaks += 1;
                                        eprint!(
                                            "Leak of {} blocks found in xmlAddDocEntity",
                                            xml_mem_blocks() - mem_base
                                        );
                                        eprint!(" {}", n_doc);
                                        eprint!(" {}", n_name);
                                        eprint!(" {}", n_type);
                                        eprint!(" {}", n_system_id);
                                        eprint!(" {}", n_system_id);
                                        eprintln!(" {}", n_content);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlAddDocEntity()");
        }
    }

    #[test]
    fn test_xml_add_dtd_entity() {
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_type in 0..GEN_NB_INT {
                        for n_external_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            for n_system_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                    let mem_base = xml_mem_blocks();
                                    let doc = gen_xml_doc_ptr(n_doc, 0);
                                    let name = gen_const_xml_char_ptr(n_name, 1);
                                    let typ = gen_int(n_type, 2);
                                    let external_id = gen_const_xml_char_ptr(n_external_id, 3);
                                    let system_id = gen_const_xml_char_ptr(n_system_id, 4);
                                    let content = gen_const_xml_char_ptr(n_content, 5);

                                    let ret_val = xml_add_dtd_entity(
                                        doc,
                                        name,
                                        typ,
                                        external_id,
                                        system_id,
                                        content,
                                    );
                                    desret_xml_entity_ptr(ret_val);
                                    des_xml_doc_ptr(n_doc, doc, 0);
                                    des_const_xml_char_ptr(n_name, name, 1);
                                    des_int(n_type, typ, 2);
                                    des_const_xml_char_ptr(n_external_id, external_id, 3);
                                    des_const_xml_char_ptr(n_system_id, system_id, 4);
                                    des_const_xml_char_ptr(n_content, content, 5);
                                    reset_last_error();
                                    if mem_base != xml_mem_blocks() {
                                        leaks += 1;
                                        eprint!(
                                            "Leak of {} blocks found in xmlAddDtdEntity",
                                            xml_mem_blocks() - mem_base
                                        );
                                        eprint!(" {}", n_doc);
                                        eprint!(" {}", n_name);
                                        eprint!(" {}", n_type);
                                        eprint!(" {}", n_external_id);
                                        eprint!(" {}", n_system_id);
                                        eprintln!(" {}", n_content);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlAddDtdEntity()");
        }
    }

    #[test]
    fn test_xml_copy_entities_table() {

        /* missing type support */
    }

    #[test]
    fn test_xml_create_entities_table() {

        /* missing type support */
    }

    #[test]
    fn test_xml_dump_entities_table() {
        #[cfg(feature = "libxml_output")]
        unsafe {
            let mut leaks = 0;

            for n_buf in 0..GEN_NB_XML_BUFFER_PTR {
                for n_table in 0..GEN_NB_XML_ENTITIES_TABLE_PTR {
                    let mem_base = xml_mem_blocks();
                    let buf = gen_const_xml_buf_ptr(n_buf, 0) as _;
                    let table = gen_xml_entities_table_ptr(n_table, 1);

                    xml_dump_entities_table(buf, table);
                    des_const_xml_buf_ptr(n_buf, buf as _, 0);
                    des_xml_entities_table_ptr(n_table, table, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlDumpEntitiesTable",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_buf);
                        eprintln!(" {}", n_table);
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlDumpEntitiesTable()"
            );
        }
    }

    #[test]
    fn test_xml_dump_entity_decl() {
        #[cfg(feature = "libxml_output")]
        unsafe {
            let mut leaks = 0;

            for n_buf in 0..GEN_NB_XML_BUFFER_PTR {
                for n_ent in 0..GEN_NB_XML_ENTITY_PTR {
                    let mem_base = xml_mem_blocks();
                    let buf = gen_const_xml_buf_ptr(n_buf, 0) as _;
                    let ent = gen_xml_entity_ptr(n_ent, 1);

                    xml_dump_entity_decl(buf, ent);
                    des_const_xml_buf_ptr(n_buf, buf, 0);
                    des_xml_entity_ptr(n_ent, ent, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlDumpEntityDecl",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_buf);
                        eprintln!(" {}", n_ent);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlDumpEntityDecl()");
        }
    }

    #[test]
    fn test_xml_encode_entities_reentrant() {
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_input in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let doc = gen_xml_doc_ptr(n_doc, 0);
                    let input = gen_const_xml_char_ptr(n_input, 1);

                    let ret_val = xml_encode_entities_reentrant(doc, input);
                    desret_xml_char_ptr(ret_val);
                    des_xml_doc_ptr(n_doc, doc, 0);
                    des_const_xml_char_ptr(n_input, input, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlEncodeEntitiesReentrant",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_doc);
                        eprintln!(" {}", n_input);
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlEncodeEntitiesReentrant()"
            );
        }
    }

    #[test]
    fn test_xml_encode_special_chars() {
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_CONST_XML_DOC_PTR {
                for n_input in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let doc = gen_const_xml_doc_ptr(n_doc, 0);
                    let input = gen_const_xml_char_ptr(n_input, 1);

                    let ret_val = xml_encode_special_chars(doc, input);
                    desret_xml_char_ptr(ret_val);
                    des_const_xml_doc_ptr(n_doc, doc, 0);
                    des_const_xml_char_ptr(n_input, input, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlEncodeSpecialChars",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_doc);
                        eprintln!(" {}", n_input);
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlEncodeSpecialChars()"
            );
        }
    }

    #[test]
    fn test_xml_get_doc_entity() {
        unsafe {
            let mut leaks = 0;
            for n_doc in 0..GEN_NB_CONST_XML_DOC_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let doc = gen_const_xml_doc_ptr(n_doc, 0);
                    let name = gen_const_xml_char_ptr(n_name, 1);

                    let ret_val = xml_get_doc_entity(doc, name);
                    desret_xml_entity_ptr(ret_val);
                    des_const_xml_doc_ptr(n_doc, doc, 0);
                    des_const_xml_char_ptr(n_name, name, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlGetDocEntity",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_doc);
                        eprintln!(" {}", n_name);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlGetDocEntity()");
        }
    }

    #[test]
    fn test_xml_get_dtd_entity() {
        unsafe {
            let mut leaks = 0;
            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let doc = gen_xml_doc_ptr(n_doc, 0);
                    let name = gen_const_xml_char_ptr(n_name, 1);

                    let ret_val = xml_get_dtd_entity(doc, name);
                    desret_xml_entity_ptr(ret_val);
                    des_xml_doc_ptr(n_doc, doc, 0);
                    des_const_xml_char_ptr(n_name, name, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlGetDtdEntity",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_doc);
                        eprintln!(" {}", n_name);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlGetDtdEntity()");
        }
    }

    #[test]
    fn test_xml_get_parameter_entity() {
        unsafe {
            let mut leaks = 0;
            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let doc = gen_xml_doc_ptr(n_doc, 0);
                    let name = gen_const_xml_char_ptr(n_name, 1);

                    let ret_val = xml_get_parameter_entity(doc, name);
                    desret_xml_entity_ptr(ret_val);
                    des_xml_doc_ptr(n_doc, doc, 0);
                    des_const_xml_char_ptr(n_name, name, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlGetParameterEntity",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_doc);
                        eprintln!(" {}", n_name);
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlGetParameterEntity()"
            );
        }
    }

    #[test]
    fn test_xml_get_predefined_entity() {
        unsafe {
            let mut leaks = 0;

            for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let name = gen_const_xml_char_ptr(n_name, 0);

                let ret_val = xml_get_predefined_entity(name as *const XmlChar);
                desret_xml_entity_ptr(ret_val);
                des_const_xml_char_ptr(n_name, name, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlGetPredefinedEntity",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_name);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlGetPredefinedEntity()"
            );
        }
    }

    #[test]
    fn test_xml_new_entity() {
        unsafe {
            let mut leaks = 0;

            let n_doc = 2;
            let n_name = 2;
            let n_type = 1;
            let n_external_id = 0;
            let n_system_id = 0;
            let n_content = 0;
            let mem_base = xml_mem_blocks();
            let doc = gen_xml_doc_ptr(n_doc, 0);
            let name = gen_const_xml_char_ptr(n_name, 1);
            let typ = gen_int(n_type, 2);
            let external_id = gen_const_xml_char_ptr(n_external_id, 3);
            let system_id = gen_const_xml_char_ptr(n_system_id, 4);
            let content = gen_const_xml_char_ptr(n_content, 5);

            let ret_val = xml_new_entity(doc, name, typ, external_id, system_id, content);
            desret_xml_entity_ptr(ret_val);
            des_xml_doc_ptr(n_doc, doc, 0);
            des_const_xml_char_ptr(n_name, name, 1);
            des_int(n_type, typ, 2);
            des_const_xml_char_ptr(n_external_id, external_id, 3);
            des_const_xml_char_ptr(n_system_id, system_id, 4);
            des_const_xml_char_ptr(n_content, content, 5);
            reset_last_error();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprint!(
                    "Leak of {} blocks found in xmlNewEntity",
                    xml_mem_blocks() - mem_base
                );
                eprint!(" {}", n_doc);
                eprint!(" {}", n_name);
                eprint!(" {}", n_type);
                eprint!(" {}", n_external_id);
                eprint!(" {}", n_system_id);
                eprintln!(" {}", n_content);
            }

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_type in 0..GEN_NB_INT {
                        for n_external_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            for n_system_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                    let mem_base = xml_mem_blocks();
                                    let doc = gen_xml_doc_ptr(n_doc, 0);
                                    let name = gen_const_xml_char_ptr(n_name, 1);
                                    let typ = gen_int(n_type, 2);
                                    let external_id = gen_const_xml_char_ptr(n_external_id, 3);
                                    let system_id = gen_const_xml_char_ptr(n_system_id, 4);
                                    let content = gen_const_xml_char_ptr(n_content, 5);

                                    let ret_val = xml_new_entity(
                                        doc,
                                        name,
                                        typ,
                                        external_id,
                                        system_id,
                                        content,
                                    );
                                    desret_xml_entity_ptr(ret_val);
                                    des_xml_doc_ptr(n_doc, doc, 0);
                                    des_const_xml_char_ptr(n_name, name, 1);
                                    des_int(n_type, typ, 2);
                                    des_const_xml_char_ptr(n_external_id, external_id, 3);
                                    des_const_xml_char_ptr(n_system_id, system_id, 4);
                                    des_const_xml_char_ptr(n_content, content, 5);
                                    reset_last_error();
                                    if mem_base != xml_mem_blocks() {
                                        leaks += 1;
                                        eprint!(
                                            "Leak of {} blocks found in xmlNewEntity",
                                            xml_mem_blocks() - mem_base
                                        );
                                        eprint!(" {}", n_doc);
                                        eprint!(" {}", n_name);
                                        eprint!(" {}", n_type);
                                        eprint!(" {}", n_external_id);
                                        eprint!(" {}", n_system_id);
                                        eprintln!(" {}", n_content);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlNewEntity()");
        }
    }
}
