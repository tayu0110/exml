use std::{ffi::c_void, ptr::null_mut};

use crate::{
    hash::{xml_hash_free, XmlHashTablePtr},
    libxml::{
        globals::{xml_free, xml_malloc},
        relaxng::{xml_relaxng_free_partition, XmlRelaxNGPartitionPtr, XmlRelaxNGType},
        xmlregexp::{xml_reg_free_regexp, XmlRegexpPtr},
    },
    tree::XmlNodePtr,
};

use super::{xml_rng_perr_memory, XmlRelaxNGParserCtxtPtr, XmlRelaxNGTypeLibraryPtr};

pub type XmlRelaxNGDefinePtr = *mut XmlRelaxNGDefine;

// TODO: all fieleds are used in only relaxng module.
#[repr(C)]
pub struct XmlRelaxNGDefine {
    pub(crate) typ: XmlRelaxNGType,             // the type of definition
    pub(crate) node: Option<XmlNodePtr>,        // the node in the source
    pub(crate) name: *mut u8,                   // the element local name if present
    pub(crate) ns: *mut u8,                     // the namespace local name if present
    pub(crate) value: *mut u8,                  // value when available
    pub(crate) data: *mut c_void,               // data lib or specific pointer
    pub(crate) content: XmlRelaxNGDefinePtr,    // the expected content
    pub(crate) parent: XmlRelaxNGDefinePtr,     // the parent definition, if any
    pub(crate) next: XmlRelaxNGDefinePtr,       // list within grouping sequences
    pub(crate) attrs: XmlRelaxNGDefinePtr,      // list of attributes for elements
    pub(crate) name_class: XmlRelaxNGDefinePtr, // the nameClass definition if any
    pub(crate) next_hash: XmlRelaxNGDefinePtr,  // next define in defs/refs hash tables
    pub(crate) depth: i16,                      // used for the cycle detection
    pub(crate) dflags: i16,                     // define related flags
    pub(crate) cont_model: XmlRegexpPtr,        // a compiled content model if available
}

impl XmlRelaxNGDefine {
    #[doc(alias = "xmlRelaxNGDefName")]
    pub(crate) fn name(&self) -> &'static str {
        match self.typ {
            XmlRelaxNGType::Empty => "empty",
            XmlRelaxNGType::NotAllowed => "notAllowed",
            XmlRelaxNGType::Except => "except",
            XmlRelaxNGType::Text => "text",
            XmlRelaxNGType::Element => "element",
            XmlRelaxNGType::Datatype => "datatype",
            XmlRelaxNGType::Value => "value",
            XmlRelaxNGType::List => "list",
            XmlRelaxNGType::Attribute => "attribute",
            XmlRelaxNGType::Def => "def",
            XmlRelaxNGType::Ref => "ref",
            XmlRelaxNGType::Externalref => "externalRef",
            XmlRelaxNGType::Parentref => "parentRef",
            XmlRelaxNGType::Optional => "optional",
            XmlRelaxNGType::Zeroormore => "zeroOrMore",
            XmlRelaxNGType::Oneormore => "oneOrMore",
            XmlRelaxNGType::Choice => "choice",
            XmlRelaxNGType::Group => "group",
            XmlRelaxNGType::Interleave => "interleave",
            XmlRelaxNGType::Start => "start",
            XmlRelaxNGType::Noop => "noop",
            XmlRelaxNGType::Param => "param",
        }
    }
}

impl Default for XmlRelaxNGDefine {
    fn default() -> Self {
        Self {
            typ: XmlRelaxNGType::default(),
            node: None,
            name: null_mut(),
            ns: null_mut(),
            value: null_mut(),
            data: null_mut(),
            content: null_mut(),
            parent: null_mut(),
            next: null_mut(),
            attrs: null_mut(),
            name_class: null_mut(),
            next_hash: null_mut(),
            depth: 0,
            dflags: 0,
            cont_model: null_mut(),
        }
    }
}

/// Allocate a new RelaxNG define.
///
/// Returns the newly allocated structure or NULL in case or error
#[doc(alias = "xmlRelaxNGNewDefine")]
pub(crate) unsafe fn xml_relaxng_new_define(
    ctxt: XmlRelaxNGParserCtxtPtr,
    node: Option<XmlNodePtr>,
) -> XmlRelaxNGDefinePtr {
    let ret: XmlRelaxNGDefinePtr = xml_malloc(size_of::<XmlRelaxNGDefine>()) as _;
    if ret.is_null() {
        xml_rng_perr_memory(ctxt, Some("allocating define\n"));
        return null_mut();
    }
    std::ptr::write(&mut *ret, XmlRelaxNGDefine::default());
    (*ctxt).def_tab.push(ret);
    (*ret).node = node;
    (*ret).depth = -1;
    ret
}

/// Deallocate a RelaxNG define structure.
#[doc(alias = "xmlRelaxNGFreeDefine")]
pub(crate) unsafe fn xml_relaxng_free_define(define: XmlRelaxNGDefinePtr) {
    if define.is_null() {
        return;
    }

    if (*define).typ == XmlRelaxNGType::Value && !(*define).attrs.is_null() {
        let lib: XmlRelaxNGTypeLibraryPtr = (*define).data as _;
        if !lib.is_null() {
            if let Some(freef) = (*lib).freef {
                freef((*lib).data, (*define).attrs as _);
            }
        }
    }
    if !(*define).data.is_null() && (*define).typ == XmlRelaxNGType::Interleave {
        xml_relaxng_free_partition((*define).data as XmlRelaxNGPartitionPtr);
    }
    if !(*define).data.is_null() && (*define).typ == XmlRelaxNGType::Choice {
        xml_hash_free((*define).data as XmlHashTablePtr, None);
    }
    if !(*define).name.is_null() {
        xml_free((*define).name as _);
    }
    if !(*define).ns.is_null() {
        xml_free((*define).ns as _);
    }
    if !(*define).value.is_null() {
        xml_free((*define).value as _);
    }
    if !(*define).cont_model.is_null() {
        xml_reg_free_regexp((*define).cont_model);
    }
    xml_free(define as _);
}
