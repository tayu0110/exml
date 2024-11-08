use std::{os::raw::c_void, ptr::null_mut};

use crate::{dict::XmlDict, encoding::XmlCharEncoding};

use super::{XmlDtd, XmlElementType, XmlNode, XmlNs};

/// An XML document.
pub type XmlDocPtr = *mut XmlDoc;
#[repr(C)]
pub struct XmlDoc {
    pub(crate) _private: *mut c_void, /* application data */
    pub(crate) typ: XmlElementType,   /* XML_DOCUMENT_NODE, must be second ! */
    pub(crate) name: *mut i8,         /* name/filename/URI of the document */
    pub children: *mut XmlNode,       /* the document tree */
    pub(crate) last: *mut XmlNode,    /* last child link */
    pub(crate) parent: *mut XmlNode,  /* child->parent link */
    pub(crate) next: *mut XmlNode,    /* next sibling link  */
    pub(crate) prev: *mut XmlNode,    /* previous sibling link  */
    pub(crate) doc: *mut XmlDoc,      /* autoreference to itself */

    /* End of common part */
    pub(crate) compression: i32, /* level of zlib compression */
    pub(crate) standalone: i32,  /* standalone document (no external refs)
                                  1 if standalone="yes"
                                  0 if standalone="no"
                                 -1 if there is no XML declaration
                                 -2 if there is an XML declaration, but no
                                 standalone attribute was specified */
    pub int_subset: *mut XmlDtd,        /* the document internal subset */
    pub(crate) ext_subset: *mut XmlDtd, /* the document external subset */
    pub(crate) old_ns: *mut XmlNs,      /* Global namespace, the old way */
    pub(crate) version: Option<String>, /* the XML version string */
    pub(crate) encoding: Option<String>, /* external initial encoding, if any */
    pub(crate) ids: *mut c_void,        /* Hash table for ID attributes if any */
    pub(crate) refs: *mut c_void,       /* Hash table for IDREFs attributes if any */
    pub(crate) url: Option<String>,     /* The URI for that document */
    pub(crate) charset: XmlCharEncoding, /* Internal flag for charset handling,
                                        actually an xmlCharEncoding */
    pub dict: *mut XmlDict,       /* dict used to allocate names or NULL */
    pub(crate) psvi: *mut c_void, /* for type/PSVI information */
    pub(crate) parse_flags: i32,  /* set of xmlParserOption used to parse the
                                  document */
    pub properties: i32, /* set of xmlDocProperties for this document
                         set at the end of parsing */
}

impl Default for XmlDoc {
    fn default() -> Self {
        Self {
            _private: null_mut(),
            typ: XmlElementType::default(),
            name: null_mut(),
            children: null_mut(),
            last: null_mut(),
            parent: null_mut(),
            next: null_mut(),
            prev: null_mut(),
            doc: null_mut(),
            compression: 0,
            standalone: 0,
            int_subset: null_mut(),
            ext_subset: null_mut(),
            old_ns: null_mut(),
            version: None,
            encoding: None,
            ids: null_mut(),
            refs: null_mut(),
            url: None,
            charset: XmlCharEncoding::None,
            dict: null_mut(),
            psvi: null_mut(),
            parse_flags: 0,
            properties: 0,
        }
    }
}
