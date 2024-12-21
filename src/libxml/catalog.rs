//! Provide methods and data structures for XML Catalogs.  
//! This module is based on `libxml/catalog.h`, `catalog.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: interfaces to the Catalog handling system
// Description: the catalog module implements the support for
// XML Catalogs and SGML catalogs
//
// SGML Open Technical Resolution TR9401:1997.
// http://www.jclark.com/sp/catalog.htm
//
// XML Catalogs Working Draft 06 August 2001
// http://www.oasis-open.org/committees/entity/spec-2001-08-06.html
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// catalog.c: set of generic Catalog related routines
//
// Reference:  SGML Open Technical Resolution TR9401:1997.
//             http://www.jclark.com/sp/catalog.htm
//
//             XML Catalogs Working Draft 06 August 2001
//             http://www.oasis-open.org/committees/entity/spec-2001-08-06.html
//
// See Copyright for the status of this software.
//
// Daniel.Veillard@imag.fr

#[cfg(feature = "libxml_output")]
use std::io::Write;
use std::{
    borrow::Cow,
    cell::RefCell,
    collections::HashMap,
    ffi::{c_char, CStr, CString, OsStr, OsString},
    fs::File,
    io::Read,
    mem::size_of,
    os::raw::c_void,
    path::{Path, PathBuf},
    ptr::{drop_in_place, null_mut},
    rc::Rc,
    str::from_utf8,
    sync::atomic::{AtomicBool, AtomicI32, AtomicPtr, Ordering},
};

use const_format::concatcp;
use libc::snprintf;

use crate::{
    encoding::XmlCharEncoding,
    error::__xml_raise_error,
    generic_error,
    io::{xml_parser_get_directory, XmlParserInputBuffer},
    libxml::{
        chvalid::xml_is_blank_char,
        globals::{xml_free, xml_malloc},
        hash::{xml_hash_add_entry, xml_hash_create, xml_hash_free, xml_hash_lookup, XmlHashTable},
        parser::{
            xml_free_parser_ctxt, xml_new_parser_ctxt, xml_parse_document, XmlParserCtxtPtr,
            XmlParserInputPtr,
        },
        parser_internals::{xml_new_input_stream, XML_MAX_NAMELEN},
        threads::{
            xml_free_rmutex, xml_get_thread_id, xml_new_rmutex, xml_rmutex_lock, xml_rmutex_unlock,
            XmlRMutex,
        },
        xmlstring::{xml_str_equal, xml_strcat, xml_strdup, xml_strndup, XmlChar},
    },
    tree::{
        xml_free_doc, xml_free_ns, xml_new_doc, xml_new_doc_node, xml_new_dtd, xml_new_ns,
        NodeCommon, XmlDocPtr, XmlDtdPtr, XmlNodePtr, XmlNsPtr, XML_XML_NAMESPACE,
    },
    uri::{build_uri, canonic_path},
    SYSCONFDIR,
};

use super::{chvalid::xml_is_pubid_char, hash::CVoidWrapper, parser_internals::xml_is_letter};

/// Handle an out of memory condition
#[doc(alias = "xmlCatalogErrMemory")]
unsafe fn xml_catalog_err_memory(extra: &str) {
    __xml_raise_error!(
        None,
        None,
        None,
        null_mut(),
        null_mut(),
        XmlErrorDomain::XmlFromCatalog,
        XmlParserErrors::XmlErrNoMemory,
        XmlErrorLevel::XmlErrError,
        None,
        0,
        Some(extra.to_owned().into()),
        None,
        None,
        0,
        0,
        "Memory allocation failed : {}\n",
        extra
    );
}

/// Handle a catalog error
#[doc(alias = "xmlCatalogErr")]
macro_rules! xml_catalog_err {
    (
        $catal:expr,
        $node:expr,
        $error:expr,
        $msg:literal,
        $str1:expr,
    ) => {
        let msg = format!($msg, $str1);
        xml_catalog_err!(@inner $catal, $node, $error, &msg, Some($str1.to_owned().into()), None, None,);
    };
    (
        $catal:expr,
        $node:expr,
        $error:expr,
        $msg:literal,
        $str1:expr,
        $str2:expr,
    ) => {
        let msg = format!($msg, $str1, $str2);
        xml_catalog_err!(@inner $catal, $node, $error, &msg, Some($str1.to_owned().into()), Some($str2.to_owned().into()), None,);
    };
    (
        $catal:expr,
        $node:expr,
        $error:expr,
        $msg:literal,
        $str1:expr,
        $str2:expr,
        $str3:expr,
    ) => {
        let msg = format!($msg, $str1, $str2, $str3);
        xml_catalog_err!(
            @inner
            $catal,
            $node,
            $error,
            &msg,
            Some($str1.to_owned().into()),
            Some($str2.to_owned().into()),
            Some($str3.to_owned().into()),
        );
    };
    (
        @inner
        $catal:expr,
        $node:expr,
        $error:expr,
        $msg:expr,
        $str1:expr,
        $str2:expr,
        $str3:expr,
    ) => {
        __xml_raise_error!(
            None,
            None,
            None,
            $catal as _,
            $node as _,
            XmlErrorDomain::XmlFromCatalog,
            $error,
            XmlErrorLevel::XmlErrError,
            None,
            0,
            $str1,
            $str2,
            $str3,
            0,
            0,
            $msg,
        );
    };
}

/// The namespace for the XML Catalogs elements.
pub(crate) const XML_CATALOGS_NAMESPACE: &CStr = c"urn:oasis:names:tc:entity:xmlns:xml:catalog";
/// The specific XML Catalog Processing Instruction name.
pub(crate) const XML_CATALOG_PI: &CStr = c"oasis-xml-catalog";

/// The API is voluntarily limited to general cataloging.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum XmlCatalogPrefer {
    #[default]
    None = 0,
    Public = 1,
    System,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlCatalogAllow {
    None = 0,
    Global = 1,
    Document = 2,
    All = 3,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub(crate) enum XmlCatalogEntryType {
    XmlCataRemoved = -1,
    #[default]
    XmlCataNone = 0,
    XmlCataCatalog,
    XmlCataBrokenCatalog,
    XmlCataNextCatalog,
    XmlCataGroup,
    XmlCataPublic,
    XmlCataSystem,
    XmlCataRewriteSystem,
    XmlCataDelegatePublic,
    XmlCataDelegateSystem,
    XmlCataURI,
    XmlCataRewriteURI,
    XmlCataDelegateURI,
    SgmlCataSystem,
    SgmlCataPublic,
    SgmlCataEntity,
    SgmlCataPentity,
    SgmlCataDoctype,
    SgmlCataLinktype,
    SgmlCataNotation,
    SgmlCataDelegate,
    SgmlCataBase,
    SgmlCataCatalog,
    SgmlCataDocument,
    SgmlCataSGMLDecl,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub(crate) enum XmlCatalogType {
    #[default]
    XmlXMLCatalogType = 1,
    XmlSGMLCatalogType,
}

const XML_MAX_SGML_CATA_DEPTH: usize = 10;

pub type XmlCatalogPtr = *mut XmlCatalog;
#[repr(C)]
pub struct XmlCatalog {
    typ: XmlCatalogType, /* either XML or SGML */

    // These fields seems to be unused in original libxml2...
    // What is the field for?
    // --------------------------
    // SGML Catalogs are stored as a simple hash table of catalog entries
    // Catalog stack to check against overflows when building the SGML catalog
    // catal_tab: [*mut c_char; XML_MAX_SGML_CATA_DEPTH], /* stack of catals */
    // catal_nr: i32,                                     /* Number of current catal streams */
    // catal_max: i32,                                    /* Max number of catal streams */
    sgml: HashMap<String, XmlCatalogEntryPtr>,

    // XML Catalogs are stored as a tree of Catalog entries
    prefer: XmlCatalogPrefer,
    xml: XmlCatalogEntryPtr,
}

impl XmlCatalog {
    /// Parse an SGML catalog content and fill up the @catal hash table with
    /// the new entries found.
    ///
    /// Returns 0 in case of success, -1 in case of error.
    #[doc(alias = "xmlParseSGMLCatalog")]
    unsafe fn parse_sgml_catalog(
        &mut self,
        value: &[u8],
        file: impl AsRef<Path>,
        is_super: i32,
    ) -> i32 {
        let mut cur = value;

        let mut base = file.as_ref().to_string_lossy().into_owned();

        while !cur.is_empty() {
            cur = skip_blanks(cur);
            if cur.is_empty() {
                break;
            }
            if cur.starts_with(b"--") {
                let Some(rem) = xml_parse_sgmlcatalog_comment(cur) else {
                    return -1;
                };
                cur = rem;
            } else {
                let mut typ: XmlCatalogEntryType = XmlCatalogEntryType::XmlCataNone;

                let Some((name, rem)) = xml_parse_sgml_catalog_name(cur) else {
                    return -1;
                };
                cur = rem;
                if cur
                    .first()
                    .filter(|&&b| xml_is_blank_char(b as u32))
                    .is_none()
                {
                    return -1;
                }
                cur = skip_blanks(cur);
                if name == b"SYSTEM" {
                    typ = XmlCatalogEntryType::SgmlCataSystem;
                } else if name == b"PUBLIC" {
                    typ = XmlCatalogEntryType::SgmlCataPublic;
                } else if name == b"DELEGATE" {
                    typ = XmlCatalogEntryType::SgmlCataDelegate;
                } else if name == b"ENTITY" {
                    typ = XmlCatalogEntryType::SgmlCataEntity;
                } else if name == b"DOCTYPE" {
                    typ = XmlCatalogEntryType::SgmlCataDoctype;
                } else if name == b"LINKTYPE" {
                    typ = XmlCatalogEntryType::SgmlCataLinktype;
                } else if name == b"NOTATION" {
                    typ = XmlCatalogEntryType::SgmlCataNotation;
                } else if name == b"SGMLDECL" {
                    typ = XmlCatalogEntryType::SgmlCataSGMLDecl;
                } else if name == b"DOCUMENT" {
                    typ = XmlCatalogEntryType::SgmlCataDocument;
                } else if name == b"CATALOG" {
                    typ = XmlCatalogEntryType::SgmlCataCatalog;
                } else if name == b"BASE" {
                    typ = XmlCatalogEntryType::SgmlCataBase;
                } else if name == b"OVERRIDE" {
                    let Some((_, rem)) = xml_parse_sgml_catalog_name(cur) else {
                        return -1;
                    };
                    cur = rem;
                    continue;
                }

                let (name, sysid) = match typ {
                    ty @ XmlCatalogEntryType::SgmlCataEntity
                    | ty @ XmlCatalogEntryType::SgmlCataPentity
                    | ty @ XmlCatalogEntryType::SgmlCataDoctype
                    | ty @ XmlCatalogEntryType::SgmlCataLinktype
                    | ty @ XmlCatalogEntryType::SgmlCataNotation => {
                        if matches!(ty, XmlCatalogEntryType::SgmlCataEntity)
                            && cur.first() == Some(&b'%')
                        {
                            typ = XmlCatalogEntryType::SgmlCataPentity;
                        }
                        let Some((name, rem)) = xml_parse_sgml_catalog_name(cur) else {
                            return -1;
                        };
                        cur = rem;
                        if cur
                            .first()
                            .filter(|&&b| xml_is_blank_char(b as u32))
                            .is_none()
                        {
                            return -1;
                        }
                        cur = skip_blanks(cur);
                        let Some((sysid, rem)) = xml_parse_sgml_catalog_pubid(cur) else {
                            return -1;
                        };
                        cur = rem;
                        (Some(Cow::Borrowed(name)), Some(Cow::Borrowed(sysid)))
                    }
                    XmlCatalogEntryType::SgmlCataPublic
                    | XmlCatalogEntryType::SgmlCataSystem
                    | XmlCatalogEntryType::SgmlCataDelegate => {
                        let Some((name, rem)) = xml_parse_sgml_catalog_pubid(cur) else {
                            return -1;
                        };
                        cur = rem;
                        let mut cow_name = Some(Cow::Borrowed(name));
                        if !matches!(typ, XmlCatalogEntryType::SgmlCataSystem) {
                            if let Some(normid) = normalize_public(name) {
                                if !normid.is_empty() {
                                    cow_name = Some(Cow::Owned(normid));
                                } else {
                                    cow_name = None;
                                }
                            }
                        }
                        if cur
                            .first()
                            .filter(|&&b| xml_is_blank_char(b as u32))
                            .is_none()
                        {
                            return -1;
                        }
                        cur = skip_blanks(cur);
                        let Some((sysid, rem)) = xml_parse_sgml_catalog_pubid(cur) else {
                            return -1;
                        };
                        cur = rem;
                        (cow_name, Some(Cow::Borrowed(sysid)))
                    }
                    XmlCatalogEntryType::SgmlCataBase
                    | XmlCatalogEntryType::SgmlCataCatalog
                    | XmlCatalogEntryType::SgmlCataDocument
                    | XmlCatalogEntryType::SgmlCataSGMLDecl => {
                        let Some((sysid, rem)) = xml_parse_sgml_catalog_pubid(cur) else {
                            return -1;
                        };
                        cur = rem;
                        (None, Some(Cow::Borrowed(sysid)))
                    }
                    _ => (None, None),
                };
                if matches!(typ, XmlCatalogEntryType::SgmlCataBase) {
                    base = String::from_utf8_lossy(sysid.unwrap().as_ref()).into_owned();
                } else if matches!(
                    typ,
                    XmlCatalogEntryType::SgmlCataPublic | XmlCatalogEntryType::SgmlCataSystem
                ) {
                    let sysid = sysid.unwrap();
                    let sysid = String::from_utf8_lossy(sysid.as_ref());
                    if let Some(filename) = build_uri(&sysid, &base) {
                        let entry: XmlCatalogEntryPtr = xml_new_catalog_entry(
                            typ,
                            name.as_deref().and_then(|n| from_utf8(n).ok()),
                            Some(&filename),
                            None,
                            XmlCatalogPrefer::None,
                            null_mut(),
                        );
                        let name = String::from_utf8_lossy(name.unwrap().as_ref()).into_owned();
                        self.sgml.insert(name, entry);
                    }
                } else if matches!(typ, XmlCatalogEntryType::SgmlCataCatalog) {
                    if is_super != 0 {
                        let entry: XmlCatalogEntryPtr = xml_new_catalog_entry(
                            typ,
                            sysid.as_deref().and_then(|n| from_utf8(n).ok()),
                            None,
                            None,
                            XmlCatalogPrefer::None,
                            null_mut(),
                        );
                        let sysid = String::from_utf8_lossy(sysid.unwrap().as_ref()).into_owned();
                        self.sgml.insert(sysid, entry);
                    } else if let Some(sysid) = sysid {
                        let sysid = String::from_utf8_lossy(&sysid);
                        if let Some(filename) = build_uri(&sysid, &base) {
                            self.expand_catalog(filename);
                        }
                    }
                }
            }
        }
        0
    }

    /// Load the catalog and expand the existing catal structure.
    /// This can be either an XML Catalog or an SGML Catalog
    ///
    /// Returns 0 in case of success, -1 in case of error
    #[doc(alias = "xmlExpandCatalog")]
    unsafe fn expand_catalog(&mut self, filename: impl AsRef<Path>) -> i32 {
        let filename = filename.as_ref();

        if matches!(self.typ, XmlCatalogType::XmlSGMLCatalogType) {
            let Some(content) = xml_load_file_content(filename) else {
                return -1;
            };

            let ret = self.parse_sgml_catalog(&content, filename, 0);
            if ret < 0 {
                return -1;
            }
        } else {
            let mut cur: XmlCatalogEntryPtr;
            let tmp: XmlCatalogEntryPtr = xml_new_catalog_entry(
                XmlCatalogEntryType::XmlCataCatalog,
                None,
                None,
                Some(filename.to_owned()),
                XML_CATALOG_DEFAULT_PREFER,
                null_mut(),
            );

            cur = self.xml;
            if cur.is_null() {
                self.xml = tmp;
            } else {
                while !(*cur).next.is_null() {
                    cur = (*cur).next;
                }
                (*cur).next = tmp;
            }
        }
        0
    }

    /// Add an entry in the catalog, it may overwrite existing but different entries.
    ///
    /// Returns 0 if successful, -1 otherwise
    #[doc(alias = "xmlACatalogAdd")]
    pub unsafe fn add(
        &mut self,
        typ: Option<&str>,
        orig: Option<&str>,
        replace: *const XmlChar,
    ) -> i32 {
        let mut res: i32 = -1;

        if matches!(self.typ, XmlCatalogType::XmlXMLCatalogType) {
            res = (*self.xml).add_xml_catalog(typ, orig, replace);
        } else {
            let cattype: XmlCatalogEntryType = xml_get_sgml_catalog_entry_type(typ);
            if !matches!(cattype, XmlCatalogEntryType::XmlCataNone) {
                let entry: XmlCatalogEntryPtr = xml_new_catalog_entry(
                    cattype,
                    orig,
                    (!replace.is_null())
                        .then(|| CStr::from_ptr(replace as *const i8).to_string_lossy())
                        .as_deref(),
                    None,
                    XmlCatalogPrefer::None,
                    null_mut(),
                );
                if let Some(orig) = orig {
                    self.sgml.insert(orig.to_owned(), entry);
                    res = 0;
                }
                if res < 0 {
                    xml_free_catalog_entry(entry);
                }
            }
        }
        res
    }

    /// Remove an entry from the catalog
    ///
    /// Returns the number of entries removed if successful, -1 otherwise
    #[doc(alias = "xmlACatalogRemove")]
    pub unsafe fn remove(&mut self, value: &str) -> i32 {
        if matches!(self.typ, XmlCatalogType::XmlXMLCatalogType) {
            (*self.xml).del_xml_catalog(value)
        } else if let Some(removed) = self.sgml.remove(value) {
            xml_free_catalog_entry(removed);
            1
        } else {
            0
        }
    }

    /// Check is a catalog is empty
    ///
    /// Returns 1 if the catalog is empty, 0 if not, amd -1 in case of error.
    #[doc(alias = "xmlCatalogIsEmpty")]
    pub unsafe fn is_empty(&self) -> i32 {
        if matches!(self.typ, XmlCatalogType::XmlXMLCatalogType) {
            if self.xml.is_null() {
                return 1;
            }
            if (*self.xml).typ != XmlCatalogEntryType::XmlCataCatalog
                && (*self.xml).typ != XmlCatalogEntryType::XmlCataBrokenCatalog
            {
                return -1;
            }
            if (*self.xml).children.is_null() {
                return 1;
            }
            0
        } else {
            self.sgml.is_empty() as i32
        }
    }

    /// Dump the given catalog to the given file.
    #[doc(alias = "xmlACatalogDump")]
    #[cfg(feature = "libxml_output")]
    pub unsafe fn dump<'a>(&mut self, mut out: impl Write + 'a) {
        if matches!(self.typ, XmlCatalogType::XmlXMLCatalogType) {
            (*self.xml).dump_xml_catalog(out);
        } else {
            for &entry in self.sgml.values() {
                if !entry.is_null() {
                    (*entry).dump_entry(&mut out);
                }
            }
        }
    }

    /// Do a complete resolution lookup of an External Identifier.
    ///
    /// Returns the URI of the resource or null_mut() if not found
    #[doc(alias = "xmlCatalogSGMLResolve")]
    unsafe fn sgml_resolve<'a>(
        &self,
        pub_id: Option<&str>,
        sys_id: Option<&str>,
    ) -> Option<&'a Path> {
        let mut ret = None;

        if let Some(pub_id) = pub_id {
            ret = xml_catalog_get_sgml_public(&self.sgml, pub_id);
        }
        if ret.is_some() {
            return ret;
        }
        if let Some(sys_id) = sys_id {
            ret = xml_catalog_get_sgml_system(&self.sgml, sys_id);
        }
        if ret.is_some() {
            return ret;
        }
        None
    }

    /// Do a complete resolution lookup of an URI
    ///
    /// Returns the URI of the resource or null_mut() if not found,
    /// it must be freed by the caller.
    #[doc(alias = "xmlACatalogResolveURI")]
    pub unsafe fn resolve_uri(&self, uri: &str) -> *mut XmlChar {
        let mut ret: *mut XmlChar = null_mut();

        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            generic_error!("Resolve URI {uri}\n");
        }

        if matches!(self.typ, XmlCatalogType::XmlXMLCatalogType) {
            ret = (*self.xml).list_xml_resolve_uri(uri);
            if ret == XML_CATAL_BREAK {
                ret = null_mut();
            }
        } else if let Some(sgml) = self.sgml_resolve(None, Some(uri)) {
            let sgml = CString::new(sgml.to_string_lossy().as_ref()).unwrap();
            ret = xml_strdup(sgml.as_ptr() as *const u8);
        }
        ret
    }

    /// Try to lookup the catalog local reference associated to a public ID in that catalog
    ///
    /// Returns the local resource if found or null_mut() otherwise,
    /// the value returned must be freed by the caller.
    #[doc(alias = "xmlACatalogResolvePublic")]
    pub unsafe fn resolve_public(&self, pub_id: &str) -> *mut XmlChar {
        let mut ret: *mut XmlChar = null_mut();

        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            generic_error!("Resolve pubID {pub_id}\n");
        }

        if matches!(self.typ, XmlCatalogType::XmlXMLCatalogType) {
            ret = (*self.xml).list_xml_resolve(Some(pub_id), None);
            if ret == XML_CATAL_BREAK {
                ret = null_mut();
            }
        } else if let Some(sgml) = xml_catalog_get_sgml_public(&self.sgml, pub_id) {
            let sgml = CString::new(sgml.to_string_lossy().as_ref()).unwrap();
            ret = xml_strdup(sgml.as_ptr() as *const u8);
        }
        ret
    }

    /// Try to lookup the catalog resource for a system ID
    ///
    /// Returns the resource if found or null_mut() otherwise,
    /// the value returned must be freed by the caller.
    #[doc(alias = "xmlACatalogResolveSystem")]
    pub unsafe fn resolve_system(&self, sys_id: &str) -> *mut XmlChar {
        let mut ret: *mut XmlChar = null_mut();

        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            generic_error!("Resolve sysID {sys_id}\n");
        }

        if matches!(self.typ, XmlCatalogType::XmlXMLCatalogType) {
            ret = (*self.xml).list_xml_resolve(None, Some(sys_id));
            if ret == XML_CATAL_BREAK {
                ret = null_mut();
            }
        } else if let Some(sgml) = xml_catalog_get_sgml_system(&self.sgml, sys_id) {
            let sgml = CString::new(sgml.to_string_lossy().as_ref()).unwrap();
            ret = xml_strdup(sgml.as_ptr() as *const u8);
        }
        ret
    }

    /// Do a complete resolution lookup of an External Identifier
    ///
    /// Returns the URI of the resource or null_mut() if not found, it must be freed by the caller.
    #[doc(alias = "xmlACatalogResolve")]
    pub unsafe fn resolve(&self, pub_id: Option<&str>, sys_id: Option<&str>) -> *mut XmlChar {
        let mut ret: *mut XmlChar = null_mut();

        if pub_id.is_none() && sys_id.is_none() {
            return null_mut();
        }

        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            if let (Some(pub_id), Some(sys_id)) = (pub_id, sys_id) {
                generic_error!("Resolve: pubID {pub_id} sysID {sys_id}\n");
            } else if let Some(pub_id) = pub_id {
                generic_error!("Resolve: pubID {pub_id}\n");
            } else {
                generic_error!("Resolve: sysID {}\n", sys_id.unwrap());
            }
        }

        if matches!(self.typ, XmlCatalogType::XmlXMLCatalogType) {
            ret = (*self.xml).list_xml_resolve(pub_id, sys_id);
            if ret == XML_CATAL_BREAK {
                ret = null_mut();
            }
        } else if let Some(sgml) = self.sgml_resolve(pub_id, sys_id) {
            let sgml = CString::new(sgml.to_string_lossy().as_ref()).unwrap();
            ret = xml_strdup(sgml.as_ptr() as *const u8);
        }
        ret
    }

    /// Convert all the SGML catalog entries as XML ones
    ///
    /// Returns the number of entries converted if successful, -1 otherwise
    #[doc(alias = "xmlConvertSGMLCatalog")]
    pub unsafe fn convert_sgml_catalog(&mut self) -> i32 {
        if !matches!(self.typ, XmlCatalogType::XmlSGMLCatalogType) {
            return -1;
        }

        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            generic_error!("Converting SGML catalog to XML\n");
        }

        if self.xml.is_null() {
            return 0;
        }

        for (_, entry) in self.sgml.drain() {
            if entry.is_null() {
                continue;
            }
            match (*entry).typ {
                XmlCatalogEntryType::SgmlCataEntity => {
                    (*entry).typ = XmlCatalogEntryType::XmlCataPublic;
                }
                XmlCatalogEntryType::SgmlCataPentity => {
                    (*entry).typ = XmlCatalogEntryType::XmlCataPublic;
                }
                XmlCatalogEntryType::SgmlCataDoctype => {
                    (*entry).typ = XmlCatalogEntryType::XmlCataPublic;
                }
                XmlCatalogEntryType::SgmlCataLinktype => {
                    (*entry).typ = XmlCatalogEntryType::XmlCataPublic;
                }
                XmlCatalogEntryType::SgmlCataNotation => {
                    (*entry).typ = XmlCatalogEntryType::XmlCataPublic;
                }
                XmlCatalogEntryType::SgmlCataPublic => {
                    (*entry).typ = XmlCatalogEntryType::XmlCataPublic;
                }
                XmlCatalogEntryType::SgmlCataSystem => {
                    (*entry).typ = XmlCatalogEntryType::XmlCataSystem;
                }
                XmlCatalogEntryType::SgmlCataDelegate => {
                    (*entry).typ = XmlCatalogEntryType::XmlCataDelegatePublic;
                }
                XmlCatalogEntryType::SgmlCataCatalog => {
                    (*entry).typ = XmlCatalogEntryType::XmlCataCatalog;
                }
                _ => {
                    xml_free_catalog_entry(entry);
                    continue;
                }
            }
            // Conversion successful, remove from the SGML catalog
            // and add it to the default XML one
            (*entry).parent = self.xml;
            (*entry).next = null_mut();
            if (*self.xml).children.is_null() {
                (*self.xml).children = entry;
            } else {
                let mut prev: XmlCatalogEntryPtr;

                prev = (*self.xml).children;
                while !(*prev).next.is_null() {
                    prev = (*prev).next;
                }
                (*prev).next = entry;
            }
        }
        0
    }
}

impl Default for XmlCatalog {
    fn default() -> Self {
        Self {
            typ: XmlCatalogType::default(),
            sgml: HashMap::new(),
            prefer: XmlCatalogPrefer::default(),
            xml: null_mut(),
        }
    }
}

pub type XmlCatalogEntryPtr = *mut XmlCatalogEntry;
#[repr(C)]
pub struct XmlCatalogEntry {
    next: *mut XmlCatalogEntry,
    parent: *mut XmlCatalogEntry,
    children: *mut XmlCatalogEntry,
    typ: XmlCatalogEntryType,
    name: Option<String>,
    value: Option<String>,
    url: Option<PathBuf>, /* The expanded URL using the base */
    prefer: XmlCatalogPrefer,
    dealloc: i32,
    depth: i32,
    group: *mut XmlCatalogEntry,
}

impl XmlCatalogEntry {
    /// Fetch and parse the subcatalog referenced by an entry
    ///
    /// Returns 0 in case of success, -1 otherwise
    #[doc(alias = "xmlFetchXMLCatalogFile")]
    unsafe fn fetch_xml_catalog_file(&mut self) -> i32 {
        let mut doc: XmlCatalogEntryPtr;

        let Some(url) = self.url.as_deref() else {
            return -1;
        };

        // lock the whole catalog for modification
        let mutex = XML_CATALOG_MUTEX.load(Ordering::Acquire);
        xml_rmutex_lock(mutex);
        if !self.children.is_null() {
            // Okay someone else did it in the meantime
            xml_rmutex_unlock(mutex);
            return 0;
        }

        let mut catalog_files = XML_CATALOG_XMLFILES.load(Ordering::Acquire);
        if !catalog_files.is_null() {
            let curl = CString::new(url.to_string_lossy().as_ref()).unwrap();
            doc = xml_hash_lookup(catalog_files, curl.as_ptr() as *const u8) as XmlCatalogEntryPtr;
            if !doc.is_null() {
                if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                    generic_error!("Found {} in file hash\n", url.display());
                }

                if self.typ == XmlCatalogEntryType::XmlCataCatalog {
                    self.children = (*doc).children;
                } else {
                    self.children = doc;
                }
                self.dealloc = 0;
                xml_rmutex_unlock(mutex);
                return 0;
            }
            if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                generic_error!("{} not found in file hash\n", url.display());
            }
        }

        // Fetch and parse. Note that xmlParseXMLCatalogFile does not use the existing catalog,
        // there is no recursion allowed at that level.
        doc = xml_parse_xml_catalog_file(self.prefer, url.to_string_lossy().as_ref());
        if doc.is_null() {
            self.typ = XmlCatalogEntryType::XmlCataBrokenCatalog;
            xml_rmutex_unlock(mutex);
            return -1;
        }

        if matches!(self.typ, XmlCatalogEntryType::XmlCataCatalog) {
            self.children = (*doc).children;
        } else {
            self.children = doc;
        }

        (*doc).dealloc = 1;

        if catalog_files.is_null() {
            catalog_files = xml_hash_create(10);
        }
        if !catalog_files.is_null() {
            if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                generic_error!("{} added to file hash\n", url.display());
            }
            let curl = CString::new(url.to_string_lossy().as_ref()).unwrap();
            xml_hash_add_entry(catalog_files, curl.as_ptr() as *const u8, doc as _);
        }
        xml_rmutex_unlock(mutex);
        XML_CATALOG_XMLFILES.store(catalog_files, Ordering::Release);
        0
    }

    /// Do a complete resolution lookup of an URI using a
    /// document's private catalog list
    ///
    /// Returns the URI of the resource or null_mut() if not found,
    /// it must be freed by the caller.
    #[doc(alias = "xmlCatalogLocalResolveURI")]
    pub unsafe fn local_resolve_uri(&mut self, uri: &str) -> *mut XmlChar {
        if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
            xml_initialize_catalog();
        }

        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            generic_error!("Resolve URI {uri}\n");
        }

        let ret: *mut XmlChar = self.list_xml_resolve_uri(uri);
        if !ret.is_null() && ret != XML_CATAL_BREAK {
            return ret;
        }
        null_mut()
    }

    /// Do a complete resolution lookup of an External Identifier using a
    /// document's private catalog list
    ///
    /// Returns the URI of the resource or null_mut() if not found,
    /// it must be freed by the caller.
    #[doc(alias = "xmlCatalogLocalResolve")]
    pub unsafe fn local_resolve(
        &mut self,
        pub_id: Option<&str>,
        sys_id: Option<&str>,
    ) -> *mut XmlChar {
        if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
            xml_initialize_catalog();
        }

        if pub_id.is_none() && sys_id.is_none() {
            return null_mut();
        }

        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            if let (Some(pub_id), Some(sys_id)) = (pub_id, sys_id) {
                generic_error!("Local Resolve: pubID {pub_id} sysID {sys_id}\n");
            } else if let Some(pub_id) = pub_id {
                generic_error!("Local Resolve: pubID {pub_id}\n");
            } else {
                generic_error!("Local Resolve: sysID {}\n", sys_id.unwrap());
            }
        }

        let ret: *mut XmlChar = self.list_xml_resolve(pub_id, sys_id);
        if !ret.is_null() && ret != XML_CATAL_BREAK {
            return ret;
        }
        null_mut()
    }

    /// Do a complete resolution lookup of an URI for a list of catalogs
    ///
    /// Implements (or tries to) 7.2. URI Resolution
    /// from http://www.oasis-open.org/committees/entity/spec-2001-08-06.html
    ///
    /// Returns the URI of the resource or null_mut() if not found
    #[doc(alias = "xmlCatalogListXMLResolveURI")]
    unsafe fn list_xml_resolve_uri(&mut self, uri: &str) -> *mut XmlChar {
        let mut ret: *mut XmlChar = null_mut();

        if uri.starts_with(XML_URN_PUBID) {
            let urn_id = xml_catalog_unwrap_urn(uri);
            if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                if let Some(urn_id) = urn_id.as_deref() {
                    generic_error!("URN ID expanded to {urn_id}\n");
                } else {
                    generic_error!("URN ID {uri} expanded to NULL\n");
                }
            }
            if let Some(urn_id) = urn_id {
                ret = self.list_xml_resolve(Some(&urn_id), None);
            } else {
                ret = null_mut();
            }
            return ret;
        }
        let mut catal = Some(self);
        while let Some(now) = catal {
            if now.typ == XmlCatalogEntryType::XmlCataCatalog {
                if now.children.is_null() {
                    now.fetch_xml_catalog_file();
                }
                if !now.children.is_null() {
                    ret = (*now.children).xml_resolve_uri(uri);
                    if !ret.is_null() {
                        return ret;
                    }
                }
            }
            let next = now.next;
            catal = (!next.is_null()).then(|| &mut *next);
        }
        ret
    }

    /// Do a complete resolution lookup of an External Identifier for a list of catalogs
    ///
    /// Implements (or tries to) 7.1. External Identifier Resolution
    /// from http://www.oasis-open.org/committees/entity/spec-2001-08-06.html
    ///
    /// Returns the URI of the resource or null_mut() if not found
    #[doc(alias = "xmlCatalogListXMLResolve")]
    unsafe fn list_xml_resolve(
        &mut self,
        pub_id: Option<&str>,
        sys_id: Option<&str>,
    ) -> *mut XmlChar {
        let mut ret: *mut XmlChar = null_mut();

        if pub_id.is_none() && sys_id.is_none() {
            return null_mut();
        }

        let mut pub_id = pub_id.map(Cow::Borrowed);
        if let Some(normid) = pub_id
            .as_deref()
            .and_then(|pub_id| normalize_public(pub_id.as_bytes()))
        {
            if normid.is_empty() {
                pub_id = None;
            } else {
                pub_id = String::from_utf8(normid).ok().map(Cow::Owned);
            }
        }

        if let Some(pub_id) = pub_id.as_deref().filter(|p| p.starts_with(XML_URN_PUBID)) {
            let urn_id = xml_catalog_unwrap_urn(pub_id);
            if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                if let Some(urn_id) = urn_id.as_deref() {
                    generic_error!("Public URN ID expanded to {urn_id}\n");
                } else {
                    generic_error!("Public URN ID {pub_id} expanded to null_mut()\n",);
                }
            }
            if let Some(urn_id) = urn_id {
                ret = self.list_xml_resolve(Some(&urn_id), sys_id);
            } else {
                ret = null_mut();
            }
            return ret;
        }
        if let Some(sys_id) = sys_id.filter(|s| s.starts_with(XML_URN_PUBID)) {
            let urn_id = xml_catalog_unwrap_urn(sys_id);
            if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                if let Some(urn_id) = urn_id.as_deref() {
                    generic_error!("System URN ID expanded to {urn_id}\n");
                } else {
                    generic_error!("System URN ID {sys_id} expanded to null_mut()\n");
                }
            }
            if let Some(urn_id) = urn_id {
                if let Some(pub_id) = pub_id {
                    if pub_id == urn_id {
                        ret = self.list_xml_resolve(Some(&pub_id), None);
                    } else {
                        ret = self.list_xml_resolve(Some(&pub_id), Some(&urn_id));
                    }
                } else {
                    ret = self.list_xml_resolve(Some(&urn_id), None);
                }
            } else {
                ret = null_mut();
            }
            return ret;
        }
        let mut catal = Some(self);
        while let Some(now) = catal {
            if now.typ == XmlCatalogEntryType::XmlCataCatalog {
                if now.children.is_null() {
                    now.fetch_xml_catalog_file();
                }
                if !now.children.is_null() {
                    ret = (*now.children).xml_resolve(pub_id.as_deref(), sys_id);
                    if !ret.is_null() {
                        break;
                    } else if (*now.children).depth > MAX_CATAL_DEPTH as i32 {
                        ret = null_mut();
                        break;
                    }
                }
            }
            let next = now.next;
            catal = (!next.is_null()).then(|| &mut *next);
        }
        ret
    }

    /// Do a complete resolution lookup of an External Identifier for a list of catalog entries.
    ///
    /// Implements (or tries to) 7.2.2. URI Resolution
    /// from http://www.oasis-open.org/committees/entity/spec-2001-08-06.html
    ///
    /// Returns the URI of the resource or null_mut() if not found
    #[doc(alias = "xmlCatalogXMLResolveURI")]
    unsafe fn xml_resolve_uri(&mut self, uri: &str) -> *mut XmlChar {
        let mut ret: *mut XmlChar;
        let mut have_next: i32 = 0;
        let mut lenrewrite: i32 = 0;

        if self.depth > MAX_CATAL_DEPTH as i32 {
            xml_catalog_err!(
                self as *mut XmlCatalogEntry,
                null_mut(),
                XmlParserErrors::XmlCatalogRecursion,
                "Detected recursion in catalog {}\n",
                self.name.as_deref().unwrap(),
            );
            return null_mut();
        }

        // First tries steps 2/ 3/ 4/ if a system ID is provided.
        let mut cur = Some(&mut *self);
        let mut rewrite = None;
        let mut have_delegate = 0;
        while let Some(now) = cur {
            let next = now.next;
            cur = (!next.is_null()).then(|| &mut *next);
            match now.typ {
                XmlCatalogEntryType::XmlCataURI => {
                    if Some(uri) == now.name.as_deref() {
                        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                            generic_error!("Found URI match {}\n", now.name.as_deref().unwrap());
                        }
                        return if let Some(url) = now.url.as_deref() {
                            let url = CString::new(url.to_string_lossy().as_ref()).unwrap();
                            xml_strdup(url.as_ptr() as *const u8)
                        } else {
                            null_mut()
                        };
                    }
                }
                XmlCatalogEntryType::XmlCataRewriteURI => {
                    let len = now.name.as_deref().map_or(0, |n| n.len()) as i32;
                    if len > lenrewrite && uri.starts_with(now.name.as_deref().unwrap()) {
                        lenrewrite = len;
                        rewrite = Some(now);
                    }
                }
                XmlCatalogEntryType::XmlCataDelegateURI => {
                    if uri.starts_with(now.name.as_deref().unwrap()) {
                        have_delegate += 1;
                    }
                }
                XmlCatalogEntryType::XmlCataNextCatalog => {
                    have_next += 1;
                }
                _ => {}
            }
        }
        if let Some(rewrite) = rewrite {
            if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                generic_error!(
                    "Using rewriting rule {}\n",
                    rewrite.name.as_deref().unwrap()
                );
            }
            if let Some(url) = rewrite.url.as_deref() {
                let url = CString::new(url.to_string_lossy().as_ref()).unwrap();
                ret = xml_strdup(url.as_ptr() as *const u8);
            } else {
                ret = null_mut();
            }
            if !ret.is_null() {
                let uri = CString::new(&uri[lenrewrite as usize..]).unwrap();
                ret = xml_strcat(ret, uri.as_ptr() as *const u8);
            }
            return ret;
        }
        if have_delegate != 0 {
            let mut delegates: [Option<&Path>; MAX_DELEGATE] = [None; MAX_DELEGATE];
            let mut nb_list: usize = 0;

            // Assume the entries have been sorted by decreasing substring
            // matches when the list was produced.
            let mut cur = Some(&mut *self);
            'b: while let Some(now) = cur {
                if matches!(
                    now.typ,
                    XmlCatalogEntryType::XmlCataDelegateSystem
                        | XmlCatalogEntryType::XmlCataDelegateURI
                ) && uri.starts_with(now.name.as_deref().unwrap())
                {
                    for i in 0..nb_list {
                        if now.url.as_deref() == delegates[i] {
                            let next = now.next;
                            cur = (!next.is_null()).then(|| &mut *next);
                            continue 'b;
                        }
                    }
                    if now.children.is_null() {
                        now.fetch_xml_catalog_file();
                    }
                    if nb_list < MAX_DELEGATE {
                        delegates[nb_list] = now.url.as_deref();
                        nb_list += 1;
                    }

                    if !now.children.is_null() {
                        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                            generic_error!(
                                "Trying URI delegate {}\n",
                                now.url.as_deref().unwrap().display()
                            );
                        }
                        ret = (*now.children).list_xml_resolve_uri(uri);
                        if !ret.is_null() {
                            return ret;
                        }
                    }
                }
                let next = now.next;
                cur = (!next.is_null()).then(|| &mut *next);
            }
            // Apply the cut algorithm explained in 4/
            return XML_CATAL_BREAK;
        }
        if have_next != 0 {
            let mut cur = Some(&mut *self);
            while let Some(now) = cur {
                if now.typ == XmlCatalogEntryType::XmlCataNextCatalog {
                    if now.children.is_null() {
                        now.fetch_xml_catalog_file();
                    }
                    if !now.children.is_null() {
                        ret = (*now.children).list_xml_resolve_uri(uri);
                        if !ret.is_null() {
                            return ret;
                        }
                    }
                }
                let next = now.next;
                cur = (!next.is_null()).then(|| &mut *next);
            }
        }

        null_mut()
    }

    /// Do a complete resolution lookup of an External Identifier for a list of catalog entries.
    ///
    /// Implements (or tries to) 7.1. External Identifier Resolution
    /// from http://www.oasis-open.org/committees/entity/spec-2001-08-06.html
    ///
    /// Returns the URI of the resource or null_mut() if not found
    #[doc(alias = "xmlCatalogXMLResolve")]
    unsafe fn xml_resolve(&mut self, pub_id: Option<&str>, sys_id: Option<&str>) -> *mut XmlChar {
        let mut ret: *mut XmlChar;
        let mut have_delegate: i32;
        let mut have_next: i32 = 0;

        // protection against loops
        if self.depth > MAX_CATAL_DEPTH as i32 {
            xml_catalog_err!(
                self as *mut XmlCatalogEntry,
                null_mut(),
                XmlParserErrors::XmlCatalogRecursion,
                "Detected recursion in catalog {}\n",
                self.name.as_deref().unwrap(),
            );
            return null_mut();
        }
        self.depth += 1;

        // First tries steps 2/ 3/ 4/ if a system ID is provided.
        if let Some(sys_id) = sys_id {
            let mut rewrite = None;
            let mut lenrewrite = 0;
            let mut cur = Some(&mut *self);
            have_delegate = 0;
            while let Some(now) = cur {
                let next = now.next;
                cur = (!next.is_null()).then(|| &mut *next);
                match now.typ {
                    XmlCatalogEntryType::XmlCataSystem => {
                        if let Some(name) = now.name.as_deref() {
                            if Some(sys_id) == now.name.as_deref() {
                                let url = now.url.as_deref().unwrap();
                                let curl = CString::new(url.to_string_lossy().as_ref()).unwrap();
                                if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                                    generic_error!(
                                        "Found system match {name}, using {}\n",
                                        url.display()
                                    );
                                }
                                self.depth -= 1;
                                return xml_strdup(curl.as_ptr() as *const u8);
                            }
                        }
                    }
                    XmlCatalogEntryType::XmlCataRewriteSystem => {
                        let len = now.name.as_deref().map_or(0, |n| n.len());
                        if len > lenrewrite && sys_id.starts_with(now.name.as_deref().unwrap()) {
                            lenrewrite = len;
                            rewrite = Some(&mut *now);
                        }
                    }
                    XmlCatalogEntryType::XmlCataDelegateSystem => {
                        if sys_id.starts_with(now.name.as_deref().unwrap()) {
                            have_delegate += 1;
                        }
                    }
                    XmlCatalogEntryType::XmlCataNextCatalog => {
                        have_next += 1;
                    }
                    _ => {}
                }
            }
            if let Some(rewrite) = rewrite {
                if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                    generic_error!(
                        "Using rewriting rule {}\n",
                        rewrite.name.as_deref().unwrap()
                    );
                }
                if let Some(url) = rewrite.url.as_deref() {
                    let url = CString::new(url.to_string_lossy().as_ref()).unwrap();
                    ret = xml_strdup(url.as_ptr() as *const u8);
                } else {
                    ret = null_mut();
                }
                if !ret.is_null() {
                    let sys_id = CString::new(&sys_id[lenrewrite..]).unwrap();
                    ret = xml_strcat(ret, sys_id.as_ptr() as *const u8);
                }
                self.depth -= 1;
                return ret;
            }
            if have_delegate != 0 {
                let mut delegates: [Option<&Path>; MAX_DELEGATE] = [None; MAX_DELEGATE];
                let mut nb_list = 0;

                // Assume the entries have been sorted by decreasing substring
                // matches when the list was produced.
                let mut cur = Some(&mut *self);
                'b: while let Some(now) = cur {
                    if matches!(now.typ, XmlCatalogEntryType::XmlCataDelegateSystem)
                        && sys_id.starts_with(now.name.as_deref().unwrap())
                    {
                        for i in 0..nb_list {
                            if now.url.as_deref() == delegates[i] {
                                let next = now.next;
                                cur = (!next.is_null()).then(|| &mut *next);
                                continue 'b;
                            }
                        }
                        if now.children.is_null() {
                            now.fetch_xml_catalog_file();
                        }
                        if nb_list < MAX_DELEGATE {
                            delegates[nb_list] = now.url.as_deref();
                            nb_list += 1;
                        }

                        if !now.children.is_null() {
                            if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                                generic_error!(
                                    "Trying system delegate {}\n",
                                    now.url.as_deref().unwrap().display()
                                );
                            }
                            ret = (*now.children).list_xml_resolve(None, Some(sys_id));
                            if !ret.is_null() {
                                self.depth -= 1;
                                return ret;
                            }
                        }
                    }
                    let next = now.next;
                    cur = (!next.is_null()).then(|| &mut *next);
                }
                // Apply the cut algorithm explained in 4/
                self.depth -= 1;
                return XML_CATAL_BREAK;
            }
        }
        // Then tries 5/ 6/ if a public ID is provided
        if let Some(pub_id) = pub_id {
            let mut cur = Some(&mut *self);
            have_delegate = 0;
            while let Some(now) = cur {
                match now.typ {
                    XmlCatalogEntryType::XmlCataPublic => {
                        if Some(pub_id) == now.name.as_deref() {
                            if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                                generic_error!(
                                    "Found public match {}\n",
                                    now.name.as_deref().unwrap()
                                );
                            }
                            let res = if let Some(url) = now.url.as_deref() {
                                let url = CString::new(url.to_string_lossy().as_ref()).unwrap();
                                xml_strdup(url.as_ptr() as *const u8)
                            } else {
                                null_mut()
                            };
                            self.depth -= 1;
                            return res;
                        }
                    }
                    XmlCatalogEntryType::XmlCataDelegatePublic => {
                        if pub_id.starts_with(now.name.as_deref().unwrap())
                            && matches!(now.prefer, XmlCatalogPrefer::Public)
                        {
                            have_delegate += 1;
                        }
                    }
                    XmlCatalogEntryType::XmlCataNextCatalog => {
                        if sys_id.is_none() {
                            have_next += 1;
                        }
                    }
                    _ => {}
                }
                let next = now.next;
                cur = (!next.is_null()).then(|| &mut *next);
            }
            if have_delegate != 0 {
                let mut delegates: [Option<&Path>; MAX_DELEGATE] = [None; MAX_DELEGATE];
                let mut nb_list: usize = 0;

                // Assume the entries have been sorted by decreasing substring
                // matches when the list was produced.
                let mut cur = Some(&mut *self);
                'b: while let Some(now) = cur {
                    if now.typ == XmlCatalogEntryType::XmlCataDelegatePublic
                        && matches!(now.prefer, XmlCatalogPrefer::Public)
                        && pub_id.starts_with(now.name.as_deref().unwrap())
                    {
                        for i in 0..nb_list {
                            if now.url.as_deref() == delegates[i] {
                                let next = now.next;
                                cur = (!next.is_null()).then(|| &mut *next);
                                continue 'b;
                            }
                        }
                        if now.children.is_null() {
                            now.fetch_xml_catalog_file();
                        }
                        if nb_list < MAX_DELEGATE {
                            delegates[nb_list] = now.url.as_deref();
                            nb_list += 1;
                        }

                        if !now.children.is_null() {
                            if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                                generic_error!(
                                    "Trying public delegate {}\n",
                                    now.url.as_deref().unwrap().display()
                                );
                            }
                            ret = (*now.children).list_xml_resolve(Some(pub_id), None);
                            if !ret.is_null() {
                                self.depth -= 1;
                                return ret;
                            }
                        }
                    }
                    let next = now.next;
                    cur = (!next.is_null()).then(|| &mut *next);
                }
                // Apply the cut algorithm explained in 4/
                self.depth -= 1;
                return XML_CATAL_BREAK;
            }
        }
        if have_next != 0 {
            let current_depth = self.depth;
            let mut cur = Some(&mut *self);
            while let Some(now) = cur {
                if now.typ == XmlCatalogEntryType::XmlCataNextCatalog {
                    if now.children.is_null() {
                        now.fetch_xml_catalog_file();
                    }
                    if !now.children.is_null() {
                        ret = (*now.children).list_xml_resolve(pub_id, sys_id);
                        if !ret.is_null() {
                            self.depth -= 1;
                            return ret;
                        } else if current_depth > MAX_CATAL_DEPTH as i32 {
                            return null_mut();
                        }
                    }
                }
                let next = now.next;
                cur = (!next.is_null()).then(|| &mut *next);
            }
        }

        self.depth -= 1;
        null_mut()
    }

    /// Add an entry in the XML catalog, it may overwrite existing but different entries.
    ///
    /// Returns 0 if successful, -1 otherwise
    #[doc(alias = "xmlAddXMLCatalog")]
    unsafe fn add_xml_catalog(
        &mut self,
        typs: Option<&str>,
        orig: Option<&str>,
        replace: *const XmlChar,
    ) -> i32 {
        let mut cur: XmlCatalogEntryPtr;
        let mut doregister: i32 = 0;

        if !matches!(
            self.typ,
            XmlCatalogEntryType::XmlCataCatalog | XmlCatalogEntryType::XmlCataBrokenCatalog
        ) {
            return -1;
        }
        if self.children.is_null() {
            self.fetch_xml_catalog_file();
        }
        if self.children.is_null() {
            doregister = 1;
        }

        let typ: XmlCatalogEntryType = xml_get_xml_catalog_entry_type(typs);
        if matches!(typ, XmlCatalogEntryType::XmlCataNone) {
            if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                if let Some(types) = typs {
                    generic_error!("Failed to add unknown element {types} to catalog\n",);
                } else {
                    generic_error!("Failed to add unknown element (NULL) to catalog\n",);
                }
            }
            return -1;
        }

        cur = self.children;
        // Might be a simple "update in place"
        if !cur.is_null() {
            while !cur.is_null() {
                if orig.is_some() && (*cur).typ == typ && orig == (*cur).name.as_deref() {
                    if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                        if let Some(types) = typs {
                            generic_error!("Updating element {types} to catalog\n");
                        } else {
                            generic_error!("Updating element (NULL) to catalog\n",);
                        }
                    }
                    let _ = (*cur).value.take();
                    let _ = (*cur).url.take();
                    (*cur).value = Some(
                        CStr::from_ptr(replace as *const i8)
                            .to_string_lossy()
                            .into_owned(),
                    );
                    (*cur).url = Some(PathBuf::from(
                        CStr::from_ptr(replace as *const i8)
                            .to_string_lossy()
                            .into_owned(),
                    ));
                    return 0;
                }
                if (*cur).next.is_null() {
                    break;
                }
                cur = (*cur).next;
            }
        }
        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            if let Some(types) = typs {
                generic_error!("Adding element {types} to catalog\n");
            } else {
                generic_error!("Adding element (NULL) to catalog\n");
            }
        }
        if cur.is_null() {
            self.children = xml_new_catalog_entry(
                typ,
                orig,
                (!replace.is_null())
                    .then(|| CStr::from_ptr(replace as *const i8).to_string_lossy())
                    .as_deref(),
                None,
                self.prefer,
                null_mut(),
            );
        } else {
            (*cur).next = xml_new_catalog_entry(
                typ,
                orig,
                (!replace.is_null())
                    .then(|| CStr::from_ptr(replace as *const i8).to_string_lossy())
                    .as_deref(),
                None,
                self.prefer,
                null_mut(),
            );
        }
        if doregister != 0 {
            self.typ = XmlCatalogEntryType::XmlCataCatalog;
            if let Some(url) = self.url.as_deref() {
                let curl = CString::new(url.to_string_lossy().as_ref()).unwrap();
                cur = xml_hash_lookup(
                    XML_CATALOG_XMLFILES.load(Ordering::Relaxed),
                    curl.as_ptr() as *const u8,
                ) as XmlCatalogEntryPtr;
            }
            if !cur.is_null() {
                (*cur).children = self.children;
            }
        }

        0
    }

    /// Remove entries in the XML catalog where the value or the URI is equal to `value`.
    ///
    /// Returns the number of entries removed if successful, -1 otherwise
    #[doc(alias = "xmlDelXMLCatalog")]
    unsafe fn del_xml_catalog(&mut self, value: &str) -> i32 {
        let mut cur: XmlCatalogEntryPtr;
        let ret: i32 = 0;

        if !matches!(
            self.typ,
            XmlCatalogEntryType::XmlCataCatalog | XmlCatalogEntryType::XmlCataBrokenCatalog
        ) {
            return -1;
        }
        if self.children.is_null() {
            self.fetch_xml_catalog_file();
        }

        // Scan the children
        cur = self.children;
        while !cur.is_null() {
            if ((*cur).name.is_some() && Some(value) == (*cur).name.as_deref())
                || Some(value) == (*cur).value.as_deref()
            {
                if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                    if let Some(name) = (*cur).name.as_deref() {
                        generic_error!("Removing element {name} from catalog\n");
                    } else {
                        generic_error!(
                            "Removing element {} from catalog\n",
                            (*cur).value.as_deref().unwrap()
                        );
                    }
                }
                (*cur).typ = XmlCatalogEntryType::XmlCataRemoved;
            }
            cur = (*cur).next;
        }
        ret
    }

    /// Serialize an SGML Catalog entry
    #[doc(alias = "xmlCatalogDumpEntry")]
    #[cfg(feature = "libxml_output")]
    fn dump_entry<'a>(&self, out: &mut (impl Write + 'a)) {
        match self.typ {
            XmlCatalogEntryType::SgmlCataEntity => {
                write!(out, "ENTITY ").ok();
            }
            XmlCatalogEntryType::SgmlCataPentity => {
                write!(out, "ENTITY %").ok();
            }
            XmlCatalogEntryType::SgmlCataDoctype => {
                write!(out, "DOCTYPE ").ok();
            }
            XmlCatalogEntryType::SgmlCataLinktype => {
                write!(out, "LINKTYPE ").ok();
            }
            XmlCatalogEntryType::SgmlCataNotation => {
                write!(out, "NOTATION ").ok();
            }
            XmlCatalogEntryType::SgmlCataPublic => {
                write!(out, "PUBLIC ").ok();
            }
            XmlCatalogEntryType::SgmlCataSystem => {
                write!(out, "SYSTEM ").ok();
            }
            XmlCatalogEntryType::SgmlCataDelegate => {
                write!(out, "DELEGATE ").ok();
            }
            XmlCatalogEntryType::SgmlCataBase => {
                write!(out, "BASE ").ok();
            }
            XmlCatalogEntryType::SgmlCataCatalog => {
                write!(out, "CATALOG ").ok();
            }
            XmlCatalogEntryType::SgmlCataDocument => {
                write!(out, "DOCUMENT ").ok();
            }
            XmlCatalogEntryType::SgmlCataSGMLDecl => {
                write!(out, "SGMLDECL ").ok();
            }
            _ => {
                return;
            }
        }
        match self.typ {
            XmlCatalogEntryType::SgmlCataEntity
            | XmlCatalogEntryType::SgmlCataPentity
            | XmlCatalogEntryType::SgmlCataDoctype
            | XmlCatalogEntryType::SgmlCataLinktype
            | XmlCatalogEntryType::SgmlCataNotation => {
                write!(out, "{}", self.name.as_deref().unwrap()).ok();
            }
            XmlCatalogEntryType::SgmlCataPublic
            | XmlCatalogEntryType::SgmlCataSystem
            | XmlCatalogEntryType::SgmlCataSGMLDecl
            | XmlCatalogEntryType::SgmlCataDocument
            | XmlCatalogEntryType::SgmlCataCatalog
            | XmlCatalogEntryType::SgmlCataBase
            | XmlCatalogEntryType::SgmlCataDelegate => {
                write!(out, "\"{}\"", self.name.as_deref().unwrap()).ok();
            }
            _ => {}
        }
        match self.typ {
            XmlCatalogEntryType::SgmlCataEntity
            | XmlCatalogEntryType::SgmlCataPentity
            | XmlCatalogEntryType::SgmlCataDoctype
            | XmlCatalogEntryType::SgmlCataLinktype
            | XmlCatalogEntryType::SgmlCataNotation
            | XmlCatalogEntryType::SgmlCataPublic
            | XmlCatalogEntryType::SgmlCataSystem
            | XmlCatalogEntryType::SgmlCataDelegate => {
                write!(out, " \"{}\"", self.value.as_deref().unwrap()).ok();
            }
            _ => {}
        }
        writeln!(out).ok();
    }

    /// Serializes a Catalog entry, called by xmlDumpXMLCatalog and recursively for group entries
    #[doc(alias = "xmlDumpXMLCatalogNode")]
    #[cfg(feature = "libxml_output")]
    unsafe fn dump_xml_catalog_node(
        &self,
        catalog: XmlNodePtr,
        doc: XmlDocPtr,
        ns: XmlNsPtr,
        cgroup: Option<&Self>,
    ) {
        use crate::tree::NodeCommon;

        let mut node: XmlNodePtr;
        // add all the catalog entries
        let mut cur = Some(self);
        while let Some(now) = cur {
            if now.group == cgroup.map_or(null_mut(), |g| g as *const Self as *mut Self) {
                match now.typ {
                    XmlCatalogEntryType::XmlCataRemoved => {}
                    XmlCatalogEntryType::XmlCataBrokenCatalog
                    | XmlCatalogEntryType::XmlCataCatalog => {
                        if std::ptr::eq(now, self) {
                            let children = now.children;
                            cur = (!children.is_null()).then(|| &*children);
                            continue;
                        }
                    }
                    XmlCatalogEntryType::XmlCataNextCatalog => {
                        node = xml_new_doc_node(doc, ns, c"nextCatalog".as_ptr() as _, null_mut());
                        (*node).set_prop("catalog", now.value.as_deref());
                        (*catalog).add_child(node);
                    }
                    XmlCatalogEntryType::XmlCataNone => {}
                    XmlCatalogEntryType::XmlCataGroup => {
                        node = xml_new_doc_node(doc, ns, c"group".as_ptr() as _, null_mut());
                        (*node).set_prop("id", now.name.as_deref());
                        if let Some(value) = now.value.as_deref() {
                            let xns: XmlNsPtr =
                                (*node).search_ns_by_href(doc, XML_XML_NAMESPACE.to_str().unwrap());
                            if !xns.is_null() {
                                (*node).set_ns_prop(xns, "base", Some(value));
                            }
                        }
                        match now.prefer {
                            XmlCatalogPrefer::None => {}
                            XmlCatalogPrefer::Public => {
                                (*node).set_prop("prefer", Some("public"));
                            }
                            XmlCatalogPrefer::System => {
                                (*node).set_prop("prefer", Some("system"));
                            }
                        }
                        (*now.next).dump_xml_catalog_node(node, doc, ns, Some(now));
                        (*catalog).add_child(node);
                    }
                    XmlCatalogEntryType::XmlCataPublic => {
                        node = xml_new_doc_node(doc, ns, c"public".as_ptr() as _, null_mut());
                        (*node).set_prop("publicId", now.name.as_deref());
                        (*node).set_prop("uri", now.value.as_deref());
                        (*catalog).add_child(node);
                    }
                    XmlCatalogEntryType::XmlCataSystem => {
                        node = xml_new_doc_node(doc, ns, c"system".as_ptr() as _, null_mut());
                        (*node).set_prop("systemId", now.name.as_deref());
                        (*node).set_prop("uri", now.value.as_deref());
                        (*catalog).add_child(node);
                    }
                    XmlCatalogEntryType::XmlCataRewriteSystem => {
                        node =
                            xml_new_doc_node(doc, ns, c"rewriteSystem".as_ptr() as _, null_mut());
                        (*node).set_prop("systemIdStartString", now.name.as_deref());
                        (*node).set_prop("rewritePrefix", now.value.as_deref());
                        (*catalog).add_child(node);
                    }
                    XmlCatalogEntryType::XmlCataDelegatePublic => {
                        node =
                            xml_new_doc_node(doc, ns, c"delegatePublic".as_ptr() as _, null_mut());
                        (*node).set_prop("publicIdStartString", now.name.as_deref());
                        (*node).set_prop("catalog", now.value.as_deref());
                        (*catalog).add_child(node);
                    }
                    XmlCatalogEntryType::XmlCataDelegateSystem => {
                        node =
                            xml_new_doc_node(doc, ns, c"delegateSystem".as_ptr() as _, null_mut());
                        (*node).set_prop("systemIdStartString", now.name.as_deref());
                        (*node).set_prop("catalog", now.value.as_deref());
                        (*catalog).add_child(node);
                    }
                    XmlCatalogEntryType::XmlCataURI => {
                        node = xml_new_doc_node(doc, ns, c"uri".as_ptr() as _, null_mut());
                        (*node).set_prop("name", now.name.as_deref());
                        (*node).set_prop("uri", now.value.as_deref());
                        (*catalog).add_child(node);
                    }
                    XmlCatalogEntryType::XmlCataRewriteURI => {
                        node = xml_new_doc_node(doc, ns, c"rewriteURI".as_ptr() as _, null_mut());
                        (*node).set_prop("uriStartString", now.name.as_deref());
                        (*node).set_prop("rewritePrefix", now.value.as_deref());
                        (*catalog).add_child(node);
                    }
                    XmlCatalogEntryType::XmlCataDelegateURI => {
                        node = xml_new_doc_node(doc, ns, c"delegateURI".as_ptr() as _, null_mut());
                        (*node).set_prop("uriStartString", now.name.as_deref());
                        (*node).set_prop("catalog", now.value.as_deref());
                        (*catalog).add_child(node);
                    }
                    XmlCatalogEntryType::SgmlCataSystem
                    | XmlCatalogEntryType::SgmlCataPublic
                    | XmlCatalogEntryType::SgmlCataEntity
                    | XmlCatalogEntryType::SgmlCataPentity
                    | XmlCatalogEntryType::SgmlCataDoctype
                    | XmlCatalogEntryType::SgmlCataLinktype
                    | XmlCatalogEntryType::SgmlCataNotation
                    | XmlCatalogEntryType::SgmlCataDelegate
                    | XmlCatalogEntryType::SgmlCataBase
                    | XmlCatalogEntryType::SgmlCataCatalog
                    | XmlCatalogEntryType::SgmlCataDocument
                    | XmlCatalogEntryType::SgmlCataSGMLDecl => {}
                }
            }
            let next = now.next;
            cur = (!next.is_null()).then(|| &*next);
        }
    }

    #[doc(alias = "xmlDumpXMLCatalog")]
    #[cfg(feature = "libxml_output")]
    unsafe fn dump_xml_catalog<'a>(&self, out: impl Write + 'a) -> i32 {
        use crate::{io::XmlOutputBuffer, tree::NodeCommon};

        // Rebuild a catalog
        let doc: XmlDocPtr = xml_new_doc(None);
        if doc.is_null() {
            return -1;
        }
        let dtd: XmlDtdPtr = xml_new_dtd(
            doc,
            c"catalog".as_ptr() as _,
            Some("-//OASIS//DTD Entity Resolution XML Catalog V1.0//EN"),
            Some("http://www.oasis-open.org/committees/entity/release/1.0/catalog.dtd"),
        );

        (*doc).add_child(dtd as _);

        let ns: XmlNsPtr = xml_new_ns(null_mut(), XML_CATALOGS_NAMESPACE.as_ptr() as _, null_mut());
        if ns.is_null() {
            xml_free_doc(doc);
            return -1;
        }
        let catalog: XmlNodePtr = xml_new_doc_node(doc, ns, c"catalog".as_ptr() as _, null_mut());
        if catalog.is_null() {
            xml_free_ns(ns);
            xml_free_doc(doc);
            return -1;
        }
        (*catalog).ns_def = ns;
        (*doc).add_child(catalog as _);

        self.dump_xml_catalog_node(catalog, doc, ns, None);

        // reserialize it
        let Some(buf) = XmlOutputBuffer::from_writer(out, None) else {
            xml_free_doc(doc);
            return -1;
        };
        let ret: i32 = (*doc).save_format_file_to(buf, None, 1);

        // Free it
        xml_free_doc(doc);

        ret
    }
}

impl Default for XmlCatalogEntry {
    fn default() -> Self {
        Self {
            next: null_mut(),
            parent: null_mut(),
            children: null_mut(),
            typ: XmlCatalogEntryType::default(),
            name: None,
            value: None,
            url: None,
            prefer: XmlCatalogPrefer::default(),
            dealloc: 0,
            depth: 0,
            group: null_mut(),
        }
    }
}

// Those are preferences
static XML_DEBUG_CATALOGS: AtomicI32 = AtomicI32::new(0); /* used for debugging */
static mut XML_CATALOG_DEFAULT_ALLOW: XmlCatalogAllow = XmlCatalogAllow::All;
static mut XML_CATALOG_DEFAULT_PREFER: XmlCatalogPrefer = XmlCatalogPrefer::Public;

/// create a new Catalog, this type is shared both by XML and SGML catalogs,
/// but the acceptable types values differs.
///
/// Returns the xmlCatalogPtr or null_mut() in case of error
#[doc(alias = "xmlCreateNewCatalog")]
unsafe fn xml_create_new_catalog(typ: XmlCatalogType, prefer: XmlCatalogPrefer) -> XmlCatalogPtr {
    let ret: XmlCatalogPtr = xml_malloc(size_of::<XmlCatalog>()) as XmlCatalogPtr;
    if ret.is_null() {
        xml_catalog_err_memory("allocating catalog");
        return null_mut();
    }
    std::ptr::write(&mut *ret, XmlCatalog::default());
    (*ret).typ = typ;
    (*ret).prefer = prefer;
    if matches!((*ret).typ, XmlCatalogType::XmlSGMLCatalogType) {
        (*ret).sgml = HashMap::new();
    }
    ret
}

/// create a new Catalog.
///
/// Returns the xmlCatalogPtr or null_mut() in case of error
#[doc(alias = "xmlNewCatalog")]
pub unsafe fn xml_new_catalog(sgml: bool) -> XmlCatalogPtr {
    if sgml {
        xml_create_new_catalog(
            XmlCatalogType::XmlSGMLCatalogType,
            XML_CATALOG_DEFAULT_PREFER,
        )
    } else {
        xml_create_new_catalog(
            XmlCatalogType::XmlXMLCatalogType,
            XML_CATALOG_DEFAULT_PREFER,
        )
    }
}

/// Load a file content into memory.
///
/// Return the content buffer.  
/// If it is failed to get contents, return `None`.
#[doc(alias = "xmlLoadFileContent")]
fn xml_load_file_content(filename: impl AsRef<Path>) -> Option<Vec<u8>> {
    fn _load_file_content(filename: &Path) -> Option<Vec<u8>> {
        let mut file = File::open(filename).ok()?;
        let mut buf = vec![];
        file.read_to_end(&mut buf).ok()?;
        Some(buf)
    }
    _load_file_content(filename.as_ref())
}

/// Trim blank chars at the head of `cur`.
fn skip_blanks(mut cur: &[u8]) -> &[u8] {
    while !cur.is_empty() && xml_is_blank_char(cur[0] as u32) {
        cur = &cur[1..];
    }
    cur
}

/// Skip a comment in an SGML catalog
///
/// Returns new current character
#[doc(alias = "xmlParseSGMLCatalogComment")]
fn xml_parse_sgmlcatalog_comment(mut cur: &[u8]) -> Option<&[u8]> {
    let Some(rem) = cur.strip_prefix(b"--") else {
        return Some(cur);
    };
    cur = rem;
    while !cur.is_empty() && !cur.starts_with(b"--") {
        cur = &cur[1..];
    }
    if cur.is_empty() {
        None
    } else {
        Some(&cur[2..])
    }
}

/// Parse an SGML catalog name
///
/// If parsed successfully, return `Some((parsed_name, remained buffer))`.  
/// Otherwise, return `None`.
#[doc(alias = "xmlParseSGMLCatalogName")]
fn xml_parse_sgml_catalog_name(cur: &[u8]) -> Option<(&[u8], &[u8])> {
    // Handler for more complex cases
    cur.first()
        .filter(|&&b| xml_is_letter(b as u32) || b == b'_' || b == b':')?;

    let len = cur
        .iter()
        .take_while(|&&b| {
            xml_is_letter(b as u32)
                || b.is_ascii_digit()
                || b == b'.'
                || b == b'-'
                || b == b'_'
                || b == b':'
        })
        .count();
    if len >= XML_MAX_NAMELEN {
        return None;
    }
    Some(cur.split_at(len))
}

/// Parse an SGML catalog ID
///
/// If parsed successfully, return `Some((parsed_pubid, remained buffer))`.  
/// Otherwise, return `None`.
#[doc(alias = "xmlParseSGMLCatalogPubid")]
fn xml_parse_sgml_catalog_pubid(mut cur: &[u8]) -> Option<(&[u8], &[u8])> {
    let stop = if let Some(rem) = cur.strip_prefix(b"\"") {
        cur = rem;
        b'"'
    } else if let Some(rem) = cur.strip_prefix(b"'") {
        cur = rem;
        b'\''
    } else {
        b' '
    };
    let mut len = 0;
    let orig = cur;
    while let Some(&now) = cur
        .first()
        .filter(|&&b| xml_is_pubid_char(b as u32) || b == b'?')
    {
        if now == stop && stop != b' ' {
            break;
        }
        if stop == b' ' && xml_is_blank_char(now as u32) {
            break;
        }
        len += 1;
        cur = &cur[1..];
    }
    let id = &orig[..len];
    if stop == b' ' {
        cur.first().filter(|&&b| xml_is_blank_char(b as u32))?;
    } else {
        if cur.first() != Some(&stop) {
            return None;
        }
        cur = &cur[1..];
    }
    Some((id, cur))
}

/// Normalizes the Public Identifier
///
/// Implements 6.2. Public Identifier Normalization
/// from http://www.oasis-open.org/committees/entity/spec-2001-08-06.html
///
/// If performed normalization, return normalized bytes wrapped `Some`.  
/// Otherwise, return `None`.
#[doc(alias = "xmlCatalogNormalizePublic")]
fn normalize_public(pub_id: &[u8]) -> Option<Vec<u8>> {
    let mut ok = true;
    let mut white = true;
    let mut p = pub_id;
    while !p.is_empty() && ok {
        if !xml_is_blank_char(p[0] as u32) {
            white = false;
        } else if p[0] == 0x20 && !white {
            white = true;
        } else {
            ok = false;
        }
        p = &p[1..];
    }
    if ok && !white {
        // is normalized
        return None;
    }

    let mut ret = Vec::with_capacity(pub_id.len());
    let mut white = false;
    let mut p = pub_id;
    while !p.is_empty() {
        if xml_is_blank_char(p[0] as u32) {
            if !ret.is_empty() {
                white = true;
            }
        } else {
            if white {
                ret.push(0x20);
                white = false;
            }
            ret.push(p[0]);
        }
        p = &p[1..];
    }
    Some(ret)
}

/// create a new Catalog entry, this type is shared both by XML and SGML catalogs,
/// but the acceptable types values differs.
///
/// Returns the xmlCatalogEntryPtr or null_mut() in case of error
#[doc(alias = "xmlNewCatalogEntry")]
unsafe fn xml_new_catalog_entry(
    typ: XmlCatalogEntryType,
    name: Option<&str>,
    value: Option<&str>,
    mut url: Option<PathBuf>,
    prefer: XmlCatalogPrefer,
    group: XmlCatalogEntryPtr,
) -> XmlCatalogEntryPtr {
    let ret: XmlCatalogEntryPtr = xml_malloc(size_of::<XmlCatalogEntry>()) as XmlCatalogEntryPtr;
    if ret.is_null() {
        xml_catalog_err_memory("allocating catalog entry");
        return null_mut();
    }
    std::ptr::write(&mut *ret, XmlCatalogEntry::default());
    (*ret).next = null_mut();
    (*ret).parent = null_mut();
    (*ret).children = null_mut();
    (*ret).typ = typ;
    let mut name = name.map(Cow::Borrowed);
    if matches!(
        typ,
        XmlCatalogEntryType::XmlCataPublic | XmlCatalogEntryType::XmlCataDelegatePublic
    ) {
        if let Some(n) = name.as_deref() {
            if let Some(normid) = normalize_public(n.as_bytes()) {
                if normid.is_empty() {
                    name = None;
                } else {
                    name = String::from_utf8(normid).ok().map(Cow::Owned);
                }
            }
        }
    }
    if let Some(name) = name {
        (*ret).name = Some(name.into_owned());
    }
    (*ret).value = value.map(|v| v.to_owned());
    if url.is_none() {
        url = value.map(PathBuf::from);
    }
    (*ret).url = url;
    (*ret).prefer = prefer;
    (*ret).dealloc = 0;
    (*ret).depth = 0;
    (*ret).group = group;
    ret
}

/// Free the memory allocated to a Catalog entry
#[doc(alias = "xmlFreeCatalogEntry")]
unsafe fn xml_free_catalog_entry(payload: XmlCatalogEntryPtr) {
    let ret: XmlCatalogEntryPtr = payload as XmlCatalogEntryPtr;
    if ret.is_null() {
        return;
    }
    // Entries stored in the file hash must be deallocated only by the file hash cleaner !
    if (*ret).dealloc == 1 {
        return;
    }

    if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
        if let Some(name) = (*ret).name.take() {
            generic_error!("Free catalog entry {name}\n");
        } else if let Some(value) = (*ret).value.as_deref() {
            generic_error!("Free catalog entry {value}\n");
        } else {
            generic_error!("Free catalog entry\n");
        }
    }

    let _ = (*ret).value.take();
    let _ = (*ret).url.take();
    xml_free(ret as _);
}

/// Load the catalog and build the associated data structures.  
/// This can be either an XML Catalog or an SGML Catalog.  
/// It will recurse in SGML CATALOG entries.  
/// On the other hand XML Catalogs are not handled recursively.
///
/// Returns the catalog parsed or null_mut() in case of error
#[doc(alias = "xmlLoadACatalog")]
pub unsafe fn xml_load_a_catalog(filename: impl AsRef<Path>) -> XmlCatalogPtr {
    let filename = filename.as_ref();
    let Some(content) = xml_load_file_content(filename) else {
        return null_mut();
    };

    let mut first = &content[..];

    while !first.is_empty()
        && first[0] != b'-'
        && first[0] != b'<'
        && !((first[0] >= b'A' && first[0] <= b'Z') || (first[0] >= b'a' && first[0] <= b'z'))
    {
        first = &first[1..];
    }

    if first.first() != Some(&b'<') {
        let catal = xml_create_new_catalog(
            XmlCatalogType::XmlSGMLCatalogType,
            XML_CATALOG_DEFAULT_PREFER,
        );
        if catal.is_null() {
            return null_mut();
        }
        let ret = (*catal).parse_sgml_catalog(&content, filename, 0);
        if ret < 0 {
            xml_free_catalog(catal);
            return null_mut();
        }
        catal
    } else {
        let catal = xml_create_new_catalog(
            XmlCatalogType::XmlXMLCatalogType,
            XML_CATALOG_DEFAULT_PREFER,
        );
        if catal.is_null() {
            return null_mut();
        }
        (*catal).xml = xml_new_catalog_entry(
            XmlCatalogEntryType::XmlCataCatalog,
            None,
            None,
            Some(filename.to_owned()),
            XML_CATALOG_DEFAULT_PREFER,
            null_mut(),
        );
        catal
    }
}

/// Load an SGML super catalog. It won't expand CATALOG or DELEGATE references.  
/// This is only needed for manipulating SGML Super Catalogs
/// like adding and removing CATALOG or DELEGATE entries.
///
/// Returns the catalog parsed or null_mut() in case of error
#[doc(alias = "xmlLoadSGMLSuperCatalog")]
pub unsafe fn xml_load_sgml_super_catalog(filename: impl AsRef<Path>) -> XmlCatalogPtr {
    let filename = filename.as_ref();
    let Some(content) = xml_load_file_content(filename) else {
        return null_mut();
    };

    let catal: XmlCatalogPtr = xml_create_new_catalog(
        XmlCatalogType::XmlSGMLCatalogType,
        XML_CATALOG_DEFAULT_PREFER,
    );
    if catal.is_null() {
        return null_mut();
    }

    let ret = (*catal).parse_sgml_catalog(&content, filename, 1);
    if ret < 0 {
        xml_free_catalog(catal);
        return null_mut();
    }
    catal
}

// Hash table containing all the trees of XML catalogs parsed by the application.
static XML_CATALOG_XMLFILES: AtomicPtr<XmlHashTable<'static, CVoidWrapper>> =
    AtomicPtr::new(null_mut());

// A mutex for modifying the shared global catalog(s) xmlDefaultCatalog tree.
// It also protects xmlCatalogXMLFiles.
// The core of this readers/writer scheme is in `xmlFetchXMLCatalogFile()`.
static XML_CATALOG_MUTEX: AtomicPtr<XmlRMutex> = AtomicPtr::new(null_mut());

/// Finishes the examination of an XML tree node of a catalog and build
/// a Catalog entry from it.
///
/// Returns the new Catalog entry node or null_mut() in case of error.
#[doc(alias = "xmlParseXMLCatalogOneNode")]
unsafe fn xml_parse_xml_catalog_one_node(
    cur: XmlNodePtr,
    typ: XmlCatalogEntryType,
    name: &str,
    attr_name: Option<&str>,
    uri_attr_name: &str,
    prefer: XmlCatalogPrefer,
    cgroup: XmlCatalogEntryPtr,
) -> XmlCatalogEntryPtr {
    let mut ok = true;
    let mut name_value = None;

    let mut ret: XmlCatalogEntryPtr = null_mut();

    if let Some(attr_name) = attr_name {
        name_value = (*cur).get_prop(attr_name);
        if name_value.is_none() {
            xml_catalog_err!(
                ret,
                cur,
                XmlParserErrors::XmlCatalogMissingAttr,
                "{} entry lacks '{}'\n",
                name,
                attr_name,
            );
            ok = false;
        }
    }
    let Some(uri_value) = (*cur).get_prop(uri_attr_name) else {
        xml_catalog_err!(
            ret,
            cur,
            XmlParserErrors::XmlCatalogMissingAttr,
            "{} entry lacks '{}'\n",
            name,
            uri_attr_name,
        );
        return null_mut();
    };
    if !ok {
        return null_mut();
    }

    if let Some(url) = (*cur)
        .get_base((*cur).doc)
        .and_then(|base| build_uri(&uri_value, &base))
    {
        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) > 1 {
            if let Some(name_value) = name_value.as_deref() {
                generic_error!("Found {}: '{}' '{}'\n", name, name_value, url);
            } else {
                generic_error!("Found {}: '{}'\n", name, url);
            }
        }
        ret = xml_new_catalog_entry(
            typ,
            name_value.as_deref(),
            Some(&uri_value),
            Some(PathBuf::from(url)),
            prefer,
            cgroup,
        );
    } else {
        xml_catalog_err!(
            ret,
            cur,
            XmlParserErrors::XmlCatalogEntryBroken,
            "{} entry '{}' broken ?: {}\n",
            name,
            uri_attr_name,
            uri_value,
        );
    }
    ret
}

/// Examines an XML tree node of a catalog and build a Catalog entry from it adding it to its parent.
/// The examination can be recursive.
#[doc(alias = "xmlParseXMLCatalogNode")]
unsafe fn xml_parse_xml_catalog_node(
    cur: XmlNodePtr,
    mut prefer: XmlCatalogPrefer,
    parent: XmlCatalogEntryPtr,
    cgroup: XmlCatalogEntryPtr,
) {
    let mut entry: XmlCatalogEntryPtr = null_mut();

    if cur.is_null() {
        return;
    }
    if (*cur).name().as_deref() == Some("group") {
        let mut pref: XmlCatalogPrefer = XmlCatalogPrefer::None;

        if let Some(prop) = (*cur).get_prop("prefer") {
            if prop == "system" {
                prefer = XmlCatalogPrefer::System;
            } else if prop == "public" {
                prefer = XmlCatalogPrefer::Public;
            } else {
                xml_catalog_err!(
                    parent,
                    cur,
                    XmlParserErrors::XmlCatalogPreferValue,
                    "Invalid value for prefer: '{}'\n",
                    prop,
                );
            }
            pref = prefer;
        }
        let prop = (*cur).get_prop("id");
        let base = (*cur).get_ns_prop("base", XML_XML_NAMESPACE.to_str().ok());
        entry = xml_new_catalog_entry(
            XmlCatalogEntryType::XmlCataGroup,
            prop.as_deref(),
            base.as_deref(),
            None,
            pref,
            cgroup,
        );
    } else if (*cur).name().as_deref() == Some("public") {
        entry = xml_parse_xml_catalog_one_node(
            cur,
            XmlCatalogEntryType::XmlCataPublic,
            "public",
            Some("publicId"),
            "uri",
            prefer,
            cgroup,
        );
    } else if (*cur).name().as_deref() == Some("system") {
        entry = xml_parse_xml_catalog_one_node(
            cur,
            XmlCatalogEntryType::XmlCataSystem,
            "system",
            Some("systemId"),
            "uri",
            prefer,
            cgroup,
        );
    } else if (*cur).name().as_deref() == Some("rewriteSystem") {
        entry = xml_parse_xml_catalog_one_node(
            cur,
            XmlCatalogEntryType::XmlCataRewriteSystem,
            "rewriteSystem",
            Some("systemIdStartString"),
            "rewritePrefix",
            prefer,
            cgroup,
        );
    } else if (*cur).name().as_deref() == Some("delegatePublic") {
        entry = xml_parse_xml_catalog_one_node(
            cur,
            XmlCatalogEntryType::XmlCataDelegatePublic,
            "delegatePublic",
            Some("publicIdStartString"),
            "catalog",
            prefer,
            cgroup,
        );
    } else if (*cur).name().as_deref() == Some("delegateSystem") {
        entry = xml_parse_xml_catalog_one_node(
            cur,
            XmlCatalogEntryType::XmlCataDelegateSystem,
            "delegateSystem",
            Some("systemIdStartString"),
            "catalog",
            prefer,
            cgroup,
        );
    } else if (*cur).name().as_deref() == Some("uri") {
        entry = xml_parse_xml_catalog_one_node(
            cur,
            XmlCatalogEntryType::XmlCataURI,
            "uri",
            Some("name"),
            "uri",
            prefer,
            cgroup,
        );
    } else if (*cur).name().as_deref() == Some("rewriteURI") {
        entry = xml_parse_xml_catalog_one_node(
            cur,
            XmlCatalogEntryType::XmlCataRewriteURI,
            "rewriteURI",
            Some("uriStartString"),
            "rewritePrefix",
            prefer,
            cgroup,
        );
    } else if (*cur).name().as_deref() == Some("delegateURI") {
        entry = xml_parse_xml_catalog_one_node(
            cur,
            XmlCatalogEntryType::XmlCataDelegateURI,
            "delegateURI",
            Some("uriStartString"),
            "catalog",
            prefer,
            cgroup,
        );
    } else if (*cur).name().as_deref() == Some("nextCatalog") {
        entry = xml_parse_xml_catalog_one_node(
            cur,
            XmlCatalogEntryType::XmlCataNextCatalog,
            "nextCatalog",
            None,
            "catalog",
            prefer,
            cgroup,
        );
    }
    if !entry.is_null() {
        if !parent.is_null() {
            (*entry).parent = parent;
            if (*parent).children.is_null() {
                (*parent).children = entry;
            } else {
                let mut prev: XmlCatalogEntryPtr;

                prev = (*parent).children;
                while !(*prev).next.is_null() {
                    prev = (*prev).next;
                }
                (*prev).next = entry;
            }
        }
        if (*entry).typ == XmlCatalogEntryType::XmlCataGroup {
            // Recurse to propagate prefer to the subtree
            // (xml:base handling is automated)
            xml_parse_xml_catalog_node_list(
                (*cur).children().map_or(null_mut(), |c| c.as_ptr()),
                prefer,
                parent,
                entry,
            );
        }
    }
}

/// Examines a list of XML sibling nodes of a catalog and build
/// a list of Catalog entry from it adding it to the parent.
/// The examination will recurse to examine node subtrees.
#[doc(alias = "xmlParseXMLCatalogNodeList")]
unsafe fn xml_parse_xml_catalog_node_list(
    mut cur: XmlNodePtr,
    prefer: XmlCatalogPrefer,
    parent: XmlCatalogEntryPtr,
    cgroup: XmlCatalogEntryPtr,
) {
    while !cur.is_null() {
        if !(*cur).ns.is_null()
            && !(*(*cur).ns).href.is_null()
            && xml_str_equal((*(*cur).ns).href, XML_CATALOGS_NAMESPACE.as_ptr() as _)
        {
            xml_parse_xml_catalog_node(cur, prefer, parent, cgroup);
        }
        cur = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
    }
    /* TODO: sort the list according to REWRITE lengths and prefer value */
}

/// Parses the catalog file to extract the XML tree and then analyze the
/// tree to build a list of Catalog entries corresponding to this catalog
///
/// Returns the resulting Catalog entries list
#[doc(alias = "xmlParseXMLCatalogFile")]
unsafe fn xml_parse_xml_catalog_file(
    mut prefer: XmlCatalogPrefer,
    filename: &str,
) -> XmlCatalogEntryPtr {
    let mut cur: XmlNodePtr;
    let parent: XmlCatalogEntryPtr;

    let doc: XmlDocPtr = xml_parse_catalog_file(filename);
    if doc.is_null() {
        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            generic_error!("Failed to parse catalog {filename}\n");
        }
        return null_mut();
    }

    if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
        generic_error!("{} Parsing catalog {filename}\n", xml_get_thread_id());
    }

    cur = (*doc).get_root_element();
    if !cur.is_null()
        && xml_str_equal((*cur).name, c"catalog".as_ptr() as _)
        && !(*cur).ns.is_null()
        && !(*(*cur).ns).href.is_null()
        && xml_str_equal((*(*cur).ns).href, XML_CATALOGS_NAMESPACE.as_ptr() as _)
    {
        parent = xml_new_catalog_entry(
            XmlCatalogEntryType::XmlCataCatalog,
            None,
            Some(filename),
            None,
            prefer,
            null_mut(),
        );
        if parent.is_null() {
            xml_free_doc(doc);
            return null_mut();
        }

        if let Some(prop) = (*cur).get_prop("prefer") {
            if prop == "system" {
                prefer = XmlCatalogPrefer::System;
            } else if prop == "public" {
                prefer = XmlCatalogPrefer::Public;
            } else {
                xml_catalog_err!(
                    null_mut(),
                    cur,
                    XmlParserErrors::XmlCatalogPreferValue,
                    "Invalid value for prefer: '{}'\n",
                    prop,
                );
            }
        }
        cur = (*cur).children().map_or(null_mut(), |c| c.as_ptr());
        xml_parse_xml_catalog_node_list(cur, prefer, parent, null_mut());
    } else {
        xml_catalog_err!(
            null_mut(),
            doc,
            XmlParserErrors::XmlCatalogNotCatalog,
            "File {} is not an XML Catalog\n",
            filename,
        );
        xml_free_doc(doc);
        return null_mut();
    }
    xml_free_doc(doc);
    parent
}

/// lookup the internal type associated to an XML catalog entry name
///
/// Returns the type associated with that name
#[doc(alias = "xmlGetXMLCatalogEntryType")]
fn xml_get_xml_catalog_entry_type(name: Option<&str>) -> XmlCatalogEntryType {
    let mut typ: XmlCatalogEntryType = XmlCatalogEntryType::XmlCataNone;
    if name == Some("system") {
        typ = XmlCatalogEntryType::XmlCataSystem;
    } else if name == Some("public") {
        typ = XmlCatalogEntryType::XmlCataPublic;
    } else if name == Some("rewriteSystem") {
        typ = XmlCatalogEntryType::XmlCataRewriteSystem;
    } else if name == Some("delegatePublic") {
        typ = XmlCatalogEntryType::XmlCataDelegatePublic;
    } else if name == Some("delegateSystem") {
        typ = XmlCatalogEntryType::XmlCataDelegateSystem;
    } else if name == Some("uri") {
        typ = XmlCatalogEntryType::XmlCataURI;
    } else if name == Some("rewriteURI") {
        typ = XmlCatalogEntryType::XmlCataRewriteURI;
    } else if name == Some("delegateURI") {
        typ = XmlCatalogEntryType::XmlCataDelegateURI;
    } else if name == Some("nextCatalog") {
        typ = XmlCatalogEntryType::XmlCataNextCatalog;
    } else if name == Some("catalog") {
        typ = XmlCatalogEntryType::XmlCataCatalog;
    }
    typ
}

/// Get the Catalog entry type for a given SGML Catalog name
///
/// Returns Catalog entry type
#[doc(alias = "xmlGetSGMLCatalogEntryType")]
fn xml_get_sgml_catalog_entry_type(name: Option<&str>) -> XmlCatalogEntryType {
    let mut typ: XmlCatalogEntryType = XmlCatalogEntryType::XmlCataNone;
    if name == Some("SYSTEM") {
        typ = XmlCatalogEntryType::SgmlCataSystem;
    } else if name == Some("PUBLIC") {
        typ = XmlCatalogEntryType::SgmlCataPublic;
    } else if name == Some("DELEGATE") {
        typ = XmlCatalogEntryType::SgmlCataDelegate;
    } else if name == Some("ENTITY") {
        typ = XmlCatalogEntryType::SgmlCataEntity;
    } else if name == Some("DOCTYPE") {
        typ = XmlCatalogEntryType::SgmlCataDoctype;
    } else if name == Some("LINKTYPE") {
        typ = XmlCatalogEntryType::SgmlCataLinktype;
    } else if name == Some("NOTATION") {
        typ = XmlCatalogEntryType::SgmlCataNotation;
    } else if name == Some("SGMLDECL") {
        typ = XmlCatalogEntryType::SgmlCataSGMLDecl;
    } else if name == Some("DOCUMENT") {
        typ = XmlCatalogEntryType::SgmlCataDocument;
    } else if name == Some("CATALOG") {
        typ = XmlCatalogEntryType::SgmlCataCatalog;
    } else if name == Some("BASE") {
        typ = XmlCatalogEntryType::SgmlCataBase;
    }
    typ
}

const XML_CATAL_BREAK: *mut XmlChar = usize::MAX as *mut XmlChar;
const XML_URN_PUBID: &str = "urn:publicid:";
const MAX_DELEGATE: usize = 50;
const MAX_CATAL_DEPTH: usize = 50;

/// Expand the URN into the equivalent Public Identifier
///
/// Returns the new identifier or null_mut(), the string must be deallocated by the caller.
#[doc(alias = "xmlCatalogUnWrapURN")]
fn xml_catalog_unwrap_urn(urn: &str) -> Option<String> {
    let urn = urn.strip_prefix(XML_URN_PUBID)?;
    let mut urn = urn.as_bytes();
    let mut res = Vec::with_capacity(urn.len());
    while !urn.is_empty() {
        if urn[0] == b'+' {
            res.push(b' ');
            urn = &urn[1..];
        } else if urn[0] == b':' {
            res.extend_from_slice(b"//");
            urn = &urn[1..];
        } else if urn[0] == b';' {
            res.extend_from_slice(b"::");
            urn = &urn[1..];
        } else if urn[0] == b'%' {
            match urn[1..] {
                [b'2', b'B', ..] => res.push(b'+'),
                [b'3', b'A', ..] => res.push(b':'),
                [b'2', b'F', ..] => res.push(b'/'),
                [b'3', b'B', ..] => res.push(b';'),
                [b'2', b'7', ..] => res.push(b'\''),
                [b'3', b'F', ..] => res.push(b'?'),
                [b'2', b'3', ..] => res.push(b'#'),
                [b'2', b'5', ..] => res.push(b'%'),
                _ => {
                    res.push(urn[0]);
                    urn = &urn[1..];
                    continue;
                }
            }
            urn = &urn[3..];
        } else {
            res.push(urn[0]);
            urn = &urn[1..];
        }
    }
    String::from_utf8(res).ok()
}

/// Try to lookup the catalog local reference associated to a public ID.
///
/// Returns the local resource if found or null_mut() otherwise.
#[doc(alias = "xmlCatalogGetSGMLPublic")]
unsafe fn xml_catalog_get_sgml_public<'a>(
    catal: &HashMap<String, XmlCatalogEntryPtr>,
    pub_id: &str,
) -> Option<&'a Path> {
    let mut pub_id = Cow::Borrowed(pub_id);
    if let Some(normid) = normalize_public(pub_id.as_bytes()) {
        if normid.is_empty() {
            return None;
        }
        pub_id = Cow::Owned(String::from_utf8(normid).ok()?);
    }

    let entry: XmlCatalogEntryPtr = *catal.get(pub_id.as_ref()).unwrap_or(&null_mut());
    if entry.is_null() {
        return None;
    }
    if (*entry).typ == XmlCatalogEntryType::SgmlCataPublic {
        return (*entry).url.as_deref();
    }
    None
}

/// Try to lookup the catalog local reference for a system ID.
///
/// Returns the local resource if found or null_mut() otherwise.
#[doc(alias = "xmlCatalogGetSGMLSystem")]
unsafe fn xml_catalog_get_sgml_system<'a>(
    catal: &HashMap<String, XmlCatalogEntryPtr>,
    sys_id: &str,
) -> Option<&'a Path> {
    let entry: XmlCatalogEntryPtr = *catal.get(sys_id).unwrap_or(&null_mut());
    if entry.is_null() {
        return None;
    }
    if (*entry).typ == XmlCatalogEntryType::SgmlCataSystem {
        return (*entry).url.as_deref();
    }
    None
}

/// Free the memory allocated to a full chained list of Catalog entries
#[doc(alias = "xmlFreeCatalogEntryList")]
unsafe fn xml_free_catalog_entry_list(mut ret: XmlCatalogEntryPtr) {
    let mut next: XmlCatalogEntryPtr;

    while !ret.is_null() {
        next = (*ret).next;
        xml_free_catalog_entry(ret);
        ret = next;
    }
}

/// Free the memory allocated to a Catalog
#[doc(alias = "xmlFreeCatalog")]
pub unsafe fn xml_free_catalog(catal: XmlCatalogPtr) {
    if catal.is_null() {
        return;
    }
    if !(*catal).xml.is_null() {
        xml_free_catalog_entry_list((*catal).xml);
    }
    for (_, entry) in (*catal).sgml.drain() {
        xml_free_catalog_entry(entry);
    }
    drop_in_place(catal);
    xml_free(catal as _);
}

// Whether the catalog support was initialized.
static XML_CATALOG_INITIALIZED: AtomicBool = AtomicBool::new(false);
// The default catalog in use by the application
static XML_DEFAULT_CATALOG: AtomicPtr<XmlCatalog> = AtomicPtr::new(null_mut());

const XML_XML_DEFAULT_CATALOG: &str = concatcp!("file://", SYSCONFDIR, "/xml/catalog");
const XML_SGML_DEFAULT_CATALOG: &str = concatcp!("file://", SYSCONFDIR, "/sgml/catalog");

/// Do the catalog initialization only of global data, doesn't try to load
/// any catalog actually.
/// this function is not thread safe, catalog initialization should
/// preferably be done once at startup
#[doc(alias = "xmlInitializeCatalogData")]
unsafe fn xml_initialize_catalog_data() {
    let is_initialized = XML_CATALOG_INITIALIZED.load(Ordering::Acquire);
    if is_initialized {
        return;
    }

    if std::env::var_os("XML_DEBUG_CATALOG").is_some() {
        XML_DEBUG_CATALOGS.store(1, Ordering::Release);
    }
    XML_CATALOG_MUTEX.store(xml_new_rmutex(), Ordering::Release);

    XML_CATALOG_INITIALIZED.store(true, Ordering::Release);
}

/// Do the catalog initialization.  
/// this function is not thread safe, catalog initialization should
/// preferably be done once at startup
#[doc(alias = "xmlInitializeCatalog")]
pub unsafe fn xml_initialize_catalog() {
    let is_initialized = XML_CATALOG_INITIALIZED.load(Ordering::Acquire);
    if is_initialized {
        return;
    }

    xml_initialize_catalog_data();
    let mutex = XML_CATALOG_MUTEX.load(Ordering::Acquire);
    xml_rmutex_lock(mutex);

    if std::env::var_os("XML_DEBUG_CATALOG").is_some() {
        XML_DEBUG_CATALOGS.store(1, Ordering::Release);
    }

    if XML_DEFAULT_CATALOG.load(Ordering::Relaxed).is_null() {
        let mut nextent: *mut XmlCatalogEntryPtr;

        let catalogs = std::env::var_os("XML_CATALOG_FILES")
            .unwrap_or_else(|| OsString::from(XML_XML_DEFAULT_CATALOG));

        let catal: XmlCatalogPtr = xml_create_new_catalog(
            XmlCatalogType::XmlXMLCatalogType,
            XML_CATALOG_DEFAULT_PREFER,
        );
        if !catal.is_null() {
            nextent = &raw mut (*catal).xml;
            // the XML_CATALOG_FILES envvar is allowed to contain a space-separated list of entries.
            let bytes = catalogs.as_encoded_bytes();
            for path in bytes.split(|b| xml_is_blank_char(*b as u32)) {
                if !path.is_empty() {
                    let path = PathBuf::from(OsStr::from_encoded_bytes_unchecked(path));
                    *nextent = xml_new_catalog_entry(
                        XmlCatalogEntryType::XmlCataCatalog,
                        None,
                        None,
                        Some(path),
                        XML_CATALOG_DEFAULT_PREFER,
                        null_mut(),
                    );
                    if !(*nextent).is_null() {
                        nextent = &raw mut (*(*nextent)).next;
                    }
                }
            }
            XML_DEFAULT_CATALOG.store(catal, Ordering::Relaxed);
        }
    }

    xml_rmutex_unlock(mutex);
}

/// Load the catalog and makes its definitions effective for the default
/// external entity loader. It will recurse in SGML CATALOG entries.
/// this function is not thread safe, catalog initialization should
/// preferably be done once at startup
///
/// Returns 0 in case of success -1 in case of error
#[doc(alias = "xmlLoadCatalog")]
pub unsafe fn xml_load_catalog(filename: impl AsRef<Path>) -> i32 {
    let catal: XmlCatalogPtr;

    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog_data();
    }

    let mutex = XML_CATALOG_MUTEX.load(Ordering::Acquire);
    xml_rmutex_lock(mutex);

    let filename = filename.as_ref();
    let default_catalog = XML_DEFAULT_CATALOG.load(Ordering::Acquire);
    if default_catalog.is_null() {
        catal = xml_load_a_catalog(filename);
        if catal.is_null() {
            xml_rmutex_unlock(mutex);
            return -1;
        }

        XML_DEFAULT_CATALOG.store(catal, Ordering::Release);
        xml_rmutex_unlock(mutex);
        return 0;
    }

    let ret: i32 = (*default_catalog).expand_catalog(filename);
    xml_rmutex_unlock(mutex);
    ret
}

#[cfg(target_os = "windows")]
const PATH_SEPARATOR: u8 = b';';
#[cfg(not(target_os = "windows"))]
const PATH_SEPARATOR: u8 = b':';

/// Load the catalogs and makes their definitions effective for the default
/// external entity loader.
///
/// This function is not thread safe, catalog initialization should
/// preferably be done once at startup
#[doc(alias = "xmlLoadCatalogs")]
pub unsafe fn xml_load_catalogs(pathss: *const c_char) {
    let mut cur: *const c_char;
    let mut paths: *const c_char;
    let mut path: *mut XmlChar;
    #[cfg(target_os = "windows")]
    let i: i32;
    #[cfg(target_os = "windows")]
    let iLen: i32;

    if pathss.is_null() {
        return;
    }

    cur = pathss;
    while *cur != 0 {
        while xml_is_blank_char(*cur as u32) {
            cur = cur.add(1);
        }
        if *cur != 0 {
            paths = cur;
            while *cur != 0 && *cur != PATH_SEPARATOR as i8 && !xml_is_blank_char(*cur as u32) {
                cur = cur.add(1);
            }
            path = xml_strndup(paths as _, cur.offset_from(paths) as _);
            if !path.is_null() {
                #[cfg(target_os = "windows")]
                {
                    iLen = strlen(path);
                    for i in 0..iLen {
                        if *path.add(i as usize) == b'\\' {
                            *path.add(i as usize) = b'/';
                        }
                    }
                }
                xml_load_catalog(CStr::from_ptr(path as *const i8).to_string_lossy().as_ref());
                xml_free(path as _);
            }
        }
        while *cur == PATH_SEPARATOR as i8 {
            cur = cur.add(1);
        }
    }
}

/// Free the memory allocated to list of Catalog entries from the
/// catalog file hash.
#[doc(alias = "xmlFreeCatalogHashEntryList")]
extern "C" fn xml_free_catalog_hash_entry_list(payload: *mut c_void, _name: *const XmlChar) {
    let catal: XmlCatalogEntryPtr = payload as XmlCatalogEntryPtr;
    let mut children: XmlCatalogEntryPtr;
    let mut next: XmlCatalogEntryPtr;

    if catal.is_null() {
        return;
    }

    unsafe {
        children = (*catal).children;
        while !children.is_null() {
            next = (*children).next;
            (*children).dealloc = 0;
            (*children).children = null_mut();
            xml_free_catalog_entry(children);
            children = next;
        }
        (*catal).dealloc = 0;
        xml_free_catalog_entry(catal);
    }
}

/// Free up all the memory associated with catalogs
#[doc(alias = "xmlCatalogCleanup")]
pub unsafe fn xml_catalog_cleanup() {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Acquire) {
        return;
    }

    let mutex = XML_CATALOG_MUTEX.load(Ordering::Acquire);
    xml_rmutex_lock(mutex);
    if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
        generic_error!("Catalogs cleanup\n");
    }
    let files = XML_CATALOG_XMLFILES.load(Ordering::Acquire);
    if !files.is_null() {
        xml_hash_free(files, Some(xml_free_catalog_hash_entry_list));
    }
    XML_CATALOG_XMLFILES.store(null_mut(), Ordering::Release);
    let default_catalog = XML_DEFAULT_CATALOG.load(Ordering::Acquire);
    if !default_catalog.is_null() {
        xml_free_catalog(default_catalog);
    }
    XML_DEFAULT_CATALOG.store(null_mut(), Ordering::Release);
    XML_DEBUG_CATALOGS.store(0, Ordering::Release);
    XML_CATALOG_INITIALIZED.store(false, Ordering::Release);
    xml_rmutex_unlock(mutex);
    xml_free_rmutex(mutex);
}

/// Dump all the global catalog content to the given file.
#[doc(alias = "xmlCatalogDump")]
#[cfg(feature = "libxml_output")]
pub unsafe fn xml_catalog_dump<'a>(out: impl Write + 'a) {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    (*XML_DEFAULT_CATALOG.load(Ordering::Relaxed)).dump(out);
}

/// Do a complete resolution lookup of an External Identifier
///
/// Returns the URI of the resource or null_mut() if not found,
/// it must be freed by the caller.
#[doc(alias = "xmlCatalogResolve")]
pub unsafe fn xml_catalog_resolve(pub_id: Option<&str>, sys_id: Option<&str>) -> *mut XmlChar {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    (*XML_DEFAULT_CATALOG.load(Ordering::Relaxed)).resolve(pub_id, sys_id)
}

/// Try to lookup the catalog resource for a system ID
///
/// Returns the resource if found or null_mut() otherwise,
/// the value returned must be freed by the caller.
#[doc(alias = "xmlCatalogResolveSystem")]
pub unsafe fn xml_catalog_resolve_system(sys_id: &str) -> *mut XmlChar {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    (*XML_DEFAULT_CATALOG.load(Ordering::Relaxed)).resolve_system(sys_id)
}

/// Try to lookup the catalog reference associated to a public ID
///
/// Returns the resource if found or null_mut() otherwise,
/// the value returned must be freed by the caller.
#[doc(alias = "xmlCatalogResolvePublic")]
pub unsafe fn xml_catalog_resolve_public(pub_id: &str) -> *mut XmlChar {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    (*XML_DEFAULT_CATALOG.load(Ordering::Relaxed)).resolve_public(pub_id)
}

/// Do a complete resolution lookup of an URI
///
/// Returns the URI of the resource or null_mut() if not found,
/// it must be freed by the caller.
#[doc(alias = "xmlCatalogResolveURI")]
pub unsafe fn xml_catalog_resolve_uri(uri: &str) -> *mut XmlChar {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    (*XML_DEFAULT_CATALOG.load(Ordering::Relaxed)).resolve_uri(uri)
}

/// Add an entry in the catalog, it may overwrite existing but
/// different entries.
/// If called before any other catalog routine, allows to override the
/// default shared catalog put in place by `xmlInitializeCatalog()`.
///
/// Returns 0 if successful, -1 otherwise
#[doc(alias = "xmlCatalogAdd")]
pub unsafe fn xml_catalog_add(
    typ: Option<&str>,
    orig: *const XmlChar,
    replace: *const XmlChar,
) -> i32 {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog_data();
    }

    let mutex = XML_CATALOG_MUTEX.load(Ordering::Acquire);
    xml_rmutex_lock(mutex);
    // Specific case where one want to override the default catalog
    // put in place by xmlInitializeCatalog();
    let mut default_catalog = XML_DEFAULT_CATALOG.load(Ordering::Acquire);
    if default_catalog.is_null() && typ == Some("catalog") {
        default_catalog = xml_create_new_catalog(
            XmlCatalogType::XmlXMLCatalogType,
            XML_CATALOG_DEFAULT_PREFER,
        ) as _;

        if !default_catalog.is_null() {
            (*default_catalog).xml = xml_new_catalog_entry(
                XmlCatalogEntryType::XmlCataCatalog,
                None,
                (!orig.is_null())
                    .then(|| CStr::from_ptr(orig as *const i8).to_string_lossy())
                    .as_deref(),
                None,
                XML_CATALOG_DEFAULT_PREFER,
                null_mut(),
            );
        }
        XML_DEFAULT_CATALOG.store(default_catalog, Ordering::Release);
        xml_rmutex_unlock(mutex);
        return 0;
    }

    let res: i32 = (*default_catalog).add(
        typ,
        (!orig.is_null())
            .then(|| CStr::from_ptr(orig as *const i8).to_string_lossy())
            .as_deref(),
        replace,
    );
    XML_DEFAULT_CATALOG.store(default_catalog, Ordering::Release);
    xml_rmutex_unlock(mutex);
    res
}

/// Remove an entry from the catalog
///
/// Returns the number of entries removed if successful, -1 otherwise
#[doc(alias = "xmlCatalogRemove")]
pub unsafe fn xml_catalog_remove(value: &str) -> i32 {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    let mutex = XML_CATALOG_MUTEX.load(Ordering::Acquire);
    xml_rmutex_lock(mutex);
    let res: i32 = (*XML_DEFAULT_CATALOG.load(Ordering::Relaxed)).remove(value);
    xml_rmutex_unlock(mutex);
    res
}

/// Parse an XML file and build a tree.
/// It's like `xmlParseFile()` except it bypass all catalog lookups.
///
/// Returns the resulting document tree or null_mut() in case of error
#[doc(alias = "xmlParseCatalogFile")]
pub unsafe fn xml_parse_catalog_file(filename: &str) -> XmlDocPtr {
    let ret: XmlDocPtr;
    let mut directory: *mut c_char = null_mut();

    let ctxt: XmlParserCtxtPtr = xml_new_parser_ctxt();
    if ctxt.is_null() {
        xml_catalog_err_memory("allocating parser context");
        return null_mut();
    }

    let Some(buf) = XmlParserInputBuffer::from_uri(filename, XmlCharEncoding::None) else {
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    };

    let input_stream: XmlParserInputPtr = xml_new_input_stream(ctxt);
    if input_stream.is_null() {
        // xml_free_parser_input_buffer(buf);
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    }

    {
        let canonic = canonic_path(filename);
        (*input_stream).filename = Some(canonic.into_owned());
    }
    std::ptr::write(
        &raw mut (*input_stream).buf,
        Some(Rc::new(RefCell::new(buf))),
    );
    (*input_stream).reset_base();

    (*ctxt).input_push(input_stream);
    if (*ctxt).directory.is_none() {
        let filename = CString::new(filename).unwrap();
        directory = xml_parser_get_directory(filename.as_ptr());
    }
    if (*ctxt).directory.is_none() && !directory.is_null() {
        (*ctxt).directory = Some(CStr::from_ptr(directory).to_string_lossy().into_owned());
        xml_free(directory as _);
    }
    (*ctxt).valid = 0;
    (*ctxt).validate = 0;
    (*ctxt).loadsubset = 0;
    (*ctxt).pedantic = 0;
    (*ctxt).dict_names = 1;

    xml_parse_document(ctxt);

    if (*ctxt).well_formed != 0 {
        ret = (*ctxt).my_doc;
    } else {
        ret = null_mut();
        xml_free_doc((*ctxt).my_doc);
        (*ctxt).my_doc = null_mut();
    }
    xml_free_parser_ctxt(ctxt);

    ret
}

/// Convert all the SGML catalog entries as XML ones
///
/// Returns the number of entries converted if successful, -1 otherwise
#[doc(alias = "xmlCatalogConvert")]
pub unsafe fn xml_catalog_convert() -> i32 {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    let mutex = XML_CATALOG_MUTEX.load(Ordering::Acquire);
    xml_rmutex_lock(mutex);
    let res: i32 = (*XML_DEFAULT_CATALOG.load(Ordering::Relaxed)).convert_sgml_catalog();
    xml_rmutex_unlock(mutex);
    res
}

/// Free up the memory associated to the catalog list
#[doc(alias = "xmlCatalogFreeLocal")]
pub unsafe fn xml_catalog_free_local(catalogs: XmlCatalogEntryPtr) {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    let catal: XmlCatalogEntryPtr = catalogs as XmlCatalogEntryPtr;
    if !catal.is_null() {
        xml_free_catalog_entry_list(catal);
    }
}

/// Add the new entry to the catalog list
///
/// Returns the updated list
#[doc(alias = "xmlCatalogAddLocal")]
pub unsafe fn xml_catalog_add_local(
    catalogs: XmlCatalogEntryPtr,
    url: *const XmlChar,
) -> XmlCatalogEntryPtr {
    let mut catal: XmlCatalogEntryPtr;

    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    if url.is_null() {
        return catalogs;
    }

    if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
        generic_error!(
            "Adding document catalog {}\n",
            CStr::from_ptr(url as *const i8).to_string_lossy()
        );
    }

    let add: XmlCatalogEntryPtr = xml_new_catalog_entry(
        XmlCatalogEntryType::XmlCataCatalog,
        None,
        Some(CStr::from_ptr(url as *const i8).to_string_lossy().as_ref()),
        None,
        XML_CATALOG_DEFAULT_PREFER,
        null_mut(),
    );
    if add.is_null() {
        return catalogs;
    }

    catal = catalogs as XmlCatalogEntryPtr;
    if catal.is_null() {
        return add as _;
    }

    while !(*catal).next.is_null() {
        catal = (*catal).next;
    }
    (*catal).next = add;
    catalogs
}

/// Used to set the debug level for catalog operation, 0 disable
/// debugging, 1 enable it
///
/// Returns the previous value of the catalog debugging level
#[doc(alias = "xmlCatalogSetDebug")]
pub unsafe fn xml_catalog_set_debug(level: i32) -> i32 {
    let ret: i32 = XML_DEBUG_CATALOGS.load(Ordering::Acquire);

    if level <= 0 {
        XML_DEBUG_CATALOGS.store(0, Ordering::Release);
    } else {
        XML_DEBUG_CATALOGS.store(level, Ordering::Release);
    }
    ret
}

/// Allows to set the preference between public and system for deletion
/// in XML Catalog resolution. C.f. section 4.1.1 of the spec
/// Values accepted are xmlCatalogPrefer::XML_CATA_PREFER_PUBLIC or xmlCatalogPrefer::XML_CATA_PREFER_SYSTEM
///
/// Returns the previous value of the default preference for delegation
#[doc(alias = "xmlCatalogSetDefaultPrefer")]
pub unsafe fn xml_catalog_set_default_prefer(prefer: XmlCatalogPrefer) -> XmlCatalogPrefer {
    let ret: XmlCatalogPrefer = XML_CATALOG_DEFAULT_PREFER;

    if matches!(prefer, XmlCatalogPrefer::None) {
        return ret;
    }

    if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
        match prefer {
            XmlCatalogPrefer::Public => {
                generic_error!("Setting catalog preference to PUBLIC\n");
            }
            XmlCatalogPrefer::System => {
                generic_error!("Setting catalog preference to SYSTEM\n");
            }
            _ => {
                return ret;
            }
        }
    }
    XML_CATALOG_DEFAULT_PREFER = prefer;
    ret
}

/// Used to set the user preference w.r.t. to what catalogs should be accepted
#[doc(alias = "xmlCatalogSetDefaults")]
pub unsafe fn xml_catalog_set_defaults(allow: XmlCatalogAllow) {
    if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
        match allow {
            XmlCatalogAllow::None => {
                generic_error!("Disabling catalog usage\n");
            }
            XmlCatalogAllow::Global => {
                generic_error!("Allowing only global catalogs\n");
            }
            XmlCatalogAllow::Document => {
                generic_error!("Allowing only catalogs from the document\n");
            }
            XmlCatalogAllow::All => {
                generic_error!("Allowing all catalogs\n");
            }
        }
    }
    XML_CATALOG_DEFAULT_ALLOW = allow;
}

/// Used to get the user preference w.r.t. to what catalogs should be accepted
///
/// Returns the current xmlCatalogAllow value
#[doc(alias = "xmlCatalogGetDefaults")]
pub unsafe fn xml_catalog_get_defaults() -> XmlCatalogAllow {
    XML_CATALOG_DEFAULT_ALLOW
}

/// Try to lookup the catalog reference associated to a system ID
///
/// Returns the resource if found or null_mut() otherwise.
#[deprecated = "use xmlCatalogResolveSystem()"]
#[doc(alias = "xmlCatalogGetSystem")]
pub unsafe fn xml_catalog_get_system(sys_id: &str) -> *const XmlChar {
    let ret: *mut XmlChar;
    static mut RESULT: [XmlChar; 1000] = [0; 1000];
    static MSG: AtomicI32 = AtomicI32::new(0);

    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    if MSG.load(Ordering::Relaxed) == 0 {
        generic_error!("Use of deprecated xmlCatalogGetSystem() call\n");
        MSG.fetch_add(1, Ordering::AcqRel);
    }

    // Check first the XML catalogs
    let default_catalog = XML_DEFAULT_CATALOG.load(Ordering::Acquire);
    if !default_catalog.is_null() {
        ret = (*(*default_catalog).xml).list_xml_resolve(None, Some(sys_id));
        if !ret.is_null() && ret != XML_CATAL_BREAK {
            snprintf(
                RESULT.as_mut_ptr() as _,
                RESULT.len() - 1,
                c"%s".as_ptr() as _,
                ret,
            );
            RESULT[RESULT.len() - 1] = 0;
            return RESULT.as_ptr() as _;
        }
    }

    if !default_catalog.is_null() {
        return if let Some(sgml) = xml_catalog_get_sgml_system(&(*default_catalog).sgml, sys_id) {
            let sgml = CString::new(sgml.to_string_lossy().as_ref()).unwrap();
            snprintf(
                RESULT.as_mut_ptr() as _,
                RESULT.len() - 1,
                c"%s".as_ptr() as _,
                sgml,
            );
            RESULT[RESULT.len() - 1] = 0;
            return RESULT.as_ptr() as _;
        } else {
            null_mut()
        };
    }
    null_mut()
}

/// Try to lookup the catalog reference associated to a public ID
///
/// Returns the resource if found or null_mut() otherwise.
#[deprecated = "use xmlCatalogResolvePublic()"]
#[doc(alias = "xmlCatalogGetPublic")]
pub unsafe fn xml_catalog_get_public(pub_id: &str) -> *const XmlChar {
    let ret: *mut XmlChar;
    static mut RESULT: [XmlChar; 1000] = [0; 1000];
    static MSG: AtomicI32 = AtomicI32::new(0);

    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    let old = MSG.load(Ordering::Acquire);
    if old == 0 {
        generic_error!("Use of deprecated xmlCatalogGetPublic() call\n");
        MSG.store(old + 1, Ordering::Release);
    }

    // Check first the XML catalogs
    let default_catalog = XML_DEFAULT_CATALOG.load(Ordering::Acquire);
    if !default_catalog.is_null() {
        ret = (*(*default_catalog).xml).list_xml_resolve(Some(pub_id), None);
        if !ret.is_null() && ret != XML_CATAL_BREAK {
            snprintf(
                RESULT.as_mut_ptr() as _,
                RESULT.len() - 1,
                c"%s".as_ptr() as _,
                ret,
            );
            RESULT[RESULT.len() - 1] = 0;
            return RESULT.as_ptr() as _;
        }
    }

    if !default_catalog.is_null() {
        return if let Some(sgml) = xml_catalog_get_sgml_public(&(*default_catalog).sgml, pub_id) {
            let sgml = CString::new(sgml.to_string_lossy().as_ref()).unwrap();
            snprintf(
                RESULT.as_mut_ptr() as _,
                RESULT.len() - 1,
                c"%s".as_ptr() as _,
                sgml,
            );
            RESULT[RESULT.len() - 1] = 0;
            return RESULT.as_ptr() as _;
        } else {
            null_mut()
        };
    }
    null_mut()
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_catalog_cleanup() {
        let lock = TEST_CATALOG_LOCK.lock().unwrap();
        #[cfg(feature = "catalog")]
        unsafe {
            xml_catalog_cleanup();
            reset_last_error();
        }
        drop(lock);
    }

    #[test]
    fn test_xml_catalog_convert() {
        let lock = TEST_CATALOG_LOCK.lock().unwrap();
        #[cfg(feature = "catalog")]
        unsafe {
            let ret_val = xml_catalog_convert();
            desret_int(ret_val);
            reset_last_error();
        }
        drop(lock);
    }

    #[test]
    fn test_xml_catalog_dump() {
        let lock = TEST_CATALOG_LOCK.lock().unwrap();
        #[cfg(all(feature = "catalog", feature = "libxml_output"))]
        unsafe {
            let mut leaks = 0;

            for n_out in 0..GEN_NB_FILE_PTR {
                let mem_base = xml_mem_blocks();
                let out = gen_file_ptr(n_out, 0).unwrap();

                xml_catalog_dump(out);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlCatalogDump",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_out);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlCatalogDump()");
        }
        drop(lock);
    }

    #[test]
    fn test_xml_catalog_get_defaults() {
        let lock = TEST_CATALOG_LOCK.lock().unwrap();
        #[cfg(feature = "catalog")]
        unsafe {
            let mut leaks = 0;

            let mem_base = xml_mem_blocks();

            let ret_val = xml_catalog_get_defaults();
            desret_xml_catalog_allow(ret_val);
            reset_last_error();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in xmlCatalogGetDefaults",
                    xml_mem_blocks() - mem_base
                );
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlCatalogGetDefaults()"
            );
        }
        drop(lock);
    }

    #[test]
    fn test_xml_catalog_set_default_prefer() {
        let lock = TEST_CATALOG_LOCK.lock().unwrap();
        #[cfg(feature = "catalog")]
        unsafe {
            let mut leaks = 0;

            for n_prefer in 0..GEN_NB_XML_CATALOG_PREFER {
                let mem_base = xml_mem_blocks();
                let prefer = gen_xml_catalog_prefer(n_prefer, 0);

                let ret_val = xml_catalog_set_default_prefer(prefer);
                desret_xml_catalog_prefer(ret_val);
                des_xml_catalog_prefer(n_prefer, prefer, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlCatalogSetDefaultPrefer",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_prefer);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlCatalogSetDefaultPrefer()"
            );
        }
        drop(lock);
    }

    #[test]
    fn test_xml_catalog_set_defaults() {
        let lock = TEST_CATALOG_LOCK.lock().unwrap();
        #[cfg(feature = "catalog")]
        unsafe {
            let mut leaks = 0;

            for n_allow in 0..GEN_NB_XML_CATALOG_ALLOW {
                let mem_base = xml_mem_blocks();
                let allow = gen_xml_catalog_allow(n_allow, 0);

                xml_catalog_set_defaults(allow);
                des_xml_catalog_allow(n_allow, allow, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlCatalogSetDefaults",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_allow);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlCatalogSetDefaults()"
            );
        }
        drop(lock);
    }

    #[test]
    fn test_xml_initialize_catalog() {
        let lock = TEST_CATALOG_LOCK.lock().unwrap();
        #[cfg(feature = "catalog")]
        unsafe {
            let mut leaks = 0;

            let mem_base = xml_mem_blocks();

            xml_initialize_catalog();
            reset_last_error();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in xmlInitializeCatalog",
                    xml_mem_blocks() - mem_base
                );
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlInitializeCatalog()"
            );
        }
        drop(lock);
    }

    #[test]
    fn test_xml_load_catalogs() {
        let lock = TEST_CATALOG_LOCK.lock().unwrap();
        #[cfg(feature = "catalog")]
        unsafe {
            for n_pathss in 0..GEN_NB_CONST_CHAR_PTR {
                let pathss = gen_const_char_ptr(n_pathss, 0);

                xml_load_catalogs(pathss);
                des_const_char_ptr(n_pathss, pathss, 0);
                reset_last_error();
            }
        }
        drop(lock);
    }
}
