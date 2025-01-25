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
    collections::{BTreeMap, HashMap},
    env::split_paths,
    ffi::CStr,
    fs::File,
    io::Read,
    path::{Path, PathBuf},
    ptr::{drop_in_place, null, null_mut},
    rc::Rc,
    str::from_utf8,
    sync::{
        atomic::{AtomicBool, AtomicI32, Ordering},
        Arc, RwLock, Weak,
    },
};

use const_format::concatcp;

#[cfg(feature = "libxml_output")]
use crate::tree::XmlNs;
use crate::{
    encoding::XmlCharEncoding,
    error::__xml_raise_error,
    generic_error,
    io::{xml_parser_get_directory, XmlParserInputBuffer},
    libxml::{
        chvalid::xml_is_blank_char,
        globals::xml_free,
        parser::xml_parse_document,
        parser_internals::XML_MAX_NAMELEN,
        threads::xml_get_thread_id,
        xmlstring::{xml_str_equal, XmlChar},
    },
    parser::{xml_free_parser_ctxt, xml_new_input_stream, xml_new_parser_ctxt, XmlParserInputPtr},
    tree::{
        xml_free_doc, xml_free_ns, xml_new_doc, xml_new_doc_node, xml_new_dtd, xml_new_ns,
        NodeCommon, XmlDoc, XmlNode, XML_XML_NAMESPACE,
    },
    uri::{build_uri, canonic_path},
    SYSCONFDIR,
};

use super::{chvalid::xml_is_pubid_char, parser_internals::xml_is_letter};

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

// Hash table containing all the trees of XML catalogs parsed by the application.
static XML_CATALOG_XMLFILES: RwLock<BTreeMap<String, Arc<RwLock<CatalogEntryListNode>>>> =
    RwLock::new(BTreeMap::new());

// Those are preferences
static XML_DEBUG_CATALOGS: AtomicI32 = AtomicI32::new(0); /* used for debugging */
static XML_CATALOG_DEFAULT_ALLOW: RwLock<XmlCatalogAllow> = RwLock::new(XmlCatalogAllow::All);
static XML_CATALOG_DEFAULT_PREFER: RwLock<XmlCatalogPrefer> = RwLock::new(XmlCatalogPrefer::Public);

// Whether the catalog support was initialized.
static XML_CATALOG_INITIALIZED: AtomicBool = AtomicBool::new(false);
// The default catalog in use by the application
static XML_DEFAULT_CATALOG: RwLock<Option<XmlCatalog>> = RwLock::new(None);

const XML_XML_DEFAULT_CATALOG: &str = concatcp!("file://", SYSCONFDIR, "/xml/catalog");
const XML_SGML_DEFAULT_CATALOG: &str = concatcp!("file://", SYSCONFDIR, "/sgml/catalog");

/// The namespace for the XML Catalogs elements.
pub(crate) const XML_CATALOGS_NAMESPACE: &CStr = c"urn:oasis:names:tc:entity:xmlns:xml:catalog";
/// The specific XML Catalog Processing Instruction name.
pub(crate) const XML_CATALOG_PI: &str = "oasis-xml-catalog";

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
#[derive(Default)]
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
    sgml: HashMap<String, XmlCatalogEntry>,

    // XML Catalogs are stored as a tree of Catalog entries
    prefer: XmlCatalogPrefer,
    xml: Option<XmlCatalogEntry>,
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
                        let entry = xml_new_catalog_entry(
                            typ,
                            name.as_deref().and_then(|n| from_utf8(n).ok()),
                            Some(&filename),
                            None,
                            XmlCatalogPrefer::None,
                            None,
                        );
                        let name = String::from_utf8_lossy(name.unwrap().as_ref()).into_owned();
                        self.sgml.insert(name, entry);
                    }
                } else if matches!(typ, XmlCatalogEntryType::SgmlCataCatalog) {
                    if is_super != 0 {
                        let entry = xml_new_catalog_entry(
                            typ,
                            sysid.as_deref().and_then(|n| from_utf8(n).ok()),
                            None,
                            None,
                            XmlCatalogPrefer::None,
                            None,
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
            let tmp = xml_new_catalog_entry(
                XmlCatalogEntryType::XmlCataCatalog,
                None,
                None,
                Some(filename.to_owned()),
                *XML_CATALOG_DEFAULT_PREFER.read().unwrap(),
                None,
            );

            if let Some(xml) = self.xml.as_ref() {
                let mut prev = None;
                let mut cur = Some(xml.node.clone());
                while let Some(now) = cur.clone() {
                    (prev, cur) = (cur, now.read().unwrap().next.clone());
                }
                prev.unwrap().write().unwrap().next = Some(tmp.node);
            } else {
                self.xml = Some(tmp);
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
        replace: Option<&str>,
    ) -> i32 {
        let mut res: i32 = -1;

        if matches!(self.typ, XmlCatalogType::XmlXMLCatalogType) {
            res = self
                .xml
                .as_mut()
                .unwrap()
                .add_xml_catalog(typ, orig, replace);
        } else {
            let cattype: XmlCatalogEntryType = xml_get_sgml_catalog_entry_type(typ);
            if !matches!(cattype, XmlCatalogEntryType::XmlCataNone) {
                let entry = xml_new_catalog_entry(
                    cattype,
                    orig,
                    replace,
                    None,
                    XmlCatalogPrefer::None,
                    None,
                );
                if let Some(orig) = orig {
                    self.sgml.insert(orig.to_owned(), entry);
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
            self.xml.as_mut().unwrap().del_xml_catalog(value)
        } else {
            self.sgml.remove(value).is_some() as i32
        }
    }

    /// Check is a catalog is empty
    ///
    /// Returns 1 if the catalog is empty, 0 if not, amd -1 in case of error.
    #[doc(alias = "xmlCatalogIsEmpty")]
    pub unsafe fn is_empty(&self) -> i32 {
        if matches!(self.typ, XmlCatalogType::XmlXMLCatalogType) {
            let Some(xml) = self.xml.as_ref() else {
                return 1;
            };
            let xml = xml.node.read().unwrap();
            if xml.typ != XmlCatalogEntryType::XmlCataCatalog
                && xml.typ != XmlCatalogEntryType::XmlCataBrokenCatalog
            {
                return -1;
            }
            if xml.children.is_none() {
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
            self.xml.as_ref().unwrap().dump_xml_catalog(out);
        } else {
            for entry in self.sgml.values() {
                entry.dump_entry(&mut out);
            }
        }
    }

    /// Do a complete resolution lookup of an External Identifier.
    ///
    /// Returns the URI of the resource or null_mut() if not found
    #[doc(alias = "xmlCatalogSGMLResolve")]
    unsafe fn sgml_resolve(&self, pub_id: Option<&str>, sys_id: Option<&str>) -> Option<PathBuf> {
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
    pub unsafe fn resolve_uri(&mut self, uri: &str) -> Option<String> {
        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            generic_error!("Resolve URI {uri}\n");
        }

        if matches!(self.typ, XmlCatalogType::XmlXMLCatalogType) {
            self.xml.as_mut().unwrap().list_xml_resolve_uri(uri)
        } else {
            self.sgml_resolve(None, Some(uri))
                .map(|sgml| sgml.to_string_lossy().into_owned())
        }
    }

    /// Try to lookup the catalog local reference associated to a public ID in that catalog
    ///
    /// Returns the local resource if found or null_mut() otherwise,
    /// the value returned must be freed by the caller.
    #[doc(alias = "xmlACatalogResolvePublic")]
    pub unsafe fn resolve_public(&mut self, pub_id: &str) -> Option<String> {
        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            generic_error!("Resolve pubID {pub_id}\n");
        }

        if matches!(self.typ, XmlCatalogType::XmlXMLCatalogType) {
            self.xml
                .as_mut()
                .unwrap()
                .list_xml_resolve(Some(pub_id), None)
        } else {
            xml_catalog_get_sgml_public(&self.sgml, pub_id)
                .map(|sgml| sgml.to_string_lossy().into_owned())
        }
    }

    /// Try to lookup the catalog resource for a system ID
    ///
    /// Returns the resource if found or null_mut() otherwise,
    /// the value returned must be freed by the caller.
    #[doc(alias = "xmlACatalogResolveSystem")]
    pub unsafe fn resolve_system(&mut self, sys_id: &str) -> Option<String> {
        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            generic_error!("Resolve sysID {sys_id}\n");
        }

        if matches!(self.typ, XmlCatalogType::XmlXMLCatalogType) {
            self.xml
                .as_mut()
                .unwrap()
                .list_xml_resolve(None, Some(sys_id))
        } else {
            xml_catalog_get_sgml_system(&self.sgml, sys_id)
                .map(|sgml| sgml.to_string_lossy().into_owned())
        }
    }

    /// Do a complete resolution lookup of an External Identifier
    ///
    /// Returns the URI of the resource or null_mut() if not found, it must be freed by the caller.
    #[doc(alias = "xmlACatalogResolve")]
    pub unsafe fn resolve(&mut self, pub_id: Option<&str>, sys_id: Option<&str>) -> Option<String> {
        if pub_id.is_none() && sys_id.is_none() {
            return None;
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
            self.xml.as_mut().unwrap().list_xml_resolve(pub_id, sys_id)
        } else {
            self.sgml_resolve(pub_id, sys_id)
                .map(|sgml| sgml.to_string_lossy().into_owned())
        }
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

        if self.xml.is_none() {
            return 0;
        }

        for (_, XmlCatalogEntry { node }) in self.sgml.drain() {
            match node.read().unwrap().typ {
                XmlCatalogEntryType::SgmlCataEntity => {
                    node.write().unwrap().typ = XmlCatalogEntryType::XmlCataPublic;
                }
                XmlCatalogEntryType::SgmlCataPentity => {
                    node.write().unwrap().typ = XmlCatalogEntryType::XmlCataPublic;
                }
                XmlCatalogEntryType::SgmlCataDoctype => {
                    node.write().unwrap().typ = XmlCatalogEntryType::XmlCataPublic;
                }
                XmlCatalogEntryType::SgmlCataLinktype => {
                    node.write().unwrap().typ = XmlCatalogEntryType::XmlCataPublic;
                }
                XmlCatalogEntryType::SgmlCataNotation => {
                    node.write().unwrap().typ = XmlCatalogEntryType::XmlCataPublic;
                }
                XmlCatalogEntryType::SgmlCataPublic => {
                    node.write().unwrap().typ = XmlCatalogEntryType::XmlCataPublic;
                }
                XmlCatalogEntryType::SgmlCataSystem => {
                    node.write().unwrap().typ = XmlCatalogEntryType::XmlCataSystem;
                }
                XmlCatalogEntryType::SgmlCataDelegate => {
                    node.write().unwrap().typ = XmlCatalogEntryType::XmlCataDelegatePublic;
                }
                XmlCatalogEntryType::SgmlCataCatalog => {
                    node.write().unwrap().typ = XmlCatalogEntryType::XmlCataCatalog;
                }
                _ => {
                    continue;
                }
            }
            // Conversion successful, remove from the SGML catalog
            // and add it to the default XML one
            node.write().unwrap().parent = self.xml.as_ref().map(|xml| Arc::downgrade(&xml.node));
            node.write().unwrap().next = None;
            if self
                .xml
                .as_ref()
                .unwrap()
                .node
                .read()
                .unwrap()
                .children
                .is_none()
            {
                self.xml.as_mut().unwrap().node.write().unwrap().children = Some(node);
            } else {
                let mut prev = None;
                let mut cur = self
                    .xml
                    .as_mut()
                    .unwrap()
                    .node
                    .write()
                    .unwrap()
                    .children
                    .clone();
                while let Some(now) = cur.clone() {
                    (prev, cur) = (cur, now.read().unwrap().next.clone());
                }
                prev.unwrap().write().unwrap().next = Some(node);
            }
        }
        0
    }
}

#[derive(Debug, Default)]
struct CatalogEntryListNode {
    next: Option<Arc<RwLock<CatalogEntryListNode>>>,
    parent: Option<Weak<RwLock<CatalogEntryListNode>>>,
    children: Option<Arc<RwLock<CatalogEntryListNode>>>,
    typ: XmlCatalogEntryType,
    name: Option<String>,
    value: Option<String>,
    url: Option<PathBuf>, /* The expanded URL using the base */
    prefer: XmlCatalogPrefer,
    dealloc: i32,
    depth: i32,
    group: Option<Arc<RwLock<CatalogEntryListNode>>>,
}

impl CatalogEntryListNode {
    /// Fetch and parse the subcatalog referenced by an entry
    ///
    /// Returns 0 in case of success, -1 otherwise
    #[doc(alias = "xmlFetchXMLCatalogFile")]
    unsafe fn fetch_xml_catalog_file(&mut self) -> i32 {
        let Some(url) = self.url.as_deref() else {
            return -1;
        };

        // lock the whole catalog for modification
        let mut catalog_files = XML_CATALOG_XMLFILES.write().unwrap();
        if self.children.is_some() {
            // Okay someone else did it in the meantime
            return 0;
        }

        if let Some(doc) = catalog_files.get(url.to_string_lossy().as_ref()) {
            if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                generic_error!("Found {} in file hash\n", url.display());
            }

            if self.typ == XmlCatalogEntryType::XmlCataCatalog {
                self.children = doc.read().unwrap().children.clone();
            } else {
                self.children = Some(doc.clone());
            }
            self.dealloc = 0;
            return 0;
        }
        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            generic_error!("{} not found in file hash\n", url.display());
        }

        // Fetch and parse. Note that xmlParseXMLCatalogFile does not use the existing catalog,
        // there is no recursion allowed at that level.
        let Some(doc) = xml_parse_xml_catalog_file(self.prefer, url.to_string_lossy().as_ref())
        else {
            self.typ = XmlCatalogEntryType::XmlCataBrokenCatalog;
            return -1;
        };

        if matches!(self.typ, XmlCatalogEntryType::XmlCataCatalog) {
            self.children = doc.read().unwrap().children.clone();
        } else {
            self.children = Some(doc.clone());
        }

        doc.write().unwrap().dealloc = 1;

        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            generic_error!("{} added to file hash\n", url.display());
        }
        catalog_files.insert(url.to_string_lossy().into_owned(), doc);
        0
    }

    /// Do a complete resolution lookup of an URI for a list of catalogs
    ///
    /// Implements (or tries to) 7.2. URI Resolution
    /// from http://www.oasis-open.org/committees/entity/spec-2001-08-06.html
    ///
    /// Returns the URI of the resource or null_mut() if not found
    #[doc(alias = "xmlCatalogListXMLResolveURI")]
    unsafe fn list_xml_resolve_uri(entry: &Arc<RwLock<Self>>, uri: &str) -> Option<String> {
        if uri.starts_with(XML_URN_PUBID) {
            let urn_id = xml_catalog_unwrap_urn(uri);
            if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                if let Some(urn_id) = urn_id.as_deref() {
                    generic_error!("URN ID expanded to {urn_id}\n");
                } else {
                    generic_error!("URN ID {uri} expanded to NULL\n");
                }
            }
            return Self::list_xml_resolve(entry, Some(&urn_id?), None);
        }
        let mut catal = Some(entry.clone());
        while let Some(now) = catal {
            let mut now = now.write().unwrap();
            if now.typ == XmlCatalogEntryType::XmlCataCatalog {
                if now.children.is_none() {
                    now.fetch_xml_catalog_file();
                }
                if let Some(children) = now.children.as_ref() {
                    if let Some(ret) = Self::xml_resolve_uri(children, uri) {
                        return Some(ret);
                    }
                }
            }
            catal = now.next.clone();
        }
        None
    }

    /// Do a complete resolution lookup of an External Identifier for a list of catalogs
    ///
    /// Implements (or tries to) 7.1. External Identifier Resolution
    /// from http://www.oasis-open.org/committees/entity/spec-2001-08-06.html
    ///
    /// Returns the URI of the resource or null_mut() if not found
    #[doc(alias = "xmlCatalogListXMLResolve")]
    unsafe fn list_xml_resolve(
        entry: &Arc<RwLock<Self>>,
        pub_id: Option<&str>,
        sys_id: Option<&str>,
    ) -> Option<String> {
        if pub_id.is_none() && sys_id.is_none() {
            return None;
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
            return Self::list_xml_resolve(entry, Some(&urn_id?), sys_id);
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
            let urn_id = urn_id?;
            return if let Some(pub_id) = pub_id {
                if pub_id == urn_id {
                    Self::list_xml_resolve(entry, Some(&pub_id), None)
                } else {
                    Self::list_xml_resolve(entry, Some(&pub_id), Some(&urn_id))
                }
            } else {
                Self::list_xml_resolve(entry, Some(&urn_id), None)
            };
        }
        let mut catal = Some(entry.clone());
        while let Some(now) = catal {
            let mut n = now.write().unwrap();
            catal = n.next.clone();
            if n.typ == XmlCatalogEntryType::XmlCataCatalog {
                if n.children.is_none() {
                    n.fetch_xml_catalog_file();
                }
                if let Some(children) = n.children.as_ref() {
                    if let Some(ret) = Self::xml_resolve(children, pub_id.as_deref(), sys_id) {
                        return Some(ret);
                    }
                    if children.read().unwrap().depth > MAX_CATAL_DEPTH as i32 {
                        return None;
                    }
                }
            }
        }
        None
    }

    /// Do a complete resolution lookup of an External Identifier for a list of catalog entries.
    ///
    /// Implements (or tries to) 7.2.2. URI Resolution
    /// from http://www.oasis-open.org/committees/entity/spec-2001-08-06.html
    ///
    /// Returns the URI of the resource or null_mut() if not found
    #[doc(alias = "xmlCatalogXMLResolveURI")]
    unsafe fn xml_resolve_uri(node: &Arc<RwLock<Self>>, uri: &str) -> Option<String> {
        let mut have_next: i32 = 0;
        let mut lenrewrite: i32 = 0;

        if node.read().unwrap().depth > MAX_CATAL_DEPTH as i32 {
            xml_catalog_err!(
                Arc::as_ptr(node) as *mut RwLock<Self>,
                null_mut(),
                XmlParserErrors::XmlCatalogRecursion,
                "Detected recursion in catalog {}\n",
                node.read().unwrap().name.as_deref().unwrap(),
            );
            return None;
        }

        // First tries steps 2/ 3/ 4/ if a system ID is provided.
        let mut cur = Some(node.clone());
        let mut rewrite = None;
        let mut have_delegate = 0;
        while let Some(now) = cur {
            let n = now.write().unwrap();
            cur = n.next.clone();
            match n.typ {
                XmlCatalogEntryType::XmlCataURI => {
                    if Some(uri) == n.name.as_deref() {
                        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                            generic_error!("Found URI match {}\n", n.name.as_deref().unwrap());
                        }
                        return Some(n.url.as_deref()?.to_string_lossy().into_owned());
                    }
                }
                XmlCatalogEntryType::XmlCataRewriteURI => {
                    let len = n.name.as_deref().map_or(0, |n| n.len()) as i32;
                    if len > lenrewrite && uri.starts_with(n.name.as_deref().unwrap()) {
                        lenrewrite = len;
                        drop(n);
                        rewrite = Some(now);
                    }
                }
                XmlCatalogEntryType::XmlCataDelegateURI => {
                    if uri.starts_with(n.name.as_deref().unwrap()) {
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
            let rewrite = rewrite.read().unwrap();
            if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                generic_error!(
                    "Using rewriting rule {}\n",
                    rewrite.name.as_deref().unwrap()
                );
            }
            let mut url = rewrite.url.as_deref()?.to_string_lossy().into_owned();
            let uri = &uri[lenrewrite as usize..];
            url.push_str(uri);
            return Some(url);
        }
        if have_delegate != 0 {
            let mut delegates: [Option<PathBuf>; MAX_DELEGATE] = [const { None }; MAX_DELEGATE];
            let mut nb_list: usize = 0;

            // Assume the entries have been sorted by decreasing substring
            // matches when the list was produced.
            let mut cur = Some(node.clone());
            'b: while let Some(now) = cur {
                let mut now = now.write().unwrap();
                if matches!(
                    now.typ,
                    XmlCatalogEntryType::XmlCataDelegateSystem
                        | XmlCatalogEntryType::XmlCataDelegateURI
                ) && uri.starts_with(now.name.as_deref().unwrap())
                {
                    for i in 0..nb_list {
                        if now.url == delegates[i] {
                            cur = now.next.clone();
                            continue 'b;
                        }
                    }
                    if now.children.is_none() {
                        now.fetch_xml_catalog_file();
                    }
                    if nb_list < MAX_DELEGATE {
                        delegates[nb_list] = now.url.as_deref().map(|u| u.to_owned());
                        nb_list += 1;
                    }

                    if let Some(children) = now.children.as_ref() {
                        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                            generic_error!(
                                "Trying URI delegate {}\n",
                                now.url.as_deref().unwrap().display()
                            );
                        }
                        if let Some(ret) = Self::list_xml_resolve_uri(children, uri) {
                            return Some(ret);
                        }
                    }
                }
                cur = now.next.clone();
            }
            // Apply the cut algorithm explained in 4/
            return None;
        }
        if have_next != 0 {
            let mut cur = Some(node.clone());
            while let Some(now) = cur {
                let mut now = now.write().unwrap();
                if now.typ == XmlCatalogEntryType::XmlCataNextCatalog {
                    if now.children.is_none() {
                        now.fetch_xml_catalog_file();
                    }
                    if let Some(children) = now.children.as_ref() {
                        if let Some(ret) = Self::list_xml_resolve_uri(children, uri) {
                            return Some(ret);
                        }
                    }
                }
                cur = now.next.clone();
            }
        }

        None
    }

    /// Do a complete resolution lookup of an External Identifier for a list of catalog entries.
    ///
    /// Implements (or tries to) 7.1. External Identifier Resolution
    /// from http://www.oasis-open.org/committees/entity/spec-2001-08-06.html
    ///
    /// Returns the URI of the resource or null_mut() if not found
    #[doc(alias = "xmlCatalogXMLResolve")]
    unsafe fn xml_resolve(
        entry: &Arc<RwLock<Self>>,
        pub_id: Option<&str>,
        sys_id: Option<&str>,
    ) -> Option<String> {
        let mut have_delegate: i32;
        let mut have_next: i32 = 0;

        // protection against loops
        if entry.read().unwrap().depth > MAX_CATAL_DEPTH as i32 {
            xml_catalog_err!(
                Arc::as_ptr(entry) as *mut RwLock<Self>,
                null_mut(),
                XmlParserErrors::XmlCatalogRecursion,
                "Detected recursion in catalog {}\n",
                entry.read().unwrap().name.as_deref().unwrap(),
            );
            return None;
        }
        entry.write().unwrap().depth += 1;

        // First tries steps 2/ 3/ 4/ if a system ID is provided.
        if let Some(sys_id) = sys_id {
            let mut rewrite = None;
            let mut lenrewrite = 0;
            let mut cur = Some(entry.clone());
            have_delegate = 0;
            while let Some(now) = cur {
                let n = now.write().unwrap();
                cur = n.next.clone();
                match n.typ {
                    XmlCatalogEntryType::XmlCataSystem => {
                        if let Some(name) = n.name.as_deref() {
                            if Some(sys_id) == n.name.as_deref() {
                                let url = n.url.as_deref().unwrap();
                                if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                                    generic_error!(
                                        "Found system match {name}, using {}\n",
                                        url.display()
                                    );
                                }
                                let res = url.to_string_lossy().into_owned();
                                // If `n == entry`, deadlock occurs at next line `entry.write()....`.
                                // Therefore, drop `n` at this.
                                drop(n);
                                entry.write().unwrap().depth -= 1;
                                return Some(res);
                            }
                        }
                    }
                    XmlCatalogEntryType::XmlCataRewriteSystem => {
                        let len = n.name.as_deref().map_or(0, |n| n.len());
                        if len > lenrewrite && sys_id.starts_with(n.name.as_deref().unwrap()) {
                            lenrewrite = len;
                            drop(n);
                            rewrite = Some(now.clone());
                        }
                    }
                    XmlCatalogEntryType::XmlCataDelegateSystem => {
                        if sys_id.starts_with(n.name.as_deref().unwrap()) {
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
                let rewrite = rewrite.read().unwrap();
                if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                    generic_error!(
                        "Using rewriting rule {}\n",
                        rewrite.name.as_deref().unwrap()
                    );
                }
                let url = rewrite
                    .url
                    .as_deref()
                    .map(|p| p.to_string_lossy().into_owned());
                // If `rewrite == entry`, deadlock occurs at next line `entry.write()....`.
                // Therefore, drop `rewirte` at this.
                drop(rewrite);
                entry.write().unwrap().depth -= 1;
                let mut url = url?;
                url.push_str(&sys_id[lenrewrite..]);
                return Some(url);
            }
            if have_delegate != 0 {
                let mut delegates: [Option<PathBuf>; MAX_DELEGATE] = [const { None }; MAX_DELEGATE];
                let mut nb_list = 0;

                // Assume the entries have been sorted by decreasing substring
                // matches when the list was produced.
                let mut cur = Some(entry.clone());
                'b: while let Some(now) = cur {
                    let mut n = now.write().unwrap();
                    cur = n.next.clone();
                    if matches!(n.typ, XmlCatalogEntryType::XmlCataDelegateSystem)
                        && sys_id.starts_with(n.name.as_deref().unwrap())
                    {
                        for i in 0..nb_list {
                            if n.url == delegates[i] {
                                continue 'b;
                            }
                        }
                        if n.children.is_none() {
                            n.fetch_xml_catalog_file();
                        }
                        if nb_list < MAX_DELEGATE {
                            delegates[nb_list] = n.url.clone();
                            nb_list += 1;
                        }

                        if let Some(children) = n.children.as_ref() {
                            if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                                generic_error!(
                                    "Trying system delegate {}\n",
                                    n.url.as_deref().unwrap().display()
                                );
                            }
                            if let Some(ret) = Self::list_xml_resolve(children, None, Some(sys_id))
                            {
                                // If `n == entry`, deadlock occurs at next line `entry.write()....`.
                                // Therefore, drop `n` at this.
                                drop(n);
                                entry.write().unwrap().depth -= 1;
                                return Some(ret);
                            }
                        }
                    }
                }
                // Apply the cut algorithm explained in 4/
                entry.write().unwrap().depth -= 1;
                return None;
            }
        }
        // Then tries 5/ 6/ if a public ID is provided
        if let Some(pub_id) = pub_id {
            let mut cur = Some(entry.clone());
            have_delegate = 0;
            while let Some(now) = cur {
                let n = now.write().unwrap();
                cur = n.next.clone();
                match n.typ {
                    XmlCatalogEntryType::XmlCataPublic => {
                        if Some(pub_id) == n.name.as_deref() {
                            if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                                generic_error!(
                                    "Found public match {}\n",
                                    n.name.as_deref().unwrap()
                                );
                            }
                            let res = n.url.as_deref().map(|u| u.to_string_lossy().into_owned());
                            // If `n == entry`, deadlock occurs at next line `entry.write()....`.
                            // Therefore, drop `n` at this.
                            drop(n);
                            entry.write().unwrap().depth -= 1;
                            return res;
                        }
                    }
                    XmlCatalogEntryType::XmlCataDelegatePublic => {
                        if pub_id.starts_with(n.name.as_deref().unwrap())
                            && matches!(n.prefer, XmlCatalogPrefer::Public)
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
            }
            if have_delegate != 0 {
                let mut delegates: [Option<PathBuf>; MAX_DELEGATE] = [const { None }; MAX_DELEGATE];
                let mut nb_list: usize = 0;

                // Assume the entries have been sorted by decreasing substring
                // matches when the list was produced.
                let mut cur = Some(entry.clone());
                'b: while let Some(now) = cur {
                    let mut n = now.write().unwrap();
                    cur = n.next.clone();
                    if n.typ == XmlCatalogEntryType::XmlCataDelegatePublic
                        && matches!(n.prefer, XmlCatalogPrefer::Public)
                        && pub_id.starts_with(n.name.as_deref().unwrap())
                    {
                        for i in 0..nb_list {
                            if n.url == delegates[i] {
                                continue 'b;
                            }
                        }
                        if n.children.is_none() {
                            n.fetch_xml_catalog_file();
                        }
                        if nb_list < MAX_DELEGATE {
                            delegates[nb_list] = n.url.clone();
                            nb_list += 1;
                        }

                        if let Some(children) = n.children.as_ref() {
                            if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                                generic_error!(
                                    "Trying public delegate {}\n",
                                    n.url.as_deref().unwrap().display()
                                );
                            }
                            if let Some(ret) = Self::list_xml_resolve(children, Some(pub_id), None)
                            {
                                // If `n == entry`, deadlock occurs at next line `entry.write()....`.
                                // Therefore, drop `n` at this.
                                drop(n);
                                entry.write().unwrap().depth -= 1;
                                return Some(ret);
                            }
                        }
                    }
                }
                // Apply the cut algorithm explained in 4/
                entry.write().unwrap().depth -= 1;
                return None;
            }
        }
        if have_next != 0 {
            let current_depth = entry.read().unwrap().depth;
            let mut cur = Some(entry.clone());
            while let Some(now) = cur {
                let mut n = now.write().unwrap();
                cur = n.next.clone();
                if n.typ == XmlCatalogEntryType::XmlCataNextCatalog {
                    if n.children.is_none() {
                        n.fetch_xml_catalog_file();
                    }
                    if let Some(children) = n.children.as_ref() {
                        if let Some(ret) = Self::list_xml_resolve(children, pub_id, sys_id) {
                            // If `n == entry`, deadlock occurs at next line `entry.write()....`.
                            // Therefore, drop `n` at this.
                            drop(n);
                            entry.write().unwrap().depth -= 1;
                            return Some(ret);
                        }
                        if current_depth > MAX_CATAL_DEPTH as i32 {
                            return None;
                        }
                    }
                }
            }
        }

        entry.write().unwrap().depth -= 1;
        None
    }

    /// Add an entry in the XML catalog, it may overwrite existing but different entries.
    ///
    /// Returns 0 if successful, -1 otherwise
    #[doc(alias = "xmlAddXMLCatalog")]
    unsafe fn add_xml_catalog(
        entry: &Arc<RwLock<Self>>,
        types: Option<&str>,
        orig: Option<&str>,
        replace: Option<&str>,
    ) -> i32 {
        let mut doregister = 0;

        let mut entry_lock = entry.write().unwrap();
        if !matches!(
            entry_lock.typ,
            XmlCatalogEntryType::XmlCataCatalog | XmlCatalogEntryType::XmlCataBrokenCatalog
        ) {
            return -1;
        }
        if entry_lock.children.is_none() {
            entry_lock.fetch_xml_catalog_file();
        }
        if entry_lock.children.is_none() {
            doregister = 1;
        }

        let typ: XmlCatalogEntryType = xml_get_xml_catalog_entry_type(types);
        if matches!(typ, XmlCatalogEntryType::XmlCataNone) {
            if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                if let Some(types) = types {
                    generic_error!("Failed to add unknown element {types} to catalog\n",);
                } else {
                    generic_error!("Failed to add unknown element (NULL) to catalog\n",);
                }
            }
            return -1;
        }

        let mut cur = entry_lock.children.clone();
        drop(entry_lock);
        // Might be a simple "update in place"
        if cur.is_some() {
            while let Some(now) = cur.clone() {
                let mut n = now.write().unwrap();
                if orig.is_some() && n.typ == typ && orig == n.name.as_deref() {
                    if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                        if let Some(types) = types {
                            generic_error!("Updating element {types} to catalog\n");
                        } else {
                            generic_error!("Updating element (NULL) to catalog\n",);
                        }
                    }
                    n.value = replace.map(|r| r.to_owned());
                    n.url = replace.map(PathBuf::from);
                    return 0;
                }
                let Some(next) = n.next.as_ref() else {
                    break;
                };
                cur = Some(next.clone());
            }
        }
        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            if let Some(types) = types {
                generic_error!("Adding element {types} to catalog\n");
            } else {
                generic_error!("Adding element (NULL) to catalog\n");
            }
        }
        let prefer = entry.read().unwrap().prefer;
        if let Some(cur) = cur {
            let new = xml_new_catalog_entry(typ, orig, replace, None, prefer, None);
            cur.write().unwrap().next = Some(new.node);
        } else {
            let new = xml_new_catalog_entry(typ, orig, replace, None, prefer, None);
            entry.write().unwrap().children = Some(new.node);
        }
        if doregister != 0 {
            entry.write().unwrap().typ = XmlCatalogEntryType::XmlCataCatalog;
            if let Some(url) = entry.read().unwrap().url.as_deref() {
                let files = XML_CATALOG_XMLFILES.read().unwrap();
                if let Some(cur) = files.get(url.to_string_lossy().as_ref()) {
                    let child = entry.read().unwrap().children.clone();
                    cur.write().unwrap().children = child;
                }
            }
        }

        0
    }

    /// Remove entries in the XML catalog where the value or the URI is equal to `value`.
    ///
    /// Returns the number of entries removed if successful, -1 otherwise
    #[doc(alias = "xmlDelXMLCatalog")]
    unsafe fn del_xml_catalog(entry: &Arc<RwLock<Self>>, value: &str) -> i32 {
        let ret = 0;

        let mut entry_lock = entry.write().unwrap();
        if !matches!(
            entry_lock.typ,
            XmlCatalogEntryType::XmlCataCatalog | XmlCatalogEntryType::XmlCataBrokenCatalog
        ) {
            return -1;
        }
        if entry_lock.children.is_none() {
            entry_lock.fetch_xml_catalog_file();
        }

        // Scan the children
        let mut cur = entry_lock.children.clone();
        while let Some(now) = cur {
            let mut n = now.write().unwrap();
            if (n.name.is_some() && Some(value) == n.name.as_deref())
                || Some(value) == n.value.as_deref()
            {
                if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                    if let Some(name) = n.name.as_deref() {
                        generic_error!("Removing element {name} from catalog\n");
                    } else {
                        generic_error!(
                            "Removing element {} from catalog\n",
                            n.value.as_deref().unwrap()
                        );
                    }
                }
                n.typ = XmlCatalogEntryType::XmlCataRemoved;
            }
            cur = n.next.clone();
        }
        ret
    }

    /// Serializes a Catalog entry, called by xmlDumpXMLCatalog and recursively for group entries
    #[doc(alias = "xmlDumpXMLCatalogNode")]
    #[cfg(feature = "libxml_output")]
    unsafe fn dump_xml_catalog_node(
        entry: &Arc<RwLock<Self>>,
        catalog: *mut XmlNode,
        doc: *mut XmlDoc,
        ns: *mut XmlNs,
        cgroup: Option<&Arc<RwLock<Self>>>,
    ) {
        use crate::tree::NodeCommon;

        let mut node: *mut XmlNode;
        // add all the catalog entries
        let mut cur = Some(entry.clone());
        while let Some(now) = cur {
            let n = now.read().unwrap();
            if n.group
                .as_ref()
                .zip(cgroup)
                .filter(|(ng, cg)| Arc::ptr_eq(ng, cg))
                .is_some()
            {
                match n.typ {
                    XmlCatalogEntryType::XmlCataRemoved => {}
                    XmlCatalogEntryType::XmlCataBrokenCatalog
                    | XmlCatalogEntryType::XmlCataCatalog => {
                        if Arc::ptr_eq(&now, entry) {
                            cur = n.children.clone();
                            continue;
                        }
                    }
                    XmlCatalogEntryType::XmlCataNextCatalog => {
                        node = xml_new_doc_node(doc, ns, "nextCatalog", null_mut());
                        (*node).set_prop("catalog", n.value.as_deref());
                        (*catalog).add_child(node);
                    }
                    XmlCatalogEntryType::XmlCataNone => {}
                    XmlCatalogEntryType::XmlCataGroup => {
                        node = xml_new_doc_node(doc, ns, "group", null_mut());
                        (*node).set_prop("id", n.name.as_deref());
                        if let Some(value) = n.value.as_deref() {
                            if let Some(xns) =
                                (*node).search_ns_by_href(doc, XML_XML_NAMESPACE.to_str().unwrap())
                            {
                                (*node).set_ns_prop(xns.as_ptr(), "base", Some(value));
                            }
                        }
                        match n.prefer {
                            XmlCatalogPrefer::None => {}
                            XmlCatalogPrefer::Public => {
                                (*node).set_prop("prefer", Some("public"));
                            }
                            XmlCatalogPrefer::System => {
                                (*node).set_prop("prefer", Some("system"));
                            }
                        }
                        if let Some(next) = n.next.as_ref() {
                            Self::dump_xml_catalog_node(next, node, doc, ns, Some(&now));
                        }
                        (*catalog).add_child(node);
                    }
                    XmlCatalogEntryType::XmlCataPublic => {
                        node = xml_new_doc_node(doc, ns, "public", null_mut());
                        (*node).set_prop("publicId", n.name.as_deref());
                        (*node).set_prop("uri", n.value.as_deref());
                        (*catalog).add_child(node);
                    }
                    XmlCatalogEntryType::XmlCataSystem => {
                        node = xml_new_doc_node(doc, ns, "system", null_mut());
                        (*node).set_prop("systemId", n.name.as_deref());
                        (*node).set_prop("uri", n.value.as_deref());
                        (*catalog).add_child(node);
                    }
                    XmlCatalogEntryType::XmlCataRewriteSystem => {
                        node = xml_new_doc_node(doc, ns, "rewriteSystem", null_mut());
                        (*node).set_prop("systemIdStartString", n.name.as_deref());
                        (*node).set_prop("rewritePrefix", n.value.as_deref());
                        (*catalog).add_child(node);
                    }
                    XmlCatalogEntryType::XmlCataDelegatePublic => {
                        node = xml_new_doc_node(doc, ns, "delegatePublic", null_mut());
                        (*node).set_prop("publicIdStartString", n.name.as_deref());
                        (*node).set_prop("catalog", n.value.as_deref());
                        (*catalog).add_child(node);
                    }
                    XmlCatalogEntryType::XmlCataDelegateSystem => {
                        node = xml_new_doc_node(doc, ns, "delegateSystem", null_mut());
                        (*node).set_prop("systemIdStartString", n.name.as_deref());
                        (*node).set_prop("catalog", n.value.as_deref());
                        (*catalog).add_child(node);
                    }
                    XmlCatalogEntryType::XmlCataURI => {
                        node = xml_new_doc_node(doc, ns, "uri", null_mut());
                        (*node).set_prop("name", n.name.as_deref());
                        (*node).set_prop("uri", n.value.as_deref());
                        (*catalog).add_child(node);
                    }
                    XmlCatalogEntryType::XmlCataRewriteURI => {
                        node = xml_new_doc_node(doc, ns, "rewriteURI", null_mut());
                        (*node).set_prop("uriStartString", n.name.as_deref());
                        (*node).set_prop("rewritePrefix", n.value.as_deref());
                        (*catalog).add_child(node);
                    }
                    XmlCatalogEntryType::XmlCataDelegateURI => {
                        node = xml_new_doc_node(doc, ns, "delegateURI", null_mut());
                        (*node).set_prop("uriStartString", n.name.as_deref());
                        (*node).set_prop("catalog", n.value.as_deref());
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
            cur = n.next.clone();
        }
    }
}

pub type XmlCatalogEntryPtr = *mut XmlCatalogEntry;
#[repr(C)]
pub struct XmlCatalogEntry {
    node: Arc<RwLock<CatalogEntryListNode>>,
}

impl XmlCatalogEntry {
    /// Add the new entry to the catalog list
    #[doc(alias = "xmlCatalogAddLocal")]
    pub fn new(url: &str) -> Self {
        if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
            xml_initialize_catalog();
        }

        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            generic_error!("Adding document catalog {url}\n");
        }

        xml_new_catalog_entry(
            XmlCatalogEntryType::XmlCataCatalog,
            None,
            Some(url),
            None,
            *XML_CATALOG_DEFAULT_PREFER.read().unwrap(),
            None,
        )
    }

    /// Fetch and parse the subcatalog referenced by an entry
    ///
    /// Returns 0 in case of success, -1 otherwise
    #[doc(alias = "xmlFetchXMLCatalogFile")]
    unsafe fn fetch_xml_catalog_file(&mut self) -> i32 {
        self.node.write().unwrap().fetch_xml_catalog_file()
    }

    /// Do a complete resolution lookup of an URI using a
    /// document's private catalog list
    ///
    /// Returns the URI of the resource or null_mut() if not found,
    /// it must be freed by the caller.
    #[doc(alias = "xmlCatalogLocalResolveURI")]
    pub unsafe fn local_resolve_uri(&mut self, uri: &str) -> Option<String> {
        if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
            xml_initialize_catalog();
        }

        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            generic_error!("Resolve URI {uri}\n");
        }

        self.list_xml_resolve_uri(uri)
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
    ) -> Option<String> {
        if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
            xml_initialize_catalog();
        }

        if pub_id.is_none() && sys_id.is_none() {
            return None;
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

        self.list_xml_resolve(pub_id, sys_id)
    }

    /// Do a complete resolution lookup of an URI for a list of catalogs
    ///
    /// Implements (or tries to) 7.2. URI Resolution
    /// from http://www.oasis-open.org/committees/entity/spec-2001-08-06.html
    ///
    /// Returns the URI of the resource or null_mut() if not found
    #[doc(alias = "xmlCatalogListXMLResolveURI")]
    unsafe fn list_xml_resolve_uri(&mut self, uri: &str) -> Option<String> {
        CatalogEntryListNode::list_xml_resolve_uri(&self.node, uri)
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
    ) -> Option<String> {
        CatalogEntryListNode::list_xml_resolve(&self.node, pub_id, sys_id)
    }

    /// Do a complete resolution lookup of an External Identifier for a list of catalog entries.
    ///
    /// Implements (or tries to) 7.2.2. URI Resolution
    /// from http://www.oasis-open.org/committees/entity/spec-2001-08-06.html
    ///
    /// Returns the URI of the resource or null_mut() if not found
    #[doc(alias = "xmlCatalogXMLResolveURI")]
    unsafe fn xml_resolve_uri(&mut self, uri: &str) -> Option<String> {
        CatalogEntryListNode::xml_resolve_uri(&self.node, uri)
    }

    /// Do a complete resolution lookup of an External Identifier for a list of catalog entries.
    ///
    /// Implements (or tries to) 7.1. External Identifier Resolution
    /// from http://www.oasis-open.org/committees/entity/spec-2001-08-06.html
    ///
    /// Returns the URI of the resource or null_mut() if not found
    #[doc(alias = "xmlCatalogXMLResolve")]
    unsafe fn xml_resolve(&mut self, pub_id: Option<&str>, sys_id: Option<&str>) -> Option<String> {
        CatalogEntryListNode::xml_resolve(&self.node, pub_id, sys_id)
    }

    /// Add the new entry to the catalog list
    ///
    /// # Note
    /// In original libxml2, you can create a new catalog by passing a NULL pointer to this method,
    /// but it is not possible in this crate.  
    /// You need to use `XmlCatalogEntry::new` instead.
    #[doc(alias = "xmlCatalogAddLocal")]
    pub fn add_local(&mut self, url: &str) {
        if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
            xml_initialize_catalog();
        }

        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            generic_error!("Adding document catalog {url}\n");
        }

        let add = xml_new_catalog_entry(
            XmlCatalogEntryType::XmlCataCatalog,
            None,
            Some(url),
            None,
            *XML_CATALOG_DEFAULT_PREFER.read().unwrap(),
            None,
        );

        let mut prev = None;
        let mut cur = Some(self.node.clone());
        while let Some(now) = cur.clone() {
            (prev, cur) = (cur, now.read().unwrap().next.clone());
        }
        prev.unwrap().write().unwrap().next = Some(add.node.clone());
    }

    /// Add an entry in the XML catalog, it may overwrite existing but different entries.
    ///
    /// Returns 0 if successful, -1 otherwise
    #[doc(alias = "xmlAddXMLCatalog")]
    unsafe fn add_xml_catalog(
        &mut self,
        types: Option<&str>,
        orig: Option<&str>,
        replace: Option<&str>,
    ) -> i32 {
        CatalogEntryListNode::add_xml_catalog(&self.node, types, orig, replace)
    }

    /// Remove entries in the XML catalog where the value or the URI is equal to `value`.
    ///
    /// Returns the number of entries removed if successful, -1 otherwise
    #[doc(alias = "xmlDelXMLCatalog")]
    unsafe fn del_xml_catalog(&mut self, value: &str) -> i32 {
        CatalogEntryListNode::del_xml_catalog(&self.node, value)
    }

    /// Serialize an SGML Catalog entry
    #[doc(alias = "xmlCatalogDumpEntry")]
    #[cfg(feature = "libxml_output")]
    fn dump_entry<'a>(&self, out: &mut (impl Write + 'a)) {
        let entry = self.node.read().unwrap();
        match entry.typ {
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
        match entry.typ {
            XmlCatalogEntryType::SgmlCataEntity
            | XmlCatalogEntryType::SgmlCataPentity
            | XmlCatalogEntryType::SgmlCataDoctype
            | XmlCatalogEntryType::SgmlCataLinktype
            | XmlCatalogEntryType::SgmlCataNotation => {
                write!(out, "{}", entry.name.as_deref().unwrap()).ok();
            }
            XmlCatalogEntryType::SgmlCataPublic
            | XmlCatalogEntryType::SgmlCataSystem
            | XmlCatalogEntryType::SgmlCataSGMLDecl
            | XmlCatalogEntryType::SgmlCataDocument
            | XmlCatalogEntryType::SgmlCataCatalog
            | XmlCatalogEntryType::SgmlCataBase
            | XmlCatalogEntryType::SgmlCataDelegate => {
                write!(out, "\"{}\"", entry.name.as_deref().unwrap()).ok();
            }
            _ => {}
        }
        match entry.typ {
            XmlCatalogEntryType::SgmlCataEntity
            | XmlCatalogEntryType::SgmlCataPentity
            | XmlCatalogEntryType::SgmlCataDoctype
            | XmlCatalogEntryType::SgmlCataLinktype
            | XmlCatalogEntryType::SgmlCataNotation
            | XmlCatalogEntryType::SgmlCataPublic
            | XmlCatalogEntryType::SgmlCataSystem
            | XmlCatalogEntryType::SgmlCataDelegate => {
                write!(out, " \"{}\"", entry.value.as_deref().unwrap()).ok();
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
        catalog: *mut XmlNode,
        doc: *mut XmlDoc,
        ns: *mut XmlNs,
        cgroup: Option<&Self>,
    ) {
        CatalogEntryListNode::dump_xml_catalog_node(
            &self.node,
            catalog,
            doc,
            ns,
            cgroup.map(|c| &c.node),
        )
    }

    #[doc(alias = "xmlDumpXMLCatalog")]
    #[cfg(feature = "libxml_output")]
    unsafe fn dump_xml_catalog<'a>(&self, out: impl Write + 'a) -> i32 {
        use crate::{
            io::XmlOutputBuffer,
            tree::{NodeCommon, XmlNs, XmlNsPtr},
        };

        // Rebuild a catalog
        let doc: *mut XmlDoc = xml_new_doc(None);
        if doc.is_null() {
            return -1;
        }
        let dtd = xml_new_dtd(
            doc,
            Some("catalog"),
            Some("-//OASIS//DTD Entity Resolution XML Catalog V1.0//EN"),
            Some("http://www.oasis-open.org/committees/entity/release/1.0/catalog.dtd"),
        );

        (*doc).add_child(dtd.map_or(null_mut(), |dtd| dtd.as_ptr()) as _);

        let ns: *mut XmlNs = xml_new_ns(null_mut(), XML_CATALOGS_NAMESPACE.as_ptr() as _, None);
        if ns.is_null() {
            xml_free_doc(doc);
            return -1;
        }
        let catalog: *mut XmlNode = xml_new_doc_node(doc, ns, "catalog", null_mut());
        if catalog.is_null() {
            xml_free_ns(XmlNsPtr::from_raw(ns).unwrap().unwrap());
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

impl Clone for XmlCatalogEntry {
    fn clone(&self) -> Self {
        Self {
            node: self.node.clone(),
        }
    }
}

impl Default for XmlCatalogEntry {
    fn default() -> Self {
        Self {
            node: Arc::new(RwLock::new(CatalogEntryListNode::default())),
        }
    }
}

/// create a new Catalog, this type is shared both by XML and SGML catalogs,
/// but the acceptable types values differs.
///
/// Returns the xmlCatalogPtr or null_mut() in case of error
#[doc(alias = "xmlCreateNewCatalog")]
fn xml_create_new_catalog(typ: XmlCatalogType, prefer: XmlCatalogPrefer) -> XmlCatalog {
    let mut ret = XmlCatalog {
        typ,
        prefer,
        ..Default::default()
    };
    if matches!(ret.typ, XmlCatalogType::XmlSGMLCatalogType) {
        ret.sgml = HashMap::new();
    }
    ret
}

/// create a new Catalog.
///
/// Returns the xmlCatalogPtr or null_mut() in case of error
#[doc(alias = "xmlNewCatalog")]
pub fn xml_new_catalog(sgml: bool) -> XmlCatalog {
    if sgml {
        xml_create_new_catalog(
            XmlCatalogType::XmlSGMLCatalogType,
            *XML_CATALOG_DEFAULT_PREFER.read().unwrap(),
        )
    } else {
        xml_create_new_catalog(
            XmlCatalogType::XmlXMLCatalogType,
            *XML_CATALOG_DEFAULT_PREFER.read().unwrap(),
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
fn xml_new_catalog_entry(
    typ: XmlCatalogEntryType,
    name: Option<&str>,
    value: Option<&str>,
    mut url: Option<PathBuf>,
    prefer: XmlCatalogPrefer,
    group: Option<XmlCatalogEntry>,
) -> XmlCatalogEntry {
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
    if url.is_none() {
        url = value.map(PathBuf::from);
    }
    let node = CatalogEntryListNode {
        next: None,
        parent: None,
        children: None,
        typ,
        name: name.map(|n| n.into_owned()),
        value: value.map(|v| v.to_owned()),
        url,
        prefer,
        dealloc: 0,
        depth: 0,
        group: group.map(|g| g.node.clone()),
    };
    XmlCatalogEntry {
        node: Arc::new(RwLock::new(node)),
    }
}

/// Load the catalog and build the associated data structures.  
/// This can be either an XML Catalog or an SGML Catalog.  
/// It will recurse in SGML CATALOG entries.  
/// On the other hand XML Catalogs are not handled recursively.
///
/// Returns the catalog parsed or null_mut() in case of error
#[doc(alias = "xmlLoadACatalog")]
pub unsafe fn xml_load_a_catalog(filename: impl AsRef<Path>) -> Option<XmlCatalog> {
    let filename = filename.as_ref();
    let content = xml_load_file_content(filename)?;

    let mut first = &content[..];

    while !first.is_empty()
        && first[0] != b'-'
        && first[0] != b'<'
        && !((first[0] >= b'A' && first[0] <= b'Z') || (first[0] >= b'a' && first[0] <= b'z'))
    {
        first = &first[1..];
    }

    if first.first() != Some(&b'<') {
        let mut catal = xml_create_new_catalog(
            XmlCatalogType::XmlSGMLCatalogType,
            *XML_CATALOG_DEFAULT_PREFER.read().unwrap(),
        );
        let ret = catal.parse_sgml_catalog(&content, filename, 0);
        if ret < 0 {
            return None;
        }
        Some(catal)
    } else {
        let mut catal = xml_create_new_catalog(
            XmlCatalogType::XmlXMLCatalogType,
            *XML_CATALOG_DEFAULT_PREFER.read().unwrap(),
        );
        catal.xml = Some(xml_new_catalog_entry(
            XmlCatalogEntryType::XmlCataCatalog,
            None,
            None,
            Some(filename.to_owned()),
            *XML_CATALOG_DEFAULT_PREFER.read().unwrap(),
            None,
        ));
        Some(catal)
    }
}

/// Load an SGML super catalog. It won't expand CATALOG or DELEGATE references.  
/// This is only needed for manipulating SGML Super Catalogs
/// like adding and removing CATALOG or DELEGATE entries.
///
/// Returns the catalog parsed or null_mut() in case of error
#[doc(alias = "xmlLoadSGMLSuperCatalog")]
pub unsafe fn xml_load_sgml_super_catalog(filename: impl AsRef<Path>) -> Option<XmlCatalog> {
    let filename = filename.as_ref();
    let content = xml_load_file_content(filename)?;

    let mut catal = xml_create_new_catalog(
        XmlCatalogType::XmlSGMLCatalogType,
        *XML_CATALOG_DEFAULT_PREFER.read().unwrap(),
    );

    let ret = catal.parse_sgml_catalog(&content, filename, 1);
    if ret < 0 {
        return None;
    }
    Some(catal)
}

/// Finishes the examination of an XML tree node of a catalog and build
/// a Catalog entry from it.
///
/// Returns the new Catalog entry node or null_mut() in case of error.
#[doc(alias = "xmlParseXMLCatalogOneNode")]
unsafe fn xml_parse_xml_catalog_one_node(
    cur: *mut XmlNode,
    typ: XmlCatalogEntryType,
    name: &str,
    attr_name: Option<&str>,
    uri_attr_name: &str,
    prefer: XmlCatalogPrefer,
    cgroup: Option<XmlCatalogEntry>,
) -> Option<XmlCatalogEntry> {
    let mut ok = true;
    let mut name_value = None;

    if let Some(attr_name) = attr_name {
        name_value = (*cur).get_prop(attr_name);
        if name_value.is_none() {
            xml_catalog_err!(
                null_mut(),
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
            null_mut(),
            cur,
            XmlParserErrors::XmlCatalogMissingAttr,
            "{} entry lacks '{}'\n",
            name,
            uri_attr_name,
        );
        return None;
    };
    if !ok {
        return None;
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
        Some(xml_new_catalog_entry(
            typ,
            name_value.as_deref(),
            Some(&uri_value),
            Some(PathBuf::from(url)),
            prefer,
            cgroup,
        ))
    } else {
        xml_catalog_err!(
            null_mut(),
            cur,
            XmlParserErrors::XmlCatalogEntryBroken,
            "{} entry '{}' broken ?: {}\n",
            name,
            uri_attr_name,
            uri_value,
        );
        None
    }
}

/// Examines an XML tree node of a catalog and build a Catalog entry from it adding it to its parent.
/// The examination can be recursive.
#[doc(alias = "xmlParseXMLCatalogNode")]
unsafe fn xml_parse_xml_catalog_node(
    cur: *mut XmlNode,
    mut prefer: XmlCatalogPrefer,
    parent: Option<XmlCatalogEntry>,
    cgroup: Option<XmlCatalogEntry>,
) {
    if cur.is_null() {
        return;
    }
    let entry = if (*cur).name().as_deref() == Some("group") {
        let mut pref: XmlCatalogPrefer = XmlCatalogPrefer::None;

        if let Some(prop) = (*cur).get_prop("prefer") {
            if prop == "system" {
                prefer = XmlCatalogPrefer::System;
            } else if prop == "public" {
                prefer = XmlCatalogPrefer::Public;
            } else {
                xml_catalog_err!(
                    parent.as_ref().map_or(null(), |p| Arc::as_ptr(&p.node))
                        as *mut RwLock<CatalogEntryListNode>,
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
        Some(xml_new_catalog_entry(
            XmlCatalogEntryType::XmlCataGroup,
            prop.as_deref(),
            base.as_deref(),
            None,
            pref,
            cgroup,
        ))
    } else if (*cur).name().as_deref() == Some("public") {
        xml_parse_xml_catalog_one_node(
            cur,
            XmlCatalogEntryType::XmlCataPublic,
            "public",
            Some("publicId"),
            "uri",
            prefer,
            cgroup,
        )
    } else if (*cur).name().as_deref() == Some("system") {
        xml_parse_xml_catalog_one_node(
            cur,
            XmlCatalogEntryType::XmlCataSystem,
            "system",
            Some("systemId"),
            "uri",
            prefer,
            cgroup,
        )
    } else if (*cur).name().as_deref() == Some("rewriteSystem") {
        xml_parse_xml_catalog_one_node(
            cur,
            XmlCatalogEntryType::XmlCataRewriteSystem,
            "rewriteSystem",
            Some("systemIdStartString"),
            "rewritePrefix",
            prefer,
            cgroup,
        )
    } else if (*cur).name().as_deref() == Some("delegatePublic") {
        xml_parse_xml_catalog_one_node(
            cur,
            XmlCatalogEntryType::XmlCataDelegatePublic,
            "delegatePublic",
            Some("publicIdStartString"),
            "catalog",
            prefer,
            cgroup,
        )
    } else if (*cur).name().as_deref() == Some("delegateSystem") {
        xml_parse_xml_catalog_one_node(
            cur,
            XmlCatalogEntryType::XmlCataDelegateSystem,
            "delegateSystem",
            Some("systemIdStartString"),
            "catalog",
            prefer,
            cgroup,
        )
    } else if (*cur).name().as_deref() == Some("uri") {
        xml_parse_xml_catalog_one_node(
            cur,
            XmlCatalogEntryType::XmlCataURI,
            "uri",
            Some("name"),
            "uri",
            prefer,
            cgroup,
        )
    } else if (*cur).name().as_deref() == Some("rewriteURI") {
        xml_parse_xml_catalog_one_node(
            cur,
            XmlCatalogEntryType::XmlCataRewriteURI,
            "rewriteURI",
            Some("uriStartString"),
            "rewritePrefix",
            prefer,
            cgroup,
        )
    } else if (*cur).name().as_deref() == Some("delegateURI") {
        xml_parse_xml_catalog_one_node(
            cur,
            XmlCatalogEntryType::XmlCataDelegateURI,
            "delegateURI",
            Some("uriStartString"),
            "catalog",
            prefer,
            cgroup,
        )
    } else if (*cur).name().as_deref() == Some("nextCatalog") {
        xml_parse_xml_catalog_one_node(
            cur,
            XmlCatalogEntryType::XmlCataNextCatalog,
            "nextCatalog",
            None,
            "catalog",
            prefer,
            cgroup,
        )
    } else {
        None
    };
    if let Some(entry) = entry {
        if let Some(parent) = parent.as_ref() {
            entry.node.write().unwrap().parent = Some(Arc::downgrade(&parent.node));
            if parent.node.read().unwrap().children.is_none() {
                parent.node.write().unwrap().children = Some(entry.node.clone());
            } else {
                let mut prev = None;
                let mut cur = parent.node.write().unwrap().children.clone();
                while let Some(now) = cur.clone() {
                    (prev, cur) = (cur, now.read().unwrap().next.clone());
                }
                prev.unwrap().write().unwrap().next = Some(entry.node.clone());
            }
        }
        if entry.node.read().unwrap().typ == XmlCatalogEntryType::XmlCataGroup {
            // Recurse to propagate prefer to the subtree
            // (xml:base handling is automated)
            xml_parse_xml_catalog_node_list(
                (*cur).children().map_or(null_mut(), |c| c.as_ptr()),
                prefer,
                parent,
                Some(entry),
            );
        }
    }
}

/// Examines a list of XML sibling nodes of a catalog and build
/// a list of Catalog entry from it adding it to the parent.
/// The examination will recurse to examine node subtrees.
#[doc(alias = "xmlParseXMLCatalogNodeList")]
unsafe fn xml_parse_xml_catalog_node_list(
    mut cur: *mut XmlNode,
    prefer: XmlCatalogPrefer,
    parent: Option<XmlCatalogEntry>,
    cgroup: Option<XmlCatalogEntry>,
) {
    while !cur.is_null() {
        if !(*cur).ns.is_null()
            && !(*(*cur).ns).href.is_null()
            && xml_str_equal((*(*cur).ns).href, XML_CATALOGS_NAMESPACE.as_ptr() as _)
        {
            xml_parse_xml_catalog_node(cur, prefer, parent.clone(), cgroup.clone());
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
) -> Option<Arc<RwLock<CatalogEntryListNode>>> {
    let mut cur: *mut XmlNode;

    let doc: *mut XmlDoc = xml_parse_catalog_file(filename);
    if doc.is_null() {
        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            generic_error!("Failed to parse catalog {filename}\n");
        }
        return None;
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
        let parent = xml_new_catalog_entry(
            XmlCatalogEntryType::XmlCataCatalog,
            None,
            Some(filename),
            None,
            prefer,
            None,
        );

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
        xml_parse_xml_catalog_node_list(cur, prefer, Some(parent.clone()), None);
        xml_free_doc(doc);
        Some(parent.node)
    } else {
        xml_catalog_err!(
            null_mut(),
            doc,
            XmlParserErrors::XmlCatalogNotCatalog,
            "File {} is not an XML Catalog\n",
            filename,
        );
        xml_free_doc(doc);
        None
    }
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
fn xml_catalog_get_sgml_public(
    catal: &HashMap<String, XmlCatalogEntry>,
    pub_id: &str,
) -> Option<PathBuf> {
    let mut pub_id = Cow::Borrowed(pub_id);
    if let Some(normid) = normalize_public(pub_id.as_bytes()) {
        if normid.is_empty() {
            return None;
        }
        pub_id = Cow::Owned(String::from_utf8(normid).ok()?);
    }

    let entry = catal.get(pub_id.as_ref())?;
    if entry.node.read().unwrap().typ == XmlCatalogEntryType::SgmlCataPublic {
        return entry
            .node
            .read()
            .unwrap()
            .url
            .as_deref()
            .map(|u| u.to_owned());
    }
    None
}

/// Try to lookup the catalog local reference for a system ID.
///
/// Returns the local resource if found or null_mut() otherwise.
#[doc(alias = "xmlCatalogGetSGMLSystem")]
fn xml_catalog_get_sgml_system(
    catal: &HashMap<String, XmlCatalogEntry>,
    sys_id: &str,
) -> Option<PathBuf> {
    let entry = catal.get(sys_id)?;
    if entry.node.read().unwrap().typ == XmlCatalogEntryType::SgmlCataSystem {
        return entry
            .node
            .read()
            .unwrap()
            .url
            .as_deref()
            .map(|u| u.to_owned());
    }
    None
}

/// Free the memory allocated to a Catalog
#[doc(alias = "xmlFreeCatalog")]
pub unsafe fn xml_free_catalog(catal: XmlCatalogPtr) {
    if catal.is_null() {
        return;
    }
    (*catal).xml = None;
    (*catal).sgml.clear();
    drop_in_place(catal);
    xml_free(catal as _);
}

/// Do the catalog initialization only of global data, doesn't try to load
/// any catalog actually.
/// this function is not thread safe, catalog initialization should
/// preferably be done once at startup
#[doc(alias = "xmlInitializeCatalogData")]
fn xml_initialize_catalog_data() {
    let is_initialized = XML_CATALOG_INITIALIZED.load(Ordering::Acquire);
    if is_initialized {
        return;
    }

    if std::env::var_os("XML_DEBUG_CATALOG").is_some() {
        XML_DEBUG_CATALOGS.store(1, Ordering::Release);
    }

    XML_CATALOG_INITIALIZED.store(true, Ordering::Release);
}

/// Do the catalog initialization.  
/// this function is not thread safe, catalog initialization should
/// preferably be done once at startup
#[doc(alias = "xmlInitializeCatalog")]
pub fn xml_initialize_catalog() {
    let is_initialized = XML_CATALOG_INITIALIZED.load(Ordering::Acquire);
    if is_initialized {
        return;
    }

    xml_initialize_catalog_data();
    let mut default_catalog = XML_DEFAULT_CATALOG.write().unwrap();

    if std::env::var_os("XML_DEBUG_CATALOG").is_some() {
        XML_DEBUG_CATALOGS.store(1, Ordering::Release);
    }

    if default_catalog.is_none() {
        let catalogs = std::env::var("XML_CATALOG_FILES")
            .ok()
            .unwrap_or_else(|| XML_XML_DEFAULT_CATALOG.to_owned());

        let mut catal = xml_create_new_catalog(
            XmlCatalogType::XmlXMLCatalogType,
            *XML_CATALOG_DEFAULT_PREFER.read().unwrap(),
        );
        let mut next = None::<XmlCatalogEntry>;
        // the XML_CATALOG_FILES envvar is allowed to contain a space-separated list of entries.
        for path in catalogs.split(|b: char| xml_is_blank_char(b as u32)) {
            if !path.is_empty() {
                let path = PathBuf::from(path);
                let new = xml_new_catalog_entry(
                    XmlCatalogEntryType::XmlCataCatalog,
                    None,
                    None,
                    Some(path),
                    *XML_CATALOG_DEFAULT_PREFER.read().unwrap(),
                    None,
                );
                if let Some(nt) = next {
                    nt.node.write().unwrap().next = Some(new.node.clone());
                } else {
                    catal.xml = Some(new.clone());
                }
                next = Some(new);
            }
        }
        *default_catalog = Some(catal);
    }
}

/// Load the catalog and makes its definitions effective for the default
/// external entity loader. It will recurse in SGML CATALOG entries.
/// this function is not thread safe, catalog initialization should
/// preferably be done once at startup
///
/// Returns 0 in case of success -1 in case of error
#[doc(alias = "xmlLoadCatalog")]
pub unsafe fn xml_load_catalog(filename: impl AsRef<Path>) -> i32 {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog_data();
    }

    let filename = filename.as_ref();
    let mut default_catalog = XML_DEFAULT_CATALOG.write().unwrap();
    let Some(default_catalog) = default_catalog.as_mut() else {
        let Some(catal) = xml_load_a_catalog(filename) else {
            return -1;
        };
        *default_catalog = Some(catal);
        return 0;
    };

    default_catalog.expand_catalog(filename)
}

/// Load the catalogs and makes their definitions effective for the default
/// external entity loader.
///
/// This function is not thread safe, catalog initialization should
/// preferably be done once at startup
#[doc(alias = "xmlLoadCatalogs")]
pub unsafe fn xml_load_catalogs(pathss: &str) {
    for path in split_paths(pathss) {
        xml_load_catalog(path);
    }
}

/// Free up all the memory associated with catalogs
#[doc(alias = "xmlCatalogCleanup")]
pub unsafe fn xml_catalog_cleanup() {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Acquire) {
        return;
    }

    let mut files = XML_CATALOG_XMLFILES.write().unwrap();
    if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
        generic_error!("Catalogs cleanup\n");
    }
    files.clear();
    XML_DEFAULT_CATALOG.write().unwrap().take();
    XML_DEBUG_CATALOGS.store(0, Ordering::Release);
    XML_CATALOG_INITIALIZED.store(false, Ordering::Release);
}

/// Dump all the global catalog content to the given file.
#[doc(alias = "xmlCatalogDump")]
#[cfg(feature = "libxml_output")]
pub unsafe fn xml_catalog_dump<'a>(out: impl Write + 'a) {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }
    let mut lock = XML_DEFAULT_CATALOG.write().unwrap();
    if let Some(catalog) = lock.as_mut() {
        catalog.dump(out);
    }
}

/// Do a complete resolution lookup of an External Identifier
///
/// Returns the URI of the resource or null_mut() if not found,
/// it must be freed by the caller.
#[doc(alias = "xmlCatalogResolve")]
pub unsafe fn xml_catalog_resolve(pub_id: Option<&str>, sys_id: Option<&str>) -> Option<String> {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    let mut lock = XML_DEFAULT_CATALOG.write().unwrap();
    if let Some(catalog) = lock.as_mut() {
        catalog.resolve(pub_id, sys_id)
    } else {
        None
    }
}

/// Try to lookup the catalog resource for a system ID
///
/// Returns the resource if found or null_mut() otherwise,
/// the value returned must be freed by the caller.
#[doc(alias = "xmlCatalogResolveSystem")]
pub unsafe fn xml_catalog_resolve_system(sys_id: &str) -> Option<String> {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    let mut lock = XML_DEFAULT_CATALOG.write().unwrap();
    if let Some(catalog) = lock.as_mut() {
        catalog.resolve_system(sys_id)
    } else {
        None
    }
}

/// Try to lookup the catalog reference associated to a public ID
///
/// Returns the resource if found or null_mut() otherwise,
/// the value returned must be freed by the caller.
#[doc(alias = "xmlCatalogResolvePublic")]
pub unsafe fn xml_catalog_resolve_public(pub_id: &str) -> Option<String> {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    let mut lock = XML_DEFAULT_CATALOG.write().unwrap();
    if let Some(catalog) = lock.as_mut() {
        catalog.resolve_public(pub_id)
    } else {
        None
    }
}

/// Do a complete resolution lookup of an URI
///
/// Returns the URI of the resource or null_mut() if not found,
/// it must be freed by the caller.
#[doc(alias = "xmlCatalogResolveURI")]
pub unsafe fn xml_catalog_resolve_uri(uri: &str) -> Option<String> {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    let mut lock = XML_DEFAULT_CATALOG.write().unwrap();
    if let Some(catalog) = lock.as_mut() {
        catalog.resolve_uri(uri)
    } else {
        None
    }
}

/// Add an entry in the catalog, it may overwrite existing but
/// different entries.
/// If called before any other catalog routine, allows to override the
/// default shared catalog put in place by `xmlInitializeCatalog()`.
///
/// Returns 0 if successful, -1 otherwise
#[doc(alias = "xmlCatalogAdd")]
pub unsafe fn xml_catalog_add(typ: Option<&str>, orig: Option<&str>, replace: Option<&str>) -> i32 {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog_data();
    }

    // Specific case where one want to override the default catalog
    // put in place by xmlInitializeCatalog();
    let mut default_catalog = XML_DEFAULT_CATALOG.write().unwrap();
    let Some(default_catalog) = default_catalog.as_mut() else {
        if typ == Some("catalog") {
            let mut new = xml_create_new_catalog(
                XmlCatalogType::XmlXMLCatalogType,
                *XML_CATALOG_DEFAULT_PREFER.read().unwrap(),
            );

            new.xml = Some(xml_new_catalog_entry(
                XmlCatalogEntryType::XmlCataCatalog,
                None,
                orig,
                None,
                *XML_CATALOG_DEFAULT_PREFER.read().unwrap(),
                None,
            ));
            *default_catalog = Some(new);
            return 0;
        } else {
            return -1;
        }
    };

    default_catalog.add(typ, orig, replace)
}

/// Remove an entry from the catalog
///
/// Returns the number of entries removed if successful, -1 otherwise
#[doc(alias = "xmlCatalogRemove")]
pub unsafe fn xml_catalog_remove(value: &str) -> i32 {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    let mut default_catalog = XML_DEFAULT_CATALOG.write().unwrap();
    if let Some(default_catalog) = default_catalog.as_mut() {
        default_catalog.remove(value)
    } else {
        -1
    }
}

/// Parse an XML file and build a tree.
/// It's like `xmlParseFile()` except it bypass all catalog lookups.
///
/// Returns the resulting document tree or null_mut() in case of error
#[doc(alias = "xmlParseCatalogFile")]
pub unsafe fn xml_parse_catalog_file(filename: &str) -> *mut XmlDoc {
    let ret: *mut XmlDoc;

    let ctxt = xml_new_parser_ctxt();
    if ctxt.is_null() {
        xml_catalog_err_memory("allocating parser context");
        return null_mut();
    }

    let Some(buf) = XmlParserInputBuffer::from_uri(filename, XmlCharEncoding::None) else {
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    };

    let input_stream: XmlParserInputPtr = xml_new_input_stream(Some(&mut *ctxt));
    if input_stream.is_null() {
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
        if let Some(directory) = xml_parser_get_directory(filename) {
            (*ctxt).directory = Some(directory.to_string_lossy().into_owned());
        }
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

    let mut default_catalog = XML_DEFAULT_CATALOG.write().unwrap();
    if let Some(default_catalog) = default_catalog.as_mut() {
        default_catalog.convert_sgml_catalog()
    } else {
        -1
    }
}

/// Used to set the debug level for catalog operation, 0 disable
/// debugging, 1 enable it
///
/// Returns the previous value of the catalog debugging level
#[doc(alias = "xmlCatalogSetDebug")]
pub fn xml_catalog_set_debug(level: i32) -> i32 {
    let ret = XML_DEBUG_CATALOGS.load(Ordering::Acquire);

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
pub fn xml_catalog_set_default_prefer(prefer: XmlCatalogPrefer) -> XmlCatalogPrefer {
    let ret = *XML_CATALOG_DEFAULT_PREFER.read().unwrap();

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
    *XML_CATALOG_DEFAULT_PREFER.write().unwrap() = prefer;
    ret
}

/// Used to set the user preference w.r.t. to what catalogs should be accepted
#[doc(alias = "xmlCatalogSetDefaults")]
pub fn xml_catalog_set_defaults(allow: XmlCatalogAllow) {
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
    *XML_CATALOG_DEFAULT_ALLOW.write().unwrap() = allow;
}

/// Used to get the user preference w.r.t. to what catalogs should be accepted
///
/// Returns the current xmlCatalogAllow value
#[doc(alias = "xmlCatalogGetDefaults")]
pub fn xml_catalog_get_defaults() -> XmlCatalogAllow {
    *XML_CATALOG_DEFAULT_ALLOW.read().unwrap()
}

/// Try to lookup the catalog reference associated to a system ID
///
/// Returns the resource if found or null_mut() otherwise.
#[deprecated = "use xmlCatalogResolveSystem()"]
#[doc(alias = "xmlCatalogGetSystem")]
pub unsafe fn xml_catalog_get_system(sys_id: &str) -> Option<String> {
    static MSG: AtomicI32 = AtomicI32::new(0);

    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    if MSG.load(Ordering::Relaxed) == 0 {
        generic_error!("Use of deprecated xmlCatalogGetSystem() call\n");
        MSG.fetch_add(1, Ordering::AcqRel);
    }

    // Check first the XML catalogs
    let mut default_catalog = XML_DEFAULT_CATALOG.write().unwrap();
    if let Some(default_catalog) = default_catalog.as_mut() {
        if let Some(ret) = default_catalog
            .xml
            .as_mut()
            .unwrap()
            .list_xml_resolve(None, Some(sys_id))
        {
            return Some(ret);
        }
    }

    let default_catalog = default_catalog.as_mut()?;
    Some(
        xml_catalog_get_sgml_system(&default_catalog.sgml, sys_id)?
            .to_string_lossy()
            .into_owned(),
    )
}

/// Try to lookup the catalog reference associated to a public ID
///
/// Returns the resource if found or null_mut() otherwise.
#[deprecated = "use xmlCatalogResolvePublic()"]
#[doc(alias = "xmlCatalogGetPublic")]
pub unsafe fn xml_catalog_get_public(pub_id: &str) -> Option<String> {
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
    let mut default_catalog = XML_DEFAULT_CATALOG.write().unwrap();
    if let Some(default_catalog) = default_catalog.as_mut() {
        if let Some(ret) = default_catalog
            .xml
            .as_mut()
            .unwrap()
            .list_xml_resolve(Some(pub_id), None)
        {
            return Some(ret);
        }
    }

    let default_catalog = default_catalog.as_mut()?;
    let sgml = xml_catalog_get_sgml_public(&default_catalog.sgml, pub_id)?;
    Some(sgml.to_string_lossy().into_owned())
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
}
