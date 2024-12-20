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
    ffi::{c_char, CStr, CString},
    fs::File,
    io::Read,
    mem::{size_of, transmute},
    os::raw::c_void,
    path::{Path, PathBuf},
    ptr::{null, null_mut},
    rc::Rc,
    sync::atomic::{AtomicBool, AtomicI32, AtomicPtr, Ordering},
};

use const_format::concatcp;
use libc::{getenv, snprintf};

use crate::{
    encoding::XmlCharEncoding,
    error::__xml_raise_error,
    generic_error,
    hash::XmlHashTableRef,
    io::{xml_parser_get_directory, XmlParserInputBuffer},
    libxml::{
        chvalid::xml_is_blank_char,
        globals::{xml_free, xml_malloc},
        hash::{
            xml_hash_add_entry, xml_hash_create, xml_hash_free, xml_hash_lookup,
            xml_hash_remove_entry, xml_hash_size, XmlHashTable, XmlHashTablePtr,
        },
        parser::{
            xml_free_parser_ctxt, xml_new_parser_ctxt, xml_parse_document, XmlParserCtxtPtr,
            XmlParserInputPtr,
        },
        parser_internals::{xml_new_input_stream, XML_MAX_NAMELEN},
        threads::{
            xml_free_rmutex, xml_get_thread_id, xml_new_rmutex, xml_rmutex_lock, xml_rmutex_unlock,
            XmlRMutex,
        },
        uri::xml_canonic_path,
        xmlstring::{
            xml_str_equal, xml_strcat, xml_strdup, xml_strlen, xml_strncmp, xml_strndup, XmlChar,
        },
    },
    tree::{
        xml_free_doc, xml_free_ns, xml_new_doc, xml_new_doc_node, xml_new_dtd, xml_new_ns,
        NodeCommon, XmlDocPtr, XmlDtdPtr, XmlNodePtr, XmlNsPtr, XML_XML_NAMESPACE,
    },
    uri::build_uri,
    SYSCONFDIR,
};

use super::{chvalid::xml_is_pubid_char, hash::CVoidWrapper, parser_internals::xml_is_letter};

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

    // SGML Catalogs are stored as a simple hash table of catalog entries
    // Catalog stack to check against overflows when building the SGML catalog
    catal_tab: [*mut c_char; XML_MAX_SGML_CATA_DEPTH], /* stack of catals */
    catal_nr: i32,                                     /* Number of current catal streams */
    catal_max: i32,                                    /* Max number of catal streams */
    sgml: XmlHashTablePtr,

    // XML Catalogs are stored as a tree of Catalog entries
    prefer: XmlCatalogPrefer,
    xml: XmlCatalogEntryPtr,
}

impl Default for XmlCatalog {
    fn default() -> Self {
        Self {
            typ: XmlCatalogType::default(),
            catal_tab: [null_mut(); XML_MAX_SGML_CATA_DEPTH],
            catal_nr: 0,
            catal_max: 0,
            sgml: null_mut(),
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
    name: *mut XmlChar,
    value: *mut XmlChar,
    url: Option<PathBuf>, /* The expanded URL using the base */
    prefer: XmlCatalogPrefer,
    dealloc: i32,
    depth: i32,
    group: *mut XmlCatalogEntry,
}

impl Default for XmlCatalogEntry {
    fn default() -> Self {
        Self {
            next: null_mut(),
            parent: null_mut(),
            children: null_mut(),
            typ: XmlCatalogEntryType::default(),
            name: null_mut(),
            value: null_mut(),
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
    (*ret).catal_nr = 0;
    (*ret).catal_max = XML_MAX_SGML_CATA_DEPTH as _;
    (*ret).prefer = prefer;
    if matches!((*ret).typ, XmlCatalogType::XmlSGMLCatalogType) {
        (*ret).sgml = xml_hash_create(10);
    }
    ret
}

/// create a new Catalog.
///
/// Returns the xmlCatalogPtr or null_mut() in case of error
#[doc(alias = "xmlNewCatalog")]
pub unsafe fn xml_new_catalog(sgml: bool) -> XmlCatalogPtr {
    if sgml {
        let catal = xml_create_new_catalog(
            XmlCatalogType::XmlSGMLCatalogType,
            XML_CATALOG_DEFAULT_PREFER,
        );
        if !catal.is_null() && (*catal).sgml.is_null() {
            (*catal).sgml = xml_hash_create(10);
        }
        catal
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

macro_rules! RAW {
    ($cur:expr) => {
        *$cur
    };
}
macro_rules! NEXT {
    ($cur:expr) => {
        $cur = $cur.add(1);
    };
}
macro_rules! SKIP {
    ($cur:expr, $x:expr) => {
        $cur = $cur.add($x as usize);
    };
}

macro_rules! SKIP_BLANKS {
    ($cur:expr) => {
        while xml_is_blank_char(*$cur as u32) {
            NEXT!($cur);
        }
    };
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
/// Returns the new string or null_mut(), the string must be deallocated by the caller.
#[doc(alias = "xmlCatalogNormalizePublic")]
unsafe fn xml_catalog_normalize_public(pub_id: *const XmlChar) -> *mut XmlChar {
    let mut ok: i32 = 1;
    let mut white: i32;
    let mut q: *mut XmlChar;

    if pub_id.is_null() {
        return null_mut();
    }

    white = 1;
    let mut p = pub_id;
    while *p != 0 && ok != 0 {
        if !xml_is_blank_char(*p as u32) {
            white = 0;
        } else if *p == 0x20 && white == 0 {
            white = 1;
        } else {
            ok = 0;
        }
        p = p.add(1);
    }
    if ok != 0 && white == 0 {
        // is normalized
        return null_mut();
    }

    let ret: *mut XmlChar = xml_strdup(pub_id);
    q = ret;
    white = 0;
    p = pub_id;
    while *p != 0 {
        if xml_is_blank_char(*p as u32) {
            if q != ret {
                white = 1;
            }
        } else {
            if white != 0 {
                *q = 0x20;
                q = q.add(1);
                white = 0;
            }
            *q = *p;
            q = q.add(1);
        }
        p = p.add(1);
    }
    *q = 0;
    ret
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
    mut name: *const XmlChar,
    value: *const XmlChar,
    mut url: *const XmlChar,
    prefer: XmlCatalogPrefer,
    group: XmlCatalogEntryPtr,
) -> XmlCatalogEntryPtr {
    let mut normid: *mut XmlChar = null_mut();

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
    if matches!(
        typ,
        XmlCatalogEntryType::XmlCataPublic | XmlCatalogEntryType::XmlCataDelegatePublic
    ) {
        normid = xml_catalog_normalize_public(name);
        if !normid.is_null() {
            name = if *normid != 0 { normid } else { null_mut() };
        }
    }
    if !name.is_null() {
        (*ret).name = xml_strdup(name);
    } else {
        (*ret).name = null_mut();
    }
    if !normid.is_null() {
        xml_free(normid as _);
    }
    if !value.is_null() {
        (*ret).value = xml_strdup(value);
    } else {
        (*ret).value = null_mut();
    }
    if url.is_null() {
        url = value;
    }
    if !url.is_null() {
        (*ret).url = Some(PathBuf::from(
            CStr::from_ptr(url as *const i8)
                .to_string_lossy()
                .into_owned(),
        ));
    }
    (*ret).prefer = prefer;
    (*ret).dealloc = 0;
    (*ret).depth = 0;
    (*ret).group = group;
    ret
}

/// Free the memory allocated to a Catalog entry
#[doc(alias = "xmlFreeCatalogEntry")]
extern "C" fn xml_free_catalog_entry(payload: *mut c_void, _name: *const XmlChar) {
    let ret: XmlCatalogEntryPtr = payload as XmlCatalogEntryPtr;
    if ret.is_null() {
        return;
    }
    // Entries stored in the file hash must be deallocated only by the file hash cleaner !
    unsafe {
        if (*ret).dealloc == 1 {
            return;
        }

        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            if !(*ret).name.is_null() {
                generic_error!(
                    "Free catalog entry {}\n",
                    CStr::from_ptr((*ret).name as *const i8).to_string_lossy()
                );
            } else if !(*ret).value.is_null() {
                generic_error!(
                    "Free catalog entry {}\n",
                    CStr::from_ptr((*ret).value as *const i8).to_string_lossy()
                );
            } else {
                generic_error!("Free catalog entry\n");
            }
        }

        if !(*ret).name.is_null() {
            xml_free((*ret).name as _);
        }
        if !(*ret).value.is_null() {
            xml_free((*ret).value as _);
        }
        let _ = (*ret).url.take();
        xml_free(ret as _);
    }
}

/// Load the catalog and expand the existing catal structure.
/// This can be either an XML Catalog or an SGML Catalog
///
/// Returns 0 in case of success, -1 in case of error
#[doc(alias = "xmlExpandCatalog")]
unsafe fn xml_expand_catalog(catal: XmlCatalogPtr, filename: impl AsRef<Path>) -> i32 {
    if catal.is_null() {
        return -1;
    }
    let filename = filename.as_ref();

    if matches!((*catal).typ, XmlCatalogType::XmlSGMLCatalogType) {
        let Some(content) = xml_load_file_content(filename) else {
            return -1;
        };

        let ret = xml_parse_sgml_catalog(catal, &content, filename, 0);
        if ret < 0 {
            return -1;
        }
    } else {
        let filename = CString::new(filename.to_string_lossy().as_ref()).unwrap();
        let mut cur: XmlCatalogEntryPtr;
        let tmp: XmlCatalogEntryPtr = xml_new_catalog_entry(
            XmlCatalogEntryType::XmlCataCatalog,
            null_mut(),
            null_mut(),
            filename.as_ptr() as *const u8,
            XML_CATALOG_DEFAULT_PREFER,
            null_mut(),
        );

        cur = (*catal).xml;
        if cur.is_null() {
            (*catal).xml = tmp;
        } else {
            while !(*cur).next.is_null() {
                cur = (*cur).next;
            }
            (*cur).next = tmp;
        }
    }
    0
}

/// Parse an SGML catalog content and fill up the @catal hash table with
/// the new entries found.
///
/// Returns 0 in case of success, -1 in case of error.
#[doc(alias = "xmlParseSGMLCatalog")]
unsafe fn xml_parse_sgml_catalog(
    catal: XmlCatalogPtr,
    value: &[u8],
    file: impl AsRef<Path>,
    is_super: i32,
) -> i32 {
    let mut cur = value;
    let mut res: i32;

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
                    let filename = CString::new(filename).unwrap();
                    let name =
                        CString::new(String::from_utf8_lossy(name.unwrap().as_ref()).as_ref())
                            .unwrap();
                    let entry: XmlCatalogEntryPtr = xml_new_catalog_entry(
                        typ,
                        name.as_ptr() as *const u8,
                        filename.as_ptr() as *const u8,
                        null_mut(),
                        XmlCatalogPrefer::None,
                        null_mut(),
                    );
                    res = xml_hash_add_entry((*catal).sgml, name.as_ptr() as *const u8, entry as _);
                    if res < 0 {
                        xml_free_catalog_entry(entry as _, null_mut());
                    }
                }
            } else if matches!(typ, XmlCatalogEntryType::SgmlCataCatalog) {
                if is_super != 0 {
                    let sysid =
                        CString::new(String::from_utf8_lossy(sysid.unwrap().as_ref()).as_ref())
                            .unwrap();
                    let entry: XmlCatalogEntryPtr = xml_new_catalog_entry(
                        typ,
                        sysid.as_ptr() as *const u8,
                        null_mut(),
                        null_mut(),
                        XmlCatalogPrefer::None,
                        null_mut(),
                    );
                    res =
                        xml_hash_add_entry((*catal).sgml, sysid.as_ptr() as *const u8, entry as _);
                    if res < 0 {
                        xml_free_catalog_entry(entry as _, null_mut());
                    }
                } else if let Some(sysid) = sysid {
                    let sysid = String::from_utf8_lossy(&sysid);
                    if let Some(filename) = build_uri(&sysid, &base) {
                        xml_expand_catalog(catal, filename);
                    }
                }
            }
        }
    }
    0
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
        let ret = xml_parse_sgml_catalog(catal, &content, filename, 0);
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
        let filename = CString::new(filename.to_string_lossy().as_ref()).unwrap();
        (*catal).xml = xml_new_catalog_entry(
            XmlCatalogEntryType::XmlCataCatalog,
            null_mut(),
            null_mut(),
            filename.as_ptr() as *const u8,
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

    let ret = xml_parse_sgml_catalog(catal, &content, filename, 1);
    if ret < 0 {
        xml_free_catalog(catal);
        return null_mut();
    }
    catal
}

/// Convert all the SGML catalog entries as XML ones
///
/// Returns the number of entries converted if successful, -1 otherwise
#[doc(alias = "xmlConvertSGMLCatalog")]
pub unsafe fn xml_convert_sgml_catalog(catal: XmlCatalogPtr) -> i32 {
    if catal.is_null() || !matches!((*catal).typ, XmlCatalogType::XmlSGMLCatalogType) {
        return -1;
    }

    if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
        generic_error!("Converting SGML catalog to XML\n");
    }

    let Some(mut sgml) = XmlHashTableRef::from_raw((*catal).sgml) else {
        return 0;
    };

    if (*catal).xml.is_null() {
        return 0;
    }

    for (payload, name, _, _) in sgml.drain() {
        let entry: XmlCatalogEntryPtr = payload.0 as XmlCatalogEntryPtr;
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
                xml_free_catalog_entry(
                    payload.0,
                    name.map(|n| n.as_ptr()).unwrap_or(null()) as *const u8,
                );
                continue;
            }
        }
        // Conversion successful, remove from the SGML catalog
        // and add it to the default XML one
        (*entry).parent = (*catal).xml;
        (*entry).next = null_mut();
        if (*(*catal).xml).children.is_null() {
            (*(*catal).xml).children = entry;
        } else {
            let mut prev: XmlCatalogEntryPtr;

            prev = (*(*catal).xml).children;
            while !(*prev).next.is_null() {
                prev = (*prev).next;
            }
            (*prev).next = entry;
        }
    }
    // xml_hash_scan(
    //     (*catal).sgml,
    //     xml_catalog_convert_entry,
    //     addr_of_mut!(catal) as _,
    // );
    0
}

// Hash table containing all the trees of XML catalogs parsed by the application.
static XML_CATALOG_XMLFILES: AtomicPtr<XmlHashTable<'static, CVoidWrapper>> =
    AtomicPtr::new(null_mut());

// A mutex for modifying the shared global catalog(s) xmlDefaultCatalog tree.
// It also protects xmlCatalogXMLFiles.
// The core of this readers/writer scheme is in `xmlFetchXMLCatalogFile()`.
static XML_CATALOG_MUTEX: AtomicPtr<XmlRMutex> = AtomicPtr::new(null_mut());

/// Handle a catalog error
#[doc(alias = "xmlCatalogErr")]
macro_rules! xml_catalog_err {
    (
        $catal:expr,
        $node:expr,
        $error:expr,
        $msg:literal
    ) => {
        xml_catalog_err!(@inner $catal, $node, $error, $msg, None, None, None,);
    };
    (
        $catal:expr,
        $node:expr,
        $error:expr,
        $msg:literal,
        $str1:expr,
    ) => {
        let msg = format!($msg, $str1);
        xml_catalog_err!(@inner $catal, $node, $error, &msg, Some($str1.into()), None, None,);
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
        let name_value = name_value.map(|n| CString::new(n).unwrap());
        let uri_value = CString::new(uri_value).unwrap();
        let url = CString::new(url).unwrap();
        ret = xml_new_catalog_entry(
            typ,
            name_value
                .as_ref()
                .map_or(null_mut(), |n| n.as_ptr() as *const u8),
            uri_value.as_ptr() as *const u8,
            url.as_ptr() as *const u8,
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
        let prop = (*cur).get_prop("id").map(|p| CString::new(p).unwrap());
        let base = (*cur)
            .get_ns_prop("base", XML_XML_NAMESPACE.to_str().ok())
            .map(|b| CString::new(b).unwrap());
        entry = xml_new_catalog_entry(
            XmlCatalogEntryType::XmlCataGroup,
            prop.as_ref()
                .map_or(null_mut(), |p| p.as_ptr() as *const u8),
            base.as_ref()
                .map_or(null_mut(), |p| p.as_ptr() as *const u8),
            null_mut(),
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
    filename: *const XmlChar,
) -> XmlCatalogEntryPtr {
    let mut cur: XmlNodePtr;
    let parent: XmlCatalogEntryPtr;

    if filename.is_null() {
        return null_mut();
    }

    let doc: XmlDocPtr = xml_parse_catalog_file(filename as _);
    if doc.is_null() {
        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            generic_error!(
                "Failed to parse catalog {}\n",
                CStr::from_ptr(filename as *const i8).to_string_lossy()
            );
        }
        return null_mut();
    }

    if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
        generic_error!(
            "{} Parsing catalog {}\n",
            xml_get_thread_id(),
            CStr::from_ptr(filename as *const i8).to_string_lossy()
        );
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
            null_mut(),
            filename,
            null_mut(),
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
            CStr::from_ptr(filename as *const i8).to_string_lossy(),
        );
        xml_free_doc(doc);
        return null_mut();
    }
    xml_free_doc(doc);
    parent
}

/// Fetch and parse the subcatalog referenced by an entry
///
/// Returns 0 in case of success, -1 otherwise
#[doc(alias = "xmlFetchXMLCatalogFile")]
unsafe fn xml_fetch_xml_catalog_file(catal: XmlCatalogEntryPtr) -> i32 {
    let mut doc: XmlCatalogEntryPtr;

    if catal.is_null() {
        return -1;
    }
    let Some(url) = (*catal).url.as_deref() else {
        return -1;
    };

    // lock the whole catalog for modification
    let mutex = XML_CATALOG_MUTEX.load(Ordering::Acquire);
    xml_rmutex_lock(mutex);
    if !(*catal).children.is_null() {
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

            if (*catal).typ == XmlCatalogEntryType::XmlCataCatalog {
                (*catal).children = (*doc).children;
            } else {
                (*catal).children = doc;
            }
            (*catal).dealloc = 0;
            xml_rmutex_unlock(mutex);
            return 0;
        }
        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            generic_error!("{} not found in file hash\n", url.display());
        }
    }

    // Fetch and parse. Note that xmlParseXMLCatalogFile does not use the existing catalog,
    // there is no recursion allowed at that level.
    let curl = CString::new(url.to_string_lossy().as_ref()).unwrap();
    doc = xml_parse_xml_catalog_file((*catal).prefer, curl.as_ptr() as *const u8);
    if doc.is_null() {
        (*catal).typ = XmlCatalogEntryType::XmlCataBrokenCatalog;
        xml_rmutex_unlock(mutex);
        return -1;
    }

    if matches!((*catal).typ, XmlCatalogEntryType::XmlCataCatalog) {
        (*catal).children = (*doc).children;
    } else {
        (*catal).children = doc;
    }

    (*doc).dealloc = 1;

    if catalog_files.is_null() {
        catalog_files = xml_hash_create(10);
    }
    if !catalog_files.is_null() {
        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            generic_error!("{} added to file hash\n", url.display());
        }
        xml_hash_add_entry(catalog_files, curl.as_ptr() as *const u8, doc as _);
    }
    xml_rmutex_unlock(mutex);
    XML_CATALOG_XMLFILES.store(catalog_files, Ordering::Release);
    0
}

/// lookup the internal type associated to an XML catalog entry name
///
/// Returns the type associated with that name
#[doc(alias = "xmlGetXMLCatalogEntryType")]
unsafe fn xml_get_xml_catalog_entry_type(name: *const XmlChar) -> XmlCatalogEntryType {
    let mut typ: XmlCatalogEntryType = XmlCatalogEntryType::XmlCataNone;
    if xml_str_equal(name, c"system".as_ptr() as _) {
        typ = XmlCatalogEntryType::XmlCataSystem;
    } else if xml_str_equal(name, c"public".as_ptr() as _) {
        typ = XmlCatalogEntryType::XmlCataPublic;
    } else if xml_str_equal(name, c"rewriteSystem".as_ptr() as _) {
        typ = XmlCatalogEntryType::XmlCataRewriteSystem;
    } else if xml_str_equal(name, c"delegatePublic".as_ptr() as _) {
        typ = XmlCatalogEntryType::XmlCataDelegatePublic;
    } else if xml_str_equal(name, c"delegateSystem".as_ptr() as _) {
        typ = XmlCatalogEntryType::XmlCataDelegateSystem;
    } else if xml_str_equal(name, c"uri".as_ptr() as _) {
        typ = XmlCatalogEntryType::XmlCataURI;
    } else if xml_str_equal(name, c"rewriteURI".as_ptr() as _) {
        typ = XmlCatalogEntryType::XmlCataRewriteURI;
    } else if xml_str_equal(name, c"delegateURI".as_ptr() as _) {
        typ = XmlCatalogEntryType::XmlCataDelegateURI;
    } else if xml_str_equal(name, c"nextCatalog".as_ptr() as _) {
        typ = XmlCatalogEntryType::XmlCataNextCatalog;
    } else if xml_str_equal(name, c"catalog".as_ptr() as _) {
        typ = XmlCatalogEntryType::XmlCataCatalog;
    }
    typ
}

/// Add an entry in the XML catalog, it may overwrite existing but different entries.
///
/// Returns 0 if successful, -1 otherwise
#[doc(alias = "xmlAddXMLCatalog")]
unsafe fn xml_add_xml_catalog(
    catal: XmlCatalogEntryPtr,
    typs: *const XmlChar,
    orig: *const XmlChar,
    replace: *const XmlChar,
) -> i32 {
    let mut cur: XmlCatalogEntryPtr;
    let mut doregister: i32 = 0;

    if catal.is_null()
        || !matches!(
            (*catal).typ,
            XmlCatalogEntryType::XmlCataCatalog | XmlCatalogEntryType::XmlCataBrokenCatalog
        )
    {
        return -1;
    }
    if (*catal).children.is_null() {
        xml_fetch_xml_catalog_file(catal);
    }
    if (*catal).children.is_null() {
        doregister = 1;
    }

    let typ: XmlCatalogEntryType = xml_get_xml_catalog_entry_type(typs);
    if matches!(typ, XmlCatalogEntryType::XmlCataNone) {
        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            if typs.is_null() {
                generic_error!("Failed to add unknown element (NULL) to catalog\n",);
            } else {
                generic_error!(
                    "Failed to add unknown element {} to catalog\n",
                    CStr::from_ptr(typs as *const i8).to_string_lossy()
                );
            }
        }
        return -1;
    }

    cur = (*catal).children;
    // Might be a simple "update in place"
    if !cur.is_null() {
        while !cur.is_null() {
            if !orig.is_null() && ((*cur).typ == typ) && xml_str_equal(orig, (*cur).name) {
                if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                    if typs.is_null() {
                        generic_error!("Updating element (NULL) to catalog\n",);
                    } else {
                        generic_error!(
                            "Updating element {} to catalog\n",
                            CStr::from_ptr(typs as *const i8).to_string_lossy()
                        );
                    }
                }
                if !(*cur).value.is_null() {
                    xml_free((*cur).value as _);
                }
                let _ = (*cur).url.take();
                (*cur).value = xml_strdup(replace);
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
        generic_error!(
            "Adding element {} to catalog\n",
            CStr::from_ptr(typs as *const i8).to_string_lossy()
        );
    }
    if cur.is_null() {
        (*catal).children =
            xml_new_catalog_entry(typ, orig, replace, null_mut(), (*catal).prefer, null_mut());
    } else {
        (*cur).next =
            xml_new_catalog_entry(typ, orig, replace, null_mut(), (*catal).prefer, null_mut());
    }
    if doregister != 0 {
        (*catal).typ = XmlCatalogEntryType::XmlCataCatalog;
        if let Some(url) = (*catal).url.as_deref() {
            let curl = CString::new(url.to_string_lossy().as_ref()).unwrap();
            cur = xml_hash_lookup(
                XML_CATALOG_XMLFILES.load(Ordering::Relaxed),
                curl.as_ptr() as *const u8,
            ) as XmlCatalogEntryPtr;
        }
        if !cur.is_null() {
            (*cur).children = (*catal).children;
        }
    }

    0
}

/// Get the Catalog entry type for a given SGML Catalog name
///
/// Returns Catalog entry type
#[doc(alias = "xmlGetSGMLCatalogEntryType")]
unsafe fn xml_get_sgml_catalog_entry_type(name: *const XmlChar) -> XmlCatalogEntryType {
    let mut typ: XmlCatalogEntryType = XmlCatalogEntryType::XmlCataNone;
    if xml_str_equal(name, c"SYSTEM".as_ptr() as _) {
        typ = XmlCatalogEntryType::SgmlCataSystem;
    } else if xml_str_equal(name, c"PUBLIC".as_ptr() as _) {
        typ = XmlCatalogEntryType::SgmlCataPublic;
    } else if xml_str_equal(name, c"DELEGATE".as_ptr() as _) {
        typ = XmlCatalogEntryType::SgmlCataDelegate;
    } else if xml_str_equal(name, c"ENTITY".as_ptr() as _) {
        typ = XmlCatalogEntryType::SgmlCataEntity;
    } else if xml_str_equal(name, c"DOCTYPE".as_ptr() as _) {
        typ = XmlCatalogEntryType::SgmlCataDoctype;
    } else if xml_str_equal(name, c"LINKTYPE".as_ptr() as _) {
        typ = XmlCatalogEntryType::SgmlCataLinktype;
    } else if xml_str_equal(name, c"NOTATION".as_ptr() as _) {
        typ = XmlCatalogEntryType::SgmlCataNotation;
    } else if xml_str_equal(name, c"SGMLDECL".as_ptr() as _) {
        typ = XmlCatalogEntryType::SgmlCataSGMLDecl;
    } else if xml_str_equal(name, c"DOCUMENT".as_ptr() as _) {
        typ = XmlCatalogEntryType::SgmlCataDocument;
    } else if xml_str_equal(name, c"CATALOG".as_ptr() as _) {
        typ = XmlCatalogEntryType::SgmlCataCatalog;
    } else if xml_str_equal(name, c"BASE".as_ptr() as _) {
        typ = XmlCatalogEntryType::SgmlCataBase;
    }
    typ
}

/// Add an entry in the catalog, it may overwrite existing but different entries.
///
/// Returns 0 if successful, -1 otherwise
#[doc(alias = "xmlACatalogAdd")]
pub unsafe fn xml_a_catalog_add(
    catal: XmlCatalogPtr,
    typ: *const XmlChar,
    orig: *const XmlChar,
    replace: *const XmlChar,
) -> i32 {
    let mut res: i32 = -1;

    if catal.is_null() {
        return -1;
    }

    if matches!((*catal).typ, XmlCatalogType::XmlXMLCatalogType) {
        res = xml_add_xml_catalog((*catal).xml, typ, orig, replace);
    } else {
        let cattype: XmlCatalogEntryType = xml_get_sgml_catalog_entry_type(typ);
        if !matches!(cattype, XmlCatalogEntryType::XmlCataNone) {
            let entry: XmlCatalogEntryPtr = xml_new_catalog_entry(
                cattype,
                orig,
                replace,
                null_mut(),
                XmlCatalogPrefer::None,
                null_mut(),
            );
            if (*catal).sgml.is_null() {
                (*catal).sgml = xml_hash_create(10);
            }
            res = xml_hash_add_entry((*catal).sgml, orig, entry as _);
            if res < 0 {
                xml_free_catalog_entry(entry as _, null_mut());
            }
        }
    }
    res
}

/// Remove entries in the XML catalog where the value or the URI is equal to `value`.
///
/// Returns the number of entries removed if successful, -1 otherwise
#[doc(alias = "xmlDelXMLCatalog")]
unsafe fn xml_del_xml_catalog(catal: XmlCatalogEntryPtr, value: *const XmlChar) -> i32 {
    let mut cur: XmlCatalogEntryPtr;
    let ret: i32 = 0;

    if catal.is_null()
        || !matches!(
            (*catal).typ,
            XmlCatalogEntryType::XmlCataCatalog | XmlCatalogEntryType::XmlCataBrokenCatalog
        )
    {
        return -1;
    }
    if value.is_null() {
        return -1;
    }
    if (*catal).children.is_null() {
        xml_fetch_xml_catalog_file(catal);
    }

    /*
     * Scan the children
     */
    cur = (*catal).children;
    while !cur.is_null() {
        if (!(*cur).name.is_null() && xml_str_equal(value, (*cur).name))
            || xml_str_equal(value, (*cur).value)
        {
            if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                if !(*cur).name.is_null() {
                    generic_error!(
                        "Removing element {} from catalog\n",
                        CStr::from_ptr((*cur).name as *const i8).to_string_lossy()
                    );
                } else {
                    generic_error!(
                        "Removing element {} from catalog\n",
                        CStr::from_ptr((*cur).value as *const i8).to_string_lossy()
                    );
                }
            }
            (*cur).typ = XmlCatalogEntryType::XmlCataRemoved;
        }
        cur = (*cur).next;
    }
    ret
}

/// Remove an entry from the catalog
///
/// Returns the number of entries removed if successful, -1 otherwise
#[doc(alias = "xmlACatalogRemove")]
pub unsafe fn xml_a_catalog_remove(catal: XmlCatalogPtr, value: *const XmlChar) -> i32 {
    let mut res: i32;

    if catal.is_null() || value.is_null() {
        return -1;
    }

    if matches!((*catal).typ, XmlCatalogType::XmlXMLCatalogType) {
        res = xml_del_xml_catalog((*catal).xml, value);
    } else {
        res = xml_hash_remove_entry((*catal).sgml, value, Some(xml_free_catalog_entry));
        if res == 0 {
            res = 1;
        }
    }
    res
}

const XML_CATAL_BREAK: *mut XmlChar = usize::MAX as *mut XmlChar;
const XML_URN_PUBID: &CStr = c"urn:publicid:";
const MAX_DELEGATE: usize = 50;
const MAX_CATAL_DEPTH: usize = 50;

/// Expand the URN into the equivalent Public Identifier
///
/// Returns the new identifier or null_mut(), the string must be deallocated by the caller.
#[doc(alias = "xmlCatalogUnWrapURN")]
unsafe fn xml_catalog_unwrap_urn(mut urn: *const XmlChar) -> *mut XmlChar {
    let mut result: [XmlChar; 2000] = [0; 2000];
    let mut i: usize = 0;

    if xml_strncmp(
        urn,
        XML_URN_PUBID.as_ptr() as _,
        XML_URN_PUBID.to_bytes().len() as i32,
    ) != 0
    {
        return null_mut();
    }
    urn = urn.add(XML_URN_PUBID.to_bytes().len());

    while *urn != 0 {
        if i > result.len() - 4 {
            break;
        }
        if *urn == b'+' {
            result[i] = b' ';
            i += 1;
            urn = urn.add(1);
        } else if *urn == b':' {
            result[i] = b'/';
            i += 1;
            result[i] = b'/';
            i += 1;
            urn = urn.add(1);
        } else if *urn == b';' {
            result[i] = b':';
            i += 1;
            result[i] = b':';
            i += 1;
            urn = urn.add(1);
        } else if *urn == b'%' {
            if *urn.add(1) == b'2' && *urn.add(2) == b'B' {
                result[i] = b'+';
                i += 1;
            } else if *urn.add(1) == b'3' && *urn.add(2) == b'A' {
                result[i] = b':';
                i += 1;
            } else if *urn.add(1) == b'2' && *urn.add(2) == b'F' {
                result[i] = b'/';
                i += 1;
            } else if *urn.add(1) == b'3' && *urn.add(2) == b'B' {
                result[i] = b';';
                i += 1;
            } else if *urn.add(1) == b'2' && *urn.add(2) == b'7' {
                result[i] = b'\'';
                i += 1;
            } else if *urn.add(1) == b'3' && *urn.add(2) == b'F' {
                result[i] = b'?';
                i += 1;
            } else if *urn.add(1) == b'2' && *urn.add(2) == b'3' {
                result[i] = b'#';
                i += 1;
            } else if *urn.add(1) == b'2' && *urn.add(2) == b'5' {
                result[i] = b'%';
                i += 1;
            } else {
                result[i] = *urn;
                i += 1;
                urn = urn.add(1);
                continue;
            }
            urn = urn.add(3);
        } else {
            result[i] = *urn;
            i += 1;
            urn = urn.add(1);
        }
    }
    result[i] = 0;

    xml_strdup(result.as_ptr() as _)
}

/// Do a complete resolution lookup of an External Identifier for a list of catalog entries.
///
/// Implements (or tries to) 7.1. External Identifier Resolution
/// from http://www.oasis-open.org/committees/entity/spec-2001-08-06.html
///
/// Returns the URI of the resource or null_mut() if not found
#[doc(alias = "xmlCatalogXMLResolve")]
unsafe fn xml_catalog_xml_resolve(
    catal: XmlCatalogEntryPtr,
    pub_id: *const XmlChar,
    sys_id: *const XmlChar,
) -> *mut XmlChar {
    let mut ret: *mut XmlChar;
    let mut cur: XmlCatalogEntryPtr;
    let mut have_delegate: i32;
    let mut have_next: i32 = 0;

    // protection against loops
    if (*catal).depth > MAX_CATAL_DEPTH as i32 {
        xml_catalog_err!(
            catal,
            null_mut(),
            XmlParserErrors::XmlCatalogRecursion,
            "Detected recursion in catalog {}\n",
            CStr::from_ptr((*catal).name as *const i8).to_string_lossy(),
        );
        return null_mut();
    }
    (*catal).depth += 1;

    // First tries steps 2/ 3/ 4/ if a system ID is provided.
    if !sys_id.is_null() {
        let mut rewrite: XmlCatalogEntryPtr = null_mut();
        let mut lenrewrite: i32 = 0;
        let mut len: i32;
        cur = catal;
        have_delegate = 0;
        while !cur.is_null() {
            match (*cur).typ {
                XmlCatalogEntryType::XmlCataSystem => {
                    if xml_str_equal(sys_id, (*cur).name) {
                        let url = (*cur).url.as_deref().unwrap();
                        let curl = CString::new(url.to_string_lossy().as_ref()).unwrap();
                        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                            generic_error!(
                                "Found system match {}, using {}\n",
                                CStr::from_ptr((*cur).name as *const i8).to_string_lossy(),
                                url.display()
                            );
                        }
                        (*catal).depth -= 1;
                        return xml_strdup(curl.as_ptr() as *const u8);
                    }
                }
                XmlCatalogEntryType::XmlCataRewriteSystem => {
                    len = xml_strlen((*cur).name);
                    if len > lenrewrite && xml_strncmp(sys_id, (*cur).name, len) == 0 {
                        lenrewrite = len;
                        rewrite = cur;
                    }
                }
                XmlCatalogEntryType::XmlCataDelegateSystem => {
                    if xml_strncmp(sys_id, (*cur).name, xml_strlen((*cur).name)) == 0 {
                        have_delegate += 1;
                    }
                }
                XmlCatalogEntryType::XmlCataNextCatalog => {
                    have_next += 1;
                }
                _ => {}
            }
            cur = (*cur).next;
        }
        if !rewrite.is_null() {
            if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                generic_error!(
                    "Using rewriting rule {}\n",
                    CStr::from_ptr((*rewrite).name as *const i8).to_string_lossy()
                );
            }
            if let Some(url) = (*rewrite).url.as_deref() {
                let url = CString::new(url.to_string_lossy().as_ref()).unwrap();
                ret = xml_strdup(url.as_ptr() as *const u8);
            } else {
                ret = null_mut();
            }
            if !ret.is_null() {
                ret = xml_strcat(ret, sys_id.add(lenrewrite as usize));
            }
            (*catal).depth -= 1;
            return ret;
        }
        if have_delegate != 0 {
            let mut delegates: [Option<&Path>; MAX_DELEGATE] = [None; MAX_DELEGATE];
            let mut nb_list = 0;

            // Assume the entries have been sorted by decreasing substring
            // matches when the list was produced.
            cur = catal;
            'b: while !cur.is_null() {
                if matches!((*cur).typ, XmlCatalogEntryType::XmlCataDelegateSystem)
                    && xml_strncmp(sys_id, (*cur).name, xml_strlen((*cur).name)) == 0
                {
                    for i in 0..nb_list {
                        if (*cur).url.as_deref() == delegates[i] {
                            cur = (*cur).next;
                            continue 'b;
                        }
                    }
                    if nb_list < MAX_DELEGATE {
                        delegates[nb_list] = (*cur).url.as_deref();
                        nb_list += 1;
                    }

                    if (*cur).children.is_null() {
                        xml_fetch_xml_catalog_file(cur);
                    }
                    if !(*cur).children.is_null() {
                        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                            generic_error!(
                                "Trying system delegate {}\n",
                                (*cur).url.as_deref().unwrap().display()
                            );
                        }
                        ret = xml_catalog_list_xml_resolve((*cur).children, null_mut(), sys_id);
                        if !ret.is_null() {
                            (*catal).depth -= 1;
                            return ret;
                        }
                    }
                }
                cur = (*cur).next;
            }
            // Apply the cut algorithm explained in 4/
            (*catal).depth -= 1;
            return XML_CATAL_BREAK;
        }
    }
    // Then tries 5/ 6/ if a public ID is provided
    if !pub_id.is_null() {
        cur = catal;
        have_delegate = 0;
        while !cur.is_null() {
            match (*cur).typ {
                XmlCatalogEntryType::XmlCataPublic => {
                    if xml_str_equal(pub_id, (*cur).name) {
                        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                            generic_error!(
                                "Found public match {}\n",
                                CStr::from_ptr((*cur).name as *const i8).to_string_lossy()
                            );
                        }
                        (*catal).depth -= 1;
                        return if let Some(url) = (*cur).url.as_deref() {
                            let url = CString::new(url.to_string_lossy().as_ref()).unwrap();
                            xml_strdup(url.as_ptr() as *const u8)
                        } else {
                            null_mut()
                        };
                    }
                }
                XmlCatalogEntryType::XmlCataDelegatePublic => {
                    if xml_strncmp(pub_id, (*cur).name, xml_strlen((*cur).name)) == 0
                        && matches!((*cur).prefer, XmlCatalogPrefer::Public)
                    {
                        have_delegate += 1;
                    }
                }
                XmlCatalogEntryType::XmlCataNextCatalog => {
                    if sys_id.is_null() {
                        have_next += 1;
                    }
                }
                _ => {}
            }
            cur = (*cur).next;
        }
        if have_delegate != 0 {
            let mut delegates: [Option<&Path>; MAX_DELEGATE] = [None; MAX_DELEGATE];
            let mut nb_list: usize = 0;

            // Assume the entries have been sorted by decreasing substring
            // matches when the list was produced.
            cur = catal;
            'b: while !cur.is_null() {
                if (*cur).typ == XmlCatalogEntryType::XmlCataDelegatePublic
                    && matches!((*cur).prefer, XmlCatalogPrefer::Public)
                    && xml_strncmp(pub_id, (*cur).name, xml_strlen((*cur).name)) == 0
                {
                    for i in 0..nb_list {
                        if (*cur).url.as_deref() == delegates[i] {
                            cur = (*cur).next;
                            continue 'b;
                        }
                    }
                    if nb_list < MAX_DELEGATE {
                        delegates[nb_list] = (*cur).url.as_deref();
                        nb_list += 1;
                    }

                    if (*cur).children.is_null() {
                        xml_fetch_xml_catalog_file(cur);
                    }
                    if !(*cur).children.is_null() {
                        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                            generic_error!(
                                "Trying public delegate {}\n",
                                (*cur).url.as_deref().unwrap().display()
                            );
                        }
                        ret = xml_catalog_list_xml_resolve((*cur).children, pub_id, null_mut());
                        if !ret.is_null() {
                            (*catal).depth -= 1;
                            return ret;
                        }
                    }
                }
                cur = (*cur).next;
            }
            /*
             * Apply the cut algorithm explained in 4/
             */
            (*catal).depth -= 1;
            return XML_CATAL_BREAK;
        }
    }
    if have_next != 0 {
        cur = catal;
        while !cur.is_null() {
            if (*cur).typ == XmlCatalogEntryType::XmlCataNextCatalog {
                if (*cur).children.is_null() {
                    xml_fetch_xml_catalog_file(cur);
                }
                if !(*cur).children.is_null() {
                    ret = xml_catalog_list_xml_resolve((*cur).children, pub_id, sys_id);
                    if !ret.is_null() {
                        (*catal).depth -= 1;
                        return ret;
                    } else if (*catal).depth > MAX_CATAL_DEPTH as i32 {
                        return null_mut();
                    }
                }
            }
            cur = (*cur).next;
        }
    }

    (*catal).depth -= 1;
    null_mut()
}

/// Do a complete resolution lookup of an External Identifier for a list of catalogs
///
/// Implements (or tries to) 7.1. External Identifier Resolution
/// from http://www.oasis-open.org/committees/entity/spec-2001-08-06.html
///
/// Returns the URI of the resource or null_mut() if not found
#[doc(alias = "xmlCatalogListXMLResolve")]
unsafe fn xml_catalog_list_xml_resolve(
    mut catal: XmlCatalogEntryPtr,
    mut pub_id: *const XmlChar,
    sys_id: *const XmlChar,
) -> *mut XmlChar {
    let mut ret: *mut XmlChar = null_mut();
    let urn_id: *mut XmlChar;

    if catal.is_null() {
        return null_mut();
    }
    if pub_id.is_null() && sys_id.is_null() {
        return null_mut();
    }

    let normid: *mut XmlChar = xml_catalog_normalize_public(pub_id);
    if !normid.is_null() {
        pub_id = if *normid != 0 { normid } else { null_mut() };
    }

    if xml_strncmp(
        pub_id,
        XML_URN_PUBID.as_ptr() as _,
        XML_URN_PUBID.to_bytes().len() as i32,
    ) == 0
    {
        urn_id = xml_catalog_unwrap_urn(pub_id);
        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            if urn_id.is_null() {
                generic_error!(
                    "Public URN ID {} expanded to null_mut()\n",
                    CStr::from_ptr(pub_id as *const i8).to_string_lossy()
                );
            } else {
                generic_error!(
                    "Public URN ID expanded to {}\n",
                    CStr::from_ptr(urn_id as *const i8).to_string_lossy()
                );
            }
        }
        ret = xml_catalog_list_xml_resolve(catal, urn_id, sys_id);
        if !urn_id.is_null() {
            xml_free(urn_id as _);
        }
        if !normid.is_null() {
            xml_free(normid as _);
        }
        return ret;
    }
    if xml_strncmp(
        sys_id,
        XML_URN_PUBID.as_ptr() as _,
        XML_URN_PUBID.to_bytes().len() as i32,
    ) == 0
    {
        urn_id = xml_catalog_unwrap_urn(sys_id);
        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            if urn_id.is_null() {
                generic_error!(
                    "System URN ID {} expanded to null_mut()\n",
                    CStr::from_ptr(sys_id as *const i8).to_string_lossy()
                );
            } else {
                generic_error!(
                    "System URN ID expanded to {}\n",
                    CStr::from_ptr(urn_id as *const i8).to_string_lossy()
                );
            }
        }
        if pub_id.is_null() {
            ret = xml_catalog_list_xml_resolve(catal, urn_id, null_mut());
        } else if xml_str_equal(pub_id, urn_id) {
            ret = xml_catalog_list_xml_resolve(catal, pub_id, null_mut());
        } else {
            ret = xml_catalog_list_xml_resolve(catal, pub_id, urn_id);
        }
        if !urn_id.is_null() {
            xml_free(urn_id as _);
        }
        if !normid.is_null() {
            xml_free(normid as _);
        }
        return ret;
    }
    while !catal.is_null() {
        if (*catal).typ == XmlCatalogEntryType::XmlCataCatalog {
            if (*catal).children.is_null() {
                xml_fetch_xml_catalog_file(catal);
            }
            if !(*catal).children.is_null() {
                ret = xml_catalog_xml_resolve((*catal).children, pub_id, sys_id);
                if !ret.is_null() {
                    break;
                } else if (*(*catal).children).depth > MAX_CATAL_DEPTH as i32 {
                    ret = null_mut();
                    break;
                }
            }
        }
        catal = (*catal).next;
    }
    if !normid.is_null() {
        xml_free(normid as _);
    }
    ret
}

/// Try to lookup the catalog local reference associated to a public ID.
///
/// Returns the local resource if found or null_mut() otherwise.
#[doc(alias = "xmlCatalogGetSGMLPublic")]
unsafe fn xml_catalog_get_sgml_public<'a>(
    catal: XmlHashTablePtr,
    mut pub_id: *const XmlChar,
) -> Option<&'a Path> {
    if catal.is_null() {
        return None;
    }

    let normid: *mut XmlChar = xml_catalog_normalize_public(pub_id);
    if !normid.is_null() {
        pub_id = if *normid != 0 { normid } else { null_mut() };
    }

    let entry: XmlCatalogEntryPtr = xml_hash_lookup(catal, pub_id) as XmlCatalogEntryPtr;
    if entry.is_null() {
        if !normid.is_null() {
            xml_free(normid as _);
        }
        return None;
    }
    if (*entry).typ == XmlCatalogEntryType::SgmlCataPublic {
        if !normid.is_null() {
            xml_free(normid as _);
        }
        return (*entry).url.as_deref();
    }
    if !normid.is_null() {
        xml_free(normid as _);
    }
    None
}

/// Try to lookup the catalog local reference for a system ID.
///
/// Returns the local resource if found or null_mut() otherwise.
#[doc(alias = "xmlCatalogGetSGMLSystem")]
unsafe fn xml_catalog_get_sgml_system<'a>(
    catal: XmlHashTablePtr,
    sys_id: *const XmlChar,
) -> Option<&'a Path> {
    if catal.is_null() {
        return None;
    }

    let entry: XmlCatalogEntryPtr = xml_hash_lookup(catal, sys_id) as XmlCatalogEntryPtr;
    if entry.is_null() {
        return None;
    }
    if (*entry).typ == XmlCatalogEntryType::SgmlCataSystem {
        return (*entry).url.as_deref();
    }
    None
}

/// Do a complete resolution lookup of an External Identifier.
///
/// Returns the URI of the resource or null_mut() if not found
#[doc(alias = "xmlCatalogSGMLResolve")]
unsafe fn xml_catalog_sgml_resolve<'a>(
    catal: XmlCatalogPtr,
    pub_id: *const XmlChar,
    sys_id: *const XmlChar,
) -> Option<&'a Path> {
    let mut ret = None;

    if (*catal).sgml.is_null() {
        return None;
    }

    if !pub_id.is_null() {
        ret = xml_catalog_get_sgml_public((*catal).sgml, pub_id);
    }
    if ret.is_some() {
        return ret;
    }
    if !sys_id.is_null() {
        ret = xml_catalog_get_sgml_system((*catal).sgml, sys_id);
    }
    if ret.is_some() {
        return ret;
    }
    None
}

/// Do a complete resolution lookup of an External Identifier
///
/// Returns the URI of the resource or null_mut() if not found, it must be freed by the caller.
#[doc(alias = "xmlACatalogResolve")]
pub unsafe fn xml_a_catalog_resolve(
    catal: XmlCatalogPtr,
    pub_id: *const XmlChar,
    sys_id: *const XmlChar,
) -> *mut XmlChar {
    let mut ret: *mut XmlChar = null_mut();

    if (pub_id.is_null() && sys_id.is_null()) || catal.is_null() {
        return null_mut();
    }

    if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
        if !pub_id.is_null() && !sys_id.is_null() {
            generic_error!(
                "Resolve: pubID {} sysID {}\n",
                CStr::from_ptr(pub_id as *const i8).to_string_lossy(),
                CStr::from_ptr(sys_id as *const i8).to_string_lossy()
            );
        } else if !pub_id.is_null() {
            generic_error!(
                "Resolve: pubID {}\n",
                CStr::from_ptr(pub_id as *const i8).to_string_lossy()
            );
        } else {
            generic_error!(
                "Resolve: sysID {}\n",
                CStr::from_ptr(sys_id as *const i8).to_string_lossy()
            );
        }
    }

    if matches!((*catal).typ, XmlCatalogType::XmlXMLCatalogType) {
        ret = xml_catalog_list_xml_resolve((*catal).xml, pub_id, sys_id);
        if ret == XML_CATAL_BREAK {
            ret = null_mut();
        }
    } else if let Some(sgml) = xml_catalog_sgml_resolve(catal, pub_id, sys_id) {
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
pub unsafe fn xml_a_catalog_resolve_public(
    catal: XmlCatalogPtr,
    pub_id: *const XmlChar,
) -> *mut XmlChar {
    let mut ret: *mut XmlChar = null_mut();

    if pub_id.is_null() || catal.is_null() {
        return null_mut();
    }

    if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
        generic_error!(
            "Resolve pubID {}\n",
            CStr::from_ptr(pub_id as *const i8).to_string_lossy()
        );
    }

    if matches!((*catal).typ, XmlCatalogType::XmlXMLCatalogType) {
        ret = xml_catalog_list_xml_resolve((*catal).xml, pub_id, null_mut());
        if ret == XML_CATAL_BREAK {
            ret = null_mut();
        }
    } else if let Some(sgml) = xml_catalog_get_sgml_public((*catal).sgml, pub_id) {
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
pub unsafe fn xml_a_catalog_resolve_system(
    catal: XmlCatalogPtr,
    sys_id: *const XmlChar,
) -> *mut XmlChar {
    let mut ret: *mut XmlChar = null_mut();

    if sys_id.is_null() || catal.is_null() {
        return null_mut();
    }

    if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
        generic_error!(
            "Resolve sysID {}\n",
            CStr::from_ptr(sys_id as *const i8).to_string_lossy()
        );
    }

    if matches!((*catal).typ, XmlCatalogType::XmlXMLCatalogType) {
        ret = xml_catalog_list_xml_resolve((*catal).xml, null_mut(), sys_id);
        if ret == XML_CATAL_BREAK {
            ret = null_mut();
        }
    } else if let Some(sgml) = xml_catalog_get_sgml_system((*catal).sgml, sys_id) {
        let sgml = CString::new(sgml.to_string_lossy().as_ref()).unwrap();
        ret = xml_strdup(sgml.as_ptr() as *const u8);
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
unsafe fn xml_catalog_xml_resolve_uri(
    catal: XmlCatalogEntryPtr,
    uri: *const XmlChar,
) -> *mut XmlChar {
    let mut ret: *mut XmlChar;
    let mut cur: XmlCatalogEntryPtr;
    let mut have_delegate: i32;
    let mut have_next: i32 = 0;
    let mut rewrite: XmlCatalogEntryPtr = null_mut();
    let mut lenrewrite: i32 = 0;
    let mut len: i32;

    if catal.is_null() {
        return null_mut();
    }

    if uri.is_null() {
        return null_mut();
    }

    if (*catal).depth > MAX_CATAL_DEPTH as i32 {
        xml_catalog_err!(
            catal,
            null_mut(),
            XmlParserErrors::XmlCatalogRecursion,
            "Detected recursion in catalog {}\n",
            CStr::from_ptr((*catal).name as *const i8).to_string_lossy(),
        );
        return null_mut();
    }

    // First tries steps 2/ 3/ 4/ if a system ID is provided.
    cur = catal;
    have_delegate = 0;
    while !cur.is_null() {
        match (*cur).typ {
            XmlCatalogEntryType::XmlCataURI => {
                if xml_str_equal(uri, (*cur).name) {
                    if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                        generic_error!(
                            "Found URI match {}\n",
                            CStr::from_ptr((*cur).name as *const i8).to_string_lossy()
                        );
                    }
                    return if let Some(url) = (*cur).url.as_deref() {
                        let url = CString::new(url.to_string_lossy().as_ref()).unwrap();
                        xml_strdup(url.as_ptr() as *const u8)
                    } else {
                        null_mut()
                    };
                }
            }
            XmlCatalogEntryType::XmlCataRewriteURI => {
                len = xml_strlen((*cur).name);
                if len > lenrewrite && xml_strncmp(uri, (*cur).name, len) == 0 {
                    lenrewrite = len;
                    rewrite = cur;
                }
            }
            XmlCatalogEntryType::XmlCataDelegateURI => {
                if xml_strncmp(uri, (*cur).name, xml_strlen((*cur).name)) == 0 {
                    have_delegate += 1;
                }
            }
            XmlCatalogEntryType::XmlCataNextCatalog => {
                have_next += 1;
            }
            _ => {}
        }
        cur = (*cur).next;
    }
    if !rewrite.is_null() {
        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            generic_error!(
                "Using rewriting rule {}\n",
                CStr::from_ptr((*rewrite).name as *const i8).to_string_lossy()
            );
        }
        if let Some(url) = (*rewrite).url.as_deref() {
            let url = CString::new(url.to_string_lossy().as_ref()).unwrap();
            ret = xml_strdup(url.as_ptr() as *const u8);
        } else {
            ret = null_mut();
        }
        if !ret.is_null() {
            ret = xml_strcat(ret, uri.add(lenrewrite as usize));
        }
        return ret;
    }
    if have_delegate != 0 {
        let mut delegates: [Option<&Path>; MAX_DELEGATE] = [None; MAX_DELEGATE];
        let mut nb_list: usize = 0;

        // Assume the entries have been sorted by decreasing substring
        // matches when the list was produced.
        cur = catal;
        'b: while !cur.is_null() {
            if matches!(
                (*cur).typ,
                XmlCatalogEntryType::XmlCataDelegateSystem
                    | XmlCatalogEntryType::XmlCataDelegateURI
            ) && xml_strncmp(uri, (*cur).name, xml_strlen((*cur).name)) == 0
            {
                for i in 0..nb_list {
                    if (*cur).url.as_deref() == delegates[i] {
                        cur = (*cur).next;
                        continue 'b;
                    }
                }
                if nb_list < MAX_DELEGATE {
                    delegates[nb_list] = (*cur).url.as_deref();
                    nb_list += 1;
                }

                if (*cur).children.is_null() {
                    xml_fetch_xml_catalog_file(cur);
                }
                if !(*cur).children.is_null() {
                    if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                        generic_error!(
                            "Trying URI delegate {}\n",
                            (*cur).url.as_deref().unwrap().display()
                        );
                    }
                    ret = xml_catalog_list_xml_resolve_uri((*cur).children, uri);
                    if !ret.is_null() {
                        return ret;
                    }
                }
            }
            cur = (*cur).next;
        }
        // Apply the cut algorithm explained in 4/
        return XML_CATAL_BREAK;
    }
    if have_next != 0 {
        cur = catal;
        while !cur.is_null() {
            if (*cur).typ == XmlCatalogEntryType::XmlCataNextCatalog {
                if (*cur).children.is_null() {
                    xml_fetch_xml_catalog_file(cur);
                }
                if !(*cur).children.is_null() {
                    ret = xml_catalog_list_xml_resolve_uri((*cur).children, uri);
                    if !ret.is_null() {
                        return ret;
                    }
                }
            }
            cur = (*cur).next;
        }
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
unsafe fn xml_catalog_list_xml_resolve_uri(
    mut catal: XmlCatalogEntryPtr,
    uri: *const XmlChar,
) -> *mut XmlChar {
    let mut ret: *mut XmlChar = null_mut();
    let urn_id: *mut XmlChar;

    if catal.is_null() {
        return null_mut();
    }
    if uri.is_null() {
        return null_mut();
    }

    if xml_strncmp(
        uri,
        XML_URN_PUBID.as_ptr() as _,
        XML_URN_PUBID.to_bytes().len() as i32,
    ) == 0
    {
        urn_id = xml_catalog_unwrap_urn(uri);
        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
            if urn_id.is_null() {
                generic_error!(
                    "URN ID {} expanded to NULL\n",
                    CStr::from_ptr(uri as *const i8).to_string_lossy()
                );
            } else {
                generic_error!(
                    "URN ID expanded to {}\n",
                    CStr::from_ptr(urn_id as *const i8).to_string_lossy()
                );
            }
        }
        ret = xml_catalog_list_xml_resolve(catal, urn_id, null_mut());
        if !urn_id.is_null() {
            xml_free(urn_id as _);
        }
        return ret;
    }
    while !catal.is_null() {
        if (*catal).typ == XmlCatalogEntryType::XmlCataCatalog {
            if (*catal).children.is_null() {
                xml_fetch_xml_catalog_file(catal);
            }
            if !(*catal).children.is_null() {
                ret = xml_catalog_xml_resolve_uri((*catal).children, uri);
                if !ret.is_null() {
                    return ret;
                }
            }
        }
        catal = (*catal).next;
    }
    ret
}

/// Do a complete resolution lookup of an URI
///
/// Returns the URI of the resource or null_mut() if not found,
/// it must be freed by the caller.
#[doc(alias = "xmlACatalogResolveURI")]
pub unsafe fn xml_a_catalog_resolve_uri(catal: XmlCatalogPtr, uri: *const XmlChar) -> *mut XmlChar {
    let mut ret: *mut XmlChar = null_mut();

    if uri.is_null() || catal.is_null() {
        return null_mut();
    }

    if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
        generic_error!(
            "Resolve URI {}\n",
            CStr::from_ptr(uri as *const i8).to_string_lossy()
        );
    }

    if matches!((*catal).typ, XmlCatalogType::XmlXMLCatalogType) {
        ret = xml_catalog_list_xml_resolve_uri((*catal).xml, uri);
        if ret == XML_CATAL_BREAK {
            ret = null_mut();
        }
    } else if let Some(sgml) = xml_catalog_sgml_resolve(catal, null_mut(), uri) {
        let sgml = CString::new(sgml.to_string_lossy().as_ref()).unwrap();
        ret = xml_strdup(sgml.as_ptr() as *const u8);
    }
    ret
}

/// Serialize an SGML Catalog entry
#[doc(alias = "xmlCatalogDumpEntry")]
#[cfg(feature = "libxml_output")]
fn xml_catalog_dump_entry(payload: *mut c_void, out: &mut impl Write) {
    let entry: XmlCatalogEntryPtr = payload as XmlCatalogEntryPtr;
    if entry.is_null() {
        return;
    }
    unsafe {
        match (*entry).typ {
            XmlCatalogEntryType::SgmlCataEntity => {
                write!(out, "ENTITY ");
            }
            XmlCatalogEntryType::SgmlCataPentity => {
                write!(out, "ENTITY %");
            }
            XmlCatalogEntryType::SgmlCataDoctype => {
                write!(out, "DOCTYPE ");
            }
            XmlCatalogEntryType::SgmlCataLinktype => {
                write!(out, "LINKTYPE ");
            }
            XmlCatalogEntryType::SgmlCataNotation => {
                write!(out, "NOTATION ");
            }
            XmlCatalogEntryType::SgmlCataPublic => {
                write!(out, "PUBLIC ");
            }
            XmlCatalogEntryType::SgmlCataSystem => {
                write!(out, "SYSTEM ");
            }
            XmlCatalogEntryType::SgmlCataDelegate => {
                write!(out, "DELEGATE ");
            }
            XmlCatalogEntryType::SgmlCataBase => {
                write!(out, "BASE ");
            }
            XmlCatalogEntryType::SgmlCataCatalog => {
                write!(out, "CATALOG ");
            }
            XmlCatalogEntryType::SgmlCataDocument => {
                write!(out, "DOCUMENT ");
            }
            XmlCatalogEntryType::SgmlCataSGMLDecl => {
                write!(out, "SGMLDECL ");
            }
            _ => {
                return;
            }
        }
        match (*entry).typ {
            XmlCatalogEntryType::SgmlCataEntity
            | XmlCatalogEntryType::SgmlCataPentity
            | XmlCatalogEntryType::SgmlCataDoctype
            | XmlCatalogEntryType::SgmlCataLinktype
            | XmlCatalogEntryType::SgmlCataNotation => {
                write!(
                    out,
                    "{}",
                    CStr::from_ptr((*entry).name as *const i8).to_string_lossy()
                );
            }
            XmlCatalogEntryType::SgmlCataPublic
            | XmlCatalogEntryType::SgmlCataSystem
            | XmlCatalogEntryType::SgmlCataSGMLDecl
            | XmlCatalogEntryType::SgmlCataDocument
            | XmlCatalogEntryType::SgmlCataCatalog
            | XmlCatalogEntryType::SgmlCataBase
            | XmlCatalogEntryType::SgmlCataDelegate => {
                write!(
                    out,
                    "\"{}\"",
                    CStr::from_ptr((*entry).name as *const i8).to_string_lossy()
                );
            }
            _ => {}
        }
        match (*entry).typ {
            XmlCatalogEntryType::SgmlCataEntity
            | XmlCatalogEntryType::SgmlCataPentity
            | XmlCatalogEntryType::SgmlCataDoctype
            | XmlCatalogEntryType::SgmlCataLinktype
            | XmlCatalogEntryType::SgmlCataNotation
            | XmlCatalogEntryType::SgmlCataPublic
            | XmlCatalogEntryType::SgmlCataSystem
            | XmlCatalogEntryType::SgmlCataDelegate => {
                write!(
                    out,
                    " \"{}\"",
                    CStr::from_ptr((*entry).value as *const i8).to_string_lossy()
                );
            }
            _ => {}
        }
        writeln!(out);
    }
}

/// Serializes a Catalog entry, called by xmlDumpXMLCatalog and recursively for group entries
#[doc(alias = "xmlDumpXMLCatalogNode")]
#[cfg(feature = "libxml_output")]
unsafe fn xml_dump_xml_catalog_node(
    catal: XmlCatalogEntryPtr,
    catalog: XmlNodePtr,
    doc: XmlDocPtr,
    ns: XmlNsPtr,
    cgroup: XmlCatalogEntryPtr,
) {
    use crate::tree::NodeCommon;

    let mut node: XmlNodePtr;
    let mut cur: XmlCatalogEntryPtr;
    // add all the catalog entries
    cur = catal;
    while !cur.is_null() {
        if (*cur).group == cgroup {
            match (*cur).typ {
                XmlCatalogEntryType::XmlCataRemoved => {}
                XmlCatalogEntryType::XmlCataBrokenCatalog | XmlCatalogEntryType::XmlCataCatalog => {
                    if cur == catal {
                        cur = (*cur).children;
                        continue;
                    }
                }
                XmlCatalogEntryType::XmlCataNextCatalog => {
                    node = xml_new_doc_node(doc, ns, c"nextCatalog".as_ptr() as _, null_mut());
                    let value = (!(*cur).value.is_null())
                        .then(|| CStr::from_ptr((*cur).value as *const i8).to_string_lossy());
                    (*node).set_prop("catalog", value.as_deref());
                    (*catalog).add_child(node);
                }
                XmlCatalogEntryType::XmlCataNone => {}
                XmlCatalogEntryType::XmlCataGroup => {
                    node = xml_new_doc_node(doc, ns, c"group".as_ptr() as _, null_mut());
                    let name = (!(*cur).name.is_null())
                        .then(|| CStr::from_ptr((*cur).name as *const i8).to_string_lossy());
                    (*node).set_prop("id", name.as_deref());
                    if !(*cur).value.is_null() {
                        let xns: XmlNsPtr =
                            (*node).search_ns_by_href(doc, XML_XML_NAMESPACE.to_str().unwrap());
                        if !xns.is_null() {
                            let value = (!(*cur).value.is_null()).then(|| {
                                CStr::from_ptr((*cur).value as *const i8).to_string_lossy()
                            });
                            (*node).set_ns_prop(xns, "base", value.as_deref());
                        }
                    }
                    match (*cur).prefer {
                        XmlCatalogPrefer::None => {}
                        XmlCatalogPrefer::Public => {
                            (*node).set_prop("prefer", Some("public"));
                        }
                        XmlCatalogPrefer::System => {
                            (*node).set_prop("prefer", Some("system"));
                        }
                    }
                    xml_dump_xml_catalog_node((*cur).next, node, doc, ns, cur);
                    (*catalog).add_child(node);
                }
                XmlCatalogEntryType::XmlCataPublic => {
                    node = xml_new_doc_node(doc, ns, c"public".as_ptr() as _, null_mut());
                    let name = (!(*cur).name.is_null())
                        .then(|| CStr::from_ptr((*cur).name as *const i8).to_string_lossy());
                    (*node).set_prop("publicId", name.as_deref());
                    let value = (!(*cur).value.is_null())
                        .then(|| CStr::from_ptr((*cur).value as *const i8).to_string_lossy());
                    (*node).set_prop("uri", value.as_deref());
                    (*catalog).add_child(node);
                }
                XmlCatalogEntryType::XmlCataSystem => {
                    node = xml_new_doc_node(doc, ns, c"system".as_ptr() as _, null_mut());
                    let name = (!(*cur).name.is_null())
                        .then(|| CStr::from_ptr((*cur).name as *const i8).to_string_lossy());
                    (*node).set_prop("systemId", name.as_deref());
                    let value = (!(*cur).value.is_null())
                        .then(|| CStr::from_ptr((*cur).value as *const i8).to_string_lossy());
                    (*node).set_prop("uri", value.as_deref());
                    (*catalog).add_child(node);
                }
                XmlCatalogEntryType::XmlCataRewriteSystem => {
                    node = xml_new_doc_node(doc, ns, c"rewriteSystem".as_ptr() as _, null_mut());
                    let name = (!(*cur).name.is_null())
                        .then(|| CStr::from_ptr((*cur).name as *const i8).to_string_lossy());
                    (*node).set_prop("systemIdStartString", name.as_deref());
                    let value = (!(*cur).value.is_null())
                        .then(|| CStr::from_ptr((*cur).value as *const i8).to_string_lossy());
                    (*node).set_prop("rewritePrefix", value.as_deref());
                    (*catalog).add_child(node);
                }
                XmlCatalogEntryType::XmlCataDelegatePublic => {
                    node = xml_new_doc_node(doc, ns, c"delegatePublic".as_ptr() as _, null_mut());
                    let name = (!(*cur).name.is_null())
                        .then(|| CStr::from_ptr((*cur).name as *const i8).to_string_lossy());
                    (*node).set_prop("publicIdStartString", name.as_deref());
                    let value = (!(*cur).value.is_null())
                        .then(|| CStr::from_ptr((*cur).value as *const i8).to_string_lossy());
                    (*node).set_prop("catalog", value.as_deref());
                    (*catalog).add_child(node);
                }
                XmlCatalogEntryType::XmlCataDelegateSystem => {
                    node = xml_new_doc_node(doc, ns, c"delegateSystem".as_ptr() as _, null_mut());
                    let name = (!(*cur).name.is_null())
                        .then(|| CStr::from_ptr((*cur).name as *const i8).to_string_lossy());
                    (*node).set_prop("systemIdStartString", name.as_deref());
                    let value = (!(*cur).value.is_null())
                        .then(|| CStr::from_ptr((*cur).value as *const i8).to_string_lossy());
                    (*node).set_prop("catalog", value.as_deref());
                    (*catalog).add_child(node);
                }
                XmlCatalogEntryType::XmlCataURI => {
                    node = xml_new_doc_node(doc, ns, c"uri".as_ptr() as _, null_mut());
                    let name = (!(*cur).name.is_null())
                        .then(|| CStr::from_ptr((*cur).name as *const i8).to_string_lossy());
                    (*node).set_prop("name", name.as_deref());
                    let value = (!(*cur).value.is_null())
                        .then(|| CStr::from_ptr((*cur).value as *const i8).to_string_lossy());
                    (*node).set_prop("uri", value.as_deref());
                    (*catalog).add_child(node);
                }
                XmlCatalogEntryType::XmlCataRewriteURI => {
                    node = xml_new_doc_node(doc, ns, c"rewriteURI".as_ptr() as _, null_mut());
                    let name = (!(*cur).name.is_null())
                        .then(|| CStr::from_ptr((*cur).name as *const i8).to_string_lossy());
                    (*node).set_prop("uriStartString", name.as_deref());
                    let value = (!(*cur).value.is_null())
                        .then(|| CStr::from_ptr((*cur).value as *const i8).to_string_lossy());
                    (*node).set_prop("rewritePrefix", value.as_deref());
                    (*catalog).add_child(node);
                }
                XmlCatalogEntryType::XmlCataDelegateURI => {
                    node = xml_new_doc_node(doc, ns, c"delegateURI".as_ptr() as _, null_mut());
                    let name = (!(*cur).name.is_null())
                        .then(|| CStr::from_ptr((*cur).name as *const i8).to_string_lossy());
                    (*node).set_prop("uriStartString", name.as_deref());
                    let value = (!(*cur).value.is_null())
                        .then(|| CStr::from_ptr((*cur).value as *const i8).to_string_lossy());
                    (*node).set_prop("catalog", value.as_deref());
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
        cur = (*cur).next;
    }
}

#[doc(alias = "xmlDumpXMLCatalog")]
#[cfg(feature = "libxml_output")]
unsafe fn xml_dump_xml_catalog<'a>(out: impl Write + 'a, catal: XmlCatalogEntryPtr) -> i32 {
    // Rebuild a catalog

    use crate::{io::XmlOutputBuffer, tree::NodeCommon};
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

    xml_dump_xml_catalog_node(catal, catalog, doc, ns, null_mut());

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

/// Dump the given catalog to the given file.
#[doc(alias = "xmlACatalogDump")]
#[cfg(feature = "libxml_output")]
pub unsafe fn xml_a_catalog_dump<'a>(catal: XmlCatalogPtr, mut out: impl Write + 'a) {
    if catal.is_null() {
        return;
    }

    if matches!((*catal).typ, XmlCatalogType::XmlXMLCatalogType) {
        xml_dump_xml_catalog(out, (*catal).xml);
    } else if let Some(sgml) = XmlHashTableRef::from_raw((*catal).sgml) {
        sgml.scan(|data, _, _, _| {
            if !data.0.is_null() {
                xml_catalog_dump_entry(data.0, &mut out);
            }
        });
    }
}

/// Free the memory allocated to a full chained list of Catalog entries
#[doc(alias = "xmlFreeCatalogEntryList")]
unsafe fn xml_free_catalog_entry_list(mut ret: XmlCatalogEntryPtr) {
    let mut next: XmlCatalogEntryPtr;

    while !ret.is_null() {
        next = (*ret).next;
        xml_free_catalog_entry(ret as _, null_mut());
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
    if !(*catal).sgml.is_null() {
        xml_hash_free((*catal).sgml, Some(xml_free_catalog_entry));
    }
    xml_free(catal as _);
}

/// Check is a catalog is empty
///
/// Returns 1 if the catalog is empty, 0 if not, amd -1 in case of error.
#[doc(alias = "xmlCatalogIsEmpty")]
pub unsafe fn xml_catalog_is_empty(catal: XmlCatalogPtr) -> i32 {
    if catal.is_null() {
        return -1;
    }

    if matches!((*catal).typ, XmlCatalogType::XmlXMLCatalogType) {
        if (*catal).xml.is_null() {
            return 1;
        }
        if (*(*catal).xml).typ != XmlCatalogEntryType::XmlCataCatalog
            && (*(*catal).xml).typ != XmlCatalogEntryType::XmlCataBrokenCatalog
        {
            return -1;
        }
        if (*(*catal).xml).children.is_null() {
            return 1;
        }
        return 0;
    } else {
        if (*catal).sgml.is_null() {
            return 1;
        }
        let res: i32 = xml_hash_size((*catal).sgml);
        if res == 0 {
            return 1;
        }
        if res < 0 {
            return -1;
        }
    }
    0
}

// Whether the catalog support was initialized.
static XML_CATALOG_INITIALIZED: AtomicBool = AtomicBool::new(false);
// The default catalog in use by the application
static XML_DEFAULT_CATALOG: AtomicPtr<XmlCatalog> = AtomicPtr::new(null_mut());

const XML_XML_DEFAULT_CATALOG: &CStr =
    unsafe { transmute(concatcp!("file://", SYSCONFDIR, "/xml/catalog", "\0")) };
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

    if !getenv(c"XML_DEBUG_CATALOG".as_ptr() as _).is_null() {
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

    if !getenv(c"XML_DEBUG_CATALOG".as_ptr() as _).is_null() {
        XML_DEBUG_CATALOGS.store(1, Ordering::Release);
    }

    if XML_DEFAULT_CATALOG.load(Ordering::Relaxed).is_null() {
        let mut catalogs: *const c_char;
        let mut path: *mut c_char;
        let mut cur: *const c_char;
        let mut paths: *const c_char;
        let mut nextent: *mut XmlCatalogEntryPtr;

        catalogs = getenv(c"XML_CATALOG_FILES".as_ptr() as _);
        if catalogs.is_null() {
            #[cfg(target_os = "windows")]
            {
                let hmodule: *mut c_void;
                hmodule = GetModuleHandleA(c"libxml2.dll".as_ptr() as _);
                if hmodule.is_null() {
                    hmodule = GetModuleHandleA(null_mut());
                }
                if !hmodule.is_null() {
                    let buf: [c_char; 256];
                    let len: c_ulong = GetModuleFileNameA(hmodule, buf, 255);
                    if len != 0 {
                        let mut p: *mut c_char = buf.as_mut_ptr().add(len);
                        while (*p != b'\\' && p > buf) {
                            p = p.sub(1);
                        }
                        if p != buf.as_mut_ptr() {
                            let uri: *mut XmlChar;
                            strncpy(p, c"\\..\\etc\\catalog".as_ptr() as _, 255 - (p - buf));
                            uri = xml_canonic_path(buf);
                            if !uri.is_null() {
                                strncpy(XML_XML_DEFAULT_CATALOG, uri, 255);
                                xml_free(uri as _);
                            }
                        }
                    }
                }
                catalogs = XML_XML_DEFAULT_CATALOG.as_ptr() as _;
            }
            #[cfg(not(target_os = "windows"))]
            {
                catalogs = XML_XML_DEFAULT_CATALOG.as_ptr() as _;
            }
        }

        let catal: XmlCatalogPtr = xml_create_new_catalog(
            XmlCatalogType::XmlXMLCatalogType,
            XML_CATALOG_DEFAULT_PREFER,
        );
        if !catal.is_null() {
            /* the XML_CATALOG_FILES envvar is allowed to contain a
            space-separated list of entries. */
            cur = catalogs;
            nextent = &raw mut (*catal).xml;
            while *cur != b'\0' as i8 {
                while xml_is_blank_char(*cur as u32) {
                    cur = cur.add(1);
                }
                if *cur != 0 {
                    paths = cur;
                    while *cur != 0 && !xml_is_blank_char(*cur as u32) {
                        cur = cur.add(1);
                    }
                    path = xml_strndup(paths as _, cur.offset_from(paths) as _) as _;
                    if !path.is_null() {
                        *nextent = xml_new_catalog_entry(
                            XmlCatalogEntryType::XmlCataCatalog,
                            null_mut(),
                            null_mut(),
                            path as _,
                            XML_CATALOG_DEFAULT_PREFER,
                            null_mut(),
                        );
                        if !(*nextent).is_null() {
                            nextent = &raw mut (*(*nextent)).next;
                        }
                        xml_free(path as _);
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

    let ret: i32 = xml_expand_catalog(default_catalog, filename);
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
            xml_free_catalog_entry(children as _, null_mut());
            children = next;
        }
        (*catal).dealloc = 0;
        xml_free_catalog_entry(catal as _, null_mut());
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

    xml_a_catalog_dump(XML_DEFAULT_CATALOG.load(Ordering::Relaxed), out);
}

/// Do a complete resolution lookup of an External Identifier
///
/// Returns the URI of the resource or null_mut() if not found,
/// it must be freed by the caller.
#[doc(alias = "xmlCatalogResolve")]
pub unsafe fn xml_catalog_resolve(pub_id: *const XmlChar, sys_id: *const XmlChar) -> *mut XmlChar {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    let ret: *mut XmlChar =
        xml_a_catalog_resolve(XML_DEFAULT_CATALOG.load(Ordering::Relaxed), pub_id, sys_id);
    ret
}

/// Try to lookup the catalog resource for a system ID
///
/// Returns the resource if found or null_mut() otherwise,
/// the value returned must be freed by the caller.
#[doc(alias = "xmlCatalogResolveSystem")]
pub unsafe fn xml_catalog_resolve_system(sys_id: *const XmlChar) -> *mut XmlChar {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    let ret: *mut XmlChar =
        xml_a_catalog_resolve_system(XML_DEFAULT_CATALOG.load(Ordering::Relaxed), sys_id);
    ret
}

/// Try to lookup the catalog reference associated to a public ID
///
/// Returns the resource if found or null_mut() otherwise,
/// the value returned must be freed by the caller.
#[doc(alias = "xmlCatalogResolvePublic")]
pub unsafe fn xml_catalog_resolve_public(pub_id: *const XmlChar) -> *mut XmlChar {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    let ret: *mut XmlChar =
        xml_a_catalog_resolve_public(XML_DEFAULT_CATALOG.load(Ordering::Relaxed), pub_id);
    ret
}

/// Do a complete resolution lookup of an URI
///
/// Returns the URI of the resource or null_mut() if not found,
/// it must be freed by the caller.
#[doc(alias = "xmlCatalogResolveURI")]
pub unsafe fn xml_catalog_resolve_uri(uri: *const XmlChar) -> *mut XmlChar {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    let ret: *mut XmlChar =
        xml_a_catalog_resolve_uri(XML_DEFAULT_CATALOG.load(Ordering::Relaxed), uri);
    ret
}

/// Add an entry in the catalog, it may overwrite existing but
/// different entries.
/// If called before any other catalog routine, allows to override the
/// default shared catalog put in place by `xmlInitializeCatalog()`.
///
/// Returns 0 if successful, -1 otherwise
#[doc(alias = "xmlCatalogAdd")]
pub unsafe fn xml_catalog_add(
    typ: *const XmlChar,
    orig: *const XmlChar,
    replace: *const XmlChar,
) -> i32 {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog_data();
    }

    let mutex = XML_CATALOG_MUTEX.load(Ordering::Acquire);
    xml_rmutex_lock(mutex);
    /*
     * Specific case where one want to override the default catalog
     * put in place by xmlInitializeCatalog();
     */
    let mut default_catalog = XML_DEFAULT_CATALOG.load(Ordering::Acquire);
    if default_catalog.is_null() && xml_str_equal(typ, c"catalog".as_ptr() as _) {
        default_catalog = xml_create_new_catalog(
            XmlCatalogType::XmlXMLCatalogType,
            XML_CATALOG_DEFAULT_PREFER,
        ) as _;

        if !default_catalog.is_null() {
            (*default_catalog).xml = xml_new_catalog_entry(
                XmlCatalogEntryType::XmlCataCatalog,
                null_mut(),
                orig,
                null_mut(),
                XML_CATALOG_DEFAULT_PREFER,
                null_mut(),
            );
        }
        XML_DEFAULT_CATALOG.store(default_catalog, Ordering::Release);
        xml_rmutex_unlock(mutex);
        return 0;
    }

    let res: i32 = xml_a_catalog_add(default_catalog, typ, orig, replace);
    XML_DEFAULT_CATALOG.store(default_catalog, Ordering::Release);
    xml_rmutex_unlock(mutex);
    res
}

/// Remove an entry from the catalog
///
/// Returns the number of entries removed if successful, -1 otherwise
#[doc(alias = "xmlCatalogRemove")]
pub unsafe fn xml_catalog_remove(value: *const XmlChar) -> i32 {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    let mutex = XML_CATALOG_MUTEX.load(Ordering::Acquire);
    xml_rmutex_lock(mutex);
    let res: i32 = xml_a_catalog_remove(XML_DEFAULT_CATALOG.load(Ordering::Relaxed), value);
    xml_rmutex_unlock(mutex);
    res
}

/// Parse an XML file and build a tree.
/// It's like `xmlParseFile()` except it bypass all catalog lookups.
///
/// Returns the resulting document tree or null_mut() in case of error
#[doc(alias = "xmlParseCatalogFile")]
pub unsafe fn xml_parse_catalog_file(filename: *const c_char) -> XmlDocPtr {
    let ret: XmlDocPtr;
    let mut directory: *mut c_char = null_mut();

    let ctxt: XmlParserCtxtPtr = xml_new_parser_ctxt();
    if ctxt.is_null() {
        xml_catalog_err_memory("allocating parser context");
        return null_mut();
    }

    if filename.is_null() {
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    }

    let Some(buf) = XmlParserInputBuffer::from_uri(
        CStr::from_ptr(filename).to_string_lossy().as_ref(),
        XmlCharEncoding::None,
    ) else {
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
        let canonic = xml_canonic_path(filename as _);
        if !canonic.is_null() {
            (*input_stream).filename = Some(
                CStr::from_ptr(canonic as *const i8)
                    .to_string_lossy()
                    .into_owned(),
            );
            xml_free(canonic as _);
        }
    }
    std::ptr::write(
        &raw mut (*input_stream).buf,
        Some(Rc::new(RefCell::new(buf))),
    );
    (*input_stream).reset_base();

    (*ctxt).input_push(input_stream);
    if (*ctxt).directory.is_none() {
        directory = xml_parser_get_directory(filename);
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
    let res: i32 = xml_convert_sgml_catalog(XML_DEFAULT_CATALOG.load(Ordering::Relaxed));
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
        null_mut(),
        url,
        null_mut(),
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

/// Do a complete resolution lookup of an External Identifier using a
/// document's private catalog list
///
/// Returns the URI of the resource or null_mut() if not found,
/// it must be freed by the caller.
#[doc(alias = "xmlCatalogLocalResolve")]
pub unsafe fn xml_catalog_local_resolve(
    catalogs: XmlCatalogEntryPtr,
    pub_id: *const XmlChar,
    sys_id: *const XmlChar,
) -> *mut XmlChar {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    if pub_id.is_null() && sys_id.is_null() {
        return null_mut();
    }

    if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
        if !pub_id.is_null() && !sys_id.is_null() {
            generic_error!(
                "Local Resolve: pubID {} sysID {}\n",
                CStr::from_ptr(pub_id as *const i8).to_string_lossy(),
                CStr::from_ptr(sys_id as *const i8).to_string_lossy()
            );
        } else if !pub_id.is_null() {
            generic_error!(
                "Local Resolve: pubID {}\n",
                CStr::from_ptr(pub_id as *const i8).to_string_lossy()
            );
        } else {
            generic_error!(
                "Local Resolve: sysID {}\n",
                CStr::from_ptr(sys_id as *const i8).to_string_lossy()
            );
        }
    }

    let catal: XmlCatalogEntryPtr = catalogs as XmlCatalogEntryPtr;
    if catal.is_null() {
        return null_mut();
    }
    let ret: *mut XmlChar = xml_catalog_list_xml_resolve(catal, pub_id, sys_id);
    if !ret.is_null() && ret != XML_CATAL_BREAK {
        return ret;
    }
    null_mut()
}

/// Do a complete resolution lookup of an URI using a
/// document's private catalog list
///
/// Returns the URI of the resource or null_mut() if not found,
/// it must be freed by the caller.
#[doc(alias = "xmlCatalogLocalResolveURI")]
pub unsafe fn xml_catalog_local_resolve_uri(
    catalogs: XmlCatalogEntryPtr,
    uri: *const XmlChar,
) -> *mut XmlChar {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    if uri.is_null() {
        return null_mut();
    }

    if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
        generic_error!(
            "Resolve URI {}\n",
            CStr::from_ptr(uri as *const i8).to_string_lossy()
        );
    }

    let catal: XmlCatalogEntryPtr = catalogs as XmlCatalogEntryPtr;
    if catal.is_null() {
        return null_mut();
    }
    let ret: *mut XmlChar = xml_catalog_list_xml_resolve_uri(catal, uri);
    if !ret.is_null() && ret != XML_CATAL_BREAK {
        return ret;
    }
    null_mut()
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
pub unsafe fn xml_catalog_get_system(sys_id: *const XmlChar) -> *const XmlChar {
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

    if sys_id.is_null() {
        return null_mut();
    }

    // Check first the XML catalogs
    let default_catalog = XML_DEFAULT_CATALOG.load(Ordering::Acquire);
    if !default_catalog.is_null() {
        ret = xml_catalog_list_xml_resolve((*default_catalog).xml, null_mut(), sys_id);
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
        return if let Some(sgml) = xml_catalog_get_sgml_system((*default_catalog).sgml, sys_id) {
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
pub unsafe fn xml_catalog_get_public(pub_id: *const XmlChar) -> *const XmlChar {
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

    if pub_id.is_null() {
        return null_mut();
    }

    // Check first the XML catalogs
    let default_catalog = XML_DEFAULT_CATALOG.load(Ordering::Acquire);
    if !default_catalog.is_null() {
        ret = xml_catalog_list_xml_resolve((*default_catalog).xml, pub_id, null_mut());
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
        return if let Some(sgml) = xml_catalog_get_sgml_public((*default_catalog).sgml, pub_id) {
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
    fn test_xml_acatalog_add() {
        let lock = TEST_CATALOG_LOCK.lock().unwrap();
        #[cfg(feature = "catalog")]
        unsafe {
            let mut leaks = 0;

            for n_catal in 0..GEN_NB_XML_CATALOG_PTR {
                for n_type in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_orig in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_replace in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            let mem_base = xml_mem_blocks();
                            let catal = gen_xml_catalog_ptr(n_catal, 0);
                            let typ = gen_const_xml_char_ptr(n_type, 1);
                            let orig = gen_const_xml_char_ptr(n_orig, 2);
                            let replace = gen_const_xml_char_ptr(n_replace, 3);

                            let ret_val = xml_a_catalog_add(catal, typ, orig, replace);
                            desret_int(ret_val);
                            des_xml_catalog_ptr(n_catal, catal, 0);
                            des_const_xml_char_ptr(n_type, typ, 1);
                            des_const_xml_char_ptr(n_orig, orig, 2);
                            des_const_xml_char_ptr(n_replace, replace, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlACatalogAdd",
                                    xml_mem_blocks() - mem_base
                                );
                                eprint!(" {}", n_catal);
                                eprint!(" {}", n_type);
                                eprint!(" {}", n_orig);
                                eprintln!(" {}", n_replace);
                            }
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlACatalogAdd()");
        }
        drop(lock);
    }

    #[test]
    fn test_xml_acatalog_dump() {
        let lock = TEST_CATALOG_LOCK.lock().unwrap();
        #[cfg(all(feature = "catalog", feature = "libxml_output"))]
        unsafe {
            let mut leaks = 0;

            for n_catal in 0..GEN_NB_XML_CATALOG_PTR {
                for n_out in 0..GEN_NB_FILE_PTR {
                    let mem_base = xml_mem_blocks();
                    let catal = gen_xml_catalog_ptr(n_catal, 0);
                    let out = gen_file_ptr(n_out, 1).unwrap();

                    xml_a_catalog_dump(catal, out);
                    des_xml_catalog_ptr(n_catal, catal, 0);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlACatalogDump",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_catal);
                        eprintln!(" {}", n_out);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlACatalogDump()");
        }
        drop(lock);
    }

    #[test]
    fn test_xml_acatalog_remove() {
        let lock = TEST_CATALOG_LOCK.lock().unwrap();
        #[cfg(feature = "catalog")]
        unsafe {
            let mut leaks = 0;

            for n_catal in 0..GEN_NB_XML_CATALOG_PTR {
                for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let catal = gen_xml_catalog_ptr(n_catal, 0);
                    let value = gen_const_xml_char_ptr(n_value, 1);

                    let ret_val = xml_a_catalog_remove(catal, value);
                    desret_int(ret_val);
                    des_xml_catalog_ptr(n_catal, catal, 0);
                    des_const_xml_char_ptr(n_value, value, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlACatalogRemove",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_catal);
                        eprintln!(" {}", n_value);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlACatalogRemove()");
        }
        drop(lock);
    }

    #[test]
    fn test_xml_acatalog_resolve() {
        let lock = TEST_CATALOG_LOCK.lock().unwrap();
        #[cfg(feature = "catalog")]
        unsafe {
            let mut leaks = 0;

            for n_catal in 0..GEN_NB_XML_CATALOG_PTR {
                for n_pub_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_sys_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let catal = gen_xml_catalog_ptr(n_catal, 0);
                        let pub_id = gen_const_xml_char_ptr(n_pub_id, 1);
                        let sys_id = gen_const_xml_char_ptr(n_sys_id, 2);

                        let ret_val = xml_a_catalog_resolve(catal, pub_id, sys_id);
                        desret_xml_char_ptr(ret_val);
                        des_xml_catalog_ptr(n_catal, catal, 0);
                        des_const_xml_char_ptr(n_pub_id, pub_id, 1);
                        des_const_xml_char_ptr(n_sys_id, sys_id, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlACatalogResolve",
                                xml_mem_blocks() - mem_base
                            );
                            eprint!(" {}", n_catal);
                            eprint!(" {}", n_pub_id);
                            eprintln!(" {}", n_sys_id);
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlACatalogResolve()"
            );
        }
        drop(lock);
    }

    #[test]
    fn test_xml_acatalog_resolve_public() {
        let lock = TEST_CATALOG_LOCK.lock().unwrap();
        #[cfg(feature = "catalog")]
        unsafe {
            let mut leaks = 0;

            for n_catal in 0..GEN_NB_XML_CATALOG_PTR {
                for n_pub_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let catal = gen_xml_catalog_ptr(n_catal, 0);
                    let pub_id = gen_const_xml_char_ptr(n_pub_id, 1);

                    let ret_val = xml_a_catalog_resolve_public(catal, pub_id);
                    desret_xml_char_ptr(ret_val);
                    des_xml_catalog_ptr(n_catal, catal, 0);
                    des_const_xml_char_ptr(n_pub_id, pub_id, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlACatalogResolvePublic",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_catal);
                        eprintln!(" {}", n_pub_id);
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlACatalogResolvePublic()"
            );
        }
        drop(lock);
    }

    #[test]
    fn test_xml_acatalog_resolve_system() {
        let lock = TEST_CATALOG_LOCK.lock().unwrap();
        #[cfg(feature = "catalog")]
        unsafe {
            let mut leaks = 0;

            for n_catal in 0..GEN_NB_XML_CATALOG_PTR {
                for n_sys_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let catal = gen_xml_catalog_ptr(n_catal, 0);
                    let sys_id = gen_const_xml_char_ptr(n_sys_id, 1);

                    let ret_val = xml_a_catalog_resolve_system(catal, sys_id);
                    desret_xml_char_ptr(ret_val);
                    des_xml_catalog_ptr(n_catal, catal, 0);
                    des_const_xml_char_ptr(n_sys_id, sys_id, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlACatalogResolveSystem",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_catal);
                        eprintln!(" {}", n_sys_id);
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlACatalogResolveSystem()"
            );
        }
        drop(lock);
    }

    #[test]
    fn test_xml_acatalog_resolve_uri() {
        let lock = TEST_CATALOG_LOCK.lock().unwrap();
        #[cfg(feature = "catalog")]
        unsafe {
            let mut leaks = 0;

            for n_catal in 0..GEN_NB_XML_CATALOG_PTR {
                for n_uri in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let catal = gen_xml_catalog_ptr(n_catal, 0);
                    let uri = gen_const_xml_char_ptr(n_uri, 1);

                    let ret_val = xml_a_catalog_resolve_uri(catal, uri);
                    desret_xml_char_ptr(ret_val);
                    des_xml_catalog_ptr(n_catal, catal, 0);
                    des_const_xml_char_ptr(n_uri, uri, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlACatalogResolveURI",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_catal);
                        eprintln!(" {}", n_uri);
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlACatalogResolveURI()"
            );
        }
        drop(lock);
    }

    #[test]
    fn test_xml_catalog_add() {
        let lock = TEST_CATALOG_LOCK.lock().unwrap();
        #[cfg(feature = "catalog")]
        unsafe {
            let mut leaks = 0;

            for n_type in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_orig in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_replace in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let typ = gen_const_xml_char_ptr(n_type, 0);
                        let orig = gen_const_xml_char_ptr(n_orig, 1);
                        let replace = gen_const_xml_char_ptr(n_replace, 2);

                        let ret_val = xml_catalog_add(typ as *const XmlChar, orig, replace);
                        desret_int(ret_val);
                        des_const_xml_char_ptr(n_type, typ, 0);
                        des_const_xml_char_ptr(n_orig, orig, 1);
                        des_const_xml_char_ptr(n_replace, replace, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlCatalogAdd",
                                xml_mem_blocks() - mem_base
                            );
                            eprint!(" {}", n_type);
                            eprint!(" {}", n_orig);
                            eprintln!(" {}", n_replace);
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlCatalogAdd()");
        }
        drop(lock);
    }

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
    fn test_xml_catalog_is_empty() {
        let lock = TEST_CATALOG_LOCK.lock().unwrap();
        #[cfg(feature = "catalog")]
        unsafe {
            let mut leaks = 0;

            for n_catal in 0..GEN_NB_XML_CATALOG_PTR {
                let mem_base = xml_mem_blocks();
                let catal = gen_xml_catalog_ptr(n_catal, 0);

                let ret_val = xml_catalog_is_empty(catal);
                desret_int(ret_val);
                des_xml_catalog_ptr(n_catal, catal, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlCatalogIsEmpty",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_catal);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlCatalogIsEmpty()");
        }
        drop(lock);
    }

    #[test]
    fn test_xml_catalog_remove() {
        let lock = TEST_CATALOG_LOCK.lock().unwrap();
        #[cfg(feature = "catalog")]
        unsafe {
            for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let value = gen_const_xml_char_ptr(n_value, 0);

                let ret_val = xml_catalog_remove(value as *const XmlChar);
                desret_int(ret_val);
                des_const_xml_char_ptr(n_value, value, 0);
                reset_last_error();
            }
        }
        drop(lock);
    }

    #[test]
    fn test_xml_catalog_resolve() {
        let lock = TEST_CATALOG_LOCK.lock().unwrap();
        #[cfg(feature = "catalog")]
        unsafe {
            for n_pub_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_sys_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let pub_id = gen_const_xml_char_ptr(n_pub_id, 0);
                    let sys_id = gen_const_xml_char_ptr(n_sys_id, 1);

                    let ret_val = xml_catalog_resolve(pub_id as *const XmlChar, sys_id);
                    desret_xml_char_ptr(ret_val);
                    des_const_xml_char_ptr(n_pub_id, pub_id, 0);
                    des_const_xml_char_ptr(n_sys_id, sys_id, 1);
                    reset_last_error();
                }
            }
        }
        drop(lock);
    }

    #[test]
    fn test_xml_catalog_resolve_public() {
        let lock = TEST_CATALOG_LOCK.lock().unwrap();
        #[cfg(feature = "catalog")]
        unsafe {
            let mut leaks = 0;

            for n_pub_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let pub_id = gen_const_xml_char_ptr(n_pub_id, 0);

                let ret_val = xml_catalog_resolve_public(pub_id as *const XmlChar);
                desret_xml_char_ptr(ret_val);
                des_const_xml_char_ptr(n_pub_id, pub_id, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlCatalogResolvePublic",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_pub_id);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlCatalogResolvePublic()"
            );
        }
        drop(lock);
    }

    #[test]
    fn test_xml_catalog_resolve_system() {
        let lock = TEST_CATALOG_LOCK.lock().unwrap();
        #[cfg(feature = "catalog")]
        unsafe {
            let mut leaks = 0;

            for n_sys_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let sys_id = gen_const_xml_char_ptr(n_sys_id, 0);

                let ret_val = xml_catalog_resolve_system(sys_id as *const XmlChar);
                desret_xml_char_ptr(ret_val);
                des_const_xml_char_ptr(n_sys_id, sys_id, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlCatalogResolveSystem",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_sys_id);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlCatalogResolveSystem()"
            );
        }
        drop(lock);
    }

    #[test]
    fn test_xml_catalog_resolve_uri() {
        let lock = TEST_CATALOG_LOCK.lock().unwrap();
        #[cfg(feature = "catalog")]
        unsafe {
            let mut leaks = 0;

            for n_uri in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let uri = gen_const_xml_char_ptr(n_uri, 0);

                let ret_val = xml_catalog_resolve_uri(uri as *const XmlChar);
                desret_xml_char_ptr(ret_val);
                des_const_xml_char_ptr(n_uri, uri, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlCatalogResolveURI",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_uri);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlCatalogResolveURI()"
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
    fn test_xml_convert_sgmlcatalog() {
        let lock = TEST_CATALOG_LOCK.lock().unwrap();
        #[cfg(feature = "catalog")]
        unsafe {
            let mut leaks = 0;

            for n_catal in 0..GEN_NB_XML_CATALOG_PTR {
                let mem_base = xml_mem_blocks();
                let catal = gen_xml_catalog_ptr(n_catal, 0);

                let ret_val = xml_convert_sgml_catalog(catal);
                desret_int(ret_val);
                des_xml_catalog_ptr(n_catal, catal, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlConvertSGMLCatalog",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_catal);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlConvertSGMLCatalog()"
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

    #[test]
    fn test_xml_parse_catalog_file() {
        let lock = TEST_CATALOG_LOCK.lock().unwrap();
        #[cfg(feature = "catalog")]
        unsafe {
            let mut leaks = 0;

            for n_filename in 0..GEN_NB_FILEPATH {
                let mem_base = xml_mem_blocks();
                let filename = gen_filepath(n_filename, 0);

                let ret_val = xml_parse_catalog_file(filename);
                desret_xml_doc_ptr(ret_val);
                des_filepath(n_filename, filename, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlParseCatalogFile",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_filename);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlParseCatalogFile()"
            );
        }
        drop(lock);
    }
}
