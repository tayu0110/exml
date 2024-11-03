//! Provide methods and data structures for XML Catalogs.  
//! This module is based on `libxml/catalog.h`, `catalog.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    cell::RefCell,
    ffi::{c_char, c_int, c_long, c_uint, CStr},
    mem::{size_of, transmute, zeroed},
    os::raw::c_void,
    ptr::{addr_of_mut, null, null_mut},
    rc::Rc,
    sync::atomic::{AtomicBool, AtomicI32, AtomicPtr, Ordering},
};

use const_format::concatcp;
use libc::{close, fprintf, getenv, memset, open, read, snprintf, stat, FILE, O_RDONLY};

use crate::{
    __xml_raise_error, generic_error,
    hash::XmlHashTableRef,
    io::{xml_parser_get_directory, XmlParserInputBuffer},
    libxml::{
        chvalid::xml_is_blank_char,
        globals::{xml_free, xml_malloc, xml_malloc_atomic, xml_realloc},
        hash::{
            xml_hash_add_entry, xml_hash_create, xml_hash_free, xml_hash_lookup,
            xml_hash_remove_entry, xml_hash_scan, xml_hash_size, XmlHashTable, XmlHashTablePtr,
        },
        parser::{
            xml_free_parser_ctxt, xml_new_parser_ctxt, xml_parse_document, XmlParserCtxtPtr,
            XmlParserInputPtr,
        },
        parser_internals::{input_push, xml_new_input_stream, XML_MAX_NAMELEN},
        threads::{
            xml_free_rmutex, xml_get_thread_id, xml_new_rmutex, xml_rmutex_lock, xml_rmutex_unlock,
            XmlRMutex,
        },
        tree::{
            xml_add_child, xml_doc_get_root_element, xml_free_doc, xml_free_ns, xml_get_ns_prop,
            xml_get_prop, xml_new_doc, xml_new_doc_node, xml_new_dtd, xml_new_ns,
            xml_node_get_base, xml_save_format_file_to, xml_search_ns_by_href, xml_set_ns_prop,
            xml_set_prop, XmlDocPtr, XmlDtdPtr, XmlNodePtr, XmlNsPtr, XML_XML_NAMESPACE,
        },
        uri::{xml_build_uri, xml_canonic_path},
        xmlerror::XmlParserErrors,
        xmlstring::{
            xml_str_equal, xml_strcat, xml_strdup, xml_strlen, xml_strncmp, xml_strndup, XmlChar,
        },
    },
    SYSCONFDIR,
};

use super::{
    chvalid::{xml_is_digit, xml_is_pubid_char},
    hash::CVoidWrapper,
    parser_internals::xml_is_letter,
};

/**
 * XML_CATALOGS_NAMESPACE:
 *
 * The namespace for the XML Catalogs elements.
 */
pub(crate) const XML_CATALOGS_NAMESPACE: &CStr = c"urn:oasis:names:tc:entity:xmlns:xml:catalog";
/**
 * XML_CATALOG_PI:
 *
 * The specific XML Catalog Processing Instruction name.
 */
pub(crate) const XML_CATALOG_PI: &CStr = c"oasis-xml-catalog";

/*
 * The API is voluntarily limited to general cataloging.
 */
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlCatalogPrefer {
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum XmlCatalogEntryType {
    XmlCataRemoved = -1,
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
    XmlCataUri,
    XmlCataRewriteUri,
    XmlCataDelegateUri,
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
    SgmlCataSgmldecl,
}

#[repr(C)]
pub(crate) enum XmlCatalogType {
    XmlXmlCatalogType = 1,
    XmlSgmlCatalogType,
}

const XML_MAX_SGML_CATA_DEPTH: usize = 10;

pub type XmlCatalogPtr = *mut XmlCatalog;
#[repr(C)]
pub struct XmlCatalog {
    typ: XmlCatalogType, /* either XML or SGML */

    /*
     * SGML Catalogs are stored as a simple hash table of catalog entries
     * Catalog stack to check against overflows when building the
     * SGML catalog
     */
    catal_tab: [*mut c_char; XML_MAX_SGML_CATA_DEPTH], /* stack of catals */
    catal_nr: c_int,                                   /* Number of current catal streams */
    catal_max: c_int,                                  /* Max number of catal streams */
    sgml: XmlHashTablePtr,

    /*
     * XML Catalogs are stored as a tree of Catalog entries
     */
    prefer: XmlCatalogPrefer,
    xml: XmlCatalogEntryPtr,
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
    url: *mut XmlChar, /* The expanded URL using the base */
    prefer: XmlCatalogPrefer,
    dealloc: c_int,
    depth: c_int,
    group: *mut XmlCatalogEntry,
}

/*
 * Those are preferences
 */
static XML_DEBUG_CATALOGS: AtomicI32 = AtomicI32::new(0); /* used for debugging */
static mut XML_CATALOG_DEFAULT_ALLOW: XmlCatalogAllow = XmlCatalogAllow::All;
static mut XML_CATALOG_DEFAULT_PREFER: XmlCatalogPrefer = XmlCatalogPrefer::Public;

/**
 * xmlCatalogErrMemory:
 * @extra:  extra information
 *
 * Handle an out of memory condition
 */
unsafe extern "C" fn xml_catalog_err_memory(extra: *const c_char) {
    __xml_raise_error!(
        None,
        None,
        None,
        null_mut(),
        null_mut(),
        XmlErrorDomain::XmlFromCatalog,
        XmlParserErrors::XmlErrNoMemory,
        XmlErrorLevel::XmlErrError,
        null_mut(),
        0,
        (!extra.is_null()).then(|| CStr::from_ptr(extra).to_string_lossy().into_owned().into()),
        None,
        None,
        0,
        0,
        c"Memory allocation failed : %s\n".as_ptr(),
        extra
    );
}

/**
 * xmlCreateNewCatalog:
 * @type:  type of catalog
 * @prefer:  the PUBLIC vs. SYSTEM current preference value
 *
 * create a new Catalog, this type is shared both by XML and
 * SGML catalogs, but the acceptable types values differs.
 *
 * Returns the xmlCatalogPtr or null_mut() in case of error
 */
unsafe extern "C" fn xml_create_new_catalog(
    typ: XmlCatalogType,
    prefer: XmlCatalogPrefer,
) -> XmlCatalogPtr {
    let ret: XmlCatalogPtr = xml_malloc(size_of::<XmlCatalog>()) as XmlCatalogPtr;
    if ret.is_null() {
        xml_catalog_err_memory(c"allocating catalog".as_ptr());
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlCatalog>());
    (*ret).typ = typ;
    (*ret).catal_nr = 0;
    (*ret).catal_max = XML_MAX_SGML_CATA_DEPTH as _;
    (*ret).prefer = prefer;
    if matches!((*ret).typ, XmlCatalogType::XmlSgmlCatalogType) {
        (*ret).sgml = xml_hash_create(10);
    }
    ret
}

/*
 * Operations on a given catalog.
 */
/**
 * xmlNewCatalog:
 * @sgml:  should this create an SGML catalog
 *
 * create a new Catalog.
 *
 * Returns the xmlCatalogPtr or null_mut() in case of error
 */
pub unsafe extern "C" fn xml_new_catalog(sgml: c_int) -> XmlCatalogPtr {
    let catal: XmlCatalogPtr;

    if sgml != 0 {
        catal = xml_create_new_catalog(
            XmlCatalogType::XmlSgmlCatalogType,
            XML_CATALOG_DEFAULT_PREFER,
        );
        if !catal.is_null() && (*catal).sgml.is_null() {
            (*catal).sgml = xml_hash_create(10);
        }
    } else {
        catal = xml_create_new_catalog(
            XmlCatalogType::XmlXmlCatalogType,
            XML_CATALOG_DEFAULT_PREFER,
        );
    }
    catal
}

/**
 * xmlLoadFileContent:
 * @filename:  a file path
 *
 * Load a file content into memory.
 *
 * Returns a pointer to the 0 terminated string or null_mut() in case of error
 */
unsafe extern "C" fn xml_load_file_content(filename: *const c_char) -> *mut XmlChar {
    let mut info: stat = unsafe { zeroed() };

    if filename.is_null() {
        return null_mut();
    }

    if stat(filename, addr_of_mut!(info)) < 0 {
        return null_mut();
    }

    let fd: c_int = open(filename, O_RDONLY);
    if fd < 0 {
        return null_mut();
    }
    let size: c_long = info.st_size;
    let content: *mut XmlChar = xml_malloc_atomic(size as usize + 10) as _;
    if content.is_null() {
        xml_catalog_err_memory(c"allocating catalog data".as_ptr());
        close(fd);
        return null_mut();
    }
    let len: c_int = read(fd, content as _, size as _) as _;
    close(fd);
    if len < 0 {
        xml_free(content as _);
        return null_mut();
    }
    *content.add(len as usize) = 0;

    content
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

/**
 * xmlParseSGMLCatalogComment:
 * @cur:  the current character
 *
 * Skip a comment in an SGML catalog
 *
 * Returns new current character
 */
unsafe extern "C" fn xml_parse_sgmlcatalog_comment(mut cur: *const XmlChar) -> *const XmlChar {
    if *cur.add(0) != b'-' || *cur.add(1) != b'-' {
        return cur;
    }
    SKIP!(cur, 2);
    while *cur.add(0) != 0 && (*cur.add(0) != b'-' || *cur.add(1) != b'-') {
        NEXT!(cur);
    }
    if *cur.add(0) == 0 {
        return null_mut();
    }
    cur.add(2)
}

/**
 * xmlParseSGMLCatalogName:
 * @cur:  the current character
 * @name:  the return location
 *
 * Parse an SGML catalog name
 *
 * Returns new current character and store the value in @name
 */
unsafe extern "C" fn xml_parse_sgml_catalog_name(
    mut cur: *const XmlChar,
    name: *mut *mut XmlChar,
) -> *const XmlChar {
    let mut buf: [XmlChar; XML_MAX_NAMELEN + 5] = [0; XML_MAX_NAMELEN + 5];
    let mut len: usize = 0;
    let mut c: c_uint;

    *name = null_mut();

    /*
     * Handler for more complex cases
     */
    c = *cur as _;
    if !xml_is_letter(c) && c != b'_' as u32 && c != b':' as u32 {
        return null_mut();
    }

    while xml_is_letter(c)
        || xml_is_digit(c)
        || c == b'.' as u32
        || c == b'-' as u32
        || c == b'_' as u32
        || c == b':' as u32
    {
        buf[len] = c as _;
        len += 1;
        cur = cur.add(1);
        c = *cur as _;
        if len >= XML_MAX_NAMELEN {
            return null_mut();
        }
    }
    *name = xml_strndup(buf.as_mut_ptr() as _, len as i32);
    cur
}

/**
 * xmlParseSGMLCatalogPubid:
 * @cur:  the current character
 * @id:  the return location
 *
 * Parse an SGML catalog ID
 *
 * Returns new current character and store the value in @id
 */
unsafe extern "C" fn xml_parse_sgml_catalog_pubid(
    mut cur: *const XmlChar,
    id: *mut *mut XmlChar,
) -> *const XmlChar {
    let mut buf: *mut XmlChar;
    let mut tmp: *mut XmlChar;
    let mut len: usize = 0;
    let mut size: usize = 50;
    let stop: XmlChar;

    *id = null_mut();

    if RAW!(cur) == b'"' {
        NEXT!(cur);
        stop = b'"';
    } else if RAW!(cur) == b'\'' {
        NEXT!(cur);
        stop = b'\'';
    } else {
        stop = b' ';
    }
    buf = xml_malloc_atomic(size) as _;
    if buf.is_null() {
        xml_catalog_err_memory(c"allocating public ID".as_ptr() as _);
        return null_mut();
    }
    while xml_is_pubid_char(*cur as u32) || *cur == b'?' {
        if *cur == stop && stop != b' ' {
            break;
        }
        if stop == b' ' && xml_is_blank_char(*cur as u32) {
            break;
        }
        if len + 1 >= size {
            size *= 2;
            tmp = xml_realloc(buf as _, size) as _;
            if tmp.is_null() {
                xml_catalog_err_memory(c"allocating public ID".as_ptr() as _);
                xml_free(buf as _);
                return null_mut();
            }
            buf = tmp;
        }
        *buf.add(len) = *cur;
        len += 1;
        NEXT!(cur);
    }
    *buf.add(len) = 0;
    if stop == b' ' {
        if !xml_is_blank_char(*cur as u32) {
            xml_free(buf as _);
            return null_mut();
        }
    } else {
        if *cur != stop {
            xml_free(buf as _);
            return null_mut();
        }
        NEXT!(cur);
    }
    *id = buf;
    cur
}

/**
 * xmlCatalogNormalizePublic:
 * @pubID:  the public ID string
 *
 *  Normalizes the Public Identifier
 *
 * Implements 6.2. Public Identifier Normalization
 * from http://www.oasis-open.org/committees/entity/spec-2001-08-06.html
 *
 * Returns the new string or null_mut(), the string must be deallocated
 *         by the caller.
 */
unsafe extern "C" fn xml_catalog_normalize_public(pub_id: *const XmlChar) -> *mut XmlChar {
    let mut ok: c_int = 1;
    let mut white: c_int;
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
        /* is normalized */
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

/**
 * xmlNewCatalogEntry:
 * @type:  type of entry
 * @name:  name of the entry
 * @value:  value of the entry
 * @prefer:  the PUBLIC vs. SYSTEM current preference value
 * @group:  for members of a group, the group entry
 *
 * create a new Catalog entry, this type is shared both by XML and
 * SGML catalogs, but the acceptable types values differs.
 *
 * Returns the xmlCatalogEntryPtr or null_mut() in case of error
 */
unsafe extern "C" fn xml_new_catalog_entry(
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
        xml_catalog_err_memory(c"allocating catalog entry".as_ptr() as _);
        return null_mut();
    }
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
        (*ret).url = xml_strdup(url);
    } else {
        (*ret).url = null_mut();
    }
    (*ret).prefer = prefer;
    (*ret).dealloc = 0;
    (*ret).depth = 0;
    (*ret).group = group;
    ret
}

/**
 * xmlFreeCatalogEntry:
 * @payload:  a Catalog entry
 *
 * Free the memory allocated to a Catalog entry
 */
extern "C" fn xml_free_catalog_entry(payload: *mut c_void, _name: *const XmlChar) {
    let ret: XmlCatalogEntryPtr = payload as XmlCatalogEntryPtr;
    if ret.is_null() {
        return;
    }
    /*
     * Entries stored in the file hash must be deallocated
     * only by the file hash cleaner !
     */
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
        if !(*ret).url.is_null() {
            xml_free((*ret).url as _);
        }
        xml_free(ret as _);
    }
}

/**
 * xmlExpandCatalog:
 * @catal:  a catalog
 * @filename:  a file path
 *
 * Load the catalog and expand the existing catal structure.
 * This can be either an XML Catalog or an SGML Catalog
 *
 * Returns 0 in case of success, -1 in case of error
 */
unsafe extern "C" fn xml_expand_catalog(catal: XmlCatalogPtr, filename: *const c_char) -> c_int {
    let ret: c_int;

    if catal.is_null() || filename.is_null() {
        return -1;
    }

    if matches!((*catal).typ, XmlCatalogType::XmlSgmlCatalogType) {
        let content: *mut XmlChar = xml_load_file_content(filename);
        if content.is_null() {
            return -1;
        }

        ret = xml_parse_sgml_catalog(catal, content, filename, 0);
        if ret < 0 {
            xml_free(content as _);
            return -1;
        }
        xml_free(content as _);
    } else {
        let mut cur: XmlCatalogEntryPtr;
        let tmp: XmlCatalogEntryPtr = xml_new_catalog_entry(
            XmlCatalogEntryType::XmlCataCatalog,
            null_mut(),
            null_mut(),
            filename as _,
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

/**
 * xmlParseSGMLCatalog:
 * @catal:  the SGML Catalog
 * @value:  the content of the SGML Catalog serialization
 * @file:  the filepath for the catalog
 * @super:  should this be handled as a Super Catalog in which case
 *          parsing is not recursive
 *
 * Parse an SGML catalog content and fill up the @catal hash table with
 * the new entries found.
 *
 * Returns 0 in case of success, -1 in case of error.
 */
unsafe extern "C" fn xml_parse_sgml_catalog(
    catal: XmlCatalogPtr,
    value: *const XmlChar,
    file: *const c_char,
    is_super: c_int,
) -> c_int {
    let mut cur: *const XmlChar = value;
    let mut base: *mut XmlChar;
    let mut res: c_int;

    if cur.is_null() || file.is_null() {
        return -1;
    }
    base = xml_strdup(file as _);

    while !cur.is_null() && *cur.add(0) != 0 {
        SKIP_BLANKS!(cur);
        if *cur.add(0) == 0 {
            break;
        }
        if *cur.add(0) == b'-' && *cur.add(1) == b'-' {
            cur = xml_parse_sgmlcatalog_comment(cur);
            if cur.is_null() {
                /* error */
                break;
            }
        } else {
            let mut sysid: *mut XmlChar = null_mut();
            let mut name: *mut XmlChar = null_mut();
            let mut typ: XmlCatalogEntryType = XmlCatalogEntryType::XmlCataNone;

            cur = xml_parse_sgml_catalog_name(cur, addr_of_mut!(name));
            if cur.is_null() || name.is_null() {
                /* error */
                break;
            }
            if !xml_is_blank_char(*cur as u32) {
                /* error */
                xml_free(name as _);
                break;
            }
            SKIP_BLANKS!(cur);
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
                typ = XmlCatalogEntryType::SgmlCataSgmldecl;
            } else if xml_str_equal(name, c"DOCUMENT".as_ptr() as _) {
                typ = XmlCatalogEntryType::SgmlCataDocument;
            } else if xml_str_equal(name, c"CATALOG".as_ptr() as _) {
                typ = XmlCatalogEntryType::SgmlCataCatalog;
            } else if xml_str_equal(name, c"BASE".as_ptr() as _) {
                typ = XmlCatalogEntryType::SgmlCataBase;
            } else if xml_str_equal(name, c"OVERRIDE".as_ptr() as _) {
                xml_free(name as _);
                cur = xml_parse_sgml_catalog_name(cur, addr_of_mut!(name));
                if name.is_null() {
                    /* error */
                    break;
                }
                xml_free(name as _);
                continue;
            }
            xml_free(name as _);
            name = null_mut();

            match typ {
                ty @ XmlCatalogEntryType::SgmlCataEntity
                | ty @ XmlCatalogEntryType::SgmlCataPentity
                | ty @ XmlCatalogEntryType::SgmlCataDoctype
                | ty @ XmlCatalogEntryType::SgmlCataLinktype
                | ty @ XmlCatalogEntryType::SgmlCataNotation => 'to_break: {
                    if matches!(ty, XmlCatalogEntryType::SgmlCataEntity) && *cur == b'%' {
                        typ = XmlCatalogEntryType::SgmlCataPentity;
                    }
                    cur = xml_parse_sgml_catalog_name(cur, addr_of_mut!(name));
                    if cur.is_null() {
                        /* error */
                        break 'to_break;
                    }
                    if !xml_is_blank_char(*cur as u32) {
                        /* error */
                        break 'to_break;
                    }
                    SKIP_BLANKS!(cur);
                    cur = xml_parse_sgml_catalog_pubid(cur, addr_of_mut!(sysid));
                    if cur.is_null() {
                        /* error */
                        break 'to_break;
                    }
                }
                XmlCatalogEntryType::SgmlCataPublic
                | XmlCatalogEntryType::SgmlCataSystem
                | XmlCatalogEntryType::SgmlCataDelegate => 'to_break: {
                    cur = xml_parse_sgml_catalog_pubid(cur, addr_of_mut!(name));
                    if cur.is_null() {
                        /* error */
                        break 'to_break;
                    }
                    if !matches!(typ, XmlCatalogEntryType::SgmlCataSystem) {
                        let normid: *mut XmlChar = xml_catalog_normalize_public(name);
                        if !normid.is_null() {
                            if !name.is_null() {
                                xml_free(name as _);
                            }
                            if *normid != 0 {
                                name = normid;
                            } else {
                                xml_free(normid as _);
                                name = null_mut();
                            }
                        }
                    }
                    if !xml_is_blank_char(*cur as u32) {
                        /* error */
                        break 'to_break;
                    }
                    SKIP_BLANKS!(cur);
                    cur = xml_parse_sgml_catalog_pubid(cur, addr_of_mut!(sysid));
                    if cur.is_null() {
                        /* error */
                        break 'to_break;
                    }
                }
                XmlCatalogEntryType::SgmlCataBase
                | XmlCatalogEntryType::SgmlCataCatalog
                | XmlCatalogEntryType::SgmlCataDocument
                | XmlCatalogEntryType::SgmlCataSgmldecl => 'to_break: {
                    cur = xml_parse_sgml_catalog_pubid(cur, addr_of_mut!(sysid));
                    if cur.is_null() {
                        /* error */
                        break 'to_break;
                    }
                }
                _ => {}
            }
            if cur.is_null() {
                if !name.is_null() {
                    xml_free(name as _);
                }
                if !sysid.is_null() {
                    xml_free(sysid as _);
                }
                break;
            } else if matches!(typ, XmlCatalogEntryType::SgmlCataBase) {
                if !base.is_null() {
                    xml_free(base as _);
                }
                base = xml_strdup(sysid);
            } else if matches!(
                typ,
                XmlCatalogEntryType::SgmlCataPublic | XmlCatalogEntryType::SgmlCataSystem
            ) {
                let filename: *mut XmlChar = xml_build_uri(sysid, base);
                if !filename.is_null() {
                    let entry: XmlCatalogEntryPtr = xml_new_catalog_entry(
                        typ,
                        name,
                        filename,
                        null_mut(),
                        XmlCatalogPrefer::None,
                        null_mut(),
                    );
                    res = xml_hash_add_entry((*catal).sgml, name, entry as _);
                    if res < 0 {
                        xml_free_catalog_entry(entry as _, null_mut());
                    }
                    xml_free(filename as _);
                }
            } else if matches!(typ, XmlCatalogEntryType::SgmlCataCatalog) {
                if is_super != 0 {
                    let entry: XmlCatalogEntryPtr = xml_new_catalog_entry(
                        typ,
                        sysid,
                        null_mut(),
                        null_mut(),
                        XmlCatalogPrefer::None,
                        null_mut(),
                    );
                    res = xml_hash_add_entry((*catal).sgml, sysid, entry as _);
                    if res < 0 {
                        xml_free_catalog_entry(entry as _, null_mut());
                    }
                } else {
                    let filename: *mut XmlChar = xml_build_uri(sysid, base);
                    if !filename.is_null() {
                        xml_expand_catalog(catal, filename as _);
                        xml_free(filename as _);
                    }
                }
            }
            /*
             * drop anything else we won't handle it
             */
            if !name.is_null() {
                xml_free(name as _);
            }
            if !sysid.is_null() {
                xml_free(sysid as _);
            }
        }
    }
    if !base.is_null() {
        xml_free(base as _);
    }
    if cur.is_null() {
        return -1;
    }
    0
}

/**
 * xmlLoadACatalog:
 * @filename:  a file path
 *
 * Load the catalog and build the associated data structures.
 * This can be either an XML Catalog or an SGML Catalog
 * It will recurse in SGML CATALOG entries. On the other hand XML
 * Catalogs are not handled recursively.
 *
 * Returns the catalog parsed or null_mut() in case of error
 */
pub unsafe extern "C" fn xml_load_a_catalog(filename: *const c_char) -> XmlCatalogPtr {
    let mut first: *mut XmlChar;
    let catal: XmlCatalogPtr;
    let ret: c_int;

    let content: *mut XmlChar = xml_load_file_content(filename);
    if content.is_null() {
        return null_mut();
    }

    first = content;

    while *first != 0
        && *first != b'-'
        && *first != b'<'
        && !((*first >= b'A' && *first <= b'Z') || (*first >= b'a' && *first <= b'z'))
    {
        first = first.add(1);
    }

    if *first != b'<' {
        catal = xml_create_new_catalog(
            XmlCatalogType::XmlSgmlCatalogType,
            XML_CATALOG_DEFAULT_PREFER,
        );
        if catal.is_null() {
            xml_free(content as _);
            return null_mut();
        }
        ret = xml_parse_sgml_catalog(catal, content, filename, 0);
        if ret < 0 {
            xml_free_catalog(catal);
            xml_free(content as _);
            return null_mut();
        }
    } else {
        catal = xml_create_new_catalog(
            XmlCatalogType::XmlXmlCatalogType,
            XML_CATALOG_DEFAULT_PREFER,
        );
        if catal.is_null() {
            xml_free(content as _);
            return null_mut();
        }
        (*catal).xml = xml_new_catalog_entry(
            XmlCatalogEntryType::XmlCataCatalog,
            null_mut(),
            null_mut(),
            filename as _,
            XML_CATALOG_DEFAULT_PREFER,
            null_mut(),
        );
    }
    xml_free(content as _);
    catal
}

/**
 * xmlLoadSGMLSuperCatalog:
 * @filename:  a file path
 *
 * Load an SGML super catalog. It won't expand CATALOG or DELEGATE
 * references. This is only needed for manipulating SGML Super Catalogs
 * like adding and removing CATALOG or DELEGATE entries.
 *
 * Returns the catalog parsed or null_mut() in case of error
 */
pub unsafe extern "C" fn xml_load_sgml_super_catalog(filename: *const c_char) -> XmlCatalogPtr {
    let content: *mut XmlChar = xml_load_file_content(filename);
    if content.is_null() {
        return null_mut();
    }

    let catal: XmlCatalogPtr = xml_create_new_catalog(
        XmlCatalogType::XmlSgmlCatalogType,
        XML_CATALOG_DEFAULT_PREFER,
    );
    if catal.is_null() {
        xml_free(content as _);
        return null_mut();
    }

    let ret: c_int = xml_parse_sgml_catalog(catal, content, filename, 1);
    xml_free(content as _);
    if ret < 0 {
        xml_free_catalog(catal);
        return null_mut();
    }
    catal
}

// /**
//  * xmlCatalogConvertEntry:
//  * @entry:  the entry
//  * @catal:  pointer to the catalog being converted
//  *
//  * Convert one entry from the catalog
//  */
// unsafe extern "C" fn xml_catalog_convert_entry(
//     payload: *mut c_void,
//     data: *mut c_void,
//     _name: *const XmlChar,
// ) {
//     let entry: XmlCatalogEntryPtr = payload as XmlCatalogEntryPtr;
//     let catal: XmlCatalogPtr = data as XmlCatalogPtr;
//     if entry.is_null() || catal.is_null() || (*catal).sgml.is_null() || (*catal).xml.is_null() {
//         return;
//     }
//     match (*entry).typ {
//         XmlCatalogEntryType::SgmlCataEntity => {
//             (*entry).typ = XmlCatalogEntryType::XmlCataPublic;
//         }
//         XmlCatalogEntryType::SgmlCataPentity => {
//             (*entry).typ = XmlCatalogEntryType::XmlCataPublic;
//         }
//         XmlCatalogEntryType::SgmlCataDoctype => {
//             (*entry).typ = XmlCatalogEntryType::XmlCataPublic;
//         }
//         XmlCatalogEntryType::SgmlCataLinktype => {
//             (*entry).typ = XmlCatalogEntryType::XmlCataPublic;
//         }
//         XmlCatalogEntryType::SgmlCataNotation => {
//             (*entry).typ = XmlCatalogEntryType::XmlCataPublic;
//         }
//         XmlCatalogEntryType::SgmlCataPublic => {
//             (*entry).typ = XmlCatalogEntryType::XmlCataPublic;
//         }
//         XmlCatalogEntryType::SgmlCataSystem => {
//             (*entry).typ = XmlCatalogEntryType::XmlCataSystem;
//         }
//         XmlCatalogEntryType::SgmlCataDelegate => {
//             (*entry).typ = XmlCatalogEntryType::XmlCataDelegatePublic;
//         }
//         XmlCatalogEntryType::SgmlCataCatalog => {
//             (*entry).typ = XmlCatalogEntryType::XmlCataCatalog;
//         }
//         _ => {
//             xml_hash_remove_entry((*catal).sgml, (*entry).name, Some(xml_free_catalog_entry));
//             return;
//         }
//     }
//     /*
//      * Conversion successful, remove from the SGML catalog
//      * and add it to the default XML one
//      */
//     xml_hash_remove_entry((*catal).sgml, (*entry).name, None);
//     (*entry).parent = (*catal).xml;
//     (*entry).next = null_mut();
//     if (*(*catal).xml).children.is_null() {
//         (*(*catal).xml).children = entry;
//     } else {
//         let mut prev: XmlCatalogEntryPtr;

//         prev = (*(*catal).xml).children;
//         while !(*prev).next.is_null() {
//             prev = (*prev).next;
//         }
//         (*prev).next = entry;
//     }
// }

/**
 * xmlConvertSGMLCatalog:
 * @catal: the catalog
 *
 * Convert all the SGML catalog entries as XML ones
 *
 * Returns the number of entries converted if successful, -1 otherwise
 */
pub unsafe extern "C" fn xml_convert_sgml_catalog(catal: XmlCatalogPtr) -> c_int {
    if catal.is_null() || !matches!((*catal).typ, XmlCatalogType::XmlSgmlCatalogType) {
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
        /*
         * Conversion successful, remove from the SGML catalog
         * and add it to the default XML one
         */
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

/*
 * Hash table containing all the trees of XML catalogs parsed by
 * the application.
 */
static XML_CATALOG_XMLFILES: AtomicPtr<XmlHashTable<'static, CVoidWrapper>> =
    AtomicPtr::new(null_mut());

/*
 * A mutex for modifying the shared global catalog(s)
 * xmlDefaultCatalog tree.
 * It also protects xmlCatalogXMLFiles
 * The core of this readers/writer scheme is in xmlFetchXMLCatalogFile()
 */
static XML_CATALOG_MUTEX: AtomicPtr<XmlRMutex> = AtomicPtr::new(null_mut());

/**
 * xmlCatalogErr:
 * @catal: the Catalog entry
 * @node: the context node
 * @msg:  the error message
 * @extra:  extra information
 *
 * Handle a catalog error
 */
unsafe extern "C" fn xml_catalog_err(
    catal: XmlCatalogEntryPtr,
    node: XmlNodePtr,
    error: XmlParserErrors,
    msg: *const c_char,
    str1: *const XmlChar,
    str2: *const XmlChar,
    str3: *const XmlChar,
) {
    __xml_raise_error!(
        None,
        None,
        None,
        catal as _,
        node as _,
        XmlErrorDomain::XmlFromCatalog,
        error,
        XmlErrorLevel::XmlErrError,
        null_mut(),
        0,
        (!str1.is_null()).then(|| CStr::from_ptr(str1 as *const i8)
            .to_string_lossy()
            .into_owned()
            .into()),
        (!str2.is_null()).then(|| CStr::from_ptr(str2 as *const i8)
            .to_string_lossy()
            .into_owned()
            .into()),
        (!str3.is_null()).then(|| CStr::from_ptr(str3 as *const i8)
            .to_string_lossy()
            .into_owned()
            .into()),
        0,
        0,
        msg,
        str1,
        str2,
        str3
    );
}

/**
 * xmlParseXMLCatalogOneNode:
 * @cur:  the XML node
 * @type:  the type of Catalog entry
 * @name:  the name of the node
 * @attrName:  the attribute holding the value
 * @uriAttrName:  the attribute holding the URI-Reference
 * @prefer:  the PUBLIC vs. SYSTEM current preference value
 * @cgroup:  the group which includes this node
 *
 * Finishes the examination of an XML tree node of a catalog and build
 * a Catalog entry from it.
 *
 * Returns the new Catalog entry node or null_mut() in case of error.
 */
unsafe extern "C" fn xml_parse_xml_catalog_one_node(
    cur: XmlNodePtr,
    typ: XmlCatalogEntryType,
    name: *const XmlChar,
    attr_name: *const XmlChar,
    uri_attr_name: *const XmlChar,
    prefer: XmlCatalogPrefer,
    cgroup: XmlCatalogEntryPtr,
) -> XmlCatalogEntryPtr {
    let mut ok: c_int = 1;
    let mut name_value: *mut XmlChar = null_mut();

    let mut ret: XmlCatalogEntryPtr = null_mut();

    if !attr_name.is_null() {
        name_value = xml_get_prop(cur, attr_name);
        if name_value.is_null() {
            xml_catalog_err(
                ret,
                cur,
                XmlParserErrors::XmlCatalogMissingAttr,
                c"%s entry lacks '%s'\n".as_ptr() as _,
                name,
                attr_name,
                null_mut(),
            );
            ok = 0;
        }
    }
    let uri_value: *mut XmlChar = xml_get_prop(cur, uri_attr_name);
    if uri_value.is_null() {
        xml_catalog_err(
            ret,
            cur,
            XmlParserErrors::XmlCatalogMissingAttr,
            c"%s entry lacks '%s'\n".as_ptr() as _,
            name,
            uri_attr_name,
            null_mut(),
        );
        ok = 0;
    }
    if ok == 0 {
        if !name_value.is_null() {
            xml_free(name_value as _);
        }
        if !uri_value.is_null() {
            xml_free(uri_value as _);
        }
        return null_mut();
    }

    let base: *mut XmlChar = xml_node_get_base((*cur).doc, cur);
    let url: *mut XmlChar = xml_build_uri(uri_value, base);
    if !url.is_null() {
        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) > 1 {
            if !name_value.is_null() {
                generic_error!(
                    "Found {}: '{}' '{}'\n",
                    CStr::from_ptr(name as *const i8).to_string_lossy(),
                    CStr::from_ptr(name_value as *const i8).to_string_lossy(),
                    CStr::from_ptr(url as *const i8).to_string_lossy()
                );
            } else {
                generic_error!(
                    "Found {}: '{}'\n",
                    CStr::from_ptr(name as *const i8).to_string_lossy(),
                    CStr::from_ptr(url as *const i8).to_string_lossy()
                );
            }
        }
        ret = xml_new_catalog_entry(typ, name_value, uri_value, url, prefer, cgroup);
    } else {
        xml_catalog_err(
            ret,
            cur,
            XmlParserErrors::XmlCatalogEntryBroken,
            c"%s entry '%s' broken ?: %s\n".as_ptr() as _,
            name,
            uri_attr_name,
            uri_value,
        );
    }
    if !name_value.is_null() {
        xml_free(name_value as _);
    }
    if !uri_value.is_null() {
        xml_free(uri_value as _);
    }
    if !base.is_null() {
        xml_free(base as _);
    }
    if !url.is_null() {
        xml_free(url as _);
    }
    ret
}

/**
 * xmlParseXMLCatalogNode:
 * @cur:  the XML node
 * @prefer:  the PUBLIC vs. SYSTEM current preference value
 * @parent:  the parent Catalog entry
 * @cgroup:  the group which includes this node
 *
 * Examines an XML tree node of a catalog and build
 * a Catalog entry from it adding it to its parent. The examination can
 * be recursive.
 */
unsafe extern "C" fn xml_parse_xml_catalog_node(
    cur: XmlNodePtr,
    mut prefer: XmlCatalogPrefer,
    parent: XmlCatalogEntryPtr,
    cgroup: XmlCatalogEntryPtr,
) {
    let mut base: *mut XmlChar = null_mut();
    let mut entry: XmlCatalogEntryPtr = null_mut();

    if cur.is_null() {
        return;
    }
    if xml_str_equal((*cur).name, c"group".as_ptr() as _) {
        let mut prop: *mut XmlChar;
        let mut pref: XmlCatalogPrefer = XmlCatalogPrefer::None;

        prop = xml_get_prop(cur, c"prefer".as_ptr() as _);
        if !prop.is_null() {
            if xml_str_equal(prop, c"system".as_ptr() as _) {
                prefer = XmlCatalogPrefer::System;
            } else if xml_str_equal(prop, c"public".as_ptr() as _) {
                prefer = XmlCatalogPrefer::Public;
            } else {
                xml_catalog_err(
                    parent,
                    cur,
                    XmlParserErrors::XmlCatalogPreferValue,
                    c"Invalid value for prefer: '%s'\n".as_ptr() as _,
                    prop,
                    null_mut(),
                    null_mut(),
                );
            }
            xml_free(prop as _);
            pref = prefer;
        }
        prop = xml_get_prop(cur, c"id".as_ptr() as _);
        base = xml_get_ns_prop(cur, c"base".as_ptr() as _, XML_XML_NAMESPACE.as_ptr() as _);
        entry = xml_new_catalog_entry(
            XmlCatalogEntryType::XmlCataGroup,
            prop,
            base,
            null_mut(),
            pref,
            cgroup,
        );
        xml_free(prop as _);
    } else if xml_str_equal((*cur).name, c"public".as_ptr() as _) {
        entry = xml_parse_xml_catalog_one_node(
            cur,
            XmlCatalogEntryType::XmlCataPublic,
            c"public".as_ptr() as _,
            c"publicId".as_ptr() as _,
            c"uri".as_ptr() as _,
            prefer,
            cgroup,
        );
    } else if xml_str_equal((*cur).name, c"system".as_ptr() as _) {
        entry = xml_parse_xml_catalog_one_node(
            cur,
            XmlCatalogEntryType::XmlCataSystem,
            c"system".as_ptr() as _,
            c"systemId".as_ptr() as _,
            c"uri".as_ptr() as _,
            prefer,
            cgroup,
        );
    } else if xml_str_equal((*cur).name, c"rewriteSystem".as_ptr() as _) {
        entry = xml_parse_xml_catalog_one_node(
            cur,
            XmlCatalogEntryType::XmlCataRewriteSystem,
            c"rewriteSystem".as_ptr() as _,
            c"systemIdStartString".as_ptr() as _,
            c"rewritePrefix".as_ptr() as _,
            prefer,
            cgroup,
        );
    } else if xml_str_equal((*cur).name, c"delegatePublic".as_ptr() as _) {
        entry = xml_parse_xml_catalog_one_node(
            cur,
            XmlCatalogEntryType::XmlCataDelegatePublic,
            c"delegatePublic".as_ptr() as _,
            c"publicIdStartString".as_ptr() as _,
            c"catalog".as_ptr() as _,
            prefer,
            cgroup,
        );
    } else if xml_str_equal((*cur).name, c"delegateSystem".as_ptr() as _) {
        entry = xml_parse_xml_catalog_one_node(
            cur,
            XmlCatalogEntryType::XmlCataDelegateSystem,
            c"delegateSystem".as_ptr() as _,
            c"systemIdStartString".as_ptr() as _,
            c"catalog".as_ptr() as _,
            prefer,
            cgroup,
        );
    } else if xml_str_equal((*cur).name, c"uri".as_ptr() as _) {
        entry = xml_parse_xml_catalog_one_node(
            cur,
            XmlCatalogEntryType::XmlCataUri,
            c"uri".as_ptr() as _,
            c"name".as_ptr() as _,
            c"uri".as_ptr() as _,
            prefer,
            cgroup,
        );
    } else if xml_str_equal((*cur).name, c"rewriteURI".as_ptr() as _) {
        entry = xml_parse_xml_catalog_one_node(
            cur,
            XmlCatalogEntryType::XmlCataRewriteUri,
            c"rewriteURI".as_ptr() as _,
            c"uriStartString".as_ptr() as _,
            c"rewritePrefix".as_ptr() as _,
            prefer,
            cgroup,
        );
    } else if xml_str_equal((*cur).name, c"delegateURI".as_ptr() as _) {
        entry = xml_parse_xml_catalog_one_node(
            cur,
            XmlCatalogEntryType::XmlCataDelegateUri,
            c"delegateURI".as_ptr() as _,
            c"uriStartString".as_ptr() as _,
            c"catalog".as_ptr() as _,
            prefer,
            cgroup,
        );
    } else if xml_str_equal((*cur).name, c"nextCatalog".as_ptr() as _) {
        entry = xml_parse_xml_catalog_one_node(
            cur,
            XmlCatalogEntryType::XmlCataNextCatalog,
            c"nextCatalog".as_ptr() as _,
            null_mut(),
            c"catalog".as_ptr() as _,
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
            /*
             * Recurse to propagate prefer to the subtree
             * (xml:base handling is automated)
             */
            xml_parse_xml_catalog_node_list((*cur).children, prefer, parent, entry);
        }
    }
    if !base.is_null() {
        xml_free(base as _);
    }
}

/**
 * xmlParseXMLCatalogNodeList:
 * @cur:  the XML node list of siblings
 * @prefer:  the PUBLIC vs. SYSTEM current preference value
 * @parent:  the parent Catalog entry
 * @cgroup:  the group which includes this list
 *
 * Examines a list of XML sibling nodes of a catalog and build
 * a list of Catalog entry from it adding it to the parent.
 * The examination will recurse to examine node subtrees.
 */
unsafe extern "C" fn xml_parse_xml_catalog_node_list(
    mut cur: XmlNodePtr,
    prefer: XmlCatalogPrefer,
    parent: XmlCatalogEntryPtr,
    cgroup: XmlCatalogEntryPtr,
) {
    while !cur.is_null() {
        if !(*cur).ns.is_null()
            && !(*(*cur).ns).href.load(Ordering::Relaxed).is_null()
            && xml_str_equal(
                (*(*cur).ns).href.load(Ordering::Relaxed) as _,
                XML_CATALOGS_NAMESPACE.as_ptr() as _,
            )
        {
            xml_parse_xml_catalog_node(cur, prefer, parent, cgroup);
        }
        cur = (*cur).next;
    }
    /* TODO: sort the list according to REWRITE lengths and prefer value */
}

/**
 * xmlParseXMLCatalogFile:
 * @prefer:  the PUBLIC vs. SYSTEM current preference value
 * @filename:  the filename for the catalog
 *
 * Parses the catalog file to extract the XML tree and then analyze the
 * tree to build a list of Catalog entries corresponding to this catalog
 *
 * Returns the resulting Catalog entries list
 */
unsafe extern "C" fn xml_parse_xml_catalog_file(
    mut prefer: XmlCatalogPrefer,
    filename: *const XmlChar,
) -> XmlCatalogEntryPtr {
    let mut cur: XmlNodePtr;
    let prop: *mut XmlChar;
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

    cur = xml_doc_get_root_element(doc);
    if !cur.is_null()
        && xml_str_equal((*cur).name, c"catalog".as_ptr() as _)
        && !(*cur).ns.is_null()
        && !(*(*cur).ns).href.load(Ordering::Relaxed).is_null()
        && xml_str_equal(
            (*(*cur).ns).href.load(Ordering::Relaxed),
            XML_CATALOGS_NAMESPACE.as_ptr() as _,
        )
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

        prop = xml_get_prop(cur, c"prefer".as_ptr() as _);
        if !prop.is_null() {
            if xml_str_equal(prop, c"system".as_ptr() as _) {
                prefer = XmlCatalogPrefer::System;
            } else if xml_str_equal(prop, c"public".as_ptr() as _) {
                prefer = XmlCatalogPrefer::Public;
            } else {
                xml_catalog_err(
                    null_mut(),
                    cur,
                    XmlParserErrors::XmlCatalogPreferValue,
                    c"Invalid value for prefer: '%s'\n".as_ptr() as _,
                    prop,
                    null_mut(),
                    null_mut(),
                );
            }
            xml_free(prop as _);
        }
        cur = (*cur).children;
        xml_parse_xml_catalog_node_list(cur, prefer, parent, null_mut());
    } else {
        xml_catalog_err(
            null_mut(),
            doc as _,
            XmlParserErrors::XmlCatalogNotCatalog,
            c"File %s is not an XML Catalog\n".as_ptr() as _,
            filename,
            null_mut(),
            null_mut(),
        );
        xml_free_doc(doc);
        return null_mut();
    }
    xml_free_doc(doc);
    parent
}

/**
 * xmlFetchXMLCatalogFile:
 * @catal:  an existing but incomplete catalog entry
 *
 * Fetch and parse the subcatalog referenced by an entry
 *
 * Returns 0 in case of success, -1 otherwise
 */
unsafe extern "C" fn xml_fetch_xml_catalog_file(catal: XmlCatalogEntryPtr) -> c_int {
    let mut doc: XmlCatalogEntryPtr;

    if catal.is_null() {
        return -1;
    }
    if (*catal).url.is_null() {
        return -1;
    }

    /*
     * lock the whole catalog for modification
     */
    let mutex = XML_CATALOG_MUTEX.load(Ordering::Acquire);
    xml_rmutex_lock(mutex);
    if !(*catal).children.is_null() {
        /* Okay someone else did it in the meantime */
        xml_rmutex_unlock(mutex);
        return 0;
    }

    let mut catalog_files = XML_CATALOG_XMLFILES.load(Ordering::Acquire);
    if !catalog_files.is_null() {
        doc = xml_hash_lookup(catalog_files, (*catal).url) as XmlCatalogEntryPtr;
        if !doc.is_null() {
            if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                generic_error!(
                    "Found {} in file hash\n",
                    CStr::from_ptr((*catal).url as *const i8).to_string_lossy()
                );
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
            generic_error!(
                "{} not found in file hash\n",
                CStr::from_ptr((*catal).url as *const i8).to_string_lossy()
            );
        }
    }

    /*
     * Fetch and parse. Note that xmlParseXMLCatalogFile does not
     * use the existing catalog, there is no recursion allowed at
     * that level.
     */
    doc = xml_parse_xml_catalog_file((*catal).prefer, (*catal).url);
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
            generic_error!(
                "{} added to file hash\n",
                CStr::from_ptr((*catal).url as *const i8).to_string_lossy()
            );
        }
        xml_hash_add_entry(catalog_files, (*catal).url, doc as _);
    }
    xml_rmutex_unlock(mutex);
    XML_CATALOG_XMLFILES.store(catalog_files, Ordering::Release);
    0
}

/**
 * xmlGetXMLCatalogEntryType:
 * @name:  the name
 *
 * lookup the internal type associated to an XML catalog entry name
 *
 * Returns the type associated with that name
 */
unsafe extern "C" fn xml_get_xml_catalog_entry_type(name: *const XmlChar) -> XmlCatalogEntryType {
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
        typ = XmlCatalogEntryType::XmlCataUri;
    } else if xml_str_equal(name, c"rewriteURI".as_ptr() as _) {
        typ = XmlCatalogEntryType::XmlCataRewriteUri;
    } else if xml_str_equal(name, c"delegateURI".as_ptr() as _) {
        typ = XmlCatalogEntryType::XmlCataDelegateUri;
    } else if xml_str_equal(name, c"nextCatalog".as_ptr() as _) {
        typ = XmlCatalogEntryType::XmlCataNextCatalog;
    } else if xml_str_equal(name, c"catalog".as_ptr() as _) {
        typ = XmlCatalogEntryType::XmlCataCatalog;
    }
    typ
}

/**
 * xmlAddXMLCatalog:
 * @catal:  top of an XML catalog
 * @type:  the type of record to add to the catalog
 * @orig:  the system, public or prefix to match (or null_mut())
 * @replace:  the replacement value for the match
 *
 * Add an entry in the XML catalog, it may overwrite existing but
 * different entries.
 *
 * Returns 0 if successful, -1 otherwise
 */
unsafe extern "C" fn xml_add_xml_catalog(
    catal: XmlCatalogEntryPtr,
    typs: *const XmlChar,
    orig: *const XmlChar,
    replace: *const XmlChar,
) -> c_int {
    let mut cur: XmlCatalogEntryPtr;
    let mut doregister: c_int = 0;

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
            generic_error!(
                "Failed to add unknown element {} to catalog\n",
                CStr::from_ptr(typs as *const i8).to_string_lossy()
            );
        }
        return -1;
    }

    cur = (*catal).children;
    /*
     * Might be a simple "update in place"
     */
    if !cur.is_null() {
        while !cur.is_null() {
            if !orig.is_null() && ((*cur).typ == typ) && xml_str_equal(orig, (*cur).name) {
                if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                    generic_error!(
                        "Updating element {} to catalog\n",
                        CStr::from_ptr(typs as *const i8).to_string_lossy()
                    );
                }
                if !(*cur).value.is_null() {
                    xml_free((*cur).value as _);
                }
                if !(*cur).url.is_null() {
                    xml_free((*cur).url as _);
                }
                (*cur).value = xml_strdup(replace);
                (*cur).url = xml_strdup(replace);
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
        cur = xml_hash_lookup(XML_CATALOG_XMLFILES.load(Ordering::Relaxed), (*catal).url)
            as XmlCatalogEntryPtr;
        if !cur.is_null() {
            (*cur).children = (*catal).children;
        }
    }

    0
}

/**
 * xmlGetSGMLCatalogEntryType:
 * @name:  the entry name
 *
 * Get the Catalog entry type for a given SGML Catalog name
 *
 * Returns Catalog entry type
 */
unsafe extern "C" fn xml_get_sgml_catalog_entry_type(name: *const XmlChar) -> XmlCatalogEntryType {
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
        typ = XmlCatalogEntryType::SgmlCataSgmldecl;
    } else if xml_str_equal(name, c"DOCUMENT".as_ptr() as _) {
        typ = XmlCatalogEntryType::SgmlCataDocument;
    } else if xml_str_equal(name, c"CATALOG".as_ptr() as _) {
        typ = XmlCatalogEntryType::SgmlCataCatalog;
    } else if xml_str_equal(name, c"BASE".as_ptr() as _) {
        typ = XmlCatalogEntryType::SgmlCataBase;
    }
    typ
}

/**
 * xmlACatalogAdd:
 * @catal:  a Catalog
 * @type:  the type of record to add to the catalog
 * @orig:  the system, public or prefix to match
 * @replace:  the replacement value for the match
 *
 * Add an entry in the catalog, it may overwrite existing but
 * different entries.
 *
 * Returns 0 if successful, -1 otherwise
 */
pub unsafe extern "C" fn xml_a_catalog_add(
    catal: XmlCatalogPtr,
    typ: *const XmlChar,
    orig: *const XmlChar,
    replace: *const XmlChar,
) -> c_int {
    let mut res: c_int = -1;

    if catal.is_null() {
        return -1;
    }

    if matches!((*catal).typ, XmlCatalogType::XmlXmlCatalogType) {
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

/**
 * xmlDelXMLCatalog:
 * @catal:  top of an XML catalog
 * @value:  the value to remove from the catalog
 *
 * Remove entries in the XML catalog where the value or the URI
 * is equal to @value
 *
 * Returns the number of entries removed if successful, -1 otherwise
 */
unsafe extern "C" fn xml_del_xml_catalog(
    catal: XmlCatalogEntryPtr,
    value: *const XmlChar,
) -> c_int {
    let mut cur: XmlCatalogEntryPtr;
    let ret: c_int = 0;

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

/**
 * xmlACatalogRemove:
 * @catal:  a Catalog
 * @value:  the value to remove
 *
 * Remove an entry from the catalog
 *
 * Returns the number of entries removed if successful, -1 otherwise
 */
pub unsafe extern "C" fn xml_a_catalog_remove(
    catal: XmlCatalogPtr,
    value: *const XmlChar,
) -> c_int {
    let mut res: c_int;

    if catal.is_null() || value.is_null() {
        return -1;
    }

    if matches!((*catal).typ, XmlCatalogType::XmlXmlCatalogType) {
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

/**
 * xmlCatalogUnWrapURN:
 * @urn:  an "urn:publicid:" to unwrap
 *
 * Expand the URN into the equivalent Public Identifier
 *
 * Returns the new identifier or null_mut(), the string must be deallocated
 *         by the caller.
 */
unsafe extern "C" fn xml_catalog_unwrap_urn(mut urn: *const XmlChar) -> *mut XmlChar {
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

/**
 * xmlCatalogXMLResolve:
 * @catal:  a catalog list
 * @pubID:  the public ID string
 * @sysID:  the system ID string
 *
 * Do a complete resolution lookup of an External Identifier for a
 * list of catalog entries.
 *
 * Implements (or tries to) 7.1. External Identifier Resolution
 * from http://www.oasis-open.org/committees/entity/spec-2001-08-06.html
 *
 * Returns the URI of the resource or null_mut() if not found
 */
unsafe extern "C" fn xml_catalog_xml_resolve(
    catal: XmlCatalogEntryPtr,
    pub_id: *const XmlChar,
    sys_id: *const XmlChar,
) -> *mut XmlChar {
    let mut ret: *mut XmlChar;
    let mut cur: XmlCatalogEntryPtr;
    let mut have_delegate: c_int;
    let mut have_next: c_int = 0;

    /*
     * protection against loops
     */
    if (*catal).depth > MAX_CATAL_DEPTH as i32 {
        xml_catalog_err(
            catal,
            null_mut(),
            XmlParserErrors::XmlCatalogRecursion,
            c"Detected recursion in catalog %s\n".as_ptr() as _,
            (*catal).name,
            null_mut(),
            null_mut(),
        );
        return null_mut();
    }
    (*catal).depth += 1;

    /*
     * First tries steps 2/ 3/ 4/ if a system ID is provided.
     */
    if !sys_id.is_null() {
        let mut rewrite: XmlCatalogEntryPtr = null_mut();
        let mut lenrewrite: c_int = 0;
        let mut len: c_int;
        cur = catal;
        have_delegate = 0;
        while !cur.is_null() {
            match (*cur).typ {
                XmlCatalogEntryType::XmlCataSystem => {
                    if xml_str_equal(sys_id, (*cur).name) {
                        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                            generic_error!(
                                "Found system match {}, using {}\n",
                                CStr::from_ptr((*cur).name as *const i8).to_string_lossy(),
                                CStr::from_ptr((*cur).url as *const i8).to_string_lossy()
                            );
                        }
                        (*catal).depth -= 1;
                        return xml_strdup((*cur).url);
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
            ret = xml_strdup((*rewrite).url);
            if !ret.is_null() {
                ret = xml_strcat(ret, sys_id.add(lenrewrite as usize));
            }
            (*catal).depth -= 1;
            return ret;
        }
        if have_delegate != 0 {
            let mut delegates: [*const XmlChar; MAX_DELEGATE] = [null(); MAX_DELEGATE];
            let mut nb_list: usize = 0;

            /*
             * Assume the entries have been sorted by decreasing substring
             * matches when the list was produced.
             */
            cur = catal;
            'b: while !cur.is_null() {
                if matches!((*cur).typ, XmlCatalogEntryType::XmlCataDelegateSystem)
                    && xml_strncmp(sys_id, (*cur).name, xml_strlen((*cur).name)) == 0
                {
                    for i in 0..nb_list {
                        if xml_str_equal((*cur).url, delegates[i]) {
                            cur = (*cur).next;
                            continue 'b;
                        }
                    }
                    if nb_list < MAX_DELEGATE {
                        delegates[nb_list] = (*cur).url;
                        nb_list += 1;
                    }

                    if (*cur).children.is_null() {
                        xml_fetch_xml_catalog_file(cur);
                    }
                    if !(*cur).children.is_null() {
                        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                            generic_error!(
                                "Trying system delegate {}\n",
                                CStr::from_ptr((*cur).url as *const i8).to_string_lossy()
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
            /*
             * Apply the cut algorithm explained in 4/
             */
            (*catal).depth -= 1;
            return XML_CATAL_BREAK;
        }
    }
    /*
     * Then tries 5/ 6/ if a public ID is provided
     */
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
                        return xml_strdup((*cur).url);
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
            let mut delegates: [*const XmlChar; MAX_DELEGATE] = [null(); MAX_DELEGATE];
            let mut nb_list: usize = 0;

            /*
             * Assume the entries have been sorted by decreasing substring
             * matches when the list was produced.
             */
            cur = catal;
            'b: while !cur.is_null() {
                if (*cur).typ == XmlCatalogEntryType::XmlCataDelegatePublic
                    && matches!((*cur).prefer, XmlCatalogPrefer::Public)
                    && xml_strncmp(pub_id, (*cur).name, xml_strlen((*cur).name)) == 0
                {
                    for i in 0..nb_list {
                        if xml_str_equal((*cur).url, delegates[i]) {
                            cur = (*cur).next;
                            continue 'b;
                        }
                    }
                    if nb_list < MAX_DELEGATE {
                        delegates[nb_list] = (*cur).url;
                        nb_list += 1;
                    }

                    if (*cur).children.is_null() {
                        xml_fetch_xml_catalog_file(cur);
                    }
                    if !(*cur).children.is_null() {
                        if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                            generic_error!(
                                "Trying public delegate {}\n",
                                CStr::from_ptr((*cur).url as *const i8).to_string_lossy()
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

/**
 * xmlCatalogListXMLResolve:
 * @catal:  a catalog list
 * @pubID:  the public ID string
 * @sysID:  the system ID string
 *
 * Do a complete resolution lookup of an External Identifier for a
 * list of catalogs
 *
 * Implements (or tries to) 7.1. External Identifier Resolution
 * from http://www.oasis-open.org/committees/entity/spec-2001-08-06.html
 *
 * Returns the URI of the resource or null_mut() if not found
 */
unsafe extern "C" fn xml_catalog_list_xml_resolve(
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

/**
 * xmlCatalogGetSGMLPublic:
 * @catal:  an SGML catalog hash
 * @pubID:  the public ID string
 *
 * Try to lookup the catalog local reference associated to a public ID
 *
 * Returns the local resource if found or null_mut() otherwise.
 */
unsafe extern "C" fn xml_catalog_get_sgml_public(
    catal: XmlHashTablePtr,
    mut pub_id: *const XmlChar,
) -> *const XmlChar {
    if catal.is_null() {
        return null_mut();
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
        return null_mut();
    }
    if (*entry).typ == XmlCatalogEntryType::SgmlCataPublic {
        if !normid.is_null() {
            xml_free(normid as _);
        }
        return (*entry).url;
    }
    if !normid.is_null() {
        xml_free(normid as _);
    }
    null_mut()
}

/**
 * xmlCatalogGetSGMLSystem:
 * @catal:  an SGML catalog hash
 * @sysID:  the system ID string
 *
 * Try to lookup the catalog local reference for a system ID
 *
 * Returns the local resource if found or null_mut() otherwise.
 */
unsafe extern "C" fn xml_catalog_get_sgml_system(
    catal: XmlHashTablePtr,
    sys_id: *const XmlChar,
) -> *const XmlChar {
    if catal.is_null() {
        return null_mut();
    }

    let entry: XmlCatalogEntryPtr = xml_hash_lookup(catal, sys_id) as XmlCatalogEntryPtr;
    if entry.is_null() {
        return null_mut();
    }
    if (*entry).typ == XmlCatalogEntryType::SgmlCataSystem {
        return (*entry).url;
    }
    null_mut()
}

/**
 * xmlCatalogSGMLResolve:
 * @catal:  the SGML catalog
 * @pubID:  the public ID string
 * @sysID:  the system ID string
 *
 * Do a complete resolution lookup of an External Identifier
 *
 * Returns the URI of the resource or null_mut() if not found
 */
unsafe extern "C" fn xml_catalog_sgml_resolve(
    catal: XmlCatalogPtr,
    pub_id: *const XmlChar,
    sys_id: *const XmlChar,
) -> *const XmlChar {
    let mut ret: *const XmlChar = null();

    if (*catal).sgml.is_null() {
        return null_mut();
    }

    if !pub_id.is_null() {
        ret = xml_catalog_get_sgml_public((*catal).sgml, pub_id);
    }
    if !ret.is_null() {
        return ret;
    }
    if !sys_id.is_null() {
        ret = xml_catalog_get_sgml_system((*catal).sgml, sys_id);
    }
    if !ret.is_null() {
        return ret;
    }
    null_mut()
}

/**
 * xmlACatalogResolve:
 * @catal:  a Catalog
 * @pubID:  the public ID string
 * @sysID:  the system ID string
 *
 * Do a complete resolution lookup of an External Identifier
 *
 * Returns the URI of the resource or null_mut() if not found, it must be freed
 *      by the caller.
 */
pub unsafe extern "C" fn xml_a_catalog_resolve(
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

    if matches!((*catal).typ, XmlCatalogType::XmlXmlCatalogType) {
        ret = xml_catalog_list_xml_resolve((*catal).xml, pub_id, sys_id);
        if ret == XML_CATAL_BREAK {
            ret = null_mut();
        }
    } else {
        let sgml: *const XmlChar = xml_catalog_sgml_resolve(catal, pub_id, sys_id);
        if !sgml.is_null() {
            ret = xml_strdup(sgml);
        }
    }
    ret
}

/**
 * xmlACatalogResolvePublic:
 * @catal:  a Catalog
 * @pubID:  the public ID string
 *
 * Try to lookup the catalog local reference associated to a public ID in that catalog
 *
 * Returns the local resource if found or null_mut() otherwise, the value returned
 *      must be freed by the caller.
 */
pub unsafe extern "C" fn xml_a_catalog_resolve_public(
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

    if matches!((*catal).typ, XmlCatalogType::XmlXmlCatalogType) {
        ret = xml_catalog_list_xml_resolve((*catal).xml, pub_id, null_mut());
        if ret == XML_CATAL_BREAK {
            ret = null_mut();
        }
    } else {
        let sgml: *const XmlChar = xml_catalog_get_sgml_public((*catal).sgml, pub_id);
        if !sgml.is_null() {
            ret = xml_strdup(sgml);
        }
    }
    ret
}

/**
 * xmlACatalogResolveSystem:
 * @catal:  a Catalog
 * @sysID:  the system ID string
 *
 * Try to lookup the catalog resource for a system ID
 *
 * Returns the resource if found or null_mut() otherwise, the value returned
 *      must be freed by the caller.
 */
pub unsafe extern "C" fn xml_a_catalog_resolve_system(
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

    if matches!((*catal).typ, XmlCatalogType::XmlXmlCatalogType) {
        ret = xml_catalog_list_xml_resolve((*catal).xml, null_mut(), sys_id);
        if ret == XML_CATAL_BREAK {
            ret = null_mut();
        }
    } else {
        let sgml: *const XmlChar = xml_catalog_get_sgml_system((*catal).sgml, sys_id);
        if !sgml.is_null() {
            ret = xml_strdup(sgml);
        }
    }
    ret
}

/**
 * xmlCatalogXMLResolveURI:
 * @catal:  a catalog list
 * @URI:  the URI
 * @sysID:  the system ID string
 *
 * Do a complete resolution lookup of an External Identifier for a
 * list of catalog entries.
 *
 * Implements (or tries to) 7.2.2. URI Resolution
 * from http://www.oasis-open.org/committees/entity/spec-2001-08-06.html
 *
 * Returns the URI of the resource or null_mut() if not found
 */
unsafe extern "C" fn xml_catalog_xml_resolve_uri(
    catal: XmlCatalogEntryPtr,
    uri: *const XmlChar,
) -> *mut XmlChar {
    let mut ret: *mut XmlChar;
    let mut cur: XmlCatalogEntryPtr;
    let mut have_delegate: c_int;
    let mut have_next: c_int = 0;
    let mut rewrite: XmlCatalogEntryPtr = null_mut();
    let mut lenrewrite: c_int = 0;
    let mut len: c_int;

    if catal.is_null() {
        return null_mut();
    }

    if uri.is_null() {
        return null_mut();
    }

    if (*catal).depth > MAX_CATAL_DEPTH as i32 {
        xml_catalog_err(
            catal,
            null_mut(),
            XmlParserErrors::XmlCatalogRecursion,
            c"Detected recursion in catalog %s\n".as_ptr() as _,
            (*catal).name,
            null_mut(),
            null_mut(),
        );
        return null_mut();
    }

    /*
     * First tries steps 2/ 3/ 4/ if a system ID is provided.
     */
    cur = catal;
    have_delegate = 0;
    while !cur.is_null() {
        match (*cur).typ {
            XmlCatalogEntryType::XmlCataUri => {
                if xml_str_equal(uri, (*cur).name) {
                    if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                        generic_error!(
                            "Found URI match {}\n",
                            CStr::from_ptr((*cur).name as *const i8).to_string_lossy()
                        );
                    }
                    return xml_strdup((*cur).url);
                }
            }
            XmlCatalogEntryType::XmlCataRewriteUri => {
                len = xml_strlen((*cur).name);
                if len > lenrewrite && xml_strncmp(uri, (*cur).name, len) == 0 {
                    lenrewrite = len;
                    rewrite = cur;
                }
            }
            XmlCatalogEntryType::XmlCataDelegateUri => {
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
        ret = xml_strdup((*rewrite).url);
        if !ret.is_null() {
            ret = xml_strcat(ret, uri.add(lenrewrite as usize));
        }
        return ret;
    }
    if have_delegate != 0 {
        let mut delegates: [*const XmlChar; MAX_DELEGATE] = [null(); MAX_DELEGATE];
        let mut nb_list: usize = 0;

        /*
         * Assume the entries have been sorted by decreasing substring
         * matches when the list was produced.
         */
        cur = catal;
        'b: while !cur.is_null() {
            if matches!(
                (*cur).typ,
                XmlCatalogEntryType::XmlCataDelegateSystem
                    | XmlCatalogEntryType::XmlCataDelegateUri
            ) && xml_strncmp(uri, (*cur).name, xml_strlen((*cur).name)) == 0
            {
                for i in 0..nb_list {
                    if xml_str_equal((*cur).url, delegates[i]) {
                        cur = (*cur).next;
                        continue 'b;
                    }
                }
                if nb_list < MAX_DELEGATE {
                    delegates[nb_list] = (*cur).url;
                    nb_list += 1;
                }

                if (*cur).children.is_null() {
                    xml_fetch_xml_catalog_file(cur);
                }
                if !(*cur).children.is_null() {
                    if XML_DEBUG_CATALOGS.load(Ordering::Relaxed) != 0 {
                        generic_error!(
                            "Trying URI delegate {}\n",
                            CStr::from_ptr((*cur).url as *const i8).to_string_lossy()
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
        /*
         * Apply the cut algorithm explained in 4/
         */
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

/**
 * xmlCatalogListXMLResolveURI:
 * @catal:  a catalog list
 * @URI:  the URI
 *
 * Do a complete resolution lookup of an URI for a list of catalogs
 *
 * Implements (or tries to) 7.2. URI Resolution
 * from http://www.oasis-open.org/committees/entity/spec-2001-08-06.html
 *
 * Returns the URI of the resource or null_mut() if not found
 */
unsafe extern "C" fn xml_catalog_list_xml_resolve_uri(
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

/**
 * xmlACatalogResolveURI:
 * @catal:  a Catalog
 * @URI:  the URI
 *
 * Do a complete resolution lookup of an URI
 *
 * Returns the URI of the resource or null_mut() if not found, it must be freed
 *      by the caller.
 */
pub unsafe extern "C" fn xml_a_catalog_resolve_uri(
    catal: XmlCatalogPtr,
    uri: *const XmlChar,
) -> *mut XmlChar {
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

    if matches!((*catal).typ, XmlCatalogType::XmlXmlCatalogType) {
        ret = xml_catalog_list_xml_resolve_uri((*catal).xml, uri);
        if ret == XML_CATAL_BREAK {
            ret = null_mut();
        }
    } else {
        let sgml: *const XmlChar = xml_catalog_sgml_resolve(catal, null_mut(), uri);
        if !sgml.is_null() {
            ret = xml_strdup(sgml);
        }
    }
    ret
}

/**
 * xmlCatalogDumpEntry:
 * @entry:  the catalog entry
 * @out:  the file.
 *
 * Serialize an SGML Catalog entry
 */
#[cfg(feature = "output")]
extern "C" fn xml_catalog_dump_entry(
    payload: *mut c_void,
    data: *mut c_void,
    _name: *const XmlChar,
) {
    let entry: XmlCatalogEntryPtr = payload as XmlCatalogEntryPtr;
    let out: *mut FILE = data as _;
    if entry.is_null() || out.is_null() {
        return;
    }
    unsafe {
        match (*entry).typ {
            XmlCatalogEntryType::SgmlCataEntity => {
                fprintf(out, c"ENTITY ".as_ptr() as _);
            }
            XmlCatalogEntryType::SgmlCataPentity => {
                fprintf(out, c"ENTITY %%".as_ptr() as _);
            }
            XmlCatalogEntryType::SgmlCataDoctype => {
                fprintf(out, c"DOCTYPE ".as_ptr() as _);
            }
            XmlCatalogEntryType::SgmlCataLinktype => {
                fprintf(out, c"LINKTYPE ".as_ptr() as _);
            }
            XmlCatalogEntryType::SgmlCataNotation => {
                fprintf(out, c"NOTATION ".as_ptr() as _);
            }
            XmlCatalogEntryType::SgmlCataPublic => {
                fprintf(out, c"PUBLIC ".as_ptr() as _);
            }
            XmlCatalogEntryType::SgmlCataSystem => {
                fprintf(out, c"SYSTEM ".as_ptr() as _);
            }
            XmlCatalogEntryType::SgmlCataDelegate => {
                fprintf(out, c"DELEGATE ".as_ptr() as _);
            }
            XmlCatalogEntryType::SgmlCataBase => {
                fprintf(out, c"BASE ".as_ptr() as _);
            }
            XmlCatalogEntryType::SgmlCataCatalog => {
                fprintf(out, c"CATALOG ".as_ptr() as _);
            }
            XmlCatalogEntryType::SgmlCataDocument => {
                fprintf(out, c"DOCUMENT ".as_ptr() as _);
            }
            XmlCatalogEntryType::SgmlCataSgmldecl => {
                fprintf(out, c"SGMLDECL ".as_ptr() as _);
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
                fprintf(out, c"%s".as_ptr() as _, (*entry).name);
            }
            XmlCatalogEntryType::SgmlCataPublic
            | XmlCatalogEntryType::SgmlCataSystem
            | XmlCatalogEntryType::SgmlCataSgmldecl
            | XmlCatalogEntryType::SgmlCataDocument
            | XmlCatalogEntryType::SgmlCataCatalog
            | XmlCatalogEntryType::SgmlCataBase
            | XmlCatalogEntryType::SgmlCataDelegate => {
                fprintf(out, c"\"%s\"".as_ptr() as _, (*entry).name);
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
                fprintf(out, c" \"%s\"".as_ptr() as _, (*entry).value);
            }
            _ => {}
        }
        fprintf(out, c"\n".as_ptr() as _);
    }
}

/**
 * xmlDumpXMLCatalogNode:
 * @catal:  top catalog entry
 * @catalog: pointer to the xml tree
 * @doc: the containing document
 * @ns: the current namespace
 * @cgroup: group node for group members
 *
 * Serializes a Catalog entry, called by xmlDumpXMLCatalog and recursively
 * for group entries
 */
#[cfg(feature = "output")]
unsafe extern "C" fn xml_dump_xml_catalog_node(
    catal: XmlCatalogEntryPtr,
    catalog: XmlNodePtr,
    doc: XmlDocPtr,
    ns: XmlNsPtr,
    cgroup: XmlCatalogEntryPtr,
) {
    let mut node: XmlNodePtr;
    let mut cur: XmlCatalogEntryPtr;
    /*
     * add all the catalog entries
     */
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
                    xml_set_prop(node, c"catalog".as_ptr() as _, (*cur).value);
                    xml_add_child(catalog, node);
                }
                XmlCatalogEntryType::XmlCataNone => {}
                XmlCatalogEntryType::XmlCataGroup => {
                    node = xml_new_doc_node(doc, ns, c"group".as_ptr() as _, null_mut());
                    xml_set_prop(node, c"id".as_ptr() as _, (*cur).name);
                    if !(*cur).value.is_null() {
                        let xns: XmlNsPtr =
                            xml_search_ns_by_href(doc, node, XML_XML_NAMESPACE.as_ptr() as _);
                        if !xns.is_null() {
                            xml_set_ns_prop(node, xns, c"base".as_ptr() as _, (*cur).value);
                        }
                    }
                    match (*cur).prefer {
                        XmlCatalogPrefer::None => {}
                        XmlCatalogPrefer::Public => {
                            xml_set_prop(node, c"prefer".as_ptr() as _, c"public".as_ptr() as _);
                        }
                        XmlCatalogPrefer::System => {
                            xml_set_prop(node, c"prefer".as_ptr() as _, c"system".as_ptr() as _);
                        }
                    }
                    xml_dump_xml_catalog_node((*cur).next, node, doc, ns, cur);
                    xml_add_child(catalog, node);
                }
                XmlCatalogEntryType::XmlCataPublic => {
                    node = xml_new_doc_node(doc, ns, c"public".as_ptr() as _, null_mut());
                    xml_set_prop(node, c"publicId".as_ptr() as _, (*cur).name);
                    xml_set_prop(node, c"uri".as_ptr() as _, (*cur).value);
                    xml_add_child(catalog, node);
                }
                XmlCatalogEntryType::XmlCataSystem => {
                    node = xml_new_doc_node(doc, ns, c"system".as_ptr() as _, null_mut());
                    xml_set_prop(node, c"systemId".as_ptr() as _, (*cur).name);
                    xml_set_prop(node, c"uri".as_ptr() as _, (*cur).value);
                    xml_add_child(catalog, node);
                }
                XmlCatalogEntryType::XmlCataRewriteSystem => {
                    node = xml_new_doc_node(doc, ns, c"rewriteSystem".as_ptr() as _, null_mut());
                    xml_set_prop(node, c"systemIdStartString".as_ptr() as _, (*cur).name);
                    xml_set_prop(node, c"rewritePrefix".as_ptr() as _, (*cur).value);
                    xml_add_child(catalog, node);
                }
                XmlCatalogEntryType::XmlCataDelegatePublic => {
                    node = xml_new_doc_node(doc, ns, c"delegatePublic".as_ptr() as _, null_mut());
                    xml_set_prop(node, c"publicIdStartString".as_ptr() as _, (*cur).name);
                    xml_set_prop(node, c"catalog".as_ptr() as _, (*cur).value);
                    xml_add_child(catalog, node);
                }
                XmlCatalogEntryType::XmlCataDelegateSystem => {
                    node = xml_new_doc_node(doc, ns, c"delegateSystem".as_ptr() as _, null_mut());
                    xml_set_prop(node, c"systemIdStartString".as_ptr() as _, (*cur).name);
                    xml_set_prop(node, c"catalog".as_ptr() as _, (*cur).value);
                    xml_add_child(catalog, node);
                }
                XmlCatalogEntryType::XmlCataUri => {
                    node = xml_new_doc_node(doc, ns, c"uri".as_ptr() as _, null_mut());
                    xml_set_prop(node, c"name".as_ptr() as _, (*cur).name);
                    xml_set_prop(node, c"uri".as_ptr() as _, (*cur).value);
                    xml_add_child(catalog, node);
                }
                XmlCatalogEntryType::XmlCataRewriteUri => {
                    node = xml_new_doc_node(doc, ns, c"rewriteURI".as_ptr() as _, null_mut());
                    xml_set_prop(node, c"uriStartString".as_ptr() as _, (*cur).name);
                    xml_set_prop(node, c"rewritePrefix".as_ptr() as _, (*cur).value);
                    xml_add_child(catalog, node);
                }
                XmlCatalogEntryType::XmlCataDelegateUri => {
                    node = xml_new_doc_node(doc, ns, c"delegateURI".as_ptr() as _, null_mut());
                    xml_set_prop(node, c"uriStartString".as_ptr() as _, (*cur).name);
                    xml_set_prop(node, c"catalog".as_ptr() as _, (*cur).value);
                    xml_add_child(catalog, node);
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
                | XmlCatalogEntryType::SgmlCataSgmldecl => {}
            }
        }
        cur = (*cur).next;
    }
}

#[cfg(feature = "output")]
unsafe extern "C" fn xml_dump_xml_catalog(out: *mut FILE, catal: XmlCatalogEntryPtr) -> c_int {
    /*
     * Rebuild a catalog
     */

    use crate::io::{xml_output_buffer_create_file, XmlOutputBufferPtr};
    let doc: XmlDocPtr = xml_new_doc(null_mut());
    if doc.is_null() {
        return -1;
    }
    let dtd: XmlDtdPtr = xml_new_dtd(
        doc,
        c"catalog".as_ptr() as _,
        c"-//OASIS//DTD Entity Resolution XML Catalog V1.0//EN".as_ptr() as _,
        c"http://www.oasis-open.org/committees/entity/release/1.0/catalog.dtd".as_ptr() as _,
    );

    xml_add_child(doc as _, dtd as _);

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
    xml_add_child(doc as _, catalog);

    xml_dump_xml_catalog_node(catal, catalog, doc, ns, null_mut());

    /*
     * reserialize it
     */
    let buf: XmlOutputBufferPtr = xml_output_buffer_create_file(out, None);
    if buf.is_null() {
        xml_free_doc(doc);
        return -1;
    }
    let ret: c_int = xml_save_format_file_to(buf, doc, null_mut(), 1);

    /*
     * Free it
     */
    xml_free_doc(doc);

    ret
}

/**
 * xmlACatalogDump:
 * @catal:  a Catalog
 * @out:  the file.
 *
 * Dump the given catalog to the given file.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_a_catalog_dump(catal: XmlCatalogPtr, out: *mut FILE) {
    if out.is_null() || catal.is_null() {
        return;
    }

    if matches!((*catal).typ, XmlCatalogType::XmlXmlCatalogType) {
        xml_dump_xml_catalog(out, (*catal).xml);
    } else {
        xml_hash_scan((*catal).sgml, Some(xml_catalog_dump_entry), out as _);
    }
}

/**
 * xmlFreeCatalogEntryList:
 * @ret:  a Catalog entry list
 *
 * Free the memory allocated to a full chained list of Catalog entries
 */
unsafe extern "C" fn xml_free_catalog_entry_list(mut ret: XmlCatalogEntryPtr) {
    let mut next: XmlCatalogEntryPtr;

    while !ret.is_null() {
        next = (*ret).next;
        xml_free_catalog_entry(ret as _, null_mut());
        ret = next;
    }
}

/**
 * xmlFreeCatalog:
 * @catal:  a Catalog
 *
 * Free the memory allocated to a Catalog
 */
pub unsafe extern "C" fn xml_free_catalog(catal: XmlCatalogPtr) {
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

/**
 * xmlCatalogIsEmpty:
 * @catal:  should this create an SGML catalog
 *
 * Check is a catalog is empty
 *
 * Returns 1 if the catalog is empty, 0 if not, amd -1 in case of error.
 */
pub unsafe extern "C" fn xml_catalog_is_empty(catal: XmlCatalogPtr) -> c_int {
    if catal.is_null() {
        return -1;
    }

    if matches!((*catal).typ, XmlCatalogType::XmlXmlCatalogType) {
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
        let res: c_int = xml_hash_size((*catal).sgml);
        if res == 0 {
            return 1;
        }
        if res < 0 {
            return -1;
        }
    }
    0
}

/*
 * Global operations.
 */

/*
 * Whether the catalog support was initialized.
 */
static XML_CATALOG_INITIALIZED: AtomicBool = AtomicBool::new(false);
/*
 * The default catalog in use by the application
 */
static XML_DEFAULT_CATALOG: AtomicPtr<XmlCatalog> = AtomicPtr::new(null_mut());

const XML_XML_DEFAULT_CATALOG: &CStr =
    unsafe { transmute(concatcp!("file://", SYSCONFDIR, "/xml/catalog", "\0")) };
const XML_SGML_DEFAULT_CATALOG: &str = concatcp!("file://", SYSCONFDIR, "/sgml/catalog");

/**
 * xmlInitializeCatalogData:
 *
 * Do the catalog initialization only of global data, doesn't try to load
 * any catalog actually.
 * this function is not thread safe, catalog initialization should
 * preferably be done once at startup
 */
unsafe extern "C" fn xml_initialize_catalog_data() {
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

/**
 * xmlInitializeCatalog:
 *
 * Do the catalog initialization.
 * this function is not thread safe, catalog initialization should
 * preferably be done once at startup
 */
pub unsafe extern "C" fn xml_initialize_catalog() {
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
            XmlCatalogType::XmlXmlCatalogType,
            XML_CATALOG_DEFAULT_PREFER,
        );
        if !catal.is_null() {
            /* the XML_CATALOG_FILES envvar is allowed to contain a
            space-separated list of entries. */
            cur = catalogs;
            nextent = addr_of_mut!((*catal).xml);
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
                            nextent = addr_of_mut!((*(*nextent)).next);
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

/**
 * xmlLoadCatalog:
 * @filename:  a file path
 *
 * Load the catalog and makes its definitions effective for the default
 * external entity loader. It will recurse in SGML CATALOG entries.
 * this function is not thread safe, catalog initialization should
 * preferably be done once at startup
 *
 * Returns 0 in case of success -1 in case of error
 */
pub unsafe extern "C" fn xml_load_catalog(filename: *const c_char) -> c_int {
    let catal: XmlCatalogPtr;

    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog_data();
    }

    let mutex = XML_CATALOG_MUTEX.load(Ordering::Acquire);
    xml_rmutex_lock(mutex);

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

    let ret: c_int = xml_expand_catalog(default_catalog, filename);
    xml_rmutex_unlock(mutex);
    ret
}

#[cfg(target_os = "windows")]
const PATH_SEPARATOR: u8 = b';';
#[cfg(not(target_os = "windows"))]
const PATH_SEPARATOR: u8 = b':';

/**
 * xmlLoadCatalogs:
 * @pathss:  a list of directories separated by a colon or a space.
 *
 * Load the catalogs and makes their definitions effective for the default
 * external entity loader.
 * this function is not thread safe, catalog initialization should
 * preferably be done once at startup
 */
pub unsafe extern "C" fn xml_load_catalogs(pathss: *const c_char) {
    let mut cur: *const c_char;
    let mut paths: *const c_char;
    let mut path: *mut XmlChar;
    #[cfg(target_os = "windows")]
    let i: c_int;
    #[cfg(target_os = "windows")]
    let iLen: c_int;

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
                xml_load_catalog(path as _);
                xml_free(path as _);
            }
        }
        while *cur == PATH_SEPARATOR as i8 {
            cur = cur.add(1);
        }
    }
}

/**
 * xmlFreeCatalogHashEntryList:
 * @payload:  a Catalog entry list
 *
 * Free the memory allocated to list of Catalog entries from the
 * catalog file hash.
 */
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

/**
 * xmlCatalogCleanup:
 *
 * Free up all the memory associated with catalogs
 */
pub unsafe extern "C" fn xml_catalog_cleanup() {
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

/**
 * xmlCatalogDump:
 * @out:  the file.
 *
 * Dump all the global catalog content to the given file.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_catalog_dump(out: *mut FILE) {
    if out.is_null() {
        return;
    }

    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    xml_a_catalog_dump(XML_DEFAULT_CATALOG.load(Ordering::Relaxed), out);
}

/**
 * xmlCatalogResolve:
 * @pubID:  the public ID string
 * @sysID:  the system ID string
 *
 * Do a complete resolution lookup of an External Identifier
 *
 * Returns the URI of the resource or null_mut() if not found, it must be freed
 *      by the caller.
 */
pub unsafe extern "C" fn xml_catalog_resolve(
    pub_id: *const XmlChar,
    sys_id: *const XmlChar,
) -> *mut XmlChar {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    let ret: *mut XmlChar =
        xml_a_catalog_resolve(XML_DEFAULT_CATALOG.load(Ordering::Relaxed), pub_id, sys_id);
    ret
}

/**
 * xmlCatalogResolveSystem:
 * @sysID:  the system ID string
 *
 * Try to lookup the catalog resource for a system ID
 *
 * Returns the resource if found or null_mut() otherwise, the value returned
 *      must be freed by the caller.
 */
pub unsafe extern "C" fn xml_catalog_resolve_system(sys_id: *const XmlChar) -> *mut XmlChar {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    let ret: *mut XmlChar =
        xml_a_catalog_resolve_system(XML_DEFAULT_CATALOG.load(Ordering::Relaxed), sys_id);
    ret
}

/**
 * xmlCatalogResolvePublic:
 * @pubID:  the public ID string
 *
 * Try to lookup the catalog reference associated to a public ID
 *
 * Returns the resource if found or null_mut() otherwise, the value returned
 *      must be freed by the caller.
 */
pub unsafe extern "C" fn xml_catalog_resolve_public(pub_id: *const XmlChar) -> *mut XmlChar {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    let ret: *mut XmlChar =
        xml_a_catalog_resolve_public(XML_DEFAULT_CATALOG.load(Ordering::Relaxed), pub_id);
    ret
}

/**
 * xmlCatalogResolveURI:
 * @URI:  the URI
 *
 * Do a complete resolution lookup of an URI
 *
 * Returns the URI of the resource or null_mut() if not found, it must be freed
 *      by the caller.
 */
pub unsafe extern "C" fn xml_catalog_resolve_uri(uri: *const XmlChar) -> *mut XmlChar {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    let ret: *mut XmlChar =
        xml_a_catalog_resolve_uri(XML_DEFAULT_CATALOG.load(Ordering::Relaxed), uri);
    ret
}

/**
 * xmlCatalogAdd:
 * @type:  the type of record to add to the catalog
 * @orig:  the system, public or prefix to match
 * @replace:  the replacement value for the match
 *
 * Add an entry in the catalog, it may overwrite existing but
 * different entries.
 * If called before any other catalog routine, allows to override the
 * default shared catalog put in place by xmlInitializeCatalog();
 *
 * Returns 0 if successful, -1 otherwise
 */
pub unsafe extern "C" fn xml_catalog_add(
    typ: *const XmlChar,
    orig: *const XmlChar,
    replace: *const XmlChar,
) -> c_int {
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
            XmlCatalogType::XmlXmlCatalogType,
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

    let res: c_int = xml_a_catalog_add(default_catalog, typ, orig, replace);
    XML_DEFAULT_CATALOG.store(default_catalog, Ordering::Release);
    xml_rmutex_unlock(mutex);
    res
}

/**
 * xmlCatalogRemove:
 * @value:  the value to remove
 *
 * Remove an entry from the catalog
 *
 * Returns the number of entries removed if successful, -1 otherwise
 */
pub unsafe extern "C" fn xml_catalog_remove(value: *const XmlChar) -> c_int {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    let mutex = XML_CATALOG_MUTEX.load(Ordering::Acquire);
    xml_rmutex_lock(mutex);
    let res: c_int = xml_a_catalog_remove(XML_DEFAULT_CATALOG.load(Ordering::Relaxed), value);
    xml_rmutex_unlock(mutex);
    res
}

/**
 * xmlParseCatalogFile:
 * @filename:  the filename
 *
 * parse an XML file and build a tree. It's like xmlParseFile()
 * except it bypass all catalog lookups.
 *
 * Returns the resulting document tree or null_mut() in case of error
 */
pub unsafe extern "C" fn xml_parse_catalog_file(filename: *const c_char) -> XmlDocPtr {
    let ret: XmlDocPtr;
    let mut directory: *mut c_char = null_mut();

    let ctxt: XmlParserCtxtPtr = xml_new_parser_ctxt();
    if ctxt.is_null() {
        xml_catalog_err_memory(c"allocating parser context".as_ptr());
        return null_mut();
    }

    if filename.is_null() {
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    }

    let Some(buf) = XmlParserInputBuffer::from_uri(
        CStr::from_ptr(filename).to_string_lossy().as_ref(),
        crate::encoding::XmlCharEncoding::None,
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

    (*input_stream).filename = xml_canonic_path(filename as _) as _;
    // (*input_stream).buf = Some(buf);
    std::ptr::write(
        &raw mut (*input_stream).buf,
        Some(Rc::new(RefCell::new(buf))),
    );
    (*input_stream).reset_base();

    input_push(ctxt, input_stream);
    if (*ctxt).directory.is_null() {
        directory = xml_parser_get_directory(filename);
    }
    if (*ctxt).directory.is_null() && !directory.is_null() {
        (*ctxt).directory = directory;
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

/**
 * xmlCatalogConvert:
 *
 * Convert all the SGML catalog entries as XML ones
 *
 * Returns the number of entries converted if successful, -1 otherwise
 */
pub unsafe extern "C" fn xml_catalog_convert() -> c_int {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    let mutex = XML_CATALOG_MUTEX.load(Ordering::Acquire);
    xml_rmutex_lock(mutex);
    let res: c_int = xml_convert_sgml_catalog(XML_DEFAULT_CATALOG.load(Ordering::Relaxed));
    xml_rmutex_unlock(mutex);
    res
}

/*
 * Strictly minimal interfaces for per-document catalogs used
 * by the parser.
 */
/**
 * xmlCatalogFreeLocal:
 * @catalogs:  a document's list of catalogs
 *
 * Free up the memory associated to the catalog list
 */
pub unsafe extern "C" fn xml_catalog_free_local(catalogs: *mut c_void) {
    if !XML_CATALOG_INITIALIZED.load(Ordering::Relaxed) {
        xml_initialize_catalog();
    }

    let catal: XmlCatalogEntryPtr = catalogs as XmlCatalogEntryPtr;
    if !catal.is_null() {
        xml_free_catalog_entry_list(catal);
    }
}

/**
 * xmlCatalogAddLocal:
 * @catalogs:  a document's list of catalogs
 * @URL:  the URL to a new local catalog
 *
 * Add the new entry to the catalog list
 *
 * Returns the updated list
 */
pub unsafe extern "C" fn xml_catalog_add_local(
    catalogs: *mut c_void,
    url: *const XmlChar,
) -> *mut c_void {
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

/**
 * xmlCatalogLocalResolve:
 * @catalogs:  a document's list of catalogs
 * @pubID:  the public ID string
 * @sysID:  the system ID string
 *
 * Do a complete resolution lookup of an External Identifier using a
 * document's private catalog list
 *
 * Returns the URI of the resource or null_mut() if not found, it must be freed
 *      by the caller.
 */
pub unsafe extern "C" fn xml_catalog_local_resolve(
    catalogs: *mut c_void,
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

/**
 * xmlCatalogLocalResolveURI:
 * @catalogs:  a document's list of catalogs
 * @URI:  the URI
 *
 * Do a complete resolution lookup of an URI using a
 * document's private catalog list
 *
 * Returns the URI of the resource or null_mut() if not found, it must be freed
 *      by the caller.
 */
pub unsafe extern "C" fn xml_catalog_local_resolve_uri(
    catalogs: *mut c_void,
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

/*
 * Preference settings.
 */
/**
 * xmlCatalogSetDebug:
 * @level:  the debug level of catalogs required
 *
 * Used to set the debug level for catalog operation, 0 disable
 * debugging, 1 enable it
 *
 * Returns the previous value of the catalog debugging level
 */
pub unsafe extern "C" fn xml_catalog_set_debug(level: c_int) -> c_int {
    let ret: c_int = XML_DEBUG_CATALOGS.load(Ordering::Acquire);

    if level <= 0 {
        XML_DEBUG_CATALOGS.store(0, Ordering::Release);
    } else {
        XML_DEBUG_CATALOGS.store(level, Ordering::Release);
    }
    ret
}

/**
 * xmlCatalogSetDefaultPrefer:
 * @prefer:  the default preference for delegation
 *
 * Allows to set the preference between public and system for deletion
 * in XML Catalog resolution. C.f. section 4.1.1 of the spec
 * Values accepted are xmlCatalogPrefer::XML_CATA_PREFER_PUBLIC or xmlCatalogPrefer::XML_CATA_PREFER_SYSTEM
 *
 * Returns the previous value of the default preference for delegation
 */
pub unsafe extern "C" fn xml_catalog_set_default_prefer(
    prefer: XmlCatalogPrefer,
) -> XmlCatalogPrefer {
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

/**
 * xmlCatalogSetDefaults:
 * @allow:  what catalogs should be accepted
 *
 * Used to set the user preference w.r.t. to what catalogs should
 * be accepted
 */
pub unsafe extern "C" fn xml_catalog_set_defaults(allow: XmlCatalogAllow) {
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

/**
 * xmlCatalogGetDefaults:
 *
 * Used to get the user preference w.r.t. to what catalogs should
 * be accepted
 *
 * Returns the current xmlCatalogAllow value
 */
pub unsafe extern "C" fn xml_catalog_get_defaults() -> XmlCatalogAllow {
    XML_CATALOG_DEFAULT_ALLOW
}

/* DEPRECATED interfaces */
/**
 * xmlCatalogGetSystem:
 * @sysID:  the system ID string
 *
 * Try to lookup the catalog reference associated to a system ID
 * DEPRECATED, use xmlCatalogResolveSystem()
 *
 * Returns the resource if found or null_mut() otherwise.
 */
pub unsafe extern "C" fn xml_catalog_get_system(sys_id: *const XmlChar) -> *const XmlChar {
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

    /*
     * Check first the XML catalogs
     */
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
        return xml_catalog_get_sgml_system((*default_catalog).sgml, sys_id);
    }
    null_mut()
}

/**
 * xmlCatalogGetPublic:
 * @pubID:  the public ID string
 *
 * Try to lookup the catalog reference associated to a public ID
 * DEPRECATED, use xmlCatalogResolvePublic()
 *
 * Returns the resource if found or null_mut() otherwise.
 */
pub unsafe extern "C" fn xml_catalog_get_public(pub_id: *const XmlChar) -> *const XmlChar {
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

    /*
     * Check first the XML catalogs
     */
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
        return xml_catalog_get_sgml_public((*default_catalog).sgml, pub_id);
    }
    null_mut()
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_acatalog_add() {
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
    }

    #[test]
    fn test_xml_acatalog_dump() {
        #[cfg(all(feature = "catalog", feature = "output"))]
        unsafe {
            let mut leaks = 0;

            for n_catal in 0..GEN_NB_XML_CATALOG_PTR {
                for n_out in 0..GEN_NB_FILE_PTR {
                    let mem_base = xml_mem_blocks();
                    let catal = gen_xml_catalog_ptr(n_catal, 0);
                    let out = gen_file_ptr(n_out, 1);

                    xml_a_catalog_dump(catal, out);
                    des_xml_catalog_ptr(n_catal, catal, 0);
                    des_file_ptr(n_out, out, 1);
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
    }

    #[test]
    fn test_xml_acatalog_remove() {
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
    }

    #[test]
    fn test_xml_acatalog_resolve() {
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
    }

    #[test]
    fn test_xml_acatalog_resolve_public() {
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
    }

    #[test]
    fn test_xml_acatalog_resolve_system() {
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
    }

    #[test]
    fn test_xml_acatalog_resolve_uri() {
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
    }

    #[test]
    fn test_xml_catalog_add() {
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
    }

    #[test]
    fn test_xml_catalog_cleanup() {
        #[cfg(feature = "catalog")]
        unsafe {
            xml_catalog_cleanup();
            reset_last_error();
        }
    }

    #[test]
    fn test_xml_catalog_convert() {
        #[cfg(feature = "catalog")]
        unsafe {
            let ret_val = xml_catalog_convert();
            desret_int(ret_val);
            reset_last_error();
        }
    }

    #[test]
    fn test_xml_catalog_dump() {
        #[cfg(all(feature = "catalog", feature = "output"))]
        unsafe {
            let mut leaks = 0;

            for n_out in 0..GEN_NB_FILE_PTR {
                let mem_base = xml_mem_blocks();
                let out = gen_file_ptr(n_out, 0);

                xml_catalog_dump(out);
                des_file_ptr(n_out, out, 0);
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
    }

    #[test]
    fn test_xml_catalog_get_defaults() {
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
    }

    #[test]
    fn test_xml_catalog_is_empty() {
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
    }

    #[test]
    fn test_xml_catalog_local_resolve() {
        #[cfg(feature = "catalog")]
        unsafe {
            let mut leaks = 0;

            for n_catalogs in 0..GEN_NB_VOID_PTR {
                for n_pub_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_sys_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let catalogs = gen_void_ptr(n_catalogs, 0);
                        let pub_id = gen_const_xml_char_ptr(n_pub_id, 1);
                        let sys_id = gen_const_xml_char_ptr(n_sys_id, 2);

                        let ret_val = xml_catalog_local_resolve(catalogs, pub_id, sys_id);
                        desret_xml_char_ptr(ret_val);
                        des_void_ptr(n_catalogs, catalogs, 0);
                        des_const_xml_char_ptr(n_pub_id, pub_id, 1);
                        des_const_xml_char_ptr(n_sys_id, sys_id, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlCatalogLocalResolve",
                                xml_mem_blocks() - mem_base
                            );
                            eprint!(" {}", n_catalogs);
                            eprint!(" {}", n_pub_id);
                            eprintln!(" {}", n_sys_id);
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlCatalogLocalResolve()"
            );
        }
    }

    #[test]
    fn test_xml_catalog_local_resolve_uri() {
        #[cfg(feature = "catalog")]
        unsafe {
            let mut leaks = 0;

            for n_catalogs in 0..GEN_NB_VOID_PTR {
                for n_uri in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let catalogs = gen_void_ptr(n_catalogs, 0);
                    let uri = gen_const_xml_char_ptr(n_uri, 1);

                    let ret_val = xml_catalog_local_resolve_uri(catalogs, uri);
                    desret_xml_char_ptr(ret_val);
                    des_void_ptr(n_catalogs, catalogs, 0);
                    des_const_xml_char_ptr(n_uri, uri, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlCatalogLocalResolveURI",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_catalogs);
                        eprintln!(" {}", n_uri);
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlCatalogLocalResolveURI()"
            );
        }
    }

    #[test]
    fn test_xml_catalog_remove() {
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
    }

    #[test]
    fn test_xml_catalog_resolve() {
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
    }

    #[test]
    fn test_xml_catalog_resolve_public() {
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
    }

    #[test]
    fn test_xml_catalog_resolve_system() {
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
    }

    #[test]
    fn test_xml_catalog_resolve_uri() {
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
    }

    #[test]
    fn test_xml_catalog_set_default_prefer() {
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
    }

    #[test]
    fn test_xml_catalog_set_defaults() {
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
    }

    #[test]
    fn test_xml_convert_sgmlcatalog() {
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
    }

    #[test]
    fn test_xml_initialize_catalog() {
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
    }

    #[test]
    fn test_xml_load_acatalog() {

        /* missing type support */
    }

    #[test]
    fn test_xml_load_catalog() {
        #[cfg(feature = "catalog")]
        unsafe {
            for n_filename in 0..GEN_NB_FILEPATH {
                let filename = gen_filepath(n_filename, 0);

                let ret_val = xml_load_catalog(filename);
                desret_int(ret_val);
                des_filepath(n_filename, filename, 0);
                reset_last_error();
            }
        }
    }

    #[test]
    fn test_xml_load_catalogs() {
        #[cfg(feature = "catalog")]
        unsafe {
            for n_pathss in 0..GEN_NB_CONST_CHAR_PTR {
                let pathss = gen_const_char_ptr(n_pathss, 0);

                xml_load_catalogs(pathss);
                des_const_char_ptr(n_pathss, pathss, 0);
                reset_last_error();
            }
        }
    }

    #[test]
    fn test_xml_load_sgmlsuper_catalog() {

        /* missing type support */
    }

    #[test]
    fn test_xml_new_catalog() {

        /* missing type support */
    }

    #[test]
    fn test_xml_parse_catalog_file() {
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
    }
}
