//! Provide methods and data structures for XInclude.  
//! This module is based on `libxml/xinclude.h`, `xinclude.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: implementation of XInclude
// Description: API to handle XInclude processing,
// implements the
// World Wide Web Consortium Last Call Working Draft 10 November 2003
// http://www.w3.org/TR/2003/WD-xinclude-20031110
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// xinclude.c : Code to implement XInclude processing
//
// World Wide Web Consortium W3C Last Call Working Draft 10 November 2003
// http://www.w3.org/TR/2003/WD-xinclude-20031110
//
// See Copyright for the status of this software.
//
// daniel@veillard.com

use std::{
    ffi::{CStr, CString},
    mem::{size_of, size_of_val, zeroed},
    os::raw::c_void,
    ptr::{addr_of_mut, null, null_mut},
    sync::atomic::Ordering,
};

use libc::memset;

#[cfg(feature = "libxml_xptr_locs")]
use crate::libxml::xpointer::XmlLocationSetPtr;
use crate::{
    encoding::{get_encoding_handler, XmlCharEncoding},
    error::__xml_raise_error,
    io::xml_parser_get_directory,
    libxml::{
        dict::{xml_dict_free, xml_dict_reference},
        entities::{xml_add_doc_entity, xml_get_doc_entity, XmlEntityPtr, XmlEntityType},
        globals::{xml_free, xml_malloc, xml_realloc},
        parser::{
            xml_ctxt_use_options, xml_free_parser_ctxt, xml_init_parser, xml_load_external_entity,
            xml_new_parser_ctxt, xml_parse_document, XmlParserCtxtPtr, XmlParserInputPtr,
            XmlParserOption, XML_DETECT_IDS,
        },
        parser_internals::{xml_free_input_stream, xml_string_current_char},
        uri::{xml_build_relative_uri, xml_free_uri, xml_parse_uri, xml_save_uri, XmlURIPtr},
        xmlstring::{xml_str_equal, xml_strchr, xml_strcmp, xml_strdup, XmlChar},
        xpointer::{xml_xptr_eval, xml_xptr_new_context},
    },
    tree::{
        xml_create_int_subset, xml_doc_copy_node, xml_free_doc, xml_free_node, xml_free_node_list,
        xml_new_doc_node, xml_new_doc_text, xml_static_copy_node, xml_static_copy_node_list,
        NodeCommon, NodePtr, XmlDocPtr, XmlDtdPtr, XmlElementType, XmlNodePtr, XML_XML_NAMESPACE,
    },
    uri::{build_uri, escape_url, XmlURI},
    xpath::{
        xml_xpath_free_context, xml_xpath_free_object, XmlXPathContextPtr, XmlXPathObjectPtr,
        XmlXPathObjectType,
    },
};

use super::chvalid::xml_is_char;

/// A constant defining the Xinclude namespace: `http://www.w3.org/2003/XInclude`
pub const XINCLUDE_NS: &CStr = c"http://www.w3.org/2003/XInclude";
/// A constant defining the draft Xinclude namespace: `http://www.w3.org/2001/XInclude`
pub const XINCLUDE_OLD_NS: &CStr = c"http://www.w3.org/2001/XInclude";
/// A constant defining "include"
pub const XINCLUDE_NODE: &str = "include";
/// A constant defining "fallback"
pub const XINCLUDE_FALLBACK: &str = "fallback";
/// A constant defining "href"
pub const XINCLUDE_HREF: &CStr = c"href";
/// A constant defining "parse"
pub const XINCLUDE_PARSE: &CStr = c"parse";
/// A constant defining "xml"
pub const XINCLUDE_PARSE_XML: &str = "xml";
/// A constant defining "text"
pub const XINCLUDE_PARSE_TEXT: &str = "text";
/// A constant defining "encoding"
pub const XINCLUDE_PARSE_ENCODING: &CStr = c"encoding";
/// A constant defining "xpointer"
pub const XINCLUDE_PARSE_XPOINTER: &CStr = c"xpointer";

pub type XmlURL = *mut XmlChar;

pub type XmlXincludeRefPtr = *mut XmlXincludeRef;
#[repr(C)]
pub struct XmlXincludeRef {
    uri: *mut XmlChar,      /* the fully resolved resource URL */
    fragment: *mut XmlChar, /* the fragment in the URI */
    elem: XmlNodePtr,       /* the xi:include element */
    inc: XmlNodePtr,        /* the included copy */
    xml: i32,               /* xml or txt */
    fallback: i32,          /* fallback was loaded */
    empty_fb: i32,          /* flag to show fallback empty */
    expanding: i32,         /* flag to detect inclusion loops */
    replace: i32,           /* should the node be replaced? */
}

pub type XmlXincludeDocPtr = *mut XmlXincludeDoc;
#[repr(C)]
pub struct XmlXincludeDoc {
    doc: XmlDocPtr,    /* the parsed document */
    url: *mut XmlChar, /* the URL */
    expanding: i32,    /* flag to detect inclusion loops */
}

pub type XmlXincludeTxtPtr = *mut XmlXincludeTxt;
#[repr(C)]
pub struct XmlXincludeTxt {
    text: *mut XmlChar, /* text string */
    url: *mut XmlChar,  /* the URL */
}

pub type XmlXincludeCtxtPtr = *mut XmlXincludeCtxt;
/// An XInclude context
#[repr(C)]
pub struct XmlXincludeCtxt {
    doc: XmlDocPtr,                  /* the source document */
    inc_nr: i32,                     /* number of includes */
    inc_max: i32,                    /* size of includes tab */
    inc_tab: *mut XmlXincludeRefPtr, /* array of included references */

    txt_nr: i32,                  /* number of unparsed documents */
    txt_max: i32,                 /* size of unparsed documents tab */
    txt_tab: *mut XmlXincludeTxt, /* array of unparsed documents */

    url_nr: i32,                  /* number of documents stacked */
    url_max: i32,                 /* size of document stack */
    url_tab: *mut XmlXincludeDoc, /* document stack */

    nb_errors: i32,     /* the number of errors detected */
    fatal_err: i32,     /* abort processing */
    legacy: i32,        /* using XINCLUDE_OLD_NS */
    parse_flags: i32,   /* the flags used for parsing XML documents */
    base: *mut XmlChar, /* the current xml:base */

    _private: *mut c_void, /* application data */

    // #ifdef FUZZING_BUILD_MODE_UNSAFE_FOR_PRODUCTION
    //     c_ulong    incTotal; /* total number of processed inclusions */
    // #endif
    depth: i32,     /* recursion depth */
    is_stream: i32, /* streaming mode */
}

/// Implement the XInclude substitution on the XML document @doc
///
/// Returns 0 if no substitution were done, -1 if some processing failed
/// or the number of substitutions done.
#[doc(alias = "xmlXIncludeProcess")]
pub unsafe extern "C" fn xml_xinclude_process(doc: XmlDocPtr) -> i32 {
    xml_xinclude_process_flags(doc, 0)
}

/// Implement the XInclude substitution on the XML document @doc
///
/// Returns 0 if no substitution were done, -1 if some processing failed
/// or the number of substitutions done.
#[doc(alias = "xmlXIncludeProcessFlags")]
pub unsafe extern "C" fn xml_xinclude_process_flags(doc: XmlDocPtr, flags: i32) -> i32 {
    xml_xinclude_process_flags_data(doc, flags, null_mut())
}

/// Implement the XInclude substitution on the XML document @doc
///
/// Returns 0 if no substitution were done, -1 if some processing failed
/// or the number of substitutions done.
#[doc(alias = "xmlXIncludeProcessFlagsData")]
pub unsafe extern "C" fn xml_xinclude_process_flags_data(
    doc: XmlDocPtr,
    flags: i32,
    data: *mut c_void,
) -> i32 {
    if doc.is_null() {
        return -1;
    }
    let tree: XmlNodePtr = (*doc).get_root_element();
    if tree.is_null() {
        return -1;
    }
    xml_xinclude_process_tree_flags_data(tree, flags, data)
}

/// Handle an XInclude error
#[doc(alias = "xmlXIncludeErr")]
macro_rules! xml_xinclude_err {
    ($ctxt:expr, $node:expr, $error:expr, $msg:expr) => {
        xml_xinclude_err!(@inner, $ctxt, $node, $error, $msg, None);
    };
    ($ctxt:expr, $node:expr, $error:expr, $msg:expr, $extra:expr) => {
        let msg = format!($msg, $extra);
        xml_xinclude_err!(@inner, $ctxt, $node, $error, &msg, Some($extra.to_owned().into()));
    };
    (@inner, $ctxt:expr, $node:expr, $error:expr, $msg:expr, $extra:expr) => {
        let ctxt = $ctxt as *mut XmlXincludeCtxt;
        if !ctxt.is_null() {
            (*ctxt).nb_errors += 1;
        }
        __xml_raise_error!(
            None,
            None,
            None,
            ctxt as _,
            $node as _,
            XmlErrorDomain::XmlFromXInclude,
            $error,
            XmlErrorLevel::XmlErrError,
            None,
            0,
            $extra,
            None,
            None,
            0,
            0,
            $msg,
        );
    };
}

/// Test if the node is an XInclude node
///
/// Returns 1 true, 0 otherwise
#[doc(alias = "xmlXIncludeTestNode")]
unsafe extern "C" fn xml_xinclude_test_node(ctxt: XmlXincludeCtxtPtr, node: XmlNodePtr) -> i32 {
    if node.is_null() {
        return 0;
    }
    if (*node).element_type() != XmlElementType::XmlElementNode {
        return 0;
    }
    if (*node).ns.is_null() {
        return 0;
    }
    if xml_str_equal((*(*node).ns).href, XINCLUDE_NS.as_ptr() as _)
        || xml_str_equal((*(*node).ns).href, XINCLUDE_OLD_NS.as_ptr() as _)
    {
        if xml_str_equal((*(*node).ns).href, XINCLUDE_OLD_NS.as_ptr() as _) && (*ctxt).legacy == 0 {
            (*ctxt).legacy = 1;
        }
        if (*node).name().as_deref() == Some(XINCLUDE_NODE) {
            let mut child: XmlNodePtr = (*node).children().map_or(null_mut(), |c| c.as_ptr());
            let mut nb_fallback: i32 = 0;

            while !child.is_null() {
                if (*child).element_type() == XmlElementType::XmlElementNode
                    && !(*child).ns.is_null()
                    && (xml_str_equal((*(*child).ns).href, XINCLUDE_NS.as_ptr() as _)
                        || xml_str_equal((*(*child).ns).href, XINCLUDE_OLD_NS.as_ptr() as _))
                {
                    if (*child).name().as_deref() == Some(XINCLUDE_NODE) {
                        xml_xinclude_err!(
                            ctxt,
                            node,
                            XmlParserErrors::XmlXIncludeIncludeInInclude,
                            "{} has an 'include' child\n",
                            XINCLUDE_NODE
                        );
                        return 0;
                    }
                    if (*child).name().as_deref() == Some(XINCLUDE_FALLBACK) {
                        nb_fallback += 1;
                    }
                }
                child = (*child).next.map_or(null_mut(), |n| n.as_ptr());
            }
            if nb_fallback > 1 {
                xml_xinclude_err!(
                    ctxt,
                    node,
                    XmlParserErrors::XmlXIncludeFallbacksInInclude,
                    "{} has multiple fallback children\n",
                    XINCLUDE_NODE
                );
                return 0;
            }
            return 1;
        }
        if (*node).name().as_deref() == Some(XINCLUDE_FALLBACK)
            && ((*node).parent().is_none()
                || (*node).parent().unwrap().element_type() != XmlElementType::XmlElementNode
                || (*node).parent().unwrap().ns.is_null()
                || (!xml_str_equal(
                    (*(*node).parent().unwrap().ns).href,
                    XINCLUDE_NS.as_ptr() as _,
                ) && !xml_str_equal(
                    (*(*node).parent().unwrap().ns).href,
                    XINCLUDE_OLD_NS.as_ptr() as _,
                ))
                || (*node).parent().unwrap().name().as_deref() != Some(XINCLUDE_NODE))
        {
            xml_xinclude_err!(
                ctxt,
                node,
                XmlParserErrors::XmlXIncludeFallbackNotInInclude,
                "{} is not the child of an 'include'\n",
                XINCLUDE_FALLBACK
            );
        }
    }
    0
}

const XINCLUDE_MAX_DEPTH: i32 = 40;

/// Get an XInclude attribute
///
/// Returns the value (to be freed) or NULL if not found
#[doc(alias = "xmlXIncludeGetProp")]
unsafe fn xml_xinclude_get_prop(
    ctxt: XmlXincludeCtxtPtr,
    cur: XmlNodePtr,
    name: &str,
) -> Option<String> {
    if let Some(ret) = (*cur).get_ns_prop(XINCLUDE_NS.to_string_lossy().as_ref(), Some(name)) {
        return Some(ret);
    }
    if (*ctxt).legacy != 0 {
        if let Some(ret) =
            (*cur).get_ns_prop(XINCLUDE_OLD_NS.to_string_lossy().as_ref(), Some(name))
        {
            return Some(ret);
        }
    }
    (*cur).get_prop(name)
}

/// Handle an out of memory condition
#[doc(alias = "xmlXIncludeErrMemory")]
unsafe fn xml_xinclude_err_memory(ctxt: XmlXincludeCtxtPtr, node: XmlNodePtr, extra: Option<&str>) {
    if !ctxt.is_null() {
        (*ctxt).nb_errors += 1;
    }
    if let Some(extra) = extra {
        __xml_raise_error!(
            None,
            None,
            None,
            ctxt as _,
            node as _,
            XmlErrorDomain::XmlFromXInclude,
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
    } else {
        __xml_raise_error!(
            None,
            None,
            None,
            ctxt as _,
            node as _,
            XmlErrorDomain::XmlFromXInclude,
            XmlParserErrors::XmlErrNoMemory,
            XmlErrorLevel::XmlErrError,
            None,
            0,
            None,
            None,
            None,
            0,
            0,
            "Memory allocation failed\n",
        );
    }
}

/// Free an XInclude reference
#[doc(alias = "xmlXIncludeFreeRef")]
unsafe extern "C" fn xml_xinclude_free_ref(refe: XmlXincludeRefPtr) {
    if refe.is_null() {
        return;
    }
    if !(*refe).uri.is_null() {
        xml_free((*refe).uri as _);
    }
    if !(*refe).fragment.is_null() {
        xml_free((*refe).fragment as _);
    }
    xml_free(refe as _);
}

/// Creates a new reference within an XInclude context
///
/// Returns the new set
#[doc(alias = "xmlXIncludeNewRef")]
unsafe extern "C" fn xml_xinclude_new_ref(
    ctxt: XmlXincludeCtxtPtr,
    uri: *const XmlChar,
    elem: XmlNodePtr,
) -> XmlXincludeRefPtr {
    let ret: XmlXincludeRefPtr = xml_malloc(size_of::<XmlXincludeRef>()) as XmlXincludeRefPtr;
    if ret.is_null() {
        xml_xinclude_err_memory(ctxt, elem, Some("growing XInclude context"));
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlXincludeRef>());
    if uri.is_null() {
        (*ret).uri = null_mut();
    } else {
        (*ret).uri = xml_strdup(uri);
    }
    (*ret).fragment = null_mut();
    (*ret).elem = elem;
    (*ret).xml = 0;
    (*ret).inc = null_mut();
    if (*ctxt).inc_max == 0 {
        (*ctxt).inc_max = 4;
        (*ctxt).inc_tab =
            xml_malloc((*ctxt).inc_max as usize * size_of_val(&*(*ctxt).inc_tab.add(0)))
                as *mut XmlXincludeRefPtr;
        if (*ctxt).inc_tab.is_null() {
            xml_xinclude_err_memory(ctxt, elem, Some("growing XInclude context"));
            xml_xinclude_free_ref(ret);
            return null_mut();
        }
    }
    if (*ctxt).inc_nr >= (*ctxt).inc_max {
        let new_size: usize = (*ctxt).inc_max as usize * 2;

        let tmp: *mut XmlXincludeRefPtr = xml_realloc(
            (*ctxt).inc_tab as _,
            new_size * size_of_val(&*(*ctxt).inc_tab.add(0)),
        ) as *mut XmlXincludeRefPtr;
        if tmp.is_null() {
            xml_xinclude_err_memory(ctxt, elem, Some("growing XInclude context"));
            xml_xinclude_free_ref(ret);
            return null_mut();
        }
        (*ctxt).inc_tab = tmp;
        (*ctxt).inc_max *= 2;
    }
    *(*ctxt).inc_tab.add((*ctxt).inc_nr as usize) = ret;
    (*ctxt).inc_nr += 1;
    ret
}

/// Add a new node to process to an XInclude context
#[doc(alias = "xmlXIncludeAddNode")]
unsafe extern "C" fn xml_xinclude_add_node(
    ctxt: XmlXincludeCtxtPtr,
    cur: XmlNodePtr,
) -> XmlXincludeRefPtr {
    let mut xml: i32 = 1;
    let mut local: i32 = 0;

    if ctxt.is_null() {
        return null_mut();
    }
    if cur.is_null() {
        return null_mut();
    }

    // read the attributes
    let href =
        xml_xinclude_get_prop(ctxt, cur, XINCLUDE_HREF.to_str().unwrap()).unwrap_or("".to_owned());
    let parse = xml_xinclude_get_prop(ctxt, cur, XINCLUDE_PARSE.to_str().unwrap());
    if let Some(parse) = parse {
        if parse == XINCLUDE_PARSE_XML {
            xml = 1;
        } else if parse == XINCLUDE_PARSE_TEXT {
            xml = 0;
        } else {
            xml_xinclude_err!(
                ctxt,
                cur,
                XmlParserErrors::XmlXIncludeParseValue,
                "invalid value {} for 'parse'\n",
                parse
            );
            return null_mut();
        }
    }

    // compute the URI
    let mut base = None;
    let mut uri = if let Some(b) = (*cur).get_base((*ctxt).doc) {
        base = Some(b);
        build_uri(&href, base.as_deref().unwrap())
    } else {
        (*(*ctxt).doc)
            .url
            .as_deref()
            .and_then(|base| build_uri(&href, base))
    };
    if uri.is_none() {
        if let Some(base) = base.as_deref() {
            // Some escaping may be needed
            if let (Some(escbase), Some(eschref)) = (escape_url(base), escape_url(&href)) {
                uri = build_uri(&eschref, &escbase);
            }
        }
    }
    let Some(uri) = uri else {
        xml_xinclude_err!(
            ctxt,
            cur,
            XmlParserErrors::XmlXIncludeHrefURI,
            "failed build URL\n"
        );
        return null_mut();
    };
    let mut fragment = xml_xinclude_get_prop(ctxt, cur, XINCLUDE_PARSE_XPOINTER.to_str().unwrap());

    // Check the URL and remove any fragment identifier
    let Some(mut parsed_uri) = XmlURI::parse(&uri) else {
        xml_xinclude_err!(
            ctxt,
            cur,
            XmlParserErrors::XmlXIncludeHrefURI,
            "invalid value URI {}\n",
            uri
        );
        return null_mut();
    };

    if parsed_uri.fragment.is_some() {
        if (*ctxt).legacy != 0 {
            if fragment.is_none() {
                fragment = parsed_uri.fragment.as_deref().map(|f| f.to_owned());
            }
        } else {
            xml_xinclude_err!(
                ctxt,
                cur,
                XmlParserErrors::XmlXIncludeFragmentID,
                "Invalid fragment identifier in URI {} use the xpointer attribute\n",
                uri
            );
            return null_mut();
        }
        parsed_uri.fragment = None;
    }
    let url = parsed_uri.save();

    if (*(*ctxt).doc).url.as_deref() == Some(url.as_str()) {
        local = 1;
    }

    // If local and xml then we need a fragment
    if local == 1
        && xml == 1
        && (fragment.is_none() || fragment.as_deref().map_or(true, |f| f.is_empty()))
    {
        xml_xinclude_err!(
            ctxt,
            cur,
            XmlParserErrors::XmlXIncludeRecursion,
            "detected a local recursion with no xpointer in {}\n",
            url
        );
        return null_mut();
    }
    let url = CString::new(url).unwrap();

    let refe: XmlXincludeRefPtr = xml_xinclude_new_ref(ctxt, url.as_ptr() as *const u8, cur);
    if refe.is_null() {
        return null_mut();
    }
    let fragment = fragment.map(|f| CString::new(f).unwrap());
    (*refe).fragment = fragment
        .as_ref()
        .map_or(null_mut(), |f| xml_strdup(f.as_ptr() as *const u8));
    (*refe).xml = xml;
    refe
}

/// Parse a document for XInclude
#[doc(alias = "xmlXIncludeParseFile")]
unsafe fn xml_xinclude_parse_file(ctxt: XmlXincludeCtxtPtr, mut url: &str) -> XmlDocPtr {
    let ret: XmlDocPtr;

    xml_init_parser();

    let pctxt: XmlParserCtxtPtr = xml_new_parser_ctxt();
    if pctxt.is_null() {
        xml_xinclude_err_memory(ctxt, null_mut(), Some("cannot allocate parser context"));
        return null_mut();
    }

    // pass in the application data to the parser context.
    (*pctxt)._private = (*ctxt)._private;

    // try to ensure that new documents included are actually
    // built with the same dictionary as the including document.
    if !(*ctxt).doc.is_null() && !(*(*ctxt).doc).dict.is_null() {
        if !(*pctxt).dict.is_null() {
            xml_dict_free((*pctxt).dict);
        }
        (*pctxt).dict = (*(*ctxt).doc).dict;
        xml_dict_reference((*pctxt).dict);
    }

    xml_ctxt_use_options(
        pctxt,
        (*ctxt).parse_flags | XmlParserOption::XmlParseDtdload as i32,
    );

    // Don't read from stdin.
    if url == "-" {
        url = "./-";
    }

    let input_stream: XmlParserInputPtr = xml_load_external_entity(Some(url), None, pctxt);
    if input_stream.is_null() {
        xml_free_parser_ctxt(pctxt);
        return null_mut();
    }

    (*pctxt).input_push(input_stream);

    if (*pctxt).directory.is_none() {
        if let Some(dir) = xml_parser_get_directory(url) {
            (*pctxt).directory = Some(dir.to_string_lossy().into_owned());
        }
    }

    (*pctxt).loadsubset |= XML_DETECT_IDS as i32;

    xml_parse_document(pctxt);

    if (*pctxt).well_formed != 0 {
        ret = (*pctxt).my_doc;
    } else {
        ret = null_mut();
        if !(*pctxt).my_doc.is_null() {
            xml_free_doc((*pctxt).my_doc);
        }
        (*pctxt).my_doc = null_mut();
    }
    xml_free_parser_ctxt(pctxt);

    ret
}

pub type XmlXIncludeMergeDataPtr = *mut XmlXIncludeMergeData;
pub struct XmlXIncludeMergeData {
    doc: XmlDocPtr,
    ctxt: XmlXincludeCtxtPtr,
}

/// Implements the merge of one entity
#[doc(alias = "xmlXIncludeMergeOneEntity")]
extern "C" fn xml_xinclude_merge_entity(ent: XmlEntityPtr, vdata: *mut c_void) {
    let data: XmlXIncludeMergeDataPtr = vdata as XmlXIncludeMergeDataPtr;
    let prev: XmlEntityPtr;

    if ent.is_null() || data.is_null() {
        return;
    }
    unsafe {
        let ctxt: XmlXincludeCtxtPtr = (*data).ctxt;
        let doc: XmlDocPtr = (*data).doc;
        if ctxt.is_null() || doc.is_null() {
            return;
        }
        match (*ent).etype {
            XmlEntityType::XmlInternalParameterEntity
            | XmlEntityType::XmlExternalParameterEntity
            | XmlEntityType::XmlInternalPredefinedEntity => return,
            XmlEntityType::XmlInternalGeneralEntity
            | XmlEntityType::XmlExternalGeneralParsedEntity
            | XmlEntityType::XmlExternalGeneralUnparsedEntity => {}
            _ => unreachable!(),
        }
        let external_id = (*ent).external_id.load(Ordering::Relaxed);
        let system_id = (*ent).system_id.load(Ordering::Relaxed);
        let content = (*ent).content.load(Ordering::Relaxed);
        let ret: XmlEntityPtr = xml_add_doc_entity(
            doc,
            &(*ent).name().unwrap(),
            (*ent).etype,
            (!external_id.is_null())
                .then(|| CStr::from_ptr(external_id as *const i8).to_string_lossy())
                .as_deref(),
            (!system_id.is_null())
                .then(|| CStr::from_ptr(system_id as *const i8).to_string_lossy())
                .as_deref(),
            (!content.is_null())
                .then(|| CStr::from_ptr(content as *const i8).to_string_lossy())
                .as_deref(),
        );
        if !ret.is_null() {
            if !(*ent).uri.load(Ordering::Relaxed).is_null() {
                (*ret).uri.store(
                    xml_strdup((*ent).uri.load(Ordering::Relaxed)),
                    Ordering::Relaxed,
                );
            }
        } else {
            prev = xml_get_doc_entity(doc, &(*ent).name().unwrap());
            if !prev.is_null() {
                let error = || {
                    match (*ent).etype {
                        XmlEntityType::XmlInternalParameterEntity
                        | XmlEntityType::XmlExternalParameterEntity
                        | XmlEntityType::XmlInternalPredefinedEntity
                        | XmlEntityType::XmlInternalGeneralEntity
                        | XmlEntityType::XmlExternalGeneralParsedEntity => return,
                        XmlEntityType::XmlExternalGeneralUnparsedEntity => {}
                        _ => unreachable!(),
                    }
                    xml_xinclude_err!(
                        ctxt,
                        ent as XmlNodePtr,
                        XmlParserErrors::XmlXIncludeEntityDefMismatch,
                        "mismatch in redefinition of entity {}\n",
                        (*ent).name().unwrap()
                    );
                };

                if (*ent).etype != (*prev).etype {
                    // goto error;
                    return error();
                }

                if !(*ent).system_id.load(Ordering::Relaxed).is_null()
                    && !(*prev).system_id.load(Ordering::Relaxed).is_null()
                {
                    if !xml_str_equal(
                        (*ent).system_id.load(Ordering::Relaxed),
                        (*prev).system_id.load(Ordering::Relaxed),
                    ) {
                        // goto error;
                        error()
                    }
                } else if !(*ent).external_id.load(Ordering::Relaxed).is_null()
                    && !(*prev).external_id.load(Ordering::Relaxed).is_null()
                {
                    if !xml_str_equal(
                        (*ent).external_id.load(Ordering::Relaxed),
                        (*prev).external_id.load(Ordering::Relaxed),
                    ) {
                        // goto error;
                        return error();
                    }
                } else if !(*ent).content.load(Ordering::Relaxed).is_null()
                    && !(*prev).content.load(Ordering::Relaxed).is_null()
                {
                    if !xml_str_equal(
                        (*ent).content.load(Ordering::Relaxed),
                        (*prev).content.load(Ordering::Relaxed),
                    ) {
                        // goto error;
                        return error();
                    }
                } else {
                    // goto error;
                    return error();
                }
            }
        }
    }
    // error:
    //     match (*ent).etype {
    //         xmlEntityType::XML_INTERNAL_PARAMETER_ENTITY|
    //         xmlEntityType::XML_EXTERNAL_PARAMETER_ENTITY|
    //         xmlEntityType::XML_INTERNAL_PREDEFINED_ENTITY|
    //         xmlEntityType::XML_INTERNAL_GENERAL_ENTITY|
    //         xmlEntityType::XML_EXTERNAL_GENERAL_PARSED_ENTITY =>
    //         return,
    //         xmlEntityType::XML_EXTERNAL_GENERAL_UNPARSED_ENTITY => {}
    //     }
    //     xmlXIncludeErr(ctxt, ent as xmlNodePtr, xmlParserErrors::XML_XINCLUDE_ENTITY_DEF_MISMATCH as i32, c"mismatch in redefinition of entity %s\n".as_ptr() as _, (*ent).name);
}

/// Implements the entity merge
///
/// Returns 0 if merge succeeded, -1 if some processing failed
#[doc(alias = "xmlXIncludeMergeEntities")]
unsafe extern "C" fn xml_xinclude_merge_entities(
    ctxt: XmlXincludeCtxtPtr,
    doc: XmlDocPtr,
    from: XmlDocPtr,
) -> i32 {
    let cur: XmlNodePtr;
    let mut target: XmlDtdPtr;
    let mut source: XmlDtdPtr;

    if ctxt.is_null() {
        return -1;
    }

    if from.is_null() || (*from).int_subset.is_null() {
        return 0;
    }

    target = (*doc).int_subset;
    if target.is_null() {
        cur = if doc.is_null() {
            null_mut()
        } else {
            (*doc).get_root_element()
        };
        if cur.is_null() {
            return -1;
        }
        target = xml_create_int_subset(doc, (*cur).name().as_deref(), None, None);
        if target.is_null() {
            return -1;
        }
    }

    source = (*from).int_subset;
    if !source.is_null() {
        if let Some(entities) = (*source).entities {
            let mut data: XmlXIncludeMergeData = unsafe { zeroed() };
            data.ctxt = ctxt;
            data.doc = doc;

            entities.scan(|payload, _, _, _| {
                xml_xinclude_merge_entity(*payload, &raw mut data as _);
            });
        }
    }
    source = (*from).ext_subset;
    if !source.is_null() {
        if let Some(entities) = (*source).entities {
            let mut data: XmlXIncludeMergeData = unsafe { zeroed() };
            data.ctxt = ctxt;
            data.doc = doc;

            // don't duplicate existing stuff when external subsets are the same
            if (*target).external_id != (*source).external_id
                && (*target).system_id != (*source).system_id
            {
                entities.scan(|payload, _, _, _| {
                    xml_xinclude_merge_entity(*payload, &raw mut data as _);
                });
            }
        }
    }
    0
}

/// The XInclude recursive nature is handled at this point.
#[doc(alias = "xmlXIncludeRecurseDoc")]
unsafe extern "C" fn xml_xinclude_recurse_doc(
    ctxt: XmlXincludeCtxtPtr,
    doc: XmlDocPtr,
    _url: XmlURL,
) {
    let old_doc: XmlDocPtr = (*ctxt).doc;
    let old_inc_max: i32 = (*ctxt).inc_max;
    let old_inc_nr: i32 = (*ctxt).inc_nr;
    let old_inc_tab: *mut XmlXincludeRefPtr = (*ctxt).inc_tab;
    let old_is_stream: i32 = (*ctxt).is_stream;
    (*ctxt).doc = doc;
    (*ctxt).inc_max = 0;
    (*ctxt).inc_nr = 0;
    (*ctxt).inc_tab = null_mut();
    (*ctxt).is_stream = 0;

    xml_xinclude_do_process(
        ctxt,
        if doc.is_null() {
            null_mut()
        } else {
            (*doc).get_root_element()
        },
    );

    if !(*ctxt).inc_tab.is_null() {
        for i in 0..(*ctxt).inc_nr {
            xml_xinclude_free_ref(*(*ctxt).inc_tab.add(i as usize));
        }
        xml_free((*ctxt).inc_tab as _);
    }

    (*ctxt).doc = old_doc;
    (*ctxt).inc_max = old_inc_max;
    (*ctxt).inc_nr = old_inc_nr;
    (*ctxt).inc_tab = old_inc_tab;
    (*ctxt).is_stream = old_is_stream;
}

/// Make a copy of the node while expanding nested XIncludes.
///
/// Returns a node list, not a single node.
#[doc(alias = "xmlXIncludeCopyNode")]
unsafe extern "C" fn xml_xinclude_copy_node(
    ctxt: XmlXincludeCtxtPtr,
    elem: XmlNodePtr,
    copy_children: i32,
) -> XmlNodePtr {
    let mut result: XmlNodePtr = null_mut();
    let mut insert_parent: XmlNodePtr = null_mut();
    let mut insert_last: XmlNodePtr = null_mut();
    let mut cur: XmlNodePtr;

    if copy_children != 0 {
        cur = (*elem).children().map_or(null_mut(), |c| c.as_ptr());
        if cur.is_null() {
            return null_mut();
        }
    } else {
        cur = elem;
    }

    loop {
        let mut copy: XmlNodePtr = null_mut();
        let mut recurse: i32 = 0;

        if matches!(
            (*cur).element_type(),
            XmlElementType::XmlDocumentNode | XmlElementType::XmlDTDNode
        ) {
        } else if (*cur).element_type() == XmlElementType::XmlElementNode
            && !(*cur).ns.is_null()
            && (*cur).name().as_deref() == Some(XINCLUDE_NODE)
            && (xml_str_equal((*(*cur).ns).href, XINCLUDE_NS.as_ptr() as _)
                || xml_str_equal((*(*cur).ns).href, XINCLUDE_OLD_NS.as_ptr() as _))
        {
            let refe: XmlXincludeRefPtr = xml_xinclude_expand_node(ctxt, cur);

            if refe.is_null() {
                // goto error;
                xml_free_node_list(result);
                return null_mut();
            }
            /*
             * TODO: Insert xmlElementType::XML_XINCLUDE_START and xmlElementType::XML_XINCLUDE_END nodes
             */
            if !(*refe).inc.is_null() {
                copy = xml_static_copy_node_list((*refe).inc, (*ctxt).doc, insert_parent);
                if copy.is_null() {
                    // goto error;
                    xml_free_node_list(result);
                    return null_mut();
                }
            }
        } else {
            copy = xml_static_copy_node(cur, (*ctxt).doc, insert_parent, 2);
            if copy.is_null() {
                // goto error;
                xml_free_node_list(result);
                return null_mut();
            }

            recurse = ((*cur).element_type() != XmlElementType::XmlEntityRefNode
                && (*cur).children().is_some()) as i32;
        }

        if !copy.is_null() {
            if result.is_null() {
                result = copy;
            }
            if !insert_last.is_null() {
                (*insert_last).next = NodePtr::from_ptr(copy);
                (*copy).prev = NodePtr::from_ptr(insert_last);
            } else if !insert_parent.is_null() {
                (*insert_parent).set_children(NodePtr::from_ptr(copy));
            }
            insert_last = copy;
            while let Some(next) = (*insert_last).next {
                insert_last = next.as_ptr();
            }
        }

        if recurse != 0 {
            cur = (*cur).children().map_or(null_mut(), |c| c.as_ptr());
            insert_parent = insert_last;
            insert_last = null_mut();
            continue;
        }

        if cur == elem {
            return result;
        }

        while (*cur).next.is_none() {
            if !insert_parent.is_null() {
                (*insert_parent).set_last(NodePtr::from_ptr(insert_last));
            }
            cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
            if cur == elem {
                return result;
            }
            insert_last = insert_parent;
            insert_parent = (*insert_parent).parent().map_or(null_mut(), |p| p.as_ptr());
        }

        cur = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
    }

    // error:
    // xmlFreeNodeList(result);
    // return null_mut();
}

/// Returns the @n'th element child of @cur or NULL
#[doc(alias = "xmlXIncludeGetNthChild")]
#[cfg(feature = "libxml_xptr_locs")]
unsafe fn xml_xinclude_get_nth_child(cur: XmlNodePtr, no: i32) -> XmlNodePtr {
    let mut i: i32;
    if cur.is_null() || (*cur).element_type() == XmlElementType::XmlNamespaceDecl {
        return null_mut();
    }
    let mut cur = (*cur).children();
    i = 0;
    while i <= no {
        let Some(now) = cur else {
            return null_mut();
        };
        if matches!(
            now.element_type(),
            XmlElementType::XmlElementNode
                | XmlElementType::XmlDocumentNode
                | XmlElementType::XmlHTMLDocumentNode
        ) {
            i += 1;
            if i == no {
                break;
            }
        }

        cur = now.next();
    }
    cur.map_or(null_mut(), |c| c.as_ptr())
}

/// Build a node list tree copy of the XPointer result.
///
/// Returns an xmlNodePtr list or NULL.
/// The caller has to free the node tree.
#[doc(alias = "xmlXIncludeCopyRange")]
#[cfg(feature = "libxml_xptr_locs")]
unsafe fn xml_xinclude_copy_range(
    ctxt: XmlXincludeCtxtPtr,
    range: XmlXPathObjectPtr,
) -> XmlNodePtr {
    use crate::{
        libxml::xpointer::xml_xptr_advance_node,
        tree::{xml_new_doc_text, xml_new_doc_text_len},
    };

    /* pointers to generated nodes */
    let mut list: XmlNodePtr = null_mut();
    let mut last: XmlNodePtr = null_mut();
    let mut list_parent: XmlNodePtr = null_mut();
    let mut tmp: XmlNodePtr;
    let mut tmp2: XmlNodePtr;
    /* pointers to traversal nodes */
    let mut cur: XmlNodePtr;
    let mut end: XmlNodePtr;
    let mut index1: i32;
    let mut index2: i32;
    let mut level: i32 = 0;
    let mut last_level: i32 = 0;
    let mut end_level: i32 = 0;
    let mut end_flag: i32 = 0;

    if ctxt.is_null() || range.is_null() {
        return null_mut();
    }
    if (*range).typ != XmlXPathObjectType::XPathRange {
        return null_mut();
    }
    let start: XmlNodePtr = (*range).user as XmlNodePtr;

    if start.is_null() || (*start).typ == XmlElementType::XmlNamespaceDecl {
        return null_mut();
    }
    end = (*range).user2 as _;
    if end.is_null() {
        return xml_doc_copy_node(start, (*ctxt).doc, 1);
    }
    if (*end).typ == XmlElementType::XmlNamespaceDecl {
        return null_mut();
    }

    cur = start;
    index1 = (*range).index;
    index2 = (*range).index2;
    // level is depth of the current node under consideration
    // list is the pointer to the root of the output tree
    // listParent is a pointer to the parent of output tree (within
    // the included file) in case we need to add another level
    // last is a pointer to the last node added to the output tree
    // lastLevel is the depth of last (relative to the root)
    while !cur.is_null() {
        // Check if our output tree needs a parent
        if level < 0 {
            while level < 0 {
                // copy must include namespaces and properties
                tmp2 = xml_doc_copy_node(list_parent, (*ctxt).doc, 2);
                (*tmp2).add_child(list);
                list = tmp2;
                list_parent = (*list_parent).parent().map_or(null_mut(), |n| n.as_ptr());
                level += 1;
            }
            last = list;
            last_level = 0;
        }
        // Check whether we need to change our insertion point
        while level < last_level {
            last = (*last).parent().map_or(null_mut(), |p| p.as_ptr());
            last_level -= 1;
        }
        if cur == end {
            // Are we at the end of the range?
            if (*cur).element_type() == XmlElementType::XmlTextNode {
                let mut content: *const XmlChar = (*cur).content;
                let mut len: i32;

                if content.is_null() {
                    tmp = xml_new_doc_text_len((*ctxt).doc, null_mut(), 0);
                } else {
                    len = index2;
                    if cur == start && index1 > 1 {
                        content = content.add(index1 as usize - 1);
                        len -= index1 - 1;
                    } else {
                        len = index2;
                    }
                    tmp = xml_new_doc_text_len((*ctxt).doc, content, len);
                }
                // single sub text node selection
                if list.is_null() {
                    return tmp;
                }
                // prune and return full set
                if level == last_level {
                    (*last).add_next_sibling(tmp);
                } else {
                    (*last).add_child(tmp);
                }
                return list;
            } else {
                // ending node not a text node
                end_level = level; /* remember the level of the end node */
                end_flag = 1;
                // last node - need to take care of properties + namespaces
                tmp = xml_doc_copy_node(cur, (*ctxt).doc, 2);
                if list.is_null() {
                    list = tmp;
                    list_parent = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
                    last = tmp;
                } else if level == last_level {
                    last = (*last).add_next_sibling(tmp);
                } else {
                    last = (*last).add_child(tmp);
                    last_level = level;
                }

                if index2 > 1 {
                    end = xml_xinclude_get_nth_child(cur, index2 - 1);
                    index2 = 0;
                }
                if cur == start && index1 > 1 {
                    cur = xml_xinclude_get_nth_child(cur, index1 - 1);
                    index1 = 0;
                } else {
                    cur = (*cur).children().map_or(null_mut(), |p| p.as_ptr());
                }
                // increment level to show change
                level += 1;
                // Now gather the remaining nodes from cur to end
                continue; /* while */
            }
        } else if cur == start {
            // Not at the end, are we at start?
            if matches!(
                (*cur).element_type(),
                XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
            ) {
                let mut content: *const XmlChar = (*cur).content;

                if content.is_null() {
                    tmp = xml_new_doc_text_len((*ctxt).doc, null_mut(), 0);
                } else {
                    if index1 > 1 {
                        content = content.add(index1 as usize - 1);
                        index1 = 0;
                    }
                    tmp = xml_new_doc_text((*ctxt).doc, content);
                }
                last = tmp;
                list = tmp;
                list_parent = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
            } else {
                // Not text node

                // start of the range - need to take care of
                // properties and namespaces
                tmp = xml_doc_copy_node(cur, (*ctxt).doc, 2);
                list = tmp;
                last = tmp;
                list_parent = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
                if index1 > 1 {
                    // Do we need to position?
                    cur = xml_xinclude_get_nth_child(cur, index1 - 1);
                    level = 1;
                    last_level = 1;
                    index1 = 0;
                    // Now gather the remaining nodes from cur to end
                    continue; /* while */
                }
            }
        } else {
            tmp = null_mut();
            match (*cur).typ {
                XmlElementType::XmlDTDNode
                | XmlElementType::XmlElementDecl
                | XmlElementType::XmlAttributeDecl
                | XmlElementType::XmlEntityNode => { /* Do not copy DTD information */ }
                XmlElementType::XmlEntityDecl => { /* handle crossing entities -> stack needed */ }
                XmlElementType::XmlXIncludeStart | XmlElementType::XmlXIncludeEnd => {
                    // don't consider it part of the tree content
                }
                XmlElementType::XmlAttributeNode => { /* Humm, should not happen ! */ }
                _ => {
                    // Middle of the range - need to take care of
                    // properties and namespaces
                    tmp = xml_doc_copy_node(cur, (*ctxt).doc, 2);
                }
            }
            if !tmp.is_null() {
                if level == last_level {
                    last = (*last).add_next_sibling(tmp);
                } else {
                    last = (*last).add_child(tmp);
                    last_level = level;
                }
            }
        }
        // Skip to next node in document order
        cur = xml_xptr_advance_node(cur, addr_of_mut!(level));
        if end_flag != 0 && level >= end_level {
            break;
        }
    }
    list
}

/// Build a node list tree copy of the XPointer result.
/// This will drop Attributes and Namespace declarations.
///
/// Returns an xmlNodePtr list or NULL.
/// The caller has to free the node tree.
#[doc(alias = "xmlXIncludeCopyXPointer")]
unsafe extern "C" fn xml_xinclude_copy_xpointer(
    ctxt: XmlXincludeCtxtPtr,
    obj: XmlXPathObjectPtr,
) -> XmlNodePtr {
    let mut list: XmlNodePtr = null_mut();
    let mut last: XmlNodePtr = null_mut();
    let mut copy: XmlNodePtr;

    if ctxt.is_null() || obj.is_null() {
        return null_mut();
    }
    match (*obj).typ {
        XmlXPathObjectType::XPathNodeset => {
            let Some(set) = (*obj).nodesetval.as_deref() else {
                return null_mut();
            };
            for &now in &set.node_tab {
                if now.is_null() {
                    continue;
                }
                let node = match (*now).element_type() {
                    XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                        let node = (*now)
                            .as_document_node()
                            .unwrap()
                            .as_ref()
                            .get_root_element();
                        if node.is_null() {
                            xml_xinclude_err!(
                                ctxt,
                                now,
                                XmlParserErrors::XmlErrInternalError,
                                "document without root\n"
                            );
                            continue;
                        }
                        node
                    }
                    XmlElementType::XmlTextNode
                    | XmlElementType::XmlCDATASectionNode
                    | XmlElementType::XmlElementNode
                    | XmlElementType::XmlPINode
                    | XmlElementType::XmlCommentNode => now,
                    _ => {
                        xml_xinclude_err!(
                            ctxt,
                            now,
                            XmlParserErrors::XmlXIncludeXPtrResult,
                            "invalid node type in XPtr result\n"
                        );
                        continue; /* for */
                    }
                };
                // OPTIMIZE TODO: External documents should already be
                // expanded, so xmlDocCopyNode should work as well.
                // xmlXIncludeCopyNode is only required for the initial document.
                copy = xml_xinclude_copy_node(ctxt, node, 0);
                if copy.is_null() {
                    xml_free_node_list(list);
                    return null_mut();
                }
                if last.is_null() {
                    list = copy;
                } else {
                    while let Some(next) = (*last).next {
                        last = next.as_ptr();
                    }
                    (*copy).prev = NodePtr::from_ptr(last);
                    (*last).next = NodePtr::from_ptr(copy);
                }
                last = copy;
            }
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XPathLocationset => {
            let set: XmlLocationSetPtr = (*obj).user as XmlLocationSetPtr;
            if set.is_null() {
                return null_mut();
            }
            for i in 0..(*set).loc_nr {
                if last.is_null() {
                    list = xml_xinclude_copy_xpointer(ctxt, *(*set).loc_tab.add(i as usize));
                    last = list;
                } else {
                    (*last).add_next_sibling(xml_xinclude_copy_xpointer(
                        ctxt,
                        *(*set).loc_tab.add(i as usize),
                    ));
                }
                if !last.is_null() {
                    while let Some(next) = (*last).next() {
                        last = next.as_ptr();
                    }
                }
            }
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XPathRange => return xml_xinclude_copy_range(ctxt, obj),
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XPathPoint => { /* points are ignored in XInclude */ }
        _ => {}
    }
    list
}

/// Load the document, and store the result in the XInclude context
///
/// Returns 0 in case of success, -1 in case of failure
#[doc(alias = "xmlXIncludeLoadDoc")]
unsafe fn xml_xinclude_load_doc(
    ctxt: XmlXincludeCtxtPtr,
    url: *const XmlChar,
    refe: XmlXincludeRefPtr,
) -> i32 {
    let mut cache: XmlXincludeDocPtr;
    let doc: XmlDocPtr;
    let mut fragment: *mut XmlChar = null_mut();
    let mut ret: i32 = -1;
    let cache_nr: i32;
    #[cfg(feature = "xpointer")]
    let save_flags: i32;

    // Check the URL and remove any fragment identifier
    let uri: XmlURIPtr = xml_parse_uri(url as _);
    if uri.is_null() {
        let url = CStr::from_ptr(url as *const i8).to_string_lossy();
        xml_xinclude_err!(
            ctxt,
            (*refe).elem,
            XmlParserErrors::XmlXIncludeHrefURI,
            "invalid value URI {}\n",
            url
        );
        // goto error;
        return ret;
    }
    if !(*uri).fragment.is_null() {
        fragment = (*uri).fragment as _;
        (*uri).fragment = null_mut();
    }
    if !(*refe).fragment.is_null() {
        if !fragment.is_null() {
            xml_free(fragment as _);
        }
        fragment = xml_strdup((*refe).fragment);
    }
    let mut url = {
        let tmp = xml_save_uri(uri);
        xml_free_uri(uri);
        if tmp.is_null() {
            let url = CStr::from_ptr(url as *const i8).to_string_lossy();
            xml_xinclude_err!(
                ctxt,
                (*refe).elem,
                XmlParserErrors::XmlXIncludeHrefURI,
                "invalid value URI {}\n",
                url
            );
            // goto error;
            xml_free(fragment as _);
            return ret;
        }
        tmp
    };

    'error: {
        // Handling of references to the local document are done
        // directly through (*ctxt).doc.
        'load: {
            if *url.add(0) == 0
                || *url.add(0) == b'#'
                || (!(*ctxt).doc.is_null()
                    && (*(*ctxt).doc).url.as_deref()
                        == CStr::from_ptr(url as *const i8).to_str().ok())
            {
                doc = (*ctxt).doc;
                break 'load;
            }
            // Prevent reloading the document twice.
            for i in 0..(*ctxt).url_nr {
                if xml_str_equal(url, (*(*ctxt).url_tab.add(i as usize)).url) {
                    if (*(*ctxt).url_tab.add(i as usize)).expanding != 0 {
                        xml_xinclude_err!(
                            ctxt,
                            (*refe).elem,
                            XmlParserErrors::XmlXIncludeRecursion,
                            "inclusion loop detected\n"
                        );
                        break 'error;
                    }
                    doc = (*(*ctxt).url_tab.add(i as usize)).doc;
                    if doc.is_null() {
                        break 'error;
                    }
                    break 'load;
                }
            }

            // Load it.
            #[cfg(feature = "xpointer")]
            {
                // If this is an XPointer evaluation, we want to assure that
                // all entities have been resolved prior to processing the
                // referenced document
                save_flags = (*ctxt).parse_flags;
                if !fragment.is_null() {
                    /* if this is an XPointer eval */
                    (*ctxt).parse_flags |= XmlParserOption::XmlParseNoent as i32;
                }
            }

            doc = xml_xinclude_parse_file(
                ctxt,
                CStr::from_ptr(url as *const i8).to_string_lossy().as_ref(),
            );
            #[cfg(feature = "xpointer")]
            {
                (*ctxt).parse_flags = save_flags;
            }

            /* Also cache NULL docs */
            if (*ctxt).url_nr >= (*ctxt).url_max {
                let new_size: usize = if (*ctxt).url_max != 0 {
                    (*ctxt).url_max as usize * 2
                } else {
                    8
                };

                let tmp: *mut XmlXincludeDoc =
                    xml_realloc((*ctxt).url_tab as _, size_of::<XmlXincludeDoc>() * new_size) as _;
                if tmp.is_null() {
                    xml_xinclude_err_memory(ctxt, (*refe).elem, Some("growing XInclude URL table"));
                    xml_free_doc(doc);
                    break 'error;
                }
                (*ctxt).url_max = new_size as _;
                (*ctxt).url_tab = tmp;
            }
            cache_nr = (*ctxt).url_nr;
            (*ctxt).url_nr += 1;
            cache = (*ctxt).url_tab.add(cache_nr as usize);
            (*cache).doc = doc;
            (*cache).url = xml_strdup(url);
            (*cache).expanding = 0;

            if doc.is_null() {
                break 'error;
            }
            // It's possible that the requested URL has been mapped to a
            // completely different location (e.g. through a catalog entry).
            // To check for this, we compare the URL with that of the doc
            // and change it if they disagree (bug 146988).
            if (*doc).url.as_deref() != CStr::from_ptr(url as *const i8).to_str().ok() {
                xml_free(url as _);
                let new = CString::new((*doc).url.as_deref().unwrap()).unwrap();
                url = xml_strdup(new.as_ptr() as *const u8);
            }

            // Make sure we have all entities fixed up
            xml_xinclude_merge_entities(ctxt, (*ctxt).doc, doc);

            /*
             * We don't need the DTD anymore, free up space
            if ((*doc).intSubset != null_mut()) {
            xmlUnlinkNode((xmlNodePtr) (*doc).intSubset);
            xmlFreeNode((xmlNodePtr) (*doc).intSubset);
            (*doc).intSubset = NULL;
            }
            if ((*doc).extSubset != null_mut()) {
            xmlUnlinkNode((xmlNodePtr) (*doc).extSubset);
            xmlFreeNode((xmlNodePtr) (*doc).extSubset);
            (*doc).extSubset = NULL;
            }
             */
            (*cache).expanding = 1;
            xml_xinclude_recurse_doc(ctxt, doc, url);
            /* urlTab might be reallocated. */
            cache = (*ctxt).url_tab.add(cache_nr as usize);
            (*cache).expanding = 0;
        }

        // loaded:
        if fragment.is_null() {
            // Add the top children list as the replacement copy.
            (*refe).inc = xml_doc_copy_node((*doc).get_root_element(), (*ctxt).doc, 1);
        } else {
            #[cfg(feature = "xpointer")]
            {
                // Computes the XPointer expression and make a copy used
                // as the replacement copy.

                if (*ctxt).is_stream != 0 && doc == (*ctxt).doc {
                    xml_xinclude_err!(
                        ctxt,
                        (*refe).elem,
                        XmlParserErrors::XmlXIncludeXPtrFailed,
                        "XPointer expressions not allowed in streaming mode\n"
                    );
                    break 'error;
                }

                let xptrctxt: XmlXPathContextPtr =
                    xml_xptr_new_context(doc, null_mut(), null_mut());
                if xptrctxt.is_null() {
                    xml_xinclude_err!(
                        ctxt,
                        (*refe).elem,
                        XmlParserErrors::XmlXIncludeXPtrFailed,
                        "could not create XPointer context\n"
                    );
                    break 'error;
                }
                let xptr: XmlXPathObjectPtr = xml_xptr_eval(fragment, xptrctxt);
                if xptr.is_null() {
                    let fragment = CStr::from_ptr(fragment as *const i8).to_string_lossy();
                    xml_xinclude_err!(
                        ctxt,
                        (*refe).elem,
                        XmlParserErrors::XmlXIncludeXPtrFailed,
                        "XPointer evaluation failed: #{}\n",
                        fragment
                    );
                    xml_xpath_free_context(xptrctxt);
                    break 'error;
                }
                match (*xptr).typ {
                    XmlXPathObjectType::XPathUndefined
                    | XmlXPathObjectType::XPathBoolean
                    | XmlXPathObjectType::XPathNumber
                    | XmlXPathObjectType::XPathString
                    | XmlXPathObjectType::XPathUsers
                    | XmlXPathObjectType::XPathXSLTTree => {
                        let fragment = CStr::from_ptr(fragment as *const i8).to_string_lossy();
                        xml_xinclude_err!(
                            ctxt,
                            (*refe).elem,
                            XmlParserErrors::XmlXIncludeXPtrResult,
                            "XPointer is not a range: #{}\n",
                            fragment
                        );
                        xml_xpath_free_object(xptr);
                        xml_xpath_free_context(xptrctxt);
                        break 'error;
                    }
                    #[cfg(feature = "libxml_xptr_locs")]
                    XmlXPathObjectType::XPathPoint => {
                        let fragment = CStr::from_ptr(fragment as *const i8).to_string_lossy();
                        xml_xinclude_err!(
                            ctxt,
                            (*refe).elem,
                            XmlParserErrors::XmlXIncludeXPtrResult,
                            "XPointer is not a range: #{}\n",
                            fragment
                        );
                        xml_xpath_free_object(xptr);
                        xml_xpath_free_context(xptrctxt);
                        break 'error;
                    }
                    XmlXPathObjectType::XPathNodeset => {
                        if (*xptr).nodesetval.as_deref().map_or(true, |n| n.is_empty()) {
                            xml_xpath_free_object(xptr);
                            xml_xpath_free_context(xptrctxt);
                            break 'error;
                        }
                    }
                    #[cfg(feature = "libxml_xptr_locs")]
                    XmlXPathObjectType::XPathRange | XmlXPathObjectType::XPathLocationset => {} // _ => {}
                }
                if let Some(set) = (*xptr).nodesetval.as_deref_mut() {
                    for node in set.node_tab.iter_mut() {
                        if node.is_null() {
                            continue;
                        }
                        match (**node).element_type() {
                            XmlElementType::XmlElementNode
                            | XmlElementType::XmlTextNode
                            | XmlElementType::XmlCDATASectionNode
                            | XmlElementType::XmlEntityRefNode
                            | XmlElementType::XmlEntityNode
                            | XmlElementType::XmlPINode
                            | XmlElementType::XmlCommentNode
                            | XmlElementType::XmlDocumentNode
                            | XmlElementType::XmlHTMLDocumentNode => continue,

                            XmlElementType::XmlAttributeNode => {
                                let fragment =
                                    CStr::from_ptr(fragment as *const i8).to_string_lossy();
                                xml_xinclude_err!(
                                    ctxt,
                                    (*refe).elem,
                                    XmlParserErrors::XmlXIncludeXPtrResult,
                                    "XPointer selects an attribute: #{}\n",
                                    fragment
                                );
                                *node = null_mut();
                                continue;
                            }
                            XmlElementType::XmlNamespaceDecl => {
                                let fragment =
                                    CStr::from_ptr(fragment as *const i8).to_string_lossy();
                                xml_xinclude_err!(
                                    ctxt,
                                    (*refe).elem,
                                    XmlParserErrors::XmlXIncludeXPtrResult,
                                    "XPointer selects a namespace: #{}\n",
                                    fragment
                                );
                                *node = null_mut();
                                continue;
                            }
                            XmlElementType::XmlDocumentTypeNode
                            | XmlElementType::XmlDocumentFragNode
                            | XmlElementType::XmlNotationNode
                            | XmlElementType::XmlDTDNode
                            | XmlElementType::XmlElementDecl
                            | XmlElementType::XmlAttributeDecl
                            | XmlElementType::XmlEntityDecl
                            | XmlElementType::XmlXIncludeStart
                            | XmlElementType::XmlXIncludeEnd => {
                                let fragment =
                                    CStr::from_ptr(fragment as *const i8).to_string_lossy();
                                xml_xinclude_err!(
                                    ctxt,
                                    (*refe).elem,
                                    XmlParserErrors::XmlXIncludeXPtrResult,
                                    "XPointer selects unexpected nodes: #{}\n",
                                    fragment
                                );
                                *node = null_mut();
                                *node = null_mut();
                                continue; /* for */
                            }
                            _ => unreachable!(),
                        }
                    }
                }
                (*refe).inc = xml_xinclude_copy_xpointer(ctxt, xptr);
                xml_xpath_free_object(xptr);
                xml_xpath_free_context(xptrctxt);
            }
        }

        // Do the xml:base fixup if needed
        if !doc.is_null()
            && !url.is_null()
            && (*ctxt).parse_flags & XmlParserOption::XmlParseNobasefix as i32 == 0
            && (*doc).parse_flags & XmlParserOption::XmlParseNobasefix as i32 == 0
        {
            let mut node: XmlNodePtr;

            // The base is only adjusted if "necessary", i.e. if the xinclude node
            // has a base specified, or the URL is relative
            let base = (*(*refe).elem)
                .get_ns_prop("base", XML_XML_NAMESPACE.to_str().ok())
                .map(|b| CString::new(b).unwrap());
            let mut base = base
                .as_ref()
                .map_or(null_mut(), |b| xml_strdup(b.as_ptr() as *const u8));
            if base.is_null() {
                // No xml:base on the xinclude node, so we check whether the
                // URI base is different than (relative to) the context base
                let cur_base = xml_build_relative_uri(url, (*ctxt).base);
                if cur_base.is_null() {
                    let url = CStr::from_ptr(url as *const i8).to_string_lossy();
                    // Error return
                    xml_xinclude_err!(
                        ctxt,
                        (*refe).elem,
                        XmlParserErrors::XmlXIncludeHrefURI,
                        "trying to build relative URI from {}\n",
                        url
                    );
                } else {
                    /* If the URI doesn't contain a slash, it's not relative */
                    if xml_strchr(cur_base, b'/').is_null() {
                        xml_free(cur_base as _);
                    } else {
                        base = cur_base;
                    }
                }
            }
            if !base.is_null() {
                // Adjustment may be needed
                node = (*refe).inc;
                while !node.is_null() {
                    // Only work on element nodes
                    if (*node).element_type() == XmlElementType::XmlElementNode {
                        if let Some(cur_base) = (*node).get_base((*node).doc) {
                            // If the current base is the same as the
                            // URL of the document, then reset it to be
                            // the specified xml:base or the relative URI
                            if (*(*node).doc).url.as_deref() == Some(cur_base.as_str()) {
                                (*node).set_base(Some(
                                    CStr::from_ptr(base as *const i8).to_string_lossy().as_ref(),
                                ));
                            } else {
                                // If the element already has an xml:base set,
                                // then relativise it if necessary

                                if let Some(xml_base) =
                                    (*node).get_ns_prop("base", XML_XML_NAMESPACE.to_str().ok())
                                {
                                    let base = CStr::from_ptr(base as *const i8).to_string_lossy();
                                    let rel_base = build_uri(&xml_base, &base);
                                    if let Some(rel_base) = rel_base {
                                        (*node).set_base(Some(&rel_base));
                                    } else {
                                        // error
                                        xml_xinclude_err!(
                                            ctxt,
                                            (*refe).elem,
                                            XmlParserErrors::XmlXIncludeHrefURI,
                                            "trying to rebuild base from {}\n",
                                            xml_base
                                        );
                                    }
                                }
                            }
                        } else {
                            // If no current base, set it
                            (*node).set_base(Some(
                                CStr::from_ptr(base as *const i8).to_string_lossy().as_ref(),
                            ));
                        }
                    }
                    node = (*node).next.map_or(null_mut(), |n| n.as_ptr());
                }
                xml_free(base as _);
            }
        }
        ret = 0;
    }

    // error:
    xml_free(url as _);
    xml_free(fragment as _);
    ret
}

/// Load the content, and store the result in the XInclude context
///
/// Returns 0 in case of success, -1 in case of failure
#[doc(alias = "xmlXIncludeLoadTxt")]
unsafe extern "C" fn xml_xinclude_load_txt(
    ctxt: XmlXincludeCtxtPtr,
    mut url: *const XmlChar,
    refe: XmlXincludeRefPtr,
) -> i32 {
    let mut node: XmlNodePtr = null_mut();
    let mut i: i32;
    let mut ret: i32 = -1;
    let mut enc = XmlCharEncoding::None;
    let mut pctxt: XmlParserCtxtPtr = null_mut();
    let mut input_stream: XmlParserInputPtr = null_mut();

    /* Don't read from stdin. */
    if xml_strcmp(url, c"-".as_ptr() as _) == 0 {
        url = c"./-".as_ptr() as _;
    }

    // Check the URL and remove any fragment identifier
    let uri: XmlURIPtr = xml_parse_uri(url as _);
    if uri.is_null() {
        let url = CStr::from_ptr(url as *const i8).to_string_lossy();
        xml_xinclude_err!(
            ctxt,
            (*refe).elem,
            XmlParserErrors::XmlXIncludeHrefURI,
            "invalid value URI {}\n",
            url
        );
        // goto error;
        xml_free_node(node);
        xml_free_input_stream(input_stream);
        xml_free_parser_ctxt(pctxt);
        xml_free_uri(uri);
        return ret;
    }
    if !(*uri).fragment.is_null() {
        let fragment = CStr::from_ptr((*uri).fragment as *const i8).to_string_lossy();
        xml_xinclude_err!(
            ctxt,
            (*refe).elem,
            XmlParserErrors::XmlXIncludeTextFragment,
            "fragment identifier forbidden for text: {}\n",
            fragment
        );
        // goto error;
        xml_free_node(node);
        xml_free_input_stream(input_stream);
        xml_free_parser_ctxt(pctxt);
        xml_free_uri(uri);
        return ret;
    }
    let url = {
        let tmp = xml_save_uri(uri);
        if tmp.is_null() {
            let url = CStr::from_ptr(url as *const i8).to_string_lossy();
            xml_xinclude_err!(
                ctxt,
                (*refe).elem,
                XmlParserErrors::XmlXIncludeHrefURI,
                "invalid value URI {}\n",
                url
            );
            // goto error;
            xml_free_node(node);
            xml_free_input_stream(input_stream);
            xml_free_parser_ctxt(pctxt);
            xml_free_uri(uri);
            return ret;
        }
        tmp
    };

    // Handling of references to the local document are done directly through (*ctxt).doc.
    if *url.add(0) == 0 {
        xml_xinclude_err!(
            ctxt,
            (*refe).elem,
            XmlParserErrors::XmlXIncludeTextDocument,
            "text serialization of document not available\n"
        );
        // goto error;
        xml_free_node(node);
        xml_free_input_stream(input_stream);
        xml_free_parser_ctxt(pctxt);
        xml_free_uri(uri);
        xml_free(url as _);
        return ret;
    }

    // Prevent reloading the document twice.
    for i in 0..(*ctxt).txt_nr {
        if xml_str_equal(url, (*(*ctxt).txt_tab.add(i as usize)).url) {
            node = xml_new_doc_text((*ctxt).doc, (*(*ctxt).txt_tab.add(i as usize)).text);
            // goto loaded;
            (*refe).inc = node;
            node = null_mut();
            xml_free_node(node);
            xml_free_input_stream(input_stream);
            xml_free_parser_ctxt(pctxt);
            xml_free_uri(uri);
            xml_free(url as _);
            return 0;
        }
    }

    // Try to get the encoding if available
    let mut encoding = None;
    if !(*refe).elem.is_null() {
        encoding = (*(*refe).elem).get_prop(XINCLUDE_PARSE_ENCODING.to_str().unwrap());
    }
    if let Some(encoding) = encoding {
        // TODO: we should not have to remap to the xmlCharEncoding
        //       predefined set, a better interface than
        //       xmlParserInputBufferCreateFilename should allow any
        //       encoding supported by iconv
        match encoding.parse::<XmlCharEncoding>() {
            Ok(e) => enc = e,
            _ => {
                xml_xinclude_err!(
                    ctxt,
                    (*refe).elem,
                    XmlParserErrors::XmlXIncludeUnknownEncoding,
                    "encoding {} not supported\n",
                    encoding
                );
                // goto error;
                xml_free_node(node);
                xml_free_input_stream(input_stream);
                xml_free_parser_ctxt(pctxt);
                xml_free_uri(uri);
                xml_free(url as _);
                return ret;
            }
        }
    }

    // Load it.
    pctxt = xml_new_parser_ctxt();
    input_stream = xml_load_external_entity(
        Some(CStr::from_ptr(url as *const i8).to_string_lossy().as_ref()),
        None,
        pctxt,
    );
    if input_stream.is_null() {
        // goto error;
        xml_free_node(node);
        xml_free_input_stream(input_stream);
        xml_free_parser_ctxt(pctxt);
        xml_free_uri(uri);
        xml_free(url as _);
        return ret;
    }
    let Some(buf) = (*input_stream).buf.as_mut() else {
        // goto error;
        xml_free_node(node);
        xml_free_input_stream(input_stream);
        xml_free_parser_ctxt(pctxt);
        xml_free_uri(uri);
        xml_free(url as _);
        return ret;
    };
    buf.borrow_mut().encoder = get_encoding_handler(enc);
    node = xml_new_doc_text((*ctxt).doc, null_mut());
    if node.is_null() {
        xml_xinclude_err_memory(ctxt, (*refe).elem, None);
        // goto error;
        xml_free_node(node);
        xml_free_input_stream(input_stream);
        xml_free_parser_ctxt(pctxt);
        xml_free_uri(uri);
        xml_free(url as _);
        return ret;
    }

    // Scan all chars from the resource and add the to the node
    while buf.borrow_mut().grow(4096) > 0 {}

    let content: *const XmlChar = buf.borrow().buffer.map_or(null_mut(), |buf| {
        if buf.is_ok() {
            buf.as_ref().as_ptr()
        } else {
            null_mut()
        }
    });
    let len: i32 = buf.borrow().buffer.map_or(0, |buf| buf.len()) as i32;
    i = 0;
    while i < len {
        let mut l: i32 = 0;

        let cur: i32 =
            xml_string_current_char(null_mut(), content.add(i as usize), addr_of_mut!(l));
        if !xml_is_char(cur as u32) {
            let u = CStr::from_ptr(url as *const i8).to_string_lossy();
            xml_xinclude_err!(
                ctxt,
                (*refe).elem,
                XmlParserErrors::XmlXIncludeInvalidChar,
                "{} contains invalid char\n",
                u
            );
            // goto error;
            xml_free_node(node);
            xml_free_input_stream(input_stream);
            xml_free_parser_ctxt(pctxt);
            xml_free_uri(uri);
            xml_free(url as _);
            return ret;
        }

        i += l;
    }

    (*node).add_content_len(content, len);

    if (*ctxt).txt_nr >= (*ctxt).txt_max {
        let new_size: usize = if (*ctxt).txt_max != 0 {
            (*ctxt).txt_max as usize * 2
        } else {
            8
        };

        let tmp: *mut XmlXincludeTxt =
            xml_realloc((*ctxt).txt_tab as _, size_of::<XmlXincludeTxt>() * new_size) as _;
        if tmp.is_null() {
            xml_xinclude_err_memory(ctxt, (*refe).elem, Some("growing XInclude text table"));
            // goto error;
            xml_free_node(node);
            xml_free_input_stream(input_stream);
            xml_free_parser_ctxt(pctxt);
            xml_free_uri(uri);
            xml_free(url as _);
            return ret;
        }
        (*ctxt).txt_max = new_size as _;
        (*ctxt).txt_tab = tmp;
    }
    (*(*ctxt).txt_tab.add((*ctxt).txt_nr as usize)).text = xml_strdup((*node).content);
    (*(*ctxt).txt_tab.add((*ctxt).txt_nr as usize)).url = xml_strdup(url);
    (*ctxt).txt_nr += 1;

    // loaded:
    // Add the element as the replacement copy.
    (*refe).inc = node;
    node = null_mut();
    ret = 0;

    // error:
    xml_free_node(node);
    xml_free_input_stream(input_stream);
    xml_free_parser_ctxt(pctxt);
    xml_free_uri(uri);
    xml_free(url as _);
    ret
}

/// Load the content of the fallback node, and store the result in the XInclude context
///
/// Returns 0 in case of success, -1 in case of failure
#[doc(alias = "xmlXIncludeLoadFallback")]
unsafe extern "C" fn xml_xinclude_load_fallback(
    ctxt: XmlXincludeCtxtPtr,
    fallback: XmlNodePtr,
    refe: XmlXincludeRefPtr,
) -> i32 {
    let mut ret: i32 = 0;
    let old_nb_errors: i32;

    if fallback.is_null()
        || (*fallback).element_type() == XmlElementType::XmlNamespaceDecl
        || ctxt.is_null()
    {
        return -1;
    }
    if (*fallback).children().is_some() {
        /*
         * It's possible that the fallback also has 'includes'
         * (Bug 129969), so we re-process the fallback just in case
         */
        old_nb_errors = (*ctxt).nb_errors;
        (*refe).inc = xml_xinclude_copy_node(ctxt, fallback, 1);
        if (*ctxt).nb_errors > old_nb_errors {
            ret = -1;
        } else if (*refe).inc.is_null() {
            (*refe).empty_fb = 1;
        }
    } else {
        (*refe).inc = null_mut();
        (*refe).empty_fb = 1; /* flag empty callback */
    }
    (*refe).fallback = 1;
    ret
}

/// Find and load the infoset replacement for the given node.
///
/// Returns 0 if substitution succeeded, -1 if some processing failed
#[doc(alias = "xmlXIncludeLoadNode")]
unsafe extern "C" fn xml_xinclude_load_node(
    ctxt: XmlXincludeCtxtPtr,
    refe: XmlXincludeRefPtr,
) -> i32 {
    let mut xml: i32 = 1; /* default Issue 64 */
    let mut ret: i32;

    if ctxt.is_null() || refe.is_null() {
        return -1;
    }
    let cur: XmlNodePtr = (*refe).elem;
    if cur.is_null() {
        return -1;
    }

    // read the attributes
    let href =
        xml_xinclude_get_prop(ctxt, cur, XINCLUDE_HREF.to_str().unwrap()).unwrap_or("".to_owned());
    let parse = xml_xinclude_get_prop(ctxt, cur, XINCLUDE_PARSE.to_str().unwrap());
    if let Some(parse) = parse {
        if parse == XINCLUDE_PARSE_XML {
            xml = 1;
        } else if parse == XINCLUDE_PARSE_TEXT {
            xml = 0;
        } else {
            xml_xinclude_err!(
                ctxt,
                cur,
                XmlParserErrors::XmlXIncludeParseValue,
                "invalid value {} for 'parse'\n",
                parse
            );
            return -1;
        }
    }

    // compute the URI
    let mut base = None;
    let mut uri = if let Some(b) = (*cur).get_base((*ctxt).doc) {
        base = Some(b);
        build_uri(&href, base.as_deref().unwrap())
    } else {
        (*(*ctxt).doc)
            .url
            .as_deref()
            .and_then(|base| build_uri(&href, base))
    };
    if uri.is_none() {
        if let Some(base) = base.as_deref() {
            // Some escaping may be needed
            if let (Some(escbase), Some(eschref)) = (escape_url(base), escape_url(&href)) {
                uri = build_uri(&eschref, &escbase);
            }
        }
    }
    let Some(uri) = uri else {
        xml_xinclude_err!(
            ctxt,
            cur,
            XmlParserErrors::XmlXIncludeHrefURI,
            "failed build URL\n"
        );
        return -1;
    };
    let uri = CString::new(uri).unwrap();

    // Save the base for this include (saving the current one)
    let old_base: *mut XmlChar = (*ctxt).base;
    (*ctxt).base = base.as_ref().map_or(null_mut(), |b| b.as_ptr() as *mut u8);

    if xml != 0 {
        ret = xml_xinclude_load_doc(ctxt, uri.as_ptr() as *const u8, refe);
    // xmlXIncludeGetFragment(ctxt, cur, URI);
    } else {
        ret = xml_xinclude_load_txt(ctxt, uri.as_ptr() as *const u8, refe);
    }

    // Restore the original base before checking for fallback
    (*ctxt).base = old_base;

    if ret < 0 {
        // Time to try a fallback if available
        let mut children = (*cur).children().map_or(null_mut(), |c| c.as_ptr());
        while !children.is_null() {
            if (*children).element_type() == XmlElementType::XmlElementNode
                && !(*children).ns.is_null()
                && (*children).name().as_deref() == Some(XINCLUDE_FALLBACK)
                && (xml_str_equal((*(*children).ns).href, XINCLUDE_NS.as_ptr() as _)
                    || xml_str_equal((*(*children).ns).href, XINCLUDE_OLD_NS.as_ptr() as _))
            {
                ret = xml_xinclude_load_fallback(ctxt, children, refe);
                break;
            }
            children = (*children).next.map_or(null_mut(), |n| n.as_ptr());
        }
    }
    if ret < 0 {
        xml_xinclude_err!(
            ctxt,
            cur,
            XmlParserErrors::XmlXIncludeNoFallback,
            "could not load {}, and no fallback was found\n",
            uri.to_string_lossy().into_owned()
        );
    }

    0
}

/// If the XInclude node wasn't processed yet, create a new RefPtr,
/// add it to (*ctxt).incTab and load the included items.
///
/// Returns the new or existing xmlXIncludeRefPtr, or NULL in case of error.
#[doc(alias = "xmlXIncludeExpandNode")]
unsafe extern "C" fn xml_xinclude_expand_node(
    ctxt: XmlXincludeCtxtPtr,
    node: XmlNodePtr,
) -> XmlXincludeRefPtr {
    if (*ctxt).fatal_err != 0 {
        return null_mut();
    }
    if (*ctxt).depth >= XINCLUDE_MAX_DEPTH {
        xml_xinclude_err!(
            ctxt,
            node,
            XmlParserErrors::XmlXIncludeRecursion,
            "maximum recursion depth exceeded\n"
        );
        (*ctxt).fatal_err = 1;
        return null_mut();
    }

    for i in 0..(*ctxt).inc_nr {
        if (*(*(*ctxt).inc_tab.add(i as usize))).elem == node {
            if (*(*(*ctxt).inc_tab.add(i as usize))).expanding != 0 {
                xml_xinclude_err!(
                    ctxt,
                    node,
                    XmlParserErrors::XmlXIncludeRecursion,
                    "inclusion loop detected\n"
                );
                return null_mut();
            }
            return *(*ctxt).inc_tab.add(i as usize);
        }
    }

    let refe: XmlXincludeRefPtr = xml_xinclude_add_node(ctxt, node);
    if refe.is_null() {
        return null_mut();
    }
    (*refe).expanding = 1;
    (*ctxt).depth += 1;
    xml_xinclude_load_node(ctxt, refe);
    (*ctxt).depth -= 1;
    (*refe).expanding = 0;

    refe
}

/// Implement the infoset replacement for the given node
///
/// Returns 0 if substitution succeeded, -1 if some processing failed
#[doc(alias = "xmlXIncludeIncludeNode")]
unsafe extern "C" fn xml_xinclude_include_node(
    ctxt: XmlXincludeCtxtPtr,
    refe: XmlXincludeRefPtr,
) -> i32 {
    let mut cur: XmlNodePtr;
    let mut end: XmlNodePtr;
    let mut list: XmlNodePtr;
    let mut tmp: XmlNodePtr;

    if ctxt.is_null() || refe.is_null() {
        return -1;
    }
    cur = (*refe).elem;
    if cur.is_null() || (*cur).element_type() == XmlElementType::XmlNamespaceDecl {
        return -1;
    }

    list = (*refe).inc;
    (*refe).inc = null_mut();
    (*refe).empty_fb = 0;

    // Check against the risk of generating a multi-rooted document
    if (*cur)
        .parent()
        .filter(|p| p.element_type() != XmlElementType::XmlElementNode)
        .is_some()
    {
        let mut nb_elem: i32 = 0;

        tmp = list;
        while !tmp.is_null() {
            if (*tmp).element_type() == XmlElementType::XmlElementNode {
                nb_elem += 1;
            }
            tmp = (*tmp).next.map_or(null_mut(), |n| n.as_ptr());
        }
        if nb_elem > 1 {
            xml_xinclude_err!(
                ctxt,
                (*refe).elem,
                XmlParserErrors::XmlXIncludeMultipleRoot,
                "XInclude error: would result in multiple root nodes\n"
            );
            xml_free_node_list(list);
            return -1;
        }
    }

    if (*ctxt).parse_flags & XmlParserOption::XmlParseNoxincnode as i32 != 0 {
        // Add the list of nodes
        while !list.is_null() {
            end = list;
            list = (*list).next.map_or(null_mut(), |n| n.as_ptr());

            (*cur).add_prev_sibling(end);
        }
        /*
         * FIXME: xmlUnlinkNode doesn't coalesce text nodes.
         */
        (*cur).unlink();
        xml_free_node(cur);
    } else {
        // Change the current node as an XInclude start one, and add an XInclude end one
        if (*refe).fallback != 0 {
            (*cur).unset_prop("href");
        }
        (*cur).typ = XmlElementType::XmlXIncludeStart;
        // Remove fallback children
        let mut child = (*cur).children();
        while let Some(mut now) = child {
            let next = now.next;
            now.unlink();
            xml_free_node(now.as_ptr());
            child = next;
        }
        end = xml_new_doc_node((*cur).doc, (*cur).ns, (*cur).name, null_mut());
        if end.is_null() {
            xml_xinclude_err!(
                ctxt,
                (*refe).elem,
                XmlParserErrors::XmlXIncludeBuildFailed,
                "failed to build node\n"
            );
            xml_free_node_list(list);
            return -1;
        }
        (*end).typ = XmlElementType::XmlXIncludeEnd;
        (*cur).add_next_sibling(end);

        // Add the list of nodes
        while !list.is_null() {
            cur = list;
            list = (*list).next.map_or(null_mut(), |n| n.as_ptr());

            (*end).add_prev_sibling(cur);
        }
    }

    0
}

/// Implement the XInclude substitution on the XML document @doc
///
/// Returns 0 if no substitution were done, -1 if some processing failed
/// or the number of substitutions done.
#[doc(alias = "xmlXIncludeDoProcess")]
unsafe extern "C" fn xml_xinclude_do_process(ctxt: XmlXincludeCtxtPtr, tree: XmlNodePtr) -> i32 {
    let mut refe: XmlXincludeRefPtr;
    let mut cur: XmlNodePtr;
    let mut ret: i32 = 0;

    if tree.is_null() || (*tree).element_type() == XmlElementType::XmlNamespaceDecl {
        return -1;
    }
    if ctxt.is_null() {
        return -1;
    }

    /*
     * First phase: lookup the elements in the document
     */
    let start: i32 = (*ctxt).inc_nr;
    cur = tree;
    while {
        'inner: {
            /* TODO: need to work on entities -> stack */
            if xml_xinclude_test_node(ctxt, cur) == 1 {
                refe = xml_xinclude_expand_node(ctxt, cur);
                /*
                 * Mark direct includes.
                 */
                if !refe.is_null() {
                    (*refe).replace = 1;
                }
            } else if let Some(children) = (*cur).children().filter(|_| {
                matches!(
                    (*cur).element_type(),
                    XmlElementType::XmlDocumentNode | XmlElementType::XmlElementNode
                )
            }) {
                cur = children.as_ptr();
                break 'inner;
            }
            'b: while {
                if cur == tree {
                    break 'b;
                }
                if let Some(next) = (*cur).next {
                    cur = next.as_ptr();
                    break 'b;
                }
                cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());

                !cur.is_null()
            } {}
        }

        !cur.is_null() && cur != tree
    } {}

    /*
     * Second phase: extend the original document infoset.
     */
    for i in start..(*ctxt).inc_nr {
        if (*(*(*ctxt).inc_tab.add(i as usize))).replace != 0 {
            if !(*(*(*ctxt).inc_tab.add(i as usize))).inc.is_null()
                || (*(*(*ctxt).inc_tab.add(i as usize))).empty_fb != 0
            {
                /* (empty fallback) */
                xml_xinclude_include_node(ctxt, *(*ctxt).inc_tab.add(i as usize));
            }
            (*(*(*ctxt).inc_tab.add(i as usize))).replace = 0;
        } else {
            /*
             * Ignore includes which were added indirectly, for example
             * inside xi:fallback elements.
             */
            if !(*(*(*ctxt).inc_tab.add(i as usize))).inc.is_null() {
                xml_free_node_list((*(*(*ctxt).inc_tab.add(i as usize))).inc);
                (*(*(*ctxt).inc_tab.add(i as usize))).inc = null_mut();
            }
        }
        ret += 1;
    }

    if (*ctxt).is_stream != 0 {
        /*
         * incTab references nodes which will eventually be deleted in
         * streaming mode. The table is only required for XPointer
         * expressions which aren't allowed in streaming mode.
         */
        for i in 0..(*ctxt).inc_nr {
            xml_xinclude_free_ref(*(*ctxt).inc_tab.add(i as usize));
        }
        (*ctxt).inc_nr = 0;
    }

    ret
}

/// Implement the XInclude substitution on the XML node @tree
///
/// Returns 0 if no substitution were done, -1 if some processing failed
/// or the number of substitutions done.
#[doc(alias = "xmlXIncludeProcessTreeFlagsData")]
pub unsafe extern "C" fn xml_xinclude_process_tree_flags_data(
    tree: XmlNodePtr,
    flags: i32,
    data: *mut c_void,
) -> i32 {
    let mut ret: i32;

    if tree.is_null()
        || (*tree).element_type() == XmlElementType::XmlNamespaceDecl
        || (*tree).doc.is_null()
    {
        return -1;
    }

    let ctxt: XmlXincludeCtxtPtr = xml_xinclude_new_context((*tree).doc);
    if ctxt.is_null() {
        return -1;
    }
    (*ctxt)._private = data;
    let url = (*(*tree).doc)
        .url
        .as_deref()
        .map(|u| CString::new(u).unwrap());
    (*ctxt).base = xml_strdup(url.as_ref().map_or(null(), |u| u.as_ptr() as *const u8));
    xml_xinclude_set_flags(ctxt, flags);
    ret = xml_xinclude_do_process(ctxt, tree);
    if ret >= 0 && (*ctxt).nb_errors > 0 {
        ret = -1;
    }

    xml_xinclude_free_context(ctxt);
    ret
}

/// Implement the XInclude substitution for the given subtree
///
/// Returns 0 if no substitution were done, -1 if some processing failed
/// or the number of substitutions done.
#[doc(alias = "xmlXIncludeProcessTree")]
pub unsafe extern "C" fn xml_xinclude_process_tree(tree: XmlNodePtr) -> i32 {
    xml_xinclude_process_tree_flags(tree, 0)
}

/// Implement the XInclude substitution for the given subtree
///
/// Returns 0 if no substitution were done, -1 if some processing failed
/// or the number of substitutions done.
#[doc(alias = "xmlXIncludeProcessTreeFlags")]
pub unsafe extern "C" fn xml_xinclude_process_tree_flags(tree: XmlNodePtr, flags: i32) -> i32 {
    let mut ret: i32;

    if tree.is_null()
        || (*tree).element_type() == XmlElementType::XmlNamespaceDecl
        || (*tree).doc.is_null()
    {
        return -1;
    }
    let ctxt: XmlXincludeCtxtPtr = xml_xinclude_new_context((*tree).doc);
    if ctxt.is_null() {
        return -1;
    }
    let tmp = (*tree)
        .get_base((*tree).doc)
        .map(|c| CString::new(c).unwrap());
    (*ctxt).base = tmp
        .as_ref()
        .map_or(null_mut(), |c| xml_strdup(c.as_ptr() as *const u8));
    xml_xinclude_set_flags(ctxt, flags);
    ret = xml_xinclude_do_process(ctxt, tree);
    if ret >= 0 && (*ctxt).nb_errors > 0 {
        ret = -1;
    }

    xml_xinclude_free_context(ctxt);
    ret
}

/// Creates a new XInclude context
///
/// Returns the new set
#[doc(alias = "xmlXIncludeNewContext")]
pub unsafe extern "C" fn xml_xinclude_new_context(doc: XmlDocPtr) -> XmlXincludeCtxtPtr {
    if doc.is_null() {
        return null_mut();
    }
    let ret: XmlXincludeCtxtPtr = xml_malloc(size_of::<XmlXincludeCtxt>()) as XmlXincludeCtxtPtr;
    if ret.is_null() {
        xml_xinclude_err_memory(
            null_mut(),
            doc as XmlNodePtr,
            Some("creating XInclude context"),
        );
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlXincludeCtxt>());
    (*ret).doc = doc;
    (*ret).inc_nr = 0;
    (*ret).inc_max = 0;
    (*ret).inc_tab = null_mut();
    (*ret).nb_errors = 0;
    ret
}

/// Set the flags used for further processing of XML resources.
///
/// Returns 0 in case of success and -1 in case of error.
#[doc(alias = "xmlXIncludeSetFlags")]
pub unsafe extern "C" fn xml_xinclude_set_flags(ctxt: XmlXincludeCtxtPtr, flags: i32) -> i32 {
    if ctxt.is_null() {
        return -1;
    }
    (*ctxt).parse_flags = flags;
    0
}

/// Free an XInclude context
#[doc(alias = "xmlXIncludeFreeContext")]
pub unsafe extern "C" fn xml_xinclude_free_context(ctxt: XmlXincludeCtxtPtr) {
    if ctxt.is_null() {
        return;
    }
    if !(*ctxt).url_tab.is_null() {
        for i in 0..(*ctxt).url_nr {
            xml_free_doc((*(*ctxt).url_tab.add(i as usize)).doc);
            xml_free((*(*ctxt).url_tab.add(i as usize)).url as _);
        }
        xml_free((*ctxt).url_tab as _);
    }
    for i in 0..(*ctxt).inc_nr {
        if !(*(*ctxt).inc_tab.add(i as usize)).is_null() {
            xml_xinclude_free_ref(*(*ctxt).inc_tab.add(i as usize));
        }
    }
    if !(*ctxt).inc_tab.is_null() {
        xml_free((*ctxt).inc_tab as _);
    }
    if !(*ctxt).txt_tab.is_null() {
        for i in 0..(*ctxt).txt_nr {
            xml_free((*(*ctxt).txt_tab.add(i as usize)).text as _);
            xml_free((*(*ctxt).txt_tab.add(i as usize)).url as _);
        }
        xml_free((*ctxt).txt_tab as _);
    }
    if !(*ctxt).base.is_null() {
        xml_free((*ctxt).base as _);
    }
    xml_free(ctxt as _);
}

/// Implement the XInclude substitution for the given subtree reusing
/// the information and data coming from the given context.
///
/// Returns 0 if no substitution were done, -1 if some processing failed
/// or the number of substitutions done.
#[doc(alias = "xmlXIncludeProcessNode")]
pub unsafe extern "C" fn xml_xinclude_process_node(
    ctxt: XmlXincludeCtxtPtr,
    node: XmlNodePtr,
) -> i32 {
    let mut ret: i32;

    if node.is_null()
        || (*node).element_type() == XmlElementType::XmlNamespaceDecl
        || (*node).doc.is_null()
        || ctxt.is_null()
    {
        return -1;
    }
    ret = xml_xinclude_do_process(ctxt, node);
    if ret >= 0 && (*ctxt).nb_errors > 0 {
        ret = -1;
    }
    ret
}

/// In streaming mode, XPointer expressions aren't allowed.
///
/// Returns 0 in case of success and -1 in case of error.
#[doc(alias = "xmlXIncludeSetStreamingMode")]
pub(crate) unsafe extern "C" fn xml_xinclude_set_streaming_mode(
    ctxt: XmlXincludeCtxtPtr,
    mode: i32,
) -> i32 {
    if ctxt.is_null() {
        return -1;
    }
    (*ctxt).is_stream = (mode != 0) as i32;
    0
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_xinclude_new_context() {

        /* missing type support */
    }

    #[test]
    fn test_xml_xinclude_process() {
        #[cfg(feature = "xinclude")]
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                let mem_base = xml_mem_blocks();
                let doc = gen_xml_doc_ptr(n_doc, 0);

                let ret_val = xml_xinclude_process(doc);
                desret_int(ret_val);
                des_xml_doc_ptr(n_doc, doc, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXIncludeProcess",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXIncludeProcess()"
                    );
                    eprintln!(" {}", n_doc);
                }
            }
        }
    }

    #[test]
    fn test_xml_xinclude_process_flags() {
        #[cfg(feature = "xinclude")]
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_flags in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let doc = gen_xml_doc_ptr(n_doc, 0);
                    let flags = gen_int(n_flags, 1);

                    let ret_val = xml_xinclude_process_flags(doc, flags);
                    desret_int(ret_val);
                    des_xml_doc_ptr(n_doc, doc, 0);
                    des_int(n_flags, flags, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXIncludeProcessFlags",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXIncludeProcessFlags()"
                        );
                        eprint!(" {}", n_doc);
                        eprintln!(" {}", n_flags);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xinclude_process_flags_data() {
        #[cfg(feature = "xinclude")]
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_flags in 0..GEN_NB_INT {
                    for n_data in 0..GEN_NB_USERDATA {
                        let mem_base = xml_mem_blocks();
                        let doc = gen_xml_doc_ptr(n_doc, 0);
                        let flags = gen_int(n_flags, 1);
                        let data = gen_userdata(n_data, 2);

                        let ret_val = xml_xinclude_process_flags_data(doc, flags, data);
                        desret_int(ret_val);
                        des_xml_doc_ptr(n_doc, doc, 0);
                        des_int(n_flags, flags, 1);
                        des_userdata(n_data, data, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlXIncludeProcessFlagsData",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlXIncludeProcessFlagsData()"
                            );
                            eprint!(" {}", n_doc);
                            eprint!(" {}", n_flags);
                            eprintln!(" {}", n_data);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xinclude_process_node() {
        #[cfg(feature = "xinclude")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XINCLUDE_CTXT_PTR {
                for n_node in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xinclude_ctxt_ptr(n_ctxt, 0);
                    let node = gen_xml_node_ptr(n_node, 1);

                    let ret_val = xml_xinclude_process_node(ctxt, node);
                    desret_int(ret_val);
                    des_xml_xinclude_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_xml_node_ptr(n_node, node, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXIncludeProcessNode",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXIncludeProcessNode()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_node);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xinclude_process_tree() {
        #[cfg(feature = "xinclude")]
        unsafe {
            let mut leaks = 0;

            for n_tree in 0..GEN_NB_XML_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let tree = gen_xml_node_ptr(n_tree, 0);

                let ret_val = xml_xinclude_process_tree(tree);
                desret_int(ret_val);
                des_xml_node_ptr(n_tree, tree, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXIncludeProcessTree",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXIncludeProcessTree()"
                    );
                    eprintln!(" {}", n_tree);
                }
            }
        }
    }

    #[test]
    fn test_xml_xinclude_process_tree_flags() {
        #[cfg(feature = "xinclude")]
        unsafe {
            let mut leaks = 0;

            for n_tree in 0..GEN_NB_XML_NODE_PTR {
                for n_flags in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let tree = gen_xml_node_ptr(n_tree, 0);
                    let flags = gen_int(n_flags, 1);

                    let ret_val = xml_xinclude_process_tree_flags(tree, flags);
                    desret_int(ret_val);
                    des_xml_node_ptr(n_tree, tree, 0);
                    des_int(n_flags, flags, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXIncludeProcessTreeFlags",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXIncludeProcessTreeFlags()"
                        );
                        eprint!(" {}", n_tree);
                        eprintln!(" {}", n_flags);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xinclude_process_tree_flags_data() {
        #[cfg(feature = "xinclude")]
        unsafe {
            let mut leaks = 0;

            for n_tree in 0..GEN_NB_XML_NODE_PTR {
                for n_flags in 0..GEN_NB_INT {
                    for n_data in 0..GEN_NB_USERDATA {
                        let mem_base = xml_mem_blocks();
                        let tree = gen_xml_node_ptr(n_tree, 0);
                        let flags = gen_int(n_flags, 1);
                        let data = gen_userdata(n_data, 2);

                        let ret_val = xml_xinclude_process_tree_flags_data(tree, flags, data);
                        desret_int(ret_val);
                        des_xml_node_ptr(n_tree, tree, 0);
                        des_int(n_flags, flags, 1);
                        des_userdata(n_data, data, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlXIncludeProcessTreeFlagsData",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlXIncludeProcessTreeFlagsData()"
                            );
                            eprint!(" {}", n_tree);
                            eprint!(" {}", n_flags);
                            eprintln!(" {}", n_data);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xinclude_set_flags() {
        #[cfg(feature = "xinclude")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XINCLUDE_CTXT_PTR {
                for n_flags in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xinclude_ctxt_ptr(n_ctxt, 0);
                    let flags = gen_int(n_flags, 1);

                    let ret_val = xml_xinclude_set_flags(ctxt, flags);
                    desret_int(ret_val);
                    des_xml_xinclude_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_int(n_flags, flags, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXIncludeSetFlags",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXIncludeSetFlags()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_flags);
                    }
                }
            }
        }
    }
}
