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
    mem::{size_of, take, zeroed},
    os::raw::c_void,
    ptr::{addr_of_mut, drop_in_place, null, null_mut},
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
        globals::{xml_free, xml_malloc},
        parser::{
            xml_ctxt_use_options, xml_init_parser, xml_load_external_entity, xml_parse_document,
            XmlParserOption, XML_DETECT_IDS,
        },
        parser_internals::xml_string_current_char,
        uri::{xml_free_uri, xml_parse_uri, xml_save_uri, XmlURIPtr},
        xmlstring::{xml_str_equal, xml_strcmp, xml_strdup, XmlChar},
        xpointer::{xml_xptr_eval, xml_xptr_new_context},
    },
    parser::{xml_free_input_stream, xml_free_parser_ctxt, xml_new_parser_ctxt, XmlParserInputPtr},
    tree::{
        xml_add_doc_entity, xml_create_int_subset, xml_doc_copy_node, xml_free_doc, xml_free_node,
        xml_free_node_list, xml_get_doc_entity, xml_new_doc_node, xml_new_doc_text,
        xml_static_copy_node, xml_static_copy_node_list, NodeCommon, NodePtr, XmlDocPtr,
        XmlElementType, XmlEntityPtr, XmlEntityType, XmlGenericNodePtr, XmlNode, XmlNodePtr,
        XML_XML_NAMESPACE,
    },
    uri::{build_relative_uri, build_uri, escape_url, XmlURI},
    xpath::{
        xml_xpath_free_context, xml_xpath_free_object, XmlXPathContextPtr, XmlXPathObjectPtr,
        XmlXPathObjectType,
    },
};

use super::{chvalid::xml_is_char, xmlstring::xml_strndup};

/// A constant defining the Xinclude namespace: `http://www.w3.org/2003/XInclude`
pub const XINCLUDE_NS: &CStr = c"http://www.w3.org/2003/XInclude";
/// A constant defining the draft Xinclude namespace: `http://www.w3.org/2001/XInclude`
pub const XINCLUDE_OLD_NS: &CStr = c"http://www.w3.org/2001/XInclude";
/// A constant defining "include"
pub const XINCLUDE_NODE: &str = "include";
/// A constant defining "fallback"
pub const XINCLUDE_FALLBACK: &str = "fallback";
/// A constant defining "href"
pub const XINCLUDE_HREF: &str = "href";
/// A constant defining "parse"
pub const XINCLUDE_PARSE: &str = "parse";
/// A constant defining "xml"
pub const XINCLUDE_PARSE_XML: &str = "xml";
/// A constant defining "text"
pub const XINCLUDE_PARSE_TEXT: &str = "text";
/// A constant defining "encoding"
pub const XINCLUDE_PARSE_ENCODING: &str = "encoding";
/// A constant defining "xpointer"
pub const XINCLUDE_PARSE_XPOINTER: &CStr = c"xpointer";

pub type XmlURL = *mut XmlChar;

pub type XmlXIncludeRefPtr = *mut XmlXIncludeRef;
#[repr(C)]
pub struct XmlXIncludeRef {
    uri: *mut XmlChar,        /* the fully resolved resource URL */
    fragment: *mut XmlChar,   /* the fragment in the URI */
    elem: Option<XmlNodePtr>, /* the xi:include element */
    inc: Option<XmlNodePtr>,  /* the included copy */
    xml: i32,                 /* xml or txt */
    fallback: i32,            /* fallback was loaded */
    empty_fb: i32,            /* flag to show fallback empty */
    expanding: i32,           /* flag to detect inclusion loops */
    replace: i32,             /* should the node be replaced? */
}

pub type XmlXIncludeDocPtr = *mut XmlXIncludeDoc;
#[repr(C)]
pub struct XmlXIncludeDoc {
    doc: Option<XmlDocPtr>, /* the parsed document */
    url: *mut XmlChar,      /* the URL */
    expanding: i32,         /* flag to detect inclusion loops */
}

pub type XmlXIncludeTxtPtr = *mut XmlXIncludeTxt;
#[repr(C)]
pub struct XmlXIncludeTxt {
    text: *mut XmlChar, /* text string */
    url: *mut XmlChar,  /* the URL */
}

pub type XmlXIncludeCtxtPtr = *mut XmlXIncludeCtxt;
/// An XInclude context
#[repr(C)]
pub struct XmlXIncludeCtxt {
    doc: Option<XmlDocPtr>, /* the source document */
    // inc_nr: i32,                     /* number of includes */
    // inc_max: i32,                    /* size of includes tab */
    inc_tab: Vec<XmlXIncludeRefPtr>, /* array of included references */

    // txt_nr: i32,                  /* number of unparsed documents */
    // txt_max: i32,                 /* size of unparsed documents tab */
    txt_tab: Vec<XmlXIncludeTxt>, /* array of unparsed documents */

    // url_nr: i32,                  /* number of documents stacked */
    // url_max: i32,                 /* size of document stack */
    url_tab: Vec<XmlXIncludeDoc>, /* document stack */

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

impl Default for XmlXIncludeCtxt {
    fn default() -> Self {
        Self {
            doc: None,
            inc_tab: vec![],
            txt_tab: vec![],
            url_tab: vec![],
            nb_errors: 0,
            fatal_err: 0,
            legacy: 0,
            parse_flags: 0,
            base: null_mut(),
            _private: null_mut(),
            depth: 0,
            is_stream: 0,
        }
    }
}

/// Implement the XInclude substitution on the XML document @doc
///
/// Returns 0 if no substitution were done, -1 if some processing failed
/// or the number of substitutions done.
#[doc(alias = "xmlXIncludeProcess")]
pub unsafe fn xml_xinclude_process(doc: XmlDocPtr) -> i32 {
    xml_xinclude_process_flags(doc, 0)
}

/// Implement the XInclude substitution on the XML document @doc
///
/// Returns 0 if no substitution were done, -1 if some processing failed
/// or the number of substitutions done.
#[doc(alias = "xmlXIncludeProcessFlags")]
pub unsafe fn xml_xinclude_process_flags(doc: XmlDocPtr, flags: i32) -> i32 {
    xml_xinclude_process_flags_data(doc, flags, null_mut())
}

/// Implement the XInclude substitution on the XML document @doc
///
/// Returns 0 if no substitution were done, -1 if some processing failed
/// or the number of substitutions done.
#[doc(alias = "xmlXIncludeProcessFlagsData")]
pub unsafe fn xml_xinclude_process_flags_data(
    doc: XmlDocPtr,
    flags: i32,
    data: *mut c_void,
) -> i32 {
    let Some(tree) = doc.get_root_element() else {
        return -1;
    };
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
        let ctxt = $ctxt as *mut XmlXIncludeCtxt;
        if !ctxt.is_null() {
            (*ctxt).nb_errors += 1;
        }
        __xml_raise_error!(
            None,
            None,
            None,
            ctxt as _,
            $node,
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
unsafe fn xml_xinclude_test_node(ctxt: XmlXIncludeCtxtPtr, node: XmlNodePtr) -> i32 {
    if node.element_type() != XmlElementType::XmlElementNode {
        return 0;
    }
    let Some(node_ns) = node.ns else {
        return 0;
    };
    if node_ns.href().as_deref() == Some(XINCLUDE_NS.to_str().unwrap())
        || node_ns.href().as_deref() == Some(XINCLUDE_OLD_NS.to_str().unwrap())
    {
        if node_ns.href().as_deref() == Some(XINCLUDE_OLD_NS.to_str().unwrap())
            && (*ctxt).legacy == 0
        {
            (*ctxt).legacy = 1;
        }
        if node.name().as_deref() == Some(XINCLUDE_NODE) {
            let mut child = node
                .children
                .and_then(|c| XmlNodePtr::from_raw(c.as_ptr()).unwrap());
            let mut nb_fallback: i32 = 0;

            while let Some(cur_node) = child {
                if cur_node.element_type() == XmlElementType::XmlElementNode
                    && cur_node.ns.map_or(false, |ns| {
                        ns.href().as_deref() == Some(XINCLUDE_NS.to_str().unwrap())
                            || ns.href().as_deref() == Some(XINCLUDE_OLD_NS.to_str().unwrap())
                    })
                {
                    if cur_node.name().as_deref() == Some(XINCLUDE_NODE) {
                        xml_xinclude_err!(
                            ctxt,
                            Some(node.into()),
                            XmlParserErrors::XmlXIncludeIncludeInInclude,
                            "{} has an 'include' child\n",
                            XINCLUDE_NODE
                        );
                        return 0;
                    }
                    if cur_node.name().as_deref() == Some(XINCLUDE_FALLBACK) {
                        nb_fallback += 1;
                    }
                }
                child = cur_node
                    .next
                    .and_then(|n| XmlNodePtr::from_raw(n.as_ptr()).unwrap());
            }
            if nb_fallback > 1 {
                xml_xinclude_err!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlXIncludeFallbacksInInclude,
                    "{} has multiple fallback children\n",
                    XINCLUDE_NODE
                );
                return 0;
            }
            return 1;
        }
        if node.name().as_deref() == Some(XINCLUDE_FALLBACK)
            && (node.parent().is_none()
                || node.parent().unwrap().element_type() != XmlElementType::XmlElementNode
                || node.parent.unwrap().ns.map_or(true, |ns| {
                    ns.href().as_deref() != Some(XINCLUDE_NS.to_str().unwrap())
                        && ns.href().as_deref() != Some(XINCLUDE_OLD_NS.to_str().unwrap())
                })
                || node.parent().unwrap().name().as_deref() != Some(XINCLUDE_NODE))
        {
            xml_xinclude_err!(
                ctxt,
                Some(node.into()),
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
    ctxt: XmlXIncludeCtxtPtr,
    cur: XmlNodePtr,
    name: &str,
) -> Option<String> {
    if let Some(ret) = cur.get_ns_prop(XINCLUDE_NS.to_string_lossy().as_ref(), Some(name)) {
        return Some(ret);
    }
    if (*ctxt).legacy != 0 {
        if let Some(ret) = cur.get_ns_prop(XINCLUDE_OLD_NS.to_string_lossy().as_ref(), Some(name)) {
            return Some(ret);
        }
    }
    cur.get_prop(name)
}

/// Handle an out of memory condition
#[doc(alias = "xmlXIncludeErrMemory")]
unsafe fn xml_xinclude_err_memory(
    ctxt: XmlXIncludeCtxtPtr,
    node: Option<XmlGenericNodePtr>,
    extra: Option<&str>,
) {
    if !ctxt.is_null() {
        (*ctxt).nb_errors += 1;
    }
    if let Some(extra) = extra {
        __xml_raise_error!(
            None,
            None,
            None,
            ctxt as _,
            node,
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
            node,
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
unsafe fn xml_xinclude_free_ref(refe: XmlXIncludeRefPtr) {
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
unsafe fn xml_xinclude_new_ref(
    ctxt: XmlXIncludeCtxtPtr,
    uri: *const XmlChar,
    elem: XmlNodePtr,
) -> XmlXIncludeRefPtr {
    let ret: XmlXIncludeRefPtr = xml_malloc(size_of::<XmlXIncludeRef>()) as XmlXIncludeRefPtr;
    if ret.is_null() {
        xml_xinclude_err_memory(ctxt, Some(elem.into()), Some("growing XInclude context"));
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlXIncludeRef>());
    if uri.is_null() {
        (*ret).uri = null_mut();
    } else {
        (*ret).uri = xml_strdup(uri);
    }
    (*ret).fragment = null_mut();
    (*ret).elem = Some(elem);
    (*ret).xml = 0;
    (*ret).inc = None;
    (*ctxt).inc_tab.push(ret);
    ret
}

/// Add a new node to process to an XInclude context
#[doc(alias = "xmlXIncludeAddNode")]
unsafe fn xml_xinclude_add_node(ctxt: XmlXIncludeCtxtPtr, cur: XmlNodePtr) -> XmlXIncludeRefPtr {
    let mut xml: i32 = 1;
    let mut local: i32 = 0;

    if ctxt.is_null() {
        return null_mut();
    }

    // read the attributes
    let href = xml_xinclude_get_prop(ctxt, cur, XINCLUDE_HREF).unwrap_or("".to_owned());
    let parse = xml_xinclude_get_prop(ctxt, cur, XINCLUDE_PARSE);
    if let Some(parse) = parse {
        if parse == XINCLUDE_PARSE_XML {
            xml = 1;
        } else if parse == XINCLUDE_PARSE_TEXT {
            xml = 0;
        } else {
            xml_xinclude_err!(
                ctxt,
                Some(cur.into()),
                XmlParserErrors::XmlXIncludeParseValue,
                "invalid value {} for 'parse'\n",
                parse
            );
            return null_mut();
        }
    }

    // compute the URI
    let mut base = None;
    let mut uri = (*ctxt).doc.and_then(|doc| {
        if let Some(b) = (*cur).get_base(Some(doc)) {
            base = Some(b);
            build_uri(&href, base.as_deref().unwrap())
        } else {
            doc.url.as_deref().and_then(|base| build_uri(&href, base))
        }
    });
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
            Some(cur.into()),
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
            Some(cur.into()),
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
                Some(cur.into()),
                XmlParserErrors::XmlXIncludeFragmentID,
                "Invalid fragment identifier in URI {} use the xpointer attribute\n",
                uri
            );
            return null_mut();
        }
        parsed_uri.fragment = None;
    }
    let url = parsed_uri.save();

    if (*ctxt).doc.as_deref().and_then(|doc| doc.url.as_deref()) == Some(url.as_str()) {
        local = 1;
    }

    // If local and xml then we need a fragment
    if local == 1
        && xml == 1
        && (fragment.is_none() || fragment.as_deref().map_or(true, |f| f.is_empty()))
    {
        xml_xinclude_err!(
            ctxt,
            Some(cur.into()),
            XmlParserErrors::XmlXIncludeRecursion,
            "detected a local recursion with no xpointer in {}\n",
            url
        );
        return null_mut();
    }
    let url = CString::new(url).unwrap();

    let refe: XmlXIncludeRefPtr = xml_xinclude_new_ref(ctxt, url.as_ptr() as *const u8, cur);
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
unsafe fn xml_xinclude_parse_file(ctxt: XmlXIncludeCtxtPtr, mut url: &str) -> Option<XmlDocPtr> {
    xml_init_parser();

    let pctxt = xml_new_parser_ctxt();
    if pctxt.is_null() {
        xml_xinclude_err_memory(ctxt, None, Some("cannot allocate parser context"));
        return None;
    }

    // pass in the application data to the parser context.
    (*pctxt)._private = (*ctxt)._private;

    // try to ensure that new documents included are actually
    // built with the same dictionary as the including document.
    // if !(*ctxt).doc.is_null() && !(*(*ctxt).doc).dict.is_null() {
    //     if !(*pctxt).dict.is_null() {
    //         xml_dict_free((*pctxt).dict);
    //     }
    //     (*pctxt).dict = (*(*ctxt).doc).dict;
    //     xml_dict_reference((*pctxt).dict);
    // }

    xml_ctxt_use_options(
        pctxt,
        (*ctxt).parse_flags | XmlParserOption::XmlParseDTDLoad as i32,
    );

    // Don't read from stdin.
    if url == "-" {
        url = "./-";
    }

    let input_stream: XmlParserInputPtr = xml_load_external_entity(Some(url), None, pctxt);
    if input_stream.is_null() {
        xml_free_parser_ctxt(pctxt);
        return None;
    }

    (*pctxt).input_push(input_stream);

    if (*pctxt).directory.is_none() {
        if let Some(dir) = xml_parser_get_directory(url) {
            (*pctxt).directory = Some(dir.to_string_lossy().into_owned());
        }
    }

    (*pctxt).loadsubset |= XML_DETECT_IDS as i32;

    xml_parse_document(pctxt);

    let ret = if (*pctxt).well_formed != 0 {
        (*pctxt).my_doc
    } else {
        if let Some(my_doc) = (*pctxt).my_doc.take() {
            xml_free_doc(my_doc);
        }
        None
    };
    xml_free_parser_ctxt(pctxt);
    ret
}

pub type XmlXIncludeMergeDataPtr = *mut XmlXIncludeMergeData;
pub struct XmlXIncludeMergeData {
    doc: Option<XmlDocPtr>,
    ctxt: XmlXIncludeCtxtPtr,
}

/// Implements the merge of one entity
#[doc(alias = "xmlXIncludeMergeOneEntity")]
unsafe fn xml_xinclude_merge_entity(ent: XmlEntityPtr, vdata: *mut c_void) {
    let data: XmlXIncludeMergeDataPtr = vdata as XmlXIncludeMergeDataPtr;

    if data.is_null() {
        return;
    }
    let ctxt: XmlXIncludeCtxtPtr = (*data).ctxt;
    if ctxt.is_null() {
        return;
    }
    let Some(doc) = (*data).doc else {
        return;
    };
    match ent.etype {
        XmlEntityType::XmlInternalParameterEntity
        | XmlEntityType::XmlExternalParameterEntity
        | XmlEntityType::XmlInternalPredefinedEntity => return,
        XmlEntityType::XmlInternalGeneralEntity
        | XmlEntityType::XmlExternalGeneralParsedEntity
        | XmlEntityType::XmlExternalGeneralUnparsedEntity => {}
        _ => unreachable!(),
    }
    let external_id = ent.external_id.load(Ordering::Relaxed);
    let system_id = ent.system_id.load(Ordering::Relaxed);
    let content = ent.content.load(Ordering::Relaxed);
    let ret = xml_add_doc_entity(
        doc,
        &ent.name().unwrap(),
        ent.etype,
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
    if let Some(ret) = ret {
        if !ent.uri.load(Ordering::Relaxed).is_null() {
            ret.uri.store(
                xml_strdup(ent.uri.load(Ordering::Relaxed)),
                Ordering::Relaxed,
            );
        }
    } else {
        let prev = xml_get_doc_entity(Some(doc), &ent.name().unwrap());
        if let Some(prev) = prev {
            let error = || {
                match ent.etype {
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
                    Some(ent.into()),
                    XmlParserErrors::XmlXIncludeEntityDefMismatch,
                    "mismatch in redefinition of entity {}\n",
                    (*ent).name().unwrap().into_owned()
                );
            };

            if ent.etype != prev.etype {
                // goto error;
                return error();
            }

            if !ent.system_id.load(Ordering::Relaxed).is_null()
                && !prev.system_id.load(Ordering::Relaxed).is_null()
            {
                if !xml_str_equal(
                    ent.system_id.load(Ordering::Relaxed),
                    prev.system_id.load(Ordering::Relaxed),
                ) {
                    // goto error;
                    error()
                }
            } else if !ent.external_id.load(Ordering::Relaxed).is_null()
                && !prev.external_id.load(Ordering::Relaxed).is_null()
            {
                if !xml_str_equal(
                    ent.external_id.load(Ordering::Relaxed),
                    prev.external_id.load(Ordering::Relaxed),
                ) {
                    // goto error;
                    return error();
                }
            } else if !ent.content.load(Ordering::Relaxed).is_null()
                && !prev.content.load(Ordering::Relaxed).is_null()
            {
                if !xml_str_equal(
                    ent.content.load(Ordering::Relaxed),
                    prev.content.load(Ordering::Relaxed),
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
unsafe fn xml_xinclude_merge_entities(
    ctxt: XmlXIncludeCtxtPtr,
    doc: XmlDocPtr,
    from: XmlDocPtr,
) -> i32 {
    if ctxt.is_null() {
        return -1;
    }

    if from.int_subset.is_none() {
        return 0;
    }

    let Some(target) = doc.int_subset.or_else(|| {
        let cur = doc.get_root_element()?;
        xml_create_int_subset(Some(doc), cur.name().as_deref(), None, None)
    }) else {
        return -1;
    };

    let source = from.int_subset;
    if let Some(source) = source {
        if let Some(entities) = source.entities {
            let mut data: XmlXIncludeMergeData = unsafe { zeroed() };
            data.ctxt = ctxt;
            data.doc = Some(doc);

            entities.scan(|payload, _, _, _| {
                xml_xinclude_merge_entity(*payload, &raw mut data as _);
            });
        }
    }
    let source = from.ext_subset;
    if let Some(source) = source {
        if let Some(entities) = source.entities {
            let mut data: XmlXIncludeMergeData = unsafe { zeroed() };
            data.ctxt = ctxt;
            data.doc = Some(doc);

            // don't duplicate existing stuff when external subsets are the same
            if target.external_id != source.external_id && target.system_id != source.system_id {
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
unsafe fn xml_xinclude_recurse_doc(ctxt: XmlXIncludeCtxtPtr, doc: XmlDocPtr, _url: XmlURL) {
    let old_doc = (*ctxt).doc;
    let old_inc_tab = take(&mut (*ctxt).inc_tab);
    let old_is_stream: i32 = (*ctxt).is_stream;
    (*ctxt).doc = Some(doc);
    (*ctxt).is_stream = 0;

    xml_xinclude_do_process(ctxt, doc.get_root_element().unwrap());

    for &inc in &(*ctxt).inc_tab {
        xml_xinclude_free_ref(inc);
    }

    (*ctxt).doc = old_doc;
    (*ctxt).inc_tab = old_inc_tab;
    (*ctxt).is_stream = old_is_stream;
}

/// Make a copy of the node while expanding nested XIncludes.
///
/// Returns a node list, not a single node.
#[doc(alias = "xmlXIncludeCopyNode")]
unsafe fn xml_xinclude_copy_node(
    ctxt: XmlXIncludeCtxtPtr,
    elem: XmlNodePtr,
    copy_children: i32,
) -> Option<XmlNodePtr> {
    let mut result: Option<XmlNodePtr> = None;
    let mut insert_parent: Option<XmlNodePtr> = None;
    let mut insert_last: Option<XmlNodePtr> = None;

    let mut cur = if copy_children != 0 {
        elem.children
            .and_then(|c| XmlNodePtr::from_raw(c.as_ptr()).unwrap())?
    } else {
        elem
    };

    loop {
        let mut copy = None;
        let mut recurse: i32 = 0;

        if matches!(
            cur.element_type(),
            XmlElementType::XmlDocumentNode | XmlElementType::XmlDTDNode
        ) {
        } else if cur.element_type() == XmlElementType::XmlElementNode
            && cur.name().as_deref() == Some(XINCLUDE_NODE)
            && cur.ns.map_or(false, |ns| {
                ns.href().as_deref() == Some(XINCLUDE_NS.to_str().unwrap())
                    || ns.href().as_deref() == Some(XINCLUDE_OLD_NS.to_str().unwrap())
            })
        {
            let refe: XmlXIncludeRefPtr = xml_xinclude_expand_node(ctxt, cur);

            if refe.is_null() {
                // goto error;
                xml_free_node_list(result);
                return None;
            }
            // TODO: Insert xmlElementType::XML_XINCLUDE_START and xmlElementType::XML_XINCLUDE_END nodes
            if let Some(inc) = (*refe).inc {
                let Some(res) = xml_static_copy_node_list(
                    Some(XmlGenericNodePtr::from(inc)),
                    (*ctxt).doc,
                    insert_parent.map(|parent| parent.into()),
                ) else {
                    // goto error;
                    xml_free_node_list(result);
                    return None;
                };
                copy = Some(XmlNodePtr::try_from(res).unwrap());
            }
        } else {
            let Some(res) = xml_static_copy_node(
                XmlGenericNodePtr::from(cur),
                (*ctxt).doc,
                insert_parent.map(|parent| parent.into()),
                2,
            ) else {
                // goto error;
                xml_free_node_list(result);
                return None;
            };
            copy = Some(XmlNodePtr::try_from(res).unwrap());

            recurse = (cur.element_type() != XmlElementType::XmlEntityRefNode
                && cur.children().is_some()) as i32;
        }

        if let Some(mut copy) = copy {
            if result.is_none() {
                result = Some(copy);
            }
            if let Some(mut insert_last) = insert_last {
                insert_last.next = NodePtr::from_ptr(copy.as_ptr());
                copy.prev = NodePtr::from_ptr(insert_last.as_ptr());
            } else if let Some(mut insert_parent) = insert_parent {
                insert_parent.children = NodePtr::from_ptr(copy.as_ptr());
            }
            let mut now = copy;
            while let Some(next) = now
                .next
                .and_then(|next| XmlNodePtr::from_raw(next.as_ptr()).unwrap())
            {
                now = next;
            }
            insert_last = Some(now);
        }

        if recurse != 0 {
            cur = cur
                .children
                .and_then(|c| XmlNodePtr::from_raw(c.as_ptr()).unwrap())
                .unwrap();
            insert_parent = insert_last.take();
            continue;
        }

        if cur == elem {
            return result;
        }

        while cur.next.is_none() {
            if let Some(mut insert_parent) = insert_parent {
                insert_parent.last =
                    NodePtr::from_ptr(insert_last.map_or(null_mut(), |node| node.as_ptr()));
            }
            cur = cur
                .parent
                .and_then(|p| XmlNodePtr::from_raw(p.as_ptr()).unwrap())
                .unwrap();
            if cur == elem {
                return result;
            }
            insert_last = insert_parent;
            insert_parent = insert_parent
                .unwrap()
                .parent
                .and_then(|p| XmlNodePtr::from_raw(p.as_ptr()).unwrap());
        }

        cur = cur
            .next
            .and_then(|n| XmlNodePtr::from_raw(n.as_ptr()).unwrap())
            .unwrap();
    }

    // error:
    // xmlFreeNodeList(result);
    // return null_mut();
}

/// Returns the @n'th element child of @cur or NULL
#[doc(alias = "xmlXIncludeGetNthChild")]
#[cfg(feature = "libxml_xptr_locs")]
unsafe fn xml_xinclude_get_nth_child(cur: XmlGenericNodePtr, no: i32) -> Option<XmlGenericNodePtr> {
    if cur.element_type() == XmlElementType::XmlNamespaceDecl {
        return None;
    }
    let mut cur = cur
        .children()
        .and_then(|children| XmlGenericNodePtr::from_raw(children.as_ptr()));
    let mut i = 0;
    while i <= no {
        let now = cur?;
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

        cur = now
            .next()
            .and_then(|next| XmlGenericNodePtr::from_raw(next.as_ptr()));
    }
    cur
}

/// Build a node list tree copy of the XPointer result.
///
/// Returns an xmlNodePtr list or NULL.
/// The caller has to free the node tree.
#[doc(alias = "xmlXIncludeCopyRange")]
#[cfg(feature = "libxml_xptr_locs")]
unsafe fn xml_xinclude_copy_range(
    ctxt: XmlXIncludeCtxtPtr,
    range: XmlXPathObjectPtr,
) -> Option<XmlGenericNodePtr> {
    use crate::{
        libxml::xpointer::xml_xptr_advance_node,
        tree::{xml_new_doc_text, xml_new_doc_text_len},
    };

    /* pointers to generated nodes */
    let mut list = None;
    let mut last = None;
    let mut list_parent = None;
    let mut level: i32 = 0;
    let mut last_level: i32 = 0;
    let mut end_level: i32 = 0;
    let mut end_flag: i32 = 0;

    if ctxt.is_null() || range.is_null() {
        return None;
    }
    if (*range).typ != XmlXPathObjectType::XPathRange {
        return None;
    }
    let start = XmlGenericNodePtr::from_raw((*range).user as *mut XmlNode)
        .filter(|node| node.element_type() != XmlElementType::XmlNamespaceDecl)?;

    let Some(mut end) = XmlGenericNodePtr::from_raw((*range).user2 as *mut XmlNode) else {
        return xml_doc_copy_node(start, (*ctxt).doc, 1);
    };
    if end.element_type() == XmlElementType::XmlNamespaceDecl {
        return None;
    }

    let mut cur = Some(start);
    let mut index1 = (*range).index;
    let mut index2 = (*range).index2;
    // level is depth of the current node under consideration
    // list is the pointer to the root of the output tree
    // listParent is a pointer to the parent of output tree (within
    // the included file) in case we need to add another level
    // last is a pointer to the last node added to the output tree
    // lastLevel is the depth of last (relative to the root)
    while let Some(cur_node) = cur {
        // Check if our output tree needs a parent
        if level < 0 {
            while level < 0 {
                // copy must include namespaces and properties
                let mut tmp2 = xml_doc_copy_node(list_parent.unwrap(), (*ctxt).doc, 2).unwrap();
                tmp2.add_child(list.unwrap());
                list = Some(tmp2);
                list_parent = list_parent
                    .unwrap()
                    .parent()
                    .and_then(|n| XmlGenericNodePtr::from_raw(n.as_ptr()));
                level += 1;
            }
            last = list;
            last_level = 0;
        }
        // Check whether we need to change our insertion point
        while level < last_level {
            last = last
                .unwrap()
                .parent()
                .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()));
            last_level -= 1;
        }
        if cur_node == end {
            // Are we at the end of the range?
            if cur_node.element_type() == XmlElementType::XmlTextNode {
                let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                let mut content: *const XmlChar = cur_node.content;
                let mut len: i32;

                let tmp = if content.is_null() {
                    xml_new_doc_text_len((*ctxt).doc, null_mut(), 0)
                } else {
                    len = index2;
                    if start == cur_node.into() && index1 > 1 {
                        content = content.add(index1 as usize - 1);
                        len -= index1 - 1;
                    } else {
                        len = index2;
                    }
                    xml_new_doc_text_len((*ctxt).doc, content, len)
                };
                // single sub text node selection
                if list.is_none() {
                    return tmp.map(|node| node.into());
                }
                // prune and return full set
                if level == last_level {
                    last.unwrap()
                        .add_next_sibling(tmp.unwrap().into())
                        .map_or(null_mut(), |node| node.as_ptr());
                } else {
                    last.unwrap().add_child(tmp.unwrap().into());
                }
                return list;
            } else {
                // ending node not a text node
                end_level = level; /* remember the level of the end node */
                end_flag = 1;
                // last node - need to take care of properties + namespaces
                let tmp = xml_doc_copy_node(cur_node, (*ctxt).doc, 2);
                if list.is_none() {
                    list = tmp;
                    list_parent = cur_node
                        .parent()
                        .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()));
                    last = tmp;
                } else if level == last_level {
                    last = last.unwrap().add_next_sibling(tmp.unwrap());
                } else {
                    last = last.unwrap().add_child(tmp.unwrap());
                    last_level = level;
                }

                if index2 > 1 {
                    end = xml_xinclude_get_nth_child(cur_node, index2 - 1).unwrap();
                    index2 = 0;
                }
                if cur_node == start && index1 > 1 {
                    cur = xml_xinclude_get_nth_child(cur_node, index1 - 1);
                    index1 = 0;
                } else {
                    cur = cur_node
                        .children()
                        .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()));
                }
                // increment level to show change
                level += 1;
                // Now gather the remaining nodes from cur to end
                continue; /* while */
            }
        } else if cur_node == start {
            // Not at the end, are we at start?
            if matches!(
                cur_node.element_type(),
                XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
            ) {
                let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                let mut content: *const XmlChar = cur_node.content;

                let tmp = if content.is_null() {
                    xml_new_doc_text_len((*ctxt).doc, null_mut(), 0)
                } else {
                    if index1 > 1 {
                        content = content.add(index1 as usize - 1);
                        index1 = 0;
                    }
                    xml_new_doc_text((*ctxt).doc, content)
                };
                last = tmp.map(|node| node.into());
                list = tmp.map(|node| node.into());
                list_parent = cur_node
                    .parent()
                    .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()));
            } else {
                // Not text node

                // start of the range - need to take care of
                // properties and namespaces
                let tmp = xml_doc_copy_node(cur_node, (*ctxt).doc, 2);
                list = tmp;
                last = tmp;
                list_parent = cur_node
                    .parent()
                    .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()));
                if index1 > 1 {
                    // Do we need to position?
                    cur = xml_xinclude_get_nth_child(cur_node, index1 - 1);
                    level = 1;
                    last_level = 1;
                    index1 = 0;
                    // Now gather the remaining nodes from cur to end
                    continue; /* while */
                }
            }
        } else {
            let mut tmp = None;
            match cur_node.element_type() {
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
                    tmp = xml_doc_copy_node(cur_node, (*ctxt).doc, 2);
                }
            }
            if let Some(tmp) = tmp {
                if level == last_level {
                    last = last.unwrap().add_next_sibling(tmp);
                } else {
                    last = last.unwrap().add_child(tmp);
                    last_level = level;
                }
            }
        }
        // Skip to next node in document order
        cur = xml_xptr_advance_node(cur_node, addr_of_mut!(level));
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
unsafe fn xml_xinclude_copy_xpointer(
    ctxt: XmlXIncludeCtxtPtr,
    obj: XmlXPathObjectPtr,
) -> Option<XmlNodePtr> {
    let mut list: Option<XmlNodePtr> = None;

    if ctxt.is_null() || obj.is_null() {
        return None;
    }
    match (*obj).typ {
        XmlXPathObjectType::XPathNodeset => {
            let set = (*obj).nodesetval.as_deref()?;
            let mut last: Option<XmlNodePtr> = None;
            for &now in &set.node_tab {
                let node = match now.element_type() {
                    XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                        let Some(node) = XmlDocPtr::try_from(now).unwrap().get_root_element()
                        else {
                            xml_xinclude_err!(
                                ctxt,
                                Some(now),
                                XmlParserErrors::XmlErrInternalError,
                                "document without root\n"
                            );
                            continue;
                        };
                        node
                    }
                    XmlElementType::XmlTextNode
                    | XmlElementType::XmlCDATASectionNode
                    | XmlElementType::XmlElementNode
                    | XmlElementType::XmlPINode
                    | XmlElementType::XmlCommentNode => XmlNodePtr::try_from(now).unwrap(),
                    _ => {
                        xml_xinclude_err!(
                            ctxt,
                            Some(now),
                            XmlParserErrors::XmlXIncludeXPtrResult,
                            "invalid node type in XPtr result\n"
                        );
                        continue;
                    }
                };
                // OPTIMIZE TODO: External documents should already be
                // expanded, so xmlDocCopyNode should work as well.
                // xmlXIncludeCopyNode is only required for the initial document.
                let Some(mut copy) = xml_xinclude_copy_node(ctxt, node, 0) else {
                    xml_free_node_list(list);
                    return None;
                };
                if let Some(mut last) = last {
                    while let Some(next) = last
                        .next
                        .and_then(|next| XmlNodePtr::from_raw(next.as_ptr()).unwrap())
                    {
                        last = next;
                    }
                    copy.prev = NodePtr::from_ptr(last.as_ptr());
                    last.next = NodePtr::from_ptr(copy.as_ptr());
                } else {
                    list = Some(copy);
                }
                last = Some(copy);
            }
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XPathLocationset => {
            let set: XmlLocationSetPtr = (*obj).user as XmlLocationSetPtr;
            if set.is_null() {
                return None;
            }
            let mut last: Option<XmlNodePtr> = None;
            for &loc in &(*set).loc_tab {
                if let Some(mut last) = last {
                    last.add_next_sibling(xml_xinclude_copy_xpointer(ctxt, loc).unwrap().into());
                } else {
                    list = xml_xinclude_copy_xpointer(ctxt, loc);
                    last = list;
                }
                if let Some(mut l) = last {
                    while let Some(next) = l
                        .next
                        .and_then(|next| XmlNodePtr::from_raw(next.as_ptr()).unwrap())
                    {
                        l = next;
                    }
                    last = Some(l);
                }
            }
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XPathRange => {
            return xml_xinclude_copy_range(ctxt, obj)
                .map(|node| XmlNodePtr::try_from(node).unwrap());
        }
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
    ctxt: XmlXIncludeCtxtPtr,
    url: *const XmlChar,
    refe: XmlXIncludeRefPtr,
) -> i32 {
    let mut cache: XmlXIncludeDocPtr;
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
            (*refe).elem.map(|node| node.into()),
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
                (*refe).elem.map(|node| node.into()),
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
        let doc = 'load: {
            if *url.add(0) == 0
                || *url.add(0) == b'#'
                || (*ctxt).doc.map_or(false, |doc| {
                    doc.url.as_deref() == CStr::from_ptr(url as *const i8).to_str().ok()
                })
            {
                break 'load (*ctxt).doc;
            }
            // Prevent reloading the document twice.
            for inc_doc in &(*ctxt).url_tab {
                if xml_str_equal(url, inc_doc.url) {
                    if inc_doc.expanding != 0 {
                        xml_xinclude_err!(
                            ctxt,
                            (*refe).elem.map(|node| node.into()),
                            XmlParserErrors::XmlXIncludeRecursion,
                            "inclusion loop detected\n"
                        );
                        break 'error;
                    }
                    let Some(doc) = inc_doc.doc else {
                        break 'error;
                    };
                    break 'load Some(doc);
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
                    (*ctxt).parse_flags |= XmlParserOption::XmlParseNoEnt as i32;
                }
            }

            let doc = xml_xinclude_parse_file(
                ctxt,
                CStr::from_ptr(url as *const i8).to_string_lossy().as_ref(),
            );
            #[cfg(feature = "xpointer")]
            {
                (*ctxt).parse_flags = save_flags;
            }

            // Also cache NULL docs
            cache_nr = (*ctxt).url_tab.len() as i32;
            (*ctxt).url_tab.push(XmlXIncludeDoc {
                doc,
                url: xml_strdup(url),
                expanding: 0,
            });
            cache = &raw mut (*ctxt).url_tab[cache_nr as usize];

            let Some(doc) = doc else {
                break 'error;
            };
            // It's possible that the requested URL has been mapped to a
            // completely different location (e.g. through a catalog entry).
            // To check for this, we compare the URL with that of the doc
            // and change it if they disagree (bug 146988).
            if doc.url.as_deref() != CStr::from_ptr(url as *const i8).to_str().ok() {
                xml_free(url as _);
                let new = CString::new(doc.url.as_deref().unwrap()).unwrap();
                url = xml_strdup(new.as_ptr() as *const u8);
            }

            // Make sure we have all entities fixed up
            xml_xinclude_merge_entities(ctxt, (*ctxt).doc.unwrap(), doc);

            // We don't need the DTD anymore, free up space
            // if ((*doc).intSubset != null_mut()) {
            //     xmlUnlinkNode((xmlNodePtr) (*doc).intSubset);
            //     xmlFreeNode((xmlNodePtr) (*doc).intSubset);
            //     (*doc).intSubset = NULL;
            // }
            // if ((*doc).extSubset != null_mut()) {
            //     xmlUnlinkNode((xmlNodePtr) (*doc).extSubset);
            //     xmlFreeNode((xmlNodePtr) (*doc).extSubset);
            //     (*doc).extSubset = NULL;
            // }
            (*cache).expanding = 1;
            xml_xinclude_recurse_doc(ctxt, doc, url);
            // urlTab might be reallocated.
            cache = &raw mut (*ctxt).url_tab[cache_nr as usize];
            (*cache).expanding = 0;
            Some(doc)
        };

        // loaded:
        if fragment.is_null() {
            // Add the top children list as the replacement copy.
            (*refe).inc = doc
                .and_then(|doc| {
                    xml_doc_copy_node(doc.get_root_element().unwrap().into(), (*ctxt).doc, 1)
                })
                .and_then(|node| XmlNodePtr::try_from(node).ok());
        } else {
            #[cfg(feature = "xpointer")]
            {
                // Computes the XPointer expression and make a copy used
                // as the replacement copy.

                if (*ctxt).is_stream != 0 && doc == (*ctxt).doc {
                    xml_xinclude_err!(
                        ctxt,
                        (*refe).elem.map(|node| node.into()),
                        XmlParserErrors::XmlXIncludeXPtrFailed,
                        "XPointer expressions not allowed in streaming mode\n"
                    );
                    break 'error;
                }

                let xptrctxt: XmlXPathContextPtr = xml_xptr_new_context(doc, None, None);
                if xptrctxt.is_null() {
                    xml_xinclude_err!(
                        ctxt,
                        (*refe).elem.map(|node| node.into()),
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
                        (*refe).elem.map(|node| node.into()),
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
                            (*refe).elem.map(|node| node.into()),
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
                            (*refe).elem.map(|node| node.into()),
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
                    let mut i = 0;
                    while i < set.node_tab.len() {
                        let node = set.node_tab[i];
                        match node.element_type() {
                            XmlElementType::XmlElementNode
                            | XmlElementType::XmlTextNode
                            | XmlElementType::XmlCDATASectionNode
                            | XmlElementType::XmlEntityRefNode
                            | XmlElementType::XmlEntityNode
                            | XmlElementType::XmlPINode
                            | XmlElementType::XmlCommentNode
                            | XmlElementType::XmlDocumentNode
                            | XmlElementType::XmlHTMLDocumentNode => {
                                // continue to next loop
                            }

                            XmlElementType::XmlAttributeNode => {
                                let fragment =
                                    CStr::from_ptr(fragment as *const i8).to_string_lossy();
                                xml_xinclude_err!(
                                    ctxt,
                                    (*refe).elem.map(|node| node.into()),
                                    XmlParserErrors::XmlXIncludeXPtrResult,
                                    "XPointer selects an attribute: #{}\n",
                                    fragment
                                );
                                set.node_tab.swap_remove(i);
                                continue;
                            }
                            XmlElementType::XmlNamespaceDecl => {
                                let fragment =
                                    CStr::from_ptr(fragment as *const i8).to_string_lossy();
                                xml_xinclude_err!(
                                    ctxt,
                                    (*refe).elem.map(|node| node.into()),
                                    XmlParserErrors::XmlXIncludeXPtrResult,
                                    "XPointer selects a namespace: #{}\n",
                                    fragment
                                );
                                set.node_tab.swap_remove(i);
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
                                    (*refe).elem.map(|node| node.into()),
                                    XmlParserErrors::XmlXIncludeXPtrResult,
                                    "XPointer selects unexpected nodes: #{}\n",
                                    fragment
                                );
                                set.node_tab.swap_remove(i);
                                continue; /* for */
                            }
                            _ => unreachable!(),
                        }
                        i += 1;
                    }
                }
                (*refe).inc = xml_xinclude_copy_xpointer(ctxt, xptr);
                xml_xpath_free_object(xptr);
                xml_xpath_free_context(xptrctxt);
            }
        }

        // Do the xml:base fixup if needed
        if doc.map_or(false, |doc| {
            doc.parse_flags & XmlParserOption::XmlParseNoBasefix as i32 == 0
        }) && !url.is_null()
            && (*ctxt).parse_flags & XmlParserOption::XmlParseNoBasefix as i32 == 0
        {
            // The base is only adjusted if "necessary", i.e. if the xinclude node
            // has a base specified, or the URL is relative
            let base = (*refe)
                .elem
                .unwrap()
                .get_ns_prop("base", XML_XML_NAMESPACE.to_str().ok());
            let mut cbase = base
                .as_deref()
                .map_or(null_mut(), |b| xml_strndup(b.as_ptr(), b.len() as i32));
            if cbase.is_null() {
                // No xml:base on the xinclude node, so we check whether the
                // URI base is different than (relative to) the context base
                let url = CStr::from_ptr(url as *const i8).to_string_lossy();
                if let Some(cur_base) = build_relative_uri(
                    &url,
                    (!(*ctxt).base.is_null())
                        .then(|| CStr::from_ptr((*ctxt).base as *const i8).to_string_lossy())
                        .as_deref(),
                ) {
                    // If the URI doesn't contain a slash, it's not relative
                    if cur_base.contains('/') {
                        cbase = xml_strndup(cur_base.as_ptr(), cur_base.len() as i32);
                    }
                } else {
                    // Error return
                    xml_xinclude_err!(
                        ctxt,
                        (*refe).elem.map(|node| node.into()),
                        XmlParserErrors::XmlXIncludeHrefURI,
                        "trying to build relative URI from {}\n",
                        url
                    );
                }
            }
            if !cbase.is_null() {
                // Adjustment may be needed
                let mut node = (*refe).inc;
                while let Some(mut cur_node) = node {
                    // Only work on element nodes
                    if cur_node.element_type() == XmlElementType::XmlElementNode {
                        if let Some(cur_base) = cur_node.get_base(cur_node.doc) {
                            // If the current base is the same as the
                            // URL of the document, then reset it to be
                            // the specified xml:base or the relative URI
                            if cur_node.doc.as_deref().and_then(|doc| doc.url.as_deref())
                                == Some(cur_base.as_str())
                            {
                                cur_node.set_base(Some(
                                    CStr::from_ptr(cbase as *const i8)
                                        .to_string_lossy()
                                        .as_ref(),
                                ));
                            } else {
                                // If the element already has an xml:base set,
                                // then relativise it if necessary

                                if let Some(xml_base) =
                                    cur_node.get_ns_prop("base", XML_XML_NAMESPACE.to_str().ok())
                                {
                                    let base = CStr::from_ptr(cbase as *const i8).to_string_lossy();
                                    let rel_base = build_uri(&xml_base, &base);
                                    if let Some(rel_base) = rel_base {
                                        cur_node.set_base(Some(&rel_base));
                                    } else {
                                        // error
                                        xml_xinclude_err!(
                                            ctxt,
                                            (*refe).elem.map(|node| node.into()),
                                            XmlParserErrors::XmlXIncludeHrefURI,
                                            "trying to rebuild base from {}\n",
                                            xml_base
                                        );
                                    }
                                }
                            }
                        } else {
                            // If no current base, set it
                            cur_node.set_base(Some(
                                CStr::from_ptr(cbase as *const i8)
                                    .to_string_lossy()
                                    .as_ref(),
                            ));
                        }
                    }
                    node = cur_node
                        .next
                        .and_then(|n| XmlNodePtr::from_raw(n.as_ptr()).unwrap());
                }
                xml_free(cbase as _);
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
unsafe fn xml_xinclude_load_txt(
    ctxt: XmlXIncludeCtxtPtr,
    mut url: *const XmlChar,
    refe: XmlXIncludeRefPtr,
) -> i32 {
    let mut i: i32;
    let mut ret: i32 = -1;
    let mut enc = XmlCharEncoding::None;
    let mut pctxt = null_mut();
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
            (*refe).elem.map(|node| node.into()),
            XmlParserErrors::XmlXIncludeHrefURI,
            "invalid value URI {}\n",
            url
        );
        // goto error;
        xml_free_input_stream(input_stream);
        xml_free_parser_ctxt(pctxt);
        xml_free_uri(uri);
        return ret;
    }
    if !(*uri).fragment.is_null() {
        let fragment = CStr::from_ptr((*uri).fragment as *const i8).to_string_lossy();
        xml_xinclude_err!(
            ctxt,
            (*refe).elem.map(|node| node.into()),
            XmlParserErrors::XmlXIncludeTextFragment,
            "fragment identifier forbidden for text: {}\n",
            fragment
        );
        // goto error;
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
                (*refe).elem.map(|node| node.into()),
                XmlParserErrors::XmlXIncludeHrefURI,
                "invalid value URI {}\n",
                url
            );
            // goto error;
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
            (*refe).elem.map(|node| node.into()),
            XmlParserErrors::XmlXIncludeTextDocument,
            "text serialization of document not available\n"
        );
        // goto error;
        xml_free_input_stream(input_stream);
        xml_free_parser_ctxt(pctxt);
        xml_free_uri(uri);
        xml_free(url as _);
        return ret;
    }

    // Prevent reloading the document twice.
    for txt in &(*ctxt).txt_tab {
        if xml_str_equal(url, txt.url) {
            let node = xml_new_doc_text((*ctxt).doc, txt.text);
            // goto loaded;
            (*refe).inc = node;
            xml_free_input_stream(input_stream);
            xml_free_parser_ctxt(pctxt);
            xml_free_uri(uri);
            xml_free(url as _);
            return 0;
        }
    }

    // Try to get the encoding if available
    let mut encoding = None;
    if let Some(elem) = (*refe).elem {
        encoding = elem.get_prop(XINCLUDE_PARSE_ENCODING);
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
                    (*refe).elem.map(|node| node.into()),
                    XmlParserErrors::XmlXIncludeUnknownEncoding,
                    "encoding {} not supported\n",
                    encoding
                );
                // goto error;
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
        xml_free_input_stream(input_stream);
        xml_free_parser_ctxt(pctxt);
        xml_free_uri(uri);
        xml_free(url as _);
        return ret;
    }
    let Some(buf) = (*input_stream).buf.as_mut() else {
        // goto error;
        xml_free_input_stream(input_stream);
        xml_free_parser_ctxt(pctxt);
        xml_free_uri(uri);
        xml_free(url as _);
        return ret;
    };
    buf.borrow_mut().encoder = get_encoding_handler(enc);
    let Some(mut node) = xml_new_doc_text((*ctxt).doc, null_mut()) else {
        xml_xinclude_err_memory(ctxt, (*refe).elem.map(|node| node.into()), None);
        // goto error;
        xml_free_input_stream(input_stream);
        xml_free_parser_ctxt(pctxt);
        xml_free_uri(uri);
        xml_free(url as _);
        return ret;
    };

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
                (*refe).elem.map(|node| node.into()),
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

    (*ctxt).txt_tab.push(XmlXIncludeTxt {
        text: xml_strdup(node.content),
        url: xml_strdup(url),
    });

    // loaded:
    // Add the element as the replacement copy.
    (*refe).inc = Some(node);
    ret = 0;

    // error:
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
unsafe fn xml_xinclude_load_fallback(
    ctxt: XmlXIncludeCtxtPtr,
    fallback: XmlNodePtr,
    refe: XmlXIncludeRefPtr,
) -> i32 {
    let mut ret: i32 = 0;
    let old_nb_errors: i32;

    if fallback.element_type() == XmlElementType::XmlNamespaceDecl || ctxt.is_null() {
        return -1;
    }
    if fallback.children().is_some() {
        // It's possible that the fallback also has 'includes'
        // (Bug 129969), so we re-process the fallback just in case
        old_nb_errors = (*ctxt).nb_errors;
        (*refe).inc = xml_xinclude_copy_node(ctxt, fallback, 1);
        if (*ctxt).nb_errors > old_nb_errors {
            ret = -1;
        } else if (*refe).inc.is_none() {
            (*refe).empty_fb = 1;
        }
    } else {
        (*refe).inc = None;
        (*refe).empty_fb = 1; /* flag empty callback */
    }
    (*refe).fallback = 1;
    ret
}

/// Find and load the infoset replacement for the given node.
///
/// Returns 0 if substitution succeeded, -1 if some processing failed
#[doc(alias = "xmlXIncludeLoadNode")]
unsafe fn xml_xinclude_load_node(ctxt: XmlXIncludeCtxtPtr, refe: XmlXIncludeRefPtr) -> i32 {
    let mut xml: i32 = 1; /* default Issue 64 */
    let mut ret: i32;

    if ctxt.is_null() || refe.is_null() {
        return -1;
    }
    let Some(cur) = (*refe).elem else {
        return -1;
    };

    // read the attributes
    let href = xml_xinclude_get_prop(ctxt, cur, XINCLUDE_HREF).unwrap_or("".to_owned());
    let parse = xml_xinclude_get_prop(ctxt, cur, XINCLUDE_PARSE);
    if let Some(parse) = parse {
        if parse == XINCLUDE_PARSE_XML {
            xml = 1;
        } else if parse == XINCLUDE_PARSE_TEXT {
            xml = 0;
        } else {
            xml_xinclude_err!(
                ctxt,
                Some(cur.into()),
                XmlParserErrors::XmlXIncludeParseValue,
                "invalid value {} for 'parse'\n",
                parse
            );
            return -1;
        }
    }

    // compute the URI
    let mut base = None;
    let mut uri = (*ctxt).doc.and_then(|doc| {
        if let Some(b) = cur.get_base(Some(doc)) {
            base = Some(b);
            build_uri(&href, base.as_deref().unwrap())
        } else {
            doc.url.as_deref().and_then(|base| build_uri(&href, base))
        }
    });
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
            Some(cur.into()),
            XmlParserErrors::XmlXIncludeHrefURI,
            "failed build URL\n"
        );
        return -1;
    };
    let uri = CString::new(uri).unwrap();

    // Save the base for this include (saving the current one)
    let old_base: *mut XmlChar = (*ctxt).base;
    (*ctxt).base = base
        .as_deref()
        .map_or(null_mut(), |b| xml_strndup(b.as_ptr(), b.len() as i32));

    if xml != 0 {
        ret = xml_xinclude_load_doc(ctxt, uri.as_ptr() as *const u8, refe);
    // xmlXIncludeGetFragment(ctxt, cur, URI);
    } else {
        ret = xml_xinclude_load_txt(ctxt, uri.as_ptr() as *const u8, refe);
    }

    // Restore the original base before checking for fallback
    xml_free((*ctxt).base as _);
    (*ctxt).base = old_base;

    if ret < 0 {
        // Time to try a fallback if available
        let mut children = cur
            .children
            .and_then(|c| XmlNodePtr::from_raw(c.as_ptr()).unwrap());
        while let Some(cur_node) = children {
            if cur_node.element_type() == XmlElementType::XmlElementNode
                && cur_node.name().as_deref() == Some(XINCLUDE_FALLBACK)
                && cur_node.ns.map_or(false, |ns| {
                    ns.href().as_deref() == Some(XINCLUDE_NS.to_str().unwrap())
                        || ns.href().as_deref() == Some(XINCLUDE_OLD_NS.to_str().unwrap())
                })
            {
                ret = xml_xinclude_load_fallback(ctxt, cur_node, refe);
                break;
            }
            children = cur_node
                .next
                .and_then(|n| XmlNodePtr::from_raw(n.as_ptr()).unwrap());
        }
    }
    if ret < 0 {
        xml_xinclude_err!(
            ctxt,
            Some(cur.into()),
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
unsafe fn xml_xinclude_expand_node(
    ctxt: XmlXIncludeCtxtPtr,
    node: XmlNodePtr,
) -> XmlXIncludeRefPtr {
    if (*ctxt).fatal_err != 0 {
        return null_mut();
    }
    if (*ctxt).depth >= XINCLUDE_MAX_DEPTH {
        xml_xinclude_err!(
            ctxt,
            Some(node.into()),
            XmlParserErrors::XmlXIncludeRecursion,
            "maximum recursion depth exceeded\n"
        );
        (*ctxt).fatal_err = 1;
        return null_mut();
    }

    for &inc in &(*ctxt).inc_tab {
        if (*inc).elem == Some(node) {
            if (*inc).expanding != 0 {
                xml_xinclude_err!(
                    ctxt,
                    Some(node.into()),
                    XmlParserErrors::XmlXIncludeRecursion,
                    "inclusion loop detected\n"
                );
                return null_mut();
            }
            return inc;
        }
    }

    let refe: XmlXIncludeRefPtr = xml_xinclude_add_node(ctxt, node);
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
unsafe fn xml_xinclude_include_node(ctxt: XmlXIncludeCtxtPtr, refe: XmlXIncludeRefPtr) -> i32 {
    if ctxt.is_null() || refe.is_null() {
        return -1;
    }
    let cur = (*refe).elem;
    let Some(mut cur) = cur.filter(|cur| cur.element_type() != XmlElementType::XmlNamespaceDecl)
    else {
        return -1;
    };

    let mut list = (*refe).inc.take();
    (*refe).empty_fb = 0;

    // Check against the risk of generating a multi-rooted document
    if cur
        .parent()
        .filter(|p| p.element_type() != XmlElementType::XmlElementNode)
        .is_some()
    {
        let mut nb_elem: i32 = 0;

        let mut tmp = list;
        while let Some(cur) = tmp {
            if cur.element_type() == XmlElementType::XmlElementNode {
                nb_elem += 1;
            }
            tmp = cur
                .next
                .and_then(|n| XmlNodePtr::from_raw(n.as_ptr()).unwrap());
        }
        if nb_elem > 1 {
            xml_xinclude_err!(
                ctxt,
                (*refe).elem.map(|node| node.into()),
                XmlParserErrors::XmlXIncludeMultipleRoot,
                "XInclude error: would result in multiple root nodes\n"
            );
            xml_free_node_list(list);
            return -1;
        }
    }

    if (*ctxt).parse_flags & XmlParserOption::XmlParseNoXIncnode as i32 != 0 {
        // Add the list of nodes
        while let Some(cur_node) = list {
            list = cur_node
                .next
                .and_then(|n| XmlNodePtr::from_raw(n.as_ptr()).unwrap());

            cur.add_prev_sibling(XmlGenericNodePtr::from(cur_node));
        }
        // FIXME: xmlUnlinkNode doesn't coalesce text nodes.
        cur.unlink();
        xml_free_node(cur);
    } else {
        // Change the current node as an XInclude start one, and add an XInclude end one
        if (*refe).fallback != 0 {
            cur.unset_prop("href");
        }
        cur.typ = XmlElementType::XmlXIncludeStart;
        // Remove fallback children
        let mut child = cur
            .children()
            .and_then(|children| XmlGenericNodePtr::from_raw(children.as_ptr()));
        while let Some(mut now) = child {
            let next = now
                .next()
                .and_then(|next| XmlGenericNodePtr::from_raw(next.as_ptr()));
            now.unlink();
            xml_free_node(now);
            child = next;
        }
        let Some(mut end) = xml_new_doc_node(cur.doc, cur.ns, &cur.name().unwrap(), null_mut())
        else {
            xml_xinclude_err!(
                ctxt,
                (*refe).elem.map(|node| node.into()),
                XmlParserErrors::XmlXIncludeBuildFailed,
                "failed to build node\n"
            );
            xml_free_node_list(list);
            return -1;
        };
        end.typ = XmlElementType::XmlXIncludeEnd;
        cur.add_next_sibling(end.into());

        // Add the list of nodes
        while let Some(cur_node) = list {
            list = cur_node
                .next
                .and_then(|n| XmlNodePtr::from_raw(n.as_ptr()).unwrap());

            end.add_prev_sibling(XmlGenericNodePtr::from(cur_node));
        }
    }

    0
}

/// Implement the XInclude substitution on the XML document @doc
///
/// Returns 0 if no substitution were done, -1 if some processing failed
/// or the number of substitutions done.
#[doc(alias = "xmlXIncludeDoProcess")]
unsafe fn xml_xinclude_do_process(ctxt: XmlXIncludeCtxtPtr, tree: XmlNodePtr) -> i32 {
    let mut refe: XmlXIncludeRefPtr;
    let mut ret: i32 = 0;

    if tree.element_type() == XmlElementType::XmlNamespaceDecl {
        return -1;
    }
    if ctxt.is_null() {
        return -1;
    }

    // First phase: lookup the elements in the document
    let start = (*ctxt).inc_tab.len();
    let mut cur = tree;
    'main: while {
        'inner: {
            // TODO: need to work on entities -> stack
            if xml_xinclude_test_node(ctxt, cur) == 1 {
                refe = xml_xinclude_expand_node(ctxt, cur);
                // Mark direct includes.
                if !refe.is_null() {
                    (*refe).replace = 1;
                }
            } else if let Some(children) = cur
                .children()
                .filter(|_| {
                    matches!(
                        cur.element_type(),
                        XmlElementType::XmlDocumentNode | XmlElementType::XmlElementNode
                    )
                })
                .and_then(|children| XmlNodePtr::from_raw(children.as_ptr()).unwrap())
            {
                cur = children;
                break 'inner;
            }
            'b: loop {
                if cur == tree {
                    break 'main;
                }
                if let Some(next) = cur
                    .next
                    .and_then(|next| XmlNodePtr::from_raw(next.as_ptr()).unwrap())
                {
                    cur = next;
                    break 'b;
                }
                let Some(next) = cur
                    .parent
                    .and_then(|p| XmlNodePtr::from_raw(p.as_ptr()).unwrap())
                else {
                    break 'main;
                };

                cur = next;
            }
        }

        cur != tree
    } {}

    // Second phase: extend the original document infoset.
    for &inc in &(*ctxt).inc_tab[start..] {
        if (*inc).replace != 0 {
            if (*inc).inc.is_some() || (*inc).empty_fb != 0 {
                // (empty fallback)
                xml_xinclude_include_node(ctxt, inc);
            }
            (*inc).replace = 0;
        } else {
            // Ignore includes which were added indirectly, for example
            // inside xi:fallback elements.
            if let Some(inc) = (*inc).inc.take() {
                xml_free_node_list(Some(inc));
            }
        }
        ret += 1;
    }

    if (*ctxt).is_stream != 0 {
        // incTab references nodes which will eventually be deleted in
        // streaming mode. The table is only required for XPointer
        // expressions which aren't allowed in streaming mode.
        for inc in (*ctxt).inc_tab.drain(..) {
            xml_xinclude_free_ref(inc);
        }
    }

    ret
}

/// Implement the XInclude substitution on the XML node @tree
///
/// Returns 0 if no substitution were done, -1 if some processing failed
/// or the number of substitutions done.
#[doc(alias = "xmlXIncludeProcessTreeFlagsData")]
pub unsafe fn xml_xinclude_process_tree_flags_data(
    tree: XmlNodePtr,
    flags: i32,
    data: *mut c_void,
) -> i32 {
    if tree.element_type() == XmlElementType::XmlNamespaceDecl {
        return -1;
    }
    let Some(doc) = tree.doc else {
        return -1;
    };

    let ctxt: XmlXIncludeCtxtPtr = xml_xinclude_new_context(doc);
    if ctxt.is_null() {
        return -1;
    }
    (*ctxt)._private = data;
    let url = doc.url.as_deref().map(|u| CString::new(u).unwrap());
    (*ctxt).base = xml_strdup(url.as_ref().map_or(null(), |u| u.as_ptr() as *const u8));
    xml_xinclude_set_flags(ctxt, flags);
    let mut ret = xml_xinclude_do_process(ctxt, tree);
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
pub unsafe fn xml_xinclude_process_tree(tree: XmlNodePtr) -> i32 {
    xml_xinclude_process_tree_flags(tree, 0)
}

/// Implement the XInclude substitution for the given subtree
///
/// Returns 0 if no substitution were done, -1 if some processing failed
/// or the number of substitutions done.
#[doc(alias = "xmlXIncludeProcessTreeFlags")]
pub unsafe fn xml_xinclude_process_tree_flags(tree: XmlNodePtr, flags: i32) -> i32 {
    if tree.element_type() == XmlElementType::XmlNamespaceDecl {
        return -1;
    }
    let Some(doc) = tree.doc else {
        return -1;
    };
    let ctxt: XmlXIncludeCtxtPtr = xml_xinclude_new_context(doc);
    if ctxt.is_null() {
        return -1;
    }
    let tmp = tree.get_base(Some(doc)).map(|c| CString::new(c).unwrap());
    (*ctxt).base = tmp
        .as_ref()
        .map_or(null_mut(), |c| xml_strdup(c.as_ptr() as *const u8));
    xml_xinclude_set_flags(ctxt, flags);
    let mut ret = xml_xinclude_do_process(ctxt, tree);
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
pub unsafe fn xml_xinclude_new_context(doc: XmlDocPtr) -> XmlXIncludeCtxtPtr {
    let ret: XmlXIncludeCtxtPtr = xml_malloc(size_of::<XmlXIncludeCtxt>()) as XmlXIncludeCtxtPtr;
    if ret.is_null() {
        xml_xinclude_err_memory(
            null_mut(),
            Some(doc.into()),
            Some("creating XInclude context"),
        );
        return null_mut();
    }
    std::ptr::write(&mut *ret, XmlXIncludeCtxt::default());
    (*ret).doc = Some(doc);
    (*ret).nb_errors = 0;
    ret
}

/// Set the flags used for further processing of XML resources.
///
/// Returns 0 in case of success and -1 in case of error.
#[doc(alias = "xmlXIncludeSetFlags")]
pub unsafe fn xml_xinclude_set_flags(ctxt: XmlXIncludeCtxtPtr, flags: i32) -> i32 {
    if ctxt.is_null() {
        return -1;
    }
    (*ctxt).parse_flags = flags;
    0
}

/// Free an XInclude context
#[doc(alias = "xmlXIncludeFreeContext")]
pub unsafe fn xml_xinclude_free_context(ctxt: XmlXIncludeCtxtPtr) {
    if ctxt.is_null() {
        return;
    }
    for inc_doc in (*ctxt).url_tab.drain(..) {
        if let Some(doc) = inc_doc.doc {
            xml_free_doc(doc);
        }
        xml_free(inc_doc.url as _);
    }
    for inc in (*ctxt).inc_tab.drain(..) {
        if !inc.is_null() {
            xml_xinclude_free_ref(inc);
        }
    }
    for txt in (*ctxt).txt_tab.drain(..) {
        xml_free(txt.text as _);
        xml_free(txt.url as _);
    }
    if !(*ctxt).base.is_null() {
        xml_free((*ctxt).base as _);
    }
    drop_in_place(ctxt);
    xml_free(ctxt as _);
}

/// Implement the XInclude substitution for the given subtree reusing
/// the information and data coming from the given context.
///
/// Returns 0 if no substitution were done, -1 if some processing failed
/// or the number of substitutions done.
#[doc(alias = "xmlXIncludeProcessNode")]
pub unsafe fn xml_xinclude_process_node(ctxt: XmlXIncludeCtxtPtr, node: XmlNodePtr) -> i32 {
    if node.element_type() == XmlElementType::XmlNamespaceDecl
        || node.doc.is_none()
        || ctxt.is_null()
    {
        return -1;
    }
    let mut ret = xml_xinclude_do_process(ctxt, node);
    if ret >= 0 && (*ctxt).nb_errors > 0 {
        ret = -1;
    }
    ret
}

/// In streaming mode, XPointer expressions aren't allowed.
///
/// Returns 0 in case of success and -1 in case of error.
#[doc(alias = "xmlXIncludeSetStreamingMode")]
pub(crate) unsafe fn xml_xinclude_set_streaming_mode(ctxt: XmlXIncludeCtxtPtr, mode: i32) -> i32 {
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
