//! Provide methods and data structures for XInclude.
//!
//! This module is based on `libxml/xinclude.h`, `xinclude.c`, and so on in `libxml2-v2.11.8`.  
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
    borrow::Cow,
    ffi::{CStr, CString},
    mem::take,
    os::raw::c_void,
    ptr::{addr_of_mut, null_mut},
};

#[cfg(feature = "libxml_xptr_locs")]
use crate::libxml::xpointer::XmlLocationSetPtr;
use crate::{
    encoding::{XmlCharEncoding, get_encoding_handler},
    error::{__xml_raise_error, XmlErrorDomain, XmlErrorLevel, XmlParserErrors},
    io::xml_parser_get_directory,
    libxml::{
        chvalid::xml_is_char,
        parser::{
            XML_DETECT_IDS, XmlParserOption, xml_ctxt_use_options, xml_init_parser,
            xml_load_external_entity, xml_parse_document,
        },
        parser_internals::xml_string_current_char,
        xmlstring::{XmlChar, xml_str_equal},
        xpointer::{xml_xptr_eval, xml_xptr_new_context},
    },
    parser::{xml_free_parser_ctxt, xml_new_parser_ctxt},
    tree::{
        NodeCommon, XML_XML_NAMESPACE, XmlDocPtr, XmlElementType, XmlEntityPtr, XmlEntityType,
        XmlGenericNodePtr, XmlNode, XmlNodePtr, xml_add_doc_entity, xml_create_int_subset,
        xml_doc_copy_node, xml_free_doc, xml_free_node, xml_free_node_list, xml_get_doc_entity,
        xml_new_doc_node, xml_new_doc_text, xml_static_copy_node, xml_static_copy_node_list,
    },
    uri::{XmlURI, build_relative_uri, build_uri, escape_url},
    xpath::{
        XmlXPathContextPtr, XmlXPathObjectPtr, XmlXPathObjectType, xml_xpath_free_context,
        xml_xpath_free_object,
    },
};

/// A constant defining the Xinclude namespace: `http://www.w3.org/2003/XInclude`
pub const XINCLUDE_NS: &str = "http://www.w3.org/2003/XInclude";
/// A constant defining the draft Xinclude namespace: `http://www.w3.org/2001/XInclude`
pub const XINCLUDE_OLD_NS: &str = "http://www.w3.org/2001/XInclude";
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
pub const XINCLUDE_PARSE_XPOINTER: &str = "xpointer";

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
            Some($msg),
        );
    };
}

#[doc(alias = "xmlXIncludeRef")]
#[repr(C)]
#[derive(Default)]
pub struct XmlXIncludeRef {
    uri: Option<Box<str>>,      /* the fully resolved resource URL */
    fragment: Option<Box<str>>, /* the fragment in the URI */
    elem: Option<XmlNodePtr>,   /* the xi:include element */
    inc: Option<XmlNodePtr>,    /* the included copy */
    xml: i32,                   /* xml or txt */
    fallback: i32,              /* fallback was loaded */
    empty_fb: i32,              /* flag to show fallback empty */
    expanding: i32,             /* flag to detect inclusion loops */
    replace: i32,               /* should the node be replaced? */
}

#[doc(alias = "xmlXIncludeDoc")]
#[repr(C)]
pub struct XmlXIncludeDoc {
    doc: Option<XmlDocPtr>, /* the parsed document */
    url: Box<str>,          /* the URL */
    expanding: i32,         /* flag to detect inclusion loops */
}

#[doc(alias = "xmlXIncludeTxt")]
#[repr(C)]
pub struct XmlXIncludeTxt {
    text: Box<str>, /* text string */
    url: Box<str>,  /* the URL */
}

/// An XInclude context
#[doc(alias = "xmlXIncludeCtxt")]
#[repr(C)]
pub struct XmlXIncludeCtxt {
    doc: XmlDocPtr,               /* the source document */
    inc_tab: Vec<XmlXIncludeRef>, /* array of included references */

    txt_tab: Vec<XmlXIncludeTxt>, /* array of unparsed documents */

    url_tab: Vec<XmlXIncludeDoc>, /* document stack */

    nb_errors: i32,         /* the number of errors detected */
    fatal_err: i32,         /* abort processing */
    legacy: i32,            /* using XINCLUDE_OLD_NS */
    parse_flags: i32,       /* the flags used for parsing XML documents */
    base: Option<Box<str>>, /* the current xml:base */

    _private: *mut c_void, /* application data */

    depth: i32,     /* recursion depth */
    is_stream: i32, /* streaming mode */
}

impl XmlXIncludeCtxt {
    /// Creates a new XInclude context
    ///
    /// Returns the new set
    #[doc(alias = "xmlXIncludeNewContext")]
    pub fn new(doc: XmlDocPtr) -> Self {
        XmlXIncludeCtxt {
            doc,
            inc_tab: vec![],
            txt_tab: vec![],
            url_tab: vec![],
            nb_errors: 0,
            fatal_err: 0,
            legacy: 0,
            parse_flags: 0,
            base: None,
            _private: null_mut(),
            depth: 0,
            is_stream: 0,
        }
    }

    /// Get an XInclude attribute
    ///
    /// Returns the value (to be freed) or NULL if not found
    #[doc(alias = "xmlXIncludeGetProp")]
    unsafe fn get_prop(&self, cur: XmlNodePtr, name: &str) -> Option<String> {
        unsafe {
            if let Some(ret) = cur.get_ns_prop(XINCLUDE_NS, Some(name)) {
                return Some(ret);
            }
            if self.legacy != 0 {
                if let Some(ret) = cur.get_ns_prop(XINCLUDE_OLD_NS, Some(name)) {
                    return Some(ret);
                }
            }
            cur.get_prop(name)
        }
    }

    /// In streaming mode, XPointer expressions aren't allowed.
    ///
    /// Returns 0 in case of success and -1 in case of error.
    #[doc(alias = "xmlXIncludeSetStreamingMode")]
    pub(crate) fn set_streaming_mode(&mut self, mode: i32) -> i32 {
        self.is_stream = (mode != 0) as i32;
        0
    }

    /// Set the flags used for further processing of XML resources.
    ///
    /// Returns 0 in case of success and -1 in case of error.
    #[doc(alias = "xmlXIncludeSetFlags")]
    pub fn set_flags(&mut self, flags: i32) -> i32 {
        self.parse_flags = flags;
        0
    }

    /// Add a new node to process to an XInclude context
    #[doc(alias = "xmlXIncludeAddNode")]
    unsafe fn add_node(&mut self, cur: XmlNodePtr) -> usize {
        unsafe {
            let mut xml: i32 = 1;
            let mut local: i32 = 0;

            // read the attributes
            let href = self.get_prop(cur, XINCLUDE_HREF).unwrap_or("".to_owned());
            let parse = self.get_prop(cur, XINCLUDE_PARSE);
            if let Some(parse) = parse {
                if parse == XINCLUDE_PARSE_XML {
                    xml = 1;
                } else if parse == XINCLUDE_PARSE_TEXT {
                    xml = 0;
                } else {
                    xml_xinclude_err!(
                        self,
                        Some(cur.into()),
                        XmlParserErrors::XmlXIncludeParseValue,
                        "invalid value {} for 'parse'\n",
                        parse
                    );
                    return usize::MAX;
                }
            }

            // compute the URI
            let mut base = None;
            let mut uri = if let Some(b) = cur.get_base(Some(self.doc)) {
                base = Some(b);
                build_uri(&href, base.as_deref().unwrap())
            } else {
                self.doc
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
                    self,
                    Some(cur.into()),
                    XmlParserErrors::XmlXIncludeHrefURI,
                    "failed build URL\n"
                );
                return usize::MAX;
            };
            let mut fragment = self.get_prop(cur, XINCLUDE_PARSE_XPOINTER);

            // Check the URL and remove any fragment identifier
            let Some(mut parsed_uri) = XmlURI::parse(&uri) else {
                xml_xinclude_err!(
                    self,
                    Some(cur.into()),
                    XmlParserErrors::XmlXIncludeHrefURI,
                    "invalid value URI {}\n",
                    uri
                );
                return usize::MAX;
            };

            if parsed_uri.fragment.is_some() {
                if self.legacy != 0 {
                    if fragment.is_none() {
                        fragment = parsed_uri.fragment.as_deref().map(|f| f.to_owned());
                    }
                } else {
                    xml_xinclude_err!(
                        self,
                        Some(cur.into()),
                        XmlParserErrors::XmlXIncludeFragmentID,
                        "Invalid fragment identifier in URI {} use the xpointer attribute\n",
                        uri
                    );
                    return usize::MAX;
                }
                parsed_uri.fragment = None;
            }
            let url = parsed_uri.save();

            if self.doc.url.as_deref() == Some(url.as_str()) {
                local = 1;
            }

            // If local and xml then we need a fragment
            if local == 1 && xml == 1 && fragment.as_deref().is_none_or(|f| f.is_empty()) {
                xml_xinclude_err!(
                    self,
                    Some(cur.into()),
                    XmlParserErrors::XmlXIncludeRecursion,
                    "detected a local recursion with no xpointer in {}\n",
                    url
                );
                return usize::MAX;
            }

            let refe = self.add_ref(Some(&url), cur);
            self.inc_tab[refe].fragment = fragment.map(|fragment| fragment.into());
            self.inc_tab[refe].xml = xml;
            refe
        }
    }

    /// Creates a new reference within an XInclude context
    ///
    /// Returns the new set
    #[doc(alias = "xmlXIncludeNewRef")]
    fn add_ref(&mut self, uri: Option<&str>, elem: XmlNodePtr) -> usize {
        self.inc_tab.push(XmlXIncludeRef {
            uri: uri.map(|uri| uri.into()),
            fragment: None,
            elem: Some(elem),
            xml: 0,
            inc: None,
            ..Default::default()
        });
        self.inc_tab.len() - 1
    }

    /// Make a copy of the node while expanding nested XIncludes.
    ///
    /// Returns a node list, not a single node.
    #[doc(alias = "xmlXIncludeCopyNode")]
    unsafe fn copy_node(&mut self, elem: XmlNodePtr, copy_children: i32) -> Option<XmlNodePtr> {
        unsafe {
            let mut result: Option<XmlNodePtr> = None;
            let mut insert_parent: Option<XmlNodePtr> = None;
            let mut insert_last: Option<XmlNodePtr> = None;

            let mut cur = if copy_children != 0 {
                elem.children.map(|c| XmlNodePtr::try_from(c).unwrap())?
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
                    && cur.ns.is_some_and(|ns| {
                        ns.href().as_deref() == Some(XINCLUDE_NS)
                            || ns.href().as_deref() == Some(XINCLUDE_OLD_NS)
                    })
                {
                    let ref_index = self.expand_node(cur);

                    if ref_index == usize::MAX {
                        // goto error;
                        xml_free_node_list(result);
                        return None;
                    }
                    // TODO: Insert xmlElementType::XML_XINCLUDE_START and xmlElementType::XML_XINCLUDE_END nodes
                    if let Some(inc) = self.inc_tab[ref_index].inc {
                        let Some(res) = xml_static_copy_node_list(
                            Some(XmlGenericNodePtr::from(inc)),
                            Some(self.doc),
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
                        Some(self.doc),
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
                        insert_last.next = Some(copy.into());
                        copy.prev = Some(insert_last.into());
                    } else if let Some(mut insert_parent) = insert_parent {
                        insert_parent.children = Some(copy.into());
                    }
                    let mut now = copy;
                    while let Some(next) = now.next.map(|node| XmlNodePtr::try_from(node).unwrap())
                    {
                        now = next;
                    }
                    insert_last = Some(now);
                }

                if recurse != 0 {
                    cur = cur
                        .children
                        .map(|c| XmlNodePtr::try_from(c).unwrap())
                        .unwrap();
                    insert_parent = insert_last.take();
                    continue;
                }

                if cur == elem {
                    return result;
                }

                while cur.next.is_none() {
                    if let Some(mut insert_parent) = insert_parent {
                        insert_parent.last = insert_last.map(|node| node.into());
                    }
                    cur = cur
                        .parent
                        .map(|p| XmlNodePtr::try_from(p).unwrap())
                        .unwrap();
                    if cur == elem {
                        return result;
                    }
                    insert_last = insert_parent;
                    insert_parent = insert_parent
                        .unwrap()
                        .parent
                        .map(|p| XmlNodePtr::try_from(p).unwrap());
                }

                cur = cur
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap())
                    .unwrap();
            }

            // error:
            // xmlFreeNodeList(result);
            // return null_mut();
        }
    }

    /// Test if the node is an XInclude node
    ///
    /// Returns 1 true, 0 otherwise
    #[doc(alias = "xmlXIncludeTestNode")]
    unsafe fn test_node(&mut self, node: XmlNodePtr) -> i32 {
        unsafe {
            if node.element_type() != XmlElementType::XmlElementNode {
                return 0;
            }
            let Some(node_ns) = node.ns else {
                return 0;
            };
            if node_ns.href().as_deref() == Some(XINCLUDE_NS)
                || node_ns.href().as_deref() == Some(XINCLUDE_OLD_NS)
            {
                if node_ns.href().as_deref() == Some(XINCLUDE_OLD_NS) && self.legacy == 0 {
                    self.legacy = 1;
                }
                if node.name().as_deref() == Some(XINCLUDE_NODE) {
                    let mut child = node.children.map(|c| XmlNodePtr::try_from(c).unwrap());
                    let mut nb_fallback: i32 = 0;

                    while let Some(cur_node) = child {
                        if cur_node.element_type() == XmlElementType::XmlElementNode
                            && cur_node.ns.is_some_and(|ns| {
                                ns.href().as_deref() == Some(XINCLUDE_NS)
                                    || ns.href().as_deref() == Some(XINCLUDE_OLD_NS)
                            })
                        {
                            if cur_node.name().as_deref() == Some(XINCLUDE_NODE) {
                                xml_xinclude_err!(
                                    self,
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
                            .map(|node| XmlNodePtr::try_from(node).unwrap());
                    }
                    if nb_fallback > 1 {
                        xml_xinclude_err!(
                            self,
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
                        || XmlNodePtr::try_from(node.parent().unwrap())
                            .unwrap()
                            .ns
                            .is_none_or(|ns| {
                                ns.href().as_deref() != Some(XINCLUDE_NS)
                                    && ns.href().as_deref() != Some(XINCLUDE_OLD_NS)
                            })
                        || node.parent().unwrap().name().as_deref() != Some(XINCLUDE_NODE))
                {
                    xml_xinclude_err!(
                        self,
                        Some(node.into()),
                        XmlParserErrors::XmlXIncludeFallbackNotInInclude,
                        "{} is not the child of an 'include'\n",
                        XINCLUDE_FALLBACK
                    );
                }
            }
            0
        }
    }

    /// If the XInclude node wasn't processed yet, create a new RefPtr,
    /// add it to self.incTab and load the included items.
    ///
    /// Returns the index of new or existing `XmlXIncludeRef` or `usize::MAX` in case of error.
    #[doc(alias = "xmlXIncludeExpandNode")]
    unsafe fn expand_node(&mut self, node: XmlNodePtr) -> usize {
        unsafe {
            if self.fatal_err != 0 {
                return usize::MAX;
            }
            if self.depth >= XINCLUDE_MAX_DEPTH {
                xml_xinclude_err!(
                    self,
                    Some(node.into()),
                    XmlParserErrors::XmlXIncludeRecursion,
                    "maximum recursion depth exceeded\n"
                );
                self.fatal_err = 1;
                return usize::MAX;
            }

            for (i, inc) in self.inc_tab.iter().enumerate() {
                if inc.elem == Some(node) {
                    if inc.expanding != 0 {
                        xml_xinclude_err!(
                            self,
                            Some(node.into()),
                            XmlParserErrors::XmlXIncludeRecursion,
                            "inclusion loop detected\n"
                        );
                        return usize::MAX;
                    }
                    return i;
                }
            }

            let refe = self.add_node(node);
            if refe == usize::MAX {
                return usize::MAX;
            }
            self.inc_tab[refe].expanding = 1;
            self.depth += 1;
            self.load_node(refe);
            self.depth -= 1;
            self.inc_tab[refe].expanding = 0;

            refe
        }
    }

    /// Implement the infoset replacement for the given node
    ///
    /// Returns 0 if substitution succeeded, -1 if some processing failed
    #[doc(alias = "xmlXIncludeIncludeNode")]
    unsafe fn include_node(&mut self, ref_index: usize) -> i32 {
        unsafe {
            if ref_index == usize::MAX {
                return -1;
            }
            let cur = self.inc_tab[ref_index].elem;
            let Some(mut cur) =
                cur.filter(|cur| cur.element_type() != XmlElementType::XmlNamespaceDecl)
            else {
                return -1;
            };

            let mut list = self.inc_tab[ref_index].inc.take();
            self.inc_tab[ref_index].empty_fb = 0;

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
                    tmp = cur.next.map(|node| XmlNodePtr::try_from(node).unwrap());
                }
                if nb_elem > 1 {
                    xml_xinclude_err!(
                        self,
                        self.inc_tab[ref_index].elem.map(|node| node.into()),
                        XmlParserErrors::XmlXIncludeMultipleRoot,
                        "XInclude error: would result in multiple root nodes\n"
                    );
                    xml_free_node_list(list);
                    return -1;
                }
            }

            if self.parse_flags & XmlParserOption::XmlParseNoXIncnode as i32 != 0 {
                // Add the list of nodes
                while let Some(cur_node) = list {
                    list = cur_node
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());

                    cur.add_prev_sibling(XmlGenericNodePtr::from(cur_node));
                }
                // FIXME: xmlUnlinkNode doesn't coalesce text nodes.
                cur.unlink();
                xml_free_node(cur);
            } else {
                // Change the current node as an XInclude start one, and add an XInclude end one
                if self.inc_tab[ref_index].fallback != 0 {
                    cur.unset_prop("href");
                }
                cur.typ = XmlElementType::XmlXIncludeStart;
                // Remove fallback children
                let mut child = cur.children();
                while let Some(mut now) = child {
                    let next = now.next();
                    now.unlink();
                    xml_free_node(now);
                    child = next;
                }
                let Some(mut end) =
                    xml_new_doc_node(cur.doc, cur.ns, &cur.name().unwrap(), null_mut())
                else {
                    xml_xinclude_err!(
                        self,
                        self.inc_tab[ref_index].elem.map(|node| node.into()),
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
                        .map(|node| XmlNodePtr::try_from(node).unwrap());

                    end.add_prev_sibling(XmlGenericNodePtr::from(cur_node));
                }
            }

            0
        }
    }

    /// Implements the entity merge
    ///
    /// Returns 0 if merge succeeded, -1 if some processing failed
    #[doc(alias = "xmlXIncludeMergeEntities")]
    unsafe fn merge_entities(&mut self, doc: XmlDocPtr, from: XmlDocPtr) -> i32 {
        unsafe {
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
                    entities.scan(|payload, _, _, _| {
                        self.merge_entity(*payload, doc);
                    });
                }
            }
            let source = from.ext_subset;
            if let Some(source) = source {
                if let Some(entities) = source.entities {
                    // don't duplicate existing stuff when external subsets are the same
                    if target.external_id != source.external_id
                        && target.system_id != source.system_id
                    {
                        entities.scan(|payload, _, _, _| {
                            self.merge_entity(*payload, doc);
                        });
                    }
                }
            }
            0
        }
    }

    /// Implements the merge of one entity
    #[doc(alias = "xmlXIncludeMergeOneEntity")]
    unsafe fn merge_entity(&mut self, ent: XmlEntityPtr, doc: XmlDocPtr) {
        unsafe {
            match ent.etype {
                XmlEntityType::XmlInternalParameterEntity
                | XmlEntityType::XmlExternalParameterEntity
                | XmlEntityType::XmlInternalPredefinedEntity => return,
                XmlEntityType::XmlInternalGeneralEntity
                | XmlEntityType::XmlExternalGeneralParsedEntity
                | XmlEntityType::XmlExternalGeneralUnparsedEntity => {}
            }

            let content = ent.content;
            let ret = xml_add_doc_entity(
                doc,
                &ent.name().unwrap(),
                ent.etype,
                ent.external_id.as_deref(),
                ent.system_id.as_deref(),
                (!content.is_null())
                    .then(|| CStr::from_ptr(content as *const i8).to_string_lossy())
                    .as_deref(),
            );
            if let Some(mut ret) = ret {
                ret.uri = ent.uri.clone();
                return;
            }

            'error: {
                let prev = xml_get_doc_entity(Some(doc), &ent.name().unwrap());
                if let Some(prev) = prev {
                    if ent.etype != prev.etype {
                        break 'error;
                    }

                    if ent.system_id.is_some() && prev.system_id.is_some() {
                        if ent.system_id != prev.system_id {
                            break 'error;
                        }
                    } else if ent.external_id.is_some() && prev.external_id.is_some() {
                        if ent.external_id != prev.external_id {
                            break 'error;
                        }
                    } else if !ent.content.is_null() && !prev.content.is_null() {
                        if !xml_str_equal(ent.content, prev.content) {
                            break 'error;
                        }
                    } else {
                        break 'error;
                    }
                }
                return;
            }
            match ent.etype {
                XmlEntityType::XmlInternalParameterEntity
                | XmlEntityType::XmlExternalParameterEntity
                | XmlEntityType::XmlInternalPredefinedEntity
                | XmlEntityType::XmlInternalGeneralEntity
                | XmlEntityType::XmlExternalGeneralParsedEntity => return,
                XmlEntityType::XmlExternalGeneralUnparsedEntity => {}
            }
            xml_xinclude_err!(
                self,
                Some(ent.into()),
                XmlParserErrors::XmlXIncludeEntityDefMismatch,
                "mismatch in redefinition of entity {}\n",
                ent.name().unwrap().into_owned()
            );
        }
    }

    /// Build a node list tree copy of the XPointer result.
    /// This will drop Attributes and Namespace declarations.
    ///
    /// Returns an xmlNodePtr list or NULL.
    /// The caller has to free the node tree.
    #[doc(alias = "xmlXIncludeCopyXPointer")]
    unsafe fn copy_xpointer(&mut self, obj: XmlXPathObjectPtr) -> Option<XmlNodePtr> {
        unsafe {
            let mut list: Option<XmlNodePtr> = None;

            if obj.is_null() {
                return None;
            }
            match (*obj).typ {
                XmlXPathObjectType::XPathNodeset => {
                    let set = (*obj).nodesetval.as_deref()?;
                    let mut last: Option<XmlNodePtr> = None;
                    for &now in &set.node_tab {
                        let node = match now.element_type() {
                            XmlElementType::XmlDocumentNode
                            | XmlElementType::XmlHTMLDocumentNode => {
                                let Some(node) =
                                    XmlDocPtr::try_from(now).unwrap().get_root_element()
                                else {
                                    xml_xinclude_err!(
                                        self,
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
                                    self,
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
                        let Some(mut copy) = self.copy_node(node, 0) else {
                            xml_free_node_list(list);
                            return None;
                        };
                        if let Some(mut last) = last {
                            while let Some(next) =
                                last.next.map(|node| XmlNodePtr::try_from(node).unwrap())
                            {
                                last = next;
                            }
                            copy.prev = Some(last.into());
                            last.next = Some(copy.into());
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
                            last.add_next_sibling(self.copy_xpointer(loc).unwrap().into());
                        } else {
                            list = self.copy_xpointer(loc);
                            last = list;
                        }
                        if let Some(mut l) = last {
                            while let Some(next) =
                                l.next.map(|node| XmlNodePtr::try_from(node).unwrap())
                            {
                                l = next;
                            }
                            last = Some(l);
                        }
                    }
                }
                #[cfg(feature = "libxml_xptr_locs")]
                XmlXPathObjectType::XPathRange => {
                    return self
                        .copy_range(obj)
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                }
                #[cfg(feature = "libxml_xptr_locs")]
                XmlXPathObjectType::XPathPoint => { /* points are ignored in XInclude */ }
                _ => {}
            }
            list
        }
    }

    /// Build a node list tree copy of the XPointer result.
    ///
    /// Returns an xmlNodePtr list or NULL.
    /// The caller has to free the node tree.
    #[doc(alias = "xmlXIncludeCopyRange")]
    #[cfg(feature = "libxml_xptr_locs")]
    unsafe fn copy_range(&self, range: XmlXPathObjectPtr) -> Option<XmlGenericNodePtr> {
        unsafe {
            use crate::{
                libxml::xpointer::xml_xptr_advance_node,
                tree::{xml_new_doc_text, xml_new_doc_text_len},
            };

            // pointers to generated nodes
            let mut list = None;
            let mut last = None;
            let mut list_parent = None;
            let mut level: i32 = 0;
            let mut last_level: i32 = 0;
            let mut end_level: i32 = 0;
            let mut end_flag: i32 = 0;

            if range.is_null() {
                return None;
            }
            if (*range).typ != XmlXPathObjectType::XPathRange {
                return None;
            }
            let start = XmlGenericNodePtr::from_raw((*range).user as *mut XmlNode)
                .filter(|node| node.element_type() != XmlElementType::XmlNamespaceDecl)?;

            let Some(mut end) = XmlGenericNodePtr::from_raw((*range).user2 as *mut XmlNode) else {
                return xml_doc_copy_node(start, Some(self.doc), 1);
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
                        let mut tmp2 =
                            xml_doc_copy_node(list_parent.unwrap(), Some(self.doc), 2).unwrap();
                        tmp2.add_child(list.unwrap());
                        list = Some(tmp2);
                        list_parent = list_parent.unwrap().parent();
                        level += 1;
                    }
                    last = list;
                    last_level = 0;
                }
                // Check whether we need to change our insertion point
                while level < last_level {
                    last = last.unwrap().parent();
                    last_level -= 1;
                }
                if cur_node == end {
                    // Are we at the end of the range?
                    if cur_node.element_type() == XmlElementType::XmlTextNode {
                        let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                        let mut content: *const XmlChar = cur_node.content;
                        let mut len: i32;

                        let tmp = if content.is_null() {
                            xml_new_doc_text_len(Some(self.doc), null_mut(), 0)
                        } else {
                            len = index2;
                            if start == cur_node.into() && index1 > 1 {
                                content = content.add(index1 as usize - 1);
                                len -= index1 - 1;
                            } else {
                                len = index2;
                            }
                            xml_new_doc_text_len(Some(self.doc), content, len)
                        };
                        // single sub text node selection
                        if list.is_none() {
                            return tmp.map(|node| node.into());
                        }
                        // prune and return full set
                        if level == last_level {
                            last.unwrap().add_next_sibling(tmp.unwrap().into());
                        } else {
                            last.unwrap().add_child(tmp.unwrap().into());
                        }
                        return list;
                    } else {
                        // ending node not a text node
                        end_level = level; /* remember the level of the end node */
                        end_flag = 1;
                        // last node - need to take care of properties + namespaces
                        let tmp = xml_doc_copy_node(cur_node, Some(self.doc), 2);
                        if list.is_none() {
                            list = tmp;
                            list_parent = cur_node.parent();
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
                            cur = cur_node.children();
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
                            xml_new_doc_text_len(Some(self.doc), null_mut(), 0)
                        } else {
                            if index1 > 1 {
                                content = content.add(index1 as usize - 1);
                                index1 = 0;
                            }
                            xml_new_doc_text(Some(self.doc), content)
                        };
                        last = tmp.map(|node| node.into());
                        list = tmp.map(|node| node.into());
                        list_parent = cur_node.parent();
                    } else {
                        // Not text node

                        // start of the range - need to take care of
                        // properties and namespaces
                        let tmp = xml_doc_copy_node(cur_node, Some(self.doc), 2);
                        list = tmp;
                        last = tmp;
                        list_parent = cur_node.parent();
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
                        XmlElementType::XmlEntityDecl => { /* handle crossing entities -> stack needed */
                        }
                        XmlElementType::XmlXIncludeStart | XmlElementType::XmlXIncludeEnd => {
                            // don't consider it part of the tree content
                        }
                        XmlElementType::XmlAttributeNode => { /* Humm, should not happen ! */ }
                        _ => {
                            // Middle of the range - need to take care of
                            // properties and namespaces
                            tmp = xml_doc_copy_node(cur_node, Some(self.doc), 2);
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
    }

    /// The XInclude recursive nature is handled at this point.
    #[doc(alias = "xmlXIncludeRecurseDoc")]
    unsafe fn recurse_doc(&mut self, doc: XmlDocPtr, _url: &str) {
        unsafe {
            let old_doc = self.doc;
            let old_inc_tab = take(&mut self.inc_tab);
            let old_is_stream: i32 = self.is_stream;
            self.doc = doc;
            self.is_stream = 0;

            self.do_process(doc.get_root_element().unwrap());

            self.doc = old_doc;
            self.inc_tab = old_inc_tab;
            self.is_stream = old_is_stream;
        }
    }

    /// Load the document, and store the result in the XInclude context
    ///
    /// Returns 0 in case of success, -1 in case of failure
    #[doc(alias = "xmlXIncludeLoadDoc")]
    unsafe fn load_doc(&mut self, url: &str, ref_index: usize) -> i32 {
        unsafe {
            let ret: i32 = -1;
            #[cfg(feature = "xpointer")]
            let save_flags: i32;

            // Check the URL and remove any fragment identifier
            let Some(mut uri) = XmlURI::parse(url) else {
                xml_xinclude_err!(
                    self,
                    self.inc_tab[ref_index].elem.map(|node| node.into()),
                    XmlParserErrors::XmlXIncludeHrefURI,
                    "invalid value URI {}\n",
                    url
                );
                return ret;
            };
            let mut fragment = uri.fragment.take();
            if let Some(frag) = self.inc_tab[ref_index].fragment.as_deref() {
                fragment = Some(Cow::Owned(frag.to_owned()));
            }
            let mut url = uri.save();

            // Handling of references to the local document are done
            // directly through (*ctxt).doc.
            let doc = 'load: {
                if url.is_empty() || url.starts_with('#') || self.doc.url.as_deref() == Some(&url) {
                    break 'load self.doc;
                }
                // Prevent reloading the document twice.
                for inc_doc in &self.url_tab {
                    if *url == *inc_doc.url {
                        if inc_doc.expanding != 0 {
                            xml_xinclude_err!(
                                self,
                                self.inc_tab[ref_index].elem.map(|node| node.into()),
                                XmlParserErrors::XmlXIncludeRecursion,
                                "inclusion loop detected\n"
                            );
                            return ret;
                        }
                        let Some(doc) = inc_doc.doc else {
                            return ret;
                        };
                        break 'load doc;
                    }
                }

                // Load it.
                #[cfg(feature = "xpointer")]
                {
                    // If this is an XPointer evaluation, we want to assure that
                    // all entities have been resolved prior to processing the
                    // referenced document
                    save_flags = self.parse_flags;
                    if fragment.is_some() {
                        // if this is an XPointer eval
                        self.parse_flags |= XmlParserOption::XmlParseNoEnt as i32;
                    }
                }

                let doc = self.parse_file(&url);
                #[cfg(feature = "xpointer")]
                {
                    self.parse_flags = save_flags;
                }

                // Also cache NULL docs
                let cache_nr = self.url_tab.len();
                self.url_tab.push(XmlXIncludeDoc {
                    doc,
                    url: url.clone().into_boxed_str(),
                    expanding: 0,
                });

                let Some(doc) = doc else {
                    return ret;
                };
                // It's possible that the requested URL has been mapped to a
                // completely different location (e.g. through a catalog entry).
                // To check for this, we compare the URL with that of the doc
                // and change it if they disagree (bug 146988).
                if doc.url.as_deref() != Some(&url) {
                    url = doc.url.clone().unwrap();
                }

                // Make sure we have all entities fixed up
                self.merge_entities(self.doc, doc);

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
                self.url_tab[cache_nr].expanding = 1;
                self.recurse_doc(doc, &url);
                // urlTab might be reallocated.
                self.url_tab[cache_nr].expanding = 0;
                doc
            };

            // loaded:
            if let Some(fragment) = fragment {
                #[cfg(feature = "xpointer")]
                {
                    // Computes the XPointer expression and make a copy used
                    // as the replacement copy.

                    if self.is_stream != 0 && doc == self.doc {
                        xml_xinclude_err!(
                            self,
                            self.inc_tab[ref_index].elem.map(|node| node.into()),
                            XmlParserErrors::XmlXIncludeXPtrFailed,
                            "XPointer expressions not allowed in streaming mode\n"
                        );
                        return ret;
                    }

                    let xptrctxt: XmlXPathContextPtr = xml_xptr_new_context(Some(doc), None, None);
                    if xptrctxt.is_null() {
                        xml_xinclude_err!(
                            self,
                            self.inc_tab[ref_index].elem.map(|node| node.into()),
                            XmlParserErrors::XmlXIncludeXPtrFailed,
                            "could not create XPointer context\n"
                        );
                        return ret;
                    }
                    let xptr: XmlXPathObjectPtr = xml_xptr_eval(&fragment, xptrctxt);
                    if xptr.is_null() {
                        xml_xinclude_err!(
                            self,
                            self.inc_tab[ref_index].elem.map(|node| node.into()),
                            XmlParserErrors::XmlXIncludeXPtrFailed,
                            "XPointer evaluation failed: #{}\n",
                            fragment
                        );
                        xml_xpath_free_context(xptrctxt);
                        return ret;
                    }
                    match (*xptr).typ {
                        XmlXPathObjectType::XPathUndefined
                        | XmlXPathObjectType::XPathBoolean
                        | XmlXPathObjectType::XPathNumber
                        | XmlXPathObjectType::XPathString
                        | XmlXPathObjectType::XPathUsers
                        | XmlXPathObjectType::XPathXSLTTree => {
                            xml_xinclude_err!(
                                self,
                                self.inc_tab[ref_index].elem.map(|node| node.into()),
                                XmlParserErrors::XmlXIncludeXPtrResult,
                                "XPointer is not a range: #{}\n",
                                fragment
                            );
                            xml_xpath_free_object(xptr);
                            xml_xpath_free_context(xptrctxt);
                            return ret;
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XPathPoint => {
                            xml_xinclude_err!(
                                self,
                                self.inc_tab[ref_index].elem.map(|node| node.into()),
                                XmlParserErrors::XmlXIncludeXPtrResult,
                                "XPointer is not a range: #{}\n",
                                fragment
                            );
                            xml_xpath_free_object(xptr);
                            xml_xpath_free_context(xptrctxt);
                            return ret;
                        }
                        XmlXPathObjectType::XPathNodeset => {
                            if (*xptr).nodesetval.as_deref().is_none_or(|n| n.is_empty()) {
                                xml_xpath_free_object(xptr);
                                xml_xpath_free_context(xptrctxt);
                                return ret;
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
                                    xml_xinclude_err!(
                                        self,
                                        self.inc_tab[ref_index].elem.map(|node| node.into()),
                                        XmlParserErrors::XmlXIncludeXPtrResult,
                                        "XPointer selects an attribute: #{}\n",
                                        fragment
                                    );
                                    set.node_tab.swap_remove(i);
                                    continue;
                                }
                                XmlElementType::XmlNamespaceDecl => {
                                    xml_xinclude_err!(
                                        self,
                                        self.inc_tab[ref_index].elem.map(|node| node.into()),
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
                                    xml_xinclude_err!(
                                        self,
                                        self.inc_tab[ref_index].elem.map(|node| node.into()),
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
                    self.inc_tab[ref_index].inc = self.copy_xpointer(xptr);
                    xml_xpath_free_object(xptr);
                    xml_xpath_free_context(xptrctxt);
                }
            } else {
                // Add the top children list as the replacement copy.
                self.inc_tab[ref_index].inc =
                    xml_doc_copy_node(doc.get_root_element().unwrap().into(), Some(self.doc), 1)
                        .and_then(|node| XmlNodePtr::try_from(node).ok());
            }

            // Do the xml:base fixup if needed
            if doc.parse_flags & XmlParserOption::XmlParseNoBasefix as i32 == 0
                && self.parse_flags & XmlParserOption::XmlParseNoBasefix as i32 == 0
            {
                // The base is only adjusted if "necessary", i.e. if the xinclude node
                // has a base specified, or the URL is relative
                let mut base = self.inc_tab[ref_index]
                    .elem
                    .unwrap()
                    .get_ns_prop("base", XML_XML_NAMESPACE.to_str().ok());
                if base.is_none() {
                    // No xml:base on the xinclude node, so we check whether the
                    // URI base is different than (relative to) the context base
                    if let Some(cur_base) = build_relative_uri(&url, self.base.as_deref()) {
                        // If the URI doesn't contain a slash, it's not relative
                        if cur_base.contains('/') {
                            base = Some(cur_base.into_owned());
                        }
                    } else {
                        // Error return
                        xml_xinclude_err!(
                            self,
                            self.inc_tab[ref_index].elem.map(|node| node.into()),
                            XmlParserErrors::XmlXIncludeHrefURI,
                            "trying to build relative URI from {}\n",
                            url
                        );
                    }
                }
                if let Some(base) = base {
                    // Adjustment may be needed
                    let mut node = self.inc_tab[ref_index].inc;
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
                                    cur_node.set_base(Some(&base));
                                } else {
                                    // If the element already has an xml:base set,
                                    // then relativise it if necessary

                                    if let Some(xml_base) = cur_node
                                        .get_ns_prop("base", XML_XML_NAMESPACE.to_str().ok())
                                    {
                                        let rel_base = build_uri(&xml_base, &base);
                                        if let Some(rel_base) = rel_base {
                                            cur_node.set_base(Some(&rel_base));
                                        } else {
                                            // error
                                            xml_xinclude_err!(
                                                self,
                                                self.inc_tab[ref_index]
                                                    .elem
                                                    .map(|node| node.into()),
                                                XmlParserErrors::XmlXIncludeHrefURI,
                                                "trying to rebuild base from {}\n",
                                                xml_base
                                            );
                                        }
                                    }
                                }
                            } else {
                                // If no current base, set it
                                cur_node.set_base(Some(&base));
                            }
                        }
                        node = cur_node
                            .next
                            .map(|node| XmlNodePtr::try_from(node).unwrap());
                    }
                }
            }
            0
        }
    }

    /// Load the content, and store the result in the XInclude context
    ///
    /// Returns 0 in case of success, -1 in case of failure
    #[doc(alias = "xmlXIncludeLoadTxt")]
    unsafe fn load_txt(&mut self, mut url: &str, ref_index: usize) -> i32 {
        unsafe {
            let mut i: i32;
            let mut ret: i32 = -1;
            let mut enc = XmlCharEncoding::None;

            // Don't read from stdin.
            if url == "-" {
                url = "./-";
            }

            // Check the URL and remove any fragment identifier
            let Some(uri) = XmlURI::parse(url) else {
                xml_xinclude_err!(
                    self,
                    self.inc_tab[ref_index].elem.map(|node| node.into()),
                    XmlParserErrors::XmlXIncludeHrefURI,
                    "invalid value URI {}\n",
                    url
                );
                return ret;
            };
            if let Some(fragment) = uri.fragment.as_deref() {
                xml_xinclude_err!(
                    self,
                    self.inc_tab[ref_index].elem.map(|node| node.into()),
                    XmlParserErrors::XmlXIncludeTextFragment,
                    "fragment identifier forbidden for text: {}\n",
                    fragment
                );
                return ret;
            }
            let url = uri.save();

            // Handling of references to the local document are done directly through (*ctxt).doc.
            if url.is_empty() {
                xml_xinclude_err!(
                    self,
                    self.inc_tab[ref_index].elem.map(|node| node.into()),
                    XmlParserErrors::XmlXIncludeTextDocument,
                    "text serialization of document not available\n"
                );
                return ret;
            }

            // Prevent reloading the document twice.
            for txt in &self.txt_tab {
                if *url == *txt.url {
                    let text = CString::new(&*txt.text).unwrap();
                    let node = xml_new_doc_text(Some(self.doc), text.as_ptr() as *const u8);
                    self.inc_tab[ref_index].inc = node;
                    return 0;
                }
            }

            // Try to get the encoding if available
            let mut encoding = None;
            if let Some(elem) = self.inc_tab[ref_index].elem {
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
                            self,
                            self.inc_tab[ref_index].elem.map(|node| node.into()),
                            XmlParserErrors::XmlXIncludeUnknownEncoding,
                            "encoding {} not supported\n",
                            encoding
                        );
                        return ret;
                    }
                }
            }

            // Load it.
            let pctxt = xml_new_parser_ctxt();
            let Some(mut input_stream) = xml_load_external_entity(Some(&url), None, pctxt) else {
                xml_free_parser_ctxt(pctxt);
                return ret;
            };
            let Some(buf) = input_stream.buf.as_mut() else {
                xml_free_parser_ctxt(pctxt);
                return ret;
            };
            buf.borrow_mut().encoder = get_encoding_handler(enc);
            let Some(mut node) = xml_new_doc_text(Some(self.doc), null_mut()) else {
                let node = self.inc_tab[ref_index].elem.map(|node| node.into());
                xml_xinclude_err_memory(Some(self), node, None);
                xml_free_parser_ctxt(pctxt);
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
                    xml_xinclude_err!(
                        self,
                        self.inc_tab[ref_index].elem.map(|node| node.into()),
                        XmlParserErrors::XmlXIncludeInvalidChar,
                        "{} contains invalid char\n",
                        url
                    );
                    // goto error;
                    xml_free_node(node);
                    xml_free_parser_ctxt(pctxt);
                    return ret;
                }

                i += l;
            }

            node.add_content_len(content, len);

            self.txt_tab.push(XmlXIncludeTxt {
                text: CStr::from_ptr(node.content as *const i8)
                    .to_string_lossy()
                    .into_owned()
                    .into_boxed_str(),
                url: url.into_boxed_str(),
            });

            // loaded:
            // Add the element as the replacement copy.
            self.inc_tab[ref_index].inc = Some(node);
            ret = 0;

            // error:
            xml_free_parser_ctxt(pctxt);
            ret
        }
    }

    /// Load the content of the fallback node, and store the result in the XInclude context
    ///
    /// Returns 0 in case of success, -1 in case of failure
    #[doc(alias = "xmlXIncludeLoadFallback")]
    unsafe fn load_fallback(&mut self, fallback: XmlNodePtr, ref_index: usize) -> i32 {
        unsafe {
            let mut ret: i32 = 0;

            if fallback.element_type() == XmlElementType::XmlNamespaceDecl {
                return -1;
            }
            if fallback.children().is_some() {
                // It's possible that the fallback also has 'includes'
                // (Bug 129969), so we re-process the fallback just in case
                let old_nb_errors = self.nb_errors;
                self.inc_tab[ref_index].inc = self.copy_node(fallback, 1);
                if self.nb_errors > old_nb_errors {
                    ret = -1;
                } else if self.inc_tab[ref_index].inc.is_none() {
                    self.inc_tab[ref_index].empty_fb = 1;
                }
            } else {
                self.inc_tab[ref_index].inc = None;
                self.inc_tab[ref_index].empty_fb = 1; /* flag empty callback */
            }
            self.inc_tab[ref_index].fallback = 1;
            ret
        }
    }

    /// Find and load the infoset replacement for the given node.
    ///
    /// Returns 0 if substitution succeeded, -1 if some processing failed
    #[doc(alias = "xmlXIncludeLoadNode")]
    unsafe fn load_node(&mut self, ref_index: usize) -> i32 {
        unsafe {
            let mut xml: i32 = 1; /* default Issue 64 */
            let mut ret: i32;

            if ref_index == usize::MAX {
                return -1;
            }
            let Some(cur) = self.inc_tab[ref_index].elem else {
                return -1;
            };

            // read the attributes
            let href = self.get_prop(cur, XINCLUDE_HREF).unwrap_or("".to_owned());
            let parse = self.get_prop(cur, XINCLUDE_PARSE);
            if let Some(parse) = parse {
                if parse == XINCLUDE_PARSE_XML {
                    xml = 1;
                } else if parse == XINCLUDE_PARSE_TEXT {
                    xml = 0;
                } else {
                    xml_xinclude_err!(
                        self,
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
            let mut uri = if let Some(b) = cur.get_base(Some(self.doc)) {
                base = Some(b);
                build_uri(&href, base.as_deref().unwrap())
            } else {
                self.doc
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
                    self,
                    Some(cur.into()),
                    XmlParserErrors::XmlXIncludeHrefURI,
                    "failed build URL\n"
                );
                return -1;
            };

            // Save the base for this include (saving the current one)
            let old_base = self.base.take();
            self.base = base.map(|base| base.into());

            if xml != 0 {
                ret = self.load_doc(&uri, ref_index);
                // xmlXIncludeGetFragment(self, cur, URI);
            } else {
                ret = self.load_txt(&uri, ref_index);
            }

            // Restore the original base before checking for fallback
            self.base = old_base;

            if ret < 0 {
                // Time to try a fallback if available
                let mut children = cur.children.map(|c| XmlNodePtr::try_from(c).unwrap());
                while let Some(cur_node) = children {
                    if cur_node.element_type() == XmlElementType::XmlElementNode
                        && cur_node.name().as_deref() == Some(XINCLUDE_FALLBACK)
                        && cur_node.ns.is_some_and(|ns| {
                            ns.href().as_deref() == Some(XINCLUDE_NS)
                                || ns.href().as_deref() == Some(XINCLUDE_OLD_NS)
                        })
                    {
                        ret = self.load_fallback(cur_node, ref_index);
                        break;
                    }
                    children = cur_node
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                }
            }
            if ret < 0 {
                xml_xinclude_err!(
                    self,
                    Some(cur.into()),
                    XmlParserErrors::XmlXIncludeNoFallback,
                    "could not load {}, and no fallback was found\n",
                    uri
                );
            }

            0
        }
    }

    /// Parse a document for XInclude
    #[doc(alias = "xmlXIncludeParseFile")]
    unsafe fn parse_file(&mut self, mut url: &str) -> Option<XmlDocPtr> {
        unsafe {
            xml_init_parser();

            let pctxt = xml_new_parser_ctxt();
            if pctxt.is_null() {
                xml_xinclude_err_memory(Some(self), None, Some("cannot allocate parser context"));
                return None;
            }

            // pass in the application data to the parser context.
            (*pctxt)._private = self._private;

            xml_ctxt_use_options(
                pctxt,
                self.parse_flags | XmlParserOption::XmlParseDTDLoad as i32,
            );

            // Don't read from stdin.
            if url == "-" {
                url = "./-";
            }

            let Some(input_stream) = xml_load_external_entity(Some(url), None, pctxt) else {
                xml_free_parser_ctxt(pctxt);
                return None;
            };

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
    }

    /// Implement the XInclude substitution on the XML document @doc
    ///
    /// Returns 0 if no substitution were done, -1 if some processing failed
    /// or the number of substitutions done.
    #[doc(alias = "xmlXIncludeDoProcess")]
    unsafe fn do_process(&mut self, tree: XmlNodePtr) -> i32 {
        unsafe {
            let mut ret: i32 = 0;

            if tree.element_type() == XmlElementType::XmlNamespaceDecl {
                return -1;
            }

            // First phase: lookup the elements in the document
            let start = self.inc_tab.len();
            let mut cur = tree;
            'main: while {
                'inner: {
                    // TODO: need to work on entities -> stack
                    if self.test_node(cur) == 1 {
                        let ref_index = self.expand_node(cur);
                        // Mark direct includes.
                        if ref_index != usize::MAX {
                            self.inc_tab[ref_index].replace = 1;
                        }
                    } else if let Some(children) = cur
                        .children()
                        .filter(|_| {
                            matches!(
                                cur.element_type(),
                                XmlElementType::XmlDocumentNode | XmlElementType::XmlElementNode
                            )
                        })
                        .map(|children| XmlNodePtr::try_from(children).unwrap())
                    {
                        cur = children;
                        break 'inner;
                    }
                    'b: loop {
                        if cur == tree {
                            break 'main;
                        }
                        if let Some(next) = cur.next.map(|node| XmlNodePtr::try_from(node).unwrap())
                        {
                            cur = next;
                            break 'b;
                        }
                        let Some(next) = cur.parent.map(|p| XmlNodePtr::try_from(p).unwrap())
                        else {
                            break 'main;
                        };

                        cur = next;
                    }
                }

                cur != tree
            } {}

            // Second phase: extend the original document infoset.
            let len = self.inc_tab.len();
            for i in start..len {
                if self.inc_tab[i].replace != 0 {
                    if self.inc_tab[i].inc.is_some() || self.inc_tab[i].empty_fb != 0 {
                        // (empty fallback)
                        self.include_node(i);
                    }
                    self.inc_tab[i].replace = 0;
                } else {
                    // Ignore includes which were added indirectly, for example
                    // inside xi:fallback elements.
                    if let Some(inc) = self.inc_tab[i].inc.take() {
                        xml_free_node_list(Some(inc));
                    }
                }
                ret += 1;
            }

            if self.is_stream != 0 {
                // incTab references nodes which will eventually be deleted in
                // streaming mode. The table is only required for XPointer
                // expressions which aren't allowed in streaming mode.
                self.inc_tab.clear();
            }

            ret
        }
    }

    /// Implement the XInclude substitution for the given subtree reusing
    /// the information and data coming from the given context.
    ///
    /// Returns 0 if no substitution were done, -1 if some processing failed
    /// or the number of substitutions done.
    #[doc(alias = "xmlXIncludeProcessNode")]
    pub unsafe fn process_node(&mut self, node: XmlNodePtr) -> i32 {
        unsafe {
            if node.element_type() == XmlElementType::XmlNamespaceDecl || node.doc.is_none() {
                return -1;
            }
            let mut ret = self.do_process(node);
            if ret >= 0 && self.nb_errors > 0 {
                ret = -1;
            }
            ret
        }
    }
}

impl Drop for XmlXIncludeCtxt {
    /// Free an XInclude context
    #[doc(alias = "xmlXIncludeFreeContext")]
    fn drop(&mut self) {
        for inc_doc in self.url_tab.drain(..) {
            if let Some(doc) = inc_doc.doc {
                unsafe {
                    xml_free_doc(doc);
                }
            }
        }
    }
}

/// Implement the XInclude substitution on the XML document @doc
///
/// Returns 0 if no substitution were done, -1 if some processing failed
/// or the number of substitutions done.
#[doc(alias = "xmlXIncludeProcess")]
pub unsafe fn xml_xinclude_process(doc: XmlDocPtr) -> i32 {
    unsafe { xml_xinclude_process_flags(doc, 0) }
}

/// Implement the XInclude substitution on the XML document @doc
///
/// Returns 0 if no substitution were done, -1 if some processing failed
/// or the number of substitutions done.
#[doc(alias = "xmlXIncludeProcessFlags")]
pub unsafe fn xml_xinclude_process_flags(doc: XmlDocPtr, flags: i32) -> i32 {
    unsafe { xml_xinclude_process_flags_data(doc, flags, null_mut()) }
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
    unsafe {
        let Some(tree) = doc.get_root_element() else {
            return -1;
        };
        xml_xinclude_process_tree_flags_data(tree, flags, data)
    }
}

const XINCLUDE_MAX_DEPTH: i32 = 40;

/// Handle an out of memory condition
#[doc(alias = "xmlXIncludeErrMemory")]
unsafe fn xml_xinclude_err_memory(
    ctxt: Option<&mut XmlXIncludeCtxt>,
    node: Option<XmlGenericNodePtr>,
    extra: Option<&str>,
) {
    let mut ptr = null_mut();
    if let Some(ctxt) = ctxt {
        ctxt.nb_errors += 1;
        ptr = ctxt as *mut XmlXIncludeCtxt;
    }
    if let Some(extra) = extra {
        __xml_raise_error!(
            None,
            None,
            None,
            ptr as _,
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
            ptr as _,
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

/// Returns the @n'th element child of @cur or NULL
#[doc(alias = "xmlXIncludeGetNthChild")]
#[cfg(feature = "libxml_xptr_locs")]
fn xml_xinclude_get_nth_child(cur: XmlGenericNodePtr, no: i32) -> Option<XmlGenericNodePtr> {
    if cur.element_type() == XmlElementType::XmlNamespaceDecl {
        return None;
    }
    let mut cur = cur.children();
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

        cur = now.next();
    }
    cur
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
    unsafe {
        if tree.element_type() == XmlElementType::XmlNamespaceDecl {
            return -1;
        }
        let Some(doc) = tree.doc else {
            return -1;
        };

        let mut ctxt = XmlXIncludeCtxt::new(doc);
        ctxt._private = data;
        ctxt.base = doc.url.as_deref().map(|url| url.into());
        ctxt.set_flags(flags);
        let mut ret = ctxt.do_process(tree);
        if ret >= 0 && ctxt.nb_errors > 0 {
            ret = -1;
        }

        ret
    }
}

/// Implement the XInclude substitution for the given subtree
///
/// Returns 0 if no substitution were done, -1 if some processing failed
/// or the number of substitutions done.
#[doc(alias = "xmlXIncludeProcessTree")]
pub unsafe fn xml_xinclude_process_tree(tree: XmlNodePtr) -> i32 {
    unsafe { xml_xinclude_process_tree_flags(tree, 0) }
}

/// Implement the XInclude substitution for the given subtree
///
/// Returns 0 if no substitution were done, -1 if some processing failed
/// or the number of substitutions done.
#[doc(alias = "xmlXIncludeProcessTreeFlags")]
pub unsafe fn xml_xinclude_process_tree_flags(tree: XmlNodePtr, flags: i32) -> i32 {
    unsafe {
        if tree.element_type() == XmlElementType::XmlNamespaceDecl {
            return -1;
        }
        let Some(doc) = tree.doc else {
            return -1;
        };
        let mut ctxt = XmlXIncludeCtxt::new(doc);
        ctxt.base = tree.get_base(Some(doc)).map(|base| base.into());
        ctxt.set_flags(flags);
        let mut ret = ctxt.do_process(tree);
        if ret >= 0 && ctxt.nb_errors > 0 {
            ret = -1;
        }

        ret
    }
}
