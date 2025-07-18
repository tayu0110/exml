//! Provide methods and data structures for tree manipulation.
//!
//! This module is based on `libxml/tree.h`, `tree.c`, and so on in `libxml2-v2.11.8`.  
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: interfaces for tree manipulation
// Description: this module describes the structures found in an tree resulting
//              from an XML or HTML parsing, as well as the API provided for
//              various processing on that tree
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// tree.c : implementation of access function for an XML tree.
//
// References:
//   XHTML 1.0 W3C REC: http://www.w3.org/TR/2002/REC-xhtml1-20020801/
//
// See Copyright for the status of this software.
//
// daniel@veillard.com

mod attribute;
mod document;
mod dom_wrapper;
mod dtd;
#[cfg(feature = "libxml_output")]
mod dump;
mod entities;
mod generic;
mod id;
mod namespace;
mod node;

use std::{
    borrow::Cow,
    fmt::Display,
    sync::atomic::{AtomicBool, AtomicI32, Ordering},
};

use crate::{
    chvalid::XmlCharValid,
    error::{__xml_simple_error, __xml_simple_oom_error, XmlErrorDomain, XmlParserErrors},
    globals::{get_deregister_node_func, get_register_node_func},
    parser::{XML_STRING_COMMENT, XML_STRING_TEXT},
};

pub use attribute::*;
pub use document::*;
pub use dom_wrapper::*;
pub use dtd::*;
pub use entities::*;
pub use generic::*;
pub use id::*;
pub use namespace::*;
pub use node::*;

/// default buffer size 4000.
pub const BASE_BUFFER_SIZE: usize = 4096;

/// A buffer allocation scheme can be defined to either match exactly the
/// need or double it's allocated size each time it is found too small.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlBufferAllocationScheme {
    XmlBufferAllocDoubleit,  /* double each time one need to grow */
    XmlBufferAllocExact,     /* grow only to the minimal size */
    XmlBufferAllocImmutable, /* immutable buffer, deprecated */
    XmlBufferAllocIo,        /* special allocation scheme used for I/O */
    XmlBufferAllocHybrid,    /* exact up to a threshold, and doubleit thereafter */
    XmlBufferAllocBounded,   /* limit the upper size of the buffer */
}

pub(crate) static __XML_REGISTER_CALLBACKS: AtomicI32 = AtomicI32::new(0);

/// This is the namespace for the special xml: prefix predefined in the
/// XML Namespace specification.
pub const XML_XML_NAMESPACE: &str = "http://www.w3.org/XML/1998/namespace";

/// This is the name for the special xml:id attribute
pub const XML_XML_ID: &str = "xml:id";

/// The different element types carried by an XML tree.
///
/// # NOTE
/// This is synchronized with DOM Level1 values.  
/// See <http://www.w3.org/TR/REC-DOM-Level-1/>
///
/// Actually this had diverged a bit, and now XML_DOCUMENT_TYPE_NODE should
/// be deprecated to use an XML_DTD_NODE.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum XmlElementType {
    #[default]
    XmlInvalidNode = 0, // for std::mem::zeroed(). This is invalid value.
    XmlElementNode = 1,
    XmlAttributeNode = 2,
    XmlTextNode = 3,
    XmlCDATASectionNode = 4,
    XmlEntityRefNode = 5,
    XmlEntityNode = 6,
    XmlPINode = 7,
    XmlCommentNode = 8,
    XmlDocumentNode = 9,
    XmlDocumentTypeNode = 10,
    XmlDocumentFragNode = 11,
    XmlNotationNode = 12,
    XmlHTMLDocumentNode = 13,
    XmlDTDNode = 14,
    XmlElementDecl = 15,
    XmlAttributeDecl = 16,
    XmlEntityDecl = 17,
    XmlNamespaceDecl = 18,
    XmlXIncludeStart = 19,
    XmlXIncludeEnd = 20,
}

/// A DTD Attribute type definition.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlAttributeType {
    XmlAttributeCDATA = 1,
    XmlAttributeID,
    XmlAttributeIDREF,
    XmlAttributeIDREFS,
    XmlAttributeEntity,
    XmlAttributeEntities,
    XmlAttributeNmtoken,
    XmlAttributeNmtokens,
    XmlAttributeEnumeration,
    XmlAttributeNotation,
}

/// A DTD Attribute default definition.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlAttributeDefault {
    XmlAttributeNone = 1,
    XmlAttributeRequired,
    XmlAttributeImplied,
    XmlAttributeFixed,
}

/// The different possibilities for an element content type.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlElementTypeVal {
    XmlElementTypeUndefined = 0,
    XmlElementTypeEmpty = 1,
    XmlElementTypeAny,
    XmlElementTypeMixed,
    XmlElementTypeElement,
}

/// A namespace declaration node.
pub(crate) const XML_LOCAL_NAMESPACE: XmlElementType = XmlElementType::XmlNamespaceDecl;
pub type XmlNsType = XmlElementType;

/// Set of properties of the document as found by the parser.
///
/// Some of them are linked to similarly named `xmlParserOption`.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlDocProperties {
    XmlDocWellformed = 1 << 0, /* document is XML well formed */
    XmlDocNsvalid = 1 << 1,    /* document is Namespace valid */
    XmlDocOld10 = 1 << 2,      /* parsed with old XML-1.0 parser */
    XmlDocDTDValid = 1 << 3,   /* DTD validation was successful */
    XmlDocXInclude = 1 << 4,   /* XInclude substitution was done */
    XmlDocUserbuilt = 1 << 5,  /* Document was built using the API
                               and not by parsing an instance */
    XmlDocInternal = 1 << 6, /* built for internal processing */
    XmlDocHTML = 1 << 7,     /* parsed or built HTML document */
}

#[derive(Debug, Clone)]
pub struct InvalidNodePointerCastError {
    from: XmlElementType,
    to: &'static str,
}

impl Display for InvalidNodePointerCastError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Invalid casting from {:?} to {}", self.from, self.to)
    }
}

/// Check that a value conforms to the lexical space of NCName
///
/// Returns `Ok` if this validates, `Err` otherwise.
#[doc(alias = "xmlValidateNCName")]
#[cfg(any(
    feature = "libxml_tree",
    feature = "xpath",
    feature = "schema",
    feature = "html",
    feature = "sax1",
    feature = "libxml_writer",
))]
pub fn validate_ncname<const ALLOW_SPACE: bool>(value: &str) -> Result<(), &'static str> {
    use crate::parser::xml_is_letter;

    let mut cur = value;

    // First quick algorithm for ASCII range
    if ALLOW_SPACE {
        cur = cur.trim_start_matches(|c: char| c.is_xml_blank_char());
    }
    if cur.starts_with(|c: char| c.is_ascii_alphabetic() || c == '_') {
        cur = cur[1..].trim_start_matches(|c: char| {
            c.is_ascii_alphanumeric() || c == '_' || c == '-' || c == '.'
        });
        if ALLOW_SPACE {
            cur = cur.trim_start_matches(|c: char| c.is_xml_blank_char());
        }

        if cur.is_empty() {
            return Ok(());
        }
    }

    // try_complex:
    // Second check for chars outside the ASCII range
    let mut cur = value;
    if ALLOW_SPACE {
        cur = cur.trim_start_matches(|c: char| c.is_xml_blank_char());
    }
    let Some(mut cur) = cur.strip_prefix(|c: char| xml_is_letter(c as u32) || c == '_') else {
        return Err("Invalid NCName");
    };
    cur = cur.trim_start_matches(|c: char| {
        xml_is_letter(c as u32)
            || c.is_xml_digit()
            || c == '.'
            || c == '-'
            || c == '_'
            || c.is_xml_combining()
            || c.is_xml_extender()
    });
    if ALLOW_SPACE {
        cur = cur.trim_start_matches(|c: char| c.is_xml_blank_char());
    }

    if cur.is_empty() {
        Ok(())
    } else {
        Err("Invalid NCName")
    }
}

/// Check that a value conforms to the lexical space of QName
///
/// Returns `Ok` if this validates, `Err` otherwise.
#[doc(alias = "xmlValidateQName")]
#[cfg(any(feature = "libxml_tree", feature = "schema"))]
pub fn validate_qname<const ALLOW_SPACE: bool>(value: &str) -> Result<(), &'static str> {
    use crate::parser::xml_is_letter;

    let mut cur = value;

    // First quick algorithm for ASCII range
    if ALLOW_SPACE {
        cur = cur.trim_start_matches(|c: char| c.is_xml_blank_char());
    }

    if cur.starts_with(|c: char| c.is_ascii_alphabetic() || c == '_') {
        'try_complex: {
            cur = cur[1..].trim_start_matches(|c: char| {
                c.is_ascii_alphanumeric() || c == '_' || c == '-' || c == '.'
            });

            if let Some(local) = cur.strip_prefix(':') {
                let Some(rem) = local.strip_prefix(|c: char| c.is_ascii_alphabetic() || c == '_')
                else {
                    break 'try_complex;
                };
                cur = rem.trim_start_matches(|c: char| {
                    c.is_ascii_alphanumeric() || c == '_' || c == '-' || c == '.'
                });
            }
            if ALLOW_SPACE {
                cur = cur.trim_start_matches(|c: char| c.is_xml_blank_char());
            }
            if cur.is_empty() {
                return Ok(());
            }
        }
    }

    // try_complex:
    // Second check for chars outside the ASCII range
    let mut cur = value;
    if ALLOW_SPACE {
        cur = cur.trim_start_matches(|c: char| c.is_xml_blank_char());
    }
    let Some(mut cur) = cur.strip_prefix(|c: char| xml_is_letter(c as u32) || c == '_') else {
        return Err("Invalid QName");
    };
    cur = cur.trim_start_matches(|c: char| {
        xml_is_letter(c as u32)
            || c.is_xml_digit()
            || c == '.'
            || c == '-'
            || c == '_'
            || c.is_xml_combining()
            || c.is_xml_extender()
    });
    if let Some(local) = cur.strip_prefix(':') {
        let Some(rem) = local.strip_prefix(|c: char| xml_is_letter(c as u32) || c == '_') else {
            return Err("Invalid QName");
        };
        cur = rem.trim_start_matches(|c: char| {
            xml_is_letter(c as u32)
                || c.is_xml_digit()
                || c == '.'
                || c == '-'
                || c == '_'
                || c.is_xml_combining()
                || c.is_xml_extender()
        });
    }
    if ALLOW_SPACE {
        cur = cur.trim_start_matches(|c: char| c.is_xml_blank_char());
    }
    if cur.is_empty() {
        Ok(())
    } else {
        Err("Invalid QName")
    }
}

/// Check that a value conforms to the lexical space of Name
///
/// Returns `Ok` if this validates, `Err` otherwise.
#[doc(alias = "xmlValidateName")]
#[cfg(any(feature = "libxml_tree", feature = "schema"))]
pub fn validate_name<const ALLOW_SPACE: bool>(value: &str) -> Result<(), &'static str> {
    use crate::parser::xml_is_letter;

    let mut cur = value;

    // First quick algorithm for ASCII range
    if ALLOW_SPACE {
        cur = cur.trim_start_matches(|c: char| c.is_xml_blank_char());
    }
    if cur.starts_with(|c: char| c.is_ascii_alphabetic() || c == '_' || c == ':') {
        cur = cur[1..].trim_start_matches(|c: char| {
            c.is_ascii_alphanumeric() || c == '_' || c == '-' || c == '.' || c == ':'
        });
        if ALLOW_SPACE {
            cur = cur.trim_start_matches(|c: char| c.is_xml_blank_char());
        }
        if cur.is_empty() {
            return Ok(());
        }
    }

    // try_complex:
    // Second check for chars outside the ASCII range
    let mut cur = value;
    if ALLOW_SPACE {
        cur = cur.trim_start_matches(|c: char| c.is_xml_blank_char());
    }
    let Some(mut cur) = cur.strip_prefix(|c: char| xml_is_letter(c as u32) || c == '_' || c == ':')
    else {
        return Err("Invalid Name");
    };
    cur = cur.trim_start_matches(|c: char| {
        xml_is_letter(c as u32)
            || c.is_xml_digit()
            || c == '.'
            || c == ':'
            || c == '-'
            || c == '_'
            || c.is_xml_combining()
            || c.is_xml_extender()
    });
    if ALLOW_SPACE {
        cur = cur.trim_start_matches(|c: char| c.is_xml_blank_char());
    }
    if cur.is_empty() {
        Ok(())
    } else {
        Err("Invalid Name")
    }
}

/// Check that a value conforms to the lexical space of NMToken
///
/// Returns `Ok` if this validates, `Err` otherwise.
#[doc(alias = "xmlValidateNMToken")]
#[cfg(any(feature = "libxml_tree", feature = "schema"))]
pub fn validate_nmtoken<const ALLOW_SPACE: bool>(value: &str) -> Result<(), &'static str> {
    use crate::parser::xml_is_letter;

    let mut cur = value;

    // First quick algorithm for ASCII range
    if ALLOW_SPACE {
        cur = cur.trim_start_matches(|c: char| c.is_xml_blank_char());
    }
    if cur.starts_with(|c: char| {
        c.is_ascii_alphanumeric() || c == '_' || c == '-' || c == '.' || c == ':'
    }) {
        cur = cur[1..].trim_start_matches(|c: char| {
            c.is_ascii_alphanumeric() || c == '_' || c == '-' || c == '.' || c == ':'
        });
        if ALLOW_SPACE {
            cur = cur.trim_start_matches(|c: char| c.is_xml_blank_char());
        }
        if cur.is_empty() {
            return Ok(());
        }
    }

    // try_complex:
    // Second check for chars outside the ASCII range
    let mut cur = value;
    if ALLOW_SPACE {
        cur = cur.trim_start_matches(|c: char| c.is_xml_blank_char());
    }
    let Some(mut cur) = cur.strip_prefix(|c: char| {
        xml_is_letter(c as u32)
            || c.is_xml_digit()
            || c == '.'
            || c == ':'
            || c == '-'
            || c == '_'
            || c.is_xml_combining()
            || c.is_xml_extender()
    }) else {
        return Err("Invalid NMToken");
    };
    cur = cur.trim_start_matches(|c: char| {
        xml_is_letter(c as u32)
            || c.is_xml_digit()
            || c == '.'
            || c == ':'
            || c == '-'
            || c == '_'
            || c.is_xml_combining()
            || c.is_xml_extender()
    });
    if ALLOW_SPACE {
        cur = cur.trim_start_matches(|c: char| c.is_xml_blank_char());
    }
    if cur.is_empty() {
        Ok(())
    } else {
        Err("Invalid NMToken")
    }
}

/// Handle an out of memory condition
#[doc(alias = "xmlTreeErrMemory")]
fn xml_tree_err_memory(extra: &str) {
    __xml_simple_oom_error(XmlErrorDomain::XmlFromTree, None, Some(extra));
}

/// This function tries to locate a namespace definition in a tree
/// ancestors, or create a new namespace definition node similar to
/// @ns trying to reuse the same prefix. However if the given prefix is
/// null (default namespace) or reused within the subtree defined by
/// @tree or on one of its ancestors then a new prefix is generated.
/// Returns the (new) namespace definition or null_mut() in case of error
#[doc(alias = "xmlNewReconciledNs")]
fn xml_new_reconciled_ns(
    doc: Option<XmlDocPtr>,
    mut tree: XmlNodePtr,
    ns: XmlNsPtr,
) -> Option<XmlNsPtr> {
    let mut counter: i32 = 1;

    // let tree = tree?;
    if !matches!(tree.element_type(), XmlElementType::XmlElementNode) {
        return None;
    }
    if !matches!(ns.typ, XmlElementType::XmlNamespaceDecl) {
        return None;
    }
    // Search an existing namespace definition inherited.
    if let Some(def) = tree.search_ns_by_href(doc, ns.href().as_deref().unwrap()) {
        return Some(def);
    }

    // Find a close prefix which is not already in use.
    // Let's strip namespace prefixes longer than 20 chars !
    let prefix = ns.prefix().unwrap_or(Cow::Borrowed("default"));
    let mut def = tree.search_ns(doc, Some(&prefix));
    while def.is_some() {
        if counter > 1000 {
            return None;
        }
        let prefix = format!("{prefix}{counter}");
        counter += 1;
        def = tree.search_ns(doc, Some(&prefix));
    }

    // OK, now we are ready to create a new one.
    xml_new_ns(Some(tree), ns.href.as_deref(), Some(&prefix))
}

// NOTE about the CopyNode operations !
//
// They are split into external and internal parts for one
// tricky reason: namespaces. Doing a direct copy of a node
// say RPM:Copyright without changing the namespace pointer to
// something else can produce stale links. One way to do it is
// to keep a reference counter but this doesn't work as soon
// as one moves the element or the subtree out of the scope of
// the existing namespace. The actual solution seems to be to add
// a copy of the namespace at the top of the copied tree if
// not available in the subtree.
// Hence two functions, the public front-end call the inner ones
// The argument "recursive" normally indicates a recursive copy
// of the node with values 0 (no) and 1 (yes).  For XInclude,
// however, we allow a value of 2 to indicate copy properties and
// namespace info, but don't recurse on children.
pub(crate) unsafe fn xml_static_copy_node(
    node: XmlGenericNodePtr,
    doc: Option<XmlDocPtr>,
    parent: Option<XmlGenericNodePtr>,
    extended: i32,
) -> Option<XmlGenericNodePtr> {
    unsafe {
        match node.element_type() {
            XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlElementNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlPINode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => {}
            XmlElementType::XmlAttributeNode => {
                let attr = XmlAttrPtr::try_from(node).unwrap();
                return xml_copy_prop_internal(
                    doc,
                    parent.and_then(|p| XmlNodePtr::try_from(p).ok()),
                    attr,
                )
                .map(|attr| attr.into());
            }
            XmlElementType::XmlNamespaceDecl => {
                let ns = XmlNsPtr::try_from(node).unwrap();
                return xml_copy_namespace_list(Some(ns)).map(|ns| ns.into());
            }

            #[cfg(feature = "libxml_tree")]
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                let doc = XmlDocPtr::try_from(node).unwrap();
                return xml_copy_doc(doc, extended).map(|doc| doc.into());
            }
            #[cfg(not(feature = "libxml_tree"))]
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                return None;
            }
            XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlDTDNode
            | XmlElementType::XmlElementDecl
            | XmlElementType::XmlAttributeDecl
            | XmlElementType::XmlEntityDecl => {
                return None;
            }
            _ => unreachable!(),
        }

        let mut node = XmlNodePtr::try_from(node).unwrap();

        // Allocate a new node and fill the fields.
        let Some(mut ret) = XmlNodePtr::new(XmlNode {
            typ: node.element_type(),
            doc,
            parent,
            ..Default::default()
        }) else {
            xml_tree_err_memory("copying node");
            return None;
        };
        ret.name = node.name.clone();
        if !matches!(node.element_type(), XmlElementType::XmlElementNode)
            && node.content.is_some()
            && !matches!(node.element_type(), XmlElementType::XmlEntityRefNode)
            && !matches!(node.element_type(), XmlElementType::XmlXIncludeEnd)
            && !matches!(node.element_type(), XmlElementType::XmlXIncludeStart)
        {
            ret.content = node.content.clone();
        } else if matches!(node.element_type(), XmlElementType::XmlElementNode) {
            ret.line = node.line;
        }
        if let Some(mut parent) = parent {
            // this is a tricky part for the node register thing:
            // in case ret does get coalesced in xmlAddChild
            // the deregister-node callback is called; so we register ret now already
            if let Some(register) = get_register_node_func() {
                register(ret.into());
            }

            // Note that since (*ret).parent is already set, xmlAddChild will
            // return early and not actually insert the node. It will only
            // coalesce text nodes and unnecessarily call xmlSetTreeDoc.
            // Assuming that the subtree to be copied always has its text
            // nodes coalesced, the somewhat confusing call to xmlAddChild
            // could be removed.
            let tmp = parent.add_child(ret.into());
            /* node could have coalesced */
            if tmp != Some(ret.into()) {
                return tmp;
            }
        }

        if extended == 0 {
            //  goto out;
            // if parent != null_mut() we already registered the node above
            if parent.is_none() {
                if let Some(register) = get_register_node_func() {
                    register(ret.into());
                }
            }

            return Some(ret.into());
        }
        if matches!(
            node.element_type(),
            XmlElementType::XmlElementNode | XmlElementType::XmlXIncludeStart
        ) && node.ns_def.is_some()
        {
            ret.ns_def = xml_copy_namespace_list(node.ns_def);
        }

        if let Some(node_ns) = node.ns {
            let prefix = node_ns.prefix();
            if let Some(ns) = ret.search_ns(doc, prefix.as_deref()) {
                // reference the existing namespace definition in our own tree.
                ret.ns = Some(ns);
            } else {
                // Humm, we are copying an element whose namespace is defined
                // out of the new tree scope. Search it in the original tree
                // and add it at the top of the new tree
                let node_doc = node.doc;
                if let Some(ns) = node.search_ns(node_doc, prefix.as_deref()) {
                    let mut root = XmlGenericNodePtr::from(ret);

                    while let Some(parent) = root.parent() {
                        root = parent;
                    }
                    ret.ns = xml_new_ns(
                        Some(XmlNodePtr::try_from(root).unwrap()),
                        ns.href.as_deref(),
                        ns.prefix().as_deref(),
                    );
                } else {
                    ret.ns = xml_new_reconciled_ns(doc, ret, node_ns);
                }
            }
        }
        if (matches!(node.element_type(), XmlElementType::XmlElementNode)
            || matches!(node.element_type(), XmlElementType::XmlXIncludeStart))
            && node.properties.is_some()
        {
            ret.properties = xml_copy_prop_list(Some(ret), node.properties);
        }
        if matches!(node.element_type(), XmlElementType::XmlEntityRefNode) {
            if doc.is_none_or(|doc| node.doc != Some(doc)) {
                // The copied node will go into a separate document, so
                // to avoid dangling references to the ENTITY_DECL node
                // we cannot keep the reference. Try to find it in the
                // target document.
                let children = xml_get_doc_entity(doc, &ret.name().unwrap());
                ret.set_children(children.map(|children| children.into()));
            } else {
                ret.set_children(node.children());
            }
            let children = ret.children();
            ret.set_last(children);
        } else if let Some(children) = node.children().filter(|_| extended != 2) {
            let mut cur = Some(children);
            let mut insert = XmlGenericNodePtr::from(ret);
            while let Some(mut now) = cur {
                let Some(mut copy) = xml_static_copy_node(now, doc, Some(insert), 2) else {
                    xml_free_node(ret);
                    return None;
                };

                // Check for coalesced text nodes
                if insert.last() != Some(copy) {
                    if let Some(mut last) = insert.last() {
                        copy.set_prev(Some(last));
                        last.set_next(Some(copy));
                    } else {
                        insert.set_children(Some(copy));
                    }
                    insert.set_last(Some(copy));
                }

                if let Some(children) = now
                    .children()
                    .filter(|_| !matches!(now.element_type(), XmlElementType::XmlEntityRefNode))
                {
                    cur = Some(children);
                    insert = copy;
                    continue;
                }

                cur = loop {
                    if let Some(next) = now.next() {
                        break Some(next);
                    }

                    now = now.parent().unwrap();
                    if now == XmlGenericNodePtr::from(node) {
                        break None;
                    }
                    insert = insert.parent().unwrap();
                };
            }
        }

        //  out:
        /* if parent != null_mut() we already registered the node above */
        if parent.is_none() {
            if let Some(register) = get_register_node_func() {
                register(ret.into());
            }
        }

        Some(ret.into())
    }
}

pub(crate) unsafe fn xml_static_copy_node_list(
    mut node: Option<XmlGenericNodePtr>,
    doc: Option<XmlDocPtr>,
    parent: Option<XmlGenericNodePtr>,
) -> Option<XmlGenericNodePtr> {
    unsafe {
        let mut ret = None;
        let mut p = None;

        while let Some(cur_node) = node {
            #[cfg(feature = "libxml_tree")]
            let q = if matches!(cur_node.element_type(), XmlElementType::XmlDTDNode) {
                let Some(mut doc) = doc else {
                    node = cur_node.next();
                    continue;
                };
                if let Some(int_subset) = doc.int_subset {
                    let q = int_subset;
                    parent.unwrap().add_child(q.into());
                    Some(q.into())
                } else {
                    let Some(mut new) = xml_copy_dtd(XmlDtdPtr::try_from(cur_node).unwrap()) else {
                        // goto error;
                        xml_free_node_list(ret);
                        return None;
                    };
                    new.doc = Some(doc);
                    new.set_parent(parent);
                    doc.int_subset = Some(new);
                    parent.unwrap().add_child(new.into());
                    Some(new.into())
                }
            } else {
                xml_static_copy_node(cur_node, doc, parent, 1)
            };
            #[cfg(not(feature = "libxml_tree"))]
            let q = xml_static_copy_node(node, doc, parent, 1);
            let Some(mut q) = q else {
                // goto error;
                xml_free_node_list(ret);
                return None;
            };
            if ret.is_none() {
                q.set_prev(None);
                ret = Some(q);
                p = Some(q);
            } else if let Some(mut pr) = p.filter(|&p| p != q) {
                // the test is required if xmlStaticCopyNode coalesced 2 text nodes
                pr.set_next(Some(q));
                q.set_prev(Some(pr));
                p = Some(q);
            }
            node = cur_node.next();
        }
        ret
    }
}

/// Do a copy of the dtd.
///
/// Returns: a new #xmlDtdPtr, or null_mut() in case of error.
#[doc(alias = "xmlCopyDtd")]
#[cfg(feature = "libxml_tree")]
pub unsafe fn xml_copy_dtd(dtd: XmlDtdPtr) -> Option<XmlDtdPtr> {
    unsafe {
        use crate::valid::xml_get_dtd_qelement_desc;

        let mut ret = xml_new_dtd(
            None,
            dtd.name().as_deref(),
            dtd.external_id.as_deref(),
            dtd.system_id.as_deref(),
        )?;
        ret.entities = dtd
            .entities
            .iter()
            .map(|(k, v)| (k.clone(), xml_copy_entity(*v).unwrap()))
            .collect();
        ret.notations = dtd.notations.clone();
        ret.elements = dtd
            .elements
            .iter()
            .map(|(k, v)| (k.clone(), xml_copy_element(*v).unwrap()))
            .collect();
        ret.attributes = dtd
            .attributes
            .iter()
            .map(|(k, v)| {
                (
                    k.clone(),
                    XmlAttributePtr::new(XmlAttribute {
                        typ: XmlElementType::XmlAttributeDecl,
                        name: v.name.clone(),
                        atype: v.atype,
                        def: v.def,
                        default_value: v.default_value.clone(),
                        tree: v.tree.clone(),
                        prefix: v.prefix.clone(),
                        elem: v.elem.clone(),
                        ..Default::default()
                    })
                    .unwrap(),
                )
            })
            .collect();
        ret.pentities = dtd
            .pentities
            .iter()
            .map(|(k, v)| (k.clone(), xml_copy_entity(*v).unwrap()))
            .collect();

        let mut cur = dtd.children;
        let mut p: Option<XmlGenericNodePtr> = None;
        while let Some(cur_node) = cur {
            let mut q = None;

            if let Ok(tmp) = XmlEntityPtr::try_from(cur_node) {
                match tmp.etype {
                    XmlEntityType::XmlInternalGeneralEntity
                    | XmlEntityType::XmlExternalGeneralParsedEntity
                    | XmlEntityType::XmlExternalGeneralUnparsedEntity => {
                        q = ret
                            .get_entity(tmp.name().as_deref().unwrap())
                            .map(XmlGenericNodePtr::from);
                    }
                    XmlEntityType::XmlInternalParameterEntity
                    | XmlEntityType::XmlExternalParameterEntity => {
                        q = ret
                            .get_parameter_entity(tmp.name().as_deref().unwrap())
                            .map(XmlGenericNodePtr::from);
                    }
                    XmlEntityType::XmlInternalPredefinedEntity => {}
                }
            } else if let Ok(tmp) = XmlElementPtr::try_from(cur_node) {
                q = xml_get_dtd_qelement_desc(
                    Some(ret),
                    tmp.name().as_deref().unwrap(),
                    tmp.prefix.as_deref(),
                )
                .map(XmlGenericNodePtr::from);
            } else if let Ok(tmp) = XmlAttributePtr::try_from(cur_node) {
                q = ret
                    .get_qattr_desc(
                        tmp.elem.as_deref().unwrap(),
                        tmp.name().as_deref().unwrap(),
                        tmp.prefix.as_deref(),
                    )
                    .map(XmlGenericNodePtr::from);
            } else if matches!(cur_node.element_type(), XmlElementType::XmlCommentNode) {
                q = xml_copy_node(cur_node, 0);
            }

            let Some(mut q) = q else {
                cur = cur_node.next();
                continue;
            };

            if let Some(mut p) = p {
                p.set_next(Some(q));
            } else {
                ret.children = Some(q);
            }

            q.set_prev(p);
            q.set_parent(Some(ret.into()));
            q.set_next(None);
            ret.last = Some(q);
            p = Some(q);
            cur = cur_node.next();
        }

        Some(ret)
    }
}

/// Creation of a new node element within a document. @ns and @content
/// are optional (null_mut()).
/// NOTE: @content is supposed to be a piece of XML CDATA, so it allow entities
///       references, but XML special chars need to be escaped first by using
///       xmlEncodeEntitiesReentrant(). Use xmlNewDocRawNode() if you don't
///       need entities support.
///
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewDocNode")]
pub fn xml_new_doc_node(
    doc: Option<XmlDocPtr>,
    ns: Option<XmlNsPtr>,
    name: &str,
    content: Option<&str>,
) -> Option<XmlNodePtr> {
    let cur = xml_new_node(ns, name);
    if let Some(mut cur) = cur {
        cur.doc = doc;
        if let Some(content) = content {
            cur.set_children(
                doc.and_then(|doc| doc.get_node_list(content).map(|node| node.into())),
            );
            if let Some(mut ulccur) = cur.children() {
                while let Some(next) = ulccur.next() {
                    ulccur.set_parent(Some(cur.into()));
                    ulccur = next;
                }
                (*ulccur).set_parent(Some(cur.into()));
                cur.set_last(Some(ulccur));
            } else {
                cur.set_last(None);
            }
        }
    }

    cur
}

/// Creation of a new node element. @ns is optional (null_mut()).
///
/// Use of this function is DISCOURAGED in favor of xmlNewDocNode.
///
/// Returns a pointer to the new node object. Uses xml_strdup() to make copy of @name.
#[doc(alias = "xmlNewNode")]
pub fn xml_new_node(ns: Option<XmlNsPtr>, name: &str) -> Option<XmlNodePtr> {
    // Allocate a new node and fill the fields.
    let Some(cur) = XmlNodePtr::new(XmlNode {
        typ: XmlElementType::XmlElementNode,
        name: name.to_owned().into(),
        ns,
        ..Default::default()
    }) else {
        xml_tree_err_memory("building node");
        return None;
    };

    if let Some(register) = get_register_node_func() {
        register(cur.into());
    }

    Some(cur)
}

/// Creation of a new child element, added at the end of @parent children list.
/// @ns and @content parameters are optional (NULL). If @ns is NULL, the newly
/// created element inherits the namespace of @parent. If @content is non NULL,
/// a child list containing the TEXTs and ENTITY_REFs node will be created.
/// NOTE: @content is supposed to be a piece of XML CDATA, so it allows entity
///       references. XML special chars must be escaped first by using
///       xmlEncodeEntitiesReentrant(), or xmlNewTextChild() should be used.
///
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewChild")]
#[cfg(any(feature = "libxml_tree", feature = "schema"))]
pub fn xml_new_child(
    mut parent: XmlGenericNodePtr,
    ns: Option<XmlNsPtr>,
    name: &str,
    content: Option<&str>,
) -> Option<XmlNodePtr> {
    // Allocate a new node
    let mut cur = if let Some(node) = XmlNodePtr::try_from(parent)
        .ok()
        .filter(|node| node.element_type() == XmlElementType::XmlElementNode)
    {
        if ns.is_none() {
            xml_new_doc_node(node.doc, node.ns, name, content)
        } else {
            xml_new_doc_node(node.doc, ns, name, content)
        }
    } else if let Ok(parent) = XmlDocPtr::try_from(parent) {
        if ns.is_none() {
            xml_new_doc_node(Some(parent), None, name, content)
        } else {
            xml_new_doc_node(Some(parent), ns, name, content)
        }
    } else if matches!(parent.element_type(), XmlElementType::XmlDocumentFragNode) {
        xml_new_doc_node(parent.document(), ns, name, content)
    } else {
        None
    }?;

    // add the new element at the end of the children list.
    cur.typ = XmlElementType::XmlElementNode;
    cur.set_parent(Some(parent));
    cur.doc = parent.document();
    if parent.children().is_none() {
        parent.set_children(Some(cur.into()));
        parent.set_last(Some(cur.into()));
    } else {
        let mut prev = parent.last().unwrap();
        prev.set_next(Some(cur.into()));
        cur.prev = Some(prev);
        parent.set_last(Some(cur.into()));
    }

    Some(cur)
}

/// Creation of a new text node within a document.
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewDocText")]
pub fn xml_new_doc_text(doc: Option<XmlDocPtr>, content: Option<&str>) -> Option<XmlNodePtr> {
    let cur = xml_new_text(content);
    if let Some(mut cur) = cur {
        cur.doc = doc;
    }
    cur
}

/// Creation of a new text node.
///
/// Use of this function is DISCOURAGED in favor of xmlNewDocText.
///
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewText")]
pub fn xml_new_text(content: Option<&str>) -> Option<XmlNodePtr> {
    // Allocate a new node and fill the fields.
    let Some(mut cur) = XmlNodePtr::new(XmlNode {
        typ: XmlElementType::XmlTextNode,
        name: Cow::Borrowed(XML_STRING_TEXT),
        ..Default::default()
    }) else {
        xml_tree_err_memory("building text");
        return None;
    };
    cur.content = content.map(|content| content.to_owned());

    if let Some(register) = get_register_node_func() {
        register(cur.into());
    }

    Some(cur)
}

/// Creation of a processing instruction element.
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewDocPI")]
pub fn xml_new_doc_pi(
    doc: Option<XmlDocPtr>,
    name: &str,
    content: Option<&str>,
) -> Option<XmlNodePtr> {
    // Allocate a new node and fill the fields.
    let Some(mut cur) = XmlNodePtr::new(XmlNode {
        typ: XmlElementType::XmlPINode,
        name: name.to_owned().into(),
        ..Default::default()
    }) else {
        xml_tree_err_memory("building PI");
        return None;
    };
    cur.content = content.map(|content| content.to_owned());
    cur.doc = doc;

    if let Some(register) = get_register_node_func() {
        register(cur.into());
    }

    Some(cur)
}

/// Creation of a processing instruction element.
///
/// Use of this function is DISCOURAGED in favor of xmlNewDocPI.
///
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewPI")]
pub fn xml_new_pi(name: &str, content: Option<&str>) -> Option<XmlNodePtr> {
    xml_new_doc_pi(None, name, content)
}

/// Creation of a new node containing a comment within a document.
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewDocComment")]
pub fn xml_new_doc_comment(doc: Option<XmlDocPtr>, content: &str) -> Option<XmlNodePtr> {
    let cur = xml_new_comment(content);
    if let Some(mut cur) = cur {
        cur.doc = doc;
    }
    cur
}

/// Use of this function is DISCOURAGED in favor of xmlNewDocComment.
///
/// Creation of a new node containing a comment.
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewComment")]
pub fn xml_new_comment(content: &str) -> Option<XmlNodePtr> {
    // Allocate a new node and fill the fields.
    let Some(cur) = XmlNodePtr::new(XmlNode {
        typ: XmlElementType::XmlCommentNode,
        name: Cow::Borrowed(XML_STRING_COMMENT),
        content: Some(content.to_owned()),
        ..Default::default()
    }) else {
        xml_tree_err_memory("building comment");
        return None;
    };

    if let Some(register) = get_register_node_func() {
        register(cur.into());
    }

    Some(cur)
}

/// Creation of a new node containing a CDATA block.
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewCDataBlock")]
pub fn xml_new_cdata_block(doc: Option<XmlDocPtr>, content: &str) -> Option<XmlNodePtr> {
    // Allocate a new node and fill the fields.
    let Some(cur) = XmlNodePtr::new(XmlNode {
        typ: XmlElementType::XmlCDATASectionNode,
        doc,
        content: Some(content.to_owned()),
        ..Default::default()
    }) else {
        xml_tree_err_memory("building CDATA");
        return None;
    };

    if let Some(register) = get_register_node_func() {
        register(cur.into());
    }

    Some(cur)
}

/// Creation of a new character reference node.
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewCharRef")]
pub fn xml_new_char_ref(doc: Option<XmlDocPtr>, name: &str) -> Option<XmlNodePtr> {
    // Allocate a new node and fill the fields.
    let Some(mut cur) = XmlNodePtr::new(XmlNode {
        typ: XmlElementType::XmlEntityRefNode,
        doc,
        ..Default::default()
    }) else {
        xml_tree_err_memory("building character reference");
        return None;
    };
    if let Some(name) = name.strip_prefix('&') {
        if let Some(name) = name.strip_suffix(';') {
            cur.name = name.to_owned().into();
        } else {
            cur.name = name.to_owned().into();
        }
    } else {
        cur.name = name.to_owned().into();
    }

    if let Some(register) = get_register_node_func() {
        register(cur.into());
    }

    Some(cur)
}

/// Creation of a new reference node.
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewReference")]
pub fn xml_new_reference(doc: Option<XmlDocPtr>, name: &str) -> Option<XmlNodePtr> {
    // Allocate a new node and fill the fields.
    let Some(mut cur) = XmlNodePtr::new(XmlNode {
        typ: XmlElementType::XmlEntityRefNode,
        doc,
        ..Default::default()
    }) else {
        xml_tree_err_memory("building reference");
        return None;
    };
    if let Some(name) = name.strip_prefix('&') {
        if let Some(name) = name.strip_suffix(';') {
            cur.name = name.to_owned().into();
        } else {
            cur.name = name.to_owned().into();
        }
    } else {
        cur.name = name.to_owned().into();
    }

    let ent = xml_get_doc_entity(doc, &cur.name().unwrap());
    if let Some(ent) = ent {
        cur.content = ent.content.as_deref().map(|cont| cont.to_owned());
        // The parent pointer in entity is a DTD pointer and thus is NOT
        // updated.  Not sure if this is 100% correct.
        //  -George
        cur.set_children(Some(ent.into()));
        cur.set_last(Some(ent.into()));
    }

    if let Some(register) = get_register_node_func() {
        register(cur.into());
    }

    Some(cur)
}

/// Do a copy of the node.
///
/// Returns: a new #XmlNodePtr, or null_mut() in case of error.
#[doc(alias = "xmlCopyNode")]
pub unsafe fn xml_copy_node(node: XmlGenericNodePtr, extended: i32) -> Option<XmlGenericNodePtr> {
    unsafe { xml_static_copy_node(node, None, None, extended) }
}

/// Do a copy of the node to a given document.
///
/// Returns: a new #XmlNodePtr, or null_mut() in case of error.
#[doc(alias = "xmlDocCopyNode")]
pub unsafe fn xml_doc_copy_node(
    node: XmlGenericNodePtr,
    doc: Option<XmlDocPtr>,
    extended: i32,
) -> Option<XmlGenericNodePtr> {
    unsafe { xml_static_copy_node(node, doc, None, extended) }
}

/// Do a recursive copy of the node list.
///
/// Returns: a new #XmlNodePtr, or null_mut() in case of error.
#[doc(alias = "xmlDocCopyNodeList")]
pub unsafe fn xml_doc_copy_node_list(
    doc: Option<XmlDocPtr>,
    node: Option<XmlGenericNodePtr>,
) -> Option<XmlGenericNodePtr> {
    unsafe { xml_static_copy_node_list(node, doc, None) }
}

/// Do a recursive copy of the node list.
/// Use xmlDocCopyNodeList() if possible to ensure string interning.
///
/// Returns: a new #XmlNodePtr, or null_mut() in case of error.
#[doc(alias = "xmlCopyNodeList")]
pub unsafe fn xml_copy_node_list(node: Option<XmlGenericNodePtr>) -> Option<XmlGenericNodePtr> {
    unsafe { xml_static_copy_node_list(node, None, None) }
}

/// Creation of a new child element, added at the end of @parent children list.
/// @ns and @content parameters are optional (null_mut()). If @ns is null_mut(), the newly
/// created element inherits the namespace of @parent. If @content is non null_mut(),
/// a child TEXT node will be created containing the string @content.
/// NOTE: Use xmlNewChild() if @content will contain entities that need to be
/// preserved. Use this function, xmlNewTextChild(), if you need to ensure that
/// reserved XML chars that might appear in @content, such as the ampersand,
/// greater-than or less-than signs, are automatically replaced by their XML
/// escaped entity representations.
///
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewTextChild")]
#[cfg(feature = "libxml_tree")]
pub fn xml_new_text_child(
    mut parent: XmlGenericNodePtr,
    ns: Option<XmlNsPtr>,
    name: &str,
    content: Option<&str>,
) -> Option<XmlNodePtr> {
    // Allocate a new node
    let mut cur = if let Some(parent) = XmlNodePtr::try_from(parent)
        .ok()
        .filter(|parent| matches!(parent.element_type(), XmlElementType::XmlElementNode))
    {
        if ns.is_none() {
            xml_new_doc_raw_node(parent.doc, parent.ns, name, content)
        } else {
            xml_new_doc_raw_node(parent.doc, ns, name, content)
        }
    } else if let Ok(parent) = XmlDocPtr::try_from(parent) {
        if ns.is_none() {
            xml_new_doc_raw_node(Some(parent), None, name, content)
        } else {
            xml_new_doc_raw_node(Some(parent), ns, name, content)
        }
    } else if matches!(parent.element_type(), XmlElementType::XmlDocumentFragNode) {
        xml_new_doc_raw_node(parent.document(), ns, name, content)
    } else {
        None
    }?;

    // add the new element at the end of the children list.
    cur.typ = XmlElementType::XmlElementNode;
    cur.set_parent(Some(parent));
    cur.doc = parent.document();
    if parent.children().is_none() {
        parent.set_children(Some(cur.into()));
        parent.set_last(Some(cur.into()));
    } else {
        let mut prev = parent.last().unwrap();
        prev.set_next(Some(cur.into()));
        cur.prev = Some(prev);
        parent.set_last(Some(cur.into()));
    }

    Some(cur)
}

/// Creation of a new node element within a document. @ns and @content
/// are optional (null_mut()).
///
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewDocRawNode")]
#[cfg(feature = "libxml_tree")]
pub fn xml_new_doc_raw_node(
    doc: Option<XmlDocPtr>,
    ns: Option<XmlNsPtr>,
    name: &str,
    content: Option<&str>,
) -> Option<XmlNodePtr> {
    let cur = xml_new_doc_node(doc, ns, name, None);
    if let Some(mut cur) = cur {
        cur.doc = doc;
        if let Some(content) = content {
            cur.set_children(xml_new_doc_text(doc, Some(content)).map(|node| node.into()));
            if let Some(mut ulccur) = cur.children() {
                while let Some(next) = ulccur.next() {
                    ulccur.set_parent(Some(cur.into()));
                    ulccur = next;
                }
                (*ulccur).set_parent(Some(cur.into()));
                cur.set_last(Some(ulccur));
            } else {
                cur.set_last(None);
            }
        }
    }
    cur
}

/// Creation of a new Fragment node.
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewDocFragment")]
#[cfg(feature = "libxml_tree")]
pub fn xml_new_doc_fragment(doc: Option<XmlDocPtr>) -> Option<XmlNodePtr> {
    // Allocate a new DocumentFragment node and fill the fields.
    let Some(cur) = XmlNodePtr::new(XmlNode {
        typ: XmlElementType::XmlDocumentFragNode,
        doc,
        ..Default::default()
    }) else {
        xml_tree_err_memory("building fragment");
        return None;
    };

    if let Some(register) = get_register_node_func() {
        register(cur.into());
    }

    Some(cur)
}

/// Unlink the old node from its current context, prune the new one
/// at the same place. If @cur was already inserted in a document it is
/// first unlinked from its existing context.
///
/// See the note regarding namespaces in xmlAddChild.
///
/// Returns the @old node
#[doc(alias = "xmlReplaceNode")]
#[cfg(any(feature = "libxml_tree", feature = "libxml_writer"))]
pub fn xml_replace_node(
    mut old: XmlGenericNodePtr,
    cur: Option<XmlGenericNodePtr>,
) -> Option<XmlGenericNodePtr> {
    if Some(old) == cur {
        return None;
    }
    if matches!(old.element_type(), XmlElementType::XmlNamespaceDecl) || old.parent().is_none() {
        return None;
    }
    let Some(mut cur) =
        cur.filter(|cur| !matches!(cur.element_type(), XmlElementType::XmlNamespaceDecl))
    else {
        old.unlink();
        return Some(old);
    };
    if cur == old {
        return Some(old);
    }
    if matches!(old.element_type(), XmlElementType::XmlAttributeNode)
        && !matches!(cur.element_type(), XmlElementType::XmlAttributeNode)
    {
        return Some(old);
    }
    if matches!(cur.element_type(), XmlElementType::XmlAttributeNode)
        && !matches!(old.element_type(), XmlElementType::XmlAttributeNode)
    {
        return Some(old);
    }
    cur.unlink();
    cur.set_doc(old.document());
    cur.set_parent(old.parent());
    cur.set_next(old.next());
    if let Some(mut next) = cur.next() {
        next.set_prev(Some(cur));
    }
    cur.set_prev(old.prev());
    if let Some(mut prev) = cur.prev() {
        prev.set_next(Some(cur));
    }
    if let Some(mut parent) = cur.parent() {
        if matches!(cur.element_type(), XmlElementType::XmlAttributeNode) {
            let mut parent = XmlNodePtr::try_from(parent).unwrap();
            if parent.properties.map(|prop| prop.into()) == Some(old) {
                parent.properties = Some(XmlAttrPtr::try_from(cur).unwrap());
            }
        } else {
            if parent.children() == Some(old) {
                parent.set_children(Some(cur));
            }
            if parent.last() == Some(old) {
                parent.set_last(Some(cur));
            }
        }
    }
    old.set_next(None);
    old.set_prev(None);
    old.set_parent(None);
    Some(old)
}

/// Merge two text nodes into one
/// Returns the first text node augmented
#[doc(alias = "xmlTextMerge")]
pub unsafe fn xml_text_merge(
    first: Option<XmlNodePtr>,
    second: Option<XmlNodePtr>,
) -> Option<XmlNodePtr> {
    unsafe {
        match (first, second) {
            (Some(mut first), Some(mut second))
                if first.element_type() == XmlElementType::XmlTextNode
                    && second.element_type() == XmlElementType::XmlTextNode =>
            {
                if first.name != second.name {
                    return Some(first);
                }
                let content = second.content.as_deref().unwrap();
                first.add_content(content);
                second.unlink();
                xml_free_node(second);
                Some(first)
            }
            (Some(first), Some(_)) => Some(first),
            (Some(text), None) | (None, Some(text)) => Some(text),
            (None, None) => None,
        }
    }
}

/// Concat the given string at the end of the existing node content
///
/// Returns -1 in case of error, 0 otherwise
#[doc(alias = "xmlTextConcat")]
pub fn xml_text_concat(mut node: XmlNodePtr, content: &str) -> i32 {
    if !matches!(node.element_type(), XmlElementType::XmlTextNode)
        && !matches!(node.element_type(), XmlElementType::XmlCDATASectionNode)
        && !matches!(node.element_type(), XmlElementType::XmlCommentNode)
        && !matches!(node.element_type(), XmlElementType::XmlPINode)
    {
        return -1;
    }
    // need to check if content is currently in the dictionary
    let cont = node.content.get_or_insert_default();
    cont.push_str(content);
    node.properties = None;
    0
}

/// Free a node and all its siblings, this is a recursive behaviour, all
/// the children are freed too.
#[doc(alias = "xmlFreeNodeList")]
pub unsafe fn xml_free_node_list(cur: Option<impl Into<XmlGenericNodePtr>>) {
    unsafe {
        let mut depth: usize = 0;

        let Some(mut cur) = cur.map(|cur| cur.into()) else {
            return;
        };
        if let Ok(ns) = XmlNsPtr::try_from(cur) {
            xml_free_ns_list(ns);
            return;
        }
        loop {
            // This conditional statement cannot be replaced by `let Some`.
            // Calling `children()` when `cur` type is `XmlEntityRefNode` will not work correctly.
            // Cause unknown for now...
            while !matches!(
                cur.element_type(),
                XmlElementType::XmlDocumentNode
                    | XmlElementType::XmlHTMLDocumentNode
                    | XmlElementType::XmlDTDNode
                    | XmlElementType::XmlEntityRefNode
            ) && cur.children().is_some()
            {
                cur = cur.children().unwrap();
                depth += 1;
            }

            let next = cur.next();
            let parent = cur.parent();
            if let Ok(doc) = XmlDocPtr::try_from(cur) {
                xml_free_doc(doc);
            } else if !matches!(cur.element_type(), XmlElementType::XmlDTDNode) {
                let cur = XmlNodePtr::try_from(cur).unwrap();
                if let Some(deregister) = get_deregister_node_func() {
                    deregister(cur.into())
                }

                if (matches!(cur.element_type(), XmlElementType::XmlElementNode)
                    || matches!(cur.element_type(), XmlElementType::XmlXIncludeStart)
                    || matches!(cur.element_type(), XmlElementType::XmlXIncludeEnd))
                    && cur.properties.is_some()
                {
                    xml_free_prop_list(cur.properties);
                }
                if matches!(cur.element_type(), XmlElementType::XmlElementNode)
                    || matches!(cur.element_type(), XmlElementType::XmlXIncludeStart)
                    || matches!(cur.element_type(), XmlElementType::XmlXIncludeEnd)
                {
                    if let Some(ns_def) = cur.ns_def {
                        xml_free_ns_list(ns_def);
                    }
                }

                cur.free();
            }

            if let Some(next) = next {
                cur = next;
            } else {
                if depth == 0 {
                    break;
                }
                let Some(parent) = parent else {
                    break;
                };
                depth -= 1;
                cur = parent;
                cur.set_children(None);
            }
        }
    }
}

/// Free a node, this is a recursive behaviour, all the children are freed too.
/// This doesn't unlink the child from the list, use xmlUnlinkNode() first.
#[doc(alias = "xmlFreeNode")]
pub unsafe fn xml_free_node(cur: impl Into<XmlGenericNodePtr>) {
    unsafe {
        let cur = cur.into();
        // use xmlFreeDtd for DTD nodes
        if let Ok(dtd) = XmlDtdPtr::try_from(cur) {
            xml_free_dtd(dtd);
            return;
        }
        if let Ok(ns) = XmlNsPtr::try_from(cur) {
            xml_free_ns(ns);
            return;
        }
        if let Ok(attr) = XmlAttrPtr::try_from(cur) {
            xml_free_prop(attr);
            return;
        }

        if let Some(deregister) = get_deregister_node_func() {
            deregister(cur)
        }

        if let Ok(mut ent) = XmlEntityPtr::try_from(cur) {
            ent.system_id = None;
            ent.external_id = None;
        }
        if let Some(children) = cur
            .children()
            .filter(|_| !matches!(cur.element_type(), XmlElementType::XmlEntityRefNode))
        {
            xml_free_node_list(Some(children));
        }

        if let Ok(mut cur) = XmlNodePtr::try_from(cur) {
            if matches!(
                cur.element_type(),
                XmlElementType::XmlElementNode
                    | XmlElementType::XmlXIncludeStart
                    | XmlElementType::XmlXIncludeEnd
            ) {
                if cur.properties.is_some() {
                    xml_free_prop_list(cur.properties);
                }
                if let Some(ns_def) = cur.ns_def.take() {
                    xml_free_ns_list(ns_def);
                }
            }

            cur.free();
        } else if XmlEntityPtr::try_from(cur).is_ok() {
            // When a node is a text node or a comment, it uses a global static
            // variable for the name of the node.
            // Otherwise the node name might come from the document's dictionary
        } else {
            // Does this pattern occur ???
            todo!()
        }
    }
}

/// Verify that the given namespace held on @ancestor is still in scope on node.
///
/// Returns 1 if true, 0 if false and -1 in case of error.
#[doc(alias = "xmlNsInScope")]
fn xml_ns_in_scope(
    _doc: Option<XmlDocPtr>,
    mut node: Option<XmlGenericNodePtr>,
    ancestor: Option<XmlGenericNodePtr>,
    prefix: Option<&str>,
) -> i32 {
    while let Some(cur_node) = node.filter(|&node| Some(node) != ancestor) {
        if matches!(
            cur_node.element_type(),
            XmlElementType::XmlEntityRefNode
                | XmlElementType::XmlEntityNode
                | XmlElementType::XmlEntityDecl
        ) {
            return -1;
        }
        if let Some(cur_node) = XmlNodePtr::try_from(cur_node)
            .ok()
            .filter(|cur_node| matches!(cur_node.element_type(), XmlElementType::XmlElementNode))
        {
            let mut tst = cur_node.ns_def;
            while let Some(now) = tst {
                if now.prefix().is_none() && prefix.is_none() {
                    return 0;
                }
                if now.prefix().is_some() && now.prefix.as_deref() == prefix {
                    return 0;
                }
                tst = now.next;
            }
        }
        node = cur_node.parent();
    }
    if node != ancestor {
        return -1;
    }
    1
}

/// Do a copy of the namespace.
///
/// Returns: a new #xmlNsPtr, or null_mut() in case of error.
#[doc(alias = "xmlCopyNamespace")]
pub fn xml_copy_namespace(cur: Option<XmlNsPtr>) -> Option<XmlNsPtr> {
    let cur = cur?;
    match cur.element_type() {
        XML_LOCAL_NAMESPACE => xml_new_ns(None, cur.href.as_deref(), cur.prefix().as_deref()),
        _ => None,
    }
}

/// Do a copy of an namespace list.
///
/// Returns: a new #xmlNsPtr, or null_mut() in case of error.
#[doc(alias = "xmlCopyNamespaceList")]
pub unsafe fn xml_copy_namespace_list(mut cur: Option<XmlNsPtr>) -> Option<XmlNsPtr> {
    unsafe {
        let mut ret = None::<XmlNsPtr>;
        let mut p = None::<XmlNsPtr>;

        while let Some(now) = cur {
            let Some(q) = xml_copy_namespace(Some(now)) else {
                if let Some(ret) = ret {
                    xml_free_ns_list(ret);
                }
                return None;
            };
            if let Some(mut l) = p {
                l.next = Some(q);
                p = Some(q);
            } else {
                ret = Some(q);
                p = ret;
            }
            cur = now.next;
        }
        ret
    }
}

static XML_CHECK_DTD: AtomicBool = AtomicBool::new(true);

/// Handle an out of memory condition
#[doc(alias = "xmlTreeErr")]
fn xml_tree_err(code: XmlParserErrors, node: Option<XmlGenericNodePtr>, extra: Option<&str>) {
    let msg: Cow<'static, str> = match code {
        XmlParserErrors::XmlTreeInvalidHex => "invalid hexadecimal character value\n".into(),
        XmlParserErrors::XmlTreeInvalidDec => "invalid decimal character value\n".into(),
        XmlParserErrors::XmlTreeUnterminatedEntity => format!(
            "unterminated entity reference {}\n",
            extra.expect("Internal Error")
        )
        .into(),
        XmlParserErrors::XmlTreeNotUTF8 => "string is not in UTF-8\n".into(),
        _ => "unexpected error number\n".into(),
    };
    __xml_simple_error!(XmlErrorDomain::XmlFromTree, code, node, &msg);
}

const XHTML_STRICT_PUBLIC_ID: &str = "-//W3C//DTD XHTML 1.0 Strict//EN";
const XHTML_STRICT_SYSTEM_ID: &str = "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd";
const XHTML_FRAME_PUBLIC_ID: &str = "-//W3C//DTD XHTML 1.0 Frameset//EN";
const XHTML_FRAME_SYSTEM_ID: &str = "http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd";
const XHTML_TRANS_PUBLIC_ID: &str = "-//W3C//DTD XHTML 1.0 Transitional//EN";
const XHTML_TRANS_SYSTEM_ID: &str = "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd";

/// Try to find if the document correspond to an XHTML DTD.
#[doc(alias = "xmlIsXHTML")]
pub fn is_xhtml(system_id: Option<&str>, public_id: Option<&str>) -> bool {
    public_id.is_some_and(|s| {
        s == XHTML_STRICT_PUBLIC_ID || s == XHTML_FRAME_PUBLIC_ID || s == XHTML_TRANS_PUBLIC_ID
    }) || system_id.is_some_and(|s| {
        s == XHTML_STRICT_SYSTEM_ID || s == XHTML_FRAME_SYSTEM_ID || s == XHTML_TRANS_SYSTEM_ID
    })
}

static XML_COMPRESS_MODE: AtomicI32 = AtomicI32::new(0);

/// Get the default compression mode used, ZLIB based.
///
/// Returns 0 (uncompressed) to 9 (max compression)
#[doc(alias = "xmlGetCompressMode")]
pub fn get_compress_mode() -> i32 {
    XML_COMPRESS_MODE.load(Ordering::Acquire)
}

/// Set the default compression mode used, ZLIB based.
///
/// Correct values: 0 (uncompressed) to 9 (max compression)
#[doc(alias = "xmlSetCompressMode")]
pub fn set_compress_mode(mode: i32) {
    if mode < 0 {
        XML_COMPRESS_MODE.store(0, Ordering::Release);
    } else if mode > 9 {
        XML_COMPRESS_MODE.store(9, Ordering::Release);
    } else {
        XML_COMPRESS_MODE.store(mode, Ordering::Release);
    }
}

/// Searches for a ns-decl with the given prefix in @nsList.
///
/// Returns the ns-decl if found, null_mut() if not found and on API errors.
#[doc(alias = "xmlTreeLookupNsListByPrefix")]
fn xml_tree_nslist_lookup_by_prefix(
    ns_list: Option<XmlNsPtr>,
    prefix: Option<&str>,
) -> Option<XmlNsPtr> {
    let mut ns = ns_list;
    while let Some(now) = ns {
        if prefix == now.prefix.as_deref() {
            return ns;
        }
        ns = now.next;
    }
    None
}

/// Dynamically searches for a ns-declaration which matches
/// the given @nsName in the ancestor-or-self axis of @node.
///
/// Returns 1 if a ns-decl was found, 0 if not and -1 on API and internal errors.
#[doc(alias = "xmlSearchNsByPrefixStrict")]
fn xml_search_ns_by_prefix_strict(
    mut doc: XmlDocPtr,
    node: XmlGenericNodePtr,
    prefix: Option<&str>,
    mut ret_ns: Option<&mut Option<XmlNsPtr>>,
) -> i32 {
    if matches!(node.element_type(), XmlElementType::XmlNamespaceDecl) {
        return -1;
    }

    if let Some(ret_ns) = ret_ns.as_deref_mut() {
        *ret_ns = None;
    }
    if prefix == Some("xml") {
        if let Some(ret_ns) = ret_ns {
            *ret_ns = doc.ensure_xmldecl();
            if ret_ns.is_none() {
                return -1;
            }
        }
        return 1;
    }
    let mut cur = Some(node);
    while let Some(cur_node) = cur.filter(|&cur| cur.document().map(|doc| doc.into()) != Some(cur))
    {
        if matches!(cur_node.element_type(), XmlElementType::XmlElementNode) {
            let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
            let mut ns = cur_node.ns_def;
            while let Some(now) = ns {
                if prefix == now.prefix.as_deref() {
                    // Disabled namespaces, e.g. xmlns:abc="".
                    if now.href.is_none() {
                        return 0;
                    }
                    if let Some(ret_ns) = ret_ns {
                        *ret_ns = ns;
                    }
                    return 1;
                }
                ns = now.next;
            }
        } else if matches!(
            cur_node.element_type(),
            XmlElementType::XmlEntityNode | XmlElementType::XmlEntityDecl
        ) {
            return 0;
        }
        cur = cur_node.parent();
    }
    0
}

/// Dynamically searches for a ns-declaration which matches
/// the given @nsName in the ancestor-or-self axis of @node.
///
/// Returns 1 if a ns-decl was found, 0 if not and -1 on API and internal errors.
#[doc(alias = "xmlSearchNsByNamespaceStrict")]
fn xml_search_ns_by_namespace_strict(
    mut doc: XmlDocPtr,
    node: XmlGenericNodePtr,
    ns_name: &str,
    ret_ns: &mut Option<XmlNsPtr>,
    prefixed: i32,
) -> i32 {
    if matches!(node.element_type(), XmlElementType::XmlNamespaceDecl) {
        return -1;
    }

    *ret_ns = None;
    if ns_name == XML_XML_NAMESPACE {
        *ret_ns = doc.ensure_xmldecl();
        if ret_ns.is_none() {
            return -1;
        }
        return 1;
    }
    let mut cur = Some(node);
    let mut prev: Option<XmlNodePtr> = None;
    let mut out = None;
    while let Some(cur_node) = cur.filter(|&cur| Some(cur) != cur.document().map(|doc| doc.into()))
    {
        if matches!(cur_node.element_type(), XmlElementType::XmlElementNode) {
            let cur = XmlNodePtr::try_from(cur_node).unwrap();
            if cur.ns_def.is_some() {
                let mut ns = cur.ns_def;
                while let Some(now) = ns {
                    if prefixed != 0 && now.prefix().is_none() {
                        ns = now.next;
                        continue;
                    }
                    if let Some(prev) = prev {
                        // Check the last level of ns-decls for a
                        // shadowing prefix.
                        let mut prevns = prev.ns_def;
                        while let Some(pns) = prevns {
                            if pns.prefix() == now.prefix()
                                || (pns.prefix().is_some()
                                    && now.prefix().is_some()
                                    && pns.prefix() == now.prefix())
                            {
                                // Shadowed.
                                break;
                            }
                            prevns = pns.next;
                        }
                        if prevns.is_some() {
                            ns = now.next;
                            continue;
                        }
                    }
                    // Ns-name comparison.
                    if Some(ns_name) == now.href.as_deref() {
                        // At this point the prefix can only be shadowed,
                        // if we are the the (at least) 3rd level of ns-decls.
                        if out.is_some() {
                            let ret: i32 = xml_ns_in_scope(
                                Some(doc),
                                Some(node),
                                prev.map(|prev| prev.into()),
                                now.prefix.as_deref(),
                            );
                            if ret < 0 {
                                return -1;
                            }
                            // TODO: Should we try to find a matching ns-name
                            // only once? This here keeps on searching.
                            // I think we should try further since, there might
                            // be an other matching ns-decl with an unshadowed
                            // prefix.
                            if ret == 0 {
                                ns = now.next;
                                continue;
                            }
                        }
                        *ret_ns = ns;
                        return 1;
                    }
                }
                out = prev;
                prev = Some(cur);
            }
        } else if (matches!(cur_node.element_type(), XmlElementType::XmlEntityNode)
            || matches!(cur_node.element_type(), XmlElementType::XmlEntityDecl))
        {
            return 0;
        }
        cur = cur_node.parent();
    }
    0
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_get_compress_mode() {
        unsafe {
            let mut leaks = 0;
            let mem_base = xml_mem_blocks();

            let ret_val = get_compress_mode();
            desret_int(ret_val);
            reset_last_error();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in xmlGetCompressMode",
                    xml_mem_blocks() - mem_base
                );
                assert!(
                    leaks == 0,
                    "{leaks} Leaks are found in xmlGetCompressMode()"
                );
            }
        }
    }
}
