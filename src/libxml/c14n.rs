//! Provide methods and data structures for Canonical XML and Exclusive XML Canonicalization.  
//! This module is based on `libxml/c14n.h`, `c14n.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: Provide Canonical XML and Exclusive XML Canonicalization
// Description: the c14n modules provides a
//
// "Canonical XML" implementation
// http://www.w3.org/TR/xml-c14n
//
// and an
//
// "Exclusive XML Canonicalization" implementation
// http://www.w3.org/TR/xml-exc-c14n
//
// Copy: See Copyright for the status of this software.
//
// Author: Aleksey Sanin <aleksey@aleksey.com>
// --------
// "Canonical XML" implementation
// http://www.w3.org/TR/xml-c14n
//
// "Exclusive XML Canonicalization" implementation
// http://www.w3.org/TR/xml-exc-c14n
//
// See Copyright for the status of this software.
//
// Author: Aleksey Sanin <aleksey@aleksey.com>

use std::{
    any::type_name,
    cell::RefCell,
    ffi::{c_char, CStr, CString},
    ptr::{drop_in_place, null_mut},
    rc::Rc,
};

use crate::{
    error::{XmlParserErrors, __xml_raise_error},
    io::{write_quoted, XmlOutputBuffer},
    list::XmlList,
    tree::{
        xml_free_prop_list, xml_new_ns_prop, NodeCommon, XmlAttr, XmlAttrPtr, XmlDoc, XmlDocPtr,
        XmlElementType, XmlGenericNodePtr, XmlNode, XmlNodePtr, XmlNs, XmlNsPtr, XML_XML_NAMESPACE,
    },
    uri::build_uri,
    xpath::XmlNodeSet,
};

use super::{
    globals::{xml_free, xml_malloc_atomic, xml_realloc},
    uri::{xml_free_uri, xml_parse_uri, XmlURIPtr},
    xmlstring::{xml_strcmp, xml_strlen, XmlChar},
};

// Predefined values for C14N modes
#[doc(alias = "xmlC14NMode")]
#[repr(C)]
pub enum XmlC14NMode {
    XmlC14N1_0 = 0,          /* Original C14N 1.0 spec */
    XmlC14NExclusive1_0 = 1, /* Exclusive C14N 1.0 spec */
    XmlC14N1_1 = 2,          /* C14N 1.1 spec */
}

impl TryFrom<i32> for XmlC14NMode {
    type Error = anyhow::Error;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        if value == Self::XmlC14N1_0 as i32 {
            Ok(Self::XmlC14N1_0)
        } else if value == Self::XmlC14NExclusive1_0 as i32 {
            Ok(Self::XmlC14NExclusive1_0)
        } else if value == Self::XmlC14N1_1 as i32 {
            Ok(Self::XmlC14N1_1)
        } else {
            Err(anyhow::anyhow!(
                "Invalid convert from value '{value}' to {}",
                type_name::<Self>()
            ))
        }
    }
}

#[repr(C)]
pub enum XmlC14NPosition {
    XmlC14NBeforeDocumentElement = 0,
    XmlC14NInsideDocumentElement = 1,
    XmlC14NAfterDocumentElement = 2,
}

pub type XmlC14NVisibleNsStackPtr = *mut XmlC14NVisibleNsStack;
#[repr(C)]
#[derive(Default)]
pub struct XmlC14NVisibleNsStack {
    ns_cur_end: usize,         /* number of nodes in the set */
    ns_prev_start: usize,      /* the beginning of the stack for previous visible node */
    ns_prev_end: usize,        /* the end of the stack for previous visible node */
    ns_tab: Vec<XmlNsPtr>,     /* array of ns in no particular order */
    node_tab: Vec<XmlNodePtr>, /* array of nodes in no particular order */
}

impl XmlC14NVisibleNsStack {
    fn save(&self, state: &mut XmlC14NVisibleNsStack) {
        state.ns_cur_end = self.ns_cur_end;
        state.ns_prev_start = self.ns_prev_start;
        state.ns_prev_end = self.ns_prev_end;
    }

    /// Check whether `ns` was already rendered or not.  
    /// Return `true` if already rendered, otherwise return `false`.
    ///
    /// Please refer to the document of `xmlC14NVisibleNsStackFind` for original libxml2.
    #[doc(alias = "xmlC14NVisibleNsStackFind")]
    unsafe fn find(&self, ns: Option<&XmlNs>) -> bool {
        // if the default namespace xmlns="" is not defined yet then we do not want to print it out
        let prefix: *const XmlChar =
            if let Some(prefix) = ns.filter(|ns| ns.prefix().is_some()).map(|ns| ns.prefix) {
                prefix
            } else {
                c"".as_ptr() as _
            };
        let href: *const XmlChar =
            if let Some(href) = ns.filter(|ns| !ns.href.is_null()).map(|ns| ns.href) {
                href
            } else {
                c"".as_ptr() as _
            };

        let has_empty_ns =
            xml_c14n_str_equal(prefix, null_mut()) && xml_c14n_str_equal(href, null_mut());

        let start = if has_empty_ns { 0 } else { self.ns_prev_start };
        for &ns1 in self.ns_tab[start..self.ns_cur_end].iter().rev() {
            if xml_c14n_str_equal(prefix, ns1.prefix) {
                return xml_c14n_str_equal(href, ns1.href);
            }
        }
        has_empty_ns
    }

    #[doc(alias = "xmlC14NVisibleNsStackAdd")]
    fn add(&mut self, ns: XmlNsPtr, node: XmlNodePtr) {
        if self.ns_cur_end == self.ns_tab.len() {
            self.ns_tab.push(ns);
            self.node_tab.push(node);
        } else {
            self.ns_tab[self.ns_cur_end] = ns;
            self.node_tab[self.ns_cur_end] = node;
        }
        self.ns_cur_end += 1;
    }

    fn shift(&mut self) {
        self.ns_prev_start = self.ns_prev_end;
        self.ns_prev_end = self.ns_cur_end;
    }

    fn restore(&mut self, state: &XmlC14NVisibleNsStack) {
        self.ns_cur_end = state.ns_cur_end;
        self.ns_prev_start = state.ns_prev_start;
        self.ns_prev_end = state.ns_prev_end;
    }
}

/// Signature for a C14N callback on visible nodes
///
/// Returns 1 if the node should be included
#[doc(alias = "xmlC14NIsVisibleCallback")]
pub type XmlC14NIsVisibleCallback<T> = unsafe fn(
    user_data: &T,
    node: Option<XmlGenericNodePtr>,
    parent: Option<XmlGenericNodePtr>,
) -> i32;

#[repr(C)]
pub struct XmlC14NCtx<'a, T> {
    // input parameters
    doc: &'a mut XmlDoc,
    is_visible_callback: Option<XmlC14NIsVisibleCallback<T>>,
    user_data: T,
    with_comments: bool,
    buf: Rc<RefCell<XmlOutputBuffer<'a>>>,

    // position in the XML document
    pos: XmlC14NPosition,
    parent_is_doc: bool,
    ns_rendered: Box<XmlC14NVisibleNsStack>,

    // C14N mode
    mode: XmlC14NMode,

    // exclusive canonicalization
    inclusive_ns_prefixes: Option<Vec<String>>,

    // error number
    error: XmlParserErrors,
}

impl<T> XmlC14NCtx<'_, T> {
    unsafe fn is_visible(
        &self,
        node: Option<XmlGenericNodePtr>,
        parent: Option<XmlGenericNodePtr>,
    ) -> bool {
        if let Some(callback) = self.is_visible_callback {
            callback(&self.user_data, node, parent) != 0
        } else {
            true
        }
    }

    fn is_exclusive(&self) -> bool {
        matches!(self.mode, XmlC14NMode::XmlC14NExclusive1_0)
    }

    /// Checks that current element node has no relative namespaces defined
    ///
    /// Returns 0 if the node has no relative namespaces or -1 otherwise.
    #[doc(alias = "xmlC14NCheckForRelativeNamespaces")]
    unsafe fn check_for_relative_namespaces(&self, cur: &XmlNode) -> i32 {
        if !matches!(cur.element_type(), XmlElementType::XmlElementNode) {
            xml_c14n_err_param("checking for relative namespaces");
            return -1;
        }

        let mut ns = cur.ns_def;
        while let Some(now) = ns {
            if xml_strlen(now.href as _) > 0 {
                let uri: XmlURIPtr = xml_parse_uri(now.href as _);
                if uri.is_null() {
                    xml_c14n_err_internal("parsing namespace uri");
                    return -1;
                }
                if xml_strlen((*uri).scheme as _) == 0 {
                    let scheme = CStr::from_ptr((*uri).scheme as *const i8).to_string_lossy();
                    xml_c14n_err_relative_namespace(scheme.as_ref());
                    xml_free_uri(uri);
                    return -1;
                }
                xml_free_uri(uri);
            }
            ns = now.next;
        }
        0
    }

    /// Prints the given namespace to the output buffer from C14N context.
    ///
    /// Returns 1 on success or 0 on fail.
    #[doc(alias = "xmlC14NPrintNamespaces")]
    unsafe fn print_namespaces(&mut self, ns: &XmlNs) -> i32 {
        if let Some(prefix) = ns.prefix() {
            self.buf.borrow_mut().write_str(" xmlns:");
            self.buf.borrow_mut().write_str(&prefix);
            self.buf.borrow_mut().write_str("=");
        } else {
            self.buf.borrow_mut().write_str(" xmlns=");
        }
        if !ns.href.is_null() {
            write_quoted(&mut *self.buf.borrow_mut(), &ns.href().unwrap());
        } else {
            self.buf.borrow_mut().write_str("\"\"");
        }
        1
    }

    /// Prints out canonical namespace axis of the current node to the
    /// buffer from C14N context as follows
    ///
    /// Canonical XML v 1.0 (<http://www.w3.org/TR/xml-c14n>)
    ///
    /// # Namespace Axis
    /// Consider a list L containing only namespace nodes in the
    /// axis and in the node-set in lexicographic order (ascending).  
    /// To begin processing L, if the first node is not the default namespace node
    /// (a node with no namespace URI and no local name), then generate a space followed
    /// by xmlns="" if and only if the following conditions are met:
    /// - the element E that owns the axis is in the node-set
    /// - The nearest ancestor element of E in the node-set has a default namespace node
    ///   in the node-set (default namespace nodes always have non-empty values in XPath)
    ///   
    /// The latter condition eliminates unnecessary occurrences of xmlns="" in
    /// the canonical form since an element only receives an xmlns="" if its
    /// default namespace is empty and if it has an immediate parent in the
    /// canonical form that has a non-empty default namespace.  
    /// To finish processing L, simply process every namespace node in L,
    /// except omit namespace node with local name xml, which defines the xml prefix,
    /// if its string value is `http://www.w3.org/XML/1998/namespace`.
    ///
    /// Exclusive XML Canonicalization v 1.0 (<http://www.w3.org/TR/xml-exc-c14n>)
    /// Canonical XML applied to a document subset requires the search of the
    /// ancestor nodes of each orphan element node for attributes in the xml
    /// namespace, such as xml:lang and xml:space.  
    /// These are copied into the element node except if a declaration of the same attribute is already
    /// in the attribute axis of the element (whether or not it is included in the document subset).  
    /// This search and copying are omitted from the Exclusive XML Canonicalization method.
    ///
    /// Returns 0 on success or -1 on fail.
    #[doc(alias = "xmlC14NProcessNamespacesAxis")]
    unsafe fn process_namespaces_axis(&mut self, mut cur: XmlNodePtr, visible: bool) -> i32 {
        let mut has_empty_ns = false;

        if !matches!(cur.element_type(), XmlElementType::XmlElementNode) {
            xml_c14n_err_param("processing namespaces axis (c14n)");
            return -1;
        }

        // Create a sorted list to store element namespaces
        let mut list = XmlList::new(
            None,
            Rc::new(|ns1, ns2| match xml_c14n_ns_compare(*ns1, *ns2) {
                ..0 => std::cmp::Ordering::Less,
                0 => std::cmp::Ordering::Equal,
                1.. => std::cmp::Ordering::Greater,
            }),
        );

        // check all namespaces
        let mut n = Some(cur);
        while let Some(node) = n {
            let mut ns = node.ns_def;
            while let Some(now) = ns {
                let prefix = now.prefix();
                let cur_doc = cur.doc;
                let tmp = cur.search_ns(cur_doc, prefix.as_deref());

                if tmp == Some(now)
                    && !xml_c14n_is_xml_ns(now)
                    && self.is_visible(Some(now.into()), Some(cur.into()))
                {
                    let already_rendered = (*self.ns_rendered).find(Some(&*now));
                    if visible {
                        (*self.ns_rendered).add(now, cur);
                    }
                    if !already_rendered {
                        list.insert_lower_bound(now);
                    }
                    if now.prefix().map_or(0, |pre| pre.len()) == 0 {
                        has_empty_ns = true;
                    }
                }
                ns = now.next;
            }
            n = node.parent().and_then(|p| XmlNodePtr::try_from(p).ok());
        }

        // if the first node is not the default namespace node (a node with no
        //  namespace URI and no local name), then generate a space followed by
        //  xmlns="" if and only if the following conditions are met:
        //   - the element E that owns the axis is in the node-set
        //   - the nearest ancestor element of E in the node-set has a default
        //      namespace node in the node-set (default namespace nodes always
        //      have non-empty values in XPath)
        if visible && !has_empty_ns && !(*self.ns_rendered).find(Some(&XmlNs::default())) {
            self.print_namespaces(&XmlNs::default());
        }

        // print out all elements from list
        list.walk(|data| self.print_namespaces(data) != 0);

        // Cleanup
        0
    }

    /// Prints out canonical attribute axis of the current node to the
    /// buffer from C14N context as follows
    ///
    /// Canonical XML v 1.0 (<http://www.w3.org/TR/xml-c14n>)
    ///
    /// # Attribute Axis
    /// In lexicographic order (ascending), process each node that
    /// is in the element's attribute axis and in the node-set.
    ///
    /// The processing of an element node E MUST be modified slightly
    /// when an XPath node-set is given as input and the element's
    /// parent is omitted from the node-set.
    ///
    /// Exclusive XML Canonicalization v 1.0 (<http://www.w3.org/TR/xml-exc-c14n>)
    ///
    /// Canonical XML applied to a document subset requires the search of the
    /// ancestor nodes of each orphan element node for attributes in the xml
    /// namespace, such as xml:lang and xml:space. These are copied into the
    /// element node except if a declaration of the same attribute is already
    /// in the attribute axis of the element (whether or not it is included in
    /// the document subset). This search and copying are omitted from the
    /// Exclusive XML Canonicalization method.
    ///
    /// Returns 0 on success or -1 on fail.
    #[doc(alias = "xmlC14NProcessAttrsAxis")]
    unsafe fn process_attrs_axis(&mut self, cur: XmlNodePtr, parent_visible: bool) -> i32 {
        let mut attrs_to_delete = None::<XmlAttrPtr>;

        if !matches!(cur.element_type(), XmlElementType::XmlElementNode) {
            xml_c14n_err_param("processing attributes axis");
            return -1;
        }

        // Create a sorted list to store element attributes
        let mut list = XmlList::new(
            None,
            Rc::new(
                |&attr1, &attr2| match xml_c14n_attrs_compare(attr1, attr2) {
                    ..0 => std::cmp::Ordering::Less,
                    0 => std::cmp::Ordering::Equal,
                    1.. => std::cmp::Ordering::Greater,
                },
            ),
        );
        match self.mode {
            XmlC14NMode::XmlC14N1_0 => {
                // The processing of an element node E MUST be modified slightly when an XPath node-set is
                // given as input and the element's parent is omitted from the node-set. The method for processing
                // the attribute axis of an element E in the node-set is enhanced. All element nodes along E's
                // ancestor axis are examined for nearest occurrences of attributes in the xml namespace, such
                // as xml:lang and xml:space (whether or not they are in the node-set). From this list of attributes,
                // remove any that are in E's attribute axis (whether or not they are in the node-set). Then,
                // lexicographically merge this attribute list with the nodes of E's attribute axis that are in
                // the node-set. The result of visiting the attribute axis is computed by processing the attribute
                // nodes in this merged attribute list.

                // Add all visible attributes from current node.
                let mut attr = cur.properties;
                while let Some(now) = attr {
                    // check that attribute is visible
                    if self.is_visible(Some(now.into()), Some(cur.into())) {
                        list.insert_lower_bound(now);
                    }
                    attr = now.next;
                }

                // Handle xml attributes
                if parent_visible
                    && cur.parent().is_some()
                    && !self.is_visible(cur.parent(), cur.parent().and_then(|cur| cur.parent()))
                {
                    // If XPath node-set is not specified then the parent is always visible!
                    let mut tmp = cur
                        .parent()
                        .and_then(|node| XmlNodePtr::try_from(node).ok());
                    while let Some(cur_node) = tmp {
                        let mut attr = cur_node.properties;
                        while let Some(now) = attr {
                            if xml_c14n_is_xml_attr(now) && list.search(&now).is_none() {
                                list.insert_lower_bound(now);
                            }
                            attr = now.next;
                        }
                        tmp = cur_node.parent().and_then(|p| XmlNodePtr::try_from(p).ok());
                    }
                }
            }
            XmlC14NMode::XmlC14NExclusive1_0 => {
                // attributes in the XML namespace, such as xml:lang and xml:space
                // are not imported into orphan nodes of the document subset

                // Add all visible attributes from current node.
                let mut attr = cur.properties;
                while let Some(now) = attr {
                    // check that attribute is visible
                    if self.is_visible(Some(now.into()), Some(cur.into())) {
                        list.insert_lower_bound(now);
                    }
                    attr = now.next;
                }
            }
            XmlC14NMode::XmlC14N1_1 => {
                // The processing of an element node E MUST be modified slightly when an XPath node-set is
                // given as input and some of the element's ancestors are omitted from the node-set.
                //
                // Simple inheritable attributes are attributes that have a value that requires at most a simple
                // redeclaration. This redeclaration is done by supplying a new value in the child axis. The
                // redeclaration of a simple inheritable attribute A contained in one of E's ancestors is done
                // by supplying a value to an attribute Ae inside E with the same name. Simple inheritable attributes
                // are xml:lang and xml:space.
                //
                // The method for processing the attribute axis of an element E in the node-set is hence enhanced.
                // All element nodes along E's ancestor axis are examined for the nearest occurrences of simple
                // inheritable attributes in the xml namespace, such as xml:lang and xml:space (whether or not they
                // are in the node-set). From this list of attributes, any simple inheritable attributes that are
                // already in E's attribute axis (whether or not they are in the node-set) are removed. Then,
                // lexicographically merge this attribute list with the nodes of E's attribute axis that are in
                // the node-set. The result of visiting the attribute axis is computed by processing the attribute
                // nodes in this merged attribute list.
                //
                // The xml:id attribute is not a simple inheritable attribute and no processing of these attributes is
                // performed.
                //
                // The xml:base attribute is not a simple inheritable attribute and requires special processing beyond
                // a simple redeclaration.
                //
                // Attributes in the XML namespace other than xml:base, xml:id, xml:lang, and xml:space MUST be processed
                // as ordinary attributes.

                // special processing for 1.1 spec
                let mut xml_base_attr = None;
                let mut xml_lang_attr = None;
                let mut xml_space_attr = None;

                // Add all visible attributes from current node.
                let mut attr = cur.properties;
                while let Some(now) = attr {
                    // special processing for XML attribute kiks in only when we have invisible parents
                    if !parent_visible || !xml_c14n_is_xml_attr(now) {
                        // check that attribute is visible
                        if self.is_visible(Some(now.into()), Some(cur.into())) {
                            list.insert_lower_bound(now);
                        }
                    } else {
                        let mut matched: i32 = 0;

                        // check for simple inheritance attributes
                        if matched == 0
                            && xml_lang_attr.is_none()
                            && now.name().as_deref() == Some("lang")
                        {
                            xml_lang_attr = attr;
                            matched = 1;
                        }
                        if matched == 0
                            && xml_space_attr.is_none()
                            && now.name().as_deref() == Some("space")
                        {
                            xml_space_attr = attr;
                            matched = 1;
                        }

                        // check for base attr
                        if matched == 0
                            && xml_base_attr.is_none()
                            && now.name().as_deref() == Some("base")
                        {
                            xml_base_attr = attr;
                            matched = 1;
                        }

                        // otherwise, it is a normal attribute, so just check if it is visible
                        if matched == 0 && self.is_visible(Some(now.into()), Some(cur.into())) {
                            list.insert_lower_bound(now);
                        }
                    }

                    // move to the next one
                    attr = now.next;
                }

                // special processing for XML attribute kiks in only when we have invisible parents
                if parent_visible {
                    // simple inheritance attributes - copy
                    if xml_lang_attr.is_none() {
                        xml_lang_attr = self.find_hidden_parent_attr(
                            cur.parent(),
                            "lang",
                            XML_XML_NAMESPACE.to_str().unwrap(),
                        );
                    }
                    if let Some(attr) = xml_lang_attr {
                        list.insert_lower_bound(attr);
                    }
                    if xml_space_attr.is_none() {
                        xml_space_attr = self.find_hidden_parent_attr(
                            cur.parent(),
                            "space",
                            XML_XML_NAMESPACE.to_str().unwrap(),
                        );
                    }
                    if let Some(attr) = xml_space_attr {
                        list.insert_lower_bound(attr);
                    }

                    // base uri attribute - fix up
                    if xml_base_attr.is_none() {
                        // if we don't have base uri attribute, check if we have a "hidden" one above
                        xml_base_attr = self.find_hidden_parent_attr(
                            cur.parent(),
                            "base",
                            XML_XML_NAMESPACE.to_str().unwrap(),
                        );
                    }
                    if let Some(attr) = xml_base_attr {
                        xml_base_attr = self.fixup_base_attr(&attr);
                        if let Some(mut attr) = xml_base_attr {
                            list.insert_lower_bound(attr);

                            // note that we MUST delete returned attr node ourselves!
                            attr.next = attrs_to_delete;
                            attrs_to_delete = Some(attr);
                        }
                    }
                }
            }
        }

        // print out all elements from list
        list.walk(|data| self.print_attrs(*data));

        // Cleanup
        xml_free_prop_list(attrs_to_delete);
        0
    }

    /// Canonical XML v 1.0 (<http://www.w3.org/TR/xml-c14n>)
    ///
    /// # Element Nodes
    /// If the element is not in the node-set, then the result is obtained
    /// by processing the namespace axis, then the attribute axis, then
    /// processing the child nodes of the element that are in the node-set
    /// (in document order). If the element is in the node-set, then the result
    /// is an open angle bracket (<), the element QName, the result of
    /// processing the namespace axis, the result of processing the attribute
    /// axis, a close angle bracket (>), the result of processing the child
    /// nodes of the element that are in the node-set (in document order), an
    /// open angle bracket, a forward slash (/), the element QName, and a close
    /// angle bracket.
    ///
    /// Returns non-negative value on success or negative value on fail
    #[doc(alias = "xmlC14NProcessElementNode")]
    unsafe fn process_element_node(&mut self, cur: XmlNodePtr, visible: bool) -> i32 {
        let mut parent_is_doc = false;

        if !matches!((*cur).element_type(), XmlElementType::XmlElementNode) {
            xml_c14n_err_param("processing element node");
            return -1;
        }

        // Check relative relative namespaces:
        // implementations of XML canonicalization MUST report an operation
        // failure on documents containing relative namespace URIs.
        if self.check_for_relative_namespaces(&cur) < 0 {
            xml_c14n_err_internal("checking for relative namespaces");
            return -1;
        }

        let mut state: XmlC14NVisibleNsStack = XmlC14NVisibleNsStack::default();
        // Save ns_rendered stack position
        (*self.ns_rendered).save(&mut state);

        if visible {
            if self.parent_is_doc {
                // save this flag into the stack
                parent_is_doc = self.parent_is_doc;
                self.parent_is_doc = false;
                self.pos = XmlC14NPosition::XmlC14NInsideDocumentElement;
            }
            self.buf.borrow_mut().write_str("<");

            if let Some(prefix) = cur.ns.as_deref().and_then(|ns| ns.prefix()) {
                self.buf.borrow_mut().write_str(&prefix);
                self.buf.borrow_mut().write_str(":");
            }

            self.buf
                .borrow_mut()
                .write_str(CStr::from_ptr(cur.name as _).to_string_lossy().as_ref());
        }

        let ret = if !self.is_exclusive() {
            self.process_namespaces_axis(cur, visible)
        } else {
            self.exc_c14n_process_namespaces_axis(cur, visible)
        };
        if ret < 0 {
            xml_c14n_err_internal("processing namespaces axis");
            return -1;
        }
        // todo: shouldn't this go to "visible only"?
        if visible {
            (*self.ns_rendered).shift();
        }

        let ret = self.process_attrs_axis(cur, visible);
        if ret < 0 {
            xml_c14n_err_internal("processing attributes axis");
            return -1;
        }

        if visible {
            self.buf.borrow_mut().write_str(">");
        }
        if let Some(children) = (*cur).children() {
            let ret = self.process_node_list(Some(children));
            if ret < 0 {
                xml_c14n_err_internal("processing childrens list");
                return -1;
            }
        }
        if visible {
            self.buf.borrow_mut().write_str("</");
            if let Some(prefix) = cur.ns.as_deref().and_then(|ns| ns.prefix()) {
                self.buf.borrow_mut().write_str(&prefix);
                self.buf.borrow_mut().write_str(":");
            }

            self.buf
                .borrow_mut()
                .write_str(CStr::from_ptr(cur.name as _).to_string_lossy().as_ref());
            self.buf.borrow_mut().write_str(">");
            if parent_is_doc {
                // restore this flag from the stack for next node
                self.parent_is_doc = parent_is_doc;
                self.pos = XmlC14NPosition::XmlC14NAfterDocumentElement;
            }
        }

        // Restore ns_rendered stack position
        (*self.ns_rendered).restore(&state);
        0
    }

    /// Processes all nodes in the row starting from cur.
    ///
    /// Returns non-negative value on success or negative value on fail
    #[doc(alias = "xmlC14NProcessNodeList")]
    unsafe fn process_node_list(&mut self, mut cur: Option<XmlGenericNodePtr>) -> i32 {
        let mut ret = 0;
        while let Some(now) = cur {
            ret = self.process_node(now);
            cur = now.next();
            if ret < 0 {
                break;
            }
        }
        ret
    }

    /// Processes the given node
    ///
    /// Returns non-negative value on success or negative value on fail
    #[doc(alias = "xmlC14NProcessNode")]
    unsafe fn process_node(&mut self, cur: XmlGenericNodePtr) -> i32 {
        let mut ret: i32 = 0;

        let visible = self.is_visible(Some(cur), cur.parent());
        match (*cur).element_type() {
            XmlElementType::XmlElementNode => {
                let cur = XmlNodePtr::try_from(cur).unwrap();
                ret = self.process_element_node(cur, visible);
            }
            XmlElementType::XmlCDATASectionNode | XmlElementType::XmlTextNode => {
                // Text Nodes
                // the string value, except all ampersands are replaced
                // by &amp;, all open angle brackets (<) are replaced by &lt;, all closing
                // angle brackets (>) are replaced by &gt;, and all #xD characters are
                // replaced by &#xD;.

                let cur = XmlNodePtr::try_from(cur).unwrap();
                // cdata sections are processed as text nodes
                // todo: verify that cdata sections are included in XPath nodes set
                if visible && !cur.content.is_null() {
                    let buffer: *mut XmlChar = xml_c11n_normalize_text(cur.content);
                    if !buffer.is_null() {
                        self.buf
                            .borrow_mut()
                            .write_str(CStr::from_ptr(buffer as _).to_string_lossy().as_ref());
                        xml_free(buffer as _);
                    } else {
                        xml_c14n_err_internal("normalizing text node");
                        return -1;
                    }
                }
            }
            XmlElementType::XmlPINode => {
                // Processing Instruction (PI) Nodes-
                // The opening PI symbol (<?), the PI target name of the node,
                // a leading space and the string value if it is not empty, and
                // the closing PI symbol (?>). If the string value is empty,
                // then the leading space is not added. Also, a trailing #xA is
                // rendered after the closing PI symbol for PI children of the
                // root node with a lesser document order than the document
                // element, and a leading #xA is rendered before the opening PI
                // symbol of PI children of the root node with a greater document
                // order than the document element.
                if visible {
                    if matches!(self.pos, XmlC14NPosition::XmlC14NAfterDocumentElement) {
                        self.buf.borrow_mut().write_str("\x0A<?");
                    } else {
                        self.buf.borrow_mut().write_str("<?");
                    }

                    self.buf.borrow_mut().write_str(&(*cur).name().unwrap());
                    let cur = XmlNodePtr::try_from(cur).unwrap();
                    if !cur.content.is_null() && *cur.content != b'\0' {
                        self.buf.borrow_mut().write_str(" ");

                        /* todo: do we need to normalize pi? */
                        let buffer: *mut XmlChar = xml_c11n_normalize_pi(cur.content);
                        if !buffer.is_null() {
                            self.buf
                                .borrow_mut()
                                .write_str(CStr::from_ptr(buffer as _).to_string_lossy().as_ref());
                            xml_free(buffer as _);
                        } else {
                            xml_c14n_err_internal("normalizing pi node");
                            return -1;
                        }
                    }

                    if matches!(self.pos, XmlC14NPosition::XmlC14NBeforeDocumentElement) {
                        self.buf.borrow_mut().write_str("?>\x0A");
                    } else {
                        self.buf.borrow_mut().write_str("?>");
                    }
                }
            }
            XmlElementType::XmlCommentNode => {
                // Comment Nodes
                // Nothing if generating canonical XML without  comments. For
                // canonical XML with comments, generate the opening comment
                // symbol (<!--), the string value of the node, and the
                // closing comment symbol (-->). Also, a trailing #xA is rendered
                // after the closing comment symbol for comment children of the
                // root node with a lesser document order than the document
                // element, and a leading #xA is rendered before the opening
                // comment symbol of comment children of the root node with a
                // greater document order than the document element. (Comment
                // children of the root node represent comments outside of the
                // top-level document element and outside of the document type
                // declaration).
                if visible && self.with_comments {
                    if matches!(self.pos, XmlC14NPosition::XmlC14NAfterDocumentElement) {
                        self.buf.borrow_mut().write_str("\x0A<!--");
                    } else {
                        self.buf.borrow_mut().write_str("<!--");
                    }

                    let cur = XmlNodePtr::try_from(cur).unwrap();
                    if !cur.content.is_null() {
                        /* todo: do we need to normalize comment? */
                        let buffer: *mut XmlChar = xml_c11n_normalize_comment(cur.content);
                        if !buffer.is_null() {
                            self.buf
                                .borrow_mut()
                                .write_str(CStr::from_ptr(buffer as _).to_string_lossy().as_ref());
                            xml_free(buffer as _);
                        } else {
                            xml_c14n_err_internal("normalizing comment node");
                            return -1;
                        }
                    }

                    if matches!(self.pos, XmlC14NPosition::XmlC14NBeforeDocumentElement) {
                        self.buf.borrow_mut().write_str("-->\x0A");
                    } else {
                        self.buf.borrow_mut().write_str("-->");
                    }
                }
            }
            XmlElementType::XmlDocumentNode | XmlElementType::XmlDocumentFragNode => {
                // should be processed as document?
                if let Some(children) = (*cur).children() {
                    self.pos = XmlC14NPosition::XmlC14NBeforeDocumentElement;
                    self.parent_is_doc = true;
                    ret = self.process_node_list(Some(children));
                }
            }
            #[cfg(feature = "html")]
            XmlElementType::XmlHTMLDocumentNode => {
                // should be processed as document?
                if let Some(children) = (*cur).children() {
                    self.pos = XmlC14NPosition::XmlC14NBeforeDocumentElement;
                    self.parent_is_doc = true;
                    ret = self.process_node_list(Some(children));
                }
            }

            XmlElementType::XmlAttributeNode => {
                xml_c14n_err_invalid_node("XML_ATTRIBUTE_NODE", "processing node");
                return -1;
            }
            XmlElementType::XmlNamespaceDecl => {
                xml_c14n_err_invalid_node("XML_NAMESPACE_DECL", "processing node");
                return -1;
            }
            XmlElementType::XmlEntityRefNode => {
                xml_c14n_err_invalid_node("XML_ENTITY_REF_NODE", "processing node");
                return -1;
            }
            XmlElementType::XmlEntityNode => {
                xml_c14n_err_invalid_node("XML_ENTITY_NODE", "processing node");
                return -1;
            }

            XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlDTDNode
            | XmlElementType::XmlElementDecl
            | XmlElementType::XmlAttributeDecl
            | XmlElementType::XmlEntityDecl => {
                // should be ignored according to "W3C Canonical XML"
            }
            #[cfg(feature = "xinclude")]
            XmlElementType::XmlXIncludeStart | XmlElementType::XmlXIncludeEnd => {
                // should be ignored according to "W3C Canonical XML"
            }
            _ => {
                xml_c14n_err_unknown_node(cur.element_type() as i32, "processing node");
                return -1;
            }
        }

        ret
    }

    /// Prints out exclusive canonical namespace axis of the current node
    /// to the buffer from C14N context as follows
    ///
    /// Exclusive XML Canonicalization
    /// <http://www.w3.org/TR/xml-exc-c14n>
    ///
    /// If the element node is in the XPath subset then output the node in
    /// accordance with Canonical XML except for namespace nodes which are
    /// rendered as follows:
    ///
    /// 1. Render each namespace node iff:
    ///    * it is visibly utilized by the immediate parent element or one of
    ///      its attributes, or is present in InclusiveNamespaces PrefixList, and
    ///    * its prefix and value do not appear in ns_rendered. ns_rendered is
    ///      obtained by popping the state stack in order to obtain a list of
    ///      prefixes and their values which have already been rendered by
    ///      an output ancestor of the namespace node's parent element.
    /// 2. Append the rendered namespace node to the list ns_rendered of namespace
    ///    nodes rendered by output ancestors. Push ns_rendered on state stack and
    ///    recurse.
    /// 3. After the recursion returns, pop thestate stack.
    ///
    /// Returns 0 on success or -1 on fail.
    #[doc(alias = "xmlExcC14NProcessNamespacesAxis")]
    unsafe fn exc_c14n_process_namespaces_axis(
        &mut self,
        mut cur: XmlNodePtr,
        visible: bool,
    ) -> i32 {
        let mut has_empty_ns = false;
        let mut has_visibly_utilized_empty_ns = false;
        let mut has_empty_ns_in_inclusive_list = false;

        if !matches!(cur.element_type(), XmlElementType::XmlElementNode) {
            xml_c14n_err_param("processing namespaces axis (exc c14n)");
            return -1;
        }

        if !self.is_exclusive() {
            xml_c14n_err_param("processing namespaces axis (exc c14n)");
            return -1;
        }

        // Create a sorted list to store element namespaces
        let mut list = XmlList::new(
            None,
            Rc::new(|ns1, ns2| match xml_c14n_ns_compare(*ns1, *ns2) {
                ..0 => std::cmp::Ordering::Less,
                0 => std::cmp::Ordering::Equal,
                1.. => std::cmp::Ordering::Greater,
            }),
        );

        // process inclusive namespaces:
        // All namespace nodes appearing on inclusive ns list are
        // handled as provided in Canonical XML
        if let Some(inclusive_ns_prefixes) = self.inclusive_ns_prefixes.as_deref() {
            for prefix in inclusive_ns_prefixes {
                // Special values for namespace with empty prefix
                let prefix = if prefix == "#default" || prefix.is_empty() {
                    has_empty_ns_in_inclusive_list = true;
                    None
                } else {
                    Some(prefix.as_str())
                };

                let cur_doc = cur.doc;
                let ns = cur.search_ns(cur_doc, prefix);
                if let Some(ns) = ns.filter(|&ns| {
                    !xml_c14n_is_xml_ns(ns) && self.is_visible(Some(ns.into()), Some(cur.into()))
                }) {
                    let already_rendered = (*self.ns_rendered).find(Some(&*ns));
                    if visible {
                        // TODO: replace `cur` to `Rc<XmlNode>`
                        (*self.ns_rendered).add(ns, cur);
                    }
                    if !already_rendered {
                        list.insert_lower_bound(ns);
                    }
                    if (*ns).prefix().map_or(0, |pre| pre.len()) == 0 {
                        has_empty_ns = true;
                    }
                }
            }
        }

        // add node namespace
        let ns = if cur.ns.is_some() {
            cur.ns
        } else {
            let cur_doc = cur.doc;
            has_visibly_utilized_empty_ns = true;
            cur.search_ns(cur_doc, None)
        };
        if let Some(ns) = ns.filter(|&ns| !xml_c14n_is_xml_ns(ns)) {
            if visible
                && self.is_visible(Some(ns.into()), Some(cur.into()))
                && self.exc_c14n_visible_ns_stack_find(&self.ns_rendered, Some(&*ns)) == 0
            {
                list.insert_lower_bound(ns);
            }
            if visible {
                // TODO: replace `cur` to `Rc<XmlNode>`
                (*self.ns_rendered).add(ns, cur);
            }
            if (*ns).prefix().map_or(0, |pre| pre.len()) == 0 {
                has_empty_ns = true;
            }
        }

        // add attributes
        let mut attr = cur.properties;
        while let Some(cur_attr) = attr {
            // we need to check that attribute is visible and has non
            // default namespace (XML Namespaces: "default namespaces
            // do not apply directly to attributes")
            if let Some(attr_ns) = cur_attr.ns.filter(|&ns| {
                !xml_c14n_is_xml_ns(ns) && self.is_visible(Some(cur_attr.into()), Some(cur.into()))
            }) {
                let already_rendered =
                    self.exc_c14n_visible_ns_stack_find(&self.ns_rendered, Some(&*attr_ns));
                // TODO: replace `cur` to `Rc<XmlNode>`
                (*self.ns_rendered).add(attr_ns, cur);
                if already_rendered == 0 && visible {
                    list.insert_lower_bound(attr_ns);
                }
                if attr_ns.prefix().map_or(0, |pre| pre.len()) == 0 {
                    has_empty_ns = true;
                }
            } else if cur_attr.ns.map_or(false, |ns| {
                ns.prefix().map_or(0, |pre| pre.len()) == 0 && xml_strlen(ns.href) == 0
            }) {
                has_visibly_utilized_empty_ns = true;
            }
            attr = cur_attr.next;
        }

        // Process xmlns=""
        if visible
            && has_visibly_utilized_empty_ns
            && !has_empty_ns
            && !has_empty_ns_in_inclusive_list
        {
            let already_rendered =
                self.exc_c14n_visible_ns_stack_find(&self.ns_rendered, Some(&XmlNs::default()));
            if already_rendered == 0 {
                self.print_namespaces(&XmlNs::default());
            }
        } else if visible
            && !has_empty_ns
            && has_empty_ns_in_inclusive_list
            && !(*self.ns_rendered).find(Some(&XmlNs::default()))
        {
            self.print_namespaces(&XmlNs::default());
        }

        // print out all elements from list
        list.walk(|data| self.print_namespaces(data) != 0);

        // Cleanup
        0
    }

    #[doc(alias = "xmlC14NVisibleNsStackFind")]
    unsafe fn exc_c14n_visible_ns_stack_find(
        &self,
        cur: &XmlC14NVisibleNsStack,
        ns: Option<&XmlNs>,
    ) -> i32 {
        // if the default namespace xmlns="" is not defined yet then we do not want to print it out
        let prefix: *const XmlChar =
            if let Some(prefix) = ns.filter(|ns| ns.prefix().is_some()).map(|ns| ns.prefix) {
                prefix
            } else {
                c"".as_ptr() as _
            };
        let href: *const XmlChar =
            if let Some(href) = ns.filter(|ns| !ns.href.is_null()).map(|ns| ns.href) {
                href
            } else {
                c"".as_ptr() as _
            };
        let has_empty_ns: i32 =
            (xml_c14n_str_equal(prefix, null_mut()) && xml_c14n_str_equal(href, null_mut())) as _;

        for (i, &ns1) in cur.ns_tab[..cur.ns_cur_end].iter().enumerate().rev() {
            if xml_c14n_str_equal(prefix, ns1.prefix) {
                if xml_c14n_str_equal(href, ns1.href) {
                    let node = cur.node_tab[i];
                    return self.is_visible(Some(ns1.into()), Some(node.into())) as i32;
                } else {
                    return 0;
                }
            }
        }
        has_empty_ns
    }

    /// Finds an attribute in a hidden parent node.
    ///
    /// Returns a pointer to the attribute node (if found) or NULL otherwise.
    #[doc(alias = "xmlC14NFindHiddenParentAttr")]
    unsafe fn find_hidden_parent_attr(
        &self,
        mut cur: Option<XmlGenericNodePtr>,
        name: &str,
        ns: &str,
    ) -> Option<XmlAttrPtr> {
        while let Some(now) = cur.filter(|&now| !self.is_visible(Some(now), now.parent())) {
            if let Ok(now) = XmlNodePtr::try_from(now) {
                if let Some(res) = now.has_ns_prop(name, Some(ns)) {
                    // Is this `unwrap` OK ????
                    return Some(res.unwrap());
                }
            }

            cur = now.parent();
        }

        None
    }

    /// Fixes up the xml:base attribute
    ///
    /// Returns the newly created attribute or NULL
    #[doc(alias = "xmlC14NFixupBaseAttr")]
    unsafe fn fixup_base_attr(&mut self, xml_base_attr: &XmlAttr) -> Option<XmlAttrPtr> {
        let Some(parent) = xml_base_attr.parent() else {
            xml_c14n_err_param("processing xml:base attribute");
            return None;
        };

        // start from current value
        let Some(mut res) = xml_base_attr
            .children()
            .and_then(|c| c.get_string(XmlDocPtr::from_raw(self.doc).unwrap(), 1))
        else {
            xml_c14n_err_internal("processing xml:base attribute - can't get attr value");
            return None;
        };

        // go up the stack until we find a node that we rendered already
        let mut cur = parent.parent();
        while let Some(cur_node) = cur.filter(|&cur| !self.is_visible(Some(cur), cur.parent())) {
            if let Ok(cur) = XmlNodePtr::try_from(cur_node) {
                if let Some(attr) = cur.has_ns_prop("base", XML_XML_NAMESPACE.to_str().ok()) {
                    // get attr value
                    let Some(mut tmp_str) = (match attr {
                        Ok(attr) => attr
                            .children()
                            .and_then(|c| c.get_string(XmlDocPtr::from_raw(self.doc).unwrap(), 1)),
                        Err(attr) => attr
                            .children()
                            .and_then(|c| c.get_string(XmlDocPtr::from_raw(self.doc).unwrap(), 1)),
                    }) else {
                        xml_c14n_err_internal(
                            "processing xml:base attribute - can't get attr value",
                        );
                        return None;
                    };

                    // we need to add '/' if our current base uri ends with '..' or '.'
                    // to ensure that we are forced to go "up" all the time
                    let tmp_str_len = tmp_str.len();
                    if tmp_str_len > 1 && tmp_str.as_bytes()[tmp_str_len - 2] == b'.' {
                        tmp_str.push('/');
                    }

                    // build uri
                    let Some(tmp_str2) = build_uri(&res, &tmp_str) else {
                        xml_c14n_err_internal(
                            "processing xml:base attribute - can't construct uri",
                        );
                        return None;
                    };

                    res = tmp_str2;
                }
            }

            // next
            cur = cur_node.parent();
        }

        // check if result uri is empty or not
        if res.is_empty() {
            return None;
        }

        // create and return the new attribute node
        let res = CString::new(res).unwrap();
        let Some(attr) = xml_new_ns_prop(None, xml_base_attr.ns, "base", res.as_ptr() as *const u8)
        else {
            xml_c14n_err_internal("processing xml:base attribute - can't construct attribute");
            return None;
        };

        // done
        Some(attr)
    }

    /// Prints out canonical attribute urrent node to the
    /// buffer from C14N context as follows
    ///
    /// Canonical XML v 1.0 (<http://www.w3.org/TR/xml-c14n>)
    ///
    /// Returns 1 on success or 0 on fail.
    #[doc(alias = "xmlC14NPrintAttrs")]
    unsafe fn print_attrs(&mut self, attr: XmlAttrPtr) -> bool {
        let buffer: *mut XmlChar;

        self.buf.borrow_mut().write_str(" ");
        if let Some(prefix) = attr
            .ns
            .as_deref()
            .and_then(|ns| ns.prefix())
            .filter(|p| !p.is_empty())
        {
            self.buf.borrow_mut().write_str(&prefix);
            self.buf.borrow_mut().write_str(":");
        }

        self.buf.borrow_mut().write_str(&attr.name().unwrap());
        self.buf.borrow_mut().write_str("=\"");

        // todo: should we log an error if value==NULL ?
        if let Some(value) = attr
            .children()
            .and_then(|c| c.get_string(XmlDocPtr::from_raw(self.doc).unwrap(), 1))
        {
            let value = CString::new(value).unwrap();
            buffer = xml_c11n_normalize_attr(value.as_ptr() as *const u8);
            if !buffer.is_null() {
                self.buf
                    .borrow_mut()
                    .write_str(CStr::from_ptr(buffer as _).to_string_lossy().as_ref());
                xml_free(buffer as _);
            } else {
                xml_c14n_err_internal("normalizing attributes axis");
                return false;
            }
        }
        self.buf.borrow_mut().write_str("\"");
        true
    }
}

pub type XmlC14NCtxPtr<'a, T> = *mut XmlC14NCtx<'a, T>;
#[repr(C)]
pub enum XmlC14NNormalizationMode {
    XmlC14NNormalizeAttr = 0,
    XmlC14NNormalizeComment = 1,
    XmlC14NNormalizePI = 2,
    XmlC14NNormalizeText = 3,
}

unsafe fn xml_c14n_is_node_in_nodeset(
    nodes: &Option<&mut XmlNodeSet>,
    node: Option<XmlGenericNodePtr>,
    parent: Option<XmlGenericNodePtr>,
) -> i32 {
    if let (Some(nodes), Some(node)) = (nodes, node) {
        if let Ok(node) = XmlNsPtr::try_from(node) {
            let mut ns = XmlNs { ..*node };
            ns.node = ns.next.map(|ns| ns.into());

            // this is a libxml hack! check xpath.c for details
            if let Some(parent) =
                parent.filter(|p| p.element_type() == XmlElementType::XmlAttributeNode)
            {
                ns.node = parent.parent();
                // ns.next = parent.parent().map_or(null_mut(), |p| p.as_ptr()) as *mut XmlNs;
            } else if let Some(parent) = parent {
                ns.node = Some(parent);
                // ns.next = parent.as_ptr() as *mut XmlNs;
            }

            // If the input is an XPath node-set, then the node-set must explicitly
            // contain every node to be rendered to the canonical form.
            return nodes.contains(Some(
                XmlNsPtr::from_raw(&raw mut ns).unwrap().unwrap().into(),
            )) as i32;
        } else {
            return nodes.contains(Some(node)) as i32;
        }
    }
    1
}

/// Dumps the canonized image of given XML document into the provided buffer.  
/// For details see "Canonical XML" (<http://www.w3.org/TR/xml-c14n>) or
/// "Exclusive XML Canonicalization" (<http://www.w3.org/TR/xml-exc-c14n>)
///
/// Returns non-negative value on success or a negative value on fail
#[doc(alias = "xmlC14NDocSaveTo")]
pub unsafe fn xml_c14n_doc_save_to<'a>(
    doc: &'a mut XmlDoc,
    nodes: Option<&mut XmlNodeSet>,
    mode: XmlC14NMode,
    inclusive_ns_prefixes: Option<Vec<String>>,
    with_comments: bool,
    buf: Rc<RefCell<XmlOutputBuffer<'a>>>,
) -> i32 {
    xml_c14n_execute(
        doc,
        xml_c14n_is_node_in_nodeset,
        nodes,
        mode,
        inclusive_ns_prefixes,
        with_comments,
        buf,
    )
}

/// Handle a redefinition of param error
#[doc(alias = "xmlC14NErrParam")]
unsafe fn xml_c14n_err_param(extra: &str) {
    __xml_raise_error!(
        None,
        None,
        None,
        null_mut(),
        None,
        XmlErrorDomain::XmlFromC14N,
        XmlParserErrors::XmlErrInternalError,
        XmlErrorLevel::XmlErrError,
        None,
        0,
        Some(extra.to_owned().into()),
        None,
        None,
        0,
        0,
        "Invalid parameter : {}\n",
        extra
    );
}

/// Handle a redefinition of memory error
#[doc(alias = "xmlC14NErrMemory")]
unsafe fn xml_c14n_err_memory(extra: &str) {
    __xml_raise_error!(
        None,
        None,
        None,
        null_mut(),
        None,
        XmlErrorDomain::XmlFromC14N,
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

/// Handle a redefinition of internal error
#[doc(alias = "xmlC14NErrInternal")]
unsafe fn xml_c14n_err_internal(extra: &str) {
    __xml_raise_error!(
        None,
        None,
        None,
        null_mut(),
        None,
        XmlErrorDomain::XmlFromC14N,
        XmlParserErrors::XmlErrInternalError,
        XmlErrorLevel::XmlErrError,
        None,
        0,
        Some(extra.to_owned().into()),
        None,
        None,
        0,
        0,
        "Internal error : {}\n",
        extra
    );
}

/// Dumps the canonized image of given XML document into memory.  
/// For details see "Canonical XML" (<http://www.w3.org/TR/xml-c14n>) or
/// "Exclusive XML Canonicalization" (<http://www.w3.org/TR/xml-exc-c14n>)
///
/// Returns the number of bytes written on success or a negative value on fail
#[doc(alias = "xmlC14NDocDumpMemory")]
pub unsafe fn xml_c14n_doc_dump_memory(
    doc: &mut XmlDoc,
    nodes: Option<&mut XmlNodeSet>,
    mode: XmlC14NMode,
    inclusive_ns_prefixes: Option<Vec<String>>,
    with_comments: bool,
    doc_txt_ptr: &mut String,
) -> i32 {
    let mut ret: i32;

    doc_txt_ptr.clear();

    // create memory buffer with UTF8 (default) encoding
    let Some(buf) = XmlOutputBuffer::from_wrapped_encoder(None) else {
        xml_c14n_err_memory("creating output buffer");
        return -1;
    };
    let buf = Rc::new(RefCell::new(buf));

    // canonize document and write to buffer
    ret = xml_c14n_doc_save_to(
        doc,
        nodes,
        mode,
        inclusive_ns_prefixes,
        with_comments,
        buf.clone(),
    );
    if ret < 0 {
        xml_c14n_err_internal("saving doc to output buffer");
        buf.borrow_mut().flush();
        return -1;
    }

    ret = buf.borrow().buffer.map_or(0, |buf| buf.len() as i32);
    if ret >= 0 {
        if let Some(buffer) = buf.borrow_mut().buffer.take() {
            let mut bytes = vec![];
            buffer.dump(Some(&mut bytes)).ok();
            let text = String::from_utf8(bytes);
            match text {
                Ok(text) => *doc_txt_ptr = text,
                Err(_) => {
                    xml_c14n_err_memory("copying canonicalized document");
                    return -1;
                }
            }
        }
    }

    ret
}

/// Dumps the canonized image of given XML document into the file.  
/// For details see "Canonical XML" (<http://www.w3.org/TR/xml-c14n>) or
/// "Exclusive XML Canonicalization" (<http://www.w3.org/TR/xml-exc-c14n>)
///
/// Returns the number of bytes written success or a negative value on fail
#[doc(alias = "xmlC14NDocSave")]
pub unsafe fn xml_c14n_doc_save(
    doc: &mut XmlDoc,
    nodes: Option<&mut XmlNodeSet>,
    mode: XmlC14NMode,
    inclusive_ns_prefixes: Option<Vec<String>>,
    with_comments: bool,
    filename: *const c_char,
    compression: i32,
) -> i32 {
    if filename.is_null() {
        xml_c14n_err_param("saving doc");
        return -1;
    }

    // save the content to a temp buffer, use default UTF8 encoding.
    let filename = CStr::from_ptr(filename).to_string_lossy();
    let Some(buf) = XmlOutputBuffer::from_uri(filename.as_ref(), None, compression) else {
        xml_c14n_err_internal("creating temporary filename");
        return -1;
    };

    let buf = Rc::new(RefCell::new(buf));
    // canonize document and write to buffer
    let ret = xml_c14n_doc_save_to(
        doc,
        nodes,
        mode,
        inclusive_ns_prefixes,
        with_comments,
        buf.clone(),
    );
    if ret < 0 {
        xml_c14n_err_internal("canonize document to buffer");
        buf.borrow_mut().flush();
        return -1;
    }

    // get the numbers of bytes written
    let is_ok = buf.borrow().error.is_ok();
    if is_ok {
        buf.borrow_mut().flush();
        buf.borrow().written
    } else {
        -1
    }
}

/// Handle a redefinition of attribute error
#[doc(alias = "xmlC14NErr")]
unsafe fn xml_c14n_err<T>(
    ctxt: XmlC14NCtxPtr<'_, T>,
    node: Option<XmlGenericNodePtr>,
    error: XmlParserErrors,
    msg: &str,
) {
    if !ctxt.is_null() {
        (*ctxt).error = error;
    }
    __xml_raise_error!(
        None,
        None,
        None,
        ctxt as _,
        node,
        XmlErrorDomain::XmlFromC14N,
        error,
        XmlErrorLevel::XmlErrError,
        None,
        0,
        None,
        None,
        None,
        0,
        0,
        msg,
    );
}

unsafe fn xml_c14n_visible_ns_stack_destroy(cur: XmlC14NVisibleNsStackPtr) {
    if cur.is_null() {
        xml_c14n_err_param("destroying namespaces stack");
        return;
    }
    drop_in_place(cur);
    xml_free(cur as _);
}

/// Cleanups the C14N context object.
#[doc(alias = "xmlC14NFreeCtx")]
unsafe fn xml_c14n_free_ctx<T>(ctx: XmlC14NCtxPtr<'_, T>) {
    if ctx.is_null() {
        xml_c14n_err_param("freeing context");
        return;
    }

    drop_in_place(ctx);
    xml_free(ctx as _);
}

/// Creates new C14N context object to store C14N parameters.
///
/// Returns pointer to newly created object (success) or NULL (fail)
#[doc(alias = "xmlC14NNewCtx")]
unsafe fn xml_c14n_new_ctx<'a, T>(
    doc: &'a mut XmlDoc,
    is_visible_callback: Option<XmlC14NIsVisibleCallback<T>>,
    user_data: T,
    mode: XmlC14NMode,
    inclusive_ns_prefixes: Option<Vec<String>>,
    with_comments: bool,
    buf: Rc<RefCell<XmlOutputBuffer<'a>>>,
) -> Option<XmlC14NCtx<'a, T>> {
    // Validate the encoding output buffer encoding
    if buf.borrow().encoder.is_some() {
        xml_c14n_err::<T>(
            null_mut(),
            Some(XmlDocPtr::from_raw(doc).unwrap().unwrap().into()),
            XmlParserErrors::XmlC14NRequiresUtf8,
            "xmlC14NNewCtx: output buffer encoder != NULL but C14N requires UTF8 output\n",
        );
        return None;
    }

    // initialize C14N context
    let mut context = XmlC14NCtx {
        doc,
        with_comments,
        is_visible_callback,
        user_data,
        buf,
        parent_is_doc: true,
        pos: XmlC14NPosition::XmlC14NBeforeDocumentElement,
        ns_rendered: Box::new(XmlC14NVisibleNsStack::default()),
        mode: XmlC14NMode::XmlC14N1_0,
        inclusive_ns_prefixes: None,
        error: XmlParserErrors::default(),
    };

    // Set "mode" flag and remember list of inclusive prefixes for exclusive c14n
    context.mode = mode;
    if context.is_exclusive() {
        context.inclusive_ns_prefixes = inclusive_ns_prefixes;
    }
    Some(context)
}

/// Handle a redefinition of relative namespace error
#[doc(alias = "xmlC14NErrRelativeNamespace")]
unsafe fn xml_c14n_err_relative_namespace(ns_uri: &str) {
    __xml_raise_error!(
        None,
        None,
        None,
        null_mut(),
        None,
        XmlErrorDomain::XmlFromC14N,
        XmlParserErrors::XmlC14NRelativeNamespace,
        XmlErrorLevel::XmlErrError,
        None,
        0,
        None,
        None,
        None,
        0,
        0,
        "Relative namespace UR is invalid here : {}\n",
        ns_uri
    );
}

/// Compares the namespaces by names (prefixes).
///
/// Returns -1 if ns1 < ns2, 0 if ns1 == ns2 or 1 if ns1 > ns2.
#[doc(alias = "xmlC14NNsCompare")]
unsafe fn xml_c14n_ns_compare(ns1: XmlNsPtr, ns2: XmlNsPtr) -> i32 {
    if ns1 == ns2 {
        return 0;
    }
    // if ns1.is_null() {
    //     return -1;
    // }
    // if ns2.is_null() {
    //     return 1;
    // }

    xml_strcmp(ns1.prefix, ns2.prefix)
}

/// Check whether `ns` is a default 'xml:' namespace with `href="http://www.w3.org/XML/1998/namespace"`.  
/// Return `true` if so, otherwise return `false`.
///
/// Please refer to the document of `xmlC14NIsXmlNs` for original libxml2.
/* todo: make it a define? */
#[doc(alias = "xmlC14NIsXmlNs")]
unsafe fn xml_c14n_is_xml_ns(ns: XmlNsPtr) -> bool {
    ns.prefix().as_deref() == Some("xml")
        && ns.href().as_deref() == Some(XML_XML_NAMESPACE.to_str().unwrap())
}

#[doc(alias = "xmlC14NStrEqual")]
unsafe fn xml_c14n_str_equal(mut str1: *const XmlChar, mut str2: *const XmlChar) -> bool {
    if str1 == str2 {
        return true;
    }
    if str1.is_null() {
        return *str2 == b'\0';
    }
    if str2.is_null() {
        return *str1 == b'\0';
    }
    while {
        if *str1 != *str2 {
            return false;
        }
        str1 = str1.add(1);
        str2 = str2.add(1);
        *str2.sub(1) != 0
    } {}
    true
}

const XML_NAMESPACES_DEFAULT: usize = 16;

/// Prints the given attribute to the output buffer from C14N context.
///
/// Returns -1 if attr1 < attr2, 0 if attr1 == attr2 or 1 if attr1 > attr2.
#[doc(alias = "xmlC14NAttrsCompare")]
unsafe fn xml_c14n_attrs_compare(attr1: XmlAttrPtr, attr2: XmlAttrPtr) -> i32 {
    // Simple cases
    if attr1 == attr2 {
        return 0;
    }
    // if attr1.is_null() {
    //     return -1;
    // }
    // if attr2.is_null() {
    //     return 1;
    // }

    if attr1.ns == attr2.ns {
        return xml_strcmp(attr1.name, attr2.name);
    }

    // Attributes in the default namespace are first
    // because the default namespace is not applied to
    // unqualified attributes
    let Some(attr1_ns) = attr1.ns else {
        return -1;
    };
    let Some(attr2_ns) = attr2.ns else {
        return 1;
    };
    if attr1_ns.prefix().is_none() {
        return -1;
    }
    if attr2_ns.prefix().is_none() {
        return 1;
    }

    let mut ret: i32 = xml_strcmp(attr1_ns.href, attr2_ns.href);
    if ret == 0 {
        ret = xml_strcmp(attr1.name, attr2.name);
    }
    ret
}

/// Checks whether `attr` is a default "xml:" namespace with `href="http://www.w3.org/XML/1998/namespace"`.  
/// Return `true` if so, otherwise return false.
/* todo: make it a define? */
#[doc(alias = "xmlC14NIsXmlAttr")]
unsafe fn xml_c14n_is_xml_attr(attr: XmlAttrPtr) -> bool {
    let ns = attr.ns;
    ns.map_or(false, |ns| xml_c14n_is_xml_ns(ns))
}

// Macro used to grow the current buffer.
macro_rules! grow_buffer_reentrant {
    ($buffer:expr, $buffer_size:expr) => {
        $buffer_size *= 2;
        $buffer = xml_realloc($buffer as _, $buffer_size as usize) as _;
        if $buffer.is_null() {
            xml_c14n_err_memory("growing buffer");
            return null_mut();
        }
    };
}

/// Converts a string to a canonical (normalized) format.  
/// The code is stolen from xmlEncodeEntitiesReentrant().  
/// Added normalization of \x09, \x0a, \x0A and the `mode` parameter
///
/// Returns a normalized string (caller is responsible for calling xmlFree())
/// or NULL if an error occurs
#[doc(alias = "xmlC11NNormalizeString")]
unsafe fn xml_c11n_normalize_string(
    input: *const XmlChar,
    mode: XmlC14NNormalizationMode,
) -> *mut XmlChar {
    let mut cur: *const XmlChar = input;

    if input.is_null() {
        return null_mut();
    }

    // allocate an translation buffer.
    let mut buffer_size: i32 = 1000;
    let mut buffer: *mut XmlChar = xml_malloc_atomic(buffer_size as usize) as _;
    if buffer.is_null() {
        xml_c14n_err_memory("allocating buffer");
        return null_mut();
    }
    let mut out: *mut XmlChar = buffer;

    while *cur != b'\0' {
        if out.offset_from(buffer) > buffer_size as isize - 10 {
            let index: i32 = out.offset_from(buffer) as _;

            grow_buffer_reentrant!(buffer, buffer_size);
            out = buffer.add(index as usize);
        }

        if *cur == b'<'
            && matches!(
                mode,
                XmlC14NNormalizationMode::XmlC14NNormalizeAttr
                    | XmlC14NNormalizationMode::XmlC14NNormalizeText
            )
        {
            *out = b'&';
            out = out.add(1);
            *out = b'l';
            out = out.add(1);
            *out = b't';
            out = out.add(1);
            *out = b';';
            out = out.add(1);
        } else if *cur == b'>' && matches!(mode, XmlC14NNormalizationMode::XmlC14NNormalizeText) {
            *out = b'&';
            out = out.add(1);
            *out = b'g';
            out = out.add(1);
            *out = b't';
            out = out.add(1);
            *out = b';';
            out = out.add(1);
        } else if *cur == b'&'
            && matches!(
                mode,
                XmlC14NNormalizationMode::XmlC14NNormalizeAttr
                    | XmlC14NNormalizationMode::XmlC14NNormalizeText
            )
        {
            *out = b'&';
            out = out.add(1);
            *out = b'a';
            out = out.add(1);
            *out = b'm';
            out = out.add(1);
            *out = b'p';
            out = out.add(1);
            *out = b';';
            out = out.add(1);
        } else if *cur == b'"' && matches!(mode, XmlC14NNormalizationMode::XmlC14NNormalizeAttr) {
            *out = b'&';
            out = out.add(1);
            *out = b'q';
            out = out.add(1);
            *out = b'u';
            out = out.add(1);
            *out = b'o';
            out = out.add(1);
            *out = b't';
            out = out.add(1);
            *out = b';';
            out = out.add(1);
        } else if *cur == b'\x09' && matches!(mode, XmlC14NNormalizationMode::XmlC14NNormalizeAttr)
        {
            *out = b'&';
            out = out.add(1);
            *out = b'#';
            out = out.add(1);
            *out = b'x';
            out = out.add(1);
            *out = b'9';
            out = out.add(1);
            *out = b';';
            out = out.add(1);
        } else if *cur == b'\x0A' && matches!(mode, XmlC14NNormalizationMode::XmlC14NNormalizeAttr)
        {
            *out = b'&';
            out = out.add(1);
            *out = b'#';
            out = out.add(1);
            *out = b'x';
            out = out.add(1);
            *out = b'A';
            out = out.add(1);
            *out = b';';
            out = out.add(1);
        } else if *cur == b'\x0D'
            && matches!(
                mode,
                XmlC14NNormalizationMode::XmlC14NNormalizeAttr
                    | XmlC14NNormalizationMode::XmlC14NNormalizeText
                    | XmlC14NNormalizationMode::XmlC14NNormalizeComment
                    | XmlC14NNormalizationMode::XmlC14NNormalizePI
            )
        {
            *out = b'&';
            out = out.add(1);
            *out = b'#';
            out = out.add(1);
            *out = b'x';
            out = out.add(1);
            *out = b'D';
            out = out.add(1);
            *out = b';';
            out = out.add(1);
        } else {
            // Works because on UTF-8, all extended sequences cannot
            // result in bytes in the ASCII range.
            *out = *cur;
            out = out.add(1);
        }
        cur = cur.add(1);
    }
    *out = 0;
    buffer
}

#[doc(alias = "xmlC11NNormalizeAttr")]
unsafe fn xml_c11n_normalize_attr(a: *const u8) -> *mut XmlChar {
    xml_c11n_normalize_string(a, XmlC14NNormalizationMode::XmlC14NNormalizeAttr)
}

unsafe fn xml_c11n_normalize_text(a: *const u8) -> *mut XmlChar {
    xml_c11n_normalize_string(a, XmlC14NNormalizationMode::XmlC14NNormalizeText)
}

unsafe fn xml_c11n_normalize_comment(a: *const u8) -> *mut XmlChar {
    xml_c11n_normalize_string(a, XmlC14NNormalizationMode::XmlC14NNormalizeComment)
}

unsafe fn xml_c11n_normalize_pi(a: *const u8) -> *mut XmlChar {
    xml_c11n_normalize_string(a, XmlC14NNormalizationMode::XmlC14NNormalizePI)
}

/// Handle a redefinition of invalid node error
#[doc(alias = "xmlC14NErrInvalidNode")]
unsafe fn xml_c14n_err_invalid_node(node_type: &str, extra: &str) {
    __xml_raise_error!(
        None,
        None,
        None,
        null_mut(),
        None,
        XmlErrorDomain::XmlFromC14N,
        XmlParserErrors::XmlC14NInvalidNode,
        XmlErrorLevel::XmlErrError,
        None,
        0,
        Some(extra.to_owned().into()),
        None,
        None,
        0,
        0,
        "Node {} is invalid here : {}\n",
        node_type,
        extra
    );
}

/// Handle a redefinition of unknown node error
#[doc(alias = "xmlC14NErrUnknownNode")]
unsafe fn xml_c14n_err_unknown_node(node_type: i32, extra: &str) {
    __xml_raise_error!(
        None,
        None,
        None,
        null_mut(),
        None,
        XmlErrorDomain::XmlFromC14N,
        XmlParserErrors::XmlC14NUnknowNode,
        XmlErrorLevel::XmlErrError,
        None,
        0,
        Some(extra.to_owned().into()),
        None,
        None,
        0,
        0,
        "Unknown node type {} found : {}\n",
        node_type,
        extra
    );
}

/// Dumps the canonized image of given XML document into the provided buffer.
/// For details see "Canonical XML" (<http://www.w3.org/TR/xml-c14n>) or
/// "Exclusive XML Canonicalization" (<http://www.w3.org/TR/xml-exc-c14n>)
///
/// Returns non-negative value on success or a negative value on fail
#[doc(alias = "xmlC14NExecute")]
pub unsafe fn xml_c14n_execute<'a, T>(
    doc: &'a mut XmlDoc,
    is_visible_callback: XmlC14NIsVisibleCallback<T>,
    user_data: T,
    mode: XmlC14NMode,
    inclusive_ns_prefixes: Option<Vec<String>>,
    with_comments: bool,
    buf: Rc<RefCell<XmlOutputBuffer<'a>>>,
) -> i32 {
    let mut ret: i32;

    //  Validate the encoding output buffer encoding
    if buf.borrow().encoder.is_some() {
        xml_c14n_err::<T>(
            null_mut(),
            Some(XmlDocPtr::from_raw(doc).unwrap().unwrap().into()),
            XmlParserErrors::XmlC14NRequiresUtf8,
            "xmlC14NExecute: output buffer encoder != NULL but C14N requires UTF8 output\n",
        );
        return -1;
    }

    let children = doc.children;
    // currently, `xml_c14n_new_ctx` checks only output buffer encoding.
    // It is already checked at this point, so buffer creation does not fail.
    let mut ctx = xml_c14n_new_ctx(
        doc,
        Some(is_visible_callback),
        user_data,
        mode,
        inclusive_ns_prefixes,
        with_comments,
        buf.clone(),
    )
    .unwrap();

    // Root Node
    // The root node is the parent of the top-level document element. The
    // result of processing each of its child nodes that is in the node-set
    // in document order. The root node does not generate a byte order mark,
    // XML declaration, nor anything from within the document type
    // declaration.
    if let Some(children) = children {
        ret = ctx.process_node_list(Some(children));
        if ret < 0 {
            xml_c14n_err_internal("processing docs children list");
            return -1;
        }
    }

    // Flush buffer to get number of bytes written
    ret = buf.borrow_mut().flush();
    if ret < 0 {
        xml_c14n_err_internal("flushing output buffer");
        return -1;
    }

    ret
}
