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

use std::{
    borrow::Cow,
    ffi::{CStr, CString},
    ops::{Deref, DerefMut},
    os::raw::c_void,
    ptr::{null, null_mut, NonNull},
    sync::atomic::Ordering,
};

use libc::memset;

use crate::{
    libxml::{
        globals::{xml_free, xml_malloc},
        valid::xml_remove_id,
        xmlstring::{xml_str_equal, xml_strcat, xml_strdup, xml_strncat, XmlChar},
    },
    tree::{xml_free_node_list, XmlAttribute},
};

use super::{
    xml_encode_attribute_entities, xml_encode_entities_reentrant, xml_free_node, xml_free_prop,
    xml_get_doc_entity, xml_is_blank_char, xml_ns_in_scope, xml_tree_err_memory, NodeCommon,
    XmlAttr, XmlAttributeType, XmlDoc, XmlElementType, XmlEntity, XmlNs, XML_CHECK_DTD,
    XML_LOCAL_NAMESPACE, XML_XML_NAMESPACE,
};

#[repr(C)]
pub struct XmlNode {
    pub _private: *mut c_void,        /* application data */
    pub(crate) typ: XmlElementType,   /* type number, must be second ! */
    pub name: *const XmlChar,         /* the name of the node, or the entity */
    children: Option<NodePtr>,        /* parent->childs link */
    last: Option<NodePtr>,            /* last child link */
    parent: Option<NodePtr>,          /* child->parent link */
    pub next: Option<NodePtr>,        /* next sibling link  */
    pub(crate) prev: Option<NodePtr>, /* previous sibling link  */
    pub doc: *mut XmlDoc,             /* the containing document */

    /* End of common part */
    pub(crate) ns: *mut XmlNs, /* pointer to the associated namespace */
    pub content: *mut XmlChar, /* the content */
    pub(crate) properties: *mut XmlAttr, /* properties list */
    pub ns_def: *mut XmlNs,    /* namespace definitions on this node */
    pub(crate) psvi: *mut c_void, /* for type/PSVI information */
    pub(crate) line: u16,      /* line number */
    pub(crate) extra: u16,     /* extra data for XPath/XSLT */
}

impl XmlNode {
    /// Checks whether this node is an empty or whitespace only (and possibly ignorable) text-node.
    #[doc(alias = "xmlIsBlankNode")]
    pub unsafe fn is_blank_node(&self) -> bool {
        if !matches!(
            self.element_type(),
            XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
        ) {
            return false;
        }
        if self.content.is_null() {
            return true;
        }
        let mut cur = self.content;
        while *cur != 0 {
            if !xml_is_blank_char(*cur as u32) {
                return false;
            }
            cur = cur.add(1);
        }

        true
    }

    /// Get line number of `self`.
    /// Try to override the limitation of lines being store in 16 bits ints
    ///
    /// Returns the line number if successful, -1 otherwise
    #[doc(alias = "xmlGetLineNoInternal")]
    unsafe fn get_line_no_internal(&self, depth: i32) -> i64 {
        let mut result: i64 = -1;

        if depth >= 5 {
            return -1;
        }

        if matches!(
            self.element_type(),
            XmlElementType::XmlElementNode
                | XmlElementType::XmlTextNode
                | XmlElementType::XmlCommentNode
                | XmlElementType::XmlPINode
        ) {
            if self.line == 65535 {
                if matches!(self.element_type(), XmlElementType::XmlTextNode)
                    && !self.psvi.is_null()
                {
                    result = self.psvi as isize as i64;
                } else if let Some(children) = self
                    .children()
                    .filter(|_| matches!(self.element_type(), XmlElementType::XmlElementNode))
                {
                    result = children.get_line_no_internal(depth + 1);
                } else if let Some(next) = self.next() {
                    result = next.get_line_no_internal(depth + 1);
                } else if let Some(prev) = self.prev() {
                    result = prev.get_line_no_internal(depth + 1);
                }
            }
            if result == -1 || result == 65535 {
                result = self.line as i64;
            }
        } else if let Some(prev) = self.prev().filter(|p| {
            matches!(
                p.element_type(),
                XmlElementType::XmlElementNode
                    | XmlElementType::XmlTextNode
                    | XmlElementType::XmlCommentNode
                    | XmlElementType::XmlPINode
            )
        }) {
            result = prev.get_line_no_internal(depth + 1);
        } else if let Some(parent) = self
            .parent()
            .filter(|p| matches!(p.element_type(), XmlElementType::XmlElementNode))
        {
            result = parent.get_line_no_internal(depth + 1);
        }

        result
    }

    /// Get line number of `self`.
    /// Try to override the limitation of lines being store in 16 bits ints
    /// if XML_PARSE_BIG_LINES parser option was used
    ///
    /// Returns the line number if successful, -1 otherwise
    #[doc(alias = "xmlGetLineNo")]
    pub unsafe fn get_line_no(&self) -> i64 {
        self.get_line_no_internal(0)
    }

    /// Build a structure based Path for the given node
    ///
    /// Returns the new path or `None` in case of error.  
    #[doc(alias = "xmlGetNodePath")]
    #[cfg(feature = "libxml_tree")]
    pub unsafe fn get_node_path(&self) -> Option<String> {
        let mut occur: i32;
        let mut generic: i32;

        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }

        let mut buffer = String::with_capacity(500);
        let mut buf = String::with_capacity(500);
        let mut cur = NodePtr::from_ptr(self as *const XmlNode as *mut XmlNode);
        let mut sep: &str;
        while let Some(current) = cur {
            let mut name: Cow<'_, str> = "".into();
            occur = 0;
            let next = if matches!(
                current.element_type(),
                XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode
            ) {
                if buffer.starts_with('/') {
                    break;
                }
                sep = "/";
                None
            } else if matches!(current.element_type(), XmlElementType::XmlElementNode) {
                generic = 0;
                sep = "/";
                if !current.ns.is_null() {
                    name = if let Some(prefix) = (*current.ns).prefix() {
                        Cow::Owned(format!("{prefix}:{}", current.name().unwrap(),))
                    } else {
                        // We cannot express named elements in the default
                        // namespace, so use "*".
                        generic = 1;
                        "*".into()
                    };
                } else {
                    name = current.name().unwrap();
                }

                // Thumbler index computation
                // TODO: the occurrence test seems bogus for namespaced names
                let mut tmp = current.prev();
                while let Some(now) = tmp {
                    if matches!(now.element_type(), XmlElementType::XmlElementNode)
                        && (generic != 0
                            || (current.name() == now.name()
                                && (now.ns == current.ns
                                    || (!now.ns.is_null()
                                        && !current.ns.is_null()
                                        && (*current.ns).prefix() == (*now.ns).prefix()))))
                    {
                        occur += 1;
                    }
                    tmp = now.prev();
                }
                if occur == 0 {
                    let mut tmp = current.next();
                    while let Some(now) = tmp.filter(|_| occur == 0) {
                        if matches!(now.element_type(), XmlElementType::XmlElementNode)
                            && (generic != 0
                                || (current.name() == now.name()
                                    && (now.ns == current.ns
                                        || (!now.ns.is_null()
                                            && !current.ns.is_null()
                                            && (*current.ns).prefix() == (*now.ns).prefix()))))
                        {
                            occur += 1;
                        }
                        tmp = now.next();
                    }
                    if occur != 0 {
                        occur = 1;
                    }
                } else {
                    occur += 1;
                }
                current.parent()
            } else if matches!(current.element_type(), XmlElementType::XmlCommentNode) {
                sep = "/";
                name = "comment()".into();

                // Thumbler index computation
                let mut tmp = current.prev();
                while let Some(now) = tmp {
                    if matches!(now.element_type(), XmlElementType::XmlCommentNode) {
                        occur += 1;
                    }
                    tmp = now.prev();
                }
                if occur == 0 {
                    let mut tmp = current.next();
                    while let Some(now) = tmp.filter(|_| occur == 0) {
                        if matches!(now.element_type(), XmlElementType::XmlCommentNode) {
                            occur += 1;
                        }
                        tmp = now.next();
                    }
                    if occur != 0 {
                        occur = 1;
                    }
                } else {
                    occur += 1;
                }
                current.parent()
            } else if matches!(
                current.element_type(),
                XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
            ) {
                sep = "/";
                name = "text()".into();

                // Thumbler index computation
                let mut tmp = current.prev();
                while let Some(now) = tmp {
                    if matches!(
                        now.element_type(),
                        XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
                    ) {
                        occur += 1;
                    }
                    tmp = now.prev();
                }
                // Evaluate if this is the only text- or CDATA-section-node;
                // if yes, then we'll get "text()".as_ptr() as _, otherwise "text()[1]".
                if occur == 0 {
                    let mut tmp = current.next();
                    while let Some(now) = tmp {
                        if matches!(
                            now.element_type(),
                            XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
                        ) {
                            occur = 1;
                            break;
                        }
                        tmp = now.next();
                    }
                } else {
                    occur += 1;
                }
                current.parent()
            } else if matches!(current.element_type(), XmlElementType::XmlPINode) {
                sep = "/";
                name = Cow::Owned(format!(
                    "processing-instruction('{}')",
                    current.name().unwrap()
                ));

                // Thumbler index computation
                let mut tmp = current.prev();
                while let Some(now) = tmp {
                    if matches!(now.element_type(), XmlElementType::XmlPINode)
                        && current.name() == now.name()
                    {
                        occur += 1;
                    }
                    tmp = now.prev();
                }
                if occur == 0 {
                    let mut tmp = current.next();
                    while let Some(now) = tmp.filter(|_| occur == 0) {
                        if matches!(now.element_type(), XmlElementType::XmlPINode)
                            && current.name() == now.name()
                        {
                            occur += 1;
                        }
                        tmp = now.next();
                    }
                    if occur != 0 {
                        occur = 1;
                    }
                } else {
                    occur += 1;
                }
                current.parent()
            } else if matches!(current.element_type(), XmlElementType::XmlAttributeNode) {
                sep = "/@";
                if !current.ns.is_null() {
                    name = if let Some(prefix) = (*current.ns).prefix() {
                        format!("{prefix}:{}", current.name().unwrap()).into()
                    } else {
                        format!("{}", current.name().unwrap()).into()
                    };
                } else {
                    name = current
                        .as_attribute_node()
                        .unwrap()
                        .as_ref()
                        .name()
                        .unwrap();
                }
                current.as_attribute_node().unwrap().as_ref().parent()
            } else {
                return None;
            };

            {
                use std::fmt::Write as _;
                if occur == 0 {
                    write!(buf, "{sep}{name}{buffer}").ok();
                } else {
                    write!(buf, "{sep}{name}[{occur}]{buffer}").ok();
                }
            }
            (buffer, buf) = (buf, buffer);
            buf.clear();
            cur = next;
        }
        Some(buffer)
    }

    /// Read the value of a node, this can be either the text carried
    /// directly by this node if it's a TEXT node or the aggregate string
    /// of the values carried by this node child's (TEXT and ENTITY_REF).  
    ///
    /// Entity references are substituted.
    ///
    /// Returns a new #XmlChar * or null_mut() if no content is available.  
    /// It's up to the caller to free the memory with xml_free().
    #[doc(alias = "xmlNodeGetContent")]
    pub unsafe fn get_content(&self) -> Option<String> {
        match self.element_type() {
            XmlElementType::XmlDocumentFragNode | XmlElementType::XmlElementNode => {
                let mut buf = String::with_capacity(64);
                self.get_content_to(&mut buf);
                Some(buf)
            }
            XmlElementType::XmlAttributeNode => {
                (*(self as *const XmlNode as *const XmlAttr)).get_prop_node_value_internal()
            }
            XmlElementType::XmlCommentNode | XmlElementType::XmlPINode => {
                if !self.content.is_null() {
                    return Some(
                        CStr::from_ptr(self.content as *const i8)
                            .to_string_lossy()
                            .into_owned(),
                    );
                }
                None
            }
            XmlElementType::XmlEntityRefNode => {
                // lookup entity declaration
                let ent: *mut XmlEntity =
                    xml_get_doc_entity(self.document(), &self.name().unwrap());
                if ent.is_null() {
                    return None;
                }

                let mut buf = String::new();
                self.get_content_to(&mut buf);
                Some(buf)
            }
            XmlElementType::XmlEntityNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlDTDNode
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => None,
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                let mut buf = String::new();
                self.get_content_to(&mut buf);
                Some(buf)
            }
            XmlElementType::XmlNamespaceDecl => {
                let ns = self.as_namespace_decl_node().unwrap();
                (!ns.as_ref().href.is_null()).then(|| {
                    CStr::from_ptr(ns.as_ref().href as *const i8)
                        .to_string_lossy()
                        .into_owned()
                })
            }
            XmlElementType::XmlElementDecl => {
                /* TODO !!! */
                None
            }
            XmlElementType::XmlAttributeDecl => {
                /* TODO !!! */
                None
            }
            XmlElementType::XmlEntityDecl => {
                /* TODO !!! */
                None
            }
            XmlElementType::XmlCDATASectionNode | XmlElementType::XmlTextNode => {
                if !self.content.is_null() {
                    return Some(
                        CStr::from_ptr(self.content as *const i8)
                            .to_string_lossy()
                            .into_owned(),
                    );
                }
                None
            }
            _ => unreachable!(),
        }
    }

    /// Read the value of a node `cur`, this can be either the text carried
    /// directly by this node if it's a TEXT node or the aggregate string
    /// of the values carried by this node child's (TEXT and ENTITY_REF).
    ///
    /// Entity references are substituted. Fills up the buffer `buf` with this value.
    ///
    /// Returns 0 in case of success and -1 in case of error.
    #[doc(alias = "xmlBufGetNodeContent")]
    pub unsafe fn get_content_to(&self, buf: &mut String) -> i32 {
        match self.element_type() {
            XmlElementType::XmlCDATASectionNode | XmlElementType::XmlTextNode => {
                buf.push_str(
                    CStr::from_ptr(self.content as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                );
            }
            XmlElementType::XmlDocumentFragNode | XmlElementType::XmlElementNode => {
                let mut tmp: *const XmlNode = self;

                while !tmp.is_null() {
                    match (*tmp).element_type() {
                        XmlElementType::XmlCDATASectionNode | XmlElementType::XmlTextNode => {
                            if !(*tmp).content.is_null() {
                                buf.push_str(
                                    CStr::from_ptr((*tmp).content as *const i8)
                                        .to_string_lossy()
                                        .as_ref(),
                                );
                            }
                        }
                        XmlElementType::XmlEntityRefNode => {
                            (*tmp).get_content_to(buf);
                        }
                        _ => {}
                    }
                    // Skip to next node
                    if let Some(children) = (*tmp).children().filter(|children| {
                        !matches!(children.element_type(), XmlElementType::XmlEntityDecl)
                    }) {
                        tmp = children.as_ptr();
                        continue;
                    }
                    if tmp == self {
                        break;
                    } else {
                        if let Some(next) = (*tmp).next() {
                            tmp = next.as_ptr();
                            continue;
                        }

                        'lp: while {
                            tmp = (*tmp).parent().map_or(null_mut(), |p| p.as_ptr());
                            if tmp.is_null() {
                                break 'lp;
                            }
                            if tmp == self {
                                tmp = null_mut();
                                break 'lp;
                            }
                            if let Some(next) = (*tmp).next() {
                                tmp = next.as_ptr();
                                break 'lp;
                            }

                            !tmp.is_null()
                        } {}
                    }
                }
            }
            XmlElementType::XmlAttributeNode => {
                let attr: *mut XmlAttr = self as *const XmlNode as _;
                let mut tmp = (*attr).children();

                while let Some(now) = tmp {
                    if matches!(now.element_type(), XmlElementType::XmlTextNode) {
                        buf.push_str(
                            CStr::from_ptr(now.content as *const i8)
                                .to_string_lossy()
                                .as_ref(),
                        );
                    } else {
                        now.get_content_to(buf);
                    }
                    tmp = now.next;
                }
            }
            XmlElementType::XmlCommentNode | XmlElementType::XmlPINode => {
                buf.push_str(
                    CStr::from_ptr(self.content as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                );
            }
            XmlElementType::XmlEntityRefNode => {
                // lookup entity declaration
                let ent: *mut XmlEntity =
                    xml_get_doc_entity(self.document(), &self.name().unwrap());
                if ent.is_null() {
                    return -1;
                }

                // an entity content can be any "well balanced chunk",
                // i.e. the result of the content [43] production:
                // http://www.w3.org/TR/REC-xml#NT-content
                // -> we iterate through child nodes and recursive call
                // xmlNodeGetContent() which handles all possible node types
                let mut tmp = (*ent).children();
                while let Some(now) = tmp {
                    now.get_content_to(buf);
                    tmp = now.next();
                }
            }
            XmlElementType::XmlEntityNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlDTDNode
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => {}
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                let mut next = self.children();
                while let Some(cur) = next {
                    if matches!(
                        cur.element_type(),
                        XmlElementType::XmlElementNode
                            | XmlElementType::XmlTextNode
                            | XmlElementType::XmlCDATASectionNode
                    ) {
                        cur.get_content_to(buf);
                    }
                    next = cur.next();
                }
            }
            XmlElementType::XmlNamespaceDecl => {
                buf.push_str(
                    &self
                        .as_namespace_decl_node()
                        .unwrap()
                        .as_ref()
                        .href()
                        .unwrap(),
                );
            }
            XmlElementType::XmlElementDecl
            | XmlElementType::XmlAttributeDecl
            | XmlElementType::XmlEntityDecl => {}
            _ => unreachable!(),
        }
        0
    }

    unsafe fn get_prop_node_internal(
        &self,
        name: &str,
        ns_name: Option<&str>,
        use_dtd: bool,
    ) -> *mut XmlAttr {
        let mut prop: *mut XmlAttr;

        if !matches!(self.element_type(), XmlElementType::XmlElementNode) {
            return null_mut();
        }

        if !self.properties.is_null() {
            prop = self.properties;
            if let Some(ns_name) = ns_name {
                // We want the attr to be in the specified namespace.
                let ns_name = CString::new(ns_name).unwrap();
                while !prop.is_null() {
                    if !(*prop).ns.is_null()
                        && (*prop).name().as_deref() == Some(name)
                        && ((*(*prop).ns).href == ns_name.as_ptr() as _
                            || xml_str_equal((*(*prop).ns).href, ns_name.as_ptr() as *const u8))
                    {
                        return prop;
                    }
                    prop = (*prop)
                        .next()
                        .map_or(null_mut(), |p| p.as_ptr() as *mut XmlAttr);
                }
            } else {
                // We want the attr to be in no namespace.
                while {
                    if (*prop).ns.is_null() && (*prop).name().as_deref() == Some(name) {
                        return prop;
                    }
                    prop = (*prop)
                        .next()
                        .map_or(null_mut(), |p| p.as_ptr() as *mut XmlAttr);
                    !prop.is_null()
                } {}
            }
        }

        #[cfg(feature = "libxml_tree")]
        {
            if !use_dtd {
                return null_mut();
            }
            // Check if there is a default/fixed attribute declaration in
            // the internal or external subset.
            if !self.doc.is_null() && !(*self.doc).int_subset.is_null() {
                let doc: *mut XmlDoc = self.doc;
                let mut attr_decl: *mut XmlAttribute = null_mut();
                let elem_qname: *mut XmlChar;
                let mut tmpstr: *mut XmlChar = null_mut();

                // We need the QName of the element for the DTD-lookup.
                if !self.ns.is_null() && !(*self.ns).prefix.is_null() {
                    tmpstr = xml_strdup((*self.ns).prefix);
                    tmpstr = xml_strcat(tmpstr, c":".as_ptr() as _);
                    tmpstr = xml_strcat(tmpstr, self.name);
                    if tmpstr.is_null() {
                        return null_mut();
                    }
                    elem_qname = tmpstr;
                } else {
                    elem_qname = self.name as _;
                }
                if let Some(ns_name) = ns_name {
                    if ns_name == XML_XML_NAMESPACE.to_string_lossy() {
                        // The XML namespace must be bound to prefix 'xml'.
                        attr_decl = (*(*doc).int_subset).get_qattr_desc(
                            CStr::from_ptr(elem_qname as *const i8)
                                .to_string_lossy()
                                .as_ref(),
                            name,
                            Some("xml"),
                        );
                        if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                            attr_decl = (*(*doc).ext_subset).get_qattr_desc(
                                CStr::from_ptr(elem_qname as *const i8)
                                    .to_string_lossy()
                                    .as_ref(),
                                name,
                                Some("xml"),
                            );
                        }
                    } else {
                        // The ugly case: Search using the prefixes of in-scope
                        // ns-decls corresponding to @nsName.
                        let Some(ns_list) = self.get_ns_list(self.doc) else {
                            if !tmpstr.is_null() {
                                xml_free(tmpstr as _);
                            }
                            return null_mut();
                        };
                        let ns_name = CString::new(ns_name).unwrap();
                        for cur in ns_list {
                            if xml_str_equal((*cur).href, ns_name.as_ptr() as *const u8) {
                                let prefix = (*cur).prefix();
                                attr_decl = (*(*doc).int_subset).get_qattr_desc(
                                    CStr::from_ptr(elem_qname as *const i8)
                                        .to_string_lossy()
                                        .as_ref(),
                                    name,
                                    prefix.as_deref(),
                                );
                                if !attr_decl.is_null() {
                                    break;
                                }
                                if !(*doc).ext_subset.is_null() {
                                    attr_decl = (*(*doc).ext_subset).get_qattr_desc(
                                        CStr::from_ptr(elem_qname as *const i8)
                                            .to_string_lossy()
                                            .as_ref(),
                                        name,
                                        prefix.as_deref(),
                                    );
                                    if !attr_decl.is_null() {
                                        break;
                                    }
                                }
                            }
                        }
                    }
                } else {
                    // The common and nice case: Attr in no namespace.
                    attr_decl = (*(*doc).int_subset).get_qattr_desc(
                        CStr::from_ptr(elem_qname as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                        name,
                        None,
                    );
                    if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                        attr_decl = (*(*doc).ext_subset).get_qattr_desc(
                            CStr::from_ptr(elem_qname as *const i8)
                                .to_string_lossy()
                                .as_ref(),
                            name,
                            None,
                        );
                    }
                }
                if !tmpstr.is_null() {
                    xml_free(tmpstr as _);
                }
                // Only default/fixed attrs are relevant.
                if !attr_decl.is_null() && !(*attr_decl).default_value.is_null() {
                    return attr_decl as _;
                }
            }
        }
        null_mut()
    }

    /// Search and get the value of an attribute associated to a node.  
    ///
    /// This does the entity substitution.
    ///
    /// This function looks in DTD attribute declaration for #FIXED or
    /// default declaration values unless DTD use has been turned off.
    ///
    /// # Note
    /// This function acts independently of namespaces associated to the attribute.  
    /// Use xmlGetNsProp() or xmlGetNoNsProp() for namespace aware processing.
    ///
    /// Returns the attribute value or NULL if not found.  
    /// It's up to the caller to free the memory with xml_free().
    #[doc(alias = "xmlGetProp")]
    pub unsafe fn get_prop(&self, name: &str) -> Option<String> {
        let prop = self.has_prop(name);
        if prop.is_null() {
            return None;
        }
        (*prop).get_prop_node_value_internal()
    }

    /// Search and get the value of an attribute associated to a node.
    ///
    /// This attribute has to be anchored in the namespace specified.
    ///
    /// This does the entity substitution.
    ///
    /// This function looks in DTD attribute declaration for #FIXED or
    /// default declaration values unless DTD use has been turned off.
    ///
    /// Returns the attribute value or NULL if not found.  
    /// It's up to the caller to free the memory with xml_free().
    #[doc(alias = "xmlGetNsProp")]
    pub unsafe fn get_ns_prop(&self, name: &str, name_space: Option<&str>) -> Option<String> {
        let prop =
            self.get_prop_node_internal(name, name_space, XML_CHECK_DTD.load(Ordering::Relaxed));
        if prop.is_null() {
            return None;
        }
        (*prop).get_prop_node_value_internal()
    }

    /// Search and get the value of an attribute associated to a node
    /// This does the entity substitution.
    ///
    /// This function looks in DTD attribute declaration for #FIXED or
    /// default declaration values unless DTD use has been turned off.  
    ///
    /// This function is similar to xmlGetProp except it will accept only
    /// an attribute in no namespace.
    ///
    /// Returns the attribute value or NULL if not found.  
    /// It's up to the caller to free the memory with xml_free().
    #[doc(alias = "xmlGetNoNsProp")]
    pub unsafe fn get_no_ns_prop(&self, name: &str) -> Option<String> {
        let prop = self.get_prop_node_internal(name, None, XML_CHECK_DTD.load(Ordering::Relaxed));
        if prop.is_null() {
            return None;
        }
        (*prop).get_prop_node_value_internal()
    }

    /// Search all the namespace applying to a given element.
    ///
    /// Returns an `Vec` of all the `xmlNsPtr` found.
    #[doc(alias = "xmlGetNsList")]
    #[cfg(any(feature = "libxml_tree", feature = "xpath", feature = "schema"))]
    pub unsafe fn get_ns_list(&self, _doc: *const XmlDoc) -> Option<Vec<*mut XmlNs>> {
        let mut cur: *mut XmlNs;

        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }

        let mut ret: Vec<*mut XmlNs> = vec![];
        let mut node = self as *const XmlNode;
        while !node.is_null() {
            if matches!((*node).element_type(), XmlElementType::XmlElementNode) {
                cur = (*node).ns_def;
                'b: while !cur.is_null() {
                    for i in 0..ret.len() {
                        if (*cur).prefix() == (*ret[i]).prefix() {
                            cur = (*cur).next;
                            continue 'b;
                        }
                    }
                    ret.push(cur);
                }
            }
            node = (*node).parent().map_or(null_mut(), |p| p.as_ptr());
        }
        Some(ret)
    }

    /// Searches the language of a node, i.e. the values of the xml:lang
    /// attribute or the one carried by the nearest ancestor.
    ///
    /// Returns a pointer to the lang value, or null_mut() if not found.  
    /// It's up to the caller to free the memory with xml_free().
    #[doc(alias = "xmlNodeGetLang")]
    pub unsafe fn get_lang(&self) -> Option<String> {
        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }
        let mut cur = NodePtr::from_ptr(self as *const XmlNode as *mut XmlNode);
        while let Some(now) = cur {
            let lang = now.get_ns_prop("lang", XML_XML_NAMESPACE.to_str().ok());
            if lang.is_some() {
                return lang;
            }
            cur = now.parent();
        }
        None
    }

    /// Searches the space preserving behaviour of a node, i.e. the values
    /// of the xml:space attribute or the one carried by the nearest ancestor.
    ///
    /// Returns -1 if xml:space is not inherited, 0 if "default", 1 if "preserve"
    #[doc(alias = "xmlNodeGetSpacePreserve")]
    pub unsafe fn get_space_preserve(&self) -> i32 {
        if !matches!(self.element_type(), XmlElementType::XmlElementNode) {
            return -1;
        }
        let mut cur = self as *const XmlNode;
        while !cur.is_null() {
            if let Some(space) = (*cur).get_ns_prop("space", XML_XML_NAMESPACE.to_str().ok()) {
                if space == "preserve" {
                    return 1;
                }
                if space == "default" {
                    return 0;
                }
            }
            cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
        }
        -1
    }

    /// Build the string equivalent to the text contained in the Node list
    /// made of TEXTs and ENTITY_REFs.
    ///
    /// Returns a pointer to the string copy, the caller must free it with `xml_free()`.
    #[doc(alias = "xmlNodeListGetString")]
    pub unsafe fn get_string(&self, doc: *mut XmlDoc, in_line: i32) -> Option<String> {
        let mut node: *const XmlNode = self;
        let mut ret: *mut XmlChar = null_mut();
        let mut ent: *mut XmlEntity;

        let attr = self
            .parent()
            .filter(|p| matches!(p.element_type(), XmlElementType::XmlAttributeNode))
            .is_some();

        while !node.is_null() {
            if matches!(
                (*node).element_type(),
                XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
            ) {
                if in_line != 0 {
                    ret = xml_strcat(ret, (*node).content);
                } else {
                    let buffer = if attr {
                        xml_encode_attribute_entities(doc, (*node).content)
                    } else {
                        xml_encode_entities_reentrant(doc, (*node).content)
                    };
                    if !buffer.is_null() {
                        ret = xml_strcat(ret, buffer);
                        xml_free(buffer as _);
                    }
                }
            } else if matches!((*node).element_type(), XmlElementType::XmlEntityRefNode) {
                if in_line != 0 {
                    ent = xml_get_doc_entity(doc, &(*node).name().unwrap());
                    if !ent.is_null() {
                        // an entity content can be any "well balanced chunk",
                        // i.e. the result of the content [43] production:
                        // http://www.w3.org/TR/REC-xml#NT-content.
                        // So it can contain text, CDATA section or nested
                        // entity reference nodes (among others).
                        // -> we recursive  call xmlNodeListGetString()
                        // which handles these types
                        let children = (*ent).children();
                        if let Some(buffer) = children.and_then(|c| c.get_string(doc, 1)) {
                            let buffer = CString::new(buffer).unwrap();
                            ret = xml_strcat(ret, buffer.as_ptr() as *const u8);
                        }
                    } else {
                        ret = xml_strcat(ret, (*node).content);
                    }
                } else {
                    let mut buf: [XmlChar; 2] = [0; 2];

                    buf[0] = b'&';
                    buf[1] = 0;
                    ret = xml_strncat(ret, buf.as_ptr() as _, 1);
                    ret = xml_strcat(ret, (*node).name);
                    buf[0] = b';';
                    buf[1] = 0;
                    ret = xml_strncat(ret, buf.as_ptr() as _, 1);
                }
            }
            node = (*node).next().map_or(null_mut(), |n| n.as_ptr());
        }
        let r = (!ret.is_null()).then(|| {
            CStr::from_ptr(ret as *const i8)
                .to_string_lossy()
                .into_owned()
        });
        xml_free(ret as _);
        r
    }

    /// Builds the string equivalent to the text contained in the Node list
    /// made of TEXTs and ENTITY_REFs, contrary to `xmlNodeListGetString()`
    /// this function doesn't do any character encoding handling.
    ///
    /// Returns a pointer to the string copy
    #[doc(alias = "xmlNodeListGetRawString")]
    #[cfg(feature = "libxml_tree")]
    pub unsafe fn get_raw_string(&self, doc: *const XmlDoc, in_line: i32) -> Option<String> {
        use super::xml_encode_special_chars;

        let mut node: *const XmlNode = self;
        let mut ret: *mut XmlChar = null_mut();
        let mut ent: *mut XmlEntity;

        while !node.is_null() {
            if matches!(
                (*node).element_type(),
                XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
            ) {
                if in_line != 0 {
                    ret = xml_strcat(ret, (*node).content);
                } else {
                    let buffer: *mut XmlChar = xml_encode_special_chars(doc, (*node).content);
                    if !buffer.is_null() {
                        ret = xml_strcat(ret, buffer);
                        xml_free(buffer as _);
                    }
                }
            } else if matches!((*node).element_type(), XmlElementType::XmlEntityRefNode) {
                if in_line != 0 {
                    ent = xml_get_doc_entity(doc, &(*node).name().unwrap());
                    if !ent.is_null() {
                        // an entity content can be any "well balanced chunk",
                        // i.e. the result of the content [43] production:
                        // http://www.w3.org/TR/REC-xml#NT-content.
                        // So it can contain text, CDATA section or nested
                        // entity reference nodes (among others).
                        // -> we recursive  call xmlNodeListGetRawString()
                        // which handles these types
                        let children = (*ent).children();
                        let buffer = children.and_then(|c| c.get_raw_string(doc, 1));
                        if let Some(buffer) = buffer.map(|b| CString::new(b).unwrap()) {
                            ret = xml_strcat(ret, buffer.as_ptr() as *const u8);
                        }
                    } else {
                        ret = xml_strcat(ret, (*node).content);
                    }
                } else {
                    let mut buf: [XmlChar; 2] = [0; 2];

                    buf[0] = b'&';
                    buf[1] = 0;
                    ret = xml_strncat(ret, buf.as_ptr(), 1);
                    ret = xml_strcat(ret, (*node).name);
                    buf[0] = b';';
                    buf[1] = 0;
                    ret = xml_strncat(ret, buf.as_ptr(), 1);
                }
            }
            node = (*node).next().map_or(null_mut(), |n| n.as_ptr());
        }
        let r = (!ret.is_null()).then(|| {
            CStr::from_ptr(ret as *const i8)
                .to_string_lossy()
                .into_owned()
        });
        xml_free(ret as _);
        r
    }

    /// Set (or reset) the name of a node.
    #[doc(alias = "xmlNodeSetName")]
    #[cfg(feature = "libxml_tree")]
    pub unsafe fn set_name(&mut self, name: &str) {
        use crate::libxml::{globals::xml_free, xmlstring::xml_strdup};

        match self.element_type() {
            XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlHTMLDocumentNode
            | XmlElementType::XmlNamespaceDecl
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => {
                return;
            }
            XmlElementType::XmlElementNode
            | XmlElementType::XmlAttributeNode
            | XmlElementType::XmlPINode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlDTDNode
            | XmlElementType::XmlDocumentNode
            | XmlElementType::XmlElementDecl
            | XmlElementType::XmlAttributeDecl
            | XmlElementType::XmlEntityDecl => {}
            _ => unreachable!(),
        }
        let mut freeme: *const XmlChar = null_mut();
        let name = CString::new(name).unwrap();
        if !self.name.is_null() {
            freeme = self.name;
        }
        self.name = xml_strdup(name.as_ptr() as *const u8);

        if !freeme.is_null() {
            xml_free(freeme as _);
        }
    }

    /// Set (or reset) an attribute carried by a node.
    ///
    /// If `name` has a prefix, then the corresponding namespace-binding will be used,
    /// if in scope; it is an error it there's no such ns-binding for the prefix in scope.
    ///
    /// Returns the attribute pointer.
    #[doc(alias = "xmlSetProp")]
    #[cfg(any(
        feature = "libxml_tree",
        feature = "xinclude",
        feature = "schema",
        feature = "html"
    ))]
    pub unsafe fn set_prop(&mut self, name: &str, value: Option<&str>) -> *mut XmlAttr {
        use crate::parser::split_qname2;

        if !matches!(self.element_type(), XmlElementType::XmlElementNode) {
            return null_mut();
        }

        // handle QNames
        if let Some((prefix, local)) = split_qname2(name) {
            let ns = self.search_ns(self.document(), Some(prefix));
            if !ns.is_null() {
                return self.set_ns_prop(ns, local, value);
            }
        }
        self.set_ns_prop(null_mut(), name, value)
    }

    /// Remove an attribute carried by a node.  
    /// This handles only attributes in no namespace.
    ///
    /// Returns 0 if successful, -1 if not found
    #[doc(alias = "xmlUnsetProp")]
    #[cfg(any(feature = "libxml_tree", feature = "schema"))]
    pub unsafe fn unset_prop(&mut self, name: &str) -> i32 {
        let prop = self.get_prop_node_internal(name, None, false);
        if prop.is_null() {
            return -1;
        }
        (*prop).unlink();
        xml_free_prop(prop);
        0
    }

    /// Set (or reset) an attribute carried by a node.  
    /// The ns structure must be in scope, this is not checked.
    ///
    /// Returns the attribute pointer.
    #[doc(alias = "xmlSetNsProp")]
    #[cfg(any(
        feature = "libxml_tree",
        feature = "xinclude",
        feature = "schema",
        feature = "html"
    ))]
    pub unsafe fn set_ns_prop(
        &mut self,
        ns: *mut XmlNs,
        name: &str,
        value: Option<&str>,
    ) -> *mut XmlAttr {
        use std::ptr::null;

        use crate::{
            libxml::valid::{xml_add_id, xml_remove_id},
            tree::{xml_free_node_list, xml_new_doc_text, xml_new_prop_internal, XmlAttributeType},
        };

        if !ns.is_null() && (*ns).href.is_null() {
            return null_mut();
        }
        let href = if !ns.is_null() {
            (*ns).href as *const i8
        } else {
            null_mut()
        };
        let prop = self.get_prop_node_internal(
            name,
            (!href.is_null())
                .then(|| CStr::from_ptr(href).to_string_lossy())
                .as_deref(),
            false,
        );
        if !prop.is_null() {
            // Modify the attribute's value.
            if matches!((*prop).atype, Some(XmlAttributeType::XmlAttributeID)) {
                xml_remove_id(self.document(), prop);
                (*prop).atype = Some(XmlAttributeType::XmlAttributeID);
            }
            if let Some(children) = (*prop).children() {
                xml_free_node_list(children.as_ptr());
            }
            (*prop).set_children(None);
            (*prop).set_last(None);
            (*prop).ns = ns;
            if let Some(value) = value {
                let value = CString::new(value).unwrap();
                (*prop).set_children(NodePtr::from_ptr(xml_new_doc_text(
                    self.doc,
                    value.as_ptr() as *const u8,
                )));
                (*prop).set_last(None);
                let mut tmp = (*prop).children();
                while let Some(mut now) = tmp {
                    now.parent = NodePtr::from_ptr(prop as *mut XmlNode);
                    if now.next().is_none() {
                        (*prop).set_last(Some(now));
                    }
                    tmp = now.next();
                }
            }
            if matches!((*prop).atype, Some(XmlAttributeType::XmlAttributeID)) {
                xml_add_id(null_mut(), self.document(), value.unwrap(), prop);
            }
            return prop;
        }
        // No equal attr found; create a new one.
        let value = value.map(|v| CString::new(v).unwrap());
        xml_new_prop_internal(
            self,
            ns,
            name,
            value.as_deref().map_or(null(), |v| v.as_ptr() as *const u8),
        )
    }

    /// Remove an attribute carried by a node.
    ///
    /// Returns 0 if successful, -1 if not found
    #[doc(alias = "xmlUnsetNsProp")]
    #[cfg(any(feature = "libxml_tree", feature = "schema"))]
    pub unsafe fn unset_ns_prop(&mut self, ns: *mut XmlNs, name: &str) -> i32 {
        let href = if !ns.is_null() {
            (*ns).href as *const i8
        } else {
            null_mut()
        };
        let prop = self.get_prop_node_internal(
            name,
            (!href.is_null())
                .then(|| CStr::from_ptr(href).to_string_lossy())
                .as_deref(),
            false,
        );
        if prop.is_null() {
            return -1;
        }
        (*prop).unlink();
        xml_free_prop(prop);
        0
    }

    /// Associate a namespace to a node, a posteriori.
    #[doc(alias = "xmlSetNs")]
    pub fn set_ns(&mut self, ns: *mut XmlNs) {
        if matches!(
            self.element_type(),
            XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode
        ) {
            self.ns = ns;
        }
    }

    /// Set (or reset) the base URI of a node, i.e. the value of the xml:base attribute.
    #[doc(alias = "xmlNodeSetBase")]
    #[cfg(any(feature = "libxml_tree", feature = "xinclude"))]
    pub unsafe fn set_base(&mut self, uri: Option<&str>) {
        use crate::uri::path_to_uri;

        match self.element_type() {
            XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlDTDNode
            | XmlElementType::XmlElementDecl
            | XmlElementType::XmlAttributeDecl
            | XmlElementType::XmlEntityDecl
            | XmlElementType::XmlPINode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlNamespaceDecl
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => {
                return;
            }
            XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode => {}
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                let doc = self.as_document_node().unwrap().as_mut();
                let url = uri.map(path_to_uri);
                doc.url = url.map(|url| url.into_owned());
                return;
            }
            _ => unreachable!(),
        }

        let ns = self.search_ns_by_href(self.document(), XML_XML_NAMESPACE.to_str().unwrap());
        if ns.is_null() {
            return;
        }
        if let Some(uri) = uri {
            let fixed = path_to_uri(uri);
            self.set_ns_prop(ns, "base", Some(&fixed));
        } else {
            self.set_ns_prop(ns, "base", None);
        }
    }

    /// Set the language of a node, i.e. the values of the xml:lang
    /// attribute.
    #[doc(alias = "xmlNodeSetLang")]
    #[cfg(feature = "libxml_tree")]
    pub unsafe fn set_lang(&mut self, lang: Option<&str>) {
        match self.element_type() {
            XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlDocumentNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlHTMLDocumentNode
            | XmlElementType::XmlDTDNode
            | XmlElementType::XmlElementDecl
            | XmlElementType::XmlAttributeDecl
            | XmlElementType::XmlEntityDecl
            | XmlElementType::XmlPINode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlNamespaceDecl
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => {
                return;
            }
            XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode => {}
            _ => unreachable!(),
        }
        let ns = self.search_ns_by_href(self.doc, XML_XML_NAMESPACE.to_str().unwrap());
        if ns.is_null() {
            return;
        }
        self.set_ns_prop(ns, "lang", lang);
    }

    /// Set (or reset) the space preserving behaviour of a node,   
    /// i.e. the value of the xml:space attribute.
    #[doc(alias = "xmlNodeSetSpacePreserve")]
    #[cfg(feature = "libxml_tree")]
    pub unsafe fn set_space_preserve(&mut self, val: i32) {
        match self.element_type() {
            XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlDocumentNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlHTMLDocumentNode
            | XmlElementType::XmlDTDNode
            | XmlElementType::XmlElementDecl
            | XmlElementType::XmlAttributeDecl
            | XmlElementType::XmlEntityDecl
            | XmlElementType::XmlPINode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlNamespaceDecl
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => {
                return;
            }
            XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode => {}
            _ => unreachable!(),
        }
        let ns = self.search_ns_by_href(self.doc, XML_XML_NAMESPACE.to_str().unwrap());
        if ns.is_null() {
            return;
        }
        match val {
            0 => {
                self.set_ns_prop(ns, "space", Some("default"));
            }
            1 => {
                self.set_ns_prop(ns, "space", Some("preserve"));
            }
            _ => {}
        }
    }

    /// Replace the content of a node.
    ///
    /// # Note
    /// `content` is supposed to be a piece of XML CDATA, so it allows entity references,
    /// but XML special chars need to be escaped first by using `xmlEncodeEntitiesReentrant()`
    /// resp. `xmlEncodeSpecialChars()`.
    #[doc(alias = "xmlNodeSetContent")]
    pub unsafe fn set_content(&mut self, content: *const XmlChar) {
        match self.element_type() {
            XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlElementNode
            | XmlElementType::XmlAttributeNode => {
                if let Some(children) = self.children() {
                    xml_free_node_list(children.as_ptr());
                }
                self.children = (!self.document().is_null())
                    .then(|| NodePtr::from_ptr((*self.document()).get_node_list(content)))
                    .flatten();
                if let Some(mut ulccur) = self.children() {
                    while let Some(next) = ulccur.next() {
                        ulccur.set_parent(NodePtr::from_ptr(self as *mut XmlNode));
                        ulccur = next;
                    }
                    ulccur.set_parent(NodePtr::from_ptr(self as *mut XmlNode));
                    self.set_last(Some(ulccur));
                } else {
                    self.set_last(None);
                }
            }
            XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlPINode
            | XmlElementType::XmlCommentNode => {
                if !self.content.is_null() {
                    xml_free(self.content as _);
                }
                if let Some(children) = self.children() {
                    xml_free_node_list(children.as_ptr());
                }
                self.set_last(None);
                self.set_children(None);
                if !content.is_null() {
                    self.content = xml_strdup(content);
                } else {
                    self.content = null_mut();
                }
                self.properties = null_mut();
            }
            XmlElementType::XmlDocumentNode
            | XmlElementType::XmlHTMLDocumentNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => {}
            XmlElementType::XmlNotationNode => {}
            XmlElementType::XmlDTDNode => {}
            XmlElementType::XmlNamespaceDecl => {}
            XmlElementType::XmlElementDecl => { /* TODO !!! */ }
            XmlElementType::XmlAttributeDecl => { /* TODO !!! */ }
            XmlElementType::XmlEntityDecl => { /* TODO !!! */ }
            _ => unreachable!(),
        }
    }

    /// Replace the content of a node.
    ///
    /// # Note
    /// `content` is supposed to be a piece of XML CDATA, so it allows entity
    /// references, but XML special chars need to be escaped first by using
    /// xmlEncodeEntitiesReentrant() resp. xmlEncodeSpecialChars().
    #[doc(alias = "xmlNodeSetContentLen")]
    #[cfg(feature = "libxml_tree")]
    pub unsafe fn set_content_len(&mut self, content: *const XmlChar, len: i32) {
        use crate::libxml::xmlstring::xml_strndup;

        use super::xml_free_node_list;

        match self.element_type() {
            XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlElementNode
            | XmlElementType::XmlAttributeNode => {
                if let Some(children) = self.children() {
                    xml_free_node_list(children.as_ptr());
                }
                self.children = (!self.document().is_null())
                    .then(|| {
                        NodePtr::from_ptr(
                            (*self.document()).get_node_list_with_strlen(content, len),
                        )
                    })
                    .flatten();
                if let Some(mut ulccur) = self.children() {
                    while let Some(next) = ulccur.next() {
                        ulccur.set_parent(NodePtr::from_ptr(self as *mut XmlNode));
                        ulccur = next;
                    }
                    ulccur.set_parent(NodePtr::from_ptr(self as *mut XmlNode));
                    self.set_last(Some(ulccur));
                } else {
                    self.set_last(None);
                }
            }
            XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlPINode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlNotationNode => {
                if !self.content.is_null() {
                    xml_free(self.content as _);
                }
                if let Some(children) = self.children() {
                    xml_free_node_list(children.as_ptr());
                }
                self.set_children(None);
                self.set_last(None);
                if !content.is_null() {
                    self.content = xml_strndup(content, len);
                } else {
                    self.content = null_mut();
                }
                self.properties = null_mut();
            }
            XmlElementType::XmlDocumentNode
            | XmlElementType::XmlDTDNode
            | XmlElementType::XmlHTMLDocumentNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlNamespaceDecl
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => {}
            XmlElementType::XmlElementDecl => { /* TODO !!! */ }
            XmlElementType::XmlAttributeDecl => { /* TODO !!! */ }
            XmlElementType::XmlEntityDecl => { /* TODO !!! */ }
            _ => unreachable!(),
        }
    }

    /// update all nodes under the tree to point to the right document
    #[doc(alias = "xmlSetTreeDoc")]
    pub unsafe fn set_doc(&mut self, doc: *mut XmlDoc) {
        let mut prop: *mut XmlAttr;

        if self.element_type() == XmlElementType::XmlNamespaceDecl {
            return;
        }
        if self.document() != doc {
            if matches!(self.element_type(), XmlElementType::XmlElementNode) {
                prop = self.properties;
                while !prop.is_null() {
                    if matches!((*prop).atype, Some(XmlAttributeType::XmlAttributeID)) {
                        xml_remove_id(self.document(), prop);
                    }

                    if (*prop).document() != doc {
                        (*prop).set_document(doc);
                    }
                    if let Some(mut children) = (*prop).children() {
                        children.set_doc_all_sibling(doc);
                    }

                    // TODO: ID attributes should be also added to the new
                    //       document, but this breaks things like xmlReplaceNode.
                    //       The underlying problem is that xmlRemoveID is only called
                    //       if a node is destroyed, not if it's unlinked.
                    // if (xmlIsID(doc, tree, prop)) {
                    //     XmlChar *idVal = xmlNodeListGetString(doc, (*prop).children, 1);
                    //     xmlAddID(null_mut(), doc, idVal, prop);
                    // }

                    prop = (*prop).next;
                }
            }
            if matches!(self.element_type(), XmlElementType::XmlEntityRefNode) {
                // Clear 'children' which points to the entity declaration
                // from the original document.
                self.set_children(None);
            } else if let Some(mut children) = self.children() {
                children.set_doc_all_sibling(doc);
            }

            // FIXME: self.ns should be updated as in xmlStaticCopyNode().
            self.set_document(doc);
        }
    }

    /// update all nodes in the list to point to the right document
    #[doc(alias = "xmlSetListDoc")]
    pub unsafe fn set_doc_all_sibling(&mut self, doc: *mut XmlDoc) {
        if self.element_type() == XmlElementType::XmlNamespaceDecl {
            return;
        }
        let mut cur = NodePtr::from_ptr(self as *mut XmlNode);
        while let Some(mut now) = cur {
            if now.document() != doc {
                now.set_doc(doc);
            }
            cur = now.next();
        }
    }

    /// Search an attribute associated to a node.  
    ///
    /// This function also looks in DTD attribute declaration for #FIXED or
    /// default declaration values unless DTD use has been turned off.
    ///
    /// Returns the attribute or the attribute declaration or NULL if neither was found.
    #[doc(alias = "xmlHasProp")]
    pub unsafe fn has_prop(&self, name: &str) -> *mut XmlAttr {
        if !matches!(self.element_type(), XmlElementType::XmlElementNode) {
            return null_mut();
        }
        // Check on the properties attached to the node
        let mut prop = self.properties;
        while !prop.is_null() {
            if (*prop).name().as_deref() == Some(name) {
                return prop;
            }
            prop = (*prop).next;
        }
        if !XML_CHECK_DTD.load(Ordering::Relaxed) {
            return null_mut();
        }

        // Check if there is a default declaration in the internal or external subsets
        let doc = self.document();
        if !doc.is_null() && !(*doc).int_subset.is_null() {
            let mut attr_decl =
                (*(*doc).int_subset).get_attr_desc(self.name().as_deref().unwrap(), name);
            if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                attr_decl =
                    (*(*doc).ext_subset).get_attr_desc(self.name().as_deref().unwrap(), name);
            }
            if !attr_decl.is_null() && !(*attr_decl).default_value.is_null() {
                // return attribute declaration only if a default value is given
                // (that includes #FIXED declarations)
                return attr_decl as *mut XmlAttr;
            }
        }
        null_mut()
    }

    /// Search for an attribute associated to a node.
    ///
    /// This attribute has to be anchored in the namespace specified.  
    /// This does the entity substitution.  
    /// This function looks in DTD attribute declaration for #FIXED or
    /// default declaration values unless DTD use has been turned off.  
    ///
    /// Note that a namespace of NULL indicates to use the default namespace.
    ///
    /// Returns the attribute or the attribute declaration or NULL if neither was found.
    #[doc(alias = "xmlHasNsProp")]
    pub unsafe fn has_ns_prop(&self, name: &str, namespace: Option<&str>) -> *mut XmlAttr {
        self.get_prop_node_internal(name, namespace, XML_CHECK_DTD.load(Ordering::Relaxed))
    }

    /// Add a new element `elem` to the list of siblings of `self`
    /// merging adjacent TEXT nodes (`elem` may be freed)  
    /// If the new element was already inserted in a document
    /// it is first unlinked from its existing context.
    ///
    /// See the note regarding namespaces in xmlAddChild.
    ///
    /// Returns the new element or NULL in case of error.
    #[doc(alias = "xmlAddSibling")]
    pub unsafe fn add_sibling(&mut self, elem: *mut XmlNode) -> *mut XmlNode {
        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        if elem.is_null() || ((*elem).element_type() == XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        let cur = self as *mut XmlNode;
        if cur == elem {
            return null_mut();
        }
        let mut cur = NodePtr::from_ptr(cur).unwrap();

        // Constant time is we can rely on the -> parent -> last to find the last sibling.
        if let Some(parent) = self.parent().filter(|p| {
            !matches!(self.element_type(), XmlElementType::XmlAttributeNode)
                && p.children().is_some()
                && p.last().filter(|l| l.next().is_none()).is_some()
        }) {
            cur = parent.last().unwrap();
        } else {
            while let Some(next) = cur.next() {
                cur = next;
            }
        }

        (*elem).unlink();

        if matches!((*cur).element_type(), XmlElementType::XmlTextNode)
            && matches!((*elem).element_type(), XmlElementType::XmlTextNode)
            && cur.name() == (*elem).name()
        {
            (*cur).add_content((*elem).content);
            xml_free_node(elem);
            return cur.as_ptr();
        } else if matches!((*elem).element_type(), XmlElementType::XmlAttributeNode) {
            return add_prop_sibling(cur.as_ptr(), cur.as_ptr(), elem);
        }

        if (*elem).document() != cur.document() {
            (*elem).set_doc((*cur).document());
        }
        let parent = cur.parent();
        (*elem).set_prev(Some(cur));
        (*elem).set_next(None);
        (*elem).set_parent(parent);
        (*cur).set_next(NodePtr::from_ptr(elem));
        if let Some(mut parent) = parent {
            parent.set_last(NodePtr::from_ptr(elem));
        }

        elem
    }

    /// Add a new node `elem` as the previous sibling of `self`
    /// merging adjacent TEXT nodes (`elem` may be freed)  
    /// If the new node was already inserted in a document it is
    /// first unlinked from its existing context.  
    /// If the new node is ATTRIBUTE, it is added into properties instead of children.  
    /// If there is an attribute with equal name, it is first destroyed.
    ///
    /// See the note regarding namespaces in xmlAddChild.
    ///
    /// Returns the new node or null_mut() in case of error.
    #[doc(alias = "xmlAddPrevSibling")]
    #[cfg(any(
        feature = "libxml_tree",
        feature = "html",
        feature = "schema",
        feature = "xinclude"
    ))]
    pub unsafe fn add_prev_sibling(&mut self, elem: *mut XmlNode) -> *mut XmlNode {
        use crate::libxml::{
            globals::xml_free,
            xmlstring::{xml_strcat, xml_strdup},
        };

        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }
        if elem.is_null() || ((*elem).element_type() == XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        if self as *mut XmlNode == elem {
            return null_mut();
        }

        (*elem).unlink();

        if matches!((*elem).element_type(), XmlElementType::XmlTextNode) {
            if matches!(self.element_type(), XmlElementType::XmlTextNode) {
                let mut tmp: *mut XmlChar;

                tmp = xml_strdup((*elem).content);
                tmp = xml_strcat(tmp, self.content);
                self.set_content(tmp);
                xml_free(tmp as _);
                xml_free_node(elem);
                return self as *mut XmlNode;
            }
            if let Some(mut prev) = self.prev().filter(|p| {
                matches!(p.element_type(), XmlElementType::XmlTextNode) && self.name == p.name
            }) {
                prev.add_content((*elem).content);
                xml_free_node(elem);
                return prev.as_ptr();
            }
        } else if matches!((*elem).element_type(), XmlElementType::XmlAttributeNode) {
            return add_prop_sibling(self.prev.map_or(null_mut(), |p| p.as_ptr()), self, elem);
        }

        if (*elem).document() != self.document() {
            (*elem).set_doc(self.document());
        }
        (*elem).set_parent(self.parent());
        (*elem).set_prev(self.prev());
        self.set_prev(NodePtr::from_ptr(elem));
        (*elem).next = Some(NodePtr::from(self));
        if let Some(mut prev) = (*elem).prev() {
            prev.set_next(NodePtr::from_ptr(elem));
        }
        if let Some(mut parent) = (*elem).parent().filter(|p| p.children() == (*elem).next()) {
            parent.set_children(NodePtr::from_ptr(elem));
        }
        elem
    }

    /// Add a new node `elem` as the next sibling of `self`.  
    /// If the new node was already inserted in a document it is
    /// first unlinked from its existing context.
    /// As a result of text merging `elem` may be freed.  
    /// If the new node is ATTRIBUTE, it is added into properties instead of children.  
    /// If there is an attribute with equal name, it is first destroyed.  
    ///
    /// See the note regarding namespaces in xmlAddChild.
    ///
    /// Returns the new node or NULL in case of error.
    #[doc(alias = "xmlAddNextSibling")]
    pub unsafe extern "C" fn add_next_sibling(&mut self, elem: *mut XmlNode) -> *mut XmlNode {
        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }
        if elem.is_null() || ((*elem).element_type() == XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        if self as *mut XmlNode == elem {
            return null_mut();
        }

        (*elem).unlink();

        if matches!((*elem).element_type(), XmlElementType::XmlTextNode) {
            if matches!(self.element_type(), XmlElementType::XmlTextNode) {
                self.add_content((*elem).content);
                xml_free_node(elem);
                return self as *mut XmlNode;
            }
            if let Some(mut next) = self.next().filter(|next| {
                matches!(next.element_type(), XmlElementType::XmlTextNode) && self.name == next.name
            }) {
                let mut tmp = xml_strdup((*elem).content);
                tmp = xml_strcat(tmp, next.content);
                next.set_content(tmp);
                xml_free(tmp as _);
                xml_free_node(elem);
                return next.as_ptr();
            }
        } else if matches!((*elem).element_type(), XmlElementType::XmlAttributeNode) {
            return add_prop_sibling(self, self, elem);
        }

        if (*elem).document() != self.document() {
            (*elem).set_doc(self.document());
        }
        (*elem).set_parent(self.parent());
        (*elem).set_prev(NodePtr::from_ptr(self as *mut XmlNode));
        (*elem).set_next(self.next());
        self.next = NodePtr::from_ptr(elem);
        if let Some(mut next) = (*elem).next() {
            next.set_prev(NodePtr::from_ptr(elem));
        }
        if let Some(mut parent) = (*elem)
            .parent()
            .filter(|p| p.last() == NodePtr::from_ptr(self as *mut XmlNode))
        {
            parent.set_last(NodePtr::from_ptr(elem));
        }
        elem
    }

    /// Add a list of node at the end of the child list of the parent
    /// merging adjacent TEXT nodes (`cur` may be freed)
    ///
    /// See the note regarding namespaces in xmlAddChild.
    ///
    /// Returns the last child or NULL in case of error.
    #[doc(alias = "xmlAddChildList")]
    pub unsafe fn add_child_list(&mut self, mut cur: *mut XmlNode) -> *mut XmlNode {
        let mut prev: *mut XmlNode;

        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        if cur.is_null() || matches!((*cur).element_type(), XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        // add the first element at the end of the children list.
        if self.children().is_none() {
            self.set_children(NodePtr::from_ptr(cur));
        } else {
            // If cur and self.last both are TEXT nodes, then merge them.
            if matches!((*cur).element_type(), XmlElementType::XmlTextNode)
                && matches!(
                    self.last().unwrap().element_type(),
                    XmlElementType::XmlTextNode
                )
                && (*cur).name() == self.last.unwrap().name()
            {
                self.last().unwrap().add_content((*cur).content);
                // if it's the only child, nothing more to be done.
                let Some(next) = (*cur).next() else {
                    xml_free_node(cur);
                    return self.last().map_or(null_mut(), |l| l.as_ptr());
                };
                prev = cur;
                cur = next.as_ptr();
                xml_free_node(prev);
            }
            prev = self.last().map_or(null_mut(), |l| l.as_ptr());
            (*prev).set_next(NodePtr::from_ptr(cur));
            (*cur).set_prev(NodePtr::from_ptr(prev));
        }
        while let Some(next) = (*cur).next() {
            (*cur).set_parent(NodePtr::from_ptr(self as *mut XmlNode));
            if (*cur).document() != self.document() {
                (*cur).set_doc(self.document());
            }
            cur = next.as_ptr();
        }
        (*cur).set_parent(NodePtr::from_ptr(self as *mut XmlNode));
        // the parent may not be linked to a doc !
        if (*cur).document() != self.document() {
            (*cur).set_doc(self.document());
        }
        self.set_last(NodePtr::from_ptr(cur));

        cur
    }

    /// Search a Ns registered under a given name space for a document.  
    /// recurse on the parents until it finds the defined namespace or return NULL otherwise.  
    /// `namespace` can be NULL, this is a search for the default namespace.
    ///
    /// We don't allow to cross entities boundaries.  
    /// If you don't declare the namespace within those you will be in troubles !!!  
    /// A warning is generated to cover this case.
    ///
    /// Returns the namespace pointer or NULL.
    #[doc(alias = "xmlSearchNs")]
    pub unsafe fn search_ns(
        &mut self,
        mut doc: *mut XmlDoc,
        namespace: Option<&str>,
    ) -> *mut XmlNs {
        let mut cur: *mut XmlNs;
        let orig: *const XmlNode = self;

        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }
        if namespace == Some("xml") {
            if doc.is_null() && matches!(self.element_type(), XmlElementType::XmlElementNode) {
                // The XML-1.0 namespace is normally held on the root element.
                // In this case exceptionally create it on the node element.
                cur = xml_malloc(size_of::<XmlNs>()) as _;
                if cur.is_null() {
                    xml_tree_err_memory("searching namespace");
                    return null_mut();
                }
                memset(cur as _, 0, size_of::<XmlNs>());
                (*cur).typ = XML_LOCAL_NAMESPACE;
                (*cur).href = xml_strdup(XML_XML_NAMESPACE.as_ptr() as _);
                (*cur).prefix = xml_strdup(c"xml".as_ptr() as _);
                (*cur).next = self.ns_def;
                self.ns_def = cur;
                return cur;
            }
            if doc.is_null() {
                doc = self.document();
                if doc.is_null() {
                    return null_mut();
                }
            }
            // Return the XML namespace declaration held by the doc.
            if (*doc).old_ns.is_null() {
                return (*doc).ensure_xmldecl() as _;
            } else {
                return (*doc).old_ns;
            }
        }
        let mut node = self as *mut XmlNode;
        while !node.is_null() {
            if matches!(
                (*node).element_type(),
                XmlElementType::XmlEntityRefNode
                    | XmlElementType::XmlEntityNode
                    | XmlElementType::XmlEntityDecl
            ) {
                return null_mut();
            }
            if matches!((*node).element_type(), XmlElementType::XmlElementNode) {
                cur = (*node).ns_def;
                while !cur.is_null() {
                    if (*cur).prefix().is_none() && namespace.is_none() && !(*cur).href.is_null() {
                        return cur;
                    }
                    if (*cur).href().is_some()
                        && (*cur).prefix().is_some()
                        && namespace == (*cur).prefix().as_deref()
                    {
                        return cur;
                    }
                    cur = (*cur).next;
                }
                if orig != node {
                    cur = (*node).ns;
                    if !cur.is_null() {
                        if (*cur).prefix().is_none()
                            && namespace.is_none()
                            && !(*cur).href.is_null()
                        {
                            return cur;
                        }
                        if (*cur).prefix().is_some()
                            && (*cur).href().is_some()
                            && namespace == (*cur).prefix().as_deref()
                        {
                            return cur;
                        }
                    }
                }
            }
            node = (*node).parent().map_or(null_mut(), |p| p.as_ptr());
        }
        null_mut()
    }

    /// Search a Ns aliasing a given URI.
    /// Recurse on the parents until it finds the defined namespace or return NULL otherwise.
    ///
    /// Returns the namespace pointer or NULL.
    #[doc(alias = "xmlSearchNsByHref")]
    pub unsafe fn search_ns_by_href(&mut self, mut doc: *mut XmlDoc, href: &str) -> *mut XmlNs {
        let mut cur: *mut XmlNs;
        let orig: *mut XmlNode = self;

        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }
        if href == XML_XML_NAMESPACE.to_str().unwrap() {
            // Only the document can hold the XML spec namespace.
            if doc.is_null() && matches!(self.element_type(), XmlElementType::XmlElementNode) {
                // The XML-1.0 namespace is normally held on the root element.
                // In this case exceptionally create it on the node element.
                cur = xml_malloc(size_of::<XmlNs>()) as _;
                if cur.is_null() {
                    xml_tree_err_memory("searching namespace");
                    return null_mut();
                }
                memset(cur as _, 0, size_of::<XmlNs>());
                (*cur).typ = XML_LOCAL_NAMESPACE;
                (*cur).href = xml_strdup(XML_XML_NAMESPACE.as_ptr() as _);
                (*cur).prefix = xml_strdup(c"xml".as_ptr() as _);
                (*cur).next = self.ns_def;
                self.ns_def = cur;
                return cur;
            }
            if doc.is_null() {
                doc = self.document();
                if doc.is_null() {
                    return null_mut();
                }
            }
            // Return the XML namespace declaration held by the doc.
            if (*doc).old_ns.is_null() {
                return (*doc).ensure_xmldecl();
            } else {
                return (*doc).old_ns;
            }
        }
        let is_attr = matches!(self.element_type(), XmlElementType::XmlAttributeNode);
        let mut node = self as *mut XmlNode;
        while !node.is_null() {
            if matches!(
                (*node).element_type(),
                XmlElementType::XmlEntityRefNode
                    | XmlElementType::XmlEntityNode
                    | XmlElementType::XmlEntityDecl
            ) {
                return null_mut();
            }
            if matches!((*node).element_type(), XmlElementType::XmlElementNode) {
                let href = CString::new(href).unwrap();
                cur = (*node).ns_def;
                while !cur.is_null() {
                    if !(*cur).href.is_null()
                        && xml_str_equal((*cur).href, href.as_ptr() as *const u8)
                        && (!is_attr || (*cur).prefix().is_some())
                        && xml_ns_in_scope(doc, orig, node, (*cur).prefix) == 1
                    {
                        return cur;
                    }
                    cur = (*cur).next;
                }
                if orig != node {
                    cur = (*node).ns;
                    if !cur.is_null()
                        && !(*cur).href.is_null()
                        && xml_str_equal((*cur).href, href.as_ptr() as *const u8)
                        && (!is_attr || (*cur).prefix().is_some())
                        && xml_ns_in_scope(doc, orig, node, (*cur).prefix) == 1
                    {
                        return cur;
                    }
                }
            }
            node = (*node).parent().map_or(null_mut(), |p| p.as_ptr());
        }
        null_mut()
    }

    /// This function checks that all the namespaces declared within the given tree are properly declared.
    /// This is needed for example after Copy or Cut and then paste operations.
    ///
    /// The subtree may still hold pointers to namespace declarations outside the subtree or invalid/masked.
    ///
    /// As much as possible the function try to reuse the existing namespaces found in the new environment.
    /// If not possible the new namespaces are redeclared on `tree` at the top of the given subtree.
    ///
    /// Returns the number of namespace declarations created or -1 in case of error.
    #[doc(alias = "xmlReconciliateNs")]
    #[cfg(feature = "libxml_tree")]
    pub unsafe fn reconciliate_ns(&mut self, doc: *mut XmlDoc) -> i32 {
        use crate::libxml::globals::xml_realloc;

        use super::xml_new_reconciled_ns;

        let mut old_ns: *mut *mut XmlNs = null_mut();
        let mut new_ns: *mut *mut XmlNs = null_mut();
        let mut size_cache: i32 = 0;
        let mut nb_cache: i32 = 0;
        let mut n: *mut XmlNs;
        let mut node: *mut XmlNode = self;
        let mut attr: *mut XmlAttr;
        let ret: i32 = 0;

        if !matches!((*node).element_type(), XmlElementType::XmlElementNode) {
            return -1;
        }
        if doc.is_null() || !matches!((*doc).element_type(), XmlElementType::XmlDocumentNode) {
            return -1;
        }
        if (*node).document() != doc {
            return -1;
        }
        while !node.is_null() {
            // Reconciliate the node namespace
            if !(*node).ns.is_null() {
                // initialize the cache if needed
                if size_cache == 0 {
                    size_cache = 10;
                    old_ns = xml_malloc(size_cache as usize * size_of::<*mut XmlNs>()) as _;
                    if old_ns.is_null() {
                        xml_tree_err_memory("fixing namespaces");
                        return -1;
                    }
                    new_ns = xml_malloc(size_cache as usize * size_of::<*mut XmlNs>()) as _;
                    if new_ns.is_null() {
                        xml_tree_err_memory("fixing namespaces");
                        xml_free(old_ns as _);
                        return -1;
                    }
                }
                let mut f = false;
                for i in 0..nb_cache {
                    if *old_ns.add(i as usize) == (*node).ns {
                        (*node).ns = *new_ns.add(i as usize);
                        f = true;
                        break;
                    }
                }
                if !f {
                    // OK we need to recreate a new namespace definition
                    n = xml_new_reconciled_ns(doc, self, (*node).ns);
                    if !n.is_null() {
                        // :-( what if else ???
                        // check if we need to grow the cache buffers.
                        if size_cache <= nb_cache {
                            size_cache *= 2;
                            old_ns = xml_realloc(
                                old_ns as _,
                                size_cache as usize * size_of::<*mut XmlNs>(),
                            ) as _;
                            if old_ns.is_null() {
                                xml_tree_err_memory("fixing namespaces");
                                xml_free(new_ns as _);
                                return -1;
                            }
                            new_ns = xml_realloc(
                                new_ns as _,
                                size_cache as usize * size_of::<*mut XmlNs>(),
                            ) as _;
                            if new_ns.is_null() {
                                xml_tree_err_memory("fixing namespaces");
                                xml_free(old_ns as _);
                                return -1;
                            }
                        }
                        *new_ns.add(nb_cache as usize) = n;
                        *old_ns.add(nb_cache as usize) = (*node).ns;
                        nb_cache += 1;
                        (*node).ns = n;
                    }
                }
            }
            // now check for namespace held by attributes on the node.
            if matches!((*node).typ, XmlElementType::XmlElementNode) {
                attr = (*node).properties;
                while !attr.is_null() {
                    if !(*attr).ns.is_null() {
                        // initialize the cache if needed
                        if size_cache == 0 {
                            size_cache = 10;
                            old_ns = xml_malloc(size_cache as usize * size_of::<*mut XmlNs>()) as _;
                            if old_ns.is_null() {
                                xml_tree_err_memory("fixing namespaces");
                                return -1;
                            }
                            new_ns = xml_malloc(size_cache as usize * size_of::<*mut XmlNs>()) as _;
                            if new_ns.is_null() {
                                xml_tree_err_memory("fixing namespaces");
                                xml_free(old_ns as _);
                                return -1;
                            }
                        }
                        let mut f = false;
                        for i in 0..nb_cache {
                            if *old_ns.add(i as usize) == (*attr).ns {
                                (*attr).ns = *new_ns.add(i as usize);
                                f = true;
                                break;
                            }
                        }
                        if !f {
                            // OK we need to recreate a new namespace definition
                            n = xml_new_reconciled_ns(doc, self, (*attr).ns);
                            if !n.is_null() {
                                // :-( what if else ???
                                // check if we need to grow the cache buffers.
                                if size_cache <= nb_cache {
                                    size_cache *= 2;
                                    old_ns = xml_realloc(
                                        old_ns as _,
                                        size_cache as usize * size_of::<*mut XmlNs>(),
                                    ) as _;
                                    if old_ns.is_null() {
                                        xml_tree_err_memory("fixing namespaces");
                                        xml_free(new_ns as _);
                                        return -1;
                                    }
                                    new_ns = xml_realloc(
                                        new_ns as _,
                                        size_cache as usize * size_of::<*mut XmlNs>(),
                                    ) as _;
                                    if new_ns.is_null() {
                                        xml_tree_err_memory("fixing namespaces");
                                        xml_free(old_ns as _);
                                        return -1;
                                    }
                                }
                                *new_ns.add(nb_cache as usize) = n;
                                *old_ns.add(nb_cache as usize) = (*attr).ns;
                                nb_cache += 1;
                                (*attr).ns = n;
                            }
                        }
                    }
                    attr = (*attr).next;
                }
            }

            // Browse the full subtree, deep first
            if let Some(children) = (*node)
                .children()
                .filter(|_| !matches!((*node).element_type(), XmlElementType::XmlEntityRefNode))
            {
                // deep first
                node = children.as_ptr();
            } else if let Some(next) = (*node).next().filter(|_| node != self) {
                // then siblings
                node = next.as_ptr();
            } else if node != self {
                // go up to parents->next if needed
                while node != self {
                    if (*node).parent().is_some() {
                        node = (*node).parent().map_or(null_mut(), |p| p.as_ptr());
                    }
                    if let Some(next) = (*node).next().filter(|_| node != self) {
                        node = next.as_ptr();
                        break;
                    }
                    if (*node).parent().is_none() {
                        node = null_mut();
                        break;
                    }
                }
                // exit condition
                if node == self {
                    node = null_mut();
                }
            } else {
                break;
            }
        }
        if !old_ns.is_null() {
            xml_free(old_ns as _);
        }
        if !new_ns.is_null() {
            xml_free(new_ns as _);
        }
        ret
    }
}

impl Default for XmlNode {
    fn default() -> Self {
        Self {
            _private: null_mut(),
            typ: XmlElementType::default(),
            name: null(),
            children: None,
            last: None,
            parent: None,
            next: None,
            prev: None,
            doc: null_mut(),
            ns: null_mut(),
            content: null_mut(),
            properties: null_mut(),
            ns_def: null_mut(),
            psvi: null_mut(),
            line: 0,
            extra: 0,
        }
    }
}

impl NodeCommon for XmlNode {
    fn document(&self) -> *mut XmlDoc {
        self.doc
    }
    fn set_document(&mut self, doc: *mut XmlDoc) {
        self.doc = doc;
    }
    fn element_type(&self) -> XmlElementType {
        self.typ
    }
    fn name(&self) -> Option<Cow<'_, str>> {
        (!self.name.is_null())
            .then(|| unsafe { CStr::from_ptr(self.name as *const i8).to_string_lossy() })
    }
    fn children(&self) -> Option<NodePtr> {
        self.children
    }
    fn set_children(&mut self, children: Option<NodePtr>) {
        self.children = children;
    }
    fn last(&self) -> Option<NodePtr> {
        self.last
    }
    fn set_last(&mut self, last: Option<NodePtr>) {
        self.last = last;
    }
    fn next(&self) -> Option<NodePtr> {
        self.next
    }
    fn set_next(&mut self, next: Option<NodePtr>) {
        self.next = next;
    }
    fn prev(&self) -> Option<NodePtr> {
        self.prev
    }
    fn set_prev(&mut self, prev: Option<NodePtr>) {
        self.prev = prev;
    }
    fn parent(&self) -> Option<NodePtr> {
        self.parent
    }
    fn set_parent(&mut self, parent: Option<NodePtr>) {
        self.parent = parent;
    }
}

pub struct NodePtr(NonNull<XmlNode>);

impl NodePtr {
    pub(crate) fn from_ptr(ptr: *mut XmlNode) -> Option<Self> {
        NonNull::new(ptr).map(Self)
    }

    pub fn as_ptr(&self) -> *mut XmlNode {
        self.0.as_ptr()
    }
}

impl Clone for NodePtr {
    fn clone(&self) -> Self {
        *self
    }
}

impl Copy for NodePtr {}

impl PartialEq for NodePtr {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ptr() == other.0.as_ptr()
    }
}

impl Deref for NodePtr {
    type Target = XmlNode;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

impl DerefMut for NodePtr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}

impl From<&XmlNode> for NodePtr {
    fn from(value: &XmlNode) -> Self {
        // Safety
        // `value` is a reference,
        // it is not NULL as long as the constraint of references is observed.
        unsafe {
            Self(NonNull::new_unchecked(
                value as *const XmlNode as *mut XmlNode,
            ))
        }
    }
}

impl From<&mut XmlNode> for NodePtr {
    fn from(value: &mut XmlNode) -> Self {
        // Safety
        // `value` is a reference,
        // it is not NULL as long as the constraint of references is observed.
        unsafe {
            Self(NonNull::new_unchecked(
                value as *const XmlNode as *mut XmlNode,
            ))
        }
    }
}

/// Add a new attribute after `prev` using `cur` as base attribute.  
/// When inserting before `cur`, `prev` is passed as `cur.prev`.  
/// When inserting after `cur`, `prev` is passed as `cur`.  
/// If an existing attribute is found it is destroyed prior to adding `prop`.  
///
/// See the note regarding namespaces in xmlAddChild.
///
/// Returns the attribute being inserted or NULL in case of error.
#[doc(alias = "xmlAddPropSibling")]
unsafe fn add_prop_sibling(
    prev: *mut XmlNode,
    cur: *mut XmlNode,
    prop: *mut XmlNode,
) -> *mut XmlNode {
    if cur.is_null()
        || !matches!((*cur).typ, XmlElementType::XmlAttributeNode)
        || prop.is_null()
        || !matches!((*prop).typ, XmlElementType::XmlAttributeNode)
        || (!prev.is_null() && !matches!((*prev).typ, XmlElementType::XmlAttributeNode))
    {
        return null_mut();
    }

    /* check if an attribute with the same name exists */
    let attr = if (*prop).ns.is_null() {
        (*cur).parent.expect("Internal Error").has_ns_prop(
            CStr::from_ptr((*prop).name as *const i8)
                .to_string_lossy()
                .as_ref(),
            None,
        )
    } else {
        let href = (*(*prop).ns).href;
        (*cur).parent.expect("Internal Error").has_ns_prop(
            CStr::from_ptr((*prop).name as *const i8)
                .to_string_lossy()
                .as_ref(),
            (!href.is_null())
                .then(|| CStr::from_ptr(href as *const i8).to_string_lossy())
                .as_deref(),
        )
    };

    if (*prop).doc != (*cur).doc {
        (*prop).set_doc((*cur).doc);
    }
    (*prop).parent = (*cur).parent;
    (*prop).prev = NodePtr::from_ptr(prev);
    if !prev.is_null() {
        (*prop).next = (*prev).next;
        (*prev).next = NodePtr::from_ptr(prop);
        if let Some(mut next) = (*prop).next {
            next.prev = NodePtr::from_ptr(prop);
        }
    } else {
        (*prop).next = NodePtr::from_ptr(cur);
        (*cur).prev = NodePtr::from_ptr(prop);
    }
    if let Some(mut parent) = (*prop).parent.filter(|_| (*prop).prev.is_none()) {
        parent.properties = prop as _;
    }
    if !attr.is_null() && ((*attr).typ != XmlElementType::XmlAttributeDecl) {
        /* different instance, destroy it (attributes must be unique) */
        (*attr).remove_prop();
    }
    prop
}
