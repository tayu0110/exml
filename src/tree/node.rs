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
    any::type_name,
    borrow::Cow,
    ffi::{CStr, CString},
    ops::{Deref, DerefMut},
    os::raw::c_void,
    ptr::{null, null_mut, NonNull},
    sync::atomic::Ordering,
};

use crate::{
    libxml::{
        globals::xml_free,
        valid::xml_remove_id,
        xmlstring::{xml_str_equal, xml_strcat, xml_strdup, xml_strncat, XmlChar},
    },
    tree::xml_free_node_list,
};

use super::{
    xml_encode_attribute_entities, xml_encode_entities_reentrant, xml_free_node, xml_free_prop,
    xml_get_doc_entity, xml_is_blank_char, xml_ns_in_scope, xml_tree_err_memory,
    InvalidNodePointerCastError, NodeCommon, XmlAttr, XmlAttrPtr, XmlAttributePtr,
    XmlAttributeType, XmlDoc, XmlDocPtr, XmlElementType, XmlGenericNodePtr, XmlNs, XmlNsPtr,
    XML_CHECK_DTD, XML_LOCAL_NAMESPACE, XML_XML_NAMESPACE,
};

fn verify_xml_node(node: &XmlNode) -> bool {
    matches!(
        node.typ,
        XmlElementType::XmlElementNode
            | XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlPINode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd
    )
}

#[repr(C)]
pub struct XmlNode {
    pub _private: *mut c_void,                      /* application data */
    pub(crate) typ: XmlElementType,                 /* type number, must be second ! */
    pub name: *const XmlChar,                       /* the name of the node, or the entity */
    pub(crate) children: Option<XmlGenericNodePtr>, /* parent->childs link */
    pub(crate) last: Option<XmlGenericNodePtr>,     /* last child link */
    pub(crate) parent: Option<XmlGenericNodePtr>,   /* child->parent link */
    pub next: Option<XmlGenericNodePtr>,            /* next sibling link  */
    pub(crate) prev: Option<XmlGenericNodePtr>,     /* previous sibling link  */
    pub doc: Option<XmlDocPtr>,                     /* the containing document */

    /* End of common part */
    pub(crate) ns: Option<XmlNsPtr>, /* pointer to the associated namespace */
    pub content: *mut XmlChar,       /* the content */
    pub(crate) properties: Option<XmlAttrPtr>, /* properties list */
    pub ns_def: Option<XmlNsPtr>,    /* namespace definitions on this node */
    pub(crate) psvi: *mut c_void,    /* for type/PSVI information */
    pub(crate) line: u16,            /* line number */
    pub(crate) extra: u16,           /* extra data for XPath/XSLT */
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
        let mut cur = XmlGenericNodePtr::from_raw(self as *const Self as *mut Self);
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
                let current = XmlNodePtr::try_from(current).unwrap();
                generic = 0;
                sep = "/";
                if let Some(ns) = current.ns {
                    name = if let Some(prefix) = ns.prefix() {
                        Cow::Owned(format!("{prefix}:{}", current.name().unwrap(),))
                    } else {
                        // We cannot express named elements in the default
                        // namespace, so use "*".
                        generic = 1;
                        "*".into()
                    };
                } else {
                    name = current.name().unwrap().into_owned().into();
                }

                // Thumbler index computation
                // TODO: the occurrence test seems bogus for namespaced names
                let mut tmp = current.prev();
                while let Some(now) = tmp {
                    if XmlNodePtr::try_from(now)
                        .ok()
                        .filter(|now| now.element_type() == XmlElementType::XmlElementNode)
                        .filter(|now| {
                            generic != 0
                                || (current.name() == now.name()
                                    && (now.ns == current.ns
                                        || current
                                            .ns
                                            .zip(now.ns)
                                            .map_or(false, |(c, n)| c.prefix() == n.prefix())))
                        })
                        .is_some()
                    {
                        occur += 1;
                    }
                    tmp = now.prev();
                }
                if occur == 0 {
                    let mut tmp = current.next();
                    while let Some(now) = tmp.filter(|_| occur == 0) {
                        if XmlNodePtr::try_from(now)
                            .ok()
                            .filter(|now| now.element_type() == XmlElementType::XmlElementNode)
                            .filter(|now| {
                                generic != 0
                                    || (current.name() == now.name()
                                        && (now.ns == current.ns
                                            || current
                                                .ns
                                                .zip(now.ns)
                                                .map_or(false, |(c, n)| c.prefix() == n.prefix())))
                            })
                            .is_some()
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
                let attr = XmlAttrPtr::try_from(current).unwrap();
                sep = "/@";
                if let Some(ns) = attr.ns {
                    name = if let Some(prefix) = ns.prefix() {
                        format!("{prefix}:{}", attr.name().unwrap()).into()
                    } else {
                        format!("{}", attr.name().unwrap()).into()
                    };
                } else {
                    name = attr.name().unwrap().into_owned().into();
                }
                attr.parent()
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
                let attr = XmlAttrPtr::from_raw(self as *const XmlNode as *mut XmlAttr)
                    .unwrap()
                    .unwrap();
                attr.get_content()
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
                xml_get_doc_entity(self.document(), &self.name().unwrap())?;

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
                let doc = XmlDocPtr::from_raw(self as *const XmlNode as *mut XmlDoc)
                    .unwrap()
                    .unwrap();
                doc.get_content()
            }
            XmlElementType::XmlNamespaceDecl => {
                let ns = XmlNsPtr::from_raw(self as *const XmlNode as *mut XmlNs)
                    .unwrap()
                    .unwrap();
                ns.get_content()
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
                let mut tmp = XmlGenericNodePtr::from_raw(self as *const Self as *mut Self);
                let orig = tmp;

                while let Some(cur_node) = tmp {
                    match cur_node.element_type() {
                        XmlElementType::XmlCDATASectionNode | XmlElementType::XmlTextNode => {
                            let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                            if !cur_node.content.is_null() {
                                buf.push_str(
                                    CStr::from_ptr(cur_node.content as *const i8)
                                        .to_string_lossy()
                                        .as_ref(),
                                );
                            }
                        }
                        XmlElementType::XmlEntityRefNode => {
                            cur_node.get_content_to(buf);
                        }
                        _ => {}
                    }
                    // Skip to next node
                    if let Some(children) = cur_node.children().filter(|children| {
                        !matches!(children.element_type(), XmlElementType::XmlEntityDecl)
                    }) {
                        tmp = Some(children);
                        continue;
                    }
                    if tmp == orig {
                        break;
                    } else {
                        if let Some(next) = cur_node.next() {
                            tmp = Some(next);
                            continue;
                        }

                        while let Some(cur_node) = tmp {
                            let Some(next) = cur_node.parent() else {
                                break;
                            };
                            tmp = Some(next);
                            if tmp == orig {
                                tmp = None;
                                break;
                            }
                            if let Some(next) = next.next() {
                                tmp = Some(next);
                                break;
                            }
                        }
                    }
                }
            }
            XmlElementType::XmlAttributeNode => {
                let attr = XmlAttrPtr::from_raw(self as *const XmlNode as *mut XmlAttr)
                    .unwrap()
                    .unwrap();
                attr.get_content_to(buf);
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
                let Some(ent) = xml_get_doc_entity(self.document(), &self.name().unwrap()) else {
                    return -1;
                };

                // an entity content can be any "well balanced chunk",
                // i.e. the result of the content [43] production:
                // http://www.w3.org/TR/REC-xml#NT-content
                // -> we iterate through child nodes and recursive call
                // xmlNodeGetContent() which handles all possible node types
                let mut tmp = ent.children();
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
                let doc = XmlDocPtr::from_raw(self as *const XmlNode as *mut XmlDoc)
                    .unwrap()
                    .unwrap();
                doc.get_content_to(buf);
            }
            XmlElementType::XmlNamespaceDecl => {
                let ns = XmlNsPtr::from_raw(self as *const XmlNode as *mut XmlNs)
                    .unwrap()
                    .unwrap();
                ns.get_content_to(buf);
            }
            XmlElementType::XmlElementDecl
            | XmlElementType::XmlAttributeDecl
            | XmlElementType::XmlEntityDecl => {}
            _ => unreachable!(),
        }
        0
    }

    /// If `use_dtd` is `true` and no suitable attribute is found,
    /// and a Default/Fixed attribute declaration is found,
    /// this method may return `Err(XmlAttributePtr)` pointing to that declaration.
    unsafe fn get_prop_node_internal(
        &self,
        name: &str,
        ns_name: Option<&str>,
        use_dtd: bool,
    ) -> Option<Result<XmlAttrPtr, XmlAttributePtr>> {
        if !matches!(self.element_type(), XmlElementType::XmlElementNode) {
            return None;
        }

        if self.properties.is_some() {
            let mut prop = self.properties;
            if let Some(ns_name) = ns_name {
                // We want the attr to be in the specified namespace.
                let ns_name = CString::new(ns_name).unwrap();
                while let Some(now) = prop {
                    if now.name().as_deref() == Some(name)
                        && now.ns.map_or(false, |ns| {
                            ns.href == ns_name.as_ptr() as _
                                || xml_str_equal(ns.href, ns_name.as_ptr() as *const u8)
                        })
                    {
                        return Some(Ok(now));
                    }
                    prop = now.next;
                }
            } else {
                // We want the attr to be in no namespace.
                while let Some(now) = prop {
                    if now.ns.is_none() && now.name().as_deref() == Some(name) {
                        return Some(Ok(now));
                    }
                    prop = now.next;
                }
            }
        }

        #[cfg(feature = "libxml_tree")]
        {
            if !use_dtd {
                return None;
            }
            // Check if there is a default/fixed attribute declaration in
            // the internal or external subset.
            if let Some(doc) = self.doc {
                if let Some(int_subset) = doc.int_subset {
                    let elem_qname: *mut XmlChar;
                    let mut tmpstr: *mut XmlChar = null_mut();

                    // We need the QName of the element for the DTD-lookup.
                    if let Some(prefix) = self.ns.map(|ns| ns.prefix).filter(|p| !p.is_null()) {
                        tmpstr = xml_strdup(prefix);
                        tmpstr = xml_strcat(tmpstr, c":".as_ptr() as _);
                        tmpstr = xml_strcat(tmpstr, self.name);
                        if tmpstr.is_null() {
                            return None;
                        }
                        elem_qname = tmpstr;
                    } else {
                        elem_qname = self.name as _;
                    }
                    let mut attr_decl = None;
                    if let Some(ns_name) = ns_name {
                        if ns_name == XML_XML_NAMESPACE.to_string_lossy() {
                            // The XML namespace must be bound to prefix 'xml'.
                            attr_decl = int_subset.get_qattr_desc(
                                CStr::from_ptr(elem_qname as *const i8)
                                    .to_string_lossy()
                                    .as_ref(),
                                name,
                                Some("xml"),
                            );
                            if attr_decl.is_none() {
                                if let Some(ext_subset) = doc.ext_subset {
                                    attr_decl = ext_subset.get_qattr_desc(
                                        CStr::from_ptr(elem_qname as *const i8)
                                            .to_string_lossy()
                                            .as_ref(),
                                        name,
                                        Some("xml"),
                                    );
                                }
                            }
                        } else {
                            // The ugly case: Search using the prefixes of in-scope
                            // ns-decls corresponding to @nsName.
                            let Some(ns_list) = self.get_ns_list(self.doc) else {
                                if !tmpstr.is_null() {
                                    xml_free(tmpstr as _);
                                }
                                return None;
                            };
                            let ns_name = CString::new(ns_name).unwrap();
                            for cur in ns_list {
                                if xml_str_equal(cur.href, ns_name.as_ptr() as *const u8) {
                                    let prefix = (*cur).prefix();
                                    attr_decl = int_subset.get_qattr_desc(
                                        CStr::from_ptr(elem_qname as *const i8)
                                            .to_string_lossy()
                                            .as_ref(),
                                        name,
                                        prefix.as_deref(),
                                    );
                                    if attr_decl.is_some() {
                                        break;
                                    }
                                    if let Some(ext_subset) = doc.ext_subset {
                                        attr_decl = ext_subset.get_qattr_desc(
                                            CStr::from_ptr(elem_qname as *const i8)
                                                .to_string_lossy()
                                                .as_ref(),
                                            name,
                                            prefix.as_deref(),
                                        );
                                        if attr_decl.is_some() {
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        // The common and nice case: Attr in no namespace.
                        attr_decl = int_subset.get_qattr_desc(
                            CStr::from_ptr(elem_qname as *const i8)
                                .to_string_lossy()
                                .as_ref(),
                            name,
                            None,
                        );
                        if attr_decl.is_none() {
                            if let Some(ext_subset) = doc.ext_subset {
                                attr_decl = ext_subset.get_qattr_desc(
                                    CStr::from_ptr(elem_qname as *const i8)
                                        .to_string_lossy()
                                        .as_ref(),
                                    name,
                                    None,
                                );
                            }
                        }
                    }
                    if !tmpstr.is_null() {
                        xml_free(tmpstr as _);
                    }
                    // Only default/fixed attrs are relevant.
                    if let Some(attr_decl) =
                        attr_decl.filter(|attr_decl| !attr_decl.default_value.is_null())
                    {
                        return Some(Err(attr_decl));
                    }
                }
            }
        }
        None
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
    #[doc(alias = "xmlGetProp")]
    pub unsafe fn get_prop(&self, name: &str) -> Option<String> {
        match self.has_prop(name)? {
            Ok(prop) => prop.get_prop_node_value_internal(),
            Err(prop) => prop.get_prop_node_value_internal(),
        }
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
    #[doc(alias = "xmlGetNsProp")]
    pub unsafe fn get_ns_prop(&self, name: &str, name_space: Option<&str>) -> Option<String> {
        let prop =
            self.get_prop_node_internal(name, name_space, XML_CHECK_DTD.load(Ordering::Relaxed))?;
        match prop {
            Ok(prop) => prop.get_prop_node_value_internal(),
            Err(prop) => prop.get_prop_node_value_internal(),
        }
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
    #[doc(alias = "xmlGetNoNsProp")]
    pub unsafe fn get_no_ns_prop(&self, name: &str) -> Option<String> {
        let prop =
            self.get_prop_node_internal(name, None, XML_CHECK_DTD.load(Ordering::Relaxed))?;
        match prop {
            Ok(prop) => prop.get_prop_node_value_internal(),
            Err(prop) => prop.get_prop_node_value_internal(),
        }
    }

    /// Search all the namespace applying to a given element.
    ///
    /// Returns an `Vec` of all the `xmlNsPtr` found.
    #[doc(alias = "xmlGetNsList")]
    #[cfg(any(feature = "libxml_tree", feature = "xpath", feature = "schema"))]
    pub unsafe fn get_ns_list(&self, _doc: Option<XmlDocPtr>) -> Option<Vec<XmlNsPtr>> {
        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }

        let mut ret: Vec<XmlNsPtr> = vec![];
        let mut node = XmlGenericNodePtr::from_raw(self as *const Self as *mut Self);
        while let Some(cur_node) = node {
            if matches!(cur_node.element_type(), XmlElementType::XmlElementNode) {
                let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                let mut cur = cur_node.ns_def;
                while let Some(now) = cur {
                    if ret.iter().all(|&ret| now.prefix() != ret.prefix()) {
                        ret.push(now);
                    }
                    cur = now.next
                }
            }
            node = cur_node.parent();
        }
        Some(ret)
    }

    /// Searches the language of a node, i.e. the values of the xml:lang
    /// attribute or the one carried by the nearest ancestor.
    ///
    /// Returns a pointer to the lang value, or null_mut() if not found.  
    #[doc(alias = "xmlNodeGetLang")]
    pub unsafe fn get_lang(&self) -> Option<String> {
        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }
        if let Some(lang) = self.get_ns_prop("lang", XML_XML_NAMESPACE.to_str().ok()) {
            return Some(lang);
        }
        let mut cur = self.parent;
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
        let mut cur = XmlGenericNodePtr::from_raw(self as *const Self as *mut Self);
        while let Some(now) = cur {
            if let Some(space) = now.get_ns_prop("space", XML_XML_NAMESPACE.to_str().ok()) {
                if space == "preserve" {
                    return 1;
                }
                if space == "default" {
                    return 0;
                }
            }
            cur = now.parent();
        }
        -1
    }

    /// Build the string equivalent to the text contained in the Node list
    /// made of TEXTs and ENTITY_REFs.
    ///
    /// Returns a pointer to the string copy.
    #[doc(alias = "xmlNodeListGetString")]
    pub unsafe fn get_string(&self, doc: Option<XmlDocPtr>, in_line: i32) -> Option<String> {
        let mut node = XmlGenericNodePtr::from_raw(self as *const Self as *mut Self);
        let mut ret: *mut XmlChar = null_mut();

        let attr = self
            .parent()
            .filter(|p| matches!(p.element_type(), XmlElementType::XmlAttributeNode))
            .is_some();

        while let Some(cur_node) = node {
            if matches!(
                cur_node.element_type(),
                XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
            ) {
                let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                if in_line != 0 {
                    ret = xml_strcat(ret, cur_node.content);
                } else {
                    let buffer = if attr {
                        xml_encode_attribute_entities(doc, cur_node.content)
                    } else {
                        xml_encode_entities_reentrant(doc, cur_node.content)
                    };
                    if !buffer.is_null() {
                        ret = xml_strcat(ret, buffer);
                        xml_free(buffer as _);
                    }
                }
            } else if matches!(cur_node.element_type(), XmlElementType::XmlEntityRefNode) {
                let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                if in_line != 0 {
                    let ent = xml_get_doc_entity(doc, &cur_node.name().unwrap());
                    if let Some(ent) = ent {
                        // an entity content can be any "well balanced chunk",
                        // i.e. the result of the content [43] production:
                        // http://www.w3.org/TR/REC-xml#NT-content.
                        // So it can contain text, CDATA section or nested
                        // entity reference nodes (among others).
                        // -> we recursive  call xmlNodeListGetString()
                        // which handles these types
                        let children = ent.children();
                        if let Some(buffer) = children.and_then(|c| c.get_string(doc, 1)) {
                            let buffer = CString::new(buffer).unwrap();
                            ret = xml_strcat(ret, buffer.as_ptr() as *const u8);
                        }
                    } else {
                        ret = xml_strcat(ret, cur_node.content);
                    }
                } else {
                    let mut buf: [XmlChar; 2] = [0; 2];

                    buf[0] = b'&';
                    buf[1] = 0;
                    ret = xml_strncat(ret, buf.as_ptr() as _, 1);
                    ret = xml_strcat(ret, cur_node.name);
                    buf[0] = b';';
                    buf[1] = 0;
                    ret = xml_strncat(ret, buf.as_ptr() as _, 1);
                }
            }
            node = cur_node.next();
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
    pub unsafe fn get_raw_string(&self, doc: Option<XmlDocPtr>, in_line: i32) -> Option<String> {
        use super::xml_encode_special_chars;

        let mut node = XmlGenericNodePtr::from_raw(self as *const Self as *mut Self);
        let mut ret: *mut XmlChar = null_mut();

        while let Some(cur_node) = node {
            if matches!(
                cur_node.element_type(),
                XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
            ) {
                let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                if in_line != 0 {
                    ret = xml_strcat(ret, cur_node.content);
                } else {
                    let buffer: *mut XmlChar = xml_encode_special_chars(doc, cur_node.content);
                    if !buffer.is_null() {
                        ret = xml_strcat(ret, buffer);
                        xml_free(buffer as _);
                    }
                }
            } else if matches!(cur_node.element_type(), XmlElementType::XmlEntityRefNode) {
                let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                if in_line != 0 {
                    let ent = xml_get_doc_entity(doc, &cur_node.name().unwrap());
                    if let Some(ent) = ent {
                        // an entity content can be any "well balanced chunk",
                        // i.e. the result of the content [43] production:
                        // http://www.w3.org/TR/REC-xml#NT-content.
                        // So it can contain text, CDATA section or nested
                        // entity reference nodes (among others).
                        // -> we recursive  call xmlNodeListGetRawString()
                        // which handles these types
                        let children = ent.children();
                        let buffer = children.and_then(|c| c.get_raw_string(doc, 1));
                        if let Some(buffer) = buffer.map(|b| CString::new(b).unwrap()) {
                            ret = xml_strcat(ret, buffer.as_ptr() as *const u8);
                        }
                    } else {
                        ret = xml_strcat(ret, cur_node.content);
                    }
                } else {
                    let mut buf: [u8; 2] = [0; 2];

                    buf[0] = b'&';
                    buf[1] = 0;
                    ret = xml_strncat(ret, buf.as_ptr(), 1);
                    ret = xml_strcat(ret, cur_node.name);
                    buf[0] = b';';
                    buf[1] = 0;
                    ret = xml_strncat(ret, buf.as_ptr(), 1);
                }
            }
            node = cur_node.next();
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
    pub unsafe fn set_prop(&mut self, name: &str, value: Option<&str>) -> Option<XmlAttrPtr> {
        use crate::parser::split_qname2;

        if !matches!(self.element_type(), XmlElementType::XmlElementNode) {
            return None;
        }

        // handle QNames
        if let Some((prefix, local)) = split_qname2(name) {
            if let Some(ns) = self.search_ns(self.document(), Some(prefix)) {
                return self.set_ns_prop(Some(ns), local, value);
            }
        }
        self.set_ns_prop(None, name, value)
    }

    /// Remove an attribute carried by a node.  
    /// This handles only attributes in no namespace.
    ///
    /// Returns 0 if successful, -1 if not found
    #[doc(alias = "xmlUnsetProp")]
    #[cfg(any(feature = "libxml_tree", feature = "schema"))]
    pub unsafe fn unset_prop(&mut self, name: &str) -> i32 {
        let Some(Ok(mut prop)) = self.get_prop_node_internal(name, None, false) else {
            return -1;
        };
        prop.unlink();
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
        ns: Option<XmlNsPtr>,
        name: &str,
        value: Option<&str>,
    ) -> Option<XmlAttrPtr> {
        use std::ptr::null;

        use crate::{
            libxml::valid::{xml_add_id, xml_remove_id},
            tree::{xml_free_node_list, xml_new_doc_text, xml_new_prop_internal, XmlAttributeType},
        };

        if ns.map_or(false, |ns| ns.href.is_null()) {
            return None;
        }
        let prop = self.get_prop_node_internal(
            name,
            ns.as_deref().and_then(|ns| ns.href()).as_deref(),
            false,
        );
        if let Some(Ok(mut prop)) = prop {
            // Modify the attribute's value.
            if matches!(prop.atype, Some(XmlAttributeType::XmlAttributeID)) {
                xml_remove_id(self.document().unwrap(), prop);
                prop.atype = Some(XmlAttributeType::XmlAttributeID);
            }
            if let Some(children) = prop.children() {
                xml_free_node_list(Some(children));
            }
            prop.set_children(None);
            prop.set_last(None);
            prop.ns = ns;
            if let Some(value) = value {
                let value = CString::new(value).unwrap();
                prop.set_children(
                    xml_new_doc_text(self.doc, value.as_ptr() as *const u8).map(|node| node.into()),
                );
                prop.set_last(None);
                let mut tmp = prop.children();
                while let Some(mut now) = tmp {
                    now.set_parent(Some(prop.into()));
                    if now.next().is_none() {
                        prop.set_last(Some(now));
                    }
                    tmp = now.next();
                }
            }
            if matches!(prop.atype, Some(XmlAttributeType::XmlAttributeID)) {
                xml_add_id(null_mut(), self.document().unwrap(), value.unwrap(), prop);
            }
            return Some(prop);
        }
        // No equal attr found; create a new one.
        let value = value.map(|v| CString::new(v).unwrap());
        xml_new_prop_internal(
            XmlNodePtr::from_raw(self).unwrap(),
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
    pub unsafe fn unset_ns_prop(&mut self, ns: Option<XmlNsPtr>, name: &str) -> i32 {
        let Some(Ok(mut prop)) = self.get_prop_node_internal(
            name,
            ns.as_deref().and_then(|ns| ns.prefix()).as_deref(),
            false,
        ) else {
            return -1;
        };
        prop.unlink();
        xml_free_prop(prop);
        0
    }

    /// Associate a namespace to a node, a posteriori.
    #[doc(alias = "xmlSetNs")]
    pub fn set_ns(&mut self, ns: Option<XmlNsPtr>) {
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
            XmlElementType::XmlAttributeNode => {
                let mut attr =
                    XmlAttrPtr::try_from(XmlGenericNodePtr::from_raw(self).unwrap()).unwrap();
                attr.set_base(uri);
                return;
            }
            XmlElementType::XmlElementNode => {}
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                let mut doc =
                    XmlDocPtr::try_from(XmlGenericNodePtr::from_raw(self).unwrap()).unwrap();
                doc.set_base(uri);
                return;
            }
            _ => unreachable!(),
        }

        let Some(ns) = self.search_ns_by_href(self.document(), XML_XML_NAMESPACE.to_str().unwrap())
        else {
            return;
        };
        if let Some(uri) = uri {
            let fixed = path_to_uri(uri);
            self.set_ns_prop(Some(ns), "base", Some(&fixed));
        } else {
            self.set_ns_prop(Some(ns), "base", None);
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
        let Some(ns) = self.search_ns_by_href(self.doc, XML_XML_NAMESPACE.to_str().unwrap()) else {
            return;
        };
        self.set_ns_prop(Some(ns), "lang", lang);
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
        let Some(ns) = self.search_ns_by_href(self.doc, XML_XML_NAMESPACE.to_str().unwrap()) else {
            return;
        };
        match val {
            0 => {
                self.set_ns_prop(Some(ns), "space", Some("default"));
            }
            1 => {
                self.set_ns_prop(Some(ns), "space", Some("preserve"));
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
                    xml_free_node_list(Some(children));
                }
                self.set_children(
                    self.document()
                        .and_then(|doc| doc.get_node_list(content))
                        .map(|node| node.into()),
                );
                if let Some(mut ulccur) = self.children() {
                    while let Some(next) = ulccur.next() {
                        ulccur.set_parent(XmlGenericNodePtr::from_raw(self));
                        ulccur = next;
                    }
                    ulccur.set_parent(XmlGenericNodePtr::from_raw(self));
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
                    xml_free_node_list(Some(children));
                }
                self.set_last(None);
                self.set_children(None);
                if !content.is_null() {
                    self.content = xml_strdup(content);
                } else {
                    self.content = null_mut();
                }
                self.properties = None;
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
                    xml_free_node_list(Some(children));
                }
                self.children = self
                    .document()
                    .and_then(|doc| doc.get_node_list_with_strlen(content, len))
                    .map(|node| node.into());
                if let Some(mut ulccur) = self.children() {
                    while let Some(next) = ulccur.next() {
                        ulccur.set_parent(XmlGenericNodePtr::from_raw(self));
                        ulccur = next;
                    }
                    ulccur.set_parent(XmlGenericNodePtr::from_raw(self));
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
                    xml_free_node_list(Some(children));
                }
                self.set_children(None);
                self.set_last(None);
                if !content.is_null() {
                    self.content = xml_strndup(content, len);
                } else {
                    self.content = null_mut();
                }
                self.properties = None;
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
    pub unsafe fn set_doc(&mut self, doc: Option<XmlDocPtr>) {
        if self.element_type() == XmlElementType::XmlNamespaceDecl {
            return;
        }
        if self.document() != doc {
            if matches!(self.element_type(), XmlElementType::XmlElementNode) {
                let mut prop = self.properties;
                while let Some(mut now) = prop {
                    if matches!(now.atype, Some(XmlAttributeType::XmlAttributeID)) {
                        xml_remove_id(self.document().unwrap(), now);
                    }

                    if now.document() != doc {
                        now.set_document(doc);
                    }
                    if let Some(children) = now.children() {
                        children.set_doc_all_sibling(doc);
                    }

                    // TODO: ID attributes should be also added to the new
                    //       document, but this breaks things like xmlReplaceNode.
                    //       The underlying problem is that xmlRemoveID is only called
                    //       if a node is destroyed, not if it's unlinked.
                    // if (xmlIsID(doc, tree, prop)) {
                    //     XmlChar *idVal = xmlNodeListGetString(doc, now.children, 1);
                    //     xmlAddID(null_mut(), doc, idVal, prop);
                    // }

                    prop = now.next;
                }
            }
            if matches!(self.element_type(), XmlElementType::XmlEntityRefNode) {
                // Clear 'children' which points to the entity declaration
                // from the original document.
                self.set_children(None);
            } else if let Some(children) = self.children() {
                children.set_doc_all_sibling(doc);
            }

            // FIXME: self.ns should be updated as in xmlStaticCopyNode().
            self.set_document(doc);
        }
    }

    /// update all nodes in the list to point to the right document
    #[doc(alias = "xmlSetListDoc")]
    pub unsafe fn set_doc_all_sibling(&mut self, doc: Option<XmlDocPtr>) {
        if self.element_type() == XmlElementType::XmlNamespaceDecl {
            return;
        }
        if self.document() != doc {
            self.set_doc(doc);
        }
        let mut cur = self.next.map(XmlGenericNodePtr::from);
        while let Some(now) = cur {
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
    pub unsafe fn has_prop(&self, name: &str) -> Option<Result<XmlAttrPtr, XmlAttributePtr>> {
        if !matches!(self.element_type(), XmlElementType::XmlElementNode) {
            return None;
        }
        // Check on the properties attached to the node
        let mut prop = self.properties;
        while let Some(now) = prop {
            if now.name().as_deref() == Some(name) {
                return Some(Ok(now));
            }
            prop = now.next;
        }
        if !XML_CHECK_DTD.load(Ordering::Relaxed) {
            return None;
        }

        // Check if there is a default declaration in the internal or external subsets
        if let Some(doc) = self.document() {
            if let Some(int_subset) = doc.int_subset {
                let mut attr_decl = int_subset.get_attr_desc(self.name().as_deref().unwrap(), name);
                if attr_decl.is_none() {
                    if let Some(ext_subset) = doc.ext_subset {
                        attr_decl = ext_subset.get_attr_desc(self.name().as_deref().unwrap(), name);
                    }
                }
                if let Some(attr_decl) =
                    attr_decl.filter(|attr_decl| !attr_decl.default_value.is_null())
                {
                    // return attribute declaration only if a default value is given
                    // (that includes #FIXED declarations)
                    return Some(Err(attr_decl));
                }
            }
        }
        None
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
    pub unsafe fn has_ns_prop(
        &self,
        name: &str,
        namespace: Option<&str>,
    ) -> Option<Result<XmlAttrPtr, XmlAttributePtr>> {
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
    pub unsafe fn add_sibling(&mut self, mut elem: XmlGenericNodePtr) -> Option<XmlGenericNodePtr> {
        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }

        if elem.element_type() == XmlElementType::XmlNamespaceDecl {
            return None;
        }

        let mut cur = XmlGenericNodePtr::from_raw(self).unwrap();
        if cur == elem {
            return None;
        }

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

        elem.unlink();

        if matches!(cur.element_type(), XmlElementType::XmlTextNode)
            && matches!(elem.element_type(), XmlElementType::XmlTextNode)
            && cur.name() == elem.name()
        {
            let mut cur = XmlNodePtr::try_from(cur).unwrap();
            let elem = XmlNodePtr::try_from(elem).unwrap();
            cur.add_content(elem.content);
            xml_free_node(elem);
            return Some(cur.into());
        } else if matches!(elem.element_type(), XmlElementType::XmlAttributeNode) {
            return Some(
                add_prop_sibling(
                    XmlAttrPtr::try_from(cur).ok(),
                    XmlAttrPtr::try_from(cur).unwrap(),
                    XmlAttrPtr::try_from(elem).unwrap(),
                )
                .into(),
            );
        }

        if elem.document() != cur.document() {
            elem.set_doc(cur.document());
        }
        let parent = cur.parent();
        elem.set_prev(Some(cur));
        elem.set_next(None);
        elem.set_parent(parent);
        cur.set_next(Some(elem));
        if let Some(mut parent) = parent {
            parent.set_last(Some(elem));
        }

        Some(elem)
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
    pub unsafe fn add_prev_sibling(
        &mut self,
        mut elem: XmlGenericNodePtr,
    ) -> Option<XmlGenericNodePtr> {
        use crate::libxml::{
            globals::xml_free,
            xmlstring::{xml_strcat, xml_strdup},
        };

        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }
        if elem.element_type() == XmlElementType::XmlNamespaceDecl {
            return None;
        }

        if XmlGenericNodePtr::from_raw(self) == Some(elem) {
            return None;
        }

        elem.unlink();

        if matches!(elem.element_type(), XmlElementType::XmlTextNode) {
            let elem = XmlNodePtr::try_from(elem).unwrap();
            if matches!(self.element_type(), XmlElementType::XmlTextNode) {
                let mut tmp = xml_strdup(elem.content);
                tmp = xml_strcat(tmp, self.content);
                self.set_content(tmp);
                xml_free(tmp as _);
                xml_free_node(elem);
                return XmlGenericNodePtr::from_raw(self);
            }
            if let Some(mut prev) = self.prev().filter(|p| {
                matches!(p.element_type(), XmlElementType::XmlTextNode) && self.name() == p.name()
            }) {
                prev.add_content(elem.content);
                xml_free_node(elem);
                return Some(prev);
            }
        } else if matches!(elem.element_type(), XmlElementType::XmlAttributeNode) {
            return Some(
                add_prop_sibling(
                    self.prev.map(|p| XmlAttrPtr::try_from(p).unwrap()),
                    XmlAttrPtr::try_from(XmlGenericNodePtr::from_raw(self).unwrap()).unwrap(),
                    XmlAttrPtr::try_from(elem).unwrap(),
                )
                .into(),
            );
        }

        if elem.document() != self.document() {
            elem.set_doc(self.document());
        }
        elem.set_parent(self.parent());
        elem.set_prev(self.prev());
        self.set_prev(Some(elem));
        elem.set_next(XmlGenericNodePtr::from_raw(self));
        if let Some(mut prev) = elem.prev() {
            prev.set_next(Some(elem));
        }
        if let Some(mut parent) = elem.parent().filter(|p| p.children() == elem.next()) {
            parent.set_children(Some(elem));
        }
        Some(elem)
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
    pub unsafe fn add_next_sibling(
        &mut self,
        mut elem: XmlGenericNodePtr,
    ) -> Option<XmlGenericNodePtr> {
        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }
        if elem.element_type() == XmlElementType::XmlNamespaceDecl {
            return None;
        }

        if XmlGenericNodePtr::from_raw(self) == Some(elem) {
            return None;
        }

        elem.unlink();

        if matches!(elem.element_type(), XmlElementType::XmlTextNode) {
            let elem = XmlNodePtr::try_from(elem).unwrap();
            if matches!(self.element_type(), XmlElementType::XmlTextNode) {
                self.add_content(elem.content);
                xml_free_node(elem);
                return XmlGenericNodePtr::from_raw(self);
            }
            if let Some(mut next) = self
                .next()
                .filter(|next| {
                    matches!(next.element_type(), XmlElementType::XmlTextNode)
                        && self.name() == next.name()
                })
                .map(|next| XmlNodePtr::try_from(next).unwrap())
            {
                let mut tmp = xml_strdup(elem.content);
                tmp = xml_strcat(tmp, next.content);
                next.set_content(tmp);
                xml_free(tmp as _);
                xml_free_node(elem);
                return Some(next.into());
            }
        } else if matches!(elem.element_type(), XmlElementType::XmlAttributeNode) {
            return Some(
                add_prop_sibling(
                    XmlAttrPtr::try_from(XmlGenericNodePtr::from_raw(self).unwrap()).ok(),
                    XmlAttrPtr::try_from(XmlGenericNodePtr::from_raw(self).unwrap()).unwrap(),
                    XmlAttrPtr::try_from(elem).unwrap(),
                )
                .into(),
            );
        }

        if elem.document() != self.document() {
            elem.set_doc(self.document());
        }
        elem.set_parent(self.parent());
        elem.set_prev(XmlGenericNodePtr::from_raw(self));
        elem.set_next(self.next());
        self.set_next(Some(elem));
        if let Some(mut next) = elem.next() {
            next.set_prev(Some(elem));
        }
        if let Some(mut parent) = elem
            .parent()
            .filter(|p| p.last() == XmlGenericNodePtr::from_raw(self))
        {
            parent.set_last(Some(elem));
        }
        Some(elem)
    }

    /// Add a list of node at the end of the child list of the parent
    /// merging adjacent TEXT nodes (`cur` may be freed)
    ///
    /// See the note regarding namespaces in xmlAddChild.
    ///
    /// Returns the last child or NULL in case of error.
    #[doc(alias = "xmlAddChildList")]
    pub unsafe fn add_child_list(
        &mut self,
        mut cur: XmlGenericNodePtr,
    ) -> Option<XmlGenericNodePtr> {
        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }

        if matches!(cur.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }

        // add the first element at the end of the children list.
        if self.children().is_none() {
            self.set_children(Some(cur));
        } else {
            // If cur and self.last both are TEXT nodes, then merge them.
            if matches!(cur.element_type(), XmlElementType::XmlTextNode)
                && matches!(
                    self.last().unwrap().element_type(),
                    XmlElementType::XmlTextNode
                )
                && cur.name() == self.last().unwrap().name()
            {
                let node = XmlNodePtr::try_from(cur).unwrap();
                self.last().unwrap().add_content(node.content);
                // if it's the only child, nothing more to be done.
                let Some(next) = node.next() else {
                    xml_free_node(node);
                    return self.last();
                };
                let prev = node;
                cur = next;
                xml_free_node(prev);
            }
            let mut prev = self.last().unwrap();
            prev.set_next(Some(cur));
            cur.set_prev(Some(prev));
        }
        while let Some(next) = cur.next() {
            cur.set_parent(XmlGenericNodePtr::from_raw(self));
            if cur.document() != self.document() {
                cur.set_doc(self.document());
            }
            cur = next;
        }
        cur.set_parent(XmlGenericNodePtr::from_raw(self));
        // the parent may not be linked to a doc !
        if cur.document() != self.document() {
            cur.set_doc(self.document());
        }
        self.set_last(Some(cur));
        Some(cur)
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
        doc: Option<XmlDocPtr>,
        namespace: Option<&str>,
    ) -> Option<XmlNsPtr> {
        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }
        if namespace == Some("xml") {
            if doc.is_none() && matches!(self.element_type(), XmlElementType::XmlElementNode) {
                // The XML-1.0 namespace is normally held on the root element.
                // In this case exceptionally create it on the node element.
                let Some(cur) = XmlNsPtr::new(XmlNs {
                    typ: XML_LOCAL_NAMESPACE,
                    href: xml_strdup(XML_XML_NAMESPACE.as_ptr() as _),
                    prefix: xml_strdup(c"xml".as_ptr() as _),
                    next: self.ns_def,
                    ..Default::default()
                }) else {
                    xml_tree_err_memory("searching namespace");
                    return None;
                };
                self.ns_def = Some(cur);
                return Some(cur);
            }
            let mut doc = doc.or(self.document())?;
            // Return the XML namespace declaration held by the doc.
            if doc.old_ns.is_none() {
                return doc.ensure_xmldecl();
            } else {
                return doc.old_ns;
            }
        }
        let mut node = XmlGenericNodePtr::from_raw(self);
        let orig = node;
        while let Some(cur_node) = node {
            if matches!(
                cur_node.element_type(),
                XmlElementType::XmlEntityRefNode
                    | XmlElementType::XmlEntityNode
                    | XmlElementType::XmlEntityDecl
            ) {
                return None;
            }
            if matches!(cur_node.element_type(), XmlElementType::XmlElementNode) {
                let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                let mut cur = cur_node.ns_def;
                while let Some(now) = cur {
                    if now.prefix().is_none() && namespace.is_none() && !now.href.is_null() {
                        return Some(now);
                    }
                    if now.href().is_some()
                        && now.prefix().is_some()
                        && namespace == now.prefix().as_deref()
                    {
                        return Some(now);
                    }
                    cur = now.next;
                }
                if orig != node {
                    let cur = cur_node.ns;
                    if let Some(cur) = cur {
                        if cur.prefix().is_none() && namespace.is_none() && !cur.href.is_null() {
                            return Some(cur);
                        }
                        if cur.prefix().is_some()
                            && cur.href().is_some()
                            && namespace == cur.prefix().as_deref()
                        {
                            return Some(cur);
                        }
                    }
                }
            }
            node = cur_node.parent();
        }
        None
    }

    /// Search a Ns aliasing a given URI.
    /// Recurse on the parents until it finds the defined namespace or return NULL otherwise.
    ///
    /// Returns the namespace pointer or NULL.
    #[doc(alias = "xmlSearchNsByHref")]
    pub unsafe fn search_ns_by_href(
        &mut self,
        doc: Option<XmlDocPtr>,
        href: &str,
    ) -> Option<XmlNsPtr> {
        let orig = XmlGenericNodePtr::from_raw(self).unwrap();

        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }
        if href == XML_XML_NAMESPACE.to_str().unwrap() {
            // Only the document can hold the XML spec namespace.
            if doc.is_none() && matches!(self.element_type(), XmlElementType::XmlElementNode) {
                // The XML-1.0 namespace is normally held on the root element.
                // In this case exceptionally create it on the node element.
                let Some(cur) = XmlNsPtr::new(XmlNs {
                    typ: XML_LOCAL_NAMESPACE,
                    href: xml_strdup(XML_XML_NAMESPACE.as_ptr() as _),
                    prefix: xml_strdup(c"xml".as_ptr() as _),
                    next: self.ns_def,
                    ..Default::default()
                }) else {
                    xml_tree_err_memory("searching namespace");
                    return None;
                };
                self.ns_def = Some(cur);
                return Some(cur);
            }
            let mut doc = doc.or(self.document())?;
            // Return the XML namespace declaration held by the doc.
            if doc.old_ns.is_none() {
                return doc.ensure_xmldecl();
            } else {
                return doc.old_ns;
            }
        }
        let is_attr = matches!(self.element_type(), XmlElementType::XmlAttributeNode);
        let mut node = Some(orig);
        while let Some(cur_node) = node {
            if matches!(
                cur_node.element_type(),
                XmlElementType::XmlEntityRefNode
                    | XmlElementType::XmlEntityNode
                    | XmlElementType::XmlEntityDecl
            ) {
                return None;
            }
            if matches!(cur_node.element_type(), XmlElementType::XmlElementNode) {
                let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                let href = CString::new(href).unwrap();
                let mut cur = cur_node.ns_def;
                while let Some(now) = cur {
                    if !now.href.is_null()
                        && xml_str_equal(now.href, href.as_ptr() as *const u8)
                        && (!is_attr || now.prefix().is_some())
                        && xml_ns_in_scope(doc, Some(orig), node, now.prefix) == 1
                    {
                        return Some(now);
                    }
                    cur = now.next;
                }
                if orig != cur_node.into() {
                    let cur = cur_node.ns;
                    if let Some(cur) = cur.filter(|cur| {
                        !cur.href.is_null()
                            && xml_str_equal(cur.href, href.as_ptr() as *const u8)
                            && (!is_attr || (*cur).prefix().is_some())
                            && xml_ns_in_scope(doc, Some(orig), node, cur.prefix) == 1
                    }) {
                        return Some(cur);
                    }
                }
            }
            node = cur_node.parent();
        }
        None
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
    pub unsafe fn reconciliate_ns(&mut self, doc: XmlDocPtr) -> i32 {
        use super::xml_new_reconciled_ns;

        let mut old_ns = vec![];
        let mut new_ns = vec![];
        let ret: i32 = 0;

        if !matches!(self.element_type(), XmlElementType::XmlElementNode) {
            return -1;
        }
        if !matches!(doc.element_type(), XmlElementType::XmlDocumentNode) {
            return -1;
        }
        if self.document() != Some(doc) {
            return -1;
        }
        let mut node = XmlNodePtr::from_raw(self).unwrap();
        let orig = node;
        while let Some(mut cur_node) = node {
            // Reconciliate the node namespace
            if let Some(node_ns) = cur_node.ns.as_mut() {
                // initialize the cache if needed
                for (i, &ns) in old_ns.iter().enumerate() {
                    if ns == *node_ns {
                        *node_ns = new_ns[i];
                        // OK we need to recreate a new namespace definition
                        if let Some(n) = xml_new_reconciled_ns(
                            Some(doc),
                            XmlNodePtr::from_raw(self).unwrap().unwrap(),
                            *node_ns,
                        ) {
                            // :-( what if else ???
                            // check if we need to grow the cache buffers.
                            new_ns.push(n);
                            old_ns.push(*node_ns);
                            *node_ns = n;
                        }
                        break;
                    }
                }
            }
            // now check for namespace held by attributes on the node.
            if matches!(cur_node.typ, XmlElementType::XmlElementNode) {
                let mut attr = cur_node.properties;
                while let Some(mut now) = attr {
                    if let Some(mut attr_ns) = now.ns {
                        // initialize the cache if needed
                        for (i, &ns) in old_ns.iter().enumerate() {
                            if Some(ns) == now.ns {
                                now.ns = Some(new_ns[i]);
                                attr_ns = new_ns[i];
                                // OK we need to recreate a new namespace definition
                                if let Some(n) = xml_new_reconciled_ns(
                                    Some(doc),
                                    XmlNodePtr::from_raw(self).unwrap().unwrap(),
                                    attr_ns,
                                ) {
                                    // :-( what if else ???
                                    // check if we need to grow the cache buffers.
                                    new_ns.push(n);
                                    old_ns.push(attr_ns);
                                    now.ns = Some(n);
                                }
                                break;
                            }
                        }
                    }
                    attr = now.next;
                }
            }

            // Browse the full subtree, deep first
            if let Some(children) = cur_node
                .children()
                .filter(|_| !matches!(cur_node.element_type(), XmlElementType::XmlEntityRefNode))
            {
                // deep first
                node = Some(XmlNodePtr::try_from(children).unwrap());
            } else if let Some(next) = cur_node.next().filter(|_| Some(cur_node) != orig) {
                // then siblings
                node = Some(XmlNodePtr::try_from(next).unwrap());
            } else if Some(cur_node) != orig {
                let mut now = cur_node;
                // go up to parents->next if needed
                while Some(now) != orig {
                    if let Some(parent) = now.parent() {
                        now = XmlNodePtr::try_from(parent).unwrap();
                    }
                    if let Some(next) = now.next().filter(|_| Some(now) != orig) {
                        now = XmlNodePtr::try_from(next).unwrap();
                        node = Some(now);
                        break;
                    }
                    if now.parent().is_none() {
                        node = None;
                        break;
                    }
                }
                // exit condition
                if Some(now) == orig {
                    node = None;
                }
            } else {
                break;
            }
        }
        ret
    }
}

impl Default for XmlNode {
    fn default() -> Self {
        Self {
            _private: null_mut(),
            typ: XmlElementType::XmlElementNode,
            name: null(),
            children: None,
            last: None,
            parent: None,
            next: None,
            prev: None,
            doc: None,
            ns: None,
            content: null_mut(),
            properties: None,
            ns_def: None,
            psvi: null_mut(),
            line: 0,
            extra: 0,
        }
    }
}

impl NodeCommon for XmlNode {
    fn document(&self) -> Option<XmlDocPtr> {
        assert!(verify_xml_node(self), "Actual node type: {:?}", self.typ);
        self.doc
    }
    fn set_document(&mut self, doc: Option<XmlDocPtr>) {
        assert!(verify_xml_node(self), "Actual node type: {:?}", self.typ);
        self.doc = doc;
    }
    fn element_type(&self) -> XmlElementType {
        // assert!(verify_xml_node(self), "Actual node type: {:?}", self.typ);
        self.typ
    }
    fn name(&self) -> Option<Cow<'_, str>> {
        assert!(verify_xml_node(self), "Actual node type: {:?}", self.typ);
        (!self.name.is_null())
            .then(|| unsafe { CStr::from_ptr(self.name as *const i8).to_string_lossy() })
    }
    fn children(&self) -> Option<XmlGenericNodePtr> {
        assert!(verify_xml_node(self), "Actual node type: {:?}", self.typ);
        self.children
    }
    fn set_children(&mut self, children: Option<XmlGenericNodePtr>) {
        assert!(verify_xml_node(self), "Actual node type: {:?}", self.typ);
        self.children = children;
    }
    fn last(&self) -> Option<XmlGenericNodePtr> {
        assert!(verify_xml_node(self), "Actual node type: {:?}", self.typ);
        self.last
    }
    fn set_last(&mut self, last: Option<XmlGenericNodePtr>) {
        assert!(verify_xml_node(self), "Actual node type: {:?}", self.typ);
        self.last = last;
    }
    fn next(&self) -> Option<XmlGenericNodePtr> {
        assert!(verify_xml_node(self), "Actual node type: {:?}", self.typ);
        self.next
    }
    fn set_next(&mut self, next: Option<XmlGenericNodePtr>) {
        assert!(verify_xml_node(self), "Actual node type: {:?}", self.typ);
        self.next = next;
    }
    fn prev(&self) -> Option<XmlGenericNodePtr> {
        assert!(verify_xml_node(self), "Actual node type: {:?}", self.typ);
        self.prev
    }
    fn set_prev(&mut self, prev: Option<XmlGenericNodePtr>) {
        assert!(verify_xml_node(self), "Actual node type: {:?}", self.typ);
        self.prev = prev
    }
    fn parent(&self) -> Option<XmlGenericNodePtr> {
        assert!(verify_xml_node(self), "Actual node type: {:?}", self.typ);
        self.parent
    }
    fn set_parent(&mut self, parent: Option<XmlGenericNodePtr>) {
        assert!(verify_xml_node(self), "Actual node type: {:?}", self.typ);
        self.parent = parent
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct XmlNodePtr(NonNull<XmlNode>);

impl XmlNodePtr {
    /// Allocate new memory and create new `XmlNodePtr` from an owned xml node.
    ///
    /// This method leaks allocated memory.  
    /// Users can use `free` method for deallocating memory.
    pub(crate) fn new(node: XmlNode) -> Option<Self> {
        let boxed = Box::new(node);
        NonNull::new(Box::leak(boxed)).map(Self)
    }

    /// Create `XmlNodePtr` from a raw pointer.  
    ///
    /// If `ptr` is a NULL pointer, return `Ok(None)`.  
    /// If `ptr` is a valid pointer of `XmlNode`, return `Ok(Some(Self))`.  
    /// Otherwise, return `Err`.
    ///
    /// # Safety
    /// - `ptr` must be a pointer of types that is implemented `NodeCommon` at least.
    ///
    /// # TODO
    /// - fix to private mathod
    pub unsafe fn from_raw(ptr: *mut XmlNode) -> Result<Option<Self>, InvalidNodePointerCastError> {
        if ptr.is_null() {
            return Ok(None);
        }
        match (*ptr).element_type() {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlPINode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => Ok(Some(Self(NonNull::new_unchecked(ptr)))),
            _ => Err(InvalidNodePointerCastError {
                from: (*ptr).element_type(),
                to: type_name::<Self>(),
            }),
        }
    }

    // pub(crate) fn as_ptr(self) -> *mut XmlNode {
    //     self.0.as_ptr()
    // }

    /// Deallocate memory.
    ///
    /// # Safety
    /// This method should be called only once.  
    /// If called more than twice, the behavior is undefined.
    pub(crate) unsafe fn free(self) {
        let _ = *Box::from_raw(self.0.as_ptr());
    }

    /// Acquire the ownership of the inner value.  
    /// As a result, `self` will be invalid. `self` must not be used after performs this method.
    ///
    /// # Safety
    /// This method should be called only once.  
    /// If called more than twice, the behavior is undefined.
    pub(crate) unsafe fn into_inner(self) -> Box<XmlNode> {
        Box::from_raw(self.0.as_ptr())
    }
}

impl Clone for XmlNodePtr {
    fn clone(&self) -> Self {
        *self
    }
}

impl Copy for XmlNodePtr {}

impl Deref for XmlNodePtr {
    type Target = XmlNode;
    fn deref(&self) -> &Self::Target {
        // # Safety
        // I don't implement the pointer casting and addition/subtraction methods
        // and don't expose the inner `NonNull` for `*mut XmlNode`.
        // Therefore, as long as the constructor is correctly implemented,
        // the pointer dereference is valid.
        unsafe { self.0.as_ref() }
    }
}

impl DerefMut for XmlNodePtr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // # Safety
        // I don't implement the pointer casting and addition/subtraction methods
        // and don't expose the inner `NonNull` for `*mut XmlNode`.
        // Therefore, as long as the constructor is correctly implemented,
        // the pointer dereference is valid.
        unsafe { self.0.as_mut() }
    }
}

impl TryFrom<XmlGenericNodePtr> for XmlNodePtr {
    type Error = InvalidNodePointerCastError;

    fn try_from(value: XmlGenericNodePtr) -> Result<Self, Self::Error> {
        match value.element_type() {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlPINode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => Ok(Self(value.0.cast())),
            _ => Err(InvalidNodePointerCastError {
                from: value.element_type(),
                to: type_name::<Self>(),
            }),
        }
    }
}

impl From<XmlNodePtr> for XmlGenericNodePtr {
    fn from(value: XmlNodePtr) -> Self {
        Self(value.0 as NonNull<dyn NodeCommon>)
    }
}

impl From<XmlNodePtr> for *mut XmlNode {
    fn from(value: XmlNodePtr) -> Self {
        value.0.as_ptr()
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
pub(super) unsafe fn add_prop_sibling(
    prev: Option<XmlAttrPtr>,
    mut cur: XmlAttrPtr,
    mut prop: XmlAttrPtr,
) -> XmlAttrPtr {
    // check if an attribute with the same name exists
    let attr = if let Some(ns) = prop.ns {
        let href = ns.href();
        cur.parent()
            .map(|parent| XmlNodePtr::try_from(parent).unwrap())
            .expect("Internal Error")
            .has_ns_prop(&prop.name().unwrap(), href.as_deref())
    } else {
        cur.parent()
            .map(|parent| XmlNodePtr::try_from(parent).unwrap())
            .expect("Internal Error")
            .has_ns_prop(&prop.name().unwrap(), None)
    };

    if prop.doc != cur.doc {
        prop.set_doc(cur.doc);
    }
    prop.parent = cur.parent;
    prop.prev = prev;
    if let Some(mut prev) = prev {
        prop.next = prev.next;
        prev.next = Some(prop);
        if let Some(mut next) = prop.next {
            next.prev = Some(prop);
        }
    } else {
        prop.next = Some(cur);
        cur.prev = Some(prop);
    }
    if let Some(mut parent) = prop
        .parent()
        .filter(|_| prop.prev.is_none())
        .map(|parent| XmlNodePtr::try_from(parent).unwrap())
    {
        parent.properties = Some(prop);
    }
    if let Some(Ok(mut attr)) = attr {
        // ifferent instance, destroy it (attributes must e unique)
        attr.remove_prop();
    }
    prop
}
