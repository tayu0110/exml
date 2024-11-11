use std::{ffi::CString, os::raw::c_void, ptr::null_mut, sync::atomic::Ordering};

use libc::memset;

use crate::{
    hash::{xml_hash_lookup, xml_hash_remove_entry},
    libxml::{
        entities::{xml_get_doc_entity, XmlEntityPtr},
        globals::{xml_free, xml_malloc},
        uri::xml_build_uri,
        valid::xml_get_dtd_attr_desc,
        xmlstring::{
            xml_str_equal, xml_strcasecmp, xml_strcat, xml_strdup, xml_strlen, xml_strncmp, XmlChar,
        },
    },
};

use super::{
    xml_buf_create, xml_buf_create_size, xml_buf_detach, xml_buf_free, xml_buf_get_node_content,
    xml_buf_set_allocation_scheme, xml_free_node, xml_free_prop, xml_get_prop_node_internal,
    xml_get_prop_node_value_internal, xml_has_ns_prop, xml_is_blank_char, xml_node_add_content_len,
    xml_node_set_content, xml_ns_in_scope, xml_remove_prop, xml_set_tree_doc, xml_tree_err_memory,
    XmlAttr, XmlAttrPtr, XmlBufferAllocationScheme, XmlDoc, XmlDocPtr, XmlDtd, XmlElementType,
    XmlNs, XmlNsPtr, XML_CHECK_DTD, XML_LOCAL_NAMESPACE, XML_XML_NAMESPACE,
};

pub trait NodeCommon {
    fn element_type(&self) -> XmlElementType;
    fn name(&self) -> *const u8;
    fn children(&self) -> *mut XmlNode;
    fn set_children(&mut self, children: *mut XmlNode);
    fn last(&self) -> *mut XmlNode;
    fn set_last(&mut self, last: *mut XmlNode);
    fn parent(&self) -> *mut XmlNode;
    fn set_parent(&mut self, parent: *mut XmlNode);
    fn next(&self) -> *mut XmlNode;
    fn set_next(&mut self, next: *mut XmlNode);
    fn prev(&self) -> *mut XmlNode;
    fn set_prev(&mut self, prev: *mut XmlNode);
    fn document(&self) -> *mut XmlDoc;

    /// Add a new node to `self`, at the end of the child (or property) list
    /// merging adjacent TEXT nodes (in which case `cur` is freed)  
    /// If the new node is ATTRIBUTE, it is added into properties instead of children.  
    /// If there is an attribute with equal name, it is first destroyed.  
    ///
    /// All tree manipulation functions can safely move nodes within a document.  
    /// But when moving nodes from one document to another, references to
    /// namespaces in element or attribute nodes are NOT fixed. In this case,
    /// you MUST call xmlReconciliateNs after the move operation to avoid memory errors.
    ///
    /// Returns the child or NULL in case of error.
    #[doc(alias = "xmlAddChild")]
    unsafe fn add_child(&mut self, cur: XmlNodePtr) -> XmlNodePtr {
        let mut prev: XmlNodePtr;

        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        if cur.is_null() || matches!((*cur).element_type(), XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        if self as *mut Self as *mut XmlNode == cur {
            return null_mut();
        }
        /*
         * If cur is a TEXT node, merge its content with adjacent TEXT nodes
         * cur is then freed.
         */
        if matches!((*cur).typ, XmlElementType::XmlTextNode) {
            if matches!(self.element_type(), XmlElementType::XmlTextNode)
                && !(*(self as *mut Self as *mut XmlNode)).content.is_null()
                && self.name() == (*cur).name
            {
                (*(self as *mut Self as *mut XmlNode)).add_content((*cur).content);
                xml_free_node(cur);
                return self as *mut Self as *mut XmlNode;
            }
            if !self.last().is_null()
                && matches!((*self.last()).typ, XmlElementType::XmlTextNode)
                && ((*self.last()).name == (*cur).name)
                && (self.last() != cur)
            {
                (*self.last()).add_content((*cur).content);
                xml_free_node(cur);
                return self.last();
            }
        }

        /*
         * add the new element at the end of the children list.
         */
        prev = (*cur).parent;
        (*cur).parent = self as *mut Self as *mut XmlNode;
        if (*cur).doc != self.document() {
            xml_set_tree_doc(cur, self.document());
        }
        /* this check prevents a loop on tree-traversions if a developer
         * tries to add a node to its parent multiple times
         */
        if prev == self as *mut Self as *mut XmlNode {
            return cur;
        }

        /*
         * Coalescing
         */
        if matches!(self.element_type(), XmlElementType::XmlTextNode)
            && !(*(self as *mut Self as *mut XmlNode)).content.is_null()
            && self as *mut Self as *mut XmlNode != cur
        {
            (*(self as *mut Self as *mut XmlNode)).add_content((*cur).content);
            xml_free_node(cur);
            return self as *mut Self as *mut XmlNode;
        }
        if matches!((*cur).typ, XmlElementType::XmlAttributeNode) {
            if !matches!(self.element_type(), XmlElementType::XmlElementNode) {
                return null_mut();
            }
            if !(*(self as *mut Self as *mut XmlNode)).properties.is_null() {
                /* check if an attribute with the same name exists */

                let lastattr = if (*cur).ns.is_null() {
                    xml_has_ns_prop(self as *mut Self as *mut XmlNode, (*cur).name, null_mut())
                } else {
                    xml_has_ns_prop(
                        self as *mut Self as *mut XmlNode,
                        (*cur).name,
                        (*(*cur).ns).href.load(Ordering::Relaxed),
                    )
                };
                if !lastattr.is_null()
                    && lastattr != cur as _
                    && !matches!((*lastattr).typ, XmlElementType::XmlAttributeDecl)
                {
                    /* different instance, destroy it (attributes must be unique) */
                    (*lastattr).unlink();
                    xml_free_prop(lastattr);
                }
                if lastattr == cur as _ {
                    return cur;
                }
            }
            if (*(self as *mut Self as *mut XmlNode)).properties.is_null() {
                (*(self as *mut Self as *mut XmlNode)).properties = cur as _;
            } else {
                /* find the end */
                let mut lastattr = (*(self as *mut Self as *mut XmlNode)).properties;
                while !(*lastattr).next.is_null() {
                    lastattr = (*lastattr).next;
                }
                (*lastattr).next = cur as _;
                (*(cur as *mut XmlAttr)).prev = lastattr;
            }
        } else if self.children().is_null() {
            self.set_children(cur);
            self.set_last(cur);
        } else {
            prev = self.last();
            (*prev).next = cur;
            (*cur).prev = prev;
            self.set_last(cur);
        }
        cur
    }

    /// Unlink a node from it's current context, the node is not freed.  
    /// If one need to free the node, use xmlFreeNode() routine after the unlink to discard it.  
    ///
    /// Note that namespace nodes can't be unlinked as they do not have pointer to their parent.
    #[doc(alias = "xmlUnlinkNode")]

    unsafe fn unlink(&mut self) {
        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return;
        }

        if matches!(self.element_type(), XmlElementType::XmlDTDNode) {
            let doc = self.document();
            if !doc.is_null() {
                if (*doc).int_subset == self as *mut Self as *mut XmlDtd {
                    (*doc).int_subset = null_mut();
                }
                if (*doc).ext_subset == self as *mut Self as *mut XmlDtd {
                    (*doc).ext_subset = null_mut();
                }
            }
        }
        if matches!(self.element_type(), XmlElementType::XmlEntityDecl) {
            let doc = self.document();
            if !doc.is_null() {
                if !(*doc).int_subset.is_null() {
                    if xml_hash_lookup((*(*doc).int_subset).entities as _, self.name())
                        == self as *mut Self as *mut c_void
                    {
                        xml_hash_remove_entry(
                            (*(*doc).int_subset).entities as _,
                            self.name(),
                            None,
                        );
                    }
                    if xml_hash_lookup((*(*doc).int_subset).pentities as _, self.name())
                        == self as *mut Self as *mut c_void
                    {
                        xml_hash_remove_entry(
                            (*(*doc).int_subset).pentities as _,
                            self.name(),
                            None,
                        );
                    }
                }
                if !(*doc).ext_subset.is_null() {
                    if xml_hash_lookup((*(*doc).ext_subset).entities as _, self.name())
                        == self as *mut Self as *mut c_void
                    {
                        xml_hash_remove_entry(
                            (*(*doc).ext_subset).entities as _,
                            self.name(),
                            None,
                        );
                    }
                    if xml_hash_lookup((*(*doc).ext_subset).pentities as _, self.name())
                        == self as *mut Self as *mut c_void
                    {
                        xml_hash_remove_entry(
                            (*(*doc).ext_subset).pentities as _,
                            self.name(),
                            None,
                        );
                    }
                }
            }
        }
        if !self.parent().is_null() {
            let parent = self.parent();
            if matches!(self.element_type(), XmlElementType::XmlAttributeNode) {
                if (*parent).properties == self as *mut Self as *mut XmlAttr {
                    (*parent).properties = (*(self as *mut Self as *mut XmlAttr)).next;
                }
            } else {
                if (*parent).children == self as *mut Self as *mut XmlNode {
                    (*parent).children = self.next();
                }
                if (*parent).last == self as *mut Self as *mut XmlNode {
                    (*parent).last = self.prev();
                }
            }
            self.set_parent(null_mut());
        }
        if !self.next().is_null() {
            (*self.next()).prev = self.prev();
        }
        if !self.prev().is_null() {
            (*self.prev()).next = self.next();
        }
        self.set_next(null_mut());
        self.set_prev(null_mut());
    }
}

/// A node in an XML tree.
pub type XmlNodePtr = *mut XmlNode;
#[repr(C)]
pub struct XmlNode {
    pub _private: *mut c_void,       /* application data */
    pub typ: XmlElementType,         /* type number, must be second ! */
    pub name: *const XmlChar,        /* the name of the node, or the entity */
    pub children: *mut XmlNode,      /* parent->childs link */
    pub last: *mut XmlNode,          /* last child link */
    pub(crate) parent: *mut XmlNode, /* child->parent link */
    pub next: *mut XmlNode,          /* next sibling link  */
    pub(crate) prev: *mut XmlNode,   /* previous sibling link  */
    pub doc: *mut XmlDoc,            /* the containing document */

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
    /// Check whether this node is a Text node or not.
    #[doc(alias = "xmlNodeIsText")]
    pub fn is_text_node(&self) -> bool {
        matches!(self.typ, XmlElementType::XmlTextNode)
    }

    /// Checks whether this node is an empty or whitespace only (and possibly ignorable) text-node.
    #[doc(alias = "xmlIsBlankNode")]
    pub unsafe fn is_blank_node(&self) -> bool {
        if !matches!(
            self.typ,
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
            self.typ,
            XmlElementType::XmlElementNode
                | XmlElementType::XmlTextNode
                | XmlElementType::XmlCommentNode
                | XmlElementType::XmlPINode
        ) {
            if self.line == 65535 {
                if matches!(self.typ, XmlElementType::XmlTextNode) && !self.psvi.is_null() {
                    result = self.psvi as isize as i64;
                } else if matches!(self.typ, XmlElementType::XmlElementNode)
                    && !self.children.is_null()
                {
                    result = (*self.children).get_line_no_internal(depth + 1);
                } else if !self.next.is_null() {
                    result = (*self.next).get_line_no_internal(depth + 1);
                } else if !self.prev.is_null() {
                    result = (*self.prev).get_line_no_internal(depth + 1);
                }
            }
            if result == -1 || result == 65535 {
                result = self.line as i64;
            }
        } else if !self.prev.is_null()
            && matches!(
                (*self.prev).typ,
                XmlElementType::XmlElementNode
                    | XmlElementType::XmlTextNode
                    | XmlElementType::XmlCommentNode
                    | XmlElementType::XmlPINode
            )
        {
            result = (*self.prev).get_line_no_internal(depth + 1);
        } else if !self.parent.is_null()
            && matches!((*self.parent).typ, XmlElementType::XmlElementNode)
        {
            result = (*self.parent).get_line_no_internal(depth + 1);
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
    /// Returns the new path or null_mut() in case of error.  
    /// The caller must free the returned string.
    #[doc(alias = "xmlGetNodePath")]
    #[cfg(feature = "tree")]
    pub unsafe fn get_node_path(&self) -> *mut XmlChar {
        use std::{ptr::null_mut, sync::atomic::Ordering};

        use libc::snprintf;

        use crate::{
            libxml::{
                globals::{xml_free, xml_malloc_atomic, xml_realloc},
                xmlstring::{xml_str_equal, xml_strlen},
            },
            tree::{xml_tree_err_memory, XmlAttrPtr},
        };

        let mut tmp: *const XmlNode;
        let mut next: *const XmlNode;
        let mut buffer: *mut XmlChar;
        let mut temp: *mut XmlChar;
        let mut buf_len: usize;
        let mut buf: *mut XmlChar;
        let mut sep: *const i8;
        let mut name: *const i8;
        let mut nametemp: [i8; 100] = [0; 100];
        let mut occur: i32;
        let mut generic: i32;

        if matches!(self.typ, XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        buf_len = 500;
        buffer = xml_malloc_atomic(buf_len) as _;
        if buffer.is_null() {
            xml_tree_err_memory(c"getting node path".as_ptr() as _);
            return null_mut();
        }
        buf = xml_malloc_atomic(buf_len) as _;
        if buf.is_null() {
            xml_tree_err_memory(c"getting node path".as_ptr() as _);
            xml_free(buffer as _);
            return null_mut();
        }

        *buffer.add(0) = 0;
        let mut cur = self as *const XmlNode;
        while !cur.is_null() {
            name = c"".as_ptr() as _;
            // sep = c"?".as_ptr() as _;
            occur = 0;
            if matches!(
                (*cur).typ,
                XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode
            ) {
                if *buffer.add(0) == b'/' {
                    break;
                }
                sep = c"/".as_ptr() as _;
                next = null_mut();
            } else if matches!((*cur).typ, XmlElementType::XmlElementNode) {
                generic = 0;
                sep = c"/".as_ptr() as _;
                name = (*cur).name as _;
                if !(*cur).ns.is_null() {
                    if !(*(*cur).ns).prefix.load(Ordering::Relaxed).is_null() {
                        snprintf(
                            nametemp.as_mut_ptr() as _,
                            nametemp.len() - 1,
                            c"%s:%s".as_ptr() as _,
                            (*(*cur).ns).prefix.load(Ordering::Relaxed) as *const i8,
                            (*cur).name,
                        );
                        *nametemp.last_mut().unwrap() = 0;
                        name = nametemp.as_ptr() as _;
                    } else {
                        /*
                         * We cannot express named elements in the default
                         * namespace, so use "*".
                         */
                        generic = 1;
                        name = c"*".as_ptr() as _;
                    }
                }
                next = (*cur).parent;

                /*
                 * Thumbler index computation
                 * TODO: the occurrence test seems bogus for namespaced names
                 */
                tmp = (*cur).prev;
                while !tmp.is_null() {
                    if matches!((*tmp).typ, XmlElementType::XmlElementNode)
                        && (generic != 0
                            || (xml_str_equal((*cur).name, (*tmp).name)
                                && ((*tmp).ns == (*cur).ns
                                    || (!(*tmp).ns.is_null()
                                        && !(*cur).ns.is_null()
                                        && xml_str_equal(
                                            (*(*cur).ns).prefix.load(Ordering::Relaxed),
                                            (*(*tmp).ns).prefix.load(Ordering::Relaxed),
                                        )))))
                    {
                        occur += 1;
                    }
                    tmp = (*tmp).prev;
                }
                if occur == 0 {
                    tmp = (*cur).next;
                    while !tmp.is_null() && occur == 0 {
                        if matches!((*tmp).typ, XmlElementType::XmlElementNode)
                            && (generic != 0
                                || (xml_str_equal((*cur).name, (*tmp).name)
                                    && (((*tmp).ns == (*cur).ns)
                                        || (!(*tmp).ns.is_null()
                                            && !(*cur).ns.is_null()
                                            && (xml_str_equal(
                                                (*(*cur).ns).prefix.load(Ordering::Relaxed),
                                                (*(*tmp).ns).prefix.load(Ordering::Relaxed),
                                            ))))))
                        {
                            occur += 1;
                        }
                        tmp = (*tmp).next;
                    }
                    if occur != 0 {
                        occur = 1;
                    }
                } else {
                    occur += 1;
                }
            } else if matches!((*cur).typ, XmlElementType::XmlCommentNode) {
                sep = c"/".as_ptr() as _;
                name = c"comment()".as_ptr() as _;
                next = (*cur).parent;

                /*
                 * Thumbler index computation
                 */
                tmp = (*cur).prev;
                while !tmp.is_null() {
                    if matches!((*tmp).typ, XmlElementType::XmlCommentNode) {
                        occur += 1;
                    }
                    tmp = (*tmp).prev;
                }
                if occur == 0 {
                    tmp = (*cur).next;
                    while !tmp.is_null() && occur == 0 {
                        if matches!((*tmp).typ, XmlElementType::XmlCommentNode) {
                            occur += 1;
                        }
                        tmp = (*tmp).next;
                    }
                    if occur != 0 {
                        occur = 1;
                    }
                } else {
                    occur += 1;
                }
            } else if matches!(
                (*cur).typ,
                XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
            ) {
                sep = c"/".as_ptr() as _;
                name = c"text()".as_ptr() as _;
                next = (*cur).parent;

                /*
                 * Thumbler index computation
                 */
                tmp = (*cur).prev;
                while !tmp.is_null() {
                    if matches!(
                        (*tmp).typ,
                        XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
                    ) {
                        occur += 1;
                    }
                    tmp = (*tmp).prev;
                }
                /*
                 * Evaluate if this is the only text- or CDATA-section-node;
                 * if yes, then we'll get "text()".as_ptr() as _, otherwise "text()[1]".
                 */
                if occur == 0 {
                    tmp = (*cur).next;
                    while !tmp.is_null() {
                        if matches!(
                            (*tmp).typ,
                            XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
                        ) {
                            occur = 1;
                            break;
                        }
                        tmp = (*tmp).next;
                    }
                } else {
                    occur += 1;
                }
            } else if matches!((*cur).typ, XmlElementType::XmlPINode) {
                sep = c"/".as_ptr() as _;
                snprintf(
                    nametemp.as_mut_ptr() as _,
                    nametemp.len() - 1,
                    c"processing-instruction('%s')".as_ptr() as _,
                    (*cur).name,
                );
                *nametemp.last_mut().unwrap() = 0;
                name = nametemp.as_ptr() as _;

                next = (*cur).parent;

                /*
                 * Thumbler index computation
                 */
                tmp = (*cur).prev;
                while !tmp.is_null() {
                    if matches!((*tmp).typ, XmlElementType::XmlPINode)
                        && xml_str_equal((*cur).name, (*tmp).name)
                    {
                        occur += 1;
                    }
                    tmp = (*tmp).prev;
                }
                if occur == 0 {
                    tmp = (*cur).next;
                    while !tmp.is_null() && occur == 0 {
                        if matches!((*tmp).typ, XmlElementType::XmlPINode)
                            && xml_str_equal((*cur).name, (*tmp).name)
                        {
                            occur += 1;
                        }
                        tmp = (*tmp).next;
                    }
                    if occur != 0 {
                        occur = 1;
                    }
                } else {
                    occur += 1;
                }
            } else if matches!((*cur).typ, XmlElementType::XmlAttributeNode) {
                sep = c"/@".as_ptr() as _;
                name = (*(cur as XmlAttrPtr)).name as _;
                if !(*cur).ns.is_null() {
                    if !(*(*cur).ns).prefix.load(Ordering::Relaxed).is_null() {
                        snprintf(
                            nametemp.as_mut_ptr() as _,
                            nametemp.len() - 1,
                            c"%s:%s".as_ptr() as _,
                            (*(*cur).ns).prefix.load(Ordering::Relaxed) as *const i8,
                            (*cur).name,
                        );
                    } else {
                        snprintf(
                            nametemp.as_mut_ptr() as _,
                            nametemp.len() - 1,
                            c"%s".as_ptr() as _,
                            (*cur).name,
                        );
                    }
                    *nametemp.last_mut().unwrap() = 0;
                    name = nametemp.as_ptr() as _;
                }
                next = (*(cur as XmlAttrPtr)).parent;
            } else {
                xml_free(buf as _);
                xml_free(buffer as _);
                return null_mut();
            }

            /*
             * Make sure there is enough room
             */
            if xml_strlen(buffer) as usize + nametemp.len() + 20 > buf_len {
                buf_len = 2 * buf_len + xml_strlen(buffer) as usize + nametemp.len() + 20;
                temp = xml_realloc(buffer as _, buf_len) as _;
                if temp.is_null() {
                    xml_tree_err_memory(c"getting node path".as_ptr() as _);
                    xml_free(buf as _);
                    xml_free(buffer as _);
                    return null_mut();
                }
                buffer = temp;
                temp = xml_realloc(buf as _, buf_len) as _;
                if temp.is_null() {
                    xml_tree_err_memory(c"getting node path".as_ptr() as _);
                    xml_free(buf as _);
                    xml_free(buffer as _);
                    return null_mut();
                }
                buf = temp;
            }
            if occur == 0 {
                snprintf(
                    buf as _,
                    buf_len,
                    c"%s%s%s".as_ptr() as _,
                    sep,
                    name,
                    buffer,
                );
            } else {
                snprintf(
                    buf as _,
                    buf_len,
                    c"%s%s[%d]%s".as_ptr() as _,
                    sep,
                    name,
                    occur,
                    buffer,
                );
            }
            snprintf(buffer as _, buf_len, c"%s".as_ptr() as _, buf);
            cur = next;
        }
        xml_free(buf as _);
        buffer
    }

    /// Search the last child of a node.
    /// Returns the last child or null_mut() if none.
    #[doc(alias = "xmlGetLastChild")]
    pub fn get_last_child(&self) -> XmlNodePtr {
        if matches!(self.typ, XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }
        self.last
    }

    /// Searches for the BASE URL. The code should work on both XML
    /// and HTML document even if base mechanisms are completely different.  
    /// It returns the base as defined in RFC 2396 sections
    /// 5.1.1. Base URI within Document Content and  5.1.2. Base URI from the Encapsulating Entity.  
    /// However it does not return the document base (5.1.3), use `doc.url` in this case
    ///
    /// Returns a pointer to the base URL, or NULL if not found.  
    /// It's up to the caller to free the memory with `xml_free()`.
    #[doc(alias = "xmlNodeGetBase")]
    pub unsafe fn get_base(&self, mut doc: *const XmlDoc) -> *mut XmlChar {
        let mut oldbase: *mut XmlChar = null_mut();
        let mut base: *mut XmlChar;
        let mut newbase: *mut XmlChar;

        if matches!(self.typ, XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }
        if doc.is_null() {
            doc = self.doc;
        }
        let mut cur = self as *const XmlNode;
        if !doc.is_null() && matches!((*doc).typ, XmlElementType::XmlHTMLDocumentNode) {
            cur = (*doc).children;
            while !cur.is_null() && !(*cur).name.is_null() {
                if !matches!((*cur).typ, XmlElementType::XmlElementNode) {
                    cur = (*cur).next;
                    continue;
                }
                if xml_strcasecmp((*cur).name, c"html".as_ptr() as _) == 0 {
                    cur = (*cur).children;
                    continue;
                }
                if xml_strcasecmp((*cur).name, c"head".as_ptr() as _) == 0 {
                    cur = (*cur).children;
                    continue;
                }
                if xml_strcasecmp((*cur).name, c"base".as_ptr() as _) == 0 {
                    return (*cur).get_prop(c"href".as_ptr() as _);
                }
                cur = (*cur).next;
            }
            return null_mut();
        }
        while !cur.is_null() {
            if matches!((*cur).typ, XmlElementType::XmlEntityDecl) {
                let ent = cur as XmlEntityPtr;
                return xml_strdup((*ent).uri.load(Ordering::Relaxed));
            }
            if matches!((*cur).typ, XmlElementType::XmlElementNode) {
                base = (*cur).get_ns_prop(c"base".as_ptr() as _, XML_XML_NAMESPACE.as_ptr() as _);
                if !base.is_null() {
                    if !oldbase.is_null() {
                        newbase = xml_build_uri(oldbase, base);
                        if !newbase.is_null() {
                            xml_free(oldbase as _);
                            xml_free(base as _);
                            oldbase = newbase;
                        } else {
                            xml_free(oldbase as _);
                            xml_free(base as _);
                            return null_mut();
                        }
                    } else {
                        oldbase = base;
                    }
                    if xml_strncmp(oldbase, c"http://".as_ptr() as _, 7) == 0
                        || xml_strncmp(oldbase, c"ftp://".as_ptr() as _, 6) == 0
                        || xml_strncmp(oldbase, c"urn:".as_ptr() as _, 4) == 0
                    {
                        return oldbase;
                    }
                }
            }
            cur = (*cur).parent;
        }
        if !doc.is_null() && (*doc).url.is_some() {
            let url = CString::new((*doc).url.as_deref().unwrap()).unwrap();
            if oldbase.is_null() {
                return xml_strdup(url.as_ptr() as *const u8);
            }
            newbase = xml_build_uri(oldbase, url.as_ptr() as *const u8);
            xml_free(oldbase as _);
            return newbase;
        }
        oldbase
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
    pub unsafe fn get_content(&self) -> *mut XmlChar {
        match self.typ {
            XmlElementType::XmlDocumentFragNode | XmlElementType::XmlElementNode => {
                let buf = xml_buf_create_size(64);
                if buf.is_null() {
                    return null_mut();
                }
                xml_buf_set_allocation_scheme(
                    buf,
                    XmlBufferAllocationScheme::XmlBufferAllocDoubleit,
                );
                xml_buf_get_node_content(buf, self);
                let ret: *mut XmlChar = xml_buf_detach(buf);
                xml_buf_free(buf);
                ret
            }
            XmlElementType::XmlAttributeNode => {
                xml_get_prop_node_value_internal(self as *const XmlNode as *const XmlAttr)
            }
            XmlElementType::XmlCommentNode | XmlElementType::XmlPINode => {
                if !self.content.is_null() {
                    return xml_strdup(self.content);
                }
                null_mut()
            }
            XmlElementType::XmlEntityRefNode => {
                /* lookup entity declaration */
                let ent: XmlEntityPtr = xml_get_doc_entity(self.doc, self.name);
                if ent.is_null() {
                    return null_mut();
                }

                let buf = xml_buf_create();
                if buf.is_null() {
                    return null_mut();
                }
                xml_buf_set_allocation_scheme(
                    buf,
                    XmlBufferAllocationScheme::XmlBufferAllocDoubleit,
                );

                xml_buf_get_node_content(buf, self);

                let ret: *mut XmlChar = xml_buf_detach(buf);
                xml_buf_free(buf);
                ret
            }
            XmlElementType::XmlEntityNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlDTDNode
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => null_mut(),
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                let buf = xml_buf_create();
                if buf.is_null() {
                    return null_mut();
                }
                xml_buf_set_allocation_scheme(
                    buf,
                    XmlBufferAllocationScheme::XmlBufferAllocDoubleit,
                );

                xml_buf_get_node_content(buf, self);

                let ret: *mut XmlChar = xml_buf_detach(buf);
                xml_buf_free(buf);
                ret
            }
            XmlElementType::XmlNamespaceDecl => {
                let tmp: *mut XmlChar = xml_strdup(
                    (*(self as *const XmlNode as *const XmlNs))
                        .href
                        .load(Ordering::Relaxed),
                );
                tmp
            }
            XmlElementType::XmlElementDecl => {
                /* TODO !!! */
                null_mut()
            }
            XmlElementType::XmlAttributeDecl => {
                /* TODO !!! */
                null_mut()
            }
            XmlElementType::XmlEntityDecl => {
                /* TODO !!! */
                null_mut()
            }
            XmlElementType::XmlCDATASectionNode | XmlElementType::XmlTextNode => {
                if !self.content.is_null() {
                    return xml_strdup(self.content);
                }
                null_mut()
            }
            _ => unreachable!(),
        }
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
    pub unsafe fn get_prop(&self, name: *const XmlChar) -> *mut XmlChar {
        let prop: XmlAttrPtr = self.has_prop(name);
        if prop.is_null() {
            return null_mut();
        }
        xml_get_prop_node_value_internal(prop)
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
    pub unsafe fn get_ns_prop(
        &self,
        name: *const XmlChar,
        name_space: *const XmlChar,
    ) -> *mut XmlChar {
        let prop: XmlAttrPtr = xml_get_prop_node_internal(
            self,
            name,
            name_space,
            XML_CHECK_DTD.load(Ordering::Relaxed),
        );
        if prop.is_null() {
            return null_mut();
        }
        xml_get_prop_node_value_internal(prop)
    }

    /// Search all the namespace applying to a given element.
    ///
    /// Returns an NULL terminated array of all the `xmlNsPtr` found
    /// that need to be freed by the caller or NULL if no namespace if defined.
    #[doc(alias = "xmlGetNsList")]
    #[cfg(any(feature = "tree", feature = "xpath", feature = "schema"))]
    pub unsafe fn get_ns_list(&self, _doc: *const XmlDoc) -> *mut XmlNsPtr {
        use crate::{
            libxml::{globals::xml_realloc, xmlstring::xml_str_equal},
            tree::xml_tree_err_memory,
        };

        let mut cur: XmlNsPtr;
        let mut ret: *mut XmlNsPtr = null_mut();
        let mut nbns: i32 = 0;
        let mut maxns: i32 = 0;

        if matches!(self.typ, XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        let mut node = self as *const XmlNode;
        while !node.is_null() {
            if matches!((*node).typ, XmlElementType::XmlElementNode) {
                cur = (*node).ns_def;
                'b: while !cur.is_null() {
                    for i in 0..nbns {
                        if ((*cur).prefix.load(Ordering::Relaxed)
                            == (*(*ret.add(i as usize))).prefix.load(Ordering::Relaxed))
                            || xml_str_equal(
                                (*cur).prefix.load(Ordering::Relaxed),
                                (*(*ret.add(i as usize))).prefix.load(Ordering::Relaxed),
                            )
                        {
                            cur = (*cur).next.load(Ordering::Relaxed);
                            continue 'b;
                        }
                    }
                    if nbns >= maxns {
                        maxns = if maxns != 0 { maxns * 2 } else { 10 };
                        let tmp: *mut XmlNsPtr =
                            xml_realloc(ret as _, (maxns as usize + 1) * size_of::<XmlNsPtr>())
                                as _;
                        if tmp.is_null() {
                            xml_tree_err_memory(c"getting namespace list".as_ptr() as _);
                            xml_free(ret as _);
                            return null_mut();
                        }
                        ret = tmp;
                    }
                    *ret.add(nbns as usize) = cur;
                    nbns += 1;
                    *ret.add(nbns as usize) = null_mut();
                }
            }
            node = (*node).parent;
        }
        ret
    }

    /// Searches the language of a node, i.e. the values of the xml:lang
    /// attribute or the one carried by the nearest ancestor.
    ///
    /// Returns a pointer to the lang value, or null_mut() if not found.  
    /// It's up to the caller to free the memory with xml_free().
    #[doc(alias = "xmlNodeGetLang")]
    pub unsafe fn get_lang(&self) -> *mut XmlChar {
        if matches!(self.typ, XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }
        let mut cur = self as *const XmlNode;
        while !cur.is_null() {
            let lang = (*cur).get_ns_prop(c"lang".as_ptr() as _, XML_XML_NAMESPACE.as_ptr() as _);
            if !lang.is_null() {
                return lang;
            }
            cur = (*cur).parent;
        }
        null_mut()
    }

    /// Set (or reset) the name of a node.
    #[doc(alias = "xmlNodeSetName")]
    #[cfg(feature = "tree")]
    pub unsafe fn set_name(&mut self, name: *const XmlChar) {
        use crate::{
            dict::{xml_dict_lookup, xml_dict_owns},
            libxml::{globals::xml_free, xmlstring::xml_strdup},
        };

        let mut freeme: *const XmlChar = null_mut();

        if name.is_null() {
            return;
        }
        match self.typ {
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
        let doc = self.doc;
        let dict = if !doc.is_null() {
            (*doc).dict
        } else {
            null_mut()
        };
        if !dict.is_null() {
            if !self.name.is_null() && xml_dict_owns(dict, self.name) == 0 {
                freeme = self.name;
            }
            self.name = xml_dict_lookup(dict, name, -1);
        } else {
            if !self.name.is_null() {
                freeme = self.name;
            }
            self.name = xml_strdup(name);
        }

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
        feature = "tree",
        feature = "xinclude",
        feature = "schema",
        feature = "html"
    ))]
    pub unsafe fn set_prop(&mut self, name: *const XmlChar, value: *const XmlChar) -> XmlAttrPtr {
        use crate::{libxml::xmlstring::xml_strndup, tree::xml_search_ns};

        use super::xml_split_qname3;

        let mut len: i32 = 0;

        if name.is_null() || !matches!(self.typ, XmlElementType::XmlElementNode) {
            return null_mut();
        }

        // handle QNames
        let nqname: *const XmlChar = xml_split_qname3(name, &raw mut len);
        if !nqname.is_null() {
            let prefix: *mut XmlChar = xml_strndup(name, len);
            let ns: XmlNsPtr = xml_search_ns(self.doc, self, prefix);
            if !prefix.is_null() {
                xml_free(prefix as _);
            }
            if !ns.is_null() {
                return self.set_ns_prop(ns, nqname, value);
            }
        }
        self.set_ns_prop(null_mut(), name, value)
    }

    /// Set (or reset) an attribute carried by a node.  
    /// The ns structure must be in scope, this is not checked.
    ///
    /// Returns the attribute pointer.
    #[doc(alias = "xmlSetNsProp")]
    #[cfg(any(
        feature = "tree",
        feature = "xinclude",
        feature = "schema",
        feature = "html"
    ))]
    pub unsafe fn set_ns_prop(
        &mut self,
        ns: XmlNsPtr,
        name: *const XmlChar,
        value: *const XmlChar,
    ) -> XmlAttrPtr {
        use crate::{
            libxml::valid::{xml_add_id, xml_remove_id},
            tree::{xml_free_node_list, xml_new_doc_text, xml_new_prop_internal, XmlAttributeType},
        };

        use super::xml_get_prop_node_internal;

        if !ns.is_null() && (*ns).href.load(Ordering::Relaxed).is_null() {
            return null_mut();
        }
        let prop = xml_get_prop_node_internal(
            self,
            name,
            if !ns.is_null() {
                (*ns).href.load(Ordering::Relaxed)
            } else {
                null_mut()
            },
            0,
        );
        if !prop.is_null() {
            /*
            	* Modify the attribute's value.
            	*/
            if matches!((*prop).atype, Some(XmlAttributeType::XmlAttributeID)) {
                xml_remove_id(self.doc, prop);
                (*prop).atype = Some(XmlAttributeType::XmlAttributeID);
            }
            if !(*prop).children.is_null() {
                xml_free_node_list((*prop).children);
            }
            (*prop).children = null_mut();
            (*prop).last = null_mut();
            (*prop).ns = ns;
            if !value.is_null() {
                let mut tmp: XmlNodePtr;

                (*prop).children = xml_new_doc_text(self.doc, value);
                (*prop).last = null_mut();
                tmp = (*prop).children;
                while !tmp.is_null() {
                    (*tmp).parent = prop as _;
                    if (*tmp).next.is_null() {
                        (*prop).last = tmp;
                    }
                    tmp = (*tmp).next;
                }
            }
            if matches!((*prop).atype, Some(XmlAttributeType::XmlAttributeID)) {
                xml_add_id(null_mut(), self.doc, value, prop);
            }
            return prop;
        }
        /*
         * No equal attr found; create a new one.
         */
        xml_new_prop_internal(self, ns, name, value, 0)
    }

    /// Set (or reset) the base URI of a node, i.e. the value of the xml:base attribute.
    #[doc(alias = "xmlNodeSetBase")]
    #[cfg(any(feature = "tree", feature = "xinclude"))]
    pub unsafe fn set_base(&mut self, uri: *const XmlChar) {
        use std::ffi::CStr;

        use crate::{libxml::uri::xml_path_to_uri, tree::XmlDocPtr};

        match self.typ {
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
                let doc = self as *mut XmlNode as XmlDocPtr;

                if uri.is_null() {
                    (*doc).url = None;
                } else {
                    let uri = xml_path_to_uri(uri);
                    if !uri.is_null() {
                        (*doc).url = Some(
                            CStr::from_ptr(uri as *const i8)
                                .to_string_lossy()
                                .into_owned(),
                        );
                        xml_free(uri as _);
                    } else {
                        (*doc).url = None;
                    }
                }
                return;
            }
            _ => unreachable!(),
        }

        let ns: XmlNsPtr = self.search_ns_by_href(self.doc, XML_XML_NAMESPACE.as_ptr() as _);
        if ns.is_null() {
            return;
        }
        let fixed: *mut XmlChar = xml_path_to_uri(uri);
        if !fixed.is_null() {
            self.set_ns_prop(ns, c"base".as_ptr() as _, fixed);
            xml_free(fixed as _);
        } else {
            self.set_ns_prop(ns, c"base".as_ptr() as _, uri);
        }
    }

    /// Set the language of a node, i.e. the values of the xml:lang
    /// attribute.
    #[doc(alias = "xmlNodeSetLang")]
    #[cfg(feature = "tree")]
    pub unsafe fn set_lang(&mut self, lang: *const XmlChar) {
        match self.typ {
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
        let ns: XmlNsPtr = self.search_ns_by_href(self.doc, XML_XML_NAMESPACE.as_ptr() as _);
        if ns.is_null() {
            return;
        }
        self.set_ns_prop(ns, c"lang".as_ptr() as _, lang);
    }

    /// Search an attribute associated to a node.  
    ///
    /// This function also looks in DTD attribute declaration for #FIXED or
    /// default declaration values unless DTD use has been turned off.
    ///
    /// Returns the attribute or the attribute declaration or NULL if neither was found.
    #[doc(alias = "xmlHasProp")]
    pub unsafe fn has_prop(&self, name: *const XmlChar) -> XmlAttrPtr {
        if !matches!(self.typ, XmlElementType::XmlElementNode) || name.is_null() {
            return null_mut();
        }
        /*
         * Check on the properties attached to the node
         */
        let mut prop = self.properties;
        while !prop.is_null() {
            if xml_str_equal((*prop).name, name) {
                return prop;
            }
            prop = (*prop).next;
        }
        if XML_CHECK_DTD.load(Ordering::Relaxed) == 0 {
            return null_mut();
        }

        /*
         * Check if there is a default declaration in the internal
         * or external subsets
         */
        let doc = self.doc;
        if !doc.is_null() && !(*doc).int_subset.is_null() {
            let mut attr_decl = xml_get_dtd_attr_desc((*doc).int_subset, self.name, name);
            if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                attr_decl = xml_get_dtd_attr_desc((*doc).ext_subset, self.name, name);
            }
            if !attr_decl.is_null() && !(*attr_decl).default_value.is_null() {
                // return attribute declaration only if a default value is given
                // (that includes #FIXED declarations)
                return attr_decl as _;
            }
        }
        null_mut()
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
    pub unsafe fn add_sibling(&mut self, elem: XmlNodePtr) -> XmlNodePtr {
        if matches!(self.typ, XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        if elem.is_null() || ((*elem).typ == XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        let mut cur = self as *mut XmlNode;
        if cur == elem {
            return null_mut();
        }

        // Constant time is we can rely on the -> parent -> last to find the last sibling.
        if !matches!(self.typ, XmlElementType::XmlAttributeNode)
            && !self.parent.is_null()
            && !(*self.parent).children.is_null()
            && !(*self.parent).last.is_null()
            && (*(*self.parent).last).next.is_null()
        {
            cur = (*(*cur).parent).last;
        } else {
            while !(*cur).next.is_null() {
                cur = (*cur).next;
            }
        }

        (*elem).unlink();

        if (matches!((*cur).typ, XmlElementType::XmlTextNode)
            && matches!((*elem).typ, XmlElementType::XmlTextNode)
            && (*cur).name == (*elem).name)
        {
            (*cur).add_content((*elem).content);
            xml_free_node(elem);
            return cur;
        } else if matches!((*elem).typ, XmlElementType::XmlAttributeNode) {
            return add_prop_sibling(cur, cur, elem);
        }

        if (*elem).doc != (*cur).doc {
            xml_set_tree_doc(elem, (*cur).doc);
        }
        let parent: XmlNodePtr = (*cur).parent;
        (*elem).prev = cur;
        (*elem).next = null_mut();
        (*elem).parent = parent;
        (*cur).next = elem;
        if !parent.is_null() {
            (*parent).last = elem;
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
        feature = "tree",
        feature = "html",
        feature = "schema",
        feature = "xinclude"
    ))]
    pub unsafe fn add_prev_sibling(&mut self, elem: XmlNodePtr) -> XmlNodePtr {
        use crate::{
            libxml::{
                globals::xml_free,
                xmlstring::{xml_strcat, xml_strdup},
            },
            tree::xml_node_set_content,
        };

        if matches!(self.typ, XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }
        if elem.is_null() || ((*elem).typ == XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        if self as *mut XmlNode == elem {
            return null_mut();
        }

        (*elem).unlink();

        if matches!((*elem).typ, XmlElementType::XmlTextNode) {
            if matches!(self.typ, XmlElementType::XmlTextNode) {
                let mut tmp: *mut XmlChar;

                tmp = xml_strdup((*elem).content);
                tmp = xml_strcat(tmp, self.content);
                xml_node_set_content(self, tmp);
                xml_free(tmp as _);
                xml_free_node(elem);
                return self as *mut XmlNode;
            }
            if !self.prev.is_null()
                && matches!((*self.prev).typ, XmlElementType::XmlTextNode)
                && (self.name == (*self.prev).name)
            {
                (*self.prev).add_content((*elem).content);
                xml_free_node(elem);
                return self.prev;
            }
        } else if matches!((*elem).typ, XmlElementType::XmlAttributeNode) {
            return add_prop_sibling(self.prev, self, elem);
        }

        if (*elem).doc != self.doc {
            xml_set_tree_doc(elem, self.doc);
        }
        (*elem).parent = self.parent;
        (*elem).next = self as *mut XmlNode;
        (*elem).prev = self.prev;
        self.prev = elem;
        if !(*elem).prev.is_null() {
            (*(*elem).prev).next = elem;
        }
        if !(*elem).parent.is_null() && (*(*elem).parent).children == self as *mut XmlNode {
            (*(*elem).parent).children = elem;
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
    pub unsafe extern "C" fn add_next_sibling(&mut self, elem: XmlNodePtr) -> XmlNodePtr {
        if matches!(self.typ, XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }
        if elem.is_null() || ((*elem).typ == XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        if self as *mut XmlNode == elem {
            return null_mut();
        }

        (*elem).unlink();

        if matches!((*elem).typ, XmlElementType::XmlTextNode) {
            if matches!(self.typ, XmlElementType::XmlTextNode) {
                self.add_content((*elem).content);
                xml_free_node(elem);
                return self as *mut XmlNode;
            }
            if !self.next.is_null()
                && matches!((*self.next).typ, XmlElementType::XmlTextNode)
                && self.name == (*self.next).name
            {
                let mut tmp: *mut XmlChar;

                tmp = xml_strdup((*elem).content);
                tmp = xml_strcat(tmp, (*self.next).content);
                xml_node_set_content(self.next, tmp);
                xml_free(tmp as _);
                xml_free_node(elem);
                return self.next;
            }
        } else if matches!((*elem).typ, XmlElementType::XmlAttributeNode) {
            return add_prop_sibling(self, self, elem);
        }

        if (*elem).doc != self.doc {
            xml_set_tree_doc(elem, self.doc);
        }
        (*elem).parent = self.parent;
        (*elem).prev = self as *mut XmlNode;
        (*elem).next = self.next;
        self.next = elem;
        if !(*elem).next.is_null() {
            (*(*elem).next).prev = elem;
        }
        if !(*elem).parent.is_null() && (*(*elem).parent).last == self as *mut XmlNode {
            (*(*elem).parent).last = elem;
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
    pub unsafe fn add_child_list(&mut self, mut cur: XmlNodePtr) -> XmlNodePtr {
        let mut prev: XmlNodePtr;

        if matches!(self.typ, XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        if cur.is_null() || matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        /*
         * add the first element at the end of the children list.
         */

        if self.children.is_null() {
            self.children = cur;
        } else {
            /*
             * If cur and self.last both are TEXT nodes, then merge them.
             */
            if matches!((*cur).typ, XmlElementType::XmlTextNode)
                && matches!((*self.last).typ, XmlElementType::XmlTextNode)
                && (*cur).name == (*self.last).name
            {
                (*self.last).add_content((*cur).content);
                /*
                 * if it's the only child, nothing more to be done.
                 */
                if (*cur).next.is_null() {
                    xml_free_node(cur);
                    return self.last;
                }
                prev = cur;
                cur = (*cur).next;
                xml_free_node(prev);
            }
            prev = self.last;
            (*prev).next = cur;
            (*cur).prev = prev;
        }
        while !(*cur).next.is_null() {
            (*cur).parent = self;
            if (*cur).doc != self.doc {
                xml_set_tree_doc(cur, self.doc);
            }
            cur = (*cur).next;
        }
        (*cur).parent = self;
        /* the parent may not be linked to a doc ! */
        if (*cur).doc != self.doc {
            xml_set_tree_doc(cur, self.doc);
        }
        self.last = cur;

        cur
    }

    /// Append the extra substring to the node content.
    ///
    /// # Note
    /// In contrast to xmlNodeSetContent(), @content is supposed to be raw text,
    /// so unescaped XML special chars are allowed, entity references are not supported.
    #[doc(alias = "xmlNodeAddContent")]
    pub unsafe fn add_content(&mut self, content: *const XmlChar) {
        if content.is_null() {
            return;
        }
        let len: i32 = xml_strlen(content);
        xml_node_add_content_len(self, content, len);
    }

    /// Finds the current number of child nodes of that element which are element nodes.
    ///
    /// Note the handling of entities references is different than in
    /// the W3C DOM element traversal spec since we don't have back reference
    /// from entities content to entities references.
    ///
    /// Returns the count of element child or 0 if not available
    #[doc(alias = "xmlChildElementCount")]
    #[cfg(feature = "tree")]
    pub unsafe fn child_element_count(&self) -> u64 {
        let mut ret = 0;

        let mut cur = match self.typ {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlDocumentNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlHTMLDocumentNode => self.children,
            _ => {
                return 0;
            }
        };
        while !cur.is_null() {
            if matches!((*cur).typ, XmlElementType::XmlElementNode) {
                ret += 1;
            }
            cur = (*cur).next;
        }
        ret
    }

    /// Finds the first closest next sibling of the node which is an element node.
    ///
    /// Note the handling of entities references is different than in
    /// the W3C DOM element traversal spec since we don't have back reference
    /// from entities content to entities references.
    ///
    /// Returns the next element sibling or NULL if not available
    #[doc(alias = "xmlNextElementSibling")]
    #[cfg(feature = "tree")]
    pub unsafe fn next_element_sibling(&self) -> XmlNodePtr {
        let mut cur = match self.typ {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlPINode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlDTDNode
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => self.next,
            _ => {
                return null_mut();
            }
        };
        while !cur.is_null() {
            if matches!((*cur).typ, XmlElementType::XmlElementNode) {
                return cur;
            }
            cur = (*cur).next;
        }
        null_mut()
    }

    /// Finds the first child node of that element which is a Element node.
    ///
    /// Note the handling of entities references is different than in
    /// the W3C DOM element traversal spec since we don't have back reference
    /// from entities content to entities references.
    ///
    /// Returns the first element child or NULL if not available.
    #[doc(alias = "xmlFirstElementChild")]
    #[cfg(feature = "tree")]
    pub unsafe fn first_element_child(&self) -> XmlNodePtr {
        let mut cur = match self.typ {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlDocumentNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlHTMLDocumentNode => self.children,
            _ => {
                return null_mut();
            }
        };
        while !cur.is_null() {
            if matches!((*cur).typ, XmlElementType::XmlElementNode) {
                return cur;
            }
            cur = (*cur).next;
        }
        null_mut()
    }

    /// Finds the last child node of that element which is a Element node.
    ///
    /// Note the handling of entities references is different than in
    /// the W3C DOM element traversal spec since we don't have back reference
    /// from entities content to entities references.
    ///
    /// Returns the last element child or NULL if not available.
    #[doc(alias = "xmlLastElementChild")]
    #[cfg(feature = "tree")]
    pub unsafe fn last_element_child(&self) -> XmlNodePtr {
        let mut cur = match self.typ {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlDocumentNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlHTMLDocumentNode => self.last,
            _ => {
                return null_mut();
            }
        };
        while !cur.is_null() {
            if matches!((*cur).typ, XmlElementType::XmlElementNode) {
                return cur;
            }
            cur = (*cur).prev;
        }
        null_mut()
    }

    /// Finds the first closest previous sibling of the node which is an element node.
    ///
    /// Note the handling of entities references is different than in
    /// the W3C DOM element traversal spec since we don't have back reference
    /// from entities content to entities references.
    ///
    /// Returns the previous element sibling or null_mut() if not available
    #[doc(alias = "xmlPreviousElementSibling")]
    #[cfg(feature = "tree")]
    pub unsafe fn previous_element_sibling(&self) -> XmlNodePtr {
        let mut node = match self.typ {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlPINode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => self.prev,
            _ => {
                return null_mut();
            }
        };
        while !node.is_null() {
            if matches!((*node).typ, XmlElementType::XmlElementNode) {
                return node;
            }
            node = (*node).prev;
        }
        null_mut()
    }

    /// Search a Ns aliasing a given URI.
    /// Recurse on the parents until it finds the defined namespace or return NULL otherwise.
    ///
    /// Returns the namespace pointer or NULL.
    #[doc(alias = "xmlSearchNsByHref")]
    pub unsafe fn search_ns_by_href(
        &mut self,
        mut doc: XmlDocPtr,
        href: *const XmlChar,
    ) -> XmlNsPtr {
        let mut cur: XmlNsPtr;
        let orig: XmlNodePtr = self;

        if matches!(self.typ, XmlElementType::XmlNamespaceDecl) || href.is_null() {
            return null_mut();
        }
        if xml_str_equal(href, XML_XML_NAMESPACE.as_ptr() as _) {
            /*
             * Only the document can hold the XML spec namespace.
             */
            if doc.is_null() && matches!(self.typ, XmlElementType::XmlElementNode) {
                /*
                 * The XML-1.0 namespace is normally held on the root
                 * element. In this case exceptionally create it on the
                 * node element.
                 */
                cur = xml_malloc(size_of::<XmlNs>()) as _;
                if cur.is_null() {
                    xml_tree_err_memory(c"searching namespace".as_ptr() as _);
                    return null_mut();
                }
                memset(cur as _, 0, size_of::<XmlNs>());
                (*cur).typ = Some(XML_LOCAL_NAMESPACE);
                (*cur).href.store(
                    xml_strdup(XML_XML_NAMESPACE.as_ptr() as _),
                    Ordering::Relaxed,
                );
                (*cur)
                    .prefix
                    .store(xml_strdup(c"xml".as_ptr() as _), Ordering::Relaxed);
                (*cur).next.store(self.ns_def, Ordering::Relaxed);
                self.ns_def = cur;
                return cur;
            }
            if doc.is_null() {
                doc = self.doc;
                if doc.is_null() {
                    return null_mut();
                }
            }
            /*
            	* Return the XML namespace declaration held by the doc.
            	*/
            if (*doc).old_ns.is_null() {
                return (*doc).ensure_xmldecl();
            } else {
                return (*doc).old_ns;
            }
        }
        let is_attr = matches!(self.typ, XmlElementType::XmlAttributeNode);
        let mut node = self as *mut XmlNode;
        while !node.is_null() {
            if matches!(
                (*node).typ,
                XmlElementType::XmlEntityRefNode
                    | XmlElementType::XmlEntityNode
                    | XmlElementType::XmlEntityDecl
            ) {
                return null_mut();
            }
            if matches!((*node).typ, XmlElementType::XmlElementNode) {
                cur = (*node).ns_def;
                while !cur.is_null() {
                    if !(*cur).href.load(Ordering::Relaxed).is_null()
                        && !href.is_null()
                        && xml_str_equal((*cur).href.load(Ordering::Relaxed), href)
                        && (!is_attr || !(*cur).prefix.load(Ordering::Relaxed).is_null())
                        && xml_ns_in_scope(doc, orig, node, (*cur).prefix.load(Ordering::Relaxed))
                            == 1
                    {
                        return cur;
                    }
                    cur = (*cur).next.load(Ordering::Relaxed);
                }
                if orig != node {
                    cur = (*node).ns;
                    if !cur.is_null()
                        && !(*cur).href.load(Ordering::Relaxed).is_null()
                        && !href.is_null()
                        && xml_str_equal((*cur).href.load(Ordering::Relaxed), href)
                        && (!is_attr || !(*cur).prefix.load(Ordering::Relaxed).is_null())
                        && xml_ns_in_scope(doc, orig, node, (*cur).prefix.load(Ordering::Relaxed))
                            == 1
                    {
                        return cur;
                    }
                }
            }
            node = (*node).parent;
        }
        null_mut()
    }
}

impl NodeCommon for XmlNode {
    fn document(&self) -> *mut XmlDoc {
        self.doc
    }
    fn element_type(&self) -> XmlElementType {
        self.typ
    }
    fn name(&self) -> *const u8 {
        self.name
    }
    fn children(&self) -> *mut XmlNode {
        self.children
    }
    fn set_children(&mut self, children: *mut XmlNode) {
        self.children = children
    }
    fn last(&self) -> *mut XmlNode {
        self.last
    }
    fn set_last(&mut self, last: *mut XmlNode) {
        self.last = last;
    }
    fn next(&self) -> *mut XmlNode {
        self.next
    }
    fn set_next(&mut self, next: *mut XmlNode) {
        self.next = next;
    }
    fn prev(&self) -> *mut XmlNode {
        self.prev
    }
    fn set_prev(&mut self, prev: *mut XmlNode) {
        self.prev = prev;
    }
    fn parent(&self) -> *mut XmlNode {
        self.parent
    }
    fn set_parent(&mut self, parent: *mut XmlNode) {
        self.parent = parent;
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
unsafe fn add_prop_sibling(prev: XmlNodePtr, cur: XmlNodePtr, prop: XmlNodePtr) -> XmlNodePtr {
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
        xml_has_ns_prop((*cur).parent, (*prop).name, null_mut())
    } else {
        xml_has_ns_prop(
            (*cur).parent,
            (*prop).name,
            (*(*prop).ns).href.load(Ordering::Relaxed),
        )
    };

    if (*prop).doc != (*cur).doc {
        xml_set_tree_doc(prop, (*cur).doc);
    }
    (*prop).parent = (*cur).parent;
    (*prop).prev = prev;
    if !prev.is_null() {
        (*prop).next = (*prev).next;
        (*prev).next = prop;
        if !(*prop).next.is_null() {
            (*(*prop).next).prev = prop;
        }
    } else {
        (*prop).next = cur;
        (*cur).prev = prop;
    }
    if (*prop).prev.is_null() && !(*prop).parent.is_null() {
        (*(*prop).parent).properties = prop as _;
    }
    if !attr.is_null() && ((*attr).typ != XmlElementType::XmlAttributeDecl) {
        /* different instance, destroy it (attributes must be unique) */
        xml_remove_prop(attr);
    }
    prop
}
