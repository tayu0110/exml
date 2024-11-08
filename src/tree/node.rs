use std::{os::raw::c_void, ptr::null_mut, sync::atomic::Ordering};

use crate::{
    hash::{xml_hash_lookup, xml_hash_remove_entry},
    libxml::{
        globals::xml_free,
        xmlstring::{xml_strcat, xml_strdup, XmlChar},
    },
};

use super::{
    xml_free_node, xml_free_prop, xml_has_ns_prop, xml_is_blank_char, xml_node_add_content,
    xml_node_set_content, xml_remove_prop, xml_set_tree_doc, XmlAttr, XmlDoc, XmlDtd,
    XmlElementType, XmlNs,
};

pub trait NodeCommon {
    fn element_type(&self) -> XmlElementType;
    fn name(&self) -> *const u8;
    fn parent(&self) -> *mut XmlNode;
    fn set_parent(&mut self, parent: *mut XmlNode);
    fn next(&self) -> *mut XmlNode;
    fn set_next(&mut self, next: *mut XmlNode);
    fn prev(&self) -> *mut XmlNode;
    fn set_prev(&mut self, prev: *mut XmlNode);
    fn document(&self) -> *mut XmlDoc;

    /// Unlink a node from it's current context, the node is not freed.  
    /// If one need to free the node, use xmlFreeNode() routine after the unlink to discard it.  
    ///
    /// Note that namespace nodes can't be unlinked as they do not have pointer to their parent.
    #[doc(alias = "xmlUnlinkNode")]

    unsafe fn unlink(&mut self) {
        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return;
        }

        if matches!(self.element_type(), XmlElementType::XmlDtdNode) {
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
            XmlElementType::XmlTextNode | XmlElementType::XmlCdataSectionNode
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
                | XmlElementType::XmlPiNode
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
                    | XmlElementType::XmlPiNode
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
                XmlElementType::XmlDocumentNode | XmlElementType::XmlHtmlDocumentNode
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
                XmlElementType::XmlTextNode | XmlElementType::XmlCdataSectionNode
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
                        XmlElementType::XmlTextNode | XmlElementType::XmlCdataSectionNode
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
                            XmlElementType::XmlTextNode | XmlElementType::XmlCdataSectionNode
                        ) {
                            occur = 1;
                            break;
                        }
                        tmp = (*tmp).next;
                    }
                } else {
                    occur += 1;
                }
            } else if matches!((*cur).typ, XmlElementType::XmlPiNode) {
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
                    if matches!((*tmp).typ, XmlElementType::XmlPiNode)
                        && xml_str_equal((*cur).name, (*tmp).name)
                    {
                        occur += 1;
                    }
                    tmp = (*tmp).prev;
                }
                if occur == 0 {
                    tmp = (*cur).next;
                    while !tmp.is_null() && occur == 0 {
                        if matches!((*tmp).typ, XmlElementType::XmlPiNode)
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
            | XmlElementType::XmlCdataSectionNode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlHtmlDocumentNode
            | XmlElementType::XmlNamespaceDecl
            | XmlElementType::XmlXincludeStart
            | XmlElementType::XmlXincludeEnd => {
                return;
            }
            XmlElementType::XmlElementNode
            | XmlElementType::XmlAttributeNode
            | XmlElementType::XmlPiNode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlDtdNode
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
            xml_node_add_content(cur, (*elem).content);
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
                xml_node_add_content(self.prev, (*elem).content);
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
                xml_node_add_content(self, (*elem).content);
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
    pub unsafe fn add_child(&mut self, cur: XmlNodePtr) -> XmlNodePtr {
        let mut prev: XmlNodePtr;

        if matches!(self.typ, XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        if cur.is_null() || matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        if self as *mut XmlNode == cur {
            return null_mut();
        }
        /*
         * If cur is a TEXT node, merge its content with adjacent TEXT nodes
         * cur is then freed.
         */
        if matches!((*cur).typ, XmlElementType::XmlTextNode) {
            if matches!(self.typ, XmlElementType::XmlTextNode)
                && !self.content.is_null()
                && self.name == (*cur).name
            {
                xml_node_add_content(self, (*cur).content);
                xml_free_node(cur);
                return self;
            }
            if !self.last.is_null()
                && matches!((*self.last).typ, XmlElementType::XmlTextNode)
                && ((*self.last).name == (*cur).name)
                && (self.last != cur)
            {
                xml_node_add_content(self.last, (*cur).content);
                xml_free_node(cur);
                return self.last;
            }
        }

        /*
         * add the new element at the end of the children list.
         */
        prev = (*cur).parent;
        (*cur).parent = self as *mut XmlNode;
        if (*cur).doc != self.doc {
            xml_set_tree_doc(cur, self.doc);
        }
        /* this check prevents a loop on tree-traversions if a developer
         * tries to add a node to its parent multiple times
         */
        if prev == self as *mut XmlNode {
            return cur;
        }

        /*
         * Coalescing
         */
        if matches!(self.typ, XmlElementType::XmlTextNode)
            && !self.content.is_null()
            && self as *mut XmlNode != cur
        {
            xml_node_add_content(self, (*cur).content);
            xml_free_node(cur);
            return self as *mut XmlNode;
        }
        if matches!((*cur).typ, XmlElementType::XmlAttributeNode) {
            if !matches!(self.typ, XmlElementType::XmlElementNode) {
                return null_mut();
            }
            if !self.properties.is_null() {
                /* check if an attribute with the same name exists */

                let lastattr = if (*cur).ns.is_null() {
                    xml_has_ns_prop(self, (*cur).name, null_mut())
                } else {
                    xml_has_ns_prop(self, (*cur).name, (*(*cur).ns).href.load(Ordering::Relaxed))
                };
                if !lastattr.is_null()
                    && lastattr != cur as _
                    && !matches!((*lastattr).typ, XmlElementType::XmlAttributeDecl)
                {
                    /* different instance, destroy it (attributes must be unique) */
                    (*(lastattr as *mut XmlNode)).unlink();
                    xml_free_prop(lastattr);
                }
                if lastattr == cur as _ {
                    return cur;
                }
            }
            if self.properties.is_null() {
                self.properties = cur as _;
            } else {
                /* find the end */
                let mut lastattr = self.properties;
                while !(*lastattr).next.is_null() {
                    lastattr = (*lastattr).next;
                }
                (*lastattr).next = cur as _;
                (*(cur as *mut XmlAttr)).prev = lastattr;
            }
        } else if self.children.is_null() {
            self.children = cur;
            self.last = cur;
        } else {
            prev = self.last;
            (*prev).next = cur;
            (*cur).prev = prev;
            self.last = cur;
        }
        cur
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
                xml_node_add_content(self.last, (*cur).content);
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
