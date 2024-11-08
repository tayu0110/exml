use std::{os::raw::c_void, ptr::null_mut};

use crate::libxml::xmlstring::XmlChar;

use super::{XmlAttr, XmlDoc, XmlElementType, XmlNs};

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
}
