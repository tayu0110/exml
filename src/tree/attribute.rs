use std::{os::raw::c_void, ptr::null_mut, sync::atomic::Ordering};

use libc::memset;

use crate::{
    dict::xml_dict_lookup,
    libxml::{
        globals::{xml_malloc, xml_register_node_default_value},
        xmlstring::{xml_strdup, XmlChar},
    },
};

use super::{
    xml_free_prop, xml_new_prop_internal, xml_tree_err_memory, NodeCommon, NodePtr,
    XmlAttributePtr, XmlAttributeType, XmlDoc, XmlDocPtr, XmlElementType, XmlNode, XmlNodePtr,
    XmlNs, XmlNsPtr, __XML_REGISTER_CALLBACKS,
};

/// An attribute on an XML node.
pub type XmlAttrPtr = *mut XmlAttr;
#[repr(C)]
pub struct XmlAttr {
    pub(crate) _private: *mut c_void,           /* application data */
    pub(crate) typ: XmlElementType,             /* XML_ATTRIBUTE_NODE, must be second ! */
    pub(crate) name: *const XmlChar,            /* the name of the property */
    pub(crate) children: *mut XmlNode,          /* the value of the property */
    pub(crate) last: *mut XmlNode,              /* NULL */
    pub(crate) parent: *mut XmlNode,            /* child->parent link */
    pub(crate) next: *mut XmlAttr,              /* next sibling link  */
    pub(crate) prev: *mut XmlAttr,              /* previous sibling link  */
    pub(crate) doc: *mut XmlDoc,                /* the containing document */
    pub(crate) ns: *mut XmlNs,                  /* pointer to the associated namespace */
    pub(crate) atype: Option<XmlAttributeType>, /* the attribute type if validating */
    pub(crate) psvi: *mut c_void,               /* for type/PSVI information */
}

impl XmlAttr {
    /// Unlink and free one attribute, all the content is freed too.
    ///
    /// Note this doesn't work for namespace definition attributes.
    ///
    /// Returns 0 if success and -1 in case of error.
    #[doc(alias = "xmlRemoveProp")]
    pub unsafe fn remove_prop(&mut self) -> i32 {
        let mut tmp: XmlAttrPtr;
        if self.parent.is_null() {
            return -1;
        }
        tmp = (*self.parent).properties;
        if tmp == self {
            (*self.parent).properties = self.next;
            if !self.next.is_null() {
                (*self.next).prev = null_mut();
            }
            xml_free_prop(self);
            return 0;
        }
        while !tmp.is_null() {
            if (*tmp).next == self {
                (*tmp).next = self.next;
                if !(*tmp).next.is_null() {
                    (*(*tmp).next).prev = tmp;
                }
                xml_free_prop(self);
                return 0;
            }
            tmp = (*tmp).next;
        }
        -1
    }

    pub(super) unsafe fn get_prop_node_value_internal(&self) -> *mut XmlChar {
        if matches!(self.typ, XmlElementType::XmlAttributeNode) {
            /*
             * Note that we return at least the empty string.
             *   TODO: Do we really always want that?
             */
            if !self.children.is_null() {
                if (*self.children).next.is_none()
                    && matches!(
                        (*self.children).typ,
                        XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
                    )
                {
                    /*
                     * Optimization for the common case: only 1 text node.
                     */
                    return xml_strdup((*self.children).content);
                } else {
                    let ret: *mut XmlChar = (*self.children).get_string(self.doc, 1);
                    if !ret.is_null() {
                        return ret;
                    }
                }
            }
            return xml_strdup(c"".as_ptr() as _);
        } else if matches!(self.typ, XmlElementType::XmlAttributeDecl) {
            return xml_strdup((*(self as *const XmlAttr as XmlAttributePtr)).default_value);
        }
        null_mut()
    }
}

impl NodeCommon for XmlAttr {
    fn document(&self) -> *mut XmlDoc {
        self.doc
    }
    fn element_type(&self) -> XmlElementType {
        self.typ
    }
    fn name(&self) -> *const u8 {
        self.name
    }
    fn children(&self) -> Option<NodePtr> {
        NodePtr::from_ptr(self.children)
    }
    fn set_children(&mut self, children: Option<NodePtr>) {
        self.children = children.map_or(null_mut(), |c| c.as_ptr())
    }
    fn last(&self) -> Option<NodePtr> {
        NodePtr::from_ptr(self.last)
    }
    fn set_last(&mut self, last: Option<NodePtr>) {
        self.last = last.map_or(null_mut(), |l| l.as_ptr());
    }
    fn next(&self) -> Option<NodePtr> {
        NodePtr::from_ptr(self.next as *mut XmlNode)
    }
    fn set_next(&mut self, next: Option<NodePtr>) {
        self.next = next.map_or(null_mut(), |n| n.as_ptr()) as *mut XmlAttr;
    }
    fn prev(&self) -> Option<NodePtr> {
        NodePtr::from_ptr(self.prev as *mut XmlNode)
    }
    fn set_prev(&mut self, prev: Option<NodePtr>) {
        self.prev = prev.map_or(null_mut(), |p| p.as_ptr()) as *mut XmlAttr;
    }
    fn parent(&self) -> Option<NodePtr> {
        NodePtr::from_ptr(self.parent)
    }
    fn set_parent(&mut self, parent: Option<NodePtr>) {
        self.parent = parent.map_or(null_mut(), |p| p.as_ptr());
    }
}

/// Create a new property carried by a document.  
/// Returns a pointer to the attribute
///
/// # NOTE
/// `value` is supposed to be a piece of XML CDATA, so it allows entity references,
/// but XML special chars need to be escaped first by using.  
/// xmlEncodeEntitiesReentrant(). Use xmlNewProp() if you don't need entities support.
#[doc(alias = "xmlNewDocProp")]
pub unsafe extern "C" fn xml_new_doc_prop(
    doc: XmlDocPtr,
    name: *const XmlChar,
    value: *const XmlChar,
) -> XmlAttrPtr {
    if name.is_null() {
        return null_mut();
    }

    /*
     * Allocate a new property and fill the fields.
     */
    let cur: XmlAttrPtr = xml_malloc(size_of::<XmlAttr>()) as _;
    if cur.is_null() {
        xml_tree_err_memory(c"building attribute".as_ptr() as _);
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlAttr>());
    (*cur).typ = XmlElementType::XmlAttributeNode;

    if !doc.is_null() && !(*doc).dict.is_null() {
        (*cur).name = xml_dict_lookup((*doc).dict, name, -1);
    } else {
        (*cur).name = xml_strdup(name);
    }
    (*cur).doc = doc;
    if !value.is_null() {
        (*cur).children = if doc.is_null() {
            null_mut()
        } else {
            (*doc).get_node_list(value)
        };
        (*cur).last = null_mut();

        let mut tmp = (*cur).children;
        while !tmp.is_null() {
            (*tmp).parent = NodePtr::from_ptr(cur as *mut XmlNode);
            if (*tmp).next.is_none() {
                (*cur).last = tmp;
            }
            tmp = (*tmp).next.map_or(null_mut(), |c| c.as_ptr());
        }
    }

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    //  && xmlRegisterNodeDefaultValue.is_some()
    {
        xml_register_node_default_value(cur as _);
    }
    cur
}

/// Create a new property carried by a node.  
/// Returns a pointer to the attribute
#[doc(alias = "xmlNewProp")]
#[cfg(any(feature = "tree", feature = "html", feature = "schema"))]
pub unsafe fn xml_new_prop(
    node: XmlNodePtr,
    name: *const XmlChar,
    value: *const XmlChar,
) -> XmlAttrPtr {
    if name.is_null() {
        return null_mut();
    }

    xml_new_prop_internal(node, null_mut(), name, value, 0)
}

/// Create a new property tagged with a namespace and carried by a node.  
/// Returns a pointer to the attribute
#[doc(alias = "xmlNewNsProp")]
pub unsafe fn xml_new_ns_prop(
    node: XmlNodePtr,
    ns: XmlNsPtr,
    name: *const XmlChar,
    value: *const XmlChar,
) -> XmlAttrPtr {
    if name.is_null() {
        return null_mut();
    }

    xml_new_prop_internal(node, ns, name, value, 0)
}

/// Create a new property tagged with a namespace and carried by a node.  
/// Returns a pointer to the attribute
#[doc(alias = "xmlNewNsPropEatName")]
pub unsafe extern "C" fn xml_new_ns_prop_eat_name(
    node: XmlNodePtr,
    ns: XmlNsPtr,
    name: *mut XmlChar,
    value: *const XmlChar,
) -> XmlAttrPtr {
    if name.is_null() {
        return null_mut();
    }

    xml_new_prop_internal(node, ns, name, value, 1)
}
