use std::{borrow::Cow, ffi::CStr, os::raw::c_void, ptr::null_mut, sync::atomic::Ordering};

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
    pub(crate) children: Option<NodePtr>,       /* the value of the property */
    pub(crate) last: Option<NodePtr>,           /* NULL */
    pub(crate) parent: Option<NodePtr>,         /* child->parent link */
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
        let Some(mut parent) = self.parent else {
            return -1;
        };
        tmp = parent.properties;
        if tmp == self {
            parent.properties = self.next;
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
            if let Some(children) = self.children {
                if children.next.is_none()
                    && matches!(
                        children.typ,
                        XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
                    )
                {
                    /*
                     * Optimization for the common case: only 1 text node.
                     */
                    return xml_strdup(children.content);
                } else {
                    let ret: *mut XmlChar = children.get_string(self.doc, 1);
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
        self.parent
    }
    fn set_parent(&mut self, parent: Option<NodePtr>) {
        self.parent = parent;
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
        (*cur).children = (!doc.is_null())
            .then(|| NodePtr::from_ptr((*doc).get_node_list(value)))
            .flatten();
        (*cur).last = None;

        let mut tmp = (*cur).children;
        while let Some(mut now) = tmp {
            now.parent = NodePtr::from_ptr(cur as *mut XmlNode);
            if now.next.is_none() {
                (*cur).last = Some(now);
            }
            tmp = now.next;
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
#[cfg(any(feature = "libxml_tree", feature = "html", feature = "schema"))]
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
