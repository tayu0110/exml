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
    xml_new_prop_internal, xml_string_get_node_list, xml_tree_err_memory, NodeCommon,
    XmlAttributeType, XmlDoc, XmlDocPtr, XmlElementType, XmlNode, XmlNodePtr, XmlNs, XmlNsPtr,
    __XML_REGISTER_CALLBACKS,
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
        self.next as *mut XmlNode
    }
    fn set_next(&mut self, next: *mut XmlNode) {
        self.next = next as *mut XmlAttr;
    }
    fn prev(&self) -> *mut XmlNode {
        self.prev as *mut XmlNode
    }
    fn set_prev(&mut self, prev: *mut XmlNode) {
        self.prev = prev as *mut XmlAttr;
    }
    fn parent(&self) -> *mut XmlNode {
        self.parent
    }
    fn set_parent(&mut self, parent: *mut XmlNode) {
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
        (*cur).children = xml_string_get_node_list(doc, value);
        (*cur).last = null_mut();

        let mut tmp = (*cur).children;
        while !tmp.is_null() {
            (*tmp).parent = cur as _;
            if (*tmp).next.is_null() {
                (*cur).last = tmp;
            }
            tmp = (*tmp).next;
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
