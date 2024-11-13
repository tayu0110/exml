use std::{os::raw::c_void, ptr::null_mut, sync::atomic::AtomicPtr};

use libc::memset;

use crate::libxml::{
    globals::{xml_free, xml_malloc},
    xmlstring::{xml_str_equal, xml_strdup, XmlChar},
};

use super::{
    xml_tree_err_memory, XmlDoc, XmlElementType, XmlNodePtr, XmlNsType, XML_LOCAL_NAMESPACE,
    XML_XML_NAMESPACE,
};

/// An XML namespace.
/// Note that prefix == NULL is valid, it defines the default namespace
/// within the subtree (until overridden).
///
/// xmlNsType is unified with xmlElementType.
pub type XmlNsPtr = *mut XmlNs;
#[repr(C)]
pub struct XmlNs {
    pub next: *mut XmlNs,                   /* next Ns link for this node  */
    pub(crate) typ: XmlNsType,              /* global or local */
    pub href: *const XmlChar,               /* URL for the namespace */
    pub prefix: *const XmlChar,             /* prefix for the namespace */
    pub(crate) _private: AtomicPtr<c_void>, /* application data */
    pub(crate) context: *mut XmlDoc,        /* normally an xmlDoc */
}

impl Default for XmlNs {
    fn default() -> Self {
        Self {
            next: null_mut(),
            typ: XmlNsType::XmlInvalidNode,
            href: null_mut(),
            prefix: null_mut(),
            _private: AtomicPtr::new(null_mut()),
            context: null_mut(),
        }
    }
}

/// Creation of a new Namespace. This function will refuse to create
/// a namespace with a similar prefix than an existing one present on this node.
/// Note that for a default namespace, `prefix` should be NULL.
///
/// We use href == NULL in the case of an element creation where the namespace was not defined.
///
/// Returns a new namespace pointer or NULL
#[doc(alias = "xmlNewNs")]
pub unsafe fn xml_new_ns(
    node: XmlNodePtr,
    href: *const XmlChar,
    prefix: *const XmlChar,
) -> XmlNsPtr {
    if !node.is_null() && !matches!((*node).typ, XmlElementType::XmlElementNode) {
        return null_mut();
    }

    if !prefix.is_null() && xml_str_equal(prefix, c"xml".as_ptr() as _) {
        /* xml namespace is predefined, no need to add it */
        if xml_str_equal(href, XML_XML_NAMESPACE.as_ptr() as _) {
            return null_mut();
        }

        /*
         * Problem, this is an attempt to bind xml prefix to a wrong
         * namespace, which breaks
         * Namespace constraint: Reserved Prefixes and Namespace Names
         * from XML namespace. But documents authors may not care in
         * their context so let's proceed.
         */
    }

    /*
     * Allocate a new Namespace and fill the fields.
     */
    let cur: XmlNsPtr = xml_malloc(size_of::<XmlNs>()) as _;
    if cur.is_null() {
        xml_tree_err_memory(c"building namespace".as_ptr() as _);
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlNs>());
    (*cur).typ = XML_LOCAL_NAMESPACE;

    if !href.is_null() {
        (*cur).href = xml_strdup(href);
    }
    if !prefix.is_null() {
        (*cur).prefix = xml_strdup(prefix);
    }

    /*
     * Add it at the end to preserve parsing order ...
     * and checks for existing use of the prefix
     */
    if !node.is_null() {
        if (*node).ns_def.is_null() {
            (*node).ns_def = cur;
        } else {
            let mut prev: XmlNsPtr = (*node).ns_def;

            if ((*prev).prefix.is_null() && (*cur).prefix.is_null())
                || xml_str_equal((*prev).prefix, (*cur).prefix)
            {
                xml_free_ns(cur);
                return null_mut();
            }
            while !(*prev).next.is_null() {
                prev = (*prev).next;
                if ((*prev).prefix.is_null() && (*cur).prefix.is_null())
                    || xml_str_equal((*prev).prefix, (*cur).prefix)
                {
                    xml_free_ns(cur);
                    return null_mut();
                }
            }
            (*prev).next = cur;
        }
    }
    cur
}

/// Free up the structures associated to a namespace
#[doc(alias = "xmlFreeNs")]
pub unsafe extern "C" fn xml_free_ns(cur: XmlNsPtr) {
    if cur.is_null() {
        return;
    }
    if !(*cur).href.is_null() {
        xml_free((*cur).href as _);
    }
    if !(*cur).prefix.is_null() {
        xml_free((*cur).prefix as _);
    }
    xml_free(cur as _);
}

/// Free up all the structures associated to the chained namespaces.
#[doc(alias = "xmlFreeNsList")]
pub unsafe fn xml_free_ns_list(mut cur: XmlNsPtr) {
    let mut next: XmlNsPtr;
    if cur.is_null() {
        return;
    }
    while !cur.is_null() {
        next = (*cur).next;
        xml_free_ns(cur);
        cur = next;
    }
}
