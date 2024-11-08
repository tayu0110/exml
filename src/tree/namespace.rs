use std::{
    os::raw::c_void,
    ptr::null_mut,
    sync::atomic::{AtomicPtr, Ordering},
};

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
    pub next: AtomicPtr<XmlNs>,             /* next Ns link for this node  */
    pub(crate) typ: Option<XmlNsType>,      /* global or local */
    pub href: AtomicPtr<XmlChar>,           /* URL for the namespace */
    pub prefix: AtomicPtr<XmlChar>,         /* prefix for the namespace */
    pub(crate) _private: AtomicPtr<c_void>, /* application data */
    pub(crate) context: AtomicPtr<XmlDoc>,  /* normally an xmlDoc */
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
    (*cur).typ = Some(XML_LOCAL_NAMESPACE);

    if !href.is_null() {
        (*cur).href = AtomicPtr::new(xml_strdup(href));
    }
    if !prefix.is_null() {
        (*cur).prefix = AtomicPtr::new(xml_strdup(prefix));
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

            if ((*prev).prefix.load(Ordering::Relaxed).is_null()
                && (*cur).prefix.load(Ordering::Relaxed).is_null())
                || xml_str_equal(
                    (*prev).prefix.load(Ordering::Relaxed),
                    (*cur).prefix.load(Ordering::Relaxed),
                )
            {
                xml_free_ns(cur);
                return null_mut();
            }
            while !(*prev).next.load(Ordering::Relaxed).is_null() {
                prev = (*prev).next.load(Ordering::Relaxed);
                if ((*prev).prefix.load(Ordering::Relaxed).is_null()
                    && (*cur).prefix.load(Ordering::Relaxed).is_null())
                    || xml_str_equal(
                        (*prev).prefix.load(Ordering::Relaxed),
                        (*cur).prefix.load(Ordering::Relaxed),
                    )
                {
                    xml_free_ns(cur);
                    return null_mut();
                }
            }
            (*prev).next = AtomicPtr::new(cur);
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
    if !(*cur).href.load(Ordering::Relaxed).is_null() {
        xml_free((*cur).href.load(Ordering::Relaxed) as _);
    }
    if !(*cur).prefix.load(Ordering::Relaxed).is_null() {
        xml_free((*cur).prefix.load(Ordering::Relaxed) as _);
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
        next = (*cur).next.load(Ordering::Relaxed);
        xml_free_ns(cur);
        cur = next;
    }
}
