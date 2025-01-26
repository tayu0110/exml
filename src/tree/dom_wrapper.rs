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

use std::{ffi::CStr, os::raw::c_void, ptr::null_mut, sync::atomic::Ordering};

use libc::snprintf;

use crate::libxml::{
    globals::{xml_free, xml_malloc, xml_realloc},
    parser_internals::{XML_STRING_COMMENT, XML_STRING_TEXT, XML_STRING_TEXT_NOENC},
    valid::{xml_add_id, xml_is_id, xml_remove_id},
    xmlstring::{xml_str_equal, xml_strdup, XmlChar},
};

use super::{
    xml_free_ns, xml_get_doc_entity, xml_new_ns, xml_search_ns_by_namespace_strict,
    xml_search_ns_by_prefix_strict, xml_tree_err_memory, xml_tree_nslist_lookup_by_prefix,
    NodeCommon, NodePtr, XmlAttr, XmlAttributeType, XmlDoc, XmlElementType, XmlNode, XmlNs,
    XmlNsPtr, XML_LOCAL_NAMESPACE,
};

/// A function called to acquire namespaces (xmlNs) from the wrapper.
///
/// Returns an xmlNsPtr or NULL in case of an error.
#[doc(alias = "xmlDOMWrapAcquireNsFunction")]
pub type XmlDOMWrapAcquireNsFunction = unsafe fn(
    ctxt: XmlDOMWrapCtxtPtr,
    node: *mut XmlNode,
    ns_name: *const XmlChar,
    ns_prefix: *const XmlChar,
) -> *mut XmlNs;

/// Context for DOM wrapper-operations.
pub type XmlDOMWrapCtxtPtr = *mut XmlDOMWrapCtxt;
#[repr(C)]
pub struct XmlDOMWrapCtxt {
    pub(super) _private: *mut c_void,
    /// The type of this context, just in case we need specialized
    /// contexts in the future.
    pub(super) typ: i32,
    /// Internal namespace map used for various operations.
    pub(super) namespace_map: *mut c_void,
    /// Use this one to acquire an xmlNsPtr intended for node->ns.  
    /// (Note that this is not intended for elem->nsDef).
    pub(super) get_ns_for_node_func: Option<XmlDOMWrapAcquireNsFunction>,
}

impl Default for XmlDOMWrapCtxt {
    fn default() -> Self {
        Self {
            _private: null_mut(),
            typ: 0,
            namespace_map: null_mut(),
            get_ns_for_node_func: None,
        }
    }
}

pub type XmlNsMapItemPtr = *mut XmlNsMapItem;
#[repr(C)]
pub struct XmlNsMapItem {
    next: XmlNsMapItemPtr,
    prev: XmlNsMapItemPtr,
    old_ns: Option<XmlNsPtr>, /* old ns decl reference */
    new_ns: *mut XmlNs,       /* new ns decl reference */
    shadow_depth: i32,        /* Shadowed at this depth */
    /// depth:
    /// `>= 0` == @node's ns-decls
    /// `-1`   == @parent's ns-decls
    /// `-2`   == the (*doc).oldNs XML ns-decl
    /// `-3`   == the (*doc).oldNs storage ns-decls
    /// `-4`   == ns-decls provided via custom ns-handling
    depth: i32,
}

impl Default for XmlNsMapItem {
    fn default() -> Self {
        Self {
            next: null_mut(),
            prev: null_mut(),
            old_ns: None,
            new_ns: null_mut(),
            shadow_depth: 0,
            depth: 0,
        }
    }
}

pub type XmlNsMapPtr = *mut XmlNsMap;
#[repr(C)]
pub struct XmlNsMap {
    first: XmlNsMapItemPtr,
    last: XmlNsMapItemPtr,
    pool: XmlNsMapItemPtr,
}

impl Default for XmlNsMap {
    fn default() -> Self {
        Self {
            first: null_mut(),
            last: null_mut(),
            pool: null_mut(),
        }
    }
}

/// Allocates and initializes a new DOM-wrapper context.
///
/// Returns the xmlDOMWrapCtxtPtr or null_mut() in case of an internal error.
#[doc(alias = "xmlDOMWrapNewCtxt")]
pub unsafe fn xml_dom_wrap_new_ctxt() -> XmlDOMWrapCtxtPtr {
    let ret: XmlDOMWrapCtxtPtr = xml_malloc(size_of::<XmlDOMWrapCtxt>()) as _;
    if ret.is_null() {
        xml_tree_err_memory("allocating DOM-wrapper context");
        return null_mut();
    }
    std::ptr::write(&mut *ret, XmlDOMWrapCtxt::default());
    ret
}

/// Frees the ns-map
#[doc(alias = "xmlDOMWrapNsMapFree")]
unsafe fn xml_dom_wrap_ns_map_free(nsmap: XmlNsMapPtr) {
    let mut cur: XmlNsMapItemPtr;
    let mut tmp: XmlNsMapItemPtr;

    if nsmap.is_null() {
        return;
    }
    cur = (*nsmap).pool;
    while !cur.is_null() {
        tmp = cur;
        cur = (*cur).next;
        xml_free(tmp as _);
    }
    cur = (*nsmap).first;
    while !cur.is_null() {
        tmp = cur;
        cur = (*cur).next;
        xml_free(tmp as _);
    }
    xml_free(nsmap as _);
}

/// Frees the DOM-wrapper context.
#[doc(alias = "xmlDOMWrapFreeCtxt")]
pub unsafe fn xml_dom_wrap_free_ctxt(ctxt: XmlDOMWrapCtxtPtr) {
    if ctxt.is_null() {
        return;
    }
    if !(*ctxt).namespace_map.is_null() {
        xml_dom_wrap_ns_map_free((*ctxt).namespace_map as _);
    }
    // TODO: Store the namespace map in the context.
    xml_free(ctxt as _);
}

macro_rules! XML_NSMAP_NOTEMPTY {
    ($m:expr) => {
        !$m.is_null() && !(*$m).first.is_null()
    };
}

macro_rules! XML_NSMAP_FOREACH {
    ($m:expr, $i:expr, $b:block) => {
        let mut __initialized = false;
        $i = (*$m).first;
        while {
            if !__initialized {
                __initialized = true;
            } else {
                $i = (*$i).next;
            }
            !$i.is_null()
        } {
            $b;
        }
    };
}

macro_rules! XML_NSMAP_POP {
    ($m:expr, $i:expr) => {
        $i = (*$m).last;
        (*$m).last = (*$i).prev;
        if (*$m).last.is_null() {
            (*$m).first = null_mut();
        } else {
            (*(*$m).last).next = null_mut();
        }
        (*$i).next = (*$m).pool;
        (*$m).pool = $i;
    };
}

#[repr(C)]
enum XmlDomreconcileNsoptions {
    XmlDomReconnsRemoveredund = 1 << 0,
}

const XML_TREE_NSMAP_PARENT: i32 = -1;
const XML_TREE_NSMAP_XML: i32 = -2;
const XML_TREE_NSMAP_DOC: i32 = -3;
const XML_TREE_NSMAP_CUSTOM: i32 = -4;

/// Adds an ns-mapping item.
#[doc(alias = "xmlDOMWrapNsMapAddItem")]
unsafe fn xml_dom_wrap_ns_map_add_item(
    nsmap: *mut XmlNsMapPtr,
    position: i32,
    old_ns: Option<XmlNsPtr>,
    new_ns: *mut XmlNs,
    depth: i32,
) -> XmlNsMapItemPtr {
    let ret: XmlNsMapItemPtr;
    let mut map: XmlNsMapPtr;

    if nsmap.is_null() {
        return null_mut();
    }
    if position != -1 && position != 0 {
        return null_mut();
    }
    map = *nsmap;

    if map.is_null() {
        // Create the ns-map.
        map = xml_malloc(size_of::<XmlNsMap>()) as _;
        if map.is_null() {
            xml_tree_err_memory("allocating namespace map");
            return null_mut();
        }
        std::ptr::write(&mut *map, XmlNsMap::default());
        *nsmap = map;
    }

    if !(*map).pool.is_null() {
        // Reuse an item from the pool.
        ret = (*map).pool;
        (*map).pool = (*ret).next;
        std::ptr::write(&mut *ret, XmlNsMapItem::default());
    } else {
        // Create a new item.
        ret = xml_malloc(size_of::<XmlNsMapItem>()) as _;
        if ret.is_null() {
            xml_tree_err_memory("allocating namespace map item");
            return null_mut();
        }
        std::ptr::write(&mut *ret, XmlNsMapItem::default());
    }

    if (*map).first.is_null() {
        // First ever.
        (*map).first = ret;
        (*map).last = ret;
    } else if position == -1 {
        // Append.
        (*ret).prev = (*map).last;
        (*(*map).last).next = ret;
        (*map).last = ret;
    } else if position == 0 {
        // Set on first position.
        (*(*map).first).prev = ret;
        (*ret).next = (*map).first;
        (*map).first = ret;
    }

    (*ret).old_ns = old_ns;
    (*ret).new_ns = new_ns;
    (*ret).shadow_depth = -1;
    (*ret).depth = depth;
    ret
}

/// Puts in-scope namespaces into the ns-map.
///
/// Returns 0 on success, -1 on API or internal errors.
#[doc(alias = "xmlDOMWrapNSNormGatherInScopeNs")]
unsafe fn xml_dom_wrap_ns_norm_gather_in_scope_ns(
    map: *mut XmlNsMapPtr,
    node: *mut XmlNode,
) -> i32 {
    let mut cur: *mut XmlNode;
    let mut mi: XmlNsMapItemPtr;
    let mut shadowed: i32;

    if map.is_null() || !(*map).is_null() {
        return -1;
    }
    if node.is_null() || matches!((*node).element_type(), XmlElementType::XmlNamespaceDecl) {
        return -1;
    }
    // Get in-scope ns-decls of @parent.
    cur = node;
    while !cur.is_null() && cur != (*cur).doc as _ {
        if matches!((*cur).element_type(), XmlElementType::XmlElementNode)
            && !(*cur).ns_def.is_null()
        {
            let mut ns = XmlNsPtr::from_raw((*cur).ns_def).unwrap();
            while let Some(now) = ns {
                shadowed = 0;
                if XML_NSMAP_NOTEMPTY!(*map) {
                    // Skip shadowed prefixes.
                    XML_NSMAP_FOREACH!(*map, mi, {
                        if now.prefix() == (*(*mi).new_ns).prefix() {
                            shadowed = 1;
                            break;
                        }
                    });
                }
                // Insert mapping.
                mi =
                    xml_dom_wrap_ns_map_add_item(map, 0, None, now.as_ptr(), XML_TREE_NSMAP_PARENT);
                if mi.is_null() {
                    return -1;
                }
                if shadowed != 0 {
                    (*mi).shadow_depth = 0;
                }
                ns = XmlNsPtr::from_raw(now.next).unwrap();
            }
        }
        cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
    }
    0
}

/// For internal use. Adds a ns-decl mapping.
///
/// Returns 0 on success, -1 on internal errors.
#[doc(alias = "xmlDOMWrapNSNormAddNsMapItem2")]
unsafe fn xml_dom_wrap_ns_norm_add_ns_map_item2(
    list: &mut Vec<*mut XmlNs>,
    // size: *mut i32,
    number: *mut i32,
    old_ns: *mut XmlNs,
    new_ns: *mut XmlNs,
) -> i32 {
    list.push(old_ns);
    list.push(new_ns);
    (*number) += 1;
    0
}

/// Creates or reuses an xmlNs struct on (*doc).oldNs with
/// the given prefix and namespace name.
///
/// Returns the acquired ns struct or null_mut() in case of an API or internal error.
#[doc(alias = "xmlDOMWrapStoreNs")]
unsafe fn xml_dom_wrap_store_ns(
    doc: *mut XmlDoc,
    ns_name: *const XmlChar,
    prefix: Option<&str>,
) -> *mut XmlNs {
    if doc.is_null() {
        return null_mut();
    }
    let Some(mut ns) = (*doc).ensure_xmldecl() else {
        return null_mut();
    };
    if let Some(next) = XmlNsPtr::from_raw(ns.next).unwrap() {
        if next.prefix().as_deref() == prefix && xml_str_equal(next.href, ns_name) {
            return next.as_ptr();
        }
        ns = next;
        while let Some(next) = XmlNsPtr::from_raw(ns.next).unwrap() {
            if next.prefix().as_deref() == prefix && xml_str_equal(next.href, ns_name) {
                return next.as_ptr();
            }
            ns = next;
        }
    }
    // Create.
    ns.next = xml_new_ns(null_mut(), ns_name, prefix);
    ns.next
}

/// Declares a new namespace on @elem. It tries to use the
/// given @prefix; if a ns-decl with the given prefix is already existent
/// on @elem, it will generate an other prefix.
///
/// Returns 1 if a ns-decl was found, 0 if not and -1 on API and internal errors.
#[doc(alias = "xmlDOMWrapNSNormDeclareNsForced")]
unsafe fn xml_dom_wrap_nsnorm_declare_ns_forced(
    doc: *mut XmlDoc,
    elem: *mut XmlNode,
    ns_name: *const XmlChar,
    prefix: *const XmlChar,
    check_shadow: i32,
) -> *mut XmlNs {
    let ret: *mut XmlNs;
    let mut buf: [i8; 50] = [0; 50];
    let mut pref: *const XmlChar;
    let mut counter: i32 = 0;

    if doc.is_null()
        || elem.is_null()
        || !matches!((*elem).element_type(), XmlElementType::XmlElementNode)
    {
        return null_mut();
    }
    // Create a ns-decl on @anchor.
    pref = prefix;
    loop {
        // Lookup whether the prefix is unused in elem's ns-decls.
        if !(*elem).ns_def.is_null()
            && xml_tree_nslist_lookup_by_prefix(XmlNsPtr::from_raw((*elem).ns_def).unwrap(), pref)
                .is_some()
        {
            // goto ns_next_prefix;
        } else {
            // Does it shadow ancestor ns-decls?
            if check_shadow != 0
                && (*elem)
                    .parent()
                    .filter(|p| {
                        p.doc != p.as_ptr() as *mut XmlDoc
                            && xml_search_ns_by_prefix_strict(doc, p.as_ptr(), pref, None) == 1
                    })
                    .is_some()
            {
                // goto ns_next_prefix;
            } else {
                ret = xml_new_ns(
                    null_mut(),
                    ns_name,
                    (!pref.is_null())
                        .then(|| CStr::from_ptr(pref as *const i8).to_string_lossy())
                        .as_deref(),
                );
                if ret.is_null() {
                    return null_mut();
                }
                if (*elem).ns_def.is_null() {
                    (*elem).ns_def = ret;
                } else {
                    let mut ns2: *mut XmlNs = (*elem).ns_def;
                    while !(*ns2).next.is_null() {
                        ns2 = (*ns2).next;
                    }
                    (*ns2).next = ret;
                }
                return ret;
            }
        }
        // ns_next_prefix:
        counter += 1;
        if counter > 1000 {
            return null_mut();
        }
        if prefix.is_null() {
            snprintf(
                buf.as_mut_ptr() as _,
                buf.len(),
                c"ns_%d".as_ptr() as _,
                counter,
            );
        } else {
            snprintf(
                buf.as_mut_ptr() as _,
                buf.len(),
                c"%.30s_%d".as_ptr() as _,
                prefix,
                counter,
            );
        }
        pref = buf.as_ptr() as _;
    }
}

/// Searches for a matching ns-name in the ns-decls of @nsMap, if not
/// found it will either declare it on @elem, or store it in (*doc).oldNs.
/// If a new ns-decl needs to be declared on @elem, it tries to use the
/// @(*ns).prefix for it, if this prefix is already in use on @elem, it will
/// change the prefix or the new ns-decl.
///
/// Returns 0 if succeeded, -1 otherwise and on API/internal errors.
#[allow(clippy::too_many_arguments)]
#[doc(alias = "xmlDOMWrapNSNormAcquireNormalizedNs")]
unsafe fn xml_dom_wrap_ns_norm_acquire_normalized_ns(
    doc: *mut XmlDoc,
    elem: *mut XmlNode,
    ns: *mut XmlNs,
    ret_ns: &mut *mut XmlNs,
    ns_map: *mut XmlNsMapPtr,
    depth: i32,
    ancestors_only: i32,
    prefixed: i32,
) -> i32 {
    let mut mi: XmlNsMapItemPtr;

    if doc.is_null() || ns.is_null() || ret_ns.is_null() || ns_map.is_null() {
        return -1;
    }

    *ret_ns = null_mut();
    // Handle XML namespace.
    if (*ns).prefix().as_deref() == Some("xml") {
        // Insert XML namespace mapping.
        *ret_ns = (*doc).ensure_xmldecl().map_or(null_mut(), |ns| ns.as_ptr());
        if (*ret_ns).is_null() {
            return -1;
        }
        return 0;
    }
    // If the search should be done in ancestors only and no
    // @elem (the first ancestor) was specified, then skip the search.
    if XML_NSMAP_NOTEMPTY!(*ns_map) && !(ancestors_only != 0 && elem.is_null()) {
        // Try to find an equal ns-name in in-scope ns-decls.
        XML_NSMAP_FOREACH!(*ns_map, mi, {
            if ((*mi).depth >= XML_TREE_NSMAP_PARENT) &&
		        // ancestorsOnly: This should be turned on to gain speed,
                // if one knows that the branch itself was already
                // ns-wellformed and no stale references existed.
                // I.e. it searches in the ancestor axis only.
		        (ancestors_only == 0 || (*mi).depth == XML_TREE_NSMAP_PARENT) &&
		        // Skip shadowed prefixes.
		        (*mi).shadow_depth == -1 &&
		        // Skip xmlns="" or xmlns:foo="".
		        (!(*(*mi).new_ns).href.is_null() &&
                *(*(*mi).new_ns).href.add(0) != 0) &&
		        // Ensure a prefix if wanted.
		        (prefixed == 0 || (*(*mi).new_ns).prefix().is_some()) &&
		        // Equal ns name
		        ((*(*mi).new_ns).href == (*ns).href ||
                xml_str_equal((*(*mi).new_ns).href, (*ns).href) )
            {
                // Set the mapping.
                (*mi).old_ns = XmlNsPtr::from_raw(ns).unwrap();
                *ret_ns = (*mi).new_ns;
                return 0;
            }
        });
    }
    // No luck, the namespace is out of scope or shadowed.
    if elem.is_null() {
        // Store ns-decls in "oldNs" of the document-node.
        let tmpns: *mut XmlNs = xml_dom_wrap_store_ns(doc, (*ns).href, (*ns).prefix().as_deref());
        if tmpns.is_null() {
            return -1;
        }
        // Insert mapping.
        if xml_dom_wrap_ns_map_add_item(
            ns_map,
            -1,
            XmlNsPtr::from_raw(ns).unwrap(),
            tmpns,
            XML_TREE_NSMAP_DOC,
        )
        .is_null()
        {
            xml_free_ns(XmlNsPtr::from_raw(tmpns).unwrap().unwrap());
            return -1;
        }
        *ret_ns = tmpns;
    } else {
        let tmpns: *mut XmlNs =
            xml_dom_wrap_nsnorm_declare_ns_forced(doc, elem, (*ns).href, (*ns).prefix, 0);
        if tmpns.is_null() {
            return -1;
        }

        if !(*ns_map).is_null() {
            // Does it shadow ancestor ns-decls?
            XML_NSMAP_FOREACH!(*ns_map, mi, {
                if ((*mi).depth < depth)
                    && (*mi).shadow_depth == -1
                    && (*ns).prefix() == (*(*mi).new_ns).prefix()
                {
                    // Shadows.
                    (*mi).shadow_depth = depth;
                    break;
                }
            });
        }
        if xml_dom_wrap_ns_map_add_item(ns_map, -1, XmlNsPtr::from_raw(ns).unwrap(), tmpns, depth)
            .is_null()
        {
            xml_free_ns(XmlNsPtr::from_raw(tmpns).unwrap().unwrap());
            return -1;
        }
        *ret_ns = tmpns;
    }
    0
}

/// Ensures that ns-references point to ns-decls hold on element-nodes.
/// Ensures that the tree is namespace wellformed by creating additional
/// ns-decls where needed. Note that, since prefixes of already existent
/// ns-decls can be shadowed by this process, it could break QNames in
/// attribute values or element content.
///
/// NOTE: This function was not intensively tested.
///
/// Returns 0 if succeeded, -1 otherwise and on API/internal errors.
#[doc(alias = "xmlDOMWrapReconcileNamespaces")]
pub unsafe fn xml_dom_wrap_reconcile_namespaces(
    _ctxt: XmlDOMWrapCtxtPtr,
    elem: *mut XmlNode,
    options: i32,
) -> i32 {
    let mut depth: i32 = -1;
    let mut adoptns: i32;
    let mut parnsdone: i32 = 0;
    let mut cur: *mut XmlNode;
    let mut cur_elem: *mut XmlNode = null_mut();
    let mut ns_map: XmlNsMapPtr = null_mut();
    let mut mi: XmlNsMapItemPtr;
    // @ancestorsOnly should be set by an option flag.
    let ancestors_only: i32 = 0;
    let opt_remove_redundant_ns: i32 =
        if options & XmlDomreconcileNsoptions::XmlDomReconnsRemoveredund as i32 != 0 {
            1
        } else {
            0
        };
    let mut list_redund = vec![];
    let mut nb_redund: i32 = 0;

    if elem.is_null()
        || (*elem).doc.is_null()
        || !matches!((*elem).element_type(), XmlElementType::XmlElementNode)
    {
        return -1;
    }

    let ret;
    let doc: *mut XmlDoc = (*elem).doc;
    cur = elem;
    'exit: {
        'internal_error: {
            'main: while {
                match (*cur).element_type() {
                    XmlElementType::XmlElementNode => {
                        adoptns = 1;
                        cur_elem = cur;
                        depth += 1;
                        // Namespace declarations.
                        if !(*cur).ns_def.is_null() {
                            let mut prevns = None::<XmlNsPtr>;
                            let mut ns = XmlNsPtr::from_raw((*cur).ns_def).unwrap();
                            'b: while let Some(cur_ns) = ns {
                                if parnsdone == 0 {
                                    if let Some(parent) = (*elem)
                                        .parent()
                                        .filter(|p| p.doc != p.as_ptr() as *mut XmlDoc)
                                    {
                                        // Gather ancestor in-scope ns-decls.
                                        if xml_dom_wrap_ns_norm_gather_in_scope_ns(
                                            &raw mut ns_map,
                                            parent.as_ptr(),
                                        ) == -1
                                        {
                                            break 'internal_error;
                                        }
                                    }
                                    parnsdone = 1;
                                }

                                // Lookup the ns ancestor-axis for equal ns-decls in scope.
                                if opt_remove_redundant_ns != 0 && XML_NSMAP_NOTEMPTY!(ns_map) {
                                    XML_NSMAP_FOREACH!(ns_map, mi, {
                                        if (*mi).depth >= XML_TREE_NSMAP_PARENT
                                            && (*mi).shadow_depth == -1
                                            && cur_ns.prefix() == (*(*mi).new_ns).prefix()
                                            && cur_ns.href() == (*(*mi).new_ns).href()
                                        {
                                            // A redundant ns-decl was found.
                                            // Add it to the list of redundant ns-decls.
                                            if xml_dom_wrap_ns_norm_add_ns_map_item2(
                                                &mut list_redund,
                                                // &raw mut size_redund,
                                                &raw mut nb_redund,
                                                cur_ns.as_ptr(),
                                                (*mi).new_ns,
                                            ) == -1
                                            {
                                                break 'internal_error;
                                            }
                                            // Remove the ns-decl from the element-node.
                                            if let Some(mut prevns) = prevns {
                                                prevns.next = cur_ns.next;
                                            } else {
                                                (*cur).ns_def = cur_ns.next;
                                            }
                                            // goto next_ns_decl;
                                            ns = XmlNsPtr::from_raw(cur_ns.next).unwrap();
                                            continue 'b;
                                        }
                                    });
                                }

                                // Skip ns-references handling if the referenced
                                // ns-decl is declared on the same element.
                                if !(*cur).ns.is_null()
                                    && adoptns != 0
                                    && XmlNsPtr::from_raw((*cur).ns).unwrap() == Some(cur_ns)
                                {
                                    adoptns = 0;
                                }
                                // Does it shadow any ns-decl?
                                if XML_NSMAP_NOTEMPTY!(ns_map) {
                                    XML_NSMAP_FOREACH!(ns_map, mi, {
                                        if (*mi).depth >= XML_TREE_NSMAP_PARENT
                                            && (*mi).shadow_depth == -1
                                            && cur_ns.prefix() == (*(*mi).new_ns).prefix()
                                        {
                                            (*mi).shadow_depth = depth;
                                        }
                                    });
                                }
                                // Push mapping.
                                if xml_dom_wrap_ns_map_add_item(
                                    &raw mut ns_map,
                                    -1,
                                    Some(cur_ns),
                                    cur_ns.as_ptr(),
                                    depth,
                                )
                                .is_null()
                                {
                                    break 'internal_error;
                                }

                                prevns = Some(cur_ns);
                                // next_ns_decl:
                                ns = XmlNsPtr::from_raw(cur_ns.next).unwrap();
                            }
                        }
                        if adoptns == 0 {
                            // goto ns_end;
                            if matches!((*cur).element_type(), XmlElementType::XmlElementNode)
                                && !(*cur).properties.is_null()
                            {
                                // Process attributes.
                                cur = (*cur).properties as _;
                                if cur.is_null() {
                                    break 'main;
                                }
                                continue 'main;
                            }
                        }

                        /* No ns, no fun. */
                        if (*cur).ns.is_null() {
                            // goto ns_end;
                            if matches!((*cur).element_type(), XmlElementType::XmlElementNode)
                                && !(*cur).properties.is_null()
                            {
                                // Process attributes.
                                cur = (*cur).properties as _;
                                if cur.is_null() {
                                    break 'main;
                                }
                                continue 'main;
                            }
                        }

                        if parnsdone == 0 {
                            if (*elem)
                                .parent()
                                .filter(|p| {
                                    p.doc != p.as_ptr() as *mut XmlDoc
                                        && xml_dom_wrap_ns_norm_gather_in_scope_ns(
                                            &raw mut ns_map,
                                            p.as_ptr(),
                                        ) == -1
                                })
                                .is_some()
                            {
                                break 'internal_error;
                            }
                            parnsdone = 1;
                        }
                        // Adjust the reference if this was a redundant ns-decl.
                        for (_, j) in (0..nb_redund).zip((0..).step_by(2)) {
                            if (*cur).ns == list_redund[j] {
                                (*cur).ns = list_redund[j + 1];
                                break;
                            }
                        }
                        // Adopt ns-references.
                        if XML_NSMAP_NOTEMPTY!(ns_map) {
                            // Search for a mapping.
                            XML_NSMAP_FOREACH!(ns_map, mi, {
                                if (*mi).shadow_depth == -1
                                    && XmlNsPtr::from_raw((*cur).ns).unwrap() == (*mi).old_ns
                                {
                                    (*cur).ns = (*mi).new_ns;
                                    // goto ns_end;
                                    if matches!(
                                        (*cur).element_type(),
                                        XmlElementType::XmlElementNode
                                    ) && !(*cur).properties.is_null()
                                    {
                                        // Process attributes.
                                        cur = (*cur).properties as _;
                                        if cur.is_null() {
                                            break 'main;
                                        }
                                        continue 'main;
                                    }
                                }
                            });
                        }
                        // Acquire a normalized ns-decl and add it to the map.
                        let mut ns = null_mut();
                        if xml_dom_wrap_ns_norm_acquire_normalized_ns(
                            doc,
                            cur_elem,
                            (*cur).ns,
                            &mut ns,
                            &raw mut ns_map,
                            depth,
                            ancestors_only,
                            matches!((*cur).element_type(), XmlElementType::XmlAttributeNode)
                                as i32,
                        ) == -1
                        {
                            break 'internal_error;
                        }
                        (*cur).ns = ns;

                        // ns_end:
                        if matches!((*cur).element_type(), XmlElementType::XmlElementNode)
                            && !(*cur).properties.is_null()
                        {
                            // Process attributes.
                            cur = (*cur).properties as _;
                            if cur.is_null() {
                                break 'main;
                            }
                            continue 'main;
                        }
                    }
                    XmlElementType::XmlAttributeNode => {
                        if parnsdone == 0 {
                            if (*elem)
                                .parent()
                                .filter(|p| {
                                    p.doc != p.as_ptr() as *mut XmlDoc
                                        && xml_dom_wrap_ns_norm_gather_in_scope_ns(
                                            &raw mut ns_map,
                                            p.as_ptr(),
                                        ) == -1
                                })
                                .is_some()
                            {
                                break 'internal_error;
                            }
                            parnsdone = 1;
                        }
                        // Adjust the reference if this was a redundant ns-decl.
                        for (_, j) in (0..nb_redund).zip((0..).step_by(2)) {
                            if (*cur).ns == list_redund[j] {
                                (*cur).ns = list_redund[j + 1];
                                break;
                            }
                        }
                        // Adopt ns-references.
                        if XML_NSMAP_NOTEMPTY!(ns_map) {
                            // Search for a mapping.
                            XML_NSMAP_FOREACH!(ns_map, mi, {
                                if (*mi).shadow_depth == -1
                                    && XmlNsPtr::from_raw((*cur).ns).unwrap() == (*mi).old_ns
                                {
                                    (*cur).ns = (*mi).new_ns;
                                    // goto ns_end;
                                    if matches!(
                                        (*cur).element_type(),
                                        XmlElementType::XmlElementNode
                                    ) && !(*cur).properties.is_null()
                                    {
                                        // Process attributes.
                                        cur = (*cur).properties as _;
                                        if cur.is_null() {
                                            break 'main;
                                        }
                                        continue 'main;
                                    }
                                }
                            });
                        }
                        // Acquire a normalized ns-decl and add it to the map.
                        let mut ns = null_mut();
                        if xml_dom_wrap_ns_norm_acquire_normalized_ns(
                            doc,
                            cur_elem,
                            (*cur).ns,
                            &mut ns,
                            &raw mut ns_map,
                            depth,
                            ancestors_only,
                            matches!((*cur).element_type(), XmlElementType::XmlAttributeNode)
                                as i32,
                        ) == -1
                        {
                            break 'internal_error;
                        }
                        (*cur).ns = ns;

                        // ns_end:
                        if matches!((*cur).element_type(), XmlElementType::XmlElementNode)
                            && !(*cur).properties.is_null()
                        {
                            // Process attributes.
                            cur = (*cur).properties as _;
                            if cur.is_null() {
                                break 'main;
                            }
                            continue 'main;
                        }
                    }
                    _ => {
                        // goto next_sibling;
                        'next_sibling: loop {
                            if cur == elem {
                                break 'main;
                            }
                            if matches!((*cur).element_type(), XmlElementType::XmlElementNode) {
                                if XML_NSMAP_NOTEMPTY!(ns_map) {
                                    // Pop mappings.
                                    while !(*ns_map).last.is_null()
                                        && (*(*ns_map).last).depth >= depth
                                    {
                                        XML_NSMAP_POP!(ns_map, mi);
                                    }
                                    // Unshadow.
                                    XML_NSMAP_FOREACH!(ns_map, mi, {
                                        if (*mi).shadow_depth >= depth {
                                            (*mi).shadow_depth = -1;
                                        }
                                    });
                                }
                                depth -= 1;
                            }
                            if let Some(next) = (*cur).next {
                                cur = next.as_ptr();
                            } else {
                                if matches!((*cur).element_type(), XmlElementType::XmlAttributeNode)
                                {
                                    cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
                                    // goto into_content;
                                    break 'next_sibling;
                                }
                                cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
                                // goto next_sibling;
                                continue 'next_sibling;
                            }

                            if cur.is_null() {
                                break 'main;
                            }
                            continue 'main;
                        }
                    }
                }
                // into_content:
                'into_content: loop {
                    if let Some(children) = (*cur)
                        .children()
                        .filter(|_| matches!((*cur).element_type(), XmlElementType::XmlElementNode))
                    {
                        // Process content of element-nodes only.
                        cur = children.as_ptr();
                        continue;
                    }
                    // next_sibling:
                    'next_sibling: loop {
                        if cur == elem {
                            break 'main;
                        }
                        if matches!((*cur).element_type(), XmlElementType::XmlElementNode) {
                            if XML_NSMAP_NOTEMPTY!(ns_map) {
                                // Pop mappings.
                                while !(*ns_map).last.is_null() && (*(*ns_map).last).depth >= depth
                                {
                                    XML_NSMAP_POP!(ns_map, mi);
                                }
                                // Unshadow.
                                XML_NSMAP_FOREACH!(ns_map, mi, {
                                    if (*mi).shadow_depth >= depth {
                                        (*mi).shadow_depth = -1;
                                    }
                                });
                            }
                            depth -= 1;
                        }
                        if let Some(next) = (*cur).next {
                            cur = next.as_ptr();
                        } else {
                            if matches!((*cur).element_type(), XmlElementType::XmlAttributeNode) {
                                cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
                                // goto into_content;
                                continue 'into_content;
                            }
                            cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
                            // goto next_sibling;
                            continue 'next_sibling;
                        }
                        break 'next_sibling;
                    }

                    break 'into_content;
                }

                !cur.is_null()
            } {}

            ret = 0;
            break 'exit;
        }
        // internal_error:
        ret = -1;
    }
    // exit:
    for (_, j) in (0..nb_redund).zip((0..).step_by(2)) {
        xml_free_ns(XmlNsPtr::from_raw(list_redund[j]).unwrap().unwrap());
    }
    if !ns_map.is_null() {
        xml_dom_wrap_ns_map_free(ns_map);
    }
    ret
}

/// Ensures that ns-references point to @destDoc: either to
/// elements->nsDef entries if @destParent is given, or to
/// @(*destDoc).oldNs otherwise.
/// If @destParent is given, it ensures that the tree is namespace
/// wellformed by creating additional ns-decls where needed.
/// Note that, since prefixes of already existent ns-decls can be
/// shadowed by this process, it could break QNames in attribute
/// values or element content.
///
/// NOTE: This function was not intensively tested.
///
/// Returns 0 if succeeded, -1 otherwise and on API/internal errors.
#[doc(alias = "xmlDOMWrapAdoptBranch")]
unsafe fn xml_dom_wrap_adopt_branch(
    ctxt: XmlDOMWrapCtxtPtr,
    source_doc: *mut XmlDoc,
    node: *mut XmlNode,
    dest_doc: *mut XmlDoc,
    dest_parent: *mut XmlNode,
    _options: i32,
) -> i32 {
    let mut ret: i32 = 0;
    let mut cur: *mut XmlNode;
    let mut cur_elem: *mut XmlNode = null_mut();
    let mut ns_map: XmlNsMapPtr = null_mut();
    let mut mi: XmlNsMapItemPtr;
    let mut depth: i32 = -1;
    // gather @parent's ns-decls.
    let mut parnsdone: i32;
    // @ancestorsOnly should be set per option.
    let ancestors_only: i32 = 0;

    // Get the ns-map from the context if available.
    if !ctxt.is_null() {
        ns_map = (*ctxt).namespace_map as _;
    }
    // Disable search for ns-decls in the parent-axis of the
    // destination element, if:
    // 1) there's no destination parent
    // 2) custom ns-reference handling is used
    if dest_parent.is_null() || (!ctxt.is_null() && (*ctxt).get_ns_for_node_func.is_some()) {
        parnsdone = 1;
    } else {
        parnsdone = 0;
    }

    'exit: {
        'internal_error: {
            cur = node;
            if !cur.is_null() && matches!((*cur).element_type(), XmlElementType::XmlNamespaceDecl) {
                break 'internal_error;
            }

            'main: while !cur.is_null() {
                let mut leave_node = false;

                // Paranoid source-doc sanity check.
                if (*cur).doc != source_doc {
                    // We'll assume XIncluded nodes if the doc differs.
                    // TODO: Do we need to reconciliate XIncluded nodes?
                    // This here skips XIncluded nodes and tries to handle
                    // broken sequences.
                    if (*cur).next.is_some() {
                        let mut next = (*cur).next;
                        while let Some(now) = next.filter(|now| {
                            !matches!(now.element_type(), XmlElementType::XmlXIncludeEnd)
                                && now.doc != (*node).doc
                        }) {
                            cur = now.as_ptr();
                            next = now.next;
                        }

                        if (*cur).doc != (*node).doc {
                            // goto leave_node;
                            leave_node = true;
                        }
                    } else {
                        // goto leave_node;
                        leave_node = true;
                    }
                }

                if !leave_node {
                    (*cur).doc = dest_doc;
                    match (*cur).element_type() {
                        XmlElementType::XmlXIncludeStart | XmlElementType::XmlXIncludeEnd => {
                            // TODO
                            return -1;
                        }
                        XmlElementType::XmlElementNode => {
                            cur_elem = cur;
                            depth += 1;
                            // Namespace declarations.
                            // - (*ns).href and (*ns).prefix are never in the dict, so
                            //   we need not move the values over to the destination dict.
                            // - Note that for custom handling of ns-references,
                            //   the ns-decls need not be stored in the ns-map,
                            //   since they won't be referenced by (*node).ns.
                            if !(*cur).ns_def.is_null()
                                && (ctxt.is_null() || (*ctxt).get_ns_for_node_func.is_none())
                            {
                                if parnsdone == 0 {
                                    // Gather @parent's in-scope ns-decls.
                                    if xml_dom_wrap_ns_norm_gather_in_scope_ns(
                                        &raw mut ns_map,
                                        dest_parent,
                                    ) == -1
                                    {
                                        break 'internal_error;
                                    }

                                    parnsdone = 1;
                                }
                                let mut ns = XmlNsPtr::from_raw((*cur).ns_def).unwrap();
                                while let Some(now) = ns {
                                    // Does it shadow any ns-decl?
                                    if XML_NSMAP_NOTEMPTY!(ns_map) {
                                        XML_NSMAP_FOREACH!(ns_map, mi, {
                                            if (*mi).depth >= XML_TREE_NSMAP_PARENT
                                                && (*mi).shadow_depth == -1
                                                && now.prefix() == (*(*mi).new_ns).prefix()
                                            {
                                                (*mi).shadow_depth = depth;
                                            }
                                        });
                                    }
                                    // Push mapping.
                                    if xml_dom_wrap_ns_map_add_item(
                                        &raw mut ns_map,
                                        -1,
                                        Some(now),
                                        now.as_ptr(),
                                        depth,
                                    )
                                    .is_null()
                                    {
                                        break 'internal_error;
                                    }
                                    ns = XmlNsPtr::from_raw(now.next).unwrap();
                                }
                            }
                            /* No namespace, no fun. */
                            if (*cur).ns.is_null() {
                                // goto ns_end;
                            } else {
                                if parnsdone == 0 {
                                    if xml_dom_wrap_ns_norm_gather_in_scope_ns(
                                        &raw mut ns_map,
                                        dest_parent,
                                    ) == -1
                                    {
                                        break 'internal_error;
                                    }
                                    parnsdone = 1;
                                }
                                // Adopt ns-references.
                                let mut ns_end = false;
                                if XML_NSMAP_NOTEMPTY!(ns_map) {
                                    // Search for a mapping.
                                    XML_NSMAP_FOREACH!(ns_map, mi, {
                                        if (*mi).shadow_depth == -1
                                            && XmlNsPtr::from_raw((*cur).ns).unwrap()
                                                == (*mi).old_ns
                                        {
                                            (*cur).ns = (*mi).new_ns;
                                            // goto ns_end;
                                            ns_end = true;
                                            break;
                                        }
                                    });
                                }

                                if !ns_end {
                                    // No matching namespace in scope. We need a new one.
                                    if !ctxt.is_null() && (*ctxt).get_ns_for_node_func.is_some() {
                                        // User-defined behaviour.
                                        let ns = ((*ctxt).get_ns_for_node_func.unwrap())(
                                            ctxt,
                                            cur,
                                            (*(*cur).ns).href,
                                            (*(*cur).ns).prefix,
                                        );
                                        // Insert mapping if ns is available; it's the users fault
                                        // if not.
                                        if xml_dom_wrap_ns_map_add_item(
                                            &raw mut ns_map,
                                            -1,
                                            XmlNsPtr::from_raw((*cur).ns).unwrap(),
                                            ns,
                                            XML_TREE_NSMAP_CUSTOM,
                                        )
                                        .is_null()
                                        {
                                            break 'internal_error;
                                        }
                                        (*cur).ns = ns;
                                    } else {
                                        // Acquire a normalized ns-decl and add it to the map.
                                        let mut ns = null_mut();
                                        if xml_dom_wrap_ns_norm_acquire_normalized_ns(
                                            dest_doc,
                                            // ns-decls on curElem or on (*destDoc).oldNs
                                            if !dest_parent.is_null() {
                                                cur_elem
                                            } else {
                                                null_mut()
                                            },
                                            (*cur).ns,
                                            &mut ns,
                                            &raw mut ns_map,
                                            depth,
                                            ancestors_only,
                                            // ns-decls must be prefixed for attributes.
                                            matches!(
                                                (*cur).element_type(),
                                                XmlElementType::XmlAttributeNode
                                            ) as i32,
                                        ) == -1
                                        {
                                            break 'internal_error;
                                        }
                                        (*cur).ns = ns;
                                    }
                                }
                            }
                            // ns_end:
                            // Further node properties.
                            // TODO: Is this all?
                            (*cur).psvi = null_mut();
                            (*cur).line = 0;
                            (*cur).extra = 0;
                            // Walk attributes.
                            if !(*cur).properties.is_null() {
                                // Process first attribute node.
                                cur = (*cur).properties as _;
                                continue;
                            }
                        }
                        XmlElementType::XmlAttributeNode => {
                            /* No namespace, no fun. */
                            if (*cur).ns.is_null() {
                                // goto ns_end;
                            } else {
                                if parnsdone == 0 {
                                    if xml_dom_wrap_ns_norm_gather_in_scope_ns(
                                        &raw mut ns_map,
                                        dest_parent,
                                    ) == -1
                                    {
                                        break 'internal_error;
                                    }
                                    parnsdone = 1;
                                }
                                // Adopt ns-references.
                                let mut ns_end = false;
                                if XML_NSMAP_NOTEMPTY!(ns_map) {
                                    // Search for a mapping.
                                    XML_NSMAP_FOREACH!(ns_map, mi, {
                                        if (*mi).shadow_depth == -1
                                            && XmlNsPtr::from_raw((*cur).ns).unwrap()
                                                == (*mi).old_ns
                                        {
                                            (*cur).ns = (*mi).new_ns;
                                            // goto ns_end;
                                            ns_end = true;
                                            break;
                                        }
                                    });
                                }

                                if !ns_end {
                                    // No matching namespace in scope. We need a new one.
                                    if !ctxt.is_null() && (*ctxt).get_ns_for_node_func.is_some() {
                                        // User-defined behaviour.
                                        let ns = ((*ctxt).get_ns_for_node_func.unwrap())(
                                            ctxt,
                                            cur,
                                            (*(*cur).ns).href,
                                            (*(*cur).ns).prefix,
                                        );
                                        // Insert mapping if ns is available; it's the users fault
                                        // if not.
                                        if xml_dom_wrap_ns_map_add_item(
                                            &raw mut ns_map,
                                            -1,
                                            XmlNsPtr::from_raw((*cur).ns).unwrap(),
                                            ns,
                                            XML_TREE_NSMAP_CUSTOM,
                                        )
                                        .is_null()
                                        {
                                            break 'internal_error;
                                        }
                                        (*cur).ns = ns;
                                    } else {
                                        // Acquire a normalized ns-decl and add it to the map.
                                        let mut ns = null_mut();
                                        if xml_dom_wrap_ns_norm_acquire_normalized_ns(
                                            dest_doc,
                                            // ns-decls on curElem or on (*destDoc).oldNs
                                            if !dest_parent.is_null() {
                                                cur_elem
                                            } else {
                                                null_mut()
                                            },
                                            (*cur).ns,
                                            &mut ns,
                                            &raw mut ns_map,
                                            depth,
                                            ancestors_only,
                                            // ns-decls must be prefixed for attributes.
                                            matches!(
                                                (*cur).element_type(),
                                                XmlElementType::XmlAttributeNode
                                            ) as i32,
                                        ) == -1
                                        {
                                            break 'internal_error;
                                        }
                                        (*cur).ns = ns;
                                    }
                                }
                            }
                            // ns_end:
                            // Further node properties.
                            // TODO: Is this all?
                            // Attributes.
                            if !source_doc.is_null()
                                && matches!(
                                    (*cur).as_attribute_node().unwrap().as_ref().atype,
                                    Some(XmlAttributeType::XmlAttributeID)
                                )
                            {
                                xml_remove_id(source_doc, cur as _);
                            }
                            (*cur).as_attribute_node().unwrap().as_mut().atype = None;
                            (*cur).as_attribute_node().unwrap().as_mut().psvi = null_mut();
                        }
                        XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode => {
                            // goto leave_node;
                            leave_node = true;
                        }
                        XmlElementType::XmlEntityRefNode => {
                            // Remove reference to the entity-node.
                            (*cur).content = null_mut();
                            (*cur).set_children(None);
                            (*cur).set_last(None);
                            if (*dest_doc).int_subset.is_some() || (*dest_doc).ext_subset.is_some()
                            {
                                // Assign new entity-node if available.
                                let ent = xml_get_doc_entity(dest_doc, &(*cur).name().unwrap());
                                if let Some(ent) = ent {
                                    (*cur).content = ent.content.load(Ordering::Relaxed);
                                    (*cur).set_children(NodePtr::from_ptr(
                                        ent.as_ptr() as *mut XmlNode
                                    ));
                                    (*cur)
                                        .set_last(NodePtr::from_ptr(ent.as_ptr() as *mut XmlNode));
                                }
                            }
                            // goto leave_node;
                            leave_node = true;
                        }
                        XmlElementType::XmlPINode => {}
                        XmlElementType::XmlCommentNode => {}
                        _ => {
                            break 'internal_error;
                        }
                    }

                    if !leave_node {
                        // Walk the tree.
                        if let Some(children) = (*cur).children() {
                            cur = children.as_ptr();
                            continue;
                        }
                    }
                }

                // leave_node:
                'leave_node: loop {
                    if cur == node {
                        break 'main;
                    }
                    if matches!(
                        (*cur).element_type(),
                        XmlElementType::XmlElementNode
                            | XmlElementType::XmlXIncludeStart
                            | XmlElementType::XmlXIncludeEnd
                    ) {
                        // TODO: Do we expect nsDefs on xmlElementType::XML_XINCLUDE_START?
                        if XML_NSMAP_NOTEMPTY!(ns_map) {
                            // Pop mappings.
                            while !(*ns_map).last.is_null() && (*(*ns_map).last).depth >= depth {
                                XML_NSMAP_POP!(ns_map, mi);
                            }
                            // Unshadow.
                            XML_NSMAP_FOREACH!(ns_map, mi, {
                                if (*mi).shadow_depth >= depth {
                                    (*mi).shadow_depth = -1;
                                }
                            });
                        }
                        depth -= 1;
                    }
                    if let Some(next) = (*cur).next {
                        cur = next.as_ptr();
                    } else if let Some(children) = (*cur).parent().and_then(|p| {
                        p.children().filter(|_| {
                            matches!((*cur).element_type(), XmlElementType::XmlAttributeNode)
                        })
                    }) {
                        cur = children.as_ptr();
                    } else {
                        cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
                        // goto leave_node;
                        continue 'leave_node;
                    }

                    break;
                }
            }
            break 'exit;
        }
        // internal_error:
        ret = -1;
    }

    // exit:
    // Cleanup.
    if !ns_map.is_null() {
        if !ctxt.is_null() && (*ctxt).namespace_map == ns_map as _ {
            // Just cleanup the map but don't free.
            if !(*ns_map).first.is_null() {
                if !(*ns_map).pool.is_null() {
                    (*(*ns_map).last).next = (*ns_map).pool;
                }
                (*ns_map).pool = (*ns_map).first;
                (*ns_map).first = null_mut();
            }
        } else {
            xml_dom_wrap_ns_map_free(ns_map);
        }
    }
    ret
}

/// @attr is adopted by @destDoc.
/// Ensures that ns-references point to @destDoc: either to
/// elements->nsDef entries if @destParent is given, or to
/// @(*destDoc).oldNs otherwise.
///
/// Returns 0 if succeeded, -1 otherwise and on API/internal errors.
#[doc(alias = "xmlDOMWrapAdoptAttr")]
unsafe fn xml_dom_wrap_adopt_attr(
    ctxt: XmlDOMWrapCtxtPtr,
    _source_doc: *mut XmlDoc,
    attr: *mut XmlAttr,
    dest_doc: *mut XmlDoc,
    dest_parent: *mut XmlNode,
    _options: i32,
) -> i32 {
    let mut cur: *mut XmlNode;

    if !attr.is_null() || dest_doc.is_null() {
        return -1;
    }

    (*attr).doc = dest_doc;
    if !(*attr).ns.is_null() {
        let mut ns = None;

        if !ctxt.is_null() { /* TODO: User defined. */ }
        // XML Namespace.
        if (*(*attr).ns).prefix().as_deref() == Some("xml") {
            ns = (*dest_doc).ensure_xmldecl();
        } else if dest_parent.is_null() {
            // Store in @(*destDoc).oldNs.
            ns = XmlNsPtr::from_raw(xml_dom_wrap_store_ns(
                dest_doc,
                (*(*attr).ns).href,
                (*(*attr).ns).prefix().as_deref(),
            ))
            .unwrap();
        } else {
            // Declare on @destParent.
            if xml_search_ns_by_namespace_strict(
                dest_doc,
                dest_parent,
                (*(*attr).ns).href,
                &mut ns,
                1,
            ) == -1
            {
                // goto internal_error;
                return -1;
            }
            if ns.is_none() {
                ns = XmlNsPtr::from_raw(xml_dom_wrap_nsnorm_declare_ns_forced(
                    dest_doc,
                    dest_parent,
                    (*(*attr).ns).href,
                    (*(*attr).ns).prefix,
                    1,
                ))
                .unwrap();
            }
        }
        let Some(ns) = ns else {
            // goto internal_error;
            return -1;
        };
        (*attr).ns = ns.as_ptr();
    }

    (*attr).atype = None;
    (*attr).psvi = null_mut();
    // Walk content.
    let Some(children) = (*attr).children else {
        return 0;
    };
    cur = children.as_ptr();
    if !cur.is_null() && matches!((*cur).element_type(), XmlElementType::XmlNamespaceDecl) {
        // goto internal_error;
        return -1;
    }
    while !cur.is_null() {
        (*cur).doc = dest_doc;
        match (*cur).element_type() {
            XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode => {}
            XmlElementType::XmlEntityRefNode => {
                // Remove reference to the entity-node.
                (*cur).content = null_mut();
                (*cur).set_children(None);
                (*cur).set_last(None);
                if (*dest_doc).int_subset.is_some() || (*dest_doc).ext_subset.is_some() {
                    // Assign new entity-node if available.
                    let ent = xml_get_doc_entity(dest_doc, &(*cur).name().unwrap());
                    if let Some(ent) = ent {
                        (*cur).content = ent.content.load(Ordering::Relaxed);
                        (*cur).set_children(NodePtr::from_ptr(ent.as_ptr() as *mut XmlNode));
                        (*cur).set_last(NodePtr::from_ptr(ent.as_ptr() as *mut XmlNode));
                    }
                }
            }
            _ => {}
        }
        if let Some(children) = (*cur).children() {
            cur = children.as_ptr();
            continue;
        }
        // next_sibling:
        'next_sibling: loop {
            if cur == attr as _ {
                break;
            }
            if let Some(next) = (*cur).next {
                cur = next.as_ptr();
            } else {
                cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
                // goto next_sibling;
                continue 'next_sibling;
            }

            break;
        }
    }
    0
    // internal_error:
    //     return -1;
}

/// References of out-of scope ns-decls are remapped to point to @destDoc:
/// 1) If @destParent is given, then nsDef entries on element-nodes are used
/// 2) If *no* @destParent is given, then @(*destDoc).oldNs entries are used
///    This is the case when you have an unlinked node and just want to move it
///    to the context of
///
/// If @destParent is given, it ensures that the tree is namespace
/// wellformed by creating additional ns-decls where needed.
/// Note that, since prefixes of already existent ns-decls can be
/// shadowed by this process, it could break QNames in attribute
/// values or element content.
/// NOTE: This function was not intensively tested.
///
/// Returns 0 if the operation succeeded,
///         1 if a node of unsupported type was given,
///         2 if a node of not yet supported type was given and
///         -1 on API/internal errors.
#[doc(alias = "xmlDOMWrapAdoptNode")]
pub unsafe fn xml_dom_wrap_adopt_node(
    ctxt: XmlDOMWrapCtxtPtr,
    mut source_doc: *mut XmlDoc,
    node: *mut XmlNode,
    dest_doc: *mut XmlDoc,
    dest_parent: *mut XmlNode,
    options: i32,
) -> i32 {
    if node.is_null()
        || matches!((*node).element_type(), XmlElementType::XmlNamespaceDecl)
        || dest_doc.is_null()
        || (!dest_parent.is_null() && (*dest_parent).doc != dest_doc)
    {
        return -1;
    }
    // Check (*node).doc sanity.
    if !(*node).doc.is_null() && !source_doc.is_null() && (*node).doc != source_doc {
        // Might be an XIncluded node.
        return -1;
    }
    if source_doc.is_null() {
        source_doc = (*node).doc;
    }
    if source_doc == dest_doc {
        return -1;
    }
    match (*node).element_type() {
        XmlElementType::XmlElementNode
        | XmlElementType::XmlAttributeNode
        | XmlElementType::XmlTextNode
        | XmlElementType::XmlCDATASectionNode
        | XmlElementType::XmlEntityRefNode
        | XmlElementType::XmlPINode
        | XmlElementType::XmlCommentNode => {}
        XmlElementType::XmlDocumentFragNode => {
            // TODO: Support document-fragment-nodes.
            return 2;
        }
        _ => {
            return 1;
        }
    }
    // Unlink only if @node was not already added to @destParent.
    if (*node)
        .parent()
        .filter(|p| dest_parent != p.as_ptr())
        .is_some()
    {
        (*node).unlink();
    }

    if matches!((*node).element_type(), XmlElementType::XmlElementNode) {
        return xml_dom_wrap_adopt_branch(ctxt, source_doc, node, dest_doc, dest_parent, options);
    } else if matches!((*node).element_type(), XmlElementType::XmlAttributeNode) {
        return xml_dom_wrap_adopt_attr(
            ctxt,
            source_doc,
            node as _,
            dest_doc,
            dest_parent,
            options,
        );
    } else {
        let cur: *mut XmlNode = node;

        (*cur).doc = dest_doc;
        // Optimize string adoption.
        // if !source_doc.is_null() && (*source_doc).dict == (*dest_doc).dict {
        //     adopt_str = 0;
        // }
        match (*node).element_type() {
            XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode => {}
            XmlElementType::XmlEntityRefNode => {
                // Remove reference to the entity-node.
                (*node).content = null_mut();
                (*node).set_children(None);
                (*node).set_last(None);
                if (*dest_doc).int_subset.is_some() || (*dest_doc).ext_subset.is_some() {
                    // Assign new entity-node if available.
                    let ent = xml_get_doc_entity(dest_doc, &(*node).name().unwrap());
                    if let Some(ent) = ent {
                        (*node).content = ent.content.load(Ordering::Relaxed);
                        (*node).set_children(NodePtr::from_ptr(ent.as_ptr() as *mut XmlNode));
                        (*node).set_last(NodePtr::from_ptr(ent.as_ptr() as *mut XmlNode));
                    }
                }
            }
            XmlElementType::XmlPINode => {}
            _ => {}
        }
    }
    0
}

/// Unlinks the given node from its owner.
/// This will substitute ns-references to (*node).nsDef for
/// ns-references to (*doc).oldNs, thus ensuring the removed
/// branch to be autark wrt ns-references.
///
/// NOTE: This function was not intensively tested.
///
/// Returns 0 on success, 1 if the node is not supported,
///         -1 on API and internal errors.
#[doc(alias = "xmlDOMWrapRemoveNode")]
pub unsafe fn xml_dom_wrap_remove_node(
    ctxt: XmlDOMWrapCtxtPtr,
    doc: *mut XmlDoc,
    mut node: *mut XmlNode,
    _options: i32,
) -> i32 {
    let mut list = vec![];
    let mut nb_list: i32 = 0;

    if node.is_null() || doc.is_null() || (*node).doc != doc {
        return -1;
    }

    // TODO: 0 or -1 ?
    if (*node).parent().is_none() {
        return 0;
    }

    match (*node).element_type() {
        XmlElementType::XmlTextNode
        | XmlElementType::XmlCDATASectionNode
        | XmlElementType::XmlEntityRefNode
        | XmlElementType::XmlPINode
        | XmlElementType::XmlCommentNode => {
            (*node).unlink();
            return 0;
        }
        XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode => {}
        _ => {
            return 1;
        }
    }
    (*node).unlink();
    // Save out-of-scope ns-references in (*doc).oldNs.
    'main: loop {
        match (*node).element_type() {
            XmlElementType::XmlElementNode => {
                if ctxt.is_null() && !(*node).ns_def.is_null() {
                    let mut ns = XmlNsPtr::from_raw((*node).ns_def).unwrap();
                    while let Some(now) = ns {
                        if xml_dom_wrap_ns_norm_add_ns_map_item2(
                            &mut list,
                            // &raw mut size_list,
                            &raw mut nb_list,
                            now.as_ptr(),
                            now.as_ptr(),
                        ) == -1
                        {
                            // goto internal_error;
                            return -1;
                        }
                        ns = XmlNsPtr::from_raw(now.next).unwrap();
                    }
                }

                if !(*node).ns.is_null() {
                    // Find a mapping.
                    for (_, j) in (0..nb_list).zip((0..).step_by(2)) {
                        if (*node).ns == list[j] {
                            (*node).ns = list[j + 1];
                            // goto next_node;
                            if let Some(children) = (*node).children().filter(|_| {
                                matches!((*node).element_type(), XmlElementType::XmlElementNode)
                            }) {
                                node = children.as_ptr();
                                continue 'main;
                            }
                            // next_sibling:
                            'next_sibling: loop {
                                if node.is_null() {
                                    break;
                                }
                                if let Some(next) = (*node).next {
                                    node = next.as_ptr();
                                } else {
                                    node = (*node).parent().map_or(null_mut(), |p| p.as_ptr());
                                    // goto next_sibling;
                                    continue 'next_sibling;
                                }

                                if node.is_null() {
                                    break 'main;
                                }

                                continue 'main;
                            }
                        }
                    }
                    let mut ns = null_mut();
                    if !ctxt.is_null() {
                        // User defined.
                    } else {
                        // Add to doc's oldNs.
                        ns = xml_dom_wrap_store_ns(
                            doc,
                            (*(*node).ns).href,
                            (*(*node).ns).prefix().as_deref(),
                        );
                        if ns.is_null() {
                            // goto internal_error;
                            return -1;
                        }
                    }
                    if !ns.is_null() {
                        // Add mapping.
                        if xml_dom_wrap_ns_norm_add_ns_map_item2(
                            &mut list,
                            // &raw mut size_list,
                            &raw mut nb_list,
                            (*node).ns,
                            ns,
                        ) == -1
                        {
                            // goto internal_error;
                            return -1;
                        }
                    }
                    (*node).ns = ns;
                }
                if matches!((*node).element_type(), XmlElementType::XmlElementNode)
                    && !(*node).properties.is_null()
                {
                    node = (*node).properties as _;
                    continue;
                }
            }
            XmlElementType::XmlAttributeNode => {
                if !(*node).ns.is_null() {
                    // Find a mapping.
                    for (_, j) in (0..nb_list).zip((0..).step_by(2)) {
                        if (*node).ns == list[j] {
                            (*node).ns = list[j + 1];
                            // goto next_node;
                            if let Some(children) = (*node).children().filter(|_| {
                                matches!((*node).element_type(), XmlElementType::XmlElementNode)
                            }) {
                                node = children.as_ptr();
                                continue 'main;
                            }
                            // next_sibling:
                            'next_sibling: loop {
                                if node.is_null() {
                                    break;
                                }
                                if let Some(next) = (*node).next {
                                    node = next.as_ptr();
                                } else {
                                    node = (*node).parent().map_or(null_mut(), |p| p.as_ptr());
                                    // goto next_sibling;
                                    continue 'next_sibling;
                                }

                                if node.is_null() {
                                    break 'main;
                                }

                                continue 'main;
                            }
                        }
                    }
                    let mut ns = null_mut();
                    if !ctxt.is_null() {
                        // User defined.
                    } else {
                        // Add to doc's oldNs.
                        ns = xml_dom_wrap_store_ns(
                            doc,
                            (*(*node).ns).href,
                            (*(*node).ns).prefix().as_deref(),
                        );
                        if ns.is_null() {
                            // goto internal_error;
                            return -1;
                        }
                    }
                    if !ns.is_null() {
                        // Add mapping.
                        if xml_dom_wrap_ns_norm_add_ns_map_item2(
                            &mut list,
                            // &raw mut size_list,
                            &raw mut nb_list,
                            (*node).ns,
                            ns,
                        ) == -1
                        {
                            // goto internal_error;
                            return -1;
                        }
                    }
                    (*node).ns = ns;
                }
                if matches!((*node).element_type(), XmlElementType::XmlElementNode)
                    && !(*node).properties.is_null()
                {
                    node = (*node).properties as _;
                    continue;
                }
            }
            _ => {
                // goto next_sibling;
                'next_sibling: loop {
                    if node.is_null() {
                        break;
                    }
                    if let Some(next) = (*node).next {
                        node = next.as_ptr();
                    } else {
                        node = (*node).parent().map_or(null_mut(), |p| p.as_ptr());
                        // goto next_sibling;
                        continue 'next_sibling;
                    }

                    if node.is_null() {
                        break 'main;
                    }
                    continue 'main;
                }
            }
        }
        // next_node:
        if let Some(children) = (*node)
            .children()
            .filter(|_| matches!((*node).element_type(), XmlElementType::XmlElementNode))
        {
            node = children.as_ptr();
            continue;
        }
        // next_sibling:
        'next_sibling: loop {
            if node.is_null() {
                break;
            }
            if let Some(next) = (*node).next {
                node = next.as_ptr();
            } else {
                node = (*node).parent().map_or(null_mut(), |p| p.as_ptr());
                // goto next_sibling;
                continue 'next_sibling;
            }
        }

        if node.is_null() {
            break;
        }
    }

    0

    // internal_error:
    // if !list.is_null() {
    // 	xml_free(list as _);
    // }
    // return -1;
}

/// References of out-of scope ns-decls are remapped to point to @destDoc:
/// 1) If @destParent is given, then nsDef entries on element-nodes are used
/// 2) If *no* @destParent is given, then @(*destDoc).oldNs entries are used.
///    This is the case when you don't know already where the cloned branch
///    will be added to.
///
/// If @destParent is given, it ensures that the tree is namespace
/// wellformed by creating additional ns-decls where needed.
/// Note that, since prefixes of already existent ns-decls can be
/// shadowed by this process, it could break QNames in attribute
/// values or element content.
/// TODO:
///   1) What to do with XInclude? Currently this returns an error for XInclude.
///
/// Returns 0 if the operation succeeded,
///         1 if a node of unsupported (or not yet supported) type was given,
///         -1 on API/internal errors.
#[doc(alias = "xmlDOMWrapCloneNode")]
#[allow(clippy::too_many_arguments)]
pub unsafe fn xml_dom_wrap_clone_node(
    ctxt: XmlDOMWrapCtxtPtr,
    mut source_doc: *mut XmlDoc,
    node: *mut XmlNode,
    res_node: *mut *mut XmlNode,
    dest_doc: *mut XmlDoc,
    dest_parent: *mut XmlNode,
    deep: i32,
    _options: i32,
) -> i32 {
    let mut ret: i32 = 0;
    let mut cur: *mut XmlNode;
    let mut cur_elem: *mut XmlNode = null_mut();
    let mut ns_map: XmlNsMapPtr = null_mut();
    let mut mi: XmlNsMapItemPtr;
    let mut depth: i32 = -1;
    // let adoptStr: i32 = 1;
    // gather @parent's ns-decls.
    let mut parnsdone: i32 = 0;
    // @ancestorsOnly:
    // TODO: @ancestorsOnly should be set per option.
    let ancestors_only: i32 = 0;
    let mut result_clone: *mut XmlNode = null_mut();
    let mut clone: *mut XmlNode;
    let mut parent_clone: *mut XmlNode = null_mut();
    let mut prev_clone: *mut XmlNode = null_mut();

    if node.is_null() || res_node.is_null() || dest_doc.is_null() {
        return -1;
    }
    // TODO: Initially we support only element-nodes.
    if !matches!((*node).element_type(), XmlElementType::XmlElementNode) {
        return 1;
    }
    // Check (*node).doc sanity.
    if !(*node).doc.is_null() && !source_doc.is_null() && (*node).doc != source_doc {
        // Might be an XIncluded node.
        return -1;
    }
    if source_doc.is_null() {
        source_doc = (*node).doc;
    }
    if source_doc.is_null() {
        return -1;
    }

    // Reuse the namespace map of the context.
    if !ctxt.is_null() {
        ns_map = (*ctxt).namespace_map as _;
    }

    *res_node = null_mut();

    cur = node;
    if !cur.is_null() && matches!((*cur).element_type(), XmlElementType::XmlNamespaceDecl) {
        return -1;
    }

    'exit: {
        'internal_error: {
            'main: while !cur.is_null() {
                if (*cur).doc != source_doc {
                    // We'll assume XIncluded nodes if the doc differs.
                    // TODO: Do we need to reconciliate XIncluded nodes?
                    // TODO: This here returns -1 in this case.
                    break 'internal_error;
                }
                // Create a new node.
                match (*cur).element_type() {
                    XmlElementType::XmlXIncludeStart | XmlElementType::XmlXIncludeEnd => {
                        // TODO: What to do with XInclude?
                        break 'internal_error;
                    }
                    XmlElementType::XmlElementNode
                    | XmlElementType::XmlTextNode
                    | XmlElementType::XmlCDATASectionNode
                    | XmlElementType::XmlCommentNode
                    | XmlElementType::XmlPINode
                    | XmlElementType::XmlDocumentFragNode
                    | XmlElementType::XmlEntityRefNode
                    | XmlElementType::XmlEntityNode => {
                        // Nodes of xmlNode structure.
                        clone = xml_malloc(size_of::<XmlNode>()) as _;
                        if clone.is_null() {
                            xml_tree_err_memory("xmlDOMWrapCloneNode(): allocating a node");
                            break 'internal_error;
                        }
                        std::ptr::write(&mut *clone, XmlNode::default());
                        // Set hierarchical links.
                        if !result_clone.is_null() {
                            (*clone).set_parent(NodePtr::from_ptr(parent_clone));
                            if !prev_clone.is_null() {
                                (*prev_clone).next = NodePtr::from_ptr(clone);
                                (*clone).prev = NodePtr::from_ptr(prev_clone);
                            } else {
                                (*parent_clone).set_children(NodePtr::from_ptr(clone));
                            }
                        } else {
                            result_clone = clone;
                        }
                    }
                    XmlElementType::XmlAttributeNode => {
                        // Attributes (xmlAttr).
                        // Use xmlRealloc to avoid -Warray-bounds warning
                        clone = xml_realloc(null_mut(), size_of::<XmlAttr>()) as _;
                        if clone.is_null() {
                            xml_tree_err_memory("xmlDOMWrapCloneNode(): allocating an attr-node");
                            break 'internal_error;
                        }
                        std::ptr::write(&mut *(clone as *mut XmlAttr), XmlAttr::default());
                        // Set hierarchical links.
                        // TODO: Change this to add to the end of attributes.
                        if !result_clone.is_null() {
                            (*clone).set_parent(NodePtr::from_ptr(parent_clone));
                            if !prev_clone.is_null() {
                                (*prev_clone).next = NodePtr::from_ptr(clone);
                                (*clone).prev = NodePtr::from_ptr(prev_clone);
                            } else {
                                (*parent_clone).properties = clone as _;
                            }
                        } else {
                            result_clone = clone;
                        }
                    }
                    _ => {
                        // TODO QUESTION: Any other nodes expected?
                        break 'internal_error;
                    }
                }

                (*clone).typ = (*cur).element_type();
                (*clone).doc = dest_doc;

                // Clone the name of the node if any.
                if (*cur).name == XML_STRING_TEXT.as_ptr() as _ {
                    (*clone).name = XML_STRING_TEXT.as_ptr() as _;
                } else if (*cur).name == XML_STRING_TEXT_NOENC.as_ptr() as _ {
                    // NOTE: Although xmlStringTextNoenc is never assigned to a node
                    //   in tree.c, it might be set in Libxslt via
                    //   "xsl:disable-output-escaping".
                    (*clone).name = XML_STRING_TEXT_NOENC.as_ptr() as _;
                } else if (*cur).name == XML_STRING_COMMENT.as_ptr() as _ {
                    (*clone).name = XML_STRING_COMMENT.as_ptr() as _;
                } else if !(*cur).name.is_null() {
                    (*clone).name = xml_strdup((*cur).name);
                }

                let mut leave_node = false;
                match (*cur).element_type() {
                    XmlElementType::XmlXIncludeStart | XmlElementType::XmlXIncludeEnd => {
                        // TODO
                        return -1;
                    }
                    XmlElementType::XmlElementNode => {
                        cur_elem = cur;
                        depth += 1;
                        // Namespace declarations.
                        if !(*cur).ns_def.is_null() {
                            if parnsdone == 0 {
                                if !dest_parent.is_null() && ctxt.is_null() {
                                    // Gather @parent's in-scope ns-decls.
                                    if xml_dom_wrap_ns_norm_gather_in_scope_ns(
                                        &raw mut ns_map,
                                        dest_parent,
                                    ) == -1
                                    {
                                        break 'internal_error;
                                    }
                                }
                                parnsdone = 1;
                            }
                            // Clone namespace declarations.
                            let mut clone_ns_def_slot = None::<XmlNsPtr>;
                            let mut ns = XmlNsPtr::from_raw((*cur).ns_def).unwrap();
                            while let Some(now) = ns {
                                // Create a new xmlNs.
                                let Some(mut new) = XmlNsPtr::new(XmlNs {
                                    typ: XML_LOCAL_NAMESPACE,
                                    ..Default::default()
                                }) else {
                                    xml_tree_err_memory(
                                        "xmlDOMWrapCloneNode(): allocating namespace",
                                    );
                                    return -1;
                                };

                                if !now.href.is_null() {
                                    new.href = xml_strdup(now.href);
                                }
                                if now.prefix().is_some() {
                                    new.prefix = xml_strdup(now.prefix);
                                }

                                if let Some(mut last) = clone_ns_def_slot {
                                    last.next = new.as_ptr();
                                    clone_ns_def_slot = Some(new);
                                } else {
                                    clone_ns_def_slot = Some(new);
                                    (*clone).ns_def = new.as_ptr();
                                }

                                // Note that for custom handling of ns-references,
                                // the ns-decls need not be stored in the ns-map,
                                // since they won't be referenced by (*node).ns.
                                if ctxt.is_null() || (*ctxt).get_ns_for_node_func.is_none() {
                                    // Does it shadow any ns-decl?
                                    if XML_NSMAP_NOTEMPTY!(ns_map) {
                                        XML_NSMAP_FOREACH!(ns_map, mi, {
                                            if ((*mi).depth >= XML_TREE_NSMAP_PARENT)
                                                && (*mi).shadow_depth == -1
                                                && now.prefix() == (*(*mi).new_ns).prefix()
                                            {
                                                // Mark as shadowed at the current depth.
                                                (*mi).shadow_depth = depth;
                                            }
                                        });
                                    }
                                    // Push mapping.
                                    if xml_dom_wrap_ns_map_add_item(
                                        &raw mut ns_map,
                                        -1,
                                        Some(now),
                                        new.as_ptr(),
                                        depth,
                                    )
                                    .is_null()
                                    {
                                        break 'internal_error;
                                    }
                                }
                                ns = XmlNsPtr::from_raw(now.next).unwrap();
                            }
                        }
                        // (*cur).ns will be processed further down.
                    }
                    XmlElementType::XmlAttributeNode => {
                        // IDs will be processed further down.
                        // (*cur).ns will be processed further down.
                    }
                    XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode => {
                        // Note that this will also cover the values of attributes.
                        (*clone).content = xml_strdup((*cur).content);
                        // goto leave_node;
                        leave_node = true;
                    }
                    XmlElementType::XmlEntityNode => {
                        // TODO: What to do here?
                        // goto leave_node;
                        leave_node = true;
                    }
                    XmlElementType::XmlEntityRefNode => {
                        if source_doc != dest_doc {
                            if (*dest_doc).int_subset.is_some() || (*dest_doc).ext_subset.is_some()
                            {
                                // Different doc: Assign new entity-node if available.
                                let ent = xml_get_doc_entity(dest_doc, &(*cur).name().unwrap());
                                if let Some(ent) = ent {
                                    (*clone).content = ent.content.load(Ordering::Relaxed);
                                    (*clone).set_children(NodePtr::from_ptr(
                                        ent.as_ptr() as *mut XmlNode
                                    ));
                                    (*clone)
                                        .set_last(NodePtr::from_ptr(ent.as_ptr() as *mut XmlNode));
                                }
                            }
                        } else {
                            // Same doc: Use the current node's entity declaration and value.
                            (*clone).content = (*cur).content;
                            (*clone).set_children((*cur).children());
                            (*clone).set_last((*cur).last());
                        }
                        // goto leave_node;
                        leave_node = true;
                    }
                    XmlElementType::XmlPINode => {
                        (*clone).content = xml_strdup((*cur).content);
                        // goto leave_node;
                        leave_node = true;
                    }
                    XmlElementType::XmlCommentNode => {
                        (*clone).content = xml_strdup((*cur).content);
                        // goto leave_node;
                        leave_node = true;
                    }
                    _ => {
                        break 'internal_error;
                    }
                }

                if !leave_node {
                    if (*cur).ns.is_null() {
                        // goto end_ns_reference;
                    } else {
                        // handle_ns_reference:
                        // The following will take care of references to ns-decls
                        // and is intended only for element- and attribute-nodes.
                        //
                        if parnsdone == 0 {
                            if (!dest_parent.is_null() && ctxt.is_null())
                                && (xml_dom_wrap_ns_norm_gather_in_scope_ns(
                                    &raw mut ns_map,
                                    dest_parent,
                                ) == -1)
                            {
                                break 'internal_error;
                            }
                            parnsdone = 1;
                        }
                        // Adopt ns-references.
                        let mut end_ns_reference = false;
                        if XML_NSMAP_NOTEMPTY!(ns_map) {
                            // Search for a mapping.
                            XML_NSMAP_FOREACH!(ns_map, mi, {
                                if (*mi).shadow_depth == -1
                                    && XmlNsPtr::from_raw((*cur).ns).unwrap() == (*mi).old_ns
                                {
                                    // This is the nice case: a mapping was found.
                                    (*clone).ns = (*mi).new_ns;
                                    // goto end_ns_reference;
                                    end_ns_reference = true;
                                    break;
                                }
                            });
                        }

                        if !end_ns_reference {
                            // No matching namespace in scope. We need a new one.
                            if !ctxt.is_null() && (*ctxt).get_ns_for_node_func.is_some() {
                                // User-defined behaviour.
                                let ns = ((*ctxt).get_ns_for_node_func.unwrap())(
                                    ctxt,
                                    cur,
                                    (*(*cur).ns).href,
                                    (*(*cur).ns).prefix,
                                );
                                // Add user's mapping.
                                if xml_dom_wrap_ns_map_add_item(
                                    &raw mut ns_map,
                                    -1,
                                    XmlNsPtr::from_raw((*cur).ns).unwrap(),
                                    ns,
                                    XML_TREE_NSMAP_CUSTOM,
                                )
                                .is_null()
                                {
                                    break 'internal_error;
                                }
                                (*clone).ns = ns;
                            } else {
                                // Acquire a normalized ns-decl and add it to the map.
                                let mut ns = null_mut();
                                if xml_dom_wrap_ns_norm_acquire_normalized_ns(
                                    dest_doc,
                                    // ns-decls on curElem or on (*destDoc).oldNs
                                    if !dest_parent.is_null() {
                                        cur_elem
                                    } else {
                                        null_mut()
                                    },
                                    (*cur).ns,
                                    &mut ns,
                                    &raw mut ns_map,
                                    depth,
                                    // if we need to search only in the ancestor-axis
                                    ancestors_only,
                                    // ns-decls must be prefixed for attributes.
                                    matches!(
                                        (*cur).element_type(),
                                        XmlElementType::XmlAttributeNode
                                    ) as i32,
                                ) == -1
                                {
                                    break 'internal_error;
                                }
                                (*clone).ns = ns;
                            }
                        }
                    }

                    // end_ns_reference:

                    // Some post-processing.
                    //
                    // Handle ID attributes.
                    if matches!((*clone).element_type(), XmlElementType::XmlAttributeNode)
                        && (*clone).parent().is_some()
                        && xml_is_id(dest_doc, (*clone).parent().unwrap().as_ptr(), clone as _) != 0
                    {
                        let children = (*cur).children();
                        if let Some(id_val) = children.and_then(|c| c.get_string((*cur).doc, 1)) {
                            if xml_add_id(null_mut(), dest_doc, &id_val, cur as _).is_none() {
                                // TODO: error message.
                                break 'internal_error;
                            }
                        }
                    }
                    // The following will traverse the tree **************************
                    //
                    // Walk the element's attributes before descending into child-nodes.
                    if matches!((*cur).element_type(), XmlElementType::XmlElementNode)
                        && !(*cur).properties.is_null()
                    {
                        prev_clone = null_mut();
                        parent_clone = clone;
                        cur = (*cur).properties as _;
                        continue 'main;
                    }
                    // into_content:
                    // Descend into child-nodes.
                    if let Some(children) = (*cur).children().filter(|_| {
                        deep != 0
                            || matches!((*cur).element_type(), XmlElementType::XmlAttributeNode)
                    }) {
                        prev_clone = null_mut();
                        parent_clone = clone;
                        cur = children.as_ptr();
                        continue 'main;
                    }
                }

                // leave_node:
                'leave_node: loop {
                    // At this point we are done with the node, its content
                    // and an element-nodes's attribute-nodes.
                    if cur == node {
                        break 'main;
                    }
                    if matches!((*cur).element_type(), XmlElementType::XmlElementNode)
                        || matches!((*cur).element_type(), XmlElementType::XmlXIncludeStart)
                        || matches!((*cur).element_type(), XmlElementType::XmlXIncludeEnd)
                    {
                        // TODO: Do we expect nsDefs on xmlElementType::XML_XINCLUDE_START?
                        if XML_NSMAP_NOTEMPTY!(ns_map) {
                            // Pop mappings.
                            while !(*ns_map).last.is_null() && (*(*ns_map).last).depth >= depth {
                                XML_NSMAP_POP!(ns_map, mi);
                            }
                            // Unshadow.
                            XML_NSMAP_FOREACH!(ns_map, mi, {
                                if (*mi).shadow_depth >= depth {
                                    (*mi).shadow_depth = -1;
                                }
                            });
                        }
                        depth -= 1;
                    }
                    if let Some(next) = (*cur).next {
                        prev_clone = clone;
                        cur = next.as_ptr();
                    } else if !matches!((*cur).element_type(), XmlElementType::XmlAttributeNode) {
                        // Set (*clone).last.
                        if let Some(mut parent) = (*clone).parent() {
                            parent.set_last(NodePtr::from_ptr(clone));
                        }
                        clone = (*clone).parent().map_or(null_mut(), |p| p.as_ptr());
                        if !clone.is_null() {
                            parent_clone = (*clone).parent().map_or(null_mut(), |p| p.as_ptr());
                        }
                        // Process parent --> next;
                        cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
                        // goto leave_node;
                        continue 'leave_node;
                    } else {
                        // This is for attributes only.
                        clone = (*clone).parent().map_or(null_mut(), |p| p.as_ptr());
                        parent_clone = (*clone).parent().map_or(null_mut(), |p| p.as_ptr());
                        // Process parent-element --> children.
                        cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
                        // goto into_content;
                        // Descend into child-nodes.
                        if let Some(children) = (*cur).children().filter(|_| {
                            deep != 0
                                || matches!((*cur).element_type(), XmlElementType::XmlAttributeNode)
                        }) {
                            prev_clone = null_mut();
                            parent_clone = clone;
                            cur = children.as_ptr();
                            continue 'main;
                        }
                        continue 'leave_node;
                    }

                    break;
                }
            }
            break 'exit;
        }

        // internal_error:
        ret = -1;
    }
    // exit:
    // Cleanup.
    if !ns_map.is_null() {
        if !ctxt.is_null() && (*ctxt).namespace_map == ns_map as _ {
            // Just cleanup the map but don't free.
            if !(*ns_map).first.is_null() {
                if !(*ns_map).pool.is_null() {
                    (*(*ns_map).last).next = (*ns_map).pool;
                }
                (*ns_map).pool = (*ns_map).first;
                (*ns_map).first = null_mut();
            }
        } else {
            xml_dom_wrap_ns_map_free(ns_map);
        }
    }
    // TODO: Should we try a cleanup of the cloned node in case of a fatal error?
    *res_node = result_clone;
    ret
}
