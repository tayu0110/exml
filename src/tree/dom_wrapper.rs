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
    globals::{xml_free, xml_malloc},
    parser_internals::{XML_STRING_COMMENT, XML_STRING_TEXT, XML_STRING_TEXT_NOENC},
    valid::{xml_add_id, xml_is_id, xml_remove_id},
    xmlstring::{xml_str_equal, xml_strdup, XmlChar},
};

use super::{
    xml_free_ns, xml_get_doc_entity, xml_new_ns, xml_search_ns_by_namespace_strict,
    xml_search_ns_by_prefix_strict, xml_tree_err_memory, xml_tree_nslist_lookup_by_prefix,
    NodeCommon, NodePtr, XmlAttr, XmlAttrPtr, XmlAttributeType, XmlDoc, XmlDocPtr, XmlElementType,
    XmlGenericNodePtr, XmlNode, XmlNodePtr, XmlNs, XmlNsPtr, XML_LOCAL_NAMESPACE,
};

/// A function called to acquire namespaces (xmlNs) from the wrapper.
///
/// Returns an xmlNsPtr or NULL in case of an error.
#[doc(alias = "xmlDOMWrapAcquireNsFunction")]
pub type XmlDOMWrapAcquireNsFunction = unsafe fn(
    ctxt: XmlDOMWrapCtxtPtr,
    node: Option<XmlGenericNodePtr>,
    ns_name: *const XmlChar,
    ns_prefix: *const XmlChar,
) -> Option<XmlNsPtr>;

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
    new_ns: Option<XmlNsPtr>, /* new ns decl reference */
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
            new_ns: None,
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
    new_ns: Option<XmlNsPtr>,
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
    node: Option<XmlGenericNodePtr>,
) -> i32 {
    let mut mi: XmlNsMapItemPtr;
    let mut shadowed: i32;

    if map.is_null() || !(*map).is_null() {
        return -1;
    }
    // In original libxml2, check if element type is not XmlNamespaceDecl.
    let Some(node) = node else {
        return -1;
    };
    // Get in-scope ns-decls of @parent.
    let mut cur = Some(node);
    while let Some(cur_node) = cur.filter(|&cur| Some(cur) != cur.document().map(|doc| doc.into()))
    {
        if let Ok(cur_node) = XmlNodePtr::try_from(cur_node) {
            if matches!(cur_node.element_type(), XmlElementType::XmlElementNode)
                && cur_node.ns_def.is_some()
            {
                let mut ns = cur_node.ns_def;
                while let Some(now) = ns {
                    shadowed = 0;
                    if XML_NSMAP_NOTEMPTY!(*map) {
                        // Skip shadowed prefixes.
                        XML_NSMAP_FOREACH!(*map, mi, {
                            if now.prefix() == (*mi).new_ns.unwrap().prefix() {
                                shadowed = 1;
                                break;
                            }
                        });
                    }
                    // Insert mapping.
                    mi = xml_dom_wrap_ns_map_add_item(
                        map,
                        0,
                        None,
                        Some(now),
                        XML_TREE_NSMAP_PARENT,
                    );
                    if mi.is_null() {
                        return -1;
                    }
                    if shadowed != 0 {
                        (*mi).shadow_depth = 0;
                    }
                    ns = now.next;
                }
            }
        }
        cur = cur_node
            .parent()
            .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()));
    }
    0
}

/// For internal use. Adds a ns-decl mapping.
///
/// Returns 0 on success, -1 on internal errors.
#[doc(alias = "xmlDOMWrapNSNormAddNsMapItem2")]
unsafe fn xml_dom_wrap_ns_norm_add_ns_map_item2(
    list: &mut Vec<XmlNsPtr>,
    // size: *mut i32,
    number: *mut i32,
    old_ns: XmlNsPtr,
    new_ns: XmlNsPtr,
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
    mut doc: XmlDocPtr,
    ns_name: *const XmlChar,
    prefix: Option<&str>,
) -> Option<XmlNsPtr> {
    let mut ns = doc.ensure_xmldecl()?;
    if let Some(next) = ns.next {
        if next.prefix().as_deref() == prefix && xml_str_equal(next.href, ns_name) {
            return Some(next);
        }
        ns = next;
        while let Some(next) = ns.next {
            if next.prefix().as_deref() == prefix && xml_str_equal(next.href, ns_name) {
                return Some(next);
            }
            ns = next;
        }
    }
    // Create.
    ns.next = xml_new_ns(None, ns_name, prefix);
    ns.next
}

/// Declares a new namespace on @elem. It tries to use the
/// given @prefix; if a ns-decl with the given prefix is already existent
/// on @elem, it will generate an other prefix.
///
/// Returns 1 if a ns-decl was found, 0 if not and -1 on API and internal errors.
#[doc(alias = "xmlDOMWrapNSNormDeclareNsForced")]
unsafe fn xml_dom_wrap_nsnorm_declare_ns_forced(
    doc: XmlDocPtr,
    mut elem: XmlNodePtr,
    ns_name: *const XmlChar,
    prefix: *const XmlChar,
    check_shadow: i32,
) -> Option<XmlNsPtr> {
    let mut buf: [i8; 50] = [0; 50];
    let mut pref: *const XmlChar;
    let mut counter: i32 = 0;

    // let doc = doc?;
    // let elem = elem?;
    if !matches!(elem.element_type(), XmlElementType::XmlElementNode) {
        return None;
    }
    // Create a ns-decl on @anchor.
    pref = prefix;
    loop {
        // Lookup whether the prefix is unused in elem's ns-decls.
        if elem.ns_def.is_some() && xml_tree_nslist_lookup_by_prefix(elem.ns_def, pref).is_some() {
            // goto ns_next_prefix;
        } else {
            // Does it shadow ancestor ns-decls?
            if check_shadow != 0
                && elem
                    .parent()
                    .filter(|p| {
                        p.doc.map_or(null_mut(), |doc| doc.as_ptr()) != p.as_ptr() as *mut XmlDoc
                            && xml_search_ns_by_prefix_strict(
                                doc,
                                XmlGenericNodePtr::from_raw(p.as_ptr()).unwrap(),
                                pref,
                                None,
                            ) == 1
                    })
                    .is_some()
            {
                // goto ns_next_prefix;
            } else {
                let ret = xml_new_ns(
                    None,
                    ns_name,
                    (!pref.is_null())
                        .then(|| CStr::from_ptr(pref as *const i8).to_string_lossy())
                        .as_deref(),
                )?;
                if let Some(ns_def) = elem.ns_def {
                    let mut ns2 = ns_def;
                    while let Some(next) = ns2.next {
                        ns2 = next;
                    }
                    ns2.next = Some(ret);
                } else {
                    elem.ns_def = Some(ret);
                }
                return Some(ret);
            }
        }
        // ns_next_prefix:
        counter += 1;
        if counter > 1000 {
            return None;
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
    mut doc: XmlDocPtr,
    elem: Option<XmlNodePtr>,
    ns: XmlNsPtr,
    ret_ns: &mut Option<XmlNsPtr>,
    ns_map: *mut XmlNsMapPtr,
    depth: i32,
    ancestors_only: i32,
    prefixed: i32,
) -> i32 {
    let mut mi: XmlNsMapItemPtr;

    if ns_map.is_null() {
        return -1;
    }

    *ret_ns = None;
    // Handle XML namespace.
    if (*ns).prefix().as_deref() == Some("xml") {
        // Insert XML namespace mapping.
        *ret_ns = doc.ensure_xmldecl();
        if (*ret_ns).is_none() {
            return -1;
        }
        return 0;
    }
    // If the search should be done in ancestors only and no
    // @elem (the first ancestor) was specified, then skip the search.
    if XML_NSMAP_NOTEMPTY!(*ns_map) && !(ancestors_only != 0 && elem.is_none()) {
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
		        (!(*mi).new_ns.unwrap().href.is_null() &&
                *(*mi).new_ns.unwrap().href.add(0) != 0) &&
		        // Ensure a prefix if wanted.
		        (prefixed == 0 || (*mi).new_ns.unwrap().prefix().is_some()) &&
		        // Equal ns name
		        ((*mi).new_ns.unwrap().href == ns.href ||
                xml_str_equal((*mi).new_ns.unwrap().href, ns.href) )
            {
                // Set the mapping.
                (*mi).old_ns = Some(ns);
                *ret_ns = (*mi).new_ns;
                return 0;
            }
        });
    }
    // No luck, the namespace is out of scope or shadowed.
    if let Some(elem) = elem {
        let Some(tmpns) = xml_dom_wrap_nsnorm_declare_ns_forced(doc, elem, ns.href, ns.prefix, 0)
        else {
            return -1;
        };

        if !(*ns_map).is_null() {
            // Does it shadow ancestor ns-decls?
            XML_NSMAP_FOREACH!(*ns_map, mi, {
                if ((*mi).depth < depth)
                    && (*mi).shadow_depth == -1
                    && ns.prefix() == (*mi).new_ns.unwrap().prefix()
                {
                    // Shadows.
                    (*mi).shadow_depth = depth;
                    break;
                }
            });
        }
        if xml_dom_wrap_ns_map_add_item(ns_map, -1, Some(ns), Some(tmpns), depth).is_null() {
            xml_free_ns(tmpns);
            return -1;
        }
        *ret_ns = Some(tmpns);
    } else {
        // Store ns-decls in "oldNs" of the document-node.
        let Some(tmpns) = xml_dom_wrap_store_ns(doc, ns.href, ns.prefix().as_deref()) else {
            return -1;
        };
        // Insert mapping.
        if xml_dom_wrap_ns_map_add_item(ns_map, -1, Some(ns), Some(tmpns), XML_TREE_NSMAP_DOC)
            .is_null()
        {
            xml_free_ns(tmpns);
            return -1;
        }
        *ret_ns = Some(tmpns);
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
    elem: XmlNodePtr,
    options: i32,
) -> i32 {
    let mut depth: i32 = -1;
    let mut adoptns: i32;
    let mut parnsdone: i32 = 0;
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

    // if elem.is_null() {
    //     return -1;
    // }
    if !matches!(elem.element_type(), XmlElementType::XmlElementNode) {
        return -1;
    }

    let Some(doc) = elem.doc else {
        return -1;
    };
    let ret;
    let mut cur = XmlGenericNodePtr::from(elem);
    let mut cur_elem = None;
    'exit: {
        'internal_error: {
            'main: loop {
                match cur.element_type() {
                    XmlElementType::XmlElementNode => {
                        let mut cur_node = XmlNodePtr::try_from(cur).unwrap();
                        adoptns = 1;
                        cur_elem = Some(cur_node);
                        depth += 1;
                        // Namespace declarations.
                        if cur_node.ns_def.is_some() {
                            let mut prevns = None::<XmlNsPtr>;
                            let mut ns = cur_node.ns_def;
                            'b: while let Some(cur_ns) = ns {
                                if parnsdone == 0 {
                                    if let Some(parent) = elem.parent().filter(|p| {
                                        p.doc.map_or(null_mut(), |doc| doc.as_ptr())
                                            != p.as_ptr() as *mut XmlDoc
                                    }) {
                                        // Gather ancestor in-scope ns-decls.
                                        if xml_dom_wrap_ns_norm_gather_in_scope_ns(
                                            &raw mut ns_map,
                                            XmlGenericNodePtr::from_raw(parent.as_ptr()),
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
                                            && cur_ns.prefix() == (*mi).new_ns.unwrap().prefix()
                                            && cur_ns.href() == (*mi).new_ns.unwrap().href()
                                        {
                                            // A redundant ns-decl was found.
                                            // Add it to the list of redundant ns-decls.
                                            if xml_dom_wrap_ns_norm_add_ns_map_item2(
                                                &mut list_redund,
                                                // &raw mut size_redund,
                                                &raw mut nb_redund,
                                                cur_ns,
                                                (*mi).new_ns.unwrap(),
                                            ) == -1
                                            {
                                                break 'internal_error;
                                            }
                                            // Remove the ns-decl from the element-node.
                                            if let Some(mut prevns) = prevns {
                                                prevns.next = cur_ns.next;
                                            } else {
                                                cur_node.ns_def = cur_ns.next;
                                            }
                                            // goto next_ns_decl;
                                            ns = cur_ns.next;
                                            continue 'b;
                                        }
                                    });
                                }

                                // Skip ns-references handling if the referenced
                                // ns-decl is declared on the same element.
                                if cur_node.ns.is_some()
                                    && adoptns != 0
                                    && cur_node.ns == Some(cur_ns)
                                {
                                    adoptns = 0;
                                }
                                // Does it shadow any ns-decl?
                                if XML_NSMAP_NOTEMPTY!(ns_map) {
                                    XML_NSMAP_FOREACH!(ns_map, mi, {
                                        if (*mi).depth >= XML_TREE_NSMAP_PARENT
                                            && (*mi).shadow_depth == -1
                                            && cur_ns.prefix() == (*mi).new_ns.unwrap().prefix()
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
                                    Some(cur_ns),
                                    depth,
                                )
                                .is_null()
                                {
                                    break 'internal_error;
                                }

                                prevns = Some(cur_ns);
                                // next_ns_decl:
                                ns = cur_ns.next;
                            }
                        }
                        if adoptns == 0 {
                            // goto ns_end;
                            if matches!(cur_node.element_type(), XmlElementType::XmlElementNode) {
                                if let Some(prop) = cur_node.properties {
                                    // Process attributes.
                                    cur = prop.into();
                                    continue 'main;
                                }
                            }
                        }

                        // No ns, no fun.
                        if cur_node.ns.is_none() {
                            // goto ns_end;
                            if matches!(cur_node.element_type(), XmlElementType::XmlElementNode) {
                                if let Some(prop) = cur_node.properties {
                                    // Process attributes.
                                    cur = prop.into();
                                    continue 'main;
                                }
                            }
                        }

                        if parnsdone == 0 {
                            if (*elem)
                                .parent()
                                .filter(|p| {
                                    p.doc.map_or(null_mut(), |doc| doc.as_ptr())
                                        != p.as_ptr() as *mut XmlDoc
                                        && xml_dom_wrap_ns_norm_gather_in_scope_ns(
                                            &raw mut ns_map,
                                            XmlGenericNodePtr::from_raw(p.as_ptr()),
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
                            if cur_node.ns == Some(list_redund[j]) {
                                cur_node.ns = Some(list_redund[j + 1]);
                                break;
                            }
                        }
                        // Adopt ns-references.
                        if XML_NSMAP_NOTEMPTY!(ns_map) {
                            // Search for a mapping.
                            XML_NSMAP_FOREACH!(ns_map, mi, {
                                if (*mi).shadow_depth == -1 && cur_node.ns == (*mi).old_ns {
                                    cur_node.ns = (*mi).new_ns;
                                    // goto ns_end;
                                    if matches!(cur.element_type(), XmlElementType::XmlElementNode)
                                    {
                                        if let Some(prop) = cur_node.properties {
                                            // Process attributes.
                                            cur = prop.into();
                                            continue 'main;
                                        }
                                    }
                                }
                            });
                        }
                        // Acquire a normalized ns-decl and add it to the map.
                        let mut ns = None;
                        if xml_dom_wrap_ns_norm_acquire_normalized_ns(
                            doc,
                            cur_elem,
                            cur_node.ns.unwrap(),
                            &mut ns,
                            &raw mut ns_map,
                            depth,
                            ancestors_only,
                            matches!(cur.element_type(), XmlElementType::XmlAttributeNode) as i32,
                        ) == -1
                        {
                            break 'internal_error;
                        }
                        cur_node.ns = ns;

                        // ns_end:
                        if matches!(cur.element_type(), XmlElementType::XmlElementNode) {
                            if let Some(prop) = cur_node.properties {
                                // Process attributes.
                                cur = prop.into();
                                continue 'main;
                            }
                        }
                    }
                    XmlElementType::XmlAttributeNode => {
                        let mut cur_attr = XmlAttrPtr::try_from(cur).unwrap();
                        if parnsdone == 0 {
                            if elem
                                .parent()
                                .filter(|p| {
                                    p.doc.map_or(null_mut(), |doc| doc.as_ptr())
                                        != p.as_ptr() as *mut XmlDoc
                                        && xml_dom_wrap_ns_norm_gather_in_scope_ns(
                                            &raw mut ns_map,
                                            XmlGenericNodePtr::from_raw(p.as_ptr()),
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
                            if cur_attr.ns == Some(list_redund[j]) {
                                cur_attr.ns = Some(list_redund[j + 1]);
                                break;
                            }
                        }
                        // Adopt ns-references.
                        if XML_NSMAP_NOTEMPTY!(ns_map) {
                            // Search for a mapping.
                            XML_NSMAP_FOREACH!(ns_map, mi, {
                                if (*mi).shadow_depth == -1 && cur_attr.ns == (*mi).old_ns {
                                    cur_attr.ns = (*mi).new_ns;
                                }
                            });
                        }
                        // Acquire a normalized ns-decl and add it to the map.
                        let mut ns = None;
                        if xml_dom_wrap_ns_norm_acquire_normalized_ns(
                            doc,
                            cur_elem,
                            cur_attr.ns.unwrap(),
                            &mut ns,
                            &raw mut ns_map,
                            depth,
                            ancestors_only,
                            matches!(cur.element_type(), XmlElementType::XmlAttributeNode) as i32,
                        ) == -1
                        {
                            break 'internal_error;
                        }
                        cur_attr.ns = ns;
                    }
                    _ => {
                        // goto next_sibling;
                        'next_sibling: loop {
                            if cur == elem.into() {
                                break 'main;
                            }
                            if matches!(cur.element_type(), XmlElementType::XmlElementNode) {
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
                            if let Some(next) = cur
                                .next()
                                .and_then(|next| XmlGenericNodePtr::from_raw(next.as_ptr()))
                            {
                                cur = next;
                            } else {
                                if matches!(cur.element_type(), XmlElementType::XmlAttributeNode) {
                                    cur = cur
                                        .parent()
                                        .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()))
                                        .unwrap();
                                    // goto into_content;
                                    break 'next_sibling;
                                }
                                cur = cur
                                    .parent()
                                    .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()))
                                    .unwrap();
                                // goto next_sibling;
                                continue 'next_sibling;
                            }

                            continue 'main;
                        }
                    }
                }
                // into_content:
                'into_content: loop {
                    if let Some(children) = cur
                        .children()
                        .filter(|_| matches!(cur.element_type(), XmlElementType::XmlElementNode))
                        .and_then(|c| XmlGenericNodePtr::from_raw(c.as_ptr()))
                    {
                        // Process content of element-nodes only.
                        cur = children;
                        continue;
                    }
                    // next_sibling:
                    'next_sibling: loop {
                        if cur == elem.into() {
                            break 'main;
                        }
                        if matches!(cur.element_type(), XmlElementType::XmlElementNode) {
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
                        if let Some(next) = cur
                            .next()
                            .and_then(|next| XmlGenericNodePtr::from_raw(next.as_ptr()))
                        {
                            cur = next;
                        } else {
                            if matches!(cur.element_type(), XmlElementType::XmlAttributeNode) {
                                cur = cur
                                    .parent()
                                    .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()))
                                    .unwrap();
                                // goto into_content;
                                continue 'into_content;
                            }
                            cur = cur
                                .parent()
                                .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()))
                                .unwrap();
                            // goto next_sibling;
                            continue 'next_sibling;
                        }
                        break 'next_sibling;
                    }

                    break 'into_content;
                }
            }

            ret = 0;
            break 'exit;
        }
        // internal_error:
        ret = -1;
    }
    // exit:
    for (_, j) in (0..nb_redund).zip((0..).step_by(2)) {
        xml_free_ns(list_redund[j]);
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
    source_doc: Option<XmlDocPtr>,
    node: XmlNodePtr,
    dest_doc: XmlDocPtr,
    dest_parent: Option<XmlNodePtr>,
    _options: i32,
) -> i32 {
    let mut ret: i32 = 0;
    let mut cur_elem = None;
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
    if dest_parent.is_none() || (!ctxt.is_null() && (*ctxt).get_ns_for_node_func.is_some()) {
        parnsdone = 1;
    } else {
        parnsdone = 0;
    }

    'exit: {
        'internal_error: {
            let mut cur = Some(XmlGenericNodePtr::from(node));

            'main: while let Some(mut cur_node) = cur {
                let mut leave_node = false;

                // Paranoid source-doc sanity check.
                if cur_node.document() != source_doc {
                    // We'll assume XIncluded nodes if the doc differs.
                    // TODO: Do we need to reconciliate XIncluded nodes?
                    // This here skips XIncluded nodes and tries to handle
                    // broken sequences.
                    if cur_node.next().is_some() {
                        let mut next = cur_node.next();
                        while let Some(now) = next.filter(|now| {
                            !matches!(now.element_type(), XmlElementType::XmlXIncludeEnd)
                                && now.doc != node.doc
                        }) {
                            cur_node = XmlGenericNodePtr::from_raw(now.as_ptr()).unwrap();
                            next = now.next;
                        }

                        if cur_node.document() != node.doc {
                            // goto leave_node;
                            leave_node = true;
                        }
                    } else {
                        // goto leave_node;
                        leave_node = true;
                    }
                }

                if !leave_node {
                    cur_node.set_document(Some(dest_doc));
                    match cur_node.element_type() {
                        XmlElementType::XmlXIncludeStart | XmlElementType::XmlXIncludeEnd => {
                            // TODO
                            return -1;
                        }
                        XmlElementType::XmlElementNode => {
                            let mut node = XmlNodePtr::try_from(cur_node).unwrap();
                            cur_elem = Some(node);
                            depth += 1;
                            // Namespace declarations.
                            // - (*ns).href and (*ns).prefix are never in the dict, so
                            //   we need not move the values over to the destination dict.
                            // - Note that for custom handling of ns-references,
                            //   the ns-decls need not be stored in the ns-map,
                            //   since they won't be referenced by (*node).ns.
                            if node.ns_def.is_some()
                                && (ctxt.is_null() || (*ctxt).get_ns_for_node_func.is_none())
                            {
                                if parnsdone == 0 {
                                    // Gather @parent's in-scope ns-decls.
                                    if xml_dom_wrap_ns_norm_gather_in_scope_ns(
                                        &raw mut ns_map,
                                        dest_parent.map(|d| d.into()),
                                    ) == -1
                                    {
                                        break 'internal_error;
                                    }

                                    parnsdone = 1;
                                }
                                let mut ns = node.ns_def;
                                while let Some(now) = ns {
                                    // Does it shadow any ns-decl?
                                    if XML_NSMAP_NOTEMPTY!(ns_map) {
                                        XML_NSMAP_FOREACH!(ns_map, mi, {
                                            if (*mi).depth >= XML_TREE_NSMAP_PARENT
                                                && (*mi).shadow_depth == -1
                                                && now.prefix() == (*mi).new_ns.unwrap().prefix()
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
                                        Some(now),
                                        depth,
                                    )
                                    .is_null()
                                    {
                                        break 'internal_error;
                                    }
                                    ns = now.next;
                                }
                            }
                            // No namespace, no fun.
                            if node.ns.is_some() {
                                if parnsdone == 0 {
                                    if xml_dom_wrap_ns_norm_gather_in_scope_ns(
                                        &raw mut ns_map,
                                        dest_parent.map(|d| d.into()),
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
                                        if (*mi).shadow_depth == -1 && node.ns == (*mi).old_ns {
                                            node.ns = (*mi).new_ns;
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
                                            Some(node.into()),
                                            node.ns.unwrap().href,
                                            node.ns.unwrap().prefix,
                                        );
                                        // Insert mapping if ns is available; it's the users fault
                                        // if not.
                                        if xml_dom_wrap_ns_map_add_item(
                                            &raw mut ns_map,
                                            -1,
                                            node.ns,
                                            ns,
                                            XML_TREE_NSMAP_CUSTOM,
                                        )
                                        .is_null()
                                        {
                                            break 'internal_error;
                                        }
                                        node.ns = ns;
                                    } else {
                                        // Acquire a normalized ns-decl and add it to the map.
                                        let mut ns = None;
                                        if xml_dom_wrap_ns_norm_acquire_normalized_ns(
                                            dest_doc,
                                            // ns-decls on curElem or on (*destDoc).oldNs
                                            dest_parent.and(cur_elem),
                                            node.ns.unwrap(),
                                            &mut ns,
                                            &raw mut ns_map,
                                            depth,
                                            ancestors_only,
                                            // ns-decls must be prefixed for attributes.
                                            matches!(
                                                node.element_type(),
                                                XmlElementType::XmlAttributeNode
                                            ) as i32,
                                        ) == -1
                                        {
                                            break 'internal_error;
                                        }
                                        node.ns = ns;
                                    }
                                }
                            }
                            // ns_end:
                            // Further node properties.
                            // TODO: Is this all?
                            node.psvi = null_mut();
                            node.line = 0;
                            node.extra = 0;
                            // Walk attributes.
                            if let Some(prop) = node.properties {
                                // Process first attribute node.
                                cur = Some(prop.into());
                                continue;
                            }
                        }
                        XmlElementType::XmlAttributeNode => {
                            let mut attr = XmlAttrPtr::try_from(cur_node).unwrap();
                            // No namespace, no fun.
                            if attr.ns.is_some() {
                                if parnsdone == 0 {
                                    if xml_dom_wrap_ns_norm_gather_in_scope_ns(
                                        &raw mut ns_map,
                                        dest_parent.map(|d| d.into()),
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
                                        if (*mi).shadow_depth == -1 && attr.ns == (*mi).old_ns {
                                            attr.ns = (*mi).new_ns;
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
                                            Some(attr.into()),
                                            attr.ns.unwrap().href,
                                            attr.ns.unwrap().prefix,
                                        );
                                        // Insert mapping if ns is available; it's the users fault
                                        // if not.
                                        if xml_dom_wrap_ns_map_add_item(
                                            &raw mut ns_map,
                                            -1,
                                            attr.ns,
                                            ns,
                                            XML_TREE_NSMAP_CUSTOM,
                                        )
                                        .is_null()
                                        {
                                            break 'internal_error;
                                        }
                                        attr.ns = ns;
                                    } else {
                                        // Acquire a normalized ns-decl and add it to the map.
                                        let mut ns = None;
                                        if xml_dom_wrap_ns_norm_acquire_normalized_ns(
                                            dest_doc,
                                            // ns-decls on curElem or on (*destDoc).oldNs
                                            dest_parent.and(cur_elem),
                                            attr.ns.unwrap(),
                                            &mut ns,
                                            &raw mut ns_map,
                                            depth,
                                            ancestors_only,
                                            // ns-decls must be prefixed for attributes.
                                            matches!(
                                                attr.element_type(),
                                                XmlElementType::XmlAttributeNode
                                            ) as i32,
                                        ) == -1
                                        {
                                            break 'internal_error;
                                        }
                                        attr.ns = ns;
                                    }
                                }
                            }
                            // Further node properties.
                            // TODO: Is this all?
                            // Attributes.
                            if let Some(source_doc) = source_doc.filter(|_| {
                                matches!(attr.atype, Some(XmlAttributeType::XmlAttributeID))
                            }) {
                                xml_remove_id(source_doc, attr);
                            }
                            attr.atype = None;
                            attr.psvi = null_mut();
                        }
                        XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode => {
                            // goto leave_node;
                            leave_node = true;
                        }
                        XmlElementType::XmlEntityRefNode => {
                            let mut cur = XmlNodePtr::try_from(cur_node).unwrap();
                            // Remove reference to the entity-node.
                            cur.content = null_mut();
                            cur.set_children(None);
                            cur.set_last(None);
                            if dest_doc.int_subset.is_some() || dest_doc.ext_subset.is_some() {
                                // Assign new entity-node if available.
                                let ent = xml_get_doc_entity(Some(dest_doc), &cur.name().unwrap());
                                if let Some(ent) = ent {
                                    cur.content = ent.content.load(Ordering::Relaxed);
                                    cur.set_children(NodePtr::from_ptr(
                                        ent.as_ptr() as *mut XmlNode
                                    ));
                                    cur.set_last(NodePtr::from_ptr(ent.as_ptr() as *mut XmlNode));
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
                        if let Some(children) = cur_node.children() {
                            cur = XmlGenericNodePtr::from_raw(children.as_ptr());
                            continue;
                        }
                    }
                }

                // leave_node:
                'leave_node: loop {
                    if cur_node == node.into() {
                        break 'main;
                    }
                    if matches!(
                        cur_node.element_type(),
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
                    if let Some(next) = cur_node.next() {
                        cur_node = XmlGenericNodePtr::from_raw(next.as_ptr()).unwrap();
                    } else if let Some(children) = cur_node.parent().and_then(|p| {
                        p.children().filter(|_| {
                            matches!(cur_node.element_type(), XmlElementType::XmlAttributeNode)
                        })
                    }) {
                        cur_node = XmlGenericNodePtr::from_raw(children.as_ptr()).unwrap();
                    } else {
                        cur_node = cur_node
                            .parent()
                            .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()))
                            .unwrap();
                        // goto leave_node;
                        continue 'leave_node;
                    }

                    cur = Some(cur_node);
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
    _source_doc: Option<XmlDocPtr>,
    mut attr: XmlAttrPtr,
    mut dest_doc: XmlDocPtr,
    dest_parent: Option<XmlNodePtr>,
    _options: i32,
) -> i32 {
    attr.doc = Some(dest_doc);
    if let Some(attr_ns) = attr.ns {
        let mut ns = None;

        if !ctxt.is_null() { /* TODO: User defined. */ }
        // XML Namespace.
        if attr_ns.prefix().as_deref() == Some("xml") {
            ns = dest_doc.ensure_xmldecl();
        } else if let Some(dest_parent) = dest_parent {
            // Declare on @destParent.
            if xml_search_ns_by_namespace_strict(
                dest_doc,
                dest_parent.into(),
                attr_ns.href,
                &mut ns,
                1,
            ) == -1
            {
                // goto internal_error;
                return -1;
            }
            if ns.is_none() {
                ns = xml_dom_wrap_nsnorm_declare_ns_forced(
                    dest_doc,
                    dest_parent,
                    attr_ns.href,
                    attr_ns.prefix,
                    1,
                );
            }
        } else {
            // Store in @(*destDoc).oldNs.
            ns = xml_dom_wrap_store_ns(dest_doc, attr_ns.href, attr_ns.prefix().as_deref());
        }
        let Some(ns) = ns else {
            // goto internal_error;
            return -1;
        };
        attr.ns = Some(ns);
    }

    attr.atype = None;
    attr.psvi = null_mut();
    // Walk content.
    let Some(children) = attr.children else {
        return 0;
    };
    if matches!(children.element_type(), XmlElementType::XmlNamespaceDecl) {
        // goto internal_error;
        return -1;
    }
    let mut cur = XmlGenericNodePtr::from_raw(children.as_ptr());
    'main: while let Some(mut cur_node) = cur {
        cur_node.set_document(Some(dest_doc));
        match cur_node.element_type() {
            XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode => {}
            XmlElementType::XmlEntityRefNode => {
                let mut cur = XmlNodePtr::try_from(cur_node).unwrap();
                // Remove reference to the entity-node.
                cur.content = null_mut();
                cur.set_children(None);
                cur.set_last(None);
                if dest_doc.int_subset.is_some() || dest_doc.ext_subset.is_some() {
                    // Assign new entity-node if available.
                    let ent = xml_get_doc_entity(Some(dest_doc), &cur.name().unwrap());
                    if let Some(ent) = ent {
                        cur.content = ent.content.load(Ordering::Relaxed);
                        cur.set_children(NodePtr::from_ptr(ent.as_ptr() as *mut XmlNode));
                        cur.set_last(NodePtr::from_ptr(ent.as_ptr() as *mut XmlNode));
                    }
                }
            }
            _ => {}
        }
        if let Some(children) = cur_node
            .children()
            .and_then(|c| XmlGenericNodePtr::from_raw(c.as_ptr()))
        {
            cur = Some(children);
            continue;
        }
        // next_sibling:
        'next_sibling: loop {
            if cur_node == attr.into() {
                break 'main;
            }
            if let Some(next) = cur_node
                .next()
                .and_then(|next| XmlGenericNodePtr::from_raw(next.as_ptr()))
            {
                cur_node = next;
            } else {
                cur_node = cur_node
                    .parent()
                    .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()))
                    .unwrap();
                // goto next_sibling;
                continue 'next_sibling;
            }

            cur = Some(cur_node);
            break 'next_sibling;
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
    source_doc: Option<XmlDocPtr>,
    mut node: XmlGenericNodePtr,
    dest_doc: XmlDocPtr,
    dest_parent: Option<XmlNodePtr>,
    options: i32,
) -> i32 {
    if matches!(node.element_type(), XmlElementType::XmlNamespaceDecl)
        || dest_parent.map_or(false, |dest_parent| dest_parent.doc != Some(dest_doc))
    {
        return -1;
    }
    // Check node.doc sanity.
    if node.document().map_or(false, |doc| Some(doc) != source_doc) && source_doc.is_some() {
        // Might be an XIncluded node.
        return -1;
    }
    let source_doc = source_doc.or(node.document());
    if source_doc == Some(dest_doc) {
        return -1;
    }
    match node.element_type() {
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
    if node
        .parent()
        .filter(|p| dest_parent.map(|d| d.into()) != XmlGenericNodePtr::from_raw(p.as_ptr()))
        .is_some()
    {
        node.unlink();
    }

    if let Some(node) = XmlNodePtr::try_from(node)
        .ok()
        .filter(|node| matches!(node.element_type(), XmlElementType::XmlElementNode))
    {
        return xml_dom_wrap_adopt_branch(ctxt, source_doc, node, dest_doc, dest_parent, options);
    } else if let Ok(attr) = XmlAttrPtr::try_from(node) {
        return xml_dom_wrap_adopt_attr(ctxt, source_doc, attr, dest_doc, dest_parent, options);
    } else {
        node.set_document(Some(dest_doc));
        match node.element_type() {
            XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode => {}
            XmlElementType::XmlEntityRefNode => {
                let mut node = XmlNodePtr::try_from(node).unwrap();
                // Remove reference to the entity-node.
                node.content = null_mut();
                node.set_children(None);
                node.set_last(None);
                if dest_doc.int_subset.is_some() || dest_doc.ext_subset.is_some() {
                    // Assign new entity-node if available.
                    let ent = xml_get_doc_entity(Some(dest_doc), &node.name().unwrap());
                    if let Some(ent) = ent {
                        node.content = ent.content.load(Ordering::Relaxed);
                        node.set_children(NodePtr::from_ptr(ent.as_ptr() as *mut XmlNode));
                        node.set_last(NodePtr::from_ptr(ent.as_ptr() as *mut XmlNode));
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
    doc: XmlDocPtr,
    mut node: XmlGenericNodePtr,
    _options: i32,
) -> i32 {
    let mut list = vec![];
    let mut nb_list: i32 = 0;

    if node.document() != Some(doc) {
        return -1;
    }

    // TODO: 0 or -1 ?
    if node.parent().is_none() {
        return 0;
    }

    match node.element_type() {
        XmlElementType::XmlTextNode
        | XmlElementType::XmlCDATASectionNode
        | XmlElementType::XmlEntityRefNode
        | XmlElementType::XmlPINode
        | XmlElementType::XmlCommentNode => {
            node.unlink();
            return 0;
        }
        XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode => {}
        _ => {
            return 1;
        }
    }
    node.unlink();
    // Save out-of-scope ns-references in (*doc).oldNs.
    'main: loop {
        match node.element_type() {
            XmlElementType::XmlElementNode => {
                let mut cur_node = XmlNodePtr::try_from(node).unwrap();
                if ctxt.is_null() && cur_node.ns_def.is_some() {
                    let mut ns = cur_node.ns_def;
                    while let Some(now) = ns {
                        if xml_dom_wrap_ns_norm_add_ns_map_item2(
                            &mut list,
                            // &raw mut size_list,
                            &raw mut nb_list,
                            now,
                            now,
                        ) == -1
                        {
                            // goto internal_error;
                            return -1;
                        }
                        ns = now.next;
                    }
                }

                if cur_node.ns.is_some() {
                    // Find a mapping.
                    for (_, j) in (0..nb_list).zip((0..).step_by(2)) {
                        if cur_node.ns == Some(list[j]) {
                            cur_node.ns = Some(list[j + 1]);
                            // goto next_node;
                            if let Some(children) = cur_node.children().filter(|_| {
                                matches!(cur_node.element_type(), XmlElementType::XmlElementNode)
                            }) {
                                node = XmlGenericNodePtr::from_raw(children.as_ptr()).unwrap();
                                continue 'main;
                            }
                            // next_sibling:
                            'next_sibling: loop {
                                if let Some(next) = node.next() {
                                    node = XmlGenericNodePtr::from_raw(next.as_ptr()).unwrap();
                                } else {
                                    let Some(parent) = node
                                        .parent()
                                        .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()))
                                    else {
                                        break 'main;
                                    };
                                    node = parent;
                                    // goto next_sibling;
                                    continue 'next_sibling;
                                }

                                continue 'main;
                            }
                        }
                    }
                    let mut ns = None;
                    if !ctxt.is_null() {
                        // User defined.
                    } else {
                        // Add to doc's oldNs.
                        ns = xml_dom_wrap_store_ns(
                            doc,
                            cur_node.ns.unwrap().href,
                            cur_node.ns.unwrap().prefix().as_deref(),
                        );
                        if ns.is_none() {
                            // goto internal_error;
                            return -1;
                        }
                    }
                    if let Some(ns) = ns {
                        // Add mapping.
                        if xml_dom_wrap_ns_norm_add_ns_map_item2(
                            &mut list,
                            // &raw mut size_list,
                            &raw mut nb_list,
                            cur_node.ns.unwrap(),
                            ns,
                        ) == -1
                        {
                            // goto internal_error;
                            return -1;
                        }
                    }
                    cur_node.ns = ns;
                }
                if matches!(node.element_type(), XmlElementType::XmlElementNode) {
                    if let Some(prop) = cur_node.properties {
                        node = XmlGenericNodePtr::from_raw(prop.as_ptr()).unwrap();
                        continue;
                    }
                }
            }
            XmlElementType::XmlAttributeNode => {
                let mut attr = XmlAttrPtr::try_from(node).unwrap();
                if attr.ns.is_some() {
                    // Find a mapping.
                    for (_, j) in (0..nb_list).zip((0..).step_by(2)) {
                        if attr.ns == Some(list[j]) {
                            attr.ns = Some(list[j + 1]);
                            // goto next_node;
                            if let Some(children) = attr.children().filter(|_| {
                                matches!(attr.element_type(), XmlElementType::XmlElementNode)
                            }) {
                                node = XmlGenericNodePtr::from_raw(children.as_ptr()).unwrap();
                                continue 'main;
                            }
                            // next_sibling:
                            'next_sibling: loop {
                                if let Some(next) = node.next() {
                                    node = XmlGenericNodePtr::from_raw(next.as_ptr()).unwrap();
                                } else {
                                    let Some(parent) = node
                                        .parent()
                                        .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()))
                                    else {
                                        break 'main;
                                    };
                                    node = parent;
                                    // goto next_sibling;
                                    continue 'next_sibling;
                                }

                                continue 'main;
                            }
                        }
                    }
                    let mut ns = None;
                    if !ctxt.is_null() {
                        // User defined.
                    } else {
                        // Add to doc's oldNs.
                        ns = xml_dom_wrap_store_ns(
                            doc,
                            attr.ns.unwrap().href,
                            attr.ns.unwrap().prefix().as_deref(),
                        );
                        if ns.is_none() {
                            // goto internal_error;
                            return -1;
                        }
                    }
                    if let Some(ns) = ns {
                        // Add mapping.
                        if xml_dom_wrap_ns_norm_add_ns_map_item2(
                            &mut list,
                            // &raw mut size_list,
                            &raw mut nb_list,
                            attr.ns.unwrap(),
                            ns,
                        ) == -1
                        {
                            // goto internal_error;
                            return -1;
                        }
                    }
                    attr.ns = ns;
                }
            }
            _ => {
                'next_sibling: loop {
                    if let Some(next) = node.next() {
                        node = XmlGenericNodePtr::from_raw(next.as_ptr()).unwrap();
                    } else {
                        let Some(parent) = node
                            .parent()
                            .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()))
                        else {
                            break 'main;
                        };
                        node = parent;
                        // goto next_sibling;
                        continue 'next_sibling;
                    }

                    continue 'main;
                }
            }
        }
        // next_node:
        if let Some(children) = node
            .children()
            .filter(|_| matches!(node.element_type(), XmlElementType::XmlElementNode))
        {
            node = XmlGenericNodePtr::from_raw(children.as_ptr()).unwrap();
            continue;
        }
        // next_sibling:
        'next_sibling: loop {
            if let Some(next) = node.next() {
                node = XmlGenericNodePtr::from_raw(next.as_ptr()).unwrap();
            } else {
                let Some(parent) = node
                    .parent()
                    .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()))
                else {
                    break 'main;
                };
                node = parent;
                // goto next_sibling;
                continue 'next_sibling;
            }

            continue 'main;
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
    source_doc: Option<XmlDocPtr>,
    node: XmlGenericNodePtr,
    res_node: &mut Option<XmlGenericNodePtr>,
    dest_doc: XmlDocPtr,
    dest_parent: Option<XmlGenericNodePtr>,
    deep: i32,
    _options: i32,
) -> i32 {
    let mut ret: i32 = 0;
    let mut cur_elem = None;
    let mut ns_map: XmlNsMapPtr = null_mut();
    let mut mi: XmlNsMapItemPtr;
    let mut depth: i32 = -1;
    // let adoptStr: i32 = 1;
    // gather @parent's ns-decls.
    let mut parnsdone: i32 = 0;
    // @ancestorsOnly:
    // TODO: @ancestorsOnly should be set per option.
    let ancestors_only: i32 = 0;
    let mut result_clone: Option<XmlGenericNodePtr> = None;
    let mut parent_clone: Option<XmlGenericNodePtr> = None;
    let mut prev_clone: Option<XmlGenericNodePtr> = None;

    // if node.is_null() {
    //     return -1;
    // }
    // TODO: Initially we support only element-nodes.
    if !matches!(node.element_type(), XmlElementType::XmlElementNode) {
        return 1;
    }
    // Check node.doc sanity.
    if node.document().map_or(false, |doc| Some(doc) != source_doc) && source_doc.is_some() {
        // Might be an XIncluded node.
        return -1;
    }
    let Some(source_doc) = source_doc.or(node.document()) else {
        return -1;
    };

    // Reuse the namespace map of the context.
    if !ctxt.is_null() {
        ns_map = (*ctxt).namespace_map as _;
    }

    *res_node = None;

    let mut cur = Some(node);
    'exit: {
        'internal_error: {
            'main: while let Some(cur_node) = cur {
                if cur_node.document() != Some(source_doc) {
                    // We'll assume XIncluded nodes if the doc differs.
                    // TODO: Do we need to reconciliate XIncluded nodes?
                    // TODO: This here returns -1 in this case.
                    break 'internal_error;
                }
                // Create a new node.
                let mut clone = match cur_node.element_type() {
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
                        let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                        // Nodes of xmlNode structure.
                        let Some(mut new) = XmlNodePtr::new(XmlNode::default()) else {
                            xml_tree_err_memory("xmlDOMWrapCloneNode(): allocating a node");
                            break 'internal_error;
                        };
                        new.typ = cur_node.element_type();
                        // Set hierarchical links.
                        if result_clone.is_some() {
                            new.set_parent(NodePtr::from_ptr(parent_clone.unwrap().as_ptr()));
                            if let Some(mut prev_clone) = prev_clone {
                                prev_clone.set_next(NodePtr::from_ptr(new.as_ptr()));
                                new.prev = NodePtr::from_ptr(prev_clone.as_ptr());
                            } else {
                                parent_clone
                                    .unwrap()
                                    .set_children(NodePtr::from_ptr(new.as_ptr()));
                            }
                        } else {
                            result_clone = Some(XmlGenericNodePtr::from(new));
                        }
                        // Clone the name of the node if any.
                        if cur_node.name == XML_STRING_TEXT.as_ptr() as _ {
                            new.name = XML_STRING_TEXT.as_ptr() as _;
                        } else if cur_node.name == XML_STRING_TEXT_NOENC.as_ptr() as _ {
                            // NOTE: Although xmlStringTextNoenc is never assigned to a node
                            //   in tree.c, it might be set in Libxslt via
                            //   "xsl:disable-output-escaping".
                            new.name = XML_STRING_TEXT_NOENC.as_ptr() as _;
                        } else if cur_node.name == XML_STRING_COMMENT.as_ptr() as _ {
                            new.name = XML_STRING_COMMENT.as_ptr() as _;
                        } else if !cur_node.name.is_null() {
                            new.name = xml_strdup(cur_node.name);
                        }
                        XmlGenericNodePtr::from(new)
                    }
                    XmlElementType::XmlAttributeNode => {
                        let cur_node = XmlAttrPtr::try_from(cur_node).unwrap();
                        // Attributes (xmlAttr).
                        // Use xmlRealloc to avoid -Warray-bounds warning
                        let Some(mut new) = XmlAttrPtr::new(XmlAttr::default()) else {
                            xml_tree_err_memory("xmlDOMWrapCloneNode(): allocating an attr-node");
                            break 'internal_error;
                        };
                        new.typ = cur_node.element_type();
                        // Set hierarchical links.
                        // TODO: Change this to add to the end of attributes.
                        if result_clone.is_some() {
                            new.set_parent(NodePtr::from_ptr(parent_clone.unwrap().as_ptr()));
                            if let Some(mut prev_clone) = prev_clone {
                                prev_clone
                                    .set_next(NodePtr::from_ptr(new.as_ptr() as *mut XmlNode));
                                new.prev = XmlAttrPtr::try_from(prev_clone).ok();
                            } else {
                                XmlNodePtr::try_from(parent_clone.unwrap())
                                    .unwrap()
                                    .properties = Some(new);
                            }
                        } else {
                            result_clone = Some(new.into());
                        }
                        // Clone the name of the node if any.
                        if cur_node.name == XML_STRING_TEXT.as_ptr() as _ {
                            new.name = XML_STRING_TEXT.as_ptr() as _;
                        } else if cur_node.name == XML_STRING_TEXT_NOENC.as_ptr() as _ {
                            // NOTE: Although xmlStringTextNoenc is never assigned to a node
                            //   in tree.c, it might be set in Libxslt via
                            //   "xsl:disable-output-escaping".
                            new.name = XML_STRING_TEXT_NOENC.as_ptr() as _;
                        } else if cur_node.name == XML_STRING_COMMENT.as_ptr() as _ {
                            new.name = XML_STRING_COMMENT.as_ptr() as _;
                        } else if !cur_node.name.is_null() {
                            new.name = xml_strdup(cur_node.name);
                        }

                        XmlGenericNodePtr::from(new)
                    }
                    _ => {
                        // TODO QUESTION: Any other nodes expected?
                        break 'internal_error;
                    }
                };
                clone.set_document(Some(dest_doc));

                let mut leave_node = false;
                match cur_node.element_type() {
                    XmlElementType::XmlXIncludeStart | XmlElementType::XmlXIncludeEnd => {
                        // TODO
                        return -1;
                    }
                    XmlElementType::XmlElementNode => {
                        let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                        cur_elem = Some(cur_node);
                        depth += 1;
                        // Namespace declarations.
                        if cur_node.ns_def.is_some() {
                            if parnsdone == 0 {
                                if dest_parent.is_some() && ctxt.is_null() {
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
                            let mut ns = cur_node.ns_def;
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
                                    last.next = Some(new);
                                    clone_ns_def_slot = Some(new);
                                } else {
                                    clone_ns_def_slot = Some(new);
                                    XmlNodePtr::try_from(clone).unwrap().ns_def = Some(new);
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
                                                && now.prefix() == (*mi).new_ns.unwrap().prefix()
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
                                        Some(new),
                                        depth,
                                    )
                                    .is_null()
                                    {
                                        break 'internal_error;
                                    }
                                }
                                ns = now.next;
                            }
                        }
                        // cur_node.ns will be processed further down.
                    }
                    XmlElementType::XmlAttributeNode => {
                        // IDs will be processed further down.
                        // cur_node.ns will be processed further down.
                    }
                    XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode => {
                        let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                        // Note that this will also cover the values of attributes.
                        XmlNodePtr::try_from(clone).unwrap().content = xml_strdup(cur_node.content);
                        // goto leave_node;
                        leave_node = true;
                    }
                    XmlElementType::XmlEntityNode => {
                        // TODO: What to do here?
                        // goto leave_node;
                        leave_node = true;
                    }
                    XmlElementType::XmlEntityRefNode => {
                        let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                        if source_doc != dest_doc {
                            if dest_doc.int_subset.is_some() || dest_doc.ext_subset.is_some() {
                                // Different doc: Assign new entity-node if available.
                                let ent =
                                    xml_get_doc_entity(Some(dest_doc), &cur_node.name().unwrap());
                                if let Some(ent) = ent {
                                    XmlNodePtr::try_from(clone).unwrap().content =
                                        ent.content.load(Ordering::Relaxed);
                                    clone.set_children(NodePtr::from_ptr(
                                        ent.as_ptr() as *mut XmlNode
                                    ));
                                    clone.set_last(NodePtr::from_ptr(ent.as_ptr() as *mut XmlNode));
                                }
                            }
                        } else {
                            // Same doc: Use the current node's entity declaration and value.
                            XmlNodePtr::try_from(clone).unwrap().content = cur_node.content;
                            clone.set_children(cur_node.children());
                            clone.set_last(cur_node.last());
                        }
                        // goto leave_node;
                        leave_node = true;
                    }
                    XmlElementType::XmlPINode => {
                        let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                        XmlNodePtr::try_from(clone).unwrap().content = xml_strdup(cur_node.content);
                        // goto leave_node;
                        leave_node = true;
                    }
                    XmlElementType::XmlCommentNode => {
                        let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                        XmlNodePtr::try_from(clone).unwrap().content = xml_strdup(cur_node.content);
                        // goto leave_node;
                        leave_node = true;
                    }
                    _ => {
                        break 'internal_error;
                    }
                }

                if !leave_node {
                    let cur_ns = if let Ok(node) = XmlNodePtr::try_from(node) {
                        node.ns
                    } else {
                        XmlAttrPtr::try_from(node).unwrap().ns
                    };
                    if let Some(cur_ns) = cur_ns {
                        // handle_ns_reference:
                        // The following will take care of references to ns-decls
                        // and is intended only for element- and attribute-nodes.
                        //
                        if parnsdone == 0 {
                            if dest_parent.is_some()
                                && ctxt.is_null()
                                && xml_dom_wrap_ns_norm_gather_in_scope_ns(
                                    &raw mut ns_map,
                                    dest_parent,
                                ) == -1
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
                                if (*mi).shadow_depth == -1 && Some(cur_ns) == (*mi).old_ns {
                                    // This is the nice case: a mapping was found.
                                    XmlNodePtr::try_from(clone).unwrap().ns = (*mi).new_ns;
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
                                    Some(cur_node),
                                    cur_ns.href,
                                    cur_ns.prefix,
                                );
                                // Add user's mapping.
                                if xml_dom_wrap_ns_map_add_item(
                                    &raw mut ns_map,
                                    -1,
                                    Some(cur_ns),
                                    ns,
                                    XML_TREE_NSMAP_CUSTOM,
                                )
                                .is_null()
                                {
                                    break 'internal_error;
                                }
                                XmlNodePtr::try_from(clone).unwrap().ns = ns;
                            } else {
                                // Acquire a normalized ns-decl and add it to the map.
                                let mut ns = None;
                                if xml_dom_wrap_ns_norm_acquire_normalized_ns(
                                    dest_doc,
                                    // ns-decls on curElem or on (*destDoc).oldNs
                                    dest_parent.and(cur_elem),
                                    cur_ns,
                                    &mut ns,
                                    &raw mut ns_map,
                                    depth,
                                    // if we need to search only in the ancestor-axis
                                    ancestors_only,
                                    // ns-decls must be prefixed for attributes.
                                    matches!(
                                        cur_node.element_type(),
                                        XmlElementType::XmlAttributeNode
                                    ) as i32,
                                ) == -1
                                {
                                    break 'internal_error;
                                }
                                XmlNodePtr::try_from(clone).unwrap().ns = ns;
                            }
                        }
                    }

                    // Some post-processing.
                    //
                    // Handle ID attributes.
                    if matches!(clone.element_type(), XmlElementType::XmlAttributeNode)
                        && clone.parent().is_some()
                        && xml_is_id(
                            Some(dest_doc),
                            XmlNodePtr::from_raw(clone.parent().unwrap().as_ptr()).unwrap(),
                            XmlAttrPtr::try_from(clone).ok(),
                        ) != 0
                    {
                        let children = cur_node.children();
                        if let Some(id_val) =
                            children.and_then(|c| c.get_string(cur_node.document(), 1))
                        {
                            if xml_add_id(
                                null_mut(),
                                dest_doc,
                                &id_val,
                                XmlAttrPtr::try_from(cur_node).unwrap(),
                            )
                            .is_none()
                            {
                                // TODO: error message.
                                break 'internal_error;
                            }
                        }
                    }
                    // The following will traverse the tree **************************
                    //
                    // Walk the element's attributes before descending into child-nodes.
                    if matches!(cur_node.element_type(), XmlElementType::XmlElementNode) {
                        let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                        if let Some(prop) = cur_node.properties {
                            prev_clone = None;
                            parent_clone = Some(clone);
                            cur = Some(prop.into());
                            continue 'main;
                        }
                    }
                    // into_content:
                    // Descend into child-nodes.
                    if let Some(children) = cur_node.children().filter(|_| {
                        deep != 0
                            || matches!(cur_node.element_type(), XmlElementType::XmlAttributeNode)
                    }) {
                        prev_clone = None;
                        parent_clone = Some(clone);
                        cur = XmlGenericNodePtr::from_raw(children.as_ptr());
                        continue 'main;
                    }
                }

                // leave_node:
                'leave_node: loop {
                    // At this point we are done with the node, its content
                    // and an element-nodes's attribute-nodes.
                    if cur == Some(node) {
                        break 'main;
                    }
                    let cur_node = cur.unwrap();
                    if matches!(cur_node.element_type(), XmlElementType::XmlElementNode)
                        || matches!(cur_node.element_type(), XmlElementType::XmlXIncludeStart)
                        || matches!(cur_node.element_type(), XmlElementType::XmlXIncludeEnd)
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
                    if let Some(next) = cur_node.next() {
                        prev_clone = Some(clone);
                        cur = XmlGenericNodePtr::from_raw(next.as_ptr());
                    } else if !matches!(cur_node.element_type(), XmlElementType::XmlAttributeNode) {
                        // Set clone.last.
                        if let Some(mut parent) = clone.parent() {
                            parent.set_last(NodePtr::from_ptr(clone.as_ptr()));
                        }
                        clone = clone
                            .parent()
                            .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()))
                            .unwrap();
                        parent_clone = clone
                            .parent()
                            .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()));
                        // Process parent --> next;
                        cur = cur_node
                            .parent()
                            .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()));
                        // goto leave_node;
                        continue 'leave_node;
                    } else {
                        // This is for attributes only.
                        clone = clone
                            .parent()
                            .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()))
                            .unwrap();
                        parent_clone = clone
                            .parent()
                            .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()));
                        // Process parent-element --> children.
                        cur = cur_node
                            .parent()
                            .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()));
                        // goto into_content;
                        // Descend into child-nodes.
                        if let Some(children) = cur_node.children().filter(|_| {
                            deep != 0
                                || matches!(
                                    cur_node.element_type(),
                                    XmlElementType::XmlAttributeNode
                                )
                        }) {
                            prev_clone = None;
                            parent_clone = Some(clone);
                            cur = XmlGenericNodePtr::from_raw(children.as_ptr());
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
