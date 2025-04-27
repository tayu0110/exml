//! Provide internal methods and data structures for handling XML Pointers.
//!
//! This module is based on `libxml/xpointer.h`, `xpointer.c` and so on in `libxml2-v2.11.8`.  
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: API to handle XML Pointers
// Description: API to handle XML Pointers
// Base implementation was made accordingly to
// W3C Candidate Recommendation 7 June 2000
// http://www.w3.org/TR/2000/CR-xptr-20000607
//
// Added support for the element() scheme described in:
// W3C Proposed Recommendation 13 November 2002
// http://www.w3.org/TR/2002/PR-xptr-element-20021113/
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// xpointer.c : Code to handle XML Pointer
//
// Base implementation was made accordingly to
// W3C Candidate Recommendation 7 June 2000
// http://www.w3.org/TR/2000/CR-xptr-20000607
//
// Added support for the element() scheme described in:
// W3C Proposed Recommendation 13 November 2002
// http://www.w3.org/TR/2002/PR-xptr-element-20021113/
//
// See Copyright for the status of this software.
//
// daniel@veillard.com

use std::{ffi::CStr, mem::replace, ptr::null_mut, rc::Rc};

use crate::xpath::XmlXPathContext;
use crate::xpath::functions::xml_xpath_id_function;
#[cfg(feature = "libxml_xptr_locs")]
use crate::xpath::{XmlNodeSet, XmlXPathParserContext, functions::check_arity};
#[cfg(feature = "libxml_xptr_locs")]
use crate::xpath::{XmlXPathObject, internals::xml_xpath_err, xml_xpath_cmp_nodes};
use crate::{
    error::{__xml_raise_error, XmlErrorDomain, XmlErrorLevel, XmlParserErrors},
    libxml::{
        globals::xml_free,
        xmlstring::{XmlChar, xml_str_equal},
    },
    parser::xml_init_parser,
    tree::{NodeCommon, XmlDocPtr, XmlElementType, XmlGenericNodePtr},
    xpath::{
        XmlXPathContextPtr, XmlXPathError, XmlXPathObjectPtr, XmlXPathObjectType,
        internals::xml_xpath_root, xml_xpath_new_context, xml_xpath_new_node_set,
        xml_xpath_new_string,
    },
};

use super::xmlstring::xml_strndup;

/// A Location Set
#[cfg(feature = "libxml_xptr_locs")]
#[repr(C)]
#[derive(Clone, PartialEq)]
pub struct XmlLocationSet {
    pub(crate) loc_tab: Vec<Rc<XmlXPathObject>>, /* array of locations */
}

#[cfg(feature = "libxml_xptr_locs")]
impl XmlLocationSet {
    /// Create a new xmlLocationSetPtr of type double and of value @val
    ///
    /// Returns the newly created object.
    #[doc(alias = "xmlXPtrLocationSetCreate")]
    pub(crate) fn new(val: Option<Rc<XmlXPathObject>>) -> XmlLocationSet {
        let mut ret = XmlLocationSet {
            loc_tab: Vec::with_capacity(XML_RANGESET_DEFAULT),
        };
        if let Some(val) = val {
            ret.loc_tab.push(val);
        }
        ret
    }

    /// Add a new xmlXPathObjectPtr to an existing LocationSet
    /// If the location already exist in the set @val is freed.
    #[doc(alias = "xmlXPtrLocationSetAdd")]
    pub(crate) fn push(&mut self, val: Rc<XmlXPathObject>) {
        // check against doublons
        for loc in &self.loc_tab {
            if xml_xptr_ranges_equal(loc, &val) != 0 {
                return;
            }
        }

        self.loc_tab.push(val);
    }

    /// Merges two rangesets, all ranges from @val2 are added to @val1
    ///
    /// Returns val1 once extended or NULL in case of error.
    #[doc(alias = "xmlXPtrLocationSetMerge")]
    pub(crate) fn merge(&mut self, val2: &XmlLocationSet) {
        // !!!!! this can be optimized a lot, knowing that both
        //       val1 and val2 already have unicity of their values.
        for loc in &val2.loc_tab {
            self.push(loc.clone());
        }
    }
}

#[cfg(feature = "libxml_xptr_locs")]
const XML_RANGESET_DEFAULT: usize = 10;

#[cfg(feature = "libxml_xptr_locs")]
macro_rules! STRANGE {
    () => {
        $crate::generic_error!("Internal error at {}:{}\n", file!(), line!());
    };
}

/// Handle a redefinition of attribute error
#[doc(alias = "xmlXPtrErrMemory")]
fn xml_xptr_err_memory(extra: &str) {
    __xml_raise_error!(
        None,
        None,
        None,
        null_mut(),
        None,
        XmlErrorDomain::XmlFromXPointer,
        XmlParserErrors::XmlErrNoMemory,
        XmlErrorLevel::XmlErrError,
        None,
        0,
        Some(extra.to_owned().into()),
        None,
        None,
        0,
        0,
        "Memory allocation failed : {}\n",
        extra
    );
}

/// Create a new xmlXPathObjectPtr of type LocationSet and initialize
/// it with the single range made of the two nodes @start and @end
///
/// Returns the newly created object.
#[doc(alias = "xmlXPtrNewLocationSetNodes")]
#[cfg(feature = "libxml_xptr_locs")]
fn xml_xptr_new_location_set_nodes(
    start: XmlGenericNodePtr,
    end: Option<XmlGenericNodePtr>,
) -> XmlXPathObject {
    use crate::xpath::XmlXPathObjectUserData;

    let mut ret = XmlXPathObject::default();
    ret.typ = XmlXPathObjectType::XPathLocationset;
    let loc = if let Some(end) = end {
        XmlLocationSet::new(xml_xptr_new_range_nodes(start, end).map(Rc::new))
    } else {
        XmlLocationSet::new(xml_xptr_new_collapsed_range(start).map(Rc::new))
    };
    ret.user = Some(XmlXPathObjectUserData::LocationSet(loc));
    ret
}

/// Internal function to create a new xmlXPathObjectPtr of type range
///
/// Returns the newly created object.
#[doc(alias = "xmlXPtrNewRangeInternal")]
#[cfg(feature = "libxml_xptr_locs")]
fn xml_xptr_new_range_internal(
    start: Option<XmlGenericNodePtr>,
    startindex: i32,
    end: Option<XmlGenericNodePtr>,
    endindex: i32,
) -> Option<XmlXPathObject> {
    use crate::xpath::XmlXPathObjectUserData;

    // Namespace nodes must be copied (see xmlXPathNodeSetDupNs).
    // Disallow them for now.
    if start.is_some_and(|start| matches!(start.element_type(), XmlElementType::XmlNamespaceDecl)) {
        return None;
    }
    if end.is_some_and(|end| matches!(end.element_type(), XmlElementType::XmlNamespaceDecl)) {
        return None;
    }

    let mut ret = XmlXPathObject::default();
    ret.typ = XmlXPathObjectType::XPathRange;
    ret.user = start.map(XmlXPathObjectUserData::Node);
    ret.index = startindex;
    ret.user2 = end.map(XmlXPathObjectUserData::Node);
    ret.index2 = endindex;
    Some(ret)
}

/// Compare two points w.r.t document order
///
/// Returns -2 in case of error 1 if first point < second point, 0 if
///         that's the same point, -1 otherwise
#[doc(alias = "xmlXPtrCmpPoints")]
#[cfg(feature = "libxml_xptr_locs")]
fn xml_xptr_cmp_points(
    node1: XmlGenericNodePtr,
    index1: i32,
    node2: XmlGenericNodePtr,
    index2: i32,
) -> i32 {
    // a couple of optimizations which will avoid computations in most cases
    if node1 == node2 {
        if index1 < index2 {
            return 1;
        }
        if index1 > index2 {
            return -1;
        }
        return 0;
    }
    xml_xpath_cmp_nodes(node1, node2)
}

/// Make sure the points in the range are in the right order
#[doc(alias = "xmlXPtrRangeCheckOrder")]
#[cfg(feature = "libxml_xptr_locs")]
fn xml_xptr_range_check_order(range: Option<&mut XmlXPathObject>) {
    let Some(range) = range else {
        return;
    };
    if !matches!(range.typ, XmlXPathObjectType::XPathRange) {
        return;
    }
    if range.user2.is_none() {
        return;
    }
    let tmp = xml_xptr_cmp_points(
        range
            .user
            .as_ref()
            .and_then(|user| user.as_node())
            .copied()
            .unwrap(),
        range.index,
        range
            .user2
            .as_ref()
            .and_then(|user| user.as_node())
            .copied()
            .unwrap(),
        range.index2,
    );
    if tmp == -1 {
        std::mem::swap(&mut range.user, &mut range.user2);
        std::mem::swap(&mut range.index, &mut range.index2);
    }
}

/// Create a new xmlXPathObjectPtr of type range
///
/// Returns the newly created object.
#[doc(alias = "xmlXPtrNewRange")]
#[cfg(feature = "libxml_xptr_locs")]
pub(crate) fn xml_xptr_new_range(
    start: XmlGenericNodePtr,
    startindex: i32,
    end: XmlGenericNodePtr,
    endindex: i32,
) -> Option<XmlXPathObject> {
    if startindex < 0 {
        return None;
    }
    if endindex < 0 {
        return None;
    }

    let mut ret = xml_xptr_new_range_internal(Some(start), startindex, Some(end), endindex);
    xml_xptr_range_check_order(ret.as_mut());
    ret
}

/// Create a new xmlXPathObjectPtr of type range using 2 Points
///
/// Returns the newly created object.
#[doc(alias = "xmlXPtrNewRangePoints")]
#[cfg(feature = "libxml_xptr_locs")]
pub unsafe fn xml_xptr_new_range_points(
    start: XmlXPathObjectPtr,
    end: XmlXPathObjectPtr,
) -> Option<XmlXPathObject> {
    unsafe {
        if start.is_null() {
            return None;
        }
        if end.is_null() {
            return None;
        }
        if !matches!((*start).typ, XmlXPathObjectType::XPathPoint) {
            return None;
        }
        if !matches!((*end).typ, XmlXPathObjectType::XPathPoint) {
            return None;
        }

        let mut ret = xml_xptr_new_range_internal(
            (*start)
                .user
                .as_ref()
                .and_then(|user| user.as_node())
                .copied(),
            (*start).index,
            (*end)
                .user
                .as_ref()
                .and_then(|user| user.as_node())
                .copied(),
            (*end).index,
        );
        xml_xptr_range_check_order(ret.as_mut());
        ret
    }
}

// /// Create a new xmlXPathObjectPtr of type range from a node to a point
// ///
// /// Returns the newly created object.
// #[doc(alias = "xmlXPtrNewRangeNodePoint")]
// #[cfg(feature = "libxml_xptr_locs")]
// pub(crate) unsafe fn xml_xptr_new_range_node_point(
//     start: XmlGenericNodePtr,
//     end: XmlXPathObjectPtr,
// ) -> XmlXPathObjectPtr {
//     unsafe {
//         // if start.is_null() {
//         //     return null_mut();
//         // }
//         if end.is_null() {
//             return null_mut();
//         }
//         if !matches!((*end).typ, XmlXPathObjectType::XPathPoint) {
//             return null_mut();
//         }

//         let ret: XmlXPathObjectPtr = xml_xptr_new_range_internal(
//             Some(start),
//             -1,
//             XmlGenericNodePtr::from_raw((*end).user as *mut XmlNode),
//             (*end).index,
//         );
//         xml_xptr_range_check_order(ret);
//         ret
//     }
// }

// /// Create a new xmlXPathObjectPtr of type range from a point to a node
// ///
// /// Returns the newly created object.
// #[doc(alias = "xmlXPtrNewRangePointNode")]
// #[cfg(feature = "libxml_xptr_locs")]
// pub(crate) unsafe fn xml_xptr_new_range_point_node(
//     start: XmlXPathObjectPtr,
//     end: XmlGenericNodePtr,
// ) -> XmlXPathObjectPtr {
//     unsafe {
//         if start.is_null() {
//             return null_mut();
//         }
//         // if end.is_null() {
//         //     return null_mut();
//         // }
//         if !matches!((*start).typ, XmlXPathObjectType::XPathPoint) {
//             return null_mut();
//         }

//         let ret: XmlXPathObjectPtr = xml_xptr_new_range_internal(
//             XmlGenericNodePtr::from_raw((*start).user as *mut XmlNode),
//             (*start).index,
//             Some(end),
//             -1,
//         );
//         xml_xptr_range_check_order(ret);
//         ret
//     }
// }

/// Create a new xmlXPathObjectPtr of type range using 2 nodes
///
/// Returns the newly created object.
#[doc(alias = "xmlXPtrNewRangeNodes")]
#[cfg(feature = "libxml_xptr_locs")]
pub(crate) fn xml_xptr_new_range_nodes(
    start: XmlGenericNodePtr,
    end: XmlGenericNodePtr,
) -> Option<XmlXPathObject> {
    let mut ret = xml_xptr_new_range_internal(Some(start), -1, Some(end), -1);
    xml_xptr_range_check_order(ret.as_mut());
    ret
}

/// Create a new xmlXPathObjectPtr of type LocationSet and initialize
/// it with all the nodes from @set
///
/// Returns the newly created object.
#[doc(alias = "xmlXPtrNewLocationSetNodeSet")]
#[cfg(feature = "libxml_xptr_locs")]
pub(crate) fn xml_xptr_new_location_set_node_set(set: Option<&XmlNodeSet>) -> XmlXPathObject {
    use crate::xpath::XmlXPathObjectUserData;

    let mut ret = XmlXPathObject::default();
    ret.typ = XmlXPathObjectType::XPathLocationset;
    if let Some(set) = set {
        let mut newset = XmlLocationSet::new(None);

        for &node in &set.node_tab {
            newset.push(xml_xptr_new_collapsed_range(node).map(Rc::new).unwrap());
        }

        ret.user = Some(XmlXPathObjectUserData::LocationSet(newset));
    }
    ret
}

/// Create a new xmlXPathObjectPtr of type range from a not to an object
///
/// Returns the newly created object.
#[doc(alias = "xmlXPtrNewRangeNodeObject")]
#[cfg(feature = "libxml_xptr_locs")]
pub(crate) fn xml_xptr_new_range_node_object(
    start: XmlGenericNodePtr,
    end: &XmlXPathObject,
) -> Option<XmlXPathObject> {
    let end_index: i32;

    let end_node = match end.typ {
        XmlXPathObjectType::XPathPoint => {
            end_index = end.index;
            end.user.as_ref().and_then(|user| user.as_node()).copied()
        }
        XmlXPathObjectType::XPathRange => {
            end_index = end.index2;
            end.user2.as_ref().and_then(|user| user.as_node()).copied()
        }
        XmlXPathObjectType::XPathNodeset => {
            // Empty set ...
            let nodeset = end.nodesetval.as_deref().filter(|s| !s.is_empty())?;
            end_index = -1;
            nodeset.node_tab.last().copied()
        }
        _ => {
            /* TODO */
            return None;
        }
    };

    let mut ret = xml_xptr_new_range_internal(Some(start), -1, end_node, end_index);
    xml_xptr_range_check_order(ret.as_mut());
    ret
}

/// Create a new xmlXPathObjectPtr of type range using a single nodes
///
/// Returns the newly created object.
#[doc(alias = "xmlXPtrNewCollapsedRange")]
#[cfg(feature = "libxml_xptr_locs")]
pub(crate) fn xml_xptr_new_collapsed_range(start: XmlGenericNodePtr) -> Option<XmlXPathObject> {
    xml_xptr_new_range_internal(Some(start), -1, None, -1)
}

/// Compare two ranges
///
/// Returns 1 if equal, 0 otherwise
#[doc(alias = "xmlXPtrRangesEqual")]
#[cfg(feature = "libxml_xptr_locs")]
fn xml_xptr_ranges_equal(range1: &XmlXPathObject, range2: &XmlXPathObject) -> i32 {
    if std::ptr::eq(range1, range2) {
        return 1;
    }
    if range1.typ != range2.typ {
        return 0;
    }
    if range1.typ != XmlXPathObjectType::XPathRange {
        return 0;
    }
    if range1.user != range2.user {
        return 0;
    }
    if range1.index != range2.index {
        return 0;
    }
    if range1.user2 != range2.user2 {
        return 0;
    }
    if range1.index2 != range2.index2 {
        return 0;
    }
    1
}

/// Wrap the LocationSet @val in a new xmlXPathObjectPtr
///
/// Returns the newly created object.
#[doc(alias = "xmlXPtrWrapLocationSet")]
#[cfg(feature = "libxml_xptr_locs")]
pub(crate) fn xml_xptr_wrap_location_set(val: XmlLocationSet) -> XmlXPathObject {
    use crate::xpath::XmlXPathObjectUserData;

    let mut ret = XmlXPathObject::default();
    ret.typ = XmlXPathObjectType::XPathLocationset;
    ret.user = Some(XmlXPathObjectUserData::LocationSet(val));
    ret
}

// /// Removes an xmlXPathObjectPtr from an existing LocationSet
// #[doc(alias = "xmlXPtrLocationSetDel")]
// #[cfg(feature = "libxml_xptr_locs")]
// pub(crate) unsafe fn xml_xptr_location_set_del(cur: XmlLocationSetPtr, val: XmlXPathObjectPtr) {
//     unsafe {
//         if cur.is_null() {
//             return;
//         }
//         if val.is_null() {
//             return;
//         }

//         // check against doublons
//         let Some(i) = (*cur).loc_tab.iter().position(|&loc| loc == val) else {
//             return;
//         };
//         (*cur).loc_tab.remove(i);
//     }
// }

// /// Removes an entry from an existing LocationSet list.
// #[doc(alias = "xmlXPtrLocationSetRemove")]
// #[cfg(feature = "libxml_xptr_locs")]
// pub(crate) unsafe fn xml_xptr_location_set_remove(cur: XmlLocationSetPtr, val: i32) {
//     unsafe {
//         if cur.is_null() {
//             return;
//         }
//         if val >= (*cur).loc_tab.len() as i32 {
//             return;
//         }
//         (*cur).loc_tab.remove(val as usize);
//     }
// }

/// Returns the number of child for an element, -1 in case of error
#[doc(alias = "xmlXPtrGetArity")]
#[cfg(feature = "libxml_xptr_locs")]
unsafe fn xml_xptr_get_arity(cur: XmlGenericNodePtr) -> i32 {
    let mut i: i32;
    if cur.element_type() == XmlElementType::XmlNamespaceDecl {
        return -1;
    }
    let mut cur = cur.children();
    i = 0;
    while let Some(now) = cur {
        if matches!(
            now.element_type(),
            XmlElementType::XmlElementNode
                | XmlElementType::XmlDocumentNode
                | XmlElementType::XmlHTMLDocumentNode
        ) {
            i += 1;
        }
        cur = now.next();
    }
    i
}

/// Returns the index of the node in its parent children list, -1 in case of error
#[doc(alias = "xmlXPtrGetIndex")]
#[cfg(feature = "libxml_xptr_locs")]
unsafe fn xml_xptr_get_index(cur: XmlGenericNodePtr) -> i32 {
    let mut i: i32;
    if cur.element_type() == XmlElementType::XmlNamespaceDecl {
        return -1;
    }
    i = 1;
    let mut cur = Some(cur);
    while let Some(now) = cur {
        if matches!(
            now.element_type(),
            XmlElementType::XmlElementNode
                | XmlElementType::XmlDocumentNode
                | XmlElementType::XmlHTMLDocumentNode
        ) {
            i += 1;
        }
        cur = now.prev();
    }
    i
}

/// A covering range is a range that wholly encompasses a location
/// Section 5.3.3. Covering Ranges for All Location Types
///        http://www.w3.org/TR/xptr#N2267
///
/// Returns a new location or NULL in case of error
#[doc(alias = "xmlXPtrCoveringRange")]
#[cfg(feature = "libxml_xptr_locs")]
unsafe fn xml_xptr_covering_range(
    ctxt: &mut XmlXPathParserContext,
    loc: Rc<XmlXPathObject>,
) -> Option<XmlXPathObject> {
    unsafe {
        let doc = ctxt.context.doc?;
        match loc.typ {
            XmlXPathObjectType::XPathPoint => {
                return xml_xptr_new_range(
                    loc.user
                        .as_ref()
                        .and_then(|user| user.as_node())
                        .copied()
                        .unwrap(),
                    loc.index,
                    loc.user
                        .as_ref()
                        .and_then(|user| user.as_node())
                        .copied()
                        .unwrap(),
                    loc.index,
                );
            }
            XmlXPathObjectType::XPathRange => {
                if loc.user2.is_some() {
                    return xml_xptr_new_range(
                        loc.user
                            .as_ref()
                            .and_then(|user| user.as_node())
                            .copied()
                            .unwrap(),
                        loc.index,
                        loc.user2
                            .as_ref()
                            .and_then(|user| user.as_node())
                            .copied()
                            .unwrap(),
                        loc.index2,
                    );
                } else {
                    let mut node = loc
                        .user
                        .as_ref()
                        .and_then(|user| user.as_node())
                        .copied()
                        .unwrap();

                    if node == XmlGenericNodePtr::from(doc) {
                        return xml_xptr_new_range(node, 0, node, xml_xptr_get_arity(node));
                    } else {
                        match node.element_type() {
                            XmlElementType::XmlAttributeNode => {
                                /* !!! our model is slightly different than XPath */
                                return xml_xptr_new_range(node, 0, node, xml_xptr_get_arity(node));
                            }
                            XmlElementType::XmlElementNode
                            | XmlElementType::XmlTextNode
                            | XmlElementType::XmlCDATASectionNode
                            | XmlElementType::XmlEntityRefNode
                            | XmlElementType::XmlPINode
                            | XmlElementType::XmlCommentNode
                            | XmlElementType::XmlDocumentNode
                            | XmlElementType::XmlNotationNode
                            | XmlElementType::XmlHTMLDocumentNode => {
                                let indx: i32 = xml_xptr_get_index(node);

                                node = node.parent().unwrap();
                                return xml_xptr_new_range(node, indx - 1, node, indx + 1);
                            }
                            _ => return None,
                        }
                    }
                }
            }
            _ => {
                // TODO /* missed one case ??? */
            }
        }
        None
    }
}

/// Function implementing the range() function 5.4.3
///  location-set range(location-set )
///
///  The range function returns ranges covering the locations in
///  the argument location-set. For each location x in the argument
///  location-set, a range location representing the covering range of
///  x is added to the result location-set.
#[doc(alias = "xmlXPtrRangeFunction")]
#[cfg(feature = "libxml_xptr_locs")]
unsafe fn xml_xptr_range_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    unsafe {
        if check_arity(ctxt, nargs, 1).is_err() {
            return;
        }
        if ctxt.value().is_none_or(|value| {
            !matches!(
                value.typ,
                XmlXPathObjectType::XPathLocationset | XmlXPathObjectType::XPathNodeset
            )
        }) {
            xml_xpath_err(Some(&mut *ctxt), XmlXPathError::XPathInvalidType as i32);
            return;
        }

        let mut set = ctxt.value_pop().unwrap();
        if set.typ == XmlXPathObjectType::XPathNodeset {
            // First convert to a location set
            set = xml_xptr_new_location_set_node_set(set.nodesetval.as_deref());
        }
        let oldset = set.user.as_ref().and_then(|user| user.as_location_set());

        // The loop is to compute the covering range for each item and add it
        let mut newset = XmlLocationSet::new(None);
        if let Some(oldset) = oldset {
            for loc in &oldset.loc_tab {
                newset.push(
                    xml_xptr_covering_range(ctxt, loc.clone())
                        .map(Rc::new)
                        .unwrap(),
                );
            }
        }

        // Save the new value and cleanup
        ctxt.value_push(xml_xptr_wrap_location_set(newset));
    }
}

/// A inside range is a range described in the range-inside() description
///
/// Returns a new location or NULL in case of error
#[doc(alias = "xmlXPtrInsideRange")]
#[cfg(feature = "libxml_xptr_locs")]
unsafe fn xml_xptr_inside_range(
    ctxt: &mut XmlXPathParserContext,
    loc: Rc<XmlXPathObject>,
) -> Option<XmlXPathObject> {
    unsafe {
        use crate::tree::XmlNodePtr;

        ctxt.context.doc?;
        match loc.typ {
            XmlXPathObjectType::XPathPoint => {
                let node = loc
                    .user
                    .as_ref()
                    .and_then(|user| user.as_node())
                    .copied()
                    .unwrap();

                match node.element_type() {
                    XmlElementType::XmlPINode
                    | XmlElementType::XmlCommentNode
                    | XmlElementType::XmlTextNode
                    | XmlElementType::XmlCDATASectionNode => {
                        let node = XmlNodePtr::try_from(node).unwrap();
                        if let Some(content) = node.content.as_deref() {
                            return xml_xptr_new_range(
                                node.into(),
                                0,
                                node.into(),
                                content.len() as i32,
                            );
                        } else {
                            return xml_xptr_new_range(node.into(), 0, node.into(), 0);
                        }
                    }
                    XmlElementType::XmlAttributeNode
                    | XmlElementType::XmlElementNode
                    | XmlElementType::XmlEntityRefNode
                    | XmlElementType::XmlDocumentNode
                    | XmlElementType::XmlNotationNode
                    | XmlElementType::XmlHTMLDocumentNode => {
                        return xml_xptr_new_range(node, 0, node, xml_xptr_get_arity(node));
                    }
                    _ => {}
                }
                return None;
            }
            XmlXPathObjectType::XPathRange => {
                let node = loc
                    .user
                    .as_ref()
                    .and_then(|user| user.as_node())
                    .copied()
                    .unwrap();

                if loc.user2.is_some() {
                    return xml_xptr_new_range(
                        node,
                        loc.index,
                        loc.user2
                            .as_ref()
                            .and_then(|user| user.as_node())
                            .copied()
                            .unwrap(),
                        loc.index2,
                    );
                } else {
                    match node.element_type() {
                        XmlElementType::XmlPINode
                        | XmlElementType::XmlCommentNode
                        | XmlElementType::XmlTextNode
                        | XmlElementType::XmlCDATASectionNode => {
                            let node = XmlNodePtr::try_from(node).unwrap();
                            if let Some(content) = node.content.as_deref() {
                                return xml_xptr_new_range(
                                    node.into(),
                                    0,
                                    node.into(),
                                    content.len() as i32,
                                );
                            } else {
                                return xml_xptr_new_range(node.into(), 0, node.into(), 0);
                            }
                        }
                        XmlElementType::XmlAttributeNode
                        | XmlElementType::XmlElementNode
                        | XmlElementType::XmlEntityRefNode
                        | XmlElementType::XmlDocumentNode
                        | XmlElementType::XmlNotationNode
                        | XmlElementType::XmlHTMLDocumentNode => {
                            return xml_xptr_new_range(node, 0, node, xml_xptr_get_arity(node));
                        }
                        _ => {}
                    }
                    return None;
                }
            }
            _ => {
                // TODO /* missed one case ??? */
            }
        }
        None
    }
}

/// Function implementing the range-inside() function 5.4.3
///  location-set range-inside(location-set )
///
///  The range-inside function returns ranges covering the contents of
///  the locations in the argument location-set. For each location x in
///  the argument location-set, a range location is added to the result
///  location-set. If x is a range location, then x is added to the
///  result location-set. If x is not a range location, then x is used
///  as the container location of the start and end points of the range
///  location to be added; the index of the start point of the range is
///  zero; if the end point is a character point then its index is the
///  length of the string-value of x, and otherwise is the number of
///  location children of x.
#[doc(alias = "xmlXPtrRangeInsideFunction")]
#[cfg(feature = "libxml_xptr_locs")]
unsafe fn xml_xptr_range_inside_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    unsafe {
        if check_arity(ctxt, nargs, 1).is_err() {
            return;
        }
        if ctxt.value().is_none_or(|value| {
            !matches!(
                value.typ,
                XmlXPathObjectType::XPathLocationset | XmlXPathObjectType::XPathNodeset
            )
        }) {
            xml_xpath_err(Some(&mut *ctxt), XmlXPathError::XPathInvalidType as i32);
            return;
        }

        let mut set = ctxt.value_pop().unwrap();
        if set.typ == XmlXPathObjectType::XPathNodeset {
            // First convert to a location set
            set = xml_xptr_new_location_set_node_set(set.nodesetval.as_deref());
        }

        // The loop is to compute the covering range for each item and add it
        let mut newset = XmlLocationSet::new(None);
        let oldset = set.user.as_ref().and_then(|user| user.as_location_set());

        if let Some(oldset) = oldset {
            for loc in &oldset.loc_tab {
                newset.push(
                    xml_xptr_inside_range(ctxt, loc.clone())
                        .map(Rc::new)
                        .unwrap(),
                );
            }
        }

        // Save the new value and cleanup
        ctxt.value_push(xml_xptr_wrap_location_set(newset));
    }
}

/// Read the object and return the start point coordinates.
///
/// Returns -1 in case of failure, 0 otherwise
#[doc(alias = "xmlXPtrGetStartPoint")]
#[cfg(feature = "libxml_xptr_locs")]
fn xml_xptr_get_start_point(
    obj: &XmlXPathObject,
    node: &mut Option<XmlGenericNodePtr>,
    indx: &mut usize,
) -> i32 {
    match obj.typ {
        XmlXPathObjectType::XPathPoint => {
            *node = obj.user.as_ref().and_then(|user| user.as_node()).copied();

            if obj.index <= 0 {
                *indx = 0;
            } else {
                *indx = obj.index as usize;
            }
            return 0;
        }
        XmlXPathObjectType::XPathRange => {
            *node = obj.user.as_ref().and_then(|user| user.as_node()).copied();

            if obj.index <= 0 {
                *indx = 0;
            } else {
                *indx = obj.index as usize;
            }
            return 0;
        }
        _ => {}
    }
    -1
}

/// Read the object and return the end point coordinates.
///
/// Returns -1 in case of failure, 0 otherwise
#[doc(alias = "xmlXPtrGetEndPoint")]
#[cfg(feature = "libxml_xptr_locs")]
fn xml_xptr_get_end_point(
    obj: &XmlXPathObject,
    node: &mut Option<XmlGenericNodePtr>,
    indx: &mut usize,
) -> i32 {
    match obj.typ {
        XmlXPathObjectType::XPathPoint => {
            *node = obj.user.as_ref().and_then(|user| user.as_node()).copied();

            if obj.index <= 0 {
                *indx = 0;
            } else {
                *indx = obj.index as usize;
            }
            return 0;
        }
        XmlXPathObjectType::XPathRange => {
            *node = obj.user.as_ref().and_then(|user| user.as_node()).copied();

            if obj.index <= 0 {
                *indx = 0;
            } else {
                *indx = obj.index as usize;
            }
            return 0;
        }
        _ => {}
    }
    -1
}

/// Returns the @no'th element child of @cur or NULL
#[doc(alias = "xmlXPtrGetNthChild")]
unsafe fn xml_xptr_get_nth_child(cur: XmlGenericNodePtr, no: usize) -> Option<XmlGenericNodePtr> {
    if cur.element_type() == XmlElementType::XmlNamespaceDecl {
        return Some(cur);
    }
    let mut cur = cur.children();
    let mut i = 0;
    while i <= no {
        let now = cur?;
        if matches!(
            now.element_type(),
            XmlElementType::XmlElementNode
                | XmlElementType::XmlDocumentNode
                | XmlElementType::XmlHTMLDocumentNode
        ) {
            i += 1;
            if i == no {
                break;
            }
        }

        cur = now.next();
    }
    cur
}

/// Advance to the next element or text node in document order
/// TODO: add a stack for entering/exiting entities
///
/// Returns -1 in case of failure, 0 otherwise
#[doc(alias = "xmlXPtrAdvanceNode")]
#[cfg(feature = "libxml_xptr_locs")]
pub(crate) unsafe fn xml_xptr_advance_node(
    mut cur: XmlGenericNodePtr,
    level: *mut i32,
) -> Option<XmlGenericNodePtr> {
    unsafe {
        // next:
        'next: loop {
            if cur.element_type() == XmlElementType::XmlNamespaceDecl {
                return None;
            }
            if let Some(children) = cur.children() {
                cur = children;
                if !level.is_null() {
                    *level += 1;
                }
                // goto found;
                // found:
                if !matches!(
                    cur.element_type(),
                    XmlElementType::XmlElementNode
                        | XmlElementType::XmlTextNode
                        | XmlElementType::XmlDocumentNode
                        | XmlElementType::XmlHTMLDocumentNode
                        | XmlElementType::XmlCDATASectionNode
                ) {
                    if cur.element_type() == XmlElementType::XmlEntityRefNode {
                        // Shouldn't happen
                        // TODO
                        // goto skip;
                    } else {
                        // goto next;
                        continue 'next;
                    }
                } else {
                    return Some(cur);
                }
            }
            // skip:		/* This label should only be needed if something is wrong! */
            'skip: loop {
                if let Some(next) = cur.next() {
                    cur = next;
                    // goto found;
                } else {
                    loop {
                        if !level.is_null() {
                            *level -= 1;
                        }
                        cur = cur.parent()?;
                        if let Some(next) = cur.next() {
                            cur = next;
                            // goto found;
                            break;
                        }
                    }
                }

                // found:
                if !matches!(
                    cur.element_type(),
                    XmlElementType::XmlElementNode
                        | XmlElementType::XmlTextNode
                        | XmlElementType::XmlDocumentNode
                        | XmlElementType::XmlHTMLDocumentNode
                        | XmlElementType::XmlCDATASectionNode
                ) {
                    if cur.element_type() == XmlElementType::XmlEntityRefNode {
                        /* Shouldn't happen */
                        // TODO
                        // goto skip;
                        continue 'skip;
                    }
                    // goto next;
                    continue 'next;
                }

                break 'next Some(cur);
            }
        }
    }
}

/// Advance a point of the associated number of bytes (not UTF8 chars)
///
/// Returns -1 in case of failure, 0 otherwise
#[doc(alias = "xmlXPtrAdvanceChar")]
#[cfg(feature = "libxml_xptr_locs")]
unsafe fn xml_xptr_advance_char(
    node: &mut Option<XmlGenericNodePtr>,
    indx: &mut usize,
    mut bytes: i32,
) -> i32 {
    unsafe {
        use crate::tree::XmlNodePtr;

        let mut cur = *node;
        if cur.is_none_or(|cur| cur.element_type() == XmlElementType::XmlNamespaceDecl) {
            return -1;
        }
        let mut pos = *indx as i32;

        while bytes >= 0 {
            // First position to the beginning of the first text node
            // corresponding to this point
            while let Some(now) = cur.filter(|cur| {
                matches!(
                    cur.element_type(),
                    XmlElementType::XmlElementNode
                        | XmlElementType::XmlDocumentNode
                        | XmlElementType::XmlHTMLDocumentNode
                )
            }) {
                if pos > 0 {
                    cur = xml_xptr_get_nth_child(now, pos as usize);
                    pos = 0;
                } else {
                    cur = xml_xptr_advance_node(now, null_mut());
                    pos = 0;
                }
            }

            let Some(now) = cur else {
                *node = None;
                *indx = 0;
                return -1;
            };

            // if there is no move needed return the current value.
            if pos == 0 {
                pos = 1;
            }
            if bytes == 0 {
                *node = cur;
                *indx = pos as usize;
                return 0;
            }
            // We should have a text (or cdata) node ...
            let mut len = 0;
            if now.element_type() != XmlElementType::XmlElementNode {
                let cur = XmlNodePtr::try_from(now).unwrap();
                if let Some(content) = cur.content.as_deref() {
                    len = content.len() as i32;
                }
            }
            if pos > len {
                // Strange, the indx in the text node is greater than it's len
                STRANGE!();
                pos = len;
            }
            if pos + bytes >= len {
                bytes -= len - pos;
                cur = xml_xptr_advance_node(now, null_mut());
                pos = 0;
            } else if pos + bytes < len {
                pos += bytes;
                *node = cur;
                *indx = pos as usize;
                return 0;
            }
        }
        -1
    }
}

/// Computes the point coordinates of the last c_char of this point
///
/// Returns -1 in case of failure, 0 otherwise
#[doc(alias = "xmlXPtrGetLastChar")]
#[cfg(feature = "libxml_xptr_locs")]
unsafe fn xml_xptr_get_last_char(node: &mut Option<XmlGenericNodePtr>, indx: &mut usize) -> i32 {
    unsafe {
        use crate::tree::XmlNodePtr;

        let mut len = 0;

        if node.map_or(true, |node| {
            node.element_type() == XmlElementType::XmlNamespaceDecl
        }) {
            return -1;
        }
        let mut cur = *node;
        let pos = *indx;

        if matches!(
            cur.unwrap().element_type(),
            XmlElementType::XmlElementNode
                | XmlElementType::XmlDocumentNode
                | XmlElementType::XmlHTMLDocumentNode
        ) && pos > 0
        {
            cur = xml_xptr_get_nth_child(cur.unwrap(), pos);
        }
        while let Some(now) = cur {
            if let Some(last) = now.last() {
                cur = Some(last);
            } else if let Some(content) = XmlNodePtr::try_from(now)
                .ok()
                .as_deref()
                .filter(|now| now.element_type() != XmlElementType::XmlElementNode)
                .and_then(|now| now.content.as_deref())
            {
                len = content.len();
                break;
            } else {
                return -1;
            }
        }
        if cur.is_none() {
            return -1;
        }
        *node = cur;
        *indx = len;
        0
    }
}

/// Check whether the document contains @string at the position
/// (@start, @startindex) and limited by the (@end, @endindex) point
///
/// Returns -1 in case of failure, 0 if not found, 1 if found in which case
///            (@start, @startindex) will indicate the position of the beginning
///            of the range and (@end, @endindex) will indicate the end
///            of the range
#[doc(alias = "xmlXPtrMatchString")]
#[cfg(feature = "libxml_xptr_locs")]
unsafe fn xml_xptr_match_string(
    mut string: &str,
    start: XmlGenericNodePtr,
    startindex: usize,
    end: &mut Option<XmlGenericNodePtr>,
    endindex: &mut usize,
) -> i32 {
    unsafe {
        use crate::tree::XmlNodePtr;

        if start.element_type() == XmlElementType::XmlNamespaceDecl {
            return -1;
        }
        if end.map_or(true, |end| {
            end.element_type() == XmlElementType::XmlNamespaceDecl
        }) {
            return -1;
        }
        let mut cur = start;
        let mut pos = startindex - 1;
        let mut stringlen = string.len();

        while stringlen > 0 {
            if Some(cur) == *end && pos + stringlen > *endindex {
                return 0;
            }

            if cur.element_type() != XmlElementType::XmlElementNode {
                let cur = XmlNodePtr::try_from(cur).unwrap();
                if let Some(content) = cur.content.as_deref() {
                    let len = content.len();
                    if len >= pos + stringlen {
                        let is_match =
                            &content.as_bytes()[pos..pos + stringlen] == string.as_bytes();
                        if is_match {
                            *end = Some(cur.into());
                            *endindex = pos + stringlen;
                            return 1;
                        } else {
                            return 0;
                        }
                    } else {
                        let sub = len - pos;
                        let is_match =
                            content.as_bytes()[pos..pos + sub] == string.as_bytes()[..sub];
                        if is_match {
                            string = &string[sub..];
                            stringlen -= sub;
                        } else {
                            return 0;
                        }
                    }
                }
            }
            let Some(next) = xml_xptr_advance_node(cur, null_mut()) else {
                return 0;
            };
            cur = next;
            pos = 0;
        }
        1
    }
}

/// Search the next occurrence of @string within the document content
/// until the (@end, @endindex) point is reached
///
/// Returns -1 in case of failure, 0 if not found, 1 if found in which case
///            (@start, @startindex) will indicate the position of the beginning
///            of the range and (@end, @endindex) will indicate the end
///            of the range
#[doc(alias = "xmlXPtrSearchString")]
#[cfg(feature = "libxml_xptr_locs")]
unsafe fn xml_xptr_search_string(
    string: &str,
    start: &mut Option<XmlGenericNodePtr>,
    startindex: &mut usize,
    end: &mut Option<XmlGenericNodePtr>,
    endindex: &mut usize,
) -> i32 {
    unsafe {
        use crate::tree::XmlNodePtr;

        if start.map_or(true, |start| {
            start.element_type() == XmlElementType::XmlNamespaceDecl
        }) {
            return -1;
        }
        let mut cur = *start;
        let mut pos = *startindex - 1;
        let first = *string.as_bytes().first().unwrap_or(&0);

        while let Some(cur_node) = cur {
            if cur_node.element_type() != XmlElementType::XmlElementNode {
                let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                if let Some(content) = cur_node.content.as_deref() {
                    let len = content.len();
                    while pos <= len {
                        if first != 0 {
                            if let Some(p) = content[pos..].find(first as char) {
                                pos += p;
                                if xml_xptr_match_string(
                                    string,
                                    cur_node.into(),
                                    pos + 1,
                                    end,
                                    endindex,
                                ) != 0
                                {
                                    *start = cur;
                                    *startindex = pos + 1;
                                    return 1;
                                }
                                pos += 1;
                            } else {
                                pos = len + 1;
                            }
                        } else {
                            // An empty string is considered to match before each
                            // character of the string-value and after the final character.
                            *start = cur;
                            *startindex = pos + 1;
                            *end = cur;
                            *endindex = pos + 1;
                            return 1;
                        }
                    }
                }
            }
            if cur == *end && pos >= *endindex {
                return 0;
            }
            cur = xml_xptr_advance_node(cur_node, null_mut());
            pos = 1;
        }
        0
    }
}

/// Function implementing the string-range() function
/// range as described in 5.4.2
///
/// ------------------------------
/// [Definition: For each location in the location-set argument,
/// string-range returns a set of string ranges, a set of substrings in a
/// string. Specifically, the string-value of the location is searched for
/// substrings that match the string argument, and the resulting location-set
/// will contain a range location for each non-overlapping match.]
/// An empty string is considered to match before each character of the
/// string-value and after the final character. Whitespace in a string
/// is matched literally, with no normalization except that provided by
/// XML for line ends. The third argument gives the position of the first
/// character to be in the resulting range, relative to the start of the
/// match. The default value is 1, which makes the range start immediately
/// before the first character of the matched string. The fourth argument
/// gives the number of characters in the range; the default is that the
/// range extends to the end of the matched string.
///
/// Element boundaries, as well as entire embedded nodes such as processing
/// instructions and comments, are ignored as defined in [XPath].
///
/// If the string in the second argument is not found in the string-value
/// of the location, or if a value in the third or fourth argument indicates
/// a string that is beyond the beginning or end of the document, the
/// expression fails.
///
/// The points of the range-locations in the returned location-set will
/// all be character points.
/// ------------------------------
#[doc(alias = "xmlXPtrStringRangeFunction")]
#[cfg(feature = "libxml_xptr_locs")]
unsafe fn xml_xptr_string_range_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    unsafe {
        let mut startindex = 0usize;
        let mut endindex = 0usize;
        let mut fendindex: usize;
        let mut start = None;
        let mut end = None;
        let mut fend;
        let mut newset = None;
        let mut position = None;
        let mut number = None;
        let mut found: i32;
        let mut pos = 0;
        let mut num: i32 = 0;

        // Grab the arguments
        if !(2..=4).contains(&nargs) {
            xml_xpath_err(Some(&mut *ctxt), XmlXPathError::XPathInvalidArity as i32);
            return;
        }

        'goto_error: {
            if nargs >= 4 {
                if ctxt
                    .value()
                    .is_none_or(|value| value.typ != XmlXPathObjectType::XPathNumber)
                {
                    xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidType as i32);
                    // goto error;
                    break 'goto_error;
                }
                number = ctxt.value_pop();
                if let Some(number) = number.as_ref() {
                    num = number.floatval as i32;
                }
            }
            if nargs >= 3 {
                if ctxt
                    .value()
                    .is_none_or(|value| value.typ != XmlXPathObjectType::XPathNumber)
                {
                    xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidType as i32);
                    // goto error;
                    break 'goto_error;
                }
                position = ctxt.value_pop();
                if let Some(position) = position.as_ref() {
                    pos = position.floatval as i32;
                }
            }
            if ctxt
                .value()
                .is_none_or(|value| value.typ != XmlXPathObjectType::XPathString)
            {
                xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidType as i32);
                // goto error;
                break 'goto_error;
            }
            let string = ctxt.value_pop().unwrap();
            if ctxt.value().is_none_or(|value| {
                !matches!(
                    value.typ,
                    XmlXPathObjectType::XPathLocationset | XmlXPathObjectType::XPathNodeset
                )
            }) {
                xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidType as i32);
                // goto error;
                break 'goto_error;
            }
            let mut set = ctxt.value_pop().unwrap();
            newset = Some(XmlLocationSet::new(None));

            if set.nodesetval.is_none() {
                // goto error;
                break 'goto_error;
            }
            if set.typ == XmlXPathObjectType::XPathNodeset {
                // First convert to a location set
                set = xml_xptr_new_location_set_node_set(set.nodesetval.as_deref());
            }
            let oldset = set
                .user
                .as_ref()
                .and_then(|user| user.as_location_set())
                .unwrap();

            // The loop is to search for each element in the location set
            // the list of location set corresponding to that search
            for loc in &oldset.loc_tab {
                xml_xptr_get_start_point(loc, &mut start, &mut startindex);
                xml_xptr_get_end_point(loc, &mut end, &mut endindex);
                xml_xptr_advance_char(&mut start, &mut startindex, 0);
                xml_xptr_get_last_char(&mut end, &mut endindex);

                while {
                    fend = end;
                    fendindex = endindex;
                    found = xml_xptr_search_string(
                        string.stringval.as_deref().unwrap(),
                        &mut start,
                        &mut startindex,
                        &mut fend,
                        &mut fendindex,
                    );
                    if found == 1 {
                        if position.is_none() {
                            newset.as_mut().unwrap().push(
                                xml_xptr_new_range(
                                    start.unwrap(),
                                    startindex as i32,
                                    fend.unwrap(),
                                    fendindex as i32,
                                )
                                .map(Rc::new)
                                .unwrap(),
                            );
                        } else if xml_xptr_advance_char(&mut start, &mut startindex, pos - 1) == 0 {
                            if number.is_some() && num > 0 {
                                let mut rend = start;
                                let mut rindx = startindex - 1;
                                if xml_xptr_advance_char(&mut rend, &mut rindx, num) == 0 {
                                    newset.as_mut().unwrap().push(
                                        xml_xptr_new_range(
                                            start.unwrap(),
                                            startindex as i32,
                                            rend.unwrap(),
                                            rindx as i32,
                                        )
                                        .map(Rc::new)
                                        .unwrap(),
                                    );
                                }
                            } else if number.is_some() && num <= 0 {
                                newset.as_mut().unwrap().push(
                                    xml_xptr_new_range(
                                        start.unwrap(),
                                        startindex as i32,
                                        start.unwrap(),
                                        startindex as i32,
                                    )
                                    .map(Rc::new)
                                    .unwrap(),
                                );
                            } else {
                                newset.as_mut().unwrap().push(
                                    xml_xptr_new_range(
                                        start.unwrap(),
                                        startindex as i32,
                                        fend.unwrap(),
                                        fendindex as i32,
                                    )
                                    .map(Rc::new)
                                    .unwrap(),
                                );
                            }
                        }
                        start = fend;
                        startindex = fendindex;
                        if string.stringval.as_deref().unwrap().is_empty() {
                            startindex += 1;
                        }
                    }

                    found == 1
                } {}
            }
        }

        // Save the new value and cleanup
        // error:
        if let Some(newset) = newset {
            (*ctxt).value_push(xml_xptr_wrap_location_set(newset));
        }
    }
}

/// Create a new xmlXPathObjectPtr of type point
///
/// Returns the newly created object.
#[doc(alias = "xmlXPtrNewPoint")]
#[cfg(feature = "libxml_xptr_locs")]
fn xml_xptr_new_point(node: XmlGenericNodePtr, indx: i32) -> Option<XmlXPathObject> {
    use crate::xpath::XmlXPathObjectUserData;

    if indx < 0 {
        return None;
    }

    let mut ret = XmlXPathObject::default();
    ret.typ = XmlXPathObjectType::XPathPoint;
    ret.user = Some(XmlXPathObjectUserData::Node(node));
    ret.index = indx;
    Some(ret)
}

/// Function implementing start-point() operation
/// as described in 5.4.3
/// ----------------
/// location-set start-point(location-set)
///
/// For each location x in the argument location-set, start-point adds a
/// location of type point to the result location-set. That point represents
/// the start point of location x and is determined by the following rules:
///
/// - If x is of type point, the start point is x.
/// - If x is of type range, the start point is the start point of x.
/// - If x is of type root, element, text, comment, or processing instruction,
/// - the container node of the start point is x and the index is 0.
/// - If x is of type attribute or namespace, the function must signal a
///   syntax error.
/// ----------------
#[doc(alias = "xmlXPtrStartPointFunction")]
#[cfg(feature = "libxml_xptr_locs")]
fn xml_xptr_start_point_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    if check_arity(ctxt, nargs, 1).is_err() {
        return;
    }
    if ctxt.value().is_none_or(|value| {
        !matches!(
            value.typ,
            XmlXPathObjectType::XPathLocationset | XmlXPathObjectType::XPathNodeset
        )
    }) {
        xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidType as i32);
        return;
    }

    let mut obj = ctxt.value_pop().unwrap();
    if obj.typ == XmlXPathObjectType::XPathNodeset {
        // First convert to a location set
        obj = xml_xptr_new_location_set_node_set(obj.nodesetval.as_deref());
    }

    let mut newset = XmlLocationSet::new(None);
    let oldset = obj.user.as_ref().and_then(|user| user.as_location_set());

    if let Some(oldset) = oldset {
        for tmp in &oldset.loc_tab {
            let mut point = None;
            match tmp.typ {
                XmlXPathObjectType::XPathPoint => {
                    point = xml_xptr_new_point(
                        tmp.user
                            .as_ref()
                            .and_then(|user| user.as_node())
                            .copied()
                            .unwrap(),
                        tmp.index,
                    )
                }
                XmlXPathObjectType::XPathRange => {
                    let node = tmp.user.as_ref().and_then(|user| user.as_node()).copied();

                    if let Some(node) = node {
                        if matches!(
                            node.element_type(),
                            XmlElementType::XmlAttributeNode | XmlElementType::XmlNamespaceDecl
                        ) {
                            xml_xpath_err(Some(ctxt), XmlXPathError::XPtrSyntaxError as i32);
                            return;
                        }
                        point = xml_xptr_new_point(node, tmp.index);
                    }
                }
                _ => {
                    /*** Should we raise an error ?
                    xmlXPathFreeObject(obj);
                    xmlXPathFreeObject(newset);
                    XP_ERROR!(Some(&mut *ctxt), xmlXPathError::XPATH_INVALID_TYPE as i32);
                    ***/
                }
            }
            if let Some(point) = point {
                newset.push(Rc::new(point));
            }
        }
    }
    ctxt.value_push(xml_xptr_wrap_location_set(newset));
}

// /// Count the number of location children of @node or the length of the
// /// string value in case of text/PI/Comments nodes
// ///
// /// Returns the number of location children
// #[doc(alias = "xmlXPtrNbLocChildren")]
// #[cfg(feature = "libxml_xptr_locs")]
// unsafe fn xml_xptr_nb_loc_children(node: XmlGenericNodePtr) -> i32 {
//     unsafe {
//         use crate::tree::XmlNodePtr;

//         let mut ret: i32 = 0;
//         // if node.is_null() {
//         //     return -1;
//         // }
//         match node.element_type() {
//             XmlElementType::XmlHTMLDocumentNode
//             | XmlElementType::XmlDocumentNode
//             | XmlElementType::XmlElementNode => {
//                 let mut node = node.children();
//                 while let Some(now) = node {
//                     if now.element_type() == XmlElementType::XmlElementNode {
//                         ret += 1;
//                     }
//                     node = now.next();
//                 }
//             }
//             XmlElementType::XmlAttributeNode => return -1,
//             XmlElementType::XmlPINode
//             | XmlElementType::XmlCommentNode
//             | XmlElementType::XmlTextNode
//             | XmlElementType::XmlCDATASectionNode
//             | XmlElementType::XmlEntityRefNode => {
//                 let node = XmlNodePtr::try_from(node).unwrap();
//                 ret = xml_strlen(node.content);
//             }
//             _ => return -1,
//         }
//         ret
//     }
// }

/// Function implementing end-point() operation
/// as described in 5.4.3
/// ----------------------------
/// location-set end-point(location-set)
///
/// For each location x in the argument location-set, end-point adds a
/// location of type point to the result location-set. That point represents
/// the end point of location x and is determined by the following rules:
///
/// - If x is of type point, the resulting point is x.
/// - If x is of type range, the resulting point is the end point of x.
/// - If x is of type root or element, the container node of the resulting
///   point is x and the index is the number of location children of x.
/// - If x is of type text, comment, or processing instruction, the container
///   node of the resulting point is x and the index is the length of the
///   string-value of x.
/// - If x is of type attribute or namespace, the function must signal a
///   syntax error.
/// ----------------------------
#[doc(alias = "xmlXPtrEndPointFunction")]
#[cfg(feature = "libxml_xptr_locs")]
fn xml_xptr_end_point_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    if check_arity(ctxt, nargs, 1).is_err() {
        return;
    }
    if ctxt.value().is_none_or(|value| {
        !matches!(
            value.typ,
            XmlXPathObjectType::XPathLocationset | XmlXPathObjectType::XPathNodeset
        )
    }) {
        xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidType as i32);
        return;
    }

    let mut obj = ctxt.value_pop().unwrap();
    if obj.typ == XmlXPathObjectType::XPathNodeset {
        // First convert to a location set
        obj = xml_xptr_new_location_set_node_set(obj.nodesetval.as_deref());
    }

    let mut newset = XmlLocationSet::new(None);
    let oldset = obj.user.as_ref().and_then(|user| user.as_location_set());

    if let Some(oldset) = oldset {
        for tmp in &oldset.loc_tab {
            let mut point = None;
            match tmp.typ {
                XmlXPathObjectType::XPathPoint => {
                    point = xml_xptr_new_point(
                        tmp.user
                            .as_ref()
                            .and_then(|user| user.as_node())
                            .copied()
                            .unwrap(),
                        tmp.index,
                    )
                }
                XmlXPathObjectType::XPathRange => {
                    let node = tmp.user2.as_ref().and_then(|user| user.as_node()).copied();

                    if let Some(node) = node {
                        if matches!(
                            node.element_type(),
                            XmlElementType::XmlAttributeNode | XmlElementType::XmlNamespaceDecl
                        ) {
                            xml_xpath_err(Some(ctxt), XmlXPathError::XPtrSyntaxError as i32);
                            return;
                        }
                        point = xml_xptr_new_point(node, tmp.index2);
                    } else if tmp.user.is_none() {
                        // The following code seems that always fails...
                        // point = xml_xptr_new_point(node, xml_xptr_nb_loc_children(node));
                    }
                }
                _ => {
                    // Should we raise an error ?
                    // xmlXPathFreeObject(obj);
                    // xmlXPathFreeObject(newset);
                    // XP_ERROR!(Some(ctxt), xmlXPathError::XPATH_INVALID_TYPE as i32);
                }
            }
            if let Some(point) = point {
                newset.push(Rc::new(point));
            }
        }
    }
    ctxt.value_push(xml_xptr_wrap_location_set(newset));
}

/// Function implementing here() operation as described in 5.4.3
#[doc(alias = "xmlXPtrHereFunction")]
#[cfg(feature = "libxml_xptr_locs")]
fn xml_xptr_here_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    if check_arity(ctxt, nargs, 0).is_err() {
        return;
    }

    let Some(here) = ctxt.context.here else {
        xml_xpath_err(Some(&mut *ctxt), XmlXPathError::XPtrSyntaxError as i32);
        return;
    };

    ctxt.value_push(xml_xptr_new_location_set_nodes(here, None));
}

/// Function implementing origin() operation
/// as described in 5.4.3
#[doc(alias = "xmlXPtrOriginFunction")]
#[cfg(feature = "libxml_xptr_locs")]
fn xml_xptr_origin_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    if check_arity(ctxt, nargs, 0).is_err() {
        return;
    }

    let Some(origin) = ctxt.context.origin else {
        xml_xpath_err(Some(ctxt), XmlXPathError::XPtrSyntaxError as i32);
        return;
    };

    ctxt.value_push(xml_xptr_new_location_set_nodes(origin, None));
}

/// Create a new XPointer context.  
/// Please refer to the document of `xmlXPtrNewContext` in original libxml2.
///
/// # Safety
/// - A valid pointer generated by the API for this crate must be given.
/// - If the context generation fails, this method may return null.
pub unsafe fn xml_xptr_new_context(
    doc: Option<XmlDocPtr>,
    here: Option<XmlGenericNodePtr>,
    origin: Option<XmlGenericNodePtr>,
) -> XmlXPathContextPtr {
    unsafe {
        let ret: XmlXPathContextPtr = xml_xpath_new_context(doc);
        if ret.is_null() {
            return ret;
        }
        #[cfg(feature = "libxml_xptr_locs")]
        {
            (*ret).xptr = 1;
            (*ret).here = here;
            (*ret).origin = origin;
        }
        (*ret).register_function("range".into(), Some(xml_xptr_range_function));
        (*ret).register_function("range-inside".into(), Some(xml_xptr_range_inside_function));
        (*ret).register_function("string-range".into(), Some(xml_xptr_string_range_function));
        (*ret).register_function("start-point".into(), Some(xml_xptr_start_point_function));
        (*ret).register_function("end-point".into(), Some(xml_xptr_end_point_function));
        (*ret).register_function("here".into(), Some(xml_xptr_here_function));
        (*ret).register_function("origin".into(), Some(xml_xptr_origin_function));

        ret
    }
}

/// Handle a redefinition of attribute error
#[doc(alias = "xmlXPtrErr")]
macro_rules! xml_xptr_err {
    ($ctxt:expr, $error:expr, $msg:literal) => {
        xml_xptr_err!(@inner, $ctxt, $error, $msg, None);
    };
    ($ctxt:expr, $error:expr, $msg:literal, $extra:expr) => {
        let msg = format!($msg, $extra);
        xml_xptr_err!(@inner, $ctxt, $error, &msg, Some($extra.to_owned().into()));
    };
    (@inner, $ctxt:expr, $error:expr, $msg:expr, $extra:expr) => {
        let error: XmlParserErrors = $error;
        $ctxt.error = error as i32;
        // cleanup current last error
        $ctxt.context.last_error.reset();

        $ctxt.context.last_error.domain = XmlErrorDomain::XmlFromXPointer;
        $ctxt.context.last_error.code = error;
        $ctxt.context.last_error.level = XmlErrorLevel::XmlErrError;
        $ctxt.context.last_error.str1 = Some($ctxt.base.to_string().into());
        $ctxt.context.last_error.int1 = $ctxt.cur as _;
        $ctxt.context.last_error.node = $ctxt.context.debug_node;
        if let Some(error) = $ctxt.context.error {
            error(
                $ctxt.context.user_data.clone(),
                &$ctxt.context.last_error,
            );
        } else {
            __xml_raise_error!(
                None,
                None,
                None,
                null_mut(),
                $ctxt.context.debug_node,
                XmlErrorDomain::XmlFromXPointer,
                error,
                XmlErrorLevel::XmlErrError,
                None,
                0,
                $extra,
                Some($ctxt.base.to_string().into()),
                None,
                $ctxt.cur as _,
                0,
                Some($msg),
            );
        }
    };
}

/// Move the current node of the nodeset on the stack to the given child if found
#[doc(alias = "xmlXPtrGetChildNo")]
unsafe fn xml_xptr_get_child_no(ctxt: &mut XmlXPathParserContext, indx: i32) {
    unsafe {
        if ctxt
            .value()
            .is_none_or(|value| value.typ != XmlXPathObjectType::XPathNodeset)
        {
            xml_xpath_err(
                Some(&mut *ctxt),
                crate::xpath::XmlXPathError::XPathInvalidType as i32,
            );
            return;
        };
        let mut obj = ctxt.value_pop().unwrap();
        let Some(oldset) = obj.nodesetval.as_deref_mut().filter(|_| indx > 0) else {
            ctxt.value_push(xml_xpath_new_node_set(None));
            return;
        };
        if oldset.node_tab.len() != 1 {
            ctxt.value_push(xml_xpath_new_node_set(None));
            return;
        }
        let Some(cur) = xml_xptr_get_nth_child(oldset.node_tab[0], indx as usize) else {
            ctxt.value_push(xml_xpath_new_node_set(None));
            return;
        };
        oldset.node_tab[0] = cur;
        ctxt.value_push(obj);
    }
}

/// `ChildSeq ::= '/1' ('/' [0-9]*)* | Name ('/' [0-9]*)+`
///
/// Parse and evaluate a Child Sequence. This routine also handle the
/// case of a Bare Name used to get a document ID.
#[doc(alias = "xmlXPtrEvalChildSeq")]
unsafe fn xml_xptr_eval_child_seq(ctxt: &mut XmlXPathParserContext, name: Option<&str>) {
    unsafe {
        // XPointer don't allow by syntax to address in multirooted trees
        // this might prove useful in some cases, warn about it.
        if name.is_none() && ctxt.current_char() == Some('/') && ctxt.nth_byte(1) != Some(b'1') {
            xml_xptr_err!(
                ctxt,
                XmlParserErrors::XmlXPtrChildseqStart,
                "warning: ChildSeq not starting by /1\n"
            );
        }

        if let Some(name) = name {
            ctxt.value_push(xml_xpath_new_string(Some(name)));
            xml_xpath_id_function(&mut *ctxt, 1);
            if ctxt.error != XmlXPathError::XPathExpressionOK as i32 {
                return;
            };
        }

        while ctxt.current_char() == Some('/') {
            let mut child: i32 = 0;
            let mut overflow = false;
            ctxt.next_char();

            while let Some(cur) = ctxt.current_char().filter(|c| c.is_ascii_digit()) {
                let (c, f) = child.overflowing_mul(10);
                overflow |= f;
                let (c, f) = c.overflowing_add(cur as i32 - b'0' as i32);
                overflow |= f;
                child = c;
                ctxt.next_char();
            }
            if overflow {
                child = 0;
            }
            xml_xptr_get_child_no(ctxt, child);
        }
    }
}

/// ```text
/// XPtrPart ::= 'xpointer' '(' XPtrExpr ')'
///            | Scheme '(' SchemeSpecificExpr ')'
///
/// Scheme   ::=  NCName - 'xpointer' [VC: Non-XPointer schemes]
///
/// SchemeSpecificExpr ::= StringWithBalancedParens
///
/// StringWithBalancedParens ::=
///              [^()]* ('(' StringWithBalancedParens ')' [^()]*)*
///              [VC: Parenthesis escaping]
///
/// XPtrExpr ::= Expr [VC: Parenthesis escaping]
/// ```
///
/// VC: Parenthesis escaping:
///   The end of an XPointer part is signaled by the right parenthesis ")"
///   character that is balanced with the left parenthesis "(" character
///   that began the part. Any unbalanced parenthesis character inside the
///   expression, even within literals, must be escaped with a circumflex (^)
///   character preceding it. If the expression contains any literal
///   occurrences of the circumflex, each must be escaped with an additional
///   circumflex (that is, ^^). If the unescaped parentheses in the expression
///   are not balanced, a syntax error results.
///
/// Parse and evaluate an XPtrPart. Basically it generates the unescaped
/// string and if the scheme is 'xpointer' it will call the XPath interpreter.
///
/// TODO: there is no new scheme registration mechanism
#[doc(alias = "xmlXPtrEvalXPtrPart")]
unsafe fn xml_xptr_eval_xptr_part(ctxt: &mut XmlXPathParserContext, mut name: *mut XmlChar) {
    unsafe {
        if name.is_null() {
            name = ctxt.parse_name().map_or(null_mut(), |name| {
                xml_strndup(name.as_ptr(), name.len() as i32)
            });
        }
        if name.is_null() {
            xml_xpath_err(Some(&mut *ctxt), XmlXPathError::XPathExprError as i32);
            return;
        }

        if ctxt.current_char() != Some('(') {
            xml_free(name as _);
            xml_xpath_err(Some(&mut *ctxt), XmlXPathError::XPathExprError as i32);
            return;
        }
        ctxt.next_char();
        let mut level = 1;

        let mut buffer = String::with_capacity(ctxt.current_str().len());
        while let Some(c) = ctxt.current_char() {
            if c == ')' {
                level -= 1;
                if level == 0 {
                    ctxt.next_char();
                    break;
                }
            } else if c == '(' {
                level += 1;
            } else if c == '^' && matches!(ctxt.nth_byte(1), Some(b')' | b'(' | b'^')) {
                ctxt.next_char();
            }
            if let Some(c) = ctxt.next_char() {
                buffer.push(c);
            }
        }

        if level != 0 && ctxt.current_char().is_none() {
            xml_free(name as _);
            xml_xpath_err(Some(&mut *ctxt), XmlXPathError::XPtrSyntaxError as i32);
            return;
        }

        if xml_str_equal(name, c"xpointer".as_ptr() as _)
            || xml_str_equal(name, c"xpath1".as_ptr() as _)
        {
            let old_base = replace(&mut ctxt.base, buffer.into_boxed_str());
            let old_cur = ctxt.cur;
            ctxt.cur = 0;
            // To evaluate an xpointer scheme element (4.3) we need:
            //   context initialized to the root
            //   context position initialized to 1
            //   context size initialized to 1
            ctxt.context.node = ctxt.context.doc.map(|doc| doc.into());
            ctxt.context.proximity_position = 1;
            ctxt.context.context_size = 1;
            #[cfg(feature = "libxml_xptr_locs")]
            {
                ctxt.xptr = xml_str_equal(name, c"xpointer".as_ptr() as _) as i32;
            }
            ctxt.evaluate_expression();
            ctxt.base = old_base;
            ctxt.cur = old_cur;
        } else if xml_str_equal(name, c"element".as_ptr() as _) {
            let old_base = replace(&mut ctxt.base, buffer.into_boxed_str());
            let old_cur = ctxt.cur;
            ctxt.cur = 0;

            let name2: *mut XmlChar;

            if ctxt.base.starts_with('/') {
                xml_xpath_root(ctxt);
                xml_xptr_eval_child_seq(ctxt, None);
            } else {
                name2 = ctxt.parse_name().map_or(null_mut(), |name| {
                    xml_strndup(name.as_ptr(), name.len() as i32)
                });
                if name2.is_null() {
                    ctxt.base = old_base;
                    ctxt.cur = old_cur;
                    xml_free(name as _);
                    xml_xpath_err(Some(&mut *ctxt), XmlXPathError::XPathExprError as i32);
                    return;
                }
                xml_xptr_eval_child_seq(
                    ctxt,
                    Some(
                        CStr::from_ptr(name2 as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                    ),
                );
                xml_free(name2 as _);
            }
            ctxt.base = old_base;
            ctxt.cur = old_cur;
        } else if xml_str_equal(name, c"xmlns".as_ptr() as _) {
            let old_base = replace(&mut ctxt.base, buffer.into_boxed_str());
            let old_cur = ctxt.cur;
            ctxt.cur = 0;

            let Some(prefix) = ctxt.parse_ncname() else {
                ctxt.base = old_base;
                ctxt.cur = old_cur;
                xml_free(name as _);
                xml_xpath_err(Some(ctxt), XmlXPathError::XPtrSyntaxError as i32);
                return;
            };
            ctxt.skip_blanks();
            if ctxt.current_char() != Some('=') {
                ctxt.base = old_base;
                ctxt.cur = old_cur;
                xml_free(name as _);
                xml_xpath_err(Some(ctxt), XmlXPathError::XPtrSyntaxError as i32);
                return;
            }
            ctxt.next_char();
            ctxt.skip_blanks();

            let ns_uri = ctxt.current_str().to_owned();
            ctxt.context.register_ns(&prefix, Some(&ns_uri));
            ctxt.base = old_base;
            ctxt.cur = old_cur;
        } else {
            xml_xptr_err!(
                ctxt,
                XmlParserErrors::XmlXPtrUnknownScheme,
                "unsupported scheme '{}'\n",
                CStr::from_ptr(name as *const i8).to_string_lossy()
            );
        }
        xml_free(name as _);
    }
}

/// `FullXPtr ::= XPtrPart (S? XPtrPart)*`
///
/// As the specs says:
/// -----------
/// When multiple XPtrParts are provided, they must be evaluated in
/// left-to-right order. If evaluation of one part fails, the nexti
/// is evaluated. The following conditions cause XPointer part failure:
///
/// - An unknown scheme
/// - A scheme that does not locate any sub-resource present in the resource
/// - A scheme that is not applicable to the media type of the resource
///
/// The XPointer application must consume a failed XPointer part and
/// attempt to evaluate the next one, if any. The result of the first
/// XPointer part whose evaluation succeeds is taken to be the fragment
/// located by the XPointer as a whole. If all the parts fail, the result
/// for the XPointer as a whole is a sub-resource error.
/// -----------
///
/// Parse and evaluate a Full XPtr i.e. possibly a cascade of XPath based
/// expressions or other schemes.
#[doc(alias = "xmlXPtrEvalFullXPtr")]
unsafe fn xml_xptr_eval_full_xptr(ctxt: &mut XmlXPathParserContext, mut name: *mut XmlChar) {
    unsafe {
        if name.is_null() {
            name = ctxt.parse_name().map_or(null_mut(), |name| {
                xml_strndup(name.as_ptr(), name.len() as i32)
            });
        }
        if name.is_null() {
            xml_xpath_err(Some(&mut *ctxt), XmlXPathError::XPathExprError as i32);
            return;
        }
        while !name.is_null() {
            ctxt.error = XmlXPathError::XPathExpressionOK as i32;
            xml_xptr_eval_xptr_part(ctxt, name);

            // in case of syntax error, break here
            if ctxt.error != XmlXPathError::XPathExpressionOK as i32
                && ctxt.error != XmlParserErrors::XmlXPtrUnknownScheme as i32
            {
                return;
            }

            // If the returned value is a non-empty nodeset or location set, return here.
            if let Some(obj) = ctxt.value() {
                match obj.typ {
                    #[cfg(feature = "libxml_xptr_locs")]
                    XmlXPathObjectType::XPathLocationset => {
                        let loc = obj.user.as_ref().and_then(|user| user.as_location_set());

                        if loc.is_some_and(|loc| !loc.loc_tab.is_empty()) {
                            return;
                        }
                    }
                    XmlXPathObjectType::XPathNodeset => {
                        let loc = obj.nodesetval.as_deref();
                        if loc.is_some_and(|l| !l.is_empty()) {
                            return;
                        }
                    }
                    _ => {}
                }

                // Evaluating to improper values is equivalent to
                // a sub-resource error, clean-up the stack
                while ctxt.value_pop().is_some() {}
            }

            // Is there another XPointer part.
            ctxt.skip_blanks();
            name = ctxt.parse_name().map_or(null_mut(), |name| {
                xml_strndup(name.as_ptr(), name.len() as i32)
            });
        }
    }
}

/// `XPointer ::= Name | ChildSeq | FullXPtr`
///
/// Parse and evaluate an XPointer
#[doc(alias = "xmlXPtrEvalXPointer")]
unsafe fn xml_xptr_eval_xpointer(ctxt: &mut XmlXPathParserContext) {
    unsafe {
        ctxt.skip_blanks();
        if ctxt.current_char() == Some('/') {
            xml_xpath_root(ctxt);
            xml_xptr_eval_child_seq(ctxt, None);
        } else {
            let name: *mut XmlChar = ctxt.parse_name().map_or(null_mut(), |name| {
                xml_strndup(name.as_ptr(), name.len() as i32)
            });
            if name.is_null() {
                xml_xpath_err(Some(ctxt), XmlXPathError::XPathExprError as i32);
                return;
            }
            if ctxt.current_char() == Some('(') {
                xml_xptr_eval_full_xptr(ctxt, name);
                // Short evaluation
                return;
            } else {
                // this handle both Bare Names and Child Sequences
                xml_xptr_eval_child_seq(
                    ctxt,
                    Some(CStr::from_ptr(name as *const i8).to_string_lossy().as_ref()),
                );
                xml_free(name as _);
            }
        }
        ctxt.skip_blanks();
        if ctxt.current_char().is_some() {
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathExprError as i32);
        }
    }
}

/// Evaluate the XPath Location Path in the given context.  
/// Please refer to the document of `xmlXPtrEval` in original libxml2.
///
/// # Safety
/// - A valid pointer generated by the API for this crate must be given.
/// - If the evaluation fails or arguments is invalid, this method may return null.
pub unsafe fn xml_xptr_eval(xpath: &str, ctx: &mut XmlXPathContext) -> Option<XmlXPathObject> {
    unsafe {
        let mut res = None;
        let mut stack: i32 = 0;

        xml_init_parser();

        let mut ctxt = XmlXPathParserContext::new(xpath, ctx);
        xml_xptr_eval_xpointer(&mut ctxt);

        #[cfg(feature = "libxml_xptr_locs")]
        let f = ctxt.value().is_some_and(|value| {
            !matches!(
                value.typ,
                XmlXPathObjectType::XPathLocationset | XmlXPathObjectType::XPathNodeset
            )
        });
        #[cfg(not(feature = "libxml_xptr_locs"))]
        let f = ctxt
            .value()
            .is_some_and(|value| (*value).typ != XmlXPathObjectType::XPathNodeset);
        if f {
            xml_xptr_err!(
                &mut ctxt,
                XmlParserErrors::XmlXPtrEvalFailed,
                "xmlXPtrEval: evaluation failed to return a node set\n"
            );
        } else {
            res = ctxt.value_pop();
        }

        while let Some(tmp) = ctxt.value_pop() {
            if tmp.typ == XmlXPathObjectType::XPathNodeset {
                // Evaluation may push a root nodeset which is unused
                let set = tmp.nodesetval.as_deref();
                if set.is_none_or(|s| {
                    s.len() != 1 || s.get(0) != ctxt.context.doc.map(|doc| doc.into())
                }) {
                    stack += 1;
                }
            } else {
                stack += 1;
            }
        }
        if stack != 0 {
            xml_xptr_err!(
                &mut ctxt,
                XmlParserErrors::XmlXPtrExtraObjects,
                "xmlXPtrEval: object(s) left on the eval stack\n"
            );
        }
        if ctxt.error != XmlXPathError::XPathExpressionOK as i32 {
            res = None;
        }

        res
    }
}

/// Implement the range-to() XPointer function
///
/// Obsolete. range-to is not a real function but a special type of location
/// step which is handled in xpath.c.
#[doc(alias = "xmlXPtrRangeToFunction")]
#[cfg(feature = "libxml_xptr_locs")]
pub fn xml_xptr_range_to_function(ctxt: &mut XmlXPathParserContext, _nargs: i32) {
    xml_xpath_err(Some(ctxt), XmlXPathError::XPathExprError as i32);
}

// /// Build a node list tree copy of the range
// ///
// /// Returns an xmlNodePtr list or NULL. The caller has to free the node tree.
// #[doc(alias = "xmlXPtrBuildRangeNodeList")]
// #[cfg(feature = "libxml_xptr_locs")]
// unsafe fn xml_xptr_build_range_node_list(range: XmlXPathObjectPtr) -> Option<XmlGenericNodePtr> {
//     unsafe {
//         use crate::tree::{
//             XmlGenericNodePtr, XmlNodePtr, xml_copy_node, xml_new_text, xml_new_text_len,
//         };
//         let mut list = None;
//         let mut last: Option<XmlGenericNodePtr> = None;
//         let mut parent: Option<XmlGenericNodePtr> = None;

//         if range.is_null() {
//             return None;
//         }
//         if (*range).typ != XmlXPathObjectType::XPathRange {
//             return None;
//         }
//         let start = XmlGenericNodePtr::from_raw((*range).user as *mut XmlNode);

//         if start.is_none_or(|start| start.element_type() == XmlElementType::XmlNamespaceDecl) {
//             return None;
//         }
//         let Some(mut end) = XmlGenericNodePtr::from_raw((*range).user2 as *mut XmlNode) else {
//             return xml_copy_node(start.unwrap(), 1);
//         };
//         if end.element_type() == XmlElementType::XmlNamespaceDecl {
//             return None;
//         }

//         let mut cur = start;
//         let mut index1 = (*range).index;
//         let mut index2 = (*range).index2;
//         while let Some(cur_node) = cur {
//             if cur_node == end {
//                 if cur_node.element_type() == XmlElementType::XmlTextNode {
//                     let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
//                     let mut content: *const XmlChar = cur_node.content;
//                     let mut len: i32;

//                     let tmp = if content.is_null() {
//                         xml_new_text_len(null_mut(), 0)
//                     } else {
//                         len = index2;
//                         if cur == start && index1 > 1 {
//                             content = content.add(index1 as usize - 1);
//                             len -= index1 - 1;
//                             // index1 = 0;
//                         } else {
//                             len = index2;
//                         }
//                         xml_new_text_len(content, len)
//                     };
//                     // single sub text node selection
//                     if list.is_none() {
//                         return tmp.map(|node| node.into());
//                     }
//                     // prune and return full set
//                     if let Some(last) = last {
//                         last.add_next_sibling(tmp.unwrap().into());
//                     } else {
//                         parent.unwrap().add_child(tmp.unwrap().into());
//                     }
//                     return list;
//                 } else {
//                     let tmp = xml_copy_node(cur_node, 0);
//                     if list.is_some() {
//                         if let Some(last) = last {
//                             parent = last.add_next_sibling(tmp.unwrap());
//                         } else {
//                             parent = parent.unwrap().add_child(tmp.unwrap());
//                         }
//                     } else {
//                         list = tmp;
//                         parent = tmp;
//                     }
//                     last = None;

//                     if index2 > 1 {
//                         end = xml_xptr_get_nth_child(cur_node, index2 as usize - 1).unwrap();
//                         index2 = 0;
//                     }
//                     if cur == start && index1 > 1 {
//                         cur = xml_xptr_get_nth_child(cur_node, index1 as usize - 1);
//                         index1 = 0;
//                     } else {
//                         cur = cur_node.children();
//                     }
//                     // Now gather the remaining nodes from cur to end
//                     continue; /* while */
//                 }
//             } else if cur == start && list.is_none() {
//                 // looks superfluous but ...
//                 if matches!(
//                     cur_node.element_type(),
//                     XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
//                 ) {
//                     let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
//                     let mut content: *const XmlChar = cur_node.content;

//                     let tmp = if content.is_null() {
//                         xml_new_text_len(null_mut(), 0)
//                     } else {
//                         if index1 > 1 {
//                             content = content.add(index1 as usize - 1);
//                         }
//                         xml_new_text(content)
//                     };
//                     last = tmp.map(|tmp| tmp.into());
//                     list = tmp.map(|tmp| tmp.into());
//                 } else {
//                     if cur == start && index1 > 1 {
//                         let tmp = xml_copy_node(cur_node, 0);
//                         list = tmp;
//                         parent = tmp;
//                         last = None;
//                         cur = xml_xptr_get_nth_child(cur_node, index1 as usize - 1);
//                         index1 = 0;
//                         // Now gather the remaining nodes from cur to end
//                         continue; /* while */
//                     }
//                     let tmp = xml_copy_node(cur_node, 1);
//                     list = tmp;
//                     parent = None;
//                     last = tmp;
//                 }
//             } else {
//                 let mut tmp = None;
//                 match cur_node.element_type() {
//                     XmlElementType::XmlDTDNode
//                     | XmlElementType::XmlElementDecl
//                     | XmlElementType::XmlAttributeDecl
//                     | XmlElementType::XmlEntityNode => { /* Do not copy DTD information */ }
//                     XmlElementType::XmlEntityDecl => {
//                         // TODO /* handle crossing entities -> stack needed */
//                     }
//                     XmlElementType::XmlXIncludeStart | XmlElementType::XmlXIncludeEnd => {
//                         // don't consider it part of the tree content
//                     }
//                     XmlElementType::XmlAttributeNode => {
//                         // Humm, should not happen !
//                         STRANGE!();
//                     }
//                     _ => {
//                         tmp = xml_copy_node(cur_node, 1);
//                     }
//                 }
//                 if let Some(tmp) = tmp {
//                     if list.is_none() || (last.is_none() && parent.is_none()) {
//                         STRANGE!();
//                         return None;
//                     }
//                     if let Some(last) = last {
//                         last.add_next_sibling(tmp);
//                     } else {
//                         last = parent.unwrap().add_child(tmp);
//                     }
//                 }
//             }
//             // Skip to next node in document order
//             if list.is_none() || (last.is_none() && parent.is_none()) {
//                 STRANGE!();
//                 return None;
//             }
//             cur = xml_xptr_advance_node(cur_node, null_mut());
//         }
//         list
//     }
// }

// /// Build a node list tree copy of the XPointer result.
// /// This will drop Attributes and Namespace declarations.
// ///
// /// Returns an xmlNodePtr list or NULL. The caller has to free the node tree.
// #[doc(alias = "xmlXPtrBuildNodeList")]
// #[cfg(feature = "libxml_xptr_locs")]
// pub(crate) unsafe fn xml_xptr_build_node_list(obj: XmlXPathObjectPtr) -> Option<XmlGenericNodePtr> {
//     unsafe {
//         use crate::tree::{XmlGenericNodePtr, xml_copy_node};

//         let mut list = None;
//         let mut last: Option<XmlGenericNodePtr> = None;

//         if obj.is_null() {
//             return None;
//         }
//         match (*obj).typ {
//             XmlXPathObjectType::XPathNodeset => {
//                 let set = (*obj).nodesetval.as_deref()?;
//                 for &node in &set.node_tab {
//                     match node.element_type() {
//                         XmlElementType::XmlTextNode
//                         | XmlElementType::XmlCDATASectionNode
//                         | XmlElementType::XmlElementNode
//                         | XmlElementType::XmlEntityRefNode
//                         | XmlElementType::XmlEntityNode
//                         | XmlElementType::XmlPINode
//                         | XmlElementType::XmlCommentNode
//                         | XmlElementType::XmlDocumentNode
//                         | XmlElementType::XmlHTMLDocumentNode
//                         | XmlElementType::XmlXIncludeStart
//                         | XmlElementType::XmlXIncludeEnd => {}
//                         XmlElementType::XmlAttributeNode
//                         | XmlElementType::XmlNamespaceDecl
//                         | XmlElementType::XmlDocumentTypeNode
//                         | XmlElementType::XmlDocumentFragNode
//                         | XmlElementType::XmlNotationNode
//                         | XmlElementType::XmlDTDNode
//                         | XmlElementType::XmlElementDecl
//                         | XmlElementType::XmlAttributeDecl
//                         | XmlElementType::XmlEntityDecl => continue,
//                         _ => unreachable!(),
//                     }
//                     if let Some(l) = last {
//                         l.add_next_sibling(xml_copy_node(node, 1).unwrap());
//                         if let Some(next) = l.next() {
//                             last = Some(next);
//                         }
//                     } else {
//                         list = xml_copy_node(node, 1);
//                         last = list;
//                     }
//                 }
//             }
//             XmlXPathObjectType::XPathLocationset => {
//                 let set: XmlLocationSetPtr = (*obj).user as XmlLocationSetPtr;
//                 if set.is_null() {
//                     return None;
//                 }
//                 for &loc in &(*set).loc_tab {
//                     if let Some(last) = last {
//                         last.add_next_sibling(xml_xptr_build_node_list(loc).unwrap());
//                     } else {
//                         list = xml_xptr_build_node_list(loc);
//                         last = list;
//                     }
//                     if let Some(mut l) = last {
//                         while let Some(next) = l.next() {
//                             l = next;
//                         }
//                         last = Some(l);
//                     }
//                 }
//             }
//             XmlXPathObjectType::XPathRange => return xml_xptr_build_range_node_list(obj),
//             XmlXPathObjectType::XPathPoint => {
//                 return xml_copy_node(
//                     XmlGenericNodePtr::from_raw((*obj).user as *mut XmlNode).unwrap(),
//                     0,
//                 );
//             }
//             _ => {}
//         }
//         list
//     }
// }

/// ```text
/// [8]   Predicate ::=   '[' PredicateExpr ']'
/// [9]   PredicateExpr ::=   Expr
/// ```
///
/// Evaluate a predicate as in xmlXPathEvalPredicate() but for
/// a Location Set instead of a node set
#[doc(alias = "xmlXPtrEvalRangePredicate")]
#[cfg(feature = "libxml_xptr_locs")]
pub unsafe fn xml_xptr_eval_range_predicate(ctxt: &mut XmlXPathParserContext) {
    unsafe {
        ctxt.skip_blanks();
        if ctxt.current_char() != Some('[') {
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidPredicateError as i32);
            return;
        }
        ctxt.next_char();
        ctxt.skip_blanks();

        // Extract the old set, and then evaluate the result of the
        // expression for all the element in the set. use it to grow
        // up a new set.
        if ctxt
            .value()
            .is_none_or(|value| value.typ != XmlXPathObjectType::XPathLocationset)
        {
            xml_xpath_err(
                Some(ctxt),
                crate::xpath::XmlXPathError::XPathInvalidType as i32,
            );
            return;
        };
        let obj = ctxt.value_pop().unwrap();
        let oldset = obj.user.as_ref().and_then(|user| user.as_location_set());

        ctxt.context.node = None;

        if let Some(oldset) = oldset.filter(|oldset| !oldset.loc_tab.is_empty()) {
            // Save the expression pointer since we will have to evaluate
            // it multiple times. Initialize the new set.
            let cur = ctxt.cur;
            let mut newset = XmlLocationSet::new(None);

            for (i, loc) in oldset.loc_tab.iter().enumerate() {
                ctxt.cur = cur;

                // Run the evaluation with a node list made of a single item in the nodeset.
                ctxt.context.node = loc.user.as_ref().and_then(|user| user.as_node()).copied();

                let tmp = xml_xpath_new_node_set(ctxt.context.node);
                ctxt.value_push(tmp);
                let keep_stack_len = ctxt.value_tab.len();
                ctxt.context.context_size = oldset.loc_tab.len() as i32;
                ctxt.context.proximity_position = i as i32 + 1;

                ctxt.evaluate_expression();
                if ctxt.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                    return;
                };

                // The result of the evaluation need to be tested to
                // decided whether the filter succeeded or not
                let res = ctxt.value_pop().unwrap();
                if ctxt.evaluate_predicate_result(&res) != 0 {
                    newset.push(loc.clone());
                }

                // Cleanup
                if keep_stack_len == ctxt.value_tab.len() {
                    ctxt.value_pop();
                }

                ctxt.context.node = None;
            }

            // The result is used as the new evaluation set.
            ctxt.context.node = None;
            ctxt.context.context_size = -1;
            ctxt.context.proximity_position = -1;
            ctxt.value_push(xml_xptr_wrap_location_set(newset));
        } else {
            ctxt.context.context_size = 0;
            ctxt.context.proximity_position = 0;
            ctxt.evaluate_expression();
            ctxt.value_pop();
            ctxt.value_push(obj);
            if ctxt.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                return;
            };
        }
        if ctxt.current_char() != Some(']') {
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidPredicateError as i32);
            return;
        }

        ctxt.next_char();
        ctxt.skip_blanks();
    }
}
