use std::{borrow::Cow, ffi::c_void};

use crate::{generic_error, tree::XmlGenericNodePtr, xpointer::XmlLocationSet};

use super::{
    XML_XPATH_NAN, XmlNodeSet, xml_xpath_cast_boolean_to_number, xml_xpath_cast_boolean_to_string,
    xml_xpath_cast_node_set_to_boolean, xml_xpath_cast_node_set_to_number,
    xml_xpath_cast_node_set_to_string, xml_xpath_cast_number_to_boolean,
    xml_xpath_cast_number_to_string, xml_xpath_cast_string_to_boolean,
    xml_xpath_cast_string_to_number, xml_xpath_free_node_set, xml_xpath_free_value_tree,
    xml_xpath_node_set_create, xml_xpath_node_set_merge,
};

// An expression is evaluated to yield an object, which
// has one of the following four basic types:
//   - node-set
//   - boolean
//   - number
//   - string
//
// @@ XPointer will add more types !
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum XmlXPathObjectType {
    #[default]
    XPathUndefined = 0,
    XPathNodeset = 1,
    XPathBoolean = 2,
    XPathNumber = 3,
    XPathString = 4,
    #[cfg(feature = "libxml_xptr_locs")]
    XPathPoint = 5,
    #[cfg(feature = "libxml_xptr_locs")]
    XPathRange = 6,
    #[cfg(feature = "libxml_xptr_locs")]
    XPathLocationset = 7,
    XPathUsers = 8,
    XPathXSLTTree = 9, /* An XSLT value tree, non modifiable */
}

// #[cfg(all(feature = "libxml_xptr_locs", feature = "xpath"))]
// const XPATH_POINT: usize = 5;
// #[cfg(all(feature = "libxml_xptr_locs", feature = "xpath"))]
// const XPATH_RANGE: usize = 6;
// #[cfg(all(feature = "libxml_xptr_locs", feature = "xpath"))]
// const XPATH_LOCATIONSET: usize = 7;

#[repr(C)]
#[derive(Clone, PartialEq)]
pub struct XmlXPathObject {
    pub typ: XmlXPathObjectType,
    pub nodesetval: Option<Box<XmlNodeSet>>,
    pub boolval: bool,
    pub floatval: f64,
    pub stringval: Option<String>,
    pub(crate) user: Option<XmlXPathObjectUserData>,
    pub(crate) index: i32,
    pub(crate) user2: Option<XmlXPathObjectUserData>,
    pub(crate) index2: i32,
}

impl Default for XmlXPathObject {
    fn default() -> Self {
        Self {
            typ: XmlXPathObjectType::default(),
            nodesetval: None,
            boolval: false,
            floatval: 0.0,
            stringval: None,
            user: None,
            index: 0,
            user2: None,
            index2: 0,
        }
    }
}

impl Drop for XmlXPathObject {
    /// Free up an object: xmlXPathObjectPtr.
    #[doc(alias = "xmlXPathFreeObject")]
    fn drop(&mut self) {
        if matches!(
            self.typ,
            XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
        ) {
            if self.boolval {
                self.typ = XmlXPathObjectType::XPathXSLTTree; // TODO: Just for debugging. 
                xml_xpath_free_value_tree(self.nodesetval.take());
            } else if let Some(set) = self.nodesetval.take() {
                xml_xpath_free_node_set(Some(set));
            }
        } else if matches!(self.typ, XmlXPathObjectType::XPathString) && self.stringval.is_some() {
            let _ = self.stringval.take();
        } else {
            #[cfg(feature = "libxml_xptr_locs")]
            let _ = self.user.take();
            // #[cfg(feature = "libxml_xptr_locs")]
            // if let Some(loc) = self.user.take() {
            //     if let Some(&loc) = loc.as_location_set() {
            //         xml_xptr_free_location_set(loc);
            //     }
            // }
        }
    }
}

impl From<&str> for XmlXPathObject {
    fn from(value: &str) -> Self {
        let mut ret = Self::default();
        ret.typ = XmlXPathObjectType::XPathString;
        ret.stringval = Some(value.to_owned());
        ret
    }
}

impl From<String> for XmlXPathObject {
    fn from(value: String) -> Self {
        let mut ret = Self::default();
        ret.typ = XmlXPathObjectType::XPathString;
        ret.stringval = Some(value);
        ret
    }
}

impl From<f64> for XmlXPathObject {
    fn from(value: f64) -> Self {
        let mut ret = Self::default();
        ret.typ = XmlXPathObjectType::XPathNumber;
        ret.floatval = value;
        ret
    }
}

impl From<bool> for XmlXPathObject {
    fn from(value: bool) -> Self {
        let mut ret = Self::default();
        ret.typ = XmlXPathObjectType::XPathBoolean;
        ret.boolval = value;
        ret
    }
}

#[derive(Clone, PartialEq)]
pub enum XmlXPathObjectUserData {
    Node(XmlGenericNodePtr),
    LocationSet(XmlLocationSet),
    External(*mut c_void),
}

impl XmlXPathObjectUserData {
    pub fn as_node(&self) -> Option<&XmlGenericNodePtr> {
        match self {
            Self::Node(node) => Some(node),
            _ => None,
        }
    }

    pub fn as_location_set(&self) -> Option<&XmlLocationSet> {
        match self {
            Self::LocationSet(loc) => Some(loc),
            _ => None,
        }
    }

    pub fn as_location_set_mut(&mut self) -> Option<&mut XmlLocationSet> {
        match self {
            Self::LocationSet(loc) => Some(loc),
            _ => None,
        }
    }

    pub fn as_external(&self) -> Option<&*mut c_void> {
        match self {
            Self::External(external) => Some(external),
            _ => None,
        }
    }
}

/// Create a new xmlXPathObjectPtr of type string and of value @val
///
/// Returns the newly created object.
#[doc(
    alias = "xmlXPathNewCString",
    alias = "xmlXPathNewString",
    alias = "xmlXPathCacheNewString",
    alias = "xmlXPathCacheNewCString"
)]
pub fn xml_xpath_new_string(val: Option<&str>) -> XmlXPathObject {
    XmlXPathObject::from(val.unwrap_or(""))
}

/// Wraps the @val string into an XPath object.
///
/// Returns the newly created object.
///
/// Frees @val in case of error.
#[doc(
    alias = "xmlXPathWrapString",
    alias = "xmlXPathWrapCString",
    alias = "xmlXPathCacheWrapString"
)]
pub fn xml_xpath_wrap_string(val: Option<&str>) -> XmlXPathObject {
    let mut ret = XmlXPathObject::default();
    ret.typ = XmlXPathObjectType::XPathString;
    ret.stringval = val.map(|s| s.to_owned());
    ret
}

/// Create a new xmlXPathObjectPtr of type f64 and of value @val
///
/// Returns the newly created object.
#[doc(alias = "xmlXPathNewFloat", alias = "xmlXPathCacheNewFloat")]
pub fn xml_xpath_new_float(val: f64) -> XmlXPathObject {
    XmlXPathObject::from(val)
}

/// Create a new xmlXPathObjectPtr of type boolean and of value @val
///
/// Returns the newly created object.
#[doc(alias = "xmlXPathNewBoolean", alias = "xmlXPathCacheNewBoolean")]
pub fn xml_xpath_new_boolean(val: bool) -> XmlXPathObject {
    XmlXPathObject::from(val)
}

/// Create a new xmlXPathObjectPtr of type NodeSet and initialize
/// it with the single Node @val
///
/// Returns the newly created object.
#[doc(alias = "xmlXPathNewNodeSet", alias = "xmlXPathCacheNewNodeSet")]
pub fn xml_xpath_new_node_set(val: Option<XmlGenericNodePtr>) -> XmlXPathObject {
    let mut ret = XmlXPathObject::default();
    ret.typ = XmlXPathObjectType::XPathNodeset;
    ret.boolval = false;
    // TODO: Check memory error.
    ret.nodesetval = xml_xpath_node_set_create(val);
    /* @@ with_ns to check whether namespace nodes should be looked at @@ */
    ret
}

/// Wrap the Nodeset @val in a new xmlXPathObjectPtr
///
/// Returns the newly created object.
///
/// In case of error the node set is destroyed and NULL is returned.
#[doc(alias = "xmlXPathWrapNodeSet", alias = "xmlXPathCacheWrapNodeSet")]
pub fn xml_xpath_wrap_node_set(val: Option<Box<XmlNodeSet>>) -> XmlXPathObject {
    let mut ret = XmlXPathObject::default();
    ret.typ = XmlXPathObjectType::XPathNodeset;
    ret.nodesetval = val;
    ret
}

/// Create a new xmlXPathObjectPtr of type Value Tree (XSLT) and initialize
/// it with the tree root @val
///
/// Returns the newly created object.
#[doc(alias = "xmlXPathNewValueTree")]
pub fn xml_xpath_new_value_tree(val: Option<XmlGenericNodePtr>) -> XmlXPathObject {
    let mut ret = XmlXPathObject::default();
    ret.typ = XmlXPathObjectType::XPathXSLTTree;
    ret.boolval = true;
    ret.user = val.map(XmlXPathObjectUserData::Node);
    ret.nodesetval = xml_xpath_node_set_create(val);
    ret
}

/// Wraps the @val data into an XPath object.
///
/// Returns the newly created object.
#[doc(alias = "xmlXPathWrapExternal")]
pub fn xml_xpath_wrap_external(val: *mut c_void) -> XmlXPathObject {
    let mut ret = XmlXPathObject::default();
    ret.typ = XmlXPathObjectType::XPathUsers;
    ret.user = (!val.is_null()).then_some(XmlXPathObjectUserData::External(val));
    ret
}

/// Allocate a new copy of a given object
///
/// Returns the newly created object.
#[doc(alias = "xmlXPathObjectCopy", alias = "xmlXPathCacheObjectCopy")]
pub fn xml_xpath_object_copy(val: &XmlXPathObject) -> XmlXPathObject {
    let mut ret = val.clone();
    match val.typ {
        XmlXPathObjectType::XPathBoolean | XmlXPathObjectType::XPathNumber => {}
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XPathPoint | XmlXPathObjectType::XPathRange => {}
        XmlXPathObjectType::XPathString => {}
        XmlXPathObjectType::XPathXSLTTree | XmlXPathObjectType::XPathNodeset => {
            // TODO: Check memory error.
            ret.nodesetval = xml_xpath_node_set_merge(None, val.nodesetval.as_deref());
            // Do not deallocate the copied tree value
            ret.boolval = false;
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XPathLocationset => {
            // What is this ????
            // It seems that `xml_xptr_location_set_merge` always returns `null_mut()`...
            // if let Some(loc) = (*val).user.as_ref().and_then(|user| user.as_location_set()) {
            //     let loc = xml_xptr_location_set_merge(null_mut(), loc);
            //     ret.user =
            //         (!loc.is_null()).then_some(XmlXPathObjectUserData::LocationSet(loc));
            // }
        }
        XmlXPathObjectType::XPathUsers => {
            ret.user = val.user.clone();
        }
        XmlXPathObjectType::XPathUndefined => {
            generic_error!("xmlXPathObjectCopy: unsupported type {}\n", val.typ as i32);
        } // _ => {}
    }
    ret
}

/// Converts an XPath object to its boolean value
///
/// Returns the boolean value
#[doc(alias = "xmlXPathCastToBoolean")]
pub fn xml_xpath_cast_to_boolean(val: &XmlXPathObject) -> bool {
    match val.typ {
        XmlXPathObjectType::XPathUndefined => false,
        XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree => {
            xml_xpath_cast_node_set_to_boolean(val.nodesetval.as_deref())
        }
        XmlXPathObjectType::XPathString => {
            xml_xpath_cast_string_to_boolean(val.stringval.as_deref())
        }
        XmlXPathObjectType::XPathNumber => xml_xpath_cast_number_to_boolean(val.floatval),
        XmlXPathObjectType::XPathBoolean => val.boolval,
        XmlXPathObjectType::XPathUsers => {
            // todo!();
            false
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XPathPoint
        | XmlXPathObjectType::XPathRange
        | XmlXPathObjectType::XPathLocationset => {
            // todo!();
            false
        }
    }
}

/// Converts an XPath object to its number value
///
/// Returns the number value
#[doc(alias = "xmlXPathCastToNumber")]
pub fn xml_xpath_cast_to_number(val: &mut XmlXPathObject) -> f64 {
    match val.typ {
        XmlXPathObjectType::XPathUndefined => XML_XPATH_NAN,
        XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree => {
            xml_xpath_cast_node_set_to_number(val.nodesetval.as_deref_mut())
        }
        XmlXPathObjectType::XPathString => {
            let strval = val.stringval.as_deref();
            xml_xpath_cast_string_to_number(strval)
        }
        XmlXPathObjectType::XPathNumber => val.floatval,
        XmlXPathObjectType::XPathBoolean => xml_xpath_cast_boolean_to_number(val.boolval),
        XmlXPathObjectType::XPathUsers => {
            // todo!();
            XML_XPATH_NAN
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XPathPoint
        | XmlXPathObjectType::XPathRange
        | XmlXPathObjectType::XPathLocationset => {
            // todo!();
            XML_XPATH_NAN
        }
    }
}

/// Converts an existing object to its string() equivalent
///
/// Returns the allocated string value of the object, `None` in case of error.
#[doc(alias = "xmlXPathCastToString")]
pub fn xml_xpath_cast_to_string(val: &mut XmlXPathObject) -> Cow<'static, str> {
    match val.typ {
        XmlXPathObjectType::XPathUndefined => "".into(),
        XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree => {
            xml_xpath_cast_node_set_to_string(val.nodesetval.as_deref_mut())
        }
        XmlXPathObjectType::XPathString => val.stringval.clone().unwrap().into(),
        XmlXPathObjectType::XPathBoolean => xml_xpath_cast_boolean_to_string(val.boolval).into(),
        XmlXPathObjectType::XPathNumber => xml_xpath_cast_number_to_string(val.floatval),
        XmlXPathObjectType::XPathUsers => {
            // todo!();
            "".into()
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XPathPoint
        | XmlXPathObjectType::XPathRange
        | XmlXPathObjectType::XPathLocationset => {
            // todo!();
            "".into()
        }
    }
}

/// Converts an existing object to its boolean() equivalent
///
/// Returns the new object, the old one is freed (or the operation is done directly on @val)
#[doc(
    alias = "xmlXPathConvertBoolean",
    alias = "xmlXPathCacheConvertBoolean"
)]
pub fn xml_xpath_convert_boolean(val: Option<XmlXPathObject>) -> XmlXPathObject {
    let Some(val) = val else {
        return xml_xpath_new_boolean(false);
    };
    if val.typ == XmlXPathObjectType::XPathBoolean {
        return val;
    }
    xml_xpath_new_boolean(xml_xpath_cast_to_boolean(&val))
}

/// Converts an existing object to its number() equivalent
///
/// Returns the new object, the old one is freed (or the operation is done directly on @val)
#[doc(alias = "xmlXPathConvertNumber", alias = "xmlXPathCacheConvertNumber")]
pub fn xml_xpath_convert_number(val: Option<XmlXPathObject>) -> XmlXPathObject {
    let Some(mut val) = val else {
        return xml_xpath_new_float(0.0);
    };
    if val.typ == XmlXPathObjectType::XPathNumber {
        return val;
    }
    xml_xpath_new_float(xml_xpath_cast_to_number(&mut val))
}

/// Converts an existing object to its string() equivalent
///
/// Returns the new object, the old one is freed (or the operation is done directly on @val)
#[doc(alias = "xmlXPathConvertString", alias = "xmlXPathCacheConvertString")]
pub fn xml_xpath_convert_string(val: Option<XmlXPathObject>) -> XmlXPathObject {
    let Some(mut val) = val else {
        return xml_xpath_new_string(Some(""));
    };

    let mut res = None::<Cow<'_, str>>;
    match val.typ {
        XmlXPathObjectType::XPathUndefined => {}
        XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree => {
            res = Some(xml_xpath_cast_node_set_to_string(
                val.nodesetval.as_deref_mut(),
            ));
        }
        XmlXPathObjectType::XPathString => {
            return val;
        }
        XmlXPathObjectType::XPathBoolean => {
            res = Some(xml_xpath_cast_boolean_to_string(val.boolval).into());
        }
        XmlXPathObjectType::XPathNumber => {
            res = Some(xml_xpath_cast_number_to_string(val.floatval));
        }
        XmlXPathObjectType::XPathUsers => {
            // todo!()
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XPathPoint
        | XmlXPathObjectType::XPathRange
        | XmlXPathObjectType::XPathLocationset => {
            // todo!()
        }
    }
    let Some(res) = res else {
        return xml_xpath_new_string(Some(""));
    };
    xml_xpath_wrap_string(Some(&res))
}
