use std::{ffi::CString, iter::repeat, ptr::null_mut};

use crate::{
    libxml::chvalid::xml_is_blank_char,
    parser::build_qname,
    tree::{NodeCommon, XmlAttrPtr, XmlElementType, XmlNodePtr, XmlNsPtr},
    xpath::{XmlXPathObjectPtr, XmlXPathObjectType},
};

use super::{
    XmlXPathError, XmlXPathParserContext, xml_xpath_cast_node_to_number,
    xml_xpath_cast_node_to_string, xml_xpath_convert_boolean, xml_xpath_convert_number,
    xml_xpath_convert_string, xml_xpath_err, xml_xpath_free_node_set, xml_xpath_free_object,
    xml_xpath_get_elements_by_ids, xml_xpath_new_boolean, xml_xpath_new_float,
    xml_xpath_new_node_set, xml_xpath_new_string, xml_xpath_node_set_create,
    xml_xpath_node_set_merge, xml_xpath_string_eval_number, xml_xpath_wrap_node_set,
    xml_xpath_wrap_string,
};

/// Macro to check that the number of args passed to an XPath function matches.
#[doc(alias = "CHECK_ARITY")]
pub(crate) fn check_arity(
    ctxt: &mut XmlXPathParserContext,
    nargs: usize,
    x: usize,
) -> Result<(), XmlXPathError> {
    if nargs != x {
        unsafe {
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidArity as i32);
        }
        return Err(XmlXPathError::XPathInvalidArity);
    }
    if (ctxt.value_tab.len() as i32) < ctxt.value_frame + x as i32 {
        unsafe {
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathStackError as i32);
        }
        return Err(XmlXPathError::XPathInvalidArity);
    }
    Ok(())
}

/// Macro to try to cast the value on the top of the XPath stack to a number.
#[doc(alias = "CAST_TO_NUMBER")]
pub(super) unsafe fn cast_to_number(ctxt: &mut XmlXPathParserContext) {
    unsafe {
        if ctxt
            .value()
            .is_some_and(|value| (*value).typ != XmlXPathObjectType::XPathNumber)
        {
            xml_xpath_number_function(ctxt, 1);
        }
    }
}

/// Macro to try to cast the value on the top of the XPath stack to a string.
#[doc(alias = "CAST_TO_STRING")]
pub(super) unsafe fn cast_to_string(ctxt: &mut XmlXPathParserContext) {
    unsafe {
        if ctxt
            .value()
            .is_some_and(|value| (*value).typ != XmlXPathObjectType::XPathString)
        {
            xml_xpath_string_function(ctxt, 1);
        }
    }
}

/// Macro to try to cast the value on the top of the XPath stack to a boolean.
#[doc(alias = "CAST_TO_BOOLEAN")]
pub(super) unsafe fn cast_to_boolean(ctxt: &mut XmlXPathParserContext) {
    unsafe {
        if ctxt
            .value()
            .is_some_and(|value| (*value).typ != XmlXPathObjectType::XPathBoolean)
        {
            xml_xpath_boolean_function(ctxt, 1);
        }
    }
}

/// Implement the boolean() XPath function
///    boolean boolean(object)
/// The boolean function converts its argument to a boolean as follows:
///    - a number is true if and only if it is neither positive or
///      negative zero nor NaN
///    - a node-set is true if and only if it is non-empty
///    - a string is true if and only if its length is non-zero
#[doc(alias = "xmlXPathBooleanFunction")]
pub unsafe fn xml_xpath_boolean_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    unsafe {
        if check_arity(ctxt, nargs, 1).is_err() {
            return;
        }
        let cur = ctxt.value_pop();
        if cur.is_null() {
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidOperand as i32);
            return;
        }
        let cur = xml_xpath_convert_boolean(cur);
        ctxt.value_push(cur);
    }
}

/// Implement the ceiling() XPath function
///    number ceiling(number)
/// The ceiling function returns the smallest (closest to negative infinity)
/// number that is not less than the argument and that is an integer.
#[doc(alias = "xmlXPathCeilingFunction")]
pub unsafe fn xml_xpath_ceiling_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    unsafe {
        if check_arity(ctxt, nargs, 1).is_err() {
            return;
        }
        cast_to_number(ctxt);
        if ctxt
            .value()
            .is_none_or(|value| (*value).typ != XmlXPathObjectType::XPathNumber)
        {
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidType as i32);
            return;
        };

        let val = &mut (**ctxt.value_mut().unwrap()).floatval;
        *val = val.ceil();
    }
}

/// Implement the count() XPath function
///    number count(node-set)
#[doc(alias = "xmlXPathCountFunction")]
pub unsafe fn xml_xpath_count_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    unsafe {
        if check_arity(ctxt, nargs, 1).is_err() {
            return;
        }
        if ctxt.value().is_none_or(|value| {
            !matches!(
                (*value).typ,
                XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
            )
        }) {
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidType as i32);
            return;
        }
        let cur: XmlXPathObjectPtr = ctxt.value_pop();

        if cur.is_null() {
            ctxt.value_push(xml_xpath_new_float(0.0));
        } else if let Some(nodeset) = (*cur).nodesetval.as_deref() {
            ctxt.value_push(xml_xpath_new_float(nodeset.len() as f64));
        } else {
            ctxt.value_push(xml_xpath_new_float(0.0));
        }
        xml_xpath_free_object(cur);
    }
}

/// Implement the concat() XPath function
///    string concat(string, string, string*)
/// The concat function returns the concatenation of its arguments.
#[doc(alias = "xmlXPathConcatFunction")]
pub unsafe fn xml_xpath_concat_function(ctxt: &mut XmlXPathParserContext, mut nargs: usize) {
    unsafe {
        let mut newobj: XmlXPathObjectPtr;

        if nargs < 2 && check_arity(ctxt, nargs, 2).is_err() {
            return;
        }

        cast_to_string(ctxt);
        let cur: XmlXPathObjectPtr = ctxt.value_pop();
        if cur.is_null() || !matches!((*cur).typ, XmlXPathObjectType::XPathString) {
            xml_xpath_free_object(cur);
            return;
        }
        nargs -= 1;

        while nargs > 0 {
            cast_to_string(ctxt);
            newobj = ctxt.value_pop();
            if newobj.is_null() || (*newobj).typ != XmlXPathObjectType::XPathString {
                xml_xpath_free_object(newobj);
                xml_xpath_free_object(cur);
                xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidType as i32);
                return;
            }
            let mut tmp = (*newobj).stringval.take();
            if let Some(curstr) = (*cur).stringval.take() {
                tmp.get_or_insert_with(String::new).push_str(&curstr);
                (*newobj).stringval = Some(curstr);
            }
            (*cur).stringval = tmp;
            xml_xpath_free_object(newobj);
            nargs -= 1;
        }
        ctxt.value_push(cur);
    }
}

/// Implement the contains() XPath function
///    boolean contains(string, string)
/// The contains function returns true if the first argument string
/// contains the second argument string, and otherwise returns false.
#[doc(alias = "xmlXPathContainsFunction")]
pub unsafe fn xml_xpath_contains_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    unsafe {
        if check_arity(ctxt, nargs, 2).is_err() {
            return;
        }
        cast_to_string(ctxt);
        if ctxt
            .value()
            .is_none_or(|value| (*value).typ != XmlXPathObjectType::XPathString)
        {
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidType as i32);
            return;
        };
        let needle: XmlXPathObjectPtr = ctxt.value_pop();
        cast_to_string(ctxt);
        let hay: XmlXPathObjectPtr = ctxt.value_pop();

        if hay.is_null() || (*hay).typ != XmlXPathObjectType::XPathString {
            xml_xpath_free_object(hay);
            xml_xpath_free_object(needle);
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidType as i32);
            return;
        }
        if (*needle)
            .stringval
            .as_deref()
            .filter(|&s| {
                (*hay)
                    .stringval
                    .as_deref()
                    .expect("Internal Error")
                    .contains(s)
            })
            .is_some()
        {
            ctxt.value_push(xml_xpath_new_boolean(true));
        } else {
            ctxt.value_push(xml_xpath_new_boolean(false));
        }
        xml_xpath_free_object(hay);
        xml_xpath_free_object(needle);
    }
}

/// Implement the id() XPath function
///    node-set id(object)
/// The id function selects elements by their unique ID
/// (see [5.2.1 Unique IDs]). When the argument to id is of type node-set,
/// then the result is the union of the result of applying id to the
/// string value of each of the nodes in the argument node-set. When the
/// argument to id is of any other type, the argument is converted to a
/// string as if by a call to the string function; the string is split
/// into a whitespace-separated list of tokens (whitespace is any sequence
/// of characters matching the production S); the result is a node-set
/// containing the elements in the same document as the context node that
/// have a unique ID equal to any of the tokens in the list.
#[doc(alias = "xmlXPathIdFunction")]
pub unsafe fn xml_xpath_id_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    unsafe {
        if check_arity(ctxt, nargs, 1).is_err() {
            return;
        }
        let mut obj = ctxt.value_pop();
        if obj.is_null() {
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidOperand as i32);
            return;
        }
        if matches!(
            (*obj).typ,
            XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
        ) {
            // TODO: Check memory error.
            let mut ret = xml_xpath_node_set_create(None);

            if let Some(nodeset) = (*obj).nodesetval.as_deref() {
                for &node in &nodeset.node_tab {
                    let tokens = xml_xpath_cast_node_to_string(Some(node));
                    let tokens = CString::new(tokens).unwrap();
                    let ns = xml_xpath_get_elements_by_ids(
                        (*ctxt.context).doc.unwrap(),
                        tokens.as_ptr() as *const u8,
                    );
                    // TODO: Check memory error.
                    ret = xml_xpath_node_set_merge(ret, ns.as_deref());
                    xml_xpath_free_node_set(ns);
                }
            }
            xml_xpath_free_object(obj);
            ctxt.value_push(xml_xpath_wrap_node_set(ret));
            return;
        }
        obj = xml_xpath_convert_string(obj);
        if obj.is_null() {
            return;
        }
        let strval = (*obj)
            .stringval
            .as_deref()
            .map(|s| CString::new(s).unwrap());
        let ret = xml_xpath_get_elements_by_ids(
            (*ctxt.context).doc.unwrap(),
            strval
                .as_deref()
                .map_or(null_mut(), |s| s.as_ptr() as *const u8),
        );
        ctxt.value_push(xml_xpath_wrap_node_set(ret));
        xml_xpath_free_object(obj);
    }
}

/// Implement the false() XPath function
///    boolean false()
#[doc(alias = "xmlXPathFalseFunction")]
pub unsafe fn xml_xpath_false_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    unsafe {
        if check_arity(ctxt, nargs, 0).is_err() {
            return;
        }
        ctxt.value_push(xml_xpath_new_boolean(false));
    }
}

/// Implement the floor() XPath function
///    number floor(number)
/// The floor function returns the largest (closest to positive infinity)
/// number that is not greater than the argument and that is an integer.
#[doc(alias = "xmlXPathFloorFunction")]
pub unsafe fn xml_xpath_floor_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    unsafe {
        if check_arity(ctxt, nargs, 1).is_err() {
            return;
        }
        cast_to_number(ctxt);
        if ctxt
            .value()
            .is_none_or(|value| (*value).typ != XmlXPathObjectType::XPathNumber)
        {
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidType as i32);
            return;
        };

        let val = &mut (**ctxt.value_mut().unwrap()).floatval;
        *val = val.floor();
    }
}

/// Implement the last() XPath function
///    number last()
/// The last function returns the number of nodes in the context node list.
#[doc(alias = "xmlXPathLastFunction")]
pub unsafe fn xml_xpath_last_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    unsafe {
        if check_arity(ctxt, nargs, 0).is_err() {
            return;
        }
        if (*ctxt.context).context_size >= 0 {
            ctxt.value_push(xml_xpath_new_float((*ctxt.context).context_size as f64));
        } else {
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidCtxtSize as i32);
        }
    }
}

/// Implement the lang() XPath function
///    boolean lang(string)
/// The lang function returns true or false depending on whether the
/// language of the context node as specified by xml:lang attributes
/// is the same as or is a sublanguage of the language specified by
/// the argument string. The language of the context node is determined
/// by the value of the xml:lang attribute on the context node, or, if
/// the context node has no xml:lang attribute, by the value of the
/// xml:lang attribute on the nearest ancestor of the context node that
/// has an xml:lang attribute. If there is no such attribute, then lang
/// returns false. If there is such an attribute, then lang returns
/// true if the attribute value is equal to the argument ignoring case,
/// or if there is some suffix starting with - such that the attribute
/// value is equal to the argument ignoring that suffix of the attribute
/// value and ignoring case.
#[doc(alias = "xmlXPathLangFunction")]
pub unsafe fn xml_xpath_lang_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    unsafe {
        let mut ret: i32 = 0;

        if check_arity(ctxt, nargs, 1).is_err() {
            return;
        }
        cast_to_string(ctxt);
        if ctxt
            .value()
            .is_none_or(|value| (*value).typ != XmlXPathObjectType::XPathString)
        {
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidType as i32);
            return;
        };
        let val = ctxt.value_pop();
        let lang = (*val).stringval.as_deref();
        let the_lang = (*ctxt.context).node.unwrap().get_lang();
        'not_equal: {
            if let (Some(the_lang), Some(lang)) = (the_lang, lang) {
                let the_lang = the_lang.as_bytes();
                let lang = lang.as_bytes();
                let mut i = 0;
                while i < lang.len() {
                    if !lang[i].eq_ignore_ascii_case(the_lang.get(i).unwrap_or(&0)) {
                        break 'not_equal;
                    }
                    i += 1;
                }
                if the_lang.get(i).unwrap_or(&0) == &0 || the_lang[i] == b'-' {
                    ret = 1;
                }
            }
        }
        // not_equal:

        xml_xpath_free_object(val);
        ctxt.value_push(xml_xpath_new_boolean(ret != 0));
    }
}

/// Implement the local-name() XPath function
///    string local-name(node-set?)
/// The local-name function returns a string containing the local part
/// of the name of the node in the argument node-set that is first in
/// document order. If the node-set is empty or the first node has no
/// name, an empty string is returned. If the argument is omitted it
/// defaults to the context node.
#[doc(alias = "xmlXPathLocalNameFunction")]
pub unsafe fn xml_xpath_local_name_function(ctxt: &mut XmlXPathParserContext, mut nargs: usize) {
    unsafe {
        if nargs == 0 {
            ctxt.value_push(xml_xpath_new_node_set((*ctxt.context).node));
            nargs = 1;
        }

        if check_arity(ctxt, nargs, 1).is_err() {
            return;
        }
        if ctxt.value().is_none_or(|value| {
            !matches!(
                (*value).typ,
                XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
            )
        }) {
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidType as i32);
            return;
        }
        let cur: XmlXPathObjectPtr = ctxt.value_pop();

        if let Some(nodeset) = (*cur).nodesetval.as_deref() {
            if !nodeset.node_tab.is_empty() {
                let table = &nodeset.node_tab;
                let i = 0; /* Should be first in document order !!!!! */
                match table[i].element_type() {
                    XmlElementType::XmlElementNode
                    | XmlElementType::XmlAttributeNode
                    | XmlElementType::XmlPINode => {
                        if table[i].name().is_some_and(|name| name.starts_with(' ')) {
                            ctxt.value_push(xml_xpath_new_string(Some("")));
                        } else {
                            ctxt.value_push(xml_xpath_new_string((*table[i]).name().as_deref()));
                        }
                    }
                    XmlElementType::XmlNamespaceDecl => {
                        let ns = XmlNsPtr::try_from(table[i]).unwrap();
                        let prefix = ns.prefix();
                        let value = xml_xpath_new_string(prefix.as_deref());
                        ctxt.value_push(value);
                    }
                    _ => {
                        ctxt.value_push(xml_xpath_new_string(Some("")));
                    }
                }
            } else {
                ctxt.value_push(xml_xpath_new_string(Some("")));
            }
        } else {
            ctxt.value_push(xml_xpath_new_string(Some("")));
        }
        xml_xpath_free_object(cur);
    }
}

/// Implement the not() XPath function
///    boolean not(boolean)
/// The not function returns true if its argument is false,
/// and false otherwise.
#[doc(alias = "xmlXPathNotFunction")]
pub unsafe fn xml_xpath_not_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    unsafe {
        if check_arity(ctxt, nargs, 1).is_err() {
            return;
        }
        cast_to_boolean(ctxt);
        if ctxt
            .value()
            .is_none_or(|value| (*value).typ != XmlXPathObjectType::XPathBoolean)
        {
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidType as i32);
            return;
        };
        let val = &mut (**ctxt.value_mut().unwrap()).boolval;
        *val = !*val;
    }
}

/// Implement the name() XPath function
///    string name(node-set?)
/// The name function returns a string containing a QName representing
/// the name of the node in the argument node-set that is first in document
/// order. The QName must represent the name with respect to the namespace
/// declarations in effect on the node whose name is being represented.
/// Typically, this will be the form in which the name occurred in the XML
/// source. This need not be the case if there are namespace declarations
/// in effect on the node that associate multiple prefixes with the same
/// namespace. However, an implementation may include information about
/// the original prefix in its representation of nodes; in this case, an
/// implementation can ensure that the returned string is always the same
/// as the QName used in the XML source. If the argument it omitted it
/// defaults to the context node.
/// Libxml keep the original prefix so the "real qualified name" used is returned.
#[doc(alias = "xmlXPathNameFunction")]
pub(super) unsafe fn xml_xpath_name_function(ctxt: &mut XmlXPathParserContext, mut nargs: usize) {
    unsafe {
        if nargs == 0 {
            ctxt.value_push(xml_xpath_new_node_set((*ctxt.context).node));
            nargs = 1;
        }

        if check_arity(ctxt, nargs, 1).is_err() {
            return;
        }
        if ctxt.value().is_none_or(|value| {
            !matches!(
                (*value).typ,
                XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
            )
        }) {
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidType as i32);
            return;
        }
        let cur: XmlXPathObjectPtr = ctxt.value_pop();

        if let Some(nodeset) = (*cur).nodesetval.as_deref() {
            if !nodeset.node_tab.is_empty() {
                let table = &nodeset.node_tab;
                let i = 0; /* Should be first in document order !!!!! */

                match table[i].element_type() {
                    XmlElementType::XmlElementNode => {
                        let node = XmlNodePtr::try_from(table[i]).unwrap();
                        if node.name.starts_with(' ') {
                            ctxt.value_push(xml_xpath_new_string(Some("")));
                        } else if let Some(prefix) =
                            node.ns.as_deref().and_then(|ns| ns.prefix.as_deref())
                        {
                            let nodename = node.name().unwrap();
                            let fullname = build_qname(&nodename, Some(prefix));
                            ctxt.value_push(xml_xpath_wrap_string(Some(&fullname)));
                        } else {
                            ctxt.value_push(xml_xpath_new_string(node.name().as_deref()));
                        }
                    }
                    XmlElementType::XmlAttributeNode => {
                        let attr = XmlAttrPtr::try_from(table[i]).unwrap();
                        if attr.name.starts_with(' ') {
                            ctxt.value_push(xml_xpath_new_string(Some("")));
                        } else if let Some(prefix) =
                            attr.ns.as_deref().and_then(|ns| ns.prefix.as_deref())
                        {
                            let attrname = attr.name().unwrap();
                            let fullname = build_qname(&attrname, Some(prefix));
                            ctxt.value_push(xml_xpath_wrap_string(Some(&fullname)));
                        } else {
                            ctxt.value_push(xml_xpath_new_string(attr.name().as_deref()));
                        }
                    }
                    _ => {
                        ctxt.value_push(xml_xpath_new_node_set(Some(table[i])));
                        xml_xpath_local_name_function(ctxt, 1);
                    }
                }
            } else {
                ctxt.value_push(xml_xpath_new_string(Some("")));
            }
        } else {
            ctxt.value_push(xml_xpath_new_string(Some("")));
        }
        xml_xpath_free_object(cur);
    }
}

/// Implement the namespace-uri() XPath function
///    string namespace-uri(node-set?)
/// The namespace-uri function returns a string containing the
/// namespace URI of the expanded name of the node in the argument
/// node-set that is first in document order. If the node-set is empty,
/// the first node has no name, or the expanded name has no namespace
/// URI, an empty string is returned. If the argument is omitted it
/// defaults to the context node.
#[doc(alias = "xmlXPathNamespaceURIFunction")]
pub unsafe fn xml_xpath_namespace_uri_function(ctxt: &mut XmlXPathParserContext, mut nargs: usize) {
    unsafe {
        if nargs == 0 {
            ctxt.value_push(xml_xpath_new_node_set((*ctxt.context).node));
            nargs = 1;
        }
        if check_arity(ctxt, nargs, 1).is_err() {
            return;
        }
        if ctxt.value().is_none_or(|value| {
            !matches!(
                (*value).typ,
                XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
            )
        }) {
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidType as i32);
            return;
        }
        let cur: XmlXPathObjectPtr = ctxt.value_pop();

        if let Some(nodeset) = (*cur).nodesetval.as_deref() {
            if !nodeset.node_tab.is_empty() {
                let table = &nodeset.node_tab;
                let i = 0; /* Should be first in document order !!!!! */
                match table[i].element_type() {
                    XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode => {
                        if let Ok(Some(ns)) = XmlNodePtr::try_from(table[i])
                            .map(|node| node.ns)
                            .or_else(|_| XmlAttrPtr::try_from(table[i]).map(|attr| attr.ns))
                        {
                            ctxt.value_push(xml_xpath_new_string(ns.href.as_deref()));
                        } else {
                            ctxt.value_push(xml_xpath_new_string(Some("")));
                        }
                    }
                    _ => {
                        ctxt.value_push(xml_xpath_new_string(Some("")));
                    }
                }
            } else {
                ctxt.value_push(xml_xpath_new_string(Some("")));
            }
        } else {
            ctxt.value_push(xml_xpath_new_string(Some("")));
        }
        xml_xpath_free_object(cur);
    }
}

/// Implement the normalize-space() XPath function
///    string normalize-space(string?)
/// The normalize-space function returns the argument string with white
/// space normalized by stripping leading and trailing whitespace
/// and replacing sequences of whitespace characters by a single
/// space. Whitespace characters are the same allowed by the S production
/// in XML. If the argument is omitted, it defaults to the context
/// node converted to a string, in other words the value of the context node.
#[doc(alias = "xmlXPathNormalizeFunction")]
pub unsafe fn xml_xpath_normalize_function(ctxt: &mut XmlXPathParserContext, mut nargs: usize) {
    unsafe {
        if nargs == 0 {
            // Use current context node
            let val = xml_xpath_cast_node_to_string((*ctxt.context).node);
            ctxt.value_push(xml_xpath_wrap_string(Some(&val)));
            nargs = 1;
        }

        if check_arity(ctxt, nargs, 1).is_err() {
            return;
        }
        cast_to_string(ctxt);
        if ctxt
            .value()
            .is_none_or(|value| (*value).typ != XmlXPathObjectType::XPathString)
        {
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidType as i32);
            return;
        };
        let Some(source) = (*ctxt.value().unwrap()).stringval.as_deref_mut() else {
            return;
        };
        let oldlen = source.len();
        // Skip leading whitespaces
        let Some(start) = source.find(|c| !xml_is_blank_char(c as u32)) else {
            (**ctxt.value_mut().unwrap())
                .stringval
                .as_mut()
                .unwrap()
                .clear();
            return;
        };
        let target = source.as_bytes_mut();

        // Collapse intermediate whitespaces, and skip trailing whitespaces
        let mut written = 0;
        let mut blank = false;
        for i in start..oldlen {
            let c = target[i];
            if xml_is_blank_char(c as u32) {
                blank = true;
            } else {
                if blank {
                    target[written] = 0x20;
                    written += 1;
                    blank = false;
                }
                target[written] = c;
                written += 1;
            }
        }
        (**ctxt.value_mut().unwrap())
            .stringval
            .as_mut()
            .unwrap()
            .truncate(written);
    }
}

/// Implement the number() XPath function
///    number number(object?)
#[doc(alias = "xmlXPathNumberFunction")]
pub unsafe fn xml_xpath_number_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    unsafe {
        let res: f64;

        if nargs == 0 {
            if let Some(context_node) = (*ctxt.context).node {
                let content = context_node.get_content();
                res = xml_xpath_string_eval_number(content.as_deref());
                ctxt.value_push(xml_xpath_new_float(res));
            } else {
                ctxt.value_push(xml_xpath_new_float(0.0));
            }
            return;
        }

        if check_arity(ctxt, nargs, 1).is_err() {
            return;
        }
        let cur: XmlXPathObjectPtr = ctxt.value_pop();
        ctxt.value_push(xml_xpath_convert_number(cur));
    }
}

/// Implement the position() XPath function
///    number position()
/// The position function returns the position of the context node in the
/// context node list. The first position is 1, and so the last position
/// will be equal to last().
#[doc(alias = "xmlXPathPositionFunction")]
pub unsafe fn xml_xpath_position_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    unsafe {
        if check_arity(ctxt, nargs, 0).is_err() {
            return;
        }
        if (*ctxt.context).proximity_position >= 0 {
            ctxt.value_push(xml_xpath_new_float(
                (*ctxt.context).proximity_position as f64,
            ));
        } else {
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidCtxtPosition as i32);
        }
    }
}

/// Implement the round() XPath function
///    number round(number)
/// The round function returns the number that is closest to the
/// argument and that is an integer. If there are two such numbers,
/// then the one that is closest to positive infinity is returned.
#[doc(alias = "xmlXPathRoundFunction")]
pub unsafe fn xml_xpath_round_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    unsafe {
        if check_arity(ctxt, nargs, 1).is_err() {
            return;
        }
        cast_to_number(ctxt);
        if ctxt
            .value()
            .is_none_or(|value| (*value).typ != XmlXPathObjectType::XPathNumber)
        {
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidType as i32);
            return;
        };

        let f: f64 = (*ctxt.value().unwrap()).floatval;

        if (-0.5..0.5).contains(&f) {
            /* Handles negative zero. */
            (**ctxt.value_mut().unwrap()).floatval *= 0.0;
        } else {
            let mut rounded: f64 = f.floor();
            if f - rounded >= 0.5 {
                rounded += 1.0;
            }
            (**ctxt.value_mut().unwrap()).floatval = rounded;
        }
    }
}

/// Implement the string() XPath function
///    string string(object?)
/// The string function converts an object to a string as follows:
///    - A node-set is converted to a string by returning the value of
///      the node in the node-set that is first in document order.
///      If the node-set is empty, an empty string is returned.
///    - A number is converted to a string as follows
///      + NaN is converted to the string NaN
///      + positive zero is converted to the string 0
///      + negative zero is converted to the string 0
///      + positive infinity is converted to the string Infinity
///      + negative infinity is converted to the string -Infinity
///      + if the number is an integer, the number is represented in
///        decimal form as a Number with no decimal point and no leading
///        zeros, preceded by a minus sign (-) if the number is negative
///      + otherwise, the number is represented in decimal form as a
///        Number including a decimal point with at least one digit
///        before the decimal point and at least one digit after the
///        decimal point, preceded by a minus sign (-) if the number
///        is negative; there must be no leading zeros before the decimal
///        point apart possibly from the one required digit immediately
///        before the decimal point; beyond the one required digit
///        after the decimal point there must be as many, but only as
///        many, more digits as are needed to uniquely distinguish the
///        number from all other IEEE 754 numeric values.
///    - The boolean false value is converted to the string false.
///      The boolean true value is converted to the string true.
///
/// If the argument is omitted, it defaults to a node-set with the
/// context node as its only member.
#[doc(alias = "xmlXPathStringFunction")]
pub unsafe fn xml_xpath_string_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    unsafe {
        if nargs == 0 {
            let val = xml_xpath_cast_node_to_string((*ctxt.context).node);
            ctxt.value_push(xml_xpath_wrap_string(Some(&val)));
            return;
        }

        if check_arity(ctxt, nargs, 1).is_err() {
            return;
        }
        let cur: XmlXPathObjectPtr = ctxt.value_pop();
        if cur.is_null() {
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidOperand as i32);
            return;
        }
        ctxt.value_push(xml_xpath_convert_string(cur));
    }
}

/// Implement the string-length() XPath function
///    number string-length(string?)
/// The string-length returns the number of characters in the string
/// (see [3.6 Strings]). If the argument is omitted, it defaults to
/// the context node converted to a string, in other words the value
/// of the context node.
#[doc(alias = "xmlXPathStringLengthFunction")]
pub unsafe fn xml_xpath_string_length_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    unsafe {
        if nargs == 0 {
            if ctxt.context.is_null() {
                return;
            }
            if let Some(context_node) = (*ctxt.context).node {
                let content = xml_xpath_cast_node_to_string(Some(context_node));
                ctxt.value_push(xml_xpath_new_float(content.chars().count() as f64));
            } else {
                ctxt.value_push(xml_xpath_new_float(0.0));
            }
            return;
        }
        if check_arity(ctxt, nargs, 1).is_err() {
            return;
        }
        cast_to_string(ctxt);
        if ctxt
            .value()
            .is_none_or(|value| (*value).typ != XmlXPathObjectType::XPathString)
        {
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidType as i32);
            return;
        };
        let cur: XmlXPathObjectPtr = ctxt.value_pop();
        ctxt.value_push(xml_xpath_new_float(
            (*cur).stringval.as_deref().map_or(0, |s| s.chars().count()) as _,
        ));
        xml_xpath_free_object(cur);
    }
}

/// Implement the starts-with() XPath function
///    boolean starts-with(string, string)
/// The starts-with function returns true if the first argument string
/// starts with the second argument string, and otherwise returns false.
#[doc(alias = "xmlXPathStartsWithFunction")]
pub unsafe fn xml_xpath_starts_with_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    unsafe {
        if check_arity(ctxt, nargs, 2).is_err() {
            return;
        }
        cast_to_string(ctxt);
        if ctxt
            .value()
            .is_none_or(|value| (*value).typ != XmlXPathObjectType::XPathString)
        {
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidType as i32);
            return;
        };
        let needle: XmlXPathObjectPtr = ctxt.value_pop();
        cast_to_string(ctxt);
        let hay: XmlXPathObjectPtr = ctxt.value_pop();

        if hay.is_null() || (*hay).typ != XmlXPathObjectType::XPathString {
            xml_xpath_free_object(hay);
            xml_xpath_free_object(needle);
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidType as i32);
            return;
        }
        let m = (*hay).stringval.as_deref().map_or(0, |s| s.len());
        let n = (*needle).stringval.as_deref().map_or(0, |s| s.len());
        if &(*hay).stringval.as_deref().unwrap()[..n.min(m)]
            != (*needle).stringval.as_deref().expect("Internal Error")
        {
            ctxt.value_push(xml_xpath_new_boolean(false));
        } else {
            ctxt.value_push(xml_xpath_new_boolean(true));
        }
        xml_xpath_free_object(hay);
        xml_xpath_free_object(needle);
    }
}

/// Implement the substring() XPath function
///    string substring(string, number, number?)
/// The substring function returns the substring of the first argument
/// starting at the position specified in the second argument with
/// length specified in the third argument. For example,
/// substring("12345",2,3) returns "234". If the third argument is not
/// specified, it returns the substring starting at the position specified
/// in the second argument and continuing to the end of the string. For
/// example, substring("12345",2) returns "2345".  More precisely, each
/// character in the string (see [3.6 Strings]) is considered to have a
/// numeric position: the position of the first character is 1, the position
/// of the second character is 2 and so on. The returned substring contains
/// those characters for which the position of the character is greater than
/// or equal to the second argument and, if the third argument is specified,
/// less than the sum of the second and third arguments; the comparisons
/// and addition used for the above follow the standard IEEE 754 rules. Thus:
///  - substring("12345", 1.5, 2.6) returns "234"
///  - substring("12345", 0, 3) returns "12"
///  - substring("12345", 0 div 0, 3) returns ""
///  - substring("12345", 1, 0 div 0) returns ""
///  - substring("12345", -42, 1 div 0) returns "12345"
///  - substring("12345", -1 div 0, 1 div 0) returns ""
#[doc(alias = "xmlXPathSubstringFunction")]
pub unsafe fn xml_xpath_substring_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    unsafe {
        let len: XmlXPathObjectPtr;
        let mut le: f64 = 0.0;
        let mut i: i32 = 1;
        let mut j: i32 = i32::MAX;

        if nargs < 2 && check_arity(ctxt, nargs, 2).is_err() {
            return;
        }
        if nargs > 3 && check_arity(ctxt, nargs, 3).is_err() {
            return;
        }
        // take care of possible last (position) argument
        if nargs == 3 {
            cast_to_number(ctxt);
            if ctxt
                .value()
                .is_none_or(|value| (*value).typ != XmlXPathObjectType::XPathNumber)
            {
                xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidType as i32);
                return;
            };
            len = ctxt.value_pop();
            le = (*len).floatval;
            xml_xpath_free_object(len);
        }

        cast_to_number(ctxt);
        if ctxt
            .value()
            .is_none_or(|value| (*value).typ != XmlXPathObjectType::XPathNumber)
        {
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidType as i32);
            return;
        };
        let start: XmlXPathObjectPtr = ctxt.value_pop();
        let input: f64 = (*start).floatval;
        xml_xpath_free_object(start);
        cast_to_string(ctxt);
        if ctxt
            .value()
            .is_none_or(|value| (*value).typ != XmlXPathObjectType::XPathString)
        {
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidType as i32);
            return;
        };
        let str: XmlXPathObjectPtr = ctxt.value_pop();

        if !matches!(
            input.partial_cmp(&(i32::MAX as f64)),
            Some(std::cmp::Ordering::Less)
        ) {
            // Logical NOT to handle NaNs
            i = i32::MAX;
        } else if input >= 1.0 {
            i = input as _;
            if input - input.floor() >= 0.5 {
                i += 1;
            }
        }

        if nargs == 3 {
            let mut rin: f64;
            let mut rle: f64;

            rin = input.floor();
            if input - rin >= 0.5 {
                rin += 1.0;
            }

            rle = le.floor();
            if le - rle >= 0.5 {
                rle += 1.0;
            }

            let end: f64 = rin + rle;
            if !matches!(
                end.partial_cmp(&1.0),
                Some(std::cmp::Ordering::Equal) | Some(std::cmp::Ordering::Greater)
            ) {
                // Logical NOT to handle NaNs
                j = 1;
            } else if end < i32::MAX as f64 {
                j = end as _;
            }
        }

        if i < j {
            let ret = (*str)
                .stringval
                .as_deref()
                .expect("Internal Error")
                .chars()
                .skip(i as usize - 1)
                .take((j - i) as usize)
                .collect::<String>();
            ctxt.value_push(xml_xpath_new_string(Some(&ret)));
        } else {
            ctxt.value_push(xml_xpath_new_string(Some("")));
        }

        xml_xpath_free_object(str);
    }
}

/// Implement the substring-before() XPath function
///    string substring-before(string, string)
/// The substring-before function returns the substring of the first
/// argument string that precedes the first occurrence of the second
/// argument string in the first argument string, or the empty string
/// if the first argument string does not contain the second argument
/// string. For example, substring-before("1999/04/01","/") returns 1999.
#[doc(alias = "xmlXPathSubstringBeforeFunction")]
pub unsafe fn xml_xpath_substring_before_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    unsafe {
        if check_arity(ctxt, nargs, 2).is_err() {
            return;
        }
        cast_to_string(ctxt);
        let find: XmlXPathObjectPtr = ctxt.value_pop();
        cast_to_string(ctxt);
        let str: XmlXPathObjectPtr = ctxt.value_pop();

        let ss = (*str).stringval.as_deref().unwrap();
        let fs = (*find).stringval.as_deref().unwrap();
        let target = ss.find(fs).map(|pos| ss[..pos].to_owned());
        ctxt.value_push(xml_xpath_new_string(target.as_deref()));
        xml_xpath_free_object(str);
        xml_xpath_free_object(find);
    }
}

/// Implement the substring-after() XPath function
///    string substring-after(string, string)
/// The substring-after function returns the substring of the first
/// argument string that follows the first occurrence of the second
/// argument string in the first argument string, or the empty stringi
/// if the first argument string does not contain the second argument
/// string. For example, substring-after("1999/04/01","/") returns 04/01,
/// and substring-after("1999/04/01","19") returns 99/04/01.
#[doc(alias = "xmlXPathSubstringAfterFunction")]
pub unsafe fn xml_xpath_substring_after_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    unsafe {
        if check_arity(ctxt, nargs, 2).is_err() {
            return;
        }
        cast_to_string(ctxt);
        let find: XmlXPathObjectPtr = ctxt.value_pop();
        cast_to_string(ctxt);
        let str: XmlXPathObjectPtr = ctxt.value_pop();

        let ss = (*str).stringval.as_deref().unwrap();
        let fs = (*find).stringval.as_deref().unwrap();
        let target = ss.find(fs).map(|pos| ss[pos..].to_owned());
        ctxt.value_push(xml_xpath_new_string(target.as_deref()));
        xml_xpath_free_object(str);
        xml_xpath_free_object(find);
    }
}

/// Implement the sum() XPath function
///    number sum(node-set)
/// The sum function returns the sum of the values of the nodes in
/// the argument node-set.
#[doc(alias = "xmlXPathSumFunction")]
pub unsafe fn xml_xpath_sum_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    unsafe {
        let mut res: f64 = 0.0;

        if check_arity(ctxt, nargs, 1).is_err() {
            return;
        }
        if ctxt.value().is_none_or(|value| {
            !matches!(
                (*value).typ,
                XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
            )
        }) {
            xml_xpath_err(Some(ctxt), XmlXPathError::XPathInvalidType as i32);
            return;
        }
        let cur: XmlXPathObjectPtr = ctxt.value_pop();

        if let Some(nodeset) = (*cur).nodesetval.as_deref().filter(|n| !n.is_empty()) {
            if !nodeset.node_tab.is_empty() {
                for &node in &nodeset.node_tab {
                    res += xml_xpath_cast_node_to_number(Some(node));
                }
            }
        }
        ctxt.value_push(xml_xpath_new_float(res));
        xml_xpath_free_object(cur);
    }
}

/// Implement the true() XPath function
///    boolean true()
#[doc(alias = "xmlXPathTrueFunction")]
pub unsafe fn xml_xpath_true_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    unsafe {
        if check_arity(ctxt, nargs, 0).is_err() {
            return;
        }
        ctxt.value_push(xml_xpath_new_boolean(true));
    }
}

/// Implement the translate() XPath function
///    string translate(string, string, string)
/// The translate function returns the first argument string with
/// occurrences of characters in the second argument string replaced
/// by the character at the corresponding position in the third argument
/// string. For example, translate("bar","abc","ABC") returns the string
/// BAr. If there is a character in the second argument string with no
/// character at a corresponding position in the third argument string
/// (because the second argument string is longer than the third argument
/// string), then occurrences of that character in the first argument
/// string are removed. For example, translate("--aaa--","abc-","ABC")
/// returns "AAA". If a character occurs more than once in second
/// argument string, then the first occurrence determines the replacement
/// character. If the third argument string is longer than the second
/// argument string, then excess characters are ignored.
#[doc(alias = "xmlXPathTranslateFunction")]
pub unsafe fn xml_xpath_translate_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    unsafe {
        if check_arity(ctxt, nargs, 3).is_err() {
            return;
        }

        cast_to_string(ctxt);
        let to: XmlXPathObjectPtr = ctxt.value_pop();
        cast_to_string(ctxt);
        let from: XmlXPathObjectPtr = ctxt.value_pop();
        cast_to_string(ctxt);
        let str: XmlXPathObjectPtr = ctxt.value_pop();

        let to_str = (*to).stringval.as_deref().unwrap();
        let from_str = (*from).stringval.as_deref().unwrap();
        let arg = (*str).stringval.as_deref().unwrap();
        let mut target = String::with_capacity(arg.len());
        for c in arg.chars() {
            if let Some((_, replace)) = from_str
                .chars()
                .zip(to_str.chars().map(Ok).chain(repeat(Err(()))))
                .find(|e| e.0 == c)
            {
                if let Ok(c) = replace {
                    target.push(c);
                }
            } else {
                target.push(c);
            }
        }
        ctxt.value_push(xml_xpath_new_string(Some(&target)));
        xml_xpath_free_object(str);
        xml_xpath_free_object(from);
        xml_xpath_free_object(to);
    }
}

/// Implement the escape-uri() XPath function
///    string escape-uri(string $str, bool $escape-reserved)
///
/// This function applies the URI escaping rules defined in section 2 of [RFC
/// 2396] to the string supplied as $uri-part, which typically represents all
/// or part of a URI. The effect of the function is to replace any special
/// character in the string by an escape sequence of the form %xx%yy...,
/// where xxyy... is the hexadecimal representation of the octets used to
/// represent the character in UTF-8.
///
/// The set of characters that are escaped depends on the setting of the
/// boolean argument $escape-reserved.
///
/// If $escape-reserved is true, all characters are escaped other than lower
/// case letters a-z, upper case letters A-Z, digits 0-9, and the characters
/// referred to in [RFC 2396] as "marks": specifically, "-" | "_" | "." | "!"
/// | "~" | "*" | "'" | "(" | ")". The "%" character itself is escaped only
/// if it is not followed by two hexadecimal digits (that is, 0-9, a-f, and A-F).
///
/// If $escape-reserved is false, the behavior differs in that characters
/// referred to in [RFC 2396] as reserved characters are not escaped. These
/// characters are ";" | "/" | "?" | ":" | "@" | "&" | "=" | "+" | "$" | ",".
///
/// [RFC 2396] does not define whether escaped URIs should use lower case or
/// upper case for hexadecimal digits. To ensure that escaped URIs can be
/// compared using string comparison functions, this function must always use
/// the upper-case letters A-F.
///
/// Generally, $escape-reserved should be set to true when escaping a string
/// that is to form a single part of a URI, and to false when escaping an
/// entire URI or URI reference.
///
/// In the case of non-ascii characters, the string is encoded according to
/// utf-8 and then converted according to RFC 2396.
///
/// Examples
///  xf:escape-uri ("gopher://spinaltap.micro.umn.edu/00/Weather/California/Los%20Angeles#ocean"), true())
///  returns "gopher%3A%2F%2Fspinaltap.micro.umn.edu%2F00%2FWeather%2FCalifornia%2FLos%20Angeles%23ocean"
///  xf:escape-uri ("gopher://spinaltap.micro.umn.edu/00/Weather/California/Los%20Angeles#ocean"), false())
///  returns "gopher://spinaltap.micro.umn.edu/00/Weather/California/Los%20Angeles%23ocean"
#[doc(alias = "xmlXPathEscapeUriFunction")]
pub(super) unsafe fn xml_xpath_escape_uri_function(ctxt: &mut XmlXPathParserContext, nargs: usize) {
    unsafe {
        let mut escape: [u8; 4] = [0; 4];

        if check_arity(ctxt, nargs, 2).is_err() {
            return;
        }

        let escape_reserved = ctxt.pop_boolean();

        cast_to_string(ctxt);
        let str: XmlXPathObjectPtr = ctxt.value_pop();

        escape[0] = b'%';
        escape[3] = 0;

        let mut target = String::new();
        let cptr = (*str)
            .stringval
            .as_deref()
            .expect("Internal Error")
            .as_bytes();
        for (i, &c) in cptr.iter().enumerate() {
            if c.is_ascii_alphanumeric()
                || c == b'-'
                || c == b'_'
                || c == b'.'
                || c == b'!'
                || c == b'~'
                || c == b'*'
                || c == b'\''
                || c == b'('
                || c == b')'
                || (c == b'%'
                    && (i + 1 < cptr.len() && cptr[i + 1].is_ascii_hexdigit())
                    && (i + 2 < cptr.len() && cptr[i + 2].is_ascii_hexdigit()))
                || (!escape_reserved
                    && (c == b';'
                        || c == b'/'
                        || c == b'?'
                        || c == b':'
                        || c == b'@'
                        || c == b'&'
                        || c == b'='
                        || c == b'+'
                        || c == b'$'
                        || c == b','))
            {
                target.push(c as char);
            } else {
                target.push('%');
                let hi = if c >> 4 < 10 {
                    b'0' + (c >> 4)
                } else {
                    b'A' - 10 + (c >> 4)
                };
                target.push(hi as char);
                let lo = if c & 0xF < 10 {
                    b'0' + (c & 0xF)
                } else {
                    b'A' - 10 + (c & 0xF)
                };
                target.push(lo as char);
            }
        }
        ctxt.value_push(xml_xpath_new_string(Some(&target)));
        xml_xpath_free_object(str);
    }
}
