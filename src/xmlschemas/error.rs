use std::{
    borrow::Cow,
    ffi::{CStr, CString},
    ptr::{null, null_mut},
};

use crate::{
    error::{
        __xml_raise_error, __xml_simple_error, __xml_simple_oom_error, XmlErrorDomain,
        XmlErrorLevel, XmlParserErrors,
    },
    globals::{GenericError, StructuredError},
    libxml::{
        globals::xml_free,
        schemas_internals::{
            XML_SCHEMAS_ANY_LAX, XML_SCHEMAS_ANY_SKIP, XML_SCHEMAS_ANY_STRICT,
            XML_SCHEMAS_TYPE_GLOBAL, XmlSchemaFacet, XmlSchemaFacetPtr, XmlSchemaTypeType,
            XmlSchemaValType, XmlSchemaWildcardPtr,
        },
        xmlschemas::{
            XML_SCHEMA_CTXT_PARSER, XML_SCHEMA_CTXT_VALIDATOR, XmlSchemaAbstractCtxtPtr,
            XmlSchemaAttrInfo, XmlSchemaNodeInfoPtr, XmlSchemaPSVIIDCNodePtr,
            xml_schema_facet_type_to_string, xml_schema_format_qname,
            xml_schema_get_canon_value_whtsp_ext, xml_schema_get_component_designation,
            xml_schema_get_component_node, xml_schema_get_component_qname,
            xml_schema_get_component_type_str, xml_schema_item_type_to_str,
        },
        xmlschemastypes::{XmlSchemaWhitespaceValueType, xml_schema_get_facet_value_as_ulong},
        xmlstring::{
            xml_char_strndup, xml_escape_format_string, xml_strcat, xml_strdup, xml_strncat,
            xml_strndup,
        },
    },
    tree::{NodeCommon, XmlAttrPtr, XmlElementType, XmlGenericNodePtr, XmlNodePtr, XmlNsPtr},
    xmlschemas::context::XmlSchemaParserCtxtPtr,
};

use super::{
    context::XmlSchemaValidCtxtPtr,
    items::{
        XmlSchemaAttributePtr, XmlSchemaAttributeUsePtr, XmlSchemaBasicItemPtr,
        XmlSchemaElementPtr, XmlSchemaIDCPtr, XmlSchemaType, XmlSchemaTypePtr,
    },
};

macro_rules! FREE_AND_NULL {
    ($str:expr) => {
        if !$str.is_null() {
            $crate::libxml::globals::xml_free($str as _);
            #[allow(unused_assignments)]
            {
                $str = null_mut();
            }
        }
    };
}

/// Returns a string representation of the type of processContents.
#[doc(alias = "xmlSchemaWildcardPCToString")]
fn xml_schema_wildcard_pcto_string(pc: i32) -> &'static str {
    if XML_SCHEMAS_ANY_SKIP == pc {
        "skip"
    } else if XML_SCHEMAS_ANY_LAX == pc {
        "lax"
    } else if XML_SCHEMAS_ANY_STRICT == pc {
        "strict"
    } else {
        "invalid process contents"
    }
}

unsafe fn xml_schema_eval_error_node_type(
    actxt: XmlSchemaAbstractCtxtPtr,
    node: Option<XmlGenericNodePtr>,
) -> i32 {
    unsafe {
        if let Some(node) = node {
            return node.element_type() as i32;
        }
        if (*actxt).typ == XML_SCHEMA_CTXT_VALIDATOR
            && !(*(actxt as XmlSchemaValidCtxtPtr)).inode.is_null()
        {
            return (*(*(actxt as XmlSchemaValidCtxtPtr)).inode).node_type;
        }
        -1
    }
}

fn xml_schema_format_qname_ns(ns: Option<XmlNsPtr>, local_name: Option<&str>) -> String {
    if let Some(ns) = ns {
        xml_schema_format_qname(ns.href().as_deref(), local_name)
    } else {
        xml_schema_format_qname(None, local_name)
    }
}

fn xml_schema_format_error_node_qname(
    ni: Option<&XmlSchemaAttrInfo>,
    node: Option<XmlGenericNodePtr>,
) -> Option<String> {
    if let Some(node) = node {
        let (name, ns) = if let Ok(node) = XmlNodePtr::try_from(node) {
            (node.name().unwrap().into_owned(), node.ns)
        } else {
            let attr = XmlAttrPtr::try_from(node).unwrap();
            (attr.name.to_string(), attr.ns)
        };
        if let Some(ns) = ns {
            return Some(xml_schema_format_qname(ns.href().as_deref(), Some(&name)));
        } else {
            return Some(xml_schema_format_qname(None, Some(&name)));
        }
    } else if let Some(ni) = ni {
        return Some(xml_schema_format_qname(
            ni.ns_name.as_deref(),
            ni.local_name.as_deref(),
        ));
    }
    None
}

/// Builds a string consisting of all enumeration elements.
///
/// Returns a string of all enumeration elements.
#[doc(alias = "xmlSchemaFormatFacetEnumSet")]
unsafe fn xml_schema_format_facet_enum_set(
    actxt: XmlSchemaAbstractCtxtPtr,
    buf: *mut *mut u8,
    mut typ: &XmlSchemaType,
) -> *const u8 {
    unsafe {
        let mut facet: XmlSchemaFacetPtr;
        let mut ws: XmlSchemaWhitespaceValueType;
        let mut value: *mut u8 = null_mut();
        let mut res: i32;
        let mut found: i32 = 0;

        if !(*buf).is_null() {
            xml_free(*buf as _);
        }
        *buf = null_mut();

        loop {
            // Use the whitespace type of the base type.
            ws = (*typ.base_type).white_space_facet_value().unwrap();
            facet = typ.facets;
            while !facet.is_null() {
                if (*facet).typ != XmlSchemaTypeType::XmlSchemaFacetEnumeration {
                    facet = (*facet).next;
                    continue;
                }
                found = 1;
                res = xml_schema_get_canon_value_whtsp_ext((*facet).val, ws, &raw mut value);
                if res == -1 {
                    xml_schema_internal_err(
                        actxt,
                        "xmlSchemaFormatFacetEnumSet",
                        "compute the canonical lexical representation",
                    );
                    if !(*buf).is_null() {
                        xml_free(*buf as _);
                    }
                    *buf = null_mut();
                    return null_mut();
                }
                if (*buf).is_null() {
                    *buf = xml_strdup(c"'".as_ptr() as _);
                } else {
                    *buf = xml_strcat(*buf, c", '".as_ptr() as _);
                }
                *buf = xml_strcat(*buf, value);
                *buf = xml_strcat(*buf, c"'".as_ptr() as _);
                if !value.is_null() {
                    xml_free(value as _);
                    value = null_mut();
                }
                facet = (*facet).next;
            }
            // The enumeration facet of a type restricts the enumeration
            // facet of the ancestor type; i.e., such restricted enumerations
            // do not belong to the set of the given type. Thus we break
            // on the first found enumeration.
            if found != 0 {
                break;
            }
            if typ.base_type.is_null() {
                break;
            }
            typ = &*typ.base_type;

            if typ.typ == XmlSchemaTypeType::XmlSchemaTypeBasic {
                break;
            }
        }

        *buf
    }
}

/// Returns a representation of the given item used
/// for error reports.
///
/// The following order is used to build the resulting
/// designation if the arguments are not NULL:
/// 1a. If itemDes not NULL -> itemDes
/// 1b. If (itemDes not NULL) and (itemName not NULL)
///     -> itemDes + itemName
/// 2. If the preceding was NULL and (item not NULL) -> item
/// 3. If the preceding was NULL and (itemNode not NULL) -> itemNode
///
/// If the itemNode is an attribute node, the name of the attribute
/// will be appended to the result.
///
/// Returns the formatted string.
///
/// # Note
/// In the original libxml2, this function returns a string with escaped characters
/// that could be recognized as format specifiers ('%').
///
/// However, this operation is unnecessary in Rust
/// because string formatting is not performed at runtime,
/// and in fact, no such operation is performed.
#[doc(alias = "xmlSchemaFormatItemForReport")]
unsafe fn xml_schema_format_item_for_report(
    item_des: Option<&str>,
    item: XmlSchemaBasicItemPtr,
    item_node: Option<XmlGenericNodePtr>,
) -> String {
    unsafe {
        let mut named: i32 = 1;

        let mut res = String::new();

        if let Some(item_des) = item_des {
            res.push_str(item_des);
        } else if !item.is_null() {
            match (*item).typ {
                XmlSchemaTypeType::XmlSchemaTypeBasic => {
                    let typ: XmlSchemaTypePtr = item as XmlSchemaTypePtr;

                    if (*typ).wxs_is_atomic() {
                        res.push_str("atomic type 'xs:");
                    } else if (*typ).wxs_is_list() {
                        res.push_str("list type 'xs:");
                    } else if (*typ).wxs_is_union() {
                        res.push_str("union type 'xs:");
                    } else {
                        res.push_str("simple type 'xs:");
                    }
                    res.push_str(
                        CStr::from_ptr((*typ).name as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                    );
                    res.push('\'');
                }
                XmlSchemaTypeType::XmlSchemaTypeSimple => {
                    let typ: XmlSchemaTypePtr = item as XmlSchemaTypePtr;

                    if (*typ).flags & XML_SCHEMAS_TYPE_GLOBAL == 0 {
                        res.push_str("local ");
                    }
                    if (*typ).wxs_is_atomic() {
                        res.push_str("atomic type");
                    } else if (*typ).wxs_is_list() {
                        res.push_str("list type");
                    } else if (*typ).wxs_is_union() {
                        res.push_str("union type");
                    } else {
                        res.push_str("simple type");
                    }
                    if (*typ).flags & XML_SCHEMAS_TYPE_GLOBAL != 0 {
                        res.push_str(" '");
                        res.push_str(
                            CStr::from_ptr((*typ).name as *const i8)
                                .to_string_lossy()
                                .as_ref(),
                        );
                        res.push('\'');
                    }
                }
                XmlSchemaTypeType::XmlSchemaTypeComplex => {
                    let typ: XmlSchemaTypePtr = item as XmlSchemaTypePtr;

                    if (*typ).flags & XML_SCHEMAS_TYPE_GLOBAL == 0 {
                        res.push_str("local ");
                    }
                    res.push_str("complex type");
                    if (*typ).flags & XML_SCHEMAS_TYPE_GLOBAL != 0 {
                        res.push_str(" '");
                        res.push_str(
                            CStr::from_ptr((*typ).name as *const i8)
                                .to_string_lossy()
                                .as_ref(),
                        );
                        res.push('\'');
                    }
                }
                XmlSchemaTypeType::XmlSchemaTypeAttributeUse => {
                    let ause: XmlSchemaAttributeUsePtr = item as XmlSchemaAttributeUsePtr;
                    res.push_str("attribute use ");
                    if !(*ause).attr_decl.is_null() {
                        res.push('\'');
                        res.push_str(
                            xml_schema_get_component_qname((*ause).attr_decl as _).as_str(),
                        );
                        res.push('\'');
                    } else {
                        res.push_str("(unknown)");
                    }
                }
                XmlSchemaTypeType::XmlSchemaTypeAttribute => {
                    let attr: XmlSchemaAttributePtr = item as XmlSchemaAttributePtr;
                    res.push_str("attribute decl.");
                    res.push_str(" '");
                    res.push_str(
                        xml_schema_format_qname(
                            Some(
                                CStr::from_ptr((*attr).target_namespace as *const i8)
                                    .to_string_lossy()
                                    .as_ref(),
                            ),
                            Some(
                                CStr::from_ptr((*attr).name as *const i8)
                                    .to_string_lossy()
                                    .as_ref(),
                            ),
                        )
                        .as_str(),
                    );
                    res.push('\'');
                }
                XmlSchemaTypeType::XmlSchemaTypeAttributeGroup => {
                    res.push_str(xml_schema_get_component_designation(item as _).as_str());
                }
                XmlSchemaTypeType::XmlSchemaTypeElement => {
                    let elem: XmlSchemaElementPtr = item as XmlSchemaElementPtr;
                    res.push_str("element decl.");
                    res.push_str(" '");
                    let namespace_name = (*elem).target_namespace as *const i8;
                    res.push_str(
                        xml_schema_format_qname(
                            (!namespace_name.is_null())
                                .then(|| CStr::from_ptr(namespace_name).to_string_lossy())
                                .as_deref(),
                            Some(
                                CStr::from_ptr((*elem).name as *const i8)
                                    .to_string_lossy()
                                    .as_ref(),
                            ),
                        )
                        .as_str(),
                    );
                    res.push('\'');
                }
                XmlSchemaTypeType::XmlSchemaTypeIDCUnique
                | XmlSchemaTypeType::XmlSchemaTypeIDCKey
                | XmlSchemaTypeType::XmlSchemaTypeIDCKeyref => {
                    if (*item).typ == XmlSchemaTypeType::XmlSchemaTypeIDCUnique {
                        res.push_str("unique '");
                    } else if (*item).typ == XmlSchemaTypeType::XmlSchemaTypeIDCKey {
                        res.push_str("key '");
                    } else {
                        res.push_str("keyRef '");
                    }
                    res.push_str(
                        CStr::from_ptr((*(item as XmlSchemaIDCPtr)).name as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                    );
                    res.push('\'');
                }
                XmlSchemaTypeType::XmlSchemaTypeAny
                | XmlSchemaTypeType::XmlSchemaTypeAnyAttribute => {
                    let s = xml_schema_wildcard_pcto_string(
                        (*(item as XmlSchemaWildcardPtr)).process_contents,
                    );
                    res.push_str(s);
                    res.push_str(" wildcard");
                }
                XmlSchemaTypeType::XmlSchemaFacetMinInclusive
                | XmlSchemaTypeType::XmlSchemaFacetMinExclusive
                | XmlSchemaTypeType::XmlSchemaFacetMaxInclusive
                | XmlSchemaTypeType::XmlSchemaFacetMaxExclusive
                | XmlSchemaTypeType::XmlSchemaFacetTotalDigits
                | XmlSchemaTypeType::XmlSchemaFacetFractionDigits
                | XmlSchemaTypeType::XmlSchemaFacetPattern
                | XmlSchemaTypeType::XmlSchemaFacetEnumeration
                | XmlSchemaTypeType::XmlSchemaFacetWhitespace
                | XmlSchemaTypeType::XmlSchemaFacetLength
                | XmlSchemaTypeType::XmlSchemaFacetMaxLength
                | XmlSchemaTypeType::XmlSchemaFacetMinLength => {
                    res.push_str("facet '");
                    let s = xml_schema_facet_type_to_string((*item).typ);
                    res.push_str(s);
                    res.push('\'');
                }
                XmlSchemaTypeType::XmlSchemaTypeGroup => {
                    res.push_str("model group def.");
                    res.push_str(" '");
                    res.push_str(xml_schema_get_component_qname(item as _).as_str());
                    res.push('\'');
                }
                XmlSchemaTypeType::XmlSchemaTypeSequence
                | XmlSchemaTypeType::XmlSchemaTypeChoice
                | XmlSchemaTypeType::XmlSchemaTypeAll
                | XmlSchemaTypeType::XmlSchemaTypeParticle => {
                    let typestr = xml_schema_get_component_type_str(item as _);
                    res.push_str(typestr);
                }
                XmlSchemaTypeType::XmlSchemaTypeNotation => {
                    let typestr = xml_schema_get_component_type_str(item as _);
                    res.push_str(typestr);
                    res.push_str(" '");
                    res.push_str(xml_schema_get_component_qname(item as _).as_str());
                    res.push('\'');
                    named = 0;
                }
                _ => {
                    named = 0;
                }
            }
        } else {
            named = 0;
        }

        if named == 0 {
            if let Some(item_node) = item_node {
                let elem = if let Ok(attr) = XmlAttrPtr::try_from(item_node) {
                    attr.parent.unwrap()
                } else {
                    XmlNodePtr::try_from(item_node).unwrap()
                };
                res.push_str("Element '");
                if let Some(ns) = elem.ns {
                    res.push_str(
                        xml_schema_format_qname(ns.href().as_deref(), elem.name().as_deref())
                            .as_str(),
                    );
                } else {
                    res.push_str(&elem.name);
                }
                res.push('\'');
            }
        }
        if let Some(attr) = item_node.and_then(|node| XmlAttrPtr::try_from(node).ok()) {
            res.push_str(", attribute '");
            if let Some(ns) = attr.ns {
                res.push_str(
                    xml_schema_format_qname(ns.href().as_deref(), attr.name().as_deref()).as_str(),
                );
            } else {
                res.push_str(&attr.name);
            }
            res.push('\'');
        }

        res
    }
}

/// # Note
/// In the original libxml2, this function returns a string with escaped characters
/// that could be recognized as format specifiers ('%').
///
/// However, this operation is unnecessary in Rust
/// because string formatting is not performed at runtime,
/// and in fact, no such operation is performed.
unsafe fn xml_schema_format_node_for_error(
    actxt: XmlSchemaAbstractCtxtPtr,
    node: Option<XmlGenericNodePtr>,
) -> Option<String> {
    unsafe {
        if node.is_some_and(|node| {
            !matches!(
                node.element_type(),
                XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode
            )
        }) {
            // Don't try to format other nodes than element and
            // attribute nodes.
            // Play safe and return an empty string.
            return Some("".to_owned());
        }

        let mut res = String::new();
        if let Some(node) = node {
            // Work on tree nodes.
            let (name, ns) = if let Ok(node) = XmlAttrPtr::try_from(node) {
                let elem = node.parent.unwrap();

                res.push_str("Element '");
                if let Some(ns) = elem.ns {
                    res.push_str(
                        xml_schema_format_qname(ns.href().as_deref(), elem.name().as_deref())
                            .as_str(),
                    );
                } else {
                    res.push_str(xml_schema_format_qname(None, elem.name().as_deref()).as_str());
                }
                res.push_str("', ");
                res.push_str("attribute '");
                (node.name.to_string(), node.ns)
            } else {
                res.push_str("Element '");
                let node = XmlNodePtr::try_from(node).unwrap();
                (node.name().unwrap().into_owned(), node.ns)
            };
            if let Some(ns) = ns {
                res.push_str(xml_schema_format_qname(ns.href().as_deref(), Some(&name)).as_str());
            } else {
                res.push_str(xml_schema_format_qname(None, Some(&name)).as_str());
            }
            res.push_str("': ");
        } else if (*actxt).typ == XML_SCHEMA_CTXT_VALIDATOR {
            let vctxt: XmlSchemaValidCtxtPtr = actxt as XmlSchemaValidCtxtPtr;
            // Work on node infos.
            if (*(*vctxt).inode).node_type == XmlElementType::XmlAttributeNode as i32 {
                let ielem: XmlSchemaNodeInfoPtr = *(*vctxt).elem_infos.add((*vctxt).depth as usize);

                res.push_str("Element '");
                res.push_str(
                    xml_schema_format_qname(
                        (*ielem).ns_name.as_deref(),
                        (*ielem).local_name.as_deref(),
                    )
                    .as_str(),
                );
                res.push_str("', ");
                res.push_str("attribute '");
            } else {
                res.push_str("Element '");
            }

            res.push_str(
                xml_schema_format_qname(
                    (*(*vctxt).inode).ns_name.as_deref(),
                    (*(*vctxt).inode).local_name.as_deref(),
                )
                .as_str(),
            );
            res.push_str("': ");
        } else if (*actxt).typ == XML_SCHEMA_CTXT_PARSER {
            // Hmm, no node while parsing?
            // Return an empty string, in case NULL will break something.
        } else {
            // TODO
            return None;
        }

        // VAL TODO: The output of the given schema component is currently disabled.
        // #if 0
        // if ((type != null_mut()) && (xmlSchemaIsGlobalItem(type))) {
        //     *msg = xmlStrcat(*msg, c" [".as_ptr() as _);
        //     *msg = xmlStrcat(*msg, xmlSchemaFormatItemForReport(addr_of_mut!(str), null_mut(), type, null_mut(), 0));
        //     FREE_AND_NULL!(str);
        //     *msg = xmlStrcat(*msg, c"]".as_ptr() as _);
        // }
        // #endif
        Some(res)
    }
}

/// # Note
/// This function does not format string.
pub(crate) unsafe fn xml_schema_err(
    actxt: XmlSchemaAbstractCtxtPtr,
    error: XmlParserErrors,
    node: Option<XmlGenericNodePtr>,
    msg: &str,
    str1: Option<&str>,
    str2: Option<&str>,
) {
    unsafe {
        xml_schema_err4(actxt, error, node, msg, str1, str2, None, None);
    }
}

/// # Note
/// This function does not format string.
#[allow(clippy::too_many_arguments)]
pub(crate) unsafe fn xml_schema_err4(
    actxt: XmlSchemaAbstractCtxtPtr,
    error: XmlParserErrors,
    node: Option<XmlGenericNodePtr>,
    msg: &str,
    str1: Option<&str>,
    str2: Option<&str>,
    str3: Option<&str>,
    str4: Option<&str>,
) {
    unsafe {
        xml_schema_err4_line(
            actxt,
            XmlErrorLevel::XmlErrError,
            error,
            node,
            0,
            msg,
            str1,
            str2,
            str3,
            str4,
        );
    }
}

/// # Note
/// This function does not format string.
pub(crate) unsafe fn xml_schema_internal_err2(
    actxt: XmlSchemaAbstractCtxtPtr,
    func_name: &str,
    message: &str,
    str1: Option<&str>,
    str2: Option<&str>,
) {
    unsafe {
        if actxt.is_null() {
            return;
        }
        let msg = format!("Internal error: {func_name}, {message}.\n");
        if (*actxt).typ == XML_SCHEMA_CTXT_VALIDATOR {
            xml_schema_err3(
                actxt,
                XmlParserErrors::XmlSchemavInternal as _,
                None,
                msg.as_str(),
                Some(func_name),
                str1,
                str2,
            );
        } else if (*actxt).typ == XML_SCHEMA_CTXT_PARSER {
            xml_schema_err3(
                actxt,
                XmlParserErrors::XmlSchemapInternal as _,
                None,
                msg.as_str(),
                Some(func_name),
                str1,
                str2,
            );
        }
    }
}

/// Handle a validation error
///
/// # Note
/// This function does not format string.
#[doc(alias = "xmlSchemaErr3")]
pub(crate) unsafe fn xml_schema_err3(
    actxt: XmlSchemaAbstractCtxtPtr,
    error: XmlParserErrors,
    node: Option<XmlGenericNodePtr>,
    msg: &str,
    str1: Option<&str>,
    str2: Option<&str>,
    str3: Option<&str>,
) {
    unsafe {
        xml_schema_err4_line(
            actxt,
            XmlErrorLevel::XmlErrError,
            error,
            node,
            0,
            msg,
            str1,
            str2,
            str3,
            None,
        );
    }
}

/// Handle a validation error
///
/// # Note
/// This function does not format string.
#[doc(alias = "xmlSchemaErr4Line")]
#[allow(clippy::too_many_arguments)]
pub(crate) unsafe fn xml_schema_err4_line(
    ctxt: XmlSchemaAbstractCtxtPtr,
    error_level: XmlErrorLevel,
    error: XmlParserErrors,
    mut node: Option<XmlGenericNodePtr>,
    mut line: i32,
    msg: &str,
    str1: Option<&str>,
    str2: Option<&str>,
    str3: Option<&str>,
    _str4: Option<&str>,
) {
    unsafe {
        let schannel: Option<StructuredError>;
        let channel: Option<GenericError>;

        if !ctxt.is_null() {
            if (*ctxt).typ == XML_SCHEMA_CTXT_VALIDATOR {
                let vctxt: XmlSchemaValidCtxtPtr = ctxt as XmlSchemaValidCtxtPtr;
                #[allow(unused_assignments)]
                let mut dummy = Some(c"".to_owned());
                let mut file: *const i8 = null();
                let mut col: i32 = 0;
                if !matches!(error_level, XmlErrorLevel::XmlErrWarning) {
                    (*vctxt).nberrors += 1;
                    (*vctxt).err = error as i32;
                    channel = (*vctxt).error;
                } else {
                    channel = (*vctxt).warning;
                }
                schannel = (*vctxt).serror;
                let data = (*vctxt).err_ctxt.clone();

                // Error node. If we specify a line number, then
                // do not channel any node to the error function.
                if line == 0 {
                    if node.is_none() && (*vctxt).depth >= 0 && !(*vctxt).inode.is_null() {
                        node = (*(*vctxt).inode).node.map(|node| node.into());
                    }
                    // Get filename and line if no node-tree.
                    if node.is_none() {
                        if let Some(input) = (*vctxt)
                            .parser_ctxt
                            .as_deref()
                            .and_then(|ctxt| ctxt.input())
                        {
                            dummy = input.filename.as_deref().map(|f| CString::new(f).unwrap());
                            file = dummy.as_ref().map_or(null(), |c| c.as_ptr());
                            line = input.line;
                            col = input.col;
                        }
                    }
                } else {
                    // Override the given node's (if any) position
                    // and channel only the given line number.
                    node = None;
                    // Get filename.
                    if let Some(doc) = (*vctxt).doc {
                        dummy = doc.url.as_deref().map(|u| CString::new(u).unwrap());
                        file = dummy.as_ref().map_or(null(), |c| c.as_ptr());
                    } else if let Some(input) = (*vctxt)
                        .parser_ctxt
                        .as_deref()
                        .and_then(|ctxt| ctxt.input())
                    {
                        dummy = input.filename.as_deref().map(|f| CString::new(f).unwrap());
                        file = dummy.as_ref().map_or(null(), |c| c.as_ptr());
                    }
                }
                if let Some(loc_func) = (*vctxt).loc_func {
                    if file.is_null() || line == 0 {
                        let mut l: u64 = 0;
                        let mut f = None;
                        loc_func((*vctxt).loc_ctxt, &raw mut f, &raw mut l);
                        if file.is_null() {
                            dummy = f.as_deref().map(|f| CString::new(f).unwrap());
                            file = dummy.as_ref().map_or(null(), |c| c.as_ptr());
                        }
                        if line == 0 {
                            line = l as _;
                        }
                    }
                }
                let file = if file.is_null() && (*vctxt).filename.is_some() {
                    (*vctxt).filename.clone()
                } else {
                    (!file.is_null()).then(|| CStr::from_ptr(file).to_string_lossy().into_owned())
                };

                __xml_raise_error!(
                    schannel,
                    channel,
                    data,
                    ctxt as _,
                    node,
                    XmlErrorDomain::XmlFromSchemasv,
                    error,
                    error_level,
                    file.map(|f| f.into()),
                    line,
                    str1.map(|s| s.to_owned().into()),
                    str2.map(|s| s.to_owned().into()),
                    str3.map(|s| s.to_owned().into()),
                    0,
                    col,
                    Some(msg),
                );
            } else if (*ctxt).typ == XML_SCHEMA_CTXT_PARSER {
                let pctxt: XmlSchemaParserCtxtPtr = ctxt as XmlSchemaParserCtxtPtr;
                if !matches!(error_level, XmlErrorLevel::XmlErrWarning) {
                    (*pctxt).nberrors += 1;
                    (*pctxt).err = error as i32;
                    channel = (*pctxt).error;
                } else {
                    channel = (*pctxt).warning;
                }
                schannel = (*pctxt).serror;
                let data = (*pctxt).err_ctxt.clone();
                __xml_raise_error!(
                    schannel,
                    channel,
                    data,
                    ctxt as _,
                    node,
                    XmlErrorDomain::XmlFromSchemasp,
                    error,
                    error_level,
                    None,
                    0,
                    str1.map(|s| s.to_owned().into()),
                    str2.map(|s| s.to_owned().into()),
                    str3.map(|s| s.to_owned().into()),
                    0,
                    0,
                    Some(msg),
                );
            } else {
                // TODO
                todo!()
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
pub(crate) unsafe fn xml_schema_custom_warning(
    actxt: XmlSchemaAbstractCtxtPtr,
    error: XmlParserErrors,
    node: Option<XmlGenericNodePtr>,
    _typ: Option<&XmlSchemaType>,
    message: &str,
    str1: Option<&str>,
    str2: Option<&str>,
    str3: Option<&str>,
) {
    unsafe {
        use std::fmt::Write as _;

        let mut msg =
            xml_schema_format_node_for_error(actxt, node).unwrap_or_else(|| "".to_owned());
        writeln!(msg, "{message}.").ok();

        // URGENT TODO: Set the error code to something sane.
        xml_schema_err4_line(
            actxt,
            XmlErrorLevel::XmlErrWarning,
            error as _,
            node,
            0,
            msg.as_str(),
            str1,
            str2,
            str3,
            None,
        );
    }
}

/// # Note
/// This function does not format string.
pub(crate) unsafe fn xml_schema_custom_err(
    actxt: XmlSchemaAbstractCtxtPtr,
    error: XmlParserErrors,
    node: Option<XmlGenericNodePtr>,
    item: XmlSchemaBasicItemPtr,
    message: &str,
    str1: Option<&str>,
    str2: Option<&str>,
) {
    unsafe {
        xml_schema_custom_err4(actxt, error, node, item, message, str1, str2, None, None);
    }
}

/// # Note
/// This function does not format string.
#[allow(clippy::too_many_arguments)]
pub(crate) unsafe fn xml_schema_custom_err4(
    actxt: XmlSchemaAbstractCtxtPtr,
    error: XmlParserErrors,
    mut node: Option<XmlGenericNodePtr>,
    item: XmlSchemaBasicItemPtr,
    message: &str,
    str1: Option<&str>,
    str2: Option<&str>,
    str3: Option<&str>,
    str4: Option<&str>,
) {
    unsafe {
        let mut msg =
            if node.is_none() && !item.is_null() && (*actxt).typ == XML_SCHEMA_CTXT_PARSER {
                node = xml_schema_get_component_node(item as _).map(|node| node.into());
                let mut res = xml_schema_format_item_for_report(None, item, None);
                res.push_str(": ");
                Some(res)
            } else {
                xml_schema_format_node_for_error(actxt, node)
            }
            .unwrap_or_else(|| "".to_owned());
        msg.push_str(message);
        msg.push_str(".\n");
        xml_schema_err4(actxt, error, node, msg.as_str(), str1, str2, str3, str4);
    }
}

/// # Note
/// This function does not format string.
pub(crate) unsafe fn xml_schema_internal_err(
    actxt: XmlSchemaAbstractCtxtPtr,
    func_name: &str,
    message: &str,
) {
    unsafe {
        xml_schema_internal_err2(actxt, func_name, message, None, None);
    }
}

/// # Note
/// This function does not format string.
#[allow(clippy::too_many_arguments)]
pub(crate) unsafe fn xml_schema_facet_err(
    actxt: XmlSchemaAbstractCtxtPtr,
    error: XmlParserErrors,
    node: Option<XmlGenericNodePtr>,
    value: &str,
    length: u64,
    typ: &XmlSchemaType,
    facet: &XmlSchemaFacet,
    message: Option<&str>,
    str1: Option<&str>,
    str2: Option<&str>,
) {
    unsafe {
        use std::fmt::Write as _;

        let mut str: *mut u8 = null_mut();
        let node_type: i32 = xml_schema_eval_error_node_type(actxt, node);

        let mut msg =
            xml_schema_format_node_for_error(actxt, node).unwrap_or_else(|| "".to_owned());
        let facet_type = if matches!(error, XmlParserErrors::XmlSchemavCvcEnumerationValid) {
            XmlSchemaTypeType::XmlSchemaFacetEnumeration
        // If enumerations are validated, one must not expect the facet to be given.
        } else {
            facet.typ
        };
        write!(
            msg,
            "[facet '{}'] ",
            xml_schema_facet_type_to_string(facet_type)
        )
        .ok();
        if let Some(message) = message {
            writeln!(msg, "{message}.").ok();
            xml_schema_err(actxt, error, node, msg.as_str(), str1, str2);
        } else {
            // Use a default message.
            if matches!(
                facet_type,
                XmlSchemaTypeType::XmlSchemaFacetLength
                    | XmlSchemaTypeType::XmlSchemaFacetMinLength
                    | XmlSchemaTypeType::XmlSchemaFacetMaxLength
            ) {
                let len = xml_schema_get_facet_value_as_ulong(facet).to_string();
                let act_len = length.to_string();

                // FIXME, TODO: What is the max expected string length of the this value?
                if node_type == XmlElementType::XmlAttributeNode as i32 {
                    msg.push_str(
                        format!("The value '{value}' has a length of '{act_len}'; ").as_str(),
                    );
                } else {
                    msg.push_str(format!("The value has a length of '{act_len}'; ").as_str());
                }

                if facet_type == XmlSchemaTypeType::XmlSchemaFacetLength {
                    msg.push_str(
                        format!("this differs from the allowed length of '{len}'.\n").as_str(),
                    );
                } else if facet_type == XmlSchemaTypeType::XmlSchemaFacetMaxLength {
                    msg.push_str(
                        format!("this exceeds the allowed maximum length of '{len}'.\n").as_str(),
                    );
                } else if facet_type == XmlSchemaTypeType::XmlSchemaFacetMinLength {
                    msg.push_str(
                        format!("this underruns the allowed minimum length of '{len}'.\n").as_str(),
                    );
                }

                if node_type == XmlElementType::XmlAttributeNode as i32 {
                    xml_schema_err3(
                        actxt,
                        error,
                        node,
                        msg.as_str(),
                        Some(value),
                        Some(&act_len),
                        Some(&len),
                    );
                } else {
                    xml_schema_err(actxt, error, node, msg.as_str(), Some(&act_len), Some(&len));
                }
            } else if facet_type == XmlSchemaTypeType::XmlSchemaFacetEnumeration {
                let set = xml_schema_format_facet_enum_set(actxt, &raw mut str, typ);
                let set = CStr::from_ptr(set as *const i8).to_string_lossy();
                msg.push_str(
                    format!("The value '{value}' is not an element of the set {{{set}}}.\n")
                        .as_str(),
                );
                xml_schema_err(actxt, error, node, msg.as_str(), Some(value), Some(&set));
            } else if facet_type == XmlSchemaTypeType::XmlSchemaFacetPattern {
                let facet_value = CStr::from_ptr(facet.value as *const i8).to_string_lossy();
                msg.push_str(
                    format!(
                        "The value '{value}' is not accepted by the pattern '{facet_value}'.\n"
                    )
                    .as_str(),
                );
                xml_schema_err(
                    actxt,
                    error,
                    node,
                    msg.as_str(),
                    Some(value),
                    Some(&facet_value),
                );
            } else if facet_type == XmlSchemaTypeType::XmlSchemaFacetMinInclusive {
                let facet_value = CStr::from_ptr(facet.value as *const i8).to_string_lossy();
                msg.push_str(
                format!("The value '{value}' is less than the minimum value allowed ('{facet_value}').\n")
                    .as_str(),
            );
                xml_schema_err(
                    actxt,
                    error,
                    node,
                    msg.as_str(),
                    Some(value),
                    Some(&facet_value),
                );
            } else if facet_type == XmlSchemaTypeType::XmlSchemaFacetMaxInclusive {
                let facet_value = CStr::from_ptr(facet.value as *const i8).to_string_lossy();
                msg.push_str(
                format!("The value '{value}' is greater than the maximum value allowed ('{facet_value}').\n")
                    .as_str(),
            );
                xml_schema_err(
                    actxt,
                    error,
                    node,
                    msg.as_str(),
                    Some(value),
                    Some(&facet_value),
                );
            } else if facet_type == XmlSchemaTypeType::XmlSchemaFacetMinExclusive {
                let facet_value = CStr::from_ptr(facet.value as *const i8).to_string_lossy();
                msg.push_str(
                    format!("The value '{value}' must be greater than '{facet_value}'.\n").as_str(),
                );
                xml_schema_err(
                    actxt,
                    error,
                    node,
                    msg.as_str(),
                    Some(value),
                    Some(&facet_value),
                );
            } else if facet_type == XmlSchemaTypeType::XmlSchemaFacetMaxExclusive {
                let facet_value = CStr::from_ptr(facet.value as *const i8).to_string_lossy();
                msg.push_str(
                    format!("The value '{value}' must be less than '{facet_value}'.\n").as_str(),
                );
                xml_schema_err(
                    actxt,
                    error,
                    node,
                    msg.as_str(),
                    Some(value),
                    Some(&facet_value),
                );
            } else if facet_type == XmlSchemaTypeType::XmlSchemaFacetTotalDigits {
                let facet_value = CStr::from_ptr(facet.value as *const i8).to_string_lossy();
                msg.push_str(
                    format!(
                        "The value '{value}' has more digits than are allowed ('{facet_value}').\n"
                    )
                    .as_str(),
                );
                xml_schema_err(
                    actxt,
                    error,
                    node,
                    msg.as_str(),
                    Some(value),
                    Some(&facet_value),
                );
            } else if facet_type == XmlSchemaTypeType::XmlSchemaFacetFractionDigits {
                let facet_value = CStr::from_ptr(facet.value as *const i8).to_string_lossy();
                msg.push_str(
                format!(
                    "The value '{value}' has more fractional digits than are allowed ('{facet_value}').\n"
                )
                .as_str(),
            );
                xml_schema_err(
                    actxt,
                    error,
                    node,
                    msg.as_str(),
                    Some(value),
                    Some(&facet_value),
                );
            } else if node_type == XmlElementType::XmlAttributeNode as i32 {
                msg.push_str(format!("The value '{value}' is not facet-valid.\n").as_str());
                xml_schema_err(actxt, error, node, msg.as_str(), Some(value), None);
            } else {
                msg.push_str("The value is not facet-valid.\n");
                xml_schema_err(actxt, error, node, msg.as_str(), None, None);
            }
        }
        FREE_AND_NULL!(str);
    }
}

pub(crate) unsafe fn xml_schema_simple_type_err(
    actxt: XmlSchemaAbstractCtxtPtr,
    error: XmlParserErrors,
    node: Option<XmlGenericNodePtr>,
    value: &str,
    typ: &XmlSchemaType,
    display_value: i32,
) {
    unsafe {
        let mut msg =
            xml_schema_format_node_for_error(actxt, node).unwrap_or_else(|| "".to_owned());

        if display_value != 0
            || xml_schema_eval_error_node_type(actxt, node)
                == XmlElementType::XmlAttributeNode as i32
        {
            msg.push_str(format!("'{value}' is not a valid value of ").as_str());
        } else {
            msg.push_str("The character content is not a valid value of ");
        }

        if !typ.is_global_item() {
            msg.push_str("the local ");
        } else {
            msg.push_str("the ");
        }

        if (*typ).wxs_is_atomic() {
            msg.push_str("atomic type");
        } else if (*typ).wxs_is_list() {
            msg.push_str("list type");
        } else if (*typ).wxs_is_union() {
            msg.push_str("union type");
        }

        if typ.is_global_item() {
            msg.push_str(" '");
            let mut str = if typ.built_in_type != XmlSchemaValType::XmlSchemasUnknown {
                msg.push_str("xs:");
                xml_strdup(typ.name)
            } else {
                let namespace_name = typ.target_namespace as *const i8;
                let qname = xml_schema_format_qname(
                    (!namespace_name.is_null())
                        .then(|| CStr::from_ptr(namespace_name).to_string_lossy())
                        .as_deref(),
                    Some(
                        CStr::from_ptr(typ.name as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                    ),
                );
                xml_strndup(qname.as_ptr(), qname.len() as i32)
            };
            msg.push_str(CStr::from_ptr(str as *const i8).to_string_lossy().as_ref());
            msg.push('\'');
            FREE_AND_NULL!(str);
        }
        msg.push_str(".\n");
        if display_value != 0
            || xml_schema_eval_error_node_type(actxt, node)
                == XmlElementType::XmlAttributeNode as i32
        {
            xml_schema_err(actxt, error as _, node, msg.as_str(), Some(value), None);
        } else {
            xml_schema_err(actxt, error as _, node, msg.as_str(), None, None);
        }
    }
}

/// Reports a simple type validation error.
/// TODO: Should this report the value of an element as well?
#[allow(clippy::too_many_arguments)]
#[doc(alias = "xmlSchemaPSimpleTypeErr")]
pub(crate) unsafe fn xml_schema_psimple_type_err(
    ctxt: XmlSchemaParserCtxtPtr,
    error: XmlParserErrors,
    _owner_item: XmlSchemaBasicItemPtr,
    node: XmlGenericNodePtr,
    typ: Option<&XmlSchemaType>,
    expected: Option<&str>,
    value: Option<&str>,
    message: Option<&str>,
    str1: Option<&str>,
    str2: Option<&str>,
) {
    unsafe {
        use std::fmt::Write as _;

        let mut msg =
            xml_schema_format_node_for_error(ctxt as XmlSchemaAbstractCtxtPtr, Some(node))
                .unwrap_or_else(|| "".to_owned());
        if let Some(message) = message {
            writeln!(msg, "{message}.").ok();
            xml_schema_perr_ext(
                ctxt,
                Some(node),
                error,
                None,
                None,
                None,
                &msg,
                str1,
                str2,
                None,
                None,
                None,
            );
        } else {
            // Use default messages.
            if let Some(typ) = typ {
                if node.element_type() == XmlElementType::XmlAttributeNode {
                    msg.push_str(format!("'{}' is not a valid value of ", value.unwrap()).as_str());
                } else {
                    msg.push_str("The character content is not a valid value of ");
                }
                if !typ.is_global_item() {
                    msg.push_str("the local ");
                } else {
                    msg.push_str("the ");
                }

                if typ.wxs_is_atomic() {
                    msg.push_str("atomic type");
                } else if typ.wxs_is_list() {
                    msg.push_str("list type");
                } else if typ.wxs_is_union() {
                    msg.push_str("union type");
                }

                if typ.is_global_item() {
                    msg.push_str(" '");
                    let mut str = if typ.built_in_type != XmlSchemaValType::XmlSchemasUnknown {
                        msg.push_str("xs:");
                        xml_strdup(typ.name)
                    } else {
                        let qname = xml_schema_format_qname(
                            Some(
                                CStr::from_ptr(typ.target_namespace as *const i8)
                                    .to_string_lossy()
                                    .as_ref(),
                            ),
                            Some(
                                CStr::from_ptr(typ.name as *const i8)
                                    .to_string_lossy()
                                    .as_ref(),
                            ),
                        );
                        xml_strndup(qname.as_ptr(), qname.len() as i32)
                    };
                    msg.push_str(
                        CStr::from_ptr(xml_escape_format_string(&raw mut str) as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                    );
                    msg.push_str("'.");
                    FREE_AND_NULL!(str);
                }
            } else if node.element_type() == XmlElementType::XmlAttributeNode {
                msg.push_str(format!("The value '{}' is not valid.", value.unwrap()).as_str());
            } else {
                msg.push_str("The character content is not valid.");
            }
            if let Some(expected) = expected {
                let mut expected_escaped =
                    xml_char_strndup(expected.as_ptr() as *const i8, expected.len() as i32);
                msg.push_str(" Expected is '");

                msg.push_str(
                    CStr::from_ptr(
                        xml_escape_format_string(&raw mut expected_escaped) as *const i8
                    )
                    .to_string_lossy()
                    .as_ref(),
                );
                FREE_AND_NULL!(expected_escaped);
                msg.push_str("'.\n");
            } else {
                msg.push('\n');
            }
            if node.element_type() == XmlElementType::XmlAttributeNode {
                xml_schema_perr(ctxt, Some(node), error, &msg, value, None);
            } else {
                xml_schema_perr(ctxt, Some(node), error, &msg, None, None);
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
pub(crate) unsafe fn xml_schema_complex_type_err(
    actxt: XmlSchemaAbstractCtxtPtr,
    error: XmlParserErrors,
    node: Option<XmlGenericNodePtr>,
    _typ: Option<&XmlSchemaType>,
    message: &str,
    nbval: usize,
    nbneg: usize,
    values: &[Cow<'static, str>],
) {
    use std::fmt::Write as _;

    unsafe {
        let mut str: *mut u8;
        let mut local_name: *mut u8;
        let mut ns_name: *mut u8;

        let mut msg =
            xml_schema_format_node_for_error(actxt, node).unwrap_or_else(|| "".to_owned());
        write!(msg, "{message}.").ok();
        // Note that is does not make sense to report that we have a wildcard here,
        // since the wildcard might be unfolded into multiple transitions.
        if nbval + nbneg > 0 {
            if nbval + nbneg > 1 {
                str = xml_strdup(c" Expected is one of ( ".as_ptr() as _);
            } else {
                str = xml_strdup(c" Expected is ( ".as_ptr() as _);
            }
            // ns_name = null_mut();

            for i in 0..nbval + nbneg {
                let mut cur = values[i].as_ref();
                if let Some(rem) = cur.strip_prefix("not ") {
                    cur = rem;
                    str = xml_strcat(str, c"##other".as_ptr() as _);
                }
                // Get the local name.
                local_name = null_mut();

                let mut end = cur;
                if let Some(rem) = end.strip_prefix('*') {
                    local_name = xml_strdup(c"*".as_ptr() as _);
                    end = rem;
                } else {
                    end = end.trim_start_matches(|c| c != '|');
                    local_name = xml_strncat(
                        local_name,
                        cur.as_ptr(),
                        cur.len() as i32 - end.len() as i32,
                    );
                }
                if !end.is_empty() {
                    end = &end[1..];
                    // Skip "*|*" if they come with negated expressions, since
                    // they represent the same negated wildcard.
                    if nbneg == 0 || !end.starts_with('*') || *local_name != b'*' {
                        // Get the namespace name.
                        cur = end;
                        if end.starts_with('*') {
                            ns_name = xml_strdup(c"{*}".as_ptr() as _);
                        } else {
                            if i >= nbval {
                                ns_name = xml_strdup(c"{##other:".as_ptr() as _);
                            } else {
                                ns_name = xml_strdup(c"{".as_ptr() as _);
                            }

                            ns_name = xml_strncat(ns_name, cur.as_ptr(), cur.len() as i32);
                            ns_name = xml_strcat(ns_name, c"}".as_ptr() as _);
                        }
                        str = xml_strcat(str, ns_name);
                        FREE_AND_NULL!(ns_name);
                    } else {
                        FREE_AND_NULL!(local_name);
                        continue;
                    }
                }
                str = xml_strcat(str, local_name);
                FREE_AND_NULL!(local_name);

                if i < nbval + nbneg - 1 {
                    str = xml_strcat(str, c", ".as_ptr() as _);
                }
            }
            str = xml_strcat(str, c" ).\n".as_ptr() as _);
            msg.push_str(CStr::from_ptr(str as *const i8).to_string_lossy().as_ref());
            FREE_AND_NULL!(str);
        } else {
            msg.push('\n');
        }
        xml_schema_err(actxt, error, node, msg.as_str(), None, None);
    }
}

pub(crate) unsafe fn xml_schema_illegal_attr_err(
    actxt: XmlSchemaAbstractCtxtPtr,
    error: XmlParserErrors,
    ni: Option<&XmlSchemaAttrInfo>,
    node: Option<XmlGenericNodePtr>,
) {
    unsafe {
        let mut msg =
            xml_schema_format_node_for_error(actxt, node).unwrap_or_else(|| "".to_owned());
        let qname = xml_schema_format_error_node_qname(ni, node).unwrap();

        msg.push_str(format!("The attribute '{qname}' is not allowed.\n").as_str());
        xml_schema_err(actxt, error, node, msg.as_str(), Some(&qname), None);
    }
}

/// Reports an illegal attribute during the parse.
#[doc(alias = "xmlSchemaPIllegalAttrErr")]
pub(crate) unsafe fn xml_schema_pillegal_attr_err(
    ctxt: XmlSchemaParserCtxtPtr,
    error: XmlParserErrors,
    _owner_comp: XmlSchemaBasicItemPtr,
    attr: XmlAttrPtr,
) {
    unsafe {
        let str1 = xml_schema_format_node_for_error(
            ctxt as XmlSchemaAbstractCtxtPtr,
            attr.parent.map(|p| p.into()),
        )
        .unwrap();
        let str2 = xml_schema_format_qname_ns(attr.ns, attr.name().as_deref());
        xml_schema_err4(
            ctxt as XmlSchemaAbstractCtxtPtr,
            error,
            Some(attr.into()),
            format!("{str1}The attribute '{str2}' is not allowed.\n").as_str(),
            Some(&str1),
            Some(&str2),
            None,
            None,
        );
    }
}

/// Handle a parser error
///
/// # Note
/// This function does not format string.
#[doc(alias = "xmlSchemaPErr")]
pub(crate) unsafe fn xml_schema_perr(
    ctxt: XmlSchemaParserCtxtPtr,
    node: Option<XmlGenericNodePtr>,
    error: XmlParserErrors,
    msg: &str,
    str1: Option<&str>,
    str2: Option<&str>,
) {
    unsafe {
        let mut channel: Option<GenericError> = None;
        let mut schannel: Option<StructuredError> = None;
        let mut data = None;

        if !ctxt.is_null() {
            (*ctxt).nberrors += 1;
            (*ctxt).err = error as i32;
            channel = (*ctxt).error;
            data = (*ctxt).err_ctxt.clone();
            schannel = (*ctxt).serror;
        }
        __xml_raise_error!(
            schannel,
            channel,
            data,
            ctxt as _,
            node,
            XmlErrorDomain::XmlFromSchemasp,
            error,
            XmlErrorLevel::XmlErrError,
            None,
            0,
            str1.map(|s| s.to_owned().into()),
            str2.map(|s| s.to_owned().into()),
            None,
            0,
            0,
            Some(msg),
        );
    }
}

/// Handle a parser error
///
/// # Note
/// This function does not format string.
#[doc(alias = "xmlSchemaPErr2")]
pub(crate) unsafe fn xml_schema_perr2(
    ctxt: XmlSchemaParserCtxtPtr,
    node: Option<XmlGenericNodePtr>,
    child: Option<XmlGenericNodePtr>,
    error: XmlParserErrors,
    msg: &str,
    str1: Option<&str>,
    str2: Option<&str>,
) {
    unsafe {
        if child.is_some() {
            xml_schema_perr(ctxt, child, error, msg, str1, str2);
        } else {
            xml_schema_perr(ctxt, node, error, msg, str1, str2);
        }
    }
}

/// Handle a parser error
///
/// # Note
/// This funtion does not format string.
#[allow(clippy::too_many_arguments)]
#[doc(alias = "xmlSchemaPErrExt")]
unsafe fn xml_schema_perr_ext(
    ctxt: XmlSchemaParserCtxtPtr,
    node: Option<XmlGenericNodePtr>,
    error: XmlParserErrors,
    str_data1: Option<&str>,
    str_data2: Option<&str>,
    str_data3: Option<&str>,
    msg: &str,
    _str1: Option<&str>,
    _str2: Option<&str>,
    _str3: Option<&str>,
    _str4: Option<&str>,
    _str5: Option<&str>,
) {
    unsafe {
        let mut channel: Option<GenericError> = None;
        let mut schannel: Option<StructuredError> = None;
        let mut data = None;

        if !ctxt.is_null() {
            (*ctxt).nberrors += 1;
            (*ctxt).err = error as i32;
            channel = (*ctxt).error;
            data = (*ctxt).err_ctxt.clone();
            schannel = (*ctxt).serror;
        }
        __xml_raise_error!(
            schannel,
            channel,
            data,
            ctxt as _,
            node,
            XmlErrorDomain::XmlFromSchemasp,
            error,
            XmlErrorLevel::XmlErrError,
            None,
            0,
            str_data1.map(|s| s.to_owned().into()),
            str_data2.map(|s| s.to_owned().into()),
            str_data3.map(|s| s.to_owned().into()),
            0,
            0,
            Some(msg),
        );
    }
}

/// Reports an error during parsing.
pub(crate) unsafe fn xml_schema_pcustom_err(
    ctxt: XmlSchemaParserCtxtPtr,
    error: XmlParserErrors,
    item: XmlSchemaBasicItemPtr,
    item_elem: Option<XmlGenericNodePtr>,
    message: &str,
    str1: Option<&str>,
) {
    unsafe {
        xml_schema_pcustom_err_ext(ctxt, error, item, item_elem, message, str1, None, None);
    }
}

/// Reports an error during parsing.
///
/// # Note
/// This function does not format string.
#[allow(clippy::too_many_arguments)]
pub(crate) unsafe fn xml_schema_pcustom_err_ext(
    ctxt: XmlSchemaParserCtxtPtr,
    error: XmlParserErrors,
    item: XmlSchemaBasicItemPtr,
    mut item_elem: Option<XmlGenericNodePtr>,
    message: &str,
    str1: Option<&str>,
    str2: Option<&str>,
    str3: Option<&str>,
) {
    unsafe {
        let des = xml_schema_format_item_for_report(None, item, item_elem);
        let msg = format!("{des}: {message}.\n");
        if item_elem.is_none() && !item.is_null() {
            item_elem = xml_schema_get_component_node(item as _).map(|node| node.into());
        }
        xml_schema_perr_ext(
            ctxt,
            item_elem,
            error as _,
            None,
            None,
            None,
            &msg,
            Some(&des),
            str1,
            str2,
            str3,
            None,
        );
    }
}

/// Reports an illegal attribute during the parse.
#[doc(alias = "xmlSchemaPCustomAttrErr")]
pub(crate) unsafe fn xml_schema_pcustom_attr_err(
    ctxt: XmlSchemaParserCtxtPtr,
    error: XmlParserErrors,
    owner_des: *mut *mut u8,
    owner_item: XmlSchemaBasicItemPtr,
    attr: Option<XmlAttrPtr>,
    msg: &str,
) {
    unsafe {
        let des = if owner_des.is_null() {
            xml_schema_format_item_for_report(
                None,
                owner_item,
                attr.unwrap().parent.map(|p| p.into()),
            )
        } else if (*owner_des).is_null() {
            let des = xml_schema_format_item_for_report(
                None,
                owner_item,
                attr.unwrap().parent.map(|p| p.into()),
            );
            *owner_des = xml_strndup(des.as_ptr(), des.len() as i32);
            des
        } else {
            CStr::from_ptr((*owner_des) as *const i8)
                .to_string_lossy()
                .into_owned()
        };
        if let Some(attr) = attr {
            let name = attr.name().unwrap();
            xml_schema_perr_ext(
                ctxt,
                Some(attr.into()),
                error,
                None,
                None,
                None,
                format!("{des}, attribute '{name}': {msg}.\n").as_str(),
                Some(&des),
                Some(&name),
                Some(msg),
                None,
                None,
            );
        } else {
            xml_schema_perr_ext(
                ctxt,
                None,
                error,
                None,
                None,
                None,
                format!("{des}, attribute 'Unknown': {msg}.\n").as_str(),
                Some(&des),
                Some("Unknown"),
                Some(msg),
                None,
                None,
            );
        }
    }
}

/// Reports an attribute use error during parsing.
///
/// # Note
/// This function does not format string.
#[allow(clippy::too_many_arguments)]
#[doc(alias = "xmlSchemaPAttrUseErr")]
pub(crate) unsafe fn xml_schema_pattr_use_err4(
    ctxt: XmlSchemaParserCtxtPtr,
    error: XmlParserErrors,
    node: Option<XmlGenericNodePtr>,
    owner_item: XmlSchemaBasicItemPtr,
    attruse: XmlSchemaAttributeUsePtr,
    message: &str,
    str1: Option<&str>,
    str2: Option<&str>,
    str3: Option<&str>,
    str4: Option<&str>,
) {
    use std::fmt::Write as _;

    unsafe {
        let mut msg = xml_schema_format_item_for_report(None, owner_item, None);
        writeln!(
            msg,
            ", {}: {message}.",
            xml_schema_format_item_for_report(None, attruse as XmlSchemaBasicItemPtr, None,)
        )
        .ok();

        xml_schema_err4(
            ctxt as XmlSchemaAbstractCtxtPtr,
            error as _,
            node,
            msg.as_str(),
            str1,
            str2,
            str3,
            str4,
        );
    }
}

/// Reports an error concerning the content of a schema element.
#[doc(alias = "xmlSchemaPContentErr")]
pub(crate) unsafe fn xml_schema_pcontent_err(
    ctxt: XmlSchemaParserCtxtPtr,
    error: XmlParserErrors,
    owner_item: XmlSchemaBasicItemPtr,
    owner_elem: XmlNodePtr,
    child: Option<XmlGenericNodePtr>,
    message: Option<&str>,
    content: Option<&str>,
) {
    unsafe {
        let des = xml_schema_format_item_for_report(None, owner_item, Some(owner_elem.into()));
        if let Some(message) = message {
            xml_schema_perr2(
                ctxt,
                Some(owner_elem.into()),
                child,
                error as _,
                format!("{des}: {message}.\n").as_str(),
                Some(&des),
                Some(message),
            );
        } else if let Some(content) = content {
            xml_schema_perr2(
                ctxt,
                Some(owner_elem.into()),
                child,
                error as _,
                format!("{des}: The content is not valid. Expected is {content}.\n").as_str(),
                Some(&des),
                Some(content),
            );
        } else {
            xml_schema_perr2(
                ctxt,
                Some(owner_elem.into()),
                child,
                error as _,
                format!("{des}: The content is not valid.\n").as_str(),
                Some(&des),
                None,
            );
        }
    }
}

/// Reports an illegal attribute.
#[doc(alias = "xmlSchemaPMutualExclAttrErr")]
pub(crate) unsafe fn xml_schema_pmutual_excl_attr_err(
    ctxt: XmlSchemaParserCtxtPtr,
    error: XmlParserErrors,
    owner_item: XmlSchemaBasicItemPtr,
    attr: XmlAttrPtr,
    name1: *const i8,
    name2: *const i8,
) {
    unsafe {
        let des = xml_schema_format_item_for_report(
            None,
            owner_item as XmlSchemaBasicItemPtr,
            attr.parent.map(|p| p.into()),
        );
        let name1 = CStr::from_ptr(name1).to_string_lossy();
        let name2 = CStr::from_ptr(name2).to_string_lossy();
        xml_schema_perr_ext(
            ctxt,
            Some(attr.into()),
            error,
            None,
            None,
            None,
            format!("{des}: The attributes '{name1}' and '{name2}' are mutually exclusive.\n")
                .as_str(),
            Some(&des),
            Some(&name1),
            Some(&name2),
            None,
            None,
        );
    }
}

/// Reports an illegal attribute.
#[doc(alias = "xmlSchemaPMissingAttrErr")]
pub(crate) unsafe fn xml_schema_pmissing_attr_err(
    ctxt: XmlSchemaParserCtxtPtr,
    error: XmlParserErrors,
    owner_item: XmlSchemaBasicItemPtr,
    owner_elem: Option<XmlGenericNodePtr>,
    name: Option<&str>,
    message: Option<&str>,
) {
    unsafe {
        let des = xml_schema_format_item_for_report(None, owner_item, owner_elem);
        if let Some(message) = message {
            xml_schema_perr(
                ctxt,
                owner_elem,
                error,
                format!("{des}: {message}.\n").as_str(),
                Some(&des),
                Some(message),
            );
        } else {
            xml_schema_perr(
                ctxt,
                owner_elem,
                error,
                format!(
                    "{des}: The attribute '{}' is required but missing.\n",
                    name.unwrap()
                )
                .as_str(),
                Some(&des),
                name,
            );
        }
    }
}

/// Used to report QName attribute values that failed to resolve to schema components.
#[doc(alias = "xmlSchemaPResCompAttrErr")]
#[allow(clippy::too_many_arguments)]
pub(crate) unsafe fn xml_schema_pres_comp_attr_err(
    ctxt: XmlSchemaParserCtxtPtr,
    error: XmlParserErrors,
    owner_item: XmlSchemaBasicItemPtr,
    owner_elem: Option<XmlGenericNodePtr>,
    name: &str,
    ref_name: Option<&str>,
    ref_uri: Option<&str>,
    ref_type: XmlSchemaTypeType,
    ref_type_str: Option<&str>,
) {
    unsafe {
        let des = xml_schema_format_item_for_report(None, owner_item, owner_elem);
        let ref_type_str = ref_type_str.unwrap_or_else(|| xml_schema_item_type_to_str(ref_type));
        let qname = xml_schema_format_qname(ref_uri, ref_name);
        xml_schema_perr_ext(
            ctxt,
            owner_elem,
            error as _,
            None,
            None,
            None,
            format!(
                "{des}, attribute '{name}': The QName value '{qname}' does not resolve to a(n) {ref_type_str}.\n"
            )
            .as_str(),
            Some(&des),
            Some(name),
            Some(&qname),
            Some(ref_type_str),
            None,
        );
    }
}

pub(crate) unsafe fn xml_schema_derive_facet_err(
    pctxt: XmlSchemaParserCtxtPtr,
    facet1: XmlSchemaFacetPtr,
    facet2: XmlSchemaFacetPtr,
    less_greater: i32,
    or_equal: i32,
    of_base: i32,
) {
    unsafe {
        use std::fmt::Write as _;

        let mut msg = format!(
            "'{}' has to be",
            xml_schema_facet_type_to_string((*facet1).typ)
        );
        if less_greater == 0 {
            msg.push_str(" equal to");
        }
        if less_greater == 1 {
            msg.push_str(" greater than");
        } else {
            msg.push_str(" less than");
        }

        if or_equal != 0 {
            msg.push_str(" or equal to");
        }
        write!(msg, " '{}", xml_schema_facet_type_to_string((*facet2).typ)).ok();
        if of_base != 0 {
            msg.push_str("' of the base type");
        } else {
            msg.push('\'');
        }

        xml_schema_pcustom_err(
            pctxt,
            XmlParserErrors::XmlSchemapInvalidFacetValue,
            facet1 as XmlSchemaBasicItemPtr,
            None,
            msg.as_str(),
            None,
        );
    }
}

/// Reports an illegal facet for atomic simple types.
#[doc(alias = "xmlSchemaPIllegalFacetAtomicErr")]
pub(crate) unsafe fn xml_schema_pillegal_facet_atomic_err(
    ctxt: XmlSchemaParserCtxtPtr,
    error: XmlParserErrors,
    typ: XmlSchemaTypePtr,
    base_type: XmlSchemaTypePtr,
    facet: XmlSchemaFacetPtr,
) {
    unsafe {
        let des = xml_schema_format_item_for_report(
            None,
            typ as XmlSchemaBasicItemPtr,
            (*typ).node.map(|node| node.into()),
        );
        let facet_type = xml_schema_facet_type_to_string((*facet).typ);
        let item =
            xml_schema_format_item_for_report(None, base_type as XmlSchemaBasicItemPtr, None);
        xml_schema_perr_ext(
            ctxt,
            (*typ).node.map(|node| node.into()),
            error,
            None,
            None,
            None,
            format!(
                "{des}: The facet '{facet_type}' is not allowed on types derived from the type {item}.\n"
            )
            .as_str(),
            Some(&des),
            Some(facet_type),
            Some(&item),
            None,
            None,
        );
    }
}

/// Reports an illegal facet for <list> and <union>.
#[doc(alias = "xmlSchemaPIllegalFacetListUnionErr")]
pub(crate) unsafe fn xml_schema_pillegal_facet_list_union_err(
    ctxt: XmlSchemaParserCtxtPtr,
    error: XmlParserErrors,
    typ: XmlSchemaTypePtr,
    facet: XmlSchemaFacetPtr,
) {
    unsafe {
        let des = xml_schema_format_item_for_report(
            None,
            typ as XmlSchemaBasicItemPtr,
            (*typ).node.map(|node| node.into()),
        );
        let facet_type = xml_schema_facet_type_to_string((*facet).typ);
        xml_schema_perr(
            ctxt,
            (*typ).node.map(|node| node.into()),
            error,
            format!("{des}: The facet '{facet_type}' is not allowed.\n").as_str(),
            Some(&des),
            Some(facet_type),
        );
    }
}

/// # Note
/// This function does not format string.
pub(crate) unsafe fn xml_schema_keyref_err(
    vctxt: XmlSchemaValidCtxtPtr,
    error: XmlParserErrors,
    idc_node: XmlSchemaPSVIIDCNodePtr,
    _typ: XmlSchemaTypePtr,
    message: &str,
    str1: Option<&str>,
    str2: Option<&str>,
) {
    unsafe {
        let namespace_name =
            (*(*vctxt).node_qnames).items[(*idc_node).node_qname_id as usize + 1] as *const i8;
        let qn = xml_schema_format_qname(
            (!namespace_name.is_null())
                .then(|| CStr::from_ptr(namespace_name).to_string_lossy())
                .as_deref(),
            Some(
                CStr::from_ptr(
                    (*(*vctxt).node_qnames).items[(*idc_node).node_qname_id as usize] as *const i8,
                )
                .to_string_lossy()
                .as_ref(),
            ),
        );
        let msg = format!("Element '{qn}': {message}.\n");
        xml_schema_err4_line(
            vctxt as XmlSchemaAbstractCtxtPtr,
            XmlErrorLevel::XmlErrError,
            error as _,
            None,
            (*idc_node).node_line,
            msg.as_str(),
            Some(&qn),
            str1,
            str2,
            None,
        );
    }
}

/// Handle an out of memory condition
#[doc(alias = "xmlSchemaPErrMemory")]
pub(crate) unsafe fn xml_schema_perr_memory(
    ctxt: XmlSchemaParserCtxtPtr,
    extra: &str,
    node: Option<XmlGenericNodePtr>,
) {
    unsafe {
        if !ctxt.is_null() {
            (*ctxt).nberrors += 1;
        }
        __xml_simple_oom_error(XmlErrorDomain::XmlFromSchemasp, node, Some(extra));
    }
}

/// Handle an out of memory condition
#[doc(alias = "xmlSchemaVTypeErrMemory")]
pub(crate) unsafe fn xml_schema_verr_memory(
    ctxt: XmlSchemaValidCtxtPtr,
    extra: &str,
    node: Option<XmlGenericNodePtr>,
) {
    unsafe {
        if !ctxt.is_null() {
            (*ctxt).nberrors += 1;
            (*ctxt).err = XmlParserErrors::XmlSchemavInternal as i32;
        }
        __xml_simple_oom_error(XmlErrorDomain::XmlFromSchemasv, node, Some(extra));
    }
}

pub(super) fn xml_schema_psimple_err(msg: &str) {
    __xml_simple_oom_error(XmlErrorDomain::XmlFromSchemasp, None, Some(msg));
}

pub(crate) fn xml_schema_psimple_internal_err(node: Option<XmlGenericNodePtr>, msg: &str) {
    __xml_simple_error!(
        XmlErrorDomain::XmlFromSchemasp,
        XmlParserErrors::XmlSchemapInternal,
        node,
        msg
    );
}
