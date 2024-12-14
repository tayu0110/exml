//! Provide methods and data structures for debug XML documents.  
//! This module is based on `libxml/debugXML.h`, `debugXML.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: Tree debugging APIs
// Description: Interfaces to a set of routines used for debugging the tree
//              produced by the XML parser.
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// debugXML.c : This is a set of routines used for debugging the tree produced by the XML parser.
//
// See Copyright for the status of this software.
//
// Daniel Veillard <daniel@veillard.com>

use std::{
    ffi::{c_char, CStr, CString},
    io::{stdout, Write},
    mem::zeroed,
    ptr::{addr_of_mut, null_mut},
    str::from_utf8_unchecked,
    sync::atomic::Ordering,
};

use libc::strlen;

use crate::{
    error::{XmlParserErrors, __xml_raise_error},
    generic_error,
    libxml::{chvalid::xml_is_blank_char, entities::XmlEntityPtr},
    tree::{
        xml_free_node_list, xml_validate_name, NodeCommon, NodePtr, XmlAttrPtr,
        XmlAttributeDefault, XmlAttributePtr, XmlAttributeType, XmlDocPtr, XmlDtdPtr,
        XmlElementPtr, XmlElementType, XmlElementTypeVal, XmlEnumerationPtr, XmlNode, XmlNodePtr,
        XmlNsPtr,
    },
};

#[cfg(feature = "xpath")]
use super::xpath::{XmlXPathContextPtr, XmlXPathObjectPtr};
use super::{
    dict::{xml_dict_lookup, xml_dict_owns, XmlDictPtr},
    entities::{xml_get_doc_entity, XmlEntityType},
    parser::{xml_parse_in_node_context, XmlParserOption},
    parser_internals::{XML_STRING_COMMENT, XML_STRING_TEXT, XML_STRING_TEXT_NOENC},
    valid::xml_snprintf_element_content,
    xmlstring::{xml_check_utf8, xml_str_equal, xml_strchr, xml_strlen, xml_strstr, XmlChar},
    xpath::XmlXPathObjectType,
};

pub type XmlDebugCtxtPtr<'a> = *mut XmlDebugCtxt<'a>;
#[repr(C)]
pub struct XmlDebugCtxt<'a> {
    output: Box<dyn Write + 'a>, /* the output file */
    shift: [u8; 100],            /* used for indenting */
    depth: i32,                  /* current depth */
    doc: XmlDocPtr,              /* current document */
    node: XmlNodePtr,            /* current node */
    dict: XmlDictPtr,            /* the doc dictionary */
    check: i32,                  /* do just checkings */
    errors: i32,                 /* number of errors found */
    nodict: i32,                 /* if the document has no dictionary */
    options: i32,                /* options */
}

impl Default for XmlDebugCtxt<'_> {
    fn default() -> Self {
        Self {
            depth: 0,
            check: 0,
            errors: 0,
            output: Box::new(stdout()),
            doc: null_mut(),
            node: null_mut(),
            dict: null_mut(),
            nodict: 0,
            options: 0,
            shift: [b' '; 100],
        }
    }
}

/// Dumps information about the string, shorten it if necessary
#[doc(alias = "xmlDebugDumpString")]
pub unsafe extern "C" fn xml_debug_dump_string<'a>(
    mut output: Option<&mut (impl Write + 'a)>,
    str: *const XmlChar,
) {
    let mut stdout = stdout();
    let output = output
        .as_mut()
        .map(|o| o as &mut dyn Write)
        .unwrap_or(&mut stdout as &mut dyn Write);
    if str.is_null() {
        write!(output, "(NULL)");
        return;
    }
    for i in 0..40 {
        if *str.add(i) == 0 {
            return;
        } else if xml_is_blank_char(*str.add(i) as u32) {
            write!(output, " ");
        } else if *str.add(i) >= 0x80 {
            write!(output, "#{:X}", *str.add(i) as i32);
        } else {
            write!(output, "{}", *str.add(i) as char);
        }
    }
    write!(output, "...");
}

#[doc(alias = "xmlCtxtDumpInitCtxt")]
unsafe extern "C" fn xml_ctxt_dump_init_ctxt(ctxt: XmlDebugCtxtPtr) {
    (*ctxt).depth = 0;
    (*ctxt).check = 0;
    (*ctxt).errors = 0;
    (*ctxt).output = Box::new(stdout());
    (*ctxt).doc = null_mut();
    (*ctxt).node = null_mut();
    (*ctxt).dict = null_mut();
    (*ctxt).nodict = 0;
    (*ctxt).options = 0;
    for i in 0..100 {
        (*ctxt).shift[i] = b' ' as _;
    }
}

#[doc(alias = "xmlCtxtDumpSpaces")]
unsafe extern "C" fn xml_ctxt_dump_spaces(ctxt: XmlDebugCtxtPtr) {
    if (*ctxt).check != 0 {
        return;
    }
    if (*ctxt).depth > 0 {
        if (*ctxt).depth < 50 {
            let spaces = from_utf8_unchecked(&(*ctxt).shift[100 - 2 * (*ctxt).depth as usize..]);
            write!((*ctxt).output, "{spaces}",);
        } else {
            write!((*ctxt).output, "{}", from_utf8_unchecked(&(*ctxt).shift));
        }
    }
}

#[doc(alias = "xmlCtxtDumpString")]
unsafe extern "C" fn xml_ctxt_dump_string(ctxt: XmlDebugCtxtPtr, str: *const XmlChar) {
    if (*ctxt).check != 0 {
        return;
    }
    /* TODO: check UTF8 content of the string */
    if str.is_null() {
        write!((*ctxt).output, "(NULL)");
        return;
    }

    for i in 0..40 {
        if *str.add(i) == 0 {
            return;
        } else if xml_is_blank_char(*str.add(i) as u32) {
            write!((*ctxt).output, " ");
        } else if *str.add(i) >= 0x80 {
            write!((*ctxt).output, "#{:0X}", *str.add(i) as i32);
        } else {
            write!((*ctxt).output, "{}", *str.add(i) as char);
        }
    }
    write!((*ctxt).output, "...");
}

const DUMP_TEXT_TYPE: i32 = 1;

/// Handle a debug error.
#[doc(alias = "xmlDebugErr", alias = "xmlDebugErr2", alias = "xmlDebugErr3")]
macro_rules! xml_debug_err {
    ($ctxt:expr, $error:expr, $msg:literal, $( $args:expr ),*) => {
        (*$ctxt).errors += 1;
        let msg = format!($msg, $( $args ),*);
        __xml_raise_error!(
            None,
            None,
            None,
            null_mut(),
            (*$ctxt).node as _,
            XmlErrorDomain::XmlFromCheck,
            $error,
            XmlErrorLevel::XmlErrError,
            None,
            0,
            None,
            None,
            None,
            0,
            0,
            msg.as_str(),
        );
    };
}

/// Check that a given namespace is in scope on a node.
///
/// Returns 1 if in scope, -1 in case of argument error,
/// -2 if the namespace is not in scope,
/// and -3 if not on an ancestor node.
#[doc(alias = "xmlNsCheckScope")]
unsafe extern "C" fn xml_ns_check_scope(mut node: XmlNodePtr, ns: XmlNsPtr) -> i32 {
    let mut cur: XmlNsPtr;

    if node.is_null() || ns.is_null() {
        return -1;
    }

    if !matches!(
        (*node).element_type(),
        XmlElementType::XmlElementNode
            | XmlElementType::XmlAttributeNode
            | XmlElementType::XmlDocumentNode
            | XmlElementType::XmlTextNode
            | XmlElementType::XmlHTMLDocumentNode
            | XmlElementType::XmlXIncludeStart
    ) {
        return -2;
    }

    while !node.is_null()
        && matches!(
            (*node).element_type(),
            XmlElementType::XmlElementNode
                | XmlElementType::XmlAttributeNode
                | XmlElementType::XmlTextNode
                | XmlElementType::XmlXIncludeStart
        )
    {
        if matches!(
            (*node).element_type(),
            XmlElementType::XmlElementNode | XmlElementType::XmlXIncludeStart
        ) {
            cur = (*node).ns_def;
            while !cur.is_null() {
                if cur == ns {
                    return 1;
                }
                if xml_str_equal((*cur).prefix, (*ns).prefix) {
                    return -2;
                }
                cur = (*cur).next;
            }
        }
        node = (*node).parent().map_or(null_mut(), |p| p.as_ptr());
    }
    /* the xml namespace may be declared on the document node */
    if !node.is_null()
        && matches!(
            (*node).element_type(),
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode
        )
    {
        let old_ns: XmlNsPtr = (*node).as_document_node().unwrap().as_ref().old_ns;
        if old_ns == ns {
            return 1;
        }
    }
    -3
}

/// Report if a given namespace is is not in scope.
#[doc(alias = "xmlCtxtNsCheckScope")]
unsafe extern "C" fn xml_ctxt_ns_check_scope(
    ctxt: XmlDebugCtxtPtr,
    node: XmlNodePtr,
    ns: XmlNsPtr,
) {
    let ret: i32 = xml_ns_check_scope(node, ns);
    if ret == -2 {
        if (*ns).prefix.is_null() {
            xml_debug_err!(
                ctxt,
                XmlParserErrors::XmlCheckNsScope,
                "Reference to default namespace not in scope\n",
            );
        } else {
            xml_debug_err!(
                ctxt,
                XmlParserErrors::XmlCheckNsScope,
                "Reference to namespace '{}' not in scope\n",
                CStr::from_ptr((*ns).prefix as *const i8).to_string_lossy()
            );
        }
    }
    if ret == -3 {
        if (*ns).prefix.is_null() {
            xml_debug_err!(
                ctxt,
                XmlParserErrors::XmlCheckNsAncestor,
                "Reference to default namespace not on ancestor\n",
            );
        } else {
            xml_debug_err!(
                ctxt,
                XmlParserErrors::XmlCheckNsAncestor,
                "Reference to namespace '{}' not on ancestor\n",
                CStr::from_ptr((*ns).prefix as *const i8).to_string_lossy()
            );
        }
    }
}

/// Do debugging on the string, currently it just checks the UTF-8 content
#[doc(alias = "xmlCtxtCheckString")]
unsafe extern "C" fn xml_ctxt_check_string(ctxt: XmlDebugCtxtPtr, str: *const XmlChar) {
    if str.is_null() {
        return;
    }
    if (*ctxt).check != 0 && xml_check_utf8(str) == 0 {
        xml_debug_err!(
            ctxt,
            XmlParserErrors::XmlCheckNotUTF8,
            "String is not UTF-8 {}",
            CStr::from_ptr(str as *const i8).to_string_lossy()
        );
    }
}

/// Do debugging on the name, for example the dictionary status and
/// conformance to the Name production.
#[doc(alias = "xmlCtxtCheckName")]
unsafe extern "C" fn xml_ctxt_check_name(ctxt: XmlDebugCtxtPtr, name: *const XmlChar) {
    if (*ctxt).check != 0 {
        if name.is_null() {
            xml_debug_err!(ctxt, XmlParserErrors::XmlCheckNoName, "Name is NULL",);
            return;
        }
        #[cfg(any(feature = "libxml_tree", feature = "schema"))]
        if xml_validate_name(name, 0) != 0 {
            xml_debug_err!(
                ctxt,
                XmlParserErrors::XmlCheckNotNCName,
                "Name is not an NCName '{}'",
                CStr::from_ptr(name as *const i8).to_string_lossy()
            );
        }
        if !(*ctxt).dict.is_null()
            && xml_dict_owns((*ctxt).dict, name) == 0
            && ((*ctxt).doc.is_null()
                || (*(*ctxt).doc).parse_flags
                    & (XmlParserOption::XmlParseSax1 as i32
                        | XmlParserOption::XmlParseNodict as i32)
                    == 0)
        {
            xml_debug_err!(
                ctxt,
                XmlParserErrors::XmlCheckOutsideDict,
                "Name is not from the document dictionary '{}'",
                CStr::from_ptr(name as *const i8).to_string_lossy()
            );
        }
    }
}

#[doc(alias = "xmlCtxtGenericNodeCheck")]
unsafe extern "C" fn xml_ctxt_generic_node_check(ctxt: XmlDebugCtxtPtr, node: XmlNodePtr) {
    let dict: XmlDictPtr;
    let doc: XmlDocPtr = (*node).doc;

    if (*node).parent().is_none() {
        xml_debug_err!(
            ctxt,
            XmlParserErrors::XmlCheckNoParent,
            "Node has no parent\n",
        );
    }
    if (*node).doc.is_null() {
        xml_debug_err!(ctxt, XmlParserErrors::XmlCheckNoDoc, "Node has no doc\n",);
        // dict = null_mut();
    } else {
        dict = (*doc).dict;
        if dict.is_null() && (*ctxt).nodict == 0 {
            (*ctxt).nodict = 1;
        }
        if (*ctxt).doc.is_null() {
            (*ctxt).doc = doc;
        }

        if (*ctxt).dict.is_null() {
            (*ctxt).dict = dict;
        }
    }
    if (*node)
        .parent()
        .filter(|p| {
            (*node).doc != p.doc && !xml_str_equal((*node).name, c"pseudoroot".as_ptr() as _)
        })
        .is_some()
    {
        xml_debug_err!(
            ctxt,
            XmlParserErrors::XmlCheckWrongDoc,
            "Node doc differs from parent's one\n",
        );
    }
    if (*node).prev.is_none() {
        if (*node).element_type() == XmlElementType::XmlAttributeNode {
            if (*node)
                .parent()
                .filter(|p| node != p.properties as *mut XmlNode)
                .is_some()
            {
                xml_debug_err!(
                    ctxt,
                    XmlParserErrors::XmlCheckNoPrev,
                    "Attr has no prev and not first of attr list\n",
                );
            }
        } else if (*node)
            .parent()
            .filter(|p| p.children() != NodePtr::from_ptr(node))
            .is_some()
        {
            xml_debug_err!(
                ctxt,
                XmlParserErrors::XmlCheckNoPrev,
                "Node has no prev and not first of parent list\n",
            );
        }
    } else if (*node).prev.unwrap().next != NodePtr::from_ptr(node) {
        xml_debug_err!(
            ctxt,
            XmlParserErrors::XmlCheckWrongPrev,
            "Node prev->next : back link wrong\n",
        );
    }
    if let Some(next) = (*node).next {
        if next.prev != NodePtr::from_ptr(node) {
            xml_debug_err!(
                ctxt,
                XmlParserErrors::XmlCheckWrongNext,
                "Node next->prev : forward link wrong\n",
            );
        }
        if next.parent() != (*node).parent() {
            xml_debug_err!(
                ctxt,
                XmlParserErrors::XmlCheckWrongParent,
                "Node next->prev : forward link wrong\n",
            );
        }
    } else if (*node)
        .parent()
        .filter(|p| {
            (*node).element_type() != XmlElementType::XmlAttributeNode
                && p.last() != NodePtr::from_ptr(node)
                && p.element_type() == XmlElementType::XmlElementNode
        })
        .is_some()
    {
        xml_debug_err!(
            ctxt,
            XmlParserErrors::XmlCheckNoNext,
            "Node has no next and not last of parent list\n",
        );
    }
    if (*node).element_type() == XmlElementType::XmlElementNode {
        let mut ns: XmlNsPtr;

        ns = (*node).ns_def;
        while !ns.is_null() {
            xml_ctxt_ns_check_scope(ctxt, node, ns);
            ns = (*ns).next;
        }
        if !(*node).ns.is_null() {
            xml_ctxt_ns_check_scope(ctxt, node, (*node).ns);
        }
    } else if (*node).element_type() == XmlElementType::XmlAttributeNode && !(*node).ns.is_null() {
        xml_ctxt_ns_check_scope(ctxt, node, (*node).ns);
    }

    if !matches!(
        (*node).element_type(),
        XmlElementType::XmlElementNode
            | XmlElementType::XmlAttributeNode
            | XmlElementType::XmlElementDecl
            | XmlElementType::XmlAttributeDecl
            | XmlElementType::XmlDTDNode
            | XmlElementType::XmlHTMLDocumentNode
            | XmlElementType::XmlDocumentNode
    ) && !(*node).content.is_null()
    {
        xml_ctxt_check_string(ctxt, (*node).content as *const XmlChar);
    }
    match (*node).element_type() {
        XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode => {
            xml_ctxt_check_name(ctxt, (*node).name);
        }
        XmlElementType::XmlTextNode => {
            if (*node).name == XML_STRING_TEXT.as_ptr() as _
                || (*node).name == XML_STRING_TEXT_NOENC.as_ptr() as _
            {
                // break;
            } else {
                /* some case of entity substitution can lead to this */
                if !(*ctxt).dict.is_null()
                    && ((*node).name == xml_dict_lookup((*ctxt).dict, c"nbktext".as_ptr() as _, 7))
                {
                    // break;
                } else {
                    xml_debug_err!(
                        ctxt,
                        XmlParserErrors::XmlCheckWrongName,
                        "Text node has wrong name '{}'",
                        (*node).name().unwrap()
                    );
                }
            }
        }
        XmlElementType::XmlCommentNode => {
            if (*node).name == XML_STRING_COMMENT.as_ptr() as _ {
                // break;
            } else {
                xml_debug_err!(
                    ctxt,
                    XmlParserErrors::XmlCheckWrongName,
                    "Comment node has wrong name '{}'",
                    (*node).name().unwrap()
                );
            }
        }
        XmlElementType::XmlPINode => {
            xml_ctxt_check_name(ctxt, (*node).name);
        }
        XmlElementType::XmlCDATASectionNode => {
            if (*node).name.is_null() {
                // break;
            } else {
                xml_debug_err!(
                    ctxt,
                    XmlParserErrors::XmlCheckNameNotNull,
                    "CData section has non NULL name '{}'",
                    (*node).name().unwrap()
                );
            }
        }
        XmlElementType::XmlEntityRefNode
        | XmlElementType::XmlEntityNode
        | XmlElementType::XmlDocumentTypeNode
        | XmlElementType::XmlDocumentFragNode
        | XmlElementType::XmlNotationNode
        | XmlElementType::XmlDTDNode
        | XmlElementType::XmlElementDecl
        | XmlElementType::XmlAttributeDecl
        | XmlElementType::XmlEntityDecl
        | XmlElementType::XmlNamespaceDecl
        | XmlElementType::XmlXIncludeStart
        | XmlElementType::XmlXIncludeEnd
        | XmlElementType::XmlDocumentNode
        | XmlElementType::XmlHTMLDocumentNode => {}
        _ => unreachable!(),
    }
}

#[doc(alias = "xmlCtxtDumpDtdNode")]
unsafe extern "C" fn xml_ctxt_dump_dtd_node(ctxt: XmlDebugCtxtPtr, dtd: XmlDtdPtr) {
    xml_ctxt_dump_spaces(ctxt);

    if dtd.is_null() {
        if (*ctxt).check == 0 {
            writeln!((*ctxt).output, "DTD node is NULL");
        }
        return;
    }

    if (*dtd).typ != XmlElementType::XmlDTDNode {
        xml_debug_err!(ctxt, XmlParserErrors::XmlCheckNotDTD, "Node is not a DTD",);
        return;
    }
    if (*ctxt).check == 0 {
        if !(*dtd).name.is_null() {
            let name = CStr::from_ptr((*dtd).name as *const i8).to_string_lossy();
            write!((*ctxt).output, "DTD({})", name);
        } else {
            write!((*ctxt).output, "DTD");
        }
        if let Some(external_id) = (*dtd).external_id.as_deref() {
            write!((*ctxt).output, ", PUBLIC {external_id}");
        }
        if let Some(system_id) = (*dtd).system_id.as_deref() {
            write!((*ctxt).output, ", SYSTEM {system_id}");
        }
        writeln!((*ctxt).output);
    }
    /*
     * Do a bit of checking
     */
    xml_ctxt_generic_node_check(ctxt, dtd as XmlNodePtr);
}

/// Dumps debug information for the DTD
#[doc(alias = "xmlCtxtDumpDTD")]
unsafe extern "C" fn xml_ctxt_dump_dtd(ctxt: XmlDebugCtxtPtr, dtd: XmlDtdPtr) {
    if dtd.is_null() {
        if (*ctxt).check == 0 {
            writeln!((*ctxt).output, "DTD is NULL");
        }
        return;
    }
    xml_ctxt_dump_dtd_node(ctxt, dtd);
    if let Some(children) = (*dtd).children {
        (*ctxt).depth += 1;
        xml_ctxt_dump_node_list(ctxt, children.as_ptr());
        (*ctxt).depth -= 1;
    } else {
        writeln!((*ctxt).output, "    DTD is empty");
    }
}

#[doc(alias = "xmlCtxtDumpElemDecl")]
unsafe extern "C" fn xml_ctxt_dump_elem_decl(ctxt: XmlDebugCtxtPtr, elem: XmlElementPtr) {
    xml_ctxt_dump_spaces(ctxt);

    if elem.is_null() {
        if (*ctxt).check == 0 {
            writeln!((*ctxt).output, "Element declaration is NULL");
        }
        return;
    }
    if (*elem).typ != XmlElementType::XmlElementDecl {
        xml_debug_err!(
            ctxt,
            XmlParserErrors::XmlCheckNotElemDecl,
            "Node is not an element declaration",
        );
        return;
    }
    if let Some(name) = (*elem).name.as_deref() {
        if (*ctxt).check == 0 {
            let name = CString::new(name.as_str()).unwrap();
            write!((*ctxt).output, "ELEMDECL(");
            xml_ctxt_dump_string(ctxt, name.as_ptr() as *const u8);
            write!((*ctxt).output, ")");
        }
    } else {
        xml_debug_err!(
            ctxt,
            XmlParserErrors::XmlCheckNoName,
            "Element declaration has no name",
        );
    }
    if (*ctxt).check == 0 {
        match (*elem).etype {
            XmlElementTypeVal::XmlElementTypeUndefined => {
                write!((*ctxt).output, ", UNDEFINED");
            }
            XmlElementTypeVal::XmlElementTypeEmpty => {
                write!((*ctxt).output, ", EMPTY");
            }
            XmlElementTypeVal::XmlElementTypeAny => {
                write!((*ctxt).output, ", ANY");
            }
            XmlElementTypeVal::XmlElementTypeMixed => {
                write!((*ctxt).output, ", MIXED ");
            }
            XmlElementTypeVal::XmlElementTypeElement => {
                write!((*ctxt).output, ", MIXED ");
            }
        }
        if (*elem).typ != XmlElementType::XmlElementNode && !(*elem).content.is_null() {
            let mut buf: [c_char; 5001] = [0; 5001];

            buf[0] = 0;
            xml_snprintf_element_content(buf.as_mut_ptr(), 5000, (*elem).content, 1);
            buf[5000] = 0;
            let elem = CStr::from_ptr(buf.as_ptr()).to_string_lossy();
            write!((*ctxt).output, "{}", elem);
        }
        writeln!((*ctxt).output);
    }

    // Do a bit of checking
    xml_ctxt_generic_node_check(ctxt, elem as XmlNodePtr);
}

#[doc(alias = "xmlCtxtDumpAttrDecl")]
unsafe extern "C" fn xml_ctxt_dump_attr_decl(ctxt: XmlDebugCtxtPtr, attr: XmlAttributePtr) {
    xml_ctxt_dump_spaces(ctxt);

    if attr.is_null() {
        if (*ctxt).check == 0 {
            writeln!((*ctxt).output, "Attribute declaration is NULL");
        }
        return;
    }
    if (*attr).typ != XmlElementType::XmlAttributeDecl {
        xml_debug_err!(
            ctxt,
            XmlParserErrors::XmlCheckNotAttrDecl,
            "Node is not an attribute declaration",
        );
        return;
    }
    if !(*attr).name.is_null() {
        if (*ctxt).check == 0 {
            let name = CStr::from_ptr((*attr).name as *const i8).to_string_lossy();
            write!((*ctxt).output, "ATTRDECL({name})");
        }
    } else {
        xml_debug_err!(
            ctxt,
            XmlParserErrors::XmlCheckNoName,
            "Node attribute declaration has no name",
        );
    }
    if let Some(elem) = (*attr).elem.as_deref() {
        if (*ctxt).check == 0 {
            write!((*ctxt).output, " for {elem}");
        }
    } else {
        xml_debug_err!(
            ctxt,
            XmlParserErrors::XmlCheckNoElem,
            "Node attribute declaration has no element name",
        );
    }
    if (*ctxt).check == 0 {
        match (*attr).atype {
            XmlAttributeType::XmlAttributeCDATA => {
                write!((*ctxt).output, " CDATA");
            }
            XmlAttributeType::XmlAttributeID => {
                write!((*ctxt).output, " ID");
            }
            XmlAttributeType::XmlAttributeIDREF => {
                write!((*ctxt).output, " IDREF");
            }
            XmlAttributeType::XmlAttributeIDREFS => {
                write!((*ctxt).output, " IDREFS");
            }
            XmlAttributeType::XmlAttributeEntity => {
                write!((*ctxt).output, " ENTITY");
            }
            XmlAttributeType::XmlAttributeEntities => {
                write!((*ctxt).output, " ENTITIES");
            }
            XmlAttributeType::XmlAttributeNmtoken => {
                write!((*ctxt).output, " NMTOKEN");
            }
            XmlAttributeType::XmlAttributeNmtokens => {
                write!((*ctxt).output, " NMTOKENS");
            }
            XmlAttributeType::XmlAttributeEnumeration => {
                write!((*ctxt).output, " ENUMERATION");
            }
            XmlAttributeType::XmlAttributeNotation => {
                write!((*ctxt).output, " NOTATION ");
            }
        }
        if !(*attr).tree.is_null() {
            let mut cur: XmlEnumerationPtr = (*attr).tree;

            for indx in 0..5 {
                let name = (*cur).name.as_deref().unwrap();
                if indx != 0 {
                    write!((*ctxt).output, "|{name}");
                } else {
                    write!((*ctxt).output, " ({name}");
                }
                cur = (*cur).next;
                if cur.is_null() {
                    break;
                }
            }
            if cur.is_null() {
                write!((*ctxt).output, ")");
            } else {
                write!((*ctxt).output, "...)");
            }
        }
        match (*attr).def {
            XmlAttributeDefault::XmlAttributeNone => {}
            XmlAttributeDefault::XmlAttributeRequired => {
                write!((*ctxt).output, " REQUIRED");
            }
            XmlAttributeDefault::XmlAttributeImplied => {
                write!((*ctxt).output, " IMPLIED");
            }
            XmlAttributeDefault::XmlAttributeFixed => {
                write!((*ctxt).output, " FIXED");
            }
        }
        if !(*attr).default_value.is_null() {
            write!((*ctxt).output, "\"");
            xml_ctxt_dump_string(ctxt, (*attr).default_value);
            write!((*ctxt).output, "\"");
        }
        writeln!((*ctxt).output);
    }

    /*
     * Do a bit of checking
     */
    xml_ctxt_generic_node_check(ctxt, attr as XmlNodePtr);
}

#[doc(alias = "xmlCtxtDumpEntityDecl")]
unsafe extern "C" fn xml_ctxt_dump_entity_decl(ctxt: XmlDebugCtxtPtr, ent: XmlEntityPtr) {
    xml_ctxt_dump_spaces(ctxt);

    if ent.is_null() {
        if (*ctxt).check == 0 {
            writeln!((*ctxt).output, "Entity declaration is NULL");
        }
        return;
    }
    if (*ent).typ != XmlElementType::XmlEntityDecl {
        xml_debug_err!(
            ctxt,
            XmlParserErrors::XmlCheckNotEntityDecl,
            "Node is not an entity declaration",
        );
        return;
    }
    if !(*ent).name.load(Ordering::Relaxed).is_null() {
        if (*ctxt).check == 0 {
            write!((*ctxt).output, "ENTITYDECL(");
            xml_ctxt_dump_string(ctxt, (*ent).name.load(Ordering::Relaxed));
            write!((*ctxt).output, ")");
        }
    } else {
        xml_debug_err!(
            ctxt,
            XmlParserErrors::XmlCheckNoName,
            "Entity declaration has no name",
        );
    }
    if (*ctxt).check == 0 {
        match (*ent).etype {
            Some(XmlEntityType::XmlInternalGeneralEntity) => {
                writeln!((*ctxt).output, ", internal");
            }
            Some(XmlEntityType::XmlExternalGeneralParsedEntity) => {
                writeln!((*ctxt).output, ", external parsed");
            }
            Some(XmlEntityType::XmlExternalGeneralUnparsedEntity) => {
                writeln!((*ctxt).output, ", unparsed");
            }
            Some(XmlEntityType::XmlInternalParameterEntity) => {
                writeln!((*ctxt).output, ", parameter");
            }
            Some(XmlEntityType::XmlExternalParameterEntity) => {
                writeln!((*ctxt).output, ", external parameter");
            }
            Some(XmlEntityType::XmlInternalPredefinedEntity) => {
                writeln!((*ctxt).output, ", predefined");
            }
            _ => unreachable!(),
        }
        if !(*ent).external_id.load(Ordering::Relaxed).is_null() {
            xml_ctxt_dump_spaces(ctxt);
            let external_id =
                CStr::from_ptr((*ent).external_id.load(Ordering::Relaxed) as *const i8)
                    .to_string_lossy();
            writeln!((*ctxt).output, " ExternalID={external_id}");
        }
        if !(*ent).system_id.load(Ordering::Relaxed).is_null() {
            xml_ctxt_dump_spaces(ctxt);
            let system_id = CStr::from_ptr((*ent).system_id.load(Ordering::Relaxed) as *const i8)
                .to_string_lossy();
            writeln!((*ctxt).output, " SystemID={system_id}");
        }
        if !(*ent).uri.load(Ordering::Relaxed).is_null() {
            xml_ctxt_dump_spaces(ctxt);
            let uri =
                CStr::from_ptr((*ent).uri.load(Ordering::Relaxed) as *const i8).to_string_lossy();
            writeln!((*ctxt).output, " URI={uri}");
        }
        if !(*ent).content.load(Ordering::Relaxed).is_null() {
            xml_ctxt_dump_spaces(ctxt);
            write!((*ctxt).output, " content=");
            xml_ctxt_dump_string(ctxt, (*ent).content.load(Ordering::Relaxed));
            writeln!((*ctxt).output);
        }
    }

    /*
     * Do a bit of checking
     */
    xml_ctxt_generic_node_check(ctxt, ent as XmlNodePtr);
}

#[doc(alias = "xmlCtxtDumpNamespace")]
unsafe extern "C" fn xml_ctxt_dump_namespace(ctxt: XmlDebugCtxtPtr, ns: XmlNsPtr) {
    xml_ctxt_dump_spaces(ctxt);

    if ns.is_null() {
        if (*ctxt).check == 0 {
            writeln!((*ctxt).output, "namespace node is NULL");
        }
        return;
    }
    if (*ns).typ != XmlElementType::XmlNamespaceDecl {
        xml_debug_err!(
            ctxt,
            XmlParserErrors::XmlCheckNotNsDecl,
            "Node is not a namespace declaration",
        );
        return;
    }
    if (*ns).href.is_null() {
        if !(*ns).prefix.is_null() {
            xml_debug_err!(
                ctxt,
                XmlParserErrors::XmlCheckNoHref,
                "Incomplete namespace {} href=NULL\n",
                CStr::from_ptr((*ns).prefix as *const i8).to_string_lossy()
            );
        } else {
            xml_debug_err!(
                ctxt,
                XmlParserErrors::XmlCheckNoHref,
                "Incomplete default namespace href=NULL\n",
            );
        }
    } else if (*ctxt).check == 0 {
        if !(*ns).prefix.is_null() {
            let prefix = CStr::from_ptr((*ns).prefix as *const i8).to_string_lossy();
            write!((*ctxt).output, "namespace {prefix} href=");
        } else {
            write!((*ctxt).output, "default namespace href=");
        }

        xml_ctxt_dump_string(ctxt, (*ns).href);
        writeln!((*ctxt).output);
    }
}

#[doc(alias = "xmlCtxtDumpNamespaceList")]
unsafe extern "C" fn xml_ctxt_dump_namespace_list(ctxt: XmlDebugCtxtPtr, mut ns: XmlNsPtr) {
    while !ns.is_null() {
        xml_ctxt_dump_namespace(ctxt, ns);
        ns = (*ns).next;
    }
}

#[doc(alias = "xmlCtxtDumpEntity")]
unsafe extern "C" fn xml_ctxt_dump_entity(ctxt: XmlDebugCtxtPtr, ent: XmlEntityPtr) {
    xml_ctxt_dump_spaces(ctxt);

    if ent.is_null() {
        if (*ctxt).check == 0 {
            writeln!((*ctxt).output, "Entity is NULL");
        }
        return;
    }
    if (*ctxt).check == 0 {
        match (*ent).etype {
            Some(XmlEntityType::XmlInternalGeneralEntity) => {
                write!((*ctxt).output, "INTERNAL_GENERAL_ENTITY ");
            }
            Some(XmlEntityType::XmlExternalGeneralParsedEntity) => {
                write!((*ctxt).output, "EXTERNAL_GENERAL_PARSED_ENTITY ");
            }
            Some(XmlEntityType::XmlExternalGeneralUnparsedEntity) => {
                write!((*ctxt).output, "EXTERNAL_GENERAL_UNPARSED_ENTITY ");
            }
            Some(XmlEntityType::XmlInternalParameterEntity) => {
                write!((*ctxt).output, "INTERNAL_PARAMETER_ENTITY ");
            }
            Some(XmlEntityType::XmlExternalParameterEntity) => {
                write!((*ctxt).output, "EXTERNAL_PARAMETER_ENTITY ");
            }
            Some(e) => {
                write!((*ctxt).output, "ENTITY_{} ! ", e as i32);
            }
            _ => unreachable!(),
        }
        let name =
            CStr::from_ptr((*ent).name.load(Ordering::Relaxed) as *const i8).to_string_lossy();
        writeln!((*ctxt).output, "{name}");
        if !(*ent).external_id.load(Ordering::Relaxed).is_null() {
            xml_ctxt_dump_spaces(ctxt);
            let external_id =
                CStr::from_ptr((*ent).external_id.load(Ordering::Relaxed) as *const i8)
                    .to_string_lossy();
            writeln!((*ctxt).output, "ExternalID={external_id}");
        }
        if !(*ent).system_id.load(Ordering::Relaxed).is_null() {
            xml_ctxt_dump_spaces(ctxt);
            let system_id = CStr::from_ptr((*ent).system_id.load(Ordering::Relaxed) as *const i8)
                .to_string_lossy();
            writeln!((*ctxt).output, "SystemID={system_id}");
        }
        if !(*ent).uri.load(Ordering::Relaxed).is_null() {
            xml_ctxt_dump_spaces(ctxt);
            let uri =
                CStr::from_ptr((*ent).uri.load(Ordering::Relaxed) as *const i8).to_string_lossy();
            writeln!((*ctxt).output, "URI={uri}");
        }
        if !(*ent).content.load(Ordering::Relaxed).is_null() {
            xml_ctxt_dump_spaces(ctxt);
            write!((*ctxt).output, "content=");
            xml_ctxt_dump_string(ctxt, (*ent).content.load(Ordering::Relaxed));
            writeln!((*ctxt).output);
        }
    }
}

/// Dumps debug information for the attribute list
#[doc(alias = "xmlCtxtDumpAttrList")]
unsafe extern "C" fn xml_ctxt_dump_attr_list(ctxt: XmlDebugCtxtPtr, mut attr: XmlAttrPtr) {
    while !attr.is_null() {
        xml_ctxt_dump_attr(ctxt, attr);
        attr = (*attr).next;
    }
}

/// Dumps debug information for the element node, it is not recursive
#[doc(alias = "xmlCtxtDumpOneNode")]
unsafe extern "C" fn xml_ctxt_dump_one_node(ctxt: XmlDebugCtxtPtr, node: XmlNodePtr) {
    if node.is_null() {
        if (*ctxt).check == 0 {
            xml_ctxt_dump_spaces(ctxt);
            writeln!((*ctxt).output, "node is NULL");
        }
        return;
    }
    (*ctxt).node = node;

    match (*node).element_type() {
        XmlElementType::XmlElementNode => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
                write!((*ctxt).output, "ELEMENT ");
                if !(*node).ns.is_null() && !(*(*node).ns).prefix.is_null() {
                    xml_ctxt_dump_string(ctxt, (*(*node).ns).prefix);
                    write!((*ctxt).output, ":");
                }
                xml_ctxt_dump_string(ctxt, (*node).name);
                writeln!((*ctxt).output);
            }
        }
        XmlElementType::XmlAttributeNode => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
            }
            writeln!((*ctxt).output, "Error, ATTRIBUTE found here");
            xml_ctxt_generic_node_check(ctxt, node);
            return;
        }
        XmlElementType::XmlTextNode => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
                if (*node).name == XML_STRING_TEXT_NOENC.as_ptr() as *const XmlChar {
                    write!((*ctxt).output, "TEXT no enc");
                } else {
                    write!((*ctxt).output, "TEXT");
                }
                if (*ctxt).options & DUMP_TEXT_TYPE != 0 {
                    if (*node).content == addr_of_mut!((*node).properties) as *mut XmlChar {
                        writeln!((*ctxt).output, " compact");
                    } else if xml_dict_owns((*ctxt).dict, (*node).content) == 1 {
                        writeln!((*ctxt).output, " interned");
                    } else {
                        writeln!((*ctxt).output);
                    }
                } else {
                    writeln!((*ctxt).output);
                }
            }
        }
        XmlElementType::XmlCDATASectionNode => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
                writeln!((*ctxt).output, "CDATA_SECTION");
            }
        }
        XmlElementType::XmlEntityRefNode => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
                let name = CStr::from_ptr((*node).name as *const i8).to_string_lossy();
                writeln!((*ctxt).output, "ENTITY_REF({name})");
            }
        }
        XmlElementType::XmlEntityNode => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
                writeln!((*ctxt).output, "ENTITY");
            }
        }
        XmlElementType::XmlPINode => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
                let name = CStr::from_ptr((*node).name as *const i8).to_string_lossy();
                writeln!((*ctxt).output, "PI {name}");
            }
        }
        XmlElementType::XmlCommentNode => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
                writeln!((*ctxt).output, "COMMENT");
            }
        }
        XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
            }
            writeln!((*ctxt).output, "Error, DOCUMENT found here");
            xml_ctxt_generic_node_check(ctxt, node);
            return;
        }
        XmlElementType::XmlDocumentTypeNode => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
                writeln!((*ctxt).output, "DOCUMENT_TYPE");
            }
        }
        XmlElementType::XmlDocumentFragNode => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
                writeln!((*ctxt).output, "DOCUMENT_FRAG");
            }
        }
        XmlElementType::XmlNotationNode => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
                writeln!((*ctxt).output, "NOTATION");
            }
        }
        XmlElementType::XmlDTDNode => {
            xml_ctxt_dump_dtd_node(ctxt, (*node).as_dtd_node().unwrap().as_ptr());
            return;
        }
        XmlElementType::XmlElementDecl => {
            xml_ctxt_dump_elem_decl(ctxt, (*node).as_element_decl_node().unwrap().as_ptr());
            return;
        }
        XmlElementType::XmlAttributeDecl => {
            xml_ctxt_dump_attr_decl(ctxt, (*node).as_attribute_decl_node().unwrap().as_ptr());
            return;
        }
        XmlElementType::XmlEntityDecl => {
            xml_ctxt_dump_entity_decl(ctxt, node as XmlEntityPtr);
            return;
        }
        XmlElementType::XmlNamespaceDecl => {
            xml_ctxt_dump_namespace(ctxt, node as XmlNsPtr);
            return;
        }
        XmlElementType::XmlXIncludeStart => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
                writeln!((*ctxt).output, "INCLUDE START");
            }
            return;
        }
        XmlElementType::XmlXIncludeEnd => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
                writeln!((*ctxt).output, "INCLUDE END");
            }
            return;
        }
        _ => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
            }
            xml_debug_err!(
                ctxt,
                XmlParserErrors::XmlCheckUnknownNode,
                "Unknown node type {}\n",
                (*node).element_type() as i32
            );
            return;
        }
    }
    if (*node).doc.is_null() {
        if (*ctxt).check == 0 {
            xml_ctxt_dump_spaces(ctxt);
        }
        writeln!((*ctxt).output, "PBM: doc.is_null() !!!");
    }
    (*ctxt).depth += 1;
    if (*node).element_type() == XmlElementType::XmlElementNode && !(*node).ns_def.is_null() {
        xml_ctxt_dump_namespace_list(ctxt, (*node).ns_def);
    }
    if (*node).element_type() == XmlElementType::XmlElementNode && !(*node).properties.is_null() {
        xml_ctxt_dump_attr_list(ctxt, (*node).properties);
    }
    if (*node).element_type() != XmlElementType::XmlEntityRefNode {
        if ((*node).element_type() != XmlElementType::XmlElementNode && !(*node).content.is_null())
            && (*ctxt).check == 0
        {
            xml_ctxt_dump_spaces(ctxt);
            write!((*ctxt).output, "content=");
            xml_ctxt_dump_string(ctxt, (*node).content);
            writeln!((*ctxt).output);
        }
    } else {
        let ent: XmlEntityPtr = xml_get_doc_entity((*node).doc, (*node).name);
        if !ent.is_null() {
            xml_ctxt_dump_entity(ctxt, ent);
        }
    }
    (*ctxt).depth -= 1;

    /*
     * Do a bit of checking
     */
    xml_ctxt_generic_node_check(ctxt, node);
}

/// Dumps debug information for the element node, it is recursive
#[doc(alias = "xmlCtxtDumpNode")]
unsafe extern "C" fn xml_ctxt_dump_node(ctxt: XmlDebugCtxtPtr, node: XmlNodePtr) {
    if node.is_null() {
        if (*ctxt).check == 0 {
            xml_ctxt_dump_spaces(ctxt);
            writeln!((*ctxt).output, "node is NULL");
        }
        return;
    }
    xml_ctxt_dump_one_node(ctxt, node);
    if let Some(children) = (*node).children().filter(|_| {
        (*node).element_type() != XmlElementType::XmlNamespaceDecl
            && (*node).element_type() != XmlElementType::XmlEntityRefNode
    }) {
        (*ctxt).depth += 1;
        xml_ctxt_dump_node_list(ctxt, children.as_ptr());
        (*ctxt).depth -= 1;
    }
}

/// Dumps debug information for the list of element node, it is recursive
#[doc(alias = "xmlCtxtDumpNodeList")]
unsafe extern "C" fn xml_ctxt_dump_node_list(ctxt: XmlDebugCtxtPtr, mut node: XmlNodePtr) {
    while !node.is_null() {
        xml_ctxt_dump_node(ctxt, node);
        node = (*node).next.map_or(null_mut(), |n| n.as_ptr());
    }
}

/// Dumps debug information for the attribute
#[doc(alias = "xmlCtxtDumpAttr")]
unsafe extern "C" fn xml_ctxt_dump_attr(ctxt: XmlDebugCtxtPtr, attr: XmlAttrPtr) {
    xml_ctxt_dump_spaces(ctxt);

    if attr.is_null() {
        if (*ctxt).check == 0 {
            write!((*ctxt).output, "Attr is NULL");
        }
        return;
    }
    if (*ctxt).check == 0 {
        write!((*ctxt).output, "ATTRIBUTE ");
        xml_ctxt_dump_string(ctxt, (*attr).name);
        writeln!((*ctxt).output);
        if let Some(children) = (*attr).children {
            (*ctxt).depth += 1;
            xml_ctxt_dump_node_list(ctxt, children.as_ptr());
            (*ctxt).depth -= 1;
        }
    }
    if (*attr).name.is_null() {
        xml_debug_err!(
            ctxt,
            XmlParserErrors::XmlCheckNoName,
            "Attribute has no name",
        );
    }

    /*
     * Do a bit of checking
     */
    xml_ctxt_generic_node_check(ctxt, attr as XmlNodePtr);
}

#[doc(alias = "xmlCtxtDumpCleanCtxt")]
unsafe extern "C" fn xml_ctxt_dump_clean_ctxt(_ctxt: XmlDebugCtxtPtr) {
    /* remove the ATTRIBUTE_UNUSED when this is added */
}

/// Dumps debug information for the attribute
#[doc(alias = "xmlDebugDumpAttr")]
pub unsafe extern "C" fn xml_debug_dump_attr(
    output: &mut impl Write,
    attr: XmlAttrPtr,
    depth: i32,
) {
    let mut ctxt = XmlDebugCtxt::default();

    xml_ctxt_dump_init_ctxt(addr_of_mut!(ctxt));
    ctxt.output = Box::new(output);
    ctxt.depth = depth;
    xml_ctxt_dump_attr(addr_of_mut!(ctxt), attr);
    xml_ctxt_dump_clean_ctxt(addr_of_mut!(ctxt));
}

/// Dumps debug information for the attribute list
#[doc(alias = "xmlDebugDumpAttrList")]
pub unsafe extern "C" fn xml_debug_dump_attr_list<'a>(
    output: &mut (impl Write + 'a),
    attr: XmlAttrPtr,
    depth: i32,
) {
    let mut ctxt = XmlDebugCtxt::default();

    xml_ctxt_dump_init_ctxt(addr_of_mut!(ctxt));
    ctxt.output = Box::new(output);
    ctxt.depth = depth;
    xml_ctxt_dump_attr_list(addr_of_mut!(ctxt), attr);
    xml_ctxt_dump_clean_ctxt(addr_of_mut!(ctxt));
}

/// Dumps debug information for the element node, it is not recursive
#[doc(alias = "xmlDebugDumpOneNode")]
pub unsafe extern "C" fn xml_debug_dump_one_node(
    output: &mut impl Write,
    node: XmlNodePtr,
    depth: i32,
) {
    let mut ctxt = XmlDebugCtxt::default();

    xml_ctxt_dump_init_ctxt(addr_of_mut!(ctxt));
    ctxt.output = Box::new(output);
    ctxt.depth = depth;
    xml_ctxt_dump_one_node(addr_of_mut!(ctxt), node);
    xml_ctxt_dump_clean_ctxt(addr_of_mut!(ctxt));
}

/// Dumps debug information for the element node, it is recursive
#[doc(alias = "xmlDebugDumpNode")]
pub unsafe fn xml_debug_dump_node<'a>(
    output: Option<impl Write + 'a>,
    node: XmlNodePtr,
    depth: i32,
) {
    let mut ctxt = XmlDebugCtxt::default();

    xml_ctxt_dump_init_ctxt(addr_of_mut!(ctxt));
    ctxt.output = output.map_or(Box::new(stdout()) as Box<dyn Write>, |o| Box::new(o));
    ctxt.depth = depth;
    xml_ctxt_dump_node(addr_of_mut!(ctxt), node);
    xml_ctxt_dump_clean_ctxt(addr_of_mut!(ctxt));
}

/// Dumps debug information for the list of element node, it is recursive
#[doc(alias = "xmlDebugDumpNodeList")]
pub unsafe fn xml_debug_dump_node_list(output: Option<impl Write>, node: XmlNodePtr, depth: i32) {
    let mut ctxt = XmlDebugCtxt::default();

    xml_ctxt_dump_init_ctxt(addr_of_mut!(ctxt));
    ctxt.output = output.map_or(Box::new(stdout()) as Box<dyn Write>, |o| Box::new(o));
    ctxt.depth = depth;
    xml_ctxt_dump_node_list(addr_of_mut!(ctxt), node);
    xml_ctxt_dump_clean_ctxt(addr_of_mut!(ctxt));
}

#[doc(alias = "xmlCtxtDumpDocHead")]
unsafe extern "C" fn xml_ctxt_dump_doc_head(ctxt: XmlDebugCtxtPtr, doc: XmlDocPtr) {
    if doc.is_null() {
        if (*ctxt).check == 0 {
            writeln!((*ctxt).output, "DOCUMENT.is_null() !");
        }
        return;
    }
    (*ctxt).node = doc as XmlNodePtr;

    match (*doc).typ {
        XmlElementType::XmlElementNode => {
            xml_debug_err!(
                ctxt,
                XmlParserErrors::XmlCheckFoundElement,
                "Misplaced ELEMENT node\n",
            );
        }
        XmlElementType::XmlAttributeNode => {
            xml_debug_err!(
                ctxt,
                XmlParserErrors::XmlCheckFoundAttribute,
                "Misplaced ATTRIBUTE node\n",
            );
        }
        XmlElementType::XmlTextNode => {
            xml_debug_err!(
                ctxt,
                XmlParserErrors::XmlCheckFoundText,
                "Misplaced TEXT node\n",
            );
        }
        XmlElementType::XmlCDATASectionNode => {
            xml_debug_err!(
                ctxt,
                XmlParserErrors::XmlCheckFoundCDATA,
                "Misplaced CDATA node\n",
            );
        }
        XmlElementType::XmlEntityRefNode => {
            xml_debug_err!(
                ctxt,
                XmlParserErrors::XmlCheckFoundEntityRef,
                "Misplaced ENTITYREF node\n",
            );
        }
        XmlElementType::XmlEntityNode => {
            xml_debug_err!(
                ctxt,
                XmlParserErrors::XmlCheckFoundEntity,
                "Misplaced ENTITY node\n",
            );
        }
        XmlElementType::XmlPINode => {
            xml_debug_err!(
                ctxt,
                XmlParserErrors::XmlCheckFoundPI,
                "Misplaced PI node\n",
            );
        }
        XmlElementType::XmlCommentNode => {
            xml_debug_err!(
                ctxt,
                XmlParserErrors::XmlCheckFoundComment,
                "Misplaced COMMENT node\n",
            );
        }
        XmlElementType::XmlDocumentNode => {
            if (*ctxt).check == 0 {
                writeln!((*ctxt).output, "DOCUMENT");
            }
        }
        XmlElementType::XmlHTMLDocumentNode => {
            if (*ctxt).check == 0 {
                writeln!((*ctxt).output, "HTML DOCUMENT");
            }
        }
        XmlElementType::XmlDocumentTypeNode => {
            xml_debug_err!(
                ctxt,
                XmlParserErrors::XmlCheckFoundDoctype,
                "Misplaced DOCTYPE node\n",
            );
        }
        XmlElementType::XmlDocumentFragNode => {
            xml_debug_err!(
                ctxt,
                XmlParserErrors::XmlCheckFoundFragment,
                "Misplaced FRAGMENT node\n",
            );
        }
        XmlElementType::XmlNotationNode => {
            xml_debug_err!(
                ctxt,
                XmlParserErrors::XmlCheckFoundNotation,
                "Misplaced NOTATION node\n",
            );
        }
        _ => {
            xml_debug_err!(
                ctxt,
                XmlParserErrors::XmlCheckUnknownNode,
                "Unknown node type {}\n",
                (*doc).typ as i32
            );
        }
    }
}

/// Dumps debug information concerning the document, not recursive
#[doc(alias = "xmlCtxtDumpDocumentHead")]
unsafe extern "C" fn xml_ctxt_dump_document_head(ctxt: XmlDebugCtxtPtr, doc: XmlDocPtr) {
    if doc.is_null() {
        return;
    }
    xml_ctxt_dump_doc_head(ctxt, doc);
    if (*ctxt).check == 0 {
        if !(*doc).name.is_null() {
            write!((*ctxt).output, "name=");
            xml_ctxt_dump_string(ctxt, (*doc).name as _);
            writeln!((*ctxt).output);
        }
        if let Some(version) = (*doc).version.as_deref() {
            write!((*ctxt).output, "version=");
            let version = CString::new(version).unwrap();
            xml_ctxt_dump_string(ctxt, version.as_ptr() as *const u8);
            writeln!((*ctxt).output);
        }
        if let Some(encoding) = (*doc).encoding.as_deref() {
            write!((*ctxt).output, "encoding=");
            let encoding = CString::new(encoding).unwrap();
            xml_ctxt_dump_string(ctxt, encoding.as_ptr() as *const u8);
            writeln!((*ctxt).output);
        }
        if let Some(url) = (*doc).url.as_deref() {
            write!((*ctxt).output, "URL=");
            let url = CString::new(url).unwrap();
            xml_ctxt_dump_string(ctxt, url.as_ptr() as *const u8);
            writeln!((*ctxt).output);
        }
        if (*doc).standalone != 0 {
            writeln!((*ctxt).output, "standalone=true");
        }
    }
    if !(*doc).old_ns.is_null() {
        xml_ctxt_dump_namespace_list(ctxt, (*doc).old_ns);
    }
}

/// Dumps debug information concerning the document, not recursive
#[doc(alias = "xmlDebugDumpDocumentHead")]
pub unsafe fn xml_debug_dump_document_head(output: Option<impl Write>, doc: XmlDocPtr) {
    let mut ctxt = XmlDebugCtxt::default();

    xml_ctxt_dump_init_ctxt(addr_of_mut!(ctxt));
    ctxt.options |= DUMP_TEXT_TYPE;
    ctxt.output = output.map_or(Box::new(stdout()) as Box<dyn Write>, |o| Box::new(o));
    xml_ctxt_dump_document_head(addr_of_mut!(ctxt), doc);
    xml_ctxt_dump_clean_ctxt(addr_of_mut!(ctxt));
}

/// Dumps debug information for the document, it's recursive
#[doc(alias = "xmlCtxtDumpDocument")]
unsafe extern "C" fn xml_ctxt_dump_document(ctxt: XmlDebugCtxtPtr, doc: XmlDocPtr) {
    if doc.is_null() {
        if (*ctxt).check == 0 {
            writeln!((*ctxt).output, "DOCUMENT.is_null() !");
        }
        return;
    }
    xml_ctxt_dump_document_head(ctxt, doc);
    if let Some(children) = (*doc).children.filter(|_| {
        matches!(
            (*doc).typ,
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode
        )
    }) {
        (*ctxt).depth += 1;
        xml_ctxt_dump_node_list(ctxt, children.as_ptr());
        (*ctxt).depth -= 1;
    }
}

/// Dumps debug information for the document, it's recursive
#[doc(alias = "xmlDebugDumpDocument")]
pub unsafe fn xml_debug_dump_document(output: Option<impl Write>, doc: XmlDocPtr) {
    let mut ctxt = XmlDebugCtxt::default();

    xml_ctxt_dump_init_ctxt(addr_of_mut!(ctxt));
    ctxt.options |= DUMP_TEXT_TYPE;
    ctxt.output = output.map_or(Box::new(stdout()) as Box<dyn Write>, |o| Box::new(o));
    xml_ctxt_dump_document(addr_of_mut!(ctxt), doc);
    xml_ctxt_dump_clean_ctxt(addr_of_mut!(ctxt));
}

/// Dumps debug information for the DTD
#[doc(alias = "xmlDebugDumpDTD")]
pub unsafe fn xml_debug_dump_dtd(output: Option<impl Write>, dtd: XmlDtdPtr) {
    let mut ctxt = XmlDebugCtxt::default();

    xml_ctxt_dump_init_ctxt(addr_of_mut!(ctxt));
    ctxt.options |= DUMP_TEXT_TYPE;
    ctxt.output = output.map_or(Box::new(stdout()) as Box<dyn Write>, |o| Box::new(o));
    xml_ctxt_dump_dtd(addr_of_mut!(ctxt), dtd);
    xml_ctxt_dump_clean_ctxt(addr_of_mut!(ctxt));
}

#[doc(alias = "xmlCtxtDumpEntityCallback")]
extern "C" fn xml_ctxt_dump_entity_callback(cur: XmlEntityPtr, ctxt: XmlDebugCtxtPtr) {
    unsafe {
        if cur.is_null() {
            if (*ctxt).check == 0 {
                write!((*ctxt).output, "Entity is NULL");
            }
            return;
        }
        if (*ctxt).check == 0 {
            let name =
                CStr::from_ptr((*cur).name.load(Ordering::Relaxed) as *const i8).to_string_lossy();
            write!((*ctxt).output, "{name} : ");
            match (*cur).etype {
                Some(XmlEntityType::XmlInternalGeneralEntity) => {
                    write!((*ctxt).output, "INTERNAL GENERAL, ");
                }
                Some(XmlEntityType::XmlExternalGeneralParsedEntity) => {
                    write!((*ctxt).output, "EXTERNAL PARSED, ");
                }
                Some(XmlEntityType::XmlExternalGeneralUnparsedEntity) => {
                    write!((*ctxt).output, "EXTERNAL UNPARSED, ");
                }
                Some(XmlEntityType::XmlInternalParameterEntity) => {
                    write!((*ctxt).output, "INTERNAL PARAMETER, ");
                }
                Some(XmlEntityType::XmlExternalParameterEntity) => {
                    write!((*ctxt).output, "EXTERNAL PARAMETER, ");
                }
                Some(e) => {
                    xml_debug_err!(
                        ctxt,
                        XmlParserErrors::XmlCheckEntityType,
                        "Unknown entity type {}\n",
                        e as i32
                    );
                }
                _ => unreachable!(),
            }
            if !(*cur).external_id.load(Ordering::Relaxed).is_null() {
                let external_id =
                    CStr::from_ptr((*cur).external_id.load(Ordering::Relaxed) as *const i8)
                        .to_string_lossy();
                write!((*ctxt).output, "ID \"{external_id}\"");
            }
            if !(*cur).system_id.load(Ordering::Relaxed).is_null() {
                let system_id =
                    CStr::from_ptr((*cur).system_id.load(Ordering::Relaxed) as *const i8)
                        .to_string_lossy();
                write!((*ctxt).output, "SYSTEM \"{system_id}\"");
            }
            if !(*cur).orig.load(Ordering::Relaxed).is_null() {
                let orig = CStr::from_ptr((*cur).orig.load(Ordering::Relaxed) as *const i8)
                    .to_string_lossy();
                write!((*ctxt).output, "\n orig \"{orig}\"");
            }
            if (*cur).typ != XmlElementType::XmlElementNode
                && !(*cur).content.load(Ordering::Relaxed).is_null()
            {
                let content = CStr::from_ptr((*cur).content.load(Ordering::Relaxed) as *const i8)
                    .to_string_lossy();
                write!((*ctxt).output, "\n content \"{content}\"");
            }
            writeln!((*ctxt).output);
        }
    }
}

/// Dumps debug information for all the entities in use by the document
#[doc(alias = "xmlCtxtDumpEntities")]
unsafe extern "C" fn xml_ctxt_dump_entities(ctxt: XmlDebugCtxtPtr, doc: XmlDocPtr) {
    if doc.is_null() {
        return;
    }
    xml_ctxt_dump_doc_head(ctxt, doc);
    if !(*doc).int_subset.is_null() {
        if let Some(table) = (*(*doc).int_subset).entities {
            if (*ctxt).check == 0 {
                writeln!((*ctxt).output, "Entities in internal subset");
            }
            table.scan(|payload, _, _, _| {
                xml_ctxt_dump_entity_callback(*payload, ctxt);
            });
        }
    } else {
        writeln!((*ctxt).output, "No entities in internal subset");
    }
    if !(*doc).ext_subset.is_null() {
        if let Some(table) = (*(*doc).ext_subset).entities {
            if (*ctxt).check == 0 {
                writeln!((*ctxt).output, "Entities in external subset");
            }
            table.scan(|payload, _, _, _| {
                xml_ctxt_dump_entity_callback(*payload, ctxt);
            });
        }
    } else if (*ctxt).check == 0 {
        writeln!((*ctxt).output, "No entities in external subset");
    }
}

/// Dumps debug information for all the entities in use by the document
#[doc(alias = "xmlDebugDumpEntities")]
pub unsafe fn xml_debug_dump_entities<'a>(output: impl Write + 'a, doc: XmlDocPtr) {
    let mut ctxt = XmlDebugCtxt::default();

    xml_ctxt_dump_init_ctxt(addr_of_mut!(ctxt));
    ctxt.output = Box::new(output);
    xml_ctxt_dump_entities(addr_of_mut!(ctxt), doc);
    xml_ctxt_dump_clean_ctxt(addr_of_mut!(ctxt));
}

/// Check the document for potential content problems, and output
/// the errors to `output`
///
/// Returns the number of errors found
#[doc(alias = "xmlDebugCheckDocument")]
pub unsafe fn xml_debug_check_document(
    output: Option<impl Write + 'static>,
    doc: XmlDocPtr,
) -> i32 {
    let mut ctxt = XmlDebugCtxt::default();

    xml_ctxt_dump_init_ctxt(addr_of_mut!(ctxt));
    ctxt.output = output.map_or(Box::new(stdout()) as Box<dyn Write>, |o| Box::new(o));
    ctxt.check = 1;
    xml_ctxt_dump_document(addr_of_mut!(ctxt), doc);
    xml_ctxt_dump_clean_ctxt(addr_of_mut!(ctxt));
    ctxt.errors
}

/// Dump to `output` the type and name of @node.
#[doc(alias = "xmlLsOneNode")]
pub unsafe extern "C" fn xml_ls_one_node<'a>(output: &mut (impl Write + 'a), node: XmlNodePtr) {
    if node.is_null() {
        writeln!(output, "NULL");
        return;
    }
    match (*node).element_type() {
        XmlElementType::XmlElementNode => {
            write!(output, "-");
        }
        XmlElementType::XmlAttributeNode => {
            write!(output, "a");
        }
        XmlElementType::XmlTextNode => {
            write!(output, "t");
        }
        XmlElementType::XmlCDATASectionNode => {
            write!(output, "C");
        }
        XmlElementType::XmlEntityRefNode => {
            write!(output, "e");
        }
        XmlElementType::XmlEntityNode => {
            write!(output, "E");
        }
        XmlElementType::XmlPINode => {
            write!(output, "p");
        }
        XmlElementType::XmlCommentNode => {
            write!(output, "c");
        }
        XmlElementType::XmlDocumentNode => {
            write!(output, "d");
        }
        XmlElementType::XmlHTMLDocumentNode => {
            write!(output, "h");
        }
        XmlElementType::XmlDocumentTypeNode => {
            write!(output, "T");
        }
        XmlElementType::XmlDocumentFragNode => {
            write!(output, "F");
        }
        XmlElementType::XmlNotationNode => {
            write!(output, "N");
        }
        XmlElementType::XmlNamespaceDecl => {
            write!(output, "n");
        }
        _ => {
            write!(output, "?");
        }
    }
    if (*node).element_type() != XmlElementType::XmlNamespaceDecl {
        if !(*node).properties.is_null() {
            write!(output, "a");
        } else {
            write!(output, "-");
        }
        if !(*node).ns_def.is_null() {
            write!(output, "n");
        } else {
            write!(output, "-");
        }
    }

    write!(output, " {} ", xml_ls_count_node(node));

    match (*node).element_type() {
        XmlElementType::XmlElementNode => {
            if !(*node).name.is_null() {
                if !(*node).ns.is_null() && !(*(*node).ns).prefix.is_null() {
                    let prefix =
                        CStr::from_ptr((*(*node).ns).prefix as *const i8).to_string_lossy();
                    write!(output, "{prefix}:");
                }
                let name = CStr::from_ptr((*node).name as *const i8).to_string_lossy();
                write!(output, "{name}");
            }
        }
        XmlElementType::XmlAttributeNode => {
            if !(*node).name.is_null() {
                let name = CStr::from_ptr((*node).name as *const i8).to_string_lossy();
                write!(output, "{name}");
            }
        }
        XmlElementType::XmlTextNode => {
            if !(*node).content.is_null() {
                xml_debug_dump_string(Some(output), (*node).content);
            }
        }
        XmlElementType::XmlCDATASectionNode => {}
        XmlElementType::XmlEntityRefNode => {
            if !(*node).name.is_null() {
                let name = CStr::from_ptr((*node).name as *const i8).to_string_lossy();
                write!(output, "{name}");
            }
        }
        XmlElementType::XmlEntityNode => {
            if !(*node).name.is_null() {
                let name = CStr::from_ptr((*node).name as *const i8).to_string_lossy();
                write!(output, "{name}");
            }
        }
        XmlElementType::XmlPINode => {
            if !(*node).name.is_null() {
                let name = CStr::from_ptr((*node).name as *const i8).to_string_lossy();
                write!(output, "{name}");
            }
        }
        XmlElementType::XmlCommentNode => {}
        XmlElementType::XmlDocumentNode => {}
        XmlElementType::XmlHTMLDocumentNode => {}
        XmlElementType::XmlDocumentTypeNode => {}
        XmlElementType::XmlDocumentFragNode => {}
        XmlElementType::XmlNotationNode => {}
        XmlElementType::XmlNamespaceDecl => {
            let ns: XmlNsPtr = node as XmlNsPtr;

            let href = CStr::from_ptr((*ns).href as *const i8).to_string_lossy();
            if (*ns).prefix.is_null() {
                write!(output, "default -> {href}");
            } else {
                let prefix = CStr::from_ptr((*ns).prefix as *const i8).to_string_lossy();
                write!(output, "{prefix} -> {href}");
            }
        }
        _ => {
            if !(*node).name.is_null() {
                let name = CStr::from_ptr((*node).name as *const i8).to_string_lossy();
                write!(output, "{name}");
            }
        }
    }
    writeln!(output);
}

/// Count the children of @node.
///
/// Returns the number of children of @node.
#[doc(alias = "xmlLsCountNode")]
pub unsafe extern "C" fn xml_ls_count_node(node: XmlNodePtr) -> i32 {
    let mut ret: i32 = 0;
    let mut list: XmlNodePtr = null_mut();

    if node.is_null() {
        return 0;
    }

    match (*node).element_type() {
        XmlElementType::XmlElementNode => {
            list = (*node).children().map_or(null_mut(), |c| c.as_ptr());
        }
        XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
            list = (*node)
                .as_document_node()
                .unwrap()
                .as_ref()
                .children
                .map_or(null_mut(), |c| c.as_ptr());
        }
        XmlElementType::XmlAttributeNode => {
            list = (*node)
                .as_attribute_node()
                .unwrap()
                .as_ref()
                .children
                .map_or(null_mut(), |c| c.as_ptr());
        }
        XmlElementType::XmlTextNode
        | XmlElementType::XmlCDATASectionNode
        | XmlElementType::XmlPINode
        | XmlElementType::XmlCommentNode => {
            if !(*node).content.is_null() {
                ret = xml_strlen((*node).content);
            }
        }
        XmlElementType::XmlEntityRefNode
        | XmlElementType::XmlDocumentTypeNode
        | XmlElementType::XmlEntityNode
        | XmlElementType::XmlDocumentFragNode
        | XmlElementType::XmlNotationNode
        | XmlElementType::XmlDTDNode
        | XmlElementType::XmlElementDecl
        | XmlElementType::XmlAttributeDecl
        | XmlElementType::XmlEntityDecl
        | XmlElementType::XmlNamespaceDecl
        | XmlElementType::XmlXIncludeStart
        | XmlElementType::XmlXIncludeEnd => {
            ret = 1;
        }
        _ => unreachable!(),
    }
    while !list.is_null() {
        list = (*list).next.map_or(null_mut(), |n| n.as_ptr());
        ret += 1;
    }
    ret
}

/// Convenient way to turn bool into text
///
/// Returns a pointer to either "True" or "False"
#[doc(alias = "xmlBoolToText")]
pub unsafe extern "C" fn xml_bool_to_text(boolval: i32) -> *const c_char {
    if boolval != 0 {
        c"True".as_ptr()
    } else {
        c"False".as_ptr()
    }
}

/// This is a generic signature for the XML shell input function.
///
/// Returns a string which will be freed by the Shell.
#[doc(alias = "xmlShellReadlineFunc")]
#[cfg(feature = "xpath")]
pub type XmlShellReadlineFunc = unsafe extern "C" fn(prompt: *mut c_char) -> *mut c_char;

/// A debugging shell context.  
/// TODO: add the defined function tables.
#[cfg(feature = "xpath")]
pub type XmlShellCtxtPtr<'a> = *mut XmlShellCtxt<'a>;
#[doc(alias = "xmlShellCtxt")]
#[cfg(feature = "xpath")]
#[repr(C)]
pub struct XmlShellCtxt<'a> {
    filename: *mut c_char,
    doc: XmlDocPtr,
    node: XmlNodePtr,
    pctxt: XmlXPathContextPtr,
    loaded: i32,
    output: Box<dyn Write + 'a>,
    input: XmlShellReadlineFunc,
}

/// This is a generic signature for the XML shell functions.
///
/// Returns an int, negative returns indicating errors.
#[doc(alias = "xmlShellCmd")]
#[cfg(feature = "xpath")]
pub type XmlShellCmd = unsafe extern "C" fn(
    ctxt: XmlShellCtxtPtr,
    arg: *mut c_char,
    node: XmlNodePtr,
    node2: XmlNodePtr,
) -> i32;

/// Print the xpath error to libxml default error channel
#[doc(alias = "xmlShellPrintXPathError")]
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_shell_print_xpath_error(error_type: i32, mut arg: *const c_char) {
    use std::ffi::CStr;

    use crate::generic_error;

    let default_arg: *const c_char = c"Result".as_ptr();

    if arg.is_null() {
        arg = default_arg;
    }

    match XmlXPathObjectType::try_from(error_type) {
        Ok(XmlXPathObjectType::XpathUndefined) => {
            generic_error!("{}: no such node\n", CStr::from_ptr(arg).to_string_lossy());
        }
        Ok(XmlXPathObjectType::XpathBoolean) => {
            generic_error!("{} is a Boolean\n", CStr::from_ptr(arg).to_string_lossy());
        }
        Ok(XmlXPathObjectType::XpathNumber) => {
            generic_error!("{} is a number\n", CStr::from_ptr(arg).to_string_lossy());
        }
        Ok(XmlXPathObjectType::XpathString) => {
            generic_error!("{} is a string\n", CStr::from_ptr(arg).to_string_lossy());
        }
        #[cfg(feature = "libxml_xptr_locs")]
        Ok(XmlXPathObjectType::XpathPoint) => {
            generic_error!("{} is a point\n", CStr::from_ptr(arg).to_string_lossy());
        }
        #[cfg(feature = "libxml_xptr_locs")]
        Ok(XmlXPathObjectType::XpathRange) => {
            generic_error!("{} is a range\n", CStr::from_ptr(arg).to_string_lossy());
        }
        #[cfg(feature = "libxml_xptr_locs")]
        Ok(XmlXPathObjectType::XpathLocationset) => {
            generic_error!("{} is a range\n", CStr::from_ptr(arg).to_string_lossy());
        }
        Ok(XmlXPathObjectType::XpathUsers) => {
            generic_error!(
                "{} is user-defined\n",
                CStr::from_ptr(arg).to_string_lossy()
            );
        }
        Ok(XmlXPathObjectType::XpathXsltTree) => {
            generic_error!(
                "{} is an XSLT value tree\n",
                CStr::from_ptr(arg).to_string_lossy()
            );
        }
        _ => unreachable!(),
    }
}

/// Print node to the output FILE
#[doc(alias = "xmlShellPrintNodeCtxt")]
#[cfg(feature = "libxml_output")]
unsafe extern "C" fn xml_shell_print_node_ctxt(ctxt: XmlShellCtxtPtr, node: XmlNodePtr) {
    if node.is_null() {
        return;
    }

    let stdout = &mut stdout();
    let fp = if ctxt.is_null() {
        stdout as &mut dyn Write
    } else {
        (*ctxt).output.as_mut()
    };
    let mut boxed = Box::new(fp);
    if (*node).element_type() == XmlElementType::XmlDocumentNode {
        (*node)
            .as_document_node()
            .unwrap()
            .as_mut()
            .dump_file(&mut boxed);
    } else if (*node).element_type() == XmlElementType::XmlAttributeNode {
        xml_debug_dump_attr_list(&mut boxed, (*node).as_attribute_node().unwrap().as_ptr(), 0);
    } else {
        (*node).dump_file(&mut boxed, (*node).doc);
    }

    writeln!(boxed);
}

/// Prints result to the output FILE
#[doc(alias = "xmlShellPrintXPathResultCtxt")]
unsafe extern "C" fn xml_shell_print_xpath_result_ctxt(
    ctxt: XmlShellCtxtPtr,
    list: XmlXPathObjectPtr,
) {
    if !ctxt.is_null() {
        return;
    }

    if !list.is_null() {
        match (*list).typ {
            XmlXPathObjectType::XpathNodeset => {
                #[cfg(feature = "libxml_output")]
                if !(*list).nodesetval.is_null() {
                    if let Some(table) = (*(*list).nodesetval).node_tab.as_deref() {
                        for &node in table {
                            xml_shell_print_node_ctxt(ctxt, node);
                        }
                    }
                } else {
                    generic_error!("Empty node set\n");
                }
                #[cfg(not(feature = "libxml_output"))]
                {
                    generic_error!("Node set\n");
                }
            }
            XmlXPathObjectType::XpathBoolean => {
                generic_error!(
                    "Is a Boolean:{}\n",
                    CStr::from_ptr(xml_bool_to_text((*list).boolval)).to_string_lossy()
                );
            }
            XmlXPathObjectType::XpathNumber => {
                generic_error!("Is a number:{}\n", (*list).floatval);
            }
            XmlXPathObjectType::XpathString => {
                generic_error!(
                    "Is a string:{}\n",
                    CStr::from_ptr((*list).stringval as *const i8).to_string_lossy()
                );
            }
            _ => {
                xml_shell_print_xpath_error((*list).typ as i32, null_mut());
            }
        }
    }
}

/// Prints result to the output FILE
#[doc(alias = "xmlShellPrintXPathResult")]
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_shell_print_xpath_result(list: XmlXPathObjectPtr) {
    xml_shell_print_xpath_result_ctxt(null_mut(), list);
}

/// Implements the XML shell function "ls"
/// Does an Unix like listing of the given node (like a directory)
///
/// Returns 0
#[doc(alias = "xmlShellList")]
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_shell_list(
    ctxt: XmlShellCtxtPtr,
    _arg: *mut c_char,
    node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    let mut cur: XmlNodePtr;
    if ctxt.is_null() {
        return 0;
    }
    if node.is_null() {
        writeln!((*ctxt).output, "NULL");
        return 0;
    }
    if (*node).element_type() == XmlElementType::XmlDocumentNode
        || (*node).element_type() == XmlElementType::XmlHTMLDocumentNode
    {
        cur = (*node)
            .as_document_node()
            .unwrap()
            .as_ref()
            .children
            .map_or(null_mut(), |c| c.as_ptr());
    } else if (*node).element_type() == XmlElementType::XmlNamespaceDecl {
        xml_ls_one_node(&mut (*ctxt).output, node);
        return 0;
    } else if let Some(children) = (*node).children() {
        cur = children.as_ptr();
    } else {
        xml_ls_one_node(&mut (*ctxt).output, node);
        return 0;
    }
    while !cur.is_null() {
        xml_ls_one_node(&mut (*ctxt).output, cur);
        cur = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
    }
    0
}

/// Implements the XML shell function "base"
/// dumps the current XML base of the node
///
/// Returns 0
#[doc(alias = "xmlShellBase")]
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_shell_base(
    ctxt: XmlShellCtxtPtr,
    _arg: *mut c_char,
    node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    use crate::tree::NodeCommon;

    if ctxt.is_null() {
        return 0;
    }
    if node.is_null() {
        writeln!((*ctxt).output, "NULL");
        return 0;
    }

    if let Some(base) = (*node).get_base((*node).doc) {
        writeln!((*ctxt).output, "{base}");
    } else {
        writeln!((*ctxt).output, " No base found !!!");
    }
    0
}

/// Implements the XML shell function "dir"
/// dumps information about the node (namespace, attributes, content).
///
/// Returns 0
#[doc(alias = "xmlShellDir")]
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_shell_dir(
    ctxt: XmlShellCtxtPtr,
    _arg: *mut c_char,
    node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    if ctxt.is_null() {
        return 0;
    }
    if node.is_null() {
        writeln!((*ctxt).output, "NULL");
        return 0;
    }
    if (*node).element_type() == XmlElementType::XmlDocumentNode
        || (*node).element_type() == XmlElementType::XmlHTMLDocumentNode
    {
        xml_debug_dump_document_head(
            Some(&mut (*ctxt).output),
            (*node).as_document_node().unwrap().as_ptr(),
        );
    } else if (*node).element_type() == XmlElementType::XmlAttributeNode {
        xml_debug_dump_attr(
            &mut (*ctxt).output,
            (*node).as_attribute_node().unwrap().as_ptr(),
            0,
        );
    } else {
        xml_debug_dump_one_node(&mut (*ctxt).output, node, 0);
    }
    0
}

/// Implements the XML shell function "load"
/// loads a new document specified by the filename
///
/// Returns 0 or -1 if loading failed
#[doc(alias = "xmlShellLoad")]
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_shell_load(
    ctxt: XmlShellCtxtPtr,
    filename: *mut c_char,
    _node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    use crate::libxml::{
        globals::xml_free,
        htmlparser::html_parse_file,
        parser::xml_read_file,
        uri::xml_canonic_path,
        xpath::{xml_xpath_free_context, xml_xpath_new_context},
    };

    use crate::tree::xml_free_doc;

    let doc: XmlDocPtr;
    let mut html: i32 = 0;

    if ctxt.is_null() || filename.is_null() {
        return -1;
    }
    if !(*ctxt).doc.is_null() {
        html = ((*(*ctxt).doc).typ == XmlElementType::XmlHTMLDocumentNode) as i32;
    }

    if html != 0 {
        #[cfg(feature = "html")]
        {
            doc = html_parse_file(filename, None);
        }
        #[cfg(not(feature = "html"))]
        {
            write!((*ctxt).output, "HTML support not compiled in\n".as_ptr());
            doc = null_mut();
        }
    } else {
        doc = xml_read_file(filename, None, 0);
    }
    if !doc.is_null() {
        if (*ctxt).loaded == 1 {
            xml_free_doc((*ctxt).doc);
        }
        (*ctxt).loaded = 1;
        #[cfg(feature = "xpath")]
        {
            xml_xpath_free_context((*ctxt).pctxt);
        }
        xml_free((*ctxt).filename as _);
        (*ctxt).doc = doc;
        (*ctxt).node = doc as XmlNodePtr;
        #[cfg(feature = "xpath")]
        {
            (*ctxt).pctxt = xml_xpath_new_context(doc);
        }
        (*ctxt).filename = xml_canonic_path(filename as *mut XmlChar) as *mut c_char;
    } else {
        return -1;
    }
    0
}

/// Print node to the output FILE
#[doc(alias = "xmlShellPrintNode")]
#[cfg(all(feature = "xpath", feature = "libxml_output"))]
pub unsafe extern "C" fn xml_shell_print_node(node: XmlNodePtr) {
    xml_shell_print_node_ctxt(null_mut(), node);
}

/// Implements the XML shell function "cat"
/// dumps the serialization node content (XML or HTML).
///
/// Returns 0
#[doc(alias = "xmlShellCat")]
#[cfg(all(feature = "xpath", feature = "libxml_output"))]
pub unsafe extern "C" fn xml_shell_cat(
    ctxt: XmlShellCtxtPtr,
    _arg: *mut c_char,
    node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    use crate::libxml::htmltree::{html_doc_dump, html_node_dump_file};

    use super::htmlparser::HtmlDocPtr;

    if ctxt.is_null() {
        return 0;
    }
    if node.is_null() {
        writeln!((*ctxt).output, "NULL");
        return 0;
    }
    if (*(*ctxt).doc).typ == XmlElementType::XmlHTMLDocumentNode {
        #[cfg(feature = "html")]
        if (*node).element_type() == XmlElementType::XmlHTMLDocumentNode {
            html_doc_dump(&mut (*ctxt).output, node as HtmlDocPtr);
        } else {
            html_node_dump_file(&mut (*ctxt).output, (*ctxt).doc, node);
        }
        #[cfg(not(feature = "html"))]
        if (*node).element_type() == XmlElementType::XmlDocumentNode {
            (*node)
                .as_document_node()
                .unwrap()
                .as_mut()
                .dump_file((*ctxt).output);
        } else {
            xml_elem_dump((*ctxt).output, (*ctxt).doc, node);
        }
    } else if (*node).element_type() == XmlElementType::XmlDocumentNode {
        (*node)
            .as_document_node()
            .unwrap()
            .as_mut()
            .dump_file(&mut (*ctxt).output);
    } else {
        (*node).dump_file(&mut (*ctxt).output, (*ctxt).doc);
    }
    writeln!((*ctxt).output);
    0
}

/// Implements the XML shell function "write"
/// Write the current node to the filename, it saves the serialization
/// of the subtree under the @node specified
///
/// Returns 0 or -1 in case of error
#[doc(alias = "xmlShellWrite")]
#[cfg(all(feature = "xpath", feature = "libxml_output"))]
pub unsafe fn xml_shell_write(
    ctxt: XmlShellCtxtPtr,
    filename: &str,
    node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    use std::fs::File;

    use crate::libxml::htmltree::html_save_file;

    if node.is_null() {
        return -1;
    }
    if filename.is_empty() {
        return -1;
    }
    let cfilename = CString::new(filename).unwrap();
    match (*node).element_type() {
        XmlElementType::XmlDocumentNode => {
            if (*ctxt).doc.is_null() || (*(*ctxt).doc).save_file(filename) < -1 {
                generic_error!("Failed to write to {filename}\n");
                return -1;
            }
        }
        XmlElementType::XmlHTMLDocumentNode => {
            #[cfg(feature = "html")]
            if html_save_file(cfilename.as_ptr(), (*ctxt).doc) < 0 {
                generic_error!("Failed to write to {filename}\n");
                return -1;
            }
            #[cfg(not(feature = "html"))]
            if xml_save_file(filename as *mut c_char, (*ctxt).doc) < -1 {
                generic_error!(
                    "Failed to write to {}\n",
                    CStr::from_ptr(filename as *const i8).to_string_lossy()
                );
                return -1;
            }
        }
        _ => {
            match File::options()
                .write(true)
                .truncate(true)
                .create(true)
                .open(filename)
            {
                Ok(mut f) => {
                    (*node).dump_file(&mut f, (*ctxt).doc);
                }
                _ => {
                    generic_error!("Failed to write to {filename}\n");
                    return -1;
                }
            }
        }
    }
    0
}

/// Implements the XML shell function "save"
/// Write the current document to the filename, or it's original name
///
/// Returns 0 or -1 in case of error
#[doc(alias = "xmlShellSave")]
#[cfg(all(feature = "xpath", feature = "libxml_output"))]
pub unsafe extern "C" fn xml_shell_save(
    ctxt: XmlShellCtxtPtr,
    mut filename: *mut c_char,
    _node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    use crate::libxml::htmltree::html_save_file;

    if ctxt.is_null() || (*ctxt).doc.is_null() {
        return -1;
    }
    if filename.is_null() || *filename.add(0) == 0 {
        filename = (*ctxt).filename;
    }
    if filename.is_null() {
        return -1;
    }
    match (*(*ctxt).doc).typ {
        XmlElementType::XmlDocumentNode => {
            if (*ctxt).doc.is_null()
                || (*(*ctxt).doc).save_file(CStr::from_ptr(filename).to_string_lossy().as_ref()) < 0
            {
                generic_error!(
                    "Failed to save to {}\n",
                    CStr::from_ptr(filename as *const i8).to_string_lossy()
                );
            }
        }
        XmlElementType::XmlHTMLDocumentNode => {
            #[cfg(feature = "html")]
            if html_save_file(filename as *mut c_char, (*ctxt).doc) < 0 {
                generic_error!(
                    "Failed to save to {}\n",
                    CStr::from_ptr(filename as *const i8).to_string_lossy()
                );
            }
            #[cfg(not(feature = "html"))]
            if (xml_save_file(filename as *mut c_char, (*ctxt).doc) < 0) {
                generic_error!(
                    "Failed to save to {}\n",
                    CStr::from_ptr(filename as *const i8).to_string_lossy()
                );
            }
        }
        _ => {
            generic_error!("To save to subparts of a document use the 'write' command\n");
            return -1;
        }
    }
    0
}

/// Implements the XML shell function "validate"
/// Validate the document, if a DTD path is provided, then the validation
/// is done against the given DTD.
///
/// Returns 0 or -1 in case of error
#[doc(alias = "xmlShellValidate")]
#[cfg(all(feature = "xpath", feature = "libxml_valid"))]
pub unsafe extern "C" fn xml_shell_validate(
    ctxt: XmlShellCtxtPtr,
    dtd: *mut c_char,
    _node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    use std::mem::size_of_val;

    use libc::memset;

    use crate::{
        globals::GLOBAL_STATE,
        libxml::{
            parser::xml_parse_dtd,
            valid::{xml_validate_document, xml_validate_dtd},
        },
        tree::xml_free_dtd,
    };

    use super::valid::XmlValidCtxt;

    let mut vctxt: XmlValidCtxt = unsafe { zeroed() };
    let mut res: i32 = -1;

    if ctxt.is_null() || (*ctxt).doc.is_null() {
        return -1;
    }
    memset(addr_of_mut!(vctxt) as _, 0, size_of_val(&vctxt));
    vctxt.error = Some(GLOBAL_STATE.with_borrow(|state| state.generic_error));
    vctxt.warning = vctxt.error;

    if dtd.is_null() || *dtd.add(0) == 0 {
        res = xml_validate_document(addr_of_mut!(vctxt), (*ctxt).doc);
    } else {
        let subset: XmlDtdPtr = xml_parse_dtd(null_mut(), dtd as *mut XmlChar);
        if !subset.is_null() {
            res = xml_validate_dtd(addr_of_mut!(vctxt), (*ctxt).doc, subset);

            xml_free_dtd(subset);
        }
    }
    res
}

/// Implements the XML shell function "du"
/// show the structure of the subtree under node @tree
/// If @tree is null, the command works on the current node.
///
/// Returns 0 or -1 in case of error
#[doc(alias = "xmlShellDu")]
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_shell_du(
    ctxt: XmlShellCtxtPtr,
    _arg: *mut c_char,
    tree: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    let mut node: XmlNodePtr;
    let mut indent: i32 = 0;

    if ctxt.is_null() {
        return -1;
    }

    if tree.is_null() {
        return -1;
    }
    node = tree;
    while !node.is_null() {
        if (*node).element_type() == XmlElementType::XmlDocumentNode
            || (*node).element_type() == XmlElementType::XmlHTMLDocumentNode
        {
            writeln!((*ctxt).output, "/");
        } else if (*node).element_type() == XmlElementType::XmlElementNode {
            for _ in 0..indent {
                write!((*ctxt).output, "  ");
            }
            if !(*node).ns.is_null() && !(*(*node).ns).prefix.is_null() {
                let prefix = CStr::from_ptr((*(*node).ns).prefix as *const i8).to_string_lossy();
                write!((*ctxt).output, "{prefix}:");
            }
            let name = CStr::from_ptr((*node).name as *const i8).to_string_lossy();
            writeln!((*ctxt).output, "{name}");
        }

        // Browse the full subtree, deep first
        if (*node).element_type() == XmlElementType::XmlDocumentNode
            || (*node).element_type() == XmlElementType::XmlHTMLDocumentNode
        {
            node = (*node)
                .as_document_node()
                .unwrap()
                .as_ref()
                .children
                .map_or(null_mut(), |c| c.as_ptr());
        } else if let Some(children) = (*node)
            .children()
            .filter(|_| (*node).element_type() != XmlElementType::XmlEntityRefNode)
        {
            // deep first
            node = children.as_ptr();
            indent += 1;
        } else if let Some(next) = (*node).next.filter(|_| node != tree) {
            // then siblings
            node = next.as_ptr();
        } else if node != tree {
            // go up to parents->next if needed
            while node != tree {
                if let Some(parent) = (*node).parent() {
                    node = parent.as_ptr();
                    indent -= 1;
                }
                if let Some(next) = (*node).next.filter(|_| node != tree) {
                    node = next.as_ptr();
                    break;
                }
                if (*node).parent().is_none() {
                    node = null_mut();
                    break;
                }
                if node == tree {
                    node = null_mut();
                    break;
                }
            }
            /* exit condition */
            if node == tree {
                node = null_mut();
            }
        } else {
            node = null_mut();
        }
    }
    0
}

/// Implements the XML shell function "pwd"
/// Show the full path from the root to the node, if needed building
/// thumblers when similar elements exists at a given ancestor level.
/// The output is compatible with XPath commands.
///
/// Returns 0 or -1 in case of error
#[doc(alias = "xmlShellPwd")]
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_shell_pwd(
    _ctxt: XmlShellCtxtPtr,
    buffer: *mut c_char,
    node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    use libc::snprintf;

    if node.is_null() || buffer.is_null() {
        return -1;
    }

    let Some(path) = (*node).get_node_path() else {
        return -1;
    };

    // This test prevents buffer overflow, because this routine
    // is only called by xmlShell, in which the second argument is
    // 500 chars long.
    // It is a dirty hack before a cleaner solution is found.
    // Documentation should mention that the second argument must
    // be at least 500 chars long, and could be stripped if too long.
    std::ptr::copy_nonoverlapping(path.as_ptr() as *const i8, buffer, path.len().min(499));
    snprintf(buffer as _, 499, c"%s".as_ptr(), path);
    *buffer.add(499) = b'0' as _;
    0
}

/// Implements the XML shell function "relaxng"
/// validating the instance against a Relax-NG schemas
///
/// Returns 0
#[doc(alias = "xmlShellRNGValidate")]
#[cfg(feature = "schema")]
unsafe extern "C" fn xml_shell_rng_validate(
    sctxt: XmlShellCtxtPtr,
    schemas: *mut c_char,
    _node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    use crate::{
        globals::GLOBAL_STATE,
        libxml::relaxng::{
            xml_relaxng_free, xml_relaxng_free_parser_ctxt, xml_relaxng_free_valid_ctxt,
            xml_relaxng_new_parser_ctxt, xml_relaxng_new_valid_ctxt, xml_relaxng_parse,
            xml_relaxng_set_parser_errors, xml_relaxng_set_valid_errors, xml_relaxng_validate_doc,
            XmlRelaxNGParserCtxtPtr, XmlRelaxNGValidCtxtPtr,
        },
    };

    use super::relaxng::XmlRelaxNGPtr;

    let ctxt: XmlRelaxNGParserCtxtPtr = xml_relaxng_new_parser_ctxt(schemas);
    let generic_error = GLOBAL_STATE.with_borrow(|state| state.generic_error);
    xml_relaxng_set_parser_errors(ctxt, Some(generic_error), Some(generic_error), None);
    let relaxngschemas: XmlRelaxNGPtr = xml_relaxng_parse(ctxt);
    xml_relaxng_free_parser_ctxt(ctxt);
    if relaxngschemas.is_null() {
        generic_error!(
            "Relax-NG schema {} failed to compile\n",
            CStr::from_ptr(schemas as *const i8).to_string_lossy()
        );
        return -1;
    }
    let vctxt: XmlRelaxNGValidCtxtPtr = xml_relaxng_new_valid_ctxt(relaxngschemas);
    xml_relaxng_set_valid_errors(vctxt, Some(generic_error), Some(generic_error), None);
    let ret: i32 = xml_relaxng_validate_doc(vctxt, (*sctxt).doc);

    match ret.cmp(&0) {
        std::cmp::Ordering::Equal => {
            let filename = CStr::from_ptr((*sctxt).filename).to_string_lossy();
            eprintln!("{filename} validates");
        }
        std::cmp::Ordering::Greater => {
            let filename = CStr::from_ptr((*sctxt).filename).to_string_lossy();
            eprintln!("{filename} fails to validate");
        }
        std::cmp::Ordering::Less => {
            let filename = CStr::from_ptr((*sctxt).filename).to_string_lossy();
            eprintln!("{filename} validation generated an internal error");
        }
    }
    xml_relaxng_free_valid_ctxt(vctxt);
    if !relaxngschemas.is_null() {
        xml_relaxng_free(relaxngschemas);
    }
    0
}

/// Implements the XML shell function "grep"
/// dumps information about the node (namespace, attributes, content).
///
/// Returns 0
#[doc(alias = "xmlShellGrep")]
unsafe extern "C" fn xml_shell_grep(
    ctxt: XmlShellCtxtPtr,
    arg: *mut c_char,
    mut node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    if ctxt.is_null() {
        return 0;
    }
    if node.is_null() {
        return 0;
    }
    if arg.is_null() {
        return 0;
    }
    // what does the following do... ?
    // #[cfg(feature = "regexp")]
    // if !xmlStrchr(arg as *mut xmlChar, b'?').is_null()
    //     || !xmlStrchr(arg as *mut xmlChar, b'*').is_null()
    //     || !xmlStrchr(arg as *mut xmlChar, b'.').is_null()
    //     || !xmlStrchr(arg as *mut xmlChar, b'[').is_null()
    // {}
    while !node.is_null() {
        if (*node).element_type() == XmlElementType::XmlCommentNode {
            if !xml_strstr((*node).content, arg as *mut XmlChar).is_null() {
                let path = (*node).get_node_path().unwrap();
                write!((*ctxt).output, "{path} : ");
                xml_shell_list(ctxt, null_mut(), node, null_mut());
            }
        } else if (*node).element_type() == XmlElementType::XmlTextNode
            && !xml_strstr((*node).content, arg as *mut XmlChar).is_null()
        {
            let path = (*node).parent().unwrap().get_node_path().unwrap();
            write!((*ctxt).output, "{path} : ");
            xml_shell_list(
                ctxt,
                null_mut(),
                (*node).parent().map_or(null_mut(), |p| p.as_ptr()),
                null_mut(),
            );
        }

        // Browse the full subtree, deep first
        if (*node).element_type() == XmlElementType::XmlDocumentNode
            || (*node).element_type() == XmlElementType::XmlHTMLDocumentNode
        {
            node = (*node)
                .as_document_node()
                .unwrap()
                .as_ref()
                .children
                .map_or(null_mut(), |c| c.as_ptr());
        } else if let Some(children) = (*node)
            .children()
            .filter(|_| (*node).element_type() != XmlElementType::XmlEntityRefNode)
        {
            // deep first
            node = children.as_ptr();
        } else if let Some(next) = (*node).next {
            // then siblings
            node = next.as_ptr();
        } else {
            // go up to parents->next if needed
            while !node.is_null() {
                if let Some(parent) = (*node).parent() {
                    node = parent.as_ptr();
                }
                if let Some(next) = (*node).next {
                    node = next.as_ptr();
                    break;
                }
                if (*node).parent().is_none() {
                    node = null_mut();
                    break;
                }
            }
        }
    }
    0
}

/// Implements the XML shell function "dir"
/// dumps information about the node (namespace, attributes, content).
///
/// Returns 0
#[doc(alias = "xmlShellSetContent")]
unsafe extern "C" fn xml_shell_set_content(
    ctxt: XmlShellCtxtPtr,
    value: *mut c_char,
    node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    let mut results: XmlNodePtr = null_mut();

    if ctxt.is_null() {
        return 0;
    }
    if node.is_null() {
        writeln!((*ctxt).output, "NULL");
        return 0;
    }
    if value.is_null() {
        writeln!((*ctxt).output, "NULL");
        return 0;
    }

    let ret = xml_parse_in_node_context(
        node,
        CStr::from_ptr(value).to_bytes().to_vec(),
        0,
        addr_of_mut!(results),
    );
    if ret == XmlParserErrors::XmlErrOK {
        if let Some(children) = (*node).children() {
            xml_free_node_list(children.as_ptr());
            (*node).set_children(None);
            (*node).set_last(None);
        }
        (*node).add_child_list(results);
    } else {
        writeln!((*ctxt).output, "failed to parse content");
    }
    0
}

/// Implements the XML shell function "setns"
/// register/unregister a prefix=namespace pair on the XPath context
///
/// Returns 0 on success and a negative value otherwise.
#[doc(alias = "xmlShellRegisterNamespace")]
#[cfg(feature = "xpath")]
unsafe extern "C" fn xml_shell_register_namespace(
    ctxt: XmlShellCtxtPtr,
    arg: *mut c_char,
    _node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    use crate::libxml::{xmlstring::xml_strdup, xpath_internals::xml_xpath_register_ns};

    use super::globals::xml_free;

    let mut prefix: *mut XmlChar;
    let mut href: *mut XmlChar;
    let mut next: *mut XmlChar;

    let ns_list_dup: *mut XmlChar = xml_strdup(arg as *mut XmlChar);
    next = ns_list_dup;
    while !next.is_null() {
        /* skip spaces */
        /*while ((*next) == b' ') next++;*/
        if (*next) == b'\0' {
            break;
        }

        /* find prefix */
        prefix = next;
        next = xml_strchr(next, b'=') as *mut XmlChar;
        if next.is_null() {
            writeln!((*ctxt).output, "setns: prefix=[nsuri] required");
            xml_free(ns_list_dup as _);
            return -1;
        }
        *next = b'\0';
        next = next.add(1);

        /* find href */
        href = next;
        next = xml_strchr(next, b' ') as *mut XmlChar;
        if !next.is_null() {
            *next = b'\0';
            next = next.add(1);
        }

        /* do register namespace */
        if xml_xpath_register_ns((*ctxt).pctxt, prefix, href) != 0 {
            let prefix = CStr::from_ptr(prefix as *const i8).to_string_lossy();
            let href = CStr::from_ptr(href as *const i8).to_string_lossy();
            writeln!(
                (*ctxt).output,
                "Error: unable to register NS with prefix=\"{prefix}\" and href=\"{href}\""
            );
            xml_free(ns_list_dup as _);
            return -1;
        }
    }

    xml_free(ns_list_dup as _);
    0
}

/// Implements the XML shell function "setrootns"
/// which registers all namespaces declarations found on the root element.
///
/// Returns 0 on success and a negative value otherwise.
#[doc(alias = "xmlShellRegisterRootNamespaces")]
#[cfg(feature = "xpath")]
unsafe extern "C" fn xml_shell_register_root_namespaces(
    ctxt: XmlShellCtxtPtr,
    _arg: *mut c_char,
    root: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    use super::xpath_internals::xml_xpath_register_ns;

    let mut ns: XmlNsPtr;

    if root.is_null()
        || (*root).element_type() != XmlElementType::XmlElementNode
        || (*root).ns_def.is_null()
        || ctxt.is_null()
        || (*ctxt).pctxt.is_null()
    {
        return -1;
    }
    ns = (*root).ns_def;
    while !ns.is_null() {
        if (*ns).prefix.is_null() {
            xml_xpath_register_ns((*ctxt).pctxt, c"defaultns".as_ptr() as _, (*ns).href);
        } else {
            xml_xpath_register_ns((*ctxt).pctxt, (*ns).prefix, (*ns).href);
        }
        ns = (*ns).next;
    }
    0
}

/// Implements the XML shell function "setbase"
/// change the current XML base of the node
///
/// Returns 0
#[doc(alias = "xmlShellSetBase")]
#[cfg(feature = "libxml_tree")]
unsafe extern "C" fn xml_shell_set_base(
    _ctxt: XmlShellCtxtPtr,
    arg: *mut c_char,
    node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    if node.is_null() {
        return 0;
    }
    if arg.is_null() {
        (*node).set_base(None);
    } else {
        (*node).set_base(Some(
            CStr::from_ptr(arg as *const i8).to_string_lossy().as_ref(),
        ));
    }
    0
}

/// Implements the XML shell
/// This allow to load, validate, view, modify and save a document
/// using a environment similar to a UNIX commandline.
#[doc(alias = "xmlShell")]
#[cfg(feature = "xpath")]
pub unsafe fn xml_shell<'a>(
    doc: XmlDocPtr,
    filename: *mut c_char,
    input: Option<XmlShellReadlineFunc>,
    output: Option<impl Write + 'a>,
) {
    use std::mem::size_of;

    use libc::{free, snprintf, sscanf, strcmp};

    use crate::{
        libxml::{
            globals::{xml_free, xml_malloc},
            xmlmemory::xml_mem_show,
            xmlstring::xml_strdup,
            xpath::{
                xml_xpath_eval, xml_xpath_free_context, xml_xpath_free_object,
                xml_xpath_new_context,
            },
            xpath_internals::xml_xpath_debug_dump_object,
        },
        tree::xml_free_doc,
    };

    let mut prompt: [u8; 500] = [0; 500];
    prompt[..c"/ > ".to_bytes().len()].copy_from_slice(c"/ > ".to_bytes());
    let mut cmdline: *mut c_char;
    let mut cur: *mut c_char;
    let mut command: [c_char; 100] = [0; 100];
    let mut arg: [c_char; 400] = [0; 400];
    let mut i: i32;
    let mut list: XmlXPathObjectPtr;

    if doc.is_null() {
        return;
    }
    if filename.is_null() {
        return;
    }
    if input.is_none() {
        return;
    }

    let ctxt: XmlShellCtxtPtr = xml_malloc(size_of::<XmlShellCtxt>()) as XmlShellCtxtPtr;
    if ctxt.is_null() {
        return;
    }
    (*ctxt).loaded = 0;
    (*ctxt).doc = doc;
    (*ctxt).input = input.unwrap();
    (*ctxt).output = output.map_or(Box::new(stdout()) as Box<dyn Write + 'a>, |o| Box::new(o));
    (*ctxt).filename = xml_strdup(filename as *mut XmlChar) as *mut c_char;
    (*ctxt).node = (*ctxt).doc as XmlNodePtr;

    #[cfg(feature = "xpath")]
    {
        (*ctxt).pctxt = xml_xpath_new_context((*ctxt).doc);
        if (*ctxt).pctxt.is_null() {
            xml_free(ctxt as _);
            return;
        }
    }
    loop {
        if (*ctxt).node == (*ctxt).doc as XmlNodePtr {
            snprintf(
                prompt.as_mut_ptr() as _,
                prompt.len(),
                c"%s > ".as_ptr(),
                c"/".as_ptr(),
            );
        } else if !(*ctxt).node.is_null()
            && !(*(*ctxt).node).name.is_null()
            && !(*(*ctxt).node).ns.is_null()
            && !(*(*(*ctxt).node).ns).prefix.is_null()
        {
            snprintf(
                prompt.as_mut_ptr() as _,
                prompt.len(),
                c"%s:%s > ".as_ptr(),
                (*(*(*ctxt).node).ns).prefix,
                (*(*ctxt).node).name,
            );
        } else if !(*ctxt).node.is_null() && !(*(*ctxt).node).name.is_null() {
            snprintf(
                prompt.as_mut_ptr() as _,
                prompt.len(),
                c"%s > ".as_ptr(),
                (*(*ctxt).node).name,
            );
        } else {
            snprintf(prompt.as_mut_ptr() as _, prompt.len(), c"? > ".as_ptr());
        }
        prompt[prompt.len() - 1] = 0;

        /*
         * Get a new command line
         */
        cmdline = ((*ctxt).input)(prompt.as_mut_ptr() as _);
        if cmdline.is_null() {
            break;
        }

        /*
         * Parse the command itself
         */
        cur = cmdline;
        while *cur == b' ' as i8 || *cur == b'\t' as i8 {
            cur = cur.add(1);
        }
        i = 0;
        while *cur != b' ' as i8
            && *cur != b'\t' as i8
            && *cur != b'\n' as i8
            && *cur != b'\r' as i8
        {
            if *cur == 0 {
                break;
            }
            command[i as usize] = *cur;
            i += 1;
            cur = cur.add(1);
        }
        command[i as usize] = 0;
        if i == 0 {
            continue;
        }

        /*
         * Parse the argument
         */
        while *cur == b' ' as i8 || *cur == b'\t' as i8 {
            cur = cur.add(1);
        }
        i = 0;
        while *cur != b'\n' as i8 && *cur != b'\r' as i8 && *cur != 0 {
            if *cur == 0 {
                break;
            }
            arg[i as usize] = *cur;
            i += 1;
            cur = cur.add(1);
        }
        arg[i as usize] = 0;

        /*
         * start interpreting the command
         */
        if strcmp(command.as_mut_ptr(), c"exit".as_ptr()) == 0 {
            break;
        }
        if strcmp(command.as_mut_ptr(), c"quit".as_ptr()) == 0 {
            break;
        }
        if strcmp(command.as_mut_ptr(), c"bye".as_ptr()) == 0 {
            break;
        }
        if strcmp(command.as_mut_ptr(), c"help".as_ptr()) == 0 {
            writeln!(
                (*ctxt).output,
                "\tbase         display XML base of the node",
            );
            writeln!(
                (*ctxt).output,
                "\tsetbase URI  change the XML base of the node"
            );
            writeln!((*ctxt).output, "\tbye          leave shell");
            writeln!(
                (*ctxt).output,
                "\tcat [node]   display node or current node"
            );
            writeln!(
                (*ctxt).output,
                "\tcd [path]    change directory to path or to root"
            );
            writeln!(
                (*ctxt).output,
                "\tdir [path]   dumps information about the node (namespace, attributes, content)"
            );
            writeln!(
                (*ctxt).output,
                "\tdu [path]    show the structure of the subtree under path or the current node"
            );
            writeln!((*ctxt).output, "\texit         leave shell");
            writeln!((*ctxt).output, "\thelp         display this help");
            writeln!((*ctxt).output, "\tfree         display memory usage");
            writeln!(
                (*ctxt).output,
                "\tload [name]  load a new document with name"
            );
            writeln!(
                (*ctxt).output,
                "\tls [path]    list contents of path or the current directory"
            );
            writeln!((*ctxt).output, "\tset xml_fragment replace the current node content with the fragment parsed in context");
            #[cfg(feature = "xpath")]
            {
                writeln!((*ctxt).output, "\txpath expr   evaluate the XPath expression in that context and print the result");
                writeln!((*ctxt).output, "\tsetns nsreg  register a namespace to a prefix in the XPath evaluation context");
                writeln!((*ctxt).output, "\t             format for nsreg is: prefix=[nsuri] (i.e. prefix= unsets a prefix)");
                writeln!(
                    (*ctxt).output,
                    "\tsetrootns    register all namespace found on the root element"
                );
                writeln!(
                    (*ctxt).output,
                    "\t             the default namespace if any uses 'defaultns' prefix"
                );
            }
            writeln!(
                (*ctxt).output,
                "\tpwd          display current working directory"
            );
            writeln!(
                (*ctxt).output,
                "\twhereis      display absolute path of [path] or current working directory"
            );
            writeln!((*ctxt).output, "\tquit         leave shell");
            #[cfg(feature = "libxml_output")]
            {
                writeln!(
                    (*ctxt).output,
                    "\tsave [name]  save this document to name or the original name"
                );
                writeln!(
                    (*ctxt).output,
                    "\twrite [name] write the current node to the filename"
                );
            }
            #[cfg(feature = "libxml_valid")]
            {
                writeln!(
                    (*ctxt).output,
                    "\tvalidate     check the document for errors"
                );
            }
            #[cfg(feature = "schema")]
            {
                writeln!(
                    (*ctxt).output,
                    "\trelaxng rng  validate the document against the Relax-NG schemas"
                );
            }
            writeln!(
                (*ctxt).output,
                "\tgrep string  search for a string in the subtree"
            );
        } else if {
            #[cfg(feature = "libxml_valid")]
            {
                strcmp(command.as_ptr(), c"validate".as_ptr()) == 0
            }
            #[cfg(not(feature = "libxml_valid"))]
            {
                false
            }
        } {
            #[cfg(feature = "libxml_valid")]
            {
                xml_shell_validate(ctxt, arg.as_mut_ptr(), null_mut(), null_mut());
            }
        } else if strcmp(command.as_ptr(), c"load".as_ptr()) == 0 {
            xml_shell_load(ctxt, arg.as_mut_ptr(), null_mut(), null_mut());
        } else if {
            #[cfg(feature = "schema")]
            {
                strcmp(command.as_ptr(), c"relaxng".as_ptr()) == 0
            }
            #[cfg(not(feature = "schema"))]
            {
                false
            }
        } {
            #[cfg(feature = "schema")]
            {
                xml_shell_rng_validate(ctxt, arg.as_mut_ptr(), null_mut(), null_mut());
            }
        } else if {
            #[cfg(feature = "libxml_output")]
            {
                strcmp(command.as_ptr(), c"save".as_ptr()) == 0
            }
            #[cfg(not(feature = "libxml_output"))]
            {
                false
            }
        } {
            #[cfg(feature = "libxml_output")]
            {
                xml_shell_save(ctxt, arg.as_mut_ptr(), null_mut(), null_mut());
            }
        } else if {
            #[cfg(feature = "libxml_output")]
            {
                strcmp(command.as_ptr(), c"write".as_ptr()) == 0
            }
            #[cfg(not(feature = "libxml_output"))]
            {
                false
            }
        } {
            #[cfg(feature = "libxml_output")]
            if arg[0] == 0 {
                generic_error!("Write command requires a filename argument\n");
            } else {
                xml_shell_write(
                    ctxt,
                    CStr::from_ptr(arg.as_ptr()).to_string_lossy().as_ref(),
                    (*ctxt).node,
                    null_mut(),
                );
            }
        } else if strcmp(command.as_ptr(), c"grep".as_ptr()) == 0 {
            xml_shell_grep(ctxt, arg.as_mut_ptr(), (*ctxt).node, null_mut());
        } else if strcmp(command.as_ptr(), c"free".as_ptr()) == 0 {
            if arg[0] == 0 {
                xml_mem_show(&mut (*ctxt).output, 0);
            } else {
                let mut len: i32 = 0;

                sscanf(arg.as_mut_ptr(), c"%d".as_ptr(), addr_of_mut!(len));
                xml_mem_show(&mut (*ctxt).output, len);
            }
        } else if strcmp(command.as_ptr(), c"pwd".as_ptr()) == 0 {
            let mut dir: [c_char; 500] = [0; 500];

            if xml_shell_pwd(ctxt, dir.as_mut_ptr(), (*ctxt).node, null_mut()) == 0 {
                let dir = CStr::from_ptr(dir.as_ptr()).to_string_lossy();
                writeln!((*ctxt).output, "{dir}");
            }
        } else if strcmp(command.as_ptr(), c"du".as_ptr()) == 0 {
            if arg[0] == 0 {
                xml_shell_du(ctxt, null_mut(), (*ctxt).node, null_mut());
            } else {
                (*(*ctxt).pctxt).node = (*ctxt).node;
                #[cfg(feature = "xpath")]
                {
                    (*(*ctxt).pctxt).node = (*ctxt).node;
                    list = xml_xpath_eval(arg.as_mut_ptr() as *mut XmlChar, (*ctxt).pctxt);
                }
                #[cfg(not(feature = "xpath"))]
                {
                    list = null_mut();
                }
                if !list.is_null() {
                    match (*list).typ {
                        XmlXPathObjectType::XpathUndefined => {
                            generic_error!(
                                "{}: no such node\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathNodeset => {
                            if !(*list).nodesetval.is_null() {
                                if let Some(table) = (*(*list).nodesetval).node_tab.as_deref() {
                                    for &node in table {
                                        xml_shell_du(ctxt, null_mut(), node, null_mut());
                                    }
                                }
                            }
                        }
                        XmlXPathObjectType::XpathBoolean => {
                            generic_error!(
                                "{} is a Boolean\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathNumber => {
                            generic_error!(
                                "{} is a number\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathString => {
                            generic_error!(
                                "{} is a string\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathPoint => {
                            generic_error!(
                                "{} is a point\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathRange => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathLocationset => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathUsers => {
                            generic_error!(
                                "{} is user-defined\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathXsltTree => {
                            generic_error!(
                                "{} is an XSLT value tree\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                    }
                    #[cfg(feature = "xpath")]
                    {
                        xml_xpath_free_object(list);
                    }
                } else {
                    generic_error!(
                        "{}: no such node\n",
                        CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                    );
                }
                (*(*ctxt).pctxt).node = null_mut();
            }
        } else if strcmp(command.as_ptr(), c"base".as_ptr()) == 0 {
            xml_shell_base(ctxt, null_mut(), (*ctxt).node, null_mut());
        } else if strcmp(command.as_ptr(), c"set".as_ptr()) == 0 {
            xml_shell_set_content(ctxt, arg.as_mut_ptr(), (*ctxt).node, null_mut());
        } else if {
            #[cfg(feature = "xpath")]
            {
                strcmp(command.as_ptr(), c"setns".as_ptr()) == 0
            }
            #[cfg(not(feature = "xpath"))]
            {
                false
            }
        } {
            #[cfg(feature = "xpath")]
            if arg[0] == 0 {
                generic_error!("setns: prefix=[nsuri] required\n");
            } else {
                xml_shell_register_namespace(ctxt, arg.as_mut_ptr(), null_mut(), null_mut());
            }
        } else if {
            #[cfg(feature = "xpath")]
            {
                strcmp(command.as_ptr(), c"setrootns".as_ptr()) == 0
            }
            #[cfg(not(feature = "xpath"))]
            {
                false
            }
        } {
            #[cfg(feature = "xpath")]
            {
                let root: XmlNodePtr = (*(*ctxt).doc).get_root_element();
                xml_shell_register_root_namespaces(ctxt, null_mut(), root, null_mut());
            }
        } else if {
            #[cfg(feature = "xpath")]
            {
                strcmp(command.as_ptr(), c"xpath".as_ptr()) == 0
            }
            #[cfg(not(feature = "xpath"))]
            {
                false
            }
        } {
            #[cfg(feature = "xpath")]
            if arg[0] == 0 {
                generic_error!("xpath: expression required\n");
            } else {
                (*(*ctxt).pctxt).node = (*ctxt).node;
                list = xml_xpath_eval(arg.as_mut_ptr() as *mut XmlChar, (*ctxt).pctxt);
                xml_xpath_debug_dump_object(&mut (*ctxt).output, list, 0);
                xml_xpath_free_object(list);
            }
        } else if {
            #[cfg(feature = "libxml_tree")]
            {
                strcmp(command.as_ptr(), c"setbase".as_ptr()) == 0
            }
            #[cfg(not(feature = "libxml_tree"))]
            {
                false
            }
        } {
            #[cfg(feature = "libxml_tree")]
            {
                xml_shell_set_base(ctxt, arg.as_mut_ptr(), (*ctxt).node, null_mut());
            }
        } else if strcmp(command.as_ptr(), c"ls".as_ptr()) == 0
            || strcmp(command.as_ptr(), c"dir".as_ptr()) == 0
        {
            let dir: i32 = (strcmp(command.as_ptr(), c"dir".as_ptr()) == 0) as i32;

            if arg[0] == 0 {
                if dir != 0 {
                    xml_shell_dir(ctxt, null_mut(), (*ctxt).node, null_mut());
                } else {
                    xml_shell_list(ctxt, null_mut(), (*ctxt).node, null_mut());
                }
            } else {
                (*(*ctxt).pctxt).node = (*ctxt).node;
                #[cfg(feature = "xpath")]
                {
                    (*(*ctxt).pctxt).node = (*ctxt).node;
                    list = xml_xpath_eval(arg.as_mut_ptr() as *mut XmlChar, (*ctxt).pctxt);
                }
                #[cfg(not(feature = "xpath"))]
                {
                    list = null_mut();
                }
                if !list.is_null() {
                    match (*list).typ {
                        XmlXPathObjectType::XpathUndefined => {
                            generic_error!(
                                "{}: no such node\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathNodeset => {
                            if (*list).nodesetval.is_null() {
                                break;
                            }

                            if let Some(table) = (*(*list).nodesetval).node_tab.as_deref() {
                                for &node in table {
                                    if dir != 0 {
                                        xml_shell_dir(ctxt, null_mut(), node, null_mut());
                                    } else {
                                        xml_shell_list(ctxt, null_mut(), node, null_mut());
                                    }
                                }
                            }
                        }
                        XmlXPathObjectType::XpathBoolean => {
                            generic_error!(
                                "{} is a Boolean\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathNumber => {
                            generic_error!(
                                "{} is a number\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathString => {
                            generic_error!(
                                "{} is a string\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathPoint => {
                            generic_error!(
                                "{} is a point\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathRange => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathLocationset => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathUsers => {
                            generic_error!(
                                "{} is user-defined\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathXsltTree => {
                            generic_error!(
                                "{} is an XSLT value tree\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                    }
                    #[cfg(feature = "xpath")]
                    {
                        xml_xpath_free_object(list);
                    }
                } else {
                    generic_error!(
                        "{}: no such node\n",
                        CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                    );
                }
                (*(*ctxt).pctxt).node = null_mut();
            }
        } else if strcmp(command.as_ptr(), c"whereis".as_ptr()) == 0 {
            let mut dir: [c_char; 500] = [0; 500];

            if arg[0] == 0 {
                if xml_shell_pwd(ctxt, dir.as_mut_ptr(), (*ctxt).node, null_mut()) == 0 {
                    let dir = CStr::from_ptr(dir.as_ptr()).to_string_lossy();
                    writeln!((*ctxt).output, "{dir}");
                }
            } else {
                (*(*ctxt).pctxt).node = (*ctxt).node;
                #[cfg(feature = "xpath")]
                {
                    list = xml_xpath_eval(arg.as_mut_ptr() as *mut XmlChar, (*ctxt).pctxt);
                }
                #[cfg(not(feature = "xpath"))]
                {
                    list = null_mut();
                }
                if !list.is_null() {
                    match (*list).typ {
                        XmlXPathObjectType::XpathUndefined => {
                            generic_error!(
                                "{}: no such node\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathNodeset => {
                            if !(*list).nodesetval.is_null() {
                                if let Some(table) = (*(*list).nodesetval).node_tab.as_deref() {
                                    for &node in table {
                                        if xml_shell_pwd(ctxt, dir.as_mut_ptr(), node, null_mut())
                                            == 0
                                        {
                                            let dir =
                                                CStr::from_ptr(dir.as_ptr()).to_string_lossy();
                                            writeln!((*ctxt).output, "{dir}");
                                        }
                                    }
                                }
                            }
                        }
                        XmlXPathObjectType::XpathBoolean => {
                            generic_error!(
                                "{} is a Boolean\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathNumber => {
                            generic_error!(
                                "{} is a number\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathString => {
                            generic_error!(
                                "{} is a string\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathPoint => {
                            generic_error!(
                                "{} is a point\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathRange => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathLocationset => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathUsers => {
                            generic_error!(
                                "{} is user-defined\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathXsltTree => {
                            generic_error!(
                                "{} is an XSLT value tree\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                    }
                    #[cfg(feature = "xpath")]
                    {
                        xml_xpath_free_object(list);
                    }
                } else {
                    generic_error!(
                        "{}: no such node\n",
                        CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                    );
                }
                (*(*ctxt).pctxt).node = null_mut();
            }
        } else if strcmp(command.as_ptr(), c"cd".as_ptr()) == 0 {
            if arg[0] == 0 {
                (*ctxt).node = (*ctxt).doc as XmlNodePtr;
            } else {
                #[cfg(feature = "xpath")]
                {
                    (*(*ctxt).pctxt).node = (*ctxt).node;
                    let l = strlen(arg.as_ptr());
                    if l >= 2 && arg[l - 1] == b'/' as _ {
                        arg[l - 1] = 0;
                    }
                    list = xml_xpath_eval(arg.as_mut_ptr() as *mut XmlChar, (*ctxt).pctxt);
                }
                #[cfg(not(feature = "xpath"))]
                {
                    list = null_mut();
                }
                if !list.is_null() {
                    match (*list).typ {
                        XmlXPathObjectType::XpathUndefined => {
                            generic_error!(
                                "{}: no such node\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathNodeset => {
                            if !(*list).nodesetval.is_null() {
                                if let Some(table) = (*(*list).nodesetval)
                                    .node_tab
                                    .as_deref()
                                    .filter(|t| t.len() == 1)
                                {
                                    (*ctxt).node = table[0];
                                    if !(*ctxt).node.is_null()
                                        && ((*(*ctxt).node).element_type()
                                            == XmlElementType::XmlNamespaceDecl)
                                    {
                                        generic_error!("cannot cd to namespace\n");
                                        (*ctxt).node = null_mut();
                                    }
                                } else {
                                    generic_error!(
                                        "{} is a {} Node Set\n",
                                        CStr::from_ptr(arg.as_ptr()).to_string_lossy(),
                                        (*(*list).nodesetval)
                                            .node_tab
                                            .as_ref()
                                            .map_or(0, |t| t.len())
                                    );
                                }
                            } else {
                                generic_error!(
                                    "{} is an empty Node Set\n",
                                    CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                                );
                            }
                        }
                        XmlXPathObjectType::XpathBoolean => {
                            generic_error!(
                                "{} is a Boolean\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathNumber => {
                            generic_error!(
                                "{} is a number\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathString => {
                            generic_error!(
                                "{} is a string\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathPoint => {
                            generic_error!(
                                "{} is a point\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathRange => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathLocationset => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathUsers => {
                            generic_error!(
                                "{} is user-defined\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathXsltTree => {
                            generic_error!(
                                "{} is an XSLT value tree\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                    }
                    #[cfg(feature = "xpath")]
                    {
                        xml_xpath_free_object(list);
                    }
                } else {
                    generic_error!(
                        "{}: no such node\n",
                        CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                    );
                }

                (*(*ctxt).pctxt).node = null_mut();
            }
        } else if {
            #[cfg(feature = "libxml_output")]
            {
                strcmp(command.as_ptr(), c"cat".as_ptr()) == 0
            }
            #[cfg(not(feature = "libxml_output"))]
            {
                false
            }
        } {
            #[cfg(feature = "libxml_output")]
            if arg[0] == 0 {
                xml_shell_cat(ctxt, null_mut(), (*ctxt).node, null_mut());
            } else {
                (*(*ctxt).pctxt).node = (*ctxt).node;
                #[cfg(feature = "xpath")]
                {
                    (*(*ctxt).pctxt).node = (*ctxt).node;
                    list = xml_xpath_eval(arg.as_mut_ptr() as *mut XmlChar, (*ctxt).pctxt);
                }
                #[cfg(not(feature = "xpath"))]
                {
                    list = null_mut();
                }
                if !list.is_null() {
                    match (*list).typ {
                        XmlXPathObjectType::XpathUndefined => {
                            generic_error!(
                                "{}: no such node\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathNodeset => {
                            if !(*list).nodesetval.is_null() {
                                if let Some(table) = (*(*list).nodesetval).node_tab.as_deref() {
                                    for &node in table {
                                        if i > 0 {
                                            writeln!((*ctxt).output, " -------");
                                        }
                                        xml_shell_cat(ctxt, null_mut(), node, null_mut());
                                    }
                                }
                            }
                        }
                        XmlXPathObjectType::XpathBoolean => {
                            generic_error!(
                                "{} is a Boolean\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathNumber => {
                            generic_error!(
                                "{} is a number\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathString => {
                            generic_error!(
                                "{} is a string\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathPoint => {
                            generic_error!(
                                "{} is a point\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathRange => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathLocationset => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathUsers => {
                            generic_error!(
                                "{} is user-defined\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathXsltTree => {
                            generic_error!(
                                "{} is an XSLT value tree\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                    }
                    #[cfg(feature = "xpath")]
                    {
                        xml_xpath_free_object(list);
                    }
                } else {
                    generic_error!(
                        "{}: no such node\n",
                        CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                    );
                }
                (*(*ctxt).pctxt).node = null_mut();
            }
        } else {
            generic_error!(
                "Unknown command {}\n",
                CStr::from_ptr(command.as_ptr()).to_string_lossy()
            );
        }
        free(cmdline as _); /* not xmlFree here ! */
        // cmdline = null_mut();
    }
    #[cfg(feature = "xpath")]
    {
        xml_xpath_free_context((*ctxt).pctxt);
    }
    if (*ctxt).loaded != 0 {
        xml_free_doc((*ctxt).doc);
    }
    if !(*ctxt).filename.is_null() {
        xml_free((*ctxt).filename as _);
    }
    xml_free(ctxt as _);
    if !cmdline.is_null() {
        free(cmdline as _); /* not xmlFree here ! */
    }
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_bool_to_text() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_boolval in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let boolval = gen_int(n_boolval, 0);

                let ret_val = xml_bool_to_text(boolval);
                desret_const_char_ptr(ret_val);
                des_int(n_boolval, boolval, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlBoolToText",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlBoolToText()");
                    eprintln!(" {}", n_boolval);
                }
            }
        }
    }

    #[test]
    fn test_xml_debug_check_document() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_DEBUG_FILE_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    let mem_base = xml_mem_blocks();
                    let output = gen_debug_file_ptr(n_output, 0);
                    let doc = gen_xml_doc_ptr(n_doc, 1);

                    let ret_val = xml_debug_check_document(output, doc);
                    desret_int(ret_val);
                    des_xml_doc_ptr(n_doc, doc, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlDebugCheckDocument",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlDebugCheckDocument()"
                        );
                        eprint!(" {}", n_output);
                        eprintln!(" {}", n_doc);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_debug_dump_attr() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_DEBUG_FILE_PTR {
                for n_attr in 0..GEN_NB_XML_ATTR_PTR {
                    for n_depth in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let mut output = gen_debug_file_ptr(n_output, 0).unwrap();
                        let attr = gen_xml_attr_ptr(n_attr, 1);
                        let depth = gen_int(n_depth, 2);

                        xml_debug_dump_attr(&mut output, attr, depth);
                        des_xml_attr_ptr(n_attr, attr, 1);
                        des_int(n_depth, depth, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlDebugDumpAttr",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlDebugDumpAttr()");
                            eprint!(" {}", n_output);
                            eprint!(" {}", n_attr);
                            eprintln!(" {}", n_depth);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_debug_dump_attr_list() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_DEBUG_FILE_PTR {
                for n_attr in 0..GEN_NB_XML_ATTR_PTR {
                    for n_depth in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let mut output = gen_debug_file_ptr(n_output, 0).unwrap();
                        let attr = gen_xml_attr_ptr(n_attr, 1);
                        let depth = gen_int(n_depth, 2);

                        xml_debug_dump_attr_list(&mut output, attr, depth);
                        des_xml_attr_ptr(n_attr, attr, 1);
                        des_int(n_depth, depth, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlDebugDumpAttrList",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlDebugDumpAttrList()"
                            );
                            eprint!(" {}", n_output);
                            eprint!(" {}", n_attr);
                            eprintln!(" {}", n_depth);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_debug_dump_dtd() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_DEBUG_FILE_PTR {
                for n_dtd in 0..GEN_NB_XML_DTD_PTR {
                    let mem_base = xml_mem_blocks();
                    let output = gen_debug_file_ptr(n_output, 0);
                    let dtd = gen_xml_dtd_ptr(n_dtd, 1);

                    xml_debug_dump_dtd(output, dtd);
                    des_xml_dtd_ptr(n_dtd, dtd, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlDebugDumpDTD",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlDebugDumpDTD()");
                        eprint!(" {}", n_output);
                        eprintln!(" {}", n_dtd);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_debug_dump_document() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_DEBUG_FILE_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    let mem_base = xml_mem_blocks();
                    let output = gen_debug_file_ptr(n_output, 0);
                    let doc = gen_xml_doc_ptr(n_doc, 1);

                    xml_debug_dump_document(output, doc);
                    des_xml_doc_ptr(n_doc, doc, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlDebugDumpDocument",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlDebugDumpDocument()"
                        );
                        eprint!(" {}", n_output);
                        eprintln!(" {}", n_doc);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_debug_dump_document_head() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_DEBUG_FILE_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    let mem_base = xml_mem_blocks();
                    let output = gen_debug_file_ptr(n_output, 0);
                    let doc = gen_xml_doc_ptr(n_doc, 1);

                    xml_debug_dump_document_head(output, doc);
                    des_xml_doc_ptr(n_doc, doc, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlDebugDumpDocumentHead",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlDebugDumpDocumentHead()"
                        );
                        eprint!(" {}", n_output);
                        eprintln!(" {}", n_doc);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_debug_dump_entities() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_DEBUG_FILE_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    let mem_base = xml_mem_blocks();
                    let output = gen_debug_file_ptr(n_output, 0).unwrap();
                    let doc = gen_xml_doc_ptr(n_doc, 1);

                    xml_debug_dump_entities(output, doc);
                    des_xml_doc_ptr(n_doc, doc, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlDebugDumpEntities",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlDebugDumpEntities()"
                        );
                        eprint!(" {}", n_output);
                        eprintln!(" {}", n_doc);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_debug_dump_node() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_DEBUG_FILE_PTR {
                for n_node in 0..GEN_NB_XML_NODE_PTR {
                    for n_depth in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let output = gen_debug_file_ptr(n_output, 0);
                        let node = gen_xml_node_ptr(n_node, 1);
                        let depth = gen_int(n_depth, 2);

                        xml_debug_dump_node(output, node, depth);
                        des_xml_node_ptr(n_node, node, 1);
                        des_int(n_depth, depth, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlDebugDumpNode",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlDebugDumpNode()");
                            eprint!(" {}", n_output);
                            eprint!(" {}", n_node);
                            eprintln!(" {}", n_depth);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_debug_dump_node_list() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_DEBUG_FILE_PTR {
                for n_node in 0..GEN_NB_XML_NODE_PTR {
                    for n_depth in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let output = gen_debug_file_ptr(n_output, 0);
                        let node = gen_xml_node_ptr(n_node, 1);
                        let depth = gen_int(n_depth, 2);

                        xml_debug_dump_node_list(output, node, depth);
                        des_xml_node_ptr(n_node, node, 1);
                        des_int(n_depth, depth, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlDebugDumpNodeList",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlDebugDumpNodeList()"
                            );
                            eprint!(" {}", n_output);
                            eprint!(" {}", n_node);
                            eprintln!(" {}", n_depth);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_debug_dump_one_node() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_DEBUG_FILE_PTR {
                for n_node in 0..GEN_NB_XML_NODE_PTR {
                    for n_depth in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let mut output = gen_debug_file_ptr(n_output, 0).unwrap();
                        let node = gen_xml_node_ptr(n_node, 1);
                        let depth = gen_int(n_depth, 2);

                        xml_debug_dump_one_node(&mut output, node, depth);
                        des_xml_node_ptr(n_node, node, 1);
                        des_int(n_depth, depth, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlDebugDumpOneNode",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlDebugDumpOneNode()"
                            );
                            eprint!(" {}", n_output);
                            eprint!(" {}", n_node);
                            eprintln!(" {}", n_depth);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_debug_dump_string() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_DEBUG_FILE_PTR {
                for n_str in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let mut output = gen_debug_file_ptr(n_output, 0);
                    let str = gen_const_xml_char_ptr(n_str, 1);

                    xml_debug_dump_string(output.as_mut(), str);
                    des_const_xml_char_ptr(n_str, str, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlDebugDumpString",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlDebugDumpString()"
                        );
                        eprint!(" {}", n_output);
                        eprintln!(" {}", n_str);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_ls_count_node() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_node in 0..GEN_NB_XML_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let node = gen_xml_node_ptr(n_node, 0);

                let ret_val = xml_ls_count_node(node);
                desret_int(ret_val);
                des_xml_node_ptr(n_node, node, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlLsCountNode",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlLsCountNode()");
                    eprintln!(" {}", n_node);
                }
            }
        }
    }

    #[test]
    fn test_xml_ls_one_node() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_DEBUG_FILE_PTR {
                for n_node in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let mut output = gen_debug_file_ptr(n_output, 0).unwrap();
                    let node = gen_xml_node_ptr(n_node, 1);

                    xml_ls_one_node(&mut output, node);
                    des_xml_node_ptr(n_node, node, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlLsOneNode",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlLsOneNode()");
                        eprint!(" {}", n_output);
                        eprintln!(" {}", n_node);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_shell() {

        /* missing type support */
    }

    #[test]
    fn test_xml_shell_base() {
        #[cfg(all(feature = "libxml_debug", feature = "xpath"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SHELL_CTXT_PTR {
                for n_arg in 0..GEN_NB_CHAR_PTR {
                    for n_node in 0..GEN_NB_XML_NODE_PTR {
                        for n_node2 in 0..GEN_NB_XML_NODE_PTR {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_shell_ctxt_ptr(n_ctxt, 0);
                            let arg = gen_char_ptr(n_arg, 1);
                            let node = gen_xml_node_ptr(n_node, 2);
                            let node2 = gen_xml_node_ptr(n_node2, 3);

                            let ret_val = xml_shell_base(ctxt, arg, node, node2);
                            desret_int(ret_val);
                            des_xml_shell_ctxt_ptr(n_ctxt, ctxt, 0);
                            des_char_ptr(n_arg, arg, 1);
                            des_xml_node_ptr(n_node, node, 2);
                            des_xml_node_ptr(n_node2, node2, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlShellBase",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlShellBase()");
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_arg);
                                eprint!(" {}", n_node);
                                eprintln!(" {}", n_node2);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_shell_cat() {
        #[cfg(all(feature = "libxml_debug", feature = "xpath", feature = "libxml_output"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SHELL_CTXT_PTR {
                for n_arg in 0..GEN_NB_CHAR_PTR {
                    for n_node in 0..GEN_NB_XML_NODE_PTR {
                        for n_node2 in 0..GEN_NB_XML_NODE_PTR {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_shell_ctxt_ptr(n_ctxt, 0);
                            let arg = gen_char_ptr(n_arg, 1);
                            let node = gen_xml_node_ptr(n_node, 2);
                            let node2 = gen_xml_node_ptr(n_node2, 3);

                            let ret_val = xml_shell_cat(ctxt, arg, node, node2);
                            desret_int(ret_val);
                            des_xml_shell_ctxt_ptr(n_ctxt, ctxt, 0);
                            des_char_ptr(n_arg, arg, 1);
                            des_xml_node_ptr(n_node, node, 2);
                            des_xml_node_ptr(n_node2, node2, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlShellCat",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlShellCat()");
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_arg);
                                eprint!(" {}", n_node);
                                eprintln!(" {}", n_node2);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_shell_dir() {
        #[cfg(all(feature = "libxml_debug", feature = "xpath"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SHELL_CTXT_PTR {
                for n_arg in 0..GEN_NB_CHAR_PTR {
                    for n_node in 0..GEN_NB_XML_NODE_PTR {
                        for n_node2 in 0..GEN_NB_XML_NODE_PTR {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_shell_ctxt_ptr(n_ctxt, 0);
                            let arg = gen_char_ptr(n_arg, 1);
                            let node = gen_xml_node_ptr(n_node, 2);
                            let node2 = gen_xml_node_ptr(n_node2, 3);

                            let ret_val = xml_shell_dir(ctxt, arg, node, node2);
                            desret_int(ret_val);
                            des_xml_shell_ctxt_ptr(n_ctxt, ctxt, 0);
                            des_char_ptr(n_arg, arg, 1);
                            des_xml_node_ptr(n_node, node, 2);
                            des_xml_node_ptr(n_node2, node2, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlShellDir",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlShellDir()");
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_arg);
                                eprint!(" {}", n_node);
                                eprintln!(" {}", n_node2);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_shell_du() {
        #[cfg(all(feature = "libxml_debug", feature = "xpath"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SHELL_CTXT_PTR {
                for n_arg in 0..GEN_NB_CHAR_PTR {
                    for n_tree in 0..GEN_NB_XML_NODE_PTR {
                        for n_node2 in 0..GEN_NB_XML_NODE_PTR {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_shell_ctxt_ptr(n_ctxt, 0);
                            let arg = gen_char_ptr(n_arg, 1);
                            let tree = gen_xml_node_ptr(n_tree, 2);
                            let node2 = gen_xml_node_ptr(n_node2, 3);

                            let ret_val = xml_shell_du(ctxt, arg, tree, node2);
                            desret_int(ret_val);
                            des_xml_shell_ctxt_ptr(n_ctxt, ctxt, 0);
                            des_char_ptr(n_arg, arg, 1);
                            des_xml_node_ptr(n_tree, tree, 2);
                            des_xml_node_ptr(n_node2, node2, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlShellDu",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlShellDu()");
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_arg);
                                eprint!(" {}", n_tree);
                                eprintln!(" {}", n_node2);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_shell_list() {
        #[cfg(all(feature = "libxml_debug", feature = "xpath"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SHELL_CTXT_PTR {
                for n_arg in 0..GEN_NB_CHAR_PTR {
                    for n_node in 0..GEN_NB_XML_NODE_PTR {
                        for n_node2 in 0..GEN_NB_XML_NODE_PTR {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_shell_ctxt_ptr(n_ctxt, 0);
                            let arg = gen_char_ptr(n_arg, 1);
                            let node = gen_xml_node_ptr(n_node, 2);
                            let node2 = gen_xml_node_ptr(n_node2, 3);

                            let ret_val = xml_shell_list(ctxt, arg, node, node2);
                            desret_int(ret_val);
                            des_xml_shell_ctxt_ptr(n_ctxt, ctxt, 0);
                            des_char_ptr(n_arg, arg, 1);
                            des_xml_node_ptr(n_node, node, 2);
                            des_xml_node_ptr(n_node2, node2, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlShellList",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlShellList()");
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_arg);
                                eprint!(" {}", n_node);
                                eprintln!(" {}", n_node2);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_shell_load() {
        #[cfg(all(feature = "libxml_debug", feature = "xpath"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SHELL_CTXT_PTR {
                for n_filename in 0..GEN_NB_CHAR_PTR {
                    for n_node in 0..GEN_NB_XML_NODE_PTR {
                        for n_node2 in 0..GEN_NB_XML_NODE_PTR {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_shell_ctxt_ptr(n_ctxt, 0);
                            let filename = gen_char_ptr(n_filename, 1);
                            let node = gen_xml_node_ptr(n_node, 2);
                            let node2 = gen_xml_node_ptr(n_node2, 3);

                            let ret_val = xml_shell_load(ctxt, filename, node, node2);
                            desret_int(ret_val);
                            des_xml_shell_ctxt_ptr(n_ctxt, ctxt, 0);
                            des_char_ptr(n_filename, filename, 1);
                            des_xml_node_ptr(n_node, node, 2);
                            des_xml_node_ptr(n_node2, node2, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlShellLoad",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlShellLoad()");
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_filename);
                                eprint!(" {}", n_node);
                                eprintln!(" {}", n_node2);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_shell_print_xpath_result() {
        #[cfg(all(feature = "libxml_debug", feature = "xpath"))]
        unsafe {
            let mut leaks = 0;

            for n_list in 0..GEN_NB_XML_XPATH_OBJECT_PTR {
                let mem_base = xml_mem_blocks();
                let list = gen_xml_xpath_object_ptr(n_list, 0);

                xml_shell_print_xpath_result(list);
                des_xml_xpath_object_ptr(n_list, list, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlShellPrintXPathResult",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlShellPrintXPathResult()"
                    );
                    eprintln!(" {}", n_list);
                }
            }
        }
    }

    #[test]
    fn test_xml_shell_pwd() {
        #[cfg(all(feature = "libxml_debug", feature = "xpath"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SHELL_CTXT_PTR {
                for n_buffer in 0..GEN_NB_CHAR_PTR {
                    for n_node in 0..GEN_NB_XML_NODE_PTR {
                        for n_node2 in 0..GEN_NB_XML_NODE_PTR {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_shell_ctxt_ptr(n_ctxt, 0);
                            let buffer = gen_char_ptr(n_buffer, 1);
                            let node = gen_xml_node_ptr(n_node, 2);
                            let node2 = gen_xml_node_ptr(n_node2, 3);

                            let ret_val = xml_shell_pwd(ctxt, buffer, node, node2);
                            desret_int(ret_val);
                            des_xml_shell_ctxt_ptr(n_ctxt, ctxt, 0);
                            des_char_ptr(n_buffer, buffer, 1);
                            des_xml_node_ptr(n_node, node, 2);
                            des_xml_node_ptr(n_node2, node2, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlShellPwd",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlShellPwd()");
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_buffer);
                                eprint!(" {}", n_node);
                                eprintln!(" {}", n_node2);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_shell_save() {
        #[cfg(all(feature = "libxml_debug", feature = "xpath", feature = "libxml_output"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SHELL_CTXT_PTR {
                for n_filename in 0..GEN_NB_CHAR_PTR {
                    for n_node in 0..GEN_NB_XML_NODE_PTR {
                        for n_node2 in 0..GEN_NB_XML_NODE_PTR {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_shell_ctxt_ptr(n_ctxt, 0);
                            let filename = gen_char_ptr(n_filename, 1);
                            let node = gen_xml_node_ptr(n_node, 2);
                            let node2 = gen_xml_node_ptr(n_node2, 3);

                            let ret_val = xml_shell_save(ctxt, filename, node, node2);
                            desret_int(ret_val);
                            des_xml_shell_ctxt_ptr(n_ctxt, ctxt, 0);
                            des_char_ptr(n_filename, filename, 1);
                            des_xml_node_ptr(n_node, node, 2);
                            des_xml_node_ptr(n_node2, node2, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlShellSave",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlShellSave()");
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_filename);
                                eprint!(" {}", n_node);
                                eprintln!(" {}", n_node2);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_shell_validate() {
        #[cfg(all(feature = "libxml_debug", feature = "xpath", feature = "libxml_valid"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SHELL_CTXT_PTR {
                for n_dtd in 0..GEN_NB_CHAR_PTR {
                    for n_node in 0..GEN_NB_XML_NODE_PTR {
                        for n_node2 in 0..GEN_NB_XML_NODE_PTR {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_shell_ctxt_ptr(n_ctxt, 0);
                            let dtd = gen_char_ptr(n_dtd, 1);
                            let node = gen_xml_node_ptr(n_node, 2);
                            let node2 = gen_xml_node_ptr(n_node2, 3);

                            let ret_val = xml_shell_validate(ctxt, dtd, node, node2);
                            desret_int(ret_val);
                            des_xml_shell_ctxt_ptr(n_ctxt, ctxt, 0);
                            des_char_ptr(n_dtd, dtd, 1);
                            des_xml_node_ptr(n_node, node, 2);
                            des_xml_node_ptr(n_node2, node2, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlShellValidate",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlShellValidate()"
                                );
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_dtd);
                                eprint!(" {}", n_node);
                                eprintln!(" {}", n_node2);
                            }
                        }
                    }
                }
            }
        }
    }
}
