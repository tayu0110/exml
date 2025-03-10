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
    ffi::{CStr, CString, c_char},
    io::{Write, stdout},
    ptr::{addr_of_mut, null, null_mut},
};

#[cfg(feature = "xpath")]
use crate::xpath::{XmlXPathContextPtr, XmlXPathObjectPtr, XmlXPathObjectType};
use crate::{
    dict::{XmlDictPtr, xml_dict_lookup, xml_dict_owns},
    error::{__xml_raise_error, XmlParserErrors},
    generic_error,
    libxml::{
        chvalid::xml_is_blank_char,
        parser::{XmlParserOption, xml_parse_in_node_context},
        parser_internals::{XML_STRING_COMMENT, XML_STRING_TEXT, XML_STRING_TEXT_NOENC},
        valid::xml_snprintf_element_content,
        xmlstring::xml_strstr,
    },
    tree::{
        NodeCommon, XmlAttrPtr, XmlAttributeDefault, XmlAttributePtr, XmlAttributeType, XmlDocPtr,
        XmlDtdPtr, XmlElementPtr, XmlElementType, XmlElementTypeVal, XmlEntity, XmlEntityPtr,
        XmlEntityType, XmlGenericNodePtr, XmlNodePtr, XmlNs, XmlNsPtr, validate_name,
        xml_free_node_list, xml_get_doc_entity,
    },
};

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
            (*$ctxt).node,
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

pub type XmlDebugCtxtPtr<'a> = *mut XmlDebugCtxt<'a>;
#[repr(C)]
pub struct XmlDebugCtxt<'a> {
    output: Box<dyn Write + 'a>,     /* the output file */
    shift: String,                   /* used for indenting */
    depth: i32,                      /* current depth */
    doc: Option<XmlDocPtr>,          /* current document */
    node: Option<XmlGenericNodePtr>, /* current node */
    dict: XmlDictPtr,                /* the doc dictionary */
    check: i32,                      /* do just checkings */
    errors: i32,                     /* number of errors found */
    nodict: i32,                     /* if the document has no dictionary */
    options: i32,                    /* options */
}

impl XmlDebugCtxt<'_> {
    #[doc(alias = "xmlCtxtDumpSpaces")]
    fn dump_spaces(&mut self) {
        if self.check != 0 {
            return;
        }
        if self.depth > 0 {
            if self.depth < 50 {
                write!(self.output, "{}", &self.shift[..2 * self.depth as usize]).ok();
            } else {
                write!(self.output, "{}", &self.shift[..100]).ok();
            }
        }
    }

    #[doc(alias = "xmlCtxtDumpString")]
    fn dump_string(&mut self, s: Option<&str>) {
        if self.check != 0 {
            return;
        }
        // TODO: check UTF8 content of the string
        let Some(s) = s else {
            write!(self.output, "(NULL)").ok();
            return;
        };

        for c in s.bytes().take(40) {
            if xml_is_blank_char(c as u32) {
                write!(self.output, " ").ok();
            } else if c >= 0x80 {
                write!(self.output, "#{:0X}", c as i32).ok();
            } else {
                write!(self.output, "{}", c as char).ok();
            }
        }
        if s.len() > 40 {
            write!(self.output, "...").ok();
        }
    }

    /// Report if a given namespace is is not in scope.
    #[doc(alias = "xmlCtxtNsCheckScope")]
    unsafe fn ns_check_scope(&mut self, node: XmlGenericNodePtr, ns: XmlNsPtr) {
        unsafe {
            let ret: i32 = xml_ns_check_scope(node, ns);
            if ret == -2 {
                if let Some(prefix) = ns.prefix() {
                    xml_debug_err!(
                        self,
                        XmlParserErrors::XmlCheckNsScope,
                        "Reference to namespace '{}' not in scope\n",
                        prefix
                    );
                } else {
                    xml_debug_err!(
                        self,
                        XmlParserErrors::XmlCheckNsScope,
                        "Reference to default namespace not in scope\n",
                    );
                }
            }
            if ret == -3 {
                if let Some(prefix) = ns.prefix() {
                    xml_debug_err!(
                        self,
                        XmlParserErrors::XmlCheckNsAncestor,
                        "Reference to namespace '{}' not on ancestor\n",
                        prefix
                    );
                } else {
                    xml_debug_err!(
                        self,
                        XmlParserErrors::XmlCheckNsAncestor,
                        "Reference to default namespace not on ancestor\n",
                    );
                }
            }
        }
    }

    /// Do debugging on the string, currently it just checks the UTF-8 content
    #[doc(alias = "xmlCtxtCheckString")]
    unsafe fn check_string(&mut self, s: &str) {
        unsafe {
            if self.check != 0 {
                xml_debug_err!(
                    self,
                    XmlParserErrors::XmlCheckNotUTF8,
                    "String is not UTF-8 {s}",
                );
            }
        }
    }

    /// Do debugging on the name, for example the dictionary status and
    /// conformance to the Name production.
    #[doc(alias = "xmlCtxtCheckName")]
    unsafe fn check_name(&mut self, name: Option<&str>) {
        unsafe {
            if self.check != 0 {
                let Some(name) = name else {
                    xml_debug_err!(self, XmlParserErrors::XmlCheckNoName, "Name is NULL",);
                    return;
                };
                let cname = CString::new(name).unwrap();
                #[cfg(any(feature = "libxml_tree", feature = "schema"))]
                if validate_name::<false>(name).is_err() {
                    xml_debug_err!(
                        self,
                        XmlParserErrors::XmlCheckNotNCName,
                        "Name is not an NCName '{name}'",
                    );
                }
                if !self.dict.is_null()
                    && xml_dict_owns(self.dict, cname.as_ptr() as *const u8) == 0
                    && self.doc.is_none_or(|doc| {
                        doc.parse_flags
                            & (XmlParserOption::XmlParseSAX1 as i32
                                | XmlParserOption::XmlParseNoDict as i32)
                            == 0
                    })
                {
                    xml_debug_err!(
                        self,
                        XmlParserErrors::XmlCheckOutsideDict,
                        "Name is not from the document dictionary '{name}'",
                    );
                }
            }
        }
    }

    #[doc(alias = "xmlCtxtGenericNodeCheck")]
    unsafe fn generic_node_check(&mut self, node: XmlGenericNodePtr) {
        unsafe {
            let doc = node.document();

            if node.parent().is_none() {
                xml_debug_err!(
                    self,
                    XmlParserErrors::XmlCheckNoParent,
                    "Node has no parent\n",
                );
            }
            if let Some(doc) = doc {
                self.nodict = 1;
                if self.doc.is_none() {
                    self.doc = Some(doc);
                }
            } else {
                xml_debug_err!(self, XmlParserErrors::XmlCheckNoDoc, "Node has no doc\n",);
            }
            if node
                .parent()
                .filter(|p| {
                    node.document() != p.document() && node.name().as_deref() != Some("pseudoroot")
                })
                .is_some()
            {
                xml_debug_err!(
                    self,
                    XmlParserErrors::XmlCheckWrongDoc,
                    "Node doc differs from parent's one\n",
                );
            }
            if node.prev().is_none() {
                if let Ok(attr) = XmlAttrPtr::try_from(node) {
                    if attr
                        .parent()
                        .map(|parent| XmlNodePtr::try_from(parent).unwrap())
                        .filter(|p| Some(attr) != p.properties)
                        .is_some()
                    {
                        xml_debug_err!(
                            self,
                            XmlParserErrors::XmlCheckNoPrev,
                            "Attr has no prev and not first of attr list\n",
                        );
                    }
                } else if node
                    .parent()
                    .filter(|p| p.children() != Some(node))
                    .is_some()
                {
                    xml_debug_err!(
                        self,
                        XmlParserErrors::XmlCheckNoPrev,
                        "Node has no prev and not first of parent list\n",
                    );
                }
            } else if node.prev().unwrap().next() != Some(node) {
                xml_debug_err!(
                    self,
                    XmlParserErrors::XmlCheckWrongPrev,
                    "Node prev->next : back link wrong\n",
                );
            }
            if let Some(next) = node.next() {
                if next.prev() != Some(node) {
                    xml_debug_err!(
                        self,
                        XmlParserErrors::XmlCheckWrongNext,
                        "Node next->prev : forward link wrong\n",
                    );
                }
                if next.parent() != node.parent() {
                    xml_debug_err!(
                        self,
                        XmlParserErrors::XmlCheckWrongParent,
                        "Node next->prev : forward link wrong\n",
                    );
                }
            } else if node
                .parent()
                .filter(|p| {
                    node.element_type() != XmlElementType::XmlAttributeNode
                        && p.last() != Some(node)
                        && p.element_type() == XmlElementType::XmlElementNode
                })
                .is_some()
            {
                xml_debug_err!(
                    self,
                    XmlParserErrors::XmlCheckNoNext,
                    "Node has no next and not last of parent list\n",
                );
            }
            if node.element_type() == XmlElementType::XmlElementNode {
                let node = XmlNodePtr::try_from(node).unwrap();
                let mut ns = node.ns_def;
                while let Some(now) = ns {
                    self.ns_check_scope(node.into(), now);
                    ns = now.next;
                }
                if let Some(ns) = node.ns {
                    self.ns_check_scope(node.into(), ns);
                }
            } else if let Some(ns) = XmlAttrPtr::try_from(node).ok().and_then(|attr| attr.ns) {
                self.ns_check_scope(node, ns);
            }

            if !matches!(
                node.element_type(),
                XmlElementType::XmlElementNode
                    | XmlElementType::XmlAttributeNode
                    | XmlElementType::XmlElementDecl
                    | XmlElementType::XmlAttributeDecl
                    | XmlElementType::XmlDTDNode
                    | XmlElementType::XmlHTMLDocumentNode
                    | XmlElementType::XmlDocumentNode
            ) {
                let content = if let Ok(node) = XmlNodePtr::try_from(node) {
                    node.content
                } else if let Ok(ent) = XmlEntityPtr::try_from(node) {
                    ent.content
                } else {
                    todo!("What is this type ????: {:?}", node.element_type());
                };
                if !content.is_null() {
                    self.check_string(&CStr::from_ptr(content as *const i8).to_string_lossy());
                }
            }
            match node.element_type() {
                XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode => {
                    self.check_name(node.name().as_deref());
                }
                XmlElementType::XmlTextNode => {
                    if node.name().map_or(null(), |n| n.as_ptr()) != XML_STRING_TEXT.as_ptr() as _
                        && node.name().map_or(null(), |n| n.as_ptr())
                            != XML_STRING_TEXT_NOENC.as_ptr() as _
                    {
                        // some case of entity substitution can lead to this
                        if self.dict.is_null()
                            || (node.name().map_or(null(), |n| n.as_ptr())
                                != xml_dict_lookup(self.dict, c"nbktext".as_ptr() as _, 7))
                        {
                            xml_debug_err!(
                                self,
                                XmlParserErrors::XmlCheckWrongName,
                                "Text node has wrong name '{}'",
                                node.name().unwrap()
                            );
                        }
                    }
                }
                XmlElementType::XmlCommentNode => {
                    if node.name().map_or(null(), |n| n.as_ptr())
                        != XML_STRING_COMMENT.as_ptr() as _
                    {
                        xml_debug_err!(
                            self,
                            XmlParserErrors::XmlCheckWrongName,
                            "Comment node has wrong name '{}'",
                            node.name().unwrap()
                        );
                    }
                }
                XmlElementType::XmlPINode => {
                    self.check_name(node.name().as_deref());
                }
                XmlElementType::XmlCDATASectionNode => {
                    if let Some(name) = node.name() {
                        xml_debug_err!(
                            self,
                            XmlParserErrors::XmlCheckNameNotNull,
                            "CData section has non NULL name '{name}'",
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
    }

    #[doc(alias = "xmlCtxtDumpDtdNode")]
    unsafe fn dump_dtd_node(&mut self, dtd: Option<XmlDtdPtr>) {
        unsafe {
            self.dump_spaces();

            let Some(dtd) = dtd else {
                if self.check == 0 {
                    writeln!(self.output, "DTD node is NULL").ok();
                }
                return;
            };

            if dtd.element_type() != XmlElementType::XmlDTDNode {
                xml_debug_err!(self, XmlParserErrors::XmlCheckNotDTD, "Node is not a DTD",);
                return;
            }
            if self.check == 0 {
                if let Some(name) = dtd.name() {
                    write!(self.output, "DTD({name})").ok();
                } else {
                    write!(self.output, "DTD").ok();
                }
                if let Some(external_id) = dtd.external_id.as_deref() {
                    write!(self.output, ", PUBLIC {external_id}").ok();
                }
                if let Some(system_id) = dtd.system_id.as_deref() {
                    write!(self.output, ", SYSTEM {system_id}").ok();
                }
                writeln!(self.output).ok();
            }
            // Do a bit of checking
            self.generic_node_check(dtd.into());
        }
    }

    /// Dumps debug information for the DTD
    #[doc(alias = "xmlCtxtDumpDTD")]
    unsafe fn dump_dtd(&mut self, dtd: Option<XmlDtdPtr>) {
        unsafe {
            let Some(dtd) = dtd else {
                if self.check == 0 {
                    writeln!(self.output, "DTD is NULL").ok();
                }
                return;
            };
            self.dump_dtd_node(Some(dtd));
            if let Some(children) = dtd.children() {
                self.depth += 1;
                self.dump_node_list(Some(children));
                self.depth -= 1;
            } else {
                writeln!(self.output, "    DTD is empty").ok();
            }
        }
    }

    #[doc(alias = "xmlCtxtDumpElemDecl")]
    unsafe fn dump_elem_decl(&mut self, elem: Option<XmlElementPtr>) {
        unsafe {
            self.dump_spaces();

            let Some(elem) = elem else {
                if self.check == 0 {
                    writeln!(self.output, "Element declaration is NULL").ok();
                }
                return;
            };
            if elem.element_type() != XmlElementType::XmlElementDecl {
                xml_debug_err!(
                    self,
                    XmlParserErrors::XmlCheckNotElemDecl,
                    "Node is not an element declaration",
                );
                return;
            }
            if let Some(name) = elem.name() {
                if self.check == 0 {
                    write!(self.output, "ELEMDECL(").ok();
                    self.dump_string(Some(&name));
                    write!(self.output, ")").ok();
                }
            } else {
                xml_debug_err!(
                    self,
                    XmlParserErrors::XmlCheckNoName,
                    "Element declaration has no name",
                );
            }
            if self.check == 0 {
                match elem.etype {
                    XmlElementTypeVal::XmlElementTypeUndefined => {
                        write!(self.output, ", UNDEFINED").ok();
                    }
                    XmlElementTypeVal::XmlElementTypeEmpty => {
                        write!(self.output, ", EMPTY").ok();
                    }
                    XmlElementTypeVal::XmlElementTypeAny => {
                        write!(self.output, ", ANY").ok();
                    }
                    XmlElementTypeVal::XmlElementTypeMixed => {
                        write!(self.output, ", MIXED ").ok();
                    }
                    XmlElementTypeVal::XmlElementTypeElement => {
                        write!(self.output, ", MIXED ").ok();
                    }
                }
                if elem.element_type() != XmlElementType::XmlElementNode && !elem.content.is_null()
                {
                    let mut buf = String::with_capacity(5000);

                    xml_snprintf_element_content(&mut buf, 5000, elem.content, 1);
                    write!(self.output, "{}", buf).ok();
                }
                writeln!(self.output).ok();
            }

            // Do a bit of checking
            self.generic_node_check(elem.into());
        }
    }

    #[doc(alias = "xmlCtxtDumpAttrDecl")]
    unsafe fn dump_attr_decl(&mut self, attr: Option<XmlAttributePtr>) {
        unsafe {
            self.dump_spaces();

            let Some(attr) = attr else {
                if self.check == 0 {
                    writeln!(self.output, "Attribute declaration is NULL").ok();
                }
                return;
            };
            if attr.element_type() != XmlElementType::XmlAttributeDecl {
                xml_debug_err!(
                    self,
                    XmlParserErrors::XmlCheckNotAttrDecl,
                    "Node is not an attribute declaration",
                );
                return;
            }
            if let Some(name) = attr.name() {
                if self.check == 0 {
                    write!(self.output, "ATTRDECL({name})").ok();
                }
            } else {
                xml_debug_err!(
                    self,
                    XmlParserErrors::XmlCheckNoName,
                    "Node attribute declaration has no name",
                );
            }
            if let Some(elem) = attr.elem.as_deref() {
                if self.check == 0 {
                    write!(self.output, " for {elem}").ok();
                }
            } else {
                xml_debug_err!(
                    self,
                    XmlParserErrors::XmlCheckNoElem,
                    "Node attribute declaration has no element name",
                );
            }
            if self.check == 0 {
                match attr.atype {
                    XmlAttributeType::XmlAttributeCDATA => {
                        write!(self.output, " CDATA").ok();
                    }
                    XmlAttributeType::XmlAttributeID => {
                        write!(self.output, " ID").ok();
                    }
                    XmlAttributeType::XmlAttributeIDREF => {
                        write!(self.output, " IDREF").ok();
                    }
                    XmlAttributeType::XmlAttributeIDREFS => {
                        write!(self.output, " IDREFS").ok();
                    }
                    XmlAttributeType::XmlAttributeEntity => {
                        write!(self.output, " ENTITY").ok();
                    }
                    XmlAttributeType::XmlAttributeEntities => {
                        write!(self.output, " ENTITIES").ok();
                    }
                    XmlAttributeType::XmlAttributeNmtoken => {
                        write!(self.output, " NMTOKEN").ok();
                    }
                    XmlAttributeType::XmlAttributeNmtokens => {
                        write!(self.output, " NMTOKENS").ok();
                    }
                    XmlAttributeType::XmlAttributeEnumeration => {
                        write!(self.output, " ENUMERATION").ok();
                    }
                    XmlAttributeType::XmlAttributeNotation => {
                        write!(self.output, " NOTATION ").ok();
                    }
                }
                if let Some(mut cur) = attr.tree.as_deref() {
                    let mut remain = true;
                    for indx in 0..5 {
                        if indx != 0 {
                            write!(self.output, "|{}", cur.name).ok();
                        } else {
                            write!(self.output, " ({}", cur.name).ok();
                        }
                        let Some(next) = cur.next.as_deref() else {
                            remain = false;
                            break;
                        };
                        cur = next;
                    }
                    if !remain {
                        write!(self.output, ")").ok();
                    } else {
                        write!(self.output, "...)").ok();
                    }
                }
                match attr.def {
                    XmlAttributeDefault::XmlAttributeNone => {}
                    XmlAttributeDefault::XmlAttributeRequired => {
                        write!(self.output, " REQUIRED").ok();
                    }
                    XmlAttributeDefault::XmlAttributeImplied => {
                        write!(self.output, " IMPLIED").ok();
                    }
                    XmlAttributeDefault::XmlAttributeFixed => {
                        write!(self.output, " FIXED").ok();
                    }
                }
                if !attr.default_value.is_null() {
                    write!(self.output, "\"").ok();
                    let def_value = attr.default_value;
                    self.dump_string(
                        Some(CStr::from_ptr(def_value as *const i8).to_string_lossy()).as_deref(),
                    );
                    write!(self.output, "\"").ok();
                }
                writeln!(self.output).ok();
            }

            // Do a bit of checking
            self.generic_node_check(attr.into());
        }
    }

    #[doc(alias = "xmlCtxtDumpEntityDecl")]
    unsafe fn dump_entity_decl(&mut self, ent: Option<XmlEntityPtr>) {
        unsafe {
            self.dump_spaces();

            let Some(ent) = ent else {
                if self.check == 0 {
                    writeln!(self.output, "Entity declaration is NULL").ok();
                }
                return;
            };
            if ent.element_type() != XmlElementType::XmlEntityDecl {
                xml_debug_err!(
                    self,
                    XmlParserErrors::XmlCheckNotEntityDecl,
                    "Node is not an entity declaration",
                );
                return;
            }
            if let Some(name) = ent.name() {
                if self.check == 0 {
                    write!(self.output, "ENTITYDECL(").ok();
                    self.dump_string(Some(&name));
                    write!(self.output, ")").ok();
                }
            } else {
                xml_debug_err!(
                    self,
                    XmlParserErrors::XmlCheckNoName,
                    "Entity declaration has no name",
                );
            }
            if self.check == 0 {
                match ent.etype {
                    XmlEntityType::XmlInternalGeneralEntity => {
                        writeln!(self.output, ", internal").ok();
                    }
                    XmlEntityType::XmlExternalGeneralParsedEntity => {
                        writeln!(self.output, ", external parsed").ok();
                    }
                    XmlEntityType::XmlExternalGeneralUnparsedEntity => {
                        writeln!(self.output, ", unparsed").ok();
                    }
                    XmlEntityType::XmlInternalParameterEntity => {
                        writeln!(self.output, ", parameter").ok();
                    }
                    XmlEntityType::XmlExternalParameterEntity => {
                        writeln!(self.output, ", external parameter").ok();
                    }
                    XmlEntityType::XmlInternalPredefinedEntity => {
                        writeln!(self.output, ", predefined").ok();
                    }
                }
                if !ent.external_id.is_null() {
                    self.dump_spaces();
                    let external_id =
                        CStr::from_ptr(ent.external_id as *const i8).to_string_lossy();
                    writeln!(self.output, " ExternalID={external_id}").ok();
                }
                if !ent.system_id.is_null() {
                    self.dump_spaces();
                    let system_id = CStr::from_ptr(ent.system_id as *const i8).to_string_lossy();
                    writeln!(self.output, " SystemID={system_id}").ok();
                }
                if !ent.uri.is_null() {
                    self.dump_spaces();
                    let uri = CStr::from_ptr(ent.uri as *const i8).to_string_lossy();
                    writeln!(self.output, " URI={uri}").ok();
                }
                let content = ent.content;
                if !content.is_null() {
                    self.dump_spaces();
                    write!(self.output, " content=").ok();
                    self.dump_string(Some(
                        &CStr::from_ptr(content as *const i8).to_string_lossy(),
                    ));
                    writeln!(self.output).ok();
                }
            }

            // Do a bit of checking
            self.generic_node_check(ent.into());
        }
    }

    #[doc(alias = "xmlCtxtDumpNamespace")]
    unsafe fn dump_namespace(&mut self, ns: Option<&XmlNs>) {
        unsafe {
            self.dump_spaces();

            let Some(ns) = ns else {
                if self.check == 0 {
                    writeln!(self.output, "namespace node is NULL").ok();
                }
                return;
            };
            if ns.element_type() != XmlElementType::XmlNamespaceDecl {
                xml_debug_err!(
                    self,
                    XmlParserErrors::XmlCheckNotNsDecl,
                    "Node is not a namespace declaration",
                );
                return;
            }
            if ns.href.is_null() {
                if let Some(prefix) = ns.prefix() {
                    xml_debug_err!(
                        self,
                        XmlParserErrors::XmlCheckNoHref,
                        "Incomplete namespace {} href=NULL\n",
                        prefix
                    );
                } else {
                    xml_debug_err!(
                        self,
                        XmlParserErrors::XmlCheckNoHref,
                        "Incomplete default namespace href=NULL\n",
                    );
                }
            } else if self.check == 0 {
                if let Some(prefix) = ns.prefix() {
                    write!(self.output, "namespace {prefix} href=").ok();
                } else {
                    write!(self.output, "default namespace href=").ok();
                }

                let href = ns.href;
                self.dump_string(
                    (!href.is_null())
                        .then(|| CStr::from_ptr(href as *const i8).to_string_lossy())
                        .as_deref(),
                );
                writeln!(self.output).ok();
            }
        }
    }

    #[doc(alias = "xmlCtxtDumpNamespaceList")]
    unsafe fn dump_namespace_list(&mut self, mut ns: Option<XmlNsPtr>) {
        unsafe {
            while let Some(now) = ns {
                self.dump_namespace(Some(&*now));
                let next = now.next;
                ns = next;
            }
        }
    }

    #[doc(alias = "xmlCtxtDumpEntity")]
    unsafe fn dump_entity(&mut self, ent: Option<&XmlEntity>) {
        unsafe {
            self.dump_spaces();

            let Some(ent) = ent else {
                if self.check == 0 {
                    writeln!(self.output, "Entity is NULL").ok();
                }
                return;
            };
            if self.check == 0 {
                match ent.etype {
                    XmlEntityType::XmlInternalGeneralEntity => {
                        write!(self.output, "INTERNAL_GENERAL_ENTITY ").ok();
                    }
                    XmlEntityType::XmlExternalGeneralParsedEntity => {
                        write!(self.output, "EXTERNAL_GENERAL_PARSED_ENTITY ").ok();
                    }
                    XmlEntityType::XmlExternalGeneralUnparsedEntity => {
                        write!(self.output, "EXTERNAL_GENERAL_UNPARSED_ENTITY ").ok();
                    }
                    XmlEntityType::XmlInternalParameterEntity => {
                        write!(self.output, "INTERNAL_PARAMETER_ENTITY ").ok();
                    }
                    XmlEntityType::XmlExternalParameterEntity => {
                        write!(self.output, "EXTERNAL_PARAMETER_ENTITY ").ok();
                    }
                    e => {
                        write!(self.output, "ENTITY_{} ! ", e as i32).ok();
                    }
                }
                writeln!(self.output, "{}", ent.name().unwrap()).ok();
                if !ent.external_id.is_null() {
                    self.dump_spaces();
                    let external_id =
                        CStr::from_ptr(ent.external_id as *const i8).to_string_lossy();
                    writeln!(self.output, "ExternalID={external_id}").ok();
                }
                if !ent.system_id.is_null() {
                    self.dump_spaces();
                    let system_id = CStr::from_ptr(ent.system_id as *const i8).to_string_lossy();
                    writeln!(self.output, "SystemID={system_id}").ok();
                }
                if !ent.uri.is_null() {
                    self.dump_spaces();
                    let uri = CStr::from_ptr(ent.uri as *const i8).to_string_lossy();
                    writeln!(self.output, "URI={uri}").ok();
                }
                let content = ent.content;
                if !content.is_null() {
                    self.dump_spaces();
                    write!(self.output, "content=").ok();
                    self.dump_string(Some(
                        &CStr::from_ptr(content as *const i8).to_string_lossy(),
                    ));
                    writeln!(self.output).ok();
                }
            }
        }
    }

    /// Dumps debug information for the attribute
    #[doc(alias = "xmlCtxtDumpAttr")]
    unsafe fn dump_attr(&mut self, attr: Option<XmlAttrPtr>) {
        unsafe {
            self.dump_spaces();

            let Some(attr) = attr else {
                if self.check == 0 {
                    write!(self.output, "Attr is NULL").ok();
                }
                return;
            };
            if self.check == 0 {
                write!(self.output, "ATTRIBUTE ").ok();
                self.dump_string(attr.name().as_deref());
                writeln!(self.output).ok();
                if let Some(children) = attr.children() {
                    self.depth += 1;
                    self.dump_node_list(Some(children));
                    self.depth -= 1;
                }
            }
            if attr.name().is_none() {
                xml_debug_err!(
                    self,
                    XmlParserErrors::XmlCheckNoName,
                    "Attribute has no name",
                );
            }

            // Do a bit of checking
            self.generic_node_check(attr.into());
        }
    }

    /// Dumps debug information for the attribute list
    #[doc(alias = "xmlCtxtDumpAttrList")]
    unsafe fn dump_attr_list(&mut self, mut attr: Option<XmlAttrPtr>) {
        unsafe {
            while let Some(now) = attr {
                self.dump_attr(Some(now));
                attr = now.next;
            }
        }
    }

    /// Dumps debug information for the element node, it is not recursive
    #[doc(alias = "xmlCtxtDumpOneNode")]
    unsafe fn dump_one_node(&mut self, node: Option<XmlGenericNodePtr>) {
        unsafe {
            let Some(node) = node else {
                if self.check == 0 {
                    self.dump_spaces();
                    writeln!(self.output, "node is NULL").ok();
                }
                return;
            };
            self.node = Some(node);

            match node.element_type() {
                XmlElementType::XmlElementNode => {
                    if self.check == 0 {
                        self.dump_spaces();
                        write!(self.output, "ELEMENT ").ok();
                        let node = XmlNodePtr::try_from(node).unwrap();
                        if let Some(ns) = node.ns {
                            if let Some(prefix) = ns.prefix() {
                                self.dump_string(Some(&prefix));
                            }
                            write!(self.output, ":").ok();
                        }
                        self.dump_string(node.name().as_deref());
                        writeln!(self.output).ok();
                    }
                }
                XmlElementType::XmlAttributeNode => {
                    if self.check == 0 {
                        self.dump_spaces();
                    }
                    writeln!(self.output, "Error, ATTRIBUTE found here").ok();
                    self.generic_node_check(node);
                    return;
                }
                XmlElementType::XmlTextNode => {
                    if self.check == 0 {
                        self.dump_spaces();
                        let node = XmlNodePtr::try_from(node).unwrap();
                        if node.name == XML_STRING_TEXT_NOENC.as_ptr() as *const u8 {
                            write!(self.output, "TEXT no enc").ok();
                        } else {
                            write!(self.output, "TEXT").ok();
                        }
                        if self.options & DUMP_TEXT_TYPE != 0 {
                            if xml_dict_owns(self.dict, node.content) == 1 {
                                writeln!(self.output, " interned").ok();
                            } else {
                                writeln!(self.output).ok();
                            }
                        } else {
                            writeln!(self.output).ok();
                        }
                    }
                }
                XmlElementType::XmlCDATASectionNode => {
                    if self.check == 0 {
                        self.dump_spaces();
                        writeln!(self.output, "CDATA_SECTION").ok();
                    }
                }
                XmlElementType::XmlEntityRefNode => {
                    if self.check == 0 {
                        self.dump_spaces();
                        writeln!(self.output, "ENTITY_REF({})", node.name().unwrap()).ok();
                    }
                }
                XmlElementType::XmlEntityNode => {
                    if self.check == 0 {
                        self.dump_spaces();
                        writeln!(self.output, "ENTITY").ok();
                    }
                }
                XmlElementType::XmlPINode => {
                    if self.check == 0 {
                        self.dump_spaces();
                        writeln!(self.output, "PI {}", node.name().unwrap()).ok();
                    }
                }
                XmlElementType::XmlCommentNode => {
                    if self.check == 0 {
                        self.dump_spaces();
                        writeln!(self.output, "COMMENT").ok();
                    }
                }
                XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                    if self.check == 0 {
                        self.dump_spaces();
                    }
                    writeln!(self.output, "Error, DOCUMENT found here").ok();
                    self.generic_node_check(node);
                    return;
                }
                XmlElementType::XmlDocumentTypeNode => {
                    if self.check == 0 {
                        self.dump_spaces();
                        writeln!(self.output, "DOCUMENT_TYPE").ok();
                    }
                }
                XmlElementType::XmlDocumentFragNode => {
                    if self.check == 0 {
                        self.dump_spaces();
                        writeln!(self.output, "DOCUMENT_FRAG").ok();
                    }
                }
                XmlElementType::XmlNotationNode => {
                    if self.check == 0 {
                        self.dump_spaces();
                        writeln!(self.output, "NOTATION").ok();
                    }
                }
                XmlElementType::XmlDTDNode => {
                    self.dump_dtd_node(XmlDtdPtr::try_from(node).ok());
                    return;
                }
                XmlElementType::XmlElementDecl => {
                    self.dump_elem_decl(XmlElementPtr::try_from(node).ok());
                    return;
                }
                XmlElementType::XmlAttributeDecl => {
                    self.dump_attr_decl(XmlAttributePtr::try_from(node).ok());
                    return;
                }
                XmlElementType::XmlEntityDecl => {
                    self.dump_entity_decl(XmlEntityPtr::try_from(node).ok());
                    return;
                }
                XmlElementType::XmlNamespaceDecl => {
                    self.dump_namespace(XmlNsPtr::try_from(node).ok().as_deref());
                    return;
                }
                XmlElementType::XmlXIncludeStart => {
                    if self.check == 0 {
                        self.dump_spaces();
                        writeln!(self.output, "INCLUDE START").ok();
                    }
                    return;
                }
                XmlElementType::XmlXIncludeEnd => {
                    if self.check == 0 {
                        self.dump_spaces();
                        writeln!(self.output, "INCLUDE END").ok();
                    }
                    return;
                }
                _ => {
                    if self.check == 0 {
                        self.dump_spaces();
                    }
                    xml_debug_err!(
                        self,
                        XmlParserErrors::XmlCheckUnknownNode,
                        "Unknown node type {}\n",
                        node.element_type() as i32
                    );
                    return;
                }
            }
            if node.document().is_none() {
                if self.check == 0 {
                    self.dump_spaces();
                }
                writeln!(self.output, "PBM: doc.is_null() !!!").ok();
            }
            self.depth += 1;
            if let Some(ns_def) = XmlNodePtr::try_from(node)
                .ok()
                .filter(|n| n.element_type() == XmlElementType::XmlElementNode)
                .and_then(|node| node.ns_def)
            {
                self.dump_namespace_list(Some(ns_def));
            }
            if let Some(prop) = XmlNodePtr::try_from(node)
                .ok()
                .filter(|n| n.element_type() == XmlElementType::XmlElementNode)
                .and_then(|n| n.properties)
            {
                self.dump_attr_list(Some(prop));
            }
            if node.element_type() != XmlElementType::XmlEntityRefNode {
                if node.element_type() != XmlElementType::XmlElementNode && self.check == 0 {
                    let content = if let Ok(node) = XmlNodePtr::try_from(node) {
                        node.content
                    } else {
                        null()
                    };
                    if !content.is_null() {
                        self.dump_spaces();
                        write!(self.output, "content=").ok();
                        self.dump_string(Some(
                            &CStr::from_ptr(content as *const i8).to_string_lossy(),
                        ));
                        writeln!(self.output).ok();
                    }
                }
            } else {
                let name = node.name().unwrap();
                if let Some(ent) = xml_get_doc_entity(node.document(), &name) {
                    self.dump_entity(Some(&*ent));
                }
            }
            self.depth -= 1;

            // Do a bit of checking
            self.generic_node_check(node);
        }
    }

    /// Dumps debug information for the element node, it is recursive
    #[doc(alias = "xmlCtxtDumpNode")]
    unsafe fn dump_node(&mut self, node: Option<XmlGenericNodePtr>) {
        unsafe {
            let Some(node) = node else {
                if self.check == 0 {
                    self.dump_spaces();
                    writeln!(self.output, "node is NULL").ok();
                }
                return;
            };
            self.dump_one_node(Some(node));
            if let Some(children) = node.children().filter(|_| {
                node.element_type() != XmlElementType::XmlNamespaceDecl
                    && node.element_type() != XmlElementType::XmlEntityRefNode
            }) {
                self.depth += 1;
                self.dump_node_list(Some(children));
                self.depth -= 1;
            }
        }
    }

    /// Dumps debug information for the list of element node, it is recursive
    #[doc(alias = "xmlCtxtDumpNodeList")]
    unsafe fn dump_node_list(&mut self, node: Option<XmlGenericNodePtr>) {
        unsafe {
            if let Some(mut node) = node {
                self.dump_node(Some(node));
                while let Some(next) = node.next() {
                    self.dump_node(Some(next));
                    node = next;
                }
            }
        }
    }

    #[doc(alias = "xmlCtxtDumpDocHead")]
    unsafe fn dump_doc_head(&mut self, doc: XmlDocPtr) {
        unsafe {
            self.node = Some(XmlGenericNodePtr::from(doc));

            match doc.typ {
                XmlElementType::XmlElementNode => {
                    xml_debug_err!(
                        self,
                        XmlParserErrors::XmlCheckFoundElement,
                        "Misplaced ELEMENT node\n",
                    );
                }
                XmlElementType::XmlAttributeNode => {
                    xml_debug_err!(
                        self,
                        XmlParserErrors::XmlCheckFoundAttribute,
                        "Misplaced ATTRIBUTE node\n",
                    );
                }
                XmlElementType::XmlTextNode => {
                    xml_debug_err!(
                        self,
                        XmlParserErrors::XmlCheckFoundText,
                        "Misplaced TEXT node\n",
                    );
                }
                XmlElementType::XmlCDATASectionNode => {
                    xml_debug_err!(
                        self,
                        XmlParserErrors::XmlCheckFoundCDATA,
                        "Misplaced CDATA node\n",
                    );
                }
                XmlElementType::XmlEntityRefNode => {
                    xml_debug_err!(
                        self,
                        XmlParserErrors::XmlCheckFoundEntityRef,
                        "Misplaced ENTITYREF node\n",
                    );
                }
                XmlElementType::XmlEntityNode => {
                    xml_debug_err!(
                        self,
                        XmlParserErrors::XmlCheckFoundEntity,
                        "Misplaced ENTITY node\n",
                    );
                }
                XmlElementType::XmlPINode => {
                    xml_debug_err!(
                        self,
                        XmlParserErrors::XmlCheckFoundPI,
                        "Misplaced PI node\n",
                    );
                }
                XmlElementType::XmlCommentNode => {
                    xml_debug_err!(
                        self,
                        XmlParserErrors::XmlCheckFoundComment,
                        "Misplaced COMMENT node\n",
                    );
                }
                XmlElementType::XmlDocumentNode => {
                    if self.check == 0 {
                        writeln!(self.output, "DOCUMENT").ok();
                    }
                }
                XmlElementType::XmlHTMLDocumentNode => {
                    if self.check == 0 {
                        writeln!(self.output, "HTML DOCUMENT").ok();
                    }
                }
                XmlElementType::XmlDocumentTypeNode => {
                    xml_debug_err!(
                        self,
                        XmlParserErrors::XmlCheckFoundDoctype,
                        "Misplaced DOCTYPE node\n",
                    );
                }
                XmlElementType::XmlDocumentFragNode => {
                    xml_debug_err!(
                        self,
                        XmlParserErrors::XmlCheckFoundFragment,
                        "Misplaced FRAGMENT node\n",
                    );
                }
                XmlElementType::XmlNotationNode => {
                    xml_debug_err!(
                        self,
                        XmlParserErrors::XmlCheckFoundNotation,
                        "Misplaced NOTATION node\n",
                    );
                }
                _ => {
                    xml_debug_err!(
                        self,
                        XmlParserErrors::XmlCheckUnknownNode,
                        "Unknown node type {}\n",
                        doc.element_type() as i32
                    );
                }
            }
        }
    }

    /// Dumps debug information concerning the document, not recursive
    #[doc(alias = "xmlCtxtDumpDocumentHead")]
    unsafe fn dump_document_head(&mut self, doc: Option<XmlDocPtr>) {
        unsafe {
            if let Some(doc) = doc {
                self.dump_doc_head(doc);
                if self.check == 0 {
                    if let Some(name) = doc.name() {
                        write!(self.output, "name=").ok();
                        self.dump_string(Some(&name));
                        writeln!(self.output).ok();
                    }
                    if let Some(version) = doc.version.as_deref() {
                        write!(self.output, "version=").ok();
                        self.dump_string(Some(version));
                        writeln!(self.output).ok();
                    }
                    if let Some(encoding) = doc.encoding.as_deref() {
                        write!(self.output, "encoding=").ok();
                        self.dump_string(Some(encoding));
                        writeln!(self.output).ok();
                    }
                    if let Some(url) = doc.url.as_deref() {
                        write!(self.output, "URL=").ok();
                        self.dump_string(Some(url));
                        writeln!(self.output).ok();
                    }
                    if doc.standalone != 0 {
                        writeln!(self.output, "standalone=true").ok();
                    }
                }
                if let Some(old_ns) = doc.old_ns {
                    self.dump_namespace_list(Some(old_ns));
                }
            }
        }
    }

    /// Dumps debug information for the document, it's recursive
    #[doc(alias = "xmlCtxtDumpDocument")]
    unsafe fn dump_document(&mut self, doc: Option<XmlDocPtr>) {
        unsafe {
            let Some(doc) = doc else {
                if self.check == 0 {
                    writeln!(self.output, "DOCUMENT.is_null() !").ok();
                }
                return;
            };
            self.dump_document_head(Some(doc));
            if let Some(children) = (*doc).children().filter(|_| {
                matches!(
                    doc.element_type(),
                    XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode
                )
            }) {
                self.depth += 1;
                self.dump_node_list(Some(children));
                self.depth -= 1;
            }
        }
    }

    #[doc(alias = "xmlCtxtDumpEntityCallback")]
    unsafe fn dump_entities_callback(&mut self, cur: Option<&XmlEntity>) {
        unsafe {
            let Some(cur) = cur else {
                if self.check == 0 {
                    write!(self.output, "Entity is NULL").ok();
                }
                return;
            };
            if self.check == 0 {
                write!(self.output, "{} : ", cur.name().unwrap()).ok();
                match cur.etype {
                    XmlEntityType::XmlInternalGeneralEntity => {
                        write!(self.output, "INTERNAL GENERAL, ").ok();
                    }
                    XmlEntityType::XmlExternalGeneralParsedEntity => {
                        write!(self.output, "EXTERNAL PARSED, ").ok();
                    }
                    XmlEntityType::XmlExternalGeneralUnparsedEntity => {
                        write!(self.output, "EXTERNAL UNPARSED, ").ok();
                    }
                    XmlEntityType::XmlInternalParameterEntity => {
                        write!(self.output, "INTERNAL PARAMETER, ").ok();
                    }
                    XmlEntityType::XmlExternalParameterEntity => {
                        write!(self.output, "EXTERNAL PARAMETER, ").ok();
                    }
                    e => {
                        xml_debug_err!(
                            self,
                            XmlParserErrors::XmlCheckEntityType,
                            "Unknown entity type {}\n",
                            e as i32
                        );
                    }
                }
                if !cur.external_id.is_null() {
                    let external_id =
                        CStr::from_ptr(cur.external_id as *const i8).to_string_lossy();
                    write!(self.output, "ID \"{external_id}\"").ok();
                }
                if !cur.system_id.is_null() {
                    let system_id = CStr::from_ptr(cur.system_id as *const i8).to_string_lossy();
                    write!(self.output, "SYSTEM \"{system_id}\"").ok();
                }
                if !cur.orig.is_null() {
                    let orig = CStr::from_ptr(cur.orig as *const i8).to_string_lossy();
                    write!(self.output, "\n orig \"{orig}\"").ok();
                }
                if cur.typ != XmlElementType::XmlElementNode && !cur.content.is_null() {
                    let content = CStr::from_ptr(cur.content as *const i8).to_string_lossy();
                    write!(self.output, "\n content \"{content}\"").ok();
                }
                writeln!(self.output).ok();
            }
        }
    }

    /// Dumps debug information for all the entities in use by the document
    #[doc(alias = "xmlCtxtDumpEntities")]
    unsafe fn dump_entities(&mut self, doc: Option<XmlDocPtr>) {
        unsafe {
            if let Some(doc) = doc {
                self.dump_doc_head(doc);
                if let Some(int_subset) = doc.int_subset {
                    if let Some(table) = int_subset.entities {
                        if self.check == 0 {
                            writeln!(self.output, "Entities in internal subset").ok();
                        }
                        table.scan(|payload, _, _, _| {
                            let entity = *payload;
                            self.dump_entities_callback(Some(&*entity));
                        });
                    }
                } else {
                    writeln!(self.output, "No entities in internal subset").ok();
                }
                if let Some(ext_subset) = doc.ext_subset {
                    if let Some(table) = ext_subset.entities {
                        if self.check == 0 {
                            writeln!(self.output, "Entities in external subset").ok();
                        }
                        table.scan(|payload, _, _, _| {
                            let entity = *payload;
                            self.dump_entities_callback(Some(&*entity));
                        });
                    }
                } else if self.check == 0 {
                    writeln!(self.output, "No entities in external subset").ok();
                }
            }
        }
    }
}

impl Default for XmlDebugCtxt<'_> {
    fn default() -> Self {
        Self {
            depth: 0,
            check: 0,
            errors: 0,
            output: Box::new(stdout()),
            doc: None,
            node: None,
            dict: null_mut(),
            nodict: 0,
            options: 0,
            shift: " ".repeat(100),
        }
    }
}

/// Dumps information about the string, shorten it if necessary
#[doc(alias = "xmlDebugDumpString")]
pub fn xml_debug_dump_string<'a>(mut output: Option<&mut (impl Write + 'a)>, s: Option<&str>) {
    let mut stdout = stdout();
    let output = output
        .as_mut()
        .map(|o| o as &mut dyn Write)
        .unwrap_or(&mut stdout as &mut dyn Write);
    let Some(s) = s else {
        write!(output, "(NULL)").ok();
        return;
    };
    for c in s.bytes().take(40) {
        if xml_is_blank_char(c as u32) {
            write!(output, " ").ok();
        } else if c >= 0x80 {
            write!(output, "#{:X}", c as i32).ok();
        } else {
            write!(output, "{}", c as char).ok();
        }
    }
    if s.len() > 40 {
        write!(output, "...").ok();
    }
}

const DUMP_TEXT_TYPE: i32 = 1;

/// Check that a given namespace is in scope on a node.
///
/// Returns 1 if in scope, -1 in case of argument error,
/// -2 if the namespace is not in scope,
/// and -3 if not on an ancestor node.
#[doc(alias = "xmlNsCheckScope")]
unsafe fn xml_ns_check_scope(node: XmlGenericNodePtr, ns: XmlNsPtr) -> i32 {
    unsafe {
        if !matches!(
            node.element_type(),
            XmlElementType::XmlElementNode
                | XmlElementType::XmlAttributeNode
                | XmlElementType::XmlDocumentNode
                | XmlElementType::XmlTextNode
                | XmlElementType::XmlHTMLDocumentNode
                | XmlElementType::XmlXIncludeStart
        ) {
            return -2;
        }

        let mut node = Some(node);
        while let Some(now) = node.filter(|n| {
            matches!(
                n.element_type(),
                XmlElementType::XmlElementNode
                    | XmlElementType::XmlAttributeNode
                    | XmlElementType::XmlTextNode
                    | XmlElementType::XmlXIncludeStart
            )
        }) {
            if matches!(
                now.element_type(),
                XmlElementType::XmlElementNode | XmlElementType::XmlXIncludeStart
            ) {
                let mut cur = XmlNodePtr::try_from(now).unwrap().ns_def;
                while let Some(now) = cur {
                    if now == ns {
                        return 1;
                    }
                    if now.prefix() == ns.prefix() {
                        return -2;
                    }
                    cur = now.next;
                }
            }
            node = now.parent();
        }
        // the xml namespace may be declared on the document node
        if let Some(node) = node.and_then(|node| XmlDocPtr::try_from(node).ok()) {
            let old_ns = node.old_ns;
            if old_ns == Some(ns) {
                return 1;
            }
        }
        -3
    }
}

/// Dumps debug information for the attribute
#[doc(alias = "xmlDebugDumpAttr")]
pub unsafe fn xml_debug_dump_attr<'a>(
    output: &mut (impl Write + 'a),
    attr: Option<XmlAttrPtr>,
    depth: i32,
) {
    unsafe {
        let mut ctxt = XmlDebugCtxt {
            output: Box::new(output),
            depth,
            ..Default::default()
        };
        ctxt.dump_attr(attr);
    }
}

/// Dumps debug information for the attribute list
#[doc(alias = "xmlDebugDumpAttrList")]
pub unsafe fn xml_debug_dump_attr_list<'a>(
    output: &mut (impl Write + 'a),
    attr: Option<XmlAttrPtr>,
    depth: i32,
) {
    unsafe {
        let mut ctxt = XmlDebugCtxt {
            output: Box::new(output),
            depth,
            ..Default::default()
        };
        ctxt.dump_attr_list(attr);
    }
}

/// Dumps debug information for the element node, it is not recursive
#[doc(alias = "xmlDebugDumpOneNode")]
pub unsafe fn xml_debug_dump_one_node<'a>(
    output: &mut (impl Write + 'a),
    node: Option<XmlGenericNodePtr>,
    depth: i32,
) {
    unsafe {
        let mut ctxt = XmlDebugCtxt {
            output: Box::new(output),
            depth,
            ..Default::default()
        };
        ctxt.dump_one_node(node);
    }
}

/// Dumps debug information for the element node, it is recursive
#[doc(alias = "xmlDebugDumpNode")]
pub unsafe fn xml_debug_dump_node<'a>(
    output: Option<impl Write + 'a>,
    node: Option<XmlGenericNodePtr>,
    depth: i32,
) {
    unsafe {
        let mut ctxt = XmlDebugCtxt {
            output: output.map_or(Box::new(stdout()) as Box<dyn Write>, |o| Box::new(o)),
            depth,
            ..Default::default()
        };
        ctxt.dump_node(node);
    }
}

/// Dumps debug information for the list of element node, it is recursive
#[doc(alias = "xmlDebugDumpNodeList")]
pub unsafe fn xml_debug_dump_node_list<'a>(
    output: Option<impl Write + 'a>,
    node: Option<XmlGenericNodePtr>,
    depth: i32,
) {
    unsafe {
        let mut ctxt = XmlDebugCtxt {
            output: output.map_or(Box::new(stdout()) as Box<dyn Write>, |o| Box::new(o)),
            depth,
            ..Default::default()
        };
        ctxt.dump_node_list(node);
    }
}

/// Dumps debug information concerning the document, not recursive
#[doc(alias = "xmlDebugDumpDocumentHead")]
pub unsafe fn xml_debug_dump_document_head<'a>(
    output: Option<impl Write + 'a>,
    doc: Option<XmlDocPtr>,
) {
    unsafe {
        let mut ctxt = XmlDebugCtxt::default();

        ctxt.options |= DUMP_TEXT_TYPE;
        ctxt.output = output.map_or(Box::new(stdout()) as Box<dyn Write>, |o| Box::new(o));
        ctxt.dump_document_head(doc);
    }
}

/// Dumps debug information for the document, it's recursive
#[doc(alias = "xmlDebugDumpDocument")]
pub unsafe fn xml_debug_dump_document<'a>(output: Option<impl Write + 'a>, doc: Option<XmlDocPtr>) {
    unsafe {
        let mut ctxt = XmlDebugCtxt::default();

        ctxt.options |= DUMP_TEXT_TYPE;
        ctxt.output = output.map_or(Box::new(stdout()) as Box<dyn Write>, |o| Box::new(o));
        ctxt.dump_document(doc);
    }
}

/// Dumps debug information for the DTD
#[doc(alias = "xmlDebugDumpDTD")]
pub unsafe fn xml_debug_dump_dtd<'a>(output: Option<impl Write + 'a>, dtd: Option<XmlDtdPtr>) {
    unsafe {
        let mut ctxt = XmlDebugCtxt::default();

        ctxt.options |= DUMP_TEXT_TYPE;
        ctxt.output = output.map_or(Box::new(stdout()) as Box<dyn Write>, |o| Box::new(o));
        ctxt.dump_dtd(dtd);
    }
}

/// Dumps debug information for all the entities in use by the document
#[doc(alias = "xmlDebugDumpEntities")]
pub unsafe fn xml_debug_dump_entities<'a>(output: impl Write + 'a, doc: Option<XmlDocPtr>) {
    unsafe {
        let mut ctxt = XmlDebugCtxt {
            output: Box::new(output),
            ..Default::default()
        };
        ctxt.dump_entities(doc);
    }
}

/// Check the document for potential content problems, and output
/// the errors to `output`
///
/// Returns the number of errors found
#[doc(alias = "xmlDebugCheckDocument")]
pub unsafe fn xml_debug_check_document<'a>(
    output: Option<impl Write + 'a>,
    doc: Option<XmlDocPtr>,
) -> i32 {
    unsafe {
        let mut ctxt = XmlDebugCtxt {
            output: output.map_or(Box::new(stdout()) as Box<dyn Write>, |o| Box::new(o)),
            check: 1,
            ..Default::default()
        };
        ctxt.dump_document(doc);
        ctxt.errors
    }
}

/// Dump to `output` the type and name of @node.
#[doc(alias = "xmlLsOneNode")]
pub unsafe fn xml_ls_one_node<'a>(output: &mut (impl Write + 'a), node: Option<XmlGenericNodePtr>) {
    let Some(node) = node else {
        writeln!(output, "NULL").ok();
        return;
    };
    match node.element_type() {
        XmlElementType::XmlElementNode => write!(output, "-").ok(),
        XmlElementType::XmlAttributeNode => write!(output, "a").ok(),
        XmlElementType::XmlTextNode => write!(output, "t").ok(),
        XmlElementType::XmlCDATASectionNode => write!(output, "C").ok(),
        XmlElementType::XmlEntityRefNode => write!(output, "e").ok(),
        XmlElementType::XmlEntityNode => write!(output, "E").ok(),
        XmlElementType::XmlPINode => write!(output, "p").ok(),
        XmlElementType::XmlCommentNode => write!(output, "c").ok(),
        XmlElementType::XmlDocumentNode => write!(output, "d").ok(),
        XmlElementType::XmlHTMLDocumentNode => write!(output, "h").ok(),
        XmlElementType::XmlDocumentTypeNode => write!(output, "T").ok(),
        XmlElementType::XmlDocumentFragNode => write!(output, "F").ok(),
        XmlElementType::XmlNotationNode => write!(output, "N").ok(),
        XmlElementType::XmlNamespaceDecl => write!(output, "n").ok(),
        _ => write!(output, "?").ok(),
    };
    if node.element_type() != XmlElementType::XmlNamespaceDecl {
        if let Ok(node) = XmlNodePtr::try_from(node) {
            if node.properties.is_some() {
                write!(output, "a").ok();
            } else {
                write!(output, "-").ok();
            }
            if node.ns_def.is_some() {
                write!(output, "n").ok();
            } else {
                write!(output, "-").ok();
            }
        } else {
            write!(output, "--").ok();
        }
    }
    unsafe {
        write!(output, " {} ", xml_ls_count_node(Some(node))).ok();

        match node.element_type() {
            XmlElementType::XmlElementNode => {
                let node = XmlNodePtr::try_from(node).unwrap();
                if !node.name.is_null() {
                    if let Some(prefix) = node.ns.as_deref().and_then(|ns| ns.prefix()) {
                        write!(output, "{prefix}:").ok();
                    }
                    let name = CStr::from_ptr(node.name as *const i8).to_string_lossy();
                    write!(output, "{name}").ok();
                }
            }
            XmlElementType::XmlAttributeNode => {
                let node = XmlAttrPtr::try_from(node).unwrap();
                if !node.name.is_null() {
                    let name = CStr::from_ptr(node.name as *const i8).to_string_lossy();
                    write!(output, "{name}").ok();
                }
            }
            XmlElementType::XmlTextNode => {
                let node = XmlNodePtr::try_from(node).unwrap();
                if !node.content.is_null() {
                    let content = CStr::from_ptr(node.content as *const i8).to_string_lossy();
                    xml_debug_dump_string(Some(output), Some(&content));
                }
            }
            XmlElementType::XmlCDATASectionNode => {}
            XmlElementType::XmlEntityRefNode => {
                let node = XmlNodePtr::try_from(node).unwrap();
                if !node.name.is_null() {
                    let name = CStr::from_ptr(node.name as *const i8).to_string_lossy();
                    write!(output, "{name}").ok();
                }
            }
            XmlElementType::XmlEntityNode => {
                let node = XmlNodePtr::try_from(node).unwrap();
                if !node.name.is_null() {
                    let name = CStr::from_ptr(node.name as *const i8).to_string_lossy();
                    write!(output, "{name}").ok();
                }
            }
            XmlElementType::XmlPINode => {
                let node = XmlNodePtr::try_from(node).unwrap();
                if !node.name.is_null() {
                    let name = CStr::from_ptr(node.name as *const i8).to_string_lossy();
                    write!(output, "{name}").ok();
                }
            }
            XmlElementType::XmlCommentNode => {}
            XmlElementType::XmlDocumentNode => {}
            XmlElementType::XmlHTMLDocumentNode => {}
            XmlElementType::XmlDocumentTypeNode => {}
            XmlElementType::XmlDocumentFragNode => {}
            XmlElementType::XmlNotationNode => {}
            XmlElementType::XmlNamespaceDecl => {
                let ns = XmlNsPtr::try_from(node).unwrap();
                let href = CStr::from_ptr(ns.href as *const i8).to_string_lossy();
                if let Some(prefix) = ns.prefix() {
                    write!(output, "{prefix} -> {href}").ok();
                } else {
                    write!(output, "default -> {href}").ok();
                }
            }
            _ => {
                if let Some(name) = node.name() {
                    write!(output, "{name}").ok();
                }
            }
        }
        writeln!(output).ok();
    }
}

/// Count the children of @node.
///
/// Returns the number of children of @node.
#[doc(alias = "xmlLsCountNode")]
pub unsafe fn xml_ls_count_node(node: Option<XmlGenericNodePtr>) -> usize {
    let Some(node) = node else {
        return 0;
    };
    unsafe {
        let mut list = match node.element_type() {
            XmlElementType::XmlElementNode => node.children(),
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                XmlDocPtr::try_from(node).unwrap().children()
            }
            XmlElementType::XmlAttributeNode => XmlAttrPtr::try_from(node).unwrap().children(),
            XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlPINode
            | XmlElementType::XmlCommentNode => {
                let node = XmlNodePtr::try_from(node).unwrap();
                let content = node.content;
                return if !content.is_null() {
                    CStr::from_ptr(content as *const i8).to_bytes().len()
                } else {
                    0
                };
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
                return 1;
            }
            _ => unreachable!(),
        };
        let mut ret = 0;
        while let Some(now) = list {
            list = now.next();
            ret += 1;
        }
        ret
    }
}

/// Convenient way to turn bool into text
///
/// Returns a pointer to either "True" or "False"
#[doc(alias = "xmlBoolToText")]
pub fn xml_bool_to_text(boolval: bool) -> &'static str {
    if boolval { "True" } else { "False" }
}

/// This is a generic signature for the XML shell input function.
///
/// Returns a string which will be freed by the Shell.
#[doc(alias = "xmlShellReadlineFunc")]
#[cfg(feature = "xpath")]
pub type XmlShellReadlineFunc = fn(prompt: &str) -> Option<String>;

/// A debugging shell context.  
/// TODO: add the defined function tables.
#[doc(alias = "xmlShellCtxt")]
#[cfg(feature = "xpath")]
#[repr(C)]
pub struct XmlShellCtxt<'a> {
    filename: String,
    doc: Option<XmlDocPtr>,
    node: Option<XmlGenericNodePtr>,
    pctxt: XmlXPathContextPtr,
    loaded: i32,
    output: Box<dyn Write + 'a>,
    input: XmlShellReadlineFunc,
}

#[cfg(feature = "xpath")]
impl XmlShellCtxt<'_> {
    /// Implements the XML shell function "ls"
    /// Does an Unix like listing of the given node (like a directory)
    ///
    /// Returns 0
    #[doc(alias = "xmlShellList")]
    #[cfg(feature = "xpath")]
    pub unsafe fn xml_shell_list(
        &mut self,
        _arg: Option<&str>,
        node: Option<XmlGenericNodePtr>,
        _node2: Option<XmlGenericNodePtr>,
    ) -> i32 {
        unsafe {
            let Some(node) = node else {
                writeln!(self.output, "NULL").ok();
                return 0;
            };
            let mut cur = if let Ok(doc) = XmlDocPtr::try_from(node) {
                doc.children()
            } else if let Ok(ns) = XmlNsPtr::try_from(node) {
                xml_ls_one_node(&mut self.output, Some(ns.into()));
                return 0;
            } else if let Some(children) = node.children() {
                Some(children)
            } else {
                xml_ls_one_node(&mut self.output, Some(node));
                return 0;
            };
            while let Some(now) = cur {
                xml_ls_one_node(&mut self.output, Some(now));
                cur = now.next();
            }
            0
        }
    }

    /// Implements the XML shell function "base"
    /// dumps the current XML base of the node
    ///
    /// Returns 0
    #[doc(alias = "xmlShellBase")]
    #[cfg(feature = "xpath")]
    pub unsafe fn xml_shell_base(
        &mut self,
        _arg: Option<&str>,
        node: Option<XmlGenericNodePtr>,
        _node2: Option<XmlGenericNodePtr>,
    ) -> i32 {
        unsafe {
            let Some(node) = node else {
                writeln!(self.output, "NULL").ok();
                return 0;
            };

            if let Some(base) = node.get_base(node.document()) {
                writeln!(self.output, "{base}").ok();
            } else {
                writeln!(self.output, " No base found !!!").ok();
            }
            0
        }
    }

    /// Implements the XML shell function "dir"
    /// dumps information about the node (namespace, attributes, content).
    ///
    /// Returns 0
    #[doc(alias = "xmlShellDir")]
    #[cfg(feature = "xpath")]
    pub unsafe fn xml_shell_dir(
        &mut self,
        _arg: Option<&str>,
        node: Option<XmlGenericNodePtr>,
        _node2: Option<XmlGenericNodePtr>,
    ) -> i32 {
        unsafe {
            let Some(node) = node else {
                writeln!(self.output, "NULL").ok();
                return 0;
            };
            if let Ok(doc) = XmlDocPtr::try_from(node) {
                xml_debug_dump_document_head(Some(&mut self.output), Some(doc));
            } else if node.element_type() == XmlElementType::XmlAttributeNode {
                xml_debug_dump_attr(&mut self.output, XmlAttrPtr::try_from(node).ok(), 0);
            } else {
                xml_debug_dump_one_node(&mut self.output, Some(node), 0);
            }
            0
        }
    }

    /// Implements the XML shell function "load"
    /// loads a new document specified by the filename
    ///
    /// Returns 0 or -1 if loading failed
    #[doc(alias = "xmlShellLoad")]
    #[cfg(feature = "xpath")]
    pub unsafe fn xml_shell_load(
        &mut self,
        filename: &str,
        _node: Option<XmlGenericNodePtr>,
        _node2: Option<XmlGenericNodePtr>,
    ) -> i32 {
        unsafe {
            use crate::{
                libxml::htmlparser::html_parse_file,
                parser::xml_read_file,
                uri::canonic_path,
                xpath::{xml_xpath_free_context, xml_xpath_new_context},
            };

            use crate::tree::xml_free_doc;

            let mut html: i32 = 0;

            if let Some(doc) = self.doc {
                html = (doc.typ == XmlElementType::XmlHTMLDocumentNode) as i32;
            }

            let Some(doc) = (if html != 0 {
                #[cfg(feature = "html")]
                {
                    html_parse_file(filename, None)
                }
                #[cfg(not(feature = "html"))]
                {
                    write!(self.output, "HTML support not compiled in\n".as_ptr()).ok();
                    None
                }
            } else {
                xml_read_file(filename, None, 0)
            }) else {
                return -1;
            };
            if self.loaded == 1 {
                if let Some(doc) = self.doc {
                    xml_free_doc(doc);
                }
            }
            self.loaded = 1;
            #[cfg(feature = "xpath")]
            {
                xml_xpath_free_context(self.pctxt);
            }
            self.doc = Some(doc);
            self.node = Some(doc.into());
            #[cfg(feature = "xpath")]
            {
                self.pctxt = xml_xpath_new_context(Some(doc));
            }
            let canonic = canonic_path(filename);
            self.filename = canonic.into_owned();
            0
        }
    }

    /// Implements the XML shell function "cat"
    /// dumps the serialization node content (XML or HTML).
    ///
    /// Returns 0
    #[doc(alias = "xmlShellCat")]
    #[cfg(all(feature = "xpath", feature = "libxml_output"))]
    pub unsafe fn xml_shell_cat(
        &mut self,
        _arg: Option<&str>,
        node: Option<XmlGenericNodePtr>,
        _node2: Option<XmlGenericNodePtr>,
    ) -> i32 {
        unsafe {
            use crate::libxml::htmltree::{html_doc_dump, html_node_dump_file};

            let Some(node) = node else {
                writeln!(self.output, "NULL").ok();
                return 0;
            };
            if let Some(doc) = self
                .doc
                .filter(|doc| doc.typ == XmlElementType::XmlHTMLDocumentNode)
            {
                #[cfg(feature = "html")]
                if let Some(doc) = XmlDocPtr::try_from(node)
                    .ok()
                    .filter(|doc| doc.element_type() == XmlElementType::XmlHTMLDocumentNode)
                {
                    html_doc_dump(&mut self.output, doc);
                } else {
                    html_node_dump_file(&mut self.output, Some(doc), Some(node));
                }
                #[cfg(not(feature = "html"))]
                if let Some(mut doc) = XmlDocPtr::try_from(node)
                    .ok()
                    .filter(|doc| doc.element_type() == XmlElementType::XmlDocumentNode)
                {
                    doc.dump_file(self.output);
                } else {
                    xml_elem_dump(self.output, self.doc, node);
                }
            } else if let Some(mut doc) = XmlDocPtr::try_from(node)
                .ok()
                .filter(|doc| doc.element_type() == XmlElementType::XmlDocumentNode)
            {
                doc.dump_file(&mut self.output);
            } else {
                // Is this `unwrap` OK ????
                XmlNodePtr::try_from(node)
                    .unwrap()
                    .dump_file(&mut self.output, self.doc);
            }
            writeln!(self.output).ok();
            0
        }
    }

    /// Implements the XML shell function "write"
    /// Write the current node to the filename, it saves the serialization
    /// of the subtree under the @node specified
    ///
    /// Returns 0 or -1 in case of error
    #[doc(alias = "xmlShellWrite")]
    #[cfg(all(feature = "xpath", feature = "libxml_output"))]
    pub unsafe fn xml_shell_write(
        &mut self,
        filename: &str,
        node: XmlGenericNodePtr,
        _node2: Option<XmlNodePtr>,
    ) -> i32 {
        unsafe {
            use std::fs::File;

            use crate::libxml::htmltree::html_save_file;

            if filename.is_empty() {
                return -1;
            }
            let cfilename = CString::new(filename).unwrap();
            match node.element_type() {
                XmlElementType::XmlDocumentNode => {
                    if self.doc.is_none_or(|mut doc| doc.save_file(filename) < -1) {
                        generic_error!("Failed to write to {filename}\n");
                        return -1;
                    }
                }
                XmlElementType::XmlHTMLDocumentNode => {
                    #[cfg(feature = "html")]
                    if html_save_file(cfilename.as_ptr(), self.doc.unwrap()) < 0 {
                        generic_error!("Failed to write to {filename}\n");
                        return -1;
                    }
                    #[cfg(not(feature = "html"))]
                    if xml_save_file(filename as *mut c_char, self.doc) < -1 {
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
                            let mut node = XmlNodePtr::try_from(node).unwrap();
                            node.dump_file(&mut f, self.doc);
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
    }

    /// Implements the XML shell function "save"
    /// Write the current document to the filename, or it's original name
    ///
    /// Returns 0 or -1 in case of error
    #[doc(alias = "xmlShellSave")]
    #[cfg(all(feature = "xpath", feature = "libxml_output"))]
    pub unsafe fn xml_shell_save(
        &mut self,
        filename: Option<&str>,
        _node: Option<XmlGenericNodePtr>,
        _node2: Option<XmlGenericNodePtr>,
    ) -> i32 {
        unsafe {
            use crate::libxml::htmltree::html_save_file;

            let Some(mut doc) = self.doc else {
                return -1;
            };
            let filename = filename
                .filter(|filename| !filename.is_empty())
                .unwrap_or(&self.filename);
            match doc.typ {
                XmlElementType::XmlDocumentNode => {
                    if doc.save_file(filename) < 0 {
                        generic_error!("Failed to save to {}\n", filename);
                    }
                }
                XmlElementType::XmlHTMLDocumentNode => {
                    let cfilename = CString::new(filename).unwrap();
                    #[cfg(feature = "html")]
                    if html_save_file(cfilename.as_ptr() as *mut c_char, doc) < 0 {
                        generic_error!("Failed to save to {}\n", filename);
                    }
                    #[cfg(not(feature = "html"))]
                    if (xml_save_file(filename as *mut c_char, self.doc) < 0) {
                        generic_error!("Failed to save to {}\n", filename);
                    }
                }
                _ => {
                    generic_error!("To save to subparts of a document use the 'write' command\n");
                    return -1;
                }
            }
            0
        }
    }

    /// Implements the XML shell function "validate"
    /// Validate the document, if a DTD path is provided, then the validation
    /// is done against the given DTD.
    ///
    /// Returns 0 or -1 in case of error
    #[doc(alias = "xmlShellValidate")]
    #[cfg(all(feature = "xpath", feature = "libxml_valid"))]
    pub unsafe fn xml_shell_validate(
        &mut self,
        dtd: Option<&str>,
        _node: Option<XmlGenericNodePtr>,
        _node2: Option<XmlGenericNodePtr>,
    ) -> i32 {
        use crate::libxml::valid::XmlValidCtxt;

        unsafe {
            use crate::{
                globals::GLOBAL_STATE,
                libxml::{
                    parser::xml_parse_dtd,
                    valid::{xml_validate_document, xml_validate_dtd},
                },
                tree::xml_free_dtd,
            };

            let mut vctxt = XmlValidCtxt::default();
            let mut res: i32 = -1;

            let Some(doc) = self.doc else {
                return -1;
            };
            vctxt.error = Some(GLOBAL_STATE.with_borrow(|state| state.generic_error));
            vctxt.warning = vctxt.error;

            if let Some(dtd) = dtd.filter(|dtd| !dtd.is_empty()) {
                let subset = xml_parse_dtd(None, Some(dtd));
                if let Some(subset) = subset {
                    res = xml_validate_dtd(addr_of_mut!(vctxt), doc, subset);
                    xml_free_dtd(subset);
                }
            } else {
                res = xml_validate_document(addr_of_mut!(vctxt), doc);
            }
            res
        }
    }

    /// Implements the XML shell function "du"
    /// show the structure of the subtree under node @tree
    /// If @tree is null, the command works on the current node.
    ///
    /// Returns 0 or -1 in case of error
    #[doc(alias = "xmlShellDu")]
    #[cfg(feature = "xpath")]
    pub unsafe fn xml_shell_du(
        &mut self,
        _arg: Option<&str>,
        tree: XmlGenericNodePtr,
        _node2: Option<XmlGenericNodePtr>,
    ) -> i32 {
        unsafe {
            let mut indent: i32 = 0;

            let mut node = Some(tree);
            while let Some(now) = node {
                if now.element_type() == XmlElementType::XmlDocumentNode
                    || now.element_type() == XmlElementType::XmlHTMLDocumentNode
                {
                    writeln!(self.output, "/").ok();
                } else if let Some(node) = XmlNodePtr::try_from(now)
                    .ok()
                    .filter(|node| node.element_type() == XmlElementType::XmlElementNode)
                {
                    for _ in 0..indent {
                        write!(self.output, "  ").ok();
                    }
                    if let Some(prefix) = node.ns.as_deref().and_then(|ns| ns.prefix()) {
                        write!(self.output, "{prefix}:").ok();
                    }
                    let name = CStr::from_ptr(node.name as *const i8).to_string_lossy();
                    writeln!(self.output, "{name}").ok();
                }

                // Browse the full subtree, deep first
                if let Ok(doc) = XmlDocPtr::try_from(now) {
                    node = doc.children;
                } else if let Some(children) = now
                    .children()
                    .filter(|_| now.element_type() != XmlElementType::XmlEntityRefNode)
                {
                    // deep first
                    node = Some(children);
                    indent += 1;
                } else if let Some(next) = now.next().filter(|_| node != Some(tree)) {
                    // then siblings
                    node = Some(next);
                } else if node != Some(tree) {
                    // go up to parents->next if needed
                    while node != Some(tree) {
                        if let Some(parent) = now.parent() {
                            node = Some(parent);
                            indent -= 1;
                        }
                        if let Some(next) = now.next().filter(|_| node != Some(tree)) {
                            node = Some(next);
                            break;
                        }
                        if now.parent().is_none() {
                            node = None;
                            break;
                        }
                        if node == Some(tree) {
                            node = None;
                            break;
                        }
                    }
                    // exit condition
                    if node == Some(tree) {
                        node = None;
                    }
                } else {
                    node = None;
                }
            }
            0
        }
    }

    /// Implements the XML shell function "pwd"
    /// Show the full path from the root to the node, if needed building
    /// thumblers when similar elements exists at a given ancestor level.
    /// The output is compatible with XPath commands.
    ///
    /// Returns 0 or -1 in case of error
    #[doc(alias = "xmlShellPwd")]
    #[cfg(feature = "xpath")]
    pub unsafe fn xml_shell_pwd(
        &mut self,
        buffer: &mut String,
        node: Option<XmlGenericNodePtr>,
        _node2: Option<XmlGenericNodePtr>,
    ) -> i32 {
        let Some(node) = node else {
            return -1;
        };
        unsafe {
            let Some(path) = node.get_node_path() else {
                return -1;
            };

            *buffer = path;
        }
        0
    }

    /// Implements the XML shell function "relaxng"
    /// validating the instance against a Relax-NG schemas
    ///
    /// Returns 0
    #[doc(alias = "xmlShellRNGValidate")]
    #[cfg(feature = "schema")]
    unsafe fn xml_shell_rng_validate(
        &mut self,
        schemas: &str,
        _node: Option<XmlGenericNodePtr>,
        _node2: Option<XmlGenericNodePtr>,
    ) -> i32 {
        use crate::libxml::relaxng::XmlRelaxNGPtr;

        unsafe {
            use crate::{
                globals::GLOBAL_STATE,
                libxml::relaxng::{
                    xml_relaxng_free, xml_relaxng_parse, xml_relaxng_set_valid_errors,
                    xml_relaxng_validate_doc,
                },
                relaxng::{
                    xml_relaxng_free_parser_ctxt, xml_relaxng_free_valid_ctxt,
                    xml_relaxng_new_parser_ctxt, xml_relaxng_new_valid_ctxt,
                },
            };

            let ctxt = xml_relaxng_new_parser_ctxt(schemas);
            let generic_error = GLOBAL_STATE.with_borrow(|state| state.generic_error);
            (*ctxt).set_parser_errors(Some(generic_error), Some(generic_error), None);
            let relaxngschemas: XmlRelaxNGPtr = xml_relaxng_parse(ctxt);
            xml_relaxng_free_parser_ctxt(ctxt);
            if relaxngschemas.is_null() {
                generic_error!("Relax-NG schema {} failed to compile\n", schemas);
                return -1;
            }
            let vctxt = xml_relaxng_new_valid_ctxt(relaxngschemas);
            xml_relaxng_set_valid_errors(vctxt, Some(generic_error), Some(generic_error), None);
            let ret = if let Some(doc) = self.doc {
                xml_relaxng_validate_doc(vctxt, doc)
            } else {
                -1
            };

            match ret.cmp(&0) {
                std::cmp::Ordering::Equal => {
                    eprintln!("{} validates", self.filename);
                }
                std::cmp::Ordering::Greater => {
                    eprintln!("{} fails to validate", self.filename);
                }
                std::cmp::Ordering::Less => {
                    eprintln!("{} validation generated an internal error", self.filename);
                }
            }
            xml_relaxng_free_valid_ctxt(vctxt);
            if !relaxngschemas.is_null() {
                xml_relaxng_free(relaxngschemas);
            }
            0
        }
    }

    /// Implements the XML shell function "grep"
    /// dumps information about the node (namespace, attributes, content).
    ///
    /// Returns 0
    #[doc(alias = "xmlShellGrep")]
    unsafe fn xml_shell_grep(
        &mut self,
        arg: Option<&str>,
        mut node: Option<XmlGenericNodePtr>,
        _node2: Option<XmlGenericNodePtr>,
    ) -> i32 {
        unsafe {
            if node.is_none() {
                return 0;
            }
            let Some(arg) = arg else {
                return 0;
            };
            // what does the following do... ?
            // #[cfg(feature = "regexp")]
            // if !xmlStrchr(arg as *mut xmlChar, b'?').is_null()
            //     || !xmlStrchr(arg as *mut xmlChar, b'*').is_null()
            //     || !xmlStrchr(arg as *mut xmlChar, b'.').is_null()
            //     || !xmlStrchr(arg as *mut xmlChar, b'[').is_null()
            // {}
            while let Some(now) = node {
                if let Ok(now) = XmlNodePtr::try_from(now) {
                    let arg = CString::new(arg).unwrap();
                    if now.element_type() == XmlElementType::XmlCommentNode {
                        if !xml_strstr(now.content, arg.as_ptr() as *mut u8).is_null() {
                            let path = now.get_node_path().unwrap();
                            write!(self.output, "{path} : ").ok();
                            self.xml_shell_list(None, Some(now.into()), None);
                        }
                    } else if now.element_type() == XmlElementType::XmlTextNode
                        && !xml_strstr(now.content, arg.as_ptr() as *mut u8).is_null()
                    {
                        let path = now.parent().unwrap().get_node_path().unwrap();
                        write!(self.output, "{path} : ").ok();
                        self.xml_shell_list(None, now.parent, None);
                    }
                }

                // Browse the full subtree, deep first
                if let Ok(doc) = XmlDocPtr::try_from(now) {
                    node = doc.children;
                } else if let Some(children) = now
                    .children()
                    .filter(|_| now.element_type() != XmlElementType::XmlEntityRefNode)
                {
                    // deep first
                    node = Some(children);
                } else if let Some(next) = now.next() {
                    // then siblings
                    node = Some(next);
                } else {
                    // go up to parents->next if needed
                    while let Some(now) = node {
                        if let Some(parent) = now.parent() {
                            node = Some(parent);
                        }
                        if let Some(next) = now.next() {
                            node = Some(next);
                            break;
                        }
                        if now.parent().is_none() {
                            node = None;
                            break;
                        }
                    }
                }
            }
            0
        }
    }

    /// Implements the XML shell function "dir"
    /// dumps information about the node (namespace, attributes, content).
    ///
    /// Returns 0
    #[doc(alias = "xmlShellSetContent")]
    unsafe fn xml_shell_set_content(
        &mut self,
        value: Option<&str>,
        node: Option<XmlGenericNodePtr>,
        _node2: Option<XmlGenericNodePtr>,
    ) -> i32 {
        unsafe {
            let Some(mut node) = node else {
                writeln!(self.output, "NULL").ok();
                return 0;
            };
            let Some(value) = value else {
                writeln!(self.output, "NULL").ok();
                return 0;
            };

            let mut results = None;
            let ret = xml_parse_in_node_context(node, value.as_bytes().to_vec(), 0, &mut results);
            if ret == XmlParserErrors::XmlErrOK {
                if let Some(children) = node.children() {
                    xml_free_node_list(Some(children));
                    node.set_children(None);
                    node.set_last(None);
                }
                node.add_child_list(results.unwrap());
            } else {
                writeln!(self.output, "failed to parse content").ok();
            }
            0
        }
    }

    /// Implements the XML shell function "setns"
    /// register/unregister a prefix=namespace pair on the XPath context
    ///
    /// Returns 0 on success and a negative value otherwise.
    #[doc(alias = "xmlShellRegisterNamespace")]
    #[cfg(feature = "xpath")]
    unsafe fn xml_shell_register_namespace(
        &mut self,
        arg: Option<&str>,
        _node: Option<XmlGenericNodePtr>,
        _node2: Option<XmlGenericNodePtr>,
    ) -> i32 {
        unsafe {
            use crate::xpath::internals::xml_xpath_register_ns;

            let Some(mut arg) = arg else {
                return 0;
            };

            arg = arg.trim_start();
            while !arg.is_empty() {
                let Some((prefix, rem)) = arg.split_once('=') else {
                    writeln!(self.output, "setns: prefix=[nsuri] required").ok();
                    return -1;
                };
                let (href, rem) = rem.split_once(' ').unwrap_or((rem, ""));
                arg = rem.trim_start();

                // do register namespace
                if xml_xpath_register_ns(self.pctxt, prefix, Some(href)) != 0 {
                    writeln!(
                        self.output,
                        "Error: unable to register NS with prefix=\"{prefix}\" and href=\"{href}\""
                    )
                    .ok();
                    return -1;
                }
            }
            0
        }
    }

    /// Implements the XML shell function "setrootns"
    /// which registers all namespaces declarations found on the root element.
    ///
    /// Returns 0 on success and a negative value otherwise.
    #[doc(alias = "xmlShellRegisterRootNamespaces")]
    #[cfg(feature = "xpath")]
    unsafe fn xml_shell_register_root_namespaces(
        &mut self,
        _arg: Option<&str>,
        root: XmlNodePtr,
        _node2: Option<XmlGenericNodePtr>,
    ) -> i32 {
        unsafe {
            use crate::xpath::internals::xml_xpath_register_ns;

            if root.element_type() != XmlElementType::XmlElementNode
                || root.ns_def.is_none()
                || self.pctxt.is_null()
            {
                return -1;
            }
            let mut ns = root.ns_def;
            while let Some(now) = ns {
                if let Some(prefix) = now.prefix() {
                    xml_xpath_register_ns(self.pctxt, &prefix, now.href().as_deref());
                } else {
                    xml_xpath_register_ns(self.pctxt, "defaultns", now.href().as_deref());
                }
                ns = now.next;
            }
            0
        }
    }

    /// Implements the XML shell function "setbase"
    /// change the current XML base of the node
    ///
    /// Returns 0
    #[doc(alias = "xmlShellSetBase")]
    #[cfg(feature = "libxml_tree")]
    unsafe fn xml_shell_set_base(
        &mut self,
        arg: Option<&str>,
        node: XmlGenericNodePtr,
        _node2: Option<XmlGenericNodePtr>,
    ) -> i32 {
        unsafe {
            node.set_base(arg);
            0
        }
    }
}

/// This is a generic signature for the XML shell functions.
///
/// Returns an int, negative returns indicating errors.
#[doc(alias = "xmlShellCmd")]
#[cfg(feature = "xpath")]
pub type XmlShellCmd = unsafe fn(
    ctxt: &mut XmlShellCtxt<'_>,
    arg: Option<&str>,
    node: Option<XmlGenericNodePtr>,
    node2: Option<XmlGenericNodePtr>,
) -> i32;

/// Print the xpath error to libxml default error channel
#[doc(alias = "xmlShellPrintXPathError")]
#[cfg(feature = "xpath")]
pub fn xml_shell_print_xpath_error(error_type: XmlXPathObjectType, arg: Option<&str>) {
    use crate::generic_error;

    let arg = arg.unwrap_or("Result");

    match error_type {
        XmlXPathObjectType::XPathUndefined => generic_error!("{arg}: no such node\n"),
        XmlXPathObjectType::XPathBoolean => generic_error!("{arg} is a Boolean\n"),
        XmlXPathObjectType::XPathNumber => generic_error!("{arg} is a number\n"),
        XmlXPathObjectType::XPathString => generic_error!("{arg} is a string\n"),
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XPathPoint => generic_error!("{arg} is a point\n"),
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XPathRange => generic_error!("{arg} is a range\n"),
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XPathLocationset => generic_error!("{arg} is a range\n"),
        XmlXPathObjectType::XPathUsers => generic_error!("{arg} is user-defined\n"),
        XmlXPathObjectType::XPathXSLTTree => generic_error!("{arg} is an XSLT value tree\n"),
        _ => unreachable!(),
    }
}

/// Print node to the output FILE
#[doc(alias = "xmlShellPrintNodeCtxt")]
#[cfg(feature = "libxml_output")]
unsafe fn xml_shell_print_node_ctxt(ctxt: Option<&mut XmlShellCtxt<'_>>, node: XmlGenericNodePtr) {
    unsafe {
        let stdout = &mut stdout();
        let fp = if let Some(ctxt) = ctxt {
            ctxt.output.as_mut()
        } else {
            stdout as &mut dyn Write
        };
        let mut boxed = Box::new(fp);
        if let Some(mut doc) = XmlDocPtr::try_from(node)
            .ok()
            .filter(|doc| doc.element_type() == XmlElementType::XmlDocumentNode)
        {
            doc.dump_file(&mut boxed);
        } else if node.element_type() == XmlElementType::XmlAttributeNode {
            xml_debug_dump_attr_list(&mut boxed, XmlAttrPtr::try_from(node).ok(), 0);
        } else {
            // Is this `unwrap` OK ?????
            XmlNodePtr::try_from(node)
                .unwrap()
                .dump_file(&mut boxed, node.document());
        }

        writeln!(boxed).ok();
    }
}

/// Prints result to the output FILE
#[doc(alias = "xmlShellPrintXPathResultCtxt")]
unsafe fn xml_shell_print_xpath_result_ctxt(
    mut ctxt: Option<&mut XmlShellCtxt<'_>>,
    list: XmlXPathObjectPtr,
) {
    unsafe {
        if !list.is_null() {
            match (*list).typ {
                XmlXPathObjectType::XPathNodeset => {
                    #[cfg(feature = "libxml_output")]
                    if let Some(nodeset) = (*list).nodesetval.as_deref() {
                        for &node in &nodeset.node_tab {
                            xml_shell_print_node_ctxt(ctxt.as_deref_mut(), node);
                        }
                    } else {
                        generic_error!("Empty node set\n");
                    }
                    #[cfg(not(feature = "libxml_output"))]
                    {
                        generic_error!("Node set\n");
                    }
                }
                XmlXPathObjectType::XPathBoolean => {
                    generic_error!("Is a Boolean:{}\n", xml_bool_to_text((*list).boolval));
                }
                XmlXPathObjectType::XPathNumber => {
                    generic_error!("Is a number:{}\n", (*list).floatval);
                }
                XmlXPathObjectType::XPathString => {
                    generic_error!("Is a string:{}\n", (*list).stringval.as_deref().unwrap());
                }
                _ => {
                    xml_shell_print_xpath_error((*list).typ, None);
                }
            }
        }
    }
}

/// Prints result to the output FILE
#[doc(alias = "xmlShellPrintXPathResult")]
#[cfg(feature = "xpath")]
pub unsafe fn xml_shell_print_xpath_result(list: XmlXPathObjectPtr) {
    unsafe {
        xml_shell_print_xpath_result_ctxt(None, list);
    }
}

/// Print node to the output FILE
#[doc(alias = "xmlShellPrintNode")]
#[cfg(all(feature = "xpath", feature = "libxml_output"))]
pub unsafe fn xml_shell_print_node(node: XmlGenericNodePtr) {
    unsafe {
        xml_shell_print_node_ctxt(None, node);
    }
}

const XML_SHELL_HELP_CONTENT: &str = r#"
    base         display XML base of the node
    setbase URI  change the XML base of the node
    bye          leave shell
    cat [node]   display node or current node
    cd [path]    change directory to path or to root
    dir [path]   dumps information about the node (namespace, attributes, content)
    du [path]    show the structure of the subtree under path or the current node
    exit         leave shell
    help         display this help
    free         display memory usage
    load [name]  load a new document with name
    ls [path]    list contents of path or the current directory
    set xml_fragment replace the current node content with the fragment parsed in context
    pwd          display current working directory
    whereis      display absolute path of [path] or current working directory
    quit         leave shell
    grep string  search for a string in the subtree
"#;
const XML_SHELL_HELP_CONTENT_XPATH: &str = r#"
    xpath expr   evaluate the XPath expression in that context and print the result
    setns nsreg  register a namespace to a prefix in the XPath evaluation context
                 format for nsreg is: prefix=[nsuri] (i.e. prefix= unsets a prefix)
    setrootns    register all namespace found on the root element
                 the default namespace if any uses 'defaultns' prefix
"#;
const XML_SHELL_HELP_CONTENT_OUTPUT: &str = r#"
    save [name]  save this document to name or the original name
    write [name] write the current node to the filename
"#;
const XML_SHELL_HELP_CONTENT_VALID: &str = r#"
    validate     check the document for errors
"#;
const XML_SHELL_HELP_CONTENT_SCHEMA: &str = r#"
    relaxng rng  validate the document against the Relax-NG schemas
"#;

/// Implements the XML shell
/// This allow to load, validate, view, modify and save a document
/// using a environment similar to a UNIX commandline.
#[doc(alias = "xmlShell")]
#[cfg(feature = "xpath")]
pub unsafe fn xml_shell<'a>(
    doc: XmlDocPtr,
    filename: &str,
    input: Option<XmlShellReadlineFunc>,
    output: Option<impl Write + 'a>,
) {
    unsafe {
        use crate::{
            libxml::xmlmemory::xml_mem_show,
            tree::xml_free_doc,
            xpath::{
                xml_xpath_debug_dump_object, xml_xpath_eval, xml_xpath_free_context,
                xml_xpath_free_object, xml_xpath_new_context,
            },
        };

        let mut list: XmlXPathObjectPtr;

        if input.is_none() {
            return;
        }

        let mut ctxt = XmlShellCtxt {
            loaded: 0,
            doc: Some(doc),
            input: input.unwrap(),
            output: output.map_or(Box::new(stdout()) as Box<dyn Write + 'a>, |o| Box::new(o)),
            filename: filename.into(),
            node: Some(doc.into()),
            pctxt: null_mut(),
        };

        #[cfg(feature = "xpath")]
        {
            ctxt.pctxt = xml_xpath_new_context(Some(doc));
            if ctxt.pctxt.is_null() {
                return;
            }
        }
        let mut prompt = String::with_capacity(500);
        loop {
            prompt.clear();
            if ctxt.node == Some(doc.into()) {
                prompt.push_str("/ > ");
            } else if let Some((prefix, name)) = ctxt
                .node
                .and_then(|node| XmlNodePtr::try_from(node).ok())
                .as_deref()
                .and_then(|node| node.ns.as_deref().map(|ns| (node, ns)))
                .and_then(|(node, ns)| ns.prefix().zip(node.name()))
            {
                prompt.push_str(&prefix);
                prompt.push(':');
                prompt.push_str(&name);
                prompt.push_str(" > ");
            } else if let Some(name) = ctxt.node.as_deref().and_then(|node| node.name()) {
                prompt.push_str(&name);
                prompt.push_str(" > ");
            } else {
                prompt.push_str("? > ");
            }

            // Get a new command line
            let Some(cmdline) = (ctxt.input)(&prompt) else {
                break;
            };

            // Parse the command itself
            let mut cur = cmdline
                .split([' ', '\t', '\n', '\r'])
                .filter(|token| !token.is_empty());
            let Some(command) = cur.next() else {
                continue;
            };

            // Parse the argument
            let arg = cur.next().unwrap_or("");

            // start interpreting the command
            match command {
                "exit" => break,
                "quit" => break,
                "bye" => break,
                "help" => {
                    writeln!(ctxt.output, "{}", XML_SHELL_HELP_CONTENT).ok();
                    #[cfg(feature = "xpath")]
                    {
                        writeln!(ctxt.output, "{}", XML_SHELL_HELP_CONTENT_XPATH).ok();
                    }
                    #[cfg(feature = "libxml_output")]
                    {
                        writeln!(ctxt.output, "{}", XML_SHELL_HELP_CONTENT_OUTPUT).ok();
                    }
                    #[cfg(feature = "libxml_valid")]
                    {
                        writeln!(ctxt.output, "{}", XML_SHELL_HELP_CONTENT_VALID).ok();
                    }
                    #[cfg(feature = "schema")]
                    {
                        writeln!(ctxt.output, "{}", XML_SHELL_HELP_CONTENT_SCHEMA).ok();
                    }
                }
                #[cfg(feature = "libxml_valid")]
                "validate" => {
                    ctxt.xml_shell_validate(Some(arg), None, None);
                }
                "load" => {
                    ctxt.xml_shell_load(arg, None, None);
                }
                #[cfg(feature = "schema")]
                "relaxng" => {
                    ctxt.xml_shell_rng_validate(arg, None, None);
                }
                #[cfg(feature = "libxml_output")]
                "save" => {
                    ctxt.xml_shell_save(Some(arg), None, None);
                }
                #[cfg(feature = "libxml_output")]
                "write" => {
                    if arg.is_empty() {
                        generic_error!("Write command requires a filename argument\n");
                    } else {
                        ctxt.xml_shell_write(arg, ctxt.node.unwrap(), None);
                    }
                }
                "grep" => {
                    ctxt.xml_shell_grep(Some(arg), ctxt.node, None);
                }
                "free" => {
                    if arg.is_empty() {
                        xml_mem_show(&mut ctxt.output, 0);
                    } else {
                        let len = arg.parse::<i32>().unwrap_or_default();
                        xml_mem_show(&mut ctxt.output, len);
                    }
                }
                "pwd" => {
                    let mut dir = String::with_capacity(500);

                    if ctxt.xml_shell_pwd(&mut dir, ctxt.node, None) == 0 {
                        writeln!(ctxt.output, "{dir}").ok();
                    }
                }
                "du" => {
                    if arg.is_empty() {
                        ctxt.xml_shell_du(None, ctxt.node.unwrap(), None);
                        continue;
                    }
                    (*ctxt.pctxt).node = ctxt.node;
                    #[cfg(feature = "xpath")]
                    {
                        (*ctxt.pctxt).node = ctxt.node;
                        list = xml_xpath_eval(arg, ctxt.pctxt);
                    }
                    #[cfg(not(feature = "xpath"))]
                    {
                        list = null_mut();
                    }
                    if !list.is_null() {
                        match (*list).typ {
                            XmlXPathObjectType::XPathUndefined => {
                                generic_error!("{}: no such node\n", arg);
                            }
                            XmlXPathObjectType::XPathNodeset => {
                                if let Some(nodeset) = (*list).nodesetval.as_deref() {
                                    for &node in &nodeset.node_tab {
                                        ctxt.xml_shell_du(None, node, None);
                                    }
                                }
                            }
                            XmlXPathObjectType::XPathBoolean => {
                                generic_error!("{} is a Boolean\n", arg);
                            }
                            XmlXPathObjectType::XPathNumber => {
                                generic_error!("{} is a number\n", arg);
                            }
                            XmlXPathObjectType::XPathString => {
                                generic_error!("{} is a string\n", arg);
                            }
                            #[cfg(feature = "libxml_xptr_locs")]
                            XmlXPathObjectType::XPathPoint => {
                                generic_error!("{} is a point\n", arg);
                            }
                            #[cfg(feature = "libxml_xptr_locs")]
                            XmlXPathObjectType::XPathRange => {
                                generic_error!("{} is a range\n", arg);
                            }
                            #[cfg(feature = "libxml_xptr_locs")]
                            XmlXPathObjectType::XPathLocationset => {
                                generic_error!("{} is a range\n", arg);
                            }
                            XmlXPathObjectType::XPathUsers => {
                                generic_error!("{} is user-defined\n", arg);
                            }
                            XmlXPathObjectType::XPathXSLTTree => {
                                generic_error!("{} is an XSLT value tree\n", arg);
                            }
                        }
                        #[cfg(feature = "xpath")]
                        {
                            xml_xpath_free_object(list);
                        }
                    } else {
                        generic_error!("{}: no such node\n", arg);
                    }
                    (*ctxt.pctxt).node = None;
                }
                "base" => {
                    ctxt.xml_shell_base(None, ctxt.node, None);
                }
                "set" => {
                    ctxt.xml_shell_set_content(Some(arg), ctxt.node, None);
                }
                #[cfg(feature = "xpath")]
                "setns" => {
                    if arg.is_empty() {
                        generic_error!("setns: prefix=[nsuri] required\n");
                    } else {
                        ctxt.xml_shell_register_namespace(Some(arg), None, None);
                    }
                }
                #[cfg(feature = "xpath")]
                "setrootns" => {
                    let root = doc.get_root_element();
                    ctxt.xml_shell_register_root_namespaces(None, root.unwrap(), None);
                }
                #[cfg(feature = "xpath")]
                "xpath" => {
                    if arg.is_empty() {
                        generic_error!("xpath: expression required\n");
                    } else {
                        (*ctxt.pctxt).node = ctxt.node;
                        list = xml_xpath_eval(arg, ctxt.pctxt);
                        xml_xpath_debug_dump_object(&mut ctxt.output, list, 0);
                        xml_xpath_free_object(list);
                    }
                }
                #[cfg(feature = "libxml_tree")]
                "setbase" => {
                    ctxt.xml_shell_set_base(Some(arg), ctxt.node.unwrap(), None);
                }
                "ls" | "dir" => {
                    let dir = command == "dir";

                    if arg.is_empty() {
                        if dir {
                            ctxt.xml_shell_dir(None, ctxt.node, None);
                        } else {
                            ctxt.xml_shell_list(None, ctxt.node, None);
                        }
                        continue;
                    }
                    (*ctxt.pctxt).node = ctxt.node;
                    #[cfg(feature = "xpath")]
                    {
                        (*ctxt.pctxt).node = ctxt.node;
                        list = xml_xpath_eval(arg, ctxt.pctxt);
                    }
                    #[cfg(not(feature = "xpath"))]
                    {
                        list = null_mut();
                    }
                    if !list.is_null() {
                        match (*list).typ {
                            XmlXPathObjectType::XPathUndefined => {
                                generic_error!("{}: no such node\n", arg);
                            }
                            XmlXPathObjectType::XPathNodeset => {
                                let Some(nodeset) = (*list).nodesetval.as_deref() else {
                                    break;
                                };

                                for &node in &nodeset.node_tab {
                                    if dir {
                                        ctxt.xml_shell_dir(None, Some(node), None);
                                    } else {
                                        ctxt.xml_shell_list(None, Some(node), None);
                                    }
                                }
                            }
                            XmlXPathObjectType::XPathBoolean => {
                                generic_error!("{} is a Boolean\n", arg);
                            }
                            XmlXPathObjectType::XPathNumber => {
                                generic_error!("{} is a number\n", arg);
                            }
                            XmlXPathObjectType::XPathString => {
                                generic_error!("{} is a string\n", arg);
                            }
                            #[cfg(feature = "libxml_xptr_locs")]
                            XmlXPathObjectType::XPathPoint => {
                                generic_error!("{} is a point\n", arg);
                            }
                            #[cfg(feature = "libxml_xptr_locs")]
                            XmlXPathObjectType::XPathRange => {
                                generic_error!("{} is a range\n", arg);
                            }
                            #[cfg(feature = "libxml_xptr_locs")]
                            XmlXPathObjectType::XPathLocationset => {
                                generic_error!("{} is a range\n", arg);
                            }
                            XmlXPathObjectType::XPathUsers => {
                                generic_error!("{} is user-defined\n", arg);
                            }
                            XmlXPathObjectType::XPathXSLTTree => {
                                generic_error!("{} is an XSLT value tree\n", arg);
                            }
                        }
                        #[cfg(feature = "xpath")]
                        {
                            xml_xpath_free_object(list);
                        }
                    } else {
                        generic_error!("{}: no such node\n", arg);
                    }
                    (*ctxt.pctxt).node = None;
                }
                "whereis" => {
                    let mut dir = String::with_capacity(500);

                    if arg.is_empty() {
                        if ctxt.xml_shell_pwd(&mut dir, ctxt.node, None) == 0 {
                            writeln!(ctxt.output, "{dir}").ok();
                        }
                    } else {
                        (*ctxt.pctxt).node = ctxt.node;
                        #[cfg(feature = "xpath")]
                        {
                            list = xml_xpath_eval(arg, ctxt.pctxt);
                        }
                        #[cfg(not(feature = "xpath"))]
                        {
                            list = null_mut();
                        }
                        if !list.is_null() {
                            match (*list).typ {
                                XmlXPathObjectType::XPathUndefined => {
                                    generic_error!("{}: no such node\n", arg);
                                }
                                XmlXPathObjectType::XPathNodeset => {
                                    if let Some(nodeset) = (*list).nodesetval.as_deref() {
                                        for &node in &nodeset.node_tab {
                                            dir.clear();
                                            if ctxt.xml_shell_pwd(&mut dir, Some(node), None) == 0 {
                                                writeln!(ctxt.output, "{dir}").ok();
                                            }
                                        }
                                    }
                                }
                                XmlXPathObjectType::XPathBoolean => {
                                    generic_error!("{} is a Boolean\n", arg);
                                }
                                XmlXPathObjectType::XPathNumber => {
                                    generic_error!("{} is a number\n", arg);
                                }
                                XmlXPathObjectType::XPathString => {
                                    generic_error!("{} is a string\n", arg);
                                }
                                #[cfg(feature = "libxml_xptr_locs")]
                                XmlXPathObjectType::XPathPoint => {
                                    generic_error!("{} is a point\n", arg);
                                }
                                #[cfg(feature = "libxml_xptr_locs")]
                                XmlXPathObjectType::XPathRange => {
                                    generic_error!("{} is a range\n", arg);
                                }
                                #[cfg(feature = "libxml_xptr_locs")]
                                XmlXPathObjectType::XPathLocationset => {
                                    generic_error!("{} is a range\n", arg);
                                }
                                XmlXPathObjectType::XPathUsers => {
                                    generic_error!("{} is user-defined\n", arg);
                                }
                                XmlXPathObjectType::XPathXSLTTree => {
                                    generic_error!("{} is an XSLT value tree\n", arg);
                                }
                            }
                            #[cfg(feature = "xpath")]
                            {
                                xml_xpath_free_object(list);
                            }
                        } else {
                            generic_error!("{}: no such node\n", arg);
                        }
                        (*ctxt.pctxt).node = None;
                    }
                }
                "cd" => {
                    if arg.is_empty() {
                        ctxt.node = Some(doc.into());
                        continue;
                    }
                    let mut arg = arg;
                    #[cfg(feature = "xpath")]
                    {
                        (*ctxt.pctxt).node = ctxt.node;
                        if arg.len() >= 2 {
                            if let Some(new) = arg.strip_suffix('/') {
                                arg = new;
                            }
                        }
                        list = xml_xpath_eval(arg, ctxt.pctxt);
                    }
                    #[cfg(not(feature = "xpath"))]
                    {
                        list = null_mut();
                    }
                    if !list.is_null() {
                        match (*list).typ {
                            XmlXPathObjectType::XPathUndefined => {
                                generic_error!("{}: no such node\n", arg);
                            }
                            XmlXPathObjectType::XPathNodeset => {
                                if let Some(nodeset) = (*list).nodesetval.as_deref() {
                                    if nodeset.node_tab.len() == 1 {
                                        ctxt.node = Some(nodeset.node_tab[0]);
                                        if ctxt
                                            .node
                                            .take_if(|node| {
                                                node.element_type()
                                                    == XmlElementType::XmlNamespaceDecl
                                            })
                                            .is_some()
                                        {
                                            generic_error!("cannot cd to namespace\n");
                                        }
                                    } else {
                                        generic_error!(
                                            "{} is a {} Node Set\n",
                                            arg,
                                            nodeset.node_tab.len()
                                        );
                                    }
                                } else {
                                    generic_error!("{} is an empty Node Set\n", arg);
                                }
                            }
                            XmlXPathObjectType::XPathBoolean => {
                                generic_error!("{} is a Boolean\n", arg);
                            }
                            XmlXPathObjectType::XPathNumber => {
                                generic_error!("{} is a number\n", arg);
                            }
                            XmlXPathObjectType::XPathString => {
                                generic_error!("{} is a string\n", arg);
                            }
                            #[cfg(feature = "libxml_xptr_locs")]
                            XmlXPathObjectType::XPathPoint => {
                                generic_error!("{} is a point\n", arg);
                            }
                            #[cfg(feature = "libxml_xptr_locs")]
                            XmlXPathObjectType::XPathRange => {
                                generic_error!("{} is a range\n", arg);
                            }
                            #[cfg(feature = "libxml_xptr_locs")]
                            XmlXPathObjectType::XPathLocationset => {
                                generic_error!("{} is a range\n", arg);
                            }
                            XmlXPathObjectType::XPathUsers => {
                                generic_error!("{} is user-defined\n", arg);
                            }
                            XmlXPathObjectType::XPathXSLTTree => {
                                generic_error!("{} is an XSLT value tree\n", arg);
                            }
                        }
                        #[cfg(feature = "xpath")]
                        {
                            xml_xpath_free_object(list);
                        }
                    } else {
                        generic_error!("{}: no such node\n", arg);
                    }

                    (*ctxt.pctxt).node = None;
                }
                "cat" => {
                    if arg.is_empty() {
                        ctxt.xml_shell_cat(None, ctxt.node, None);
                        continue;
                    }
                    (*ctxt.pctxt).node = ctxt.node;
                    #[cfg(feature = "xpath")]
                    {
                        (*ctxt.pctxt).node = ctxt.node;
                        list = xml_xpath_eval(arg, ctxt.pctxt);
                    }
                    #[cfg(not(feature = "xpath"))]
                    {
                        list = null_mut();
                    }
                    if !list.is_null() {
                        match (*list).typ {
                            XmlXPathObjectType::XPathUndefined => {
                                generic_error!("{}: no such node\n", arg);
                            }
                            XmlXPathObjectType::XPathNodeset => {
                                if let Some(nodeset) = (*list).nodesetval.as_deref() {
                                    for &node in &nodeset.node_tab {
                                        if !arg.is_empty() {
                                            writeln!(ctxt.output, " -------").ok();
                                        }
                                        ctxt.xml_shell_cat(None, Some(node), None);
                                    }
                                }
                            }
                            XmlXPathObjectType::XPathBoolean => {
                                generic_error!("{} is a Boolean\n", arg);
                            }
                            XmlXPathObjectType::XPathNumber => {
                                generic_error!("{} is a number\n", arg);
                            }
                            XmlXPathObjectType::XPathString => {
                                generic_error!("{} is a string\n", arg);
                            }
                            #[cfg(feature = "libxml_xptr_locs")]
                            XmlXPathObjectType::XPathPoint => {
                                generic_error!("{} is a point\n", arg);
                            }
                            #[cfg(feature = "libxml_xptr_locs")]
                            XmlXPathObjectType::XPathRange => {
                                generic_error!("{} is a range\n", arg);
                            }
                            #[cfg(feature = "libxml_xptr_locs")]
                            XmlXPathObjectType::XPathLocationset => {
                                generic_error!("{} is a range\n", arg);
                            }
                            XmlXPathObjectType::XPathUsers => {
                                generic_error!("{} is user-defined\n", arg);
                            }
                            XmlXPathObjectType::XPathXSLTTree => {
                                generic_error!("{} is an XSLT value tree\n", arg);
                            }
                        }
                        #[cfg(feature = "xpath")]
                        {
                            xml_xpath_free_object(list);
                        }
                    } else {
                        generic_error!("{}: no such node\n", arg);
                    }
                    (*ctxt.pctxt).node = None;
                }
                _ => {
                    generic_error!("Unknown command {command}\n");
                }
            }
        }
        #[cfg(feature = "xpath")]
        {
            xml_xpath_free_context(ctxt.pctxt);
        }
        if ctxt.loaded != 0 {
            xml_free_doc(doc);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

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
}
