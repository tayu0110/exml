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
    ptr::{addr_of_mut, null, null_mut},
    sync::atomic::Ordering,
};

use libc::strlen;

#[cfg(feature = "xpath")]
use crate::xpath::{XmlXPathContextPtr, XmlXPathObjectPtr, XmlXPathObjectType};
use crate::{
    error::{XmlParserErrors, __xml_raise_error},
    generic_error,
    libxml::chvalid::xml_is_blank_char,
    tree::{
        xml_free_node_list, xml_get_doc_entity, xml_validate_name, NodeCommon, XmlAttrPtr,
        XmlAttributeDefault, XmlAttributePtr, XmlAttributeType, XmlDoc, XmlDocPtr, XmlDtdPtr,
        XmlElementPtr, XmlElementType, XmlElementTypeVal, XmlEntity, XmlEntityPtr, XmlEntityType,
        XmlGenericNodePtr, XmlNode, XmlNodePtr, XmlNs, XmlNsPtr,
    },
};

use super::{
    dict::{xml_dict_lookup, xml_dict_owns, XmlDictPtr},
    parser::{xml_parse_in_node_context, XmlParserOption},
    parser_internals::{XML_STRING_COMMENT, XML_STRING_TEXT, XML_STRING_TEXT_NOENC},
    valid::xml_snprintf_element_content,
    xmlstring::{xml_strchr, xml_strstr, XmlChar},
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
            (*$ctxt).node.map_or(null_mut(), |node| node.as_ptr()) as _,
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
                write!(self.output, "{}", &self.shift[..2 * self.depth as usize]);
            } else {
                write!(self.output, "{}", &self.shift[..100]);
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
            write!(self.output, "(NULL)");
            return;
        };

        for c in s.bytes().take(40) {
            if xml_is_blank_char(c as u32) {
                write!(self.output, " ");
            } else if c >= 0x80 {
                write!(self.output, "#{:0X}", c as i32);
            } else {
                write!(self.output, "{}", c as char);
            }
        }
        if s.len() > 40 {
            write!(self.output, "...");
        }
    }

    /// Report if a given namespace is is not in scope.
    #[doc(alias = "xmlCtxtNsCheckScope")]
    unsafe fn ns_check_scope(&mut self, node: XmlGenericNodePtr, ns: XmlNsPtr) {
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

    /// Do debugging on the string, currently it just checks the UTF-8 content
    #[doc(alias = "xmlCtxtCheckString")]
    unsafe fn check_string(&mut self, s: &str) {
        if self.check != 0 {
            xml_debug_err!(
                self,
                XmlParserErrors::XmlCheckNotUTF8,
                "String is not UTF-8 {s}",
            );
        }
    }

    /// Do debugging on the name, for example the dictionary status and
    /// conformance to the Name production.
    #[doc(alias = "xmlCtxtCheckName")]
    unsafe fn check_name(&mut self, name: Option<&str>) {
        if self.check != 0 {
            let Some(name) = name else {
                xml_debug_err!(self, XmlParserErrors::XmlCheckNoName, "Name is NULL",);
                return;
            };
            let cname = CString::new(name).unwrap();
            #[cfg(any(feature = "libxml_tree", feature = "schema"))]
            if xml_validate_name(cname.as_ptr() as *const u8, 0) != 0 {
                xml_debug_err!(
                    self,
                    XmlParserErrors::XmlCheckNotNCName,
                    "Name is not an NCName '{name}'",
                );
            }
            if !self.dict.is_null()
                && xml_dict_owns(self.dict, cname.as_ptr() as *const u8) == 0
                && self.doc.map_or(true, |doc| {
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

    #[doc(alias = "xmlCtxtGenericNodeCheck")]
    unsafe fn generic_node_check(&mut self, node: XmlGenericNodePtr) {
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
            if node.element_type() == XmlElementType::XmlAttributeNode {
                let attr = XmlAttrPtr::try_from(node).unwrap();
                if node
                    .parent()
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
                .filter(|p| p.children().map_or(null_mut(), |p| p.as_ptr()) != node.as_ptr())
                .is_some()
            {
                xml_debug_err!(
                    self,
                    XmlParserErrors::XmlCheckNoPrev,
                    "Node has no prev and not first of parent list\n",
                );
            }
        } else if node
            .prev()
            .unwrap()
            .next
            .map_or(null_mut(), |node| node.as_ptr())
            != node.as_ptr()
        {
            xml_debug_err!(
                self,
                XmlParserErrors::XmlCheckWrongPrev,
                "Node prev->next : back link wrong\n",
            );
        }
        if let Some(next) = node.next() {
            if next.prev.map_or(null_mut(), |prev| prev.as_ptr()) != node.as_ptr() {
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
                    && p.last().map_or(null_mut(), |last| last.as_ptr()) != node.as_ptr()
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
            let mut ns = node.as_node().unwrap().as_ref().ns_def;
            while let Some(now) = ns {
                self.ns_check_scope(node, now);
                ns = XmlNsPtr::from_raw(now.next).unwrap();
            }
            if let Some(ns) = node.as_node().unwrap().as_ref().ns {
                self.ns_check_scope(node, ns);
            }
        } else if let Some(ns) = node.as_attribute_node().and_then(|attr| attr.as_ref().ns) {
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
        ) && !node.as_node().unwrap().as_ref().content.is_null()
        {
            let content = node.as_node().unwrap().as_ref().content;
            self.check_string(&CStr::from_ptr(content as *const i8).to_string_lossy());
        }
        match (*node).element_type() {
            XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode => {
                self.check_name(node.name().as_deref());
            }
            XmlElementType::XmlTextNode => {
                if node.name().map_or(null(), |n| n.as_ptr()) != XML_STRING_TEXT.as_ptr() as _
                    && (*node).name().map_or(null(), |n| n.as_ptr())
                        != XML_STRING_TEXT_NOENC.as_ptr() as _
                {
                    // some case of entity substitution can lead to this
                    if self.dict.is_null()
                        || ((*node).name().map_or(null(), |n| n.as_ptr())
                            != xml_dict_lookup(self.dict, c"nbktext".as_ptr() as _, 7))
                    {
                        xml_debug_err!(
                            self,
                            XmlParserErrors::XmlCheckWrongName,
                            "Text node has wrong name '{}'",
                            (*node).name().unwrap()
                        );
                    }
                }
            }
            XmlElementType::XmlCommentNode => {
                if node.name().map_or(null(), |n| n.as_ptr()) != XML_STRING_COMMENT.as_ptr() as _ {
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

    #[doc(alias = "xmlCtxtDumpDtdNode")]
    unsafe fn dump_dtd_node(&mut self, dtd: Option<XmlDtdPtr>) {
        self.dump_spaces();

        let Some(dtd) = dtd else {
            if self.check == 0 {
                writeln!(self.output, "DTD node is NULL");
            }
            return;
        };

        if dtd.element_type() != XmlElementType::XmlDTDNode {
            xml_debug_err!(self, XmlParserErrors::XmlCheckNotDTD, "Node is not a DTD",);
            return;
        }
        if self.check == 0 {
            if let Some(name) = dtd.name() {
                write!(self.output, "DTD({name})");
            } else {
                write!(self.output, "DTD");
            }
            if let Some(external_id) = dtd.external_id.as_deref() {
                write!(self.output, ", PUBLIC {external_id}");
            }
            if let Some(system_id) = dtd.system_id.as_deref() {
                write!(self.output, ", SYSTEM {system_id}");
            }
            writeln!(self.output);
        }
        // Do a bit of checking
        self.generic_node_check(dtd.into());
    }

    /// Dumps debug information for the DTD
    #[doc(alias = "xmlCtxtDumpDTD")]
    unsafe fn dump_dtd(&mut self, dtd: Option<XmlDtdPtr>) {
        let Some(dtd) = dtd else {
            if self.check == 0 {
                writeln!(self.output, "DTD is NULL");
            }
            return;
        };
        self.dump_dtd_node(Some(dtd));
        if let Some(children) = dtd.children() {
            self.depth += 1;
            self.dump_node_list(XmlGenericNodePtr::from_raw(children.as_ptr()));
            self.depth -= 1;
        } else {
            writeln!(self.output, "    DTD is empty");
        }
    }

    #[doc(alias = "xmlCtxtDumpElemDecl")]
    unsafe fn dump_elem_decl(&mut self, elem: Option<XmlElementPtr>) {
        self.dump_spaces();

        let Some(elem) = elem else {
            if self.check == 0 {
                writeln!(self.output, "Element declaration is NULL");
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
                write!(self.output, "ELEMDECL(");
                self.dump_string(Some(&name));
                write!(self.output, ")");
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
                    write!(self.output, ", UNDEFINED");
                }
                XmlElementTypeVal::XmlElementTypeEmpty => {
                    write!(self.output, ", EMPTY");
                }
                XmlElementTypeVal::XmlElementTypeAny => {
                    write!(self.output, ", ANY");
                }
                XmlElementTypeVal::XmlElementTypeMixed => {
                    write!(self.output, ", MIXED ");
                }
                XmlElementTypeVal::XmlElementTypeElement => {
                    write!(self.output, ", MIXED ");
                }
            }
            if elem.element_type() != XmlElementType::XmlElementNode && !elem.content.is_null() {
                let mut buf: [c_char; 5001] = [0; 5001];

                buf[0] = 0;
                xml_snprintf_element_content(buf.as_mut_ptr(), 5000, elem.content, 1);
                buf[5000] = 0;
                let elem = CStr::from_ptr(buf.as_ptr()).to_string_lossy();
                write!(self.output, "{}", elem);
            }
            writeln!(self.output);
        }

        // Do a bit of checking
        self.generic_node_check(elem.into());
    }

    #[doc(alias = "xmlCtxtDumpAttrDecl")]
    unsafe fn dump_attr_decl(&mut self, attr: Option<XmlAttributePtr>) {
        self.dump_spaces();

        let Some(attr) = attr else {
            if self.check == 0 {
                writeln!(self.output, "Attribute declaration is NULL");
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
                write!(self.output, "ATTRDECL({name})");
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
                write!(self.output, " for {elem}");
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
                    write!(self.output, " CDATA");
                }
                XmlAttributeType::XmlAttributeID => {
                    write!(self.output, " ID");
                }
                XmlAttributeType::XmlAttributeIDREF => {
                    write!(self.output, " IDREF");
                }
                XmlAttributeType::XmlAttributeIDREFS => {
                    write!(self.output, " IDREFS");
                }
                XmlAttributeType::XmlAttributeEntity => {
                    write!(self.output, " ENTITY");
                }
                XmlAttributeType::XmlAttributeEntities => {
                    write!(self.output, " ENTITIES");
                }
                XmlAttributeType::XmlAttributeNmtoken => {
                    write!(self.output, " NMTOKEN");
                }
                XmlAttributeType::XmlAttributeNmtokens => {
                    write!(self.output, " NMTOKENS");
                }
                XmlAttributeType::XmlAttributeEnumeration => {
                    write!(self.output, " ENUMERATION");
                }
                XmlAttributeType::XmlAttributeNotation => {
                    write!(self.output, " NOTATION ");
                }
            }
            if let Some(mut cur) = attr.tree.as_deref() {
                let mut remain = true;
                for indx in 0..5 {
                    if indx != 0 {
                        write!(self.output, "|{}", cur.name);
                    } else {
                        write!(self.output, " ({}", cur.name);
                    }
                    let Some(next) = cur.next.as_deref() else {
                        remain = false;
                        break;
                    };
                    cur = next;
                }
                if !remain {
                    write!(self.output, ")");
                } else {
                    write!(self.output, "...)");
                }
            }
            match attr.def {
                XmlAttributeDefault::XmlAttributeNone => {}
                XmlAttributeDefault::XmlAttributeRequired => {
                    write!(self.output, " REQUIRED");
                }
                XmlAttributeDefault::XmlAttributeImplied => {
                    write!(self.output, " IMPLIED");
                }
                XmlAttributeDefault::XmlAttributeFixed => {
                    write!(self.output, " FIXED");
                }
            }
            if !attr.default_value.is_null() {
                write!(self.output, "\"");
                let def_value = attr.default_value;
                self.dump_string(
                    Some(CStr::from_ptr(def_value as *const i8).to_string_lossy()).as_deref(),
                );
                write!(self.output, "\"");
            }
            writeln!(self.output);
        }

        // Do a bit of checking
        self.generic_node_check(attr.into());
    }

    #[doc(alias = "xmlCtxtDumpEntityDecl")]
    unsafe fn dump_entity_decl(&mut self, ent: Option<XmlEntityPtr>) {
        self.dump_spaces();

        let Some(ent) = ent else {
            if self.check == 0 {
                writeln!(self.output, "Entity declaration is NULL");
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
                write!(self.output, "ENTITYDECL(");
                self.dump_string(Some(&name));
                write!(self.output, ")");
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
                    writeln!(self.output, ", internal");
                }
                XmlEntityType::XmlExternalGeneralParsedEntity => {
                    writeln!(self.output, ", external parsed");
                }
                XmlEntityType::XmlExternalGeneralUnparsedEntity => {
                    writeln!(self.output, ", unparsed");
                }
                XmlEntityType::XmlInternalParameterEntity => {
                    writeln!(self.output, ", parameter");
                }
                XmlEntityType::XmlExternalParameterEntity => {
                    writeln!(self.output, ", external parameter");
                }
                XmlEntityType::XmlInternalPredefinedEntity => {
                    writeln!(self.output, ", predefined");
                }
                _ => unreachable!(),
            }
            if !ent.external_id.load(Ordering::Relaxed).is_null() {
                self.dump_spaces();
                let external_id =
                    CStr::from_ptr(ent.external_id.load(Ordering::Relaxed) as *const i8)
                        .to_string_lossy();
                writeln!(self.output, " ExternalID={external_id}");
            }
            if !ent.system_id.load(Ordering::Relaxed).is_null() {
                self.dump_spaces();
                let system_id = CStr::from_ptr(ent.system_id.load(Ordering::Relaxed) as *const i8)
                    .to_string_lossy();
                writeln!(self.output, " SystemID={system_id}");
            }
            if !ent.uri.load(Ordering::Relaxed).is_null() {
                self.dump_spaces();
                let uri =
                    CStr::from_ptr(ent.uri.load(Ordering::Relaxed) as *const i8).to_string_lossy();
                writeln!(self.output, " URI={uri}");
            }
            let content = ent.content.load(Ordering::Relaxed);
            if !content.is_null() {
                self.dump_spaces();
                write!(self.output, " content=");
                self.dump_string(Some(
                    &CStr::from_ptr(content as *const i8).to_string_lossy(),
                ));
                writeln!(self.output);
            }
        }

        // Do a bit of checking
        self.generic_node_check(ent.into());
    }

    #[doc(alias = "xmlCtxtDumpNamespace")]
    unsafe fn dump_namespace(&mut self, ns: Option<&XmlNs>) {
        self.dump_spaces();

        let Some(ns) = ns else {
            if self.check == 0 {
                writeln!(self.output, "namespace node is NULL");
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
                write!(self.output, "namespace {prefix} href=");
            } else {
                write!(self.output, "default namespace href=");
            }

            let href = ns.href;
            self.dump_string(
                (!href.is_null())
                    .then(|| CStr::from_ptr(href as *const i8).to_string_lossy())
                    .as_deref(),
            );
            writeln!(self.output);
        }
    }

    #[doc(alias = "xmlCtxtDumpNamespaceList")]
    unsafe fn dump_namespace_list(&mut self, mut ns: Option<&XmlNs>) {
        while let Some(now) = ns {
            self.dump_namespace(Some(now));
            let next = now.next;
            ns = (!next.is_null()).then(|| &*next);
        }
    }

    #[doc(alias = "xmlCtxtDumpEntity")]
    unsafe fn dump_entity(&mut self, ent: Option<&XmlEntity>) {
        self.dump_spaces();

        let Some(ent) = ent else {
            if self.check == 0 {
                writeln!(self.output, "Entity is NULL");
            }
            return;
        };
        if self.check == 0 {
            match ent.etype {
                XmlEntityType::XmlInternalGeneralEntity => {
                    write!(self.output, "INTERNAL_GENERAL_ENTITY ");
                }
                XmlEntityType::XmlExternalGeneralParsedEntity => {
                    write!(self.output, "EXTERNAL_GENERAL_PARSED_ENTITY ");
                }
                XmlEntityType::XmlExternalGeneralUnparsedEntity => {
                    write!(self.output, "EXTERNAL_GENERAL_UNPARSED_ENTITY ");
                }
                XmlEntityType::XmlInternalParameterEntity => {
                    write!(self.output, "INTERNAL_PARAMETER_ENTITY ");
                }
                XmlEntityType::XmlExternalParameterEntity => {
                    write!(self.output, "EXTERNAL_PARAMETER_ENTITY ");
                }
                e => {
                    write!(self.output, "ENTITY_{} ! ", e as i32);
                }
                _ => unreachable!(),
            }
            writeln!(self.output, "{}", ent.name().unwrap());
            if !ent.external_id.load(Ordering::Relaxed).is_null() {
                self.dump_spaces();
                let external_id =
                    CStr::from_ptr(ent.external_id.load(Ordering::Relaxed) as *const i8)
                        .to_string_lossy();
                writeln!(self.output, "ExternalID={external_id}");
            }
            if !ent.system_id.load(Ordering::Relaxed).is_null() {
                self.dump_spaces();
                let system_id = CStr::from_ptr(ent.system_id.load(Ordering::Relaxed) as *const i8)
                    .to_string_lossy();
                writeln!(self.output, "SystemID={system_id}");
            }
            if !ent.uri.load(Ordering::Relaxed).is_null() {
                self.dump_spaces();
                let uri =
                    CStr::from_ptr(ent.uri.load(Ordering::Relaxed) as *const i8).to_string_lossy();
                writeln!(self.output, "URI={uri}");
            }
            let content = ent.content.load(Ordering::Relaxed);
            if !content.is_null() {
                self.dump_spaces();
                write!(self.output, "content=");
                self.dump_string(Some(
                    &CStr::from_ptr(content as *const i8).to_string_lossy(),
                ));
                writeln!(self.output);
            }
        }
    }

    /// Dumps debug information for the attribute
    #[doc(alias = "xmlCtxtDumpAttr")]
    unsafe fn dump_attr(&mut self, attr: Option<XmlAttrPtr>) {
        self.dump_spaces();

        let Some(attr) = attr else {
            if self.check == 0 {
                write!(self.output, "Attr is NULL");
            }
            return;
        };
        if self.check == 0 {
            write!(self.output, "ATTRIBUTE ");
            self.dump_string(attr.name().as_deref());
            writeln!(self.output);
            if let Some(children) = attr.children() {
                self.depth += 1;
                self.dump_node_list(XmlGenericNodePtr::from_raw(children.as_ptr()));
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

    /// Dumps debug information for the attribute list
    #[doc(alias = "xmlCtxtDumpAttrList")]
    unsafe fn dump_attr_list(&mut self, mut attr: Option<XmlAttrPtr>) {
        while let Some(now) = attr {
            self.dump_attr(Some(now));
            attr = now.next;
        }
    }

    /// Dumps debug information for the element node, it is not recursive
    #[doc(alias = "xmlCtxtDumpOneNode")]
    unsafe fn dump_one_node(&mut self, node: Option<XmlGenericNodePtr>) {
        let Some(node) = node else {
            if self.check == 0 {
                self.dump_spaces();
                writeln!(self.output, "node is NULL");
            }
            return;
        };
        self.node = Some(node);

        match node.element_type() {
            XmlElementType::XmlElementNode => {
                if self.check == 0 {
                    self.dump_spaces();
                    write!(self.output, "ELEMENT ");
                    let node = XmlNodePtr::try_from(node).unwrap();
                    if let Some(ns) = node.ns {
                        if let Some(prefix) = ns.prefix() {
                            self.dump_string(Some(&prefix));
                        }
                        write!(self.output, ":");
                    }
                    self.dump_string(node.name().as_deref());
                    writeln!(self.output);
                }
            }
            XmlElementType::XmlAttributeNode => {
                if self.check == 0 {
                    self.dump_spaces();
                }
                writeln!(self.output, "Error, ATTRIBUTE found here");
                self.generic_node_check(node);
                return;
            }
            XmlElementType::XmlTextNode => {
                if self.check == 0 {
                    self.dump_spaces();
                    let node = node.as_node().unwrap();
                    if node.as_ref().name == XML_STRING_TEXT_NOENC.as_ptr() as *const XmlChar {
                        write!(self.output, "TEXT no enc");
                    } else {
                        write!(self.output, "TEXT");
                    }
                    if self.options & DUMP_TEXT_TYPE != 0 {
                        if xml_dict_owns(self.dict, node.as_ref().content) == 1 {
                            writeln!(self.output, " interned");
                        } else {
                            writeln!(self.output);
                        }
                    } else {
                        writeln!(self.output);
                    }
                }
            }
            XmlElementType::XmlCDATASectionNode => {
                if self.check == 0 {
                    self.dump_spaces();
                    writeln!(self.output, "CDATA_SECTION");
                }
            }
            XmlElementType::XmlEntityRefNode => {
                if self.check == 0 {
                    self.dump_spaces();
                    writeln!(self.output, "ENTITY_REF({})", node.name().unwrap());
                }
            }
            XmlElementType::XmlEntityNode => {
                if self.check == 0 {
                    self.dump_spaces();
                    writeln!(self.output, "ENTITY");
                }
            }
            XmlElementType::XmlPINode => {
                if self.check == 0 {
                    self.dump_spaces();
                    writeln!(self.output, "PI {}", node.name().unwrap());
                }
            }
            XmlElementType::XmlCommentNode => {
                if self.check == 0 {
                    self.dump_spaces();
                    writeln!(self.output, "COMMENT");
                }
            }
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                if self.check == 0 {
                    self.dump_spaces();
                }
                writeln!(self.output, "Error, DOCUMENT found here");
                self.generic_node_check(node);
                return;
            }
            XmlElementType::XmlDocumentTypeNode => {
                if self.check == 0 {
                    self.dump_spaces();
                    writeln!(self.output, "DOCUMENT_TYPE");
                }
            }
            XmlElementType::XmlDocumentFragNode => {
                if self.check == 0 {
                    self.dump_spaces();
                    writeln!(self.output, "DOCUMENT_FRAG");
                }
            }
            XmlElementType::XmlNotationNode => {
                if self.check == 0 {
                    self.dump_spaces();
                    writeln!(self.output, "NOTATION");
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
                    writeln!(self.output, "INCLUDE START");
                }
                return;
            }
            XmlElementType::XmlXIncludeEnd => {
                if self.check == 0 {
                    self.dump_spaces();
                    writeln!(self.output, "INCLUDE END");
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
                    (*node).element_type() as i32
                );
                return;
            }
        }
        if node.document().is_none() {
            if self.check == 0 {
                self.dump_spaces();
            }
            writeln!(self.output, "PBM: doc.is_null() !!!");
        }
        self.depth += 1;
        if let Some(ns_def) = node
            .as_node()
            .filter(|n| n.as_ref().element_type() == XmlElementType::XmlElementNode)
            .and_then(|node| node.as_ref().ns_def)
        {
            self.dump_namespace_list(Some(&*ns_def));
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
                let content = if let Some(node) = node.as_node() {
                    node.as_ref().content
                } else {
                    null()
                };
                if !content.is_null() {
                    self.dump_spaces();
                    write!(self.output, "content=");
                    self.dump_string(Some(
                        &CStr::from_ptr(content as *const i8).to_string_lossy(),
                    ));
                    writeln!(self.output);
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

    /// Dumps debug information for the element node, it is recursive
    #[doc(alias = "xmlCtxtDumpNode")]
    unsafe fn dump_node(&mut self, node: Option<XmlGenericNodePtr>) {
        let Some(node) = node else {
            if self.check == 0 {
                self.dump_spaces();
                writeln!(self.output, "node is NULL");
            }
            return;
        };
        self.dump_one_node(Some(node));
        if let Some(children) = node.children().filter(|_| {
            node.element_type() != XmlElementType::XmlNamespaceDecl
                && node.element_type() != XmlElementType::XmlEntityRefNode
        }) {
            self.depth += 1;
            self.dump_node_list(XmlGenericNodePtr::from_raw(children.as_ptr()));
            self.depth -= 1;
        }
    }

    /// Dumps debug information for the list of element node, it is recursive
    #[doc(alias = "xmlCtxtDumpNodeList")]
    unsafe fn dump_node_list(&mut self, node: Option<XmlGenericNodePtr>) {
        if let Some(mut node) = node {
            self.dump_node(Some(node));
            while let Some(next) = node.next() {
                self.dump_node(XmlGenericNodePtr::from_raw(next.as_ptr()));
                node = XmlGenericNodePtr::from_raw(next.as_ptr()).unwrap();
            }
        }
    }

    #[doc(alias = "xmlCtxtDumpDocHead")]
    unsafe fn dump_doc_head(&mut self, doc: &XmlDoc) {
        self.node = XmlGenericNodePtr::from_raw(doc as *const XmlDoc as *mut XmlDoc);

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
                    writeln!(self.output, "DOCUMENT");
                }
            }
            XmlElementType::XmlHTMLDocumentNode => {
                if self.check == 0 {
                    writeln!(self.output, "HTML DOCUMENT");
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

    /// Dumps debug information concerning the document, not recursive
    #[doc(alias = "xmlCtxtDumpDocumentHead")]
    unsafe fn dump_document_head(&mut self, doc: Option<&XmlDoc>) {
        if let Some(doc) = doc {
            self.dump_doc_head(doc);
            if self.check == 0 {
                if let Some(name) = doc.name() {
                    write!(self.output, "name=");
                    self.dump_string(Some(&name));
                    writeln!(self.output);
                }
                if let Some(version) = doc.version.as_deref() {
                    write!(self.output, "version=");
                    self.dump_string(Some(version));
                    writeln!(self.output);
                }
                if let Some(encoding) = doc.encoding.as_deref() {
                    write!(self.output, "encoding=");
                    self.dump_string(Some(encoding));
                    writeln!(self.output);
                }
                if let Some(url) = doc.url.as_deref() {
                    write!(self.output, "URL=");
                    self.dump_string(Some(url));
                    writeln!(self.output);
                }
                if doc.standalone != 0 {
                    writeln!(self.output, "standalone=true");
                }
            }
            if let Some(old_ns) = doc.old_ns {
                self.dump_namespace_list(Some(&*old_ns));
            }
        }
    }

    /// Dumps debug information for the document, it's recursive
    #[doc(alias = "xmlCtxtDumpDocument")]
    unsafe fn dump_document(&mut self, doc: Option<&XmlDoc>) {
        let Some(doc) = doc else {
            if self.check == 0 {
                writeln!(self.output, "DOCUMENT.is_null() !");
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
            self.dump_node_list(XmlGenericNodePtr::from_raw(children.as_ptr()));
            self.depth -= 1;
        }
    }

    #[doc(alias = "xmlCtxtDumpEntityCallback")]
    unsafe fn dump_entities_callback(&mut self, cur: Option<&XmlEntity>) {
        let Some(cur) = cur else {
            if self.check == 0 {
                write!(self.output, "Entity is NULL");
            }
            return;
        };
        if self.check == 0 {
            write!(self.output, "{} : ", cur.name().unwrap());
            match cur.etype {
                XmlEntityType::XmlInternalGeneralEntity => {
                    write!(self.output, "INTERNAL GENERAL, ");
                }
                XmlEntityType::XmlExternalGeneralParsedEntity => {
                    write!(self.output, "EXTERNAL PARSED, ");
                }
                XmlEntityType::XmlExternalGeneralUnparsedEntity => {
                    write!(self.output, "EXTERNAL UNPARSED, ");
                }
                XmlEntityType::XmlInternalParameterEntity => {
                    write!(self.output, "INTERNAL PARAMETER, ");
                }
                XmlEntityType::XmlExternalParameterEntity => {
                    write!(self.output, "EXTERNAL PARAMETER, ");
                }
                e => {
                    xml_debug_err!(
                        self,
                        XmlParserErrors::XmlCheckEntityType,
                        "Unknown entity type {}\n",
                        e as i32
                    );
                }
                _ => unreachable!(),
            }
            if !cur.external_id.load(Ordering::Relaxed).is_null() {
                let external_id =
                    CStr::from_ptr(cur.external_id.load(Ordering::Relaxed) as *const i8)
                        .to_string_lossy();
                write!(self.output, "ID \"{external_id}\"");
            }
            if !cur.system_id.load(Ordering::Relaxed).is_null() {
                let system_id = CStr::from_ptr(cur.system_id.load(Ordering::Relaxed) as *const i8)
                    .to_string_lossy();
                write!(self.output, "SYSTEM \"{system_id}\"");
            }
            if !cur.orig.load(Ordering::Relaxed).is_null() {
                let orig =
                    CStr::from_ptr(cur.orig.load(Ordering::Relaxed) as *const i8).to_string_lossy();
                write!(self.output, "\n orig \"{orig}\"");
            }
            if cur.typ != XmlElementType::XmlElementNode
                && !cur.content.load(Ordering::Relaxed).is_null()
            {
                let content = CStr::from_ptr(cur.content.load(Ordering::Relaxed) as *const i8)
                    .to_string_lossy();
                write!(self.output, "\n content \"{content}\"");
            }
            writeln!(self.output);
        }
    }

    /// Dumps debug information for all the entities in use by the document
    #[doc(alias = "xmlCtxtDumpEntities")]
    unsafe fn dump_entities(&mut self, doc: Option<&XmlDoc>) {
        if let Some(doc) = doc {
            self.dump_doc_head(doc);
            if let Some(int_subset) = doc.int_subset {
                if let Some(table) = int_subset.entities {
                    if self.check == 0 {
                        writeln!(self.output, "Entities in internal subset");
                    }
                    table.scan(|payload, _, _, _| {
                        let entity = *payload;
                        self.dump_entities_callback(Some(&*entity));
                    });
                }
            } else {
                writeln!(self.output, "No entities in internal subset");
            }
            if let Some(ext_subset) = doc.ext_subset {
                if let Some(table) = ext_subset.entities {
                    if self.check == 0 {
                        writeln!(self.output, "Entities in external subset");
                    }
                    table.scan(|payload, _, _, _| {
                        let entity = *payload;
                        self.dump_entities_callback(Some(&*entity));
                    });
                }
            } else if self.check == 0 {
                writeln!(self.output, "No entities in external subset");
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
        write!(output, "(NULL)");
        return;
    };
    for c in s.bytes().take(40) {
        if xml_is_blank_char(c as u32) {
            write!(output, " ");
        } else if c >= 0x80 {
            write!(output, "#{:X}", c as i32);
        } else {
            write!(output, "{}", c as char);
        }
    }
    if s.len() > 40 {
        write!(output, "...");
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
            let mut cur = now.as_node().unwrap().as_ref().ns_def;
            while let Some(now) = cur {
                if now == ns {
                    return 1;
                }
                if now.prefix() == ns.prefix() {
                    return -2;
                }
                cur = XmlNsPtr::from_raw(now.next).unwrap();
            }
        }
        node = now
            .parent()
            .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()));
    }
    // the xml namespace may be declared on the document node
    if let Some(node) = node.filter(|node| {
        matches!(
            node.element_type(),
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode
        )
    }) {
        let old_ns = node.as_document_node().unwrap().as_ref().old_ns;
        if old_ns == Some(ns) {
            return 1;
        }
    }
    -3
}

/// Dumps debug information for the attribute
#[doc(alias = "xmlDebugDumpAttr")]
pub unsafe fn xml_debug_dump_attr<'a>(
    output: &mut (impl Write + 'a),
    attr: Option<XmlAttrPtr>,
    depth: i32,
) {
    let mut ctxt = XmlDebugCtxt {
        output: Box::new(output),
        depth,
        ..Default::default()
    };
    ctxt.dump_attr(attr);
}

/// Dumps debug information for the attribute list
#[doc(alias = "xmlDebugDumpAttrList")]
pub unsafe fn xml_debug_dump_attr_list<'a>(
    output: &mut (impl Write + 'a),
    attr: Option<XmlAttrPtr>,
    depth: i32,
) {
    let mut ctxt = XmlDebugCtxt {
        output: Box::new(output),
        depth,
        ..Default::default()
    };
    ctxt.dump_attr_list(attr);
}

/// Dumps debug information for the element node, it is not recursive
#[doc(alias = "xmlDebugDumpOneNode")]
pub unsafe fn xml_debug_dump_one_node<'a>(
    output: &mut (impl Write + 'a),
    node: Option<XmlGenericNodePtr>,
    depth: i32,
) {
    let mut ctxt = XmlDebugCtxt {
        output: Box::new(output),
        depth,
        ..Default::default()
    };
    ctxt.dump_one_node(node);
}

/// Dumps debug information for the element node, it is recursive
#[doc(alias = "xmlDebugDumpNode")]
pub unsafe fn xml_debug_dump_node<'a>(
    output: Option<impl Write + 'a>,
    node: Option<XmlGenericNodePtr>,
    depth: i32,
) {
    let mut ctxt = XmlDebugCtxt {
        output: output.map_or(Box::new(stdout()) as Box<dyn Write>, |o| Box::new(o)),
        depth,
        ..Default::default()
    };
    ctxt.dump_node(node);
}

/// Dumps debug information for the list of element node, it is recursive
#[doc(alias = "xmlDebugDumpNodeList")]
pub unsafe fn xml_debug_dump_node_list<'a>(
    output: Option<impl Write + 'a>,
    node: Option<XmlGenericNodePtr>,
    depth: i32,
) {
    let mut ctxt = XmlDebugCtxt {
        output: output.map_or(Box::new(stdout()) as Box<dyn Write>, |o| Box::new(o)),
        depth,
        ..Default::default()
    };
    ctxt.dump_node_list(node);
}

/// Dumps debug information concerning the document, not recursive
#[doc(alias = "xmlDebugDumpDocumentHead")]
pub unsafe fn xml_debug_dump_document_head<'a>(
    output: Option<impl Write + 'a>,
    doc: Option<&XmlDoc>,
) {
    let mut ctxt = XmlDebugCtxt::default();

    ctxt.options |= DUMP_TEXT_TYPE;
    ctxt.output = output.map_or(Box::new(stdout()) as Box<dyn Write>, |o| Box::new(o));
    ctxt.dump_document_head(doc);
}

/// Dumps debug information for the document, it's recursive
#[doc(alias = "xmlDebugDumpDocument")]
pub unsafe fn xml_debug_dump_document<'a>(output: Option<impl Write + 'a>, doc: Option<&XmlDoc>) {
    let mut ctxt = XmlDebugCtxt::default();

    ctxt.options |= DUMP_TEXT_TYPE;
    ctxt.output = output.map_or(Box::new(stdout()) as Box<dyn Write>, |o| Box::new(o));
    ctxt.dump_document(doc);
}

/// Dumps debug information for the DTD
#[doc(alias = "xmlDebugDumpDTD")]
pub unsafe fn xml_debug_dump_dtd<'a>(output: Option<impl Write + 'a>, dtd: Option<XmlDtdPtr>) {
    let mut ctxt = XmlDebugCtxt::default();

    ctxt.options |= DUMP_TEXT_TYPE;
    ctxt.output = output.map_or(Box::new(stdout()) as Box<dyn Write>, |o| Box::new(o));
    ctxt.dump_dtd(dtd);
}

/// Dumps debug information for all the entities in use by the document
#[doc(alias = "xmlDebugDumpEntities")]
pub unsafe fn xml_debug_dump_entities<'a>(output: impl Write + 'a, doc: Option<&XmlDoc>) {
    let mut ctxt = XmlDebugCtxt {
        output: Box::new(output),
        ..Default::default()
    };
    ctxt.dump_entities(doc);
}

/// Check the document for potential content problems, and output
/// the errors to `output`
///
/// Returns the number of errors found
#[doc(alias = "xmlDebugCheckDocument")]
pub unsafe fn xml_debug_check_document<'a>(
    output: Option<impl Write + 'a>,
    doc: Option<&XmlDoc>,
) -> i32 {
    let mut ctxt = XmlDebugCtxt {
        output: output.map_or(Box::new(stdout()) as Box<dyn Write>, |o| Box::new(o)),
        check: 1,
        ..Default::default()
    };
    ctxt.dump_document(doc);
    ctxt.errors
}

/// Dump to `output` the type and name of @node.
#[doc(alias = "xmlLsOneNode")]
pub unsafe fn xml_ls_one_node<'a>(output: &mut (impl Write + 'a), node: Option<XmlGenericNodePtr>) {
    let Some(node) = node else {
        writeln!(output, "NULL");
        return;
    };
    match node.element_type() {
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
    if node.element_type() != XmlElementType::XmlNamespaceDecl {
        if let Ok(node) = XmlNodePtr::try_from(node) {
            if node.properties.is_some() {
                write!(output, "a");
            } else {
                write!(output, "-");
            }
            if node.ns_def.is_some() {
                write!(output, "n");
            } else {
                write!(output, "-");
            }
        } else {
            write!(output, "--");
        }
    }

    write!(output, " {} ", xml_ls_count_node(Some(node)));

    match node.element_type() {
        XmlElementType::XmlElementNode => {
            let node = XmlNodePtr::try_from(node).unwrap();
            if !node.name.is_null() {
                if let Some(prefix) = node.ns.as_deref().and_then(|ns| ns.prefix()) {
                    write!(output, "{prefix}:");
                }
                let name = CStr::from_ptr(node.name as *const i8).to_string_lossy();
                write!(output, "{name}");
            }
        }
        XmlElementType::XmlAttributeNode => {
            let node = XmlAttrPtr::try_from(node).unwrap();
            if !node.name.is_null() {
                let name = CStr::from_ptr(node.name as *const i8).to_string_lossy();
                write!(output, "{name}");
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
                write!(output, "{name}");
            }
        }
        XmlElementType::XmlEntityNode => {
            let node = XmlNodePtr::try_from(node).unwrap();
            if !node.name.is_null() {
                let name = CStr::from_ptr(node.name as *const i8).to_string_lossy();
                write!(output, "{name}");
            }
        }
        XmlElementType::XmlPINode => {
            let node = XmlNodePtr::try_from(node).unwrap();
            if !node.name.is_null() {
                let name = CStr::from_ptr(node.name as *const i8).to_string_lossy();
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
            let ns = XmlNsPtr::try_from(node).unwrap();
            let href = CStr::from_ptr(ns.href as *const i8).to_string_lossy();
            if let Some(prefix) = ns.prefix() {
                write!(output, "{prefix} -> {href}");
            } else {
                write!(output, "default -> {href}");
            }
        }
        _ => {
            if let Some(name) = node.name() {
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
pub unsafe fn xml_ls_count_node(node: Option<XmlGenericNodePtr>) -> usize {
    let Some(node) = node else {
        return 0;
    };

    let mut list = match node.element_type() {
        XmlElementType::XmlElementNode => node.children(),
        XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
            node.as_document_node().unwrap().as_ref().children()
        }
        XmlElementType::XmlAttributeNode => node.as_attribute_node().unwrap().as_ref().children(),
        XmlElementType::XmlTextNode
        | XmlElementType::XmlCDATASectionNode
        | XmlElementType::XmlPINode
        | XmlElementType::XmlCommentNode => {
            let node = node.as_node().unwrap();
            let content = node.as_ref().content;
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

/// Convenient way to turn bool into text
///
/// Returns a pointer to either "True" or "False"
#[doc(alias = "xmlBoolToText")]
pub fn xml_bool_to_text(boolval: bool) -> &'static str {
    if boolval {
        "True"
    } else {
        "False"
    }
}

/// This is a generic signature for the XML shell input function.
///
/// Returns a string which will be freed by the Shell.
#[doc(alias = "xmlShellReadlineFunc")]
#[cfg(feature = "xpath")]
pub type XmlShellReadlineFunc = unsafe fn(prompt: *mut c_char) -> *mut c_char;

/// A debugging shell context.  
/// TODO: add the defined function tables.
#[cfg(feature = "xpath")]
pub type XmlShellCtxtPtr<'a> = *mut XmlShellCtxt<'a>;
#[doc(alias = "xmlShellCtxt")]
#[cfg(feature = "xpath")]
#[repr(C)]
pub struct XmlShellCtxt<'a> {
    filename: *mut c_char,
    doc: Option<XmlDocPtr>,
    node: *mut XmlNode,
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
pub type XmlShellCmd = unsafe fn(
    ctxt: XmlShellCtxtPtr,
    arg: *mut c_char,
    node: *mut XmlNode,
    node2: *mut XmlNode,
) -> i32;

/// Print the xpath error to libxml default error channel
#[doc(alias = "xmlShellPrintXPathError")]
#[cfg(feature = "xpath")]
pub fn xml_shell_print_xpath_error(error_type: XmlXPathObjectType, arg: Option<&str>) {
    use crate::generic_error;

    let arg = arg.unwrap_or("Result");

    match error_type {
        XmlXPathObjectType::XPathUndefined => {
            generic_error!("{arg}: no such node\n");
        }
        XmlXPathObjectType::XPathBoolean => {
            generic_error!("{arg} is a Boolean\n");
        }
        XmlXPathObjectType::XPathNumber => {
            generic_error!("{arg} is a number\n");
        }
        XmlXPathObjectType::XPathString => {
            generic_error!("{arg} is a string\n");
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XPathPoint => {
            generic_error!("{arg} is a point\n");
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XPathRange => {
            generic_error!("{arg} is a range\n");
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XPathLocationset => {
            generic_error!("{arg} is a range\n");
        }
        XmlXPathObjectType::XPathUsers => {
            generic_error!("{arg} is user-defined\n");
        }
        XmlXPathObjectType::XPathXSLTTree => {
            generic_error!("{arg} is an XSLT value tree\n");
        }
        _ => unreachable!(),
    }
}

/// Print node to the output FILE
#[doc(alias = "xmlShellPrintNodeCtxt")]
#[cfg(feature = "libxml_output")]
unsafe fn xml_shell_print_node_ctxt(ctxt: XmlShellCtxtPtr, node: XmlGenericNodePtr) {
    // if node.is_null() {
    //     return;
    // }

    let stdout = &mut stdout();
    let fp = if ctxt.is_null() {
        stdout as &mut dyn Write
    } else {
        (*ctxt).output.as_mut()
    };
    let mut boxed = Box::new(fp);
    if node.element_type() == XmlElementType::XmlDocumentNode {
        node.as_document_node()
            .unwrap()
            .as_mut()
            .dump_file(&mut boxed);
    } else if node.element_type() == XmlElementType::XmlAttributeNode {
        xml_debug_dump_attr_list(&mut boxed, XmlAttrPtr::try_from(node).ok(), 0);
    } else {
        // Is this `unwrap` OK ?????
        XmlNodePtr::try_from(node)
            .unwrap()
            .dump_file(&mut boxed, node.document());
    }

    writeln!(boxed);
}

/// Prints result to the output FILE
#[doc(alias = "xmlShellPrintXPathResultCtxt")]
unsafe fn xml_shell_print_xpath_result_ctxt(ctxt: XmlShellCtxtPtr, list: XmlXPathObjectPtr) {
    if !ctxt.is_null() {
        return;
    }

    if !list.is_null() {
        match (*list).typ {
            XmlXPathObjectType::XPathNodeset => {
                #[cfg(feature = "libxml_output")]
                if let Some(nodeset) = (*list).nodesetval.as_deref() {
                    for &node in &nodeset.node_tab {
                        xml_shell_print_node_ctxt(ctxt, XmlGenericNodePtr::from_raw(node).unwrap());
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

/// Prints result to the output FILE
#[doc(alias = "xmlShellPrintXPathResult")]
#[cfg(feature = "xpath")]
pub unsafe fn xml_shell_print_xpath_result(list: XmlXPathObjectPtr) {
    xml_shell_print_xpath_result_ctxt(null_mut(), list);
}

/// Implements the XML shell function "ls"
/// Does an Unix like listing of the given node (like a directory)
///
/// Returns 0
#[doc(alias = "xmlShellList")]
#[cfg(feature = "xpath")]
pub unsafe fn xml_shell_list(
    ctxt: XmlShellCtxtPtr,
    _arg: *mut c_char,
    node: Option<XmlGenericNodePtr>,
    _node2: Option<XmlGenericNodePtr>,
) -> i32 {
    if ctxt.is_null() {
        return 0;
    }
    let Some(node) = node else {
        writeln!((*ctxt).output, "NULL");
        return 0;
    };
    let mut cur = if let Ok(doc) = XmlDocPtr::try_from(node) {
        XmlGenericNodePtr::from_raw(doc.children.map_or(null_mut(), |c| c.as_ptr()))
    } else if let Ok(ns) = XmlNsPtr::try_from(node) {
        xml_ls_one_node(&mut (*ctxt).output, Some(ns.into()));
        return 0;
    } else if let Some(children) = node.children() {
        XmlGenericNodePtr::from_raw(children.as_ptr())
    } else {
        xml_ls_one_node(&mut (*ctxt).output, Some(node));
        return 0;
    };
    while let Some(now) = cur {
        xml_ls_one_node(&mut (*ctxt).output, Some(now));
        cur = now
            .next()
            .and_then(|n| XmlGenericNodePtr::from_raw(n.as_ptr()));
    }
    0
}

/// Implements the XML shell function "base"
/// dumps the current XML base of the node
///
/// Returns 0
#[doc(alias = "xmlShellBase")]
#[cfg(feature = "xpath")]
pub unsafe fn xml_shell_base(
    ctxt: XmlShellCtxtPtr,
    _arg: *mut c_char,
    node: Option<XmlGenericNodePtr>,
    _node2: Option<XmlGenericNodePtr>,
) -> i32 {
    if ctxt.is_null() {
        return 0;
    }
    let Some(node) = node else {
        writeln!((*ctxt).output, "NULL");
        return 0;
    };

    if let Some(base) = node.get_base(node.document()) {
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
pub unsafe fn xml_shell_dir(
    ctxt: XmlShellCtxtPtr,
    _arg: *mut c_char,
    node: Option<XmlGenericNodePtr>,
    _node2: Option<XmlGenericNodePtr>,
) -> i32 {
    if ctxt.is_null() {
        return 0;
    }
    let Some(node) = node else {
        writeln!((*ctxt).output, "NULL");
        return 0;
    };
    if node.element_type() == XmlElementType::XmlDocumentNode
        || node.element_type() == XmlElementType::XmlHTMLDocumentNode
    {
        xml_debug_dump_document_head(
            Some(&mut (*ctxt).output),
            node.as_document_node().map(|d| d.as_ref()),
        );
    } else if node.element_type() == XmlElementType::XmlAttributeNode {
        xml_debug_dump_attr(&mut (*ctxt).output, XmlAttrPtr::try_from(node).ok(), 0);
    } else {
        xml_debug_dump_one_node(&mut (*ctxt).output, Some(node), 0);
    }
    0
}

/// Implements the XML shell function "load"
/// loads a new document specified by the filename
///
/// Returns 0 or -1 if loading failed
#[doc(alias = "xmlShellLoad")]
#[cfg(feature = "xpath")]
pub unsafe fn xml_shell_load(
    ctxt: XmlShellCtxtPtr,
    filename: &str,
    _node: Option<XmlGenericNodePtr>,
    _node2: Option<XmlGenericNodePtr>,
) -> i32 {
    use crate::{
        libxml::{globals::xml_free, htmlparser::html_parse_file, xmlstring::xml_strdup},
        parser::xml_read_file,
        uri::canonic_path,
        xpath::{xml_xpath_free_context, xml_xpath_new_context},
    };

    use crate::tree::xml_free_doc;

    let mut html: i32 = 0;

    if ctxt.is_null() {
        return -1;
    }
    if let Some(doc) = (*ctxt).doc {
        html = (doc.typ == XmlElementType::XmlHTMLDocumentNode) as i32;
    }

    let Some(doc) = (if html != 0 {
        #[cfg(feature = "html")]
        {
            html_parse_file(filename, None)
        }
        #[cfg(not(feature = "html"))]
        {
            write!((*ctxt).output, "HTML support not compiled in\n".as_ptr());
            None
        }
    } else {
        xml_read_file(filename, None, 0)
    }) else {
        return -1;
    };
    if (*ctxt).loaded == 1 {
        if let Some(doc) = (*ctxt).doc {
            xml_free_doc(doc);
        }
    }
    (*ctxt).loaded = 1;
    #[cfg(feature = "xpath")]
    {
        xml_xpath_free_context((*ctxt).pctxt);
    }
    xml_free((*ctxt).filename as _);
    (*ctxt).doc = Some(doc);
    (*ctxt).node = doc.as_ptr() as *mut XmlNode;
    #[cfg(feature = "xpath")]
    {
        (*ctxt).pctxt = xml_xpath_new_context(Some(doc));
    }
    let canonic = canonic_path(filename);
    let canonic = CString::new(canonic.as_ref()).unwrap();
    (*ctxt).filename = xml_strdup(canonic.as_ptr() as *const u8) as *mut i8;
    0
}

/// Print node to the output FILE
#[doc(alias = "xmlShellPrintNode")]
#[cfg(all(feature = "xpath", feature = "libxml_output"))]
pub unsafe fn xml_shell_print_node(node: XmlGenericNodePtr) {
    xml_shell_print_node_ctxt(null_mut(), node);
}

/// Implements the XML shell function "cat"
/// dumps the serialization node content (XML or HTML).
///
/// Returns 0
#[doc(alias = "xmlShellCat")]
#[cfg(all(feature = "xpath", feature = "libxml_output"))]
pub unsafe fn xml_shell_cat(
    ctxt: XmlShellCtxtPtr,
    _arg: *mut c_char,
    node: Option<XmlGenericNodePtr>,
    _node2: Option<XmlGenericNodePtr>,
) -> i32 {
    use crate::libxml::htmltree::{html_doc_dump, html_node_dump_file};

    if ctxt.is_null() {
        return 0;
    }
    let Some(node) = node else {
        writeln!((*ctxt).output, "NULL");
        return 0;
    };
    if let Some(doc) = (*ctxt)
        .doc
        .filter(|doc| doc.typ == XmlElementType::XmlHTMLDocumentNode)
    {
        #[cfg(feature = "html")]
        if let Some(doc) = XmlDocPtr::try_from(node)
            .ok()
            .filter(|doc| doc.element_type() == XmlElementType::XmlHTMLDocumentNode)
        {
            html_doc_dump(&mut (*ctxt).output, doc);
        } else {
            html_node_dump_file(&mut (*ctxt).output, Some(doc), Some(node));
        }
        #[cfg(not(feature = "html"))]
        if let Some(mut doc) = XmlDocPtr::try_from(node)
            .ok()
            .filter(|doc| doc.element_type() == XmlElementType::XmlDocumentNode)
        {
            doc.dump_file((*ctxt).output);
        } else {
            xml_elem_dump((*ctxt).output, (*ctxt).doc, node);
        }
    } else if let Some(mut doc) = XmlDocPtr::try_from(node)
        .ok()
        .filter(|doc| doc.element_type() == XmlElementType::XmlDocumentNode)
    {
        doc.dump_file(&mut (*ctxt).output);
    } else {
        // Is this `unwrap` OK ????
        XmlNodePtr::try_from(node)
            .unwrap()
            .dump_file(&mut (*ctxt).output, (*ctxt).doc);
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
    node: XmlGenericNodePtr,
    _node2: Option<XmlNodePtr>,
) -> i32 {
    use std::fs::File;

    use crate::libxml::htmltree::html_save_file;

    // if node.is_null() {
    //     return -1;
    // }
    if filename.is_empty() {
        return -1;
    }
    let cfilename = CString::new(filename).unwrap();
    match node.element_type() {
        XmlElementType::XmlDocumentNode => {
            if (*ctxt)
                .doc
                .map_or(true, |mut doc| doc.save_file(filename) < -1)
            {
                generic_error!("Failed to write to {filename}\n");
                return -1;
            }
        }
        XmlElementType::XmlHTMLDocumentNode => {
            #[cfg(feature = "html")]
            if html_save_file(cfilename.as_ptr(), (*ctxt).doc.unwrap()) < 0 {
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
                    let mut node = XmlNodePtr::try_from(node).unwrap();
                    node.dump_file(&mut f, (*ctxt).doc);
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
pub unsafe fn xml_shell_save(
    ctxt: XmlShellCtxtPtr,
    mut filename: *mut c_char,
    _node: Option<XmlGenericNodePtr>,
    _node2: Option<XmlGenericNodePtr>,
) -> i32 {
    use crate::libxml::htmltree::html_save_file;

    if ctxt.is_null() {
        return -1;
    }
    let Some(mut doc) = (*ctxt).doc else {
        return -1;
    };
    if filename.is_null() || *filename.add(0) == 0 {
        filename = (*ctxt).filename;
    }
    if filename.is_null() {
        return -1;
    }
    match doc.typ {
        XmlElementType::XmlDocumentNode => {
            if doc.save_file(CStr::from_ptr(filename).to_string_lossy().as_ref()) < 0 {
                generic_error!(
                    "Failed to save to {}\n",
                    CStr::from_ptr(filename as *const i8).to_string_lossy()
                );
            }
        }
        XmlElementType::XmlHTMLDocumentNode => {
            #[cfg(feature = "html")]
            if html_save_file(filename as *mut c_char, doc) < 0 {
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
pub unsafe fn xml_shell_validate(
    ctxt: XmlShellCtxtPtr,
    dtd: *mut c_char,
    _node: Option<XmlGenericNodePtr>,
    _node2: Option<XmlGenericNodePtr>,
) -> i32 {
    use crate::{
        globals::GLOBAL_STATE,
        libxml::{
            parser::xml_parse_dtd,
            valid::{xml_validate_document, xml_validate_dtd},
        },
        tree::xml_free_dtd,
    };

    use super::valid::XmlValidCtxt;

    let mut vctxt = XmlValidCtxt::default();
    let mut res: i32 = -1;

    if ctxt.is_null() {
        return -1;
    }
    let Some(doc) = (*ctxt).doc else {
        return -1;
    };
    vctxt.error = Some(GLOBAL_STATE.with_borrow(|state| state.generic_error));
    vctxt.warning = vctxt.error;

    if dtd.is_null() || *dtd.add(0) == 0 {
        res = xml_validate_document(addr_of_mut!(vctxt), doc);
    } else {
        let subset = xml_parse_dtd(
            None,
            (!dtd.is_null())
                .then(|| CStr::from_ptr(dtd as *const i8).to_string_lossy())
                .as_deref(),
        );
        if let Some(subset) = subset {
            res = xml_validate_dtd(addr_of_mut!(vctxt), doc, subset);
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
pub unsafe fn xml_shell_du(
    ctxt: XmlShellCtxtPtr,
    _arg: *mut c_char,
    tree: *mut XmlNode,
    _node2: Option<XmlGenericNodePtr>,
) -> i32 {
    let mut node: *mut XmlNode;
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
            if let Some(prefix) = (*node).ns.as_deref().and_then(|ns| ns.prefix()) {
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
pub unsafe fn xml_shell_pwd(
    _ctxt: XmlShellCtxtPtr,
    buffer: *mut c_char,
    node: *mut XmlNode,
    _node2: Option<XmlGenericNodePtr>,
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
unsafe fn xml_shell_rng_validate(
    sctxt: XmlShellCtxtPtr,
    schemas: &str,
    _node: Option<XmlGenericNodePtr>,
    _node2: Option<XmlGenericNodePtr>,
) -> i32 {
    use crate::{
        globals::GLOBAL_STATE,
        libxml::relaxng::{
            xml_relaxng_free, xml_relaxng_parse, xml_relaxng_set_valid_errors,
            xml_relaxng_validate_doc,
        },
        relaxng::{
            xml_relaxng_free_parser_ctxt, xml_relaxng_free_valid_ctxt, xml_relaxng_new_parser_ctxt,
            xml_relaxng_new_valid_ctxt,
        },
    };

    use super::relaxng::XmlRelaxNGPtr;

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
    let ret = if let Some(doc) = (*sctxt).doc {
        xml_relaxng_validate_doc(vctxt, doc)
    } else {
        -1
    };

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
unsafe fn xml_shell_grep(
    ctxt: XmlShellCtxtPtr,
    arg: *mut c_char,
    mut node: *mut XmlNode,
    _node2: Option<XmlGenericNodePtr>,
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
                xml_shell_list(ctxt, null_mut(), XmlGenericNodePtr::from_raw(node), None);
            }
        } else if (*node).element_type() == XmlElementType::XmlTextNode
            && !xml_strstr((*node).content, arg as *mut XmlChar).is_null()
        {
            let path = (*node).parent().unwrap().get_node_path().unwrap();
            write!((*ctxt).output, "{path} : ");
            xml_shell_list(
                ctxt,
                null_mut(),
                (*node)
                    .parent
                    .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr())),
                None,
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
unsafe fn xml_shell_set_content(
    ctxt: XmlShellCtxtPtr,
    value: *mut c_char,
    node: *mut XmlNode,
    _node2: Option<XmlGenericNodePtr>,
) -> i32 {
    let mut results: *mut XmlNode = null_mut();

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
unsafe fn xml_shell_register_namespace(
    ctxt: XmlShellCtxtPtr,
    arg: *mut c_char,
    _node: Option<XmlGenericNodePtr>,
    _node2: Option<XmlGenericNodePtr>,
) -> i32 {
    use crate::{libxml::xmlstring::xml_strdup, xpath::internals::xml_xpath_register_ns};

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
unsafe fn xml_shell_register_root_namespaces(
    ctxt: XmlShellCtxtPtr,
    _arg: *mut c_char,
    root: *mut XmlNode,
    _node2: Option<XmlGenericNodePtr>,
) -> i32 {
    use crate::xpath::internals::xml_xpath_register_ns;

    if root.is_null()
        || (*root).element_type() != XmlElementType::XmlElementNode
        || (*root).ns_def.is_none()
        || ctxt.is_null()
        || (*ctxt).pctxt.is_null()
    {
        return -1;
    }
    let mut ns = (*root).ns_def;
    while let Some(now) = ns {
        if let Some(prefix) = now.prefix() {
            let prefix = CString::new(prefix.as_ref()).unwrap();
            xml_xpath_register_ns((*ctxt).pctxt, prefix.as_ptr() as *const u8, now.href);
        } else {
            xml_xpath_register_ns((*ctxt).pctxt, c"defaultns".as_ptr() as _, now.href);
        }
        ns = XmlNsPtr::from_raw(now.next).unwrap();
    }
    0
}

/// Implements the XML shell function "setbase"
/// change the current XML base of the node
///
/// Returns 0
#[doc(alias = "xmlShellSetBase")]
#[cfg(feature = "libxml_tree")]
unsafe fn xml_shell_set_base(
    _ctxt: XmlShellCtxtPtr,
    arg: *mut c_char,
    node: *mut XmlNode,
    _node2: Option<XmlGenericNodePtr>,
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
        },
        tree::xml_free_doc,
        xpath::{
            xml_xpath_debug_dump_object, xml_xpath_eval, xml_xpath_free_context,
            xml_xpath_free_object, xml_xpath_new_context,
        },
    };

    let mut prompt: [u8; 500] = [0; 500];
    prompt[..c"/ > ".to_bytes().len()].copy_from_slice(c"/ > ".to_bytes());
    let mut cmdline: *mut c_char;
    let mut cur: *mut c_char;
    let mut command: [c_char; 100] = [0; 100];
    let mut arg: [c_char; 400] = [0; 400];
    let mut i: i32;
    let mut list: XmlXPathObjectPtr;

    // if doc.is_null() {
    //     return;
    // }
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
    (*ctxt).doc = Some(doc);
    (*ctxt).input = input.unwrap();
    (*ctxt).output = output.map_or(Box::new(stdout()) as Box<dyn Write + 'a>, |o| Box::new(o));
    (*ctxt).filename = xml_strdup(filename as *mut XmlChar) as *mut c_char;
    (*ctxt).node = doc.as_ptr() as *mut XmlNode;

    #[cfg(feature = "xpath")]
    {
        (*ctxt).pctxt = xml_xpath_new_context(Some(doc));
        if (*ctxt).pctxt.is_null() {
            xml_free(ctxt as _);
            return;
        }
    }
    loop {
        if (*ctxt).node == doc.as_ptr() as *mut XmlNode {
            snprintf(
                prompt.as_mut_ptr() as _,
                prompt.len(),
                c"%s > ".as_ptr(),
                c"/".as_ptr(),
            );
        } else if let Some(prefix) = (*(*ctxt).node)
            .ns
            .map(|ns| ns.prefix)
            .filter(|p| !p.is_null() && !(*ctxt).node.is_null() && !(*(*ctxt).node).name.is_null())
        {
            snprintf(
                prompt.as_mut_ptr() as _,
                prompt.len(),
                c"%s:%s > ".as_ptr(),
                prefix,
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

        // Get a new command line
        cmdline = ((*ctxt).input)(prompt.as_mut_ptr() as _);
        if cmdline.is_null() {
            break;
        }

        // Parse the command itself
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

        // Parse the argument
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

        // start interpreting the command
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
                xml_shell_validate(ctxt, arg.as_mut_ptr(), None, None);
            }
        } else if strcmp(command.as_ptr(), c"load".as_ptr()) == 0 {
            xml_shell_load(
                ctxt,
                CStr::from_ptr(arg.as_ptr()).to_string_lossy().as_ref(),
                None,
                None,
            );
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
                xml_shell_rng_validate(
                    ctxt,
                    CStr::from_ptr(arg.as_mut_ptr()).to_string_lossy().as_ref(),
                    None,
                    None,
                );
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
                xml_shell_save(ctxt, arg.as_mut_ptr(), None, None);
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
                    XmlGenericNodePtr::from_raw((*ctxt).node).unwrap(),
                    None,
                );
            }
        } else if strcmp(command.as_ptr(), c"grep".as_ptr()) == 0 {
            xml_shell_grep(ctxt, arg.as_mut_ptr(), (*ctxt).node, None);
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

            if xml_shell_pwd(ctxt, dir.as_mut_ptr(), (*ctxt).node, None) == 0 {
                let dir = CStr::from_ptr(dir.as_ptr()).to_string_lossy();
                writeln!((*ctxt).output, "{dir}");
            }
        } else if strcmp(command.as_ptr(), c"du".as_ptr()) == 0 {
            if arg[0] == 0 {
                xml_shell_du(ctxt, null_mut(), (*ctxt).node, None);
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
                        XmlXPathObjectType::XPathUndefined => {
                            generic_error!(
                                "{}: no such node\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XPathNodeset => {
                            if let Some(nodeset) = (*list).nodesetval.as_deref() {
                                for &node in &nodeset.node_tab {
                                    xml_shell_du(ctxt, null_mut(), node, None);
                                }
                            }
                        }
                        XmlXPathObjectType::XPathBoolean => {
                            generic_error!(
                                "{} is a Boolean\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XPathNumber => {
                            generic_error!(
                                "{} is a number\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XPathString => {
                            generic_error!(
                                "{} is a string\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XPathPoint => {
                            generic_error!(
                                "{} is a point\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XPathRange => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XPathLocationset => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XPathUsers => {
                            generic_error!(
                                "{} is user-defined\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XPathXSLTTree => {
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
            xml_shell_base(
                ctxt,
                null_mut(),
                XmlGenericNodePtr::from_raw((*ctxt).node),
                None,
            );
        } else if strcmp(command.as_ptr(), c"set".as_ptr()) == 0 {
            xml_shell_set_content(ctxt, arg.as_mut_ptr(), (*ctxt).node, None);
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
                xml_shell_register_namespace(ctxt, arg.as_mut_ptr(), None, None);
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
                let root = doc.get_root_element();
                xml_shell_register_root_namespaces(
                    ctxt,
                    null_mut(),
                    root.map_or(null_mut(), |node| node.as_ptr()),
                    None,
                );
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
                xml_shell_set_base(ctxt, arg.as_mut_ptr(), (*ctxt).node, None);
            }
        } else if strcmp(command.as_ptr(), c"ls".as_ptr()) == 0
            || strcmp(command.as_ptr(), c"dir".as_ptr()) == 0
        {
            let dir: i32 = (strcmp(command.as_ptr(), c"dir".as_ptr()) == 0) as i32;

            if arg[0] == 0 {
                if dir != 0 {
                    xml_shell_dir(
                        ctxt,
                        null_mut(),
                        XmlGenericNodePtr::from_raw((*ctxt).node),
                        None,
                    );
                } else {
                    xml_shell_list(
                        ctxt,
                        null_mut(),
                        XmlGenericNodePtr::from_raw((*ctxt).node),
                        None,
                    );
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
                        XmlXPathObjectType::XPathUndefined => {
                            generic_error!(
                                "{}: no such node\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XPathNodeset => {
                            let Some(nodeset) = (*list).nodesetval.as_deref() else {
                                break;
                            };

                            for &node in &nodeset.node_tab {
                                if dir != 0 {
                                    xml_shell_dir(
                                        ctxt,
                                        null_mut(),
                                        XmlGenericNodePtr::from_raw(node),
                                        None,
                                    );
                                } else {
                                    xml_shell_list(
                                        ctxt,
                                        null_mut(),
                                        XmlGenericNodePtr::from_raw(node),
                                        None,
                                    );
                                }
                            }
                        }
                        XmlXPathObjectType::XPathBoolean => {
                            generic_error!(
                                "{} is a Boolean\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XPathNumber => {
                            generic_error!(
                                "{} is a number\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XPathString => {
                            generic_error!(
                                "{} is a string\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XPathPoint => {
                            generic_error!(
                                "{} is a point\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XPathRange => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XPathLocationset => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XPathUsers => {
                            generic_error!(
                                "{} is user-defined\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XPathXSLTTree => {
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
                if xml_shell_pwd(ctxt, dir.as_mut_ptr(), (*ctxt).node, None) == 0 {
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
                        XmlXPathObjectType::XPathUndefined => {
                            generic_error!(
                                "{}: no such node\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XPathNodeset => {
                            if let Some(nodeset) = (*list).nodesetval.as_deref() {
                                for &node in &nodeset.node_tab {
                                    if xml_shell_pwd(ctxt, dir.as_mut_ptr(), node, None) == 0 {
                                        let dir = CStr::from_ptr(dir.as_ptr()).to_string_lossy();
                                        writeln!((*ctxt).output, "{dir}");
                                    }
                                }
                            }
                        }
                        XmlXPathObjectType::XPathBoolean => {
                            generic_error!(
                                "{} is a Boolean\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XPathNumber => {
                            generic_error!(
                                "{} is a number\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XPathString => {
                            generic_error!(
                                "{} is a string\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XPathPoint => {
                            generic_error!(
                                "{} is a point\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XPathRange => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XPathLocationset => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XPathUsers => {
                            generic_error!(
                                "{} is user-defined\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XPathXSLTTree => {
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
                (*ctxt).node = doc.as_ptr() as *mut XmlNode;
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
                        XmlXPathObjectType::XPathUndefined => {
                            generic_error!(
                                "{}: no such node\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XPathNodeset => {
                            if let Some(nodeset) = (*list).nodesetval.as_deref() {
                                if nodeset.node_tab.len() == 1 {
                                    (*ctxt).node = nodeset.node_tab[0];
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
                                        nodeset.node_tab.len()
                                    );
                                }
                            } else {
                                generic_error!(
                                    "{} is an empty Node Set\n",
                                    CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                                );
                            }
                        }
                        XmlXPathObjectType::XPathBoolean => {
                            generic_error!(
                                "{} is a Boolean\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XPathNumber => {
                            generic_error!(
                                "{} is a number\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XPathString => {
                            generic_error!(
                                "{} is a string\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XPathPoint => {
                            generic_error!(
                                "{} is a point\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XPathRange => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XPathLocationset => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XPathUsers => {
                            generic_error!(
                                "{} is user-defined\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XPathXSLTTree => {
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
                xml_shell_cat(
                    ctxt,
                    null_mut(),
                    XmlGenericNodePtr::from_raw((*ctxt).node),
                    None,
                );
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
                        XmlXPathObjectType::XPathUndefined => {
                            generic_error!(
                                "{}: no such node\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XPathNodeset => {
                            if let Some(nodeset) = (*list).nodesetval.as_deref() {
                                for &node in &nodeset.node_tab {
                                    if i > 0 {
                                        writeln!((*ctxt).output, " -------");
                                    }
                                    xml_shell_cat(
                                        ctxt,
                                        null_mut(),
                                        XmlGenericNodePtr::from_raw(node),
                                        None,
                                    );
                                }
                            }
                        }
                        XmlXPathObjectType::XPathBoolean => {
                            generic_error!(
                                "{} is a Boolean\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XPathNumber => {
                            generic_error!(
                                "{} is a number\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XPathString => {
                            generic_error!(
                                "{} is a string\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XPathPoint => {
                            generic_error!(
                                "{} is a point\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XPathRange => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XPathLocationset => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XPathUsers => {
                            generic_error!(
                                "{} is user-defined\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XPathXSLTTree => {
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
        xml_free_doc(doc);
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
