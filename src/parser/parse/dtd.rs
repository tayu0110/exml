use std::{borrow::Cow, ptr::null_mut};

use crate::{
    encoding::{XmlCharEncoding, detect_encoding},
    error::XmlParserErrors,
    generic_error,
    globals::{GenericErrorContext, get_parser_debug_entities},
    hash::{XmlHashTable, XmlHashTableRef},
    libxml::{
        chvalid::xml_is_blank_char,
        parser::{SAX_COMPAT_MODE, XmlParserInputState, XmlParserOption},
        sax2::{xml_sax2_entity_decl, xml_sax2_get_entity},
        valid::{xml_free_doc_element_content, xml_new_doc_element_content},
    },
    parser::{
        XmlParserCtxt, XmlParserInput, split_qname2, xml_err_memory, xml_err_msg_str,
        xml_fatal_err, xml_fatal_err_msg, xml_fatal_err_msg_int, xml_fatal_err_msg_str, xml_ns_err,
        xml_validity_error, xml_warning_msg,
    },
    tree::{
        XML_ENT_EXPANDING, XML_ENT_PARSED, XmlAttributeDefault, XmlAttributeType, XmlDocProperties,
        XmlElementContentOccur, XmlElementContentPtr, XmlElementContentType, XmlElementTypeVal,
        XmlEntityType, XmlEnumeration, xml_create_enumeration, xml_create_int_subset, xml_new_doc,
        xml_new_dtd,
    },
    uri::XmlURI,
};

use super::{
    attr_normalize_space, parse_att_value, parse_comment, parse_entity_value, parse_external_id,
    parse_name, parse_nmtoken, parse_pi, parse_text_decl,
};

impl XmlParserCtxt {
    /// Add a defaulted attribute for an element
    #[doc(alias = "xmlAddDefAttrs")]
    unsafe fn add_def_attrs(&mut self, fullname: &str, fullattr: &str, value: &str) {
        unsafe {
            // Allows to detect attribute redefinitions
            if self
                .atts_special
                .filter(|t| t.lookup2(fullname, Some(fullattr)).is_some())
                .is_some()
            {
                return;
            }

            // split the element name into prefix:localname , the string found
            // are within the DTD and then not associated to namespace names.
            let (prefix, name) = split_qname2(fullname)
                .map(|(pre, loc)| (Some(pre), loc))
                .unwrap_or((None, fullname));

            // make sure there is some storage
            let defaults = self
                .atts_default
                .entry((
                    name.to_owned().into(),
                    prefix.map(|prefix| prefix.to_owned().into()),
                ))
                .or_default();

            // Split the element name into prefix:localname , the string found
            // are within the DTD and hen not associated to namespace names.
            let (prefix, name) = split_qname2(fullattr)
                .map(|(pre, loc)| (Some(pre), loc))
                .unwrap_or((None, fullattr));

            defaults.push((
                name.to_owned(),
                prefix.map(|prefix| prefix.to_owned()),
                value.to_owned(),
                if self.external != 0 {
                    Some("external")
                } else {
                    None
                },
            ));
            return;

            xml_err_memory(self, None);
        }
    }

    /// Register this attribute type
    #[doc(alias = "xmlAddSpecialAttr")]
    unsafe fn add_special_attr(&mut self, fullname: &str, fullattr: &str, typ: XmlAttributeType) {
        unsafe {
            let mut atts_special = if let Some(table) = self.atts_special {
                table
            } else {
                let table = XmlHashTable::with_capacity(10);
                let Some(table) = XmlHashTableRef::from_table(table) else {
                    xml_err_memory(self, None);
                    return;
                };
                self.atts_special = Some(table);
                table
            };

            if atts_special.lookup2(fullname, Some(fullattr)).is_some() {
                return;
            }

            atts_special.add_entry2(fullname, Some(fullattr), typ).ok();
        }
    }
}

/// Parse a DOCTYPE declaration
///
/// ```text
/// [28] doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' (markupdecl | PEReference | S)* ']' S?)? '>'
///
/// [ VC: Root Element Type ]
/// The Name in the document type declaration must match the element type of the root element.
/// ```
#[doc(alias = "xmlParseDocTypeDecl")]
pub(crate) unsafe fn parse_doctypedecl(ctxt: &mut XmlParserCtxt) {
    unsafe {
        // We know that '<!DOCTYPE' has been detected.
        ctxt.advance(9);
        ctxt.skip_blanks();

        // Parse the DOCTYPE name.
        let name = parse_name(ctxt);
        if name.is_none() {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrNameRequired,
                "xmlParseDocTypeDecl : no DOCTYPE name !\n",
            );
        }
        ctxt.int_sub_name = name.as_deref().map(|n| n.to_owned());
        ctxt.skip_blanks();

        // Check for SystemID and ExternalID
        let (external_id, uri) = parse_external_id(ctxt, true);

        if uri.is_some() || external_id.is_some() {
            ctxt.has_external_subset = 1;
        }
        ctxt.ext_sub_uri = uri;
        ctxt.ext_sub_system = external_id;

        ctxt.skip_blanks();

        // Create and update the internal subset.
        if ctxt.disable_sax == 0 {
            if let Some(internal_subset) =
                ctxt.sax.as_deref_mut().and_then(|sax| sax.internal_subset)
            {
                internal_subset(
                    ctxt.user_data.clone(),
                    name.as_deref(),
                    ctxt.ext_sub_system.as_deref(),
                    ctxt.ext_sub_uri.as_deref(),
                );
            }
        }
        if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            return;
        }

        // Is there any internal subset declarations ?
        // they are handled separately in xmlParseInternalSubset()
        if ctxt.current_byte() == b'[' {
            return;
        }

        // We should be at the end of the DOCTYPE declaration.
        if ctxt.current_byte() != b'>' {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrDoctypeNotFinished, None);
        }
        ctxt.skip_char();
    }
}

/// Parse the internal subset declaration
///
/// ```text
/// [28 end] ('[' (markupdecl | PEReference | S)* ']' S?)? '>'
/// ```
#[doc(alias = "xmlParseInternalSubset")]
pub(crate) unsafe fn parse_internal_subset(ctxt: &mut XmlParserCtxt) {
    unsafe {
        // Is there any DTD definition ?
        if ctxt.current_byte() == b'[' {
            let base_input_nr = ctxt.input_tab.len();
            ctxt.instate = XmlParserInputState::XmlParserDTD;
            ctxt.skip_char();
            // Parse the succession of Markup declarations and
            // PEReferences.
            // Subsequence (markupdecl | PEReference | S)*
            ctxt.skip_blanks();
            while (ctxt.current_byte() != b']' || ctxt.input_tab.len() > base_input_nr)
                && !matches!(ctxt.instate, XmlParserInputState::XmlParserEOF)
            {
                // Conditional sections are allowed from external entities included
                // by PE References in the internal subset.
                if ctxt.input_tab.len() > 1
                    && ctxt.input().unwrap().filename.is_some()
                    && ctxt.content_bytes().starts_with(b"<![")
                {
                    parse_conditional_sections(ctxt);
                } else if ctxt.current_byte() == b'<'
                    && (ctxt.nth_byte(1) == b'!' || ctxt.nth_byte(1) == b'?')
                {
                    parse_markup_decl(ctxt);
                } else if ctxt.current_byte() == b'%' {
                    parse_pe_reference(ctxt);
                } else {
                    xml_fatal_err(
                        ctxt,
                        XmlParserErrors::XmlErrInternalError,
                        Some("xmlParseInternalSubset: error detected in Markup declaration\n"),
                    );
                    ctxt.halt();
                    return;
                }
                ctxt.skip_blanks();
                ctxt.shrink();
                ctxt.grow();
            }
            if ctxt.current_byte() == b']' {
                ctxt.skip_char();
                ctxt.skip_blanks();
            }
        }

        // We should be at the end of the DOCTYPE declaration.
        if ctxt.current_byte() != b'>' {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrDoctypeNotFinished, None);
            return;
        }
        ctxt.skip_char();
    }
}

/// Parse markup declarations. Always consumes '<!' or '<?'.
///
/// ```text
/// [29] markupdecl ::= elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment
///
/// [ VC: Proper Declaration/PE Nesting ]
/// Parameter-entity replacement text must be properly nested with
/// markup declarations. That is to say, if either the first character
/// or the last character of a markup declaration (markupdecl above) is
/// contained in the replacement text for a parameter-entity reference,
/// both must be contained in the same replacement text.
///
/// [ WFC: PEs in Internal Subset ]
/// In the internal DTD subset, parameter-entity references can occur
/// only where markup declarations can occur, not within markup declarations.
/// (This does not apply to references that occur in external parameter
/// entities or to the external subset.)
/// ```
#[doc(alias = "xmlParseMarkupDecl")]
unsafe fn parse_markup_decl(ctxt: &mut XmlParserCtxt) {
    unsafe {
        ctxt.grow();
        if ctxt.current_byte() == b'<' {
            if ctxt.nth_byte(1) == b'!' {
                match ctxt.nth_byte(2) {
                    b'E' => match ctxt.nth_byte(3) {
                        b'L' => {
                            parse_element_decl(ctxt);
                        }
                        b'N' => parse_entity_decl(ctxt),
                        _ => ctxt.advance(2),
                    },
                    b'A' => parse_attribute_list_decl(ctxt),
                    b'N' => parse_notation_decl(ctxt),
                    b'-' => parse_comment(ctxt),
                    // there is an error but it will be detected later
                    _ => ctxt.advance(2),
                }
            } else if ctxt.nth_byte(1) == b'?' {
                parse_pi(ctxt);
            }
        }

        // detect requirement to exit there and act accordingly
        // and avoid having instate overridden later on
        if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            return;
        }

        ctxt.instate = XmlParserInputState::XmlParserDTD;
    }
}

/// Parse Markup declarations from an external subset
///
/// ```text
/// [30] extSubset ::= textDecl? extSubsetDecl
/// [31] extSubsetDecl ::= (markupdecl | conditionalSect | PEReference | S) *
/// ```
#[doc(alias = "xmlParseExternalSubset")]
pub unsafe fn parse_external_subset(
    ctxt: &mut XmlParserCtxt,
    external_id: Option<&str>,
    system_id: Option<&str>,
) {
    unsafe {
        ctxt.detect_sax2();
        ctxt.grow();

        if ctxt.encoding.is_none() && ctxt.input().unwrap().remainder_len() >= 4 {
            let mut start = [0; 4];
            start.copy_from_slice(&ctxt.content_bytes()[..4]);
            let enc = detect_encoding(&start);
            if !matches!(enc, XmlCharEncoding::None) {
                ctxt.switch_encoding(enc);
            }
        }

        if ctxt.content_bytes().starts_with(b"<?xml") {
            parse_text_decl(ctxt);
            if ctxt.err_no == XmlParserErrors::XmlErrUnsupportedEncoding as i32 {
                // The XML REC instructs us to stop parsing right here
                ctxt.halt();
                return;
            }
        }
        let my_doc = if let Some(my_doc) = ctxt.my_doc {
            my_doc
        } else {
            ctxt.my_doc = xml_new_doc(Some("1.0"));
            let Some(mut my_doc) = ctxt.my_doc else {
                xml_err_memory(ctxt, Some("New Doc failed"));
                return;
            };
            my_doc.properties = XmlDocProperties::XmlDocInternal as i32;
            my_doc
        };
        if my_doc.int_subset.is_none() {
            xml_create_int_subset(ctxt.my_doc, None, external_id, system_id);
        }

        ctxt.instate = XmlParserInputState::XmlParserDTD;
        ctxt.external = 1;
        ctxt.skip_blanks();
        while !matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) && ctxt.current_byte() != 0
        {
            ctxt.grow();
            match ctxt.content_bytes() {
                [b'<', b'!', b'[', ..] => parse_conditional_sections(ctxt),
                [b'<', b'!', ..] | [b'<', b'?', ..] => parse_markup_decl(ctxt),
                _ => {
                    xml_fatal_err(ctxt, XmlParserErrors::XmlErrExtSubsetNotFinished, None);
                    ctxt.halt();
                    return;
                }
            }
            ctxt.skip_blanks();
            ctxt.shrink();
        }

        if ctxt.current_byte() != 0 {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrExtSubsetNotFinished, None);
        }
    }
}

/// Parse an element declaration. Always consumes '<!'.
///
/// ```text
/// [45] elementdecl ::= '<!ELEMENT' S Name S contentspec S? '>'
///
/// [ VC: Unique Element Type Declaration ]
/// No element type may be declared more than once
/// ```
///
/// Returns the type of the element, or -1 in case of error
#[doc(alias = "xmlParseElementDecl")]
unsafe fn parse_element_decl(ctxt: &mut XmlParserCtxt) -> i32 {
    unsafe {
        if !ctxt.content_bytes().starts_with(b"<!") {
            return -1;
        }
        ctxt.advance(2);

        // GROW; done in the caller

        if !ctxt.content_bytes().starts_with(b"ELEMENT") {
            return -1;
        }
        let inputid: i32 = ctxt.input().unwrap().id;
        ctxt.advance(7);

        if ctxt.skip_blanks() == 0 {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                "Space required after 'ELEMENT'\n",
            );
            return -1;
        }
        let Some(name) = parse_name(ctxt) else {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrNameRequired,
                "xmlParseElementDecl: no name for Element\n",
            );
            return -1;
        };

        if ctxt.skip_blanks() == 0 {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                "Space required after the element name\n",
            );
        }

        let mut content: XmlElementContentPtr = null_mut();
        let ret = match ctxt.content_bytes() {
            [b'E', b'M', b'P', b'T', b'Y', ..] => {
                ctxt.advance(5);
                // Element must always be empty.
                Some(XmlElementTypeVal::XmlElementTypeEmpty)
            }
            [b'A', b'N', b'Y', ..] => {
                ctxt.advance(3);
                // Element is a generic container.
                Some(XmlElementTypeVal::XmlElementTypeAny)
            }
            [b'(', ..] => parse_element_content_decl(ctxt, &name, &mut content),
            _ => {
                // [ WFC: PEs in Internal Subset ] error handling.
                if ctxt.current_byte() == b'%' && ctxt.external == 0 && ctxt.input_tab.len() == 1 {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrPERefInIntSubset,
                        "PEReference: forbidden within markup decl in internal subset\n",
                    );
                } else {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrElemcontentNotStarted,
                        "xmlParseElementDecl: 'EMPTY', 'ANY' or '(' expected\n",
                    );
                }
                return -1;
            }
        };

        ctxt.skip_blanks();

        if ctxt.current_byte() != b'>' {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrGtRequired, None);
            if !content.is_null() {
                xml_free_doc_element_content(ctxt.my_doc, content);
            }
        } else {
            if inputid != ctxt.input().unwrap().id {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrEntityBoundary,
                    "Element declaration doesn't start and stop in the same entity\n",
                );
            }

            ctxt.skip_char();
            if let Some(element_decl) = ctxt
                .sax
                .as_deref_mut()
                .filter(|_| ctxt.disable_sax == 0)
                .and_then(|sax| sax.element_decl)
            {
                if !content.is_null() {
                    (*content).parent = null_mut();
                }
                element_decl(ctxt.user_data.clone(), &name, ret, content);
                if !content.is_null() && (*content).parent.is_null() {
                    // this is a trick: if xmlAddElementDecl is called,
                    // instead of copying the full tree it is plugged directly
                    // if called from the parser. Avoid duplicating the
                    // interfaces or change the API/ABI
                    xml_free_doc_element_content(ctxt.my_doc, content);
                }
            } else if !content.is_null() {
                xml_free_doc_element_content(ctxt.my_doc, content);
            }
        }
        ret.map_or(-1, |ret| ret as i32)
    }
}

/// Parse the declaration for an Element content either Mixed or Children,
/// the cases EMPTY and ANY are handled directly in xmlParseElementDecl
///
/// ```text
/// [46] contentspec ::= 'EMPTY' | 'ANY' | Mixed | children
/// ```
///
/// returns: the type of element content XML_ELEMENT_TYPE_xxx
#[doc(alias = "xmlParseElementContentDecl")]
unsafe fn parse_element_content_decl(
    ctxt: &mut XmlParserCtxt,
    name: &str,
    result: &mut XmlElementContentPtr,
) -> Option<XmlElementTypeVal> {
    unsafe {
        let tree: XmlElementContentPtr;
        let inputid: i32 = ctxt.input().unwrap().id;

        *result = null_mut();

        if ctxt.current_byte() != b'(' {
            xml_fatal_err_msg_str!(
                ctxt,
                XmlParserErrors::XmlErrElemcontentNotStarted,
                "xmlParseElementContentDecl : {} '(' expected\n",
                name
            );
            return None;
        }
        ctxt.skip_char();
        ctxt.grow();
        if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            return None;
        }
        ctxt.skip_blanks();
        let res = if ctxt.content_bytes().starts_with(b"#PCDATA") {
            tree = parse_element_mixed_content_decl(ctxt, inputid);
            XmlElementTypeVal::XmlElementTypeMixed
        } else {
            tree = parse_element_children_content_decl_priv(ctxt, inputid, 1);
            XmlElementTypeVal::XmlElementTypeElement
        };
        ctxt.skip_blanks();
        *result = tree;
        Some(res)
    }
}

/// Parse the declaration for a Mixed Element content
/// The leading '(' and spaces have been skipped in xmlParseElementContentDecl
///
/// ```text
/// [47] children ::= (choice | seq) ('?' | '*' | '+')?
/// [48] cp ::= (Name | choice | seq) ('?' | '*' | '+')?
/// [49] choice ::= '(' S? cp ( S? '|' S? cp )* S? ')'
/// [50] seq ::= '(' S? cp ( S? ',' S? cp )* S? ')'
///
/// [ VC: Proper Group/PE Nesting ] applies to [49] and [50]  
/// ```
/// TODO Parameter-entity replacement text must be properly nested
///    with parenthesized groups. That is to say, if either of the
///    opening or closing parentheses in a choice, seq, or Mixed
///    construct is contained in the replacement text for a parameter
///    entity, both must be contained in the same replacement text. For
///    interoperability, if a parameter-entity reference appears in a
///    choice, seq, or Mixed construct, its replacement text should not
///    be empty, and neither the first nor last non-blank character of
///    the replacement text should be a connector (| or ,).
///
/// Returns the tree of xmlElementContentPtr describing the element hierarchy.
#[doc(alias = "xmlParseElementChildrenContentDeclPriv")]
unsafe fn parse_element_children_content_decl_priv(
    ctxt: &mut XmlParserCtxt,
    inputchk: i32,
    depth: i32,
) -> XmlElementContentPtr {
    unsafe {
        let mut ret: XmlElementContentPtr;
        let mut cur: XmlElementContentPtr;
        let mut last: XmlElementContentPtr = null_mut();
        let mut op: XmlElementContentPtr;
        let mut typ = 0;

        if (depth > 128 && ctxt.options & XmlParserOption::XmlParseHuge as i32 == 0) || depth > 2048
        {
            xml_fatal_err_msg_int!(
                ctxt,
                XmlParserErrors::XmlErrElemcontentNotFinished,
                "xmlParseElementChildrenContentDecl : depth %d too deep, use xmlParserOption::XML_PARSE_HUGE\n",
                depth
            );
            return null_mut();
        }
        ctxt.skip_blanks();
        ctxt.grow();
        if ctxt.current_byte() == b'(' {
            let inputid: i32 = ctxt.input().unwrap().id;

            // Recurse on first child
            ctxt.skip_char();
            ctxt.skip_blanks();
            cur = parse_element_children_content_decl_priv(ctxt, inputid, depth + 1);
            ret = cur;
            if cur.is_null() {
                return null_mut();
            }
            ctxt.skip_blanks();
            ctxt.grow();
        } else {
            let Some(elem) = parse_name(ctxt) else {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrElemcontentNotStarted, None);
                return null_mut();
            };
            cur = xml_new_doc_element_content(
                ctxt.my_doc,
                Some(&elem),
                XmlElementContentType::XmlElementContentElement,
            );
            ret = cur;
            if cur.is_null() {
                xml_err_memory(ctxt, None);
                return null_mut();
            }
            ctxt.grow();
            match ctxt.current_byte() {
                b'?' => {
                    (*cur).ocur = XmlElementContentOccur::XmlElementContentOpt;
                    ctxt.skip_char();
                }
                b'*' => {
                    (*cur).ocur = XmlElementContentOccur::XmlElementContentMult;
                    ctxt.skip_char();
                }
                b'+' => {
                    (*cur).ocur = XmlElementContentOccur::XmlElementContentPlus;
                    ctxt.skip_char();
                }
                _ => (*cur).ocur = XmlElementContentOccur::XmlElementContentOnce,
            }
            ctxt.grow();
        }
        ctxt.skip_blanks();
        while ctxt.current_byte() != b')'
            && !matches!(ctxt.instate, XmlParserInputState::XmlParserEOF)
        {
            // Each loop we parse one separator and one element.
            if ctxt.current_byte() == b',' {
                if typ == 0 {
                    typ = ctxt.current_byte();
                } else if typ != ctxt.current_byte() {
                    // Detect "Name | Name , Name" error
                    xml_fatal_err_msg_int!(
                        ctxt,
                        XmlParserErrors::XmlErrSeparatorRequired,
                        format!(
                            "xmlParseElementChildrenContentDecl : '{}' expected\n",
                            typ as char
                        )
                        .as_str(),
                        typ as i32
                    );
                    if !last.is_null() && last != ret {
                        xml_free_doc_element_content(ctxt.my_doc, last);
                    }
                    if !ret.is_null() {
                        xml_free_doc_element_content(ctxt.my_doc, ret);
                    }
                    return null_mut();
                }
                ctxt.skip_char();

                op = xml_new_doc_element_content(
                    ctxt.my_doc,
                    None,
                    XmlElementContentType::XmlElementContentSeq,
                );
                if op.is_null() {
                    if !last.is_null() && last != ret {
                        xml_free_doc_element_content(ctxt.my_doc, last);
                    }
                    xml_free_doc_element_content(ctxt.my_doc, ret);
                    return null_mut();
                }
                if last.is_null() {
                    (*op).c1 = ret;
                    if !ret.is_null() {
                        (*ret).parent = op;
                    }
                    ret = op;
                    cur = ret;
                } else {
                    (*cur).c2 = op;
                    if !op.is_null() {
                        (*op).parent = cur;
                    }
                    (*op).c1 = last;
                    if !last.is_null() {
                        (*last).parent = op;
                    }
                    cur = op;
                    // last = null_mut();
                }
            } else if ctxt.current_byte() == b'|' {
                if typ == 0 {
                    typ = ctxt.current_byte();
                } else if typ != ctxt.current_byte() {
                    // Detect "Name , Name | Name" error
                    xml_fatal_err_msg_int!(
                        ctxt,
                        XmlParserErrors::XmlErrSeparatorRequired,
                        format!(
                            "xmlParseElementChildrenContentDecl : '{}' expected\n",
                            typ as char
                        )
                        .as_str(),
                        typ as i32
                    );
                    if !last.is_null() && last != ret {
                        xml_free_doc_element_content(ctxt.my_doc, last);
                    }
                    if !ret.is_null() {
                        xml_free_doc_element_content(ctxt.my_doc, ret);
                    }
                    return null_mut();
                }
                ctxt.skip_char();

                op = xml_new_doc_element_content(
                    ctxt.my_doc,
                    None,
                    XmlElementContentType::XmlElementContentOr,
                );
                if op.is_null() {
                    if !last.is_null() && last != ret {
                        xml_free_doc_element_content(ctxt.my_doc, last);
                    }
                    if !ret.is_null() {
                        xml_free_doc_element_content(ctxt.my_doc, ret);
                    }
                    return null_mut();
                }
                if last.is_null() {
                    (*op).c1 = ret;
                    if !ret.is_null() {
                        (*ret).parent = op;
                    }
                    ret = op;
                    cur = ret;
                } else {
                    (*cur).c2 = op;
                    if !op.is_null() {
                        (*op).parent = cur;
                    }
                    (*op).c1 = last;
                    if !last.is_null() {
                        (*last).parent = op;
                    }
                    cur = op;
                    // last = null_mut();
                }
            } else {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrElemcontentNotFinished, None);
                if !last.is_null() && last != ret {
                    xml_free_doc_element_content(ctxt.my_doc, last);
                }
                if !ret.is_null() {
                    xml_free_doc_element_content(ctxt.my_doc, ret);
                }
                return null_mut();
            }
            ctxt.grow();
            ctxt.skip_blanks();
            ctxt.grow();
            if ctxt.current_byte() == b'(' {
                let inputid: i32 = ctxt.input().unwrap().id;
                // Recurse on second child
                ctxt.skip_char();
                ctxt.skip_blanks();
                last = parse_element_children_content_decl_priv(ctxt, inputid, depth + 1);
                if last.is_null() {
                    if !ret.is_null() {
                        xml_free_doc_element_content(ctxt.my_doc, ret);
                    }
                    return null_mut();
                }
                ctxt.skip_blanks();
            } else {
                let Some(elem) = parse_name(ctxt) else {
                    xml_fatal_err(ctxt, XmlParserErrors::XmlErrElemcontentNotStarted, None);
                    if !ret.is_null() {
                        xml_free_doc_element_content(ctxt.my_doc, ret);
                    }
                    return null_mut();
                };
                last = xml_new_doc_element_content(
                    ctxt.my_doc,
                    Some(&elem),
                    XmlElementContentType::XmlElementContentElement,
                );
                if last.is_null() {
                    if !ret.is_null() {
                        xml_free_doc_element_content(ctxt.my_doc, ret);
                    }
                    return null_mut();
                }
                match ctxt.current_byte() {
                    b'?' => {
                        (*last).ocur = XmlElementContentOccur::XmlElementContentOpt;
                        ctxt.skip_char();
                    }
                    b'*' => {
                        (*last).ocur = XmlElementContentOccur::XmlElementContentMult;
                        ctxt.skip_char();
                    }
                    b'+' => {
                        (*last).ocur = XmlElementContentOccur::XmlElementContentPlus;
                        ctxt.skip_char();
                    }
                    _ => (*last).ocur = XmlElementContentOccur::XmlElementContentOnce,
                }
            }
            ctxt.skip_blanks();
            ctxt.grow();
        }
        if !cur.is_null() && !last.is_null() {
            (*cur).c2 = last;
            if !last.is_null() {
                (*last).parent = cur;
            }
        }
        if ctxt.input().unwrap().id != inputchk {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrEntityBoundary,
                "Element content declaration doesn't start and stop in the same entity\n",
            );
        }
        ctxt.skip_char();
        if ctxt.current_byte() == b'?' {
            if !ret.is_null() {
                if matches!((*ret).ocur, XmlElementContentOccur::XmlElementContentPlus)
                    || matches!((*ret).ocur, XmlElementContentOccur::XmlElementContentMult)
                {
                    (*ret).ocur = XmlElementContentOccur::XmlElementContentMult;
                } else {
                    (*ret).ocur = XmlElementContentOccur::XmlElementContentOpt;
                }
            }
            ctxt.skip_char();
        } else if ctxt.current_byte() == b'*' {
            if !ret.is_null() {
                (*ret).ocur = XmlElementContentOccur::XmlElementContentMult;
                cur = ret;
                // Some normalization:
                // (a | b* | c?)* == (a | b | c)*
                while !cur.is_null()
                    && matches!((*cur).typ, XmlElementContentType::XmlElementContentOr)
                {
                    if !(*cur).c1.is_null()
                        && (matches!(
                            (*(*cur).c1).ocur,
                            XmlElementContentOccur::XmlElementContentOpt
                        ) || matches!(
                            (*(*cur).c1).ocur,
                            XmlElementContentOccur::XmlElementContentMult
                        ))
                    {
                        (*(*cur).c1).ocur = XmlElementContentOccur::XmlElementContentOnce;
                    }
                    if !(*cur).c2.is_null()
                        && (matches!(
                            (*(*cur).c2).ocur,
                            XmlElementContentOccur::XmlElementContentOpt
                        ) || matches!(
                            (*(*cur).c2).ocur,
                            XmlElementContentOccur::XmlElementContentMult
                        ))
                    {
                        (*(*cur).c2).ocur = XmlElementContentOccur::XmlElementContentOnce;
                    }
                    cur = (*cur).c2;
                }
            }
            ctxt.skip_char();
        } else if ctxt.current_byte() == b'+' {
            if !ret.is_null() {
                let mut found: i32 = 0;

                if matches!((*ret).ocur, XmlElementContentOccur::XmlElementContentOpt)
                    || matches!((*ret).ocur, XmlElementContentOccur::XmlElementContentMult)
                {
                    (*ret).ocur = XmlElementContentOccur::XmlElementContentMult;
                } else {
                    (*ret).ocur = XmlElementContentOccur::XmlElementContentPlus;
                }
                // Some normalization:
                // (a | b*)+ == (a | b)*
                // (a | b?)+ == (a | b)*
                while !cur.is_null()
                    && matches!((*cur).typ, XmlElementContentType::XmlElementContentOr)
                {
                    if !(*cur).c1.is_null()
                        && (matches!(
                            (*(*cur).c1).ocur,
                            XmlElementContentOccur::XmlElementContentOpt
                        ) || matches!(
                            (*(*cur).c1).ocur,
                            XmlElementContentOccur::XmlElementContentMult
                        ))
                    {
                        (*(*cur).c1).ocur = XmlElementContentOccur::XmlElementContentOnce;
                        found = 1;
                    }
                    if !(*cur).c2.is_null()
                        && (matches!(
                            (*(*cur).c2).ocur,
                            XmlElementContentOccur::XmlElementContentOpt
                        ) || matches!(
                            (*(*cur).c2).ocur,
                            XmlElementContentOccur::XmlElementContentMult
                        ))
                    {
                        (*(*cur).c2).ocur = XmlElementContentOccur::XmlElementContentOnce;
                        found = 1;
                    }
                    cur = (*cur).c2;
                }
                if found != 0 {
                    (*ret).ocur = XmlElementContentOccur::XmlElementContentMult;
                }
            }
            ctxt.skip_char();
        }
        ret
    }
}

/// Parse the declaration for a Mixed Element content
/// The leading '(' and spaces have been skipped in xmlParseElementContentDecl
///
/// ```text
/// [51] Mixed ::= '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*' | '(' S? '#PCDATA' S? ')'
///
/// [ VC: Proper Group/PE Nesting ] applies to [51] too (see [49])
///
/// [ VC: No Duplicate Types ]
/// The same name must not appear more than once in a single
/// mixed-content declaration.
/// ```
///
/// returns: the list of the xmlElementContentPtr describing the element choices
#[doc(alias = "xmlParseElementMixedContentDecl")]
unsafe fn parse_element_mixed_content_decl(
    ctxt: &mut XmlParserCtxt,
    inputchk: i32,
) -> XmlElementContentPtr {
    unsafe {
        let mut ret: XmlElementContentPtr = null_mut();
        let mut cur: XmlElementContentPtr = null_mut();
        let mut n: XmlElementContentPtr;

        ctxt.grow();
        if !ctxt.content_bytes().starts_with(b"#PCDATA") {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrPCDATARequired, None);
        }
        ctxt.advance(7);
        ctxt.skip_blanks();
        if ctxt.current_byte() == b')' {
            if ctxt.input().unwrap().id != inputchk {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrEntityBoundary,
                    "Element content declaration doesn't start and stop in the same entity\n",
                );
            }
            ctxt.skip_char();
            ret = xml_new_doc_element_content(
                ctxt.my_doc,
                None,
                XmlElementContentType::XmlElementContentPCDATA,
            );
            if ret.is_null() {
                return null_mut();
            }
            if ctxt.current_byte() == b'*' {
                (*ret).ocur = XmlElementContentOccur::XmlElementContentMult;
                ctxt.skip_char();
            }
            return ret;
        }
        if matches!(ctxt.current_byte(), b'(' | b'|') {
            ret = xml_new_doc_element_content(
                ctxt.my_doc,
                None,
                XmlElementContentType::XmlElementContentPCDATA,
            );
            cur = ret;
            if ret.is_null() {
                return null_mut();
            }
        }
        let mut elem: Option<String> = None;
        while ctxt.current_byte() == b'|'
            && !matches!(ctxt.instate, XmlParserInputState::XmlParserEOF)
        {
            ctxt.skip_char();
            if let Some(elem) = elem.as_deref() {
                n = xml_new_doc_element_content(
                    ctxt.my_doc,
                    None,
                    XmlElementContentType::XmlElementContentOr,
                );
                if n.is_null() {
                    xml_free_doc_element_content(ctxt.my_doc, ret);
                    return null_mut();
                }
                (*n).c1 = xml_new_doc_element_content(
                    ctxt.my_doc,
                    Some(elem),
                    XmlElementContentType::XmlElementContentElement,
                );
                if !(*n).c1.is_null() {
                    (*(*n).c1).parent = n;
                }
                (*cur).c2 = n;
                if !n.is_null() {
                    (*n).parent = cur;
                }
                cur = n;
            } else {
                ret = xml_new_doc_element_content(
                    ctxt.my_doc,
                    None,
                    XmlElementContentType::XmlElementContentOr,
                );
                if ret.is_null() {
                    xml_free_doc_element_content(ctxt.my_doc, cur);
                    return null_mut();
                }
                (*ret).c1 = cur;
                if !cur.is_null() {
                    (*cur).parent = ret;
                }
                cur = ret;
            }
            ctxt.skip_blanks();
            elem = parse_name(ctxt);
            if elem.is_none() {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrNameRequired,
                    "xmlParseElementMixedContentDecl : Name expected\n",
                );
                xml_free_doc_element_content(ctxt.my_doc, ret);
                return null_mut();
            }
            ctxt.skip_blanks();
            ctxt.grow();
        }
        if ctxt.content_bytes().starts_with(b")*") {
            if let Some(elem) = elem {
                (*cur).c2 = xml_new_doc_element_content(
                    ctxt.my_doc,
                    Some(&elem),
                    XmlElementContentType::XmlElementContentElement,
                );
                if !(*cur).c2.is_null() {
                    (*(*cur).c2).parent = cur;
                }
            }
            if !ret.is_null() {
                (*ret).ocur = XmlElementContentOccur::XmlElementContentMult;
            }
            if ctxt.input().unwrap().id != inputchk {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrEntityBoundary,
                    "Element content declaration doesn't start and stop in the same entity\n",
                );
            }
            ctxt.advance(2);
        } else {
            xml_free_doc_element_content(ctxt.my_doc, ret);
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrMixedNotStarted, None);
            return null_mut();
        }
        ret
    }
}

/// Parse an attribute list declaration for an element. Always consumes '<!'.
///
/// ```text
/// [52] AttlistDecl ::= '<!ATTLIST' S Name AttDef* S? '>'
/// [53] AttDef      ::= S Name S AttType S DefaultDecl
/// ```
#[doc(alias = "xmlParseAttributeListDecl")]
unsafe fn parse_attribute_list_decl(ctxt: &mut XmlParserCtxt) {
    unsafe {
        if !ctxt.content_bytes().starts_with(b"<!") {
            return;
        }
        ctxt.advance(2);

        if ctxt.content_bytes().starts_with(b"ATTLIST") {
            let inputid: i32 = ctxt.input().unwrap().id;

            ctxt.advance(7);
            if ctxt.skip_blanks() == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Space required after '<!ATTLIST'\n",
                );
            }
            let Some(elem_name) = parse_name(ctxt) else {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrNameRequired,
                    "ATTLIST: no name for Element\n",
                );
                return;
            };
            ctxt.skip_blanks();
            ctxt.grow();
            while ctxt.current_byte() != b'>'
                && !matches!(ctxt.instate, XmlParserInputState::XmlParserEOF)
            {
                ctxt.grow();
                let Some(attr_name) = parse_name(ctxt) else {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrNameRequired,
                        "ATTLIST: no name for Attribute\n",
                    );
                    break;
                };
                ctxt.grow();
                if ctxt.skip_blanks() == 0 {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrSpaceRequired,
                        "Space required after the attribute name\n",
                    );
                    break;
                }

                let mut tree = None;
                let Some(typ) = parse_attribute_type(ctxt, &mut tree) else {
                    break;
                };

                ctxt.grow();
                if ctxt.skip_blanks() == 0 {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrSpaceRequired,
                        "Space required after the attribute typ\n",
                    );
                    break;
                }

                let (def, mut default_value) = parse_default_decl(ctxt);
                if typ != XmlAttributeType::XmlAttributeCDATA {
                    if let Some(value) = default_value {
                        default_value = Some(attr_normalize_space(&value).into_owned());
                    }
                }

                ctxt.grow();
                if ctxt.current_byte() != b'>' && ctxt.skip_blanks() == 0 {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrSpaceRequired,
                        "Space required after the attribute default value\n",
                    );
                    break;
                }
                if let Some(attribute_decl) = ctxt
                    .sax
                    .as_deref_mut()
                    .filter(|_| ctxt.disable_sax == 0)
                    .and_then(|sax| sax.attribute_decl)
                {
                    attribute_decl(
                        ctxt.user_data.clone(),
                        &elem_name,
                        &attr_name,
                        typ,
                        def,
                        default_value.as_deref(),
                        tree,
                    );
                }

                if ctxt.sax2 != 0
                    && default_value.is_some()
                    && def != XmlAttributeDefault::XmlAttributeImplied
                    && def != XmlAttributeDefault::XmlAttributeRequired
                {
                    ctxt.add_def_attrs(&elem_name, &attr_name, default_value.as_deref().unwrap());
                }
                if ctxt.sax2 != 0 {
                    ctxt.add_special_attr(&elem_name, &attr_name, typ);
                }
                ctxt.grow();
            }
            if ctxt.current_byte() == b'>' {
                if inputid != ctxt.input().unwrap().id {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrEntityBoundary,
                        "Attribute list declaration doesn't start and stop in the same entity\n",
                    );
                }
                ctxt.skip_char();
            }
        }
    }
}

/// Parse the Attribute list def for an element
///
/// ```text
/// [54] AttType ::= StringType | TokenizedType | EnumeratedType
/// [55] StringType ::= 'CDATA'
/// [56] TokenizedType ::= 'ID' | 'IDREF' | 'IDREFS' | 'ENTITY' | 'ENTITIES' | 'NMTOKEN' | 'NMTOKENS'
///
/// [ VC: ID ]
/// Values of type ID must match the Name production. A name must not
/// appear more than once in an XML document as a value of this type;
/// i.e., ID values must uniquely identify the elements which bear them.
///
/// [ VC: One ID per Element Type ]
/// No element type may have more than one ID attribute specified.
///
/// [ VC: ID Attribute Default ]
/// An ID attribute must have a declared default of #IMPLIED or #REQUIRED.
///
/// [ VC: IDREF ]
/// Values of type IDREF must match the Name production, and values
/// of type IDREFS must match Names; each IDREF Name must match the value
/// of an ID attribute on some element in the XML document; i.e. IDREF
/// values must match the value of some ID attribute.
///
/// [ VC: Entity Name ]
/// Values of type ENTITY must match the Name production, values
/// of type ENTITIES must match Names; each Entity Name must match the
/// name of an unparsed entity declared in the DTD.
///
/// [ VC: Name Token ]
/// Values of type NMTOKEN must match the Nmtoken production; values
/// of type NMTOKENS must match Nmtokens.
/// ```
///
/// Validity constraints for attribute values syntax are checked in xmlValidateAttributeValue()
///
/// Returns the attribute type
#[doc(alias = "xmlParseAttributeType")]
unsafe fn parse_attribute_type(
    ctxt: &mut XmlParserCtxt,
    tree: &mut Option<Box<XmlEnumeration>>,
) -> Option<XmlAttributeType> {
    unsafe {
        if ctxt.content_bytes().starts_with(b"CDATA") {
            ctxt.advance(5);
            return Some(XmlAttributeType::XmlAttributeCDATA);
        } else if ctxt.content_bytes().starts_with(b"IDREFS") {
            ctxt.advance(6);
            return Some(XmlAttributeType::XmlAttributeIDREFS);
        } else if ctxt.content_bytes().starts_with(b"IDREF") {
            ctxt.advance(5);
            return Some(XmlAttributeType::XmlAttributeIDREF);
        } else if ctxt.content_bytes().starts_with(b"ID") {
            ctxt.advance(2);
            return Some(XmlAttributeType::XmlAttributeID);
        } else if ctxt.content_bytes().starts_with(b"ENTITY") {
            ctxt.advance(6);
            return Some(XmlAttributeType::XmlAttributeEntity);
        } else if ctxt.content_bytes().starts_with(b"ENTITIES") {
            ctxt.advance(8);
            return Some(XmlAttributeType::XmlAttributeEntities);
        } else if ctxt.content_bytes().starts_with(b"NMTOKENS") {
            ctxt.advance(8);
            return Some(XmlAttributeType::XmlAttributeNmtokens);
        } else if ctxt.content_bytes().starts_with(b"NMTOKEN") {
            ctxt.advance(7);
            return Some(XmlAttributeType::XmlAttributeNmtoken);
        }
        parse_enumerated_type(ctxt, tree)
    }
}

/// Parse an Enumerated attribute type.
///
/// ```text
/// [57] EnumeratedType ::= NotationType | Enumeration
/// [58] NotationType ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
/// ```
///
/// Returns: XML_ATTRIBUTE_ENUMERATION or XML_ATTRIBUTE_NOTATION
#[doc(alias = "xmlParseEnumeratedType")]
unsafe fn parse_enumerated_type(
    ctxt: &mut XmlParserCtxt,
    tree: &mut Option<Box<XmlEnumeration>>,
) -> Option<XmlAttributeType> {
    unsafe {
        if ctxt.content_bytes().starts_with(b"NOTATION") {
            ctxt.advance(8);
            if ctxt.skip_blanks() == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Space required after 'NOTATION'\n",
                );
                return None;
            }
            *tree = parse_notation_type(ctxt);
            if tree.is_none() {
                return None;
            }
            return Some(XmlAttributeType::XmlAttributeNotation);
        }
        *tree = parse_enumeration_type(ctxt);
        if tree.is_none() {
            return None;
        }
        Some(XmlAttributeType::XmlAttributeEnumeration)
    }
}

/// Parse an Notation attribute type.
///
/// # Note
/// The leading 'NOTATION' S part has already being parsed...
///
/// ```text
/// [58] NotationType ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
///
/// [ VC: Notation Attributes ]
/// Values of this type must match one of the notation names included
/// in the declaration; all notation names in the declaration must be declared.
/// ```
///
/// Returns: the notation attribute tree built while parsing
#[doc(alias = "xmlParseNotationType")]
unsafe fn parse_notation_type(ctxt: &mut XmlParserCtxt) -> Option<Box<XmlEnumeration>> {
    unsafe {
        let mut ret = None::<Box<XmlEnumeration>>;

        if ctxt.current_byte() != b'(' {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotationNotStarted, None);
            return None;
        }
        while {
            ctxt.skip_char();
            ctxt.skip_blanks();
            let Some(name) = parse_name(ctxt) else {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrNameRequired,
                    "Name expected in NOTATION declaration\n",
                );
                return None;
            };
            let mut tmp = ret.as_deref();
            while let Some(now) = tmp {
                if name == now.name {
                    xml_validity_error!(
                        ctxt,
                        XmlParserErrors::XmlDTDDupToken,
                        "standalone: attribute notation value token {} duplicated\n",
                        name
                    );
                    break;
                }
                tmp = now.next.as_deref();
            }
            if tmp.is_none() {
                let cur = xml_create_enumeration(&name);
                if let Some(mut ret) = ret.as_deref_mut() {
                    while ret.next.is_some() {
                        ret = ret.next.as_deref_mut().unwrap();
                    }
                    ret.next = Some(cur);
                } else {
                    ret = Some(cur);
                }
            }
            ctxt.skip_blanks();
            ctxt.current_byte() == b'|'
        } {}
        if ctxt.current_byte() != b')' {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotationNotFinished, None);
            return None;
        }
        ctxt.skip_char();
        ret
    }
}

/// Parse an Enumeration attribute type.
///
/// ```text
/// [59] Enumeration ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'
///
/// [ VC: Enumeration ]
/// Values of this type must match one of the Nmtoken tokens in the declaration
/// ```
///
/// Returns: the enumeration attribute tree built while parsing
#[doc(alias = "xmlParseEnumerationType")]
unsafe fn parse_enumeration_type(ctxt: &mut XmlParserCtxt) -> Option<Box<XmlEnumeration>> {
    unsafe {
        let mut ret = None::<Box<XmlEnumeration>>;

        if ctxt.current_byte() != b'(' {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrAttlistNotStarted, None);
            return None;
        }
        while {
            ctxt.skip_char();
            ctxt.skip_blanks();
            let Some(name) = parse_nmtoken(ctxt) else {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrNmtokenRequired, None);
                return ret;
            };
            let mut tmp = ret.as_deref();
            while let Some(now) = tmp {
                if name == now.name {
                    xml_validity_error!(
                        ctxt,
                        XmlParserErrors::XmlDTDDupToken,
                        "standalone: attribute enumeration value token {} duplicated\n",
                        name
                    );
                    break;
                }
                tmp = now.next.as_deref();
            }
            if tmp.is_none() {
                let cur = xml_create_enumeration(&name);
                if let Some(mut ret) = ret.as_deref_mut() {
                    while ret.next.is_some() {
                        ret = ret.next.as_deref_mut().unwrap();
                    }
                    ret.next = Some(cur);
                } else {
                    ret = Some(cur);
                }
            }
            ctxt.skip_blanks();
            ctxt.current_byte() == b'|'
        } {}
        if ctxt.current_byte() != b')' {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrAttlistNotFinished, None);
            return ret;
        }
        ctxt.skip_char();
        ret
    }
}

/// Parse an attribute default declaration
///
/// ```text
/// [60] DefaultDecl ::= '#REQUIRED' | '#IMPLIED' | (('#FIXED' S)? AttValue)
///
/// [ VC: Required Attribute ]
/// if the default declaration is the keyword #REQUIRED, then the
/// attribute must be specified for all elements of the type in the
/// attribute-list declaration.
///
/// [ VC: Attribute Default Legal ]
/// The declared default value must meet the lexical constraints of
/// the declared attribute type c.f. xmlValidateAttributeDecl()
///
/// [ VC: Fixed Attribute Default ]
/// if an attribute has a default value declared with the #FIXED
/// keyword, instances of that attribute must match the default value.
///
/// [ WFC: No < in Attribute Values ]
/// handled in xmlParseAttValue()
/// ```
///
///
/// returns: XML_ATTRIBUTE_NONE, XML_ATTRIBUTE_REQUIRED, XML_ATTRIBUTE_IMPLIED
///  or XML_ATTRIBUTE_FIXED.
#[doc(alias = "xmlParseDefaultDecl")]
unsafe fn parse_default_decl(ctxt: &mut XmlParserCtxt) -> (XmlAttributeDefault, Option<String>) {
    unsafe {
        if ctxt.content_bytes().starts_with(b"#REQUIRED") {
            ctxt.advance(9);
            return (XmlAttributeDefault::XmlAttributeRequired, None);
        }
        if ctxt.content_bytes().starts_with(b"#IMPLIED") {
            ctxt.advance(8);
            return (XmlAttributeDefault::XmlAttributeImplied, None);
        }
        let mut val = XmlAttributeDefault::XmlAttributeNone;
        if ctxt.content_bytes().starts_with(b"#FIXED") {
            ctxt.advance(6);
            val = XmlAttributeDefault::XmlAttributeFixed;
            if ctxt.skip_blanks() == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Space required after '#FIXED'\n",
                );
            }
        }
        let ret = parse_att_value(ctxt);
        ctxt.instate = XmlParserInputState::XmlParserDTD;
        if ret.is_none() {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::try_from(ctxt.err_no).unwrap(),
                "Attribute default value declaration error\n",
            );
        }
        (val, ret)
    }
}

/// Parse a conditional section. Always consumes '<!['.
///
/// ```text
/// [61] conditionalSect ::= includeSect | ignoreSect
/// [62] includeSect ::= '<![' S? 'INCLUDE' S? '[' extSubsetDecl ']]>'
/// [63] ignoreSect ::= '<![' S? 'IGNORE' S? '[' ignoreSectContents* ']]>'
/// [64] ignoreSectContents ::= Ignore ('<![' ignoreSectContents ']]>' Ignore)*
/// [65] Ignore ::= Char* - (Char* ('<![' | ']]>') Char*)
/// ```
#[doc(alias = "xmlParseConditionalSections")]
unsafe fn parse_conditional_sections(ctxt: &mut XmlParserCtxt) {
    unsafe {
        let mut depth = 0;
        let mut input_ids = vec![];

        while !matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            match ctxt.content_bytes() {
                [b'<', b'!', b'[', ..] => {
                    let id: i32 = ctxt.input().unwrap().id;

                    ctxt.advance(3);
                    ctxt.skip_blanks();

                    match ctxt.content_bytes() {
                        [b'I', b'N', b'C', b'L', b'U', b'D', b'E', ..] => {
                            ctxt.advance(7);
                            ctxt.skip_blanks();
                            if ctxt.current_byte() != b'[' {
                                xml_fatal_err(ctxt, XmlParserErrors::XmlErrCondsecInvalid, None);
                                ctxt.halt();
                                return;
                            }
                            if ctxt.input().unwrap().id != id {
                                xml_fatal_err_msg(
                                    ctxt,
                                    XmlParserErrors::XmlErrEntityBoundary,
                                    "All markup of the conditional section is not in the same entity\n",
                                );
                            }
                            ctxt.skip_char();

                            if input_ids.len() <= depth {
                                input_ids.resize(depth + 1, 0);
                            }
                            input_ids[depth] = id;
                            depth += 1;
                        }
                        [b'I', b'G', b'N', b'O', b'R', b'E', ..] => {
                            let mut ignore_depth = 0;

                            ctxt.advance(6);
                            ctxt.skip_blanks();
                            if ctxt.current_byte() != b'[' {
                                xml_fatal_err(ctxt, XmlParserErrors::XmlErrCondsecInvalid, None);
                                ctxt.halt();
                                return;
                            }
                            if ctxt.input().unwrap().id != id {
                                xml_fatal_err_msg(
                                    ctxt,
                                    XmlParserErrors::XmlErrEntityBoundary,
                                    "All markup of the conditional section is not in the same entity\n",
                                );
                            }
                            ctxt.skip_char();

                            while ctxt.current_byte() != 0 {
                                if ctxt.content_bytes().starts_with(b"<![") {
                                    ctxt.advance(3);
                                    ignore_depth += 1;
                                    // Check for integer overflow
                                    if ignore_depth == 0 {
                                        xml_err_memory(ctxt, None);
                                        return;
                                    }
                                } else if ctxt.content_bytes().starts_with(b"]]>") {
                                    if ignore_depth == 0 {
                                        break;
                                    }
                                    ctxt.advance(3);
                                    ignore_depth -= 1;
                                } else {
                                    ctxt.skip_char();
                                }
                            }

                            if ctxt.current_byte() == 0 {
                                xml_fatal_err(
                                    ctxt,
                                    XmlParserErrors::XmlErrCondsecNotFinished,
                                    None,
                                );
                                return;
                            }
                            if ctxt.input().unwrap().id != id {
                                xml_fatal_err_msg(
                                    ctxt,
                                    XmlParserErrors::XmlErrEntityBoundary,
                                    "All markup of the conditional section is not in the same entity\n",
                                );
                            }
                            ctxt.advance(3);
                        }
                        _ => {
                            xml_fatal_err(ctxt, XmlParserErrors::XmlErrCondsecInvalidKeyword, None);
                            ctxt.halt();
                            return;
                        }
                    }
                }
                [b']', b']', b'>', ..] if depth > 0 => {
                    depth -= 1;
                    if ctxt.input().unwrap().id != input_ids[depth] {
                        xml_fatal_err_msg(
                            ctxt,
                            XmlParserErrors::XmlErrEntityBoundary,
                            "All markup of the conditional section is not in the same entity\n",
                        );
                    }
                    ctxt.advance(3);
                }
                [b'<', b'!', ..] | [b'<', b'?', ..] => parse_markup_decl(ctxt),
                _ => {
                    xml_fatal_err(ctxt, XmlParserErrors::XmlErrExtSubsetNotFinished, None);
                    ctxt.halt();
                    return;
                }
            }

            if depth == 0 {
                break;
            }

            ctxt.skip_blanks();
            ctxt.shrink();
            ctxt.grow();
        }
    }
}

/// Parse a parameter entity reference. Always consumes '%'.
///
/// The entity content is handled directly by pushing it's content as a new input stream.
///
/// ```text
/// [69] PEReference ::= '%' Name ';'
///
/// [ WFC: No Recursion ]
/// A parsed entity must not contain a recursive
/// reference to itself, either directly or indirectly.
///
/// [ WFC: Entity Declared ]
/// In a document without any DTD, a document with only an internal DTD
/// subset which contains no parameter entity references, or a document
/// with "standalone='yes'", ...  ... The declaration of a parameter
/// entity must precede any reference to it...
///
/// [ VC: Entity Declared ]
/// In a document with an external subset or external parameter entities
/// with "standalone='no'", ...  ... The declaration of a parameter entity
/// must precede any reference to it...
///
/// [ WFC: In DTD ]
/// Parameter-entity references may only appear in the DTD.
/// NOTE: misleading but this is handled.
/// ```
#[doc(alias = "xmlParsePEReference")]
pub(crate) unsafe fn parse_pe_reference(ctxt: &mut XmlParserCtxt) {
    unsafe {
        if ctxt.current_byte() != b'%' {
            return;
        }
        ctxt.skip_char();
        let Some(name) = parse_name(ctxt) else {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrPERefNoName,
                "PEReference: no name\n",
            );
            return;
        };
        if get_parser_debug_entities() != 0 {
            generic_error!("PEReference: {}\n", name);
        }
        if ctxt.current_byte() != b';' {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrPERefSemicolMissing, None);
            return;
        }

        ctxt.skip_char();

        // Request the entity from SAX
        let entity = ctxt
            .sax
            .as_deref_mut()
            .and_then(|sax| sax.get_parameter_entity)
            .and_then(|get_parameter_entity| get_parameter_entity(ctxt.user_data.clone(), &name));
        if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            return;
        }
        if let Some(mut entity) = entity {
            // Internal checking in case the entity quest barfed
            if !matches!(
                entity.etype,
                XmlEntityType::XmlInternalParameterEntity
                    | XmlEntityType::XmlExternalParameterEntity
            ) {
                xml_warning_msg!(
                    ctxt,
                    XmlParserErrors::XmlWarUndeclaredEntity,
                    "Internal: %{}; is not a parameter entity\n",
                    name
                );
            } else {
                if matches!(entity.etype, XmlEntityType::XmlExternalParameterEntity)
                    && ctxt.options & XmlParserOption::XmlParseNoEnt as i32 == 0
                    && ctxt.options & XmlParserOption::XmlParseDTDValid as i32 == 0
                    && ctxt.options & XmlParserOption::XmlParseDTDLoad as i32 == 0
                    && ctxt.options & XmlParserOption::XmlParseDTDAttr as i32 == 0
                    && ctxt.replace_entities == 0
                    && ctxt.validate == 0
                {
                    return;
                }

                if entity.flags & XML_ENT_EXPANDING as i32 != 0 {
                    xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityLoop, None);
                    ctxt.halt();
                    return;
                }

                // Must be computed from old input before pushing new input.
                let mut parent_consumed = ctxt.input().unwrap().parent_consumed;
                let old_ent = ctxt.input().unwrap().entity;
                if old_ent.is_none_or(|old_ent| {
                    matches!(old_ent.etype, XmlEntityType::XmlExternalParameterEntity)
                        && old_ent.flags & XML_ENT_PARSED as i32 == 0
                }) {
                    parent_consumed =
                        parent_consumed.saturating_add(ctxt.input().unwrap().consumed);
                    parent_consumed = parent_consumed
                        .saturating_add(ctxt.input().unwrap().offset_from_base() as u64);
                }

                let Some(mut input) = XmlParserInput::from_entity(ctxt, entity) else {
                    return;
                };
                input.parent_consumed = parent_consumed;
                if ctxt.push_input(input) < 0 {
                    return;
                }

                entity.flags |= XML_ENT_EXPANDING as i32;

                if matches!(entity.etype, XmlEntityType::XmlExternalParameterEntity) {
                    // Get the 4 first bytes and decode the charset
                    // if enc != XML_CHAR_ENCODING_NONE
                    // plug some encoding conversion routines.
                    // Note that, since we may have some non-UTF8
                    // encoding (like UTF16, bug 135229), the 'length'
                    // is not known, but we can calculate based upon
                    // the amount of data in the buffer.
                    ctxt.grow();
                    if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
                        return;
                    }
                    if ctxt.input().unwrap().remainder_len() >= 4 {
                        let mut start: [u8; 4] = [0; 4];
                        start.copy_from_slice(&ctxt.content_bytes()[..4]);
                        let enc = detect_encoding(&start);
                        if !matches!(enc, XmlCharEncoding::None) {
                            ctxt.switch_encoding(enc);
                        }
                    }

                    if ctxt.content_bytes().starts_with(b"<?xml")
                        && xml_is_blank_char(ctxt.nth_byte(5) as u32)
                    {
                        parse_text_decl(ctxt);
                    }
                }
            }
        } else {
            // [ WFC: Entity Declared ]
            // In a document without any DTD, a document with only an
            // internal DTD subset which contains no parameter entity
            // references, or a document with "standalone='yes'", ...
            // ... The declaration of a parameter entity must precede
            // any reference to it...
            if ctxt.standalone == 1 || (ctxt.has_external_subset == 0 && ctxt.has_perefs == 0) {
                xml_fatal_err_msg_str!(
                    ctxt,
                    XmlParserErrors::XmlErrUndeclaredEntity,
                    "PEReference: %{}; not found\n",
                    name
                );
            } else {
                // [ VC: Entity Declared ]
                // In a document with an external subset or external
                // parameter entities with "standalone='no'", ...
                // ... The declaration of a parameter entity must
                // precede any reference to it...
                if ctxt.validate != 0 && ctxt.vctxt.error.is_some() {
                    xml_validity_error!(
                        ctxt,
                        XmlParserErrors::XmlWarUndeclaredEntity,
                        "PEReference: %{}; not found\n",
                        name
                    );
                } else {
                    xml_warning_msg!(
                        ctxt,
                        XmlParserErrors::XmlWarUndeclaredEntity,
                        "PEReference: %{}; not found\n",
                        name
                    );
                }
                ctxt.valid = 0;
            }
        }
        ctxt.has_perefs = 1;
    }
}

/// Parse an entity declaration. Always consumes '<!'.
///
/// ```text
/// [70] EntityDecl ::= GEDecl | PEDecl
/// [71] GEDecl ::= '<!ENTITY' S Name S EntityDef S? '>'
/// [72] PEDecl ::= '<!ENTITY' S '%' S Name S PEDef S? '>'
/// [73] EntityDef ::= EntityValue | (ExternalID NDataDecl?)
/// [74] PEDef ::= EntityValue | ExternalID
/// [76] NDataDecl ::= S 'NDATA' S Name
/// ```
///
/// `[ VC: Notation Declared ]`  
/// The Name must match the declared name of a notation.
#[doc(alias = "xmlParseEntityDecl")]
unsafe fn parse_entity_decl(ctxt: &mut XmlParserCtxt) {
    unsafe {
        let mut is_parameter: i32 = 0;

        if !ctxt.content_bytes().starts_with(b"<!") {
            return;
        }
        ctxt.advance(2);

        // GROW; done in the caller
        if ctxt.content_bytes().starts_with(b"ENTITY") {
            let inputid: i32 = ctxt.input().unwrap().id;
            ctxt.advance(6);
            if ctxt.skip_blanks() == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Space required after '<!ENTITY'\n",
                );
            }

            if ctxt.current_byte() == b'%' {
                ctxt.skip_char();
                if ctxt.skip_blanks() == 0 {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrSpaceRequired,
                        "Space required after '%'\n",
                    );
                }
                is_parameter = 1;
            }

            let Some(name) = parse_name(ctxt) else {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrNameRequired,
                    "xmlParseEntityDecl: no name\n",
                );
                return;
            };
            if name.contains(':') {
                xml_ns_err!(
                    ctxt,
                    XmlParserErrors::XmlNsErrColon,
                    "colons are forbidden from entities names '{}'\n",
                    name
                );
            }
            if ctxt.skip_blanks() == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Space required after the entity name\n",
                );
            }

            ctxt.instate = XmlParserInputState::XmlParserEntityDecl;

            // parsed entity value without reference substituted
            let mut orig = None;

            // handle the various case of definitions...
            if is_parameter != 0 {
                if ctxt.current_byte() == b'"' || ctxt.current_byte() == b'\'' {
                    let (value, original) = parse_entity_value(ctxt);
                    orig = original;
                    if let Some(value) = value {
                        if ctxt.disable_sax == 0 {
                            if let Some(entity_decl) =
                                ctxt.sax.as_deref_mut().and_then(|sax| sax.entity_decl)
                            {
                                entity_decl(
                                    ctxt.user_data.clone(),
                                    &name,
                                    XmlEntityType::XmlInternalParameterEntity,
                                    None,
                                    None,
                                    Some(&value),
                                );
                            }
                        }
                    }
                } else {
                    let (literal, uri) = parse_external_id(ctxt, true);
                    if uri.is_none() && literal.is_none() {
                        xml_fatal_err(ctxt, XmlParserErrors::XmlErrValueRequired, None);
                    }
                    if let Some(uri) = uri {
                        if let Some(parsed_uri) = XmlURI::parse(&uri) {
                            // This really ought to be a well formedness error
                            // but the XML Core WG decided otherwise c.f. issue
                            // E26 of the XML erratas.
                            if parsed_uri.fragment.is_some() {
                                // Okay this is foolish to block those but not invalid URIs.
                                xml_fatal_err(ctxt, XmlParserErrors::XmlErrURIFragment, None);
                            } else if ctxt.disable_sax == 0 {
                                if let Some(entity_decl) =
                                    ctxt.sax.as_deref_mut().and_then(|sax| sax.entity_decl)
                                {
                                    entity_decl(
                                        ctxt.user_data.clone(),
                                        &name,
                                        XmlEntityType::XmlExternalParameterEntity,
                                        literal.as_deref(),
                                        Some(&uri),
                                        None,
                                    );
                                }
                            }
                        } else {
                            xml_err_msg_str!(
                                ctxt,
                                XmlParserErrors::XmlErrInvalidURI,
                                "Invalid URI: {}\n",
                                uri
                            );
                        }
                    }
                }
            } else if ctxt.current_byte() == b'"' || ctxt.current_byte() == b'\'' {
                let (value, original) = parse_entity_value(ctxt);
                orig = original;
                if ctxt.disable_sax == 0 {
                    if let Some(entity_decl) =
                        ctxt.sax.as_deref_mut().and_then(|sax| sax.entity_decl)
                    {
                        entity_decl(
                            ctxt.user_data.clone(),
                            &name,
                            XmlEntityType::XmlInternalGeneralEntity,
                            None,
                            None,
                            value.as_deref(),
                        );
                    }
                }
                // For expat compatibility in SAX mode.
                if ctxt
                    .my_doc
                    .is_none_or(|doc| doc.version.as_deref() == Some(SAX_COMPAT_MODE))
                {
                    let mut my_doc = if let Some(my_doc) = ctxt.my_doc {
                        my_doc
                    } else {
                        ctxt.my_doc = xml_new_doc(Some(SAX_COMPAT_MODE));
                        let Some(mut my_doc) = ctxt.my_doc else {
                            xml_err_memory(ctxt, Some("New Doc failed"));
                            return;
                        };
                        my_doc.properties = XmlDocProperties::XmlDocInternal as i32;
                        my_doc
                    };
                    if my_doc.int_subset.is_none() {
                        my_doc.int_subset = xml_new_dtd(ctxt.my_doc, Some("fake"), None, None);
                    }

                    xml_sax2_entity_decl(
                        Some(GenericErrorContext::new(ctxt as *mut XmlParserCtxt)),
                        &name,
                        XmlEntityType::XmlInternalGeneralEntity,
                        None,
                        None,
                        value.as_deref(),
                    );
                }
            } else {
                let (literal, uri) = parse_external_id(ctxt, true);
                if uri.is_none() && literal.is_none() {
                    xml_fatal_err(ctxt, XmlParserErrors::XmlErrValueRequired, None);
                }
                if let Some(uri) = uri.as_deref() {
                    if let Some(parsed_uri) = XmlURI::parse(uri) {
                        // This really ought to be a well formedness error
                        // but the XML Core WG decided otherwise c.f. issue
                        // E26 of the XML erratas.
                        if parsed_uri.fragment.is_some() {
                            // Okay this is foolish to block those but not invalid URIs.
                            xml_fatal_err(ctxt, XmlParserErrors::XmlErrURIFragment, None);
                        }
                    } else {
                        xml_err_msg_str!(
                            ctxt,
                            XmlParserErrors::XmlErrInvalidURI,
                            "Invalid URI: {}\n",
                            uri
                        );
                    }
                }
                if ctxt.current_byte() != b'>' && ctxt.skip_blanks() == 0 {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrSpaceRequired,
                        "Space required before 'NDATA'\n",
                    );
                }
                if ctxt.content_bytes().starts_with(b"NDATA") {
                    ctxt.advance(5);
                    if ctxt.skip_blanks() == 0 {
                        xml_fatal_err_msg(
                            ctxt,
                            XmlParserErrors::XmlErrSpaceRequired,
                            "Space required after 'NDATA'\n",
                        );
                    }
                    let ndata = parse_name(ctxt);
                    if ctxt.disable_sax == 0 {
                        if let Some(unparsed_ent) = ctxt
                            .sax
                            .as_deref_mut()
                            .and_then(|sax| sax.unparsed_entity_decl)
                        {
                            unparsed_ent(
                                ctxt.user_data.clone(),
                                &name,
                                literal.as_deref(),
                                uri.as_deref(),
                                ndata.as_deref(),
                            );
                        }
                    }
                } else {
                    if ctxt.disable_sax == 0 {
                        if let Some(entity_decl) =
                            ctxt.sax.as_deref_mut().and_then(|sax| sax.entity_decl)
                        {
                            entity_decl(
                                ctxt.user_data.clone(),
                                &name,
                                XmlEntityType::XmlExternalGeneralParsedEntity,
                                literal.as_deref(),
                                uri.as_deref(),
                                None,
                            );
                        }
                    }
                    // For expat compatibility in SAX mode.
                    // assuming the entity replacement was asked for
                    if ctxt.replace_entities != 0
                        && ctxt
                            .my_doc
                            .is_none_or(|doc| doc.version.as_deref() == Some(SAX_COMPAT_MODE))
                    {
                        let mut my_doc = if let Some(my_doc) = ctxt.my_doc {
                            my_doc
                        } else {
                            ctxt.my_doc = xml_new_doc(Some(SAX_COMPAT_MODE));
                            let Some(mut my_doc) = ctxt.my_doc else {
                                xml_err_memory(ctxt, Some("New Doc failed"));
                                return;
                            };
                            my_doc.properties = XmlDocProperties::XmlDocInternal as i32;
                            my_doc
                        };

                        if my_doc.int_subset.is_none() {
                            my_doc.int_subset = xml_new_dtd(ctxt.my_doc, Some("fake"), None, None);
                        }
                        xml_sax2_entity_decl(
                            Some(GenericErrorContext::new(ctxt as *mut XmlParserCtxt)),
                            &name,
                            XmlEntityType::XmlExternalGeneralParsedEntity,
                            literal.as_deref(),
                            uri.as_deref(),
                            None,
                        );
                    }
                }
            }
            if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
                return;
            }
            ctxt.skip_blanks();
            if ctxt.current_byte() != b'>' {
                xml_fatal_err_msg_str!(
                    ctxt,
                    XmlParserErrors::XmlErrEntityNotFinished,
                    "xmlParseEntityDecl: entity {} not terminated\n",
                    name
                );
                ctxt.halt();
            } else {
                if inputid != ctxt.input().unwrap().id {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrEntityBoundary,
                        "Entity declaration doesn't start and stop in the same entity\n",
                    );
                }
                ctxt.skip_char();
            }
            if let Some(orig) = orig {
                // Ugly mechanism to save the raw entity value.
                let mut cur = None;

                if is_parameter != 0 {
                    if let Some(get_parameter_entity) = ctxt
                        .sax
                        .as_deref_mut()
                        .and_then(|sax| sax.get_parameter_entity)
                    {
                        cur = get_parameter_entity(ctxt.user_data.clone(), &name);
                    }
                } else {
                    if let Some(get_entity) = ctxt.sax.as_deref_mut().and_then(|sax| sax.get_entity)
                    {
                        cur = get_entity(ctxt.user_data.clone(), &name);
                    }
                    if cur.is_none()
                        && ctxt
                            .user_data
                            .as_ref()
                            .and_then(|d| d.lock().downcast_ref::<*mut XmlParserCtxt>().copied())
                            == Some(ctxt)
                    {
                        cur = xml_sax2_get_entity(
                            Some(GenericErrorContext::new(ctxt as *mut XmlParserCtxt)),
                            &name,
                        );
                    }
                }
                if let Some(mut cur) = cur.filter(|cur| cur.orig.is_none()) {
                    cur.orig = Some(Cow::Owned(orig));
                }
            }
        }
    }
}

/// Parse a notation declaration. Always consumes '<!'.
///
/// ```text
/// [82] NotationDecl ::= '<!NOTATION' S Name S (ExternalID |  PublicID) S? '>'
/// ```
///
/// Hence there is actually 3 choices:
/// - 'PUBLIC' S PubidLiteral
/// - 'PUBLIC' S PubidLiteral S SystemLiteral
/// - 'SYSTEM' S SystemLiteral
///
/// See the NOTE on xmlParseExternalID().
#[doc(alias = "xmlParseNotationDecl")]
unsafe fn parse_notation_decl(ctxt: &mut XmlParserCtxt) {
    unsafe {
        if !ctxt.content_bytes().starts_with(b"<!") {
            return;
        }
        ctxt.advance(2);

        if ctxt.content_bytes().starts_with(b"NOTATION") {
            let inputid: i32 = ctxt.input().unwrap().id;
            ctxt.advance(8);
            if ctxt.skip_blanks() == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Space required after '<!NOTATION'\n",
                );
                return;
            }

            let Some(name) = parse_name(ctxt) else {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotationNotStarted, None);
                return;
            };
            if name.contains(':') {
                xml_ns_err!(
                    ctxt,
                    XmlParserErrors::XmlNsErrColon,
                    "colons are forbidden from notation names '{}'\n",
                    name
                );
            }
            if ctxt.skip_blanks() == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Space required after the NOTATION name'\n",
                );
                return;
            }

            // Parse the IDs.
            let (pubid, systemid) = parse_external_id(ctxt, false);
            ctxt.skip_blanks();

            if ctxt.current_byte() == b'>' {
                if inputid != ctxt.input().unwrap().id {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrEntityBoundary,
                        "Notation declaration doesn't start and stop in the same entity\n",
                    );
                }
                ctxt.skip_char();
                if ctxt.disable_sax == 0 {
                    if let Some(notation_decl) =
                        ctxt.sax.as_deref_mut().and_then(|sax| sax.notation_decl)
                    {
                        notation_decl(
                            ctxt.user_data.clone(),
                            &name,
                            pubid.as_deref(),
                            systemid.as_deref(),
                        );
                    }
                }
            } else {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotationNotFinished, None);
            }
        }
    }
}
