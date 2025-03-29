use std::{borrow::Cow, cell::RefCell, mem::take, rc::Rc};

use crate::{
    error::XmlParserErrors,
    libxml::{
        chvalid::{xml_is_blank_char, xml_is_char},
        parser::{XmlParserInputState, XmlParserOption},
        parser_internals::XML_PARSER_MAX_DEPTH,
        valid::xml_validate_root,
    },
    parser::{
        XmlParserCtxt, XmlParserNodeInfo, xml_err_attribute_dup, xml_fatal_err, xml_fatal_err_msg,
        xml_fatal_err_msg_int, xml_fatal_err_msg_str_int_str, xml_ns_err, xml_ns_warn,
        xml_validity_error,
    },
    uri::XmlURI,
};

use super::{parse_attribute2, parse_name, parse_qname};

/// Parse the start of an XML element. Returns -1 in case of error, 0 if an
/// opening tag was parsed, 1 if an empty element was parsed.
///
/// Always consumes '<'.
#[doc(alias = "xmlParseElementStart")]
pub(crate) unsafe fn parse_element_start(ctxt: &mut XmlParserCtxt) -> i32 {
    unsafe {
        let ns_nr = ctxt.ns_tab.len();

        if ctxt.name_tab.len() as u32 > XML_PARSER_MAX_DEPTH
            && ctxt.options & XmlParserOption::XmlParseHuge as i32 == 0
        {
            let max_depth = XML_PARSER_MAX_DEPTH as i32;
            xml_fatal_err_msg_int!(
                ctxt,
                XmlParserErrors::XmlErrInternalError,
                format!(
                    "Excessive depth in document: {} use xmlParserOption::XML_PARSE_HUGE option\n",
                    max_depth
                )
                .as_str(),
                max_depth
            );
            ctxt.halt();
            return -1;
        }

        // Capture start position
        let (begin_pos, begin_line) = if ctxt.record_info != 0 {
            (
                ctxt.input().unwrap().consumed + ctxt.input().unwrap().offset_from_base() as u64,
                ctxt.input().unwrap().line as u64,
            )
        } else {
            (0, 0)
        };

        if ctxt.space_tab.is_empty() || ctxt.space() == -2 {
            ctxt.space_push(-1);
        } else {
            ctxt.space_push(ctxt.space());
        }

        let line: i32 = ctxt.input().unwrap().line;
        #[cfg(feature = "sax1")]
        let tag = if ctxt.sax2 != 0 {
            parse_start_tag2(ctxt)
        } else {
            parse_start_tag(ctxt).map(|name| (name, None, None))
        };
        #[cfg(not(feature = "sax1"))]
        let tag = parse_start_tag2(ctxt);

        if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            return -1;
        }
        let Some((name, prefix, uri)) = tag else {
            ctxt.space_pop();
            return -1;
        };
        ctxt.name_ns_push(
            &name,
            prefix.as_deref(),
            uri.as_deref(),
            line,
            ctxt.ns_tab.len() as i32 - ns_nr as i32,
        );
        let cur = ctxt.node;

        // [ VC: Root Element Type ]
        // The Name in the document type declaration must match the element type of the root element.
        #[cfg(feature = "libxml_valid")]
        if ctxt.validate != 0 && ctxt.well_formed != 0 {
            if let Some(context_node) = ctxt.node {
                if let Some(my_doc) = ctxt
                    .my_doc
                    .filter(|doc| doc.children == Some(context_node.into()))
                {
                    ctxt.valid &= xml_validate_root(&raw mut ctxt.vctxt, my_doc);
                }
            }
        }

        // Check for an Empty Element.
        if ctxt.content_bytes().starts_with(b"/>") {
            ctxt.advance(2);
            if ctxt.sax2 != 0 {
                if ctxt.disable_sax == 0 {
                    if let Some(end_element_ns) =
                        ctxt.sax.as_deref_mut().and_then(|sax| sax.end_element_ns)
                    {
                        end_element_ns(
                            ctxt.user_data.clone(),
                            &name,
                            prefix.as_deref(),
                            uri.as_deref(),
                        );
                    }
                }
            } else {
                #[cfg(feature = "sax1")]
                if ctxt.disable_sax == 0 {
                    if let Some(end_element) =
                        ctxt.sax.as_deref_mut().and_then(|sax| sax.end_element)
                    {
                        end_element(ctxt.user_data.clone(), &name);
                    }
                }
            }
            ctxt.name_pop();
            ctxt.space_pop();
            if ns_nr != ctxt.ns_tab.len() {
                ctxt.ns_pop(ctxt.ns_tab.len() - ns_nr);
            }
            if let Some(cur) = cur {
                if ctxt.record_info != 0 {
                    let node_info = XmlParserNodeInfo {
                        node: Some(cur),
                        begin_pos,
                        begin_line,
                        end_pos: ctxt.input().unwrap().consumed
                            + ctxt.input().unwrap().offset_from_base() as u64,
                        end_line: ctxt.input().unwrap().line as u64,
                    };
                    ctxt.add_node_info(Rc::new(RefCell::new(node_info)));
                }
            }
            return 1;
        }
        if ctxt.current_byte() == b'>' {
            ctxt.advance(1);
            if let Some(cur) = cur {
                if ctxt.record_info != 0 {
                    let node_info = XmlParserNodeInfo {
                        node: Some(cur),
                        begin_pos,
                        begin_line,
                        end_pos: 0,
                        end_line: 0,
                    };
                    ctxt.add_node_info(Rc::new(RefCell::new(node_info)));
                }
            }
        } else {
            xml_fatal_err_msg_str_int_str!(
                ctxt,
                XmlParserErrors::XmlErrGtRequired,
                "Couldn't find end of Start Tag {} line {}\n",
                name,
                line
            );

            // end of parsing of this node.
            ctxt.node_pop();
            ctxt.name_pop();
            ctxt.space_pop();
            if ns_nr != ctxt.ns_tab.len() {
                ctxt.ns_pop(ctxt.ns_tab.len() - ns_nr);
            }
            return -1;
        }

        0
    }
}

/// Parse a start tag. Always consumes '<'.
///
/// ```text
/// [40] STag ::= '<' Name (S Attribute)* S? '>'
///
/// [ WFC: Unique Att Spec ]
/// No attribute name may appear more than once in the same start-tag or empty-element tag.
///
/// [44] EmptyElemTag ::= '<' Name (S Attribute)* S? '/>'
///
/// [ WFC: Unique Att Spec ]
/// No attribute name may appear more than once in the same start-tag or empty-element tag.
///
/// With namespace:
/// [NS 8] STag ::= '<' QName (S Attribute)* S? '>'
/// [NS 10] EmptyElement ::= '<' QName (S Attribute)* S? '/>'
/// ```
///
/// Returns the element name parsed
#[doc(alias = "xmlParseStartTag")]
#[cfg(feature = "sax1")]
pub(crate) unsafe fn parse_start_tag(ctxt: &mut XmlParserCtxt) -> Option<String> {
    use crate::parser::{parse_attribute, parse_name};

    unsafe {
        use crate::parser::xml_err_attribute_dup;

        if ctxt.current_byte() != b'<' {
            return None;
        }
        ctxt.advance(1);

        let Some(name) = parse_name(ctxt) else {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrNameRequired,
                "xmlParseStartTag: invalid element name\n",
            );
            return None;
        };

        // Now parse the attributes, it ends up with the ending
        //
        // (S Attribute)* S?
        ctxt.skip_blanks();
        ctxt.grow();

        let mut atts: Vec<(String, Option<String>)> = vec![];
        while ctxt.current_byte() != b'>'
            && !ctxt.content_bytes().starts_with(b"/>")
            && xml_is_char(ctxt.current_byte() as u32)
            && !matches!(ctxt.instate, XmlParserInputState::XmlParserEOF)
        {
            let (Some(attname), attvalue) = parse_attribute(ctxt) else {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    "xmlParseStartTag: problem parsing attributes\n",
                );
                break;
            };

            if let Some(attvalue) = attvalue {
                // [ WFC: Unique Att Spec ]
                // No attribute name may appear more than once in the same
                // start-tag or empty-element tag.
                if atts.iter().any(|(att, _)| att.as_str() == attname) {
                    xml_err_attribute_dup(ctxt, None, &attname);
                } else {
                    // Add the pair to atts
                    atts.push((attname, Some(attvalue)));
                }
            }

            ctxt.grow();
            if ctxt.current_byte() == b'>' || ctxt.content_bytes().starts_with(b"/>") {
                break;
            }
            if ctxt.skip_blanks() == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "attributes construct error\n",
                );
            }
            ctxt.shrink();
            ctxt.grow();
        }

        // SAX: Start of Element !
        if ctxt.disable_sax == 0 {
            if let Some(start_element) = ctxt.sax.as_deref_mut().and_then(|sax| sax.start_element) {
                if !atts.is_empty() {
                    start_element(ctxt.user_data.clone(), &name, atts.as_slice());
                } else {
                    start_element(ctxt.user_data.clone(), &name, &[]);
                }
            }
        }

        Some(name)
    }
}

/// Parse a start tag. Always consumes '<'.
///
/// This routine is called when running SAX2 parsing
///
/// ```text
/// [40] STag ::= '<' Name (S Attribute)* S? '>'
///
/// [ WFC: Unique Att Spec ]
/// No attribute name may appear more than once in the same start-tag or
/// empty-element tag.
///
/// [44] EmptyElemTag ::= '<' Name (S Attribute)* S? '/>'
///
/// [ WFC: Unique Att Spec ]
/// No attribute name may appear more than once in the same start-tag or empty-element tag.
///
/// With namespace:
/// [NS 8] STag ::= '<' QName (S Attribute)* S? '>'
/// [NS 10] EmptyElement ::= '<' QName (S Attribute)* S? '/>'
/// ```
///
/// Returns (Name, Prefix, URI)
#[doc(alias = "xmlParseStartTag2")]
pub(crate) unsafe fn parse_start_tag2(
    ctxt: &mut XmlParserCtxt,
) -> Option<(String, Option<String>, Option<String>)> {
    unsafe {
        if ctxt.current_byte() != b'<' {
            return None;
        }
        ctxt.advance(1);

        let inputid: i32 = ctxt.input().unwrap().id;
        let mut nbdef = 0usize;
        let mut nb_ns = 0usize;
        // Forget any namespaces added during an earlier parse of this element.
        // ctxt.ns_tab.len() = ns_nr;

        let (prefix, localname) = parse_qname(ctxt);
        let Some(localname) = localname else {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrNameRequired,
                "StartTag: invalid element name\n",
            );
            return None;
        };

        // Now parse the attributes, it ends up with the ending
        //
        // (S Attribute)* S?
        ctxt.skip_blanks();
        ctxt.grow();

        let mut atts = vec![];

        while ctxt.current_byte() != b'>'
            && (ctxt.current_byte() != b'/' || ctxt.nth_byte(1) != b'>')
            && xml_is_char(ctxt.current_byte() as u32)
            && !matches!(ctxt.instate, XmlParserInputState::XmlParserEOF)
        {
            let Some((aprefix, attname, attvalue)) =
                parse_attribute2(ctxt, prefix.as_deref(), &localname)
            else {
                xml_fatal_err(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    Some("xmlParseStartTag: problem parsing attributes\n"),
                );
                break;
            };

            'next_attr: {
                let Some(attvalue) = attvalue else {
                    break 'next_attr;
                };

                if Some(attname.as_str()) == ctxt.str_xmlns.as_deref() && aprefix.is_none() {
                    let url = attvalue.clone();
                    if !url.is_empty() {
                        if let Some(uri) = XmlURI::parse(&url) {
                            if uri.scheme.is_none() {
                                xml_ns_warn!(
                                    ctxt,
                                    XmlParserErrors::XmlWarNsURIRelative,
                                    "xmlns: URI {} is not absolute\n",
                                    url
                                );
                            }
                        } else {
                            xml_ns_err!(
                                ctxt,
                                XmlParserErrors::XmlWarNsURI,
                                "xmlns: '{}' is not a valid URI\n",
                                url
                            );
                        }
                        if Some(url.as_str()) == ctxt.str_xml_ns.as_deref() {
                            if Some(attname.as_str()) != ctxt.str_xml.as_deref() {
                                xml_ns_err!(
                                    ctxt,
                                    XmlParserErrors::XmlNsErrXmlNamespace,
                                    "xml namespace URI cannot be the default namespace\n"
                                );
                            }
                            break 'next_attr;
                        }
                        if url == "http://www.w3.org/2000/xmlns/" {
                            xml_ns_err!(
                                ctxt,
                                XmlParserErrors::XmlNsErrXmlNamespace,
                                "reuse of the xmlns namespace name is forbidden\n"
                            );
                            break 'next_attr;
                        }
                    }

                    // check that it's not a defined namespace
                    if ctxt
                        .ns_tab
                        .iter()
                        .rev()
                        .take(nb_ns)
                        .any(|(pre, _)| pre.is_none())
                    {
                        xml_err_attribute_dup(ctxt, None, &attname);
                    } else if ctxt.ns_push(None, &url) > 0 {
                        nb_ns += 1;
                    }
                } else if aprefix.as_deref() == ctxt.str_xmlns.as_deref() {
                    let url = attvalue.clone();
                    if Some(attname.as_str()) == ctxt.str_xml.as_deref() {
                        if Some(url.as_str()) != ctxt.str_xml_ns.as_deref() {
                            xml_ns_err!(
                                ctxt,
                                XmlParserErrors::XmlNsErrXmlNamespace,
                                "xml namespace prefix mapped to wrong URI\n"
                            );
                        }
                        // Do not keep a namespace definition node
                        break 'next_attr;
                    }
                    if Some(url.as_str()) == ctxt.str_xml_ns.as_deref() {
                        if Some(attname.as_str()) != ctxt.str_xml.as_deref() {
                            xml_ns_err!(
                                ctxt,
                                XmlParserErrors::XmlNsErrXmlNamespace,
                                "xml namespace URI mapped to wrong prefix\n"
                            );
                        }
                        break 'next_attr;
                    }
                    if Some(attname.as_ref()) == ctxt.str_xmlns.as_deref() {
                        xml_ns_err!(
                            ctxt,
                            XmlParserErrors::XmlNsErrXmlNamespace,
                            "redefinition of the xmlns prefix is forbidden\n"
                        );
                        break 'next_attr;
                    }
                    if url == "http://www.w3.org/2000/xmlns/" {
                        xml_ns_err!(
                            ctxt,
                            XmlParserErrors::XmlNsErrXmlNamespace,
                            "reuse of the xmlns namespace name is forbidden\n"
                        );
                        break 'next_attr;
                    }
                    if url.is_empty() {
                        xml_ns_err!(
                            ctxt,
                            XmlParserErrors::XmlNsErrXmlNamespace,
                            "xmlns:{}: Empty XML namespace is not allowed\n",
                            attname
                        );
                        break 'next_attr;
                    }
                    if let Some(uri) = XmlURI::parse(&url) {
                        if ctxt.pedantic != 0 && uri.scheme.is_none() {
                            xml_ns_warn!(
                                ctxt,
                                XmlParserErrors::XmlWarNsURIRelative,
                                "xmlns:{}: URI {} is not absolute\n",
                                attname,
                                url
                            );
                        }
                    } else {
                        xml_ns_err!(
                            ctxt,
                            XmlParserErrors::XmlWarNsURI,
                            "xmlns:{}: '{}' is not a valid URI\n",
                            attname,
                            url
                        );
                    }
                    // check that it's not a defined namespace
                    if ctxt
                        .ns_tab
                        .iter()
                        .take(nb_ns)
                        .any(|(pre, _)| pre.as_deref() == Some(&attname))
                    {
                        xml_err_attribute_dup(ctxt, aprefix.as_deref(), &attname);
                    } else if ctxt.ns_push(Some(&attname), &url) > 0 {
                        nb_ns += 1;
                    }
                } else {
                    // Add the pair to atts
                    atts.push((attname, aprefix, None, attvalue));
                }
            }

            //  next_attr:

            ctxt.grow();
            if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
                break;
            }
            if ctxt.current_byte() == b'>'
                || (ctxt.current_byte() == b'/' && ctxt.nth_byte(1) == b'>')
            {
                break;
            }
            if ctxt.skip_blanks() == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "attributes construct error\n",
                );
                break;
            }
            ctxt.grow();
        }

        if ctxt.input().unwrap().id != inputid {
            xml_fatal_err(
                ctxt,
                XmlParserErrors::XmlErrInternalError,
                Some("Unexpected change of input\n"),
            );
            return None;
        }

        // The attributes defaulting
        let atts_default = take(&mut ctxt.atts_default);
        let defaults = atts_default.get(&(
            localname.as_str().into(),
            prefix.as_deref().map(Cow::Borrowed),
        ));
        if let Some(defaults) = defaults {
            for (attname, aprefix, def, external) in defaults {
                // special work for namespaces defaulted defs
                if Some(attname.as_str()) == ctxt.str_xmlns.as_deref() && aprefix.is_none() {
                    // check that it's not a defined namespace
                    if ctxt.ns_tab.iter().any(|(pre, _)| pre.is_none()) {
                        continue;
                    }

                    let nsname = ctxt.get_namespace(None);
                    if nsname != Some(def) && ctxt.ns_push(None, def) > 0 {
                        nb_ns += 1;
                    }
                } else if aprefix.as_deref() == ctxt.str_xmlns.as_deref() {
                    // check that it's not a defined namespace
                    if ctxt
                        .ns_tab
                        .iter()
                        .any(|(pre, _)| pre.as_deref() == Some(attname))
                    {
                        continue;
                    }

                    let nsname = ctxt.get_namespace(Some(attname));
                    if nsname != Some(def) && ctxt.ns_push(Some(attname), def) > 0 {
                        nb_ns += 1;
                    }
                } else {
                    // check that it's not a defined attribute
                    if atts
                        .iter()
                        .any(|att| att.0 == *attname && att.1.as_deref() == aprefix.as_deref())
                    {
                        continue;
                    }

                    let uri = aprefix.as_deref().and_then(|p| ctxt.get_namespace(Some(p)));
                    atts.push((
                        attname.clone(),
                        aprefix.clone(),
                        uri.map(|uri| uri.to_owned()),
                        def.clone(),
                    ));
                    if ctxt.standalone == 1 && external.is_some() {
                        xml_validity_error!(
                            ctxt,
                            XmlParserErrors::XmlDTDStandaloneDefaulted,
                            "standalone: attribute {} on {} defaulted from external subset\n",
                            attname,
                            localname
                        );
                    }
                    nbdef += 1;
                }
            }
        }
        ctxt.atts_default = atts_default;

        // The attributes checkings
        for i in 0..atts.len() {
            // The default namespace does not apply to attribute names.
            let nsname = if atts[i].1.is_some() {
                let nsname = ctxt
                    .get_namespace(atts[i].1.as_deref())
                    .map(|uri| uri.to_owned());
                if nsname.is_none() {
                    let pre = atts[i].1.as_deref().unwrap();
                    let loc = atts[i].0.as_str();
                    xml_ns_err!(
                        ctxt,
                        XmlParserErrors::XmlNsErrUndefinedNamespace,
                        "Namespace prefix {} for {} on {} is not defined\n",
                        pre,
                        loc,
                        localname
                    );
                }
                atts[i].2 = nsname;
                atts[i].2.as_deref()
            } else {
                None
            };
            // [ WFC: Unique Att Spec ]
            // No attribute name may appear more than once in the same
            // start-tag or empty-element tag.
            // As extended by the Namespace in XML REC.
            for j in 0..i {
                if atts[i].0 == atts[j].0 {
                    if atts[i].1 == atts[j].1 {
                        let pre = atts[i].1.as_deref();
                        let loc = atts[i].0.as_str();
                        xml_err_attribute_dup(ctxt, pre, loc);
                        break;
                    }
                    if let Some(nsname) = nsname.filter(|&a| Some(a) == atts[j].2.as_deref()) {
                        let loc = atts[i].0.as_str();
                        xml_ns_err!(
                            ctxt,
                            XmlParserErrors::XmlNsErrAttributeRedefined,
                            "Namespaced Attribute {} in '{}' redefined\n",
                            loc,
                            nsname
                        );
                        break;
                    }
                }
            }
        }

        let nsname = ctxt.get_namespace(prefix.as_deref());
        let uri = nsname.map(|uri| uri.to_owned());
        if let Some(prefix) = prefix.as_deref() {
            if nsname.is_none() {
                xml_ns_err!(
                    ctxt,
                    XmlParserErrors::XmlNsErrUndefinedNamespace,
                    "Namespace prefix {} on {} is not defined\n",
                    prefix,
                    localname
                );
            }
        }

        // SAX: Start of Element !
        if ctxt.disable_sax == 0 {
            if let Some(start_element_ns) =
                ctxt.sax.as_deref_mut().and_then(|sax| sax.start_element_ns)
            {
                start_element_ns(
                    ctxt.user_data.clone(),
                    &localname,
                    prefix.as_deref(),
                    uri.as_deref(),
                    &ctxt.ns_tab[ctxt.ns_tab.len() - nb_ns..],
                    nbdef,
                    &atts,
                );
            }
        }

        Some((localname, prefix, uri))
    }
}

/// Parse the end of an XML element. Always consumes '</'.
#[doc(alias = "xmlParseElementEnd")]
pub(crate) unsafe fn parse_element_end(ctxt: &mut XmlParserCtxt) {
    unsafe {
        let cur = ctxt.node;

        if ctxt.name_tab.is_empty() {
            if ctxt.content_bytes().starts_with(b"</") {
                ctxt.advance(2);
            }
            return;
        }

        // parse the end of tag: '</' should be here.
        if ctxt.sax2 != 0 {
            parse_end_tag2(ctxt);
            ctxt.name_pop();
        } else {
            #[cfg(feature = "sax1")]
            {
                parse_end_tag1(ctxt, 0);
            }
        }

        // Capture end position
        if let Some(cur) = cur {
            if ctxt.record_info != 0 {
                if let Some(node_info) = ctxt.find_node_info(cur) {
                    node_info.borrow_mut().end_pos = ctxt.input().unwrap().consumed
                        + ctxt.input().unwrap().offset_from_base() as u64;
                    node_info.borrow_mut().end_line = ctxt.input().unwrap().line as _;
                }
            }
        }
    }
}

/// Parse an XML name and compares for match (specialized for endtag parsing)
///
/// Returns NULL for an illegal name, (XmlChar*) 1 for success
/// and the name for mismatch
#[doc(alias = "xmlParseNameAndCompare")]
unsafe fn parse_name_and_compare(ctxt: &mut XmlParserCtxt) -> Result<(), Option<String>> {
    unsafe {
        ctxt.grow();
        if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            return Err(None);
        }

        let input = ctxt.content_bytes();
        let count = input
            .iter()
            .copied()
            .zip(ctxt.name.as_deref().unwrap().bytes())
            .take_while(|(i, o)| i == o)
            .count();
        if count == ctxt.name.as_deref().unwrap().len()
            && input
                .get(count)
                .is_some_and(|&b| b == b'>' || xml_is_blank_char(b as u32))
        {
            // success
            ctxt.advance(count);
            return Ok(());
        }
        // failure (or end of input buffer), check with full function
        let ret = parse_name(ctxt);
        // strings coming from the dictionary direct compare possible
        if ret == ctxt.name {
            return Ok(());
        }
        Err(ret)
    }
}

/// Parse an XML name and compares for match
/// (specialized for endtag parsing)
///
/// Returns NULL for an illegal name, (XmlChar*) 1 for success
/// and the name for mismatch
#[doc(alias = "xmlParseQNameAndCompare")]
unsafe fn parse_qname_and_compare(ctxt: &mut XmlParserCtxt) -> Result<(), Option<String>> {
    unsafe {
        ctxt.grow();
        let tag_index = ctxt.name_tab.len() - 1;
        let prefix = ctxt.push_tab[tag_index].prefix.as_deref().unwrap();
        if ctxt
            .content_bytes()
            .strip_prefix(prefix.as_bytes())
            .and_then(|input| input.strip_prefix(b":"))
            .and_then(|input| input.strip_prefix(ctxt.name.as_deref().unwrap().as_bytes()))
            .and_then(|input| input.first())
            .is_some_and(|&b| b == b'>' || xml_is_blank_char(b as u32))
        {
            // success
            let len = prefix.len() + 1 + ctxt.name.as_deref().unwrap().len();
            ctxt.advance(len);
            return Ok(());
        }

        // all strings coms from the dictionary, equality can be done directly
        let (pre, ret) = parse_qname(ctxt);
        let tag_index = ctxt.name_tab.len() - 1;
        let prefix = ctxt.push_tab[tag_index].prefix.as_deref().unwrap();
        if ret == ctxt.name && pre.as_deref() == Some(prefix) {
            return Ok(());
        }
        Err(ret)
    }
}

/// Parse an end tag. Always consumes '</'.
///
/// ```text
/// [42] ETag ::= '</' Name S? '>'
///
/// With namespace
/// [NS 9] ETag ::= '</' QName S? '>'
/// ```
#[doc(alias = "xmlParseEndTag1")]
#[cfg(feature = "sax1")]
pub(crate) unsafe fn parse_end_tag1(ctxt: &mut XmlParserCtxt, line: i32) {
    unsafe {
        ctxt.grow();
        if !ctxt.content_bytes().starts_with(b"</") {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrLtSlashRequired,
                "xmlParseEndTag: '</' not found\n",
            );
            return;
        }
        ctxt.advance(2);

        let name = parse_name_and_compare(ctxt);

        // We should definitely be at the ending "S? '>'" part
        ctxt.grow();
        ctxt.skip_blanks();
        if !xml_is_char(ctxt.current_byte() as u32) || ctxt.current_byte() != b'>' {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrGtRequired, None);
        } else {
            ctxt.advance(1);
        }

        // [ WFC: Element Type Match ]
        // The Name in an element's end-tag must match the element type in the start-tag.
        if let Err(name) = name {
            let name = name.as_deref().unwrap_or("unparsable");
            xml_fatal_err_msg_str_int_str!(
                ctxt,
                XmlParserErrors::XmlErrTagNameMismatch,
                "Opening and ending tag mismatch: {} line {} and {}\n",
                ctxt.name.as_deref().unwrap(),
                line,
                name
            );
        }

        // SAX: End of Tag
        if ctxt.disable_sax == 0 {
            if let Some(end_element) = ctxt.sax.as_deref_mut().and_then(|sax| sax.end_element) {
                end_element(ctxt.user_data.clone(), ctxt.name.as_deref().unwrap());
            }
        }

        ctxt.name_pop();
        ctxt.space_pop();
    }
}

/// Parse an end tag. Always consumes '</'.
///
/// ```text
/// [42] ETag ::= '</' Name S? '>'
///
/// With namespace
/// [NS 9] ETag ::= '</' QName S? '>'
/// ```
#[doc(alias = "xmlParseEndTag2")]
pub(crate) unsafe fn parse_end_tag2(ctxt: &mut XmlParserCtxt) {
    unsafe {
        ctxt.grow();
        if !ctxt.content_bytes().starts_with(b"</") {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrLtSlashRequired, None);
            return;
        }
        ctxt.advance(2);

        let tag_index = ctxt.name_tab.len() - 1;
        let name = if ctxt.push_tab[tag_index].prefix.is_some() {
            parse_qname_and_compare(ctxt)
        } else {
            parse_name_and_compare(ctxt)
        };

        // We should definitely be at the ending "S? '>'" part
        ctxt.grow();
        if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            return;
        }
        ctxt.skip_blanks();
        if !xml_is_char(ctxt.current_byte() as u32) || ctxt.current_byte() != b'>' {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrGtRequired, None);
        } else {
            ctxt.advance(1);
        }

        // [ WFC: Element Type Match ]
        // The Name in an element's end-tag must match the element type in the start-tag.
        if let Err(name) = name {
            let name = name.as_deref().unwrap_or("unparsable");
            xml_fatal_err_msg_str_int_str!(
                ctxt,
                XmlParserErrors::XmlErrTagNameMismatch,
                "Opening and ending tag mismatch: {} line {} and {}\n",
                ctxt.name.as_deref().unwrap(),
                ctxt.push_tab[tag_index].line,
                name
            );
        }

        // SAX: End of Tag
        if ctxt.disable_sax == 0 {
            if let Some(end_element_ns) = ctxt.sax.as_deref_mut().and_then(|sax| sax.end_element_ns)
            {
                end_element_ns(
                    ctxt.user_data.clone(),
                    ctxt.name.as_deref().unwrap(),
                    ctxt.push_tab[tag_index].prefix.as_deref(),
                    ctxt.push_tab[tag_index].uri.as_deref(),
                );
            }
        }

        ctxt.space_pop();
        if ctxt.push_tab[tag_index].ns_nr != 0 {
            ctxt.ns_pop(ctxt.push_tab[tag_index].ns_nr as usize);
        }
    }
}
