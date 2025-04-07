use std::{borrow::Cow, cell::RefCell, mem::take, rc::Rc};

use crate::{
    error::XmlParserErrors,
    libxml::{
        chvalid::{xml_is_blank_char, xml_is_char},
        valid::xml_validate_root,
    },
    parser::{
        XML_PARSER_MAX_DEPTH, XmlParserCtxt, XmlParserInputState, XmlParserNodeInfo,
        XmlParserOption, xml_err_attribute_dup, xml_fatal_err, xml_fatal_err_msg,
        xml_fatal_err_msg_int, xml_fatal_err_msg_str_int_str, xml_ns_err, xml_ns_warn,
        xml_validity_error,
    },
    uri::XmlURI,
};

impl XmlParserCtxt {
    /// parse an XML element
    ///
    /// ```text
    /// [39] element ::= EmptyElemTag | STag content ETag
    ///
    /// [ WFC: Element Type Match ]
    /// The Name in an element's end-tag must match the element type in the start-tag.
    /// ```
    #[doc(alias = "xmlParseElement")]
    pub(crate) unsafe fn parse_element(&mut self) {
        unsafe {
            if self.parse_element_start() != 0 {
                return;
            }

            self.parse_content_internal();
            if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                return;
            }

            if self.current_byte() == 0 {
                let name = self.name_tab[self.name_tab.len() - 1].clone();
                let line: i32 = self.push_tab[self.name_tab.len() - 1].line;
                xml_fatal_err_msg_str_int_str!(
                    self,
                    XmlParserErrors::XmlErrTagNotFinished,
                    "Premature end of data in tag {} line {}\n",
                    name,
                    line
                );
                return;
            }

            self.parse_element_end();
        }
    }

    /// Parse the start of an XML element. Returns -1 in case of error, 0 if an
    /// opening tag was parsed, 1 if an empty element was parsed.
    ///
    /// Always consumes '<'.
    #[doc(alias = "xmlParseElementStart")]
    pub(crate) unsafe fn parse_element_start(&mut self) -> i32 {
        unsafe {
            let ns_nr = self.ns_tab.len();

            if self.name_tab.len() as u32 > XML_PARSER_MAX_DEPTH
                && self.options & XmlParserOption::XmlParseHuge as i32 == 0
            {
                let max_depth = XML_PARSER_MAX_DEPTH as i32;
                xml_fatal_err_msg_int!(
                    self,
                    XmlParserErrors::XmlErrInternalError,
                    format!(
                        "Excessive depth in document: {} use xmlParserOption::XML_PARSE_HUGE option\n",
                        max_depth
                    )
                    .as_str(),
                    max_depth
                );
                self.halt();
                return -1;
            }

            // Capture start position
            let (begin_pos, begin_line) = if self.record_info != 0 {
                (
                    self.input().unwrap().consumed
                        + self.input().unwrap().offset_from_base() as u64,
                    self.input().unwrap().line as u64,
                )
            } else {
                (0, 0)
            };

            if self.space_tab.is_empty() || self.space() == -2 {
                self.space_push(-1);
            } else {
                self.space_push(self.space());
            }

            let line: i32 = self.input().unwrap().line;
            #[cfg(feature = "sax1")]
            let tag = if self.sax2 != 0 {
                self.parse_start_tag2()
            } else {
                self.parse_start_tag().map(|name| (name, None, None))
            };
            #[cfg(not(feature = "sax1"))]
            let tag = self.parse_start_tag2();

            if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                return -1;
            }
            let Some((name, prefix, uri)) = tag else {
                self.space_pop();
                return -1;
            };
            self.name_ns_push(
                &name,
                prefix.as_deref(),
                uri.as_deref(),
                line,
                self.ns_tab.len() as i32 - ns_nr as i32,
            );
            let cur = self.node;

            // [ VC: Root Element Type ]
            // The Name in the document type declaration must match the element type of the root element.
            #[cfg(feature = "libxml_valid")]
            if self.validate != 0 && self.well_formed != 0 {
                if let Some(context_node) = self.node {
                    if let Some(my_doc) = self
                        .my_doc
                        .filter(|doc| doc.children == Some(context_node.into()))
                    {
                        self.valid &= xml_validate_root(&raw mut self.vctxt, my_doc);
                    }
                }
            }

            // Check for an Empty Element.
            if self.content_bytes().starts_with(b"/>") {
                self.advance(2);
                if self.sax2 != 0 {
                    if self.disable_sax == 0 {
                        if let Some(end_element_ns) =
                            self.sax.as_deref_mut().and_then(|sax| sax.end_element_ns)
                        {
                            end_element_ns(self, &name, prefix.as_deref(), uri.as_deref());
                        }
                    }
                } else {
                    #[cfg(feature = "sax1")]
                    if self.disable_sax == 0 {
                        if let Some(end_element) =
                            self.sax.as_deref_mut().and_then(|sax| sax.end_element)
                        {
                            end_element(self, &name);
                        }
                    }
                }
                self.name_pop();
                self.space_pop();
                if ns_nr != self.ns_tab.len() {
                    self.ns_pop(self.ns_tab.len() - ns_nr);
                }
                if let Some(cur) = cur {
                    if self.record_info != 0 {
                        let node_info = XmlParserNodeInfo {
                            node: Some(cur),
                            begin_pos,
                            begin_line,
                            end_pos: self.input().unwrap().consumed
                                + self.input().unwrap().offset_from_base() as u64,
                            end_line: self.input().unwrap().line as u64,
                        };
                        self.add_node_info(Rc::new(RefCell::new(node_info)));
                    }
                }
                return 1;
            }
            if self.current_byte() == b'>' {
                self.advance(1);
                if let Some(cur) = cur {
                    if self.record_info != 0 {
                        let node_info = XmlParserNodeInfo {
                            node: Some(cur),
                            begin_pos,
                            begin_line,
                            end_pos: 0,
                            end_line: 0,
                        };
                        self.add_node_info(Rc::new(RefCell::new(node_info)));
                    }
                }
            } else {
                xml_fatal_err_msg_str_int_str!(
                    self,
                    XmlParserErrors::XmlErrGtRequired,
                    "Couldn't find end of Start Tag {} line {}\n",
                    name,
                    line
                );

                // end of parsing of this node.
                self.node_pop();
                self.name_pop();
                self.space_pop();
                if ns_nr != self.ns_tab.len() {
                    self.ns_pop(self.ns_tab.len() - ns_nr);
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
    pub(crate) unsafe fn parse_start_tag(&mut self) -> Option<String> {
        unsafe {
            use crate::parser::xml_err_attribute_dup;

            if self.current_byte() != b'<' {
                return None;
            }
            self.advance(1);

            let Some(name) = self.parse_name() else {
                xml_fatal_err_msg(
                    self,
                    XmlParserErrors::XmlErrNameRequired,
                    "xmlParseStartTag: invalid element name\n",
                );
                return None;
            };

            // Now parse the attributes, it ends up with the ending
            //
            // (S Attribute)* S?
            self.skip_blanks();
            self.grow();

            let mut atts: Vec<(String, Option<String>)> = vec![];
            while self.current_byte() != b'>'
                && !self.content_bytes().starts_with(b"/>")
                && xml_is_char(self.current_byte() as u32)
                && !matches!(self.instate, XmlParserInputState::XmlParserEOF)
            {
                let (Some(attname), attvalue) = self.parse_attribute() else {
                    xml_fatal_err_msg(
                        self,
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
                        xml_err_attribute_dup(self, None, &attname);
                    } else {
                        // Add the pair to atts
                        atts.push((attname, Some(attvalue)));
                    }
                }

                self.grow();
                if self.current_byte() == b'>' || self.content_bytes().starts_with(b"/>") {
                    break;
                }
                if self.skip_blanks() == 0 {
                    xml_fatal_err_msg(
                        self,
                        XmlParserErrors::XmlErrSpaceRequired,
                        "attributes construct error\n",
                    );
                }
                self.shrink();
                self.grow();
            }

            // SAX: Start of Element !
            if self.disable_sax == 0 {
                if let Some(start_element) =
                    self.sax.as_deref_mut().and_then(|sax| sax.start_element)
                {
                    if !atts.is_empty() {
                        start_element(self, &name, atts.as_slice());
                    } else {
                        start_element(self, &name, &[]);
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
        &mut self,
    ) -> Option<(String, Option<String>, Option<String>)> {
        unsafe {
            if self.current_byte() != b'<' {
                return None;
            }
            self.advance(1);

            let inputid: i32 = self.input().unwrap().id;
            let mut nbdef = 0usize;
            let mut nb_ns = 0usize;
            // Forget any namespaces added during an earlier parse of this element.
            // self.ns_tab.len() = ns_nr;

            let (prefix, localname) = self.parse_qname();
            let Some(localname) = localname else {
                xml_fatal_err_msg(
                    self,
                    XmlParserErrors::XmlErrNameRequired,
                    "StartTag: invalid element name\n",
                );
                return None;
            };

            // Now parse the attributes, it ends up with the ending
            //
            // (S Attribute)* S?
            self.skip_blanks();
            self.grow();

            let mut atts = vec![];

            while self.current_byte() != b'>'
                && (self.current_byte() != b'/' || self.nth_byte(1) != b'>')
                && xml_is_char(self.current_byte() as u32)
                && !matches!(self.instate, XmlParserInputState::XmlParserEOF)
            {
                let Some((aprefix, attname, attvalue)) =
                    self.parse_attribute2(prefix.as_deref(), &localname)
                else {
                    xml_fatal_err(
                        self,
                        XmlParserErrors::XmlErrInternalError,
                        Some("xmlParseStartTag: problem parsing attributes\n"),
                    );
                    break;
                };

                'next_attr: {
                    let Some(attvalue) = attvalue else {
                        break 'next_attr;
                    };

                    if Some(attname.as_str()) == self.str_xmlns.as_deref() && aprefix.is_none() {
                        let url = attvalue.clone();
                        if !url.is_empty() {
                            if let Some(uri) = XmlURI::parse(&url) {
                                if uri.scheme.is_none() {
                                    xml_ns_warn!(
                                        self,
                                        XmlParserErrors::XmlWarNsURIRelative,
                                        "xmlns: URI {} is not absolute\n",
                                        url
                                    );
                                }
                            } else {
                                xml_ns_err!(
                                    self,
                                    XmlParserErrors::XmlWarNsURI,
                                    "xmlns: '{}' is not a valid URI\n",
                                    url
                                );
                            }
                            if Some(url.as_str()) == self.str_xml_ns.as_deref() {
                                if Some(attname.as_str()) != self.str_xml.as_deref() {
                                    xml_ns_err!(
                                        self,
                                        XmlParserErrors::XmlNsErrXmlNamespace,
                                        "xml namespace URI cannot be the default namespace\n"
                                    );
                                }
                                break 'next_attr;
                            }
                            if url == "http://www.w3.org/2000/xmlns/" {
                                xml_ns_err!(
                                    self,
                                    XmlParserErrors::XmlNsErrXmlNamespace,
                                    "reuse of the xmlns namespace name is forbidden\n"
                                );
                                break 'next_attr;
                            }
                        }

                        // check that it's not a defined namespace
                        if self
                            .ns_tab
                            .iter()
                            .rev()
                            .take(nb_ns)
                            .any(|(pre, _)| pre.is_none())
                        {
                            xml_err_attribute_dup(self, None, &attname);
                        } else if self.ns_push(None, &url) > 0 {
                            nb_ns += 1;
                        }
                    } else if aprefix.as_deref() == self.str_xmlns.as_deref() {
                        let url = attvalue.clone();
                        if Some(attname.as_str()) == self.str_xml.as_deref() {
                            if Some(url.as_str()) != self.str_xml_ns.as_deref() {
                                xml_ns_err!(
                                    self,
                                    XmlParserErrors::XmlNsErrXmlNamespace,
                                    "xml namespace prefix mapped to wrong URI\n"
                                );
                            }
                            // Do not keep a namespace definition node
                            break 'next_attr;
                        }
                        if Some(url.as_str()) == self.str_xml_ns.as_deref() {
                            if Some(attname.as_str()) != self.str_xml.as_deref() {
                                xml_ns_err!(
                                    self,
                                    XmlParserErrors::XmlNsErrXmlNamespace,
                                    "xml namespace URI mapped to wrong prefix\n"
                                );
                            }
                            break 'next_attr;
                        }
                        if Some(attname.as_ref()) == self.str_xmlns.as_deref() {
                            xml_ns_err!(
                                self,
                                XmlParserErrors::XmlNsErrXmlNamespace,
                                "redefinition of the xmlns prefix is forbidden\n"
                            );
                            break 'next_attr;
                        }
                        if url == "http://www.w3.org/2000/xmlns/" {
                            xml_ns_err!(
                                self,
                                XmlParserErrors::XmlNsErrXmlNamespace,
                                "reuse of the xmlns namespace name is forbidden\n"
                            );
                            break 'next_attr;
                        }
                        if url.is_empty() {
                            xml_ns_err!(
                                self,
                                XmlParserErrors::XmlNsErrXmlNamespace,
                                "xmlns:{}: Empty XML namespace is not allowed\n",
                                attname
                            );
                            break 'next_attr;
                        }
                        if let Some(uri) = XmlURI::parse(&url) {
                            if self.pedantic != 0 && uri.scheme.is_none() {
                                xml_ns_warn!(
                                    self,
                                    XmlParserErrors::XmlWarNsURIRelative,
                                    "xmlns:{}: URI {} is not absolute\n",
                                    attname,
                                    url
                                );
                            }
                        } else {
                            xml_ns_err!(
                                self,
                                XmlParserErrors::XmlWarNsURI,
                                "xmlns:{}: '{}' is not a valid URI\n",
                                attname,
                                url
                            );
                        }
                        // check that it's not a defined namespace
                        if self
                            .ns_tab
                            .iter()
                            .take(nb_ns)
                            .any(|(pre, _)| pre.as_deref() == Some(&attname))
                        {
                            xml_err_attribute_dup(self, aprefix.as_deref(), &attname);
                        } else if self.ns_push(Some(&attname), &url) > 0 {
                            nb_ns += 1;
                        }
                    } else {
                        // Add the pair to atts
                        atts.push((attname, aprefix, None, attvalue));
                    }
                }

                //  next_attr:

                self.grow();
                if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                    break;
                }
                if self.current_byte() == b'>'
                    || (self.current_byte() == b'/' && self.nth_byte(1) == b'>')
                {
                    break;
                }
                if self.skip_blanks() == 0 {
                    xml_fatal_err_msg(
                        self,
                        XmlParserErrors::XmlErrSpaceRequired,
                        "attributes construct error\n",
                    );
                    break;
                }
                self.grow();
            }

            if self.input().unwrap().id != inputid {
                xml_fatal_err(
                    self,
                    XmlParserErrors::XmlErrInternalError,
                    Some("Unexpected change of input\n"),
                );
                return None;
            }

            // The attributes defaulting
            let atts_default = take(&mut self.atts_default);
            let defaults = atts_default.get(&(
                localname.as_str().into(),
                prefix.as_deref().map(Cow::Borrowed),
            ));
            if let Some(defaults) = defaults {
                for (attname, aprefix, def, external) in defaults {
                    // special work for namespaces defaulted defs
                    if Some(attname.as_str()) == self.str_xmlns.as_deref() && aprefix.is_none() {
                        // check that it's not a defined namespace
                        if self.ns_tab.iter().any(|(pre, _)| pre.is_none()) {
                            continue;
                        }

                        let nsname = self.get_namespace(None);
                        if nsname != Some(def) && self.ns_push(None, def) > 0 {
                            nb_ns += 1;
                        }
                    } else if aprefix.as_deref() == self.str_xmlns.as_deref() {
                        // check that it's not a defined namespace
                        if self
                            .ns_tab
                            .iter()
                            .any(|(pre, _)| pre.as_deref() == Some(attname))
                        {
                            continue;
                        }

                        let nsname = self.get_namespace(Some(attname));
                        if nsname != Some(def) && self.ns_push(Some(attname), def) > 0 {
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

                        let uri = aprefix.as_deref().and_then(|p| self.get_namespace(Some(p)));
                        atts.push((
                            attname.clone(),
                            aprefix.clone(),
                            uri.map(|uri| uri.to_owned()),
                            def.clone(),
                        ));
                        if self.standalone == 1 && external.is_some() {
                            xml_validity_error!(
                                self,
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
            self.atts_default = atts_default;

            // The attributes checkings
            for i in 0..atts.len() {
                // The default namespace does not apply to attribute names.
                let nsname = if atts[i].1.is_some() {
                    let nsname = self
                        .get_namespace(atts[i].1.as_deref())
                        .map(|uri| uri.to_owned());
                    if nsname.is_none() {
                        let pre = atts[i].1.as_deref().unwrap();
                        let loc = atts[i].0.as_str();
                        xml_ns_err!(
                            self,
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
                            xml_err_attribute_dup(self, pre, loc);
                            break;
                        }
                        if let Some(nsname) = nsname.filter(|&a| Some(a) == atts[j].2.as_deref()) {
                            let loc = atts[i].0.as_str();
                            xml_ns_err!(
                                self,
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

            let nsname = self.get_namespace(prefix.as_deref());
            let uri = nsname.map(|uri| uri.to_owned());
            if let Some(prefix) = prefix.as_deref() {
                if nsname.is_none() {
                    xml_ns_err!(
                        self,
                        XmlParserErrors::XmlNsErrUndefinedNamespace,
                        "Namespace prefix {} on {} is not defined\n",
                        prefix,
                        localname
                    );
                }
            }

            // SAX: Start of Element !
            if self.disable_sax == 0 {
                if let Some(start_element_ns) =
                    self.sax.as_deref_mut().and_then(|sax| sax.start_element_ns)
                {
                    let ns_tab = self.ns_tab[self.ns_tab.len() - nb_ns..].to_vec();
                    start_element_ns(
                        self,
                        &localname,
                        prefix.as_deref(),
                        uri.as_deref(),
                        &ns_tab,
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
    pub(crate) unsafe fn parse_element_end(&mut self) {
        unsafe {
            let cur = self.node;

            if self.name_tab.is_empty() {
                if self.content_bytes().starts_with(b"</") {
                    self.advance(2);
                }
                return;
            }

            // parse the end of tag: '</' should be here.
            if self.sax2 != 0 {
                self.parse_end_tag2();
                self.name_pop();
            } else {
                #[cfg(feature = "sax1")]
                {
                    self.parse_end_tag1(0);
                }
            }

            // Capture end position
            if let Some(cur) = cur {
                if self.record_info != 0 {
                    if let Some(node_info) = self.find_node_info(cur) {
                        node_info.borrow_mut().end_pos = self.input().unwrap().consumed
                            + self.input().unwrap().offset_from_base() as u64;
                        node_info.borrow_mut().end_line = self.input().unwrap().line as _;
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
    fn parse_name_and_compare(&mut self) -> Result<(), Option<String>> {
        self.grow();
        if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
            return Err(None);
        }

        let input = self.content_bytes();
        let count = input
            .iter()
            .copied()
            .zip(self.name.as_deref().unwrap().bytes())
            .take_while(|(i, o)| i == o)
            .count();
        if count == self.name.as_deref().unwrap().len()
            && input
                .get(count)
                .is_some_and(|&b| b == b'>' || xml_is_blank_char(b as u32))
        {
            // success
            self.advance(count);
            return Ok(());
        }
        // failure (or end of input buffer), check with full function
        let ret = self.parse_name();
        // strings coming from the dictionary direct compare possible
        if ret.as_deref() == self.name.as_deref() {
            return Ok(());
        }
        Err(ret)
    }

    /// Parse an XML name and compares for match
    /// (specialized for endtag parsing)
    ///
    /// Returns NULL for an illegal name, (XmlChar*) 1 for success
    /// and the name for mismatch
    #[doc(alias = "xmlParseQNameAndCompare")]
    fn parse_qname_and_compare(&mut self) -> Result<(), Option<String>> {
        self.grow();
        let tag_index = self.name_tab.len() - 1;
        let prefix = self.push_tab[tag_index].prefix.as_deref().unwrap();
        if self
            .content_bytes()
            .strip_prefix(prefix.as_bytes())
            .and_then(|input| input.strip_prefix(b":"))
            .and_then(|input| input.strip_prefix(self.name.as_deref().unwrap().as_bytes()))
            .and_then(|input| input.first())
            .is_some_and(|&b| b == b'>' || xml_is_blank_char(b as u32))
        {
            // success
            let len = prefix.len() + 1 + self.name.as_deref().unwrap().len();
            self.advance(len);
            return Ok(());
        }

        // all strings coms from the dictionary, equality can be done directly
        let (pre, ret) = self.parse_qname();
        let tag_index = self.name_tab.len() - 1;
        let prefix = self.push_tab[tag_index].prefix.as_deref().unwrap();
        if ret.as_deref() == self.name.as_deref() && pre.as_deref() == Some(prefix) {
            return Ok(());
        }
        Err(ret)
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
    pub(crate) unsafe fn parse_end_tag1(&mut self, line: i32) {
        unsafe {
            self.grow();
            if !self.content_bytes().starts_with(b"</") {
                xml_fatal_err_msg(
                    self,
                    XmlParserErrors::XmlErrLtSlashRequired,
                    "xmlParseEndTag: '</' not found\n",
                );
                return;
            }
            self.advance(2);

            let name = self.parse_name_and_compare();

            // We should definitely be at the ending "S? '>'" part
            self.grow();
            self.skip_blanks();
            if !xml_is_char(self.current_byte() as u32) || self.current_byte() != b'>' {
                xml_fatal_err(self, XmlParserErrors::XmlErrGtRequired, None);
            } else {
                self.advance(1);
            }

            // [ WFC: Element Type Match ]
            // The Name in an element's end-tag must match the element type in the start-tag.
            if let Err(name) = name {
                let name = name.as_deref().unwrap_or("unparsable");
                xml_fatal_err_msg_str_int_str!(
                    self,
                    XmlParserErrors::XmlErrTagNameMismatch,
                    "Opening and ending tag mismatch: {} line {} and {}\n",
                    self.name.as_deref().unwrap(),
                    line,
                    name
                );
            }

            // SAX: End of Tag
            if self.disable_sax == 0 {
                if let Some(end_element) = self.sax.as_deref_mut().and_then(|sax| sax.end_element) {
                    end_element(self, self.name.clone().as_deref().unwrap());
                }
            }

            self.name_pop();
            self.space_pop();
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
    pub(crate) unsafe fn parse_end_tag2(&mut self) {
        unsafe {
            self.grow();
            if !self.content_bytes().starts_with(b"</") {
                xml_fatal_err(self, XmlParserErrors::XmlErrLtSlashRequired, None);
                return;
            }
            self.advance(2);

            let tag_index = self.name_tab.len() - 1;
            let name = if self.push_tab[tag_index].prefix.is_some() {
                self.parse_qname_and_compare()
            } else {
                self.parse_name_and_compare()
            };

            // We should definitely be at the ending "S? '>'" part
            self.grow();
            if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                return;
            }
            self.skip_blanks();
            if !xml_is_char(self.current_byte() as u32) || self.current_byte() != b'>' {
                xml_fatal_err(self, XmlParserErrors::XmlErrGtRequired, None);
            } else {
                self.advance(1);
            }

            // [ WFC: Element Type Match ]
            // The Name in an element's end-tag must match the element type in the start-tag.
            if let Err(name) = name {
                let name = name.as_deref().unwrap_or("unparsable");
                xml_fatal_err_msg_str_int_str!(
                    self,
                    XmlParserErrors::XmlErrTagNameMismatch,
                    "Opening and ending tag mismatch: {} line {} and {}\n",
                    self.name.as_deref().unwrap(),
                    self.push_tab[tag_index].line,
                    name
                );
            }

            // SAX: End of Tag
            if self.disable_sax == 0 {
                if let Some(end_element_ns) =
                    self.sax.as_deref_mut().and_then(|sax| sax.end_element_ns)
                {
                    end_element_ns(
                        self,
                        self.name.clone().as_deref().unwrap(),
                        self.push_tab[tag_index].prefix.clone().as_deref(),
                        self.push_tab[tag_index].uri.clone().as_deref(),
                    );
                }
            }

            self.space_pop();
            if self.push_tab[tag_index].ns_nr != 0 {
                self.ns_pop(self.push_tab[tag_index].ns_nr as usize);
            }
        }
    }

    /// Parse a content sequence. Stops at EOF or '</'. Leaves checking of unexpected EOF to the caller.
    #[doc(alias = "xmlParseContentInternal")]
    pub(crate) unsafe fn parse_content_internal(&mut self) {
        unsafe {
            let name_nr = self.name_tab.len();

            self.grow();
            while !self.content_bytes().is_empty()
                && !matches!(self.instate, XmlParserInputState::XmlParserEOF)
            {
                match self.content_bytes() {
                    // First case : a Processing Instruction.
                    [b'<', b'?', ..] => self.parse_pi(),
                    // Second case : a CDSection
                    // 2.6.0 test was *cur not RAW
                    [b'<', b'!', b'[', b'C', b'D', b'A', b'T', b'A', b'[', ..] => {
                        self.parse_cdsect()
                    }
                    // Third case :  a comment
                    [b'<', b'!', b'-', b'-', ..] => {
                        self.parse_comment();
                        self.instate = XmlParserInputState::XmlParserContent;
                    }
                    // Fourth case :  a sub-element.
                    [b'<', b'/', ..] => {
                        if self.name_tab.len() <= name_nr {
                            break;
                        }
                        self.parse_element_end();
                    }
                    [b'<', ..] => {
                        self.parse_element_start();
                    }
                    // Fifth case : a reference. If if has not been resolved,
                    //    parsing returns it's Name, create the node
                    [b'&', ..] => self.parse_reference(),
                    // Last case, text. Note that References are handled directly.
                    _ => self.parse_char_data_internal(0),
                }

                self.shrink();
                self.grow();
            }
        }
    }

    /// Parse a content sequence. Stops at EOF or '</'.
    ///
    /// ```text
    /// [43] content ::= (element | CharData | Reference | CDSect | PI | Comment)*
    /// ```
    #[doc(alias = "xmlParseContent")]
    pub unsafe fn parse_content(&mut self) {
        unsafe {
            let name_nr = self.name_tab.len();

            self.parse_content_internal();

            if !matches!(self.instate, XmlParserInputState::XmlParserEOF)
                && self.name_tab.len() > name_nr
            {
                let name = self.name_tab[self.name_tab.len() - 1].clone();
                let line: i32 = self.push_tab[self.name_tab.len() - 1].line;
                xml_fatal_err_msg_str_int_str!(
                    self,
                    XmlParserErrors::XmlErrTagNotFinished,
                    "Premature end of data in tag {} line {}\n",
                    name,
                    line
                );
            }
        }
    }
}
