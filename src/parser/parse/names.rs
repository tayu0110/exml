use crate::{
    error::XmlParserErrors,
    libxml::chvalid::XmlCharValid,
    parser::{
        XML_MAX_NAME_LENGTH, XML_MAX_TEXT_LENGTH, XmlParserCharValid, XmlParserCtxt,
        XmlParserInputState, XmlParserOption, build_qname, xml_fatal_err, xml_is_letter,
        xml_ns_err,
    },
};

impl XmlParserCtxt<'_> {
    /// Parse an XML Nmtoken.
    ///
    /// ```text
    /// [7] Nmtoken ::= (NameChar)+
    /// [8] Nmtokens ::= Nmtoken (#x20 Nmtoken)*
    /// ```
    ///
    /// Returns the Nmtoken parsed or NULL
    #[doc(alias = "xmlParseNmtoken")]
    pub(crate) fn parse_nmtoken(&mut self) -> Option<String> {
        let max_length = if self.options & XmlParserOption::XmlParseHuge as i32 != 0 {
            XML_MAX_TEXT_LENGTH
        } else {
            XML_MAX_NAME_LENGTH
        };
        let mut res = String::new();

        while let Some(c) = self.consume_char_if(|ctxt, c| c.is_name_char(ctxt)) {
            res.push(c);
            if res.len() > max_length {
                xml_fatal_err(self, XmlParserErrors::XmlErrNameTooLong, Some("NmToken"));
                return None;
            }
        }
        if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
            return None;
        }
        if res.is_empty() {
            return None;
        }
        Some(res)
    }

    #[doc(alias = "xmlParseNCNameComplex")]
    pub(crate) fn parse_ncname_complex(&mut self) -> Option<String> {
        let max_length = if self.options & XmlParserOption::XmlParseHuge as i32 != 0 {
            XML_MAX_TEXT_LENGTH
        } else {
            XML_MAX_NAME_LENGTH
        };

        // Handler for more complex cases
        let c = self.consume_char_if(|ctxt, c| {
            c != ' ' && c != '>' && c != '/' && c.is_name_start_char(ctxt) && c != ':'
        })?;
        let mut buf = String::with_capacity(c.len_utf8());
        buf.push(c);

        while let Some(c) = self.consume_char_if(|ctxt, c| {
            c != ' ' && c != '>' && c != '/' && c.is_name_char(ctxt) && c != ':'
        }) {
            buf.push(c);
            if buf.len() > max_length {
                xml_fatal_err(self, XmlParserErrors::XmlErrNameTooLong, Some("NCName"));
                return None;
            }
        }
        if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
            return None;
        }
        (!buf.is_empty()).then_some(buf)
    }

    /// Parse an XML name.
    ///
    /// ```text
    /// [4NS] NCNameChar ::= Letter | Digit | '.' | '-' | '_' | CombiningChar | Extender
    /// [5NS] NCName ::= (Letter | '_') (NCNameChar)*
    /// ```
    ///
    /// Returns the Name parsed or NULL
    #[doc(alias = "xmlParseNCName")]
    pub(crate) fn parse_ncname(&mut self) -> Option<String> {
        let max_length = if self.options & XmlParserOption::XmlParseHuge as i32 != 0 {
            XML_MAX_TEXT_LENGTH
        } else {
            XML_MAX_NAME_LENGTH
        };

        // Accelerator for simple ASCII names
        let content = self.content_bytes();
        if !content.is_empty() && (content[0].is_ascii_alphabetic() || content[0] == b'_') {
            for (i, &b) in content.iter().enumerate().skip(1) {
                if !b.is_ascii_alphanumeric() && b != b'_' && b != b'-' && b != b'.' {
                    if !(1..0x80).contains(&b) {
                        break;
                    }
                    if i > max_length {
                        xml_fatal_err(self, XmlParserErrors::XmlErrNameTooLong, Some("NCName"));
                        return None;
                    }
                    let res = unsafe {
                        // # Safety
                        // `content[..i]` contains only ASCII characters.
                        // Therefore, UTF-8 validation won't fail.
                        String::from_utf8_unchecked(content[..i].to_vec())
                    };
                    // `content[..i]` contains no line delimiters,
                    // so we need not use `self.advance_with_line_handling(i)`.
                    self.advance(i);
                    return Some(res);
                }
            }
        }
        self.parse_ncname_complex()
    }

    #[doc(alias = "xmlParseNameComplex")]
    fn parse_name_complex(&mut self, mut buf: String) -> Option<String> {
        let max_length = if self.options & XmlParserOption::XmlParseHuge as i32 != 0 {
            XML_MAX_TEXT_LENGTH
        } else {
            XML_MAX_NAME_LENGTH
        };

        // Handler for more complex cases
        if self.options & XmlParserOption::XmlParseOld10 as i32 == 0 {
            // Use the new checks of production [4] [4a] amd [5] of the
            // Update 5 of XML-1.0
            if buf.is_empty() {
                let c = self.consume_char_if(|_, c| {
                    c != ' '
                        && c != '>'
                        && c != '/'
                        && (c.is_ascii_alphabetic()
                            || c == '_'
                            || c == ':'
                            || ('\u{C0}'..='\u{D6}').contains(&c)
                            || ('\u{D8}'..='\u{F6}').contains(&c)
                            || ('\u{F8}'..='\u{2FF}').contains(&c)
                            || ('\u{370}'..='\u{37D}').contains(&c)
                            || ('\u{37F}'..='\u{1FFF}').contains(&c)
                            || ('\u{200C}'..='\u{200D}').contains(&c)
                            || ('\u{2070}'..='\u{218F}').contains(&c)
                            || ('\u{2C00}'..='\u{2FEF}').contains(&c)
                            || ('\u{3001}'..='\u{D7FF}').contains(&c)
                            || ('\u{F900}'..='\u{FDCF}').contains(&c)
                            || ('\u{FDF0}'..='\u{FFFD}').contains(&c)
                            || ('\u{10000}'..='\u{EFFFF}').contains(&c))
                })?;
                buf.push(c);
            }
            while let Some(c) = self.consume_char_if(|_, c| {
                c != ' '
                    && c != '>'
                    && c != '/'
                    && (c.is_ascii_alphanumeric()
                        || c == '_'
                        || c == ':'
                        || c == '-'
                        || c == '.'
                        || c == '\u{B7}'
                        || ('\u{C0}'..='\u{D6}').contains(&c)
                        || ('\u{D8}'..='\u{F6}').contains(&c)
                        || ('\u{F8}'..='\u{2FF}').contains(&c)
                        || ('\u{300}'..='\u{36F}').contains(&c)
                        || ('\u{370}'..='\u{37D}').contains(&c)
                        || ('\u{37F}'..='\u{1FFF}').contains(&c)
                        || ('\u{200C}'..='\u{200D}').contains(&c)
                        || ('\u{203F}'..='\u{2040}').contains(&c)
                        || ('\u{2070}'..='\u{218F}').contains(&c)
                        || ('\u{2C00}'..='\u{2FEF}').contains(&c)
                        || ('\u{3001}'..='\u{D7FF}').contains(&c)
                        || ('\u{F900}'..='\u{FDCF}').contains(&c)
                        || ('\u{FDF0}'..='\u{FFFD}').contains(&c)
                        || ('\u{10000}'..='\u{EFFFF}').contains(&c))
            }) {
                buf.push(c);
            }
        } else {
            if buf.is_empty() {
                let c = self.consume_char_if(|_, c| {
                    c != ' '
                        && c != '>'
                        && c != '/'
                        && (xml_is_letter(c as u32) || c == '_' || c == ':')
                })?;
                buf.push(c);
            }

            while let Some(c) = self.consume_char_if(|_, c| {
                c != ' '
                    && c != '>'
                    && c != '/'
                    && (xml_is_letter(c as u32)
                        || c.is_xml_digit()
                        || c == '.'
                        || c == '-'
                        || c == '_'
                        || c == ':'
                        || c.is_xml_combining()
                        || c.is_xml_extender())
            }) {
                buf.push(c);
            }
        }
        if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
            return None;
        }
        if buf.len() > max_length {
            xml_fatal_err(self, XmlParserErrors::XmlErrNameTooLong, Some("Name"));
            return None;
        }
        if self.input().unwrap().offset_from_base() < buf.len() {
            // There were a couple of bugs where PERefs lead to to a change
            // of the buffer. Check the buffer size to avoid passing an invalid
            // pointer to xmlDictLookup.
            xml_fatal_err(
                self,
                XmlParserErrors::XmlErrInternalError,
                Some("unexpected change of input buffer"),
            );
            return None;
        }
        Some(buf)
    }

    /// Parse an XML name.
    ///
    /// ```text
    /// [4] NameChar ::= Letter | Digit | '.' | '-' | '_' | ':' | CombiningChar | Extender
    /// [5] Name ::= (Letter | '_' | ':') (NameChar)*
    /// [6] Names ::= Name (#x20 Name)*
    /// ```
    ///
    /// Returns the Name parsed or NULL
    #[doc(alias = "xmlParseName")]
    pub(crate) fn parse_name(&mut self) -> Option<String> {
        let max_length = if self.options & XmlParserOption::XmlParseHuge as i32 != 0 {
            XML_MAX_TEXT_LENGTH
        } else {
            XML_MAX_NAME_LENGTH
        };

        self.grow();
        if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
            return None;
        }

        let mut buf = String::new();
        // Accelerator for simple ASCII names
        let content = self.content_bytes();
        if !content.is_empty()
            && (content[0].is_ascii_alphabetic() || content[0] == b'_' || content[0] == b':')
        {
            buf.push(content[0] as char);
            self.advance(1);
            let mut cur = self.content_bytes();
            let mut content = cur;
            while !cur.is_empty() {
                let b = cur[0];
                if !b.is_ascii_alphanumeric() && b != b'_' && b != b'-' && b != b':' && b != b'.' {
                    let diff = content.len() - cur.len();
                    // `content[..diff]` contains no line delimiters,
                    // so we need not use `self.advance_with_line_handling(i)`.
                    self.advance(diff);
                    if !(1..0x80).contains(&b) {
                        break;
                    }
                    return Some(buf);
                }
                buf.push(b as char);
                cur = &cur[1..];
                if buf.len() > max_length {
                    self.advance(content.len() - cur.len());
                    xml_fatal_err(self, XmlParserErrors::XmlErrNameTooLong, Some("Name"));
                    return None;
                }
                if cur.is_empty() {
                    let len = content.len();
                    self.advance(len);
                    self.grow();
                    content = self.content_bytes();
                    cur = content;
                    if cur.is_empty() {
                        return Some(buf);
                    }
                }
            }
        }
        // accelerator for special cases
        self.parse_name_complex(buf)
    }

    /// Parse an XML name.
    ///
    /// ```text
    /// [4] NameChar ::= Letter | Digit | '.' | '-' | '_' | ':' | CombiningChar | Extender
    /// [5] Name ::= (Letter | '_' | ':') (NameChar)*
    /// [6] Names ::= Name (#x20 Name)*
    /// ```
    ///
    /// Returns the Name parsed or NULL. The @str pointer is updated to the current location in the string.
    #[doc(alias = "xmlParseStringName")]
    pub(crate) fn parse_string_name<'a>(&mut self, s: &'a str) -> (Option<&'a str>, &'a str) {
        let max_length = if self.options & XmlParserOption::XmlParseHuge as i32 != 0 {
            XML_MAX_TEXT_LENGTH
        } else {
            XML_MAX_NAME_LENGTH
        };

        if s.starts_with(|c: char| !c.is_name_start_char(self)) {
            return (None, s);
        }
        let pos = s.find(|c: char| !c.is_name_char(self)).unwrap_or(s.len());

        if pos > max_length {
            xml_fatal_err(self, XmlParserErrors::XmlErrNameTooLong, Some("NCName"));
            return (None, s);
        }

        let (name, rem) = s.split_at(pos);
        (Some(name), rem)
    }

    /// Parse an XML Namespace QName
    ///
    /// ```text
    /// [6]  QName  ::= (Prefix ':')? LocalPart
    /// [7]  Prefix  ::= NCName
    /// [8]  LocalPart  ::= NCName
    /// ```
    ///
    /// Returns `(Prefix, LocalPart)`,
    #[doc(alias = "xmlParseQName")]
    pub(crate) fn parse_qname(&mut self) -> (Option<String>, Option<String>) {
        self.grow();
        if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
            return (None, None);
        }

        let Some(l) = self.parse_ncname() else {
            if self.current_byte() == b':' {
                if let Some(l) = self.parse_name() {
                    xml_ns_err!(
                        self,
                        XmlParserErrors::XmlNsErrQname,
                        "Failed to parse QName '{}'\n",
                        l
                    );
                    return (None, Some(l));
                }
            }
            return (None, None);
        };
        if self.current_byte() == b':' {
            self.skip_char();
            let p = l;
            let Some(l) = self.parse_ncname() else {
                if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                    return (None, None);
                }
                xml_ns_err!(
                    self,
                    XmlParserErrors::XmlNsErrQname,
                    "Failed to parse QName '{}:'\n",
                    p
                );
                let l = self.parse_nmtoken();
                let p = if let Some(l) = l.as_deref() {
                    build_qname(l, Some(&p))
                } else {
                    if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                        return (None, None);
                    }
                    build_qname("", Some(&p))
                };
                return (None, Some(p.into_owned()));
            };
            if self.current_byte() == b':' {
                xml_ns_err!(
                    self,
                    XmlParserErrors::XmlNsErrQname,
                    "Failed to parse QName '{}:{}:'\n",
                    p,
                    l
                );
                self.skip_char();
                if let Some(tmp) = self.parse_name() {
                    let l = build_qname(&tmp, Some(&l));
                    return (Some(p), Some(l.into_owned()));
                }
                if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                    return (None, None);
                }
                let l = build_qname("", Some(&l));
                return (Some(p), Some(l.into_owned()));
            }
            (Some(p), Some(l))
        } else {
            (None, Some(l))
        }
    }
}
