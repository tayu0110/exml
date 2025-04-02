use crate::{
    error::XmlParserErrors,
    libxml::{
        chvalid::{xml_is_char, xml_is_pubid_char},
        parser_internals::{XML_MAX_NAME_LENGTH, XML_MAX_TEXT_LENGTH},
    },
    parser::{
        XmlParserCtxt, XmlParserInputState, XmlParserOption, xml_fatal_err, xml_fatal_err_msg,
    },
};

impl XmlParserCtxt {
    /// Parse an XML Literal
    ///
    /// ```text
    /// [11] SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")
    /// ```
    ///
    /// Returns the SystemLiteral parsed or NULL
    #[doc(alias = "xmlParseSystemLiteral")]
    unsafe fn parse_system_literal(&mut self) -> Option<String> {
        unsafe {
            let mut l = 0;
            let max_length = if self.options & XmlParserOption::XmlParseHuge as i32 != 0 {
                XML_MAX_TEXT_LENGTH
            } else {
                XML_MAX_NAME_LENGTH
            };
            let state = self.instate;

            if !matches!(self.current_byte(), b'"' | b'\'') {
                xml_fatal_err(self, XmlParserErrors::XmlErrLiteralNotStarted, None);
                return None;
            }
            let stop = self.current_byte();
            self.skip_char();

            let mut buf = String::new();
            self.instate = XmlParserInputState::XmlParserSystemLiteral;
            let mut cur = self.current_char(&mut l).unwrap_or('\0');
            while xml_is_char(cur as u32) && cur as i32 != stop as i32 {
                buf.push(cur);
                if buf.len() > max_length {
                    xml_fatal_err(
                        self,
                        XmlParserErrors::XmlErrNameTooLong,
                        Some("SystemLiteral"),
                    );
                    self.instate = state;
                    return None;
                }
                self.advance_with_line_handling(cur.len_utf8());
                cur = self.current_char(&mut l).unwrap_or('\0');
            }
            if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                return None;
            }
            self.instate = state;
            if !xml_is_char(cur as u32) {
                xml_fatal_err(self, XmlParserErrors::XmlErrLiteralNotFinished, None);
            } else {
                self.skip_char();
            }
            Some(buf)
        }
    }

    /// Parse an XML public literal
    ///
    /// ```text
    /// [12] PubidLiteral ::= '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
    /// ```
    ///
    /// Returns the PubidLiteral parsed or NULL.
    #[doc(alias = "xmlParsePubidLiteral")]
    unsafe fn parse_pubid_literal(&mut self) -> Option<String> {
        unsafe {
            let max_length = if self.options & XmlParserOption::XmlParseHuge as i32 != 0 {
                XML_MAX_TEXT_LENGTH
            } else {
                XML_MAX_NAME_LENGTH
            };

            let oldstate = self.instate;

            if !matches!(self.current_byte(), b'"' | b'\'') {
                xml_fatal_err(self, XmlParserErrors::XmlErrLiteralNotStarted, None);
                return None;
            }
            let stop = self.current_byte();
            self.skip_char();

            let mut buf = String::new();
            self.instate = XmlParserInputState::XmlParserPublicLiteral;
            let mut cur = self.current_byte();
            while xml_is_pubid_char(cur as u32) && cur != stop {
                // Since PubidChar is a subset of ASCII,
                // there is no problem with casting to `char`.
                buf.push(cur as char);
                if buf.len() > max_length {
                    xml_fatal_err(self, XmlParserErrors::XmlErrNameTooLong, Some("Public ID"));
                    return None;
                }
                self.skip_char();
                cur = self.current_byte();
            }
            if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                return None;
            }
            if cur != stop {
                xml_fatal_err(self, XmlParserErrors::XmlErrLiteralNotFinished, None);
            } else {
                self.advance(1);
            }
            self.instate = oldstate;
            Some(buf)
        }
    }

    /// Parse an External ID or a Public ID
    ///
    /// # Note
    /// Productions [75] and [83] interact badly since [75] can generate
    /// 'PUBLIC' S PubidLiteral S SystemLiteral
    ///
    /// ```text
    /// [75] ExternalID ::= 'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral
    /// [83] PublicID ::= 'PUBLIC' S PubidLiteral
    /// ```
    ///
    /// If ExternalID is parsed and PubidLiteral is found, return `(PubidLiteral, SystemLiteral)`,
    /// if ExternalID is parsed and PubidLiteral is not found, return `(None, SystemLiteral)`,
    /// if PublicID is parsed, return `(PubidLiteral, None)`,
    /// otherwise, return `(None, None)`.
    #[doc(alias = "xmlParseExternalID")]
    pub(crate) unsafe fn parse_external_id(
        &mut self,
        strict: bool,
    ) -> (Option<String>, Option<String>) {
        unsafe {
            let mut uri = None;
            let mut public_id = None;

            if self.content_bytes().starts_with(b"SYSTEM") {
                self.advance(6);
                if self.skip_blanks() == 0 {
                    xml_fatal_err_msg(
                        self,
                        XmlParserErrors::XmlErrSpaceRequired,
                        "Space required after b'SYSTEM'\n",
                    );
                }
                uri = self.parse_system_literal();
                if uri.is_none() {
                    xml_fatal_err(self, XmlParserErrors::XmlErrURIRequired, None);
                }
            } else if self.content_bytes().starts_with(b"PUBLIC") {
                self.advance(6);
                if self.skip_blanks() == 0 {
                    xml_fatal_err_msg(
                        self,
                        XmlParserErrors::XmlErrSpaceRequired,
                        "Space required after 'PUBLIC'\n",
                    );
                }
                public_id = self.parse_pubid_literal();
                if public_id.is_none() {
                    xml_fatal_err(self, XmlParserErrors::XmlErrPubidRequired, None);
                }
                if strict {
                    // We don't handle [83] so "S SystemLiteral" is required.
                    if self.skip_blanks() == 0 {
                        xml_fatal_err_msg(
                            self,
                            XmlParserErrors::XmlErrSpaceRequired,
                            "Space required after the Public Identifier\n",
                        );
                    }
                } else {
                    // We handle [83] so we return immediately, if
                    // "S SystemLiteral" is not detected. We skip blanks if no
                    // system literal was found, but this is harmless since we must
                    // be at the end of a NotationDecl.
                    if self.skip_blanks() == 0 {
                        return (public_id, None);
                    }
                    if self.current_byte() != b'\'' && self.current_byte() != b'"' {
                        return (public_id, None);
                    }
                }
                uri = self.parse_system_literal();
                if uri.is_none() {
                    xml_fatal_err(self, XmlParserErrors::XmlErrURIRequired, None);
                }
            }
            (public_id, uri)
        }
    }
}
