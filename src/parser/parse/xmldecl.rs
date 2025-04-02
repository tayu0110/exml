use crate::{
    encoding::find_encoding_handler,
    error::XmlParserErrors,
    libxml::{
        chvalid::xml_is_blank_char,
        parser_internals::{XML_MAX_NAME_LENGTH, XML_MAX_TEXT_LENGTH},
    },
    parser::{
        XML_DEFAULT_VERSION, XmlParserCtxt, XmlParserInputState, XmlParserOption, xml_fatal_err,
        xml_fatal_err_msg, xml_fatal_err_msg_str, xml_warning_msg,
    },
};

impl XmlParserCtxt {
    /// Parse the XML version.
    ///
    /// ```text
    /// [24] VersionInfo ::= S 'version' Eq (' VersionNum ' | " VersionNum ")
    /// [25] Eq ::= S? '=' S?
    /// ```
    ///
    /// Returns the version string, e.g. "1.0"
    #[doc(alias = "xmlParseVersionInfo")]
    unsafe fn parse_version_info(&mut self) -> Option<String> {
        unsafe {
            if !self.content_bytes().starts_with(b"version") {
                return None;
            }
            self.advance(7);
            self.skip_blanks();
            if self.consume_char_if(|_, c| c == '=').is_none() {
                xml_fatal_err(self, XmlParserErrors::XmlErrEqualRequired, None);
                return None;
            }
            self.skip_blanks();
            let Some(quoto) = self.consume_char_if(|_, c| c == '"' || c == '\'') else {
                xml_fatal_err(self, XmlParserErrors::XmlErrStringNotStarted, None);
                return None;
            };
            let version = self.parse_version_num();
            if self.consume_char_if(|_, c| c == quoto).is_none() {
                xml_fatal_err(self, XmlParserErrors::XmlErrStringNotClosed, None);
                return None;
            }
            version
        }
    }

    /// Parse the XML version value.
    ///
    /// ```text
    /// [26] VersionNum ::= '1.' [0-9]+
    /// ```
    ///
    /// In practice allow [0-9].[0-9]+ at that level
    ///
    /// Returns the string giving the XML version number, or NULL
    #[doc(alias = "xmlParseVersionNum")]
    unsafe fn parse_version_num(&mut self) -> Option<String> {
        unsafe {
            let mut buf = String::with_capacity(10);
            buf.push(self.consume_char_if(|_, c| c.is_ascii_digit())?);
            self.consume_char_if(|_, c| c == '.')?;
            buf.push('.');
            while let Some(c) = self.consume_char_if(|_, c| c.is_ascii_digit()) {
                buf.push(c);
            }
            Some(buf)
        }
    }

    /// Parse the XML encoding declaration
    ///
    /// ```text
    /// [80] EncodingDecl ::= S 'encoding' Eq ('"' EncName '"' |  "'" EncName "'")
    /// ```
    ///
    /// this setups the conversion filters.
    ///
    /// Returns the encoding value or NULL
    #[doc(alias = "xmlParseEncodingDecl")]
    unsafe fn parse_encoding_decl(&mut self) -> Option<String> {
        unsafe {
            self.skip_blanks();
            if !self.content_bytes().starts_with(b"encoding") {
                return None;
            }
            self.advance(8);
            self.skip_blanks();
            if self.consume_char_if(|_, c| c == '=').is_none() {
                xml_fatal_err(self, XmlParserErrors::XmlErrEqualRequired, None);
                return None;
            }
            self.skip_blanks();
            let encoding = if let Some(quote) = self.consume_char_if(|_, c| c == '"' || c == '\'') {
                let encoding = self.parse_enc_name();
                if self.consume_char_if(|_, c| c == quote).is_none() {
                    xml_fatal_err(self, XmlParserErrors::XmlErrStringNotClosed, None);
                    return None;
                }
                encoding
            } else {
                xml_fatal_err(self, XmlParserErrors::XmlErrStringNotStarted, None);
                None
            };

            // Non standard parsing, allowing the user to ignore encoding
            if self.options & XmlParserOption::XmlParseIgnoreEnc as i32 != 0 {
                return None;
            }

            // UTF-16 encoding match has already taken place at this stage,
            // more over the little-endian/big-endian selection is already done
            if let Some(encoding) = encoding
                .as_deref()
                .filter(|e| e.eq_ignore_ascii_case("UTF-16") || e.eq_ignore_ascii_case("UTF16"))
            {
                // If no encoding was passed to the parser, that we are
                // using UTF-16 and no decoder is present i.e. the
                // document is apparently UTF-8 compatible, then raise an
                // encoding mismatch fatal error
                if self.encoding.is_none()
                    && self.input().unwrap().buf.is_some()
                    && self
                        .input()
                        .unwrap()
                        .buf
                        .as_ref()
                        .unwrap()
                        .borrow()
                        .encoder
                        .is_none()
                {
                    xml_fatal_err_msg(
                        self,
                        XmlParserErrors::XmlErrInvalidEncoding,
                        "Document labelled UTF-16 but has UTF-8 content\n",
                    );
                }
                self.encoding = Some(encoding.to_owned());
            } else if let Some(encoding) = encoding.as_deref().filter(|e| {
                // UTF-8 encoding is handled natively
                e.eq_ignore_ascii_case("UTF-8") || e.eq_ignore_ascii_case("UTF8")
            }) {
                // TODO: Check for encoding mismatch.
                self.encoding = Some(encoding.to_owned());
            } else if let Some(encoding) = encoding.as_deref() {
                self.input_mut().unwrap().encoding = Some(encoding.to_owned());

                if let Some(handler) = find_encoding_handler(encoding) {
                    if self.switch_to_encoding(handler) < 0 {
                        // failed to convert
                        self.err_no = XmlParserErrors::XmlErrUnsupportedEncoding as i32;
                        return None;
                    }
                } else {
                    xml_fatal_err_msg_str!(
                        self,
                        XmlParserErrors::XmlErrUnsupportedEncoding,
                        "Unsupported encoding {}\n",
                        encoding
                    );
                    return None;
                }
            }
            encoding
        }
    }

    /// parse the XML encoding name
    ///
    /// ```text
    /// [81] EncName ::= [A-Za-z] ([A-Za-z0-9._] | '-')*
    /// ```
    ///
    /// Returns the encoding name value or NULL
    #[doc(alias = "xmlParseEncName")]
    unsafe fn parse_enc_name(&mut self) -> Option<String> {
        unsafe {
            let max_length = if self.options & XmlParserOption::XmlParseHuge as i32 != 0 {
                XML_MAX_TEXT_LENGTH
            } else {
                XML_MAX_NAME_LENGTH
            };
            let Some(first) = self.consume_char_if(|_, c| c.is_ascii_alphabetic()) else {
                xml_fatal_err(self, XmlParserErrors::XmlErrEncodingName, None);
                return None;
            };
            let mut buf = String::with_capacity(10);
            buf.push(first);
            while let Some(c) =
                self.consume_char_if(|_, c| c.is_alphanumeric() || c == '.' || c == '_' || c == '-')
            {
                buf.push(c);
                if buf.len() > max_length {
                    xml_fatal_err(self, XmlParserErrors::XmlErrNameTooLong, Some("EncName"));
                    return None;
                }
            }
            Some(buf)
        }
    }

    /// Parse the XML standalone declaration
    ///
    /// ```text
    /// [32] SDDecl ::= S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no')'"'))
    ///
    /// [ VC: Standalone Document Declaration ]
    /// TODO The standalone document declaration must have the value "no"
    /// if any external markup declarations contain declarations of:
    ///  - attributes with default values, if elements to which these
    ///    attributes apply appear in the document without specifications
    ///    of values for these attributes, or
    ///  - entities (other than amp, lt, gt, apos, quot), if references
    ///    to those entities appear in the document, or
    ///  - attributes with values subject to normalization, where the
    ///    attribute appears in the document with a value which will change
    ///    as a result of normalization, or
    ///  - element types with element content, if white space occurs directly
    ///    within any instance of those types.
    /// ```
    ///
    /// Returns:
    /// - 1 if standalone="yes"
    /// - 0 if standalone="no"
    /// - -2 if standalone attribute is missing or invalid
    ///      (A standalone value of -2 means that the XML declaration was found,
    ///       but no value was specified for the standalone attribute).
    #[doc(alias = "xmlParseSDDecl")]
    unsafe fn parse_sddecl(&mut self) -> i32 {
        unsafe {
            self.skip_blanks();
            if !self.content_bytes().starts_with(b"standalone") {
                return -2;
            }
            self.advance(10);
            self.skip_blanks();
            if self.consume_char_if(|_, c| c == '=').is_none() {
                xml_fatal_err(self, XmlParserErrors::XmlErrEqualRequired, None);
                return -2;
            }
            self.skip_blanks();
            let Some(quoto) = self.consume_char_if(|_, c| c == '"' || c == '\'') else {
                xml_fatal_err(self, XmlParserErrors::XmlErrStringNotStarted, None);
                return -2;
            };
            let standalone = if self.content_bytes().starts_with(b"no") {
                self.advance(2);
                0
            } else if self.content_bytes().starts_with(b"yes") {
                self.advance(3);
                1
            } else {
                xml_fatal_err(self, XmlParserErrors::XmlErrStandaloneValue, None);
                -2
            };
            if self.consume_char_if(|_, c| c == quoto).is_none() {
                xml_fatal_err(self, XmlParserErrors::XmlErrStringNotClosed, None);
            }
            standalone
        }
    }

    /// parse an XML declaration header
    ///
    /// ```text
    /// [23] XMLDecl ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
    /// ```
    #[doc(alias = "xmlParseXMLDecl")]
    pub(crate) unsafe fn parse_xmldecl(&mut self) {
        unsafe {
            // This value for standalone indicates that the document has an
            // XML declaration but it does not have a standalone attribute.
            // It will be overwritten later if a standalone attribute is found.
            self.input_mut().unwrap().standalone = -2;

            // We know that '<?xml' is here.
            assert!(self.content_bytes().starts_with(b"<?xml"));
            self.advance(5);

            if !xml_is_blank_char(self.current_byte() as u32) {
                xml_fatal_err_msg(
                    self,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Blank needed after '<?xml'\n",
                );
            }
            self.skip_blanks();

            // We must have the VersionInfo here.
            if let Some(version) = self.parse_version_info() {
                if version != XML_DEFAULT_VERSION {
                    // Changed here for XML-1.0 5th edition
                    if self.options & XmlParserOption::XmlParseOld10 as i32 != 0 {
                        xml_fatal_err_msg_str!(
                            self,
                            XmlParserErrors::XmlErrUnknownVersion,
                            "Unsupported version '{}'\n",
                            version
                        );
                    } else if version.starts_with("1.") {
                        xml_warning_msg!(
                            self,
                            XmlParserErrors::XmlWarUnknownVersion,
                            "Unsupported version '{}'\n",
                            version
                        );
                    } else {
                        xml_fatal_err_msg_str!(
                            self,
                            XmlParserErrors::XmlErrUnknownVersion,
                            "Unsupported version '{}'\n",
                            version
                        );
                    }
                }
                self.version = Some(version);
            } else {
                xml_fatal_err(self, XmlParserErrors::XmlErrVersionMissing, None);
            }

            // We may have the encoding declaration
            if !xml_is_blank_char(self.current_byte() as u32) {
                if self.current_byte() == b'?' && self.nth_byte(1) == b'>' {
                    self.advance(2);
                    return;
                }
                xml_fatal_err_msg(
                    self,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Blank needed here\n",
                );
            }
            self.parse_encoding_decl();
            if self.err_no == XmlParserErrors::XmlErrUnsupportedEncoding as i32
                || matches!(self.instate, XmlParserInputState::XmlParserEOF)
            {
                // The XML REC instructs us to stop parsing right here
                return;
            }

            // We may have the standalone status.
            if self.input().unwrap().encoding.is_some()
                && !xml_is_blank_char(self.current_byte() as u32)
            {
                if self.content_bytes().starts_with(b"?>") {
                    self.advance(2);
                    return;
                }
                xml_fatal_err_msg(
                    self,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Blank needed here\n",
                );
            }

            // We can grow the input buffer freely at that point
            self.grow();

            self.skip_blanks();
            self.input_mut().unwrap().standalone = self.parse_sddecl();
            self.skip_blanks();
            if self.content_bytes().starts_with(b"?>") {
                self.advance(2);
            } else if self.current_byte() == b'>' {
                // Deprecated old WD ...
                xml_fatal_err(self, XmlParserErrors::XmlErrXMLDeclNotFinished, None);
                self.skip_char();
            } else {
                xml_fatal_err(self, XmlParserErrors::XmlErrXMLDeclNotFinished, None);
                self.grow();
                while !self.content_bytes().is_empty() {
                    match self.content_bytes().iter().position(|&c| c == b'>') {
                        Some(pos) => {
                            self.advance_with_line_handling(pos + 1);
                            break;
                        }
                        None => {
                            self.advance_with_line_handling(self.content_bytes().len());
                            self.grow();
                        }
                    }
                }
            }
        }
    }

    /// Parse an XML declaration header for external entities
    ///
    /// ```text
    /// [77] TextDecl ::= '<?xml' VersionInfo? EncodingDecl S? '?>'
    /// ```
    #[doc(alias = "xmlParseTextDecl")]
    pub(crate) unsafe fn parse_text_decl(&mut self) {
        unsafe {
            // We know that '<?xml' is here.
            if !self.content_bytes().starts_with(b"<?xml")
                || !xml_is_blank_char(self.nth_byte(5) as u32)
            {
                xml_fatal_err(self, XmlParserErrors::XmlErrXMLDeclNotStarted, None);
                return;
            }
            self.advance(5);

            // Avoid expansion of parameter entities when skipping blanks.
            let oldstate = self.instate;
            self.instate = XmlParserInputState::XmlParserStart;

            if self.skip_blanks() == 0 {
                xml_fatal_err_msg(
                    self,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Space needed after '<?xml'\n",
                );
            }

            // We may have the VersionInfo here.
            let version = self.parse_version_info();
            if version.is_some() && self.skip_blanks() == 0 {
                xml_fatal_err_msg(
                    self,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Space needed here\n",
                );
            }
            self.input_mut().unwrap().version = version.or(Some(XML_DEFAULT_VERSION.to_owned()));

            // We must have the encoding declaration
            let encoding = self.parse_encoding_decl();
            if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                return;
            }
            if self.err_no == XmlParserErrors::XmlErrUnsupportedEncoding as i32 {
                // The XML REC instructs us to stop parsing right here
                self.instate = oldstate;
                return;
            }
            if encoding.is_none() && self.err_no == XmlParserErrors::XmlErrOK as i32 {
                xml_fatal_err_msg(
                    self,
                    XmlParserErrors::XmlErrMissingEncoding,
                    "Missing encoding in text declaration\n",
                );
            }

            self.skip_blanks();
            if self.content_bytes().starts_with(b"?>") {
                self.advance(2);
            } else if self.consume_char_if(|_, c| c == '>').is_some() {
                // Deprecated old WD ...
                xml_fatal_err(self, XmlParserErrors::XmlErrXMLDeclNotFinished, None);
            } else {
                xml_fatal_err(self, XmlParserErrors::XmlErrXMLDeclNotFinished, None);
                self.grow();
                while !self.content_bytes().is_empty() {
                    match self.content_bytes().iter().position(|&c| c == b'>') {
                        Some(pos) => {
                            self.advance_with_line_handling(pos + 1);
                            break;
                        }
                        None => {
                            self.advance_with_line_handling(self.content_bytes().len());
                            self.grow();
                        }
                    }
                }
            }

            self.instate = oldstate;
        }
    }
}
